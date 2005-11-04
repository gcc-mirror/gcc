/* SSA Dominator optimizations for trees
   Copyright (C) 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.
   Contributed by Diego Novillo <dnovillo@redhat.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "flags.h"
#include "rtl.h"
#include "tm_p.h"
#include "ggc.h"
#include "basic-block.h"
#include "cfgloop.h"
#include "output.h"
#include "expr.h"
#include "function.h"
#include "diagnostic.h"
#include "timevar.h"
#include "tree-dump.h"
#include "tree-flow.h"
#include "domwalk.h"
#include "real.h"
#include "tree-pass.h"
#include "tree-ssa-propagate.h"
#include "langhooks.h"
#include "params.h"

/* This file implements optimizations on the dominator tree.  */


/* Structure for recording edge equivalences as well as any pending
   edge redirections during the dominator optimizer.

   Computing and storing the edge equivalences instead of creating
   them on-demand can save significant amounts of time, particularly
   for pathological cases involving switch statements.  

   These structures live for a single iteration of the dominator
   optimizer in the edge's AUX field.  At the end of an iteration we
   free each of these structures and update the AUX field to point
   to any requested redirection target (the code for updating the
   CFG and SSA graph for edge redirection expects redirection edge
   targets to be in the AUX field for each edge.  */

struct edge_info
{
  /* If this edge creates a simple equivalence, the LHS and RHS of
     the equivalence will be stored here.  */
  tree lhs;
  tree rhs;

  /* Traversing an edge may also indicate one or more particular conditions
     are true or false.  The number of recorded conditions can vary, but
     can be determined by the condition's code.  So we have an array
     and its maximum index rather than use a varray.  */
  tree *cond_equivalences;
  unsigned int max_cond_equivalences;

  /* If we can thread this edge this field records the new target.  */
  edge redirection_target;
};


/* Hash table with expressions made available during the renaming process.
   When an assignment of the form X_i = EXPR is found, the statement is
   stored in this table.  If the same expression EXPR is later found on the
   RHS of another statement, it is replaced with X_i (thus performing
   global redundancy elimination).  Similarly as we pass through conditionals
   we record the conditional itself as having either a true or false value
   in this table.  */
static htab_t avail_exprs;

/* Stack of available expressions in AVAIL_EXPRs.  Each block pushes any
   expressions it enters into the hash table along with a marker entry
   (null).  When we finish processing the block, we pop off entries and
   remove the expressions from the global hash table until we hit the
   marker.  */
static VEC(tree,heap) *avail_exprs_stack;

/* Stack of statements we need to rescan during finalization for newly
   exposed variables.

   Statement rescanning must occur after the current block's available
   expressions are removed from AVAIL_EXPRS.  Else we may change the
   hash code for an expression and be unable to find/remove it from
   AVAIL_EXPRS.  */
static VEC(tree,heap) *stmts_to_rescan;

/* Structure for entries in the expression hash table.

   This requires more memory for the hash table entries, but allows us
   to avoid creating silly tree nodes and annotations for conditionals,
   eliminates 2 global hash tables and two block local varrays.
   
   It also allows us to reduce the number of hash table lookups we
   have to perform in lookup_avail_expr and finally it allows us to
   significantly reduce the number of calls into the hashing routine
   itself.  */

struct expr_hash_elt
{
  /* The value (lhs) of this expression.  */
  tree lhs;

  /* The expression (rhs) we want to record.  */
  tree rhs;

  /* The stmt pointer if this element corresponds to a statement.  */
  tree stmt;

  /* The hash value for RHS/ann.  */
  hashval_t hash;
};

/* Stack of dest,src pairs that need to be restored during finalization.

   A NULL entry is used to mark the end of pairs which need to be
   restored during finalization of this block.  */
static VEC(tree,heap) *const_and_copies_stack;

/* Bitmap of SSA_NAMEs known to have a nonzero value, even if we do not
   know their exact value.  */
static bitmap nonzero_vars;

/* Bitmap of blocks that are scheduled to be threaded through.  This
   is used to communicate with thread_through_blocks.  */
static bitmap threaded_blocks;

/* Stack of SSA_NAMEs which need their NONZERO_VARS property cleared
   when the current block is finalized. 

   A NULL entry is used to mark the end of names needing their 
   entry in NONZERO_VARS cleared during finalization of this block.  */
static VEC(tree,heap) *nonzero_vars_stack;

/* Track whether or not we have changed the control flow graph.  */
static bool cfg_altered;

/* Bitmap of blocks that have had EH statements cleaned.  We should
   remove their dead edges eventually.  */
static bitmap need_eh_cleanup;

/* Statistics for dominator optimizations.  */
struct opt_stats_d
{
  long num_stmts;
  long num_exprs_considered;
  long num_re;
  long num_const_prop;
  long num_copy_prop;
  long num_iterations;
};

static struct opt_stats_d opt_stats;

/* Value range propagation record.  Each time we encounter a conditional
   of the form SSA_NAME COND CONST we create a new vrp_element to record
   how the condition affects the possible values SSA_NAME may have.

   Each record contains the condition tested (COND), and the range of
   values the variable may legitimately have if COND is true.  Note the
   range of values may be a smaller range than COND specifies if we have
   recorded other ranges for this variable.  Each record also contains the
   block in which the range was recorded for invalidation purposes.

   Note that the current known range is computed lazily.  This allows us
   to avoid the overhead of computing ranges which are never queried.

   When we encounter a conditional, we look for records which constrain
   the SSA_NAME used in the condition.  In some cases those records allow
   us to determine the condition's result at compile time.  In other cases
   they may allow us to simplify the condition.

   We also use value ranges to do things like transform signed div/mod
   operations into unsigned div/mod or to simplify ABS_EXPRs. 

   Simple experiments have shown these optimizations to not be all that
   useful on switch statements (much to my surprise).  So switch statement
   optimizations are not performed.

   Note carefully we do not propagate information through each statement
   in the block.  i.e., if we know variable X has a value defined of
   [0, 25] and we encounter Y = X + 1, we do not track a value range
   for Y (which would be [1, 26] if we cared).  Similarly we do not
   constrain values as we encounter narrowing typecasts, etc.  */

struct vrp_element
{
  /* The highest and lowest values the variable in COND may contain when
     COND is true.  Note this may not necessarily be the same values
     tested by COND if the same variable was used in earlier conditionals. 

     Note this is computed lazily and thus can be NULL indicating that
     the values have not been computed yet.  */
  tree low;
  tree high;

  /* The actual conditional we recorded.  This is needed since we compute
     ranges lazily.  */
  tree cond;

  /* The basic block where this record was created.  We use this to determine
     when to remove records.  */
  basic_block bb;
};

/* A hash table holding value range records (VRP_ELEMENTs) for a given
   SSA_NAME.  We used to use a varray indexed by SSA_NAME_VERSION, but
   that gets awful wasteful, particularly since the density objects
   with useful information is very low.  */
static htab_t vrp_data;

typedef struct vrp_element *vrp_element_p;

DEF_VEC_P(vrp_element_p);
DEF_VEC_ALLOC_P(vrp_element_p,heap);

/* An entry in the VRP_DATA hash table.  We record the variable and a
   varray of VRP_ELEMENT records associated with that variable.  */
struct vrp_hash_elt
{
  tree var;
  VEC(vrp_element_p,heap) *records;
};

/* Array of variables which have their values constrained by operations
   in this basic block.  We use this during finalization to know
   which variables need their VRP data updated.  */

/* Stack of SSA_NAMEs which had their values constrained by operations
   in this basic block.  During finalization of this block we use this
   list to determine which variables need their VRP data updated.

   A NULL entry marks the end of the SSA_NAMEs associated with this block.  */
static VEC(tree,heap) *vrp_variables_stack;

struct eq_expr_value
{
  tree src;
  tree dst;
};

/* Local functions.  */
static void optimize_stmt (struct dom_walk_data *, 
			   basic_block bb,
			   block_stmt_iterator);
static tree lookup_avail_expr (tree, bool);
static hashval_t vrp_hash (const void *);
static int vrp_eq (const void *, const void *);
static hashval_t avail_expr_hash (const void *);
static hashval_t real_avail_expr_hash (const void *);
static int avail_expr_eq (const void *, const void *);
static void htab_statistics (FILE *, htab_t);
static void record_cond (tree, tree);
static void record_const_or_copy (tree, tree);
static void record_equality (tree, tree);
static tree update_rhs_and_lookup_avail_expr (tree, tree, bool);
static tree simplify_rhs_and_lookup_avail_expr (tree, int);
static tree simplify_cond_and_lookup_avail_expr (tree, stmt_ann_t, int);
static tree simplify_switch_and_lookup_avail_expr (tree, int);
static tree find_equivalent_equality_comparison (tree);
static void record_range (tree, basic_block);
static bool extract_range_from_cond (tree, tree *, tree *, int *);
static void record_equivalences_from_phis (basic_block);
static void record_equivalences_from_incoming_edge (basic_block);
static bool eliminate_redundant_computations (tree, stmt_ann_t);
static void record_equivalences_from_stmt (tree, int, stmt_ann_t);
static void thread_across_edge (struct dom_walk_data *, edge);
static void dom_opt_finalize_block (struct dom_walk_data *, basic_block);
static void dom_opt_initialize_block (struct dom_walk_data *, basic_block);
static void propagate_to_outgoing_edges (struct dom_walk_data *, basic_block);
static void remove_local_expressions_from_table (void);
static void restore_vars_to_original_value (void);
static edge single_incoming_edge_ignoring_loop_edges (basic_block);
static void restore_nonzero_vars_to_original_value (void);
static inline bool unsafe_associative_fp_binop (tree);


/* Local version of fold that doesn't introduce cruft.  */

static tree
local_fold (tree t)
{
  t = fold (t);

  /* Strip away useless type conversions.  Both the NON_LVALUE_EXPR that
     may have been added by fold, and "useless" type conversions that might
     now be apparent due to propagation.  */
  STRIP_USELESS_TYPE_CONVERSION (t);

  return t;
}

/* Allocate an EDGE_INFO for edge E and attach it to E.
   Return the new EDGE_INFO structure.  */

static struct edge_info *
allocate_edge_info (edge e)
{
  struct edge_info *edge_info;

  edge_info = xcalloc (1, sizeof (struct edge_info));

  e->aux = edge_info;
  return edge_info;
}

/* Free all EDGE_INFO structures associated with edges in the CFG.
   If a particular edge can be threaded, copy the redirection
   target from the EDGE_INFO structure into the edge's AUX field
   as required by code to update the CFG and SSA graph for
   jump threading.  */

static void
free_all_edge_infos (void)
{
  basic_block bb;
  edge_iterator ei;
  edge e;

  FOR_EACH_BB (bb)
    {
      FOR_EACH_EDGE (e, ei, bb->preds)
        {
	 struct edge_info *edge_info = e->aux;

	  if (edge_info)
	    {
	      e->aux = edge_info->redirection_target;
	      if (edge_info->cond_equivalences)
		free (edge_info->cond_equivalences);
	      free (edge_info);
	    }
	}
    }
}

/* Free an instance of vrp_hash_elt.  */

static void
vrp_free (void *data)
{
  struct vrp_hash_elt *elt = data;
  struct VEC(vrp_element_p,heap) **vrp_elt = &elt->records;

  VEC_free (vrp_element_p, heap, *vrp_elt);
  free (elt);
}

/* Jump threading, redundancy elimination and const/copy propagation. 

   This pass may expose new symbols that need to be renamed into SSA.  For
   every new symbol exposed, its corresponding bit will be set in
   VARS_TO_RENAME.  */

static void
tree_ssa_dominator_optimize (void)
{
  struct dom_walk_data walk_data;
  unsigned int i;
  struct loops loops_info;

  memset (&opt_stats, 0, sizeof (opt_stats));

  /* Create our hash tables.  */
  avail_exprs = htab_create (1024, real_avail_expr_hash, avail_expr_eq, free);
  vrp_data = htab_create (ceil_log2 (num_ssa_names), vrp_hash, vrp_eq,
			  vrp_free);
  avail_exprs_stack = VEC_alloc (tree, heap, 20);
  const_and_copies_stack = VEC_alloc (tree, heap, 20);
  nonzero_vars_stack = VEC_alloc (tree, heap, 20);
  vrp_variables_stack = VEC_alloc (tree, heap, 20);
  stmts_to_rescan = VEC_alloc (tree, heap, 20);
  nonzero_vars = BITMAP_ALLOC (NULL);
  threaded_blocks = BITMAP_ALLOC (NULL);
  need_eh_cleanup = BITMAP_ALLOC (NULL);

  /* Setup callbacks for the generic dominator tree walker.  */
  walk_data.walk_stmts_backward = false;
  walk_data.dom_direction = CDI_DOMINATORS;
  walk_data.initialize_block_local_data = NULL;
  walk_data.before_dom_children_before_stmts = dom_opt_initialize_block;
  walk_data.before_dom_children_walk_stmts = optimize_stmt;
  walk_data.before_dom_children_after_stmts = propagate_to_outgoing_edges;
  walk_data.after_dom_children_before_stmts = NULL;
  walk_data.after_dom_children_walk_stmts = NULL;
  walk_data.after_dom_children_after_stmts = dom_opt_finalize_block;
  /* Right now we only attach a dummy COND_EXPR to the global data pointer.
     When we attach more stuff we'll need to fill this out with a real
     structure.  */
  walk_data.global_data = NULL;
  walk_data.block_local_data_size = 0;
  walk_data.interesting_blocks = NULL;

  /* Now initialize the dominator walker.  */
  init_walk_dominator_tree (&walk_data);

  calculate_dominance_info (CDI_DOMINATORS);

  /* We need to know which edges exit loops so that we can
     aggressively thread through loop headers to an exit
     edge.  */
  flow_loops_find (&loops_info);
  mark_loop_exit_edges (&loops_info);
  flow_loops_free (&loops_info);

  /* Clean up the CFG so that any forwarder blocks created by loop
     canonicalization are removed.  */
  cleanup_tree_cfg ();
  calculate_dominance_info (CDI_DOMINATORS);

  /* If we prove certain blocks are unreachable, then we want to
     repeat the dominator optimization process as PHI nodes may
     have turned into copies which allows better propagation of
     values.  So we repeat until we do not identify any new unreachable
     blocks.  */
  do
    {
      /* Optimize the dominator tree.  */
      cfg_altered = false;

      /* We need accurate information regarding back edges in the CFG
	 for jump threading.  */
      mark_dfs_back_edges ();

      /* Recursively walk the dominator tree optimizing statements.  */
      walk_dominator_tree (&walk_data, ENTRY_BLOCK_PTR);

      {
	block_stmt_iterator bsi;
	basic_block bb;
	FOR_EACH_BB (bb)
	  {
	    for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
	      {
		update_stmt_if_modified (bsi_stmt (bsi));
	      }
	  }
      }

      /* If we exposed any new variables, go ahead and put them into
	 SSA form now, before we handle jump threading.  This simplifies
	 interactions between rewriting of _DECL nodes into SSA form
	 and rewriting SSA_NAME nodes into SSA form after block
	 duplication and CFG manipulation.  */
      update_ssa (TODO_update_ssa);

      free_all_edge_infos ();

      /* Thread jumps, creating duplicate blocks as needed.  */
      cfg_altered |= thread_through_all_blocks (threaded_blocks);

      /* Removal of statements may make some EH edges dead.  Purge
	 such edges from the CFG as needed.  */
      if (!bitmap_empty_p (need_eh_cleanup))
	{
	  cfg_altered |= tree_purge_all_dead_eh_edges (need_eh_cleanup);
	  bitmap_zero (need_eh_cleanup);
	}

      if (cfg_altered)
        free_dominance_info (CDI_DOMINATORS);

      /* Only iterate if we threaded jumps AND the CFG cleanup did
	 something interesting.  Other cases generate far fewer
	 optimization opportunities and thus are not worth another
	 full DOM iteration.  */
      cfg_altered &= cleanup_tree_cfg ();

      if (rediscover_loops_after_threading)
	{
	  /* Rerun basic loop analysis to discover any newly
	     created loops and update the set of exit edges.  */
	  rediscover_loops_after_threading = false;
	  flow_loops_find (&loops_info);
	  mark_loop_exit_edges (&loops_info);
	  flow_loops_free (&loops_info);

	  /* Remove any forwarder blocks inserted by loop
	     header canonicalization.  */
	  cleanup_tree_cfg ();
	}

      calculate_dominance_info (CDI_DOMINATORS);

      update_ssa (TODO_update_ssa);

      /* Reinitialize the various tables.  */
      bitmap_clear (nonzero_vars);
      bitmap_clear (threaded_blocks);
      htab_empty (avail_exprs);
      htab_empty (vrp_data);

      /* Finally, remove everything except invariants in SSA_NAME_VALUE.

	 This must be done before we iterate as we might have a
	 reference to an SSA_NAME which was removed by the call to
	 update_ssa.

	 Long term we will be able to let everything in SSA_NAME_VALUE
	 persist.  However, for now, we know this is the safe thing to do.  */
      for (i = 0; i < num_ssa_names; i++)
	{
	  tree name = ssa_name (i);
	  tree value;

	  if (!name)
	    continue;

	  value = SSA_NAME_VALUE (name);
	  if (value && !is_gimple_min_invariant (value))
	    SSA_NAME_VALUE (name) = NULL;
	}

      opt_stats.num_iterations++;
    }
  while (optimize > 1 && cfg_altered);

  /* Debugging dumps.  */
  if (dump_file && (dump_flags & TDF_STATS))
    dump_dominator_optimization_stats (dump_file);

  /* We emptied the hash table earlier, now delete it completely.  */
  htab_delete (avail_exprs);
  htab_delete (vrp_data);

  /* It is not necessary to clear CURRDEFS, REDIRECTION_EDGES, VRP_DATA,
     CONST_AND_COPIES, and NONZERO_VARS as they all get cleared at the bottom
     of the do-while loop above.  */

  /* And finalize the dominator walker.  */
  fini_walk_dominator_tree (&walk_data);

  /* Free nonzero_vars.  */
  BITMAP_FREE (nonzero_vars);
  BITMAP_FREE (threaded_blocks);
  BITMAP_FREE (need_eh_cleanup);
  
  VEC_free (tree, heap, avail_exprs_stack);
  VEC_free (tree, heap, const_and_copies_stack);
  VEC_free (tree, heap, nonzero_vars_stack);
  VEC_free (tree, heap, vrp_variables_stack);
  VEC_free (tree, heap, stmts_to_rescan);
}

static bool
gate_dominator (void)
{
  return flag_tree_dom != 0;
}

struct tree_opt_pass pass_dominator = 
{
  "dom",				/* name */
  gate_dominator,			/* gate */
  tree_ssa_dominator_optimize,		/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_SSA_DOMINATOR_OPTS,		/* tv_id */
  PROP_cfg | PROP_ssa | PROP_alias,	/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func
    | TODO_update_ssa
    | TODO_verify_ssa,			/* todo_flags_finish */
  0					/* letter */
};


/* We are exiting E->src, see if E->dest ends with a conditional
   jump which has a known value when reached via E. 

   Special care is necessary if E is a back edge in the CFG as we
   will have already recorded equivalences for E->dest into our
   various tables, including the result of the conditional at
   the end of E->dest.  Threading opportunities are severely
   limited in that case to avoid short-circuiting the loop
   incorrectly.

   Note it is quite common for the first block inside a loop to
   end with a conditional which is either always true or always
   false when reached via the loop backedge.  Thus we do not want
   to blindly disable threading across a loop backedge.  */

static void
thread_across_edge (struct dom_walk_data *walk_data, edge e)
{
  block_stmt_iterator bsi;
  tree stmt = NULL;
  tree phi;
  int stmt_count = 0;
  int max_stmt_count;


  /* If E->dest does not end with a conditional, then there is
     nothing to do.  */
  bsi = bsi_last (e->dest);
  if (bsi_end_p (bsi)
      || ! bsi_stmt (bsi)
      || (TREE_CODE (bsi_stmt (bsi)) != COND_EXPR
	  && TREE_CODE (bsi_stmt (bsi)) != GOTO_EXPR
	  && TREE_CODE (bsi_stmt (bsi)) != SWITCH_EXPR))
    return;

  /* The basic idea here is to use whatever knowledge we have
     from our dominator walk to simplify statements in E->dest,
     with the ultimate goal being to simplify the conditional
     at the end of E->dest.

     Note that we must undo any changes we make to the underlying
     statements as the simplifications we are making are control
     flow sensitive (ie, the simplifications are valid when we 
     traverse E, but may not be valid on other paths to E->dest.  */
     
  /* Each PHI creates a temporary equivalence, record them.  Again
     these are context sensitive equivalences and will be removed
     by our caller.  */
  for (phi = phi_nodes (e->dest); phi; phi = PHI_CHAIN (phi))
    {
      tree src = PHI_ARG_DEF_FROM_EDGE (phi, e);
      tree dst = PHI_RESULT (phi);

      /* Do not include virtual PHIs in our statement count as
	 they never generate code.  */
      if (is_gimple_reg (dst))
	stmt_count++;

      /* If the desired argument is not the same as this PHI's result 
	 and it is set by a PHI in E->dest, then we can not thread
	 through E->dest.  */
      if (src != dst
	  && TREE_CODE (src) == SSA_NAME
	  && TREE_CODE (SSA_NAME_DEF_STMT (src)) == PHI_NODE
	  && bb_for_stmt (SSA_NAME_DEF_STMT (src)) == e->dest)
	return;

      record_const_or_copy (dst, src);
    }

  /* Try to simplify each statement in E->dest, ultimately leading to
     a simplification of the COND_EXPR at the end of E->dest.

     We might consider marking just those statements which ultimately
     feed the COND_EXPR.  It's not clear if the overhead of bookkeeping
     would be recovered by trying to simplify fewer statements.

     If we are able to simplify a statement into the form
     SSA_NAME = (SSA_NAME | gimple invariant), then we can record
     a context sensitive equivalency which may help us simplify
     later statements in E->dest. 

     Failure to simplify into the form above merely means that the
     statement provides no equivalences to help simplify later
     statements.  This does not prevent threading through E->dest.  */
  max_stmt_count = PARAM_VALUE (PARAM_MAX_JUMP_THREAD_DUPLICATION_STMTS);
  for (bsi = bsi_start (e->dest); ! bsi_end_p (bsi); bsi_next (&bsi))
    {
      tree cached_lhs = NULL;

      stmt = bsi_stmt (bsi);

      /* Ignore empty statements and labels.  */
      if (IS_EMPTY_STMT (stmt) || TREE_CODE (stmt) == LABEL_EXPR)
	continue;

      /* If duplicating this block is going to cause too much code
	 expansion, then do not thread through this block.  */
      stmt_count++;
      if (stmt_count > max_stmt_count)
	return;

      /* Safely handle threading across loop backedges.  This is
	 over conservative, but still allows us to capture the
	 majority of the cases where we can thread across a loop
	 backedge.  */
      if ((e->flags & EDGE_DFS_BACK) != 0
	  && TREE_CODE (stmt) != COND_EXPR
	  && TREE_CODE (stmt) != SWITCH_EXPR)
	return;

      /* If the statement has volatile operands, then we assume we
	 can not thread through this block.  This is overly
	 conservative in some ways.  */
      if (TREE_CODE (stmt) == ASM_EXPR && ASM_VOLATILE_P (stmt))
	return;

      /* If this is not a MODIFY_EXPR which sets an SSA_NAME to a new
	 value, then do not try to simplify this statement as it will
	 not simplify in any way that is helpful for jump threading.  */
      if (TREE_CODE (stmt) != MODIFY_EXPR
	  || TREE_CODE (TREE_OPERAND (stmt, 0)) != SSA_NAME)
	continue;

      /* At this point we have a statement which assigns an RHS to an
	 SSA_VAR on the LHS.  We want to try and simplify this statement
	 to expose more context sensitive equivalences which in turn may
	 allow us to simplify the condition at the end of the loop.  */
      if (TREE_CODE (TREE_OPERAND (stmt, 1)) == SSA_NAME)
	cached_lhs = TREE_OPERAND (stmt, 1);
      else
	{
	  /* Copy the operands.  */
	  tree *copy, pre_fold_expr;
	  ssa_op_iter iter;
	  use_operand_p use_p;
	  unsigned int num, i = 0;

	  num = NUM_SSA_OPERANDS (stmt, (SSA_OP_USE | SSA_OP_VUSE));
	  copy = xcalloc (num, sizeof (tree));

	  /* Make a copy of the uses & vuses into USES_COPY, then cprop into
	     the operands.  */
	  FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_USE | SSA_OP_VUSE)
	    {
	      tree tmp = NULL;
	      tree use = USE_FROM_PTR (use_p);

	      copy[i++] = use;
	      if (TREE_CODE (use) == SSA_NAME)
		tmp = SSA_NAME_VALUE (use);
	      if (tmp && TREE_CODE (tmp) != VALUE_HANDLE)
		SET_USE (use_p, tmp);
	    }

	  /* Try to fold/lookup the new expression.  Inserting the
	     expression into the hash table is unlikely to help
	     Sadly, we have to handle conditional assignments specially
	     here, because fold expects all the operands of an expression
	     to be folded before the expression itself is folded, but we
	     can't just substitute the folded condition here.  */
	  if (TREE_CODE (TREE_OPERAND (stmt, 1)) == COND_EXPR)
	    {
	      tree cond = COND_EXPR_COND (TREE_OPERAND (stmt, 1));
	      cond = fold (cond);
	      if (cond == boolean_true_node)
		pre_fold_expr = COND_EXPR_THEN (TREE_OPERAND (stmt, 1));
	      else if (cond == boolean_false_node)
		pre_fold_expr = COND_EXPR_ELSE (TREE_OPERAND (stmt, 1));
	      else
		pre_fold_expr = TREE_OPERAND (stmt, 1);
	    }
	  else
	    pre_fold_expr = TREE_OPERAND (stmt, 1);

	  if (pre_fold_expr)
	    {
	      cached_lhs = fold (pre_fold_expr);
	      if (TREE_CODE (cached_lhs) != SSA_NAME
		  && !is_gimple_min_invariant (cached_lhs))
	        cached_lhs = lookup_avail_expr (stmt, false);
	    }

	  /* Restore the statement's original uses/defs.  */
	  i = 0;
	  FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_USE | SSA_OP_VUSE)
	    SET_USE (use_p, copy[i++]);

	  free (copy);
	}

      /* Record the context sensitive equivalence if we were able
	 to simplify this statement.  */
      if (cached_lhs
	  && (TREE_CODE (cached_lhs) == SSA_NAME
	      || is_gimple_min_invariant (cached_lhs)))
	record_const_or_copy (TREE_OPERAND (stmt, 0), cached_lhs);
    }

  /* If we stopped at a COND_EXPR or SWITCH_EXPR, see if we know which arm
     will be taken.  */
  if (stmt
      && (TREE_CODE (stmt) == COND_EXPR
	  || TREE_CODE (stmt) == GOTO_EXPR
	  || TREE_CODE (stmt) == SWITCH_EXPR))
    {
      tree cond, cached_lhs;

      /* Now temporarily cprop the operands and try to find the resulting
	 expression in the hash tables.  */
      if (TREE_CODE (stmt) == COND_EXPR)
	cond = COND_EXPR_COND (stmt);
      else if (TREE_CODE (stmt) == GOTO_EXPR)
	cond = GOTO_DESTINATION (stmt);
      else
	cond = SWITCH_COND (stmt);

      if (COMPARISON_CLASS_P (cond))
	{
	  tree dummy_cond, op0, op1;
	  enum tree_code cond_code;

	  op0 = TREE_OPERAND (cond, 0);
	  op1 = TREE_OPERAND (cond, 1);
	  cond_code = TREE_CODE (cond);

	  /* Get the current value of both operands.  */
	  if (TREE_CODE (op0) == SSA_NAME)
	    {
	      tree tmp = SSA_NAME_VALUE (op0);
	      if (tmp && TREE_CODE (tmp) != VALUE_HANDLE)
		op0 = tmp;
	    }

	  if (TREE_CODE (op1) == SSA_NAME)
	    {
	      tree tmp = SSA_NAME_VALUE (op1);
	      if (tmp && TREE_CODE (tmp) != VALUE_HANDLE)
		op1 = tmp;
	    }

	  /* Stuff the operator and operands into our dummy conditional
	     expression, creating the dummy conditional if necessary.  */
	  dummy_cond = walk_data->global_data;
	  if (! dummy_cond)
	    {
	      dummy_cond = build (cond_code, boolean_type_node, op0, op1);
	      dummy_cond = build (COND_EXPR, void_type_node,
				  dummy_cond, NULL, NULL);
	      walk_data->global_data = dummy_cond;
	    }
	  else
	    {
	      TREE_SET_CODE (COND_EXPR_COND (dummy_cond), cond_code);
	      TREE_OPERAND (COND_EXPR_COND (dummy_cond), 0) = op0;
	      TREE_OPERAND (COND_EXPR_COND (dummy_cond), 1) = op1;
	    }

	  /* If the conditional folds to an invariant, then we are done,
	     otherwise look it up in the hash tables.  */
	  cached_lhs = local_fold (COND_EXPR_COND (dummy_cond));
	  if (! is_gimple_min_invariant (cached_lhs))
	    {
	      cached_lhs = lookup_avail_expr (dummy_cond, false);
	      if (!cached_lhs || ! is_gimple_min_invariant (cached_lhs))
		cached_lhs = simplify_cond_and_lookup_avail_expr (dummy_cond,
								  NULL,
								  false);
	    }
	}
      /* We can have conditionals which just test the state of a
	 variable rather than use a relational operator.  These are
	 simpler to handle.  */
      else if (TREE_CODE (cond) == SSA_NAME)
	{
	  cached_lhs = cond;
	  cached_lhs = SSA_NAME_VALUE (cached_lhs);
	  if (cached_lhs && ! is_gimple_min_invariant (cached_lhs))
	    cached_lhs = NULL;
	}
      else
	cached_lhs = lookup_avail_expr (stmt, false);

      if (cached_lhs)
	{
	  edge taken_edge = find_taken_edge (e->dest, cached_lhs);
	  basic_block dest = (taken_edge ? taken_edge->dest : NULL);

	  if (dest == e->dest)
	    return;

	  /* If we have a known destination for the conditional, then
	     we can perform this optimization, which saves at least one
	     conditional jump each time it applies since we get to
	     bypass the conditional at our original destination.  */
	  if (dest)
	    {
	      struct edge_info *edge_info;

	      if (e->aux)
		edge_info = e->aux;
	      else
		edge_info = allocate_edge_info (e);
	      edge_info->redirection_target = taken_edge;
	      bitmap_set_bit (threaded_blocks, e->dest->index);
	    }
	}
    }
}


/* Initialize local stacks for this optimizer and record equivalences
   upon entry to BB.  Equivalences can come from the edge traversed to
   reach BB or they may come from PHI nodes at the start of BB.  */

static void
dom_opt_initialize_block (struct dom_walk_data *walk_data ATTRIBUTE_UNUSED,
			  basic_block bb)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\n\nOptimizing block #%d\n\n", bb->index);

  /* Push a marker on the stacks of local information so that we know how
     far to unwind when we finalize this block.  */
  VEC_safe_push (tree, heap, avail_exprs_stack, NULL_TREE);
  VEC_safe_push (tree, heap, const_and_copies_stack, NULL_TREE);
  VEC_safe_push (tree, heap, nonzero_vars_stack, NULL_TREE);
  VEC_safe_push (tree, heap, vrp_variables_stack, NULL_TREE);

  record_equivalences_from_incoming_edge (bb);

  /* PHI nodes can create equivalences too.  */
  record_equivalences_from_phis (bb);
}

/* Given an expression EXPR (a relational expression or a statement), 
   initialize the hash table element pointed to by ELEMENT.  */

static void
initialize_hash_element (tree expr, tree lhs, struct expr_hash_elt *element)
{
  /* Hash table elements may be based on conditional expressions or statements.

     For the former case, we have no annotation and we want to hash the
     conditional expression.  In the latter case we have an annotation and
     we want to record the expression the statement evaluates.  */
  if (COMPARISON_CLASS_P (expr) || TREE_CODE (expr) == TRUTH_NOT_EXPR)
    {
      element->stmt = NULL;
      element->rhs = expr;
    }
  else if (TREE_CODE (expr) == COND_EXPR)
    {
      element->stmt = expr;
      element->rhs = COND_EXPR_COND (expr);
    }
  else if (TREE_CODE (expr) == SWITCH_EXPR)
    {
      element->stmt = expr;
      element->rhs = SWITCH_COND (expr);
    }
  else if (TREE_CODE (expr) == RETURN_EXPR && TREE_OPERAND (expr, 0))
    {
      element->stmt = expr;
      element->rhs = TREE_OPERAND (TREE_OPERAND (expr, 0), 1);
    }
  else if (TREE_CODE (expr) == GOTO_EXPR)
    {
      element->stmt = expr;
      element->rhs = GOTO_DESTINATION (expr);
    }
  else
    {
      element->stmt = expr;
      element->rhs = TREE_OPERAND (expr, 1);
    }

  element->lhs = lhs;
  element->hash = avail_expr_hash (element);
}

/* Remove all the expressions in LOCALS from TABLE, stopping when there are
   LIMIT entries left in LOCALs.  */

static void
remove_local_expressions_from_table (void)
{
  /* Remove all the expressions made available in this block.  */
  while (VEC_length (tree, avail_exprs_stack) > 0)
    {
      struct expr_hash_elt element;
      tree expr = VEC_pop (tree, avail_exprs_stack);

      if (expr == NULL_TREE)
	break;

      initialize_hash_element (expr, NULL, &element);
      htab_remove_elt_with_hash (avail_exprs, &element, element.hash);
    }
}

/* Use the SSA_NAMES in LOCALS to restore TABLE to its original
   state, stopping when there are LIMIT entries left in LOCALs.  */

static void
restore_nonzero_vars_to_original_value (void)
{
  while (VEC_length (tree, nonzero_vars_stack) > 0)
    {
      tree name = VEC_pop (tree, nonzero_vars_stack);

      if (name == NULL)
	break;

      bitmap_clear_bit (nonzero_vars, SSA_NAME_VERSION (name));
    }
}

/* Use the source/dest pairs in CONST_AND_COPIES_STACK to restore
   CONST_AND_COPIES to its original state, stopping when we hit a
   NULL marker.  */

static void
restore_vars_to_original_value (void)
{
  while (VEC_length (tree, const_and_copies_stack) > 0)
    {
      tree prev_value, dest;

      dest = VEC_pop (tree, const_and_copies_stack);

      if (dest == NULL)
	break;

      prev_value = VEC_pop (tree, const_and_copies_stack);
      SSA_NAME_VALUE (dest) =  prev_value;
    }
}

/* We have finished processing the dominator children of BB, perform
   any finalization actions in preparation for leaving this node in
   the dominator tree.  */

static void
dom_opt_finalize_block (struct dom_walk_data *walk_data, basic_block bb)
{
  tree last;

  /* If we have an outgoing edge to a block with multiple incoming and
     outgoing edges, then we may be able to thread the edge.  ie, we
     may be able to statically determine which of the outgoing edges
     will be traversed when the incoming edge from BB is traversed.  */
  if (single_succ_p (bb)
      && (single_succ_edge (bb)->flags & EDGE_ABNORMAL) == 0
      && !single_pred_p (single_succ (bb))
      && !single_succ_p (single_succ (bb)))
	
    {
      thread_across_edge (walk_data, single_succ_edge (bb));
    }
  else if ((last = last_stmt (bb))
	   && TREE_CODE (last) == COND_EXPR
	   && (COMPARISON_CLASS_P (COND_EXPR_COND (last))
	       || TREE_CODE (COND_EXPR_COND (last)) == SSA_NAME)
	   && EDGE_COUNT (bb->succs) == 2
	   && (EDGE_SUCC (bb, 0)->flags & EDGE_ABNORMAL) == 0
	   && (EDGE_SUCC (bb, 1)->flags & EDGE_ABNORMAL) == 0)
    {
      edge true_edge, false_edge;

      extract_true_false_edges_from_block (bb, &true_edge, &false_edge);

      /* Only try to thread the edge if it reaches a target block with
	 more than one predecessor and more than one successor.  */
      if (!single_pred_p (true_edge->dest) && !single_succ_p (true_edge->dest))
	{
	  struct edge_info *edge_info;
	  unsigned int i;

	  /* Push a marker onto the available expression stack so that we
	     unwind any expressions related to the TRUE arm before processing
	     the false arm below.  */
	  VEC_safe_push (tree, heap, avail_exprs_stack, NULL_TREE);
	  VEC_safe_push (tree, heap, const_and_copies_stack, NULL_TREE);

	  edge_info = true_edge->aux;

	  /* If we have info associated with this edge, record it into
	     our equivalency tables.  */
	  if (edge_info)
	    {
	      tree *cond_equivalences = edge_info->cond_equivalences;
	      tree lhs = edge_info->lhs;
	      tree rhs = edge_info->rhs;

	      /* If we have a simple NAME = VALUE equivalency record it.  */
	      if (lhs && TREE_CODE (lhs) == SSA_NAME)
		record_const_or_copy (lhs, rhs);

	      /* If we have 0 = COND or 1 = COND equivalences, record them
		 into our expression hash tables.  */
	      if (cond_equivalences)
		for (i = 0; i < edge_info->max_cond_equivalences; i += 2)
		  {
		    tree expr = cond_equivalences[i];
		    tree value = cond_equivalences[i + 1];

		    record_cond (expr, value);
		  }
	    }

	  /* Now thread the edge.  */
	  thread_across_edge (walk_data, true_edge);

	  /* And restore the various tables to their state before
	     we threaded this edge.  */
	  remove_local_expressions_from_table ();
	  restore_vars_to_original_value ();
	}

      /* Similarly for the ELSE arm.  */
      if (!single_pred_p (false_edge->dest) && !single_succ_p (false_edge->dest))
	{
	  struct edge_info *edge_info;
	  unsigned int i;

	  edge_info = false_edge->aux;

	  /* If we have info associated with this edge, record it into
	     our equivalency tables.  */
	  if (edge_info)
	    {
	      tree *cond_equivalences = edge_info->cond_equivalences;
	      tree lhs = edge_info->lhs;
	      tree rhs = edge_info->rhs;

	      /* If we have a simple NAME = VALUE equivalency record it.  */
	      if (lhs && TREE_CODE (lhs) == SSA_NAME)
		record_const_or_copy (lhs, rhs);

	      /* If we have 0 = COND or 1 = COND equivalences, record them
		 into our expression hash tables.  */
	      if (cond_equivalences)
		for (i = 0; i < edge_info->max_cond_equivalences; i += 2)
		  {
		    tree expr = cond_equivalences[i];
		    tree value = cond_equivalences[i + 1];

		    record_cond (expr, value);
		  }
	    }

	  thread_across_edge (walk_data, false_edge);

	  /* No need to remove local expressions from our tables
	     or restore vars to their original value as that will
	     be done immediately below.  */
	}
    }

  remove_local_expressions_from_table ();
  restore_nonzero_vars_to_original_value ();
  restore_vars_to_original_value ();

  /* Remove VRP records associated with this basic block.  They are no
     longer valid.

     To be efficient, we note which variables have had their values
     constrained in this block.  So walk over each variable in the
     VRP_VARIABLEs array.  */
  while (VEC_length (tree, vrp_variables_stack) > 0)
    {
      tree var = VEC_pop (tree, vrp_variables_stack);
      struct vrp_hash_elt vrp_hash_elt, *vrp_hash_elt_p;
      void **slot;

      /* Each variable has a stack of value range records.  We want to
	 invalidate those associated with our basic block.  So we walk
	 the array backwards popping off records associated with our
	 block.  Once we hit a record not associated with our block
	 we are done.  */
      VEC(vrp_element_p,heap) **var_vrp_records;

      if (var == NULL)
	break;

      vrp_hash_elt.var = var;
      vrp_hash_elt.records = NULL;

      slot = htab_find_slot (vrp_data, &vrp_hash_elt, NO_INSERT);

      vrp_hash_elt_p = (struct vrp_hash_elt *) *slot;
      var_vrp_records = &vrp_hash_elt_p->records;

      while (VEC_length (vrp_element_p, *var_vrp_records) > 0)
	{
	  struct vrp_element *element
	    = VEC_last (vrp_element_p, *var_vrp_records);

	  if (element->bb != bb)
	    break;
  
	  VEC_pop (vrp_element_p, *var_vrp_records);
	}
    }

  /* If we queued any statements to rescan in this block, then
     go ahead and rescan them now.  */
  while (VEC_length (tree, stmts_to_rescan) > 0)
    {
      tree stmt = VEC_last (tree, stmts_to_rescan);
      basic_block stmt_bb = bb_for_stmt (stmt);

      if (stmt_bb != bb)
	break;

      VEC_pop (tree, stmts_to_rescan);
      mark_new_vars_to_rename (stmt);
    }
}

/* PHI nodes can create equivalences too.

   Ignoring any alternatives which are the same as the result, if
   all the alternatives are equal, then the PHI node creates an
   equivalence.

   Additionally, if all the PHI alternatives are known to have a nonzero
   value, then the result of this PHI is known to have a nonzero value,
   even if we do not know its exact value.  */

static void
record_equivalences_from_phis (basic_block bb)
{
  tree phi;

  for (phi = phi_nodes (bb); phi; phi = PHI_CHAIN (phi))
    {
      tree lhs = PHI_RESULT (phi);
      tree rhs = NULL;
      int i;

      for (i = 0; i < PHI_NUM_ARGS (phi); i++)
	{
	  tree t = PHI_ARG_DEF (phi, i);

	  /* Ignore alternatives which are the same as our LHS.  Since
	     LHS is a PHI_RESULT, it is known to be a SSA_NAME, so we
	     can simply compare pointers.  */
	  if (lhs == t)
	    continue;

	  /* If we have not processed an alternative yet, then set
	     RHS to this alternative.  */
	  if (rhs == NULL)
	    rhs = t;
	  /* If we have processed an alternative (stored in RHS), then
	     see if it is equal to this one.  If it isn't, then stop
	     the search.  */
	  else if (! operand_equal_for_phi_arg_p (rhs, t))
	    break;
	}

      /* If we had no interesting alternatives, then all the RHS alternatives
	 must have been the same as LHS.  */
      if (!rhs)
	rhs = lhs;

      /* If we managed to iterate through each PHI alternative without
	 breaking out of the loop, then we have a PHI which may create
	 a useful equivalence.  We do not need to record unwind data for
	 this, since this is a true assignment and not an equivalence
	 inferred from a comparison.  All uses of this ssa name are dominated
	 by this assignment, so unwinding just costs time and space.  */
      if (i == PHI_NUM_ARGS (phi)
	  && may_propagate_copy (lhs, rhs))
	SSA_NAME_VALUE (lhs) = rhs;

      /* Now see if we know anything about the nonzero property for the
	 result of this PHI.  */
      for (i = 0; i < PHI_NUM_ARGS (phi); i++)
	{
	  if (!PHI_ARG_NONZERO (phi, i))
	    break;
	}

      if (i == PHI_NUM_ARGS (phi))
	bitmap_set_bit (nonzero_vars, SSA_NAME_VERSION (PHI_RESULT (phi)));
    }
}

/* Ignoring loop backedges, if BB has precisely one incoming edge then
   return that edge.  Otherwise return NULL.  */
static edge
single_incoming_edge_ignoring_loop_edges (basic_block bb)
{
  edge retval = NULL;
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->preds)
    {
      /* A loop back edge can be identified by the destination of
	 the edge dominating the source of the edge.  */
      if (dominated_by_p (CDI_DOMINATORS, e->src, e->dest))
	continue;

      /* If we have already seen a non-loop edge, then we must have
	 multiple incoming non-loop edges and thus we return NULL.  */
      if (retval)
	return NULL;

      /* This is the first non-loop incoming edge we have found.  Record
	 it.  */
      retval = e;
    }

  return retval;
}

/* Record any equivalences created by the incoming edge to BB.  If BB
   has more than one incoming edge, then no equivalence is created.  */

static void
record_equivalences_from_incoming_edge (basic_block bb)
{
  edge e;
  basic_block parent;
  struct edge_info *edge_info;

  /* If our parent block ended with a control statement, then we may be
     able to record some equivalences based on which outgoing edge from
     the parent was followed.  */
  parent = get_immediate_dominator (CDI_DOMINATORS, bb);

  e = single_incoming_edge_ignoring_loop_edges (bb);

  /* If we had a single incoming edge from our parent block, then enter
     any data associated with the edge into our tables.  */
  if (e && e->src == parent)
    {
      unsigned int i;

      edge_info = e->aux;

      if (edge_info)
	{
	  tree lhs = edge_info->lhs;
	  tree rhs = edge_info->rhs;
	  tree *cond_equivalences = edge_info->cond_equivalences;

	  if (lhs)
	    record_equality (lhs, rhs);

	  if (cond_equivalences)
	    {
	      bool recorded_range = false;
	      for (i = 0; i < edge_info->max_cond_equivalences; i += 2)
		{
		  tree expr = cond_equivalences[i];
		  tree value = cond_equivalences[i + 1];

		  record_cond (expr, value);

		  /* For the first true equivalence, record range
		     information.  We only do this for the first
		     true equivalence as it should dominate any
		     later true equivalences.  */
		  if (! recorded_range 
		      && COMPARISON_CLASS_P (expr)
		      && value == boolean_true_node
		      && TREE_CONSTANT (TREE_OPERAND (expr, 1)))
		    {
		      record_range (expr, bb);
		      recorded_range = true;
		    }
		}
	    }
	}
    }
}

/* Dump SSA statistics on FILE.  */

void
dump_dominator_optimization_stats (FILE *file)
{
  long n_exprs;

  fprintf (file, "Total number of statements:                   %6ld\n\n",
	   opt_stats.num_stmts);
  fprintf (file, "Exprs considered for dominator optimizations: %6ld\n",
           opt_stats.num_exprs_considered);

  n_exprs = opt_stats.num_exprs_considered;
  if (n_exprs == 0)
    n_exprs = 1;

  fprintf (file, "    Redundant expressions eliminated:         %6ld (%.0f%%)\n",
	   opt_stats.num_re, PERCENT (opt_stats.num_re,
				      n_exprs));
  fprintf (file, "    Constants propagated:                     %6ld\n",
	   opt_stats.num_const_prop);
  fprintf (file, "    Copies propagated:                        %6ld\n",
	   opt_stats.num_copy_prop);

  fprintf (file, "\nTotal number of DOM iterations:             %6ld\n",
	   opt_stats.num_iterations);

  fprintf (file, "\nHash table statistics:\n");

  fprintf (file, "    avail_exprs: ");
  htab_statistics (file, avail_exprs);
}


/* Dump SSA statistics on stderr.  */

void
debug_dominator_optimization_stats (void)
{
  dump_dominator_optimization_stats (stderr);
}


/* Dump statistics for the hash table HTAB.  */

static void
htab_statistics (FILE *file, htab_t htab)
{
  fprintf (file, "size %ld, %ld elements, %f collision/search ratio\n",
	   (long) htab_size (htab),
	   (long) htab_elements (htab),
	   htab_collisions (htab));
}

/* Record the fact that VAR has a nonzero value, though we may not know
   its exact value.  Note that if VAR is already known to have a nonzero
   value, then we do nothing.  */

static void
record_var_is_nonzero (tree var)
{
  int indx = SSA_NAME_VERSION (var);

  if (bitmap_bit_p (nonzero_vars, indx))
    return;

  /* Mark it in the global table.  */
  bitmap_set_bit (nonzero_vars, indx);

  /* Record this SSA_NAME so that we can reset the global table
     when we leave this block.  */
  VEC_safe_push (tree, heap, nonzero_vars_stack, var);
}

/* Enter a statement into the true/false expression hash table indicating
   that the condition COND has the value VALUE.  */

static void
record_cond (tree cond, tree value)
{
  struct expr_hash_elt *element = xmalloc (sizeof (struct expr_hash_elt));
  void **slot;

  initialize_hash_element (cond, value, element);

  slot = htab_find_slot_with_hash (avail_exprs, (void *)element,
				   element->hash, INSERT);
  if (*slot == NULL)
    {
      *slot = (void *) element;
      VEC_safe_push (tree, heap, avail_exprs_stack, cond);
    }
  else
    free (element);
}

/* Build a new conditional using NEW_CODE, OP0 and OP1 and store
   the new conditional into *p, then store a boolean_true_node
   into *(p + 1).  */
   
static void
build_and_record_new_cond (enum tree_code new_code, tree op0, tree op1, tree *p)
{
  *p = build2 (new_code, boolean_type_node, op0, op1);
  p++;
  *p = boolean_true_node;
}

/* Record that COND is true and INVERTED is false into the edge information
   structure.  Also record that any conditions dominated by COND are true
   as well.

   For example, if a < b is true, then a <= b must also be true.  */

static void
record_conditions (struct edge_info *edge_info, tree cond, tree inverted)
{
  tree op0, op1;

  if (!COMPARISON_CLASS_P (cond))
    return;

  op0 = TREE_OPERAND (cond, 0);
  op1 = TREE_OPERAND (cond, 1);

  switch (TREE_CODE (cond))
    {
    case LT_EXPR:
    case GT_EXPR:
      edge_info->max_cond_equivalences = 12;
      edge_info->cond_equivalences = xmalloc (12 * sizeof (tree));
      build_and_record_new_cond ((TREE_CODE (cond) == LT_EXPR
				  ? LE_EXPR : GE_EXPR),
				 op0, op1, &edge_info->cond_equivalences[4]);
      build_and_record_new_cond (ORDERED_EXPR, op0, op1,
				 &edge_info->cond_equivalences[6]);
      build_and_record_new_cond (NE_EXPR, op0, op1,
				 &edge_info->cond_equivalences[8]);
      build_and_record_new_cond (LTGT_EXPR, op0, op1,
				 &edge_info->cond_equivalences[10]);
      break;

    case GE_EXPR:
    case LE_EXPR:
      edge_info->max_cond_equivalences = 6;
      edge_info->cond_equivalences = xmalloc (6 * sizeof (tree));
      build_and_record_new_cond (ORDERED_EXPR, op0, op1,
				 &edge_info->cond_equivalences[4]);
      break;

    case EQ_EXPR:
      edge_info->max_cond_equivalences = 10;
      edge_info->cond_equivalences = xmalloc (10 * sizeof (tree));
      build_and_record_new_cond (ORDERED_EXPR, op0, op1,
				 &edge_info->cond_equivalences[4]);
      build_and_record_new_cond (LE_EXPR, op0, op1,
				 &edge_info->cond_equivalences[6]);
      build_and_record_new_cond (GE_EXPR, op0, op1,
				 &edge_info->cond_equivalences[8]);
      break;

    case UNORDERED_EXPR:
      edge_info->max_cond_equivalences = 16;
      edge_info->cond_equivalences = xmalloc (16 * sizeof (tree));
      build_and_record_new_cond (NE_EXPR, op0, op1,
				 &edge_info->cond_equivalences[4]);
      build_and_record_new_cond (UNLE_EXPR, op0, op1,
				 &edge_info->cond_equivalences[6]);
      build_and_record_new_cond (UNGE_EXPR, op0, op1,
				 &edge_info->cond_equivalences[8]);
      build_and_record_new_cond (UNEQ_EXPR, op0, op1,
				 &edge_info->cond_equivalences[10]);
      build_and_record_new_cond (UNLT_EXPR, op0, op1,
				 &edge_info->cond_equivalences[12]);
      build_and_record_new_cond (UNGT_EXPR, op0, op1,
				 &edge_info->cond_equivalences[14]);
      break;

    case UNLT_EXPR:
    case UNGT_EXPR:
      edge_info->max_cond_equivalences = 8;
      edge_info->cond_equivalences = xmalloc (8 * sizeof (tree));
      build_and_record_new_cond ((TREE_CODE (cond) == UNLT_EXPR
				  ? UNLE_EXPR : UNGE_EXPR),
				 op0, op1, &edge_info->cond_equivalences[4]);
      build_and_record_new_cond (NE_EXPR, op0, op1,
				 &edge_info->cond_equivalences[6]);
      break;

    case UNEQ_EXPR:
      edge_info->max_cond_equivalences = 8;
      edge_info->cond_equivalences = xmalloc (8 * sizeof (tree));
      build_and_record_new_cond (UNLE_EXPR, op0, op1,
				 &edge_info->cond_equivalences[4]);
      build_and_record_new_cond (UNGE_EXPR, op0, op1,
				 &edge_info->cond_equivalences[6]);
      break;

    case LTGT_EXPR:
      edge_info->max_cond_equivalences = 8;
      edge_info->cond_equivalences = xmalloc (8 * sizeof (tree));
      build_and_record_new_cond (NE_EXPR, op0, op1,
				 &edge_info->cond_equivalences[4]);
      build_and_record_new_cond (ORDERED_EXPR, op0, op1,
				 &edge_info->cond_equivalences[6]);
      break;

    default:
      edge_info->max_cond_equivalences = 4;
      edge_info->cond_equivalences = xmalloc (4 * sizeof (tree));
      break;
    }

  /* Now store the original true and false conditions into the first
     two slots.  */
  edge_info->cond_equivalences[0] = cond;
  edge_info->cond_equivalences[1] = boolean_true_node;
  edge_info->cond_equivalences[2] = inverted;
  edge_info->cond_equivalences[3] = boolean_false_node;
}

/* A helper function for record_const_or_copy and record_equality.
   Do the work of recording the value and undo info.  */

static void
record_const_or_copy_1 (tree x, tree y, tree prev_x)
{
  SSA_NAME_VALUE (x) = y;

  VEC_reserve (tree, heap, const_and_copies_stack, 2);
  VEC_quick_push (tree, const_and_copies_stack, prev_x);
  VEC_quick_push (tree, const_and_copies_stack, x);
}


/* Return the loop depth of the basic block of the defining statement of X.
   This number should not be treated as absolutely correct because the loop
   information may not be completely up-to-date when dom runs.  However, it
   will be relatively correct, and as more passes are taught to keep loop info
   up to date, the result will become more and more accurate.  */

int
loop_depth_of_name (tree x)
{
  tree defstmt;
  basic_block defbb;

  /* If it's not an SSA_NAME, we have no clue where the definition is.  */
  if (TREE_CODE (x) != SSA_NAME)
    return 0;

  /* Otherwise return the loop depth of the defining statement's bb.
     Note that there may not actually be a bb for this statement, if the
     ssa_name is live on entry.  */
  defstmt = SSA_NAME_DEF_STMT (x);
  defbb = bb_for_stmt (defstmt);
  if (!defbb)
    return 0;

  return defbb->loop_depth;
}


/* Record that X is equal to Y in const_and_copies.  Record undo
   information in the block-local vector.  */

static void
record_const_or_copy (tree x, tree y)
{
  tree prev_x = SSA_NAME_VALUE (x);

  if (TREE_CODE (y) == SSA_NAME)
    {
      tree tmp = SSA_NAME_VALUE (y);
      if (tmp)
	y = tmp;
    }

  record_const_or_copy_1 (x, y, prev_x);
}

/* Similarly, but assume that X and Y are the two operands of an EQ_EXPR.
   This constrains the cases in which we may treat this as assignment.  */

static void
record_equality (tree x, tree y)
{
  tree prev_x = NULL, prev_y = NULL;

  if (TREE_CODE (x) == SSA_NAME)
    prev_x = SSA_NAME_VALUE (x);
  if (TREE_CODE (y) == SSA_NAME)
    prev_y = SSA_NAME_VALUE (y);

  /* If one of the previous values is invariant, or invariant in more loops
     (by depth), then use that.
     Otherwise it doesn't matter which value we choose, just so
     long as we canonicalize on one value.  */
  if (TREE_INVARIANT (y))
    ;
  else if (TREE_INVARIANT (x) || (loop_depth_of_name (x) <= loop_depth_of_name (y)))
    prev_x = x, x = y, y = prev_x, prev_x = prev_y;
  else if (prev_x && TREE_INVARIANT (prev_x))
    x = y, y = prev_x, prev_x = prev_y;
  else if (prev_y && TREE_CODE (prev_y) != VALUE_HANDLE)
    y = prev_y;

  /* After the swapping, we must have one SSA_NAME.  */
  if (TREE_CODE (x) != SSA_NAME)
    return;

  /* For IEEE, -0.0 == 0.0, so we don't necessarily know the sign of a
     variable compared against zero.  If we're honoring signed zeros,
     then we cannot record this value unless we know that the value is
     nonzero.  */
  if (HONOR_SIGNED_ZEROS (TYPE_MODE (TREE_TYPE (x)))
      && (TREE_CODE (y) != REAL_CST
	  || REAL_VALUES_EQUAL (dconst0, TREE_REAL_CST (y))))
    return;

  record_const_or_copy_1 (x, y, prev_x);
}

/* Return true, if it is ok to do folding of an associative expression.
   EXP is the tree for the associative expression.  */ 

static inline bool
unsafe_associative_fp_binop (tree exp)
{
  enum tree_code code = TREE_CODE (exp);
  return !(!flag_unsafe_math_optimizations
           && (code == MULT_EXPR || code == PLUS_EXPR
	       || code == MINUS_EXPR)
           && FLOAT_TYPE_P (TREE_TYPE (exp)));
}

/* Returns true when STMT is a simple iv increment.  It detects the
   following situation:
   
   i_1 = phi (..., i_2)
   i_2 = i_1 +/- ...  */

static bool
simple_iv_increment_p (tree stmt)
{
  tree lhs, rhs, preinc, phi;
  unsigned i;

  if (TREE_CODE (stmt) != MODIFY_EXPR)
    return false;

  lhs = TREE_OPERAND (stmt, 0);
  if (TREE_CODE (lhs) != SSA_NAME)
    return false;

  rhs = TREE_OPERAND (stmt, 1);

  if (TREE_CODE (rhs) != PLUS_EXPR
      && TREE_CODE (rhs) != MINUS_EXPR)
    return false;

  preinc = TREE_OPERAND (rhs, 0);
  if (TREE_CODE (preinc) != SSA_NAME)
    return false;

  phi = SSA_NAME_DEF_STMT (preinc);
  if (TREE_CODE (phi) != PHI_NODE)
    return false;

  for (i = 0; i < (unsigned) PHI_NUM_ARGS (phi); i++)
    if (PHI_ARG_DEF (phi, i) == lhs)
      return true;

  return false;
}

/* STMT is a MODIFY_EXPR for which we were unable to find RHS in the
   hash tables.  Try to simplify the RHS using whatever equivalences
   we may have recorded.

   If we are able to simplify the RHS, then lookup the simplified form in
   the hash table and return the result.  Otherwise return NULL.  */

static tree
simplify_rhs_and_lookup_avail_expr (tree stmt, int insert)
{
  tree rhs = TREE_OPERAND (stmt, 1);
  enum tree_code rhs_code = TREE_CODE (rhs);
  tree result = NULL;

  /* If we have lhs = ~x, look and see if we earlier had x = ~y.
     In which case we can change this statement to be lhs = y.
     Which can then be copy propagated. 

     Similarly for negation.  */
  if ((rhs_code == BIT_NOT_EXPR || rhs_code == NEGATE_EXPR)
      && TREE_CODE (TREE_OPERAND (rhs, 0)) == SSA_NAME)
    {
      /* Get the definition statement for our RHS.  */
      tree rhs_def_stmt = SSA_NAME_DEF_STMT (TREE_OPERAND (rhs, 0));

      /* See if the RHS_DEF_STMT has the same form as our statement.  */
      if (TREE_CODE (rhs_def_stmt) == MODIFY_EXPR
	  && TREE_CODE (TREE_OPERAND (rhs_def_stmt, 1)) == rhs_code)
	{
	  tree rhs_def_operand;

	  rhs_def_operand = TREE_OPERAND (TREE_OPERAND (rhs_def_stmt, 1), 0);

	  /* Verify that RHS_DEF_OPERAND is a suitable SSA variable.  */
	  if (TREE_CODE (rhs_def_operand) == SSA_NAME
	      && ! SSA_NAME_OCCURS_IN_ABNORMAL_PHI (rhs_def_operand))
	    result = update_rhs_and_lookup_avail_expr (stmt,
						       rhs_def_operand,
						       insert);
	}
    }

  /* If we have z = (x OP C1), see if we earlier had x = y OP C2.
     If OP is associative, create and fold (y OP C2) OP C1 which
     should result in (y OP C3), use that as the RHS for the
     assignment.  Add minus to this, as we handle it specially below.  */
  if ((associative_tree_code (rhs_code) || rhs_code == MINUS_EXPR)
      && TREE_CODE (TREE_OPERAND (rhs, 0)) == SSA_NAME
      && is_gimple_min_invariant (TREE_OPERAND (rhs, 1)))
    {
      tree rhs_def_stmt = SSA_NAME_DEF_STMT (TREE_OPERAND (rhs, 0));

      /* If the statement defines an induction variable, do not propagate
	 its value, so that we do not create overlapping life ranges.  */
      if (simple_iv_increment_p (rhs_def_stmt))
	goto dont_fold_assoc;

      /* See if the RHS_DEF_STMT has the same form as our statement.  */
      if (TREE_CODE (rhs_def_stmt) == MODIFY_EXPR)
	{
	  tree rhs_def_rhs = TREE_OPERAND (rhs_def_stmt, 1);
	  enum tree_code rhs_def_code = TREE_CODE (rhs_def_rhs);

	  if ((rhs_code == rhs_def_code && unsafe_associative_fp_binop (rhs))
	      || (rhs_code == PLUS_EXPR && rhs_def_code == MINUS_EXPR)
	      || (rhs_code == MINUS_EXPR && rhs_def_code == PLUS_EXPR))
	    {
	      tree def_stmt_op0 = TREE_OPERAND (rhs_def_rhs, 0);
	      tree def_stmt_op1 = TREE_OPERAND (rhs_def_rhs, 1);

	      if (TREE_CODE (def_stmt_op0) == SSA_NAME
		  && ! SSA_NAME_OCCURS_IN_ABNORMAL_PHI (def_stmt_op0)
		  && is_gimple_min_invariant (def_stmt_op1))
		{
		  tree outer_const = TREE_OPERAND (rhs, 1);
		  tree type = TREE_TYPE (TREE_OPERAND (stmt, 0));
		  tree t;

		  /* If we care about correct floating point results, then
		     don't fold x + c1 - c2.  Note that we need to take both
		     the codes and the signs to figure this out.  */
		  if (FLOAT_TYPE_P (type)
		      && !flag_unsafe_math_optimizations
		      && (rhs_def_code == PLUS_EXPR
			  || rhs_def_code == MINUS_EXPR))
		    {
		      bool neg = false;

		      neg ^= (rhs_code == MINUS_EXPR);
		      neg ^= (rhs_def_code == MINUS_EXPR);
		      neg ^= real_isneg (TREE_REAL_CST_PTR (outer_const));
		      neg ^= real_isneg (TREE_REAL_CST_PTR (def_stmt_op1));

		      if (neg)
			goto dont_fold_assoc;
		    }

		  /* Ho hum.  So fold will only operate on the outermost
		     thingy that we give it, so we have to build the new
		     expression in two pieces.  This requires that we handle
		     combinations of plus and minus.  */
		  if (rhs_def_code != rhs_code)
		    {
		      if (rhs_def_code == MINUS_EXPR)
		        t = build (MINUS_EXPR, type, outer_const, def_stmt_op1);
		      else
		        t = build (MINUS_EXPR, type, def_stmt_op1, outer_const);
		      rhs_code = PLUS_EXPR;
		    }
		  else if (rhs_def_code == MINUS_EXPR)
		    t = build (PLUS_EXPR, type, def_stmt_op1, outer_const);
		  else
		    t = build (rhs_def_code, type, def_stmt_op1, outer_const);
		  t = local_fold (t);
		  t = build (rhs_code, type, def_stmt_op0, t);
		  t = local_fold (t);

		  /* If the result is a suitable looking gimple expression,
		     then use it instead of the original for STMT.  */
		  if (TREE_CODE (t) == SSA_NAME
		      || (UNARY_CLASS_P (t)
			  && TREE_CODE (TREE_OPERAND (t, 0)) == SSA_NAME)
		      || ((BINARY_CLASS_P (t) || COMPARISON_CLASS_P (t))
			  && TREE_CODE (TREE_OPERAND (t, 0)) == SSA_NAME
			  && is_gimple_val (TREE_OPERAND (t, 1))))
		    result = update_rhs_and_lookup_avail_expr (stmt, t, insert);
		}
	    }
	}
 dont_fold_assoc:;
    }

  /* Optimize *"foo" into 'f'.  This is done here rather than
     in fold to avoid problems with stuff like &*"foo".  */
  if (TREE_CODE (rhs) == INDIRECT_REF || TREE_CODE (rhs) == ARRAY_REF)
    {
      tree t = fold_read_from_constant_string (rhs);

      if (t)
        result = update_rhs_and_lookup_avail_expr (stmt, t, insert);
    }

  return result;
}

/* COND is a condition of the form:

     x == const or x != const

   Look back to x's defining statement and see if x is defined as

     x = (type) y;

   If const is unchanged if we convert it to type, then we can build
   the equivalent expression:


      y == const or y != const

   Which may allow further optimizations.

   Return the equivalent comparison or NULL if no such equivalent comparison
   was found.  */

static tree
find_equivalent_equality_comparison (tree cond)
{
  tree op0 = TREE_OPERAND (cond, 0);
  tree op1 = TREE_OPERAND (cond, 1);
  tree def_stmt = SSA_NAME_DEF_STMT (op0);

  /* OP0 might have been a parameter, so first make sure it
     was defined by a MODIFY_EXPR.  */
  if (def_stmt && TREE_CODE (def_stmt) == MODIFY_EXPR)
    {
      tree def_rhs = TREE_OPERAND (def_stmt, 1);


      /* If either operand to the comparison is a pointer to
	 a function, then we can not apply this optimization
	 as some targets require function pointers to be
	 canonicalized and in this case this optimization would
	 eliminate a necessary canonicalization.  */
      if ((POINTER_TYPE_P (TREE_TYPE (op0))
	   && TREE_CODE (TREE_TYPE (TREE_TYPE (op0))) == FUNCTION_TYPE)
	  || (POINTER_TYPE_P (TREE_TYPE (op1))
	      && TREE_CODE (TREE_TYPE (TREE_TYPE (op1))) == FUNCTION_TYPE))
	return NULL;
	      
      /* Now make sure the RHS of the MODIFY_EXPR is a typecast.  */
      if ((TREE_CODE (def_rhs) == NOP_EXPR
	   || TREE_CODE (def_rhs) == CONVERT_EXPR)
	  && TREE_CODE (TREE_OPERAND (def_rhs, 0)) == SSA_NAME)
	{
	  tree def_rhs_inner = TREE_OPERAND (def_rhs, 0);
	  tree def_rhs_inner_type = TREE_TYPE (def_rhs_inner);
	  tree new;

	  if (TYPE_PRECISION (def_rhs_inner_type)
	      > TYPE_PRECISION (TREE_TYPE (def_rhs)))
	    return NULL;

	  /* If the inner type of the conversion is a pointer to
	     a function, then we can not apply this optimization
	     as some targets require function pointers to be
	     canonicalized.  This optimization would result in
	     canonicalization of the pointer when it was not originally
	     needed/intended.  */
	  if (POINTER_TYPE_P (def_rhs_inner_type)
	      && TREE_CODE (TREE_TYPE (def_rhs_inner_type)) == FUNCTION_TYPE)
	    return NULL;

	  /* What we want to prove is that if we convert OP1 to
	     the type of the object inside the NOP_EXPR that the
	     result is still equivalent to SRC. 

	     If that is true, the build and return new equivalent
	     condition which uses the source of the typecast and the
	     new constant (which has only changed its type).  */
	  new = build1 (TREE_CODE (def_rhs), def_rhs_inner_type, op1);
	  new = local_fold (new);
	  if (is_gimple_val (new) && tree_int_cst_equal (new, op1))
	    return build (TREE_CODE (cond), TREE_TYPE (cond),
			  def_rhs_inner, new);
	}
    }
  return NULL;
}

/* STMT is a COND_EXPR for which we could not trivially determine its
   result.  This routine attempts to find equivalent forms of the
   condition which we may be able to optimize better.  It also 
   uses simple value range propagation to optimize conditionals.  */

static tree
simplify_cond_and_lookup_avail_expr (tree stmt,
				     stmt_ann_t ann,
				     int insert)
{
  tree cond = COND_EXPR_COND (stmt);

  if (COMPARISON_CLASS_P (cond))
    {
      tree op0 = TREE_OPERAND (cond, 0);
      tree op1 = TREE_OPERAND (cond, 1);

      if (TREE_CODE (op0) == SSA_NAME && is_gimple_min_invariant (op1))
	{
	  int limit;
	  tree low, high, cond_low, cond_high;
	  int lowequal, highequal, swapped, no_overlap, subset, cond_inverted;
	  VEC(vrp_element_p,heap) **vrp_records;
	  struct vrp_element *element;
	  struct vrp_hash_elt vrp_hash_elt, *vrp_hash_elt_p;
	  void **slot;

	  /* First see if we have test of an SSA_NAME against a constant
	     where the SSA_NAME is defined by an earlier typecast which
	     is irrelevant when performing tests against the given
	     constant.  */
	  if (TREE_CODE (cond) == EQ_EXPR || TREE_CODE (cond) == NE_EXPR)
	    {
	      tree new_cond = find_equivalent_equality_comparison (cond);

	      if (new_cond)
		{
		  /* Update the statement to use the new equivalent
		     condition.  */
		  COND_EXPR_COND (stmt) = new_cond;

		  /* If this is not a real stmt, ann will be NULL and we
		     avoid processing the operands.  */
		  if (ann)
		    mark_stmt_modified (stmt);

		  /* Lookup the condition and return its known value if it
		     exists.  */
		  new_cond = lookup_avail_expr (stmt, insert);
		  if (new_cond)
		    return new_cond;

		  /* The operands have changed, so update op0 and op1.  */
		  op0 = TREE_OPERAND (cond, 0);
		  op1 = TREE_OPERAND (cond, 1);
		}
	    }

	  /* Consult the value range records for this variable (if they exist)
	     to see if we can eliminate or simplify this conditional. 

	     Note two tests are necessary to determine no records exist.
	     First we have to see if the virtual array exists, if it 
	     exists, then we have to check its active size. 

	     Also note the vast majority of conditionals are not testing
	     a variable which has had its range constrained by an earlier
	     conditional.  So this filter avoids a lot of unnecessary work.  */
	  vrp_hash_elt.var = op0;
	  vrp_hash_elt.records = NULL;
          slot = htab_find_slot (vrp_data, &vrp_hash_elt, NO_INSERT);
          if (slot == NULL)
	    return NULL;

	  vrp_hash_elt_p = (struct vrp_hash_elt *) *slot;
	  vrp_records = &vrp_hash_elt_p->records;

	  limit = VEC_length (vrp_element_p, *vrp_records);

	  /* If we have no value range records for this variable, or we are
	     unable to extract a range for this condition, then there is
	     nothing to do.  */
	  if (limit == 0
	      || ! extract_range_from_cond (cond, &cond_high,
					    &cond_low, &cond_inverted))
	    return NULL;

	  /* We really want to avoid unnecessary computations of range
	     info.  So all ranges are computed lazily; this avoids a
	     lot of unnecessary work.  i.e., we record the conditional,
	     but do not process how it constrains the variable's 
	     potential values until we know that processing the condition
	     could be helpful.

	     However, we do not want to have to walk a potentially long
	     list of ranges, nor do we want to compute a variable's
	     range more than once for a given path.

	     Luckily, each time we encounter a conditional that can not
	     be otherwise optimized we will end up here and we will
	     compute the necessary range information for the variable
	     used in this condition.

	     Thus you can conclude that there will never be more than one
	     conditional associated with a variable which has not been
	     processed.  So we never need to merge more than one new
	     conditional into the current range. 

	     These properties also help us avoid unnecessary work.  */
	   element = VEC_last (vrp_element_p, *vrp_records);

	  if (element->high && element->low)
	    {
	      /* The last element has been processed, so there is no range
		 merging to do, we can simply use the high/low values
		 recorded in the last element.  */
	      low = element->low;
	      high = element->high;
	    }
	  else
	    {
	      tree tmp_high, tmp_low;
	      int dummy;

	      /* The last element has not been processed.  Process it now.
		 record_range should ensure for cond inverted is not set.
		 This call can only fail if cond is x < min or x > max,
		 which fold should have optimized into false.
		 If that doesn't happen, just pretend all values are
		 in the range.  */
	      if (! extract_range_from_cond (element->cond, &tmp_high,
					     &tmp_low, &dummy))
		gcc_unreachable ();
	      else
		gcc_assert (dummy == 0);

	      /* If this is the only element, then no merging is necessary, 
		 the high/low values from extract_range_from_cond are all
		 we need.  */
	      if (limit == 1)
		{
		  low = tmp_low;
		  high = tmp_high;
		}
	      else
		{
		  /* Get the high/low value from the previous element.  */
		  struct vrp_element *prev
		    = VEC_index (vrp_element_p, *vrp_records, limit - 2);
		  low = prev->low;
		  high = prev->high;

		  /* Merge in this element's range with the range from the
		     previous element.

		     The low value for the merged range is the maximum of
		     the previous low value and the low value of this record.

		     Similarly the high value for the merged range is the
		     minimum of the previous high value and the high value of
		     this record.  */
		  low = (low && tree_int_cst_compare (low, tmp_low) == 1
			 ? low : tmp_low);
		  high = (high && tree_int_cst_compare (high, tmp_high) == -1
			  ? high : tmp_high);
		}

	      /* And record the computed range.  */
	      element->low = low;
	      element->high = high;

	    }

	  /* After we have constrained this variable's potential values,
	     we try to determine the result of the given conditional.

	     To simplify later tests, first determine if the current
	     low value is the same low value as the conditional.
	     Similarly for the current high value and the high value
	     for the conditional.  */
	  lowequal = tree_int_cst_equal (low, cond_low);
	  highequal = tree_int_cst_equal (high, cond_high);

	  if (lowequal && highequal)
	    return (cond_inverted ? boolean_false_node : boolean_true_node);

	  /* To simplify the overlap/subset tests below we may want
	     to swap the two ranges so that the larger of the two
	     ranges occurs "first".  */
	  swapped = 0;
	  if (tree_int_cst_compare (low, cond_low) == 1
	      || (lowequal 
		  && tree_int_cst_compare (cond_high, high) == 1))
	    {
	      tree temp;

	      swapped = 1;
	      temp = low;
	      low = cond_low;
	      cond_low = temp;
	      temp = high;
	      high = cond_high;
	      cond_high = temp;
	    }

	  /* Now determine if there is no overlap in the ranges
	     or if the second range is a subset of the first range.  */
	  no_overlap = tree_int_cst_lt (high, cond_low);
	  subset = tree_int_cst_compare (cond_high, high) != 1;

	  /* If there was no overlap in the ranges, then this conditional
	     always has a false value (unless we had to invert this
	     conditional, in which case it always has a true value).  */
	  if (no_overlap)
	    return (cond_inverted ? boolean_true_node : boolean_false_node);

	  /* If the current range is a subset of the condition's range,
	     then this conditional always has a true value (unless we
	     had to invert this conditional, in which case it always
	     has a true value).  */
	  if (subset && swapped)
	    return (cond_inverted ? boolean_false_node : boolean_true_node);

	  /* We were unable to determine the result of the conditional.
	     However, we may be able to simplify the conditional.  First
	     merge the ranges in the same manner as range merging above.  */
	  low = tree_int_cst_compare (low, cond_low) == 1 ? low : cond_low;
	  high = tree_int_cst_compare (high, cond_high) == -1 ? high : cond_high;
	  
	  /* If the range has converged to a single point, then turn this
	     into an equality comparison.  */
	  if (TREE_CODE (cond) != EQ_EXPR
	      && TREE_CODE (cond) != NE_EXPR
	      && tree_int_cst_equal (low, high))
	    {
	      TREE_SET_CODE (cond, EQ_EXPR);
	      TREE_OPERAND (cond, 1) = high;
	    }
	}
    }
  return 0;
}

/* STMT is a SWITCH_EXPR for which we could not trivially determine its
   result.  This routine attempts to find equivalent forms of the
   condition which we may be able to optimize better.  */

static tree
simplify_switch_and_lookup_avail_expr (tree stmt, int insert)
{
  tree cond = SWITCH_COND (stmt);
  tree def, to, ti;

  /* The optimization that we really care about is removing unnecessary
     casts.  That will let us do much better in propagating the inferred
     constant at the switch target.  */
  if (TREE_CODE (cond) == SSA_NAME)
    {
      def = SSA_NAME_DEF_STMT (cond);
      if (TREE_CODE (def) == MODIFY_EXPR)
	{
	  def = TREE_OPERAND (def, 1);
	  if (TREE_CODE (def) == NOP_EXPR)
	    {
	      int need_precision;
	      bool fail;

	      def = TREE_OPERAND (def, 0);

#ifdef ENABLE_CHECKING
	      /* ??? Why was Jeff testing this?  We are gimple...  */
	      gcc_assert (is_gimple_val (def));
#endif

	      to = TREE_TYPE (cond);
	      ti = TREE_TYPE (def);

	      /* If we have an extension that preserves value, then we
		 can copy the source value into the switch.  */

	      need_precision = TYPE_PRECISION (ti);
	      fail = false;
	      if (TYPE_UNSIGNED (to) && !TYPE_UNSIGNED (ti))
		fail = true;
	      else if (!TYPE_UNSIGNED (to) && TYPE_UNSIGNED (ti))
		need_precision += 1;
	      if (TYPE_PRECISION (to) < need_precision)
		fail = true;

	      if (!fail)
		{
		  SWITCH_COND (stmt) = def;
		  mark_stmt_modified (stmt);

		  return lookup_avail_expr (stmt, insert);
		}
	    }
	}
    }

  return 0;
}


/* CONST_AND_COPIES is a table which maps an SSA_NAME to the current
   known value for that SSA_NAME (or NULL if no value is known).  

   NONZERO_VARS is the set SSA_NAMES known to have a nonzero value,
   even if we don't know their precise value.

   Propagate values from CONST_AND_COPIES and NONZERO_VARS into the PHI
   nodes of the successors of BB.  */

static void
cprop_into_successor_phis (basic_block bb, bitmap nonzero_vars)
{
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      tree phi;
      int indx;

      /* If this is an abnormal edge, then we do not want to copy propagate
	 into the PHI alternative associated with this edge.  */
      if (e->flags & EDGE_ABNORMAL)
	continue;

      phi = phi_nodes (e->dest);
      if (! phi)
	continue;

      indx = e->dest_idx;
      for ( ; phi; phi = PHI_CHAIN (phi))
	{
	  tree new;
	  use_operand_p orig_p;
	  tree orig;

	  /* The alternative may be associated with a constant, so verify
	     it is an SSA_NAME before doing anything with it.  */
	  orig_p = PHI_ARG_DEF_PTR (phi, indx);
	  orig = USE_FROM_PTR (orig_p);
	  if (TREE_CODE (orig) != SSA_NAME)
	    continue;

	  /* If the alternative is known to have a nonzero value, record
	     that fact in the PHI node itself for future use.  */
	  if (bitmap_bit_p (nonzero_vars, SSA_NAME_VERSION (orig)))
	    PHI_ARG_NONZERO (phi, indx) = true;

	  /* If we have *ORIG_P in our constant/copy table, then replace
	     ORIG_P with its value in our constant/copy table.  */
	  new = SSA_NAME_VALUE (orig);
	  if (new
	      && new != orig
	      && (TREE_CODE (new) == SSA_NAME
		  || is_gimple_min_invariant (new))
	      && may_propagate_copy (orig, new))
	    propagate_value (orig_p, new);
	}
    }
}

/* We have finished optimizing BB, record any information implied by
   taking a specific outgoing edge from BB.  */

static void
record_edge_info (basic_block bb)
{
  block_stmt_iterator bsi = bsi_last (bb);
  struct edge_info *edge_info;

  if (! bsi_end_p (bsi))
    {
      tree stmt = bsi_stmt (bsi);

      if (stmt && TREE_CODE (stmt) == SWITCH_EXPR)
	{
	  tree cond = SWITCH_COND (stmt);

	  if (TREE_CODE (cond) == SSA_NAME)
	    {
	      tree labels = SWITCH_LABELS (stmt);
	      int i, n_labels = TREE_VEC_LENGTH (labels);
	      tree *info = xcalloc (last_basic_block, sizeof (tree));
	      edge e;
	      edge_iterator ei;

	      for (i = 0; i < n_labels; i++)
		{
		  tree label = TREE_VEC_ELT (labels, i);
		  basic_block target_bb = label_to_block (CASE_LABEL (label));

		  if (CASE_HIGH (label)
		      || !CASE_LOW (label)
		      || info[target_bb->index])
		    info[target_bb->index] = error_mark_node;
		  else
		    info[target_bb->index] = label;
		}

	      FOR_EACH_EDGE (e, ei, bb->succs)
		{
		  basic_block target_bb = e->dest;
		  tree node = info[target_bb->index];

		  if (node != NULL && node != error_mark_node)
		    {
		      tree x = fold_convert (TREE_TYPE (cond), CASE_LOW (node));
		      edge_info = allocate_edge_info (e);
		      edge_info->lhs = cond;
		      edge_info->rhs = x;
		    }
		}
	      free (info);
	    }
	}

      /* A COND_EXPR may create equivalences too.  */
      if (stmt && TREE_CODE (stmt) == COND_EXPR)
	{
	  tree cond = COND_EXPR_COND (stmt);
	  edge true_edge;
	  edge false_edge;

	  extract_true_false_edges_from_block (bb, &true_edge, &false_edge);

	  /* If the conditional is a single variable 'X', record 'X = 1'
	     for the true edge and 'X = 0' on the false edge.  */
	  if (SSA_VAR_P (cond))
	    {
	      struct edge_info *edge_info;

	      edge_info = allocate_edge_info (true_edge);
	      edge_info->lhs = cond;
	      edge_info->rhs = constant_boolean_node (1, TREE_TYPE (cond));

	      edge_info = allocate_edge_info (false_edge);
	      edge_info->lhs = cond;
	      edge_info->rhs = constant_boolean_node (0, TREE_TYPE (cond));
	    }
	  /* Equality tests may create one or two equivalences.  */
	  else if (COMPARISON_CLASS_P (cond))
	    {
	      tree op0 = TREE_OPERAND (cond, 0);
	      tree op1 = TREE_OPERAND (cond, 1);

	      /* Special case comparing booleans against a constant as we
		 know the value of OP0 on both arms of the branch.  i.e., we
		 can record an equivalence for OP0 rather than COND.  */
	      if ((TREE_CODE (cond) == EQ_EXPR || TREE_CODE (cond) == NE_EXPR)
		  && TREE_CODE (op0) == SSA_NAME
		  && TREE_CODE (TREE_TYPE (op0)) == BOOLEAN_TYPE
		  && is_gimple_min_invariant (op1))
		{
		  if (TREE_CODE (cond) == EQ_EXPR)
		    {
		      edge_info = allocate_edge_info (true_edge);
		      edge_info->lhs = op0;
		      edge_info->rhs = (integer_zerop (op1)
					    ? boolean_false_node
					    : boolean_true_node);

		      edge_info = allocate_edge_info (false_edge);
		      edge_info->lhs = op0;
		      edge_info->rhs = (integer_zerop (op1)
					    ? boolean_true_node
					    : boolean_false_node);
		    }
		  else
		    {
		      edge_info = allocate_edge_info (true_edge);
		      edge_info->lhs = op0;
		      edge_info->rhs = (integer_zerop (op1)
					    ? boolean_true_node
					    : boolean_false_node);

		      edge_info = allocate_edge_info (false_edge);
		      edge_info->lhs = op0;
		      edge_info->rhs = (integer_zerop (op1)
					    ? boolean_false_node
					    : boolean_true_node);
		    }
		}

	      else if (is_gimple_min_invariant (op0)
		       && (TREE_CODE (op1) == SSA_NAME
			   || is_gimple_min_invariant (op1)))
		{
		  tree inverted = invert_truthvalue (cond);
		  struct edge_info *edge_info;

		  edge_info = allocate_edge_info (true_edge);
		  record_conditions (edge_info, cond, inverted);

		  if (TREE_CODE (cond) == EQ_EXPR)
		    {
		      edge_info->lhs = op1;
		      edge_info->rhs = op0;
		    }

		  edge_info = allocate_edge_info (false_edge);
		  record_conditions (edge_info, inverted, cond);

		  if (TREE_CODE (cond) == NE_EXPR)
		    {
		      edge_info->lhs = op1;
		      edge_info->rhs = op0;
		    }
		}

	      else if (TREE_CODE (op0) == SSA_NAME
		       && (is_gimple_min_invariant (op1)
			   || TREE_CODE (op1) == SSA_NAME))
		{
		  tree inverted = invert_truthvalue (cond);
		  struct edge_info *edge_info;

		  edge_info = allocate_edge_info (true_edge);
		  record_conditions (edge_info, cond, inverted);

		  if (TREE_CODE (cond) == EQ_EXPR)
		    {
		      edge_info->lhs = op0;
		      edge_info->rhs = op1;
		    }

		  edge_info = allocate_edge_info (false_edge);
		  record_conditions (edge_info, inverted, cond);

		  if (TREE_CODE (cond) == NE_EXPR)
		    {
		      edge_info->lhs = op0;
		      edge_info->rhs = op1;
		    }
		}
	    }

	  /* ??? TRUTH_NOT_EXPR can create an equivalence too.  */
	}
    }
}

/* Propagate information from BB to its outgoing edges.

   This can include equivalency information implied by control statements
   at the end of BB and const/copy propagation into PHIs in BB's
   successor blocks.  */

static void
propagate_to_outgoing_edges (struct dom_walk_data *walk_data ATTRIBUTE_UNUSED,
			     basic_block bb)
{
  record_edge_info (bb);
  cprop_into_successor_phis (bb, nonzero_vars);
}

/* Search for redundant computations in STMT.  If any are found, then
   replace them with the variable holding the result of the computation.

   If safe, record this expression into the available expression hash
   table.  */

static bool
eliminate_redundant_computations (tree stmt, stmt_ann_t ann)
{
  tree *expr_p, def = NULL_TREE;
  bool insert = true;
  tree cached_lhs;
  bool retval = false;
  bool modify_expr_p = false;

  if (TREE_CODE (stmt) == MODIFY_EXPR)
    def = TREE_OPERAND (stmt, 0);

  /* Certain expressions on the RHS can be optimized away, but can not
     themselves be entered into the hash tables.  */
  if (ann->makes_aliased_stores
      || ! def
      || TREE_CODE (def) != SSA_NAME
      || SSA_NAME_OCCURS_IN_ABNORMAL_PHI (def)
      || !ZERO_SSA_OPERANDS (stmt, SSA_OP_VMAYDEF)
      /* Do not record equivalences for increments of ivs.  This would create
	 overlapping live ranges for a very questionable gain.  */
      || simple_iv_increment_p (stmt))
    insert = false;

  /* Check if the expression has been computed before.  */
  cached_lhs = lookup_avail_expr (stmt, insert);

  /* If this is an assignment and the RHS was not in the hash table,
     then try to simplify the RHS and lookup the new RHS in the
     hash table.  */
  if (! cached_lhs && TREE_CODE (stmt) == MODIFY_EXPR)
    cached_lhs = simplify_rhs_and_lookup_avail_expr (stmt, insert);
  /* Similarly if this is a COND_EXPR and we did not find its
     expression in the hash table, simplify the condition and
     try again.  */
  else if (! cached_lhs && TREE_CODE (stmt) == COND_EXPR)
    cached_lhs = simplify_cond_and_lookup_avail_expr (stmt, ann, insert);
  /* Similarly for a SWITCH_EXPR.  */
  else if (!cached_lhs && TREE_CODE (stmt) == SWITCH_EXPR)
    cached_lhs = simplify_switch_and_lookup_avail_expr (stmt, insert);

  opt_stats.num_exprs_considered++;

  /* Get a pointer to the expression we are trying to optimize.  */
  if (TREE_CODE (stmt) == COND_EXPR)
    expr_p = &COND_EXPR_COND (stmt);
  else if (TREE_CODE (stmt) == SWITCH_EXPR)
    expr_p = &SWITCH_COND (stmt);
  else if (TREE_CODE (stmt) == RETURN_EXPR && TREE_OPERAND (stmt, 0))
    {
      expr_p = &TREE_OPERAND (TREE_OPERAND (stmt, 0), 1);
      modify_expr_p = true;
    }
  else
    {
      expr_p = &TREE_OPERAND (stmt, 1);
      modify_expr_p = true;
    }

  /* It is safe to ignore types here since we have already done
     type checking in the hashing and equality routines.  In fact
     type checking here merely gets in the way of constant
     propagation.  Also, make sure that it is safe to propagate
     CACHED_LHS into *EXPR_P.  */
  if (cached_lhs
      && ((TREE_CODE (cached_lhs) != SSA_NAME
	   && (modify_expr_p
	       || tree_ssa_useless_type_conversion_1 (TREE_TYPE (*expr_p),
						      TREE_TYPE (cached_lhs))))
	  || may_propagate_copy (*expr_p, cached_lhs)))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "  Replaced redundant expr '");
	  print_generic_expr (dump_file, *expr_p, dump_flags);
	  fprintf (dump_file, "' with '");
	  print_generic_expr (dump_file, cached_lhs, dump_flags);
	   fprintf (dump_file, "'\n");
	}

      opt_stats.num_re++;

#if defined ENABLE_CHECKING
      gcc_assert (TREE_CODE (cached_lhs) == SSA_NAME
		  || is_gimple_min_invariant (cached_lhs));
#endif

      if (TREE_CODE (cached_lhs) == ADDR_EXPR
	  || (POINTER_TYPE_P (TREE_TYPE (*expr_p))
	      && is_gimple_min_invariant (cached_lhs)))
	retval = true;
      
      if (modify_expr_p
	  && !tree_ssa_useless_type_conversion_1 (TREE_TYPE (*expr_p),
						  TREE_TYPE (cached_lhs)))
	cached_lhs = fold_convert (TREE_TYPE (*expr_p), cached_lhs);

      propagate_tree_value (expr_p, cached_lhs);
      mark_stmt_modified (stmt);
    }
  return retval;
}

/* STMT, a MODIFY_EXPR, may create certain equivalences, in either
   the available expressions table or the const_and_copies table.
   Detect and record those equivalences.  */

static void
record_equivalences_from_stmt (tree stmt,
			       int may_optimize_p,
			       stmt_ann_t ann)
{
  tree lhs = TREE_OPERAND (stmt, 0);
  enum tree_code lhs_code = TREE_CODE (lhs);
  int i;

  if (lhs_code == SSA_NAME)
    {
      tree rhs = TREE_OPERAND (stmt, 1);

      /* Strip away any useless type conversions.  */
      STRIP_USELESS_TYPE_CONVERSION (rhs);

      /* If the RHS of the assignment is a constant or another variable that
	 may be propagated, register it in the CONST_AND_COPIES table.  We
	 do not need to record unwind data for this, since this is a true
	 assignment and not an equivalence inferred from a comparison.  All
	 uses of this ssa name are dominated by this assignment, so unwinding
	 just costs time and space.  */
      if (may_optimize_p
	  && (TREE_CODE (rhs) == SSA_NAME
	      || is_gimple_min_invariant (rhs)))
	SSA_NAME_VALUE (lhs) = rhs;

      if (tree_expr_nonzero_p (rhs))
	record_var_is_nonzero (lhs);
    }

  /* Look at both sides for pointer dereferences.  If we find one, then
     the pointer must be nonnull and we can enter that equivalence into
     the hash tables.  */
  if (flag_delete_null_pointer_checks)
    for (i = 0; i < 2; i++)
      {
	tree t = TREE_OPERAND (stmt, i);

	/* Strip away any COMPONENT_REFs.  */
	while (TREE_CODE (t) == COMPONENT_REF)
	  t = TREE_OPERAND (t, 0);

	/* Now see if this is a pointer dereference.  */
	if (INDIRECT_REF_P (t))
          {
	    tree op = TREE_OPERAND (t, 0);

	    /* If the pointer is a SSA variable, then enter new
	       equivalences into the hash table.  */
	    while (TREE_CODE (op) == SSA_NAME)
	      {
		tree def = SSA_NAME_DEF_STMT (op);

		record_var_is_nonzero (op);

		/* And walk up the USE-DEF chains noting other SSA_NAMEs
		   which are known to have a nonzero value.  */
		if (def
		    && TREE_CODE (def) == MODIFY_EXPR
		    && TREE_CODE (TREE_OPERAND (def, 1)) == NOP_EXPR)
		  op = TREE_OPERAND (TREE_OPERAND (def, 1), 0);
		else
		  break;
	      }
	  }
      }

  /* A memory store, even an aliased store, creates a useful
     equivalence.  By exchanging the LHS and RHS, creating suitable
     vops and recording the result in the available expression table,
     we may be able to expose more redundant loads.  */
  if (!ann->has_volatile_ops
      && (TREE_CODE (TREE_OPERAND (stmt, 1)) == SSA_NAME
	  || is_gimple_min_invariant (TREE_OPERAND (stmt, 1)))
      && !is_gimple_reg (lhs))
    {
      tree rhs = TREE_OPERAND (stmt, 1);
      tree new;

      /* FIXME: If the LHS of the assignment is a bitfield and the RHS
         is a constant, we need to adjust the constant to fit into the
         type of the LHS.  If the LHS is a bitfield and the RHS is not
	 a constant, then we can not record any equivalences for this
	 statement since we would need to represent the widening or
	 narrowing of RHS.  This fixes gcc.c-torture/execute/921016-1.c
	 and should not be necessary if GCC represented bitfields
	 properly.  */
      if (lhs_code == COMPONENT_REF
	  && DECL_BIT_FIELD (TREE_OPERAND (lhs, 1)))
	{
	  if (TREE_CONSTANT (rhs))
	    rhs = widen_bitfield (rhs, TREE_OPERAND (lhs, 1), lhs);
	  else
	    rhs = NULL;

	  /* If the value overflowed, then we can not use this equivalence.  */
	  if (rhs && ! is_gimple_min_invariant (rhs))
	    rhs = NULL;
	}

      if (rhs)
	{
	  /* Build a new statement with the RHS and LHS exchanged.  */
	  new = build (MODIFY_EXPR, TREE_TYPE (stmt), rhs, lhs);

	  create_ssa_artficial_load_stmt (new, stmt);

	  /* Finally enter the statement into the available expression
	     table.  */
	  lookup_avail_expr (new, true);
	}
    }
}

/* Replace *OP_P in STMT with any known equivalent value for *OP_P from
   CONST_AND_COPIES.  */

static bool
cprop_operand (tree stmt, use_operand_p op_p)
{
  bool may_have_exposed_new_symbols = false;
  tree val;
  tree op = USE_FROM_PTR (op_p);

  /* If the operand has a known constant value or it is known to be a
     copy of some other variable, use the value or copy stored in
     CONST_AND_COPIES.  */
  val = SSA_NAME_VALUE (op);
  if (val && val != op && TREE_CODE (val) != VALUE_HANDLE)
    {
      tree op_type, val_type;

      /* Do not change the base variable in the virtual operand
	 tables.  That would make it impossible to reconstruct
	 the renamed virtual operand if we later modify this
	 statement.  Also only allow the new value to be an SSA_NAME
	 for propagation into virtual operands.  */
      if (!is_gimple_reg (op)
	  && (TREE_CODE (val) != SSA_NAME
	      || is_gimple_reg (val)
	      || get_virtual_var (val) != get_virtual_var (op)))
	return false;

      /* Do not replace hard register operands in asm statements.  */
      if (TREE_CODE (stmt) == ASM_EXPR
	  && !may_propagate_copy_into_asm (op))
	return false;

      /* Get the toplevel type of each operand.  */
      op_type = TREE_TYPE (op);
      val_type = TREE_TYPE (val);

      /* While both types are pointers, get the type of the object
	 pointed to.  */
      while (POINTER_TYPE_P (op_type) && POINTER_TYPE_P (val_type))
	{
	  op_type = TREE_TYPE (op_type);
	  val_type = TREE_TYPE (val_type);
	}

      /* Make sure underlying types match before propagating a constant by
	 converting the constant to the proper type.  Note that convert may
	 return a non-gimple expression, in which case we ignore this
	 propagation opportunity.  */
      if (TREE_CODE (val) != SSA_NAME)
	{
	  if (!lang_hooks.types_compatible_p (op_type, val_type))
	    {
	      val = fold_convert (TREE_TYPE (op), val);
	      if (!is_gimple_min_invariant (val))
		return false;
	    }
	}

      /* Certain operands are not allowed to be copy propagated due
	 to their interaction with exception handling and some GCC
	 extensions.  */
      else if (!may_propagate_copy (op, val))
	return false;
      
      /* Do not propagate copies if the propagated value is at a deeper loop
	 depth than the propagatee.  Otherwise, this may move loop variant
	 variables outside of their loops and prevent coalescing
	 opportunities.  If the value was loop invariant, it will be hoisted
	 by LICM and exposed for copy propagation.  */
      if (loop_depth_of_name (val) > loop_depth_of_name (op))
	return false;

      /* Dump details.  */
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "  Replaced '");
	  print_generic_expr (dump_file, op, dump_flags);
	  fprintf (dump_file, "' with %s '",
		   (TREE_CODE (val) != SSA_NAME ? "constant" : "variable"));
	  print_generic_expr (dump_file, val, dump_flags);
	  fprintf (dump_file, "'\n");
	}

      /* If VAL is an ADDR_EXPR or a constant of pointer type, note
	 that we may have exposed a new symbol for SSA renaming.  */
      if (TREE_CODE (val) == ADDR_EXPR
	  || (POINTER_TYPE_P (TREE_TYPE (op))
	      && is_gimple_min_invariant (val)))
	may_have_exposed_new_symbols = true;

      if (TREE_CODE (val) != SSA_NAME)
	opt_stats.num_const_prop++;
      else
	opt_stats.num_copy_prop++;

      propagate_value (op_p, val);

      /* And note that we modified this statement.  This is now
	 safe, even if we changed virtual operands since we will
	 rescan the statement and rewrite its operands again.  */
      mark_stmt_modified (stmt);
    }
  return may_have_exposed_new_symbols;
}

/* CONST_AND_COPIES is a table which maps an SSA_NAME to the current
   known value for that SSA_NAME (or NULL if no value is known).  

   Propagate values from CONST_AND_COPIES into the uses, vuses and
   v_may_def_ops of STMT.  */

static bool
cprop_into_stmt (tree stmt)
{
  bool may_have_exposed_new_symbols = false;
  use_operand_p op_p;
  ssa_op_iter iter;

  FOR_EACH_SSA_USE_OPERAND (op_p, stmt, iter, SSA_OP_ALL_USES)
    {
      if (TREE_CODE (USE_FROM_PTR (op_p)) == SSA_NAME)
	may_have_exposed_new_symbols |= cprop_operand (stmt, op_p);
    }

  return may_have_exposed_new_symbols;
}


/* Optimize the statement pointed to by iterator SI.
   
   We try to perform some simplistic global redundancy elimination and
   constant propagation:

   1- To detect global redundancy, we keep track of expressions that have
      been computed in this block and its dominators.  If we find that the
      same expression is computed more than once, we eliminate repeated
      computations by using the target of the first one.

   2- Constant values and copy assignments.  This is used to do very
      simplistic constant and copy propagation.  When a constant or copy
      assignment is found, we map the value on the RHS of the assignment to
      the variable in the LHS in the CONST_AND_COPIES table.  */

static void
optimize_stmt (struct dom_walk_data *walk_data ATTRIBUTE_UNUSED,
	       basic_block bb, block_stmt_iterator si)
{
  stmt_ann_t ann;
  tree stmt, old_stmt;
  bool may_optimize_p;
  bool may_have_exposed_new_symbols = false;

  old_stmt = stmt = bsi_stmt (si);

  update_stmt_if_modified (stmt);
  ann = stmt_ann (stmt);
  opt_stats.num_stmts++;
  may_have_exposed_new_symbols = false;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Optimizing statement ");
      print_generic_stmt (dump_file, stmt, TDF_SLIM);
    }

  /* Const/copy propagate into USES, VUSES and the RHS of V_MAY_DEFs.  */
  may_have_exposed_new_symbols = cprop_into_stmt (stmt);

  /* If the statement has been modified with constant replacements,
     fold its RHS before checking for redundant computations.  */
  if (ann->modified)
    {
      tree rhs;

      /* Try to fold the statement making sure that STMT is kept
	 up to date.  */
      if (fold_stmt (bsi_stmt_ptr (si)))
	{
	  stmt = bsi_stmt (si);
	  ann = stmt_ann (stmt);

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "  Folded to: ");
	      print_generic_stmt (dump_file, stmt, TDF_SLIM);
	    }
	}

      rhs = get_rhs (stmt);
      if (rhs && TREE_CODE (rhs) == ADDR_EXPR)
	recompute_tree_invarant_for_addr_expr (rhs);

      /* Constant/copy propagation above may change the set of 
	 virtual operands associated with this statement.  Folding
	 may remove the need for some virtual operands.

	 Indicate we will need to rescan and rewrite the statement.  */
      may_have_exposed_new_symbols = true;
    }

  /* Check for redundant computations.  Do this optimization only
     for assignments that have no volatile ops and conditionals.  */
  may_optimize_p = (!ann->has_volatile_ops
		    && ((TREE_CODE (stmt) == RETURN_EXPR
			 && TREE_OPERAND (stmt, 0)
			 && TREE_CODE (TREE_OPERAND (stmt, 0)) == MODIFY_EXPR
			 && ! (TREE_SIDE_EFFECTS
			       (TREE_OPERAND (TREE_OPERAND (stmt, 0), 1))))
			|| (TREE_CODE (stmt) == MODIFY_EXPR
			    && ! TREE_SIDE_EFFECTS (TREE_OPERAND (stmt, 1)))
			|| TREE_CODE (stmt) == COND_EXPR
			|| TREE_CODE (stmt) == SWITCH_EXPR));

  if (may_optimize_p)
    may_have_exposed_new_symbols
      |= eliminate_redundant_computations (stmt, ann);

  /* Record any additional equivalences created by this statement.  */
  if (TREE_CODE (stmt) == MODIFY_EXPR)
    record_equivalences_from_stmt (stmt,
				   may_optimize_p,
				   ann);

  /* If STMT is a COND_EXPR and it was modified, then we may know
     where it goes.  If that is the case, then mark the CFG as altered.

     This will cause us to later call remove_unreachable_blocks and
     cleanup_tree_cfg when it is safe to do so.  It is not safe to 
     clean things up here since removal of edges and such can trigger
     the removal of PHI nodes, which in turn can release SSA_NAMEs to
     the manager.

     That's all fine and good, except that once SSA_NAMEs are released
     to the manager, we must not call create_ssa_name until all references
     to released SSA_NAMEs have been eliminated.

     All references to the deleted SSA_NAMEs can not be eliminated until
     we remove unreachable blocks.

     We can not remove unreachable blocks until after we have completed
     any queued jump threading.

     We can not complete any queued jump threads until we have taken
     appropriate variables out of SSA form.  Taking variables out of
     SSA form can call create_ssa_name and thus we lose.

     Ultimately I suspect we're going to need to change the interface
     into the SSA_NAME manager.  */

  if (ann->modified)
    {
      tree val = NULL;

      if (TREE_CODE (stmt) == COND_EXPR)
	val = COND_EXPR_COND (stmt);
      else if (TREE_CODE (stmt) == SWITCH_EXPR)
	val = SWITCH_COND (stmt);

      if (val && TREE_CODE (val) == INTEGER_CST && find_taken_edge (bb, val))
	cfg_altered = true;

      /* If we simplified a statement in such a way as to be shown that it
	 cannot trap, update the eh information and the cfg to match.  */
      if (maybe_clean_or_replace_eh_stmt (old_stmt, stmt))
	{
	  bitmap_set_bit (need_eh_cleanup, bb->index);
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "  Flagged to clear EH edges.\n");
	}
    }

  if (may_have_exposed_new_symbols)
    VEC_safe_push (tree, heap, stmts_to_rescan, bsi_stmt (si));
}

/* Replace the RHS of STMT with NEW_RHS.  If RHS can be found in the
   available expression hashtable, then return the LHS from the hash
   table.

   If INSERT is true, then we also update the available expression
   hash table to account for the changes made to STMT.  */

static tree
update_rhs_and_lookup_avail_expr (tree stmt, tree new_rhs, bool insert)
{
  tree cached_lhs = NULL;

  /* Remove the old entry from the hash table.  */
  if (insert)
    {
      struct expr_hash_elt element;

      initialize_hash_element (stmt, NULL, &element);
      htab_remove_elt_with_hash (avail_exprs, &element, element.hash);
    }

  /* Now update the RHS of the assignment.  */
  TREE_OPERAND (stmt, 1) = new_rhs;

  /* Now lookup the updated statement in the hash table.  */
  cached_lhs = lookup_avail_expr (stmt, insert);

  /* We have now called lookup_avail_expr twice with two different
     versions of this same statement, once in optimize_stmt, once here.

     We know the call in optimize_stmt did not find an existing entry
     in the hash table, so a new entry was created.  At the same time
     this statement was pushed onto the AVAIL_EXPRS_STACK vector. 

     If this call failed to find an existing entry on the hash table,
     then the new version of this statement was entered into the
     hash table.  And this statement was pushed onto BLOCK_AVAIL_EXPR
     for the second time.  So there are two copies on BLOCK_AVAIL_EXPRs

     If this call succeeded, we still have one copy of this statement
     on the BLOCK_AVAIL_EXPRs vector.

     For both cases, we need to pop the most recent entry off the
     BLOCK_AVAIL_EXPRs vector.  For the case where we never found this
     statement in the hash tables, that will leave precisely one
     copy of this statement on BLOCK_AVAIL_EXPRs.  For the case where
     we found a copy of this statement in the second hash table lookup
     we want _no_ copies of this statement in BLOCK_AVAIL_EXPRs.  */
  if (insert)
    VEC_pop (tree, avail_exprs_stack);

  /* And make sure we record the fact that we modified this
     statement.  */
  mark_stmt_modified (stmt);

  return cached_lhs;
}

/* Search for an existing instance of STMT in the AVAIL_EXPRS table.  If
   found, return its LHS. Otherwise insert STMT in the table and return
   NULL_TREE.

   Also, when an expression is first inserted in the AVAIL_EXPRS table, it
   is also added to the stack pointed to by BLOCK_AVAIL_EXPRS_P, so that they
   can be removed when we finish processing this block and its children.

   NOTE: This function assumes that STMT is a MODIFY_EXPR node that
   contains no CALL_EXPR on its RHS and makes no volatile nor
   aliased references.  */

static tree
lookup_avail_expr (tree stmt, bool insert)
{
  void **slot;
  tree lhs;
  tree temp;
  struct expr_hash_elt *element = xmalloc (sizeof (struct expr_hash_elt));

  lhs = TREE_CODE (stmt) == MODIFY_EXPR ? TREE_OPERAND (stmt, 0) : NULL;

  initialize_hash_element (stmt, lhs, element);

  /* Don't bother remembering constant assignments and copy operations.
     Constants and copy operations are handled by the constant/copy propagator
     in optimize_stmt.  */
  if (TREE_CODE (element->rhs) == SSA_NAME
      || is_gimple_min_invariant (element->rhs))
    {
      free (element);
      return NULL_TREE;
    }

  /* If this is an equality test against zero, see if we have recorded a
     nonzero value for the variable in question.  */
  if ((TREE_CODE (element->rhs) == EQ_EXPR
       || TREE_CODE  (element->rhs) == NE_EXPR)
      && TREE_CODE (TREE_OPERAND (element->rhs, 0)) == SSA_NAME
      && integer_zerop (TREE_OPERAND (element->rhs, 1)))
    {
      int indx = SSA_NAME_VERSION (TREE_OPERAND (element->rhs, 0));

      if (bitmap_bit_p (nonzero_vars, indx))
	{
	  tree t = element->rhs;
	  free (element);
	  return constant_boolean_node (TREE_CODE (t) != EQ_EXPR,
					TREE_TYPE (t));
	}
    }

  /* Finally try to find the expression in the main expression hash table.  */
  slot = htab_find_slot_with_hash (avail_exprs, element, element->hash,
				   (insert ? INSERT : NO_INSERT));
  if (slot == NULL)
    {
      free (element);
      return NULL_TREE;
    }

  if (*slot == NULL)
    {
      *slot = (void *) element;
      VEC_safe_push (tree, heap, avail_exprs_stack,
		     stmt ? stmt : element->rhs);
      return NULL_TREE;
    }

  /* Extract the LHS of the assignment so that it can be used as the current
     definition of another variable.  */
  lhs = ((struct expr_hash_elt *)*slot)->lhs;

  /* See if the LHS appears in the CONST_AND_COPIES table.  If it does, then
     use the value from the const_and_copies table.  */
  if (TREE_CODE (lhs) == SSA_NAME)
    {
      temp = SSA_NAME_VALUE (lhs);
      if (temp && TREE_CODE (temp) != VALUE_HANDLE)
	lhs = temp;
    }

  free (element);
  return lhs;
}

/* Given a condition COND, record into HI_P, LO_P and INVERTED_P the
   range of values that result in the conditional having a true value.

   Return true if we are successful in extracting a range from COND and
   false if we are unsuccessful.  */

static bool
extract_range_from_cond (tree cond, tree *hi_p, tree *lo_p, int *inverted_p)
{
  tree op1 = TREE_OPERAND (cond, 1);
  tree high, low, type;
  int inverted;

  type = TREE_TYPE (op1);

  /* Experiments have shown that it's rarely, if ever useful to
     record ranges for enumerations.  Presumably this is due to
     the fact that they're rarely used directly.  They are typically
     cast into an integer type and used that way.  */
  if (TREE_CODE (type) != INTEGER_TYPE
      /* We don't know how to deal with types with variable bounds.  */
      || TREE_CODE (TYPE_MIN_VALUE (type)) != INTEGER_CST
      || TREE_CODE (TYPE_MAX_VALUE (type)) != INTEGER_CST)
    return 0;

  switch (TREE_CODE (cond))
    {
    case EQ_EXPR:
      high = low = op1;
      inverted = 0;
      break;

    case NE_EXPR:
      high = low = op1;
      inverted = 1;
      break;

    case GE_EXPR:
      low = op1;
      high = TYPE_MAX_VALUE (type);
      inverted = 0;
      break;

    case GT_EXPR:
      high = TYPE_MAX_VALUE (type);
      if (!tree_int_cst_lt (op1, high))
	return 0;
      low = int_const_binop (PLUS_EXPR, op1, integer_one_node, 1);
      inverted = 0;
      break;

    case LE_EXPR:
      high = op1;
      low = TYPE_MIN_VALUE (type);
      inverted = 0;
      break;

    case LT_EXPR:
      low = TYPE_MIN_VALUE (type);
      if (!tree_int_cst_lt (low, op1))
	return 0;
      high = int_const_binop (MINUS_EXPR, op1, integer_one_node, 1);
      inverted = 0;
      break;

    default:
      return 0;
    }

  *hi_p = high;
  *lo_p = low;
  *inverted_p = inverted;
  return 1;
}

/* Record a range created by COND for basic block BB.  */

static void
record_range (tree cond, basic_block bb)
{
  enum tree_code code = TREE_CODE (cond);

  /* We explicitly ignore NE_EXPRs and all the unordered comparisons.
     They rarely allow for meaningful range optimizations and significantly
     complicate the implementation.  */
  if ((code == LT_EXPR || code == LE_EXPR || code == GT_EXPR
       || code == GE_EXPR || code == EQ_EXPR)
      && TREE_CODE (TREE_TYPE (TREE_OPERAND (cond, 1))) == INTEGER_TYPE)
    {
      struct vrp_hash_elt *vrp_hash_elt;
      struct vrp_element *element;
      VEC(vrp_element_p,heap) **vrp_records_p;
      void **slot;


      vrp_hash_elt = xmalloc (sizeof (struct vrp_hash_elt));
      vrp_hash_elt->var = TREE_OPERAND (cond, 0);
      vrp_hash_elt->records = NULL;
      slot = htab_find_slot (vrp_data, vrp_hash_elt, INSERT);

      if (*slot == NULL)
	*slot = (void *) vrp_hash_elt;
      else
	vrp_free (vrp_hash_elt);

      vrp_hash_elt = (struct vrp_hash_elt *) *slot;
      vrp_records_p = &vrp_hash_elt->records;

      element = ggc_alloc (sizeof (struct vrp_element));
      element->low = NULL;
      element->high = NULL;
      element->cond = cond;
      element->bb = bb;

      VEC_safe_push (vrp_element_p, heap, *vrp_records_p, element);
      VEC_safe_push (tree, heap, vrp_variables_stack, TREE_OPERAND (cond, 0));
    }
}

/* Hashing and equality functions for VRP_DATA.

   Since this hash table is addressed by SSA_NAMEs, we can hash on
   their version number and equality can be determined with a 
   pointer comparison.  */

static hashval_t
vrp_hash (const void *p)
{
  tree var = ((struct vrp_hash_elt *)p)->var;

  return SSA_NAME_VERSION (var);
}

static int
vrp_eq (const void *p1, const void *p2)
{
  tree var1 = ((struct vrp_hash_elt *)p1)->var;
  tree var2 = ((struct vrp_hash_elt *)p2)->var;

  return var1 == var2;
}

/* Hashing and equality functions for AVAIL_EXPRS.  The table stores
   MODIFY_EXPR statements.  We compute a value number for expressions using
   the code of the expression and the SSA numbers of its operands.  */

static hashval_t
avail_expr_hash (const void *p)
{
  tree stmt = ((struct expr_hash_elt *)p)->stmt;
  tree rhs = ((struct expr_hash_elt *)p)->rhs;
  tree vuse;
  ssa_op_iter iter;
  hashval_t val = 0;

  /* iterative_hash_expr knows how to deal with any expression and
     deals with commutative operators as well, so just use it instead
     of duplicating such complexities here.  */
  val = iterative_hash_expr (rhs, val);

  /* If the hash table entry is not associated with a statement, then we
     can just hash the expression and not worry about virtual operands
     and such.  */
  if (!stmt || !stmt_ann (stmt))
    return val;

  /* Add the SSA version numbers of every vuse operand.  This is important
     because compound variables like arrays are not renamed in the
     operands.  Rather, the rename is done on the virtual variable
     representing all the elements of the array.  */
  FOR_EACH_SSA_TREE_OPERAND (vuse, stmt, iter, SSA_OP_VUSE)
    val = iterative_hash_expr (vuse, val);

  return val;
}

static hashval_t
real_avail_expr_hash (const void *p)
{
  return ((const struct expr_hash_elt *)p)->hash;
}

static int
avail_expr_eq (const void *p1, const void *p2)
{
  tree stmt1 = ((struct expr_hash_elt *)p1)->stmt;
  tree rhs1 = ((struct expr_hash_elt *)p1)->rhs;
  tree stmt2 = ((struct expr_hash_elt *)p2)->stmt;
  tree rhs2 = ((struct expr_hash_elt *)p2)->rhs;

  /* If they are the same physical expression, return true.  */
  if (rhs1 == rhs2 && stmt1 == stmt2)
    return true;

  /* If their codes are not equal, then quit now.  */
  if (TREE_CODE (rhs1) != TREE_CODE (rhs2))
    return false;

  /* In case of a collision, both RHS have to be identical and have the
     same VUSE operands.  */
  if ((TREE_TYPE (rhs1) == TREE_TYPE (rhs2)
       || lang_hooks.types_compatible_p (TREE_TYPE (rhs1), TREE_TYPE (rhs2)))
      && operand_equal_p (rhs1, rhs2, OEP_PURE_SAME))
    {
      bool ret = compare_ssa_operands_equal (stmt1, stmt2, SSA_OP_VUSE);
      gcc_assert (!ret || ((struct expr_hash_elt *)p1)->hash
		  == ((struct expr_hash_elt *)p2)->hash);
      return ret;
    }

  return false;
}
