/* Control flow functions for trees.
   Copyright (C) 2001-2019 Free Software Foundation, Inc.
   Contributed by Diego Novillo <dnovillo@redhat.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "tree-pass.h"
#include "ssa.h"
#include "cgraph.h"
#include "gimple-pretty-print.h"
#include "diagnostic-core.h"
#include "fold-const.h"
#include "trans-mem.h"
#include "stor-layout.h"
#include "print-tree.h"
#include "cfganal.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "gimple-walk.h"
#include "tree-cfg.h"
#include "tree-ssa-loop-manip.h"
#include "tree-ssa-loop-niter.h"
#include "tree-into-ssa.h"
#include "tree-dfa.h"
#include "tree-ssa.h"
#include "except.h"
#include "cfgloop.h"
#include "tree-ssa-propagate.h"
#include "value-prof.h"
#include "tree-inline.h"
#include "tree-ssa-live.h"
#include "omp-general.h"
#include "omp-expand.h"
#include "tree-cfgcleanup.h"
#include "gimplify.h"
#include "attribs.h"
#include "selftest.h"
#include "opts.h"
#include "asan.h"

/* This file contains functions for building the Control Flow Graph (CFG)
   for a function tree.  */

/* Local declarations.  */

/* Initial capacity for the basic block array.  */
static const int initial_cfg_capacity = 20;

/* This hash table allows us to efficiently lookup all CASE_LABEL_EXPRs
   which use a particular edge.  The CASE_LABEL_EXPRs are chained together
   via their CASE_CHAIN field, which we clear after we're done with the
   hash table to prevent problems with duplication of GIMPLE_SWITCHes.

   Access to this list of CASE_LABEL_EXPRs allows us to efficiently
   update the case vector in response to edge redirections.

   Right now this table is set up and torn down at key points in the
   compilation process.  It would be nice if we could make the table
   more persistent.  The key is getting notification of changes to
   the CFG (particularly edge removal, creation and redirection).  */

static hash_map<edge, tree> *edge_to_cases;

/* If we record edge_to_cases, this bitmap will hold indexes
   of basic blocks that end in a GIMPLE_SWITCH which we touched
   due to edge manipulations.  */

static bitmap touched_switch_bbs;

/* CFG statistics.  */
struct cfg_stats_d
{
  long num_merged_labels;
};

static struct cfg_stats_d cfg_stats;

/* Data to pass to replace_block_vars_by_duplicates_1.  */
struct replace_decls_d
{
  hash_map<tree, tree> *vars_map;
  tree to_context;
};

/* Hash table to store last discriminator assigned for each locus.  */
struct locus_discrim_map
{
  int location_line;
  int discriminator;
};

/* Hashtable helpers.  */

struct locus_discrim_hasher : free_ptr_hash <locus_discrim_map>
{
  static inline hashval_t hash (const locus_discrim_map *);
  static inline bool equal (const locus_discrim_map *,
			    const locus_discrim_map *);
};

/* Trivial hash function for a location_t.  ITEM is a pointer to
   a hash table entry that maps a location_t to a discriminator.  */

inline hashval_t
locus_discrim_hasher::hash (const locus_discrim_map *item)
{
  return item->location_line;
}

/* Equality function for the locus-to-discriminator map.  A and B
   point to the two hash table entries to compare.  */

inline bool
locus_discrim_hasher::equal (const locus_discrim_map *a,
			     const locus_discrim_map *b)
{
  return a->location_line == b->location_line;
}

static hash_table<locus_discrim_hasher> *discriminator_per_locus;

/* Basic blocks and flowgraphs.  */
static void make_blocks (gimple_seq);

/* Edges.  */
static void make_edges (void);
static void assign_discriminators (void);
static void make_cond_expr_edges (basic_block);
static void make_gimple_switch_edges (gswitch *, basic_block);
static bool make_goto_expr_edges (basic_block);
static void make_gimple_asm_edges (basic_block);
static edge gimple_redirect_edge_and_branch (edge, basic_block);
static edge gimple_try_redirect_by_replacing_jump (edge, basic_block);

/* Various helpers.  */
static inline bool stmt_starts_bb_p (gimple *, gimple *);
static int gimple_verify_flow_info (void);
static void gimple_make_forwarder_block (edge);
static gimple *first_non_label_stmt (basic_block);
static bool verify_gimple_transaction (gtransaction *);
static bool call_can_make_abnormal_goto (gimple *);

/* Flowgraph optimization and cleanup.  */
static void gimple_merge_blocks (basic_block, basic_block);
static bool gimple_can_merge_blocks_p (basic_block, basic_block);
static void remove_bb (basic_block);
static edge find_taken_edge_computed_goto (basic_block, tree);
static edge find_taken_edge_cond_expr (const gcond *, tree);

void
init_empty_tree_cfg_for_function (struct function *fn)
{
  /* Initialize the basic block array.  */
  init_flow (fn);
  profile_status_for_fn (fn) = PROFILE_ABSENT;
  n_basic_blocks_for_fn (fn) = NUM_FIXED_BLOCKS;
  last_basic_block_for_fn (fn) = NUM_FIXED_BLOCKS;
  vec_alloc (basic_block_info_for_fn (fn), initial_cfg_capacity);
  vec_safe_grow_cleared (basic_block_info_for_fn (fn),
			 initial_cfg_capacity);

  /* Build a mapping of labels to their associated blocks.  */
  vec_alloc (label_to_block_map_for_fn (fn), initial_cfg_capacity);
  vec_safe_grow_cleared (label_to_block_map_for_fn (fn),
			 initial_cfg_capacity);

  SET_BASIC_BLOCK_FOR_FN (fn, ENTRY_BLOCK, ENTRY_BLOCK_PTR_FOR_FN (fn));
  SET_BASIC_BLOCK_FOR_FN (fn, EXIT_BLOCK, EXIT_BLOCK_PTR_FOR_FN (fn));

  ENTRY_BLOCK_PTR_FOR_FN (fn)->next_bb
    = EXIT_BLOCK_PTR_FOR_FN (fn);
  EXIT_BLOCK_PTR_FOR_FN (fn)->prev_bb
    = ENTRY_BLOCK_PTR_FOR_FN (fn);
}

void
init_empty_tree_cfg (void)
{
  init_empty_tree_cfg_for_function (cfun);
}

/*---------------------------------------------------------------------------
			      Create basic blocks
---------------------------------------------------------------------------*/

/* Entry point to the CFG builder for trees.  SEQ is the sequence of
   statements to be added to the flowgraph.  */

static void
build_gimple_cfg (gimple_seq seq)
{
  /* Register specific gimple functions.  */
  gimple_register_cfg_hooks ();

  memset ((void *) &cfg_stats, 0, sizeof (cfg_stats));

  init_empty_tree_cfg ();

  make_blocks (seq);

  /* Make sure there is always at least one block, even if it's empty.  */
  if (n_basic_blocks_for_fn (cfun) == NUM_FIXED_BLOCKS)
    create_empty_bb (ENTRY_BLOCK_PTR_FOR_FN (cfun));

  /* Adjust the size of the array.  */
  if (basic_block_info_for_fn (cfun)->length ()
      < (size_t) n_basic_blocks_for_fn (cfun))
    vec_safe_grow_cleared (basic_block_info_for_fn (cfun),
			   n_basic_blocks_for_fn (cfun));

  /* To speed up statement iterator walks, we first purge dead labels.  */
  cleanup_dead_labels ();

  /* Group case nodes to reduce the number of edges.
     We do this after cleaning up dead labels because otherwise we miss
     a lot of obvious case merging opportunities.  */
  group_case_labels ();

  /* Create the edges of the flowgraph.  */
  discriminator_per_locus = new hash_table<locus_discrim_hasher> (13);
  make_edges ();
  assign_discriminators ();
  cleanup_dead_labels ();
  delete discriminator_per_locus;
  discriminator_per_locus = NULL;
}

/* Look for ANNOTATE calls with loop annotation kind in BB; if found, remove
   them and propagate the information to LOOP.  We assume that the annotations
   come immediately before the condition in BB, if any.  */

static void
replace_loop_annotate_in_block (basic_block bb, struct loop *loop)
{
  gimple_stmt_iterator gsi = gsi_last_bb (bb);
  gimple *stmt = gsi_stmt (gsi);

  if (!(stmt && gimple_code (stmt) == GIMPLE_COND))
    return;

  for (gsi_prev_nondebug (&gsi); !gsi_end_p (gsi); gsi_prev (&gsi))
    {
      stmt = gsi_stmt (gsi);
      if (gimple_code (stmt) != GIMPLE_CALL)
	break;
      if (!gimple_call_internal_p (stmt)
	  || gimple_call_internal_fn (stmt) != IFN_ANNOTATE)
	break;

      switch ((annot_expr_kind) tree_to_shwi (gimple_call_arg (stmt, 1)))
	{
	case annot_expr_ivdep_kind:
	  loop->safelen = INT_MAX;
	  break;
	case annot_expr_unroll_kind:
	  loop->unroll
	    = (unsigned short) tree_to_shwi (gimple_call_arg (stmt, 2));
	  cfun->has_unroll = true;
	  break;
	case annot_expr_no_vector_kind:
	  loop->dont_vectorize = true;
	  break;
	case annot_expr_vector_kind:
	  loop->force_vectorize = true;
	  cfun->has_force_vectorize_loops = true;
	  break;
	case annot_expr_parallel_kind:
	  loop->can_be_parallel = true;
	  loop->safelen = INT_MAX;
	  break;
	default:
	  gcc_unreachable ();
	}

      stmt = gimple_build_assign (gimple_call_lhs (stmt),
				  gimple_call_arg (stmt, 0));
      gsi_replace (&gsi, stmt, true);
    }
}

/* Look for ANNOTATE calls with loop annotation kind; if found, remove
   them and propagate the information to the loop.  We assume that the
   annotations come immediately before the condition of the loop.  */

static void
replace_loop_annotate (void)
{
  struct loop *loop;
  basic_block bb;
  gimple_stmt_iterator gsi;
  gimple *stmt;

  FOR_EACH_LOOP (loop, 0)
    {
      /* First look into the header.  */
      replace_loop_annotate_in_block (loop->header, loop);

      /* Then look into the latch, if any.  */
      if (loop->latch)
	replace_loop_annotate_in_block (loop->latch, loop);
    }

  /* Remove IFN_ANNOTATE.  Safeguard for the case loop->latch == NULL.  */
  FOR_EACH_BB_FN (bb, cfun)
    {
      for (gsi = gsi_last_bb (bb); !gsi_end_p (gsi); gsi_prev (&gsi))
	{
	  stmt = gsi_stmt (gsi);
	  if (gimple_code (stmt) != GIMPLE_CALL)
	    continue;
	  if (!gimple_call_internal_p (stmt)
	      || gimple_call_internal_fn (stmt) != IFN_ANNOTATE)
	    continue;

	  switch ((annot_expr_kind) tree_to_shwi (gimple_call_arg (stmt, 1)))
	    {
	    case annot_expr_ivdep_kind:
	    case annot_expr_unroll_kind:
	    case annot_expr_no_vector_kind:
	    case annot_expr_vector_kind:
	    case annot_expr_parallel_kind:
	      break;
	    default:
	      gcc_unreachable ();
	    }

	  warning_at (gimple_location (stmt), 0, "ignoring loop annotation");
	  stmt = gimple_build_assign (gimple_call_lhs (stmt),
				      gimple_call_arg (stmt, 0));
	  gsi_replace (&gsi, stmt, true);
	}
    }
}

static unsigned int
execute_build_cfg (void)
{
  gimple_seq body = gimple_body (current_function_decl);

  build_gimple_cfg (body);
  gimple_set_body (current_function_decl, NULL);
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Scope blocks:\n");
      dump_scope_blocks (dump_file, dump_flags);
    }
  cleanup_tree_cfg ();
  loop_optimizer_init (AVOID_CFG_MODIFICATIONS);
  replace_loop_annotate ();
  return 0;
}

namespace {

const pass_data pass_data_build_cfg =
{
  GIMPLE_PASS, /* type */
  "cfg", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_CFG, /* tv_id */
  PROP_gimple_leh, /* properties_required */
  ( PROP_cfg | PROP_loops ), /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_build_cfg : public gimple_opt_pass
{
public:
  pass_build_cfg (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_build_cfg, ctxt)
  {}

  /* opt_pass methods: */
  virtual unsigned int execute (function *) { return execute_build_cfg (); }

}; // class pass_build_cfg

} // anon namespace

gimple_opt_pass *
make_pass_build_cfg (gcc::context *ctxt)
{
  return new pass_build_cfg (ctxt);
}


/* Return true if T is a computed goto.  */

bool
computed_goto_p (gimple *t)
{
  return (gimple_code (t) == GIMPLE_GOTO
	  && TREE_CODE (gimple_goto_dest (t)) != LABEL_DECL);
}

/* Returns true if the sequence of statements STMTS only contains
   a call to __builtin_unreachable ().  */

bool
gimple_seq_unreachable_p (gimple_seq stmts)
{
  if (stmts == NULL
      /* Return false if -fsanitize=unreachable, we don't want to
	 optimize away those calls, but rather turn them into
	 __ubsan_handle_builtin_unreachable () or __builtin_trap ()
	 later.  */
      || sanitize_flags_p (SANITIZE_UNREACHABLE))
    return false;

  gimple_stmt_iterator gsi = gsi_last (stmts);

  if (!gimple_call_builtin_p (gsi_stmt (gsi), BUILT_IN_UNREACHABLE))
    return false;

  for (gsi_prev (&gsi); !gsi_end_p (gsi); gsi_prev (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);
      if (gimple_code (stmt) != GIMPLE_LABEL
	  && !is_gimple_debug (stmt)
	  && !gimple_clobber_p (stmt))
      return false;
    }
  return true;
}

/* Returns true for edge E where e->src ends with a GIMPLE_COND and
   the other edge points to a bb with just __builtin_unreachable ().
   I.e. return true for C->M edge in:
   <bb C>:
   ...
   if (something)
     goto <bb N>;
   else
     goto <bb M>;
   <bb N>:
   __builtin_unreachable ();
   <bb M>:  */

bool
assert_unreachable_fallthru_edge_p (edge e)
{
  basic_block pred_bb = e->src;
  gimple *last = last_stmt (pred_bb);
  if (last && gimple_code (last) == GIMPLE_COND)
    {
      basic_block other_bb = EDGE_SUCC (pred_bb, 0)->dest;
      if (other_bb == e->dest)
	other_bb = EDGE_SUCC (pred_bb, 1)->dest;
      if (EDGE_COUNT (other_bb->succs) == 0)
	return gimple_seq_unreachable_p (bb_seq (other_bb));
    }
  return false;
}


/* Initialize GF_CALL_CTRL_ALTERING flag, which indicates the call
   could alter control flow except via eh. We initialize the flag at
   CFG build time and only ever clear it later.  */

static void
gimple_call_initialize_ctrl_altering (gimple *stmt)
{
  int flags = gimple_call_flags (stmt);

  /* A call alters control flow if it can make an abnormal goto.  */
  if (call_can_make_abnormal_goto (stmt)
      /* A call also alters control flow if it does not return.  */
      || flags & ECF_NORETURN
      /* TM ending statements have backedges out of the transaction.
	 Return true so we split the basic block containing them.
	 Note that the TM_BUILTIN test is merely an optimization.  */
      || ((flags & ECF_TM_BUILTIN)
	  && is_tm_ending_fndecl (gimple_call_fndecl (stmt)))
      /* BUILT_IN_RETURN call is same as return statement.  */
      || gimple_call_builtin_p (stmt, BUILT_IN_RETURN)
      /* IFN_UNIQUE should be the last insn, to make checking for it
	 as cheap as possible.  */
      || (gimple_call_internal_p (stmt)
	  && gimple_call_internal_unique_p (stmt)))
    gimple_call_set_ctrl_altering (stmt, true);
  else
    gimple_call_set_ctrl_altering (stmt, false);
}


/* Insert SEQ after BB and build a flowgraph.  */

static basic_block
make_blocks_1 (gimple_seq seq, basic_block bb)
{
  gimple_stmt_iterator i = gsi_start (seq);
  gimple *stmt = NULL;
  gimple *prev_stmt = NULL;
  bool start_new_block = true;
  bool first_stmt_of_seq = true;

  while (!gsi_end_p (i))
    {
      /* PREV_STMT should only be set to a debug stmt if the debug
	 stmt is before nondebug stmts.  Once stmt reaches a nondebug
	 nonlabel, prev_stmt will be set to it, so that
	 stmt_starts_bb_p will know to start a new block if a label is
	 found.  However, if stmt was a label after debug stmts only,
	 keep the label in prev_stmt even if we find further debug
	 stmts, for there may be other labels after them, and they
	 should land in the same block.  */
      if (!prev_stmt || !stmt || !is_gimple_debug (stmt))
	prev_stmt = stmt;
      stmt = gsi_stmt (i);

      if (stmt && is_gimple_call (stmt))
	gimple_call_initialize_ctrl_altering (stmt);

      /* If the statement starts a new basic block or if we have determined
	 in a previous pass that we need to create a new block for STMT, do
	 so now.  */
      if (start_new_block || stmt_starts_bb_p (stmt, prev_stmt))
	{
	  if (!first_stmt_of_seq)
	    gsi_split_seq_before (&i, &seq);
	  bb = create_basic_block (seq, bb);
	  start_new_block = false;
	  prev_stmt = NULL;
	}

      /* Now add STMT to BB and create the subgraphs for special statement
	 codes.  */
      gimple_set_bb (stmt, bb);

      /* If STMT is a basic block terminator, set START_NEW_BLOCK for the
	 next iteration.  */
      if (stmt_ends_bb_p (stmt))
	{
	  /* If the stmt can make abnormal goto use a new temporary
	     for the assignment to the LHS.  This makes sure the old value
	     of the LHS is available on the abnormal edge.  Otherwise
	     we will end up with overlapping life-ranges for abnormal
	     SSA names.  */
	  if (gimple_has_lhs (stmt)
	      && stmt_can_make_abnormal_goto (stmt)
	      && is_gimple_reg_type (TREE_TYPE (gimple_get_lhs (stmt))))
	    {
	      tree lhs = gimple_get_lhs (stmt);
	      tree tmp = create_tmp_var (TREE_TYPE (lhs));
	      gimple *s = gimple_build_assign (lhs, tmp);
	      gimple_set_location (s, gimple_location (stmt));
	      gimple_set_block (s, gimple_block (stmt));
	      gimple_set_lhs (stmt, tmp);
	      if (TREE_CODE (TREE_TYPE (tmp)) == COMPLEX_TYPE
		  || TREE_CODE (TREE_TYPE (tmp)) == VECTOR_TYPE)
		DECL_GIMPLE_REG_P (tmp) = 1;
	      gsi_insert_after (&i, s, GSI_SAME_STMT);
	    }
	  start_new_block = true;
	}

      gsi_next (&i);
      first_stmt_of_seq = false;
    }
  return bb;
}

/* Build a flowgraph for the sequence of stmts SEQ.  */

static void
make_blocks (gimple_seq seq)
{
  /* Look for debug markers right before labels, and move the debug
     stmts after the labels.  Accepting labels among debug markers
     adds no value, just complexity; if we wanted to annotate labels
     with view numbers (so sequencing among markers would matter) or
     somesuch, we're probably better off still moving the labels, but
     adding other debug annotations in their original positions or
     emitting nonbind or bind markers associated with the labels in
     the original position of the labels.

     Moving labels would probably be simpler, but we can't do that:
     moving labels assigns label ids to them, and doing so because of
     debug markers makes for -fcompare-debug and possibly even codegen
     differences.  So, we have to move the debug stmts instead.  To
     that end, we scan SEQ backwards, marking the position of the
     latest (earliest we find) label, and moving debug stmts that are
     not separated from it by nondebug nonlabel stmts after the
     label.  */
  if (MAY_HAVE_DEBUG_MARKER_STMTS)
    {
      gimple_stmt_iterator label = gsi_none ();

      for (gimple_stmt_iterator i = gsi_last (seq); !gsi_end_p (i); gsi_prev (&i))
	{
	  gimple *stmt = gsi_stmt (i);

	  /* If this is the first label we encounter (latest in SEQ)
	     before nondebug stmts, record its position.  */
	  if (is_a <glabel *> (stmt))
	    {
	      if (gsi_end_p (label))
		label = i;
	      continue;
	    }

	  /* Without a recorded label position to move debug stmts to,
	     there's nothing to do.  */
	  if (gsi_end_p (label))
	    continue;

	  /* Move the debug stmt at I after LABEL.  */
	  if (is_gimple_debug (stmt))
	    {
	      gcc_assert (gimple_debug_nonbind_marker_p (stmt));
	      /* As STMT is removed, I advances to the stmt after
		 STMT, so the gsi_prev in the for "increment"
		 expression gets us to the stmt we're to visit after
		 STMT.  LABEL, however, would advance to the moved
		 stmt if we passed it to gsi_move_after, so pass it a
		 copy instead, so as to keep LABEL pointing to the
		 LABEL.  */
	      gimple_stmt_iterator copy = label;
	      gsi_move_after (&i, &copy);
	      continue;
	    }

	  /* There aren't any (more?) debug stmts before label, so
	     there isn't anything else to move after it.  */
	  label = gsi_none ();
	}
    }

  make_blocks_1 (seq, ENTRY_BLOCK_PTR_FOR_FN (cfun));
}

/* Create and return a new empty basic block after bb AFTER.  */

static basic_block
create_bb (void *h, void *e, basic_block after)
{
  basic_block bb;

  gcc_assert (!e);

  /* Create and initialize a new basic block.  Since alloc_block uses
     GC allocation that clears memory to allocate a basic block, we do
     not have to clear the newly allocated basic block here.  */
  bb = alloc_block ();

  bb->index = last_basic_block_for_fn (cfun);
  bb->flags = BB_NEW;
  set_bb_seq (bb, h ? (gimple_seq) h : NULL);

  /* Add the new block to the linked list of blocks.  */
  link_block (bb, after);

  /* Grow the basic block array if needed.  */
  if ((size_t) last_basic_block_for_fn (cfun)
      == basic_block_info_for_fn (cfun)->length ())
    {
      size_t new_size =
	(last_basic_block_for_fn (cfun)
	 + (last_basic_block_for_fn (cfun) + 3) / 4);
      vec_safe_grow_cleared (basic_block_info_for_fn (cfun), new_size);
    }

  /* Add the newly created block to the array.  */
  SET_BASIC_BLOCK_FOR_FN (cfun, last_basic_block_for_fn (cfun), bb);

  n_basic_blocks_for_fn (cfun)++;
  last_basic_block_for_fn (cfun)++;

  return bb;
}


/*---------------------------------------------------------------------------
				 Edge creation
---------------------------------------------------------------------------*/

/* If basic block BB has an abnormal edge to a basic block
   containing IFN_ABNORMAL_DISPATCHER internal call, return
   that the dispatcher's basic block, otherwise return NULL.  */

basic_block
get_abnormal_succ_dispatcher (basic_block bb)
{
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->succs)
    if ((e->flags & (EDGE_ABNORMAL | EDGE_EH)) == EDGE_ABNORMAL)
      {
	gimple_stmt_iterator gsi
	  = gsi_start_nondebug_after_labels_bb (e->dest);
	gimple *g = gsi_stmt (gsi);
	if (g && gimple_call_internal_p (g, IFN_ABNORMAL_DISPATCHER))
	  return e->dest;
      }
  return NULL;
}

/* Helper function for make_edges.  Create a basic block with
   with ABNORMAL_DISPATCHER internal call in it if needed, and
   create abnormal edges from BBS to it and from it to FOR_BB
   if COMPUTED_GOTO is false, otherwise factor the computed gotos.  */

static void
handle_abnormal_edges (basic_block *dispatcher_bbs,
		       basic_block for_bb, int *bb_to_omp_idx,
		       auto_vec<basic_block> *bbs, bool computed_goto)
{
  basic_block *dispatcher = dispatcher_bbs + (computed_goto ? 1 : 0);
  unsigned int idx = 0;
  basic_block bb;
  bool inner = false;

  if (bb_to_omp_idx)
    {
      dispatcher = dispatcher_bbs + 2 * bb_to_omp_idx[for_bb->index];
      if (bb_to_omp_idx[for_bb->index] != 0)
	inner = true;
    }

  /* If the dispatcher has been created already, then there are basic
     blocks with abnormal edges to it, so just make a new edge to
     for_bb.  */
  if (*dispatcher == NULL)
    {
      /* Check if there are any basic blocks that need to have
	 abnormal edges to this dispatcher.  If there are none, return
	 early.  */
      if (bb_to_omp_idx == NULL)
	{
	  if (bbs->is_empty ())
	    return;
	}
      else
	{
	  FOR_EACH_VEC_ELT (*bbs, idx, bb)
	    if (bb_to_omp_idx[bb->index] == bb_to_omp_idx[for_bb->index])
	      break;
	  if (bb == NULL)
	    return;
	}

      /* Create the dispatcher bb.  */
      *dispatcher = create_basic_block (NULL, for_bb);
      if (computed_goto)
	{
	  /* Factor computed gotos into a common computed goto site.  Also
	     record the location of that site so that we can un-factor the
	     gotos after we have converted back to normal form.  */
	  gimple_stmt_iterator gsi = gsi_start_bb (*dispatcher);

	  /* Create the destination of the factored goto.  Each original
	     computed goto will put its desired destination into this
	     variable and jump to the label we create immediately below.  */
	  tree var = create_tmp_var (ptr_type_node, "gotovar");

	  /* Build a label for the new block which will contain the
	     factored computed goto.  */
	  tree factored_label_decl
	    = create_artificial_label (UNKNOWN_LOCATION);
	  gimple *factored_computed_goto_label
	    = gimple_build_label (factored_label_decl);
	  gsi_insert_after (&gsi, factored_computed_goto_label, GSI_NEW_STMT);

	  /* Build our new computed goto.  */
	  gimple *factored_computed_goto = gimple_build_goto (var);
	  gsi_insert_after (&gsi, factored_computed_goto, GSI_NEW_STMT);

	  FOR_EACH_VEC_ELT (*bbs, idx, bb)
	    {
	      if (bb_to_omp_idx
		  && bb_to_omp_idx[bb->index] != bb_to_omp_idx[for_bb->index])
		continue;

	      gsi = gsi_last_bb (bb);
	      gimple *last = gsi_stmt (gsi);

	      gcc_assert (computed_goto_p (last));

	      /* Copy the original computed goto's destination into VAR.  */
	      gimple *assignment
		= gimple_build_assign (var, gimple_goto_dest (last));
	      gsi_insert_before (&gsi, assignment, GSI_SAME_STMT);

	      edge e = make_edge (bb, *dispatcher, EDGE_FALLTHRU);
	      e->goto_locus = gimple_location (last);
	      gsi_remove (&gsi, true);
	    }
	}
      else
	{
	  tree arg = inner ? boolean_true_node : boolean_false_node;
	  gimple *g = gimple_build_call_internal (IFN_ABNORMAL_DISPATCHER,
						 1, arg);
	  gimple_stmt_iterator gsi = gsi_after_labels (*dispatcher);
	  gsi_insert_after (&gsi, g, GSI_NEW_STMT);

	  /* Create predecessor edges of the dispatcher.  */
	  FOR_EACH_VEC_ELT (*bbs, idx, bb)
	    {
	      if (bb_to_omp_idx
		  && bb_to_omp_idx[bb->index] != bb_to_omp_idx[for_bb->index])
		continue;
	      make_edge (bb, *dispatcher, EDGE_ABNORMAL);
	    }
	}
    }

  make_edge (*dispatcher, for_bb, EDGE_ABNORMAL);
}

/* Creates outgoing edges for BB.  Returns 1 when it ends with an
   computed goto, returns 2 when it ends with a statement that
   might return to this function via an nonlocal goto, otherwise
   return 0.  Updates *PCUR_REGION with the OMP region this BB is in.  */

static int
make_edges_bb (basic_block bb, struct omp_region **pcur_region, int *pomp_index)
{
  gimple *last = last_stmt (bb);
  bool fallthru = false;
  int ret = 0;

  if (!last)
    return ret;

  switch (gimple_code (last))
    {
    case GIMPLE_GOTO:
      if (make_goto_expr_edges (bb))
	ret = 1;
      fallthru = false;
      break;
    case GIMPLE_RETURN:
      {
	edge e = make_edge (bb, EXIT_BLOCK_PTR_FOR_FN (cfun), 0);
	e->goto_locus = gimple_location (last);
	fallthru = false;
      }
      break;
    case GIMPLE_COND:
      make_cond_expr_edges (bb);
      fallthru = false;
      break;
    case GIMPLE_SWITCH:
      make_gimple_switch_edges (as_a <gswitch *> (last), bb);
      fallthru = false;
      break;
    case GIMPLE_RESX:
      make_eh_edges (last);
      fallthru = false;
      break;
    case GIMPLE_EH_DISPATCH:
      fallthru = make_eh_dispatch_edges (as_a <geh_dispatch *> (last));
      break;

    case GIMPLE_CALL:
      /* If this function receives a nonlocal goto, then we need to
	 make edges from this call site to all the nonlocal goto
	 handlers.  */
      if (stmt_can_make_abnormal_goto (last))
	ret = 2;

      /* If this statement has reachable exception handlers, then
	 create abnormal edges to them.  */
      make_eh_edges (last);

      /* BUILTIN_RETURN is really a return statement.  */
      if (gimple_call_builtin_p (last, BUILT_IN_RETURN))
	{
	  make_edge (bb, EXIT_BLOCK_PTR_FOR_FN (cfun), 0);
	  fallthru = false;
	}
      /* Some calls are known not to return.  */
      else
	fallthru = !gimple_call_noreturn_p (last);
      break;

    case GIMPLE_ASSIGN:
      /* A GIMPLE_ASSIGN may throw internally and thus be considered
	 control-altering.  */
      if (is_ctrl_altering_stmt (last))
	make_eh_edges (last);
      fallthru = true;
      break;

    case GIMPLE_ASM:
      make_gimple_asm_edges (bb);
      fallthru = true;
      break;

    CASE_GIMPLE_OMP:
      fallthru = omp_make_gimple_edges (bb, pcur_region, pomp_index);
      break;

    case GIMPLE_TRANSACTION:
      {
        gtransaction *txn = as_a <gtransaction *> (last);
	tree label1 = gimple_transaction_label_norm (txn);
	tree label2 = gimple_transaction_label_uninst (txn);

	if (label1)
	  make_edge (bb, label_to_block (cfun, label1), EDGE_FALLTHRU);
	if (label2)
	  make_edge (bb, label_to_block (cfun, label2),
		     EDGE_TM_UNINSTRUMENTED | (label1 ? 0 : EDGE_FALLTHRU));

	tree label3 = gimple_transaction_label_over (txn);
	if (gimple_transaction_subcode (txn)
	    & (GTMA_HAVE_ABORT | GTMA_IS_OUTER))
	  make_edge (bb, label_to_block (cfun, label3), EDGE_TM_ABORT);

	fallthru = false;
      }
      break;

    default:
      gcc_assert (!stmt_ends_bb_p (last));
      fallthru = true;
      break;
    }

  if (fallthru)
    make_edge (bb, bb->next_bb, EDGE_FALLTHRU);

  return ret;
}

/* Join all the blocks in the flowgraph.  */

static void
make_edges (void)
{
  basic_block bb;
  struct omp_region *cur_region = NULL;
  auto_vec<basic_block> ab_edge_goto;
  auto_vec<basic_block> ab_edge_call;
  int *bb_to_omp_idx = NULL;
  int cur_omp_region_idx = 0;

  /* Create an edge from entry to the first block with executable
     statements in it.  */
  make_edge (ENTRY_BLOCK_PTR_FOR_FN (cfun),
	     BASIC_BLOCK_FOR_FN (cfun, NUM_FIXED_BLOCKS),
	     EDGE_FALLTHRU);

  /* Traverse the basic block array placing edges.  */
  FOR_EACH_BB_FN (bb, cfun)
    {
      int mer;

      if (bb_to_omp_idx)
	bb_to_omp_idx[bb->index] = cur_omp_region_idx;

      mer = make_edges_bb (bb, &cur_region, &cur_omp_region_idx);
      if (mer == 1)
	ab_edge_goto.safe_push (bb);
      else if (mer == 2)
	ab_edge_call.safe_push (bb);

      if (cur_region && bb_to_omp_idx == NULL)
	bb_to_omp_idx = XCNEWVEC (int, n_basic_blocks_for_fn (cfun));
    }

  /* Computed gotos are hell to deal with, especially if there are
     lots of them with a large number of destinations.  So we factor
     them to a common computed goto location before we build the
     edge list.  After we convert back to normal form, we will un-factor
     the computed gotos since factoring introduces an unwanted jump.
     For non-local gotos and abnormal edges from calls to calls that return
     twice or forced labels, factor the abnormal edges too, by having all
     abnormal edges from the calls go to a common artificial basic block
     with ABNORMAL_DISPATCHER internal call and abnormal edges from that
     basic block to all forced labels and calls returning twice.
     We do this per-OpenMP structured block, because those regions
     are guaranteed to be single entry single exit by the standard,
     so it is not allowed to enter or exit such regions abnormally this way,
     thus all computed gotos, non-local gotos and setjmp/longjmp calls
     must not transfer control across SESE region boundaries.  */
  if (!ab_edge_goto.is_empty () || !ab_edge_call.is_empty ())
    {
      gimple_stmt_iterator gsi;
      basic_block dispatcher_bb_array[2] = { NULL, NULL };
      basic_block *dispatcher_bbs = dispatcher_bb_array;
      int count = n_basic_blocks_for_fn (cfun);

      if (bb_to_omp_idx)
	dispatcher_bbs = XCNEWVEC (basic_block, 2 * count);

      FOR_EACH_BB_FN (bb, cfun)
	{
	  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	    {
	      glabel *label_stmt = dyn_cast <glabel *> (gsi_stmt (gsi));
	      tree target;

	      if (!label_stmt)
		break;

	      target = gimple_label_label (label_stmt);

	      /* Make an edge to every label block that has been marked as a
		 potential target for a computed goto or a non-local goto.  */
	      if (FORCED_LABEL (target))
		handle_abnormal_edges (dispatcher_bbs, bb, bb_to_omp_idx,
				       &ab_edge_goto, true);
	      if (DECL_NONLOCAL (target))
		{
		  handle_abnormal_edges (dispatcher_bbs, bb, bb_to_omp_idx,
					 &ab_edge_call, false);
		  break;
		}
	    }

	  if (!gsi_end_p (gsi) && is_gimple_debug (gsi_stmt (gsi)))
	    gsi_next_nondebug (&gsi);
	  if (!gsi_end_p (gsi))
	    {
	      /* Make an edge to every setjmp-like call.  */
	      gimple *call_stmt = gsi_stmt (gsi);
	      if (is_gimple_call (call_stmt)
		  && ((gimple_call_flags (call_stmt) & ECF_RETURNS_TWICE)
		      || gimple_call_builtin_p (call_stmt,
						BUILT_IN_SETJMP_RECEIVER)))
		handle_abnormal_edges (dispatcher_bbs, bb, bb_to_omp_idx,
				       &ab_edge_call, false);
	    }
	}

      if (bb_to_omp_idx)
	XDELETE (dispatcher_bbs);
    }

  XDELETE (bb_to_omp_idx);

  omp_free_regions ();
}

/* Add SEQ after GSI.  Start new bb after GSI, and created further bbs as
   needed.  Returns true if new bbs were created.
   Note: This is transitional code, and should not be used for new code.  We
   should be able to get rid of this by rewriting all target va-arg
   gimplification hooks to use an interface gimple_build_cond_value as described
   in https://gcc.gnu.org/ml/gcc-patches/2015-02/msg01194.html.  */

bool
gimple_find_sub_bbs (gimple_seq seq, gimple_stmt_iterator *gsi)
{
  gimple *stmt = gsi_stmt (*gsi);
  basic_block bb = gimple_bb (stmt);
  basic_block lastbb, afterbb;
  int old_num_bbs = n_basic_blocks_for_fn (cfun);
  edge e;
  lastbb = make_blocks_1 (seq, bb);
  if (old_num_bbs == n_basic_blocks_for_fn (cfun))
    return false;
  e = split_block (bb, stmt);
  /* Move e->dest to come after the new basic blocks.  */
  afterbb = e->dest;
  unlink_block (afterbb);
  link_block (afterbb, lastbb);
  redirect_edge_succ (e, bb->next_bb);
  bb = bb->next_bb;
  while (bb != afterbb)
    {
      struct omp_region *cur_region = NULL;
      profile_count cnt = profile_count::zero ();
      bool all = true;

      int cur_omp_region_idx = 0;
      int mer = make_edges_bb (bb, &cur_region, &cur_omp_region_idx);
      gcc_assert (!mer && !cur_region);
      add_bb_to_loop (bb, afterbb->loop_father);

      edge e;
      edge_iterator ei;
      FOR_EACH_EDGE (e, ei, bb->preds)
	{
	  if (e->count ().initialized_p ())
	    cnt += e->count ();
	  else
	    all = false;
	}
      tree_guess_outgoing_edge_probabilities (bb);
      if (all || profile_status_for_fn (cfun) == PROFILE_READ)
        bb->count = cnt;

      bb = bb->next_bb;
    }
  return true;
}

/* Find the next available discriminator value for LOCUS.  The
   discriminator distinguishes among several basic blocks that
   share a common locus, allowing for more accurate sample-based
   profiling.  */

static int
next_discriminator_for_locus (int line)
{
  struct locus_discrim_map item;
  struct locus_discrim_map **slot;

  item.location_line = line;
  item.discriminator = 0;
  slot = discriminator_per_locus->find_slot_with_hash (&item, line, INSERT);
  gcc_assert (slot);
  if (*slot == HTAB_EMPTY_ENTRY)
    {
      *slot = XNEW (struct locus_discrim_map);
      gcc_assert (*slot);
      (*slot)->location_line = line;
      (*slot)->discriminator = 0;
    }
  (*slot)->discriminator++;
  return (*slot)->discriminator;
}

/* Return TRUE if LOCUS1 and LOCUS2 refer to the same source line.  */

static bool
same_line_p (location_t locus1, expanded_location *from, location_t locus2)
{
  expanded_location to;

  if (locus1 == locus2)
    return true;

  to = expand_location (locus2);

  if (from->line != to.line)
    return false;
  if (from->file == to.file)
    return true;
  return (from->file != NULL
          && to.file != NULL
          && filename_cmp (from->file, to.file) == 0);
}

/* Assign discriminators to each basic block.  */

static void
assign_discriminators (void)
{
  basic_block bb;

  FOR_EACH_BB_FN (bb, cfun)
    {
      edge e;
      edge_iterator ei;
      gimple *last = last_stmt (bb);
      location_t locus = last ? gimple_location (last) : UNKNOWN_LOCATION;

      if (locus == UNKNOWN_LOCATION)
	continue;

      expanded_location locus_e = expand_location (locus);

      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  gimple *first = first_non_label_stmt (e->dest);
	  gimple *last = last_stmt (e->dest);
	  if ((first && same_line_p (locus, &locus_e,
				     gimple_location (first)))
	      || (last && same_line_p (locus, &locus_e,
				       gimple_location (last))))
	    {
	      if (e->dest->discriminator != 0 && bb->discriminator == 0)
		bb->discriminator
		  = next_discriminator_for_locus (locus_e.line);
	      else
		e->dest->discriminator
		  = next_discriminator_for_locus (locus_e.line);
	    }
	}
    }
}

/* Create the edges for a GIMPLE_COND starting at block BB.  */

static void
make_cond_expr_edges (basic_block bb)
{
  gcond *entry = as_a <gcond *> (last_stmt (bb));
  gimple *then_stmt, *else_stmt;
  basic_block then_bb, else_bb;
  tree then_label, else_label;
  edge e;

  gcc_assert (entry);
  gcc_assert (gimple_code (entry) == GIMPLE_COND);

  /* Entry basic blocks for each component.  */
  then_label = gimple_cond_true_label (entry);
  else_label = gimple_cond_false_label (entry);
  then_bb = label_to_block (cfun, then_label);
  else_bb = label_to_block (cfun, else_label);
  then_stmt = first_stmt (then_bb);
  else_stmt = first_stmt (else_bb);

  e = make_edge (bb, then_bb, EDGE_TRUE_VALUE);
  e->goto_locus = gimple_location (then_stmt);
  e = make_edge (bb, else_bb, EDGE_FALSE_VALUE);
  if (e)
    e->goto_locus = gimple_location (else_stmt);

  /* We do not need the labels anymore.  */
  gimple_cond_set_true_label (entry, NULL_TREE);
  gimple_cond_set_false_label (entry, NULL_TREE);
}


/* Called for each element in the hash table (P) as we delete the
   edge to cases hash table.

   Clear all the CASE_CHAINs to prevent problems with copying of
   SWITCH_EXPRs and structure sharing rules, then free the hash table
   element.  */

bool
edge_to_cases_cleanup (edge const &, tree const &value, void *)
{
  tree t, next;

  for (t = value; t; t = next)
    {
      next = CASE_CHAIN (t);
      CASE_CHAIN (t) = NULL;
    }

  return true;
}

/* Start recording information mapping edges to case labels.  */

void
start_recording_case_labels (void)
{
  gcc_assert (edge_to_cases == NULL);
  edge_to_cases = new hash_map<edge, tree>;
  touched_switch_bbs = BITMAP_ALLOC (NULL);
}

/* Return nonzero if we are recording information for case labels.  */

static bool
recording_case_labels_p (void)
{
  return (edge_to_cases != NULL);
}

/* Stop recording information mapping edges to case labels and
   remove any information we have recorded.  */
void
end_recording_case_labels (void)
{
  bitmap_iterator bi;
  unsigned i;
  edge_to_cases->traverse<void *, edge_to_cases_cleanup> (NULL);
  delete edge_to_cases;
  edge_to_cases = NULL;
  EXECUTE_IF_SET_IN_BITMAP (touched_switch_bbs, 0, i, bi)
    {
      basic_block bb = BASIC_BLOCK_FOR_FN (cfun, i);
      if (bb)
	{
	  gimple *stmt = last_stmt (bb);
	  if (stmt && gimple_code (stmt) == GIMPLE_SWITCH)
	    group_case_labels_stmt (as_a <gswitch *> (stmt));
	}
    }
  BITMAP_FREE (touched_switch_bbs);
}

/* If we are inside a {start,end}_recording_cases block, then return
   a chain of CASE_LABEL_EXPRs from T which reference E.

   Otherwise return NULL.  */

static tree
get_cases_for_edge (edge e, gswitch *t)
{
  tree *slot;
  size_t i, n;

  /* If we are not recording cases, then we do not have CASE_LABEL_EXPR
     chains available.  Return NULL so the caller can detect this case.  */
  if (!recording_case_labels_p ())
    return NULL;

  slot = edge_to_cases->get (e);
  if (slot)
    return *slot;

  /* If we did not find E in the hash table, then this must be the first
     time we have been queried for information about E & T.  Add all the
     elements from T to the hash table then perform the query again.  */

  n = gimple_switch_num_labels (t);
  for (i = 0; i < n; i++)
    {
      tree elt = gimple_switch_label (t, i);
      tree lab = CASE_LABEL (elt);
      basic_block label_bb = label_to_block (cfun, lab);
      edge this_edge = find_edge (e->src, label_bb);

      /* Add it to the chain of CASE_LABEL_EXPRs referencing E, or create
	 a new chain.  */
      tree &s = edge_to_cases->get_or_insert (this_edge);
      CASE_CHAIN (elt) = s;
      s = elt;
    }

  return *edge_to_cases->get (e);
}

/* Create the edges for a GIMPLE_SWITCH starting at block BB.  */

static void
make_gimple_switch_edges (gswitch *entry, basic_block bb)
{
  size_t i, n;

  n = gimple_switch_num_labels (entry);

  for (i = 0; i < n; ++i)
    {
      basic_block label_bb = gimple_switch_label_bb (cfun, entry, i);
      make_edge (bb, label_bb, 0);
    }
}


/* Return the basic block holding label DEST.  */

basic_block
label_to_block (struct function *ifun, tree dest)
{
  int uid = LABEL_DECL_UID (dest);

  /* We would die hard when faced by an undefined label.  Emit a label to
     the very first basic block.  This will hopefully make even the dataflow
     and undefined variable warnings quite right.  */
  if (seen_error () && uid < 0)
    {
      gimple_stmt_iterator gsi =
	gsi_start_bb (BASIC_BLOCK_FOR_FN (cfun, NUM_FIXED_BLOCKS));
      gimple *stmt;

      stmt = gimple_build_label (dest);
      gsi_insert_before (&gsi, stmt, GSI_NEW_STMT);
      uid = LABEL_DECL_UID (dest);
    }
  if (vec_safe_length (ifun->cfg->x_label_to_block_map) <= (unsigned int) uid)
    return NULL;
  return (*ifun->cfg->x_label_to_block_map)[uid];
}

/* Create edges for a goto statement at block BB.  Returns true
   if abnormal edges should be created.  */

static bool
make_goto_expr_edges (basic_block bb)
{
  gimple_stmt_iterator last = gsi_last_bb (bb);
  gimple *goto_t = gsi_stmt (last);

  /* A simple GOTO creates normal edges.  */
  if (simple_goto_p (goto_t))
    {
      tree dest = gimple_goto_dest (goto_t);
      basic_block label_bb = label_to_block (cfun, dest);
      edge e = make_edge (bb, label_bb, EDGE_FALLTHRU);
      e->goto_locus = gimple_location (goto_t);
      gsi_remove (&last, true);
      return false;
    }

  /* A computed GOTO creates abnormal edges.  */
  return true;
}

/* Create edges for an asm statement with labels at block BB.  */

static void
make_gimple_asm_edges (basic_block bb)
{
  gasm *stmt = as_a <gasm *> (last_stmt (bb));
  int i, n = gimple_asm_nlabels (stmt);

  for (i = 0; i < n; ++i)
    {
      tree label = TREE_VALUE (gimple_asm_label_op (stmt, i));
      basic_block label_bb = label_to_block (cfun, label);
      make_edge (bb, label_bb, 0);
    }
}

/*---------------------------------------------------------------------------
			       Flowgraph analysis
---------------------------------------------------------------------------*/

/* Cleanup useless labels in basic blocks.  This is something we wish
   to do early because it allows us to group case labels before creating
   the edges for the CFG, and it speeds up block statement iterators in
   all passes later on.
   We rerun this pass after CFG is created, to get rid of the labels that
   are no longer referenced.  After then we do not run it any more, since
   (almost) no new labels should be created.  */

/* A map from basic block index to the leading label of that block.  */
static struct label_record
{
  /* The label.  */
  tree label;

  /* True if the label is referenced from somewhere.  */
  bool used;
} *label_for_bb;

/* Given LABEL return the first label in the same basic block.  */

static tree
main_block_label (tree label)
{
  basic_block bb = label_to_block (cfun, label);
  tree main_label = label_for_bb[bb->index].label;

  /* label_to_block possibly inserted undefined label into the chain.  */
  if (!main_label)
    {
      label_for_bb[bb->index].label = label;
      main_label = label;
    }

  label_for_bb[bb->index].used = true;
  return main_label;
}

/* Clean up redundant labels within the exception tree.  */

static void
cleanup_dead_labels_eh (void)
{
  eh_landing_pad lp;
  eh_region r;
  tree lab;
  int i;

  if (cfun->eh == NULL)
    return;

  for (i = 1; vec_safe_iterate (cfun->eh->lp_array, i, &lp); ++i)
    if (lp && lp->post_landing_pad)
      {
	lab = main_block_label (lp->post_landing_pad);
	if (lab != lp->post_landing_pad)
	  {
	    EH_LANDING_PAD_NR (lp->post_landing_pad) = 0;
	    EH_LANDING_PAD_NR (lab) = lp->index;
	  }
      }

  FOR_ALL_EH_REGION (r)
    switch (r->type)
      {
      case ERT_CLEANUP:
      case ERT_MUST_NOT_THROW:
	break;

      case ERT_TRY:
	{
	  eh_catch c;
	  for (c = r->u.eh_try.first_catch; c ; c = c->next_catch)
	    {
	      lab = c->label;
	      if (lab)
		c->label = main_block_label (lab);
	    }
	}
	break;

      case ERT_ALLOWED_EXCEPTIONS:
	lab = r->u.allowed.label;
	if (lab)
	  r->u.allowed.label = main_block_label (lab);
	break;
      }
}


/* Cleanup redundant labels.  This is a three-step process:
     1) Find the leading label for each block.
     2) Redirect all references to labels to the leading labels.
     3) Cleanup all useless labels.  */

void
cleanup_dead_labels (void)
{
  basic_block bb;
  label_for_bb = XCNEWVEC (struct label_record, last_basic_block_for_fn (cfun));

  /* Find a suitable label for each block.  We use the first user-defined
     label if there is one, or otherwise just the first label we see.  */
  FOR_EACH_BB_FN (bb, cfun)
    {
      gimple_stmt_iterator i;

      for (i = gsi_start_bb (bb); !gsi_end_p (i); gsi_next (&i))
	{
	  tree label;
	  glabel *label_stmt = dyn_cast <glabel *> (gsi_stmt (i));

	  if (!label_stmt)
	    break;

	  label = gimple_label_label (label_stmt);

	  /* If we have not yet seen a label for the current block,
	     remember this one and see if there are more labels.  */
	  if (!label_for_bb[bb->index].label)
	    {
	      label_for_bb[bb->index].label = label;
	      continue;
	    }

	  /* If we did see a label for the current block already, but it
	     is an artificially created label, replace it if the current
	     label is a user defined label.  */
	  if (!DECL_ARTIFICIAL (label)
	      && DECL_ARTIFICIAL (label_for_bb[bb->index].label))
	    {
	      label_for_bb[bb->index].label = label;
	      break;
	    }
	}
    }

  /* Now redirect all jumps/branches to the selected label.
     First do so for each block ending in a control statement.  */
  FOR_EACH_BB_FN (bb, cfun)
    {
      gimple *stmt = last_stmt (bb);
      tree label, new_label;

      if (!stmt)
	continue;

      switch (gimple_code (stmt))
	{
	case GIMPLE_COND:
	  {
	    gcond *cond_stmt = as_a <gcond *> (stmt);
	    label = gimple_cond_true_label (cond_stmt);
	    if (label)
	      {
		new_label = main_block_label (label);
		if (new_label != label)
		  gimple_cond_set_true_label (cond_stmt, new_label);
	      }

	    label = gimple_cond_false_label (cond_stmt);
	    if (label)
	      {
		new_label = main_block_label (label);
		if (new_label != label)
		  gimple_cond_set_false_label (cond_stmt, new_label);
	      }
	  }
	  break;

	case GIMPLE_SWITCH:
	  {
	    gswitch *switch_stmt = as_a <gswitch *> (stmt);
	    size_t i, n = gimple_switch_num_labels (switch_stmt);

	    /* Replace all destination labels.  */
	    for (i = 0; i < n; ++i)
	      {
		tree case_label = gimple_switch_label (switch_stmt, i);
		label = CASE_LABEL (case_label);
		new_label = main_block_label (label);
		if (new_label != label)
		  CASE_LABEL (case_label) = new_label;
	      }
	    break;
	  }

	case GIMPLE_ASM:
	  {
	    gasm *asm_stmt = as_a <gasm *> (stmt);
	    int i, n = gimple_asm_nlabels (asm_stmt);

	    for (i = 0; i < n; ++i)
	      {
		tree cons = gimple_asm_label_op (asm_stmt, i);
		tree label = main_block_label (TREE_VALUE (cons));
		TREE_VALUE (cons) = label;
	      }
	    break;
	  }

	/* We have to handle gotos until they're removed, and we don't
	   remove them until after we've created the CFG edges.  */
	case GIMPLE_GOTO:
	  if (!computed_goto_p (stmt))
	    {
	      ggoto *goto_stmt = as_a <ggoto *> (stmt);
	      label = gimple_goto_dest (goto_stmt);
	      new_label = main_block_label (label);
	      if (new_label != label)
		gimple_goto_set_dest (goto_stmt, new_label);
	    }
	  break;

	case GIMPLE_TRANSACTION:
	  {
	    gtransaction *txn = as_a <gtransaction *> (stmt);

	    label = gimple_transaction_label_norm (txn);
	    if (label)
	      {
		new_label = main_block_label (label);
		if (new_label != label)
		  gimple_transaction_set_label_norm (txn, new_label);
	      }

	    label = gimple_transaction_label_uninst (txn);
	    if (label)
	      {
		new_label = main_block_label (label);
		if (new_label != label)
		  gimple_transaction_set_label_uninst (txn, new_label);
	      }

	    label = gimple_transaction_label_over (txn);
	    if (label)
	      {
		new_label = main_block_label (label);
		if (new_label != label)
		  gimple_transaction_set_label_over (txn, new_label);
	      }
	  }
	  break;

	default:
	  break;
      }
    }

  /* Do the same for the exception region tree labels.  */
  cleanup_dead_labels_eh ();

  /* Finally, purge dead labels.  All user-defined labels and labels that
     can be the target of non-local gotos and labels which have their
     address taken are preserved.  */
  FOR_EACH_BB_FN (bb, cfun)
    {
      gimple_stmt_iterator i;
      tree label_for_this_bb = label_for_bb[bb->index].label;

      if (!label_for_this_bb)
	continue;

      /* If the main label of the block is unused, we may still remove it.  */
      if (!label_for_bb[bb->index].used)
	label_for_this_bb = NULL;

      for (i = gsi_start_bb (bb); !gsi_end_p (i); )
	{
	  tree label;
	  glabel *label_stmt = dyn_cast <glabel *> (gsi_stmt (i));

	  if (!label_stmt)
	    break;

	  label = gimple_label_label (label_stmt);

	  if (label == label_for_this_bb
	      || !DECL_ARTIFICIAL (label)
	      || DECL_NONLOCAL (label)
	      || FORCED_LABEL (label))
	    gsi_next (&i);
	  else
	    gsi_remove (&i, true);
	}
    }

  free (label_for_bb);
}

/* Scan the sorted vector of cases in STMT (a GIMPLE_SWITCH) and combine
   the ones jumping to the same label.
   Eg. three separate entries 1: 2: 3: become one entry 1..3:  */

bool
group_case_labels_stmt (gswitch *stmt)
{
  int old_size = gimple_switch_num_labels (stmt);
  int i, next_index, new_size;
  basic_block default_bb = NULL;

  default_bb = gimple_switch_default_bb (cfun, stmt);

  /* Look for possible opportunities to merge cases.  */
  new_size = i = 1;
  while (i < old_size)
    {
      tree base_case, base_high;
      basic_block base_bb;

      base_case = gimple_switch_label (stmt, i);

      gcc_assert (base_case);
      base_bb = label_to_block (cfun, CASE_LABEL (base_case));

      /* Discard cases that have the same destination as the default case or
	 whose destiniation blocks have already been removed as unreachable.  */
      if (base_bb == NULL || base_bb == default_bb)
	{
	  i++;
	  continue;
	}

      base_high = CASE_HIGH (base_case)
	  ? CASE_HIGH (base_case)
	  : CASE_LOW (base_case);
      next_index = i + 1;

      /* Try to merge case labels.  Break out when we reach the end
	 of the label vector or when we cannot merge the next case
	 label with the current one.  */
      while (next_index < old_size)
	{
	  tree merge_case = gimple_switch_label (stmt, next_index);
	  basic_block merge_bb = label_to_block (cfun, CASE_LABEL (merge_case));
	  wide_int bhp1 = wi::to_wide (base_high) + 1;

	  /* Merge the cases if they jump to the same place,
	     and their ranges are consecutive.  */
	  if (merge_bb == base_bb
	      && wi::to_wide (CASE_LOW (merge_case)) == bhp1)
	    {
	      base_high = CASE_HIGH (merge_case) ?
		  CASE_HIGH (merge_case) : CASE_LOW (merge_case);
	      CASE_HIGH (base_case) = base_high;
	      next_index++;
	    }
	  else
	    break;
	}

      /* Discard cases that have an unreachable destination block.  */
      if (EDGE_COUNT (base_bb->succs) == 0
	  && gimple_seq_unreachable_p (bb_seq (base_bb))
	  /* Don't optimize this if __builtin_unreachable () is the
	     implicitly added one by the C++ FE too early, before
	     -Wreturn-type can be diagnosed.  We'll optimize it later
	     during switchconv pass or any other cfg cleanup.  */
	  && (gimple_in_ssa_p (cfun)
	      || (LOCATION_LOCUS (gimple_location (last_stmt (base_bb)))
		  != BUILTINS_LOCATION)))
	{
	  edge base_edge = find_edge (gimple_bb (stmt), base_bb);
	  if (base_edge != NULL)
	    remove_edge_and_dominated_blocks (base_edge);
	  i = next_index;
	  continue;
	}

      if (new_size < i)
	gimple_switch_set_label (stmt, new_size,
				 gimple_switch_label (stmt, i));
      i = next_index;
      new_size++;
    }

  gcc_assert (new_size <= old_size);

  if (new_size < old_size)
    gimple_switch_set_num_labels (stmt, new_size);

  return new_size < old_size;
}

/* Look for blocks ending in a multiway branch (a GIMPLE_SWITCH),
   and scan the sorted vector of cases.  Combine the ones jumping to the
   same label.  */

bool
group_case_labels (void)
{
  basic_block bb;
  bool changed = false;

  FOR_EACH_BB_FN (bb, cfun)
    {
      gimple *stmt = last_stmt (bb);
      if (stmt && gimple_code (stmt) == GIMPLE_SWITCH)
	changed |= group_case_labels_stmt (as_a <gswitch *> (stmt));
    }

  return changed;
}

/* Checks whether we can merge block B into block A.  */

static bool
gimple_can_merge_blocks_p (basic_block a, basic_block b)
{
  gimple *stmt;

  if (!single_succ_p (a))
    return false;

  if (single_succ_edge (a)->flags & EDGE_COMPLEX)
    return false;

  if (single_succ (a) != b)
    return false;

  if (!single_pred_p (b))
    return false;

  if (a == ENTRY_BLOCK_PTR_FOR_FN (cfun)
      || b == EXIT_BLOCK_PTR_FOR_FN (cfun))
    return false;

  /* If A ends by a statement causing exceptions or something similar, we
     cannot merge the blocks.  */
  stmt = last_stmt (a);
  if (stmt && stmt_ends_bb_p (stmt))
    return false;

  /* Do not allow a block with only a non-local label to be merged.  */
  if (stmt)
    if (glabel *label_stmt = dyn_cast <glabel *> (stmt))
      if (DECL_NONLOCAL (gimple_label_label (label_stmt)))
	return false;

  /* Examine the labels at the beginning of B.  */
  for (gimple_stmt_iterator gsi = gsi_start_bb (b); !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      tree lab;
      glabel *label_stmt = dyn_cast <glabel *> (gsi_stmt (gsi));
      if (!label_stmt)
	break;
      lab = gimple_label_label (label_stmt);

      /* Do not remove user forced labels or for -O0 any user labels.  */
      if (!DECL_ARTIFICIAL (lab) && (!optimize || FORCED_LABEL (lab)))
	return false;
    }

  /* Protect simple loop latches.  We only want to avoid merging
     the latch with the loop header or with a block in another
     loop in this case.  */
  if (current_loops
      && b->loop_father->latch == b
      && loops_state_satisfies_p (LOOPS_HAVE_SIMPLE_LATCHES)
      && (b->loop_father->header == a
	  || b->loop_father != a->loop_father))
    return false;

  /* It must be possible to eliminate all phi nodes in B.  If ssa form
     is not up-to-date and a name-mapping is registered, we cannot eliminate
     any phis.  Symbols marked for renaming are never a problem though.  */
  for (gphi_iterator gsi = gsi_start_phis (b); !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();
      /* Technically only new names matter.  */
      if (name_registered_for_update_p (PHI_RESULT (phi)))
	return false;
    }

  /* When not optimizing, don't merge if we'd lose goto_locus.  */
  if (!optimize
      && single_succ_edge (a)->goto_locus != UNKNOWN_LOCATION)
    {
      location_t goto_locus = single_succ_edge (a)->goto_locus;
      gimple_stmt_iterator prev, next;
      prev = gsi_last_nondebug_bb (a);
      next = gsi_after_labels (b);
      if (!gsi_end_p (next) && is_gimple_debug (gsi_stmt (next)))
	gsi_next_nondebug (&next);
      if ((gsi_end_p (prev)
	   || gimple_location (gsi_stmt (prev)) != goto_locus)
	  && (gsi_end_p (next)
	      || gimple_location (gsi_stmt (next)) != goto_locus))
	return false;
    }

  return true;
}

/* Replaces all uses of NAME by VAL.  */

void
replace_uses_by (tree name, tree val)
{
  imm_use_iterator imm_iter;
  use_operand_p use;
  gimple *stmt;
  edge e;

  FOR_EACH_IMM_USE_STMT (stmt, imm_iter, name)
    {
      /* Mark the block if we change the last stmt in it.  */
      if (cfgcleanup_altered_bbs
	  && stmt_ends_bb_p (stmt))
	bitmap_set_bit (cfgcleanup_altered_bbs, gimple_bb (stmt)->index);

      FOR_EACH_IMM_USE_ON_STMT (use, imm_iter)
        {
	  replace_exp (use, val);

	  if (gimple_code (stmt) == GIMPLE_PHI)
	    {
	      e = gimple_phi_arg_edge (as_a <gphi *> (stmt),
				       PHI_ARG_INDEX_FROM_USE (use));
	      if (e->flags & EDGE_ABNORMAL
		  && !SSA_NAME_OCCURS_IN_ABNORMAL_PHI (val))
		{
		  /* This can only occur for virtual operands, since
		     for the real ones SSA_NAME_OCCURS_IN_ABNORMAL_PHI (name))
		     would prevent replacement.  */
		  gcc_checking_assert (virtual_operand_p (name));
		  SSA_NAME_OCCURS_IN_ABNORMAL_PHI (val) = 1;
		}
	    }
	}

      if (gimple_code (stmt) != GIMPLE_PHI)
	{
	  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
	  gimple *orig_stmt = stmt;
	  size_t i;

	  /* FIXME.  It shouldn't be required to keep TREE_CONSTANT
	     on ADDR_EXPRs up-to-date on GIMPLE.  Propagation will
	     only change sth from non-invariant to invariant, and only
	     when propagating constants.  */
	  if (is_gimple_min_invariant (val))
	    for (i = 0; i < gimple_num_ops (stmt); i++)
	      {
		tree op = gimple_op (stmt, i);
		/* Operands may be empty here.  For example, the labels
		   of a GIMPLE_COND are nulled out following the creation
		   of the corresponding CFG edges.  */
		if (op && TREE_CODE (op) == ADDR_EXPR)
		  recompute_tree_invariant_for_addr_expr (op);
	      }

	  if (fold_stmt (&gsi))
	    stmt = gsi_stmt (gsi);

	  if (maybe_clean_or_replace_eh_stmt (orig_stmt, stmt))
	    gimple_purge_dead_eh_edges (gimple_bb (stmt));

	  update_stmt (stmt);
	}
    }

  gcc_checking_assert (has_zero_uses (name));

  /* Also update the trees stored in loop structures.  */
  if (current_loops)
    {
      struct loop *loop;

      FOR_EACH_LOOP (loop, 0)
	{
	  substitute_in_loop_info (loop, name, val);
	}
    }
}

/* Merge block B into block A.  */

static void
gimple_merge_blocks (basic_block a, basic_block b)
{
  gimple_stmt_iterator last, gsi;
  gphi_iterator psi;

  if (dump_file)
    fprintf (dump_file, "Merging blocks %d and %d\n", a->index, b->index);

  /* Remove all single-valued PHI nodes from block B of the form
     V_i = PHI <V_j> by propagating V_j to all the uses of V_i.  */
  gsi = gsi_last_bb (a);
  for (psi = gsi_start_phis (b); !gsi_end_p (psi); )
    {
      gimple *phi = gsi_stmt (psi);
      tree def = gimple_phi_result (phi), use = gimple_phi_arg_def (phi, 0);
      gimple *copy;
      bool may_replace_uses = (virtual_operand_p (def)
			       || may_propagate_copy (def, use));

      /* In case we maintain loop closed ssa form, do not propagate arguments
	 of loop exit phi nodes.  */
      if (current_loops
	  && loops_state_satisfies_p (LOOP_CLOSED_SSA)
	  && !virtual_operand_p (def)
	  && TREE_CODE (use) == SSA_NAME
	  && a->loop_father != b->loop_father)
	may_replace_uses = false;

      if (!may_replace_uses)
	{
	  gcc_assert (!virtual_operand_p (def));

	  /* Note that just emitting the copies is fine -- there is no problem
	     with ordering of phi nodes.  This is because A is the single
	     predecessor of B, therefore results of the phi nodes cannot
	     appear as arguments of the phi nodes.  */
	  copy = gimple_build_assign (def, use);
	  gsi_insert_after (&gsi, copy, GSI_NEW_STMT);
          remove_phi_node (&psi, false);
	}
      else
        {
	  /* If we deal with a PHI for virtual operands, we can simply
	     propagate these without fussing with folding or updating
	     the stmt.  */
	  if (virtual_operand_p (def))
	    {
	      imm_use_iterator iter;
	      use_operand_p use_p;
	      gimple *stmt;

	      FOR_EACH_IMM_USE_STMT (stmt, iter, def)
		FOR_EACH_IMM_USE_ON_STMT (use_p, iter)
		  SET_USE (use_p, use);

	      if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (def))
		SSA_NAME_OCCURS_IN_ABNORMAL_PHI (use) = 1;
	    }
	  else
            replace_uses_by (def, use);

          remove_phi_node (&psi, true);
        }
    }

  /* Ensure that B follows A.  */
  move_block_after (b, a);

  gcc_assert (single_succ_edge (a)->flags & EDGE_FALLTHRU);
  gcc_assert (!last_stmt (a) || !stmt_ends_bb_p (last_stmt (a)));

  /* Remove labels from B and set gimple_bb to A for other statements.  */
  for (gsi = gsi_start_bb (b); !gsi_end_p (gsi);)
    {
      gimple *stmt = gsi_stmt (gsi);
      if (glabel *label_stmt = dyn_cast <glabel *> (stmt))
	{
	  tree label = gimple_label_label (label_stmt);
	  int lp_nr;

	  gsi_remove (&gsi, false);

	  /* Now that we can thread computed gotos, we might have
	     a situation where we have a forced label in block B
	     However, the label at the start of block B might still be
	     used in other ways (think about the runtime checking for
	     Fortran assigned gotos).  So we cannot just delete the
	     label.  Instead we move the label to the start of block A.  */
	  if (FORCED_LABEL (label))
	    {
	      gimple_stmt_iterator dest_gsi = gsi_start_bb (a);
	      gsi_insert_before (&dest_gsi, stmt, GSI_NEW_STMT);
	    }
	  /* Other user labels keep around in a form of a debug stmt.  */
	  else if (!DECL_ARTIFICIAL (label) && MAY_HAVE_DEBUG_BIND_STMTS)
	    {
	      gimple *dbg = gimple_build_debug_bind (label,
						     integer_zero_node,
						     stmt);
	      gimple_debug_bind_reset_value (dbg);
	      gsi_insert_before (&gsi, dbg, GSI_SAME_STMT);
	    }

	  lp_nr = EH_LANDING_PAD_NR (label);
	  if (lp_nr)
	    {
	      eh_landing_pad lp = get_eh_landing_pad_from_number (lp_nr);
	      lp->post_landing_pad = NULL;
	    }
	}
      else
	{
	  gimple_set_bb (stmt, a);
	  gsi_next (&gsi);
	}
    }

  /* When merging two BBs, if their counts are different, the larger count
     is selected as the new bb count. This is to handle inconsistent
     profiles.  */
  if (a->loop_father == b->loop_father)
    {
      a->count = a->count.merge (b->count);
    }

  /* Merge the sequences.  */
  last = gsi_last_bb (a);
  gsi_insert_seq_after (&last, bb_seq (b), GSI_NEW_STMT);
  set_bb_seq (b, NULL);

  if (cfgcleanup_altered_bbs)
    bitmap_set_bit (cfgcleanup_altered_bbs, a->index);
}


/* Return the one of two successors of BB that is not reachable by a
   complex edge, if there is one.  Else, return BB.  We use
   this in optimizations that use post-dominators for their heuristics,
   to catch the cases in C++ where function calls are involved.  */

basic_block
single_noncomplex_succ (basic_block bb)
{
  edge e0, e1;
  if (EDGE_COUNT (bb->succs) != 2)
    return bb;

  e0 = EDGE_SUCC (bb, 0);
  e1 = EDGE_SUCC (bb, 1);
  if (e0->flags & EDGE_COMPLEX)
    return e1->dest;
  if (e1->flags & EDGE_COMPLEX)
    return e0->dest;

  return bb;
}

/* T is CALL_EXPR.  Set current_function_calls_* flags.  */

void
notice_special_calls (gcall *call)
{
  int flags = gimple_call_flags (call);

  if (flags & ECF_MAY_BE_ALLOCA)
    cfun->calls_alloca = true;
  if (flags & ECF_RETURNS_TWICE)
    cfun->calls_setjmp = true;
}


/* Clear flags set by notice_special_calls.  Used by dead code removal
   to update the flags.  */

void
clear_special_calls (void)
{
  cfun->calls_alloca = false;
  cfun->calls_setjmp = false;
}

/* Remove PHI nodes associated with basic block BB and all edges out of BB.  */

static void
remove_phi_nodes_and_edges_for_unreachable_block (basic_block bb)
{
  /* Since this block is no longer reachable, we can just delete all
     of its PHI nodes.  */
  remove_phi_nodes (bb);

  /* Remove edges to BB's successors.  */
  while (EDGE_COUNT (bb->succs) > 0)
    remove_edge (EDGE_SUCC (bb, 0));
}


/* Remove statements of basic block BB.  */

static void
remove_bb (basic_block bb)
{
  gimple_stmt_iterator i;

  if (dump_file)
    {
      fprintf (dump_file, "Removing basic block %d\n", bb->index);
      if (dump_flags & TDF_DETAILS)
	{
	  dump_bb (dump_file, bb, 0, TDF_BLOCKS);
	  fprintf (dump_file, "\n");
	}
    }

  if (current_loops)
    {
      struct loop *loop = bb->loop_father;

      /* If a loop gets removed, clean up the information associated
	 with it.  */
      if (loop->latch == bb
	  || loop->header == bb)
	free_numbers_of_iterations_estimates (loop);
    }

  /* Remove all the instructions in the block.  */
  if (bb_seq (bb) != NULL)
    {
      /* Walk backwards so as to get a chance to substitute all
	 released DEFs into debug stmts.  See
	 eliminate_unnecessary_stmts() in tree-ssa-dce.c for more
	 details.  */
      for (i = gsi_last_bb (bb); !gsi_end_p (i);)
	{
	  gimple *stmt = gsi_stmt (i);
	  glabel *label_stmt = dyn_cast <glabel *> (stmt);
	  if (label_stmt
	      && (FORCED_LABEL (gimple_label_label (label_stmt))
		  || DECL_NONLOCAL (gimple_label_label (label_stmt))))
	    {
	      basic_block new_bb;
	      gimple_stmt_iterator new_gsi;

	      /* A non-reachable non-local label may still be referenced.
		 But it no longer needs to carry the extra semantics of
		 non-locality.  */
	      if (DECL_NONLOCAL (gimple_label_label (label_stmt)))
		{
		  DECL_NONLOCAL (gimple_label_label (label_stmt)) = 0;
		  FORCED_LABEL (gimple_label_label (label_stmt)) = 1;
		}

	      new_bb = bb->prev_bb;
	      /* Don't move any labels into ENTRY block.  */
	      if (new_bb == ENTRY_BLOCK_PTR_FOR_FN (cfun))
		{
		  new_bb = single_succ (new_bb);
		  gcc_assert (new_bb != bb);
		}
	      new_gsi = gsi_after_labels (new_bb);
	      gsi_remove (&i, false);
	      gsi_insert_before (&new_gsi, stmt, GSI_NEW_STMT);
	    }
	  else
	    {
	      /* Release SSA definitions.  */
	      release_defs (stmt);
	      gsi_remove (&i, true);
	    }

	  if (gsi_end_p (i))
	    i = gsi_last_bb (bb);
	  else
	    gsi_prev (&i);
	}
    }

  remove_phi_nodes_and_edges_for_unreachable_block (bb);
  bb->il.gimple.seq = NULL;
  bb->il.gimple.phi_nodes = NULL;
}


/* Given a basic block BB and a value VAL for use in the final statement
   of the block (if a GIMPLE_COND, GIMPLE_SWITCH, or computed goto), return
   the edge that will be taken out of the block.
   If VAL is NULL_TREE, then the current value of the final statement's
   predicate or index is used.
   If the value does not match a unique edge, NULL is returned.  */

edge
find_taken_edge (basic_block bb, tree val)
{
  gimple *stmt;

  stmt = last_stmt (bb);

  /* Handle ENTRY and EXIT.  */
  if (!stmt)
    return NULL;

  if (gimple_code (stmt) == GIMPLE_COND)
    return find_taken_edge_cond_expr (as_a <gcond *> (stmt), val);

  if (gimple_code (stmt) == GIMPLE_SWITCH)
    return find_taken_edge_switch_expr (as_a <gswitch *> (stmt), val);

  if (computed_goto_p (stmt))
    {
      /* Only optimize if the argument is a label, if the argument is
	 not a label then we cannot construct a proper CFG.

         It may be the case that we only need to allow the LABEL_REF to
         appear inside an ADDR_EXPR, but we also allow the LABEL_REF to
         appear inside a LABEL_EXPR just to be safe.  */
      if (val
	  && (TREE_CODE (val) == ADDR_EXPR || TREE_CODE (val) == LABEL_EXPR)
	  && TREE_CODE (TREE_OPERAND (val, 0)) == LABEL_DECL)
	return find_taken_edge_computed_goto (bb, TREE_OPERAND (val, 0));
    }

  /* Otherwise we only know the taken successor edge if it's unique.  */
  return single_succ_p (bb) ? single_succ_edge (bb) : NULL;
}

/* Given a constant value VAL and the entry block BB to a GOTO_EXPR
   statement, determine which of the outgoing edges will be taken out of the
   block.  Return NULL if either edge may be taken.  */

static edge
find_taken_edge_computed_goto (basic_block bb, tree val)
{
  basic_block dest;
  edge e = NULL;

  dest = label_to_block (cfun, val);
  if (dest)
    e = find_edge (bb, dest);

  /* It's possible for find_edge to return NULL here on invalid code
     that abuses the labels-as-values extension (e.g. code that attempts to
     jump *between* functions via stored labels-as-values; PR 84136).
     If so, then we simply return that NULL for the edge.
     We don't currently have a way of detecting such invalid code, so we
     can't assert that it was the case when a NULL edge occurs here.  */

  return e;
}

/* Given COND_STMT and a constant value VAL for use as the predicate,
   determine which of the two edges will be taken out of
   the statement's block.  Return NULL if either edge may be taken.
   If VAL is NULL_TREE, then the current value of COND_STMT's predicate
   is used.  */

static edge
find_taken_edge_cond_expr (const gcond *cond_stmt, tree val)
{
  edge true_edge, false_edge;

  if (val == NULL_TREE)
    {
      /* Use the current value of the predicate.  */
      if (gimple_cond_true_p (cond_stmt))
	val = integer_one_node;
      else if (gimple_cond_false_p (cond_stmt))
	val = integer_zero_node;
      else
	return NULL;
    }
  else if (TREE_CODE (val) != INTEGER_CST)
    return NULL;

  extract_true_false_edges_from_block (gimple_bb (cond_stmt),
				       &true_edge, &false_edge);

  return (integer_zerop (val) ? false_edge : true_edge);
}

/* Given SWITCH_STMT and an INTEGER_CST VAL for use as the index, determine
   which edge will be taken out of the statement's block.  Return NULL if any
   edge may be taken.
   If VAL is NULL_TREE, then the current value of SWITCH_STMT's index
   is used.  */

edge
find_taken_edge_switch_expr (const gswitch *switch_stmt, tree val)
{
  basic_block dest_bb;
  edge e;
  tree taken_case;

  if (gimple_switch_num_labels (switch_stmt) == 1)
    taken_case = gimple_switch_default_label (switch_stmt);
  else
    {
      if (val == NULL_TREE)
	val = gimple_switch_index (switch_stmt);
      if (TREE_CODE (val) != INTEGER_CST)
	return NULL;
      else
	taken_case = find_case_label_for_value (switch_stmt, val);
    }
  dest_bb = label_to_block (cfun, CASE_LABEL (taken_case));

  e = find_edge (gimple_bb (switch_stmt), dest_bb);
  gcc_assert (e);
  return e;
}


/* Return the CASE_LABEL_EXPR that SWITCH_STMT will take for VAL.
   We can make optimal use here of the fact that the case labels are
   sorted: We can do a binary search for a case matching VAL.  */

tree
find_case_label_for_value (const gswitch *switch_stmt, tree val)
{
  size_t low, high, n = gimple_switch_num_labels (switch_stmt);
  tree default_case = gimple_switch_default_label (switch_stmt);

  for (low = 0, high = n; high - low > 1; )
    {
      size_t i = (high + low) / 2;
      tree t = gimple_switch_label (switch_stmt, i);
      int cmp;

      /* Cache the result of comparing CASE_LOW and val.  */
      cmp = tree_int_cst_compare (CASE_LOW (t), val);

      if (cmp > 0)
	high = i;
      else
	low = i;

      if (CASE_HIGH (t) == NULL)
	{
	  /* A singe-valued case label.  */
	  if (cmp == 0)
	    return t;
	}
      else
	{
	  /* A case range.  We can only handle integer ranges.  */
	  if (cmp <= 0 && tree_int_cst_compare (CASE_HIGH (t), val) >= 0)
	    return t;
	}
    }

  return default_case;
}


/* Dump a basic block on stderr.  */

void
gimple_debug_bb (basic_block bb)
{
  dump_bb (stderr, bb, 0, TDF_VOPS|TDF_MEMSYMS|TDF_BLOCKS);
}


/* Dump basic block with index N on stderr.  */

basic_block
gimple_debug_bb_n (int n)
{
  gimple_debug_bb (BASIC_BLOCK_FOR_FN (cfun, n));
  return BASIC_BLOCK_FOR_FN (cfun, n);
}


/* Dump the CFG on stderr.

   FLAGS are the same used by the tree dumping functions
   (see TDF_* in dumpfile.h).  */

void
gimple_debug_cfg (dump_flags_t flags)
{
  gimple_dump_cfg (stderr, flags);
}


/* Dump the program showing basic block boundaries on the given FILE.

   FLAGS are the same used by the tree dumping functions (see TDF_* in
   tree.h).  */

void
gimple_dump_cfg (FILE *file, dump_flags_t flags)
{
  if (flags & TDF_DETAILS)
    {
      dump_function_header (file, current_function_decl, flags);
      fprintf (file, ";; \n%d basic blocks, %d edges, last basic block %d.\n\n",
	       n_basic_blocks_for_fn (cfun), n_edges_for_fn (cfun),
	       last_basic_block_for_fn (cfun));

      brief_dump_cfg (file, flags);
      fprintf (file, "\n");
    }

  if (flags & TDF_STATS)
    dump_cfg_stats (file);

  dump_function_to_file (current_function_decl, file, flags | TDF_BLOCKS);
}


/* Dump CFG statistics on FILE.  */

void
dump_cfg_stats (FILE *file)
{
  static long max_num_merged_labels = 0;
  unsigned long size, total = 0;
  long num_edges;
  basic_block bb;
  const char * const fmt_str   = "%-30s%-13s%12s\n";
  const char * const fmt_str_1 = "%-30s%13d" PRsa (11) "\n";
  const char * const fmt_str_2 = "%-30s%13ld" PRsa (11) "\n";
  const char * const fmt_str_3 = "%-43s" PRsa (11) "\n";
  const char *funcname = current_function_name ();

  fprintf (file, "\nCFG Statistics for %s\n\n", funcname);

  fprintf (file, "---------------------------------------------------------\n");
  fprintf (file, fmt_str, "", "  Number of  ", "Memory");
  fprintf (file, fmt_str, "", "  instances  ", "used ");
  fprintf (file, "---------------------------------------------------------\n");

  size = n_basic_blocks_for_fn (cfun) * sizeof (struct basic_block_def);
  total += size;
  fprintf (file, fmt_str_1, "Basic blocks", n_basic_blocks_for_fn (cfun),
	   SIZE_AMOUNT (size));

  num_edges = 0;
  FOR_EACH_BB_FN (bb, cfun)
    num_edges += EDGE_COUNT (bb->succs);
  size = num_edges * sizeof (struct edge_def);
  total += size;
  fprintf (file, fmt_str_2, "Edges", num_edges, SIZE_AMOUNT (size));

  fprintf (file, "---------------------------------------------------------\n");
  fprintf (file, fmt_str_3, "Total memory used by CFG data",
	   SIZE_AMOUNT (total));
  fprintf (file, "---------------------------------------------------------\n");
  fprintf (file, "\n");

  if (cfg_stats.num_merged_labels > max_num_merged_labels)
    max_num_merged_labels = cfg_stats.num_merged_labels;

  fprintf (file, "Coalesced label blocks: %ld (Max so far: %ld)\n",
	   cfg_stats.num_merged_labels, max_num_merged_labels);

  fprintf (file, "\n");
}


/* Dump CFG statistics on stderr.  Keep extern so that it's always
   linked in the final executable.  */

DEBUG_FUNCTION void
debug_cfg_stats (void)
{
  dump_cfg_stats (stderr);
}

/*---------------------------------------------------------------------------
			     Miscellaneous helpers
---------------------------------------------------------------------------*/

/* Return true if T, a GIMPLE_CALL, can make an abnormal transfer of control
   flow.  Transfers of control flow associated with EH are excluded.  */

static bool
call_can_make_abnormal_goto (gimple *t)
{
  /* If the function has no non-local labels, then a call cannot make an
     abnormal transfer of control.  */
  if (!cfun->has_nonlocal_label
      && !cfun->calls_setjmp)
   return false;

  /* Likewise if the call has no side effects.  */
  if (!gimple_has_side_effects (t))
    return false;

  /* Likewise if the called function is leaf.  */
  if (gimple_call_flags (t) & ECF_LEAF)
    return false;

  return true;
}


/* Return true if T can make an abnormal transfer of control flow.
   Transfers of control flow associated with EH are excluded.  */

bool
stmt_can_make_abnormal_goto (gimple *t)
{
  if (computed_goto_p (t))
    return true;
  if (is_gimple_call (t))
    return call_can_make_abnormal_goto (t);
  return false;
}


/* Return true if T represents a stmt that always transfers control.  */

bool
is_ctrl_stmt (gimple *t)
{
  switch (gimple_code (t))
    {
    case GIMPLE_COND:
    case GIMPLE_SWITCH:
    case GIMPLE_GOTO:
    case GIMPLE_RETURN:
    case GIMPLE_RESX:
      return true;
    default:
      return false;
    }
}


/* Return true if T is a statement that may alter the flow of control
   (e.g., a call to a non-returning function).  */

bool
is_ctrl_altering_stmt (gimple *t)
{
  gcc_assert (t);

  switch (gimple_code (t))
    {
    case GIMPLE_CALL:
      /* Per stmt call flag indicates whether the call could alter
	 controlflow.  */
      if (gimple_call_ctrl_altering_p (t))
	return true;
      break;

    case GIMPLE_EH_DISPATCH:
      /* EH_DISPATCH branches to the individual catch handlers at
	 this level of a try or allowed-exceptions region.  It can
	 fallthru to the next statement as well.  */
      return true;

    case GIMPLE_ASM:
      if (gimple_asm_nlabels (as_a <gasm *> (t)) > 0)
	return true;
      break;

    CASE_GIMPLE_OMP:
      /* OpenMP directives alter control flow.  */
      return true;

    case GIMPLE_TRANSACTION:
      /* A transaction start alters control flow.  */
      return true;

    default:
      break;
    }

  /* If a statement can throw, it alters control flow.  */
  return stmt_can_throw_internal (cfun, t);
}


/* Return true if T is a simple local goto.  */

bool
simple_goto_p (gimple *t)
{
  return (gimple_code (t) == GIMPLE_GOTO
	  && TREE_CODE (gimple_goto_dest (t)) == LABEL_DECL);
}


/* Return true if STMT should start a new basic block.  PREV_STMT is
   the statement preceding STMT.  It is used when STMT is a label or a
   case label.  Labels should only start a new basic block if their
   previous statement wasn't a label.  Otherwise, sequence of labels
   would generate unnecessary basic blocks that only contain a single
   label.  */

static inline bool
stmt_starts_bb_p (gimple *stmt, gimple *prev_stmt)
{
  if (stmt == NULL)
    return false;

  /* PREV_STMT is only set to a debug stmt if the debug stmt is before
     any nondebug stmts in the block.  We don't want to start another
     block in this case: the debug stmt will already have started the
     one STMT would start if we weren't outputting debug stmts.  */
  if (prev_stmt && is_gimple_debug (prev_stmt))
    return false;

  /* Labels start a new basic block only if the preceding statement
     wasn't a label of the same type.  This prevents the creation of
     consecutive blocks that have nothing but a single label.  */
  if (glabel *label_stmt = dyn_cast <glabel *> (stmt))
    {
      /* Nonlocal and computed GOTO targets always start a new block.  */
      if (DECL_NONLOCAL (gimple_label_label (label_stmt))
	  || FORCED_LABEL (gimple_label_label (label_stmt)))
	return true;

      if (prev_stmt && gimple_code (prev_stmt) == GIMPLE_LABEL)
	{
	  if (DECL_NONLOCAL (gimple_label_label (
			       as_a <glabel *> (prev_stmt))))
	    return true;

	  cfg_stats.num_merged_labels++;
	  return false;
	}
      else
	return true;
    }
  else if (gimple_code (stmt) == GIMPLE_CALL)
    {
      if (gimple_call_flags (stmt) & ECF_RETURNS_TWICE)
	/* setjmp acts similar to a nonlocal GOTO target and thus should
	   start a new block.  */
	return true;
      if (gimple_call_internal_p (stmt, IFN_PHI)
	  && prev_stmt
	  && gimple_code (prev_stmt) != GIMPLE_LABEL
	  && (gimple_code (prev_stmt) != GIMPLE_CALL
	      || ! gimple_call_internal_p (prev_stmt, IFN_PHI)))
	/* PHI nodes start a new block unless preceeded by a label
	   or another PHI.  */
	return true;
    }

  return false;
}


/* Return true if T should end a basic block.  */

bool
stmt_ends_bb_p (gimple *t)
{
  return is_ctrl_stmt (t) || is_ctrl_altering_stmt (t);
}

/* Remove block annotations and other data structures.  */

void
delete_tree_cfg_annotations (struct function *fn)
{
  vec_free (label_to_block_map_for_fn (fn));
}

/* Return the virtual phi in BB.  */

gphi *
get_virtual_phi (basic_block bb)
{
  for (gphi_iterator gsi = gsi_start_phis (bb);
       !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      gphi *phi = gsi.phi ();

      if (virtual_operand_p (PHI_RESULT (phi)))
	return phi;
    }

  return NULL;
}

/* Return the first statement in basic block BB.  */

gimple *
first_stmt (basic_block bb)
{
  gimple_stmt_iterator i = gsi_start_bb (bb);
  gimple *stmt = NULL;

  while (!gsi_end_p (i) && is_gimple_debug ((stmt = gsi_stmt (i))))
    {
      gsi_next (&i);
      stmt = NULL;
    }
  return stmt;
}

/* Return the first non-label statement in basic block BB.  */

static gimple *
first_non_label_stmt (basic_block bb)
{
  gimple_stmt_iterator i = gsi_start_bb (bb);
  while (!gsi_end_p (i) && gimple_code (gsi_stmt (i)) == GIMPLE_LABEL)
    gsi_next (&i);
  return !gsi_end_p (i) ? gsi_stmt (i) : NULL;
}

/* Return the last statement in basic block BB.  */

gimple *
last_stmt (basic_block bb)
{
  gimple_stmt_iterator i = gsi_last_bb (bb);
  gimple *stmt = NULL;

  while (!gsi_end_p (i) && is_gimple_debug ((stmt = gsi_stmt (i))))
    {
      gsi_prev (&i);
      stmt = NULL;
    }
  return stmt;
}

/* Return the last statement of an otherwise empty block.  Return NULL
   if the block is totally empty, or if it contains more than one
   statement.  */

gimple *
last_and_only_stmt (basic_block bb)
{
  gimple_stmt_iterator i = gsi_last_nondebug_bb (bb);
  gimple *last, *prev;

  if (gsi_end_p (i))
    return NULL;

  last = gsi_stmt (i);
  gsi_prev_nondebug (&i);
  if (gsi_end_p (i))
    return last;

  /* Empty statements should no longer appear in the instruction stream.
     Everything that might have appeared before should be deleted by
     remove_useless_stmts, and the optimizers should just gsi_remove
     instead of smashing with build_empty_stmt.

     Thus the only thing that should appear here in a block containing
     one executable statement is a label.  */
  prev = gsi_stmt (i);
  if (gimple_code (prev) == GIMPLE_LABEL)
    return last;
  else
    return NULL;
}

/* Reinstall those PHI arguments queued in OLD_EDGE to NEW_EDGE.  */

static void
reinstall_phi_args (edge new_edge, edge old_edge)
{
  edge_var_map *vm;
  int i;
  gphi_iterator phis;

  vec<edge_var_map> *v = redirect_edge_var_map_vector (old_edge);
  if (!v)
    return;

  for (i = 0, phis = gsi_start_phis (new_edge->dest);
       v->iterate (i, &vm) && !gsi_end_p (phis);
       i++, gsi_next (&phis))
    {
      gphi *phi = phis.phi ();
      tree result = redirect_edge_var_map_result (vm);
      tree arg = redirect_edge_var_map_def (vm);

      gcc_assert (result == gimple_phi_result (phi));

      add_phi_arg (phi, arg, new_edge, redirect_edge_var_map_location (vm));
    }

  redirect_edge_var_map_clear (old_edge);
}

/* Returns the basic block after which the new basic block created
   by splitting edge EDGE_IN should be placed.  Tries to keep the new block
   near its "logical" location.  This is of most help to humans looking
   at debugging dumps.  */

basic_block
split_edge_bb_loc (edge edge_in)
{
  basic_block dest = edge_in->dest;
  basic_block dest_prev = dest->prev_bb;

  if (dest_prev)
    {
      edge e = find_edge (dest_prev, dest);
      if (e && !(e->flags & EDGE_COMPLEX))
	return edge_in->src;
    }
  return dest_prev;
}

/* Split a (typically critical) edge EDGE_IN.  Return the new block.
   Abort on abnormal edges.  */

static basic_block
gimple_split_edge (edge edge_in)
{
  basic_block new_bb, after_bb, dest;
  edge new_edge, e;

  /* Abnormal edges cannot be split.  */
  gcc_assert (!(edge_in->flags & EDGE_ABNORMAL));

  dest = edge_in->dest;

  after_bb = split_edge_bb_loc (edge_in);

  new_bb = create_empty_bb (after_bb);
  new_bb->count = edge_in->count ();

  e = redirect_edge_and_branch (edge_in, new_bb);
  gcc_assert (e == edge_in);

  new_edge = make_single_succ_edge (new_bb, dest, EDGE_FALLTHRU);
  reinstall_phi_args (new_edge, e);

  return new_bb;
}


/* Verify properties of the address expression T whose base should be
   TREE_ADDRESSABLE if VERIFY_ADDRESSABLE is true.  */

static bool 
verify_address (tree t, bool verify_addressable)
{
  bool old_constant;
  bool old_side_effects;
  bool new_constant;
  bool new_side_effects;

  old_constant = TREE_CONSTANT (t);
  old_side_effects = TREE_SIDE_EFFECTS (t);

  recompute_tree_invariant_for_addr_expr (t);
  new_side_effects = TREE_SIDE_EFFECTS (t);
  new_constant = TREE_CONSTANT (t);

  if (old_constant != new_constant)
    {
      error ("constant not recomputed when ADDR_EXPR changed");
      return true;
    }
  if (old_side_effects != new_side_effects)
    {
      error ("side effects not recomputed when ADDR_EXPR changed");
      return true;
    }

  tree base = TREE_OPERAND (t, 0);
  while (handled_component_p (base))
    base = TREE_OPERAND (base, 0);

  if (!(VAR_P (base)
	|| TREE_CODE (base) == PARM_DECL
	|| TREE_CODE (base) == RESULT_DECL))
    return false;

  if (DECL_GIMPLE_REG_P (base))
    {
      error ("DECL_GIMPLE_REG_P set on a variable with address taken");
      return true;
    }

  if (verify_addressable && !TREE_ADDRESSABLE (base))
    {
      error ("address taken, but ADDRESSABLE bit not set");
      return true;
    }

  return false;
}


/* Verify if EXPR is either a GIMPLE ID or a GIMPLE indirect reference.
   Returns true if there is an error, otherwise false.  */

static bool
verify_types_in_gimple_min_lval (tree expr)
{
  tree op;

  if (is_gimple_id (expr))
    return false;

  if (TREE_CODE (expr) != TARGET_MEM_REF
      && TREE_CODE (expr) != MEM_REF)
    {
      error ("invalid expression for min lvalue");
      return true;
    }

  /* TARGET_MEM_REFs are strange beasts.  */
  if (TREE_CODE (expr) == TARGET_MEM_REF)
    return false;

  op = TREE_OPERAND (expr, 0);
  if (!is_gimple_val (op))
    {
      error ("invalid operand in indirect reference");
      debug_generic_stmt (op);
      return true;
    }
  /* Memory references now generally can involve a value conversion.  */

  return false;
}

/* Verify if EXPR is a valid GIMPLE reference expression.  If
   REQUIRE_LVALUE is true verifies it is an lvalue.  Returns true
   if there is an error, otherwise false.  */

static bool
verify_types_in_gimple_reference (tree expr, bool require_lvalue)
{
  if (TREE_CODE (expr) == REALPART_EXPR
      || TREE_CODE (expr) == IMAGPART_EXPR
      || TREE_CODE (expr) == BIT_FIELD_REF)
    {
      tree op = TREE_OPERAND (expr, 0);
      if (!is_gimple_reg_type (TREE_TYPE (expr)))
	{
	  error ("non-scalar BIT_FIELD_REF, IMAGPART_EXPR or REALPART_EXPR");
	  return true;
	}

      if (TREE_CODE (expr) == BIT_FIELD_REF)
	{
	  tree t1 = TREE_OPERAND (expr, 1);
	  tree t2 = TREE_OPERAND (expr, 2);
	  poly_uint64 size, bitpos;
	  if (!poly_int_tree_p (t1, &size)
	      || !poly_int_tree_p (t2, &bitpos)
	      || !types_compatible_p (bitsizetype, TREE_TYPE (t1))
	      || !types_compatible_p (bitsizetype, TREE_TYPE (t2)))
	    {
	      error ("invalid position or size operand to BIT_FIELD_REF");
	      return true;
	    }
	  if (INTEGRAL_TYPE_P (TREE_TYPE (expr))
	      && maybe_ne (TYPE_PRECISION (TREE_TYPE (expr)), size))
	    {
	      error ("integral result type precision does not match "
		     "field size of BIT_FIELD_REF");
	      return true;
	    }
	  else if (!INTEGRAL_TYPE_P (TREE_TYPE (expr))
		   && TYPE_MODE (TREE_TYPE (expr)) != BLKmode
		   && maybe_ne (GET_MODE_BITSIZE (TYPE_MODE (TREE_TYPE (expr))),
				size))
	    {
	      error ("mode size of non-integral result does not "
		     "match field size of BIT_FIELD_REF");
	      return true;
	    }
	  if (INTEGRAL_TYPE_P (TREE_TYPE (op))
	      && !type_has_mode_precision_p (TREE_TYPE (op)))
	    {
	      error ("BIT_FIELD_REF of non-mode-precision operand");
	      return true;
	    }
	  if (!AGGREGATE_TYPE_P (TREE_TYPE (op))
	      && maybe_gt (size + bitpos,
			   tree_to_poly_uint64 (TYPE_SIZE (TREE_TYPE (op)))))
	    {
	      error ("position plus size exceeds size of referenced object in "
		     "BIT_FIELD_REF");
	      return true;
	    }
	}

      if ((TREE_CODE (expr) == REALPART_EXPR
	   || TREE_CODE (expr) == IMAGPART_EXPR)
	  && !useless_type_conversion_p (TREE_TYPE (expr),
					 TREE_TYPE (TREE_TYPE (op))))
	{
	  error ("type mismatch in real/imagpart reference");
	  debug_generic_stmt (TREE_TYPE (expr));
	  debug_generic_stmt (TREE_TYPE (TREE_TYPE (op)));
	  return true;
	}
      expr = op;
    }

  while (handled_component_p (expr))
    {
      if (TREE_CODE (expr) == REALPART_EXPR
	  || TREE_CODE (expr) == IMAGPART_EXPR
	  || TREE_CODE (expr) == BIT_FIELD_REF)
	{
	  error ("non-top-level BIT_FIELD_REF, IMAGPART_EXPR or REALPART_EXPR");
	  return true;
	}

      tree op = TREE_OPERAND (expr, 0);

      if (TREE_CODE (expr) == ARRAY_REF
	  || TREE_CODE (expr) == ARRAY_RANGE_REF)
	{
	  if (!is_gimple_val (TREE_OPERAND (expr, 1))
	      || (TREE_OPERAND (expr, 2)
		  && !is_gimple_val (TREE_OPERAND (expr, 2)))
	      || (TREE_OPERAND (expr, 3)
		  && !is_gimple_val (TREE_OPERAND (expr, 3))))
	    {
	      error ("invalid operands to array reference");
	      debug_generic_stmt (expr);
	      return true;
	    }
	}

      /* Verify if the reference array element types are compatible.  */
      if (TREE_CODE (expr) == ARRAY_REF
	  && !useless_type_conversion_p (TREE_TYPE (expr),
					 TREE_TYPE (TREE_TYPE (op))))
	{
	  error ("type mismatch in array reference");
	  debug_generic_stmt (TREE_TYPE (expr));
	  debug_generic_stmt (TREE_TYPE (TREE_TYPE (op)));
	  return true;
	}
      if (TREE_CODE (expr) == ARRAY_RANGE_REF
	  && !useless_type_conversion_p (TREE_TYPE (TREE_TYPE (expr)),
					 TREE_TYPE (TREE_TYPE (op))))
	{
	  error ("type mismatch in array range reference");
	  debug_generic_stmt (TREE_TYPE (TREE_TYPE (expr)));
	  debug_generic_stmt (TREE_TYPE (TREE_TYPE (op)));
	  return true;
	}

      if (TREE_CODE (expr) == COMPONENT_REF)
	{
	  if (TREE_OPERAND (expr, 2)
	      && !is_gimple_val (TREE_OPERAND (expr, 2)))
	    {
	      error ("invalid COMPONENT_REF offset operator");
	      return true;
	    }
	  if (!useless_type_conversion_p (TREE_TYPE (expr),
					  TREE_TYPE (TREE_OPERAND (expr, 1))))
	    {
	      error ("type mismatch in component reference");
	      debug_generic_stmt (TREE_TYPE (expr));
	      debug_generic_stmt (TREE_TYPE (TREE_OPERAND (expr, 1)));
	      return true;
	    }
	}

      if (TREE_CODE (expr) == VIEW_CONVERT_EXPR)
	{
	  /* For VIEW_CONVERT_EXPRs which are allowed here too, we only check
	     that their operand is not an SSA name or an invariant when
	     requiring an lvalue (this usually means there is a SRA or IPA-SRA
	     bug).  Otherwise there is nothing to verify, gross mismatches at
	     most invoke undefined behavior.  */
	  if (require_lvalue
	      && (TREE_CODE (op) == SSA_NAME
		  || is_gimple_min_invariant (op)))
	    {
	      error ("conversion of an SSA_NAME on the left hand side");
	      debug_generic_stmt (expr);
	      return true;
	    }
	  else if (TREE_CODE (op) == SSA_NAME
		   && TYPE_SIZE (TREE_TYPE (expr)) != TYPE_SIZE (TREE_TYPE (op)))
	    {
	      error ("conversion of register to a different size");
	      debug_generic_stmt (expr);
	      return true;
	    }
	  else if (!handled_component_p (op))
	    return false;
	}

      expr = op;
    }

  if (TREE_CODE (expr) == MEM_REF)
    {
      if (!is_gimple_mem_ref_addr (TREE_OPERAND (expr, 0))
	  || (TREE_CODE (TREE_OPERAND (expr, 0)) == ADDR_EXPR
	      && verify_address (TREE_OPERAND (expr, 0), false)))
	{
	  error ("invalid address operand in MEM_REF");
	  debug_generic_stmt (expr);
	  return true;
	}
      if (!poly_int_tree_p (TREE_OPERAND (expr, 1))
	  || !POINTER_TYPE_P (TREE_TYPE (TREE_OPERAND (expr, 1))))
	{
	  error ("invalid offset operand in MEM_REF");
	  debug_generic_stmt (expr);
	  return true;
	}
    }
  else if (TREE_CODE (expr) == TARGET_MEM_REF)
    {
      if (!TMR_BASE (expr)
	  || !is_gimple_mem_ref_addr (TMR_BASE (expr))
	  || (TREE_CODE (TMR_BASE (expr)) == ADDR_EXPR
	      && verify_address (TMR_BASE (expr), false)))
	{
	  error ("invalid address operand in TARGET_MEM_REF");
	  return true;
	}
      if (!TMR_OFFSET (expr)
	  || !poly_int_tree_p (TMR_OFFSET (expr))
	  || !POINTER_TYPE_P (TREE_TYPE (TMR_OFFSET (expr))))
	{
	  error ("invalid offset operand in TARGET_MEM_REF");
	  debug_generic_stmt (expr);
	  return true;
	}
    }
  else if (TREE_CODE (expr) == INDIRECT_REF)
    {
      error ("INDIRECT_REF in gimple IL");
      debug_generic_stmt (expr);
      return true;
    }

  return ((require_lvalue || !is_gimple_min_invariant (expr))
	  && verify_types_in_gimple_min_lval (expr));
}

/* Returns true if there is one pointer type in TYPE_POINTER_TO (SRC_OBJ)
   list of pointer-to types that is trivially convertible to DEST.  */

static bool
one_pointer_to_useless_type_conversion_p (tree dest, tree src_obj)
{
  tree src;

  if (!TYPE_POINTER_TO (src_obj))
    return true;

  for (src = TYPE_POINTER_TO (src_obj); src; src = TYPE_NEXT_PTR_TO (src))
    if (useless_type_conversion_p (dest, src))
      return true;

  return false;
}

/* Return true if TYPE1 is a fixed-point type and if conversions to and
   from TYPE2 can be handled by FIXED_CONVERT_EXPR.  */

static bool
valid_fixed_convert_types_p (tree type1, tree type2)
{
  return (FIXED_POINT_TYPE_P (type1)
	  && (INTEGRAL_TYPE_P (type2)
	      || SCALAR_FLOAT_TYPE_P (type2)
	      || FIXED_POINT_TYPE_P (type2)));
}

/* Verify the contents of a GIMPLE_CALL STMT.  Returns true when there
   is a problem, otherwise false.  */

static bool
verify_gimple_call (gcall *stmt)
{
  tree fn = gimple_call_fn (stmt);
  tree fntype, fndecl;
  unsigned i;

  if (gimple_call_internal_p (stmt))
    {
      if (fn)
	{
	  error ("gimple call has two targets");
	  debug_generic_stmt (fn);
	  return true;
	}
    }
  else
    {
      if (!fn)
	{
	  error ("gimple call has no target");
	  return true;
	}
    }

  if (fn && !is_gimple_call_addr (fn))
    {
      error ("invalid function in gimple call");
      debug_generic_stmt (fn);
      return true;
    }

  if (fn
      && (!POINTER_TYPE_P (TREE_TYPE (fn))
	  || (TREE_CODE (TREE_TYPE (TREE_TYPE (fn))) != FUNCTION_TYPE
	      && TREE_CODE (TREE_TYPE (TREE_TYPE (fn))) != METHOD_TYPE)))
    {
      error ("non-function in gimple call");
      return true;
    }

   fndecl = gimple_call_fndecl (stmt);
   if (fndecl
       && TREE_CODE (fndecl) == FUNCTION_DECL
       && DECL_LOOPING_CONST_OR_PURE_P (fndecl)
       && !DECL_PURE_P (fndecl)
       && !TREE_READONLY (fndecl))
     {
       error ("invalid pure const state for function");
       return true;
     }

  tree lhs = gimple_call_lhs (stmt);
  if (lhs
      && (!is_gimple_lvalue (lhs)
	  || verify_types_in_gimple_reference (lhs, true)))
    {
      error ("invalid LHS in gimple call");
      return true;
    }

  if (gimple_call_ctrl_altering_p (stmt)
      && gimple_call_noreturn_p (stmt)
      && should_remove_lhs_p (lhs))
    {
      error ("LHS in noreturn call");
      return true;
    }

  fntype = gimple_call_fntype (stmt);
  if (fntype
      && lhs
      && !useless_type_conversion_p (TREE_TYPE (lhs), TREE_TYPE (fntype))
      /* ???  At least C++ misses conversions at assignments from
	 void * call results.
	 For now simply allow arbitrary pointer type conversions.  */
      && !(POINTER_TYPE_P (TREE_TYPE (lhs))
	   && POINTER_TYPE_P (TREE_TYPE (fntype))))
    {
      error ("invalid conversion in gimple call");
      debug_generic_stmt (TREE_TYPE (lhs));
      debug_generic_stmt (TREE_TYPE (fntype));
      return true;
    }

  if (gimple_call_chain (stmt)
      && !is_gimple_val (gimple_call_chain (stmt)))
    {
      error ("invalid static chain in gimple call");
      debug_generic_stmt (gimple_call_chain (stmt));
      return true;
    }

  /* If there is a static chain argument, the call should either be
     indirect, or the decl should have DECL_STATIC_CHAIN set.  */
  if (gimple_call_chain (stmt)
      && fndecl
      && !DECL_STATIC_CHAIN (fndecl))
    {
      error ("static chain with function that doesn%'t use one");
      return true;
    }

  if (fndecl && fndecl_built_in_p (fndecl, BUILT_IN_NORMAL))
    {
      switch (DECL_FUNCTION_CODE (fndecl))
	{
	case BUILT_IN_UNREACHABLE:
	case BUILT_IN_TRAP:
	  if (gimple_call_num_args (stmt) > 0)
	    {
	      /* Built-in unreachable with parameters might not be caught by
		 undefined behavior sanitizer.  Front-ends do check users do not
		 call them that way but we also produce calls to
		 __builtin_unreachable internally, for example when IPA figures
		 out a call cannot happen in a legal program.  In such cases,
		 we must make sure arguments are stripped off.  */
	      error ("%<__builtin_unreachable%> or %<__builtin_trap%> call "
		     "with arguments");
	      return true;
	    }
	  break;
	default:
	  break;
	}
    }

  /* ???  The C frontend passes unpromoted arguments in case it
     didn't see a function declaration before the call.  So for now
     leave the call arguments mostly unverified.  Once we gimplify
     unit-at-a-time we have a chance to fix this.  */

  for (i = 0; i < gimple_call_num_args (stmt); ++i)
    {
      tree arg = gimple_call_arg (stmt, i);
      if ((is_gimple_reg_type (TREE_TYPE (arg))
	   && !is_gimple_val (arg))
	  || (!is_gimple_reg_type (TREE_TYPE (arg))
	      && !is_gimple_lvalue (arg)))
	{
	  error ("invalid argument to gimple call");
	  debug_generic_expr (arg);
	  return true;
	}
    }

  return false;
}

/* Verifies the gimple comparison with the result type TYPE and
   the operands OP0 and OP1, comparison code is CODE.  */

static bool
verify_gimple_comparison (tree type, tree op0, tree op1, enum tree_code code)
{
  tree op0_type = TREE_TYPE (op0);
  tree op1_type = TREE_TYPE (op1);

  if (!is_gimple_val (op0) || !is_gimple_val (op1))
    {
      error ("invalid operands in gimple comparison");
      return true;
    }

  /* For comparisons we do not have the operations type as the
     effective type the comparison is carried out in.  Instead
     we require that either the first operand is trivially
     convertible into the second, or the other way around.
     Because we special-case pointers to void we allow
     comparisons of pointers with the same mode as well.  */
  if (!useless_type_conversion_p (op0_type, op1_type)
      && !useless_type_conversion_p (op1_type, op0_type)
      && (!POINTER_TYPE_P (op0_type)
	  || !POINTER_TYPE_P (op1_type)
	  || TYPE_MODE (op0_type) != TYPE_MODE (op1_type)))
    {
      error ("mismatching comparison operand types");
      debug_generic_expr (op0_type);
      debug_generic_expr (op1_type);
      return true;
    }

  /* The resulting type of a comparison may be an effective boolean type.  */
  if (INTEGRAL_TYPE_P (type)
      && (TREE_CODE (type) == BOOLEAN_TYPE
	  || TYPE_PRECISION (type) == 1))
    {
      if ((TREE_CODE (op0_type) == VECTOR_TYPE
	   || TREE_CODE (op1_type) == VECTOR_TYPE)
	  && code != EQ_EXPR && code != NE_EXPR
	  && !VECTOR_BOOLEAN_TYPE_P (op0_type)
	  && !VECTOR_INTEGER_TYPE_P (op0_type))
	{
	  error ("unsupported operation or type for vector comparison"
		 " returning a boolean");
	  debug_generic_expr (op0_type);
	  debug_generic_expr (op1_type);
	  return true;
        }
    }
  /* Or a boolean vector type with the same element count
     as the comparison operand types.  */
  else if (TREE_CODE (type) == VECTOR_TYPE
	   && TREE_CODE (TREE_TYPE (type)) == BOOLEAN_TYPE)
    {
      if (TREE_CODE (op0_type) != VECTOR_TYPE
	  || TREE_CODE (op1_type) != VECTOR_TYPE)
        {
          error ("non-vector operands in vector comparison");
          debug_generic_expr (op0_type);
          debug_generic_expr (op1_type);
          return true;
        }

      if (maybe_ne (TYPE_VECTOR_SUBPARTS (type),
		    TYPE_VECTOR_SUBPARTS (op0_type)))
        {
          error ("invalid vector comparison resulting type");
          debug_generic_expr (type);
          return true;
        }
    }
  else
    {
      error ("bogus comparison result type");
      debug_generic_expr (type);
      return true;
    }

  return false;
}

/* Verify a gimple assignment statement STMT with an unary rhs.
   Returns true if anything is wrong.  */

static bool
verify_gimple_assign_unary (gassign *stmt)
{
  enum tree_code rhs_code = gimple_assign_rhs_code (stmt);
  tree lhs = gimple_assign_lhs (stmt);
  tree lhs_type = TREE_TYPE (lhs);
  tree rhs1 = gimple_assign_rhs1 (stmt);
  tree rhs1_type = TREE_TYPE (rhs1);

  if (!is_gimple_reg (lhs))
    {
      error ("non-register as LHS of unary operation");
      return true;
    }

  if (!is_gimple_val (rhs1))
    {
      error ("invalid operand in unary operation");
      return true;
    }

  /* First handle conversions.  */
  switch (rhs_code)
    {
    CASE_CONVERT:
      {
	/* Allow conversions from pointer type to integral type only if
	   there is no sign or zero extension involved.
	   For targets were the precision of ptrofftype doesn't match that
	   of pointers we need to allow arbitrary conversions to ptrofftype.  */
	if ((POINTER_TYPE_P (lhs_type)
	     && INTEGRAL_TYPE_P (rhs1_type))
	    || (POINTER_TYPE_P (rhs1_type)
		&& INTEGRAL_TYPE_P (lhs_type)
		&& (TYPE_PRECISION (rhs1_type) >= TYPE_PRECISION (lhs_type)
		    || ptrofftype_p (lhs_type))))
	  return false;

	/* Allow conversion from integral to offset type and vice versa.  */
	if ((TREE_CODE (lhs_type) == OFFSET_TYPE
	     && INTEGRAL_TYPE_P (rhs1_type))
	    || (INTEGRAL_TYPE_P (lhs_type)
		&& TREE_CODE (rhs1_type) == OFFSET_TYPE))
	  return false;

	/* Otherwise assert we are converting between types of the
	   same kind.  */
	if (INTEGRAL_TYPE_P (lhs_type) != INTEGRAL_TYPE_P (rhs1_type))
	  {
	    error ("invalid types in nop conversion");
	    debug_generic_expr (lhs_type);
	    debug_generic_expr (rhs1_type);
	    return true;
	  }

	return false;
      }

    case ADDR_SPACE_CONVERT_EXPR:
      {
	if (!POINTER_TYPE_P (rhs1_type) || !POINTER_TYPE_P (lhs_type)
	    || (TYPE_ADDR_SPACE (TREE_TYPE (rhs1_type))
		== TYPE_ADDR_SPACE (TREE_TYPE (lhs_type))))
	  {
	    error ("invalid types in address space conversion");
	    debug_generic_expr (lhs_type);
	    debug_generic_expr (rhs1_type);
	    return true;
	  }

	return false;
      }

    case FIXED_CONVERT_EXPR:
      {
	if (!valid_fixed_convert_types_p (lhs_type, rhs1_type)
	    && !valid_fixed_convert_types_p (rhs1_type, lhs_type))
	  {
	    error ("invalid types in fixed-point conversion");
	    debug_generic_expr (lhs_type);
	    debug_generic_expr (rhs1_type);
	    return true;
	  }

	return false;
      }

    case FLOAT_EXPR:
      {
	if ((!INTEGRAL_TYPE_P (rhs1_type) || !SCALAR_FLOAT_TYPE_P (lhs_type))
	    && (!VECTOR_INTEGER_TYPE_P (rhs1_type)
	        || !VECTOR_FLOAT_TYPE_P (lhs_type)))
	  {
	    error ("invalid types in conversion to floating point");
	    debug_generic_expr (lhs_type);
	    debug_generic_expr (rhs1_type);
	    return true;
	  }

        return false;
      }

    case FIX_TRUNC_EXPR:
      {
        if ((!INTEGRAL_TYPE_P (lhs_type) || !SCALAR_FLOAT_TYPE_P (rhs1_type))
            && (!VECTOR_INTEGER_TYPE_P (lhs_type)
                || !VECTOR_FLOAT_TYPE_P (rhs1_type)))
	  {
	    error ("invalid types in conversion to integer");
	    debug_generic_expr (lhs_type);
	    debug_generic_expr (rhs1_type);
	    return true;
	  }

        return false;
      }

    case VEC_UNPACK_HI_EXPR:
    case VEC_UNPACK_LO_EXPR:
    case VEC_UNPACK_FLOAT_HI_EXPR:
    case VEC_UNPACK_FLOAT_LO_EXPR:
    case VEC_UNPACK_FIX_TRUNC_HI_EXPR:
    case VEC_UNPACK_FIX_TRUNC_LO_EXPR:
      if (TREE_CODE (rhs1_type) != VECTOR_TYPE
          || TREE_CODE (lhs_type) != VECTOR_TYPE
          || (!INTEGRAL_TYPE_P (TREE_TYPE (lhs_type))
	      && !SCALAR_FLOAT_TYPE_P (TREE_TYPE (lhs_type)))
          || (!INTEGRAL_TYPE_P (TREE_TYPE (rhs1_type))
	      && !SCALAR_FLOAT_TYPE_P (TREE_TYPE (rhs1_type)))
	  || ((rhs_code == VEC_UNPACK_HI_EXPR
	       || rhs_code == VEC_UNPACK_LO_EXPR)
	      && (INTEGRAL_TYPE_P (TREE_TYPE (lhs_type))
		  != INTEGRAL_TYPE_P (TREE_TYPE (rhs1_type))))
	  || ((rhs_code == VEC_UNPACK_FLOAT_HI_EXPR
	       || rhs_code == VEC_UNPACK_FLOAT_LO_EXPR)
	      && (INTEGRAL_TYPE_P (TREE_TYPE (lhs_type))
		  || SCALAR_FLOAT_TYPE_P (TREE_TYPE (rhs1_type))))
	  || ((rhs_code == VEC_UNPACK_FIX_TRUNC_HI_EXPR
	       || rhs_code == VEC_UNPACK_FIX_TRUNC_LO_EXPR)
	      && (INTEGRAL_TYPE_P (TREE_TYPE (rhs1_type))
		  || SCALAR_FLOAT_TYPE_P (TREE_TYPE (lhs_type))))
	  || (maybe_ne (GET_MODE_SIZE (element_mode (lhs_type)),
			2 * GET_MODE_SIZE (element_mode (rhs1_type)))
	      && (!VECTOR_BOOLEAN_TYPE_P (lhs_type)
		  || !VECTOR_BOOLEAN_TYPE_P (rhs1_type)))
	  || maybe_ne (2 * TYPE_VECTOR_SUBPARTS (lhs_type),
		       TYPE_VECTOR_SUBPARTS (rhs1_type)))
	{
	  error ("type mismatch in vector unpack expression");
	  debug_generic_expr (lhs_type);
	  debug_generic_expr (rhs1_type);
	  return true;
        }

      return false;

    case NEGATE_EXPR:
    case ABS_EXPR:
    case BIT_NOT_EXPR:
    case PAREN_EXPR:
    case CONJ_EXPR:
      break;

    case ABSU_EXPR:
      if (!ANY_INTEGRAL_TYPE_P (lhs_type)
	  || !TYPE_UNSIGNED (lhs_type)
	  || !ANY_INTEGRAL_TYPE_P (rhs1_type)
	  || TYPE_UNSIGNED (rhs1_type)
	  || element_precision (lhs_type) != element_precision (rhs1_type))
	{
	  error ("invalid types for ABSU_EXPR");
	  debug_generic_expr (lhs_type);
	  debug_generic_expr (rhs1_type);
	  return true;
	}
      return false;

    case VEC_DUPLICATE_EXPR:
      if (TREE_CODE (lhs_type) != VECTOR_TYPE
	  || !useless_type_conversion_p (TREE_TYPE (lhs_type), rhs1_type))
	{
	  error ("vec_duplicate should be from a scalar to a like vector");
	  debug_generic_expr (lhs_type);
	  debug_generic_expr (rhs1_type);
	  return true;
	}
      return false;

    default:
      gcc_unreachable ();
    }

  /* For the remaining codes assert there is no conversion involved.  */
  if (!useless_type_conversion_p (lhs_type, rhs1_type))
    {
      error ("non-trivial conversion in unary operation");
      debug_generic_expr (lhs_type);
      debug_generic_expr (rhs1_type);
      return true;
    }

  return false;
}

/* Verify a gimple assignment statement STMT with a binary rhs.
   Returns true if anything is wrong.  */

static bool
verify_gimple_assign_binary (gassign *stmt)
{
  enum tree_code rhs_code = gimple_assign_rhs_code (stmt);
  tree lhs = gimple_assign_lhs (stmt);
  tree lhs_type = TREE_TYPE (lhs);
  tree rhs1 = gimple_assign_rhs1 (stmt);
  tree rhs1_type = TREE_TYPE (rhs1);
  tree rhs2 = gimple_assign_rhs2 (stmt);
  tree rhs2_type = TREE_TYPE (rhs2);

  if (!is_gimple_reg (lhs))
    {
      error ("non-register as LHS of binary operation");
      return true;
    }

  if (!is_gimple_val (rhs1)
      || !is_gimple_val (rhs2))
    {
      error ("invalid operands in binary operation");
      return true;
    }

  /* First handle operations that involve different types.  */
  switch (rhs_code)
    {
    case COMPLEX_EXPR:
      {
	if (TREE_CODE (lhs_type) != COMPLEX_TYPE
	    || !(INTEGRAL_TYPE_P (rhs1_type)
	         || SCALAR_FLOAT_TYPE_P (rhs1_type))
	    || !(INTEGRAL_TYPE_P (rhs2_type)
	         || SCALAR_FLOAT_TYPE_P (rhs2_type)))
	  {
	    error ("type mismatch in complex expression");
	    debug_generic_expr (lhs_type);
	    debug_generic_expr (rhs1_type);
	    debug_generic_expr (rhs2_type);
	    return true;
	  }

	return false;
      }

    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
      {
	/* Shifts and rotates are ok on integral types, fixed point
	   types and integer vector types.  */
	if ((!INTEGRAL_TYPE_P (rhs1_type)
	     && !FIXED_POINT_TYPE_P (rhs1_type)
	     && !(TREE_CODE (rhs1_type) == VECTOR_TYPE
		  && INTEGRAL_TYPE_P (TREE_TYPE (rhs1_type))))
	    || (!INTEGRAL_TYPE_P (rhs2_type)
		/* Vector shifts of vectors are also ok.  */
		&& !(TREE_CODE (rhs1_type) == VECTOR_TYPE
		     && INTEGRAL_TYPE_P (TREE_TYPE (rhs1_type))
		     && TREE_CODE (rhs2_type) == VECTOR_TYPE
		     && INTEGRAL_TYPE_P (TREE_TYPE (rhs2_type))))
	    || !useless_type_conversion_p (lhs_type, rhs1_type))
	  {
	    error ("type mismatch in shift expression");
	    debug_generic_expr (lhs_type);
	    debug_generic_expr (rhs1_type);
	    debug_generic_expr (rhs2_type);
	    return true;
	  }

	return false;
      }

    case WIDEN_LSHIFT_EXPR:
      {
        if (!INTEGRAL_TYPE_P (lhs_type)
            || !INTEGRAL_TYPE_P (rhs1_type)
            || TREE_CODE (rhs2) != INTEGER_CST
            || (2 * TYPE_PRECISION (rhs1_type) > TYPE_PRECISION (lhs_type)))
          {
            error ("type mismatch in widening vector shift expression");
            debug_generic_expr (lhs_type);
            debug_generic_expr (rhs1_type);
            debug_generic_expr (rhs2_type);
            return true;
          }

        return false;
      }

    case VEC_WIDEN_LSHIFT_HI_EXPR:
    case VEC_WIDEN_LSHIFT_LO_EXPR:
      {
        if (TREE_CODE (rhs1_type) != VECTOR_TYPE
            || TREE_CODE (lhs_type) != VECTOR_TYPE
            || !INTEGRAL_TYPE_P (TREE_TYPE (rhs1_type))
            || !INTEGRAL_TYPE_P (TREE_TYPE (lhs_type))
            || TREE_CODE (rhs2) != INTEGER_CST
            || (2 * TYPE_PRECISION (TREE_TYPE (rhs1_type))
                > TYPE_PRECISION (TREE_TYPE (lhs_type))))
          {
            error ("type mismatch in widening vector shift expression");
            debug_generic_expr (lhs_type);
            debug_generic_expr (rhs1_type);
            debug_generic_expr (rhs2_type);
            return true;
          }

        return false;
      }

    case PLUS_EXPR:
    case MINUS_EXPR:
      {
	tree lhs_etype = lhs_type;
	tree rhs1_etype = rhs1_type;
	tree rhs2_etype = rhs2_type;
	if (TREE_CODE (lhs_type) == VECTOR_TYPE)
	  {
	    if (TREE_CODE (rhs1_type) != VECTOR_TYPE
		|| TREE_CODE (rhs2_type) != VECTOR_TYPE)
	      {
		error ("invalid non-vector operands to vector valued plus");
		return true;
	      }
	    lhs_etype = TREE_TYPE (lhs_type);
	    rhs1_etype = TREE_TYPE (rhs1_type);
	    rhs2_etype = TREE_TYPE (rhs2_type);
	  }
	if (POINTER_TYPE_P (lhs_etype)
	    || POINTER_TYPE_P (rhs1_etype)
	    || POINTER_TYPE_P (rhs2_etype))
	  {
	    error ("invalid (pointer) operands to plus/minus");
	    return true;
	  }

	/* Continue with generic binary expression handling.  */
	break;
      }

    case POINTER_PLUS_EXPR:
      {
	if (!POINTER_TYPE_P (rhs1_type)
	    || !useless_type_conversion_p (lhs_type, rhs1_type)
	    || !ptrofftype_p (rhs2_type))
	  {
	    error ("type mismatch in pointer plus expression");
	    debug_generic_stmt (lhs_type);
	    debug_generic_stmt (rhs1_type);
	    debug_generic_stmt (rhs2_type);
	    return true;
	  }

	return false;
      }

    case POINTER_DIFF_EXPR:
      {
	if (!POINTER_TYPE_P (rhs1_type)
	    || !POINTER_TYPE_P (rhs2_type)
	    /* Because we special-case pointers to void we allow difference
	       of arbitrary pointers with the same mode.  */
	    || TYPE_MODE (rhs1_type) != TYPE_MODE (rhs2_type)
	    || TREE_CODE (lhs_type) != INTEGER_TYPE
	    || TYPE_UNSIGNED (lhs_type)
	    || TYPE_PRECISION (lhs_type) != TYPE_PRECISION (rhs1_type))
	  {
	    error ("type mismatch in pointer diff expression");
	    debug_generic_stmt (lhs_type);
	    debug_generic_stmt (rhs1_type);
	    debug_generic_stmt (rhs2_type);
	    return true;
	  }

	return false;
      }

    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:

      gcc_unreachable ();

    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
    case UNORDERED_EXPR:
    case ORDERED_EXPR:
    case UNLT_EXPR:
    case UNLE_EXPR:
    case UNGT_EXPR:
    case UNGE_EXPR:
    case UNEQ_EXPR:
    case LTGT_EXPR:
      /* Comparisons are also binary, but the result type is not
	 connected to the operand types.  */
      return verify_gimple_comparison (lhs_type, rhs1, rhs2, rhs_code);

    case WIDEN_MULT_EXPR:
      if (TREE_CODE (lhs_type) != INTEGER_TYPE)
	return true;
      return ((2 * TYPE_PRECISION (rhs1_type) > TYPE_PRECISION (lhs_type))
	      || (TYPE_PRECISION (rhs1_type) != TYPE_PRECISION (rhs2_type)));

    case WIDEN_SUM_EXPR:
      {
        if (((TREE_CODE (rhs1_type) != VECTOR_TYPE
	      || TREE_CODE (lhs_type) != VECTOR_TYPE)
	     && ((!INTEGRAL_TYPE_P (rhs1_type)
		  && !SCALAR_FLOAT_TYPE_P (rhs1_type))
		 || (!INTEGRAL_TYPE_P (lhs_type)
		     && !SCALAR_FLOAT_TYPE_P (lhs_type))))
	    || !useless_type_conversion_p (lhs_type, rhs2_type)
	    || maybe_lt (GET_MODE_SIZE (element_mode (rhs2_type)),
			 2 * GET_MODE_SIZE (element_mode (rhs1_type))))
          {
            error ("type mismatch in widening sum reduction");
            debug_generic_expr (lhs_type);
            debug_generic_expr (rhs1_type);
            debug_generic_expr (rhs2_type);
            return true;
          }
        return false;
      }

    case VEC_WIDEN_MULT_HI_EXPR:
    case VEC_WIDEN_MULT_LO_EXPR:
    case VEC_WIDEN_MULT_EVEN_EXPR:
    case VEC_WIDEN_MULT_ODD_EXPR:
      {
        if (TREE_CODE (rhs1_type) != VECTOR_TYPE
            || TREE_CODE (lhs_type) != VECTOR_TYPE
	    || !types_compatible_p (rhs1_type, rhs2_type)
	    || maybe_ne (GET_MODE_SIZE (element_mode (lhs_type)),
			 2 * GET_MODE_SIZE (element_mode (rhs1_type))))
          {
            error ("type mismatch in vector widening multiplication");
            debug_generic_expr (lhs_type);
            debug_generic_expr (rhs1_type);
            debug_generic_expr (rhs2_type);
            return true;
          }
        return false;
      }

    case VEC_PACK_TRUNC_EXPR:
      /* ???  We currently use VEC_PACK_TRUNC_EXPR to simply concat
	 vector boolean types.  */
      if (VECTOR_BOOLEAN_TYPE_P (lhs_type)
	  && VECTOR_BOOLEAN_TYPE_P (rhs1_type)
	  && types_compatible_p (rhs1_type, rhs2_type)
	  && known_eq (TYPE_VECTOR_SUBPARTS (lhs_type),
		       2 * TYPE_VECTOR_SUBPARTS (rhs1_type)))
	return false;

      /* Fallthru.  */
    case VEC_PACK_SAT_EXPR:
    case VEC_PACK_FIX_TRUNC_EXPR:
      {
        if (TREE_CODE (rhs1_type) != VECTOR_TYPE
            || TREE_CODE (lhs_type) != VECTOR_TYPE
            || !((rhs_code == VEC_PACK_FIX_TRUNC_EXPR
		  && SCALAR_FLOAT_TYPE_P (TREE_TYPE (rhs1_type))
		  && INTEGRAL_TYPE_P (TREE_TYPE (lhs_type)))
		 || (INTEGRAL_TYPE_P (TREE_TYPE (rhs1_type))
		     == INTEGRAL_TYPE_P (TREE_TYPE (lhs_type))))
	    || !types_compatible_p (rhs1_type, rhs2_type)
	    || maybe_ne (GET_MODE_SIZE (element_mode (rhs1_type)),
			 2 * GET_MODE_SIZE (element_mode (lhs_type)))
	    || maybe_ne (2 * TYPE_VECTOR_SUBPARTS (rhs1_type),
			 TYPE_VECTOR_SUBPARTS (lhs_type)))
          {
            error ("type mismatch in vector pack expression");
            debug_generic_expr (lhs_type);
            debug_generic_expr (rhs1_type);
            debug_generic_expr (rhs2_type);
            return true;
          }

        return false;
      }

    case VEC_PACK_FLOAT_EXPR:
      if (TREE_CODE (rhs1_type) != VECTOR_TYPE
	  || TREE_CODE (lhs_type) != VECTOR_TYPE
	  || !INTEGRAL_TYPE_P (TREE_TYPE (rhs1_type))
	  || !SCALAR_FLOAT_TYPE_P (TREE_TYPE (lhs_type))
	  || !types_compatible_p (rhs1_type, rhs2_type)
	  || maybe_ne (GET_MODE_SIZE (element_mode (rhs1_type)),
		       2 * GET_MODE_SIZE (element_mode (lhs_type)))
	  || maybe_ne (2 * TYPE_VECTOR_SUBPARTS (rhs1_type),
		       TYPE_VECTOR_SUBPARTS (lhs_type)))
	{
	  error ("type mismatch in vector pack expression");
	  debug_generic_expr (lhs_type);
	  debug_generic_expr (rhs1_type);
	  debug_generic_expr (rhs2_type);
	  return true;
	}

      return false;

    case MULT_EXPR:
    case MULT_HIGHPART_EXPR:
    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case TRUNC_MOD_EXPR:
    case CEIL_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case ROUND_MOD_EXPR:
    case RDIV_EXPR:
    case EXACT_DIV_EXPR:
    case MIN_EXPR:
    case MAX_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case BIT_AND_EXPR:
      /* Continue with generic binary expression handling.  */
      break;

    case VEC_SERIES_EXPR:
      if (!useless_type_conversion_p (rhs1_type, rhs2_type))
	{
	  error ("type mismatch in series expression");
	  debug_generic_expr (rhs1_type);
	  debug_generic_expr (rhs2_type);
	  return true;
	}
      if (TREE_CODE (lhs_type) != VECTOR_TYPE
	  || !useless_type_conversion_p (TREE_TYPE (lhs_type), rhs1_type))
	{
	  error ("vector type expected in series expression");
	  debug_generic_expr (lhs_type);
	  return true;
	}
      return false;

    default:
      gcc_unreachable ();
    }

  if (!useless_type_conversion_p (lhs_type, rhs1_type)
      || !useless_type_conversion_p (lhs_type, rhs2_type))
    {
      error ("type mismatch in binary expression");
      debug_generic_stmt (lhs_type);
      debug_generic_stmt (rhs1_type);
      debug_generic_stmt (rhs2_type);
      return true;
    }

  return false;
}

/* Verify a gimple assignment statement STMT with a ternary rhs.
   Returns true if anything is wrong.  */

static bool
verify_gimple_assign_ternary (gassign *stmt)
{
  enum tree_code rhs_code = gimple_assign_rhs_code (stmt);
  tree lhs = gimple_assign_lhs (stmt);
  tree lhs_type = TREE_TYPE (lhs);
  tree rhs1 = gimple_assign_rhs1 (stmt);
  tree rhs1_type = TREE_TYPE (rhs1);
  tree rhs2 = gimple_assign_rhs2 (stmt);
  tree rhs2_type = TREE_TYPE (rhs2);
  tree rhs3 = gimple_assign_rhs3 (stmt);
  tree rhs3_type = TREE_TYPE (rhs3);

  if (!is_gimple_reg (lhs))
    {
      error ("non-register as LHS of ternary operation");
      return true;
    }

  if (((rhs_code == VEC_COND_EXPR || rhs_code == COND_EXPR)
       ? !is_gimple_condexpr (rhs1) : !is_gimple_val (rhs1))
      || !is_gimple_val (rhs2)
      || !is_gimple_val (rhs3))
    {
      error ("invalid operands in ternary operation");
      return true;
    }

  /* First handle operations that involve different types.  */
  switch (rhs_code)
    {
    case WIDEN_MULT_PLUS_EXPR:
    case WIDEN_MULT_MINUS_EXPR:
      if ((!INTEGRAL_TYPE_P (rhs1_type)
	   && !FIXED_POINT_TYPE_P (rhs1_type))
	  || !useless_type_conversion_p (rhs1_type, rhs2_type)
	  || !useless_type_conversion_p (lhs_type, rhs3_type)
	  || 2 * TYPE_PRECISION (rhs1_type) > TYPE_PRECISION (lhs_type)
	  || TYPE_PRECISION (rhs1_type) != TYPE_PRECISION (rhs2_type))
	{
	  error ("type mismatch in widening multiply-accumulate expression");
	  debug_generic_expr (lhs_type);
	  debug_generic_expr (rhs1_type);
	  debug_generic_expr (rhs2_type);
	  debug_generic_expr (rhs3_type);
	  return true;
	}
      break;

    case VEC_COND_EXPR:
      if (!VECTOR_BOOLEAN_TYPE_P (rhs1_type)
	  || maybe_ne (TYPE_VECTOR_SUBPARTS (rhs1_type),
		       TYPE_VECTOR_SUBPARTS (lhs_type)))
	{
	  error ("the first argument of a VEC_COND_EXPR must be of a "
		 "boolean vector type of the same number of elements "
		 "as the result");
	  debug_generic_expr (lhs_type);
	  debug_generic_expr (rhs1_type);
	  return true;
	}
      /* Fallthrough.  */
    case COND_EXPR:
      if (!is_gimple_val (rhs1)
	  && verify_gimple_comparison (TREE_TYPE (rhs1),
				       TREE_OPERAND (rhs1, 0),
				       TREE_OPERAND (rhs1, 1),
				       TREE_CODE (rhs1)))
	return true;
      if (!useless_type_conversion_p (lhs_type, rhs2_type)
	  || !useless_type_conversion_p (lhs_type, rhs3_type))
	{
	  error ("type mismatch in conditional expression");
	  debug_generic_expr (lhs_type);
	  debug_generic_expr (rhs2_type);
	  debug_generic_expr (rhs3_type);
	  return true;
	}
      break;

    case VEC_PERM_EXPR:
      if (!useless_type_conversion_p (lhs_type, rhs1_type)
	  || !useless_type_conversion_p (lhs_type, rhs2_type))
	{
	  error ("type mismatch in vector permute expression");
	  debug_generic_expr (lhs_type);
	  debug_generic_expr (rhs1_type);
	  debug_generic_expr (rhs2_type);
	  debug_generic_expr (rhs3_type);
	  return true;
	}

      if (TREE_CODE (rhs1_type) != VECTOR_TYPE
	  || TREE_CODE (rhs2_type) != VECTOR_TYPE
	  || TREE_CODE (rhs3_type) != VECTOR_TYPE)
	{
	  error ("vector types expected in vector permute expression");
	  debug_generic_expr (lhs_type);
	  debug_generic_expr (rhs1_type);
	  debug_generic_expr (rhs2_type);
	  debug_generic_expr (rhs3_type);
	  return true;
	}

      if (maybe_ne (TYPE_VECTOR_SUBPARTS (rhs1_type),
		    TYPE_VECTOR_SUBPARTS (rhs2_type))
	  || maybe_ne (TYPE_VECTOR_SUBPARTS (rhs2_type),
		       TYPE_VECTOR_SUBPARTS (rhs3_type))
	  || maybe_ne (TYPE_VECTOR_SUBPARTS (rhs3_type),
		       TYPE_VECTOR_SUBPARTS (lhs_type)))
	{
	  error ("vectors with different element number found "
		 "in vector permute expression");
	  debug_generic_expr (lhs_type);
	  debug_generic_expr (rhs1_type);
	  debug_generic_expr (rhs2_type);
	  debug_generic_expr (rhs3_type);
	  return true;
	}

      if (TREE_CODE (TREE_TYPE (rhs3_type)) != INTEGER_TYPE
	  || (TREE_CODE (rhs3) != VECTOR_CST
	      && (GET_MODE_BITSIZE (SCALAR_INT_TYPE_MODE
				    (TREE_TYPE (rhs3_type)))
		  != GET_MODE_BITSIZE (SCALAR_TYPE_MODE
				       (TREE_TYPE (rhs1_type))))))
	{
	  error ("invalid mask type in vector permute expression");
	  debug_generic_expr (lhs_type);
	  debug_generic_expr (rhs1_type);
	  debug_generic_expr (rhs2_type);
	  debug_generic_expr (rhs3_type);
	  return true;
	}

      return false;

    case SAD_EXPR:
      if (!useless_type_conversion_p (rhs1_type, rhs2_type)
	  || !useless_type_conversion_p (lhs_type, rhs3_type)
	  || 2 * GET_MODE_UNIT_BITSIZE (TYPE_MODE (TREE_TYPE (rhs1_type)))
	       > GET_MODE_UNIT_BITSIZE (TYPE_MODE (TREE_TYPE (lhs_type))))
	{
	  error ("type mismatch in sad expression");
	  debug_generic_expr (lhs_type);
	  debug_generic_expr (rhs1_type);
	  debug_generic_expr (rhs2_type);
	  debug_generic_expr (rhs3_type);
	  return true;
	}

      if (TREE_CODE (rhs1_type) != VECTOR_TYPE
	  || TREE_CODE (rhs2_type) != VECTOR_TYPE
	  || TREE_CODE (rhs3_type) != VECTOR_TYPE)
	{
	  error ("vector types expected in sad expression");
	  debug_generic_expr (lhs_type);
	  debug_generic_expr (rhs1_type);
	  debug_generic_expr (rhs2_type);
	  debug_generic_expr (rhs3_type);
	  return true;
	}

      return false;

    case BIT_INSERT_EXPR:
      if (! useless_type_conversion_p (lhs_type, rhs1_type))
	{
	  error ("type mismatch in BIT_INSERT_EXPR");
	  debug_generic_expr (lhs_type);
	  debug_generic_expr (rhs1_type);
	  return true;
	}
      if (! ((INTEGRAL_TYPE_P (rhs1_type)
	      && INTEGRAL_TYPE_P (rhs2_type))
	     || (VECTOR_TYPE_P (rhs1_type)
		 && types_compatible_p (TREE_TYPE (rhs1_type), rhs2_type))))
	{
	  error ("not allowed type combination in BIT_INSERT_EXPR");
	  debug_generic_expr (rhs1_type);
	  debug_generic_expr (rhs2_type);
	  return true;
	}
      if (! tree_fits_uhwi_p (rhs3)
	  || ! types_compatible_p (bitsizetype, TREE_TYPE (rhs3))
	  || ! tree_fits_uhwi_p (TYPE_SIZE (rhs2_type)))
	{
	  error ("invalid position or size in BIT_INSERT_EXPR");
	  return true;
	}
      if (INTEGRAL_TYPE_P (rhs1_type)
	  && !type_has_mode_precision_p (rhs1_type))
	{
	  error ("BIT_INSERT_EXPR into non-mode-precision operand");
	  return true;
	}
      if (INTEGRAL_TYPE_P (rhs1_type))
	{
	  unsigned HOST_WIDE_INT bitpos = tree_to_uhwi (rhs3);
	  if (bitpos >= TYPE_PRECISION (rhs1_type)
	      || (bitpos + TYPE_PRECISION (rhs2_type)
		  > TYPE_PRECISION (rhs1_type)))
	    {
	      error ("insertion out of range in BIT_INSERT_EXPR");
	      return true;
	    }
	}
      else if (VECTOR_TYPE_P (rhs1_type))
	{
	  unsigned HOST_WIDE_INT bitpos = tree_to_uhwi (rhs3);
	  unsigned HOST_WIDE_INT bitsize = tree_to_uhwi (TYPE_SIZE (rhs2_type));
	  if (bitpos % bitsize != 0)
	    {
	      error ("vector insertion not at element boundary");
	      return true;
	    }
	}
      return false;

    case DOT_PROD_EXPR:
      {
        if (((TREE_CODE (rhs1_type) != VECTOR_TYPE
	      || TREE_CODE (lhs_type) != VECTOR_TYPE)
	     && ((!INTEGRAL_TYPE_P (rhs1_type)
		  && !SCALAR_FLOAT_TYPE_P (rhs1_type))
		 || (!INTEGRAL_TYPE_P (lhs_type)
		     && !SCALAR_FLOAT_TYPE_P (lhs_type))))
	    || !types_compatible_p (rhs1_type, rhs2_type)
	    || !useless_type_conversion_p (lhs_type, rhs3_type)
	    || maybe_lt (GET_MODE_SIZE (element_mode (rhs3_type)),
			 2 * GET_MODE_SIZE (element_mode (rhs1_type))))
          {
            error ("type mismatch in dot product reduction");
            debug_generic_expr (lhs_type);
            debug_generic_expr (rhs1_type);
            debug_generic_expr (rhs2_type);
            return true;
          }
        return false;
      }

    case REALIGN_LOAD_EXPR:
      /* FIXME.  */
      return false;

    default:
      gcc_unreachable ();
    }
  return false;
}

/* Verify a gimple assignment statement STMT with a single rhs.
   Returns true if anything is wrong.  */

static bool
verify_gimple_assign_single (gassign *stmt)
{
  enum tree_code rhs_code = gimple_assign_rhs_code (stmt);
  tree lhs = gimple_assign_lhs (stmt);
  tree lhs_type = TREE_TYPE (lhs);
  tree rhs1 = gimple_assign_rhs1 (stmt);
  tree rhs1_type = TREE_TYPE (rhs1);
  bool res = false;

  if (!useless_type_conversion_p (lhs_type, rhs1_type))
    {
      error ("non-trivial conversion at assignment");
      debug_generic_expr (lhs_type);
      debug_generic_expr (rhs1_type);
      return true;
    }

  if (gimple_clobber_p (stmt)
      && !(DECL_P (lhs) || TREE_CODE (lhs) == MEM_REF))
    {
      error ("non-decl/MEM_REF LHS in clobber statement");
      debug_generic_expr (lhs);
      return true;
    }

  if (handled_component_p (lhs)
      || TREE_CODE (lhs) == MEM_REF
      || TREE_CODE (lhs) == TARGET_MEM_REF)
    res |= verify_types_in_gimple_reference (lhs, true);

  /* Special codes we cannot handle via their class.  */
  switch (rhs_code)
    {
    case ADDR_EXPR:
      {
	tree op = TREE_OPERAND (rhs1, 0);
	if (!is_gimple_addressable (op))
	  {
	    error ("invalid operand in unary expression");
	    return true;
	  }

	/* Technically there is no longer a need for matching types, but
	   gimple hygiene asks for this check.  In LTO we can end up
	   combining incompatible units and thus end up with addresses
	   of globals that change their type to a common one.  */
	if (!in_lto_p
	    && !types_compatible_p (TREE_TYPE (op),
				    TREE_TYPE (TREE_TYPE (rhs1)))
	    && !one_pointer_to_useless_type_conversion_p (TREE_TYPE (rhs1),
							  TREE_TYPE (op)))
	  {
	    error ("type mismatch in address expression");
	    debug_generic_stmt (TREE_TYPE (rhs1));
	    debug_generic_stmt (TREE_TYPE (op));
	    return true;
	  }

	return (verify_address (rhs1, true)
		|| verify_types_in_gimple_reference (op, true));
      }

    /* tcc_reference  */
    case INDIRECT_REF:
      error ("INDIRECT_REF in gimple IL");
      return true;

    case COMPONENT_REF:
    case BIT_FIELD_REF:
    case ARRAY_REF:
    case ARRAY_RANGE_REF:
    case VIEW_CONVERT_EXPR:
    case REALPART_EXPR:
    case IMAGPART_EXPR:
    case TARGET_MEM_REF:
    case MEM_REF:
      if (!is_gimple_reg (lhs)
	  && is_gimple_reg_type (TREE_TYPE (lhs)))
	{
	  error ("invalid rhs for gimple memory store");
	  debug_generic_stmt (lhs);
	  debug_generic_stmt (rhs1);
	  return true;
	}
      return res || verify_types_in_gimple_reference (rhs1, false);

    /* tcc_constant  */
    case SSA_NAME:
    case INTEGER_CST:
    case REAL_CST:
    case FIXED_CST:
    case COMPLEX_CST:
    case VECTOR_CST:
    case STRING_CST:
      return res;

    /* tcc_declaration  */
    case CONST_DECL:
      return res;
    case VAR_DECL:
    case PARM_DECL:
      if (!is_gimple_reg (lhs)
	  && !is_gimple_reg (rhs1)
	  && is_gimple_reg_type (TREE_TYPE (lhs)))
	{
	  error ("invalid rhs for gimple memory store");
	  debug_generic_stmt (lhs);
	  debug_generic_stmt (rhs1);
	  return true;
	}
      return res;

    case CONSTRUCTOR:
      if (TREE_CODE (rhs1_type) == VECTOR_TYPE)
	{
	  unsigned int i;
	  tree elt_i, elt_v, elt_t = NULL_TREE;

	  if (CONSTRUCTOR_NELTS (rhs1) == 0)
	    return res;
	  /* For vector CONSTRUCTORs we require that either it is empty
	     CONSTRUCTOR, or it is a CONSTRUCTOR of smaller vector elements
	     (then the element count must be correct to cover the whole
	     outer vector and index must be NULL on all elements, or it is
	     a CONSTRUCTOR of scalar elements, where we as an exception allow
	     smaller number of elements (assuming zero filling) and
	     consecutive indexes as compared to NULL indexes (such
	     CONSTRUCTORs can appear in the IL from FEs).  */
	  FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (rhs1), i, elt_i, elt_v)
	    {
	      if (elt_t == NULL_TREE)
		{
		  elt_t = TREE_TYPE (elt_v);
		  if (TREE_CODE (elt_t) == VECTOR_TYPE)
		    {
		      tree elt_t = TREE_TYPE (elt_v);
		      if (!useless_type_conversion_p (TREE_TYPE (rhs1_type),
						      TREE_TYPE (elt_t)))
			{
			  error ("incorrect type of vector CONSTRUCTOR"
				 " elements");
			  debug_generic_stmt (rhs1);
			  return true;
			}
		      else if (maybe_ne (CONSTRUCTOR_NELTS (rhs1)
					 * TYPE_VECTOR_SUBPARTS (elt_t),
					 TYPE_VECTOR_SUBPARTS (rhs1_type)))
			{
			  error ("incorrect number of vector CONSTRUCTOR"
				 " elements");
			  debug_generic_stmt (rhs1);
			  return true;
			}
		    }
		  else if (!useless_type_conversion_p (TREE_TYPE (rhs1_type),
						       elt_t))
		    {
		      error ("incorrect type of vector CONSTRUCTOR elements");
		      debug_generic_stmt (rhs1);
		      return true;
		    }
		  else if (maybe_gt (CONSTRUCTOR_NELTS (rhs1),
				     TYPE_VECTOR_SUBPARTS (rhs1_type)))
		    {
		      error ("incorrect number of vector CONSTRUCTOR elements");
		      debug_generic_stmt (rhs1);
		      return true;
		    }
		}
	      else if (!useless_type_conversion_p (elt_t, TREE_TYPE (elt_v)))
		{
		  error ("incorrect type of vector CONSTRUCTOR elements");
		  debug_generic_stmt (rhs1);
		  return true;
		}
	      if (elt_i != NULL_TREE
		  && (TREE_CODE (elt_t) == VECTOR_TYPE
		      || TREE_CODE (elt_i) != INTEGER_CST
		      || compare_tree_int (elt_i, i) != 0))
		{
		  error ("vector CONSTRUCTOR with non-NULL element index");
		  debug_generic_stmt (rhs1);
		  return true;
		}
	      if (!is_gimple_val (elt_v))
		{
		  error ("vector CONSTRUCTOR element is not a GIMPLE value");
		  debug_generic_stmt (rhs1);
		  return true;
		}
	    }
	}
      else if (CONSTRUCTOR_NELTS (rhs1) != 0)
	{
	  error ("non-vector CONSTRUCTOR with elements");
	  debug_generic_stmt (rhs1);
	  return true;
	}
      return res;

    case ASSERT_EXPR:
      /* FIXME.  */
      rhs1 = fold (ASSERT_EXPR_COND (rhs1));
      if (rhs1 == boolean_false_node)
	{
	  error ("ASSERT_EXPR with an always-false condition");
	  debug_generic_stmt (rhs1);
	  return true;
	}
      break;

    case OBJ_TYPE_REF:
    case WITH_SIZE_EXPR:
      /* FIXME.  */
      return res;

    default:;
    }

  return res;
}

/* Verify the contents of a GIMPLE_ASSIGN STMT.  Returns true when there
   is a problem, otherwise false.  */

static bool
verify_gimple_assign (gassign *stmt)
{
  switch (gimple_assign_rhs_class (stmt))
    {
    case GIMPLE_SINGLE_RHS:
      return verify_gimple_assign_single (stmt);

    case GIMPLE_UNARY_RHS:
      return verify_gimple_assign_unary (stmt);

    case GIMPLE_BINARY_RHS:
      return verify_gimple_assign_binary (stmt);

    case GIMPLE_TERNARY_RHS:
      return verify_gimple_assign_ternary (stmt);

    default:
      gcc_unreachable ();
    }
}

/* Verify the contents of a GIMPLE_RETURN STMT.  Returns true when there
   is a problem, otherwise false.  */

static bool
verify_gimple_return (greturn *stmt)
{
  tree op = gimple_return_retval (stmt);
  tree restype = TREE_TYPE (TREE_TYPE (cfun->decl));

  /* We cannot test for present return values as we do not fix up missing
     return values from the original source.  */
  if (op == NULL)
    return false;

  if (!is_gimple_val (op)
      && TREE_CODE (op) != RESULT_DECL)
    {
      error ("invalid operand in return statement");
      debug_generic_stmt (op);
      return true;
    }

  if ((TREE_CODE (op) == RESULT_DECL
       && DECL_BY_REFERENCE (op))
      || (TREE_CODE (op) == SSA_NAME
	  && SSA_NAME_VAR (op)
	  && TREE_CODE (SSA_NAME_VAR (op)) == RESULT_DECL
	  && DECL_BY_REFERENCE (SSA_NAME_VAR (op))))
    op = TREE_TYPE (op);

  if (!useless_type_conversion_p (restype, TREE_TYPE (op)))
    {
      error ("invalid conversion in return statement");
      debug_generic_stmt (restype);
      debug_generic_stmt (TREE_TYPE (op));
      return true;
    }

  return false;
}


/* Verify the contents of a GIMPLE_GOTO STMT.  Returns true when there
   is a problem, otherwise false.  */

static bool
verify_gimple_goto (ggoto *stmt)
{
  tree dest = gimple_goto_dest (stmt);

  /* ???  We have two canonical forms of direct goto destinations, a
     bare LABEL_DECL and an ADDR_EXPR of a LABEL_DECL.  */
  if (TREE_CODE (dest) != LABEL_DECL
      && (!is_gimple_val (dest)
	  || !POINTER_TYPE_P (TREE_TYPE (dest))))
    {
      error ("goto destination is neither a label nor a pointer");
      return true;
    }

  return false;
}

/* Verify the contents of a GIMPLE_SWITCH STMT.  Returns true when there
   is a problem, otherwise false.  */

static bool
verify_gimple_switch (gswitch *stmt)
{
  unsigned int i, n;
  tree elt, prev_upper_bound = NULL_TREE;
  tree index_type, elt_type = NULL_TREE;

  if (!is_gimple_val (gimple_switch_index (stmt)))
    {
      error ("invalid operand to switch statement");
      debug_generic_stmt (gimple_switch_index (stmt));
      return true;
    }

  index_type = TREE_TYPE (gimple_switch_index (stmt));
  if (! INTEGRAL_TYPE_P (index_type))
    {
      error ("non-integral type switch statement");
      debug_generic_expr (index_type);
      return true;
    }

  elt = gimple_switch_label (stmt, 0);
  if (CASE_LOW (elt) != NULL_TREE
      || CASE_HIGH (elt) != NULL_TREE
      || CASE_CHAIN (elt) != NULL_TREE)
    {
      error ("invalid default case label in switch statement");
      debug_generic_expr (elt);
      return true;
    }

  n = gimple_switch_num_labels (stmt);
  for (i = 1; i < n; i++)
    {
      elt = gimple_switch_label (stmt, i);

      if (CASE_CHAIN (elt))
	{
	  error ("invalid CASE_CHAIN");
	  debug_generic_expr (elt);
	  return true;
	}
      if (! CASE_LOW (elt))
	{
	  error ("invalid case label in switch statement");
	  debug_generic_expr (elt);
	  return true;
	}
      if (CASE_HIGH (elt)
	  && ! tree_int_cst_lt (CASE_LOW (elt), CASE_HIGH (elt)))
	{
	  error ("invalid case range in switch statement");
	  debug_generic_expr (elt);
	  return true;
	}

      if (elt_type)
	{
	  if (TREE_TYPE (CASE_LOW (elt)) != elt_type
	      || (CASE_HIGH (elt) && TREE_TYPE (CASE_HIGH (elt)) != elt_type))
	    {
	      error ("type mismatch for case label in switch statement");
	      debug_generic_expr (elt);
	      return true;
	    }
	}
      else
	{
	  elt_type = TREE_TYPE (CASE_LOW (elt));
	  if (TYPE_PRECISION (index_type) < TYPE_PRECISION (elt_type))
	    {
	      error ("type precision mismatch in switch statement");
	      return true;
	    }
	}

      if (prev_upper_bound)
	{
	  if (! tree_int_cst_lt (prev_upper_bound, CASE_LOW (elt)))
	    {
	      error ("case labels not sorted in switch statement");
	      return true;
	    }
	}

      prev_upper_bound = CASE_HIGH (elt);
      if (! prev_upper_bound)
	prev_upper_bound = CASE_LOW (elt);
    }

  return false;
}

/* Verify a gimple debug statement STMT.
   Returns true if anything is wrong.  */

static bool
verify_gimple_debug (gimple *stmt ATTRIBUTE_UNUSED)
{
  /* There isn't much that could be wrong in a gimple debug stmt.  A
     gimple debug bind stmt, for example, maps a tree, that's usually
     a VAR_DECL or a PARM_DECL, but that could also be some scalarized
     component or member of an aggregate type, to another tree, that
     can be an arbitrary expression.  These stmts expand into debug
     insns, and are converted to debug notes by var-tracking.c.  */
  return false;
}

/* Verify a gimple label statement STMT.
   Returns true if anything is wrong.  */

static bool
verify_gimple_label (glabel *stmt)
{
  tree decl = gimple_label_label (stmt);
  int uid;
  bool err = false;

  if (TREE_CODE (decl) != LABEL_DECL)
    return true;
  if (!DECL_NONLOCAL (decl) && !FORCED_LABEL (decl)
      && DECL_CONTEXT (decl) != current_function_decl)
    {
      error ("label%'s context is not the current function decl");
      err |= true;
    }

  uid = LABEL_DECL_UID (decl);
  if (cfun->cfg
      && (uid == -1
	  || (*label_to_block_map_for_fn (cfun))[uid] != gimple_bb (stmt)))
    {
      error ("incorrect entry in label_to_block_map");
      err |= true;
    }

  uid = EH_LANDING_PAD_NR (decl);
  if (uid)
    {
      eh_landing_pad lp = get_eh_landing_pad_from_number (uid);
      if (decl != lp->post_landing_pad)
	{
	  error ("incorrect setting of landing pad number");
	  err |= true;
	}
    }

  return err;
}

/* Verify a gimple cond statement STMT.
   Returns true if anything is wrong.  */

static bool
verify_gimple_cond (gcond *stmt)
{
  if (TREE_CODE_CLASS (gimple_cond_code (stmt)) != tcc_comparison)
    {
      error ("invalid comparison code in gimple cond");
      return true;
    }
  if (!(!gimple_cond_true_label (stmt)
	|| TREE_CODE (gimple_cond_true_label (stmt)) == LABEL_DECL)
      || !(!gimple_cond_false_label (stmt)
	   || TREE_CODE (gimple_cond_false_label (stmt)) == LABEL_DECL))
    {
      error ("invalid labels in gimple cond");
      return true;
    }

  return verify_gimple_comparison (boolean_type_node,
				   gimple_cond_lhs (stmt),
				   gimple_cond_rhs (stmt),
				   gimple_cond_code (stmt));
}

/* Verify the GIMPLE statement STMT.  Returns true if there is an
   error, otherwise false.  */

static bool
verify_gimple_stmt (gimple *stmt)
{
  switch (gimple_code (stmt))
    {
    case GIMPLE_ASSIGN:
      return verify_gimple_assign (as_a <gassign *> (stmt));

    case GIMPLE_LABEL:
      return verify_gimple_label (as_a <glabel *> (stmt));

    case GIMPLE_CALL:
      return verify_gimple_call (as_a <gcall *> (stmt));

    case GIMPLE_COND:
      return verify_gimple_cond (as_a <gcond *> (stmt));

    case GIMPLE_GOTO:
      return verify_gimple_goto (as_a <ggoto *> (stmt));

    case GIMPLE_SWITCH:
      return verify_gimple_switch (as_a <gswitch *> (stmt));

    case GIMPLE_RETURN:
      return verify_gimple_return (as_a <greturn *> (stmt));

    case GIMPLE_ASM:
      return false;

    case GIMPLE_TRANSACTION:
      return verify_gimple_transaction (as_a <gtransaction *> (stmt));

    /* Tuples that do not have tree operands.  */
    case GIMPLE_NOP:
    case GIMPLE_PREDICT:
    case GIMPLE_RESX:
    case GIMPLE_EH_DISPATCH:
    case GIMPLE_EH_MUST_NOT_THROW:
      return false;

    CASE_GIMPLE_OMP:
      /* OpenMP directives are validated by the FE and never operated
	 on by the optimizers.  Furthermore, GIMPLE_OMP_FOR may contain
	 non-gimple expressions when the main index variable has had
	 its address taken.  This does not affect the loop itself
	 because the header of an GIMPLE_OMP_FOR is merely used to determine
	 how to setup the parallel iteration.  */
      return false;

    case GIMPLE_DEBUG:
      return verify_gimple_debug (stmt);

    default:
      gcc_unreachable ();
    }
}

/* Verify the contents of a GIMPLE_PHI.  Returns true if there is a problem,
   and false otherwise.  */

static bool
verify_gimple_phi (gphi *phi)
{
  bool err = false;
  unsigned i;
  tree phi_result = gimple_phi_result (phi);
  bool virtual_p;

  if (!phi_result)
    {
      error ("invalid PHI result");
      return true;
    }

  virtual_p = virtual_operand_p (phi_result);
  if (TREE_CODE (phi_result) != SSA_NAME
      || (virtual_p
	  && SSA_NAME_VAR (phi_result) != gimple_vop (cfun)))
    {
      error ("invalid PHI result");
      err = true;
    }

  for (i = 0; i < gimple_phi_num_args (phi); i++)
    {
      tree t = gimple_phi_arg_def (phi, i);

      if (!t)
	{
	  error ("missing PHI def");
	  err |= true;
	  continue;
	}
      /* Addressable variables do have SSA_NAMEs but they
	 are not considered gimple values.  */
      else if ((TREE_CODE (t) == SSA_NAME
		&& virtual_p != virtual_operand_p (t))
	       || (virtual_p
		   && (TREE_CODE (t) != SSA_NAME
		       || SSA_NAME_VAR (t) != gimple_vop (cfun)))
	       || (!virtual_p
		   && !is_gimple_val (t)))
	{
	  error ("invalid PHI argument");
	  debug_generic_expr (t);
	  err |= true;
	}
#ifdef ENABLE_TYPES_CHECKING
      if (!useless_type_conversion_p (TREE_TYPE (phi_result), TREE_TYPE (t)))
	{
	  error ("incompatible types in PHI argument %u", i);
	  debug_generic_stmt (TREE_TYPE (phi_result));
	  debug_generic_stmt (TREE_TYPE (t));
	  err |= true;
	}
#endif
    }

  return err;
}

/* Verify the GIMPLE statements inside the sequence STMTS.  */

static bool
verify_gimple_in_seq_2 (gimple_seq stmts)
{
  gimple_stmt_iterator ittr;
  bool err = false;

  for (ittr = gsi_start (stmts); !gsi_end_p (ittr); gsi_next (&ittr))
    {
      gimple *stmt = gsi_stmt (ittr);

      switch (gimple_code (stmt))
        {
	case GIMPLE_BIND:
	  err |= verify_gimple_in_seq_2 (
                   gimple_bind_body (as_a <gbind *> (stmt)));
	  break;

	case GIMPLE_TRY:
	  err |= verify_gimple_in_seq_2 (gimple_try_eval (stmt));
	  err |= verify_gimple_in_seq_2 (gimple_try_cleanup (stmt));
	  break;

	case GIMPLE_EH_FILTER:
	  err |= verify_gimple_in_seq_2 (gimple_eh_filter_failure (stmt));
	  break;

	case GIMPLE_EH_ELSE:
	  {
	    geh_else *eh_else = as_a <geh_else *> (stmt);
	    err |= verify_gimple_in_seq_2 (gimple_eh_else_n_body (eh_else));
	    err |= verify_gimple_in_seq_2 (gimple_eh_else_e_body (eh_else));
	  }
	  break;

	case GIMPLE_CATCH:
	  err |= verify_gimple_in_seq_2 (gimple_catch_handler (
					   as_a <gcatch *> (stmt)));
	  break;

	case GIMPLE_TRANSACTION:
	  err |= verify_gimple_transaction (as_a <gtransaction *> (stmt));
	  break;

	default:
	  {
	    bool err2 = verify_gimple_stmt (stmt);
	    if (err2)
	      debug_gimple_stmt (stmt);
	    err |= err2;
	  }
	}
    }

  return err;
}

/* Verify the contents of a GIMPLE_TRANSACTION.  Returns true if there
   is a problem, otherwise false.  */

static bool
verify_gimple_transaction (gtransaction *stmt)
{
  tree lab;

  lab = gimple_transaction_label_norm (stmt);
  if (lab != NULL && TREE_CODE (lab) != LABEL_DECL)
    return true;
  lab = gimple_transaction_label_uninst (stmt);
  if (lab != NULL && TREE_CODE (lab) != LABEL_DECL)
    return true;
  lab = gimple_transaction_label_over (stmt);
  if (lab != NULL && TREE_CODE (lab) != LABEL_DECL)
    return true;

  return verify_gimple_in_seq_2 (gimple_transaction_body (stmt));
}


/* Verify the GIMPLE statements inside the statement list STMTS.  */

DEBUG_FUNCTION void
verify_gimple_in_seq (gimple_seq stmts)
{
  timevar_push (TV_TREE_STMT_VERIFY);
  if (verify_gimple_in_seq_2 (stmts))
    internal_error ("verify_gimple failed");
  timevar_pop (TV_TREE_STMT_VERIFY);
}

/* Return true when the T can be shared.  */

static bool
tree_node_can_be_shared (tree t)
{
  if (IS_TYPE_OR_DECL_P (t)
      || TREE_CODE (t) == SSA_NAME
      || TREE_CODE (t) == IDENTIFIER_NODE
      || TREE_CODE (t) == CASE_LABEL_EXPR
      || is_gimple_min_invariant (t))
    return true;

  if (t == error_mark_node)
    return true;

  return false;
}

/* Called via walk_tree.  Verify tree sharing.  */

static tree
verify_node_sharing_1 (tree *tp, int *walk_subtrees, void *data)
{
  hash_set<void *> *visited = (hash_set<void *> *) data;

  if (tree_node_can_be_shared (*tp))
    {
      *walk_subtrees = false;
      return NULL;
    }

  if (visited->add (*tp))
    return *tp;

  return NULL;
}

/* Called via walk_gimple_stmt.  Verify tree sharing.  */

static tree
verify_node_sharing (tree *tp, int *walk_subtrees, void *data)
{
  struct walk_stmt_info *wi = (struct walk_stmt_info *) data;
  return verify_node_sharing_1 (tp, walk_subtrees, wi->info);
}

static bool eh_error_found;
bool
verify_eh_throw_stmt_node (gimple *const &stmt, const int &,
			   hash_set<gimple *> *visited)
{
  if (!visited->contains (stmt))
    {
      error ("dead STMT in EH table");
      debug_gimple_stmt (stmt);
      eh_error_found = true;
    }
  return true;
}

/* Verify if the location LOCs block is in BLOCKS.  */

static bool
verify_location (hash_set<tree> *blocks, location_t loc)
{
  tree block = LOCATION_BLOCK (loc);
  if (block != NULL_TREE
      && !blocks->contains (block))
    {
      error ("location references block not in block tree");
      return true;
    }
  if (block != NULL_TREE)
    return verify_location (blocks, BLOCK_SOURCE_LOCATION (block));
  return false;
}

/* Called via walk_tree.  Verify that expressions have no blocks.  */

static tree
verify_expr_no_block (tree *tp, int *walk_subtrees, void *)
{
  if (!EXPR_P (*tp))
    {
      *walk_subtrees = false;
      return NULL;
    }

  location_t loc = EXPR_LOCATION (*tp);
  if (LOCATION_BLOCK (loc) != NULL)
    return *tp;

  return NULL;
}

/* Called via walk_tree.  Verify locations of expressions.  */

static tree
verify_expr_location_1 (tree *tp, int *walk_subtrees, void *data)
{
  hash_set<tree> *blocks = (hash_set<tree> *) data;
  tree t = *tp;

  /* ???  This doesn't really belong here but there's no good place to
     stick this remainder of old verify_expr.  */
  /* ???  This barfs on debug stmts which contain binds to vars with
     different function context.  */
#if 0
  if (VAR_P (t)
      || TREE_CODE (t) == PARM_DECL
      || TREE_CODE (t) == RESULT_DECL)
    {
      tree context = decl_function_context (t);
      if (context != cfun->decl
	  && !SCOPE_FILE_SCOPE_P (context)
	  && !TREE_STATIC (t)
	  && !DECL_EXTERNAL (t))
	{
	  error ("local declaration from a different function");
	  return t;
	}
    }
#endif

  if (VAR_P (t) && DECL_HAS_DEBUG_EXPR_P (t))
    {
      tree x = DECL_DEBUG_EXPR (t);
      tree addr = walk_tree (&x, verify_expr_no_block, NULL, NULL);
      if (addr)
	return addr;
    }
  if ((VAR_P (t)
       || TREE_CODE (t) == PARM_DECL
       || TREE_CODE (t) == RESULT_DECL)
      && DECL_HAS_VALUE_EXPR_P (t))
    {
      tree x = DECL_VALUE_EXPR (t);
      tree addr = walk_tree (&x, verify_expr_no_block, NULL, NULL);
      if (addr)
	return addr;
    }

  if (!EXPR_P (t))
    {
      *walk_subtrees = false;
      return NULL;
    }

  location_t loc = EXPR_LOCATION (t);
  if (verify_location (blocks, loc))
    return t;

  return NULL;
}

/* Called via walk_gimple_op.  Verify locations of expressions.  */

static tree
verify_expr_location (tree *tp, int *walk_subtrees, void *data)
{
  struct walk_stmt_info *wi = (struct walk_stmt_info *) data;
  return verify_expr_location_1 (tp, walk_subtrees, wi->info);
}

/* Insert all subblocks of BLOCK into BLOCKS and recurse.  */

static void
collect_subblocks (hash_set<tree> *blocks, tree block)
{
  tree t;
  for (t = BLOCK_SUBBLOCKS (block); t; t = BLOCK_CHAIN (t))
    {
      blocks->add (t);
      collect_subblocks (blocks, t);
    }
}

/* Verify the GIMPLE statements in the CFG of FN.  */

DEBUG_FUNCTION void
verify_gimple_in_cfg (struct function *fn, bool verify_nothrow)
{
  basic_block bb;
  bool err = false;

  timevar_push (TV_TREE_STMT_VERIFY);
  hash_set<void *> visited;
  hash_set<gimple *> visited_throwing_stmts;

  /* Collect all BLOCKs referenced by the BLOCK tree of FN.  */
  hash_set<tree> blocks;
  if (DECL_INITIAL (fn->decl))
    {
      blocks.add (DECL_INITIAL (fn->decl));
      collect_subblocks (&blocks, DECL_INITIAL (fn->decl));
    }

  FOR_EACH_BB_FN (bb, fn)
    {
      gimple_stmt_iterator gsi;
      edge_iterator ei;
      edge e;

      for (gphi_iterator gpi = gsi_start_phis (bb);
	   !gsi_end_p (gpi);
	   gsi_next (&gpi))
	{
	  gphi *phi = gpi.phi ();
	  bool err2 = false;
	  unsigned i;

	  if (gimple_bb (phi) != bb)
	    {
	      error ("gimple_bb (phi) is set to a wrong basic block");
	      err2 = true;
	    }

	  err2 |= verify_gimple_phi (phi);

	  /* Only PHI arguments have locations.  */
	  if (gimple_location (phi) != UNKNOWN_LOCATION)
	    {
	      error ("PHI node with location");
	      err2 = true;
	    }

	  for (i = 0; i < gimple_phi_num_args (phi); i++)
	    {
	      tree arg = gimple_phi_arg_def (phi, i);
	      tree addr = walk_tree (&arg, verify_node_sharing_1,
				     &visited, NULL);
	      if (addr)
		{
		  error ("incorrect sharing of tree nodes");
		  debug_generic_expr (addr);
		  err2 |= true;
		}
	      location_t loc = gimple_phi_arg_location (phi, i);
	      if (virtual_operand_p (gimple_phi_result (phi))
		  && loc != UNKNOWN_LOCATION)
		{
		  error ("virtual PHI with argument locations");
		  err2 = true;
		}
	      addr = walk_tree (&arg, verify_expr_location_1, &blocks, NULL);
	      if (addr)
		{
		  debug_generic_expr (addr);
		  err2 = true;
		}
	      err2 |= verify_location (&blocks, loc);
	    }

	  if (err2)
	    debug_gimple_stmt (phi);
	  err |= err2;
	}

      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  bool err2 = false;
	  struct walk_stmt_info wi;
	  tree addr;
	  int lp_nr;

	  if (gimple_bb (stmt) != bb)
	    {
	      error ("gimple_bb (stmt) is set to a wrong basic block");
	      err2 = true;
	    }

	  err2 |= verify_gimple_stmt (stmt);
	  err2 |= verify_location (&blocks, gimple_location (stmt));

	  memset (&wi, 0, sizeof (wi));
	  wi.info = (void *) &visited;
	  addr = walk_gimple_op (stmt, verify_node_sharing, &wi);
	  if (addr)
	    {
	      error ("incorrect sharing of tree nodes");
	      debug_generic_expr (addr);
	      err2 |= true;
	    }

	  memset (&wi, 0, sizeof (wi));
	  wi.info = (void *) &blocks;
	  addr = walk_gimple_op (stmt, verify_expr_location, &wi);
	  if (addr)
	    {
	      debug_generic_expr (addr);
	      err2 |= true;
	    }

	  /* If the statement is marked as part of an EH region, then it is
	     expected that the statement could throw.  Verify that when we
	     have optimizations that simplify statements such that we prove
	     that they cannot throw, that we update other data structures
	     to match.  */
	  lp_nr = lookup_stmt_eh_lp (stmt);
	  if (lp_nr != 0)
	    visited_throwing_stmts.add (stmt);
	  if (lp_nr > 0)
	    {
	      if (!stmt_could_throw_p (cfun, stmt))
		{
		  if (verify_nothrow)
		    {
		      error ("statement marked for throw, but doesn%'t");
		      err2 |= true;
		    }
		}
	      else if (!gsi_one_before_end_p (gsi))
		{
		  error ("statement marked for throw in middle of block");
		  err2 |= true;
		}
	    }

	  if (err2)
	    debug_gimple_stmt (stmt);
	  err |= err2;
	}

      FOR_EACH_EDGE (e, ei, bb->succs)
	if (e->goto_locus != UNKNOWN_LOCATION)
	  err |= verify_location (&blocks, e->goto_locus);
    }

  hash_map<gimple *, int> *eh_table = get_eh_throw_stmt_table (cfun);
  eh_error_found = false;
  if (eh_table)
    eh_table->traverse<hash_set<gimple *> *, verify_eh_throw_stmt_node>
      (&visited_throwing_stmts);

  if (err || eh_error_found)
    internal_error ("verify_gimple failed");

  verify_histograms ();
  timevar_pop (TV_TREE_STMT_VERIFY);
}


/* Verifies that the flow information is OK.  */

static int
gimple_verify_flow_info (void)
{
  int err = 0;
  basic_block bb;
  gimple_stmt_iterator gsi;
  gimple *stmt;
  edge e;
  edge_iterator ei;

  if (ENTRY_BLOCK_PTR_FOR_FN (cfun)->il.gimple.seq
      || ENTRY_BLOCK_PTR_FOR_FN (cfun)->il.gimple.phi_nodes)
    {
      error ("ENTRY_BLOCK has IL associated with it");
      err = 1;
    }

  if (EXIT_BLOCK_PTR_FOR_FN (cfun)->il.gimple.seq
      || EXIT_BLOCK_PTR_FOR_FN (cfun)->il.gimple.phi_nodes)
    {
      error ("EXIT_BLOCK has IL associated with it");
      err = 1;
    }

  FOR_EACH_EDGE (e, ei, EXIT_BLOCK_PTR_FOR_FN (cfun)->preds)
    if (e->flags & EDGE_FALLTHRU)
      {
	error ("fallthru to exit from bb %d", e->src->index);
	err = 1;
      }

  FOR_EACH_BB_FN (bb, cfun)
    {
      bool found_ctrl_stmt = false;

      stmt = NULL;

      /* Skip labels on the start of basic block.  */
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  tree label;
	  gimple *prev_stmt = stmt;

	  stmt = gsi_stmt (gsi);

	  if (gimple_code (stmt) != GIMPLE_LABEL)
	    break;

	  label = gimple_label_label (as_a <glabel *> (stmt));
	  if (prev_stmt && DECL_NONLOCAL (label))
	    {
	      error ("nonlocal label ");
	      print_generic_expr (stderr, label);
	      fprintf (stderr, " is not first in a sequence of labels in bb %d",
		       bb->index);
	      err = 1;
	    }

	  if (prev_stmt && EH_LANDING_PAD_NR (label) != 0)
	    {
	      error ("EH landing pad label ");
	      print_generic_expr (stderr, label);
	      fprintf (stderr, " is not first in a sequence of labels in bb %d",
		       bb->index);
	      err = 1;
	    }

	  if (label_to_block (cfun, label) != bb)
	    {
	      error ("label ");
	      print_generic_expr (stderr, label);
	      fprintf (stderr, " to block does not match in bb %d",
		       bb->index);
	      err = 1;
	    }

	  if (decl_function_context (label) != current_function_decl)
	    {
	      error ("label ");
	      print_generic_expr (stderr, label);
	      fprintf (stderr, " has incorrect context in bb %d",
		       bb->index);
	      err = 1;
	    }
	}

      /* Verify that body of basic block BB is free of control flow.  */
      for (; !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);

	  if (found_ctrl_stmt)
	    {
	      error ("control flow in the middle of basic block %d",
		     bb->index);
	      err = 1;
	    }

	  if (stmt_ends_bb_p (stmt))
	    found_ctrl_stmt = true;

	  if (glabel *label_stmt = dyn_cast <glabel *> (stmt))
	    {
	      error ("label ");
	      print_generic_expr (stderr, gimple_label_label (label_stmt));
	      fprintf (stderr, " in the middle of basic block %d", bb->index);
	      err = 1;
	    }
	}

      gsi = gsi_last_nondebug_bb (bb);
      if (gsi_end_p (gsi))
	continue;

      stmt = gsi_stmt (gsi);

      if (gimple_code (stmt) == GIMPLE_LABEL)
	continue;

      err |= verify_eh_edges (stmt);

      if (is_ctrl_stmt (stmt))
	{
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    if (e->flags & EDGE_FALLTHRU)
	      {
		error ("fallthru edge after a control statement in bb %d",
		       bb->index);
		err = 1;
	      }
	}

      if (gimple_code (stmt) != GIMPLE_COND)
	{
	  /* Verify that there are no edges with EDGE_TRUE/FALSE_FLAG set
	     after anything else but if statement.  */
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    if (e->flags & (EDGE_TRUE_VALUE | EDGE_FALSE_VALUE))
	      {
		error ("true/false edge after a non-GIMPLE_COND in bb %d",
		       bb->index);
		err = 1;
	      }
	}

      switch (gimple_code (stmt))
	{
	case GIMPLE_COND:
	  {
	    edge true_edge;
	    edge false_edge;

	    extract_true_false_edges_from_block (bb, &true_edge, &false_edge);

	    if (!true_edge
		|| !false_edge
		|| !(true_edge->flags & EDGE_TRUE_VALUE)
		|| !(false_edge->flags & EDGE_FALSE_VALUE)
		|| (true_edge->flags & (EDGE_FALLTHRU | EDGE_ABNORMAL))
		|| (false_edge->flags & (EDGE_FALLTHRU | EDGE_ABNORMAL))
		|| EDGE_COUNT (bb->succs) >= 3)
	      {
		error ("wrong outgoing edge flags at end of bb %d",
		       bb->index);
		err = 1;
	      }
	  }
	  break;

	case GIMPLE_GOTO:
	  if (simple_goto_p (stmt))
	    {
	      error ("explicit goto at end of bb %d", bb->index);
	      err = 1;
	    }
	  else
	    {
	      /* FIXME.  We should double check that the labels in the
		 destination blocks have their address taken.  */
	      FOR_EACH_EDGE (e, ei, bb->succs)
		if ((e->flags & (EDGE_FALLTHRU | EDGE_TRUE_VALUE
				 | EDGE_FALSE_VALUE))
		    || !(e->flags & EDGE_ABNORMAL))
		  {
		    error ("wrong outgoing edge flags at end of bb %d",
			   bb->index);
		    err = 1;
		  }
	    }
	  break;

	case GIMPLE_CALL:
	  if (!gimple_call_builtin_p (stmt, BUILT_IN_RETURN))
	    break;
	  /* fallthru */
	case GIMPLE_RETURN:
	  if (!single_succ_p (bb)
	      || (single_succ_edge (bb)->flags
		  & (EDGE_FALLTHRU | EDGE_ABNORMAL
		     | EDGE_TRUE_VALUE | EDGE_FALSE_VALUE)))
	    {
	      error ("wrong outgoing edge flags at end of bb %d", bb->index);
	      err = 1;
	    }
	  if (single_succ (bb) != EXIT_BLOCK_PTR_FOR_FN (cfun))
	    {
	      error ("return edge does not point to exit in bb %d",
		     bb->index);
	      err = 1;
	    }
	  break;

	case GIMPLE_SWITCH:
	  {
	    gswitch *switch_stmt = as_a <gswitch *> (stmt);
	    tree prev;
	    edge e;
	    size_t i, n;

	    n = gimple_switch_num_labels (switch_stmt);

	    /* Mark all the destination basic blocks.  */
	    for (i = 0; i < n; ++i)
	      {
		basic_block label_bb = gimple_switch_label_bb (cfun, switch_stmt, i);
		gcc_assert (!label_bb->aux || label_bb->aux == (void *)1);
		label_bb->aux = (void *)1;
	      }

	    /* Verify that the case labels are sorted.  */
	    prev = gimple_switch_label (switch_stmt, 0);
	    for (i = 1; i < n; ++i)
	      {
		tree c = gimple_switch_label (switch_stmt, i);
		if (!CASE_LOW (c))
		  {
		    error ("found default case not at the start of "
			   "case vector");
		    err = 1;
		    continue;
		  }
		if (CASE_LOW (prev)
		    && !tree_int_cst_lt (CASE_LOW (prev), CASE_LOW (c)))
		  {
		    error ("case labels not sorted: ");
		    print_generic_expr (stderr, prev);
		    fprintf (stderr," is greater than ");
		    print_generic_expr (stderr, c);
		    fprintf (stderr," but comes before it.\n");
		    err = 1;
		  }
		prev = c;
	      }
	    /* VRP will remove the default case if it can prove it will
	       never be executed.  So do not verify there always exists
	       a default case here.  */

	    FOR_EACH_EDGE (e, ei, bb->succs)
	      {
		if (!e->dest->aux)
		  {
		    error ("extra outgoing edge %d->%d",
			   bb->index, e->dest->index);
		    err = 1;
		  }

		e->dest->aux = (void *)2;
		if ((e->flags & (EDGE_FALLTHRU | EDGE_ABNORMAL
				 | EDGE_TRUE_VALUE | EDGE_FALSE_VALUE)))
		  {
		    error ("wrong outgoing edge flags at end of bb %d",
			   bb->index);
		    err = 1;
		  }
	      }

	    /* Check that we have all of them.  */
	    for (i = 0; i < n; ++i)
	      {
		basic_block label_bb = gimple_switch_label_bb (cfun,
							       switch_stmt, i);

		if (label_bb->aux != (void *)2)
		  {
		    error ("missing edge %i->%i", bb->index, label_bb->index);
		    err = 1;
		  }
	      }

	    FOR_EACH_EDGE (e, ei, bb->succs)
	      e->dest->aux = (void *)0;
	  }
	  break;

	case GIMPLE_EH_DISPATCH:
	  err |= verify_eh_dispatch_edge (as_a <geh_dispatch *> (stmt));
	  break;

	default:
	  break;
	}
    }

  if (dom_info_state (CDI_DOMINATORS) >= DOM_NO_FAST_QUERY)
    verify_dominators (CDI_DOMINATORS);

  return err;
}


/* Updates phi nodes after creating a forwarder block joined
   by edge FALLTHRU.  */

static void
gimple_make_forwarder_block (edge fallthru)
{
  edge e;
  edge_iterator ei;
  basic_block dummy, bb;
  tree var;
  gphi_iterator gsi;

  dummy = fallthru->src;
  bb = fallthru->dest;

  if (single_pred_p (bb))
    return;

  /* If we redirected a branch we must create new PHI nodes at the
     start of BB.  */
  for (gsi = gsi_start_phis (dummy); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gphi *phi, *new_phi;

      phi = gsi.phi ();
      var = gimple_phi_result (phi);
      new_phi = create_phi_node (var, bb);
      gimple_phi_set_result (phi, copy_ssa_name (var, phi));
      add_phi_arg (new_phi, gimple_phi_result (phi), fallthru,
		   UNKNOWN_LOCATION);
    }

  /* Add the arguments we have stored on edges.  */
  FOR_EACH_EDGE (e, ei, bb->preds)
    {
      if (e == fallthru)
	continue;

      flush_pending_stmts (e);
    }
}


/* Return a non-special label in the head of basic block BLOCK.
   Create one if it doesn't exist.  */

tree
gimple_block_label (basic_block bb)
{
  gimple_stmt_iterator i, s = gsi_start_bb (bb);
  bool first = true;
  tree label;
  glabel *stmt;

  for (i = s; !gsi_end_p (i); first = false, gsi_next (&i))
    {
      stmt = dyn_cast <glabel *> (gsi_stmt (i));
      if (!stmt)
	break;
      label = gimple_label_label (stmt);
      if (!DECL_NONLOCAL (label))
	{
	  if (!first)
	    gsi_move_before (&i, &s);
	  return label;
	}
    }

  label = create_artificial_label (UNKNOWN_LOCATION);
  stmt = gimple_build_label (label);
  gsi_insert_before (&s, stmt, GSI_NEW_STMT);
  return label;
}


/* Attempt to perform edge redirection by replacing a possibly complex
   jump instruction by a goto or by removing the jump completely.
   This can apply only if all edges now point to the same block.  The
   parameters and return values are equivalent to
   redirect_edge_and_branch.  */

static edge
gimple_try_redirect_by_replacing_jump (edge e, basic_block target)
{
  basic_block src = e->src;
  gimple_stmt_iterator i;
  gimple *stmt;

  /* We can replace or remove a complex jump only when we have exactly
     two edges.  */
  if (EDGE_COUNT (src->succs) != 2
      /* Verify that all targets will be TARGET.  Specifically, the
	 edge that is not E must also go to TARGET.  */
      || EDGE_SUCC (src, EDGE_SUCC (src, 0) == e)->dest != target)
    return NULL;

  i = gsi_last_bb (src);
  if (gsi_end_p (i))
    return NULL;

  stmt = gsi_stmt (i);

  if (gimple_code (stmt) == GIMPLE_COND || gimple_code (stmt) == GIMPLE_SWITCH)
    {
      gsi_remove (&i, true);
      e = ssa_redirect_edge (e, target);
      e->flags = EDGE_FALLTHRU;
      return e;
    }

  return NULL;
}


/* Redirect E to DEST.  Return NULL on failure.  Otherwise, return the
   edge representing the redirected branch.  */

static edge
gimple_redirect_edge_and_branch (edge e, basic_block dest)
{
  basic_block bb = e->src;
  gimple_stmt_iterator gsi;
  edge ret;
  gimple *stmt;

  if (e->flags & EDGE_ABNORMAL)
    return NULL;

  if (e->dest == dest)
    return NULL;

  if (e->flags & EDGE_EH)
    return redirect_eh_edge (e, dest);

  if (e->src != ENTRY_BLOCK_PTR_FOR_FN (cfun))
    {
      ret = gimple_try_redirect_by_replacing_jump (e, dest);
      if (ret)
	return ret;
    }

  gsi = gsi_last_nondebug_bb (bb);
  stmt = gsi_end_p (gsi) ? NULL : gsi_stmt (gsi);

  switch (stmt ? gimple_code (stmt) : GIMPLE_ERROR_MARK)
    {
    case GIMPLE_COND:
      /* For COND_EXPR, we only need to redirect the edge.  */
      break;

    case GIMPLE_GOTO:
      /* No non-abnormal edges should lead from a non-simple goto, and
	 simple ones should be represented implicitly.  */
      gcc_unreachable ();

    case GIMPLE_SWITCH:
      {
	gswitch *switch_stmt = as_a <gswitch *> (stmt);
	tree label = gimple_block_label (dest);
        tree cases = get_cases_for_edge (e, switch_stmt);

	/* If we have a list of cases associated with E, then use it
	   as it's a lot faster than walking the entire case vector.  */
	if (cases)
	  {
	    edge e2 = find_edge (e->src, dest);
	    tree last, first;

	    first = cases;
	    while (cases)
	      {
		last = cases;
		CASE_LABEL (cases) = label;
		cases = CASE_CHAIN (cases);
	      }

	    /* If there was already an edge in the CFG, then we need
	       to move all the cases associated with E to E2.  */
	    if (e2)
	      {
		tree cases2 = get_cases_for_edge (e2, switch_stmt);

		CASE_CHAIN (last) = CASE_CHAIN (cases2);
		CASE_CHAIN (cases2) = first;
	      }
	    bitmap_set_bit (touched_switch_bbs, gimple_bb (stmt)->index);
	  }
	else
	  {
	    size_t i, n = gimple_switch_num_labels (switch_stmt);

	    for (i = 0; i < n; i++)
	      {
		tree elt = gimple_switch_label (switch_stmt, i);
		if (label_to_block (cfun, CASE_LABEL (elt)) == e->dest)
		  CASE_LABEL (elt) = label;
	      }
	  }
      }
      break;

    case GIMPLE_ASM:
      {
	gasm *asm_stmt = as_a <gasm *> (stmt);
	int i, n = gimple_asm_nlabels (asm_stmt);
	tree label = NULL;

	for (i = 0; i < n; ++i)
	  {
	    tree cons = gimple_asm_label_op (asm_stmt, i);
	    if (label_to_block (cfun, TREE_VALUE (cons)) == e->dest)
	      {
		if (!label)
		  label = gimple_block_label (dest);
		TREE_VALUE (cons) = label;
	      }
	  }

	/* If we didn't find any label matching the former edge in the
	   asm labels, we must be redirecting the fallthrough
	   edge.  */
	gcc_assert (label || (e->flags & EDGE_FALLTHRU));
      }
      break;

    case GIMPLE_RETURN:
      gsi_remove (&gsi, true);
      e->flags |= EDGE_FALLTHRU;
      break;

    case GIMPLE_OMP_RETURN:
    case GIMPLE_OMP_CONTINUE:
    case GIMPLE_OMP_SECTIONS_SWITCH:
    case GIMPLE_OMP_FOR:
      /* The edges from OMP constructs can be simply redirected.  */
      break;

    case GIMPLE_EH_DISPATCH:
      if (!(e->flags & EDGE_FALLTHRU))
	redirect_eh_dispatch_edge (as_a <geh_dispatch *> (stmt), e, dest);
      break;

    case GIMPLE_TRANSACTION:
      if (e->flags & EDGE_TM_ABORT)
	gimple_transaction_set_label_over (as_a <gtransaction *> (stmt),
				           gimple_block_label (dest));
      else if (e->flags & EDGE_TM_UNINSTRUMENTED)
	gimple_transaction_set_label_uninst (as_a <gtransaction *> (stmt),
				             gimple_block_label (dest));
      else
	gimple_transaction_set_label_norm (as_a <gtransaction *> (stmt),
				           gimple_block_label (dest));
      break;

    default:
      /* Otherwise it must be a fallthru edge, and we don't need to
	 do anything besides redirecting it.  */
      gcc_assert (e->flags & EDGE_FALLTHRU);
      break;
    }

  /* Update/insert PHI nodes as necessary.  */

  /* Now update the edges in the CFG.  */
  e = ssa_redirect_edge (e, dest);

  return e;
}

/* Returns true if it is possible to remove edge E by redirecting
   it to the destination of the other edge from E->src.  */

static bool
gimple_can_remove_branch_p (const_edge e)
{
  if (e->flags & (EDGE_ABNORMAL | EDGE_EH))
    return false;

  return true;
}

/* Simple wrapper, as we can always redirect fallthru edges.  */

static basic_block
gimple_redirect_edge_and_branch_force (edge e, basic_block dest)
{
  e = gimple_redirect_edge_and_branch (e, dest);
  gcc_assert (e);

  return NULL;
}


/* Splits basic block BB after statement STMT (but at least after the
   labels).  If STMT is NULL, BB is split just after the labels.  */

static basic_block
gimple_split_block (basic_block bb, void *stmt)
{
  gimple_stmt_iterator gsi;
  gimple_stmt_iterator gsi_tgt;
  gimple_seq list;
  basic_block new_bb;
  edge e;
  edge_iterator ei;

  new_bb = create_empty_bb (bb);

  /* Redirect the outgoing edges.  */
  new_bb->succs = bb->succs;
  bb->succs = NULL;
  FOR_EACH_EDGE (e, ei, new_bb->succs)
    e->src = new_bb;

  /* Get a stmt iterator pointing to the first stmt to move.  */
  if (!stmt || gimple_code ((gimple *) stmt) == GIMPLE_LABEL)
    gsi = gsi_after_labels (bb);
  else
    {
      gsi = gsi_for_stmt ((gimple *) stmt);
      gsi_next (&gsi);
    }
 
  /* Move everything from GSI to the new basic block.  */
  if (gsi_end_p (gsi))
    return new_bb;

  /* Split the statement list - avoid re-creating new containers as this
     brings ugly quadratic memory consumption in the inliner.
     (We are still quadratic since we need to update stmt BB pointers,
     sadly.)  */
  gsi_split_seq_before (&gsi, &list);
  set_bb_seq (new_bb, list);
  for (gsi_tgt = gsi_start (list);
       !gsi_end_p (gsi_tgt); gsi_next (&gsi_tgt))
    gimple_set_bb (gsi_stmt (gsi_tgt), new_bb);

  return new_bb;
}


/* Moves basic block BB after block AFTER.  */

static bool
gimple_move_block_after (basic_block bb, basic_block after)
{
  if (bb->prev_bb == after)
    return true;

  unlink_block (bb);
  link_block (bb, after);

  return true;
}


/* Return TRUE if block BB has no executable statements, otherwise return
   FALSE.  */

static bool
gimple_empty_block_p (basic_block bb)
{
  /* BB must have no executable statements.  */
  gimple_stmt_iterator gsi = gsi_after_labels (bb);
  if (phi_nodes (bb))
    return false;
  while (!gsi_end_p (gsi))
    {
      gimple *stmt = gsi_stmt (gsi);
      if (is_gimple_debug (stmt))
	;
      else if (gimple_code (stmt) == GIMPLE_NOP
	       || gimple_code (stmt) == GIMPLE_PREDICT)
	;
      else
	return false;
      gsi_next (&gsi);
    }
  return true;
}


/* Split a basic block if it ends with a conditional branch and if the
   other part of the block is not empty.  */

static basic_block
gimple_split_block_before_cond_jump (basic_block bb)
{
  gimple *last, *split_point;
  gimple_stmt_iterator gsi = gsi_last_nondebug_bb (bb);
  if (gsi_end_p (gsi))
    return NULL;
  last = gsi_stmt (gsi);
  if (gimple_code (last) != GIMPLE_COND
      && gimple_code (last) != GIMPLE_SWITCH)
    return NULL;
  gsi_prev (&gsi);
  split_point = gsi_stmt (gsi);
  return split_block (bb, split_point)->dest;
}


/* Return true if basic_block can be duplicated.  */

static bool
gimple_can_duplicate_bb_p (const_basic_block bb ATTRIBUTE_UNUSED)
{
  return true;
}

/* Create a duplicate of the basic block BB.  NOTE: This does not
   preserve SSA form.  */

static basic_block
gimple_duplicate_bb (basic_block bb, copy_bb_data *id)
{
  basic_block new_bb;
  gimple_stmt_iterator gsi_tgt;

  new_bb = create_empty_bb (EXIT_BLOCK_PTR_FOR_FN (cfun)->prev_bb);

  /* Copy the PHI nodes.  We ignore PHI node arguments here because
     the incoming edges have not been setup yet.  */
  for (gphi_iterator gpi = gsi_start_phis (bb);
       !gsi_end_p (gpi);
       gsi_next (&gpi))
    {
      gphi *phi, *copy;
      phi = gpi.phi ();
      copy = create_phi_node (NULL_TREE, new_bb);
      create_new_def_for (gimple_phi_result (phi), copy,
			  gimple_phi_result_ptr (copy));
      gimple_set_uid (copy, gimple_uid (phi));
    }

  gsi_tgt = gsi_start_bb (new_bb);
  for (gimple_stmt_iterator gsi = gsi_start_bb (bb);
       !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      def_operand_p def_p;
      ssa_op_iter op_iter;
      tree lhs;
      gimple *stmt, *copy;

      stmt = gsi_stmt (gsi);
      if (gimple_code (stmt) == GIMPLE_LABEL)
	continue;

      /* Don't duplicate label debug stmts.  */
      if (gimple_debug_bind_p (stmt)
	  && TREE_CODE (gimple_debug_bind_get_var (stmt))
	     == LABEL_DECL)
	continue;

      /* Create a new copy of STMT and duplicate STMT's virtual
	 operands.  */
      copy = gimple_copy (stmt);
      gsi_insert_after (&gsi_tgt, copy, GSI_NEW_STMT);

      maybe_duplicate_eh_stmt (copy, stmt);
      gimple_duplicate_stmt_histograms (cfun, copy, cfun, stmt);

      /* When copying around a stmt writing into a local non-user
	 aggregate, make sure it won't share stack slot with other
	 vars.  */
      lhs = gimple_get_lhs (stmt);
      if (lhs && TREE_CODE (lhs) != SSA_NAME)
	{
	  tree base = get_base_address (lhs);
	  if (base
	      && (VAR_P (base) || TREE_CODE (base) == RESULT_DECL)
	      && DECL_IGNORED_P (base)
	      && !TREE_STATIC (base)
	      && !DECL_EXTERNAL (base)
	      && (!VAR_P (base) || !DECL_HAS_VALUE_EXPR_P (base)))
	    DECL_NONSHAREABLE (base) = 1;
	}
 
      /* If requested remap dependence info of cliques brought in
         via inlining.  */
      if (id)
	for (unsigned i = 0; i < gimple_num_ops (copy); ++i)
	  {
	    tree op = gimple_op (copy, i);
	    if (!op)
	      continue;
	    if (TREE_CODE (op) == ADDR_EXPR
		|| TREE_CODE (op) == WITH_SIZE_EXPR)
	      op = TREE_OPERAND (op, 0);
	    while (handled_component_p (op))
	      op = TREE_OPERAND (op, 0);
	    if ((TREE_CODE (op) == MEM_REF
		 || TREE_CODE (op) == TARGET_MEM_REF)
		&& MR_DEPENDENCE_CLIQUE (op) > 1
		&& MR_DEPENDENCE_CLIQUE (op) != bb->loop_father->owned_clique)
	      {
		if (!id->dependence_map)
		  id->dependence_map = new hash_map<dependence_hash,
						    unsigned short>;
		bool existed;
		unsigned short &newc = id->dependence_map->get_or_insert
		    (MR_DEPENDENCE_CLIQUE (op), &existed);
		if (!existed)
		  {
		    gcc_assert (MR_DEPENDENCE_CLIQUE (op) <= cfun->last_clique);
		    newc = ++cfun->last_clique;
		  }
		MR_DEPENDENCE_CLIQUE (op) = newc;
	      }
	  }

      /* Create new names for all the definitions created by COPY and
	 add replacement mappings for each new name.  */
      FOR_EACH_SSA_DEF_OPERAND (def_p, copy, op_iter, SSA_OP_ALL_DEFS)
	create_new_def_for (DEF_FROM_PTR (def_p), copy, def_p);
    }

  return new_bb;
}

/* Adds phi node arguments for edge E_COPY after basic block duplication.  */

static void
add_phi_args_after_copy_edge (edge e_copy)
{
  basic_block bb, bb_copy = e_copy->src, dest;
  edge e;
  edge_iterator ei;
  gphi *phi, *phi_copy;
  tree def;
  gphi_iterator psi, psi_copy;

  if (gimple_seq_empty_p (phi_nodes (e_copy->dest)))
    return;

  bb = bb_copy->flags & BB_DUPLICATED ? get_bb_original (bb_copy) : bb_copy;

  if (e_copy->dest->flags & BB_DUPLICATED)
    dest = get_bb_original (e_copy->dest);
  else
    dest = e_copy->dest;

  e = find_edge (bb, dest);
  if (!e)
    {
      /* During loop unrolling the target of the latch edge is copied.
	 In this case we are not looking for edge to dest, but to
	 duplicated block whose original was dest.  */
      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  if ((e->dest->flags & BB_DUPLICATED)
	      && get_bb_original (e->dest) == dest)
	    break;
	}

      gcc_assert (e != NULL);
    }

  for (psi = gsi_start_phis (e->dest),
       psi_copy = gsi_start_phis (e_copy->dest);
       !gsi_end_p (psi);
       gsi_next (&psi), gsi_next (&psi_copy))
    {
      phi = psi.phi ();
      phi_copy = psi_copy.phi ();
      def = PHI_ARG_DEF_FROM_EDGE (phi, e);
      add_phi_arg (phi_copy, def, e_copy,
		   gimple_phi_arg_location_from_edge (phi, e));
    }
}


/* Basic block BB_COPY was created by code duplication.  Add phi node
   arguments for edges going out of BB_COPY.  The blocks that were
   duplicated have BB_DUPLICATED set.  */

void
add_phi_args_after_copy_bb (basic_block bb_copy)
{
  edge e_copy;
  edge_iterator ei;

  FOR_EACH_EDGE (e_copy, ei, bb_copy->succs)
    {
      add_phi_args_after_copy_edge (e_copy);
    }
}

/* Blocks in REGION_COPY array of length N_REGION were created by
   duplication of basic blocks.  Add phi node arguments for edges
   going from these blocks.  If E_COPY is not NULL, also add
   phi node arguments for its destination.*/

void
add_phi_args_after_copy (basic_block *region_copy, unsigned n_region,
			 edge e_copy)
{
  unsigned i;

  for (i = 0; i < n_region; i++)
    region_copy[i]->flags |= BB_DUPLICATED;

  for (i = 0; i < n_region; i++)
    add_phi_args_after_copy_bb (region_copy[i]);
  if (e_copy)
    add_phi_args_after_copy_edge (e_copy);

  for (i = 0; i < n_region; i++)
    region_copy[i]->flags &= ~BB_DUPLICATED;
}

/* Duplicates a REGION (set of N_REGION basic blocks) with just a single
   important exit edge EXIT.  By important we mean that no SSA name defined
   inside region is live over the other exit edges of the region.  All entry
   edges to the region must go to ENTRY->dest.  The edge ENTRY is redirected
   to the duplicate of the region.  Dominance and loop information is
   updated if UPDATE_DOMINANCE is true, but not the SSA web.  If
   UPDATE_DOMINANCE is false then we assume that the caller will update the
   dominance information after calling this function.  The new basic
   blocks are stored to REGION_COPY in the same order as they had in REGION,
   provided that REGION_COPY is not NULL.
   The function returns false if it is unable to copy the region,
   true otherwise.  */

bool
gimple_duplicate_sese_region (edge entry, edge exit,
			    basic_block *region, unsigned n_region,
			    basic_block *region_copy,
			    bool update_dominance)
{
  unsigned i;
  bool free_region_copy = false, copying_header = false;
  struct loop *loop = entry->dest->loop_father;
  edge exit_copy;
  vec<basic_block> doms = vNULL;
  edge redirected;
  profile_count total_count = profile_count::uninitialized ();
  profile_count entry_count = profile_count::uninitialized ();

  if (!can_copy_bbs_p (region, n_region))
    return false;

  /* Some sanity checking.  Note that we do not check for all possible
     missuses of the functions.  I.e. if you ask to copy something weird,
     it will work, but the state of structures probably will not be
     correct.  */
  for (i = 0; i < n_region; i++)
    {
      /* We do not handle subloops, i.e. all the blocks must belong to the
	 same loop.  */
      if (region[i]->loop_father != loop)
	return false;

      if (region[i] != entry->dest
	  && region[i] == loop->header)
	return false;
    }

  /* In case the function is used for loop header copying (which is the primary
     use), ensure that EXIT and its copy will be new latch and entry edges.  */
  if (loop->header == entry->dest)
    {
      copying_header = true;

      if (!dominated_by_p (CDI_DOMINATORS, loop->latch, exit->src))
	return false;

      for (i = 0; i < n_region; i++)
	if (region[i] != exit->src
	    && dominated_by_p (CDI_DOMINATORS, region[i], exit->src))
	  return false;
    }

  initialize_original_copy_tables ();

  if (copying_header)
    set_loop_copy (loop, loop_outer (loop));
  else
    set_loop_copy (loop, loop);

  if (!region_copy)
    {
      region_copy = XNEWVEC (basic_block, n_region);
      free_region_copy = true;
    }

  /* Record blocks outside the region that are dominated by something
     inside.  */
  if (update_dominance)
    {
      doms.create (0);
      doms = get_dominated_by_region (CDI_DOMINATORS, region, n_region);
    }

  if (entry->dest->count.initialized_p ())
    {
      total_count = entry->dest->count;
      entry_count = entry->count ();
      /* Fix up corner cases, to avoid division by zero or creation of negative
	 frequencies.  */
      if (entry_count > total_count)
	entry_count = total_count;
    }

  copy_bbs (region, n_region, region_copy, &exit, 1, &exit_copy, loop,
	    split_edge_bb_loc (entry), update_dominance);
  if (total_count.initialized_p () && entry_count.initialized_p ())
    {
      scale_bbs_frequencies_profile_count (region, n_region,
				           total_count - entry_count,
				           total_count);
      scale_bbs_frequencies_profile_count (region_copy, n_region, entry_count,
				           total_count);
    }

  if (copying_header)
    {
      loop->header = exit->dest;
      loop->latch = exit->src;
    }

  /* Redirect the entry and add the phi node arguments.  */
  redirected = redirect_edge_and_branch (entry, get_bb_copy (entry->dest));
  gcc_assert (redirected != NULL);
  flush_pending_stmts (entry);

  /* Concerning updating of dominators:  We must recount dominators
     for entry block and its copy.  Anything that is outside of the
     region, but was dominated by something inside needs recounting as
     well.  */
  if (update_dominance)
    {
      set_immediate_dominator (CDI_DOMINATORS, entry->dest, entry->src);
      doms.safe_push (get_bb_original (entry->dest));
      iterate_fix_dominators (CDI_DOMINATORS, doms, false);
      doms.release ();
    }

  /* Add the other PHI node arguments.  */
  add_phi_args_after_copy (region_copy, n_region, NULL);

  if (free_region_copy)
    free (region_copy);

  free_original_copy_tables ();
  return true;
}

/* Checks if BB is part of the region defined by N_REGION BBS.  */
static bool 
bb_part_of_region_p (basic_block bb, basic_block* bbs, unsigned n_region)
{
  unsigned int n;

  for (n = 0; n < n_region; n++)
    {
     if (bb == bbs[n])
       return true;
    }
  return false;
}

/* Duplicates REGION consisting of N_REGION blocks.  The new blocks
   are stored to REGION_COPY in the same order in that they appear
   in REGION, if REGION_COPY is not NULL.  ENTRY is the entry to
   the region, EXIT an exit from it.  The condition guarding EXIT
   is moved to ENTRY.  Returns true if duplication succeeds, false
   otherwise.

   For example,

   some_code;
   if (cond)
     A;
   else
     B;

   is transformed to

   if (cond)
     {
       some_code;
       A;
     }
   else
     {
       some_code;
       B;
     }
*/

bool
gimple_duplicate_sese_tail (edge entry, edge exit,
			  basic_block *region, unsigned n_region,
			  basic_block *region_copy)
{
  unsigned i;
  bool free_region_copy = false;
  struct loop *loop = exit->dest->loop_father;
  struct loop *orig_loop = entry->dest->loop_father;
  basic_block switch_bb, entry_bb, nentry_bb;
  vec<basic_block> doms;
  profile_count total_count = profile_count::uninitialized (),
		exit_count = profile_count::uninitialized ();
  edge exits[2], nexits[2], e;
  gimple_stmt_iterator gsi;
  gimple *cond_stmt;
  edge sorig, snew;
  basic_block exit_bb;
  gphi_iterator psi;
  gphi *phi;
  tree def;
  struct loop *target, *aloop, *cloop;

  gcc_assert (EDGE_COUNT (exit->src->succs) == 2);
  exits[0] = exit;
  exits[1] = EDGE_SUCC (exit->src, EDGE_SUCC (exit->src, 0) == exit);

  if (!can_copy_bbs_p (region, n_region))
    return false;

  initialize_original_copy_tables ();
  set_loop_copy (orig_loop, loop);

  target= loop;
  for (aloop = orig_loop->inner; aloop; aloop = aloop->next)
    {
      if (bb_part_of_region_p (aloop->header, region, n_region))
	{
	  cloop = duplicate_loop (aloop, target);
	  duplicate_subloops (aloop, cloop);
	}
    }

  if (!region_copy)
    {
      region_copy = XNEWVEC (basic_block, n_region);
      free_region_copy = true;
    }

  gcc_assert (!need_ssa_update_p (cfun));

  /* Record blocks outside the region that are dominated by something
     inside.  */
  doms = get_dominated_by_region (CDI_DOMINATORS, region, n_region);

  total_count = exit->src->count;
  exit_count = exit->count ();
  /* Fix up corner cases, to avoid division by zero or creation of negative
     frequencies.  */
  if (exit_count > total_count)
    exit_count = total_count;

  copy_bbs (region, n_region, region_copy, exits, 2, nexits, orig_loop,
	    split_edge_bb_loc (exit), true);
  if (total_count.initialized_p () && exit_count.initialized_p ())
    {
      scale_bbs_frequencies_profile_count (region, n_region,
				           total_count - exit_count,
				           total_count);
      scale_bbs_frequencies_profile_count (region_copy, n_region, exit_count,
				           total_count);
    }

  /* Create the switch block, and put the exit condition to it.  */
  entry_bb = entry->dest;
  nentry_bb = get_bb_copy (entry_bb);
  if (!last_stmt (entry->src)
      || !stmt_ends_bb_p (last_stmt (entry->src)))
    switch_bb = entry->src;
  else
    switch_bb = split_edge (entry);
  set_immediate_dominator (CDI_DOMINATORS, nentry_bb, switch_bb);

  gsi = gsi_last_bb (switch_bb);
  cond_stmt = last_stmt (exit->src);
  gcc_assert (gimple_code (cond_stmt) == GIMPLE_COND);
  cond_stmt = gimple_copy (cond_stmt);

  gsi_insert_after (&gsi, cond_stmt, GSI_NEW_STMT);

  sorig = single_succ_edge (switch_bb);
  sorig->flags = exits[1]->flags;
  sorig->probability = exits[1]->probability;
  snew = make_edge (switch_bb, nentry_bb, exits[0]->flags);
  snew->probability = exits[0]->probability;
  

  /* Register the new edge from SWITCH_BB in loop exit lists.  */
  rescan_loop_exit (snew, true, false);

  /* Add the PHI node arguments.  */
  add_phi_args_after_copy (region_copy, n_region, snew);

  /* Get rid of now superfluous conditions and associated edges (and phi node
     arguments).  */
  exit_bb = exit->dest;

  e = redirect_edge_and_branch (exits[0], exits[1]->dest);
  PENDING_STMT (e) = NULL;

  /* The latch of ORIG_LOOP was copied, and so was the backedge 
     to the original header.  We redirect this backedge to EXIT_BB.  */
  for (i = 0; i < n_region; i++)
    if (get_bb_original (region_copy[i]) == orig_loop->latch)
      {
	gcc_assert (single_succ_edge (region_copy[i]));
	e = redirect_edge_and_branch (single_succ_edge (region_copy[i]), exit_bb);
	PENDING_STMT (e) = NULL;
	for (psi = gsi_start_phis (exit_bb);
	     !gsi_end_p (psi);
	     gsi_next (&psi))
	  {
	    phi = psi.phi ();
	    def = PHI_ARG_DEF (phi, nexits[0]->dest_idx);
	    add_phi_arg (phi, def, e, gimple_phi_arg_location_from_edge (phi, e));
	  }
      }
  e = redirect_edge_and_branch (nexits[1], nexits[0]->dest);
  PENDING_STMT (e) = NULL;
  
  /* Anything that is outside of the region, but was dominated by something
     inside needs to update dominance info.  */
  iterate_fix_dominators (CDI_DOMINATORS, doms, false);
  doms.release ();
  /* Update the SSA web.  */
  update_ssa (TODO_update_ssa);

  if (free_region_copy)
    free (region_copy);

  free_original_copy_tables ();
  return true;
}

/* Add all the blocks dominated by ENTRY to the array BBS_P.  Stop
   adding blocks when the dominator traversal reaches EXIT.  This
   function silently assumes that ENTRY strictly dominates EXIT.  */

void
gather_blocks_in_sese_region (basic_block entry, basic_block exit,
			      vec<basic_block> *bbs_p)
{
  basic_block son;

  for (son = first_dom_son (CDI_DOMINATORS, entry);
       son;
       son = next_dom_son (CDI_DOMINATORS, son))
    {
      bbs_p->safe_push (son);
      if (son != exit)
	gather_blocks_in_sese_region (son, exit, bbs_p);
    }
}

/* Replaces *TP with a duplicate (belonging to function TO_CONTEXT).
   The duplicates are recorded in VARS_MAP.  */

static void
replace_by_duplicate_decl (tree *tp, hash_map<tree, tree> *vars_map,
			   tree to_context)
{
  tree t = *tp, new_t;
  struct function *f = DECL_STRUCT_FUNCTION (to_context);

  if (DECL_CONTEXT (t) == to_context)
    return;

  bool existed;
  tree &loc = vars_map->get_or_insert (t, &existed);

  if (!existed)
    {
      if (SSA_VAR_P (t))
	{
	  new_t = copy_var_decl (t, DECL_NAME (t), TREE_TYPE (t));
	  add_local_decl (f, new_t);
	}
      else
	{
	  gcc_assert (TREE_CODE (t) == CONST_DECL);
	  new_t = copy_node (t);
	}
      DECL_CONTEXT (new_t) = to_context;

      loc = new_t;
    }
  else
    new_t = loc;

  *tp = new_t;
}


/* Creates an ssa name in TO_CONTEXT equivalent to NAME.
   VARS_MAP maps old ssa names and var_decls to the new ones.  */

static tree
replace_ssa_name (tree name, hash_map<tree, tree> *vars_map,
		  tree to_context)
{
  tree new_name;

  gcc_assert (!virtual_operand_p (name));

  tree *loc = vars_map->get (name);

  if (!loc)
    {
      tree decl = SSA_NAME_VAR (name);
      if (decl)
	{
	  gcc_assert (!SSA_NAME_IS_DEFAULT_DEF (name));
	  replace_by_duplicate_decl (&decl, vars_map, to_context);
	  new_name = make_ssa_name_fn (DECL_STRUCT_FUNCTION (to_context),
				       decl, SSA_NAME_DEF_STMT (name));
	}
      else
	new_name = copy_ssa_name_fn (DECL_STRUCT_FUNCTION (to_context),
				     name, SSA_NAME_DEF_STMT (name));

      /* Now that we've used the def stmt to define new_name, make sure it
	 doesn't define name anymore.  */
      SSA_NAME_DEF_STMT (name) = NULL;

      vars_map->put (name, new_name);
    }
  else
    new_name = *loc;

  return new_name;
}

struct move_stmt_d
{
  tree orig_block;
  tree new_block;
  tree from_context;
  tree to_context;
  hash_map<tree, tree> *vars_map;
  htab_t new_label_map;
  hash_map<void *, void *> *eh_map;
  bool remap_decls_p;
};

/* Helper for move_block_to_fn.  Set TREE_BLOCK in every expression
   contained in *TP if it has been ORIG_BLOCK previously and change the
   DECL_CONTEXT of every local variable referenced in *TP.  */

static tree
move_stmt_op (tree *tp, int *walk_subtrees, void *data)
{
  struct walk_stmt_info *wi = (struct walk_stmt_info *) data;
  struct move_stmt_d *p = (struct move_stmt_d *) wi->info;
  tree t = *tp;

  if (EXPR_P (t))
    {
      tree block = TREE_BLOCK (t);
      if (block == NULL_TREE)
	;
      else if (block == p->orig_block
	       || p->orig_block == NULL_TREE)
	{
	  /* tree_node_can_be_shared says we can share invariant
	     addresses but unshare_expr copies them anyways.  Make sure
	     to unshare before adjusting the block in place - we do not
	     always see a copy here.  */
	  if (TREE_CODE (t) == ADDR_EXPR
	      && is_gimple_min_invariant (t))
	    *tp = t = unshare_expr (t);
	  TREE_SET_BLOCK (t, p->new_block);
	}
      else if (flag_checking)
	{
	  while (block && TREE_CODE (block) == BLOCK && block != p->orig_block)
	    block = BLOCK_SUPERCONTEXT (block);
	  gcc_assert (block == p->orig_block);
	}
    }
  else if (DECL_P (t) || TREE_CODE (t) == SSA_NAME)
    {
      if (TREE_CODE (t) == SSA_NAME)
	*tp = replace_ssa_name (t, p->vars_map, p->to_context);
      else if (TREE_CODE (t) == PARM_DECL
	       && gimple_in_ssa_p (cfun))
	*tp = *(p->vars_map->get (t));
      else if (TREE_CODE (t) == LABEL_DECL)
	{
	  if (p->new_label_map)
	    {
	      struct tree_map in, *out;
	      in.base.from = t;
	      out = (struct tree_map *)
		htab_find_with_hash (p->new_label_map, &in, DECL_UID (t));
	      if (out)
		*tp = t = out->to;
	    }

	  /* For FORCED_LABELs we can end up with references from other
	     functions if some SESE regions are outlined.  It is UB to
	     jump in between them, but they could be used just for printing
	     addresses etc.  In that case, DECL_CONTEXT on the label should
	     be the function containing the glabel stmt with that LABEL_DECL,
	     rather than whatever function a reference to the label was seen
	     last time.  */
	  if (!FORCED_LABEL (t) && !DECL_NONLOCAL (t))
	    DECL_CONTEXT (t) = p->to_context;
	}
      else if (p->remap_decls_p)
	{
	  /* Replace T with its duplicate.  T should no longer appear in the
	     parent function, so this looks wasteful; however, it may appear
	     in referenced_vars, and more importantly, as virtual operands of
	     statements, and in alias lists of other variables.  It would be
	     quite difficult to expunge it from all those places.  ??? It might
	     suffice to do this for addressable variables.  */
	  if ((VAR_P (t) && !is_global_var (t))
	      || TREE_CODE (t) == CONST_DECL)
	    replace_by_duplicate_decl (tp, p->vars_map, p->to_context);
	}
      *walk_subtrees = 0;
    }
  else if (TYPE_P (t))
    *walk_subtrees = 0;

  return NULL_TREE;
}

/* Helper for move_stmt_r.  Given an EH region number for the source
   function, map that to the duplicate EH regio number in the dest.  */

static int
move_stmt_eh_region_nr (int old_nr, struct move_stmt_d *p)
{
  eh_region old_r, new_r;

  old_r = get_eh_region_from_number (old_nr);
  new_r = static_cast<eh_region> (*p->eh_map->get (old_r));

  return new_r->index;
}

/* Similar, but operate on INTEGER_CSTs.  */

static tree
move_stmt_eh_region_tree_nr (tree old_t_nr, struct move_stmt_d *p)
{
  int old_nr, new_nr;

  old_nr = tree_to_shwi (old_t_nr);
  new_nr = move_stmt_eh_region_nr (old_nr, p);

  return build_int_cst (integer_type_node, new_nr);
}

/* Like move_stmt_op, but for gimple statements.

   Helper for move_block_to_fn.  Set GIMPLE_BLOCK in every expression
   contained in the current statement in *GSI_P and change the
   DECL_CONTEXT of every local variable referenced in the current
   statement.  */

static tree
move_stmt_r (gimple_stmt_iterator *gsi_p, bool *handled_ops_p,
	     struct walk_stmt_info *wi)
{
  struct move_stmt_d *p = (struct move_stmt_d *) wi->info;
  gimple *stmt = gsi_stmt (*gsi_p);
  tree block = gimple_block (stmt);

  if (block == p->orig_block
      || (p->orig_block == NULL_TREE
	  && block != NULL_TREE))
    gimple_set_block (stmt, p->new_block);

  switch (gimple_code (stmt))
    {
    case GIMPLE_CALL:
      /* Remap the region numbers for __builtin_eh_{pointer,filter}.  */
      {
	tree r, fndecl = gimple_call_fndecl (stmt);
	if (fndecl && fndecl_built_in_p (fndecl, BUILT_IN_NORMAL))
	  switch (DECL_FUNCTION_CODE (fndecl))
	    {
	    case BUILT_IN_EH_COPY_VALUES:
	      r = gimple_call_arg (stmt, 1);
	      r = move_stmt_eh_region_tree_nr (r, p);
	      gimple_call_set_arg (stmt, 1, r);
	      /* FALLTHRU */

	    case BUILT_IN_EH_POINTER:
	    case BUILT_IN_EH_FILTER:
	      r = gimple_call_arg (stmt, 0);
	      r = move_stmt_eh_region_tree_nr (r, p);
	      gimple_call_set_arg (stmt, 0, r);
	      break;

	    default:
	      break;
	    }
      }
      break;

    case GIMPLE_RESX:
      {
	gresx *resx_stmt = as_a <gresx *> (stmt);
	int r = gimple_resx_region (resx_stmt);
	r = move_stmt_eh_region_nr (r, p);
	gimple_resx_set_region (resx_stmt, r);
      }
      break;

    case GIMPLE_EH_DISPATCH:
      {
	geh_dispatch *eh_dispatch_stmt = as_a <geh_dispatch *> (stmt);
	int r = gimple_eh_dispatch_region (eh_dispatch_stmt);
	r = move_stmt_eh_region_nr (r, p);
	gimple_eh_dispatch_set_region (eh_dispatch_stmt, r);
      }
      break;

    case GIMPLE_OMP_RETURN:
    case GIMPLE_OMP_CONTINUE:
      break;

    case GIMPLE_LABEL:
      {
	/* For FORCED_LABEL, move_stmt_op doesn't adjust DECL_CONTEXT,
	   so that such labels can be referenced from other regions.
	   Make sure to update it when seeing a GIMPLE_LABEL though,
	   that is the owner of the label.  */
	walk_gimple_op (stmt, move_stmt_op, wi);
	*handled_ops_p = true;
	tree label = gimple_label_label (as_a <glabel *> (stmt));
	if (FORCED_LABEL (label) || DECL_NONLOCAL (label))
	  DECL_CONTEXT (label) = p->to_context;
      }
      break;

    default:
      if (is_gimple_omp (stmt))
	{
	  /* Do not remap variables inside OMP directives.  Variables
	     referenced in clauses and directive header belong to the
	     parent function and should not be moved into the child
	     function.  */
	  bool save_remap_decls_p = p->remap_decls_p;
	  p->remap_decls_p = false;
	  *handled_ops_p = true;

	  walk_gimple_seq_mod (gimple_omp_body_ptr (stmt), move_stmt_r,
			       move_stmt_op, wi);

	  p->remap_decls_p = save_remap_decls_p;
	}
      break;
    }

  return NULL_TREE;
}

/* Move basic block BB from function CFUN to function DEST_FN.  The
   block is moved out of the original linked list and placed after
   block AFTER in the new list.  Also, the block is removed from the
   original array of blocks and placed in DEST_FN's array of blocks.
   If UPDATE_EDGE_COUNT_P is true, the edge counts on both CFGs is
   updated to reflect the moved edges.

   The local variables are remapped to new instances, VARS_MAP is used
   to record the mapping.  */

static void
move_block_to_fn (struct function *dest_cfun, basic_block bb,
		  basic_block after, bool update_edge_count_p,
		  struct move_stmt_d *d)
{
  struct control_flow_graph *cfg;
  edge_iterator ei;
  edge e;
  gimple_stmt_iterator si;
  unsigned old_len, new_len;

  /* Remove BB from dominance structures.  */
  delete_from_dominance_info (CDI_DOMINATORS, bb);

  /* Move BB from its current loop to the copy in the new function.  */
  if (current_loops)
    {
      struct loop *new_loop = (struct loop *)bb->loop_father->aux;
      if (new_loop)
	bb->loop_father = new_loop;
    }

  /* Link BB to the new linked list.  */
  move_block_after (bb, after);

  /* Update the edge count in the corresponding flowgraphs.  */
  if (update_edge_count_p)
    FOR_EACH_EDGE (e, ei, bb->succs)
      {
	cfun->cfg->x_n_edges--;
	dest_cfun->cfg->x_n_edges++;
      }

  /* Remove BB from the original basic block array.  */
  (*cfun->cfg->x_basic_block_info)[bb->index] = NULL;
  cfun->cfg->x_n_basic_blocks--;

  /* Grow DEST_CFUN's basic block array if needed.  */
  cfg = dest_cfun->cfg;
  cfg->x_n_basic_blocks++;
  if (bb->index >= cfg->x_last_basic_block)
    cfg->x_last_basic_block = bb->index + 1;

  old_len = vec_safe_length (cfg->x_basic_block_info);
  if ((unsigned) cfg->x_last_basic_block >= old_len)
    {
      new_len = cfg->x_last_basic_block + (cfg->x_last_basic_block + 3) / 4;
      vec_safe_grow_cleared (cfg->x_basic_block_info, new_len);
    }

  (*cfg->x_basic_block_info)[bb->index] = bb;

  /* Remap the variables in phi nodes.  */
  for (gphi_iterator psi = gsi_start_phis (bb);
       !gsi_end_p (psi); )
    {
      gphi *phi = psi.phi ();
      use_operand_p use;
      tree op = PHI_RESULT (phi);
      ssa_op_iter oi;
      unsigned i;

      if (virtual_operand_p (op))
	{
	  /* Remove the phi nodes for virtual operands (alias analysis will be
	     run for the new function, anyway).  But replace all uses that
	     might be outside of the region we move.  */
	  use_operand_p use_p;
	  imm_use_iterator iter;
	  gimple *use_stmt;
	  FOR_EACH_IMM_USE_STMT (use_stmt, iter, op)
	    FOR_EACH_IMM_USE_ON_STMT (use_p, iter)
	      SET_USE (use_p, SSA_NAME_VAR (op));
          remove_phi_node (&psi, true);
	  continue;
	}

      SET_PHI_RESULT (phi,
		      replace_ssa_name (op, d->vars_map, dest_cfun->decl));
      FOR_EACH_PHI_ARG (use, phi, oi, SSA_OP_USE)
	{
	  op = USE_FROM_PTR (use);
	  if (TREE_CODE (op) == SSA_NAME)
	    SET_USE (use, replace_ssa_name (op, d->vars_map, dest_cfun->decl));
	}

      for (i = 0; i < EDGE_COUNT (bb->preds); i++)
	{
	  location_t locus = gimple_phi_arg_location (phi, i);
	  tree block = LOCATION_BLOCK (locus);

	  if (locus == UNKNOWN_LOCATION)
	    continue;
	  if (d->orig_block == NULL_TREE || block == d->orig_block)
	    {
	      locus = set_block (locus, d->new_block);
	      gimple_phi_arg_set_location (phi, i, locus);
	    }
	}

      gsi_next (&psi);
    }

  for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
    {
      gimple *stmt = gsi_stmt (si);
      struct walk_stmt_info wi;

      memset (&wi, 0, sizeof (wi));
      wi.info = d;
      walk_gimple_stmt (&si, move_stmt_r, move_stmt_op, &wi);

      if (glabel *label_stmt = dyn_cast <glabel *> (stmt))
	{
	  tree label = gimple_label_label (label_stmt);
	  int uid = LABEL_DECL_UID (label);

	  gcc_assert (uid > -1);

	  old_len = vec_safe_length (cfg->x_label_to_block_map);
	  if (old_len <= (unsigned) uid)
	    {
	      new_len = 3 * uid / 2 + 1;
	      vec_safe_grow_cleared (cfg->x_label_to_block_map, new_len);
	    }

	  (*cfg->x_label_to_block_map)[uid] = bb;
	  (*cfun->cfg->x_label_to_block_map)[uid] = NULL;

	  gcc_assert (DECL_CONTEXT (label) == dest_cfun->decl);

	  if (uid >= dest_cfun->cfg->last_label_uid)
	    dest_cfun->cfg->last_label_uid = uid + 1;
	}

      maybe_duplicate_eh_stmt_fn (dest_cfun, stmt, cfun, stmt, d->eh_map, 0);
      remove_stmt_from_eh_lp_fn (cfun, stmt);

      gimple_duplicate_stmt_histograms (dest_cfun, stmt, cfun, stmt);
      gimple_remove_stmt_histograms (cfun, stmt);

      /* We cannot leave any operands allocated from the operand caches of
	 the current function.  */
      free_stmt_operands (cfun, stmt);
      push_cfun (dest_cfun);
      update_stmt (stmt);
      pop_cfun ();
    }

  FOR_EACH_EDGE (e, ei, bb->succs)
    if (e->goto_locus != UNKNOWN_LOCATION)
      {
	tree block = LOCATION_BLOCK (e->goto_locus);
	if (d->orig_block == NULL_TREE
	    || block == d->orig_block)
	  e->goto_locus = set_block (e->goto_locus, d->new_block);
      }
}

/* Examine the statements in BB (which is in SRC_CFUN); find and return
   the outermost EH region.  Use REGION as the incoming base EH region.
   If there is no single outermost region, return NULL and set *ALL to
   true.  */

static eh_region
find_outermost_region_in_block (struct function *src_cfun,
				basic_block bb, eh_region region,
				bool *all)
{
  gimple_stmt_iterator si;

  for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
    {
      gimple *stmt = gsi_stmt (si);
      eh_region stmt_region;
      int lp_nr;

      lp_nr = lookup_stmt_eh_lp_fn (src_cfun, stmt);
      stmt_region = get_eh_region_from_lp_number_fn (src_cfun, lp_nr);
      if (stmt_region)
	{
	  if (region == NULL)
	    region = stmt_region;
	  else if (stmt_region != region)
	    {
	      region = eh_region_outermost (src_cfun, stmt_region, region);
	      if (region == NULL)
		{
		  *all = true;
		  return NULL;
		}
	    }
	}
    }

  return region;
}

static tree
new_label_mapper (tree decl, void *data)
{
  htab_t hash = (htab_t) data;
  struct tree_map *m;
  void **slot;

  gcc_assert (TREE_CODE (decl) == LABEL_DECL);

  m = XNEW (struct tree_map);
  m->hash = DECL_UID (decl);
  m->base.from = decl;
  m->to = create_artificial_label (UNKNOWN_LOCATION);
  LABEL_DECL_UID (m->to) = LABEL_DECL_UID (decl);
  if (LABEL_DECL_UID (m->to) >= cfun->cfg->last_label_uid)
    cfun->cfg->last_label_uid = LABEL_DECL_UID (m->to) + 1;

  slot = htab_find_slot_with_hash (hash, m, m->hash, INSERT);
  gcc_assert (*slot == NULL);

  *slot = m;

  return m->to;
}

/* Tree walker to replace the decls used inside value expressions by
   duplicates.  */

static tree
replace_block_vars_by_duplicates_1 (tree *tp, int *walk_subtrees, void *data)
{
  struct replace_decls_d *rd = (struct replace_decls_d *)data;

  switch (TREE_CODE (*tp))
    {
    case VAR_DECL:
    case PARM_DECL:
    case RESULT_DECL:
      replace_by_duplicate_decl (tp, rd->vars_map, rd->to_context);
      break;
    default:
      break;
    }

  if (IS_TYPE_OR_DECL_P (*tp))
    *walk_subtrees = false;

  return NULL;
}

/* Change DECL_CONTEXT of all BLOCK_VARS in block, including
   subblocks.  */

static void
replace_block_vars_by_duplicates (tree block, hash_map<tree, tree> *vars_map,
				  tree to_context)
{
  tree *tp, t;

  for (tp = &BLOCK_VARS (block); *tp; tp = &DECL_CHAIN (*tp))
    {
      t = *tp;
      if (!VAR_P (t) && TREE_CODE (t) != CONST_DECL)
	continue;
      replace_by_duplicate_decl (&t, vars_map, to_context);
      if (t != *tp)
	{
	  if (VAR_P (*tp) && DECL_HAS_VALUE_EXPR_P (*tp))
	    {
	      tree x = DECL_VALUE_EXPR (*tp);
	      struct replace_decls_d rd = { vars_map, to_context };
	      unshare_expr (x);
	      walk_tree (&x, replace_block_vars_by_duplicates_1, &rd, NULL);
	      SET_DECL_VALUE_EXPR (t, x);
	      DECL_HAS_VALUE_EXPR_P (t) = 1;
	    }
	  DECL_CHAIN (t) = DECL_CHAIN (*tp);
	  *tp = t;
	}
    }

  for (block = BLOCK_SUBBLOCKS (block); block; block = BLOCK_CHAIN (block))
    replace_block_vars_by_duplicates (block, vars_map, to_context);
}

/* Fixup the loop arrays and numbers after moving LOOP and its subloops
   from FN1 to FN2.  */

static void
fixup_loop_arrays_after_move (struct function *fn1, struct function *fn2,
			      struct loop *loop)
{
  /* Discard it from the old loop array.  */
  (*get_loops (fn1))[loop->num] = NULL;

  /* Place it in the new loop array, assigning it a new number.  */
  loop->num = number_of_loops (fn2);
  vec_safe_push (loops_for_fn (fn2)->larray, loop);

  /* Recurse to children.  */
  for (loop = loop->inner; loop; loop = loop->next)
    fixup_loop_arrays_after_move (fn1, fn2, loop);
}

/* Verify that the blocks in BBS_P are a single-entry, single-exit region
   delimited by ENTRY_BB and EXIT_BB, possibly containing noreturn blocks.  */

DEBUG_FUNCTION void
verify_sese (basic_block entry, basic_block exit, vec<basic_block> *bbs_p)
{
  basic_block bb;
  edge_iterator ei;
  edge e;
  bitmap bbs = BITMAP_ALLOC (NULL);
  int i;

  gcc_assert (entry != NULL);
  gcc_assert (entry != exit);
  gcc_assert (bbs_p != NULL);

  gcc_assert (bbs_p->length () > 0);

  FOR_EACH_VEC_ELT (*bbs_p, i, bb)
    bitmap_set_bit (bbs, bb->index);

  gcc_assert (bitmap_bit_p (bbs, entry->index));
  gcc_assert (exit == NULL || bitmap_bit_p (bbs, exit->index));

  FOR_EACH_VEC_ELT (*bbs_p, i, bb)
    {
      if (bb == entry)
	{
	  gcc_assert (single_pred_p (entry));
	  gcc_assert (!bitmap_bit_p (bbs, single_pred (entry)->index));
	}
      else
	for (ei = ei_start (bb->preds); !ei_end_p (ei); ei_next (&ei))
	  {
	    e = ei_edge (ei);
	    gcc_assert (bitmap_bit_p (bbs, e->src->index));
	  }

      if (bb == exit)
	{
	  gcc_assert (single_succ_p (exit));
	  gcc_assert (!bitmap_bit_p (bbs, single_succ (exit)->index));
	}
      else
	for (ei = ei_start (bb->succs); !ei_end_p (ei); ei_next (&ei))
	  {
	    e = ei_edge (ei);
	    gcc_assert (bitmap_bit_p (bbs, e->dest->index));
	  }
    }

  BITMAP_FREE (bbs);
}

/* If FROM is an SSA_NAME, mark the version in bitmap DATA.  */

bool
gather_ssa_name_hash_map_from (tree const &from, tree const &, void *data)
{
  bitmap release_names = (bitmap)data;

  if (TREE_CODE (from) != SSA_NAME)
    return true;

  bitmap_set_bit (release_names, SSA_NAME_VERSION (from));
  return true;
}

/* Return LOOP_DIST_ALIAS call if present in BB.  */

static gimple *
find_loop_dist_alias (basic_block bb)
{
  gimple *g = last_stmt (bb);
  if (g == NULL || gimple_code (g) != GIMPLE_COND)
    return NULL;

  gimple_stmt_iterator gsi = gsi_for_stmt (g);
  gsi_prev (&gsi);
  if (gsi_end_p (gsi))
    return NULL;

  g = gsi_stmt (gsi);
  if (gimple_call_internal_p (g, IFN_LOOP_DIST_ALIAS))
    return g;
  return NULL;
}

/* Fold loop internal call G like IFN_LOOP_VECTORIZED/IFN_LOOP_DIST_ALIAS
   to VALUE and update any immediate uses of it's LHS.  */

void
fold_loop_internal_call (gimple *g, tree value)
{
  tree lhs = gimple_call_lhs (g);
  use_operand_p use_p;
  imm_use_iterator iter;
  gimple *use_stmt;
  gimple_stmt_iterator gsi = gsi_for_stmt (g);

  update_call_from_tree (&gsi, value);
  FOR_EACH_IMM_USE_STMT (use_stmt, iter, lhs)
    {
      FOR_EACH_IMM_USE_ON_STMT (use_p, iter)
	SET_USE (use_p, value);
      update_stmt (use_stmt);
    }
}

/* Move a single-entry, single-exit region delimited by ENTRY_BB and
   EXIT_BB to function DEST_CFUN.  The whole region is replaced by a
   single basic block in the original CFG and the new basic block is
   returned.  DEST_CFUN must not have a CFG yet.

   Note that the region need not be a pure SESE region.  Blocks inside
   the region may contain calls to abort/exit.  The only restriction
   is that ENTRY_BB should be the only entry point and it must
   dominate EXIT_BB.

   Change TREE_BLOCK of all statements in ORIG_BLOCK to the new
   functions outermost BLOCK, move all subblocks of ORIG_BLOCK
   to the new function.

   All local variables referenced in the region are assumed to be in
   the corresponding BLOCK_VARS and unexpanded variable lists
   associated with DEST_CFUN.

   TODO: investigate whether we can reuse gimple_duplicate_sese_region to
   reimplement move_sese_region_to_fn by duplicating the region rather than
   moving it.  */

basic_block
move_sese_region_to_fn (struct function *dest_cfun, basic_block entry_bb,
		        basic_block exit_bb, tree orig_block)
{
  vec<basic_block> bbs, dom_bbs;
  basic_block dom_entry = get_immediate_dominator (CDI_DOMINATORS, entry_bb);
  basic_block after, bb, *entry_pred, *exit_succ, abb;
  struct function *saved_cfun = cfun;
  int *entry_flag, *exit_flag;
  profile_probability *entry_prob, *exit_prob;
  unsigned i, num_entry_edges, num_exit_edges, num_nodes;
  edge e;
  edge_iterator ei;
  htab_t new_label_map;
  hash_map<void *, void *> *eh_map;
  struct loop *loop = entry_bb->loop_father;
  struct loop *loop0 = get_loop (saved_cfun, 0);
  struct move_stmt_d d;

  /* If ENTRY does not strictly dominate EXIT, this cannot be an SESE
     region.  */
  gcc_assert (entry_bb != exit_bb
              && (!exit_bb
		  || dominated_by_p (CDI_DOMINATORS, exit_bb, entry_bb)));

  /* Collect all the blocks in the region.  Manually add ENTRY_BB
     because it won't be added by dfs_enumerate_from.  */
  bbs.create (0);
  bbs.safe_push (entry_bb);
  gather_blocks_in_sese_region (entry_bb, exit_bb, &bbs);

  if (flag_checking)
    verify_sese (entry_bb, exit_bb, &bbs);

  /* The blocks that used to be dominated by something in BBS will now be
     dominated by the new block.  */
  dom_bbs = get_dominated_by_region (CDI_DOMINATORS,
				     bbs.address (),
				     bbs.length ());

  /* Detach ENTRY_BB and EXIT_BB from CFUN->CFG.  We need to remember
     the predecessor edges to ENTRY_BB and the successor edges to
     EXIT_BB so that we can re-attach them to the new basic block that
     will replace the region.  */
  num_entry_edges = EDGE_COUNT (entry_bb->preds);
  entry_pred = XNEWVEC (basic_block, num_entry_edges);
  entry_flag = XNEWVEC (int, num_entry_edges);
  entry_prob = XNEWVEC (profile_probability, num_entry_edges);
  i = 0;
  for (ei = ei_start (entry_bb->preds); (e = ei_safe_edge (ei)) != NULL;)
    {
      entry_prob[i] = e->probability;
      entry_flag[i] = e->flags;
      entry_pred[i++] = e->src;
      remove_edge (e);
    }

  if (exit_bb)
    {
      num_exit_edges = EDGE_COUNT (exit_bb->succs);
      exit_succ = XNEWVEC (basic_block, num_exit_edges);
      exit_flag = XNEWVEC (int, num_exit_edges);
      exit_prob = XNEWVEC (profile_probability, num_exit_edges);
      i = 0;
      for (ei = ei_start (exit_bb->succs); (e = ei_safe_edge (ei)) != NULL;)
	{
	  exit_prob[i] = e->probability;
	  exit_flag[i] = e->flags;
	  exit_succ[i++] = e->dest;
	  remove_edge (e);
	}
    }
  else
    {
      num_exit_edges = 0;
      exit_succ = NULL;
      exit_flag = NULL;
      exit_prob = NULL;
    }

  /* Switch context to the child function to initialize DEST_FN's CFG.  */
  gcc_assert (dest_cfun->cfg == NULL);
  push_cfun (dest_cfun);

  init_empty_tree_cfg ();

  /* Initialize EH information for the new function.  */
  eh_map = NULL;
  new_label_map = NULL;
  if (saved_cfun->eh)
    {
      eh_region region = NULL;
      bool all = false;

      FOR_EACH_VEC_ELT (bbs, i, bb)
	{
	  region = find_outermost_region_in_block (saved_cfun, bb, region, &all);
	  if (all)
	    break;
	}

      init_eh_for_function ();
      if (region != NULL || all)
	{
	  new_label_map = htab_create (17, tree_map_hash, tree_map_eq, free);
	  eh_map = duplicate_eh_regions (saved_cfun, region, 0,
					 new_label_mapper, new_label_map);
	}
    }

  /* Initialize an empty loop tree.  */
  struct loops *loops = ggc_cleared_alloc<struct loops> ();
  init_loops_structure (dest_cfun, loops, 1);
  loops->state = LOOPS_MAY_HAVE_MULTIPLE_LATCHES;
  set_loops_for_fn (dest_cfun, loops);

  vec<loop_p, va_gc> *larray = get_loops (saved_cfun)->copy ();

  /* Move the outlined loop tree part.  */
  num_nodes = bbs.length ();
  FOR_EACH_VEC_ELT (bbs, i, bb)
    {
      if (bb->loop_father->header == bb)
	{
	  struct loop *this_loop = bb->loop_father;
	  struct loop *outer = loop_outer (this_loop);
	  if (outer == loop
	      /* If the SESE region contains some bbs ending with
		 a noreturn call, those are considered to belong
		 to the outermost loop in saved_cfun, rather than
		 the entry_bb's loop_father.  */
	      || outer == loop0)
	    {
	      if (outer != loop)
		num_nodes -= this_loop->num_nodes;
	      flow_loop_tree_node_remove (bb->loop_father);
	      flow_loop_tree_node_add (get_loop (dest_cfun, 0), this_loop);
	      fixup_loop_arrays_after_move (saved_cfun, cfun, this_loop);
	    }
	}
      else if (bb->loop_father == loop0 && loop0 != loop)
	num_nodes--;

      /* Remove loop exits from the outlined region.  */
      if (loops_for_fn (saved_cfun)->exits)
	FOR_EACH_EDGE (e, ei, bb->succs)
	  {
	    struct loops *l = loops_for_fn (saved_cfun);
	    loop_exit **slot
	      = l->exits->find_slot_with_hash (e, htab_hash_pointer (e),
					       NO_INSERT);
	    if (slot)
	      l->exits->clear_slot (slot);
	  }
    }

  /* Adjust the number of blocks in the tree root of the outlined part.  */
  get_loop (dest_cfun, 0)->num_nodes = bbs.length () + 2;

  /* Setup a mapping to be used by move_block_to_fn.  */
  loop->aux = current_loops->tree_root;
  loop0->aux = current_loops->tree_root;

  /* Fix up orig_loop_num.  If the block referenced in it has been moved
     to dest_cfun, update orig_loop_num field, otherwise clear it.  */
  struct loop *dloop;
  signed char *moved_orig_loop_num = NULL;
  FOR_EACH_LOOP_FN (dest_cfun, dloop, 0)
    if (dloop->orig_loop_num)
      {
	if (moved_orig_loop_num == NULL)
	  moved_orig_loop_num
	    = XCNEWVEC (signed char, vec_safe_length (larray));
	if ((*larray)[dloop->orig_loop_num] != NULL
	    && get_loop (saved_cfun, dloop->orig_loop_num) == NULL)
	  {
	    if (moved_orig_loop_num[dloop->orig_loop_num] >= 0
		&& moved_orig_loop_num[dloop->orig_loop_num] < 2)
	      moved_orig_loop_num[dloop->orig_loop_num]++;
	    dloop->orig_loop_num = (*larray)[dloop->orig_loop_num]->num;
	  }
	else
	  {
	    moved_orig_loop_num[dloop->orig_loop_num] = -1;
	    dloop->orig_loop_num = 0;
	  }
      }
  pop_cfun ();

  if (moved_orig_loop_num)
    {
      FOR_EACH_VEC_ELT (bbs, i, bb)
	{
	  gimple *g = find_loop_dist_alias (bb);
	  if (g == NULL)
	    continue;

	  int orig_loop_num = tree_to_shwi (gimple_call_arg (g, 0));
	  gcc_assert (orig_loop_num
		      && (unsigned) orig_loop_num < vec_safe_length (larray));
	  if (moved_orig_loop_num[orig_loop_num] == 2)
	    {
	      /* If we have moved both loops with this orig_loop_num into
		 dest_cfun and the LOOP_DIST_ALIAS call is being moved there
		 too, update the first argument.  */
	      gcc_assert ((*larray)[dloop->orig_loop_num] != NULL
			  && (get_loop (saved_cfun, dloop->orig_loop_num)
			      == NULL));
	      tree t = build_int_cst (integer_type_node,
				      (*larray)[dloop->orig_loop_num]->num);
	      gimple_call_set_arg (g, 0, t);
	      update_stmt (g);
	      /* Make sure the following loop will not update it.  */
	      moved_orig_loop_num[orig_loop_num] = 0;
	    }
	  else
	    /* Otherwise at least one of the loops stayed in saved_cfun.
	       Remove the LOOP_DIST_ALIAS call.  */
	    fold_loop_internal_call (g, gimple_call_arg (g, 1));
	}
      FOR_EACH_BB_FN (bb, saved_cfun)
	{
	  gimple *g = find_loop_dist_alias (bb);
	  if (g == NULL)
	    continue;
	  int orig_loop_num = tree_to_shwi (gimple_call_arg (g, 0));
	  gcc_assert (orig_loop_num
		      && (unsigned) orig_loop_num < vec_safe_length (larray));
	  if (moved_orig_loop_num[orig_loop_num])
	    /* LOOP_DIST_ALIAS call remained in saved_cfun, if at least one
	       of the corresponding loops was moved, remove it.  */
	    fold_loop_internal_call (g, gimple_call_arg (g, 1));
	}
      XDELETEVEC (moved_orig_loop_num);
    }
  ggc_free (larray);

  /* Move blocks from BBS into DEST_CFUN.  */
  gcc_assert (bbs.length () >= 2);
  after = dest_cfun->cfg->x_entry_block_ptr;
  hash_map<tree, tree> vars_map;

  memset (&d, 0, sizeof (d));
  d.orig_block = orig_block;
  d.new_block = DECL_INITIAL (dest_cfun->decl);
  d.from_context = cfun->decl;
  d.to_context = dest_cfun->decl;
  d.vars_map = &vars_map;
  d.new_label_map = new_label_map;
  d.eh_map = eh_map;
  d.remap_decls_p = true;

  if (gimple_in_ssa_p (cfun))
    for (tree arg = DECL_ARGUMENTS (d.to_context); arg; arg = DECL_CHAIN (arg))
      {
	tree narg = make_ssa_name_fn (dest_cfun, arg, gimple_build_nop ());
	set_ssa_default_def (dest_cfun, arg, narg);
	vars_map.put (arg, narg);
      }

  FOR_EACH_VEC_ELT (bbs, i, bb)
    {
      /* No need to update edge counts on the last block.  It has
	 already been updated earlier when we detached the region from
	 the original CFG.  */
      move_block_to_fn (dest_cfun, bb, after, bb != exit_bb, &d);
      after = bb;
    }

  loop->aux = NULL;
  loop0->aux = NULL;
  /* Loop sizes are no longer correct, fix them up.  */
  loop->num_nodes -= num_nodes;
  for (struct loop *outer = loop_outer (loop);
       outer; outer = loop_outer (outer))
    outer->num_nodes -= num_nodes;
  loop0->num_nodes -= bbs.length () - num_nodes;

  if (saved_cfun->has_simduid_loops || saved_cfun->has_force_vectorize_loops)
    {
      struct loop *aloop;
      for (i = 0; vec_safe_iterate (loops->larray, i, &aloop); i++)
	if (aloop != NULL)
	  {
	    if (aloop->simduid)
	      {
		replace_by_duplicate_decl (&aloop->simduid, d.vars_map,
					   d.to_context);
		dest_cfun->has_simduid_loops = true;
	      }
	    if (aloop->force_vectorize)
	      dest_cfun->has_force_vectorize_loops = true;
	  }
    }

  /* Rewire BLOCK_SUBBLOCKS of orig_block.  */
  if (orig_block)
    {
      tree block;
      gcc_assert (BLOCK_SUBBLOCKS (DECL_INITIAL (dest_cfun->decl))
		  == NULL_TREE);
      BLOCK_SUBBLOCKS (DECL_INITIAL (dest_cfun->decl))
	= BLOCK_SUBBLOCKS (orig_block);
      for (block = BLOCK_SUBBLOCKS (orig_block);
	   block; block = BLOCK_CHAIN (block))
	BLOCK_SUPERCONTEXT (block) = DECL_INITIAL (dest_cfun->decl);
      BLOCK_SUBBLOCKS (orig_block) = NULL_TREE;
    }

  replace_block_vars_by_duplicates (DECL_INITIAL (dest_cfun->decl),
				    &vars_map, dest_cfun->decl);

  if (new_label_map)
    htab_delete (new_label_map);
  if (eh_map)
    delete eh_map;

  if (gimple_in_ssa_p (cfun))
    {
      /* We need to release ssa-names in a defined order, so first find them,
	 and then iterate in ascending version order.  */
      bitmap release_names = BITMAP_ALLOC (NULL);
      vars_map.traverse<void *, gather_ssa_name_hash_map_from> (release_names);
      bitmap_iterator bi;
      unsigned i;
      EXECUTE_IF_SET_IN_BITMAP (release_names, 0, i, bi)
	release_ssa_name (ssa_name (i));
      BITMAP_FREE (release_names);
    }

  /* Rewire the entry and exit blocks.  The successor to the entry
     block turns into the successor of DEST_FN's ENTRY_BLOCK_PTR in
     the child function.  Similarly, the predecessor of DEST_FN's
     EXIT_BLOCK_PTR turns into the predecessor of EXIT_BLOCK_PTR.  We
     need to switch CFUN between DEST_CFUN and SAVED_CFUN so that the
     various CFG manipulation function get to the right CFG.

     FIXME, this is silly.  The CFG ought to become a parameter to
     these helpers.  */
  push_cfun (dest_cfun);
  ENTRY_BLOCK_PTR_FOR_FN (cfun)->count = entry_bb->count;
  make_single_succ_edge (ENTRY_BLOCK_PTR_FOR_FN (cfun), entry_bb, EDGE_FALLTHRU);
  if (exit_bb)
    {
      make_single_succ_edge (exit_bb,  EXIT_BLOCK_PTR_FOR_FN (cfun), 0);
      EXIT_BLOCK_PTR_FOR_FN (cfun)->count = exit_bb->count;
    }
  else
    EXIT_BLOCK_PTR_FOR_FN (cfun)->count = profile_count::zero ();
  pop_cfun ();

  /* Back in the original function, the SESE region has disappeared,
     create a new basic block in its place.  */
  bb = create_empty_bb (entry_pred[0]);
  if (current_loops)
    add_bb_to_loop (bb, loop);
  for (i = 0; i < num_entry_edges; i++)
    {
      e = make_edge (entry_pred[i], bb, entry_flag[i]);
      e->probability = entry_prob[i];
    }

  for (i = 0; i < num_exit_edges; i++)
    {
      e = make_edge (bb, exit_succ[i], exit_flag[i]);
      e->probability = exit_prob[i];
    }

  set_immediate_dominator (CDI_DOMINATORS, bb, dom_entry);
  FOR_EACH_VEC_ELT (dom_bbs, i, abb)
    set_immediate_dominator (CDI_DOMINATORS, abb, bb);
  dom_bbs.release ();

  if (exit_bb)
    {
      free (exit_prob);
      free (exit_flag);
      free (exit_succ);
    }
  free (entry_prob);
  free (entry_flag);
  free (entry_pred);
  bbs.release ();

  return bb;
}

/* Dump default def DEF to file FILE using FLAGS and indentation
   SPC.  */

static void
dump_default_def (FILE *file, tree def, int spc, dump_flags_t flags)
{
  for (int i = 0; i < spc; ++i)
    fprintf (file, " ");
  dump_ssaname_info_to_file (file, def, spc);

  print_generic_expr (file, TREE_TYPE (def), flags);
  fprintf (file, " ");
  print_generic_expr (file, def, flags);
  fprintf (file, " = ");
  print_generic_expr (file, SSA_NAME_VAR (def), flags);
  fprintf (file, ";\n");
}

/* Print no_sanitize attribute to FILE for a given attribute VALUE.  */

static void
print_no_sanitize_attr_value (FILE *file, tree value)
{
  unsigned int flags = tree_to_uhwi (value);
  bool first = true;
  for (int i = 0; sanitizer_opts[i].name != NULL; ++i)
    {
      if ((sanitizer_opts[i].flag & flags) == sanitizer_opts[i].flag)
	{
	  if (!first)
	    fprintf (file, " | ");
	  fprintf (file, "%s", sanitizer_opts[i].name);
	  first = false;
	}
    }
}

/* Dump FUNCTION_DECL FN to file FILE using FLAGS (see TDF_* in dumpfile.h)
   */

void
dump_function_to_file (tree fndecl, FILE *file, dump_flags_t flags)
{
  tree arg, var, old_current_fndecl = current_function_decl;
  struct function *dsf;
  bool ignore_topmost_bind = false, any_var = false;
  basic_block bb;
  tree chain;
  bool tmclone = (TREE_CODE (fndecl) == FUNCTION_DECL
		  && decl_is_tm_clone (fndecl));
  struct function *fun = DECL_STRUCT_FUNCTION (fndecl);

  if (DECL_ATTRIBUTES (fndecl) != NULL_TREE)
    {
      fprintf (file, "__attribute__((");

      bool first = true;
      tree chain;
      for (chain = DECL_ATTRIBUTES (fndecl); chain;
	   first = false, chain = TREE_CHAIN (chain))
	{
	  if (!first)
	    fprintf (file, ", ");

	  tree name = get_attribute_name (chain);
	  print_generic_expr (file, name, dump_flags);
	  if (TREE_VALUE (chain) != NULL_TREE)
	    {
	      fprintf (file, " (");

	      if (strstr (IDENTIFIER_POINTER (name), "no_sanitize"))
		print_no_sanitize_attr_value (file, TREE_VALUE (chain));
	      else
		print_generic_expr (file, TREE_VALUE (chain), dump_flags);
	      fprintf (file, ")");
	    }
	}

      fprintf (file, "))\n");
    }

  current_function_decl = fndecl;
  if (flags & TDF_GIMPLE)
    {
      print_generic_expr (file, TREE_TYPE (TREE_TYPE (fndecl)),
			  dump_flags | TDF_SLIM);
      fprintf (file, " __GIMPLE (%s)\n%s (",
	       (fun->curr_properties & PROP_ssa) ? "ssa"
	       : (fun->curr_properties & PROP_cfg) ? "cfg"
	       : "",
	       function_name (fun));
    }
  else
    fprintf (file, "%s %s(", function_name (fun), tmclone ? "[tm-clone] " : "");

  arg = DECL_ARGUMENTS (fndecl);
  while (arg)
    {
      print_generic_expr (file, TREE_TYPE (arg), dump_flags);
      fprintf (file, " ");
      print_generic_expr (file, arg, dump_flags);
      if (DECL_CHAIN (arg))
	fprintf (file, ", ");
      arg = DECL_CHAIN (arg);
    }
  fprintf (file, ")\n");

  dsf = DECL_STRUCT_FUNCTION (fndecl);
  if (dsf && (flags & TDF_EH))
    dump_eh_tree (file, dsf);

  if (flags & TDF_RAW && !gimple_has_body_p (fndecl))
    {
      dump_node (fndecl, TDF_SLIM | flags, file);
      current_function_decl = old_current_fndecl;
      return;
    }

  /* When GIMPLE is lowered, the variables are no longer available in
     BIND_EXPRs, so display them separately.  */
  if (fun && fun->decl == fndecl && (fun->curr_properties & PROP_gimple_lcf))
    {
      unsigned ix;
      ignore_topmost_bind = true;

      fprintf (file, "{\n");
      if (gimple_in_ssa_p (fun)
	  && (flags & TDF_ALIAS))
	{
	  for (arg = DECL_ARGUMENTS (fndecl); arg != NULL;
	       arg = DECL_CHAIN (arg))
	    {
	      tree def = ssa_default_def (fun, arg);
	      if (def)
		dump_default_def (file, def, 2, flags);
	    }

	  tree res = DECL_RESULT (fun->decl);
	  if (res != NULL_TREE
	      && DECL_BY_REFERENCE (res))
	    {
	      tree def = ssa_default_def (fun, res);
	      if (def)
		dump_default_def (file, def, 2, flags);
	    }

	  tree static_chain = fun->static_chain_decl;
	  if (static_chain != NULL_TREE)
	    {
	      tree def = ssa_default_def (fun, static_chain);
	      if (def)
		dump_default_def (file, def, 2, flags);
	    }
	}

      if (!vec_safe_is_empty (fun->local_decls))
	FOR_EACH_LOCAL_DECL (fun, ix, var)
	  {
	    print_generic_decl (file, var, flags);
	    fprintf (file, "\n");

	    any_var = true;
	  }

      tree name;

      if (gimple_in_ssa_p (cfun))
	FOR_EACH_SSA_NAME (ix, name, cfun)
	  {
	    if (!SSA_NAME_VAR (name))
	      {
		fprintf (file, "  ");
		print_generic_expr (file, TREE_TYPE (name), flags);
		fprintf (file, " ");
		print_generic_expr (file, name, flags);
		fprintf (file, ";\n");

		any_var = true;
	      }
	  }
    }

  if (fun && fun->decl == fndecl
      && fun->cfg
      && basic_block_info_for_fn (fun))
    {
      /* If the CFG has been built, emit a CFG-based dump.  */
      if (!ignore_topmost_bind)
	fprintf (file, "{\n");

      if (any_var && n_basic_blocks_for_fn (fun))
	fprintf (file, "\n");

      FOR_EACH_BB_FN (bb, fun)
	dump_bb (file, bb, 2, flags);

      fprintf (file, "}\n");
    }
  else if (fun->curr_properties & PROP_gimple_any)
    {
      /* The function is now in GIMPLE form but the CFG has not been
	 built yet.  Emit the single sequence of GIMPLE statements
	 that make up its body.  */
      gimple_seq body = gimple_body (fndecl);

      if (gimple_seq_first_stmt (body)
	  && gimple_seq_first_stmt (body) == gimple_seq_last_stmt (body)
	  && gimple_code (gimple_seq_first_stmt (body)) == GIMPLE_BIND)
	print_gimple_seq (file, body, 0, flags);
      else
	{
	  if (!ignore_topmost_bind)
	    fprintf (file, "{\n");

	  if (any_var)
	    fprintf (file, "\n");

	  print_gimple_seq (file, body, 2, flags);
	  fprintf (file, "}\n");
	}
    }
  else
    {
      int indent;

      /* Make a tree based dump.  */
      chain = DECL_SAVED_TREE (fndecl);
      if (chain && TREE_CODE (chain) == BIND_EXPR)
	{
	  if (ignore_topmost_bind)
	    {
	      chain = BIND_EXPR_BODY (chain);
	      indent = 2;
	    }
	  else
	    indent = 0;
	}
      else
	{
	  if (!ignore_topmost_bind)
	    {
	      fprintf (file, "{\n");
	      /* No topmost bind, pretend it's ignored for later.  */
	      ignore_topmost_bind = true;
	    }
	  indent = 2;
	}

      if (any_var)
	fprintf (file, "\n");

      print_generic_stmt_indented (file, chain, flags, indent);
      if (ignore_topmost_bind)
	fprintf (file, "}\n");
    }

  if (flags & TDF_ENUMERATE_LOCALS)
    dump_enumerated_decls (file, flags);
  fprintf (file, "\n\n");

  current_function_decl = old_current_fndecl;
}

/* Dump FUNCTION_DECL FN to stderr using FLAGS (see TDF_* in tree.h)  */

DEBUG_FUNCTION void
debug_function (tree fn, dump_flags_t flags)
{
  dump_function_to_file (fn, stderr, flags);
}


/* Print on FILE the indexes for the predecessors of basic_block BB.  */

static void
print_pred_bbs (FILE *file, basic_block bb)
{
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->preds)
    fprintf (file, "bb_%d ", e->src->index);
}


/* Print on FILE the indexes for the successors of basic_block BB.  */

static void
print_succ_bbs (FILE *file, basic_block bb)
{
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->succs)
    fprintf (file, "bb_%d ", e->dest->index);
}

/* Print to FILE the basic block BB following the VERBOSITY level.  */

void
print_loops_bb (FILE *file, basic_block bb, int indent, int verbosity)
{
  char *s_indent = (char *) alloca ((size_t) indent + 1);
  memset ((void *) s_indent, ' ', (size_t) indent);
  s_indent[indent] = '\0';

  /* Print basic_block's header.  */
  if (verbosity >= 2)
    {
      fprintf (file, "%s  bb_%d (preds = {", s_indent, bb->index);
      print_pred_bbs (file, bb);
      fprintf (file, "}, succs = {");
      print_succ_bbs (file, bb);
      fprintf (file, "})\n");
    }

  /* Print basic_block's body.  */
  if (verbosity >= 3)
    {
      fprintf (file, "%s  {\n", s_indent);
      dump_bb (file, bb, indent + 4, TDF_VOPS|TDF_MEMSYMS);
      fprintf (file, "%s  }\n", s_indent);
    }
}

static void print_loop_and_siblings (FILE *, struct loop *, int, int);

/* Pretty print LOOP on FILE, indented INDENT spaces.  Following
   VERBOSITY level this outputs the contents of the loop, or just its
   structure.  */

static void
print_loop (FILE *file, struct loop *loop, int indent, int verbosity)
{
  char *s_indent;
  basic_block bb;

  if (loop == NULL)
    return;

  s_indent = (char *) alloca ((size_t) indent + 1);
  memset ((void *) s_indent, ' ', (size_t) indent);
  s_indent[indent] = '\0';

  /* Print loop's header.  */
  fprintf (file, "%sloop_%d (", s_indent, loop->num);
  if (loop->header)
    fprintf (file, "header = %d", loop->header->index);
  else
    {
      fprintf (file, "deleted)\n");
      return;
    }
  if (loop->latch)
    fprintf (file, ", latch = %d", loop->latch->index);
  else
    fprintf (file, ", multiple latches");
  fprintf (file, ", niter = ");
  print_generic_expr (file, loop->nb_iterations);

  if (loop->any_upper_bound)
    {
      fprintf (file, ", upper_bound = ");
      print_decu (loop->nb_iterations_upper_bound, file);
    }
  if (loop->any_likely_upper_bound)
    {
      fprintf (file, ", likely_upper_bound = ");
      print_decu (loop->nb_iterations_likely_upper_bound, file);
    }

  if (loop->any_estimate)
    {
      fprintf (file, ", estimate = ");
      print_decu (loop->nb_iterations_estimate, file);
    }
  if (loop->unroll)
    fprintf (file, ", unroll = %d", loop->unroll);
  fprintf (file, ")\n");

  /* Print loop's body.  */
  if (verbosity >= 1)
    {
      fprintf (file, "%s{\n", s_indent);
      FOR_EACH_BB_FN (bb, cfun)
	if (bb->loop_father == loop)
	  print_loops_bb (file, bb, indent, verbosity);

      print_loop_and_siblings (file, loop->inner, indent + 2, verbosity);
      fprintf (file, "%s}\n", s_indent);
    }
}

/* Print the LOOP and its sibling loops on FILE, indented INDENT
   spaces.  Following VERBOSITY level this outputs the contents of the
   loop, or just its structure.  */

static void
print_loop_and_siblings (FILE *file, struct loop *loop, int indent,
			 int verbosity)
{
  if (loop == NULL)
    return;

  print_loop (file, loop, indent, verbosity);
  print_loop_and_siblings (file, loop->next, indent, verbosity);
}

/* Follow a CFG edge from the entry point of the program, and on entry
   of a loop, pretty print the loop structure on FILE.  */

void
print_loops (FILE *file, int verbosity)
{
  basic_block bb;

  bb = ENTRY_BLOCK_PTR_FOR_FN (cfun);
  fprintf (file, "\nLoops in function: %s\n", current_function_name ());
  if (bb && bb->loop_father)
    print_loop_and_siblings (file, bb->loop_father, 0, verbosity);
}

/* Dump a loop.  */

DEBUG_FUNCTION void
debug (struct loop &ref)
{
  print_loop (stderr, &ref, 0, /*verbosity*/0);
}

DEBUG_FUNCTION void
debug (struct loop *ptr)
{
  if (ptr)
    debug (*ptr);
  else
    fprintf (stderr, "<nil>\n");
}

/* Dump a loop verbosely.  */

DEBUG_FUNCTION void
debug_verbose (struct loop &ref)
{
  print_loop (stderr, &ref, 0, /*verbosity*/3);
}

DEBUG_FUNCTION void
debug_verbose (struct loop *ptr)
{
  if (ptr)
    debug (*ptr);
  else
    fprintf (stderr, "<nil>\n");
}


/* Debugging loops structure at tree level, at some VERBOSITY level.  */

DEBUG_FUNCTION void
debug_loops (int verbosity)
{
  print_loops (stderr, verbosity);
}

/* Print on stderr the code of LOOP, at some VERBOSITY level.  */

DEBUG_FUNCTION void
debug_loop (struct loop *loop, int verbosity)
{
  print_loop (stderr, loop, 0, verbosity);
}

/* Print on stderr the code of loop number NUM, at some VERBOSITY
   level.  */

DEBUG_FUNCTION void
debug_loop_num (unsigned num, int verbosity)
{
  debug_loop (get_loop (cfun, num), verbosity);
}

/* Return true if BB ends with a call, possibly followed by some
   instructions that must stay with the call.  Return false,
   otherwise.  */

static bool
gimple_block_ends_with_call_p (basic_block bb)
{
  gimple_stmt_iterator gsi = gsi_last_nondebug_bb (bb);
  return !gsi_end_p (gsi) && is_gimple_call (gsi_stmt (gsi));
}


/* Return true if BB ends with a conditional branch.  Return false,
   otherwise.  */

static bool
gimple_block_ends_with_condjump_p (const_basic_block bb)
{
  gimple *stmt = last_stmt (CONST_CAST_BB (bb));
  return (stmt && gimple_code (stmt) == GIMPLE_COND);
}


/* Return true if statement T may terminate execution of BB in ways not
   explicitly represtented in the CFG.  */

bool
stmt_can_terminate_bb_p (gimple *t)
{
  tree fndecl = NULL_TREE;
  int call_flags = 0;

  /* Eh exception not handled internally terminates execution of the whole
     function.  */
  if (stmt_can_throw_external (cfun, t))
    return true;

  /* NORETURN and LONGJMP calls already have an edge to exit.
     CONST and PURE calls do not need one.
     We don't currently check for CONST and PURE here, although
     it would be a good idea, because those attributes are
     figured out from the RTL in mark_constant_function, and
     the counter incrementation code from -fprofile-arcs
     leads to different results from -fbranch-probabilities.  */
  if (is_gimple_call (t))
    {
      fndecl = gimple_call_fndecl (t);
      call_flags = gimple_call_flags (t);
    }

  if (is_gimple_call (t)
      && fndecl
      && fndecl_built_in_p (fndecl)
      && (call_flags & ECF_NOTHROW)
      && !(call_flags & ECF_RETURNS_TWICE)
      /* fork() doesn't really return twice, but the effect of
         wrapping it in __gcov_fork() which calls __gcov_flush()
	 and clears the counters before forking has the same
	 effect as returning twice.  Force a fake edge.  */
      && !fndecl_built_in_p (fndecl, BUILT_IN_FORK))
    return false;

  if (is_gimple_call (t))
    {
      edge_iterator ei;
      edge e;
      basic_block bb;

      if (call_flags & (ECF_PURE | ECF_CONST)
	  && !(call_flags & ECF_LOOPING_CONST_OR_PURE))
	return false;

      /* Function call may do longjmp, terminate program or do other things.
	 Special case noreturn that have non-abnormal edges out as in this case
	 the fact is sufficiently represented by lack of edges out of T.  */
      if (!(call_flags & ECF_NORETURN))
	return true;

      bb = gimple_bb (t);
      FOR_EACH_EDGE (e, ei, bb->succs)
	if ((e->flags & EDGE_FAKE) == 0)
	  return true;
    }

  if (gasm *asm_stmt = dyn_cast <gasm *> (t))
    if (gimple_asm_volatile_p (asm_stmt) || gimple_asm_input_p (asm_stmt))
      return true;

  return false;
}


/* Add fake edges to the function exit for any non constant and non
   noreturn calls (or noreturn calls with EH/abnormal edges),
   volatile inline assembly in the bitmap of blocks specified by BLOCKS
   or to the whole CFG if BLOCKS is zero.  Return the number of blocks
   that were split.

   The goal is to expose cases in which entering a basic block does
   not imply that all subsequent instructions must be executed.  */

static int
gimple_flow_call_edges_add (sbitmap blocks)
{
  int i;
  int blocks_split = 0;
  int last_bb = last_basic_block_for_fn (cfun);
  bool check_last_block = false;

  if (n_basic_blocks_for_fn (cfun) == NUM_FIXED_BLOCKS)
    return 0;

  if (! blocks)
    check_last_block = true;
  else
    check_last_block = bitmap_bit_p (blocks,
				     EXIT_BLOCK_PTR_FOR_FN (cfun)->prev_bb->index);

  /* In the last basic block, before epilogue generation, there will be
     a fallthru edge to EXIT.  Special care is required if the last insn
     of the last basic block is a call because make_edge folds duplicate
     edges, which would result in the fallthru edge also being marked
     fake, which would result in the fallthru edge being removed by
     remove_fake_edges, which would result in an invalid CFG.

     Moreover, we can't elide the outgoing fake edge, since the block
     profiler needs to take this into account in order to solve the minimal
     spanning tree in the case that the call doesn't return.

     Handle this by adding a dummy instruction in a new last basic block.  */
  if (check_last_block)
    {
      basic_block bb = EXIT_BLOCK_PTR_FOR_FN (cfun)->prev_bb;
      gimple_stmt_iterator gsi = gsi_last_nondebug_bb (bb);
      gimple *t = NULL;

      if (!gsi_end_p (gsi))
	t = gsi_stmt (gsi);

      if (t && stmt_can_terminate_bb_p (t))
	{
	  edge e;

	  e = find_edge (bb, EXIT_BLOCK_PTR_FOR_FN (cfun));
	  if (e)
	    {
	      gsi_insert_on_edge (e, gimple_build_nop ());
	      gsi_commit_edge_inserts ();
	    }
	}
    }

  /* Now add fake edges to the function exit for any non constant
     calls since there is no way that we can determine if they will
     return or not...  */
  for (i = 0; i < last_bb; i++)
    {
      basic_block bb = BASIC_BLOCK_FOR_FN (cfun, i);
      gimple_stmt_iterator gsi;
      gimple *stmt, *last_stmt;

      if (!bb)
	continue;

      if (blocks && !bitmap_bit_p (blocks, i))
	continue;

      gsi = gsi_last_nondebug_bb (bb);
      if (!gsi_end_p (gsi))
	{
	  last_stmt = gsi_stmt (gsi);
	  do
	    {
	      stmt = gsi_stmt (gsi);
	      if (stmt_can_terminate_bb_p (stmt))
		{
		  edge e;

		  /* The handling above of the final block before the
		     epilogue should be enough to verify that there is
		     no edge to the exit block in CFG already.
		     Calling make_edge in such case would cause us to
		     mark that edge as fake and remove it later.  */
		  if (flag_checking && stmt == last_stmt)
		    {
		      e = find_edge (bb, EXIT_BLOCK_PTR_FOR_FN (cfun));
		      gcc_assert (e == NULL);
		    }

		  /* Note that the following may create a new basic block
		     and renumber the existing basic blocks.  */
		  if (stmt != last_stmt)
		    {
		      e = split_block (bb, stmt);
		      if (e)
			blocks_split++;
		    }
		  e = make_edge (bb, EXIT_BLOCK_PTR_FOR_FN (cfun), EDGE_FAKE);
		  e->probability = profile_probability::guessed_never ();
		}
	      gsi_prev (&gsi);
	    }
	  while (!gsi_end_p (gsi));
	}
    }

  if (blocks_split)
    checking_verify_flow_info ();

  return blocks_split;
}

/* Removes edge E and all the blocks dominated by it, and updates dominance
   information.  The IL in E->src needs to be updated separately.
   If dominance info is not available, only the edge E is removed.*/

void
remove_edge_and_dominated_blocks (edge e)
{
  vec<basic_block> bbs_to_remove = vNULL;
  vec<basic_block> bbs_to_fix_dom = vNULL;
  edge f;
  edge_iterator ei;
  bool none_removed = false;
  unsigned i;
  basic_block bb, dbb;
  bitmap_iterator bi;

  /* If we are removing a path inside a non-root loop that may change
     loop ownership of blocks or remove loops.  Mark loops for fixup.  */
  if (current_loops
      && loop_outer (e->src->loop_father) != NULL
      && e->src->loop_father == e->dest->loop_father)
    loops_state_set (LOOPS_NEED_FIXUP);

  if (!dom_info_available_p (CDI_DOMINATORS))
    {
      remove_edge (e);
      return;
    }

  /* No updating is needed for edges to exit.  */
  if (e->dest == EXIT_BLOCK_PTR_FOR_FN (cfun))
    {
      if (cfgcleanup_altered_bbs)
	bitmap_set_bit (cfgcleanup_altered_bbs, e->src->index);
      remove_edge (e);
      return;
    }

  /* First, we find the basic blocks to remove.  If E->dest has a predecessor
     that is not dominated by E->dest, then this set is empty.  Otherwise,
     all the basic blocks dominated by E->dest are removed.

     Also, to DF_IDOM we store the immediate dominators of the blocks in
     the dominance frontier of E (i.e., of the successors of the
     removed blocks, if there are any, and of E->dest otherwise).  */
  FOR_EACH_EDGE (f, ei, e->dest->preds)
    {
      if (f == e)
	continue;

      if (!dominated_by_p (CDI_DOMINATORS, f->src, e->dest))
	{
	  none_removed = true;
	  break;
	}
    }

  auto_bitmap df, df_idom;
  if (none_removed)
    bitmap_set_bit (df_idom,
		    get_immediate_dominator (CDI_DOMINATORS, e->dest)->index);
  else
    {
      bbs_to_remove = get_all_dominated_blocks (CDI_DOMINATORS, e->dest);
      FOR_EACH_VEC_ELT (bbs_to_remove, i, bb)
	{
	  FOR_EACH_EDGE (f, ei, bb->succs)
	    {
	      if (f->dest != EXIT_BLOCK_PTR_FOR_FN (cfun))
		bitmap_set_bit (df, f->dest->index);
	    }
	}
      FOR_EACH_VEC_ELT (bbs_to_remove, i, bb)
	bitmap_clear_bit (df, bb->index);

      EXECUTE_IF_SET_IN_BITMAP (df, 0, i, bi)
	{
	  bb = BASIC_BLOCK_FOR_FN (cfun, i);
	  bitmap_set_bit (df_idom,
			  get_immediate_dominator (CDI_DOMINATORS, bb)->index);
	}
    }

  if (cfgcleanup_altered_bbs)
    {
      /* Record the set of the altered basic blocks.  */
      bitmap_set_bit (cfgcleanup_altered_bbs, e->src->index);
      bitmap_ior_into (cfgcleanup_altered_bbs, df);
    }

  /* Remove E and the cancelled blocks.  */
  if (none_removed)
    remove_edge (e);
  else
    {
      /* Walk backwards so as to get a chance to substitute all
	 released DEFs into debug stmts.  See
	 eliminate_unnecessary_stmts() in tree-ssa-dce.c for more
	 details.  */
      for (i = bbs_to_remove.length (); i-- > 0; )
	delete_basic_block (bbs_to_remove[i]);
    }

  /* Update the dominance information.  The immediate dominator may change only
     for blocks whose immediate dominator belongs to DF_IDOM:

     Suppose that idom(X) = Y before removal of E and idom(X) != Y after the
     removal.  Let Z the arbitrary block such that idom(Z) = Y and
     Z dominates X after the removal.  Before removal, there exists a path P
     from Y to X that avoids Z.  Let F be the last edge on P that is
     removed, and let W = F->dest.  Before removal, idom(W) = Y (since Y
     dominates W, and because of P, Z does not dominate W), and W belongs to
     the dominance frontier of E.  Therefore, Y belongs to DF_IDOM.  */
  EXECUTE_IF_SET_IN_BITMAP (df_idom, 0, i, bi)
    {
      bb = BASIC_BLOCK_FOR_FN (cfun, i);
      for (dbb = first_dom_son (CDI_DOMINATORS, bb);
	   dbb;
	   dbb = next_dom_son (CDI_DOMINATORS, dbb))
	bbs_to_fix_dom.safe_push (dbb);
    }

  iterate_fix_dominators (CDI_DOMINATORS, bbs_to_fix_dom, true);

  bbs_to_remove.release ();
  bbs_to_fix_dom.release ();
}

/* Purge dead EH edges from basic block BB.  */

bool
gimple_purge_dead_eh_edges (basic_block bb)
{
  bool changed = false;
  edge e;
  edge_iterator ei;
  gimple *stmt = last_stmt (bb);

  if (stmt && stmt_can_throw_internal (cfun, stmt))
    return false;

  for (ei = ei_start (bb->succs); (e = ei_safe_edge (ei)); )
    {
      if (e->flags & EDGE_EH)
	{
	  remove_edge_and_dominated_blocks (e);
	  changed = true;
	}
      else
	ei_next (&ei);
    }

  return changed;
}

/* Purge dead EH edges from basic block listed in BLOCKS.  */

bool
gimple_purge_all_dead_eh_edges (const_bitmap blocks)
{
  bool changed = false;
  unsigned i;
  bitmap_iterator bi;

  EXECUTE_IF_SET_IN_BITMAP (blocks, 0, i, bi)
    {
      basic_block bb = BASIC_BLOCK_FOR_FN (cfun, i);

      /* Earlier gimple_purge_dead_eh_edges could have removed
	 this basic block already.  */
      gcc_assert (bb || changed);
      if (bb != NULL)
	changed |= gimple_purge_dead_eh_edges (bb);
    }

  return changed;
}

/* Purge dead abnormal call edges from basic block BB.  */

bool
gimple_purge_dead_abnormal_call_edges (basic_block bb)
{
  bool changed = false;
  edge e;
  edge_iterator ei;
  gimple *stmt = last_stmt (bb);

  if (!cfun->has_nonlocal_label
      && !cfun->calls_setjmp)
    return false;

  if (stmt && stmt_can_make_abnormal_goto (stmt))
    return false;

  for (ei = ei_start (bb->succs); (e = ei_safe_edge (ei)); )
    {
      if (e->flags & EDGE_ABNORMAL)
	{
	  if (e->flags & EDGE_FALLTHRU)
	    e->flags &= ~EDGE_ABNORMAL;
	  else
	    remove_edge_and_dominated_blocks (e);
	  changed = true;
	}
      else
	ei_next (&ei);
    }

  return changed;
}

/* Purge dead abnormal call edges from basic block listed in BLOCKS.  */

bool
gimple_purge_all_dead_abnormal_call_edges (const_bitmap blocks)
{
  bool changed = false;
  unsigned i;
  bitmap_iterator bi;

  EXECUTE_IF_SET_IN_BITMAP (blocks, 0, i, bi)
    {
      basic_block bb = BASIC_BLOCK_FOR_FN (cfun, i);

      /* Earlier gimple_purge_dead_abnormal_call_edges could have removed
	 this basic block already.  */
      gcc_assert (bb || changed);
      if (bb != NULL)
	changed |= gimple_purge_dead_abnormal_call_edges (bb);
    }

  return changed;
}

/* This function is called whenever a new edge is created or
   redirected.  */

static void
gimple_execute_on_growing_pred (edge e)
{
  basic_block bb = e->dest;

  if (!gimple_seq_empty_p (phi_nodes (bb)))
    reserve_phi_args_for_new_edge (bb);
}

/* This function is called immediately before edge E is removed from
   the edge vector E->dest->preds.  */

static void
gimple_execute_on_shrinking_pred (edge e)
{
  if (!gimple_seq_empty_p (phi_nodes (e->dest)))
    remove_phi_args (e);
}

/*---------------------------------------------------------------------------
  Helper functions for Loop versioning
  ---------------------------------------------------------------------------*/

/* Adjust phi nodes for 'first' basic block.  'second' basic block is a copy
   of 'first'. Both of them are dominated by 'new_head' basic block. When
   'new_head' was created by 'second's incoming edge it received phi arguments
   on the edge by split_edge(). Later, additional edge 'e' was created to
   connect 'new_head' and 'first'. Now this routine adds phi args on this
   additional edge 'e' that new_head to second edge received as part of edge
   splitting.  */

static void
gimple_lv_adjust_loop_header_phi (basic_block first, basic_block second,
				  basic_block new_head, edge e)
{
  gphi *phi1, *phi2;
  gphi_iterator psi1, psi2;
  tree def;
  edge e2 = find_edge (new_head, second);

  /* Because NEW_HEAD has been created by splitting SECOND's incoming
     edge, we should always have an edge from NEW_HEAD to SECOND.  */
  gcc_assert (e2 != NULL);

  /* Browse all 'second' basic block phi nodes and add phi args to
     edge 'e' for 'first' head. PHI args are always in correct order.  */

  for (psi2 = gsi_start_phis (second),
       psi1 = gsi_start_phis (first);
       !gsi_end_p (psi2) && !gsi_end_p (psi1);
       gsi_next (&psi2),  gsi_next (&psi1))
    {
      phi1 = psi1.phi ();
      phi2 = psi2.phi ();
      def = PHI_ARG_DEF (phi2, e2->dest_idx);
      add_phi_arg (phi1, def, e, gimple_phi_arg_location_from_edge (phi2, e2));
    }
}


/* Adds a if else statement to COND_BB with condition COND_EXPR.
   SECOND_HEAD is the destination of the THEN and FIRST_HEAD is
   the destination of the ELSE part.  */

static void
gimple_lv_add_condition_to_bb (basic_block first_head ATTRIBUTE_UNUSED,
			       basic_block second_head ATTRIBUTE_UNUSED,
			       basic_block cond_bb, void *cond_e)
{
  gimple_stmt_iterator gsi;
  gimple *new_cond_expr;
  tree cond_expr = (tree) cond_e;
  edge e0;

  /* Build new conditional expr */
  new_cond_expr = gimple_build_cond_from_tree (cond_expr,
					       NULL_TREE, NULL_TREE);

  /* Add new cond in cond_bb.  */
  gsi = gsi_last_bb (cond_bb);
  gsi_insert_after (&gsi, new_cond_expr, GSI_NEW_STMT);

  /* Adjust edges appropriately to connect new head with first head
     as well as second head.  */
  e0 = single_succ_edge (cond_bb);
  e0->flags &= ~EDGE_FALLTHRU;
  e0->flags |= EDGE_FALSE_VALUE;
}


/* Do book-keeping of basic block BB for the profile consistency checker.
   Store the counting in RECORD.  */
static void
gimple_account_profile_record (basic_block bb,
			       struct profile_record *record)
{
  gimple_stmt_iterator i;
  for (i = gsi_start_bb (bb); !gsi_end_p (i); gsi_next (&i))
    {
      record->size
	+= estimate_num_insns (gsi_stmt (i), &eni_size_weights);
      if (bb->count.initialized_p ())
	record->time
	  += estimate_num_insns (gsi_stmt (i),
				 &eni_time_weights) * bb->count.to_gcov_type ();
      else if (profile_status_for_fn (cfun) == PROFILE_GUESSED)
	record->time
	  += estimate_num_insns (gsi_stmt (i),
				 &eni_time_weights) * bb->count.to_frequency (cfun);
    }
}

struct cfg_hooks gimple_cfg_hooks = {
  "gimple",
  gimple_verify_flow_info,
  gimple_dump_bb,		/* dump_bb  */
  gimple_dump_bb_for_graph,	/* dump_bb_for_graph  */
  create_bb,			/* create_basic_block  */
  gimple_redirect_edge_and_branch, /* redirect_edge_and_branch  */
  gimple_redirect_edge_and_branch_force, /* redirect_edge_and_branch_force  */
  gimple_can_remove_branch_p,	/* can_remove_branch_p  */
  remove_bb,			/* delete_basic_block  */
  gimple_split_block,		/* split_block  */
  gimple_move_block_after,	/* move_block_after  */
  gimple_can_merge_blocks_p,	/* can_merge_blocks_p  */
  gimple_merge_blocks,		/* merge_blocks  */
  gimple_predict_edge,		/* predict_edge  */
  gimple_predicted_by_p,	/* predicted_by_p  */
  gimple_can_duplicate_bb_p,	/* can_duplicate_block_p  */
  gimple_duplicate_bb,		/* duplicate_block  */
  gimple_split_edge,		/* split_edge  */
  gimple_make_forwarder_block,	/* make_forward_block  */
  NULL,				/* tidy_fallthru_edge  */
  NULL,				/* force_nonfallthru */
  gimple_block_ends_with_call_p,/* block_ends_with_call_p */
  gimple_block_ends_with_condjump_p, /* block_ends_with_condjump_p */
  gimple_flow_call_edges_add,   /* flow_call_edges_add */
  gimple_execute_on_growing_pred,	/* execute_on_growing_pred */
  gimple_execute_on_shrinking_pred, /* execute_on_shrinking_pred */
  gimple_duplicate_loop_to_header_edge, /* duplicate loop for trees */
  gimple_lv_add_condition_to_bb, /* lv_add_condition_to_bb */
  gimple_lv_adjust_loop_header_phi, /* lv_adjust_loop_header_phi*/
  extract_true_false_edges_from_block, /* extract_cond_bb_edges */
  flush_pending_stmts, 		/* flush_pending_stmts */  
  gimple_empty_block_p,           /* block_empty_p */
  gimple_split_block_before_cond_jump, /* split_block_before_cond_jump */
  gimple_account_profile_record,
};


/* Split all critical edges.  */

unsigned int
split_critical_edges (void)
{
  basic_block bb;
  edge e;
  edge_iterator ei;

  /* split_edge can redirect edges out of SWITCH_EXPRs, which can get
     expensive.  So we want to enable recording of edge to CASE_LABEL_EXPR
     mappings around the calls to split_edge.  */
  start_recording_case_labels ();
  FOR_ALL_BB_FN (bb, cfun)
    {
      FOR_EACH_EDGE (e, ei, bb->succs)
        {
	  if (EDGE_CRITICAL_P (e) && !(e->flags & EDGE_ABNORMAL))
	    split_edge (e);
	  /* PRE inserts statements to edges and expects that
	     since split_critical_edges was done beforehand, committing edge
	     insertions will not split more edges.  In addition to critical
	     edges we must split edges that have multiple successors and
	     end by control flow statements, such as RESX.
	     Go ahead and split them too.  This matches the logic in
	     gimple_find_edge_insert_loc.  */
	  else if ((!single_pred_p (e->dest)
	            || !gimple_seq_empty_p (phi_nodes (e->dest))
		    || e->dest == EXIT_BLOCK_PTR_FOR_FN (cfun))
		   && e->src != ENTRY_BLOCK_PTR_FOR_FN (cfun)
	           && !(e->flags & EDGE_ABNORMAL))
	    {
	      gimple_stmt_iterator gsi;

	      gsi = gsi_last_bb (e->src);
	      if (!gsi_end_p (gsi)
		  && stmt_ends_bb_p (gsi_stmt (gsi))
		  && (gimple_code (gsi_stmt (gsi)) != GIMPLE_RETURN
		      && !gimple_call_builtin_p (gsi_stmt (gsi),
						 BUILT_IN_RETURN)))
		split_edge (e);
	    }
	}
    }
  end_recording_case_labels ();
  return 0;
}

namespace {

const pass_data pass_data_split_crit_edges =
{
  GIMPLE_PASS, /* type */
  "crited", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_TREE_SPLIT_EDGES, /* tv_id */
  PROP_cfg, /* properties_required */
  PROP_no_crit_edges, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_split_crit_edges : public gimple_opt_pass
{
public:
  pass_split_crit_edges (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_split_crit_edges, ctxt)
  {}

  /* opt_pass methods: */
  virtual unsigned int execute (function *) { return split_critical_edges (); }

  opt_pass * clone () { return new pass_split_crit_edges (m_ctxt); }
}; // class pass_split_crit_edges

} // anon namespace

gimple_opt_pass *
make_pass_split_crit_edges (gcc::context *ctxt)
{
  return new pass_split_crit_edges (ctxt);
}


/* Insert COND expression which is GIMPLE_COND after STMT
   in basic block BB with appropriate basic block split
   and creation of a new conditionally executed basic block.
   Update profile so the new bb is visited with probability PROB.
   Return created basic block.  */
basic_block
insert_cond_bb (basic_block bb, gimple *stmt, gimple *cond,
	        profile_probability prob)
{
  edge fall = split_block (bb, stmt);
  gimple_stmt_iterator iter = gsi_last_bb (bb);
  basic_block new_bb;

  /* Insert cond statement.  */
  gcc_assert (gimple_code (cond) == GIMPLE_COND);
  if (gsi_end_p (iter))
    gsi_insert_before (&iter, cond, GSI_CONTINUE_LINKING);
  else
    gsi_insert_after (&iter, cond, GSI_CONTINUE_LINKING);

  /* Create conditionally executed block.  */
  new_bb = create_empty_bb (bb);
  edge e = make_edge (bb, new_bb, EDGE_TRUE_VALUE);
  e->probability = prob;
  new_bb->count = e->count ();
  make_single_succ_edge (new_bb, fall->dest, EDGE_FALLTHRU);

  /* Fix edge for split bb.  */
  fall->flags = EDGE_FALSE_VALUE;
  fall->probability -= e->probability;

  /* Update dominance info.  */
  if (dom_info_available_p (CDI_DOMINATORS))
    {
      set_immediate_dominator (CDI_DOMINATORS, new_bb, bb);
      set_immediate_dominator (CDI_DOMINATORS, fall->dest, bb);
    }

  /* Update loop info.  */
  if (current_loops)
    add_bb_to_loop (new_bb, bb->loop_father);

  return new_bb;
}

/* Build a ternary operation and gimplify it.  Emit code before GSI.
   Return the gimple_val holding the result.  */

tree
gimplify_build3 (gimple_stmt_iterator *gsi, enum tree_code code,
		 tree type, tree a, tree b, tree c)
{
  tree ret;
  location_t loc = gimple_location (gsi_stmt (*gsi));

  ret = fold_build3_loc (loc, code, type, a, b, c);
  return force_gimple_operand_gsi (gsi, ret, true, NULL, true,
                                   GSI_SAME_STMT);
}

/* Build a binary operation and gimplify it.  Emit code before GSI.
   Return the gimple_val holding the result.  */

tree
gimplify_build2 (gimple_stmt_iterator *gsi, enum tree_code code,
		 tree type, tree a, tree b)
{
  tree ret;

  ret = fold_build2_loc (gimple_location (gsi_stmt (*gsi)), code, type, a, b);
  return force_gimple_operand_gsi (gsi, ret, true, NULL, true,
                                   GSI_SAME_STMT);
}

/* Build a unary operation and gimplify it.  Emit code before GSI.
   Return the gimple_val holding the result.  */

tree
gimplify_build1 (gimple_stmt_iterator *gsi, enum tree_code code, tree type,
		 tree a)
{
  tree ret;

  ret = fold_build1_loc (gimple_location (gsi_stmt (*gsi)), code, type, a);
  return force_gimple_operand_gsi (gsi, ret, true, NULL, true,
                                   GSI_SAME_STMT);
}



/* Given a basic block B which ends with a conditional and has
   precisely two successors, determine which of the edges is taken if
   the conditional is true and which is taken if the conditional is
   false.  Set TRUE_EDGE and FALSE_EDGE appropriately.  */

void
extract_true_false_edges_from_block (basic_block b,
				     edge *true_edge,
				     edge *false_edge)
{
  edge e = EDGE_SUCC (b, 0);

  if (e->flags & EDGE_TRUE_VALUE)
    {
      *true_edge = e;
      *false_edge = EDGE_SUCC (b, 1);
    }
  else
    {
      *false_edge = e;
      *true_edge = EDGE_SUCC (b, 1);
    }
}


/* From a controlling predicate in the immediate dominator DOM of
   PHIBLOCK determine the edges into PHIBLOCK that are chosen if the
   predicate evaluates to true and false and store them to
   *TRUE_CONTROLLED_EDGE and *FALSE_CONTROLLED_EDGE if
   they are non-NULL.  Returns true if the edges can be determined,
   else return false.  */

bool
extract_true_false_controlled_edges (basic_block dom, basic_block phiblock,
				     edge *true_controlled_edge,
				     edge *false_controlled_edge)
{
  basic_block bb = phiblock;
  edge true_edge, false_edge, tem;
  edge e0 = NULL, e1 = NULL;

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
    e0 = tem;
  else if (tem == false_edge
	   || (single_pred_p (false_edge->dest)
	       && (tem->src == false_edge->dest
		   || dominated_by_p (CDI_DOMINATORS,
				      tem->src, false_edge->dest))))
    e1 = tem;
  else
    return false;
  tem = EDGE_PRED (bb, 1);
  if (tem == true_edge
      || (single_pred_p (true_edge->dest)
	  && (tem->src == true_edge->dest
	      || dominated_by_p (CDI_DOMINATORS,
				 tem->src, true_edge->dest))))
    e0 = tem;
  else if (tem == false_edge
	   || (single_pred_p (false_edge->dest)
	       && (tem->src == false_edge->dest
		   || dominated_by_p (CDI_DOMINATORS,
				      tem->src, false_edge->dest))))
    e1 = tem;
  else
    return false;
  if (!e0 || !e1)
    return false;

  if (true_controlled_edge)
    *true_controlled_edge = e0;
  if (false_controlled_edge)
    *false_controlled_edge = e1;

  return true;
}

/* Generate a range test LHS CODE RHS that determines whether INDEX is in the
    range [low, high].  Place associated stmts before *GSI.  */

void
generate_range_test (basic_block bb, tree index, tree low, tree high,
		     tree *lhs, tree *rhs)
{
  tree type = TREE_TYPE (index);
  tree utype = unsigned_type_for (type);

  low = fold_convert (utype, low);
  high = fold_convert (utype, high);

  gimple_seq seq = NULL;
  index = gimple_convert (&seq, utype, index);
  *lhs = gimple_build (&seq, MINUS_EXPR, utype, index, low);
  *rhs = const_binop (MINUS_EXPR, utype, high, low);

  gimple_stmt_iterator gsi = gsi_last_bb (bb);
  gsi_insert_seq_before (&gsi, seq, GSI_SAME_STMT);
}

/* Return the basic block that belongs to label numbered INDEX
   of a switch statement.  */

basic_block
gimple_switch_label_bb (function *ifun, gswitch *gs, unsigned index)
{
  return label_to_block (ifun, CASE_LABEL (gimple_switch_label (gs, index)));
}

/* Return the default basic block of a switch statement.  */

basic_block
gimple_switch_default_bb (function *ifun, gswitch *gs)
{
  return gimple_switch_label_bb (ifun, gs, 0);
}

/* Return the edge that belongs to label numbered INDEX
   of a switch statement.  */

edge
gimple_switch_edge (function *ifun, gswitch *gs, unsigned index)
{
  return find_edge (gimple_bb (gs), gimple_switch_label_bb (ifun, gs, index));
}

/* Return the default edge of a switch statement.  */

edge
gimple_switch_default_edge (function *ifun, gswitch *gs)
{
  return gimple_switch_edge (ifun, gs, 0);
}


/* Emit return warnings.  */

namespace {

const pass_data pass_data_warn_function_return =
{
  GIMPLE_PASS, /* type */
  "*warn_function_return", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_warn_function_return : public gimple_opt_pass
{
public:
  pass_warn_function_return (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_warn_function_return, ctxt)
  {}

  /* opt_pass methods: */
  virtual unsigned int execute (function *);

}; // class pass_warn_function_return

unsigned int
pass_warn_function_return::execute (function *fun)
{
  location_t location;
  gimple *last;
  edge e;
  edge_iterator ei;

  if (!targetm.warn_func_return (fun->decl))
    return 0;

  /* If we have a path to EXIT, then we do return.  */
  if (TREE_THIS_VOLATILE (fun->decl)
      && EDGE_COUNT (EXIT_BLOCK_PTR_FOR_FN (fun)->preds) > 0)
    {
      location = UNKNOWN_LOCATION;
      for (ei = ei_start (EXIT_BLOCK_PTR_FOR_FN (fun)->preds);
	   (e = ei_safe_edge (ei)); )
	{
	  last = last_stmt (e->src);
	  if ((gimple_code (last) == GIMPLE_RETURN
	       || gimple_call_builtin_p (last, BUILT_IN_RETURN))
	      && location == UNKNOWN_LOCATION
	      && ((location = LOCATION_LOCUS (gimple_location (last)))
		  != UNKNOWN_LOCATION)
	      && !optimize)
	    break;
	  /* When optimizing, replace return stmts in noreturn functions
	     with __builtin_unreachable () call.  */
	  if (optimize && gimple_code (last) == GIMPLE_RETURN)
	    {
	      tree fndecl = builtin_decl_implicit (BUILT_IN_UNREACHABLE);
	      gimple *new_stmt = gimple_build_call (fndecl, 0);
	      gimple_set_location (new_stmt, gimple_location (last));
	      gimple_stmt_iterator gsi = gsi_for_stmt (last);
	      gsi_replace (&gsi, new_stmt, true);
	      remove_edge (e);
	    }
	  else
	    ei_next (&ei);
	}
      if (location == UNKNOWN_LOCATION)
	location = cfun->function_end_locus;
      warning_at (location, 0, "%<noreturn%> function does return");
    }

  /* If we see "return;" in some basic block, then we do reach the end
     without returning a value.  */
  else if (warn_return_type > 0
	   && !TREE_NO_WARNING (fun->decl)
	   && !VOID_TYPE_P (TREE_TYPE (TREE_TYPE (fun->decl))))
    {
      FOR_EACH_EDGE (e, ei, EXIT_BLOCK_PTR_FOR_FN (fun)->preds)
	{
	  gimple *last = last_stmt (e->src);
	  greturn *return_stmt = dyn_cast <greturn *> (last);
	  if (return_stmt
	      && gimple_return_retval (return_stmt) == NULL
	      && !gimple_no_warning_p (last))
	    {
	      location = gimple_location (last);
	      if (LOCATION_LOCUS (location) == UNKNOWN_LOCATION)
		location = fun->function_end_locus;
	      if (warning_at (location, OPT_Wreturn_type,
			      "control reaches end of non-void function"))
		TREE_NO_WARNING (fun->decl) = 1;
	      break;
	    }
	}
      /* The C++ FE turns fallthrough from the end of non-void function
	 into __builtin_unreachable () call with BUILTINS_LOCATION.
	 Recognize those too.  */
      basic_block bb;
      if (!TREE_NO_WARNING (fun->decl))
	FOR_EACH_BB_FN (bb, fun)
	  if (EDGE_COUNT (bb->succs) == 0)
	    {
	      gimple *last = last_stmt (bb);
	      const enum built_in_function ubsan_missing_ret
		= BUILT_IN_UBSAN_HANDLE_MISSING_RETURN;
	      if (last
		  && ((LOCATION_LOCUS (gimple_location (last))
		       == BUILTINS_LOCATION
		       && gimple_call_builtin_p (last, BUILT_IN_UNREACHABLE))
		      || gimple_call_builtin_p (last, ubsan_missing_ret)))
		{
		  gimple_stmt_iterator gsi = gsi_for_stmt (last);
		  gsi_prev_nondebug (&gsi);
		  gimple *prev = gsi_stmt (gsi);
		  if (prev == NULL)
		    location = UNKNOWN_LOCATION;
		  else
		    location = gimple_location (prev);
		  if (LOCATION_LOCUS (location) == UNKNOWN_LOCATION)
		    location = fun->function_end_locus;
		  if (warning_at (location, OPT_Wreturn_type,
				  "control reaches end of non-void function"))
		    TREE_NO_WARNING (fun->decl) = 1;
		  break;
		}
	    }
    }
  return 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_warn_function_return (gcc::context *ctxt)
{
  return new pass_warn_function_return (ctxt);
}

/* Walk a gimplified function and warn for functions whose return value is
   ignored and attribute((warn_unused_result)) is set.  This is done before
   inlining, so we don't have to worry about that.  */

static void
do_warn_unused_result (gimple_seq seq)
{
  tree fdecl, ftype;
  gimple_stmt_iterator i;

  for (i = gsi_start (seq); !gsi_end_p (i); gsi_next (&i))
    {
      gimple *g = gsi_stmt (i);

      switch (gimple_code (g))
	{
	case GIMPLE_BIND:
	  do_warn_unused_result (gimple_bind_body (as_a <gbind *>(g)));
	  break;
	case GIMPLE_TRY:
	  do_warn_unused_result (gimple_try_eval (g));
	  do_warn_unused_result (gimple_try_cleanup (g));
	  break;
	case GIMPLE_CATCH:
	  do_warn_unused_result (gimple_catch_handler (
				   as_a <gcatch *> (g)));
	  break;
	case GIMPLE_EH_FILTER:
	  do_warn_unused_result (gimple_eh_filter_failure (g));
	  break;

	case GIMPLE_CALL:
	  if (gimple_call_lhs (g))
	    break;
	  if (gimple_call_internal_p (g))
	    break;

	  /* This is a naked call, as opposed to a GIMPLE_CALL with an
	     LHS.  All calls whose value is ignored should be
	     represented like this.  Look for the attribute.  */
	  fdecl = gimple_call_fndecl (g);
	  ftype = gimple_call_fntype (g);

	  if (lookup_attribute ("warn_unused_result", TYPE_ATTRIBUTES (ftype)))
	    {
	      location_t loc = gimple_location (g);

	      if (fdecl)
		warning_at (loc, OPT_Wunused_result,
			    "ignoring return value of %qD, "
			    "declared with attribute warn_unused_result",
			    fdecl);
	      else
		warning_at (loc, OPT_Wunused_result,
			    "ignoring return value of function "
			    "declared with attribute warn_unused_result");
	    }
	  break;

	default:
	  /* Not a container, not a call, or a call whose value is used.  */
	  break;
	}
    }
}

namespace {

const pass_data pass_data_warn_unused_result =
{
  GIMPLE_PASS, /* type */
  "*warn_unused_result", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_gimple_any, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_warn_unused_result : public gimple_opt_pass
{
public:
  pass_warn_unused_result (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_warn_unused_result, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return flag_warn_unused_result; }
  virtual unsigned int execute (function *)
    {
      do_warn_unused_result (gimple_body (current_function_decl));
      return 0;
    }

}; // class pass_warn_unused_result

} // anon namespace

gimple_opt_pass *
make_pass_warn_unused_result (gcc::context *ctxt)
{
  return new pass_warn_unused_result (ctxt);
}

/* IPA passes, compilation of earlier functions or inlining
   might have changed some properties, such as marked functions nothrow,
   pure, const or noreturn.
   Remove redundant edges and basic blocks, and create new ones if necessary.

   This pass can't be executed as stand alone pass from pass manager, because
   in between inlining and this fixup the verify_flow_info would fail.  */

unsigned int
execute_fixup_cfg (void)
{
  basic_block bb;
  gimple_stmt_iterator gsi;
  int todo = 0;
  cgraph_node *node = cgraph_node::get (current_function_decl);
  profile_count num = node->count;
  profile_count den = ENTRY_BLOCK_PTR_FOR_FN (cfun)->count;
  bool scale = num.initialized_p () && !(num == den);

  if (scale)
    {
      profile_count::adjust_for_ipa_scaling (&num, &den);
      ENTRY_BLOCK_PTR_FOR_FN (cfun)->count = node->count;
      EXIT_BLOCK_PTR_FOR_FN (cfun)->count
        = EXIT_BLOCK_PTR_FOR_FN (cfun)->count.apply_scale (num, den);
    }

  FOR_EACH_BB_FN (bb, cfun)
    {
      if (scale)
        bb->count = bb->count.apply_scale (num, den);
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi);)
	{
	  gimple *stmt = gsi_stmt (gsi);
	  tree decl = is_gimple_call (stmt)
		      ? gimple_call_fndecl (stmt)
		      : NULL;
	  if (decl)
	    {
	      int flags = gimple_call_flags (stmt);
	      if (flags & (ECF_CONST | ECF_PURE | ECF_LOOPING_CONST_OR_PURE))
		{
		  if (gimple_purge_dead_abnormal_call_edges (bb))
		    todo |= TODO_cleanup_cfg;

		  if (gimple_in_ssa_p (cfun))
		    {
		      todo |= TODO_update_ssa | TODO_cleanup_cfg;
		      update_stmt (stmt);
		    }
		}

	      if (flags & ECF_NORETURN
		  && fixup_noreturn_call (stmt))
		todo |= TODO_cleanup_cfg;
	     }

	  /* Remove stores to variables we marked write-only.
	     Keep access when store has side effect, i.e. in case when source
	     is volatile.  */
	  if (gimple_store_p (stmt)
	      && !gimple_has_side_effects (stmt))
	    {
	      tree lhs = get_base_address (gimple_get_lhs (stmt));

	      if (VAR_P (lhs)
		  && (TREE_STATIC (lhs) || DECL_EXTERNAL (lhs))
		  && varpool_node::get (lhs)->writeonly)
		{
		  unlink_stmt_vdef (stmt);
		  gsi_remove (&gsi, true);
		  release_defs (stmt);
	          todo |= TODO_update_ssa | TODO_cleanup_cfg;
	          continue;
		}
	    }
	  /* For calls we can simply remove LHS when it is known
	     to be write-only.  */
	  if (is_gimple_call (stmt)
	      && gimple_get_lhs (stmt))
	    {
	      tree lhs = get_base_address (gimple_get_lhs (stmt));

	      if (VAR_P (lhs)
		  && (TREE_STATIC (lhs) || DECL_EXTERNAL (lhs))
		  && varpool_node::get (lhs)->writeonly)
		{
		  gimple_call_set_lhs (stmt, NULL);
		  update_stmt (stmt);
	          todo |= TODO_update_ssa | TODO_cleanup_cfg;
		}
	    }

	  if (maybe_clean_eh_stmt (stmt)
	      && gimple_purge_dead_eh_edges (bb))
	    todo |= TODO_cleanup_cfg;
	  gsi_next (&gsi);
	}

      /* If we have a basic block with no successors that does not
	 end with a control statement or a noreturn call end it with
	 a call to __builtin_unreachable.  This situation can occur
	 when inlining a noreturn call that does in fact return.  */
      if (EDGE_COUNT (bb->succs) == 0)
	{
	  gimple *stmt = last_stmt (bb);
	  if (!stmt
	      || (!is_ctrl_stmt (stmt)
		  && (!is_gimple_call (stmt)
		      || !gimple_call_noreturn_p (stmt))))
	    {
	      if (stmt && is_gimple_call (stmt))
		gimple_call_set_ctrl_altering (stmt, false);
	      tree fndecl = builtin_decl_implicit (BUILT_IN_UNREACHABLE);
	      stmt = gimple_build_call (fndecl, 0);
	      gimple_stmt_iterator gsi = gsi_last_bb (bb);
	      gsi_insert_after (&gsi, stmt, GSI_NEW_STMT);
	      if (!cfun->after_inlining)
		{
		  gcall *call_stmt = dyn_cast <gcall *> (stmt);
		  node->create_edge (cgraph_node::get_create (fndecl),
				     call_stmt, bb->count);
		}
	    }
	}
    }
  if (scale)
    compute_function_frequency ();

  if (current_loops
      && (todo & TODO_cleanup_cfg))
    loops_state_set (LOOPS_NEED_FIXUP);

  return todo;
}

namespace {

const pass_data pass_data_fixup_cfg =
{
  GIMPLE_PASS, /* type */
  "fixup_cfg", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_fixup_cfg : public gimple_opt_pass
{
public:
  pass_fixup_cfg (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_fixup_cfg, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_fixup_cfg (m_ctxt); }
  virtual unsigned int execute (function *) { return execute_fixup_cfg (); }

}; // class pass_fixup_cfg

} // anon namespace

gimple_opt_pass *
make_pass_fixup_cfg (gcc::context *ctxt)
{
  return new pass_fixup_cfg (ctxt);
}

/* Garbage collection support for edge_def.  */

extern void gt_ggc_mx (tree&);
extern void gt_ggc_mx (gimple *&);
extern void gt_ggc_mx (rtx&);
extern void gt_ggc_mx (basic_block&);

static void
gt_ggc_mx (rtx_insn *& x)
{
  if (x)
    gt_ggc_mx_rtx_def ((void *) x);
}

void
gt_ggc_mx (edge_def *e)
{
  tree block = LOCATION_BLOCK (e->goto_locus);
  gt_ggc_mx (e->src);
  gt_ggc_mx (e->dest);
  if (current_ir_type () == IR_GIMPLE)
    gt_ggc_mx (e->insns.g);
  else
    gt_ggc_mx (e->insns.r);
  gt_ggc_mx (block);
}

/* PCH support for edge_def.  */

extern void gt_pch_nx (tree&);
extern void gt_pch_nx (gimple *&);
extern void gt_pch_nx (rtx&);
extern void gt_pch_nx (basic_block&);

static void
gt_pch_nx (rtx_insn *& x)
{
  if (x)
    gt_pch_nx_rtx_def ((void *) x);
}

void
gt_pch_nx (edge_def *e)
{
  tree block = LOCATION_BLOCK (e->goto_locus);
  gt_pch_nx (e->src);
  gt_pch_nx (e->dest);
  if (current_ir_type () == IR_GIMPLE)
    gt_pch_nx (e->insns.g);
  else
    gt_pch_nx (e->insns.r);
  gt_pch_nx (block);
}

void
gt_pch_nx (edge_def *e, gt_pointer_operator op, void *cookie)
{
  tree block = LOCATION_BLOCK (e->goto_locus);
  op (&(e->src), cookie);
  op (&(e->dest), cookie);
  if (current_ir_type () == IR_GIMPLE)
    op (&(e->insns.g), cookie);
  else
    op (&(e->insns.r), cookie);
  op (&(block), cookie);
}

#if CHECKING_P

namespace selftest {

/* Helper function for CFG selftests: create a dummy function decl
   and push it as cfun.  */

static tree
push_fndecl (const char *name)
{
  tree fn_type = build_function_type_array (integer_type_node, 0, NULL);
  /* FIXME: this uses input_location: */
  tree fndecl = build_fn_decl (name, fn_type);
  tree retval = build_decl (UNKNOWN_LOCATION, RESULT_DECL,
			    NULL_TREE, integer_type_node);
  DECL_RESULT (fndecl) = retval;
  push_struct_function (fndecl);
  function *fun = DECL_STRUCT_FUNCTION (fndecl);
  ASSERT_TRUE (fun != NULL);
  init_empty_tree_cfg_for_function (fun);
  ASSERT_EQ (2, n_basic_blocks_for_fn (fun));
  ASSERT_EQ (0, n_edges_for_fn (fun));
  return fndecl;
}

/* These tests directly create CFGs.
   Compare with the static fns within tree-cfg.c:
     - build_gimple_cfg
     - make_blocks: calls create_basic_block (seq, bb);
     - make_edges.   */

/* Verify a simple cfg of the form:
     ENTRY -> A -> B -> C -> EXIT.  */

static void
test_linear_chain ()
{
  gimple_register_cfg_hooks ();

  tree fndecl = push_fndecl ("cfg_test_linear_chain");
  function *fun = DECL_STRUCT_FUNCTION (fndecl);

  /* Create some empty blocks.  */
  basic_block bb_a = create_empty_bb (ENTRY_BLOCK_PTR_FOR_FN (fun));
  basic_block bb_b = create_empty_bb (bb_a);
  basic_block bb_c = create_empty_bb (bb_b);

  ASSERT_EQ (5, n_basic_blocks_for_fn (fun));
  ASSERT_EQ (0, n_edges_for_fn (fun));

  /* Create some edges: a simple linear chain of BBs.  */
  make_edge (ENTRY_BLOCK_PTR_FOR_FN (fun), bb_a, EDGE_FALLTHRU);
  make_edge (bb_a, bb_b, 0);
  make_edge (bb_b, bb_c, 0);
  make_edge (bb_c, EXIT_BLOCK_PTR_FOR_FN (fun), 0);

  /* Verify the edges.  */
  ASSERT_EQ (4, n_edges_for_fn (fun));
  ASSERT_EQ (NULL, ENTRY_BLOCK_PTR_FOR_FN (fun)->preds);
  ASSERT_EQ (1, ENTRY_BLOCK_PTR_FOR_FN (fun)->succs->length ());
  ASSERT_EQ (1, bb_a->preds->length ());
  ASSERT_EQ (1, bb_a->succs->length ());
  ASSERT_EQ (1, bb_b->preds->length ());
  ASSERT_EQ (1, bb_b->succs->length ());
  ASSERT_EQ (1, bb_c->preds->length ());
  ASSERT_EQ (1, bb_c->succs->length ());
  ASSERT_EQ (1, EXIT_BLOCK_PTR_FOR_FN (fun)->preds->length ());
  ASSERT_EQ (NULL, EXIT_BLOCK_PTR_FOR_FN (fun)->succs);

  /* Verify the dominance information
     Each BB in our simple chain should be dominated by the one before
     it.  */
  calculate_dominance_info (CDI_DOMINATORS);
  ASSERT_EQ (bb_a, get_immediate_dominator (CDI_DOMINATORS, bb_b));
  ASSERT_EQ (bb_b, get_immediate_dominator (CDI_DOMINATORS, bb_c));
  vec<basic_block> dom_by_b = get_dominated_by (CDI_DOMINATORS, bb_b);
  ASSERT_EQ (1, dom_by_b.length ());
  ASSERT_EQ (bb_c, dom_by_b[0]);
  free_dominance_info (CDI_DOMINATORS);
  dom_by_b.release ();

  /* Similarly for post-dominance: each BB in our chain is post-dominated
     by the one after it.  */
  calculate_dominance_info (CDI_POST_DOMINATORS);
  ASSERT_EQ (bb_b, get_immediate_dominator (CDI_POST_DOMINATORS, bb_a));
  ASSERT_EQ (bb_c, get_immediate_dominator (CDI_POST_DOMINATORS, bb_b));
  vec<basic_block> postdom_by_b = get_dominated_by (CDI_POST_DOMINATORS, bb_b);
  ASSERT_EQ (1, postdom_by_b.length ());
  ASSERT_EQ (bb_a, postdom_by_b[0]);
  free_dominance_info (CDI_POST_DOMINATORS);
  postdom_by_b.release ();

  pop_cfun ();
}

/* Verify a simple CFG of the form:
     ENTRY
       |
       A
      / \
     /t  \f
    B     C
     \   /
      \ /
       D
       |
      EXIT.  */

static void
test_diamond ()
{
  gimple_register_cfg_hooks ();

  tree fndecl = push_fndecl ("cfg_test_diamond");
  function *fun = DECL_STRUCT_FUNCTION (fndecl);

  /* Create some empty blocks.  */
  basic_block bb_a = create_empty_bb (ENTRY_BLOCK_PTR_FOR_FN (fun));
  basic_block bb_b = create_empty_bb (bb_a);
  basic_block bb_c = create_empty_bb (bb_a);
  basic_block bb_d = create_empty_bb (bb_b);

  ASSERT_EQ (6, n_basic_blocks_for_fn (fun));
  ASSERT_EQ (0, n_edges_for_fn (fun));

  /* Create the edges.  */
  make_edge (ENTRY_BLOCK_PTR_FOR_FN (fun), bb_a, EDGE_FALLTHRU);
  make_edge (bb_a, bb_b, EDGE_TRUE_VALUE);
  make_edge (bb_a, bb_c, EDGE_FALSE_VALUE);
  make_edge (bb_b, bb_d, 0);
  make_edge (bb_c, bb_d, 0);
  make_edge (bb_d, EXIT_BLOCK_PTR_FOR_FN (fun), 0);

  /* Verify the edges.  */
  ASSERT_EQ (6, n_edges_for_fn (fun));
  ASSERT_EQ (1, bb_a->preds->length ());
  ASSERT_EQ (2, bb_a->succs->length ());
  ASSERT_EQ (1, bb_b->preds->length ());
  ASSERT_EQ (1, bb_b->succs->length ());
  ASSERT_EQ (1, bb_c->preds->length ());
  ASSERT_EQ (1, bb_c->succs->length ());
  ASSERT_EQ (2, bb_d->preds->length ());
  ASSERT_EQ (1, bb_d->succs->length ());

  /* Verify the dominance information.  */
  calculate_dominance_info (CDI_DOMINATORS);
  ASSERT_EQ (bb_a, get_immediate_dominator (CDI_DOMINATORS, bb_b));
  ASSERT_EQ (bb_a, get_immediate_dominator (CDI_DOMINATORS, bb_c));
  ASSERT_EQ (bb_a, get_immediate_dominator (CDI_DOMINATORS, bb_d));
  vec<basic_block> dom_by_a = get_dominated_by (CDI_DOMINATORS, bb_a);
  ASSERT_EQ (3, dom_by_a.length ()); /* B, C, D, in some order.  */
  dom_by_a.release ();
  vec<basic_block> dom_by_b = get_dominated_by (CDI_DOMINATORS, bb_b);
  ASSERT_EQ (0, dom_by_b.length ());
  dom_by_b.release ();
  free_dominance_info (CDI_DOMINATORS);

  /* Similarly for post-dominance.  */
  calculate_dominance_info (CDI_POST_DOMINATORS);
  ASSERT_EQ (bb_d, get_immediate_dominator (CDI_POST_DOMINATORS, bb_a));
  ASSERT_EQ (bb_d, get_immediate_dominator (CDI_POST_DOMINATORS, bb_b));
  ASSERT_EQ (bb_d, get_immediate_dominator (CDI_POST_DOMINATORS, bb_c));
  vec<basic_block> postdom_by_d = get_dominated_by (CDI_POST_DOMINATORS, bb_d);
  ASSERT_EQ (3, postdom_by_d.length ()); /* A, B, C in some order.  */
  postdom_by_d.release ();
  vec<basic_block> postdom_by_b = get_dominated_by (CDI_POST_DOMINATORS, bb_b);
  ASSERT_EQ (0, postdom_by_b.length ());
  postdom_by_b.release ();
  free_dominance_info (CDI_POST_DOMINATORS);

  pop_cfun ();
}

/* Verify that we can handle a CFG containing a "complete" aka
   fully-connected subgraph (where A B C D below all have edges
   pointing to each other node, also to themselves).
   e.g.:
     ENTRY  EXIT
       |    ^
       |   /
       |  /
       | /
       V/
       A<--->B
       ^^   ^^
       | \ / |
       |  X  |
       | / \ |
       VV   VV
       C<--->D
*/

static void
test_fully_connected ()
{
  gimple_register_cfg_hooks ();

  tree fndecl = push_fndecl ("cfg_fully_connected");
  function *fun = DECL_STRUCT_FUNCTION (fndecl);

  const int n = 4;

  /* Create some empty blocks.  */
  auto_vec <basic_block> subgraph_nodes;
  for (int i = 0; i < n; i++)
    subgraph_nodes.safe_push (create_empty_bb (ENTRY_BLOCK_PTR_FOR_FN (fun)));

  ASSERT_EQ (n + 2, n_basic_blocks_for_fn (fun));
  ASSERT_EQ (0, n_edges_for_fn (fun));

  /* Create the edges.  */
  make_edge (ENTRY_BLOCK_PTR_FOR_FN (fun), subgraph_nodes[0], EDGE_FALLTHRU);
  make_edge (subgraph_nodes[0], EXIT_BLOCK_PTR_FOR_FN (fun), 0);
  for (int i = 0; i < n; i++)
    for (int j = 0; j < n; j++)
      make_edge (subgraph_nodes[i], subgraph_nodes[j], 0);

  /* Verify the edges.  */
  ASSERT_EQ (2 + (n * n), n_edges_for_fn (fun));
  /* The first one is linked to ENTRY/EXIT as well as itself and
     everything else.  */
  ASSERT_EQ (n + 1, subgraph_nodes[0]->preds->length ());
  ASSERT_EQ (n + 1, subgraph_nodes[0]->succs->length ());
  /* The other ones in the subgraph are linked to everything in
     the subgraph (including themselves).  */
  for (int i = 1; i < n; i++)
    {
      ASSERT_EQ (n, subgraph_nodes[i]->preds->length ());
      ASSERT_EQ (n, subgraph_nodes[i]->succs->length ());
    }

  /* Verify the dominance information.  */
  calculate_dominance_info (CDI_DOMINATORS);
  /* The initial block in the subgraph should be dominated by ENTRY.  */
  ASSERT_EQ (ENTRY_BLOCK_PTR_FOR_FN (fun),
	     get_immediate_dominator (CDI_DOMINATORS,
				      subgraph_nodes[0]));
  /* Every other block in the subgraph should be dominated by the
     initial block.  */
  for (int i = 1; i < n; i++)
    ASSERT_EQ (subgraph_nodes[0],
	       get_immediate_dominator (CDI_DOMINATORS,
					subgraph_nodes[i]));
  free_dominance_info (CDI_DOMINATORS);

  /* Similarly for post-dominance.  */
  calculate_dominance_info (CDI_POST_DOMINATORS);
  /* The initial block in the subgraph should be postdominated by EXIT.  */
  ASSERT_EQ (EXIT_BLOCK_PTR_FOR_FN (fun),
	     get_immediate_dominator (CDI_POST_DOMINATORS,
				      subgraph_nodes[0]));
  /* Every other block in the subgraph should be postdominated by the
     initial block, since that leads to EXIT.  */
  for (int i = 1; i < n; i++)
    ASSERT_EQ (subgraph_nodes[0],
	       get_immediate_dominator (CDI_POST_DOMINATORS,
					subgraph_nodes[i]));
  free_dominance_info (CDI_POST_DOMINATORS);

  pop_cfun ();
}

/* Run all of the selftests within this file.  */

void
tree_cfg_c_tests ()
{
  test_linear_chain ();
  test_diamond ();
  test_fully_connected ();
}

} // namespace selftest

/* TODO: test the dominator/postdominator logic with various graphs/nodes:
   - loop
   - nested loops
   - switch statement (a block with many out-edges)
   - something that jumps to itself
   - etc  */

#endif /* CHECKING_P */
