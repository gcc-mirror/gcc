/* Exception handling semantics and decomposition for trees.
   Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009
   Free Software Foundation, Inc.

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
#include "tm.h"
#include "tree.h"
#include "rtl.h"
#include "tm_p.h"
#include "flags.h"
#include "function.h"
#include "except.h"
#include "tree-flow.h"
#include "tree-dump.h"
#include "tree-inline.h"
#include "tree-iterator.h"
#include "tree-pass.h"
#include "timevar.h"
#include "langhooks.h"
#include "ggc.h"
#include "toplev.h"
#include "gimple.h"
#include "target.h"

/* In some instances a tree and a gimple need to be stored in a same table,
   i.e. in hash tables. This is a structure to do this. */
typedef union {tree *tp; tree t; gimple g;} treemple;

/* Nonzero if we are using EH to handle cleanups.  */
static int using_eh_for_cleanups_p = 0;

void
using_eh_for_cleanups (void)
{
  using_eh_for_cleanups_p = 1;
}

/* Misc functions used in this file.  */

/* Compare and hash for any structure which begins with a canonical
   pointer.  Assumes all pointers are interchangeable, which is sort
   of already assumed by gcc elsewhere IIRC.  */

static int
struct_ptr_eq (const void *a, const void *b)
{
  const void * const * x = (const void * const *) a;
  const void * const * y = (const void * const *) b;
  return *x == *y;
}

static hashval_t
struct_ptr_hash (const void *a)
{
  const void * const * x = (const void * const *) a;
  return (size_t)*x >> 4;
}


/* Remember and lookup EH landing pad data for arbitrary statements.
   Really this means any statement that could_throw_p.  We could
   stuff this information into the stmt_ann data structure, but:

   (1) We absolutely rely on this information being kept until
   we get to rtl.  Once we're done with lowering here, if we lose
   the information there's no way to recover it!

   (2) There are many more statements that *cannot* throw as
   compared to those that can.  We should be saving some amount
   of space by only allocating memory for those that can throw.  */

/* Add statement T in function IFUN to landing pad NUM.  */

void
add_stmt_to_eh_lp_fn (struct function *ifun, gimple t, int num)
{
  struct throw_stmt_node *n;
  void **slot;

  gcc_assert (num != 0);

  n = GGC_NEW (struct throw_stmt_node);
  n->stmt = t;
  n->lp_nr = num;

  if (!get_eh_throw_stmt_table (ifun))
    set_eh_throw_stmt_table (ifun, htab_create_ggc (31, struct_ptr_hash,
						    struct_ptr_eq,
						    ggc_free));

  slot = htab_find_slot (get_eh_throw_stmt_table (ifun), n, INSERT);
  gcc_assert (!*slot);
  *slot = n;
}

/* Add statement T in the current function (cfun) to EH landing pad NUM.  */

void
add_stmt_to_eh_lp (gimple t, int num)
{
  add_stmt_to_eh_lp_fn (cfun, t, num);
}

/* Add statement T to the single EH landing pad in REGION.  */

static void
record_stmt_eh_region (eh_region region, gimple t)
{
  if (region == NULL)
    return;
  if (region->type == ERT_MUST_NOT_THROW)
    add_stmt_to_eh_lp_fn (cfun, t, -region->index);
  else
    {
      eh_landing_pad lp = region->landing_pads;
      if (lp == NULL)
	lp = gen_eh_landing_pad (region);
      else
	gcc_assert (lp->next_lp == NULL);
      add_stmt_to_eh_lp_fn (cfun, t, lp->index);
    }
}


/* Remove statement T in function IFUN from its EH landing pad.  */

bool
remove_stmt_from_eh_lp_fn (struct function *ifun, gimple t)
{
  struct throw_stmt_node dummy;
  void **slot;

  if (!get_eh_throw_stmt_table (ifun))
    return false;

  dummy.stmt = t;
  slot = htab_find_slot (get_eh_throw_stmt_table (ifun), &dummy,
                        NO_INSERT);
  if (slot)
    {
      htab_clear_slot (get_eh_throw_stmt_table (ifun), slot);
      return true;
    }
  else
    return false;
}


/* Remove statement T in the current function (cfun) from its
   EH landing pad.  */

bool
remove_stmt_from_eh_lp (gimple t)
{
  return remove_stmt_from_eh_lp_fn (cfun, t);
}

/* Determine if statement T is inside an EH region in function IFUN.
   Positive numbers indicate a landing pad index; negative numbers
   indicate a MUST_NOT_THROW region index; zero indicates that the
   statement is not recorded in the region table.  */

int
lookup_stmt_eh_lp_fn (struct function *ifun, gimple t)
{
  struct throw_stmt_node *p, n;

  if (ifun->eh->throw_stmt_table == NULL)
    return 0;

  n.stmt = t;
  p = (struct throw_stmt_node *) htab_find (ifun->eh->throw_stmt_table, &n);
  return p ? p->lp_nr : 0;
}

/* Likewise, but always use the current function.  */

int
lookup_stmt_eh_lp (gimple t)
{
  /* We can get called from initialized data when -fnon-call-exceptions
     is on; prevent crash.  */
  if (!cfun)
    return 0;
  return lookup_stmt_eh_lp_fn (cfun, t);
}

/* First pass of EH node decomposition.  Build up a tree of GIMPLE_TRY_FINALLY
   nodes and LABEL_DECL nodes.  We will use this during the second phase to
   determine if a goto leaves the body of a TRY_FINALLY_EXPR node.  */

struct finally_tree_node
{
  /* When storing a GIMPLE_TRY, we have to record a gimple.  However
     when deciding whether a GOTO to a certain LABEL_DECL (which is a
     tree) leaves the TRY block, its necessary to record a tree in
     this field.  Thus a treemple is used. */
  treemple child;
  gimple parent;
};

/* Note that this table is *not* marked GTY.  It is short-lived.  */
static htab_t finally_tree;

static void
record_in_finally_tree (treemple child, gimple parent)
{
  struct finally_tree_node *n;
  void **slot;

  n = XNEW (struct finally_tree_node);
  n->child = child;
  n->parent = parent;

  slot = htab_find_slot (finally_tree, n, INSERT);
  gcc_assert (!*slot);
  *slot = n;
}

static void
collect_finally_tree (gimple stmt, gimple region);

/* Go through the gimple sequence.  Works with collect_finally_tree to
   record all GIMPLE_LABEL and GIMPLE_TRY statements. */

static void
collect_finally_tree_1 (gimple_seq seq, gimple region)
{
  gimple_stmt_iterator gsi;

  for (gsi = gsi_start (seq); !gsi_end_p (gsi); gsi_next (&gsi))
    collect_finally_tree (gsi_stmt (gsi), region);
}

static void
collect_finally_tree (gimple stmt, gimple region)
{
  treemple temp;

  switch (gimple_code (stmt))
    {
    case GIMPLE_LABEL:
      temp.t = gimple_label_label (stmt);
      record_in_finally_tree (temp, region);
      break;

    case GIMPLE_TRY:
      if (gimple_try_kind (stmt) == GIMPLE_TRY_FINALLY)
        {
          temp.g = stmt;
          record_in_finally_tree (temp, region);
          collect_finally_tree_1 (gimple_try_eval (stmt), stmt);
	  collect_finally_tree_1 (gimple_try_cleanup (stmt), region);
        }
      else if (gimple_try_kind (stmt) == GIMPLE_TRY_CATCH)
        {
          collect_finally_tree_1 (gimple_try_eval (stmt), region);
          collect_finally_tree_1 (gimple_try_cleanup (stmt), region);
        }
      break;

    case GIMPLE_CATCH:
      collect_finally_tree_1 (gimple_catch_handler (stmt), region);
      break;

    case GIMPLE_EH_FILTER:
      collect_finally_tree_1 (gimple_eh_filter_failure (stmt), region);
      break;

    default:
      /* A type, a decl, or some kind of statement that we're not
	 interested in.  Don't walk them.  */
      break;
    }
}


/* Use the finally tree to determine if a jump from START to TARGET
   would leave the try_finally node that START lives in.  */

static bool
outside_finally_tree (treemple start, gimple target)
{
  struct finally_tree_node n, *p;

  do
    {
      n.child = start;
      p = (struct finally_tree_node *) htab_find (finally_tree, &n);
      if (!p)
	return true;
      start.g = p->parent;
    }
  while (start.g != target);

  return false;
}

/* Second pass of EH node decomposition.  Actually transform the GIMPLE_TRY
   nodes into a set of gotos, magic labels, and eh regions.
   The eh region creation is straight-forward, but frobbing all the gotos
   and such into shape isn't.  */

/* The sequence into which we record all EH stuff.  This will be
   placed at the end of the function when we're all done.  */
static gimple_seq eh_seq;

/* Record whether an EH region contains something that can throw,
   indexed by EH region number.  */
static bitmap eh_region_may_contain_throw_map;

/* The GOTO_QUEUE is is an array of GIMPLE_GOTO and GIMPLE_RETURN
   statements that are seen to escape this GIMPLE_TRY_FINALLY node.
   The idea is to record a gimple statement for everything except for
   the conditionals, which get their labels recorded. Since labels are
   of type 'tree', we need this node to store both gimple and tree
   objects.  REPL_STMT is the sequence used to replace the goto/return
   statement.  CONT_STMT is used to store the statement that allows
   the return/goto to jump to the original destination. */

struct goto_queue_node
{
  treemple stmt;
  gimple_seq repl_stmt;
  gimple cont_stmt;
  int index;
  /* This is used when index >= 0 to indicate that stmt is a label (as
     opposed to a goto stmt).  */
  int is_label;
};

/* State of the world while lowering.  */

struct leh_state
{
  /* What's "current" while constructing the eh region tree.  These
     correspond to variables of the same name in cfun->eh, which we
     don't have easy access to.  */
  eh_region cur_region;

  /* What's "current" for the purposes of __builtin_eh_pointer.  For
     a CATCH, this is the associated TRY.  For an EH_FILTER, this is
     the associated ALLOWED_EXCEPTIONS, etc.  */
  eh_region ehp_region;

  /* Processing of TRY_FINALLY requires a bit more state.  This is
     split out into a separate structure so that we don't have to
     copy so much when processing other nodes.  */
  struct leh_tf_state *tf;
};

struct leh_tf_state
{
  /* Pointer to the GIMPLE_TRY_FINALLY node under discussion.  The
     try_finally_expr is the original GIMPLE_TRY_FINALLY.  We need to retain
     this so that outside_finally_tree can reliably reference the tree used
     in the collect_finally_tree data structures.  */
  gimple try_finally_expr;
  gimple top_p;

  /* While lowering a top_p usually it is expanded into multiple statements,
     thus we need the following field to store them. */
  gimple_seq top_p_seq;

  /* The state outside this try_finally node.  */
  struct leh_state *outer;

  /* The exception region created for it.  */
  eh_region region;

  /* The goto queue.  */
  struct goto_queue_node *goto_queue;
  size_t goto_queue_size;
  size_t goto_queue_active;

  /* Pointer map to help in searching goto_queue when it is large.  */
  struct pointer_map_t *goto_queue_map;

  /* The set of unique labels seen as entries in the goto queue.  */
  VEC(tree,heap) *dest_array;

  /* A label to be added at the end of the completed transformed
     sequence.  It will be set if may_fallthru was true *at one time*,
     though subsequent transformations may have cleared that flag.  */
  tree fallthru_label;

  /* True if it is possible to fall out the bottom of the try block.
     Cleared if the fallthru is converted to a goto.  */
  bool may_fallthru;

  /* True if any entry in goto_queue is a GIMPLE_RETURN.  */
  bool may_return;

  /* True if the finally block can receive an exception edge.
     Cleared if the exception case is handled by code duplication.  */
  bool may_throw;
};

static gimple_seq lower_eh_must_not_throw (struct leh_state *, gimple);

/* Search for STMT in the goto queue.  Return the replacement,
   or null if the statement isn't in the queue.  */

#define LARGE_GOTO_QUEUE 20

static void lower_eh_constructs_1 (struct leh_state *state, gimple_seq seq);

static gimple_seq
find_goto_replacement (struct leh_tf_state *tf, treemple stmt)
{
  unsigned int i;
  void **slot;

  if (tf->goto_queue_active < LARGE_GOTO_QUEUE)
    {
      for (i = 0; i < tf->goto_queue_active; i++)
	if ( tf->goto_queue[i].stmt.g == stmt.g)
	  return tf->goto_queue[i].repl_stmt;
      return NULL;
    }

  /* If we have a large number of entries in the goto_queue, create a
     pointer map and use that for searching.  */

  if (!tf->goto_queue_map)
    {
      tf->goto_queue_map = pointer_map_create ();
      for (i = 0; i < tf->goto_queue_active; i++)
	{
	  slot = pointer_map_insert (tf->goto_queue_map,
                                     tf->goto_queue[i].stmt.g);
          gcc_assert (*slot == NULL);
	  *slot = &tf->goto_queue[i];
	}
    }

  slot = pointer_map_contains (tf->goto_queue_map, stmt.g);
  if (slot != NULL)
    return (((struct goto_queue_node *) *slot)->repl_stmt);

  return NULL;
}

/* A subroutine of replace_goto_queue_1.  Handles the sub-clauses of a
   lowered GIMPLE_COND.  If, by chance, the replacement is a simple goto,
   then we can just splat it in, otherwise we add the new stmts immediately
   after the GIMPLE_COND and redirect.  */

static void
replace_goto_queue_cond_clause (tree *tp, struct leh_tf_state *tf,
				gimple_stmt_iterator *gsi)
{
  tree label;
  gimple_seq new_seq;
  treemple temp;
  location_t loc = gimple_location (gsi_stmt (*gsi));

  temp.tp = tp;
  new_seq = find_goto_replacement (tf, temp);
  if (!new_seq)
    return;

  if (gimple_seq_singleton_p (new_seq)
      && gimple_code (gimple_seq_first_stmt (new_seq)) == GIMPLE_GOTO)
    {
      *tp = gimple_goto_dest (gimple_seq_first_stmt (new_seq));
      return;
    }

  label = create_artificial_label (loc);
  /* Set the new label for the GIMPLE_COND */
  *tp = label;

  gsi_insert_after (gsi, gimple_build_label (label), GSI_CONTINUE_LINKING);
  gsi_insert_seq_after (gsi, gimple_seq_copy (new_seq), GSI_CONTINUE_LINKING);
}

/* The real work of replace_goto_queue.  Returns with TSI updated to
   point to the next statement.  */

static void replace_goto_queue_stmt_list (gimple_seq, struct leh_tf_state *);

static void
replace_goto_queue_1 (gimple stmt, struct leh_tf_state *tf,
		      gimple_stmt_iterator *gsi)
{
  gimple_seq seq;
  treemple temp;
  temp.g = NULL;

  switch (gimple_code (stmt))
    {
    case GIMPLE_GOTO:
    case GIMPLE_RETURN:
      temp.g = stmt;
      seq = find_goto_replacement (tf, temp);
      if (seq)
	{
	  gsi_insert_seq_before (gsi, gimple_seq_copy (seq), GSI_SAME_STMT);
	  gsi_remove (gsi, false);
	  return;
	}
      break;

    case GIMPLE_COND:
      replace_goto_queue_cond_clause (gimple_op_ptr (stmt, 2), tf, gsi);
      replace_goto_queue_cond_clause (gimple_op_ptr (stmt, 3), tf, gsi);
      break;

    case GIMPLE_TRY:
      replace_goto_queue_stmt_list (gimple_try_eval (stmt), tf);
      replace_goto_queue_stmt_list (gimple_try_cleanup (stmt), tf);
      break;
    case GIMPLE_CATCH:
      replace_goto_queue_stmt_list (gimple_catch_handler (stmt), tf);
      break;
    case GIMPLE_EH_FILTER:
      replace_goto_queue_stmt_list (gimple_eh_filter_failure (stmt), tf);
      break;

    default:
      /* These won't have gotos in them.  */
      break;
    }

  gsi_next (gsi);
}

/* A subroutine of replace_goto_queue.  Handles GIMPLE_SEQ.  */

static void
replace_goto_queue_stmt_list (gimple_seq seq, struct leh_tf_state *tf)
{
  gimple_stmt_iterator gsi = gsi_start (seq);

  while (!gsi_end_p (gsi))
    replace_goto_queue_1 (gsi_stmt (gsi), tf, &gsi);
}

/* Replace all goto queue members.  */

static void
replace_goto_queue (struct leh_tf_state *tf)
{
  if (tf->goto_queue_active == 0)
    return;
  replace_goto_queue_stmt_list (tf->top_p_seq, tf);
}

/* Add a new record to the goto queue contained in TF. NEW_STMT is the
   data to be added, IS_LABEL indicates whether NEW_STMT is a label or
   a gimple return. */

static void
record_in_goto_queue (struct leh_tf_state *tf,
                      treemple new_stmt,
                      int index,
                      bool is_label)
{
  size_t active, size;
  struct goto_queue_node *q;

  gcc_assert (!tf->goto_queue_map);

  active = tf->goto_queue_active;
  size = tf->goto_queue_size;
  if (active >= size)
    {
      size = (size ? size * 2 : 32);
      tf->goto_queue_size = size;
      tf->goto_queue
         = XRESIZEVEC (struct goto_queue_node, tf->goto_queue, size);
    }

  q = &tf->goto_queue[active];
  tf->goto_queue_active = active + 1;

  memset (q, 0, sizeof (*q));
  q->stmt = new_stmt;
  q->index = index;
  q->is_label = is_label;
}

/* Record the LABEL label in the goto queue contained in TF.
   TF is not null.  */

static void
record_in_goto_queue_label (struct leh_tf_state *tf, treemple stmt, tree label)
{
  int index;
  treemple temp, new_stmt;

  if (!label)
    return;

  /* Computed and non-local gotos do not get processed.  Given
     their nature we can neither tell whether we've escaped the
     finally block nor redirect them if we knew.  */
  if (TREE_CODE (label) != LABEL_DECL)
    return;

  /* No need to record gotos that don't leave the try block.  */
  temp.t = label;
  if (!outside_finally_tree (temp, tf->try_finally_expr))
    return;

  if (! tf->dest_array)
    {
      tf->dest_array = VEC_alloc (tree, heap, 10);
      VEC_quick_push (tree, tf->dest_array, label);
      index = 0;
    }
  else
    {
      int n = VEC_length (tree, tf->dest_array);
      for (index = 0; index < n; ++index)
        if (VEC_index (tree, tf->dest_array, index) == label)
          break;
      if (index == n)
        VEC_safe_push (tree, heap, tf->dest_array, label);
    }

  /* In the case of a GOTO we want to record the destination label,
     since with a GIMPLE_COND we have an easy access to the then/else
     labels. */
  new_stmt = stmt;
  record_in_goto_queue (tf, new_stmt, index, true);

}

/* For any GIMPLE_GOTO or GIMPLE_RETURN, decide whether it leaves a try_finally
   node, and if so record that fact in the goto queue associated with that
   try_finally node.  */

static void
maybe_record_in_goto_queue (struct leh_state *state, gimple stmt)
{
  struct leh_tf_state *tf = state->tf;
  treemple new_stmt;

  if (!tf)
    return;

  switch (gimple_code (stmt))
    {
    case GIMPLE_COND:
      new_stmt.tp = gimple_op_ptr (stmt, 2);
      record_in_goto_queue_label (tf, new_stmt, gimple_cond_true_label (stmt));
      new_stmt.tp = gimple_op_ptr (stmt, 3);
      record_in_goto_queue_label (tf, new_stmt, gimple_cond_false_label (stmt));
      break;
    case GIMPLE_GOTO:
      new_stmt.g = stmt;
      record_in_goto_queue_label (tf, new_stmt, gimple_goto_dest (stmt));
      break;

    case GIMPLE_RETURN:
      tf->may_return = true;
      new_stmt.g = stmt;
      record_in_goto_queue (tf, new_stmt, -1, false);
      break;

    default:
      gcc_unreachable ();
    }
}


#ifdef ENABLE_CHECKING
/* We do not process GIMPLE_SWITCHes for now.  As long as the original source
   was in fact structured, and we've not yet done jump threading, then none
   of the labels will leave outer GIMPLE_TRY_FINALLY nodes. Verify this.  */

static void
verify_norecord_switch_expr (struct leh_state *state, gimple switch_expr)
{
  struct leh_tf_state *tf = state->tf;
  size_t i, n;

  if (!tf)
    return;

  n = gimple_switch_num_labels (switch_expr);

  for (i = 0; i < n; ++i)
    {
      treemple temp;
      tree lab = CASE_LABEL (gimple_switch_label (switch_expr, i));
      temp.t = lab;
      gcc_assert (!outside_finally_tree (temp, tf->try_finally_expr));
    }
}
#else
#define verify_norecord_switch_expr(state, switch_expr)
#endif

/* Redirect a RETURN_EXPR pointed to by STMT_P to FINLAB.  Place in CONT_P
   whatever is needed to finish the return.  If MOD is non-null, insert it
   before the new branch.  RETURN_VALUE_P is a cache containing a temporary
   variable to be used in manipulating the value returned from the function.  */

static void
do_return_redirection (struct goto_queue_node *q, tree finlab, gimple_seq mod,
		       tree *return_value_p)
{
  tree ret_expr;
  gimple x;

  /* In the case of a return, the queue node must be a gimple statement. */
  gcc_assert (!q->is_label);

  ret_expr = gimple_return_retval (q->stmt.g);

  if (ret_expr)
    {
      if (!*return_value_p)
        *return_value_p = ret_expr;
      else
        gcc_assert (*return_value_p == ret_expr);
      q->cont_stmt = q->stmt.g;
      /* The nasty part about redirecting the return value is that the
	 return value itself is to be computed before the FINALLY block
	 is executed.  e.g.

		int x;
		int foo (void)
		{
		  x = 0;
		  try {
		    return x;
		  } finally {
		    x++;
		  }
		}

	  should return 0, not 1.  Arrange for this to happen by copying
	  computed the return value into a local temporary.  This also
	  allows us to redirect multiple return statements through the
	  same destination block; whether this is a net win or not really
	  depends, I guess, but it does make generation of the switch in
	  lower_try_finally_switch easier.  */

      if (TREE_CODE (ret_expr) == RESULT_DECL)
	{
	  if (!*return_value_p)
	    *return_value_p = ret_expr;
	  else
	    gcc_assert (*return_value_p == ret_expr);
	  q->cont_stmt = q->stmt.g;
	}
      else
	  gcc_unreachable ();
    }
  else
      /* If we don't return a value, all return statements are the same.  */
      q->cont_stmt = q->stmt.g;

  if (!q->repl_stmt)
    q->repl_stmt = gimple_seq_alloc ();

  if (mod)
    gimple_seq_add_seq (&q->repl_stmt, mod);

  x = gimple_build_goto (finlab);
  gimple_seq_add_stmt (&q->repl_stmt, x);
}

/* Similar, but easier, for GIMPLE_GOTO.  */

static void
do_goto_redirection (struct goto_queue_node *q, tree finlab, gimple_seq mod,
		     struct leh_tf_state *tf)
{
  gimple x;

  gcc_assert (q->is_label);
  if (!q->repl_stmt)
    q->repl_stmt = gimple_seq_alloc ();

  q->cont_stmt = gimple_build_goto (VEC_index (tree, tf->dest_array, q->index));

  if (mod)
    gimple_seq_add_seq (&q->repl_stmt, mod);

  x = gimple_build_goto (finlab);
  gimple_seq_add_stmt (&q->repl_stmt, x);
}

/* Emit a standard landing pad sequence into SEQ for REGION.  */

static void
emit_post_landing_pad (gimple_seq *seq, eh_region region)
{
  eh_landing_pad lp = region->landing_pads;
  gimple x;

  if (lp == NULL)
    lp = gen_eh_landing_pad (region);

  lp->post_landing_pad = create_artificial_label (UNKNOWN_LOCATION);
  EH_LANDING_PAD_NR (lp->post_landing_pad) = lp->index;

  x = gimple_build_label (lp->post_landing_pad);
  gimple_seq_add_stmt (seq, x);
}

/* Emit a RESX statement into SEQ for REGION.  */

static void
emit_resx (gimple_seq *seq, eh_region region)
{
  gimple x = gimple_build_resx (region->index);
  gimple_seq_add_stmt (seq, x);
  if (region->outer)
    record_stmt_eh_region (region->outer, x);
}

/* Emit an EH_DISPATCH statement into SEQ for REGION.  */

static void
emit_eh_dispatch (gimple_seq *seq, eh_region region)
{
  gimple x = gimple_build_eh_dispatch (region->index);
  gimple_seq_add_stmt (seq, x);
}

/* Note that the current EH region may contain a throw, or a
   call to a function which itself may contain a throw.  */

static void
note_eh_region_may_contain_throw (eh_region region)
{
  while (!bitmap_bit_p (eh_region_may_contain_throw_map, region->index))
    {
      bitmap_set_bit (eh_region_may_contain_throw_map, region->index);
      region = region->outer;
      if (region == NULL)
	break;
    }
}

/* Check if REGION has been marked as containing a throw.  If REGION is
   NULL, this predicate is false.  */

static inline bool
eh_region_may_contain_throw (eh_region r)
{
  return r && bitmap_bit_p (eh_region_may_contain_throw_map, r->index);
}

/* We want to transform
	try { body; } catch { stuff; }
   to
	normal_seqence:
	  body;
	  over:
	eh_seqence:
	  landing_pad:
	  stuff;
	  goto over;

   TP is a GIMPLE_TRY node.  REGION is the region whose post_landing_pad
   should be placed before the second operand, or NULL.  OVER is
   an existing label that should be put at the exit, or NULL.  */

static gimple_seq
frob_into_branch_around (gimple tp, eh_region region, tree over)
{
  gimple x;
  gimple_seq cleanup, result;
  location_t loc = gimple_location (tp);

  cleanup = gimple_try_cleanup (tp);
  result = gimple_try_eval (tp);

  if (region)
    emit_post_landing_pad (&eh_seq, region);

  if (gimple_seq_may_fallthru (cleanup))
    {
      if (!over)
	over = create_artificial_label (loc);
      x = gimple_build_goto (over);
      gimple_seq_add_stmt (&cleanup, x);
    }
  gimple_seq_add_seq (&eh_seq, cleanup);

  if (over)
    {
      x = gimple_build_label (over);
      gimple_seq_add_stmt (&result, x);
    }
  return result;
}

/* A subroutine of lower_try_finally.  Duplicate the tree rooted at T.
   Make sure to record all new labels found.  */

static gimple_seq
lower_try_finally_dup_block (gimple_seq seq, struct leh_state *outer_state)
{
  gimple region = NULL;
  gimple_seq new_seq;

  new_seq = copy_gimple_seq_and_replace_locals (seq);

  if (outer_state->tf)
    region = outer_state->tf->try_finally_expr;
  collect_finally_tree_1 (new_seq, region);

  return new_seq;
}

/* A subroutine of lower_try_finally.  Create a fallthru label for
   the given try_finally state.  The only tricky bit here is that
   we have to make sure to record the label in our outer context.  */

static tree
lower_try_finally_fallthru_label (struct leh_tf_state *tf)
{
  tree label = tf->fallthru_label;
  treemple temp;

  if (!label)
    {
      label = create_artificial_label (gimple_location (tf->try_finally_expr));
      tf->fallthru_label = label;
      if (tf->outer->tf)
        {
          temp.t = label;
          record_in_finally_tree (temp, tf->outer->tf->try_finally_expr);
        }
    }
  return label;
}

/* A subroutine of lower_try_finally.  If lang_protect_cleanup_actions
   returns non-null, then the language requires that the exception path out
   of a try_finally be treated specially.  To wit: the code within the
   finally block may not itself throw an exception.  We have two choices here.
   First we can duplicate the finally block and wrap it in a must_not_throw
   region.  Second, we can generate code like

	try {
	  finally_block;
	} catch {
	  if (fintmp == eh_edge)
	    protect_cleanup_actions;
	}

   where "fintmp" is the temporary used in the switch statement generation
   alternative considered below.  For the nonce, we always choose the first
   option.

   THIS_STATE may be null if this is a try-cleanup, not a try-finally.  */

static void
honor_protect_cleanup_actions (struct leh_state *outer_state,
			       struct leh_state *this_state,
			       struct leh_tf_state *tf)
{
  tree protect_cleanup_actions;
  gimple_stmt_iterator gsi;
  bool finally_may_fallthru;
  gimple_seq finally;
  gimple x;

  /* First check for nothing to do.  */
  if (lang_protect_cleanup_actions == NULL)
    return;
  protect_cleanup_actions = lang_protect_cleanup_actions ();
  if (protect_cleanup_actions == NULL)
    return;

  finally = gimple_try_cleanup (tf->top_p);
  finally_may_fallthru = gimple_seq_may_fallthru (finally);

  /* Duplicate the FINALLY block.  Only need to do this for try-finally,
     and not for cleanups.  */
  if (this_state)
    finally = lower_try_finally_dup_block (finally, outer_state);

  /* If this cleanup consists of a TRY_CATCH_EXPR with TRY_CATCH_IS_CLEANUP
     set, the handler of the TRY_CATCH_EXPR is another cleanup which ought
     to be in an enclosing scope, but needs to be implemented at this level
     to avoid a nesting violation (see wrap_temporary_cleanups in
     cp/decl.c).  Since it's logically at an outer level, we should call
     terminate before we get to it, so strip it away before adding the
     MUST_NOT_THROW filter.  */
  gsi = gsi_start (finally);
  x = gsi_stmt (gsi);
  if (gimple_code (x) == GIMPLE_TRY
      && gimple_try_kind (x) == GIMPLE_TRY_CATCH
      && gimple_try_catch_is_cleanup (x))
    {
      gsi_insert_seq_before (&gsi, gimple_try_eval (x), GSI_SAME_STMT);
      gsi_remove (&gsi, false);
    }

  /* Wrap the block with protect_cleanup_actions as the action.  */
  x = gimple_build_eh_must_not_throw (protect_cleanup_actions);
  x = gimple_build_try (finally, gimple_seq_alloc_with_stmt (x),
			GIMPLE_TRY_CATCH);
  finally = lower_eh_must_not_throw (outer_state, x);

  /* Drop all of this into the exception sequence.  */
  emit_post_landing_pad (&eh_seq, tf->region);
  gimple_seq_add_seq (&eh_seq, finally);
  if (finally_may_fallthru)
    emit_resx (&eh_seq, tf->region);

  /* Having now been handled, EH isn't to be considered with
     the rest of the outgoing edges.  */
  tf->may_throw = false;
}

/* A subroutine of lower_try_finally.  We have determined that there is
   no fallthru edge out of the finally block.  This means that there is
   no outgoing edge corresponding to any incoming edge.  Restructure the
   try_finally node for this special case.  */

static void
lower_try_finally_nofallthru (struct leh_state *state,
			      struct leh_tf_state *tf)
{
  tree lab, return_val;
  gimple x;
  gimple_seq finally;
  struct goto_queue_node *q, *qe;

  lab = create_artificial_label (gimple_location (tf->try_finally_expr));

  /* We expect that tf->top_p is a GIMPLE_TRY. */
  finally = gimple_try_cleanup (tf->top_p);
  tf->top_p_seq = gimple_try_eval (tf->top_p);

  x = gimple_build_label (lab);
  gimple_seq_add_stmt (&tf->top_p_seq, x);

  return_val = NULL;
  q = tf->goto_queue;
  qe = q + tf->goto_queue_active;
  for (; q < qe; ++q)
    if (q->index < 0)
      do_return_redirection (q, lab, NULL, &return_val);
    else
      do_goto_redirection (q, lab, NULL, tf);

  replace_goto_queue (tf);

  lower_eh_constructs_1 (state, finally);
  gimple_seq_add_seq (&tf->top_p_seq, finally);

  if (tf->may_throw)
    {
      emit_post_landing_pad (&eh_seq, tf->region);

      x = gimple_build_goto (lab);
      gimple_seq_add_stmt (&eh_seq, x);
    }
}

/* A subroutine of lower_try_finally.  We have determined that there is
   exactly one destination of the finally block.  Restructure the
   try_finally node for this special case.  */

static void
lower_try_finally_onedest (struct leh_state *state, struct leh_tf_state *tf)
{
  struct goto_queue_node *q, *qe;
  gimple x;
  gimple_seq finally;
  tree finally_label;
  location_t loc = gimple_location (tf->try_finally_expr);

  finally = gimple_try_cleanup (tf->top_p);
  tf->top_p_seq = gimple_try_eval (tf->top_p);

  lower_eh_constructs_1 (state, finally);

  if (tf->may_throw)
    {
      /* Only reachable via the exception edge.  Add the given label to
         the head of the FINALLY block.  Append a RESX at the end.  */
      emit_post_landing_pad (&eh_seq, tf->region);
      gimple_seq_add_seq (&eh_seq, finally);
      emit_resx (&eh_seq, tf->region);
      return;
    }

  if (tf->may_fallthru)
    {
      /* Only reachable via the fallthru edge.  Do nothing but let
	 the two blocks run together; we'll fall out the bottom.  */
      gimple_seq_add_seq (&tf->top_p_seq, finally);
      return;
    }

  finally_label = create_artificial_label (loc);
  x = gimple_build_label (finally_label);
  gimple_seq_add_stmt (&tf->top_p_seq, x);

  gimple_seq_add_seq (&tf->top_p_seq, finally);

  q = tf->goto_queue;
  qe = q + tf->goto_queue_active;

  if (tf->may_return)
    {
      /* Reachable by return expressions only.  Redirect them.  */
      tree return_val = NULL;
      for (; q < qe; ++q)
	do_return_redirection (q, finally_label, NULL, &return_val);
      replace_goto_queue (tf);
    }
  else
    {
      /* Reachable by goto expressions only.  Redirect them.  */
      for (; q < qe; ++q)
	do_goto_redirection (q, finally_label, NULL, tf);
      replace_goto_queue (tf);

      if (VEC_index (tree, tf->dest_array, 0) == tf->fallthru_label)
	{
	  /* Reachable by goto to fallthru label only.  Redirect it
	     to the new label (already created, sadly), and do not
	     emit the final branch out, or the fallthru label.  */
	  tf->fallthru_label = NULL;
	  return;
	}
    }

  /* Place the original return/goto to the original destination
     immediately after the finally block. */
  x = tf->goto_queue[0].cont_stmt;
  gimple_seq_add_stmt (&tf->top_p_seq, x);
  maybe_record_in_goto_queue (state, x);
}

/* A subroutine of lower_try_finally.  There are multiple edges incoming
   and outgoing from the finally block.  Implement this by duplicating the
   finally block for every destination.  */

static void
lower_try_finally_copy (struct leh_state *state, struct leh_tf_state *tf)
{
  gimple_seq finally;
  gimple_seq new_stmt;
  gimple_seq seq;
  gimple x;
  tree tmp;
  location_t tf_loc = gimple_location (tf->try_finally_expr);

  finally = gimple_try_cleanup (tf->top_p);
  tf->top_p_seq = gimple_try_eval (tf->top_p);
  new_stmt = NULL;

  if (tf->may_fallthru)
    {
      seq = lower_try_finally_dup_block (finally, state);
      lower_eh_constructs_1 (state, seq);
      gimple_seq_add_seq (&new_stmt, seq);

      tmp = lower_try_finally_fallthru_label (tf);
      x = gimple_build_goto (tmp);
      gimple_seq_add_stmt (&new_stmt, x);
    }

  if (tf->may_throw)
    {
      seq = lower_try_finally_dup_block (finally, state);
      lower_eh_constructs_1 (state, seq);

      emit_post_landing_pad (&eh_seq, tf->region);
      gimple_seq_add_seq (&eh_seq, seq);
      emit_resx (&eh_seq, tf->region);
    }

  if (tf->goto_queue)
    {
      struct goto_queue_node *q, *qe;
      tree return_val = NULL;
      int return_index, index;
      struct labels_s
      {
	struct goto_queue_node *q;
	tree label;
      } *labels;

      return_index = VEC_length (tree, tf->dest_array);
      labels = XCNEWVEC (struct labels_s, return_index + 1);

      q = tf->goto_queue;
      qe = q + tf->goto_queue_active;
      for (; q < qe; q++)
	{
	  index = q->index < 0 ? return_index : q->index;

	  if (!labels[index].q)
	    labels[index].q = q;
	}

      for (index = 0; index < return_index + 1; index++)
	{
	  tree lab;

	  q = labels[index].q;
	  if (! q)
	    continue;

	  lab = labels[index].label
	    = create_artificial_label (tf_loc);

	  if (index == return_index)
	    do_return_redirection (q, lab, NULL, &return_val);
	  else
	    do_goto_redirection (q, lab, NULL, tf);

	  x = gimple_build_label (lab);
          gimple_seq_add_stmt (&new_stmt, x);

	  seq = lower_try_finally_dup_block (finally, state);
	  lower_eh_constructs_1 (state, seq);
          gimple_seq_add_seq (&new_stmt, seq);

          gimple_seq_add_stmt (&new_stmt, q->cont_stmt);
	  maybe_record_in_goto_queue (state, q->cont_stmt);
	}

      for (q = tf->goto_queue; q < qe; q++)
	{
	  tree lab;

	  index = q->index < 0 ? return_index : q->index;

	  if (labels[index].q == q)
	    continue;

	  lab = labels[index].label;

	  if (index == return_index)
	    do_return_redirection (q, lab, NULL, &return_val);
	  else
	    do_goto_redirection (q, lab, NULL, tf);
	}

      replace_goto_queue (tf);
      free (labels);
    }

  /* Need to link new stmts after running replace_goto_queue due
     to not wanting to process the same goto stmts twice.  */
  gimple_seq_add_seq (&tf->top_p_seq, new_stmt);
}

/* A subroutine of lower_try_finally.  There are multiple edges incoming
   and outgoing from the finally block.  Implement this by instrumenting
   each incoming edge and creating a switch statement at the end of the
   finally block that branches to the appropriate destination.  */

static void
lower_try_finally_switch (struct leh_state *state, struct leh_tf_state *tf)
{
  struct goto_queue_node *q, *qe;
  tree return_val = NULL;
  tree finally_tmp, finally_label;
  int return_index, eh_index, fallthru_index;
  int nlabels, ndests, j, last_case_index;
  tree last_case;
  VEC (tree,heap) *case_label_vec;
  gimple_seq switch_body;
  gimple x;
  tree tmp;
  gimple switch_stmt;
  gimple_seq finally;
  struct pointer_map_t *cont_map = NULL;
  /* The location of the TRY_FINALLY stmt.  */
  location_t tf_loc = gimple_location (tf->try_finally_expr);
  /* The location of the finally block.  */
  location_t finally_loc;

  switch_body = gimple_seq_alloc ();

  /* Mash the TRY block to the head of the chain.  */
  finally = gimple_try_cleanup (tf->top_p);
  tf->top_p_seq = gimple_try_eval (tf->top_p);

  /* The location of the finally is either the last stmt in the finally
     block or the location of the TRY_FINALLY itself.  */
  finally_loc = gimple_seq_last_stmt (tf->top_p_seq) != NULL ?
    gimple_location (gimple_seq_last_stmt (tf->top_p_seq))
    : tf_loc;

  /* Lower the finally block itself.  */
  lower_eh_constructs_1 (state, finally);

  /* Prepare for switch statement generation.  */
  nlabels = VEC_length (tree, tf->dest_array);
  return_index = nlabels;
  eh_index = return_index + tf->may_return;
  fallthru_index = eh_index + tf->may_throw;
  ndests = fallthru_index + tf->may_fallthru;

  finally_tmp = create_tmp_var (integer_type_node, "finally_tmp");
  finally_label = create_artificial_label (finally_loc);

  /* We use VEC_quick_push on case_label_vec throughout this function,
     since we know the size in advance and allocate precisely as muce
     space as needed.  */
  case_label_vec = VEC_alloc (tree, heap, ndests);
  last_case = NULL;
  last_case_index = 0;

  /* Begin inserting code for getting to the finally block.  Things
     are done in this order to correspond to the sequence the code is
     layed out.  */

  if (tf->may_fallthru)
    {
      x = gimple_build_assign (finally_tmp,
			       build_int_cst (NULL, fallthru_index));
      gimple_seq_add_stmt (&tf->top_p_seq, x);

      last_case = build3 (CASE_LABEL_EXPR, void_type_node,
			  build_int_cst (NULL, fallthru_index),
			  NULL, create_artificial_label (tf_loc));
      VEC_quick_push (tree, case_label_vec, last_case);
      last_case_index++;

      x = gimple_build_label (CASE_LABEL (last_case));
      gimple_seq_add_stmt (&switch_body, x);

      tmp = lower_try_finally_fallthru_label (tf);
      x = gimple_build_goto (tmp);
      gimple_seq_add_stmt (&switch_body, x);
    }

  if (tf->may_throw)
    {
      emit_post_landing_pad (&eh_seq, tf->region);

      x = gimple_build_assign (finally_tmp,
			       build_int_cst (NULL, eh_index));
      gimple_seq_add_stmt (&eh_seq, x);

      x = gimple_build_goto (finally_label);
      gimple_seq_add_stmt (&eh_seq, x);

      last_case = build3 (CASE_LABEL_EXPR, void_type_node,
			  build_int_cst (NULL, eh_index),
			  NULL, create_artificial_label (tf_loc));
      VEC_quick_push (tree, case_label_vec, last_case);
      last_case_index++;

      x = gimple_build_label (CASE_LABEL (last_case));
      gimple_seq_add_stmt (&eh_seq, x);
      emit_resx (&eh_seq, tf->region);
    }

  x = gimple_build_label (finally_label);
  gimple_seq_add_stmt (&tf->top_p_seq, x);

  gimple_seq_add_seq (&tf->top_p_seq, finally);

  /* Redirect each incoming goto edge.  */
  q = tf->goto_queue;
  qe = q + tf->goto_queue_active;
  j = last_case_index + tf->may_return;
  /* Prepare the assignments to finally_tmp that are executed upon the
     entrance through a particular edge. */
  for (; q < qe; ++q)
    {
      gimple_seq mod;
      int switch_id;
      unsigned int case_index;

      mod = gimple_seq_alloc ();

      if (q->index < 0)
	{
	  x = gimple_build_assign (finally_tmp,
				   build_int_cst (NULL, return_index));
	  gimple_seq_add_stmt (&mod, x);
	  do_return_redirection (q, finally_label, mod, &return_val);
	  switch_id = return_index;
	}
      else
	{
	  x = gimple_build_assign (finally_tmp,
				   build_int_cst (NULL, q->index));
	  gimple_seq_add_stmt (&mod, x);
	  do_goto_redirection (q, finally_label, mod, tf);
	  switch_id = q->index;
	}

      case_index = j + q->index;
      if (VEC_length (tree, case_label_vec) <= case_index
          || !VEC_index (tree, case_label_vec, case_index))
        {
          tree case_lab;
          void **slot;
          case_lab = build3 (CASE_LABEL_EXPR, void_type_node,
                             build_int_cst (NULL, switch_id),
			     NULL, NULL);
          /* We store the cont_stmt in the pointer map, so that we can recover
             it in the loop below.  We don't create the new label while
             walking the goto_queue because pointers don't offer a stable
             order.  */
          if (!cont_map)
            cont_map = pointer_map_create ();
          slot = pointer_map_insert (cont_map, case_lab);
          *slot = q->cont_stmt;
          VEC_quick_push (tree, case_label_vec, case_lab);
        }
    }
  for (j = last_case_index; j < last_case_index + nlabels; j++)
    {
      tree label;
      gimple cont_stmt;
      void **slot;

      last_case = VEC_index (tree, case_label_vec, j);

      gcc_assert (last_case);
      gcc_assert (cont_map);

      slot = pointer_map_contains (cont_map, last_case);
      /* As the comment above suggests, CASE_LABEL (last_case) was just a
         placeholder, it does not store an actual label, yet. */
      gcc_assert (slot);
      cont_stmt = *(gimple *) slot;

      label = create_artificial_label (tf_loc);
      CASE_LABEL (last_case) = label;

      x = gimple_build_label (label);
      gimple_seq_add_stmt (&switch_body, x);
      gimple_seq_add_stmt (&switch_body, cont_stmt);
      maybe_record_in_goto_queue (state, cont_stmt);
    }
  if (cont_map)
    pointer_map_destroy (cont_map);

  replace_goto_queue (tf);

  /* Make sure that the last case is the default label, as one is required.
     Then sort the labels, which is also required in GIMPLE.  */
  CASE_LOW (last_case) = NULL;
  sort_case_labels (case_label_vec);

  /* Build the switch statement, setting last_case to be the default
     label.  */
  switch_stmt = gimple_build_switch_vec (finally_tmp, last_case,
                                         case_label_vec);
  gimple_set_location (switch_stmt, finally_loc);

  /* Need to link SWITCH_STMT after running replace_goto_queue
     due to not wanting to process the same goto stmts twice.  */
  gimple_seq_add_stmt (&tf->top_p_seq, switch_stmt);
  gimple_seq_add_seq (&tf->top_p_seq, switch_body);
}

/* Decide whether or not we are going to duplicate the finally block.
   There are several considerations.

   First, if this is Java, then the finally block contains code
   written by the user.  It has line numbers associated with it,
   so duplicating the block means it's difficult to set a breakpoint.
   Since controlling code generation via -g is verboten, we simply
   never duplicate code without optimization.

   Second, we'd like to prevent egregious code growth.  One way to
   do this is to estimate the size of the finally block, multiply
   that by the number of copies we'd need to make, and compare against
   the estimate of the size of the switch machinery we'd have to add.  */

static bool
decide_copy_try_finally (int ndests, gimple_seq finally)
{
  int f_estimate, sw_estimate;

  if (!optimize)
    return false;

  /* Finally estimate N times, plus N gotos.  */
  f_estimate = count_insns_seq (finally, &eni_size_weights);
  f_estimate = (f_estimate + 1) * ndests;

  /* Switch statement (cost 10), N variable assignments, N gotos.  */
  sw_estimate = 10 + 2 * ndests;

  /* Optimize for size clearly wants our best guess.  */
  if (optimize_function_for_size_p (cfun))
    return f_estimate < sw_estimate;

  /* ??? These numbers are completely made up so far.  */
  if (optimize > 1)
    return f_estimate < 100 || f_estimate < sw_estimate * 2;
  else
    return f_estimate < 40 || f_estimate * 2 < sw_estimate * 3;
}


/* A subroutine of lower_eh_constructs_1.  Lower a GIMPLE_TRY_FINALLY nodes
   to a sequence of labels and blocks, plus the exception region trees
   that record all the magic.  This is complicated by the need to
   arrange for the FINALLY block to be executed on all exits.  */

static gimple_seq
lower_try_finally (struct leh_state *state, gimple tp)
{
  struct leh_tf_state this_tf;
  struct leh_state this_state;
  int ndests;

  /* Process the try block.  */

  memset (&this_tf, 0, sizeof (this_tf));
  this_tf.try_finally_expr = tp;
  this_tf.top_p = tp;
  this_tf.outer = state;
  if (using_eh_for_cleanups_p)
    this_tf.region = gen_eh_region_cleanup (state->cur_region);
  else
    this_tf.region = NULL;

  this_state.cur_region = this_tf.region;
  this_state.ehp_region = state->ehp_region;
  this_state.tf = &this_tf;

  lower_eh_constructs_1 (&this_state, gimple_try_eval(tp));

  /* Determine if the try block is escaped through the bottom.  */
  this_tf.may_fallthru = gimple_seq_may_fallthru (gimple_try_eval (tp));

  /* Determine if any exceptions are possible within the try block.  */
  if (using_eh_for_cleanups_p)
    this_tf.may_throw = eh_region_may_contain_throw (this_tf.region);
  if (this_tf.may_throw)
    honor_protect_cleanup_actions (state, &this_state, &this_tf);

  /* Determine how many edges (still) reach the finally block.  Or rather,
     how many destinations are reached by the finally block.  Use this to
     determine how we process the finally block itself.  */

  ndests = VEC_length (tree, this_tf.dest_array);
  ndests += this_tf.may_fallthru;
  ndests += this_tf.may_return;
  ndests += this_tf.may_throw;

  /* If the FINALLY block is not reachable, dike it out.  */
  if (ndests == 0)
    {
      gimple_seq_add_seq (&this_tf.top_p_seq, gimple_try_eval (tp));
      gimple_try_set_cleanup (tp, NULL);
    }
  /* If the finally block doesn't fall through, then any destination
     we might try to impose there isn't reached either.  There may be
     some minor amount of cleanup and redirection still needed.  */
  else if (!gimple_seq_may_fallthru (gimple_try_cleanup (tp)))
    lower_try_finally_nofallthru (state, &this_tf);

  /* We can easily special-case redirection to a single destination.  */
  else if (ndests == 1)
    lower_try_finally_onedest (state, &this_tf);
  else if (decide_copy_try_finally (ndests, gimple_try_cleanup (tp)))
    lower_try_finally_copy (state, &this_tf);
  else
    lower_try_finally_switch (state, &this_tf);

  /* If someone requested we add a label at the end of the transformed
     block, do so.  */
  if (this_tf.fallthru_label)
    {
      /* This must be reached only if ndests == 0. */
      gimple x = gimple_build_label (this_tf.fallthru_label);
      gimple_seq_add_stmt (&this_tf.top_p_seq, x);
    }

  VEC_free (tree, heap, this_tf.dest_array);
  if (this_tf.goto_queue)
    free (this_tf.goto_queue);
  if (this_tf.goto_queue_map)
    pointer_map_destroy (this_tf.goto_queue_map);

  return this_tf.top_p_seq;
}

/* A subroutine of lower_eh_constructs_1.  Lower a GIMPLE_TRY_CATCH with a
   list of GIMPLE_CATCH to a sequence of labels and blocks, plus the
   exception region trees that records all the magic.  */

static gimple_seq
lower_catch (struct leh_state *state, gimple tp)
{
  eh_region try_region = NULL;
  struct leh_state this_state = *state;
  gimple_stmt_iterator gsi;
  tree out_label;
  gimple_seq new_seq;
  gimple x;
  location_t try_catch_loc = gimple_location (tp);

  if (flag_exceptions)
    {
      try_region = gen_eh_region_try (state->cur_region);
      this_state.cur_region = try_region;
    }

  lower_eh_constructs_1 (&this_state, gimple_try_eval (tp));

  if (!eh_region_may_contain_throw (try_region))
    return gimple_try_eval (tp);

  new_seq = NULL;
  emit_eh_dispatch (&new_seq, try_region);
  emit_resx (&new_seq, try_region);

  this_state.cur_region = state->cur_region;
  this_state.ehp_region = try_region;

  out_label = NULL;
  for (gsi = gsi_start (gimple_try_cleanup (tp));
       !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      eh_catch c;
      gimple gcatch;
      gimple_seq handler;

      gcatch = gsi_stmt (gsi);
      c = gen_eh_region_catch (try_region, gimple_catch_types (gcatch));

      handler = gimple_catch_handler (gcatch);
      lower_eh_constructs_1 (&this_state, handler);

      c->label = create_artificial_label (UNKNOWN_LOCATION);
      x = gimple_build_label (c->label);
      gimple_seq_add_stmt (&new_seq, x);

      gimple_seq_add_seq (&new_seq, handler);

      if (gimple_seq_may_fallthru (new_seq))
	{
	  if (!out_label)
	    out_label = create_artificial_label (try_catch_loc);

	  x = gimple_build_goto (out_label);
	  gimple_seq_add_stmt (&new_seq, x);
	}
      if (!c->type_list)
	break;
    }

  gimple_try_set_cleanup (tp, new_seq);

  return frob_into_branch_around (tp, try_region, out_label);
}

/* A subroutine of lower_eh_constructs_1.  Lower a GIMPLE_TRY with a
   GIMPLE_EH_FILTER to a sequence of labels and blocks, plus the exception
   region trees that record all the magic.  */

static gimple_seq
lower_eh_filter (struct leh_state *state, gimple tp)
{
  struct leh_state this_state = *state;
  eh_region this_region = NULL;
  gimple inner, x;
  gimple_seq new_seq;

  inner = gimple_seq_first_stmt (gimple_try_cleanup (tp));

  if (flag_exceptions)
    {
      this_region = gen_eh_region_allowed (state->cur_region,
				           gimple_eh_filter_types (inner));
      this_state.cur_region = this_region;
    }

  lower_eh_constructs_1 (&this_state, gimple_try_eval (tp));

  if (!eh_region_may_contain_throw (this_region))
    return gimple_try_eval (tp);

  new_seq = NULL;
  this_state.cur_region = state->cur_region;
  this_state.ehp_region = this_region;

  emit_eh_dispatch (&new_seq, this_region);
  emit_resx (&new_seq, this_region);

  this_region->u.allowed.label = create_artificial_label (UNKNOWN_LOCATION);
  x = gimple_build_label (this_region->u.allowed.label);
  gimple_seq_add_stmt (&new_seq, x);

  lower_eh_constructs_1 (&this_state, gimple_eh_filter_failure (inner));
  gimple_seq_add_seq (&new_seq, gimple_eh_filter_failure (inner));

  gimple_try_set_cleanup (tp, new_seq);

  return frob_into_branch_around (tp, this_region, NULL);
}

/* A subroutine of lower_eh_constructs_1.  Lower a GIMPLE_TRY with
   an GIMPLE_EH_MUST_NOT_THROW to a sequence of labels and blocks,
   plus the exception region trees that record all the magic.  */

static gimple_seq
lower_eh_must_not_throw (struct leh_state *state, gimple tp)
{
  struct leh_state this_state = *state;

  if (flag_exceptions)
    {
      gimple inner = gimple_seq_first_stmt (gimple_try_cleanup (tp));
      eh_region this_region;

      this_region = gen_eh_region_must_not_throw (state->cur_region);
      this_region->u.must_not_throw.failure_decl
	= gimple_eh_must_not_throw_fndecl (inner);
      this_region->u.must_not_throw.failure_loc = gimple_location (tp);

      /* In order to get mangling applied to this decl, we must mark it
	 used now.  Otherwise, pass_ipa_free_lang_data won't think it
	 needs to happen.  */
      TREE_USED (this_region->u.must_not_throw.failure_decl) = 1;

      this_state.cur_region = this_region;
    }

  lower_eh_constructs_1 (&this_state, gimple_try_eval (tp));

  return gimple_try_eval (tp);
}

/* Implement a cleanup expression.  This is similar to try-finally,
   except that we only execute the cleanup block for exception edges.  */

static gimple_seq
lower_cleanup (struct leh_state *state, gimple tp)
{
  struct leh_state this_state = *state;
  eh_region this_region = NULL;
  struct leh_tf_state fake_tf;
  gimple_seq result;

  if (flag_exceptions)
    {
      this_region = gen_eh_region_cleanup (state->cur_region);
      this_state.cur_region = this_region;
    }

  lower_eh_constructs_1 (&this_state, gimple_try_eval (tp));

  if (!eh_region_may_contain_throw (this_region))
    return gimple_try_eval (tp);

  /* Build enough of a try-finally state so that we can reuse
     honor_protect_cleanup_actions.  */
  memset (&fake_tf, 0, sizeof (fake_tf));
  fake_tf.top_p = fake_tf.try_finally_expr = tp;
  fake_tf.outer = state;
  fake_tf.region = this_region;
  fake_tf.may_fallthru = gimple_seq_may_fallthru (gimple_try_eval (tp));
  fake_tf.may_throw = true;

  honor_protect_cleanup_actions (state, NULL, &fake_tf);

  if (fake_tf.may_throw)
    {
      /* In this case honor_protect_cleanup_actions had nothing to do,
	 and we should process this normally.  */
      lower_eh_constructs_1 (state, gimple_try_cleanup (tp));
      result = frob_into_branch_around (tp, this_region,
                                        fake_tf.fallthru_label);
    }
  else
    {
      /* In this case honor_protect_cleanup_actions did nearly all of
	 the work.  All we have left is to append the fallthru_label.  */

      result = gimple_try_eval (tp);
      if (fake_tf.fallthru_label)
	{
	  gimple x = gimple_build_label (fake_tf.fallthru_label);
	  gimple_seq_add_stmt (&result, x);
	}
    }
  return result;
}

/* Main loop for lowering eh constructs. Also moves gsi to the next
   statement. */

static void
lower_eh_constructs_2 (struct leh_state *state, gimple_stmt_iterator *gsi)
{
  gimple_seq replace;
  gimple x;
  gimple stmt = gsi_stmt (*gsi);

  switch (gimple_code (stmt))
    {
    case GIMPLE_CALL:
      {
	tree fndecl = gimple_call_fndecl (stmt);
	tree rhs, lhs;

	if (fndecl && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL)
	  switch (DECL_FUNCTION_CODE (fndecl))
	    {
	    case BUILT_IN_EH_POINTER:
	      /* The front end may have generated a call to
		 __builtin_eh_pointer (0) within a catch region.  Replace
		 this zero argument with the current catch region number.  */
	      if (state->ehp_region)
		{
		  tree nr = build_int_cst (NULL, state->ehp_region->index);
		  gimple_call_set_arg (stmt, 0, nr);
		}
	      else
		{
		  /* The user has dome something silly.  Remove it.  */
		  rhs = build_int_cst (ptr_type_node, 0);
		  goto do_replace;
		}
	      break;

	    case BUILT_IN_EH_FILTER:
	      /* ??? This should never appear, but since it's a builtin it
		 is accessible to abuse by users.  Just remove it and
		 replace the use with the arbitrary value zero.  */
	      rhs = build_int_cst (TREE_TYPE (TREE_TYPE (fndecl)), 0);
	    do_replace:
	      lhs = gimple_call_lhs (stmt);
	      x = gimple_build_assign (lhs, rhs);
	      gsi_insert_before (gsi, x, GSI_SAME_STMT);
	      /* FALLTHRU */

	    case BUILT_IN_EH_COPY_VALUES:
	      /* Likewise this should not appear.  Remove it.  */
	      gsi_remove (gsi, true);
	      return;

	    default:
	      break;
	    }
      }
      /* FALLTHRU */

    case GIMPLE_ASSIGN:
      /* If the stmt can throw use a new temporary for the assignment
         to a LHS.  This makes sure the old value of the LHS is
	 available on the EH edge.  Only do so for statements that
	 potentially fall thru (no noreturn calls e.g.), otherwise
	 this new assignment might create fake fallthru regions.  */
      if (stmt_could_throw_p (stmt)
	  && gimple_has_lhs (stmt)
	  && gimple_stmt_may_fallthru (stmt)
	  && !tree_could_throw_p (gimple_get_lhs (stmt))
	  && is_gimple_reg_type (TREE_TYPE (gimple_get_lhs (stmt))))
	{
	  tree lhs = gimple_get_lhs (stmt);
	  tree tmp = create_tmp_var (TREE_TYPE (lhs), NULL);
	  gimple s = gimple_build_assign (lhs, tmp);
	  gimple_set_location (s, gimple_location (stmt));
	  gimple_set_block (s, gimple_block (stmt));
	  gimple_set_lhs (stmt, tmp);
	  if (TREE_CODE (TREE_TYPE (tmp)) == COMPLEX_TYPE
	      || TREE_CODE (TREE_TYPE (tmp)) == VECTOR_TYPE)
	    DECL_GIMPLE_REG_P (tmp) = 1;
	  gsi_insert_after (gsi, s, GSI_SAME_STMT);
	}
      /* Look for things that can throw exceptions, and record them.  */
      if (state->cur_region && stmt_could_throw_p (stmt))
	{
	  record_stmt_eh_region (state->cur_region, stmt);
	  note_eh_region_may_contain_throw (state->cur_region);
	}
      break;

    case GIMPLE_COND:
    case GIMPLE_GOTO:
    case GIMPLE_RETURN:
      maybe_record_in_goto_queue (state, stmt);
      break;

    case GIMPLE_SWITCH:
      verify_norecord_switch_expr (state, stmt);
      break;

    case GIMPLE_TRY:
      if (gimple_try_kind (stmt) == GIMPLE_TRY_FINALLY)
	replace = lower_try_finally (state, stmt);
      else
	{
	  x = gimple_seq_first_stmt (gimple_try_cleanup (stmt));
	  if (!x)
	    {
	      replace = gimple_try_eval (stmt);
	      lower_eh_constructs_1 (state, replace);
	    }
	  else
	    switch (gimple_code (x))
	      {
		case GIMPLE_CATCH:
		    replace = lower_catch (state, stmt);
		    break;
		case GIMPLE_EH_FILTER:
		    replace = lower_eh_filter (state, stmt);
		    break;
		case GIMPLE_EH_MUST_NOT_THROW:
		    replace = lower_eh_must_not_throw (state, stmt);
		    break;
		default:
		    replace = lower_cleanup (state, stmt);
		    break;
	      }
	}

      /* Remove the old stmt and insert the transformed sequence
	 instead. */
      gsi_insert_seq_before (gsi, replace, GSI_SAME_STMT);
      gsi_remove (gsi, true);

      /* Return since we don't want gsi_next () */
      return;

    default:
      /* A type, a decl, or some kind of statement that we're not
	 interested in.  Don't walk them.  */
      break;
    }

  gsi_next (gsi);
}

/* A helper to unwrap a gimple_seq and feed stmts to lower_eh_constructs_2. */

static void
lower_eh_constructs_1 (struct leh_state *state, gimple_seq seq)
{
  gimple_stmt_iterator gsi;
  for (gsi = gsi_start (seq); !gsi_end_p (gsi);)
    lower_eh_constructs_2 (state, &gsi);
}

static unsigned int
lower_eh_constructs (void)
{
  struct leh_state null_state;
  gimple_seq bodyp;

  bodyp = gimple_body (current_function_decl);
  if (bodyp == NULL)
    return 0;

  finally_tree = htab_create (31, struct_ptr_hash, struct_ptr_eq, free);
  eh_region_may_contain_throw_map = BITMAP_ALLOC (NULL);
  memset (&null_state, 0, sizeof (null_state));

  collect_finally_tree_1 (bodyp, NULL);
  lower_eh_constructs_1 (&null_state, bodyp);

  /* We assume there's a return statement, or something, at the end of
     the function, and thus ploping the EH sequence afterward won't
     change anything.  */
  gcc_assert (!gimple_seq_may_fallthru (bodyp));
  gimple_seq_add_seq (&bodyp, eh_seq);

  /* We assume that since BODYP already existed, adding EH_SEQ to it
     didn't change its value, and we don't have to re-set the function.  */
  gcc_assert (bodyp == gimple_body (current_function_decl));

  htab_delete (finally_tree);
  BITMAP_FREE (eh_region_may_contain_throw_map);
  eh_seq = NULL;

  /* If this function needs a language specific EH personality routine
     and the frontend didn't already set one do so now.  */
  if (function_needs_eh_personality (cfun) == eh_personality_lang
      && !DECL_FUNCTION_PERSONALITY (current_function_decl))
    DECL_FUNCTION_PERSONALITY (current_function_decl)
      = lang_hooks.eh_personality ();

  return 0;
}

struct gimple_opt_pass pass_lower_eh =
{
 {
  GIMPLE_PASS,
  "eh",					/* name */
  NULL,					/* gate */
  lower_eh_constructs,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_EH,				/* tv_id */
  PROP_gimple_lcf,			/* properties_required */
  PROP_gimple_leh,			/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func			/* todo_flags_finish */
 }
};

/* Create the multiple edges from an EH_DISPATCH statement to all of
   the possible handlers for its EH region.  Return true if there's
   no fallthru edge; false if there is.  */

bool
make_eh_dispatch_edges (gimple stmt)
{
  eh_region r;
  eh_catch c;
  basic_block src, dst;

  r = get_eh_region_from_number (gimple_eh_dispatch_region (stmt));
  src = gimple_bb (stmt);

  switch (r->type)
    {
    case ERT_TRY:
      for (c = r->u.eh_try.first_catch; c ; c = c->next_catch)
	{
	  dst = label_to_block (c->label);
	  make_edge (src, dst, 0);

	  /* A catch-all handler doesn't have a fallthru.  */
	  if (c->type_list == NULL)
	    return false;
	}
      break;

    case ERT_ALLOWED_EXCEPTIONS:
      dst = label_to_block (r->u.allowed.label);
      make_edge (src, dst, 0);
      break;

    default:
      gcc_unreachable ();
    }

  return true;
}

/* Create the single EH edge from STMT to its nearest landing pad,
   if there is such a landing pad within the current function.  */

void
make_eh_edges (gimple stmt)
{
  basic_block src, dst;
  eh_landing_pad lp;
  int lp_nr;

  lp_nr = lookup_stmt_eh_lp (stmt);
  if (lp_nr <= 0)
    return;

  lp = get_eh_landing_pad_from_number (lp_nr);
  gcc_assert (lp != NULL);

  src = gimple_bb (stmt);
  dst = label_to_block (lp->post_landing_pad);
  make_edge (src, dst, EDGE_EH);
}

/* Do the work in redirecting EDGE_IN to NEW_BB within the EH region tree;
   do not actually perform the final edge redirection.

   CHANGE_REGION is true when we're being called from cleanup_empty_eh and
   we intend to change the destination EH region as well; this means
   EH_LANDING_PAD_NR must already be set on the destination block label.
   If false, we're being called from generic cfg manipulation code and we
   should preserve our place within the region tree.  */

static void
redirect_eh_edge_1 (edge edge_in, basic_block new_bb, bool change_region)
{
  eh_landing_pad old_lp, new_lp;
  basic_block old_bb;
  gimple throw_stmt;
  int old_lp_nr, new_lp_nr;
  tree old_label, new_label;
  edge_iterator ei;
  edge e;

  old_bb = edge_in->dest;
  old_label = gimple_block_label (old_bb);
  old_lp_nr = EH_LANDING_PAD_NR (old_label);
  gcc_assert (old_lp_nr > 0);
  old_lp = get_eh_landing_pad_from_number (old_lp_nr);

  throw_stmt = last_stmt (edge_in->src);
  gcc_assert (lookup_stmt_eh_lp (throw_stmt) == old_lp_nr);

  new_label = gimple_block_label (new_bb);

  /* Look for an existing region that might be using NEW_BB already.  */
  new_lp_nr = EH_LANDING_PAD_NR (new_label);
  if (new_lp_nr)
    {
      new_lp = get_eh_landing_pad_from_number (new_lp_nr);
      gcc_assert (new_lp);

      /* Unless CHANGE_REGION is true, the new and old landing pad
	 had better be associated with the same EH region.  */
      gcc_assert (change_region || new_lp->region == old_lp->region);
    }
  else
    {
      new_lp = NULL;
      gcc_assert (!change_region);
    }

  /* Notice when we redirect the last EH edge away from OLD_BB.  */
  FOR_EACH_EDGE (e, ei, old_bb->preds)
    if (e != edge_in && (e->flags & EDGE_EH))
      break;

  if (new_lp)
    {
      /* NEW_LP already exists.  If there are still edges into OLD_LP,
	 there's nothing to do with the EH tree.  If there are no more
	 edges into OLD_LP, then we want to remove OLD_LP as it is unused.
	 If CHANGE_REGION is true, then our caller is expecting to remove
	 the landing pad.  */
      if (e == NULL && !change_region)
	remove_eh_landing_pad (old_lp);
    }
  else
    {
      /* No correct landing pad exists.  If there are no more edges
	 into OLD_LP, then we can simply re-use the existing landing pad.
	 Otherwise, we have to create a new landing pad.  */
      if (e == NULL)
	{
	  EH_LANDING_PAD_NR (old_lp->post_landing_pad) = 0;
	  new_lp = old_lp;
	}
      else
	new_lp = gen_eh_landing_pad (old_lp->region);
      new_lp->post_landing_pad = new_label;
      EH_LANDING_PAD_NR (new_label) = new_lp->index;
    }

  /* Maybe move the throwing statement to the new region.  */
  if (old_lp != new_lp)
    {
      remove_stmt_from_eh_lp (throw_stmt);
      add_stmt_to_eh_lp (throw_stmt, new_lp->index);
    }
}

/* Redirect EH edge E to NEW_BB.  */

edge
redirect_eh_edge (edge edge_in, basic_block new_bb)
{
  redirect_eh_edge_1 (edge_in, new_bb, false);
  return ssa_redirect_edge (edge_in, new_bb);
}

/* This is a subroutine of gimple_redirect_edge_and_branch.  Update the
   labels for redirecting a non-fallthru EH_DISPATCH edge E to NEW_BB.
   The actual edge update will happen in the caller.  */

void
redirect_eh_dispatch_edge (gimple stmt, edge e, basic_block new_bb)
{
  tree new_lab = gimple_block_label (new_bb);
  bool any_changed = false;
  basic_block old_bb;
  eh_region r;
  eh_catch c;

  r = get_eh_region_from_number (gimple_eh_dispatch_region (stmt));
  switch (r->type)
    {
    case ERT_TRY:
      for (c = r->u.eh_try.first_catch; c ; c = c->next_catch)
	{
	  old_bb = label_to_block (c->label);
	  if (old_bb == e->dest)
	    {
	      c->label = new_lab;
	      any_changed = true;
	    }
	}
      break;

    case ERT_ALLOWED_EXCEPTIONS:
      old_bb = label_to_block (r->u.allowed.label);
      gcc_assert (old_bb == e->dest);
      r->u.allowed.label = new_lab;
      any_changed = true;
      break;

    default:
      gcc_unreachable ();
    }

  gcc_assert (any_changed);
}

/* Helper function for operation_could_trap_p and stmt_could_throw_p.  */

bool
operation_could_trap_helper_p (enum tree_code op,
			       bool fp_operation,
			       bool honor_trapv,
			       bool honor_nans,
			       bool honor_snans,
			       tree divisor,
			       bool *handled)
{
  *handled = true;
  switch (op)
    {
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
      if (honor_snans || honor_trapv)
	return true;
      if (fp_operation)
	return flag_trapping_math;
      if (!TREE_CONSTANT (divisor) || integer_zerop (divisor))
        return true;
      return false;

    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case LTGT_EXPR:
      /* Some floating point comparisons may trap.  */
      return honor_nans;

    case EQ_EXPR:
    case NE_EXPR:
    case UNORDERED_EXPR:
    case ORDERED_EXPR:
    case UNLT_EXPR:
    case UNLE_EXPR:
    case UNGT_EXPR:
    case UNGE_EXPR:
    case UNEQ_EXPR:
      return honor_snans;

    case CONVERT_EXPR:
    case FIX_TRUNC_EXPR:
      /* Conversion of floating point might trap.  */
      return honor_nans;

    case NEGATE_EXPR:
    case ABS_EXPR:
    case CONJ_EXPR:
      /* These operations don't trap with floating point.  */
      if (honor_trapv)
	return true;
      return false;

    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
      /* Any floating arithmetic may trap.  */
      if (fp_operation && flag_trapping_math)
	return true;
      if (honor_trapv)
	return true;
      return false;

    default:
      /* Any floating arithmetic may trap.  */
      if (fp_operation && flag_trapping_math)
	return true;

      *handled = false;
      return false;
    }
}

/* Return true if operation OP may trap.  FP_OPERATION is true if OP is applied
   on floating-point values.  HONOR_TRAPV is true if OP is applied on integer
   type operands that may trap.  If OP is a division operator, DIVISOR contains
   the value of the divisor.  */

bool
operation_could_trap_p (enum tree_code op, bool fp_operation, bool honor_trapv,
			tree divisor)
{
  bool honor_nans = (fp_operation && flag_trapping_math
		     && !flag_finite_math_only);
  bool honor_snans = fp_operation && flag_signaling_nans != 0;
  bool handled;

  if (TREE_CODE_CLASS (op) != tcc_comparison
      && TREE_CODE_CLASS (op) != tcc_unary
      && TREE_CODE_CLASS (op) != tcc_binary)
    return false;

  return operation_could_trap_helper_p (op, fp_operation, honor_trapv,
					honor_nans, honor_snans, divisor,
					&handled);
}

/* Return true if EXPR can trap, as in dereferencing an invalid pointer
   location or floating point arithmetic.  C.f. the rtl version, may_trap_p.
   This routine expects only GIMPLE lhs or rhs input.  */

bool
tree_could_trap_p (tree expr)
{
  enum tree_code code;
  bool fp_operation = false;
  bool honor_trapv = false;
  tree t, base, div = NULL_TREE;

  if (!expr)
    return false;

  code = TREE_CODE (expr);
  t = TREE_TYPE (expr);

  if (t)
    {
      if (COMPARISON_CLASS_P (expr))
	fp_operation = FLOAT_TYPE_P (TREE_TYPE (TREE_OPERAND (expr, 0)));
      else
	fp_operation = FLOAT_TYPE_P (t);
      honor_trapv = INTEGRAL_TYPE_P (t) && TYPE_OVERFLOW_TRAPS (t);
    }

  if (TREE_CODE_CLASS (code) == tcc_binary)
    div = TREE_OPERAND (expr, 1);
  if (operation_could_trap_p (code, fp_operation, honor_trapv, div))
    return true;

 restart:
  switch (code)
    {
    case TARGET_MEM_REF:
      /* For TARGET_MEM_REFs use the information based on the original
	 reference.  */
      expr = TMR_ORIGINAL (expr);
      code = TREE_CODE (expr);
      goto restart;

    case COMPONENT_REF:
    case REALPART_EXPR:
    case IMAGPART_EXPR:
    case BIT_FIELD_REF:
    case VIEW_CONVERT_EXPR:
    case WITH_SIZE_EXPR:
      expr = TREE_OPERAND (expr, 0);
      code = TREE_CODE (expr);
      goto restart;

    case ARRAY_RANGE_REF:
      base = TREE_OPERAND (expr, 0);
      if (tree_could_trap_p (base))
	return true;
      if (TREE_THIS_NOTRAP (expr))
	return false;
      return !range_in_array_bounds_p (expr);

    case ARRAY_REF:
      base = TREE_OPERAND (expr, 0);
      if (tree_could_trap_p (base))
	return true;
      if (TREE_THIS_NOTRAP (expr))
	return false;
      return !in_array_bounds_p (expr);

    case INDIRECT_REF:
    case ALIGN_INDIRECT_REF:
    case MISALIGNED_INDIRECT_REF:
      return !TREE_THIS_NOTRAP (expr);

    case ASM_EXPR:
      return TREE_THIS_VOLATILE (expr);

    case CALL_EXPR:
      t = get_callee_fndecl (expr);
      /* Assume that calls to weak functions may trap.  */
      if (!t || !DECL_P (t) || DECL_WEAK (t))
	return true;
      return false;

    default:
      return false;
    }
}


/* Helper for stmt_could_throw_p.  Return true if STMT (assumed to be a
   an assignment or a conditional) may throw.  */

static bool
stmt_could_throw_1_p (gimple stmt)
{
  enum tree_code code = gimple_expr_code (stmt);
  bool honor_nans = false;
  bool honor_snans = false;
  bool fp_operation = false;
  bool honor_trapv = false;
  tree t;
  size_t i;
  bool handled, ret;

  if (TREE_CODE_CLASS (code) == tcc_comparison
      || TREE_CODE_CLASS (code) == tcc_unary
      || TREE_CODE_CLASS (code) == tcc_binary)
    {
      t = gimple_expr_type (stmt);
      fp_operation = FLOAT_TYPE_P (t);
      if (fp_operation)
	{
	  honor_nans = flag_trapping_math && !flag_finite_math_only;
	  honor_snans = flag_signaling_nans != 0;
	}
      else if (INTEGRAL_TYPE_P (t) && TYPE_OVERFLOW_TRAPS (t))
	honor_trapv = true;
    }

  /* Check if the main expression may trap.  */
  t = is_gimple_assign (stmt) ? gimple_assign_rhs2 (stmt) : NULL;
  ret = operation_could_trap_helper_p (code, fp_operation, honor_trapv,
				       honor_nans, honor_snans, t,
				       &handled);
  if (handled)
    return ret;

  /* If the expression does not trap, see if any of the individual operands may
     trap.  */
  for (i = 0; i < gimple_num_ops (stmt); i++)
    if (tree_could_trap_p (gimple_op (stmt, i)))
      return true;

  return false;
}


/* Return true if statement STMT could throw an exception.  */

bool
stmt_could_throw_p (gimple stmt)
{
  if (!flag_exceptions)
    return false;

  /* The only statements that can throw an exception are assignments,
     conditionals, calls, resx, and asms.  */
  switch (gimple_code (stmt))
    {
    case GIMPLE_RESX:
      return true;

    case GIMPLE_CALL:
      return !gimple_call_nothrow_p (stmt);

    case GIMPLE_ASSIGN:
    case GIMPLE_COND:
      if (!flag_non_call_exceptions)
        return false;
      return stmt_could_throw_1_p (stmt);

    case GIMPLE_ASM:
      if (!flag_non_call_exceptions)
        return false;
      return gimple_asm_volatile_p (stmt);

    default:
      return false;
    }
}


/* Return true if expression T could throw an exception.  */

bool
tree_could_throw_p (tree t)
{
  if (!flag_exceptions)
    return false;
  if (TREE_CODE (t) == MODIFY_EXPR)
    {
      if (flag_non_call_exceptions
          && tree_could_trap_p (TREE_OPERAND (t, 0)))
        return true;
      t = TREE_OPERAND (t, 1);
    }

  if (TREE_CODE (t) == WITH_SIZE_EXPR)
    t = TREE_OPERAND (t, 0);
  if (TREE_CODE (t) == CALL_EXPR)
    return (call_expr_flags (t) & ECF_NOTHROW) == 0;
  if (flag_non_call_exceptions)
    return tree_could_trap_p (t);
  return false;
}

/* Return true if STMT can throw an exception that is not caught within
   the current function (CFUN).  */

bool
stmt_can_throw_external (gimple stmt)
{
  int lp_nr;

  if (!stmt_could_throw_p (stmt))
    return false;

  lp_nr = lookup_stmt_eh_lp (stmt);
  return lp_nr == 0;
}

/* Return true if STMT can throw an exception that is caught within
   the current function (CFUN).  */

bool
stmt_can_throw_internal (gimple stmt)
{
  int lp_nr;

  if (!stmt_could_throw_p (stmt))
    return false;

  lp_nr = lookup_stmt_eh_lp (stmt);
  return lp_nr > 0;
}

/* Given a statement STMT in IFUN, if STMT can no longer throw, then
   remove any entry it might have from the EH table.  Return true if
   any change was made.  */

bool
maybe_clean_eh_stmt_fn (struct function *ifun, gimple stmt)
{
  if (stmt_could_throw_p (stmt))
    return false;
  return remove_stmt_from_eh_lp_fn (ifun, stmt);
}

/* Likewise, but always use the current function.  */

bool
maybe_clean_eh_stmt (gimple stmt)
{
  return maybe_clean_eh_stmt_fn (cfun, stmt);
}

/* Given a statement OLD_STMT and a new statement NEW_STMT that has replaced
   OLD_STMT in the function, remove OLD_STMT from the EH table and put NEW_STMT
   in the table if it should be in there.  Return TRUE if a replacement was
   done that my require an EH edge purge.  */

bool
maybe_clean_or_replace_eh_stmt (gimple old_stmt, gimple new_stmt)
{
  int lp_nr = lookup_stmt_eh_lp (old_stmt);

  if (lp_nr != 0)
    {
      bool new_stmt_could_throw = stmt_could_throw_p (new_stmt);

      if (new_stmt == old_stmt && new_stmt_could_throw)
	return false;

      remove_stmt_from_eh_lp (old_stmt);
      if (new_stmt_could_throw)
	{
	  add_stmt_to_eh_lp (new_stmt, lp_nr);
	  return false;
	}
      else
	return true;
    }

  return false;
}

/* Given a statement OLD_STMT in OLD_FUN and a duplicate statment NEW_STMT
   in NEW_FUN, copy the EH table data from OLD_STMT to NEW_STMT.  The MAP
   operand is the return value of duplicate_eh_regions.  */

bool
maybe_duplicate_eh_stmt_fn (struct function *new_fun, gimple new_stmt,
			    struct function *old_fun, gimple old_stmt,
			    struct pointer_map_t *map, int default_lp_nr)
{
  int old_lp_nr, new_lp_nr;
  void **slot;

  if (!stmt_could_throw_p (new_stmt))
    return false;

  old_lp_nr = lookup_stmt_eh_lp_fn (old_fun, old_stmt);
  if (old_lp_nr == 0)
    {
      if (default_lp_nr == 0)
	return false;
      new_lp_nr = default_lp_nr;
    }
  else if (old_lp_nr > 0)
    {
      eh_landing_pad old_lp, new_lp;

      old_lp = VEC_index (eh_landing_pad, old_fun->eh->lp_array, old_lp_nr);
      slot = pointer_map_contains (map, old_lp);
      new_lp = (eh_landing_pad) *slot;
      new_lp_nr = new_lp->index;
    }
  else
    {
      eh_region old_r, new_r;

      old_r = VEC_index (eh_region, old_fun->eh->region_array, -old_lp_nr);
      slot = pointer_map_contains (map, old_r);
      new_r = (eh_region) *slot;
      new_lp_nr = -new_r->index;
    }

  add_stmt_to_eh_lp_fn (new_fun, new_stmt, new_lp_nr);
  return true;
}

/* Similar, but both OLD_STMT and NEW_STMT are within the current function,
   and thus no remapping is required.  */

bool
maybe_duplicate_eh_stmt (gimple new_stmt, gimple old_stmt)
{
  int lp_nr;

  if (!stmt_could_throw_p (new_stmt))
    return false;

  lp_nr = lookup_stmt_eh_lp (old_stmt);
  if (lp_nr == 0)
    return false;

  add_stmt_to_eh_lp (new_stmt, lp_nr);
  return true;
}

/* Returns TRUE if oneh and twoh are exception handlers (gimple_try_cleanup of
   GIMPLE_TRY) that are similar enough to be considered the same.  Currently
   this only handles handlers consisting of a single call, as that's the
   important case for C++: a destructor call for a particular object showing
   up in multiple handlers.  */

static bool
same_handler_p (gimple_seq oneh, gimple_seq twoh)
{
  gimple_stmt_iterator gsi;
  gimple ones, twos;
  unsigned int ai;

  gsi = gsi_start (oneh);
  if (!gsi_one_before_end_p (gsi))
    return false;
  ones = gsi_stmt (gsi);

  gsi = gsi_start (twoh);
  if (!gsi_one_before_end_p (gsi))
    return false;
  twos = gsi_stmt (gsi);

  if (!is_gimple_call (ones)
      || !is_gimple_call (twos)
      || gimple_call_lhs (ones)
      || gimple_call_lhs (twos)
      || gimple_call_chain (ones)
      || gimple_call_chain (twos)
      || !operand_equal_p (gimple_call_fn (ones), gimple_call_fn (twos), 0)
      || gimple_call_num_args (ones) != gimple_call_num_args (twos))
    return false;

  for (ai = 0; ai < gimple_call_num_args (ones); ++ai)
    if (!operand_equal_p (gimple_call_arg (ones, ai),
                          gimple_call_arg (twos, ai), 0))
      return false;

  return true;
}

/* Optimize
    try { A() } finally { try { ~B() } catch { ~A() } }
    try { ... } finally { ~A() }
   into
    try { A() } catch { ~B() }
    try { ~B() ... } finally { ~A() }

   This occurs frequently in C++, where A is a local variable and B is a
   temporary used in the initializer for A.  */

static void
optimize_double_finally (gimple one, gimple two)
{
  gimple oneh;
  gimple_stmt_iterator gsi;

  gsi = gsi_start (gimple_try_cleanup (one));
  if (!gsi_one_before_end_p (gsi))
    return;

  oneh = gsi_stmt (gsi);
  if (gimple_code (oneh) != GIMPLE_TRY
      || gimple_try_kind (oneh) != GIMPLE_TRY_CATCH)
    return;

  if (same_handler_p (gimple_try_cleanup (oneh), gimple_try_cleanup (two)))
    {
      gimple_seq seq = gimple_try_eval (oneh);

      gimple_try_set_cleanup (one, seq);
      gimple_try_set_kind (one, GIMPLE_TRY_CATCH);
      seq = copy_gimple_seq_and_replace_locals (seq);
      gimple_seq_add_seq (&seq, gimple_try_eval (two));
      gimple_try_set_eval (two, seq);
    }
}

/* Perform EH refactoring optimizations that are simpler to do when code
   flow has been lowered but EH structures haven't.  */

static void
refactor_eh_r (gimple_seq seq)
{
  gimple_stmt_iterator gsi;
  gimple one, two;

  one = NULL;
  two = NULL;
  gsi = gsi_start (seq);
  while (1)
    {
      one = two;
      if (gsi_end_p (gsi))
	two = NULL;
      else
	two = gsi_stmt (gsi);
      if (one
	  && two
	  && gimple_code (one) == GIMPLE_TRY
	  && gimple_code (two) == GIMPLE_TRY
	  && gimple_try_kind (one) == GIMPLE_TRY_FINALLY
	  && gimple_try_kind (two) == GIMPLE_TRY_FINALLY)
	optimize_double_finally (one, two);
      if (one)
	switch (gimple_code (one))
	  {
	  case GIMPLE_TRY:
	    refactor_eh_r (gimple_try_eval (one));
	    refactor_eh_r (gimple_try_cleanup (one));
	    break;
	  case GIMPLE_CATCH:
	    refactor_eh_r (gimple_catch_handler (one));
	    break;
	  case GIMPLE_EH_FILTER:
	    refactor_eh_r (gimple_eh_filter_failure (one));
	    break;
	  default:
	    break;
	  }
      if (two)
	gsi_next (&gsi);
      else
	break;
    }
}

static unsigned
refactor_eh (void)
{
  refactor_eh_r (gimple_body (current_function_decl));
  return 0;
}

static bool
gate_refactor_eh (void)
{
  return flag_exceptions != 0;
}

struct gimple_opt_pass pass_refactor_eh =
{
 {
  GIMPLE_PASS,
  "ehopt",				/* name */
  gate_refactor_eh,			/* gate */
  refactor_eh,				/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_EH,				/* tv_id */
  PROP_gimple_lcf,			/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func			/* todo_flags_finish */
 }
};

/* At the end of gimple optimization, we can lower RESX.  */

static bool
lower_resx (basic_block bb, gimple stmt, struct pointer_map_t *mnt_map)
{
  int lp_nr;
  eh_region src_r, dst_r;
  gimple_stmt_iterator gsi;
  gimple x;
  tree fn, src_nr;
  bool ret = false;

  lp_nr = lookup_stmt_eh_lp (stmt);
  if (lp_nr != 0)
    dst_r = get_eh_region_from_lp_number (lp_nr);
  else
    dst_r = NULL;

  src_r = get_eh_region_from_number (gimple_resx_region (stmt));
  gsi = gsi_last_bb (bb);

  if (src_r == NULL)
    {
      /* We can wind up with no source region when pass_cleanup_eh shows
	 that there are no entries into an eh region and deletes it, but
	 then the block that contains the resx isn't removed.  This can
	 happen without optimization when the switch statement created by
	 lower_try_finally_switch isn't simplified to remove the eh case.

	 Resolve this by expanding the resx node to an abort.  */

      fn = implicit_built_in_decls[BUILT_IN_TRAP];
      x = gimple_build_call (fn, 0);
      gsi_insert_before (&gsi, x, GSI_SAME_STMT);

      while (EDGE_COUNT (bb->succs) > 0)
	remove_edge (EDGE_SUCC (bb, 0));
    }
  else if (dst_r)
    {
      /* When we have a destination region, we resolve this by copying
	 the excptr and filter values into place, and changing the edge
	 to immediately after the landing pad.  */
      edge e;

      if (lp_nr < 0)
	{
	  basic_block new_bb;
	  void **slot;
	  tree lab;

	  /* We are resuming into a MUST_NOT_CALL region.  Expand a call to
	     the failure decl into a new block, if needed.  */
	  gcc_assert (dst_r->type == ERT_MUST_NOT_THROW);

	  slot = pointer_map_contains (mnt_map, dst_r);
	  if (slot == NULL)
	    {
	      gimple_stmt_iterator gsi2;

	      new_bb = create_empty_bb (bb);
	      lab = gimple_block_label (new_bb);
	      gsi2 = gsi_start_bb (new_bb);

	      fn = dst_r->u.must_not_throw.failure_decl;
	      x = gimple_build_call (fn, 0);
	      gimple_set_location (x, dst_r->u.must_not_throw.failure_loc);
	      gsi_insert_after (&gsi2, x, GSI_CONTINUE_LINKING);

	      slot = pointer_map_insert (mnt_map, dst_r);
	      *slot = lab;
	    }
	  else
	    {
	      lab = (tree) *slot;
	      new_bb = label_to_block (lab);
	    }

	  gcc_assert (EDGE_COUNT (bb->succs) == 0);
	  e = make_edge (bb, new_bb, EDGE_FALLTHRU);
	  e->count = bb->count;
	  e->probability = REG_BR_PROB_BASE;
	}
      else
	{
	  edge_iterator ei;
	  tree dst_nr = build_int_cst (NULL, dst_r->index);

	  fn = implicit_built_in_decls[BUILT_IN_EH_COPY_VALUES];
	  src_nr = build_int_cst (NULL, src_r->index);
	  x = gimple_build_call (fn, 2, dst_nr, src_nr);
	  gsi_insert_before (&gsi, x, GSI_SAME_STMT);

	  /* Update the flags for the outgoing edge.  */
	  e = single_succ_edge (bb);
	  gcc_assert (e->flags & EDGE_EH);
	  e->flags = (e->flags & ~EDGE_EH) | EDGE_FALLTHRU;

	  /* If there are no more EH users of the landing pad, delete it.  */
	  FOR_EACH_EDGE (e, ei, e->dest->preds)
	    if (e->flags & EDGE_EH)
	      break;
	  if (e == NULL)
	    {
	      eh_landing_pad lp = get_eh_landing_pad_from_number (lp_nr);
	      remove_eh_landing_pad (lp);
	    }
	}

      ret = true;
    }
  else
    {
      tree var;

      /* When we don't have a destination region, this exception escapes
	 up the call chain.  We resolve this by generating a call to the
	 _Unwind_Resume library function.  */

      /* The ARM EABI redefines _Unwind_Resume as __cxa_end_cleanup
	 with no arguments for C++ and Java.  Check for that.  */
      if (src_r->use_cxa_end_cleanup)
	{
	  fn = implicit_built_in_decls[BUILT_IN_CXA_END_CLEANUP];
	  x = gimple_build_call (fn, 0);
	  gsi_insert_before (&gsi, x, GSI_SAME_STMT);
	}
      else
	{
	  fn = implicit_built_in_decls[BUILT_IN_EH_POINTER];
	  src_nr = build_int_cst (NULL, src_r->index);
	  x = gimple_build_call (fn, 1, src_nr);
	  var = create_tmp_var (ptr_type_node, NULL);
	  var = make_ssa_name (var, x);
	  gimple_call_set_lhs (x, var);
	  gsi_insert_before (&gsi, x, GSI_SAME_STMT);

	  fn = implicit_built_in_decls[BUILT_IN_UNWIND_RESUME];
	  x = gimple_build_call (fn, 1, var);
	  gsi_insert_before (&gsi, x, GSI_SAME_STMT);
	}

      gcc_assert (EDGE_COUNT (bb->succs) == 0);
    }

  gsi_remove (&gsi, true);

  return ret;
}

static unsigned
execute_lower_resx (void)
{
  basic_block bb;
  struct pointer_map_t *mnt_map;
  bool dominance_invalidated = false;
  bool any_rewritten = false;

  mnt_map = pointer_map_create ();

  FOR_EACH_BB (bb)
    {
      gimple last = last_stmt (bb);
      if (last && is_gimple_resx (last))
	{
	  dominance_invalidated |= lower_resx (bb, last, mnt_map);
	  any_rewritten = true;
	}
    }

  pointer_map_destroy (mnt_map);

  if (dominance_invalidated)
    {
      free_dominance_info (CDI_DOMINATORS);
      free_dominance_info (CDI_POST_DOMINATORS);
    }

  return any_rewritten ? TODO_update_ssa_only_virtuals : 0;
}

static bool
gate_lower_resx (void)
{
  return flag_exceptions != 0;
}

struct gimple_opt_pass pass_lower_resx =
{
 {
  GIMPLE_PASS,
  "resx",				/* name */
  gate_lower_resx,			/* gate */
  execute_lower_resx,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_EH,				/* tv_id */
  PROP_gimple_lcf,			/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func | TODO_verify_flow	/* todo_flags_finish */
 }
};


/* At the end of inlining, we can lower EH_DISPATCH.  */

static void
lower_eh_dispatch (basic_block src, gimple stmt)
{
  gimple_stmt_iterator gsi;
  int region_nr;
  eh_region r;
  tree filter, fn;
  gimple x;

  region_nr = gimple_eh_dispatch_region (stmt);
  r = get_eh_region_from_number (region_nr);

  gsi = gsi_last_bb (src);

  switch (r->type)
    {
    case ERT_TRY:
      {
	VEC (tree, heap) *labels = NULL;
	tree default_label = NULL;
	eh_catch c;
	edge_iterator ei;
	edge e;

	/* Collect the labels for a switch.  Zero the post_landing_pad
	   field becase we'll no longer have anything keeping these labels
	   in existance and the optimizer will be free to merge these
	   blocks at will.  */
	for (c = r->u.eh_try.first_catch; c ; c = c->next_catch)
	  {
	    tree tp_node, flt_node, lab = c->label;

	    c->label = NULL;
	    tp_node = c->type_list;
	    flt_node = c->filter_list;

	    if (tp_node == NULL)
	      {
	        default_label = lab;
		break;
	      }
	    do
	      {
		tree t = build3 (CASE_LABEL_EXPR, void_type_node,
				 TREE_VALUE (flt_node), NULL, lab);
		VEC_safe_push (tree, heap, labels, t);

		tp_node = TREE_CHAIN (tp_node);
		flt_node = TREE_CHAIN (flt_node);
	      }
	    while (tp_node);
	  }

	/* Clean up the edge flags.  */
	FOR_EACH_EDGE (e, ei, src->succs)
	  {
	    if (e->flags & EDGE_FALLTHRU)
	      {
		/* If there was no catch-all, use the fallthru edge.  */
		if (default_label == NULL)
		  default_label = gimple_block_label (e->dest);
		e->flags &= ~EDGE_FALLTHRU;
	      }
	  }
	gcc_assert (default_label != NULL);

	/* Don't generate a switch if there's only a default case.
	   This is common in the form of try { A; } catch (...) { B; }.  */
	if (labels == NULL)
	  {
	    e = single_succ_edge (src);
	    e->flags |= EDGE_FALLTHRU;
	  }
	else
	  {
	    fn = implicit_built_in_decls[BUILT_IN_EH_FILTER];
	    x = gimple_build_call (fn, 1, build_int_cst (NULL, region_nr));
	    filter = create_tmp_var (TREE_TYPE (TREE_TYPE (fn)), NULL);
	    filter = make_ssa_name (filter, x);
	    gimple_call_set_lhs (x, filter);
	    gsi_insert_before (&gsi, x, GSI_SAME_STMT);

	    /* Turn the default label into a default case.  */
	    default_label = build3 (CASE_LABEL_EXPR, void_type_node,
				    NULL, NULL, default_label);
	    sort_case_labels (labels);

	    x = gimple_build_switch_vec (filter, default_label, labels);
	    gsi_insert_before (&gsi, x, GSI_SAME_STMT);

	    VEC_free (tree, heap, labels);
	  }
      }
      break;

    case ERT_ALLOWED_EXCEPTIONS:
      {
	edge b_e = BRANCH_EDGE (src);
	edge f_e = FALLTHRU_EDGE (src);

	fn = implicit_built_in_decls[BUILT_IN_EH_FILTER];
	x = gimple_build_call (fn, 1, build_int_cst (NULL, region_nr));
	filter = create_tmp_var (TREE_TYPE (TREE_TYPE (fn)), NULL);
	filter = make_ssa_name (filter, x);
	gimple_call_set_lhs (x, filter);
	gsi_insert_before (&gsi, x, GSI_SAME_STMT);

	r->u.allowed.label = NULL;
	x = gimple_build_cond (EQ_EXPR, filter,
			       build_int_cst (TREE_TYPE (filter),
					      r->u.allowed.filter),
			       NULL_TREE, NULL_TREE);
	gsi_insert_before (&gsi, x, GSI_SAME_STMT);

	b_e->flags = b_e->flags | EDGE_TRUE_VALUE;
        f_e->flags = (f_e->flags & ~EDGE_FALLTHRU) | EDGE_FALSE_VALUE;
      }
      break;

    default:
      gcc_unreachable ();
    }

  /* Replace the EH_DISPATCH with the SWITCH or COND generated above.  */
  gsi_remove (&gsi, true);
}

static unsigned
execute_lower_eh_dispatch (void)
{
  basic_block bb;
  bool any_rewritten = false;

  assign_filter_values ();

  FOR_EACH_BB (bb)
    {
      gimple last = last_stmt (bb);
      if (last && gimple_code (last) == GIMPLE_EH_DISPATCH)
	{
	  lower_eh_dispatch (bb, last);
	  any_rewritten = true;
	}
    }

  return any_rewritten ? TODO_update_ssa_only_virtuals : 0;
}

static bool
gate_lower_eh_dispatch (void)
{
  return cfun->eh->region_tree != NULL;
}

struct gimple_opt_pass pass_lower_eh_dispatch =
{
 {
  GIMPLE_PASS,
  "ehdisp",				/* name */
  gate_lower_eh_dispatch,		/* gate */
  execute_lower_eh_dispatch,		/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_EH,				/* tv_id */
  PROP_gimple_lcf,			/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func | TODO_verify_flow	/* todo_flags_finish */
 }
};

/* Walk statements, see what regions are really referenced and remove
   those that are unused.  */

static void
remove_unreachable_handlers (void)
{
  sbitmap r_reachable, lp_reachable;
  eh_region region;
  eh_landing_pad lp;
  basic_block bb;
  int lp_nr, r_nr;

  r_reachable = sbitmap_alloc (VEC_length (eh_region, cfun->eh->region_array));
  lp_reachable
    = sbitmap_alloc (VEC_length (eh_landing_pad, cfun->eh->lp_array));
  sbitmap_zero (r_reachable);
  sbitmap_zero (lp_reachable);

  FOR_EACH_BB (bb)
    {
      gimple_stmt_iterator gsi = gsi_start_bb (bb);

      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple stmt = gsi_stmt (gsi);
	  lp_nr = lookup_stmt_eh_lp (stmt);

	  /* Negative LP numbers are MUST_NOT_THROW regions which
	     are not considered BB enders.  */
	  if (lp_nr < 0)
	    SET_BIT (r_reachable, -lp_nr);

	  /* Positive LP numbers are real landing pads, are are BB enders.  */
	  else if (lp_nr > 0)
	    {
	      gcc_assert (gsi_one_before_end_p (gsi));
	      region = get_eh_region_from_lp_number (lp_nr);
	      SET_BIT (r_reachable, region->index);
	      SET_BIT (lp_reachable, lp_nr);
	    }
	}
    }

  if (dump_file)
    {
      fprintf (dump_file, "Before removal of unreachable regions:\n");
      dump_eh_tree (dump_file, cfun);
      fprintf (dump_file, "Reachable regions: ");
      dump_sbitmap_file (dump_file, r_reachable);
      fprintf (dump_file, "Reachable landing pads: ");
      dump_sbitmap_file (dump_file, lp_reachable);
    }

  for (r_nr = 1;
       VEC_iterate (eh_region, cfun->eh->region_array, r_nr, region); ++r_nr)
    if (region && !TEST_BIT (r_reachable, r_nr))
      {
	if (dump_file)
	  fprintf (dump_file, "Removing unreachable region %d\n", r_nr);
	remove_eh_handler (region);
      }

  for (lp_nr = 1;
       VEC_iterate (eh_landing_pad, cfun->eh->lp_array, lp_nr, lp); ++lp_nr)
    if (lp && !TEST_BIT (lp_reachable, lp_nr))
      {
	if (dump_file)
	  fprintf (dump_file, "Removing unreachable landing pad %d\n", lp_nr);
	remove_eh_landing_pad (lp);
      }

  if (dump_file)
    {
      fprintf (dump_file, "\n\nAfter removal of unreachable regions:\n");
      dump_eh_tree (dump_file, cfun);
      fprintf (dump_file, "\n\n");
    }

  sbitmap_free (r_reachable);
  sbitmap_free (lp_reachable);

#ifdef ENABLE_CHECKING
  verify_eh_tree (cfun);
#endif
}

/* Remove regions that do not have landing pads.  This assumes
   that remove_unreachable_handlers has already been run, and
   that we've just manipulated the landing pads since then.  */

static void
remove_unreachable_handlers_no_lp (void)
{
  eh_region r;
  int i;

  for (i = 1; VEC_iterate (eh_region, cfun->eh->region_array, i, r); ++i)
    if (r && r->landing_pads == NULL && r->type != ERT_MUST_NOT_THROW)
      {
	if (dump_file)
	  fprintf (dump_file, "Removing unreachable region %d\n", i);
	remove_eh_handler (r);
      }
}

/* Undo critical edge splitting on an EH landing pad.  Earlier, we
   optimisticaly split all sorts of edges, including EH edges.  The
   optimization passes in between may not have needed them; if not,
   we should undo the split.

   Recognize this case by having one EH edge incoming to the BB and
   one normal edge outgoing; BB should be empty apart from the
   post_landing_pad label.

   Note that this is slightly different from the empty handler case
   handled by cleanup_empty_eh, in that the actual handler may yet
   have actual code but the landing pad has been separated from the
   handler.  As such, cleanup_empty_eh relies on this transformation
   having been done first.  */

static bool
unsplit_eh (eh_landing_pad lp)
{
  basic_block bb = label_to_block (lp->post_landing_pad);
  gimple_stmt_iterator gsi;
  edge e_in, e_out;

  /* Quickly check the edge counts on BB for singularity.  */
  if (EDGE_COUNT (bb->preds) != 1 || EDGE_COUNT (bb->succs) != 1)
    return false;
  e_in = EDGE_PRED (bb, 0);
  e_out = EDGE_SUCC (bb, 0);

  /* Input edge must be EH and output edge must be normal.  */
  if ((e_in->flags & EDGE_EH) == 0 || (e_out->flags & EDGE_EH) != 0)
    return false;

  /* The block must be empty except for the labels.  */
  if (!gsi_end_p (gsi_after_labels (bb)))
    return false;

  /* The destination block must not already have a landing pad
     for a different region.  */
  for (gsi = gsi_start_bb (e_out->dest); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple stmt = gsi_stmt (gsi);
      tree lab;
      int lp_nr;

      if (gimple_code (stmt) != GIMPLE_LABEL)
	break;
      lab = gimple_label_label (stmt);
      lp_nr = EH_LANDING_PAD_NR (lab);
      if (lp_nr && get_eh_region_from_lp_number (lp_nr) != lp->region)
	return false;
    }

  /* The new destination block must not already be a destination of
     the source block, lest we merge fallthru and eh edges and get
     all sorts of confused.  */
  if (find_edge (e_in->src, e_out->dest))
    return false;

  /* ??? We can get degenerate phis due to cfg cleanups.  I would have
     thought this should have been cleaned up by a phicprop pass, but
     that doesn't appear to handle virtuals.  Propagate by hand.  */
  if (!gimple_seq_empty_p (phi_nodes (bb)))
    {
      for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); )
	{
	  gimple use_stmt, phi = gsi_stmt (gsi);
	  tree lhs = gimple_phi_result (phi);
	  tree rhs = gimple_phi_arg_def (phi, 0);
	  use_operand_p use_p;
	  imm_use_iterator iter;

	  FOR_EACH_IMM_USE_STMT (use_stmt, iter, lhs)
	    {
	      FOR_EACH_IMM_USE_ON_STMT (use_p, iter)
		SET_USE (use_p, rhs);
	    }

	  if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (lhs))
	    SSA_NAME_OCCURS_IN_ABNORMAL_PHI (rhs) = 1;

	  remove_phi_node (&gsi, true);
	}
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Unsplit EH landing pad %d to block %i.\n",
	     lp->index, e_out->dest->index);

  /* Redirect the edge.  Since redirect_eh_edge_1 expects to be moving
     a successor edge, humor it.  But do the real CFG change with the
     predecessor of E_OUT in order to preserve the ordering of arguments
     to the PHI nodes in E_OUT->DEST.  */
  redirect_eh_edge_1 (e_in, e_out->dest, false);
  redirect_edge_pred (e_out, e_in->src);
  e_out->flags = e_in->flags;
  e_out->probability = e_in->probability;
  e_out->count = e_in->count;
  remove_edge (e_in);

  return true;
}

/* Examine each landing pad block and see if it matches unsplit_eh.  */

static bool
unsplit_all_eh (void)
{
  bool changed = false;
  eh_landing_pad lp;
  int i;

  for (i = 1; VEC_iterate (eh_landing_pad, cfun->eh->lp_array, i, lp); ++i)
    if (lp)
      changed |= unsplit_eh (lp);

  return changed;
}

/* A subroutine of cleanup_empty_eh.  Redirect all EH edges incoming
   to OLD_BB to NEW_BB; return true on success, false on failure.

   OLD_BB_OUT is the edge into NEW_BB from OLD_BB, so if we miss any
   PHI variables from OLD_BB we can pick them up from OLD_BB_OUT.
   Virtual PHIs may be deleted and marked for renaming.  */

static bool
cleanup_empty_eh_merge_phis (basic_block new_bb, basic_block old_bb,
			     edge old_bb_out, bool change_region)
{
  gimple_stmt_iterator ngsi, ogsi;
  edge_iterator ei;
  edge e;
  bitmap rename_virts;
  bitmap ophi_handled;

  FOR_EACH_EDGE (e, ei, old_bb->preds)
    redirect_edge_var_map_clear (e);

  ophi_handled = BITMAP_ALLOC (NULL);
  rename_virts = BITMAP_ALLOC (NULL);

  /* First, iterate through the PHIs on NEW_BB and set up the edge_var_map
     for the edges we're going to move.  */
  for (ngsi = gsi_start_phis (new_bb); !gsi_end_p (ngsi); gsi_next (&ngsi))
    {
      gimple ophi, nphi = gsi_stmt (ngsi);
      tree nresult, nop;

      nresult = gimple_phi_result (nphi);
      nop = gimple_phi_arg_def (nphi, old_bb_out->dest_idx);

      /* Find the corresponding PHI in OLD_BB so we can forward-propagate
	 the source ssa_name.  */
      ophi = NULL;
      for (ogsi = gsi_start_phis (old_bb); !gsi_end_p (ogsi); gsi_next (&ogsi))
	{
	  ophi = gsi_stmt (ogsi);
	  if (gimple_phi_result (ophi) == nop)
	    break;
	  ophi = NULL;
	}

      /* If we did find the corresponding PHI, copy those inputs.  */
      if (ophi)
	{
	  bitmap_set_bit (ophi_handled, SSA_NAME_VERSION (nop));
	  FOR_EACH_EDGE (e, ei, old_bb->preds)
	    {
	      location_t oloc;
	      tree oop;

	      if ((e->flags & EDGE_EH) == 0)
		continue;
	      oop = gimple_phi_arg_def (ophi, e->dest_idx);
	      oloc = gimple_phi_arg_location (ophi, e->dest_idx);
	      redirect_edge_var_map_add (e, nresult, oop, oloc);
	    }
	}
      /* If we didn't find the PHI, but it's a VOP, remember to rename
	 it later, assuming all other tests succeed.  */
      else if (!is_gimple_reg (nresult))
	bitmap_set_bit (rename_virts, SSA_NAME_VERSION (nresult));
      /* If we didn't find the PHI, and it's a real variable, we know
	 from the fact that OLD_BB is tree_empty_eh_handler_p that the
	 variable is unchanged from input to the block and we can simply
	 re-use the input to NEW_BB from the OLD_BB_OUT edge.  */
      else
	{
	  location_t nloc
	    = gimple_phi_arg_location (nphi, old_bb_out->dest_idx);
	  FOR_EACH_EDGE (e, ei, old_bb->preds)
	    redirect_edge_var_map_add (e, nresult, nop, nloc);
	}
    }

  /* Second, verify that all PHIs from OLD_BB have been handled.  If not,
     we don't know what values from the other edges into NEW_BB to use.  */
  for (ogsi = gsi_start_phis (old_bb); !gsi_end_p (ogsi); gsi_next (&ogsi))
    {
      gimple ophi = gsi_stmt (ogsi);
      tree oresult = gimple_phi_result (ophi);
      if (!bitmap_bit_p (ophi_handled, SSA_NAME_VERSION (oresult)))
	goto fail;
    }

  /* At this point we know that the merge will succeed.  Remove the PHI
     nodes for the virtuals that we want to rename.  */
  if (!bitmap_empty_p (rename_virts))
    {
      for (ngsi = gsi_start_phis (new_bb); !gsi_end_p (ngsi); )
	{
	  gimple nphi = gsi_stmt (ngsi);
	  tree nresult = gimple_phi_result (nphi);
	  if (bitmap_bit_p (rename_virts, SSA_NAME_VERSION (nresult)))
	    {
	      mark_virtual_phi_result_for_renaming (nphi);
	      remove_phi_node (&ngsi, true);
	    }
	  else
	    gsi_next (&ngsi);
	}
    }

  /* Finally, move the edges and update the PHIs.  */
  for (ei = ei_start (old_bb->preds); (e = ei_safe_edge (ei)); )
    if (e->flags & EDGE_EH)
      {
	redirect_eh_edge_1 (e, new_bb, change_region);
	redirect_edge_succ (e, new_bb);
	flush_pending_stmts (e);
      }
    else
      ei_next (&ei);

  BITMAP_FREE (ophi_handled);
  BITMAP_FREE (rename_virts);
  return true;

 fail:
  FOR_EACH_EDGE (e, ei, old_bb->preds)
    redirect_edge_var_map_clear (e);
  BITMAP_FREE (ophi_handled);
  BITMAP_FREE (rename_virts);
  return false;
}

/* A subroutine of cleanup_empty_eh.  Move a landing pad LP from its
   old region to NEW_REGION at BB.  */

static void
cleanup_empty_eh_move_lp (basic_block bb, edge e_out,
			  eh_landing_pad lp, eh_region new_region)
{
  gimple_stmt_iterator gsi;
  eh_landing_pad *pp;

  for (pp = &lp->region->landing_pads; *pp != lp; pp = &(*pp)->next_lp)
    continue;
  *pp = lp->next_lp;

  lp->region = new_region;
  lp->next_lp = new_region->landing_pads;
  new_region->landing_pads = lp;

  /* Delete the RESX that was matched within the empty handler block.  */
  gsi = gsi_last_bb (bb);
  mark_virtual_ops_for_renaming (gsi_stmt (gsi));
  gsi_remove (&gsi, true);

  /* Clean up E_OUT for the fallthru.  */
  e_out->flags = (e_out->flags & ~EDGE_EH) | EDGE_FALLTHRU;
  e_out->probability = REG_BR_PROB_BASE;
}

/* A subroutine of cleanup_empty_eh.  Handle more complex cases of
   unsplitting than unsplit_eh was prepared to handle, e.g. when
   multiple incoming edges and phis are involved.  */

static bool
cleanup_empty_eh_unsplit (basic_block bb, edge e_out, eh_landing_pad lp)
{
  gimple_stmt_iterator gsi;
  tree lab;

  /* We really ought not have totally lost everything following
     a landing pad label.  Given that BB is empty, there had better
     be a successor.  */
  gcc_assert (e_out != NULL);

  /* The destination block must not already have a landing pad
     for a different region.  */
  lab = NULL;
  for (gsi = gsi_start_bb (e_out->dest); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple stmt = gsi_stmt (gsi);
      int lp_nr;

      if (gimple_code (stmt) != GIMPLE_LABEL)
	break;
      lab = gimple_label_label (stmt);
      lp_nr = EH_LANDING_PAD_NR (lab);
      if (lp_nr && get_eh_region_from_lp_number (lp_nr) != lp->region)
	return false;
    }

  /* Attempt to move the PHIs into the successor block.  */
  if (cleanup_empty_eh_merge_phis (e_out->dest, bb, e_out, false))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file,
		 "Unsplit EH landing pad %d to block %i "
		 "(via cleanup_empty_eh).\n",
		 lp->index, e_out->dest->index);
      return true;
    }

  return false;
}

/* Examine the block associated with LP to determine if it's an empty
   handler for its EH region.  If so, attempt to redirect EH edges to
   an outer region.  Return true the CFG was updated in any way.  This
   is similar to jump forwarding, just across EH edges.  */

static bool
cleanup_empty_eh (eh_landing_pad lp)
{
  basic_block bb = label_to_block (lp->post_landing_pad);
  gimple_stmt_iterator gsi;
  gimple resx;
  eh_region new_region;
  edge_iterator ei;
  edge e, e_out;
  bool has_non_eh_pred;
  int new_lp_nr;

  /* There can be zero or one edges out of BB.  This is the quickest test.  */
  switch (EDGE_COUNT (bb->succs))
    {
    case 0:
      e_out = NULL;
      break;
    case 1:
      e_out = EDGE_SUCC (bb, 0);
      break;
    default:
      return false;
    }
  gsi = gsi_after_labels (bb);

  /* Make sure to skip debug statements.  */
  if (!gsi_end_p (gsi) && is_gimple_debug (gsi_stmt (gsi)))
    gsi_next_nondebug (&gsi);

  /* If the block is totally empty, look for more unsplitting cases.  */
  if (gsi_end_p (gsi))
    return cleanup_empty_eh_unsplit (bb, e_out, lp);

  /* The block should consist only of a single RESX statement.  */
  resx = gsi_stmt (gsi);
  if (!is_gimple_resx (resx))
    return false;
  gcc_assert (gsi_one_before_end_p (gsi));

  /* Determine if there are non-EH edges, or resx edges into the handler.  */
  has_non_eh_pred = false;
  FOR_EACH_EDGE (e, ei, bb->preds)
    if (!(e->flags & EDGE_EH))
      has_non_eh_pred = true;

  /* Find the handler that's outer of the empty handler by looking at
     where the RESX instruction was vectored.  */
  new_lp_nr = lookup_stmt_eh_lp (resx);
  new_region = get_eh_region_from_lp_number (new_lp_nr);

  /* If there's no destination region within the current function,
     redirection is trivial via removing the throwing statements from
     the EH region, removing the EH edges, and allowing the block
     to go unreachable.  */
  if (new_region == NULL)
    {
      gcc_assert (e_out == NULL);
      for (ei = ei_start (bb->preds); (e = ei_safe_edge (ei)); )
	if (e->flags & EDGE_EH)
	  {
	    gimple stmt = last_stmt (e->src);
	    remove_stmt_from_eh_lp (stmt);
	    remove_edge (e);
	  }
	else
	  ei_next (&ei);
      goto succeed;
    }

  /* If the destination region is a MUST_NOT_THROW, allow the runtime
     to handle the abort and allow the blocks to go unreachable.  */
  if (new_region->type == ERT_MUST_NOT_THROW)
    {
      for (ei = ei_start (bb->preds); (e = ei_safe_edge (ei)); )
	if (e->flags & EDGE_EH)
	  {
	    gimple stmt = last_stmt (e->src);
	    remove_stmt_from_eh_lp (stmt);
	    add_stmt_to_eh_lp (stmt, new_lp_nr);
	    remove_edge (e);
	  }
	else
	  ei_next (&ei);
      goto succeed;
    }

  /* Try to redirect the EH edges and merge the PHIs into the destination
     landing pad block.  If the merge succeeds, we'll already have redirected
     all the EH edges.  The handler itself will go unreachable if there were
     no normal edges.  */
  if (cleanup_empty_eh_merge_phis (e_out->dest, bb, e_out, true))
    goto succeed;

  /* Finally, if all input edges are EH edges, then we can (potentially)
     reduce the number of transfers from the runtime by moving the landing
     pad from the original region to the new region.  This is a win when
     we remove the last CLEANUP region along a particular exception
     propagation path.  Since nothing changes except for the region with
     which the landing pad is associated, the PHI nodes do not need to be
     adjusted at all.  */
  if (!has_non_eh_pred)
    {
      cleanup_empty_eh_move_lp (bb, e_out, lp, new_region);
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Empty EH handler %i moved to EH region %i.\n",
		 lp->index, new_region->index);

      /* ??? The CFG didn't change, but we may have rendered the
	 old EH region unreachable.  Trigger a cleanup there.  */
      return true;
    }

  return false;

 succeed:
  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Empty EH handler %i removed.\n", lp->index);
  remove_eh_landing_pad (lp);
  return true;
}

/* Do a post-order traversal of the EH region tree.  Examine each
   post_landing_pad block and see if we can eliminate it as empty.  */

static bool
cleanup_all_empty_eh (void)
{
  bool changed = false;
  eh_landing_pad lp;
  int i;

  for (i = 1; VEC_iterate (eh_landing_pad, cfun->eh->lp_array, i, lp); ++i)
    if (lp)
      changed |= cleanup_empty_eh (lp);

  return changed;
}

/* Perform cleanups and lowering of exception handling
    1) cleanups regions with handlers doing nothing are optimized out
    2) MUST_NOT_THROW regions that became dead because of 1) are optimized out
    3) Info about regions that are containing instructions, and regions
       reachable via local EH edges is collected
    4) Eh tree is pruned for regions no longer neccesary.

   TODO: Push MUST_NOT_THROW regions to the root of the EH tree.
	 Unify those that have the same failure decl and locus.
*/

static unsigned int
execute_cleanup_eh (void)
{
  /* Do this first: unsplit_all_eh and cleanup_all_empty_eh can die
     looking up unreachable landing pads.  */
  remove_unreachable_handlers ();

  /* Watch out for the region tree vanishing due to all unreachable.  */
  if (cfun->eh->region_tree && optimize)
    {
      bool changed = false;

      changed |= unsplit_all_eh ();
      changed |= cleanup_all_empty_eh ();

      if (changed)
	{
	  free_dominance_info (CDI_DOMINATORS);
	  free_dominance_info (CDI_POST_DOMINATORS);

          /* We delayed all basic block deletion, as we may have performed
	     cleanups on EH edges while non-EH edges were still present.  */
	  delete_unreachable_blocks ();

	  /* We manipulated the landing pads.  Remove any region that no
	     longer has a landing pad.  */
	  remove_unreachable_handlers_no_lp ();

	  return TODO_cleanup_cfg | TODO_update_ssa_only_virtuals;
	}
    }

  return 0;
}

static bool
gate_cleanup_eh (void)
{
  return cfun->eh != NULL && cfun->eh->region_tree != NULL;
}

struct gimple_opt_pass pass_cleanup_eh = {
  {
   GIMPLE_PASS,
   "ehcleanup",			/* name */
   gate_cleanup_eh,		/* gate */
   execute_cleanup_eh,		/* execute */
   NULL,			/* sub */
   NULL,			/* next */
   0,				/* static_pass_number */
   TV_TREE_EH,			/* tv_id */
   PROP_gimple_lcf,		/* properties_required */
   0,				/* properties_provided */
   0,				/* properties_destroyed */
   0,				/* todo_flags_start */
   TODO_dump_func		/* todo_flags_finish */
   }
};

/* Verify that BB containing STMT as the last statement, has precisely the
   edge that make_eh_edges would create.  */

bool
verify_eh_edges (gimple stmt)
{
  basic_block bb = gimple_bb (stmt);
  eh_landing_pad lp = NULL;
  int lp_nr;
  edge_iterator ei;
  edge e, eh_edge;

  lp_nr = lookup_stmt_eh_lp (stmt);
  if (lp_nr > 0)
    lp = get_eh_landing_pad_from_number (lp_nr);

  eh_edge = NULL;
  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      if (e->flags & EDGE_EH)
	{
	  if (eh_edge)
	    {
	      error ("BB %i has multiple EH edges", bb->index);
	      return true;
	    }
	  else
	    eh_edge = e;
	}
    }

  if (lp == NULL)
    {
      if (eh_edge)
	{
	  error ("BB %i can not throw but has an EH edge", bb->index);
	  return true;
	}
      return false;
    }

  if (!stmt_could_throw_p (stmt))
    {
      error ("BB %i last statement has incorrectly set lp", bb->index);
      return true;
    }

  if (eh_edge == NULL)
    {
      error ("BB %i is missing an EH edge", bb->index);
      return true;
    }

  if (eh_edge->dest != label_to_block (lp->post_landing_pad))
    {
      error ("Incorrect EH edge %i->%i", bb->index, eh_edge->dest->index);
      return true;
    }

  return false;
}

/* Similarly, but handle GIMPLE_EH_DISPATCH specifically.  */

bool
verify_eh_dispatch_edge (gimple stmt)
{
  eh_region r;
  eh_catch c;
  basic_block src, dst;
  bool want_fallthru = true;
  edge_iterator ei;
  edge e, fall_edge;

  r = get_eh_region_from_number (gimple_eh_dispatch_region (stmt));
  src = gimple_bb (stmt);

  FOR_EACH_EDGE (e, ei, src->succs)
    gcc_assert (e->aux == NULL);

  switch (r->type)
    {
    case ERT_TRY:
      for (c = r->u.eh_try.first_catch; c ; c = c->next_catch)
	{
	  dst = label_to_block (c->label);
	  e = find_edge (src, dst);
	  if (e == NULL)
	    {
	      error ("BB %i is missing an edge", src->index);
	      return true;
	    }
	  e->aux = (void *)e;

	  /* A catch-all handler doesn't have a fallthru.  */
	  if (c->type_list == NULL)
	    {
	      want_fallthru = false;
	      break;
	    }
	}
      break;

    case ERT_ALLOWED_EXCEPTIONS:
      dst = label_to_block (r->u.allowed.label);
      e = find_edge (src, dst);
      if (e == NULL)
	{
	  error ("BB %i is missing an edge", src->index);
	  return true;
	}
      e->aux = (void *)e;
      break;

    default:
      gcc_unreachable ();
    }

  fall_edge = NULL;
  FOR_EACH_EDGE (e, ei, src->succs)
    {
      if (e->flags & EDGE_FALLTHRU)
	{
	  if (fall_edge != NULL)
	    {
	      error ("BB %i too many fallthru edges", src->index);
	      return true;
	    }
	  fall_edge = e;
	}
      else if (e->aux)
	e->aux = NULL;
      else
	{
	  error ("BB %i has incorrect edge", src->index);
	  return true;
	}
    }
  if ((fall_edge != NULL) ^ want_fallthru)
    {
      error ("BB %i has incorrect fallthru edge", src->index);
      return true;
    }

  return false;
}
