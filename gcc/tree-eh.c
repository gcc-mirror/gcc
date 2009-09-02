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


/* Remember and lookup EH region data for arbitrary statements.
   Really this means any statement that could_throw_p.  We could
   stuff this information into the stmt_ann data structure, but:

   (1) We absolutely rely on this information being kept until
   we get to rtl.  Once we're done with lowering here, if we lose
   the information there's no way to recover it!

   (2) There are many more statements that *cannot* throw as
   compared to those that can.  We should be saving some amount
   of space by only allocating memory for those that can throw.  */

static void
record_stmt_eh_region (struct eh_region_d *region, gimple t)
{
  if (!region)
    return;

  add_stmt_to_eh_region (t, get_eh_region_number (region));
}


/* Add statement T in function IFUN to EH region NUM.  */

void
add_stmt_to_eh_region_fn (struct function *ifun, gimple t, int num)
{
  struct throw_stmt_node *n;
  void **slot;

  gcc_assert (num >= 0);
  gcc_assert (gimple_code (t) != GIMPLE_RESX);

  n = GGC_NEW (struct throw_stmt_node);
  n->stmt = t;
  n->region_nr = num;

  if (!get_eh_throw_stmt_table (ifun))
    set_eh_throw_stmt_table (ifun, htab_create_ggc (31, struct_ptr_hash,
						    struct_ptr_eq,
						    ggc_free));

  slot = htab_find_slot (get_eh_throw_stmt_table (ifun), n, INSERT);
  gcc_assert (!*slot);
  *slot = n;
}


/* Add statement T in the current function (cfun) to EH region number
   NUM.  */

void
add_stmt_to_eh_region (gimple t, int num)
{
  add_stmt_to_eh_region_fn (cfun, t, num);
}


/* Remove statement T in function IFUN from the EH region holding it.  */

bool
remove_stmt_from_eh_region_fn (struct function *ifun, gimple t)
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


/* Remove statement T in the current function (cfun) from the EH
   region holding it.  */

bool
remove_stmt_from_eh_region (gimple t)
{
  return remove_stmt_from_eh_region_fn (cfun, t);
}

/* Determine if statement T is inside an EH region in function IFUN.
   Return the EH region number if found, return -2 if IFUN does not
   have an EH table and -1 if T could not be found in IFUN's EH region
   table.  */

int
lookup_stmt_eh_region_fn (struct function *ifun, gimple t)
{
  struct throw_stmt_node *p, n;

  if (!get_eh_throw_stmt_table (ifun))
    return -2;

  n.stmt = t;
  p = (struct throw_stmt_node *) htab_find (get_eh_throw_stmt_table (ifun), &n);
  return (p ? p->region_nr : -1);
}


/* Determine if statement T is inside an EH region in the current
   function (cfun).  Return the EH region number if found, return -2
   if cfun does not have an EH table and -1 if T could not be found in
   cfun's EH region table.  */

int
lookup_stmt_eh_region (gimple t)
{
  /* We can get called from initialized data when -fnon-call-exceptions
     is on; prevent crash.  */
  if (!cfun)
    return -1;

  return lookup_stmt_eh_region_fn (cfun, t);
}


/* Determine if expression T is inside an EH region in the current
   function (cfun).  Return the EH region number if found, return -2
   if IFUN does not have an EH table and -1 if T could not be found in
   IFUN's EH region table.  */

int
lookup_expr_eh_region (tree t)
{
  /* We can get called from initialized data when -fnon-call-exceptions
     is on; prevent crash.  */
  if (!cfun)
    return -1;

  if (!get_eh_throw_stmt_table (cfun))
    return -2;

  if (t && EXPR_P (t))
    {
      tree_ann_common_t ann = tree_common_ann (t);
      if (ann)
	return (int) ann->rn;
    }

  return -1;
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
  struct eh_region_d *cur_region;

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
  struct eh_region_d *region;

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

  /* A label that has been registered with except.c to be the
     landing pad for this try block.  */
  tree eh_label;

  /* True if it is possible to fall out the bottom of the try block.
     Cleared if the fallthru is converted to a goto.  */
  bool may_fallthru;

  /* True if any entry in goto_queue is a GIMPLE_RETURN.  */
  bool may_return;

  /* True if the finally block can receive an exception edge.
     Cleared if the exception case is handled by code duplication.  */
  bool may_throw;
};

static gimple_seq lower_eh_filter (struct leh_state *, gimple);

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

  q->cont_stmt = gimple_build_goto (VEC_index (tree, tf->dest_array,q->index));

  if (mod)
    gimple_seq_add_seq (&q->repl_stmt, mod);

  x = gimple_build_goto (finlab);
  gimple_seq_add_stmt (&q->repl_stmt, x);
}

/* We want to transform
	try { body; } catch { stuff; }
   to
	body; goto over; lab: stuff; over:

   TP is a GIMPLE_TRY node.  LAB is the label that
   should be placed before the second operand, or NULL.  OVER is
   an existing label that should be put at the exit, or NULL.  */

static gimple_seq
frob_into_branch_around (gimple tp, tree lab, tree over)
{
  gimple x;
  gimple_seq cleanup, result;
  location_t loc = gimple_location (tp);

  cleanup = gimple_try_cleanup (tp);
  result = gimple_try_eval (tp);

  if (gimple_seq_may_fallthru (result))
    {
      if (!over)
	over = create_artificial_label (loc);
      x = gimple_build_goto (over);
      gimple_seq_add_stmt (&result, x);
    }

  if (lab)
    {
      x = gimple_build_label (lab);
      gimple_seq_add_stmt (&result, x);
    }

  gimple_seq_add_seq (&result, cleanup);

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
  gimple protect_cleanup_actions;
  gimple_stmt_iterator gsi;
  bool finally_may_fallthru;
  gimple_seq finally;
  gimple x;

  /* First check for nothing to do.  */
  if (lang_protect_cleanup_actions)
    protect_cleanup_actions = lang_protect_cleanup_actions ();
  else
    protect_cleanup_actions = NULL;

  finally = gimple_try_cleanup (tf->top_p);

  /* If the EH case of the finally block can fall through, this may be a
     structure of the form
	try {
	  try {
	    throw ...;
	  } cleanup {
	    try {
	      throw ...;
	    } catch (...) {
	    }
	  }
	} catch (...) {
	  yyy;
	}
    E.g. with an inline destructor with an embedded try block.  In this
    case we must save the runtime EH data around the nested exception.

    This complication means that any time the previous runtime data might
    be used (via fallthru from the finally) we handle the eh case here,
    whether or not protect_cleanup_actions is active.  */

  finally_may_fallthru = gimple_seq_may_fallthru (finally);
  if (!finally_may_fallthru && !protect_cleanup_actions)
    return;

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
  if (protect_cleanup_actions
      && gimple_code (x) == GIMPLE_TRY
      && gimple_try_kind (x) == GIMPLE_TRY_CATCH
      && gimple_try_catch_is_cleanup (x))
    {
      gsi_insert_seq_before (&gsi, gimple_try_eval (x), GSI_SAME_STMT);
      gsi_remove (&gsi, false);
    }

  /* Resume execution after the exception.  Adding this now lets
     lower_eh_filter not add unnecessary gotos, as it is clear that
     we never fallthru from this copy of the finally block.  */
  if (finally_may_fallthru)
    {
      tree save_eptr, save_filt;
      tree tmp;

      save_eptr = create_tmp_var (ptr_type_node, "save_eptr");
      save_filt = create_tmp_var (integer_type_node, "save_filt");

      gsi = gsi_start (finally);
      tmp = build0 (EXC_PTR_EXPR, ptr_type_node);
      x = gimple_build_assign (save_eptr, tmp);
      gsi_insert_before (&gsi, x, GSI_CONTINUE_LINKING);

      tmp = build0 (FILTER_EXPR, integer_type_node);
      x = gimple_build_assign (save_filt, tmp);
      gsi_insert_before (&gsi, x, GSI_CONTINUE_LINKING);

      gsi = gsi_last (finally);
      tmp = build0 (EXC_PTR_EXPR, ptr_type_node);
      x = gimple_build_assign (tmp, save_eptr);
      gsi_insert_after (&gsi, x, GSI_CONTINUE_LINKING);

      tmp = build0 (FILTER_EXPR, integer_type_node);
      x = gimple_build_assign (tmp, save_filt);
      gsi_insert_after (&gsi, x, GSI_CONTINUE_LINKING);

      x = gimple_build_resx (get_eh_region_number (tf->region));
      gsi_insert_after (&gsi, x, GSI_CONTINUE_LINKING);
    }

  /* Wrap the block with protect_cleanup_actions as the action.  */
  if (protect_cleanup_actions)
    {
      gimple_seq seq = NULL, failure = NULL;

      gimple_seq_add_stmt (&failure, protect_cleanup_actions);
      x = gimple_build_eh_filter (NULL, failure);
      gimple_eh_filter_set_must_not_throw (x, 1);

      gimple_seq_add_stmt (&seq, x);
      x = gimple_build_try (finally, seq, GIMPLE_TRY_CATCH);
      finally = lower_eh_filter (outer_state, x);
    }
  else
    lower_eh_constructs_1 (outer_state, finally);

  /* Hook this up to the end of the existing try block.  If we
     previously fell through the end, we'll have to branch around.
     This means adding a new goto, and adding it to the queue.  */

  gsi = gsi_last (gimple_try_eval (tf->top_p));

  if (tf->may_fallthru)
    {
      tree tmp;
      tmp = lower_try_finally_fallthru_label (tf);
      x = gimple_build_goto (tmp);
      gsi_insert_after (&gsi, x, GSI_CONTINUE_LINKING);

      if (this_state)
        maybe_record_in_goto_queue (this_state, x);

      tf->may_fallthru = false;
    }

  x = gimple_build_label (tf->eh_label);
  gsi_insert_after (&gsi, x, GSI_CONTINUE_LINKING);
  gsi_insert_seq_after (&gsi, finally, GSI_CONTINUE_LINKING);

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

  if (tf->may_throw)
    lab = tf->eh_label;
  else
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

      x = gimple_build_label (tf->eh_label);
      gimple_seq_add_stmt (&tf->top_p_seq, x);

      gimple_seq_add_seq (&tf->top_p_seq, finally);

      x = gimple_build_resx (get_eh_region_number (tf->region));

      gimple_seq_add_stmt (&tf->top_p_seq, x);

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
      x = gimple_build_label (tf->eh_label);
      gimple_seq_add_stmt (&new_stmt, x);

      seq = lower_try_finally_dup_block (finally, state);
      lower_eh_constructs_1 (state, seq);
      gimple_seq_add_seq (&new_stmt, seq);

      x = gimple_build_resx (get_eh_region_number (tf->region));
      gimple_seq_add_stmt (&new_stmt, x);
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
      x = gimple_build_assign (finally_tmp, build_int_cst (integer_type_node,
					                   fallthru_index));
      gimple_seq_add_stmt (&tf->top_p_seq, x);

      if (tf->may_throw)
	{
	  x = gimple_build_goto (finally_label);
          gimple_seq_add_stmt (&tf->top_p_seq, x);
	}


      last_case = build3 (CASE_LABEL_EXPR, void_type_node,
			  build_int_cst (NULL_TREE, fallthru_index), NULL,
			  create_artificial_label (tf_loc));
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
      x = gimple_build_label (tf->eh_label);
      gimple_seq_add_stmt (&tf->top_p_seq, x);

      x = gimple_build_assign (finally_tmp, build_int_cst (integer_type_node,
                                                           eh_index));
      gimple_seq_add_stmt (&tf->top_p_seq, x);

      last_case = build3 (CASE_LABEL_EXPR, void_type_node,
			  build_int_cst (NULL_TREE, eh_index), NULL,
			  create_artificial_label (tf_loc));
      VEC_quick_push (tree, case_label_vec, last_case);
      last_case_index++;

      x = gimple_build_label (CASE_LABEL (last_case));
      gimple_seq_add_stmt (&switch_body, x);
      x = gimple_build_resx (get_eh_region_number (tf->region));
      gimple_seq_add_stmt (&switch_body, x);
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
				   build_int_cst (integer_type_node,
						  return_index));
	  gimple_seq_add_stmt (&mod, x);
	  do_return_redirection (q, finally_label, mod, &return_val);
	  switch_id = return_index;
	}
      else
	{
	  x = gimple_build_assign (finally_tmp,
				   build_int_cst (integer_type_node, q->index));
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
                             build_int_cst (NULL_TREE, switch_id), NULL,
                             NULL);
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
  location_t tf_loc = gimple_location (tp);

  /* Process the try block.  */

  memset (&this_tf, 0, sizeof (this_tf));
  this_tf.try_finally_expr = tp;
  this_tf.top_p = tp;
  this_tf.outer = state;
  if (using_eh_for_cleanups_p)
    this_tf.region
      = gen_eh_region_cleanup (state->cur_region);
  else
    this_tf.region = NULL;

  this_state.cur_region = this_tf.region;
  this_state.tf = &this_tf;

  lower_eh_constructs_1 (&this_state, gimple_try_eval(tp));

  /* Determine if the try block is escaped through the bottom.  */
  this_tf.may_fallthru = gimple_seq_may_fallthru (gimple_try_eval (tp));

  /* Determine if any exceptions are possible within the try block.  */
  if (using_eh_for_cleanups_p)
    this_tf.may_throw = get_eh_region_may_contain_throw (this_tf.region);
  if (this_tf.may_throw)
    {
      this_tf.eh_label = create_artificial_label (tf_loc);
      set_eh_region_tree_label (this_tf.region, this_tf.eh_label);
      honor_protect_cleanup_actions (state, &this_state, &this_tf);
    }

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
  struct eh_region_d *try_region;
  struct leh_state this_state;
  gimple_stmt_iterator gsi;
  tree out_label;
  location_t try_catch_loc = gimple_location (tp);

  try_region = gen_eh_region_try (state->cur_region);
  this_state.cur_region = try_region;
  this_state.tf = state->tf;

  lower_eh_constructs_1 (&this_state, gimple_try_eval (tp));

  if (!get_eh_region_may_contain_throw (try_region))
    {
      return gimple_try_eval (tp);
    }

  out_label = NULL;
  for (gsi = gsi_start (gimple_try_cleanup (tp)); !gsi_end_p (gsi); )
    {
      struct eh_region_d *catch_region;
      tree eh_label;
      gimple x, gcatch;

      gcatch = gsi_stmt (gsi);
      catch_region = gen_eh_region_catch (try_region,
                                          gimple_catch_types (gcatch));

      this_state.cur_region = catch_region;
      lower_eh_constructs_1 (&this_state, gimple_catch_handler (gcatch));

      eh_label = create_artificial_label (try_catch_loc);
      set_eh_region_tree_label (catch_region, eh_label);

      x = gimple_build_label (eh_label);
      gsi_insert_before (&gsi, x, GSI_SAME_STMT);

      if (gimple_seq_may_fallthru (gimple_catch_handler (gcatch)))
	{
	  if (!out_label)
	    out_label = create_artificial_label (try_catch_loc);

	  x = gimple_build_goto (out_label);
	  gimple_seq_add_stmt (gimple_catch_handler_ptr (gcatch), x);
	}

      gsi_insert_seq_before (&gsi, gimple_catch_handler (gcatch),
			     GSI_SAME_STMT);
      gsi_remove (&gsi, false);
    }

  return frob_into_branch_around (tp, NULL, out_label);
}

/* A subroutine of lower_eh_constructs_1.  Lower a GIMPLE_TRY with a
   GIMPLE_EH_FILTER to a sequence of labels and blocks, plus the exception
   region trees that record all the magic.  */

static gimple_seq
lower_eh_filter (struct leh_state *state, gimple tp)
{
  struct leh_state this_state;
  struct eh_region_d *this_region;
  gimple inner;
  tree eh_label;

  inner = gimple_seq_first_stmt (gimple_try_cleanup (tp));

  if (gimple_eh_filter_must_not_throw (inner))
    this_region = gen_eh_region_must_not_throw (state->cur_region);
  else
    this_region = gen_eh_region_allowed (state->cur_region,
					 gimple_eh_filter_types (inner));
  this_state = *state;
  this_state.cur_region = this_region;

  lower_eh_constructs_1 (&this_state, gimple_try_eval (tp));

  if (!get_eh_region_may_contain_throw (this_region))
    {
      return gimple_try_eval (tp);
    }

  lower_eh_constructs_1 (state, gimple_eh_filter_failure (inner));
  gimple_try_set_cleanup (tp, gimple_eh_filter_failure (inner));

  eh_label = create_artificial_label (gimple_location (inner));
  set_eh_region_tree_label (this_region, eh_label);

  return frob_into_branch_around (tp, eh_label, NULL);
}

/* Implement a cleanup expression.  This is similar to try-finally,
   except that we only execute the cleanup block for exception edges.  */

static gimple_seq
lower_cleanup (struct leh_state *state, gimple tp)
{
  struct leh_state this_state;
  struct eh_region_d *this_region;
  struct leh_tf_state fake_tf;
  gimple_seq result;

  /* If not using eh, then exception-only cleanups are no-ops.  */
  if (!flag_exceptions)
    {
      result = gimple_try_eval (tp);
      lower_eh_constructs_1 (state, result);
      return result;
    }

  this_region = gen_eh_region_cleanup (state->cur_region);
  this_state = *state;
  this_state.cur_region = this_region;

  lower_eh_constructs_1 (&this_state, gimple_try_eval (tp));

  if (!get_eh_region_may_contain_throw (this_region))
    {
      return gimple_try_eval (tp);
    }

  /* Build enough of a try-finally state so that we can reuse
     honor_protect_cleanup_actions.  */
  memset (&fake_tf, 0, sizeof (fake_tf));
  fake_tf.top_p = fake_tf.try_finally_expr = tp;
  fake_tf.outer = state;
  fake_tf.region = this_region;
  fake_tf.may_fallthru = gimple_seq_may_fallthru (gimple_try_eval (tp));
  fake_tf.may_throw = true;

  fake_tf.eh_label = create_artificial_label (gimple_location (tp));
  set_eh_region_tree_label (this_region, fake_tf.eh_label);

  honor_protect_cleanup_actions (state, NULL, &fake_tf);

  if (fake_tf.may_throw)
    {
      /* In this case honor_protect_cleanup_actions had nothing to do,
	 and we should process this normally.  */
      lower_eh_constructs_1 (state, gimple_try_cleanup (tp));
      result = frob_into_branch_around (tp, fake_tf.eh_label,
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
    case GIMPLE_ASSIGN:
      /* If the stmt can throw use a new temporary for the assignment
         to a LHS.  This makes sure the old value of the LHS is
	 available on the EH edge.  */
      if (stmt_could_throw_p (stmt)
	  && gimple_has_lhs (stmt)
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
	  switch (gimple_code (x))
	    {
	    case GIMPLE_CATCH:
	      replace = lower_catch (state, stmt);
	      break;
	    case GIMPLE_EH_FILTER:
	      replace = lower_eh_filter (state, stmt);
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

  gimple_seq bodyp = gimple_body (current_function_decl);

  finally_tree = htab_create (31, struct_ptr_hash, struct_ptr_eq, free);

  collect_finally_tree_1 (bodyp, NULL);

  memset (&null_state, 0, sizeof (null_state));
  lower_eh_constructs_1 (&null_state, bodyp);

  htab_delete (finally_tree);

  collect_eh_region_array ();
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


/* Construct EH edges for STMT.  */

static void
make_eh_edge (struct eh_region_d *region, void *data)
{
  gimple stmt;
  tree lab;
  basic_block src, dst;

  stmt = (gimple) data;
  lab = get_eh_region_tree_label (region);

  src = gimple_bb (stmt);
  dst = label_to_block (lab);

  make_edge (src, dst, EDGE_EH);
}

/* See if STMT is call that might be inlined.  */

static bool
inlinable_call_p (gimple stmt)
{
  tree decl;
  if (gimple_code (stmt) != GIMPLE_CALL)
    return false;
  if (cfun->after_inlining)
    return false;
  /* Indirect calls can be propagated to direct call
     and inlined.  */
  decl = gimple_call_fndecl (stmt);
  if (!decl)
    return true;
  if (cgraph_function_flags_ready
      && cgraph_function_body_availability (cgraph_node (decl))
      < AVAIL_OVERWRITABLE)
    return false;
  return !DECL_UNINLINABLE (decl);
}

void
make_eh_edges (gimple stmt)
{
  int region_nr;
  bool is_resx;
  bool inlinable = false;
  basic_block bb;

  if (gimple_code (stmt) == GIMPLE_RESX)
    {
      region_nr = gimple_resx_region (stmt);
      is_resx = true;
    }
  else
    {
      region_nr = lookup_stmt_eh_region (stmt);
      if (region_nr < 0)
	return;
      is_resx = false;
      inlinable = inlinable_call_p (stmt);
    }

  foreach_reachable_handler (region_nr, is_resx, inlinable, make_eh_edge, stmt);

  /* Make CFG profile more consistent assuming that exception will resume to first
     available EH handler.  In practice this makes little difference, but we get
     fewer consistency errors in the dumps.  */
  bb = gimple_bb (stmt);
  if (is_resx && EDGE_COUNT (bb->succs))
    EDGE_SUCC (bb, 0)->probability = REG_BR_PROB_BASE;
}

/* Redirect EH edge E to NEW_BB.  */

edge
redirect_eh_edge (edge e, basic_block new_bb)
{
  gimple stmt = gsi_stmt (gsi_last_bb (e->src));
  int region_nr, new_region_nr;
  bool is_resx;
  bool inlinable = false;
  tree label = gimple_block_label (new_bb);
  struct eh_region_d *r;

  if (gimple_code (stmt) == GIMPLE_RESX)
    {
      region_nr = gimple_resx_region (stmt);
      is_resx = true;
    }
  else
    {
      region_nr = lookup_stmt_eh_region (stmt);
      gcc_assert (region_nr >= 0);
      is_resx = false;
      inlinable = inlinable_call_p (stmt);
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Redirecting EH edge %i->%i to %i, region %i, resx %i\n",
	     e->src->index, e->dest->index, new_bb->index, region_nr, is_resx);
  r = redirect_eh_edge_to_label (e, label, is_resx, inlinable, region_nr);
  new_region_nr = get_eh_region_number (r);
  if (new_region_nr != region_nr)
    {
      if (is_resx)
        gimple_resx_set_region (stmt, new_region_nr);
      else
        {
	  remove_stmt_from_eh_region (stmt);
	  add_stmt_to_eh_region (stmt, new_region_nr);
        }
    }
  e = ssa_redirect_edge (e, new_bb);
  return e;
}

static bool mark_eh_edge_found_error;

/* Mark edge make_eh_edge would create for given region by setting it aux
   field, output error if something goes wrong.  */

static void
mark_eh_edge (struct eh_region_d *region, void *data)
{
  gimple stmt;
  tree lab;
  basic_block src, dst;
  edge e;

  stmt = (gimple) data;
  lab = get_eh_region_tree_label (region);

  src = gimple_bb (stmt);
  dst = label_to_block (lab);

  e = find_edge (src, dst);
  if (!e)
    {
      error ("EH edge %i->%i is missing", src->index, dst->index);
      mark_eh_edge_found_error = true;
    }
  else if (!(e->flags & EDGE_EH))
    {
      error ("EH edge %i->%i miss EH flag", src->index, dst->index);
      mark_eh_edge_found_error = true;
    }
  else if (e->aux)
    {
      /* ??? might not be mistake.  */
      error ("EH edge %i->%i has duplicated regions", src->index, dst->index);
      mark_eh_edge_found_error = true;
    }
  else
    e->aux = (void *)1;
}

/* Verify that BB containing STMT as the last statement, has precisely the
   edges that make_eh_edges would create.  */

bool
verify_eh_edges (gimple stmt)
{
  int region_nr;
  bool is_resx;
  basic_block bb = gimple_bb (stmt);
  edge_iterator ei;
  edge e;
  bool inlinable = false;

  FOR_EACH_EDGE (e, ei, bb->succs)
    gcc_assert (!e->aux);
  mark_eh_edge_found_error = false;
  if (gimple_code (stmt) == GIMPLE_RESX)
    {
      region_nr = gimple_resx_region (stmt);
      is_resx = true;
    }
  else
    {
      region_nr = lookup_stmt_eh_region (stmt);
      if (region_nr < 0)
	{
	  FOR_EACH_EDGE (e, ei, bb->succs)
	    if (e->flags & EDGE_EH)
	      {
		error ("BB %i can not throw but has EH edges", bb->index);
		return true;
	      }
	   return false;
	}
      if (!stmt_could_throw_p (stmt))
	{
	  error ("BB %i last statement has incorrectly set region", bb->index);
	  return true;
	}
      inlinable = inlinable_call_p (stmt);
      is_resx = false;
    }

  foreach_reachable_handler (region_nr, is_resx, inlinable, mark_eh_edge, stmt);
  FOR_EACH_EDGE (e, ei, bb->succs)
    {
      if ((e->flags & EDGE_EH) && !e->aux)
	{
	  error ("unnecessary EH edge %i->%i", bb->index, e->dest->index);
	  mark_eh_edge_found_error = true;
	  return true;
	}
      e->aux = NULL;
    }

  return mark_eh_edge_found_error;
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
  enum gimple_code code;

  if (!flag_exceptions)
    return false;

  /* The only statements that can throw an exception are assignments,
     conditionals, calls and asms.  */
  code = gimple_code (stmt);
  if (code != GIMPLE_ASSIGN
      && code != GIMPLE_COND
      && code != GIMPLE_CALL
      && code != GIMPLE_ASM)
    return false;

  /* If exceptions can only be thrown by function calls and STMT is not a
     GIMPLE_CALL, the statement cannot throw.  */
  if (!flag_non_call_exceptions && code != GIMPLE_CALL)
    return false;

  if (code == GIMPLE_ASSIGN || code == GIMPLE_COND)
    return stmt_could_throw_1_p (stmt);
  else if (is_gimple_call (stmt))
    return (gimple_call_flags (stmt) & ECF_NOTHROW) == 0;
  else if (gimple_code (stmt) == GIMPLE_ASM)
    return (gimple_asm_volatile_p (stmt));
  else
    gcc_unreachable ();

  return false;
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
  int region_nr;
  bool is_resx = false;
  bool inlinable_call = false;

  if (!stmt_could_throw_p (stmt))
    return false;

  if (gimple_code (stmt) == GIMPLE_RESX)
    {
      region_nr = gimple_resx_region (stmt);
      is_resx = true;
    }
  else
    region_nr = lookup_stmt_eh_region (stmt);

  if (region_nr < 0)
    return true;

  return can_throw_external_1 (region_nr, is_resx, inlinable_call);
}

/* Return true if STMT can throw an exception that is caught within
   the current function (CFUN).  */

bool
stmt_can_throw_internal (gimple stmt)
{
  int region_nr;
  bool is_resx = false;
  bool inlinable_call = false;

  if (gimple_code (stmt) == GIMPLE_RESX)
    {
      region_nr = gimple_resx_region (stmt);
      is_resx = true;
    }
  else
    {
      region_nr = lookup_stmt_eh_region (stmt);
      inlinable_call = inlinable_call_p (stmt);
    }

  if (region_nr < 0)
    return false;

  return can_throw_internal_1 (region_nr, is_resx, inlinable_call);
}


/* Given a statement OLD_STMT and a new statement NEW_STMT that has replaced
   OLD_STMT in the function, remove OLD_STMT from the EH table and put NEW_STMT
   in the table if it should be in there.  Return TRUE if a replacement was
   done that my require an EH edge purge.  */

bool 
maybe_clean_or_replace_eh_stmt (gimple old_stmt, gimple new_stmt) 
{
  int region_nr = lookup_stmt_eh_region (old_stmt);

  if (region_nr >= 0)
    {
      bool new_stmt_could_throw = stmt_could_throw_p (new_stmt);

      if (new_stmt == old_stmt && new_stmt_could_throw)
	return false;

      remove_stmt_from_eh_region (old_stmt);
      if (new_stmt_could_throw)
	{
	  add_stmt_to_eh_region (new_stmt, region_nr);
	  return false;
	}
      else
	return true;
    }

  return false;
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

struct gimple_opt_pass pass_refactor_eh =
{
 {
  GIMPLE_PASS,
  "ehopt",				/* name */
  NULL,					/* gate */
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

/* Walk statements, see what regions are really references and remove unreachable ones.  */

static void
tree_remove_unreachable_handlers (void)
{
  sbitmap reachable, contains_stmt;
  VEC(int,heap) * label_to_region;
  basic_block bb;

  label_to_region = label_to_region_map ();
  reachable = sbitmap_alloc (num_eh_regions ());
  sbitmap_zero (reachable);
  contains_stmt = sbitmap_alloc (num_eh_regions ());
  sbitmap_zero (contains_stmt);

  FOR_EACH_BB (bb)
  {
    gimple_stmt_iterator gsi;
    int region;
    bool has_eh_preds = bb_has_eh_pred (bb);

    for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
      {
	gimple stmt = gsi_stmt (gsi);

	if (gimple_code (stmt) == GIMPLE_LABEL && has_eh_preds)
	  {
	    int uid = LABEL_DECL_UID (gimple_label_label (stmt));
	    int region;

	    for (region = VEC_index (int, label_to_region, uid);
		 region; region = get_next_region_sharing_label (region))
	      SET_BIT (reachable, region);
	  }
	if (gimple_code (stmt) == GIMPLE_RESX)
	  SET_BIT (reachable,
	  	   VEC_index (eh_region, cfun->eh->region_array,
		   	      gimple_resx_region (stmt))->region_number);
	if ((region = lookup_stmt_eh_region (stmt)) >= 0)
	  SET_BIT (contains_stmt, region);
      }
  }

  if (dump_file)
    {
      fprintf (dump_file, "Before removal of unreachable regions:\n");
      dump_eh_tree (dump_file, cfun);
      fprintf (dump_file, "Reachable regions: ");
      dump_sbitmap_file (dump_file, reachable);
      fprintf (dump_file, "Regions containing insns: ");
      dump_sbitmap_file (dump_file, contains_stmt);
    }

  remove_unreachable_regions (reachable, contains_stmt);
  sbitmap_free (reachable);
  sbitmap_free (contains_stmt);
  VEC_free (int, heap, label_to_region);
  if (dump_file)
    {
      fprintf (dump_file, "\n\nAfter removal of unreachable regions:\n");
      dump_eh_tree (dump_file, cfun);
      fprintf (dump_file, "\n\n");
    }
}

/* Pattern match emtpy EH receiver looking like:
  
   save_filt.6352_662 = [filter_expr] <<<filter object>>>;
   save_eptr.6351_663 = [exc_ptr_expr] <<<exception object>>>;
   <<<exception object>>> = save_eptr.6351_663;
   <<<filter object>>> = save_filt.6352_662;
   resx 1

   And various minor variants after DCE or copy propagation.
 */

static int
tree_empty_eh_handler_p (basic_block bb)
{
  gimple_stmt_iterator gsi;
  int region;
  edge_iterator ei;
  edge e;
  use_operand_p imm_use;
  gimple use_stmt;
  bool found = false;

  gsi = gsi_last_bb (bb);

  /* RESX  */
  if (gsi_end_p (gsi))
    return 0;
  if (gimple_code (gsi_stmt (gsi)) != GIMPLE_RESX)
    return 0;
  region = gimple_resx_region (gsi_stmt (gsi));

  /* filter_object set.  */
  gsi_prev_nondebug (&gsi);
  if (gsi_end_p (gsi))
    return 0;
  if (gimple_code (gsi_stmt (gsi)) == GIMPLE_ASSIGN)
    {
      tree filter_tmp;
      tree exc_ptr_tmp;

      if (TREE_CODE (gimple_assign_lhs (gsi_stmt (gsi))) != FILTER_EXPR)
	return 0;
      filter_tmp = gimple_assign_rhs1 (gsi_stmt (gsi));

      /* filter_object set.  */
      gsi_prev_nondebug (&gsi);
      if (gsi_end_p (gsi))
	return 0;
      if (gimple_code (gsi_stmt (gsi)) != GIMPLE_ASSIGN)
	return 0;
      if (TREE_CODE (gimple_assign_lhs (gsi_stmt (gsi))) != EXC_PTR_EXPR)
	return 0;
      exc_ptr_tmp = gimple_assign_rhs1 (gsi_stmt (gsi));

      /* exc_ptr get.  */
      if (TREE_CODE (exc_ptr_tmp) != EXC_PTR_EXPR)
	{
	  gsi_prev_nondebug (&gsi);
	  if (gsi_end_p (gsi))
	    return 0;
	  if (gimple_code (gsi_stmt (gsi)) != GIMPLE_ASSIGN)
	    return 0;
	  if (TREE_CODE (gimple_assign_rhs1 (gsi_stmt (gsi))) != EXC_PTR_EXPR)
	    return 0;
	  if (exc_ptr_tmp != gimple_assign_lhs (gsi_stmt (gsi)))
	    return 0;
	  if (!single_imm_use (exc_ptr_tmp, &imm_use, &use_stmt))
	    return 0;
	}

      /* filter_object get.  */
      if (TREE_CODE (filter_tmp) != FILTER_EXPR)
	{
	  gsi_prev_nondebug (&gsi);
	  if (gsi_end_p (gsi))
	    return 0;
	  if (gimple_code (gsi_stmt (gsi)) != GIMPLE_ASSIGN)
	    return 0;
	  if (TREE_CODE (gimple_assign_rhs1 (gsi_stmt (gsi))) != FILTER_EXPR)
	    return 0;
	  if (filter_tmp != gimple_assign_lhs (gsi_stmt (gsi)))
	    return 0;
	  if (!single_imm_use (filter_tmp, &imm_use, &use_stmt))
	    return 0;
	}

      /* label.  */
      gsi_prev_nondebug (&gsi);
      if (gsi_end_p (gsi))
	return 0;
    }
  if (gimple_code (gsi_stmt (gsi)) != GIMPLE_LABEL)
    return 0;

  /* Be sure that there is at least on EH region reaching the block directly.
     After EH edge redirection, it is possible that block is reached by one handler
     but resumed by different.  */
  FOR_EACH_EDGE (e, ei, bb->preds)
    if ((e->flags & EDGE_EH))
      found = true;
  if (found)
    return region;
  return 0;
}

/* Return true if it is possible to remove basic block BB and propagate
   through PHIs.  

   This means that every PHI in BB has all uses such that they are PHIs
   of basic blocks reachable througt BB and they appears only in use
   reachable by the edge from BB to the block contianing the use.  
   
   This is same as in merge-phi code, but in slightly more general setting
   because BB can have multiple successors.  */

static bool
all_phis_safe_to_merge (basic_block bb)
{
  gimple_stmt_iterator si;
  bool ok = true;

  for (si = gsi_start_phis (bb); !gsi_end_p (si); gsi_next (&si))
    {
      gimple phi = gsi_stmt (si);
      tree result = gimple_phi_result (phi);
      gimple stmt;
      use_operand_p imm_use;
      imm_use_iterator imm_iter;

      /* If the PHI's result is never used, then we can just
	 ignore it.  */
      if (has_zero_uses (result))
        continue;
      /* We can always rebuild virtuals if needed.  */
      if (!is_gimple_reg (result))
	continue;
      FOR_EACH_IMM_USE_STMT (stmt, imm_iter, result)
        {
	  if (gimple_code (stmt) != GIMPLE_PHI)
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
	        fprintf (dump_file,
			 "PHI result has use in non-PHI statement.\n");
	      ok = false;
	      BREAK_FROM_IMM_USE_STMT (imm_iter);
	    }
	  else
            FOR_EACH_IMM_USE_ON_STMT (imm_use, imm_iter)
	      {
               edge e;
	       e = gimple_phi_arg_edge (stmt, PHI_ARG_INDEX_FROM_USE (imm_use));
	       if (e->src != bb)
	         {
	          if (dump_file && (dump_flags & TDF_DETAILS))
	            fprintf (dump_file, "PHI has use in PHI not reached from"
		    	     "empty cleanup itself.\n");
	          ok = false;
		  break;
	         }
	      }
	   if (!ok)
	     BREAK_FROM_IMM_USE_STMT (imm_iter);
	 }
       if (!ok)
         return false;
    }
  return ok;
}

static bool dominance_info_invalidated;

/* Information to pass into make_eh_edge_and_update_phi.  */

struct update_info
{
  basic_block bb_to_remove, bb;
  edge edge_to_remove;
};

/* DATA points to update-info structure.
   Like make_eh_edge create EH edge from DATA->bb to basic block containing
   handler of REGION.  In addition also update PHI operands by copying
   operands from DATA->bb_to_remove.  */

static void
make_eh_edge_and_update_phi (struct eh_region_d *region, void *data)
{
  struct update_info *info = (struct update_info *) data;
  edge e, e2;
  tree lab;
  basic_block src, dst;
  gimple_stmt_iterator si;

  lab = get_eh_region_tree_label (region);

  src = info->bb;
  dst = label_to_block (lab);

  e = find_edge (src, dst);
  if (e)
    {
      gcc_assert (e->flags & EDGE_EH);
      e->aux = e;
      return;
    }
  dominance_info_invalidated = true;
  e2 = find_edge (info->bb_to_remove, dst);
  e = make_edge (src, dst, EDGE_EH);
  e->aux = e;
  gcc_assert (e2);
  for (si = gsi_start_phis (dst); !gsi_end_p (si); gsi_next (&si))
    {
      gimple phi = gsi_stmt (si);
      tree use = USE_FROM_PTR (PHI_ARG_DEF_PTR_FROM_EDGE (phi, e2));
      gimple def = (TREE_CODE (use) == SSA_NAME
      		    ? SSA_NAME_DEF_STMT (use) : NULL);

      if (def && gimple_bb (def) == info->bb_to_remove)
        {
	  use = USE_FROM_PTR (PHI_ARG_DEF_PTR_FROM_EDGE (def,
	  						 info->edge_to_remove));
	  gcc_assert (info->bb_to_remove == info->edge_to_remove->dest);
          def = TREE_CODE (use) == SSA_NAME ? SSA_NAME_DEF_STMT (use) : NULL;
	  gcc_assert (!def
	  	      || gimple_bb (def) != info->bb_to_remove
	  	      || !is_gimple_reg (use));
        }
      SET_USE (PHI_ARG_DEF_PTR_FROM_EDGE (phi, e), use);
    }
}

/* Make EH edges corresponding to STMT while updating PHI nodes after removal
   empty cleanup BB_TO_REMOVE joined to BB containing STMT
   by EDGE_TO_REMOVE.

   Return if EDGE_TO_REMOVE was really removed.  It might stay reachable when
   not all EH regions are cleaned up.  */

static bool
update_eh_edges (gimple stmt, basic_block bb_to_remove, edge edge_to_remove)
{
  int region_nr;
  bool is_resx;
  bool inlinable = false;
  struct update_info info;
  edge_iterator ei;
  edge e;
  int probability_sum = 0;
  bool removed = false;

  info.bb_to_remove = bb_to_remove;
  info.bb = gimple_bb (stmt);
  info.edge_to_remove = edge_to_remove;

  if (gimple_code (stmt) == GIMPLE_RESX)
    {
      region_nr = gimple_resx_region (stmt);
      is_resx = true;
    }
  else
    {
      region_nr = lookup_stmt_eh_region (stmt);
      is_resx = false;
      inlinable = inlinable_call_p (stmt);
    }

  /* First add new edges as neccesary.  */
  foreach_reachable_handler (region_nr, is_resx, inlinable,
  			     make_eh_edge_and_update_phi, &info);

  /* And remove edges we didn't marked. */
  for (ei = ei_start (info.bb->succs); (e = ei_safe_edge (ei)); )
    {
      if ((e->flags & EDGE_EH) && !e->aux)
	{
	  dominance_info_invalidated = true;
	  if (e == edge_to_remove)
	    removed = true;
	  remove_edge (e);
	}
      else
        {
	  e->aux = NULL;
	  probability_sum += e->probability;
	  ei_next (&ei);
	}
    }

  /* Make CFG profile more consistent assuming that exception will resume to
     first available EH handler.  In practice this makes little difference, but
     we get fewer consistency errors in the dumps.  */
  if (is_resx && EDGE_COUNT (info.bb->succs) && !probability_sum)
    EDGE_SUCC (info.bb, 0)->probability = REG_BR_PROB_BASE;
  return removed;
}

/* Look for basic blocks containing empty exception handler and remove them.
   This is similar to jump forwarding, just across EH edges.  */

static bool
cleanup_empty_eh (basic_block bb, VEC(int,heap) * label_to_region)
{
  int region;
  gimple_stmt_iterator si;
  edge_iterator ei;

  /* When handler of EH region winds up to be empty, we can safely
     remove it.  This leads to inner EH regions to be redirected
     to outer one, if present in function. So we need to rebuild
     EH edges in all sources.   */
  if ((region = tree_empty_eh_handler_p (bb))
      && all_phis_safe_to_merge (bb))
    {
      edge e;
      bool found = false, removed_some = false, has_non_eh_preds = false;
      gimple_stmt_iterator gsi;

      /* Look for all EH regions sharing label of this block.
         If they are not same as REGION, remove them and replace them
	 by outer region of REGION.  Also note if REGION itself is one
	 of them.  */

      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
        if (gimple_code (gsi_stmt (gsi)) == GIMPLE_LABEL)
	  {
	    int uid = LABEL_DECL_UID (gimple_label_label (gsi_stmt (gsi)));
	    int r = VEC_index (int, label_to_region, uid);
	    int next;

	    while (r)
	      {
		next = get_next_region_sharing_label (r);
		if (r == region)
		  found = true;
		else
		  {
		     removed_some = true;
		     remove_eh_region_and_replace_by_outer_of (r, region);
		     if (dump_file && (dump_flags & TDF_DETAILS))
		       fprintf (dump_file, "Empty EH handler %i removed and "
		       		"replaced by %i\n", r, region);
		  }
		r = next;
	      }
	  }
	else
	  break;

      gcc_assert (found || removed_some);
      FOR_EACH_EDGE (e, ei, bb->preds)
	if (!(e->flags & EDGE_EH))
	  has_non_eh_preds = true;

      /* When block is empty EH cleanup, but it is reachable via non-EH code too,
	 we can not remove the region it is resumed via, because doing so will
	 lead to redirection of its RESX edges.

	 This case will be handled later after edge forwarding if the EH cleanup
	 is really dead.  */

      if (found && !has_non_eh_preds)
        {
	   if (dump_file && (dump_flags & TDF_DETAILS))
	     fprintf (dump_file, "Empty EH handler %i removed.\n", region);
          remove_eh_region (region);
	}
      else if (!removed_some)
        return false;

      for (ei = ei_start (bb->preds); (e = ei_safe_edge (ei)); )
	{
	  basic_block src = e->src;
	  if (!(e->flags & EDGE_EH))
	    {
	      ei_next (&ei);
	      continue;
	    }
	  if (stmt_can_throw_internal (last_stmt (src)))
	    {
	      if (!update_eh_edges (last_stmt (src), bb, e))
	        ei_next (&ei);
	    }
	  else
	    remove_edge (e);
	}

      /* Verify that we eliminated all uses of PHI we are going to remove.
         If we didn't, rebuild SSA on affected variable (this is allowed only
         for virtuals).  */
      for (si = gsi_start_phis (bb); !gsi_end_p (si); gsi_next (&si))
        {
          gimple phi = gsi_stmt (si);
          tree result = gimple_phi_result (phi);
          if (!has_zero_uses (result))
	    {
              use_operand_p use_p;
              imm_use_iterator iter;
	      gimple stmt;

	      FOR_EACH_IMM_USE_STMT (stmt, iter, result)
		{
		  /* We have use, see if it won't disappear after
		     removing BB.  */
		  if (gimple_bb (stmt) == bb)
		    continue;
		  if (gimple_code (stmt) == GIMPLE_PHI)
		    {
		      bool bad = false;

		      FOR_EACH_IMM_USE_ON_STMT (use_p, iter)
	                if (gimple_phi_arg_edge (stmt,
				PHI_ARG_INDEX_FROM_USE (use_p))->src != bb)
			  {
			    bad = true;
			    break;
			  }

		      if (!bad)
		        continue;
		    }

	          gcc_assert (!is_gimple_reg (result));
	          mark_sym_for_renaming (SSA_NAME_VAR (result));
	          /* As we are going to delete this block we will release all
		     defs which makes the immediate uses on use stmts invalid.
		     Avoid that by replacing all uses with the bare variable
		     and updating the stmts.  */
		  FOR_EACH_IMM_USE_ON_STMT (use_p, iter)
		    SET_USE (use_p, SSA_NAME_VAR (result));
		  update_stmt (stmt);
		}
	    }
	}
      if (!ei_safe_edge (ei_start (bb->preds)))
        delete_basic_block (bb);
      return true;
    }
  return false;
}


/* Perform cleanups and lowering of exception handling
    1) cleanups regions with handlers doing nothing are optimized out
    2) MUST_NOT_THROW regions that became dead because of 1) are optimized out
    3) Info about regions that are containing instructions, and regions
       reachable via local EH edges is collected
    4) Eh tree is pruned for regions no longer neccesary.
 */

static unsigned int
cleanup_eh (void)
{
  bool changed = false;
  basic_block bb;
  VEC(int,heap) * label_to_region;
  int i;

  if (!cfun->eh)
    return 0;
  if (dump_file)
    {
      fprintf (dump_file, "Before cleanups:\n");
      dump_eh_tree (dump_file, cfun);
    }

  if (optimize)
    {
      label_to_region = label_to_region_map ();
      dominance_info_invalidated = false;
      /* We cannot use FOR_EACH_BB, since the basic blocks may get removed.  */
      for (i = NUM_FIXED_BLOCKS; i < last_basic_block; i++)
	{
	  bb = BASIC_BLOCK (i);
	  if (bb)
	    changed |= cleanup_empty_eh (bb, label_to_region);
	}
      VEC_free (int, heap, label_to_region);
      if (dominance_info_invalidated)
	{
	  free_dominance_info (CDI_DOMINATORS);
	  free_dominance_info (CDI_POST_DOMINATORS);
	}

      /* Removing contained cleanup can render MUST_NOT_THROW regions empty.  */
      if (changed)
	delete_unreachable_blocks ();
    }

  tree_remove_unreachable_handlers ();
  if (dump_file)
    {
      fprintf (dump_file, "After cleanups:\n");
      dump_eh_tree (dump_file, cfun);
    }

  return (changed ? TODO_cleanup_cfg | TODO_update_ssa : 0);
}

struct gimple_opt_pass pass_cleanup_eh = {
  {
   GIMPLE_PASS,
   "ehcleanup",			/* name */
   NULL,			/* gate */
   cleanup_eh,			/* execute */
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
