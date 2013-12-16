/* Iterator routines for GIMPLE statements.
   Copyright (C) 2007-2013 Free Software Foundation, Inc.
   Contributed by Aldy Hernandez  <aldy@quesejoda.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
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
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "tree-eh.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimple-ssa.h"
#include "cgraph.h"
#include "tree-cfg.h"
#include "tree-phinodes.h"
#include "ssa-iterators.h"
#include "tree-ssa.h"
#include "value-prof.h"


/* Mark the statement STMT as modified, and update it.  */

static inline void
update_modified_stmt (gimple stmt)
{
  if (!ssa_operands_active (cfun))
    return;
  update_stmt_if_modified (stmt);
}


/* Mark the statements in SEQ as modified, and update them.  */

static void
update_modified_stmts (gimple_seq seq)
{
  gimple_stmt_iterator gsi;

  if (!ssa_operands_active (cfun))
    return;
  for (gsi = gsi_start (seq); !gsi_end_p (gsi); gsi_next (&gsi))
    update_stmt_if_modified (gsi_stmt (gsi));
}


/* Set BB to be the basic block for all the statements in the list
   starting at FIRST and LAST.  */

static void
update_bb_for_stmts (gimple_seq_node first, gimple_seq_node last,
		     basic_block bb)
{
  gimple_seq_node n;

  for (n = first; n; n = n->next)
    {
      gimple_set_bb (n, bb);
      if (n == last)
	break;
    }
}

/* Set the frequencies for the cgraph_edges for each of the calls
   starting at FIRST for their new position within BB.  */

static void
update_call_edge_frequencies (gimple_seq_node first, basic_block bb)
{
  struct cgraph_node *cfun_node = NULL;
  int bb_freq = 0;
  gimple_seq_node n;

  for (n = first; n ; n = n->next)
    if (is_gimple_call (n))
      {
	struct cgraph_edge *e;

	/* These function calls are expensive enough that we want
	   to avoid calling them if we never see any calls.  */
	if (cfun_node == NULL)
	  {
	    cfun_node = cgraph_get_node (current_function_decl);
	    bb_freq = (compute_call_stmt_bb_frequency
		       (current_function_decl, bb));
	  }

	e = cgraph_edge (cfun_node, n);
	if (e != NULL)
	  e->frequency = bb_freq;
      }
}

/* Insert the sequence delimited by nodes FIRST and LAST before
   iterator I.  M specifies how to update iterator I after insertion
   (see enum gsi_iterator_update).

   This routine assumes that there is a forward and backward path
   between FIRST and LAST (i.e., they are linked in a doubly-linked
   list).  Additionally, if FIRST == LAST, this routine will properly
   insert a single node.  */

static void
gsi_insert_seq_nodes_before (gimple_stmt_iterator *i,
			     gimple_seq_node first,
			     gimple_seq_node last,
			     enum gsi_iterator_update mode)
{
  basic_block bb;
  gimple_seq_node cur = i->ptr;

  gcc_assert (!cur || cur->prev);

  if ((bb = gsi_bb (*i)) != NULL)
    update_bb_for_stmts (first, last, bb);

  /* Link SEQ before CUR in the sequence.  */
  if (cur)
    {
      first->prev = cur->prev;
      if (first->prev->next)
	first->prev->next = first;
      else
	gimple_seq_set_first (i->seq, first);
      last->next = cur;
      cur->prev = last;
    }
  else
    {
      gimple_seq_node itlast = gimple_seq_last (*i->seq);

      /* If CUR is NULL, we link at the end of the sequence (this case happens
	 when gsi_after_labels is called for a basic block that contains only
	 labels, so it returns an iterator after the end of the block, and
	 we need to insert before it; it might be cleaner to add a flag to the
	 iterator saying whether we are at the start or end of the list).  */
      last->next = NULL;
      if (itlast)
	{
	  first->prev = itlast;
	  itlast->next = first;
	}
      else
	gimple_seq_set_first (i->seq, first);
      gimple_seq_set_last (i->seq, last);
    }

  /* Update the iterator, if requested.  */
  switch (mode)
    {
    case GSI_NEW_STMT:
    case GSI_CONTINUE_LINKING:
      i->ptr = first;
      break;
    case GSI_SAME_STMT:
      break;
    default:
      gcc_unreachable ();
    }
}


/* Inserts the sequence of statements SEQ before the statement pointed
   by iterator I.  MODE indicates what to do with the iterator after
   insertion (see enum gsi_iterator_update).

   This function does not scan for new operands.  It is provided for
   the use of the gimplifier, which manipulates statements for which
   def/use information has not yet been constructed.  Most callers
   should use gsi_insert_seq_before.  */

void
gsi_insert_seq_before_without_update (gimple_stmt_iterator *i, gimple_seq seq,
                                      enum gsi_iterator_update mode)
{
  gimple_seq_node first, last;

  if (seq == NULL)
    return;

  /* Don't allow inserting a sequence into itself.  */
  gcc_assert (seq != *i->seq);

  first = gimple_seq_first (seq);
  last = gimple_seq_last (seq);

  /* Empty sequences need no work.  */
  if (!first || !last)
    {
      gcc_assert (first == last);
      return;
    }

  gsi_insert_seq_nodes_before (i, first, last, mode);
}


/* Inserts the sequence of statements SEQ before the statement pointed
   by iterator I.  MODE indicates what to do with the iterator after
   insertion (see enum gsi_iterator_update). Scan the statements in SEQ
   for new operands.  */

void
gsi_insert_seq_before (gimple_stmt_iterator *i, gimple_seq seq,
		       enum gsi_iterator_update mode)
{
  update_modified_stmts (seq);
  gsi_insert_seq_before_without_update (i, seq, mode);
}


/* Insert the sequence delimited by nodes FIRST and LAST after
   iterator I.  M specifies how to update iterator I after insertion
   (see enum gsi_iterator_update).

   This routine assumes that there is a forward and backward path
   between FIRST and LAST (i.e., they are linked in a doubly-linked
   list).  Additionally, if FIRST == LAST, this routine will properly
   insert a single node.  */

static void
gsi_insert_seq_nodes_after (gimple_stmt_iterator *i,
			    gimple_seq_node first,
			    gimple_seq_node last,
			    enum gsi_iterator_update m)
{
  basic_block bb;
  gimple_seq_node cur = i->ptr;

  gcc_assert (!cur || cur->prev);

  /* If the iterator is inside a basic block, we need to update the
     basic block information for all the nodes between FIRST and LAST.  */
  if ((bb = gsi_bb (*i)) != NULL)
    update_bb_for_stmts (first, last, bb);

  /* Link SEQ after CUR.  */
  if (cur)
    {
      last->next = cur->next;
      if (last->next)
	{
	  last->next->prev = last;
	}
      else
	gimple_seq_set_last (i->seq, last);
      first->prev = cur;
      cur->next = first;
    }
  else
    {
      gcc_assert (!gimple_seq_last (*i->seq));
      last->next = NULL;
      gimple_seq_set_first (i->seq, first);
      gimple_seq_set_last (i->seq, last);
    }

  /* Update the iterator, if requested.  */
  switch (m)
    {
    case GSI_NEW_STMT:
      i->ptr = first;
      break;
    case GSI_CONTINUE_LINKING:
      i->ptr = last;
      break;
    case GSI_SAME_STMT:
      gcc_assert (cur);
      break;
    default:
      gcc_unreachable ();
    }
}


/* Links sequence SEQ after the statement pointed-to by iterator I.
   MODE is as in gsi_insert_after.

   This function does not scan for new operands.  It is provided for
   the use of the gimplifier, which manipulates statements for which
   def/use information has not yet been constructed.  Most callers
   should use gsi_insert_seq_after.  */

void
gsi_insert_seq_after_without_update (gimple_stmt_iterator *i, gimple_seq seq,
                                     enum gsi_iterator_update mode)
{
  gimple_seq_node first, last;

  if (seq == NULL)
    return;

  /* Don't allow inserting a sequence into itself.  */
  gcc_assert (seq != *i->seq);

  first = gimple_seq_first (seq);
  last = gimple_seq_last (seq);

  /* Empty sequences need no work.  */
  if (!first || !last)
    {
      gcc_assert (first == last);
      return;
    }

  gsi_insert_seq_nodes_after (i, first, last, mode);
}


/* Links sequence SEQ after the statement pointed-to by iterator I.
   MODE is as in gsi_insert_after.  Scan the statements in SEQ
   for new operands.  */

void
gsi_insert_seq_after (gimple_stmt_iterator *i, gimple_seq seq,
		      enum gsi_iterator_update mode)
{
  update_modified_stmts (seq);
  gsi_insert_seq_after_without_update (i, seq, mode);
}


/* Move all statements in the sequence after I to a new sequence.
   Return this new sequence.  */

gimple_seq
gsi_split_seq_after (gimple_stmt_iterator i)
{
  gimple_seq_node cur, next;
  gimple_seq *pold_seq, new_seq;

  cur = i.ptr;

  /* How can we possibly split after the end, or before the beginning?  */
  gcc_assert (cur && cur->next);
  next = cur->next;

  pold_seq = i.seq;

  gimple_seq_set_first (&new_seq, next);
  gimple_seq_set_last (&new_seq, gimple_seq_last (*pold_seq));
  gimple_seq_set_last (pold_seq, cur);
  cur->next = NULL;

  return new_seq;
}


/* Set the statement to which GSI points to STMT.  This only updates
   the iterator and the gimple sequence, it doesn't do the bookkeeping
   of gsi_replace.  */

void
gsi_set_stmt (gimple_stmt_iterator *gsi, gimple stmt)
{
  gimple orig_stmt = gsi_stmt (*gsi);
  gimple prev, next;

  stmt->next = next = orig_stmt->next;
  stmt->prev = prev = orig_stmt->prev;
  /* Note how we don't clear next/prev of orig_stmt.  This is so that
     copies of *GSI our callers might still hold (to orig_stmt)
     can be advanced as if they too were replaced.  */
  if (prev->next)
    prev->next = stmt;
  else
    gimple_seq_set_first (gsi->seq, stmt);
  if (next)
    next->prev = stmt;
  else
    gimple_seq_set_last (gsi->seq, stmt);

  gsi->ptr = stmt;
}


/* Move all statements in the sequence before I to a new sequence.
   Return this new sequence.  I is set to the head of the new list.  */

void
gsi_split_seq_before (gimple_stmt_iterator *i, gimple_seq *pnew_seq)
{
  gimple_seq_node cur, prev;
  gimple_seq old_seq;

  cur = i->ptr;

  /* How can we possibly split after the end?  */
  gcc_assert (cur);
  prev = cur->prev;

  old_seq = *i->seq;
  if (!prev->next)
    *i->seq = NULL;
  i->seq = pnew_seq;

  /* Set the limits on NEW_SEQ.  */
  gimple_seq_set_first (pnew_seq, cur);
  gimple_seq_set_last (pnew_seq, gimple_seq_last (old_seq));

  /* Cut OLD_SEQ before I.  */
  gimple_seq_set_last (&old_seq, prev);
  if (prev->next)
    prev->next = NULL;
}


/* Replace the statement pointed-to by GSI to STMT.  If UPDATE_EH_INFO
   is true, the exception handling information of the original
   statement is moved to the new statement.  Assignments must only be
   replaced with assignments to the same LHS.  */

void
gsi_replace (gimple_stmt_iterator *gsi, gimple stmt, bool update_eh_info)
{
  gimple orig_stmt = gsi_stmt (*gsi);

  if (stmt == orig_stmt)
    return;

  gcc_assert (!gimple_has_lhs (orig_stmt) || !gimple_has_lhs (stmt)
	      || gimple_get_lhs (orig_stmt) == gimple_get_lhs (stmt));

  gimple_set_location (stmt, gimple_location (orig_stmt));
  gimple_set_bb (stmt, gsi_bb (*gsi));

  /* Preserve EH region information from the original statement, if
     requested by the caller.  */
  if (update_eh_info)
    maybe_clean_or_replace_eh_stmt (orig_stmt, stmt);

  gimple_duplicate_stmt_histograms (cfun, stmt, cfun, orig_stmt);

  /* Free all the data flow information for ORIG_STMT.  */
  gimple_set_bb (orig_stmt, NULL);
  gimple_remove_stmt_histograms (cfun, orig_stmt);
  delink_stmt_imm_use (orig_stmt);

  gsi_set_stmt (gsi, stmt);
  gimple_set_modified (stmt, true);
  update_modified_stmt (stmt);
}


/* Replace the statement pointed-to by GSI with the sequence SEQ.
   If UPDATE_EH_INFO is true, the exception handling information of
   the original statement is moved to the last statement of the new
   sequence.  If the old statement is an assignment, then so must
   be the last statement of the new sequence, and they must have the
   same LHS.  */

void
gsi_replace_with_seq (gimple_stmt_iterator *gsi, gimple_seq seq,
		      bool update_eh_info)
{
  gimple_stmt_iterator seqi;
  gimple last;
  if (gimple_seq_empty_p (seq))
    {
      gsi_remove (gsi, true);
      return;
    }
  seqi = gsi_last (seq);
  last = gsi_stmt (seqi);
  gsi_remove (&seqi, false);
  gsi_insert_seq_before (gsi, seq, GSI_SAME_STMT);
  gsi_replace (gsi, last, update_eh_info);
}


/* Insert statement STMT before the statement pointed-to by iterator I.
   M specifies how to update iterator I after insertion (see enum
   gsi_iterator_update).

   This function does not scan for new operands.  It is provided for
   the use of the gimplifier, which manipulates statements for which
   def/use information has not yet been constructed.  Most callers
   should use gsi_insert_before.  */

void
gsi_insert_before_without_update (gimple_stmt_iterator *i, gimple stmt,
                                  enum gsi_iterator_update m)
{
  gsi_insert_seq_nodes_before (i, stmt, stmt, m);
}

/* Insert statement STMT before the statement pointed-to by iterator I.
   Update STMT's basic block and scan it for new operands.  M
   specifies how to update iterator I after insertion (see enum
   gsi_iterator_update).  */

void
gsi_insert_before (gimple_stmt_iterator *i, gimple stmt,
                   enum gsi_iterator_update m)
{
  update_modified_stmt (stmt);
  gsi_insert_before_without_update (i, stmt, m);
}


/* Insert statement STMT after the statement pointed-to by iterator I.
   M specifies how to update iterator I after insertion (see enum
   gsi_iterator_update).

   This function does not scan for new operands.  It is provided for
   the use of the gimplifier, which manipulates statements for which
   def/use information has not yet been constructed.  Most callers
   should use gsi_insert_after.  */

void
gsi_insert_after_without_update (gimple_stmt_iterator *i, gimple stmt,
                                 enum gsi_iterator_update m)
{
  gsi_insert_seq_nodes_after (i, stmt, stmt, m);
}


/* Insert statement STMT after the statement pointed-to by iterator I.
   Update STMT's basic block and scan it for new operands.  M
   specifies how to update iterator I after insertion (see enum
   gsi_iterator_update).  */

void
gsi_insert_after (gimple_stmt_iterator *i, gimple stmt,
		  enum gsi_iterator_update m)
{
  update_modified_stmt (stmt);
  gsi_insert_after_without_update (i, stmt, m);
}


/* Remove the current stmt from the sequence.  The iterator is updated
   to point to the next statement.

   REMOVE_PERMANENTLY is true when the statement is going to be removed
   from the IL and not reinserted elsewhere.  In that case we remove the
   statement pointed to by iterator I from the EH tables, and free its
   operand caches.  Otherwise we do not modify this information.  Returns
   true whether EH edge cleanup is required.  */

bool
gsi_remove (gimple_stmt_iterator *i, bool remove_permanently)
{
  gimple_seq_node cur, next, prev;
  gimple stmt = gsi_stmt (*i);
  bool require_eh_edge_purge = false;

  if (gimple_code (stmt) != GIMPLE_PHI)
    insert_debug_temps_for_defs (i);

  /* Free all the data flow information for STMT.  */
  gimple_set_bb (stmt, NULL);
  delink_stmt_imm_use (stmt);
  gimple_set_modified (stmt, true);

  if (remove_permanently)
    {
      require_eh_edge_purge = remove_stmt_from_eh_lp (stmt);
      gimple_remove_stmt_histograms (cfun, stmt);
    }

  /* Update the iterator and re-wire the links in I->SEQ.  */
  cur = i->ptr;
  next = cur->next;
  prev = cur->prev;
  /* See gsi_set_stmt for why we don't reset prev/next of STMT.  */

  if (next)
    /* Cur is not last.  */
    next->prev = prev;
  else if (prev->next)
    /* Cur is last but not first.  */
    gimple_seq_set_last (i->seq, prev);

  if (prev->next)
    /* Cur is not first.  */
    prev->next = next;
  else
    /* Cur is first.  */
    *i->seq = next;

  i->ptr = next;

  return require_eh_edge_purge;
}


/* Finds iterator for STMT.  */

gimple_stmt_iterator
gsi_for_stmt (gimple stmt)
{
  gimple_stmt_iterator i;
  basic_block bb = gimple_bb (stmt);

  if (gimple_code (stmt) == GIMPLE_PHI)
    i = gsi_start_phis (bb);
  else
    i = gsi_start_bb (bb);

  i.ptr = stmt;
  return i;
}


/* Move the statement at FROM so it comes right after the statement at TO.  */

void
gsi_move_after (gimple_stmt_iterator *from, gimple_stmt_iterator *to)
{
  gimple stmt = gsi_stmt (*from);
  gsi_remove (from, false);

  /* We must have GSI_NEW_STMT here, as gsi_move_after is sometimes used to
     move statements to an empty block.  */
  gsi_insert_after (to, stmt, GSI_NEW_STMT);
}


/* Move the statement at FROM so it comes right before the statement
   at TO.  */

void
gsi_move_before (gimple_stmt_iterator *from, gimple_stmt_iterator *to)
{
  gimple stmt = gsi_stmt (*from);
  gsi_remove (from, false);

  /* For consistency with gsi_move_after, it might be better to have
     GSI_NEW_STMT here; however, that breaks several places that expect
     that TO does not change.  */
  gsi_insert_before (to, stmt, GSI_SAME_STMT);
}


/* Move the statement at FROM to the end of basic block BB.  */

void
gsi_move_to_bb_end (gimple_stmt_iterator *from, basic_block bb)
{
  gimple_stmt_iterator last = gsi_last_bb (bb);
  gcc_checking_assert (gsi_bb (last) == bb);

  /* Have to check gsi_end_p because it could be an empty block.  */
  if (!gsi_end_p (last) && is_ctrl_stmt (gsi_stmt (last)))
    gsi_move_before (from, &last);
  else
    gsi_move_after (from, &last);
}


/* Add STMT to the pending list of edge E.  No actual insertion is
   made until a call to gsi_commit_edge_inserts () is made.  */

void
gsi_insert_on_edge (edge e, gimple stmt)
{
  gimple_seq_add_stmt (&PENDING_STMT (e), stmt);
}

/* Add the sequence of statements SEQ to the pending list of edge E.
   No actual insertion is made until a call to gsi_commit_edge_inserts
   is made.  */

void
gsi_insert_seq_on_edge (edge e, gimple_seq seq)
{
  gimple_seq_add_seq (&PENDING_STMT (e), seq);
}


/* Insert the statement pointed-to by GSI into edge E.  Every attempt
   is made to place the statement in an existing basic block, but
   sometimes that isn't possible.  When it isn't possible, the edge is
   split and the statement is added to the new block.

   In all cases, the returned *GSI points to the correct location.  The
   return value is true if insertion should be done after the location,
   or false if it should be done before the location.  If a new basic block
   has to be created, it is stored in *NEW_BB.  */

static bool
gimple_find_edge_insert_loc (edge e, gimple_stmt_iterator *gsi,
			     basic_block *new_bb)
{
  basic_block dest, src;
  gimple tmp;

  dest = e->dest;

  /* If the destination has one predecessor which has no PHI nodes,
     insert there.  Except for the exit block.

     The requirement for no PHI nodes could be relaxed.  Basically we
     would have to examine the PHIs to prove that none of them used
     the value set by the statement we want to insert on E.  That
     hardly seems worth the effort.  */
 restart:
  if (single_pred_p (dest)
      && gimple_seq_empty_p (phi_nodes (dest))
      && dest != EXIT_BLOCK_PTR_FOR_FN (cfun))
    {
      *gsi = gsi_start_bb (dest);
      if (gsi_end_p (*gsi))
	return true;

      /* Make sure we insert after any leading labels.  */
      tmp = gsi_stmt (*gsi);
      while (gimple_code (tmp) == GIMPLE_LABEL)
	{
	  gsi_next (gsi);
	  if (gsi_end_p (*gsi))
	    break;
	  tmp = gsi_stmt (*gsi);
	}

      if (gsi_end_p (*gsi))
	{
	  *gsi = gsi_last_bb (dest);
	  return true;
	}
      else
	return false;
    }

  /* If the source has one successor, the edge is not abnormal and
     the last statement does not end a basic block, insert there.
     Except for the entry block.  */
  src = e->src;
  if ((e->flags & EDGE_ABNORMAL) == 0
      && single_succ_p (src)
      && src != ENTRY_BLOCK_PTR_FOR_FN (cfun))
    {
      *gsi = gsi_last_bb (src);
      if (gsi_end_p (*gsi))
	return true;

      tmp = gsi_stmt (*gsi);
      if (!stmt_ends_bb_p (tmp))
	return true;

      switch (gimple_code (tmp))
	{
	case GIMPLE_RETURN:
	case GIMPLE_RESX:
	  return false;
	default:
	  break;
        }
    }

  /* Otherwise, create a new basic block, and split this edge.  */
  dest = split_edge (e);
  if (new_bb)
    *new_bb = dest;
  e = single_pred_edge (dest);
  goto restart;
}


/* Similar to gsi_insert_on_edge+gsi_commit_edge_inserts.  If a new
   block has to be created, it is returned.  */

basic_block
gsi_insert_on_edge_immediate (edge e, gimple stmt)
{
  gimple_stmt_iterator gsi;
  basic_block new_bb = NULL;
  bool ins_after;

  gcc_assert (!PENDING_STMT (e));

  ins_after = gimple_find_edge_insert_loc (e, &gsi, &new_bb);

  update_call_edge_frequencies (stmt, gsi.bb);

  if (ins_after)
    gsi_insert_after (&gsi, stmt, GSI_NEW_STMT);
  else
    gsi_insert_before (&gsi, stmt, GSI_NEW_STMT);

  return new_bb;
}

/* Insert STMTS on edge E.  If a new block has to be created, it
   is returned.  */

basic_block
gsi_insert_seq_on_edge_immediate (edge e, gimple_seq stmts)
{
  gimple_stmt_iterator gsi;
  basic_block new_bb = NULL;
  bool ins_after;

  gcc_assert (!PENDING_STMT (e));

  ins_after = gimple_find_edge_insert_loc (e, &gsi, &new_bb);
  update_call_edge_frequencies (gimple_seq_first (stmts), gsi.bb);

  if (ins_after)
    gsi_insert_seq_after (&gsi, stmts, GSI_NEW_STMT);
  else
    gsi_insert_seq_before (&gsi, stmts, GSI_NEW_STMT);

  return new_bb;
}

/* This routine will commit all pending edge insertions, creating any new
   basic blocks which are necessary.  */

void
gsi_commit_edge_inserts (void)
{
  basic_block bb;
  edge e;
  edge_iterator ei;

  gsi_commit_one_edge_insert (single_succ_edge (ENTRY_BLOCK_PTR_FOR_FN (cfun)),
			      NULL);

  FOR_EACH_BB_FN (bb, cfun)
    FOR_EACH_EDGE (e, ei, bb->succs)
      gsi_commit_one_edge_insert (e, NULL);
}


/* Commit insertions pending at edge E. If a new block is created, set NEW_BB
   to this block, otherwise set it to NULL.  */

void
gsi_commit_one_edge_insert (edge e, basic_block *new_bb)
{
  if (new_bb)
    *new_bb = NULL;

  if (PENDING_STMT (e))
    {
      gimple_stmt_iterator gsi;
      gimple_seq seq = PENDING_STMT (e);
      bool ins_after;

      PENDING_STMT (e) = NULL;

      ins_after = gimple_find_edge_insert_loc (e, &gsi, new_bb);
      update_call_edge_frequencies (gimple_seq_first (seq), gsi.bb);

      if (ins_after)
	gsi_insert_seq_after (&gsi, seq, GSI_NEW_STMT);
      else
	gsi_insert_seq_before (&gsi, seq, GSI_NEW_STMT);
    }
}

/* Returns iterator at the start of the list of phi nodes of BB.  */

gimple_stmt_iterator
gsi_start_phis (basic_block bb)
{
  gimple_seq *pseq = phi_nodes_ptr (bb);
  return gsi_start_1 (pseq);
}
