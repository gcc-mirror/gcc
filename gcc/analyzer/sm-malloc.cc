/* A state machine for detecting misuses of the malloc/free API.
   Copyright (C) 2019-2020 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "function.h"
#include "basic-block.h"
#include "gimple.h"
#include "options.h"
#include "bitmap.h"
#include "diagnostic-path.h"
#include "diagnostic-metadata.h"
#include "function.h"
#include "analyzer/analyzer.h"
#include "diagnostic-event-id.h"
#include "analyzer/analyzer-logging.h"
#include "analyzer/sm.h"
#include "analyzer/pending-diagnostic.h"

#if ENABLE_ANALYZER

namespace ana {

namespace {

/* A state machine for detecting misuses of the malloc/free API.

   See sm-malloc.dot for an overview (keep this in-sync with that file).  */

class malloc_state_machine : public state_machine
{
public:
  malloc_state_machine (logger *logger);

  bool inherited_state_p () const FINAL OVERRIDE { return false; }

  bool on_stmt (sm_context *sm_ctxt,
		const supernode *node,
		const gimple *stmt) const FINAL OVERRIDE;

  void on_phi (sm_context *sm_ctxt,
	       const supernode *node,
	       const gphi *phi,
	       tree rhs) const FINAL OVERRIDE;

  void on_condition (sm_context *sm_ctxt,
		     const supernode *node,
		     const gimple *stmt,
		     tree lhs,
		     enum tree_code op,
		     tree rhs) const FINAL OVERRIDE;

  bool can_purge_p (state_t s) const FINAL OVERRIDE;
  pending_diagnostic *on_leak (tree var) const FINAL OVERRIDE;

  /* Start state.  */
  state_t m_start;

  /* State for a pointer returned from malloc that hasn't been checked for
     NULL.
     It could be a pointer to heap-allocated memory, or could be NULL.  */
  state_t m_unchecked;

  /* State for a pointer that's known to be NULL.  */
  state_t m_null;

  /* State for a pointer to heap-allocated memory, known to be non-NULL.  */
  state_t m_nonnull;

  /* State for a pointer to freed memory.  */
  state_t m_freed;

  /* State for a pointer that's known to not be on the heap (e.g. to a local
     or global).  */
  state_t m_non_heap; // TODO: or should this be a different state machine?
  // or do we need child values etc?

  /* Stop state, for pointers we don't want to track any more.  */
  state_t m_stop;

private:
  void on_zero_assignment (sm_context *sm_ctxt,
			   const supernode *node,
			   const gimple *stmt,
			   tree lhs) const;
};

/* Class for diagnostics relating to malloc_state_machine.  */

class malloc_diagnostic : public pending_diagnostic
{
public:
  malloc_diagnostic (const malloc_state_machine &sm, tree arg)
  : m_sm (sm), m_arg (arg)
  {}

  bool subclass_equal_p (const pending_diagnostic &base_other) const OVERRIDE
  {
    return same_tree_p (m_arg, ((const malloc_diagnostic &)base_other).m_arg);
  }

  label_text describe_state_change (const evdesc::state_change &change)
    OVERRIDE
  {
    if (change.m_old_state == m_sm.m_start
	&& change.m_new_state == m_sm.m_unchecked)
      // TODO: verify that it's the allocation stmt, not a copy
      return label_text::borrow ("allocated here");
    if (change.m_old_state == m_sm.m_unchecked
	&& change.m_new_state == m_sm.m_nonnull)
      return change.formatted_print ("assuming %qE is non-NULL",
				     change.m_expr);
    if (change.m_new_state == m_sm.m_null)
      {
	if (change.m_old_state == m_sm.m_unchecked)
	  return change.formatted_print ("assuming %qE is NULL",
					 change.m_expr);
	else
	  return change.formatted_print ("%qE is NULL",
					 change.m_expr);
      }

    return label_text ();
  }

protected:
  const malloc_state_machine &m_sm;
  tree m_arg;
};

/* Concrete subclass for reporting double-free diagnostics.  */

class double_free : public malloc_diagnostic
{
public:
  double_free (const malloc_state_machine &sm, tree arg)
  : malloc_diagnostic (sm, arg)
  {}

  const char *get_kind () const FINAL OVERRIDE { return "double_free"; }

  bool emit (rich_location *rich_loc) FINAL OVERRIDE
  {
    auto_diagnostic_group d;
    diagnostic_metadata m;
    m.add_cwe (415); /* CWE-415: Double Free.  */
    return warning_meta (rich_loc, m, OPT_Wanalyzer_double_free,
			 "double-%<free%> of %qE", m_arg);
  }

  label_text describe_state_change (const evdesc::state_change &change)
    FINAL OVERRIDE
  {
    if (change.m_new_state == m_sm.m_freed)
      {
	m_first_free_event = change.m_event_id;
	return change.formatted_print ("first %qs here", "free");
      }
    return malloc_diagnostic::describe_state_change (change);
  }

  label_text describe_call_with_state (const evdesc::call_with_state &info)
    FINAL OVERRIDE
  {
    if (info.m_state == m_sm.m_freed)
      return info.formatted_print
	("passing freed pointer %qE in call to %qE from %qE",
	 info.m_expr, info.m_callee_fndecl, info.m_caller_fndecl);
    return label_text ();
  }

  label_text describe_final_event (const evdesc::final_event &ev) FINAL OVERRIDE
  {
    if (m_first_free_event.known_p ())
      return ev.formatted_print ("second %qs here; first %qs was at %@",
				 "free", "free",
				 &m_first_free_event);
    return ev.formatted_print ("second %qs here", "free");
  }

private:
  diagnostic_event_id_t m_first_free_event;
};

/* Abstract subclass for describing possible bad uses of NULL.
   Responsible for describing the call that could return NULL.  */

class possible_null : public malloc_diagnostic
{
public:
  possible_null (const malloc_state_machine &sm, tree arg)
  : malloc_diagnostic (sm, arg)
  {}

  label_text describe_state_change (const evdesc::state_change &change)
    FINAL OVERRIDE
  {
    if (change.m_old_state == m_sm.m_start
	&& change.m_new_state == m_sm.m_unchecked)
      {
	m_origin_of_unchecked_event = change.m_event_id;
	return label_text::borrow ("this call could return NULL");
      }
    return malloc_diagnostic::describe_state_change (change);
  }

  label_text describe_return_of_state (const evdesc::return_of_state &info)
    FINAL OVERRIDE
  {
    if (info.m_state == m_sm.m_unchecked)
      return info.formatted_print ("possible return of NULL to %qE from %qE",
				   info.m_caller_fndecl, info.m_callee_fndecl);
    return label_text ();
  }

protected:
  diagnostic_event_id_t m_origin_of_unchecked_event;
};

/* Concrete subclass for describing dereference of a possible NULL
   value.  */

class possible_null_deref : public possible_null
{
public:
  possible_null_deref (const malloc_state_machine &sm, tree arg)
  : possible_null (sm, arg)
  {}

  const char *get_kind () const FINAL OVERRIDE { return "possible_null_deref"; }

  bool emit (rich_location *rich_loc) FINAL OVERRIDE
  {
    /* CWE-690: Unchecked Return Value to NULL Pointer Dereference.  */
    diagnostic_metadata m;
    m.add_cwe (690);
    return warning_meta (rich_loc, m,
			 OPT_Wanalyzer_possible_null_dereference,
			 "dereference of possibly-NULL %qE", m_arg);
  }

  label_text describe_final_event (const evdesc::final_event &ev) FINAL OVERRIDE
  {
    if (m_origin_of_unchecked_event.known_p ())
      return ev.formatted_print ("%qE could be NULL: unchecked value from %@",
				 ev.m_expr,
				 &m_origin_of_unchecked_event);
    else
      return ev.formatted_print ("%qE could be NULL", ev.m_expr);
  }

};

/* Subroutine for use by possible_null_arg::emit and null_arg::emit.
   Issue a note informing that the pertinent argument must be non-NULL.  */

static void
inform_nonnull_attribute (tree fndecl, int arg_idx)
{
  inform (DECL_SOURCE_LOCATION (fndecl),
	  "argument %u of %qD must be non-null",
	  arg_idx + 1, fndecl);
  /* Ideally we would use the location of the parm and underline the
     attribute also - but we don't have the location_t values at this point
     in the middle-end.
     For reference, the C and C++ FEs have get_fndecl_argument_location.  */
}

/* Concrete subclass for describing passing a possibly-NULL value to a
   function marked with __attribute__((nonnull)).  */

class possible_null_arg : public possible_null
{
public:
  possible_null_arg (const malloc_state_machine &sm, tree arg,
		     tree fndecl, int arg_idx)
  : possible_null (sm, arg),
    m_fndecl (fndecl), m_arg_idx (arg_idx)
  {}

  const char *get_kind () const FINAL OVERRIDE { return "possible_null_arg"; }

  bool subclass_equal_p (const pending_diagnostic &base_other) const
  {
    const possible_null_arg &sub_other
      = (const possible_null_arg &)base_other;
    return (same_tree_p (m_arg, sub_other.m_arg)
	    && m_fndecl == sub_other.m_fndecl
	    && m_arg_idx == sub_other.m_arg_idx);
  }


  bool emit (rich_location *rich_loc) FINAL OVERRIDE
  {
    /* CWE-690: Unchecked Return Value to NULL Pointer Dereference.  */
    auto_diagnostic_group d;
    diagnostic_metadata m;
    m.add_cwe (690);
    bool warned
      = warning_meta (rich_loc, m, OPT_Wanalyzer_possible_null_argument,
		      "use of possibly-NULL %qE where non-null expected",
		      m_arg);
    if (warned)
      inform_nonnull_attribute (m_fndecl, m_arg_idx);
    return warned;
  }

  label_text describe_final_event (const evdesc::final_event &ev) FINAL OVERRIDE
  {
    if (m_origin_of_unchecked_event.known_p ())
      return ev.formatted_print ("argument %u (%qE) from %@ could be NULL"
				 " where non-null expected",
				 m_arg_idx + 1, ev.m_expr,
				 &m_origin_of_unchecked_event);
    else
      return ev.formatted_print ("argument %u (%qE) could be NULL"
				 " where non-null expected",
				 m_arg_idx + 1, ev.m_expr);
  }

private:
  tree m_fndecl;
  int m_arg_idx;
};

/* Concrete subclass for describing a dereference of a NULL value.  */

class null_deref : public malloc_diagnostic
{
public:
  null_deref (const malloc_state_machine &sm, tree arg)
  : malloc_diagnostic (sm, arg) {}

  const char *get_kind () const FINAL OVERRIDE { return "null_deref"; }

  bool emit (rich_location *rich_loc) FINAL OVERRIDE
  {
    /* CWE-690: Unchecked Return Value to NULL Pointer Dereference.  */
    diagnostic_metadata m;
    m.add_cwe (690);
    return warning_meta (rich_loc, m,
			 OPT_Wanalyzer_null_dereference,
			 "dereference of NULL %qE", m_arg);
  }

  label_text describe_return_of_state (const evdesc::return_of_state &info)
    FINAL OVERRIDE
  {
    if (info.m_state == m_sm.m_null)
      return info.formatted_print ("return of NULL to %qE from %qE",
				   info.m_caller_fndecl, info.m_callee_fndecl);
    return label_text ();
  }

  label_text describe_final_event (const evdesc::final_event &ev) FINAL OVERRIDE
  {
    return ev.formatted_print ("dereference of NULL %qE", ev.m_expr);
  }
};

/* Concrete subclass for describing passing a NULL value to a
   function marked with __attribute__((nonnull)).  */

class null_arg : public malloc_diagnostic
{
public:
  null_arg (const malloc_state_machine &sm, tree arg,
	    tree fndecl, int arg_idx)
  : malloc_diagnostic (sm, arg),
    m_fndecl (fndecl), m_arg_idx (arg_idx)
  {}

  const char *get_kind () const FINAL OVERRIDE { return "null_arg"; }

  bool subclass_equal_p (const pending_diagnostic &base_other) const
  {
    const null_arg &sub_other
      = (const null_arg &)base_other;
    return (same_tree_p (m_arg, sub_other.m_arg)
	    && m_fndecl == sub_other.m_fndecl
	    && m_arg_idx == sub_other.m_arg_idx);
  }

  bool emit (rich_location *rich_loc) FINAL OVERRIDE
  {
    /* CWE-690: Unchecked Return Value to NULL Pointer Dereference.  */
    auto_diagnostic_group d;
    diagnostic_metadata m;
    m.add_cwe (690);
    bool warned = warning_meta (rich_loc, m, OPT_Wanalyzer_null_argument,
				"use of NULL %qE where non-null expected",
				m_arg);
    if (warned)
      inform_nonnull_attribute (m_fndecl, m_arg_idx);
    return warned;
  }

  label_text describe_final_event (const evdesc::final_event &ev) FINAL OVERRIDE
  {
    return ev.formatted_print ("argument %u (%qE) NULL"
			       " where non-null expected",
			       m_arg_idx + 1, ev.m_expr);
  }

private:
  tree m_fndecl;
  int m_arg_idx;
};

class use_after_free : public malloc_diagnostic
{
public:
  use_after_free (const malloc_state_machine &sm, tree arg)
  : malloc_diagnostic (sm, arg)
  {}

  const char *get_kind () const FINAL OVERRIDE { return "use_after_free"; }

  bool emit (rich_location *rich_loc) FINAL OVERRIDE
  {
    /* CWE-416: Use After Free.  */
    diagnostic_metadata m;
    m.add_cwe (416);
    return warning_meta (rich_loc, m, OPT_Wanalyzer_use_after_free,
			 "use after %<free%> of %qE", m_arg);
  }

  label_text describe_state_change (const evdesc::state_change &change)
    FINAL OVERRIDE
  {
    if (change.m_new_state == m_sm.m_freed)
      {
	m_free_event = change.m_event_id;
	return label_text::borrow ("freed here");
      }
    return malloc_diagnostic::describe_state_change (change);
  }

  label_text describe_final_event (const evdesc::final_event &ev) FINAL OVERRIDE
  {
    if (m_free_event.known_p ())
      return ev.formatted_print ("use after %<free%> of %qE; freed at %@",
				 ev.m_expr, &m_free_event);
    else
      return ev.formatted_print ("use after %<free%> of %qE", ev.m_expr);
  }

private:
  diagnostic_event_id_t m_free_event;
};

class malloc_leak : public malloc_diagnostic
{
public:
  malloc_leak (const malloc_state_machine &sm, tree arg)
  : malloc_diagnostic (sm, arg) {}

  const char *get_kind () const FINAL OVERRIDE { return "malloc_leak"; }

  bool emit (rich_location *rich_loc) FINAL OVERRIDE
  {
    diagnostic_metadata m;
    m.add_cwe (401);
    return warning_meta (rich_loc, m, OPT_Wanalyzer_malloc_leak,
			 "leak of %qE", m_arg);
  }

  label_text describe_state_change (const evdesc::state_change &change)
    FINAL OVERRIDE
  {
    if (change.m_new_state == m_sm.m_unchecked)
      {
	m_malloc_event = change.m_event_id;
	return label_text::borrow ("allocated here");
      }
    return malloc_diagnostic::describe_state_change (change);
  }

  label_text describe_final_event (const evdesc::final_event &ev) FINAL OVERRIDE
  {
    if (m_malloc_event.known_p ())
      return ev.formatted_print ("%qE leaks here; was allocated at %@",
				 ev.m_expr, &m_malloc_event);
    else
      return ev.formatted_print ("%qE leaks here", ev.m_expr);
  }

private:
  diagnostic_event_id_t m_malloc_event;
};

class free_of_non_heap : public malloc_diagnostic
{
public:
  free_of_non_heap (const malloc_state_machine &sm, tree arg)
  : malloc_diagnostic (sm, arg), m_kind (KIND_UNKNOWN)
  {
  }

  const char *get_kind () const FINAL OVERRIDE { return "free_of_non_heap"; }

  bool subclass_equal_p (const pending_diagnostic &base_other) const
    FINAL OVERRIDE
  {
    const free_of_non_heap &other = (const free_of_non_heap &)base_other;
    return (same_tree_p (m_arg, other.m_arg) && m_kind == other.m_kind);
  }

  bool emit (rich_location *rich_loc) FINAL OVERRIDE
  {
    auto_diagnostic_group d;
    diagnostic_metadata m;
    m.add_cwe (590); /* CWE-590: Free of Memory not on the Heap.  */
    switch (m_kind)
      {
      default:
	gcc_unreachable ();
      case KIND_UNKNOWN:
	return warning_meta (rich_loc, m, OPT_Wanalyzer_free_of_non_heap,
			     "%<free%> of %qE which points to memory"
			     " not on the heap",
			     m_arg);
	break;
      case KIND_ALLOCA:
	return warning_meta (rich_loc, m, OPT_Wanalyzer_free_of_non_heap,
			     "%<free%> of memory allocated on the stack by"
			     " %qs (%qE) will corrupt the heap",
			     "alloca", m_arg);
	break;
      }
  }

  label_text describe_state_change (const evdesc::state_change &change)
    FINAL OVERRIDE
  {
    /* Attempt to reconstruct what kind of pointer it is.
       (It seems neater for this to be a part of the state, though).  */
    if (TREE_CODE (change.m_expr) == SSA_NAME)
      {
	gimple *def_stmt = SSA_NAME_DEF_STMT (change.m_expr);
	if (gcall *call = dyn_cast <gcall *> (def_stmt))
	  {
	    if (is_special_named_call_p (call, "alloca", 1)
		|| is_special_named_call_p (call, "__builtin_alloca", 1))
	      {
		m_kind = KIND_ALLOCA;
		return label_text::borrow
		  ("memory is allocated on the stack here");
	      }
	  }
      }
    return label_text::borrow ("pointer is from here");
  }

  label_text describe_final_event (const evdesc::final_event &ev) FINAL OVERRIDE
  {
    return ev.formatted_print ("call to %qs here", "free");
  }

private:
  enum kind
  {
    KIND_UNKNOWN,
    KIND_ALLOCA
  };
  enum kind m_kind;
};

/* malloc_state_machine's ctor.  */

malloc_state_machine::malloc_state_machine (logger *logger)
: state_machine ("malloc", logger)
{
  m_start = add_state ("start");
  m_unchecked = add_state ("unchecked");
  m_null = add_state ("null");
  m_nonnull = add_state ("nonnull");
  m_freed = add_state ("freed");
  m_non_heap = add_state ("non-heap");
  m_stop = add_state ("stop");
}

/* Implementation of state_machine::on_stmt vfunc for malloc_state_machine.  */

bool
malloc_state_machine::on_stmt (sm_context *sm_ctxt,
			       const supernode *node,
			       const gimple *stmt) const
{
  if (const gcall *call = dyn_cast <const gcall *> (stmt))
    if (tree callee_fndecl = sm_ctxt->get_fndecl_for_call (call))
      {
	if (is_named_call_p (callee_fndecl, "malloc", call, 1)
	    || is_named_call_p (callee_fndecl, "calloc", call, 2)
	    || is_std_named_call_p (callee_fndecl, "malloc", call, 1)
	    || is_std_named_call_p (callee_fndecl, "calloc", call, 2)
	    || is_named_call_p (callee_fndecl, "__builtin_malloc", call, 1)
	    || is_named_call_p (callee_fndecl, "__builtin_calloc", call, 2))
	  {
	    tree lhs = gimple_call_lhs (call);
	    if (lhs)
	      {
		lhs = sm_ctxt->get_readable_tree (lhs);
		sm_ctxt->on_transition (node, stmt, lhs, m_start, m_unchecked);
	      }
	    else
	      {
		/* TODO: report leak.  */
	      }
	    return true;
	  }

	if (is_named_call_p (callee_fndecl, "alloca", call, 1)
	    || is_named_call_p (callee_fndecl, "__builtin_alloca", call, 1))
	  {
	    tree lhs = gimple_call_lhs (call);
	    if (lhs)
	      {
		lhs = sm_ctxt->get_readable_tree (lhs);
		sm_ctxt->on_transition (node, stmt, lhs, m_start, m_non_heap);
	      }
	    return true;
	  }

	if (is_named_call_p (callee_fndecl, "free", call, 1)
	    || is_std_named_call_p (callee_fndecl, "free", call, 1)
	    || is_named_call_p (callee_fndecl, "__builtin_free", call, 1))
	  {
	    tree arg = gimple_call_arg (call, 0);

	    arg = sm_ctxt->get_readable_tree (arg);

	    /* start/unchecked/nonnull -> freed.  */
	    sm_ctxt->on_transition (node, stmt, arg, m_start, m_freed);
	    sm_ctxt->on_transition (node, stmt, arg, m_unchecked, m_freed);
	    sm_ctxt->on_transition (node, stmt, arg, m_nonnull, m_freed);

	    /* Keep state "null" as-is, rather than transitioning to "free";
	       we don't want to complain about double-free of NULL.  */

	    /* freed -> stop, with warning.  */
	    sm_ctxt->warn_for_state (node, stmt, arg, m_freed,
				     new double_free (*this, arg));
	    sm_ctxt->on_transition (node, stmt, arg, m_freed, m_stop);

	    /* non-heap -> stop, with warning.  */
	    sm_ctxt->warn_for_state (node, stmt, arg, m_non_heap,
				     new free_of_non_heap (*this, arg));
	    sm_ctxt->on_transition (node, stmt, arg, m_non_heap, m_stop);
	    return true;
	  }

	/* Handle "__attribute__((nonnull))".   */
	{
	  tree fntype = TREE_TYPE (callee_fndecl);
	  bitmap nonnull_args = get_nonnull_args (fntype);
	  if (nonnull_args)
	    {
	      for (unsigned i = 0; i < gimple_call_num_args (stmt); i++)
		{
		  tree arg = gimple_call_arg (stmt, i);
		  if (TREE_CODE (TREE_TYPE (arg)) != POINTER_TYPE)
		    continue;
		  /* If we have a nonnull-args, and either all pointers, or just
		     the specified pointers.  */
		  if (bitmap_empty_p (nonnull_args)
		      || bitmap_bit_p (nonnull_args, i))
		    {
		      sm_ctxt->warn_for_state
			(node, stmt, arg, m_unchecked,
			 new possible_null_arg (*this, arg, callee_fndecl, i));
		      sm_ctxt->on_transition (node, stmt, arg, m_unchecked,
					      m_nonnull);

		      sm_ctxt->warn_for_state
			(node, stmt, arg, m_null,
			 new null_arg (*this, arg, callee_fndecl, i));
		      sm_ctxt->on_transition (node, stmt, arg, m_null, m_stop);
		    }
		}
	      BITMAP_FREE (nonnull_args);
	    }
	}
      }

  if (tree lhs = is_zero_assignment (stmt))
    if (any_pointer_p (lhs))
      on_zero_assignment (sm_ctxt, node, stmt,lhs);

  if (const gassign *assign_stmt = dyn_cast <const gassign *> (stmt))
    {
      enum tree_code op = gimple_assign_rhs_code (assign_stmt);
      if (op == ADDR_EXPR)
	{
	  tree lhs = gimple_assign_lhs (assign_stmt);
	  if (lhs)
	    {
	      lhs = sm_ctxt->get_readable_tree (lhs);
	      sm_ctxt->on_transition (node, stmt, lhs, m_start, m_non_heap);
	    }
	}
    }

  /* Handle dereferences.  */
  for (unsigned i = 0; i < gimple_num_ops (stmt); i++)
    {
      tree op = gimple_op (stmt, i);
      if (!op)
	continue;
      if (TREE_CODE (op) == COMPONENT_REF)
	op = TREE_OPERAND (op, 0);

      if (TREE_CODE (op) == MEM_REF)
	{
	  tree arg = TREE_OPERAND (op, 0);
	  arg = sm_ctxt->get_readable_tree (arg);

	  sm_ctxt->warn_for_state (node, stmt, arg, m_unchecked,
				   new possible_null_deref (*this, arg));
	  sm_ctxt->on_transition (node, stmt, arg, m_unchecked, m_nonnull);

	  sm_ctxt->warn_for_state (node, stmt, arg, m_null,
				   new null_deref (*this, arg));
	  sm_ctxt->on_transition (node, stmt, arg, m_null, m_stop);

	  sm_ctxt->warn_for_state (node, stmt, arg, m_freed,
				   new use_after_free (*this, arg));
	  sm_ctxt->on_transition (node, stmt, arg, m_freed, m_stop);
	}
    }
  return false;
}

/* Implementation of state_machine::on_phi vfunc for malloc_state_machine.  */

void
malloc_state_machine::on_phi (sm_context *sm_ctxt,
			      const supernode *node,
			      const gphi *phi,
			      tree rhs) const
{
  if (zerop (rhs))
    {
      tree lhs = gimple_phi_result (phi);
      on_zero_assignment (sm_ctxt, node, phi, lhs);
    }
}

/* Implementation of state_machine::on_condition vfunc for malloc_state_machine.
   Potentially transition state 'unchecked' to 'nonnull' or to 'null'.  */

void
malloc_state_machine::on_condition (sm_context *sm_ctxt,
				    const supernode *node,
				    const gimple *stmt,
				    tree lhs,
				    enum tree_code op,
				    tree rhs) const
{
  if (!zerop (rhs))
    return;

  if (!any_pointer_p (lhs))
    return;
  if (!any_pointer_p (rhs))
    return;

  if (op == NE_EXPR)
    {
      log ("got 'ARG != 0' match");
      sm_ctxt->on_transition (node, stmt,
			      lhs, m_unchecked, m_nonnull);
    }
  else if (op == EQ_EXPR)
    {
      log ("got 'ARG == 0' match");
      sm_ctxt->on_transition (node, stmt,
			      lhs, m_unchecked, m_null);
    }
}

/* Implementation of state_machine::can_purge_p vfunc for malloc_state_machine.
   Don't allow purging of pointers in state 'unchecked' or 'nonnull'
   (to avoid false leak reports).  */

bool
malloc_state_machine::can_purge_p (state_t s) const
{
  return s != m_unchecked && s != m_nonnull;
}

/* Implementation of state_machine::on_leak vfunc for malloc_state_machine
   (for complaining about leaks of pointers in state 'unchecked' and
   'nonnull').  */

pending_diagnostic *
malloc_state_machine::on_leak (tree var) const
{
  return new malloc_leak (*this, var);
}

/* Shared logic for handling GIMPLE_ASSIGNs and GIMPLE_PHIs that
   assign zero to LHS.  */

void
malloc_state_machine::on_zero_assignment (sm_context *sm_ctxt,
					  const supernode *node,
					  const gimple *stmt,
					  tree lhs) const
{
  sm_ctxt->on_transition (node, stmt, lhs, m_start, m_null);
  sm_ctxt->on_transition (node, stmt, lhs, m_unchecked, m_null);
  sm_ctxt->on_transition (node, stmt, lhs, m_nonnull, m_null);
  sm_ctxt->on_transition (node, stmt, lhs, m_freed, m_null);
}

} // anonymous namespace

/* Internal interface to this file. */

state_machine *
make_malloc_state_machine (logger *logger)
{
  return new malloc_state_machine (logger);
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
