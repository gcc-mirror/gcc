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
#include "tristate.h"
#include "selftest.h"
#include "analyzer/call-string.h"
#include "analyzer/program-point.h"
#include "analyzer/store.h"
#include "analyzer/region-model.h"

#if ENABLE_ANALYZER

namespace ana {

namespace {

class api;
class malloc_state_machine;

/* An enum for discriminating between different kinds of allocation_state.  */

enum resource_state
{
  /* States that are independent of api.  */

  /* The start state.  */
  RS_START,

  /* State for a pointer that's known to be NULL.  */
  RS_NULL,

  /* State for a pointer that's known to not be on the heap (e.g. to a local
     or global).  */
  RS_NON_HEAP,

  /* Stop state, for pointers we don't want to track any more.  */
  RS_STOP,

  /* States that relate to a specific api.  */

  /* State for a pointer returned from the api's allocator that hasn't
     been checked for NULL.
     It could be a pointer to heap-allocated memory, or could be NULL.  */
  RS_UNCHECKED,

  /* State for a pointer returned from the api's allocator,
     known to be non-NULL.  */
  RS_NONNULL,

  /* State for a pointer passed to the api's deallocator.  */
  RS_FREED
};

/* Custom state subclass, which can optionally refer to an an api.  */

struct allocation_state : public state_machine::state
{
  allocation_state (const char *name, unsigned id,
		    enum resource_state rs, const api *a)
  : state (name, id), m_rs (rs), m_api (a)
  {}

  void dump_to_pp (pretty_printer *pp) const FINAL OVERRIDE;

  const allocation_state *get_nonnull () const;

  enum resource_state m_rs;
  const api *m_api;
};

/* An enum for choosing which wording to use in various diagnostics
   when describing deallocations.  */

enum wording
{
  WORDING_FREED,
  WORDING_DELETED
};

/* Represents a particular family of API calls for allocating/deallocating
   heap memory that should be matched e.g.
   - malloc/free
   - scalar new/delete
   - vector new[]/delete[]
   etc.

   We track the expected deallocation function, but not the allocation
   function - there could be more than one allocator per deallocator.  For
   example, there could be dozens of allocators for "free" beyond just
   malloc e.g. calloc, xstrdup, etc.  We don't want to explode the number
   of states by tracking individual allocators in the exploded graph;
   we merely want to track "this value expects to have 'free' called on it".
   Perhaps we can reconstruct which allocator was used later, when emitting
   the path, if it's necessary for precision of wording of diagnostics.  */

struct api
{
  api (malloc_state_machine *sm,
       const char *name,
       const char *dealloc_funcname,
       enum wording wording);

  /* An internal name for identifying this API in dumps.  */
  const char *m_name;

  /* The name of the deallocation function, for use in diagnostics.  */
  const char *m_dealloc_funcname;

  /* Which wording to use in diagnostics.  */
  enum wording m_wording;

  /* Pointers to api-specific states.
     These states are owned by the state_machine base class.  */

  /* State for an unchecked result from this api's allocator.  */
  state_machine::state_t m_unchecked;

  /* State for a known non-NULL result from this apis's allocator.  */
  state_machine::state_t m_nonnull;

  /* State for a value passed to this api's deallocator.  */
  state_machine::state_t m_freed;
};

/* A state machine for detecting misuses of the malloc/free API.

   See sm-malloc.dot for an overview (keep this in-sync with that file).  */

class malloc_state_machine : public state_machine
{
public:
  typedef allocation_state custom_data_t;

  malloc_state_machine (logger *logger);

  state_t
  add_state (const char *name, enum resource_state rs, const api *a);

  bool inherited_state_p () const FINAL OVERRIDE { return false; }

  state_machine::state_t
  get_default_state (const svalue *sval) const FINAL OVERRIDE
  {
    if (tree cst = sval->maybe_get_constant ())
      {
	if (zerop (cst))
	  return m_null;
      }
    if (const region_svalue *ptr = sval->dyn_cast_region_svalue ())
      {
	const region *reg = ptr->get_pointee ();
	if (reg->get_kind () == RK_STRING)
	  return m_non_heap;
      }
    return m_start;
  }

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

  bool reset_when_passed_to_unknown_fn_p (state_t s,
					  bool is_mutable) const FINAL OVERRIDE;

  api m_malloc;
  api m_scalar_new;
  api m_vector_new;

  /* States that are independent of api.  */

  /* State for a pointer that's known to be NULL.  */
  state_t m_null;

  /* State for a pointer that's known to not be on the heap (e.g. to a local
     or global).  */
  state_t m_non_heap; // TODO: or should this be a different state machine?
  // or do we need child values etc?

  /* Stop state, for pointers we don't want to track any more.  */
  state_t m_stop;

private:
  void on_allocator_call (sm_context *sm_ctxt,
			  const gcall *call,
			  const api &ap) const;
  void on_deallocator_call (sm_context *sm_ctxt,
			    const supernode *node,
			    const gcall *call,
			    const api &ap) const;
  void on_zero_assignment (sm_context *sm_ctxt,
			   const gimple *stmt,
			   tree lhs) const;
};

/* struct api.  */

api::api (malloc_state_machine *sm,
	  const char *name,
	  const char *dealloc_funcname,
	  enum wording wording)
: m_name (name),
  m_dealloc_funcname (dealloc_funcname),
  m_wording (wording),
  m_unchecked (sm->add_state ("unchecked", RS_UNCHECKED, this)),
  m_nonnull (sm->add_state ("nonnull", RS_NONNULL, this)),
  m_freed (sm->add_state ("freed", RS_FREED, this))
{
}

/* Return STATE cast to the custom state subclass, or NULL for the start state.
   Everything should be an allocation_state apart from the start state.  */

static const allocation_state *
dyn_cast_allocation_state (state_machine::state_t state)
{
  if (state->get_id () == 0)
    return NULL;
  return static_cast <const allocation_state *> (state);
}

/* Return STATE cast to the custom state subclass, for a state that is
   already known to not be the start state .  */

static const allocation_state *
as_a_allocation_state (state_machine::state_t state)
{
  gcc_assert (state->get_id () != 0);
  return static_cast <const allocation_state *> (state);
}

/* Get the resource_state for STATE.  */

static enum resource_state
get_rs (state_machine::state_t state)
{
  if (const allocation_state *astate = dyn_cast_allocation_state (state))
    return astate->m_rs;
  else
    return RS_START;
}

/* Return true if STATE is an unchecked result from an allocator.  */

static bool
unchecked_p (state_machine::state_t state)
{
  return get_rs (state) == RS_UNCHECKED;
}

/* Return true if STATE is a non-null result from an allocator.  */

static bool
nonnull_p (state_machine::state_t state)
{
  return get_rs (state) == RS_NONNULL;
}

/* Return true if STATE is a value that has been passed to a deallocator.  */

static bool
freed_p (state_machine::state_t state)
{
  return get_rs (state) == RS_FREED;
}

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
    if (change.m_old_state == m_sm.get_start_state ()
	&& unchecked_p (change.m_new_state))
      // TODO: verify that it's the allocation stmt, not a copy
      return label_text::borrow ("allocated here");
    if (unchecked_p (change.m_old_state)
	&& nonnull_p (change.m_new_state))
      {
	if (change.m_expr)
	  return change.formatted_print ("assuming %qE is non-NULL",
					 change.m_expr);
	else
	  return change.formatted_print ("assuming %qs is non-NULL",
					 "<unknown>");
      }
    if (change.m_new_state == m_sm.m_null)
      {
	if (unchecked_p (change.m_old_state))
	  {
	    if (change.m_expr)
	      return change.formatted_print ("assuming %qE is NULL",
					     change.m_expr);
	    else
	      return change.formatted_print ("assuming %qs is NULL",
					     "<unknown>");
	  }
	else
	  {
	    if (change.m_expr)
	      return change.formatted_print ("%qE is NULL",
					     change.m_expr);
	    else
	      return change.formatted_print ("%qs is NULL",
					     "<unknown>");
	  }
      }

    return label_text ();
  }

protected:
  const malloc_state_machine &m_sm;
  tree m_arg;
};

/* Concrete subclass for reporting mismatching allocator/deallocator
   diagnostics.  */

class mismatching_deallocation : public malloc_diagnostic
{
public:
  mismatching_deallocation (const malloc_state_machine &sm, tree arg,
			    const api *expected_dealloc,
			    const api *actual_dealloc)
  : malloc_diagnostic (sm, arg),
    m_expected_dealloc (expected_dealloc),
    m_actual_dealloc (actual_dealloc)
  {}

  const char *get_kind () const FINAL OVERRIDE
  {
    return "mismatching_deallocation";
  }

  bool emit (rich_location *rich_loc) FINAL OVERRIDE
  {
    auto_diagnostic_group d;
    diagnostic_metadata m;
    m.add_cwe (762); /* CWE-762: Mismatched Memory Management Routines.  */
    return warning_meta (rich_loc, m, OPT_Wanalyzer_mismatching_deallocation,
			 "%qE should have been deallocated with %qs"
			 " but was deallocated with %qs",
			 m_arg, m_expected_dealloc->m_dealloc_funcname,
			 m_actual_dealloc->m_dealloc_funcname);
  }

  label_text describe_state_change (const evdesc::state_change &change)
    FINAL OVERRIDE
  {
    if (unchecked_p (change.m_new_state))
      {
	m_alloc_event = change.m_event_id;
	return change.formatted_print ("allocated here"
				       " (expects deallocation with %qs)",
				       m_expected_dealloc->m_dealloc_funcname);
      }
    return malloc_diagnostic::describe_state_change (change);
  }

  label_text describe_final_event (const evdesc::final_event &ev) FINAL OVERRIDE
  {
    if (m_alloc_event.known_p ())
      return ev.formatted_print
	("deallocated with %qs here;"
	 " allocation at %@ expects deallocation with %qs",
	 m_actual_dealloc->m_dealloc_funcname, &m_alloc_event,
	 m_expected_dealloc->m_dealloc_funcname);
    return ev.formatted_print ("deallocated with %qs here",
			       m_actual_dealloc->m_dealloc_funcname);
  }

private:
  diagnostic_event_id_t m_alloc_event;
  const api *m_expected_dealloc;
  const api *m_actual_dealloc;
};

/* Concrete subclass for reporting double-free diagnostics.  */

class double_free : public malloc_diagnostic
{
public:
  double_free (const malloc_state_machine &sm, tree arg, const char *funcname)
  : malloc_diagnostic (sm, arg), m_funcname (funcname)
  {}

  const char *get_kind () const FINAL OVERRIDE { return "double_free"; }

  bool emit (rich_location *rich_loc) FINAL OVERRIDE
  {
    auto_diagnostic_group d;
    diagnostic_metadata m;
    m.add_cwe (415); /* CWE-415: Double Free.  */
    return warning_meta (rich_loc, m, OPT_Wanalyzer_double_free,
			 "double-%<%s%> of %qE", m_funcname, m_arg);
  }

  label_text describe_state_change (const evdesc::state_change &change)
    FINAL OVERRIDE
  {
    if (freed_p (change.m_new_state))
      {
	m_first_free_event = change.m_event_id;
	return change.formatted_print ("first %qs here", m_funcname);
      }
    return malloc_diagnostic::describe_state_change (change);
  }

  label_text describe_call_with_state (const evdesc::call_with_state &info)
    FINAL OVERRIDE
  {
    if (freed_p (info.m_state))
      return info.formatted_print
	("passing freed pointer %qE in call to %qE from %qE",
	 info.m_expr, info.m_callee_fndecl, info.m_caller_fndecl);
    return label_text ();
  }

  label_text describe_final_event (const evdesc::final_event &ev) FINAL OVERRIDE
  {
    if (m_first_free_event.known_p ())
      return ev.formatted_print ("second %qs here; first %qs was at %@",
				 m_funcname, m_funcname,
				 &m_first_free_event);
    return ev.formatted_print ("second %qs here", m_funcname);
  }

private:
  diagnostic_event_id_t m_first_free_event;
  const char *m_funcname;
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
    if (change.m_old_state == m_sm.get_start_state ()
	&& unchecked_p (change.m_new_state))
      {
	m_origin_of_unchecked_event = change.m_event_id;
	return label_text::borrow ("this call could return NULL");
      }
    return malloc_diagnostic::describe_state_change (change);
  }

  label_text describe_return_of_state (const evdesc::return_of_state &info)
    FINAL OVERRIDE
  {
    if (unchecked_p (info.m_state))
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

    bool warned;
    if (zerop (m_arg))
      warned = warning_meta (rich_loc, m, OPT_Wanalyzer_null_argument,
			     "use of NULL where non-null expected");
    else
      warned = warning_meta (rich_loc, m, OPT_Wanalyzer_null_argument,
			     "use of NULL %qE where non-null expected",
			     m_arg);
    if (warned)
      inform_nonnull_attribute (m_fndecl, m_arg_idx);
    return warned;
  }

  label_text describe_final_event (const evdesc::final_event &ev) FINAL OVERRIDE
  {
    if (zerop (ev.m_expr))
      return ev.formatted_print ("argument %u NULL where non-null expected",
				 m_arg_idx + 1);
    else
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
  use_after_free (const malloc_state_machine &sm, tree arg,
		  const api *a)
  : malloc_diagnostic (sm, arg), m_api (a)
  {}

  const char *get_kind () const FINAL OVERRIDE { return "use_after_free"; }

  bool emit (rich_location *rich_loc) FINAL OVERRIDE
  {
    /* CWE-416: Use After Free.  */
    diagnostic_metadata m;
    m.add_cwe (416);
    return warning_meta (rich_loc, m, OPT_Wanalyzer_use_after_free,
			 "use after %<%s%> of %qE",
			 m_api->m_dealloc_funcname, m_arg);
  }

  label_text describe_state_change (const evdesc::state_change &change)
    FINAL OVERRIDE
  {
    if (freed_p (change.m_new_state))
      {
	m_free_event = change.m_event_id;
	switch (m_api->m_wording)
	  {
	  default:
	    gcc_unreachable ();
	  case WORDING_FREED:
	    return label_text::borrow ("freed here");
	  case WORDING_DELETED:
	    return label_text::borrow ("deleted here");
	  }
      }
    return malloc_diagnostic::describe_state_change (change);
  }

  label_text describe_final_event (const evdesc::final_event &ev) FINAL OVERRIDE
  {
    const char *funcname = m_api->m_dealloc_funcname;
    if (m_free_event.known_p ())
      switch (m_api->m_wording)
	{
	default:
	  gcc_unreachable ();
	case WORDING_FREED:
	  return ev.formatted_print ("use after %<%s%> of %qE; freed at %@",
				     funcname, ev.m_expr, &m_free_event);
	case WORDING_DELETED:
	  return ev.formatted_print ("use after %<%s%> of %qE; deleted at %@",
				     funcname, ev.m_expr, &m_free_event);
	}
    else
      return ev.formatted_print ("use after %<%s%> of %qE",
				 funcname, ev.m_expr);
  }

private:
  diagnostic_event_id_t m_free_event;
  const api *m_api;
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
    if (m_arg)
      return warning_meta (rich_loc, m, OPT_Wanalyzer_malloc_leak,
			   "leak of %qE", m_arg);
    else
      return warning_meta (rich_loc, m, OPT_Wanalyzer_malloc_leak,
			   "leak of %qs", "<unknown>");
  }

  label_text describe_state_change (const evdesc::state_change &change)
    FINAL OVERRIDE
  {
    if (unchecked_p (change.m_new_state))
      {
	m_alloc_event = change.m_event_id;
	return label_text::borrow ("allocated here");
      }
    return malloc_diagnostic::describe_state_change (change);
  }

  label_text describe_final_event (const evdesc::final_event &ev) FINAL OVERRIDE
  {
    if (ev.m_expr)
      {
	if (m_alloc_event.known_p ())
	  return ev.formatted_print ("%qE leaks here; was allocated at %@",
				     ev.m_expr, &m_alloc_event);
	else
	  return ev.formatted_print ("%qE leaks here", ev.m_expr);
      }
    else
      {
	if (m_alloc_event.known_p ())
	  return ev.formatted_print ("%qs leaks here; was allocated at %@",
				     "<unknown>", &m_alloc_event);
	else
	  return ev.formatted_print ("%qs leaks here", "<unknown>");
      }
  }

private:
  diagnostic_event_id_t m_alloc_event;
};

class free_of_non_heap : public malloc_diagnostic
{
public:
  free_of_non_heap (const malloc_state_machine &sm, tree arg,
		    const char *funcname)
  : malloc_diagnostic (sm, arg), m_funcname (funcname), m_kind (KIND_UNKNOWN)
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
			     "%<%s%> of %qE which points to memory"
			     " not on the heap",
			     m_funcname, m_arg);
	break;
      case KIND_ALLOCA:
	return warning_meta (rich_loc, m, OPT_Wanalyzer_free_of_non_heap,
			     "%<%s%> of memory allocated on the stack by"
			     " %qs (%qE) will corrupt the heap",
			     m_funcname, "alloca", m_arg);
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
    return ev.formatted_print ("call to %qs here", m_funcname);
  }

private:
  enum kind
  {
    KIND_UNKNOWN,
    KIND_ALLOCA
  };
  const char *m_funcname;
  enum kind m_kind;
};

/* struct allocation_state : public state_machine::state.  */

/* Implementation of state_machine::state::dump_to_pp vfunc
   for allocation_state: append the API that this allocation is
   associated with.  */

void
allocation_state::dump_to_pp (pretty_printer *pp) const
{
  state_machine::state::dump_to_pp (pp);
  if (m_api)
    pp_printf (pp, " (%s)", m_api->m_name);
}

/* Given a allocation_state for an api, get the "nonnull" state
   for the corresponding allocator.  */

const allocation_state *
allocation_state::get_nonnull () const
{
  gcc_assert (m_api);
  return as_a_allocation_state (m_api->m_nonnull);
}

/* malloc_state_machine's ctor.  */

malloc_state_machine::malloc_state_machine (logger *logger)
: state_machine ("malloc", logger),
  m_malloc (this, "malloc", "free", WORDING_FREED),
  m_scalar_new (this, "new", "delete", WORDING_DELETED),
  m_vector_new (this, "new[]", "delete[]", WORDING_DELETED)
{
  gcc_assert (m_start->get_id () == 0);
  m_null = add_state ("null", RS_FREED, NULL);
  m_non_heap = add_state ("non-heap", RS_NON_HEAP, NULL);
  m_stop = add_state ("stop", RS_STOP, NULL);
}

state_machine::state_t
malloc_state_machine::add_state (const char *name, enum resource_state rs,
				 const api *a)
{
  return add_custom_state (new allocation_state (name, alloc_state_id (),
						 rs, a));
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
	    || is_named_call_p (callee_fndecl, "__builtin_calloc", call, 2)
	    || is_named_call_p (callee_fndecl, "strdup", call, 1)
	    || is_named_call_p (callee_fndecl, "strndup", call, 2))
	  {
	    on_allocator_call (sm_ctxt, call, m_malloc);
	    return true;
	  }

	if (is_named_call_p (callee_fndecl, "operator new", call, 1))
	  on_allocator_call (sm_ctxt, call, m_scalar_new);
	else if (is_named_call_p (callee_fndecl, "operator new []", call, 1))
	  on_allocator_call (sm_ctxt, call, m_vector_new);
	else if (is_named_call_p (callee_fndecl, "operator delete", call, 1)
		 || is_named_call_p (callee_fndecl, "operator delete", call, 2))
	  {
	    on_deallocator_call (sm_ctxt, node, call, m_scalar_new);
	    return true;
	  }
	else if (is_named_call_p (callee_fndecl, "operator delete []", call, 1))
	  {
	    on_deallocator_call (sm_ctxt, node, call, m_vector_new);
	    return true;
	  }

	if (is_named_call_p (callee_fndecl, "alloca", call, 1)
	    || is_named_call_p (callee_fndecl, "__builtin_alloca", call, 1))
	  {
	    tree lhs = gimple_call_lhs (call);
	    if (lhs)
	      sm_ctxt->on_transition (node, stmt, lhs, m_start, m_non_heap);
	    return true;
	  }

	if (is_named_call_p (callee_fndecl, "free", call, 1)
	    || is_std_named_call_p (callee_fndecl, "free", call, 1)
	    || is_named_call_p (callee_fndecl, "__builtin_free", call, 1))
	  {
	    on_deallocator_call (sm_ctxt, node, call, m_malloc);
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
		      tree diag_arg = sm_ctxt->get_diagnostic_tree (arg);
		      state_t state = sm_ctxt->get_state (stmt, arg);
		      /* Can't use a switch as the states are non-const.  */
		      if (unchecked_p (state))
			{
			  sm_ctxt->warn (node, stmt, arg,
					 new possible_null_arg (*this, diag_arg,
								callee_fndecl,
								i));
			  const allocation_state *astate
			    = as_a_allocation_state (state);
			  sm_ctxt->set_next_state (stmt, arg,
						   astate->get_nonnull ());
			}
		      else if (state == m_null)
			{
			  sm_ctxt->warn (node, stmt, arg,
					 new null_arg (*this, diag_arg,
						       callee_fndecl, i));
			  sm_ctxt->set_next_state (stmt, arg, m_stop);
			}
		    }
		}
	      BITMAP_FREE (nonnull_args);
	    }
	}
      }

  if (tree lhs = sm_ctxt->is_zero_assignment (stmt))
    if (any_pointer_p (lhs))
      on_zero_assignment (sm_ctxt, stmt,lhs);

  /* If we have "LHS = &EXPR;" and EXPR is something other than a MEM_REF,
     transition LHS from start to non_heap.
     Doing it for ADDR_EXPR(MEM_REF()) is likely wrong, and can lead to
     unbounded chains of unmergeable sm-state on pointer arithmetic in loops
     when optimization is enabled.  */
  if (const gassign *assign_stmt = dyn_cast <const gassign *> (stmt))
    {
      enum tree_code op = gimple_assign_rhs_code (assign_stmt);
      if (op == ADDR_EXPR)
	{
	  tree lhs = gimple_assign_lhs (assign_stmt);
	  if (lhs)
	    {
	      tree addr_expr = gimple_assign_rhs1 (assign_stmt);
	      if (TREE_CODE (TREE_OPERAND (addr_expr, 0)) != MEM_REF)
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
	  tree diag_arg = sm_ctxt->get_diagnostic_tree (arg);

	  state_t state = sm_ctxt->get_state (stmt, arg);
	  if (unchecked_p (state))
	    {
	      sm_ctxt->warn (node, stmt, arg,
			     new possible_null_deref (*this, diag_arg));
	      const allocation_state *astate = as_a_allocation_state (state);
	      sm_ctxt->set_next_state (stmt, arg, astate->get_nonnull ());
	    }
	  else if (state == m_null)
	    {
	      sm_ctxt->warn (node, stmt, arg,
			     new null_deref (*this, diag_arg));
	      sm_ctxt->set_next_state (stmt, arg, m_stop);
	    }
	  else if (freed_p (state))
	    {
	      const allocation_state *astate = as_a_allocation_state (state);
	      sm_ctxt->warn (node, stmt, arg,
			     new use_after_free (*this, diag_arg,
						 astate->m_api));
	      sm_ctxt->set_next_state (stmt, arg, m_stop);
	    }
	}
    }
  return false;
}

/* Handle a call to an allocator.  */

void
malloc_state_machine::on_allocator_call (sm_context *sm_ctxt,
					 const gcall *call,
					 const api &ap) const
{
  tree lhs = gimple_call_lhs (call);
  if (lhs)
    {
      if (sm_ctxt->get_state (call, lhs) == m_start)
	sm_ctxt->set_next_state (call, lhs, ap.m_unchecked);
    }
  else
    {
      /* TODO: report leak.  */
    }
}

void
malloc_state_machine::on_deallocator_call (sm_context *sm_ctxt,
					   const supernode *node,
					   const gcall *call,
					   const api &ap) const
{
  tree arg = gimple_call_arg (call, 0);
  tree diag_arg = sm_ctxt->get_diagnostic_tree (arg);

  state_t state = sm_ctxt->get_state (call, arg);

  /* start/unchecked/nonnull -> freed.  */
  if (state == m_start)
    sm_ctxt->set_next_state (call, arg, ap.m_freed);
  else if (unchecked_p (state) || nonnull_p (state))
    {
      const allocation_state *astate = as_a_allocation_state (state);

      if (astate->m_api != &ap)
	{
	  /* Wrong allocator.  */
	  pending_diagnostic *d
	    = new mismatching_deallocation (*this, diag_arg,
					    astate->m_api, &ap);
	  sm_ctxt->warn (node, call, arg, d);
	}
      sm_ctxt->set_next_state (call, arg, ap.m_freed);
    }

  /* Keep state "null" as-is, rather than transitioning to "freed";
     we don't want to complain about double-free of NULL.  */

  else if (state == ap.m_freed)
    {
      /* freed -> stop, with warning.  */
      sm_ctxt->warn (node, call, arg,
		     new double_free (*this, diag_arg,
				      ap.m_dealloc_funcname));
      sm_ctxt->set_next_state (call, arg, m_stop);
    }
  else if (state == m_non_heap)
    {
      /* non-heap -> stop, with warning.  */
      sm_ctxt->warn (node, call, arg,
		     new free_of_non_heap (*this, diag_arg,
					   ap.m_dealloc_funcname));
      sm_ctxt->set_next_state (call, arg, m_stop);
    }
}

/* Implementation of state_machine::on_phi vfunc for malloc_state_machine.  */

void
malloc_state_machine::on_phi (sm_context *sm_ctxt,
			      const supernode *node ATTRIBUTE_UNUSED,
			      const gphi *phi,
			      tree rhs) const
{
  if (zerop (rhs))
    {
      tree lhs = gimple_phi_result (phi);
      on_zero_assignment (sm_ctxt, phi, lhs);
    }
}

/* Implementation of state_machine::on_condition vfunc for malloc_state_machine.
   Potentially transition state 'unchecked' to 'nonnull' or to 'null'.  */

void
malloc_state_machine::on_condition (sm_context *sm_ctxt,
				    const supernode *node ATTRIBUTE_UNUSED,
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
      state_t s = sm_ctxt->get_state (stmt, lhs);
      if (unchecked_p (s))
	{
	  const allocation_state *astate = as_a_allocation_state (s);
	  sm_ctxt->set_next_state (stmt, lhs, astate->get_nonnull ());
	}
    }
  else if (op == EQ_EXPR)
    {
      log ("got 'ARG == 0' match");
      state_t s = sm_ctxt->get_state (stmt, lhs);
      if (unchecked_p (s))
	sm_ctxt->set_next_state (stmt, lhs, m_null);
    }
}

/* Implementation of state_machine::can_purge_p vfunc for malloc_state_machine.
   Don't allow purging of pointers in state 'unchecked' or 'nonnull'
   (to avoid false leak reports).  */

bool
malloc_state_machine::can_purge_p (state_t s) const
{
  enum resource_state rs = get_rs (s);
  return rs != RS_UNCHECKED && rs != RS_NONNULL;
}

/* Implementation of state_machine::on_leak vfunc for malloc_state_machine
   (for complaining about leaks of pointers in state 'unchecked' and
   'nonnull').  */

pending_diagnostic *
malloc_state_machine::on_leak (tree var) const
{
  return new malloc_leak (*this, var);
}

/* Implementation of state_machine::reset_when_passed_to_unknown_fn_p vfunc
   for malloc_state_machine.  */

bool
malloc_state_machine::reset_when_passed_to_unknown_fn_p (state_t s,
							 bool is_mutable) const
{
  /* An on-stack ptr doesn't stop being stack-allocated when passed to an
     unknown fn.  */
  if (s == m_non_heap)
    return false;

  /* Otherwise, pointers passed as non-const can be freed.  */
  return is_mutable;
}

/* Shared logic for handling GIMPLE_ASSIGNs and GIMPLE_PHIs that
   assign zero to LHS.  */

void
malloc_state_machine::on_zero_assignment (sm_context *sm_ctxt,
					  const gimple *stmt,
					  tree lhs) const
{
  state_t s = sm_ctxt->get_state (stmt, lhs);
  enum resource_state rs = get_rs (s);
  if (rs == RS_START
      || rs == RS_UNCHECKED
      || rs == RS_NONNULL
      || rs == RS_FREED)
    sm_ctxt->set_next_state (stmt, lhs, m_null);
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
