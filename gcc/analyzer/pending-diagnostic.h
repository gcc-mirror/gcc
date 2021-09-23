/* Classes for analyzer diagnostics.
   Copyright (C) 2019-2021 Free Software Foundation, Inc.
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

#ifndef GCC_ANALYZER_PENDING_DIAGNOSTIC_H
#define GCC_ANALYZER_PENDING_DIAGNOSTIC_H

namespace ana {

/* Various bundles of information used for generating more precise
   messages for events within a diagnostic_path, for passing to the
   various "describe_*" vfuncs of pending_diagnostic.  See those
   for more information.  */

namespace evdesc {

struct event_desc
{
  event_desc (bool colorize) : m_colorize (colorize) {}

  label_text formatted_print (const char *fmt, ...) const
    ATTRIBUTE_GCC_DIAG(2,3);

  bool m_colorize;
};

/* For use by pending_diagnostic::describe_state_change.  */

struct state_change : public event_desc
{
  state_change (bool colorize,
		tree expr,
		tree origin,
		state_machine::state_t old_state,
		state_machine::state_t new_state,
		diagnostic_event_id_t event_id,
		const state_change_event &event)
  : event_desc (colorize),
    m_expr (expr), m_origin (origin),
    m_old_state (old_state), m_new_state (new_state),
    m_event_id (event_id), m_event (event)
  {}

  bool is_global_p () const { return m_expr == NULL_TREE; }

  tree m_expr;
  tree m_origin;
  state_machine::state_t m_old_state;
  state_machine::state_t m_new_state;
  diagnostic_event_id_t m_event_id;
  const state_change_event &m_event;
};

/* For use by pending_diagnostic::describe_call_with_state.  */

struct call_with_state : public event_desc
{
  call_with_state (bool colorize,
		   tree caller_fndecl, tree callee_fndecl,
		   tree expr, state_machine::state_t state)
  : event_desc (colorize),
    m_caller_fndecl (caller_fndecl),
    m_callee_fndecl (callee_fndecl),
    m_expr (expr),
    m_state (state)
  {
  }

  tree m_caller_fndecl;
  tree m_callee_fndecl;
  tree m_expr;
  state_machine::state_t m_state;
};

/* For use by pending_diagnostic::describe_return_of_state.  */

struct return_of_state : public event_desc
{
  return_of_state (bool colorize,
		   tree caller_fndecl, tree callee_fndecl,
		   state_machine::state_t state)
  : event_desc (colorize),
    m_caller_fndecl (caller_fndecl),
    m_callee_fndecl (callee_fndecl),
    m_state (state)
  {
  }

  tree m_caller_fndecl;
  tree m_callee_fndecl;
  state_machine::state_t m_state;
};

/* For use by pending_diagnostic::describe_final_event.  */

struct final_event : public event_desc
{
  final_event (bool colorize,
	       tree expr, state_machine::state_t state)
  : event_desc (colorize),
    m_expr (expr), m_state (state)
  {}

  tree m_expr;
  state_machine::state_t m_state;
};

} /* end of namespace evdesc */

/* An abstract base class for capturing information about a diagnostic in
   a form that is ready to emit at a later point (or be rejected).
   Each kind of diagnostic will have a concrete subclass of
   pending_diagnostic.

   Normally, gcc diagnostics are emitted using va_list, which can't be
   portably stored for later use, so we have to use an "emit" virtual
   function.

   This class also supports comparison, so that multiple pending_diagnostic
   instances can be de-duplicated.

   As well as emitting a diagnostic, the class has various "precision of
   wording" virtual functions, for generating descriptions for events
   within a diagnostic_path.  These are optional, but implementing these
   allows for more precise wordings than the more generic
   implementation.  */

class pending_diagnostic
{
 public:
  virtual ~pending_diagnostic () {}

  /* Vfunc for emitting the diagnostic.  The rich_location will have been
     populated with a diagnostic_path.
     Return true if a diagnostic is actually emitted.  */
  virtual bool emit (rich_location *) = 0;

  /* Hand-coded RTTI: get an ID for the subclass.  */
  virtual const char *get_kind () const = 0;

  /* A vfunc for identifying "use of uninitialized value".  */
  virtual bool use_of_uninit_p () const { return false; }

  /* Compare for equality with OTHER, which might be of a different
     subclass.  */

  bool equal_p (const pending_diagnostic &other) const
  {
    /* Check for pointer equality on the IDs from get_kind.  */
    if (get_kind () != other.get_kind ())
      return false;
    /* Call vfunc now we know they have the same ID: */
    return subclass_equal_p (other);
  }

  /* A vfunc for testing for equality, where we've already
     checked they have the same ID.  See pending_diagnostic_subclass
     below for a convenience subclass for implementing this.  */
  virtual bool subclass_equal_p (const pending_diagnostic &other) const = 0;

  /* Return true if T1 and T2 are "the same" for the purposes of
     diagnostic deduplication.  */
  static bool same_tree_p (tree t1, tree t2);

  /* A vfunc for fixing up locations (both the primary location for the
     diagnostic, and for events in their paths), e.g. to avoid unwinding
     inside specific macros.  */
  virtual location_t fixup_location (location_t loc) const
  {
    return loc;
  }

  /* For greatest precision-of-wording, the various following "describe_*"
     virtual functions give the pending diagnostic a way to describe events
     in a diagnostic_path in terms that make sense for that diagnostic.

     In each case, return a non-NULL label_text to give the event a custom
     description; NULL otherwise (falling back on a more generic
     description).  */

  /* Precision-of-wording vfunc for describing a critical state change
     within the diagnostic_path.

     For example, a double-free diagnostic might use the descriptions:
     - "first 'free' happens here"
     - "second 'free' happens here"
     for the pertinent events, whereas a use-after-free might use the
     descriptions:
     - "freed here"
     - "use after free here"
     Note how in both cases the first event is a "free": the best
     description to use depends on the diagnostic.  */

  virtual label_text describe_state_change (const evdesc::state_change &)
  {
    /* Default no-op implementation.  */
    return label_text ();
  }

  /* Precision-of-wording vfunc for describing an interprocedural call
     carrying critial state for the diagnostic, from caller to callee.

     For example a double-free diagnostic might use:
     - "passing freed pointer 'ptr' in call to 'deallocator' from 'test'"
     to make it clearer how the freed value moves from caller to
     callee.  */

  virtual label_text describe_call_with_state (const evdesc::call_with_state &)
  {
    /* Default no-op implementation.  */
    return label_text ();
  }

  /* Precision-of-wording vfunc for describing an interprocedural return
     within the diagnostic_path that carries critial state for the
     diagnostic, from callee back to caller.

     For example, a deref-of-unchecked-malloc diagnostic might use:
     - "returning possibly-NULL pointer to 'make_obj' from 'allocator'"
     to make it clearer how the unchecked value moves from callee
     back to caller.  */

  virtual label_text describe_return_of_state (const evdesc::return_of_state &)
  {
    /* Default no-op implementation.  */
    return label_text ();
  }

  /* Precision-of-wording vfunc for describing the final event within a
     diagnostic_path.

     For example a double-free diagnostic might use:
      - "second 'free' here; first 'free' was at (3)"
     and a use-after-free might use
      - "use after 'free' here; memory was freed at (2)".  */

  virtual label_text describe_final_event (const evdesc::final_event &)
  {
    /* Default no-op implementation.  */
    return label_text ();
  }

  /* End of precision-of-wording vfuncs.  */

  /* Vfunc for extending/overriding creation of the events for an
     exploded_edge that corresponds to a superedge, allowing for custom
     events to be created that are pertinent to a particular
     pending_diagnostic subclass.

     For example, the -Wanalyzer-stale-setjmp-buffer diagnostic adds a
     custom event showing when the pertinent stack frame is popped
     (and thus the point at which the jmp_buf becomes invalid).  */

  virtual bool maybe_add_custom_events_for_superedge (const exploded_edge &,
						      checker_path *)
  {
    return false;
  }

  /* Vfunc for determining that this pending_diagnostic supercedes OTHER,
     and that OTHER should therefore not be emitted.
     They have already been tested for being at the same stmt.  */

  virtual bool
  supercedes_p (const pending_diagnostic &other ATTRIBUTE_UNUSED) const
  {
    return false;
  }
};

/* A template to make it easier to make subclasses of pending_diagnostic.

   This uses the curiously-recurring template pattern, to implement
   pending_diagnostic::subclass_equal_p by casting and calling
   the operator==

   This assumes that BASE_OTHER has already been checked to have
   been of the same subclass (which pending_diagnostic::equal_p does).  */

template <class Subclass>
class pending_diagnostic_subclass : public pending_diagnostic
{
 public:
  bool subclass_equal_p (const pending_diagnostic &base_other) const
    FINAL OVERRIDE
  {
    const Subclass &other = (const Subclass &)base_other;
    return *(const Subclass*)this == other;
  }
};

} // namespace ana

#endif /* GCC_ANALYZER_PENDING_DIAGNOSTIC_H */
