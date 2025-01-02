/* Classes for analyzer diagnostics.
   Copyright (C) 2019-2025 Free Software Foundation, Inc.
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

#include "diagnostic-metadata.h"
#include "diagnostic-path.h"
#include "analyzer/sm.h"

namespace ana {

/* A bundle of information about things that are of interest to a
   pending_diagnostic.

   For now, merely the set of regions that are pertinent to the
   diagnostic, so that we can notify the user about when they
   were created.  */

struct interesting_t
{
  void add_region_creation (const region *reg);

  void dump_to_pp (pretty_printer *pp, bool simple) const;

  auto_vec<const region *> m_region_creation;
};

/* Various bundles of information used for generating more precise
   messages for events within a diagnostic_path, for passing to the
   various "describe_*" vfuncs of pending_diagnostic.  See those
   for more information.  */

namespace evdesc {

/* For use by pending_diagnostic::describe_state_change.  */

struct state_change
{
  state_change (tree expr,
		tree origin,
		state_machine::state_t old_state,
		state_machine::state_t new_state,
		diagnostic_event_id_t event_id,
		const state_change_event &event)
  : m_expr (expr), m_origin (origin),
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

struct call_with_state
{
  call_with_state (tree caller_fndecl, tree callee_fndecl,
		   tree expr, state_machine::state_t state)
  : m_caller_fndecl (caller_fndecl),
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

struct return_of_state
{
  return_of_state (tree caller_fndecl, tree callee_fndecl,
		   state_machine::state_t state)
  : m_caller_fndecl (caller_fndecl),
    m_callee_fndecl (callee_fndecl),
    m_state (state)
  {
  }

  tree m_caller_fndecl;
  tree m_callee_fndecl;
  state_machine::state_t m_state;
};

/* For use by pending_diagnostic::describe_final_event.  */

struct final_event
{
  final_event (tree expr, state_machine::state_t state,
	       const warning_event &event)
  : m_expr (expr), m_state (state), m_event (event)
  {}

  tree m_expr;
  state_machine::state_t m_state;
  const warning_event &m_event;
};

} /* end of namespace evdesc */

/*  A bundle of information for use by implementations of the
    pending_diagnostic::emit vfunc.

    The rich_location will have already been populated with a
    diagnostic_path.  */

class diagnostic_emission_context
{
public:
  diagnostic_emission_context (const saved_diagnostic &sd,
			       rich_location &rich_loc,
			       diagnostic_metadata &metadata,
			       logger *logger)
  : m_sd (sd),
    m_rich_loc (rich_loc),
    m_metadata (metadata),
    m_logger (logger)
  {
  }

  const pending_diagnostic &get_pending_diagnostic () const;

  bool warn (const char *, ...) ATTRIBUTE_GCC_DIAG (2,3);
  void inform (const char *, ...) ATTRIBUTE_GCC_DIAG (2,3);

  location_t get_location () const { return m_rich_loc.get_loc (); }
  logger *get_logger () const { return m_logger; }

  void add_cwe (int cwe) { m_metadata.add_cwe (cwe); }
  void add_rule (const diagnostic_metadata::rule &r)
  {
    m_metadata.add_rule (r);
  }

private:
  const saved_diagnostic &m_sd;
  rich_location &m_rich_loc;
  diagnostic_metadata &m_metadata;
  logger *m_logger;
};

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

  /* Vfunc to get the command-line option used when emitting the diagnostic,
     or zero if there is none.
     Used by diagnostic_manager for early rejection of diagnostics (to avoid
     having to generate feasible execution paths for them).  */
  virtual int get_controlling_option () const = 0;

  /* Vfunc to give the diagnostic the chance to terminate the execution
     path being explored.  By default, don't terminate the path.  */
  virtual bool terminate_path_p () const { return false; }

  /* Vfunc for emitting the diagnostic.
     Return true if a diagnostic is actually emitted.  */
  virtual bool emit (diagnostic_emission_context &) = 0;

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

  /* Vfunc for fixing up locations, e.g. to avoid unwinding
     inside specific macros.  PRIMARY is true for the primary location
     for the diagnostic, and FALSE for events in their paths.  */
  virtual location_t fixup_location (location_t loc, bool primary) const;

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
     description to use depends on the diagnostic.

     Print the description to PP and return true,
     or do nothing and return false.  */

  virtual bool describe_state_change (pretty_printer &,
				      const evdesc::state_change &)
  {
    /* Default no-op implementation.  */
    return false;
  }

  /* Vfunc for implementing diagnostic_event::get_meaning for
     state_change_event.  */
  virtual diagnostic_event::meaning
  get_meaning_for_state_change (const evdesc::state_change &) const
  {
    /* Default no-op implementation.  */
    return diagnostic_event::meaning ();
  }

  /* Precision-of-wording vfunc for describing an interprocedural call
     carrying critial state for the diagnostic, from caller to callee.

     For example a double-free diagnostic might use:
     - "passing freed pointer 'ptr' in call to 'deallocator' from 'test'"
     to make it clearer how the freed value moves from caller to
     callee.  */

  virtual bool describe_call_with_state (pretty_printer &,
					 const evdesc::call_with_state &)
  {
    /* Default no-op implementation.  */
    return false;
  }

  /* Precision-of-wording vfunc for describing an interprocedural return
     within the diagnostic_path that carries critial state for the
     diagnostic, from callee back to caller.

     For example, a deref-of-unchecked-malloc diagnostic might use:
     - "returning possibly-NULL pointer to 'make_obj' from 'allocator'"
     to make it clearer how the unchecked value moves from callee
     back to caller.  */

  virtual bool describe_return_of_state (pretty_printer &,
					 const evdesc::return_of_state &)
  {
    /* Default no-op implementation.  */
    return false;
  }

  /* Precision-of-wording vfunc for describing the final event within a
     diagnostic_path.

     For example a double-free diagnostic might use:
      - "second 'free' here; first 'free' was at (3)"
     and a use-after-free might use
      - "use after 'free' here; memory was freed at (2)".  */

  virtual bool describe_final_event (pretty_printer &,
				     const evdesc::final_event &)
  {
    /* Default no-op implementation.  */
    return false;
  }

  /* End of precision-of-wording vfuncs.  */

  /* Vfunc for adding a function_entry_event to a checker_path, so that e.g.
     the infinite recursion diagnostic can add a custom event subclass
     that annotates recursively entering a function.  */

  virtual void
  add_function_entry_event (const exploded_edge &eedge,
			    checker_path *emission_path);

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

  /* Vfunc for adding a call_event to a checker_path, so that e.g.
     the varargs diagnostics can add a custom event subclass that annotates
     the variadic arguments.  */
  virtual void add_call_event (const exploded_edge &,
			       checker_path *);

  /* Vfunc for adding any events for the creation of regions identified
     by the mark_interesting_stuff vfunc.
     See the comment for class region_creation_event.  */
  virtual void add_region_creation_events (const region *reg,
					   tree capacity,
					   const event_loc_info &loc_info,
					   checker_path &emission_path);

  /* Vfunc for adding the final warning_event to a checker_path, so that e.g.
     the infinite recursion diagnostic can have its diagnostic appear at
     the callsite, but the final event in the path be at the entrypoint
     of the called function.  */
  virtual void add_final_event (const state_machine *sm,
				const exploded_node *enode,
				const event_loc_info &loc_info,
				tree var, state_machine::state_t state,
				checker_path *emission_path);

  /* Vfunc for determining that this pending_diagnostic supercedes OTHER,
     and that OTHER should therefore not be emitted.
     They have already been tested for being at the same stmt.  */

  virtual bool
  supercedes_p (const pending_diagnostic &other ATTRIBUTE_UNUSED) const
  {
    return false;
  }

  /* Vfunc for registering additional information of interest to this
     diagnostic.  */

  virtual void mark_interesting_stuff (interesting_t *)
  {
    /* Default no-op implementation.  */
  }

  /* Vfunc to give diagnostic subclasses the opportunity to reject diagnostics
     by imposing their own additional feasibility checks on the path to a
     given feasible_node.  */
  virtual bool check_valid_fpath_p (const feasible_node &,
				    const gimple *) const
  {
    /* Default implementation: accept this path.  */
    return true;
  }

  /* Vfunc for use in SARIF output to give pending_diagnostic subclasses
     the opportunity to add diagnostic-specific properties to the SARIF
     "result" object for the diagnostic.
     This is intended for use when debugging a diagnostic.  */
  virtual void maybe_add_sarif_properties (sarif_object &/*result_obj*/) const
  {
    /* Default no-op implementation.  */
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
    final override
  {
    const Subclass &other = (const Subclass &)base_other;
    return *(const Subclass*)this == other;
  }
};

/* An abstract base class for capturing additional notes that are to be
   emitted with a diagnostic.  */

class pending_note
{
public:
  virtual ~pending_note () {}

  /* Hand-coded RTTI: get an ID for the subclass.  */
  virtual const char *get_kind () const = 0;

  /* Vfunc for emitting the note.  */
  virtual void emit () const = 0;

  bool equal_p (const pending_note &other) const
  {
    /* Check for pointer equality on the IDs from get_kind.  */
    if (get_kind () != other.get_kind ())
      return false;
    /* Call vfunc now we know they have the same ID: */
    return subclass_equal_p (other);
  }

  /* A vfunc for testing for equality, where we've already
     checked they have the same ID.  See pending_note_subclass
     below for a convenience subclass for implementing this.  */
  virtual bool subclass_equal_p (const pending_note &other) const = 0;
};

/* Analogous to pending_diagnostic_subclass, but for pending_note.  */

template <class Subclass>
class pending_note_subclass : public pending_note
{
 public:
  bool subclass_equal_p (const pending_note &base_other) const
    final override
  {
    const Subclass &other = (const Subclass &)base_other;
    return *(const Subclass*)this == other;
  }
};

} // namespace ana

#endif /* GCC_ANALYZER_PENDING_DIAGNOSTIC_H */
