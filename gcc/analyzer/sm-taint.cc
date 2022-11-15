/* An experimental state machine, for tracking "taint": unsanitized uses
   of data potentially under an attacker's control.

   Copyright (C) 2019-2022 Free Software Foundation, Inc.
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
#define INCLUDE_MEMORY
#include "system.h"
#include "coretypes.h"
#include "make-unique.h"
#include "tree.h"
#include "function.h"
#include "basic-block.h"
#include "gimple.h"
#include "options.h"
#include "diagnostic-path.h"
#include "diagnostic-metadata.h"
#include "analyzer/analyzer.h"
#include "analyzer/analyzer-logging.h"
#include "gimple-iterator.h"
#include "ordered-hash-map.h"
#include "cgraph.h"
#include "cfg.h"
#include "digraph.h"
#include "stringpool.h"
#include "attribs.h"
#include "analyzer/supergraph.h"
#include "analyzer/call-string.h"
#include "analyzer/program-point.h"
#include "analyzer/store.h"
#include "analyzer/region-model.h"
#include "analyzer/sm.h"
#include "analyzer/program-state.h"
#include "analyzer/pending-diagnostic.h"
#include "analyzer/constraint-manager.h"

#if ENABLE_ANALYZER

namespace ana {

namespace {

/* An enum for describing tainted values.  */

enum bounds
{
  /* This tainted value has no upper or lower bound.  */
  BOUNDS_NONE,

  /* This tainted value has an upper bound but not lower bound.  */
  BOUNDS_UPPER,

  /* This tainted value has a lower bound but no upper bound.  */
  BOUNDS_LOWER
};

/* An experimental state machine, for tracking "taint": unsanitized uses
   of data potentially under an attacker's control.  */

class taint_state_machine : public state_machine
{
public:
  taint_state_machine (logger *logger);

  bool inherited_state_p () const final override { return true; }

  state_t alt_get_inherited_state (const sm_state_map &map,
				   const svalue *sval,
				   const extrinsic_state &ext_state)
    const final override;

  bool on_stmt (sm_context *sm_ctxt,
		const supernode *node,
		const gimple *stmt) const final override;

  void on_condition (sm_context *sm_ctxt,
		     const supernode *node,
		     const gimple *stmt,
		     const svalue *lhs,
		     enum tree_code op,
		     const svalue *rhs) const final override;
  void on_bounded_ranges (sm_context *sm_ctxt,
			  const supernode *node,
			  const gimple *stmt,
			  const svalue &sval,
			  const bounded_ranges &ranges) const final override;

  bool can_purge_p (state_t s) const final override;

  bool get_taint (state_t s, tree type, enum bounds *out) const;

  state_t combine_states (state_t s0, state_t s1) const;

private:
  void check_control_flow_arg_for_taint (sm_context *sm_ctxt,
					 const gimple *stmt,
					 tree expr) const;

  void check_for_tainted_size_arg (sm_context *sm_ctxt,
				   const supernode *node,
				   const gcall *call,
				   tree callee_fndecl) const;
  void check_for_tainted_divisor (sm_context *sm_ctxt,
				  const supernode *node,
				  const gassign *assign) const;

public:
  /* State for a "tainted" value: unsanitized data potentially under an
     attacker's control.  */
  state_t m_tainted;

  /* State for a "tainted" value that has a lower bound.  */
  state_t m_has_lb;

  /* State for a "tainted" value that has an upper bound.  */
  state_t m_has_ub;

  /* Stop state, for a value we don't want to track any more.  */
  state_t m_stop;

  /* Global state, for when the last condition had tainted arguments.  */
  state_t m_tainted_control_flow;
};

/* Class for diagnostics relating to taint_state_machine.  */

class taint_diagnostic : public pending_diagnostic
{
public:
  taint_diagnostic (const taint_state_machine &sm, tree arg,
		    enum bounds has_bounds)
  : m_sm (sm), m_arg (arg), m_has_bounds (has_bounds)
  {}

  bool subclass_equal_p (const pending_diagnostic &base_other) const override
  {
    const taint_diagnostic &other = (const taint_diagnostic &)base_other;
    return (same_tree_p (m_arg, other.m_arg)
	    && m_has_bounds == other.m_has_bounds);
  }

  label_text describe_state_change (const evdesc::state_change &change) override
  {
    if (change.m_new_state == m_sm.m_tainted)
      {
	if (change.m_origin)
	  return change.formatted_print ("%qE has an unchecked value here"
					 " (from %qE)",
					 change.m_expr, change.m_origin);
	else
	  return change.formatted_print ("%qE gets an unchecked value here",
					 change.m_expr);
      }
    else if (change.m_new_state == m_sm.m_has_lb)
      return change.formatted_print ("%qE has its lower bound checked here",
				     change.m_expr);
    else if (change.m_new_state == m_sm.m_has_ub)
      return change.formatted_print ("%qE has its upper bound checked here",
				     change.m_expr);
    return label_text ();
  }

  diagnostic_event::meaning
  get_meaning_for_state_change (const evdesc::state_change &change)
    const final override
  {
    if (change.m_new_state == m_sm.m_tainted)
      return diagnostic_event::meaning (diagnostic_event::VERB_acquire,
					diagnostic_event::NOUN_taint);
    return diagnostic_event::meaning ();
  }

protected:
  const taint_state_machine &m_sm;
  tree m_arg;
  enum bounds m_has_bounds;
};

/* Concrete taint_diagnostic subclass for reporting attacker-controlled
   array index.  */

class tainted_array_index : public taint_diagnostic
{
public:
  tainted_array_index (const taint_state_machine &sm, tree arg,
		       enum bounds has_bounds)
  : taint_diagnostic (sm, arg, has_bounds)
  {}

  const char *get_kind () const final override { return "tainted_array_index"; }

  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_tainted_array_index;
  }

  bool emit (rich_location *rich_loc) final override
  {
    diagnostic_metadata m;
    /* CWE-129: "Improper Validation of Array Index".  */
    m.add_cwe (129);
    if (m_arg)
      switch (m_has_bounds)
	{
	default:
	  gcc_unreachable ();
	case BOUNDS_NONE:
	  return warning_meta (rich_loc, m, get_controlling_option (),
			       "use of attacker-controlled value %qE"
			       " in array lookup without bounds checking",
			       m_arg);
	  break;
	case BOUNDS_UPPER:
	  return warning_meta (rich_loc, m, get_controlling_option (),
			       "use of attacker-controlled value %qE"
			       " in array lookup without checking for negative",
			       m_arg);
	  break;
	case BOUNDS_LOWER:
	  return warning_meta (rich_loc, m, get_controlling_option (),
			       "use of attacker-controlled value %qE"
			       " in array lookup without upper-bounds checking",
			       m_arg);
	  break;
	}
    else
      switch (m_has_bounds)
	{
	default:
	  gcc_unreachable ();
	case BOUNDS_NONE:
	  return warning_meta (rich_loc, m, get_controlling_option (),
			       "use of attacker-controlled value"
			       " in array lookup without bounds checking");
	  break;
	case BOUNDS_UPPER:
	  return warning_meta (rich_loc, m, get_controlling_option (),
			       "use of attacker-controlled value"
			       " in array lookup without checking for"
			       " negative");
	  break;
	case BOUNDS_LOWER:
	  return warning_meta (rich_loc, m, get_controlling_option (),
			       "use of attacker-controlled value"
			       " in array lookup without upper-bounds"
			       " checking");
	  break;
	}
  }

  label_text describe_final_event (const evdesc::final_event &ev) final override
  {
    if (m_arg)
      switch (m_has_bounds)
	{
	default:
	  gcc_unreachable ();
	case BOUNDS_NONE:
	  return ev.formatted_print
	    ("use of attacker-controlled value %qE in array lookup"
	     " without bounds checking",
	     m_arg);
	case BOUNDS_UPPER:
	  return ev.formatted_print
	    ("use of attacker-controlled value %qE"
	     " in array lookup without checking for negative",
	     m_arg);
	case BOUNDS_LOWER:
	  return ev.formatted_print
	    ("use of attacker-controlled value %qE"
	     " in array lookup without upper-bounds checking",
	     m_arg);
	}
    else
      switch (m_has_bounds)
	{
	default:
	  gcc_unreachable ();
	case BOUNDS_NONE:
	  return ev.formatted_print
	    ("use of attacker-controlled value in array lookup"
	     " without bounds checking");
	case BOUNDS_UPPER:
	  return ev.formatted_print
	    ("use of attacker-controlled value"
	     " in array lookup without checking for negative");
	case BOUNDS_LOWER:
	  return ev.formatted_print
	    ("use of attacker-controlled value"
	     " in array lookup without upper-bounds checking");
	}
  }
};

/* Concrete taint_diagnostic subclass for reporting attacker-controlled
   pointer offset.  */

class tainted_offset : public taint_diagnostic
{
public:
  tainted_offset (const taint_state_machine &sm, tree arg,
		       enum bounds has_bounds)
  : taint_diagnostic (sm, arg, has_bounds)
  {}

  const char *get_kind () const final override { return "tainted_offset"; }

  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_tainted_offset;
  }

  bool emit (rich_location *rich_loc) final override
  {
    diagnostic_metadata m;
    /* CWE-823: "Use of Out-of-range Pointer Offset".  */
    m.add_cwe (823);
    if (m_arg)
      switch (m_has_bounds)
	{
	default:
	  gcc_unreachable ();
	case BOUNDS_NONE:
	  return warning_meta (rich_loc, m, get_controlling_option (),
			       "use of attacker-controlled value %qE as offset"
			       " without bounds checking",
			       m_arg);
	  break;
	case BOUNDS_UPPER:
	  return warning_meta (rich_loc, m, get_controlling_option (),
			       "use of attacker-controlled value %qE as offset"
			       " without lower-bounds checking",
			       m_arg);
	  break;
	case BOUNDS_LOWER:
	  return warning_meta (rich_loc, m, get_controlling_option (),
			       "use of attacker-controlled value %qE as offset"
			       " without upper-bounds checking",
			       m_arg);
	  break;
	}
    else
      switch (m_has_bounds)
	{
	default:
	  gcc_unreachable ();
	case BOUNDS_NONE:
	  return warning_meta (rich_loc, m, get_controlling_option (),
			       "use of attacker-controlled value as offset"
			       " without bounds checking");
	  break;
	case BOUNDS_UPPER:
	  return warning_meta (rich_loc, m, get_controlling_option (),
			       "use of attacker-controlled value as offset"
			       " without lower-bounds checking");
	  break;
	case BOUNDS_LOWER:
	  return warning_meta (rich_loc, m, get_controlling_option (),
			       "use of attacker-controlled value as offset"
			       " without upper-bounds checking");
	  break;
	}
  }

  label_text describe_final_event (const evdesc::final_event &ev) final override
  {
    if (m_arg)
      switch (m_has_bounds)
	{
	default:
	  gcc_unreachable ();
	case BOUNDS_NONE:
	  return ev.formatted_print ("use of attacker-controlled value %qE"
				     " as offset without bounds checking",
				     m_arg);
	case BOUNDS_UPPER:
	  return ev.formatted_print ("use of attacker-controlled value %qE"
				     " as offset without lower-bounds checking",
				     m_arg);
	case BOUNDS_LOWER:
	  return ev.formatted_print ("use of attacker-controlled value %qE"
				     " as offset without upper-bounds checking",
				     m_arg);
	}
    else
      switch (m_has_bounds)
	{
	default:
	  gcc_unreachable ();
	case BOUNDS_NONE:
	  return ev.formatted_print ("use of attacker-controlled value"
				     " as offset without bounds checking");
	case BOUNDS_UPPER:
	  return ev.formatted_print ("use of attacker-controlled value"
				     " as offset without lower-bounds"
				     " checking");
	case BOUNDS_LOWER:
	  return ev.formatted_print ("use of attacker-controlled value"
				     " as offset without upper-bounds"
				     " checking");
	}
  }
};

/* Concrete taint_diagnostic subclass for reporting attacker-controlled
   size.  */

class tainted_size : public taint_diagnostic
{
public:
  tainted_size (const taint_state_machine &sm, tree arg,
		enum bounds has_bounds)
  : taint_diagnostic (sm, arg, has_bounds)
  {}

  const char *get_kind () const override { return "tainted_size"; }

  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_tainted_size;
  }

  bool emit (rich_location *rich_loc) override
  {
    /* "CWE-129: Improper Validation of Array Index".  */
    diagnostic_metadata m;
    m.add_cwe (129);
    if (m_arg)
      switch (m_has_bounds)
	{
	default:
	  gcc_unreachable ();
	case BOUNDS_NONE:
	  return warning_meta (rich_loc, m, get_controlling_option (),
			       "use of attacker-controlled value %qE as size"
			       " without bounds checking",
			       m_arg);
	  break;
	case BOUNDS_UPPER:
	  return warning_meta (rich_loc, m, get_controlling_option (),
			       "use of attacker-controlled value %qE as size"
			       " without lower-bounds checking",
			       m_arg);
	  break;
	case BOUNDS_LOWER:
	  return warning_meta (rich_loc, m, get_controlling_option (),
			       "use of attacker-controlled value %qE as size"
			       " without upper-bounds checking",
			       m_arg);
	  break;
	}
    else
      switch (m_has_bounds)
	{
	default:
	  gcc_unreachable ();
	case BOUNDS_NONE:
	  return warning_meta (rich_loc, m, get_controlling_option (),
			       "use of attacker-controlled value as size"
			       " without bounds checking");
	  break;
	case BOUNDS_UPPER:
	  return warning_meta (rich_loc, m, get_controlling_option (),
			       "use of attacker-controlled value as size"
			       " without lower-bounds checking");
	  break;
	case BOUNDS_LOWER:
	  return warning_meta (rich_loc, m, get_controlling_option (),
			       "use of attacker-controlled value as size"
			       " without upper-bounds checking");
	  break;
	}
  }

  label_text describe_final_event (const evdesc::final_event &ev) final override
  {
    if (m_arg)
      switch (m_has_bounds)
	{
	default:
	  gcc_unreachable ();
	case BOUNDS_NONE:
	  return ev.formatted_print ("use of attacker-controlled value %qE"
				     " as size without bounds checking",
				     m_arg);
	case BOUNDS_UPPER:
	  return ev.formatted_print ("use of attacker-controlled value %qE"
				     " as size without lower-bounds checking",
				     m_arg);
	case BOUNDS_LOWER:
	  return ev.formatted_print ("use of attacker-controlled value %qE"
				     " as size without upper-bounds checking",
				     m_arg);
	}
    else
      switch (m_has_bounds)
	{
	default:
	  gcc_unreachable ();
	case BOUNDS_NONE:
	  return ev.formatted_print ("use of attacker-controlled value"
				     " as size without bounds checking");
	case BOUNDS_UPPER:
	  return ev.formatted_print ("use of attacker-controlled value"
				     " as size without lower-bounds checking");
	case BOUNDS_LOWER:
	  return ev.formatted_print ("use of attacker-controlled value"
				     " as size without upper-bounds checking");
	}
  }
};

/* Subclass of tainted_size for reporting on tainted size values
   passed to an external function annotated with attribute "access".  */

class tainted_access_attrib_size : public tainted_size
{
public:
  tainted_access_attrib_size (const taint_state_machine &sm, tree arg,
			      enum bounds has_bounds, tree callee_fndecl,
			      unsigned size_argno, const char *access_str)
  : tainted_size (sm, arg, has_bounds),
    m_callee_fndecl (callee_fndecl),
    m_size_argno (size_argno), m_access_str (access_str)
  {
  }

  const char *get_kind () const override
  {
    return "tainted_access_attrib_size";
  }

  bool emit (rich_location *rich_loc) final override
  {
    bool warned = tainted_size::emit (rich_loc);
    if (warned)
      {
	inform (DECL_SOURCE_LOCATION (m_callee_fndecl),
		"parameter %i of %qD marked as a size via attribute %qs",
		m_size_argno + 1, m_callee_fndecl, m_access_str);
      }
    return warned;
  }

private:
  tree m_callee_fndecl;
  unsigned m_size_argno;
  const char *m_access_str;
};

/* Concrete taint_diagnostic subclass for reporting attacker-controlled
   divisor (so that an attacker can trigger a divide by zero).  */

class tainted_divisor : public taint_diagnostic
{
public:
  tainted_divisor (const taint_state_machine &sm, tree arg,
		   enum bounds has_bounds)
  : taint_diagnostic (sm, arg, has_bounds)
  {}

  const char *get_kind () const final override { return "tainted_divisor"; }

  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_tainted_divisor;
  }

  bool emit (rich_location *rich_loc) final override
  {
    diagnostic_metadata m;
    /* CWE-369: "Divide By Zero".  */
    m.add_cwe (369);
    if (m_arg)
      return warning_meta (rich_loc, m, get_controlling_option (),
			   "use of attacker-controlled value %qE as divisor"
			   " without checking for zero",
			   m_arg);
    else
      return warning_meta (rich_loc, m, get_controlling_option (),
			   "use of attacker-controlled value as divisor"
			   " without checking for zero");
  }

  label_text describe_final_event (const evdesc::final_event &ev) final override
  {
    if (m_arg)
      return ev.formatted_print
	("use of attacker-controlled value %qE as divisor"
	 " without checking for zero",
	 m_arg);
    else
      return ev.formatted_print
	("use of attacker-controlled value as divisor"
	 " without checking for zero");
  }
};

/* Concrete taint_diagnostic subclass for reporting attacker-controlled
   size of a dynamic allocation.  */

class tainted_allocation_size : public taint_diagnostic
{
public:
  tainted_allocation_size (const taint_state_machine &sm, tree arg,
			   enum bounds has_bounds, enum memory_space mem_space)
  : taint_diagnostic (sm, arg, has_bounds),
    m_mem_space (mem_space)
  {
  }

  const char *get_kind () const final override
  {
    return "tainted_allocation_size";
  }

  bool subclass_equal_p (const pending_diagnostic &base_other) const override
  {
    if (!taint_diagnostic::subclass_equal_p (base_other))
      return false;
    const tainted_allocation_size &other
      = (const tainted_allocation_size &)base_other;
    return m_mem_space == other.m_mem_space;
  }

  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_tainted_allocation_size;
  }

  bool emit (rich_location *rich_loc) final override
  {
    diagnostic_metadata m;
    /* "CWE-789: Memory Allocation with Excessive Size Value".  */
    m.add_cwe (789);

    bool warned;
    if (m_arg)
      switch (m_has_bounds)
	{
	default:
	  gcc_unreachable ();
	case BOUNDS_NONE:
	  warned = warning_meta (rich_loc, m, get_controlling_option (),
				 "use of attacker-controlled value %qE as"
				 " allocation size without bounds checking",
				 m_arg);
	  break;
	case BOUNDS_UPPER:
	  warned = warning_meta (rich_loc, m, get_controlling_option (),
				 "use of attacker-controlled value %qE as"
				 " allocation size without"
				 " lower-bounds checking",
				 m_arg);
	  break;
	case BOUNDS_LOWER:
	  warned = warning_meta (rich_loc, m, get_controlling_option (),
				 "use of attacker-controlled value %qE as"
				 " allocation size without"
				 " upper-bounds checking",
				 m_arg);
	  break;
	}
    else
      switch (m_has_bounds)
	{
	default:
	  gcc_unreachable ();
	case BOUNDS_NONE:
	  warned = warning_meta (rich_loc, m, get_controlling_option (),
				 "use of attacker-controlled value as"
				 " allocation size without bounds"
				 " checking");
	  break;
	case BOUNDS_UPPER:
	  warned = warning_meta (rich_loc, m, get_controlling_option (),
				 "use of attacker-controlled value as"
				 " allocation size without"
				 " lower-bounds checking");
	  break;
	case BOUNDS_LOWER:
	  warned = warning_meta (rich_loc, m, get_controlling_option (),
				 "use of attacker-controlled value as"
				 " allocation size without"
				 " upper-bounds checking");
	  break;
	}
    if (warned)
      {
	location_t loc = rich_loc->get_loc ();
	switch (m_mem_space)
	  {
	  default:
	    break;
	  case MEMSPACE_STACK:
	    inform (loc, "stack-based allocation");
	    break;
	  case MEMSPACE_HEAP:
	    inform (loc, "heap-based allocation");
	    break;
	  }
      }
    return warned;
  }

  label_text describe_final_event (const evdesc::final_event &ev) final override
  {
    if (m_arg)
      switch (m_has_bounds)
	{
	default:
	  gcc_unreachable ();
	case BOUNDS_NONE:
	  return ev.formatted_print
	    ("use of attacker-controlled value %qE as allocation size"
	     " without bounds checking",
	     m_arg);
	case BOUNDS_UPPER:
	  return ev.formatted_print
	    ("use of attacker-controlled value %qE as allocation size"
	     " without lower-bounds checking",
	     m_arg);
	case BOUNDS_LOWER:
	  return ev.formatted_print
	    ("use of attacker-controlled value %qE as allocation size"
	     " without upper-bounds checking",
	     m_arg);
	}
    else
      switch (m_has_bounds)
	{
	default:
	  gcc_unreachable ();
	case BOUNDS_NONE:
	  return ev.formatted_print
	    ("use of attacker-controlled value as allocation size"
	     " without bounds checking");
	case BOUNDS_UPPER:
	  return ev.formatted_print
	    ("use of attacker-controlled value as allocation size"
	     " without lower-bounds checking");
	case BOUNDS_LOWER:
	  return ev.formatted_print
	    ("use of attacker-controlled value as allocation size"
	     " without upper-bounds checking");
	}
  }

private:
  enum memory_space m_mem_space;
};

/* Concrete taint_diagnostic subclass for reporting attacker-controlled
   value being used as part of the condition of an assertion.  */

class tainted_assertion : public taint_diagnostic
{
public:
  tainted_assertion (const taint_state_machine &sm, tree arg,
		     tree assert_failure_fndecl)
  : taint_diagnostic (sm, arg, BOUNDS_NONE),
    m_assert_failure_fndecl (assert_failure_fndecl)
  {
    gcc_assert (m_assert_failure_fndecl);
  }

  const char *get_kind () const final override
  {
    return "tainted_assertion";
  }

  bool subclass_equal_p (const pending_diagnostic &base_other) const override
  {
    if (!taint_diagnostic::subclass_equal_p (base_other))
      return false;
    const tainted_assertion &other
      = (const tainted_assertion &)base_other;
    return m_assert_failure_fndecl == other.m_assert_failure_fndecl;
  }

  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_tainted_assertion;
  }

  bool emit (rich_location *rich_loc) final override
  {
    diagnostic_metadata m;
    /* "CWE-617: Reachable Assertion".  */
    m.add_cwe (617);

    return warning_meta (rich_loc, m, get_controlling_option (),
			 "use of attacked-controlled value in"
			 " condition for assertion");
  }

  location_t fixup_location (location_t loc,
			     bool primary) const final override
  {
    if (primary)
      /* For the primary location we want to avoid being in e.g. the
	 <assert.h> system header, since this would suppress the
	 diagnostic.  */
      return expansion_point_location_if_in_system_header (loc);
    else if (in_system_header_at (loc))
      /* For events, we want to show the implemenation of the assert
	 macro when we're describing them.  */
      return linemap_resolve_location (line_table, loc,
				       LRK_SPELLING_LOCATION,
				       NULL);
    else
      return pending_diagnostic::fixup_location (loc, primary);
  }

  label_text describe_state_change (const evdesc::state_change &change) override
  {
    if (change.m_new_state == m_sm.m_tainted_control_flow)
      return change.formatted_print
	("use of attacker-controlled value for control flow");
    return taint_diagnostic::describe_state_change (change);
  }

  label_text describe_final_event (const evdesc::final_event &ev) final override
  {
    if (mention_noreturn_attribute_p ())
      return ev.formatted_print
	("treating %qE as an assertion failure handler"
	 " due to %<__attribute__((__noreturn__))%>",
	 m_assert_failure_fndecl);
    else
      return ev.formatted_print
	("treating %qE as an assertion failure handler",
	 m_assert_failure_fndecl);
  }

private:
  bool mention_noreturn_attribute_p () const
  {
    if (fndecl_built_in_p (m_assert_failure_fndecl, BUILT_IN_UNREACHABLE))
      return false;
    return true;
  }

  tree m_assert_failure_fndecl;
};

/* taint_state_machine's ctor.  */

taint_state_machine::taint_state_machine (logger *logger)
: state_machine ("taint", logger)
{
  m_tainted = add_state ("tainted");
  m_has_lb = add_state ("has_lb");
  m_has_ub = add_state ("has_ub");
  m_stop = add_state ("stop");
  m_tainted_control_flow = add_state ("tainted-control-flow");
}

state_machine::state_t
taint_state_machine::alt_get_inherited_state (const sm_state_map &map,
					      const svalue *sval,
					      const extrinsic_state &ext_state)
  const
{
  switch (sval->get_kind ())
    {
    default:
      break;
    case SK_UNARYOP:
      {
	const unaryop_svalue *unaryop_sval
	  = as_a <const unaryop_svalue *> (sval);
	enum tree_code op = unaryop_sval->get_op ();
	const svalue *arg = unaryop_sval->get_arg ();
	switch (op)
	  {
	  case NOP_EXPR:
	    {
	      state_t arg_state = map.get_state (arg, ext_state);
	      return arg_state;
	    }
	  default:
	    break;
	  }
      }
      break;
    case SK_BINOP:
      {
	const binop_svalue *binop_sval = as_a <const binop_svalue *> (sval);
	enum tree_code op = binop_sval->get_op ();
	const svalue *arg0 = binop_sval->get_arg0 ();
	const svalue *arg1 = binop_sval->get_arg1 ();
	switch (op)
	  {
	  default:
	    break;

	  case EQ_EXPR:
	  case GE_EXPR:
	  case LE_EXPR:
	  case NE_EXPR:
	  case GT_EXPR:
	  case LT_EXPR:
	  case UNORDERED_EXPR:
	  case ORDERED_EXPR:
	  case PLUS_EXPR:
	  case MINUS_EXPR:
	  case MULT_EXPR:
	  case POINTER_PLUS_EXPR:
	  case TRUNC_DIV_EXPR:
	  case TRUNC_MOD_EXPR:
	    {
	      state_t arg0_state = map.get_state (arg0, ext_state);
	      state_t arg1_state = map.get_state (arg1, ext_state);
	      return combine_states (arg0_state, arg1_state);
	    }
	    break;

	  case BIT_AND_EXPR:
	  case RSHIFT_EXPR:
	    return NULL;
	  }
      }
      break;
    }
  return NULL;
}

/* Return true iff FNDECL should be considered to be an assertion failure
   handler by -Wanalyzer-tainted-assertion. */

static bool
is_assertion_failure_handler_p (tree fndecl)
{
  // i.e. "noreturn"
  if (TREE_THIS_VOLATILE (fndecl))
    return true;

  return false;
}

/* Implementation of state_machine::on_stmt vfunc for taint_state_machine.  */

bool
taint_state_machine::on_stmt (sm_context *sm_ctxt,
			       const supernode *node,
			       const gimple *stmt) const
{
  if (const gcall *call = dyn_cast <const gcall *> (stmt))
    if (tree callee_fndecl = sm_ctxt->get_fndecl_for_call (call))
      {
	if (is_named_call_p (callee_fndecl, "fread", call, 4))
	  {
	    tree arg = gimple_call_arg (call, 0);

	    sm_ctxt->on_transition (node, stmt, arg, m_start, m_tainted);

	    /* Dereference an ADDR_EXPR.  */
	    // TODO: should the engine do this?
	    if (TREE_CODE (arg) == ADDR_EXPR)
	      sm_ctxt->on_transition (node, stmt, TREE_OPERAND (arg, 0),
				      m_start, m_tainted);
	    return true;
	  }

	/* External function with "access" attribute. */
	if (sm_ctxt->unknown_side_effects_p ())
	  check_for_tainted_size_arg (sm_ctxt, node, call, callee_fndecl);

	if (is_assertion_failure_handler_p (callee_fndecl)
	    && sm_ctxt->get_global_state () == m_tainted_control_flow)
	  {
	    sm_ctxt->warn (node, call, NULL_TREE,
			   make_unique<tainted_assertion> (*this, NULL_TREE,
							   callee_fndecl));
	  }
      }
  // TODO: ...etc; many other sources of untrusted data

  if (const gassign *assign = dyn_cast <const gassign *> (stmt))
    {
      enum tree_code op = gimple_assign_rhs_code (assign);

      switch (op)
	{
	default:
	  break;
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
	  check_for_tainted_divisor (sm_ctxt, node, assign);
	  break;
	}
    }

  if (const gcond *cond = dyn_cast <const gcond *> (stmt))
    {
      /* Reset the state of "tainted-control-flow" before each
	 control flow statement, so that only the last one before
	 an assertion-failure-handler counts.  */
      sm_ctxt->set_global_state (m_start);
      check_control_flow_arg_for_taint (sm_ctxt, cond, gimple_cond_lhs (cond));
      check_control_flow_arg_for_taint (sm_ctxt, cond, gimple_cond_rhs (cond));
    }

  if (const gswitch *switch_ = dyn_cast <const gswitch *> (stmt))
    {
      /* Reset the state of "tainted-control-flow" before each
	 control flow statement, so that only the last one before
	 an assertion-failure-handler counts.  */
      sm_ctxt->set_global_state (m_start);
      check_control_flow_arg_for_taint (sm_ctxt, switch_,
					gimple_switch_index (switch_));
    }

  return false;
}

/* If EXPR is tainted, mark this execution path with the
   "tainted-control-flow" global state, in case we're about
   to call an assertion-failure-handler.  */

void
taint_state_machine::check_control_flow_arg_for_taint (sm_context *sm_ctxt,
						       const gimple *stmt,
						       tree expr) const
{
  const region_model *old_model = sm_ctxt->get_old_region_model ();
  const svalue *sval = old_model->get_rvalue (expr, NULL);
  state_t state = sm_ctxt->get_state (stmt, sval);
  enum bounds b;
  if (get_taint (state, TREE_TYPE (expr), &b))
    sm_ctxt->set_global_state (m_tainted_control_flow);
}

/* Implementation of state_machine::on_condition vfunc for taint_state_machine.
   Potentially transition state 'tainted' to 'has_ub' or 'has_lb',
   and states 'has_ub' and 'has_lb' to 'stop'.  */

void
taint_state_machine::on_condition (sm_context *sm_ctxt,
				   const supernode *node,
				   const gimple *stmt,
				   const svalue *lhs,
				   enum tree_code op,
				   const svalue *rhs) const
{
  if (stmt == NULL)
    return;

  // TODO
  switch (op)
    {
      //case NE_EXPR:
      //case EQ_EXPR:
    case GE_EXPR:
    case GT_EXPR:
      {
	/* (LHS >= RHS) or (LHS > RHS)
	   LHS gains a lower bound
	   RHS gains an upper bound.  */
	sm_ctxt->on_transition (node, stmt, lhs, m_tainted,
				m_has_lb);
	sm_ctxt->on_transition (node, stmt, lhs, m_has_ub,
				m_stop);
	sm_ctxt->on_transition (node, stmt, rhs, m_tainted,
				m_has_ub);
	sm_ctxt->on_transition (node, stmt, rhs, m_has_lb,
				m_stop);
      }
      break;
    case LE_EXPR:
    case LT_EXPR:
      {
	/* Detect where build_range_check has optimized
	   (c>=low) && (c<=high)
	   into
	   (c-low>=0) && (c-low<=high-low)
	   and thus into:
	   (unsigned)(c - low) <= (unsigned)(high-low).  */
	if (const binop_svalue *binop_sval
	      = lhs->dyn_cast_binop_svalue ())
	  {
	    const svalue *inner_lhs = binop_sval->get_arg0 ();
	    enum tree_code inner_op = binop_sval->get_op ();
	    const svalue *inner_rhs = binop_sval->get_arg1 ();
	    if (const svalue *before_cast = inner_lhs->maybe_undo_cast ())
	      inner_lhs = before_cast;
	    if (tree outer_rhs_cst = rhs->maybe_get_constant ())
	      if (tree inner_rhs_cst = inner_rhs->maybe_get_constant ())
		if (inner_op == PLUS_EXPR
		    && TREE_CODE (inner_rhs_cst) == INTEGER_CST
		    && TREE_CODE (outer_rhs_cst) == INTEGER_CST
		    && TYPE_UNSIGNED (TREE_TYPE (inner_rhs_cst))
		    && TYPE_UNSIGNED (TREE_TYPE (outer_rhs_cst)))
		  {
		    /* We have
		       (unsigned)(INNER_LHS + CST_A) </<= UNSIGNED_CST_B
		       and thus an optimized test of INNER_LHS (before any
		       cast to unsigned) against a range.
		       Transition any of the tainted states to the stop state.
		       We have to special-case this here rather than in
		       region_model::on_condition since we can't apply
		       both conditions simultaneously (we'd have a transition
		       from the old state to has_lb, then a transition from
		       the old state *again* to has_ub).  */
		    state_t old_state
		      = sm_ctxt->get_state (stmt, inner_lhs);
		    if (old_state == m_tainted
			|| old_state == m_has_lb
			|| old_state == m_has_ub)
		      sm_ctxt->set_next_state (stmt, inner_lhs, m_stop);
		    return;
		  }
	  }

	/* (LHS <= RHS) or (LHS < RHS)
	   LHS gains an upper bound
	   RHS gains a lower bound.  */
	sm_ctxt->on_transition (node, stmt, lhs, m_tainted,
				m_has_ub);
	sm_ctxt->on_transition (node, stmt, lhs, m_has_lb,
				m_stop);
	sm_ctxt->on_transition (node, stmt, rhs, m_tainted,
				m_has_lb);
	sm_ctxt->on_transition (node, stmt, rhs, m_has_ub,
				m_stop);
      }
      break;
    default:
      break;
    }
}

/* Implementation of state_machine::on_bounded_ranges vfunc for
   taint_state_machine, for handling switch statement cases.
   Potentially transition state 'tainted' to 'has_ub' or 'has_lb',
   and states 'has_ub' and 'has_lb' to 'stop'.  */

void
taint_state_machine::on_bounded_ranges (sm_context *sm_ctxt,
					const supernode *,
					const gimple *stmt,
					const svalue &sval,
					const bounded_ranges &ranges) const
{
  gcc_assert (!ranges.empty_p ());
  gcc_assert (ranges.get_count () > 0);

  /* We have one or more ranges; this could be a "default:", or one or
     more single or range cases.

     Look at the overall endpoints to see if the ranges impose any lower
     bounds or upper bounds beyond those of the underlying numeric type.  */

  tree lowest_bound = ranges.get_range (0).m_lower;
  tree highest_bound = ranges.get_range (ranges.get_count () - 1).m_upper;
  gcc_assert (lowest_bound);
  gcc_assert (highest_bound);

  bool ranges_have_lb
    = (lowest_bound != TYPE_MIN_VALUE (TREE_TYPE (lowest_bound)));
  bool ranges_have_ub
    = (highest_bound != TYPE_MAX_VALUE (TREE_TYPE (highest_bound)));

  if (!ranges_have_lb && !ranges_have_ub)
    return;

  /* We have new bounds from the ranges; combine them with any
     existing bounds on SVAL.  */
  state_t old_state = sm_ctxt->get_state (stmt, &sval);
  if (old_state == m_tainted)
    {
      if (ranges_have_lb && ranges_have_ub)
	sm_ctxt->set_next_state (stmt, &sval, m_stop);
      else if (ranges_have_lb)
	sm_ctxt->set_next_state (stmt, &sval, m_has_lb);
      else if (ranges_have_ub)
	sm_ctxt->set_next_state (stmt, &sval, m_has_ub);
    }
  else if (old_state == m_has_ub && ranges_have_lb)
    sm_ctxt->set_next_state (stmt, &sval, m_stop);
  else if (old_state == m_has_lb && ranges_have_ub)
    sm_ctxt->set_next_state (stmt, &sval, m_stop);
}

bool
taint_state_machine::can_purge_p (state_t s ATTRIBUTE_UNUSED) const
{
  return true;
}

/* If STATE is a tainted state, write the bounds to *OUT and return true.
   Otherwise return false.
   Use the signedness of TYPE to determine if "has_ub" is tainted.  */

bool
taint_state_machine::get_taint (state_t state, tree type,
				enum bounds *out) const
{
  /* Unsigned types have an implicit lower bound.  */
  bool is_unsigned = false;
  if (type)
    if (INTEGRAL_TYPE_P (type))
      is_unsigned = TYPE_UNSIGNED (type);

  /* Can't use a switch as the states are non-const.  */
  if (state == m_tainted)
    {
      *out = is_unsigned ? BOUNDS_LOWER : BOUNDS_NONE;
      return true;
    }
  else if (state == m_has_lb)
    {
      *out = BOUNDS_LOWER;
      return true;
    }
  else if (state == m_has_ub && !is_unsigned)
    {
      /* Missing lower bound.  */
      *out = BOUNDS_UPPER;
      return true;
    }
  return false;
}

/* Find the most tainted state of S0 and S1.  */

state_machine::state_t
taint_state_machine::combine_states (state_t s0, state_t s1) const
{
  gcc_assert (s0);
  gcc_assert (s1);
  if (s0 == s1)
    return s0;
  if (s0 == m_tainted || s1 == m_tainted)
    return m_tainted;
  if (s0 == m_start)
    return s1;
  if (s1 == m_start)
    return s0;
  if (s0 == m_stop)
    return s1;
  if (s1 == m_stop)
    return s0;
  /* The only remaining combinations are one of has_ub and has_lb
     (in either order).  */
  gcc_assert ((s0 == m_has_lb && s1 == m_has_ub)
	      || (s0 == m_has_ub && s1 == m_has_lb));
  return m_tainted;
}

/* Check for calls to external functions marked with
   __attribute__((access)) with a size-index: complain about
   tainted values passed as a size to such a function.  */

void
taint_state_machine::check_for_tainted_size_arg (sm_context *sm_ctxt,
						 const supernode *node,
						 const gcall *call,
						 tree callee_fndecl) const
{
  tree fntype = TREE_TYPE (callee_fndecl);
  if (!fntype)
    return;

  if (!TYPE_ATTRIBUTES (fntype))
    return;

  /* Initialize a map of attribute access specifications for arguments
     to the function call.  */
  rdwr_map rdwr_idx;
  init_attr_rdwr_indices (&rdwr_idx, TYPE_ATTRIBUTES (fntype));

  unsigned argno = 0;

  for (tree iter = TYPE_ARG_TYPES (fntype); iter;
       iter = TREE_CHAIN (iter), ++argno)
    {
      const attr_access* access = rdwr_idx.get (argno);
      if (!access)
	continue;

      /* Ignore any duplicate entry in the map for the size argument.  */
      if (access->ptrarg != argno)
	continue;

      if (access->sizarg == UINT_MAX)
	continue;

      tree size_arg = gimple_call_arg (call, access->sizarg);

      state_t state = sm_ctxt->get_state (call, size_arg);
      enum bounds b;
      if (get_taint (state, TREE_TYPE (size_arg), &b))
	{
	  const char* const access_str =
	    TREE_STRING_POINTER (access->to_external_string ());
	  tree diag_size = sm_ctxt->get_diagnostic_tree (size_arg);
	  sm_ctxt->warn (node, call, size_arg,
			 make_unique<tainted_access_attrib_size>
			   (*this, diag_size, b,
			    callee_fndecl,
			    access->sizarg,
			    access_str));
	}
    }
}

/* Complain if ASSIGN (a division operation) has a tainted divisor
   that could be zero.  */

void
taint_state_machine::check_for_tainted_divisor (sm_context *sm_ctxt,
						const supernode *node,
						const gassign *assign) const
{
  const region_model *old_model = sm_ctxt->get_old_region_model ();
  if (!old_model)
    return;

  tree divisor_expr = gimple_assign_rhs2 (assign);;
  const svalue *divisor_sval = old_model->get_rvalue (divisor_expr, NULL);

  state_t state = sm_ctxt->get_state (assign, divisor_sval);
  enum bounds b;
  if (get_taint (state, TREE_TYPE (divisor_expr), &b))
    {
      const svalue *zero_sval
	= old_model->get_manager ()->get_or_create_int_cst
	    (TREE_TYPE (divisor_expr), 0);
      tristate ts
	= old_model->eval_condition (divisor_sval, NE_EXPR, zero_sval);
      if (ts.is_true ())
	/* The divisor is known to not equal 0: don't warn.  */
	return;

      tree diag_divisor = sm_ctxt->get_diagnostic_tree (divisor_expr);
      sm_ctxt->warn (node, assign, divisor_expr,
		     make_unique <tainted_divisor> (*this, diag_divisor, b));
      sm_ctxt->set_next_state (assign, divisor_sval, m_stop);
    }
}

} // anonymous namespace

/* Internal interface to this file. */

state_machine *
make_taint_state_machine (logger *logger)
{
  return new taint_state_machine (logger);
}

/* Complain to CTXT if accessing REG leads could lead to arbitrary
   memory access under an attacker's control (due to taint).  */

void
region_model::check_region_for_taint (const region *reg,
				      enum access_direction,
				      region_model_context *ctxt) const
{
  gcc_assert (reg);
  gcc_assert (ctxt);

  LOG_SCOPE (ctxt->get_logger ());

  sm_state_map *smap;
  const state_machine *sm;
  unsigned sm_idx;
  if (!ctxt->get_taint_map (&smap, &sm, &sm_idx))
    return;

  gcc_assert (smap);
  gcc_assert (sm);

  const taint_state_machine &taint_sm = (const taint_state_machine &)*sm;

  const extrinsic_state *ext_state = ctxt->get_ext_state ();
  if (!ext_state)
    return;

  const region *iter_region = reg;
  while (iter_region)
    {
      switch (iter_region->get_kind ())
	{
	default:
	  break;

	case RK_ELEMENT:
	  {
	    const element_region *element_reg
	      = (const element_region *)iter_region;
	    const svalue *index = element_reg->get_index ();
	    const state_machine::state_t
	      state = smap->get_state (index, *ext_state);
	    gcc_assert (state);
	    enum bounds b;
	    if (taint_sm.get_taint (state, index->get_type (), &b))
	    {
	      tree arg = get_representative_tree (index);
	      ctxt->warn (make_unique<tainted_array_index> (taint_sm, arg, b));
	    }
	  }
	  break;

	case RK_OFFSET:
	  {
	    const offset_region *offset_reg
	      = (const offset_region *)iter_region;
	    const svalue *offset = offset_reg->get_byte_offset ();
	    const state_machine::state_t
	      state = smap->get_state (offset, *ext_state);
	    gcc_assert (state);
	    /* Handle implicit cast to sizetype.  */
	    tree effective_type = offset->get_type ();
	    if (const svalue *cast = offset->maybe_undo_cast ())
	      if (cast->get_type ())
		effective_type = cast->get_type ();
	    enum bounds b;
	    if (taint_sm.get_taint (state, effective_type, &b))
	      {
		tree arg = get_representative_tree (offset);
		ctxt->warn (make_unique<tainted_offset> (taint_sm, arg, b));
	      }
	  }
	  break;

	case RK_CAST:
	  {
	    const cast_region *cast_reg
	      = as_a <const cast_region *> (iter_region);
	    iter_region = cast_reg->get_original_region ();
	    continue;
	  }

	case RK_SIZED:
	  {
	    const sized_region *sized_reg
	      = (const sized_region *)iter_region;
	    const svalue *size_sval = sized_reg->get_byte_size_sval (m_mgr);
	    const state_machine::state_t
	      state = smap->get_state (size_sval, *ext_state);
	    gcc_assert (state);
	    enum bounds b;
	    if (taint_sm.get_taint (state, size_sval->get_type (), &b))
	      {
		tree arg = get_representative_tree (size_sval);
		ctxt->warn (make_unique<tainted_size> (taint_sm, arg, b));
	      }
	  }
	  break;
	}

      iter_region = iter_region->get_parent_region ();
    }
}

/* Complain to CTXT about a tainted allocation size if SIZE_IN_BYTES is
   under an attacker's control (due to taint), where the allocation
   is happening within MEM_SPACE.  */

void
region_model::check_dynamic_size_for_taint (enum memory_space mem_space,
					    const svalue *size_in_bytes,
					    region_model_context *ctxt) const
{
  gcc_assert (size_in_bytes);
  gcc_assert (ctxt);

  LOG_SCOPE (ctxt->get_logger ());

  sm_state_map *smap;
  const state_machine *sm;
  unsigned sm_idx;
  if (!ctxt->get_taint_map (&smap, &sm, &sm_idx))
    return;

  gcc_assert (smap);
  gcc_assert (sm);

  const taint_state_machine &taint_sm = (const taint_state_machine &)*sm;

  const extrinsic_state *ext_state = ctxt->get_ext_state ();
  if (!ext_state)
    return;

  const state_machine::state_t
    state = smap->get_state (size_in_bytes, *ext_state);
  gcc_assert (state);
  enum bounds b;
  if (taint_sm.get_taint (state, size_in_bytes->get_type (), &b))
    {
      tree arg = get_representative_tree (size_in_bytes);
      ctxt->warn (make_unique<tainted_allocation_size>
		    (taint_sm, arg, b, mem_space));
    }
}

/* Mark SVAL as TAINTED.  CTXT must be non-NULL.  */

void
region_model::mark_as_tainted (const svalue *sval,
			       region_model_context *ctxt)
{
  gcc_assert (sval);
  gcc_assert (ctxt);

  sm_state_map *smap;
  const state_machine *sm;
  unsigned sm_idx;
  if (!ctxt->get_taint_map (&smap, &sm, &sm_idx))
    return;

  gcc_assert (smap);
  gcc_assert (sm);

  const taint_state_machine &taint_sm = (const taint_state_machine &)*sm;

  const extrinsic_state *ext_state = ctxt->get_ext_state ();
  if (!ext_state)
    return;

  smap->set_state (this, sval, taint_sm.m_tainted, NULL, *ext_state);
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
