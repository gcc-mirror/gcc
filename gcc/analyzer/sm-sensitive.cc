/* An experimental state machine, for tracking exposure of sensitive
   data (e.g. through logging).
   Copyright (C) 2019-2024 Free Software Foundation, Inc.
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
#include "analyzer/analyzer.h"
#include "diagnostic-event-id.h"
#include "analyzer/analyzer-logging.h"
#include "analyzer/sm.h"
#include "analyzer/pending-diagnostic.h"

#if ENABLE_ANALYZER

namespace ana {

namespace {

/* An experimental state machine, for tracking exposure of sensitive
   data (e.g. through logging).  */

class sensitive_state_machine : public state_machine
{
public:
  sensitive_state_machine (logger *logger);

  bool inherited_state_p () const final override { return true; }

  bool on_stmt (sm_context *sm_ctxt,
		const supernode *node,
		const gimple *stmt) const final override;

  bool can_purge_p (state_t s) const final override;

  /* State for "sensitive" data, such as a password.  */
  state_t m_sensitive;

  /* Stop state, for a value we don't want to track any more.  */
  state_t m_stop;

private:
  void warn_for_any_exposure (sm_context *sm_ctxt,
			      const supernode *node,
			      const gimple *stmt,
			      tree arg) const;
};

class exposure_through_output_file
  : public pending_diagnostic_subclass<exposure_through_output_file>
{
public:
  exposure_through_output_file (const sensitive_state_machine &sm, tree arg)
  : m_sm (sm), m_arg (arg)
  {}

  const char *get_kind () const final override
  {
    return "exposure_through_output_file";
  }

  bool operator== (const exposure_through_output_file &other) const
  {
    return same_tree_p (m_arg, other.m_arg);
  }

  int get_controlling_option () const final override
  {
    return OPT_Wanalyzer_exposure_through_output_file;
  }

  bool emit (diagnostic_emission_context &ctxt) final override
  {
    /* CWE-532: Information Exposure Through Log Files */
    ctxt.add_cwe (532);
    return ctxt.warn ("sensitive value %qE written to output file",
		      m_arg);
  }

  label_text describe_state_change (const evdesc::state_change &change)
    final override
  {
    if (change.m_new_state == m_sm.m_sensitive)
      {
	m_first_sensitive_event = change.m_event_id;
	return change.formatted_print ("sensitive value acquired here");
      }
    return label_text ();
  }

  diagnostic_event::meaning
  get_meaning_for_state_change (const evdesc::state_change &change)
    const final override
  {
    if (change.m_new_state == m_sm.m_sensitive)
      return diagnostic_event::meaning (diagnostic_event::VERB_acquire,
					diagnostic_event::NOUN_sensitive);
    return diagnostic_event::meaning ();
  }
  label_text describe_call_with_state (const evdesc::call_with_state &info)
    final override
  {
    if (info.m_state == m_sm.m_sensitive)
      return info.formatted_print
	("passing sensitive value %qE in call to %qE from %qE",
	 info.m_expr, info.m_callee_fndecl, info.m_caller_fndecl);
    return label_text ();
  }

  label_text describe_return_of_state (const evdesc::return_of_state &info)
    final override
  {
    if (info.m_state == m_sm.m_sensitive)
      return info.formatted_print ("returning sensitive value to %qE from %qE",
				   info.m_caller_fndecl, info.m_callee_fndecl);
    return label_text ();
  }

  label_text describe_final_event (const evdesc::final_event &ev) final override
  {
    if (m_first_sensitive_event.known_p ())
      return ev.formatted_print ("sensitive value %qE written to output file"
				 "; acquired at %@",
				 m_arg, &m_first_sensitive_event);
    else
      return ev.formatted_print ("sensitive value %qE written to output file",
				 m_arg);
  }

private:
  const sensitive_state_machine &m_sm;
  tree m_arg;
  diagnostic_event_id_t m_first_sensitive_event;
};

/* sensitive_state_machine's ctor.  */

sensitive_state_machine::sensitive_state_machine (logger *logger)
: state_machine ("sensitive", logger),
  m_sensitive (add_state ("sensitive")),
  m_stop (add_state ("stop"))
{
}

/* Warn about an exposure at NODE and STMT if ARG is in the "sensitive"
   state.  */

void
sensitive_state_machine::warn_for_any_exposure (sm_context *sm_ctxt,
						const supernode *node,
						const gimple *stmt,
						tree arg) const
{
  if (sm_ctxt->get_state (stmt, arg) == m_sensitive)
    {
      tree diag_arg = sm_ctxt->get_diagnostic_tree (arg);
      sm_ctxt->warn (node, stmt, arg,
		     make_unique<exposure_through_output_file> (*this,
								diag_arg));
    }
}

/* Implementation of state_machine::on_stmt vfunc for
   sensitive_state_machine.  */

bool
sensitive_state_machine::on_stmt (sm_context *sm_ctxt,
				  const supernode *node,
				  const gimple *stmt) const
{
  if (const gcall *call = dyn_cast <const gcall *> (stmt))
    if (tree callee_fndecl = sm_ctxt->get_fndecl_for_call (call))
      {
	if (is_named_call_p (callee_fndecl, "getpass", call, 1))
	  {
	    tree lhs = gimple_call_lhs (call);
	    if (lhs)
	      sm_ctxt->on_transition (node, stmt, lhs, m_start, m_sensitive);
	    return true;
	  }
	else if (is_named_call_p (callee_fndecl, "fprintf")
		 || is_named_call_p (callee_fndecl, "printf"))
	  {
	    /* Handle a match at any position in varargs.  */
	    for (unsigned idx = 1; idx < gimple_call_num_args (call); idx++)
	      {
		tree arg = gimple_call_arg (call, idx);
		warn_for_any_exposure (sm_ctxt, node, stmt, arg);
	      }
	    return true;
	  }
	else if (is_named_call_p (callee_fndecl, "fwrite", call, 4))
	  {
	    tree arg = gimple_call_arg (call, 0);
	    warn_for_any_exposure (sm_ctxt, node, stmt, arg);
	    return true;
	  }
	// TODO: ...etc.  This is just a proof-of-concept at this point.
      }
  return false;
}

bool
sensitive_state_machine::can_purge_p (state_t s ATTRIBUTE_UNUSED) const
{
  return true;
}

} // anonymous namespace

/* Internal interface to this file. */

state_machine *
make_sensitive_state_machine (logger *logger)
{
  return new sensitive_state_machine (logger);
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
