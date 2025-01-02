/* Modeling API uses and misuses via state machines.
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

#include "config.h"
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "function.h"
#include "basic-block.h"
#include "gimple.h"
#include "options.h"
#include "function.h"
#include "diagnostic-core.h"
#include "pretty-print.h"
#include "diagnostic.h"
#include "tree-diagnostic.h"
#include "analyzer/analyzer.h"
#include "analyzer/analyzer-logging.h"
#include "analyzer/sm.h"
#include "analyzer/call-string.h"
#include "analyzer/program-point.h"
#include "analyzer/store.h"
#include "analyzer/svalue.h"
#include "analyzer/program-state.h"
#include "analyzer/pending-diagnostic.h"
#include "make-unique.h"

#if ENABLE_ANALYZER

namespace ana {

/* Return true if VAR has pointer or reference type.  */

bool
any_pointer_p (tree var)
{
  return POINTER_TYPE_P (TREE_TYPE (var));
}

/* Return true if SVAL has pointer or reference type.  */

bool
any_pointer_p (const svalue *sval)
{
  if (!sval->get_type ())
    return false;
  return POINTER_TYPE_P (sval->get_type ());
}

/* class state_machine::state.  */

/* Base implementation of dump_to_pp vfunc.  */

void
state_machine::state::dump_to_pp (pretty_printer *pp) const
{
  pp_string (pp, m_name);
}

/* Return a new json::string describing the state.  */

std::unique_ptr<json::value>
state_machine::state::to_json () const
{
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  dump_to_pp (&pp);
  return ::make_unique<json::string> (pp_formatted_text (&pp));
}

/* class state_machine.  */

/* state_machine's ctor.  */

state_machine::state_machine (const char *name, logger *logger)
: log_user (logger), m_name (name), m_next_state_id (0),
  m_start (add_state ("start"))
{
}

/* Add a state with name NAME to this state_machine.
   The string is required to outlive the state_machine.

   Return the state_t for the new state.  */

state_machine::state_t
state_machine::add_state (const char *name)
{
  state *s = new state (name, alloc_state_id ());
  m_states.safe_push (s);
  return s;
}

/* Get the state with name NAME, which must exist.
   This is purely intended for use in selftests.  */

state_machine::state_t
state_machine::get_state_by_name (const char *name) const
{
  unsigned i;
  state *s;
  FOR_EACH_VEC_ELT (m_states, i, s)
    if (!strcmp (name, s->get_name ()))
      return s;
  /* Name not found.  */
  gcc_unreachable ();
}

/* Base implementation of state_machine::on_leak.  */

std::unique_ptr<pending_diagnostic>
state_machine::on_leak (tree var ATTRIBUTE_UNUSED) const
{
  return NULL;
}

/* Dump a multiline representation of this state machine to PP.  */

void
state_machine::dump_to_pp (pretty_printer *pp) const
{
  unsigned i;
  state *s;
  FOR_EACH_VEC_ELT (m_states, i, s)
    {
      pp_printf (pp, "  state %i: ", i);
      s->dump_to_pp (pp);
      pp_newline (pp);
    }
}

/* Return a new json::object of the form
   {"name" : str,
    "states" : [str]}.  */

std::unique_ptr<json::object>
state_machine::to_json () const
{
  auto sm_obj = ::make_unique<json::object> ();

  sm_obj->set_string ("name", m_name);
  {
    auto states_arr = ::make_unique<json::array> ();
    unsigned i;
    state *s;
    FOR_EACH_VEC_ELT (m_states, i, s)
      states_arr->append (s->to_json ());
    sm_obj->set ("states", std::move (states_arr));
  }

  return sm_obj;
}

/* class sm_context.  */

const region_model *
sm_context::get_old_region_model () const
{
  if (const program_state *old_state = get_old_program_state ())
    return old_state->m_region_model;
  else
    return NULL;
}

/* Create instances of the various state machines, each using LOGGER,
   and populate OUT with them.  */

void
make_checkers (auto_delete_vec <state_machine> &out, logger *logger)
{
  out.safe_push (make_malloc_state_machine (logger));
  out.safe_push (make_fileptr_state_machine (logger));
  out.safe_push (make_fd_state_machine (logger));
  out.safe_push (make_taint_state_machine (logger));
  out.safe_push (make_sensitive_state_machine (logger));
  out.safe_push (make_signal_state_machine (logger));
  out.safe_push (make_va_list_state_machine (logger));

  /* We only attempt to run the pattern tests if it might have been manually
     enabled (for DejaGnu purposes).  */
  if (flag_analyzer_checker)
    out.safe_push (make_pattern_test_state_machine (logger));

  if (flag_analyzer_checker)
    {
      unsigned read_index, write_index;
      state_machine **sm;

      /* TODO: this leaks the machines
	 Would be nice to log the things that were removed.  */
      VEC_ORDERED_REMOVE_IF (out, read_index, write_index, sm,
			     0 != strcmp (flag_analyzer_checker,
					  (*sm)->get_name ()));
    }
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
