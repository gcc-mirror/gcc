/* Classes for representing locations within the program.
   Copyright (C) 2019-2026 Free Software Foundation, Inc.
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

#include "analyzer/common.h"

#include "diagnostics/event-id.h"
#include "gcc-rich-location.h"
#include "gimple-pretty-print.h"
#include "sbitmap.h"
#include "selftest.h"
#include "shortest-paths.h"

#include "analyzer/analyzer-logging.h"
#include "analyzer/call-string.h"
#include "analyzer/supergraph.h"
#include "analyzer/program-point.h"
#include "analyzer/store.h"
#include "analyzer/region-model.h"
#include "analyzer/sm.h"
#include "analyzer/program-state.h"
#include "analyzer/pending-diagnostic.h"
#include "analyzer/diagnostic-manager.h"
#include "analyzer/exploded-graph.h"
#include "analyzer/analysis-plan.h"
#include "analyzer/inlining-iterator.h"

#if ENABLE_ANALYZER

namespace ana {

/* A subclass of diagnostics::context for use by
   program_point::print_source_line.  */

class debug_diagnostic_context : public diagnostics::context
{
public:
  debug_diagnostic_context ()
  {
    diagnostic_initialize (this, 0);
    auto &source_printing_opts = get_source_printing_options ();
    source_printing_opts.show_line_numbers_p = true;
    source_printing_opts.enabled = true;
  }
  ~debug_diagnostic_context ()
  {
    diagnostic_finish (this);
  }
};

/* class program_point.  */

/* Print the source line (if any) for this program_point to PP.  */

void
program_point::print_source_line (pretty_printer *pp) const
{
  if (!useful_location_p (get_location ()))
    return;
  debug_diagnostic_context tmp_dc;
  gcc_rich_location richloc (get_location ());
  diagnostics::source_print_policy source_policy (tmp_dc);
  gcc_assert (pp);
  source_policy.print (*pp, richloc, diagnostics::kind::error, nullptr);
  pp_string (pp, pp_formatted_text (tmp_dc.get_reference_printer ()));
}

/* Print this program_point to PP.  */

void
program_point::print (pretty_printer *pp, const format &f) const
{
  pp_string (pp, "callstring: ");
  m_call_string->print (pp);
  f.spacer (pp);

  if (m_snode)
    {
      pp_printf (pp, "sn: %i", m_snode->m_id);
      if (f.m_newlines)
	{
	  pp_newline (pp);
	  print_source_line (pp);
	}
    }
  else
    pp_string (pp, "origin");
}

/* Dump this point to stderr.  */

DEBUG_FUNCTION void
program_point::dump () const
{
  tree_dump_pretty_printer pp (stderr);
  print (&pp, format (true));
}

/* Return a new json::object of the form
   {"snode_idx" : int (optional), the index of the supernode,
    "call_string": object for the call_string}.  */

std::unique_ptr<json::object>
program_point::to_json () const
{
  auto point_obj = std::make_unique<json::object> ();

  if (get_supernode ())
    point_obj->set_integer ("snode_idx", get_supernode ()->m_id);
  point_obj->set ("call_string", m_call_string->to_json ());

  return point_obj;
}

/* Pop the topmost call from the current callstack.  */
void
program_point::pop_from_call_stack ()
{
  m_call_string = m_call_string->get_parent ();
  gcc_assert (m_call_string);
}

/* Generate a hash value for this program_point.  */

hashval_t
program_point::hash () const
{
  inchash::hash hstate;
  hstate.add_ptr (m_snode);
  hstate.add_ptr (m_call_string);
  return hstate.end ();
}

/* Get the function * at DEPTH within the call stack.  */

function *
program_point::get_function_at_depth (unsigned depth) const
{
  gcc_assert (depth <= m_call_string->length ());
  if (depth == m_call_string->length ())
    return m_snode->get_function ();
  else
    return get_call_string ()[depth].get_caller_function ();
}

/* Assert that this object is sane.  */

void
program_point::validate () const
{
  /* Skip this in a release build.  */
#if !CHECKING_P
  return;
#endif

  m_call_string->validate ();
  /* The "callee" of the final entry in the callstring should be the
     function of the m_function_point.  */
  if (m_call_string->length () > 0)
    gcc_assert
      ((*m_call_string)[m_call_string->length () - 1].get_callee_function ()
       == get_function ());
}

/* class program_point.  */

program_point
program_point::origin (const region_model_manager &mgr)
{
  return program_point (nullptr,
			mgr.get_empty_call_string ());
}

program_point
program_point::from_function_entry (const region_model_manager &mgr,
				    const supergraph &sg,
				    const function &fun)
{
  return program_point (sg.get_node_for_function_entry (fun),
			mgr.get_empty_call_string ());
}

/* Return true iff POINT_A and POINT_B share the same function and
   call_string, both directly, and when attempting to undo inlining
   information.  */

bool
program_point::effectively_intraprocedural_p (const program_point &point_a,
					      const program_point &point_b)
{
  /* First, compare without considering inlining info.  */
  if (point_a.get_function ()
      != point_b.get_function ())
    return false;
  if (&point_a.get_call_string ()
      != &point_b.get_call_string ())
    return false;

  /* Consider inlining info; they must have originally come from
     the same function and have been inlined in the same way.  */
  location_t loc_a = point_a.get_location ();
  location_t loc_b = point_b.get_location ();
  inlining_iterator iter_a (loc_a);
  inlining_iterator iter_b (loc_b);
  while (!(iter_a.done_p () || iter_b.done_p ()))
    {
      if (iter_a.done_p () || iter_b.done_p ())
	return false;

      if (iter_a.get_fndecl () != iter_b.get_fndecl ())
	return false;
      if (iter_a.get_callsite () != iter_b.get_callsite ())
	return false;
      if (iter_a.get_block () != iter_b.get_block ())
	return false;

      iter_a.next ();
      iter_b.next ();
    }

  return true;
}

#if CHECKING_P

namespace selftest {

/* Verify that program_point::operator== works as expected.  */

static void
test_program_point_equality ()
{
  region_model_manager mgr;

  const supernode *snode = nullptr;

  const call_string &cs = mgr.get_empty_call_string ();

  program_point a = program_point (snode, cs);
  program_point b = program_point (snode, cs);
  ASSERT_EQ (a, b);
  // TODO: verify with non-empty callstrings, with different snodes
}

/* Run all of the selftests within this file.  */

void
analyzer_program_point_cc_tests ()
{
  test_program_point_equality ();
}

} // namespace selftest

#endif /* CHECKING_P */

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
