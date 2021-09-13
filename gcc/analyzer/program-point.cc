/* Classes for representing locations within the program.
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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "gimple-pretty-print.h"
#include "gcc-rich-location.h"
#include "json.h"
#include "analyzer/call-string.h"
#include "ordered-hash-map.h"
#include "options.h"
#include "cgraph.h"
#include "function.h"
#include "cfg.h"
#include "basic-block.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "digraph.h"
#include "analyzer/analyzer.h"
#include "analyzer/analyzer-logging.h"
#include "analyzer/supergraph.h"
#include "analyzer/program-point.h"
#include "sbitmap.h"
#include "bitmap.h"
#include "tristate.h"
#include "selftest.h"
#include "analyzer/store.h"
#include "analyzer/region-model.h"
#include "analyzer/sm.h"
#include "analyzer/program-state.h"
#include "alloc-pool.h"
#include "fibonacci_heap.h"
#include "diagnostic-event-id.h"
#include "analyzer/pending-diagnostic.h"
#include "analyzer/diagnostic-manager.h"
#include "shortest-paths.h"
#include "analyzer/exploded-graph.h"
#include "analyzer/analysis-plan.h"

#if ENABLE_ANALYZER

namespace ana {

/* Get a string for PK.  */

const char *
point_kind_to_string (enum point_kind pk)
{
  switch (pk)
    {
    default:
      gcc_unreachable ();
    case PK_ORIGIN:
      return "PK_ORIGIN";
    case PK_BEFORE_SUPERNODE:
      return "PK_BEFORE_SUPERNODE";
    case PK_BEFORE_STMT:
      return "PK_BEFORE_STMT";
    case PK_AFTER_SUPERNODE:
      return "PK_AFTER_SUPERNODE";
    case PK_EMPTY:
      return "PK_EMPTY";
    case PK_DELETED:
      return "PK_DELETED";
    }
}

/* class function_point.  */

function_point::function_point (const supernode *supernode,
				const superedge *from_edge,
				unsigned stmt_idx,
				enum point_kind kind)
: m_supernode (supernode), m_from_edge (from_edge),
  m_stmt_idx (stmt_idx), m_kind (kind)
{
  if (from_edge)
    {
      gcc_checking_assert (m_kind == PK_BEFORE_SUPERNODE);
      gcc_checking_assert (from_edge->get_kind () == SUPEREDGE_CFG_EDGE);
    }
  if (stmt_idx)
    gcc_checking_assert (m_kind == PK_BEFORE_STMT);
}

/* Print this function_point to PP.  */

void
function_point::print (pretty_printer *pp, const format &f) const
{
  switch (get_kind ())
    {
    default:
      gcc_unreachable ();

    case PK_ORIGIN:
      pp_printf (pp, "origin");
      break;

    case PK_BEFORE_SUPERNODE:
      {
	if (m_from_edge)
	  {
	    if (basic_block bb = m_from_edge->m_src->m_bb)
	      pp_printf (pp, "before SN: %i (from SN: %i (bb: %i))",
			 m_supernode->m_index, m_from_edge->m_src->m_index,
			 bb->index);
	    else
	      pp_printf (pp, "before SN: %i (from SN: %i)",
			 m_supernode->m_index, m_from_edge->m_src->m_index);
	  }
	else
	  pp_printf (pp, "before SN: %i (NULL from-edge)",
		     m_supernode->m_index);
	f.spacer (pp);
	for (gphi_iterator gpi
	       = const_cast<supernode *>(get_supernode ())->start_phis ();
	     !gsi_end_p (gpi); gsi_next (&gpi))
	  {
	    const gphi *phi = gpi.phi ();
	    pp_gimple_stmt_1 (pp, phi, 0, (dump_flags_t)0);
	  }
      }
      break;

    case PK_BEFORE_STMT:
      pp_printf (pp, "before (SN: %i stmt: %i): ", m_supernode->m_index,
		 m_stmt_idx);
      f.spacer (pp);
      pp_gimple_stmt_1 (pp, get_stmt (), 0, (dump_flags_t)0);
      if (f.m_newlines)
	{
	  pp_newline (pp);
	  print_source_line (pp);
	}
      break;

    case PK_AFTER_SUPERNODE:
      pp_printf (pp, "after SN: %i", m_supernode->m_index);
      break;
    }
}

/* Generate a hash value for this function_point.  */

hashval_t
function_point::hash () const
{
  inchash::hash hstate;
  if (m_supernode)
    hstate.add_int (m_supernode->m_index);
  hstate.add_ptr (m_from_edge);
  hstate.add_int (m_stmt_idx);
  hstate.add_int (m_kind);
  return hstate.end ();
}

/* Get the function at this point, if any.  */

function *
function_point::get_function () const
{
  if (m_supernode)
    return m_supernode->m_fun;
  else
    return NULL;
}

/* Get the gimple stmt for this function_point, if any.  */

const gimple *
function_point::get_stmt () const
{
  if (m_kind == PK_BEFORE_STMT)
    return m_supernode->m_stmts[m_stmt_idx];
  else if (m_kind == PK_AFTER_SUPERNODE)
    return m_supernode->get_last_stmt ();
  else
    return NULL;
}

/* Get a location for this function_point, if any.  */

location_t
function_point::get_location () const
{
  const gimple *stmt = get_stmt ();
  if (stmt)
    return stmt->location;
  if (m_kind == PK_BEFORE_SUPERNODE)
    return m_supernode->get_start_location ();
  else if (m_kind == PK_AFTER_SUPERNODE)
    return m_supernode->get_end_location ();
  else
    return UNKNOWN_LOCATION;
}

/* Create a function_point representing the entrypoint of function FUN.  */

function_point
function_point::from_function_entry (const supergraph &sg, function *fun)
{
  return before_supernode (sg.get_node_for_function_entry (fun), NULL);
}

/* Create a function_point representing entering supernode SUPERNODE,
   having reached it via FROM_EDGE (which could be NULL).  */

function_point
function_point::before_supernode (const supernode *supernode,
				  const superedge *from_edge)
{
  if (from_edge && from_edge->get_kind () != SUPEREDGE_CFG_EDGE)
    from_edge = NULL;
  return function_point (supernode, from_edge, 0, PK_BEFORE_SUPERNODE);
}

/* A subclass of diagnostic_context for use by
   program_point::print_source_line.  */

class debug_diagnostic_context : public diagnostic_context
{
public:
  debug_diagnostic_context ()
  {
    diagnostic_initialize (this, 0);
    show_line_numbers_p = true;
    show_caret = true;
  }
  ~debug_diagnostic_context ()
  {
    diagnostic_finish (this);
  }
};

/* Print the source line (if any) for this function_point to PP.  */

void
function_point::print_source_line (pretty_printer *pp) const
{
  const gimple *stmt = get_stmt ();
  if (!stmt)
    return;
  // TODO: monospace font
  debug_diagnostic_context tmp_dc;
  gcc_rich_location richloc (stmt->location);
  diagnostic_show_locus (&tmp_dc, &richloc, DK_ERROR);
  pp_string (pp, pp_formatted_text (tmp_dc.printer));
}

/* class program_point.  */

/* Print this program_point to PP.  */

void
program_point::print (pretty_printer *pp, const format &f) const
{
  pp_string (pp, "callstring: ");
  m_call_string.print (pp);
  f.spacer (pp);

  m_function_point.print (pp, f);
}

/* Dump this point to stderr.  */

DEBUG_FUNCTION void
program_point::dump () const
{
  pretty_printer pp;
  pp_show_color (&pp) = pp_show_color (global_dc->printer);
  pp.buffer->stream = stderr;
  print (&pp, format (true));
  pp_flush (&pp);
}

/* Return a new json::object of the form
   {"kind"  : str,
    "snode_idx" : int (optional), the index of the supernode,
    "from_edge_snode_idx" : int (only for kind=='PK_BEFORE_SUPERNODE'),
    "stmt_idx": int (only for kind=='PK_BEFORE_STMT',
    "call_string": object for the call_string}.  */

json::object *
program_point::to_json () const
{
  json::object *point_obj = new json::object ();

  point_obj->set ("kind",
		  new json::string (point_kind_to_string (get_kind ())));

  if (get_supernode ())
    point_obj->set ("snode_idx",
		    new json::integer_number (get_supernode ()->m_index));

  switch (get_kind ())
    {
    default: break;
    case PK_BEFORE_SUPERNODE:
      if (const superedge *sedge = get_from_edge ())
	point_obj->set ("from_edge_snode_idx",
			new json::integer_number (sedge->m_src->m_index));
      break;
    case PK_BEFORE_STMT:
      point_obj->set ("stmt_idx", new json::integer_number (get_stmt_idx ()));
      break;
    }

  point_obj->set ("call_string", m_call_string.to_json ());

  return point_obj;
}

/* Update the callstack to represent a call from caller to callee.

   Generally used to push a custom call to a perticular program point 
   where we don't have a superedge representing the call.  */
void
program_point::push_to_call_stack (const supernode *caller,
				   const supernode *callee)
{
  m_call_string.push_call (callee, caller);
}

/* Pop the topmost call from the current callstack.  */
void
program_point::pop_from_call_stack ()
{
  m_call_string.pop ();
}

/* Generate a hash value for this program_point.  */

hashval_t
program_point::hash () const
{
  inchash::hash hstate;
  hstate.merge_hash (m_function_point.hash ());
  hstate.merge_hash (m_call_string.hash ());
  return hstate.end ();
}

/* Get the function * at DEPTH within the call stack.  */

function *
program_point::get_function_at_depth (unsigned depth) const
{
  gcc_assert (depth <= m_call_string.length ());
  if (depth == m_call_string.length ())
    return m_function_point.get_function ();
  else
    return m_call_string[depth].get_caller_function ();
}

/* Assert that this object is sane.  */

void
program_point::validate () const
{
  /* Skip this in a release build.  */
#if !CHECKING_P
  return;
#endif

  m_call_string.validate ();
  /* The "callee" of the final entry in the callstring should be the
     function of the m_function_point.  */
  if (m_call_string.length () > 0)
    gcc_assert (m_call_string[m_call_string.length () - 1].get_callee_function ()
		== get_function ());
}

/* Check to see if SUCC is a valid edge to take (ensuring that we have
   interprocedurally valid paths in the exploded graph, and enforcing
   recursion limits).

   Update the call string if SUCC is a call or a return.

   Return true if SUCC can be taken, or false otherwise.

   This is the "point" half of exploded_node::on_edge.  */

bool
program_point::on_edge (exploded_graph &eg,
			const superedge *succ)
{
  logger * const logger = eg.get_logger ();
  LOG_FUNC (logger);
  switch (succ->m_kind)
    {
    case SUPEREDGE_CFG_EDGE:
      {
	const cfg_superedge *cfg_sedge = as_a <const cfg_superedge *> (succ);

	/* Reject abnormal edges; we special-case setjmp/longjmp.  */
	if (cfg_sedge->get_flags () & EDGE_ABNORMAL)
	  return false;
      }
      break;

    case SUPEREDGE_CALL:
      {
	const call_superedge *call_sedge = as_a <const call_superedge *> (succ);

	if (eg.get_analysis_plan ().use_summary_p (call_sedge->m_cedge))
	  {
	    if (logger)
	      logger->log ("rejecting call edge: using summary instead");
	    return false;
	  }

	/* Add the callsite to the call string.  */
	m_call_string.push_call (eg.get_supergraph (), call_sedge);

	/* Impose a maximum recursion depth and don't analyze paths
	   that exceed it further.
	   This is something of a blunt workaround, but it only
	   applies to recursion (and mutual recursion), not to
	   general call stacks.  */
	if (m_call_string.calc_recursion_depth ()
	    > param_analyzer_max_recursion_depth)
	  {
	    if (logger)
	      logger->log ("rejecting call edge: recursion limit exceeded");
	    // TODO: issue a sorry for this?
	    return false;
	  }
      }
      break;

    case SUPEREDGE_RETURN:
      {
	/* Require that we return to the call site in the call string.  */
	if (m_call_string.empty_p ())
	  {
	    if (logger)
	      logger->log ("rejecting return edge: empty call string");
	    return false;
	  }
	const call_string::element_t top_of_stack = m_call_string.pop ();
	call_string::element_t current_call_string_element (succ->m_dest,
							    succ->m_src);
	if (top_of_stack != current_call_string_element)
	  {
	    if (logger)
	      logger->log ("rejecting return edge: return to wrong callsite");
	    return false;
	  }
      }
      break;

    case SUPEREDGE_INTRAPROCEDURAL_CALL:
      {
	const callgraph_superedge *cg_sedge
	  = as_a <const callgraph_superedge *> (succ);
	/* Consider turning this edge into a use of an
	   interprocedural summary.  */
	if (eg.get_analysis_plan ().use_summary_p (cg_sedge->m_cedge))
	  {
	    if (logger)
	      logger->log ("using function summary for %qE in %qE",
			   cg_sedge->get_callee_decl (),
			   cg_sedge->get_caller_decl ());
	    return true;
	  }
	else
	  {
	    /* Otherwise, we ignore these edges  */
	    if (logger)
	      logger->log ("rejecting interprocedural edge");
	    return false;
	  }
      }
    }

  return true;
}

/* Comparator for program points within the same supernode,
   for implementing worklist::key_t comparison operators.
   Return negative if POINT_A is before POINT_B
   Return positive if POINT_A is after POINT_B
   Return 0 if they are equal.  */

int
function_point::cmp_within_supernode_1 (const function_point &point_a,
					const function_point &point_b)
{
  gcc_assert (point_a.get_supernode () == point_b.get_supernode ());

  switch (point_a.m_kind)
    {
    default:
      gcc_unreachable ();
    case PK_BEFORE_SUPERNODE:
      switch (point_b.m_kind)
	{
	default:
	  gcc_unreachable ();
	case PK_BEFORE_SUPERNODE:
	  {
	    int a_src_idx = -1;
	    int b_src_idx = -1;
	    if (point_a.m_from_edge)
	      a_src_idx = point_a.m_from_edge->m_src->m_index;
	    if (point_b.m_from_edge)
	      b_src_idx = point_b.m_from_edge->m_src->m_index;
	    return a_src_idx - b_src_idx;
	  }
	  break;

	case PK_BEFORE_STMT:
	case PK_AFTER_SUPERNODE:
	  return -1;
	}
      break;
    case PK_BEFORE_STMT:
      switch (point_b.m_kind)
	{
	default:
	  gcc_unreachable ();
	case PK_BEFORE_SUPERNODE:
	  return 1;

	case PK_BEFORE_STMT:
	  return point_a.m_stmt_idx - point_b.m_stmt_idx;

	case PK_AFTER_SUPERNODE:
	  return -1;
	}
      break;
    case PK_AFTER_SUPERNODE:
      switch (point_b.m_kind)
	{
	default:
	  gcc_unreachable ();
	case PK_BEFORE_SUPERNODE:
	case PK_BEFORE_STMT:
	  return 1;

	case PK_AFTER_SUPERNODE:
	  return 0;
	}
      break;
    }
}

/* Comparator for program points within the same supernode,
   for implementing worklist::key_t comparison operators.
   Return negative if POINT_A is before POINT_B
   Return positive if POINT_A is after POINT_B
   Return 0 if they are equal.  */

int
function_point::cmp_within_supernode (const function_point &point_a,
				      const function_point &point_b)
{
  int result = cmp_within_supernode_1 (point_a, point_b);

  /* Check that the ordering is symmetric  */
#if CHECKING_P
  int reversed = cmp_within_supernode_1 (point_b, point_a);
  gcc_assert (reversed == -result);
#endif

  return result;
}

/* Comparator for imposing an order on function_points.  */

int
function_point::cmp (const function_point &point_a,
		     const function_point &point_b)
{
  int idx_a = point_a.m_supernode ? point_a.m_supernode->m_index : -1;
  int idx_b = point_b.m_supernode ? point_b.m_supernode->m_index : -1;
  if (int cmp_idx = idx_a - idx_b)
    return cmp_idx;
  gcc_assert (point_a.m_supernode == point_b.m_supernode);
  if (point_a.m_supernode)
    return cmp_within_supernode (point_a, point_b);
  else
    return 0;
}

/* Comparator for use by vec<function_point>::qsort.  */

int
function_point::cmp_ptr (const void *p1, const void *p2)
{
  const function_point *fp1 = (const function_point *)p1;
  const function_point *fp2 = (const function_point *)p2;
  return cmp (*fp1, *fp2);
}

/* For PK_BEFORE_STMT, go to next stmt (or to PK_AFTER_SUPERNODE).  */

void
function_point::next_stmt ()
{
  gcc_assert (m_kind == PK_BEFORE_STMT);
  if (++m_stmt_idx == m_supernode->m_stmts.length ())
    {
      m_kind = PK_AFTER_SUPERNODE;
      m_stmt_idx = 0;
    }
}

/* For those program points for which there is a uniquely-defined
   successor, return it.  */

program_point
program_point::get_next () const
{
  switch (m_function_point.get_kind ())
    {
    default:
      gcc_unreachable ();
    case PK_ORIGIN:
    case PK_AFTER_SUPERNODE:
      gcc_unreachable (); /* Not uniquely defined.  */
    case PK_BEFORE_SUPERNODE:
      if (get_supernode ()->m_stmts.length () > 0)
	return before_stmt (get_supernode (), 0, get_call_string ());
      else
	return after_supernode (get_supernode (), get_call_string ());
    case PK_BEFORE_STMT:
      {
	unsigned next_idx = get_stmt_idx ();
	if (next_idx < get_supernode ()->m_stmts.length ())
	  return before_stmt (get_supernode (), next_idx, get_call_string ());
	else
	  return after_supernode (get_supernode (), get_call_string ());
      }
    }
}

#if CHECKING_P

namespace selftest {

/* Verify that function_point::operator== works as expected.  */

static void
test_function_point_equality ()
{
  const supernode *snode = NULL;

  function_point a = function_point (snode, NULL, 0,
				     PK_BEFORE_SUPERNODE);
  function_point b = function_point::before_supernode (snode, NULL);
  ASSERT_EQ (a, b);
}

/* Verify that function_point::cmp_within_supernode works as expected.  */

static void
test_function_point_ordering ()
{
  const supernode *snode = NULL;
  const call_string call_string;

  /* Populate an array with various points within the same
     snode, in order.  */
  auto_vec<function_point> points;
  points.safe_push (function_point::before_supernode (snode, NULL));
  points.safe_push (function_point::before_stmt (snode, 0));
  points.safe_push (function_point::before_stmt (snode, 1));
  points.safe_push (function_point::after_supernode (snode));

  /* Check all pairs.  */
  unsigned i;
  function_point *point_a;
  FOR_EACH_VEC_ELT (points, i, point_a)
    {
      unsigned j;
      function_point *point_b;
      FOR_EACH_VEC_ELT (points, j, point_b)
	{
	  int cmp = function_point::cmp_within_supernode (*point_a, *point_b);
	  if (i == j)
	    ASSERT_EQ (cmp, 0);
	  if (i < j)
	    ASSERT_TRUE (cmp < 0);
	  if (i > j)
	    ASSERT_TRUE (cmp > 0);
	}
    }
}

/* Verify that program_point::operator== works as expected.  */

static void
test_program_point_equality ()
{
  const supernode *snode = NULL;

  const call_string cs;

  program_point a = program_point::before_supernode (snode, NULL,
						     cs);

  program_point b = program_point::before_supernode (snode, NULL,
						     cs);

  ASSERT_EQ (a, b);
  // TODO: verify with non-empty callstrings, with different edges
}

/* Run all of the selftests within this file.  */

void
analyzer_program_point_cc_tests ()
{
  test_function_point_equality ();
  test_function_point_ordering ();
  test_program_point_equality ();
}

} // namespace selftest

#endif /* CHECKING_P */

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
