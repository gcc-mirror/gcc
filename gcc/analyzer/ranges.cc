/* Symbolic offsets and ranges.
   Copyright (C) 2023-2025 Free Software Foundation, Inc.
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
#include "diagnostic-core.h"
#include "gimple-pretty-print.h"
#include "function.h"
#include "basic-block.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "diagnostic-core.h"
#include "graphviz.h"
#include "options.h"
#include "cgraph.h"
#include "tree-dfa.h"
#include "stringpool.h"
#include "convert.h"
#include "target.h"
#include "fold-const.h"
#include "tree-pretty-print.h"
#include "bitmap.h"
#include "analyzer/analyzer.h"
#include "analyzer/analyzer-logging.h"
#include "ordered-hash-map.h"
#include "options.h"
#include "analyzer/supergraph.h"
#include "sbitmap.h"
#include "analyzer/call-string.h"
#include "analyzer/program-point.h"
#include "analyzer/store.h"
#include "analyzer/region-model.h"
#include "analyzer/constraint-manager.h"
#include "analyzer/analyzer-selftests.h"
#include "analyzer/ranges.h"
#include "make-unique.h"

#if ENABLE_ANALYZER

namespace ana {

/* class symbolic_byte_offset.  */

symbolic_byte_offset::symbolic_byte_offset (int i, region_model_manager &mgr)
: m_num_bytes_sval (mgr.get_or_create_int_cst (size_type_node, i))
{
}

symbolic_byte_offset::symbolic_byte_offset (const svalue *num_bytes_sval)
: m_num_bytes_sval (num_bytes_sval)
{
}

symbolic_byte_offset::symbolic_byte_offset (region_offset offset,
					    region_model_manager &mgr)
{
  if (offset.concrete_p ())
    {
      bit_offset_t num_bits = offset.get_bit_offset ();
      gcc_assert (num_bits % BITS_PER_UNIT == 0);
      byte_offset_t num_bytes = num_bits / BITS_PER_UNIT;
      m_num_bytes_sval = mgr.get_or_create_int_cst (size_type_node, num_bytes);
    }
  else
    m_num_bytes_sval = offset.get_symbolic_byte_offset ();
}

void
symbolic_byte_offset::dump_to_pp (pretty_printer *pp, bool simple) const
{
  pp_string (pp, "byte ");
  m_num_bytes_sval->dump_to_pp (pp, simple);
}

void
symbolic_byte_offset::dump (bool simple) const
{
  tree_dump_pretty_printer pp (stderr);
  dump_to_pp (&pp, simple);
  pp_newline (&pp);
}

std::unique_ptr<json::value>
symbolic_byte_offset::to_json () const
{
  return m_num_bytes_sval->to_json ();
}

tree
symbolic_byte_offset::maybe_get_constant () const
{
  return m_num_bytes_sval->maybe_get_constant ();
}

/* class symbolic_byte_range.  */

symbolic_byte_range::symbolic_byte_range (region_offset start,
					  const svalue *num_bytes,
					  region_model_manager &mgr)
: m_start (start, mgr),
  m_size (num_bytes)
{
}

void
symbolic_byte_range::dump_to_pp (pretty_printer *pp,
				 bool simple,
				 region_model_manager &mgr) const
{
  if (empty_p ())
    {
      pp_string (pp, "empty");
      return;
    }

  if (tree size_cst = m_size.maybe_get_constant ())
    if (integer_onep (size_cst))
      {
	pp_string (pp, "byte ");
	m_start.get_svalue ()->dump_to_pp (pp, simple);
	return;
      }

  pp_string (pp, "bytes ");
  m_start.get_svalue ()->dump_to_pp (pp, simple);
  pp_string (pp, " to ");
  get_last_byte_offset (mgr).get_svalue ()->dump_to_pp (pp, simple);
}

void
symbolic_byte_range::dump (bool simple, region_model_manager &mgr) const
{
  tree_dump_pretty_printer pp (stderr);
  dump_to_pp (&pp, simple, mgr);
  pp_newline (&pp);
}

std::unique_ptr<json::value>
symbolic_byte_range::to_json () const
{
  auto obj = ::make_unique<json::object> ();
  obj->set ("start", m_start.to_json ());
  obj->set ("size", m_size.to_json ());
  return obj;
}

bool
symbolic_byte_range::empty_p () const
{
  tree cst = m_size.maybe_get_constant ();
  if (!cst)
    return false;
  return zerop (cst);
}

symbolic_byte_offset
symbolic_byte_range::get_last_byte_offset (region_model_manager &mgr) const
{
  gcc_assert (!empty_p ());
  const symbolic_byte_offset one (1, mgr);
  return symbolic_byte_offset
    (mgr.get_or_create_binop (size_type_node,
			      MINUS_EXPR,
			      get_next_byte_offset (mgr).get_svalue (),
			      one.get_svalue ()));
}

symbolic_byte_offset
symbolic_byte_range::get_next_byte_offset (region_model_manager &mgr) const
{
  return symbolic_byte_offset (mgr.get_or_create_binop (size_type_node,
							PLUS_EXPR,
							m_start.get_svalue (),
							m_size.get_svalue ()));
}

/* Attempt to determine if THIS range intersects OTHER,
   using constraints from MODEL.  */

tristate
symbolic_byte_range::intersection (const symbolic_byte_range &other,
				   const region_model &model) const
{
  /* If either is empty, then there is no intersection.  */
  if (empty_p ())
    return tristate::TS_FALSE;
  if (other.empty_p ())
    return tristate::TS_FALSE;

  /* For brevity, consider THIS to be "range A", and OTHER to be "range B".  */

  region_model_manager *mgr = model.get_manager ();

  const svalue *first_sval_a = m_start.get_svalue ();
  const svalue *first_sval_b = other.m_start.get_svalue ();
  const svalue *last_sval_a = get_last_byte_offset (*mgr).get_svalue ();
  const svalue *last_sval_b = other.get_last_byte_offset (*mgr).get_svalue ();

  if (m_size.get_svalue ()->get_kind () == SK_UNKNOWN
      || other.m_size.get_svalue ()->get_kind () == SK_UNKNOWN)
    {
      if (first_sval_a == first_sval_b)
	return tristate::TS_TRUE;
      else
	return tristate::TS_UNKNOWN;
    }

  if (first_sval_a == first_sval_b)
    return tristate::TS_TRUE;

  /* Is B fully before A?  */
  tristate b_fully_before_a = model.eval_condition (last_sval_b,
						    LT_EXPR,
						    first_sval_a);
  /* Is B fully after A?  */
  tristate b_fully_after_a = model.eval_condition (first_sval_b,
						   GT_EXPR,
						   last_sval_a);

  if (b_fully_before_a.is_true ()
      || b_fully_after_a.is_true ())
    return tristate::TS_FALSE;

  if (b_fully_before_a.is_unknown ()
      || b_fully_after_a.is_unknown ())
    return tristate::TS_UNKNOWN;

  return tristate::TS_TRUE;
}

#if CHECKING_P

namespace selftest {

static void test_intersects (void)
{
  region_model_manager mgr;
  region_model m (&mgr);

  /* Test various concrete ranges.  */
  symbolic_byte_offset zero (0, mgr);
  symbolic_byte_offset one (1, mgr);
  symbolic_byte_offset five (5, mgr);
  symbolic_byte_offset nine (9, mgr);
  symbolic_byte_offset ten (10, mgr);

  symbolic_byte_range r0_9 (zero, ten);
  symbolic_byte_range r0 (zero, one);
  symbolic_byte_range r5_9 (five, five);
  symbolic_byte_range r9 (nine, one);
  symbolic_byte_range r10 (ten, one);
  symbolic_byte_range r10_19 (ten, ten);

  ASSERT_EQ (r0_9.get_start_byte_offset (), zero);
  ASSERT_EQ (r0_9.get_size_in_bytes (), ten);
  ASSERT_EQ (r0_9.get_next_byte_offset (mgr), ten);
  ASSERT_EQ (r0_9.get_last_byte_offset (mgr), nine);

  symbolic_byte_range concrete_empty (zero, zero);
  ASSERT_TRUE (concrete_empty.empty_p ());

  ASSERT_EQ (r0_9.intersection (r0, m), tristate::TS_TRUE);
  ASSERT_EQ (r0.intersection (r0_9, m), tristate::TS_TRUE);
  ASSERT_EQ (r0_9.intersection (r9, m), tristate::TS_TRUE);
  ASSERT_EQ (r9.intersection (r0_9, m), tristate::TS_TRUE);
  ASSERT_EQ (r0_9.intersection (r10, m), tristate::TS_FALSE);
  ASSERT_EQ (r10.intersection (r0_9, m), tristate::TS_FALSE);
  ASSERT_EQ (concrete_empty.intersection (r0_9, m), tristate::TS_FALSE);
  ASSERT_EQ (r0_9.intersection (concrete_empty, m), tristate::TS_FALSE);

  ASSERT_EQ (r5_9.intersection (r0, m), tristate::TS_FALSE);
  ASSERT_EQ (r0.intersection (r5_9, m), tristate::TS_FALSE);
  ASSERT_EQ (r9.intersection (r5_9, m), tristate::TS_TRUE);
  ASSERT_EQ (r10.intersection (r5_9, m), tristate::TS_FALSE);

  /* Test various symbolic ranges.  */
  tree x = build_global_decl ("x", size_type_node);
  const svalue *x_init_sval = m.get_rvalue (x, nullptr);
  tree y = build_global_decl ("y", size_type_node);
  const svalue *y_init_sval = m.get_rvalue (y, nullptr);

  symbolic_byte_range r0_x_minus_1 (zero, x_init_sval);
  symbolic_byte_range rx (x_init_sval, one);
  symbolic_byte_range r0_y_minus_1 (zero, y_init_sval);
  symbolic_byte_range ry (y_init_sval, one);
  symbolic_byte_range rx_x_plus_y_minus_1 (x_init_sval, y_init_sval);

  symbolic_byte_range symbolic_empty (x_init_sval, zero);
  ASSERT_TRUE (symbolic_empty.empty_p ());

  ASSERT_EQ (rx_x_plus_y_minus_1.get_start_byte_offset (), x_init_sval);
  ASSERT_EQ (rx_x_plus_y_minus_1.get_size_in_bytes (), y_init_sval);
  ASSERT_EQ
    (rx_x_plus_y_minus_1.get_next_byte_offset (mgr).get_svalue ()->get_kind (),
     SK_BINOP);
  ASSERT_EQ
    (rx_x_plus_y_minus_1.get_last_byte_offset (mgr).get_svalue ()->get_kind (),
     SK_BINOP);

  ASSERT_EQ (rx.intersection (ry, m), tristate::TS_UNKNOWN);
  ASSERT_EQ (rx.intersection (concrete_empty, m), tristate::TS_FALSE);
  ASSERT_EQ (concrete_empty.intersection (rx, m), tristate::TS_FALSE);
  ASSERT_EQ (rx.intersection (symbolic_empty, m), tristate::TS_FALSE);
  ASSERT_EQ (symbolic_empty.intersection (rx, m), tristate::TS_FALSE);
  ASSERT_EQ (r0_x_minus_1.intersection (r0, m), tristate::TS_TRUE);
#if 0
  ASSERT_EQ (r0_x_minus_1.intersection (rx, m), tristate::TS_FALSE);
  /* Fails (with UNKNOWN): b_fully_after_a is UNKNOWN, when it could
     be TRUE: last of A is (x - 1), but it's not necessarily true that
     X > (x - 1), for the case where x is (unsigned)0.  */
#endif
  ASSERT_EQ (r0_x_minus_1.intersection (r0_y_minus_1, m), tristate::TS_TRUE);
  // TODO: etc
}

/* Run all of the selftests within this file.  */

void
analyzer_ranges_cc_tests ()
{
  test_intersects ();
}

} // namespace selftest

#endif /* CHECKING_P */

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
