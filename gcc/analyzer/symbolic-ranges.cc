/* Typesafe symbolic ranges of values (e.g. "bytes m-n").
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

#include "selftest.h"

#include "analyzer/symbolic-ranges.h"
#include "analyzer/region-model-manager.h"
#include "analyzer/region-model.h"
#include "analyzer/analyzer-selftests.h"

#if ENABLE_ANALYZER

namespace ana {

template<typename ConcreteValueType>
std::unique_ptr<text_art::tree_widget>
symbolic::range<ConcreteValueType>::
make_dump_widget (symbolic::node_dump_state &nds,
		  const char *prefix) const
{
  using text_art::tree_widget;
  std::unique_ptr<tree_widget> widget;
  if (prefix)
    widget = tree_widget::from_fmt (nds, nullptr, "%s: symbolic range", prefix);
  else
    widget = tree_widget::from_fmt (nds, nullptr, "symbolic range");
  widget->add_child (m_start_offset->make_dump_widget (nds, "start offset"));
  widget->add_child (m_size->make_dump_widget (nds, "size"));
  return widget;
}

template
std::unique_ptr<text_art::tree_widget>
symbolic::range<integer_with_unit<units::bit>>::
make_dump_widget (symbolic::node_dump_state &nds,
		  const char *prefix) const;

template<typename ConcreteValueType>
void
symbolic::range<ConcreteValueType>::accept_node_visitor (node_visitor &v) const
{
  m_start_offset->accept_node_visitor (v);
  m_size->accept_node_visitor (v);
}

template
void
symbolic::range<integer_with_unit<units::bit>>::
accept_node_visitor (node_visitor &v) const;

template<>
DEBUG_FUNCTION void
symbolic_bit_range::dump () const
{
  symbolic::dump_node_tree_to_stderr<symbolic_bit_range> (*this);
}

template<>
DEBUG_FUNCTION void
symbolic_byte_range::dump () const
{
  symbolic::dump_node_tree_to_stderr<symbolic_byte_range> (*this);
}

template<typename ConcreteValueType>
void
symbolic::range<ConcreteValueType>::log (const char *label, logger &log) const
{
  pretty_printer *pp = log.get_printer ();
  log.start_log_line ();
  pp_printf (pp, "%s: ", label);
  dump_inline_to_pp (pp);
  log.end_log_line ();
}

template
void
symbolic_bit_range::log (const char *label, logger &log) const;

template<typename ConcreteValueType>
std::unique_ptr<json::object>
symbolic::range<ConcreteValueType>::to_json () const
{
  auto obj = std::make_unique<json::object> ();
  obj->set ("start_offset", m_start_offset->to_json ());
  obj->set ("size", m_size->to_json ());
  return obj;
}

template
std::unique_ptr<json::object>
symbolic_bit_range::to_json () const;

template
std::unique_ptr<json::object>
symbolic_byte_range::to_json () const;

template<typename ConcreteValueType>
void
symbolic::range<ConcreteValueType>::dump_inline_to_pp (pretty_printer *pp) const
{
  pp_character (pp, '[');
  if (m_size->is_zero_p ())
    {
      pp_character (pp, ']');
      return;
    }
  if (m_size->is_one_p ())
    {
      m_start_offset->dump_inline_to_pp (pp, units::role::offset);
      pp_character (pp, ']');
      return;
    }
  m_start_offset->dump_inline_to_pp (pp, units::role::offset);
  pp_string (pp, "..");
  get_next_offset ().dump_inline_to_pp (pp, units::role::offset);
  pp_character (pp, ')');
}

template
void
symbolic_bit_range::dump_inline_to_pp (pretty_printer *pp) const;

symbolic_bit_range
bits_from_bytes (const symbolic_byte_range &byte_range)
{
  return symbolic_bit_range (bits_from_bytes (byte_range.get_start_offset ()),
			     bits_from_bytes (byte_range.get_size ()));
}

/* Attempt to determine if THIS range intersects OTHER,
   using constraints from MODEL.  */

template<typename ConcreteValueType>
tristate
symbolic::range<ConcreteValueType>::
intersects (const symbolic::range<ConcreteValueType> &other,
	    const region_model &model) const
{
  /* If either is empty, then there is no intersection.  */
  if (this->empty_p ())
    return tristate::TS_FALSE;
  if (other.empty_p ())
    return tristate::TS_FALSE;

  /* For brevity, consider THIS to be "range A", and OTHER to be "range B".  */

  region_model_manager *mgr = model.get_manager ();

  auto &first_offset_a = get_start_offset ();
  auto &first_offset_b = other.get_start_offset ();

  if (&first_offset_a == &first_offset_b)
    return tristate::TS_TRUE;
  auto &last_offset_a = get_last_offset ();
  auto &last_offset_b = other.get_last_offset ();

  auto *first_offset_sval_a
    = mgr->get_svalue_from_scalar<ConcreteValueType> (size_type_node,
						      first_offset_a);
  auto *first_offset_sval_b
    = mgr->get_svalue_from_scalar<ConcreteValueType> (size_type_node,
						      first_offset_b);
  auto *last_offset_sval_a
    = mgr->get_svalue_from_scalar<ConcreteValueType> (size_type_node,
						      last_offset_a);
  auto *last_offset_sval_b
    = mgr->get_svalue_from_scalar<ConcreteValueType> (size_type_node,
						      last_offset_b);
  /* Is B fully before A?  */
  tristate b_fully_before_a = model.eval_condition (last_offset_sval_b,
						    LT_EXPR,
						    first_offset_sval_a);
  /* Is B fully after A?  */
  tristate b_fully_after_a = model.eval_condition (first_offset_sval_b,
						   GT_EXPR,
						   last_offset_sval_a);

  if (b_fully_before_a.is_true ()
      || b_fully_after_a.is_true ())
    return tristate::TS_FALSE;

  if (b_fully_before_a.is_unknown ()
      || b_fully_after_a.is_unknown ())
    return tristate::TS_UNKNOWN;

  return tristate::TS_TRUE;
}

template
tristate
symbolic_byte_range::intersects (const symbolic_byte_range &other,
				 const region_model &model) const;

bool
maybe_bytes_from_bits (const symbolic_bit_range &bit_range,
		       symbolic_byte_range *out)
{
  const symbolic_byte_offset *start_byte_offset
    = maybe_bytes_from_bits (bit_range.get_start_offset ());
  if (!start_byte_offset)
    return false;

  const symbolic_byte_size *byte_size
    = maybe_bytes_from_bits (bit_range.get_size ());
  if (!byte_size)
    return false;

  *out = symbolic_byte_range (*start_byte_offset, *byte_size);
  return true;
}

#if CHECKING_P

namespace selftest {

static void
test_concrete ()
{
  symbolic::node_manager node_mgr;
  auto &bit_mgr = node_mgr.get_scalars ().get_bits ();
  auto &byte_mgr = node_mgr.get_scalars ().get_bytes ();

  symbolic_bit_range bits_16_to_23
    (bit_mgr.get_concrete (16_bits),
     bit_mgr.get_concrete (8_bits));
  ASSERT_SAME_SYMBOLIC_NODE (bits_16_to_23.get_start_offset (),
			     bit_mgr.get_concrete (16_bits));
  ASSERT_SAME_SYMBOLIC_NODE (bits_16_to_23.get_size (),
			     bit_mgr.get_concrete (8_bits));
  ASSERT_SAME_SYMBOLIC_NODE (bits_16_to_23.get_last_offset (),
			     bit_mgr.get_concrete (23_bits));
  ASSERT_SAME_SYMBOLIC_NODE (bits_16_to_23.get_next_offset (),
			     bit_mgr.get_concrete (24_bits));
  bits_16_to_23.to_json ();

  ASSERT_EQ
    (symbolic_bit_range::from_endpoints (bit_mgr.get_concrete (16_bits),
					 bit_mgr.get_concrete (24_bits)),
     bits_16_to_23);

  symbolic_byte_range bytes_16_to_23
    (byte_mgr.get_concrete (16_bytes),
     byte_mgr.get_concrete (8_bytes));
  ASSERT_SAME_SYMBOLIC_NODE (bytes_16_to_23.get_start_offset (),
			     byte_mgr.get_concrete (16_bytes));
  ASSERT_SAME_SYMBOLIC_NODE (bytes_16_to_23.get_size (),
			     byte_mgr.get_concrete (8_bytes));
  ASSERT_SAME_SYMBOLIC_NODE (bytes_16_to_23.get_last_offset (),
			     byte_mgr.get_concrete (23_bytes));
  ASSERT_SAME_SYMBOLIC_NODE (bytes_16_to_23.get_next_offset (),
			     byte_mgr.get_concrete (24_bytes));
  bytes_16_to_23.to_json ();

  ASSERT_EQ
    (symbolic_byte_range::from_endpoints (byte_mgr.get_concrete (16_bytes),
					  byte_mgr.get_concrete (24_bytes)),
     bytes_16_to_23);

}

static void
test_intersects (void)
{
  symbolic::node_manager node_mgr;
  auto &byte_mgr = node_mgr.get_scalars ().get_bytes ();
  region_model m (node_mgr);

  /* Test various concrete ranges.  */
  auto &zero = byte_mgr.get_concrete (0_bytes);
  auto &one = byte_mgr.get_concrete (1_byte);
  auto &five = byte_mgr.get_concrete (5_bytes);
  auto &nine = byte_mgr.get_concrete (9_bytes);
  auto &ten = byte_mgr.get_concrete (10_bytes);

  symbolic_byte_range r0_9 (zero, ten);
  symbolic_byte_range r0 (zero, one);
  symbolic_byte_range r5_9 (five, five);
  symbolic_byte_range r9 (nine, one);
  symbolic_byte_range r10 (ten, one);
  symbolic_byte_range r10_19 (ten, ten);

  ASSERT_EQ (r0_9.get_start_offset (), zero);
  ASSERT_EQ (r0_9.get_size (), ten);
  ASSERT_EQ (r0_9.get_next_offset (), ten);
  ASSERT_EQ (r0_9.get_last_offset (), nine);

  symbolic_byte_range concrete_empty (zero, zero);
  ASSERT_TRUE (concrete_empty.empty_p ());

  ASSERT_EQ (r0_9.intersects (r0, m), tristate::TS_TRUE);
  ASSERT_EQ (r0.intersects (r0_9, m), tristate::TS_TRUE);
  ASSERT_EQ (r0_9.intersects (r9, m), tristate::TS_TRUE);
  ASSERT_EQ (r9.intersects (r0_9, m), tristate::TS_TRUE);
  ASSERT_EQ (r0_9.intersects (r10, m), tristate::TS_FALSE);
  ASSERT_EQ (r10.intersects (r0_9, m), tristate::TS_FALSE);
  ASSERT_EQ (concrete_empty.intersects (r0_9, m), tristate::TS_FALSE);
  ASSERT_EQ (r0_9.intersects (concrete_empty, m), tristate::TS_FALSE);

  ASSERT_EQ (r5_9.intersects (r0, m), tristate::TS_FALSE);
  ASSERT_EQ (r0.intersects (r5_9, m), tristate::TS_FALSE);
  ASSERT_EQ (r9.intersects (r5_9, m), tristate::TS_TRUE);
  ASSERT_EQ (r10.intersects (r5_9, m), tristate::TS_FALSE);

  /* Test various symbolic ranges.  */
  tree x = build_global_decl ("x", size_type_node);
  const svalue *x_init_sval = m.get_rvalue (x, nullptr);
  auto &x_init = byte_mgr.get_scalar_from_svalue (*x_init_sval);
  tree y = build_global_decl ("y", size_type_node);
  const svalue *y_init_sval = m.get_rvalue (y, nullptr);
  auto &y_init = byte_mgr.get_scalar_from_svalue (*y_init_sval);

  symbolic_byte_range r0_x_minus_1 (zero, x_init);
  symbolic_byte_range rx (x_init, one);
  symbolic_byte_range r0_y_minus_1 (zero, y_init);
  symbolic_byte_range ry (y_init, one);
  symbolic_byte_range rx_x_plus_y_minus_1 (x_init, y_init);
  symbolic_byte_range symbolic_empty (x_init, zero);
  ASSERT_TRUE (symbolic_empty.empty_p ());

  ASSERT_SAME_SYMBOLIC_NODE (rx_x_plus_y_minus_1.get_start_offset (), x_init);
  ASSERT_SAME_SYMBOLIC_NODE (rx_x_plus_y_minus_1.get_size (), y_init);
  ASSERT_EQ
    (rx_x_plus_y_minus_1.get_next_offset ().get_kind (),
     symbolic_byte_offset::kind::sum);
  ASSERT_EQ
    (rx_x_plus_y_minus_1.get_last_offset ().get_kind (),
     symbolic_byte_offset::kind::sum);

  ASSERT_EQ (rx.intersects (ry, m), tristate::TS_UNKNOWN);
  ASSERT_EQ (rx.intersects (concrete_empty, m), tristate::TS_FALSE);
  ASSERT_EQ (concrete_empty.intersects (rx, m), tristate::TS_FALSE);
  ASSERT_EQ (rx.intersects (symbolic_empty, m), tristate::TS_FALSE);
  ASSERT_EQ (symbolic_empty.intersects (rx, m), tristate::TS_FALSE);
  ASSERT_EQ (r0_x_minus_1.intersects (r0, m), tristate::TS_TRUE);
  ASSERT_EQ (r0_x_minus_1.intersects (r0_y_minus_1, m), tristate::TS_TRUE);
}

/* Run all of the selftests within this file.  */

void
analyzer_symbolic_ranges_cc_tests ()
{
  test_concrete ();
  test_intersects ();
}

} // namespace selftest

#endif /* CHECKING_P */

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
