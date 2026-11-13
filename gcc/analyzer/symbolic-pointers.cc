/* A (base_region, symbolic byte offset) pair.
   Copyright (C) 2026 Free Software Foundation, Inc.
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

#include "analyzer/symbolic-pointers.h"
#include "analyzer/region-model.h"
#include "analyzer/analyzer-selftests.h"

#if ENABLE_ANALYZER

namespace ana {
namespace symbolic {

// class pointer

DEBUG_FUNCTION void
pointer::dump () const
{
  symbolic::dump_node_tree_to_stderr<pointer> (*this);
}

std::unique_ptr<text_art::tree_widget>
pointer::make_dump_widget (symbolic::node_dump_state &nds) const
{
  using text_art::tree_widget;
  std::unique_ptr<tree_widget> widget
    (tree_widget::from_fmt (nds, nullptr, "Pointer"));
  widget->add_child (m_base_reg.make_dump_widget (nds, "base region"));
  widget->add_child (m_byte_offset.make_dump_widget (nds, "byte offset"));
  return widget;
}

void
pointer::accept_node_visitor (symbolic::node_visitor &v) const
{
  m_base_reg.accept_node_visitor (v);
  m_byte_offset.accept_node_visitor (v);
}

std::unique_ptr<json::object>
pointer::to_json () const
{
  auto obj = std::make_unique<json::object> ();
  obj->set ("base_reg", m_base_reg.to_json ());
  obj->set ("byte_offset", m_byte_offset.to_json ());
  return obj;
}

int
pointer::cmp (const pointer &other) const
{
  if (int c = symbolic::node::cmp (m_base_reg, other.m_base_reg))
    return c;
  return symbolic::node::cmp (m_byte_offset, other.m_byte_offset);
}

/* A symbolic::pointer is a non-NULL pointer, except in certain
   special cases (see the comment for region::non_null_p).  */

bool
pointer::non_null_p () const
{
  if (!zero_offset_p ())
    return true;
  return m_base_reg.non_null_p ();
}

/* Compare byte offsets OFFSET_A and OFFSET_B.  */

static tristate
eval_byte_offset_comparison (const pointer::symbolic_byte_offset &offset_a,
			     enum tree_code op,
			     const pointer::symbolic_byte_offset &offset_b,
			     const region_model &model)
{
  return symbolic_byte_offset::get_partial_ordering (offset_a, op, offset_b, &model);
}

/* Evaluate the condition LHS OP RHS for comparing symbolic pointers.

   See if they point to the same base region, and if we know about
   the offsets within their regions.

   Use MODEL for aliasing information and any knowledge from
   constraint_manager about ordering of pointers.  */

tristate
pointer::eval_condition (const pointer &lhs,
			 enum tree_code op,
			 const pointer &rhs,
			 const region_model &model)
{
  /* Try to determine if they're the same base region.  */
  const tristate same_base_region
    = model.get_store ()->eval_alias (&lhs.get_base_region (),
				      &rhs.get_base_region (),
				      model);

  /* Try to determine if they're the same offset relative to their
     base region.  */
  const tristate same_byte_offset
    = eval_byte_offset_comparison (lhs.get_byte_offset (),
				   EQ_EXPR,
				   rhs.get_byte_offset (),
				   model);

  /* With that, we might know if they're equal/non-equal.  */
  const tristate equality = same_base_region.and_(same_byte_offset);

  switch (op)
    {
    default:
      gcc_unreachable ();

    case EQ_EXPR:
      return equality;

    case NE_EXPR:
      return equality.not_ ();

    case GE_EXPR:
    case LE_EXPR:
      if (equality.is_true ())
	return tristate (true);
      else if (same_base_region.is_true ())
	return eval_byte_offset_comparison (lhs.get_byte_offset (),
					    op,
					    rhs.get_byte_offset (),
					    model);
      else
	return tristate::unknown ();

    case GT_EXPR:
    case LT_EXPR:
      if (equality.is_true ())
	return tristate (false);
      else if (same_base_region.is_true ())
	return eval_byte_offset_comparison (lhs.get_byte_offset (),
					    op,
					    rhs.get_byte_offset (),
					    model);
      else
	return tristate::unknown ();
    }
}

} // namespace ana::symbolic

#if CHECKING_P

namespace selftest {

static void
test_eval_condition ()
{
  symbolic::node_manager node_mgr;
  auto &rmm = node_mgr.get_svalue_manager ();
  region_model model (node_mgr);

  coord_test ct;

  tree c1 = build_global_decl ("c1", ct.m_coord_type);
  tree c2 = build_global_decl ("c2", ct.m_coord_type);

  tree c1_x = build3 (COMPONENT_REF, TREE_TYPE (ct.m_x_field),
		      c1, ct.m_x_field, NULL_TREE);
  tree c1_y = build3 (COMPONENT_REF, TREE_TYPE (ct.m_y_field),
		      c1, ct.m_y_field, NULL_TREE);

  const region *c1_reg = model.get_lvalue (c1, nullptr);
  const region *c1_x_reg = model.get_lvalue (c1_x, nullptr);
  const region *c1_y_reg = model.get_lvalue (c1_y, nullptr);

  const region *c2_reg = model.get_lvalue (c2, nullptr);

  auto ptr_to_c1 = c1_reg->get_pointer (rmm);
  auto ptr_to_c1_x = c1_x_reg->get_pointer (rmm);
  auto ptr_to_c1_y = c1_y_reg->get_pointer (rmm);

  auto ptr_to_c2 = c2_reg->get_pointer (rmm);

  // Self-comparison of pointer with concrete offset:
  {
    ASSERT_EQ (symbolic::pointer::eval_condition (ptr_to_c1,
						  EQ_EXPR,
						  ptr_to_c1,
						  model),
	       tristate (true));
    ASSERT_EQ (symbolic::pointer::eval_condition (ptr_to_c1,
						  NE_EXPR,
						  ptr_to_c1,
						  model),
	       tristate (false));
    ASSERT_EQ (symbolic::pointer::eval_condition (ptr_to_c1,
						  GE_EXPR,
						  ptr_to_c1,
						  model),
	       tristate (true));
    ASSERT_EQ (symbolic::pointer::eval_condition (ptr_to_c1,
						  LE_EXPR,
						  ptr_to_c1,
						  model),
	       tristate (true));
    ASSERT_EQ (symbolic::pointer::eval_condition (ptr_to_c1,
						  GT_EXPR,
						  ptr_to_c1,
						  model),
	       tristate (false));
    ASSERT_EQ (symbolic::pointer::eval_condition (ptr_to_c1,
						  LT_EXPR,
						  ptr_to_c1,
						  model),
	       tristate (false));
  }

  // Pointer to struct should equal pointer to initial field of that struct:
  ASSERT_EQ (ptr_to_c1, ptr_to_c1_x);

  /* Comparison of pointers to different fields in the same struct
     (same base region, different concrete offset).  */
  {
    ASSERT_EQ (symbolic::pointer::eval_condition (ptr_to_c1_x,
						  EQ_EXPR,
						  ptr_to_c1_y,
						  model),
	       tristate (false));
    ASSERT_EQ (symbolic::pointer::eval_condition (ptr_to_c1_x,
						  NE_EXPR,
						  ptr_to_c1_y,
						  model),
	       tristate (true));
    ASSERT_EQ (symbolic::pointer::eval_condition (ptr_to_c1_x,
						  GE_EXPR,
						  ptr_to_c1_y,
						  model),
	       tristate (false));
    ASSERT_EQ (symbolic::pointer::eval_condition (ptr_to_c1_x,
						  LE_EXPR,
						  ptr_to_c1_y,
						  model),
	       tristate (true));
    ASSERT_EQ (symbolic::pointer::eval_condition (ptr_to_c1_x,
						  GT_EXPR,
						  ptr_to_c1_y,
						  model),
	       tristate (false));
    ASSERT_EQ (symbolic::pointer::eval_condition (ptr_to_c1_x,
						  LT_EXPR,
						  ptr_to_c1_y,
						  model),
	       tristate (true));
  }

  /* Different base regions where we know nothing, but know
     they are concrete base regions.  */
  {
    ASSERT_EQ (symbolic::pointer::eval_condition (ptr_to_c1,
						  EQ_EXPR,
						  ptr_to_c2,
						  model),
	       tristate (false));
    ASSERT_EQ (symbolic::pointer::eval_condition (ptr_to_c1,
						  NE_EXPR,
						  ptr_to_c2,
						  model),
	       tristate (true));
    ASSERT_EQ (symbolic::pointer::eval_condition (ptr_to_c1,
						  GE_EXPR,
						  ptr_to_c2,
						  model),
	       tristate::unknown ());
    ASSERT_EQ (symbolic::pointer::eval_condition (ptr_to_c1,
						  LE_EXPR,
						  ptr_to_c2,
						  model),
	       tristate::unknown ());
    ASSERT_EQ (symbolic::pointer::eval_condition (ptr_to_c1,
						  GT_EXPR,
						  ptr_to_c2,
						  model),
	       tristate::unknown ());
    ASSERT_EQ (symbolic::pointer::eval_condition (ptr_to_c1,
						  LT_EXPR,
						  ptr_to_c2,
						  model),
	       tristate::unknown ());
  }
}

/* Run all of the selftests within this file.  */

void
analyzer_symbolic_pointers_cc_tests ()
{
  test_eval_condition ();
}

} // namespace selftest

#endif /* CHECKING_P */

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
