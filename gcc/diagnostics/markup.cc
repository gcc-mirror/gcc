/* A tree-like representation for use when building diagnostic messages.
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

#include "config.h"
#define INCLUDE_MAP
#define INCLUDE_STRING
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"

#include "diagnostics/markup.h"

#include "text-art/dump-widget-info.h"

#include "selftest.h"

namespace diagnostics {
namespace markup {

static bool
child_needs_parens_p (const node &parent, const node &child)
{
  if (parent.get_kind () == kind::inline_code)
    return false;

  switch (child.get_kind ())
    {
    case kind::text:
      return false;
    case kind::compound:
    case kind::math_negation:
    case kind::math_subtraction:
    case kind::math_sum:
    case kind::math_product:
    case kind::math_half_open_interval:
      return true;

    case kind::code_element_expr:
    case kind::code_field_expr:
    case kind::code_infix_binary_op:
      return false;

    case kind::inline_code:
      return false;
    }

  return true;
}

/* Lower CHILD of PARENT and add to DST_NODE,
   wrapping lowered node in parentheses if necessary.  */

static void
lower_and_append_with_any_parens (compound_node &dst_node,
				  const node &parent,
				  const node &child,
				  const lowering_options &opts)
{
  bool needs_parens = child_needs_parens_p (parent, child);

  if (needs_parens)
    dst_node.add_child ("(");

  dst_node.add_child (child.lower (opts));

  if (needs_parens)
    dst_node.add_child (")");
}


// class node

static void
dump_node_tree_to_pp (const node &n,
		      text_art::theme *theme,
		      pretty_printer *pp)
{
  if (!theme)
    return;

  using namespace text_art;

  style_manager sm;
  style tree_style (get_style_from_color_cap_name ("note"));

  style::id_t tree_style_id (sm.get_or_create_id (tree_style));

  dump_widget_info dwi  (sm, *theme, tree_style_id);
  if (std::unique_ptr<widget> w = n.make_dump_widget (dwi))
    {
      text_art::canvas c (w->to_canvas (sm));
      c.print_to_pp (pp);
    }
}

void
node::dump () const
{
  pretty_printer pp;
  pp.set_output_stream (stderr);
  text_art::ascii_theme theme;
  dump_node_tree_to_pp (*this, &theme, &pp);
  pp_flush (&pp);
}

// class text : public node

void
text::print_to_pp (pretty_printer *pp) const
{
  pp_string (pp, m_str.c_str ());
}

std::unique_ptr<text_art::tree_widget>
text::make_dump_widget (text_art::dump_widget_info &dwi) const
{
  return text_art::tree_widget::from_fmt (dwi, nullptr,
					  "text: %qs", m_str.c_str ());
}

std::unique_ptr<node>
text::lower (const lowering_options &opts) const
{
  return std::make_unique<text> (m_str);
}

// class compound_node : public node

void
compound_node::print_to_pp (pretty_printer *pp) const
{
  for (auto &iter : m_children)
    iter->print_to_pp (pp);
}

std::unique_ptr<text_art::tree_widget>
compound_node::make_dump_widget (text_art::dump_widget_info &dwi) const
{
  std::unique_ptr<text_art::tree_widget> w
    (text_art::tree_widget::from_fmt (dwi, nullptr,
				      "%s",
				      get_class_name ()));

  for (auto &iter : m_children)
    w->add_child (iter->make_dump_widget (dwi));

  return w;
}

std::unique_ptr<node>
compound_node::lower (const lowering_options &opts) const
{
  auto result = std::make_unique<compound_node> ();

  for (size_t idx = 0; idx < get_num_children (); ++idx)
    result->add_child (get_child (idx).lower (opts));

  return result;
}

void
compound_node::add_child (std::string str)
{
  add_child (std::make_unique<text> (std::move (str)));
}

// class unary_node : public node

void
unary_node::print_to_pp (pretty_printer *pp) const
{
  pp_string (pp, get_class_name ());
  pp_string (pp, " (");
  m_operand->print_to_pp (pp);
  pp_string (pp, ")");
}

std::unique_ptr<text_art::tree_widget>
unary_node::make_dump_widget (text_art::dump_widget_info &dwi) const
{
  std::unique_ptr<text_art::tree_widget> w
    (text_art::tree_widget::from_fmt (dwi, nullptr,
				      "%qs",
				      get_class_name ()));

  w->add_child (m_operand->make_dump_widget (dwi));

  return w;
}

// class binary_node : public node

void
binary_node::print_to_pp (pretty_printer *pp) const
{
  pp_string (pp, get_class_name ());
  pp_string (pp, " (");
  m_op0->print_to_pp (pp);
  pp_string (pp, ", ");
  m_op1->print_to_pp (pp);
  pp_string (pp, ")");
}

std::unique_ptr<text_art::tree_widget>
binary_node::make_dump_widget (text_art::dump_widget_info &dwi) const
{
  std::unique_ptr<text_art::tree_widget> w
    (text_art::tree_widget::from_fmt (dwi, nullptr,
				      "%qs",
				      get_class_name ()));

  w->add_child (m_op0->make_dump_widget (dwi));
  w->add_child (m_op1->make_dump_widget (dwi));

  return w;
}

// class math::associative_op : public compound_node

std::unique_ptr<node>
math::associative_op::lower (const lowering_options &opts) const
{
  gcc_assert (get_num_children () >= 2);

  auto result = std::make_unique<compound_node> ();

  pretty_printer pp_infix_symbol;
  pp_printf (&pp_infix_symbol, " %s ",
	     get_symbol (opts.m_use_unicode_symbols));

  for (size_t idx = 0; idx < get_num_children (); ++idx)
    {
      if (idx > 0)
	result->add_child (pp_formatted_text (&pp_infix_symbol));
      lower_and_append_with_any_parens (*result,
					*this,
					get_child (idx),
					opts);
    }

  return result;
}

// class math::sum : public associative_op

const char *
math::sum::get_symbol (bool /* use_unicode */) const
{
  return "+";
}

// class math::product : public associative_op

const char *
math::product::get_symbol (bool use_unicode) const
{
  if (use_unicode)
    return MARKUP_UNICODE_SYMBOL_MULTIPLICATION;
  else
    return "*";
}

// class math::negation : public unary_node

std::unique_ptr<node>
math::negation::lower (const lowering_options &opts) const
{
  auto result = std::make_unique<compound_node> ();
  const char *symbol = (opts.m_use_unicode_symbols
			? MARKUP_UNICODE_SYMBOL_UNARY_NEGATION
			: "-");
  result->add_child (symbol);
  lower_and_append_with_any_parens (*result, *this, get_operand (), opts);
  return result;
}

// class math::subtraction : public binary_node

std::unique_ptr<node>
math::subtraction::lower (const lowering_options &opts) const
{
  auto result = std::make_unique<compound_node> ();

  lower_and_append_with_any_parens (*result, *this, get_operand_0 (), opts);

  const char *padded_symbol
    = (opts.m_use_unicode_symbols
       ? (" " MARKUP_UNICODE_SYMBOL_SUBTRACTION " ")
       : " - ");
  result->add_child (padded_symbol);
  lower_and_append_with_any_parens (*result, *this, get_operand_1 (), opts);

  return result;
}

// class math::half_open_interval : public binary_node

std::unique_ptr<node>
math::half_open_interval::lower (const lowering_options &opts) const
{
  auto result = std::make_unique<compound_node> ();

  result->add_child ("[");
  lower_and_append_with_any_parens (*result, *this, get_operand_0 (), opts);
  result->add_child (", ");
  lower_and_append_with_any_parens (*result, *this, get_operand_1 (), opts);
  result->add_child (")");
  return result;
}

// class code::element_expr : public binary_node

std::unique_ptr<node>
code::element_expr::lower (const lowering_options &opts) const
{
  auto result = std::make_unique<compound_node> ();
  result->add_child (get_operand_0 ().lower (opts));
  result->add_child ("[");
  result->add_child (get_operand_1 ().lower (opts));
  result->add_child ("]");
  return result;
}

// class code::field_expr : public binary_node

std::unique_ptr<node>
code::field_expr::lower (const lowering_options &opts) const
{
  auto result = std::make_unique<compound_node> ();
  result->add_child (get_operand_0 ().lower (opts));
  result->add_child (".");
  result->add_child (get_operand_1 ().lower (opts));
  return result;
}

// class code::infix_binary_op : public binary_node

std::unique_ptr<node>
code::infix_binary_op::lower (const lowering_options &opts) const
{
  pretty_printer pp_infix_symbol;
  pp_printf (&pp_infix_symbol, " %s ", m_symbol);

  auto result = std::make_unique<compound_node> ();
  result->add_child (get_operand_0 ().lower (opts));
  result->add_child (pp_formatted_text (&pp_infix_symbol));
  result->add_child (get_operand_1 ().lower (opts));
  return result;
}

// class inline_code : public compound_node

void
inline_code::print_to_pp (pretty_printer *pp) const
{
  pp_begin_quote (pp, pp_show_color (pp));
  compound_node::print_to_pp (pp);
  pp_end_quote (pp, pp_show_color (pp));
}

std::unique_ptr<node>
inline_code::lower (const lowering_options &opts) const
{
  auto result = std::make_unique<inline_code> ();

  for (size_t idx = 0; idx < get_num_children (); ++idx)
    result->add_child (get_child (idx).lower (opts));

  return result;
}

} // namespace markup

#if CHECKING_P

namespace selftest {

/* Implementation detail of ASSERT_SYMBOL_MARKUP_TEXT_EQ.  */

void
assert_markup_text_eq (const ::selftest::location &loc,
		       const markup::node &node,
		       const markup::lowering_options &opts,
		       const char *expected_str)
{
  auto lowered = node.lower (opts);
  pretty_printer pp;
  lowered->print_to_pp (&pp);
  ASSERT_STREQ_AT (loc, pp_formatted_text (&pp), expected_str);
}

/* Verify that dumping markup::node hierarchies works as expected.  */

static void
test_dumping ()
{
  using namespace diagnostics::markup;

  /* "a + b + (c * d)".  */
  auto sum = std::make_unique<math::sum> ();
  sum->add_child ("a");
  sum->add_child ("b");
  {
    auto nested = std::make_unique<math::product> ();
    nested->add_child ("c");
    nested->add_child ("d");
    sum->add_child (std::move (nested));
  }

  pretty_printer pp;
  text_art::ascii_theme theme;
  dump_node_tree_to_pp (*sum, &theme, &pp);
  ASSERT_STREQ
    (pp_formatted_text (&pp),
     ("math::sum\n"
      "+- text: 'a'\n"
      "+- text: 'b'\n"
      "`- math::product\n"
      "   +- text: 'c'\n"
      "   `- text: 'd'\n"));
}

static void
test_math_nodes ()
{
  using namespace diagnostics::markup;

  {
    auto negation = std::make_unique<math::negation> (make_text_node ("a"));
    ASSERT_MARKUP_TEXT_EQ
      (*negation,
       get_ascii_markup_opts (),
       "-a");
    ASSERT_MARKUP_TEXT_EQ
      (*negation,
       get_unicode_markup_opts (),
       MARKUP_UNICODE_SYMBOL_UNARY_NEGATION "a");
  }

  {
    auto subtraction
      = std::make_unique<math::subtraction> (make_text_node ("a"),
					     make_text_node ("b"));
    ASSERT_MARKUP_TEXT_EQ
      (*subtraction,
       get_ascii_markup_opts (),
       "a - b");
    ASSERT_MARKUP_TEXT_EQ
      (*subtraction,
       get_unicode_markup_opts (),
       "a " MARKUP_UNICODE_SYMBOL_SUBTRACTION " b");
  }

  {
    auto sum = std::make_unique<math::sum> ();
    sum->add_child ("a");
    sum->add_child ("b");
    sum->add_child ("c");
    ASSERT_MARKUP_TEXT_EQ
      (*sum,
       get_ascii_markup_opts (),
       "a + b + c");
  }

  {
    auto product = std::make_unique<math::product> ();
    product->add_child ("a");
    product->add_child ("b");
    product->add_child ("c");
    ASSERT_MARKUP_TEXT_EQ
      (*product,
       get_ascii_markup_opts (),
       "a * b * c");
    ASSERT_MARKUP_TEXT_EQ
      (*product,
       get_unicode_markup_opts (),
       ("a "
	MARKUP_UNICODE_SYMBOL_MULTIPLICATION
	" b "
	MARKUP_UNICODE_SYMBOL_MULTIPLICATION
	" c"));
  }

  {
    auto half_open_interval
      = std::make_unique<math::half_open_interval> (make_text_node ("m"),
						    make_text_node ("n"));
    ASSERT_MARKUP_TEXT_EQ
      (*half_open_interval,
       get_ascii_markup_opts (),
       "[m, n)");
  }
}

static void
test_code_nodes ()
{
  using namespace diagnostics::markup;

  {
    auto element_expr
      = std::make_unique<code::element_expr> (make_text_node ("arr"),
					      make_text_node ("idx"));
    ASSERT_MARKUP_TEXT_EQ
      (*element_expr,
       get_ascii_markup_opts (),
       "arr[idx]");
  }
  {
    auto field_expr
      = std::make_unique<code::field_expr> (make_text_node ("foo"),
					    make_text_node ("m_bar"));
    ASSERT_MARKUP_TEXT_EQ
      (*field_expr,
       get_ascii_markup_opts (),
       "foo.m_bar");
  }
  {
    auto subtraction
      = std::make_unique<code::infix_binary_op> (make_text_node ("a"),
						 "-",
						 make_text_node ("b"));
    ASSERT_MARKUP_TEXT_EQ
      (*subtraction,
       get_ascii_markup_opts (),
       "a - b");
    /* Verify that a code subtraction doesn't use the
       unicode minus character.  */
    ASSERT_MARKUP_TEXT_EQ
      (*subtraction,
       get_unicode_markup_opts (),
       "a - b");
  }
}

static void
test_inline_code ()
{
  using namespace diagnostics::markup;

  auto node = std::make_unique<inline_code> ();
  node->add_child ("sizeof (foo)");
  ASSERT_MARKUP_TEXT_EQ
    (*node,
     get_ascii_markup_opts (),
     "'sizeof (foo)'");
}

/* Verify that we add parentheses correctly when lowering
   math expressions to text.  */

static void
test_parens ()
{
  using namespace diagnostics::markup;

  /* math::sum ("a", "b") --> "a + b".  */
  {
    auto sum = std::make_unique<math::sum> ();
    sum->add_child ("a");
    sum->add_child ("b");
    ASSERT_MARKUP_TEXT_EQ
      (*sum,
       get_ascii_markup_opts (),
       "a + b");
  }

  /* math::sum (math::product ("2", "a"), "b") --> "(2 * a) + b".  */
  {
    auto sum = std::make_unique<math::sum> ();
    {
      auto two_times_a = std::make_unique<math::product> ();
      two_times_a->add_child ("2");
      two_times_a->add_child ("a");
      sum->add_child (std::move (two_times_a));
    }
    sum->add_child ("b");
    ASSERT_MARKUP_TEXT_EQ
      (*sum,
       get_ascii_markup_opts (),
       "(2 * a) + b");
    ASSERT_MARKUP_TEXT_EQ
      (*sum,
       get_unicode_markup_opts (),
       "(2 " MARKUP_UNICODE_SYMBOL_MULTIPLICATION " a) + b");
  }

  /* math::product ("2", math::sum ("a", "b")) --> "(2 * a) + b".  */
  {
    auto product = std::make_unique<math::product> ();
    product->add_child ("2");
    {
      auto a_plus_b = std::make_unique<math::sum> ();
      a_plus_b->add_child ("a");
      a_plus_b->add_child ("b");
      product->add_child (std::move (a_plus_b));
    }
    ASSERT_MARKUP_TEXT_EQ
      (*product,
       get_ascii_markup_opts (),
       "2 * (a + b)");
    ASSERT_MARKUP_TEXT_EQ
      (*product,
       get_unicode_markup_opts (),
       "2 " MARKUP_UNICODE_SYMBOL_MULTIPLICATION " (a + b)");
  }
}

/* Consider marking up this message:
   | If 'x' * 'sizeof(foo)' (64 bytes) > 'SIZE_MAX' (2**64 - 1),
   |    ^^^ ^ ^^^^^^^^^^^^^            ~  ^^^^^^^^   ^^^^^^^^^
   |     |  |     |                    |     |         |
   |     |  |     code                 math  code      math
   |     |  math
   |     code
   | then 'x * sizeof(foo)' overflows.
   |      ^^^^^^^^^^^^^^^^^
   |       |
   |       code
*/
static std::unique_ptr<markup::node>
make_overflow_example ()
{
  using namespace diagnostics::markup;

  auto make_condition = [] () -> auto {
    auto make_lhs = [] () -> auto {
      auto product = std::make_unique<math::product> ();
      auto x = std::make_unique<inline_code> ();
      x->add_child ("x");
      product->add_child (std::move (x));
      product->add_child ("'sizeof(foo)' (64 bytes)"); // TODO: partially code
      return product;
    };
    auto make_rhs = [] () -> auto {
      auto term = std::make_unique<compound_node> ();
      term->add_child
	(std::make_unique<inline_code> (make_text_node ("SIZE_MAX")));
      term->add_child (" (2**64 - 1)"); // TODO: math
      return term;
    };
    auto cond = std::make_unique<compound_node> ();
    cond->add_child (make_lhs ());
    cond->add_child (" > ");
    cond->add_child (make_rhs ());
    return cond;
  };

  auto make_overflowing_expr = [] () -> auto {
    auto product = std::make_unique<code::infix_binary_op>
      (make_text_node ("x"),
       "*",
       make_text_node ("sizeof(foo)"));
    return std::make_unique<inline_code> (std::move (product));
  };

  auto msg = std::make_unique<compound_node> ();
  msg->add_child ("If ");
  msg->add_child (make_condition ());
  msg->add_child (", then ");
  msg->add_child (make_overflowing_expr ());
  msg->add_child (" overflows.");
  return msg;
}

static void
test_overflow_example ()
{
  auto markup = make_overflow_example ();

  ASSERT_MARKUP_TEXT_EQ
    (*markup, get_ascii_markup_opts (),
     ("If 'x' * 'sizeof(foo)' (64 bytes) > 'SIZE_MAX' (2**64 - 1),"
      " then 'x * sizeof(foo)' overflows."));

  ASSERT_MARKUP_TEXT_EQ
    (*markup, get_unicode_markup_opts (),
     ("If "
      "'x' " MARKUP_UNICODE_SYMBOL_MULTIPLICATION " 'sizeof(foo)' (64 bytes)"
      " > 'SIZE_MAX' (2**64 - 1),"
      " then 'x * sizeof(foo)' overflows."));

  // TODO: other formats
}

/* Run all of the selftests within this file.  */

void
markup_cc_tests ()
{
  test_dumping ();
  test_math_nodes ();
  test_code_nodes ();
  test_inline_code ();
  test_parens ();
  test_overflow_example ();
}

markup::lowering_options
get_ascii_markup_opts ()
{
  return markup::lowering_options ();
}

markup::lowering_options
get_unicode_markup_opts ()
{
  markup::lowering_options opts;
  opts.m_use_unicode_symbols = true;
  return opts;
}

} // namespace selftest

#endif /* CHECKING_P */

} // namespace diagnostics
