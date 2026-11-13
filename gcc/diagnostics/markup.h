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

#ifndef GCC_DIAGNOSTICS_MARKUP_H
#define GCC_DIAGNOSTICS_MARKUP_H

#include "selftest.h"
#include "text-art/tree-widget.h"

namespace diagnostics {
namespace markup {

struct lowering_options;

/* Indentation shows inheritance hierarchy.  */
class node;
  class text;
  class unary_node;
    namespace math { class negation; }
  class binary_node;
    namespace math { class subtraction; }
    namespace code { class element_expr; }
    namespace code { class field_expr; }
    namespace code { class infix_binary_op; }
  class compound_node;
    namespace math { class associative_op; }
      namespace math { class sum; }
      namespace math { class product; }
    class inline_code;

enum class kind {
  text,
  compound,

  math_half_open_interval,
  math_negation,
  math_product,
  math_subtraction,
  math_sum,

  code_element_expr,
  code_field_expr,
  code_infix_binary_op,

  inline_code
};

/* Abstract base class for representing text we might want to put
   in a diagnostic as an AST.

   Ideally eventually we could lower this in sink-specific ways
   e.g. text, markdown for SARIF, HTML, etc,
   but that's overkill for now.

   For now, all we need is enough for labels in the analyzer's access diagrams
   to work.  */

class node
{
public:
  virtual ~node () = default;

  virtual const char *
  get_class_name () const = 0;

  virtual kind
  get_kind () const = 0;

  virtual void
  print_to_pp (pretty_printer *pp) const = 0;

  void
  dump () const;

  virtual std::unique_ptr<text_art::tree_widget>
  make_dump_widget (text_art::dump_widget_info &dwi) const = 0;

  virtual const text *
  dyn_cast_text () const { return nullptr; }

  virtual const compound_node *
  dyn_cast_compound_node () const { return nullptr; }

  virtual const math::associative_op *
  dyn_cast_math_associative_op () const { return nullptr; }

  virtual math::negation *
  dyn_cast_math_negation () { return nullptr; }

  virtual std::unique_ptr<node>
  lower (const lowering_options &opts) const = 0;
};

/* Concrete node subclass for a fully-lowered text string.  */

class text : public node
{
public:
  text (std::string str) : m_str (std::move (str))
  {
  }

  const char *
  get_class_name () const final override { return "text"; }

  kind
  get_kind () const final override { return kind::text; }

  void
  print_to_pp (pretty_printer *pp) const final override;

  std::unique_ptr<text_art::tree_widget>
  make_dump_widget (text_art::dump_widget_info &dwi) const final override;

  const text *
  dyn_cast_text () const final override { return this; }

  std::unique_ptr<node>
  lower (const lowering_options &opts) const final override;

  const std::string &
  get_string () const { return m_str; }

private:
  std::string m_str;
};


/* Abstract subclass for nodes that have a single child.  */

class unary_node : public node
{
public:
  void
  print_to_pp (pretty_printer *pp) const final override;

  std::unique_ptr<text_art::tree_widget>
  make_dump_widget (text_art::dump_widget_info &dwi) const final override;

  const node &
  get_operand () const
  {
    return *m_operand;
  }

  std::unique_ptr<node>
  take_operand ()
  {
    return std::move (m_operand);
  }

protected:
  unary_node (std::unique_ptr<node> operand)
  : m_operand (std::move (operand))
  {
    gcc_assert (m_operand);
  }

private:
  std::unique_ptr<node> m_operand;
};

/* Abstract subclass for nodes that have two children.  */

class binary_node : public node
{
public:
  void
  print_to_pp (pretty_printer *pp) const final override;

  std::unique_ptr<text_art::tree_widget>
  make_dump_widget (text_art::dump_widget_info &dwi) const final override;

  const node &
  get_operand_0 () const
  {
    return *m_op0;
  }

  const node &
  get_operand_1 () const
  {
    return *m_op1;
  }

protected:
  binary_node (std::unique_ptr<node> op0,
	       std::unique_ptr<node> op1)
  : m_op0 (std::move (op0)),
    m_op1 (std::move (op1))
  {
  }

private:
  std::unique_ptr<node> m_op0;
  std::unique_ptr<node> m_op1;
};

/* Concrete subclass for nodes that have an arbitrary number of children.  */

class compound_node : public node
{
public:
  compound_node () : m_children () {}

  compound_node (std::vector<std::unique_ptr<node>> children)
  : m_children (std::move (children))
  {
  }

  const char *
  get_class_name () const { return "compound_node"; }

  kind
  get_kind () const override { return kind::compound; }

  void
  print_to_pp (pretty_printer *pp) const override;

  std::unique_ptr<text_art::tree_widget>
  make_dump_widget (text_art::dump_widget_info &dwi) const final override;

  const compound_node *
  dyn_cast_compound_node () const final override { return this; }

  std::unique_ptr<node>
  lower (const lowering_options &opts) const override;

  void
  add_child (std::unique_ptr<node> child)
  {
    gcc_assert (child);
    m_children.push_back (std::move (child));
  }

  void
  add_child (std::string str);

  size_t
  get_num_children () const { return m_children.size (); }

  const node &
  get_child (size_t idx) const { return *m_children[idx]; }

private:
  std::vector<std::unique_ptr<node>> m_children;
};

namespace math {

/* Abstract subclass for nodes that represent an associative sequence
   of infix binary operations.  */

class associative_op : public compound_node
{
public:
  const associative_op *
  dyn_cast_math_associative_op () const final override { return this; }

  std::unique_ptr<node>
  lower (const lowering_options &opts) const final override;

  virtual const char *
  get_symbol (bool use_unicode) const = 0;

protected:
  associative_op ()
  : compound_node ()
  {
  }

  associative_op (std::vector<std::unique_ptr<node>> operands)
  : compound_node (std::move (operands))
  {
  }
};

/* Concrete node subclass for a mathematical sum.
   The children are the addends; there can be more than 2.  */

class sum : public associative_op
{
public:
  sum () : associative_op () {}

  sum (std::vector<std::unique_ptr<node>> addends)
  : associative_op (std::move (addends))
  {
  }

  const char *
  get_class_name () const final override
  {
    return "math::sum";
  }

  kind
  get_kind () const final override { return kind::math_sum; }

  const char *
  get_symbol (bool use_unicode) const final override;
};

/* Concrete node subclass for a mathematical product.
   The children are the multiplicands; there can be more than 2.  */

class product : public associative_op
{
public:
  product () : associative_op () {}

  product (std::vector<std::unique_ptr<node>> multiplicands)
  : associative_op (std::move (multiplicands))
  {
  }

  const char *
  get_class_name () const final override
  {
    return "math::product";
  }

  kind
  get_kind () const final override { return kind::math_product; }

  const char *
  get_symbol (bool use_unicode) const final override;
};

/* Concrete node subclass for mathematical unary negation.  */

class negation : public unary_node
{
public:
  negation (std::unique_ptr<node> operand) : unary_node (std::move (operand)) {}

  negation *
  dyn_cast_math_negation () final override { return this; }

  const char *
  get_class_name () const final override
  {
    return "math::negation";
  }

  kind
  get_kind () const final override { return kind::math_negation; }

  std::unique_ptr<node>
  lower (const lowering_options &opts) const final override;
};

/* Concrete node subclass for mathematical subtraction.  */

class subtraction : public binary_node
{
public:
  subtraction (std::unique_ptr<node> op0,
	       std::unique_ptr<node> op1)
  : binary_node (std::move (op0),
		 std::move (op1))
  {
  }

  const char *
  get_class_name () const { return "math::subtraction"; }

  kind
  get_kind () const final override { return kind::math_subtraction; }

  std::unique_ptr<node>
  lower (const lowering_options &opts) const final override;
};

/* Concrete node subclass for a half-open interval
   of the form [start, end).  */

class half_open_interval : public binary_node
{
public:
  half_open_interval (std::unique_ptr<node> start,
		      std::unique_ptr<node> next)
  : binary_node (std::move (start),
		 std::move (next))
  {
  }

  const char *
  get_class_name () const final override
  {
    return "math::half_open_interval";
  }

  kind
  get_kind () const final override { return kind::math_half_open_interval; }

  std::unique_ptr<node>
  lower (const lowering_options &opts) const final override;
};

} // namespace diagnostics::markup::math

namespace code {

class element_expr : public binary_node
{
public:
  element_expr (std::unique_ptr<node> array_node,
		std::unique_ptr<node> index_node)
  : binary_node (std::move (array_node), std::move (index_node))
  {
  }

  const char *
  get_class_name () const final override
  {
    return "code::element_expr";
  }

  kind
  get_kind () const final override { return kind::code_element_expr; }

  std::unique_ptr<node>
  lower (const lowering_options &opts) const final override;
};

class field_expr : public binary_node
{
public:
  field_expr (std::unique_ptr<node> struct_or_union_node,
	      std::unique_ptr<node> field_node)
  : binary_node (std::move (struct_or_union_node), std::move (field_node))
  {
  }

  const char *
  get_class_name () const final override
  {
    return "code::field_expr";
  }

  kind
  get_kind () const final override { return kind::code_field_expr; }

  std::unique_ptr<node>
  lower (const lowering_options &opts) const final override;
};

class infix_binary_op : public binary_node
{
public:
  infix_binary_op (std::unique_ptr<node> op0,
		   const char *symbol,
		   std::unique_ptr<node> op1)
  : binary_node (std::move (op0), std::move (op1)),
    m_symbol (symbol)
  {
  }

  const char *
  get_class_name () const final override
  {
    return "code::infix_binary_op";
  }

  kind
  get_kind () const final override { return kind::code_infix_binary_op; }

  std::unique_ptr<node>
  lower (const lowering_options &opts) const final override;

private:
  const char *m_symbol;
};

} // namespace diagnostics::markup::code

/* Concrete subclass for a run of inline code within text.
   Adds quotes around its children when printed.  */

class inline_code : public compound_node
{
public:
  inline_code () {}

  inline_code (std::unique_ptr<node> child)
  : compound_node {}
  {
    add_child (std::move (child));
  }

  const char *
  get_class_name () const final override
  {
    return "inline_code";
  }

  kind
  get_kind () const final override { return kind::inline_code; }

  void
  print_to_pp (pretty_printer *pp) const override;

  std::unique_ptr<node>
  lower (const lowering_options &opts) const final override;
};

/* Helpers for making nodes.  */

inline std::unique_ptr<text>
make_text_node (std::string str)
{
  return std::make_unique<text> (std::move (str));
}

/* Lowering.  */

struct lowering_options
{
  lowering_options ()
  : m_use_unicode_symbols (false)
  {
  }

  bool m_use_unicode_symbols;
};

} // namespace diagnostics::markup

#if CHECKING_P

namespace selftest {

/* Implementation detail of ASSERT_SYMBOL_MARKUP_TEXT_EQ.  */

void
assert_markup_text_eq (const ::selftest::location &loc,
		       const markup::node &,
		       const markup::lowering_options &,
		       const char *expected_str);

/* Assert that MARKUP_NODE lowered to plain text is EXPECTED_STR.  */

#define ASSERT_MARKUP_TEXT_EQ_AT(LOC, MARKUP_NODE, LOWERING_OPTS, EXPECTED_STR) \
  SELFTEST_BEGIN_STMT							\
  ::diagnostics::selftest::assert_markup_text_eq			\
    (LOC, (MARKUP_NODE),						\
     (LOWERING_OPTS), (EXPECTED_STR));					\
  SELFTEST_END_STMT

#define ASSERT_MARKUP_TEXT_EQ(MARKUP_NODE, LOWERING_OPTS, EXPECTED_STR) \
  SELFTEST_BEGIN_STMT							\
  ::diagnostics::selftest::assert_markup_text_eq			\
    (SELFTEST_LOCATION, (MARKUP_NODE),					\
     (LOWERING_OPTS), (EXPECTED_STR));					\
  SELFTEST_END_STMT

extern markup::lowering_options
get_ascii_markup_opts ();

extern markup::lowering_options
get_unicode_markup_opts ();

} // namespace diagnostics::selftest

#endif /* #if CHECKING_P */

} // namespace diagnostics

/* Various string literals for unicode symbols.  */

/* "−": U+2212: MINUS SIGN. */
#define MARKUP_UNICODE_SYMBOL_UNARY_NEGATION "\u2212"
#define MARKUP_UNICODE_SYMBOL_SUBTRACTION "\u2212"

/* "×: U+00D7 MULTIPLICATION SIGN. */
#define MARKUP_UNICODE_SYMBOL_MULTIPLICATION "\u00D7"

#endif /* GCC_DIAGNOSTICS_MARKUP_H */
