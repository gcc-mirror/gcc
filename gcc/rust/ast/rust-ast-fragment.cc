// Copyright (C) 2020-2025 Free Software Foundation, Inc.

// This file is part of GCC.

// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.

// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.

// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include "rust-ast-fragment.h"

namespace Rust {
namespace AST {

Fragment::Fragment (FragmentKind kind, std::vector<SingleASTNode> nodes,
		    std::vector<std::unique_ptr<AST::Token>> tokens)
  : kind (kind), nodes (std::move (nodes)), tokens (std::move (tokens))
{}

Fragment::Fragment (Fragment const &other) : kind (other.get_kind ())
{
  *this = other;
}

Fragment &
Fragment::operator= (Fragment const &other)
{
  kind = other.get_kind ();

  nodes.clear ();
  nodes.reserve (other.nodes.size ());
  for (auto &n : other.nodes)
    nodes.push_back (n);

  tokens.clear ();
  tokens.reserve (other.tokens.size ());
  for (auto &t : other.tokens)
    tokens.emplace_back (t->clone_token ());

  return *this;
}

Fragment
Fragment::create_error ()
{
  return Fragment (FragmentKind::Error, {}, {});
}

Fragment
Fragment::create_empty ()
{
  return Fragment (FragmentKind::Complete, {}, {});
}

Fragment::Fragment (std::vector<AST::SingleASTNode> nodes,
		    std::vector<std::unique_ptr<AST::Token>> tokens)
  : kind (FragmentKind::Complete), nodes (std::move (nodes)),
    tokens (std::move (tokens))
{}

Fragment::Fragment (std::vector<AST::SingleASTNode> nodes,
		    std::unique_ptr<AST::Token> token)
  : kind (FragmentKind::Complete), nodes (std::move (nodes))
{
  tokens.emplace_back (std::move (token));
}

std::vector<SingleASTNode> &
Fragment::get_nodes ()
{
  return nodes;
}

std::vector<std::unique_ptr<AST::Token>> &
Fragment::get_tokens ()
{
  return tokens;
}

FragmentKind
Fragment::get_kind () const
{
  return kind;
}

bool
Fragment::is_error () const
{
  return get_kind () == FragmentKind::Error;
}

bool
Fragment::should_expand () const
{
  return !is_error ();
}

bool
Fragment::is_expression_fragment () const
{
  return is_single_fragment_of_kind (SingleASTNode::Kind::Expr);
}

bool
Fragment::is_type_fragment () const
{
  return is_single_fragment_of_kind (SingleASTNode::Kind::Type);
}

bool
Fragment::is_pattern_fragment () const
{
  return is_single_fragment_of_kind (SingleASTNode::Kind::Pattern);
}

std::unique_ptr<Expr>
Fragment::take_expression_fragment ()
{
  assert_single_fragment (SingleASTNode::Kind::Expr);
  return nodes[0].take_expr ();
}

std::unique_ptr<Type>
Fragment::take_type_fragment ()
{
  assert_single_fragment (SingleASTNode::Kind::Type);
  return nodes[0].take_type ();
}

std::unique_ptr<Pattern>
Fragment::take_pattern_fragment ()
{
  assert_single_fragment (SingleASTNode::Kind::Pattern);
  return nodes[0].take_pattern ();
}

void
Fragment::accept_vis (ASTVisitor &vis)
{
  for (auto &node : nodes)
    node.accept_vis (vis);
}

bool
Fragment::is_single_fragment () const
{
  return nodes.size () == 1;
}

bool
Fragment::is_single_fragment_of_kind (SingleASTNode::Kind expected) const
{
  return is_single_fragment () && nodes[0].get_kind () == expected;
}

void
Fragment::assert_single_fragment (SingleASTNode::Kind expected) const
{
  static const std::map<SingleASTNode::Kind, const char *> str_map = {
    {SingleASTNode::Kind::Assoc, "associated item"},
    {SingleASTNode::Kind::Item, "item"},
    {SingleASTNode::Kind::Type, "type"},
    {SingleASTNode::Kind::Expr, "expr"},
    {SingleASTNode::Kind::Stmt, "stmt"},
    {SingleASTNode::Kind::Extern, "extern"},
    {SingleASTNode::Kind::Pattern, "pattern"},
  };

  auto actual = nodes[0].get_kind ();
  auto fail = false;

  if (!is_single_fragment ())
    {
      rust_error_at (UNDEF_LOCATION, "fragment is not single");
      fail = true;
    }

  if (actual != expected)
    {
      rust_error_at (
	UNDEF_LOCATION,
	"invalid fragment operation: expected %qs node, got %qs node",
	str_map.find (expected)->second,
	str_map.find (nodes[0].get_kind ())->second);
      fail = true;
    }

  rust_assert (!fail);
}

} // namespace AST
} // namespace Rust
