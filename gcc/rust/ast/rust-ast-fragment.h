// Copyright (C) 2020-2024 Free Software Foundation, Inc.

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

#ifndef RUST_AST_FRAGMENT_H
#define RUST_AST_FRAGMENT_H

#include "rust-ast.h"
#include "rust-system.h"

namespace Rust {
namespace AST {

enum class FragmentKind
{
  /**
   * A completely expanded AST Fragment. This signifies that all
   * `SingleASTNode`s in the `nodes` vector are valid.
   *
   * Note that this doesn't imply that the expansion is "done". One of the
   * expanded nodes could very well be another macro invocation
   */
  Complete,
  /**
   * An error fragment.
   */
  Error,
};

/**
 * An AST Fragment. Previously named `ASTFragment`.
 *
 * Basically, a "fragment" that can be incorporated into the AST, created as
 * a result of macro expansion. Really annoying to work with due to the fact
 * that macros can really expand to anything. As such, horrible representation
 * at the moment.
 */
class Fragment
{
public:
  Fragment (Fragment const &other);
  Fragment &operator= (Fragment const &other);

  /**
   * Create an error fragment
   */
  static Fragment create_error ();

  /**
   * Create an empty fragment
   */
  static Fragment create_empty ();

  /**
   * Create a complete AST fragment
   */
  Fragment (std::vector<AST::SingleASTNode> nodes,
	    std::vector<std::unique_ptr<AST::Token>> tokens);

  /**
   * Create a complete AST fragment made of a single token
   */
  Fragment (std::vector<AST::SingleASTNode> nodes,
	    std::unique_ptr<AST::Token> tok);

  FragmentKind get_kind () const;
  std::vector<SingleASTNode> &get_nodes ();
  std::vector<std::unique_ptr<AST::Token>> &get_tokens ();

  bool is_error () const;
  bool should_expand () const;

  bool is_expression_fragment () const;
  bool is_type_fragment () const;

  std::unique_ptr<Expr> take_expression_fragment ();
  std::unique_ptr<Type> take_type_fragment ();

  void accept_vis (ASTVisitor &vis);

private:
  Fragment (FragmentKind kind, std::vector<SingleASTNode> nodes,
	    std::vector<std::unique_ptr<AST::Token>> tokens);

  FragmentKind kind;

  /**
   * Basic idea: essentially, a vector of tagged unions of different AST node
   * types. Now, this could actually be stored without a tagged union if the
   * different AST node types had a unified parent, but that would create
   * issues with the diamond problem or significant performance penalties. So
   * a tagged union had to be used instead. A vector is used to represent the
   * ability for a macro to expand to two statements, for instance.
   */
  std::vector<SingleASTNode> nodes;

  /**
   * The tokens associated with an AST fragment. This vector represents the
   * actual tokens of the various nodes that are part of the fragment.
   */
  std::vector<std::unique_ptr<AST::Token>> tokens;

  /**
   * We need to make a special case for Expression and Type fragments as only
   * one Node will be extracted from the `nodes` vector
   */
  bool is_single_fragment () const;
  bool is_single_fragment_of_kind (SingleASTNode::NodeType expected) const;
  void assert_single_fragment (SingleASTNode::NodeType expected) const;
};

/**
 * This is the type for transcriber functions found in
 * rust-macro-builtins.{h,cc}.
 */
using MacroTranscriberFunc
  = std::function<tl::optional<Fragment> (location_t, MacroInvocData &)>;

} // namespace AST
} // namespace Rust

#endif // !RUST_AST_FRAGMENT_H
