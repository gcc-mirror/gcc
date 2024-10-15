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

#ifndef RUST_HIR_EXPR_ABSTRACT_H
#define RUST_HIR_EXPR_ABSTRACT_H

#include "rust-ast.h"
#include "rust-hir-visitable.h"
#include "rust-hir-node.h"

namespace Rust {
namespace HIR {

// Base expression HIR node - abstract
class Expr : public Node, virtual public FullVisitable
{
public:
  using FullVisitable::accept_vis;

protected:
  AST::AttrVec outer_attrs;
  Analysis::NodeMapping mappings;

public:
  enum BlockType
  {
    WITH_BLOCK,
    WITHOUT_BLOCK,
  };

  enum ExprType
  {
    Lit,
    Operator,
    Grouped,
    Array,
    ArrayIndex,
    Tuple,
    TupleIdx,
    Struct,
    Call,
    MethodCall,
    FieldAccess,
    Closure,
    Block,
    Continue,
    Break,
    Range,
    Return,
    UnsafeBlock,
    BaseLoop,
    If,
    IfLet,
    Match,
    Await,
    AsyncBlock,
    Path,
    InlineAsm,
  };

  BaseKind get_hir_kind () override final { return Node::BaseKind::EXPR; }

  const AST::AttrVec &get_outer_attrs () const { return outer_attrs; }

  // Unique pointer custom clone function
  std::unique_ptr<Expr> clone_expr () const
  {
    return std::unique_ptr<Expr> (clone_expr_impl ());
  }

  // TODO: make pure virtual if move out outer attributes to derived classes
  virtual std::string as_string () const;

  virtual ~Expr () {}

  virtual location_t get_locus () const = 0;

  const Analysis::NodeMapping &get_mappings () const { return mappings; }

  // Clone function implementation as pure virtual method
  virtual Expr *clone_expr_impl () const = 0;

  virtual BlockType get_block_expr_type () const = 0;

  virtual ExprType get_expression_type () const = 0;

  virtual void accept_vis (HIRExpressionVisitor &vis) = 0;

protected:
  // Constructor
  Expr (Analysis::NodeMapping mappings,
	AST::AttrVec outer_attribs = AST::AttrVec ());

  // TODO: think of less hacky way to implement this kind of thing
  // Sets outer attributes.
  void set_outer_attrs (AST::AttrVec outer_attrs_to_set)
  {
    outer_attrs = std::move (outer_attrs_to_set);
  }
};

// HIR node for an expression without an accompanying block - abstract
class ExprWithoutBlock : public Expr
{
protected:
  // Constructor
  ExprWithoutBlock (Analysis::NodeMapping mappings,
		    AST::AttrVec outer_attribs = AST::AttrVec ());

  // pure virtual clone implementation
  virtual ExprWithoutBlock *clone_expr_without_block_impl () const = 0;

  /* Save having to specify two clone methods in derived classes by making expr
   * clone return exprwithoutblock clone. Hopefully won't affect performance too
   * much. */
  ExprWithoutBlock *clone_expr_impl () const override
  {
    return clone_expr_without_block_impl ();
  }

public:
  // Unique pointer custom clone function
  std::unique_ptr<ExprWithoutBlock> clone_expr_without_block () const
  {
    return std::unique_ptr<ExprWithoutBlock> (clone_expr_without_block_impl ());
  }

  BlockType get_block_expr_type () const final override
  {
    return BlockType::WITHOUT_BLOCK;
  };
};

// Base path expression HIR node - abstract
class PathExpr : public ExprWithoutBlock
{
protected:
  PathExpr (Analysis::NodeMapping mappings, AST::AttrVec outer_attribs)
    : ExprWithoutBlock (std::move (mappings), std::move (outer_attribs))
  {}

public:
  /* Replaces the outer attributes of this path expression with the given outer
   * attributes. */
  void replace_outer_attrs (AST::AttrVec outer_attrs)
  {
    set_outer_attrs (std::move (outer_attrs));
  }

  ExprType get_expression_type () const final override
  {
    return ExprType::Path;
  }
};

} // namespace HIR
} // namespace Rust

#endif
