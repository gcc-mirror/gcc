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

#include "rust-ast-builder.h"
#include "rust-ast-full-decls.h"
#include "rust-ast-full.h"
#include "rust-expr.h"
#include "rust-token.h"
#include "rust-make-unique.h"

namespace Rust {
namespace AST {

std::unique_ptr<Expr>
Builder::literal_string (std::string &&content) const
{
  return std::unique_ptr<Expr> (
    new AST::LiteralExpr (std::move (content), Literal::LitType::STRING,
			  PrimitiveCoreType::CORETYPE_STR, {}, loc));
}

std::unique_ptr<Expr>
Builder::call (std::unique_ptr<Expr> &&path,
	       std::vector<std::unique_ptr<Expr>> &&args) const
{
  return std::unique_ptr<Expr> (
    new CallExpr (std::move (path), std::move (args), {}, loc));
}

std::unique_ptr<Expr>
Builder::array (std::vector<std::unique_ptr<Expr>> &&members) const
{
  auto elts = Rust::make_unique<ArrayElemsValues> (std::move (members), loc);

  return std::unique_ptr<Expr> (new ArrayExpr (std::move (elts), {}, {}, loc));
}

std::unique_ptr<Expr>
Builder::identifier (std::string name) const
{
  return std::unique_ptr<Expr> (new IdentifierExpr (name, {}, loc));
}

std::unique_ptr<Expr>
Builder::tuple_idx (std::string receiver, int idx) const
{
  return std::unique_ptr<Expr> (
    new TupleIndexExpr (identifier (receiver), idx, {}, loc));
}

FunctionQualifiers
Builder::fn_qualifiers () const
{
  return FunctionQualifiers (loc, Async::No, Const::No, Unsafety::Normal);
}

PathExprSegment
Builder::path_segment (std::string seg) const
{
  return PathExprSegment (PathIdentSegment (seg, loc), loc);
}

std::unique_ptr<TypePathSegment>
Builder::type_path_segment (std::string seg) const
{
  return std::unique_ptr<TypePathSegment> (
    new TypePathSegment (seg, false, loc));
}

std::unique_ptr<Type>
Builder::single_type_path (std::string type) const
{
  auto segments = std::vector<std::unique_ptr<TypePathSegment>> ();
  segments.emplace_back (type_path_segment (type));

  return std::unique_ptr<Type> (new TypePath (std::move (segments), loc));
}

PathInExpression
Builder::path_in_expression (std::vector<std::string> &&segments) const
{
  auto path_segments = std::vector<PathExprSegment> ();
  for (auto &seg : segments)
    path_segments.emplace_back (path_segment (seg));

  return PathInExpression (std::move (path_segments), {}, loc);
}

std::unique_ptr<Expr>
Builder::block (std::vector<std::unique_ptr<Stmt>> &&stmts,
		std::unique_ptr<Expr> &&tail_expr) const
{
  return std::unique_ptr<Expr> (new BlockExpr (std::move (stmts),
					       std::move (tail_expr), {}, {},
					       LoopLabel::error (), loc, loc));
}

std::unique_ptr<Stmt>
Builder::let (std::unique_ptr<Pattern> pattern, std::unique_ptr<Type> type,
	      std::unique_ptr<Expr> init) const
{
  return std::unique_ptr<Stmt> (new LetStmt (std::move (pattern),
					     std::move (init), std::move (type),
					     {}, loc));
}

std::unique_ptr<Expr>
Builder::ref (std::unique_ptr<Expr> &&of, bool mut) const
{
  return std::unique_ptr<Expr> (
    new BorrowExpr (std::move (of), mut, /* is double */ false, {}, loc));
}

std::unique_ptr<Expr>
Builder::deref (std::unique_ptr<Expr> &&of) const
{
  return std::unique_ptr<Expr> (new DereferenceExpr (std::move (of), {}, loc));
}

std::unique_ptr<Expr>
Builder::struct_expr_struct (std::string struct_name) const
{
  return std::unique_ptr<Expr> (
    new StructExprStruct (path_in_expression ({struct_name}), {}, {}, loc));
}

std::unique_ptr<Expr>
Builder::struct_expr (
  std::string struct_name,
  std::vector<std::unique_ptr<StructExprField>> &&fields) const
{
  return std::unique_ptr<Expr> (
    new StructExprStructFields (path_in_expression ({struct_name}),
				std::move (fields), loc));
}

std::unique_ptr<StructExprField>
Builder::struct_expr_field (std::string field_name,
			    std::unique_ptr<Expr> &&value) const
{
  return std::unique_ptr<StructExprField> (
    new StructExprFieldIdentifierValue (field_name, std::move (value), loc));
}

std::unique_ptr<Expr>
Builder::field_access (std::unique_ptr<Expr> &&instance,
		       std::string field) const
{
  return std::unique_ptr<Expr> (
    new FieldAccessExpr (std::move (instance), field, {}, loc));
}

std::unique_ptr<Pattern>
Builder::wildcard () const
{
  return std::unique_ptr<Pattern> (new WildcardPattern (loc));
}

} // namespace AST
} // namespace Rust
