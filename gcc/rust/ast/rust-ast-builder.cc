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

#include "rust-ast-builder.h"
#include "rust-ast-full.h"

namespace Rust {
namespace AST {

std::unique_ptr<Expr>
AstBuilder::call (std::unique_ptr<Expr> &&path,
		  std::vector<std::unique_ptr<Expr>> &&args)
{
  return std::unique_ptr<Expr> (
    new CallExpr (std::move (path), std::move (args), {}, loc));
}

std::unique_ptr<Expr>
AstBuilder::identifier (std::string name)
{
  return std::unique_ptr<Expr> (new IdentifierExpr (name, {}, loc));
}

std::unique_ptr<Expr>
AstBuilder::tuple_idx (std::string receiver, int idx)
{
  return std::unique_ptr<Expr> (
    new TupleIndexExpr (identifier (receiver), idx, {}, loc));
}

FunctionQualifiers
AstBuilder::fn_qualifiers ()
{
  return FunctionQualifiers (loc, Async::No, Const::No, Unsafety::Normal);
}

PathExprSegment
AstBuilder::path_segment (std::string seg)
{
  return PathExprSegment (PathIdentSegment (seg, loc), loc);
}

std::unique_ptr<TypePathSegment>
AstBuilder::type_path_segment (std::string seg)
{
  return std::unique_ptr<TypePathSegment> (
    new TypePathSegment (seg, false, loc));
}

std::unique_ptr<Type>
AstBuilder::single_type_path (std::string type)
{
  auto segments = std::vector<std::unique_ptr<TypePathSegment>> ();
  segments.emplace_back (type_path_segment (type));

  return std::unique_ptr<Type> (new TypePath (std::move (segments), loc));
}

PathInExpression
AstBuilder::path_in_expression (std::vector<std::string> &&segments)
{
  auto path_segments = std::vector<PathExprSegment> ();
  for (auto &seg : segments)
    path_segments.emplace_back (path_segment (seg));

  return PathInExpression (std::move (path_segments), {}, loc);
}

std::unique_ptr<Expr>
AstBuilder::block (std::vector<std::unique_ptr<Stmt>> &&stmts,
		   std::unique_ptr<Expr> &&tail_expr)
{
  return std::unique_ptr<Expr> (new BlockExpr (std::move (stmts),
					       std::move (tail_expr), {}, {},
					       LoopLabel::error (), loc, loc));
}

std::unique_ptr<Stmt>
AstBuilder::let (std::unique_ptr<Pattern> pattern, std::unique_ptr<Type> type,
		 std::unique_ptr<Expr> init)
{
  return std::unique_ptr<Stmt> (new LetStmt (std::move (pattern),
					     std::move (init), std::move (type),
					     {}, loc));
}

std::unique_ptr<Expr>
AstBuilder::ref (std::unique_ptr<Expr> &&of, bool mut)
{
  return std::unique_ptr<Expr> (
    new BorrowExpr (std::move (of), mut, /* is double */ false, {}, loc));
}

std::unique_ptr<Expr>
AstBuilder::deref (std::unique_ptr<Expr> &&of)
{
  return std::unique_ptr<Expr> (new DereferenceExpr (std::move (of), {}, loc));
}

std::unique_ptr<Expr>
AstBuilder::struct_expr_struct (std::string struct_name)
{
  return std::unique_ptr<Expr> (
    new StructExprStruct (path_in_expression ({struct_name}), {}, {}, loc));
}

std::unique_ptr<Expr>
AstBuilder::struct_expr (std::string struct_name,
			 std::vector<std::unique_ptr<StructExprField>> &&fields)
{
  return std::unique_ptr<Expr> (
    new StructExprStructFields (path_in_expression ({struct_name}),
				std::move (fields), loc));
}

std::unique_ptr<StructExprField>
AstBuilder::struct_expr_field (std::string field_name,
			       std::unique_ptr<Expr> &&value)
{
  return std::unique_ptr<StructExprField> (
    new StructExprFieldIdentifierValue (field_name, std::move (value), loc));
}

std::unique_ptr<Expr>
AstBuilder::field_access (std::unique_ptr<Expr> &&instance, std::string field)
{
  return std::unique_ptr<Expr> (
    new FieldAccessExpr (std::move (instance), field, {}, loc));
}

std::unique_ptr<Pattern>
AstBuilder::wildcard ()
{
  return std::unique_ptr<Pattern> (new WildcardPattern (loc));
}

} // namespace AST
} // namespace Rust
