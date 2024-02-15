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

#ifndef AST_BUILDER_H
#define AST_BUILDER_H

#include "rust-ast-full.h"

namespace Rust {
namespace AST {

// TODO: Use this builder when expanding regular macros
/* Builder class with helper methods to create AST nodes. This builder is
 * tailored towards generating multiple AST nodes from a single location, and
 * may not be suitable to other purposes */
class AstBuilder
{
public:
  AstBuilder (location_t loc) : loc (loc) {}

  /* Create an identifier expression (`variable`) */
  std::unique_ptr<Expr> identifier (std::string name);

  /* Create a tuple index expression (`receiver.0`) */
  std::unique_ptr<Expr> tuple_idx (std::string receiver, int idx);

  /* Create a reference to an expression (`&of`) */
  std::unique_ptr<Expr> ref (std::unique_ptr<Expr> &&of, bool mut = false);

  /* Create a dereference of an expression (`*of`) */
  std::unique_ptr<Expr> deref (std::unique_ptr<Expr> &&of);

  /* Create a block with an optional tail expression */
  std::unique_ptr<Expr> block (std::vector<std::unique_ptr<Stmt>> &&stmts,
			       std::unique_ptr<Expr> &&tail_expr = nullptr);

  /* Create a let binding with an optional type and initializer (`let <name> :
   * <type> = <init>`) */
  std::unique_ptr<Stmt> let (std::unique_ptr<Pattern> pattern,
			     std::unique_ptr<Type> type = nullptr,
			     std::unique_ptr<Expr> init = nullptr);

  /**
   * Create a call expression to a function, struct or enum variant, given its
   * arguments (`path(arg0, arg1, arg2)`)
   */
  std::unique_ptr<Expr> call (std::unique_ptr<Expr> &&path,
			      std::vector<std::unique_ptr<Expr>> &&args);

  /* Empty function qualifiers, with no specific qualifiers */
  FunctionQualifiers fn_qualifiers ();

  /* Create a single path segment from one string */
  PathExprSegment path_segment (std::string seg);

  /* And similarly for type path segments */
  std::unique_ptr<TypePathSegment> type_path_segment (std::string seg);

  /* Create a Type from a single string - the most basic kind of type in our AST
   */
  std::unique_ptr<Type> single_type_path (std::string type);

  /**
   * Create a path in expression from multiple segments (`Clone::clone`). You
   * do not need to separate the segments using `::`, you can simply provide a
   * vector of strings to the functions which will get turned into path segments
   */
  PathInExpression path_in_expression (std::vector<std::string> &&segments);

  /* Create a struct expression for unit structs (`S`) */
  std::unique_ptr<Expr> struct_expr_struct (std::string struct_name);

  /**
   * Create an expression for struct instantiation with fields (`S { a, b: c }`)
   */
  std::unique_ptr<Expr>
  struct_expr (std::string struct_name,
	       std::vector<std::unique_ptr<StructExprField>> &&fields);

  /* Create a field expression for struct instantiation (`field_name: value`) */
  std::unique_ptr<StructExprField>
  struct_expr_field (std::string field_name, std::unique_ptr<Expr> &&value);

  /* Create a field access expression (`instance.field`) */
  std::unique_ptr<Expr> field_access (std::unique_ptr<Expr> &&instance,
				      std::string field);

  /* Create a wildcard pattern (`_`) */
  std::unique_ptr<Pattern> wildcard ();

private:
  /**
   * Location of the generated AST nodes
   */
  location_t loc;
};

} // namespace AST
} // namespace Rust

#endif // AST_BUILDER_H
