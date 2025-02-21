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

#ifndef AST_BUILDER_H
#define AST_BUILDER_H

#include "rust-ast-full.h"
#include "rust-expr.h"
#include "rust-ast.h"
#include "rust-item.h"
#include "rust-operators.h"

namespace Rust {
namespace AST {

template <typename T>
std::vector<std::unique_ptr<T>>
vec (std::unique_ptr<T> &&t)
{
  auto v = std::vector<std::unique_ptr<T>> ();

  v.emplace_back (std::move (t));

  return v;
}

template <typename T>
std::vector<std::unique_ptr<T>>
vec (std::unique_ptr<T> &&t1, std::unique_ptr<T> &&t2)
{
  auto v = std::vector<std::unique_ptr<T>> ();

  v.emplace_back (std::move (t1));
  v.emplace_back (std::move (t2));

  return v;
}

/* Pointer-ify something */
template <typename T>
static std::unique_ptr<T>
ptrify (T value)
{
  return std::unique_ptr<T> (new T (value));
}

// TODO: Use this builder when expanding regular macros
/* Builder class with helper methods to create AST nodes. This builder is
 * tailored towards generating multiple AST nodes from a single location, and
 * may not be suitable to other purposes */
class Builder
{
public:
  Builder (location_t loc) : loc (loc) {}

  /* Create an expression statement from an expression */
  std::unique_ptr<Stmt> statementify (std::unique_ptr<Expr> &&value,
				      bool semicolon_followed = true) const;

  /* Create a string literal expression ("content") */
  std::unique_ptr<Expr> literal_string (std::string &&content) const;

  /* Create a boolean literal expression (true) */
  std::unique_ptr<Expr> literal_bool (bool b) const;

  /* Create an identifier expression (`variable`) */
  std::unique_ptr<Expr> identifier (std::string name) const;
  std::unique_ptr<Pattern> identifier_pattern (std::string name,
					       bool mut = false) const;

  /* Create a tuple index expression (`receiver.0`) */
  std::unique_ptr<Expr> tuple_idx (std::string receiver, int idx) const;

  /* Create a tuple expression (`(a1, a2, a3)`) */
  std::unique_ptr<Expr> tuple (std::vector<std::unique_ptr<Expr>> &&values
			       = {}) const;

  /* Create a reference to an expression (`&of`) */
  std::unique_ptr<Expr> ref (std::unique_ptr<Expr> &&of,
			     bool mut = false) const;

  /* Create a dereference of an expression (`*of`) */
  std::unique_ptr<Expr> deref (std::unique_ptr<Expr> &&of) const;

  /* Build a comparison expression (`lhs == rhs`) */
  std::unique_ptr<Expr> comparison_expr (std::unique_ptr<Expr> &&lhs,
					 std::unique_ptr<Expr> &&rhs,
					 ComparisonOperator op) const;

  /* Build a lazy boolean operator expression (`lhs && rhs`) */
  std::unique_ptr<Expr> boolean_operation (std::unique_ptr<Expr> &&lhs,
					   std::unique_ptr<Expr> &&rhs,
					   LazyBooleanOperator op) const;

  /* Create a block with an optional tail expression */
  std::unique_ptr<BlockExpr> block (std::vector<std::unique_ptr<Stmt>> &&stmts,
				    std::unique_ptr<Expr> &&tail_expr
				    = nullptr) const;
  std::unique_ptr<BlockExpr> block (tl::optional<std::unique_ptr<Stmt>> &&stmt,
				    std::unique_ptr<Expr> &&tail_expr
				    = nullptr) const;
  /* Create an empty block */
  std::unique_ptr<BlockExpr> block () const;

  /* Create an early return expression with an optional expression */
  std::unique_ptr<Expr> return_expr (std::unique_ptr<Expr> &&to_return
				     = nullptr);

  /* Create a let binding with an optional type and initializer (`let <name> :
   * <type> = <init>`) */
  std::unique_ptr<Stmt> let (std::unique_ptr<Pattern> &&pattern,
			     std::unique_ptr<Type> &&type = nullptr,
			     std::unique_ptr<Expr> &&init = nullptr) const;

  /**
   * Create a call expression to a function, struct or enum variant, given its
   * arguments (`path(arg0, arg1, arg2)`)
   */
  std::unique_ptr<Expr> call (std::unique_ptr<Expr> &&path,
			      std::vector<std::unique_ptr<Expr>> &&args
			      = {}) const;
  std::unique_ptr<Expr> call (std::unique_ptr<Expr> &&path,
			      std::unique_ptr<Expr> &&arg) const;

  /**
   * Create an array expression (`[member0, member1, member2]`)
   */
  std::unique_ptr<Expr>
  array (std::vector<std::unique_ptr<Expr>> &&members) const;

  /* Create a qualified path in expression (`<type as Trait>::seg::expr`) */
  std::unique_ptr<Expr>
  qualified_path_in_expression (std::unique_ptr<Type> &&type, TypePath trait,
				PathExprSegment segment) const;
  std::unique_ptr<Expr>
  qualified_path_in_expression (std::unique_ptr<Type> &&type, TypePath trait,
				std::vector<PathExprSegment> &&segments
				= {}) const;

  /* Self parameter for a function definition (`&self`) */
  std::unique_ptr<Param> self_ref_param (bool mutability = false) const;
  /* A regular named function parameter for a definition (`a: type`) */
  std::unique_ptr<Param> function_param (std::unique_ptr<Pattern> &&pattern,
					 std::unique_ptr<Type> &&type) const;

  /* Empty function qualifiers, with no specific qualifiers */
  FunctionQualifiers fn_qualifiers () const;

  std::unique_ptr<Function>
  function (std::string function_name,
	    std::vector<std::unique_ptr<Param>> params,
	    std::unique_ptr<Type> return_type, std::unique_ptr<BlockExpr> block,
	    std::vector<std::unique_ptr<GenericParam>> generic_params = {},
	    FunctionQualifiers qualifiers
	    = FunctionQualifiers (UNKNOWN_LOCATION, Async::No, Const::No,
				  Unsafety::Normal),
	    WhereClause where_clause = WhereClause::create_empty (),
	    Visibility visibility = Visibility::create_private ()) const;

  /* Create a single path segment from one string */
  PathExprSegment path_segment (std::string seg) const;

  /* And similarly for type path segments */
  std::unique_ptr<TypePathSegment> type_path_segment (std::string seg) const;
  std::unique_ptr<TypePathSegment>
  type_path_segment (LangItem::Kind lang_item) const;

  std::unique_ptr<TypePathSegment>
  type_path_segment_generic (std::string seg, GenericArgs args) const;
  std::unique_ptr<TypePathSegment>
  type_path_segment_generic (LangItem::Kind lang_item, GenericArgs args) const;

  /* Create a Type from a single string - the most basic kind of type in our AST
   */
  std::unique_ptr<Type> single_type_path (std::string type) const;
  std::unique_ptr<Type> single_type_path (LangItem::Kind lang_item) const;

  std::unique_ptr<Type> single_generic_type_path (std::string type,
						  GenericArgs args) const;
  std::unique_ptr<Type> single_generic_type_path (LangItem::Kind lang_item,
						  GenericArgs args) const;

  TypePath type_path (std::vector<std::unique_ptr<TypePathSegment>> &&segment,
		      bool opening_scope = false) const;
  TypePath type_path (std::vector<std::string> &&segments,
		      bool opening_scope = false) const;
  TypePath type_path (std::unique_ptr<TypePathSegment> &&segment) const;
  TypePath type_path (std::string type) const;
  TypePath type_path (LangItem::Kind lang_item) const;

  std::unique_ptr<Type>
  reference_type (std::unique_ptr<TypeNoBounds> &&inner_type,
		  bool mutability = false) const;

  /**
   * Create a path in expression from multiple segments (`Clone::clone`). You
   * do not need to separate the segments using `::`, you can simply provide a
   * vector of strings to the functions which will get turned into path segments
   */
  PathInExpression path_in_expression (std::vector<std::string> &&segments,
				       bool opening_scope = false) const;

  /**
   * Create a path in expression from a lang item.
   */
  PathInExpression path_in_expression (LangItem::Kind lang_item) const;

  /* Create the path to an enum's variant (`Result::Ok`) */
  PathInExpression variant_path (const std::string &enum_path,
				 const std::string &variant) const;

  /* Create a new struct */
  std::unique_ptr<Stmt>
  struct_struct (std::string struct_name,
		 std::vector<std::unique_ptr<GenericParam>> &&generics,
		 std::vector<StructField> &&fields);

  /* Create a struct expression for unit structs (`S`) */
  std::unique_ptr<Expr> struct_expr_struct (std::string struct_name) const;

  /**
   * Create an expression for struct instantiation with fields (`S { a, b: c }`)
   * Named tuple expressions (`S(a, b, c)`) are call expressions and can thus be
   * constructed with `call`
   */
  std::unique_ptr<Expr>
  struct_expr (std::string struct_name,
	       std::vector<std::unique_ptr<StructExprField>> &&fields) const;
  std::unique_ptr<Expr>
  struct_expr (PathInExpression struct_name,
	       std::vector<std::unique_ptr<StructExprField>> &&fields) const;

  /* Create a field expression for struct instantiation (`field_name: value`) */
  std::unique_ptr<StructExprField>
  struct_expr_field (std::string field_name,
		     std::unique_ptr<Expr> &&value) const;

  /* Create a field access expression (`instance.field`) */
  std::unique_ptr<Expr> field_access (std::unique_ptr<Expr> &&instance,
				      std::string field) const;

  /* Create a wildcard pattern (`_`) */
  std::unique_ptr<Pattern> wildcard () const;
  /* Create a reference pattern (`&pattern`) */
  std::unique_ptr<Pattern> ref_pattern (std::unique_ptr<Pattern> &&inner) const;

  /* Create a lang item path usable as a general path */
  std::unique_ptr<Path> lang_item_path (LangItem::Kind) const;

  /* Create match expressions and their components */
  std::unique_ptr<Expr> match (std::unique_ptr<Expr> &&scrutinee,
			       std::vector<MatchCase> &&cases);
  MatchArm match_arm (std::unique_ptr<Pattern> &&pattern);
  MatchCase match_case (std::unique_ptr<Pattern> &&pattern,
			std::unique_ptr<Expr> &&expr);

  /* Create a loop expression */
  std::unique_ptr<Expr> loop (std::vector<std::unique_ptr<Stmt>> &&stmts);

  std::unique_ptr<TypeParamBound> trait_bound (TypePath bound);
  std::unique_ptr<Item>
  trait_impl (TypePath trait_path, std::unique_ptr<Type> target,
	      std::vector<std::unique_ptr<AssociatedItem>> trait_items = {},
	      std::vector<std::unique_ptr<GenericParam>> generics = {},
	      WhereClause where_clause = WhereClause::create_empty (),
	      Visibility visibility = Visibility::create_private ()) const;

  std::unique_ptr<GenericParam>
  generic_type_param (std::string type_representation,
		      std::vector<std::unique_ptr<TypeParamBound>> &&bounds,
		      std::unique_ptr<Type> &&type = nullptr);

  static std::unique_ptr<Type> new_type (Type &type);

  static std::unique_ptr<GenericParam>
  new_lifetime_param (LifetimeParam &param);

  static std::unique_ptr<GenericParam> new_type_param (
    TypeParam &param,
    std::vector<std::unique_ptr<TypeParamBound>> extra_trait_bounds = {});

  static Lifetime new_lifetime (const Lifetime &lifetime);

  static GenericArgs new_generic_args (GenericArgs &args);

private:
  /**
   * Location of the generated AST nodes
   */
  location_t loc;
};

} // namespace AST
} // namespace Rust

#endif // AST_BUILDER_H
