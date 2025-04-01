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
#include "optional.h"
#include "rust-ast-builder-type.h"
#include "rust-ast.h"
#include "rust-common.h"
#include "rust-expr.h"
#include "rust-keyword-values.h"
#include "rust-path.h"
#include "rust-item.h"
#include "rust-path.h"
#include "rust-pattern.h"
#include "rust-system.h"
#include "rust-token.h"
#include <memory>

namespace Rust {
namespace AST {

std::unique_ptr<Stmt>
Builder::statementify (std::unique_ptr<Expr> &&value,
		       bool semicolon_followed) const
{
  return std::make_unique<ExprStmt> (std::move (value), loc,
				     semicolon_followed);
}

std::unique_ptr<Expr>
Builder::literal_string (std::string &&content) const
{
  return std::unique_ptr<Expr> (
    new AST::LiteralExpr (std::move (content), Literal::LitType::STRING,
			  PrimitiveCoreType::CORETYPE_STR, {}, loc));
}

std::unique_ptr<Expr>
Builder::literal_bool (bool b) const
{
  auto str
    = b ? Values::Keywords::TRUE_LITERAL : Values::Keywords::FALSE_LITERAL;

  return std::unique_ptr<Expr> (
    new AST::LiteralExpr (std::move (str), Literal::LitType::BOOL,
			  PrimitiveCoreType::CORETYPE_BOOL, {}, loc));
}

std::unique_ptr<Expr>
Builder::call (std::unique_ptr<Expr> &&path,
	       std::vector<std::unique_ptr<Expr>> &&args) const
{
  return std::unique_ptr<Expr> (
    new CallExpr (std::move (path), std::move (args), {}, loc));
}

std::unique_ptr<Expr>
Builder::call (std::unique_ptr<Expr> &&path, std::unique_ptr<Expr> &&arg) const
{
  auto args = std::vector<std::unique_ptr<Expr>> ();
  args.emplace_back (std::move (arg));

  return call (std::move (path), std::move (args));
}

std::unique_ptr<Expr>
Builder::array (std::vector<std::unique_ptr<Expr>> &&members) const
{
  auto elts = std::make_unique<ArrayElemsValues> (std::move (members), loc);

  return std::unique_ptr<Expr> (new ArrayExpr (std::move (elts), {}, {}, loc));
}

std::unique_ptr<Expr>
Builder::qualified_path_in_expression (std::unique_ptr<Type> &&type,
				       TypePath trait,
				       PathExprSegment segment) const
{
  auto segments = {segment};

  return qualified_path_in_expression (std::move (type), trait, segments);
}

std::unique_ptr<Expr>
Builder::qualified_path_in_expression (
  std::unique_ptr<Type> &&type, TypePath trait,
  std::vector<PathExprSegment> &&segments) const
{
  auto qual_type = QualifiedPathType (std::move (type), loc, trait);

  return std::unique_ptr<QualifiedPathInExpression> (
    new QualifiedPathInExpression (qual_type, std::move (segments), {}, loc));
}

std::unique_ptr<Expr>
Builder::identifier (std::string name) const
{
  return std::unique_ptr<Expr> (new IdentifierExpr (name, {}, loc));
}

std::unique_ptr<Pattern>
Builder::identifier_pattern (std::string name, bool mut) const
{
  return std::unique_ptr<Pattern> (
    new IdentifierPattern (name, loc, false, mut));
}

std::unique_ptr<Expr>
Builder::tuple_idx (std::string receiver, int idx) const
{
  return std::unique_ptr<Expr> (
    new TupleIndexExpr (identifier (receiver), idx, {}, loc));
}

std::unique_ptr<Expr>
Builder::tuple (std::vector<std::unique_ptr<Expr>> &&values) const
{
  return std::unique_ptr<TupleExpr> (
    new TupleExpr (std::move (values), {}, {}, loc));
}

std::unique_ptr<Param>
Builder::self_ref_param (bool mutability) const
{
  return std::make_unique<SelfParam> (tl::nullopt, mutability, loc);
}

std::unique_ptr<Param>
Builder::function_param (std::unique_ptr<Pattern> &&pattern,
			 std::unique_ptr<Type> &&type) const
{
  return std::unique_ptr<FunctionParam> (
    new FunctionParam (std::move (pattern), std::move (type), {}, loc));
}

FunctionQualifiers
Builder::fn_qualifiers () const
{
  return FunctionQualifiers (loc, Async::No, Const::No, Unsafety::Normal);
}

std::unique_ptr<Function>
Builder::function (std::string function_name,
		   std::vector<std::unique_ptr<Param>> params,
		   std::unique_ptr<Type> return_type,
		   std::unique_ptr<BlockExpr> block,
		   std::vector<std::unique_ptr<GenericParam>> generic_params,
		   FunctionQualifiers qualifiers, WhereClause where_clause,
		   Visibility visibility) const
{
  return std::unique_ptr<Function> (
    new Function (function_name, qualifiers, std::move (generic_params),
		  std::move (params), std::move (return_type), where_clause,
		  std::move (block), visibility, {}, loc));
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

std::unique_ptr<TypePathSegment>
Builder::type_path_segment (LangItem::Kind lang_item) const
{
  return std::unique_ptr<TypePathSegment> (
    new TypePathSegment (lang_item, loc));
}

std::unique_ptr<TypePathSegment>
Builder::type_path_segment_generic (std::string seg, GenericArgs args) const
{
  return std::unique_ptr<TypePathSegment> (
    new TypePathSegmentGeneric (PathIdentSegment (seg, loc), false, args, loc));
}

std::unique_ptr<TypePathSegment>
Builder::type_path_segment_generic (LangItem::Kind lang_item,
				    GenericArgs args) const
{
  return std::unique_ptr<TypePathSegment> (
    new TypePathSegmentGeneric (lang_item, args, loc));
}

std::unique_ptr<Type>
Builder::single_type_path (std::string type) const
{
  auto segments = std::vector<std::unique_ptr<TypePathSegment>> ();
  segments.emplace_back (type_path_segment (type));

  return std::unique_ptr<Type> (new TypePath (std::move (segments), loc));
}

std::unique_ptr<Type>
Builder::single_type_path (LangItem::Kind lang_item) const
{
  return std::unique_ptr<Type> (new TypePath (lang_item, {}, loc));
}

std::unique_ptr<Type>
Builder::single_generic_type_path (std::string type, GenericArgs args) const
{
  auto segments = std::vector<std::unique_ptr<TypePathSegment>> ();
  segments.emplace_back (type_path_segment_generic (type, args));

  return std::unique_ptr<Type> (new TypePath (std::move (segments), loc));
}

std::unique_ptr<Type>
Builder::single_generic_type_path (LangItem::Kind lang_item,
				   GenericArgs args) const
{
  auto segments = std::vector<std::unique_ptr<TypePathSegment>> ();
  segments.emplace_back (type_path_segment_generic (lang_item, args));

  return std::unique_ptr<Type> (new TypePath (std::move (segments), loc));
}

TypePath
Builder::type_path (std::vector<std::unique_ptr<TypePathSegment>> &&segments,
		    bool opening_scope) const
{
  return TypePath (std::move (segments), loc, opening_scope);
}

TypePath
Builder::type_path (std::vector<std::string> &&segments,
		    bool opening_scope) const
{
  auto type_segments = std::vector<std::unique_ptr<TypePathSegment>> ();

  for (auto &&segment : segments)
    type_segments.emplace_back (type_path_segment (segment));

  return TypePath (std::move (type_segments), loc, opening_scope);
}

TypePath
Builder::type_path (std::unique_ptr<TypePathSegment> &&segment) const
{
  auto segments = std::vector<std::unique_ptr<TypePathSegment>> ();
  segments.emplace_back (std::move (segment));

  return type_path (std::move (segments));
}

TypePath
Builder::type_path (std::string type) const
{
  return type_path (type_path_segment (type));
}

TypePath
Builder::type_path (LangItem::Kind lang_item) const
{
  return type_path (type_path_segment (lang_item));
}

std::unique_ptr<Type>
Builder::reference_type (std::unique_ptr<TypeNoBounds> &&inner_type,
			 bool mutability) const
{
  return std::make_unique<ReferenceType> (mutability, std::move (inner_type),
					  loc);
}

PathInExpression
Builder::path_in_expression (std::vector<std::string> &&segments,
			     bool opening_scope) const
{
  auto path_segments = std::vector<PathExprSegment> ();
  for (auto &seg : segments)
    path_segments.emplace_back (path_segment (seg));

  return PathInExpression (std::move (path_segments), {}, loc, opening_scope);
}

PathInExpression
Builder::path_in_expression (LangItem::Kind lang_item) const
{
  return PathInExpression (lang_item, {}, loc);
}

PathInExpression
Builder::variant_path (const std::string &enum_path,
		       const std::string &variant) const
{
  return PathInExpression ({path_segment (enum_path), path_segment (variant)},
			   {}, loc, false);
}

std::unique_ptr<BlockExpr>
Builder::block (tl::optional<std::unique_ptr<Stmt>> &&stmt,
		std::unique_ptr<Expr> &&tail_expr) const
{
  auto stmts = std::vector<std::unique_ptr<Stmt>> ();

  if (stmt)
    stmts.emplace_back (std::move (*stmt));

  return block (std::move (stmts), std::move (tail_expr));
}

std::unique_ptr<BlockExpr>
Builder::block () const
{
  auto stmts = std::vector<std::unique_ptr<Stmt>> ();

  return block (std::move (stmts));
}

std::unique_ptr<BlockExpr>
Builder::block (std::vector<std::unique_ptr<Stmt>> &&stmts,
		std::unique_ptr<Expr> &&tail_expr) const
{
  return std::unique_ptr<BlockExpr> (new BlockExpr (std::move (stmts),
						    std::move (tail_expr), {},
						    {}, tl::nullopt, loc, loc));
}

std::unique_ptr<Expr>
Builder::return_expr (std::unique_ptr<Expr> &&to_return)
{
  return std::unique_ptr<Expr> (
    new ReturnExpr (std::move (to_return), {}, loc));
}

std::unique_ptr<Stmt>
Builder::let (std::unique_ptr<Pattern> &&pattern, std::unique_ptr<Type> &&type,
	      std::unique_ptr<Expr> &&init) const
{
  return std::unique_ptr<Stmt> (new LetStmt (std::move (pattern),
					     std::move (init), std::move (type),
					     tl::nullopt, {}, loc));
}

std::unique_ptr<Expr>
Builder::ref (std::unique_ptr<Expr> &&of, bool mut) const
{
  auto mutability = mut ? Mutability::Mut : Mutability::Imm;
  return std::unique_ptr<Expr> (
    new BorrowExpr (std::move (of), mutability,
		    /* raw */ false, /* is double */ false, {}, loc));
}

std::unique_ptr<Expr>
Builder::deref (std::unique_ptr<Expr> &&of) const
{
  return std::unique_ptr<Expr> (new DereferenceExpr (std::move (of), {}, loc));
}

std::unique_ptr<Expr>
Builder::comparison_expr (std::unique_ptr<Expr> &&lhs,
			  std::unique_ptr<Expr> &&rhs,
			  ComparisonOperator op) const
{
  return std::make_unique<ComparisonExpr> (std::move (lhs), std::move (rhs), op,
					   loc);
}

std::unique_ptr<Expr>
Builder::boolean_operation (std::unique_ptr<Expr> &&lhs,
			    std::unique_ptr<Expr> &&rhs,
			    LazyBooleanOperator op) const
{
  return std::make_unique<LazyBooleanExpr> (std::move (lhs), std::move (rhs),
					    op, loc);
}

std::unique_ptr<Stmt>
Builder::struct_struct (std::string struct_name,
			std::vector<std::unique_ptr<GenericParam>> &&generics,
			std::vector<StructField> &&fields)
{
  auto is_unit = fields.empty ();

  return std::unique_ptr<Stmt> (
    new StructStruct (std::move (fields), struct_name, std::move (generics),
		      WhereClause::create_empty (), is_unit,
		      Visibility::create_private (), {}, loc));
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
  return struct_expr (path_in_expression ({struct_name}), std::move (fields));
}

std::unique_ptr<Expr>
Builder::struct_expr (
  PathInExpression struct_name,
  std::vector<std::unique_ptr<StructExprField>> &&fields) const
{
  return std::unique_ptr<Expr> (
    new StructExprStructFields (struct_name, std::move (fields), loc));
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

std::unique_ptr<Pattern>
Builder::ref_pattern (std::unique_ptr<Pattern> &&inner) const
{
  return std::make_unique<ReferencePattern> (std::move (inner), false, false,
					     loc);
}

std::unique_ptr<Path>
Builder::lang_item_path (LangItem::Kind kind) const
{
  return std::unique_ptr<Path> (new PathInExpression (kind, {}, loc));
}

std::unique_ptr<Expr>
Builder::match (std::unique_ptr<Expr> &&scrutinee,
		std::vector<MatchCase> &&cases)
{
  return std::unique_ptr<Expr> (
    new MatchExpr (std::move (scrutinee), std::move (cases), {}, {}, loc));
}

MatchArm
Builder::match_arm (std::unique_ptr<Pattern> &&pattern)
{
  auto patterns = std::vector<std::unique_ptr<Pattern>> ();
  patterns.emplace_back (std::move (pattern));

  return MatchArm (std::move (patterns), loc);
}

MatchCase
Builder::match_case (std::unique_ptr<Pattern> &&pattern,
		     std::unique_ptr<Expr> &&expr)
{
  return MatchCase (match_arm (std::move (pattern)), std::move (expr));
}

std::unique_ptr<Expr>
Builder::loop (std::vector<std::unique_ptr<Stmt>> &&stmts)
{
  auto block_expr = block (std::move (stmts), nullptr);

  return std::unique_ptr<Expr> (new LoopExpr (std::move (block_expr), loc));
}

std::unique_ptr<TypeParamBound>
Builder::trait_bound (TypePath bound)
{
  return std::make_unique<TraitBound> (bound, loc);
}

std::unique_ptr<Item>
Builder::trait_impl (TypePath trait_path, std::unique_ptr<Type> target,
		     std::vector<std::unique_ptr<AssociatedItem>> trait_items,
		     std::vector<std::unique_ptr<GenericParam>> generics,
		     WhereClause where_clause, Visibility visibility) const
{
  return std::unique_ptr<Item> (
    new TraitImpl (trait_path, /* unsafe */ false,
		   /* exclam */ false, std::move (trait_items),
		   std::move (generics), std::move (target), where_clause,
		   visibility, {}, {}, loc));
}

std::unique_ptr<GenericParam>
Builder::generic_type_param (
  std::string type_representation,
  std::vector<std::unique_ptr<TypeParamBound>> &&bounds,
  std::unique_ptr<Type> &&type)
{
  return std::make_unique<TypeParam> (type_representation, loc,
				      std::move (bounds), std::move (type),
				      std::vector<Attribute> ());
}

std::unique_ptr<Type>
Builder::new_type (Type &type)
{
  Type *t = ASTTypeBuilder::build (type);
  return std::unique_ptr<Type> (t);
}

std::unique_ptr<GenericParam>
Builder::new_lifetime_param (LifetimeParam &param)
{
  Lifetime l = new_lifetime (param.get_lifetime ());
  std::vector<Lifetime> lifetime_bounds;
  for (auto b : param.get_lifetime_bounds ())
    {
      Lifetime bl = new_lifetime (b);
      lifetime_bounds.push_back (bl);
    }

  auto p = new LifetimeParam (l, std::move (lifetime_bounds),
			      param.get_outer_attrs (), param.get_locus ());
  return std::unique_ptr<GenericParam> (p);
}

std::unique_ptr<GenericParam>
Builder::new_type_param (
  TypeParam &param, std::vector<std::unique_ptr<TypeParamBound>> extra_bounds)
{
  location_t locus = param.get_locus ();
  AST::AttrVec outer_attrs = param.get_outer_attrs ();
  Identifier type_representation = param.get_type_representation ();
  std::vector<std::unique_ptr<TypeParamBound>> type_param_bounds;
  std::unique_ptr<Type> type = nullptr;

  if (param.has_type ())
    type = new_type (param.get_type ());

  for (auto &&extra_bound : extra_bounds)
    type_param_bounds.emplace_back (std::move (extra_bound));

  for (const auto &b : param.get_type_param_bounds ())
    {
      switch (b->get_bound_type ())
	{
	  case TypeParamBound::TypeParamBoundType::TRAIT: {
	    const TraitBound &tb = (const TraitBound &) *b.get ();
	    const TypePath &path = tb.get_type_path ();

	    std::vector<LifetimeParam> for_lifetimes;
	    for (const auto &lifetime : tb.get_for_lifetimes ())
	      {
		std::vector<Lifetime> lifetime_bounds;
		for (const auto &b : lifetime.get_lifetime_bounds ())
		  {
		    Lifetime bl = new_lifetime (b);
		    lifetime_bounds.push_back (std::move (bl));
		  }

		Lifetime nl = new_lifetime (lifetime.get_lifetime ());
		LifetimeParam p (std::move (nl), std::move (lifetime_bounds),
				 {}, lifetime.get_locus ());
		for_lifetimes.push_back (std::move (p));
	      }

	    std::vector<std::unique_ptr<TypePathSegment>> segments;
	    for (auto &seg : path.get_segments ())
	      {
		switch (seg->get_type ())
		  {
		    case TypePathSegment::REG: {
		      const TypePathSegment &segment
			= (const TypePathSegment &) (*seg.get ());
		      TypePathSegment *s = new TypePathSegment (
			segment.get_ident_segment (),
			segment.get_separating_scope_resolution (),
			segment.get_locus ());
		      std::unique_ptr<TypePathSegment> sg (s);
		      segments.push_back (std::move (sg));
		    }
		    break;

		    case TypePathSegment::GENERIC: {
		      TypePathSegmentGeneric &generic
			= (TypePathSegmentGeneric &) (*seg.get ());

		      GenericArgs args
			= new_generic_args (generic.get_generic_args ());
		      TypePathSegmentGeneric *s = new TypePathSegmentGeneric (
			generic.get_ident_segment (), false, std::move (args),
			generic.get_locus ());
		      std::unique_ptr<TypePathSegment> sg (s);
		      segments.push_back (std::move (sg));
		    }
		    break;

		    case TypePathSegment::FUNCTION: {
		      rust_unreachable ();
		      // TODO
		      // const TypePathSegmentFunction &fn
		      //   = (const TypePathSegmentFunction &) (*seg.get ());
		    }
		    break;
		  }
	      }

	    TypePath p (std::move (segments), path.get_locus (),
			path.has_opening_scope_resolution_op ());

	    TraitBound *b = new TraitBound (std::move (p), tb.get_locus (),
					    tb.is_in_parens (),
					    tb.has_opening_question_mark (),
					    std::move (for_lifetimes));
	    std::unique_ptr<TypeParamBound> bound (b);
	    type_param_bounds.push_back (std::move (bound));
	  }
	  break;

	  case TypeParamBound::TypeParamBoundType::LIFETIME: {
	    const Lifetime &l = (const Lifetime &) *b.get ();

	    auto bl = new Lifetime (l.get_lifetime_type (),
				    l.get_lifetime_name (), l.get_locus ());
	    std::unique_ptr<TypeParamBound> bound (bl);
	    type_param_bounds.push_back (std::move (bound));
	  }
	  break;
	}
    }

  auto type_param
    = new TypeParam (type_representation, locus, std::move (type_param_bounds),
		     std::move (type), std::move (outer_attrs));

  return std::unique_ptr<GenericParam> (type_param);
}

Lifetime
Builder::new_lifetime (const Lifetime &lifetime)
{
  return Lifetime (lifetime.get_lifetime_type (), lifetime.get_lifetime_name (),
		   lifetime.get_locus ());
}

GenericArgs
Builder::new_generic_args (GenericArgs &args)
{
  std::vector<Lifetime> lifetime_args;
  std::vector<GenericArg> generic_args;
  std::vector<GenericArgsBinding> binding_args;
  location_t locus = args.get_locus ();

  for (const auto &lifetime : args.get_lifetime_args ())
    {
      Lifetime l = new_lifetime (lifetime);
      lifetime_args.push_back (std::move (l));
    }

  for (auto &binding : args.get_binding_args ())
    {
      Type &t = *binding.get_type_ptr ().get ();
      std::unique_ptr<Type> ty = new_type (t);
      GenericArgsBinding b (binding.get_identifier (), std::move (ty),
			    binding.get_locus ());
      binding_args.push_back (std::move (b));
    }

  for (auto &arg : args.get_generic_args ())
    {
      switch (arg.get_kind ())
	{
	  case GenericArg::Kind::Type: {
	    std::unique_ptr<Type> ty = new_type (arg.get_type ());
	    GenericArg arg = GenericArg::create_type (std::move (ty));
	  }
	  break;

	default:
	  // FIXME
	  rust_unreachable ();
	  break;
	}
    }

  return GenericArgs (std::move (lifetime_args), std::move (generic_args),
		      std::move (binding_args), locus);
}

} // namespace AST
} // namespace Rust
