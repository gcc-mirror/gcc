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
#include "rust-ast-builder-type.h"
#include "rust-common.h"
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

std::unique_ptr<TypePathSegment>
Builder::generic_type_path_segment (std::string seg, GenericArgs args) const
{
  return std::unique_ptr<TypePathSegment> (
    new TypePathSegmentGeneric (PathIdentSegment (seg, loc), false, args, loc));
}

std::unique_ptr<Type>
Builder::single_type_path (std::string type) const
{
  auto segments = std::vector<std::unique_ptr<TypePathSegment>> ();
  segments.emplace_back (type_path_segment (type));

  return std::unique_ptr<Type> (new TypePath (std::move (segments), loc));
}

std::unique_ptr<Type>
Builder::single_generic_type_path (std::string type, GenericArgs args) const
{
  auto segments = std::vector<std::unique_ptr<TypePathSegment>> ();
  segments.emplace_back (generic_type_path_segment (type, args));

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
Builder::new_type_param (TypeParam &param)
{
  location_t locus = param.get_locus ();
  AST::AttrVec outer_attrs = param.get_outer_attrs ();
  Identifier type_representation = param.get_type_representation ();
  std::vector<std::unique_ptr<TypeParamBound>> type_param_bounds;
  std::unique_ptr<Type> type = nullptr;

  if (param.has_type ())
    type = new_type (param.get_type ());

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
