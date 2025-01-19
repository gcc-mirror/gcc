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

#include "rust-ast-lower-type.h"

namespace Rust {
namespace HIR {

HIR::TypePath *
ASTLowerTypePath::translate (AST::TypePath &type)
{
  ASTLowerTypePath resolver;
  type.accept_vis (resolver);
  rust_assert (resolver.translated != nullptr);
  return resolver.translated;
}

void
ASTLowerTypePath::visit (AST::TypePathSegmentFunction &segment)
{
  auto crate_num = mappings->get_current_crate ();
  auto hirid = mappings->get_next_hir_id (crate_num);
  Analysis::NodeMapping mapping (crate_num, segment.get_node_id (), hirid,
				 UNKNOWN_LOCAL_DEFID);

  HIR::PathIdentSegment ident (segment.get_ident_segment ().as_string ());

  AST::TypePathFunction &fn = segment.get_type_path_function ();
  std::vector<std::unique_ptr<HIR::Type>> inputs;
  for (auto &param : fn.get_params ())
    {
      HIR::Type *hir_type = ASTLoweringType::translate (*param);
      inputs.push_back (std::unique_ptr<HIR::Type> (hir_type));
    }

  HIR::Type *result_type
    = fn.has_return_type () ? ASTLoweringType::translate (fn.get_return_type ())
			    : nullptr;

  HIR::TypePathFunction function_path (std::move (inputs),
				       std::unique_ptr<HIR::Type> (
					 result_type));

  translated_segment = new HIR::TypePathSegmentFunction (
    mapping, std::move (ident), segment.get_separating_scope_resolution (),
    std::move (function_path), segment.get_locus ());
}

void
ASTLowerTypePath::visit (AST::TypePathSegment &segment)
{
  auto crate_num = mappings->get_current_crate ();
  auto hirid = mappings->get_next_hir_id (crate_num);
  Analysis::NodeMapping mapping (crate_num, segment.get_node_id (), hirid,
				 UNKNOWN_LOCAL_DEFID);

  HIR::PathIdentSegment ident (segment.get_ident_segment ().as_string ());
  translated_segment
    = new HIR::TypePathSegment (std::move (mapping), ident,
				segment.get_separating_scope_resolution (),
				segment.get_locus ());
}

void
ASTLowerTypePath::visit (AST::TypePathSegmentGeneric &segment)
{
  std::vector<HIR::GenericArgsBinding> binding_args; // TODO

  std::string segment_name = segment.get_ident_segment ().as_string ();
  bool has_separating_scope_resolution
    = segment.get_separating_scope_resolution ();

  auto generic_args = lower_generic_args (segment.get_generic_args ());

  auto crate_num = mappings->get_current_crate ();
  auto hirid = mappings->get_next_hir_id (crate_num);
  Analysis::NodeMapping mapping (crate_num, segment.get_node_id (), hirid,
				 UNKNOWN_LOCAL_DEFID);

  translated_segment
    = new HIR::TypePathSegmentGeneric (std::move (mapping), segment_name,
				       has_separating_scope_resolution,
				       generic_args, segment.get_locus ());
}

void
ASTLowerTypePath::visit (AST::TypePath &path)
{
  std::vector<std::unique_ptr<HIR::TypePathSegment>> translated_segments;

  for (auto &seg : path.get_segments ())
    {
      translated_segment = nullptr;
      seg->accept_vis (*this);
      rust_assert (translated_segment != nullptr);

      translated_segments.push_back (
	std::unique_ptr<HIR::TypePathSegment> (translated_segment));
    }

  auto crate_num = mappings->get_current_crate ();
  auto hirid = mappings->get_next_hir_id (crate_num);
  Analysis::NodeMapping mapping (crate_num, path.get_node_id (), hirid,
				 mappings->get_next_localdef_id (crate_num));

  translated
    = new HIR::TypePath (std::move (mapping), std::move (translated_segments),
			 path.get_locus (),
			 path.has_opening_scope_resolution_op ());
}

HIR::QualifiedPathInType *
ASTLowerQualifiedPathInType::translate (AST::QualifiedPathInType &type)
{
  ASTLowerQualifiedPathInType resolver;
  type.accept_vis (resolver);
  rust_assert (resolver.translated != nullptr);
  return resolver.translated;
}

void
ASTLowerQualifiedPathInType::visit (AST::QualifiedPathInType &path)
{
  auto crate_num = mappings->get_current_crate ();
  auto hirid = mappings->get_next_hir_id (crate_num);
  Analysis::NodeMapping qual_mappings (
    crate_num, path.get_qualified_path_type ().get_node_id (), hirid,
    UNKNOWN_LOCAL_DEFID);

  HIR::Type *qual_type
    = ASTLoweringType::translate (path.get_qualified_path_type ().get_type ());
  HIR::TypePath *qual_trait = ASTLowerTypePath::translate (
    path.get_qualified_path_type ().get_as_type_path ());

  HIR::QualifiedPathType qual_path_type (
    qual_mappings, std::unique_ptr<HIR::Type> (qual_type),
    std::unique_ptr<HIR::TypePath> (qual_trait),
    path.get_qualified_path_type ().get_locus ());

  translated_segment = nullptr;
  path.get_associated_segment ()->accept_vis (*this);
  rust_assert (translated_segment != nullptr);

  std::unique_ptr<HIR::TypePathSegment> associated_segment (translated_segment);

  std::vector<std::unique_ptr<HIR::TypePathSegment>> translated_segments;
  for (auto &seg : path.get_segments ())
    {
      translated_segment = nullptr;
      seg->accept_vis (*this);
      rust_assert (translated_segment != nullptr);

      translated_segments.push_back (
	std::unique_ptr<HIR::TypePathSegment> (translated_segment));
    }

  Analysis::NodeMapping mapping (crate_num, path.get_node_id (), hirid,
				 mappings->get_next_localdef_id (crate_num));
  translated = new HIR::QualifiedPathInType (std::move (mapping),
					     std::move (qual_path_type),
					     std::move (associated_segment),
					     std::move (translated_segments),
					     path.get_locus ());
}

HIR::Type *
ASTLoweringType::translate (AST::Type &type, bool default_to_static_lifetime)
{
  ASTLoweringType resolver (default_to_static_lifetime);
  type.accept_vis (resolver);

  rust_assert (resolver.translated != nullptr);
  resolver.mappings->insert_hir_type (resolver.translated);
  resolver.mappings->insert_location (
    resolver.translated->get_mappings ().get_hirid (),
    resolver.translated->get_locus ());

  return resolver.translated;
}

void
ASTLoweringType::visit (AST::BareFunctionType &fntype)
{
  bool is_variadic = false;
  std::vector<HIR::LifetimeParam> lifetime_params;
  for (auto &lifetime_param : fntype.get_for_lifetimes ())
    {
      auto generic_param = ASTLowerGenericParam::translate (lifetime_param);
      lifetime_params.push_back (
	*static_cast<HIR::LifetimeParam *> (generic_param));
    }

  HIR::FunctionQualifiers qualifiers
    = lower_qualifiers (fntype.get_function_qualifiers ());

  std::vector<HIR::MaybeNamedParam> named_params;
  for (auto &param : fntype.get_function_params ())
    {
      HIR::MaybeNamedParam::ParamKind kind;
      switch (param.get_param_kind ())
	{
	case AST::MaybeNamedParam::ParamKind::UNNAMED:
	  kind = HIR::MaybeNamedParam::ParamKind::UNNAMED;
	  break;
	case AST::MaybeNamedParam::ParamKind::IDENTIFIER:
	  kind = HIR::MaybeNamedParam::ParamKind::IDENTIFIER;
	  break;
	case AST::MaybeNamedParam::ParamKind::WILDCARD:
	  kind = HIR::MaybeNamedParam::ParamKind::WILDCARD;
	  break;
	default:
	  rust_unreachable ();
	}

      HIR::Type *param_type
	= ASTLoweringType::translate (param.get_type (),
				      default_to_static_lifetime);

      HIR::MaybeNamedParam p (param.get_name (), kind,
			      std::unique_ptr<HIR::Type> (param_type),
			      param.get_locus ());
      named_params.push_back (std::move (p));
    }

  HIR::Type *return_type = nullptr;
  if (fntype.has_return_type ())
    {
      return_type = ASTLoweringType::translate (fntype.get_return_type (),
						default_to_static_lifetime);
    }

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, fntype.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 mappings->get_next_localdef_id (crate_num));

  translated = new HIR::BareFunctionType (
    std::move (mapping), std::move (lifetime_params), std::move (qualifiers),
    std::move (named_params), is_variadic,
    std::unique_ptr<HIR::Type> (return_type), fntype.get_locus ());
}

void
ASTLoweringType::visit (AST::TupleType &tuple)
{
  std::vector<std::unique_ptr<HIR::Type>> elems;
  for (auto &e : tuple.get_elems ())
    {
      HIR::Type *t
	= ASTLoweringType::translate (*e, default_to_static_lifetime);
      elems.push_back (std::unique_ptr<HIR::Type> (t));
    }

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, tuple.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 mappings->get_next_localdef_id (crate_num));

  translated = new HIR::TupleType (std::move (mapping), std::move (elems),
				   tuple.get_locus ());
}

void
ASTLoweringType::visit (AST::TypePath &path)
{
  translated = ASTLowerTypePath::translate (path);
}

void
ASTLoweringType::visit (AST::QualifiedPathInType &path)
{
  translated = ASTLowerQualifiedPathInType::translate (path);
}

void
ASTLoweringType::visit (AST::ArrayType &type)
{
  HIR::Type *translated_type
    = ASTLoweringType::translate (type.get_elem_type (),
				  default_to_static_lifetime);
  HIR::Expr *array_size = ASTLoweringExpr::translate (type.get_size_expr ());

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, type.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 mappings->get_next_localdef_id (crate_num));

  translated
    = new HIR::ArrayType (mapping, std::unique_ptr<HIR::Type> (translated_type),
			  std::unique_ptr<HIR::Expr> (array_size),
			  type.get_locus ());
}

void
ASTLoweringType::visit (AST::ReferenceType &type)
{
  HIR::Lifetime lifetime
    = lower_lifetime (type.get_lifetime (), default_to_static_lifetime);

  HIR::Type *base_type
    = ASTLoweringType::translate (type.get_base_type (),
				  default_to_static_lifetime);

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, type.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 mappings->get_next_localdef_id (crate_num));

  translated = new HIR::ReferenceType (mapping,
				       type.get_has_mut () ? Mutability::Mut
							   : Mutability::Imm,
				       std::unique_ptr<HIR::Type> (base_type),
				       type.get_locus (), lifetime);
}

void
ASTLoweringType::visit (AST::RawPointerType &type)
{
  HIR::Type *base_type
    = ASTLoweringType::translate (type.get_type_pointed_to (),
				  default_to_static_lifetime);

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, type.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 mappings->get_next_localdef_id (crate_num));

  translated
    = new HIR::RawPointerType (mapping,
			       type.get_pointer_type ()
				   == AST::RawPointerType::PointerType::MUT
				 ? Mutability::Mut
				 : Mutability::Imm,
			       std::unique_ptr<HIR::Type> (base_type),
			       type.get_locus ());
}

void
ASTLoweringType::visit (AST::SliceType &type)
{
  HIR::Type *base_type
    = ASTLoweringType::translate (type.get_elem_type (),
				  default_to_static_lifetime);

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, type.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 mappings->get_next_localdef_id (crate_num));

  translated
    = new HIR::SliceType (mapping, std::unique_ptr<HIR::Type> (base_type),
			  type.get_locus ());
}

void
ASTLoweringType::visit (AST::InferredType &type)
{
  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, type.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 mappings->get_next_localdef_id (crate_num));

  translated = new HIR::InferredType (mapping, type.get_locus ());
}

void
ASTLoweringType::visit (AST::NeverType &type)
{
  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, type.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 mappings->get_next_localdef_id (crate_num));

  translated = new HIR::NeverType (mapping, type.get_locus ());
}

void
ASTLoweringType::visit (AST::TraitObjectTypeOneBound &type)
{
  std::vector<std::unique_ptr<HIR::TypeParamBound>> bounds;
  HIR::TypeParamBound *translated_bound
    = ASTLoweringTypeBounds::translate (type.get_trait_bound ());
  bounds.push_back (std::unique_ptr<HIR::TypeParamBound> (translated_bound));

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, type.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 mappings->get_next_localdef_id (crate_num));

  translated = new HIR::TraitObjectType (mapping, std::move (bounds),
					 type.get_locus (), type.is_dyn ());
}

void
ASTLoweringType::visit (AST::TraitObjectType &type)
{
  std::vector<std::unique_ptr<HIR::TypeParamBound>> bounds;

  for (auto &bound : type.get_type_param_bounds ())
    {
      HIR::TypeParamBound *translated_bound
	= ASTLoweringTypeBounds::translate (*bound);
      bounds.push_back (
	std::unique_ptr<HIR::TypeParamBound> (translated_bound));
    }

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, type.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 mappings->get_next_localdef_id (crate_num));

  translated = new HIR::TraitObjectType (mapping, std::move (bounds),
					 type.get_locus (), type.is_dyn ());
}

HIR::GenericParam *
ASTLowerGenericParam::translate (AST::GenericParam &param)
{
  ASTLowerGenericParam resolver;
  param.accept_vis (resolver);

  rust_assert (resolver.translated != nullptr);
  resolver.mappings->insert_location (
    resolver.translated->get_mappings ().get_hirid (), param.get_locus ());
  resolver.mappings->insert_hir_generic_param (resolver.translated);

  return resolver.translated;
}

void
ASTLowerGenericParam::visit (AST::LifetimeParam &param)
{
  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, param.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 mappings->get_next_localdef_id (crate_num));

  HIR::Lifetime lt (mapping, param.get_lifetime ().get_lifetime_type (),
		    param.get_lifetime ().get_lifetime_name (),
		    param.get_lifetime ().get_locus ());

  translated = new HIR::LifetimeParam (mapping, lt, param.get_locus (),
				       std::vector<Lifetime> ());
}

void
ASTLowerGenericParam::visit (AST::ConstGenericParam &param)
{
  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, param.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 mappings->get_next_localdef_id (crate_num));

  auto type = ASTLoweringType::translate (param.get_type ());

  HIR::Expr *default_expr = nullptr;
  if (param.has_default_value ())
    default_expr = ASTLoweringExpr::translate (
      param.get_default_value ().get_expression ());

  translated = new HIR::ConstGenericParam (param.get_name ().as_string (),
					   std::unique_ptr<Type> (type),
					   std::unique_ptr<Expr> (default_expr),
					   mapping, param.get_locus ());
}

void
ASTLowerGenericParam::visit (AST::TypeParam &param)
{
  AST::Attribute outer_attr = AST::Attribute::create_empty ();
  std::vector<std::unique_ptr<HIR::TypeParamBound>> type_param_bounds;
  if (param.has_type_param_bounds ())
    {
      for (auto &bound : param.get_type_param_bounds ())
	{
	  HIR::TypeParamBound *lowered_bound = lower_bound (*bound);
	  type_param_bounds.push_back (
	    std::unique_ptr<HIR::TypeParamBound> (lowered_bound));
	}
    }

  HIR::Type *type = param.has_type ()
		      ? ASTLoweringType::translate (param.get_type ())
		      : nullptr;

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, param.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 mappings->get_next_localdef_id (crate_num));

  translated
    = new HIR::TypeParam (mapping, param.get_type_representation (),
			  param.get_locus (), std::move (type_param_bounds),
			  std::unique_ptr<Type> (type), std::move (outer_attr));
}

HIR::TypeParamBound *
ASTLoweringTypeBounds::translate (AST::TypeParamBound &type)
{
  ASTLoweringTypeBounds resolver;
  type.accept_vis (resolver);

  rust_assert (resolver.translated != nullptr);
  resolver.mappings->insert_location (
    resolver.translated->get_mappings ().get_hirid (),
    resolver.translated->get_locus ());

  return resolver.translated;
}

void
ASTLoweringTypeBounds::visit (AST::TraitBound &bound)
{
  std::vector<HIR::LifetimeParam> for_lifetimes;
  for (auto &lifetime_param : bound.get_for_lifetimes ())
    {
      auto generic_param = ASTLowerGenericParam::translate (lifetime_param);
      for_lifetimes.push_back (
	*static_cast<HIR::LifetimeParam *> (generic_param));
    }

  AST::TypePath &ast_trait_path = bound.get_type_path ();
  HIR::TypePath *trait_path = ASTLowerTypePath::translate (ast_trait_path);

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, bound.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  BoundPolarity polarity = bound.has_opening_question_mark ()
			     ? BoundPolarity::AntiBound
			     : BoundPolarity::RegularBound;
  translated
    = new HIR::TraitBound (mapping, *trait_path, bound.get_locus (),
			   bound.is_in_parens (), polarity, for_lifetimes);
}

void
ASTLoweringTypeBounds::visit (AST::Lifetime &bound)
{
  HIR::Lifetime lifetime = lower_lifetime (bound);
  translated = new HIR::Lifetime (lifetime);
}

HIR::WhereClauseItem *
ASTLowerWhereClauseItem::translate (AST::WhereClauseItem &item)
{
  ASTLowerWhereClauseItem compiler;
  item.accept_vis (compiler);

  rust_assert (compiler.translated != nullptr);
  // FIXME
  // compiler.mappings->insert_location (
  //   compiler.translated->get_mappings ().get_hirid (),
  //   compiler.translated->get_locus ());

  return compiler.translated;
}

void
ASTLowerWhereClauseItem::visit (AST::LifetimeWhereClauseItem &item)
{
  HIR::Lifetime l = lower_lifetime (item.get_lifetime ());
  std::vector<HIR::Lifetime> lifetime_bounds;
  for (auto &lifetime_bound : item.get_lifetime_bounds ())
    {
      HIR::Lifetime ll = lower_lifetime (lifetime_bound);
      lifetime_bounds.push_back (std::move (ll));
    }

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, item.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated = new HIR::LifetimeWhereClauseItem (mapping, std::move (l),
						 std::move (lifetime_bounds),
						 item.get_locus ());
}

void
ASTLowerWhereClauseItem::visit (AST::TypeBoundWhereClauseItem &item)
{
  // FIXME
  std::vector<HIR::LifetimeParam> for_lifetimes;

  for (auto &lifetime_param : item.get_for_lifetimes ())
    {
      auto generic_param = ASTLowerGenericParam::translate (lifetime_param);
      for_lifetimes.push_back (
	*static_cast<HIR::LifetimeParam *> (generic_param));
    }

  std::unique_ptr<HIR::Type> bound_type = std::unique_ptr<HIR::Type> (
    ASTLoweringType::translate (item.get_type ()));

  std::vector<std::unique_ptr<HIR::TypeParamBound>> type_param_bounds;
  for (auto &bound : item.get_type_param_bounds ())
    {
      HIR::TypeParamBound *b = ASTLoweringTypeBounds::translate (*bound);
      type_param_bounds.push_back (std::unique_ptr<HIR::TypeParamBound> (b));
    }

  auto crate_num = mappings->get_current_crate ();
  Analysis::NodeMapping mapping (crate_num, item.get_node_id (),
				 mappings->get_next_hir_id (crate_num),
				 UNKNOWN_LOCAL_DEFID);

  translated
    = new HIR::TypeBoundWhereClauseItem (mapping, std::move (for_lifetimes),
					 std::move (bound_type),
					 std::move (type_param_bounds),
					 item.get_locus ());
}

} // namespace HIR
} // namespace Rust
