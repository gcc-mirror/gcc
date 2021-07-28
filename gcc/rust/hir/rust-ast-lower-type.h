// Copyright (C) 2020 Free Software Foundation, Inc.

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

#ifndef RUST_AST_LOWER_TYPE
#define RUST_AST_LOWER_TYPE

#include "rust-ast-lower-base.h"
#include "rust-diagnostics.h"
#include "rust-ast-lower-expr.h"

namespace Rust {
namespace HIR {

class ASTLowerTypePath : public ASTLoweringBase
{
  using Rust::HIR::ASTLoweringBase::visit;

public:
  static HIR::TypePath *translate (AST::TypePath &type)
  {
    ASTLowerTypePath resolver;
    type.accept_vis (resolver);
    rust_assert (resolver.translated != nullptr);
    return resolver.translated;
  }

  void visit (AST::TypePathSegmentFunction &) override { gcc_unreachable (); }

  void visit (AST::TypePathSegment &segment) override
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

  void visit (AST::TypePathSegmentGeneric &segment) override;

  void visit (AST::TypePath &path) override
  {
    std::vector<std::unique_ptr<HIR::TypePathSegment> > translated_segments;

    path.iterate_segments ([&] (AST::TypePathSegment *seg) mutable -> bool {
      translated_segment = nullptr;
      seg->accept_vis (*this);
      if (translated_segment == nullptr)
	{
	  rust_fatal_error (seg->get_locus (),
			    "failed to translate AST TypePathSegment");
	  return false;
	}

      translated_segments.push_back (
	std::unique_ptr<HIR::TypePathSegment> (translated_segment));
      return true;
    });

    auto crate_num = mappings->get_current_crate ();
    auto hirid = mappings->get_next_hir_id (crate_num);
    Analysis::NodeMapping mapping (crate_num, path.get_node_id (), hirid,
				   mappings->get_next_localdef_id (crate_num));

    translated
      = new HIR::TypePath (std::move (mapping), std::move (translated_segments),
			   path.get_locus (),
			   path.has_opening_scope_resolution_op ());
    mappings->insert_hir_type (crate_num, hirid, translated);
  }

private:
  HIR::TypePath *translated;
  HIR::TypePathSegment *translated_segment;
};

class ASTLoweringType : public ASTLoweringBase
{
  using Rust::HIR::ASTLoweringBase::visit;

public:
  static HIR::Type *translate (AST::Type *type)
  {
    ASTLoweringType resolver;
    type->accept_vis (resolver);

    rust_assert (resolver.translated != nullptr);
    resolver.mappings->insert_location (
      resolver.translated->get_mappings ().get_crate_num (),
      resolver.translated->get_mappings ().get_hirid (),
      type->get_locus_slow ());

    return resolver.translated;
  }

  void visit (AST::BareFunctionType &fntype) override
  {
    bool is_variadic = false;
    std::vector<HIR::LifetimeParam> lifetime_params;
    HIR::FunctionQualifiers qualifiers (
      HIR::FunctionQualifiers::AsyncConstStatus::NONE, false);

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
	    gcc_unreachable ();
	  }

	HIR::Type *param_type
	  = ASTLoweringType::translate (param.get_type ().get ());

	HIR::MaybeNamedParam p (param.get_name (), kind,
				std::unique_ptr<HIR::Type> (param_type),
				param.get_locus ());
	named_params.push_back (std::move (p));
      }

    HIR::Type *return_type = nullptr;
    if (fntype.has_return_type ())
      {
	return_type
	  = ASTLoweringType::translate (fntype.get_return_type ().get ());
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

  void visit (AST::TupleType &tuple) override
  {
    std::vector<std::unique_ptr<HIR::Type> > elems;
    for (auto &e : tuple.get_elems ())
      {
	HIR::Type *t = ASTLoweringType::translate (e.get ());
	elems.push_back (std::unique_ptr<HIR::Type> (t));
      }

    auto crate_num = mappings->get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, tuple.get_node_id (),
				   mappings->get_next_hir_id (crate_num),
				   mappings->get_next_localdef_id (crate_num));

    translated = new HIR::TupleType (std::move (mapping), std::move (elems),
				     tuple.get_locus ());
  }

  void visit (AST::TypePath &path) override
  {
    translated = ASTLowerTypePath::translate (path);
  }

  void visit (AST::ArrayType &type) override
  {
    HIR::Type *translated_type
      = ASTLoweringType::translate (type.get_elem_type ().get ());
    HIR::Expr *array_size
      = ASTLoweringExpr::translate (type.get_size_expr ().get ());

    auto crate_num = mappings->get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, type.get_node_id (),
				   mappings->get_next_hir_id (crate_num),
				   mappings->get_next_localdef_id (crate_num));

    translated
      = new HIR::ArrayType (mapping,
			    std::unique_ptr<HIR::Type> (translated_type),
			    std::unique_ptr<HIR::Expr> (array_size),
			    type.get_locus ());
    mappings->insert_hir_type (mapping.get_crate_num (), mapping.get_hirid (),
			       translated);
  }

  void visit (AST::ReferenceType &type) override
  {
    HIR::Lifetime lifetime = lower_lifetime (type.get_lifetime ());

    HIR::Type *base_type
      = ASTLoweringType::translate (type.get_base_type ().get ());

    auto crate_num = mappings->get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, type.get_node_id (),
				   mappings->get_next_hir_id (crate_num),
				   mappings->get_next_localdef_id (crate_num));

    translated = new HIR::ReferenceType (mapping, type.get_has_mut (),
					 std::unique_ptr<HIR::Type> (base_type),
					 type.get_locus (), lifetime);

    mappings->insert_hir_type (mapping.get_crate_num (), mapping.get_hirid (),
			       translated);
  }

  void visit (AST::RawPointerType &type) override
  {
    HIR::Type *base_type
      = ASTLoweringType::translate (type.get_type_pointed_to ().get ());

    auto crate_num = mappings->get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, type.get_node_id (),
				   mappings->get_next_hir_id (crate_num),
				   mappings->get_next_localdef_id (crate_num));

    translated
      = new HIR::RawPointerType (mapping,
				 type.get_pointer_type ()
				   == AST::RawPointerType::PointerType::MUT,
				 std::unique_ptr<HIR::Type> (base_type),
				 type.get_locus ());

    mappings->insert_hir_type (mapping.get_crate_num (), mapping.get_hirid (),
			       translated);
  }

  void visit (AST::InferredType &type) override
  {
    auto crate_num = mappings->get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, type.get_node_id (),
				   mappings->get_next_hir_id (crate_num),
				   mappings->get_next_localdef_id (crate_num));

    translated = new HIR::InferredType (mapping, type.get_locus ());

    mappings->insert_hir_type (mapping.get_crate_num (), mapping.get_hirid (),
			       translated);
  }

private:
  ASTLoweringType () : ASTLoweringBase (), translated (nullptr) {}

  HIR::Type *translated;
};

class ASTLowerGenericParam : public ASTLoweringBase
{
  using Rust::HIR::ASTLoweringBase::visit;

public:
  static HIR::GenericParam *translate (AST::GenericParam *param)
  {
    ASTLowerGenericParam resolver;
    param->accept_vis (resolver);

    rust_assert (resolver.translated != nullptr);
    resolver.mappings->insert_location (
      resolver.translated->get_mappings ().get_crate_num (),
      resolver.translated->get_mappings ().get_hirid (),
      param->get_locus_slow ());

    return resolver.translated;
  }

  void visit (AST::LifetimeParam &param) override
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

  void visit (AST::TypeParam &param) override
  {
    AST::Attribute outer_attr = AST::Attribute::create_empty ();
    std::vector<std::unique_ptr<HIR::TypeParamBound> > type_param_bounds;
    if (param.has_type_param_bounds ())
      {
	for (auto &bound : param.get_type_param_bounds ())
	  {
	    HIR::TypeParamBound *lowered_bound = lower_bound (bound.get ());
	    type_param_bounds.push_back (
	      std::unique_ptr<HIR::TypeParamBound> (lowered_bound));
	  }
      }

    HIR::Type *type = param.has_type ()
			? ASTLoweringType::translate (param.get_type ().get ())
			: nullptr;

    auto crate_num = mappings->get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, param.get_node_id (),
				   mappings->get_next_hir_id (crate_num),
				   mappings->get_next_localdef_id (crate_num));

    translated
      = new HIR::TypeParam (mapping, param.get_type_representation (),
			    param.get_locus (), std::move (type_param_bounds),
			    std::unique_ptr<Type> (type),
			    std::move (outer_attr));
  }

private:
  ASTLowerGenericParam () : ASTLoweringBase (), translated (nullptr) {}

  HIR::GenericParam *translated;
};

class ASTLoweringTypeBounds : public ASTLoweringBase
{
  using Rust::HIR::ASTLoweringBase::visit;

public:
  static HIR::TypeParamBound *translate (AST::TypeParamBound *type)
  {
    ASTLoweringTypeBounds resolver;
    type->accept_vis (resolver);

    rust_assert (resolver.translated != nullptr);
    resolver.mappings->insert_location (
      resolver.translated->get_mappings ().get_crate_num (),
      resolver.translated->get_mappings ().get_hirid (),
      resolver.translated->get_locus_slow ());

    return resolver.translated;
  }

  void visit (AST::TraitBound &bound) override
  {
    // FIXME
    std::vector<HIR::LifetimeParam> lifetimes;

    AST::TypePath &ast_trait_path = bound.get_type_path ();
    HIR::TypePath *trait_path = ASTLowerTypePath::translate (ast_trait_path);

    auto crate_num = mappings->get_current_crate ();
    Analysis::NodeMapping mapping (crate_num, bound.get_node_id (),
				   mappings->get_next_hir_id (crate_num),
				   UNKNOWN_LOCAL_DEFID);

    translated = new HIR::TraitBound (mapping, *trait_path, bound.get_locus (),
				      bound.is_in_parens (),
				      bound.has_opening_question_mark ());
  }

  void visit (AST::Lifetime &bound) override
  {
    HIR::Lifetime lifetime = lower_lifetime (bound);
    translated = new HIR::Lifetime (lifetime);
  }

private:
  ASTLoweringTypeBounds () : ASTLoweringBase (), translated (nullptr) {}

  HIR::TypeParamBound *translated;
};

} // namespace HIR
} // namespace Rust

#endif // RUST_AST_LOWER_TYPE
