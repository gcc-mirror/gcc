// Copyright (C) 2020-2022 Free Software Foundation, Inc.

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

#ifndef RUST_HIR_TYPE_CHECK_TYPE
#define RUST_HIR_TYPE_CHECK_TYPE

#include "rust-hir-type-check-base.h"
#include "rust-hir-full.h"
#include "rust-substitution-mapper.h"
#include "rust-hir-path-probe.h"

namespace Rust {
namespace Resolver {

class TypeCheckResolveGenericArguments : public TypeCheckBase
{
  using Rust::Resolver::TypeCheckBase::visit;

public:
  static HIR::GenericArgs resolve (HIR::TypePathSegment *segment)
  {
    TypeCheckResolveGenericArguments resolver (segment->get_locus ());
    segment->accept_vis (resolver);
    return resolver.args;
  };

  void visit (HIR::TypePathSegmentGeneric &generic) override
  {
    args = generic.get_generic_args ();
  }

private:
  TypeCheckResolveGenericArguments (Location locus)
    : TypeCheckBase (), args (HIR::GenericArgs::create_empty (locus))
  {}

  HIR::GenericArgs args;
};

class TypeCheckType : public TypeCheckBase
{
  using Rust::Resolver::TypeCheckBase::visit;

public:
  static TyTy::BaseType *Resolve (HIR::Type *type)
  {
    TypeCheckType resolver (type->get_mappings ().get_hirid ());
    type->accept_vis (resolver);
    rust_assert (resolver.translated != nullptr);
    resolver.context->insert_type (type->get_mappings (), resolver.translated);
    return resolver.translated;
  }

  void visit (HIR::BareFunctionType &fntype) override
  {
    TyTy::BaseType *return_type
      = fntype.has_return_type ()
	  ? TypeCheckType::Resolve (fntype.get_return_type ().get ())
	  : TyTy::TupleType::get_unit_type (
	    fntype.get_mappings ().get_hirid ());

    std::vector<TyTy::TyVar> params;
    for (auto &param : fntype.get_function_params ())
      {
	TyTy::BaseType *ptype
	  = TypeCheckType::Resolve (param.get_type ().get ());
	params.push_back (TyTy::TyVar (ptype->get_ref ()));
      }

    translated = new TyTy::FnPtr (fntype.get_mappings ().get_hirid (),
				  fntype.get_locus (), std::move (params),
				  TyTy::TyVar (return_type->get_ref ()));
  }

  void visit (HIR::TupleType &tuple) override
  {
    if (tuple.is_unit_type ())
      {
	auto unit_node_id = resolver->get_unit_type_node_id ();
	if (!context->lookup_builtin (unit_node_id, &translated))
	  {
	    rust_error_at (tuple.get_locus (),
			   "failed to lookup builtin unit type");
	  }
	return;
      }

    std::vector<TyTy::TyVar> fields;
    for (auto &elem : tuple.get_elems ())
      {
	auto field_ty = TypeCheckType::Resolve (elem.get ());
	fields.push_back (TyTy::TyVar (field_ty->get_ref ()));
      }

    translated = new TyTy::TupleType (tuple.get_mappings ().get_hirid (),
				      tuple.get_locus (), fields);
  }

  void visit (HIR::TypePath &path) override;

  void visit (HIR::QualifiedPathInType &path) override;

  void visit (HIR::ArrayType &type) override;

  void visit (HIR::SliceType &type) override;

  void visit (HIR::ReferenceType &type) override
  {
    TyTy::BaseType *base
      = TypeCheckType::Resolve (type.get_base_type ().get ());
    translated = new TyTy::ReferenceType (type.get_mappings ().get_hirid (),
					  TyTy::TyVar (base->get_ref ()),
					  type.get_mut ());
  }

  void visit (HIR::RawPointerType &type) override
  {
    TyTy::BaseType *base
      = TypeCheckType::Resolve (type.get_base_type ().get ());
    translated
      = new TyTy::PointerType (type.get_mappings ().get_hirid (),
			       TyTy::TyVar (base->get_ref ()), type.get_mut ());
  }

  void visit (HIR::InferredType &type) override
  {
    translated = new TyTy::InferType (type.get_mappings ().get_hirid (),
				      TyTy::InferType::InferTypeKind::GENERAL,
				      type.get_locus ());
  }

  void visit (HIR::NeverType &type) override
  {
    TyTy::BaseType *lookup = nullptr;
    bool ok = context->lookup_builtin ("!", &lookup);
    rust_assert (ok);

    translated = lookup->clone ();
  }

  void visit (HIR::TraitObjectType &type) override;

private:
  TypeCheckType (HirId id)
    : TypeCheckBase (), translated (new TyTy::ErrorType (id))
  {}

  TyTy::BaseType *resolve_root_path (HIR::TypePath &path, size_t *offset,
				     NodeId *root_resolved_node_id);

  TyTy::BaseType *resolve_segments (
    NodeId root_resolved_node_id, HirId expr_id,
    std::vector<std::unique_ptr<HIR::TypePathSegment>> &segments, size_t offset,
    TyTy::BaseType *tyseg, const Analysis::NodeMapping &expr_mappings,
    Location expr_locus);

  TyTy::BaseType *translated;
};

class TypeResolveGenericParam : public TypeCheckBase
{
  using Rust::Resolver::TypeCheckBase::visit;

public:
  static TyTy::ParamType *Resolve (HIR::GenericParam *param)
  {
    TypeResolveGenericParam resolver;
    param->accept_vis (resolver);

    if (resolver.resolved == nullptr)
      {
	rust_error_at (param->get_locus (),
		       "failed to setup generic parameter");
	return nullptr;
      }

    return resolver.resolved;
  }

  void visit (HIR::TypeParam &param) override
  {
    if (param.has_type ())
      TypeCheckType::Resolve (param.get_type ().get ());

    std::vector<TyTy::TypeBoundPredicate> specified_bounds;
    if (param.has_type_param_bounds ())
      {
	for (auto &bound : param.get_type_param_bounds ())
	  {
	    switch (bound->get_bound_type ())
	      {
		case HIR::TypeParamBound::BoundType::TRAITBOUND: {
		  HIR::TraitBound *b
		    = static_cast<HIR::TraitBound *> (bound.get ());

		  TyTy::TypeBoundPredicate predicate
		    = get_predicate_from_bound (b->get_path ());
		  if (!predicate.is_error ())
		    specified_bounds.push_back (std::move (predicate));
		}
		break;

	      default:
		break;
	      }
	  }
      }

    resolved = new TyTy::ParamType (param.get_type_representation (),
				    param.get_locus (),
				    param.get_mappings ().get_hirid (), param,
				    specified_bounds);
  }

private:
  TypeResolveGenericParam () : TypeCheckBase (), resolved (nullptr) {}

  TyTy::ParamType *resolved;
};

class ResolveWhereClauseItem : public TypeCheckBase
{
  using Rust::Resolver::TypeCheckBase::visit;

public:
  static void Resolve (HIR::WhereClauseItem &item)
  {
    ResolveWhereClauseItem resolver;
    item.accept_vis (resolver);
  }

  void visit (HIR::LifetimeWhereClauseItem &) override {}

  void visit (HIR::TypeBoundWhereClauseItem &item) override
  {
    auto &binding_type_path = item.get_bound_type ();
    TyTy::BaseType *binding = TypeCheckType::Resolve (binding_type_path.get ());

    std::vector<TyTy::TypeBoundPredicate> specified_bounds;
    for (auto &bound : item.get_type_param_bounds ())
      {
	switch (bound->get_bound_type ())
	  {
	    case HIR::TypeParamBound::BoundType::TRAITBOUND: {
	      HIR::TraitBound *b
		= static_cast<HIR::TraitBound *> (bound.get ());

	      TyTy::TypeBoundPredicate predicate
		= get_predicate_from_bound (b->get_path ());
	      if (!predicate.is_error ())
		specified_bounds.push_back (std::move (predicate));
	    }
	    break;

	  default:
	    break;
	  }
      }
    binding->inherit_bounds (specified_bounds);

    // When we apply these bounds we must lookup which type this binding
    // resolves to, as this is the type which will be used during resolution
    // of the block.
    NodeId ast_node_id = binding_type_path->get_mappings ().get_nodeid ();

    // then lookup the reference_node_id
    NodeId ref_node_id = UNKNOWN_NODEID;
    if (!resolver->lookup_resolved_type (ast_node_id, &ref_node_id))
      {
	// FIXME
	rust_error_at (Location (),
		       "Failed to lookup type reference for node: %s",
		       binding_type_path->as_string ().c_str ());
	return;
      }

    // node back to HIR
    HirId ref;
    if (!mappings->lookup_node_to_hir (ref_node_id, &ref))
      {
	// FIXME
	rust_error_at (Location (), "where-clause reverse lookup failure");
	return;
      }

    // the base reference for this name _must_ have a type set
    TyTy::BaseType *lookup;
    if (!context->lookup_type (ref, &lookup))
      {
	rust_error_at (mappings->lookup_location (ref),
		       "Failed to resolve where-clause binding type: %s",
		       binding_type_path->as_string ().c_str ());
	return;
      }

    // FIXME
    // rust_assert (binding->is_equal (*lookup));
    lookup->inherit_bounds (specified_bounds);
  }

private:
  ResolveWhereClauseItem () : TypeCheckBase () {}
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_TYPE_CHECK_TYPE
