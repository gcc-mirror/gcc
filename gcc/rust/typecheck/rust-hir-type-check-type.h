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

#ifndef RUST_HIR_TYPE_CHECK_TYPE
#define RUST_HIR_TYPE_CHECK_TYPE

#include "rust-hir-type-check-base.h"
#include "rust-hir-full.h"
#include "rust-substitution-mapper.h"

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
  static TyTy::BaseType *
  Resolve (HIR::Type *type,
	   std::vector<TyTy::SubstitutionParamMapping> *mappings = nullptr)
  {
    TypeCheckType resolver (mappings);
    type->accept_vis (resolver);

    if (resolver.translated == nullptr)
      return new TyTy::ErrorType (type->get_mappings ().get_hirid ());

    resolver.context->insert_type (type->get_mappings (), resolver.translated);
    return resolver.translated;
  }

  void visit (HIR::BareFunctionType &fntype) override
  {
    TyTy::BaseType *return_type
      = fntype.has_return_type ()
	  ? TypeCheckType::Resolve (fntype.get_return_type ().get ())
	  : new TyTy::TupleType (fntype.get_mappings ().get_hirid ());

    std::vector<TyTy::TyVar> params;
    for (auto &param : fntype.get_function_params ())
      {
	TyTy::BaseType *ptype
	  = TypeCheckType::Resolve (param.get_type ().get ());
	params.push_back (TyTy::TyVar (ptype->get_ref ()));
      }

    translated = new TyTy::FnPtr (fntype.get_mappings ().get_hirid (),
				  std::move (params),
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

    translated
      = new TyTy::TupleType (tuple.get_mappings ().get_hirid (), fields);
  }

  void visit (HIR::TypePath &path) override
  {
    // lookup the Node this resolves to
    NodeId ref;
    auto nid = path.get_mappings ().get_nodeid ();
    if (!resolver->lookup_resolved_type (nid, &ref))
      {
	rust_fatal_error (path.get_locus (),
			  "failed to resolve node '%d' to HIR", nid);
	return;
      }

    HirId hir_lookup;
    if (!context->lookup_type_by_node_id (ref, &hir_lookup))
      {
	rust_error_at (path.get_locus (),
		       "failed to lookup HIR %d for node '%s'", ref,
		       path.as_string ().c_str ());
	return;
      }

    TyTy::BaseType *lookup = nullptr;
    if (!context->lookup_type (hir_lookup, &lookup))
      {
	rust_error_at (path.get_locus (), "failed to lookup HIR TyTy");
	return;
      }

    TyTy::BaseType *path_type = lookup->clone ();
    path_type->set_ref (path.get_mappings ().get_hirid ());

    HIR::TypePathSegment *final_seg = path.get_final_segment ();
    HIR::GenericArgs args
      = TypeCheckResolveGenericArguments::resolve (final_seg);

    bool is_big_self = final_seg->is_ident_only ()
		       && (final_seg->as_string ().compare ("Self") == 0);

    if (path_type->needs_generic_substitutions ())
      {
	if (is_big_self)
	  {
	    translated = path_type;
	    return;
	  }

	translated = SubstMapper::Resolve (path_type, path.get_locus (), &args);
	if (translated->get_kind () != TyTy::TypeKind::ERROR
	    && mappings != nullptr)
	  {
	    check_for_unconstrained (args.get_type_args ());
	  }
      }
    else if (!args.is_empty ())
      {
	rust_error_at (path.get_locus (),
		       "TypePath %s declares generic arguments but "
		       "the type %s does not have any",
		       path.as_string ().c_str (),
		       translated->as_string ().c_str ());
      }
    else
      {
	translated = path_type;
      }
  }

  void visit (HIR::ArrayType &type) override;

  void visit (HIR::ReferenceType &type) override
  {
    TyTy::BaseType *base
      = TypeCheckType::Resolve (type.get_base_type ().get ());
    translated = new TyTy::ReferenceType (type.get_mappings ().get_hirid (),
					  TyTy::TyVar (base->get_ref ()));
  }

  void visit (HIR::InferredType &type) override
  {
    translated = new TyTy::InferType (type.get_mappings ().get_hirid (),
				      TyTy::InferType::InferTypeKind::GENERAL);
  }

private:
  TypeCheckType (std::vector<TyTy::SubstitutionParamMapping> *mappings)
    : TypeCheckBase (), mappings (mappings), translated (nullptr)
  {}

  void
  check_for_unconstrained (std::vector<std::unique_ptr<HIR::Type> > &type_args)
  {
    std::map<std::string, Location> param_location_map;
    std::set<std::string> param_tys;
    for (auto &mapping : *mappings)
      {
	std::string sym = mapping.get_param_ty ()->get_symbol ();
	param_tys.insert (sym);
	param_location_map[sym] = mapping.get_generic_param ().get_locus ();
      }

    std::set<std::string> args;
    for (auto &arg : type_args)
      args.insert (arg->as_string ());

    for (auto &exp : param_tys)
      {
	bool used = args.find (exp) != args.end ();
	if (!used)
	  {
	    Location locus = param_location_map.at (exp);
	    rust_error_at (locus, "unconstrained type parameter");
	  }
      }
  }

  std::vector<TyTy::SubstitutionParamMapping> *mappings;
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
	rust_error_at (param->get_locus_slow (),
		       "failed to setup generic parameter");
	return nullptr;
      }

    return resolver.resolved;
  }

  void visit (HIR::TypeParam &param) override
  {
    if (param.has_type ())
      TypeCheckType::Resolve (param.get_type ().get ());

    resolved = new TyTy::ParamType (param.get_type_representation (),
				    param.get_mappings ().get_hirid (), param);
  }

private:
  TypeResolveGenericParam () : TypeCheckBase (), resolved (nullptr) {}

  TyTy::ParamType *resolved;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_TYPE_CHECK_TYPE
