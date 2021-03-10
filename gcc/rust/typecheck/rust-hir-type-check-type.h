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

class ArrayCapacityConstant : public TypeCheckBase
{
  using Rust::Resolver::TypeCheckBase::visit;

public:
  static bool fold (HIR::Expr *expr, size_t *folded_result)
  {
    ArrayCapacityConstant folder;
    expr->accept_vis (folder);
    *folded_result = folder.result;
    return folder.ok;
  }

  virtual ~ArrayCapacityConstant () {}

  void visit (HIR::LiteralExpr &expr) override
  {
    auto literal_value = expr.get_literal ();
    switch (expr.get_lit_type ())
      {
	case HIR::Literal::LitType::INT: {
	  ok = true;
	  std::stringstream ss (literal_value->as_string ());
	  ss >> result;
	}
	break;

      default:
	return;
      }
  }

private:
  ArrayCapacityConstant () : TypeCheckBase (), ok (false), result (-1) {}

  bool ok;
  size_t result;
}; // namespace Resolver

class TypeCheckResolveGenericArguments : public TypeCheckBase
{
  using Rust::Resolver::TypeCheckBase::visit;

public:
  static HIR::GenericArgs resolve (HIR::TypePathSegment *segment)
  {
    TypeCheckResolveGenericArguments resolver;
    segment->accept_vis (resolver);
    return resolver.args;
  };

  void visit (HIR::TypePathSegmentGeneric &generic) override
  {
    args = generic.get_generic_args ();
  }

private:
  TypeCheckResolveGenericArguments ()
    : TypeCheckBase (), args (HIR::GenericArgs::create_empty ())
  {}

  HIR::GenericArgs args;
};

class TypeCheckType : public TypeCheckBase
{
  using Rust::Resolver::TypeCheckBase::visit;

public:
  static TyTy::BaseType *Resolve (HIR::Type *type)
  {
    TypeCheckType resolver;
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
	  : new TyTy::UnitType (fntype.get_mappings ().get_hirid ());

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
    if (!resolver->lookup_resolved_type (path.get_mappings ().get_nodeid (),
					 &ref))
      {
	rust_fatal_error (path.get_locus (),
			  "Failed to resolve node id to HIR");
	return;
      }

    // reverse lookup the hir node from ast node id
    HirId hir_lookup;
    if (context->lookup_type_by_node_id (ref, &hir_lookup))
      {
	// we got an HIR node
	if (context->lookup_type (hir_lookup, &translated))
	  {
	    translated = translated->clone ();
	    auto ref = path.get_mappings ().get_hirid ();
	    translated->set_ref (ref);

	    HIR::TypePathSegment *final_seg = path.get_final_segment ();
	    HIR::GenericArgs args
	      = TypeCheckResolveGenericArguments::resolve (final_seg);

	    bool path_declared_generic_arguments = !args.is_empty ();
	    if (path_declared_generic_arguments)
	      {
		if (translated->has_subsititions_defined ())
		  {
		    translated
		      = SubstMapper::Resolve (translated, path.get_locus (),
					      &args);
		  }
		else
		  {
		    rust_error_at (
		      path.get_locus (),
		      "TypePath %s declares generic argument's but "
		      "the type %s does not have any",
		      path.as_string ().c_str (),
		      translated->as_string ().c_str ());
		    return;
		  }
	      }
	    else if (translated->has_subsititions_defined ())
	      {
		translated
		  = SubstMapper::Resolve (translated, path.get_locus ());
	      }
	    return;
	  }
      }

    rust_error_at (path.get_locus (), "failed to type-resolve TypePath: %s",
		   path.as_string ().c_str ());
  }

  void visit (HIR::ArrayType &type) override
  {
    size_t capacity;
    if (!ArrayCapacityConstant::fold (type.get_size_expr (), &capacity))
      {
	rust_error_at (type.get_size_expr ()->get_locus_slow (),
		       "non-constant value");
	return;
      }

    TyTy::BaseType *base = TypeCheckType::Resolve (type.get_element_type ());
    translated = new TyTy::ArrayType (type.get_mappings ().get_hirid (),
				      capacity, TyTy::TyVar (base->get_ref ()));
  }

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
  TypeCheckType () : TypeCheckBase (), translated (nullptr) {}

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
