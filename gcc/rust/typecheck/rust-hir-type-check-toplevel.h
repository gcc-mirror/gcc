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

#ifndef RUST_HIR_TYPE_CHECK_TOPLEVEL
#define RUST_HIR_TYPE_CHECK_TOPLEVEL

#include "rust-hir-type-check-base.h"
#include "rust-hir-full.h"
#include "rust-hir-type-check-implitem.h"
#include "rust-hir-type-check-type.h"
#include "rust-hir-type-check-expr.h"
#include "rust-tyty.h"

namespace Rust {
namespace Resolver {

class TypeCheckTopLevel : public TypeCheckBase
{
  using Rust::Resolver::TypeCheckBase::visit;

public:
  static void Resolve (HIR::Item *item)
  {
    TypeCheckTopLevel resolver;
    item->accept_vis (resolver);
  }

  void visit (HIR::TypeAlias &alias) override
  {
    TyTy::BaseType *actual_type
      = TypeCheckType::Resolve (alias.get_type_aliased ().get ());

    context->insert_type (alias.get_mappings (), actual_type);
  }

  void visit (HIR::TupleStruct &struct_decl) override
  {
    std::vector<TyTy::SubstitutionParamMapping> substitutions;
    if (struct_decl.has_generics ())
      {
	for (auto &generic_param : struct_decl.get_generic_params ())
	  {
	    switch (generic_param.get ()->get_kind ())
	      {
	      case HIR::GenericParam::GenericKind::LIFETIME:
		// Skipping Lifetime completely until better handling.
		break;

		case HIR::GenericParam::GenericKind::TYPE: {
		  auto param_type
		    = TypeResolveGenericParam::Resolve (generic_param.get ());
		  context->insert_type (generic_param->get_mappings (),
					param_type);

		  substitutions.push_back (TyTy::SubstitutionParamMapping (
		    static_cast<HIR::TypeParam &> (*generic_param),
		    param_type));
		}
		break;
	      }
	  }
      }

    std::vector<TyTy::StructFieldType *> fields;

    size_t idx = 0;
    struct_decl.iterate ([&] (HIR::TupleField &field) mutable -> bool {
      TyTy::BaseType *field_type
	= TypeCheckType::Resolve (field.get_field_type ().get ());
      TyTy::StructFieldType *ty_field
	= new TyTy::StructFieldType (field.get_mappings ().get_hirid (),
				     std::to_string (idx), field_type);
      fields.push_back (ty_field);
      context->insert_type (field.get_mappings (), ty_field->get_field_type ());
      idx++;
      return true;
    });

    TyTy::BaseType *type
      = new TyTy::ADTType (struct_decl.get_mappings ().get_hirid (),
			   mappings->get_next_hir_id (),
			   struct_decl.get_identifier (), true,
			   std::move (fields), std::move (substitutions));

    context->insert_type (struct_decl.get_mappings (), type);
  }

  void visit (HIR::StructStruct &struct_decl) override
  {
    std::vector<TyTy::SubstitutionParamMapping> substitutions;
    if (struct_decl.has_generics ())
      {
	for (auto &generic_param : struct_decl.get_generic_params ())
	  {
	    switch (generic_param.get ()->get_kind ())
	      {
	      case HIR::GenericParam::GenericKind::LIFETIME:
		// Skipping Lifetime completely until better handling.
		break;

		case HIR::GenericParam::GenericKind::TYPE: {
		  auto param_type
		    = TypeResolveGenericParam::Resolve (generic_param.get ());
		  context->insert_type (generic_param->get_mappings (),
					param_type);

		  substitutions.push_back (TyTy::SubstitutionParamMapping (
		    static_cast<HIR::TypeParam &> (*generic_param),
		    param_type));
		}
		break;
	      }
	  }
      }

    std::vector<TyTy::StructFieldType *> fields;
    struct_decl.iterate ([&] (HIR::StructField &field) mutable -> bool {
      TyTy::BaseType *field_type
	= TypeCheckType::Resolve (field.get_field_type ().get ());
      TyTy::StructFieldType *ty_field
	= new TyTy::StructFieldType (field.get_mappings ().get_hirid (),
				     field.get_field_name (), field_type);
      fields.push_back (ty_field);
      context->insert_type (field.get_mappings (), ty_field->get_field_type ());
      return true;
    });

    TyTy::BaseType *type
      = new TyTy::ADTType (struct_decl.get_mappings ().get_hirid (),
			   mappings->get_next_hir_id (),
			   struct_decl.get_identifier (), false,
			   std::move (fields), std::move (substitutions));

    context->insert_type (struct_decl.get_mappings (), type);
  }

  void visit (HIR::StaticItem &var) override
  {
    TyTy::BaseType *type = TypeCheckType::Resolve (var.get_type ());
    TyTy::BaseType *expr_type = TypeCheckExpr::Resolve (var.get_expr (), false);

    context->insert_type (var.get_mappings (), type->unify (expr_type));
  }

  void visit (HIR::ConstantItem &constant) override
  {
    TyTy::BaseType *type = TypeCheckType::Resolve (constant.get_type ());
    TyTy::BaseType *expr_type
      = TypeCheckExpr::Resolve (constant.get_expr (), false);

    context->insert_type (constant.get_mappings (), type->unify (expr_type));

    // notify the constant folder of this
    ConstFold::ConstFoldItem::fold (constant);
  }

  void visit (HIR::Function &function) override
  {
    std::vector<TyTy::SubstitutionParamMapping> substitutions;
    if (function.has_generics ())
      {
	for (auto &generic_param : function.get_generic_params ())
	  {
	    switch (generic_param.get ()->get_kind ())
	      {
	      case HIR::GenericParam::GenericKind::LIFETIME:
		// Skipping Lifetime completely until better handling.
		break;

		case HIR::GenericParam::GenericKind::TYPE: {
		  auto param_type
		    = TypeResolveGenericParam::Resolve (generic_param.get ());
		  context->insert_type (generic_param->get_mappings (),
					param_type);

		  substitutions.push_back (TyTy::SubstitutionParamMapping (
		    static_cast<HIR::TypeParam &> (*generic_param),
		    param_type));
		}
		break;
	      }
	  }
      }

    TyTy::BaseType *ret_type = nullptr;
    if (!function.has_function_return_type ())
      ret_type = new TyTy::TupleType (function.get_mappings ().get_hirid ());
    else
      {
	auto resolved
	  = TypeCheckType::Resolve (function.get_return_type ().get ());
	if (resolved == nullptr)
	  {
	    rust_error_at (function.get_locus (),
			   "failed to resolve return type");
	    return;
	  }

	ret_type = resolved->clone ();
	ret_type->set_ref (
	  function.get_return_type ()->get_mappings ().get_hirid ());
      }

    std::vector<std::pair<HIR::Pattern *, TyTy::BaseType *> > params;
    for (auto &param : function.get_function_params ())
      {
	// get the name as well required for later on
	auto param_tyty = TypeCheckType::Resolve (param.get_type ());
	params.push_back (
	  std::pair<HIR::Pattern *, TyTy::BaseType *> (param.get_param_name (),
						       param_tyty));

	context->insert_type (param.get_mappings (), param_tyty);
      }

    auto fnType = new TyTy::FnType (function.get_mappings ().get_hirid (),
				    function.get_function_name (), false,
				    std::move (params), ret_type,
				    std::move (substitutions));
    context->insert_type (function.get_mappings (), fnType);
  }

  void visit (HIR::ImplBlock &impl_block) override
  {
    std::vector<TyTy::SubstitutionParamMapping> substitutions;
    if (impl_block.has_generics ())
      {
	for (auto &generic_param : impl_block.get_generic_params ())
	  {
	    switch (generic_param.get ()->get_kind ())
	      {
	      case HIR::GenericParam::GenericKind::LIFETIME:
		// Skipping Lifetime completely until better handling.
		break;

		case HIR::GenericParam::GenericKind::TYPE: {
		  auto param_type
		    = TypeResolveGenericParam::Resolve (generic_param.get ());
		  context->insert_type (generic_param->get_mappings (),
					param_type);

		  substitutions.push_back (TyTy::SubstitutionParamMapping (
		    static_cast<HIR::TypeParam &> (*generic_param),
		    param_type));
		}
		break;
	      }
	  }
      }

    auto self
      = TypeCheckType::Resolve (impl_block.get_type ().get (), &substitutions);
    if (self == nullptr || self->get_kind () == TyTy::TypeKind::ERROR)
      return;

    for (auto &impl_item : impl_block.get_impl_items ())
      TypeCheckTopLevelImplItem::Resolve (impl_item.get (), self,
					  substitutions);
  }

private:
  TypeCheckTopLevel () : TypeCheckBase () {}
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_TYPE_CHECK_TOPLEVEL
