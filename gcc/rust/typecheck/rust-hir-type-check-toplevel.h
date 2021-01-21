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
#include "rust-hir-type-check-type.h"
#include "rust-hir-type-check-expr.h"
#include "rust-tyty.h"

namespace Rust {
namespace Resolver {

class TypeCheckTopLevel : public TypeCheckBase
{
public:
  static void Resolve (HIR::Item *item)
  {
    TypeCheckTopLevel resolver;
    item->accept_vis (resolver);
  }

  void visit (HIR::TupleStruct &struct_decl)
  {
    std::vector<TyTy::StructFieldType *> fields;

    size_t idx = 0;
    struct_decl.iterate ([&] (HIR::TupleField &field) mutable -> bool {
      TyTy::TyBase *field_type
	= TypeCheckType::Resolve (field.get_field_type ().get ());
      TyTy::StructFieldType *ty_field
	= new TyTy::StructFieldType (field.get_mappings ().get_hirid (),
				     std::to_string (idx), field_type);
      fields.push_back (ty_field);
      context->insert_type (field.get_mappings (), ty_field->get_field_type ());
      idx++;
      return true;
    });

    TyTy::TyBase *type
      = new TyTy::ADTType (struct_decl.get_mappings ().get_hirid (),
			   struct_decl.get_identifier (), true,
			   std::move (fields));

    context->insert_type (struct_decl.get_mappings (), type);
  }

  void visit (HIR::StructStruct &struct_decl)
  {
    std::vector<TyTy::StructFieldType *> fields;
    struct_decl.iterate ([&] (HIR::StructField &field) mutable -> bool {
      TyTy::TyBase *field_type
	= TypeCheckType::Resolve (field.get_field_type ().get ());
      TyTy::StructFieldType *ty_field
	= new TyTy::StructFieldType (field.get_mappings ().get_hirid (),
				     field.get_field_name (), field_type);
      fields.push_back (ty_field);
      context->insert_type (field.get_mappings (), ty_field->get_field_type ());
      return true;
    });

    TyTy::TyBase *type
      = new TyTy::ADTType (struct_decl.get_mappings ().get_hirid (),
			   struct_decl.get_identifier (), false,
			   std::move (fields));

    context->insert_type (struct_decl.get_mappings (), type);
  }

  void visit (HIR::StaticItem &var)
  {
    TyTy::TyBase *type = TypeCheckType::Resolve (var.get_type ());
    TyTy::TyBase *expr_type = TypeCheckExpr::Resolve (var.get_expr ());

    context->insert_type (var.get_mappings (), type->combine (expr_type));
  }

  void visit (HIR::ConstantItem &constant)
  {
    TyTy::TyBase *type = TypeCheckType::Resolve (constant.get_type ());
    TyTy::TyBase *expr_type = TypeCheckExpr::Resolve (constant.get_expr ());

    context->insert_type (constant.get_mappings (), type->combine (expr_type));
  }

  void visit (HIR::Function &function)
  {
    TyTy::TyBase *ret_type = nullptr;
    if (!function.has_function_return_type ())
      ret_type = new TyTy::UnitType (function.get_mappings ().get_hirid ());
    else
      {
	TyTy::InferType infer (function.get_mappings ().get_hirid ());
	auto resolved = TypeCheckType::Resolve (function.return_type.get ());
	ret_type = infer.combine (resolved);
	ret_type->set_ref (function.return_type->get_mappings ().get_hirid ());
      }

    std::vector<std::pair<HIR::Pattern *, TyTy::TyBase *> > params;
    for (auto &param : function.function_params)
      {
	// get the name as well required for later on
	auto param_tyty = TypeCheckType::Resolve (param.get_type ());
	params.push_back (
	  std::pair<HIR::Pattern *, TyTy::TyBase *> (param.get_param_name (),
						     param_tyty));

	context->insert_type (param.get_mappings (), param_tyty);
      }

    auto fnType = new TyTy::FnType (function.get_mappings ().get_hirid (),
				    params, ret_type);
    context->insert_type (function.get_mappings (), fnType);
  }

private:
  TypeCheckTopLevel () : TypeCheckBase () {}
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_TYPE_CHECK_TOPLEVEL
