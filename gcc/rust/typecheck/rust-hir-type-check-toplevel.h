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

  void visit (HIR::ConstantItem &constant)
  {
    TyTy::TyBase *type = TypeCheckType::Resolve (constant.get_type ());
    TyTy::TyBase *expr_type = TypeCheckExpr::Resolve (constant.get_expr ());

    context->insert_type (constant.get_mappings ().get_hirid (),
			  type->combine (expr_type));
  }

  void visit (HIR::Function &function)
  {
    TyTy::TyBase *ret_type = nullptr;
    if (!function.has_function_return_type ())
      ret_type = new TyTy::UnitType (function.get_mappings ().get_hirid ());
    else
      ret_type = TypeCheckType::Resolve (function.return_type.get ());

    std::vector<TyTy::ParamType *> params;
    for (auto &param : function.function_params)
      {
	// get the name as well required for later on
	auto param_type = TypeCheckType::Resolve (param.type.get ());
	auto param_tyty
	  = new TyTy::ParamType (param.get_mappings ()->get_hirid (),
				 param.param_name->as_string (), param_type);
	params.push_back (param_tyty);

	context->insert_type (param.get_mappings ()->get_hirid (), param_tyty);
      }

    auto fnType = new TyTy::FnType (function.get_mappings ().get_hirid (),
				    params, ret_type);
    context->insert_type (function.get_mappings ().get_hirid (), fnType);
  }

private:
  TypeCheckTopLevel () : TypeCheckBase () {}
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_TYPE_CHECK_TOPLEVEL
