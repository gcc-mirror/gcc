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

#ifndef RUST_HIR_TYPE_CHECK_IMPLITEM_H
#define RUST_HIR_TYPE_CHECK_IMPLITEM_H

#include "rust-hir-type-check-base.h"
#include "rust-hir-full.h"
#include "rust-hir-type-check-type.h"
#include "rust-hir-type-check-expr.h"
#include "rust-tyty.h"

namespace Rust {
namespace Resolver {

class TypeCheckTopLevelImplItem : public TypeCheckBase
{
public:
  static void Resolve (HIR::InherentImplItem *item, TyTy::BaseType *self)
  {
    TypeCheckTopLevelImplItem resolver (self);
    item->accept_vis (resolver);
  }

  void visit (HIR::ConstantItem &constant)
  {
    TyTy::BaseType *type = TypeCheckType::Resolve (constant.get_type ());
    TyTy::BaseType *expr_type
      = TypeCheckExpr::Resolve (constant.get_expr (), false);

    context->insert_type (constant.get_mappings (), type->unify (expr_type));
  }

  void visit (HIR::Function &function)
  {
    TyTy::BaseType *ret_type = nullptr;
    if (!function.has_function_return_type ())
      ret_type = new TyTy::UnitType (function.get_mappings ().get_hirid ());
    else
      {
	auto resolved = TypeCheckType::Resolve (function.return_type.get ());
	if (resolved == nullptr)
	  {
	    rust_error_at (function.get_locus (),
			   "failed to resolve return type");
	    return;
	  }

	ret_type = resolved->clone ();
	ret_type->set_ref (function.return_type->get_mappings ().get_hirid ());
      }

    std::vector<std::pair<HIR::Pattern *, TyTy::BaseType *> > params;
    for (auto &param : function.function_params)
      {
	// get the name as well required for later on
	auto param_tyty = TypeCheckType::Resolve (param.get_type ());
	params.push_back (
	  std::pair<HIR::Pattern *, TyTy::BaseType *> (param.get_param_name (),
						       param_tyty));

	context->insert_type (param.get_mappings (), param_tyty);
      }

    auto fnType = new TyTy::FnType (function.get_mappings ().get_hirid (),
				    params, ret_type);
    context->insert_type (function.get_mappings (), fnType);
  }

  void visit (HIR::Method &method)
  {
    TyTy::BaseType *ret_type = nullptr;
    if (!method.has_function_return_type ())
      ret_type = new TyTy::UnitType (method.get_mappings ().get_hirid ());
    else
      {
	auto resolved
	  = TypeCheckType::Resolve (method.get_return_type ().get ());
	if (resolved == nullptr)
	  {
	    rust_error_at (method.get_locus (),
			   "failed to resolve return type");
	    return;
	  }

	ret_type = resolved->clone ();
	ret_type->set_ref (
	  method.get_return_type ()->get_mappings ().get_hirid ());
      }

    // hold all the params to the fndef
    std::vector<std::pair<HIR::Pattern *, TyTy::BaseType *> > params;

    // add the self param at the front
    HIR::SelfParam &self_param = method.get_self_param ();
    HIR::IdentifierPattern *self_pattern
      = new HIR::IdentifierPattern ("self", self_param.get_locus (),
				    self_param.get_has_ref (),
				    self_param.get_is_mut (),
				    std::unique_ptr<HIR::Pattern> (nullptr));
    context->insert_type (self_param.get_mappings (), self->clone ());
    params.push_back (
      std::pair<HIR::Pattern *, TyTy::BaseType *> (self_pattern,
						   self->clone ()));

    for (auto &param : method.get_function_params ())
      {
	// get the name as well required for later on
	auto param_tyty = TypeCheckType::Resolve (param.get_type ());
	params.push_back (
	  std::pair<HIR::Pattern *, TyTy::BaseType *> (param.get_param_name (),
						       param_tyty));

	context->insert_type (param.get_mappings (), param_tyty);
      }

    auto fnType = new TyTy::FnType (method.get_mappings ().get_hirid (), params,
				    ret_type);
    context->insert_type (method.get_mappings (), fnType);
  }

private:
  TypeCheckTopLevelImplItem (TyTy::BaseType *self)
    : TypeCheckBase (), self (self)
  {}

  TyTy::BaseType *self;
};

class TypeCheckImplItem : public TypeCheckBase
{
public:
  static void Resolve (HIR::InherentImplItem *item, TyTy::BaseType *self)
  {
    TypeCheckImplItem resolver (self);
    item->accept_vis (resolver);
  }

  void visit (HIR::Function &function)
  {
    TyTy::BaseType *lookup;
    if (!context->lookup_type (function.get_mappings ().get_hirid (), &lookup))
      {
	rust_error_at (function.get_locus (), "failed to lookup function type");
	return;
      }

    if (lookup->get_kind () != TyTy::TypeKind::FNDEF)
      {
	rust_error_at (function.get_locus (),
		       "found invalid type for function [%s]",
		       lookup->as_string ().c_str ());
	return;
      }

    // need to get the return type from this
    TyTy::FnType *resolve_fn_type = (TyTy::FnType *) lookup;
    auto expected_ret_tyty = resolve_fn_type->return_type ();
    context->push_return_type (expected_ret_tyty);

    auto result = TypeCheckExpr::Resolve (function.function_body.get (), false);
    auto ret_resolved = expected_ret_tyty->unify (result);
    if (ret_resolved == nullptr)
      return;

    context->peek_return_type ()->append_reference (ret_resolved->get_ref ());

    context->pop_return_type ();
  }

  void visit (HIR::Method &method)
  {
    TyTy::BaseType *lookup;
    if (!context->lookup_type (method.get_mappings ().get_hirid (), &lookup))
      {
	rust_error_at (method.get_locus (), "failed to lookup function type");
	return;
      }

    if (lookup->get_kind () != TyTy::TypeKind::FNDEF)
      {
	rust_error_at (method.get_locus (),
		       "found invalid type for function [%s]",
		       lookup->as_string ().c_str ());
	return;
      }

    // need to get the return type from this
    TyTy::FnType *resolve_fn_type = (TyTy::FnType *) lookup;
    auto expected_ret_tyty = resolve_fn_type->return_type ();
    context->push_return_type (expected_ret_tyty);

    auto result
      = TypeCheckExpr::Resolve (method.get_function_body ().get (), false);
    auto ret_resolved = expected_ret_tyty->unify (result);
    if (ret_resolved == nullptr)
      return;

    context->peek_return_type ()->append_reference (ret_resolved->get_ref ());

    context->pop_return_type ();
  }

private:
  TypeCheckImplItem (TyTy::BaseType *self) : TypeCheckBase (), self (self) {}

  TyTy::BaseType *self;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_TYPE_CHECK_IMPLITEM_H
