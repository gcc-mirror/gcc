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

#ifndef RUST_HIR_TYPE_CHECK_STMT
#define RUST_HIR_TYPE_CHECK_STMT

#include "rust-hir-type-check-base.h"
#include "rust-hir-full.h"
#include "rust-hir-type-check-type.h"
#include "rust-hir-type-check-expr.h"

namespace Rust {
namespace Resolver {

class TypeCheckStmt : public TypeCheckBase
{
  using Rust::Resolver::TypeCheckBase::visit;

public:
  static TyTy::BaseType *Resolve (HIR::Stmt *stmt, bool inside_loop)
  {
    TypeCheckStmt resolver (inside_loop);
    stmt->accept_vis (resolver);
    return resolver.infered;
  }

  void visit (HIR::ExprStmtWithBlock &stmt) override
  {
    infered = TypeCheckExpr::Resolve (stmt.get_expr (), inside_loop);
  }

  void visit (HIR::ExprStmtWithoutBlock &stmt) override
  {
    infered = TypeCheckExpr::Resolve (stmt.get_expr (), inside_loop);
  }

  void visit (HIR::EmptyStmt &stmt) override
  {
    infered
      = TyTy::TupleType::get_unit_type (stmt.get_mappings ().get_hirid ());
  }

  void visit (HIR::LetStmt &stmt) override
  {
    infered = new TyTy::TupleType (stmt.get_mappings ().get_hirid ());

    TyTy::BaseType *init_expr_ty = nullptr;
    if (stmt.has_init_expr ())
      {
	init_expr_ty
	  = TypeCheckExpr::Resolve (stmt.get_init_expr (), inside_loop);
	if (init_expr_ty->get_kind () == TyTy::TypeKind::ERROR)
	  return;

	init_expr_ty = init_expr_ty->clone ();
	auto ref = init_expr_ty->get_ref ();
	init_expr_ty->set_ref (stmt.get_mappings ().get_hirid ());
	init_expr_ty->append_reference (ref);
      }

    TyTy::BaseType *specified_ty = nullptr;
    if (stmt.has_type ())
      specified_ty = TypeCheckType::Resolve (stmt.get_type ());

    // let x:i32 = 123;
    if (specified_ty != nullptr && init_expr_ty != nullptr)
      {
	auto unified_ty = specified_ty->unify (init_expr_ty);
	if (unified_ty->get_kind () == TyTy::TypeKind::ERROR)
	  return;

	context->insert_type (stmt.get_mappings (), unified_ty);
      }
    else
      {
	// let x:i32;
	if (specified_ty != nullptr)
	  {
	    context->insert_type (stmt.get_mappings (), specified_ty);
	  }
	// let x = 123;
	else if (init_expr_ty != nullptr)
	  {
	    context->insert_type (stmt.get_mappings (), init_expr_ty);
	  }
	// let x;
	else
	  {
	    context->insert_type (
	      stmt.get_mappings (),
	      new TyTy::InferType (stmt.get_mappings ().get_hirid (),
				   TyTy::InferType::InferTypeKind::GENERAL));
	  }
      }

    TyTy::BaseType *lookup = nullptr;
    bool ok = context->lookup_type (stmt.get_mappings ().get_hirid (), &lookup);
    rust_assert (ok);
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
    infered = type;
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
    infered = type;
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

    TyTy::FnType *resolved_fn_type = fnType;
    auto expected_ret_tyty = resolved_fn_type->get_return_type ();
    context->push_return_type (expected_ret_tyty);

    auto block_expr_ty
      = TypeCheckExpr::Resolve (function.get_definition ().get (), false);

    context->pop_return_type ();

    if (block_expr_ty->get_kind () != TyTy::NEVER)
      expected_ret_tyty->unify (block_expr_ty);

    infered = fnType;
  }

private:
  TypeCheckStmt (bool inside_loop)
    : TypeCheckBase (), infered (nullptr), inside_loop (inside_loop)
  {}

  TyTy::BaseType *infered;
  bool inside_loop;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_TYPE_CHECK_STMT
