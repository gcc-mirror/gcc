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

#include "rust-hir-type-check-stmt.h"
#include "rust-hir-full.h"
#include "rust-hir-type-check-type.h"
#include "rust-hir-type-check-expr.h"
#include "rust-hir-type-check-enumitem.h"
#include "rust-hir-type-check-implitem.h"
#include "rust-hir-type-check-pattern.h"

namespace Rust {
namespace Resolver {

TyTy::BaseType *
TypeCheckStmt::Resolve (HIR::Stmt *stmt)
{
  TypeCheckStmt resolver;
  stmt->accept_vis (resolver);
  return resolver.infered;
}

void
TypeCheckStmt::visit (HIR::ExprStmtWithBlock &stmt)
{
  infered = TypeCheckExpr::Resolve (stmt.get_expr ());
}

void
TypeCheckStmt::visit (HIR::ExprStmtWithoutBlock &stmt)
{
  infered = TypeCheckExpr::Resolve (stmt.get_expr ());
}

void
TypeCheckStmt::visit (HIR::EmptyStmt &stmt)
{
  infered = TyTy::TupleType::get_unit_type (stmt.get_mappings ().get_hirid ());
}

void
TypeCheckStmt::visit (HIR::ExternBlock &extern_block)
{
  for (auto &item : extern_block.get_extern_items ())
    {
      TypeCheckTopLevelExternItem::Resolve (item.get (), extern_block);
    }
}

void
TypeCheckStmt::visit (HIR::ConstantItem &constant)
{
  TyTy::BaseType *type = TypeCheckType::Resolve (constant.get_type ());
  TyTy::BaseType *expr_type = TypeCheckExpr::Resolve (constant.get_expr ());

  infered = coercion_site (
    constant.get_mappings ().get_hirid (),
    TyTy::TyWithLocation (type, constant.get_type ()->get_locus ()),
    TyTy::TyWithLocation (expr_type, constant.get_expr ()->get_locus ()),
    constant.get_locus ());
  context->insert_type (constant.get_mappings (), infered);
}

void
TypeCheckStmt::visit (HIR::LetStmt &stmt)
{
  infered = TyTy::TupleType::get_unit_type (stmt.get_mappings ().get_hirid ());

  const HIR::Pattern &stmt_pattern = *stmt.get_pattern ();
  TyTy::BaseType *init_expr_ty = nullptr;
  Location init_expr_locus;
  if (stmt.has_init_expr ())
    {
      init_expr_locus = stmt.get_init_expr ()->get_locus ();
      init_expr_ty = TypeCheckExpr::Resolve (stmt.get_init_expr ());
      if (init_expr_ty->get_kind () == TyTy::TypeKind::ERROR)
	return;

      init_expr_ty->append_reference (
	stmt_pattern.get_pattern_mappings ().get_hirid ());
    }

  TyTy::BaseType *specified_ty = nullptr;
  Location specified_ty_locus;
  if (stmt.has_type ())
    {
      specified_ty = TypeCheckType::Resolve (stmt.get_type ());
      specified_ty_locus = stmt.get_type ()->get_locus ();
    }

  // let x:i32 = 123;
  if (specified_ty != nullptr && init_expr_ty != nullptr)
    {
      coercion_site (stmt.get_mappings ().get_hirid (),
		     TyTy::TyWithLocation (specified_ty, specified_ty_locus),
		     TyTy::TyWithLocation (init_expr_ty, init_expr_locus),
		     stmt.get_locus ());
      context->insert_type (stmt_pattern.get_pattern_mappings (), specified_ty);
    }
  else
    {
      // let x:i32;
      if (specified_ty != nullptr)
	{
	  context->insert_type (stmt_pattern.get_pattern_mappings (),
				specified_ty);
	}
      // let x = 123;
      else if (init_expr_ty != nullptr)
	{
	  context->insert_type (stmt_pattern.get_pattern_mappings (),
				init_expr_ty);
	}
      // let x;
      else
	{
	  context->insert_type (
	    stmt_pattern.get_pattern_mappings (),
	    new TyTy::InferType (
	      stmt_pattern.get_pattern_mappings ().get_hirid (),
	      TyTy::InferType::InferTypeKind::GENERAL, stmt.get_locus ()));
	}
    }
}

void
TypeCheckStmt::visit (HIR::TupleStruct &struct_decl)
{
  std::vector<TyTy::SubstitutionParamMapping> substitutions;
  if (struct_decl.has_generics ())
    {
      for (auto &generic_param : struct_decl.get_generic_params ())
	{
	  switch (generic_param.get ()->get_kind ())
	    {
	    case HIR::GenericParam::GenericKind::LIFETIME:
	    case HIR::GenericParam::GenericKind::CONST:
	      // FIXME: Skipping Lifetime and Const completely until better
	      // handling.
	      break;

	      case HIR::GenericParam::GenericKind::TYPE: {
		auto param_type
		  = TypeResolveGenericParam::Resolve (generic_param.get ());
		context->insert_type (generic_param->get_mappings (),
				      param_type);

		substitutions.push_back (TyTy::SubstitutionParamMapping (
		  static_cast<HIR::TypeParam &> (*generic_param), param_type));
	      }
	      break;
	    }
	}
    }

  std::vector<TyTy::StructFieldType *> fields;
  size_t idx = 0;
  for (auto &field : struct_decl.get_fields ())
    {
      TyTy::BaseType *field_type
	= TypeCheckType::Resolve (field.get_field_type ().get ());
      TyTy::StructFieldType *ty_field
	= new TyTy::StructFieldType (field.get_mappings ().get_hirid (),
				     std::to_string (idx), field_type,
				     field.get_locus ());
      fields.push_back (ty_field);
      context->insert_type (field.get_mappings (), ty_field->get_field_type ());
      idx++;
    }

  // get the path
  const CanonicalPath *canonical_path = nullptr;
  bool ok = mappings->lookup_canonical_path (
    struct_decl.get_mappings ().get_nodeid (), &canonical_path);
  rust_assert (ok);
  RustIdent ident{*canonical_path, struct_decl.get_locus ()};

  // there is only a single variant
  std::vector<TyTy::VariantDef *> variants;
  variants.push_back (new TyTy::VariantDef (
    struct_decl.get_mappings ().get_hirid (), struct_decl.get_identifier (),
    ident, TyTy::VariantDef::VariantType::TUPLE, nullptr, std::move (fields)));

  // Process #[repr(...)] attribute, if any
  const AST::AttrVec &attrs = struct_decl.get_outer_attrs ();
  TyTy::ADTType::ReprOptions repr
    = parse_repr_options (attrs, struct_decl.get_locus ());

  TyTy::BaseType *type
    = new TyTy::ADTType (struct_decl.get_mappings ().get_hirid (),
			 mappings->get_next_hir_id (),
			 struct_decl.get_identifier (), ident,
			 TyTy::ADTType::ADTKind::TUPLE_STRUCT,
			 std::move (variants), std::move (substitutions), repr);

  context->insert_type (struct_decl.get_mappings (), type);
  infered = type;
}

void
TypeCheckStmt::visit (HIR::Enum &enum_decl)
{
  std::vector<TyTy::SubstitutionParamMapping> substitutions;
  if (enum_decl.has_generics ())
    {
      for (auto &generic_param : enum_decl.get_generic_params ())
	{
	  switch (generic_param.get ()->get_kind ())
	    {
	    case HIR::GenericParam::GenericKind::LIFETIME:
	    case HIR::GenericParam::GenericKind::CONST:
	      // FIXME: Skipping Lifetime and Const completely until better
	      // handling.
	      break;

	      case HIR::GenericParam::GenericKind::TYPE: {
		auto param_type
		  = TypeResolveGenericParam::Resolve (generic_param.get ());
		context->insert_type (generic_param->get_mappings (),
				      param_type);

		substitutions.push_back (TyTy::SubstitutionParamMapping (
		  static_cast<HIR::TypeParam &> (*generic_param), param_type));
	      }
	      break;
	    }
	}
    }

  std::vector<TyTy::VariantDef *> variants;
  int64_t discriminant_value = 0;
  for (auto &variant : enum_decl.get_variants ())
    {
      TyTy::VariantDef *field_type
	= TypeCheckEnumItem::Resolve (variant.get (), discriminant_value);

      discriminant_value++;
      variants.push_back (field_type);
    }

  // get the path
  const CanonicalPath *canonical_path = nullptr;
  bool ok
    = mappings->lookup_canonical_path (enum_decl.get_mappings ().get_nodeid (),
				       &canonical_path);
  rust_assert (ok);
  RustIdent ident{*canonical_path, enum_decl.get_locus ()};

  TyTy::BaseType *type
    = new TyTy::ADTType (enum_decl.get_mappings ().get_hirid (),
			 mappings->get_next_hir_id (),
			 enum_decl.get_identifier (), ident,
			 TyTy::ADTType::ADTKind::ENUM, std::move (variants),
			 std::move (substitutions));

  context->insert_type (enum_decl.get_mappings (), type);
  infered = type;
}

void
TypeCheckStmt::visit (HIR::StructStruct &struct_decl)
{
  std::vector<TyTy::SubstitutionParamMapping> substitutions;
  if (struct_decl.has_generics ())
    {
      for (auto &generic_param : struct_decl.get_generic_params ())
	{
	  switch (generic_param.get ()->get_kind ())
	    {
	    case HIR::GenericParam::GenericKind::LIFETIME:
	    case HIR::GenericParam::GenericKind::CONST:
	      // FIXME: Skipping Lifetime and Const completely until better
	      // handling.
	      break;

	      case HIR::GenericParam::GenericKind::TYPE: {
		auto param_type
		  = TypeResolveGenericParam::Resolve (generic_param.get ());
		context->insert_type (generic_param->get_mappings (),
				      param_type);

		substitutions.push_back (TyTy::SubstitutionParamMapping (
		  static_cast<HIR::TypeParam &> (*generic_param), param_type));
	      }
	      break;
	    }
	}
    }

  std::vector<TyTy::StructFieldType *> fields;
  for (auto &field : struct_decl.get_fields ())
    {
      TyTy::BaseType *field_type
	= TypeCheckType::Resolve (field.get_field_type ().get ());
      TyTy::StructFieldType *ty_field
	= new TyTy::StructFieldType (field.get_mappings ().get_hirid (),
				     field.get_field_name (), field_type,
				     field.get_locus ());
      fields.push_back (ty_field);
      context->insert_type (field.get_mappings (), ty_field->get_field_type ());
    }

  // get the path
  const CanonicalPath *canonical_path = nullptr;
  bool ok = mappings->lookup_canonical_path (
    struct_decl.get_mappings ().get_nodeid (), &canonical_path);
  rust_assert (ok);
  RustIdent ident{*canonical_path, struct_decl.get_locus ()};

  // there is only a single variant
  std::vector<TyTy::VariantDef *> variants;
  variants.push_back (new TyTy::VariantDef (
    struct_decl.get_mappings ().get_hirid (), struct_decl.get_identifier (),
    ident, TyTy::VariantDef::VariantType::STRUCT, nullptr, std::move (fields)));

  // Process #[repr(...)] attribute, if any
  const AST::AttrVec &attrs = struct_decl.get_outer_attrs ();
  TyTy::ADTType::ReprOptions repr
    = parse_repr_options (attrs, struct_decl.get_locus ());

  TyTy::BaseType *type
    = new TyTy::ADTType (struct_decl.get_mappings ().get_hirid (),
			 mappings->get_next_hir_id (),
			 struct_decl.get_identifier (), ident,
			 TyTy::ADTType::ADTKind::STRUCT_STRUCT,
			 std::move (variants), std::move (substitutions), repr);

  context->insert_type (struct_decl.get_mappings (), type);
  infered = type;
}

void
TypeCheckStmt::visit (HIR::Union &union_decl)
{
  std::vector<TyTy::SubstitutionParamMapping> substitutions;
  if (union_decl.has_generics ())
    {
      for (auto &generic_param : union_decl.get_generic_params ())
	{
	  switch (generic_param.get ()->get_kind ())
	    {
	    case HIR::GenericParam::GenericKind::LIFETIME:
	    case HIR::GenericParam::GenericKind::CONST:
	      // FIXME: Skipping Lifetime and Const completely until better
	      // handling.
	      break;

	      case HIR::GenericParam::GenericKind::TYPE: {
		auto param_type
		  = TypeResolveGenericParam::Resolve (generic_param.get ());
		context->insert_type (generic_param->get_mappings (),
				      param_type);

		substitutions.push_back (TyTy::SubstitutionParamMapping (
		  static_cast<HIR::TypeParam &> (*generic_param), param_type));
	      }
	      break;
	    }
	}
    }

  std::vector<TyTy::StructFieldType *> fields;
  for (auto &variant : union_decl.get_variants ())
    {
      TyTy::BaseType *variant_type
	= TypeCheckType::Resolve (variant.get_field_type ().get ());
      TyTy::StructFieldType *ty_variant
	= new TyTy::StructFieldType (variant.get_mappings ().get_hirid (),
				     variant.get_field_name (), variant_type,
				     variant.get_locus ());
      fields.push_back (ty_variant);
      context->insert_type (variant.get_mappings (),
			    ty_variant->get_field_type ());
    }

  // get the path
  const CanonicalPath *canonical_path = nullptr;
  bool ok
    = mappings->lookup_canonical_path (union_decl.get_mappings ().get_nodeid (),
				       &canonical_path);
  rust_assert (ok);
  RustIdent ident{*canonical_path, union_decl.get_locus ()};

  // there is only a single variant
  std::vector<TyTy::VariantDef *> variants;
  variants.push_back (new TyTy::VariantDef (
    union_decl.get_mappings ().get_hirid (), union_decl.get_identifier (),
    ident, TyTy::VariantDef::VariantType::STRUCT, nullptr, std::move (fields)));

  TyTy::BaseType *type
    = new TyTy::ADTType (union_decl.get_mappings ().get_hirid (),
			 mappings->get_next_hir_id (),
			 union_decl.get_identifier (), ident,
			 TyTy::ADTType::ADTKind::UNION, std::move (variants),
			 std::move (substitutions));

  context->insert_type (union_decl.get_mappings (), type);
  infered = type;
}

void
TypeCheckStmt::visit (HIR::Function &function)
{
  std::vector<TyTy::SubstitutionParamMapping> substitutions;
  if (function.has_generics ())
    {
      for (auto &generic_param : function.get_generic_params ())
	{
	  switch (generic_param.get ()->get_kind ())
	    {
	    case HIR::GenericParam::GenericKind::LIFETIME:
	    case HIR::GenericParam::GenericKind::CONST:
	      // FIXME: Skipping Lifetime and Const completely until better
	      // handling.
	      break;

	      case HIR::GenericParam::GenericKind::TYPE: {
		auto param_type
		  = TypeResolveGenericParam::Resolve (generic_param.get ());
		context->insert_type (generic_param->get_mappings (),
				      param_type);

		substitutions.push_back (TyTy::SubstitutionParamMapping (
		  static_cast<HIR::TypeParam &> (*generic_param), param_type));
	      }
	      break;
	    }
	}
    }

  TyTy::BaseType *ret_type = nullptr;
  if (!function.has_function_return_type ())
    ret_type
      = TyTy::TupleType::get_unit_type (function.get_mappings ().get_hirid ());
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
      TypeCheckPattern::Resolve (param.get_param_name (), param_tyty);
    }

  // get the path
  const CanonicalPath *canonical_path = nullptr;
  bool ok
    = mappings->lookup_canonical_path (function.get_mappings ().get_nodeid (),
				       &canonical_path);
  rust_assert (ok);

  RustIdent ident{*canonical_path, function.get_locus ()};
  auto fnType = new TyTy::FnType (function.get_mappings ().get_hirid (),
				  function.get_mappings ().get_defid (),
				  function.get_function_name (), ident,
				  TyTy::FnType::FNTYPE_DEFAULT_FLAGS, ABI::RUST,
				  std::move (params), ret_type,
				  std::move (substitutions));
  context->insert_type (function.get_mappings (), fnType);

  TyTy::FnType *resolved_fn_type = fnType;
  auto expected_ret_tyty = resolved_fn_type->get_return_type ();
  context->push_return_type (TypeCheckContextItem (&function),
			     expected_ret_tyty);

  auto block_expr_ty
    = TypeCheckExpr::Resolve (function.get_definition ().get ());

  context->pop_return_type ();

  Location fn_return_locus = function.has_function_return_type ()
			       ? function.get_return_type ()->get_locus ()
			       : function.get_locus ();
  coercion_site (function.get_definition ()->get_mappings ().get_hirid (),
		 TyTy::TyWithLocation (expected_ret_tyty, fn_return_locus),
		 TyTy::TyWithLocation (block_expr_ty),
		 function.get_definition ()->get_locus ());

  infered = fnType;
}

} // namespace Resolver
} // namespace Rust
