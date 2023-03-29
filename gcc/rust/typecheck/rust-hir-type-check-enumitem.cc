// Copyright (C) 2020-2023 Free Software Foundation, Inc.

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

#include "rust-hir-full.h"
#include "rust-hir-type-check-type.h"
#include "rust-hir-type-check-expr.h"
#include "rust-hir-type-check-enumitem.h"

namespace Rust {
namespace Resolver {

TyTy::VariantDef *
TypeCheckEnumItem::Resolve (HIR::EnumItem *item, int64_t last_discriminant)
{
  TypeCheckEnumItem resolver (last_discriminant);
  switch (item->get_enum_item_kind ())
    {
    case HIR::EnumItem::EnumItemKind::Named:
      resolver.visit (static_cast<HIR::EnumItem &> (*item));
      break;

    case HIR::EnumItem::EnumItemKind::Tuple:
      resolver.visit (static_cast<HIR::EnumItemTuple &> (*item));
      break;

    case HIR::EnumItem::EnumItemKind::Struct:
      resolver.visit (static_cast<HIR::EnumItemStruct &> (*item));
      break;

    case HIR::EnumItem::EnumItemKind::Discriminant:
      resolver.visit (static_cast<HIR::EnumItemDiscriminant &> (*item));
      break;
    }
  return resolver.variant;
}

TypeCheckEnumItem::TypeCheckEnumItem (int64_t last_discriminant)
  : TypeCheckBase (), variant (nullptr), last_discriminant (last_discriminant)
{}

void
TypeCheckEnumItem::visit (HIR::EnumItem &item)
{
  if (last_discriminant == INT64_MAX)
    rust_error_at (item.get_locus (), "discriminant too big");

  Analysis::NodeMapping mapping (item.get_mappings ().get_crate_num (),
				 item.get_mappings ().get_nodeid (),
				 mappings->get_next_hir_id (
				   item.get_mappings ().get_crate_num ()),
				 item.get_mappings ().get_local_defid ());
  HIR::LiteralExpr *discim_expr
    = new HIR::LiteralExpr (mapping, std::to_string (last_discriminant),
			    HIR::Literal::LitType::INT,
			    PrimitiveCoreType::CORETYPE_I64, item.get_locus (),
			    {});

  TyTy::BaseType *isize = nullptr;
  bool ok = context->lookup_builtin ("isize", &isize);
  rust_assert (ok);
  context->insert_type (mapping, isize);

  const CanonicalPath *canonical_path = nullptr;
  ok = mappings->lookup_canonical_path (item.get_mappings ().get_nodeid (),
					&canonical_path);
  rust_assert (ok);

  RustIdent ident{*canonical_path, item.get_locus ()};
  variant = new TyTy::VariantDef (item.get_mappings ().get_hirid (),
				  item.get_mappings ().get_defid (),
				  item.get_identifier (), ident, discim_expr);
}

void
TypeCheckEnumItem::visit (HIR::EnumItemDiscriminant &item)
{
  if (last_discriminant == INT64_MAX)
    rust_error_at (item.get_locus (), "discriminant too big");

  auto &discriminant = item.get_discriminant_expression ();
  auto capacity_type = TypeCheckExpr::Resolve (discriminant.get ());
  if (capacity_type->get_kind () == TyTy::TypeKind::ERROR)
    return;

  TyTy::ISizeType *expected_ty
    = new TyTy::ISizeType (discriminant->get_mappings ().get_hirid ());
  context->insert_type (discriminant->get_mappings (), expected_ty);

  unify_site (item.get_mappings ().get_hirid (),
	      TyTy::TyWithLocation (expected_ty),
	      TyTy::TyWithLocation (capacity_type), item.get_locus ());

  const CanonicalPath *canonical_path = nullptr;
  bool ok = mappings->lookup_canonical_path (item.get_mappings ().get_nodeid (),
					     &canonical_path);
  rust_assert (ok);

  RustIdent ident{*canonical_path, item.get_locus ()};
  variant = new TyTy::VariantDef (item.get_mappings ().get_hirid (),
				  item.get_mappings ().get_defid (),
				  item.get_identifier (), ident,
				  item.get_discriminant_expression ().get ());
}

void
TypeCheckEnumItem::visit (HIR::EnumItemTuple &item)
{
  if (last_discriminant == INT64_MAX)
    rust_error_at (item.get_locus (), "discriminant too big");

  std::vector<TyTy::StructFieldType *> fields;
  size_t idx = 0;
  for (auto &field : item.get_tuple_fields ())
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

  Analysis::NodeMapping mapping (item.get_mappings ().get_crate_num (),
				 item.get_mappings ().get_nodeid (),
				 mappings->get_next_hir_id (
				   item.get_mappings ().get_crate_num ()),
				 item.get_mappings ().get_local_defid ());
  HIR::LiteralExpr *discim_expr
    = new HIR::LiteralExpr (mapping, std::to_string (last_discriminant),
			    HIR::Literal::LitType::INT,
			    PrimitiveCoreType::CORETYPE_I64, item.get_locus (),
			    {});

  TyTy::BaseType *isize = nullptr;
  bool ok = context->lookup_builtin ("isize", &isize);
  rust_assert (ok);
  context->insert_type (mapping, isize);

  const CanonicalPath *canonical_path = nullptr;
  ok = mappings->lookup_canonical_path (item.get_mappings ().get_nodeid (),
					&canonical_path);
  rust_assert (ok);

  RustIdent ident{*canonical_path, item.get_locus ()};
  variant = new TyTy::VariantDef (item.get_mappings ().get_hirid (),
				  item.get_mappings ().get_defid (),
				  item.get_identifier (), ident,
				  TyTy::VariantDef::VariantType::TUPLE,
				  discim_expr, fields);
}

void
TypeCheckEnumItem::visit (HIR::EnumItemStruct &item)
{
  if (last_discriminant == INT64_MAX)
    rust_error_at (item.get_locus (), "discriminant too big");

  std::vector<TyTy::StructFieldType *> fields;
  for (auto &field : item.get_struct_fields ())
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

  Analysis::NodeMapping mapping (item.get_mappings ().get_crate_num (),
				 item.get_mappings ().get_nodeid (),
				 mappings->get_next_hir_id (
				   item.get_mappings ().get_crate_num ()),
				 item.get_mappings ().get_local_defid ());
  HIR::LiteralExpr *discrim_expr
    = new HIR::LiteralExpr (mapping, std::to_string (last_discriminant),
			    HIR::Literal::LitType::INT,
			    PrimitiveCoreType::CORETYPE_I64, item.get_locus (),
			    {});

  TyTy::BaseType *isize = nullptr;
  bool ok = context->lookup_builtin ("isize", &isize);
  rust_assert (ok);
  context->insert_type (mapping, isize);

  const CanonicalPath *canonical_path = nullptr;
  ok = mappings->lookup_canonical_path (item.get_mappings ().get_nodeid (),
					&canonical_path);
  rust_assert (ok);

  RustIdent ident{*canonical_path, item.get_locus ()};
  variant = new TyTy::VariantDef (item.get_mappings ().get_hirid (),
				  item.get_mappings ().get_defid (),
				  item.get_identifier (), ident,
				  TyTy::VariantDef::VariantType::STRUCT,
				  discrim_expr, fields);
}

} // namespace Resolver
} // namespace Rust
