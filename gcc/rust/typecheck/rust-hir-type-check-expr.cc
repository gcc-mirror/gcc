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

#include "rust-hir-type-check-expr.h"

namespace Rust {
namespace Resolver {

void
TypeCheckExpr::visit (HIR::RangeFromToExpr &expr)
{
  auto lang_item_type = Analysis::RustLangItem::ItemType::RANGE;

  DefId respective_lang_item_id = UNKNOWN_DEFID;
  bool lang_item_defined
    = mappings->lookup_lang_item (lang_item_type, &respective_lang_item_id);

  // we need to have it maybe
  if (!lang_item_defined)
    {
      rust_internal_error_at (
	expr.get_locus (), "unable to find relevant lang item: %s",
	Analysis::RustLangItem::ToString (lang_item_type).c_str ());
      return;
    }

  // look it up and it _must_ be a struct definition
  HIR::Item *item = mappings->lookup_defid (respective_lang_item_id);
  rust_assert (item != nullptr);

  TyTy::BaseType *item_type = nullptr;
  bool ok
    = context->lookup_type (item->get_mappings ().get_hirid (), &item_type);
  rust_assert (ok);
  rust_assert (item_type->get_kind () == TyTy::TypeKind::ADT);
  TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (item_type);

  // this is a single generic item lets assert that
  rust_assert (adt->get_num_substitutions () == 1);

  // resolve the range expressions and these types must unify then we use that
  // type to substitute into the ADT
  TyTy::BaseType *from_ty
    = TypeCheckExpr::Resolve (expr.get_from_expr ().get (), false);
  TyTy::BaseType *to_ty
    = TypeCheckExpr::Resolve (expr.get_to_expr ().get (), false);
  TyTy::BaseType *unified = from_ty->unify (to_ty);

  // substitute it in
  std::vector<TyTy::SubstitutionArg> subst_mappings;
  const TyTy::SubstitutionParamMapping *param_ref = &adt->get_substs ().at (0);
  subst_mappings.push_back (TyTy::SubstitutionArg (param_ref, unified));

  TyTy::SubstitutionArgumentMappings subst (subst_mappings, expr.get_locus ());
  infered = SubstMapperInternal::Resolve (adt, subst);
}

void
TypeCheckExpr::visit (HIR::RangeFromExpr &expr)
{
  auto lang_item_type = Analysis::RustLangItem::ItemType::RANGE_FROM;

  DefId respective_lang_item_id = UNKNOWN_DEFID;
  bool lang_item_defined
    = mappings->lookup_lang_item (lang_item_type, &respective_lang_item_id);

  // we need to have it maybe
  if (!lang_item_defined)
    {
      rust_internal_error_at (
	expr.get_locus (), "unable to find relevant lang item: %s",
	Analysis::RustLangItem::ToString (lang_item_type).c_str ());
      return;
    }

  // look it up and it _must_ be a struct definition
  HIR::Item *item = mappings->lookup_defid (respective_lang_item_id);
  rust_assert (item != nullptr);

  TyTy::BaseType *item_type = nullptr;
  bool ok
    = context->lookup_type (item->get_mappings ().get_hirid (), &item_type);
  rust_assert (ok);
  rust_assert (item_type->get_kind () == TyTy::TypeKind::ADT);
  TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (item_type);

  // this is a single generic item lets assert that
  rust_assert (adt->get_num_substitutions () == 1);

  // resolve the range expressions and these types must unify then we use that
  // type to substitute into the ADT
  TyTy::BaseType *from_ty
    = TypeCheckExpr::Resolve (expr.get_from_expr ().get (), false);

  // substitute it in
  std::vector<TyTy::SubstitutionArg> subst_mappings;
  const TyTy::SubstitutionParamMapping *param_ref = &adt->get_substs ().at (0);
  subst_mappings.push_back (TyTy::SubstitutionArg (param_ref, from_ty));

  TyTy::SubstitutionArgumentMappings subst (subst_mappings, expr.get_locus ());
  infered = SubstMapperInternal::Resolve (adt, subst);
}

void
TypeCheckExpr::visit (HIR::RangeToExpr &expr)
{
  auto lang_item_type = Analysis::RustLangItem::ItemType::RANGE_TO;

  DefId respective_lang_item_id = UNKNOWN_DEFID;
  bool lang_item_defined
    = mappings->lookup_lang_item (lang_item_type, &respective_lang_item_id);

  // we need to have it maybe
  if (!lang_item_defined)
    {
      rust_internal_error_at (
	expr.get_locus (), "unable to find relevant lang item: %s",
	Analysis::RustLangItem::ToString (lang_item_type).c_str ());
      return;
    }

  // look it up and it _must_ be a struct definition
  HIR::Item *item = mappings->lookup_defid (respective_lang_item_id);
  rust_assert (item != nullptr);

  TyTy::BaseType *item_type = nullptr;
  bool ok
    = context->lookup_type (item->get_mappings ().get_hirid (), &item_type);
  rust_assert (ok);
  rust_assert (item_type->get_kind () == TyTy::TypeKind::ADT);
  TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (item_type);

  // this is a single generic item lets assert that
  rust_assert (adt->get_num_substitutions () == 1);

  // resolve the range expressions and these types must unify then we use that
  // type to substitute into the ADT
  TyTy::BaseType *from_ty
    = TypeCheckExpr::Resolve (expr.get_to_expr ().get (), false);

  // substitute it in
  std::vector<TyTy::SubstitutionArg> subst_mappings;
  const TyTy::SubstitutionParamMapping *param_ref = &adt->get_substs ().at (0);
  subst_mappings.push_back (TyTy::SubstitutionArg (param_ref, from_ty));

  TyTy::SubstitutionArgumentMappings subst (subst_mappings, expr.get_locus ());
  infered = SubstMapperInternal::Resolve (adt, subst);
}

void
TypeCheckExpr::visit (HIR::RangeFullExpr &expr)
{
  auto lang_item_type = Analysis::RustLangItem::ItemType::RANGE_FULL;

  DefId respective_lang_item_id = UNKNOWN_DEFID;
  bool lang_item_defined
    = mappings->lookup_lang_item (lang_item_type, &respective_lang_item_id);

  // we need to have it maybe
  if (!lang_item_defined)
    {
      rust_internal_error_at (
	expr.get_locus (), "unable to find relevant lang item: %s",
	Analysis::RustLangItem::ToString (lang_item_type).c_str ());
      return;
    }

  // look it up and it _must_ be a struct definition
  HIR::Item *item = mappings->lookup_defid (respective_lang_item_id);
  rust_assert (item != nullptr);

  TyTy::BaseType *item_type = nullptr;
  bool ok
    = context->lookup_type (item->get_mappings ().get_hirid (), &item_type);
  rust_assert (ok);
  rust_assert (item_type->is_unit ());

  infered = item_type;
}

void
TypeCheckExpr::visit (HIR::RangeFromToInclExpr &expr)
{
  auto lang_item_type = Analysis::RustLangItem::ItemType::RANGE_INCLUSIVE;

  DefId respective_lang_item_id = UNKNOWN_DEFID;
  bool lang_item_defined
    = mappings->lookup_lang_item (lang_item_type, &respective_lang_item_id);

  // we need to have it maybe
  if (!lang_item_defined)
    {
      rust_internal_error_at (
	expr.get_locus (), "unable to find relevant lang item: %s",
	Analysis::RustLangItem::ToString (lang_item_type).c_str ());
      return;
    }

  // look it up and it _must_ be a struct definition
  HIR::Item *item = mappings->lookup_defid (respective_lang_item_id);
  rust_assert (item != nullptr);

  TyTy::BaseType *item_type = nullptr;
  bool ok
    = context->lookup_type (item->get_mappings ().get_hirid (), &item_type);
  rust_assert (ok);
  rust_assert (item_type->get_kind () == TyTy::TypeKind::ADT);
  TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (item_type);

  // this is a single generic item lets assert that
  rust_assert (adt->get_num_substitutions () == 1);

  // resolve the range expressions and these types must unify then we use that
  // type to substitute into the ADT
  TyTy::BaseType *from_ty
    = TypeCheckExpr::Resolve (expr.get_from_expr ().get (), false);
  TyTy::BaseType *to_ty
    = TypeCheckExpr::Resolve (expr.get_to_expr ().get (), false);
  TyTy::BaseType *unified = from_ty->unify (to_ty);

  // substitute it in
  std::vector<TyTy::SubstitutionArg> subst_mappings;
  const TyTy::SubstitutionParamMapping *param_ref = &adt->get_substs ().at (0);
  subst_mappings.push_back (TyTy::SubstitutionArg (param_ref, unified));

  TyTy::SubstitutionArgumentMappings subst (subst_mappings, expr.get_locus ());
  infered = SubstMapperInternal::Resolve (adt, subst);
}

void
TypeCheckExpr::visit (HIR::ArrayIndexExpr &expr)
{
  TyTy::BaseType *size_ty;
  if (!context->lookup_builtin ("usize", &size_ty))
    {
      rust_error_at (
	expr.get_locus (),
	"Failure looking up size type for index in ArrayIndexExpr");
      return;
    }

  auto resolved_index_expr
    = size_ty->unify (TypeCheckExpr::Resolve (expr.get_index_expr (), false));
  if (resolved_index_expr->get_kind () != TyTy::TypeKind::ERROR)
    {
      // allow the index expr to fail lets just continue on
      context->insert_type (expr.get_index_expr ()->get_mappings (),
			    resolved_index_expr);
    }

  auto array_expr_ty
    = TypeCheckExpr::Resolve (expr.get_array_expr (), inside_loop);
  if (array_expr_ty->get_kind () == TyTy::TypeKind::ERROR)
    return;
  else if (array_expr_ty->get_kind () == TyTy::TypeKind::REF)
    {
      // lets try and deref it since rust allows this
      auto ref = static_cast<TyTy::ReferenceType *> (array_expr_ty);
      auto base = ref->get_base ();
      if (base->get_kind () == TyTy::TypeKind::ARRAY)
	array_expr_ty = base;
    }

  if (array_expr_ty->get_kind () != TyTy::TypeKind::ARRAY)
    {
      rust_error_at (expr.get_index_expr ()->get_locus (),
		     "expected an ArrayType got [%s]",
		     array_expr_ty->as_string ().c_str ());
      return;
    }

  TyTy::ArrayType *array_type = static_cast<TyTy::ArrayType *> (array_expr_ty);
  infered = array_type->get_element_type ()->clone ();
}

} // namespace Resolver
} // namespace Rust
