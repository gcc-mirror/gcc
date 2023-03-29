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
#include "rust-hir-type-check-item.h"
#include "rust-hir-type-check-enumitem.h"
#include "rust-hir-type-check-implitem.h"
#include "rust-hir-type-check-type.h"
#include "rust-hir-type-check-stmt.h"
#include "rust-hir-type-check-expr.h"
#include "rust-hir-type-check-pattern.h"
#include "rust-hir-trait-resolve.h"

namespace Rust {
namespace Resolver {

TypeCheckItem::TypeCheckItem () : TypeCheckBase (), infered (nullptr) {}

TyTy::BaseType *
TypeCheckItem::Resolve (HIR::Item &item)
{
  // is it already resolved?
  auto context = TypeCheckContext::get ();
  TyTy::BaseType *resolved = nullptr;
  bool already_resolved
    = context->lookup_type (item.get_mappings ().get_hirid (), &resolved);
  if (already_resolved)
    return resolved;

  rust_assert (item.get_hir_kind () == HIR::Node::BaseKind::VIS_ITEM);
  HIR::VisItem &vis_item = static_cast<HIR::VisItem &> (item);

  TypeCheckItem resolver;
  vis_item.accept_vis (resolver);
  return resolver.infered;
}

TyTy::BaseType *
TypeCheckItem::ResolveImplItem (HIR::ImplBlock &impl_block, HIR::ImplItem &item)
{
  TypeCheckItem resolver;
  return resolver.resolve_impl_item (impl_block, item);
}

TyTy::BaseType *
TypeCheckItem::ResolveImplBlockSelf (HIR::ImplBlock &impl_block)
{
  TypeCheckItem resolver;

  bool failed_flag = false;
  std::vector<TyTy::SubstitutionParamMapping> substitutions
    = resolver.resolve_impl_block_substitutions (impl_block, failed_flag);
  if (failed_flag)
    {
      return new TyTy::ErrorType (impl_block.get_mappings ().get_hirid ());
    }

  return resolver.resolve_impl_block_self (impl_block);
}

void
TypeCheckItem::visit (HIR::TypeAlias &alias)
{
  TyTy::BaseType *actual_type
    = TypeCheckType::Resolve (alias.get_type_aliased ().get ());

  context->insert_type (alias.get_mappings (), actual_type);

  for (auto &where_clause_item : alias.get_where_clause ().get_items ())
    {
      ResolveWhereClauseItem::Resolve (*where_clause_item.get ());
    }
  infered = actual_type;
}

void
TypeCheckItem::visit (HIR::TupleStruct &struct_decl)
{
  std::vector<TyTy::SubstitutionParamMapping> substitutions;
  if (struct_decl.has_generics ())
    resolve_generic_params (struct_decl.get_generic_params (), substitutions);

  for (auto &where_clause_item : struct_decl.get_where_clause ().get_items ())
    {
      ResolveWhereClauseItem::Resolve (*where_clause_item.get ());
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

  // its a single variant ADT
  std::vector<TyTy::VariantDef *> variants;
  variants.push_back (new TyTy::VariantDef (
    struct_decl.get_mappings ().get_hirid (),
    struct_decl.get_mappings ().get_defid (), struct_decl.get_identifier (),
    ident, TyTy::VariantDef::VariantType::TUPLE, nullptr, std::move (fields)));

  // Process #[repr(X)] attribute, if any
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
TypeCheckItem::visit (HIR::StructStruct &struct_decl)
{
  std::vector<TyTy::SubstitutionParamMapping> substitutions;
  if (struct_decl.has_generics ())
    resolve_generic_params (struct_decl.get_generic_params (), substitutions);

  for (auto &where_clause_item : struct_decl.get_where_clause ().get_items ())
    {
      ResolveWhereClauseItem::Resolve (*where_clause_item.get ());
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

  // its a single variant ADT
  std::vector<TyTy::VariantDef *> variants;
  variants.push_back (new TyTy::VariantDef (
    struct_decl.get_mappings ().get_hirid (),
    struct_decl.get_mappings ().get_defid (), struct_decl.get_identifier (),
    ident, TyTy::VariantDef::VariantType::STRUCT, nullptr, std::move (fields)));

  // Process #[repr(X)] attribute, if any
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
TypeCheckItem::visit (HIR::Enum &enum_decl)
{
  std::vector<TyTy::SubstitutionParamMapping> substitutions;
  if (enum_decl.has_generics ())
    resolve_generic_params (enum_decl.get_generic_params (), substitutions);

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

  // multi variant ADT
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
TypeCheckItem::visit (HIR::Union &union_decl)
{
  std::vector<TyTy::SubstitutionParamMapping> substitutions;
  if (union_decl.has_generics ())
    resolve_generic_params (union_decl.get_generic_params (), substitutions);

  for (auto &where_clause_item : union_decl.get_where_clause ().get_items ())
    {
      ResolveWhereClauseItem::Resolve (*where_clause_item.get ());
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
    union_decl.get_mappings ().get_hirid (),
    union_decl.get_mappings ().get_defid (), union_decl.get_identifier (),
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
TypeCheckItem::visit (HIR::StaticItem &var)
{
  TyTy::BaseType *type = TypeCheckType::Resolve (var.get_type ());
  TyTy::BaseType *expr_type = TypeCheckExpr::Resolve (var.get_expr ());

  TyTy::BaseType *unified
    = coercion_site (var.get_mappings ().get_hirid (),
		     TyTy::TyWithLocation (type, var.get_type ()->get_locus ()),
		     TyTy::TyWithLocation (expr_type,
					   var.get_expr ()->get_locus ()),
		     var.get_locus ());
  context->insert_type (var.get_mappings (), unified);
  infered = unified;
}

void
TypeCheckItem::visit (HIR::ConstantItem &constant)
{
  TyTy::BaseType *type = TypeCheckType::Resolve (constant.get_type ());
  TyTy::BaseType *expr_type = TypeCheckExpr::Resolve (constant.get_expr ());

  TyTy::BaseType *unified = unify_site (
    constant.get_mappings ().get_hirid (),
    TyTy::TyWithLocation (type, constant.get_type ()->get_locus ()),
    TyTy::TyWithLocation (expr_type, constant.get_expr ()->get_locus ()),
    constant.get_locus ());
  context->insert_type (constant.get_mappings (), unified);
  infered = unified;
}

void
TypeCheckItem::visit (HIR::ImplBlock &impl_block)
{
  bool failed_flag = false;
  std::vector<TyTy::SubstitutionParamMapping> substitutions
    = resolve_impl_block_substitutions (impl_block, failed_flag);
  if (failed_flag)
    {
      infered = new TyTy::ErrorType (impl_block.get_mappings ().get_hirid ());
      return;
    }

  TyTy::BaseType *self = resolve_impl_block_self (impl_block);

  // resolve each impl_item
  for (auto &impl_item : impl_block.get_impl_items ())
    {
      TypeCheckImplItem::Resolve (&impl_block, impl_item.get (), self,
				  substitutions);
    }

  // validate the impl items
  validate_trait_impl_block (impl_block, self, substitutions);
}

TyTy::BaseType *
TypeCheckItem::resolve_impl_item (HIR::ImplBlock &impl_block,
				  HIR::ImplItem &item)
{
  bool failed_flag = false;
  std::vector<TyTy::SubstitutionParamMapping> substitutions
    = resolve_impl_block_substitutions (impl_block, failed_flag);
  if (failed_flag)
    {
      return new TyTy::ErrorType (impl_block.get_mappings ().get_hirid ());
    }

  TyTy::BaseType *self = resolve_impl_block_self (impl_block);

  return TypeCheckImplItem::Resolve (&impl_block, &item, self, substitutions);
}

void
TypeCheckItem::visit (HIR::Function &function)
{
  std::vector<TyTy::SubstitutionParamMapping> substitutions;
  if (function.has_generics ())
    resolve_generic_params (function.get_generic_params (), substitutions);

  for (auto &where_clause_item : function.get_where_clause ().get_items ())
    {
      ResolveWhereClauseItem::Resolve (*where_clause_item.get ());
    }

  TyTy::BaseType *ret_type = nullptr;
  if (!function.has_function_return_type ())
    ret_type
      = TyTy::TupleType::get_unit_type (function.get_mappings ().get_hirid ());
  else
    {
      auto resolved
	= TypeCheckType::Resolve (function.get_return_type ().get ());
      if (resolved->get_kind () == TyTy::TypeKind::ERROR)
	{
	  rust_error_at (function.get_locus (),
			 "failed to resolve return type");
	  return;
	}

      ret_type = resolved->clone ();
      ret_type->set_ref (
	function.get_return_type ()->get_mappings ().get_hirid ());
    }

  std::vector<std::pair<HIR::Pattern *, TyTy::BaseType *>> params;
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

  // need to get the return type from this
  TyTy::FnType *resolved_fn_type = fnType;
  auto expected_ret_tyty = resolved_fn_type->get_return_type ();
  context->push_return_type (TypeCheckContextItem (&function),
			     expected_ret_tyty);

  auto block_expr_ty
    = TypeCheckExpr::Resolve (function.get_definition ().get ());

  Location fn_return_locus = function.has_function_return_type ()
			       ? function.get_return_type ()->get_locus ()
			       : function.get_locus ();
  coercion_site (function.get_definition ()->get_mappings ().get_hirid (),
		 TyTy::TyWithLocation (expected_ret_tyty, fn_return_locus),
		 TyTy::TyWithLocation (block_expr_ty),
		 function.get_definition ()->get_locus ());

  context->pop_return_type ();

  infered = fnType;
}

void
TypeCheckItem::visit (HIR::Module &module)
{
  for (auto &item : module.get_items ())
    TypeCheckItem::Resolve (*item.get ());
}

void
TypeCheckItem::visit (HIR::Trait &trait)
{
  TraitResolver::Resolve (trait);
}

void
TypeCheckItem::visit (HIR::ExternBlock &extern_block)
{
  for (auto &item : extern_block.get_extern_items ())
    {
      TypeCheckTopLevelExternItem::Resolve (item.get (), extern_block);
    }
}

std::vector<TyTy::SubstitutionParamMapping>
TypeCheckItem::resolve_impl_block_substitutions (HIR::ImplBlock &impl_block,
						 bool &failure_flag)
{
  std::vector<TyTy::SubstitutionParamMapping> substitutions;
  if (impl_block.has_generics ())
    resolve_generic_params (impl_block.get_generic_params (), substitutions);

  for (auto &where_clause_item : impl_block.get_where_clause ().get_items ())
    {
      ResolveWhereClauseItem::Resolve (*where_clause_item.get ());
    }

  auto specified_bound = TyTy::TypeBoundPredicate::error ();
  TraitReference *trait_reference = &TraitReference::error_node ();
  if (impl_block.has_trait_ref ())
    {
      std::unique_ptr<HIR::TypePath> &ref = impl_block.get_trait_ref ();
      trait_reference = TraitResolver::Resolve (*ref.get ());
      rust_assert (!trait_reference->is_error ());

      // we don't error out here see: gcc/testsuite/rust/compile/traits2.rs
      // for example
      specified_bound = get_predicate_from_bound (*ref.get ());
    }

  TyTy::BaseType *self = TypeCheckType::Resolve (impl_block.get_type ().get ());

  // inherit the bounds
  if (!specified_bound.is_error ())
    self->inherit_bounds ({specified_bound});

  // check for any unconstrained type-params
  const TyTy::SubstitutionArgumentMappings trait_constraints
    = specified_bound.get_substitution_arguments ();
  const TyTy::SubstitutionArgumentMappings impl_constraints
    = GetUsedSubstArgs::From (self);

  failure_flag = check_for_unconstrained (substitutions, trait_constraints,
					  impl_constraints, self);

  return substitutions;
}

void
TypeCheckItem::validate_trait_impl_block (
  HIR::ImplBlock &impl_block, TyTy::BaseType *self,
  std::vector<TyTy::SubstitutionParamMapping> &substitutions)
{
  auto specified_bound = TyTy::TypeBoundPredicate::error ();
  TraitReference *trait_reference = &TraitReference::error_node ();
  if (impl_block.has_trait_ref ())
    {
      std::unique_ptr<HIR::TypePath> &ref = impl_block.get_trait_ref ();
      trait_reference = TraitResolver::Resolve (*ref.get ());
      rust_assert (!trait_reference->is_error ());

      // we don't error out here see: gcc/testsuite/rust/compile/traits2.rs
      // for example
      specified_bound = get_predicate_from_bound (*ref.get ());
    }

  bool is_trait_impl_block = !trait_reference->is_error ();
  std::vector<const TraitItemReference *> trait_item_refs;
  for (auto &impl_item : impl_block.get_impl_items ())
    {
      if (!specified_bound.is_error ())
	{
	  auto trait_item_ref
	    = TypeCheckImplItemWithTrait::Resolve (&impl_block,
						   impl_item.get (), self,
						   specified_bound,
						   substitutions);
	  trait_item_refs.push_back (trait_item_ref.get_raw_item ());
	}
    }

  bool impl_block_missing_trait_items
    = !specified_bound.is_error ()
      && trait_reference->size () != trait_item_refs.size ();
  if (impl_block_missing_trait_items)
    {
      // filter the missing impl_items
      std::vector<std::reference_wrapper<const TraitItemReference>>
	missing_trait_items;
      for (const auto &trait_item_ref : trait_reference->get_trait_items ())
	{
	  bool found = false;
	  for (auto implemented_trait_item : trait_item_refs)
	    {
	      std::string trait_item_name = trait_item_ref.get_identifier ();
	      std::string impl_item_name
		= implemented_trait_item->get_identifier ();
	      found = trait_item_name.compare (impl_item_name) == 0;
	      if (found)
		break;
	    }

	  bool is_required_trait_item = !trait_item_ref.is_optional ();
	  if (!found && is_required_trait_item)
	    missing_trait_items.push_back (trait_item_ref);
	}

      if (missing_trait_items.size () > 0)
	{
	  std::string missing_items_buf;
	  RichLocation r (impl_block.get_locus ());
	  for (size_t i = 0; i < missing_trait_items.size (); i++)
	    {
	      bool has_more = (i + 1) < missing_trait_items.size ();
	      const TraitItemReference &missing_trait_item
		= missing_trait_items.at (i);
	      missing_items_buf += missing_trait_item.get_identifier ()
				   + (has_more ? ", " : "");
	      r.add_range (missing_trait_item.get_locus ());
	    }

	  rust_error_at (r, "missing %s in implementation of trait %<%s%>",
			 missing_items_buf.c_str (),
			 trait_reference->get_name ().c_str ());
	}
    }

  if (is_trait_impl_block)
    {
      trait_reference->clear_associated_types ();

      AssociatedImplTrait associated (trait_reference, &impl_block, self,
				      context);
      context->insert_associated_trait_impl (
	impl_block.get_mappings ().get_hirid (), std::move (associated));
      context->insert_associated_impl_mapping (
	trait_reference->get_mappings ().get_hirid (), self,
	impl_block.get_mappings ().get_hirid ());
    }
}

TyTy::BaseType *
TypeCheckItem::resolve_impl_block_self (HIR::ImplBlock &impl_block)
{
  return TypeCheckType::Resolve (impl_block.get_type ().get ());
}

} // namespace Resolver
} // namespace Rust
