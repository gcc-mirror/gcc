// Copyright (C) 2020-2024 Free Software Foundation, Inc.

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

#include "rust-hir-type-check-implitem.h"
#include "rust-diagnostics.h"
#include "rust-hir-type-check-base.h"
#include "rust-hir-type-check-type.h"
#include "rust-hir-type-check-expr.h"
#include "rust-hir-type-check-pattern.h"
#include "rust-type-util.h"
#include "rust-tyty.h"

namespace Rust {
namespace Resolver {

TypeCheckTopLevelExternItem::TypeCheckTopLevelExternItem (
  const HIR::ExternBlock &parent)
  : TypeCheckBase (), parent (parent)
{}

TyTy::BaseType *
TypeCheckTopLevelExternItem::Resolve (HIR::ExternalItem *item,
				      const HIR::ExternBlock &parent)
{
  // is it already resolved?
  auto context = TypeCheckContext::get ();
  TyTy::BaseType *resolved = nullptr;
  bool already_resolved
    = context->lookup_type (item->get_mappings ().get_hirid (), &resolved);
  if (already_resolved)
    return resolved;

  TypeCheckTopLevelExternItem resolver (parent);
  item->accept_vis (resolver);
  return resolver.resolved;
}

void
TypeCheckTopLevelExternItem::visit (HIR::ExternalStaticItem &item)
{
  TyTy::BaseType *actual_type
    = TypeCheckType::Resolve (item.get_item_type ().get ());

  context->insert_type (item.get_mappings (), actual_type);
  resolved = actual_type;
}

void
TypeCheckTopLevelExternItem::visit (HIR::ExternalFunctionItem &function)
{
  auto binder_pin = context->push_clean_lifetime_resolver ();

  std::vector<TyTy::SubstitutionParamMapping> substitutions;
  if (function.has_generics ())
    {
      for (auto &generic_param : function.get_generic_params ())
	{
	  switch (generic_param.get ()->get_kind ())
	    {
	    case HIR::GenericParam::GenericKind::LIFETIME:
	      context->intern_and_insert_lifetime (
		static_cast<HIR::LifetimeParam &> (*generic_param)
		  .get_lifetime ());
	      // TODO: handle bounds
	      break;
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

  TyTy::RegionConstraints region_constraints;
  if (function.has_where_clause ())
    {
      for (auto &where_clause_item : function.get_where_clause ().get_items ())
	{
	  ResolveWhereClauseItem::Resolve (*where_clause_item.get (),
					   region_constraints);
	}
    }

  TyTy::BaseType *ret_type = nullptr;
  if (!function.has_return_type ())
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
      auto param_tyty = TypeCheckType::Resolve (param.get_type ().get ());

      // these are implicit mappings and not used
      auto crate_num = mappings->get_current_crate ();
      Analysis::NodeMapping mapping (crate_num, mappings->get_next_node_id (),
				     mappings->get_next_hir_id (crate_num),
				     UNKNOWN_LOCAL_DEFID);

      HIR::IdentifierPattern *param_pattern
	= new HIR::IdentifierPattern (mapping, param.get_param_name (),
				      UNDEF_LOCATION, false, Mutability::Imm,
				      std::unique_ptr<HIR::Pattern> (nullptr));

      params.push_back (
	std::pair<HIR::Pattern *, TyTy::BaseType *> (param_pattern,
						     param_tyty));

      context->insert_type (param.get_mappings (), param_tyty);

      // FIXME do we need error checking for patterns here?
      // see https://github.com/Rust-GCC/gccrs/issues/995
    }

  uint8_t flags = TyTy::FnType::FNTYPE_IS_EXTERN_FLAG;
  if (function.is_variadic ())
    {
      flags |= TyTy::FnType::FNTYPE_IS_VARADIC_FLAG;
      if (parent.get_abi () != Rust::ABI::C)
	{
	  rust_error_at (
	    function.get_locus (), ErrorCode::E0045,
	    "C-variadic function must have C or cdecl calling convention");
	}
    }

  RustIdent ident{
    CanonicalPath::new_seg (function.get_mappings ().get_nodeid (),
			    function.get_item_name ().as_string ()),
    function.get_locus ()};

  auto fnType = new TyTy::FnType (
    function.get_mappings ().get_hirid (),
    function.get_mappings ().get_defid (),
    function.get_item_name ().as_string (), ident, flags, parent.get_abi (),
    std::move (params), ret_type, std::move (substitutions),
    TyTy::SubstitutionArgumentMappings::empty (
      context->get_lifetime_resolver ().get_num_bound_regions ()),
    region_constraints);

  context->insert_type (function.get_mappings (), fnType);
  resolved = fnType;
}

void
TypeCheckTopLevelExternItem::visit (HIR::ExternalTypeItem &type)
{
  rust_sorry_at (type.get_locus (), "extern types are not supported yet");
  //  auto binder_pin = context->push_clean_lifetime_resolver ();

  //  std::vector<TyTy::SubstitutionParamMapping> substitutions;
  //  if (function.has_generics ())
  //    {
  //      for (auto &generic_param : function.get_generic_params ())
  // {
  //   switch (generic_param.get ()->get_kind ())
  //     {
  //     case HIR::GenericParam::GenericKind::LIFETIME:
  //       context->intern_and_insert_lifetime (
  // 	static_cast<HIR::LifetimeParam &> (*generic_param)
  // 	  .get_lifetime ());
  //       // TODO: handle bounds
  //       break;
  //     case HIR::GenericParam::GenericKind::CONST:
  //       // FIXME: Skipping Lifetime and Const completely until better
  //       // handling.
  //       break;

  //       case HIR::GenericParam::GenericKind::TYPE: {
  // 	auto param_type
  // 	  = TypeResolveGenericParam::Resolve (generic_param.get ());
  // 	context->insert_type (generic_param->get_mappings (),
  // 			      param_type);

  // 	substitutions.push_back (TyTy::SubstitutionParamMapping (
  // 	  static_cast<HIR::TypeParam &> (*generic_param), param_type));
  //       }
  //       break;
  //     }
  // }
  //    }

  //  TyTy::RegionConstraints region_constraints;
  //  if (function.has_where_clause ())
  //    {
  //      for (auto &where_clause_item : function.get_where_clause ().get_items
  //      ())
  // {
  //   ResolveWhereClauseItem::Resolve (*where_clause_item.get (),
  // 				   region_constraints);
  // }
  //    }

  //  TyTy::BaseType *ret_type = nullptr;
  //  if (!function.has_return_type ())
  //    ret_type
  //      = TyTy::TupleType::get_unit_type (function.get_mappings ().get_hirid
  //      ());
  //  else
  //    {
  //      auto resolved
  // = TypeCheckType::Resolve (function.get_return_type ().get ());
  //      if (resolved == nullptr)
  // {
  //   rust_error_at (function.get_locus (),
  // 		 "failed to resolve return type");
  //   return;
  // }

  //      ret_type = resolved->clone ();
  //      ret_type->set_ref (
  // function.get_return_type ()->get_mappings ().get_hirid ());
  //    }

  //  std::vector<std::pair<HIR::Pattern *, TyTy::BaseType *> > params;
  //  for (auto &param : function.get_function_params ())
  //    {
  //      // get the name as well required for later on
  //      auto param_tyty = TypeCheckType::Resolve (param.get_type ().get ());

  //      // these are implicit mappings and not used
  //      auto crate_num = mappings->get_current_crate ();
  //      Analysis::NodeMapping mapping (crate_num, mappings->get_next_node_id
  //      (),
  // 			     mappings->get_next_hir_id (crate_num),
  // 			     UNKNOWN_LOCAL_DEFID);

  //      HIR::IdentifierPattern *param_pattern
  // = new HIR::IdentifierPattern (mapping, param.get_param_name (),
  // 			      UNDEF_LOCATION, false, Mutability::Imm,
  // 			      std::unique_ptr<HIR::Pattern> (nullptr));

  //      params.push_back (
  // std::pair<HIR::Pattern *, TyTy::BaseType *> (param_pattern,
  // 					     param_tyty));

  //      context->insert_type (param.get_mappings (), param_tyty);

  //      // FIXME do we need error checking for patterns here?
  //      // see https://github.com/Rust-GCC/gccrs/issues/995
  //    }

  //  uint8_t flags = TyTy::FnType::FNTYPE_IS_EXTERN_FLAG;
  //  if (function.is_variadic ())
  //    {
  //      flags |= TyTy::FnType::FNTYPE_IS_VARADIC_FLAG;
  //      if (parent.get_abi () != Rust::ABI::C)
  // {
  //   rust_error_at (
  //     function.get_locus (), ErrorCode::E0045,
  //     "C-variadic function must have C or cdecl calling convention");
  // }
  //    }

  //  RustIdent ident{
  //    CanonicalPath::new_seg (function.get_mappings ().get_nodeid (),
  // 		    function.get_item_name ().as_string ()),
  //    function.get_locus ()};

  //  auto fnType = new TyTy::FnType (
  //    function.get_mappings ().get_hirid (),
  //    function.get_mappings ().get_defid (),
  //    function.get_item_name ().as_string (), ident, flags, parent.get_abi (),
  //    std::move (params), ret_type, std::move (substitutions),
  //    TyTy::SubstitutionArgumentMappings::empty (
  //      context->get_lifetime_resolver ().get_num_bound_regions ()),
  //    region_constraints);

  //  context->insert_type (function.get_mappings (), fnType);
  //  resolved = fnType;
}

TypeCheckImplItem::TypeCheckImplItem (
  HIR::ImplBlock *parent, TyTy::BaseType *self,
  std::vector<TyTy::SubstitutionParamMapping> substitutions)
  : TypeCheckBase (), parent (parent), self (self),
    substitutions (substitutions)
{}

TyTy::BaseType *
TypeCheckImplItem::Resolve (
  HIR::ImplBlock *parent, HIR::ImplItem *item, TyTy::BaseType *self,
  std::vector<TyTy::SubstitutionParamMapping> substitutions)
{
  // is it already resolved?
  auto context = TypeCheckContext::get ();
  TyTy::BaseType *resolved = nullptr;
  bool already_resolved
    = context->lookup_type (item->get_impl_mappings ().get_hirid (), &resolved);
  if (already_resolved)
    return resolved;

  // resolve
  TypeCheckImplItem resolver (parent, self, substitutions);
  item->accept_vis (resolver);
  return resolver.result;
}

void
TypeCheckImplItem::visit (HIR::Function &function)
{
  auto binder_pin = context->push_lifetime_binder ();

  if (function.has_generics ())
    resolve_generic_params (function.get_generic_params (), substitutions);

  TyTy::RegionConstraints region_constraints;
  for (auto &where_clause_item : function.get_where_clause ().get_items ())
    {
      ResolveWhereClauseItem::Resolve (*where_clause_item.get (),
				       region_constraints);
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
  if (function.is_method ())
    {
      // these are implicit mappings and not used
      auto crate_num = mappings->get_current_crate ();
      Analysis::NodeMapping mapping (crate_num, mappings->get_next_node_id (),
				     mappings->get_next_hir_id (crate_num),
				     UNKNOWN_LOCAL_DEFID);

      // add the synthetic self param at the front, this is a placeholder for
      // compilation to know parameter names. The types are ignored but we
      // reuse the HIR identifier pattern which requires it
      HIR::SelfParam &self_param = function.get_self_param ();
      // FIXME: which location should be used for Rust::Identifier for `self`?
      HIR::IdentifierPattern *self_pattern = new HIR::IdentifierPattern (
	mapping, {"self"}, self_param.get_locus (), self_param.is_ref (),
	self_param.get_mut (), std::unique_ptr<HIR::Pattern> (nullptr));

      // might have a specified type
      TyTy::BaseType *self_type = nullptr;
      if (self_param.has_type ())
	{
	  std::unique_ptr<HIR::Type> &specified_type = self_param.get_type ();
	  self_type = TypeCheckType::Resolve (specified_type.get ());
	}
      else
	{
	  switch (self_param.get_self_kind ())
	    {
	    case HIR::SelfParam::IMM:
	    case HIR::SelfParam::MUT:
	      self_type = self->clone ();
	      break;

	      case HIR::SelfParam::IMM_REF: {
		auto region = context->lookup_and_resolve_lifetime (
		  self_param.get_lifetime ());
		if (!region.has_value ())
		  {
		    rust_inform (self_param.get_locus (),
				 "failed to resolve lifetime");
		    region = TyTy::Region::make_anonymous (); // FIXME
		  }
		self_type = new TyTy::ReferenceType (
		  self_param.get_mappings ().get_hirid (),
		  TyTy::TyVar (self->get_ref ()), Mutability::Imm,
		  region.value ());
	      }
	      break;

	      case HIR::SelfParam::MUT_REF: {
		auto region = context->lookup_and_resolve_lifetime (
		  self_param.get_lifetime ());
		if (!region.has_value ())
		  {
		    rust_error_at (self_param.get_locus (),
				   "failed to resolve lifetime");
		    return;
		  }
		self_type = new TyTy::ReferenceType (
		  self_param.get_mappings ().get_hirid (),
		  TyTy::TyVar (self->get_ref ()), Mutability::Mut,
		  region.value ());
	      }
	      break;

	    default:
	      rust_unreachable ();
	      return;
	    }
	}

      context->insert_type (self_param.get_mappings (), self_type);
      params.push_back (
	std::pair<HIR::Pattern *, TyTy::BaseType *> (self_pattern, self_type));
    }

  for (auto &param : function.get_function_params ())
    {
      // get the name as well required for later on
      auto param_tyty = TypeCheckType::Resolve (param.get_type ().get ());
      params.push_back (std::pair<HIR::Pattern *, TyTy::BaseType *> (
	param.get_param_name ().get (), param_tyty));

      context->insert_type (param.get_mappings (), param_tyty);
      TypeCheckPattern::Resolve (param.get_param_name ().get (), param_tyty);
    }

  const CanonicalPath *canonical_path = nullptr;
  bool ok
    = mappings->lookup_canonical_path (function.get_mappings ().get_nodeid (),
				       &canonical_path);
  rust_assert (ok);

  RustIdent ident{*canonical_path, function.get_locus ()};
  auto fnType = new TyTy::FnType (
    function.get_mappings ().get_hirid (),
    function.get_mappings ().get_defid (),
    function.get_function_name ().as_string (), ident,
    function.is_method () ? TyTy::FnType::FNTYPE_IS_METHOD_FLAG
			  : TyTy::FnType::FNTYPE_DEFAULT_FLAGS,
    ABI::RUST, std::move (params), ret_type, std::move (substitutions),
    TyTy::SubstitutionArgumentMappings::empty (
      context->get_lifetime_resolver ().get_num_bound_regions ()),
    region_constraints);

  context->insert_type (function.get_mappings (), fnType);
  result = fnType;

  // need to get the return type from this
  TyTy::FnType *resolve_fn_type = fnType;
  auto expected_ret_tyty = resolve_fn_type->get_return_type ();
  context->push_return_type (TypeCheckContextItem (parent, &function),
			     expected_ret_tyty);

  auto block_expr_ty
    = TypeCheckExpr::Resolve (function.get_definition ().get ());

  location_t fn_return_locus = function.has_function_return_type ()
				 ? function.get_return_type ()->get_locus ()
				 : function.get_locus ();

  coercion_site (function.get_definition ()->get_mappings ().get_hirid (),
		 TyTy::TyWithLocation (expected_ret_tyty, fn_return_locus),
		 TyTy::TyWithLocation (block_expr_ty),
		 function.get_definition ()->get_locus ());

  context->pop_return_type ();
}

void
TypeCheckImplItem::visit (HIR::ConstantItem &constant)
{
  TyTy::BaseType *type = TypeCheckType::Resolve (constant.get_type ().get ());
  TyTy::BaseType *expr_type
    = TypeCheckExpr::Resolve (constant.get_expr ().get ());

  TyTy::BaseType *unified = unify_site (
    constant.get_mappings ().get_hirid (),
    TyTy::TyWithLocation (type, constant.get_type ()->get_locus ()),
    TyTy::TyWithLocation (expr_type, constant.get_expr ()->get_locus ()),
    constant.get_locus ());
  context->insert_type (constant.get_mappings (), unified);
  result = unified;
}

void
TypeCheckImplItem::visit (HIR::TypeAlias &alias)
{
  auto binder_pin = context->push_lifetime_binder ();

  if (alias.has_generics ())
    resolve_generic_params (alias.get_generic_params (), substitutions);

  TyTy::BaseType *actual_type
    = TypeCheckType::Resolve (alias.get_type_aliased ().get ());

  context->insert_type (alias.get_mappings (), actual_type);
  result = actual_type;
  TyTy::RegionConstraints region_constraints;
  for (auto &where_clause_item : alias.get_where_clause ().get_items ())
    {
      ResolveWhereClauseItem::Resolve (*where_clause_item.get (),
				       region_constraints);
    }
}

TypeCheckImplItemWithTrait::TypeCheckImplItemWithTrait (
  HIR::ImplBlock *parent, TyTy::BaseType *self,
  TyTy::TypeBoundPredicate &trait_reference,
  std::vector<TyTy::SubstitutionParamMapping> substitutions)
  : TypeCheckBase (), trait_reference (trait_reference),
    resolved_trait_item (TyTy::TypeBoundPredicateItem::error ()),
    parent (parent), self (self), substitutions (substitutions)
{
  rust_assert (is_trait_impl_block ());
}

TyTy::TypeBoundPredicateItem
TypeCheckImplItemWithTrait::Resolve (
  HIR::ImplBlock *parent, HIR::ImplItem *item, TyTy::BaseType *self,
  TyTy::TypeBoundPredicate &trait_reference,
  std::vector<TyTy::SubstitutionParamMapping> substitutions)
{
  TypeCheckImplItemWithTrait resolver (parent, self, trait_reference,
				       substitutions);
  item->accept_vis (resolver);
  return resolver.resolved_trait_item;
}

void
TypeCheckImplItemWithTrait::visit (HIR::ConstantItem &constant)
{
  // normal resolution of the item
  TyTy::BaseType *lookup
    = TypeCheckImplItem::Resolve (parent, &constant, self, substitutions);

  // map the impl item to the associated trait item
  const auto tref = trait_reference.get ();
  const TraitItemReference *raw_trait_item = nullptr;
  bool found
    = tref->lookup_trait_item_by_type (constant.get_identifier ().as_string (),
				       TraitItemReference::TraitItemType::CONST,
				       &raw_trait_item);

  // unknown trait item - https://doc.rust-lang.org/error_codes/E0323.html
  if (!found || raw_trait_item->is_error ())
    {
      rich_location r (line_table, constant.get_locus ());
      r.add_range (trait_reference.get_locus ());
      rust_error_at (r, ErrorCode::E0323,
		     "item %qs is an associated const, which does not match "
		     "its trait %qs",
		     constant.get_identifier ().as_string ().c_str (),
		     trait_reference.get_name ().c_str ());
      return;
    }

  // get the item from the predicate
  resolved_trait_item = trait_reference.lookup_associated_item (raw_trait_item);
  rust_assert (!resolved_trait_item.is_error ());

  // merge the attributes
  const HIR::TraitItem *hir_trait_item
    = resolved_trait_item.get_raw_item ()->get_hir_trait_item ();
  merge_attributes (constant.get_outer_attrs (), *hir_trait_item);

  // check the types are compatible
  auto trait_item_type = resolved_trait_item.get_tyty_for_receiver (self);
  if (!types_compatable (TyTy::TyWithLocation (trait_item_type),
			 TyTy::TyWithLocation (lookup), constant.get_locus (),
			 true /*emit_errors*/))
    {
      rich_location r (line_table, constant.get_locus ());
      r.add_range (resolved_trait_item.get_locus ());

      rust_error_at (
	r, "constant %qs has an incompatible type for trait %qs",
	constant.get_identifier ().as_string ().c_str (),
	trait_reference.get_name ().c_str ());
    }
}

void
TypeCheckImplItemWithTrait::visit (HIR::TypeAlias &type)
{
  // normal resolution of the item
  TyTy::BaseType *lookup
    = TypeCheckImplItem::Resolve (parent, &type, self, substitutions);

  // map the impl item to the associated trait item
  const auto tref = trait_reference.get ();
  const TraitItemReference *raw_trait_item = nullptr;
  bool found
    = tref->lookup_trait_item_by_type (type.get_new_type_name ().as_string (),
				       TraitItemReference::TraitItemType::TYPE,
				       &raw_trait_item);

  // unknown trait item
  if (!found || raw_trait_item->is_error ())
    {
      rich_location r (line_table, type.get_locus ());
      r.add_range (trait_reference.get_locus ());
      rust_error_at (r, "type alias %qs is not a member of trait %qs",
		     type.get_new_type_name ().as_string ().c_str (),
		     trait_reference.get_name ().c_str ());
      return;
    }

  // get the item from the predicate
  resolved_trait_item = trait_reference.lookup_associated_item (raw_trait_item);
  rust_assert (!resolved_trait_item.is_error ());

  // merge the attributes
  const HIR::TraitItem *hir_trait_item
    = resolved_trait_item.get_raw_item ()->get_hir_trait_item ();
  merge_attributes (type.get_outer_attrs (), *hir_trait_item);

  // check the types are compatible
  auto trait_item_type = resolved_trait_item.get_tyty_for_receiver (self);
  if (!types_compatable (TyTy::TyWithLocation (trait_item_type),
			 TyTy::TyWithLocation (lookup), type.get_locus (),
			 true /*emit_errors*/))
    {
      rich_location r (line_table, type.get_locus ());
      r.add_range (resolved_trait_item.get_locus ());

      rust_error_at (
	r, "type alias %qs has an incompatible type for trait %qs",
	type.get_new_type_name ().as_string ().c_str (),
	trait_reference.get_name ().c_str ());
    }

  // its actually a projection, since we need a way to actually bind the
  // generic substitutions to the type itself
  TyTy::ProjectionType *projection
    = new TyTy::ProjectionType (type.get_mappings ().get_hirid (), lookup, tref,
				raw_trait_item->get_mappings ().get_defid (),
				substitutions);

  context->insert_type (type.get_mappings (), projection);
  raw_trait_item->associated_type_set (projection);
}

void
TypeCheckImplItemWithTrait::visit (HIR::Function &function)
{
  // normal resolution of the item
  TyTy::BaseType *lookup
    = TypeCheckImplItem::Resolve (parent, &function, self, substitutions);

  // map the impl item to the associated trait item
  const auto tref = trait_reference.get ();
  const TraitItemReference *raw_trait_item = nullptr;
  bool found = tref->lookup_trait_item_by_type (
    function.get_function_name ().as_string (),
    TraitItemReference::TraitItemType::FN, &raw_trait_item);

  // unknown trait item
  if (!found || raw_trait_item->is_error ())
    {
      rich_location r (line_table, function.get_locus ());
      r.add_range (trait_reference.get_locus ());
      rust_error_at (r, "method %qs is not a member of trait %qs",
		     function.get_function_name ().as_string ().c_str (),
		     trait_reference.get_name ().c_str ());
      return;
    }

  // get the item from the predicate
  resolved_trait_item = trait_reference.lookup_associated_item (raw_trait_item);
  rust_assert (!resolved_trait_item.is_error ());

  // merge the attributes
  const HIR::TraitItem *hir_trait_item
    = resolved_trait_item.get_raw_item ()->get_hir_trait_item ();
  merge_attributes (function.get_outer_attrs (), *hir_trait_item);

  // check the types are compatible
  auto trait_item_type = resolved_trait_item.get_tyty_for_receiver (self);
  if (!types_compatable (TyTy::TyWithLocation (trait_item_type),
			 TyTy::TyWithLocation (lookup), function.get_locus (),
			 true /*emit_errors*/))
    {
      rich_location r (line_table, function.get_locus ());
      r.add_range (resolved_trait_item.get_locus ());

      rust_error_at (r, ErrorCode::E0053,
		     "method %qs has an incompatible type for trait %qs",
		     function.get_function_name ().as_string ().c_str (),
		     trait_reference.get_name ().c_str ());
    }
}

void
TypeCheckImplItemWithTrait::merge_attributes (AST::AttrVec &impl_item_attrs,
					      const HIR::TraitItem &trait_item)
{
  for (const auto &attr : trait_item.get_outer_attrs ())
    {
      impl_item_attrs.push_back (attr);
    }
}

bool
TypeCheckImplItemWithTrait::is_trait_impl_block () const
{
  return !trait_reference.is_error ();
}

} // namespace Resolver
} // namespace Rust
