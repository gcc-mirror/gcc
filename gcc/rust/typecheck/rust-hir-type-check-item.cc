// Copyright (C) 2020-2025 Free Software Foundation, Inc.

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

#include "rust-hir-type-check-item.h"
#include "optional.h"
#include "rust-canonical-path.h"
#include "rust-diagnostics.h"
#include "rust-hir-type-check-enumitem.h"
#include "rust-hir-type-check-implitem.h"
#include "rust-hir-type-check-type.h"
#include "rust-hir-type-check-expr.h"
#include "rust-hir-type-check-pattern.h"
#include "rust-hir-trait-resolve.h"
#include "rust-identifier.h"
#include "rust-session-manager.h"
#include "rust-immutable-name-resolution-context.h"
#include "rust-substitution-mapper.h"
#include "rust-type-util.h"
#include "rust-tyty-variance-analysis.h"

// for flag_name_resolution_2_0
#include "options.h"

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
  auto result
    = resolver.resolve_impl_block_substitutions (impl_block, failed_flag);
  if (failed_flag)
    {
      return new TyTy::ErrorType (impl_block.get_mappings ().get_hirid ());
    }
  std::vector<TyTy::SubstitutionParamMapping> substitutions
    = std::move (result.first);
  TyTy::RegionConstraints region_constraints = std::move (result.second);

  return resolver.resolve_impl_block_self (impl_block);
}

TyTy::BaseType *
TypeCheckItem::ResolveImplBlockSelfWithInference (
  HIR::ImplBlock &impl, location_t locus,
  TyTy::SubstitutionArgumentMappings *infer_arguments)
{
  TypeCheckItem resolver;

  bool failed_flag = false;
  auto result = resolver.resolve_impl_block_substitutions (impl, failed_flag);
  if (failed_flag)
    {
      return new TyTy::ErrorType (impl.get_mappings ().get_hirid ());
    }
  std::vector<TyTy::SubstitutionParamMapping> substitutions
    = std::move (result.first);
  TyTy::RegionConstraints region_constraints = std::move (result.second);

  // now that we have the param mappings we need to query the self type
  TyTy::BaseType *self = resolver.resolve_impl_block_self (impl);

  // nothing to do
  if (substitutions.empty () || self->is_concrete ())
    return self;

  // generate inference variables for the subst-param-mappings
  std::vector<TyTy::SubstitutionArg> args;
  for (auto &p : substitutions)
    {
      if (p.needs_substitution ())
	{
	  TyTy::TyVar infer_var = TyTy::TyVar::get_implicit_infer_var (locus);
	  args.push_back (TyTy::SubstitutionArg (&p, infer_var.get_tyty ()));
	}
      else
	{
	  TyTy::ParamType *param = p.get_param_ty ();
	  TyTy::BaseType *resolved = param->destructure ();
	  args.push_back (TyTy::SubstitutionArg (&p, resolved));
	}
    }

  // create argument mappings
  *infer_arguments = TyTy::SubstitutionArgumentMappings (
    std::move (args), {},
    TyTy::SubstitutionArgumentMappings::regions_from_nullable_args (
      infer_arguments),
    locus);

  TyTy::BaseType *infer = SubstMapperInternal::Resolve (self, *infer_arguments);

  // we only need to apply to the bounds manually on types which dont bind
  // generics
  if (!infer->has_substitutions_defined ())
    {
      for (auto &bound : infer->get_specified_bounds ())
	bound.handle_substitions (*infer_arguments);
    }

  return infer;
}

void
TypeCheckItem::visit (HIR::TypeAlias &alias)
{
  TyTy::BaseType *actual_type
    = TypeCheckType::Resolve (alias.get_type_aliased ());

  context->insert_type (alias.get_mappings (), actual_type);

  TyTy::RegionConstraints region_constraints;
  for (auto &where_clause_item : alias.get_where_clause ().get_items ())
    {
      ResolveWhereClauseItem::Resolve (*where_clause_item, region_constraints);
    }
  infered = actual_type;
}

void
TypeCheckItem::visit (HIR::TupleStruct &struct_decl)
{
  auto lifetime_pin = context->push_clean_lifetime_resolver ();

  std::vector<TyTy::SubstitutionParamMapping> substitutions;
  if (struct_decl.has_generics ())
    resolve_generic_params (struct_decl.get_generic_params (), substitutions);

  TyTy::RegionConstraints region_constraints;
  for (auto &where_clause_item : struct_decl.get_where_clause ().get_items ())
    {
      ResolveWhereClauseItem::Resolve (*where_clause_item, region_constraints);
    }

  std::vector<TyTy::StructFieldType *> fields;
  size_t idx = 0;
  for (auto &field : struct_decl.get_fields ())
    {
      TyTy::BaseType *field_type
	= TypeCheckType::Resolve (field.get_field_type ());
      auto *ty_field
	= new TyTy::StructFieldType (field.get_mappings ().get_hirid (),
				     std::to_string (idx), field_type,
				     field.get_locus ());
      fields.push_back (ty_field);
      context->insert_type (field.get_mappings (), ty_field->get_field_type ());
      idx++;
    }

  // get the path

  auto path = CanonicalPath::create_empty ();

  // FIXME: HACK: ARTHUR: Disgusting
  if (flag_name_resolution_2_0)
    {
      auto &nr_ctx
	= Resolver2_0::ImmutableNameResolutionContext::get ().resolver ();

      path = nr_ctx.values
	       .to_canonical_path (struct_decl.get_mappings ().get_nodeid ())
	       .value ();
    }
  else
    {
      path
	= mappings
	    .lookup_canonical_path (struct_decl.get_mappings ().get_nodeid ())
	    .value ();
    }

  RustIdent ident{path, struct_decl.get_locus ()};

  // its a single variant ADT
  std::vector<TyTy::VariantDef *> variants;
  variants.push_back (
    new TyTy::VariantDef (struct_decl.get_mappings ().get_hirid (),
			  struct_decl.get_mappings ().get_defid (),
			  struct_decl.get_identifier ().as_string (), ident,
			  TyTy::VariantDef::VariantType::TUPLE, tl::nullopt,
			  std::move (fields)));

  // Process #[repr(X)] attribute, if any
  const AST::AttrVec &attrs = struct_decl.get_outer_attrs ();
  TyTy::ADTType::ReprOptions repr
    = parse_repr_options (attrs, struct_decl.get_locus ());

  auto *type = new TyTy::ADTType (
    struct_decl.get_mappings ().get_defid (),
    struct_decl.get_mappings ().get_hirid (),
    struct_decl.get_mappings ().get_hirid (),
    struct_decl.get_identifier ().as_string (), ident,
    TyTy::ADTType::ADTKind::TUPLE_STRUCT, std::move (variants),
    std::move (substitutions), repr,
    TyTy::SubstitutionArgumentMappings::empty (
      context->get_lifetime_resolver ().get_num_bound_regions ()),
    region_constraints);

  context->insert_type (struct_decl.get_mappings (), type);
  infered = type;

  context->get_variance_analysis_ctx ().add_type_constraints (*type);
}

void
TypeCheckItem::visit (HIR::StructStruct &struct_decl)
{
  auto lifetime_pin = context->push_clean_lifetime_resolver ();

  std::vector<TyTy::SubstitutionParamMapping> substitutions;
  if (struct_decl.has_generics ())
    resolve_generic_params (struct_decl.get_generic_params (), substitutions);

  TyTy::RegionConstraints region_constraints;
  for (auto &where_clause_item : struct_decl.get_where_clause ().get_items ())
    {
      ResolveWhereClauseItem::Resolve (*where_clause_item, region_constraints);
    }

  std::vector<TyTy::StructFieldType *> fields;
  for (auto &field : struct_decl.get_fields ())
    {
      TyTy::BaseType *field_type
	= TypeCheckType::Resolve (field.get_field_type ());
      auto *ty_field
	= new TyTy::StructFieldType (field.get_mappings ().get_hirid (),
				     field.get_field_name ().as_string (),
				     field_type, field.get_locus ());
      fields.push_back (ty_field);
      context->insert_type (field.get_mappings (), ty_field->get_field_type ());
    }

  auto path = CanonicalPath::create_empty ();

  // FIXME: HACK: ARTHUR: Disgusting
  if (flag_name_resolution_2_0)
    {
      auto &nr_ctx
	= Resolver2_0::ImmutableNameResolutionContext::get ().resolver ();
      auto canonical_path = nr_ctx.types.to_canonical_path (
	struct_decl.get_mappings ().get_nodeid ());

      if (!canonical_path.has_value ())
	rust_unreachable ();
      path = canonical_path.value ();
    }
  else
    {
      path
	= mappings
	    .lookup_canonical_path (struct_decl.get_mappings ().get_nodeid ())
	    .value ();
    }

  RustIdent ident{path, struct_decl.get_locus ()};

  // its a single variant ADT
  std::vector<TyTy::VariantDef *> variants;
  variants.push_back (
    new TyTy::VariantDef (struct_decl.get_mappings ().get_hirid (),
			  struct_decl.get_mappings ().get_defid (),
			  struct_decl.get_identifier ().as_string (), ident,
			  TyTy::VariantDef::VariantType::STRUCT, tl::nullopt,
			  std::move (fields)));

  // Process #[repr(X)] attribute, if any
  const AST::AttrVec &attrs = struct_decl.get_outer_attrs ();
  TyTy::ADTType::ReprOptions repr
    = parse_repr_options (attrs, struct_decl.get_locus ());

  auto *type = new TyTy::ADTType (
    struct_decl.get_mappings ().get_defid (),
    struct_decl.get_mappings ().get_hirid (),
    struct_decl.get_mappings ().get_hirid (),
    struct_decl.get_identifier ().as_string (), ident,
    TyTy::ADTType::ADTKind::STRUCT_STRUCT, std::move (variants),
    std::move (substitutions), repr,
    TyTy::SubstitutionArgumentMappings::empty (
      context->get_lifetime_resolver ().get_num_bound_regions ()),
    region_constraints);

  context->insert_type (struct_decl.get_mappings (), type);
  infered = type;

  context->get_variance_analysis_ctx ().add_type_constraints (*type);
}

void
TypeCheckItem::visit (HIR::Enum &enum_decl)
{
  auto lifetime_pin = context->push_clean_lifetime_resolver ();
  std::vector<TyTy::SubstitutionParamMapping> substitutions;
  if (enum_decl.has_generics ())
    resolve_generic_params (enum_decl.get_generic_params (), substitutions);

  // Process #[repr(X)] attribute, if any
  const AST::AttrVec &attrs = enum_decl.get_outer_attrs ();
  TyTy::ADTType::ReprOptions repr
    = parse_repr_options (attrs, enum_decl.get_locus ());

  std::vector<TyTy::VariantDef *> variants;
  int64_t discriminant_value = 0;
  for (auto &variant : enum_decl.get_variants ())
    {
      TyTy::VariantDef *field_type
	= TypeCheckEnumItem::Resolve (*variant, discriminant_value);

      discriminant_value++;
      variants.push_back (field_type);
    }

  // Check for zero-variant enum compatibility
  if (enum_decl.is_zero_variant ())
    {
      if (repr.repr_kind == TyTy::ADTType::ReprKind::INT
	  || repr.repr_kind == TyTy::ADTType::ReprKind::C)
	{
	  rust_error_at (enum_decl.get_locus (),
			 "unsupported representation for zero-variant enum");
	  return;
	}
    }

  // get the path
  tl::optional<CanonicalPath> canonical_path;

  if (flag_name_resolution_2_0)
    {
      auto &nr_ctx
	= Resolver2_0::ImmutableNameResolutionContext::get ().resolver ();

      canonical_path = nr_ctx.types.to_canonical_path (
	enum_decl.get_mappings ().get_nodeid ());
    }
  else
    {
      canonical_path = mappings.lookup_canonical_path (
	enum_decl.get_mappings ().get_nodeid ());
    }

  rust_assert (canonical_path.has_value ());

  RustIdent ident{*canonical_path, enum_decl.get_locus ()};

  // multi variant ADT
  auto *type
    = new TyTy::ADTType (enum_decl.get_mappings ().get_defid (),
			 enum_decl.get_mappings ().get_hirid (),
			 enum_decl.get_mappings ().get_hirid (),
			 enum_decl.get_identifier ().as_string (), ident,
			 TyTy::ADTType::ADTKind::ENUM, std::move (variants),
			 std::move (substitutions), repr);

  context->insert_type (enum_decl.get_mappings (), type);
  infered = type;

  context->get_variance_analysis_ctx ().add_type_constraints (*type);
}

void
TypeCheckItem::visit (HIR::Union &union_decl)
{
  auto lifetime_pin = context->push_clean_lifetime_resolver ();
  std::vector<TyTy::SubstitutionParamMapping> substitutions;
  if (union_decl.has_generics ())
    resolve_generic_params (union_decl.get_generic_params (), substitutions);

  TyTy::RegionConstraints region_constraints;
  for (auto &where_clause_item : union_decl.get_where_clause ().get_items ())
    {
      ResolveWhereClauseItem::Resolve (*where_clause_item, region_constraints);
    }

  std::vector<TyTy::StructFieldType *> fields;
  for (auto &variant : union_decl.get_variants ())
    {
      TyTy::BaseType *variant_type
	= TypeCheckType::Resolve (variant.get_field_type ());
      auto *ty_variant
	= new TyTy::StructFieldType (variant.get_mappings ().get_hirid (),
				     variant.get_field_name ().as_string (),
				     variant_type, variant.get_locus ());
      fields.push_back (ty_variant);
      context->insert_type (variant.get_mappings (),
			    ty_variant->get_field_type ());
    }

  // get the path
  tl::optional<CanonicalPath> canonical_path;

  if (flag_name_resolution_2_0)
    {
      auto &nr_ctx
	= Resolver2_0::ImmutableNameResolutionContext::get ().resolver ();

      canonical_path = nr_ctx.types.to_canonical_path (
	union_decl.get_mappings ().get_nodeid ());
    }
  else
    {
      canonical_path = mappings.lookup_canonical_path (
	union_decl.get_mappings ().get_nodeid ());
    }

  rust_assert (canonical_path.has_value ());

  RustIdent ident{*canonical_path, union_decl.get_locus ()};

  // there is only a single variant
  std::vector<TyTy::VariantDef *> variants;
  variants.push_back (
    new TyTy::VariantDef (union_decl.get_mappings ().get_hirid (),
			  union_decl.get_mappings ().get_defid (),
			  union_decl.get_identifier ().as_string (), ident,
			  TyTy::VariantDef::VariantType::STRUCT, tl::nullopt,
			  std::move (fields)));

  auto *type
    = new TyTy::ADTType (union_decl.get_mappings ().get_defid (),
			 union_decl.get_mappings ().get_hirid (),
			 union_decl.get_mappings ().get_hirid (),
			 union_decl.get_identifier ().as_string (), ident,
			 TyTy::ADTType::ADTKind::UNION, std::move (variants),
			 std::move (substitutions));

  context->insert_type (union_decl.get_mappings (), type);
  infered = type;

  context->get_variance_analysis_ctx ().add_type_constraints (*type);
}

void
TypeCheckItem::visit (HIR::StaticItem &var)
{
  TyTy::BaseType *type = TypeCheckType::Resolve (var.get_type ());
  TyTy::BaseType *expr_type = TypeCheckExpr::Resolve (var.get_expr ());

  TyTy::BaseType *unified
    = coercion_site (var.get_mappings ().get_hirid (),
		     TyTy::TyWithLocation (type, var.get_type ().get_locus ()),
		     TyTy::TyWithLocation (expr_type,
					   var.get_expr ().get_locus ()),
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
    TyTy::TyWithLocation (type, constant.get_type ().get_locus ()),
    TyTy::TyWithLocation (expr_type, constant.get_expr ().get_locus ()),
    constant.get_locus ());
  context->insert_type (constant.get_mappings (), unified);
  infered = unified;
}

void
TypeCheckItem::visit (HIR::ImplBlock &impl_block)
{
  auto binder_pin = context->push_clean_lifetime_resolver (true);

  TraitReference *trait_reference = &TraitReference::error_node ();
  if (impl_block.has_trait_ref ())
    {
      HIR::TypePath &ref = impl_block.get_trait_ref ();
      trait_reference = TraitResolver::Resolve (ref);
      if (trait_reference->is_error ())
	return;
    }

  bool failed_flag = false;
  auto result = resolve_impl_block_substitutions (impl_block, failed_flag);
  if (failed_flag)
    {
      infered = new TyTy::ErrorType (impl_block.get_mappings ().get_hirid ());
      return;
    }
  std::vector<TyTy::SubstitutionParamMapping> substitutions
    = std::move (result.first);
  TyTy::RegionConstraints region_constraints = std::move (result.second);

  TyTy::BaseType *self = resolve_impl_block_self (impl_block);

  // resolve each impl_item
  for (auto &impl_item : impl_block.get_impl_items ())
    {
      TypeCheckImplItem::Resolve (impl_block, *impl_item, self, substitutions);
    }

  // validate the impl items
  validate_trait_impl_block (trait_reference, impl_block, self, substitutions);
}

TyTy::BaseType *
TypeCheckItem::resolve_impl_item (HIR::ImplBlock &impl_block,
				  HIR::ImplItem &item)
{
  bool failed_flag = false;
  auto result = resolve_impl_block_substitutions (impl_block, failed_flag);
  if (failed_flag)
    {
      return new TyTy::ErrorType (impl_block.get_mappings ().get_hirid ());
    }

  std::vector<TyTy::SubstitutionParamMapping> substitutions
    = std::move (result.first);
  TyTy::RegionConstraints region_constraints = std::move (result.second);

  TyTy::BaseType *self = resolve_impl_block_self (impl_block);

  return TypeCheckImplItem::Resolve (impl_block, item, self, substitutions);
}

void
TypeCheckItem::visit (HIR::Function &function)
{
  auto lifetime_pin = context->push_clean_lifetime_resolver ();
  std::vector<TyTy::SubstitutionParamMapping> substitutions;
  if (function.has_generics ())
    resolve_generic_params (function.get_generic_params (),
			    substitutions); // TODO resolve constraints

  TyTy::RegionConstraints region_constraints;
  for (auto &where_clause_item : function.get_where_clause ().get_items ())
    {
      ResolveWhereClauseItem::Resolve (*where_clause_item, region_constraints);
    }

  TyTy::BaseType *ret_type = nullptr;
  if (!function.has_function_return_type ())
    ret_type = TyTy::TupleType::get_unit_type ();
  else
    {
      auto resolved = TypeCheckType::Resolve (function.get_return_type ());
      if (resolved->get_kind () == TyTy::TypeKind::ERROR)
	{
	  rust_error_at (function.get_locus (),
			 "failed to resolve return type");
	  return;
	}

      ret_type = resolved->clone ();
      ret_type->set_ref (
	function.get_return_type ().get_mappings ().get_hirid ());
    }

  std::vector<TyTy::FnParam> params;
  for (auto &param : function.get_function_params ())
    {
      // get the name as well required for later on
      auto param_tyty = TypeCheckType::Resolve (param.get_type ());
      context->insert_type (param.get_mappings (), param_tyty);
      TypeCheckPattern::Resolve (param.get_param_name (), param_tyty);
      params.push_back (
	TyTy::FnParam (param.get_param_name ().clone_pattern (), param_tyty));
    }

  auto path = CanonicalPath::create_empty ();

  // FIXME: HACK: ARTHUR: Disgusting
  if (flag_name_resolution_2_0)
    {
      auto &nr_ctx
	= Resolver2_0::ImmutableNameResolutionContext::get ().resolver ();
      auto canonical_path = nr_ctx.values.to_canonical_path (
	function.get_mappings ().get_nodeid ());

      path = canonical_path.value ();
    }
  else
    {
      path = mappings
	       .lookup_canonical_path (function.get_mappings ().get_nodeid ())
	       .value ();
    }

  RustIdent ident{path, function.get_locus ()};

  auto fn_type = new TyTy::FnType (
    function.get_mappings ().get_hirid (),
    function.get_mappings ().get_defid (),
    function.get_function_name ().as_string (), ident,
    TyTy::FnType::FNTYPE_DEFAULT_FLAGS, ABI::RUST, std::move (params), ret_type,
    std::move (substitutions),
    TyTy::SubstitutionArgumentMappings::empty (
      context->get_lifetime_resolver ().get_num_bound_regions ()),
    region_constraints);

  context->insert_type (function.get_mappings (), fn_type);

  // need to get the return type from this
  TyTy::FnType *resolved_fn_type = fn_type;
  auto expected_ret_tyty = resolved_fn_type->get_return_type ();
  context->push_return_type (TypeCheckContextItem (&function),
			     expected_ret_tyty);

  context->switch_to_fn_body ();
  auto block_expr_ty = TypeCheckExpr::Resolve (function.get_definition ());

  // emit check for
  // error[E0121]: the type placeholder `_` is not allowed within types on item
  const auto placeholder = ret_type->contains_infer ();
  if (placeholder != nullptr && function.has_return_type ())
    {
      // FIXME
      // this will be a great place for the Default Hir Visitor we want to
      // grab the locations of the placeholders (HIR::InferredType) their
      // location, for now maybe we can use their hirid to lookup the location
      location_t placeholder_locus
	= mappings.lookup_location (placeholder->get_ref ());
      location_t type_locus = function.get_return_type ().get_locus ();
      rich_location r (line_table, placeholder_locus);

      bool have_expected_type
	= block_expr_ty != nullptr && !block_expr_ty->is<TyTy::ErrorType> ();
      if (!have_expected_type)
	{
	  r.add_range (type_locus);
	}
      else
	{
	  std::string fixit
	    = "replace with the correct type " + block_expr_ty->get_name ();
	  r.add_fixit_replace (type_locus, fixit.c_str ());
	}

      rust_error_at (r, ErrorCode::E0121,
		     "the type placeholder %<_%> is not allowed within types "
		     "on item signatures");
    }

  location_t fn_return_locus = function.has_function_return_type ()
				 ? function.get_return_type ().get_locus ()
				 : function.get_locus ();
  coercion_site (function.get_definition ().get_mappings ().get_hirid (),
		 TyTy::TyWithLocation (expected_ret_tyty, fn_return_locus),
		 TyTy::TyWithLocation (block_expr_ty),
		 function.get_definition ().get_locus ());

  context->pop_return_type ();

  infered = fn_type;
}

void
TypeCheckItem::visit (HIR::Module &module)
{
  for (auto &item : module.get_items ())
    TypeCheckItem::Resolve (*item);
}

void
TypeCheckItem::visit (HIR::Trait &trait)
{
  if (trait.has_type_param_bounds ())
    {
      for (auto &tp_bound : trait.get_type_param_bounds ())
	{
	  if (tp_bound.get ()->get_bound_type ()
	      == HIR::TypeParamBound::BoundType::TRAITBOUND)
	    {
	      HIR::TraitBound &tb
		= static_cast<HIR::TraitBound &> (*tp_bound.get ());
	      if (tb.get_polarity () == BoundPolarity::AntiBound)
		{
		  rust_error_at (tb.get_locus (),
				 "%<?Trait%> is not permitted in supertraits");
		}
	    }
	}
    }

  TraitReference *trait_ref = TraitResolver::Resolve (trait);
  if (trait_ref->is_error ())
    {
      infered = new TyTy::ErrorType (trait.get_mappings ().get_hirid ());
      return;
    }

  RustIdent ident{CanonicalPath::create_empty (), trait.get_locus ()};
  infered = new TyTy::DynamicObjectType (
    trait.get_mappings ().get_hirid (), ident,
    {TyTy::TypeBoundPredicate (*trait_ref, BoundPolarity::RegularBound,
			       trait.get_locus ())});
}

void
TypeCheckItem::visit (HIR::ExternBlock &extern_block)
{
  for (auto &item : extern_block.get_extern_items ())
    {
      TypeCheckTopLevelExternItem::Resolve (*item, extern_block);
    }
}

std::pair<std::vector<TyTy::SubstitutionParamMapping>, TyTy::RegionConstraints>
TypeCheckItem::resolve_impl_block_substitutions (HIR::ImplBlock &impl_block,
						 bool &failure_flag)
{
  std::vector<TyTy::SubstitutionParamMapping> substitutions;
  if (impl_block.has_generics ())
    resolve_generic_params (impl_block.get_generic_params (), substitutions);

  TyTy::RegionConstraints region_constraints;
  for (auto &where_clause_item : impl_block.get_where_clause ().get_items ())
    {
      ResolveWhereClauseItem::Resolve (*where_clause_item, region_constraints);
    }

  auto specified_bound = TyTy::TypeBoundPredicate::error ();
  TraitReference *trait_reference = &TraitReference::error_node ();
  if (impl_block.has_trait_ref ())
    {
      auto &ref = impl_block.get_trait_ref ();
      trait_reference = TraitResolver::Resolve (ref);
      rust_assert (!trait_reference->is_error ());

      // we don't error out here see: gcc/testsuite/rust/compile/traits2.rs
      // for example
      specified_bound = get_predicate_from_bound (ref, impl_block.get_type (),
						  impl_block.get_polarity ());
    }

  TyTy::BaseType *self = TypeCheckType::Resolve (impl_block.get_type ());
  if (self->is<TyTy::ErrorType> ())
    {
      // we cannot check for unconstrained type arguments when the Self type is
      // not resolved it will just add extra errors that dont help as well as
      // the case where this could just be a recursive type query that should
      // fail and will work later on anyway
      return {substitutions, region_constraints};
    }

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

  return {substitutions, region_constraints};
}

void
TypeCheckItem::validate_trait_impl_block (
  TraitReference *trait_reference, HIR::ImplBlock &impl_block,
  TyTy::BaseType *self,
  std::vector<TyTy::SubstitutionParamMapping> &substitutions)
{
  auto specified_bound = TyTy::TypeBoundPredicate::error ();
  if (impl_block.has_trait_ref ())
    {
      auto &ref = impl_block.get_trait_ref ();
      trait_reference = TraitResolver::Resolve (ref);
      if (trait_reference->is_error ())
	return;

      // we don't error out here see: gcc/testsuite/rust/compile/traits2.rs
      // for example
      specified_bound = get_predicate_from_bound (ref, impl_block.get_type (),
						  impl_block.get_polarity ());

      // need to check that if this specified bound has super traits does this
      // Self
      // implement them?
      specified_bound.validate_type_implements_super_traits (
	*self, impl_block.get_type (), impl_block.get_trait_ref ());
    }

  bool is_trait_impl_block = !trait_reference->is_error ();
  std::vector<const TraitItemReference *> trait_item_refs;
  for (auto &impl_item : impl_block.get_impl_items ())
    {
      if (!specified_bound.is_error ())
	{
	  auto trait_item_ref
	    = TypeCheckImplItemWithTrait::Resolve (impl_block, *impl_item, self,
						   specified_bound,
						   substitutions);
	  if (!trait_item_ref.is_error ())
	    trait_item_refs.push_back (trait_item_ref.get_raw_item ());
	}
    }

  bool impl_block_missing_trait_items
    = !specified_bound.is_error ()
      && trait_reference->size () != trait_item_refs.size ();
  if (impl_block_missing_trait_items
      && impl_block.get_polarity () == BoundPolarity::RegularBound)
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
	      found = trait_item_name == impl_item_name;
	      if (found)
		break;
	    }

	  bool is_required_trait_item = !trait_item_ref.is_optional ();
	  if (!found && is_required_trait_item)
	    missing_trait_items.emplace_back (trait_item_ref);
	}

      if (!missing_trait_items.empty ())
	{
	  std::string missing_items_buf;
	  rich_location r (line_table, impl_block.get_locus ());
	  for (size_t i = 0; i < missing_trait_items.size (); i++)
	    {
	      bool has_more = (i + 1) < missing_trait_items.size ();
	      const TraitItemReference &missing_trait_item
		= missing_trait_items.at (i);
	      missing_items_buf += missing_trait_item.get_identifier ()
				   + (has_more ? ", " : "");
	      r.add_range (missing_trait_item.get_locus ());
	    }

	  rust_error_at (r, ErrorCode::E0046,
			 "missing %s in implementation of trait %qs",
			 missing_items_buf.c_str (),
			 trait_reference->get_name ().c_str ());
	}
    }

  if (is_trait_impl_block)
    {
      trait_reference->clear_associated_types ();

      AssociatedImplTrait associated (trait_reference, specified_bound,
				      &impl_block, self, context);
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
  return TypeCheckType::Resolve (impl_block.get_type ());
}

} // namespace Resolver
} // namespace Rust
