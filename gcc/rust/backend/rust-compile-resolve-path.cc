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

#include "rust-compile-resolve-path.h"
#include "options.h"
#include "rust-compile-intrinsic.h"
#include "rust-compile-item.h"
#include "rust-compile-implitem.h"
#include "rust-compile-expr.h"
#include "rust-hir-map.h"
#include "rust-hir-trait-resolve.h"
#include "rust-hir-path-probe.h"
#include "rust-compile-extern.h"
#include "rust-constexpr.h"
#include "rust-tyty.h"

namespace Rust {
namespace Compile {

tree
ResolvePathRef::Compile (HIR::QualifiedPathInExpression &expr, Context *ctx)
{
  ResolvePathRef resolver (ctx);
  return resolver.resolve_path_like (expr);
}

tree
ResolvePathRef::Compile (HIR::PathInExpression &expr, Context *ctx)
{
  ResolvePathRef resolver (ctx);
  return resolver.resolve_path_like (expr);
}

ResolvePathRef::ResolvePathRef (Context *ctx) : HIRCompileBase (ctx) {}

template <typename T>
tree
ResolvePathRef::resolve_path_like (T &expr)
{
  if (expr.is_lang_item ())
    {
      auto lang_item
	= Analysis::Mappings::get ().get_lang_item_node (expr.get_lang_item ());

      // FIXME: Is that correct? :/
      auto final_segment
	= HIR::PathIdentSegment (LangItem::ToString (expr.get_lang_item ()));

      return resolve_with_node_id (final_segment, expr.get_mappings (),
				   expr.get_locus (), true, lang_item);
    }

  return resolve (expr.get_final_segment ().get_segment (),
		  expr.get_mappings (), expr.get_locus (), true);
}

tree
ResolvePathRef::attempt_constructor_expression_lookup (
  TyTy::BaseType *lookup, Context *ctx, const Analysis::NodeMapping &mappings,
  location_t expr_locus)
{
  // it might be an enum data-less enum variant
  if (lookup->get_kind () != TyTy::TypeKind::ADT)
    return error_mark_node;

  TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (lookup);
  if (adt->is_unit ())
    return unit_expression (expr_locus);

  if (!adt->is_enum ())
    return error_mark_node;

  HirId variant_id;
  if (!ctx->get_tyctx ()->lookup_variant_definition (mappings.get_hirid (),
						     &variant_id))
    return error_mark_node;

  int union_disriminator = -1;
  TyTy::VariantDef *variant = nullptr;
  if (!adt->lookup_variant_by_id (variant_id, &variant, &union_disriminator))
    return error_mark_node;

  // this can only be for discriminant variants the others are built up
  // using call-expr or struct-init
  rust_assert (variant->get_variant_type ()
	       == TyTy::VariantDef::VariantType::NUM);

  // we need the actual gcc type
  tree compiled_adt_type = TyTyResolveCompile::compile (ctx, adt);

  // make the ctor for the union
  HIR::Expr &discrim_expr = variant->get_discriminant ();
  ctx->push_const_context ();
  tree discrim_expr_node = CompileExpr::Compile (discrim_expr, ctx);
  ctx->pop_const_context ();
  tree folded_discrim_expr = fold_expr (discrim_expr_node);
  tree qualifier = folded_discrim_expr;

  // false for is enum but this is an enum but we have a new layout
  return Backend::constructor_expression (compiled_adt_type, false, {qualifier},
					  -1, expr_locus);
}

tree
ResolvePathRef::resolve_with_node_id (
  const HIR::PathIdentSegment &final_segment,
  const Analysis::NodeMapping &mappings, location_t expr_locus,
  bool is_qualified_path, NodeId resolved_node_id)
{
  TyTy::BaseType *lookup = nullptr;
  bool ok = ctx->get_tyctx ()->lookup_type (mappings.get_hirid (), &lookup);
  rust_assert (ok);

  tl::optional<HirId> hid
    = ctx->get_mappings ().lookup_node_to_hir (resolved_node_id);
  if (!hid.has_value ())
    {
      rust_error_at (expr_locus, "reverse call path lookup failure");
      return error_mark_node;
    }
  auto ref = hid.value ();

  // might be a constant
  tree constant_expr;
  if (ctx->lookup_const_decl (ref, &constant_expr))
    {
      TREE_USED (constant_expr) = 1;
      return constant_expr;
    }

  // maybe closure binding
  tree closure_binding = error_mark_node;
  if (ctx->lookup_closure_binding (ref, &closure_binding))
    {
      TREE_USED (closure_binding) = 1;
      return closure_binding;
    }

  // this might be a variable reference or a function reference
  Bvariable *var = nullptr;
  if (ctx->lookup_var_decl (ref, &var))
    {
      // TREE_USED is setup in the gcc abstraction here
      return Backend::var_expression (var, expr_locus);
    }

  // might be a match pattern binding
  tree binding = error_mark_node;
  if (ctx->lookup_pattern_binding (ref, &binding))
    {
      TREE_USED (binding) = 1;
      return binding;
    }

  // it might be a function call
  if (lookup->get_kind () == TyTy::TypeKind::FNDEF)
    {
      TyTy::FnType *fntype = static_cast<TyTy::FnType *> (lookup);
      tree fn = NULL_TREE;
      if (ctx->lookup_function_decl (fntype->get_ty_ref (), &fn))
	{
	  TREE_USED (fn) = 1;
	  return address_expression (fn, expr_locus);
	}
      else if (fntype->get_abi () == ABI::INTRINSIC)
	{
	  Intrinsics compile (ctx);
	  fn = compile.compile (fntype);
	  TREE_USED (fn) = 1;
	  return address_expression (fn, expr_locus);
	}
    }

  // Handle unit struct
  if (lookup->get_kind () == TyTy::TypeKind::ADT)
    return attempt_constructor_expression_lookup (lookup, ctx, mappings,
						  expr_locus);

  // let the query system figure it out
  tree resolved_item = query_compile (ref, lookup, final_segment, mappings,
				      expr_locus, is_qualified_path);
  if (resolved_item != error_mark_node)
    {
      TREE_USED (resolved_item) = 1;
    }

  return resolved_item;
}

tree
ResolvePathRef::resolve (const HIR::PathIdentSegment &final_segment,
			 const Analysis::NodeMapping &mappings,
			 location_t expr_locus, bool is_qualified_path)
{
  TyTy::BaseType *lookup = nullptr;
  bool ok = ctx->get_tyctx ()->lookup_type (mappings.get_hirid (), &lookup);
  rust_assert (ok);

  // need to look up the reference for this identifier

  // this can fail because it might be a Constructor for something
  // in that case the caller should attempt ResolvePathType::Compile
  NodeId ref_node_id = UNKNOWN_NODEID;
  if (flag_name_resolution_2_0)
    {
      auto &nr_ctx
	= Resolver2_0::ImmutableNameResolutionContext::get ().resolver ();

      auto resolved = nr_ctx.lookup (mappings.get_nodeid ());

      if (!resolved)
	return attempt_constructor_expression_lookup (lookup, ctx, mappings,
						      expr_locus);

      ref_node_id = *resolved;
    }
  else
    {
      if (!ctx->get_resolver ()->lookup_resolved_name (mappings.get_nodeid (),
						       &ref_node_id))
	return attempt_constructor_expression_lookup (lookup, ctx, mappings,
						      expr_locus);
    }

  return resolve_with_node_id (final_segment, mappings, expr_locus,
			       is_qualified_path, ref_node_id);
}

tree
HIRCompileBase::query_compile (HirId ref, TyTy::BaseType *lookup,
			       const HIR::PathIdentSegment &final_segment,
			       const Analysis::NodeMapping &mappings,
			       location_t expr_locus, bool is_qualified_path)
{
  bool is_fn = lookup->get_kind () == TyTy::TypeKind::FNDEF;
  if (auto resolved_item = ctx->get_mappings ().lookup_hir_item (ref))
    {
      if (!lookup->has_substitutions_defined ())
	return CompileItem::compile (*resolved_item, ctx, nullptr, expr_locus);
      else
	return CompileItem::compile (*resolved_item, ctx, lookup, expr_locus);
    }
  else if (auto hir_extern_item
	   = ctx->get_mappings ().lookup_hir_extern_item (ref))
    {
      HIR::ExternalItem *resolved_extern_item = hir_extern_item->first;
      if (!lookup->has_substitutions_defined ())
	return CompileExternItem::compile (resolved_extern_item, ctx, nullptr,
					   expr_locus);
      else
	return CompileExternItem::compile (resolved_extern_item, ctx, lookup,
					   expr_locus);
    }
  else
    {
      if (is_fn)
	{
	  TyTy::FnType *fn = static_cast<TyTy::FnType *> (lookup);
	  TyTy::BaseType *receiver = nullptr;

	  if (fn->is_method ())
	    {
	      receiver = fn->get_self_type ();
	      receiver = receiver->destructure ();

	      return resolve_method_address (fn, receiver, expr_locus);
	    }
	}

      if (auto resolved_item = ctx->get_mappings ().lookup_hir_implitem (ref))
	{
	  if (!lookup->has_substitutions_defined ())
	    return CompileInherentImplItem::Compile (resolved_item->first, ctx,
						     nullptr, expr_locus);
	  else
	    return CompileInherentImplItem::Compile (resolved_item->first, ctx,
						     lookup, expr_locus);
	}
      else if (auto trait_item
	       = ctx->get_mappings ().lookup_hir_trait_item (ref))
	{
	  HIR::Trait *trait = ctx->get_mappings ().lookup_trait_item_mapping (
	    trait_item.value ()->get_mappings ().get_hirid ());

	  Resolver::TraitReference *trait_ref
	    = &Resolver::TraitReference::error_node ();
	  bool ok = ctx->get_tyctx ()->lookup_trait_reference (
	    trait->get_mappings ().get_defid (), &trait_ref);
	  rust_assert (ok);

	  if (trait_item.value ()->get_item_kind ()
	      == HIR::TraitItem::TraitItemKind::CONST)
	    {
	      auto &c
		= *static_cast<HIR::TraitItemConst *> (trait_item.value ());
	      if (!c.has_expr ())
		{
		  rich_location r (line_table, expr_locus);
		  r.add_range (trait->get_locus ());
		  r.add_range (c.get_locus ());
		  rust_error_at (r, "no default expression on trait constant");
		  return error_mark_node;
		}

	      return CompileExpr::Compile (c.get_expr (), ctx);
	    }

	  if (trait_item.value ()->get_item_kind ()
	      != HIR::TraitItem::TraitItemKind::FUNC)
	    return error_mark_node;

	  // the type resolver can only resolve type bounds to their trait
	  // item so its up to us to figure out if this path should resolve
	  // to an trait-impl-block-item or if it can be defaulted to the
	  // trait-impl-item's definition
	  //
	  // because we know this is resolved to a trait item we can actually
	  // just grab the Self type parameter here for the receiver to match
	  // the appropriate impl block

	  rust_assert (lookup->is<TyTy::FnType> ());
	  auto fn = lookup->as<TyTy::FnType> ();
	  rust_assert (fn->get_num_type_params () > 0);
	  auto &self = fn->get_substs ().at (0);
	  auto receiver = self.get_param_ty ();
	  auto candidates
	    = Resolver::PathProbeImplTrait::Probe (receiver, final_segment,
						   trait_ref);
	  if (candidates.size () == 0)
	    {
	      // this means we are defaulting back to the trait_item if
	      // possible
	      Resolver::TraitItemReference *trait_item_ref = nullptr;
	      bool ok = trait_ref->lookup_hir_trait_item (*trait_item.value (),
							  &trait_item_ref);
	      rust_assert (ok);				    // found
	      rust_assert (trait_item_ref->is_optional ()); // has definition

	      return CompileTraitItem::Compile (
		trait_item_ref->get_hir_trait_item (), ctx, lookup, true,
		expr_locus);
	    }
	  else
	    {
	      rust_assert (candidates.size () == 1);

	      auto candidate = *candidates.begin ();
	      rust_assert (candidate.is_impl_candidate ());

	      HIR::ImplBlock *impl = candidate.item.impl.parent;
	      HIR::ImplItem *impl_item = candidate.item.impl.impl_item;

	      TyTy::BaseType *self = nullptr;
	      bool ok = ctx->get_tyctx ()->lookup_type (
		impl->get_type ().get_mappings ().get_hirid (), &self);
	      rust_assert (ok);

	      if (!lookup->has_substitutions_defined ())
		return CompileInherentImplItem::Compile (impl_item, ctx,
							 nullptr, expr_locus);
	      else
		return CompileInherentImplItem::Compile (impl_item, ctx, lookup,
							 expr_locus);
	    }
	}
    }

  return error_mark_node;
}

} // namespace Compile
} // namespace Rust
