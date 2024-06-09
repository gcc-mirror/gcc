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

#include "rust-compile-resolve-path.h"
#include "rust-compile-intrinsic.h"
#include "rust-compile-item.h"
#include "rust-compile-implitem.h"
#include "rust-compile-expr.h"
#include "rust-hir-trait-resolve.h"
#include "rust-hir-path-probe.h"
#include "rust-compile-extern.h"
#include "rust-constexpr.h"

namespace Rust {
namespace Compile {

void
ResolvePathRef::visit (HIR::QualifiedPathInExpression &expr)
{
  resolved = resolve (expr.get_final_segment ().get_segment (),
		      expr.get_mappings (), expr.get_locus (), true);
}

void
ResolvePathRef::visit (HIR::PathInExpression &expr)
{
  resolved = resolve (expr.get_final_segment ().get_segment (),
		      expr.get_mappings (), expr.get_locus (), false);
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
  NodeId ref_node_id = UNKNOWN_NODEID;
  if (!ctx->get_resolver ()->lookup_resolved_name (mappings.get_nodeid (),
						   &ref_node_id))
    {
      // this can fail because it might be a Constructor for something
      // in that case the caller should attempt ResolvePathType::Compile

      // it might be an enum data-less enum variant
      if (lookup->get_kind () != TyTy::TypeKind::ADT)
	return error_mark_node;

      TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (lookup);
      if (adt->is_unit ())
	return unit_expression (ctx, expr_locus);

      if (!adt->is_enum ())
	return error_mark_node;

      HirId variant_id;
      if (!ctx->get_tyctx ()->lookup_variant_definition (mappings.get_hirid (),
							 &variant_id))
	return error_mark_node;

      int union_disriminator = -1;
      TyTy::VariantDef *variant = nullptr;
      if (!adt->lookup_variant_by_id (variant_id, &variant,
				      &union_disriminator))
	return error_mark_node;

      // this can only be for discriminant variants the others are built up
      // using call-expr or struct-init
      rust_assert (variant->get_variant_type ()
		   == TyTy::VariantDef::VariantType::NUM);

      // we need the actual gcc type
      tree compiled_adt_type = TyTyResolveCompile::compile (ctx, adt);

      // make the ctor for the union
      HIR::Expr *discrim_expr = variant->get_discriminant ();
      tree discrim_expr_node = CompileExpr::Compile (discrim_expr, ctx);
      tree folded_discrim_expr = fold_expr (discrim_expr_node);
      tree qualifier = folded_discrim_expr;

      return Backend::constructor_expression (compiled_adt_type, true,
					      {qualifier}, union_disriminator,
					      expr_locus);
    }

  HirId ref;
  if (!ctx->get_mappings ()->lookup_node_to_hir (ref_node_id, &ref))
    {
      rust_error_at (expr_locus, "reverse call path lookup failure");
      return error_mark_node;
    }

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
HIRCompileBase::query_compile (HirId ref, TyTy::BaseType *lookup,
			       const HIR::PathIdentSegment &final_segment,
			       const Analysis::NodeMapping &mappings,
			       location_t expr_locus, bool is_qualified_path)
{
  HIR::Item *resolved_item = ctx->get_mappings ()->lookup_hir_item (ref);
  HirId parent_block;
  HIR::ExternalItem *resolved_extern_item
    = ctx->get_mappings ()->lookup_hir_extern_item (ref, &parent_block);
  bool is_hir_item = resolved_item != nullptr;
  bool is_hir_extern_item = resolved_extern_item != nullptr;
  bool is_fn = lookup->get_kind () == TyTy::TypeKind::FNDEF;
  if (is_hir_item)
    {
      if (!lookup->has_substitutions_defined ())
	return CompileItem::compile (resolved_item, ctx, nullptr, true,
				     expr_locus);
      else
	return CompileItem::compile (resolved_item, ctx, lookup, true,
				     expr_locus);
    }
  else if (is_hir_extern_item)
    {
      if (!lookup->has_substitutions_defined ())
	return CompileExternItem::compile (resolved_extern_item, ctx, nullptr,
					   true, expr_locus);
      else
	return CompileExternItem::compile (resolved_extern_item, ctx, lookup,
					   true, expr_locus);
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

      HirId parent_impl_id = UNKNOWN_HIRID;
      HIR::ImplItem *resolved_item
	= ctx->get_mappings ()->lookup_hir_implitem (ref, &parent_impl_id);
      bool is_impl_item = resolved_item != nullptr;
      if (is_impl_item)
	{
	  if (!lookup->has_substitutions_defined ())
	    return CompileInherentImplItem::Compile (resolved_item, ctx,
						     nullptr, true, expr_locus);
	  else
	    return CompileInherentImplItem::Compile (resolved_item, ctx, lookup,
						     true, expr_locus);
	}
      else
	{
	  // it might be resolved to a trait item
	  HIR::TraitItem *trait_item
	    = ctx->get_mappings ()->lookup_hir_trait_item (ref);
	  HIR::Trait *trait = ctx->get_mappings ()->lookup_trait_item_mapping (
	    trait_item->get_mappings ().get_hirid ());

	  Resolver::TraitReference *trait_ref
	    = &Resolver::TraitReference::error_node ();
	  bool ok = ctx->get_tyctx ()->lookup_trait_reference (
	    trait->get_mappings ().get_defid (), &trait_ref);
	  rust_assert (ok);

	  TyTy::BaseType *receiver = nullptr;
	  ok = ctx->get_tyctx ()->lookup_receiver (mappings.get_hirid (),
						   &receiver);
	  rust_assert (ok);
	  receiver = receiver->destructure ();

	  // the type resolver can only resolve type bounds to their trait
	  // item so its up to us to figure out if this path should resolve
	  // to an trait-impl-block-item or if it can be defaulted to the
	  // trait-impl-item's definition
	  auto candidates
	    = Resolver::PathProbeImplTrait::Probe (receiver, final_segment,
						   trait_ref);
	  if (candidates.size () == 0)
	    {
	      // this means we are defaulting back to the trait_item if
	      // possible
	      Resolver::TraitItemReference *trait_item_ref = nullptr;
	      bool ok = trait_ref->lookup_hir_trait_item (*trait_item,
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
		impl->get_type ()->get_mappings ().get_hirid (), &self);
	      rust_assert (ok);

	      if (!lookup->has_substitutions_defined ())
		return CompileInherentImplItem::Compile (impl_item, ctx,
							 nullptr, true,
							 expr_locus);
	      else
		return CompileInherentImplItem::Compile (impl_item, ctx, lookup,
							 true, expr_locus);
	    }
	}
    }

  return error_mark_node;
}

} // namespace Compile
} // namespace Rust
