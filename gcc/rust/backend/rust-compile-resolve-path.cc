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

#include "rust-linemap.h"
#include "rust-backend.h"
#include "rust-compile-resolve-path.h"
#include "rust-compile-item.h"
#include "rust-hir-trait-resolve.h"
#include "rust-hir-path-probe.h"

namespace Rust {
namespace Compile {

void
ResolvePathRef::visit (HIR::QualifiedPathInExpression &expr)
{
  resolve (expr.get_final_segment ().get_segment (), expr.get_mappings (),
	   expr.get_locus (), true);
}

void
ResolvePathRef::visit (HIR::PathInExpression &expr)
{
  resolve (expr.get_final_segment ().get_segment (), expr.get_mappings (),
	   expr.get_locus (), false);
}

void
ResolvePathRef::resolve (const HIR::PathIdentSegment &final_segment,
			 const Analysis::NodeMapping &mappings,
			 Location expr_locus, bool is_qualified_path)
{
  // need to look up the reference for this identifier
  NodeId ref_node_id = UNKNOWN_NODEID;
  if (ctx->get_resolver ()->lookup_resolved_name (mappings.get_nodeid (),
						  &ref_node_id))
    {
      Resolver::Definition def;
      if (!ctx->get_resolver ()->lookup_definition (ref_node_id, &def))
	{
	  rust_error_at (expr_locus, "unknown reference for resolved name");
	  return;
	}
      ref_node_id = def.parent;
    }

  // this can fail because it might be a Constructor for something
  // in that case the caller should attempt ResolvePathType::Compile
  if (ref_node_id == UNKNOWN_NODEID)
    return;

  HirId ref;
  if (!ctx->get_mappings ()->lookup_node_to_hir (mappings.get_crate_num (),
						 ref_node_id, &ref))
    {
      rust_error_at (expr_locus, "reverse call path lookup failure");
      return;
    }

  // might be a constant
  if (ctx->lookup_const_decl (ref, &resolved))
    return;

  // this might be a variable reference or a function reference
  Bvariable *var = nullptr;
  if (ctx->lookup_var_decl (ref, &var))
    {
      resolved = ctx->get_backend ()->var_expression (var, expr_locus);
      return;
    }

  // must be a function call but it might be a generic function which needs to
  // be compiled first
  TyTy::BaseType *lookup = nullptr;
  bool ok = ctx->get_tyctx ()->lookup_type (mappings.get_hirid (), &lookup);
  rust_assert (ok);
  if (lookup->get_kind () == TyTy::TypeKind::FNDEF)
    {
      TyTy::FnType *fntype = static_cast<TyTy::FnType *> (lookup);

      Bfunction *fn = nullptr;
      if (ctx->lookup_function_decl (fntype->get_ty_ref (), &fn))
	{
	  resolved
	    = ctx->get_backend ()->function_code_expression (fn, expr_locus);
	}
      else
	{
	  resolved
	    = query_compile_function (ref, fntype, final_segment, mappings,
				      expr_locus, is_qualified_path);
	}
    }
  else
    {
      HIR::Item *resolved_item
	= ctx->get_mappings ()->lookup_hir_item (mappings.get_crate_num (),
						 ref);
      HirId parent_impl_id = UNKNOWN_HIRID;
      HIR::ImplItem *resolved_impl_item
	= ctx->get_mappings ()->lookup_hir_implitem (mappings.get_crate_num (),
						     ref, &parent_impl_id);
      bool is_impl_item = resolved_impl_item != nullptr;
      bool is_item = resolved_item != nullptr;

      gcc_unreachable ();
    }
}

Bexpression *
ResolvePathRef::query_compile_function (
  HirId ref, TyTy::FnType *fntype, const HIR::PathIdentSegment &final_segment,
  const Analysis::NodeMapping &mappings, Location expr_locus,
  bool is_qualified_path)
{
  HIR::Item *resolved_item
    = ctx->get_mappings ()->lookup_hir_item (mappings.get_crate_num (), ref);
  bool is_hir_item = resolved_item != nullptr;
  if (is_hir_item)
    {
      if (!fntype->has_subsititions_defined ())
	CompileItem::compile (resolved_item, ctx);
      else
	CompileItem::compile (resolved_item, ctx, true, fntype);
    }
  else
    {
      HirId parent_impl_id = UNKNOWN_HIRID;
      HIR::ImplItem *resolved_item
	= ctx->get_mappings ()->lookup_hir_implitem (mappings.get_crate_num (),
						     ref, &parent_impl_id);
      bool is_impl_item = resolved_item != nullptr;
      if (is_impl_item)
	{
	  rust_assert (parent_impl_id != UNKNOWN_HIRID);
	  HIR::Item *impl_ref
	    = ctx->get_mappings ()->lookup_hir_item (mappings.get_crate_num (),
						     parent_impl_id);
	  rust_assert (impl_ref != nullptr);
	  HIR::ImplBlock *impl = static_cast<HIR::ImplBlock *> (impl_ref);

	  TyTy::BaseType *self = nullptr;
	  bool ok = ctx->get_tyctx ()->lookup_type (
	    impl->get_type ()->get_mappings ().get_hirid (), &self);
	  rust_assert (ok);

	  if (!fntype->has_subsititions_defined ())
	    CompileInherentImplItem::Compile (self, resolved_item, ctx, true);
	  else
	    CompileInherentImplItem::Compile (self, resolved_item, ctx, true,
					      fntype);
	}
      else
	{
	  // it might be resolved to a trait item
	  HIR::TraitItem *trait_item
	    = ctx->get_mappings ()->lookup_hir_trait_item (
	      mappings.get_crate_num (), ref);
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

	  if (receiver->get_kind () == TyTy::TypeKind::PARAM)
	    {
	      TyTy::ParamType *p = static_cast<TyTy::ParamType *> (receiver);
	      receiver = p->resolve ();
	    }

	  // the type resolver can only resolve type bounds to their trait
	  // item so its up to us to figure out if this path should resolve
	  // to an trait-impl-block-item or if it can be defaulted to the
	  // trait-impl-item's definition
	  std::vector<Resolver::PathProbeCandidate> candidates;
	  if (!is_qualified_path)
	    {
	      candidates
		= Resolver::PathProbeType::Probe (receiver, final_segment, true,
						  false, true);
	    }

	  if (candidates.size () == 0)
	    {
	      // this means we are defaulting back to the trait_item if
	      // possible
	      Resolver::TraitItemReference *trait_item_ref = nullptr;
	      bool ok = trait_ref->lookup_hir_trait_item (*trait_item,
							  &trait_item_ref);
	      rust_assert (ok);				    // found
	      rust_assert (trait_item_ref->is_optional ()); // has definition

	      Analysis::NodeMapping trait_mappings
		= trait_item_ref->get_parent_trait_mappings ();
	      auto associated_impl_id
		= ctx->get_tyctx ()->lookup_associated_impl_mapping_for_self (
		  trait_mappings.get_hirid (), receiver);

	      rust_assert (associated_impl_id != UNKNOWN_HIRID);

	      Resolver::AssociatedImplTrait *associated = nullptr;
	      bool found_associated_trait_impl
		= ctx->get_tyctx ()->lookup_associated_trait_impl (
		  associated_impl_id, &associated);
	      rust_assert (found_associated_trait_impl);
	      associated->setup_associated_types ();

	      CompileTraitItem::Compile (receiver,
					 trait_item_ref->get_hir_trait_item (),
					 ctx, fntype);
	    }
	  else
	    {
	      Resolver::PathProbeCandidate &candidate = candidates.at (0);
	      rust_assert (candidate.is_impl_candidate ());

	      HIR::ImplBlock *impl = candidate.item.impl.parent;
	      HIR::ImplItem *impl_item = candidate.item.impl.impl_item;

	      TyTy::BaseType *self = nullptr;
	      bool ok = ctx->get_tyctx ()->lookup_type (
		impl->get_type ()->get_mappings ().get_hirid (), &self);
	      rust_assert (ok);

	      if (!fntype->has_subsititions_defined ())
		CompileInherentImplItem::Compile (self, impl_item, ctx, true);
	      else
		CompileInherentImplItem::Compile (self, impl_item, ctx, true,
						  fntype);

	      fntype->set_ty_ref (impl_item->get_impl_mappings ().get_hirid ());
	    }
	}
    }

  Bfunction *fn;
  if (!ctx->lookup_function_decl (fntype->get_ty_ref (), &fn))
    {
      rust_error_at (expr_locus, "forward declaration was not compiled");
      return ctx->get_backend ()->error_expression ();
    }

  return ctx->get_backend ()->function_code_expression (fn, expr_locus);
}

} // namespace Compile
} // namespace Rust
