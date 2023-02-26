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

#include "rust-type-util.h"
#include "rust-diagnostics.h"
#include "rust-hir-map.h"
#include "rust-hir-type-check-implitem.h"
#include "rust-hir-type-check-item.h"
#include "rust-hir-type-check.h"
#include "rust-hir-visitor.h"
#include "rust-name-resolver.h"
#include "rust-casts.h"
#include "rust-unify.h"
#include "rust-coercion.h"

namespace Rust {
namespace Resolver {

bool
query_type (HirId reference, TyTy::BaseType **result)
{
  Analysis::Mappings *mappings = Analysis::Mappings::get ();
  TypeCheckContext *context = TypeCheckContext::get ();

  if (context->query_in_progress (reference))
    return false;

  if (context->lookup_type (reference, result))
    return true;

  context->insert_query (reference);

  HIR::Item *item = mappings->lookup_hir_item (reference);
  if (item != nullptr)
    {
      rust_debug_loc (item->get_locus (), "resolved item {%u} to", reference);
      *result = TypeCheckItem::Resolve (*item);
      context->query_completed (reference);
      return true;
    }

  HirId parent_impl_id = UNKNOWN_HIRID;
  HIR::ImplItem *impl_item
    = mappings->lookup_hir_implitem (reference, &parent_impl_id);
  if (impl_item != nullptr)
    {
      HIR::ImplBlock *impl_block
	= mappings->lookup_hir_impl_block (parent_impl_id);
      rust_assert (impl_block != nullptr);

      // found an impl item
      rust_debug_loc (impl_item->get_locus (), "resolved impl-item {%u} to",
		      reference);

      *result = TypeCheckItem::ResolveImplItem (*impl_block, *impl_item);
      context->query_completed (reference);
      return true;
    }

  // is it an impl_type?
  HIR::ImplBlock *impl_block_by_type = nullptr;
  bool found_impl_block_type
    = mappings->lookup_impl_block_type (reference, &impl_block_by_type);
  if (found_impl_block_type)
    {
      *result = TypeCheckItem::ResolveImplBlockSelf (*impl_block_by_type);
      context->query_completed (reference);
      return true;
    }

  // is it an extern item?
  HirId parent_extern_block_id = UNKNOWN_HIRID;
  HIR::ExternalItem *extern_item
    = mappings->lookup_hir_extern_item (reference, &parent_extern_block_id);
  if (extern_item != nullptr)
    {
      HIR::ExternBlock *block
	= mappings->lookup_hir_extern_block (parent_extern_block_id);
      rust_assert (block != nullptr);

      *result = TypeCheckTopLevelExternItem::Resolve (extern_item, *block);
      context->query_completed (reference);
      return true;
    }

  // more?
  Location possible_locus = mappings->lookup_location (reference);
  rust_debug_loc (possible_locus, "query system failed to resolve: [%u]",
		  reference);
  context->query_completed (reference);

  return false;
}

TyTy::BaseType *
unify_site (HirId id, TyTy::TyWithLocation lhs, TyTy::TyWithLocation rhs,
	    Location unify_locus)
{
  TyTy::BaseType *expected = lhs.get_ty ();
  TyTy::BaseType *expr = rhs.get_ty ();

  rust_debug ("unify_site id={%u} expected={%s} expr={%s}", id,
	      expected->debug_str ().c_str (), expr->debug_str ().c_str ());

  std::vector<UnifyRules::CommitSite> commits;
  std::vector<UnifyRules::InferenceSite> infers;
  return UnifyRules::Resolve (lhs, rhs, unify_locus, true /*commit*/,
			      true /*emit_error*/, false /*infer*/, commits,
			      infers);
}

TyTy::BaseType *
unify_site_and (HirId id, TyTy::TyWithLocation lhs, TyTy::TyWithLocation rhs,
		Location unify_locus, bool emit_errors, bool commit_if_ok,
		bool implicit_infer_vars, bool cleanup)
{
  TypeCheckContext &context = *TypeCheckContext::get ();

  TyTy::BaseType *expected = lhs.get_ty ();
  TyTy::BaseType *expr = rhs.get_ty ();

  rust_debug (
    "unify_site_and commit %s infer %s id={%u} expected={%s} expr={%s}",
    commit_if_ok ? "true" : "false", implicit_infer_vars ? "true" : "false", id,
    expected->debug_str ().c_str (), expr->debug_str ().c_str ());

  std::vector<UnifyRules::CommitSite> commits;
  std::vector<UnifyRules::InferenceSite> infers;
  TyTy::BaseType *result
    = UnifyRules::Resolve (lhs, rhs, unify_locus, false /*commit inline*/,
			   emit_errors, implicit_infer_vars, commits, infers);
  bool ok = result->get_kind () != TyTy::TypeKind::ERROR;
  if (ok && commit_if_ok)
    {
      for (auto &c : commits)
	{
	  UnifyRules::commit (c.lhs, c.rhs, c.resolved);
	}
    }
  else if (cleanup)
    {
      // FIXME
      // reset the get_next_hir_id

      for (auto &i : infers)
	{
	  i.param->set_ref (i.pref);
	  i.param->set_ty_ref (i.ptyref);

	  // remove the inference variable
	  context.clear_type (i.infer);
	  delete i.infer;
	}
    }
  return result;
}

TyTy::BaseType *
coercion_site (HirId id, TyTy::TyWithLocation lhs, TyTy::TyWithLocation rhs,
	       Location locus)
{
  TyTy::BaseType *expected = lhs.get_ty ();
  TyTy::BaseType *expr = rhs.get_ty ();

  rust_debug ("coercion_site id={%u} expected={%s} expr={%s}", id,
	      expected->debug_str ().c_str (), expr->debug_str ().c_str ());

  auto context = TypeCheckContext::get ();
  if (expected->get_kind () == TyTy::TypeKind::ERROR
      || expr->get_kind () == TyTy::TypeKind::ERROR)
    return expr;

  // can we autoderef it?
  auto result = TypeCoercionRules::Coerce (expr, expected, locus);

  // the result needs to be unified
  TyTy::BaseType *receiver = expr;
  if (!result.is_error ())
    {
      receiver = result.tyty;
    }

  rust_debug ("coerce_default_unify(a={%s}, b={%s})",
	      receiver->debug_str ().c_str (), expected->debug_str ().c_str ());
  TyTy::BaseType *coerced
    = unify_site (id, lhs, TyTy::TyWithLocation (receiver, rhs.get_locus ()),
		  locus);
  context->insert_autoderef_mappings (id, std::move (result.adjustments));
  return coerced;
}

TyTy::BaseType *
cast_site (HirId id, TyTy::TyWithLocation from, TyTy::TyWithLocation to,
	   Location cast_locus)
{
  rust_debug ("cast_site id={%u} from={%s} to={%s}", id,
	      from.get_ty ()->debug_str ().c_str (),
	      to.get_ty ()->debug_str ().c_str ());

  auto context = TypeCheckContext::get ();
  if (from.get_ty ()->get_kind () == TyTy::TypeKind::ERROR
      || to.get_ty ()->get_kind () == TyTy::TypeKind::ERROR)
    return to.get_ty ();

  // do the cast
  auto result = TypeCastRules::resolve (cast_locus, from, to);

  // we assume error has already been emitted
  if (result.is_error ())
    return to.get_ty ();

  // the result needs to be unified
  TyTy::BaseType *casted_result = result.tyty;
  rust_debug ("cast_default_unify(a={%s}, b={%s})",
	      casted_result->debug_str ().c_str (),
	      to.get_ty ()->debug_str ().c_str ());

  TyTy::BaseType *casted
    = unify_site (id, to,
		  TyTy::TyWithLocation (casted_result, from.get_locus ()),
		  cast_locus);
  context->insert_cast_autoderef_mappings (id, std::move (result.adjustments));
  return casted;
}

} // namespace Resolver
} // namespace Rust
