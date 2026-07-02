// Copyright (C) 2026 Free Software Foundation, Inc.

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

#include "rust-compile-drop.h"
#include "rust-compile-drop-builder.h"
#include "rust-compile-base.h"
#include "rust-compile-context.h"
#include "rust-compile-implitem.h"
#include "rust-hir-path-probe.h"
#include "rust-hir-trait-reference.h"
#include "rust-hir-type-bounds.h"
#include "rust-lang-item.h"
#include "rust-tyty.h"

namespace Rust {
namespace Compile {

CompileDrop::CompileDrop (Context *ctx) : ctx (ctx) {}

bool
CompileDrop::type_has_drop_impl (TyTy::BaseType *ty)
{
  auto drop_lang_item
    = ctx->get_mappings ().lookup_lang_item (LangItem::Kind::DROP);

  if (!drop_lang_item.has_value ())
    return false;

  DefId drop_id = drop_lang_item.value ();

  auto candidates = Resolver::TypeBoundsProbe::Probe (ty);
  for (auto &candidate : candidates)
    {
      Resolver::TraitReference *trait_ref = candidate.first;
      if (trait_ref != nullptr && trait_ref->get_defid () == drop_id)
	return true;
    }

  return false;
}

// Find the Drop trait, look for the drop method, and build the function call.
tree
CompileDrop::compile_drop_call (Bvariable *var, TyTy::BaseType *ty,
				location_t locus)
{
  auto drop_lang = ctx->get_mappings ().lookup_lang_item (LangItem::Kind::DROP);
  if (!drop_lang.has_value ())
    return NULL_TREE;

  Resolver::TraitReference *drop_ref = nullptr;
  bool ok
    = ctx->get_tyctx ()->lookup_trait_reference (drop_lang.value (), &drop_ref);
  if (!ok)
    return NULL_TREE;

  HIR::PathIdentSegment segment ("drop");
  auto candidates
    = Resolver::PathProbeImplTrait::Probe (ty->get_root (), segment, drop_ref);

  rust_assert (candidates.size () == 1);

  auto &candidate = *candidates.begin ();
  rust_assert (candidate.is_impl_candidate ());
  rust_assert (candidate.ty->get_kind () == TyTy::TypeKind::FNDEF);

  auto *fn_type = static_cast<TyTy::FnType *> (candidate.ty);
  tree fn_addr
    = CompileInherentImplItem::Compile (candidate.item.impl.impl_item, ctx,
					fn_type, locus);

  tree var_expr = Backend::var_expression (var, locus);
  tree var_addr = HIRCompileBase::address_expression (var_expr, locus);

  return Backend::call_expression (fn_addr, {var_addr}, nullptr, locus);
}

tree
CompileDrop::build_current_scope_drop_cleanup ()
{
  std::vector<tree> drop_stmts;

  DropBuilder drop_builder (*ctx);
  auto &drop_candidates = drop_builder.peek_block_drop_candidates ();

  for (auto it = drop_candidates.rbegin (); it != drop_candidates.rend (); ++it)
    {
      TyTy::BaseType *ty = nullptr;
      Bvariable *var = nullptr;

      bool ok = ctx->get_tyctx ()->lookup_type (it->hirid, &ty);
      rust_assert (ok);

      ok = ctx->lookup_var_decl (it->hirid, &var);
      rust_assert (ok);

      tree drop_call = compile_drop_call (var, ty, it->locus);
      if (drop_call != NULL_TREE)
	drop_stmts.push_back (convert_to_void (drop_call, ICV_STATEMENT));
    }

  if (drop_stmts.empty ())
    return NULL_TREE;

  return Backend::statement_list (drop_stmts);
}

void
CompileDrop::emit_current_scope_drop_calls ()
{
  tree cleanup = build_current_scope_drop_cleanup ();
  if (cleanup != NULL_TREE)
    ctx->add_statement (cleanup);
}

} // namespace Compile
} // namespace Rust
