// Copyright (C) 2021-2025 Free Software Foundation, Inc.

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

// The idea is that all reachable symbols are live, codes called
// from live codes are live, and everything else is dead.

#include "rust-lint-marklive.h"
#include "options.h"
#include "rust-hir-full.h"
#include "rust-hir-map.h"
#include "rust-hir-path.h"
#include "rust-name-resolver.h"
#include "rust-immutable-name-resolution-context.h"
#include "rust-system.h"

namespace Rust {
namespace Analysis {

// This class trys to find the live symbols which can be used as
// seeds in MarkLive
//
// 1. TODO: explicit live
//    - Attribute like #[allow(dead_code)]
//    - Attribute like #[lang=".."], it's not a intra-crate item.
// 2. TODO: foreign item
class FindEntryPoint : public MarkLiveBase
{
  using Rust::Analysis::MarkLiveBase::visit;

public:
  static std::vector<HirId> find (HIR::Crate &crate)
  {
    FindEntryPoint findEntryPoint;
    for (auto &it : crate.get_items ())
      it->accept_vis (findEntryPoint);
    return findEntryPoint.getEntryPoint ();
  }

  // TODO not only fn main can be a entry point.
  void visit (HIR::Function &function) override
  {
    if (function.get_function_name ().as_string () == "main")
      {
	entryPoints.push_back (function.get_mappings ().get_hirid ());
      }
  }

private:
  FindEntryPoint () : MarkLiveBase () {}
  std::vector<HirId> entryPoints;
  std::vector<HirId> getEntryPoint () { return entryPoints; }
};

std::set<HirId>
MarkLive::Analysis (HIR::Crate &crate)
{
  MarkLive marklive (FindEntryPoint::find (crate));
  marklive.go (crate);

  return marklive.liveSymbols;
}

// pop a live symbol from worklist every iteration,
// if it's a function then walk the function body, and
// 1. save all the live symbols in worklist which is
//    visited first time
// 2. save all the live symbols in liveSymbols
void
MarkLive::go (HIR::Crate &)
{
  while (!worklist.empty ())
    {
      HirId hirId = worklist.back ();
      worklist.pop_back ();
      scannedSymbols.emplace (hirId);
      liveSymbols.emplace (hirId);
      if (auto item = mappings.lookup_hir_item (hirId))
	item.value ()->accept_vis (*this);
      else if (auto implItem = mappings.lookup_hir_implitem (hirId))
	implItem->first->accept_vis (*this);
    }
}

void
MarkLive::visit (HIR::PathInExpression &expr)
{
  // We should iterate every path segment in order to mark the struct which
  // is used in expression like Foo::bar(), we should mark the Foo alive.
  if (!expr.is_lang_item ())
    expr.iterate_path_segments ([&] (HIR::PathExprSegment &seg) -> bool {
      return visit_path_segment (seg);
    });

  // after iterate the path segments, we should mark functions and associated
  // functions alive.
  NodeId ast_node_id = expr.get_mappings ().get_nodeid ();
  NodeId ref_node_id = UNKNOWN_NODEID;

  if (expr.is_lang_item ())
    ref_node_id
      = Analysis::Mappings::get ().get_lang_item_node (expr.get_lang_item ());
  else
    find_ref_node_id (ast_node_id, ref_node_id);

  // node back to HIR
  tl::optional<HirId> hid = mappings.lookup_node_to_hir (ref_node_id);
  rust_assert (hid.has_value ());
  auto ref = hid.value ();

  // it must resolve to some kind of HIR::Item or HIR::InheritImplItem
  if (auto resolved_item = mappings.lookup_hir_item (ref))
    mark_hir_id (resolved_item.value ()->get_mappings ().get_hirid ());
  else if (auto resolved_item = mappings.lookup_hir_implitem (ref))
    mark_hir_id (resolved_item->first->get_impl_mappings ().get_hirid ());
}

void
MarkLive::visit (HIR::MethodCallExpr &expr)
{
  expr.get_receiver ().accept_vis (*this);
  visit_path_segment (expr.get_method_name ());
  for (auto &argument : expr.get_arguments ())
    argument->accept_vis (*this);

  // Trying to find the method definition and mark it alive.
  NodeId ast_node_id = expr.get_mappings ().get_nodeid ();
  NodeId ref_node_id = UNKNOWN_NODEID;
  find_ref_node_id (ast_node_id, ref_node_id);

  // node back to HIR
  if (auto hid = mappings.lookup_node_to_hir (ref_node_id))
    mark_hir_id (*hid);
  else
    rust_unreachable ();
}

bool
MarkLive::visit_path_segment (HIR::PathExprSegment seg)
{
  NodeId ast_node_id = seg.get_mappings ().get_nodeid ();
  NodeId ref_node_id = UNKNOWN_NODEID;

  // There are two different kinds of segment for us.
  // 1. function segment
  //      like the symbol "foo" in expression `foo()`.
  // 2. type segment
  //      like the symbol "Foo" in expression `Foo{a: 1, b: 2}`
  //
  // We should mark them alive all and ignoring other kind of segments.
  // If the segment we dont care then just return false is fine
  if (flag_name_resolution_2_0)
    {
      auto &nr_ctx
	= Resolver2_0::ImmutableNameResolutionContext::get ().resolver ();

      if (auto id = nr_ctx.lookup (ast_node_id))
	ref_node_id = *id;
      else
	return false;
    }
  else if (!resolver->lookup_resolved_name (ast_node_id, &ref_node_id))
    {
      if (!resolver->lookup_resolved_type (ast_node_id, &ref_node_id))
	return false;
    }
  if (auto hid = mappings.lookup_node_to_hir (ref_node_id))
    {
      mark_hir_id (*hid);
      return true;
    }
  rust_unreachable ();
}

void
MarkLive::visit (HIR::FieldAccessExpr &expr)
{
  // visit receiver at first
  expr.get_receiver_expr ().accept_vis (*this);

  // resolve the receiver back to ADT type
  TyTy::BaseType *receiver = nullptr;
  if (!tyctx->lookup_type (
	expr.get_receiver_expr ().get_mappings ().get_hirid (), &receiver))
    {
      rust_error_at (expr.get_receiver_expr ().get_locus (),
		     "unresolved type for receiver");
    }

  TyTy::ADTType *adt = nullptr;
  if (receiver->get_kind () == TyTy::TypeKind::ADT)
    {
      adt = static_cast<TyTy::ADTType *> (receiver);
    }
  else if (receiver->get_kind () == TyTy::TypeKind::REF)
    {
      TyTy::ReferenceType *r = static_cast<TyTy::ReferenceType *> (receiver);
      TyTy::BaseType *b = r->get_base ();
      rust_assert (b->get_kind () == TyTy::TypeKind::ADT);

      adt = static_cast<TyTy::ADTType *> (b);
    }

  rust_assert (adt != nullptr);
  rust_assert (!adt->is_enum ());
  rust_assert (adt->number_of_variants () == 1);

  TyTy::VariantDef *variant = adt->get_variants ().at (0);

  // get the field index
  size_t index;
  TyTy::StructFieldType *field;
  bool ok = variant->lookup_field (expr.get_field_name ().as_string (), &field,
				   &index);
  rust_assert (ok);
  if (index >= variant->num_fields ())
    {
      rust_error_at (expr.get_receiver_expr ().get_locus (),
		     "cannot access struct %s by index: %lu",
		     adt->get_name ().c_str (), (unsigned long) index);
      return;
    }

  // get the field hir id
  HirId field_id = field->get_ref ();
  mark_hir_id (field_id);
}

void
MarkLive::visit (HIR::TupleIndexExpr &expr)
{
  // TODO: unused tuple field detection
  expr.get_tuple_expr ().accept_vis (*this);
}

void
MarkLive::visit (HIR::TypeAlias &alias)
{
  NodeId ast_node_id = UNKNOWN_NODEID;
  if (flag_name_resolution_2_0)
    {
      auto &nr_ctx
	= Resolver2_0::ImmutableNameResolutionContext::get ().resolver ();

      if (auto id = nr_ctx.lookup (
	    alias.get_type_aliased ().get_mappings ().get_nodeid ()))
	ast_node_id = *id;
    }
  else
    {
      resolver->lookup_resolved_type (
	alias.get_type_aliased ().get_mappings ().get_nodeid (), &ast_node_id);
    }

  if (auto hid = mappings.lookup_node_to_hir (ast_node_id))
    mark_hir_id (*hid);
  else
    rust_unreachable ();
}

void
MarkLive::mark_hir_id (HirId id)
{
  if (scannedSymbols.find (id) == scannedSymbols.end ())
    {
      worklist.push_back (id);
    }
  liveSymbols.emplace (id);
}

void
MarkLive::find_ref_node_id (NodeId ast_node_id, NodeId &ref_node_id)
{
  if (flag_name_resolution_2_0)
    {
      auto &nr_ctx
	= Resolver2_0::ImmutableNameResolutionContext::get ().resolver ();

      nr_ctx.lookup (ast_node_id).map ([&ref_node_id] (NodeId resolved) {
	ref_node_id = resolved;
      });
    }
  else
    {
      if (!resolver->lookup_resolved_name (ast_node_id, &ref_node_id))
	{
	  if (!resolver->lookup_resolved_type (ast_node_id, &ref_node_id))
	    {
	      bool ok
		= resolver->lookup_resolved_misc (ast_node_id, &ref_node_id);
	      rust_assert (ok);
	    }
	}
    }
}

} // namespace Analysis
} // namespace Rust
