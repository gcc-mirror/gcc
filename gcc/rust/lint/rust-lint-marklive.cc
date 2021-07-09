// Copyright (C) 2021 Free Software Foundation, Inc.

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

#include "rust-lint-marklive.h"
#include "rust-hir-full.h"
#include "rust-name-resolver.h"

namespace Rust {
namespace Analysis {

class FindEntryPoint : public MarkLiveBase
{
  using Rust::Analysis::MarkLiveBase::visit;

public:
  static std::vector<HirId> find (HIR::Crate &crate)
  {
    FindEntryPoint findEntryPoint;
    for (auto it = crate.items.begin (); it != crate.items.end (); it++)
      {
	it->get ()->accept_vis (findEntryPoint);
      }
    return findEntryPoint.getEntryPoint ();
  }

  // TODO not only fn main can be a entry point.
  void visit (HIR::Function &function) override
  {
    if (function.get_function_name () == "main")
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

void
MarkLive::go (HIR::Crate &crate)
{
  CrateNum crateNum = crate.get_mappings ().get_crate_num ();
  while (!worklist.empty ())
    {
      HirId hirId = worklist.back ();
      worklist.pop_back ();
      scannedSymbols.emplace (hirId);
      HIR::Item *item = mappings->lookup_hir_item (crateNum, hirId);
      liveSymbols.emplace (hirId);
      if (item != nullptr)
	{
	  item->accept_vis (*this);
	}
      else
	{ // the item maybe inside a trait impl
	  HirId parent_impl_id = UNKNOWN_HIRID;
	  HIR::ImplItem *implItem
	    = mappings->lookup_hir_implitem (crateNum, hirId, &parent_impl_id);
	  if (implItem != nullptr)
	    implItem->accept_vis (*this);
	}
    }
}

void
MarkLive::visit (HIR::PathInExpression &expr)
{
  expr.iterate_path_segments ([&] (HIR::PathExprSegment &seg) -> bool {
    return visit_path_segment (seg);
  });
}

void
MarkLive::visit (HIR::MethodCallExpr &expr)
{
  expr.get_receiver ()->accept_vis (*this);
  visit_path_segment (expr.get_method_name ());
  expr.iterate_params ([&] (HIR::Expr *param) -> bool {
    param->accept_vis (*this);
    return true;
  });

  NodeId ast_node_id = expr.get_mappings ().get_nodeid ();
  NodeId ref_node_id = UNKNOWN_NODEID;
  find_ref_node_id (ast_node_id, ref_node_id, expr.get_locus (),
		    expr.as_string ());

  // node back to HIR
  HirId ref;
  bool ok = mappings->lookup_node_to_hir (expr.get_mappings ().get_crate_num (),
					  ref_node_id, &ref);
  rust_assert (ok);
  mark_hir_id (ref);
}

bool
MarkLive::visit_path_segment (HIR::PathExprSegment seg)
{
  NodeId ast_node_id = seg.get_mappings ().get_nodeid ();
  NodeId ref_node_id = UNKNOWN_NODEID;

  if (resolver->lookup_resolved_name (ast_node_id, &ref_node_id))
    {
      Resolver::Definition def;
      bool ret = resolver->lookup_definition (ref_node_id, &def);
      rust_assert (ret);
      ref_node_id = def.parent;
    }
  else if (!resolver->lookup_resolved_type (ast_node_id, &ref_node_id))
    {
      return false;
    }
  HirId ref;
  bool ok = mappings->lookup_node_to_hir (seg.get_mappings ().get_crate_num (),
					  ref_node_id, &ref);
  rust_assert (ok);
  mark_hir_id (ref);
  return true;
}

void
MarkLive::visit (HIR::FieldAccessExpr &expr)
{
  // visit receiver at first
  expr.get_receiver_expr ()->accept_vis (*this);

  // resolve the receiver back to ADT type
  TyTy::BaseType *receiver = nullptr;
  if (!tyctx->lookup_type (
	expr.get_receiver_expr ()->get_mappings ().get_hirid (), &receiver))
    {
      rust_error_at (expr.get_receiver_expr ()->get_locus_slow (),
		     "unresolved type for receiver");
    }
  bool ret = receiver->get_kind () == TyTy::TypeKind::ADT;
  rust_assert (ret);
  TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (receiver);

  // get the field index
  size_t index = 0;
  adt->get_field (expr.get_field_name (), &index);
  if (index >= adt->num_fields ())
    {
      rust_error_at (expr.get_receiver_expr ()->get_locus_slow (),
		     "cannot access struct %s by index: %ld",
		     adt->get_name ().c_str (), index);
      return;
    }
  // get the field hir id
  HirId field_id = adt->get_field (index)->get_ref ();
  mark_hir_id (field_id);
}

void
MarkLive::visit (HIR::TupleIndexExpr &expr)
{
  // TODO: unused tuple field detection
  expr.get_tuple_expr ()->accept_vis (*this);
}

void
MarkLive::visit (HIR::IdentifierExpr &expr)
{
  NodeId ast_node_id = expr.get_mappings ().get_nodeid ();
  NodeId ref_node_id = UNKNOWN_NODEID;
  find_ref_node_id (ast_node_id, ref_node_id, expr.get_locus (),
		    expr.as_string ());

  // node back to HIR
  HirId ref;
  bool ok = mappings->lookup_node_to_hir (expr.get_mappings ().get_crate_num (),
					  ref_node_id, &ref);
  rust_assert (ok);
  mark_hir_id (ref);
}

void
MarkLive::visit (HIR::TypeAlias &alias)
{
  NodeId ast_node_id;
  resolver->lookup_resolved_type (
    alias.get_type_aliased ()->get_mappings ().get_nodeid (), &ast_node_id);
  HirId hir_id;
  bool ok
    = mappings->lookup_node_to_hir (alias.get_mappings ().get_crate_num (),
				    ast_node_id, &hir_id);
  rust_assert (ok);
  mark_hir_id (hir_id);
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
MarkLive::find_ref_node_id (NodeId ast_node_id, NodeId &ref_node_id,
			    Location locus, const std::string &node_name)
{
  if (resolver->lookup_resolved_name (ast_node_id, &ref_node_id))
    {
      // these ref_node_ids will resolve to a pattern declaration but we are
      // interested in the definition that this refers to get the parent id
      Resolver::Definition def;
      bool ret = resolver->lookup_definition (ref_node_id, &def);
      rust_assert (ret);
      ref_node_id = def.parent;
    }
  else
    {
      bool ret = resolver->lookup_resolved_type (ast_node_id, &ref_node_id);
      rust_assert (ret);
    }
}

} // namespace Analysis
} // namespace Rust
