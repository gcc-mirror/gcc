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
  NodeId ast_node_id = expr.get_mappings ().get_nodeid ();
  NodeId ref_node_id = UNKNOWN_NODEID;
  if (resolver->lookup_resolved_name (ast_node_id, &ref_node_id))
    {
      Resolver::Definition def;
      if (!resolver->lookup_definition (ref_node_id, &def))
	{
	  rust_error_at (expr.get_locus (),
			 "unknown reference for resolved name");
	  return;
	}
      ref_node_id = def.parent;
      HirId ref;
      if (!mappings->lookup_node_to_hir (expr.get_mappings ().get_crate_num (),
					 ref_node_id, &ref))
	{
	  rust_error_at (expr.get_locus (), "reverse lookup failure");
	  return;
	}
      if (scannedSymbols.find (ref) == scannedSymbols.end ())
	{
	  worklist.push_back (ref);
	}
      liveSymbols.emplace (ref);
    }
}

void
MarkLive::visit (HIR::IdentifierExpr &expr)
{
  NodeId ast_node_id = expr.get_mappings ().get_nodeid ();

  // then lookup the reference_node_id
  NodeId ref_node_id = UNKNOWN_NODEID;
  if (resolver->lookup_resolved_name (ast_node_id, &ref_node_id))
    {
      // these ref_node_ids will resolve to a pattern declaration but we are
      // interested in the definition that this refers to get the parent id
      Resolver::Definition def;
      if (!resolver->lookup_definition (ref_node_id, &def))
	{
	  rust_error_at (expr.get_locus (),
			 "unknown reference for resolved name");
	  return;
	}
      ref_node_id = def.parent;
    }
  else if (!resolver->lookup_resolved_type (ast_node_id, &ref_node_id))
    {
      rust_error_at (expr.get_locus (),
		     "Failed to lookup type reference for node: %s",
		     expr.as_string ().c_str ());
      return;
    }

  if (ref_node_id == UNKNOWN_NODEID)
    {
      rust_error_at (expr.get_locus (), "unresolved node: %s",
		     expr.as_string ().c_str ());
      return;
    }

  // node back to HIR
  HirId ref;
  if (!mappings->lookup_node_to_hir (expr.get_mappings ().get_crate_num (),
				     ref_node_id, &ref))
    {
      rust_error_at (expr.get_locus (), "reverse lookup failure");
      return;
    }

  if (scannedSymbols.find (ref) == scannedSymbols.end ())
    {
      worklist.push_back (ref);
    }
  liveSymbols.emplace (ref);
}

} // namespace Analysis
} // namespace Rust
