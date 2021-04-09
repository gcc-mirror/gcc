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

#include "rust-hir-liveness.h"
#include "rust-hir-full.h"
#include "rust-name-resolver.h"

namespace Rust {
namespace Analysis {

class FindEntryPoint : public LivenessBase
{
  using Rust::Analysis::LivenessBase::visit;

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
  FindEntryPoint () : LivenessBase () {}
  std::vector<HirId> entryPoints;
  std::vector<HirId> getEntryPoint () { return entryPoints; }
};

std::set<HirId>
Liveness::Analysis (HIR::Crate &crate)
{
  Liveness liveness (FindEntryPoint::find (crate));
  liveness.go (crate);

  return liveness.liveSymbols;
}

void
Liveness::go (HIR::Crate &crate)
{
  while (!worklist.empty ())
    {
      HirId hirId = worklist.back ();
      worklist.pop_back ();
      scannedSymbols.emplace (hirId);
      HIR::Item *item
	= mappings->lookup_hir_item (crate.get_mappings ().get_crate_num (),
				     hirId);
      if (item == nullptr)
	continue;
      liveSymbols.emplace (hirId);
      item->accept_vis (*this);
    }
}

void
Liveness::visit (HIR::ExprStmtWithoutBlock &stmt)
{
  stmt.get_expr ()->accept_vis (*this);
}

void
Liveness::visit (HIR::CallExpr &expr)
{
  expr.get_fnexpr ()->accept_vis (*this);
}

void
Liveness::visit (HIR::PathInExpression &expr)
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
      if (scannedSymbols.find (ref) != scannedSymbols.end ())
	{
	  worklist.push_back (ref);
	}
      liveSymbols.emplace (ref);
    }
}

void
Liveness::visit (HIR::Function &function)
{
  function.get_definition ().get ()->accept_vis (*this);
}

void
Liveness::visit (HIR::BlockExpr &expr)
{
  expr.iterate_stmts ([&] (HIR::Stmt *s) mutable -> bool {
    s->accept_vis (*this);
    return true;
  });
}

} // namespace Analysis
} // namespace Rust
