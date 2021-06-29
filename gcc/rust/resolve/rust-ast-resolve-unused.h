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

#ifndef RUST_AST_RESOLVE_UNUSED_H
#define RUST_AST_RESOLVE_UNUSED_H

#include "rust-hir-map.h"
#include "rust-ast-resolve-base.h"

namespace Rust {
namespace Resolver {

class ScanUnused
{
public:
  static void ScanRib (Rib *r)
  {
    r->iterate_decls ([&] (NodeId decl_node_id, Location locus) -> bool {
      CanonicalPath ident = CanonicalPath::create_empty ();

      bool ok = r->lookup_canonical_path (decl_node_id, &ident);
      rust_assert (ok);

      Analysis::Mappings *map = Analysis::Mappings::get ();
      HirId decl_hir_id;
      // ScanUnused is conflicting with the dead code analysis here on types and
      // functions. So just ignoring the warnings of item which will be handled
      // by dead code analysis.
      HIR::Item *found_item = nullptr;
      if (map->lookup_node_to_hir (r->get_crate_num (), decl_node_id,
				   &decl_hir_id))
	{
	  found_item = map->lookup_hir_item (r->get_crate_num (), decl_hir_id);
	}

      if (!r->have_references_for_node (decl_node_id)
	  && ident.get ().at (0) != '_' && !found_item)
	{
	  rust_warning_at (locus, 0, "unused name '%s'", ident.get ().c_str ());
	}
      return true;
    });
  }

  static void Scan ()
  {
    auto resolver = Resolver::get ();
    resolver->iterate_name_ribs ([&] (Rib *r) -> void { ScanRib (r); });
    // ScanUnused is conflicting with the dead code analysis here on types.
    // resolver->iterate_type_ribs ([&] (Rib *r) -> void { ScanRib (r); });
    resolver->iterate_label_ribs ([&] (Rib *r) -> void { ScanRib (r); });
  }
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_AST_RESOLVE_UNUSED_H
