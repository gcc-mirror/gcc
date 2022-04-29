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

#include "rust-ast-lower-item.h"

namespace Rust {
namespace HIR {

HIR::SimplePath
ASTLoweringSimplePath::translate (const AST::SimplePath &path)
{
  ASTLoweringSimplePath resolver;

  return resolver.lower (path);
}

HIR::SimplePathSegment
ASTLoweringSimplePath::lower (const AST::SimplePathSegment &segment)
{
  auto crate_num = mappings->get_current_crate ();
  auto node_id = segment.get_node_id ();

  auto mapping = Analysis::NodeMapping (crate_num, node_id,
					mappings->get_next_hir_id (crate_num),
					UNKNOWN_LOCAL_DEFID);

  auto hir_seg = HIR::SimplePathSegment (mapping);

  mappings->insert_node_to_hir (crate_num, node_id, mapping.get_hirid ());
  mappings->insert_simple_path_segment (crate_num, node_id, &segment);

  return hir_seg;
}

HIR::SimplePath
ASTLoweringSimplePath::lower (const AST::SimplePath &path)
{
  auto segments = std::vector<HIR::SimplePathSegment> ();
  for (auto &segment : path.get_segments ())
    segments.emplace_back (lower (segment));

  auto crate_num = mappings->get_current_crate ();
  auto node_id = path.get_node_id ();

  auto mapping = Analysis::NodeMapping (crate_num, node_id,
					mappings->get_next_hir_id (crate_num),
					UNKNOWN_LOCAL_DEFID);

  auto lowered = HIR::SimplePath (std::move (segments), mapping);

  mappings->insert_node_to_hir (crate_num, node_id, mapping.get_hirid ());
  mappings->insert_simple_path (crate_num, node_id, &path);

  return lowered;
}

} // namespace HIR
} // namespace Rust
