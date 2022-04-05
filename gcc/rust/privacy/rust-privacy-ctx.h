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

#include "rust-hir-map.h"
#include "rust-privacy-check.h"

namespace Rust {
namespace Privacy {

/**
 * Reachability levels of HIR nodes. These levels are computed through the
 * `ReachabilityVisitor` visitor.
 */
enum ReachLevel
{
  Unreachable,
  Reachable,
};

class PrivacyContext
{
public:
  /**
   * Insert a new resolved visibility for a given node
   *
   * @param mappings Mappings of the node to store the reach level for
   * @param reach Level of reachability for the given node
   */
  void insert_reachability (const Analysis::NodeMapping &mapping,
			    ReachLevel reach);

  /**
   * Lookup the visibility of an already declared Node
   *
   * @param mapping Mappings of the node to fetch the reach level of
   *
   * @return `nullptr` if the reach level for the current node has not been
   * added, a valid pointer otherwise
   */
  const ReachLevel *lookup_reachability (const Analysis::NodeMapping &mapping);

private:
  std::unordered_map<HirId, ReachLevel> reachability_map;
};
} // namespace Privacy
} // namespace Rust
