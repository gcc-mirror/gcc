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

#include "rust-hir-map.h"
#include "optional.h"

namespace Rust {
namespace Analysis {

template <typename Item, InsertionPolicy policy, InsertLocation insert_location>
tl::optional<Item *>
PtrMapping<Item, policy, insert_location>::lookup (HirId id)
{
  auto it = storage.find (id);
  if (it == storage.cend ())
    return tl::nullopt;
  return it->second;
}

/* Helper required because of GCC 5 baseline
 * We need to check if we should include location and then compile
 * `item->get_locus()` only if `get_locus` exists within the target item.
 *
 * With a more recent compiler this would be written as:
 * if constexpr(InsertLocation::YES == location) {
 *   // insert location
 * }
 * but this is not available on cpp14 with gcc5.
 */
template <typename Item, InsertLocation location /* InsertLocation::YES */>
struct InsertLocationHelper
{
  void insert_location (Item *item, HirId hir_id)
  {
    Analysis::Mappings::get ().insert_location (hir_id, item->get_locus ());
  }
};

template <typename Item> struct InsertLocationHelper<Item, InsertLocation::NO>
{
  void insert_location (Item *item, HirId hir_id) {}
};

template <typename Item, InsertionPolicy policy, InsertLocation insert_location>
void
PtrMapping<Item, policy, insert_location>::insert (Item *item)
{
  auto hir_id = item->get_mappings ().get_hirid ();

  if (policy == InsertionPolicy::FORBID_DUPLICATES)
    rust_assert (!lookup (hir_id).has_value ());

  storage.insert ({hir_id, item});

  Analysis::Mappings::get ().insert_node_to_hir (
    item->get_mappings ().get_nodeid (), hir_id);

  InsertLocationHelper<Item, insert_location> location_helper;
  location_helper.insert_location (item, hir_id);
}
} // namespace Analysis
} // namespace Rust
