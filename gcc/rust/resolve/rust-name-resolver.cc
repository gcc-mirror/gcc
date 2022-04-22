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

#include "rust-name-resolver.h"

namespace Rust {
namespace Resolver {

Rib::Rib (CrateNum crateNum, NodeId node_id)
  : crate_num (crateNum), node_id (node_id),
    mappings (Analysis::Mappings::get ())
{}

void
Rib::insert_name (
  const CanonicalPath &path, NodeId id, Location locus, bool shadow,
  std::function<void (const CanonicalPath &, NodeId, Location)> dup_cb)
{
  auto it = path_mappings.find (path);
  bool path_already_exists = it != path_mappings.end ();
  if (path_already_exists && !shadow)
    {
      const auto &decl = decls_within_rib.find (it->second);
      if (decl != decls_within_rib.end ())
	dup_cb (path, it->second, decl->second);
      else
	dup_cb (path, it->second, locus);

      return;
    }

  path_mappings[path] = id;
  reverse_path_mappings.insert (std::pair<NodeId, CanonicalPath> (id, path));
  decls_within_rib.insert (std::pair<NodeId, Location> (id, locus));
  references[id] = {};
}

bool
Rib::lookup_name (const CanonicalPath &ident, NodeId *id)
{
  auto it = path_mappings.find (ident);
  if (it == path_mappings.end ())
    return false;

  *id = it->second;
  return true;
}

void
Rib::clear_name (const CanonicalPath &ident, NodeId id)
{
  auto ii = path_mappings.find (ident);
  if (ii != path_mappings.end ())
    path_mappings.erase (ii);

  auto ij = reverse_path_mappings.find (id);
  if (ij != reverse_path_mappings.end ())
    reverse_path_mappings.erase (ij);

  auto ik = decls_within_rib.find (id);
  if (ik != decls_within_rib.end ())
    decls_within_rib.erase (ik);
}

void
Rib::append_reference_for_def (NodeId def, NodeId ref)
{
  references[def].insert (ref);
}

bool
Rib::have_references_for_node (NodeId def) const
{
  auto it = references.find (def);
  if (it == references.end ())
    return false;

  return !it->second.empty ();
}

bool
Rib::decl_was_declared_here (NodeId def) const
{
  for (auto &it : decls_within_rib)
    {
      if (it.first == def)
	return true;
    }
  return false;
}

} // namespace Resolver
} // namespace Rust
