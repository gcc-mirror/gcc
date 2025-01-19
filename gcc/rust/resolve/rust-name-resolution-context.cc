// Copyright (C) 2020-2025 Free Software Foundation, Inc.

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

#include "rust-name-resolution-context.h"
#include "optional.h"
#include "rust-mapping-common.h"

namespace Rust {
namespace Resolver2_0 {

NameResolutionContext::NameResolutionContext ()
  : mappings (*Analysis::Mappings::get ())
{}

tl::expected<NodeId, DuplicateNameError>
NameResolutionContext::insert (Identifier name, NodeId id, Namespace ns)
{
  switch (ns)
    {
    case Namespace::Values:
      return values.insert (name, id);
    case Namespace::Types:
      return types.insert (name, id);
    case Namespace::Macros:
      return macros.insert (name, id);
    case Namespace::Labels:
    default:
      // return labels.insert (name, id);
      rust_unreachable ();
    }
}

tl::expected<NodeId, DuplicateNameError>
NameResolutionContext::insert_shadowable (Identifier name, NodeId id,
					  Namespace ns)
{
  switch (ns)
    {
    case Namespace::Values:
      return values.insert_shadowable (name, id);
    case Namespace::Types:
      return types.insert_shadowable (name, id);
    case Namespace::Macros:
      return macros.insert_shadowable (name, id);
    case Namespace::Labels:
    default:
      // return labels.insert (name, id);
      rust_unreachable ();
    }
}

void
NameResolutionContext::map_usage (Usage usage, Definition definition)
{
  auto inserted = resolved_nodes.emplace (usage, definition).second;

  // is that valid?
  rust_assert (inserted);
}

tl::optional<NodeId>
NameResolutionContext::lookup (NodeId usage)
{
  auto it = resolved_nodes.find (Usage (usage));

  if (it == resolved_nodes.end ())
    return tl::nullopt;

  return it->second.id;
}

void
NameResolutionContext::scoped (Rib rib, NodeId id,
			       std::function<void (void)> lambda,
			       tl::optional<Identifier> path)
{
  values.push (rib, id, path);
  types.push (rib, id, path);
  macros.push (rib, id, path);
  // labels.push (rib, id);

  lambda ();

  values.pop ();
  types.pop ();
  macros.pop ();
  // labels.pop (rib);
}

void
NameResolutionContext::scoped (Rib rib, Namespace ns, NodeId scope_id,
			       std::function<void (void)> lambda,
			       tl::optional<Identifier> path)
{
  switch (ns)
    {
    case Namespace::Values:
      values.push (rib, scope_id, path);
      break;
    case Namespace::Types:
      types.push (rib, scope_id, path);
      break;
    case Namespace::Labels:
    case Namespace::Macros:
      gcc_unreachable ();
    }

  lambda ();

  switch (ns)
    {
    case Namespace::Values:
      values.pop ();
      break;
    case Namespace::Types:
      types.pop ();
      break;
    case Namespace::Labels:
    case Namespace::Macros:
      gcc_unreachable ();
    }
}

} // namespace Resolver2_0
} // namespace Rust
