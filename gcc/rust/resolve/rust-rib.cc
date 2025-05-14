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

#include "rust-rib.h"
#include "rust-name-resolution-context.h"

namespace Rust {
namespace Resolver2_0 {

Rib::Definition::Definition (NodeId id, Mode mode, bool enum_variant)
  : enum_variant (enum_variant)
{
  switch (mode)
    {
    case Mode::SHADOWABLE:
      ids_shadowable.push_back (id);
      return;
    case Mode::NON_SHADOWABLE:
      ids_non_shadowable.push_back (id);
      return;
    case Mode::GLOBBED:
      ids_globbed.push_back (id);
      return;
    default:
      gcc_unreachable ();
    }
}

bool
Rib::Definition::is_ambiguous () const
{
  if (!ids_shadowable.empty ())
    return false;
  else if (!ids_non_shadowable.empty ())
    return ids_non_shadowable.size () > 1;
  else
    return ids_globbed.size () > 1;
}

bool
Rib::Definition::is_variant () const
{
  return enum_variant;
}

std::string
Rib::Definition::to_string () const
{
  std::stringstream out;
  const char *headers[3] = {"(S)[", "] (NS)[", "] (G)["};
  const std::vector<NodeId> *id_lists[3]
    = {&ids_shadowable, &ids_non_shadowable, &ids_globbed};
  for (int i = 0; i < 3; i++)
    {
      out << headers[i];
      std::string sep;
      for (auto id : *id_lists[i])
	{
	  out << sep << id;
	  sep = ",";
	}
    }
  out << "]";
  if (enum_variant)
    out << "(enum variant)";
  return out.str ();
}

Rib::Definition
Rib::Definition::Shadowable (NodeId id)
{
  return Definition (id, Mode::SHADOWABLE, false);
}

Rib::Definition
Rib::Definition::NonShadowable (NodeId id, bool enum_variant)
{
  return Definition (id, Mode::NON_SHADOWABLE, enum_variant);
}

Rib::Definition
Rib::Definition::Globbed (NodeId id)
{
  return Definition (id, Mode::GLOBBED, false);
}

DuplicateNameError::DuplicateNameError (std::string name, NodeId existing)
  : name (name), existing (existing)
{}

Rib::Rib (Kind kind) : kind (kind) {}

Rib::Rib (Kind kind, std::string identifier, NodeId id)
  : Rib (kind, {{identifier, id}})
{}

Rib::Rib (Kind kind, std::unordered_map<std::string, NodeId> to_insert)
  : kind (kind)
{
  for (auto &value : to_insert)
    values.insert ({value.first, Definition::NonShadowable (value.second)});
}

tl::expected<NodeId, DuplicateNameError>
Rib::insert (std::string name, Definition def)
{
  auto it = values.find (name);
  if (it == values.end ())
    {
      /* No old value */
      values[name] = def;
    }
  else if (it->second.ids_non_shadowable.empty ()
	   || def.ids_non_shadowable.empty ())
    { /* No non-shadowable conflict */
      auto &current = values[name];
      for (auto id : def.ids_non_shadowable)
	{
	  if (std::find (current.ids_non_shadowable.cbegin (),
			 current.ids_non_shadowable.cend (), id)
	      == current.ids_non_shadowable.cend ())
	    current.ids_non_shadowable.push_back (id);
	  else
	    // TODO: should this produce an error?
	    return tl::make_unexpected (DuplicateNameError (name, id));
	}
      for (auto id : def.ids_shadowable)
	{
	  if (std::find (current.ids_shadowable.cbegin (),
			 current.ids_shadowable.cend (), id)
	      == current.ids_shadowable.cend ())
	    current.ids_shadowable.push_back (id);
	  else
	    // TODO: should this produce an error?
	    return tl::make_unexpected (DuplicateNameError (name, id));
	}
      for (auto id : def.ids_globbed)
	{
	  if (std::find (current.ids_globbed.cbegin (),
			 current.ids_globbed.cend (), id)
	      == current.ids_globbed.cend ())
	    current.ids_globbed.push_back (id);
	  else
	    // TODO: should this produce an error?
	    return tl::make_unexpected (DuplicateNameError (name, id));
	}
    }
  else /* Multiple non-shadowable */
    {
      return tl::make_unexpected (
	DuplicateNameError (name, it->second.ids_non_shadowable.back ()));
    }

  if (!def.ids_shadowable.empty ())
    return def.ids_shadowable.back ();
  else if (!def.ids_non_shadowable.empty ())
    return def.ids_non_shadowable.back ();
  rust_assert (!def.ids_globbed.empty ());
  return def.ids_globbed.back ();
}

tl::optional<Rib::Definition>
Rib::get (const std::string &name)
{
  auto it = values.find (name);

  if (it == values.end ())
    return tl::nullopt;

  return it->second;
}

const std::unordered_map<std::string, Rib::Definition> &
Rib::get_values () const
{
  return values;
}

} // namespace Resolver2_0
} // namespace Rust
