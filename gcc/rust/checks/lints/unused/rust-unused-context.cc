// Copyright (C) 2025-2026 Free Software Foundation, Inc.

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

#include "rust-unused-context.h"

namespace Rust {
namespace Analysis {

void
UnusedContext::add_variable (HirId id)

{
  used_vars.emplace (id);
}

bool
UnusedContext::is_variable_used (HirId id) const
{
  return used_vars.find (id) != used_vars.end ();
}

void
UnusedContext::add_assign (HirId id_def, HirId id)
{
  assigned_vars[id_def].push_back (id);
}

void
UnusedContext::remove_assign (HirId id_def)
{
  if (assigned_vars.find (id_def) != assigned_vars.end ())
    assigned_vars[id_def].pop_back ();
}

bool
UnusedContext::is_variable_assigned (HirId id_def, HirId id)
{
  auto assigned_vec = assigned_vars[id_def];
  return std::find (assigned_vec.begin (), assigned_vec.end (), id)
	 != assigned_vec.end ();
}

void
UnusedContext::add_mut (HirId id)
{
  mutable_vars.emplace (id);
}

void
UnusedContext::remove_mut (HirId id)
{
  mutable_vars.erase (id);
}

bool
UnusedContext::is_mut_used (HirId id) const
{
  return mutable_vars.find (id) == mutable_vars.end ();
}

void
UnusedContext::add_label (HirId id)

{
  used_labels.emplace (id);
}

bool
UnusedContext::is_label_used (HirId id) const
{
  return used_labels.find (id) != used_labels.end ();
}

std::string
UnusedContext::as_string () const
{
  std::stringstream ss;
  ss << "UnusedContext: ";
  for (const auto &v : used_vars)
    {
      ss << "HirId: " << v << "\n";
    }
  return ss.str ();
}

} // namespace Analysis
} // namespace Rust
