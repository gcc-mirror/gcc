// Copyright (C) 2025 Free Software Foundation, Inc.

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

#include "rust-unused-var-context.h"

namespace Rust {
namespace Analysis {

void
UnusedVarContext::add_variable (HirId id)
{
  if (is_used.find (id) == is_used.end ())
    is_used.insert ({id, false});
}

void
UnusedVarContext::mark_used (HirId id)
{
  is_used[id] = true;
}

bool
UnusedVarContext::is_variable_used (HirId id) const
{
  auto it = is_used.find (id);
  return it != is_used.end () && it->second;
}

std::string
UnusedVarContext::as_string () const
{
  std::stringstream ss;
  ss << "UnusedVarContext: ";
  for (const auto &pair : is_used)
    {
      ss << "HirId: " << pair.first << " Used: " << (pair.second ? "Yes" : "No")
	 << "\n";
    }
  return ss.str ();
}

} // namespace Analysis
} // namespace Rust
