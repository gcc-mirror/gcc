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

#include "rust-mapping-common.h"

namespace Rust {
namespace Analysis {

class UnusedContext
{
public:
  // Unused var
  void add_variable (HirId id);
  bool is_variable_used (HirId id) const;

  // Assigned var
  void add_assign (HirId id_def, HirId id);
  void remove_assign (HirId id_def);
  bool is_variable_assigned (HirId id_def, HirId id);

  // Mutable var
  void add_mut (HirId id);
  void remove_mut (HirId id);
  bool is_mut_used (HirId id) const;
  std::string as_string () const;

private:
  std::unordered_set<HirId> used_vars;
  std::unordered_set<HirId> mutable_vars;
  std::map<HirId, std::vector<HirId>> assigned_vars;
};

} // namespace Analysis
} // namespace Rust
