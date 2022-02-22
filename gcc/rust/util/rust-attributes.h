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

#include "rust-system.h"

namespace Rust {
namespace Analysis {

enum CompilerPass
{
  UNKNOWN,

  EXPANSION,
  NAME_RESOLUTION,
  HIR_LOWERING,
  TYPE_CHECK,
  STATIC_ANALYSIS,
  CODE_GENERATION
};

struct BuiltinAttrDefinition
{
  std::string name;
  CompilerPass handler;

  static BuiltinAttrDefinition get_error ()
  {
    return BuiltinAttrDefinition{"", UNKNOWN};
  }

  static BuiltinAttrDefinition &error_node ()
  {
    static BuiltinAttrDefinition error_node = get_error ();
    return error_node;
  }

  bool is_error () const { return name.empty (); }
};

class BuiltinAttributeMappings
{
public:
  static BuiltinAttributeMappings *get ();

  const BuiltinAttrDefinition &
  lookup_builtin (const std::string &attr_name) const;

private:
  BuiltinAttributeMappings ();

  std::map<std::string, const BuiltinAttrDefinition> mappings;
};

} // namespace Analysis
} // namespace Rust
