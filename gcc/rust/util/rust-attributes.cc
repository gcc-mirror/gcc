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

#include "rust-attributes.h"

namespace Rust {
namespace Analysis {

// https://doc.rust-lang.org/stable/nightly-rustc/src/rustc_feature/builtin_attrs.rs.html#248
static const BuiltinAttrDefinition __definitions[] = {
  {"inline", CODE_GENERATION},
  {"cold", CODE_GENERATION},
  {"cfg", EXPANSION},
  {"cfg_attr", EXPANSION},
  {"deprecated", STATIC_ANALYSIS},
  {"allow", STATIC_ANALYSIS},
  {"doc", HIR_LOWERING},
  {"must_use", STATIC_ANALYSIS},
  {"lang", HIR_LOWERING},
  {"link_section", CODE_GENERATION},
  {"no_mangle", CODE_GENERATION},
  {"repr", CODE_GENERATION},
};

BuiltinAttributeMappings *
BuiltinAttributeMappings::get ()
{
  static BuiltinAttributeMappings *instance = nullptr;
  if (instance == nullptr)
    instance = new BuiltinAttributeMappings ();

  return instance;
}

const BuiltinAttrDefinition &
BuiltinAttributeMappings::lookup_builtin (const std::string &attr_name) const
{
  auto it = mappings.find (attr_name);
  if (it == mappings.end ())
    return BuiltinAttrDefinition::error_node ();

  return it->second;
}

BuiltinAttributeMappings::BuiltinAttributeMappings ()
{
  size_t ndefinitions = sizeof (__definitions) / sizeof (BuiltinAttrDefinition);
  for (size_t i = 0; i < ndefinitions; i++)
    {
      const BuiltinAttrDefinition &def = __definitions[i];
      mappings.insert ({def.name, def});
    }
}

} // namespace Analysis
} // namespace Rust
