// Copyright (C) 2020-2026 Free Software Foundation, Inc.

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
#ifndef RUST_ATTRIBUTES_H
#define RUST_ATTRIBUTES_H

#include "rust-ast.h"
#include "optional.h"

namespace Rust {
namespace Analysis {

class Attributes
{
public:
  enum class AttributeKnowledge
  {
    /**
     * Built-in attribute
     */
    Known,
    /**
     * Tool attribute, to be handled by the specified tool rather than the
     * compiler
     */
    Tool,
    /**
     * Unknown attribute
     */
    Unknown,
  };

  static AttributeKnowledge is_known (const std::string &attribute_path);
  static bool valid_outer_attribute (const std::string &attribute_path);
  static tl::optional<std::string>
  extract_string_literal (const AST::Attribute &attr);
};

enum CompilerPass
{
  UNKNOWN,

  EXPANSION,
  NAME_RESOLUTION,
  HIR_LOWERING,
  TYPE_CHECK,
  STATIC_ANALYSIS,
  CODE_GENERATION,

  // External Rust tooling attributes, like #[rustfmt::skip]
  EXTERNAL,

  // Do we need to add something here for const fns?
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

tl::optional<BuiltinAttrDefinition>
lookup_builtin (const AST::Attribute &attribute);

} // namespace Analysis
} // namespace Rust

#endif /* ! RUST_ATTRIBUTES_H */
