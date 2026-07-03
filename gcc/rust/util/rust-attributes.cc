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

#include "rust-attributes.h"
#include "rust-ast.h"
#include "rust-ast-full.h"
#include "rust-attribute-values.h"

namespace Rust {
namespace Analysis {

using Attrs = Values::Attributes;

// https://doc.rust-lang.org/stable/nightly-rustc/src/rustc_feature/builtin_attrs.rs.html#248
static const BuiltinAttrDefinition __definitions[]
  = {{Attrs::INLINE, CODE_GENERATION},
     {Attrs::COLD, CODE_GENERATION},
     {Attrs::CFG, EXPANSION},
     {Attrs::CFG_ATTR, EXPANSION},
     {Attrs::DERIVE_ATTR, EXPANSION},
     {Attrs::DEPRECATED, STATIC_ANALYSIS},
     {Attrs::ALLOW, STATIC_ANALYSIS},
     {Attrs::WARN, STATIC_ANALYSIS},
     {Attrs::DENY, STATIC_ANALYSIS},
     {Attrs::ALLOW_INTERNAL_UNSTABLE, STATIC_ANALYSIS},
     {Attrs::DOC, HIR_LOWERING},
     {Attrs::MUST_USE, STATIC_ANALYSIS},
     {Attrs::LANG, HIR_LOWERING},
     {Attrs::LINK_NAME, CODE_GENERATION},
     {Attrs::LINK_SECTION, CODE_GENERATION},
     {Attrs::NO_MANGLE, CODE_GENERATION},
     {Attrs::RUSTC_STD_INTERNAL_SYMBOL, CODE_GENERATION},
     {Attrs::EXPORT_NAME, CODE_GENERATION},
     {Attrs::REPR, CODE_GENERATION},
     {Attrs::RUSTC_BUILTIN_MACRO, EXPANSION},
     {Attrs::RUSTC_MACRO_TRANSPARENCY, EXPANSION},
     {Attrs::PATH, EXPANSION},
     {Attrs::MACRO_USE, NAME_RESOLUTION},
     {Attrs::MACRO_EXPORT, NAME_RESOLUTION},
     {Attrs::PROC_MACRO, EXPANSION},
     {Attrs::PROC_MACRO_DERIVE, EXPANSION},
     {Attrs::PROC_MACRO_ATTRIBUTE, EXPANSION},
     // FIXME: This is not implemented yet, see
     // https://github.com/Rust-GCC/gccrs/issues/1475
     {Attrs::TARGET_FEATURE, CODE_GENERATION},
     // From now on, these are reserved by the compiler and gated through
     // #![feature(rustc_attrs)]
     {Attrs::FEATURE, STATIC_ANALYSIS},
     {Attrs::NO_CORE, CODE_GENERATION},
     {Attrs::NO_STD, CODE_GENERATION},
     {Attrs::DOC, EXTERNAL},
     {Attrs::CRATE_NAME, CODE_GENERATION},
     {Attrs::CRATE_TYPE, CODE_GENERATION},
     {Attrs::MAY_DANGLE, STATIC_ANALYSIS},
     {Attrs::RUSTC_DEPRECATED, STATIC_ANALYSIS},
     {Attrs::RUSTC_INHERIT_OVERFLOW_CHECKS, CODE_GENERATION},
     {Attrs::STABLE, STATIC_ANALYSIS},
     {Attrs::UNSTABLE, STATIC_ANALYSIS},
     // assuming we keep these for static analysis
     {Attrs::RUSTC_PROMOTABLE, CODE_GENERATION},
     {Attrs::RUSTC_CONST_STABLE, STATIC_ANALYSIS},
     {Attrs::RUSTC_CONST_UNSTABLE, STATIC_ANALYSIS},
     {Attrs::RUSTC_ALLOW_CONST_FN_UNSTABLE, STATIC_ANALYSIS},
     {Attrs::PRELUDE_IMPORT, NAME_RESOLUTION},
     {Attrs::TRACK_CALLER, CODE_GENERATION},
     {Attrs::RUSTC_SPECIALIZATION_TRAIT, TYPE_CHECK},
     {Attrs::RUSTC_UNSAFE_SPECIALIZATION_MARKER, TYPE_CHECK},
     {Attrs::RUSTC_RESERVATION_IMPL, TYPE_CHECK},
     {Attrs::RUSTC_PAREN_SUGAR, TYPE_CHECK},
     {Attrs::RUSTC_NONNULL_OPTIMIZATION_GUARANTEED, TYPE_CHECK},
     {Attrs::RUSTC_LAYOUT_SCALAR_VALID_RANGE_START, CODE_GENERATION},
     // TODO: be careful about calling functions marked with this?
     {Attrs::RUSTC_ARGS_REQUIRED_CONST, CODE_GENERATION},
     {Attrs::COMPILER_BUILTINS, CODE_GENERATION},
     {Attrs::NO_BUILTINS, CODE_GENERATION},
     {Attrs::PRELUDE_IMPORT, NAME_RESOLUTION},
     {Attrs::RUSTC_DIAGNOSTIC_ITEM, STATIC_ANALYSIS},
     {Attrs::RUSTC_ON_UNIMPLEMENTED, STATIC_ANALYSIS},
     {Attrs::FUNDAMENTAL, TYPE_CHECK},
     {Attrs::NON_EXHAUSTIVE, TYPE_CHECK},
     {Attrs::RUSTFMT, EXTERNAL},
     {Attrs::TEST, CODE_GENERATION},
     {Attrs::RUSTC_ALLOCATOR, CODE_GENERATION},
     {Attrs::RUSTC_ALLOCATOR_NOUNWIND, CODE_GENERATION},
     {Attrs::GLOBAL_ALLOCATOR, CODE_GENERATION}};

static const std::set<std::string> __outer_attributes
  = {Attrs::INLINE,
     Attrs::DERIVE_ATTR,
     Attrs::ALLOW_INTERNAL_UNSTABLE,
     Attrs::LANG,
     Attrs::REPR,
     Attrs::PATH,
     Attrs::TARGET_FEATURE,
     Attrs::TEST,
     Attrs::COLD,
     Attrs::MACRO_USE,
     Attrs::MACRO_EXPORT,
     Attrs::PROC_MACRO_ATTRIBUTE,
     Attrs::PROC_MACRO_DERIVE,
     Attrs::DEPRECATED,
     Attrs::MUST_USE,
     Attrs::LINK_NAME,
     Attrs::LINK_SECTION};

bool
Attributes::is_known (const std::string &attribute_path)
{
  const auto &lookup
    = BuiltinAttributeMappings::get ()->lookup_builtin (attribute_path);

  return !lookup.is_error ();
}

bool
Attributes::valid_outer_attribute (const std::string &attribute_path)
{
  return __outer_attributes.find (attribute_path) != __outer_attributes.cend ();
}

tl::optional<std::string>
Attributes::extract_string_literal (const AST::Attribute &attr)
{
  if (!attr.has_attr_input ())
    return tl::nullopt;

  auto &attr_input = attr.get_attr_input ();

  if (attr_input.get_attr_input_type ()
      != AST::AttrInput::AttrInputType::LITERAL)
    return tl::nullopt;

  auto &literal_expr
    = static_cast<AST::AttrInputLiteral &> (attr_input).get_literal ();

  auto lit_type = literal_expr.get_lit_type ();

  // TODO: bring escape sequence handling out of lexing?
  if (lit_type != AST::Literal::LitType::STRING
      && lit_type != AST::Literal::LitType::RAW_STRING)
    return tl::nullopt;

  return literal_expr.as_string ();
}
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

tl::optional<BuiltinAttrDefinition>
lookup_builtin (const AST::Attribute &attribute)
{
  auto &segments = attribute.get_path ().get_segments ();

  // Builtin attributes always have a single segment. This avoids us creating
  // strings all over the place and performing a linear search in the builtins
  // map
  if (segments.size () != 1)
    return tl::nullopt;

  return BuiltinAttributeMappings::get ()->lookup_builtin (
    segments.at (0).get_segment_name ());
}

} // namespace Analysis
} // namespace Rust
