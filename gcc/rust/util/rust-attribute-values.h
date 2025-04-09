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

#ifndef RUST_ATTRIBUTE_VALUES_H
#define RUST_ATTRIBUTE_VALUES_H

namespace Rust {
namespace Values {
// TODO: Change this to a namespace + inline constexpr in the future
class Attributes
{
public:
  static constexpr auto &INLINE = "inline";
  static constexpr auto &COLD = "cold";
  static constexpr auto &CFG = "cfg";
  static constexpr auto &CFG_ATTR = "cfg_attr";
  static constexpr auto &DERIVE_ATTR = "derive";
  static constexpr auto &DEPRECATED = "deprecated";
  static constexpr auto &ALLOW = "allow";
  static constexpr auto &ALLOW_INTERNAL_UNSTABLE = "allow_internal_unstable";
  static constexpr auto &DOC = "doc";
  static constexpr auto &MUST_USE = "must_use";
  static constexpr auto &LANG = "lang";
  static constexpr auto &LINK_SECTION = "link_section";
  static constexpr auto &NO_MANGLE = "no_mangle";
  static constexpr auto &REPR = "repr";
  static constexpr auto &RUSTC_BUILTIN_MACRO = "rustc_builtin_macro";
  static constexpr auto &RUSTC_MACRO_TRANSPARENCY = "rustc_macro_transparency";
  static constexpr auto &PATH = "path";
  static constexpr auto &MACRO_USE = "macro_use";
  static constexpr auto &MACRO_EXPORT = "macro_export";
  static constexpr auto &PROC_MACRO = "proc_macro";
  static constexpr auto &PROC_MACRO_DERIVE = "proc_macro_derive";
  static constexpr auto &PROC_MACRO_ATTRIBUTE = "proc_macro_attribute";

  static constexpr auto &TARGET_FEATURE = "target_feature";
  // From now on, these are reserved by the compiler and gated through
  // #![feature(rustc_attrs)]
  static constexpr auto &RUSTC_DEPRECATED = "rustc_deprecated";
  static constexpr auto &RUSTC_INHERIT_OVERFLOW_CHECKS
    = "rustc_inherit_overflow_checks";
  static constexpr auto &STABLE = "stable";
  static constexpr auto &UNSTABLE = "unstable";

  static constexpr auto &RUSTC_PROMOTABLE = "rustc_promotable";
  static constexpr auto &RUSTC_CONST_STABLE = "rustc_const_stable";
  static constexpr auto &RUSTC_CONST_UNSTABLE = "rustc_const_unstable";

  static constexpr auto &RUSTC_SPECIALIZATION_TRAIT
    = "rustc_specialization_trait";
  static constexpr auto &RUSTC_UNSAFE_SPECIALIZATION_MARKER
    = "rustc_unsafe_specialization_marker";
  static constexpr auto &RUSTC_RESERVATION_IMPL = "rustc_reservation_impl";
  static constexpr auto &RUSTC_PAREN_SUGAR = "rustc_paren_sugar";
  static constexpr auto &RUSTC_NONNULL_OPTIMIZATION_GUARANTEED
    = "rustc_nonnull_optimization_guaranteed";

  static constexpr auto &RUSTC_LAYOUT_SCALAR_VALID_RANGE_START
    = "rustc_layout_scalar_valid_range_start";

  static constexpr auto &MAY_DANGLE = "may_dangle";
  static constexpr auto &PRELUDE_IMPORT = "prelude_import";
  static constexpr auto &TRACK_CALLER = "track_caller";

  static constexpr auto &RUSTC_DIAGNOSTIC_ITEM = "rustc_diagnostic_item";
  static constexpr auto &RUSTC_ON_UNIMPLEMENTED = "rustc_on_unimplemented";

  static constexpr auto &FUNDAMENTAL = "fundamental";

  static constexpr auto &NON_EXHAUSTIVE = "non_exhaustive";

  static constexpr auto &RUSTFMT = "rustfmt";
};
} // namespace Values
} // namespace Rust

#endif /* !RUST_ATTRIBUTE_VALUES_H */
