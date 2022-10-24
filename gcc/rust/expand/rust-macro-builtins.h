// Copyright (C) 2020-2023 Free Software Foundation, Inc.

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

#ifndef RUST_MACRO_BUILTINS_H
#define RUST_MACRO_BUILTINS_H

#include "rust-ast.h"
#include "rust-ast-fragment.h"
#include "rust-location.h"

/**
 * This class provides a list of builtin macros implemented by the compiler.
 * The functions defined are called "builtin transcribers" in that they replace
 * the transcribing part of a macro definition.
 *
 * Like regular macro transcribers, they are responsible for building and
 * returning an AST fragment: basically a vector of AST nodes put together.
 *
 * Unlike regular declarative macros where each match arm has its own associated
 * transcriber, builtin transcribers are responsible for handling all match arms
 * of the macro. This means that you should take extra care when implementing a
 * builtin containing multiple match arms: You will probably need to do some
 * lookahead in order to determine which match arm the user intended to use.
 *
 * An example of this is the `assert!()` macro:
 *
 * ```
 *  macro_rules! assert {
 *	($cond:expr $(,)?) => {{ ... }};
 *	($cond : expr, $ ($arg : tt) +) = > {{ ... }};
 * }
 * ```
 *
 * If more tokens exist beyond the optional comma, they need to be handled as
 * a token-tree for a custom panic message.
 *
 * These builtin macros with empty transcribers are defined in the standard
 * library. They are marked with a special attribute, `#[rustc_builtin_macro]`.
 * When this attribute is present on a macro definition, the compiler should
 * look for an associated transcriber in the mappings. Meaning that you must
 * remember to insert your transcriber in the `builtin_macros` map of the
 *`Mappings`.
 *
 * This map is built as a static variable in the `insert_macro_def()` method
 * of the `Mappings` class.
 */

namespace Rust {
class MacroBuiltin
{
public:
  static AST::Fragment assert_handler (Location invoc_locus,
				       AST::MacroInvocData &invoc);

  static AST::Fragment file_handler (Location invoc_locus,
				     AST::MacroInvocData &invoc);

  static AST::Fragment column_handler (Location invoc_locus,
				       AST::MacroInvocData &invoc);

  static AST::Fragment include_bytes_handler (Location invoc_locus,
					      AST::MacroInvocData &invoc);

  static AST::Fragment include_str_handler (Location invoc_locus,
					    AST::MacroInvocData &invoc);

  static AST::Fragment compile_error_handler (Location invoc_locus,
					      AST::MacroInvocData &invoc);

  static AST::Fragment concat_handler (Location invoc_locus,
				       AST::MacroInvocData &invoc);

  static AST::Fragment env_handler (Location invoc_locus,
				    AST::MacroInvocData &invoc);

  static AST::Fragment cfg_handler (Location invoc_locus,
				    AST::MacroInvocData &invoc);

  static AST::Fragment include_handler (Location invoc_locus,
					AST::MacroInvocData &invoc);

  static AST::Fragment line_handler (Location invoc_locus,
				     AST::MacroInvocData &invoc);
};
} // namespace Rust

#endif // RUST_MACRO_BUILTINS_H
