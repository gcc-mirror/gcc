// Copyright (C) 2024 Free Software Foundation, Inc.

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

#ifndef RUST_AST_BUILTIN_NODES_H
#define RUST_AST_BUILTIN_NODES_H

#include "rust-system.h"
#include "line-map.h"
#include "optional.h"
#include "rust-ast.h"
#include "rust-fmt.h"

namespace Rust {
namespace AST {

// Definitions, from rustc's `FormatArgs` AST struct
// https://github.com/rust-lang/rust/blob/1be468815c/compiler/rustc_ast/src/format.rs
//
// format_args!("hello {abc:.xyz$}!!", abc="world");
// └──────────────────────────────────────────────┘
//                     FormatArgs
//
// format_args!("hello {abc:.xyz$}!!", abc="world");
//                                     └─────────┘
//                                      argument
//
// format_args!("hello {abc:.xyz$}!!", abc="world");
//              └───────────────────┘
//                     template
//
// format_args!("hello {abc:.xyz$}!!", abc="world");
//               └────┘└─────────┘└┘
//                      pieces
//
// format_args!("hello {abc:.xyz$}!!", abc="world");
//               └────┘           └┘
//                   literal pieces
//
// format_args!("hello {abc:.xyz$}!!", abc="world");
//                     └─────────┘
//                     placeholder
//
// format_args!("hello {abc:.xyz$}!!", abc="world");
//                      └─┘  └─┘
//                      positions (could be names, numbers, empty, or `*`)

class FormatArgumentKind
{
public:
  Identifier &get_ident ()
  {
    rust_assert (kind == Kind::Captured || kind == Kind::Named);

    return ident.value ();
  }

private:
  enum class Kind
  {
    Normal,
    Named,
    Captured,
  } kind;

  tl::optional<Identifier> ident;
};

class FormatArgument
{
  FormatArgumentKind kind;
  std::unique_ptr<Expr> expr;
};

class FormatArguments
{
  std::vector<FormatArgument> args;
};

// TODO: Format documentation better
// Having a separate AST node for `format_args!()` expansion allows some
// important optimizations which help reduce generated code a lot. For example,
// turning `format_args!("a {} {} {}", 15, "hey", 'a')` directly into
// `format_args!("a 15 hey a")`, since all arguments are literals. Or,
// flattening imbricated `format_args!()` calls: `format_args!("heyo {}",
// format_args!("result: {}", some_result))` -> `format_args!("heyo result: {}",
// some_result)`
// FIXME: Move to rust-macro.h
class FormatArgs : public Visitable
{
public:
  enum class Newline
  {
    Yes,
    No
  };

  FormatArgs (location_t loc, Fmt::PieceSlice template_str,
	      FormatArguments arguments)
    : loc (loc), template_str (std::move (template_str)),
      arguments (std::move (arguments))
  {}

  void accept_vis (AST::ASTVisitor &vis);

private:
  location_t loc;
  Fmt::PieceSlice template_str;
  FormatArguments arguments;
};

} // namespace AST
} // namespace Rust

#endif // ! RUST_AST_BUILTIN_NODES_H
