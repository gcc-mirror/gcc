// Copyright (C) 2024-2025 Free Software Foundation, Inc.

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

// FIXME: Merge with the class below this one?
class FormatArgumentKind
{
public:
  enum class Kind
  {
    Normal,
    Named,
    Captured,
  } kind;

  Identifier &get_ident ()
  {
    rust_assert (kind == Kind::Captured || kind == Kind::Named);

    return ident.value ();
  }

  FormatArgumentKind (Kind kind, tl::optional<Identifier> ident)
    : kind (kind), ident (ident)
  {}

  FormatArgumentKind (const FormatArgumentKind &other)
  {
    kind = other.kind;
    ident = other.ident;
  }

  FormatArgumentKind operator= (const FormatArgumentKind &other)
  {
    kind = other.kind;
    ident = other.ident;

    return *this;
  }

private:
  tl::optional<Identifier> ident;
};

class FormatArgument
{
public:
  static FormatArgument normal (std::unique_ptr<Expr> expr)
  {
    return FormatArgument (FormatArgumentKind::Kind::Normal, tl::nullopt,
			   std::move (expr));
  }

  static FormatArgument named (Identifier ident, std::unique_ptr<Expr> expr)
  {
    return FormatArgument (FormatArgumentKind::Kind::Named, ident,
			   std::move (expr));
  }

  static FormatArgument captured (Identifier ident, std::unique_ptr<Expr> expr)
  {
    return FormatArgument (FormatArgumentKind::Kind::Captured, ident,
			   std::move (expr));
  }

  FormatArgument (const FormatArgument &other)
    : kind (other.kind), expr (other.expr->clone_expr ())
  {}

  FormatArgument operator= (const FormatArgument &other)
  {
    kind = other.kind;
    expr = other.expr->clone_expr ();

    return *this;
  }

  FormatArgumentKind get_kind () const { return kind; }
  const Expr &get_expr () const { return *expr; }

private:
  FormatArgument (FormatArgumentKind::Kind kind, tl::optional<Identifier> ident,
		  std::unique_ptr<Expr> expr)
    : kind (FormatArgumentKind (kind, ident)), expr (std::move (expr))
  {}

  FormatArgumentKind kind;
  std::unique_ptr<Expr> expr;
};

class FormatArguments
{
public:
  FormatArguments () {}
  FormatArguments (FormatArguments &&) = default;
  FormatArguments (const FormatArguments &other)
  {
    args = std::vector<FormatArgument> ();
    args.reserve (other.args.size ());

    for (const auto &arg : other.args)
      args.emplace_back (arg);
  };

  FormatArguments &operator= (const FormatArguments &other) = default;

  void push (FormatArgument &&elt) { args.emplace_back (std::move (elt)); }
  const FormatArgument at (size_t idx) const { return args.at (idx); }

private:
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
class FormatArgs : public Expr
{
public:
  enum class Newline
  {
    Yes,
    No
  };

  FormatArgs (location_t loc, Fmt::Pieces &&template_str,
	      FormatArguments &&arguments)
    : loc (loc), template_pieces (std::move (template_str)),
      arguments (std::move (arguments))
  {}

  FormatArgs (FormatArgs &&other) = default;
  FormatArgs (const FormatArgs &other) = default;
  FormatArgs &operator= (const FormatArgs &other) = default;

  void accept_vis (AST::ASTVisitor &vis) override;

  const Fmt::Pieces &get_template () const { return template_pieces; }
  const FormatArguments &get_arguments () const { return arguments; }
  virtual location_t get_locus () const override;

private:
  location_t loc;
  // FIXME: This probably needs to be a separate type - it is one in rustc's
  // expansion of format_args!(). There is extra handling associated with it.
  // we can maybe do that in rust-fmt.cc? in collect_pieces()? like do the
  // transformation into something we can handle better
  Fmt::Pieces template_pieces;
  FormatArguments arguments;

  bool marked_for_strip = false;

protected:
  virtual std::string as_string () const override;
  virtual bool is_expr_without_block () const override;
  virtual void mark_for_strip () override;
  virtual bool is_marked_for_strip () const override;
  virtual std::vector<Attribute> &get_outer_attrs () override;
  virtual void set_outer_attrs (std::vector<Attribute>) override;
  virtual Expr *clone_expr_impl () const override;
};

} // namespace AST
} // namespace Rust

#endif // ! RUST_AST_BUILTIN_NODES_H
