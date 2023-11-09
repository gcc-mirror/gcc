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

#ifndef RUST_FMT_H
#define RUST_FMT_H

#include "expected.h"
#include "optional.h"
#include "rust-ast.h"
#include "rust-system.h"

namespace Rust {

/**
 * This class implements the parsing of Rust format strings according to the
 * grammar here: https://doc.rust-lang.org/std/fmt/index.html#syntax
 */
// TODO: Are there features that are only present in specific Rust editions?
class Fmt
{
public:
  // TODO: Keep location information
  // TODO: Switch to a Rust::AST::Literal here
  using Input = std::string;

  enum class Error
  {
    Align,
    Sign,
  };

  template <typename T> class Result
  {
  public:
    explicit Result (Input remaining_input, T result)
      : remaining_input (remaining_input), result (result)
    {}

  private:
    Input remaining_input;
    T result;
  };

  // FIXME: Do not use an owned string here
  static tl::expected<Fmt, Fmt::Error> parse_fmt_string (Input input);

private:
  // the parse functions should return the remaining input as well as the
  // expected node let's look at nom
  // TODO: no string view :( use an owned string for now?

  template <typename T> struct ParseResult
  {
    tl::expected<Result<T>, Error> inner;

    ParseResult (tl::expected<Result<T>, Error> inner) : inner (inner) {}
    ParseResult operator= (tl::expected<Result<T>, Error> inner)
    {
      return ParseResult (inner);
    }

    Input remaining_input () { return inner->remaining_input; }
    T value () { return inner->value; }
  };

  struct Format
  {
  };

  struct Argument
  {
    enum struct Kind
    {
      Integer,
      Identifier,
    } kind;

    int integer;
    Identifier identifier;
  };

  struct FormatSpec
  {
  };

  struct Fill
  {
    char to_fill;
  };

  enum class Align
  {
    Left,
    Top,
    Right
  };

  enum class Sign
  {
    Plus,
    Minus
  };

  // let's do one function per rule in the BNF
  static tl::expected<Result<std::string>, Error> text (Input input);
  static tl::expected<Result<tl::optional<Format>>, Error>
  maybe_format (Input input);
  static tl::expected<Result<Format>, Error> format (Input input);
  static tl::expected<Result<Argument>, Error> argument (Input input);
  static tl::expected<Result<FormatSpec>, Error> format_spec (Input input);
  static tl::expected<Result<Fill>, Error> fill (Input input);
  static tl::expected<Result<Align>, Error> align (Input input);
  static tl::expected<Result<Sign>, Error> sign (Input input);
};

} // namespace Rust

#endif // ! RUST_FMT_H
