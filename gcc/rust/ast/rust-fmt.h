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

#include "rust-diagnostics.h"
#include "rust-system.h"

namespace Rust {
namespace Fmt {

struct RustHamster
{
  // hehe
};

struct InnerSpan
{
};

struct Count
{
  enum class Kind
  {
    Is,
    IsName,
    IsParam,
    IsStar,
    Implied
  } kind;

  union
  {
    size_t is;
    std::pair<RustHamster, InnerSpan> is_name;
    size_t is_param;
    size_t is_star;
  } data;
};

struct DebugHex
{
};

struct Sign
{
};

struct Alignment
{
};

struct RustString
{
  // hehe
};

struct Position
{
};

struct FormatSpec
{
  /// Optionally specified character to fill alignment with.
  tl::optional<char /* FIXME: This is a Rust char, not a C++ char - use an uint32_t instead?  */> fill;
  /// Span of the optionally specified fill character.
  tl::optional<InnerSpan> fill_span;
  /// Optionally specified alignment.
  Alignment align;
  /// The `+` or `-` flag.
  tl::optional<Sign> sign;
  /// The `#` flag.
  bool alternate;
  /// The `0` flag.
  bool zero_pad;
  /// The `x` or `X` flag. (Only for `Debug`.)
  tl::optional<DebugHex> debug_hex;
  /// The integer precision to use.
  // Count <'a> precision;
  /// The span of the precision formatting flag (for diagnostics).
  tl::optional<InnerSpan> precision_span;
  /// The string width requested for the resulting format.
  // Count <'a> width;
  /// The span of the width formatting flag (for diagnostics).
  tl::optional<InnerSpan> width_span;
  /// The descriptor string representing the name of the format desired for
  /// this argument, this can be empty or any number of characters, although
  /// it is required to be one word.
  RustHamster ty;
  // &'a str ty;
  /// The span of the descriptor string (for diagnostics).
  tl::optional<InnerSpan> ty_span;
};

struct Argument
{
  Position position;
  InnerSpan inner_span;
  FormatSpec format;
};

struct Piece
{
  enum class Kind
  {
    String,
    NextArgument
  } kind;

  union
  {
    RustString string;
    Argument *next_argument;
  } data;
};

struct PieceSlice
{
  Piece *ptr;
  size_t len;
};

extern "C" {
PieceSlice
collect_pieces (const char *);
}

struct Pieces
{
  static Pieces collect (const std::string &to_parse);
};

} // namespace Fmt
} // namespace Rust

#endif // ! RUST_FMT_H
