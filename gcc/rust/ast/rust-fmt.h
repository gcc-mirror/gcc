// Copyright (C) 2023-2025 Free Software Foundation, Inc.

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

#include "rust-system.h"
#include <cstddef>

// FIXME: How to encode Option?

namespace Rust {
namespace Fmt {

namespace ffi {

struct RustHamster
{
  const char *ptr;
  size_t len;

  std::string to_string () const;
};

/// Enum of alignments which are supported.
enum class Alignment
{
  /// The value will be aligned to the left.
  AlignLeft,
  /// The value will be aligned to the right.
  AlignRight,
  /// The value will be aligned in the center.
  AlignCenter,
  /// The value will take on a default alignment.
  AlignUnknown,
};

/// Enum for the debug hex flags.
enum class DebugHex
{
  /// The `x` flag in `{:x?}`.
  Lower,
  /// The `X` flag in `{:X?}`.
  Upper,
};

/// Enum for the sign flags.
enum class Sign
{
  /// The `+` flag.
  Plus,
  /// The `-` flag.
  Minus,
};

/// Enum describing where an argument for a format can be located.
struct Position
{
  enum class Tag
  {
    /// The argument is implied to be located at an index
    ArgumentImplicitlyIs,
    /// The argument is located at a specific index given in the format,
    ArgumentIs,
    /// The argument has a name.
    ArgumentNamed,
  };

  struct ArgumentImplicitlyIs_Body
  {
    size_t _0;
  };

  struct ArgumentIs_Body
  {
    size_t _0;
  };

  struct ArgumentNamed_Body
  {
    RustHamster _0;
  };

  Tag tag;
  union
  {
    ArgumentImplicitlyIs_Body argument_implicitly_is;
    ArgumentIs_Body argument_is;
    ArgumentNamed_Body argument_named;
  };
};

/// Range inside of a `Span` used for diagnostics when we only have access to
/// relative positions.
struct InnerSpan
{
  size_t start;
  size_t end;
};

/// A count is used for the precision and width parameters of an integer, and
/// can reference either an argument or a literal integer.
struct Count
{
  enum class Tag
  {
    /// The count is specified explicitly.
    CountIs,
    /// The count is specified by the argument with the given name.
    CountIsName,
    /// The count is specified by the argument at the given index.
    CountIsParam,
    /// The count is specified by a star (like in `{:.*}`) that refers to the
    /// argument at the given index.
    CountIsStar,
    /// The count is implied and cannot be explicitly specified.
    CountImplied,
  };

  struct CountIs_Body
  {
    size_t _0;
  };

  struct CountIsName_Body
  {
    RustHamster _0;
    InnerSpan _1;
  };

  struct CountIsParam_Body
  {
    size_t _0;
  };

  struct CountIsStar_Body
  {
    size_t _0;
  };

  Tag tag;
  union
  {
    CountIs_Body count_is;
    CountIsName_Body count_is_name;
    CountIsParam_Body count_is_param;
    CountIsStar_Body count_is_star;
  };
};

/// Specification for the formatting of an argument in the format string.
struct FormatSpec
{
  /// Optionally specified character to fill alignment with.
  const uint32_t *fill;
  /// Span of the optionally specified fill character.
  const InnerSpan *fill_span;
  /// Optionally specified alignment.
  Alignment align;
  /// The `+` or `-` flag.
  const Sign *sign;
  /// The `#` flag.
  bool alternate;
  /// The `0` flag.
  bool zero_pad;
  /// The `x` or `X` flag. (Only for `Debug`.)
  const DebugHex *debug_hex;
  /// The integer precision to use.
  Count precision;
  /// The span of the precision formatting flag (for diagnostics).
  const InnerSpan *precision_span;
  /// The string width requested for the resulting format.
  Count width;
  /// The span of the width formatting flag (for diagnostics).
  const InnerSpan *width_span;
  /// The descriptor string representing the name of the format desired for
  /// this argument, this can be empty or any number of characters, although
  /// it is required to be one word.
  RustHamster ty;
  /// The span of the descriptor string (for diagnostics).
  const InnerSpan *ty_span;
};

/// Representation of an argument specification.
struct Argument
{
  /// Where to find this argument
  Position position;
  /// The span of the position indicator. Includes any whitespace in implicit
  /// positions (`{  }`).
  InnerSpan position_span;
  /// How to format the argument
  FormatSpec format;
};

/// A piece is a portion of the format string which represents the next part
/// to emit. These are emitted as a stream by the `Parser` class.
struct Piece
{
  enum class Tag
  {
    /// A literal string which should directly be emitted
    String,
    /// This describes that formatting should process the next argument (as
    /// specified inside) for emission.
    NextArgument,
  };

  struct String_Body
  {
    RustHamster _0;
  };

  struct NextArgument_Body
  {
    Argument _0;
  };

  Tag tag;
  union
  {
    String_Body string;
    NextArgument_Body next_argument;
  };
};

struct PieceSlice
{
  const Piece *base_ptr;
  size_t len;
  size_t cap;
};

struct RustString
{
  const unsigned char *ptr;
  size_t len;
  size_t cap;
};

struct FormatArgsHandle
{
  PieceSlice piece_slice;
  RustString rust_string;
};

extern "C" {

FormatArgsHandle
collect_pieces (const char *input, bool append_newline);

FormatArgsHandle
clone_pieces (const FormatArgsHandle &);

void destroy_pieces (FormatArgsHandle);

} // extern "C"

} // namespace ffi

struct Pieces
{
  static Pieces collect (const std::string &to_parse, bool append_newline);
  ~Pieces ();

  Pieces (const Pieces &other);
  Pieces &operator= (const Pieces &other);

  Pieces (Pieces &&other);

  const std::vector<ffi::Piece> &get_pieces () const { return pieces_vector; }

  // {
  //   slice = clone_pieces (&other.slice);
  //   to_parse = other.to_parse;

  //   return *this;
  // }

private:
  Pieces (ffi::FormatArgsHandle handle, std::vector<ffi::Piece> &&pieces_vector)
    : pieces_vector (std::move (pieces_vector)), handle (handle)
  {}

  std::vector<ffi::Piece> pieces_vector;

  // this memory is held for FFI reasons - it needs to be released and cloned
  // precisely, so try to not access it/modify it if possible. you should
  // instead work with `pieces_vector`
  ffi::FormatArgsHandle handle;
};

} // namespace Fmt
} // namespace Rust

#endif // !RUST_FMT_H
