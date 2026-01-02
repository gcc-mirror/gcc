// Copyright (C) 2023-2026 Free Software Foundation, Inc.

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
#include "optional.h"

// PR122498 "rust-enabled bootstrap is broken after r16-4897"
#pragma GCC diagnostic warning "-Warray-bounds"

namespace Rust {
namespace Fmt {

namespace ffi {

extern "C" {

unsigned char *rust_ffi_alloc (size_t count, size_t elem_size, size_t align);

void rust_ffi_dealloc (unsigned char *data, size_t count, size_t elem_size,
		       size_t align);

} // extern "C"

template <typename T> class FFIVec
{
  T *data;
  size_t len;
  size_t cap;

public:
  FFIVec () : data ((T *) alignof (T)), len (0), cap (0) {}

  FFIVec (const FFIVec &) = delete;
  FFIVec &operator= (const FFIVec &) = delete;

  FFIVec (FFIVec &&other) : data (other.data), len (other.len), cap (other.cap)
  {
    other.data = (T *) alignof (T);
    other.len = 0;
    other.cap = 0;
  }

  FFIVec &operator= (FFIVec &&other)
  {
    this->~FFIVec ();
    new (this) FFIVec (std::move (other));
    return *this;
  }

  ~FFIVec ()
  {
    // T can't be zero-sized
    if (cap)
      rust_ffi_dealloc ((unsigned char *) data, cap, sizeof (T), alignof (T));
  }

  size_t size () const { return len; }

  const T &operator[] (size_t idx) const
  {
    rust_assert (idx <= len);
    return data[idx];
  }

  T *begin () { return data; }
  const T *begin () const { return data; }
  T *end () { return data + len; }
  const T *end () const { return data + len; }
};

// https://github.com/rust-lang/rfcs/blob/master/text/2195-really-tagged-unions.md
template <typename T,
	  typename =
	    typename std::enable_if<std::is_standard_layout<T>::value>::type>
class FFIOpt
{
public:
  template <typename U>
  FFIOpt (U &&val) : some{Some::KIND, std::forward<U> (val)}
  {}

  FFIOpt () : none{None::KIND} {}

  FFIOpt (const FFIOpt &other)
  {
    if (other.has_value ())
      new (&some) Some{Some::KIND, other.some.val};
    else
      new (&none) None{None::KIND};
  }

  FFIOpt (FFIOpt &&other)
  {
    if (other.has_value ())
      new (&some) Some{Some::KIND, std::move (other.some.val)};
    else
      new (&none) None{None::KIND};
  }

  ~FFIOpt ()
  {
    if (has_value ())
      some.~Some ();
    else
      none.~None ();
  }

  FFIOpt &operator= (const FFIOpt &other)
  {
    this->~FFIOpt ();
    new (this) FFIOpt (other);
    return *this;
  }

  FFIOpt &operator= (FFIOpt &&other)
  {
    this->~FFIOpt ();
    new (this) FFIOpt (std::move (other));
    return *this;
  }

  tl::optional<std::reference_wrapper<T>> get_opt ()
  {
    if (has_value ())
      return std::ref (some.val);
    else
      return tl::nullopt;
  }

  tl::optional<std::reference_wrapper<const T>> get_opt () const
  {
    if (has_value ())
      return std::ref (some.val);
    else
      return tl::nullopt;
  }

  bool has_value () const { return some.kind == Some::KIND; }

  operator bool () const { return has_value (); }

private:
  struct Some
  {
    static constexpr uint8_t KIND = 0;
    uint8_t kind;
    T val;
  };

  struct None
  {
    static constexpr uint8_t KIND = 1;
    uint8_t kind;
  };

  union
  {
    Some some;
    None none;
  };
};

struct RustHamster
{
  const char *ptr;
  size_t len;

  std::string to_string () const;

  explicit RustHamster (const std::string &str)
    : ptr (str.data ()), len (str.size ())
  {}
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
  FFIOpt<uint32_t> fill;
  /// Span of the optionally specified fill character.
  FFIOpt<InnerSpan> fill_span;
  /// Optionally specified alignment.
  Alignment align;
  /// The `+` or `-` flag.
  FFIOpt<Sign> sign;
  /// The `#` flag.
  bool alternate;
  /// The `0` flag.
  bool zero_pad;
  /// The `x` or `X` flag. (Only for `Debug`.)
  FFIOpt<DebugHex> debug_hex;
  /// The integer precision to use.
  Count precision;
  /// The span of the precision formatting flag (for diagnostics).
  FFIOpt<InnerSpan> precision_span;
  /// The string width requested for the resulting format.
  Count width;
  /// The span of the width formatting flag (for diagnostics).
  FFIOpt<InnerSpan> width_span;
  /// The descriptor string representing the name of the format desired for
  /// this argument, this can be empty or any number of characters, although
  /// it is required to be one word.
  RustHamster ty;
  /// The span of the descriptor string (for diagnostics).
  FFIOpt<InnerSpan> ty_span;
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

enum ParseMode
{
  Format = 0,
  InlineAsm,
};

extern "C" {

FFIVec<Piece> collect_pieces (RustHamster input, bool append_newline,
			      ParseMode parse_mode);

FFIVec<Piece> clone_pieces (const FFIVec<Piece> &);

} // extern "C"

} // namespace ffi

struct Pieces
{
  static Pieces collect (const std::string &to_parse, bool append_newline,
			 ffi::ParseMode parse_mode);

  const ffi::FFIVec<ffi::Piece> &get_pieces () const { return data->second; }

private:
  Pieces (std::string str, ffi::FFIVec<ffi::Piece> pieces)
    : data (
      std::make_shared<decltype (data)::element_type> (std::move (str),
						       std::move (pieces)))
  {}

  // makes copying simpler
  // also, we'd need to keep the parsed string in a shared_ptr anyways
  // since we store pointers into the parsed string
  std::shared_ptr<std::pair<std::string, ffi::FFIVec<ffi::Piece>>> data;
};

} // namespace Fmt
} // namespace Rust

#endif // !RUST_FMT_H
