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

#include "rust-fmt.h"

namespace Rust {
tl::expected<Fmt, Fmt::Error>
Fmt::parse_fmt_string (Fmt::Input input)
{
  return Fmt ();
}

tl::expected<Fmt::Result<tl::optional<Fmt::Format>>, Fmt::Error>
Fmt::maybe_format (Fmt::Input input)
{
  tl::optional<Fmt::Format> none = tl::nullopt;

  return Fmt::Result (input, none);
}

tl::expected<Fmt::Result<Fmt::Format>, Fmt::Error>
Fmt::format (Input input)
{
  return Fmt::Result (input, Format ());
}

tl::expected<Fmt::Result<Fmt::Argument>, Fmt::Error>
Fmt::argument (Input input)
{
  return Fmt::Result (input, Argument ());
}

tl::expected<Fmt::Result<Fmt::FormatSpec>, Fmt::Error>
Fmt::format_spec (Input input)
{
  return Fmt::Result (input, FormatSpec ());
}

tl::expected<Fmt::Result<Fmt::Fill>, Fmt::Error>
Fmt::fill (Input input)
{
  return Fmt::Result (input, Fill ());
}

tl::expected<Fmt::Result<Fmt::Align>, Fmt::Error>
Fmt::align (Input input)
{
  switch (input[0])
    {
    case '<':
      return Fmt::Result (input.substr (1), Align::Left);
    case '^':
      return Fmt::Result (input.substr (1), Align::Top);
    case '>':
      return Fmt::Result (input.substr (1), Align::Right);
    default:
      // TODO: Store the character here
      // TODO: Can we have proper error locations?
      // TODO: Maybe we should use a Rust::Literal string instead of a string
      return tl::make_unexpected (Error::Align);
    }
}

tl::expected<Fmt::Result<Fmt::Sign>, Fmt::Error>
Fmt::sign (Input input)
{
  switch (input[0])
    {
    case '+':
      return Fmt::Result (input.substr (1), Sign::Plus);
    case '-':
      return Fmt::Result (input.substr (1), Sign::Minus);
    default:
      // TODO: Store the character here
      // TODO: Can we have proper error locations?
      // TODO: Maybe we should use a Rust::Literal string instead of a string
      return tl::make_unexpected (Error::Sign);
    }
}

} // namespace Rust
