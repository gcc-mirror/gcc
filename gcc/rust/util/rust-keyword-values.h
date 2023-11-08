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

#ifndef RUST_KEYWORD_VALUES_H
#define RUST_KEYWORD_VALUES_H

#include "rust-token.h"

namespace Rust {
namespace Values {

// TODO: Change this to a namespace + inline constexpr in the future
class Keywords
{
public:
  const static std::map<std::string, TokenId> keywords;

  // Rust keyword values
public:
#define RS_TOKEN(x, y)
#define RS_TOKEN_KEYWORD(tok, key) static constexpr auto &tok = key;
  RS_TOKEN_LIST
#undef RS_TOKEN_KEYWORD
#undef RS_TOKEN
};

} // namespace Values
} // namespace Rust

#endif /* !RUST_KEYWORD_VALUES_H */
