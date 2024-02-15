// Copyright (C) 2020-2024 Free Software Foundation, Inc.

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

// Append keywords made from multiple tokens to the existing token-keyword list
#define RS_KEYWORD_LIST                                                        \
  RS_TOKEN_LIST                                                                \
  RS_TOKEN_KEYWORD_2015 (DOLLAR_CRATE, "$crate")                               \
  RS_TOKEN_KEYWORD_2015 (PATH_ROOT, "{{root}}")                                \
  RS_TOKEN_KEYWORD_2015 (STATIC_LIFETIME, "'static")                           \
  RS_TOKEN_KEYWORD_2015 (UNDERSCORE_LIFETIME, "'_")

namespace Rust {
namespace Values {

// TODO: Change this to a namespace + inline constexpr in the future
class Keywords
{
public:
  const static std::map<std::string, TokenId> keywords_tokens;

  const static std::set<std::string> keywords;
  // Rust keyword values
public:
#define RS_TOKEN(x, y)
#define RS_TOKEN_KEYWORD_2015(tok, key) static constexpr auto &tok = key;
#define RS_TOKEN_KEYWORD_2018 RS_TOKEN_KEYWORD_2015
  RS_KEYWORD_LIST
#undef RS_TOKEN_KEYWORD_2015
#undef RS_TOKEN_KEYWORD_2018
#undef RS_TOKEN
};

class WeakKeywords
{
public:
  static constexpr auto &AUTO = "auto";
  static constexpr auto &BUILTIN = "builtin";
  static constexpr auto &CATCH = "catch";
  static constexpr auto &DEFAULT = "default";
  static constexpr auto &GEN = "gen";
  static constexpr auto &MACRO_RULES = "macro_rules";
  static constexpr auto &RAW = "raw";
  static constexpr auto &UNION = "union";
  static constexpr auto &YEET = "yeet";
};

} // namespace Values
} // namespace Rust

#endif /* !RUST_KEYWORD_VALUES_H */
