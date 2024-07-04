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

#include "rust-keyword-values.h"
#include "rust-token.h"

namespace Rust {
namespace Values {

// TODO: Can't we do this inline ?
static std::map<std::string, TokenId>
get_keywords ()
{
  std::map<std::string, TokenId> m = {
#define RS_TOKEN(x, y)
#define RS_TOKEN_KEYWORD_2015(tok, key) {key, tok},
#define RS_TOKEN_KEYWORD_2018 RS_TOKEN_KEYWORD_2015
    RS_TOKEN_LIST
#undef RS_TOKEN_KEYWORD_2015
#undef RS_TOKEN_KEYWORD_2018
#undef RS_TOKEN
  };
  return m;
}

const std::map<std::string, TokenId> Keywords::keywords_tokens
  = get_keywords ();

const std::set<std::string> Keywords::keywords = {
#define RS_TOKEN(x, y)
#define RS_TOKEN_KEYWORD_2015(tok, key) {key},
#define RS_TOKEN_KEYWORD_2018 RS_TOKEN_KEYWORD_2015
  RS_KEYWORD_LIST
#undef RS_TOKEN_KEYWORD_2015
#undef RS_TOKEN_KEYWORD_2018
#undef RS_TOKEN
};

} // namespace Values
} // namespace Rust
