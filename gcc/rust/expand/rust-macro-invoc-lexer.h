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

#ifndef RUST_MACRO_INVOC_LEXER_H
#define RUST_MACRO_INVOC_LEXER_H

#include "rust-ast.h"

namespace Rust {
class MacroInvocLexer
{
public:
  MacroInvocLexer (std::vector<std::unique_ptr<AST::Token>> stream)
    : offs (0), token_stream (std::move (stream))
  {}

  // Returns token n tokens ahead of current position.
  const_TokenPtr peek_token (int n);

  // Peeks the current token.
  const_TokenPtr peek_token () { return peek_token (0); }

  // Advances current token to n + 1 tokens ahead of current position.
  void skip_token (int n);

  // Skips the current token.
  void skip_token () { skip_token (0); }

  // Splits the current token into two. Intended for use with nested generics
  // closes (i.e. T<U<X>> where >> is wrongly lexed as one token). Note that
  // this will only work with "simple" tokens like punctuation.
  void split_current_token (TokenId new_left, TokenId new_right);

  std::string get_filename () const
  {
    // FIXME
    gcc_unreachable ();
    return "FIXME";
  }

  size_t get_offs () const { return offs; }

  std::vector<std::unique_ptr<AST::Token>>
  get_token_slice (size_t start_idx, size_t end_idx) const;

private:
  size_t offs;
  std::vector<std::unique_ptr<AST::Token>> token_stream;
};
} // namespace Rust

#endif // RUST_MACRO_INVOC_LEXER_H
