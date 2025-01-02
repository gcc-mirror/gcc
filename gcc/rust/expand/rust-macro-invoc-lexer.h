// Copyright (C) 2020-2025 Free Software Foundation, Inc.

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
template <class T> class MacroInvocLexerBase
{
public:
  MacroInvocLexerBase (std::vector<T> stream)
    : offs (0), token_stream (std::move (stream))
  {}

  // Advances current token to n + 1 tokens ahead of current position.
  void skip_token (int n) { offs += (n + 1); }

  // Skips the current token.
  void skip_token () { skip_token (0); }

  std::string get_filename () const
  {
    // FIXME
    rust_unreachable ();
    return "FIXME";
  }

  size_t get_offs () const { return offs; }

protected:
  size_t offs;
  std::vector<T> token_stream;
};

class MacroInvocLexer : public MacroInvocLexerBase<std::unique_ptr<AST::Token>>
{
public:
  MacroInvocLexer (std::vector<std::unique_ptr<AST::Token>> stream)
    : MacroInvocLexerBase (std::move (stream))
  {}

  // Returns token n tokens ahead of current position.
  const_TokenPtr peek_token (int n);

  // Peeks the current token.
  const_TokenPtr peek_token () { return peek_token (0); }

  // Splits the current token into two. Intended for use with nested generics
  // closes (i.e. T<U<X>> where >> is wrongly lexed as one token). Note that
  // this will only work with "simple" tokens like punctuation.
  void split_current_token (TokenId new_left, TokenId new_right);

  void split_current_token (std::vector<TokenPtr> new_tokens);

  std::vector<std::unique_ptr<AST::Token>>
  get_token_slice (size_t start_idx, size_t end_idx) const;
};
} // namespace Rust

#endif // RUST_MACRO_INVOC_LEXER_H
