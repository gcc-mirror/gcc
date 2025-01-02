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

#ifndef RUST_PROC_MACRO_INVOC_LEXER_H
#define RUST_PROC_MACRO_INVOC_LEXER_H

#include "rust-lex.h"
#include "rust-macro-invoc-lexer.h"

namespace Rust {
class ProcMacroInvocLexer : public MacroInvocLexerBase<const_TokenPtr>
{
public:
  ProcMacroInvocLexer (std::vector<const_TokenPtr> stream)
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
};
} // namespace Rust

#endif /* ! RUST_PROC_MACRO_INVOC_LEXER_H */
