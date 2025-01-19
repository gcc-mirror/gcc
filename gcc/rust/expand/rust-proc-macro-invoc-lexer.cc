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

#include "rust-proc-macro-invoc-lexer.h"
#include "rust-token.h"

namespace Rust {

const_TokenPtr
ProcMacroInvocLexer::peek_token (int n)
{
  if ((offs + n) >= token_stream.size ())
    return Token::make (END_OF_FILE, UNDEF_LOCATION);

  return token_stream.at (offs + n);
}

void
ProcMacroInvocLexer::split_current_token (TokenId new_left, TokenId new_right)
{
  auto &current_token = token_stream.at (offs);
  auto current_pos = token_stream.begin () + offs;

  auto l_tok = Token::make (new_left, current_token->get_locus ());
  auto r_tok = Token::make (new_right, current_token->get_locus ());

  token_stream.erase (current_pos);

  // `insert` inserts before the specified position, so we insert the right one
  // then the left
  token_stream.insert (current_pos, l_tok);
  token_stream.insert (current_pos, r_tok);
}

void
ProcMacroInvocLexer::split_current_token (std::vector<TokenPtr> new_tokens)
{
  rust_assert (new_tokens.size () > 0);

  auto current_pos = token_stream.begin () + offs;

  token_stream.erase (current_pos);

  for (size_t i = 1; i < new_tokens.size (); i++)
    {
      token_stream.insert (current_pos + i, new_tokens[i]);
    }
}

} // namespace Rust
