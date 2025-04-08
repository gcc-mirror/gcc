// Copyright (C) 2025 Free Software Foundation, Inc.

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

#include "rust-token-tree-desugar.h"
#include "rust-ast.h"
#include "rust-token.h"

namespace Rust {
namespace AST {

DelimTokenTree
TokenTreeDesugar::go (DelimTokenTree &tts)
{
  tts.accept_vis (*this);

  return DelimTokenTree (tts.get_delim_type (), std::move (desugared),
			 tts.get_locus ());
}

void
TokenTreeDesugar::append (TokenPtr &&new_token)
{
  desugared.emplace_back (std::make_unique<Token> (std::move (new_token)));
}

void
TokenTreeDesugar::append (std::unique_ptr<TokenTree> &&new_token)
{
  desugared.emplace_back (std::move (new_token));
}

void
TokenTreeDesugar::visit (Token &tts)
{
  if (tts.get_id () == TokenId::OUTER_DOC_COMMENT
      || tts.get_id () == TokenId::INNER_DOC_COMMENT)
    {
      append (Rust::Token::make (TokenId::HASH, tts.get_locus ()));

      if (tts.get_id () == TokenId::INNER_DOC_COMMENT)
	append (Rust::Token::make (EXCLAM, tts.get_locus ()));

      append (Rust::Token::make (TokenId::LEFT_SQUARE, tts.get_locus ()));
      append (Rust::Token::make_identifier (tts.get_locus (), "doc"));
      append (Rust::Token::make (TokenId::EQUAL, tts.get_locus ()));
      append (Rust::Token::make_string (tts.get_locus (),
					std::string (tts.get_str ())));
      append (Rust::Token::make (TokenId::RIGHT_SQUARE, tts.get_locus ()));
    }
  else
    {
      append (tts.clone_token ());
    }
}

}; // namespace AST
}; // namespace Rust
