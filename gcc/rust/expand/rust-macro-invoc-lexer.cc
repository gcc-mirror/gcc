#include "rust-macro-invoc-lexer.h"
#include "rust-token.h"

namespace Rust {

const_TokenPtr
MacroInvocLexer::peek_token (int n)
{
  if ((offs + n) >= token_stream.size ())
    return Token::make (END_OF_FILE, Location ());

  return token_stream.at (offs + n)->get_tok_ptr ();
}

// Advances current token to n + 1 tokens ahead of current position.
void
MacroInvocLexer::skip_token (int n)
{
  offs += (n + 1);
}

void
MacroInvocLexer::split_current_token (TokenId new_left, TokenId new_right)
{
  auto &current_token = token_stream.at (offs);
  auto current_pos = token_stream.begin () + offs;

  auto l_tok = Token::make (new_left, current_token->get_locus ());
  auto r_tok = Token::make (new_right, current_token->get_locus ());

  token_stream.erase (current_pos);

  // `insert` inserts before the specified position, so we insert the right one
  // then the left
  token_stream.insert (current_pos,
		       std::unique_ptr<AST::Token> (new AST::Token (r_tok)));
  token_stream.insert (current_pos,
		       std::unique_ptr<AST::Token> (new AST::Token (l_tok)));
}

std::vector<std::unique_ptr<AST::Token>>
MacroInvocLexer::get_token_slice (size_t start_idx, size_t end_idx) const
{
  std::vector<std::unique_ptr<AST::Token>> slice;

  rust_assert (end_idx < token_stream.size ());

  for (size_t i = start_idx; i < end_idx; i++)
    slice.emplace_back (token_stream[i]->clone_token ());

  return slice;
}

} // namespace Rust
