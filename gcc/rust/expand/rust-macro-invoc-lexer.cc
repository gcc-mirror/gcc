#include "rust-macro-invoc-lexer.h"

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
MacroInvocLexer::split_current_token (TokenId new_left __attribute__ ((unused)),
				      TokenId new_right
				      __attribute__ ((unused)))
{
  // FIXME
  gcc_unreachable ();
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
