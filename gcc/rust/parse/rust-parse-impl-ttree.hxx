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

/* DO NOT INCLUDE ANYWHERE - this is automatically included
 *   by rust-parse-impl.h
 * This is also the reason why there are no include guards. */

#include "rust-parse.h"
#include "rust-parse-error.h"
#include "expected.h"

namespace Rust {

/* Parses a TokenTree syntactical production. This is either a delimited token
 * tree or a non-delimiter token. */
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::TokenTree>, Parse::Error::TokenTree>
Parser<ManagedTokenSource>::parse_token_tree ()
{
  const_TokenPtr t = lexer.peek_token ();

  switch (t->get_id ())
    {
    case LEFT_PAREN:
    case LEFT_SQUARE:
    case LEFT_CURLY:
      {
	// Parse delimited token tree
	auto delim_token_tree = parse_delim_token_tree ();
	if (!delim_token_tree)
	  return Parse::Error::TokenTree::
	    make_malformed_delimited_token_tree ();

	// TODO: use move rather than copy constructor
	return std::unique_ptr<AST::DelimTokenTree> (
	  new AST::DelimTokenTree (delim_token_tree.value ()));
      }
    case RIGHT_PAREN:
    case RIGHT_SQUARE:
    case RIGHT_CURLY:
      // error - should not be called when this a token
      add_error (Error (t->get_locus (), "unexpected closing delimiter %qs",
			t->get_token_description ()));

      add_error (Error (Error::Kind::Hint, t->get_locus (),
			"token tree requires either paired delimiters or "
			"non-delimiter tokens"));

      lexer.skip_token ();
      return Parse::Error::TokenTree::make_malformed ();
    default:
      // parse token itself as TokenTree
      lexer.skip_token ();
      return std::unique_ptr<AST::Token> (new AST::Token (std::move (t)));
    }
}

// Parses a delimited token tree
template <typename ManagedTokenSource>
tl::expected<AST::DelimTokenTree, Parse::Error::DelimTokenTree>
Parser<ManagedTokenSource>::parse_delim_token_tree ()
{
  const_TokenPtr t = lexer.peek_token ();
  lexer.skip_token ();
  location_t initial_loc = t->get_locus ();

  // save delim type to ensure it is reused later
  AST::DelimType delim_type = AST::PARENS;

  // Map tokens to DelimType
  switch (t->get_id ())
    {
    case LEFT_PAREN:
      delim_type = AST::PARENS;
      break;
    case LEFT_SQUARE:
      delim_type = AST::SQUARE;
      break;
    case LEFT_CURLY:
      delim_type = AST::CURLY;
      break;
    default:
      add_error (Error (t->get_locus (),
			"unexpected token %qs - expecting delimiters (for a "
			"delimited token tree)",
			t->get_token_description ()));

      return Parse::Error::DelimTokenTree::make_expected_delimiter ();
    }

  // parse actual token tree vector - 0 or more
  std::vector<std::unique_ptr<AST::TokenTree>> token_trees_in_tree;
  auto delim_open
    = std::unique_ptr<AST::Token> (new AST::Token (std::move (t)));
  token_trees_in_tree.push_back (std::move (delim_open));

  // repeat loop until finding the matching delimiter
  t = lexer.peek_token ();
  while (!Parse::Utils::token_id_matches_delims (t->get_id (), delim_type)
	 && t->get_id () != END_OF_FILE)
    {
      auto tok_tree = parse_token_tree ();
      if (!tok_tree)
	return Parse::Error::DelimTokenTree::make_invalid_token_tree ();

      token_trees_in_tree.push_back (std::move (tok_tree.value ()));

      // lexer.skip_token();
      t = lexer.peek_token ();
    }
  auto delim_close
    = std::unique_ptr<AST::Token> (new AST::Token (std::move (t)));
  token_trees_in_tree.push_back (std::move (delim_close));

  AST::DelimTokenTree token_tree (delim_type, std::move (token_trees_in_tree),
				  initial_loc);

  // parse end delimiters
  t = lexer.peek_token ();

  if (Parse::Utils::token_id_matches_delims (t->get_id (), delim_type))
    {
      // tokens match opening delimiter, so skip.
      lexer.skip_token ();
      return token_tree;
    }
  else
    {
      // tokens don't match opening delimiters, so produce error
      Error error (t->get_locus (),
		   "unexpected token %qs - expecting closing delimiter %qs "
		   "(for a delimited token tree)",
		   t->get_token_description (),
		   (delim_type == AST::PARENS
		      ? ")"
		      : (delim_type == AST::SQUARE ? "]" : "}")));
      add_error (std::move (error));

      return Parse::Error::DelimTokenTree::make_mismatched_delimiters ();
    }
}

} // namespace Rust
