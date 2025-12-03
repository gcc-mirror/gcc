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

namespace Rust {

// "Unexpected token" panic mode - flags gcc error at unexpected token
// TODO: seems to be unused, remove?
template <typename ManagedTokenSource>
void
Parser<ManagedTokenSource>::unexpected_token (const_TokenPtr t)
{
  Error error (t->get_locus (), "unexpected token %qs",
	       t->get_token_description ());
  add_error (std::move (error));
}

/* Crappy "error recovery" performed after error by skipping tokens until a
 * semi-colon is found */
template <typename ManagedTokenSource>
void
Parser<ManagedTokenSource>::skip_after_semicolon ()
{
  const_TokenPtr t = lexer.peek_token ();

  while (t->get_id () != END_OF_FILE && t->get_id () != SEMICOLON)
    {
      lexer.skip_token ();
      t = lexer.peek_token ();
    }

  if (t->get_id () == SEMICOLON)
    lexer.skip_token ();
}

/* Skips the current token */
template <typename ManagedTokenSource>
void
Parser<ManagedTokenSource>::skip_token ()
{
  lexer.skip_token ();
}

/* Checks if current token has inputted id - skips it and returns true if so,
 * diagnoses an error and returns false otherwise. */
template <typename ManagedTokenSource>
bool
Parser<ManagedTokenSource>::skip_token (TokenId token_id)
{
  return expect_token (token_id) != const_TokenPtr ();
}

/* Checks if current token is similar to inputted token - skips it and returns
 * true if so, diagnoses an error and returns false otherwise. */
template <typename ManagedTokenSource>
bool
Parser<ManagedTokenSource>::skip_token (const_TokenPtr token)
{
  return expect_token (token) != const_TokenPtr ();
}

/* Checks if current token has inputted id - skips it and returns true if so,
 * returns false otherwise without diagnosing an error */
template <typename ManagedTokenSource>
bool
Parser<ManagedTokenSource>::maybe_skip_token (TokenId token_id)
{
  if (lexer.peek_token ()->get_id () != token_id)
    return false;
  else
    return skip_token (token_id);
}

/* Checks the current token - if id is same as expected, skips and returns it,
 * otherwise diagnoses error and returns null. */
template <typename ManagedTokenSource>
const_TokenPtr
Parser<ManagedTokenSource>::expect_token (TokenId token_id)
{
  const_TokenPtr t = lexer.peek_token ();
  if (t->get_id () == token_id)
    {
      lexer.skip_token ();
      return t;
    }
  else
    {
      Error error (t->get_locus (), "expecting %qs but %qs found",
		   get_token_description (token_id),
		   t->get_token_description ());
      add_error (std::move (error));

      return const_TokenPtr ();
    }
}

/* Checks the current token - if same as expected, skips and returns it,
 * otherwise diagnoses error and returns null. */
template <typename ManagedTokenSource>
const_TokenPtr
Parser<ManagedTokenSource>::expect_token (const_TokenPtr token_expect)
{
  const_TokenPtr t = lexer.peek_token ();
  if (t->get_id () == token_expect->get_id ()
      && (!t->should_have_str () || t->get_str () == token_expect->get_str ()))
    {
      lexer.skip_token ();
      return t;
    }
  else
    {
      Error error (t->get_locus (), "expecting %qs but %qs found",
		   token_expect->get_token_description (),
		   t->get_token_description ());
      add_error (std::move (error));

      return const_TokenPtr ();
    }
}

// Skips all tokens until EOF or }. Don't use.
template <typename ManagedTokenSource>
void
Parser<ManagedTokenSource>::skip_after_end ()
{
  const_TokenPtr t = lexer.peek_token ();

  while (t->get_id () != END_OF_FILE && t->get_id () != RIGHT_CURLY)
    {
      lexer.skip_token ();
      t = lexer.peek_token ();
    }

  if (t->get_id () == RIGHT_CURLY)
    {
      lexer.skip_token ();
    }
}

/* A slightly more aware error-handler that skips all tokens until it reaches
 * the end of the block scope (i.e. when left curly brackets = right curly
 * brackets). Note: assumes currently in the middle of a block. Use
 * skip_after_next_block to skip based on the assumption that the block
 * has not been entered yet. */
template <typename ManagedTokenSource>
void
Parser<ManagedTokenSource>::skip_after_end_block ()
{
  const_TokenPtr t = lexer.peek_token ();
  int curly_count = 1;

  while (curly_count > 0 && t->get_id () != END_OF_FILE)
    {
      switch (t->get_id ())
	{
	case LEFT_CURLY:
	  curly_count++;
	  break;
	case RIGHT_CURLY:
	  curly_count--;
	  break;
	default:
	  break;
	}
      lexer.skip_token ();
      t = lexer.peek_token ();
    }
}

/* Skips tokens until the end of the next block. i.e. assumes that the block
 * has not been entered yet. */
template <typename ManagedTokenSource>
void
Parser<ManagedTokenSource>::skip_after_next_block ()
{
  const_TokenPtr t = lexer.peek_token ();

  // initial loop - skip until EOF if no left curlies encountered
  while (t->get_id () != END_OF_FILE && t->get_id () != LEFT_CURLY)
    {
      lexer.skip_token ();

      t = lexer.peek_token ();
    }

  // if next token is left, skip it and then skip after the block ends
  if (t->get_id () == LEFT_CURLY)
    {
      lexer.skip_token ();

      skip_after_end_block ();
    }
  // otherwise, do nothing as EOF
}

/* Skips all tokens until ] (the end of an attribute) - does not skip the ]
 * (as designed for attribute body use) */
template <typename ManagedTokenSource>
void
Parser<ManagedTokenSource>::skip_after_end_attribute ()
{
  const_TokenPtr t = lexer.peek_token ();

  while (t->get_id () != RIGHT_SQUARE && t->get_id () != END_OF_FILE)
    {
      lexer.skip_token ();
      t = lexer.peek_token ();
    }

  // Don't skip the RIGHT_SQUARE token
}

// Returns true if the next token is END, ELSE, or EOF;
template <typename ManagedTokenSource>
bool
Parser<ManagedTokenSource>::done_end_or_else ()
{
  const_TokenPtr t = lexer.peek_token ();
  return (t->get_id () == RIGHT_CURLY || t->get_id () == ELSE
	  || t->get_id () == END_OF_FILE);
}

// Returns true if the next token is END or EOF.
template <typename ManagedTokenSource>
bool
Parser<ManagedTokenSource>::done_end ()
{
  const_TokenPtr t = lexer.peek_token ();
  return (t->get_id () == RIGHT_CURLY || t->get_id () == END_OF_FILE);
}

} // namespace Rust
