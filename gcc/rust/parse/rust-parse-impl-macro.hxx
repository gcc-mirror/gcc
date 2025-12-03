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

// Parses a semi-coloned (except for full block) macro invocation item.
template <typename ManagedTokenSource>
std::unique_ptr<AST::MacroInvocation>
Parser<ManagedTokenSource>::parse_macro_invocation_semi (
  AST::AttrVec outer_attrs)
{
  location_t macro_locus = lexer.peek_token ()->get_locus ();
  auto path = parse_simple_path ();
  if (!path)
    return nullptr;

  if (!skip_token (EXCLAM))
    {
      // skip after somewhere?
      return nullptr;
    }

  // save delim type to ensure it is reused later
  AST::DelimType delim_type = AST::PARENS;

  // Map tokens to DelimType
  const_TokenPtr t = lexer.peek_token ();
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
			"macro invocation semi body)",
			t->get_token_description ()));

      return nullptr;
    }
  location_t tok_tree_locus = t->get_locus ();
  lexer.skip_token ();

  // parse actual token trees
  std::vector<std::unique_ptr<AST::TokenTree>> token_trees;
  auto delim_open
    = std::unique_ptr<AST::Token> (new AST::Token (std::move (t)));
  token_trees.push_back (std::move (delim_open));

  t = lexer.peek_token ();
  // parse token trees until the initial delimiter token is found again
  while (!Parse::Utils::token_id_matches_delims (t->get_id (), delim_type)
	 && t->get_id () != END_OF_FILE)
    {
      auto tree = parse_token_tree ();
      if (!tree)
	return nullptr;

      token_trees.push_back (std::move (tree.value ()));

      t = lexer.peek_token ();
    }
  auto delim_close
    = std::unique_ptr<AST::Token> (new AST::Token (std::move (t)));
  token_trees.push_back (std::move (delim_close));

  AST::DelimTokenTree delim_tok_tree (delim_type, std::move (token_trees),
				      tok_tree_locus);
  AST::MacroInvocData invoc_data (std::move (path.value ()),
				  std::move (delim_tok_tree));

  // parse end delimiters
  t = lexer.peek_token ();
  if (Parse::Utils::token_id_matches_delims (t->get_id (), delim_type))
    {
      // tokens match opening delimiter, so skip.
      lexer.skip_token ();

      if (delim_type != AST::CURLY)
	{
	  // skip semicolon at end of non-curly macro invocation semis
	  if (!skip_token (SEMICOLON))
	    {
	      // as this is the end, allow recovery (probably) - may change

	      return AST::MacroInvocation::Regular (std::move (invoc_data),
						    std::move (outer_attrs),
						    macro_locus, true);
	    }
	}

      // DEBUG:
      rust_debug ("skipped token is '%s', next token (current peek) is '%s'",
		  t->get_token_description (),
		  lexer.peek_token ()->get_token_description ());

      return AST::MacroInvocation::Regular (std::move (invoc_data),
					    std::move (outer_attrs),
					    macro_locus, true);
    }
  else
    {
      // tokens don't match opening delimiters, so produce error
      Error error (t->get_locus (),
		   "unexpected token %qs - expecting closing delimiter %qs "
		   "(for a macro invocation semi)",
		   t->get_token_description (),
		   (delim_type == AST::PARENS
		      ? ")"
		      : (delim_type == AST::SQUARE ? "]" : "}")));
      add_error (std::move (error));

      /* return empty macro invocation despite possibly parsing mostly valid one
       * - TODO is this a good idea? */
      return nullptr;
    }
}

// Parses a non-semicoloned macro invocation (i.e. as pattern or expression).
template <typename ManagedTokenSource>
std::unique_ptr<AST::MacroInvocation>
Parser<ManagedTokenSource>::parse_macro_invocation (AST::AttrVec outer_attrs)
{
  // parse macro path
  auto macro_path = parse_simple_path ();
  if (!macro_path)
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "failed to parse macro invocation path");
      add_error (std::move (error));

      // skip?
      return nullptr;
    }

  if (!skip_token (EXCLAM))
    {
      // skip after somewhere?
      return nullptr;
    }

  // parse internal delim token tree
  auto delim_tok_tree = parse_delim_token_tree ();
  if (!delim_tok_tree)
    return nullptr;

  location_t macro_locus = macro_path->get_locus ();

  return AST::MacroInvocation::Regular (
    AST::MacroInvocData (std::move (macro_path.value ()),
			 std::move (delim_tok_tree.value ())),
    std::move (outer_attrs), macro_locus);
}

// Parses a macro rule definition - does not parse semicolons.
template <typename ManagedTokenSource>
AST::MacroRule
Parser<ManagedTokenSource>::parse_macro_rule ()
{
  location_t locus = lexer.peek_token ()->get_locus ();

  // parse macro matcher
  AST::MacroMatcher matcher = parse_macro_matcher ();

  if (matcher.is_error ())
    return AST::MacroRule::create_error (locus);

  if (!skip_token (MATCH_ARROW))
    {
      // skip after somewhere?
      return AST::MacroRule::create_error (locus);
    }

  // parse transcriber (this is just a delim token tree)
  location_t token_tree_loc = lexer.peek_token ()->get_locus ();
  auto delim_token_tree = parse_delim_token_tree ();
  if (!delim_token_tree)
    return AST::MacroRule::create_error (token_tree_loc);

  AST::MacroTranscriber transcriber (delim_token_tree.value (), token_tree_loc);

  return AST::MacroRule (std::move (matcher), std::move (transcriber), locus);
}

// Parses a macro matcher (part of a macro rule definition).
template <typename ManagedTokenSource>
AST::MacroMatcher
Parser<ManagedTokenSource>::parse_macro_matcher ()
{
  // save delim type to ensure it is reused later
  AST::DelimType delim_type = AST::PARENS;

  // DEBUG
  rust_debug ("begun parsing macro matcher");

  // Map tokens to DelimType
  const_TokenPtr t = lexer.peek_token ();
  location_t locus = t->get_locus ();
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
      add_error (Error (
	t->get_locus (),
	"unexpected token %qs - expecting delimiters (for a macro matcher)",
	t->get_token_description ()));

      return AST::MacroMatcher::create_error (t->get_locus ());
    }
  lexer.skip_token ();

  // parse actual macro matches
  std::vector<std::unique_ptr<AST::MacroMatch>> matches;
  // Set of possible preceding macro matches to make sure follow-set
  // restrictions are respected.
  // TODO: Consider using std::reference_wrapper instead of raw pointers?
  std::vector<const AST::MacroMatch *> last_matches;

  t = lexer.peek_token ();
  // parse token trees until the initial delimiter token is found again
  while (!Parse::Utils::token_id_matches_delims (t->get_id (), delim_type))
    {
      std::unique_ptr<AST::MacroMatch> match = parse_macro_match ();

      if (match == nullptr)
	{
	  Error error (
	    t->get_locus (),
	    "failed to parse macro match for macro matcher - found %qs",
	    t->get_token_description ());
	  add_error (std::move (error));

	  return AST::MacroMatcher::create_error (t->get_locus ());
	}

      if (matches.size () > 0)
	{
	  const auto *last_match = matches.back ().get ();

	  // We want to check if we are dealing with a zeroable repetition
	  bool zeroable = false;
	  if (last_match->get_macro_match_type ()
	      == AST::MacroMatch::MacroMatchType::Repetition)
	    {
	      auto repetition
		= static_cast<const AST::MacroMatchRepetition *> (last_match);

	      if (repetition->get_op ()
		  != AST::MacroMatchRepetition::MacroRepOp::ONE_OR_MORE)
		zeroable = true;
	    }

	  if (!zeroable)
	    last_matches.clear ();

	  last_matches.emplace_back (last_match);

	  for (auto last : last_matches)
	    if (!is_match_compatible (*last, *match))
	      return AST::MacroMatcher::create_error (
		match->get_match_locus ());
	}

      matches.push_back (std::move (match));

      // DEBUG
      rust_debug ("pushed back a match in macro matcher");

      t = lexer.peek_token ();
    }

  // parse end delimiters
  t = lexer.peek_token ();
  if (Parse::Utils::token_id_matches_delims (t->get_id (), delim_type))
    {
      // tokens match opening delimiter, so skip.
      lexer.skip_token ();

      return AST::MacroMatcher (delim_type, std::move (matches), locus);
    }
  else
    {
      // tokens don't match opening delimiters, so produce error
      Error error (t->get_locus (),
		   "unexpected token %qs - expecting closing delimiter %qs "
		   "(for a macro matcher)",
		   t->get_token_description (),
		   (delim_type == AST::PARENS
		      ? ")"
		      : (delim_type == AST::SQUARE ? "]" : "}")));
      add_error (std::move (error));

      /* return error macro matcher despite possibly parsing mostly correct one?
       * TODO is this the best idea? */
      return AST::MacroMatcher::create_error (t->get_locus ());
    }
}

// Parses a macro match (syntax match inside a matcher in a macro rule).
template <typename ManagedTokenSource>
std::unique_ptr<AST::MacroMatch>
Parser<ManagedTokenSource>::parse_macro_match ()
{
  // branch based on token available
  const_TokenPtr t = lexer.peek_token ();
  switch (t->get_id ())
    {
    case LEFT_PAREN:
    case LEFT_SQUARE:
    case LEFT_CURLY:
      {
	// must be macro matcher as delimited
	AST::MacroMatcher matcher = parse_macro_matcher ();
	if (matcher.is_error ())
	  {
	    Error error (lexer.peek_token ()->get_locus (),
			 "failed to parse macro matcher in macro match");
	    add_error (std::move (error));

	    return nullptr;
	  }
	return std::unique_ptr<AST::MacroMatcher> (
	  new AST::MacroMatcher (std::move (matcher)));
      }
    case DOLLAR_SIGN:
      {
	// have to do more lookahead to determine if fragment or repetition
	const_TokenPtr t2 = lexer.peek_token (1);
	switch (t2->get_id ())
	  {
	  case IDENTIFIER:
	  case UNDERSCORE:
	    // macro fragment
	    return parse_macro_match_fragment ();
	  case LEFT_PAREN:
	    // macro repetition
	    return parse_macro_match_repetition ();
	  default:
	    if (token_id_is_keyword (t2->get_id ()) && t2->get_id () != CRATE)
	      {
		// keyword as macro fragment
		return parse_macro_match_fragment ();
	      }
	    else
	      {
		// error: unrecognised
		add_error (Error (
		  t2->get_locus (),
		  "unrecognised token combination %<$%s%> at start of "
		  "macro match - did you mean %<$identifier%> or %<$(%>?",
		  t2->get_token_description ()));

		// skip somewhere?
		return nullptr;
	      }
	  }
      }
    case RIGHT_PAREN:
    case RIGHT_SQUARE:
    case RIGHT_CURLY:
      // not allowed
      add_error (Error (
	t->get_locus (),
	"closing delimiters like %qs are not allowed at the start of a macro "
	"match",
	t->get_token_description ()));

      // skip somewhere?
      return nullptr;
    default:
      // just the token
      lexer.skip_token ();
      return std::unique_ptr<AST::Token> (new AST::Token (std::move (t)));
    }
}

// Parses a fragment macro match.
template <typename ManagedTokenSource>
std::unique_ptr<AST::MacroMatchFragment>
Parser<ManagedTokenSource>::parse_macro_match_fragment ()
{
  location_t fragment_locus = lexer.peek_token ()->get_locus ();
  skip_token (DOLLAR_SIGN);

  Identifier ident;
  auto identifier = lexer.peek_token ();
  if (identifier->get_id () == UNDERSCORE)
    ident = {Values::Keywords::UNDERSCORE, identifier->get_locus ()};
  else
    ident = {identifier};

  if (ident.empty ())
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "missing identifier in macro match fragment");
      add_error (std::move (error));

      return nullptr;
    }
  skip_token (identifier->get_id ());

  if (!skip_token (COLON))
    {
      // skip after somewhere?
      return nullptr;
    }

  // get MacroFragSpec for macro
  const_TokenPtr t = expect_token (IDENTIFIER);
  if (t == nullptr)
    return nullptr;

  AST::MacroFragSpec frag
    = AST::MacroFragSpec::get_frag_spec_from_str (t->get_str ());
  if (frag.is_error ())
    {
      Error error (t->get_locus (),
		   "invalid fragment specifier %qs in fragment macro match",
		   t->get_str ().c_str ());
      add_error (std::move (error));

      return nullptr;
    }

  return std::unique_ptr<AST::MacroMatchFragment> (
    new AST::MacroMatchFragment (std::move (ident), frag, fragment_locus));
}

// Parses a repetition macro match.
template <typename ManagedTokenSource>
std::unique_ptr<AST::MacroMatchRepetition>
Parser<ManagedTokenSource>::parse_macro_match_repetition ()
{
  skip_token (DOLLAR_SIGN);
  skip_token (LEFT_PAREN);

  std::vector<std::unique_ptr<AST::MacroMatch>> matches;

  // parse required first macro match
  std::unique_ptr<AST::MacroMatch> initial_match = parse_macro_match ();
  if (initial_match == nullptr)
    {
      Error error (
	lexer.peek_token ()->get_locus (),
	"could not parse required first macro match in macro match repetition");
      add_error (std::move (error));

      // skip after somewhere?
      return nullptr;
    }
  matches.push_back (std::move (initial_match));

  // parse optional later macro matches
  const_TokenPtr t = lexer.peek_token ();
  while (t->get_id () != RIGHT_PAREN)
    {
      std::unique_ptr<AST::MacroMatch> match = parse_macro_match ();

      if (match == nullptr)
	{
	  Error error (lexer.peek_token ()->get_locus (),
		       "failed to parse macro match in macro match repetition");
	  add_error (std::move (error));

	  return nullptr;
	}

      matches.push_back (std::move (match));

      t = lexer.peek_token ();
    }

  if (!skip_token (RIGHT_PAREN))
    {
      // skip after somewhere?
      return nullptr;
    }

  t = lexer.peek_token ();
  // see if separator token exists
  std::unique_ptr<AST::Token> separator = nullptr;
  switch (t->get_id ())
    {
    // repetition operators
    case ASTERISK:
    case PLUS:
    case QUESTION_MARK:
    // delimiters
    case LEFT_PAREN:
    case LEFT_CURLY:
    case LEFT_SQUARE:
    case RIGHT_PAREN:
    case RIGHT_CURLY:
    case RIGHT_SQUARE:
      // separator does not exist, so still null and don't skip token
      break;
    default:
      // separator does exist
      separator = std::unique_ptr<AST::Token> (new AST::Token (std::move (t)));
      lexer.skip_token ();
      break;
    }

  // parse repetition operator
  t = lexer.peek_token ();
  AST::MacroMatchRepetition::MacroRepOp op = AST::MacroMatchRepetition::NONE;
  switch (t->get_id ())
    {
    case ASTERISK:
      op = AST::MacroMatchRepetition::ANY;
      lexer.skip_token ();
      break;
    case PLUS:
      op = AST::MacroMatchRepetition::ONE_OR_MORE;
      lexer.skip_token ();
      break;
    case QUESTION_MARK:
      op = AST::MacroMatchRepetition::ZERO_OR_ONE;
      lexer.skip_token ();

      if (separator != nullptr)
	{
	  add_error (
	    Error (separator->get_locus (),
		   "the %<?%> macro repetition operator does not take a "
		   "separator"));
	  separator = nullptr;
	}

      break;
    default:
      add_error (
	Error (t->get_locus (),
	       "expected macro repetition operator (%<*%>, %<+%>, or %<?%>) in "
	       "macro match - found %qs",
	       t->get_token_description ()));

      // skip after somewhere?
      return nullptr;
    }

  return std::unique_ptr<AST::MacroMatchRepetition> (
    new AST::MacroMatchRepetition (std::move (matches), op,
				   std::move (separator), t->get_locus ()));
}

/* Parses a macro invocation with a path in expression already parsed (but not
 * '!' token). */
template <typename ManagedTokenSource>
std::unique_ptr<AST::MacroInvocation>
Parser<ManagedTokenSource>::parse_macro_invocation_partial (
  AST::PathInExpression path, AST::AttrVec outer_attrs,
  ParseRestrictions restrictions)
{
  // macro invocation
  if (!skip_token (EXCLAM))
    {
      return nullptr;
    }

  // convert PathInExpression to SimplePath - if this isn't possible, error
  AST::SimplePath converted_path = path.as_simple_path ();
  if (converted_path.is_empty ())
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "failed to parse simple path in macro invocation");
      add_error (std::move (error));

      return nullptr;
    }

  auto tok_tree = parse_delim_token_tree ();
  if (!tok_tree)
    return nullptr;

  rust_debug ("successfully parsed macro invocation (via partial)");

  location_t macro_locus = converted_path.get_locus ();

  return AST::MacroInvocation::Regular (
    AST::MacroInvocData (std::move (converted_path),
			 std::move (tok_tree.value ())),
    std::move (outer_attrs), macro_locus);
}

} // namespace Rust
