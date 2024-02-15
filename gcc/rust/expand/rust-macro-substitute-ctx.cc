// Copyright (C) 2020-2024 Free Software Foundation, Inc.

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

#include "rust-macro-substitute-ctx.h"

namespace Rust {

bool
SubstituteCtx::substitute_metavar (
  std::unique_ptr<AST::Token> &metavar,
  std::vector<std::unique_ptr<AST::Token>> &expanded)
{
  auto metavar_name = metavar->get_str ();

  auto it = fragments.find (metavar_name);
  if (it == fragments.end ())
    {
      // fail to substitute

      // HACK: substitute ($ crate) => (crate)
      if (metavar->get_id () != CRATE)
	return false;

      expanded.push_back (metavar->clone_token ());
      return true;
    }
  else
    {
      // If we are expanding a metavar which has a lof of matches, we are
      // currently expanding a repetition metavar - not a simple metavar. We
      // need to error out and inform the user.
      // Associated test case for an example: compile/macro-issue1224.rs
      if (!it->second->is_single_fragment ())
	{
	  rust_error_at (metavar->get_locus (),
			 "metavariable is still repeating at this depth");
	  rust_inform (
	    metavar->get_locus (),
	    "you probably forgot the repetition operator: %<%s%s%s%>", "$(",
	    metavar->as_string ().c_str (), ")*");
	  return true;
	}

      // We only care about the vector when expanding repetitions.
      // Just access the first element of the vector.
      auto &frag = it->second->get_single_fragment ();
      for (size_t offs = frag.token_offset_begin; offs < frag.token_offset_end;
	   offs++)
	{
	  auto &tok = input.at (offs);
	  expanded.push_back (tok->clone_token ());
	}
    }

  return true;
}

bool
SubstituteCtx::check_repetition_amount (size_t pattern_start,
					size_t pattern_end,
					size_t &expected_repetition_amount)
{
  bool first_fragment_found = false;
  bool is_valid = true;

  for (size_t i = pattern_start; i < pattern_end; i++)
    {
      if (macro.at (i)->get_id () == DOLLAR_SIGN)
	{
	  auto &frag_token = macro.at (i + 1);
	  if (token_id_is_keyword (frag_token->get_id ())
	      || frag_token->get_id () == IDENTIFIER)
	    {
	      auto it = fragments.find (frag_token->get_str ());
	      if (it == fragments.end ())
		{
		  // If the repetition is not anything we know (ie no declared
		  // metavars, or metavars which aren't present in the
		  // fragment), we can just error out. No need to paste the
		  // tokens as if nothing had happened.
		  rust_error_at (frag_token->get_locus (),
				 "metavar %s used in repetition does not exist",
				 frag_token->get_str ().c_str ());

		  is_valid = false;
		}

	      auto &fragment = *it->second;

	      if (!fragment.is_single_fragment ())
		{
		  auto &fragment_rep
		    = static_cast<MatchedFragmentContainerRepetition &> (
		      fragment);
		  size_t repeat_amount = fragment_rep.get_match_amount ();
		  if (!first_fragment_found)
		    {
		      first_fragment_found = true;
		      expected_repetition_amount = repeat_amount;
		    }
		  else
		    {
		      if (repeat_amount != expected_repetition_amount)
			{
			  rust_error_at (
			    frag_token->get_locus (),
			    "different amount of matches used in merged "
			    "repetitions: expected %lu, got %lu",
			    (unsigned long) expected_repetition_amount,
			    (unsigned long) repeat_amount);
			  is_valid = false;
			}
		    }
		}
	    }
	}
    }

  return is_valid && first_fragment_found;
}

std::vector<std::unique_ptr<AST::Token>>
SubstituteCtx::substitute_repetition (
  size_t pattern_start, size_t pattern_end,
  std::unique_ptr<AST::Token> separator_token)
{
  rust_assert (pattern_end < macro.size ());

  size_t repeat_amount = 0;
  if (!check_repetition_amount (pattern_start, pattern_end, repeat_amount))
    return {};

  rust_debug ("repetition amount to use: %lu", (unsigned long) repeat_amount);
  std::vector<std::unique_ptr<AST::Token>> expanded;
  std::vector<std::unique_ptr<AST::Token>> new_macro;

  // We want to generate a "new macro" to substitute with. This new macro
  // should contain only the tokens inside the pattern
  for (size_t tok_idx = pattern_start; tok_idx < pattern_end; tok_idx++)
    new_macro.emplace_back (macro.at (tok_idx)->clone_token ());

  // Then, we want to create a subset of the matches so that
  // `substitute_tokens()` can only see one fragment per metavar. Let's say we
  // have the following user input: (1 145 'h')
  // on the following match arm: ($($lit:literal)*)
  // which causes the following matches: { "lit": [1, 145, 'h'] }
  //
  // The pattern (new_macro) is `$lit:literal`
  // The first time we expand it, we want $lit to have the following token: 1
  // The second time, 145
  // The third and final time, 'h'
  //
  // In order to do so we must create "sub maps", which only contain parts of
  // the original matches
  // sub-maps: [ { "lit": 1 }, { "lit": 145 }, { "lit": 'h' } ]
  //
  // and give them to `substitute_tokens` one by one.

  for (size_t i = 0; i < repeat_amount; i++)
    {
      std::map<std::string, MatchedFragmentContainer *> sub_map;
      for (auto &kv_match : fragments)
	{
	  if (kv_match.second->is_single_fragment ())
	    sub_map.emplace (kv_match.first, kv_match.second);
	  // Hack: A repeating meta variable might not be present in the new
	  // macro. Don't include this match if the fragment doesn't have enough
	  // items, as check_repetition_amount should prevent repetition amount
	  // mismatches anyway.
	  else if (kv_match.second->get_fragments ().size () > i)
	    sub_map.emplace (kv_match.first,
			     kv_match.second->get_fragments ().at (i).get ());
	}

      auto substitute_context = SubstituteCtx (input, new_macro, sub_map);
      auto new_tokens = substitute_context.substitute_tokens ();

      // Skip the first repetition, but add the separator to the expanded
      // tokens if it is present
      if (i != 0 && separator_token)
	expanded.emplace_back (separator_token->clone_token ());

      for (auto &new_token : new_tokens)
	expanded.emplace_back (new_token->clone_token ());
    }

  // FIXME: We also need to make sure that all subsequent fragments
  // contain the same amount of repetitions as the first one

  return expanded;
}

static bool
is_rep_op (std::unique_ptr<AST::Token> &tok)
{
  auto id = tok->get_id ();
  return id == QUESTION_MARK || id == ASTERISK || id == PLUS;
}

std::pair<std::vector<std::unique_ptr<AST::Token>>, size_t>
SubstituteCtx::substitute_token (size_t token_idx)
{
  auto &token = macro.at (token_idx);

  switch (token->get_id ())
    {
    default:
      if (token_id_is_keyword (token->get_id ()))
	{
	case IDENTIFIER:
	  std::vector<std::unique_ptr<AST::Token>> expanded;

	  rust_debug ("expanding metavar: %s", token->get_str ().c_str ());

	  if (substitute_metavar (token, expanded))
	    return {std::move (expanded), 2};
	}

      // don't substitute, dollar sign is alone/metavar is unknown
      return {std::vector<std::unique_ptr<AST::Token>> (), 0};

      case LEFT_PAREN: {
	// We need to parse up until the closing delimiter and expand this
	// fragment->n times.
	rust_debug ("expanding repetition");

	// We're in a context where macro repetitions have already been
	// parsed and validated: This means that
	// 1/ There will be no delimiters as that is an error
	// 2/ There are no fragment specifiers anymore, which prevents us
	// from reusing parser functions.
	//
	// Repetition patterns are also special in that they cannot contain
	// "rogue" delimiters: For example, this is invalid, as they are
	// parsed as MacroMatches and must contain a correct amount of
	// delimiters.
	// `$($e:expr ) )`
	//            ^ rogue closing parenthesis
	//
	// With all of that in mind, we can simply skip ahead from one
	// parenthesis to the other to find the pattern to expand. Of course,
	// pairs of delimiters, including parentheses, are allowed.
	// `$($e:expr ( ) )`
	// Parentheses are the sole delimiter for which we need a special
	// behavior since they delimit the repetition pattern

	size_t pattern_start = token_idx + 1;
	size_t pattern_end = pattern_start;
	auto parentheses_stack = 0;
	for (size_t idx = pattern_start; idx < macro.size (); idx++)
	  {
	    if (macro.at (idx)->get_id () == LEFT_PAREN)
	      {
		parentheses_stack++;
	      }
	    else if (macro.at (idx)->get_id () == RIGHT_PAREN)
	      {
		if (parentheses_stack == 0)
		  {
		    pattern_end = idx;
		    break;
		  }
		parentheses_stack--;
	      }
	  }

	// Unreachable case, but let's make sure we don't ever run into it
	rust_assert (pattern_end != pattern_start);

	std::unique_ptr<AST::Token> separator_token = nullptr;
	if (pattern_end + 1 <= macro.size ())
	  {
	    auto &post_pattern_token = macro.at (pattern_end + 1);
	    if (!is_rep_op (post_pattern_token))
	      separator_token = post_pattern_token->clone_token ();
	  }

	// Amount of tokens to skip
	auto to_skip = 0;
	// Parentheses
	to_skip += 2;
	// Repetition operator
	to_skip += 1;
	// Separator
	if (separator_token)
	  to_skip += 1;

	return {substitute_repetition (pattern_start, pattern_end,
				       std::move (separator_token)),
		pattern_end - pattern_start + to_skip + 1};
      }
    }

  rust_unreachable ();
}

std::vector<std::unique_ptr<AST::Token>>
SubstituteCtx::substitute_tokens ()
{
  std::vector<std::unique_ptr<AST::Token>> replaced_tokens;
  rust_debug ("expanding tokens");

  for (size_t i = 0; i < macro.size ();)
    {
      auto &tok = macro.at (i);
      if (tok->get_id () == DOLLAR_SIGN)
	{
	  // Aaaaah, if only we had C++17 :)
	  // auto [expanded, tok_to_skip] = ...
	  auto p = substitute_token (i + 1);
	  auto expanded = std::move (p.first);
	  auto tok_to_skip = p.second;

	  if (!tok_to_skip)
	    {
	      replaced_tokens.emplace_back (tok->clone_token ());
	      tok_to_skip++;
	    }

	  i += tok_to_skip;

	  for (auto &token : expanded)
	    replaced_tokens.emplace_back (token->clone_token ());
	}
      else
	{
	  replaced_tokens.emplace_back (tok->clone_token ());
	  i++;
	}
    }

  return replaced_tokens;
}

} // namespace Rust
