#include "rust-macro-substitute-ctx.h"

namespace Rust {

std::vector<std::unique_ptr<AST::Token>>
SubstituteCtx::substitute_metavar (std::unique_ptr<AST::Token> &metavar)
{
  auto metavar_name = metavar->get_str ();

  std::vector<std::unique_ptr<AST::Token>> expanded;
  auto it = fragments.find (metavar_name);
  if (it == fragments.end ())
    {
      // Return a copy of the original token
      expanded.push_back (metavar->clone_token ());
    }
  else
    {
      // Replace
      // We only care about the vector when expanding repetitions.
      // Just access the first element of the vector.
      // FIXME: Clean this up so it makes more sense
      auto &frag = it->second[0];
      for (size_t offs = frag.token_offset_begin; offs < frag.token_offset_end;
	   offs++)
	{
	  auto &tok = input.at (offs);
	  expanded.push_back (tok->clone_token ());
	}
    }

  return expanded;
}

std::vector<std::unique_ptr<AST::Token>>
SubstituteCtx::substitute_repetition (
  size_t pattern_start, size_t pattern_end,
  std::unique_ptr<AST::Token> separator_token)
{
  rust_assert (pattern_end < macro.size ());

  std::vector<std::unique_ptr<AST::Token>> expanded;

  // Find the first fragment and get the amount of repetitions that we should
  // perform
  size_t repeat_amount = 0;
  for (size_t i = pattern_start; i < pattern_end; i++)
    {
      if (macro.at (i)->get_id () == DOLLAR_SIGN)
	{
	  auto &frag_token = macro.at (i + 1);
	  if (frag_token->get_id () == IDENTIFIER)
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
		  // FIXME:
		  return expanded;
		}

	      // FIXME: Refactor, ugly
	      repeat_amount = it->second[0].match_amount;
	    }
	}
    }

  rust_debug ("repetition amount to use: %lu", repeat_amount);
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
      std::map<std::string, std::vector<MatchedFragment>> sub_map;
      for (auto &kv_match : fragments)
	{
	  std::vector<MatchedFragment> sub_vec;

	  // FIXME: Hack: If a fragment is not repeated, how does it fit in the
	  // submap? Do we really want to expand it? Is this normal behavior?
	  if (kv_match.second.size () == 1)
	    sub_vec.emplace_back (kv_match.second[0]);
	  else
	    sub_vec.emplace_back (kv_match.second[i]);

	  sub_map.insert ({kv_match.first, sub_vec});
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
    case IDENTIFIER:
      rust_debug ("expanding metavar: %s", token->get_str ().c_str ());
      return {substitute_metavar (token), 1};
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
		pattern_end - pattern_start + to_skip};
      }
      // TODO: We need to check if the $ was alone. In that case, do
      // not error out: Simply act as if there was an empty identifier
      // with no associated fragment and paste the dollar sign in the
      // transcription. Unsure how to do that since we always have at
      // least the closing curly brace after an empty $...
    default:
      rust_error_at (token->get_locus (),
		     "unexpected token in macro transcribe: expected "
		     "%<(%> or identifier after %<$%>, got %<%s%>",
		     get_token_description (token->get_id ()));
    }

  // FIXME: gcc_unreachable() error case?
  return {std::vector<std::unique_ptr<AST::Token>> (), 0};
}

std::vector<std::unique_ptr<AST::Token>>
SubstituteCtx::substitute_tokens ()
{
  std::vector<std::unique_ptr<AST::Token>> replaced_tokens;
  rust_debug ("expanding tokens");

  for (size_t i = 0; i < macro.size (); i++)
    {
      auto &tok = macro.at (i);
      if (tok->get_id () == DOLLAR_SIGN)
	{
	  // Aaaaah, if only we had C++17 :)
	  // auto [expanded, tok_to_skip] = ...
	  auto p = substitute_token (i + 1);
	  auto expanded = std::move (p.first);
	  auto tok_to_skip = p.second;

	  i += tok_to_skip;

	  for (auto &token : expanded)
	    replaced_tokens.emplace_back (token->clone_token ());
	}
      else
	{
	  replaced_tokens.emplace_back (tok->clone_token ());
	}
    }

  return replaced_tokens;
}

} // namespace Rust
