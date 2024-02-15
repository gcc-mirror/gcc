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

#include "rust-macro-expand.h"
#include "optional.h"
#include "rust-macro-substitute-ctx.h"
#include "rust-ast-full.h"
#include "rust-ast-visitor.h"
#include "rust-diagnostics.h"
#include "rust-parse.h"
#include "rust-cfg-strip.h"
#include "rust-early-name-resolver.h"
#include "rust-session-manager.h"
#include "rust-proc-macro.h"

namespace Rust {

AST::Fragment
MacroExpander::expand_decl_macro (location_t invoc_locus,
				  AST::MacroInvocData &invoc,
				  AST::MacroRulesDefinition &rules_def,
				  bool semicolon)
{
  // ensure that both invocation and rules are in a valid state
  rust_assert (!invoc.is_marked_for_strip ());
  rust_assert (!rules_def.is_marked_for_strip ());
  rust_assert (rules_def.get_macro_rules ().size () > 0);

  /* probably something here about parsing invoc and rules def token trees to
   * token stream. if not, how would parser handle the captures of exprs and
   * stuff? on the other hand, token trees may be kind of useful in rules def as
   * creating a point where recursion can occur (like having
   * "compare_macro_match" and then it calling itself when it finds delimiters)
   */

  /* find matching rule to invoc token tree, based on macro rule's matcher. if
   * none exist, error.
   * - specifically, check each matcher in order. if one fails to match, move
   * onto next. */
  /* TODO: does doing this require parsing expressions and whatever in the
   * invoc? if so, might as well save the results if referenced using $ or
   * whatever. If not, do another pass saving them. Except this is probably
   * useless as different rules could have different starting points for exprs
   * or whatever. Decision trees could avoid this, but they have their own
   * issues. */
  /* TODO: will need to modify the parser so that it can essentially "catch"
   * errors - maybe "try_parse_expr" or whatever methods. */
  // this technically creates a back-tracking parser - this will be the
  // implementation style

  /* then, after results are saved, generate the macro output from the
   * transcriber token tree. if i understand this correctly, the macro
   * invocation gets replaced by the transcriber tokens, except with
   * substitutions made (e.g. for $i variables) */

  /* TODO: it is probably better to modify AST::Token to store a pointer to a
   * Lexer::Token (rather than being converted) - i.e. not so much have
   * AST::Token as a Token but rather a TokenContainer (as it is another type of
   * TokenTree). This will prevent re-conversion of Tokens between each type
   * all the time, while still allowing the heterogenous storage of token trees.
   */

  AST::DelimTokenTree &invoc_token_tree = invoc.get_delim_tok_tree ();

  // find matching arm
  AST::MacroRule *matched_rule = nullptr;
  std::map<std::string, std::unique_ptr<MatchedFragmentContainer>>
    matched_fragments;
  for (auto &rule : rules_def.get_rules ())
    {
      sub_stack.push ();
      bool did_match_rule = try_match_rule (rule, invoc_token_tree);
      matched_fragments = sub_stack.pop ();

      if (did_match_rule)
	{
	  //  // Debugging
	  //  for (auto &kv : matched_fragments)
	  //    rust_debug ("[fragment]: %s (%ld - %s)", kv.first.c_str (),
	  //		kv.second.get_fragments ().size (),
	  //		kv.second.get_kind ()
	  //		    == MatchedFragmentContainer::Kind::Repetition
	  //		  ? "repetition"
	  //		  : "metavar");

	  matched_rule = &rule;
	  break;
	}
    }

  if (matched_rule == nullptr)
    {
      rich_location r (line_table, invoc_locus);
      r.add_range (rules_def.get_locus ());
      rust_error_at (r, "Failed to match any rule within macro");
      return AST::Fragment::create_error ();
    }

  std::map<std::string, MatchedFragmentContainer *> matched_fragments_ptr;

  for (auto &ent : matched_fragments)
    matched_fragments_ptr.emplace (ent.first, ent.second.get ());

  return transcribe_rule (*matched_rule, invoc_token_tree,
			  matched_fragments_ptr, semicolon, peek_context ());
}

void
MacroExpander::expand_eager_invocations (AST::MacroInvocation &invoc)
{
  if (invoc.get_pending_eager_invocations ().empty ())
    return;

  // We have to basically create a new delimited token tree which contains the
  // result of one step of expansion. In the case of builtin macros called with
  // other macro invocations, such as `concat!("h", 'a', a!())`, we need to
  // expand `a!()` before expanding the concat macro.
  // This will, ideally, give us a new token tree containing the various
  // existing tokens + the result of the expansion of a!().
  // To do this, we "parse" the given token tree to find anything that "looks
  // like a macro invocation". Then, we get the corresponding macro invocation
  // from the `pending_eager_invocations` vector and expand it.
  // Because the `pending_eager_invocations` vector is created in the same order
  // that the DelimTokenTree is parsed, we know that the first macro invocation
  // within the DelimTokenTree corresponds to the first element in
  // `pending_eager_invocations`. The idea is thus to:
  // 1. Find a macro invocation in the token tree, noting the index of the start
  //    token and of the end token
  // 2. Get its associated invocation in `pending_eager_invocations`
  // 3. Expand that element
  // 4. Get the token tree associated with that AST fragment
  // 5. Replace the original tokens corresponding to the invocation with the new
  //    tokens from the fragment
  // pseudo-code:
  //
  // i = 0;
  // for tok in dtt:
  //   if tok is identifier && tok->next() is !:
  //     start = index(tok);
  //     l_delim = tok->next()->next();
  //     tok = skip_until_r_delim();
  //     end = index(tok);
  //
  //     new_tt = expand_eager_invoc(eagers[i++]);
  //     old_tt[start..end] = new_tt;

  auto dtt = invoc.get_invoc_data ().get_delim_tok_tree ();
  auto stream = dtt.to_token_stream ();
  std::vector<std::unique_ptr<AST::TokenTree>> new_stream;
  size_t current_pending = 0;

  // we need to create a clone of the delimited token tree as the lexer
  // expects ownership of the tokens
  std::vector<std::unique_ptr<Rust::AST::Token>> dtt_clone;
  for (auto &tok : stream)
    dtt_clone.emplace_back (tok->clone_token ());

  MacroInvocLexer lex (std::move (dtt_clone));
  Parser<MacroInvocLexer> parser (lex);

  // we want to build a substitution map - basically, associating a `start` and
  // `end` index for each of the pending macro invocations
  std::map<std::pair<size_t, size_t>, std::unique_ptr<AST::MacroInvocation> &>
    substitution_map;

  for (size_t i = 0; i < stream.size (); i++)
    {
      // FIXME: Can't these offsets be figure out when we actually parse the
      // pending_eager_invocation in the first place?
      auto invocation = parser.parse_macro_invocation ({});

      // if we've managed to parse a macro invocation, we look at the current
      // offset and store them in the substitution map. Otherwise, we skip one
      // token and try parsing again
      if (invocation)
	substitution_map.insert (
	  {{i, parser.get_token_source ().get_offs ()},
	   invoc.get_pending_eager_invocations ()[current_pending++]});
      else
	parser.skip_token (stream[i]->get_id ());
    }

  size_t current_idx = 0;
  for (auto kv : substitution_map)
    {
      auto &to_expand = kv.second;
      expand_invoc (*to_expand, false);

      auto fragment = take_expanded_fragment ();
      auto &new_tokens = fragment.get_tokens ();

      auto start = kv.first.first;
      auto end = kv.first.second;

      // We're now going to re-add the tokens to the invocation's token tree.
      // 1. Basically, what we want to do is insert all tokens up until the
      //    beginning of the macro invocation (start).
      // 2. Then, we'll insert all of the tokens resulting from the macro
      //    expansion: These are in `new_tokens`.
      // 3. Finally, we'll do that again from
      //    the end of macro and go back to 1.

      for (size_t i = current_idx; i < start; i++)
	new_stream.emplace_back (stream[i]->clone_token ());

      for (auto &tok : new_tokens)
	new_stream.emplace_back (tok->clone_token ());

      current_idx = end;
    }

  // Once all of that is done, we copy the last remaining tokens from the
  // original stream
  for (size_t i = current_idx; i < stream.size (); i++)
    new_stream.emplace_back (stream[i]->clone_token ());

  auto new_dtt
    = AST::DelimTokenTree (dtt.get_delim_type (), std::move (new_stream));

  invoc.get_pending_eager_invocations ().clear ();
  invoc.get_invoc_data ().set_delim_tok_tree (new_dtt);
}

void
MacroExpander::expand_invoc (AST::MacroInvocation &invoc, bool has_semicolon)
{
  if (depth_exceeds_recursion_limit ())
    {
      rust_error_at (invoc.get_locus (), "reached recursion limit");
      return;
    }

  if (invoc.get_kind () == AST::MacroInvocation::InvocKind::Builtin)
    expand_eager_invocations (invoc);

  AST::MacroInvocData &invoc_data = invoc.get_invoc_data ();

  // ??
  // switch on type of macro:
  //  - '!' syntax macro (inner switch)
  //      - procedural macro - "A token-based function-like macro"
  //      - 'macro_rules' (by example/pattern-match) macro? or not? "an
  // AST-based function-like macro"
  //      - else is unreachable
  //  - attribute syntax macro (inner switch)
  //  - procedural macro attribute syntax - "A token-based attribute
  // macro"
  //      - legacy macro attribute syntax? - "an AST-based attribute macro"
  //      - non-macro attribute: mark known
  //      - else is unreachable
  //  - derive macro (inner switch)
  //      - derive or legacy derive - "token-based" vs "AST-based"
  //      - else is unreachable
  //  - derive container macro - unreachable

  auto fragment = AST::Fragment::create_error ();
  invoc_data.set_expander (this);

  // lookup the rules
  AST::MacroRulesDefinition *rules_def = nullptr;
  bool ok = mappings->lookup_macro_invocation (invoc, &rules_def);

  // If there's no rule associated with the invocation, we can simply return
  // early. The early name resolver will have already emitted an error.
  if (!ok)
    return;

  // We store the last expanded invocation and macro definition for error
  // reporting in case the recursion limit is reached
  last_invoc = *invoc.clone_macro_invocation_impl ();
  last_def = *rules_def;

  if (rules_def->is_builtin ())
    fragment
      = rules_def->get_builtin_transcriber () (invoc.get_locus (), invoc_data)
	  .value_or (AST::Fragment::create_empty ());
  else
    fragment = expand_decl_macro (invoc.get_locus (), invoc_data, *rules_def,
				  has_semicolon);

  set_expanded_fragment (std::move (fragment));
}

void
MacroExpander::expand_crate ()
{
  NodeId scope_node_id = crate.get_node_id ();
  resolver->get_macro_scope ().push (scope_node_id);

  /* fill macro/decorator map from init list? not sure where init list comes
   * from? */

  // TODO: does cfg apply for inner attributes? research.
  // the apparent answer (from playground test) is yes

  push_context (ContextType::ITEM);

  // expand attributes recursively and strip items if required
  //  AttrVisitor attr_visitor (*this);
  auto &items = crate.items;
  for (auto it = items.begin (); it != items.end ();)
    {
      auto &item = *it;

      auto fragment = take_expanded_fragment ();
      if (fragment.should_expand ())
	{
	  // Remove the current expanded invocation
	  it = items.erase (it);
	  for (auto &node : fragment.get_nodes ())
	    {
	      it = items.insert (it, node.take_item ());
	      it++;
	    }
	}
      else if (item->is_marked_for_strip ())
	it = items.erase (it);
      else
	it++;
    }

  pop_context ();

  // TODO: should recursive attribute and macro expansion be done in the same
  // transversal? Or in separate ones like currently?

  // expand module tree recursively

  // post-process

  // extract exported macros?
}

bool
MacroExpander::depth_exceeds_recursion_limit () const
{
  return expansion_depth >= cfg.recursion_limit;
}

bool
MacroExpander::try_match_rule (AST::MacroRule &match_rule,
			       AST::DelimTokenTree &invoc_token_tree)
{
  MacroInvocLexer lex (invoc_token_tree.to_token_stream ());
  Parser<MacroInvocLexer> parser (lex);

  AST::MacroMatcher &matcher = match_rule.get_matcher ();

  expansion_depth++;
  if (!match_matcher (parser, matcher, false, false))
    {
      expansion_depth--;
      return false;
    }
  expansion_depth--;

  bool used_all_input_tokens = parser.skip_token (END_OF_FILE);
  return used_all_input_tokens;
}

bool
MacroExpander::match_fragment (Parser<MacroInvocLexer> &parser,
			       AST::MacroMatchFragment &fragment)
{
  switch (fragment.get_frag_spec ().get_kind ())
    {
    case AST::MacroFragSpec::EXPR:
      parser.parse_expr ();
      break;

    case AST::MacroFragSpec::BLOCK:
      parser.parse_block_expr ();
      break;

    case AST::MacroFragSpec::IDENT:
      parser.parse_identifier_or_keyword_token ();
      break;

    case AST::MacroFragSpec::LITERAL:
      parser.parse_literal_expr ();
      break;

    case AST::MacroFragSpec::ITEM:
      parser.parse_item (false);
      break;

    case AST::MacroFragSpec::TY:
      parser.parse_type ();
      break;

    case AST::MacroFragSpec::PAT:
      parser.parse_pattern ();
      break;

    case AST::MacroFragSpec::PATH:
      parser.parse_path_in_expression ();
      break;

    case AST::MacroFragSpec::VIS:
      parser.parse_visibility ();
      break;

      case AST::MacroFragSpec::STMT: {
	auto restrictions = ParseRestrictions ();
	restrictions.consume_semi = false;
	parser.parse_stmt (restrictions);
	break;
      }

    case AST::MacroFragSpec::LIFETIME:
      parser.parse_lifetime_params ();
      break;

      // is meta attributes?
    case AST::MacroFragSpec::META:
      parser.parse_attribute_body ();
      break;

    case AST::MacroFragSpec::TT:
      parser.parse_token_tree ();
      break;

      // i guess we just ignore invalid and just error out
    case AST::MacroFragSpec::INVALID:
      return false;
    }

  // it matches if the parser did not produce errors trying to parse that type
  // of item
  return !parser.has_errors ();
}

bool
MacroExpander::match_matcher (Parser<MacroInvocLexer> &parser,
			      AST::MacroMatcher &matcher, bool in_repetition,
			      bool match_delim)
{
  if (depth_exceeds_recursion_limit ())
    {
      rust_error_at (matcher.get_match_locus (), "reached recursion limit");
      return false;
    }

  auto delimiter = parser.peek_current_token ();

  auto check_delim = [&matcher, match_delim] (AST::DelimType delim) {
    return !match_delim || matcher.get_delim_type () == delim;
  };

  // this is used so we can check that we delimit the stream correctly.
  switch (delimiter->get_id ())
    {
      case LEFT_PAREN: {
	if (!check_delim (AST::DelimType::PARENS))
	  return false;
      }
      break;

      case LEFT_SQUARE: {
	if (!check_delim (AST::DelimType::SQUARE))
	  return false;
      }
      break;

      case LEFT_CURLY: {
	if (!check_delim (AST::DelimType::CURLY))
	  return false;
      }
      break;
    default:
      return false;
    }
  parser.skip_token ();

  const MacroInvocLexer &source = parser.get_token_source ();

  for (auto &match : matcher.get_matches ())
    {
      size_t offs_begin = source.get_offs ();

      switch (match->get_macro_match_type ())
	{
	  case AST::MacroMatch::MacroMatchType::Fragment: {
	    AST::MacroMatchFragment *fragment
	      = static_cast<AST::MacroMatchFragment *> (match.get ());
	    if (!match_fragment (parser, *fragment))
	      return false;

	    // matched fragment get the offset in the token stream
	    size_t offs_end = source.get_offs ();
	    sub_stack.insert_metavar (
	      MatchedFragment (fragment->get_ident ().as_string (), offs_begin,
			       offs_end));
	  }
	  break;

	  case AST::MacroMatch::MacroMatchType::Tok: {
	    AST::Token *tok = static_cast<AST::Token *> (match.get ());
	    if (!match_token (parser, *tok))
	      return false;
	  }
	  break;

	  case AST::MacroMatch::MacroMatchType::Repetition: {
	    AST::MacroMatchRepetition *rep
	      = static_cast<AST::MacroMatchRepetition *> (match.get ());
	    if (!match_repetition (parser, *rep))
	      return false;
	  }
	  break;

	  case AST::MacroMatch::MacroMatchType::Matcher: {
	    AST::MacroMatcher *m
	      = static_cast<AST::MacroMatcher *> (match.get ());
	    expansion_depth++;
	    if (!match_matcher (parser, *m, in_repetition))
	      {
		expansion_depth--;
		return false;
	      }
	    expansion_depth--;
	  }
	  break;
	}
    }

  switch (delimiter->get_id ())
    {
      case LEFT_PAREN: {
	if (!parser.skip_token (RIGHT_PAREN))
	  return false;
      }
      break;

      case LEFT_SQUARE: {
	if (!parser.skip_token (RIGHT_SQUARE))
	  return false;
      }
      break;

      case LEFT_CURLY: {
	if (!parser.skip_token (RIGHT_CURLY))
	  return false;
      }
      break;
    default:
      rust_unreachable ();
    }

  return true;
}

bool
MacroExpander::match_token (Parser<MacroInvocLexer> &parser, AST::Token &token)
{
  return parser.skip_token (token.get_tok_ptr ());
}

bool
MacroExpander::match_n_matches (Parser<MacroInvocLexer> &parser,
				AST::MacroMatchRepetition &rep,
				size_t &match_amount, size_t lo_bound,
				size_t hi_bound)
{
  match_amount = 0;
  auto &matches = rep.get_matches ();

  const MacroInvocLexer &source = parser.get_token_source ();
  while (true)
    {
      // If the current token is a closing macro delimiter, break away.
      // TODO: Is this correct?
      auto t_id = parser.peek_current_token ()->get_id ();
      if (t_id == RIGHT_PAREN || t_id == RIGHT_SQUARE || t_id == RIGHT_CURLY)
	break;

      // Skip parsing a separator on the first match, otherwise consume it.
      // If it isn't present, this is an error
      if (rep.has_sep () && match_amount > 0)
	if (!match_token (parser, *rep.get_sep ()))
	  break;

      sub_stack.push ();
      bool valid_current_match = false;
      for (auto &match : matches)
	{
	  size_t offs_begin = source.get_offs ();
	  switch (match->get_macro_match_type ())
	    {
	      case AST::MacroMatch::MacroMatchType::Fragment: {
		AST::MacroMatchFragment *fragment
		  = static_cast<AST::MacroMatchFragment *> (match.get ());
		valid_current_match = match_fragment (parser, *fragment);

		// matched fragment get the offset in the token stream
		size_t offs_end = source.get_offs ();

		sub_stack.insert_metavar (
		  MatchedFragment (fragment->get_ident ().as_string (),
				   offs_begin, offs_end));
	      }
	      break;

	      case AST::MacroMatch::MacroMatchType::Tok: {
		AST::Token *tok = static_cast<AST::Token *> (match.get ());
		valid_current_match = match_token (parser, *tok);
	      }
	      break;

	      case AST::MacroMatch::MacroMatchType::Repetition: {
		AST::MacroMatchRepetition *rep
		  = static_cast<AST::MacroMatchRepetition *> (match.get ());
		valid_current_match = match_repetition (parser, *rep);
	      }
	      break;

	      case AST::MacroMatch::MacroMatchType::Matcher: {
		AST::MacroMatcher *m
		  = static_cast<AST::MacroMatcher *> (match.get ());
		valid_current_match = match_matcher (parser, *m, true);
	      }
	      break;
	    }
	}
      auto old_stack = sub_stack.pop ();

      // nest metavars into repetitions
      for (auto &ent : old_stack)
	sub_stack.append_fragment (ent.first, std::move (ent.second));

      // If we've encountered an error once, stop trying to match more
      // repetitions
      if (!valid_current_match)
	break;

      match_amount++;

      // Break early if we notice there's too many expressions already
      if (hi_bound && match_amount > hi_bound)
	break;
    }

  // Check if the amount of matches we got is valid: Is it more than the lower
  // bound and less than the higher bound?
  bool did_meet_lo_bound = match_amount >= lo_bound;
  bool did_meet_hi_bound = hi_bound ? match_amount <= hi_bound : true;

  // If the end-result is valid, then we can clear the parse errors: Since
  // repetitions are parsed eagerly, it is okay to fail in some cases
  auto res = did_meet_lo_bound && did_meet_hi_bound;
  if (res)
    parser.clear_errors ();

  return res;
}

/*
 * Helper function for defining unmatched repetition metavars
 */
void
MacroExpander::match_repetition_skipped_metavars (AST::MacroMatch &match)
{
  // We have to handle zero fragments differently: They will not have been
  // "matched" but they are still valid and should be inserted as a special
  // case. So we go through the stack map, and for every fragment which doesn't
  // exist, insert a zero-matched fragment.
  switch (match.get_macro_match_type ())
    {
    case AST::MacroMatch::MacroMatchType::Fragment:
      match_repetition_skipped_metavars (
	static_cast<AST::MacroMatchFragment &> (match));
      break;
    case AST::MacroMatch::MacroMatchType::Repetition:
      match_repetition_skipped_metavars (
	static_cast<AST::MacroMatchRepetition &> (match));
      break;
    case AST::MacroMatch::MacroMatchType::Matcher:
      match_repetition_skipped_metavars (
	static_cast<AST::MacroMatcher &> (match));
      break;
    case AST::MacroMatch::MacroMatchType::Tok:
      break;
    }
}

void
MacroExpander::match_repetition_skipped_metavars (
  AST::MacroMatchFragment &fragment)
{
  auto &stack_map = sub_stack.peek ();
  auto it = stack_map.find (fragment.get_ident ().as_string ());

  if (it == stack_map.end ())
    sub_stack.insert_matches (fragment.get_ident ().as_string (),
			      MatchedFragmentContainer::zero ());
}

void
MacroExpander::match_repetition_skipped_metavars (
  AST::MacroMatchRepetition &rep)
{
  for (auto &match : rep.get_matches ())
    match_repetition_skipped_metavars (*match);
}

void
MacroExpander::match_repetition_skipped_metavars (AST::MacroMatcher &rep)
{
  for (auto &match : rep.get_matches ())
    match_repetition_skipped_metavars (*match);
}

bool
MacroExpander::match_repetition (Parser<MacroInvocLexer> &parser,
				 AST::MacroMatchRepetition &rep)
{
  size_t match_amount = 0;
  bool res = false;

  std::string lo_str;
  std::string hi_str;
  switch (rep.get_op ())
    {
    case AST::MacroMatchRepetition::MacroRepOp::ANY:
      lo_str = "0";
      hi_str = "+inf";
      res = match_n_matches (parser, rep, match_amount);
      break;
    case AST::MacroMatchRepetition::MacroRepOp::ONE_OR_MORE:
      lo_str = "1";
      hi_str = "+inf";
      res = match_n_matches (parser, rep, match_amount, 1);
      break;
    case AST::MacroMatchRepetition::MacroRepOp::ZERO_OR_ONE:
      lo_str = "0";
      hi_str = "1";
      res = match_n_matches (parser, rep, match_amount, 0, 1);
      break;
    default:
      rust_unreachable ();
    }

  rust_debug_loc (rep.get_match_locus (), "%s matched %lu times",
		  res ? "successfully" : "unsuccessfully",
		  (unsigned long) match_amount);

  match_repetition_skipped_metavars (rep);

  return res;
}

/**
 * Helper function to refactor calling a parsing function 0 or more times
 */
static AST::Fragment
parse_many (Parser<MacroInvocLexer> &parser, TokenId delimiter,
	    std::function<AST::SingleASTNode ()> parse_fn)
{
  auto &lexer = parser.get_token_source ();
  auto start = lexer.get_offs ();

  std::vector<AST::SingleASTNode> nodes;
  while (true)
    {
      if (parser.peek_current_token ()->get_id () == delimiter)
	break;

      auto node = parse_fn ();
      if (node.is_error ())
	{
	  for (auto err : parser.get_errors ())
	    err.emit ();

	  return AST::Fragment::create_error ();
	}

      nodes.emplace_back (std::move (node));
    }
  auto end = lexer.get_offs ();

  return AST::Fragment (std::move (nodes), lexer.get_token_slice (start, end));
}

/**
 * Transcribe 0 or more items from a macro invocation
 *
 * @param parser Parser to extract items from
 * @param delimiter Id of the token on which parsing should stop
 */
static AST::Fragment
transcribe_many_items (Parser<MacroInvocLexer> &parser, TokenId &delimiter)
{
  return parse_many (parser, delimiter, [&parser] () {
    auto item = parser.parse_item (true);
    return AST::SingleASTNode (std::move (item));
  });
}

/**
 * Transcribe 0 or more external items from a macro invocation
 *
 * @param parser Parser to extract items from
 * @param delimiter Id of the token on which parsing should stop
 */
static AST::Fragment
transcribe_many_ext (Parser<MacroInvocLexer> &parser, TokenId &delimiter)
{
  return parse_many (parser, delimiter, [&parser] () {
    auto item = parser.parse_external_item ();
    return AST::SingleASTNode (std::move (item));
  });
}

/**
 * Transcribe 0 or more trait items from a macro invocation
 *
 * @param parser Parser to extract items from
 * @param delimiter Id of the token on which parsing should stop
 */
static AST::Fragment
transcribe_many_trait_items (Parser<MacroInvocLexer> &parser,
			     TokenId &delimiter)
{
  return parse_many (parser, delimiter, [&parser] () {
    auto item = parser.parse_trait_item ();
    return AST::SingleASTNode (std::move (item));
  });
}

/**
 * Transcribe 0 or more impl items from a macro invocation
 *
 * @param parser Parser to extract items from
 * @param delimiter Id of the token on which parsing should stop
 */
static AST::Fragment
transcribe_many_impl_items (Parser<MacroInvocLexer> &parser, TokenId &delimiter)
{
  return parse_many (parser, delimiter, [&parser] () {
    auto item = parser.parse_inherent_impl_item ();
    return AST::SingleASTNode (std::move (item));
  });
}

/**
 * Transcribe 0 or more trait impl items from a macro invocation
 *
 * @param parser Parser to extract items from
 * @param delimiter Id of the token on which parsing should stop
 */
static AST::Fragment
transcribe_many_trait_impl_items (Parser<MacroInvocLexer> &parser,
				  TokenId &delimiter)
{
  return parse_many (parser, delimiter, [&parser] () {
    auto item = parser.parse_trait_impl_item ();
    return AST::SingleASTNode (std::move (item));
  });
}

/**
 * Transcribe 0 or more statements from a macro invocation
 *
 * @param parser Parser to extract statements from
 * @param delimiter Id of the token on which parsing should stop
 */
static AST::Fragment
transcribe_many_stmts (Parser<MacroInvocLexer> &parser, TokenId delimiter,
		       bool semicolon)
{
  auto restrictions = ParseRestrictions ();
  restrictions.allow_close_after_expr_stmt = true;

  return parse_many (parser, delimiter,
		     [&parser, restrictions, delimiter, semicolon] () {
		       auto stmt = parser.parse_stmt (restrictions);
		       if (semicolon && stmt
			   && parser.peek_current_token ()->get_id ()
				== delimiter)
			 stmt->add_semicolon ();

		       return AST::SingleASTNode (std::move (stmt));
		     });
}

/**
 * Transcribe one expression from a macro invocation
 *
 * @param parser Parser to extract statements from
 */
static AST::Fragment
transcribe_expression (Parser<MacroInvocLexer> &parser)
{
  auto &lexer = parser.get_token_source ();
  auto start = lexer.get_offs ();

  auto expr = parser.parse_expr ();
  if (expr == nullptr)
    return AST::Fragment::create_error ();

  // FIXME: make this an error for some edititons
  if (parser.peek_current_token ()->get_id () == SEMICOLON)
    {
      rust_warning_at (
	parser.peek_current_token ()->get_locus (), 0,
	"trailing semicolon in macro used in expression context");
      parser.skip_token ();
    }

  auto end = lexer.get_offs ();

  return AST::Fragment ({std::move (expr)}, lexer.get_token_slice (start, end));
}

/**
 * Transcribe one type from a macro invocation
 *
 * @param parser Parser to extract statements from
 */
static AST::Fragment
transcribe_type (Parser<MacroInvocLexer> &parser)
{
  auto &lexer = parser.get_token_source ();
  auto start = lexer.get_offs ();

  auto type = parser.parse_type (true);
  for (auto err : parser.get_errors ())
    err.emit ();

  auto end = lexer.get_offs ();

  return AST::Fragment ({std::move (type)}, lexer.get_token_slice (start, end));
}

static AST::Fragment
transcribe_context (MacroExpander::ContextType ctx,
		    Parser<MacroInvocLexer> &parser, bool semicolon,
		    AST::DelimType delimiter, TokenId last_token_id)
{
  // The flow-chart in order to choose a parsing function is as follows:
  //
  // [switch special context]
  //     -- Item --> parser.parse_item();
  //     -- Trait --> parser.parse_trait_item();
  //     -- Impl --> parser.parse_impl_item();
  //     -- Extern --> parser.parse_extern_item();
  //     -- None --> [has semicolon?]
  //                 -- Yes --> parser.parse_stmt();
  //                 -- No --> [switch invocation.delimiter()]
  //                             -- { } --> parser.parse_stmt();
  //                             -- _ --> parser.parse_expr(); // once!

  // If there is a semicolon OR we are expanding a MacroInvocationSemi, then
  // we can parse multiple items. Otherwise, parse *one* expression

  switch (ctx)
    {
    case MacroExpander::ContextType::ITEM:
      return transcribe_many_items (parser, last_token_id);
      break;
    case MacroExpander::ContextType::TRAIT:
      return transcribe_many_trait_items (parser, last_token_id);
      break;
    case MacroExpander::ContextType::IMPL:
      return transcribe_many_impl_items (parser, last_token_id);
      break;
    case MacroExpander::ContextType::TRAIT_IMPL:
      return transcribe_many_trait_impl_items (parser, last_token_id);
      break;
    case MacroExpander::ContextType::EXTERN:
      return transcribe_many_ext (parser, last_token_id);
      break;
    case MacroExpander::ContextType::TYPE:
      return transcribe_type (parser);
      break;
    case MacroExpander::ContextType::STMT:
      return transcribe_many_stmts (parser, last_token_id, semicolon);
    case MacroExpander::ContextType::EXPR:
      return transcribe_expression (parser);
    default:
      rust_unreachable ();
    }
}

static std::string
tokens_to_str (std::vector<std::unique_ptr<AST::Token>> &tokens)
{
  std::string str;
  if (!tokens.empty ())
    {
      str += tokens[0]->as_string ();
      for (size_t i = 1; i < tokens.size (); i++)
	str += " " + tokens[i]->as_string ();
    }

  return str;
}

AST::Fragment
MacroExpander::transcribe_rule (
  AST::MacroRule &match_rule, AST::DelimTokenTree &invoc_token_tree,
  std::map<std::string, MatchedFragmentContainer *> &matched_fragments,
  bool semicolon, ContextType ctx)
{
  // we can manipulate the token tree to substitute the dollar identifiers so
  // that when we call parse its already substituted for us
  AST::MacroTranscriber &transcriber = match_rule.get_transcriber ();
  AST::DelimTokenTree &transcribe_tree = transcriber.get_token_tree ();

  auto invoc_stream = invoc_token_tree.to_token_stream ();
  auto macro_rule_tokens = transcribe_tree.to_token_stream ();

  auto substitute_context
    = SubstituteCtx (invoc_stream, macro_rule_tokens, matched_fragments);
  std::vector<std::unique_ptr<AST::Token>> substituted_tokens
    = substitute_context.substitute_tokens ();

  rust_debug ("substituted tokens: %s",
	      tokens_to_str (substituted_tokens).c_str ());

  // parse it to an Fragment
  MacroInvocLexer lex (std::move (substituted_tokens));
  Parser<MacroInvocLexer> parser (lex);

  auto last_token_id = TokenId::RIGHT_CURLY;

  // this is used so we can check that we delimit the stream correctly.
  switch (transcribe_tree.get_delim_type ())
    {
    case AST::DelimType::PARENS:
      last_token_id = TokenId::RIGHT_PAREN;
      rust_assert (parser.skip_token (LEFT_PAREN));
      break;

    case AST::DelimType::CURLY:
      rust_assert (parser.skip_token (LEFT_CURLY));
      break;

    case AST::DelimType::SQUARE:
      last_token_id = TokenId::RIGHT_SQUARE;
      rust_assert (parser.skip_token (LEFT_SQUARE));
      break;
    }

  // see https://github.com/Rust-GCC/gccrs/issues/22
  // TL;DR:
  //   - Treat all macro invocations with parentheses, (), or square brackets,
  //   [], as expressions.
  //   - If the macro invocation has curly brackets, {}, it may be parsed as a
  //   statement depending on the context.
  //   - If the macro invocation has a semicolon at the end, it must be parsed
  //   as a statement (either via ExpressionStatement or
  //   MacroInvocationWithSemi)

  auto fragment
    = transcribe_context (ctx, parser, semicolon,
			  invoc_token_tree.get_delim_type (), last_token_id);

  // emit any errors
  if (parser.has_errors ())
    {
      for (auto &err : parser.get_errors ())
	rust_error_at (err.locus, "%s", err.message.c_str ());
      return AST::Fragment::create_error ();
    }

  // are all the tokens used?
  bool did_delimit = parser.skip_token (last_token_id);

  bool reached_end_of_stream = did_delimit && parser.skip_token (END_OF_FILE);
  if (!reached_end_of_stream)
    {
      // FIXME: rustc has some cases it accepts this with a warning due to
      // backwards compatibility.
      const_TokenPtr current_token = parser.peek_current_token ();
      rust_error_at (current_token->get_locus (),
		     "tokens here and after are unparsed");
    }

  return fragment;
}

AST::Fragment
MacroExpander::parse_proc_macro_output (ProcMacro::TokenStream ts)
{
  ProcMacroInvocLexer lex (convert (ts));
  Parser<ProcMacroInvocLexer> parser (lex);

  std::vector<AST::SingleASTNode> nodes;
  switch (peek_context ())
    {
    case ContextType::ITEM:
      while (lex.peek_token ()->get_id () != END_OF_FILE)
	{
	  auto result = parser.parse_item (false);
	  if (result == nullptr)
	    break;
	  nodes.push_back ({std::move (result)});
	}
      break;
    case ContextType::STMT:
      while (lex.peek_token ()->get_id () != END_OF_FILE)
	{
	  auto result = parser.parse_stmt ();
	  if (result == nullptr)
	    break;
	  nodes.push_back ({std::move (result)});
	}
      break;
    case ContextType::TRAIT:
    case ContextType::IMPL:
    case ContextType::TRAIT_IMPL:
    case ContextType::EXTERN:
    case ContextType::TYPE:
    case ContextType::EXPR:
    default:
      rust_unreachable ();
    }

  if (parser.has_errors ())
    return AST::Fragment::create_error ();
  else
    return {nodes, std::vector<std::unique_ptr<AST::Token>> ()};
}

MatchedFragment &
MatchedFragmentContainer::get_single_fragment ()
{
  rust_assert (is_single_fragment ());

  return static_cast<MatchedFragmentContainerMetaVar &> (*this).get_fragment ();
}

std::vector<std::unique_ptr<MatchedFragmentContainer>> &
MatchedFragmentContainer::get_fragments ()
{
  rust_assert (!is_single_fragment ());

  return static_cast<MatchedFragmentContainerRepetition &> (*this)
    .get_fragments ();
}

void
MatchedFragmentContainer::add_fragment (MatchedFragment fragment)
{
  rust_assert (!is_single_fragment ());

  return static_cast<MatchedFragmentContainerRepetition &> (*this)
    .add_fragment (fragment);
}

void
MatchedFragmentContainer::add_fragment (
  std::unique_ptr<MatchedFragmentContainer> fragment)
{
  rust_assert (!is_single_fragment ());

  return static_cast<MatchedFragmentContainerRepetition &> (*this)
    .add_fragment (std::move (fragment));
}

std::unique_ptr<MatchedFragmentContainer>
MatchedFragmentContainer::zero ()
{
  return std::unique_ptr<MatchedFragmentContainer> (
    new MatchedFragmentContainerRepetition ());
}

std::unique_ptr<MatchedFragmentContainer>
MatchedFragmentContainer::metavar (MatchedFragment fragment)
{
  return std::unique_ptr<MatchedFragmentContainer> (
    new MatchedFragmentContainerMetaVar (fragment));
}

} // namespace Rust
