// Copyright (C) 2020-2022 Free Software Foundation, Inc.

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

#ifndef RUST_MACRO_EXPAND_H
#define RUST_MACRO_EXPAND_H

#include "rust-buffered-queue.h"
#include "rust-parse.h"
#include "rust-token.h"
#include "rust-ast.h"
#include "rust-macro.h"
#include "rust-hir-map.h"
#include "rust-name-resolver.h"
#include "rust-macro-invoc-lexer.h"

// Provides objects and method prototypes for macro expansion

namespace Rust {
// forward decls for AST
namespace AST {
class MacroInvocation;
}

// Object used to store configuration data for macro expansion.
// NOTE: Keep all these items complying with the latest rustc.
struct ExpansionCfg
{
  // features?
  // TODO: Add `features' when we have it.
  unsigned int recursion_limit = 1024;
  bool trace_mac = false;   // trace macro
  bool should_test = false; // strip #[test] nodes if false
  bool keep_macs = false;   // keep macro definitions
  std::string crate_name = "";
};

struct MatchedFragment
{
  std::string fragment_ident;
  size_t token_offset_begin;
  size_t token_offset_end;
  size_t match_amount;

  MatchedFragment (std::string identifier, size_t token_offset_begin,
		   size_t token_offset_end, size_t match_amount = 1)
    : fragment_ident (identifier), token_offset_begin (token_offset_begin),
      token_offset_end (token_offset_end), match_amount (match_amount)
  {}

  /**
   * Create a valid fragment matched zero times. This is useful for repetitions
   * which allow the absence of a fragment, such as * and ?
   */
  static MatchedFragment zero (std::string identifier)
  {
    // We don't need offsets since there is "no match"
    return MatchedFragment (identifier, 0, 0, 0);
  }

  std::string as_string () const
  {
    return fragment_ident + "=" + std::to_string (token_offset_begin) + ":"
	   + std::to_string (token_offset_end) + " (matched "
	   + std::to_string (match_amount) + " times)";
  }

  void set_match_amount (size_t new_amount) { match_amount = new_amount; }
};

class SubstitutionScope
{
public:
  SubstitutionScope () : stack () {}

  void push () { stack.push_back ({}); }

  std::map<std::string, std::vector<MatchedFragment>> pop ()
  {
    auto top = stack.back ();
    stack.pop_back ();
    return top;
  }

  std::map<std::string, std::vector<MatchedFragment>> &peek ()
  {
    return stack.back ();
  }

  void insert_fragment (MatchedFragment fragment)
  {
    auto &current_map = stack.back ();
    auto it = current_map.find (fragment.fragment_ident);

    if (it == current_map.end ())
      {
	auto new_frags = std::vector<MatchedFragment> ();
	new_frags.emplace_back (fragment);
	current_map.insert ({fragment.fragment_ident, new_frags});
      }
    else
      {
	auto &frags = it->second;
	frags.emplace_back (fragment);
      }
  }

private:
  std::vector<std::map<std::string, std::vector<MatchedFragment>>> stack;
};

// Object used to store shared data (between functions) for macro expansion.
struct MacroExpander
{
  enum ContextType
  {
    ITEM,
    BLOCK,
  };

  ExpansionCfg cfg;
  unsigned int expansion_depth = 0;

  MacroExpander (AST::Crate &crate, ExpansionCfg cfg, Session &session)
    : cfg (cfg), crate (crate), session (session),
      sub_stack (SubstitutionScope ()), resolver (Resolver::Resolver::get ()),
      mappings (Analysis::Mappings::get ())
  {}

  ~MacroExpander () = default;

  // Expands all macros in the crate passed in.
  void expand_crate ();

  /* Expands a macro invocation (not macro invocation semi) - possibly make both
   * have similar duck-typed interface and use templates?*/
  // should this be public or private?
  void expand_invoc (AST::MacroInvocation &invoc);

  // Expands a single declarative macro.
  AST::ASTFragment expand_decl_macro (Location locus,
				      AST::MacroInvocData &invoc,
				      AST::MacroRulesDefinition &rules_def,
				      bool semicolon);

  void expand_cfg_attrs (AST::AttrVec &attrs);
  bool fails_cfg (const AST::AttrVec &attr) const;
  bool fails_cfg_with_expand (AST::AttrVec &attrs) const;

  // Expand the data of a cfg! macro.
  void parse_macro_to_meta_item (AST::MacroInvocData &invoc);
  // Get the literal representation of a cfg! macro.
  AST::Literal expand_cfg_macro (AST::MacroInvocData &invoc);

  bool depth_exceeds_recursion_limit () const;

  bool try_match_rule (AST::MacroRule &match_rule,
		       AST::DelimTokenTree &invoc_token_tree);

  AST::ASTFragment transcribe_rule (
    AST::MacroRule &match_rule, AST::DelimTokenTree &invoc_token_tree,
    std::map<std::string, std::vector<MatchedFragment>> &matched_fragments,
    bool semicolon, ContextType ctx);

  bool match_fragment (Parser<MacroInvocLexer> &parser,
		       AST::MacroMatchFragment &fragment);

  bool match_token (Parser<MacroInvocLexer> &parser, AST::Token &token);

  bool match_repetition (Parser<MacroInvocLexer> &parser,
			 AST::MacroMatchRepetition &rep);

  bool match_matcher (Parser<MacroInvocLexer> &parser,
		      AST::MacroMatcher &matcher);

  /**
   * Match any amount of matches
   *
   * @param parser Parser to use for matching
   * @param matches All consecutive matches to identify
   * @param match_amount Reference in which to store the ammount of succesful
   * and valid matches
   *
   * @param lo_bound Lower bound of the matcher. When specified, the matcher
   * will only succeed if it parses at *least* `lo_bound` fragments. If
   * unspecified, the matcher could succeed when parsing 0 fragments.
   *
   * @param hi_bound Higher bound of the matcher. When specified, the matcher
   * will only succeed if it parses *less than* `hi_bound` fragments. If
   * unspecified, the matcher could succeed when parsing an infinity of
   * fragments.
   *
   * @return true if matching was successful and within the given limits, false
   * otherwise
   */
  bool match_n_matches (Parser<MacroInvocLexer> &parser,
			std::vector<std::unique_ptr<AST::MacroMatch>> &matches,
			size_t &match_amount, size_t lo_bound = 0,
			size_t hi_bound = 0);

  /**
   * Substitute a metavariable by its given fragment in a transcribing context,
   * i.e. replacing $var with the associated fragment.
   *
   * @param input Tokens given to the transcribing context
   * @param fragments Fragments given to the macro substitution
   * @param metavar Metavariable to try and replace
   *
   * @return A token containing the associated fragment expanded into tokens if
   * any, or the cloned token if no fragment was associated
   */
  static std::vector<std::unique_ptr<AST::Token>> substitute_metavar (
    std::vector<std::unique_ptr<AST::Token>> &input,
    std::map<std::string, std::vector<MatchedFragment>> &fragments,
    std::unique_ptr<AST::Token> &metavar);

  /**
   * Substitute a macro repetition by its given fragments
   *
   * @param input Tokens given to the transcribing context
   * @param fragments Fragments given to the macro substitution
   * @param pattern_start Start index of the pattern tokens
   * @param pattern_end Index  Amount of tokens in the pattern
   *
   * @return A vector containing the repeated pattern
   */
  static std::vector<std::unique_ptr<AST::Token>> substitute_repetition (
    std::vector<std::unique_ptr<AST::Token>> &input,
    std::vector<std::unique_ptr<AST::Token>> &macro,
    std::map<std::string, std::vector<MatchedFragment>> &fragments,
    size_t pattern_start, size_t pattern_end);

  /**
   * Substitute a given token by its appropriate representation
   *
   * @param macro Tokens used in the macro declaration
   * @param input Tokens given to the transcribing context
   * @param fragments Fragments given to the macro substitution
   * @param token Current token to try and substitute
   *
   * @return A token containing the associated fragment expanded into tokens if
   * any, or the cloned token if no fragment was associated, as well as the
   * amount of tokens that should be skipped before the next invocation. Since
   * this function may consume more than just one token, it is important to skip
   * ahead of the input to avoid mis-substitutions
   */
  static std::pair<std::vector<std::unique_ptr<AST::Token>>, size_t>
  substitute_token (
    std::vector<std::unique_ptr<AST::Token>> &input,
    std::vector<std::unique_ptr<AST::Token>> &macro,
    std::map<std::string, std::vector<MatchedFragment>> &fragments,
    size_t token_idx);

  static std::vector<std::unique_ptr<AST::Token>> substitute_tokens (
    std::vector<std::unique_ptr<AST::Token>> &input,
    std::vector<std::unique_ptr<AST::Token>> &macro,
    std::map<std::string, std::vector<MatchedFragment>> &fragments);

  void push_context (ContextType t) { context.push_back (t); }

  ContextType pop_context ()
  {
    ContextType t = context.back ();
    context.pop_back ();
    return t;
  }

  ContextType peek_context () { return context.back (); }

private:
  AST::Crate &crate;
  Session &session;
  SubstitutionScope sub_stack;
  std::vector<ContextType> context;

public:
  Resolver::Resolver *resolver;
  Analysis::Mappings *mappings;
};

} // namespace Rust

#endif
