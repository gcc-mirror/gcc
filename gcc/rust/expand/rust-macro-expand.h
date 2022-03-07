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

  /* Expands a macro invocation - possibly make both
   * have similar duck-typed interface and use templates?*/
  // should this be public or private?
  void expand_invoc (AST::MacroInvocation &invoc);
  void expand_invoc_semi (AST::MacroInvocation &invoc);

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
   * @param rep Repetition to try and match
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
			AST::MacroMatchRepetition &rep, size_t &match_amount,
			size_t lo_bound = 0, size_t hi_bound = 0);

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
