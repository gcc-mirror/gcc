// Copyright (C) 2020-2023 Free Software Foundation, Inc.

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
#include "rust-early-name-resolver.h"
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

  MatchedFragment (std::string identifier, size_t token_offset_begin,
		   size_t token_offset_end)
    : fragment_ident (identifier), token_offset_begin (token_offset_begin),
      token_offset_end (token_offset_end)
  {}

  /**
   * Empty constructor for uninitialized fragments
   */
  MatchedFragment () : MatchedFragment ("", 0, 0) {}

  std::string as_string () const
  {
    return fragment_ident + "=" + std::to_string (token_offset_begin) + ":"
	   + std::to_string (token_offset_end);
  }
};

class MatchedFragmentContainer
{
public:
  // Does the container refer to a simple metavariable, different from a
  // repetition repeated once
  enum class Kind
  {
    MetaVar,
    Repetition,
  };

  MatchedFragmentContainer (std::vector<MatchedFragment> fragments,
			    Kind kind = Kind::Repetition)
    : fragments (fragments), kind (kind)
  {}

  /**
   * Create a valid fragment matched zero times. This is useful for repetitions
   * which allow the absence of a fragment, such as * and ?
   */
  static MatchedFragmentContainer zero ()
  {
    return MatchedFragmentContainer ({});
  }

  /**
   * Create a valid fragment matched one time
   */
  static MatchedFragmentContainer metavar (MatchedFragment fragment)
  {
    return MatchedFragmentContainer ({fragment}, Kind::MetaVar);
  }

  /**
   * Add a matched fragment to the container
   */
  void add_fragment (MatchedFragment fragment)
  {
    rust_assert (!is_single_fragment ());

    fragments.emplace_back (fragment);
  }

  size_t get_match_amount () const { return fragments.size (); }
  const std::vector<MatchedFragment> &get_fragments () const
  {
    return fragments;
  }
  // const std::string &get_fragment_name () const { return fragment_name; }

  bool is_single_fragment () const
  {
    return get_match_amount () == 1 && kind == Kind::MetaVar;
  }

  const MatchedFragment get_single_fragment () const
  {
    rust_assert (is_single_fragment ());

    return fragments[0];
  }

  const Kind &get_kind () const { return kind; }

private:
  /**
   * Fragments matched `match_amount` times. This can be an empty vector
   * in case having zero matches is allowed (i.e ? or * operators)
   */
  std::vector<MatchedFragment> fragments;
  Kind kind;
};

class SubstitutionScope
{
public:
  SubstitutionScope () : stack () {}

  void push () { stack.push_back ({}); }

  std::map<std::string, MatchedFragmentContainer> pop ()
  {
    auto top = stack.back ();
    stack.pop_back ();
    return top;
  }

  std::map<std::string, MatchedFragmentContainer> &peek ()
  {
    return stack.back ();
  }

  /**
   * Insert a new matched metavar into the current substitution map
   */
  void insert_metavar (MatchedFragment fragment)
  {
    auto &current_map = stack.back ();
    auto it = current_map.find (fragment.fragment_ident);

    if (it == current_map.end ())
      current_map.insert ({fragment.fragment_ident,
			   MatchedFragmentContainer::metavar (fragment)});
    else
      gcc_unreachable ();
  }

  /**
   * Append a new matched fragment to a repetition into the current substitution
   * map
   */
  void append_fragment (MatchedFragment fragment)
  {
    auto &current_map = stack.back ();
    auto it = current_map.find (fragment.fragment_ident);

    if (it == current_map.end ())
      current_map.insert (
	{fragment.fragment_ident, MatchedFragmentContainer ({fragment})});
    else
      it->second.add_fragment (fragment);
  }

  void insert_matches (std::string key, MatchedFragmentContainer matches)
  {
    auto &current_map = stack.back ();
    auto it = current_map.find (key);
    rust_assert (it == current_map.end ());

    current_map.insert ({key, matches});
  }

private:
  std::vector<std::map<std::string, MatchedFragmentContainer>> stack;
};

// Object used to store shared data (between functions) for macro expansion.
struct MacroExpander
{
  enum ContextType
  {
    ITEM,
    BLOCK,
    EXTERN,
    TYPE,
    TRAIT,
    IMPL,
    TRAIT_IMPL,
  };

  ExpansionCfg cfg;
  unsigned int expansion_depth = 0;

  MacroExpander (AST::Crate &crate, ExpansionCfg cfg, Session &session)
    : cfg (cfg), crate (crate), session (session),
      sub_stack (SubstitutionScope ()),
      expanded_fragment (AST::Fragment::create_error ()),
      has_changed_flag (false), resolver (Resolver::Resolver::get ()),
      mappings (Analysis::Mappings::get ())
  {}

  ~MacroExpander () = default;

  // Expands all macros in the crate passed in.
  void expand_crate ();

  /**
   * Expand the eager invocations contained within a builtin macro invocation.
   * Called by `expand_invoc` when expanding builtin invocations.
   */
  void expand_eager_invocations (AST::MacroInvocation &invoc);

  /* Expands a macro invocation - possibly make both
   * have similar duck-typed interface and use templates?*/
  // should this be public or private?
  void expand_invoc (AST::MacroInvocation &invoc, bool has_semicolon);

  // Expands a single declarative macro.
  AST::Fragment expand_decl_macro (Location locus, AST::MacroInvocData &invoc,
				   AST::MacroRulesDefinition &rules_def,
				   bool semicolon);

  void expand_cfg_attrs (AST::AttrVec &attrs);
  bool fails_cfg (const AST::AttrVec &attr) const;
  bool fails_cfg_with_expand (AST::AttrVec &attrs) const;

  bool depth_exceeds_recursion_limit () const;

  bool try_match_rule (AST::MacroRule &match_rule,
		       AST::DelimTokenTree &invoc_token_tree);

  AST::Fragment transcribe_rule (
    AST::MacroRule &match_rule, AST::DelimTokenTree &invoc_token_tree,
    std::map<std::string, MatchedFragmentContainer> &matched_fragments,
    bool semicolon, ContextType ctx);

  bool match_fragment (Parser<MacroInvocLexer> &parser,
		       AST::MacroMatchFragment &fragment);

  bool match_token (Parser<MacroInvocLexer> &parser, AST::Token &token);

  bool match_repetition (Parser<MacroInvocLexer> &parser,
			 AST::MacroMatchRepetition &rep);

  bool match_matcher (Parser<MacroInvocLexer> &parser,
		      AST::MacroMatcher &matcher, bool in_repetition = false);

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
    rust_assert (!context.empty ());

    ContextType t = context.back ();
    context.pop_back ();

    return t;
  }

  ContextType peek_context () { return context.back (); }

  void set_expanded_fragment (AST::Fragment &&fragment)
  {
    if (!fragment.is_error ())
      has_changed_flag = true;

    expanded_fragment = std::move (fragment);
  }

  AST::Fragment take_expanded_fragment ()
  {
    auto fragment = std::move (expanded_fragment);
    expanded_fragment = AST::Fragment::create_error ();

    return fragment;
  }

  /**
   * Has the MacroExpander expanded a macro since its state was last reset?
   */
  bool has_changed () const { return has_changed_flag; }

  /**
   * Reset the expander's "changed" state. This function should be executed at
   * each iteration in a fixed point loop
   */
  void reset_changed_state () { has_changed_flag = false; }

  AST::MacroRulesDefinition *get_last_definition () { return last_def; }
  AST::MacroInvocation *get_last_invocation () { return last_invoc; }

private:
  AST::Crate &crate;
  Session &session;
  SubstitutionScope sub_stack;
  std::vector<ContextType> context;
  AST::Fragment expanded_fragment;
  bool has_changed_flag;

  AST::MacroRulesDefinition *last_def;
  AST::MacroInvocation *last_invoc;

public:
  Resolver::Resolver *resolver;
  Analysis::Mappings *mappings;
};

} // namespace Rust

#endif
