// Copyright (C) 2020 Free Software Foundation, Inc.

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

#ifndef RUST_HIR_MACRO_H
#define RUST_HIR_MACRO_H

#include "rust-ast-full-decls.h"
#include "rust-hir.h"

namespace Rust {
namespace HIR {
// Decls as definitions moved to rust-ast.h
class MacroItem;

enum MacroFragSpec
{
  BLOCK,
  EXPR,
  IDENT,
  ITEM,
  LIFETIME,
  LITERAL,
  META,
  PAT,
  PATH,
  STMT,
  TT,
  TY,
  VIS,
  INVALID // not really a specifier, but used to mark invalid one passed in
};

inline MacroFragSpec
get_frag_spec_from_str (std::string str)
{
  if (str == "block")
    return BLOCK;
  else if (str == "expr")
    return EXPR;
  else if (str == "ident")
    return IDENT;
  else if (str == "item")
    return ITEM;
  else if (str == "lifetime")
    return LIFETIME;
  else if (str == "literal")
    return LITERAL;
  else if (str == "meta")
    return META;
  else if (str == "pat")
    return PAT;
  else if (str == "path")
    return PATH;
  else if (str == "stmt")
    return STMT;
  else if (str == "tt")
    return TT;
  else if (str == "ty")
    return TY;
  else if (str == "vis")
    return VIS;
  else
    {
      // error_at("invalid string '%s' used as fragment specifier",
      // str->c_str());
      return INVALID;
    }
}

// A macro match that has an identifier and fragment spec
class MacroMatchFragment : public MacroMatch
{
  Identifier ident;
  MacroFragSpec frag_spec;

  // TODO: should store location information?

public:
  MacroMatchFragment (Identifier ident, MacroFragSpec frag_spec)
    : ident (std::move (ident)), frag_spec (frag_spec)
  {}

  // Returns whether macro match fragment is in an error state.
  bool is_error () const { return frag_spec == INVALID; }

  // Creates an error state macro match fragment.
  static MacroMatchFragment create_error ()
  {
    return MacroMatchFragment (std::string (""), INVALID);
  }

  std::string as_string () const override;

  void accept_vis (HIRVisitor &vis) override;

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  MacroMatchFragment *clone_macro_match_impl () const override
  {
    return new MacroMatchFragment (*this);
  }
};

// A repetition macro match
class MacroMatchRepetition : public MacroMatch
{
public:
  enum MacroRepOp
  {
    NONE,
    ASTERISK,
    PLUS,
    QUESTION_MARK
  };

private:
  std::vector<std::unique_ptr<MacroMatch> > matches;
  MacroRepOp op;

  // bool has_sep;
  typedef Token MacroRepSep;
  // any token except delimiters and repetition operators
  std::unique_ptr<MacroRepSep> sep;

  // TODO: should store location information?

public:
  // Returns whether macro match repetition has separator token.
  bool has_sep () const { return sep != NULL; }

  MacroMatchRepetition (std::vector<std::unique_ptr<MacroMatch> > matches,
			MacroRepOp op, std::unique_ptr<MacroRepSep> sep)
    : matches (std::move (matches)), op (op), sep (std::move (sep))
  {}

  // Copy constructor with clone
  MacroMatchRepetition (MacroMatchRepetition const &other)
    : op (other.op), sep (other.sep->clone_token ())
  {
    matches.reserve (other.matches.size ());
    for (const auto &e : other.matches)
      matches.push_back (e->clone_macro_match ());
  }

  // Overloaded assignment operator to clone
  MacroMatchRepetition &operator= (MacroMatchRepetition const &other)
  {
    op = other.op;
    sep = other.sep->clone_token ();

    matches.reserve (other.matches.size ());
    for (const auto &e : other.matches)
      matches.push_back (e->clone_macro_match ());

    return *this;
  }

  // move constructors
  MacroMatchRepetition (MacroMatchRepetition &&other) = default;
  MacroMatchRepetition &operator= (MacroMatchRepetition &&other) = default;

  std::string as_string () const override;

  void accept_vis (HIRVisitor &vis) override;

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  MacroMatchRepetition *clone_macro_match_impl () const override
  {
    return new MacroMatchRepetition (*this);
  }
};

// can't inline due to polymorphism
class MacroMatcher : public MacroMatch
{
  AST::DelimType delim_type;
  std::vector<std::unique_ptr<MacroMatch> > matches;

  // TODO: think of way to mark invalid that doesn't take up more space
  bool is_invalid;

  // TODO: should store location information?

public:
  MacroMatcher (AST::DelimType delim_type,
		std::vector<std::unique_ptr<MacroMatch> > matches)
    : delim_type (delim_type), matches (std::move (matches)), is_invalid (false)
  {}

  // copy constructor with vector clone
  MacroMatcher (MacroMatcher const &other) : delim_type (other.delim_type)
  {
    matches.reserve (other.matches.size ());
    for (const auto &e : other.matches)
      matches.push_back (e->clone_macro_match ());
  }

  // overloaded assignment operator with vector clone
  MacroMatcher &operator= (MacroMatcher const &other)
  {
    delim_type = other.delim_type;

    matches.reserve (other.matches.size ());
    for (const auto &e : other.matches)
      matches.push_back (e->clone_macro_match ());

    return *this;
  }

  // move constructors
  MacroMatcher (MacroMatcher &&other) = default;
  MacroMatcher &operator= (MacroMatcher &&other) = default;

  // Creates an error state macro matcher.
  static MacroMatcher create_error () { return MacroMatcher (true); }

  // Returns whether MacroMatcher is in an error state.
  bool is_error () const { return is_invalid; }

  std::string as_string () const override;

  void accept_vis (HIRVisitor &vis) override;

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  MacroMatcher *clone_macro_match_impl () const override
  {
    return new MacroMatcher (*this);
  }

  // constructor only used to create error matcher
  MacroMatcher (bool is_invalid)
    : delim_type (AST::DelimType::PARENS), is_invalid (is_invalid)
  {}
};

// TODO: inline?
struct MacroTranscriber
{
private:
  AST::DelimTokenTree token_tree;

  // TODO: should store location information?

public:
  MacroTranscriber (AST::DelimTokenTree token_tree)
    : token_tree (std::move (token_tree))
  {}

  std::string as_string () const { return token_tree.as_string (); }
};

// A macro rule? Matcher and transcriber pair?
struct MacroRule
{
private:
  MacroMatcher matcher;
  MacroTranscriber transcriber;

  // TODO: should store location information?

public:
  MacroRule (MacroMatcher matcher, MacroTranscriber transcriber)
    : matcher (std::move (matcher)), transcriber (std::move (transcriber))
  {}

  // Returns whether macro rule is in error state.
  bool is_error () const { return matcher.is_error (); }

  // Creates an error state macro rule.
  static MacroRule create_error ()
  {
    return MacroRule (MacroMatcher::create_error (),
		      MacroTranscriber (AST::DelimTokenTree::create_empty ()));
  }

  std::string as_string () const;
};

// A macro rules definition item HIR node
class MacroRulesDefinition : public MacroItem
{
  Identifier rule_name;
  // MacroRulesDef rules_def; // TODO: inline
  // only curly without required semicolon at end
  AST::DelimType delim_type;
  // MacroRules rules;
  std::vector<MacroRule> rules; // inlined form

  Location locus;

public:
  std::string as_string () const override;

  MacroRulesDefinition (Analysis::NodeMapping mappings, Identifier rule_name,
			AST::DelimType delim_type, std::vector<MacroRule> rules,
			AST::AttrVec outer_attrs, Location locus)
    : MacroItem (std::move (mappings), std::move (outer_attrs)),
      rule_name (std::move (rule_name)), delim_type (delim_type),
      rules (std::move (rules)), locus (locus)
  {}

  void accept_vis (HIRVisitor &vis) override;

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  MacroRulesDefinition *clone_item_impl () const override
  {
    return new MacroRulesDefinition (*this);
  }
};

/* HIR node of a macro invocation, which is replaced by the macro result at
 * compile time */
class MacroInvocation : public TypeNoBounds,
			public Pattern,
			public ExprWithoutBlock
{
  AST::SimplePath path;
  AST::DelimTokenTree token_tree;
  Location locus;

public:
  std::string as_string () const override;

  MacroInvocation (Analysis::NodeMapping mappings, AST::SimplePath path,
		   AST::DelimTokenTree token_tree, AST::AttrVec outer_attrs,
		   Location locus)
    : TypeNoBounds (mappings),
      ExprWithoutBlock (std::move (mappings), std::move (outer_attrs)),
      path (std::move (path)), token_tree (std::move (token_tree)),
      locus (locus)
  {}

  Location get_locus () const { return locus; }
  Location get_locus_slow () const override { return get_locus (); }

  void accept_vis (HIRVisitor &vis) override;

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  MacroInvocation *clone_pattern_impl () const override
  {
    return new MacroInvocation (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  MacroInvocation *clone_expr_impl () const override
  {
    return new MacroInvocation (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  MacroInvocation *clone_expr_without_block_impl () const override
  {
    return new MacroInvocation (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  MacroInvocation *clone_type_impl () const override
  {
    return new MacroInvocation (*this);
  }

  /* Use covariance to implement clone function as returning this object rather
   * than base */
  MacroInvocation *clone_type_no_bounds_impl () const override
  {
    return new MacroInvocation (*this);
  }
};

} // namespace HIR
} // namespace Rust

#endif
