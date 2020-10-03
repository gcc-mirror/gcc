#ifndef RUST_AST_MACRO_H
#define RUST_AST_MACRO_H

#include "rust-ast.h"

namespace Rust {
namespace AST {
// Decls as definitions moved to rust-ast.h
class MacroItem;
class MacroInvocationSemi;

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

  void accept_vis (ASTVisitor &vis) override;

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

  void accept_vis (ASTVisitor &vis) override;

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
  DelimType delim_type;
  std::vector<std::unique_ptr<MacroMatch> > matches;

  // TODO: think of way to mark invalid that doesn't take up more space
  bool is_invalid;

  // TODO: should store location information?

public:
  MacroMatcher (DelimType delim_type,
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

  void accept_vis (ASTVisitor &vis) override;

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  MacroMatcher *clone_macro_match_impl () const override
  {
    return new MacroMatcher (*this);
  }

  // constructor only used to create error matcher
  MacroMatcher (bool is_invalid) : delim_type (PARENS), is_invalid (is_invalid)
  {}
};

// TODO: inline?
struct MacroTranscriber
{
private:
  DelimTokenTree token_tree;

  // TODO: should store location information?

public:
  MacroTranscriber (DelimTokenTree token_tree)
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
		      MacroTranscriber (DelimTokenTree::create_empty ()));
  }

  std::string as_string () const;
};

// A macro rules definition item AST node
class MacroRulesDefinition : public MacroItem
{
  Identifier rule_name;
  // MacroRulesDef rules_def; // TODO: inline
  // only curly without required semicolon at end
  DelimType delim_type;
  // MacroRules rules;
  std::vector<MacroRule> rules; // inlined form

  Location locus;

public:
  std::string as_string () const override;

  MacroRulesDefinition (Identifier rule_name, DelimType delim_type,
			std::vector<MacroRule> rules,
			std::vector<Attribute> outer_attrs, Location locus)
    : MacroItem (std::move (outer_attrs)), rule_name (std::move (rule_name)),
      delim_type (delim_type), rules (std::move (rules)), locus (locus)
  {}

  void accept_vis (ASTVisitor &vis) override;

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  MacroRulesDefinition *clone_item_impl () const override
  {
    return new MacroRulesDefinition (*this);
  }
};

/* AST node of a macro invocation, which is replaced by the macro result at
 * compile time */
class MacroInvocation : public TypeNoBounds,
			public Pattern,
			public ExprWithoutBlock
{
  SimplePath path;
  DelimTokenTree token_tree;
  Location locus;

public:
  std::string as_string () const override;

  MacroInvocation (SimplePath path, DelimTokenTree token_tree,
		   std::vector<Attribute> outer_attrs, Location locus)
    : ExprWithoutBlock (std::move (outer_attrs)), path (std::move (path)),
      token_tree (std::move (token_tree)), locus (locus)
  {}

  Location get_locus () const { return locus; }
  Location get_locus_slow () const override { return get_locus (); }

  void accept_vis (ASTVisitor &vis) override;

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

// more generic meta item path-only form
class MetaItemPath : public MetaItem
{
  SimplePath path;

public:
  MetaItemPath (SimplePath path) : path (std::move (path)) {}

  std::string as_string () const override { return path.as_string (); }

  void accept_vis (ASTVisitor &vis) override;

  // HACK: used to simplify parsing - returns non-empty only in this case
  SimplePath to_path_item () const override
  {
    // this should copy construct - TODO ensure it does
    return path;
  }

  bool check_cfg_predicate (const Session &session) const override;

  Attribute to_attribute () const override;

protected:
  // Use covariance to implement clone function as returning this type
  MetaItemPath *clone_meta_item_inner_impl () const override
  {
    return new MetaItemPath (*this);
  }
};

// more generic meta item sequence form
class MetaItemSeq : public MetaItem
{
  SimplePath path;
  std::vector<std::unique_ptr<MetaItemInner> > seq;

public:
  MetaItemSeq (SimplePath path,
	       std::vector<std::unique_ptr<MetaItemInner> > seq)
    : path (std::move (path)), seq (std::move (seq))
  {}

  // copy constructor with vector clone
  MetaItemSeq (const MetaItemSeq &other) : path (other.path)
  {
    seq.reserve (other.seq.size ());
    for (const auto &e : other.seq)
      seq.push_back (e->clone_meta_item_inner ());
  }

  // overloaded assignment operator with vector clone
  MetaItemSeq &operator= (const MetaItemSeq &other)
  {
    MetaItem::operator= (other);
    path = other.path;

    seq.reserve (other.seq.size ());
    for (const auto &e : other.seq)
      seq.push_back (e->clone_meta_item_inner ());

    return *this;
  }

  // default move constructors
  MetaItemSeq (MetaItemSeq &&other) = default;
  MetaItemSeq &operator= (MetaItemSeq &&other) = default;

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

  bool check_cfg_predicate (const Session &session) const override;

  Attribute to_attribute () const override;

protected:
  // Use covariance to implement clone function as returning this type
  MetaItemSeq *clone_meta_item_inner_impl () const override
  {
    return new MetaItemSeq (*this);
  }
};

// Preferred specialisation for single-identifier meta items.
class MetaWord : public MetaItem
{
  Identifier ident;

public:
  MetaWord (Identifier ident) : ident (std::move (ident)) {}

  std::string as_string () const override { return ident; }

  void accept_vis (ASTVisitor &vis) override;

  bool check_cfg_predicate (const Session &session) const override;

  Attribute to_attribute () const override;

protected:
  // Use covariance to implement clone function as returning this type
  MetaWord *clone_meta_item_inner_impl () const override
  {
    return new MetaWord (*this);
  }
};

// Preferred specialisation for "identifier '=' string literal" meta items.
class MetaNameValueStr : public MetaItem
{
  Identifier ident;
  std::string str;

public:
  MetaNameValueStr (Identifier ident, std::string str)
    : ident (std::move (ident)), str (std::move (str))
  {}

  std::string as_string () const override { return ident + " = " + str; }

  void accept_vis (ASTVisitor &vis) override;

  // HACK: used to simplify parsing - creates a copy of this
  std::unique_ptr<MetaNameValueStr> to_meta_name_value_str () const override
  {
    return std::unique_ptr<MetaNameValueStr> (clone_meta_item_inner_impl ());
  }

  bool check_cfg_predicate (const Session &session) const override;

  Attribute to_attribute () const override;

protected:
  // Use covariance to implement clone function as returning this type
  MetaNameValueStr *clone_meta_item_inner_impl () const override
  {
    return new MetaNameValueStr (*this);
  }
};

// doubles up as MetaListIdents - determine via iterating through each path?
// Preferred specialisation for "identifier '(' SimplePath, SimplePath, ... ')'"
class MetaListPaths : public MetaItem
{
  Identifier ident;
  std::vector<SimplePath> paths;

public:
  MetaListPaths (Identifier ident, std::vector<SimplePath> paths)
    : ident (std::move (ident)), paths (std::move (paths))
  {}

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

  bool check_cfg_predicate (const Session &session) const override;

  Attribute to_attribute () const override;

private:
  bool check_path_exists_in_cfg (const Session &session,
				 const SimplePath &path) const;

protected:
  // Use covariance to implement clone function as returning this type
  MetaListPaths *clone_meta_item_inner_impl () const override
  {
    return new MetaListPaths (*this);
  }
};

// Preferred specialisation for "identifier '(' MetaNameValueStr, ... ')'"
class MetaListNameValueStr : public MetaItem
{
  Identifier ident;
  std::vector<MetaNameValueStr> strs;

public:
  MetaListNameValueStr (Identifier ident, std::vector<MetaNameValueStr> strs)
    : ident (std::move (ident)), strs (std::move (strs))
  {}

  std::string as_string () const override;

  void accept_vis (ASTVisitor &vis) override;

  bool check_cfg_predicate (const Session &session) const override;

  Attribute to_attribute () const override;

protected:
  // Use covariance to implement clone function as returning this type
  MetaListNameValueStr *clone_meta_item_inner_impl () const override
  {
    return new MetaListNameValueStr (*this);
  }
};

// Object that parses macros from a token stream.
struct MacroParser
{
private:
  std::vector<std::unique_ptr<Token> > token_stream;
  /* probably have to make this mutable (mutable int stream_pos) otherwise const
   * has to be removed up to DelimTokenTree or further ok since this changing
   * would have an effect on the results of the methods run (i.e. not logically
   * const), the parsing methods shouldn't be const */
  int stream_pos;

public:
  MacroParser (std::vector<std::unique_ptr<Token> > token_stream,
	       int stream_start_pos = 0)
    : token_stream (std::move (token_stream)), stream_pos (stream_start_pos)
  {}

  ~MacroParser () = default;

  std::vector<std::unique_ptr<MetaItemInner> > parse_meta_item_seq ();

private:
  // Parses a MetaItemInner.
  std::unique_ptr<MetaItemInner> parse_meta_item_inner ();
  // Returns whether token can end a meta item.
  bool is_end_meta_item_tok (TokenId id) const;
  // Parses a simple path.
  SimplePath parse_simple_path ();
  // Parses a segment of a simple path (but not scope resolution operator).
  SimplePathSegment parse_simple_path_segment ();
  // Parses a MetaItemLitExpr.
  std::unique_ptr<MetaItemLitExpr> parse_meta_item_lit ();
  // Parses a literal.
  Literal parse_literal ();
  // Parses a meta item that begins with a simple path.
  std::unique_ptr<MetaItem> parse_path_meta_item ();

  // TODO: should this be const?
  std::unique_ptr<Token> &peek_token (int i = 0)
  {
    return token_stream[stream_pos + i];
  }

  void skip_token (int i = 0) { stream_pos += 1 + i; }
};
} // namespace AST
} // namespace Rust

#endif
