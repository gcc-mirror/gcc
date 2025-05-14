/* This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>. */

#include "rust-parse.h"
#include "rust-linemap.h"
#include "rust-diagnostics.h"
#include "rust-token.h"
#include "rust-attribute-values.h"

namespace Rust {

std::string
extract_module_path (const AST::AttrVec &inner_attrs,
		     const AST::AttrVec &outer_attrs, const std::string &name)
{
  AST::Attribute path_attr = AST::Attribute::create_empty ();
  for (const auto &attr : inner_attrs)
    {
      if (attr.get_path ().as_string () == Values::Attributes::PATH)
	{
	  path_attr = attr;
	  break;
	}
    }

  // Here, we found a path attribute, but it has no associated string. This is
  // invalid
  if (!path_attr.is_empty () && !path_attr.has_attr_input ())
    {
      rust_error_at (
	path_attr.get_locus (),
	// Split the format string so that -Wformat-diag does not complain...
	"path attributes must contain a filename: '%s'", "#![path = \"file\"]");
      return name;
    }

  for (const auto &attr : outer_attrs)
    {
      if (attr.get_path ().as_string () == Values::Attributes::PATH)
	{
	  path_attr = attr;
	  break;
	}
    }

  // We didn't find a path attribute. This is not an error, there simply isn't
  // one present
  if (path_attr.is_empty ())
    return name;

  // Here, we found a path attribute, but it has no associated string. This is
  // invalid
  if (!path_attr.has_attr_input ())
    {
      rust_error_at (
	path_attr.get_locus (),
	// Split the format string so that -Wformat-diag does not complain...
	"path attributes must contain a filename: '%s'", "#[path = \"file\"]");
      return name;
    }

  auto path_value = path_attr.get_attr_input ().as_string ();

  // At this point, the 'path' is of the following format: '= "<file.rs>"'
  // We need to remove the equal sign and only keep the actual filename.
  // In order to do this, we can simply go through the string until we find
  // a character that is not an equal sign or whitespace
  auto filename_begin = path_value.find_first_not_of ("=\t ");

  auto path = path_value.substr (filename_begin);

  // On windows, the path might mix '/' and '\' separators. Replace the
  // UNIX-like separators by MSDOS separators to make sure the path will resolve
  // properly.
  //
  // Source: rustc compiler
  // (https://github.com/rust-lang/rust/blob/9863bf51a52b8e61bcad312f81b5193d53099f9f/compiler/rustc_expand/src/module.rs#L174)
#if defined(HAVE_DOS_BASED_FILE_SYSTEM)
  std::replace (path.begin (), path.end (), '/', '\\');
#endif /* HAVE_DOS_BASED_FILE_SYSTEM */

  return path;
}

template <typename T>
static bool
contains (std::vector<T> &vec, T elm)
{
  return std::find (vec.begin (), vec.end (), elm) != vec.end ();
}

/**
 * Avoid UB by calling .front() and .back() on empty containers...
 */

template <typename T>
static const T *
get_back_ptr (const std::vector<std::unique_ptr<T>> &values)
{
  if (values.empty ())
    return nullptr;

  return values.back ().get ();
}

template <typename T>
static const T *
get_front_ptr (const std::vector<std::unique_ptr<T>> &values)
{
  if (values.empty ())
    return nullptr;

  return values.front ().get ();
}

static bool
peculiar_fragment_match_compatible_fragment (
  const AST::MacroFragSpec &last_spec, const AST::MacroFragSpec &spec,
  location_t match_locus)
{
  static std::unordered_map<AST::MacroFragSpec::Kind,
			    std::vector<AST::MacroFragSpec::Kind>>
    fragment_follow_set
    = {{AST::MacroFragSpec::PATH, {AST::MacroFragSpec::BLOCK}},
       {AST::MacroFragSpec::TY, {AST::MacroFragSpec::BLOCK}},
       {AST::MacroFragSpec::VIS,
	{AST::MacroFragSpec::IDENT, AST::MacroFragSpec::TY,
	 AST::MacroFragSpec::PATH}}};

  auto is_valid
    = contains (fragment_follow_set[last_spec.get_kind ()], spec.get_kind ());

  if (!is_valid)
    rust_error_at (
      match_locus,
      "fragment specifier %qs is not allowed after %qs fragments",
      spec.as_string ().c_str (), last_spec.as_string ().c_str ());

  return is_valid;
}

static bool
peculiar_fragment_match_compatible (const AST::MacroMatchFragment &last_match,
				    const AST::MacroMatch &match)
{
  static std::unordered_map<AST::MacroFragSpec::Kind, std::vector<TokenId>>
    follow_set
    = {{AST::MacroFragSpec::EXPR, {MATCH_ARROW, COMMA, SEMICOLON}},
       {AST::MacroFragSpec::STMT, {MATCH_ARROW, COMMA, SEMICOLON}},
       {AST::MacroFragSpec::PAT, {MATCH_ARROW, COMMA, EQUAL, PIPE, IF, IN}},
       {AST::MacroFragSpec::PATH,
	{MATCH_ARROW, COMMA, EQUAL, PIPE, SEMICOLON, COLON, RIGHT_ANGLE,
	 RIGHT_SHIFT, LEFT_SQUARE, LEFT_CURLY, AS, WHERE}},
       {AST::MacroFragSpec::TY,
	{MATCH_ARROW, COMMA, EQUAL, PIPE, SEMICOLON, COLON, RIGHT_ANGLE,
	 RIGHT_SHIFT, LEFT_SQUARE, LEFT_CURLY, AS, WHERE}},
       {AST::MacroFragSpec::VIS,
	{COMMA,
	 IDENTIFIER,
	 LEFT_PAREN,
	 LEFT_SQUARE,
	 EXCLAM,
	 ASTERISK,
	 AMP,
	 LOGICAL_AND,
	 QUESTION_MARK,
	 LIFETIME,
	 LEFT_ANGLE,
	 LEFT_SHIFT,
	 UNDERSCORE,
	 ABSTRACT,
	 AS,
	 ASYNC,
	 AUTO,
	 BECOME,
	 BOX,
	 BREAK,
	 CONST,
	 CONTINUE,
	 CRATE,
	 DO,
	 DYN,
	 ELSE,
	 ENUM_KW,
	 EXTERN_KW,
	 FALSE_LITERAL,
	 FINAL_KW,
	 FN_KW,
	 FOR,
	 IF,
	 IMPL,
	 IN,
	 LET,
	 LOOP,
	 MACRO,
	 MATCH_KW,
	 MOD,
	 MOVE,
	 MUT,
	 OVERRIDE_KW,
	 PUB,
	 REF,
	 RETURN_KW,
	 SELF_ALIAS,
	 SELF,
	 STATIC_KW,
	 STRUCT_KW,
	 SUPER,
	 TRAIT,
	 TRUE_LITERAL,
	 TRY,
	 TYPE,
	 TYPEOF,
	 UNSAFE,
	 UNSIZED,
	 USE,
	 VIRTUAL,
	 WHERE,
	 WHILE,
	 YIELD}}};

  location_t error_locus = match.get_match_locus ();
  std::string kind_str = "fragment";
  auto &allowed_toks = follow_set[last_match.get_frag_spec ().get_kind ()];

  // There are two behaviors to handle here: If the follow-up match is a token,
  // we want to check if it is allowed.
  // If it is a fragment, repetition or matcher then we know that it will be
  // an error.
  // For repetitions and matchers we want to extract a proper location to report
  // the error.
  switch (match.get_macro_match_type ())
    {
      case AST::MacroMatch::Tok: {
	auto tok = static_cast<const AST::Token *> (&match);
	if (contains (allowed_toks, tok->get_id ()))
	  return true;
	kind_str = "token `"
		   + std::string (get_token_description (tok->get_id ())) + "`";
	error_locus = tok->get_match_locus ();
	break;
      }
      break;
      case AST::MacroMatch::Repetition: {
	auto repetition
	  = static_cast<const AST::MacroMatchRepetition *> (&match);
	auto &matches = repetition->get_matches ();
	auto first_frag = get_front_ptr (matches);
	if (first_frag)
	  return peculiar_fragment_match_compatible (last_match, *first_frag);
	break;
      }
      case AST::MacroMatch::Matcher: {
	auto matcher = static_cast<const AST::MacroMatcher *> (&match);
	auto first_token = matcher->get_delim_type ();
	TokenId delim_id;
	switch (first_token)
	  {
	  case AST::PARENS:
	    delim_id = LEFT_PAREN;
	    break;
	  case AST::SQUARE:
	    delim_id = LEFT_SQUARE;
	    break;
	  case AST::CURLY:
	    delim_id = LEFT_CURLY;
	    break;
	  default:
	    rust_unreachable ();
	    break;
	  }
	if (contains (allowed_toks, delim_id))
	  return true;
	kind_str = "token `" + std::string (get_token_description (delim_id))
		   + "` at start of matcher";
	error_locus = matcher->get_match_locus ();
	break;
      }
      case AST::MacroMatch::Fragment: {
	auto last_spec = last_match.get_frag_spec ();
	auto fragment = static_cast<const AST::MacroMatchFragment *> (&match);
	if (last_spec.has_follow_set_fragment_restrictions ())
	  return peculiar_fragment_match_compatible_fragment (
	    last_spec, fragment->get_frag_spec (), match.get_match_locus ());
      }
      break;
    }

  rust_error_at (error_locus, "%s is not allowed after %qs fragment",
		 kind_str.c_str (),
		 last_match.get_frag_spec ().as_string ().c_str ());
  auto allowed_toks_str
    = "`" + std::string (get_token_description (allowed_toks[0])) + "`";
  for (size_t i = 1; i < allowed_toks.size (); i++)
    allowed_toks_str
      += ", `" + std::string (get_token_description (allowed_toks[i])) + "`";

  rust_inform (error_locus, "allowed tokens are %s", allowed_toks_str.c_str ());

  return false;
}

bool
is_match_compatible (const AST::MacroMatch &last_match,
		     const AST::MacroMatch &match)
{
  const AST::MacroMatch *new_last = nullptr;

  // We want to "extract" the concerning matches. In cases such as matchers and
  // repetitions, we actually store multiple matchers, but are only concerned
  // about the follow-set ambiguities of certain elements.
  // There are some cases where we can short-circuit the algorithm: There will
  // never be restrictions on token literals, or on certain fragments which do
  // not have a set of follow-restrictions.

  switch (last_match.get_macro_match_type ())
    {
      // This is our main stop condition: When we are finally looking at the
      // last match (or its actual last component), and it is a fragment, it
      // may contain some follow up restrictions.
      case AST::MacroMatch::Fragment: {
	auto fragment
	  = static_cast<const AST::MacroMatchFragment *> (&last_match);
	if (fragment->get_frag_spec ().has_follow_set_restrictions ())
	  return peculiar_fragment_match_compatible (*fragment, match);
	else
	  return true;
      }
      case AST::MacroMatch::Repetition: {
	// A repetition on the left hand side means we want to make sure the
	// last match of the repetition is compatible with the new match
	auto repetition
	  = static_cast<const AST::MacroMatchRepetition *> (&last_match);
	new_last = get_back_ptr (repetition->get_matches ());
	// If there are no matches in the matcher, then it can be followed by
	// anything
	if (!new_last)
	  return true;
	break;
      }
    case AST::MacroMatch::Matcher:
    case AST::MacroMatch::Tok:
      return true;
    }

  rust_assert (new_last);

  // We check recursively until we find a terminating condition
  // FIXME: Does expansion depth/limit matter here?
  return is_match_compatible (*new_last, match);
}
} // namespace Rust
