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

namespace Rust {

std::string
extract_module_path (const AST::AttrVec &inner_attrs,
		     const AST::AttrVec &outer_attrs, const std::string &name)
{
  AST::Attribute path_attr = AST::Attribute::create_empty ();
  for (const auto &attr : inner_attrs)
    {
      if (attr.get_path ().as_string () == "path")
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
      if (attr.get_path ().as_string () == "path")
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
  path.replace ('/', '\\');
#endif /* HAVE_DOS_BASED_FILE_SYSTEM */

  return path;
}

static bool
peculiar_fragment_match_compatible (AST::MacroMatchFragment &last_match,
				    AST::MacroMatch &match)
{
  static std::unordered_map<AST::MacroFragSpec::Kind, std::vector<TokenId>>
    follow_set = {
      {AST::MacroFragSpec::EXPR, {MATCH_ARROW, COMMA, SEMICOLON}},
      {AST::MacroFragSpec::STMT, {MATCH_ARROW, COMMA, SEMICOLON}},
    };

  Location error_locus = match.get_match_locus ();

  // There are two behaviors to handle here: If the follow-up match is a token,
  // we want to check if it is allowed.
  // If it is a fragment, repetition or matcher then we know that it will be
  // an error.
  // For repetitions and matchers we want to extract a proper location to report
  // the error.
  switch (match.get_macro_match_type ())
    {
      case AST::MacroMatch::Tok: {
	auto tok = static_cast<AST::Token *> (&match);
	auto &allowed_toks
	  = follow_set[last_match.get_frag_spec ().get_kind ()];
	auto is_valid = std::find (allowed_toks.begin (), allowed_toks.end (),
				   tok->get_id ())
			!= allowed_toks.end ();
	if (!is_valid)
	  // FIXME: Add hint about allowed fragments
	  rust_error_at (tok->get_match_locus (),
			 "token %<%s%> is not allowed after %<%s%> fragment",
			 tok->get_str ().c_str (),
			 last_match.get_frag_spec ().as_string ().c_str ());
	return is_valid;
      }
      break;
      case AST::MacroMatch::Repetition: {
	auto repetition = static_cast<AST::MacroMatchRepetition *> (&match);
	auto &matches = repetition->get_matches ();
	if (!matches.empty ())
	  error_locus = matches.front ()->get_match_locus ();
	break;
      }
      case AST::MacroMatch::Matcher: {
	auto matcher = static_cast<AST::MacroMatcher *> (&match);
	auto &matches = matcher->get_matches ();
	if (!matches.empty ())
	  error_locus = matches.front ()->get_match_locus ();
	break;
      }
    default:
      break;
    }

  rust_error_at (error_locus, "fragment not allowed after %<%s%> fragment",
		 last_match.get_frag_spec ().as_string ().c_str ());

  return false;
}

/**
 * Avoid UB by calling .front() and .back() on empty containers...
 */

template <typename T>
static T *
get_back_ptr (std::vector<std::unique_ptr<T>> &values)
{
  if (values.empty ())
    return nullptr;

  return values.back ().get ();
}

template <typename T>
static T *
get_front_ptr (std::vector<std::unique_ptr<T>> &values)
{
  if (values.empty ())
    return nullptr;

  return values.front ().get ();
}

bool
is_match_compatible (AST::MacroMatch &last_match, AST::MacroMatch &match)
{
  AST::MacroMatch *new_last = nullptr;

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
	auto fragment = static_cast<AST::MacroMatchFragment *> (&last_match);
	if (fragment->get_frag_spec ().has_follow_set_restrictions ())
	  return peculiar_fragment_match_compatible (*fragment, match);
	else
	  return true;
      }
      case AST::MacroMatch::Repetition: {
	// A repetition on the left hand side means we want to make sure the
	// last match of the repetition is compatible with the new match
	auto repetition
	  = static_cast<AST::MacroMatchRepetition *> (&last_match);
	new_last = get_back_ptr (repetition->get_matches ());
	// If there are no matches in the matcher, then it can be followed by
	// anything
	if (!new_last)
	  return true;
	break;
      }
      case AST::MacroMatch::Matcher: {
	// Likewise for another matcher
	auto matcher = static_cast<AST::MacroMatcher *> (&last_match);
	new_last = get_back_ptr (matcher->get_matches ());
	// If there are no matches in the matcher, then it can be followed by
	// anything
	if (!new_last)
	  return true;
	break;
      }
    case AST::MacroMatch::Tok:
      return true;
    }

  rust_assert (new_last);

  // We check recursively until we find a terminating condition
  // FIXME: Does expansion depth/limit matter here?
  return is_match_compatible (*new_last, match);
}
} // namespace Rust
