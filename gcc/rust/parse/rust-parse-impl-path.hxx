// Copyright (C) 2025 Free Software Foundation, Inc.

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

/* DO NOT INCLUDE ANYWHERE - this is automatically included
 *   by rust-parse-impl.h
 * This is also the reason why there are no include guards. */

#include "rust-parse.h"

namespace Rust {

// Parses a SimplePath AST node, if it exists. Does nothing otherwise.
template <typename ManagedTokenSource>
tl::expected<AST::SimplePath, Parse::Error::SimplePath>
Parser<ManagedTokenSource>::parse_simple_path ()
{
  bool has_opening_scope_resolution = false;
  location_t locus = UNKNOWN_LOCATION;

  using Parse::Utils::is_simple_path_segment;

  // don't parse anything if not a path upfront
  if (!is_simple_path_segment (lexer.peek_token ()->get_id ())
      && !is_simple_path_segment (lexer.peek_token (1)->get_id ()))
    return Parse::Error::SimplePath::make_malformed ();

  /* Checks for opening scope resolution (i.e. global scope fully-qualified
   * path) */
  if (lexer.peek_token ()->get_id () == SCOPE_RESOLUTION)
    {
      has_opening_scope_resolution = true;

      locus = lexer.peek_token ()->get_locus ();

      lexer.skip_token ();
    }

  // Parse single required simple path segment
  auto segment = parse_simple_path_segment ();

  if (!segment)
    return Parse::Error::SimplePath::make_malformed ();

  // get location if not gotten already
  if (locus == UNKNOWN_LOCATION)
    locus = segment->get_locus ();

  std::vector<AST::SimplePathSegment> segments;
  segments.push_back (std::move (segment.value ()));

  // Parse all other simple path segments
  while (lexer.peek_token ()->get_id () == SCOPE_RESOLUTION)
    {
      auto new_segment = parse_simple_path_segment (1);

      using Error = Parse::Error::SimplePathSegment::Kind;
      // Return path as currently constructed if segment in error state.
      if (!new_segment)
	{
	  if (new_segment.error ().kind == Error::INVALID_SIMPLE_PATH_TOKEN)
	    break; /* Could be end of path */
	  else	   /* Any other error is an hard error */
	    return Parse::Error::SimplePath::make_malformed ();
	}

      segments.push_back (std::move (new_segment.value ()));
    }

  return AST::SimplePath (std::move (segments), has_opening_scope_resolution,
			  locus);
  /* TODO: now that is_simple_path_segment exists, could probably start
   * actually making errors upon parse failure of segments and whatever */
}

/* Parses a single SimplePathSegment (does not handle the scope resolution
 * operators)
 * Starts parsing at an offset of base_peek */
template <typename ManagedTokenSource>
tl::expected<AST::SimplePathSegment, Parse::Error::SimplePathSegment>
Parser<ManagedTokenSource>::parse_simple_path_segment (int base_peek)
{
  using namespace Values;
  const_TokenPtr t = lexer.peek_token (base_peek);
  switch (t->get_id ())
    {
    case IDENTIFIER:
      lexer.skip_token (base_peek);

      return AST::SimplePathSegment (t->get_str (), t->get_locus ());
    case SUPER:
      lexer.skip_token (base_peek);

      return AST::SimplePathSegment (Keywords::SUPER, t->get_locus ());
    case SELF:
      lexer.skip_token (base_peek);

      return AST::SimplePathSegment (Keywords::SELF, t->get_locus ());
    case CRATE:
      lexer.skip_token (base_peek);

      return AST::SimplePathSegment (Keywords::CRATE, t->get_locus ());
    case DOLLAR_SIGN:
      if (lexer.peek_token (base_peek + 1)->get_id () == CRATE)
	{
	  lexer.skip_token (base_peek + 1);

	  return AST::SimplePathSegment ("$crate", t->get_locus ());
	}
      gcc_fallthrough ();
    default:
      // do nothing but inactivates warning from gcc when compiling
      /* could put the rust_error_at thing here but fallthrough (from failing
       * $crate condition) isn't completely obvious if it is. */

      return Parse::Error::SimplePathSegment::make_invalid_token_or_path_end ();
    }
  rust_unreachable ();
}

// Parses a PathIdentSegment - an identifier segment of a non-SimplePath path.
template <typename ManagedTokenSource>
tl::expected<AST::PathIdentSegment, Parse::Error::PathIdentSegment>
Parser<ManagedTokenSource>::parse_path_ident_segment ()
{
  const_TokenPtr t = lexer.peek_token ();
  switch (t->get_id ())
    {
    case IDENTIFIER:
      lexer.skip_token ();

      return AST::PathIdentSegment (t->get_str (), t->get_locus ());
    case SUPER:
      lexer.skip_token ();

      return AST::PathIdentSegment (Values::Keywords::SUPER, t->get_locus ());
    case SELF:
      lexer.skip_token ();

      return AST::PathIdentSegment (Values::Keywords::SELF, t->get_locus ());
    case SELF_ALIAS:
      lexer.skip_token ();

      return AST::PathIdentSegment (Values::Keywords::SELF_ALIAS,
				    t->get_locus ());
    case CRATE:
      lexer.skip_token ();

      return AST::PathIdentSegment (Values::Keywords::CRATE, t->get_locus ());
    case DOLLAR_SIGN:
      if (lexer.peek_token (1)->get_id () == CRATE)
	{
	  lexer.skip_token (1);

	  return AST::PathIdentSegment ("$crate", t->get_locus ());
	}
      gcc_fallthrough ();
    default:
      /* do nothing but inactivates warning from gcc when compiling
       * could put the error_at thing here but fallthrough (from failing $crate
       * condition) isn't completely obvious if it is. */

      // test prevent error
      return Parse::Error::PathIdentSegment::make_invalid_token ();
    }
  rust_unreachable ();
}

// Parses a type path.
template <typename ManagedTokenSource>
AST::TypePath
Parser<ManagedTokenSource>::parse_type_path ()
{
  bool has_opening_scope_resolution = false;
  location_t locus = lexer.peek_token ()->get_locus ();
  if (lexer.peek_token ()->get_id () == SCOPE_RESOLUTION)
    {
      has_opening_scope_resolution = true;
      lexer.skip_token ();
    }

  // create segment vector
  std::vector<std::unique_ptr<AST::TypePathSegment>> segments;

  // parse required initial segment
  std::unique_ptr<AST::TypePathSegment> initial_segment
    = parse_type_path_segment ();
  if (initial_segment == nullptr)
    {
      // skip after somewhere?
      // don't necessarily throw error but yeah
      return AST::TypePath::create_error ();
    }
  segments.push_back (std::move (initial_segment));

  // parse optional segments (as long as scope resolution operator exists)
  const_TokenPtr t = lexer.peek_token ();
  while (t->get_id () == SCOPE_RESOLUTION)
    {
      // skip scope resolution operator
      lexer.skip_token ();

      // parse the actual segment - it is an error if it doesn't exist now
      std::unique_ptr<AST::TypePathSegment> segment
	= parse_type_path_segment ();
      if (segment == nullptr)
	{
	  // skip after somewhere?
	  Error error (t->get_locus (), "could not parse type path segment");
	  add_error (std::move (error));

	  return AST::TypePath::create_error ();
	}

      segments.push_back (std::move (segment));

      t = lexer.peek_token ();
    }

  segments.shrink_to_fit ();

  return AST::TypePath (std::move (segments), locus,
			has_opening_scope_resolution);
}

/* Parses a single type path segment (not including opening scope resolution,
 * but includes any internal ones). Includes generic args or type path
 * functions too. */
template <typename ManagedTokenSource>
std::unique_ptr<AST::TypePathSegment>
Parser<ManagedTokenSource>::parse_type_path_segment ()
{
  location_t locus = lexer.peek_token ()->get_locus ();
  // parse ident segment part
  auto ident_segment_res = parse_path_ident_segment ();
  if (!ident_segment_res)
    {
      // not necessarily an error
      return nullptr;
    }
  auto ident_segment = ident_segment_res.value ();

  /* lookahead to determine if variants exist - only consume scope resolution
   * then */
  bool has_separating_scope_resolution = false;
  const_TokenPtr next = lexer.peek_token (1);
  if (lexer.peek_token ()->get_id () == SCOPE_RESOLUTION
      && (next->get_id () == LEFT_ANGLE || next->get_id () == LEFT_PAREN))
    {
      has_separating_scope_resolution = true;
      lexer.skip_token ();
    }

  // branch into variants on next token
  const_TokenPtr t = lexer.peek_token ();
  switch (t->get_id ())
    {
    case LEFT_SHIFT:
    case LEFT_ANGLE:
      {
	// parse generic args
	AST::GenericArgs generic_args = parse_path_generic_args ();

	return std::unique_ptr<AST::TypePathSegmentGeneric> (
	  new AST::TypePathSegmentGeneric (std::move (ident_segment),
					   has_separating_scope_resolution,
					   std::move (generic_args), locus));
      }
    case LEFT_PAREN:
      {
	// parse type path function
	AST::TypePathFunction type_path_function
	  = parse_type_path_function (locus);

	if (type_path_function.is_error ())
	  {
	    // skip after somewhere?
	    return nullptr;
	  }

	return std::unique_ptr<AST::TypePathSegmentFunction> (
	  new AST::TypePathSegmentFunction (std::move (ident_segment),
					    has_separating_scope_resolution,
					    std::move (type_path_function),
					    locus));
      }
    default:
      // neither of them
      return std::unique_ptr<AST::TypePathSegment> (
	new AST::TypePathSegment (std::move (ident_segment),
				  has_separating_scope_resolution, locus));
    }
  rust_unreachable ();
}

// Parses a function call representation inside a type path.
template <typename ManagedTokenSource>
AST::TypePathFunction
Parser<ManagedTokenSource>::parse_type_path_function (location_t id_location)
{
  if (!skip_token (LEFT_PAREN))
    {
      // skip somewhere?
      return AST::TypePathFunction::create_error ();
    }

  // parse function inputs
  std::vector<std::unique_ptr<AST::Type>> inputs;

  while (lexer.peek_token ()->get_id () != RIGHT_PAREN)
    {
      std::unique_ptr<AST::Type> type = parse_type ();
      if (type == nullptr)
	{
	  /* this is an error as there should've been a ')' there if there
	   * wasn't a type */
	  Error error (
	    lexer.peek_token ()->get_locus (),
	    "failed to parse type in parameters of type path function");
	  add_error (std::move (error));

	  // skip somewhere?
	  return AST::TypePathFunction::create_error ();
	}

      inputs.push_back (std::move (type));

      // skip commas, including trailing commas
      if (lexer.peek_token ()->get_id () != COMMA)
	break;

      lexer.skip_token ();
    }

  if (!skip_token (RIGHT_PAREN))
    {
      // skip somewhere?
      return AST::TypePathFunction::create_error ();
    }

  // parse optional return type
  std::unique_ptr<AST::Type> return_type = parse_function_return_type ();

  inputs.shrink_to_fit ();
  return AST::TypePathFunction (std::move (inputs), id_location,
				std::move (return_type));
}

// Parses a path inside an expression that allows generic arguments.
template <typename ManagedTokenSource>
AST::PathInExpression
Parser<ManagedTokenSource>::parse_path_in_expression ()
{
  location_t locus = UNKNOWN_LOCATION;
  bool has_opening_scope_resolution = false;
  if (lexer.peek_token ()->get_id () == SCOPE_RESOLUTION)
    {
      has_opening_scope_resolution = true;

      locus = lexer.peek_token ()->get_locus ();

      lexer.skip_token ();
    }

  // create segment vector
  std::vector<AST::PathExprSegment> segments;

  if (locus == UNKNOWN_LOCATION)
    {
      locus = lexer.peek_token ()->get_locus ();
    }

  // parse required initial segment
  AST::PathExprSegment initial_segment = parse_path_expr_segment ();
  if (initial_segment.is_error ())
    {
      // skip after somewhere?
      // don't necessarily throw error but yeah
      return AST::PathInExpression::create_error ();
    }
  segments.push_back (std::move (initial_segment));

  // parse optional segments (as long as scope resolution operator exists)
  const_TokenPtr t = lexer.peek_token ();
  while (t->get_id () == SCOPE_RESOLUTION)
    {
      // skip scope resolution operator
      lexer.skip_token ();

      // parse the actual segment - it is an error if it doesn't exist now
      AST::PathExprSegment segment = parse_path_expr_segment ();
      if (segment.is_error ())
	{
	  // skip after somewhere?
	  Error error (t->get_locus (),
		       "could not parse path expression segment");
	  add_error (std::move (error));

	  return AST::PathInExpression::create_error ();
	}

      segments.push_back (std::move (segment));

      t = lexer.peek_token ();
    }

  segments.shrink_to_fit ();

  return AST::PathInExpression (std::move (segments), {}, locus,
				has_opening_scope_resolution);
}

/* Parses a single path in expression path segment (including generic
 * arguments). */
template <typename ManagedTokenSource>
AST::PathExprSegment
Parser<ManagedTokenSource>::parse_path_expr_segment ()
{
  location_t locus = lexer.peek_token ()->get_locus ();
  // parse ident segment
  auto ident_result = parse_path_ident_segment ();
  if (!ident_result)
    {
      // not necessarily an error?
      return AST::PathExprSegment::create_error ();
    }
  auto ident = ident_result.value ();

  // parse generic args (and turbofish), if they exist
  /* use lookahead to determine if they actually exist (don't want to
   * accidently parse over next ident segment) */
  if (lexer.peek_token ()->get_id () == SCOPE_RESOLUTION
      && (lexer.peek_token (1)->get_id () == LEFT_ANGLE
	  || lexer.peek_token (1)->get_id () == LEFT_SHIFT))
    {
      // skip scope resolution
      lexer.skip_token ();

      // Let parse_path_generic_args split "<<" tokens
      AST::GenericArgs generic_args = parse_path_generic_args ();

      return AST::PathExprSegment (std::move (ident), locus,
				   std::move (generic_args));
    }

  // return a generic parameter-less expr segment if not found
  return AST::PathExprSegment (std::move (ident), locus);
}

/* Parses a fully qualified path in expression (i.e. a pattern). FIXME does
 * not parse outer attrs. */
template <typename ManagedTokenSource>
AST::QualifiedPathInExpression
Parser<ManagedTokenSource>::parse_qualified_path_in_expression (
  location_t pratt_parsed_loc)
{
  /* Note: the Rust grammar is defined in such a way that it is impossible to
   * determine whether a prospective qualified path is a
   * QualifiedPathInExpression or QualifiedPathInType in all cases by the
   * rules themselves (the only possible difference is a TypePathSegment with
   * function, and lookahead to find this is too difficult). However, as this
   * is a pattern and QualifiedPathInType is a type, I believe it that their
   * construction will not be confused (due to rules regarding patterns vs
   * types).
   * As such, this function will not attempt to minimise errors created by
   * their confusion. */

  // parse the qualified path type (required)
  AST::QualifiedPathType qual_path_type
    = parse_qualified_path_type (pratt_parsed_loc);
  if (qual_path_type.is_error ())
    {
      // TODO: should this create a parse error?
      return AST::QualifiedPathInExpression::create_error ();
    }
  location_t locus = qual_path_type.get_locus ();

  // parse path segments
  std::vector<AST::PathExprSegment> segments;

  // parse initial required segment
  if (!expect_token (SCOPE_RESOLUTION))
    {
      // skip after somewhere?

      return AST::QualifiedPathInExpression::create_error ();
    }
  AST::PathExprSegment initial_segment = parse_path_expr_segment ();
  if (initial_segment.is_error ())
    {
      // skip after somewhere?
      Error error (lexer.peek_token ()->get_locus (),
		   "required initial path expression segment in "
		   "qualified path in expression could not be parsed");
      add_error (std::move (error));

      return AST::QualifiedPathInExpression::create_error ();
    }
  segments.push_back (std::move (initial_segment));

  // parse optional segments (as long as scope resolution operator exists)
  const_TokenPtr t = lexer.peek_token ();
  while (t->get_id () == SCOPE_RESOLUTION)
    {
      // skip scope resolution operator
      lexer.skip_token ();

      // parse the actual segment - it is an error if it doesn't exist now
      AST::PathExprSegment segment = parse_path_expr_segment ();
      if (segment.is_error ())
	{
	  // skip after somewhere?
	  Error error (t->get_locus (),
		       "could not parse path expression segment in qualified "
		       "path in expression");
	  add_error (std::move (error));

	  return AST::QualifiedPathInExpression::create_error ();
	}

      segments.push_back (std::move (segment));

      t = lexer.peek_token ();
    }

  segments.shrink_to_fit ();

  // FIXME: outer attr parsing
  return AST::QualifiedPathInExpression (std::move (qual_path_type),
					 std::move (segments), {}, locus);
}

// Parses the type syntactical construction at the start of a qualified path.
template <typename ManagedTokenSource>
AST::QualifiedPathType
Parser<ManagedTokenSource>::parse_qualified_path_type (
  location_t pratt_parsed_loc)
{
  location_t locus = pratt_parsed_loc;
  /* TODO: should this actually be error? is there anywhere where this could
   * be valid? */
  if (locus == UNKNOWN_LOCATION)
    {
      locus = lexer.peek_token ()->get_locus ();

      if (lexer.peek_token ()->get_id () == LEFT_SHIFT)
	lexer.split_current_token (LEFT_ANGLE, LEFT_ANGLE);

      // skip after somewhere?
      if (!skip_token (LEFT_ANGLE))
	return AST::QualifiedPathType::create_error ();
    }

  // parse type (required)
  std::unique_ptr<AST::Type> type = parse_type ();
  if (type == nullptr)
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "could not parse type in qualified path type");
      add_error (std::move (error));

      // skip somewhere?
      return AST::QualifiedPathType::create_error ();
    }

  // parse optional as clause
  AST::TypePath as_type_path = AST::TypePath::create_error ();
  if (lexer.peek_token ()->get_id () == AS)
    {
      lexer.skip_token ();

      // parse type path, which is required now
      as_type_path = parse_type_path ();
      if (as_type_path.is_error ())
	{
	  Error error (
	    lexer.peek_token ()->get_locus (),
	    "could not parse type path in as clause in qualified path type");
	  add_error (std::move (error));

	  // skip somewhere?
	  return AST::QualifiedPathType::create_error ();
	}
    }

  /* NOTE: should actually be a right-angle token, so
   * skip_generics_right_angle shouldn't be required */
  if (!skip_token (RIGHT_ANGLE))
    {
      // skip after somewhere?
      return AST::QualifiedPathType::create_error ();
    }

  return AST::QualifiedPathType (std::move (type), locus,
				 std::move (as_type_path));
}

// Parses a fully qualified path in type (i.e. a type).
template <typename ManagedTokenSource>
AST::QualifiedPathInType
Parser<ManagedTokenSource>::parse_qualified_path_in_type ()
{
  location_t locus = lexer.peek_token ()->get_locus ();
  // parse the qualified path type (required)
  AST::QualifiedPathType qual_path_type = parse_qualified_path_type ();
  if (qual_path_type.is_error ())
    {
      // TODO: should this create a parse error?
      return AST::QualifiedPathInType::create_error ();
    }

  // parse initial required segment
  if (!expect_token (SCOPE_RESOLUTION))
    {
      // skip after somewhere?

      return AST::QualifiedPathInType::create_error ();
    }
  std::unique_ptr<AST::TypePathSegment> initial_segment
    = parse_type_path_segment ();
  if (initial_segment == nullptr)
    {
      // skip after somewhere?
      Error error (lexer.peek_token ()->get_locus (),
		   "required initial type path segment in qualified path in "
		   "type could not be parsed");
      add_error (std::move (error));

      return AST::QualifiedPathInType::create_error ();
    }

  // parse optional segments (as long as scope resolution operator exists)
  std::vector<std::unique_ptr<AST::TypePathSegment>> segments;
  const_TokenPtr t = lexer.peek_token ();
  while (t->get_id () == SCOPE_RESOLUTION)
    {
      // skip scope resolution operator
      lexer.skip_token ();

      // parse the actual segment - it is an error if it doesn't exist now
      std::unique_ptr<AST::TypePathSegment> segment
	= parse_type_path_segment ();
      if (segment == nullptr)
	{
	  // skip after somewhere?
	  Error error (
	    t->get_locus (),
	    "could not parse type path segment in qualified path in type");
	  add_error (std::move (error));

	  return AST::QualifiedPathInType::create_error ();
	}

      segments.push_back (std::move (segment));

      t = lexer.peek_token ();
    }

  segments.shrink_to_fit ();

  return AST::QualifiedPathInType (std::move (qual_path_type),
				   std::move (initial_segment),
				   std::move (segments), locus);
}
} // namespace Rust
