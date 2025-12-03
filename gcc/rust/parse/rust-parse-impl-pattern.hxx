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

template <typename ManagedTokenSource>
std::unique_ptr<AST::Pattern>
Parser<ManagedTokenSource>::parse_pattern ()
{
  location_t start_locus = lexer.peek_token ()->get_locus ();

  /* skip optional starting pipe */
  maybe_skip_token (PIPE);

  auto first = parse_pattern_no_alt ();

  if (lexer.peek_token ()->get_id () != PIPE)
    /* no alternates */
    return first;

  std::vector<std::unique_ptr<AST::Pattern>> alts;
  if (first != nullptr)
    alts.push_back (std::move (first));

  do
    {
      lexer.skip_token ();
      auto follow = parse_pattern_no_alt ();
      if (follow != nullptr)
	alts.push_back (std::move (follow));
    }

  while (lexer.peek_token ()->get_id () == PIPE);

  if (alts.empty ())
    return nullptr;

  /* alternates */
  return std::unique_ptr<AST::Pattern> (
    new AST::AltPattern (std::move (alts), start_locus));
}

// Parses a pattern without alternates ('|')
// (will further disambiguate any pattern).
template <typename ManagedTokenSource>
std::unique_ptr<AST::Pattern>
Parser<ManagedTokenSource>::parse_pattern_no_alt ()
{
  const_TokenPtr t = lexer.peek_token ();
  switch (t->get_id ())
    {
    case TRUE_LITERAL:
      lexer.skip_token ();
      return std::unique_ptr<AST::LiteralPattern> (
	new AST::LiteralPattern (Values::Keywords::TRUE_LITERAL,
				 AST::Literal::BOOL, t->get_locus (),
				 t->get_type_hint ()));
    case FALSE_LITERAL:
      lexer.skip_token ();
      return std::unique_ptr<AST::LiteralPattern> (
	new AST::LiteralPattern (Values::Keywords::FALSE_LITERAL,
				 AST::Literal::BOOL, t->get_locus (),
				 t->get_type_hint ()));
    case CHAR_LITERAL:
    case BYTE_CHAR_LITERAL:
    case INT_LITERAL:
    case FLOAT_LITERAL:
      return parse_literal_or_range_pattern ();
    case STRING_LITERAL:
      lexer.skip_token ();
      return std::unique_ptr<AST::LiteralPattern> (
	new AST::LiteralPattern (t->get_str (), AST::Literal::STRING,
				 t->get_locus (), t->get_type_hint ()));
    case BYTE_STRING_LITERAL:
      lexer.skip_token ();
      return std::unique_ptr<AST::LiteralPattern> (
	new AST::LiteralPattern (t->get_str (), AST::Literal::BYTE_STRING,
				 t->get_locus (), t->get_type_hint ()));
    case RAW_STRING_LITERAL:
      lexer.skip_token ();
      return std::unique_ptr<AST::LiteralPattern> (
	new AST::LiteralPattern (t->get_str (), AST::Literal::RAW_STRING,
				 t->get_locus (), t->get_type_hint ()));
    // raw string and raw byte string literals too if they are readded to
    // lexer
    case MINUS:
      if (lexer.peek_token (1)->get_id () == INT_LITERAL)
	{
	  return parse_literal_or_range_pattern ();
	}
      else if (lexer.peek_token (1)->get_id () == FLOAT_LITERAL)
	{
	  return parse_literal_or_range_pattern ();
	}
      else
	{
	  Error error (t->get_locus (), "unexpected token %<-%> in pattern - "
					"did you forget an integer literal");
	  add_error (std::move (error));

	  return nullptr;
	}
    case UNDERSCORE:
      lexer.skip_token ();
      return std::unique_ptr<AST::WildcardPattern> (
	new AST::WildcardPattern (t->get_locus ()));
    case DOT_DOT:
      lexer.skip_token ();
      return std::unique_ptr<AST::RestPattern> (
	new AST::RestPattern (t->get_locus ()));
    case REF:
    case MUT:
      return parse_identifier_pattern ();
    case IDENTIFIER:
      /* if identifier with no scope resolution afterwards, identifier
       * pattern. if scope resolution afterwards, path pattern (or range
       * pattern or struct pattern or tuple struct pattern) or macro
       * invocation */
      return parse_ident_leading_pattern ();
    case AMP:
    case LOGICAL_AND:
      // reference pattern
      return parse_reference_pattern ();
    case LEFT_PAREN:
      // tuple pattern or grouped pattern
      return parse_grouped_or_tuple_pattern ();
    case LEFT_SQUARE:
      // slice pattern
      return parse_slice_pattern ();
    case LEFT_SHIFT:
    case LEFT_ANGLE:
      {
	// qualified path in expression or qualified range pattern bound
	AST::QualifiedPathInExpression path
	  = parse_qualified_path_in_expression ();

	if (lexer.peek_token ()->get_id () == DOT_DOT_EQ
	    || lexer.peek_token ()->get_id () == ELLIPSIS
	    || lexer.peek_token ()->get_id () == DOT_DOT)
	  {
	    // qualified range pattern bound, so parse rest of range pattern
	    AST::RangeKind kind
	      = AST::tokenid_to_rangekind (lexer.peek_token ()->get_id ());
	    lexer.skip_token ();

	    std::unique_ptr<AST::RangePatternBoundQualPath> lower_bound (
	      new AST::RangePatternBoundQualPath (std::move (path)));
	    std::unique_ptr<AST::RangePatternBound> upper_bound
	      = parse_range_pattern_bound ();

	    return std::unique_ptr<AST::RangePattern> (
	      new AST::RangePattern (std::move (lower_bound),
				     std::move (upper_bound), kind,
				     t->get_locus ()));
	  }
	else
	  {
	    // just qualified path in expression
	    return std::unique_ptr<AST::QualifiedPathInExpression> (
	      new AST::QualifiedPathInExpression (std::move (path)));
	  }
      }
    case SUPER:
    case SELF:
    case SELF_ALIAS:
    case CRATE:
    case SCOPE_RESOLUTION:
    case DOLLAR_SIGN:
      {
	// path in expression or range pattern bound
	AST::PathInExpression path = parse_path_in_expression ();

	const_TokenPtr next = lexer.peek_token ();
	switch (next->get_id ())
	  {
	  case DOT_DOT_EQ:
	  case DOT_DOT:
	  case ELLIPSIS:
	    {
	      // qualified range pattern bound, so parse rest of range pattern
	      AST::RangeKind kind = AST::tokenid_to_rangekind (next->get_id ());
	      lexer.skip_token ();

	      std::unique_ptr<AST::RangePatternBoundPath> lower_bound (
		new AST::RangePatternBoundPath (std::move (path)));
	      std::unique_ptr<AST::RangePatternBound> upper_bound
		= parse_range_pattern_bound ();

	      return std::unique_ptr<AST::RangePattern> (
		new AST::RangePattern (std::move (lower_bound),
				       std::move (upper_bound), kind,
				       next->get_locus ()));
	    }
	  case EXCLAM:
	    return parse_macro_invocation_partial (std::move (path),
						   AST::AttrVec ());
	  case LEFT_PAREN:
	    {
	      // tuple struct
	      lexer.skip_token ();

	      // parse items
	      std::unique_ptr<AST::TupleStructItems> items
		= parse_tuple_struct_items ();
	      if (items == nullptr)
		{
		  Error error (lexer.peek_token ()->get_locus (),
			       "failed to parse tuple struct items");
		  add_error (std::move (error));

		  return nullptr;
		}

	      if (!skip_token (RIGHT_PAREN))
		{
		  return nullptr;
		}

	      return std::unique_ptr<AST::TupleStructPattern> (
		new AST::TupleStructPattern (std::move (path),
					     std::move (items)));
	    }
	  case LEFT_CURLY:
	    {
	      // struct
	      lexer.skip_token ();

	      // parse elements (optional)
	      AST::StructPatternElements elems = parse_struct_pattern_elems ();

	      if (!skip_token (RIGHT_CURLY))
		{
		  return nullptr;
		}

	      return std::unique_ptr<AST::StructPattern> (
		new AST::StructPattern (std::move (path), t->get_locus (),
					std::move (elems)));
	    }
	  default:
	    // assume path in expression
	    return std::unique_ptr<AST::PathInExpression> (
	      new AST::PathInExpression (std::move (path)));
	  }
      }
    default:
      add_error (Error (t->get_locus (), "unexpected token %qs in pattern",
			t->get_token_description ()));

      return nullptr;
    }
}

// Parses a single or double reference pattern.
template <typename ManagedTokenSource>
std::unique_ptr<AST::ReferencePattern>
Parser<ManagedTokenSource>::parse_reference_pattern ()
{
  // parse double or single ref
  bool is_double_ref = false;
  const_TokenPtr t = lexer.peek_token ();
  switch (t->get_id ())
    {
    case AMP:
      // still false
      lexer.skip_token ();
      break;
    case LOGICAL_AND:
      is_double_ref = true;
      lexer.skip_token ();
      break;
    default:
      add_error (Error (t->get_locus (),
			"unexpected token %qs in reference pattern",
			t->get_token_description ()));

      return nullptr;
    }

  // parse mut (if it exists)
  bool is_mut = false;
  if (lexer.peek_token ()->get_id () == MUT)
    {
      is_mut = true;
      lexer.skip_token ();
    }

  // parse pattern to get reference of (required)
  std::unique_ptr<AST::Pattern> pattern = parse_pattern_no_alt ();
  if (pattern == nullptr)
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "failed to parse pattern in reference pattern");
      add_error (std::move (error));

      // skip somewhere?
      return nullptr;
    }

  return std::unique_ptr<AST::ReferencePattern> (
    new AST::ReferencePattern (std::move (pattern), is_mut, is_double_ref,
			       t->get_locus ()));
}

/* Parses a grouped pattern or tuple pattern. Prefers grouped over tuple if
 * only a single element with no commas. */
template <typename ManagedTokenSource>
std::unique_ptr<AST::Pattern>
Parser<ManagedTokenSource>::parse_grouped_or_tuple_pattern ()
{
  location_t paren_locus = lexer.peek_token ()->get_locus ();
  skip_token (LEFT_PAREN);

  // detect '..' token (ranged with no lower range)
  if (lexer.peek_token ()->get_id () == DOT_DOT)
    {
      lexer.skip_token ();

      // parse new patterns while next token is a comma
      std::vector<std::unique_ptr<AST::Pattern>> patterns;

      const_TokenPtr t = lexer.peek_token ();
      while (t->get_id () == COMMA)
	{
	  lexer.skip_token ();

	  // break if next token is ')'
	  if (lexer.peek_token ()->get_id () == RIGHT_PAREN)
	    {
	      break;
	    }

	  // parse pattern, which is required
	  std::unique_ptr<AST::Pattern> pattern = parse_pattern ();
	  if (pattern == nullptr)
	    {
	      Error error (
		lexer.peek_token ()->get_locus (),
		"failed to parse pattern inside ranged tuple pattern");
	      add_error (std::move (error));

	      // skip somewhere?
	      return nullptr;
	    }
	  patterns.push_back (std::move (pattern));

	  t = lexer.peek_token ();
	}

      if (!skip_token (RIGHT_PAREN))
	{
	  // skip somewhere?
	  return nullptr;
	}

      // create tuple pattern items with only upper pattern items
      std::unique_ptr<AST::TuplePatternItemsHasRest> items (
	new AST::TuplePatternItemsHasRest (
	  std::vector<std::unique_ptr<AST::Pattern>> (), std::move (patterns)));
      return std::unique_ptr<AST::TuplePattern> (
	new AST::TuplePattern (std::move (items), paren_locus));
    }
  else if (lexer.peek_token ()->get_id () == RIGHT_PAREN)
    {
      skip_token (RIGHT_PAREN);
      auto items = std::unique_ptr<AST::TuplePatternItemsNoRest> (
	new AST::TuplePatternItemsNoRest (
	  std::vector<std::unique_ptr<AST::Pattern>> ()));
      return std::unique_ptr<AST::TuplePattern> (
	new AST::TuplePattern (std::move (items), paren_locus));
    }

  // parse initial pattern (required)
  std::unique_ptr<AST::Pattern> initial_pattern = parse_pattern ();
  if (initial_pattern == nullptr)
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "failed to parse pattern in grouped or tuple pattern");
      add_error (std::move (error));

      return nullptr;
    }

  // branch on whether next token is a comma or not
  const_TokenPtr t = lexer.peek_token ();
  switch (t->get_id ())
    {
    case RIGHT_PAREN:
      // grouped pattern
      lexer.skip_token ();

      return std::unique_ptr<AST::GroupedPattern> (
	new AST::GroupedPattern (std::move (initial_pattern), paren_locus));
    case COMMA:
      {
	// tuple pattern
	lexer.skip_token ();

	// create vector of patterns
	std::vector<std::unique_ptr<AST::Pattern>> patterns;
	patterns.push_back (std::move (initial_pattern));

	t = lexer.peek_token ();
	while (t->get_id () != RIGHT_PAREN && t->get_id () != DOT_DOT)
	  {
	    // parse pattern (required)
	    std::unique_ptr<AST::Pattern> pattern = parse_pattern ();
	    if (pattern == nullptr)
	      {
		Error error (t->get_locus (),
			     "failed to parse pattern in tuple pattern");
		add_error (std::move (error));

		return nullptr;
	      }
	    patterns.push_back (std::move (pattern));

	    if (lexer.peek_token ()->get_id () != COMMA)
	      break;

	    lexer.skip_token ();
	    t = lexer.peek_token ();
	  }

	t = lexer.peek_token ();
	if (t->get_id () == RIGHT_PAREN)
	  {
	    // non-ranged tuple pattern
	    lexer.skip_token ();

	    std::unique_ptr<AST::TuplePatternItemsNoRest> items (
	      new AST::TuplePatternItemsNoRest (std::move (patterns)));
	    return std::unique_ptr<AST::TuplePattern> (
	      new AST::TuplePattern (std::move (items), paren_locus));
	  }
	else if (t->get_id () == DOT_DOT)
	  {
	    // ranged tuple pattern
	    lexer.skip_token ();

	    // parse upper patterns
	    std::vector<std::unique_ptr<AST::Pattern>> upper_patterns;
	    t = lexer.peek_token ();
	    while (t->get_id () == COMMA)
	      {
		lexer.skip_token ();

		// break if end
		if (lexer.peek_token ()->get_id () == RIGHT_PAREN)
		  break;

		// parse pattern (required)
		std::unique_ptr<AST::Pattern> pattern = parse_pattern ();
		if (pattern == nullptr)
		  {
		    Error error (lexer.peek_token ()->get_locus (),
				 "failed to parse pattern in tuple pattern");
		    add_error (std::move (error));

		    return nullptr;
		  }
		upper_patterns.push_back (std::move (pattern));

		t = lexer.peek_token ();
	      }

	    if (!skip_token (RIGHT_PAREN))
	      {
		return nullptr;
	      }

	    std::unique_ptr<AST::TuplePatternItemsHasRest> items (
	      new AST::TuplePatternItemsHasRest (std::move (patterns),
						 std::move (upper_patterns)));
	    return std::unique_ptr<AST::TuplePattern> (
	      new AST::TuplePattern (std::move (items), paren_locus));
	  }
	else
	  {
	    // some kind of error
	    Error error (t->get_locus (),
			 "failed to parse tuple pattern (probably) or maybe "
			 "grouped pattern");
	    add_error (std::move (error));

	    return nullptr;
	  }
      }
    default:
      // error
      add_error (Error (t->get_locus (),
			"unrecognised token %qs in grouped or tuple pattern "
			"after first pattern",
			t->get_token_description ()));

      return nullptr;
    }
}

/* Parses a slice pattern that can match arrays or slices. Parses the square
 * brackets too. */
template <typename ManagedTokenSource>
std::unique_ptr<AST::SlicePattern>
Parser<ManagedTokenSource>::parse_slice_pattern ()
{
  location_t square_locus = lexer.peek_token ()->get_locus ();
  std::vector<std::unique_ptr<AST::Pattern>> patterns;
  tl::optional<std::vector<std::unique_ptr<AST::Pattern>>> upper_patterns
    = tl::nullopt;

  // lambda function to determine which vector to push new patterns into
  auto get_pattern_ref
    = [&] () -> std::vector<std::unique_ptr<AST::Pattern>> & {
    return upper_patterns.has_value () ? upper_patterns.value () : patterns;
  };

  skip_token (LEFT_SQUARE);

  if (lexer.peek_token ()->get_id () == RIGHT_SQUARE)
    {
      skip_token (RIGHT_SQUARE);
      std::unique_ptr<AST::SlicePatternItemsNoRest> items (
	new AST::SlicePatternItemsNoRest (std::move (patterns)));
      return std::unique_ptr<AST::SlicePattern> (
	new AST::SlicePattern (std::move (items), square_locus));
    }

  // parse initial pattern (required)
  if (lexer.peek_token ()->get_id () == DOT_DOT)
    {
      lexer.skip_token ();
      upper_patterns = std::vector<std::unique_ptr<AST::Pattern>> ();
    }
  else
    {
      // Not a rest pattern `..`, parse normally
      std::unique_ptr<AST::Pattern> initial_pattern = parse_pattern ();
      if (initial_pattern == nullptr)
	{
	  Error error (lexer.peek_token ()->get_locus (),
		       "failed to parse initial pattern in slice pattern");
	  add_error (std::move (error));

	  return nullptr;
	}

      patterns.push_back (std::move (initial_pattern));
    }

  const_TokenPtr t = lexer.peek_token ();
  while (t->get_id () == COMMA)
    {
      lexer.skip_token ();

      // break if end bracket
      if (lexer.peek_token ()->get_id () == RIGHT_SQUARE)
	break;

      if (lexer.peek_token ()->get_id () == DOT_DOT)
	{
	  if (upper_patterns.has_value ())
	    {
	      // DOT_DOT has been parsed before
	      Error error (lexer.peek_token ()->get_locus (), "%s",
			   "`..` can only be used once per slice pattern");
	      add_error (std::move (error));

	      return nullptr;
	    }
	  upper_patterns = std::vector<std::unique_ptr<AST::Pattern>> ();
	  lexer.skip_token ();
	  t = lexer.peek_token ();
	  continue;
	}

      // parse pattern (required)
      std::unique_ptr<AST::Pattern> pattern = parse_pattern ();
      if (pattern == nullptr)
	{
	  Error error (lexer.peek_token ()->get_locus (),
		       "failed to parse pattern in slice pattern");
	  add_error (std::move (error));

	  return nullptr;
	}
      get_pattern_ref ().push_back (std::move (pattern));

      t = lexer.peek_token ();
    }

  if (!skip_token (RIGHT_SQUARE))
    {
      return nullptr;
    }

  if (upper_patterns.has_value ())
    {
      // Slice pattern with rest
      std::unique_ptr<AST::SlicePatternItemsHasRest> items (
	new AST::SlicePatternItemsHasRest (
	  std::move (patterns), std::move (upper_patterns.value ())));
      return std::unique_ptr<AST::SlicePattern> (
	new AST::SlicePattern (std::move (items), square_locus));
    }

  // Rest-less slice pattern
  std::unique_ptr<AST::SlicePatternItemsNoRest> items (
    new AST::SlicePatternItemsNoRest (std::move (patterns)));
  return std::unique_ptr<AST::SlicePattern> (
    new AST::SlicePattern (std::move (items), square_locus));
}

/* Parses an identifier pattern (pattern that binds a value matched to a
 * variable). */
template <typename ManagedTokenSource>
std::unique_ptr<AST::IdentifierPattern>
Parser<ManagedTokenSource>::parse_identifier_pattern ()
{
  location_t locus = lexer.peek_token ()->get_locus ();

  bool has_ref = false;
  if (lexer.peek_token ()->get_id () == REF)
    {
      has_ref = true;
      lexer.skip_token ();

      // DEBUG
      rust_debug ("parsed ref in identifier pattern");
    }

  bool has_mut = false;
  if (lexer.peek_token ()->get_id () == MUT)
    {
      has_mut = true;
      lexer.skip_token ();
    }

  // parse identifier (required)
  const_TokenPtr ident_tok = expect_token (IDENTIFIER);
  if (ident_tok == nullptr)
    {
      // skip somewhere?
      return nullptr;
    }
  Identifier ident{ident_tok};

  // DEBUG
  rust_debug ("parsed identifier in identifier pattern");

  // parse optional pattern binding thing
  std::unique_ptr<AST::Pattern> bind_pattern = nullptr;
  if (lexer.peek_token ()->get_id () == PATTERN_BIND)
    {
      lexer.skip_token ();

      // parse required pattern to bind
      bind_pattern = parse_pattern_no_alt ();
      if (bind_pattern == nullptr)
	{
	  Error error (lexer.peek_token ()->get_locus (),
		       "failed to parse pattern to bind in identifier pattern");
	  add_error (std::move (error));

	  return nullptr;
	}
    }

  // DEBUG
  rust_debug ("about to return identifier pattern");

  return std::unique_ptr<AST::IdentifierPattern> (
    new AST::IdentifierPattern (std::move (ident), locus, has_ref, has_mut,
				std::move (bind_pattern)));
}

/* Parses a pattern that opens with an identifier. This includes identifier
 * patterns, path patterns (and derivatives such as struct patterns, tuple
 * struct patterns, and macro invocations), and ranges. */
template <typename ManagedTokenSource>
std::unique_ptr<AST::Pattern>
Parser<ManagedTokenSource>::parse_ident_leading_pattern ()
{
  // ensure first token is actually identifier
  const_TokenPtr initial_tok = lexer.peek_token ();
  if (initial_tok->get_id () != IDENTIFIER)
    {
      return nullptr;
    }

  // save initial identifier as it may be useful (but don't skip)
  std::string initial_ident = initial_tok->get_str ();

  // parse next tokens as a PathInExpression
  AST::PathInExpression path = parse_path_in_expression ();

  // branch on next token
  const_TokenPtr t = lexer.peek_token ();
  switch (t->get_id ())
    {
    case EXCLAM:
      return parse_macro_invocation_partial (std::move (path), AST::AttrVec ());
    case LEFT_PAREN:
      {
	// tuple struct
	lexer.skip_token ();

	// DEBUG
	rust_debug ("parsing tuple struct pattern");

	// parse items
	std::unique_ptr<AST::TupleStructItems> items
	  = parse_tuple_struct_items ();
	if (items == nullptr)
	  {
	    Error error (lexer.peek_token ()->get_locus (),
			 "failed to parse tuple struct items");
	    add_error (std::move (error));

	    return nullptr;
	  }

	// DEBUG
	rust_debug ("successfully parsed tuple struct items");

	if (!skip_token (RIGHT_PAREN))
	  {
	    return nullptr;
	  }

	// DEBUG
	rust_debug ("successfully parsed tuple struct pattern");

	return std::unique_ptr<AST::TupleStructPattern> (
	  new AST::TupleStructPattern (std::move (path), std::move (items)));
      }
    case LEFT_CURLY:
      {
	// struct
	lexer.skip_token ();

	// parse elements (optional)
	AST::StructPatternElements elems = parse_struct_pattern_elems ();

	if (!skip_token (RIGHT_CURLY))
	  {
	    return nullptr;
	  }

	// DEBUG
	rust_debug ("successfully parsed struct pattern");

	return std::unique_ptr<AST::StructPattern> (
	  new AST::StructPattern (std::move (path), initial_tok->get_locus (),
				  std::move (elems)));
      }
    case DOT_DOT_EQ:
    case DOT_DOT:
    case ELLIPSIS:
      {
	// range
	AST::RangeKind kind
	  = AST::tokenid_to_rangekind (lexer.peek_token ()->get_id ());

	lexer.skip_token ();

	std::unique_ptr<AST::RangePatternBoundPath> lower_bound (
	  new AST::RangePatternBoundPath (std::move (path)));
	std::unique_ptr<AST::RangePatternBound> upper_bound
	  = parse_range_pattern_bound ();

	return std::unique_ptr<AST::RangePattern> (
	  new AST::RangePattern (std::move (lower_bound),
				 std::move (upper_bound), kind,
				 t->get_locus ()));
      }
    case PATTERN_BIND:
      {
	// only allow on single-segment paths
	if (path.is_single_segment ())
	  {
	    // identifier with pattern bind
	    lexer.skip_token ();

	    std::unique_ptr<AST::Pattern> bind_pattern
	      = parse_pattern_no_alt ();
	    if (bind_pattern == nullptr)
	      {
		Error error (
		  t->get_locus (),
		  "failed to parse pattern to bind to identifier pattern");
		add_error (std::move (error));

		return nullptr;
	      }
	    return std::unique_ptr<AST::IdentifierPattern> (
	      new AST::IdentifierPattern (std::move (initial_ident),
					  initial_tok->get_locus (), false,
					  false, std::move (bind_pattern)));
	  }
	Error error (
	  t->get_locus (),
	  "failed to parse pattern bind to a path, not an identifier");
	add_error (std::move (error));

	return nullptr;
      }
    default:
      // assume identifier if single segment
      if (path.is_single_segment ())
	{
	  return std::unique_ptr<AST::IdentifierPattern> (
	    new AST::IdentifierPattern (std::move (initial_ident),
					initial_tok->get_locus ()));
	}
      // return path otherwise
      return std::unique_ptr<AST::PathInExpression> (
	new AST::PathInExpression (std::move (path)));
    }
}

// Parses struct pattern elements if they exist.
template <typename ManagedTokenSource>
AST::StructPatternElements
Parser<ManagedTokenSource>::parse_struct_pattern_elems ()
{
  std::vector<std::unique_ptr<AST::StructPatternField>> fields;

  AST::AttrVec etc_attrs;
  bool has_rest = false;

  // try parsing struct pattern fields
  const_TokenPtr t = lexer.peek_token ();
  while (t->get_id () != RIGHT_CURLY)
    {
      AST::AttrVec outer_attrs = parse_outer_attributes ();

      // parse etc (must be last in struct pattern, so breaks)
      if (lexer.peek_token ()->get_id () == DOT_DOT)
	{
	  lexer.skip_token ();
	  etc_attrs = std::move (outer_attrs);
	  has_rest = true;
	  break;
	}

      std::unique_ptr<AST::StructPatternField> field
	= parse_struct_pattern_field_partial (std::move (outer_attrs));
      if (field == nullptr)
	{
	  Error error (lexer.peek_token ()->get_locus (),
		       "failed to parse struct pattern field");
	  add_error (std::move (error));

	  // skip after somewhere?
	  return AST::StructPatternElements::create_empty ();
	}
      fields.push_back (std::move (field));

      if (lexer.peek_token ()->get_id () != COMMA)
	break;

      // skip comma
      lexer.skip_token ();
      t = lexer.peek_token ();
    }

  if (has_rest)
    return AST::StructPatternElements (std::move (fields),
				       std::move (etc_attrs));
  else
    return AST::StructPatternElements (std::move (fields));
}

/* Parses a struct pattern field (tuple index/pattern, identifier/pattern, or
 * identifier). */
template <typename ManagedTokenSource>
std::unique_ptr<AST::StructPatternField>
Parser<ManagedTokenSource>::parse_struct_pattern_field ()
{
  // parse outer attributes (if they exist)
  AST::AttrVec outer_attrs = parse_outer_attributes ();

  return parse_struct_pattern_field_partial (std::move (outer_attrs));
}

/* Parses a struct pattern field (tuple index/pattern, identifier/pattern, or
 * identifier), with outer attributes passed in. */
template <typename ManagedTokenSource>
std::unique_ptr<AST::StructPatternField>
Parser<ManagedTokenSource>::parse_struct_pattern_field_partial (
  AST::AttrVec outer_attrs)
{
  // branch based on next token
  const_TokenPtr t = lexer.peek_token ();
  switch (t->get_id ())
    {
    case INT_LITERAL:
      {
	// tuple index
	std::string index_str = t->get_str ();
	int index = atoi (index_str.c_str ());

	lexer.skip_token ();

	if (!skip_token (COLON))
	  {
	    return nullptr;
	  }

	// parse required pattern
	std::unique_ptr<AST::Pattern> pattern = parse_pattern ();
	if (pattern == nullptr)
	  {
	    Error error (
	      t->get_locus (),
	      "failed to parse pattern in tuple index struct pattern field");
	    add_error (std::move (error));

	    return nullptr;
	  }

	return std::unique_ptr<AST::StructPatternFieldTuplePat> (
	  new AST::StructPatternFieldTuplePat (index, std::move (pattern),
					       std::move (outer_attrs),
					       t->get_locus ()));
      }
    case IDENTIFIER:
      // identifier-pattern OR only identifier
      // branch on next token
      switch (lexer.peek_token (1)->get_id ())
	{
	case COLON:
	  {
	    // identifier-pattern
	    Identifier ident{t};
	    lexer.skip_token ();

	    skip_token (COLON);

	    // parse required pattern
	    std::unique_ptr<AST::Pattern> pattern = parse_pattern ();
	    if (pattern == nullptr)
	      {
		Error error (t->get_locus (),
			     "failed to parse pattern in struct pattern field");
		add_error (std::move (error));

		return nullptr;
	      }

	    return std::unique_ptr<AST::StructPatternFieldIdentPat> (
	      new AST::StructPatternFieldIdentPat (std::move (ident),
						   std::move (pattern),
						   std::move (outer_attrs),
						   t->get_locus ()));
	  }
	case COMMA:
	case RIGHT_CURLY:
	  {
	    // identifier only
	    Identifier ident = {t};
	    lexer.skip_token ();

	    return std::unique_ptr<AST::StructPatternFieldIdent> (
	      new AST::StructPatternFieldIdent (std::move (ident), false, false,
						std::move (outer_attrs),
						t->get_locus ()));
	  }
	default:
	  // error
	  add_error (Error (t->get_locus (),
			    "unrecognised token %qs in struct pattern field",
			    t->get_token_description ()));

	  return nullptr;
	}
    case REF:
    case MUT:
      {
	// only identifier
	bool has_ref = false;
	if (t->get_id () == REF)
	  {
	    has_ref = true;
	    lexer.skip_token ();
	  }

	bool has_mut = false;
	if (lexer.peek_token ()->get_id () == MUT)
	  {
	    has_mut = true;
	    lexer.skip_token ();
	  }

	const_TokenPtr ident_tok = expect_token (IDENTIFIER);
	if (ident_tok == nullptr)
	  {
	    return nullptr;
	  }
	Identifier ident{ident_tok};

	return std::unique_ptr<AST::StructPatternFieldIdent> (
	  new AST::StructPatternFieldIdent (std::move (ident), has_ref, has_mut,
					    std::move (outer_attrs),
					    t->get_locus ()));
      }
    default:
      // not necessarily an error
      return nullptr;
    }
}

/* Parses a literal pattern or range pattern. Assumes that literals passed in
 * are valid range pattern bounds. Do not pass in paths in expressions, for
 * instance. */
template <typename ManagedTokenSource>
std::unique_ptr<AST::Pattern>
Parser<ManagedTokenSource>::parse_literal_or_range_pattern ()
{
  const_TokenPtr range_lower = lexer.peek_token ();
  AST::Literal::LitType type = AST::Literal::STRING;
  bool has_minus = false;

  // get lit type
  switch (range_lower->get_id ())
    {
    case CHAR_LITERAL:
      type = AST::Literal::CHAR;
      lexer.skip_token ();
      break;
    case BYTE_CHAR_LITERAL:
      type = AST::Literal::BYTE;
      lexer.skip_token ();
      break;
    case INT_LITERAL:
      type = AST::Literal::INT;
      lexer.skip_token ();
      break;
    case FLOAT_LITERAL:
      type = AST::Literal::FLOAT;
      lexer.skip_token ();
      break;
    case MINUS:
      // branch on next token
      range_lower = lexer.peek_token (1);
      switch (range_lower->get_id ())
	{
	case INT_LITERAL:
	  type = AST::Literal::INT;
	  has_minus = true;
	  lexer.skip_token (1);
	  break;
	case FLOAT_LITERAL:
	  type = AST::Literal::FLOAT;
	  has_minus = true;
	  lexer.skip_token (1);
	  break;
	default:
	  add_error (Error (range_lower->get_locus (),
			    "token type %qs cannot be parsed as range pattern "
			    "bound or literal after minus symbol",
			    range_lower->get_token_description ()));

	  return nullptr;
	}
      break;
    default:
      add_error (
	Error (range_lower->get_locus (),
	       "token type %qs cannot be parsed as range pattern bound",
	       range_lower->get_token_description ()));

      return nullptr;
    }

  const_TokenPtr next = lexer.peek_token ();
  if (next->get_id () == DOT_DOT_EQ || next->get_id () == ELLIPSIS
      || next->get_id () == DOT_DOT)
    {
      AST::RangeKind kind = AST::tokenid_to_rangekind (next->get_id ());
      // range pattern
      lexer.skip_token ();
      std::unique_ptr<AST::RangePatternBound> lower (
	new AST::RangePatternBoundLiteral (
	  AST::Literal (range_lower->get_str (), type,
			PrimitiveCoreType::CORETYPE_UNKNOWN),
	  range_lower->get_locus (), has_minus));

      std::unique_ptr<AST::RangePatternBound> upper
	= parse_range_pattern_bound ();
      if (upper == nullptr)
	{
	  Error error (next->get_locus (),
		       "failed to parse range pattern bound in range pattern");
	  add_error (std::move (error));

	  return nullptr;
	}

      return std::unique_ptr<AST::RangePattern> (
	new AST::RangePattern (std::move (lower), std::move (upper), kind,
			       range_lower->get_locus ()));
    }
  else
    {
      // literal pattern
      return std::unique_ptr<AST::LiteralPattern> (
	new AST::LiteralPattern (range_lower->get_str (), type,
				 range_lower->get_locus (),
				 range_lower->get_type_hint (), has_minus));
    }
}

// Parses a range pattern bound (value only).
template <typename ManagedTokenSource>
std::unique_ptr<AST::RangePatternBound>
Parser<ManagedTokenSource>::parse_range_pattern_bound ()
{
  const_TokenPtr range_lower = lexer.peek_token ();
  location_t range_lower_locus = range_lower->get_locus ();

  // get lit type
  switch (range_lower->get_id ())
    {
    case CHAR_LITERAL:
      lexer.skip_token ();
      return std::unique_ptr<AST::RangePatternBoundLiteral> (
	new AST::RangePatternBoundLiteral (
	  AST::Literal (range_lower->get_str (), AST::Literal::CHAR,
			range_lower->get_type_hint ()),
	  range_lower_locus));
    case BYTE_CHAR_LITERAL:
      lexer.skip_token ();
      return std::unique_ptr<AST::RangePatternBoundLiteral> (
	new AST::RangePatternBoundLiteral (
	  AST::Literal (range_lower->get_str (), AST::Literal::BYTE,
			range_lower->get_type_hint ()),
	  range_lower_locus));
    case INT_LITERAL:
      lexer.skip_token ();
      return std::unique_ptr<AST::RangePatternBoundLiteral> (
	new AST::RangePatternBoundLiteral (
	  AST::Literal (range_lower->get_str (), AST::Literal::INT,
			range_lower->get_type_hint ()),
	  range_lower_locus));
    case FLOAT_LITERAL:
      lexer.skip_token ();
      rust_debug ("warning: used deprecated float range pattern bound");
      return std::unique_ptr<AST::RangePatternBoundLiteral> (
	new AST::RangePatternBoundLiteral (
	  AST::Literal (range_lower->get_str (), AST::Literal::FLOAT,
			range_lower->get_type_hint ()),
	  range_lower_locus));
    case MINUS:
      // branch on next token
      range_lower = lexer.peek_token (1);
      switch (range_lower->get_id ())
	{
	case INT_LITERAL:
	  lexer.skip_token (1);
	  return std::unique_ptr<AST::RangePatternBoundLiteral> (
	    new AST::RangePatternBoundLiteral (
	      AST::Literal (range_lower->get_str (), AST::Literal::INT,
			    range_lower->get_type_hint ()),
	      range_lower_locus, true));
	case FLOAT_LITERAL:
	  lexer.skip_token (1);
	  rust_debug ("warning: used deprecated float range pattern bound");
	  return std::unique_ptr<AST::RangePatternBoundLiteral> (
	    new AST::RangePatternBoundLiteral (
	      AST::Literal (range_lower->get_str (), AST::Literal::FLOAT,
			    range_lower->get_type_hint ()),
	      range_lower_locus, true));
	default:
	  add_error (Error (range_lower->get_locus (),
			    "token type %qs cannot be parsed as range pattern "
			    "bound after minus symbol",
			    range_lower->get_token_description ()));

	  return nullptr;
	}
    case IDENTIFIER:
    case SUPER:
    case SELF:
    case SELF_ALIAS:
    case CRATE:
    case SCOPE_RESOLUTION:
    case DOLLAR_SIGN:
      {
	// path in expression
	AST::PathInExpression path = parse_path_in_expression ();
	if (path.is_error ())
	  {
	    Error error (
	      range_lower->get_locus (),
	      "failed to parse path in expression range pattern bound");
	    add_error (std::move (error));

	    return nullptr;
	  }
	return std::unique_ptr<AST::RangePatternBoundPath> (
	  new AST::RangePatternBoundPath (std::move (path)));
      }
    case LEFT_SHIFT:
    case LEFT_ANGLE:
      {
	// qualified path in expression
	AST::QualifiedPathInExpression path
	  = parse_qualified_path_in_expression ();
	if (path.is_error ())
	  {
	    Error error (range_lower->get_locus (),
			 "failed to parse qualified path in expression range "
			 "pattern bound");
	    add_error (std::move (error));

	    return nullptr;
	  }
	return std::unique_ptr<AST::RangePatternBoundQualPath> (
	  new AST::RangePatternBoundQualPath (std::move (path)));
      }
    default:
      add_error (
	Error (range_lower->get_locus (),
	       "token type %qs cannot be parsed as range pattern bound",
	       range_lower->get_token_description ()));

      return nullptr;
    }
}

} // namespace Rust
