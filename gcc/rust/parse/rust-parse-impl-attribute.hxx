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
 *   by rust-parse-impl*.h
 * This is also the reason why there are no include guards. */

#include "rust-parse.h"
#include "rust-parse-error.h"
#include "rust-attribute-values.h"
#include "expected.h"

namespace Rust {

// Parse a inner or outer doc comment into an doc attribute
template <typename ManagedTokenSource>
Parse::AttributeBody
Parser<ManagedTokenSource>::parse_doc_comment ()
{
  const_TokenPtr token = lexer.peek_token ();
  location_t locus = token->get_locus ();
  AST::SimplePathSegment segment (Values::Attributes::DOC, locus);
  std::vector<AST::SimplePathSegment> segments;
  segments.push_back (std::move (segment));
  AST::SimplePath attr_path (std::move (segments), false, locus);
  AST::LiteralExpr lit_expr (token->get_str (), AST::Literal::STRING,
			     PrimitiveCoreType::CORETYPE_STR, {}, locus);
  std::unique_ptr<AST::AttrInput> attr_input (
    new AST::AttrInputLiteral (std::move (lit_expr)));
  lexer.skip_token ();

  return Parse::AttributeBody{std::move (attr_path), std::move (attr_input),
			      locus};
}

// Parse a single inner attribute.
template <typename ManagedTokenSource>
tl::expected<AST::Attribute, Parse::Error::Attribute>
Parser<ManagedTokenSource>::parse_inner_attribute ()
{
  if (lexer.peek_token ()->get_id () == INNER_DOC_COMMENT)
    {
      auto body = parse_doc_comment ();
      return AST::Attribute (std::move (body.path), std::move (body.input),
			     body.locus, true);
    }

  rust_assert (lexer.peek_token ()->get_id () == HASH);

  lexer.skip_token ();

  if (lexer.peek_token ()->get_id () != EXCLAM)
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "expected %<!%> or %<[%> for inner attribute");
      add_error (std::move (error));

      return Parse::Error::Attribute::make_malformed ();
    }
  lexer.skip_token ();

  if (!skip_token (LEFT_SQUARE))
    return Parse::Error::Attribute::make_malformed ();

  auto body_res = parse_attribute_body ();
  if (!body_res)
    return Parse::Error::Attribute::make_malformed ();
  auto body = std::move (body_res.value ());

  auto actual_attribute
    = AST::Attribute (std::move (body.path), std::move (body.input), body.locus,
		      true);

  if (!skip_token (RIGHT_SQUARE))
    return Parse::Error::Attribute::make_malformed ();

  return actual_attribute;
}

// Parse a single outer attribute.
template <typename ManagedTokenSource>
tl::expected<AST::Attribute, Parse::Error::Attribute>
Parser<ManagedTokenSource>::parse_outer_attribute ()
{
  if (lexer.peek_token ()->get_id () == OUTER_DOC_COMMENT)
    {
      auto body = parse_doc_comment ();
      return AST::Attribute (std::move (body.path), std::move (body.input),
			     body.locus, false);
    }

  if (lexer.peek_token ()->get_id () == INNER_DOC_COMMENT)
    {
      Error error (
	lexer.peek_token ()->get_locus (), ErrorCode::E0753,
	"expected outer doc comment, inner doc (%<//!%> or %</*!%>) only "
	"allowed at start of item "
	"and before any outer attribute or doc (%<#[%>, %<///%> or %</**%>)");
      add_error (std::move (error));
      lexer.skip_token ();
      return Parse::Error::Attribute::make_unexpected_inner ();
    }

  /* OuterAttribute -> '#' '[' Attr ']' */

  if (lexer.peek_token ()->get_id () != HASH)
    return Parse::Error::Attribute::make_malformed ();

  lexer.skip_token ();

  TokenId id = lexer.peek_token ()->get_id ();
  if (id != LEFT_SQUARE)
    {
      if (id == EXCLAM)
	{
	  // this is inner attribute syntax, so throw error
	  // inner attributes were either already parsed or not allowed here.
	  Error error (
	    lexer.peek_token ()->get_locus (),
	    "token %<!%> found, indicating inner attribute definition. Inner "
	    "attributes are not possible at this location");
	  add_error (std::move (error));
	}
      return Parse::Error::Attribute::make_unexpected_inner ();
    }

  lexer.skip_token ();

  auto body_res = parse_attribute_body ();
  if (!body_res)
    return Parse::Error::Attribute::make_malformed_body ();
  auto body = std::move (body_res.value ());

  auto actual_attribute
    = AST::Attribute (std::move (body.path), std::move (body.input), body.locus,
		      false);

  if (lexer.peek_token ()->get_id () != RIGHT_SQUARE)
    return Parse::Error::Attribute::make_malformed ();

  lexer.skip_token ();

  return actual_attribute;
}

// Parses the body of an attribute (inner or outer).
template <typename ManagedTokenSource>
tl::expected<Parse::AttributeBody, Parse::Error::AttributeBody>
Parser<ManagedTokenSource>::parse_attribute_body ()
{
  location_t locus = lexer.peek_token ()->get_locus ();

  auto attr_path = parse_simple_path ();
  // ensure path is valid to parse attribute input
  if (!attr_path)
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "empty simple path in attribute");
      add_error (std::move (error));

      // Skip past potential further info in attribute (i.e. attr_input)
      skip_after_end_attribute ();
      return Parse::Error::AttributeBody::make_invalid_path ();
    }

  auto attr_input = parse_attr_input ();
  // AttrInput is allowed to be null, so no checks here
  if (attr_input)
    return Parse::AttributeBody{std::move (attr_path.value ()),
				std::move (attr_input.value ()), locus};
  else if (attr_input.error ().kind == Parse::Error::AttrInput::Kind::MISSING)
    return Parse::AttributeBody{std::move (attr_path.value ()), nullptr, locus};
  else
    return Parse::Error::AttributeBody::make_invalid_attrinput ();
}

// Parse a contiguous block of inner attributes.
template <typename ManagedTokenSource>
AST::AttrVec
Parser<ManagedTokenSource>::parse_inner_attributes ()
{
  AST::AttrVec inner_attributes;

  auto has_valid_inner_attribute_prefix = [&] () {
    auto id = lexer.peek_token ()->get_id ();
    /* Outer attribute `#[` is not allowed, only accepts `#!` */
    return (id == HASH && lexer.peek_token (1)->get_id () == EXCLAM)
	   || id == INNER_DOC_COMMENT;
  };

  while (has_valid_inner_attribute_prefix ())
    {
      auto inner_attr = parse_inner_attribute ();

      /* Ensure only valid inner attributes are added to the inner_attributes
       * list */
      if (inner_attr)
	{
	  inner_attributes.push_back (std::move (inner_attr.value ()));
	}
      else
	{
	  /* If no more valid inner attributes, break out of loop (only
	   * contiguous inner attributes parsed). */
	  break;
	}
    }

  inner_attributes.shrink_to_fit ();
  return inner_attributes;
}

// Parses a contiguous block of outer attributes.
template <typename ManagedTokenSource>
AST::AttrVec
Parser<ManagedTokenSource>::parse_outer_attributes ()
{
  AST::AttrVec outer_attributes;

  auto has_valid_attribute_prefix = [&] () {
    auto id = lexer.peek_token ()->get_id ();
    /* We allow inner attributes `#!` and catch the error later  */
    return id == HASH || id == OUTER_DOC_COMMENT || id == INNER_DOC_COMMENT;
  };

  while (has_valid_attribute_prefix ()) /* For error handling.  */
    {
      auto outer_attr = parse_outer_attribute ();

      /* Ensure only valid outer attributes are added to the outer_attributes
       * list */
      if (outer_attr)
	{
	  outer_attributes.push_back (std::move (outer_attr.value ()));
	}
      else
	{
	  /* If no more valid outer attributes, break out of loop (only
	   * contiguous outer attributes parsed). */
	  break;
	}
    }

  outer_attributes.shrink_to_fit ();
  return outer_attributes;

  /* TODO: this shares basically all code with parse_inner_attributes except
   * function call - find way of making it more modular? function pointer? */
}

// Parses an AttrInput AST node (polymorphic, as AttrInput is abstract)
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::AttrInput>, Parse::Error::AttrInput>
Parser<ManagedTokenSource>::parse_attr_input ()
{
  const_TokenPtr t = lexer.peek_token ();
  switch (t->get_id ())
    {
    case LEFT_PAREN:
    case LEFT_SQUARE:
    case LEFT_CURLY:
      {
	auto dtoken_tree = parse_delim_token_tree ();
	if (!dtoken_tree)
	  return Parse::Error::AttrInput::make_bad_token_tree ();

	// must be a delimited token tree, so parse that
	std::unique_ptr<AST::AttrInput> input_tree (
	  new AST::DelimTokenTree (dtoken_tree.value ()));

	return tl::expected<std::unique_ptr<AST::AttrInput>,
			    Parse::Error::AttrInput>{std::move (input_tree)};
      }
    case EQUAL:
      {
	// = LiteralExpr
	lexer.skip_token ();

	t = lexer.peek_token ();

	// attempt to parse macro
	// TODO: macros may/may not be allowed in attributes
	// this is needed for "#[doc = include_str!(...)]"
	if (Parse::Utils::is_simple_path_segment (t->get_id ()))
	  {
	    std::unique_ptr<AST::MacroInvocation> invoke
	      = parse_macro_invocation ({});

	    if (!invoke)
	      return Parse::Error::AttrInput::make_bad_macro_invocation ();

	    return std::unique_ptr<AST::AttrInput> (
	      new AST::AttrInputMacro (std::move (invoke)));
	  }

	/* Ensure token is a "literal expression" (literally only a literal
	 * token of any type) */
	if (!t->is_literal ())
	  {
	    Error error (
	      t->get_locus (),
	      "unknown token %qs in attribute body - literal expected",
	      t->get_token_description ());
	    add_error (std::move (error));

	    skip_after_end_attribute ();
	    return Parse::Error::AttrInput::make_malformed ();
	  }

	AST::Literal::LitType lit_type = AST::Literal::STRING;
	// Crappy mapping of token type to literal type
	switch (t->get_id ())
	  {
	  case INT_LITERAL:
	    lit_type = AST::Literal::INT;
	    break;
	  case FLOAT_LITERAL:
	    lit_type = AST::Literal::FLOAT;
	    break;
	  case CHAR_LITERAL:
	    lit_type = AST::Literal::CHAR;
	    break;
	  case BYTE_CHAR_LITERAL:
	    lit_type = AST::Literal::BYTE;
	    break;
	  case BYTE_STRING_LITERAL:
	    lit_type = AST::Literal::BYTE_STRING;
	    break;
	  case RAW_STRING_LITERAL:
	    lit_type = AST::Literal::RAW_STRING;
	    break;
	  case STRING_LITERAL:
	  default:
	    lit_type = AST::Literal::STRING;
	    break; // TODO: raw string? don't eliminate it from lexer?
	  }

	// create actual LiteralExpr
	AST::LiteralExpr lit_expr (t->get_str (), lit_type, t->get_type_hint (),
				   {}, t->get_locus ());
	lexer.skip_token ();

	std::unique_ptr<AST::AttrInput> attr_input_lit (
	  new AST::AttrInputLiteral (std::move (lit_expr)));

	// do checks or whatever? none required, really

	// FIXME: shouldn't a skip token be required here?

	return tl::expected<std::unique_ptr<AST::AttrInput>,
			    Parse::Error::AttrInput>{
	  std::move (attr_input_lit)};
      }
      break;
    case RIGHT_PAREN:
    case RIGHT_SQUARE:
    case RIGHT_CURLY:
    case END_OF_FILE:
      // means AttrInput is missing, which is allowed
      return Parse::Error::AttrInput::make_missing_attrinput ();
    default:
      add_error (
	Error (t->get_locus (),
	       "unknown token %qs in attribute body - attribute input or "
	       "none expected",
	       t->get_token_description ()));

      skip_after_end_attribute ();
      return Parse::Error::AttrInput::make_malformed ();
    }
  rust_unreachable ();
  // TODO: find out how to stop gcc error on "no return value"
}

} // namespace Rust
