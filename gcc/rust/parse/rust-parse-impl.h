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

/* Template implementation for Rust::Parser. Previously in rust-parse.cc (before
 * Parser was template). Separated from rust-parse.h for readability. */

/* DO NOT INCLUDE ANYWHERE - this is automatically included with rust-parse.h
 * This is also the reason why there are no include guards. */

#define INCLUDE_ALGORITHM
#include "rust-diagnostics.h"
#include "rust-make-unique.h"

namespace Rust {
// Left binding powers of operations.
enum binding_powers
{
  // Highest priority
  LBP_HIGHEST = 100,

  LBP_PATH = 95,

  LBP_METHOD_CALL = 90,

  LBP_FIELD_EXPR = 85,

  LBP_FUNCTION_CALL = 80,
  LBP_ARRAY_REF = LBP_FUNCTION_CALL,

  LBP_QUESTION_MARK = 75, // unary postfix - counts as left

  LBP_UNARY_PLUS = 70,		    // Used only when the null denotation is +
  LBP_UNARY_MINUS = LBP_UNARY_PLUS, // Used only when the null denotation is -
  LBP_UNARY_ASTERISK = LBP_UNARY_PLUS, // deref operator - unary prefix
  LBP_UNARY_EXCLAM = LBP_UNARY_PLUS,
  LBP_UNARY_AMP = LBP_UNARY_PLUS,
  LBP_UNARY_AMP_MUT = LBP_UNARY_PLUS,

  LBP_AS = 65,

  LBP_MUL = 60,
  LBP_DIV = LBP_MUL,
  LBP_MOD = LBP_MUL,

  LBP_PLUS = 55,
  LBP_MINUS = LBP_PLUS,

  LBP_L_SHIFT = 50,
  LBP_R_SHIFT = LBP_L_SHIFT,

  LBP_AMP = 45,

  LBP_CARET = 40,

  LBP_PIPE = 35,

  LBP_EQUAL = 30,
  LBP_NOT_EQUAL = LBP_EQUAL,
  LBP_SMALLER_THAN = LBP_EQUAL,
  LBP_SMALLER_EQUAL = LBP_EQUAL,
  LBP_GREATER_THAN = LBP_EQUAL,
  LBP_GREATER_EQUAL = LBP_EQUAL,

  LBP_LOGICAL_AND = 25,

  LBP_LOGICAL_OR = 20,

  LBP_DOT_DOT = 15,
  LBP_DOT_DOT_EQ = LBP_DOT_DOT,

  // TODO: note all these assig operators are RIGHT associative!
  LBP_ASSIG = 10,
  LBP_PLUS_ASSIG = LBP_ASSIG,
  LBP_MINUS_ASSIG = LBP_ASSIG,
  LBP_MULT_ASSIG = LBP_ASSIG,
  LBP_DIV_ASSIG = LBP_ASSIG,
  LBP_MOD_ASSIG = LBP_ASSIG,
  LBP_AMP_ASSIG = LBP_ASSIG,
  LBP_PIPE_ASSIG = LBP_ASSIG,
  LBP_CARET_ASSIG = LBP_ASSIG,
  LBP_L_SHIFT_ASSIG = LBP_ASSIG,
  LBP_R_SHIFT_ASSIG = LBP_ASSIG,

  // return, break, and closures as lowest priority?
  LBP_RETURN = 5,
  LBP_BREAK = LBP_RETURN,
  LBP_CLOSURE = LBP_RETURN, // unary prefix operators

#if 0
  // rust precedences
  // used for closures
  PREC_CLOSURE = -40,
  // used for break, continue, return, and yield
  PREC_JUMP = -30,
  // used for range (although weird comment in rustc about this)
  PREC_RANGE = -10,
  // used for binary operators mentioned below - also cast, colon (type),
  // assign, assign_op
  PREC_BINOP = FROM_ASSOC_OP,
  // used for box, address_of, let, unary (again, weird comment on let)
  PREC_PREFIX = 50,
  // used for await, call, method call, field, index, try,
  // inline asm, macro invocation
  PREC_POSTFIX = 60,
  // used for array, repeat, tuple, literal, path, paren, if,
  // while, for, 'loop', match, block, try block, async, struct
  PREC_PAREN = 99,
  PREC_FORCE_PAREN = 100,
#endif

  // lowest priority
  LBP_LOWEST = 0
};

/* Returns whether the token can start a type (i.e. there is a valid type
 * beginning with the token). */
inline bool
can_tok_start_type (TokenId id)
{
  switch (id)
    {
    case EXCLAM:
    case LEFT_SQUARE:
    case LEFT_ANGLE:
    case UNDERSCORE:
    case ASTERISK:
    case AMP:
    case LIFETIME:
    case IDENTIFIER:
    case SUPER:
    case SELF:
    case SELF_ALIAS:
    case CRATE:
    case DOLLAR_SIGN:
    case SCOPE_RESOLUTION:
    case LEFT_PAREN:
    case FOR:
    case ASYNC:
    case CONST:
    case UNSAFE:
    case EXTERN_TOK:
    case FN_TOK:
    case IMPL:
    case DYN:
    case QUESTION_MARK:
      return true;
    default:
      return false;
    }
}

/* Returns whether the token id is (or is likely to be) a right angle bracket.
 * i.e. '>', '>>', '>=' and '>>=' tokens. */
inline bool
is_right_angle_tok (TokenId id)
{
  switch (id)
    {
    case RIGHT_ANGLE:
    case RIGHT_SHIFT:
    case GREATER_OR_EQUAL:
    case RIGHT_SHIFT_EQ:
      return true;
    default:
      return false;
    }
}

/* HACK-y special handling for skipping a right angle token at the end of
 * generic arguments.
 * Currently, this replaces the "current token" with one that is identical
 * except has the leading '>' removed (e.g. '>>' becomes '>'). This is bad
 * for several reasons - it modifies the token stream to something that
 * actually doesn't make syntactic sense, it may not worked if the token
 * has already been skipped, etc. It was done because it would not
 * actually require inserting new items into the token stream (which I
 * thought would take more work to not mess up) and because I wasn't sure
 * if the "already seen right angle" flag in the parser would work
 * correctly.
 * Those two other approaches listed are in my opinion actually better
 * long-term - insertion is probably best as it reflects syntactically
 * what occurs. On the other hand, I need to do a code audit to make sure
 * that insertion doesn't mess anything up. So that's a FIXME. */
template <typename ManagedTokenSource>
bool
Parser<ManagedTokenSource>::skip_generics_right_angle ()
{
  /* OK, new great idea. Have a lexer method called
   * "split_current_token(TokenType newLeft, TokenType newRight)", which is
   * called here with whatever arguments are appropriate. That lexer method
   * handles "replacing" the current token with the "newLeft" and "inserting"
   * the next token with the "newRight" (and creating a location, etc. for it)
   */

  /* HACK: special handling for right shift '>>', greater or equal '>=', and
   * right shift assig */
  // '>>='
  const_TokenPtr tok = lexer.peek_token ();
  switch (tok->get_id ())
    {
    case RIGHT_ANGLE:
      // this is good - skip token
      lexer.skip_token ();
      return true;
      case RIGHT_SHIFT: {
	// new implementation that should be better
	lexer.split_current_token (RIGHT_ANGLE, RIGHT_ANGLE);
	lexer.skip_token ();
	return true;
      }
      case GREATER_OR_EQUAL: {
	// new implementation that should be better
	lexer.split_current_token (RIGHT_ANGLE, EQUAL);
	lexer.skip_token ();
	return true;
      }
      case RIGHT_SHIFT_EQ: {
	// new implementation that should be better
	lexer.split_current_token (RIGHT_ANGLE, GREATER_OR_EQUAL);
	lexer.skip_token ();
	return true;
      }
    default:
      add_error (Error (tok->get_locus (),
			"expected %<>%> at end of generic argument - found %qs",
			tok->get_token_description ()));
      return false;
    }
}

/* Gets left binding power for specified token.
 * Not suitable for use at the moment or possibly ever because binding power
 * cannot be purely determined from operator token with Rust grammar - e.g.
 * method call and field access have
 * different left binding powers but the same operator token. */
template <typename ManagedTokenSource>
int
Parser<ManagedTokenSource>::left_binding_power (const_TokenPtr token)
{
  // HACK: called with "peek_token()", so lookahead is "peek_token(1)"
  switch (token->get_id ())
    {
      /* TODO: issue here - distinguish between method calls and field access
       * somehow? Also would have to distinguish between paths and function
       * calls (:: operator), maybe more stuff. */
      /* Current plan for tackling LBP - don't do it based on token, use
       * lookahead. Or alternatively, only use Pratt parsing for OperatorExpr
       * and handle other expressions without it. rustc only considers
       * arithmetic, logical/relational, 'as',
       * '?=', ranges, colons, and assignment to have operator precedence and
       * associativity rules applicable. It then has
       * a separate "ExprPrecedence" that also includes binary operators. */

      // TODO: handle operator overloading - have a function replace the
      // operator?

      /*case DOT:
	  return LBP_DOT;*/

    case SCOPE_RESOLUTION:
      rust_debug (
	"possible error - looked up LBP of scope resolution operator. should "
	"be handled elsewhere.");
      return LBP_PATH;

    /* Resolved by lookahead HACK that should work with current code. If next
     * token is identifier and token after that isn't parenthesised expression
     * list, it is a field reference. */
    case DOT:
      if (lexer.peek_token (1)->get_id () == IDENTIFIER
	  && lexer.peek_token (2)->get_id () != LEFT_PAREN)
	{
	  return LBP_FIELD_EXPR;
	}
      return LBP_METHOD_CALL;

    case LEFT_PAREN:
      return LBP_FUNCTION_CALL;

    case LEFT_SQUARE:
      return LBP_ARRAY_REF;

    // postfix question mark (i.e. error propagation expression)
    case QUESTION_MARK:
      return LBP_QUESTION_MARK;

    case AS:
      return LBP_AS;

    case ASTERISK:
      return LBP_MUL;
    case DIV:
      return LBP_DIV;
    case PERCENT:
      return LBP_MOD;

    case PLUS:
      return LBP_PLUS;
    case MINUS:
      return LBP_MINUS;

    case LEFT_SHIFT:
      return LBP_L_SHIFT;
    case RIGHT_SHIFT:
      return LBP_R_SHIFT;

    // binary & operator
    case AMP:
      return LBP_AMP;

    // binary ^ operator
    case CARET:
      return LBP_CARET;

    // binary | operator
    case PIPE:
      return LBP_PIPE;

    case EQUAL_EQUAL:
      return LBP_EQUAL;
    case NOT_EQUAL:
      return LBP_NOT_EQUAL;
    case RIGHT_ANGLE:
      return LBP_GREATER_THAN;
    case GREATER_OR_EQUAL:
      return LBP_GREATER_EQUAL;
    case LEFT_ANGLE:
      return LBP_SMALLER_THAN;
    case LESS_OR_EQUAL:
      return LBP_SMALLER_EQUAL;

    case LOGICAL_AND:
      return LBP_LOGICAL_AND;

    case OR:
      return LBP_LOGICAL_OR;

    case DOT_DOT:
      return LBP_DOT_DOT;

    case DOT_DOT_EQ:
      return LBP_DOT_DOT_EQ;

    case EQUAL:
      return LBP_ASSIG;
    case PLUS_EQ:
      return LBP_PLUS_ASSIG;
    case MINUS_EQ:
      return LBP_MINUS_ASSIG;
    case ASTERISK_EQ:
      return LBP_MULT_ASSIG;
    case DIV_EQ:
      return LBP_DIV_ASSIG;
    case PERCENT_EQ:
      return LBP_MOD_ASSIG;
    case AMP_EQ:
      return LBP_AMP_ASSIG;
    case PIPE_EQ:
      return LBP_PIPE_ASSIG;
    case CARET_EQ:
      return LBP_CARET_ASSIG;
    case LEFT_SHIFT_EQ:
      return LBP_L_SHIFT_ASSIG;
    case RIGHT_SHIFT_EQ:
      return LBP_R_SHIFT_ASSIG;

    /* HACK: float literal due to lexer misidentifying a dot then an integer as
     * a float */
    case FLOAT_LITERAL:
      return LBP_FIELD_EXPR;
      // field expr is same as tuple expr in precedence, i imagine
      // TODO: is this needed anymore? lexer shouldn't do that anymore

    // anything that can't appear in an infix position is given lowest priority
    default:
      return LBP_LOWEST;
    }
}

// Returns true when current token is EOF.
template <typename ManagedTokenSource>
bool
Parser<ManagedTokenSource>::done_end_of_file ()
{
  return lexer.peek_token ()->get_id () == END_OF_FILE;
}

// Parses a sequence of items within a module or the implicit top-level module
// in a crate
template <typename ManagedTokenSource>
std::vector<std::unique_ptr<AST::Item>>
Parser<ManagedTokenSource>::parse_items ()
{
  std::vector<std::unique_ptr<AST::Item>> items;

  const_TokenPtr t = lexer.peek_token ();
  while (t->get_id () != END_OF_FILE)
    {
      std::unique_ptr<AST::Item> item = parse_item (false);
      if (item == nullptr)
	{
	  Error error (lexer.peek_token ()->get_locus (),
		       "failed to parse item in crate");
	  add_error (std::move (error));

	  // TODO: should all items be cleared?
	  items = std::vector<std::unique_ptr<AST::Item>> ();
	  break;
	}

      items.push_back (std::move (item));

      t = lexer.peek_token ();
    }

  return items;
}

// Parses a crate (compilation unit) - entry point
template <typename ManagedTokenSource>
std::unique_ptr<AST::Crate>
Parser<ManagedTokenSource>::parse_crate ()
{
  // parse inner attributes
  AST::AttrVec inner_attrs = parse_inner_attributes ();

  // parse items
  std::vector<std::unique_ptr<AST::Item>> items = parse_items ();

  // emit all errors
  for (const auto &error : error_table)
    error.emit_error ();

  return std::unique_ptr<AST::Crate> (
    new AST::Crate (std::move (items), std::move (inner_attrs)));
}

// Parse a contiguous block of inner attributes.
template <typename ManagedTokenSource>
AST::AttrVec
Parser<ManagedTokenSource>::parse_inner_attributes ()
{
  AST::AttrVec inner_attributes;

  // only try to parse it if it starts with "#!" not only "#"
  while ((lexer.peek_token ()->get_id () == HASH
	  && lexer.peek_token (1)->get_id () == EXCLAM)
	 || lexer.peek_token ()->get_id () == INNER_DOC_COMMENT)
    {
      AST::Attribute inner_attr = parse_inner_attribute ();

      /* Ensure only valid inner attributes are added to the inner_attributes
       * list */
      if (!inner_attr.is_empty ())
	{
	  inner_attributes.push_back (std::move (inner_attr));
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

// Parse a inner or outer doc comment into an doc attribute
template <typename ManagedTokenSource>
AST::Attribute
Parser<ManagedTokenSource>::parse_doc_comment ()
{
  const_TokenPtr token = lexer.peek_token ();
  Location locus = token->get_locus ();
  AST::SimplePathSegment segment ("doc", locus);
  std::vector<AST::SimplePathSegment> segments;
  segments.push_back (std::move (segment));
  AST::SimplePath attr_path (std::move (segments), false, locus);
  AST::LiteralExpr lit_expr (token->get_str (), AST::Literal::STRING,
			     PrimitiveCoreType::CORETYPE_STR, {}, locus);
  std::unique_ptr<AST::AttrInput> attr_input (
    new AST::AttrInputLiteral (std::move (lit_expr)));
  lexer.skip_token ();
  return AST::Attribute (std::move (attr_path), std::move (attr_input), locus);
}

// Parse a single inner attribute.
template <typename ManagedTokenSource>
AST::Attribute
Parser<ManagedTokenSource>::parse_inner_attribute ()
{
  if (lexer.peek_token ()->get_id () == INNER_DOC_COMMENT)
    return parse_doc_comment ();

  if (lexer.peek_token ()->get_id () != HASH)
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "BUG: token %<#%> is missing, but %<parse_inner_attribute%> "
		   "was invoked");
      add_error (std::move (error));

      return AST::Attribute::create_empty ();
    }
  lexer.skip_token ();

  if (lexer.peek_token ()->get_id () != EXCLAM)
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "expected %<!%> or %<[%> for inner attribute");
      add_error (std::move (error));

      return AST::Attribute::create_empty ();
    }
  lexer.skip_token ();

  if (!skip_token (LEFT_SQUARE))
    return AST::Attribute::create_empty ();

  AST::Attribute actual_attribute = parse_attribute_body ();

  if (!skip_token (RIGHT_SQUARE))
    return AST::Attribute::create_empty ();

  return actual_attribute;
}

// Parses the body of an attribute (inner or outer).
template <typename ManagedTokenSource>
AST::Attribute
Parser<ManagedTokenSource>::parse_attribute_body ()
{
  Location locus = lexer.peek_token ()->get_locus ();

  AST::SimplePath attr_path = parse_simple_path ();
  // ensure path is valid to parse attribute input
  if (attr_path.is_empty ())
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "empty simple path in attribute");
      add_error (std::move (error));

      // Skip past potential further info in attribute (i.e. attr_input)
      skip_after_end_attribute ();
      return AST::Attribute::create_empty ();
    }

  std::unique_ptr<AST::AttrInput> attr_input = parse_attr_input ();
  // AttrInput is allowed to be null, so no checks here

  return AST::Attribute (std::move (attr_path), std::move (attr_input), locus);
}

/* Determines whether token is a valid simple path segment. This does not
 * include scope resolution operators. */
inline bool
is_simple_path_segment (TokenId id)
{
  switch (id)
    {
    case IDENTIFIER:
    case SUPER:
    case SELF:
    case CRATE:
      return true;
    case DOLLAR_SIGN:
      // assume that dollar sign leads to $crate
      return true;
    default:
      return false;
    }
}

// Parses a SimplePath AST node, if it exists. Does nothing otherwise.
template <typename ManagedTokenSource>
AST::SimplePath
Parser<ManagedTokenSource>::parse_simple_path ()
{
  bool has_opening_scope_resolution = false;
  Location locus = Linemap::unknown_location ();

  // don't parse anything if not a path upfront
  if (!is_simple_path_segment (lexer.peek_token ()->get_id ())
      && !is_simple_path_segment (lexer.peek_token (1)->get_id ()))
    return AST::SimplePath::create_empty ();

  /* Checks for opening scope resolution (i.e. global scope fully-qualified
   * path) */
  if (lexer.peek_token ()->get_id () == SCOPE_RESOLUTION)
    {
      has_opening_scope_resolution = true;

      locus = lexer.peek_token ()->get_locus ();

      lexer.skip_token ();
    }

  // Parse single required simple path segment
  AST::SimplePathSegment segment = parse_simple_path_segment ();

  // get location if not gotten already
  if (locus == Linemap::unknown_location ())
    locus = segment.get_locus ();

  std::vector<AST::SimplePathSegment> segments;

  // Return empty vector if first, actually required segment is an error
  if (segment.is_error ())
    return AST::SimplePath::create_empty ();

  segments.push_back (std::move (segment));

  // Parse all other simple path segments
  while (lexer.peek_token ()->get_id () == SCOPE_RESOLUTION)
    {
      // Skip scope resolution operator
      lexer.skip_token ();

      AST::SimplePathSegment new_segment = parse_simple_path_segment ();

      // Return path as currently constructed if segment in error state.
      if (new_segment.is_error ())
	break;

      segments.push_back (std::move (new_segment));
    }

  // DEBUG: check for any empty segments
  for (const auto &seg : segments)
    {
      if (seg.is_error ())
	{
	  rust_debug (
	    "when parsing simple path, somehow empty path segment was "
	    "not filtered out. Path begins with '%s'",
	    segments.at (0).as_string ().c_str ());
	}
    }

  return AST::SimplePath (std::move (segments), has_opening_scope_resolution,
			  locus);
  /* TODO: now that is_simple_path_segment exists, could probably start
   * actually making errors upon parse failure of segments and whatever */
}

/* Parses a single SimplePathSegment (does not handle the scope resolution
 * operators) */
template <typename ManagedTokenSource>
AST::SimplePathSegment
Parser<ManagedTokenSource>::parse_simple_path_segment ()
{
  const_TokenPtr t = lexer.peek_token ();
  switch (t->get_id ())
    {
    case IDENTIFIER:
      lexer.skip_token ();

      return AST::SimplePathSegment (t->get_str (), t->get_locus ());
    case SUPER:
      lexer.skip_token ();

      return AST::SimplePathSegment ("super", t->get_locus ());
    case SELF:
      lexer.skip_token ();

      return AST::SimplePathSegment ("self", t->get_locus ());
    case CRATE:
      lexer.skip_token ();

      return AST::SimplePathSegment ("crate", t->get_locus ());
    case DOLLAR_SIGN:
      if (lexer.peek_token (1)->get_id () == CRATE)
	{
	  lexer.skip_token (1);

	  return AST::SimplePathSegment ("$crate", t->get_locus ());
	}
      gcc_fallthrough ();
    default:
      // do nothing but inactivates warning from gcc when compiling
      /* could put the rust_error_at thing here but fallthrough (from failing
       * $crate condition) isn't completely obvious if it is. */

      // test prevent error
      return AST::SimplePathSegment::create_error ();
    }
  gcc_unreachable ();
  /*rust_error_at(
    t->get_locus(), "invalid token '%s' in simple path segment",
    t->get_token_description());*/
  // this is not necessarily an error, e.g. end of path
  // return AST::SimplePathSegment::create_error();
}

// Parses a PathIdentSegment - an identifier segment of a non-SimplePath path.
template <typename ManagedTokenSource>
AST::PathIdentSegment
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

      return AST::PathIdentSegment ("super", t->get_locus ());
    case SELF:
      lexer.skip_token ();

      return AST::PathIdentSegment ("self", t->get_locus ());
    case SELF_ALIAS:
      lexer.skip_token ();

      return AST::PathIdentSegment ("Self", t->get_locus ());
    case CRATE:
      lexer.skip_token ();

      return AST::PathIdentSegment ("crate", t->get_locus ());
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
      return AST::PathIdentSegment::create_error ();
    }
  gcc_unreachable ();
  // not necessarily an error
}

// Parses an AttrInput AST node (polymorphic, as AttrInput is abstract)
template <typename ManagedTokenSource>
std::unique_ptr<AST::AttrInput>
Parser<ManagedTokenSource>::parse_attr_input ()
{
  const_TokenPtr t = lexer.peek_token ();
  switch (t->get_id ())
    {
    case LEFT_PAREN:
    case LEFT_SQUARE:
      case LEFT_CURLY: {
	// must be a delimited token tree, so parse that
	std::unique_ptr<AST::AttrInput> input_tree (
	  new AST::DelimTokenTree (parse_delim_token_tree ()));

	// TODO: potential checks on DelimTokenTree before returning

	return input_tree;
      }
      case EQUAL: {
	// = LiteralExpr
	lexer.skip_token ();

	t = lexer.peek_token ();

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
	    return nullptr;
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

	return attr_input_lit;
      }
      break;
    case RIGHT_SQUARE:
      // means AttrInput is missing, which is allowed
      return nullptr;
    default:
      add_error (
	Error (t->get_locus (),
	       "unknown token %qs in attribute body - attribute input or "
	       "none expected",
	       t->get_token_description ()));

      skip_after_end_attribute ();
      return nullptr;
    }
  gcc_unreachable ();
  // TODO: find out how to stop gcc error on "no return value"
}

/* Returns true if the token id matches the delimiter type. Note that this only
 * operates for END delimiter tokens. */
inline bool
token_id_matches_delims (TokenId token_id, AST::DelimType delim_type)
{
  return ((token_id == RIGHT_PAREN && delim_type == AST::PARENS)
	  || (token_id == RIGHT_SQUARE && delim_type == AST::SQUARE)
	  || (token_id == RIGHT_CURLY && delim_type == AST::CURLY));
}

/* Returns true if the likely result of parsing the next few tokens is a path.
 * Not guaranteed, though, especially in the case of syntax errors. */
inline bool
is_likely_path_next (TokenId next_token_id)
{
  switch (next_token_id)
    {
    case IDENTIFIER:
    case SUPER:
    case SELF:
    case SELF_ALIAS:
    case CRATE:
    // maybe - maybe do extra check. But then requires another TokenId.
    case DOLLAR_SIGN:
    case SCOPE_RESOLUTION:
      return true;
    default:
      return false;
    }
}

// Parses a delimited token tree
template <typename ManagedTokenSource>
AST::DelimTokenTree
Parser<ManagedTokenSource>::parse_delim_token_tree ()
{
  const_TokenPtr t = lexer.peek_token ();
  lexer.skip_token ();
  Location initial_loc = t->get_locus ();

  // save delim type to ensure it is reused later
  AST::DelimType delim_type = AST::PARENS;

  // Map tokens to DelimType
  switch (t->get_id ())
    {
    case LEFT_PAREN:
      delim_type = AST::PARENS;
      break;
    case LEFT_SQUARE:
      delim_type = AST::SQUARE;
      break;
    case LEFT_CURLY:
      delim_type = AST::CURLY;
      break;
    default:
      add_error (Error (t->get_locus (),
			"unexpected token %qs - expecting delimiters (for a "
			"delimited token tree)",
			t->get_token_description ()));

      return AST::DelimTokenTree::create_empty ();
    }

  // parse actual token tree vector - 0 or more
  std::vector<std::unique_ptr<AST::TokenTree>> token_trees_in_tree;
  auto delim_open
    = std::unique_ptr<AST::Token> (new AST::Token (std::move (t)));
  token_trees_in_tree.push_back (std::move (delim_open));

  // repeat loop until finding the matching delimiter
  t = lexer.peek_token ();
  while (!token_id_matches_delims (t->get_id (), delim_type)
	 && t->get_id () != END_OF_FILE)
    {
      std::unique_ptr<AST::TokenTree> tok_tree = parse_token_tree ();

      if (tok_tree == nullptr)
	{
	  // TODO: is this error handling appropriate?
	  Error error (
	    t->get_locus (),
	    "failed to parse token tree in delimited token tree - found %qs",
	    t->get_token_description ());
	  add_error (std::move (error));

	  return AST::DelimTokenTree::create_empty ();
	}

      token_trees_in_tree.push_back (std::move (tok_tree));

      // lexer.skip_token();
      t = lexer.peek_token ();
    }
  auto delim_close
    = std::unique_ptr<AST::Token> (new AST::Token (std::move (t)));
  token_trees_in_tree.push_back (std::move (delim_close));

  AST::DelimTokenTree token_tree (delim_type, std::move (token_trees_in_tree),
				  initial_loc);

  // parse end delimiters
  t = lexer.peek_token ();

  if (token_id_matches_delims (t->get_id (), delim_type))
    {
      // tokens match opening delimiter, so skip.
      lexer.skip_token ();

      // DEBUG
      rust_debug ("finished parsing new delim token tree - peeked token is now "
		  "'%s' while t is '%s'",
		  lexer.peek_token ()->get_token_description (),
		  t->get_token_description ());

      return token_tree;
    }
  else
    {
      // tokens don't match opening delimiters, so produce error
      Error error (t->get_locus (),
		   "unexpected token %qs - expecting closing delimiter %qs "
		   "(for a delimited token tree)",
		   t->get_token_description (),
		   (delim_type == AST::PARENS
		      ? ")"
		      : (delim_type == AST::SQUARE ? "]" : "}")));
      add_error (std::move (error));

      /* return empty token tree despite possibly parsing valid token tree -
       * TODO is this a good idea? */
      return AST::DelimTokenTree::create_empty ();
    }
}

/* Parses a TokenTree syntactical production. This is either a delimited token
 * tree or a non-delimiter token. */
template <typename ManagedTokenSource>
std::unique_ptr<AST::TokenTree>
Parser<ManagedTokenSource>::parse_token_tree ()
{
  const_TokenPtr t = lexer.peek_token ();

  switch (t->get_id ())
    {
    case LEFT_PAREN:
    case LEFT_SQUARE:
    case LEFT_CURLY:
      // Parse delimited token tree
      // TODO: use move rather than copy constructor
      return std::unique_ptr<AST::DelimTokenTree> (
	new AST::DelimTokenTree (parse_delim_token_tree ()));
    case RIGHT_PAREN:
    case RIGHT_SQUARE:
    case RIGHT_CURLY:
      // error - should not be called when this a token
      add_error (
	Error (t->get_locus (),
	       "unexpected closing delimiter %qs - token tree requires "
	       "either paired delimiters or non-delimiter tokens",
	       t->get_token_description ()));

      lexer.skip_token ();
      return nullptr;
    default:
      // parse token itself as TokenTree
      lexer.skip_token ();
      return std::unique_ptr<AST::Token> (new AST::Token (std::move (t)));
    }
}

// Parses a single item
template <typename ManagedTokenSource>
std::unique_ptr<AST::Item>
Parser<ManagedTokenSource>::parse_item (bool called_from_statement)
{
  // has a "called_from_statement" parameter for better error message handling

  // parse outer attributes for item
  AST::AttrVec outer_attrs = parse_outer_attributes ();

  // TODO: decide how to deal with VisItem vs MacroItem dichotomy
  /* best current solution: catch all keywords that would imply a VisItem in a
   * switch and have MacroItem as a last resort */

  const_TokenPtr t = lexer.peek_token ();

  switch (t->get_id ())
    {
    case END_OF_FILE:
      // not necessarily an error, unless we just read outer
      // attributes which needs to be attached
      if (!outer_attrs.empty ())
	{
	  Rust::AST::Attribute attr = outer_attrs.back ();
	  Error error (attr.get_locus (),
		       "expected item after outer attribute or doc comment");
	  add_error (std::move (error));
	}
      return nullptr;
    case PUB:
    case MOD:
    case EXTERN_TOK:
    case USE:
    case FN_TOK:
    case TYPE:
    case STRUCT_TOK:
    case ENUM_TOK:
    case CONST:
    case STATIC_TOK:
    case TRAIT:
    case IMPL:
    /* TODO: implement union keyword but not really because of
     * context-dependence crappy hack way to parse a union written below to
     * separate it from the good code. */
    // case UNION:
    case UNSAFE: // maybe - unsafe traits are a thing
      // if any of these (should be all possible VisItem prefixes), parse a
      // VisItem
      return parse_vis_item (std::move (outer_attrs));
      break;
    case SUPER:
    case SELF:
    case CRATE:
    case DOLLAR_SIGN:
      // almost certainly macro invocation semi
      return parse_macro_item (std::move (outer_attrs));
      break;
    // crappy hack to do union "keyword"
    case IDENTIFIER:
      // TODO: ensure std::string and literal comparison works
      if (t->get_str () == "union"
	  && lexer.peek_token (1)->get_id () == IDENTIFIER)
	{
	  return parse_vis_item (std::move (outer_attrs));
	  // or should this go straight to parsing union?
	}
      else if (t->get_str () == "macro_rules")
	{
	  // macro_rules! macro item
	  return parse_macro_item (std::move (outer_attrs));
	}
      else if (lexer.peek_token (1)->get_id () == SCOPE_RESOLUTION
	       || lexer.peek_token (1)->get_id () == EXCLAM)
	{
	  /* path (probably) or macro invocation, so probably a macro invocation
	   * semi */
	  return parse_macro_item (std::move (outer_attrs));
	}
      gcc_fallthrough ();
    default:
      // otherwise unrecognised
      // return parse_macro_item(std::move(outer_attrs));
      add_error (Error (t->get_locus (),
			"unrecognised token %qs for start of %s",
			t->get_token_description (),
			called_from_statement ? "statement" : "item"));

      // skip somewhere?
      return nullptr;
      break;
    }
}

// Parses a contiguous block of outer attributes.
template <typename ManagedTokenSource>
AST::AttrVec
Parser<ManagedTokenSource>::parse_outer_attributes ()
{
  AST::AttrVec outer_attributes;

  while (lexer.peek_token ()->get_id ()
	   == HASH /* Can also be #!, which catches errors.  */
	 || lexer.peek_token ()->get_id () == OUTER_DOC_COMMENT
	 || lexer.peek_token ()->get_id ()
	      == INNER_DOC_COMMENT) /* For error handling.  */
    {
      AST::Attribute outer_attr = parse_outer_attribute ();

      /* Ensure only valid outer attributes are added to the outer_attributes
       * list */
      if (!outer_attr.is_empty ())
	{
	  outer_attributes.push_back (std::move (outer_attr));
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

// Parse a single outer attribute.
template <typename ManagedTokenSource>
AST::Attribute
Parser<ManagedTokenSource>::parse_outer_attribute ()
{
  if (lexer.peek_token ()->get_id () == OUTER_DOC_COMMENT)
    return parse_doc_comment ();

  if (lexer.peek_token ()->get_id () == INNER_DOC_COMMENT)
    {
      Error error (
	lexer.peek_token ()->get_locus (),
	"inner doc (%<//!%> or %</*!%>) only allowed at start of item "
	"and before any outer attribute or doc (%<#[%>, %<///%> or %</**%>)");
      add_error (std::move (error));
      lexer.skip_token ();
      return AST::Attribute::create_empty ();
    }

  /* OuterAttribute -> '#' '[' Attr ']' */

  if (lexer.peek_token ()->get_id () != HASH)
    return AST::Attribute::create_empty ();

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
      return AST::Attribute::create_empty ();
    }

  lexer.skip_token ();

  AST::Attribute actual_attribute = parse_attribute_body ();

  if (lexer.peek_token ()->get_id () != RIGHT_SQUARE)
    return AST::Attribute::create_empty ();

  lexer.skip_token ();

  return actual_attribute;
}

// Parses a VisItem (item that can have non-default visibility).
template <typename ManagedTokenSource>
std::unique_ptr<AST::VisItem>
Parser<ManagedTokenSource>::parse_vis_item (AST::AttrVec outer_attrs)
{
  // parse visibility, which may or may not exist
  AST::Visibility vis = parse_visibility ();

  // select VisItem to create depending on keyword
  const_TokenPtr t = lexer.peek_token ();

  switch (t->get_id ())
    {
    case MOD:
      return parse_module (std::move (vis), std::move (outer_attrs));
    case EXTERN_TOK:
      // lookahead to resolve syntactical production
      t = lexer.peek_token (1);

      switch (t->get_id ())
	{
	case CRATE:
	  return parse_extern_crate (std::move (vis), std::move (outer_attrs));
	case FN_TOK: // extern function
	  return parse_function (std::move (vis), std::move (outer_attrs));
	case LEFT_CURLY: // extern block
	  return parse_extern_block (std::move (vis), std::move (outer_attrs));
	case STRING_LITERAL: // for specifying extern ABI
	  // could be extern block or extern function, so more lookahead
	  t = lexer.peek_token (2);

	  switch (t->get_id ())
	    {
	    case FN_TOK:
	      return parse_function (std::move (vis), std::move (outer_attrs));
	    case LEFT_CURLY:
	      return parse_extern_block (std::move (vis),
					 std::move (outer_attrs));
	    default:
	      add_error (
		Error (t->get_locus (),
		       "unexpected token %qs in some sort of extern production",
		       t->get_token_description ()));

	      lexer.skip_token (2); // TODO: is this right thing to do?
	      return nullptr;
	    }
	default:
	  add_error (
	    Error (t->get_locus (),
		   "unexpected token %qs in some sort of extern production",
		   t->get_token_description ()));

	  lexer.skip_token (1); // TODO: is this right thing to do?
	  return nullptr;
	}
    case USE:
      return parse_use_decl (std::move (vis), std::move (outer_attrs));
    case FN_TOK:
      return parse_function (std::move (vis), std::move (outer_attrs));
    case TYPE:
      return parse_type_alias (std::move (vis), std::move (outer_attrs));
    case STRUCT_TOK:
      return parse_struct (std::move (vis), std::move (outer_attrs));
    case ENUM_TOK:
      return parse_enum (std::move (vis), std::move (outer_attrs));
    // TODO: implement union keyword but not really because of
    // context-dependence case UNION: crappy hack to do union "keyword"
    case IDENTIFIER:
      if (t->get_str () == "union"
	  && lexer.peek_token (1)->get_id () == IDENTIFIER)
	{
	  return parse_union (std::move (vis), std::move (outer_attrs));
	  // or should item switch go straight to parsing union?
	}
      else
	{
	  break;
	}
    case CONST:
      // lookahead to resolve syntactical production
      t = lexer.peek_token (1);

      switch (t->get_id ())
	{
	case IDENTIFIER:
	case UNDERSCORE:
	  return parse_const_item (std::move (vis), std::move (outer_attrs));
	case UNSAFE:
	case EXTERN_TOK:
	case FN_TOK:
	  return parse_function (std::move (vis), std::move (outer_attrs));
	default:
	  add_error (
	    Error (t->get_locus (),
		   "unexpected token %qs in some sort of const production",
		   t->get_token_description ()));

	  lexer.skip_token (1); // TODO: is this right thing to do?
	  return nullptr;
	}
    case STATIC_TOK:
      return parse_static_item (std::move (vis), std::move (outer_attrs));
    case TRAIT:
      return parse_trait (std::move (vis), std::move (outer_attrs));
    case IMPL:
      return parse_impl (std::move (vis), std::move (outer_attrs));
    case UNSAFE: // unsafe traits, unsafe functions, unsafe impls (trait impls),
      // lookahead to resolve syntactical production
      t = lexer.peek_token (1);

      switch (t->get_id ())
	{
	case TRAIT:
	  return parse_trait (std::move (vis), std::move (outer_attrs));
	case EXTERN_TOK:
	case FN_TOK:
	  return parse_function (std::move (vis), std::move (outer_attrs));
	case IMPL:
	  return parse_impl (std::move (vis), std::move (outer_attrs));
	default:
	  add_error (
	    Error (t->get_locus (),
		   "unexpected token %qs in some sort of unsafe production",
		   t->get_token_description ()));

	  lexer.skip_token (1); // TODO: is this right thing to do?
	  return nullptr;
	}
    default:
      // otherwise vis item clearly doesn't exist, which is not an error
      // has a catch-all post-switch return to allow other breaks to occur
      break;
    }
  return nullptr;
}

// Parses a MacroItem (either a MacroInvocationSemi or MacroRulesDefinition).
template <typename ManagedTokenSource>
std::unique_ptr<AST::MacroItem>
Parser<ManagedTokenSource>::parse_macro_item (AST::AttrVec outer_attrs)
{
  const_TokenPtr t = lexer.peek_token ();

  /* dodgy way of detecting macro due to weird context-dependence thing.
   * probably can be improved */
  // TODO: ensure that string compare works properly
  if (t->get_id () == IDENTIFIER && t->get_str () == "macro_rules")
    {
      return parse_macro_rules_def (std::move (outer_attrs));
    }
  else
    {
      // DEBUG: TODO: remove
      rust_debug (
	"DEBUG - parse_macro_item called and token is not macro_rules");
      if (t->get_id () == IDENTIFIER)
	{
	  rust_debug ("just add to last error: token is not macro_rules and is "
		      "instead '%s'",
		      t->get_str ().c_str ());
	}
      else
	{
	  rust_debug ("just add to last error: token is not macro_rules and is "
		      "not an identifier either - it is '%s'",
		      t->get_token_description ());
	}

      return parse_macro_invocation_semi (std::move (outer_attrs));
    }
}

// Parses a macro rules definition syntax extension whatever thing.
template <typename ManagedTokenSource>
std::unique_ptr<AST::MacroRulesDefinition>
Parser<ManagedTokenSource>::parse_macro_rules_def (AST::AttrVec outer_attrs)
{
  // ensure that first token is identifier saying "macro_rules"
  const_TokenPtr t = lexer.peek_token ();
  if (t->get_id () != IDENTIFIER || t->get_str () != "macro_rules")
    {
      Error error (
	t->get_locus (),
	"macro rules definition does not start with %<macro_rules%>");
      add_error (std::move (error));

      // skip after somewhere?
      return nullptr;
    }
  lexer.skip_token ();
  Location macro_locus = t->get_locus ();

  if (!skip_token (EXCLAM))
    {
      // skip after somewhere?
      return nullptr;
    }

  // parse macro name
  const_TokenPtr ident_tok = expect_token (IDENTIFIER);
  if (ident_tok == nullptr)
    {
      return nullptr;
    }
  Identifier rule_name = ident_tok->get_str ();

  // DEBUG
  rust_debug ("in macro rules def, about to parse parens.");

  // save delim type to ensure it is reused later
  AST::DelimType delim_type = AST::PARENS;

  // Map tokens to DelimType
  t = lexer.peek_token ();
  switch (t->get_id ())
    {
    case LEFT_PAREN:
      delim_type = AST::PARENS;
      break;
    case LEFT_SQUARE:
      delim_type = AST::SQUARE;
      break;
    case LEFT_CURLY:
      delim_type = AST::CURLY;
      break;
    default:
      add_error (Error (t->get_locus (),
			"unexpected token %qs - expecting delimiters (for a "
			"macro rules definition)",
			t->get_token_description ()));

      return nullptr;
    }
  lexer.skip_token ();

  // parse actual macro rules
  std::vector<AST::MacroRule> macro_rules;

  // must be at least one macro rule, so parse it
  AST::MacroRule initial_rule = parse_macro_rule ();
  if (initial_rule.is_error ())
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "required first macro rule in macro rules definition "
		   "could not be parsed");
      add_error (std::move (error));

      // skip after somewhere?
      return nullptr;
    }
  macro_rules.push_back (std::move (initial_rule));

  // DEBUG
  rust_debug ("successfully pushed back initial macro rule");

  t = lexer.peek_token ();
  // parse macro rules
  while (t->get_id () == SEMICOLON)
    {
      // skip semicolon
      lexer.skip_token ();

      // don't parse if end of macro rules
      if (token_id_matches_delims (lexer.peek_token ()->get_id (), delim_type))
	{
	  // DEBUG
	  rust_debug (
	    "broke out of parsing macro rules loop due to finding delim");

	  break;
	}

      // try to parse next rule
      AST::MacroRule rule = parse_macro_rule ();
      if (rule.is_error ())
	{
	  Error error (lexer.peek_token ()->get_locus (),
		       "failed to parse macro rule in macro rules definition");
	  add_error (std::move (error));

	  return nullptr;
	}

      macro_rules.push_back (std::move (rule));

      // DEBUG
      rust_debug ("successfully pushed back another macro rule");

      t = lexer.peek_token ();
    }

  // parse end delimiters
  t = lexer.peek_token ();
  if (token_id_matches_delims (t->get_id (), delim_type))
    {
      // tokens match opening delimiter, so skip.
      lexer.skip_token ();

      if (delim_type != AST::CURLY)
	{
	  // skip semicolon at end of non-curly macro definitions
	  if (!skip_token (SEMICOLON))
	    {
	      // as this is the end, allow recovery (probably) - may change
	      return std::unique_ptr<AST::MacroRulesDefinition> (
		new AST::MacroRulesDefinition (
		  std::move (rule_name), delim_type, std::move (macro_rules),
		  std::move (outer_attrs), macro_locus));
	    }
	}

      return std::unique_ptr<AST::MacroRulesDefinition> (
	new AST::MacroRulesDefinition (std::move (rule_name), delim_type,
				       std::move (macro_rules),
				       std::move (outer_attrs), macro_locus));
    }
  else
    {
      // tokens don't match opening delimiters, so produce error
      Error error (t->get_locus (),
		   "unexpected token %qs - expecting closing delimiter %qs "
		   "(for a macro rules definition)",
		   t->get_token_description (),
		   (delim_type == AST::PARENS
		      ? ")"
		      : (delim_type == AST::SQUARE ? "]" : "}")));
      add_error (std::move (error));

      /* return empty macro definiton despite possibly parsing mostly valid one
       * - TODO is this a good idea? */
      return nullptr;
    }
}

// Parses a semi-coloned (except for full block) macro invocation item.
template <typename ManagedTokenSource>
std::unique_ptr<AST::MacroInvocation>
Parser<ManagedTokenSource>::parse_macro_invocation_semi (
  AST::AttrVec outer_attrs)
{
  Location macro_locus = lexer.peek_token ()->get_locus ();
  AST::SimplePath path = parse_simple_path ();

  if (!skip_token (EXCLAM))
    {
      // skip after somewhere?
      return nullptr;
    }

  // save delim type to ensure it is reused later
  AST::DelimType delim_type = AST::PARENS;

  // Map tokens to DelimType
  const_TokenPtr t = lexer.peek_token ();
  switch (t->get_id ())
    {
    case LEFT_PAREN:
      delim_type = AST::PARENS;
      break;
    case LEFT_SQUARE:
      delim_type = AST::SQUARE;
      break;
    case LEFT_CURLY:
      delim_type = AST::CURLY;
      break;
    default:
      add_error (Error (t->get_locus (),
			"unexpected token %qs - expecting delimiters (for a "
			"macro invocation semi body)",
			t->get_token_description ()));

      return nullptr;
    }
  Location tok_tree_locus = t->get_locus ();
  lexer.skip_token ();

  // parse actual token trees
  std::vector<std::unique_ptr<AST::TokenTree>> token_trees;
  auto delim_open
    = std::unique_ptr<AST::Token> (new AST::Token (std::move (t)));
  token_trees.push_back (std::move (delim_open));

  t = lexer.peek_token ();
  // parse token trees until the initial delimiter token is found again
  while (!token_id_matches_delims (t->get_id (), delim_type))
    {
      std::unique_ptr<AST::TokenTree> tree = parse_token_tree ();

      if (tree == nullptr)
	{
	  Error error (t->get_locus (),
		       "failed to parse token tree for macro invocation semi "
		       "- found %qs",
		       t->get_token_description ());
	  add_error (std::move (error));

	  return nullptr;
	}

      token_trees.push_back (std::move (tree));

      t = lexer.peek_token ();
    }
  auto delim_close
    = std::unique_ptr<AST::Token> (new AST::Token (std::move (t)));
  token_trees.push_back (std::move (delim_close));

  AST::DelimTokenTree delim_tok_tree (delim_type, std::move (token_trees),
				      tok_tree_locus);
  AST::MacroInvocData invoc_data (std::move (path), std::move (delim_tok_tree));

  // parse end delimiters
  t = lexer.peek_token ();
  if (token_id_matches_delims (t->get_id (), delim_type))
    {
      // tokens match opening delimiter, so skip.
      lexer.skip_token ();

      if (delim_type != AST::CURLY)
	{
	  // skip semicolon at end of non-curly macro invocation semis
	  if (!skip_token (SEMICOLON))
	    {
	      // as this is the end, allow recovery (probably) - may change

	      return std::unique_ptr<AST::MacroInvocation> (
		new AST::MacroInvocation (std::move (invoc_data),
					  std::move (outer_attrs), macro_locus,
					  true));
	    }
	}

      // DEBUG:
      rust_debug ("skipped token is '%s', next token (current peek) is '%s'",
		  t->get_token_description (),
		  lexer.peek_token ()->get_token_description ());

      return std::unique_ptr<AST::MacroInvocation> (
	new AST::MacroInvocation (std::move (invoc_data),
				  std::move (outer_attrs), macro_locus, true));
    }
  else
    {
      // tokens don't match opening delimiters, so produce error
      Error error (t->get_locus (),
		   "unexpected token %qs - expecting closing delimiter %qs "
		   "(for a macro invocation semi)",
		   t->get_token_description (),
		   (delim_type == AST::PARENS
		      ? ")"
		      : (delim_type == AST::SQUARE ? "]" : "}")));
      add_error (std::move (error));

      /* return empty macro invocation despite possibly parsing mostly valid one
       * - TODO is this a good idea? */
      return nullptr;
    }
}

// Parses a non-semicoloned macro invocation (i.e. as pattern or expression).
template <typename ManagedTokenSource>
std::unique_ptr<AST::MacroInvocation>
Parser<ManagedTokenSource>::parse_macro_invocation (AST::AttrVec outer_attrs)
{
  // parse macro path
  AST::SimplePath macro_path = parse_simple_path ();
  if (macro_path.is_empty ())
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "failed to parse macro invocation path");
      add_error (std::move (error));

      // skip?
      return nullptr;
    }

  if (!skip_token (EXCLAM))
    {
      // skip after somewhere?
      return nullptr;
    }

  // parse internal delim token tree
  AST::DelimTokenTree delim_tok_tree = parse_delim_token_tree ();

  Location macro_locus = macro_path.get_locus ();

  return std::unique_ptr<AST::MacroInvocation> (
    new AST::MacroInvocation (AST::MacroInvocData (std::move (macro_path),
						   std::move (delim_tok_tree)),
			      std::move (outer_attrs), macro_locus));
}

// Parses a macro rule definition - does not parse semicolons.
template <typename ManagedTokenSource>
AST::MacroRule
Parser<ManagedTokenSource>::parse_macro_rule ()
{
  Location locus = lexer.peek_token ()->get_locus ();

  // parse macro matcher
  AST::MacroMatcher matcher = parse_macro_matcher ();

  if (matcher.is_error ())
    return AST::MacroRule::create_error (locus);

  if (!skip_token (MATCH_ARROW))
    {
      // skip after somewhere?
      return AST::MacroRule::create_error (locus);
    }

  // parse transcriber (this is just a delim token tree)
  Location token_tree_loc = lexer.peek_token ()->get_locus ();
  AST::MacroTranscriber transcriber (parse_delim_token_tree (), token_tree_loc);

  return AST::MacroRule (std::move (matcher), std::move (transcriber), locus);
}

// Parses a macro matcher (part of a macro rule definition).
template <typename ManagedTokenSource>
AST::MacroMatcher
Parser<ManagedTokenSource>::parse_macro_matcher ()
{
  // save delim type to ensure it is reused later
  AST::DelimType delim_type = AST::PARENS;

  // DEBUG
  rust_debug ("begun parsing macro matcher");

  // Map tokens to DelimType
  const_TokenPtr t = lexer.peek_token ();
  Location locus = t->get_locus ();
  switch (t->get_id ())
    {
    case LEFT_PAREN:
      delim_type = AST::PARENS;
      break;
    case LEFT_SQUARE:
      delim_type = AST::SQUARE;
      break;
    case LEFT_CURLY:
      delim_type = AST::CURLY;
      break;
    default:
      add_error (Error (
	t->get_locus (),
	"unexpected token %qs - expecting delimiters (for a macro matcher)",
	t->get_token_description ()));

      return AST::MacroMatcher::create_error (t->get_locus ());
    }
  lexer.skip_token ();

  // parse actual macro matches
  std::vector<std::unique_ptr<AST::MacroMatch>> matches;
  // Set of possible preceding macro matches to make sure follow-set
  // restrictions are respected.
  // TODO: Consider using std::reference_wrapper instead of raw pointers?
  std::vector<const AST::MacroMatch *> last_matches;

  t = lexer.peek_token ();
  // parse token trees until the initial delimiter token is found again
  while (!token_id_matches_delims (t->get_id (), delim_type))
    {
      std::unique_ptr<AST::MacroMatch> match = parse_macro_match ();

      if (match == nullptr)
	{
	  Error error (
	    t->get_locus (),
	    "failed to parse macro match for macro matcher - found %qs",
	    t->get_token_description ());
	  add_error (std::move (error));

	  return AST::MacroMatcher::create_error (t->get_locus ());
	}

      if (matches.size () > 0)
	{
	  const auto *last_match = matches.back ().get ();

	  // We want to check if we are dealing with a zeroable repetition
	  bool zeroable = false;
	  if (last_match->get_macro_match_type ()
	      == AST::MacroMatch::MacroMatchType::Repetition)
	    {
	      auto repetition
		= static_cast<const AST::MacroMatchRepetition *> (last_match);

	      if (repetition->get_op ()
		  != AST::MacroMatchRepetition::MacroRepOp::ONE_OR_MORE)
		zeroable = true;
	    }

	  if (!zeroable)
	    last_matches.clear ();

	  last_matches.emplace_back (last_match);

	  for (auto last : last_matches)
	    if (!is_match_compatible (*last, *match))
	      return AST::MacroMatcher::create_error (
		match->get_match_locus ());
	}

      matches.push_back (std::move (match));

      // DEBUG
      rust_debug ("pushed back a match in macro matcher");

      t = lexer.peek_token ();
    }

  // parse end delimiters
  t = lexer.peek_token ();
  if (token_id_matches_delims (t->get_id (), delim_type))
    {
      // tokens match opening delimiter, so skip.
      lexer.skip_token ();

      return AST::MacroMatcher (delim_type, std::move (matches), locus);
    }
  else
    {
      // tokens don't match opening delimiters, so produce error
      Error error (t->get_locus (),
		   "unexpected token %qs - expecting closing delimiter %qs "
		   "(for a macro matcher)",
		   t->get_token_description (),
		   (delim_type == AST::PARENS
		      ? ")"
		      : (delim_type == AST::SQUARE ? "]" : "}")));
      add_error (std::move (error));

      /* return error macro matcher despite possibly parsing mostly correct one?
       * TODO is this the best idea? */
      return AST::MacroMatcher::create_error (t->get_locus ());
    }
}

// Parses a macro match (syntax match inside a matcher in a macro rule).
template <typename ManagedTokenSource>
std::unique_ptr<AST::MacroMatch>
Parser<ManagedTokenSource>::parse_macro_match ()
{
  // branch based on token available
  const_TokenPtr t = lexer.peek_token ();
  switch (t->get_id ())
    {
    case LEFT_PAREN:
    case LEFT_SQUARE:
      case LEFT_CURLY: {
	// must be macro matcher as delimited
	AST::MacroMatcher matcher = parse_macro_matcher ();
	if (matcher.is_error ())
	  {
	    Error error (lexer.peek_token ()->get_locus (),
			 "failed to parse macro matcher in macro match");
	    add_error (std::move (error));

	    return nullptr;
	  }
	return std::unique_ptr<AST::MacroMatcher> (
	  new AST::MacroMatcher (std::move (matcher)));
      }
      case DOLLAR_SIGN: {
	// have to do more lookahead to determine if fragment or repetition
	const_TokenPtr t2 = lexer.peek_token (1);
	switch (t2->get_id ())
	  {
	  case ABSTRACT:
	  case AS:
	  case ASYNC:
	  case BECOME:
	  case BOX:
	  case BREAK:
	  case CONST:
	  case CONTINUE:
	  case CRATE:
	  case DO:
	  case DYN:
	  case ELSE:
	  case ENUM_TOK:
	  case EXTERN_TOK:
	  case FALSE_LITERAL:
	  case FINAL_TOK:
	  case FN_TOK:
	  case FOR:
	  case IF:
	  case IMPL:
	  case IN:
	  case LET:
	  case LOOP:
	  case MACRO:
	  case MATCH_TOK:
	  case MOD:
	  case MOVE:
	  case MUT:
	  case OVERRIDE_TOK:
	  case PRIV:
	  case PUB:
	  case REF:
	  case RETURN_TOK:
	  case SELF_ALIAS:
	  case SELF:
	  case STATIC_TOK:
	  case STRUCT_TOK:
	  case SUPER:
	  case TRAIT:
	  case TRUE_LITERAL:
	  case TRY:
	  case TYPE:
	  case TYPEOF:
	  case UNSAFE:
	  case UNSIZED:
	  case USE:
	  case VIRTUAL:
	  case WHERE:
	  case WHILE:
	  case YIELD:
	  case IDENTIFIER:
	    // macro fragment
	    return parse_macro_match_fragment ();
	  case LEFT_PAREN:
	    // macro repetition
	    return parse_macro_match_repetition ();
	  default:
	    // error: unrecognised
	    add_error (
	      Error (t2->get_locus (),
		     "unrecognised token combination %<$%s%> at start of "
		     "macro match - did you mean %<$identifier%> or %<$(%>?",
		     t2->get_token_description ()));

	    // skip somewhere?
	    return nullptr;
	  }
      }
    case RIGHT_PAREN:
    case RIGHT_SQUARE:
    case RIGHT_CURLY:
      // not allowed
      add_error (Error (
	t->get_locus (),
	"closing delimiters like %qs are not allowed at the start of a macro "
	"match",
	t->get_token_description ()));

      // skip somewhere?
      return nullptr;
    default:
      // just the token
      lexer.skip_token ();
      return std::unique_ptr<AST::Token> (new AST::Token (std::move (t)));
    }
}

// Parses a fragment macro match.
template <typename ManagedTokenSource>
std::unique_ptr<AST::MacroMatchFragment>
Parser<ManagedTokenSource>::parse_macro_match_fragment ()
{
  Location fragment_locus = lexer.peek_token ()->get_locus ();
  skip_token (DOLLAR_SIGN);

  Identifier ident = "";
  auto identifier = lexer.peek_token ();
  if (identifier->has_str ())
    ident = identifier->get_str ();
  else
    ident = std::string (token_id_to_str (identifier->get_id ()));

  if (ident.empty ())
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "missing identifier in macro match fragment");
      add_error (std::move (error));

      return nullptr;
    }
  skip_token (identifier->get_id ());

  if (!skip_token (COLON))
    {
      // skip after somewhere?
      return nullptr;
    }

  // get MacroFragSpec for macro
  const_TokenPtr t = expect_token (IDENTIFIER);
  if (t == nullptr)
    return nullptr;

  AST::MacroFragSpec frag
    = AST::MacroFragSpec::get_frag_spec_from_str (t->get_str ());
  if (frag.is_error ())
    {
      Error error (t->get_locus (),
		   "invalid fragment specifier %qs in fragment macro match",
		   t->get_str ().c_str ());
      add_error (std::move (error));

      return nullptr;
    }

  return std::unique_ptr<AST::MacroMatchFragment> (
    new AST::MacroMatchFragment (std::move (ident), frag, fragment_locus));
}

// Parses a repetition macro match.
template <typename ManagedTokenSource>
std::unique_ptr<AST::MacroMatchRepetition>
Parser<ManagedTokenSource>::parse_macro_match_repetition ()
{
  skip_token (DOLLAR_SIGN);
  skip_token (LEFT_PAREN);

  std::vector<std::unique_ptr<AST::MacroMatch>> matches;

  // parse required first macro match
  std::unique_ptr<AST::MacroMatch> initial_match = parse_macro_match ();
  if (initial_match == nullptr)
    {
      Error error (
	lexer.peek_token ()->get_locus (),
	"could not parse required first macro match in macro match repetition");
      add_error (std::move (error));

      // skip after somewhere?
      return nullptr;
    }
  matches.push_back (std::move (initial_match));

  // parse optional later macro matches
  const_TokenPtr t = lexer.peek_token ();
  while (t->get_id () != RIGHT_PAREN)
    {
      std::unique_ptr<AST::MacroMatch> match = parse_macro_match ();

      if (match == nullptr)
	{
	  Error error (lexer.peek_token ()->get_locus (),
		       "failed to parse macro match in macro match repetition");
	  add_error (std::move (error));

	  return nullptr;
	}

      matches.push_back (std::move (match));

      t = lexer.peek_token ();
    }

  if (!skip_token (RIGHT_PAREN))
    {
      // skip after somewhere?
      return nullptr;
    }

  t = lexer.peek_token ();
  // see if separator token exists
  std::unique_ptr<AST::Token> separator = nullptr;
  switch (t->get_id ())
    {
    // repetition operators
    case ASTERISK:
    case PLUS:
    case QUESTION_MARK:
    // delimiters
    case LEFT_PAREN:
    case LEFT_CURLY:
    case LEFT_SQUARE:
    case RIGHT_PAREN:
    case RIGHT_CURLY:
    case RIGHT_SQUARE:
      // separator does not exist, so still null and don't skip token
      break;
    default:
      // separator does exist
      separator = std::unique_ptr<AST::Token> (new AST::Token (std::move (t)));
      lexer.skip_token ();
      break;
    }

  // parse repetition operator
  t = lexer.peek_token ();
  AST::MacroMatchRepetition::MacroRepOp op = AST::MacroMatchRepetition::NONE;
  switch (t->get_id ())
    {
    case ASTERISK:
      op = AST::MacroMatchRepetition::ANY;
      lexer.skip_token ();
      break;
    case PLUS:
      op = AST::MacroMatchRepetition::ONE_OR_MORE;
      lexer.skip_token ();
      break;
    case QUESTION_MARK:
      op = AST::MacroMatchRepetition::ZERO_OR_ONE;
      lexer.skip_token ();
      break;
    default:
      add_error (
	Error (t->get_locus (),
	       "expected macro repetition operator (%<*%>, %<+%>, or %<?%>) in "
	       "macro match - found %qs",
	       t->get_token_description ()));

      // skip after somewhere?
      return nullptr;
    }

  return std::unique_ptr<AST::MacroMatchRepetition> (
    new AST::MacroMatchRepetition (std::move (matches), op,
				   std::move (separator), t->get_locus ()));
}

/* Parses a visibility syntactical production (i.e. creating a non-default
 * visibility) */
template <typename ManagedTokenSource>
AST::Visibility
Parser<ManagedTokenSource>::parse_visibility ()
{
  // check for no visibility
  if (lexer.peek_token ()->get_id () != PUB)
    {
      return AST::Visibility::create_private ();
    }

  lexer.skip_token ();

  // create simple pub visibility if no parentheses
  if (lexer.peek_token ()->get_id () != LEFT_PAREN)
    {
      return AST::Visibility::create_public ();
      // or whatever
    }

  lexer.skip_token ();

  const_TokenPtr t = lexer.peek_token ();
  auto path_loc = t->get_locus ();

  switch (t->get_id ())
    {
    case CRATE:
      lexer.skip_token ();

      skip_token (RIGHT_PAREN);

      return AST::Visibility::create_crate (path_loc);
    case SELF:
      lexer.skip_token ();

      skip_token (RIGHT_PAREN);

      return AST::Visibility::create_self (path_loc);
    case SUPER:
      lexer.skip_token ();

      skip_token (RIGHT_PAREN);

      return AST::Visibility::create_super (path_loc);
      case IN: {
	lexer.skip_token ();

	// parse the "in" path as well
	AST::SimplePath path = parse_simple_path ();
	if (path.is_empty ())
	  {
	    Error error (lexer.peek_token ()->get_locus (),
			 "missing path in pub(in path) visibility");
	    add_error (std::move (error));

	    // skip after somewhere?
	    return AST::Visibility::create_error ();
	  }

	skip_token (RIGHT_PAREN);

	return AST::Visibility::create_in_path (std::move (path));
      }
    default:
      add_error (Error (t->get_locus (), "unexpected token %qs in visibility",
			t->get_token_description ()));

      lexer.skip_token ();
      return AST::Visibility::create_error ();
    }
}

// Parses a module - either a bodied module or a module defined in another file.
template <typename ManagedTokenSource>
std::unique_ptr<AST::Module>
Parser<ManagedTokenSource>::parse_module (AST::Visibility vis,
					  AST::AttrVec outer_attrs)
{
  Location locus = lexer.peek_token ()->get_locus ();
  skip_token (MOD);

  const_TokenPtr module_name = expect_token (IDENTIFIER);
  if (module_name == nullptr)
    {
      return nullptr;
    }
  Identifier name = module_name->get_str ();

  const_TokenPtr t = lexer.peek_token ();

  switch (t->get_id ())
    {
    case SEMICOLON:
      lexer.skip_token ();

      // Construct an external module
      return std::unique_ptr<AST::Module> (
	new AST::Module (std::move (name), std::move (vis),
			 std::move (outer_attrs), locus, lexer.get_filename (),
			 inline_module_stack));
      case LEFT_CURLY: {
	lexer.skip_token ();

	// parse inner attributes
	AST::AttrVec inner_attrs = parse_inner_attributes ();

	std::string module_path_name
	  = extract_module_path (inner_attrs, outer_attrs, name);
	InlineModuleStackScope scope (*this, std::move (module_path_name));

	// parse items
	std::vector<std::unique_ptr<AST::Item>> items;
	const_TokenPtr tok = lexer.peek_token ();
	while (tok->get_id () != RIGHT_CURLY)
	  {
	    std::unique_ptr<AST::Item> item = parse_item (false);
	    if (item == nullptr)
	      {
		Error error (tok->get_locus (),
			     "failed to parse item in module");
		add_error (std::move (error));

		return nullptr;
	      }

	    items.push_back (std::move (item));

	    tok = lexer.peek_token ();
	  }

	if (!skip_token (RIGHT_CURLY))
	  {
	    // skip somewhere?
	    return nullptr;
	  }

	return std::unique_ptr<AST::Module> (
	  new AST::Module (std::move (name), locus, std::move (items),
			   std::move (vis), std::move (inner_attrs),
			   std::move (outer_attrs))); // module name?
      }
    default:
      add_error (
	Error (t->get_locus (),
	       "unexpected token %qs in module declaration/definition item",
	       t->get_token_description ()));

      lexer.skip_token ();
      return nullptr;
    }
}

// Parses an extern crate declaration (dependency on external crate)
template <typename ManagedTokenSource>
std::unique_ptr<AST::ExternCrate>
Parser<ManagedTokenSource>::parse_extern_crate (AST::Visibility vis,
						AST::AttrVec outer_attrs)
{
  Location locus = lexer.peek_token ()->get_locus ();
  if (!skip_token (EXTERN_TOK))
    {
      skip_after_semicolon ();
      return nullptr;
    }

  if (!skip_token (CRATE))
    {
      skip_after_semicolon ();
      return nullptr;
    }

  /* parse crate reference name - this has its own syntactical rule in reference
   * but seems to not be used elsewhere, so i'm putting it here */
  const_TokenPtr crate_name_tok = lexer.peek_token ();
  std::string crate_name;

  switch (crate_name_tok->get_id ())
    {
    case IDENTIFIER:
      crate_name = crate_name_tok->get_str ();
      lexer.skip_token ();
      break;
    case SELF:
      crate_name = "self";
      lexer.skip_token ();
      break;
    default:
      add_error (
	Error (crate_name_tok->get_locus (),
	       "expecting crate name (identifier or %<self%>), found %qs",
	       crate_name_tok->get_token_description ()));

      skip_after_semicolon ();
      return nullptr;
    }

  // don't parse as clause if it doesn't exist
  if (lexer.peek_token ()->get_id () == SEMICOLON)
    {
      lexer.skip_token ();

      return std::unique_ptr<AST::ExternCrate> (
	new AST::ExternCrate (std::move (crate_name), std::move (vis),
			      std::move (outer_attrs), locus));
    }

  /* parse as clause - this also has its own syntactical rule in reference and
   * also seems to not be used elsewhere, so including here again. */
  if (!skip_token (AS))
    {
      skip_after_semicolon ();
      return nullptr;
    }

  const_TokenPtr as_name_tok = lexer.peek_token ();
  std::string as_name;

  switch (as_name_tok->get_id ())
    {
    case IDENTIFIER:
      as_name = as_name_tok->get_str ();
      lexer.skip_token ();
      break;
    case UNDERSCORE:
      as_name = "_";
      lexer.skip_token ();
      break;
    default:
      add_error (
	Error (as_name_tok->get_locus (),
	       "expecting as clause name (identifier or %<_%>), found %qs",
	       as_name_tok->get_token_description ()));

      skip_after_semicolon ();
      return nullptr;
    }

  if (!skip_token (SEMICOLON))
    {
      skip_after_semicolon ();
      return nullptr;
    }

  return std::unique_ptr<AST::ExternCrate> (
    new AST::ExternCrate (std::move (crate_name), std::move (vis),
			  std::move (outer_attrs), locus, std::move (as_name)));
}

// Parses a use declaration.
template <typename ManagedTokenSource>
std::unique_ptr<AST::UseDeclaration>
Parser<ManagedTokenSource>::parse_use_decl (AST::Visibility vis,
					    AST::AttrVec outer_attrs)
{
  Location locus = lexer.peek_token ()->get_locus ();
  if (!skip_token (USE))
    {
      skip_after_semicolon ();
      return nullptr;
    }

  // parse use tree, which is required
  std::unique_ptr<AST::UseTree> use_tree = parse_use_tree ();
  if (use_tree == nullptr)
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "could not parse use tree in use declaration");
      add_error (std::move (error));

      skip_after_semicolon ();
      return nullptr;
    }

  if (!skip_token (SEMICOLON))
    {
      skip_after_semicolon ();
      return nullptr;
    }

  return std::unique_ptr<AST::UseDeclaration> (
    new AST::UseDeclaration (std::move (use_tree), std::move (vis),
			     std::move (outer_attrs), locus));
}

// Parses a use tree (which can be recursive and is actually a base class).
template <typename ManagedTokenSource>
std::unique_ptr<AST::UseTree>
Parser<ManagedTokenSource>::parse_use_tree ()
{
  /* potential syntax definitions in attempt to get algorithm:
   *  Glob:
   *      <- SimplePath :: *
   *      <- :: *
   *      <- *
   *  Nested tree thing:
   *      <- SimplePath :: { COMPLICATED_INNER_TREE_THING }
   *      <- :: COMPLICATED_INNER_TREE_THING }
   *      <- { COMPLICATED_INNER_TREE_THING }
   *  Rebind thing:
   *      <- SimplePath as IDENTIFIER
   *      <- SimplePath as _
   *      <- SimplePath
   */

  /* current plan of attack: try to parse SimplePath first - if fails, one of
   * top two then try parse :: - if fails, one of top two. Next is deciding
   * character for top two. */

  /* Thus, parsing smaller parts of use tree may require feeding into function
   * via parameters (or could handle all in this single function because other
   * use tree types aren't recognised as separate in the spec) */

  // TODO: I think this function is too complex, probably should split it

  Location locus = lexer.peek_token ()->get_locus ();

  // bool has_path = false;
  AST::SimplePath path = parse_simple_path ();

  if (path.is_empty ())
    {
      // has no path, so must be glob or nested tree UseTree type

      bool is_global = false;

      // check for global scope resolution operator
      if (lexer.peek_token ()->get_id () == SCOPE_RESOLUTION)
	{
	  lexer.skip_token ();
	  is_global = true;
	}

      const_TokenPtr t = lexer.peek_token ();
      switch (t->get_id ())
	{
	case ASTERISK:
	  // glob UseTree type
	  lexer.skip_token ();

	  if (is_global)
	    return std::unique_ptr<AST::UseTreeGlob> (
	      new AST::UseTreeGlob (AST::UseTreeGlob::GLOBAL,
				    AST::SimplePath::create_empty (), locus));
	  else
	    return std::unique_ptr<AST::UseTreeGlob> (
	      new AST::UseTreeGlob (AST::UseTreeGlob::NO_PATH,
				    AST::SimplePath::create_empty (), locus));
	  case LEFT_CURLY: {
	    // nested tree UseTree type
	    lexer.skip_token ();

	    std::vector<std::unique_ptr<AST::UseTree>> use_trees;

	    const_TokenPtr t = lexer.peek_token ();
	    while (t->get_id () != RIGHT_CURLY)
	      {
		std::unique_ptr<AST::UseTree> use_tree = parse_use_tree ();
		if (use_tree == nullptr)
		  {
		    break;
		  }

		use_trees.push_back (std::move (use_tree));

		if (lexer.peek_token ()->get_id () != COMMA)
		  break;

		lexer.skip_token ();
		t = lexer.peek_token ();
	      }

	    // skip end curly delimiter
	    if (!skip_token (RIGHT_CURLY))
	      {
		// skip after somewhere?
		return nullptr;
	      }

	    if (is_global)
	      return std::unique_ptr<AST::UseTreeList> (
		new AST::UseTreeList (AST::UseTreeList::GLOBAL,
				      AST::SimplePath::create_empty (),
				      std::move (use_trees), locus));
	    else
	      return std::unique_ptr<AST::UseTreeList> (
		new AST::UseTreeList (AST::UseTreeList::NO_PATH,
				      AST::SimplePath::create_empty (),
				      std::move (use_trees), locus));
	  }
	case AS:
	  // this is not allowed
	  add_error (Error (
	    t->get_locus (),
	    "use declaration with rebind %<as%> requires a valid simple path - "
	    "none found"));

	  skip_after_semicolon ();
	  return nullptr;
	default:
	  add_error (Error (t->get_locus (),
			    "unexpected token %qs in use tree with "
			    "no valid simple path (i.e. list"
			    " or glob use tree)",
			    t->get_token_description ()));

	  skip_after_semicolon ();
	  return nullptr;
	}
    }
  else
    {
      /* Due to aforementioned implementation issues, the trailing :: token is
       * consumed by the path, so it can not be used as a disambiguator.
       * NOPE, not true anymore - TODO what are the consequences of this? */

      const_TokenPtr t = lexer.peek_token ();
      switch (t->get_id ())
	{
	case ASTERISK:
	  // glob UseTree type
	  lexer.skip_token ();

	  return std::unique_ptr<AST::UseTreeGlob> (
	    new AST::UseTreeGlob (AST::UseTreeGlob::PATH_PREFIXED,
				  std::move (path), locus));
	  case LEFT_CURLY: {
	    // nested tree UseTree type
	    lexer.skip_token ();

	    std::vector<std::unique_ptr<AST::UseTree>> use_trees;

	    // TODO: think of better control structure
	    const_TokenPtr t = lexer.peek_token ();
	    while (t->get_id () != RIGHT_CURLY)
	      {
		std::unique_ptr<AST::UseTree> use_tree = parse_use_tree ();
		if (use_tree == nullptr)
		  {
		    break;
		  }

		use_trees.push_back (std::move (use_tree));

		if (lexer.peek_token ()->get_id () != COMMA)
		  break;

		lexer.skip_token ();
		t = lexer.peek_token ();
	      }

	    // skip end curly delimiter
	    if (!skip_token (RIGHT_CURLY))
	      {
		// skip after somewhere?
		return nullptr;
	      }

	    return std::unique_ptr<AST::UseTreeList> (
	      new AST::UseTreeList (AST::UseTreeList::PATH_PREFIXED,
				    std::move (path), std::move (use_trees),
				    locus));
	  }
	  case AS: {
	    // rebind UseTree type
	    lexer.skip_token ();

	    const_TokenPtr t = lexer.peek_token ();
	    switch (t->get_id ())
	      {
	      case IDENTIFIER:
		// skip lexer token
		lexer.skip_token ();

		return std::unique_ptr<AST::UseTreeRebind> (
		  new AST::UseTreeRebind (AST::UseTreeRebind::IDENTIFIER,
					  std::move (path), locus,
					  t->get_str ()));
	      case UNDERSCORE:
		// skip lexer token
		lexer.skip_token ();

		return std::unique_ptr<AST::UseTreeRebind> (
		  new AST::UseTreeRebind (AST::UseTreeRebind::WILDCARD,
					  std::move (path), locus, "_"));
	      default:
		add_error (Error (
		  t->get_locus (),
		  "unexpected token %qs in use tree with as clause - expected "
		  "identifier or %<_%>",
		  t->get_token_description ()));

		skip_after_semicolon ();
		return nullptr;
	      }
	  }
	case SEMICOLON:
	  // rebind UseTree type without rebinding - path only

	  // don't skip semicolon - handled in parse_use_tree
	  // lexer.skip_token();

	  return std::unique_ptr<AST::UseTreeRebind> (
	    new AST::UseTreeRebind (AST::UseTreeRebind::NONE, std::move (path),
				    locus));
	case COMMA:
	case RIGHT_CURLY:
	  // this may occur in recursive calls - assume it is ok and ignore it
	  return std::unique_ptr<AST::UseTreeRebind> (
	    new AST::UseTreeRebind (AST::UseTreeRebind::NONE, std::move (path),
				    locus));
	default:
	  add_error (Error (t->get_locus (),
			    "unexpected token %qs in use tree with valid path",
			    t->get_token_description ()));

	  // skip_after_semicolon();
	  return nullptr;
	}
    }
}

// Parses a function (not a method).
template <typename ManagedTokenSource>
std::unique_ptr<AST::Function>
Parser<ManagedTokenSource>::parse_function (AST::Visibility vis,
					    AST::AttrVec outer_attrs)
{
  Location locus = lexer.peek_token ()->get_locus ();
  // Get qualifiers for function if they exist
  AST::FunctionQualifiers qualifiers = parse_function_qualifiers ();

  skip_token (FN_TOK);

  // Save function name token
  const_TokenPtr function_name_tok = expect_token (IDENTIFIER);
  if (function_name_tok == nullptr)
    {
      skip_after_next_block ();
      return nullptr;
    }
  Identifier function_name = function_name_tok->get_str ();

  // parse generic params - if exist
  std::vector<std::unique_ptr<AST::GenericParam>> generic_params
    = parse_generic_params_in_angles ();

  if (!skip_token (LEFT_PAREN))
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "function declaration missing opening parentheses before "
		   "parameter list");
      add_error (std::move (error));

      skip_after_next_block ();
      return nullptr;
    }

  // parse function parameters (only if next token isn't right paren)
  std::vector<AST::FunctionParam> function_params;
  if (lexer.peek_token ()->get_id () != RIGHT_PAREN)
    function_params
      = parse_function_params ([] (TokenId id) { return id == RIGHT_PAREN; });

  if (!skip_token (RIGHT_PAREN))
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "function declaration missing closing parentheses after "
		   "parameter list");
      add_error (std::move (error));

      skip_after_next_block ();
      return nullptr;
    }

  // parse function return type - if exists
  std::unique_ptr<AST::Type> return_type = parse_function_return_type ();

  // parse where clause - if exists
  AST::WhereClause where_clause = parse_where_clause ();

  // parse block expression
  std::unique_ptr<AST::BlockExpr> block_expr = parse_block_expr ();

  return std::unique_ptr<AST::Function> (
    new AST::Function (std::move (function_name), std::move (qualifiers),
		       std::move (generic_params), std::move (function_params),
		       std::move (return_type), std::move (where_clause),
		       std::move (block_expr), std::move (vis),
		       std::move (outer_attrs), locus));
}

// Parses function or method qualifiers (i.e. const, unsafe, and extern).
template <typename ManagedTokenSource>
AST::FunctionQualifiers
Parser<ManagedTokenSource>::parse_function_qualifiers ()
{
  AsyncConstStatus const_status = NONE;
  bool has_unsafe = false;
  bool has_extern = false;
  std::string abi;

  // Check in order of const, unsafe, then extern
  const_TokenPtr t = lexer.peek_token ();
  Location locus = t->get_locus ();
  switch (t->get_id ())
    {
    case CONST:
      lexer.skip_token ();
      const_status = CONST_FN;
      break;
    case ASYNC:
      lexer.skip_token ();
      const_status = ASYNC_FN;
      break;
    default:
      // const status is still none
      break;
    }

  if (lexer.peek_token ()->get_id () == UNSAFE)
    {
      lexer.skip_token ();
      has_unsafe = true;
    }

  if (lexer.peek_token ()->get_id () == EXTERN_TOK)
    {
      lexer.skip_token ();
      has_extern = true;

      // detect optional abi name
      const_TokenPtr next_tok = lexer.peek_token ();
      if (next_tok->get_id () == STRING_LITERAL)
	{
	  lexer.skip_token ();
	  abi = next_tok->get_str ();
	}
    }

  return AST::FunctionQualifiers (locus, const_status, has_unsafe, has_extern,
				  std::move (abi));
}

// Parses generic (lifetime or type) params inside angle brackets (optional).
template <typename ManagedTokenSource>
std::vector<std::unique_ptr<AST::GenericParam>>
Parser<ManagedTokenSource>::parse_generic_params_in_angles ()
{
  if (lexer.peek_token ()->get_id () != LEFT_ANGLE)
    {
      // seems to be no generic params, so exit with empty vector
      return std::vector<std::unique_ptr<AST::GenericParam>> ();
    }
  lexer.skip_token ();

  // DEBUG:
  rust_debug ("skipped left angle in generic param");

  std::vector<std::unique_ptr<AST::GenericParam>> generic_params
    = parse_generic_params (is_right_angle_tok);

  // DEBUG:
  rust_debug ("finished parsing actual generic params (i.e. inside angles)");

  if (!skip_generics_right_angle ())
    {
      // DEBUG
      rust_debug ("failed to skip generics right angle - returning empty "
		  "generic params");

      return std::vector<std::unique_ptr<AST::GenericParam>> ();
    }

  return generic_params;
}

template <typename ManagedTokenSource>
template <typename EndTokenPred>
std::unique_ptr<AST::GenericParam>
Parser<ManagedTokenSource>::parse_generic_param (EndTokenPred is_end_token)
{
  auto token = lexer.peek_token ();
  auto outer_attrs = parse_outer_attribute ();
  std::unique_ptr<AST::GenericParam> param;

  switch (token->get_id ())
    {
      case LIFETIME: {
	auto lifetime = parse_lifetime ();
	if (lifetime.is_error ())
	  {
	    rust_error_at (
	      token->get_locus (),
	      "failed to parse lifetime in generic parameter list");
	    return nullptr;
	  }

	std::vector<AST::Lifetime> lifetime_bounds;
	if (lexer.peek_token ()->get_id () == COLON)
	  {
	    lexer.skip_token ();
	    // parse required bounds
	    lifetime_bounds
	      = parse_lifetime_bounds ([is_end_token] (TokenId id) {
		  return is_end_token (id) || id == COMMA;
		});
	  }

	param = std::unique_ptr<AST::LifetimeParam> (new AST::LifetimeParam (
	  std::move (lifetime), std::move (lifetime_bounds),
	  std::move (outer_attrs), token->get_locus ()));
	break;
      }
      case IDENTIFIER: {
	auto type_ident = token->get_str ();
	lexer.skip_token ();

	std::vector<std::unique_ptr<AST::TypeParamBound>> type_param_bounds;
	if (lexer.peek_token ()->get_id () == COLON)
	  {
	    lexer.skip_token ();

	    // parse optional type param bounds
	    type_param_bounds = parse_type_param_bounds ();
	  }

	std::unique_ptr<AST::Type> type = nullptr;
	if (lexer.peek_token ()->get_id () == EQUAL)
	  {
	    lexer.skip_token ();

	    // parse required type
	    type = parse_type ();
	    if (!type)
	      {
		rust_error_at (
		  lexer.peek_token ()->get_locus (),
		  "failed to parse type in type param in generic params");
		return nullptr;
	      }
	  }

	param = std::unique_ptr<AST::TypeParam> (
	  new AST::TypeParam (std::move (type_ident), token->get_locus (),
			      std::move (type_param_bounds), std::move (type),
			      std::move (outer_attrs)));
	break;
      }
      case CONST: {
	lexer.skip_token ();

	auto name_token = expect_token (IDENTIFIER);

	if (!name_token || !expect_token (COLON))
	  return nullptr;

	auto type = parse_type ();
	if (!type)
	  return nullptr;

	// optional default value
	auto default_expr = AST::GenericArg::create_error ();
	if (lexer.peek_token ()->get_id () == EQUAL)
	  {
	    lexer.skip_token ();
	    auto tok = lexer.peek_token ();
	    default_expr = parse_generic_arg ();

	    if (default_expr.is_error ())
	      rust_error_at (tok->get_locus (),
			     "invalid token for start of default value for "
			     "const generic parameter: expected %<block%>, "
			     "%<identifier%> or %<literal%>, got %qs",
			     token_id_to_str (tok->get_id ()));

	    // At this point, we *know* that we are parsing a const
	    // expression
	    if (default_expr.get_kind () == AST::GenericArg::Kind::Either)
	      default_expr = default_expr.disambiguate_to_const ();
	  }

	param = std::unique_ptr<AST::ConstGenericParam> (
	  new AST::ConstGenericParam (name_token->get_str (), std::move (type),
				      default_expr, std::move (outer_attrs),
				      token->get_locus ()));

	break;
      }
    default:
      // FIXME: Can we clean this last call with a method call?
      rust_error_at (token->get_locus (),
		     "unexpected token when parsing generic parameters: %qs",
		     token->get_str ().c_str ());
      return nullptr;
    }

  return param;
}

/* Parse generic (lifetime or type) params NOT INSIDE ANGLE BRACKETS!!! Almost
 * always parse_generic_params_in_angles is what is wanted. */
template <typename ManagedTokenSource>
template <typename EndTokenPred>
std::vector<std::unique_ptr<AST::GenericParam>>
Parser<ManagedTokenSource>::parse_generic_params (EndTokenPred is_end_token)
{
  std::vector<std::unique_ptr<AST::GenericParam>> generic_params;

  /* can't parse lifetime and type params separately due to lookahead issues
   * thus, parse them all here */

  /* HACK: used to retain attribute data if a lifetime param is tentatively
   * parsed but it turns out to be type param */
  AST::Attribute parsed_outer_attr = AST::Attribute::create_empty ();

  // Did we parse a generic type param yet
  auto type_seen = false;
  // Did the user write a lifetime parameter after a type one
  auto order_error = false;

  // parse lifetime params
  while (!is_end_token (lexer.peek_token ()->get_id ()))
    {
      auto param = parse_generic_param (is_end_token);
      if (param)
	{
	  // TODO: Handle `Const` here as well if necessary
	  if (param->get_kind () == AST::GenericParam::Kind::Type)
	    type_seen = true;
	  else if (param->get_kind () == AST::GenericParam::Kind::Lifetime
		   && type_seen)
	    order_error = true;

	  generic_params.emplace_back (std::move (param));
	  maybe_skip_token (COMMA);
	}
    }

  // FIXME: Add reordering hint
  if (order_error)
    rust_error_at (generic_params.front ()->get_locus (),
		   "invalid order for generic parameters: lifetimes should "
		   "always come before types");

  generic_params.shrink_to_fit ();
  return generic_params;
}

/* Parses lifetime generic parameters (pointers). Will also consume any
 * trailing comma. No extra checks for end token. */
template <typename ManagedTokenSource>
std::vector<std::unique_ptr<AST::LifetimeParam>>
Parser<ManagedTokenSource>::parse_lifetime_params ()
{
  std::vector<std::unique_ptr<AST::LifetimeParam>> lifetime_params;

  while (lexer.peek_token ()->get_id () != END_OF_FILE)
    {
      AST::LifetimeParam lifetime_param = parse_lifetime_param ();

      if (lifetime_param.is_error ())
	{
	  // can't treat as error as only way to get out with trailing comma
	  break;
	}

      lifetime_params.push_back (std::unique_ptr<AST::LifetimeParam> (
	new AST::LifetimeParam (std::move (lifetime_param))));

      if (lexer.peek_token ()->get_id () != COMMA)
	break;

      // skip commas, including trailing commas
      lexer.skip_token ();
    }

  lifetime_params.shrink_to_fit ();

  return lifetime_params;
}

/* Parses lifetime generic parameters (pointers). Will also consume any
 * trailing comma. Has extra is_end_token predicate checking. */
template <typename ManagedTokenSource>
template <typename EndTokenPred>
std::vector<std::unique_ptr<AST::LifetimeParam>>
Parser<ManagedTokenSource>::parse_lifetime_params (EndTokenPred is_end_token)
{
  std::vector<std::unique_ptr<AST::LifetimeParam>> lifetime_params;

  // if end_token is not specified, it defaults to EOF, so should work fine
  while (!is_end_token (lexer.peek_token ()->get_id ()))
    {
      AST::LifetimeParam lifetime_param = parse_lifetime_param ();

      if (lifetime_param.is_error ())
	{
	  /* TODO: is it worth throwing away all lifetime params just because
	   * one failed? */
	  Error error (lexer.peek_token ()->get_locus (),
		       "failed to parse lifetime param in lifetime params");
	  add_error (std::move (error));

	  return {};
	}

      lifetime_params.push_back (std::unique_ptr<AST::LifetimeParam> (
	new AST::LifetimeParam (std::move (lifetime_param))));

      if (lexer.peek_token ()->get_id () != COMMA)
	break;

      // skip commas, including trailing commas
      lexer.skip_token ();
    }

  lifetime_params.shrink_to_fit ();

  return lifetime_params;
}

/* Parses lifetime generic parameters (objects). Will also consume any
 * trailing comma. No extra checks for end token.
 * TODO: is this best solution? implements most of the same algorithm. */
template <typename ManagedTokenSource>
std::vector<AST::LifetimeParam>
Parser<ManagedTokenSource>::parse_lifetime_params_objs ()
{
  std::vector<AST::LifetimeParam> lifetime_params;

  // bad control structure as end token cannot be guaranteed
  while (true)
    {
      AST::LifetimeParam lifetime_param = parse_lifetime_param ();

      if (lifetime_param.is_error ())
	{
	  // not an error as only way to exit if trailing comma
	  break;
	}

      lifetime_params.push_back (std::move (lifetime_param));

      if (lexer.peek_token ()->get_id () != COMMA)
	break;

      // skip commas, including trailing commas
      lexer.skip_token ();
    }

  lifetime_params.shrink_to_fit ();

  return lifetime_params;
}

/* Parses lifetime generic parameters (objects). Will also consume any
 * trailing comma. Has extra is_end_token predicate checking.
 * TODO: is this best solution? implements most of the same algorithm. */
template <typename ManagedTokenSource>
template <typename EndTokenPred>
std::vector<AST::LifetimeParam>
Parser<ManagedTokenSource>::parse_lifetime_params_objs (
  EndTokenPred is_end_token)
{
  std::vector<AST::LifetimeParam> lifetime_params;

  while (!is_end_token (lexer.peek_token ()->get_id ()))
    {
      AST::LifetimeParam lifetime_param = parse_lifetime_param ();

      if (lifetime_param.is_error ())
	{
	  /* TODO: is it worth throwing away all lifetime params just because
	   * one failed? */
	  Error error (lexer.peek_token ()->get_locus (),
		       "failed to parse lifetime param in lifetime params");
	  add_error (std::move (error));

	  return {};
	}

      lifetime_params.push_back (std::move (lifetime_param));

      if (lexer.peek_token ()->get_id () != COMMA)
	break;

      // skip commas, including trailing commas
      lexer.skip_token ();
    }

  lifetime_params.shrink_to_fit ();

  return lifetime_params;
}

/* Parses a sequence of a certain grammar rule in object form (not pointer or
 * smart pointer), delimited by commas and ending when 'is_end_token' is
 * satisfied (templated). Will also consume any trailing comma.
 * FIXME: this cannot be used due to member function pointer problems (i.e.
 * parsing_function cannot be specified properly) */
template <typename ManagedTokenSource>
template <typename ParseFunction, typename EndTokenPred>
auto
Parser<ManagedTokenSource>::parse_non_ptr_sequence (
  ParseFunction parsing_function, EndTokenPred is_end_token,
  std::string error_msg) -> std::vector<decltype (parsing_function ())>
{
  std::vector<decltype (parsing_function ())> params;

  while (!is_end_token (lexer.peek_token ()->get_id ()))
    {
      auto param = parsing_function ();

      if (param.is_error ())
	{
	  // TODO: is it worth throwing away all params just because one
	  // failed?
	  Error error (lexer.peek_token ()->get_locus (),
		       std::move (error_msg));
	  add_error (std::move (error));

	  return {};
	}

      params.push_back (std::move (param));

      if (lexer.peek_token ()->get_id () != COMMA)
	break;

      // skip commas, including trailing commas
      lexer.skip_token ();
    }

  params.shrink_to_fit ();

  return params;
}

/* Parses a single lifetime generic parameter (not including comma). */
template <typename ManagedTokenSource>
AST::LifetimeParam
Parser<ManagedTokenSource>::parse_lifetime_param ()
{
  // parse outer attribute, which is optional and may not exist
  AST::Attribute outer_attr = parse_outer_attribute ();

  // save lifetime token - required
  const_TokenPtr lifetime_tok = lexer.peek_token ();
  if (lifetime_tok->get_id () != LIFETIME)
    {
      // if lifetime is missing, must not be a lifetime param, so return null
      return AST::LifetimeParam::create_error ();
    }
  lexer.skip_token ();
  /* TODO: does this always create a named lifetime? or can a different type
   * be made? */
  AST::Lifetime lifetime (AST::Lifetime::NAMED, lifetime_tok->get_str (),
			  lifetime_tok->get_locus ());

  // parse lifetime bounds, if it exists
  std::vector<AST::Lifetime> lifetime_bounds;
  if (lexer.peek_token ()->get_id () == COLON)
    {
      // parse lifetime bounds
      lifetime_bounds = parse_lifetime_bounds ();
      // TODO: have end token passed in?
    }

  return AST::LifetimeParam (std::move (lifetime), std::move (lifetime_bounds),
			     std::move (outer_attr),
			     lifetime_tok->get_locus ());
}

// Parses type generic parameters. Will also consume any trailing comma.
template <typename ManagedTokenSource>
std::vector<std::unique_ptr<AST::TypeParam>>
Parser<ManagedTokenSource>::parse_type_params ()
{
  std::vector<std::unique_ptr<AST::TypeParam>> type_params;

  // infinite loop with break on failure as no info on ending token
  while (true)
    {
      std::unique_ptr<AST::TypeParam> type_param = parse_type_param ();

      if (type_param == nullptr)
	{
	  // break if fails to parse
	  break;
	}

      type_params.push_back (std::move (type_param));

      if (lexer.peek_token ()->get_id () != COMMA)
	break;

      // skip commas, including trailing commas
      lexer.skip_token ();
    }

  type_params.shrink_to_fit ();
  return type_params;
}

// Parses type generic parameters. Will also consume any trailing comma.
template <typename ManagedTokenSource>
template <typename EndTokenPred>
std::vector<std::unique_ptr<AST::TypeParam>>
Parser<ManagedTokenSource>::parse_type_params (EndTokenPred is_end_token)
{
  std::vector<std::unique_ptr<AST::TypeParam>> type_params;

  while (!is_end_token (lexer.peek_token ()->get_id ()))
    {
      std::unique_ptr<AST::TypeParam> type_param = parse_type_param ();

      if (type_param == nullptr)
	{
	  Error error (lexer.peek_token ()->get_locus (),
		       "failed to parse type param in type params");
	  add_error (std::move (error));

	  return {};
	}

      type_params.push_back (std::move (type_param));

      if (lexer.peek_token ()->get_id () != COMMA)
	break;

      // skip commas, including trailing commas
      lexer.skip_token ();
    }

  type_params.shrink_to_fit ();
  return type_params;
  /* TODO: this shares most code with parse_lifetime_params - good place to
   * use template (i.e. parse_non_ptr_sequence if doable) */
}

/* Parses a single type (generic) parameter, not including commas. May change
 * to return value. */
template <typename ManagedTokenSource>
std::unique_ptr<AST::TypeParam>
Parser<ManagedTokenSource>::parse_type_param ()
{
  // parse outer attribute, which is optional and may not exist
  AST::Attribute outer_attr = parse_outer_attribute ();

  const_TokenPtr identifier_tok = lexer.peek_token ();
  if (identifier_tok->get_id () != IDENTIFIER)
    {
      // return null as type param can't exist without this required
      // identifier
      return nullptr;
    }
  // TODO: create identifier from identifier token
  Identifier ident = identifier_tok->get_str ();
  lexer.skip_token ();

  // parse type param bounds (if they exist)
  std::vector<std::unique_ptr<AST::TypeParamBound>> type_param_bounds;
  if (lexer.peek_token ()->get_id () == COLON)
    {
      lexer.skip_token ();

      // parse type param bounds, which may or may not exist
      type_param_bounds = parse_type_param_bounds ();
    }

  // parse type (if it exists)
  std::unique_ptr<AST::Type> type = nullptr;
  if (lexer.peek_token ()->get_id () == EQUAL)
    {
      lexer.skip_token ();

      // parse type (now required)
      type = parse_type ();
      if (type == nullptr)
	{
	  Error error (lexer.peek_token ()->get_locus (),
		       "failed to parse type in type param");
	  add_error (std::move (error));

	  return nullptr;
	}
    }

  return std::unique_ptr<AST::TypeParam> (
    new AST::TypeParam (std::move (ident), identifier_tok->get_locus (),
			std::move (type_param_bounds), std::move (type),
			std::move (outer_attr)));
}

/* Parses regular (i.e. non-generic) parameters in functions or methods. Also
 * has end token handling. */
template <typename ManagedTokenSource>
template <typename EndTokenPred>
std::vector<AST::FunctionParam>
Parser<ManagedTokenSource>::parse_function_params (EndTokenPred is_end_token)
{
  std::vector<AST::FunctionParam> params;

  if (is_end_token (lexer.peek_token ()->get_id ()))
    return params;

  AST::FunctionParam initial_param = parse_function_param ();

  // Return empty parameter list if no parameter there
  if (initial_param.is_error ())
    {
      // TODO: is this an error?
      return params;
    }

  params.push_back (std::move (initial_param));

  // maybe think of a better control structure here - do-while with an initial
  // error state? basically, loop through parameter list until can't find any
  // more params
  const_TokenPtr t = lexer.peek_token ();
  while (t->get_id () == COMMA)
    {
      // skip comma if applies
      lexer.skip_token ();

      // TODO: strictly speaking, shouldn't there be no trailing comma?
      if (is_end_token (lexer.peek_token ()->get_id ()))
	break;

      // now, as right paren would break, function param is required
      AST::FunctionParam param = parse_function_param ();
      if (param.is_error ())
	{
	  Error error (lexer.peek_token ()->get_locus (),
		       "failed to parse function param (in function params)");
	  add_error (std::move (error));

	  // skip somewhere?
	  return std::vector<AST::FunctionParam> ();
	}

      params.push_back (std::move (param));

      t = lexer.peek_token ();
    }

  params.shrink_to_fit ();
  return params;
}

/* Parses a single regular (i.e. non-generic) parameter in a function or
 * method, i.e. the "name: type" bit. Also handles it not existing. */
template <typename ManagedTokenSource>
AST::FunctionParam
Parser<ManagedTokenSource>::parse_function_param ()
{
  // parse outer attributes if they exist
  AST::AttrVec outer_attrs = parse_outer_attributes ();

  // TODO: should saved location be at start of outer attributes or pattern?
  Location locus = lexer.peek_token ()->get_locus ();
  std::unique_ptr<AST::Pattern> param_pattern = parse_pattern ();

  // create error function param if it doesn't exist
  if (param_pattern == nullptr)
    {
      // skip after something
      return AST::FunctionParam::create_error ();
    }

  if (!skip_token (COLON))
    {
      // skip after something
      return AST::FunctionParam::create_error ();
    }

  std::unique_ptr<AST::Type> param_type = parse_type ();
  if (param_type == nullptr)
    {
      // skip?
      return AST::FunctionParam::create_error ();
    }

  return AST::FunctionParam (std::move (param_pattern), std::move (param_type),
			     std::move (outer_attrs), locus);
}

/* Parses a function or method return type syntactical construction. Also
 * handles a function return type not existing. */
template <typename ManagedTokenSource>
std::unique_ptr<AST::Type>
Parser<ManagedTokenSource>::parse_function_return_type ()
{
  if (lexer.peek_token ()->get_id () != RETURN_TYPE)
    return nullptr;

  // skip return type, as it now obviously exists
  lexer.skip_token ();

  std::unique_ptr<AST::Type> type = parse_type ();

  return type;
}

/* Parses a "where clause" (in a function, struct, method, etc.). Also handles
 * a where clause not existing, in which it will return
 * WhereClause::create_empty(), which can be checked via
 * WhereClause::is_empty(). */
template <typename ManagedTokenSource>
AST::WhereClause
Parser<ManagedTokenSource>::parse_where_clause ()
{
  const_TokenPtr where_tok = lexer.peek_token ();
  if (where_tok->get_id () != WHERE)
    {
      // where clause doesn't exist, so create empty one
      return AST::WhereClause::create_empty ();
    }

  lexer.skip_token ();

  /* parse where clause items - this is not a separate rule in the reference
   * so won't be here */
  std::vector<std::unique_ptr<AST::WhereClauseItem>> where_clause_items;

  /* HACK: where clauses end with a right curly or semicolon or equals in all
   * uses currently */
  const_TokenPtr t = lexer.peek_token ();
  while (t->get_id () != LEFT_CURLY && t->get_id () != SEMICOLON
	 && t->get_id () != EQUAL)
    {
      std::unique_ptr<AST::WhereClauseItem> where_clause_item
	= parse_where_clause_item ();

      if (where_clause_item == nullptr)
	{
	  Error error (t->get_locus (), "failed to parse where clause item");
	  add_error (std::move (error));

	  return AST::WhereClause::create_empty ();
	}

      where_clause_items.push_back (std::move (where_clause_item));

      // also skip comma if it exists
      if (lexer.peek_token ()->get_id () != COMMA)
	break;

      lexer.skip_token ();
      t = lexer.peek_token ();
    }

  where_clause_items.shrink_to_fit ();
  return AST::WhereClause (std::move (where_clause_items));
}

/* Parses a where clause item (lifetime or type bound). Does not parse any
 * commas. */
template <typename ManagedTokenSource>
std::unique_ptr<AST::WhereClauseItem>
Parser<ManagedTokenSource>::parse_where_clause_item ()
{
  // shitty cheat way of determining lifetime or type bound - test for
  // lifetime
  const_TokenPtr t = lexer.peek_token ();

  if (t->get_id () == LIFETIME)
    return parse_lifetime_where_clause_item ();
  else
    return parse_type_bound_where_clause_item ();
}

// Parses a lifetime where clause item.
template <typename ManagedTokenSource>
std::unique_ptr<AST::LifetimeWhereClauseItem>
Parser<ManagedTokenSource>::parse_lifetime_where_clause_item ()
{
  AST::Lifetime lifetime = parse_lifetime ();
  if (lifetime.is_error ())
    {
      // TODO: error here?
      return nullptr;
    }

  if (!skip_token (COLON))
    {
      // TODO: skip after somewhere
      return nullptr;
    }

  std::vector<AST::Lifetime> lifetime_bounds = parse_lifetime_bounds ();
  // TODO: have end token passed in?

  Location locus = lifetime.get_locus ();

  return std::unique_ptr<AST::LifetimeWhereClauseItem> (
    new AST::LifetimeWhereClauseItem (std::move (lifetime),
				      std::move (lifetime_bounds), locus));
}

// Parses a type bound where clause item.
template <typename ManagedTokenSource>
std::unique_ptr<AST::TypeBoundWhereClauseItem>
Parser<ManagedTokenSource>::parse_type_bound_where_clause_item ()
{
  // parse for lifetimes, if it exists
  std::vector<AST::LifetimeParam> for_lifetimes;
  if (lexer.peek_token ()->get_id () == FOR)
    for_lifetimes = parse_for_lifetimes ();

  std::unique_ptr<AST::Type> type = parse_type ();
  if (type == nullptr)
    {
      return nullptr;
    }

  if (!skip_token (COLON))
    {
      // TODO: skip after somewhere
      return nullptr;
    }

  // parse type param bounds if they exist
  std::vector<std::unique_ptr<AST::TypeParamBound>> type_param_bounds
    = parse_type_param_bounds ();

  Location locus = lexer.peek_token ()->get_locus ();

  return std::unique_ptr<AST::TypeBoundWhereClauseItem> (
    new AST::TypeBoundWhereClauseItem (std::move (for_lifetimes),
				       std::move (type),
				       std::move (type_param_bounds), locus));
}

// Parses a for lifetimes clause, including the for keyword and angle
// brackets.
template <typename ManagedTokenSource>
std::vector<AST::LifetimeParam>
Parser<ManagedTokenSource>::parse_for_lifetimes ()
{
  std::vector<AST::LifetimeParam> params;

  if (!skip_token (FOR))
    {
      // skip after somewhere?
      return params;
    }

  if (!skip_token (LEFT_ANGLE))
    {
      // skip after somewhere?
      return params;
    }

  /* cannot specify end token due to parsing problems with '>' tokens being
   * nested */
  params = parse_lifetime_params_objs (is_right_angle_tok);

  if (!skip_generics_right_angle ())
    {
      // DEBUG
      rust_debug ("failed to skip generics right angle after (supposedly) "
		  "finished parsing where clause items");
      // ok, well this gets called.

      // skip after somewhere?
      return params;
    }

  return params;
}

// Parses type parameter bounds in where clause or generic arguments.
template <typename ManagedTokenSource>
std::vector<std::unique_ptr<AST::TypeParamBound>>
Parser<ManagedTokenSource>::parse_type_param_bounds ()
{
  std::vector<std::unique_ptr<AST::TypeParamBound>> type_param_bounds;

  std::unique_ptr<AST::TypeParamBound> initial_bound
    = parse_type_param_bound ();

  // quick exit if null
  if (initial_bound == nullptr)
    {
      /* error? type param bounds must have at least one term, but are bounds
       * optional? */
      return type_param_bounds;
    }
  type_param_bounds.push_back (std::move (initial_bound));

  while (lexer.peek_token ()->get_id () == PLUS)
    {
      lexer.skip_token ();

      std::unique_ptr<AST::TypeParamBound> bound = parse_type_param_bound ();
      if (bound == nullptr)
	{
	  /* not an error: bound is allowed to be null as trailing plus is
	   * allowed */
	  return type_param_bounds;
	}

      type_param_bounds.push_back (std::move (bound));
    }

  type_param_bounds.shrink_to_fit ();
  return type_param_bounds;
}

/* Parses type parameter bounds in where clause or generic arguments, with end
 * token handling. */
template <typename ManagedTokenSource>
template <typename EndTokenPred>
std::vector<std::unique_ptr<AST::TypeParamBound>>
Parser<ManagedTokenSource>::parse_type_param_bounds (EndTokenPred is_end_token)
{
  std::vector<std::unique_ptr<AST::TypeParamBound>> type_param_bounds;

  std::unique_ptr<AST::TypeParamBound> initial_bound
    = parse_type_param_bound ();

  // quick exit if null
  if (initial_bound == nullptr)
    {
      /* error? type param bounds must have at least one term, but are bounds
       * optional? */
      return type_param_bounds;
    }
  type_param_bounds.push_back (std::move (initial_bound));

  while (lexer.peek_token ()->get_id () == PLUS)
    {
      lexer.skip_token ();

      // break if end token character
      if (is_end_token (lexer.peek_token ()->get_id ()))
	break;

      std::unique_ptr<AST::TypeParamBound> bound = parse_type_param_bound ();
      if (bound == nullptr)
	{
	  // TODO how wise is it to ditch all bounds if only one failed?
	  Error error (lexer.peek_token ()->get_locus (),
		       "failed to parse type param bound in type param bounds");
	  add_error (std::move (error));

	  return {};
	}

      type_param_bounds.push_back (std::move (bound));
    }

  type_param_bounds.shrink_to_fit ();
  return type_param_bounds;
}

/* Parses a single type parameter bound in a where clause or generic argument.
 * Does not parse the '+' between arguments. */
template <typename ManagedTokenSource>
std::unique_ptr<AST::TypeParamBound>
Parser<ManagedTokenSource>::parse_type_param_bound ()
{
  // shitty cheat way of determining lifetime or trait bound - test for
  // lifetime
  const_TokenPtr t = lexer.peek_token ();
  switch (t->get_id ())
    {
    case LIFETIME:
      return std::unique_ptr<AST::Lifetime> (
	new AST::Lifetime (parse_lifetime ()));
    case LEFT_PAREN:
    case QUESTION_MARK:
    case FOR:
    case IDENTIFIER:
    case SUPER:
    case SELF:
    case SELF_ALIAS:
    case CRATE:
    case DOLLAR_SIGN:
      return parse_trait_bound ();
    default:
      // don't error - assume this is fine TODO
      return nullptr;
    }
}

// Parses a trait bound type param bound.
template <typename ManagedTokenSource>
std::unique_ptr<AST::TraitBound>
Parser<ManagedTokenSource>::parse_trait_bound ()
{
  bool has_parens = false;
  bool has_question_mark = false;

  Location locus = lexer.peek_token ()->get_locus ();

  // handle trait bound being in parentheses
  if (lexer.peek_token ()->get_id () == LEFT_PAREN)
    {
      has_parens = true;
      lexer.skip_token ();
    }

  // handle having question mark (optional)
  if (lexer.peek_token ()->get_id () == QUESTION_MARK)
    {
      has_question_mark = true;
      lexer.skip_token ();
    }

  /* parse for lifetimes, if it exists (although empty for lifetimes is ok to
   * handle this) */
  std::vector<AST::LifetimeParam> for_lifetimes;
  if (lexer.peek_token ()->get_id () == FOR)
    for_lifetimes = parse_for_lifetimes ();

  // handle TypePath
  AST::TypePath type_path = parse_type_path ();

  // handle closing parentheses
  if (has_parens)
    {
      if (!skip_token (RIGHT_PAREN))
	{
	  return nullptr;
	}
    }

  return std::unique_ptr<AST::TraitBound> (
    new AST::TraitBound (std::move (type_path), locus, has_parens,
			 has_question_mark, std::move (for_lifetimes)));
}

// Parses lifetime bounds.
template <typename ManagedTokenSource>
std::vector<AST::Lifetime>
Parser<ManagedTokenSource>::parse_lifetime_bounds ()
{
  std::vector<AST::Lifetime> lifetime_bounds;

  while (true)
    {
      AST::Lifetime lifetime = parse_lifetime ();

      // quick exit for parsing failure
      if (lifetime.is_error ())
	break;

      lifetime_bounds.push_back (std::move (lifetime));

      /* plus is maybe not allowed at end - spec defines it weirdly, so
       * assuming allowed at end */
      if (lexer.peek_token ()->get_id () != PLUS)
	break;

      lexer.skip_token ();
    }

  lifetime_bounds.shrink_to_fit ();
  return lifetime_bounds;
}

// Parses lifetime bounds, with added check for ending token.
template <typename ManagedTokenSource>
template <typename EndTokenPred>
std::vector<AST::Lifetime>
Parser<ManagedTokenSource>::parse_lifetime_bounds (EndTokenPred is_end_token)
{
  std::vector<AST::Lifetime> lifetime_bounds;

  while (!is_end_token (lexer.peek_token ()->get_id ()))
    {
      AST::Lifetime lifetime = parse_lifetime ();

      if (lifetime.is_error ())
	{
	  /* TODO: is it worth throwing away all lifetime bound info just
	   * because one failed? */
	  Error error (lexer.peek_token ()->get_locus (),
		       "failed to parse lifetime in lifetime bounds");
	  add_error (std::move (error));

	  return {};
	}

      lifetime_bounds.push_back (std::move (lifetime));

      /* plus is maybe not allowed at end - spec defines it weirdly, so
       * assuming allowed at end */
      if (lexer.peek_token ()->get_id () != PLUS)
	break;

      lexer.skip_token ();
    }

  lifetime_bounds.shrink_to_fit ();
  return lifetime_bounds;
}

/* Parses a lifetime token (named, 'static, or '_). Also handles lifetime not
 * existing. */
template <typename ManagedTokenSource>
AST::Lifetime
Parser<ManagedTokenSource>::parse_lifetime ()
{
  const_TokenPtr lifetime_tok = lexer.peek_token ();
  Location locus = lifetime_tok->get_locus ();
  // create error lifetime if doesn't exist
  if (lifetime_tok->get_id () != LIFETIME)
    {
      return AST::Lifetime::error ();
    }
  lexer.skip_token ();

  std::string lifetime_ident = lifetime_tok->get_str ();

  if (lifetime_ident == "'static")
    {
      return AST::Lifetime (AST::Lifetime::STATIC, "", locus);
    }
  else if (lifetime_ident == "'_")
    {
      return AST::Lifetime (AST::Lifetime::WILDCARD, "", locus);
    }
  else
    {
      return AST::Lifetime (AST::Lifetime::NAMED, std::move (lifetime_ident),
			    locus);
    }
}

// Parses a "type alias" (typedef) item.
template <typename ManagedTokenSource>
std::unique_ptr<AST::TypeAlias>
Parser<ManagedTokenSource>::parse_type_alias (AST::Visibility vis,
					      AST::AttrVec outer_attrs)
{
  Location locus = lexer.peek_token ()->get_locus ();
  skip_token (TYPE);

  // TODO: use this token for identifier when finished that
  const_TokenPtr alias_name_tok = expect_token (IDENTIFIER);
  if (alias_name_tok == nullptr)
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "could not parse identifier in type alias");
      add_error (std::move (error));

      skip_after_semicolon ();
      return nullptr;
    }
  Identifier alias_name = alias_name_tok->get_str ();

  // parse generic params, which may not exist
  std::vector<std::unique_ptr<AST::GenericParam>> generic_params
    = parse_generic_params_in_angles ();

  // parse where clause, which may not exist
  AST::WhereClause where_clause = parse_where_clause ();

  if (!skip_token (EQUAL))
    {
      skip_after_semicolon ();
      return nullptr;
    }

  std::unique_ptr<AST::Type> type_to_alias = parse_type ();

  if (!skip_token (SEMICOLON))
    {
      // should be skipping past this, not the next line
      return nullptr;
    }

  return std::unique_ptr<AST::TypeAlias> (
    new AST::TypeAlias (std::move (alias_name), std::move (generic_params),
			std::move (where_clause), std::move (type_to_alias),
			std::move (vis), std::move (outer_attrs), locus));
}

// Parse a struct item AST node.
template <typename ManagedTokenSource>
std::unique_ptr<AST::Struct>
Parser<ManagedTokenSource>::parse_struct (AST::Visibility vis,
					  AST::AttrVec outer_attrs)
{
  /* TODO: determine best way to parse the proper struct vs tuple struct -
   * share most of initial constructs so lookahead might be impossible, and if
   * not probably too expensive. Best way is probably unified parsing for the
   * initial parts and then pass them in as params to more derived functions.
   * Alternatively, just parse everything in this one function - do this if
   * function not too long. */

  /* Proper struct <- 'struct' IDENTIFIER generic_params? where_clause? ( '{'
   * struct_fields? '}' | ';' ) */
  /* Tuple struct <- 'struct' IDENTIFIER generic_params? '(' tuple_fields? ')'
   * where_clause? ';' */
  Location locus = lexer.peek_token ()->get_locus ();
  skip_token (STRUCT_TOK);

  // parse struct name
  const_TokenPtr name_tok = expect_token (IDENTIFIER);
  if (name_tok == nullptr)
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "could not parse struct or tuple struct identifier");
      add_error (std::move (error));

      // skip after somewhere?
      return nullptr;
    }
  Identifier struct_name = name_tok->get_str ();

  // parse generic params, which may or may not exist
  std::vector<std::unique_ptr<AST::GenericParam>> generic_params
    = parse_generic_params_in_angles ();

  // branch on next token - determines whether proper struct or tuple struct
  if (lexer.peek_token ()->get_id () == LEFT_PAREN)
    {
      // tuple struct

      // skip left parenthesis
      lexer.skip_token ();

      // parse tuple fields
      std::vector<AST::TupleField> tuple_fields;
      // Might be empty tuple for unit tuple struct.
      if (lexer.peek_token ()->get_id () == RIGHT_PAREN)
	tuple_fields = std::vector<AST::TupleField> ();
      else
	tuple_fields = parse_tuple_fields ();

      // tuple parameters must have closing parenthesis
      if (!skip_token (RIGHT_PAREN))
	{
	  skip_after_semicolon ();
	  return nullptr;
	}

      // parse where clause, which is optional
      AST::WhereClause where_clause = parse_where_clause ();

      if (!skip_token (SEMICOLON))
	{
	  // can't skip after semicolon because it's meant to be here
	  return nullptr;
	}

      return std::unique_ptr<AST::TupleStruct> (
	new AST::TupleStruct (std::move (tuple_fields), std::move (struct_name),
			      std::move (generic_params),
			      std::move (where_clause), std::move (vis),
			      std::move (outer_attrs), locus));
    }

  // assume it is a proper struct being parsed and continue outside of switch
  // - label only here to suppress warning

  // parse where clause, which is optional
  AST::WhereClause where_clause = parse_where_clause ();

  // branch on next token - determines whether struct is a unit struct
  const_TokenPtr t = lexer.peek_token ();
  switch (t->get_id ())
    {
      case LEFT_CURLY: {
	// struct with body

	// skip curly bracket
	lexer.skip_token ();

	// parse struct fields, if any
	std::vector<AST::StructField> struct_fields
	  = parse_struct_fields ([] (TokenId id) { return id == RIGHT_CURLY; });

	if (!skip_token (RIGHT_CURLY))
	  {
	    // skip somewhere?
	    return nullptr;
	  }

	return std::unique_ptr<AST::StructStruct> (new AST::StructStruct (
	  std::move (struct_fields), std::move (struct_name),
	  std::move (generic_params), std::move (where_clause), false,
	  std::move (vis), std::move (outer_attrs), locus));
      }
    case SEMICOLON:
      // unit struct declaration

      lexer.skip_token ();

      return std::unique_ptr<AST::StructStruct> (
	new AST::StructStruct (std::move (struct_name),
			       std::move (generic_params),
			       std::move (where_clause), std::move (vis),
			       std::move (outer_attrs), locus));
    default:
      add_error (Error (t->get_locus (),
			"unexpected token %qs in struct declaration",
			t->get_token_description ()));

      // skip somewhere?
      return nullptr;
    }
}

// Parses struct fields in struct declarations.
template <typename ManagedTokenSource>
std::vector<AST::StructField>
Parser<ManagedTokenSource>::parse_struct_fields ()
{
  std::vector<AST::StructField> fields;

  AST::StructField initial_field = parse_struct_field ();

  // Return empty field list if no field there
  if (initial_field.is_error ())
    return fields;

  fields.push_back (std::move (initial_field));

  while (lexer.peek_token ()->get_id () == COMMA)
    {
      lexer.skip_token ();

      AST::StructField field = parse_struct_field ();

      if (field.is_error ())
	{
	  // would occur with trailing comma, so allowed
	  break;
	}

      fields.push_back (std::move (field));
    }

  fields.shrink_to_fit ();
  return fields;
  // TODO: template if possible (parse_non_ptr_seq)
}

// Parses struct fields in struct declarations.
template <typename ManagedTokenSource>
template <typename EndTokenPred>
std::vector<AST::StructField>
Parser<ManagedTokenSource>::parse_struct_fields (EndTokenPred is_end_tok)
{
  std::vector<AST::StructField> fields;

  AST::StructField initial_field = parse_struct_field ();

  // Return empty field list if no field there
  if (initial_field.is_error ())
    return fields;

  fields.push_back (std::move (initial_field));

  while (lexer.peek_token ()->get_id () == COMMA)
    {
      lexer.skip_token ();

      if (is_end_tok (lexer.peek_token ()->get_id ()))
	break;

      AST::StructField field = parse_struct_field ();
      if (field.is_error ())
	{
	  /* TODO: should every field be ditched just because one couldn't be
	   * parsed? */
	  Error error (lexer.peek_token ()->get_locus (),
		       "failed to parse struct field in struct fields");
	  add_error (std::move (error));

	  return {};
	}

      fields.push_back (std::move (field));
    }

  fields.shrink_to_fit ();
  return fields;
  // TODO: template if possible (parse_non_ptr_seq)
}

// Parses a single struct field (in a struct definition). Does not parse
// commas.
template <typename ManagedTokenSource>
AST::StructField
Parser<ManagedTokenSource>::parse_struct_field ()
{
  // parse outer attributes, if they exist
  AST::AttrVec outer_attrs = parse_outer_attributes ();

  // parse visibility, if it exists
  AST::Visibility vis = parse_visibility ();

  Location locus = lexer.peek_token ()->get_locus ();

  // parse field name
  const_TokenPtr field_name_tok = lexer.peek_token ();
  if (field_name_tok->get_id () != IDENTIFIER)
    {
      // if not identifier, assumes there is no struct field and exits - not
      // necessarily error
      return AST::StructField::create_error ();
    }
  Identifier field_name = field_name_tok->get_str ();
  lexer.skip_token ();

  if (!skip_token (COLON))
    {
      // skip after somewhere?
      return AST::StructField::create_error ();
    }

  // parse field type - this is required
  std::unique_ptr<AST::Type> field_type = parse_type ();
  if (field_type == nullptr)
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "could not parse type in struct field definition");
      add_error (std::move (error));

      // skip after somewhere
      return AST::StructField::create_error ();
    }

  return AST::StructField (std::move (field_name), std::move (field_type),
			   std::move (vis), locus, std::move (outer_attrs));
}

// Parses tuple fields in tuple/tuple struct declarations.
template <typename ManagedTokenSource>
std::vector<AST::TupleField>
Parser<ManagedTokenSource>::parse_tuple_fields ()
{
  std::vector<AST::TupleField> fields;

  AST::TupleField initial_field = parse_tuple_field ();

  // Return empty field list if no field there
  if (initial_field.is_error ())
    {
      return fields;
    }

  fields.push_back (std::move (initial_field));

  // maybe think of a better control structure here - do-while with an initial
  // error state? basically, loop through field list until can't find any more
  // params HACK: all current syntax uses of tuple fields have them ending
  // with a right paren token
  const_TokenPtr t = lexer.peek_token ();
  while (t->get_id () == COMMA)
    {
      // skip comma if applies - e.g. trailing comma
      lexer.skip_token ();

      // break out due to right paren if it exists
      if (lexer.peek_token ()->get_id () == RIGHT_PAREN)
	{
	  break;
	}

      AST::TupleField field = parse_tuple_field ();
      if (field.is_error ())
	{
	  Error error (lexer.peek_token ()->get_locus (),
		       "failed to parse tuple field in tuple fields");
	  add_error (std::move (error));

	  return std::vector<AST::TupleField> ();
	}

      fields.push_back (std::move (field));

      t = lexer.peek_token ();
    }

  fields.shrink_to_fit ();
  return fields;

  // TODO: this shares basically all code with function params and struct
  // fields
  // - templates?
}

/* Parses a single tuple struct field in a tuple struct definition. Does not
 * parse commas. */
template <typename ManagedTokenSource>
AST::TupleField
Parser<ManagedTokenSource>::parse_tuple_field ()
{
  // parse outer attributes if they exist
  AST::AttrVec outer_attrs = parse_outer_attributes ();

  // parse visibility if it exists
  AST::Visibility vis = parse_visibility ();

  Location locus = lexer.peek_token ()->get_locus ();

  // parse type, which is required
  std::unique_ptr<AST::Type> field_type = parse_type ();
  if (field_type == nullptr)
    {
      // error if null
      Error error (lexer.peek_token ()->get_locus (),
		   "could not parse type in tuple struct field");
      add_error (std::move (error));

      // skip after something
      return AST::TupleField::create_error ();
    }

  return AST::TupleField (std::move (field_type), std::move (vis), locus,
			  std::move (outer_attrs));
}

// Parses a Rust "enum" tagged union item definition.
template <typename ManagedTokenSource>
std::unique_ptr<AST::Enum>
Parser<ManagedTokenSource>::parse_enum (AST::Visibility vis,
					AST::AttrVec outer_attrs)
{
  Location locus = lexer.peek_token ()->get_locus ();
  skip_token (ENUM_TOK);

  // parse enum name
  const_TokenPtr enum_name_tok = expect_token (IDENTIFIER);
  if (enum_name_tok == nullptr)
    return nullptr;

  Identifier enum_name = enum_name_tok->get_str ();

  // parse generic params (of enum container, not enum variants) if they exist
  std::vector<std::unique_ptr<AST::GenericParam>> generic_params
    = parse_generic_params_in_angles ();

  // parse where clause if it exists
  AST::WhereClause where_clause = parse_where_clause ();

  if (!skip_token (LEFT_CURLY))
    {
      skip_after_end_block ();
      return nullptr;
    }

  // parse actual enum variant definitions
  std::vector<std::unique_ptr<AST::EnumItem>> enum_items
    = parse_enum_items ([] (TokenId id) { return id == RIGHT_CURLY; });

  if (!skip_token (RIGHT_CURLY))
    {
      skip_after_end_block ();
      return nullptr;
    }

  return std::unique_ptr<AST::Enum> (
    new AST::Enum (std::move (enum_name), std::move (vis),
		   std::move (generic_params), std::move (where_clause),
		   std::move (enum_items), std::move (outer_attrs), locus));
}

// Parses the enum variants inside an enum definiton.
template <typename ManagedTokenSource>
std::vector<std::unique_ptr<AST::EnumItem>>
Parser<ManagedTokenSource>::parse_enum_items ()
{
  std::vector<std::unique_ptr<AST::EnumItem>> items;

  std::unique_ptr<AST::EnumItem> initial_item = parse_enum_item ();

  // Return empty item list if no field there
  if (initial_item == nullptr)
    return items;

  items.push_back (std::move (initial_item));

  while (lexer.peek_token ()->get_id () == COMMA)
    {
      lexer.skip_token ();

      std::unique_ptr<AST::EnumItem> item = parse_enum_item ();
      if (item == nullptr)
	{
	  // this would occur with a trailing comma, which is allowed
	  break;
	}

      items.push_back (std::move (item));
    }

  items.shrink_to_fit ();
  return items;

  /* TODO: use template if doable (parse_non_ptr_sequence) */
}

// Parses the enum variants inside an enum definiton.
template <typename ManagedTokenSource>
template <typename EndTokenPred>
std::vector<std::unique_ptr<AST::EnumItem>>
Parser<ManagedTokenSource>::parse_enum_items (EndTokenPred is_end_tok)
{
  std::vector<std::unique_ptr<AST::EnumItem>> items;

  std::unique_ptr<AST::EnumItem> initial_item = parse_enum_item ();

  // Return empty item list if no field there
  if (initial_item == nullptr)
    return items;

  items.push_back (std::move (initial_item));

  while (lexer.peek_token ()->get_id () == COMMA)
    {
      lexer.skip_token ();

      if (is_end_tok (lexer.peek_token ()->get_id ()))
	break;

      std::unique_ptr<AST::EnumItem> item = parse_enum_item ();
      if (item == nullptr)
	{
	  /* TODO should this ignore all successfully parsed enum items just
	   * because one failed? */
	  Error error (lexer.peek_token ()->get_locus (),
		       "failed to parse enum item in enum items");
	  add_error (std::move (error));

	  return {};
	}

      items.push_back (std::move (item));
    }

  items.shrink_to_fit ();
  return items;

  /* TODO: use template if doable (parse_non_ptr_sequence) */
}

/* Parses a single enum variant item in an enum definition. Does not parse
 * commas. */
template <typename ManagedTokenSource>
std::unique_ptr<AST::EnumItem>
Parser<ManagedTokenSource>::parse_enum_item ()
{
  // parse outer attributes if they exist
  AST::AttrVec outer_attrs = parse_outer_attributes ();

  // parse visibility, which may or may not exist
  AST::Visibility vis = parse_visibility ();

  // parse name for enum item, which is required
  const_TokenPtr item_name_tok = lexer.peek_token ();
  if (item_name_tok->get_id () != IDENTIFIER)
    {
      // this may not be an error but it means there is no enum item here
      return nullptr;
    }
  lexer.skip_token ();
  Identifier item_name = item_name_tok->get_str ();

  // branch based on next token
  const_TokenPtr t = lexer.peek_token ();
  switch (t->get_id ())
    {
      case LEFT_PAREN: {
	// tuple enum item
	lexer.skip_token ();

	std::vector<AST::TupleField> tuple_fields;
	// Might be empty tuple for unit tuple enum variant.
	if (lexer.peek_token ()->get_id () == RIGHT_PAREN)
	  tuple_fields = std::vector<AST::TupleField> ();
	else
	  tuple_fields = parse_tuple_fields ();

	if (!skip_token (RIGHT_PAREN))
	  {
	    // skip after somewhere
	    return nullptr;
	  }

	return std::unique_ptr<AST::EnumItemTuple> (new AST::EnumItemTuple (
	  std::move (item_name), std::move (vis), std::move (tuple_fields),
	  std::move (outer_attrs), item_name_tok->get_locus ()));
      }
      case LEFT_CURLY: {
	// struct enum item
	lexer.skip_token ();

	std::vector<AST::StructField> struct_fields
	  = parse_struct_fields ([] (TokenId id) { return id == RIGHT_CURLY; });

	if (!skip_token (RIGHT_CURLY))
	  {
	    // skip after somewhere
	    return nullptr;
	  }

	return std::unique_ptr<AST::EnumItemStruct> (new AST::EnumItemStruct (
	  std::move (item_name), std::move (vis), std::move (struct_fields),
	  std::move (outer_attrs), item_name_tok->get_locus ()));
      }
      case EQUAL: {
	// discriminant enum item
	lexer.skip_token ();

	std::unique_ptr<AST::Expr> discriminant_expr = parse_expr ();

	return std::unique_ptr<AST::EnumItemDiscriminant> (
	  new AST::EnumItemDiscriminant (std::move (item_name), std::move (vis),
					 std::move (discriminant_expr),
					 std::move (outer_attrs),
					 item_name_tok->get_locus ()));
      }
    default:
      // regular enum with just an identifier
      return std::unique_ptr<AST::EnumItem> (
	new AST::EnumItem (std::move (item_name), std::move (vis),
			   std::move (outer_attrs),
			   item_name_tok->get_locus ()));
    }
}

// Parses a C-style (and C-compat) untagged union declaration.
template <typename ManagedTokenSource>
std::unique_ptr<AST::Union>
Parser<ManagedTokenSource>::parse_union (AST::Visibility vis,
					 AST::AttrVec outer_attrs)
{
  /* hack - "weak keyword" by finding identifier called "union" (lookahead in
   * item switch) */
  const_TokenPtr union_keyword = expect_token (IDENTIFIER);
  rust_assert (union_keyword->get_str () == "union");
  Location locus = union_keyword->get_locus ();

  // parse actual union name
  const_TokenPtr union_name_tok = expect_token (IDENTIFIER);
  if (union_name_tok == nullptr)
    {
      skip_after_next_block ();
      return nullptr;
    }
  Identifier union_name = union_name_tok->get_str ();

  // parse optional generic parameters
  std::vector<std::unique_ptr<AST::GenericParam>> generic_params
    = parse_generic_params_in_angles ();

  // parse optional where clause
  AST::WhereClause where_clause = parse_where_clause ();

  if (!skip_token (LEFT_CURLY))
    {
      skip_after_end_block ();
      return nullptr;
    }

  /* parse union inner items as "struct fields" because hey, syntax reuse.
   * Spec said so. */
  std::vector<AST::StructField> union_fields
    = parse_struct_fields ([] (TokenId id) { return id == RIGHT_CURLY; });

  if (!skip_token (RIGHT_CURLY))
    {
      // skip after somewhere
      return nullptr;
    }

  return std::unique_ptr<AST::Union> (
    new AST::Union (std::move (union_name), std::move (vis),
		    std::move (generic_params), std::move (where_clause),
		    std::move (union_fields), std::move (outer_attrs), locus));
}

/* Parses a "constant item" (compile-time constant to maybe "inline"
 * throughout the program - like constexpr). */
template <typename ManagedTokenSource>
std::unique_ptr<AST::ConstantItem>
Parser<ManagedTokenSource>::parse_const_item (AST::Visibility vis,
					      AST::AttrVec outer_attrs)
{
  Location locus = lexer.peek_token ()->get_locus ();
  skip_token (CONST);

  /* get constant identifier - this is either a proper identifier or the _
   * wildcard */
  const_TokenPtr ident_tok = lexer.peek_token ();
  // make default identifier the underscore wildcard one
  std::string ident ("_");
  switch (ident_tok->get_id ())
    {
    case IDENTIFIER:
      ident = ident_tok->get_str ();
      lexer.skip_token ();
      break;
    case UNDERSCORE:
      // do nothing - identifier is already "_"
      lexer.skip_token ();
      break;
    default:
      add_error (
	Error (ident_tok->get_locus (),
	       "expected item name (identifier or %<_%>) in constant item "
	       "declaration - found %qs",
	       ident_tok->get_token_description ()));

      skip_after_semicolon ();
      return nullptr;
    }

  if (!skip_token (COLON))
    {
      skip_after_semicolon ();
      return nullptr;
    }

  // parse constant type (required)
  std::unique_ptr<AST::Type> type = parse_type ();

  if (!skip_token (EQUAL))
    {
      skip_after_semicolon ();
      return nullptr;
    }

  // parse constant expression (required)
  std::unique_ptr<AST::Expr> expr = parse_expr ();

  if (!skip_token (SEMICOLON))
    {
      // skip somewhere?
      return nullptr;
    }

  return std::unique_ptr<AST::ConstantItem> (
    new AST::ConstantItem (std::move (ident), std::move (vis), std::move (type),
			   std::move (expr), std::move (outer_attrs), locus));
}

// Parses a "static item" (static storage item, with 'static lifetime).
template <typename ManagedTokenSource>
std::unique_ptr<AST::StaticItem>
Parser<ManagedTokenSource>::parse_static_item (AST::Visibility vis,
					       AST::AttrVec outer_attrs)
{
  Location locus = lexer.peek_token ()->get_locus ();
  skip_token (STATIC_TOK);

  // determine whether static item is mutable
  bool is_mut = false;
  if (lexer.peek_token ()->get_id () == MUT)
    {
      is_mut = true;
      lexer.skip_token ();
    }

  const_TokenPtr ident_tok = expect_token (IDENTIFIER);
  if (ident_tok == nullptr)
    return nullptr;

  Identifier ident = ident_tok->get_str ();

  if (!skip_token (COLON))
    {
      skip_after_semicolon ();
      return nullptr;
    }

  // parse static item type (required)
  std::unique_ptr<AST::Type> type = parse_type ();

  if (!skip_token (EQUAL))
    {
      skip_after_semicolon ();
      return nullptr;
    }

  // parse static item expression (required)
  std::unique_ptr<AST::Expr> expr = parse_expr ();

  if (!skip_token (SEMICOLON))
    {
      // skip after somewhere
      return nullptr;
    }

  return std::unique_ptr<AST::StaticItem> (
    new AST::StaticItem (std::move (ident), is_mut, std::move (type),
			 std::move (expr), std::move (vis),
			 std::move (outer_attrs), locus));
}

// Parses a trait definition item, including unsafe ones.
template <typename ManagedTokenSource>
std::unique_ptr<AST::Trait>
Parser<ManagedTokenSource>::parse_trait (AST::Visibility vis,
					 AST::AttrVec outer_attrs)
{
  Location locus = lexer.peek_token ()->get_locus ();
  bool is_unsafe = false;
  if (lexer.peek_token ()->get_id () == UNSAFE)
    {
      is_unsafe = true;
      lexer.skip_token ();
    }

  skip_token (TRAIT);

  // parse trait name
  const_TokenPtr ident_tok = expect_token (IDENTIFIER);
  if (ident_tok == nullptr)
    return nullptr;

  Identifier ident = ident_tok->get_str ();

  // parse generic parameters (if they exist)
  std::vector<std::unique_ptr<AST::GenericParam>> generic_params
    = parse_generic_params_in_angles ();

  // create placeholder type param bounds in case they don't exist
  std::vector<std::unique_ptr<AST::TypeParamBound>> type_param_bounds;

  // parse type param bounds (if they exist)
  if (lexer.peek_token ()->get_id () == COLON)
    {
      lexer.skip_token ();

      type_param_bounds = parse_type_param_bounds (
	[] (TokenId id) { return id == WHERE || id == LEFT_CURLY; });
      // type_param_bounds = parse_type_param_bounds ();
    }

  // parse where clause (if it exists)
  AST::WhereClause where_clause = parse_where_clause ();

  if (!skip_token (LEFT_CURLY))
    {
      skip_after_end_block ();
      return nullptr;
    }

  // parse inner attrs (if they exist)
  AST::AttrVec inner_attrs = parse_inner_attributes ();

  // parse trait items
  std::vector<std::unique_ptr<AST::TraitItem>> trait_items;

  const_TokenPtr t = lexer.peek_token ();
  while (t->get_id () != RIGHT_CURLY)
    {
      std::unique_ptr<AST::TraitItem> trait_item = parse_trait_item ();

      if (trait_item == nullptr)
	{
	  Error error (lexer.peek_token ()->get_locus (),
		       "failed to parse trait item in trait");
	  add_error (std::move (error));

	  return nullptr;
	}
      trait_items.push_back (std::move (trait_item));

      t = lexer.peek_token ();
    }

  if (!skip_token (RIGHT_CURLY))
    {
      // skip after something
      return nullptr;
    }

  trait_items.shrink_to_fit ();
  return std::unique_ptr<AST::Trait> (
    new AST::Trait (std::move (ident), is_unsafe, std::move (generic_params),
		    std::move (type_param_bounds), std::move (where_clause),
		    std::move (trait_items), std::move (vis),
		    std::move (outer_attrs), std::move (inner_attrs), locus));
}

// Parses a trait item used inside traits (not trait, the Item).
template <typename ManagedTokenSource>
std::unique_ptr<AST::TraitItem>
Parser<ManagedTokenSource>::parse_trait_item ()
{
  // parse outer attributes (if they exist)
  AST::AttrVec outer_attrs = parse_outer_attributes ();

  // lookahead to determine what type of trait item to parse
  const_TokenPtr tok = lexer.peek_token ();
  switch (tok->get_id ())
    {
    case TYPE:
      return parse_trait_type (std::move (outer_attrs));
    case CONST:
      // disambiguate with function qualifier
      if (lexer.peek_token (1)->get_id () == IDENTIFIER)
	{
	  return parse_trait_const (std::move (outer_attrs));
	}
      // else, fallthrough to function
      // TODO: find out how to disable gcc "implicit fallthrough" error
      gcc_fallthrough ();
    case UNSAFE:
    case EXTERN_TOK:
      case FN_TOK: {
	/* function and method can't be disambiguated by lookahead alone
	 * (without a lot of work and waste), so either make a
	 * "parse_trait_function_or_method" or parse here mostly and pass in
	 * most parameters (or if short enough, parse whole thing here). */
	// parse function and method here

	// parse function or method qualifiers
	AST::FunctionQualifiers qualifiers = parse_function_qualifiers ();

	skip_token (FN_TOK);

	// parse function or method name
	const_TokenPtr ident_tok = expect_token (IDENTIFIER);
	if (ident_tok == nullptr)
	  return nullptr;

	Identifier ident = ident_tok->get_str ();

	// parse generic params
	std::vector<std::unique_ptr<AST::GenericParam>> generic_params
	  = parse_generic_params_in_angles ();

	if (!skip_token (LEFT_PAREN))
	  {
	    // skip after somewhere?
	    return nullptr;
	  }

	/* now for function vs method disambiguation - method has opening
	 * "self" param */
	AST::SelfParam self_param = parse_self_param ();
	/* FIXME: ensure that self param doesn't accidently consume tokens for
	 * a function */
	bool is_method = false;
	if (!self_param.is_error ())
	  {
	    is_method = true;

	    /* skip comma so function and method regular params can be parsed
	     * in same way */
	    if (lexer.peek_token ()->get_id () == COMMA)
	      lexer.skip_token ();
	  }

	// parse trait function params
	std::vector<AST::FunctionParam> function_params
	  = parse_function_params (
	    [] (TokenId id) { return id == RIGHT_PAREN; });

	if (!skip_token (RIGHT_PAREN))
	  {
	    // skip after somewhere?
	    return nullptr;
	  }

	// parse return type (optional)
	std::unique_ptr<AST::Type> return_type = parse_function_return_type ();

	// parse where clause (optional)
	AST::WhereClause where_clause = parse_where_clause ();

	// parse semicolon or function definition (in block)
	const_TokenPtr t = lexer.peek_token ();
	std::unique_ptr<AST::BlockExpr> definition = nullptr;
	switch (t->get_id ())
	  {
	  case SEMICOLON:
	    lexer.skip_token ();
	    // definition is already nullptr, so don't need to change it
	    break;
	  case LEFT_CURLY:
	    definition = parse_block_expr ();
	    /* FIXME: are these outer attributes meant to be passed into the
	     * block? */
	    break;
	  default:
	    add_error (
	      Error (t->get_locus (),
		     "expected %<;%> or definiton at the end of trait %s "
		     "definition - found %qs instead",
		     is_method ? "method" : "function",
		     t->get_token_description ()));

	    // skip?
	    return nullptr;
	  }

	// do actual if instead of ternary for return value optimisation
	if (is_method)
	  {
	    AST::TraitMethodDecl method_decl (std::move (ident),
					      std::move (qualifiers),
					      std::move (generic_params),
					      std::move (self_param),
					      std::move (function_params),
					      std::move (return_type),
					      std::move (where_clause));

	    // TODO: does this (method_decl) need move?
	    return std::unique_ptr<AST::TraitItemMethod> (
	      new AST::TraitItemMethod (std::move (method_decl),
					std::move (definition),
					std::move (outer_attrs),
					tok->get_locus ()));
	  }
	else
	  {
	    AST::TraitFunctionDecl function_decl (std::move (ident),
						  std::move (qualifiers),
						  std::move (generic_params),
						  std::move (function_params),
						  std::move (return_type),
						  std::move (where_clause));

	    return std::unique_ptr<AST::TraitItemFunc> (new AST::TraitItemFunc (
	      std::move (function_decl), std::move (definition),
	      std::move (outer_attrs), tok->get_locus ()));
	  }
      }
      default: {
	// TODO: try and parse macro invocation semi - if fails, maybe error.
	std::unique_ptr<AST::TraitItem> macro_invoc
	  = parse_macro_invocation_semi (outer_attrs);

	if (macro_invoc == nullptr)
	  {
	    // TODO: error?
	    return nullptr;
	  }
	else
	  {
	    return macro_invoc;
	  }
	/* FIXME: macro invocations can only start with certain tokens. be
	 * more picky with these? */
      }
    }
}

// Parse a typedef trait item.
template <typename ManagedTokenSource>
std::unique_ptr<AST::TraitItemType>
Parser<ManagedTokenSource>::parse_trait_type (AST::AttrVec outer_attrs)
{
  Location locus = lexer.peek_token ()->get_locus ();
  skip_token (TYPE);

  const_TokenPtr ident_tok = expect_token (IDENTIFIER);
  if (ident_tok == nullptr)
    return nullptr;

  Identifier ident = ident_tok->get_str ();

  std::vector<std::unique_ptr<AST::TypeParamBound>> bounds;

  // parse optional colon
  if (lexer.peek_token ()->get_id () == COLON)
    {
      lexer.skip_token ();

      // parse optional type param bounds
      bounds
	= parse_type_param_bounds ([] (TokenId id) { return id == SEMICOLON; });
      // bounds = parse_type_param_bounds ();
    }

  if (!skip_token (SEMICOLON))
    {
      // skip?
      return nullptr;
    }

  return std::unique_ptr<AST::TraitItemType> (
    new AST::TraitItemType (std::move (ident), std::move (bounds),
			    std::move (outer_attrs), locus));
}

// Parses a constant trait item.
template <typename ManagedTokenSource>
std::unique_ptr<AST::TraitItemConst>
Parser<ManagedTokenSource>::parse_trait_const (AST::AttrVec outer_attrs)
{
  Location locus = lexer.peek_token ()->get_locus ();
  skip_token (CONST);

  // parse constant item name
  const_TokenPtr ident_tok = expect_token (IDENTIFIER);
  if (ident_tok == nullptr)
    return nullptr;

  Identifier ident = ident_tok->get_str ();

  if (!skip_token (COLON))
    {
      skip_after_semicolon ();
      return nullptr;
    }

  // parse constant trait item type
  std::unique_ptr<AST::Type> type = parse_type ();

  // parse constant trait body expression, if it exists
  std::unique_ptr<AST::Expr> const_body = nullptr;
  if (lexer.peek_token ()->get_id () == EQUAL)
    {
      lexer.skip_token ();

      // expression must exist, so parse it
      const_body = parse_expr ();
    }

  if (!skip_token (SEMICOLON))
    {
      // skip after something?
      return nullptr;
    }

  return std::unique_ptr<AST::TraitItemConst> (
    new AST::TraitItemConst (std::move (ident), std::move (type),
			     std::move (const_body), std::move (outer_attrs),
			     locus));
}

/* Parses a struct "impl" item (both inherent impl and trait impl can be
 * parsed here), */
template <typename ManagedTokenSource>
std::unique_ptr<AST::Impl>
Parser<ManagedTokenSource>::parse_impl (AST::Visibility vis,
					AST::AttrVec outer_attrs)
{
  /* Note that only trait impls are allowed to be unsafe. So if unsafe, it
   * must be a trait impl. However, this isn't enough for full disambiguation,
   * so don't branch here. */
  Location locus = lexer.peek_token ()->get_locus ();
  bool is_unsafe = false;
  if (lexer.peek_token ()->get_id () == UNSAFE)
    {
      lexer.skip_token ();
      is_unsafe = true;
    }

  if (!skip_token (IMPL))
    {
      skip_after_next_block ();
      return nullptr;
    }

  // parse generic params (shared by trait and inherent impls)
  std::vector<std::unique_ptr<AST::GenericParam>> generic_params
    = parse_generic_params_in_angles ();

  // Again, trait impl-only feature, but optional one, so can be used for
  // branching yet.
  bool has_exclam = false;
  if (lexer.peek_token ()->get_id () == EXCLAM)
    {
      lexer.skip_token ();
      has_exclam = true;
    }

  /* FIXME: code that doesn't look shit for TypePath. Also, make sure this
   * doesn't parse too much and not work. */
  AST::TypePath type_path = parse_type_path ();
  if (type_path.is_error () || lexer.peek_token ()->get_id () != FOR)
    {
      /* cannot parse type path (or not for token next, at least), so must be
       * inherent impl */

      // hacky conversion of TypePath stack object to Type pointer
      std::unique_ptr<AST::Type> type = nullptr;
      if (!type_path.is_error ())
	type = std::unique_ptr<AST::TypePath> (
	  new AST::TypePath (std::move (type_path)));
      else
	type = parse_type ();

      // Type is required, so error if null
      if (type == nullptr)
	{
	  Error error (lexer.peek_token ()->get_locus (),
		       "could not parse type in inherent impl");
	  add_error (std::move (error));

	  skip_after_next_block ();
	  return nullptr;
	}

      // parse optional where clause
      AST::WhereClause where_clause = parse_where_clause ();

      if (!skip_token (LEFT_CURLY))
	{
	  // TODO: does this still skip properly?
	  skip_after_end_block ();
	  return nullptr;
	}

      // parse inner attributes (optional)
      AST::AttrVec inner_attrs = parse_inner_attributes ();

      // parse inherent impl items
      std::vector<std::unique_ptr<AST::InherentImplItem>> impl_items;

      const_TokenPtr t = lexer.peek_token ();
      while (t->get_id () != RIGHT_CURLY)
	{
	  std::unique_ptr<AST::InherentImplItem> impl_item
	    = parse_inherent_impl_item ();

	  if (impl_item == nullptr)
	    {
	      Error error (
		lexer.peek_token ()->get_locus (),
		"failed to parse inherent impl item in inherent impl");
	      add_error (std::move (error));

	      return nullptr;
	    }

	  impl_items.push_back (std::move (impl_item));

	  t = lexer.peek_token ();
	}

      if (!skip_token (RIGHT_CURLY))
	{
	  // skip somewhere
	  return nullptr;
	}

      // DEBUG
      rust_debug ("successfully parsed inherent impl");

      impl_items.shrink_to_fit ();

      return std::unique_ptr<AST::InherentImpl> (new AST::InherentImpl (
	std::move (impl_items), std::move (generic_params), std::move (type),
	std::move (where_clause), std::move (vis), std::move (inner_attrs),
	std::move (outer_attrs), locus));
    }
  else
    {
      // type path must both be valid and next token is for, so trait impl
      if (!skip_token (FOR))
	{
	  skip_after_next_block ();
	  return nullptr;
	}

      // parse type
      std::unique_ptr<AST::Type> type = parse_type ();
      // ensure type is included as it is required
      if (type == nullptr)
	{
	  Error error (lexer.peek_token ()->get_locus (),
		       "could not parse type in trait impl");
	  add_error (std::move (error));

	  skip_after_next_block ();
	  return nullptr;
	}

      // parse optional where clause
      AST::WhereClause where_clause = parse_where_clause ();

      if (!skip_token (LEFT_CURLY))
	{
	  // TODO: does this still skip properly?
	  skip_after_end_block ();
	  return nullptr;
	}

      // parse inner attributes (optional)
      AST::AttrVec inner_attrs = parse_inner_attributes ();

      // parse trait impl items
      std::vector<std::unique_ptr<AST::TraitImplItem>> impl_items;

      const_TokenPtr t = lexer.peek_token ();
      while (t->get_id () != RIGHT_CURLY)
	{
	  std::unique_ptr<AST::TraitImplItem> impl_item
	    = parse_trait_impl_item ();

	  if (impl_item == nullptr)
	    {
	      Error error (lexer.peek_token ()->get_locus (),
			   "failed to parse trait impl item in trait impl");
	      add_error (std::move (error));

	      return nullptr;
	    }

	  impl_items.push_back (std::move (impl_item));

	  t = lexer.peek_token ();

	  // DEBUG
	  rust_debug ("successfully parsed a trait impl item");
	}
      // DEBUG
      rust_debug ("successfully finished trait impl items");

      if (!skip_token (RIGHT_CURLY))
	{
	  // skip somewhere
	  return nullptr;
	}

      // DEBUG
      rust_debug ("successfully parsed trait impl");

      impl_items.shrink_to_fit ();

      return std::unique_ptr<AST::TraitImpl> (
	new AST::TraitImpl (std::move (type_path), is_unsafe, has_exclam,
			    std::move (impl_items), std::move (generic_params),
			    std::move (type), std::move (where_clause),
			    std::move (vis), std::move (inner_attrs),
			    std::move (outer_attrs), locus));
    }
}

// Parses a single inherent impl item (item inside an inherent impl block).
template <typename ManagedTokenSource>
std::unique_ptr<AST::InherentImplItem>
Parser<ManagedTokenSource>::parse_inherent_impl_item ()
{
  // parse outer attributes (if they exist)
  AST::AttrVec outer_attrs = parse_outer_attributes ();

  // TODO: cleanup - currently an unreadable mess

  // branch on next token:
  const_TokenPtr t = lexer.peek_token ();
  switch (t->get_id ())
    {
    case IDENTIFIER:
      // FIXME: Arthur: Do we need to some lookahead here?
      return parse_macro_invocation_semi (outer_attrs);
    case SUPER:
    case SELF:
    case CRATE:
      case PUB: {
	// visibility, so not a macro invocation semi - must be constant,
	// function, or method
	AST::Visibility vis = parse_visibility ();

	// TODO: is a recursive call to parse_inherent_impl_item better?
	switch (lexer.peek_token ()->get_id ())
	  {
	  case EXTERN_TOK:
	  case UNSAFE:
	  case FN_TOK:
	    // function or method
	    return parse_inherent_impl_function_or_method (std::move (vis),
							   std::move (
							     outer_attrs));
	  case CONST:
	    // lookahead to resolve production - could be function/method or
	    // const item
	    t = lexer.peek_token (1);

	    switch (t->get_id ())
	      {
	      case IDENTIFIER:
	      case UNDERSCORE:
		return parse_const_item (std::move (vis),
					 std::move (outer_attrs));
	      case UNSAFE:
	      case EXTERN_TOK:
	      case FN_TOK:
		return parse_inherent_impl_function_or_method (std::move (vis),
							       std::move (
								 outer_attrs));
	      default:
		add_error (Error (t->get_locus (),
				  "unexpected token %qs in some sort of const "
				  "item in inherent impl",
				  t->get_token_description ()));

		lexer.skip_token (1); // TODO: is this right thing to do?
		return nullptr;
	      }
	  default:
	    add_error (
	      Error (t->get_locus (),
		     "unrecognised token %qs for item in inherent impl",
		     t->get_token_description ()));
	    // skip?
	    return nullptr;
	  }
      }
    case EXTERN_TOK:
    case UNSAFE:
    case FN_TOK:
      // function or method
      return parse_inherent_impl_function_or_method (
	AST::Visibility::create_private (), std::move (outer_attrs));
    case CONST:
      /* lookahead to resolve production - could be function/method or const
       * item */
      t = lexer.peek_token (1);

      switch (t->get_id ())
	{
	case IDENTIFIER:
	case UNDERSCORE:
	  return parse_const_item (AST::Visibility::create_private (),
				   std::move (outer_attrs));
	case UNSAFE:
	case EXTERN_TOK:
	case FN_TOK:
	  return parse_inherent_impl_function_or_method (
	    AST::Visibility::create_private (), std::move (outer_attrs));
	default:
	  add_error (Error (t->get_locus (),
			    "unexpected token %qs in some sort of const item "
			    "in inherent impl",
			    t->get_token_description ()));

	  lexer.skip_token (1); // TODO: is this right thing to do?
	  return nullptr;
	}
      gcc_unreachable ();
    default:
      add_error (Error (t->get_locus (),
			"unrecognised token %qs for item in inherent impl",
			t->get_token_description ()));

      // skip?
      return nullptr;
    }
}

/* For internal use only by parse_inherent_impl_item() - splits giant method
 * into smaller ones and prevents duplication of logic. Strictly, this parses
 * a function or method item inside an inherent impl item block. */
// TODO: make this a templated function with "return type" as type param -
// InherentImplItem is this specialisation of the template while TraitImplItem
// will be the other.
template <typename ManagedTokenSource>
std::unique_ptr<AST::InherentImplItem>
Parser<ManagedTokenSource>::parse_inherent_impl_function_or_method (
  AST::Visibility vis, AST::AttrVec outer_attrs)
{
  Location locus = lexer.peek_token ()->get_locus ();
  // parse function or method qualifiers
  AST::FunctionQualifiers qualifiers = parse_function_qualifiers ();

  skip_token (FN_TOK);

  // parse function or method name
  const_TokenPtr ident_tok = expect_token (IDENTIFIER);
  if (ident_tok == nullptr)
    return nullptr;

  Identifier ident = ident_tok->get_str ();

  // parse generic params
  std::vector<std::unique_ptr<AST::GenericParam>> generic_params
    = parse_generic_params_in_angles ();

  if (!skip_token (LEFT_PAREN))
    {
      // skip after somewhere?
      return nullptr;
    }

  // now for function vs method disambiguation - method has opening "self"
  // param
  AST::SelfParam self_param = parse_self_param ();
  /* FIXME: ensure that self param doesn't accidently consume tokens for a
   * function one idea is to lookahead up to 4 tokens to see whether self is
   * one of them */
  bool is_method = false;
  if (!self_param.is_error ())
    {
      is_method = true;

      /* skip comma so function and method regular params can be parsed in
       * same way */
      if (lexer.peek_token ()->get_id () == COMMA)
	lexer.skip_token ();
    }

  // parse trait function params
  std::vector<AST::FunctionParam> function_params
    = parse_function_params ([] (TokenId id) { return id == RIGHT_PAREN; });

  if (!skip_token (RIGHT_PAREN))
    {
      skip_after_end_block ();
      return nullptr;
    }

  // parse return type (optional)
  std::unique_ptr<AST::Type> return_type = parse_function_return_type ();

  // parse where clause (optional)
  AST::WhereClause where_clause = parse_where_clause ();

  // parse function definition (in block) - semicolon not allowed
  if (lexer.peek_token ()->get_id () == SEMICOLON)
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "%s declaration in inherent impl not allowed - must have "
		   "a definition",
		   is_method ? "method" : "function");
      add_error (std::move (error));

      lexer.skip_token ();
      return nullptr;
    }
  std::unique_ptr<AST::BlockExpr> body = parse_block_expr ();
  if (body == nullptr)
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "could not parse definition in inherent impl %s definition",
		   is_method ? "method" : "function");
      add_error (std::move (error));

      skip_after_end_block ();
      return nullptr;
    }

  // do actual if instead of ternary for return value optimisation
  if (is_method)
    {
      return std::unique_ptr<AST::Method> (
	new AST::Method (std::move (ident), std::move (qualifiers),
			 std::move (generic_params), std::move (self_param),
			 std::move (function_params), std::move (return_type),
			 std::move (where_clause), std::move (body),
			 std::move (vis), std::move (outer_attrs), locus));
    }
  else
    {
      return std::unique_ptr<AST::Function> (
	new AST::Function (std::move (ident), std::move (qualifiers),
			   std::move (generic_params),
			   std::move (function_params), std::move (return_type),
			   std::move (where_clause), std::move (body),
			   std::move (vis), std::move (outer_attrs), locus));
    }
}

// Parses a single trait impl item (item inside a trait impl block).
template <typename ManagedTokenSource>
std::unique_ptr<AST::TraitImplItem>
Parser<ManagedTokenSource>::parse_trait_impl_item ()
{
  // parse outer attributes (if they exist)
  AST::AttrVec outer_attrs = parse_outer_attributes ();

  // TODO: clean this function up, it is basically unreadable hacks

  // branch on next token:
  const_TokenPtr t = lexer.peek_token ();
  switch (t->get_id ())
    {
    case IDENTIFIER:
    case SUPER:
    case SELF:
    case CRATE:
    case DOLLAR_SIGN:
      // these seem to be SimplePath tokens, so this is a macro invocation
      // semi
      return parse_macro_invocation_semi (std::move (outer_attrs));
    case TYPE:
      return parse_type_alias (AST::Visibility::create_private (),
			       std::move (outer_attrs));
      case PUB: {
	// visibility, so not a macro invocation semi - must be constant,
	// function, or method
	AST::Visibility vis = parse_visibility ();

	// TODO: is a recursive call to parse_trait_impl_item better?
	switch (lexer.peek_token ()->get_id ())
	  {
	  case TYPE:
	    return parse_type_alias (std::move (vis), std::move (outer_attrs));
	  case EXTERN_TOK:
	  case UNSAFE:
	  case FN_TOK:
	    // function or method
	    return parse_trait_impl_function_or_method (std::move (vis),
							std::move (
							  outer_attrs));
	  case CONST:
	    // lookahead to resolve production - could be function/method or
	    // const item
	    t = lexer.peek_token (1);

	    switch (t->get_id ())
	      {
	      case IDENTIFIER:
	      case UNDERSCORE:
		return parse_const_item (std::move (vis),
					 std::move (outer_attrs));
	      case UNSAFE:
	      case EXTERN_TOK:
	      case FN_TOK:
		return parse_trait_impl_function_or_method (std::move (vis),
							    std::move (
							      outer_attrs));
	      default:
		add_error (Error (t->get_locus (),
				  "unexpected token %qs in some sort of const "
				  "item in trait impl",
				  t->get_token_description ()));

		lexer.skip_token (1); // TODO: is this right thing to do?
		return nullptr;
	      }
	  default:
	    add_error (Error (t->get_locus (),
			      "unrecognised token %qs for item in trait impl",
			      t->get_token_description ()));

	    // skip?
	    return nullptr;
	  }
      }
    case EXTERN_TOK:
    case UNSAFE:
    case FN_TOK:
      // function or method
      return parse_trait_impl_function_or_method (
	AST::Visibility::create_private (), std::move (outer_attrs));
    case CONST:
      // lookahead to resolve production - could be function/method or const
      // item
      t = lexer.peek_token (1);

      switch (t->get_id ())
	{
	case IDENTIFIER:
	case UNDERSCORE:
	  return parse_const_item (AST::Visibility::create_private (),
				   std::move (outer_attrs));
	case UNSAFE:
	case EXTERN_TOK:
	case FN_TOK:
	  return parse_trait_impl_function_or_method (
	    AST::Visibility::create_private (), std::move (outer_attrs));
	default:
	  add_error (Error (
	    t->get_locus (),
	    "unexpected token %qs in some sort of const item in trait impl",
	    t->get_token_description ()));

	  lexer.skip_token (1); // TODO: is this right thing to do?
	  return nullptr;
	}
      gcc_unreachable ();
    default:
      add_error (Error (t->get_locus (),
			"unrecognised token %qs for item in trait impl",
			t->get_token_description ()));

      // skip?
      return nullptr;
    }
}

/* For internal use only by parse_trait_impl_item() - splits giant method into
 * smaller ones and prevents duplication of logic. Strictly, this parses a
 * function or method item inside a trait impl item block. */
template <typename ManagedTokenSource>
std::unique_ptr<AST::TraitImplItem>
Parser<ManagedTokenSource>::parse_trait_impl_function_or_method (
  AST::Visibility vis, AST::AttrVec outer_attrs)
{
  // this shares virtually all logic with
  // parse_inherent_impl_function_or_method
  // - template?
  Location locus = lexer.peek_token ()->get_locus ();

  // parse function or method qualifiers
  AST::FunctionQualifiers qualifiers = parse_function_qualifiers ();

  skip_token (FN_TOK);

  // parse function or method name
  const_TokenPtr ident_tok = expect_token (IDENTIFIER);
  if (ident_tok == nullptr)
    {
      return nullptr;
    }
  Identifier ident = ident_tok->get_str ();

  // DEBUG:
  rust_debug (
    "about to start parsing generic params in trait impl function or method");

  // parse generic params
  std::vector<std::unique_ptr<AST::GenericParam>> generic_params
    = parse_generic_params_in_angles ();

  // DEBUG:
  rust_debug (
    "finished parsing generic params in trait impl function or method");

  if (!skip_token (LEFT_PAREN))
    {
      // skip after somewhere?
      return nullptr;
    }

  // now for function vs method disambiguation - method has opening "self"
  // param
  AST::SelfParam self_param = parse_self_param ();
  // FIXME: ensure that self param doesn't accidently consume tokens for a
  // function
  bool is_method = false;
  if (!self_param.is_error ())
    {
      is_method = true;

      // skip comma so function and method regular params can be parsed in
      // same way
      if (lexer.peek_token ()->get_id () == COMMA)
	{
	  lexer.skip_token ();
	}

      // DEBUG
      rust_debug ("successfully parsed self param in method trait impl item");
    }

  // DEBUG
  rust_debug (
    "started to parse function params in function or method trait impl item");

  // parse trait function params (only if next token isn't right paren)
  std::vector<AST::FunctionParam> function_params;
  if (lexer.peek_token ()->get_id () != RIGHT_PAREN)
    {
      function_params
	= parse_function_params ([] (TokenId id) { return id == RIGHT_PAREN; });

      if (function_params.empty ())
	{
	  Error error (
	    lexer.peek_token ()->get_locus (),
	    "failed to parse function params in trait impl %s definition",
	    is_method ? "method" : "function");
	  add_error (std::move (error));

	  skip_after_next_block ();
	  return nullptr;
	}
    }

  // DEBUG
  rust_debug ("successfully parsed function params in function or method "
	      "trait impl item");

  if (!skip_token (RIGHT_PAREN))
    {
      skip_after_next_block ();
      return nullptr;
    }

  // parse return type (optional)
  std::unique_ptr<AST::Type> return_type = parse_function_return_type ();

  // DEBUG
  rust_debug (
    "successfully parsed return type in function or method trait impl item");

  // parse where clause (optional)
  AST::WhereClause where_clause = parse_where_clause ();

  // DEBUG
  rust_debug (
    "successfully parsed where clause in function or method trait impl item");

  // parse function definition (in block) - semicolon not allowed
  if (lexer.peek_token ()->get_id () == SEMICOLON)
    {
      Error error (
	lexer.peek_token ()->get_locus (),
	"%s declaration in trait impl not allowed - must have a definition",
	is_method ? "method" : "function");
      add_error (std::move (error));

      lexer.skip_token ();
      return nullptr;
    }
  std::unique_ptr<AST::BlockExpr> body = parse_block_expr ();
  if (body == nullptr)
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "could not parse definition in trait impl %s definition",
		   is_method ? "method" : "function");
      add_error (std::move (error));

      skip_after_end_block ();
      return nullptr;
    }

  // do actual if instead of ternary for return value optimisation
  if (is_method)
    {
      return std::unique_ptr<AST::Method> (
	new AST::Method (std::move (ident), std::move (qualifiers),
			 std::move (generic_params), std::move (self_param),
			 std::move (function_params), std::move (return_type),
			 std::move (where_clause), std::move (body),
			 std::move (vis), std::move (outer_attrs), locus));
    }
  else
    {
      return std::unique_ptr<AST::Function> (
	new AST::Function (std::move (ident), std::move (qualifiers),
			   std::move (generic_params),
			   std::move (function_params), std::move (return_type),
			   std::move (where_clause), std::move (body),
			   std::move (vis), std::move (outer_attrs), locus));
    }
}

// Parses an extern block of declarations.
template <typename ManagedTokenSource>
std::unique_ptr<AST::ExternBlock>
Parser<ManagedTokenSource>::parse_extern_block (AST::Visibility vis,
						AST::AttrVec outer_attrs)
{
  Location locus = lexer.peek_token ()->get_locus ();
  skip_token (EXTERN_TOK);

  // detect optional abi name
  std::string abi;
  const_TokenPtr next_tok = lexer.peek_token ();
  if (next_tok->get_id () == STRING_LITERAL)
    {
      lexer.skip_token ();
      abi = next_tok->get_str ();
    }

  if (!skip_token (LEFT_CURLY))
    {
      skip_after_end_block ();
      return nullptr;
    }

  AST::AttrVec inner_attrs = parse_inner_attributes ();

  // parse declarations inside extern block
  std::vector<std::unique_ptr<AST::ExternalItem>> extern_items;

  const_TokenPtr t = lexer.peek_token ();
  while (t->get_id () != RIGHT_CURLY)
    {
      std::unique_ptr<AST::ExternalItem> extern_item = parse_external_item ();

      if (extern_item == nullptr)
	{
	  Error error (t->get_locus (),
		       "failed to parse external item despite not reaching "
		       "end of extern block");
	  add_error (std::move (error));

	  return nullptr;
	}

      extern_items.push_back (std::move (extern_item));

      t = lexer.peek_token ();
    }

  if (!skip_token (RIGHT_CURLY))
    {
      // skip somewhere
      return nullptr;
    }

  extern_items.shrink_to_fit ();

  return std::unique_ptr<AST::ExternBlock> (
    new AST::ExternBlock (std::move (abi), std::move (extern_items),
			  std::move (vis), std::move (inner_attrs),
			  std::move (outer_attrs), locus));
}

// Parses a single extern block item (static or function declaration).
template <typename ManagedTokenSource>
std::unique_ptr<AST::ExternalItem>
Parser<ManagedTokenSource>::parse_external_item ()
{
  // parse optional outer attributes
  AST::AttrVec outer_attrs = parse_outer_attributes ();

  Location locus = lexer.peek_token ()->get_locus ();

  // parse optional visibility
  AST::Visibility vis = parse_visibility ();

  const_TokenPtr t = lexer.peek_token ();
  switch (t->get_id ())
    {
    case IDENTIFIER:
      return parse_macro_invocation_semi (outer_attrs);
      case STATIC_TOK: {
	// parse extern static item
	lexer.skip_token ();

	// parse mut (optional)
	bool has_mut = false;
	if (lexer.peek_token ()->get_id () == MUT)
	  {
	    lexer.skip_token ();
	    has_mut = true;
	  }

	// parse identifier
	const_TokenPtr ident_tok = expect_token (IDENTIFIER);
	if (ident_tok == nullptr)
	  {
	    skip_after_semicolon ();
	    return nullptr;
	  }
	Identifier ident = ident_tok->get_str ();

	if (!skip_token (COLON))
	  {
	    skip_after_semicolon ();
	    return nullptr;
	  }

	// parse type (required)
	std::unique_ptr<AST::Type> type = parse_type ();
	if (type == nullptr)
	  {
	    Error error (lexer.peek_token ()->get_locus (),
			 "failed to parse type in external static item");
	    add_error (std::move (error));

	    skip_after_semicolon ();
	    return nullptr;
	  }

	if (!skip_token (SEMICOLON))
	  {
	    // skip after somewhere?
	    return nullptr;
	  }

	return std::unique_ptr<AST::ExternalStaticItem> (
	  new AST::ExternalStaticItem (std::move (ident), std::move (type),
				       has_mut, std::move (vis),
				       std::move (outer_attrs), locus));
      }
      case FN_TOK: {
	// parse extern function declaration item
	// skip function token
	lexer.skip_token ();

	// parse identifier
	const_TokenPtr ident_tok = expect_token (IDENTIFIER);
	if (ident_tok == nullptr)
	  {
	    skip_after_semicolon ();
	    return nullptr;
	  }
	Identifier ident = ident_tok->get_str ();

	// parse (optional) generic params
	std::vector<std::unique_ptr<AST::GenericParam>> generic_params
	  = parse_generic_params_in_angles ();

	if (!skip_token (LEFT_PAREN))
	  {
	    skip_after_semicolon ();
	    return nullptr;
	  }

	// parse parameters
	std::vector<AST::NamedFunctionParam> function_params;
	bool is_variadic = false;
	AST::AttrVec variadic_attrs;

	const_TokenPtr t = lexer.peek_token ();
	while (t->get_id () != RIGHT_PAREN)
	  {
	    AST::AttrVec maybe_variadic_attrs = parse_outer_attributes ();
	    if (lexer.peek_token ()->get_id () == ELLIPSIS)
	      {
		// variadic - use attrs for this
		lexer.skip_token ();
		is_variadic = true;
		variadic_attrs = std::move (maybe_variadic_attrs);
		t = lexer.peek_token ();

		if (t->get_id () != RIGHT_PAREN)
		  {
		    Error error (t->get_locus (),
				 "expected right parentheses after variadic in "
				 "named function "
				 "parameters, found %qs",
				 t->get_token_description ());
		    add_error (std::move (error));

		    skip_after_semicolon ();
		    return nullptr;
		  }

		break;
	      }

	    AST::NamedFunctionParam param
	      = parse_named_function_param (std::move (maybe_variadic_attrs));
	    if (param.is_error ())
	      {
		Error error (t->get_locus (), "could not parse named function "
					      "parameter in external function");
		add_error (std::move (error));

		skip_after_semicolon ();
		return nullptr;
	      }
	    function_params.push_back (std::move (param));

	    if (lexer.peek_token ()->get_id () != COMMA)
	      break;

	    // skip comma
	    lexer.skip_token ();
	    t = lexer.peek_token ();
	  }

	if (!skip_token (RIGHT_PAREN))
	  {
	    skip_after_semicolon ();
	    return nullptr;
	  }

	// parse (optional) return type
	std::unique_ptr<AST::Type> return_type = parse_function_return_type ();

	// parse (optional) where clause
	AST::WhereClause where_clause = parse_where_clause ();

	if (!skip_token (SEMICOLON))
	  {
	    // skip somewhere?
	    return nullptr;
	  }

	function_params.shrink_to_fit ();

	return std::unique_ptr<AST::ExternalFunctionItem> (
	  new AST::ExternalFunctionItem (
	    std::move (ident), std::move (generic_params),
	    std::move (return_type), std::move (where_clause),
	    std::move (function_params), is_variadic,
	    std::move (variadic_attrs), std::move (vis),
	    std::move (outer_attrs), locus));
      }
    default:
      // error
      add_error (
	Error (t->get_locus (),
	       "unrecognised token %qs in extern block item declaration",
	       t->get_token_description ()));

      skip_after_semicolon ();
      return nullptr;
    }
}

/* Parses an extern block function param (with "pattern" being _ or an
 * identifier). */
template <typename ManagedTokenSource>
AST::NamedFunctionParam
Parser<ManagedTokenSource>::parse_named_function_param (
  AST::AttrVec outer_attrs)
{
  // parse identifier/_
  std::string name;

  const_TokenPtr t = lexer.peek_token ();
  Location name_location = t->get_locus ();
  switch (t->get_id ())
    {
    case IDENTIFIER:
      name = t->get_str ();
      lexer.skip_token ();
      break;
    case UNDERSCORE:
      name = "_";
      lexer.skip_token ();
      break;
    default:
      // this is not a function param, but not necessarily an error
      return AST::NamedFunctionParam::create_error ();
    }

  if (!skip_token (COLON))
    {
      // skip after somewhere?
      return AST::NamedFunctionParam::create_error ();
    }

  // parse (required) type
  std::unique_ptr<AST::Type> param_type = parse_type ();
  if (param_type == nullptr)
    {
      Error error (
	lexer.peek_token ()->get_locus (),
	"could not parse param type in extern block function declaration");
      add_error (std::move (error));

      skip_after_semicolon ();
      return AST::NamedFunctionParam::create_error ();
    }

  return AST::NamedFunctionParam (std::move (name), std::move (param_type),
				  std::move (outer_attrs), name_location);
}

// Parses a statement (will further disambiguate any statement).
template <typename ManagedTokenSource>
std::unique_ptr<AST::Stmt>
Parser<ManagedTokenSource>::parse_stmt (ParseRestrictions restrictions)
{
  // quick exit for empty statement
  // FIXME: Can we have empty statements without semicolons? Just nothing?
  const_TokenPtr t = lexer.peek_token ();
  if (t->get_id () == SEMICOLON)
    {
      lexer.skip_token ();
      return std::unique_ptr<AST::EmptyStmt> (
	new AST::EmptyStmt (t->get_locus ()));
    }

  // parse outer attributes
  AST::AttrVec outer_attrs = parse_outer_attributes ();

  // parsing this will be annoying because of the many different possibilities
  /* best may be just to copy paste in parse_item switch, and failing that try
   * to parse outer attributes, and then pass them in to either a let
   * statement or (fallback) expression statement. */
  // FIXME: think of a way to do this without such a large switch?
  t = lexer.peek_token ();
  switch (t->get_id ())
    {
    case LET:
      // let statement
      return parse_let_stmt (std::move (outer_attrs), restrictions);
    case PUB:
    case MOD:
    case EXTERN_TOK:
    case USE:
    case FN_TOK:
    case TYPE:
    case STRUCT_TOK:
    case ENUM_TOK:
    case CONST:
    case STATIC_TOK:
    case TRAIT:
    case IMPL:
    /* TODO: implement union keyword but not really because of
     * context-dependence crappy hack way to parse a union written below to
     * separate it from the good code. */
    // case UNION:
    case UNSAFE: // maybe - unsafe traits are a thing
      /* if any of these (should be all possible VisItem prefixes), parse a
       * VisItem can't parse item because would require reparsing outer
       * attributes */
      return parse_vis_item (std::move (outer_attrs));
      break;
    case SUPER:
    case SELF:
    case CRATE:
    case DOLLAR_SIGN:
      // almost certainly macro invocation semi
      return parse_macro_item (std::move (outer_attrs));
      break;
    // crappy hack to do union "keyword"
    case IDENTIFIER:
      if (t->get_str () == "union"
	  && lexer.peek_token (1)->get_id () == IDENTIFIER)
	{
	  return parse_vis_item (std::move (outer_attrs));
	  // or should this go straight to parsing union?
	}
      else if (t->get_str () == "macro_rules")
	{
	  // macro_rules! macro item
	  return parse_macro_item (std::move (outer_attrs));
	}
      else if (lexer.peek_token (1)->get_id () == SCOPE_RESOLUTION
	       || lexer.peek_token (1)->get_id () == EXCLAM)
	{
	  // FIXME: ensure doesn't take any expressions by mistake
	  /* path (probably) or macro invocation, so probably a macro
	   * invocation semi */
	  return parse_macro_item (std::move (outer_attrs));
	}
      gcc_fallthrough ();
      // TODO: find out how to disable gcc "implicit fallthrough" warning
    default:
      // fallback: expression statement
      return parse_expr_stmt (std::move (outer_attrs), restrictions);
      break;
    }
}

// Parses a let statement.
template <typename ManagedTokenSource>
std::unique_ptr<AST::LetStmt>
Parser<ManagedTokenSource>::parse_let_stmt (AST::AttrVec outer_attrs,
					    ParseRestrictions restrictions)
{
  Location locus = lexer.peek_token ()->get_locus ();
  skip_token (LET);

  // parse pattern (required)
  std::unique_ptr<AST::Pattern> pattern = parse_pattern ();
  if (pattern == nullptr)
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "failed to parse pattern in let statement");
      add_error (std::move (error));

      skip_after_semicolon ();
      return nullptr;
    }

  // parse type declaration (optional)
  std::unique_ptr<AST::Type> type = nullptr;
  if (lexer.peek_token ()->get_id () == COLON)
    {
      // must have a type declaration
      lexer.skip_token ();

      type = parse_type ();
      if (type == nullptr)
	{
	  Error error (lexer.peek_token ()->get_locus (),
		       "failed to parse type in let statement");
	  add_error (std::move (error));

	  skip_after_semicolon ();
	  return nullptr;
	}
    }

  // parse expression to set variable to (optional)
  std::unique_ptr<AST::Expr> expr = nullptr;
  if (lexer.peek_token ()->get_id () == EQUAL)
    {
      // must have an expression
      lexer.skip_token ();

      expr = parse_expr ();
      if (expr == nullptr)
	{
	  Error error (lexer.peek_token ()->get_locus (),
		       "failed to parse expression in let statement");
	  add_error (std::move (error));

	  skip_after_semicolon ();
	  return nullptr;
	}
    }

  if (restrictions.consume_semi)
    if (!skip_token (SEMICOLON))
      return nullptr;

  return std::unique_ptr<AST::LetStmt> (
    new AST::LetStmt (std::move (pattern), std::move (expr), std::move (type),
		      std::move (outer_attrs), locus));
}

// Parses a type path.
template <typename ManagedTokenSource>
AST::TypePath
Parser<ManagedTokenSource>::parse_type_path ()
{
  bool has_opening_scope_resolution = false;
  Location locus = lexer.peek_token ()->get_locus ();
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

template <typename ManagedTokenSource>
AST::GenericArg
Parser<ManagedTokenSource>::parse_generic_arg ()
{
  auto tok = lexer.peek_token ();
  std::unique_ptr<AST::Expr> expr = nullptr;

  switch (tok->get_id ())
    {
      case IDENTIFIER: {
	// This is a bit of a weird situation: With an identifier token, we
	// could either have a valid type or a macro (FIXME: anything else?). So
	// we need one bit of lookahead to differentiate if this is really
	auto next_tok = lexer.peek_token (1);
	if (next_tok->get_id () == EXCLAM)
	  {
	    auto type = parse_type ();
	    if (type)
	      return AST::GenericArg::create_type (std::move (type));
	    else
	      return AST::GenericArg::create_error ();
	  }
	lexer.skip_token ();
	return AST::GenericArg::create_ambiguous (tok->get_str (),
						  tok->get_locus ());
      }
    case LEFT_CURLY:
      expr = parse_block_expr ();
      break;
    case MINUS:
    case STRING_LITERAL:
    case CHAR_LITERAL:
    case INT_LITERAL:
    case FLOAT_LITERAL:
    case TRUE_LITERAL:
    case FALSE_LITERAL:
      expr = parse_literal_expr ();
      break;
      // FIXME: Because of this, error reporting is garbage for const generic
      // parameter's default values
      default: {
	auto type = parse_type ();
	// FIXME: Find a better way to do this?
	if (type)
	  return AST::GenericArg::create_type (std::move (type));
	else
	  return AST::GenericArg::create_error ();
      }
    }

  if (!expr)
    return AST::GenericArg::create_error ();

  return AST::GenericArg::create_const (std::move (expr));
}

// Parses the generic arguments in each path segment.
template <typename ManagedTokenSource>
AST::GenericArgs
Parser<ManagedTokenSource>::parse_path_generic_args ()
{
  if (!skip_token (LEFT_ANGLE))
    {
      // skip after somewhere?
      return AST::GenericArgs::create_empty ();
    }

  // We need to parse all lifetimes, then parse types and const generics in
  // any order.

  // try to parse lifetimes first
  std::vector<AST::Lifetime> lifetime_args;

  const_TokenPtr t = lexer.peek_token ();
  Location locus = t->get_locus ();
  while (!is_right_angle_tok (t->get_id ()))
    {
      AST::Lifetime lifetime = parse_lifetime ();
      if (lifetime.is_error ())
	{
	  // not necessarily an error
	  break;
	}

      lifetime_args.push_back (std::move (lifetime));

      // if next token isn't comma, then it must be end of list
      if (lexer.peek_token ()->get_id () != COMMA)
	{
	  break;
	}
      // skip comma
      lexer.skip_token ();

      t = lexer.peek_token ();
    }

  // try to parse types and const generics second
  std::vector<AST::GenericArg> generic_args;

  // TODO: think of better control structure
  t = lexer.peek_token ();
  while (!is_right_angle_tok (t->get_id ()))
    {
      // FIXME: Is it fine to break if there is one binding? Can't there be
      // bindings in between types?

      // ensure not binding being parsed as type accidently
      if (t->get_id () == IDENTIFIER
	  && lexer.peek_token (1)->get_id () == EQUAL)
	break;

      auto arg = parse_generic_arg ();
      if (!arg.is_error ())
	{
	  generic_args.emplace_back (std::move (arg));
	}

      // FIXME: Do we need to break if we encounter an error?

      // if next token isn't comma, then it must be end of list
      if (lexer.peek_token ()->get_id () != COMMA)
	break;

      // skip comma
      lexer.skip_token ();
      t = lexer.peek_token ();
    }

  // try to parse bindings third
  std::vector<AST::GenericArgsBinding> binding_args;

  // TODO: think of better control structure
  t = lexer.peek_token ();
  while (!is_right_angle_tok (t->get_id ()))
    {
      AST::GenericArgsBinding binding = parse_generic_args_binding ();
      if (binding.is_error ())
	{
	  // not necessarily an error
	  break;
	}

      binding_args.push_back (std::move (binding));

      // if next token isn't comma, then it must be end of list
      if (lexer.peek_token ()->get_id () != COMMA)
	{
	  break;
	}
      // skip comma
      lexer.skip_token ();

      t = lexer.peek_token ();
    }

  // skip any trailing commas
  if (lexer.peek_token ()->get_id () == COMMA)
    lexer.skip_token ();

  if (!skip_generics_right_angle ())
    return AST::GenericArgs::create_empty ();

  lifetime_args.shrink_to_fit ();
  generic_args.shrink_to_fit ();
  binding_args.shrink_to_fit ();

  return AST::GenericArgs (std::move (lifetime_args), std::move (generic_args),
			   std::move (binding_args), locus);
}

// Parses a binding in a generic args path segment.
template <typename ManagedTokenSource>
AST::GenericArgsBinding
Parser<ManagedTokenSource>::parse_generic_args_binding ()
{
  const_TokenPtr ident_tok = lexer.peek_token ();
  if (ident_tok->get_id () != IDENTIFIER)
    {
      // allow non error-inducing use
      // skip somewhere?
      return AST::GenericArgsBinding::create_error ();
    }
  lexer.skip_token ();
  Identifier ident = ident_tok->get_str ();

  if (!skip_token (EQUAL))
    {
      // skip after somewhere?
      return AST::GenericArgsBinding::create_error ();
    }

  // parse type (required)
  std::unique_ptr<AST::Type> type = parse_type ();
  if (type == nullptr)
    {
      // skip somewhere?
      return AST::GenericArgsBinding::create_error ();
    }

  return AST::GenericArgsBinding (std::move (ident), std::move (type),
				  ident_tok->get_locus ());
}

/* Parses a single type path segment (not including opening scope resolution,
 * but includes any internal ones). Includes generic args or type path
 * functions too. */
template <typename ManagedTokenSource>
std::unique_ptr<AST::TypePathSegment>
Parser<ManagedTokenSource>::parse_type_path_segment ()
{
  Location locus = lexer.peek_token ()->get_locus ();
  // parse ident segment part
  AST::PathIdentSegment ident_segment = parse_path_ident_segment ();
  if (ident_segment.is_error ())
    {
      // not necessarily an error
      return nullptr;
    }

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
      case LEFT_ANGLE: {
	// parse generic args
	AST::GenericArgs generic_args = parse_path_generic_args ();

	return std::unique_ptr<AST::TypePathSegmentGeneric> (
	  new AST::TypePathSegmentGeneric (std::move (ident_segment),
					   has_separating_scope_resolution,
					   std::move (generic_args), locus));
      }
      case LEFT_PAREN: {
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
  gcc_unreachable ();
}

// Parses a function call representation inside a type path.
template <typename ManagedTokenSource>
AST::TypePathFunction
Parser<ManagedTokenSource>::parse_type_path_function (Location id_location)
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
  Location locus = Linemap::unknown_location ();
  bool has_opening_scope_resolution = false;
  if (lexer.peek_token ()->get_id () == SCOPE_RESOLUTION)
    {
      has_opening_scope_resolution = true;

      locus = lexer.peek_token ()->get_locus ();

      lexer.skip_token ();
    }

  // create segment vector
  std::vector<AST::PathExprSegment> segments;

  if (locus == Linemap::unknown_location ())
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
  Location locus = lexer.peek_token ()->get_locus ();
  // parse ident segment
  AST::PathIdentSegment ident = parse_path_ident_segment ();
  if (ident.is_error ())
    {
      // not necessarily an error?
      return AST::PathExprSegment::create_error ();
    }

  // parse generic args (and turbofish), if they exist
  /* use lookahead to determine if they actually exist (don't want to
   * accidently parse over next ident segment) */
  if (lexer.peek_token ()->get_id () == SCOPE_RESOLUTION
      && lexer.peek_token (1)->get_id () == LEFT_ANGLE)
    {
      // skip scope resolution
      lexer.skip_token ();

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
  Location pratt_parsed_loc)
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
  Location locus = qual_path_type.get_locus ();

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
  Location pratt_parsed_loc)
{
  Location locus = pratt_parsed_loc;
  /* TODO: should this actually be error? is there anywhere where this could
   * be valid? */
  if (locus == Linemap::unknown_location ())
    {
      locus = lexer.peek_token ()->get_locus ();
      if (!skip_token (LEFT_ANGLE))
	{
	  // skip after somewhere?
	  return AST::QualifiedPathType::create_error ();
	}
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
  Location locus = lexer.peek_token ()->get_locus ();
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

// Parses a self param. Also handles self param not existing.
template <typename ManagedTokenSource>
AST::SelfParam
Parser<ManagedTokenSource>::parse_self_param ()
{
  bool has_reference = false;
  AST::Lifetime lifetime = AST::Lifetime::error ();

  Location locus = lexer.peek_token ()->get_locus ();

  // test if self is a reference parameter
  if (lexer.peek_token ()->get_id () == AMP)
    {
      has_reference = true;
      lexer.skip_token ();

      // now test whether it has a lifetime
      if (lexer.peek_token ()->get_id () == LIFETIME)
	{
	  lifetime = parse_lifetime ();

	  // something went wrong somehow
	  if (lifetime.is_error ())
	    {
	      Error error (lexer.peek_token ()->get_locus (),
			   "failed to parse lifetime in self param");
	      add_error (std::move (error));

	      // skip after somewhere?
	      return AST::SelfParam::create_error ();
	    }
	}
    }

  // test for mut
  bool has_mut = false;
  if (lexer.peek_token ()->get_id () == MUT)
    {
      has_mut = true;
      lexer.skip_token ();
    }

  // skip self token
  const_TokenPtr self_tok = lexer.peek_token ();
  if (self_tok->get_id () != SELF)
    {
      // skip after somewhere?
      return AST::SelfParam::create_error ();
    }
  lexer.skip_token ();

  // parse optional type
  std::unique_ptr<AST::Type> type = nullptr;
  if (lexer.peek_token ()->get_id () == COLON)
    {
      lexer.skip_token ();

      // type is now required
      type = parse_type ();
      if (type == nullptr)
	{
	  Error error (lexer.peek_token ()->get_locus (),
		       "could not parse type in self param");
	  add_error (std::move (error));

	  // skip after somewhere?
	  return AST::SelfParam::create_error ();
	}
    }

  // ensure that cannot have both type and reference
  if (type != nullptr && has_reference)
    {
      Error error (
	lexer.peek_token ()->get_locus (),
	"cannot have both a reference and a type specified in a self param");
      add_error (std::move (error));

      // skip after somewhere?
      return AST::SelfParam::create_error ();
    }

  if (has_reference)
    {
      return AST::SelfParam (std::move (lifetime), has_mut, locus);
    }
  else
    {
      // note that type may be nullptr here and that's fine
      return AST::SelfParam (std::move (type), has_mut, locus);
    }
}
