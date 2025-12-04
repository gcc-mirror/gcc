// Copyright (C) 2020-2025 Free Software Foundation, Inc.

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

/* DO NOT INCLUDE ANYWHERE - this is automatically included
 *   by rust-parse-impl-*.cc
 * This is also the reason why there are no include guards. */

#include "expected.h"
#include "rust-ast.h"
#include "rust-common.h"
#include "rust-expr.h"
#include "rust-item.h"
#include "rust-common.h"
#include "rust-parse.h"
#include "rust-token.h"
#define INCLUDE_ALGORITHM
#include "rust-diagnostics.h"
#include "rust-dir-owner.h"
#include "rust-keyword-values.h"
#include "rust-edition.h"
#include "rust-parse-error.h"

#include "optional.h"

namespace Rust {

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
    case RIGHT_SHIFT:
      {
	// new implementation that should be better
	lexer.split_current_token (RIGHT_ANGLE, RIGHT_ANGLE);
	lexer.skip_token ();
	return true;
      }
    case GREATER_OR_EQUAL:
      {
	// new implementation that should be better
	lexer.split_current_token (RIGHT_ANGLE, EQUAL);
	lexer.skip_token ();
	return true;
      }
    case RIGHT_SHIFT_EQ:
      {
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
tl::expected<std::vector<std::unique_ptr<AST::Item>>, Parse::Error::Items>
Parser<ManagedTokenSource>::parse_items ()
{
  std::vector<std::unique_ptr<AST::Item>> items;

  const_TokenPtr t = lexer.peek_token ();
  while (t->get_id () != END_OF_FILE)
    {
      auto item = parse_item (false);
      if (!item)
	return Parse::Error::Items::make_malformed (std::move (items));

      items.push_back (std::move (item.value ()));

      t = lexer.peek_token ();
    }

    // GCC 5->7 bug doesn't threat lvalue as an rvalue for the overload
#if __GNUC__ <= 7
  return std::move (items);
#else
  return items;
#endif
}

// Parses a crate (compilation unit) - entry point
template <typename ManagedTokenSource>
std::unique_ptr<AST::Crate>
Parser<ManagedTokenSource>::parse_crate ()
{
  // parse inner attributes
  AST::AttrVec inner_attrs = parse_inner_attributes ();

  // parse items
  auto items
    = parse_items ().value_or (std::vector<std::unique_ptr<AST::Item>>{});

  // emit all errors
  for (const auto &error : error_table)
    error.emit ();

  return std::unique_ptr<AST::Crate> (
    new AST::Crate (std::move (items), std::move (inner_attrs)));
}

// Parses an identifier/keyword as a Token
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::Token>, Parse::Error::Token>
Parser<ManagedTokenSource>::parse_identifier_or_keyword_token ()
{
  const_TokenPtr t = lexer.peek_token ();

  if (t->get_id () == IDENTIFIER || token_id_is_keyword (t->get_id ()))
    {
      lexer.skip_token ();
      return std::unique_ptr<AST::Token> (new AST::Token (std::move (t)));
    }
  else
    {
      add_error (Error (t->get_locus (), "expected keyword or identifier"));
      return Parse::Error::Token::make_malformed ();
    }
}

template <typename ManagedTokenSource>
bool
Parser<ManagedTokenSource>::is_macro_rules_def (const_TokenPtr t)
{
  auto macro_name = lexer.peek_token (2)->get_id ();

  bool allowed_macro_name = (macro_name == IDENTIFIER || macro_name == TRY);

  return t->get_str () == Values::WeakKeywords::MACRO_RULES
	 && lexer.peek_token (1)->get_id () == EXCLAM && allowed_macro_name;
}

// Parses a single item
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::Item>, Parse::Error::Item>
Parser<ManagedTokenSource>::parse_item (bool called_from_statement)
{
  // has a "called_from_statement" parameter for better error message handling

  // TODO: GCC 5 does not handle implicit return type correctly so we're forced
  // to specify it almost every time until the baseline GCC gets bumped.
  // Since this type is quite long and the code is dense we use an alias.
  //
  // When support for GCC 5 stops: remove this alias as well as the explicit
  // ctor calls.
  using RType = tl::expected<std::unique_ptr<AST::Item>, Parse::Error::Item>;

  // parse outer attributes for item
  AST::AttrVec outer_attrs = parse_outer_attributes ();
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
      return Parse::Error::Item::make_end_of_file ();

    case ASYNC:
    case PUB:
    case MOD:
    case EXTERN_KW:
    case USE:
    case FN_KW:
    case TYPE:
    case STRUCT_KW:
    case ENUM_KW:
    case CONST:
    case STATIC_KW:
    case AUTO:
    case TRAIT:
    case IMPL:
    case MACRO:
    /* TODO: implement union keyword but not really because of
     * context-dependence crappy hack way to parse a union written below to
     * separate it from the good code. */
    // case UNION:
    case UNSAFE: // maybe - unsafe traits are a thing
      // if any of these (should be all possible VisItem prefixes), parse a
      // VisItem
      {
	auto vis_item = parse_vis_item (std::move (outer_attrs));
	if (!vis_item)
	  return Parse::Error::Item::make_malformed ();
	return RType{std::move (vis_item)};
      }
    case SUPER:
    case SELF:
    case CRATE:
    case DOLLAR_SIGN:
      // almost certainly macro invocation semi
      {
	auto macro_invoc_semi
	  = parse_macro_invocation_semi (std::move (outer_attrs));
	if (!macro_invoc_semi)
	  return Parse::Error::Item::make_malformed ();
	return RType{std::move (macro_invoc_semi)};
      }
    // crappy hack to do union "keyword"
    case IDENTIFIER:
      // TODO: ensure std::string and literal comparison works
      if (t->get_str () == Values::WeakKeywords::UNION
	  && lexer.peek_token (1)->get_id () == IDENTIFIER)
	{
	  auto vis_item = parse_vis_item (std::move (outer_attrs));
	  if (!vis_item)
	    return Parse::Error::Item::make_malformed ();
	  return RType{std::move (vis_item)};
	  // or should this go straight to parsing union?
	}
      else if (t->get_str () == Values::WeakKeywords::DEFAULT
	       && lexer.peek_token (1)->get_id () != EXCLAM)
	{
	  add_error (Error (t->get_locus (),
			    "%qs is only allowed on items within %qs blocks",
			    "default", "impl"));
	  return Parse::Error::Item::make_malformed ();
	}
      else if (is_macro_rules_def (t))
	{
	  // macro_rules! macro item
	  auto macro_rule_def = parse_macro_rules_def (std::move (outer_attrs));
	  if (!macro_rule_def)
	    return Parse::Error::Item::make_malformed ();
	  return RType{std::move (macro_rule_def)};
	}
      else if (lexer.peek_token (1)->get_id () == SCOPE_RESOLUTION
	       || lexer.peek_token (1)->get_id () == EXCLAM)
	{
	  /* path (probably) or macro invocation, so probably a macro invocation
	   * semi */
	  auto macro_invocation_semi
	    = parse_macro_invocation_semi (std::move (outer_attrs));
	  if (!macro_invocation_semi)
	    return Parse::Error::Item::make_malformed ();
	  return RType{std::move (macro_invocation_semi)};
	}
      gcc_fallthrough ();
    default:
      // otherwise unrecognised
      add_error (Error (t->get_locus (),
			"unrecognised token %qs for start of %s",
			t->get_token_description (),
			called_from_statement ? "statement" : "item"));

      // skip somewhere?
      return Parse::Error::Item::make_malformed ();
      break;
    }
}

// Parses a VisItem (item that can have non-default visibility).
template <typename ManagedTokenSource>
std::unique_ptr<AST::VisItem>
Parser<ManagedTokenSource>::parse_vis_item (AST::AttrVec outer_attrs)
{
  // parse visibility, which may or may not exist
  auto vis_res = parse_visibility ();
  if (!vis_res)
    return nullptr;
  auto vis = vis_res.value ();

  // select VisItem to create depending on keyword
  const_TokenPtr t = lexer.peek_token ();

  switch (t->get_id ())
    {
    case MOD:
      return parse_module (std::move (vis), std::move (outer_attrs));
    case EXTERN_KW:
      // lookahead to resolve syntactical production
      t = lexer.peek_token (1);

      switch (t->get_id ())
	{
	case CRATE:
	  return parse_extern_crate (std::move (vis), std::move (outer_attrs));
	case FN_KW: // extern function
	  return parse_function (std::move (vis), std::move (outer_attrs));
	case LEFT_CURLY: // extern block
	  return parse_extern_block (std::move (vis), std::move (outer_attrs));
	case STRING_LITERAL: // for specifying extern ABI
	  // could be extern block or extern function, so more lookahead
	  t = lexer.peek_token (2);

	  switch (t->get_id ())
	    {
	    case FN_KW:
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
    case FN_KW:
      return parse_function (std::move (vis), std::move (outer_attrs));
    case TYPE:
      return parse_type_alias (std::move (vis), std::move (outer_attrs));
    case STRUCT_KW:
      return parse_struct (std::move (vis), std::move (outer_attrs));
    case ENUM_KW:
      return parse_enum (std::move (vis), std::move (outer_attrs));
    // TODO: implement union keyword but not really because of
    // context-dependence case UNION: crappy hack to do union "keyword"
    case IDENTIFIER:
      if (t->get_str () == Values::WeakKeywords::UNION
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
	case ASYNC:
	  return parse_async_item (std::move (vis), std::move (outer_attrs));
	case UNSAFE:
	case EXTERN_KW:
	case FN_KW:
	  return parse_function (std::move (vis), std::move (outer_attrs));
	default:
	  add_error (
	    Error (t->get_locus (),
		   "unexpected token %qs in some sort of const production",
		   t->get_token_description ()));

	  lexer.skip_token (1); // TODO: is this right thing to do?
	  return nullptr;
	}
    // for async functions
    case ASYNC:
      return parse_async_item (std::move (vis), std::move (outer_attrs));

    case STATIC_KW:
      return parse_static_item (std::move (vis), std::move (outer_attrs));
    case AUTO:
    case TRAIT:
      return parse_trait (std::move (vis), std::move (outer_attrs));
    case IMPL:
      return parse_impl (std::move (vis), std::move (outer_attrs));
    case UNSAFE: // unsafe traits, unsafe functions, unsafe impls (trait impls),
      // lookahead to resolve syntactical production
      t = lexer.peek_token (1);

      switch (t->get_id ())
	{
	case AUTO:
	case TRAIT:
	  return parse_trait (std::move (vis), std::move (outer_attrs));
	case EXTERN_KW:
	case FN_KW:
	  return parse_function (std::move (vis), std::move (outer_attrs));
	case IMPL:
	  return parse_impl (std::move (vis), std::move (outer_attrs));
	case MOD:
	  return parse_module (std::move (vis), std::move (outer_attrs));
	default:
	  add_error (
	    Error (t->get_locus (),
		   "unexpected token %qs in some sort of unsafe production",
		   t->get_token_description ()));

	  lexer.skip_token (1); // TODO: is this right thing to do?
	  return nullptr;
	}
    case MACRO:
      return parse_decl_macro_def (std::move (vis), std::move (outer_attrs));
    default:
      // otherwise vis item clearly doesn't exist, which is not an error
      // has a catch-all post-switch return to allow other breaks to occur
      break;
    }
  return nullptr;
}

template <typename ManagedTokenSource>
std::unique_ptr<AST::Function>
Parser<ManagedTokenSource>::parse_async_item (AST::Visibility vis,
					      AST::AttrVec outer_attrs)
{
  auto offset = (lexer.peek_token ()->get_id () == CONST) ? 1 : 0;
  const_TokenPtr t = lexer.peek_token (offset);

  if (get_rust_edition () == Edition::E2015)
    {
      add_error (Error (t->get_locus (), ErrorCode::E0670,
			"%<async fn%> is not permitted in Rust 2015"));
      add_error (
	Error::Hint (t->get_locus (),
		     "to use %<async fn%>, switch to Rust 2018 or later"));
    }

  t = lexer.peek_token (offset + 1);

  switch (t->get_id ())
    {
    case UNSAFE:
    case FN_KW:
      return parse_function (std::move (vis), std::move (outer_attrs));

    default:
      add_error (
	Error (t->get_locus (), "expected item, found keyword %<async%>"));

      lexer.skip_token (1);
      return nullptr;
    }
}

// Parses a macro rules definition syntax extension whatever thing.
template <typename ManagedTokenSource>
std::unique_ptr<AST::MacroRulesDefinition>
Parser<ManagedTokenSource>::parse_macro_rules_def (AST::AttrVec outer_attrs)
{
  // ensure that first token is identifier saying "macro_rules"
  const_TokenPtr t = lexer.peek_token ();
  if (t->get_id () != IDENTIFIER
      || t->get_str () != Values::WeakKeywords::MACRO_RULES)
    {
      Error error (
	t->get_locus (),
	"macro rules definition does not start with %<macro_rules%>");
      add_error (std::move (error));

      // skip after somewhere?
      return nullptr;
    }
  lexer.skip_token ();
  location_t macro_locus = t->get_locus ();

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
  Identifier rule_name{ident_tok};

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
      if (Parse::Utils::token_id_matches_delims (lexer.peek_token ()->get_id (),
						 delim_type))
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
  if (Parse::Utils::token_id_matches_delims (t->get_id (), delim_type))
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
		AST::MacroRulesDefinition::mbe (
		  std::move (rule_name), delim_type, std::move (macro_rules),
		  std::move (outer_attrs), macro_locus));
	    }
	}

      return std::unique_ptr<AST::MacroRulesDefinition> (
	AST::MacroRulesDefinition::mbe (std::move (rule_name), delim_type,
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

// Parses a declarative macro 2.0 definition.
template <typename ManagedTokenSource>
std::unique_ptr<AST::MacroRulesDefinition>
Parser<ManagedTokenSource>::parse_decl_macro_def (AST::Visibility vis,
						  AST::AttrVec outer_attrs)
{
  // ensure that first token is identifier saying "macro"
  const_TokenPtr t = lexer.peek_token ();
  if (t->get_id () != MACRO)
    {
      Error error (
	t->get_locus (),
	"declarative macro definition does not start with %<macro%>");
      add_error (std::move (error));

      // skip after somewhere?
      return nullptr;
    }
  lexer.skip_token ();
  location_t macro_locus = t->get_locus ();

  // parse macro name
  const_TokenPtr ident_tok = expect_token (IDENTIFIER);
  if (ident_tok == nullptr)
    {
      return nullptr;
    }
  Identifier rule_name{ident_tok};

  t = lexer.peek_token ();
  if (t->get_id () == LEFT_PAREN)
    {
      // single definiton of macro rule
      // e.g. `macro foo($e:expr) {}`

      // parse macro matcher
      location_t locus = lexer.peek_token ()->get_locus ();
      AST::MacroMatcher matcher = parse_macro_matcher ();
      if (matcher.is_error ())
	return nullptr;

      // check delimiter of macro matcher
      if (matcher.get_delim_type () != AST::DelimType::PARENS)
	{
	  Error error (locus, "only parenthesis can be used for a macro "
			      "matcher in declarative macro definition");
	  add_error (std::move (error));
	  return nullptr;
	}

      location_t transcriber_loc = lexer.peek_token ()->get_locus ();
      auto delim_tok_tree = parse_delim_token_tree ();
      if (!delim_tok_tree)
	return nullptr;

      AST::MacroTranscriber transcriber (delim_tok_tree.value (),
					 transcriber_loc);

      if (transcriber.get_token_tree ().get_delim_type ()
	  != AST::DelimType::CURLY)
	{
	  Error error (transcriber_loc,
		       "only braces can be used for a macro transcriber "
		       "in declarative macro definition");
	  add_error (std::move (error));
	  return nullptr;
	}

      std::vector<AST::MacroRule> macro_rules;
      macro_rules.emplace_back (std::move (matcher), std::move (transcriber),
				locus);

      return std::unique_ptr<AST::MacroRulesDefinition> (
	AST::MacroRulesDefinition::decl_macro (std::move (rule_name),
					       macro_rules,
					       std::move (outer_attrs),
					       macro_locus, vis));
    }
  else if (t->get_id () == LEFT_CURLY)
    {
      // multiple definitions of macro rule separated by comma
      // e.g. `macro foo { () => {}, ($e:expr) => {}, }`

      // parse left curly
      const_TokenPtr left_curly = expect_token (LEFT_CURLY);
      if (left_curly == nullptr)
	{
	  return nullptr;
	}

      // parse actual macro rules
      std::vector<AST::MacroRule> macro_rules;

      // must be at least one macro rule, so parse it
      AST::MacroRule initial_rule = parse_macro_rule ();
      if (initial_rule.is_error ())
	{
	  Error error (
	    lexer.peek_token ()->get_locus (),
	    "required first macro rule in declarative macro definition "
	    "could not be parsed");
	  add_error (std::move (error));

	  // skip after somewhere?
	  return nullptr;
	}
      macro_rules.push_back (std::move (initial_rule));

      t = lexer.peek_token ();
      // parse macro rules
      while (t->get_id () == COMMA)
	{
	  // skip comma
	  lexer.skip_token ();

	  // don't parse if end of macro rules
	  if (Parse::Utils::token_id_matches_delims (
		lexer.peek_token ()->get_id (), AST::CURLY))
	    {
	      break;
	    }

	  // try to parse next rule
	  AST::MacroRule rule = parse_macro_rule ();
	  if (rule.is_error ())
	    {
	      Error error (
		lexer.peek_token ()->get_locus (),
		"failed to parse macro rule in declarative macro definition");
	      add_error (std::move (error));

	      return nullptr;
	    }

	  macro_rules.push_back (std::move (rule));

	  t = lexer.peek_token ();
	}

      // parse right curly
      const_TokenPtr right_curly = expect_token (RIGHT_CURLY);
      if (right_curly == nullptr)
	{
	  return nullptr;
	}

      return std::unique_ptr<AST::MacroRulesDefinition> (
	AST::MacroRulesDefinition::decl_macro (std::move (rule_name),
					       std::move (macro_rules),
					       std::move (outer_attrs),
					       macro_locus, vis));
    }
  else
    {
      add_error (Error (t->get_locus (),
			"unexpected token %qs - expecting delimiters "
			"(for a declarative macro definiton)",
			t->get_token_description ()));
      return nullptr;
    }
}

/* Parses a visibility syntactical production (i.e. creating a non-default
 * visibility) */
template <typename ManagedTokenSource>
tl::expected<AST::Visibility, Parse::Error::Visibility>
Parser<ManagedTokenSource>::parse_visibility ()
{
  // check for no visibility
  if (lexer.peek_token ()->get_id () != PUB)
    {
      return AST::Visibility::create_private ();
    }

  auto vis_loc = lexer.peek_token ()->get_locus ();
  lexer.skip_token ();

  // create simple pub visibility if
  // - found no parentheses
  // - found unit type `()`
  if (lexer.peek_token ()->get_id () != LEFT_PAREN
      || lexer.peek_token (1)->get_id () == RIGHT_PAREN)
    {
      return AST::Visibility::create_public (vis_loc);
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

      return AST::Visibility::create_crate (path_loc, vis_loc);
    case SELF:
      lexer.skip_token ();

      skip_token (RIGHT_PAREN);

      return AST::Visibility::create_self (path_loc, vis_loc);
    case SUPER:
      lexer.skip_token ();

      skip_token (RIGHT_PAREN);

      return AST::Visibility::create_super (path_loc, vis_loc);
    case IN:
      {
	lexer.skip_token ();

	// parse the "in" path as well
	auto path = parse_simple_path ();
	if (!path)
	  {
	    Error error (lexer.peek_token ()->get_locus (),
			 "missing path in pub(in path) visibility");
	    add_error (std::move (error));

	    // skip after somewhere?
	    return Parse::Error::Visibility::make_missing_path ();
	  }

	skip_token (RIGHT_PAREN);

	return AST::Visibility::create_in_path (std::move (path.value ()),
						vis_loc);
      }
    default:
      add_error (Error (t->get_locus (), "unexpected token %qs in visibility",
			t->get_token_description ()));

      lexer.skip_token ();
      return Parse::Error::Visibility::make_malformed ();
    }
}

// Parses a module - either a bodied module or a module defined in another file.
template <typename ManagedTokenSource>
std::unique_ptr<AST::Module>
Parser<ManagedTokenSource>::parse_module (AST::Visibility vis,
					  AST::AttrVec outer_attrs)
{
  location_t locus = lexer.peek_token ()->get_locus ();

  Unsafety safety = Unsafety::Normal;
  if (lexer.peek_token ()->get_id () == UNSAFE)
    {
      safety = Unsafety::Unsafe;
      skip_token (UNSAFE);
    }

  skip_token (MOD);

  const_TokenPtr module_name = expect_token (IDENTIFIER);
  if (module_name == nullptr)
    {
      return nullptr;
    }
  Identifier name{module_name};

  const_TokenPtr t = lexer.peek_token ();

  switch (t->get_id ())
    {
    case SEMICOLON:
      lexer.skip_token ();

      // Construct an external module
      return std::unique_ptr<AST::Module> (
	new AST::Module (std::move (name), std::move (vis),
			 std::move (outer_attrs), locus, safety,
			 lexer.get_filename (), inline_module_stack));
    case LEFT_CURLY:
      {
	lexer.skip_token ();

	// parse inner attributes
	AST::AttrVec inner_attrs = parse_inner_attributes ();

	std::string default_path = name.as_string ();

	if (inline_module_stack.empty ())
	  {
	    std::string filename = lexer.get_filename ();
	    auto slash_idx = filename.rfind (file_separator);
	    if (slash_idx == std::string::npos)
	      slash_idx = 0;
	    else
	      slash_idx++;
	    filename = filename.substr (slash_idx);

	    std::string subdir;
	    if (get_file_subdir (filename, subdir))
	      default_path = subdir + file_separator + name.as_string ();
	  }

	std::string module_path_name
	  = extract_module_path (inner_attrs, outer_attrs, default_path);
	InlineModuleStackScope scope (*this, std::move (module_path_name));

	// parse items
	std::vector<std::unique_ptr<AST::Item>> items;
	const_TokenPtr tok = lexer.peek_token ();
	while (tok->get_id () != RIGHT_CURLY)
	  {
	    auto item = parse_item (false);
	    if (!item)
	      {
		Error error (tok->get_locus (),
			     "failed to parse item in module");
		add_error (std::move (error));

		return nullptr;
	      }

	    items.push_back (std::move (item.value ()));

	    tok = lexer.peek_token ();
	  }

	if (!skip_token (RIGHT_CURLY))
	  {
	    // skip somewhere?
	    return nullptr;
	  }

	return std::unique_ptr<AST::Module> (
	  new AST::Module (std::move (name), locus, std::move (items),
			   std::move (vis), safety, std::move (inner_attrs),
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
  location_t locus = lexer.peek_token ()->get_locus ();
  if (!skip_token (EXTERN_KW))
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
      crate_name = Values::Keywords::SELF;
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
      as_name = Values::Keywords::UNDERSCORE;
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
  location_t locus = lexer.peek_token ()->get_locus ();
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

  location_t locus = lexer.peek_token ()->get_locus ();

  // bool has_path = false;
  auto path = parse_simple_path ();

  if (!path)
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
	case LEFT_CURLY:
	  {
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
      const_TokenPtr t = lexer.peek_token ();

      switch (t->get_id ())
	{
	case AS:
	  {
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
					  std::move (path.value ()), locus, t));
	      case UNDERSCORE:
		// skip lexer token
		lexer.skip_token ();

		return std::unique_ptr<AST::UseTreeRebind> (
		  new AST::UseTreeRebind (AST::UseTreeRebind::WILDCARD,
					  std::move (path.value ()), locus,
					  {Values::Keywords::UNDERSCORE,
					   t->get_locus ()}));
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
	case COMMA:
	case RIGHT_CURLY:
	  // this may occur in recursive calls - assume it is ok and ignore it
	  return std::unique_ptr<AST::UseTreeRebind> (
	    new AST::UseTreeRebind (AST::UseTreeRebind::NONE,
				    std::move (path.value ()), locus));
	case SCOPE_RESOLUTION:
	  // keep going
	  break;
	default:
	  add_error (Error (t->get_locus (),
			    "unexpected token %qs in use tree with valid path",
			    t->get_token_description ()));
	  return nullptr;
	}

      skip_token ();
      t = lexer.peek_token ();

      switch (t->get_id ())
	{
	case ASTERISK:
	  // glob UseTree type
	  lexer.skip_token ();

	  return std::unique_ptr<AST::UseTreeGlob> (
	    new AST::UseTreeGlob (AST::UseTreeGlob::PATH_PREFIXED,
				  std::move (path.value ()), locus));
	case LEFT_CURLY:
	  {
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
				    std::move (path.value ()),
				    std::move (use_trees), locus));
	  }
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
					    AST::AttrVec outer_attrs,
					    bool is_external)
{
  location_t locus = lexer.peek_token ()->get_locus ();
  // Get qualifiers for function if they exist
  AST::FunctionQualifiers qualifiers = parse_function_qualifiers ();

  skip_token (FN_KW);

  // Save function name token
  const_TokenPtr function_name_tok = expect_token (IDENTIFIER);
  if (function_name_tok == nullptr)
    {
      skip_after_next_block ();
      return nullptr;
    }
  Identifier function_name{function_name_tok};

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

  auto initial_param = parse_self_param ();

  if (!initial_param.has_value ()
      && initial_param.error () != ParseSelfError::NOT_SELF)
    return nullptr;

  if (initial_param.has_value () && lexer.peek_token ()->get_id () == COMMA)
    skip_token ();

  // parse function parameters (only if next token isn't right paren)
  std::vector<std::unique_ptr<AST::Param>> function_params;

  if (lexer.peek_token ()->get_id () != RIGHT_PAREN)
    function_params
      = parse_function_params ([] (TokenId id) { return id == RIGHT_PAREN; });

  if (initial_param.has_value ())
    function_params.insert (function_params.begin (),
			    std::move (*initial_param));

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

  tl::optional<std::unique_ptr<AST::BlockExpr>> body = tl::nullopt;
  if (lexer.peek_token ()->get_id () == SEMICOLON)
    lexer.skip_token ();
  else
    {
      std::unique_ptr<AST::BlockExpr> block_expr = parse_block_expr ();
      if (block_expr == nullptr)
	return nullptr;
      body = std::move (block_expr);
    }

  return std::unique_ptr<AST::Function> (
    new AST::Function (std::move (function_name), std::move (qualifiers),
		       std::move (generic_params), std::move (function_params),
		       std::move (return_type), std::move (where_clause),
		       std::move (body), std::move (vis),
		       std::move (outer_attrs), locus, false, is_external));
}

// Parses function or method qualifiers (i.e. const, unsafe, and extern).
template <typename ManagedTokenSource>
AST::FunctionQualifiers
Parser<ManagedTokenSource>::parse_function_qualifiers ()
{
  Async async_status = Async::No;
  Const const_status = Const::No;
  Unsafety unsafe_status = Unsafety::Normal;
  bool has_extern = false;
  std::string abi;

  const_TokenPtr t;
  location_t locus;
  // Check in order of const, unsafe, then extern
  for (int i = 0; i < 2; i++)
    {
      t = lexer.peek_token ();
      locus = t->get_locus ();

      switch (t->get_id ())
	{
	case CONST:
	  lexer.skip_token ();
	  const_status = Const::Yes;
	  break;
	case ASYNC:
	  lexer.skip_token ();
	  async_status = Async::Yes;
	  break;
	default:
	  // const status is still none
	  break;
	}
    }

  if (lexer.peek_token ()->get_id () == UNSAFE)
    {
      lexer.skip_token ();
      unsafe_status = Unsafety::Unsafe;
    }

  if (lexer.peek_token ()->get_id () == EXTERN_KW)
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

  return AST::FunctionQualifiers (locus, async_status, const_status,
				  unsafe_status, has_extern, std::move (abi));
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
    = parse_generic_params (Parse::Utils::is_right_angle_tok);

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
  auto outer_attrs = parse_outer_attributes ();
  std::unique_ptr<AST::GenericParam> param;
  auto token = lexer.peek_token ();

  switch (token->get_id ())
    {
    case LIFETIME:
      {
	auto lifetime = parse_lifetime (false);
	if (!lifetime)
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
	  std::move (lifetime.value ()), std::move (lifetime_bounds),
	  std::move (outer_attrs), token->get_locus ()));
	break;
      }
    case IDENTIFIER:
      {
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
    case CONST:
      {
	lexer.skip_token ();

	auto name_token = expect_token (IDENTIFIER);

	if (!name_token || !expect_token (COLON))
	  return nullptr;

	auto type = parse_type ();
	if (!type)
	  return nullptr;

	// optional default value
	tl::optional<AST::GenericArg> default_expr = tl::nullopt;
	if (lexer.peek_token ()->get_id () == EQUAL)
	  {
	    lexer.skip_token ();
	    auto tok = lexer.peek_token ();
	    default_expr = parse_generic_arg ();

	    if (!default_expr)
	      {
		rust_error_at (tok->get_locus (),
			       "invalid token for start of default value for "
			       "const generic parameter: expected %<block%>, "
			       "%<identifier%> or %<literal%>, got %qs",
			       token_id_to_str (tok->get_id ()));
		return nullptr;
	      }

	    // At this point, we *know* that we are parsing a const
	    // expression
	    if (default_expr.value ().get_kind ()
		== AST::GenericArg::Kind::Either)
	      default_expr = default_expr.value ().disambiguate_to_const ();
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
		     token->as_string ().c_str ());
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
  // Did we parse a const param with a default value yet
  auto const_with_default_seen = false;
  // Did the user write a lifetime parameter after a type one
  auto order_error = false;
  // Did the user write a const param with a default value after a type one
  auto const_with_default_order_error = false;

  // parse lifetime params
  while (!is_end_token (lexer.peek_token ()->get_id ()))
    {
      auto param = parse_generic_param (is_end_token);
      if (param)
	{
	  if (param->get_kind () == AST::GenericParam::Kind::Type)
	    {
	      type_seen = true;
	      if (const_with_default_seen)
		const_with_default_order_error = true;
	    }
	  else if (param->get_kind () == AST::GenericParam::Kind::Lifetime
		   && type_seen)
	    {
	      order_error = true;
	      if (const_with_default_seen)
		const_with_default_order_error = true;
	    }
	  else if (param->get_kind () == AST::GenericParam::Kind::Const)
	    {
	      type_seen = true;
	      AST::ConstGenericParam *const_param
		= static_cast<AST::ConstGenericParam *> (param.get ());
	      if (const_param->has_default_value ())
		const_with_default_seen = true;
	      else if (const_with_default_seen)
		const_with_default_order_error = true;
	    }

	  generic_params.emplace_back (std::move (param));
	  maybe_skip_token (COMMA);
	}
      else
	break;
    }

  // FIXME: Add reordering hint
  if (order_error)
    {
      Error error (generic_params.front ()->get_locus (),
		   "invalid order for generic parameters: lifetime parameters "
		   "must be declared prior to type and const parameters");
      add_error (std::move (error));
    }
  if (const_with_default_order_error)
    {
      Error error (generic_params.front ()->get_locus (),
		   "invalid order for generic parameters: generic parameters "
		   "with a default must be trailing");
      add_error (std::move (error));
    }

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
      auto lifetime_param = parse_lifetime_param ();

      if (!lifetime_param)
	{
	  // can't treat as error as only way to get out with trailing comma
	  break;
	}

      lifetime_params.emplace_back (
	new AST::LifetimeParam (std::move (lifetime_param.value ())));

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
      auto lifetime_param = parse_lifetime_param ();

      if (!lifetime_param)
	{
	  /* TODO: is it worth throwing away all lifetime params just because
	   * one failed? */
	  Error error (lexer.peek_token ()->get_locus (),
		       "failed to parse lifetime param in lifetime params");
	  add_error (std::move (error));

	  return {};
	}

      lifetime_params.emplace_back (
	new AST::LifetimeParam (std::move (lifetime_param)));

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
 * TODO: is this best solution? implements most of the same algorithm.
 * TODO: seems to be unused, remove? */
template <typename ManagedTokenSource>
std::vector<AST::LifetimeParam>
Parser<ManagedTokenSource>::parse_lifetime_params_objs ()
{
  std::vector<AST::LifetimeParam> lifetime_params;

  // bad control structure as end token cannot be guaranteed
  while (true)
    {
      auto lifetime_param = parse_lifetime_param ();

      if (!lifetime_param)
	{
	  // not an error as only way to exit if trailing comma
	  break;
	}

      lifetime_params.push_back (std::move (lifetime_param.value ()));

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
      auto lifetime_param = parse_lifetime_param ();

      if (!lifetime_param)
	{
	  /* TODO: is it worth throwing away all lifetime params just because
	   * one failed? */
	  Error error (lexer.peek_token ()->get_locus (),
		       "failed to parse lifetime param in lifetime params");
	  add_error (std::move (error));

	  return {};
	}

      lifetime_params.push_back (std::move (lifetime_param.value ()));

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
tl::expected<AST::LifetimeParam, ParseLifetimeParamError>
Parser<ManagedTokenSource>::parse_lifetime_param ()
{
  // parse outer attributes, which are optional and may not exist
  auto outer_attrs = parse_outer_attributes ();

  // save lifetime token - required
  const_TokenPtr lifetime_tok = lexer.peek_token ();
  if (lifetime_tok->get_id () != LIFETIME)
    {
      // if lifetime is missing, must not be a lifetime param, so return error
      return tl::make_unexpected<ParseLifetimeParamError> ({});
    }
  lexer.skip_token ();
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
			     std::move (outer_attrs),
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
  // parse outer attributes, which are optional and may not exist
  auto outer_attrs = parse_outer_attributes ();

  const_TokenPtr identifier_tok = lexer.peek_token ();
  if (identifier_tok->get_id () != IDENTIFIER)
    {
      // return null as type param can't exist without this required
      // identifier
      return nullptr;
    }
  Identifier ident{identifier_tok};
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
			std::move (outer_attrs)));
}

/* Parses regular (i.e. non-generic) parameters in functions or methods. Also
 * has end token handling. */
template <typename ManagedTokenSource>
template <typename EndTokenPred>
std::vector<std::unique_ptr<AST::Param>>
Parser<ManagedTokenSource>::parse_function_params (EndTokenPred is_end_token)
{
  std::vector<std::unique_ptr<AST::Param>> params;

  if (is_end_token (lexer.peek_token ()->get_id ()))
    return params;

  auto initial_param = parse_function_param ();

  // Return empty parameter list if no parameter there
  if (initial_param == nullptr)
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
      auto param = parse_function_param ();
      if (param == nullptr)
	{
	  Error error (lexer.peek_token ()->get_locus (),
		       "failed to parse function param (in function params)");
	  add_error (std::move (error));

	  // skip somewhere?
	  return std::vector<std::unique_ptr<AST::Param>> ();
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
std::unique_ptr<AST::Param>
Parser<ManagedTokenSource>::parse_function_param ()
{
  // parse outer attributes if they exist
  AST::AttrVec outer_attrs = parse_outer_attributes ();

  // TODO: should saved location be at start of outer attributes or pattern?
  location_t locus = lexer.peek_token ()->get_locus ();

  if (lexer.peek_token ()->get_id () == ELLIPSIS) // Unnamed variadic
    {
      lexer.skip_token (); // Skip ellipsis
      return std::make_unique<AST::VariadicParam> (
	AST::VariadicParam (std::move (outer_attrs), locus));
    }

  std::unique_ptr<AST::Pattern> param_pattern = parse_pattern ();

  // create error function param if it doesn't exist
  if (param_pattern == nullptr)
    {
      // skip after something
      return nullptr;
    }

  if (!skip_token (COLON))
    {
      // skip after something
      return nullptr;
    }

  if (lexer.peek_token ()->get_id () == ELLIPSIS) // Named variadic
    {
      lexer.skip_token (); // Skip ellipsis
      return std::make_unique<AST::VariadicParam> (
	AST::VariadicParam (std::move (param_pattern), std::move (outer_attrs),
			    locus));
    }
  else
    {
      std::unique_ptr<AST::Type> param_type = parse_type ();
      if (param_type == nullptr)
	{
	  return nullptr;
	}
      return std::make_unique<AST::FunctionParam> (
	AST::FunctionParam (std::move (param_pattern), std::move (param_type),
			    std::move (outer_attrs), locus));
    }
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

  std::vector<AST::LifetimeParam> for_lifetimes;
  if (lexer.peek_token ()->get_id () == FOR)
    for_lifetimes = parse_for_lifetimes ();

  /* HACK: where clauses end with a right curly or semicolon or equals in all
   * uses currently */
  const_TokenPtr t = lexer.peek_token ();
  while (t->get_id () != LEFT_CURLY && t->get_id () != SEMICOLON
	 && t->get_id () != EQUAL)
    {
      std::unique_ptr<AST::WhereClauseItem> where_clause_item
	= parse_where_clause_item (for_lifetimes);

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
Parser<ManagedTokenSource>::parse_where_clause_item (
  const std::vector<AST::LifetimeParam> &outer_for_lifetimes)
{
  // shitty cheat way of determining lifetime or type bound - test for
  // lifetime
  const_TokenPtr t = lexer.peek_token ();

  if (t->get_id () == LIFETIME)
    return parse_lifetime_where_clause_item ();
  else
    return parse_type_bound_where_clause_item (outer_for_lifetimes);
}

// Parses a lifetime where clause item.
template <typename ManagedTokenSource>
std::unique_ptr<AST::LifetimeWhereClauseItem>
Parser<ManagedTokenSource>::parse_lifetime_where_clause_item ()
{
  auto parsed_lifetime = parse_lifetime (false);
  if (!parsed_lifetime)
    {
      // TODO: error here?
      return nullptr;
    }
  auto lifetime = parsed_lifetime.value ();

  if (!skip_token (COLON))
    {
      // TODO: skip after somewhere
      return nullptr;
    }

  std::vector<AST::Lifetime> lifetime_bounds = parse_lifetime_bounds ();
  // TODO: have end token passed in?

  location_t locus = lifetime.get_locus ();

  return std::unique_ptr<AST::LifetimeWhereClauseItem> (
    new AST::LifetimeWhereClauseItem (std::move (lifetime),
				      std::move (lifetime_bounds), locus));
}

// Parses a type bound where clause item.
template <typename ManagedTokenSource>
std::unique_ptr<AST::TypeBoundWhereClauseItem>
Parser<ManagedTokenSource>::parse_type_bound_where_clause_item (
  const std::vector<AST::LifetimeParam> &outer_for_lifetimes)
{
  std::vector<AST::LifetimeParam> for_lifetimes = outer_for_lifetimes;

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

  if (lexer.peek_token ()->get_id () == FOR)
    {
      auto for_lifetimes_inner = parse_for_lifetimes ();
      for_lifetimes.insert (for_lifetimes.end (), for_lifetimes_inner.begin (),
			    for_lifetimes_inner.end ());
    }

  // parse type param bounds if they exist
  std::vector<std::unique_ptr<AST::TypeParamBound>> type_param_bounds
    = parse_type_param_bounds ();

  location_t locus = lexer.peek_token ()->get_locus ();

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
  params = parse_lifetime_params_objs (Parse::Utils::is_right_angle_tok);

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
	new AST::Lifetime (parse_lifetime (false).value ()));
    case LEFT_PAREN:
    case QUESTION_MARK:
    case FOR:
    case IDENTIFIER:
    case SUPER:
    case SELF:
    case SELF_ALIAS:
    case CRATE:
    case DOLLAR_SIGN:
    case SCOPE_RESOLUTION:
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

  location_t locus = lexer.peek_token ()->get_locus ();

  /* parse optional `for lifetimes`. */
  std::vector<AST::LifetimeParam> for_lifetimes;
  if (lexer.peek_token ()->get_id () == FOR)
    for_lifetimes = parse_for_lifetimes ();

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
      auto lifetime = parse_lifetime (false);

      // quick exit for parsing failure
      if (!lifetime)
	break;

      lifetime_bounds.push_back (std::move (lifetime.value ()));

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
      auto lifetime = parse_lifetime (false);

      if (!lifetime)
	{
	  /* TODO: is it worth throwing away all lifetime bound info just
	   * because one failed? */
	  Error error (lexer.peek_token ()->get_locus (),
		       "failed to parse lifetime in lifetime bounds");
	  add_error (std::move (error));

	  return {};
	}

      lifetime_bounds.push_back (std::move (lifetime.value ()));

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
tl::expected<AST::Lifetime, ParseLifetimeError>
Parser<ManagedTokenSource>::parse_lifetime (bool allow_elided)
{
  const_TokenPtr lifetime_tok = lexer.peek_token ();
  if (lifetime_tok->get_id () != LIFETIME)
    {
      if (allow_elided)
	{
	  return AST::Lifetime::elided ();
	}
      else
	{
	  return tl::make_unexpected<ParseLifetimeError> ({});
	}
    }
  lexer.skip_token ();

  return lifetime_from_token (lifetime_tok);
}

template <typename ManagedTokenSource>
AST::Lifetime
Parser<ManagedTokenSource>::lifetime_from_token (const_TokenPtr tok)
{
  location_t locus = tok->get_locus ();
  std::string lifetime_ident = tok->get_str ();

  if (lifetime_ident == "static")
    {
      return AST::Lifetime (AST::Lifetime::STATIC, "", locus);
    }
  else if (lifetime_ident == "_")
    {
      // Explicitly and implicitly elided lifetimes follow the same rules.
      return AST::Lifetime (AST::Lifetime::WILDCARD, "", locus);
    }
  else
    {
      return AST::Lifetime (AST::Lifetime::NAMED, std::move (lifetime_ident),
			    locus);
    }
}

template <typename ManagedTokenSource>
std::unique_ptr<AST::ExternalTypeItem>
Parser<ManagedTokenSource>::parse_external_type_item (AST::Visibility vis,
						      AST::AttrVec outer_attrs)
{
  location_t locus = lexer.peek_token ()->get_locus ();
  skip_token (TYPE);

  const_TokenPtr alias_name_tok = expect_token (IDENTIFIER);
  if (alias_name_tok == nullptr)
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "could not parse identifier in external opaque type");
      add_error (std::move (error));

      skip_after_semicolon ();
      return nullptr;
    }

  if (!skip_token (SEMICOLON))
    return nullptr;

  return std::unique_ptr<AST::ExternalTypeItem> (
    new AST::ExternalTypeItem (alias_name_tok->get_str (), std::move (vis),
			       std::move (outer_attrs), std::move (locus)));
}

// Parses a "type alias" (typedef) item.
template <typename ManagedTokenSource>
std::unique_ptr<AST::TypeAlias>
Parser<ManagedTokenSource>::parse_type_alias (AST::Visibility vis,
					      AST::AttrVec outer_attrs)
{
  location_t locus = lexer.peek_token ()->get_locus ();
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
  Identifier alias_name{alias_name_tok};

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
  location_t locus = lexer.peek_token ()->get_locus ();
  skip_token (STRUCT_KW);

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
  Identifier struct_name{name_tok};

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
    case LEFT_CURLY:
      {
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
  auto vis = parse_visibility ();
  if (!vis)
    return AST::StructField::create_error ();

  location_t locus = lexer.peek_token ()->get_locus ();

  // parse field name
  const_TokenPtr field_name_tok = lexer.peek_token ();
  if (field_name_tok->get_id () != IDENTIFIER)
    {
      // if not identifier, assumes there is no struct field and exits - not
      // necessarily error
      return AST::StructField::create_error ();
    }
  Identifier field_name{field_name_tok};
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
			   std::move (vis.value ()), locus,
			   std::move (outer_attrs));
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
  auto visibility = parse_visibility ();
  if (!visibility)
    return AST::TupleField::create_error ();

  location_t locus = lexer.peek_token ()->get_locus ();

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

  return AST::TupleField (std::move (field_type),
			  std::move (visibility.value ()), locus,
			  std::move (outer_attrs));
}

// Parses a Rust "enum" tagged union item definition.
template <typename ManagedTokenSource>
std::unique_ptr<AST::Enum>
Parser<ManagedTokenSource>::parse_enum (AST::Visibility vis,
					AST::AttrVec outer_attrs)
{
  location_t locus = lexer.peek_token ()->get_locus ();
  skip_token (ENUM_KW);

  // parse enum name
  const_TokenPtr enum_name_tok = expect_token (IDENTIFIER);
  if (enum_name_tok == nullptr)
    return nullptr;

  Identifier enum_name = {enum_name_tok};

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
  auto vis_res = parse_visibility ();
  if (!vis_res)
    return nullptr;
  auto vis = vis_res.value ();

  // parse name for enum item, which is required
  const_TokenPtr item_name_tok = lexer.peek_token ();
  if (item_name_tok->get_id () != IDENTIFIER)
    {
      // this may not be an error but it means there is no enum item here
      return nullptr;
    }
  lexer.skip_token ();
  Identifier item_name{item_name_tok};

  // branch based on next token
  const_TokenPtr t = lexer.peek_token ();
  switch (t->get_id ())
    {
    case LEFT_PAREN:
      {
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
    case LEFT_CURLY:
      {
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
    case EQUAL:
      {
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
  rust_assert (union_keyword->get_str () == Values::WeakKeywords::UNION);
  location_t locus = union_keyword->get_locus ();

  // parse actual union name
  const_TokenPtr union_name_tok = expect_token (IDENTIFIER);
  if (union_name_tok == nullptr)
    {
      skip_after_next_block ();
      return nullptr;
    }
  Identifier union_name{union_name_tok};

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
  location_t locus = lexer.peek_token ()->get_locus ();
  skip_token (CONST);

  /* get constant identifier - this is either a proper identifier or the _
   * wildcard */
  const_TokenPtr ident_tok = lexer.peek_token ();
  // make default identifier the underscore wildcard one
  std::string ident (Values::Keywords::UNDERSCORE);
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

  // A const with no given expression value
  if (lexer.peek_token ()->get_id () == SEMICOLON)
    {
      lexer.skip_token ();
      return std::unique_ptr<AST::ConstantItem> (
	new AST::ConstantItem (std::move (ident), std::move (vis),
			       std::move (type), std::move (outer_attrs),
			       locus));
    }

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
  location_t locus = lexer.peek_token ()->get_locus ();
  skip_token (STATIC_KW);

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

  Identifier ident{ident_tok};

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
  location_t locus = lexer.peek_token ()->get_locus ();
  bool is_unsafe = false;
  bool is_auto_trait = false;

  if (lexer.peek_token ()->get_id () == UNSAFE)
    {
      is_unsafe = true;
      lexer.skip_token ();
    }

  if (lexer.peek_token ()->get_id () == AUTO)
    {
      is_auto_trait = true;
      lexer.skip_token ();
    }

  skip_token (TRAIT);

  // parse trait name
  const_TokenPtr ident_tok = expect_token (IDENTIFIER);
  if (ident_tok == nullptr)
    return nullptr;

  Identifier ident{ident_tok};

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
  std::vector<std::unique_ptr<AST::AssociatedItem>> trait_items;

  const_TokenPtr t = lexer.peek_token ();
  while (t->get_id () != RIGHT_CURLY)
    {
      std::unique_ptr<AST::AssociatedItem> trait_item = parse_trait_item ();

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
    new AST::Trait (std::move (ident), is_unsafe, is_auto_trait,
		    std::move (generic_params), std::move (type_param_bounds),
		    std::move (where_clause), std::move (trait_items),
		    std::move (vis), std::move (outer_attrs),
		    std::move (inner_attrs), locus));
}

// Parses a trait item used inside traits (not trait, the Item).
template <typename ManagedTokenSource>
std::unique_ptr<AST::AssociatedItem>
Parser<ManagedTokenSource>::parse_trait_item ()
{
  // parse outer attributes (if they exist)
  AST::AttrVec outer_attrs = parse_outer_attributes ();

  auto vis_res = parse_visibility ();
  if (!vis_res)
    return nullptr;

  auto vis = vis_res.value ();

  // lookahead to determine what type of trait item to parse
  const_TokenPtr tok = lexer.peek_token ();
  switch (tok->get_id ())
    {
    case SUPER:
    case SELF:
    case CRATE:
    case DOLLAR_SIGN:
      // these seem to be SimplePath tokens, so this is a macro invocation
      // semi
      return parse_macro_invocation_semi (std::move (outer_attrs));
    case IDENTIFIER:
      if (lexer.peek_token ()->get_str () == Values::WeakKeywords::DEFAULT)
	return parse_function (std::move (vis), std::move (outer_attrs));
      else
	return parse_macro_invocation_semi (std::move (outer_attrs));
    case TYPE:
      return parse_trait_type (std::move (outer_attrs), vis);
    case CONST:
      // disambiguate with function qualifier
      if (lexer.peek_token (1)->get_id () == IDENTIFIER)
	{
	  return parse_trait_const (std::move (outer_attrs));
	}
      // else, fallthrough to function
      // TODO: find out how to disable gcc "implicit fallthrough" error
      gcc_fallthrough ();
    case ASYNC:
    case UNSAFE:
    case EXTERN_KW:
    case FN_KW:
      return parse_function (std::move (vis), std::move (outer_attrs));
    default:
      break;
    }
  add_error (Error (tok->get_locus (),
		    "unrecognised token %qs for item in trait",
		    tok->get_token_description ()));
  // skip?
  return nullptr;
}

// Parse a typedef trait item.
template <typename ManagedTokenSource>
std::unique_ptr<AST::TraitItemType>
Parser<ManagedTokenSource>::parse_trait_type (AST::AttrVec outer_attrs,
					      AST::Visibility vis)
{
  location_t locus = lexer.peek_token ()->get_locus ();
  skip_token (TYPE);

  const_TokenPtr ident_tok = expect_token (IDENTIFIER);
  if (ident_tok == nullptr)
    return nullptr;

  Identifier ident{ident_tok};

  // Parse optional generic parameters for GATs (Generic Associated Types)
  std::vector<std::unique_ptr<AST::GenericParam>> generic_params;
  if (lexer.peek_token ()->get_id () == LEFT_ANGLE)
    {
      generic_params = parse_generic_params_in_angles ();
    }

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
    new AST::TraitItemType (std::move (ident), std::move (generic_params),
			    std::move (bounds), std::move (outer_attrs), vis,
			    locus));
}

// Parses a constant trait item.
template <typename ManagedTokenSource>
std::unique_ptr<AST::ConstantItem>
Parser<ManagedTokenSource>::parse_trait_const (AST::AttrVec outer_attrs)
{
  location_t locus = lexer.peek_token ()->get_locus ();
  skip_token (CONST);

  // parse constant item name
  const_TokenPtr ident_tok = expect_token (IDENTIFIER);
  if (ident_tok == nullptr)
    return nullptr;

  Identifier ident{ident_tok};

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

  return std::unique_ptr<AST::ConstantItem> (new AST::ConstantItem (
    std::move (ident), AST::Visibility::create_private (), std::move (type),
    std::move (const_body), std::move (outer_attrs), locus));
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
  location_t locus = lexer.peek_token ()->get_locus ();
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
      std::vector<std::unique_ptr<AST::AssociatedItem>> impl_items;

      const_TokenPtr t = lexer.peek_token ();
      while (t->get_id () != RIGHT_CURLY)
	{
	  std::unique_ptr<AST::AssociatedItem> impl_item
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
      std::vector<std::unique_ptr<AST::AssociatedItem>> impl_items;

      const_TokenPtr t = lexer.peek_token ();
      while (t->get_id () != RIGHT_CURLY)
	{
	  std::unique_ptr<AST::AssociatedItem> impl_item
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
std::unique_ptr<AST::AssociatedItem>
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
    case PUB:
      {
	// visibility, so not a macro invocation semi - must be constant,
	// function, or method
	auto vis_res = parse_visibility ();
	if (!vis_res)
	  return nullptr;
	auto vis = vis_res.value ();

	// TODO: is a recursive call to parse_inherent_impl_item better?
	switch (lexer.peek_token ()->get_id ())
	  {
	  case EXTERN_KW:
	  case UNSAFE:
	  case FN_KW:
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
	      case EXTERN_KW:
	      case FN_KW:
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
    case ASYNC:
    case EXTERN_KW:
    case UNSAFE:
    case FN_KW:
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
	case EXTERN_KW:
	case FN_KW:
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
      rust_unreachable ();
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
std::unique_ptr<AST::AssociatedItem>
Parser<ManagedTokenSource>::parse_inherent_impl_function_or_method (
  AST::Visibility vis, AST::AttrVec outer_attrs)
{
  location_t locus = lexer.peek_token ()->get_locus ();
  // parse function or method qualifiers
  AST::FunctionQualifiers qualifiers = parse_function_qualifiers ();

  skip_token (FN_KW);

  // parse function or method name
  const_TokenPtr ident_tok = expect_token (IDENTIFIER);
  if (ident_tok == nullptr)
    return nullptr;

  Identifier ident{ident_tok};

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
  auto initial_param = parse_self_param ();

  if (!initial_param.has_value ()
      && initial_param.error () != ParseSelfError::NOT_SELF)
    return nullptr;

  /* FIXME: ensure that self param doesn't accidently consume tokens for a
   * function one idea is to lookahead up to 4 tokens to see whether self is
   * one of them */
  bool is_method = false;
  if (initial_param.has_value ())
    {
      if ((*initial_param)->is_self ())
	is_method = true;

      /* skip comma so function and method regular params can be parsed in
       * same way */
      if (lexer.peek_token ()->get_id () == COMMA)
	lexer.skip_token ();
    }

  // parse trait function params
  std::vector<std::unique_ptr<AST::Param>> function_params
    = parse_function_params ([] (TokenId id) { return id == RIGHT_PAREN; });

  if (initial_param.has_value ())
    function_params.insert (function_params.begin (),
			    std::move (*initial_param));

  if (!skip_token (RIGHT_PAREN))
    {
      skip_after_end_block ();
      return nullptr;
    }

  // parse return type (optional)
  std::unique_ptr<AST::Type> return_type = parse_function_return_type ();

  // parse where clause (optional)
  AST::WhereClause where_clause = parse_where_clause ();

  tl::optional<std::unique_ptr<AST::BlockExpr>> body = tl::nullopt;
  if (lexer.peek_token ()->get_id () == SEMICOLON)
    lexer.skip_token ();
  else
    {
      auto result = parse_block_expr ();

      if (result == nullptr)
	{
	  Error error (
	    lexer.peek_token ()->get_locus (),
	    "could not parse definition in inherent impl %s definition",
	    is_method ? "method" : "function");
	  add_error (std::move (error));

	  skip_after_end_block ();
	  return nullptr;
	}
      body = std::move (result);
    }

  return std::unique_ptr<AST::Function> (
    new AST::Function (std::move (ident), std::move (qualifiers),
		       std::move (generic_params), std::move (function_params),
		       std::move (return_type), std::move (where_clause),
		       std::move (body), std::move (vis),
		       std::move (outer_attrs), locus));
}

// Parses a single trait impl item (item inside a trait impl block).
template <typename ManagedTokenSource>
std::unique_ptr<AST::AssociatedItem>
Parser<ManagedTokenSource>::parse_trait_impl_item ()
{
  // parse outer attributes (if they exist)
  AST::AttrVec outer_attrs = parse_outer_attributes ();

  auto vis_res = parse_visibility ();
  if (!vis_res)
    return nullptr;
  auto visibility = vis_res.value ();

  // branch on next token:
  const_TokenPtr t = lexer.peek_token ();
  switch (t->get_id ())
    {
    case SUPER:
    case SELF:
    case CRATE:
    case DOLLAR_SIGN:
      // these seem to be SimplePath tokens, so this is a macro invocation
      // semi
      return parse_macro_invocation_semi (std::move (outer_attrs));
    case IDENTIFIER:
      if (lexer.peek_token ()->get_str () == Values::WeakKeywords::DEFAULT)
	return parse_trait_impl_function_or_method (visibility,
						    std::move (outer_attrs));
      else
	return parse_macro_invocation_semi (std::move (outer_attrs));
    case TYPE:
      return parse_type_alias (visibility, std::move (outer_attrs));
    case EXTERN_KW:
    case UNSAFE:
    case FN_KW:
      // function or method
      return parse_trait_impl_function_or_method (visibility,
						  std::move (outer_attrs));
    case ASYNC:
      return parse_async_item (visibility, std::move (outer_attrs));
    case CONST:
      // lookahead to resolve production - could be function/method or const
      // item
      t = lexer.peek_token (1);

      switch (t->get_id ())
	{
	case IDENTIFIER:
	case UNDERSCORE:
	  return parse_const_item (visibility, std::move (outer_attrs));
	case UNSAFE:
	case EXTERN_KW:
	case FN_KW:
	  return parse_trait_impl_function_or_method (visibility,
						      std::move (outer_attrs));
	default:
	  add_error (Error (
	    t->get_locus (),
	    "unexpected token %qs in some sort of const item in trait impl",
	    t->get_token_description ()));

	  lexer.skip_token (1); // TODO: is this right thing to do?
	  return nullptr;
	}
      rust_unreachable ();
    default:
      break;
    }
  add_error (Error (t->get_locus (),
		    "unrecognised token %qs for item in trait impl",
		    t->get_token_description ()));

  // skip?
  return nullptr;
}

/* For internal use only by parse_trait_impl_item() - splits giant method into
 * smaller ones and prevents duplication of logic. Strictly, this parses a
 * function or method item inside a trait impl item block. */
template <typename ManagedTokenSource>
std::unique_ptr<AST::AssociatedItem>
Parser<ManagedTokenSource>::parse_trait_impl_function_or_method (
  AST::Visibility vis, AST::AttrVec outer_attrs)
{
  // this shares virtually all logic with
  // parse_inherent_impl_function_or_method
  // - template?
  location_t locus = lexer.peek_token ()->get_locus ();

  auto is_default = false;
  auto t = lexer.peek_token ();
  if (t->get_id () == IDENTIFIER
      && t->get_str () == Values::WeakKeywords::DEFAULT)
    {
      is_default = true;
      lexer.skip_token ();
    }

  // parse function or method qualifiers
  AST::FunctionQualifiers qualifiers = parse_function_qualifiers ();

  skip_token (FN_KW);

  // parse function or method name
  const_TokenPtr ident_tok = expect_token (IDENTIFIER);
  if (ident_tok == nullptr)
    {
      return nullptr;
    }
  Identifier ident{ident_tok};

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
  auto initial_param = parse_self_param ();

  if (!initial_param.has_value ()
      && initial_param.error () != ParseSelfError::NOT_SELF)
    return nullptr;

  // FIXME: ensure that self param doesn't accidently consume tokens for a
  // function
  bool is_method = false;
  if (initial_param.has_value ())
    {
      if ((*initial_param)->is_self ())
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
  std::vector<std::unique_ptr<AST::Param>> function_params;
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

  if (initial_param.has_value ())
    function_params.insert (function_params.begin (),
			    std::move (*initial_param));

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
  tl::optional<std::unique_ptr<AST::BlockExpr>> body = tl::nullopt;

  if (lexer.peek_token ()->get_id () == SEMICOLON)
    lexer.skip_token ();
  else
    {
      auto result = parse_block_expr ();
      if (result == nullptr)
	{
	  Error error (lexer.peek_token ()->get_locus (),
		       "could not parse definition in trait impl %s definition",
		       is_method ? "method" : "function");
	  add_error (std::move (error));

	  skip_after_end_block ();
	  return nullptr;
	}
      body = std::move (result);
    }

  return std::unique_ptr<AST::Function> (
    new AST::Function (std::move (ident), std::move (qualifiers),
		       std::move (generic_params), std::move (function_params),
		       std::move (return_type), std::move (where_clause),
		       std::move (body), std::move (vis),
		       std::move (outer_attrs), locus, is_default));
}

// Parses an extern block of declarations.
template <typename ManagedTokenSource>
std::unique_ptr<AST::ExternBlock>
Parser<ManagedTokenSource>::parse_extern_block (AST::Visibility vis,
						AST::AttrVec outer_attrs)
{
  location_t locus = lexer.peek_token ()->get_locus ();
  skip_token (EXTERN_KW);

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

  location_t locus = lexer.peek_token ()->get_locus ();

  // parse optional visibility
  auto vis_res = parse_visibility ();
  if (!vis_res)
    return nullptr;
  auto vis = vis_res.value ();

  const_TokenPtr t = lexer.peek_token ();
  switch (t->get_id ())
    {
    case IDENTIFIER:
      return parse_macro_invocation_semi (outer_attrs);
    case STATIC_KW:
      {
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
	Identifier ident{ident_tok};

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
    case FN_KW:
      return parse_function (std::move (vis), std::move (outer_attrs), true);

    case TYPE:
      return parse_external_type_item (std::move (vis),
				       std::move (outer_attrs));
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
    case EXTERN_KW:
    case USE:
    case FN_KW:
    case TYPE:
    case STRUCT_KW:
    case ENUM_KW:
    case CONST:
    case STATIC_KW:
    case AUTO:
    case TRAIT:
    case IMPL:
    case MACRO:
    /* TODO: implement union keyword but not really because of
     * context-dependence crappy hack way to parse a union written below to
     * separate it from the good code. */
    // case UNION:
    case UNSAFE: // maybe - unsafe traits are a thing
      /* if any of these (should be all possible VisItem prefixes), parse a
       * VisItem can't parse item because would require reparsing outer
       * attributes */
      // may also be unsafe block
      if (lexer.peek_token (1)->get_id () == LEFT_CURLY)
	{
	  return parse_expr_stmt (std::move (outer_attrs), restrictions);
	}
      else
	{
	  return parse_vis_item (std::move (outer_attrs));
	}
      break;
    // crappy hack to do union "keyword"
    case IDENTIFIER:
      if (t->get_str () == Values::WeakKeywords::UNION
	  && lexer.peek_token (1)->get_id () == IDENTIFIER)
	{
	  return parse_vis_item (std::move (outer_attrs));
	  // or should this go straight to parsing union?
	}
      else if (is_macro_rules_def (t))
	{
	  // macro_rules! macro item
	  return parse_macro_rules_def (std::move (outer_attrs));
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
  location_t locus = lexer.peek_token ()->get_locus ();
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
	  skip_after_semicolon ();
	  return nullptr;
	}
    }

  tl::optional<std::unique_ptr<AST::Expr>> else_expr = tl::nullopt;
  if (maybe_skip_token (ELSE))
    else_expr = parse_block_expr ();

  if (restrictions.consume_semi)
    {
      // `stmt` macro variables are parsed without a semicolon, but should be
      // parsed as a full statement when interpolated. This should be handled
      // by having the interpolated statement be distinguishable from normal
      // tokens, e.g. by NT tokens.
      if (restrictions.allow_close_after_expr_stmt)
	maybe_skip_token (SEMICOLON);
      else if (!skip_token (SEMICOLON))
	return nullptr;
    }

  return std::unique_ptr<AST::LetStmt> (
    new AST::LetStmt (std::move (pattern), std::move (expr), std::move (type),
		      std::move (else_expr), std::move (outer_attrs), locus));
}

template <typename ManagedTokenSource>
tl::optional<AST::GenericArg>
Parser<ManagedTokenSource>::parse_generic_arg ()
{
  auto tok = lexer.peek_token ();
  std::unique_ptr<AST::Expr> expr = nullptr;

  switch (tok->get_id ())
    {
    case IDENTIFIER:
      {
	// This is a bit of a weird situation: With an identifier token, we
	// could either have a valid type or a macro (FIXME: anything else?). So
	// we need one bit of lookahead to differentiate if this is really
	auto next_tok = lexer.peek_token (1);
	if (next_tok->get_id () == LEFT_ANGLE
	    || next_tok->get_id () == SCOPE_RESOLUTION
	    || next_tok->get_id () == EXCLAM)
	  {
	    auto type = parse_type ();
	    if (type)
	      return AST::GenericArg::create_type (std::move (type));
	    else
	      return tl::nullopt;
	  }
	else if (next_tok->get_id () == COLON)
	  {
	    lexer.skip_token (); // skip ident
	    lexer.skip_token (); // skip colon

	    auto tok = lexer.peek_token ();
	    std::vector<std::unique_ptr<AST::TypeParamBound>> bounds
	      = parse_type_param_bounds ();

	    auto type = std::unique_ptr<AST::TraitObjectType> (
	      new AST::TraitObjectType (std::move (bounds), tok->get_locus (),
					false));
	    if (type)
	      return AST::GenericArg::create_type (std::move (type));
	    else
	      return tl::nullopt;
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
    default:
      {
	auto type = parse_type ();
	// FIXME: Find a better way to do this?
	if (type)
	  return AST::GenericArg::create_type (std::move (type));
	else
	  return tl::nullopt;
      }
    }

  if (!expr)
    return tl::nullopt;

  return AST::GenericArg::create_const (std::move (expr));
}

// Parses the generic arguments in each path segment.
template <typename ManagedTokenSource>
AST::GenericArgs
Parser<ManagedTokenSource>::parse_path_generic_args ()
{
  if (lexer.peek_token ()->get_id () == LEFT_SHIFT)
    lexer.split_current_token (LEFT_ANGLE, LEFT_ANGLE);

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
  location_t locus = t->get_locus ();
  while (!Parse::Utils::is_right_angle_tok (t->get_id ()))
    {
      auto lifetime = parse_lifetime (false);
      if (!lifetime)
	{
	  // not necessarily an error
	  break;
	}

      lifetime_args.push_back (std::move (lifetime.value ()));

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
  while (!Parse::Utils::is_right_angle_tok (t->get_id ()))
    {
      // FIXME: Is it fine to break if there is one binding? Can't there be
      // bindings in between types?

      // ensure not binding being parsed as type accidently
      if (t->get_id () == IDENTIFIER
	  && lexer.peek_token (1)->get_id () == EQUAL)
	break;

      auto arg = parse_generic_arg ();
      if (arg)
	{
	  generic_args.emplace_back (std::move (arg.value ()));
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
  while (!Parse::Utils::is_right_angle_tok (t->get_id ()))
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
  Identifier ident{ident_tok};

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

// Parses a self param. Also handles self param not existing.
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::Param>, ParseSelfError>
Parser<ManagedTokenSource>::parse_self_param ()
{
  bool has_reference = false;
  AST::Lifetime lifetime = AST::Lifetime::elided ();

  location_t locus = lexer.peek_token ()->get_locus ();

  // TODO: Feels off, find a better way to clearly express this
  std::vector<std::vector<TokenId>> ptrs
    = {{ASTERISK, SELF} /* *self */,
       {ASTERISK, CONST, SELF} /* *const self */,
       {ASTERISK, MUT, SELF} /* *mut self */};

  for (auto &s : ptrs)
    {
      size_t i = 0;
      for (i = 0; i < s.size (); i++)
	if (lexer.peek_token (i)->get_id () != s[i])
	  break;
      if (i == s.size ())
	{
	  rust_error_at (lexer.peek_token ()->get_locus (),
			 "cannot pass %<self%> by raw pointer");
	  return tl::make_unexpected (ParseSelfError::SELF_PTR);
	}
    }

  // Trying to find those patterns:
  //
  // &'lifetime mut self
  // &'lifetime self
  // & mut self
  // & self
  // mut self
  // self
  //
  // If not found, it is probably a function, exit and let function parsing
  // handle it.
  bool is_self = false;
  for (size_t i = 0; i < 5; i++)
    if (lexer.peek_token (i)->get_id () == SELF)
      is_self = true;

  if (!is_self)
    return tl::make_unexpected (ParseSelfError::NOT_SELF);

  // test if self is a reference parameter
  if (lexer.peek_token ()->get_id () == AMP)
    {
      has_reference = true;
      lexer.skip_token ();

      // now test whether it has a lifetime
      if (lexer.peek_token ()->get_id () == LIFETIME)
	{
	  // something went wrong somehow
	  if (auto parsed_lifetime = parse_lifetime (true))
	    {
	      lifetime = parsed_lifetime.value ();
	    }
	  else
	    {
	      Error error (lexer.peek_token ()->get_locus (),
			   "failed to parse lifetime in self param");
	      add_error (std::move (error));

	      // skip after somewhere?
	      return tl::make_unexpected (ParseSelfError::PARSING);
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
      return tl::make_unexpected (ParseSelfError::NOT_SELF);
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
	  return tl::make_unexpected (ParseSelfError::PARSING);
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
      return tl::make_unexpected (ParseSelfError::PARSING);
    }

  if (has_reference)
    {
      return std::make_unique<AST::SelfParam> (std::move (lifetime), has_mut,
					       locus);
    }
  else
    {
      // note that type may be nullptr here and that's fine
      return std::make_unique<AST::SelfParam> (std::move (type), has_mut,
					       locus);
    }
}

/* Parses an expression or macro statement. */
template <typename ManagedTokenSource>
std::unique_ptr<AST::Stmt>
Parser<ManagedTokenSource>::parse_expr_stmt (AST::AttrVec outer_attrs,
					     ParseRestrictions restrictions)
{
  location_t locus = lexer.peek_token ()->get_locus ();

  std::unique_ptr<AST::Expr> expr;

  switch (lexer.peek_token ()->get_id ())
    {
    case IDENTIFIER:
    case CRATE:
    case SUPER:
    case SELF:
    case SELF_ALIAS:
    case DOLLAR_SIGN:
    case SCOPE_RESOLUTION:
      {
	AST::PathInExpression path = parse_path_in_expression ();
	std::unique_ptr<AST::Expr> null_denotation;

	if (lexer.peek_token ()->get_id () == EXCLAM)
	  {
	    std::unique_ptr<AST::MacroInvocation> invoc
	      = parse_macro_invocation_partial (std::move (path),
						std::move (outer_attrs));

	    if (restrictions.consume_semi && maybe_skip_token (SEMICOLON))
	      {
		invoc->add_semicolon ();
		// Macro invocation with semicolon.
		return invoc;
	      }

	    TokenId after_macro = lexer.peek_token ()->get_id ();

	    if (restrictions.allow_close_after_expr_stmt
		&& (after_macro == RIGHT_PAREN || after_macro == RIGHT_CURLY
		    || after_macro == RIGHT_SQUARE))
	      return invoc;

	    if (invoc->get_invoc_data ().get_delim_tok_tree ().get_delim_type ()
		  == AST::CURLY
		&& after_macro != DOT && after_macro != QUESTION_MARK)
	      {
		rust_debug ("braced macro statement");
		return invoc;
	      }

	    null_denotation = std::move (invoc);
	  }
	else
	  {
	    null_denotation
	      = null_denotation_path (std::move (path), {}, restrictions);
	  }

	expr = left_denotations (std::move (null_denotation), LBP_LOWEST,
				 std::move (outer_attrs), restrictions);
	break;
      }
    default:
      restrictions.expr_can_be_stmt = true;
      expr = parse_expr (std::move (outer_attrs), restrictions);
      break;
    }

  if (expr == nullptr)
    {
      // expr is required, error
      Error error (lexer.peek_token ()->get_locus (),
		   "failed to parse expr in expr statement");
      add_error (std::move (error));

      skip_after_semicolon ();
      return nullptr;
    }

  bool has_semi = false;

  if (restrictions.consume_semi)
    {
      if (maybe_skip_token (SEMICOLON))
	{
	  has_semi = true;
	}
      else if (expr->is_expr_without_block ())
	{
	  if (restrictions.allow_close_after_expr_stmt)
	    {
	      TokenId id = lexer.peek_token ()->get_id ();
	      if (id != RIGHT_PAREN && id != RIGHT_CURLY && id != RIGHT_SQUARE)
		{
		  expect_token (SEMICOLON);
		  return nullptr;
		}
	    }
	  else
	    {
	      expect_token (SEMICOLON);
	      return nullptr;
	    }
	}
    }

  return std::unique_ptr<AST::ExprStmt> (
    new AST::ExprStmt (std::move (expr), locus, has_semi));
}

// Parses a loop label used in loop expressions.
template <typename ManagedTokenSource>
tl::expected<AST::LoopLabel, ParseLoopLabelError>
Parser<ManagedTokenSource>::parse_loop_label (const_TokenPtr tok)
{
  // parse lifetime - if doesn't exist, assume no label
  if (tok->get_id () != LIFETIME)
    {
      // not necessarily an error
      return tl::unexpected<ParseLoopLabelError> (
	ParseLoopLabelError::NOT_LOOP_LABEL);
    }
  /* FIXME: check for named lifetime requirement here? or check in semantic
   * analysis phase? */
  AST::Lifetime label = lifetime_from_token (tok);

  if (!skip_token (COLON))
    {
      // skip somewhere?
      return tl::unexpected<ParseLoopLabelError> (
	ParseLoopLabelError::MISSING_COLON);
    }

  return tl::expected<AST::LoopLabel, ParseLoopLabelError> (
    AST::LoopLabel (std::move (label), tok->get_locus ()));
}

// Parses the "pattern" part of the match arm (the 'case x:' equivalent).
template <typename ManagedTokenSource>
AST::MatchArm
Parser<ManagedTokenSource>::parse_match_arm ()
{
  // parse optional outer attributes
  AST::AttrVec outer_attrs = parse_outer_attributes ();

  // DEBUG
  rust_debug ("about to start parsing match arm patterns");

  // break early if find right curly
  if (lexer.peek_token ()->get_id () == RIGHT_CURLY)
    {
      // not an error
      return AST::MatchArm::create_error ();
    }

  // parse match arm patterns - at least 1 is required
  std::vector<std::unique_ptr<AST::Pattern>> match_arm_patterns
    = parse_match_arm_patterns (RIGHT_CURLY);
  if (match_arm_patterns.empty ())
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "failed to parse any patterns in match arm");
      add_error (std::move (error));

      // skip somewhere?
      return AST::MatchArm::create_error ();
    }

  // DEBUG
  rust_debug ("successfully parsed match arm patterns");

  // parse match arm guard expr if it exists
  std::unique_ptr<AST::Expr> guard_expr = nullptr;
  if (lexer.peek_token ()->get_id () == IF)
    {
      lexer.skip_token ();

      guard_expr = parse_expr ();
      if (guard_expr == nullptr)
	{
	  Error error (lexer.peek_token ()->get_locus (),
		       "failed to parse guard expression in match arm");
	  add_error (std::move (error));

	  // skip somewhere?
	  return AST::MatchArm::create_error ();
	}
    }

  // DEBUG
  rust_debug ("successfully parsed match arm");

  return AST::MatchArm (std::move (match_arm_patterns),
			lexer.peek_token ()->get_locus (),
			std::move (guard_expr), std::move (outer_attrs));
}

/* Parses the patterns used in a match arm. End token id is the id of the
 * token that would exist after the patterns are done (e.g. '}' for match
 * expr, '=' for if let and while let). */
template <typename ManagedTokenSource>
std::vector<std::unique_ptr<AST::Pattern>>
Parser<ManagedTokenSource>::parse_match_arm_patterns (TokenId end_token_id)
{
  // skip optional leading '|'
  if (lexer.peek_token ()->get_id () == PIPE)
    lexer.skip_token ();
  /* TODO: do I even need to store the result of this? can't be used.
   * If semantically different, I need a wrapped "match arm patterns" object
   * for this. */

  std::vector<std::unique_ptr<AST::Pattern>> patterns;

  // quick break out if end_token_id
  if (lexer.peek_token ()->get_id () == end_token_id)
    return patterns;

  // parse required pattern - if doesn't exist, return empty
  std::unique_ptr<AST::Pattern> initial_pattern = parse_pattern ();
  if (initial_pattern == nullptr)
    {
      // FIXME: should this be an error?
      return patterns;
    }
  patterns.push_back (std::move (initial_pattern));

  // DEBUG
  rust_debug ("successfully parsed initial match arm pattern");

  // parse new patterns as long as next char is '|'
  const_TokenPtr t = lexer.peek_token ();
  while (t->get_id () == PIPE)
    {
      // skip pipe token
      lexer.skip_token ();

      // break if hit end token id
      if (lexer.peek_token ()->get_id () == end_token_id)
	break;

      // parse pattern
      std::unique_ptr<AST::Pattern> pattern = parse_pattern ();
      if (pattern == nullptr)
	{
	  // this is an error
	  Error error (lexer.peek_token ()->get_locus (),
		       "failed to parse pattern in match arm patterns");
	  add_error (std::move (error));

	  // skip somewhere?
	  return {};
	}

      patterns.push_back (std::move (pattern));

      t = lexer.peek_token ();
    }

  patterns.shrink_to_fit ();

  return patterns;
}

// Parses a single parameter used in a closure definition.
template <typename ManagedTokenSource>
AST::ClosureParam
Parser<ManagedTokenSource>::parse_closure_param ()
{
  AST::AttrVec outer_attrs = parse_outer_attributes ();

  // parse pattern (which is required)
  std::unique_ptr<AST::Pattern> pattern = parse_pattern_no_alt ();
  if (pattern == nullptr)
    {
      // not necessarily an error
      return AST::ClosureParam::create_error ();
    }

  // parse optional type of param
  std::unique_ptr<AST::Type> type = nullptr;
  if (lexer.peek_token ()->get_id () == COLON)
    {
      lexer.skip_token ();

      // parse type, which is now required
      type = parse_type ();
      if (type == nullptr)
	{
	  Error error (lexer.peek_token ()->get_locus (),
		       "failed to parse type in closure parameter");
	  add_error (std::move (error));

	  // skip somewhere?
	  return AST::ClosureParam::create_error ();
	}
    }

  location_t loc = pattern->get_locus ();
  return AST::ClosureParam (std::move (pattern), loc, std::move (type),
			    std::move (outer_attrs));
}

// Parses a type (will further disambiguate any type).
template <typename ManagedTokenSource>
std::unique_ptr<AST::Type>
Parser<ManagedTokenSource>::parse_type (bool save_errors)
{
  /* rules for all types:
   * NeverType:               '!'
   * SliceType:               '[' Type ']'
   * InferredType:            '_'
   * MacroInvocation:         SimplePath '!' DelimTokenTree
   * ParenthesisedType:       '(' Type ')'
   * ImplTraitType:           'impl' TypeParamBounds
   *  TypeParamBounds (not type)  TypeParamBound ( '+' TypeParamBound )* '+'?
   *  TypeParamBound          Lifetime | TraitBound
   * ImplTraitTypeOneBound:   'impl' TraitBound
   * TraitObjectType:         'dyn'? TypeParamBounds
   * TraitObjectTypeOneBound: 'dyn'? TraitBound
   *  TraitBound              '?'? ForLifetimes? TypePath | '(' '?'?
   * ForLifetimes? TypePath ')' BareFunctionType:        ForLifetimes?
   * FunctionQualifiers 'fn' etc. ForLifetimes (not type) 'for' '<'
   * LifetimeParams '>' FunctionQualifiers      ( 'async' | 'const' )?
   * 'unsafe'?
   * ('extern' abi?)? QualifiedPathInType:     '<' Type ( 'as' TypePath )? '>'
   * (
   * '::' TypePathSegment )+ TypePath:                '::'? TypePathSegment (
   * '::' TypePathSegment)* ArrayType:               '[' Type ';' Expr ']'
   * ReferenceType:           '&' Lifetime? 'mut'? TypeNoBounds
   * RawPointerType:          '*' ( 'mut' | 'const' ) TypeNoBounds
   * TupleType:               '(' Type etc. - regular tuple stuff. Also
   * regular tuple vs parenthesised precedence
   *
   * Disambiguate between macro and type path via type path being parsed, and
   * then if '!' found, convert type path to simple path for macro. Usual
   * disambiguation for tuple vs parenthesised. For ImplTraitType and
   * TraitObjectType individual disambiguations, they seem more like "special
   * cases", so probably just try to parse the more general ImplTraitType or
   * TraitObjectType and return OneBound versions if they satisfy those
   * criteria. */

  const_TokenPtr t = lexer.peek_token ();
  switch (t->get_id ())
    {
    case EXCLAM:
      // never type - can't be macro as no path beforehand
      lexer.skip_token ();
      return std::unique_ptr<AST::NeverType> (
	new AST::NeverType (t->get_locus ()));
    case LEFT_SQUARE:
      // slice type or array type - requires further disambiguation
      return parse_slice_or_array_type ();
    case LEFT_SHIFT:
    case LEFT_ANGLE:
      {
	// qualified path in type
	AST::QualifiedPathInType path = parse_qualified_path_in_type ();
	if (path.is_error ())
	  {
	    if (save_errors)
	      {
		Error error (t->get_locus (),
			     "failed to parse qualified path in type");
		add_error (std::move (error));
	      }

	    return nullptr;
	  }
	return std::unique_ptr<AST::QualifiedPathInType> (
	  new AST::QualifiedPathInType (std::move (path)));
      }
    case UNDERSCORE:
      // inferred type
      lexer.skip_token ();
      return std::unique_ptr<AST::InferredType> (
	new AST::InferredType (t->get_locus ()));
    case ASTERISK:
      // raw pointer type
      return parse_raw_pointer_type ();
    case AMP: // does this also include AMP_AMP?
    case LOGICAL_AND:
      // reference type
      return parse_reference_type ();
    case LIFETIME:
      {
	/* probably a lifetime bound, so probably type param bounds in
	 * TraitObjectType */
	std::vector<std::unique_ptr<AST::TypeParamBound>> bounds
	  = parse_type_param_bounds ();

	return std::unique_ptr<AST::TraitObjectType> (
	  new AST::TraitObjectType (std::move (bounds), t->get_locus (),
				    false));
      }
    case IDENTIFIER:
    case SUPER:
    case SELF:
    case SELF_ALIAS:
    case CRATE:
    case DOLLAR_SIGN:
    case SCOPE_RESOLUTION:
      {
	// macro invocation or type path - requires further disambiguation.
	/* for parsing path component of each rule, perhaps parse it as a
	 * typepath and attempt conversion to simplepath if a trailing '!' is
	 * found */
	/* Type path also includes TraitObjectTypeOneBound BUT if it starts
	 * with it, it is exactly the same as a TypePath syntactically, so
	 * this is a syntactical ambiguity. As such, the parser will parse it
	 * as a TypePath. This, however, does not prevent TraitObjectType from
	 * starting with a typepath. */

	// parse path as type path
	AST::TypePath path = parse_type_path ();
	if (path.is_error ())
	  {
	    if (save_errors)
	      {
		Error error (t->get_locus (),
			     "failed to parse path as first component of type");
		add_error (std::move (error));
	      }

	    return nullptr;
	  }
	location_t locus = path.get_locus ();

	// branch on next token
	t = lexer.peek_token ();
	switch (t->get_id ())
	  {
	  case EXCLAM:
	    {
	      // macro invocation
	      // convert to simple path
	      AST::SimplePath macro_path = path.as_simple_path ();
	      if (macro_path.is_empty ())
		{
		  if (save_errors)
		    {
		      Error error (t->get_locus (),
				   "failed to parse simple path in macro "
				   "invocation (for type)");
		      add_error (std::move (error));
		    }

		  return nullptr;
		}

	      lexer.skip_token ();

	      auto tok_tree = parse_delim_token_tree ();
	      if (!tok_tree)
		return nullptr;

	      return AST::MacroInvocation::Regular (
		AST::MacroInvocData (std::move (macro_path),
				     std::move (tok_tree.value ())),
		{}, locus);
	    }
	  case PLUS:
	    {
	      // type param bounds
	      std::vector<std::unique_ptr<AST::TypeParamBound>> bounds;

	      // convert type path to trait bound
	      std::unique_ptr<AST::TraitBound> path_bound (
		new AST::TraitBound (std::move (path), locus, false, false));
	      bounds.push_back (std::move (path_bound));

	      /* parse rest of bounds - FIXME: better way to find when to stop
	       * parsing */
	      while (t->get_id () == PLUS)
		{
		  lexer.skip_token ();

		  // parse bound if it exists - if not, assume end of sequence
		  std::unique_ptr<AST::TypeParamBound> bound
		    = parse_type_param_bound ();
		  if (bound == nullptr)
		    {
		      break;
		    }
		  bounds.push_back (std::move (bound));

		  t = lexer.peek_token ();
		}

	      return std::unique_ptr<AST::TraitObjectType> (
		new AST::TraitObjectType (std::move (bounds), locus, false));
	    }
	  default:
	    // assume that this is a type path and not an error
	    return std::unique_ptr<AST::TypePath> (
	      new AST::TypePath (std::move (path)));
	  }
      }
    case LEFT_PAREN:
      /* tuple type or parenthesised type - requires further disambiguation
       * (the usual). ok apparently can be a parenthesised TraitBound too, so
       * could be TraitObjectTypeOneBound or TraitObjectType */
      return parse_paren_prefixed_type ();
    case FOR:
      // TraitObjectTypeOneBound or BareFunctionType
      return parse_for_prefixed_type ();
    case ASYNC:
    case CONST:
    case UNSAFE:
    case EXTERN_KW:
    case FN_KW:
      // bare function type (with no for lifetimes)
      return parse_bare_function_type (std::vector<AST::LifetimeParam> ());
    case IMPL:
      lexer.skip_token ();
      if (lexer.peek_token ()->get_id () == LIFETIME)
	{
	  /* cannot be one bound because lifetime prevents it from being
	   * traitbound */
	  std::vector<std::unique_ptr<AST::TypeParamBound>> bounds
	    = parse_type_param_bounds ();

	  return std::unique_ptr<AST::ImplTraitType> (
	    new AST::ImplTraitType (std::move (bounds), t->get_locus ()));
	}
      else
	{
	  // should be trait bound, so parse trait bound
	  std::unique_ptr<AST::TraitBound> initial_bound = parse_trait_bound ();
	  if (initial_bound == nullptr)
	    {
	      if (save_errors)
		{
		  Error error (lexer.peek_token ()->get_locus (),
			       "failed to parse ImplTraitType initial bound");
		  add_error (std::move (error));
		}

	      return nullptr;
	    }

	  location_t locus = t->get_locus ();

	  // short cut if next token isn't '+'
	  t = lexer.peek_token ();
	  if (t->get_id () != PLUS)
	    {
	      return std::unique_ptr<AST::ImplTraitTypeOneBound> (
		new AST::ImplTraitTypeOneBound (std::move (initial_bound),
						locus));
	    }

	  // parse additional type param bounds
	  std::vector<std::unique_ptr<AST::TypeParamBound>> bounds;
	  bounds.push_back (std::move (initial_bound));
	  while (t->get_id () == PLUS)
	    {
	      lexer.skip_token ();

	      // parse bound if it exists
	      std::unique_ptr<AST::TypeParamBound> bound
		= parse_type_param_bound ();
	      if (bound == nullptr)
		{
		  // not an error as trailing plus may exist
		  break;
		}
	      bounds.push_back (std::move (bound));

	      t = lexer.peek_token ();
	    }

	  return std::unique_ptr<AST::ImplTraitType> (
	    new AST::ImplTraitType (std::move (bounds), locus));
	}
    case DYN:
    case QUESTION_MARK:
      {
	// either TraitObjectType or TraitObjectTypeOneBound
	bool has_dyn = false;
	if (t->get_id () == DYN)
	  {
	    lexer.skip_token ();
	    has_dyn = true;
	  }

	if (lexer.peek_token ()->get_id () == LIFETIME)
	  {
	    /* cannot be one bound because lifetime prevents it from being
	     * traitbound */
	    std::vector<std::unique_ptr<AST::TypeParamBound>> bounds
	      = parse_type_param_bounds ();

	    return std::unique_ptr<AST::TraitObjectType> (
	      new AST::TraitObjectType (std::move (bounds), t->get_locus (),
					has_dyn));
	  }
	else
	  {
	    // should be trait bound, so parse trait bound
	    std::unique_ptr<AST::TraitBound> initial_bound
	      = parse_trait_bound ();
	    if (initial_bound == nullptr)
	      {
		if (save_errors)
		  {
		    Error error (
		      lexer.peek_token ()->get_locus (),
		      "failed to parse TraitObjectType initial bound");
		    add_error (std::move (error));
		  }

		return nullptr;
	      }

	    // short cut if next token isn't '+'
	    t = lexer.peek_token ();
	    if (t->get_id () != PLUS)
	      {
		// convert trait bound to value object
		AST::TraitBound value_bound (*initial_bound);

		// DEBUG: removed as unique ptr, so should auto delete
		// delete initial_bound;

		return std::unique_ptr<AST::TraitObjectTypeOneBound> (
		  new AST::TraitObjectTypeOneBound (std::move (value_bound),
						    t->get_locus (), has_dyn));
	      }

	    // parse additional type param bounds
	    std::vector<std::unique_ptr<AST::TypeParamBound>> bounds;
	    bounds.push_back (std::move (initial_bound));
	    while (t->get_id () == PLUS)
	      {
		lexer.skip_token ();

		// parse bound if it exists
		std::unique_ptr<AST::TypeParamBound> bound
		  = parse_type_param_bound ();
		if (bound == nullptr)
		  {
		    // not an error as trailing plus may exist
		    break;
		  }
		bounds.push_back (std::move (bound));

		t = lexer.peek_token ();
	      }

	    return std::unique_ptr<AST::TraitObjectType> (
	      new AST::TraitObjectType (std::move (bounds), t->get_locus (),
					has_dyn));
	  }
      }
    default:
      if (save_errors)
	add_error (Error (t->get_locus (), "unrecognised token %qs in type",
			  t->get_token_description ()));

      return nullptr;
    }
}

/* Parses a type that has '(' as its first character. Returns a tuple type,
 * parenthesised type, TraitObjectTypeOneBound, or TraitObjectType depending
 * on following characters. */
template <typename ManagedTokenSource>
std::unique_ptr<AST::Type>
Parser<ManagedTokenSource>::parse_paren_prefixed_type ()
{
  /* NOTE: Syntactical ambiguity of a parenthesised trait bound is considered
   * a trait bound, not a parenthesised type, so that it can still be used in
   * type param bounds. */

  /* NOTE: this implementation is really shit but I couldn't think of a better
   * one. It requires essentially breaking polymorphism and downcasting via
   * virtual method abuse, as it was copied from the rustc implementation (in
   * which types are reified due to tagged union), after a more OOP attempt by
   * me failed. */
  location_t left_delim_locus = lexer.peek_token ()->get_locus ();

  // skip left delim
  lexer.skip_token ();
  /* while next token isn't close delim, parse comma-separated types, saving
   * whether trailing comma happens */
  const_TokenPtr t = lexer.peek_token ();
  bool trailing_comma = true;
  std::vector<std::unique_ptr<AST::Type>> types;

  while (t->get_id () != RIGHT_PAREN)
    {
      std::unique_ptr<AST::Type> type = parse_type ();
      if (type == nullptr)
	{
	  Error error (t->get_locus (),
		       "failed to parse type inside parentheses (probably "
		       "tuple or parenthesised)");
	  add_error (std::move (error));

	  return nullptr;
	}
      types.push_back (std::move (type));

      t = lexer.peek_token ();
      if (t->get_id () != COMMA)
	{
	  trailing_comma = false;
	  break;
	}
      lexer.skip_token ();

      t = lexer.peek_token ();
    }

  if (!skip_token (RIGHT_PAREN))
    {
      return nullptr;
    }

  // if only one type and no trailing comma, then not a tuple type
  if (types.size () == 1 && !trailing_comma)
    {
      // must be a TraitObjectType (with more than one bound)
      if (lexer.peek_token ()->get_id () == PLUS)
	{
	  // create type param bounds vector
	  std::vector<std::unique_ptr<AST::TypeParamBound>> bounds;

	  // HACK: convert type to traitbound and add to bounds
	  std::unique_ptr<AST::Type> released_ptr = std::move (types[0]);
	  std::unique_ptr<AST::TraitBound> converted_bound (
	    released_ptr->to_trait_bound (true));
	  if (converted_bound == nullptr)
	    {
	      Error error (
		lexer.peek_token ()->get_locus (),
		"failed to hackily converted parsed type to trait bound");
	      add_error (std::move (error));

	      return nullptr;
	    }
	  bounds.push_back (std::move (converted_bound));

	  t = lexer.peek_token ();
	  while (t->get_id () == PLUS)
	    {
	      lexer.skip_token ();

	      // attempt to parse typeparambound
	      std::unique_ptr<AST::TypeParamBound> bound
		= parse_type_param_bound ();
	      if (bound == nullptr)
		{
		  // not an error if null
		  break;
		}
	      bounds.push_back (std::move (bound));

	      t = lexer.peek_token ();
	    }

	  return std::unique_ptr<AST::TraitObjectType> (
	    new AST::TraitObjectType (std::move (bounds), left_delim_locus,
				      false));
	}
      else
	{
	  // release vector pointer
	  std::unique_ptr<AST::Type> released_ptr = std::move (types[0]);
	  /* HACK: attempt to convert to trait bound. if fails, parenthesised
	   * type */
	  std::unique_ptr<AST::TraitBound> converted_bound (
	    released_ptr->to_trait_bound (true));
	  if (converted_bound == nullptr)
	    {
	      // parenthesised type
	      return std::unique_ptr<AST::ParenthesisedType> (
		new AST::ParenthesisedType (std::move (released_ptr),
					    left_delim_locus));
	    }
	  else
	    {
	      // trait object type (one bound)

	      // get value semantics trait bound
	      AST::TraitBound value_bound (*converted_bound);

	      return std::unique_ptr<AST::TraitObjectTypeOneBound> (
		new AST::TraitObjectTypeOneBound (value_bound,
						  left_delim_locus));
	    }
	}
    }
  else
    {
      return std::unique_ptr<AST::TupleType> (
	new AST::TupleType (std::move (types), left_delim_locus));
    }
  /* TODO: ensure that this ensures that dynamic dispatch for traits is not
   * lost somehow */
}

/* Parses a type that has 'for' as its first character. This means it has a
 * "for lifetimes", so returns either a BareFunctionType, TraitObjectType, or
 * TraitObjectTypeOneBound depending on following characters. */
template <typename ManagedTokenSource>
std::unique_ptr<AST::Type>
Parser<ManagedTokenSource>::parse_for_prefixed_type ()
{
  location_t for_locus = lexer.peek_token ()->get_locus ();
  // parse for lifetimes in type
  std::vector<AST::LifetimeParam> for_lifetimes = parse_for_lifetimes ();

  // branch on next token - either function or a trait type
  const_TokenPtr t = lexer.peek_token ();
  switch (t->get_id ())
    {
    case ASYNC:
    case CONST:
    case UNSAFE:
    case EXTERN_KW:
    case FN_KW:
      return parse_bare_function_type (std::move (for_lifetimes));
    case SCOPE_RESOLUTION:
    case IDENTIFIER:
    case SUPER:
    case SELF:
    case SELF_ALIAS:
    case CRATE:
    case DOLLAR_SIGN:
      {
	// path, so trait type

	// parse type path to finish parsing trait bound
	AST::TypePath path = parse_type_path ();

	t = lexer.peek_token ();
	if (t->get_id () != PLUS)
	  {
	    // must be one-bound trait type
	    // create trait bound value object
	    AST::TraitBound bound (std::move (path), for_locus, false, false,
				   std::move (for_lifetimes));

	    return std::unique_ptr<AST::TraitObjectTypeOneBound> (
	      new AST::TraitObjectTypeOneBound (std::move (bound), for_locus));
	  }

	/* more than one bound trait type (or at least parsed as it - could be
	 * trailing '+') create trait bound pointer and bounds */
	std::unique_ptr<AST::TraitBound> initial_bound (
	  new AST::TraitBound (std::move (path), for_locus, false, false,
			       std::move (for_lifetimes)));
	std::vector<std::unique_ptr<AST::TypeParamBound>> bounds;
	bounds.push_back (std::move (initial_bound));

	while (t->get_id () == PLUS)
	  {
	    lexer.skip_token ();

	    // parse type param bound if it exists
	    std::unique_ptr<AST::TypeParamBound> bound
	      = parse_type_param_bound ();
	    if (bound == nullptr)
	      {
		// not an error - e.g. trailing plus
		return nullptr;
	      }
	    bounds.push_back (std::move (bound));

	    t = lexer.peek_token ();
	  }

	return std::unique_ptr<AST::TraitObjectType> (
	  new AST::TraitObjectType (std::move (bounds), for_locus, false));
      }
    default:
      // error
      add_error (Error (t->get_locus (),
			"unrecognised token %qs in bare function type or trait "
			"object type or trait object type one bound",
			t->get_token_description ()));

      return nullptr;
    }
}

// Parses a maybe named param used in bare function types.
template <typename ManagedTokenSource>
AST::MaybeNamedParam
Parser<ManagedTokenSource>::parse_maybe_named_param (AST::AttrVec outer_attrs)
{
  /* Basically guess that param is named if first token is identifier or
   * underscore and second token is semicolon. This should probably have no
   * exceptions. rustc uses backtracking to parse these, but at the time of
   * writing gccrs has no backtracking capabilities. */
  const_TokenPtr current = lexer.peek_token ();
  const_TokenPtr next = lexer.peek_token (1);

  Identifier name;
  AST::MaybeNamedParam::ParamKind kind = AST::MaybeNamedParam::UNNAMED;

  if (current->get_id () == IDENTIFIER && next->get_id () == COLON)
    {
      // named param
      name = {current};
      kind = AST::MaybeNamedParam::IDENTIFIER;
      lexer.skip_token (1);
    }
  else if (current->get_id () == UNDERSCORE && next->get_id () == COLON)
    {
      // wildcard param
      name = {Values::Keywords::UNDERSCORE, current->get_locus ()};
      kind = AST::MaybeNamedParam::WILDCARD;
      lexer.skip_token (1);
    }

  // parse type (required)
  std::unique_ptr<AST::Type> type = parse_type ();
  if (type == nullptr)
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "failed to parse type in maybe named param");
      add_error (std::move (error));

      return AST::MaybeNamedParam::create_error ();
    }

  return AST::MaybeNamedParam (std::move (name), kind, std::move (type),
			       std::move (outer_attrs), current->get_locus ());
}

/* Parses a bare function type (with the given for lifetimes for convenience -
 * does not parse them itself). */
template <typename ManagedTokenSource>
std::unique_ptr<AST::BareFunctionType>
Parser<ManagedTokenSource>::parse_bare_function_type (
  std::vector<AST::LifetimeParam> for_lifetimes)
{
  // TODO: pass in for lifetime location as param
  location_t best_try_locus = lexer.peek_token ()->get_locus ();

  AST::FunctionQualifiers qualifiers = parse_function_qualifiers ();

  if (!skip_token (FN_KW))
    return nullptr;

  if (!skip_token (LEFT_PAREN))
    return nullptr;

  // parse function params, if they exist
  std::vector<AST::MaybeNamedParam> params;
  bool is_variadic = false;
  AST::AttrVec variadic_attrs;

  const_TokenPtr t = lexer.peek_token ();
  while (t->get_id () != RIGHT_PAREN)
    {
      AST::AttrVec temp_attrs = parse_outer_attributes ();

      if (lexer.peek_token ()->get_id () == ELLIPSIS)
	{
	  lexer.skip_token ();
	  is_variadic = true;
	  variadic_attrs = std::move (temp_attrs);

	  t = lexer.peek_token ();

	  if (t->get_id () != RIGHT_PAREN)
	    {
	      Error error (t->get_locus (),
			   "expected right parentheses after variadic in maybe "
			   "named function "
			   "parameters, found %qs",
			   t->get_token_description ());
	      add_error (std::move (error));

	      return nullptr;
	    }

	  break;
	}

      AST::MaybeNamedParam param
	= parse_maybe_named_param (std::move (temp_attrs));
      if (param.is_error ())
	{
	  Error error (
	    lexer.peek_token ()->get_locus (),
	    "failed to parse maybe named param in bare function type");
	  add_error (std::move (error));

	  return nullptr;
	}
      params.push_back (std::move (param));

      if (lexer.peek_token ()->get_id () != COMMA)
	break;

      lexer.skip_token ();
      t = lexer.peek_token ();
    }

  if (!skip_token (RIGHT_PAREN))
    return nullptr;

  // bare function return type, if exists
  std::unique_ptr<AST::TypeNoBounds> return_type = nullptr;
  if (lexer.peek_token ()->get_id () == RETURN_TYPE)
    {
      lexer.skip_token ();

      // parse required TypeNoBounds
      return_type = parse_type_no_bounds ();
      if (return_type == nullptr)
	{
	  Error error (lexer.peek_token ()->get_locus (),
		       "failed to parse return type (type no bounds) in bare "
		       "function type");
	  add_error (std::move (error));

	  return nullptr;
	}
    }

  return std::unique_ptr<AST::BareFunctionType> (
    new AST::BareFunctionType (std::move (for_lifetimes),
			       std::move (qualifiers), std::move (params),
			       is_variadic, std::move (variadic_attrs),
			       std::move (return_type), best_try_locus));
}

template <typename ManagedTokenSource>
std::unique_ptr<AST::ReferenceType>
Parser<ManagedTokenSource>::parse_reference_type_inner (location_t locus)
{
  // parse optional lifetime
  AST::Lifetime lifetime = AST::Lifetime::elided ();
  if (lexer.peek_token ()->get_id () == LIFETIME)
    {
      auto parsed_lifetime = parse_lifetime (true);
      if (parsed_lifetime)
	{
	  lifetime = parsed_lifetime.value ();
	}
      else
	{
	  Error error (lexer.peek_token ()->get_locus (),
		       "failed to parse lifetime in reference type");
	  add_error (std::move (error));

	  return nullptr;
	}
    }

  bool is_mut = false;
  if (lexer.peek_token ()->get_id () == MUT)
    {
      lexer.skip_token ();
      is_mut = true;
    }

  // parse type no bounds, which is required
  std::unique_ptr<AST::TypeNoBounds> type = parse_type_no_bounds ();
  if (type == nullptr)
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "failed to parse referenced type in reference type");
      add_error (std::move (error));

      return nullptr;
    }

  return std::unique_ptr<AST::ReferenceType> (
    new AST::ReferenceType (is_mut, std::move (type), locus,
			    std::move (lifetime)));
}

// Parses a reference type (mutable or immutable, with given lifetime).
template <typename ManagedTokenSource>
std::unique_ptr<AST::ReferenceType>
Parser<ManagedTokenSource>::parse_reference_type ()
{
  auto t = lexer.peek_token ();
  auto locus = t->get_locus ();

  switch (t->get_id ())
    {
    case AMP:
      skip_token (AMP);
      return parse_reference_type_inner (locus);
    case LOGICAL_AND:
      skip_token (LOGICAL_AND);
      return std::unique_ptr<AST::ReferenceType> (
	new AST::ReferenceType (false, parse_reference_type_inner (locus),
				locus));
    default:
      rust_unreachable ();
    }
}

// Parses a raw (unsafe) pointer type.
template <typename ManagedTokenSource>
std::unique_ptr<AST::RawPointerType>
Parser<ManagedTokenSource>::parse_raw_pointer_type ()
{
  location_t locus = lexer.peek_token ()->get_locus ();
  skip_token (ASTERISK);

  AST::RawPointerType::PointerType kind = AST::RawPointerType::CONST;

  // branch on next token for pointer kind info
  const_TokenPtr t = lexer.peek_token ();
  switch (t->get_id ())
    {
    case MUT:
      kind = AST::RawPointerType::MUT;
      lexer.skip_token ();
      break;
    case CONST:
      kind = AST::RawPointerType::CONST;
      lexer.skip_token ();
      break;
    default:
      add_error (Error (t->get_locus (),
			"unrecognised token %qs in raw pointer type",
			t->get_token_description ()));

      return nullptr;
    }

  // parse type no bounds (required)
  std::unique_ptr<AST::TypeNoBounds> type = parse_type_no_bounds ();
  if (type == nullptr)
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "failed to parse pointed type of raw pointer type");
      add_error (std::move (error));

      return nullptr;
    }

  return std::unique_ptr<AST::RawPointerType> (
    new AST::RawPointerType (kind, std::move (type), locus));
}

/* Parses a slice or array type, depending on following arguments (as
 * lookahead is not possible). */
template <typename ManagedTokenSource>
std::unique_ptr<AST::TypeNoBounds>
Parser<ManagedTokenSource>::parse_slice_or_array_type ()
{
  location_t locus = lexer.peek_token ()->get_locus ();
  skip_token (LEFT_SQUARE);

  // parse inner type (required)
  std::unique_ptr<AST::Type> inner_type = parse_type ();
  if (inner_type == nullptr)
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "failed to parse inner type in slice or array type");
      add_error (std::move (error));

      return nullptr;
    }

  // branch on next token
  const_TokenPtr t = lexer.peek_token ();
  switch (t->get_id ())
    {
    case RIGHT_SQUARE:
      // slice type
      lexer.skip_token ();

      return std::unique_ptr<AST::SliceType> (
	new AST::SliceType (std::move (inner_type), locus));
    case SEMICOLON:
      {
	// array type
	lexer.skip_token ();

	// parse required array size expression
	auto size = parse_anon_const ();

	if (!size)
	  {
	    Error error (lexer.peek_token ()->get_locus (),
			 "failed to parse size expression in array type");
	    add_error (std::move (error));

	    return nullptr;
	  }

	if (!skip_token (RIGHT_SQUARE))
	  {
	    return nullptr;
	  }

	return std::unique_ptr<AST::ArrayType> (
	  new AST::ArrayType (std::move (inner_type), std::move (*size),
			      locus));
      }
    default:
      // error
      add_error (
	Error (t->get_locus (),
	       "unrecognised token %qs in slice or array type after inner type",
	       t->get_token_description ()));

      return nullptr;
    }
}

// Parses a type, taking into account type boundary disambiguation.
template <typename ManagedTokenSource>
std::unique_ptr<AST::TypeNoBounds>
Parser<ManagedTokenSource>::parse_type_no_bounds ()
{
  const_TokenPtr t = lexer.peek_token ();
  switch (t->get_id ())
    {
    case EXCLAM:
      // never type - can't be macro as no path beforehand
      lexer.skip_token ();
      return std::unique_ptr<AST::NeverType> (
	new AST::NeverType (t->get_locus ()));
    case LEFT_SQUARE:
      // slice type or array type - requires further disambiguation
      return parse_slice_or_array_type ();
    case LEFT_SHIFT:
    case LEFT_ANGLE:
      {
	// qualified path in type
	AST::QualifiedPathInType path = parse_qualified_path_in_type ();
	if (path.is_error ())
	  {
	    Error error (t->get_locus (),
			 "failed to parse qualified path in type");
	    add_error (std::move (error));

	    return nullptr;
	  }
	return std::unique_ptr<AST::QualifiedPathInType> (
	  new AST::QualifiedPathInType (std::move (path)));
      }
    case UNDERSCORE:
      // inferred type
      lexer.skip_token ();
      return std::unique_ptr<AST::InferredType> (
	new AST::InferredType (t->get_locus ()));
    case ASTERISK:
      // raw pointer type
      return parse_raw_pointer_type ();
    case AMP: // does this also include AMP_AMP? Yes! Which is... LOGICAL_AND?
    case LOGICAL_AND:
      // reference type
      return parse_reference_type ();
    case LIFETIME:
      /* probably a lifetime bound, so probably type param bounds in
       * TraitObjectType. this is not allowed, but detection here for error
       * message */
      add_error (Error (t->get_locus (),
			"lifetime bounds (i.e. in type param bounds, in "
			"TraitObjectType) are not allowed as TypeNoBounds"));

      return nullptr;
    case IDENTIFIER:
    case SUPER:
    case SELF:
    case SELF_ALIAS:
    case CRATE:
    case DOLLAR_SIGN:
    case SCOPE_RESOLUTION:
      {
	// macro invocation or type path - requires further disambiguation.
	/* for parsing path component of each rule, perhaps parse it as a
	 * typepath and attempt conversion to simplepath if a trailing '!' is
	 * found */
	/* Type path also includes TraitObjectTypeOneBound BUT if it starts
	 * with it, it is exactly the same as a TypePath syntactically, so
	 * this is a syntactical ambiguity. As such, the parser will parse it
	 * as a TypePath. This, however, does not prevent TraitObjectType from
	 * starting with a typepath. */

	// parse path as type path
	AST::TypePath path = parse_type_path ();
	if (path.is_error ())
	  {
	    Error error (
	      t->get_locus (),
	      "failed to parse path as first component of type no bounds");
	    add_error (std::move (error));

	    return nullptr;
	  }
	location_t locus = path.get_locus ();

	// branch on next token
	t = lexer.peek_token ();
	switch (t->get_id ())
	  {
	  case EXCLAM:
	    {
	      // macro invocation
	      // convert to simple path
	      AST::SimplePath macro_path = path.as_simple_path ();
	      if (macro_path.is_empty ())
		{
		  Error error (t->get_locus (),
			       "failed to parse simple path in macro "
			       "invocation (for type)");
		  add_error (std::move (error));

		  return nullptr;
		}

	      lexer.skip_token ();

	      auto tok_tree = parse_delim_token_tree ();
	      if (!tok_tree)
		return nullptr;

	      return AST::MacroInvocation::Regular (
		AST::MacroInvocData (std::move (macro_path),
				     std::move (tok_tree.value ())),
		{}, locus);
	    }
	  default:
	    // assume that this is a type path and not an error
	    return std::unique_ptr<AST::TypePath> (
	      new AST::TypePath (std::move (path)));
	  }
      }
    case LEFT_PAREN:
      /* tuple type or parenthesised type - requires further disambiguation
       * (the usual). ok apparently can be a parenthesised TraitBound too, so
       * could be TraitObjectTypeOneBound */
      return parse_paren_prefixed_type_no_bounds ();
    case FOR:
    case ASYNC:
    case CONST:
    case UNSAFE:
    case EXTERN_KW:
    case FN_KW:
      // bare function type (with no for lifetimes)
      return parse_bare_function_type (std::vector<AST::LifetimeParam> ());
    case IMPL:
      lexer.skip_token ();
      if (lexer.peek_token ()->get_id () == LIFETIME)
	{
	  /* cannot be one bound because lifetime prevents it from being
	   * traitbound not allowed as type no bounds, only here for error
	   * message */
	  Error error (
	    lexer.peek_token ()->get_locus (),
	    "lifetime (probably lifetime bound, in type param "
	    "bounds, in ImplTraitType) is not allowed in TypeNoBounds");
	  add_error (std::move (error));

	  return nullptr;
	}
      else
	{
	  // should be trait bound, so parse trait bound
	  std::unique_ptr<AST::TraitBound> initial_bound = parse_trait_bound ();
	  if (initial_bound == nullptr)
	    {
	      Error error (lexer.peek_token ()->get_locus (),
			   "failed to parse ImplTraitTypeOneBound bound");
	      add_error (std::move (error));

	      return nullptr;
	    }

	  location_t locus = t->get_locus ();

	  // ensure not a trait with multiple bounds
	  t = lexer.peek_token ();
	  if (t->get_id () == PLUS)
	    {
	      Error error (t->get_locus (),
			   "plus after trait bound means an ImplTraitType, "
			   "which is not allowed as a TypeNoBounds");
	      add_error (std::move (error));

	      return nullptr;
	    }

	  return std::unique_ptr<AST::ImplTraitTypeOneBound> (
	    new AST::ImplTraitTypeOneBound (std::move (initial_bound), locus));
	}
    case DYN:
    case QUESTION_MARK:
      {
	// either TraitObjectTypeOneBound
	bool has_dyn = false;
	if (t->get_id () == DYN)
	  {
	    lexer.skip_token ();
	    has_dyn = true;
	  }

	if (lexer.peek_token ()->get_id () == LIFETIME)
	  {
	    /* means that cannot be TraitObjectTypeOneBound - so here for
	     * error message */
	    Error error (lexer.peek_token ()->get_locus (),
			 "lifetime as bound in TraitObjectTypeOneBound "
			 "is not allowed, so cannot be TypeNoBounds");
	    add_error (std::move (error));

	    return nullptr;
	  }

	// should be trait bound, so parse trait bound
	std::unique_ptr<AST::TraitBound> initial_bound = parse_trait_bound ();
	if (initial_bound == nullptr)
	  {
	    Error error (
	      lexer.peek_token ()->get_locus (),
	      "failed to parse TraitObjectTypeOneBound initial bound");
	    add_error (std::move (error));

	    return nullptr;
	  }

	location_t locus = t->get_locus ();

	// detect error with plus as next token
	t = lexer.peek_token ();
	if (t->get_id () == PLUS)
	  {
	    Error error (t->get_locus (),
			 "plus after trait bound means a TraitObjectType, "
			 "which is not allowed as a TypeNoBounds");
	    add_error (std::move (error));

	    return nullptr;
	  }

	// convert trait bound to value object
	AST::TraitBound value_bound (*initial_bound);

	return std::unique_ptr<AST::TraitObjectTypeOneBound> (
	  new AST::TraitObjectTypeOneBound (std::move (value_bound), locus,
					    has_dyn));
      }
    default:
      add_error (Error (t->get_locus (),
			"unrecognised token %qs in type no bounds",
			t->get_token_description ()));

      return nullptr;
    }
}

// Parses a type no bounds beginning with '('.
template <typename ManagedTokenSource>
std::unique_ptr<AST::TypeNoBounds>
Parser<ManagedTokenSource>::parse_paren_prefixed_type_no_bounds ()
{
  /* NOTE: this could probably be parsed without the HACK solution of
   * parse_paren_prefixed_type, but I was lazy. So FIXME for future.*/

  /* NOTE: again, syntactical ambiguity of a parenthesised trait bound is
   * considered a trait bound, not a parenthesised type, so that it can still
   * be used in type param bounds. */

  location_t left_paren_locus = lexer.peek_token ()->get_locus ();

  // skip left delim
  lexer.skip_token ();
  /* while next token isn't close delim, parse comma-separated types, saving
   * whether trailing comma happens */
  const_TokenPtr t = lexer.peek_token ();
  bool trailing_comma = true;
  std::vector<std::unique_ptr<AST::Type>> types;

  while (t->get_id () != RIGHT_PAREN)
    {
      std::unique_ptr<AST::Type> type = parse_type ();
      if (type == nullptr)
	{
	  Error error (t->get_locus (),
		       "failed to parse type inside parentheses (probably "
		       "tuple or parenthesised)");
	  add_error (std::move (error));

	  return nullptr;
	}
      types.push_back (std::move (type));

      t = lexer.peek_token ();
      if (t->get_id () != COMMA)
	{
	  trailing_comma = false;
	  break;
	}
      lexer.skip_token ();

      t = lexer.peek_token ();
    }

  if (!skip_token (RIGHT_PAREN))
    {
      return nullptr;
    }

  // if only one type and no trailing comma, then not a tuple type
  if (types.size () == 1 && !trailing_comma)
    {
      // must be a TraitObjectType (with more than one bound)
      if (lexer.peek_token ()->get_id () == PLUS)
	{
	  // error - this is not allowed for type no bounds
	  Error error (lexer.peek_token ()->get_locus (),
		       "plus (implying TraitObjectType as type param "
		       "bounds) is not allowed in type no bounds");
	  add_error (std::move (error));

	  return nullptr;
	}
      else
	{
	  // release vector pointer
	  std::unique_ptr<AST::Type> released_ptr = std::move (types[0]);
	  /* HACK: attempt to convert to trait bound. if fails, parenthesised
	   * type */
	  std::unique_ptr<AST::TraitBound> converted_bound (
	    released_ptr->to_trait_bound (true));
	  if (converted_bound == nullptr)
	    {
	      // parenthesised type
	      return std::unique_ptr<AST::ParenthesisedType> (
		new AST::ParenthesisedType (std::move (released_ptr),
					    left_paren_locus));
	    }
	  else
	    {
	      // trait object type (one bound)

	      // get value semantics trait bound
	      AST::TraitBound value_bound (*converted_bound);

	      return std::unique_ptr<AST::TraitObjectTypeOneBound> (
		new AST::TraitObjectTypeOneBound (value_bound,
						  left_paren_locus));
	    }
	}
    }
  else
    {
      return std::unique_ptr<AST::TupleType> (
	new AST::TupleType (std::move (types), left_paren_locus));
    }
  /* TODO: ensure that this ensures that dynamic dispatch for traits is not
   * lost somehow */
}

// Parses tuple struct items if they exist. Does not parse parentheses.
template <typename ManagedTokenSource>
std::unique_ptr<AST::TupleStructItems>
Parser<ManagedTokenSource>::parse_tuple_struct_items ()
{
  std::vector<std::unique_ptr<AST::Pattern>> lower_patterns;

  // DEBUG
  rust_debug ("started parsing tuple struct items");

  // check for '..' at front
  if (lexer.peek_token ()->get_id () == DOT_DOT)
    {
      // only parse upper patterns
      lexer.skip_token ();

      // DEBUG
      rust_debug ("'..' at front in tuple struct items detected");

      std::vector<std::unique_ptr<AST::Pattern>> upper_patterns;

      const_TokenPtr t = lexer.peek_token ();
      while (t->get_id () == COMMA)
	{
	  lexer.skip_token ();

	  // break if right paren
	  if (lexer.peek_token ()->get_id () == RIGHT_PAREN)
	    break;

	  // parse pattern, which is now required
	  std::unique_ptr<AST::Pattern> pattern = parse_pattern ();
	  if (pattern == nullptr)
	    {
	      Error error (lexer.peek_token ()->get_locus (),
			   "failed to parse pattern in tuple struct items");
	      add_error (std::move (error));

	      return nullptr;
	    }
	  upper_patterns.push_back (std::move (pattern));

	  t = lexer.peek_token ();
	}

      // DEBUG
      rust_debug (
	"finished parsing tuple struct items ranged (upper/none only)");

      return std::unique_ptr<AST::TupleStructItemsHasRest> (
	new AST::TupleStructItemsHasRest (std::move (lower_patterns),
					  std::move (upper_patterns)));
    }

  // has at least some lower patterns
  const_TokenPtr t = lexer.peek_token ();
  while (t->get_id () != RIGHT_PAREN && t->get_id () != DOT_DOT)
    {
      // DEBUG
      rust_debug ("about to parse pattern in tuple struct items");

      // parse pattern, which is required
      std::unique_ptr<AST::Pattern> pattern = parse_pattern ();
      if (pattern == nullptr)
	{
	  Error error (t->get_locus (),
		       "failed to parse pattern in tuple struct items");
	  add_error (std::move (error));

	  return nullptr;
	}
      lower_patterns.push_back (std::move (pattern));

      // DEBUG
      rust_debug ("successfully parsed pattern in tuple struct items");

      if (lexer.peek_token ()->get_id () != COMMA)
	{
	  // DEBUG
	  rust_debug ("broke out of parsing patterns in tuple struct "
		      "items as no comma");

	  break;
	}
      lexer.skip_token ();
      t = lexer.peek_token ();
    }

  // branch on next token
  t = lexer.peek_token ();
  switch (t->get_id ())
    {
    case RIGHT_PAREN:
      return std::unique_ptr<AST::TupleStructItemsNoRest> (
	new AST::TupleStructItemsNoRest (std::move (lower_patterns)));
    case DOT_DOT:
      {
	// has an upper range that must be parsed separately
	lexer.skip_token ();

	std::vector<std::unique_ptr<AST::Pattern>> upper_patterns;

	t = lexer.peek_token ();
	while (t->get_id () == COMMA)
	  {
	    lexer.skip_token ();

	    // break if next token is right paren
	    if (lexer.peek_token ()->get_id () == RIGHT_PAREN)
	      break;

	    // parse pattern, which is required
	    std::unique_ptr<AST::Pattern> pattern = parse_pattern ();
	    if (pattern == nullptr)
	      {
		Error error (lexer.peek_token ()->get_locus (),
			     "failed to parse pattern in tuple struct items");
		add_error (std::move (error));

		return nullptr;
	      }
	    upper_patterns.push_back (std::move (pattern));

	    t = lexer.peek_token ();
	  }

	return std::unique_ptr<AST::TupleStructItemsHasRest> (
	  new AST::TupleStructItemsHasRest (std::move (lower_patterns),
					    std::move (upper_patterns)));
      }
    default:
      // error
      add_error (Error (t->get_locus (),
			"unexpected token %qs in tuple struct items",
			t->get_token_description ()));

      return nullptr;
    }
}

/* Parses a statement or expression (depending on whether a trailing semicolon
 * exists). Useful for block expressions where it cannot be determined through
 * lookahead whether it is a statement or expression to be parsed. */
template <typename ManagedTokenSource>
ExprOrStmt
Parser<ManagedTokenSource>::parse_stmt_or_expr ()
{
  // quick exit for empty statement
  const_TokenPtr t = lexer.peek_token ();
  if (t->get_id () == SEMICOLON)
    {
      lexer.skip_token ();
      std::unique_ptr<AST::EmptyStmt> stmt (
	new AST::EmptyStmt (t->get_locus ()));
      return ExprOrStmt (std::move (stmt));
    }

  // parse outer attributes
  AST::AttrVec outer_attrs = parse_outer_attributes ();
  ParseRestrictions restrictions;
  restrictions.expr_can_be_stmt = true;
  std::unique_ptr<AST::Expr> expr;

  // parsing this will be annoying because of the many different possibilities
  /* best may be just to copy paste in parse_item switch, and failing that try
   * to parse outer attributes, and then pass them in to either a let
   * statement or (fallback) expression statement. */
  // FIXME: think of a way to do this without such a large switch?

  /* FIXME: for expressions at least, the only way that they can really be
   * parsed properly in this way is if they don't support operators on them.
   * They must be pratt-parsed otherwise. As such due to composability, only
   * explicit statements will have special cases here. This should roughly
   * correspond to "expr-with-block", but this warning is here in case it
   * isn't the case. */
  t = lexer.peek_token ();
  switch (t->get_id ())
    {
    case LET:
      {
	// let statement
	std::unique_ptr<AST::LetStmt> stmt (
	  parse_let_stmt (std::move (outer_attrs)));
	return ExprOrStmt (std::move (stmt));
      }
    case PUB:
    case MOD:
    case EXTERN_KW:
    case USE:
    case FN_KW:
    case TYPE:
    case STRUCT_KW:
    case ENUM_KW:
    case CONST:
    case STATIC_KW:
    case AUTO:
    case TRAIT:
    case IMPL:
      {
	std::unique_ptr<AST::VisItem> item (
	  parse_vis_item (std::move (outer_attrs)));
	return ExprOrStmt (std::move (item));
      }
    /* TODO: implement union keyword but not really because of
     * context-dependence crappy hack way to parse a union written below to
     * separate it from the good code. */
    // case UNION:
    case UNSAFE:
      { // maybe - unsafe traits are a thing
	/* if any of these (should be all possible VisItem prefixes), parse a
	 * VisItem - can't parse item because would require reparsing outer
	 * attributes */
	const_TokenPtr t2 = lexer.peek_token (1);
	switch (t2->get_id ())
	  {
	  case LEFT_CURLY:
	    {
	      // unsafe block: parse as expression
	      expr = parse_expr (std::move (outer_attrs), restrictions);
	      break;
	    }
	  case AUTO:
	  case TRAIT:
	    {
	      // unsafe trait
	      std::unique_ptr<AST::VisItem> item (
		parse_vis_item (std::move (outer_attrs)));
	      return ExprOrStmt (std::move (item));
	    }
	  case EXTERN_KW:
	  case FN_KW:
	    {
	      // unsafe function
	      std::unique_ptr<AST::VisItem> item (
		parse_vis_item (std::move (outer_attrs)));
	      return ExprOrStmt (std::move (item));
	    }
	  case IMPL:
	    {
	      // unsafe trait impl
	      std::unique_ptr<AST::VisItem> item (
		parse_vis_item (std::move (outer_attrs)));
	      return ExprOrStmt (std::move (item));
	    }
	  default:
	    add_error (Error (t2->get_locus (),
			      "unrecognised token %qs after parsing unsafe - "
			      "expected beginning of expression or statement",
			      t->get_token_description ()));

	    // skip somewhere?
	    return ExprOrStmt::create_error ();
	  }
	break;
      }
      /* FIXME: this is either a macro invocation or macro invocation semi.
       * start parsing to determine which one it is. */
      // FIXME: old code there

    // crappy hack to do union "keyword"
    case IDENTIFIER:
      if (t->get_str () == Values::WeakKeywords::UNION
	  && lexer.peek_token (1)->get_id () == IDENTIFIER)
	{
	  std::unique_ptr<AST::VisItem> item (
	    parse_vis_item (std::move (outer_attrs)));
	  return ExprOrStmt (std::move (item));
	  // or should this go straight to parsing union?
	}
      else if (t->get_str () == Values::WeakKeywords::MACRO_RULES
	       && lexer.peek_token (1)->get_id () == EXCLAM)
	{
	  // macro_rules! macro item
	  std::unique_ptr<AST::Item> item (
	    parse_macro_rules_def (std::move (outer_attrs)));
	  return ExprOrStmt (std::move (item));
	}
      gcc_fallthrough ();
    case SUPER:
    case SELF:
    case SELF_ALIAS:
    case CRATE:
    case SCOPE_RESOLUTION:
    case DOLLAR_SIGN:
      {
	AST::PathInExpression path = parse_path_in_expression ();
	std::unique_ptr<AST::Expr> null_denotation;

	if (lexer.peek_token ()->get_id () == EXCLAM)
	  {
	    std::unique_ptr<AST::MacroInvocation> invoc
	      = parse_macro_invocation_partial (std::move (path),
						std::move (outer_attrs));
	    if (invoc == nullptr)
	      return ExprOrStmt::create_error ();

	    if (restrictions.consume_semi && maybe_skip_token (SEMICOLON))
	      {
		invoc->add_semicolon ();
		// Macro invocation with semicolon.
		return ExprOrStmt (
		  std::unique_ptr<AST::Stmt> (std::move (invoc)));
	      }

	    TokenId after_macro = lexer.peek_token ()->get_id ();

	    AST::DelimType delim_type = invoc->get_invoc_data ()
					  .get_delim_tok_tree ()
					  .get_delim_type ();

	    if (delim_type == AST::CURLY && after_macro != DOT
		&& after_macro != QUESTION_MARK)
	      {
		rust_debug ("braced macro statement");
		return ExprOrStmt (
		  std::unique_ptr<AST::Stmt> (std::move (invoc)));
	      }

	    null_denotation = std::move (invoc);
	  }
	else
	  {
	    null_denotation
	      = null_denotation_path (std::move (path), {}, restrictions);
	  }

	expr = left_denotations (std::move (null_denotation), LBP_LOWEST,
				 std::move (outer_attrs), restrictions);
	break;
      }
    default:
      /* expression statement or expression itself - parse
       * expression then make it statement if semi afterwards */
      expr = parse_expr (std::move (outer_attrs), restrictions);
      break;
    }

  const_TokenPtr after_expr = lexer.peek_token ();
  if (after_expr->get_id () == SEMICOLON)
    {
      // must be expression statement
      lexer.skip_token ();

      if (expr)
	{
	  std::unique_ptr<AST::ExprStmt> stmt (
	    new AST::ExprStmt (std::move (expr), t->get_locus (), true));
	  return ExprOrStmt (std::move (stmt));
	}
      else
	{
	  return ExprOrStmt::create_error ();
	}
    }

  if (expr && !expr->is_expr_without_block ()
      && after_expr->get_id () != RIGHT_CURLY)
    {
      // block expression statement.
      std::unique_ptr<AST::ExprStmt> stmt (
	new AST::ExprStmt (std::move (expr), t->get_locus (), false));
      return ExprOrStmt (std::move (stmt));
    }

  // return expression
  return ExprOrStmt (std::move (expr));
}

} // namespace Rust

#include "rust-parse-impl-utils.hxx"
#include "rust-parse-impl-attribute.hxx"
#include "rust-parse-impl-ttree.hxx"
#include "rust-parse-impl-macro.hxx"
#include "rust-parse-impl-path.hxx"
#include "rust-parse-impl-pattern.hxx"
#include "rust-parse-impl-expr.hxx"
