// Copyright (C) 2025-2026 Free Software Foundation, Inc.

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

// Parses a block expression, including the curly braces at start and end.
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::BlockExpr>, Parse::Error::Node>
Parser<ManagedTokenSource>::parse_block_expr (
  AST::AttrVec outer_attrs, tl::optional<AST::LoopLabel> label,
  location_t pratt_parsed_loc)
{
  location_t locus = pratt_parsed_loc;
  if (locus == UNKNOWN_LOCATION)
    {
      locus = lexer.peek_token ()->get_locus ();
      if (!skip_token (LEFT_CURLY))
	{
	  skip_after_end_block ();
	  return tl::unexpected<Parse::Error::Node> (
	    Parse::Error::Node::MALFORMED);
	}
    }

  AST::AttrVec inner_attrs = parse_inner_attributes ();

  // parse statements and expression
  std::vector<std::unique_ptr<AST::Stmt>> stmts;
  std::unique_ptr<AST::Expr> expr = nullptr;

  const_TokenPtr t = lexer.peek_token ();
  while (t->get_id () != RIGHT_CURLY)
    {
      auto expr_or_stmt = parse_stmt_or_expr ();
      if (!expr_or_stmt)
	{
	  skip_after_end_block ();
	  return tl::unexpected<Parse::Error::Node> (
	    Parse::Error::Node::CHILD_ERROR);
	}

      t = lexer.peek_token ();

      if (expr_or_stmt->stmt != nullptr)
	{
	  stmts.push_back (std::move (expr_or_stmt->stmt));
	}
      else
	{
	  // assign to expression and end parsing inside
	  expr = std::move (expr_or_stmt->expr);
	}
    }

  location_t end_locus = t->get_locus ();

  if (!skip_token (RIGHT_CURLY))
    {
      Error error (t->get_locus (),
		   "error may be from having an expression (as opposed to "
		   "statement) in the body of the function but not last");
      add_error (std::move (error));

      skip_after_end_block ();
      return tl::unexpected<Parse::Error::Node> (Parse::Error::Node::MALFORMED);
    }

  // grammar allows for empty block expressions

  stmts.shrink_to_fit ();

  return std::unique_ptr<AST::BlockExpr> (
    new AST::BlockExpr (std::move (stmts), std::move (expr),
			std::move (inner_attrs), std::move (outer_attrs),
			std::move (label), locus, end_locus));
}

/* Parse an anonymous const expression. This can be a regular const expression
 * or an underscore for deferred const inference */
template <typename ManagedTokenSource>
tl::expected<AST::AnonConst, Parse::Error::Node>
Parser<ManagedTokenSource>::parse_anon_const ()
{
  auto current = lexer.peek_token ();
  auto locus = current->get_locus ();

  // Special case deferred inference constants
  if (maybe_skip_token (UNDERSCORE))
    return AST::AnonConst (locus);

  auto expr = parse_expr ();

  if (!expr)
    return tl::make_unexpected (Parse::Error::Node{});

  return AST::AnonConst (std::move (expr.value ()), locus);
}

/* Parse a "const block", a block preceded by the `const` keyword whose
 * statements can be const evaluated and used in constant contexts */
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::ConstBlock>, Parse::Error::Node>
Parser<ManagedTokenSource>::parse_const_block_expr (AST::AttrVec outer_attrs,
						    location_t locus)
{
  auto block_res = parse_block_expr ();

  if (!block_res)
    {
      add_error (Error (locus, "failed to parse inner block in const block"));
      skip_after_end_block ();

      return tl::unexpected<Parse::Error::Node> (
	Parse::Error::Node::CHILD_ERROR);
    }
  auto block = std::move (block_res.value ());

  auto block_locus = block->get_locus ();

  return std::make_unique<AST::ConstBlock> (AST::AnonConst (std::move (block),
							    block_locus),
					    locus, std::move (outer_attrs));
}

/* Parses a "grouped" expression (expression in parentheses), used to control
 * precedence. */
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::GroupedExpr>, Parse::Error::Node>
Parser<ManagedTokenSource>::parse_grouped_expr (AST::AttrVec outer_attrs)
{
  location_t locus = lexer.peek_token ()->get_locus ();
  skip_token (LEFT_PAREN);

  AST::AttrVec inner_attrs = parse_inner_attributes ();

  // parse required expr inside parentheses
  auto expr_in_parens = parse_expr ();
  if (!expr_in_parens)
    {
      // skip after somewhere?
      // error?
      return tl::unexpected<Parse::Error::Node> (
	Parse::Error::Node::CHILD_ERROR);
    }

  if (!skip_token (RIGHT_PAREN))
    {
      // skip after somewhere?
      return tl::unexpected<Parse::Error::Node> (Parse::Error::Node::MALFORMED);
    }

  return std::unique_ptr<AST::GroupedExpr> (
    new AST::GroupedExpr (std::move (expr_in_parens.value ()),
			  std::move (inner_attrs), std::move (outer_attrs),
			  locus));
}

// Parses a closure expression (closure definition).
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::ClosureExpr>, Parse::Error::Node>
Parser<ManagedTokenSource>::parse_closure_expr (AST::AttrVec outer_attrs)
{
  location_t locus = lexer.peek_token ()->get_locus ();
  // detect optional "move"
  bool has_move = false;
  if (lexer.peek_token ()->get_id () == MOVE)
    {
      lexer.skip_token ();
      has_move = true;
    }

  // handle parameter list
  std::vector<AST::ClosureParam> params;

  const_TokenPtr t = lexer.peek_token ();
  switch (t->get_id ())
    {
    case OR:
      // skip token, no parameters
      lexer.skip_token ();
      break;
    case PIPE:
      // actually may have parameters
      lexer.skip_token ();
      t = lexer.peek_token ();

      while (t->get_id () != PIPE)
	{
	  AST::ClosureParam param = parse_closure_param ();
	  if (param.is_error ())
	    {
	      // TODO is this really an error?
	      Error error (t->get_locus (), "could not parse closure param");
	      add_error (std::move (error));

	      break;
	    }
	  params.push_back (std::move (param));

	  if (lexer.peek_token ()->get_id () != COMMA)
	    {
	      lexer.skip_token ();
	      // not an error but means param list is done
	      break;
	    }
	  // skip comma
	  lexer.skip_token ();

	  t = lexer.peek_token ();
	}
      params.shrink_to_fit ();
      break;
    default:
      add_error (Error (t->get_locus (),
			"unexpected token %qs in closure expression - expected "
			"%<|%> or %<||%>",
			t->get_token_description ()));

      // skip somewhere?
      return tl::unexpected<Parse::Error::Node> (Parse::Error::Node::MALFORMED);
    }

  // again branch based on next token
  t = lexer.peek_token ();
  if (t->get_id () == RETURN_TYPE)
    {
      // must be return type closure with block expr

      // skip "return type" token
      lexer.skip_token ();

      // parse actual type, which is required
      std::unique_ptr<AST::TypeNoBounds> type = parse_type_no_bounds ();
      if (type == nullptr)
	{
	  // error
	  Error error (t->get_locus (), "failed to parse type for closure");
	  add_error (std::move (error));

	  // skip somewhere?
	  return tl::unexpected<Parse::Error::Node> (
	    Parse::Error::Node::CHILD_ERROR);
	}

      // parse block expr, which is required
      auto block = parse_block_expr ();
      if (!block)
	{
	  // error
	  Error error (lexer.peek_token ()->get_locus (),
		       "failed to parse block expr in closure");
	  add_error (std::move (error));

	  // skip somewhere?
	  return tl::unexpected<Parse::Error::Node> (
	    Parse::Error::Node::CHILD_ERROR);
	}

      return std::unique_ptr<AST::ClosureExprInnerTyped> (
	new AST::ClosureExprInnerTyped (std::move (type),
					std::move (block.value ()),
					std::move (params), locus, has_move,
					std::move (outer_attrs)));
    }
  else
    {
      // must be expr-only closure

      // parse expr, which is required
      auto expr = parse_expr ();
      if (!expr)
	{
	  Error error (t->get_locus (),
		       "failed to parse expression in closure");
	  add_error (std::move (error));

	  // skip somewhere?
	  return tl::unexpected<Parse::Error::Node> (
	    Parse::Error::Node::CHILD_ERROR);
	}

      return std::unique_ptr<AST::ClosureExprInner> (
	new AST::ClosureExprInner (std::move (expr.value ()),
				   std::move (params), locus, has_move,
				   std::move (outer_attrs)));
    }
}

// Parses a literal token (to literal expression).
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::LiteralExpr>, Parse::Error::Node>
Parser<ManagedTokenSource>::parse_literal_expr (AST::AttrVec outer_attrs)
{
  // TODO: change if literal representation in lexer changes

  std::string literal_value;
  AST::Literal::LitType type = AST::Literal::STRING;

  // branch based on token
  const_TokenPtr t = lexer.peek_token ();
  switch (t->get_id ())
    {
    case CHAR_LITERAL:
      type = AST::Literal::CHAR;
      literal_value = t->get_str ();
      lexer.skip_token ();
      break;
    case STRING_LITERAL:
      type = AST::Literal::STRING;
      literal_value = t->get_str ();
      lexer.skip_token ();
      break;
    case BYTE_CHAR_LITERAL:
      type = AST::Literal::BYTE;
      literal_value = t->get_str ();
      lexer.skip_token ();
      break;
    case BYTE_STRING_LITERAL:
      type = AST::Literal::BYTE_STRING;
      literal_value = t->get_str ();
      lexer.skip_token ();
      break;
    case RAW_STRING_LITERAL:
      type = AST::Literal::RAW_STRING;
      literal_value = t->get_str ();
      lexer.skip_token ();
      break;
    case INT_LITERAL:
      type = AST::Literal::INT;
      literal_value = t->get_str ();
      lexer.skip_token ();
      break;
    case FLOAT_LITERAL:
      type = AST::Literal::FLOAT;
      literal_value = t->get_str ();
      lexer.skip_token ();
      break;
    // case BOOL_LITERAL
    // use true and false keywords rather than "bool literal" Rust terminology
    case TRUE_LITERAL:
      type = AST::Literal::BOOL;
      literal_value = Values::Keywords::TRUE_LITERAL;
      lexer.skip_token ();
      break;
    case FALSE_LITERAL:
      type = AST::Literal::BOOL;
      literal_value = Values::Keywords::FALSE_LITERAL;
      lexer.skip_token ();
      break;
    default:
      // error - cannot be a literal expr
      add_error (Error (t->get_locus (),
			"unexpected token %qs when parsing literal expression",
			t->get_token_description ()));

      // skip?
      return tl::unexpected<Parse::Error::Node> (Parse::Error::Node::MALFORMED);
    }

  // create literal based on stuff in switch
  return std::unique_ptr<AST::LiteralExpr> (
    new AST::LiteralExpr (std::move (literal_value), std::move (type),
			  t->get_type_hint (), std::move (outer_attrs),
			  t->get_locus ()));
}

template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::BoxExpr>, Parse::Error::Node>
Parser<ManagedTokenSource>::parse_box_expr (AST::AttrVec outer_attrs,
					    location_t pratt_parsed_loc)
{
  location_t locus = pratt_parsed_loc;
  if (locus == UNKNOWN_LOCATION)
    {
      locus = lexer.peek_token ()->get_locus ();
      skip_token (BOX);
    }

  ParseRestrictions restrictions;
  restrictions.expr_can_be_null = false;

  auto expr = parse_expr (AST::AttrVec (), restrictions);
  if (!expr)
    return tl::unexpected<Parse::Error::Node> (Parse::Error::Node::CHILD_ERROR);

  return std::unique_ptr<AST::BoxExpr> (
    new AST::BoxExpr (std::move (expr.value ()), std::move (outer_attrs),
		      locus));
}

// Parses a return expression (including any expression to return).
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::ReturnExpr>, Parse::Error::Node>
Parser<ManagedTokenSource>::parse_return_expr (AST::AttrVec outer_attrs,
					       location_t pratt_parsed_loc)
{
  location_t locus = pratt_parsed_loc;
  if (locus == UNKNOWN_LOCATION)
    {
      locus = lexer.peek_token ()->get_locus ();
      skip_token (RETURN_KW);
    }

  // parse expression to return, if it exists
  ParseRestrictions restrictions;
  restrictions.expr_can_be_null = true;
  auto returned_expr = parse_expr (AST::AttrVec (), restrictions);
  tl::optional<std::unique_ptr<AST::Expr>> expr = tl::nullopt;
  if (returned_expr)
    expr = std::move (returned_expr.value ());

  return std::make_unique<AST::ReturnExpr> (std::move (expr),
					    std::move (outer_attrs), locus);
}

// Parses a try expression.
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::TryExpr>, Parse::Error::Node>
Parser<ManagedTokenSource>::parse_try_expr (AST::AttrVec outer_attrs,
					    location_t pratt_parsed_loc)
{
  location_t locus = pratt_parsed_loc;
  if (locus == UNKNOWN_LOCATION)
    {
      locus = lexer.peek_token ()->get_locus ();
      skip_token (TRY);
    }

  auto block_expr = parse_block_expr ();

  if (!block_expr)
    return tl::unexpected<Parse::Error::Node> (Parse::Error::Node::CHILD_ERROR);

  return std::unique_ptr<AST::TryExpr> (
    new AST::TryExpr (std::move (block_expr.value ()), std::move (outer_attrs),
		      locus));
}

/* Parses a break expression (including any label to break to AND any return
 * expression). */
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::BreakExpr>, Parse::Error::Node>
Parser<ManagedTokenSource>::parse_break_expr (AST::AttrVec outer_attrs,
					      location_t pratt_parsed_loc)
{
  location_t locus = pratt_parsed_loc;
  if (locus == UNKNOWN_LOCATION)
    {
      locus = lexer.peek_token ()->get_locus ();
      skip_token (BREAK);
    }

  auto parsed_label = parse_lifetime (false);
  auto label = (parsed_label)
		 ? tl::optional<AST::Lifetime> (parsed_label.value ())
		 : tl::nullopt;

  // parse break return expression if it exists
  ParseRestrictions restrictions;
  restrictions.expr_can_be_null = true;
  auto return_expr = parse_expr (AST::AttrVec (), restrictions);

  if (return_expr)
    return std::unique_ptr<AST::BreakExpr> (
      new AST::BreakExpr (std::move (label), std::move (return_expr.value ()),
			  std::move (outer_attrs), locus));
  else if (return_expr.error () == Parse::Error::Expr::NULL_EXPR)
    return std::unique_ptr<AST::BreakExpr> (
      new AST::BreakExpr (std::move (label), tl::nullopt,
			  std::move (outer_attrs), locus));
  else
    return tl::unexpected<Parse::Error::Node> (Parse::Error::Node::CHILD_ERROR);
}

// Parses a continue expression (including any label to continue from).
template <typename ManagedTokenSource>
std::unique_ptr<AST::ContinueExpr>
Parser<ManagedTokenSource>::parse_continue_expr (AST::AttrVec outer_attrs,
						 location_t pratt_parsed_loc)
{
  location_t locus = pratt_parsed_loc;
  if (locus == UNKNOWN_LOCATION)
    {
      locus = lexer.peek_token ()->get_locus ();
      skip_token (CONTINUE);
    }

  auto parsed_label = parse_lifetime (false);
  auto label = (parsed_label)
		 ? tl::optional<AST::Lifetime> (parsed_label.value ())
		 : tl::nullopt;

  return std::make_unique<AST::ContinueExpr> (std::move (label),
					      std::move (outer_attrs), locus);
}

/* Parses an if expression of any kind, including with else, else if, else if
 * let, and neither. Note that any outer attributes will be ignored because if
 * expressions don't support them. */
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::IfExpr>, Parse::Error::Node>
Parser<ManagedTokenSource>::parse_if_expr (AST::AttrVec outer_attrs,
					   location_t pratt_parsed_loc)
{
  // TODO: make having outer attributes an error?
  location_t locus = pratt_parsed_loc;
  if (locus == UNKNOWN_LOCATION)
    {
      locus = lexer.peek_token ()->get_locus ();
      if (!skip_token (IF))
	{
	  skip_after_end_block ();
	  return tl::unexpected<Parse::Error::Node> (
	    Parse::Error::Node::MALFORMED);
	}
    }

  // detect accidental if let
  if (lexer.peek_token ()->get_id () == LET)
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "if let expression probably exists, but is being parsed "
		   "as an if expression. This may be a parser error");
      add_error (std::move (error));

      // skip somewhere?
      return tl::unexpected<Parse::Error::Node> (Parse::Error::Node::MALFORMED);
    }

  /* parse required condition expr - HACK to prevent struct expr from being
   * parsed */
  ParseRestrictions no_struct_expr;
  no_struct_expr.can_be_struct_expr = false;
  auto condition = parse_expr ({}, no_struct_expr);
  if (!condition)
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "failed to parse condition expression in if expression");
      add_error (std::move (error));

      // skip somewhere?
      return tl::unexpected<Parse::Error::Node> (
	Parse::Error::Node::CHILD_ERROR);
    }

  // parse required block expr
  auto if_body = parse_block_expr ();
  if (!if_body)
    return tl::unexpected<Parse::Error::Node> (Parse::Error::Node::CHILD_ERROR);

  // branch to parse end or else (and then else, else if, or else if let)
  if (lexer.peek_token ()->get_id () != ELSE)
    {
      // single selection - end of if expression
      return std::unique_ptr<AST::IfExpr> (
	new AST::IfExpr (std::move (condition.value ()),
			 std::move (if_body.value ()), std::move (outer_attrs),
			 locus));
    }
  else
    {
      // double or multiple selection - branch on end, else if, or else if let

      // skip "else"
      lexer.skip_token ();

      // branch on whether next token is '{' or 'if'
      const_TokenPtr t = lexer.peek_token ();
      switch (t->get_id ())
	{
	case LEFT_CURLY:
	  {
	    // double selection - else
	    // parse else block expr (required)
	    auto else_body = parse_block_expr ();
	    if (!else_body)
	      {
		Error error (lexer.peek_token ()->get_locus (),
			     "failed to parse else body block expression in "
			     "if expression");
		add_error (std::move (error));

		// skip somewhere?
		return tl::unexpected<Parse::Error::Node> (
		  Parse::Error::Node::CHILD_ERROR);
	      }

	    return std::unique_ptr<AST::IfExprConseqElse> (
	      new AST::IfExprConseqElse (std::move (condition.value ()),
					 std::move (if_body.value ()),
					 std::move (else_body.value ()),
					 std::move (outer_attrs), locus));
	  }
	case IF:
	  {
	    // multiple selection - else if or else if let
	    // branch on whether next token is 'let' or not
	    if (lexer.peek_token (1)->get_id () == LET)
	      {
		// parse if let expr (required)
		auto if_let_expr = parse_if_let_expr ();
		if (!if_let_expr)
		  {
		    Error error (lexer.peek_token ()->get_locus (),
				 "failed to parse (else) if let expression "
				 "after if expression");
		    add_error (std::move (error));

		    // skip somewhere?
		    return tl::unexpected<Parse::Error::Node> (
		      Parse::Error::Node::CHILD_ERROR);
		  }

		return std::unique_ptr<AST::IfExprConseqElse> (
		  new AST::IfExprConseqElse (std::move (condition.value ()),
					     std::move (if_body.value ()),
					     std::move (if_let_expr.value ()),
					     std::move (outer_attrs), locus));
	      }
	    else
	      {
		// parse if expr (required)
		auto if_expr = parse_if_expr ();
		if (!if_expr)
		  {
		    Error error (lexer.peek_token ()->get_locus (),
				 "failed to parse (else) if expression after "
				 "if expression");
		    add_error (std::move (error));

		    // skip somewhere?
		    return tl::unexpected<Parse::Error::Node> (
		      Parse::Error::Node::CHILD_ERROR);
		  }

		return std::unique_ptr<AST::IfExprConseqElse> (
		  new AST::IfExprConseqElse (std::move (condition.value ()),
					     std::move (if_body.value ()),
					     std::move (if_expr.value ()),
					     std::move (outer_attrs), locus));
	      }
	  }
	default:
	  // error - invalid token
	  add_error (Error (t->get_locus (),
			    "unexpected token %qs after else in if expression",
			    t->get_token_description ()));

	  // skip somewhere?
	  return tl::unexpected<Parse::Error::Node> (
	    Parse::Error::Node::MALFORMED);
	}
    }
}

/* Parses an if let expression of any kind, including with else, else if, else
 * if let, and none. Note that any outer attributes will be ignored as if let
 * expressions don't support them. */
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::IfLetExpr>, Parse::Error::Node>
Parser<ManagedTokenSource>::parse_if_let_expr (AST::AttrVec outer_attrs,
					       location_t pratt_parsed_loc)
{
  // TODO: make having outer attributes an error?
  location_t locus = pratt_parsed_loc;
  if (locus == UNKNOWN_LOCATION)
    {
      locus = lexer.peek_token ()->get_locus ();
      if (!skip_token (IF))
	{
	  skip_after_end_block ();
	  return tl::unexpected<Parse::Error::Node> (
	    Parse::Error::Node::MALFORMED);
	}
    }

  // detect accidental if expr parsed as if let expr
  if (lexer.peek_token ()->get_id () != LET)
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "if expression probably exists, but is being parsed as an "
		   "if let expression. This may be a parser error");
      add_error (std::move (error));

      // skip somewhere?
      return tl::unexpected<Parse::Error::Node> (Parse::Error::Node::MALFORMED);
    }
  lexer.skip_token ();

  // parse match arm patterns (which are required)
  std::unique_ptr<AST::Pattern> match_arm_pattern
    = parse_match_arm_pattern (EQUAL);
  if (match_arm_pattern == nullptr)
    {
      Error error (
	lexer.peek_token ()->get_locus (),
	"failed to parse any match arm patterns in if let expression");
      add_error (std::move (error));

      // skip somewhere?
      return tl::unexpected<Parse::Error::Node> (
	Parse::Error::Node::CHILD_ERROR);
    }

  if (!skip_token (EQUAL))
    {
      // skip somewhere?
      return tl::unexpected<Parse::Error::Node> (Parse::Error::Node::MALFORMED);
    }

  // parse expression (required) - HACK to prevent struct expr being parsed
  ParseRestrictions no_struct_expr;
  no_struct_expr.can_be_struct_expr = false;
  auto scrutinee_expr = parse_expr ({}, no_struct_expr);
  if (!scrutinee_expr)
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "failed to parse scrutinee expression in if let expression");
      add_error (std::move (error));

      // skip somewhere?
      return tl::unexpected<Parse::Error::Node> (
	Parse::Error::Node::CHILD_ERROR);
    }
  /* TODO: check for expression not being a struct expression or lazy boolean
   * expression here? or actually probably in semantic analysis. */

  // parse block expression (required)
  auto if_let_body = parse_block_expr ();
  if (!if_let_body)
    {
      Error error (
	lexer.peek_token ()->get_locus (),
	"failed to parse if let body block expression in if let expression");
      add_error (std::move (error));

      // skip somewhere?
      return tl::unexpected<Parse::Error::Node> (
	Parse::Error::Node::CHILD_ERROR);
    }

  // branch to parse end or else (and then else, else if, or else if let)
  if (lexer.peek_token ()->get_id () != ELSE)
    {
      // single selection - end of if let expression
      return std::unique_ptr<AST::IfLetExpr> (new AST::IfLetExpr (
	std::move (match_arm_pattern), std::move (scrutinee_expr.value ()),
	std::move (if_let_body.value ()), std::move (outer_attrs), locus));
    }
  else
    {
      // double or multiple selection - branch on end, else if, or else if let

      // skip "else"
      lexer.skip_token ();

      // branch on whether next token is '{' or 'if'
      const_TokenPtr t = lexer.peek_token ();
      switch (t->get_id ())
	{
	case LEFT_CURLY:
	  {
	    // double selection - else
	    // parse else block expr (required)
	    auto else_body = parse_block_expr ();
	    if (!else_body)
	      {
		Error error (lexer.peek_token ()->get_locus (),
			     "failed to parse else body block expression in "
			     "if let expression");
		add_error (std::move (error));

		// skip somewhere?
		return tl::unexpected<Parse::Error::Node> (
		  Parse::Error::Node::CHILD_ERROR);
	      }

	    return std::unique_ptr<AST::IfLetExprConseqElse> (
	      new AST::IfLetExprConseqElse (std::move (match_arm_pattern),
					    std::move (scrutinee_expr.value ()),
					    std::move (if_let_body.value ()),
					    std::move (else_body.value ()),
					    std::move (outer_attrs), locus));
	  }
	case IF:
	  {
	    // multiple selection - else if or else if let
	    // branch on whether next token is 'let' or not
	    if (lexer.peek_token (1)->get_id () == LET)
	      {
		// parse if let expr (required)
		auto if_let_expr = parse_if_let_expr ();
		if (!if_let_expr)
		  {
		    Error error (lexer.peek_token ()->get_locus (),
				 "failed to parse (else) if let expression "
				 "after if let expression");
		    add_error (std::move (error));

		    // skip somewhere?
		    return tl::unexpected<Parse::Error::Node> (
		      Parse::Error::Node::CHILD_ERROR);
		  }

		return std::unique_ptr<AST::IfLetExprConseqElse> (
		  new AST::IfLetExprConseqElse (
		    std::move (match_arm_pattern),
		    std::move (scrutinee_expr.value ()),
		    std::move (if_let_body.value ()),
		    std::move (if_let_expr.value ()), std::move (outer_attrs),
		    locus));
	      }
	    else
	      {
		// parse if expr (required)
		auto if_expr = parse_if_expr ();
		if (!if_expr)
		  {
		    Error error (lexer.peek_token ()->get_locus (),
				 "failed to parse (else) if expression after "
				 "if let expression");
		    add_error (std::move (error));

		    // skip somewhere?
		    return tl::unexpected<Parse::Error::Node> (
		      Parse::Error::Node::CHILD_ERROR);
		  }

		return std::unique_ptr<AST::IfLetExprConseqElse> (
		  new AST::IfLetExprConseqElse (
		    std::move (match_arm_pattern),
		    std::move (scrutinee_expr.value ()),
		    std::move (if_let_body.value ()),
		    std::move (if_expr.value ()), std::move (outer_attrs),
		    locus));
	      }
	  }
	default:
	  // error - invalid token
	  add_error (
	    Error (t->get_locus (),
		   "unexpected token %qs after else in if let expression",
		   t->get_token_description ()));

	  // skip somewhere?
	  return tl::unexpected<Parse::Error::Node> (
	    Parse::Error::Node::MALFORMED);
	}
    }
}

/* TODO: possibly decide on different method of handling label (i.e. not
 * parameter) */

/* Parses a "loop" infinite loop expression. Label is not parsed and should be
 * parsed via parse_labelled_loop_expr, which would call this. */
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::LoopExpr>, Parse::Error::Node>
Parser<ManagedTokenSource>::parse_loop_expr (AST::AttrVec outer_attrs,
					     tl::optional<AST::LoopLabel> label,
					     location_t pratt_parsed_loc)
{
  location_t locus = pratt_parsed_loc;
  if (locus == UNKNOWN_LOCATION)
    {
      if (label)
	locus = label->get_locus ();
      else
	locus = lexer.peek_token ()->get_locus ();

      if (!skip_token (LOOP))
	{
	  skip_after_end_block ();
	  return tl::unexpected<Parse::Error::Node> (
	    Parse::Error::Node::MALFORMED);
	}
    }
  else
    {
      if (label)
	locus = label->get_locus ();
    }

  // parse loop body, which is required
  auto loop_body = parse_block_expr ();
  if (!loop_body)
    return tl::unexpected<Parse::Error::Node> (Parse::Error::Node::CHILD_ERROR);

  return std::unique_ptr<AST::LoopExpr> (
    new AST::LoopExpr (std::move (loop_body.value ()), locus, std::move (label),
		       std::move (outer_attrs)));
}

/* Parses a "while" loop expression. Label is not parsed and should be parsed
 * via parse_labelled_loop_expr, which would call this. */
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::WhileLoopExpr>, Parse::Error::Node>
Parser<ManagedTokenSource>::parse_while_loop_expr (
  AST::AttrVec outer_attrs, tl::optional<AST::LoopLabel> label,
  location_t pratt_parsed_loc)
{
  location_t locus = pratt_parsed_loc;
  if (locus == UNKNOWN_LOCATION)
    {
      if (label)
	locus = label->get_locus ();
      else
	locus = lexer.peek_token ()->get_locus ();

      if (!skip_token (WHILE))
	{
	  skip_after_end_block ();
	  return tl::unexpected<Parse::Error::Node> (
	    Parse::Error::Node::MALFORMED);
	}
    }
  else
    {
      if (label)
	locus = label->get_locus ();
    }

  // ensure it isn't a while let loop
  if (lexer.peek_token ()->get_id () == LET)
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "appears to be while let loop but is being parsed by "
		   "while loop - this may be a compiler issue");
      add_error (std::move (error));

      // skip somewhere?
      return tl::unexpected<Parse::Error::Node> (Parse::Error::Node::MALFORMED);
    }

  // parse loop predicate (required) with HACK to prevent struct expr parsing
  ParseRestrictions no_struct_expr;
  no_struct_expr.can_be_struct_expr = false;
  auto predicate = parse_expr ({}, no_struct_expr);
  if (!predicate)
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "failed to parse predicate expression in while loop");
      add_error (std::move (error));

      // skip somewhere?
      return tl::unexpected<Parse::Error::Node> (
	Parse::Error::Node::CHILD_ERROR);
    }
  /* TODO: check that it isn't struct expression here? actually, probably in
   * semantic analysis */

  // parse loop body (required)
  auto body = parse_block_expr ();
  if (!body)
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "failed to parse loop body block expression in while loop");
      add_error (std::move (error));

      // skip somewhere
      return tl::unexpected<Parse::Error::Node> (
	Parse::Error::Node::CHILD_ERROR);
    }

  return std::unique_ptr<AST::WhileLoopExpr> (
    new AST::WhileLoopExpr (std::move (predicate.value ()),
			    std::move (body.value ()), locus, std::move (label),
			    std::move (outer_attrs)));
}

/* Parses a "while let" loop expression. Label is not parsed and should be
 * parsed via parse_labelled_loop_expr, which would call this. */
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::WhileLetLoopExpr>, Parse::Error::Node>
Parser<ManagedTokenSource>::parse_while_let_loop_expr (
  AST::AttrVec outer_attrs, tl::optional<AST::LoopLabel> label)
{
  location_t locus = UNKNOWN_LOCATION;
  if (label)
    locus = label->get_locus ();
  else
    locus = lexer.peek_token ()->get_locus ();
  maybe_skip_token (WHILE);

  /* check for possible accidental recognition of a while loop as a while let
   * loop */
  if (lexer.peek_token ()->get_id () != LET)
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "appears to be a while loop but is being parsed by "
		   "while let loop - this may be a compiler issue");
      add_error (std::move (error));

      // skip somewhere
      return tl::unexpected<Parse::Error::Node> (Parse::Error::Node::MALFORMED);
    }
  // as this token is definitely let now, save the computation of comparison
  lexer.skip_token ();

  // parse predicate patterns
  std::unique_ptr<AST::Pattern> predicate_pattern
    = parse_match_arm_pattern (EQUAL);
  // ensure that there is at least 1 pattern
  if (predicate_pattern == nullptr)
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "should be at least 1 pattern");
      add_error (std::move (error));
      return tl::unexpected<Parse::Error::Node> (Parse::Error::Node::MALFORMED);
    }

  if (!skip_token (EQUAL))
    {
      // skip somewhere?
      return tl::unexpected<Parse::Error::Node> (Parse::Error::Node::MALFORMED);
    }

  /* parse predicate expression, which is required (and HACK to prevent struct
   * expr) */
  ParseRestrictions no_struct_expr;
  no_struct_expr.can_be_struct_expr = false;
  auto predicate_expr = parse_expr ({}, no_struct_expr);
  if (!predicate_expr)
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "failed to parse predicate expression in while let loop");
      add_error (std::move (error));

      // skip somewhere?
      return tl::unexpected<Parse::Error::Node> (
	Parse::Error::Node::CHILD_ERROR);
    }
  /* TODO: ensure that struct expression is not parsed? Actually, probably in
   * semantic analysis. */

  // parse loop body, which is required
  auto body = parse_block_expr ();
  if (!body)
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "failed to parse block expr (loop body) of while let loop");
      add_error (std::move (error));

      // skip somewhere?
      return tl::unexpected<Parse::Error::Node> (
	Parse::Error::Node::CHILD_ERROR);
    }

  return std::unique_ptr<AST::WhileLetLoopExpr> (
    new AST::WhileLetLoopExpr (std::move (predicate_pattern),
			       std::move (predicate_expr.value ()),
			       std::move (body.value ()), locus,
			       std::move (label), std::move (outer_attrs)));
}

/* Parses a "for" iterative loop. Label is not parsed and should be parsed via
 * parse_labelled_loop_expr, which would call this. */
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::ForLoopExpr>, Parse::Error::Node>
Parser<ManagedTokenSource>::parse_for_loop_expr (
  AST::AttrVec outer_attrs, tl::optional<AST::LoopLabel> label)
{
  location_t locus = UNKNOWN_LOCATION;
  if (label)
    locus = label->get_locus ();
  else
    locus = lexer.peek_token ()->get_locus ();
  maybe_skip_token (FOR);

  // parse pattern, which is required
  std::unique_ptr<AST::Pattern> pattern = parse_pattern ();
  if (!pattern)
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "failed to parse iterator pattern in for loop");
      add_error (std::move (error));

      // skip somewhere?
      return tl::unexpected<Parse::Error::Node> (
	Parse::Error::Node::CHILD_ERROR);
    }

  if (!skip_token (IN))
    {
      // skip somewhere?
      return tl::unexpected<Parse::Error::Node> (Parse::Error::Node::MALFORMED);
    }

  /* parse iterator expression, which is required - also HACK to prevent
   * struct expr */
  ParseRestrictions no_struct_expr;
  no_struct_expr.can_be_struct_expr = false;
  auto expr = parse_expr ({}, no_struct_expr);
  if (!expr)
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "failed to parse iterator expression in for loop");
      add_error (std::move (error));

      // skip somewhere?
      return tl::unexpected<Parse::Error::Node> (
	Parse::Error::Node::CHILD_ERROR);
    }
  // TODO: check to ensure this isn't struct expr? Or in semantic analysis.

  // parse loop body, which is required
  auto body = parse_block_expr ();
  if (!body)
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "failed to parse loop body block expression in for loop");
      add_error (std::move (error));

      // skip somewhere?
      return tl::unexpected<Parse::Error::Node> (
	Parse::Error::Node::CHILD_ERROR);
    }
  return std::unique_ptr<AST::ForLoopExpr> (
    new AST::ForLoopExpr (std::move (pattern), std::move (expr.value ()),
			  std::move (body.value ()), locus, std::move (label),
			  std::move (outer_attrs)));
}

// Parses a loop expression with label (any kind of loop - disambiguates).
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::Expr>, Parse::Error::Node>
Parser<ManagedTokenSource>::parse_labelled_loop_expr (const_TokenPtr tok,
						      AST::AttrVec outer_attrs)
{
  // parse loop label (required)
  auto parsed_label = parse_loop_label (tok);
  if (!parsed_label)
    {
      /* TODO: decide whether it should not work if there is no label, or parse
       * it with no label at the moment, I will make it not work with no label
       * because that's the implication. */

      if (parsed_label.error ().kind
	  == Parse::Error::LoopLabel::Kind::NOT_LOOP_LABEL)
	{
	  Error error (tok->get_locus (),
		       "expected lifetime in labelled loop expr (to parse loop "
		       "label) - found %qs",
		       tok->get_token_description ());
	  add_error (std::move (error));
	  return tl::unexpected<Parse::Error::Node> (
	    Parse::Error::Node::CHILD_ERROR);
	}

      else
	{
	  Error error (lexer.peek_token ()->get_locus (),
		       "failed to parse loop label in labelled loop expr");
	  add_error (std::move (error));

	  // skip?
	  return tl::unexpected<Parse::Error::Node> (
	    Parse::Error::Node::CHILD_ERROR);
	}
    }

  auto label = parsed_label
		 ? tl::optional<AST::LoopLabel> (parsed_label.value ())
		 : tl::nullopt;

  // branch on next token
  const_TokenPtr t = lexer.peek_token ();
  switch (t->get_id ())
    {
    case LOOP:
      return parse_loop_expr (std::move (outer_attrs), std::move (label));
    case FOR:
      return parse_for_loop_expr (std::move (outer_attrs), std::move (label));
    case WHILE:
      // further disambiguate into while vs while let
      if (lexer.peek_token (1)->get_id () == LET)
	return parse_while_let_loop_expr (std::move (outer_attrs),
					  std::move (label));
      else
	return parse_while_loop_expr (std::move (outer_attrs),
				      std::move (label));
    case LEFT_CURLY:
      return parse_block_expr (std::move (outer_attrs), std::move (label));
    default:
      // error
      add_error (Error (t->get_locus (),
			"unexpected token %qs when parsing labelled loop",
			t->get_token_description ()));

      // skip?
      return tl::unexpected<Parse::Error::Node> (
	Parse::Error::Node::CHILD_ERROR);
    }
}

// Parses a match expression.
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::MatchExpr>, Parse::Error::Node>
Parser<ManagedTokenSource>::parse_match_expr (AST::AttrVec outer_attrs,
					      location_t pratt_parsed_loc)
{
  location_t locus = pratt_parsed_loc;
  if (locus == UNKNOWN_LOCATION)
    {
      locus = lexer.peek_token ()->get_locus ();
      skip_token (MATCH_KW);
    }

  /* parse scrutinee expression, which is required (and HACK to prevent struct
   * expr) */
  ParseRestrictions no_struct_expr;
  no_struct_expr.can_be_struct_expr = false;
  auto scrutinee = parse_expr ({}, no_struct_expr);
  if (!scrutinee)
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "failed to parse scrutinee expression in match expression");
      add_error (std::move (error));

      // skip somewhere?
      return tl::unexpected<Parse::Error::Node> (
	Parse::Error::Node::CHILD_ERROR);
    }
  /* TODO: check for scrutinee expr not being struct expr? or do so in
   * semantic analysis */

  if (!skip_token (LEFT_CURLY))
    {
      // skip somewhere?
      return tl::unexpected<Parse::Error::Node> (Parse::Error::Node::MALFORMED);
    }

  // parse inner attributes (if they exist)
  AST::AttrVec inner_attrs = parse_inner_attributes ();

  // parse match arms (if they exist)
  // std::vector<std::unique_ptr<AST::MatchCase> > match_arms;
  std::vector<AST::MatchCase> match_arms;

  // parse match cases
  while (lexer.peek_token ()->get_id () != RIGHT_CURLY)
    {
      // parse match arm itself, which is required
      AST::MatchArm arm = parse_match_arm ();
      if (arm.is_error ())
	{
	  // TODO is this worth throwing everything away?
	  Error error (lexer.peek_token ()->get_locus (),
		       "failed to parse match arm in match arms");
	  add_error (std::move (error));

	  return tl::unexpected<Parse::Error::Node> (
	    Parse::Error::Node::CHILD_ERROR);
	}

      if (!skip_token (MATCH_ARROW))
	{
	  // skip after somewhere?
	  // TODO is returning here a good idea? or is break better?
	  return tl::unexpected<Parse::Error::Node> (
	    Parse::Error::Node::MALFORMED);
	}

      ParseRestrictions restrictions;
      restrictions.expr_can_be_stmt = true;

      auto expr = parse_expr ({}, restrictions);

      if (!expr)
	{
	  Error error (lexer.peek_token ()->get_locus (),
		       "failed to parse expr in match arm in match expr");
	  add_error (std::move (error));

	  // skip somewhere?
	  return tl::unexpected<Parse::Error::Node> (
	    Parse::Error::Node::CHILD_ERROR);
	}

      bool is_expr_without_block = expr.value ()->is_expr_without_block ();

      match_arms.push_back (
	AST::MatchCase (std::move (arm), std::move (expr.value ())));

      // handle comma presence
      if (lexer.peek_token ()->get_id () != COMMA)
	{
	  if (!is_expr_without_block)
	    {
	      // allowed even if not final case
	      continue;
	    }
	  else if (is_expr_without_block
		   && lexer.peek_token ()->get_id () != RIGHT_CURLY)
	    {
	      // not allowed if not final case
	      Error error (lexer.peek_token ()->get_locus (),
			   "exprwithoutblock requires comma after match case "
			   "expression in match arm (if not final case)");
	      add_error (std::move (error));

	      return tl::unexpected<Parse::Error::Node> (
		Parse::Error::Node::MALFORMED);
	    }
	  else
	    {
	      // otherwise, must be final case, so fine
	      break;
	    }
	}
      lexer.skip_token ();
    }

  if (!skip_token (RIGHT_CURLY))
    {
      // skip somewhere?
      return tl::unexpected<Parse::Error::Node> (Parse::Error::Node::MALFORMED);
    }

  match_arms.shrink_to_fit ();

  return std::unique_ptr<AST::MatchExpr> (
    new AST::MatchExpr (std::move (scrutinee.value ()), std::move (match_arms),
			std::move (inner_attrs), std::move (outer_attrs),
			locus));
}

// Parses an async block expression.
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::AsyncBlockExpr>, Parse::Error::Node>
Parser<ManagedTokenSource>::parse_async_block_expr (AST::AttrVec outer_attrs)
{
  location_t locus = lexer.peek_token ()->get_locus ();
  skip_token (ASYNC);

  // detect optional move token
  bool has_move = false;
  if (lexer.peek_token ()->get_id () == MOVE)
    {
      lexer.skip_token ();
      has_move = true;
    }

  // parse block expression (required)
  auto block_expr = parse_block_expr ();
  if (!block_expr)
    {
      Error error (
	lexer.peek_token ()->get_locus (),
	"failed to parse block expression of async block expression");
      add_error (std::move (error));

      // skip somewhere?
      return tl::unexpected<Parse::Error::Node> (
	Parse::Error::Node::CHILD_ERROR);
    }

  return std::unique_ptr<AST::AsyncBlockExpr> (
    new AST::AsyncBlockExpr (std::move (block_expr.value ()), has_move,
			     std::move (outer_attrs), locus));
}

// Parses an unsafe block expression.
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::UnsafeBlockExpr>, Parse::Error::Node>
Parser<ManagedTokenSource>::parse_unsafe_block_expr (
  AST::AttrVec outer_attrs, location_t pratt_parsed_loc)
{
  location_t locus = pratt_parsed_loc;
  if (locus == UNKNOWN_LOCATION)
    {
      locus = lexer.peek_token ()->get_locus ();
      skip_token (UNSAFE);
    }

  // parse block expression (required)
  auto block_expr = parse_block_expr ();
  if (!block_expr)
    {
      Error error (
	lexer.peek_token ()->get_locus (),
	"failed to parse block expression of unsafe block expression");
      add_error (std::move (error));

      // skip somewhere?
      return tl::unexpected<Parse::Error::Node> (
	Parse::Error::Node::CHILD_ERROR);
    }
  return std::unique_ptr<AST::UnsafeBlockExpr> (
    new AST::UnsafeBlockExpr (std::move (block_expr.value ()),
			      std::move (outer_attrs), locus));
}

// Parses an array definition expression.
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::ArrayExpr>, Parse::Error::Node>
Parser<ManagedTokenSource>::parse_array_expr (AST::AttrVec outer_attrs,
					      location_t pratt_parsed_loc)
{
  location_t locus = pratt_parsed_loc;
  if (locus == UNKNOWN_LOCATION)
    {
      locus = lexer.peek_token ()->get_locus ();
      skip_token (LEFT_SQUARE);
    }

  // parse optional inner attributes
  AST::AttrVec inner_attrs = parse_inner_attributes ();

  // parse the "array elements" section, which is optional
  if (lexer.peek_token ()->get_id () == RIGHT_SQUARE)
    {
      // no array elements
      lexer.skip_token ();

      std::vector<std::unique_ptr<AST::Expr>> exprs;
      auto array_elems
	= std::make_unique<AST::ArrayElemsValues> (std::move (exprs), locus);
      return std::make_unique<AST::ArrayExpr> (std::move (array_elems),
					       std::move (inner_attrs),
					       std::move (outer_attrs), locus);
    }
  else
    {
      // should have array elements
      // parse initial expression, which is required for either
      auto initial_expr = parse_expr ();
      if (!initial_expr)
	{
	  Error error (lexer.peek_token ()->get_locus (),
		       "could not parse expression in array expression "
		       "(even though arrayelems seems to be present)");
	  add_error (std::move (error));

	  // skip somewhere?
	  return tl::unexpected<Parse::Error::Node> (
	    Parse::Error::Node::CHILD_ERROR);
	}

      if (lexer.peek_token ()->get_id () == SEMICOLON)
	{
	  // copy array elems
	  lexer.skip_token ();

	  // parse copy amount expression (required)
	  auto copy_amount = parse_expr ();
	  if (!copy_amount)
	    {
	      Error error (lexer.peek_token ()->get_locus (),
			   "could not parse copy amount expression in array "
			   "expression (arrayelems)");
	      add_error (std::move (error));

	      // skip somewhere?
	      return tl::unexpected<Parse::Error::Node> (
		Parse::Error::Node::CHILD_ERROR);
	    }

	  skip_token (RIGHT_SQUARE);

	  std::unique_ptr<AST::ArrayElemsCopied> copied_array_elems (
	    new AST::ArrayElemsCopied (std::move (initial_expr.value ()),
				       std::move (copy_amount.value ()),
				       locus));
	  return std::unique_ptr<AST::ArrayExpr> (
	    new AST::ArrayExpr (std::move (copied_array_elems),
				std::move (inner_attrs),
				std::move (outer_attrs), locus));
	}
      else if (lexer.peek_token ()->get_id () == RIGHT_SQUARE)
	{
	  // single-element array expression
	  std::vector<std::unique_ptr<AST::Expr>> exprs;
	  exprs.reserve (1);
	  exprs.push_back (std::move (initial_expr.value ()));
	  exprs.shrink_to_fit ();

	  skip_token (RIGHT_SQUARE);

	  std::unique_ptr<AST::ArrayElemsValues> array_elems (
	    new AST::ArrayElemsValues (std::move (exprs), locus));
	  return std::unique_ptr<AST::ArrayExpr> (
	    new AST::ArrayExpr (std::move (array_elems),
				std::move (inner_attrs),
				std::move (outer_attrs), locus));
	}
      else if (lexer.peek_token ()->get_id () == COMMA)
	{
	  // multi-element array expression (or trailing comma)
	  std::vector<std::unique_ptr<AST::Expr>> exprs;
	  exprs.push_back (std::move (initial_expr.value ()));

	  const_TokenPtr t = lexer.peek_token ();
	  while (t->get_id () == COMMA)
	    {
	      lexer.skip_token ();

	      // quick break if right square bracket
	      if (lexer.peek_token ()->get_id () == RIGHT_SQUARE)
		break;

	      // parse expression (required)
	      auto expr = parse_expr ();
	      if (!expr)
		{
		  Error error (lexer.peek_token ()->get_locus (),
			       "failed to parse element in array expression");
		  add_error (std::move (error));

		  // skip somewhere?
		  return tl::unexpected<Parse::Error::Node> (
		    Parse::Error::Node::CHILD_ERROR);
		}
	      exprs.push_back (std::move (expr.value ()));

	      t = lexer.peek_token ();
	    }

	  skip_token (RIGHT_SQUARE);

	  exprs.shrink_to_fit ();

	  std::unique_ptr<AST::ArrayElemsValues> array_elems (
	    new AST::ArrayElemsValues (std::move (exprs), locus));
	  return std::unique_ptr<AST::ArrayExpr> (
	    new AST::ArrayExpr (std::move (array_elems),
				std::move (inner_attrs),
				std::move (outer_attrs), locus));
	}
      else
	{
	  // error
	  Error error (lexer.peek_token ()->get_locus (),
		       "unexpected token %qs in array expression (arrayelems)",
		       lexer.peek_token ()->get_token_description ());
	  add_error (std::move (error));

	  // skip somewhere?
	  return tl::unexpected<Parse::Error::Node> (
	    Parse::Error::Node::MALFORMED);
	}
    }
}

// Parses a grouped or tuple expression (disambiguates).
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::ExprWithoutBlock>, Parse::Error::Node>
Parser<ManagedTokenSource>::parse_grouped_or_tuple_expr (
  AST::AttrVec outer_attrs, location_t pratt_parsed_loc)
{
  // adjustment to allow Pratt parsing to reuse function without copy-paste
  location_t locus = pratt_parsed_loc;
  if (locus == UNKNOWN_LOCATION)
    {
      locus = lexer.peek_token ()->get_locus ();
      skip_token (LEFT_PAREN);
    }

  // parse optional inner attributes
  AST::AttrVec inner_attrs = parse_inner_attributes ();

  if (lexer.peek_token ()->get_id () == RIGHT_PAREN)
    {
      // must be empty tuple
      lexer.skip_token ();

      // create tuple with empty tuple elems
      return std::unique_ptr<AST::TupleExpr> (
	new AST::TupleExpr (std::vector<std::unique_ptr<AST::Expr>> (),
			    std::move (inner_attrs), std::move (outer_attrs),
			    locus));
    }

  // parse first expression (required)
  auto first_expr = parse_expr ();
  if (!first_expr)
    {
      Error error (lexer.peek_token ()->get_locus (),
		   "failed to parse expression in grouped or tuple expression");
      add_error (std::move (error));

      // skip after somewhere?
      return tl::unexpected<Parse::Error::Node> (
	Parse::Error::Node::CHILD_ERROR);
    }

  // detect whether grouped expression with right parentheses as next token
  if (lexer.peek_token ()->get_id () == RIGHT_PAREN)
    {
      // must be grouped expr
      lexer.skip_token ();

      // create grouped expr
      return std::unique_ptr<AST::GroupedExpr> (
	new AST::GroupedExpr (std::move (first_expr.value ()),
			      std::move (inner_attrs), std::move (outer_attrs),
			      locus));
    }
  else if (lexer.peek_token ()->get_id () == COMMA)
    {
      // tuple expr
      std::vector<std::unique_ptr<AST::Expr>> exprs;
      exprs.push_back (std::move (first_expr.value ()));

      // parse potential other tuple exprs
      const_TokenPtr t = lexer.peek_token ();
      while (t->get_id () == COMMA)
	{
	  lexer.skip_token ();

	  // break out if right paren
	  if (lexer.peek_token ()->get_id () == RIGHT_PAREN)
	    break;

	  // parse expr, which is now required
	  auto expr = parse_expr ();
	  if (!expr)
	    {
	      Error error (lexer.peek_token ()->get_locus (),
			   "failed to parse expr in tuple expr");
	      add_error (std::move (error));

	      // skip somewhere?
	      return tl::unexpected<Parse::Error::Node> (
		Parse::Error::Node::CHILD_ERROR);
	    }
	  exprs.push_back (std::move (expr.value ()));

	  t = lexer.peek_token ();
	}

      // skip right paren
      skip_token (RIGHT_PAREN);

      return std::unique_ptr<AST::TupleExpr> (
	new AST::TupleExpr (std::move (exprs), std::move (inner_attrs),
			    std::move (outer_attrs), locus));
    }
  else
    {
      // error
      const_TokenPtr t = lexer.peek_token ();
      Error error (t->get_locus (),
		   "unexpected token %qs in grouped or tuple expression "
		   "(parenthesised expression) - expected %<)%> for grouped "
		   "expr and %<,%> for tuple expr",
		   t->get_token_description ());
      add_error (std::move (error));

      // skip somewhere?
      return tl::unexpected<Parse::Error::Node> (Parse::Error::Node::MALFORMED);
    }
}

// Parses a struct expression field.
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::StructExprField>,
	     Parse::Error::StructExprField>
Parser<ManagedTokenSource>::parse_struct_expr_field ()
{
  AST::AttrVec outer_attrs = parse_outer_attributes ();
  const_TokenPtr t = lexer.peek_token ();
  switch (t->get_id ())
    {
    case IDENTIFIER:
      if (lexer.peek_token (1)->get_id () == COLON)
	{
	  // struct expr field with identifier and expr
	  Identifier ident = {t};
	  lexer.skip_token (1);

	  // parse expression (required)
	  auto expr = parse_expr ();
	  if (!expr)
	    {
	      Error error (t->get_locus (),
			   "failed to parse struct expression field with "
			   "identifier and expression");
	      add_error (std::move (error));

	      return tl::unexpected<Parse::Error::StructExprField> (
		Parse::Error::StructExprField::CHILD_ERROR);
	    }

	  return std::unique_ptr<AST::StructExprFieldIdentifierValue> (
	    new AST::StructExprFieldIdentifierValue (std::move (ident),
						     std::move (expr.value ()),
						     std::move (outer_attrs),
						     t->get_locus ()));
	}
      else
	{
	  // struct expr field with identifier only
	  Identifier ident{t};
	  lexer.skip_token ();

	  return std::unique_ptr<AST::StructExprFieldIdentifier> (
	    new AST::StructExprFieldIdentifier (std::move (ident),
						std::move (outer_attrs),
						t->get_locus ()));
	}
    case INT_LITERAL:
      {
	// parse tuple index field
	int index = atoi (t->get_str ().c_str ());
	lexer.skip_token ();

	if (!skip_token (COLON))
	  {
	    // skip somewhere?
	    return tl::unexpected<Parse::Error::StructExprField> (
	      Parse::Error::StructExprField::MALFORMED);
	  }

	// parse field expression (required)
	auto expr = parse_expr ();
	if (!expr)
	  {
	    Error error (t->get_locus (),
			 "failed to parse expr in struct (or enum) expr "
			 "field with tuple index");
	    add_error (std::move (error));

	    return tl::unexpected<Parse::Error::StructExprField> (
	      Parse::Error::StructExprField::CHILD_ERROR);
	  }

	return std::unique_ptr<AST::StructExprFieldIndexValue> (
	  new AST::StructExprFieldIndexValue (index, std::move (expr.value ()),
					      std::move (outer_attrs),
					      t->get_locus ()));
      }
    case DOT_DOT:
      /* this is a struct base and can't be parsed here, so just return
       * nothing without erroring */

      return tl::unexpected<Parse::Error::StructExprField> (
	Parse::Error::StructExprField::STRUCT_BASE);
    default:
      add_error (
	Error (t->get_locus (),
	       "unrecognised token %qs as first token of struct expr field - "
	       "expected identifier or integer literal",
	       t->get_token_description ()));

      return tl::unexpected<Parse::Error::StructExprField> (
	Parse::Error::StructExprField::MALFORMED);
    }
}

/* Pratt parser impl of parse_expr. FIXME: this is only provisional and
 * probably will be changed. */
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::Expr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_expr (int right_binding_power,
					AST::AttrVec outer_attrs,
					ParseRestrictions restrictions)
{
  const_TokenPtr current_token = lexer.peek_token ();
  // Special hack because we are allowed to return nullptr, in that case we
  // don't want to skip the token, since we don't actually parse it. But if
  // null isn't allowed it indicates an error, and we want to skip past that.
  // So return early if it is one of the tokens that ends an expression
  // (or at least cannot start a new expression).
  if (restrictions.expr_can_be_null)
    {
      TokenId id = current_token->get_id ();
      if (id == SEMICOLON || id == RIGHT_PAREN || id == RIGHT_CURLY
	  || id == RIGHT_SQUARE || id == COMMA || id == LEFT_CURLY)
	return tl::unexpected<Parse::Error::Expr> (
	  Parse::Error::Expr::NULL_EXPR);
    }

  ParseRestrictions null_denotation_restrictions = restrictions;
  null_denotation_restrictions.expr_can_be_stmt = false;

  // parse null denotation (unary part of expression)
  tl::expected<std::unique_ptr<AST::Expr>, Parse::Error::Expr> expr
    = null_denotation ({}, null_denotation_restrictions);
  if (!expr)
    return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::CHILD_ERROR);

  return left_denotations (std::move (expr), right_binding_power,
			   std::move (outer_attrs), restrictions);
}

// Parse expression with lowest left binding power.
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::Expr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_expr (AST::AttrVec outer_attrs,
					ParseRestrictions restrictions)
{
  return parse_expr (LBP_LOWEST, std::move (outer_attrs), restrictions);
}

template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::Expr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::left_denotations (
  tl::expected<std::unique_ptr<AST::Expr>, Parse::Error::Expr> expr,
  int right_binding_power, AST::AttrVec outer_attrs,
  ParseRestrictions restrictions)
{
  if (!expr)
    {
      // DEBUG
      rust_debug ("null denotation is null; returning null for parse_expr");
      return tl::unexpected<Parse::Error::Expr> (
	Parse::Error::Expr::NULL_DENOTATION);
    }

  const_TokenPtr current_token = lexer.peek_token ();

  if (restrictions.expr_can_be_stmt && !expr.value ()->is_expr_without_block ()
      && current_token->get_id () != DOT
      && current_token->get_id () != QUESTION_MARK)
    {
      rust_debug ("statement expression with block");
      expr.value ()->set_outer_attrs (std::move (outer_attrs));
      return expr;
    }

  restrictions.expr_can_be_stmt = false;

  // stop parsing if find lower priority token - parse higher priority first
  while (right_binding_power < left_binding_power (current_token))
    {
      lexer.skip_token ();

      // FIXME attributes should generally be applied to the null denotation.
      expr = left_denotation (current_token, std::move (expr.value ()),
			      std::move (outer_attrs), restrictions);

      if (!expr)
	{
	  // DEBUG
	  rust_debug ("left denotation is null; returning null for parse_expr");

	  return tl::unexpected<Parse::Error::Expr> (
	    Parse::Error::Expr::LEFT_DENOTATION);
	}

      current_token = lexer.peek_token ();
    }

  return expr;
}

/* Determines action to take when finding token at beginning of expression. */
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::Expr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::null_denotation (AST::AttrVec outer_attrs,
					     ParseRestrictions restrictions)
{
  /* note: tok is previous character in input stream, not current one, as
   * parse_expr skips it before passing it in */

  /* as a Pratt parser (which works by decomposing expressions into a null
   * denotation and then a left denotation), null denotations handle primaries
   * and unary operands (but only prefix unary operands) */

  auto tok = lexer.peek_token ();

  switch (tok->get_id ())
    {
    case IDENTIFIER:
    case SELF:
    case SELF_ALIAS:
    case DOLLAR_SIGN:
    case CRATE:
    case SUPER:
    case SCOPE_RESOLUTION:
      {
	// DEBUG
	rust_debug ("beginning null denotation identifier handling");

	/* best option: parse as path, then extract identifier, macro,
	 * struct/enum, or just path info from it */
	AST::PathInExpression path = parse_path_in_expression ();

	return null_denotation_path (std::move (path), std::move (outer_attrs),
				     restrictions);
      }
    case HASH:
      {
	// Parse outer attributes and then the expression that follows
	AST::AttrVec attrs = parse_outer_attributes ();

	// Merge with any existing outer attributes
	if (!outer_attrs.empty ())
	  attrs.insert (attrs.begin (), outer_attrs.begin (),
			outer_attrs.end ());

	// Try to parse the expression that should follow the attributes
	auto expr = parse_expr (std::move (attrs), restrictions);
	if (!expr)
	  {
	    /* If parsing failed and we're at a semicolon, provide a better
	     * error
	     */
	    const_TokenPtr next_tok = lexer.peek_token ();
	    if (next_tok->get_id () == SEMICOLON)
	      add_error (Error (next_tok->get_locus (),
				"expected expression, found %<;%>"));
	    return tl::unexpected<Parse::Error::Expr> (
	      Parse::Error::Expr::CHILD_ERROR);
	  }
	return expr;
      }
    default:
      if (tok->get_id () == LEFT_SHIFT)
	{
	  lexer.split_current_token (LEFT_ANGLE, LEFT_ANGLE);
	  tok = lexer.peek_token ();
	}

      lexer.skip_token ();
      return null_denotation_not_path (std::move (tok), std::move (outer_attrs),
				       restrictions);
    }
}

// Handling of expresions that start with a path for `null_denotation`.
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::Expr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::null_denotation_path (
  AST::PathInExpression path, AST::AttrVec outer_attrs,
  ParseRestrictions restrictions)
{
  rust_debug ("parsing null denotation after path");

  // HACK: always make "self" by itself a path (regardless of next
  // tokens)
  if (path.is_single_segment () && path.get_segments ()[0].is_lower_self_seg ())
    {
      // HACK: add outer attrs to path
      path.set_outer_attrs (std::move (outer_attrs));
      return std::make_unique<AST::PathInExpression> (std::move (path));
    }

  // branch on next token
  const_TokenPtr t = lexer.peek_token ();
  switch (t->get_id ())
    {
    case EXCLAM:
      // macro
      return parse_macro_invocation_partial (std::move (path),
					     std::move (outer_attrs));
    case LEFT_CURLY:
      {
	bool not_a_block = lexer.peek_token (1)->get_id () == IDENTIFIER
			   && (lexer.peek_token (2)->get_id () == COMMA
			       || (lexer.peek_token (2)->get_id () == COLON
				   && (lexer.peek_token (4)->get_id () == COMMA
				       || !Parse::Utils::can_tok_start_type (
					 lexer.peek_token (3)->get_id ()))));

	/* definitely not a block:
	 *  path '{' ident ','
	 *  path '{' ident ':' [anything] ','
	 *  path '{' ident ':' [not a type]
	 * otherwise, assume block expr and thus path */
	// DEBUG
	rust_debug ("values of lookahead: '%s' '%s' '%s' '%s' ",
		    lexer.peek_token (1)->get_token_description (),
		    lexer.peek_token (2)->get_token_description (),
		    lexer.peek_token (3)->get_token_description (),
		    lexer.peek_token (4)->get_token_description ());

	rust_debug ("can be struct expr: '%s', not a block: '%s'",
		    restrictions.can_be_struct_expr ? "true" : "false",
		    not_a_block ? "true" : "false");

	// struct/enum expr struct
	if (!restrictions.can_be_struct_expr && !not_a_block)
	  {
	    // HACK: add outer attrs to path
	    path.set_outer_attrs (std::move (outer_attrs));
	    return std::unique_ptr<AST::PathInExpression> (
	      new AST::PathInExpression (std::move (path)));
	  }
	return parse_struct_expr_struct_partial (std::move (path),
						 std::move (outer_attrs));
      }
    case LEFT_PAREN:
      // struct/enum expr tuple
      if (!restrictions.can_be_struct_expr)
	{
	  // assume path is returned
	  // HACK: add outer attributes to path
	  path.set_outer_attrs (std::move (outer_attrs));
	  return std::make_unique<AST::PathInExpression> (std::move (path));
	}
      return parse_struct_expr_tuple_partial (std::move (path),
					      std::move (outer_attrs));
    default:
      // assume path is returned if not single segment
      if (path.is_single_segment ())
	{
	  // FIXME: This should probably be returned as a path.
	  /* HACK: may have to become permanent, but this is my current
	   * identifier expression */
	  return std::unique_ptr<AST::IdentifierExpr> (new AST::IdentifierExpr (
	    path.get_segments ()[0].get_ident_segment ().as_string (), {},
	    path.get_locus ()));
	}
      // HACK: add outer attrs to path
      path.set_outer_attrs (std::move (outer_attrs));
      return std::unique_ptr<AST::PathInExpression> (
	new AST::PathInExpression (std::move (path)));
    }
  rust_unreachable ();
}

// Handling of expresions that do not start with a path for `null_denotation`.
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::Expr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::null_denotation_not_path (
  const_TokenPtr tok, AST::AttrVec outer_attrs, ParseRestrictions restrictions)
{
  switch (tok->get_id ())
    {
    // FIXME: Handle in null_denotation_path?
    case LEFT_SHIFT:
    case LEFT_ANGLE:
      {
	// qualified path
	// HACK: add outer attrs to path
	AST::QualifiedPathInExpression path
	  = parse_qualified_path_in_expression (tok->get_locus ());
	path.set_outer_attrs (std::move (outer_attrs));
	return std::make_unique<AST::QualifiedPathInExpression> (
	  std::move (path));
      }
    // FIXME: delegate to parse_literal_expr instead? would have to rejig
    // tokens and whatever.
    // FIXME: for literal exprs, outer attrs should be passed in, and later
    // error if it does not make up the entire statement.
    case INT_LITERAL:
      // we should check the range, but ignore for now
      // encode as int?
      return std::unique_ptr<AST::LiteralExpr> (
	new AST::LiteralExpr (tok->get_str (), AST::Literal::INT,
			      tok->get_type_hint (), {}, tok->get_locus ()));
    case FLOAT_LITERAL:
      // encode as float?
      return std::unique_ptr<AST::LiteralExpr> (
	new AST::LiteralExpr (tok->get_str (), AST::Literal::FLOAT,
			      tok->get_type_hint (), {}, tok->get_locus ()));
    case STRING_LITERAL:
      return std::unique_ptr<AST::LiteralExpr> (
	new AST::LiteralExpr (tok->get_str (), AST::Literal::STRING,
			      tok->get_type_hint (), {}, tok->get_locus ()));
    case BYTE_STRING_LITERAL:
      return std::unique_ptr<AST::LiteralExpr> (
	new AST::LiteralExpr (tok->get_str (), AST::Literal::BYTE_STRING,
			      tok->get_type_hint (), {}, tok->get_locus ()));
    case RAW_STRING_LITERAL:
      return std::unique_ptr<AST::LiteralExpr> (
	new AST::LiteralExpr (tok->get_str (), AST::Literal::RAW_STRING,
			      tok->get_type_hint (), {}, tok->get_locus ()));
    case CHAR_LITERAL:
      return std::unique_ptr<AST::LiteralExpr> (
	new AST::LiteralExpr (tok->get_str (), AST::Literal::CHAR,
			      tok->get_type_hint (), {}, tok->get_locus ()));
    case BYTE_CHAR_LITERAL:
      return std::unique_ptr<AST::LiteralExpr> (
	new AST::LiteralExpr (tok->get_str (), AST::Literal::BYTE,
			      tok->get_type_hint (), {}, tok->get_locus ()));
    case TRUE_LITERAL:
      return std::unique_ptr<AST::LiteralExpr> (
	new AST::LiteralExpr (Values::Keywords::TRUE_LITERAL,
			      AST::Literal::BOOL, tok->get_type_hint (), {},
			      tok->get_locus ()));
    case FALSE_LITERAL:
      return std::unique_ptr<AST::LiteralExpr> (
	new AST::LiteralExpr (Values::Keywords::FALSE_LITERAL,
			      AST::Literal::BOOL, tok->get_type_hint (), {},
			      tok->get_locus ()));
    case LEFT_PAREN:
      {
	auto grouped_or_tuple_expr
	  = parse_grouped_or_tuple_expr (std::move (outer_attrs),
					 tok->get_locus ());
	if (grouped_or_tuple_expr)
	  return std::move (grouped_or_tuple_expr.value ());
	else
	  return tl::unexpected<Parse::Error::Expr> (
	    Parse::Error::Expr::CHILD_ERROR);
      }

    /*case PLUS: { // unary plus operator
	// invoke parse_expr recursively with appropriate priority, etc. for
    below AST::Expr* expr = parse_expr(LBP_UNARY_PLUS);

	if (expr == nullptr)
	    return nullptr;
	// can only apply to integer and float expressions
	if (expr->get_type() != integer_type_node || expr->get_type() !=
    float_type_node) { rust_error_at(tok->get_locus(), "operand of unary
    plus must be int or float but it is %s", print_type(expr->get_type()));
    return nullptr;
	}

	return Tree(expr, tok->get_locus());
    }*/
    // Rust has no unary plus operator
    case MINUS:
      { // unary minus
	ParseRestrictions entered_from_unary;
	entered_from_unary.entered_from_unary = true;
	if (!restrictions.can_be_struct_expr)
	  entered_from_unary.can_be_struct_expr = false;
	auto expr = parse_expr (LBP_UNARY_MINUS, {}, entered_from_unary);

	if (!expr)
	  return tl::unexpected<Parse::Error::Expr> (
	    Parse::Error::Expr::CHILD_ERROR);
	// can only apply to integer and float expressions
	/*if (expr.get_type() != integer_type_node || expr.get_type() !=
	float_type_node) { rust_error_at(tok->get_locus(), "operand of unary
	minus must be int or float but it is %s",
	print_type(expr.get_type())); return Tree::error();
	}*/
	/* FIXME: when implemented the "get type" method on expr, ensure it is
	 * int or float type (except unsigned int). Actually, this would
	 * probably have to be done in semantic analysis (as type checking).
	 */

	/* FIXME: allow outer attributes on these expressions by having an
	 * outer attrs parameter in function*/
	return std::make_unique<AST::NegationExpr> (std::move (expr.value ()),
						    NegationOperator::NEGATE,
						    std::move (outer_attrs),
						    tok->get_locus ());
      }
    case EXCLAM:
      { // logical or bitwise not
	ParseRestrictions entered_from_unary;
	entered_from_unary.entered_from_unary = true;
	if (!restrictions.can_be_struct_expr)
	  entered_from_unary.can_be_struct_expr = false;
	auto expr = parse_expr (LBP_UNARY_EXCLAM, {}, entered_from_unary);

	if (!expr)
	  return tl::unexpected<Parse::Error::Expr> (
	    Parse::Error::Expr::CHILD_ERROR);
	// can only apply to boolean expressions
	/*if (expr.get_type() != boolean_type_node) {
	    rust_error_at(tok->get_locus(),
	      "operand of logical not must be a boolean but it is %s",
	      print_type(expr.get_type()));
	    return Tree::error();
	}*/
	/* FIXME: type checking for boolean or integer expressions in semantic
	 * analysis */

	// FIXME: allow outer attributes on these expressions
	return std::make_unique<AST::NegationExpr> (std::move (expr.value ()),
						    NegationOperator::NOT,
						    std::move (outer_attrs),
						    tok->get_locus ());
      }
    case ASTERISK:
      {
	/* pointer dereference only - HACK: as struct expressions should
	 * always be value expressions, cannot be dereferenced */
	ParseRestrictions entered_from_unary;
	entered_from_unary.entered_from_unary = true;
	entered_from_unary.can_be_struct_expr = false;
	auto expr = parse_expr (LBP_UNARY_ASTERISK, {}, entered_from_unary);
	if (!expr)
	  return tl::unexpected<Parse::Error::Expr> (
	    Parse::Error::Expr::CHILD_ERROR);
	// FIXME: allow outer attributes on expression
	return std::make_unique<AST::DereferenceExpr> (std::move (
							 expr.value ()),
						       std::move (outer_attrs),
						       tok->get_locus ());
      }
    case AMP:
      {
	// (single) "borrow" expression - shared (mutable) or immutable
	tl::expected<std::unique_ptr<AST::Expr>, Parse::Error::Expr> expr
	  = tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::MALFORMED);
	Mutability mutability = Mutability::Imm;
	bool raw_borrow = false;

	ParseRestrictions entered_from_unary;
	entered_from_unary.entered_from_unary = true;
	if (!restrictions.can_be_struct_expr)
	  entered_from_unary.can_be_struct_expr = false;

	auto is_mutability = [] (const_TokenPtr token) {
	  return token->get_id () == CONST || token->get_id () == MUT;
	};

	auto t = lexer.peek_token ();
	// Weak raw keyword, we look (1) ahead and treat it as an identifier if
	// there is no mut nor const.
	if (t->get_id () == IDENTIFIER
	    && t->get_str () == Values::WeakKeywords::RAW
	    && is_mutability (lexer.peek_token (1)))
	  {
	    lexer.skip_token ();
	    switch (lexer.peek_token ()->get_id ())
	      {
	      case MUT:
		mutability = Mutability::Mut;
		break;
	      case CONST:
		mutability = Mutability::Imm;
		break;
	      default:
		rust_error_at (lexer.peek_token ()->get_locus (),
			       "raw borrow should be either const or mut");
	      }
	    lexer.skip_token ();
	    auto expr_result
	      = parse_expr (LBP_UNARY_AMP_MUT, {}, entered_from_unary);
	    if (expr_result)
	      expr = std::move (expr_result.value ());
	    else
	      return tl::unexpected<Parse::Error::Expr> (
		Parse::Error::Expr::CHILD_ERROR);
	    raw_borrow = true;
	  }
	else if (t->get_id () == MUT)
	  {
	    lexer.skip_token ();
	    auto expr_result
	      = parse_expr (LBP_UNARY_AMP_MUT, {}, entered_from_unary);
	    if (expr_result)
	      expr = std::move (expr_result.value ());
	    else
	      return tl::unexpected<Parse::Error::Expr> (
		Parse::Error::Expr::CHILD_ERROR);
	    mutability = Mutability::Mut;
	    raw_borrow = false;
	  }
	else
	  {
	    auto expr_result
	      = parse_expr (LBP_UNARY_AMP, {}, entered_from_unary);
	    if (expr_result)
	      expr = std::move (expr_result.value ());
	    else
	      return tl::unexpected<Parse::Error::Expr> (
		Parse::Error::Expr::CHILD_ERROR);
	    raw_borrow = false;
	  }

	// FIXME: allow outer attributes on expression
	return std::make_unique<AST::BorrowExpr> (std::move (expr.value ()),
						  mutability, raw_borrow, false,
						  std::move (outer_attrs),
						  tok->get_locus ());
      }
    case LOGICAL_AND:
      {
	// (double) "borrow" expression - shared (mutable) or immutable
	std::unique_ptr<AST::Expr> expr = nullptr;
	Mutability mutability = Mutability::Imm;

	ParseRestrictions entered_from_unary;
	entered_from_unary.entered_from_unary = true;

	if (lexer.peek_token ()->get_id () == MUT)
	  {
	    lexer.skip_token ();
	    auto expr_res
	      = parse_expr (LBP_UNARY_AMP_MUT, {}, entered_from_unary);
	    if (!expr_res)
	      return tl::unexpected<Parse::Error::Expr> (
		Parse::Error::Expr::CHILD_ERROR);
	    expr = std::move (expr_res.value ());
	    mutability = Mutability::Mut;
	  }
	else
	  {
	    auto expr_result
	      = parse_expr (LBP_UNARY_AMP, {}, entered_from_unary);
	    if (expr_result)
	      expr = std::move (expr_result.value ());
	    else
	      return tl::unexpected<Parse::Error::Expr> (
		Parse::Error::Expr::CHILD_ERROR);
	    mutability = Mutability::Imm;
	  }

	// FIXME: allow outer attributes on expression
	return std::make_unique<AST::BorrowExpr> (std::move (expr), mutability,
						  false, true,
						  std::move (outer_attrs),
						  tok->get_locus ());
      }
    case OR:
    case PIPE:
    case MOVE:
      // closure expression
      {
	auto ret = parse_closure_expr_pratt (tok, std::move (outer_attrs));
	if (ret)
	  return std::move (ret.value ());
	else
	  return tl::unexpected<Parse::Error::Expr> (
	    Parse::Error::Expr::CHILD_ERROR);
      }
    case DOT_DOT:
      // either "range to" or "range full" expressions
      {
	auto ret
	  = parse_nud_range_exclusive_expr (tok, std::move (outer_attrs));
	if (ret)
	  return std::move (ret.value ());
	else
	  return tl::unexpected<Parse::Error::Expr> (
	    Parse::Error::Expr::CHILD_ERROR);
      }
    case DOT_DOT_EQ:
      // range to inclusive expr
      {
	auto ret = parse_range_to_inclusive_expr (tok, std::move (outer_attrs));
	if (ret)
	  return std::move (ret.value ());
	else
	  return tl::unexpected<Parse::Error::Expr> (
	    Parse::Error::Expr::CHILD_ERROR);
      }
    case RETURN_KW:
      // FIXME: is this really a null denotation expression?
      {
	auto ret
	  = parse_return_expr (std::move (outer_attrs), tok->get_locus ());
	if (ret)
	  return std::move (ret.value ());
	else
	  return tl::unexpected<Parse::Error::Expr> (
	    Parse::Error::Expr::CHILD_ERROR);
      }
    case TRY:
      // FIXME: is this really a null denotation expression?
      {
	auto ret = parse_try_expr (std::move (outer_attrs), tok->get_locus ());
	if (ret)
	  return std::move (ret.value ());
	else
	  return tl::unexpected<Parse::Error::Expr> (
	    Parse::Error::Expr::CHILD_ERROR);
      }
    case BREAK:
      // FIXME: is this really a null denotation expression?
      {
	auto ret
	  = parse_break_expr (std::move (outer_attrs), tok->get_locus ());
	if (ret)
	  return std::move (ret.value ());
	else
	  return tl::unexpected<Parse::Error::Expr> (
	    Parse::Error::Expr::CHILD_ERROR);
      }
    case CONTINUE:
      return parse_continue_expr (std::move (outer_attrs), tok->get_locus ());
    case LEFT_CURLY:
      // ok - this is an expression with block for once.
      {
	auto ret = parse_block_expr (std::move (outer_attrs), tl::nullopt,
				     tok->get_locus ());
	if (ret)
	  return std::move (ret.value ());
	else
	  return tl::unexpected<Parse::Error::Expr> (
	    Parse::Error::Expr::CHILD_ERROR);
      }
    case IF:
      // if or if let, so more lookahead to find out
      if (lexer.peek_token ()->get_id () == LET)
	{
	  // if let expr
	  auto ret
	    = parse_if_let_expr (std::move (outer_attrs), tok->get_locus ());
	  if (ret)
	    return std::move (ret.value ());
	  else
	    return tl::unexpected<Parse::Error::Expr> (
	      Parse::Error::Expr::CHILD_ERROR);
	}
      else
	{
	  // if expr
	  auto ret = parse_if_expr (std::move (outer_attrs), tok->get_locus ());
	  if (ret)
	    return std::move (ret.value ());
	  else
	    return tl::unexpected<Parse::Error::Expr> (
	      Parse::Error::Expr::CHILD_ERROR);
	}
    case LIFETIME:
      {
	auto ret = parse_labelled_loop_expr (tok, std::move (outer_attrs));
	if (ret)
	  return std::move (ret.value ());
	else
	  return tl::unexpected<Parse::Error::Expr> (
	    Parse::Error::Expr::CHILD_ERROR);
      }
    case LOOP:
      {
	auto ret = parse_loop_expr (std::move (outer_attrs), tl::nullopt,
				    tok->get_locus ());
	if (ret)
	  return std::move (ret.value ());
	else
	  return tl::unexpected<Parse::Error::Expr> (
	    Parse::Error::Expr::CHILD_ERROR);
      }
    case WHILE:
      if (lexer.peek_token ()->get_id () == LET)
	{
	  auto ret = parse_while_let_loop_expr (std::move (outer_attrs));
	  if (ret)
	    return std::move (ret.value ());
	  else
	    return tl::unexpected<Parse::Error::Expr> (
	      Parse::Error::Expr::CHILD_ERROR);
	}
      else
	{
	  auto ret = parse_while_loop_expr (std::move (outer_attrs),
					    tl::nullopt, tok->get_locus ());
	  if (ret)
	    return std::move (ret.value ());
	  else
	    return tl::unexpected<Parse::Error::Expr> (
	      Parse::Error::Expr::CHILD_ERROR);
	}
    case FOR:
      {
	auto ret = parse_for_loop_expr (std::move (outer_attrs), tl::nullopt);
	if (ret)
	  return std::move (ret.value ());
	else
	  return tl::unexpected<Parse::Error::Expr> (
	    Parse::Error::Expr::CHILD_ERROR);
      }
    case MATCH_KW:
      // also an expression with block
      {
	auto ret
	  = parse_match_expr (std::move (outer_attrs), tok->get_locus ());
	if (ret)
	  return std::move (ret.value ());
	else
	  return tl::unexpected<Parse::Error::Expr> (
	    Parse::Error::Expr::CHILD_ERROR);
      }

    case LEFT_SQUARE:
      // array definition expr (not indexing)
      {
	auto ret
	  = parse_array_expr (std::move (outer_attrs), tok->get_locus ());
	if (ret)
	  return std::move (ret.value ());
	else
	  return tl::unexpected<Parse::Error::Expr> (
	    Parse::Error::Expr::CHILD_ERROR);
      }
    case UNSAFE:
      {
	auto ret = parse_unsafe_block_expr (std::move (outer_attrs),
					    tok->get_locus ());
	if (ret)
	  return std::move (ret.value ());
	else
	  return tl::unexpected<Parse::Error::Expr> (
	    Parse::Error::Expr::CHILD_ERROR);
      }
    case BOX:
      {
	auto ret = parse_box_expr (std::move (outer_attrs), tok->get_locus ());
	if (ret)
	  return std::move (ret.value ());
	else
	  return tl::unexpected<Parse::Error::Expr> (
	    Parse::Error::Expr::CHILD_ERROR);
      }
    case UNDERSCORE:
      add_error (
	Error (tok->get_locus (),
	       "use of %qs is not allowed on the right-side of an assignment",
	       tok->get_token_description ()));
      return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::MALFORMED);
    case CONST:
      {
	auto ret
	  = parse_const_block_expr (std::move (outer_attrs), tok->get_locus ());
	if (ret)
	  return std::move (ret.value ());
	else
	  return tl::unexpected<Parse::Error::Expr> (
	    Parse::Error::Expr::CHILD_ERROR);
      }
    default:
      if (!restrictions.expr_can_be_null)
	add_error (Error (tok->get_locus (),
			  "found unexpected token %qs in null denotation",
			  tok->get_token_description ()));
      return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::MALFORMED);
    }
}

/* Called for each token that can appear in infix (between) position. Can be
 * operators or other punctuation. Returns a function pointer to member
 * function that implements the left denotation for the token given. */
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::Expr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::left_denotation (const_TokenPtr tok,
					     std::unique_ptr<AST::Expr> left,
					     AST::AttrVec outer_attrs,
					     ParseRestrictions restrictions)
{
  // Token passed in has already been skipped, so peek gives "next" token
  switch (tok->get_id ())
    {
    // FIXME: allow for outer attributes to be applied
    case QUESTION_MARK:
      {
	location_t left_locus = left->get_locus ();
	// error propagation expression - unary postfix
	return std::make_unique<AST::ErrorPropagationExpr> (
	  std::move (left), std::move (outer_attrs), left_locus);
      }
    case PLUS:
      // sum expression - binary infix
      /*return parse_binary_plus_expr (tok, std::move (left),
				     std::move (outer_attrs), restrictions);*/
      return parse_arithmetic_or_logical_expr (tok, std::move (left),
					       std::move (outer_attrs),
					       ArithmeticOrLogicalOperator::ADD,
					       restrictions);
    case MINUS:
      // difference expression - binary infix
      /*return parse_binary_minus_expr (tok, std::move (left),
				      std::move (outer_attrs),
	 restrictions);*/
      return parse_arithmetic_or_logical_expr (
	tok, std::move (left), std::move (outer_attrs),
	ArithmeticOrLogicalOperator::SUBTRACT, restrictions);
    case ASTERISK:
      // product expression - binary infix
      /*return parse_binary_mult_expr (tok, std::move (left),
				     std::move (outer_attrs), restrictions);*/
      return parse_arithmetic_or_logical_expr (
	tok, std::move (left), std::move (outer_attrs),
	ArithmeticOrLogicalOperator::MULTIPLY, restrictions);
    case DIV:
      // quotient expression - binary infix
      /*return parse_binary_div_expr (tok, std::move (left),
				    std::move (outer_attrs), restrictions);*/
      return parse_arithmetic_or_logical_expr (
	tok, std::move (left), std::move (outer_attrs),
	ArithmeticOrLogicalOperator::DIVIDE, restrictions);
    case PERCENT:
      // modulo expression - binary infix
      /*return parse_binary_mod_expr (tok, std::move (left),
				    std::move (outer_attrs), restrictions);*/
      return parse_arithmetic_or_logical_expr (
	tok, std::move (left), std::move (outer_attrs),
	ArithmeticOrLogicalOperator::MODULUS, restrictions);
    case AMP:
      // logical or bitwise and expression - binary infix
      /*return parse_bitwise_and_expr (tok, std::move (left),
				     std::move (outer_attrs), restrictions);*/
      return parse_arithmetic_or_logical_expr (
	tok, std::move (left), std::move (outer_attrs),
	ArithmeticOrLogicalOperator::BITWISE_AND, restrictions);
    case PIPE:
      // logical or bitwise or expression - binary infix
      /*return parse_bitwise_or_expr (tok, std::move (left),
				    std::move (outer_attrs), restrictions);*/
      return parse_arithmetic_or_logical_expr (
	tok, std::move (left), std::move (outer_attrs),
	ArithmeticOrLogicalOperator::BITWISE_OR, restrictions);
    case CARET:
      // logical or bitwise xor expression - binary infix
      /*return parse_bitwise_xor_expr (tok, std::move (left),
				     std::move (outer_attrs), restrictions);*/
      return parse_arithmetic_or_logical_expr (
	tok, std::move (left), std::move (outer_attrs),
	ArithmeticOrLogicalOperator::BITWISE_XOR, restrictions);
    case LEFT_SHIFT:
      // left shift expression - binary infix
      /*return parse_left_shift_expr (tok, std::move (left),
				    std::move (outer_attrs), restrictions);*/
      return parse_arithmetic_or_logical_expr (
	tok, std::move (left), std::move (outer_attrs),
	ArithmeticOrLogicalOperator::LEFT_SHIFT, restrictions);
    case RIGHT_SHIFT:
      // right shift expression - binary infix
      /*return parse_right_shift_expr (tok, std::move (left),
				     std::move (outer_attrs), restrictions);*/
      return parse_arithmetic_or_logical_expr (
	tok, std::move (left), std::move (outer_attrs),
	ArithmeticOrLogicalOperator::RIGHT_SHIFT, restrictions);
    case EQUAL_EQUAL:
      // equal to expression - binary infix (no associativity)
      /*return parse_binary_equal_expr (tok, std::move (left),
				      std::move (outer_attrs),
	 restrictions);*/
      return parse_comparison_expr (tok, std::move (left),
				    std::move (outer_attrs),
				    ComparisonOperator::EQUAL, restrictions);
    case NOT_EQUAL:
      // not equal to expression - binary infix (no associativity)
      /*return parse_binary_not_equal_expr (tok, std::move (left),
					  std::move (outer_attrs),
					  restrictions);*/
      return parse_comparison_expr (tok, std::move (left),
				    std::move (outer_attrs),
				    ComparisonOperator::NOT_EQUAL,
				    restrictions);
    case RIGHT_ANGLE:
      // greater than expression - binary infix (no associativity)
      /*return parse_binary_greater_than_expr (tok, std::move (left),
					     std::move (outer_attrs),
					     restrictions);*/
      return parse_comparison_expr (tok, std::move (left),
				    std::move (outer_attrs),
				    ComparisonOperator::GREATER_THAN,
				    restrictions);
    case LEFT_ANGLE:
      // less than expression - binary infix (no associativity)
      /*return parse_binary_less_than_expr (tok, std::move (left),
					  std::move (outer_attrs),
					  restrictions);*/
      return parse_comparison_expr (tok, std::move (left),
				    std::move (outer_attrs),
				    ComparisonOperator::LESS_THAN,
				    restrictions);
    case GREATER_OR_EQUAL:
      // greater than or equal to expression - binary infix (no associativity)
      /*return parse_binary_greater_equal_expr (tok, std::move (left),
					      std::move (outer_attrs),
					      restrictions);*/
      return parse_comparison_expr (tok, std::move (left),
				    std::move (outer_attrs),
				    ComparisonOperator::GREATER_OR_EQUAL,
				    restrictions);
    case LESS_OR_EQUAL:
      // less than or equal to expression - binary infix (no associativity)
      /*return parse_binary_less_equal_expr (tok, std::move (left),
					   std::move (outer_attrs),
					   restrictions);*/
      return parse_comparison_expr (tok, std::move (left),
				    std::move (outer_attrs),
				    ComparisonOperator::LESS_OR_EQUAL,
				    restrictions);
    case OR:
      // lazy logical or expression - binary infix
      return parse_lazy_or_expr (tok, std::move (left), std::move (outer_attrs),
				 restrictions);
    case LOGICAL_AND:
      // lazy logical and expression - binary infix
      return parse_lazy_and_expr (tok, std::move (left),
				  std::move (outer_attrs), restrictions);
    case AS:
      /* type cast expression - kind of binary infix (RHS is actually a
       * TypeNoBounds) */
      return parse_type_cast_expr (tok, std::move (left),
				   std::move (outer_attrs), restrictions);
    case EQUAL:
      // assignment expression - binary infix (note right-to-left
      // associativity)
      return parse_assig_expr (tok, std::move (left), std::move (outer_attrs),
			       restrictions);
    case PLUS_EQ:
      /* plus-assignment expression - binary infix (note right-to-left
       * associativity) */
      /*return parse_plus_assig_expr (tok, std::move (left),
				    std::move (outer_attrs), restrictions);*/
      return parse_compound_assignment_expr (tok, std::move (left),
					     std::move (outer_attrs),
					     CompoundAssignmentOperator::ADD,
					     restrictions);
    case MINUS_EQ:
      /* minus-assignment expression - binary infix (note right-to-left
       * associativity) */
      /*return parse_minus_assig_expr (tok, std::move (left),
				     std::move (outer_attrs), restrictions);*/
      return parse_compound_assignment_expr (
	tok, std::move (left), std::move (outer_attrs),
	CompoundAssignmentOperator::SUBTRACT, restrictions);
    case ASTERISK_EQ:
      /* multiply-assignment expression - binary infix (note right-to-left
       * associativity) */
      /*return parse_mult_assig_expr (tok, std::move (left),
				    std::move (outer_attrs), restrictions);*/
      return parse_compound_assignment_expr (
	tok, std::move (left), std::move (outer_attrs),
	CompoundAssignmentOperator::MULTIPLY, restrictions);
    case DIV_EQ:
      /* division-assignment expression - binary infix (note right-to-left
       * associativity) */
      /*return parse_div_assig_expr (tok, std::move (left),
				   std::move (outer_attrs), restrictions);*/
      return parse_compound_assignment_expr (tok, std::move (left),
					     std::move (outer_attrs),
					     CompoundAssignmentOperator::DIVIDE,
					     restrictions);
    case PERCENT_EQ:
      /* modulo-assignment expression - binary infix (note right-to-left
       * associativity) */
      /*return parse_mod_assig_expr (tok, std::move (left),
				   std::move (outer_attrs), restrictions);*/
      return parse_compound_assignment_expr (
	tok, std::move (left), std::move (outer_attrs),
	CompoundAssignmentOperator::MODULUS, restrictions);
    case AMP_EQ:
      /* bitwise and-assignment expression - binary infix (note right-to-left
       * associativity) */
      /*return parse_and_assig_expr (tok, std::move (left),
				   std::move (outer_attrs), restrictions);*/
      return parse_compound_assignment_expr (
	tok, std::move (left), std::move (outer_attrs),
	CompoundAssignmentOperator::BITWISE_AND, restrictions);
    case PIPE_EQ:
      /* bitwise or-assignment expression - binary infix (note right-to-left
       * associativity) */
      /*return parse_or_assig_expr (tok, std::move (left),
				  std::move (outer_attrs), restrictions);*/
      return parse_compound_assignment_expr (
	tok, std::move (left), std::move (outer_attrs),
	CompoundAssignmentOperator::BITWISE_OR, restrictions);
    case CARET_EQ:
      /* bitwise xor-assignment expression - binary infix (note right-to-left
       * associativity) */
      /*return parse_xor_assig_expr (tok, std::move (left),
				   std::move (outer_attrs), restrictions);*/
      return parse_compound_assignment_expr (
	tok, std::move (left), std::move (outer_attrs),
	CompoundAssignmentOperator::BITWISE_XOR, restrictions);
    case LEFT_SHIFT_EQ:
      /* left shift-assignment expression - binary infix (note right-to-left
       * associativity) */
      /*return parse_left_shift_assig_expr (tok, std::move (left),
					  std::move (outer_attrs),
					  restrictions);*/
      return parse_compound_assignment_expr (
	tok, std::move (left), std::move (outer_attrs),
	CompoundAssignmentOperator::LEFT_SHIFT, restrictions);
    case RIGHT_SHIFT_EQ:
      /* right shift-assignment expression - binary infix (note right-to-left
       * associativity) */
      /*return parse_right_shift_assig_expr (tok, std::move (left),
					   std::move (outer_attrs),
					   restrictions);*/
      return parse_compound_assignment_expr (
	tok, std::move (left), std::move (outer_attrs),
	CompoundAssignmentOperator::RIGHT_SHIFT, restrictions);
    case DOT_DOT:
      /* range exclusive expression - binary infix (no associativity)
       * either "range" or "range from" */
      return parse_led_range_exclusive_expr (tok, std::move (left),
					     std::move (outer_attrs),
					     restrictions);
    case DOT_DOT_EQ:
      /* range inclusive expression - binary infix (no associativity)
       * unambiguously RangeInclusiveExpr */
      return parse_range_inclusive_expr (tok, std::move (left),
					 std::move (outer_attrs), restrictions);
    case SCOPE_RESOLUTION:
      // path expression - binary infix? FIXME should this even be parsed
      // here?
      add_error (
	Error (tok->get_locus (),
	       "found scope resolution operator in left denotation "
	       "function - this should probably be handled elsewhere"));

      return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::MALFORMED);
    case DOT:
      {
	/* field expression or method call - relies on parentheses after next
	 * identifier or await if token after is "await" (unary postfix) or
	 * tuple index if token after is a decimal int literal */

	const_TokenPtr next_tok = lexer.peek_token ();
	if (next_tok->get_id () == IDENTIFIER
	    && next_tok->get_str () == Values::Keywords::AWAIT)
	  {
	    // await expression
	    return parse_await_expr (tok, std::move (left),
				     std::move (outer_attrs));
	  }
	else if (next_tok->get_id () == INT_LITERAL)
	  {
	    // tuple index expression - TODO check for decimal int literal
	    return parse_tuple_index_expr (tok, std::move (left),
					   std::move (outer_attrs),
					   restrictions);
	  }
	else if (next_tok->get_id () == FLOAT_LITERAL)
	  {
	    // Lexer has misidentified a tuple index as a float literal
	    // eg: `(x, (y, z)).1.0` -> 1.0 has been identified as a float
	    // literal. This means we should split it into three new separate
	    // tokens, the first tuple index, the dot and the second tuple
	    // index.
	    auto current_loc = next_tok->get_locus ();
	    auto str = next_tok->get_str ();
	    auto dot_pos = str.find (".");
	    auto prefix = str.substr (0, dot_pos);
	    auto suffix = str.substr (dot_pos + 1);
	    if (dot_pos == str.size () - 1)
	      lexer.split_current_token (
		{Token::make_int (current_loc, std::move (prefix),
				  CORETYPE_PURE_DECIMAL),
		 Token::make (DOT, current_loc + 1)});
	    else
	      lexer.split_current_token (
		{Token::make_int (current_loc, std::move (prefix),
				  CORETYPE_PURE_DECIMAL),
		 Token::make (DOT, current_loc + 1),
		 Token::make_int (current_loc + 2, std::move (suffix),
				  CORETYPE_PURE_DECIMAL)});
	    return parse_tuple_index_expr (tok, std::move (left),
					   std::move (outer_attrs),
					   restrictions);
	  }
	else if (next_tok->get_id () == IDENTIFIER
		 && lexer.peek_token (1)->get_id () != LEFT_PAREN
		 && lexer.peek_token (1)->get_id () != SCOPE_RESOLUTION)
	  {
	    /* field expression (or should be) - FIXME: scope resolution right
	     * after identifier should always be method, I'm pretty sure */
	    return parse_field_access_expr (tok, std::move (left),
					    std::move (outer_attrs),
					    restrictions);
	  }
	else
	  {
	    // method call (probably)
	    return parse_method_call_expr (tok, std::move (left),
					   std::move (outer_attrs),
					   restrictions);
	  }
      }
    case LEFT_PAREN:
      // function call - method call is based on dot notation first
      return parse_function_call_expr (tok, std::move (left),
				       std::move (outer_attrs), restrictions);
    case LEFT_SQUARE:
      // array or slice index expression (pseudo binary infix)
      return parse_index_expr (tok, std::move (left), std::move (outer_attrs),
			       restrictions);
    default:
      add_error (Error (tok->get_locus (),
			"found unexpected token %qs in left denotation",
			tok->get_token_description ()));

      return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::MALFORMED);
    }
}

/* Returns the left binding power for the given ArithmeticOrLogicalExpr type.
 * TODO make constexpr? Would that even do anything useful? */
inline binding_powers
get_lbp_for_arithmetic_or_logical_expr (
  AST::ArithmeticOrLogicalExpr::ExprType expr_type)
{
  switch (expr_type)
    {
    case ArithmeticOrLogicalOperator::ADD:
      return LBP_PLUS;
    case ArithmeticOrLogicalOperator::SUBTRACT:
      return LBP_MINUS;
    case ArithmeticOrLogicalOperator::MULTIPLY:
      return LBP_MUL;
    case ArithmeticOrLogicalOperator::DIVIDE:
      return LBP_DIV;
    case ArithmeticOrLogicalOperator::MODULUS:
      return LBP_MOD;
    case ArithmeticOrLogicalOperator::BITWISE_AND:
      return LBP_AMP;
    case ArithmeticOrLogicalOperator::BITWISE_OR:
      return LBP_PIPE;
    case ArithmeticOrLogicalOperator::BITWISE_XOR:
      return LBP_CARET;
    case ArithmeticOrLogicalOperator::LEFT_SHIFT:
      return LBP_L_SHIFT;
    case ArithmeticOrLogicalOperator::RIGHT_SHIFT:
      return LBP_R_SHIFT;
    default:
      // WTF? should not happen, this is an error
      rust_unreachable ();

      return LBP_PLUS;
    }
}

// Parses an arithmetic or logical expression (with Pratt parsing).
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::ArithmeticOrLogicalExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_arithmetic_or_logical_expr (
  const_TokenPtr, std::unique_ptr<AST::Expr> left, AST::AttrVec,
  AST::ArithmeticOrLogicalExpr::ExprType expr_type,
  ParseRestrictions restrictions)
{
  // parse RHS (as tok has already been consumed in parse_expression)
  auto right = parse_expr (get_lbp_for_arithmetic_or_logical_expr (expr_type),
			   AST::AttrVec (), restrictions);
  if (!right)
    return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::CHILD_ERROR);

  // TODO: check types. actually, do so during semantic analysis
  location_t locus = left->get_locus ();

  return std::make_unique<AST::ArithmeticOrLogicalExpr> (
    std::move (left), std::move (right.value ()), expr_type, locus);
}

// Parses a binary addition expression (with Pratt parsing).
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::ArithmeticOrLogicalExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_binary_plus_expr (
  const_TokenPtr tok ATTRIBUTE_UNUSED, std::unique_ptr<AST::Expr> left,
  AST::AttrVec outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions)
{
  // parse RHS (as tok has already been consumed in parse_expression)
  auto right = parse_expr (LBP_PLUS, AST::AttrVec (), restrictions);
  if (!right)
    return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::CHILD_ERROR);

  // TODO: check types. actually, do so during semantic analysis
  location_t locus = left->get_locus ();

  return std::make_unique<AST::ArithmeticOrLogicalExpr> (
    std::move (left), std::move (right.value ()),
    ArithmeticOrLogicalOperator::ADD, locus);
}

// Parses a binary subtraction expression (with Pratt parsing).
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::ArithmeticOrLogicalExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_binary_minus_expr (
  const_TokenPtr tok ATTRIBUTE_UNUSED, std::unique_ptr<AST::Expr> left,
  AST::AttrVec outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions)
{
  // parse RHS (as tok has already been consumed in parse_expression)
  auto right = parse_expr (LBP_MINUS, AST::AttrVec (), restrictions);
  if (!right)
    return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::CHILD_ERROR);

  // TODO: check types. actually, do so during semantic analysis
  location_t locus = left->get_locus ();

  return std::make_unique<AST::ArithmeticOrLogicalExpr> (
    std::move (left), std::move (right.value ()),
    ArithmeticOrLogicalOperator::SUBTRACT, locus);
}

// Parses a binary multiplication expression (with Pratt parsing).
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::ArithmeticOrLogicalExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_binary_mult_expr (
  const_TokenPtr tok ATTRIBUTE_UNUSED, std::unique_ptr<AST::Expr> left,
  AST::AttrVec outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions)
{
  // parse RHS (as tok has already been consumed in parse_expression)
  auto right = parse_expr (LBP_MUL, AST::AttrVec (), restrictions);
  if (!right)
    return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::CHILD_ERROR);

  // TODO: check types. actually, do so during semantic analysis
  location_t locus = left->get_locus ();

  return std::make_unique<AST::ArithmeticOrLogicalExpr> (
    std::move (left), std::move (right.value ()),
    ArithmeticOrLogicalOperator::MULTIPLY, locus);
}

// Parses a binary division expression (with Pratt parsing).
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::ArithmeticOrLogicalExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_binary_div_expr (
  const_TokenPtr tok ATTRIBUTE_UNUSED, std::unique_ptr<AST::Expr> left,
  AST::AttrVec outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions)
{
  // parse RHS (as tok has already been consumed in parse_expression)
  auto right = parse_expr (LBP_DIV, AST::AttrVec (), restrictions);
  if (!right)
    return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::CHILD_ERROR);

  // TODO: check types. actually, do so during semantic analysis
  location_t locus = left->get_locus ();

  return std::make_unique<AST::ArithmeticOrLogicalExpr> (
    std::move (left), std::move (right.value ()),
    ArithmeticOrLogicalOperator::DIVIDE, locus);
}

// Parses a binary modulo expression (with Pratt parsing).
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::ArithmeticOrLogicalExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_binary_mod_expr (
  const_TokenPtr tok ATTRIBUTE_UNUSED, std::unique_ptr<AST::Expr> left,
  AST::AttrVec outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions)
{
  // parse RHS (as tok has already been consumed in parse_expression)
  auto right = parse_expr (LBP_MOD, AST::AttrVec (), restrictions);
  if (!right)
    return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::CHILD_ERROR);

  // TODO: check types. actually, do so during semantic analysis
  location_t locus = left->get_locus ();

  return std::make_unique<AST::ArithmeticOrLogicalExpr> (
    std::move (left), std::move (right.value ()),
    ArithmeticOrLogicalOperator::MODULUS, locus);
}

/* Parses a binary bitwise (or eager logical) and expression (with Pratt
 * parsing). */
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::ArithmeticOrLogicalExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_bitwise_and_expr (
  const_TokenPtr tok ATTRIBUTE_UNUSED, std::unique_ptr<AST::Expr> left,
  AST::AttrVec outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions)
{
  // parse RHS (as tok has already been consumed in parse_expression)
  auto right = parse_expr (LBP_AMP, AST::AttrVec (), restrictions);
  if (!right)
    return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::CHILD_ERROR);

  // TODO: check types. actually, do so during semantic analysis
  location_t locus = left->get_locus ();

  return std::make_unique<AST::ArithmeticOrLogicalExpr> (
    std::move (left), std::move (right.value ()),
    ArithmeticOrLogicalOperator::BITWISE_AND, locus);
}

/* Parses a binary bitwise (or eager logical) or expression (with Pratt
 * parsing). */
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::ArithmeticOrLogicalExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_bitwise_or_expr (
  const_TokenPtr tok ATTRIBUTE_UNUSED, std::unique_ptr<AST::Expr> left,
  AST::AttrVec outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions)
{
  // parse RHS (as tok has already been consumed in parse_expression)
  auto right = parse_expr (LBP_PIPE, AST::AttrVec (), restrictions);
  if (!right)
    return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::CHILD_ERROR);

  // TODO: check types. actually, do so during semantic analysis
  location_t locus = left->get_locus ();

  return std::make_unique<AST::ArithmeticOrLogicalExpr> (
    std::move (left), std::move (right.value ()),
    ArithmeticOrLogicalOperator::BITWISE_OR, locus);
}

/* Parses a binary bitwise (or eager logical) xor expression (with Pratt
 * parsing). */
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::ArithmeticOrLogicalExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_bitwise_xor_expr (
  const_TokenPtr tok ATTRIBUTE_UNUSED, std::unique_ptr<AST::Expr> left,
  AST::AttrVec outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions)
{
  // parse RHS (as tok has already been consumed in parse_expression)
  auto right = parse_expr (LBP_CARET, AST::AttrVec (), restrictions);
  if (!right)
    return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::CHILD_ERROR);

  // TODO: check types. actually, do so during semantic analysis
  location_t locus = left->get_locus ();

  return std::make_unique<AST::ArithmeticOrLogicalExpr> (
    std::move (left), std::move (right.value ()),
    ArithmeticOrLogicalOperator::BITWISE_XOR, locus);
}

// Parses a binary left shift expression (with Pratt parsing).
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::ArithmeticOrLogicalExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_left_shift_expr (
  const_TokenPtr tok ATTRIBUTE_UNUSED, std::unique_ptr<AST::Expr> left,
  AST::AttrVec outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions)
{
  // parse RHS (as tok has already been consumed in parse_expression)
  auto right = parse_expr (LBP_L_SHIFT, AST::AttrVec (), restrictions);
  if (!right)
    return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::CHILD_ERROR);

  // TODO: check types. actually, do so during semantic analysis
  location_t locus = left->get_locus ();

  return std::make_unique<AST::ArithmeticOrLogicalExpr> (
    std::move (left), std::move (right.value ()),
    ArithmeticOrLogicalOperator::LEFT_SHIFT, locus);
}

// Parses a binary right shift expression (with Pratt parsing).
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::ArithmeticOrLogicalExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_right_shift_expr (
  const_TokenPtr tok ATTRIBUTE_UNUSED, std::unique_ptr<AST::Expr> left,
  AST::AttrVec outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions)
{
  // parse RHS (as tok has already been consumed in parse_expression)
  auto right = parse_expr (LBP_R_SHIFT, AST::AttrVec (), restrictions);
  if (!right)
    return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::CHILD_ERROR);

  // TODO: check types. actually, do so during semantic analysis
  location_t locus = left->get_locus ();

  return std::make_unique<AST::ArithmeticOrLogicalExpr> (
    std::move (left), std::move (right.value ()),
    ArithmeticOrLogicalOperator::RIGHT_SHIFT, locus);
}

/* Returns the left binding power for the given ComparisonExpr type.
 * TODO make constexpr? Would that even do anything useful? */
inline binding_powers
get_lbp_for_comparison_expr (AST::ComparisonExpr::ExprType expr_type)
{
  switch (expr_type)
    {
    case ComparisonOperator::EQUAL:
      return LBP_EQUAL;
    case ComparisonOperator::NOT_EQUAL:
      return LBP_NOT_EQUAL;
    case ComparisonOperator::GREATER_THAN:
      return LBP_GREATER_THAN;
    case ComparisonOperator::LESS_THAN:
      return LBP_SMALLER_THAN;
    case ComparisonOperator::GREATER_OR_EQUAL:
      return LBP_GREATER_EQUAL;
    case ComparisonOperator::LESS_OR_EQUAL:
      return LBP_SMALLER_EQUAL;
    default:
      // WTF? should not happen, this is an error
      rust_unreachable ();

      return LBP_EQUAL;
    }
}

/* Parses a ComparisonExpr of given type and LBP. TODO find a way to only
 * specify one and have the other looked up - e.g. specify ExprType and
 * binding power is looked up? */
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::ComparisonExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_comparison_expr (
  const_TokenPtr, std::unique_ptr<AST::Expr> left, AST::AttrVec,
  AST::ComparisonExpr::ExprType expr_type, ParseRestrictions restrictions)
{
  // parse RHS (as tok has already been consumed in parse_expression)
  auto right = parse_expr (get_lbp_for_comparison_expr (expr_type),
			   AST::AttrVec (), restrictions);
  if (!right)
    return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::CHILD_ERROR);

  // TODO: check types. actually, do so during semantic analysis
  location_t locus = left->get_locus ();

  return std::make_unique<AST::ComparisonExpr> (std::move (left),
						std::move (right.value ()),
						expr_type, locus);
}

// Parses a binary equal to expression (with Pratt parsing).
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::ComparisonExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_binary_equal_expr (
  const_TokenPtr tok ATTRIBUTE_UNUSED, std::unique_ptr<AST::Expr> left,
  AST::AttrVec outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions)
{
  // parse RHS (as tok has already been consumed in parse_expression)
  auto right = parse_expr (LBP_EQUAL, AST::AttrVec (), restrictions);
  if (!right)
    return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::CHILD_ERROR);

  // TODO: check types. actually, do so during semantic analysis
  location_t locus = left->get_locus ();

  return std::make_unique<AST::ComparisonExpr> (std::move (left),
						std::move (right.value ()),
						ComparisonOperator::EQUAL,
						locus);
}

// Parses a binary not equal to expression (with Pratt parsing).
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::ComparisonExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_binary_not_equal_expr (
  const_TokenPtr tok ATTRIBUTE_UNUSED, std::unique_ptr<AST::Expr> left,
  AST::AttrVec outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions)
{
  // parse RHS (as tok has already been consumed in parse_expression)
  auto right = parse_expr (LBP_NOT_EQUAL, AST::AttrVec (), restrictions);
  if (!right)
    return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::CHILD_ERROR);

  // TODO: check types. actually, do so during semantic analysis
  location_t locus = left->get_locus ();

  return std::make_unique<AST::ComparisonExpr> (std::move (left),
						std::move (right.value ()),
						ComparisonOperator::NOT_EQUAL,
						locus);
}

// Parses a binary greater than expression (with Pratt parsing).
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::ComparisonExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_binary_greater_than_expr (
  const_TokenPtr tok ATTRIBUTE_UNUSED, std::unique_ptr<AST::Expr> left,
  AST::AttrVec outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions)
{
  // parse RHS (as tok has already been consumed in parse_expression)
  auto right = parse_expr (LBP_GREATER_THAN, AST::AttrVec (), restrictions);
  if (!right)
    return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::CHILD_ERROR);

  // TODO: check types. actually, do so during semantic analysis
  location_t locus = left->get_locus ();

  return std::make_unique<AST::ComparisonExpr> (
    std::move (left), std::move (right.value ()),
    ComparisonOperator::GREATER_THAN, locus);
}

// Parses a binary less than expression (with Pratt parsing).
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::ComparisonExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_binary_less_than_expr (
  const_TokenPtr tok ATTRIBUTE_UNUSED, std::unique_ptr<AST::Expr> left,
  AST::AttrVec outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions)
{
  // parse RHS (as tok has already been consumed in parse_expression)
  auto right = parse_expr (LBP_SMALLER_THAN, AST::AttrVec (), restrictions);
  if (!right)
    return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::CHILD_ERROR);

  // TODO: check types. actually, do so during semantic analysis
  location_t locus = left->get_locus ();

  return std::make_unique<AST::ComparisonExpr> (std::move (left),
						std::move (right.value ()),
						ComparisonOperator::LESS_THAN,
						locus);
}

// Parses a binary greater than or equal to expression (with Pratt parsing).
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::ComparisonExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_binary_greater_equal_expr (
  const_TokenPtr tok ATTRIBUTE_UNUSED, std::unique_ptr<AST::Expr> left,
  AST::AttrVec outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions)
{
  // parse RHS (as tok has already been consumed in parse_expression)
  auto right = parse_expr (LBP_GREATER_EQUAL, AST::AttrVec (), restrictions);
  if (!right)
    return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::CHILD_ERROR);

  // TODO: check types. actually, do so during semantic analysis
  location_t locus = left->get_locus ();

  return std::make_unique<AST::ComparisonExpr> (
    std::move (left), std::move (right.value ()),
    ComparisonOperator::GREATER_OR_EQUAL, locus);
}

// Parses a binary less than or equal to expression (with Pratt parsing).
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::ComparisonExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_binary_less_equal_expr (
  const_TokenPtr tok ATTRIBUTE_UNUSED, std::unique_ptr<AST::Expr> left,
  AST::AttrVec outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions)
{
  // parse RHS (as tok has already been consumed in parse_expression)
  auto right = parse_expr (LBP_SMALLER_EQUAL, AST::AttrVec (), restrictions);
  if (!right)
    return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::CHILD_ERROR);

  // TODO: check types. actually, do so during semantic analysis
  location_t locus = left->get_locus ();

  return std::make_unique<AST::ComparisonExpr> (
    std::move (left), std::move (right.value ()),
    ComparisonOperator::LESS_OR_EQUAL, locus);
}

// Parses a binary lazy boolean or expression (with Pratt parsing).
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::LazyBooleanExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_lazy_or_expr (
  const_TokenPtr tok ATTRIBUTE_UNUSED, std::unique_ptr<AST::Expr> left,
  AST::AttrVec outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions)
{
  // parse RHS (as tok has already been consumed in parse_expression)
  auto right = parse_expr (LBP_LOGICAL_OR, AST::AttrVec (), restrictions);
  if (!right)
    return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::CHILD_ERROR);

  // TODO: check types. actually, do so during semantic analysis
  location_t locus = left->get_locus ();

  return std::make_unique<AST::LazyBooleanExpr> (
    std::move (left), std::move (right.value ()),
    LazyBooleanOperator::LOGICAL_OR, locus);
}

// Parses a binary lazy boolean and expression (with Pratt parsing).
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::LazyBooleanExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_lazy_and_expr (
  const_TokenPtr tok ATTRIBUTE_UNUSED, std::unique_ptr<AST::Expr> left,
  AST::AttrVec outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions)
{
  // parse RHS (as tok has already been consumed in parse_expression)
  auto right = parse_expr (LBP_LOGICAL_AND, AST::AttrVec (), restrictions);
  if (!right)
    return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::CHILD_ERROR);

  // TODO: check types. actually, do so during semantic analysis
  location_t locus = left->get_locus ();

  return std::make_unique<AST::LazyBooleanExpr> (
    std::move (left), std::move (right.value ()),
    LazyBooleanOperator::LOGICAL_AND, locus);
}

// Parses a pseudo-binary infix type cast expression (with Pratt parsing).
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::TypeCastExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_type_cast_expr (
  const_TokenPtr tok ATTRIBUTE_UNUSED, std::unique_ptr<AST::Expr> expr_to_cast,
  AST::AttrVec outer_attrs ATTRIBUTE_UNUSED,
  ParseRestrictions restrictions ATTRIBUTE_UNUSED)
{
  // parse RHS (as tok has already been consumed in parse_expression)
  auto type = parse_type_no_bounds ();
  if (!type)
    return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::CHILD_ERROR);
  // FIXME: how do I get precedence put in here?

  // TODO: check types. actually, do so during semantic analysis
  location_t locus = expr_to_cast->get_locus ();

  return std::make_unique<AST::TypeCastExpr> (std::move (expr_to_cast),
					      std::move (type), locus);
}

// Parses a binary assignment expression (with Pratt parsing).
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::AssignmentExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_assig_expr (
  const_TokenPtr tok ATTRIBUTE_UNUSED, std::unique_ptr<AST::Expr> left,
  AST::AttrVec outer_attrs, ParseRestrictions restrictions)
{
  // parse RHS (as tok has already been consumed in parse_expression)
  auto right = parse_expr (LBP_ASSIG - 1, AST::AttrVec (), restrictions);
  if (!right)
    return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::CHILD_ERROR);
  // FIXME: ensure right-associativity for this - 'LBP - 1' may do this?

  location_t locus = left->get_locus ();

  return std::make_unique<AST::AssignmentExpr> (std::move (left),
						std::move (right.value ()),
						std::move (outer_attrs), locus);
}

/* Returns the left binding power for the given CompoundAssignmentExpr type.
 * TODO make constexpr? Would that even do anything useful? */
inline binding_powers
get_lbp_for_compound_assignment_expr (
  AST::CompoundAssignmentExpr::ExprType expr_type)
{
  switch (expr_type)
    {
    case CompoundAssignmentOperator::ADD:
      return LBP_PLUS;
    case CompoundAssignmentOperator::SUBTRACT:
      return LBP_MINUS;
    case CompoundAssignmentOperator::MULTIPLY:
      return LBP_MUL;
    case CompoundAssignmentOperator::DIVIDE:
      return LBP_DIV;
    case CompoundAssignmentOperator::MODULUS:
      return LBP_MOD;
    case CompoundAssignmentOperator::BITWISE_AND:
      return LBP_AMP;
    case CompoundAssignmentOperator::BITWISE_OR:
      return LBP_PIPE;
    case CompoundAssignmentOperator::BITWISE_XOR:
      return LBP_CARET;
    case CompoundAssignmentOperator::LEFT_SHIFT:
      return LBP_L_SHIFT;
    case CompoundAssignmentOperator::RIGHT_SHIFT:
      return LBP_R_SHIFT;
    default:
      // WTF? should not happen, this is an error
      rust_unreachable ();

      return LBP_PLUS;
    }
}

// Parses a compound assignment expression (with Pratt parsing).
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::CompoundAssignmentExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_compound_assignment_expr (
  const_TokenPtr, std::unique_ptr<AST::Expr> left, AST::AttrVec,
  AST::CompoundAssignmentExpr::ExprType expr_type,
  ParseRestrictions restrictions)
{
  // parse RHS (as tok has already been consumed in parse_expression)
  auto right = parse_expr (get_lbp_for_compound_assignment_expr (expr_type) - 1,
			   AST::AttrVec (), restrictions);
  if (!right)
    return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::CHILD_ERROR);
  // FIXME: ensure right-associativity for this - 'LBP - 1' may do this?

  // TODO: check types. actually, do so during semantic analysis
  location_t locus = left->get_locus ();

  return std::make_unique<AST::CompoundAssignmentExpr> (
    std::move (left), std::move (right.value ()), expr_type, locus);
}

// Parses a binary add-assignment expression (with Pratt parsing).
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::CompoundAssignmentExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_plus_assig_expr (
  const_TokenPtr tok ATTRIBUTE_UNUSED, std::unique_ptr<AST::Expr> left,
  AST::AttrVec outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions)
{
  // parse RHS (as tok has already been consumed in parse_expression)
  auto right = parse_expr (LBP_PLUS_ASSIG - 1, AST::AttrVec (), restrictions);
  if (!right)
    return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::CHILD_ERROR);
  // FIXME: ensure right-associativity for this - 'LBP - 1' may do this?

  // TODO: check types. actually, do so during semantic analysis
  location_t locus = left->get_locus ();

  return std::make_unique<AST::CompoundAssignmentExpr> (
    std::move (left), std::move (right.value ()),
    CompoundAssignmentOperator::ADD, locus);
}

// Parses a binary minus-assignment expression (with Pratt parsing).
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::CompoundAssignmentExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_minus_assig_expr (
  const_TokenPtr tok ATTRIBUTE_UNUSED, std::unique_ptr<AST::Expr> left,
  AST::AttrVec outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions)
{
  // parse RHS (as tok has already been consumed in parse_expression)
  auto right = parse_expr (LBP_MINUS_ASSIG - 1, AST::AttrVec (), restrictions);
  if (!right)
    return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::CHILD_ERROR);
  // FIXME: ensure right-associativity for this - 'LBP - 1' may do this?

  // TODO: check types. actually, do so during semantic analysis
  location_t locus = left->get_locus ();

  return std::make_unique<AST::CompoundAssignmentExpr> (
    std::move (left), std::move (right.value ()),
    CompoundAssignmentOperator::SUBTRACT, locus);
}

// Parses a binary multiplication-assignment expression (with Pratt parsing).
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::CompoundAssignmentExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_mult_assig_expr (
  const_TokenPtr tok ATTRIBUTE_UNUSED, std::unique_ptr<AST::Expr> left,
  AST::AttrVec outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions)
{
  // parse RHS (as tok has already been consumed in parse_expression)
  auto right = parse_expr (LBP_MULT_ASSIG - 1, AST::AttrVec (), restrictions);
  if (!right)
    return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::CHILD_ERROR);
  // FIXME: ensure right-associativity for this - 'LBP - 1' may do this?

  // TODO: check types. actually, do so during semantic analysis
  location_t locus = left->get_locus ();

  return std::make_unique<AST::CompoundAssignmentExpr> (
    std::move (left), std::move (right.value ()),
    CompoundAssignmentOperator::MULTIPLY, locus);
}

// Parses a binary division-assignment expression (with Pratt parsing).
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::CompoundAssignmentExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_div_assig_expr (
  const_TokenPtr tok ATTRIBUTE_UNUSED, std::unique_ptr<AST::Expr> left,
  AST::AttrVec outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions)
{
  // parse RHS (as tok has already been consumed in parse_expression)
  auto right = parse_expr (LBP_DIV_ASSIG - 1, AST::AttrVec (), restrictions);
  if (!right)
    return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::CHILD_ERROR);
  // FIXME: ensure right-associativity for this - 'LBP - 1' may do this?

  // TODO: check types. actually, do so during semantic analysis
  location_t locus = left->get_locus ();

  return std::make_unique<AST::CompoundAssignmentExpr> (
    std::move (left), std::move (right.value ()),
    CompoundAssignmentOperator::DIVIDE, locus);
}

// Parses a binary modulo-assignment expression (with Pratt parsing).
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::CompoundAssignmentExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_mod_assig_expr (
  const_TokenPtr tok ATTRIBUTE_UNUSED, std::unique_ptr<AST::Expr> left,
  AST::AttrVec outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions)
{
  // parse RHS (as tok has already been consumed in parse_expression)
  auto right = parse_expr (LBP_MOD_ASSIG - 1, AST::AttrVec (), restrictions);
  if (!right)
    return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::CHILD_ERROR);
  // FIXME: ensure right-associativity for this - 'LBP - 1' may do this?

  // TODO: check types. actually, do so during semantic analysis
  location_t locus = left->get_locus ();

  return std::make_unique<AST::CompoundAssignmentExpr> (
    std::move (left), std::move (right.value ()),
    CompoundAssignmentOperator::MODULUS, locus);
}

// Parses a binary and-assignment expression (with Pratt parsing).
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::CompoundAssignmentExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_and_assig_expr (
  const_TokenPtr tok ATTRIBUTE_UNUSED, std::unique_ptr<AST::Expr> left,
  AST::AttrVec outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions)
{
  // parse RHS (as tok has already been consumed in parse_expression)
  auto right = parse_expr (LBP_AMP_ASSIG - 1, AST::AttrVec (), restrictions);
  if (!right)
    return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::CHILD_ERROR);
  // FIXME: ensure right-associativity for this - 'LBP - 1' may do this?

  // TODO: check types. actually, do so during semantic analysis
  location_t locus = left->get_locus ();

  return std::make_unique<AST::CompoundAssignmentExpr> (
    std::move (left), std::move (right.value ()),
    CompoundAssignmentOperator::BITWISE_AND, locus);
}

// Parses a binary or-assignment expression (with Pratt parsing).
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::CompoundAssignmentExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_or_assig_expr (
  const_TokenPtr tok ATTRIBUTE_UNUSED, std::unique_ptr<AST::Expr> left,
  AST::AttrVec outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions)
{
  // parse RHS (as tok has already been consumed in parse_expression)
  auto right = parse_expr (LBP_PIPE_ASSIG - 1, AST::AttrVec (), restrictions);
  if (!right)
    return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::CHILD_ERROR);
  // FIXME: ensure right-associativity for this - 'LBP - 1' may do this?

  // TODO: check types. actually, do so during semantic analysis
  location_t locus = left->get_locus ();

  return std::make_unique<AST::CompoundAssignmentExpr> (
    std::move (left), std::move (right.value ()),
    CompoundAssignmentOperator::BITWISE_OR, locus);
}

// Parses a binary xor-assignment expression (with Pratt parsing).
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::CompoundAssignmentExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_xor_assig_expr (
  const_TokenPtr tok ATTRIBUTE_UNUSED, std::unique_ptr<AST::Expr> left,
  AST::AttrVec outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions)
{
  // parse RHS (as tok has already been consumed in parse_expression)
  auto right = parse_expr (LBP_CARET_ASSIG - 1, AST::AttrVec (), restrictions);
  if (!right)
    return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::CHILD_ERROR);
  // FIXME: ensure right-associativity for this - 'LBP - 1' may do this?

  // TODO: check types. actually, do so during semantic analysis
  location_t locus = left->get_locus ();

  return std::make_unique<AST::CompoundAssignmentExpr> (
    std::move (left), std::move (right.value ()),
    CompoundAssignmentOperator::BITWISE_XOR, locus);
}

// Parses a binary left shift-assignment expression (with Pratt parsing).
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::CompoundAssignmentExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_left_shift_assig_expr (
  const_TokenPtr tok ATTRIBUTE_UNUSED, std::unique_ptr<AST::Expr> left,
  AST::AttrVec outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions)
{
  // parse RHS (as tok has already been consumed in parse_expression)
  auto right
    = parse_expr (LBP_L_SHIFT_ASSIG - 1, AST::AttrVec (), restrictions);
  if (!right)
    return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::CHILD_ERROR);
  // FIXME: ensure right-associativity for this - 'LBP - 1' may do this?

  // TODO: check types. actually, do so during semantic analysis
  location_t locus = left->get_locus ();

  return std::make_unique<AST::CompoundAssignmentExpr> (
    std::move (left), std::move (right.value ()),
    CompoundAssignmentOperator::LEFT_SHIFT, locus);
}

// Parses a binary right shift-assignment expression (with Pratt parsing).
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::CompoundAssignmentExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_right_shift_assig_expr (
  const_TokenPtr tok ATTRIBUTE_UNUSED, std::unique_ptr<AST::Expr> left,
  AST::AttrVec outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions)
{
  // parse RHS (as tok has already been consumed in parse_expression)
  auto right
    = parse_expr (LBP_R_SHIFT_ASSIG - 1, AST::AttrVec (), restrictions);
  if (!right)
    return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::CHILD_ERROR);
  // FIXME: ensure right-associativity for this - 'LBP - 1' may do this?

  // TODO: check types. actually, do so during semantic analysis
  location_t locus = left->get_locus ();

  return std::make_unique<AST::CompoundAssignmentExpr> (
    std::move (left), std::move (right.value ()),
    CompoundAssignmentOperator::RIGHT_SHIFT, locus);
}

// Parses a postfix unary await expression (with Pratt parsing).
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::AwaitExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_await_expr (
  const_TokenPtr tok, std::unique_ptr<AST::Expr> expr_to_await,
  AST::AttrVec outer_attrs)
{
  /* skip "await" identifier (as "." has already been consumed in
   * parse_expression) this assumes that the identifier was already identified
   * as await */
  if (!skip_token (IDENTIFIER))
    {
      Error error (tok->get_locus (), "failed to skip %<await%> in await expr "
				      "- this is probably a deep issue");
      add_error (std::move (error));

      // skip somewhere?
      return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::MALFORMED);
    }

  // TODO: check inside async block in semantic analysis
  location_t locus = expr_to_await->get_locus ();

  return std::unique_ptr<AST::AwaitExpr> (
    new AST::AwaitExpr (std::move (expr_to_await), std::move (outer_attrs),
			locus));
}

/* Parses an exclusive range ('..') in left denotation position (i.e.
 * RangeFromExpr or RangeFromToExpr). */
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::RangeExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_led_range_exclusive_expr (
  const_TokenPtr tok ATTRIBUTE_UNUSED, std::unique_ptr<AST::Expr> left,
  AST::AttrVec outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions)
{
  // FIXME: this probably parses expressions accidently or whatever
  // try parsing RHS (as tok has already been consumed in parse_expression)
  // Can be nullptr, in which case it is a RangeFromExpr, otherwise a
  // RangeFromToExpr.
  restrictions.expr_can_be_null = true;
  auto right = parse_expr (LBP_DOT_DOT, AST::AttrVec (), restrictions);

  location_t locus = left->get_locus ();

  if (!right)
    {
      // range from expr
      return std::make_unique<AST::RangeFromExpr> (std::move (left), locus);
    }
  else
    {
      return std::make_unique<AST::RangeFromToExpr> (std::move (left),
						     std::move (right.value ()),
						     locus);
    }
  // FIXME: make non-associative
}

/* Parses an exclusive range ('..') in null denotation position (i.e.
 * RangeToExpr or RangeFullExpr). */
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::RangeExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_nud_range_exclusive_expr (
  const_TokenPtr tok, AST::AttrVec outer_attrs ATTRIBUTE_UNUSED)
{
  auto restrictions = ParseRestrictions ();
  restrictions.expr_can_be_null = true;

  // FIXME: this probably parses expressions accidently or whatever
  // try parsing RHS (as tok has already been consumed in parse_expression)
  auto right = parse_expr (LBP_DOT_DOT, AST::AttrVec (), restrictions);

  location_t locus = tok->get_locus ();

  if (!right)
    {
      // range from expr
      return std::make_unique<AST::RangeFullExpr> (locus);
    }
  else
    {
      return std::make_unique<AST::RangeToExpr> (std::move (right.value ()),
						 locus);
    }
  // FIXME: make non-associative
}

// Parses a full binary range inclusive expression.
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::RangeFromToInclExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_range_inclusive_expr (
  const_TokenPtr tok ATTRIBUTE_UNUSED, std::unique_ptr<AST::Expr> left,
  AST::AttrVec outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions)
{
  // parse RHS (as tok has already been consumed in parse_expression)
  auto right = parse_expr (LBP_DOT_DOT_EQ, AST::AttrVec (), restrictions);
  if (!right)
    return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::CHILD_ERROR);
  // FIXME: make non-associative

  // TODO: check types. actually, do so during semantic analysis
  location_t locus = left->get_locus ();

  return std::make_unique<AST::RangeFromToInclExpr> (std::move (left),
						     std::move (right.value ()),
						     locus);
}

// Parses an inclusive range-to prefix unary expression.
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::RangeToInclExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_range_to_inclusive_expr (
  const_TokenPtr tok, AST::AttrVec outer_attrs ATTRIBUTE_UNUSED)
{
  // parse RHS (as tok has already been consumed in parse_expression)
  auto right = parse_expr (LBP_DOT_DOT_EQ);
  if (!right)
    return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::CHILD_ERROR);
  // FIXME: make non-associative

  // TODO: check types. actually, do so during semantic analysis

  return std::make_unique<AST::RangeToInclExpr> (std::move (right.value ()),
						 tok->get_locus ());
}

// Parses a pseudo-binary infix tuple index expression.
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::TupleIndexExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_tuple_index_expr (
  const_TokenPtr tok ATTRIBUTE_UNUSED, std::unique_ptr<AST::Expr> tuple_expr,
  AST::AttrVec outer_attrs, ParseRestrictions restrictions ATTRIBUTE_UNUSED)
{
  // parse int literal (as token already skipped)
  const_TokenPtr index_tok = expect_token (INT_LITERAL);
  if (index_tok == nullptr)
    return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::MALFORMED);

  std::string index = index_tok->get_str ();

  // convert to integer
  if (!index_tok->is_pure_decimal ())
    {
      Error error (index_tok->get_locus (),
		   "tuple index should be a pure decimal literal");
      add_error (std::move (error));
    }
  int index_int = atoi (index.c_str ());

  location_t locus = tuple_expr->get_locus ();

  return std::make_unique<AST::TupleIndexExpr> (std::move (tuple_expr),
						index_int,
						std::move (outer_attrs), locus);
}

// Parses a pseudo-binary infix array (or slice) index expression.
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::ArrayIndexExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_index_expr (
  const_TokenPtr, std::unique_ptr<AST::Expr> array_expr,
  AST::AttrVec outer_attrs, ParseRestrictions)
{
  // parse RHS (as tok has already been consumed in parse_expression)
  /*std::unique_ptr<AST::Expr> index_expr
    = parse_expr (LBP_ARRAY_REF, AST::AttrVec (),
    restrictions);*/
  // TODO: conceptually, should treat [] as brackets, so just parse all expr
  auto index_expr = parse_expr ();
  if (!index_expr)
    return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::CHILD_ERROR);

  // skip ']' at end of array
  if (!skip_token (RIGHT_SQUARE))
    {
      // skip somewhere?
      return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::MALFORMED);
    }

  // TODO: check types. actually, do so during semantic analysis
  location_t locus = array_expr->get_locus ();

  return std::make_unique<AST::ArrayIndexExpr> (std::move (array_expr),
						std::move (index_expr.value ()),
						std::move (outer_attrs), locus);
}

// Parses a pseudo-binary infix struct field access expression.
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::FieldAccessExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_field_access_expr (
  const_TokenPtr tok ATTRIBUTE_UNUSED, std::unique_ptr<AST::Expr> struct_expr,
  AST::AttrVec outer_attrs, ParseRestrictions restrictions ATTRIBUTE_UNUSED)
{
  /* get field name identifier (assume that this is a field access expr and
   * not await, for instance) */
  const_TokenPtr ident_tok = expect_token (IDENTIFIER);
  if (ident_tok == nullptr)
    return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::MALFORMED);

  Identifier ident{ident_tok};

  location_t locus = struct_expr->get_locus ();

  // TODO: check types. actually, do so during semantic analysis
  return std::make_unique<AST::FieldAccessExpr> (std::move (struct_expr),
						 std::move (ident),
						 std::move (outer_attrs),
						 locus);
}

// Parses a pseudo-binary infix method call expression.
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::MethodCallExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_method_call_expr (
  const_TokenPtr tok, std::unique_ptr<AST::Expr> receiver_expr,
  AST::AttrVec outer_attrs, ParseRestrictions)
{
  // parse path expr segment
  AST::PathExprSegment segment = parse_path_expr_segment ();
  if (segment.is_error ())
    {
      Error error (tok->get_locus (),
		   "failed to parse path expr segment of method call expr");
      add_error (std::move (error));

      return tl::unexpected<Parse::Error::Expr> (
	Parse::Error::Expr::CHILD_ERROR);
    }

  // skip left parentheses
  if (!skip_token (LEFT_PAREN))
    {
      return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::MALFORMED);
    }

  // parse method params (if they exist)
  std::vector<std::unique_ptr<AST::Expr>> params;

  const_TokenPtr t = lexer.peek_token ();
  while (t->get_id () != RIGHT_PAREN)
    {
      auto param = parse_expr ();
      if (!param)
	{
	  Error error (t->get_locus (),
		       "failed to parse method param in method call");
	  add_error (std::move (error));

	  return tl::unexpected<Parse::Error::Expr> (
	    Parse::Error::Expr::CHILD_ERROR);
	}
      params.push_back (std::move (param.value ()));

      if (lexer.peek_token ()->get_id () != COMMA)
	break;

      lexer.skip_token ();
      t = lexer.peek_token ();
    }

  // skip right paren
  if (!skip_token (RIGHT_PAREN))
    {
      return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::MALFORMED);
    }

  // TODO: check types. actually do so in semantic analysis pass.
  location_t locus = receiver_expr->get_locus ();

  return std::make_unique<AST::MethodCallExpr> (std::move (receiver_expr),
						std::move (segment),
						std::move (params),
						std::move (outer_attrs), locus);
}

// Parses a pseudo-binary infix function call expression.
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::CallExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_function_call_expr (
  const_TokenPtr, std::unique_ptr<AST::Expr> function_expr,
  AST::AttrVec outer_attrs, ParseRestrictions)
{
  // parse function params (if they exist)
  std::vector<std::unique_ptr<AST::Expr>> params;

  const_TokenPtr t = lexer.peek_token ();
  while (t->get_id () != RIGHT_PAREN)
    {
      auto param = parse_expr ();
      if (!param)
	{
	  Error error (t->get_locus (),
		       "failed to parse function param in function call");
	  add_error (std::move (error));

	  return tl::unexpected<Parse::Error::Expr> (
	    Parse::Error::Expr::CHILD_ERROR);
	}
      params.push_back (std::move (param.value ()));

      if (lexer.peek_token ()->get_id () != COMMA)
	break;

      lexer.skip_token ();
      t = lexer.peek_token ();
    }

  // skip ')' at end of param list
  if (!skip_token (RIGHT_PAREN))
    {
      // skip somewhere?
      return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::MALFORMED);
    }

  // TODO: check types. actually, do so during semantic analysis
  location_t locus = function_expr->get_locus ();

  return std::make_unique<AST::CallExpr> (std::move (function_expr),
					  std::move (params),
					  std::move (outer_attrs), locus);
}

/* Parses a struct expr struct with a path in expression already parsed (but
 * not
 * '{' token). */
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::StructExprStruct>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_struct_expr_struct_partial (
  AST::PathInExpression path, AST::AttrVec outer_attrs)
{
  // assume struct expr struct (as struct-enum disambiguation requires name
  // lookup) again, make statement if final ';'
  if (!skip_token (LEFT_CURLY))
    {
      return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::MALFORMED);
    }

  // parse inner attributes
  AST::AttrVec inner_attrs = parse_inner_attributes ();

  // branch based on next token
  const_TokenPtr t = lexer.peek_token ();
  location_t path_locus = path.get_locus ();
  switch (t->get_id ())
    {
    case RIGHT_CURLY:
      // struct with no body
      lexer.skip_token ();

      return std::make_unique<AST::StructExprStruct> (std::move (path),
						      std::move (inner_attrs),
						      std::move (outer_attrs),
						      path_locus);
    case DOT_DOT:
      /* technically this would give a struct base-only struct, but this
       * algorithm should work too. As such, AST type not happening. */
    case IDENTIFIER:
    case HASH:
    case INT_LITERAL:
      {
	// struct with struct expr fields

	// parse struct expr fields
	std::vector<std::unique_ptr<AST::StructExprField>> fields;

	while (t->get_id () != RIGHT_CURLY && t->get_id () != DOT_DOT)
	  {
	    auto field = parse_struct_expr_field ();
	    if (!field
		&& field.error () != Parse::Error::StructExprField::STRUCT_BASE)
	      {
		Error error (t->get_locus (),
			     "failed to parse struct (or enum) expr field");
		add_error (std::move (error));

		return tl::unexpected<Parse::Error::Expr> (
		  Parse::Error::Expr::CHILD_ERROR);
	      }

	    // DEBUG:
	    rust_debug ("struct/enum expr field validated to not be null");

	    fields.push_back (std::move (field.value ()));

	    // DEBUG:
	    rust_debug ("struct/enum expr field pushed back");

	    if (lexer.peek_token ()->get_id () != COMMA)
	      {
		// DEBUG:
		rust_debug ("lack of comma detected in struct/enum expr "
			    "fields - break");
		break;
	      }
	    lexer.skip_token ();

	    // DEBUG:
	    rust_debug ("struct/enum expr fields comma skipped ");

	    t = lexer.peek_token ();
	  }

	// DEBUG:
	rust_debug ("struct/enum expr about to parse struct base ");

	// parse struct base if it exists
	AST::StructBase struct_base = AST::StructBase::error ();
	if (lexer.peek_token ()->get_id () == DOT_DOT)
	  {
	    location_t dot_dot_location = lexer.peek_token ()->get_locus ();
	    lexer.skip_token ();

	    // parse required struct base expr
	    auto base_expr = parse_expr ();
	    if (!base_expr)
	      {
		Error error (lexer.peek_token ()->get_locus (),
			     "failed to parse struct base expression in struct "
			     "expression");
		add_error (std::move (error));

		return tl::unexpected<Parse::Error::Expr> (
		  Parse::Error::Expr::CHILD_ERROR);
	      }

	    // DEBUG:
	    rust_debug ("struct/enum expr - parsed and validated base expr");

	    struct_base = AST::StructBase (std::move (base_expr.value ()),
					   dot_dot_location);

	    // DEBUG:
	    rust_debug ("assigned struct base to new struct base ");
	  }

	if (!skip_token (RIGHT_CURLY))
	  {
	    return tl::unexpected<Parse::Error::Expr> (
	      Parse::Error::Expr::MALFORMED);
	  }

	// DEBUG:
	rust_debug (
	  "struct/enum expr skipped right curly - done and ready to return");

	return std::make_unique<AST::StructExprStructFields> (
	  std::move (path), std::move (fields), path_locus,
	  std::move (struct_base), std::move (inner_attrs),
	  std::move (outer_attrs));
      }
    default:
      add_error (
	Error (t->get_locus (),
	       "unrecognised token %qs in struct (or enum) expression - "
	       "expected %<}%>, identifier, integer literal, or %<..%>",
	       t->get_token_description ()));

      return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::MALFORMED);
    }
}

/* Parses a struct expr tuple with a path in expression already parsed (but
 * not
 * '(' token).
 * FIXME: this currently outputs a call expr, as they cannot be disambiguated.
 * A better solution would be to just get this to call that function directly.
 * */
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::CallExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_struct_expr_tuple_partial (
  AST::PathInExpression path, AST::AttrVec outer_attrs)
{
  if (!skip_token (LEFT_PAREN))
    {
      return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::MALFORMED);
    }

  AST::AttrVec inner_attrs = parse_inner_attributes ();

  std::vector<std::unique_ptr<AST::Expr>> exprs;

  const_TokenPtr t = lexer.peek_token ();
  while (t->get_id () != RIGHT_PAREN)
    {
      // parse expression (required)
      auto expr = parse_expr ();
      if (!expr)
	{
	  Error error (t->get_locus (), "failed to parse expression in "
					"struct (or enum) expression tuple");
	  add_error (std::move (error));

	  return tl::unexpected<Parse::Error::Expr> (
	    Parse::Error::Expr::CHILD_ERROR);
	}
      exprs.push_back (std::move (expr.value ()));

      if (lexer.peek_token ()->get_id () != COMMA)
	break;

      lexer.skip_token ();

      t = lexer.peek_token ();
    }

  if (!skip_token (RIGHT_PAREN))
    {
      return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::MALFORMED);
    }

  location_t path_locus = path.get_locus ();

  auto pathExpr = std::make_unique<AST::PathInExpression> (std::move (path));

  return std::make_unique<AST::CallExpr> (std::move (pathExpr),
					  std::move (exprs),
					  std::move (outer_attrs), path_locus);
}

// Parses a closure expression with pratt parsing (from null denotation).
template <typename ManagedTokenSource>
tl::expected<std::unique_ptr<AST::ClosureExpr>, Parse::Error::Expr>
Parser<ManagedTokenSource>::parse_closure_expr_pratt (const_TokenPtr tok,
						      AST::AttrVec outer_attrs)
{
  // TODO: does this need pratt parsing (for precedence)? probably not, but
  // idk
  location_t locus = tok->get_locus ();
  bool has_move = false;
  if (tok->get_id () == MOVE)
    {
      has_move = true;
      tok = lexer.peek_token ();
      lexer.skip_token ();
      // skip token and reassign
    }

  // handle parameter list
  std::vector<AST::ClosureParam> params;

  switch (tok->get_id ())
    {
    case OR:
      // no parameters, don't skip token
      break;
    case PIPE:
      {
	// actually may have parameters
	// don't skip token
	const_TokenPtr t = lexer.peek_token ();
	while (t->get_id () != PIPE)
	  {
	    AST::ClosureParam param = parse_closure_param ();
	    if (param.is_error ())
	      {
		// TODO is this really an error?
		Error error (t->get_locus (), "could not parse closure param");
		add_error (std::move (error));

		return tl::unexpected<Parse::Error::Expr> (
		  Parse::Error::Expr::CHILD_ERROR);
	      }
	    params.push_back (std::move (param));

	    if (lexer.peek_token ()->get_id () != COMMA)
	      {
		if (lexer.peek_token ()->get_id () == OR)
		  lexer.split_current_token (PIPE, PIPE);
		// not an error but means param list is done
		break;
	      }
	    // skip comma
	    lexer.skip_token ();

	    if (lexer.peek_token ()->get_id () == OR)
	      lexer.split_current_token (PIPE, PIPE);

	    t = lexer.peek_token ();
	  }

	if (!skip_token (PIPE))
	  {
	    return tl::unexpected<Parse::Error::Expr> (
	      Parse::Error::Expr::MALFORMED);
	  }
	break;
      }
    default:
      add_error (Error (tok->get_locus (),
			"unexpected token %qs in closure expression - expected "
			"%<|%> or %<||%>",
			tok->get_token_description ()));

      // skip somewhere?
      return tl::unexpected<Parse::Error::Expr> (Parse::Error::Expr::MALFORMED);
    }

  // again branch based on next token
  tok = lexer.peek_token ();
  if (tok->get_id () == RETURN_TYPE)
    {
      // must be return type closure with block expr

      // skip "return type" token
      lexer.skip_token ();

      // parse actual type, which is required
      auto type = parse_type_no_bounds ();
      if (!type)
	{
	  // error
	  Error error (tok->get_locus (), "failed to parse type for closure");
	  add_error (std::move (error));

	  // skip somewhere?
	  return tl::unexpected<Parse::Error::Expr> (
	    Parse::Error::Expr::CHILD_ERROR);
	}

      // parse block expr, which is required
      auto block = parse_block_expr ();
      if (!block)
	{
	  // error
	  Error error (lexer.peek_token ()->get_locus (),
		       "failed to parse block expr in closure");
	  add_error (std::move (error));

	  // skip somewhere?
	  return tl::unexpected<Parse::Error::Expr> (
	    Parse::Error::Expr::CHILD_ERROR);
	}

      return std::make_unique<AST::ClosureExprInnerTyped> (
	std::move (type), std::move (block.value ()), std::move (params), locus,
	has_move, std::move (outer_attrs));
    }
  else
    {
      // must be expr-only closure

      // parse expr, which is required
      auto expr = parse_expr ();
      if (!expr)
	{
	  Error error (tok->get_locus (),
		       "failed to parse expression in closure");
	  add_error (std::move (error));

	  // skip somewhere?
	  return tl::unexpected<Parse::Error::Expr> (
	    Parse::Error::Expr::CHILD_ERROR);
	}

      return std::make_unique<AST::ClosureExprInner> (std::move (expr.value ()),
						      std::move (params), locus,
						      has_move,
						      std::move (outer_attrs));
    }
}

} // namespace Rust
