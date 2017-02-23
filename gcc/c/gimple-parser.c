/* Parser for GIMPLE.
   Copyright (C) 2016-2017 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "function.h"
#include "c-tree.h"
#include "timevar.h"
#include "stringpool.h"
#include "cgraph.h"
#include "attribs.h"
#include "stor-layout.h"
#include "varasm.h"
#include "trans-mem.h"
#include "c-family/c-pragma.h"
#include "c-lang.h"
#include "c-family/c-objc.h"
#include "plugin.h"
#include "builtins.h"
#include "gomp-constants.h"
#include "c-family/c-indentation.h"
#include "gimple-expr.h"
#include "context.h"
#include "gcc-rich-location.h"
#include "c-parser.h"
#include "tree-vrp.h"
#include "tree-pass.h"
#include "tree-pretty-print.h"
#include "tree.h"
#include "basic-block.h"
#include "gimple.h"
#include "gimple-pretty-print.h"
#include "tree-ssa.h"
#include "pass_manager.h"
#include "tree-ssanames.h"
#include "gimple-ssa.h"
#include "tree-dfa.h"
#include "tree-dump.h"


/* Gimple parsing functions.  */
static bool c_parser_gimple_compound_statement (c_parser *, gimple_seq *);
static void c_parser_gimple_label (c_parser *, gimple_seq *);
static void c_parser_gimple_statement (c_parser *, gimple_seq *);
static struct c_expr c_parser_gimple_binary_expression (c_parser *);
static struct c_expr c_parser_gimple_unary_expression (c_parser *);
static struct c_expr c_parser_gimple_postfix_expression (c_parser *);
static struct c_expr c_parser_gimple_postfix_expression_after_primary (c_parser *,
								       location_t,
								       struct c_expr);
static void c_parser_gimple_declaration (c_parser *);
static void c_parser_gimple_goto_stmt (location_t, tree, gimple_seq *);
static void c_parser_gimple_if_stmt (c_parser *, gimple_seq *);
static void c_parser_gimple_switch_stmt (c_parser *, gimple_seq *);
static void c_parser_gimple_return_stmt (c_parser *, gimple_seq *);
static void c_finish_gimple_return (location_t, tree);
static tree c_parser_gimple_paren_condition (c_parser *);
static void c_parser_gimple_expr_list (c_parser *, vec<tree> *);


/* Parse the body of a function declaration marked with "__GIMPLE".  */

void
c_parser_parse_gimple_body (c_parser *parser)
{
  gimple_seq seq = NULL;
  gimple_seq body = NULL;
  tree stmt = push_stmt_list ();
  push_scope ();
  location_t loc1 = c_parser_peek_token (parser)->location;

  init_tree_ssa (cfun);

  if (! c_parser_gimple_compound_statement (parser, &seq))
    {
      gimple *ret = gimple_build_return (NULL);
      gimple_seq_add_stmt (&seq, ret);
    }

  tree block = pop_scope ();
  stmt = pop_stmt_list (stmt);
  stmt = c_build_bind_expr (loc1, block, stmt);

  block = DECL_INITIAL (current_function_decl);
  BLOCK_SUBBLOCKS (block) = NULL_TREE;
  BLOCK_CHAIN (block) = NULL_TREE;
  TREE_ASM_WRITTEN (block) = 1;

  gbind *bind_stmt = gimple_build_bind (BIND_EXPR_VARS (stmt), NULL,
					BIND_EXPR_BLOCK (stmt));
  gimple_bind_set_body (bind_stmt, seq);
  gimple_seq_add_stmt (&body, bind_stmt);
  gimple_set_body (current_function_decl, body);

  /* While we have SSA names in the IL we do not have a CFG built yet
     and PHIs are represented using a PHI internal function.  We do
     have lowered control flow and exception handling (well, we do not
     have parser support for EH yet).  But as we still have BINDs
     we have to go through lowering again.  */
  cfun->curr_properties = PROP_gimple_any;

  dump_function (TDI_generic, current_function_decl);
}

/* Parse a compound statement in gimple function body.

   gimple-statement:
     gimple-statement
     gimple-declaration-statement
     gimple-if-statement
     gimple-switch-statement
     gimple-labeled-statement
     gimple-expression-statement
     gimple-goto-statement
     gimple-phi-statement
     gimple-return-statement
*/

static bool
c_parser_gimple_compound_statement (c_parser *parser, gimple_seq *seq)
{
  bool return_p = false;

  if (! c_parser_require (parser, CPP_OPEN_BRACE, "expected %<{%>"))
    return false;

  /* A compund statement starts with optional declarations.  */
  while (c_parser_next_tokens_start_declaration (parser))
    {
      c_parser_gimple_declaration (parser);
      if (! c_parser_require (parser, CPP_SEMICOLON, "expected %<;%>"))
	return false;
    }

  while (c_parser_next_token_is_not (parser, CPP_CLOSE_BRACE))
    {
      if (c_parser_error (parser))
	{
	  c_parser_skip_until_found (parser, CPP_CLOSE_BRACE, NULL);
	  return return_p;
	}
      else if (c_parser_next_token_is (parser, CPP_EOF))
	{
	  c_parser_error (parser, "expected declaration or statement");
	  return return_p;
	}

      switch (c_parser_peek_token (parser)->type)
	{
	case CPP_KEYWORD:
	  switch (c_parser_peek_token (parser)->keyword)
	    {
	    case RID_IF:
	      c_parser_gimple_if_stmt (parser, seq);
	      break;
	    case RID_SWITCH:
	      c_parser_gimple_switch_stmt (parser, seq);
	      break;
	    case RID_GOTO:
	      {
		location_t loc = c_parser_peek_token (parser)->location;
		c_parser_consume_token (parser);
		if (c_parser_next_token_is (parser, CPP_NAME))
		  {
		    c_parser_gimple_goto_stmt (loc,
					       c_parser_peek_token
					       (parser)->value,
					       seq);
		    c_parser_consume_token (parser);
		    if (! c_parser_require (parser, CPP_SEMICOLON,
					    "expected %<;%>"))
		      return return_p;
		  }
		}
	      break;
	    case RID_RETURN:
	      return_p = true;
	      c_parser_gimple_return_stmt (parser, seq);
	      if (! c_parser_require (parser, CPP_SEMICOLON,
				      "expected %<;%>"))
		return return_p;
	      break;
	    default:
	      goto expr_stmt;
	    }
	  break;
	case CPP_NAME:
	  if (c_parser_peek_2nd_token (parser)->type == CPP_COLON)
	    {
	      c_parser_gimple_label (parser, seq);
	      break;
	    }
	  goto expr_stmt;

	case CPP_SEMICOLON:
	  {
	    /* Empty stmt.  */
	    location_t loc = c_parser_peek_token (parser)->location;
	    c_parser_consume_token (parser);
	    gimple *nop = gimple_build_nop ();
	    gimple_set_location (nop, loc);
	    gimple_seq_add_stmt (seq, nop);
	    break;
	  }

	default:
expr_stmt:
	  c_parser_gimple_statement (parser, seq);
	  if (! c_parser_require (parser, CPP_SEMICOLON, "expected %<;%>"))
	    c_parser_skip_until_found (parser, CPP_SEMICOLON, NULL);
	}
    }
  c_parser_consume_token (parser);
  return return_p;
}

/* Parse a gimple statement.

   gimple-statement:
     gimple-call-expression
     gimple-assign-statement
     gimple-phi-statement

   gimple-assign-statement:
     gimple-unary-expression = gimple-assign-rhs
 
   gimple-assign-rhs:
     gimple-cast-expression
     gimple-unary-expression
     gimple-binary-expression
     gimple-call-expression

   gimple-phi-statement:
     identifier = __PHI ( label : gimple_primary-expression, ... )

   gimple-call-expr:
     gimple-primary-expression ( argument-list )

   gimple-cast-expression:
     ( type-name ) gimple-primary-expression

*/

static void
c_parser_gimple_statement (c_parser *parser, gimple_seq *seq)
{
  struct c_expr lhs, rhs;
  gimple *assign = NULL;
  location_t loc;
  tree arg = NULL_TREE;
  auto_vec<tree> vargs;

  lhs = c_parser_gimple_unary_expression (parser);
  loc = EXPR_LOCATION (lhs.value);
  rhs.set_error ();

  /* GIMPLE call statement without LHS.  */
  if (c_parser_next_token_is (parser, CPP_SEMICOLON)
      && TREE_CODE (lhs.value) == CALL_EXPR)
    {
      gimple *call;
      call = gimple_build_call_from_tree (lhs.value);
      gimple_seq_add_stmt (seq, call);
      gimple_set_location (call, loc);
      return;
    }

  /* All following cases are statements with LHS.  */
  if (! c_parser_require (parser, CPP_EQ, "expected %<=%>"))
    return;

  /* Cast expression.  */
  if (c_parser_next_token_is (parser, CPP_OPEN_PAREN)
      && c_token_starts_typename (c_parser_peek_2nd_token (parser)))
    {
      c_parser_consume_token (parser);
      struct c_type_name *type_name = c_parser_type_name (parser);
      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, "expected %<)%>");
      if (type_name == NULL)
	return;
      /* ???  The actual type used in the cast expression is ignored as
         in GIMPLE it is encoded by the type of the LHS.  */
      rhs = c_parser_gimple_postfix_expression (parser);
      if (lhs.value != error_mark_node
	  && rhs.value != error_mark_node)
	{
	  enum tree_code code = NOP_EXPR;
	  if (VECTOR_TYPE_P (TREE_TYPE (lhs.value)))
	    {
	      code = VIEW_CONVERT_EXPR;
	      rhs.value = build1 (VIEW_CONVERT_EXPR,
				  TREE_TYPE (lhs.value), rhs.value);
	    }
	  else if (FLOAT_TYPE_P (TREE_TYPE (lhs.value))
		   && ! FLOAT_TYPE_P (TREE_TYPE (rhs.value)))
	    code = FLOAT_EXPR;
	  else if (! FLOAT_TYPE_P (TREE_TYPE (lhs.value))
		   && FLOAT_TYPE_P (TREE_TYPE (rhs.value)))
	    code = FIX_TRUNC_EXPR;
	  assign = gimple_build_assign (lhs.value, code, rhs.value);
	  gimple_seq_add_stmt (seq, assign);
	  gimple_set_location (assign, loc);
	  return;
	}
    }

  /* Unary expression.  */
  switch (c_parser_peek_token (parser)->type)
    {
    case CPP_NAME:
      {
	tree id = c_parser_peek_token (parser)->value;
	if (strcmp (IDENTIFIER_POINTER (id), "__ABS") == 0)
	  goto build_unary_expr;
	break;
      }
    case CPP_KEYWORD:
      if (c_parser_peek_token (parser)->keyword != RID_REALPART
	  && c_parser_peek_token (parser)->keyword != RID_IMAGPART)
	break;
      /* Fallthru.  */
    case CPP_AND:
    case CPP_PLUS:
    case CPP_MINUS:
    case CPP_COMPL:
    case CPP_NOT:
    case CPP_MULT: /* pointer deref */
    build_unary_expr:
      rhs = c_parser_gimple_unary_expression (parser);
      if (rhs.value != error_mark_node)
	{
	  assign = gimple_build_assign (lhs.value, rhs.value);
	  gimple_set_location (assign, loc);
	  gimple_seq_add_stmt (seq, assign);
	}
      return;

    default:;
    }

  /* GIMPLE PHI statement.  */
  if (c_parser_next_token_is_keyword (parser, RID_PHI))
    {
      c_parser_consume_token (parser);

      if (! c_parser_require (parser, CPP_OPEN_PAREN, "expected %<(%>"))
	return;

      if (c_parser_next_token_is (parser, CPP_OPEN_PAREN))
	c_parser_consume_token (parser);

      while (c_parser_next_token_is_not (parser, CPP_CLOSE_PAREN))
	{
	  if (c_parser_next_token_is (parser, CPP_NAME)
	      && c_parser_peek_2nd_token (parser)->type == CPP_COLON)
	    {
	      arg = lookup_label_for_goto (loc,
					   c_parser_peek_token (parser)->value);
	      c_parser_consume_token (parser);

	      if (c_parser_next_token_is (parser, CPP_COLON))
		c_parser_consume_token (parser);
	      vargs.safe_push (arg);
	    }
	  else if (c_parser_next_token_is (parser, CPP_COMMA))
	    c_parser_consume_token (parser);
	  else
	    {
	      arg = c_parser_gimple_unary_expression (parser).value;
	      vargs.safe_push (arg);
	    }
	}

      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
				 "expected %<)%>");

      /* Build internal function for PHI.  */
      gcall *call_stmt = gimple_build_call_internal_vec (IFN_PHI, vargs);
      gimple_call_set_lhs (call_stmt, lhs.value);
      gimple_set_location (call_stmt, UNKNOWN_LOCATION);
      gimple_seq_add_stmt (seq, call_stmt);
      return;
    }

  /* GIMPLE call with lhs.  */
  if (c_parser_next_token_is (parser, CPP_NAME)
      && c_parser_peek_2nd_token (parser)->type == CPP_OPEN_PAREN
      && lookup_name (c_parser_peek_token (parser)->value))
    {
      rhs = c_parser_gimple_unary_expression (parser);
      if (rhs.value != error_mark_node)
	{
	  gimple *call = gimple_build_call_from_tree (rhs.value);
	  gimple_call_set_lhs (call, lhs.value);
	  gimple_seq_add_stmt (seq, call);
	  gimple_set_location (call, loc);
	}
      return;
    }

  rhs = c_parser_gimple_binary_expression (parser);
  if (lhs.value != error_mark_node
      && rhs.value != error_mark_node)
    {
      assign = gimple_build_assign (lhs.value, rhs.value);
      gimple_seq_add_stmt (seq, assign);
      gimple_set_location (assign, loc);
    }
  return;
}

/* Parse gimple binary expr.

   gimple-binary-expression:
     gimple-unary-expression * gimple-unary-expression
     gimple-unary-expression / gimple-unary-expression
     gimple-unary-expression % gimple-unary-expression
     gimple-unary-expression + gimple-unary-expression
     gimple-unary-expression - gimple-unary-expression
     gimple-unary-expression << gimple-unary-expression
     gimple-unary-expression >> gimple-unary-expression
     gimple-unary-expression < gimple-unary-expression
     gimple-unary-expression > gimple-unary-expression
     gimple-unary-expression <= gimple-unary-expression
     gimple-unary-expression >= gimple-unary-expression
     gimple-unary-expression == gimple-unary-expression
     gimple-unary-expression != gimple-unary-expression
     gimple-unary-expression & gimple-unary-expression
     gimple-unary-expression ^ gimple-unary-expression
     gimple-unary-expression | gimple-unary-expression

*/

static c_expr
c_parser_gimple_binary_expression (c_parser *parser)
{
  /* Location of the binary operator.  */
  struct c_expr ret, lhs, rhs;
  enum tree_code code = ERROR_MARK;
  ret.set_error ();
  lhs = c_parser_gimple_postfix_expression (parser);
  if (c_parser_error (parser))
    return ret;
  tree ret_type = TREE_TYPE (lhs.value);
  switch (c_parser_peek_token (parser)->type)
    {
    case CPP_MULT:
      code = MULT_EXPR;
      break;
    case CPP_DIV:
      code = TRUNC_DIV_EXPR;
      break;
    case CPP_MOD:
      code = TRUNC_MOD_EXPR;
      break;
    case CPP_PLUS:
      if (POINTER_TYPE_P (TREE_TYPE (lhs.value)))
	code = POINTER_PLUS_EXPR;
      else
	code = PLUS_EXPR;
      break;
    case CPP_MINUS:
      code = MINUS_EXPR;
      break;
    case CPP_LSHIFT:
      code = LSHIFT_EXPR;
      break;
    case CPP_RSHIFT:
      code = RSHIFT_EXPR;
      break;
    case CPP_LESS:
      code = LT_EXPR;
      ret_type = boolean_type_node;
      break;
    case CPP_GREATER:
      code = GT_EXPR;
      ret_type = boolean_type_node;
      break;
    case CPP_LESS_EQ:
      code = LE_EXPR;
      ret_type = boolean_type_node;
      break;
    case CPP_GREATER_EQ:
      code = GE_EXPR;
      ret_type = boolean_type_node;
      break;
    case CPP_EQ_EQ:
      code = EQ_EXPR;
      ret_type = boolean_type_node;
      break;
    case CPP_NOT_EQ:
      code = NE_EXPR;
      ret_type = boolean_type_node;
      break;
    case CPP_AND:
      code = BIT_AND_EXPR;
      break;
    case CPP_XOR:
      code = BIT_XOR_EXPR;
      break;
    case CPP_OR:
      code = BIT_IOR_EXPR;
      break;
    case CPP_AND_AND:
      c_parser_error (parser, "%<&&%> not valid in GIMPLE");
      return ret;
    case CPP_OR_OR:
      c_parser_error (parser, "%<||%> not valid in GIMPLE");
      return ret;
    default:
      /* Not a binary expression.  */
      return lhs;
    }
  location_t ret_loc = c_parser_peek_token (parser)->location;
  c_parser_consume_token (parser);
  rhs = c_parser_gimple_postfix_expression (parser);
  if (lhs.value != error_mark_node && rhs.value != error_mark_node)
    ret.value = build2_loc (ret_loc, code, ret_type, lhs.value, rhs.value);
  return ret;
}

/* Parse gimple unary expression.

   gimple-unary-expression:
     gimple-postfix-expression
     unary-operator gimple-postfix-expression

   unary-operator: one of
     & * + - ~ abs_expr
*/

static c_expr
c_parser_gimple_unary_expression (c_parser *parser)
{
  struct c_expr ret, op;
  location_t op_loc = c_parser_peek_token (parser)->location;
  location_t finish;
  ret.set_error ();
  switch (c_parser_peek_token (parser)->type)
    {
    case CPP_AND:
      c_parser_consume_token (parser);
      op = c_parser_gimple_postfix_expression (parser);
      mark_exp_read (op.value);
      return parser_build_unary_op (op_loc, ADDR_EXPR, op);
    case CPP_MULT:
      {
	c_parser_consume_token (parser);
	op = c_parser_gimple_postfix_expression (parser);
	if (op.value == error_mark_node)
	  return ret;
	finish = op.get_finish ();
	location_t combined_loc = make_location (op_loc, op_loc, finish);
	ret.value = build_simple_mem_ref_loc (combined_loc, op.value);
	TREE_SIDE_EFFECTS (ret.value)
	  = TREE_THIS_VOLATILE (ret.value)
	  = TYPE_VOLATILE (TREE_TYPE (TREE_TYPE (op.value)));
	ret.src_range.m_start = op_loc;
	ret.src_range.m_finish = finish;
	return ret;
      }
    case CPP_PLUS:
      c_parser_consume_token (parser);
      op = c_parser_gimple_postfix_expression (parser);
      return parser_build_unary_op (op_loc, CONVERT_EXPR, op);
    case CPP_MINUS:
      c_parser_consume_token (parser);
      op = c_parser_gimple_postfix_expression (parser);
      return parser_build_unary_op (op_loc, NEGATE_EXPR, op);
    case CPP_COMPL:
      c_parser_consume_token (parser);
      op = c_parser_gimple_postfix_expression (parser);
      return parser_build_unary_op (op_loc, BIT_NOT_EXPR, op);
    case CPP_NOT:
      c_parser_error (parser, "%<!%> not valid in GIMPLE");
      return ret;
    case CPP_KEYWORD:
      switch (c_parser_peek_token (parser)->keyword)
	{
	case RID_REALPART:
	  c_parser_consume_token (parser);
	  op = c_parser_gimple_postfix_expression (parser);
	  return parser_build_unary_op (op_loc, REALPART_EXPR, op);
	case RID_IMAGPART:
	  c_parser_consume_token (parser);
	  op = c_parser_gimple_postfix_expression (parser);
	  return parser_build_unary_op (op_loc, IMAGPART_EXPR, op);
	default:
	  return c_parser_gimple_postfix_expression (parser);
	}
    case CPP_NAME:
	{
	  tree id = c_parser_peek_token (parser)->value;
	  if (strcmp (IDENTIFIER_POINTER (id), "__ABS") == 0)
	    {
	      c_parser_consume_token (parser);
	      op = c_parser_gimple_postfix_expression (parser);
	      return parser_build_unary_op (op_loc, ABS_EXPR, op);
	    }
	  else
	    return c_parser_gimple_postfix_expression (parser);
	}
    default:
      return c_parser_gimple_postfix_expression (parser);
    }
}

/* Decompose ID into base name (ID until ver_offset) and VERSION.  Return
   true if ID matches a SSA name.  */

static bool
c_parser_parse_ssa_name_id (tree id, unsigned *version, unsigned *ver_offset)
{
  const char *token = IDENTIFIER_POINTER (id);
  const char *var_version = strrchr (token, '_');
  if (! var_version)
    return false;

  *ver_offset = var_version - token;
  for (const char *p = var_version + 1; *p; ++p)
    if (! ISDIGIT (*p))
      return false;
  *version = atoi (var_version + 1);
  return *version > 0;
}

/* Get at the actual SSA name ID with VERSION starting at VER_OFFSET.
   TYPE is the type if the SSA name is being declared.  */

static tree 
c_parser_parse_ssa_name (c_parser *parser,
			 tree id, tree type, unsigned version,
			 unsigned ver_offset)
{
  tree name = NULL_TREE;
  const char *token = IDENTIFIER_POINTER (id);

  if (ver_offset == 0)
    {
      /* Anonymous unnamed SSA name.  */
      if (version < num_ssa_names)
	name = ssa_name (version);
      if (! name)
	{
	  if (! type)
	    {
	      c_parser_error (parser, "SSA name undeclared"); 
	      return error_mark_node;
	    }
	  name = make_ssa_name_fn (cfun, type, NULL, version);
	}
    }
  else
    {
      if (version < num_ssa_names)
	name = ssa_name (version);
      if (! name)
	{
	  /* Separate var name from version.  */
	  char *var_name = XNEWVEC (char, ver_offset + 1);
	  memcpy (var_name, token, ver_offset);
	  var_name[ver_offset] = '\0';
	  /* lookup for parent decl.  */
	  id = get_identifier (var_name);
	  tree parent = lookup_name (id);
	  XDELETEVEC (var_name);
	  if (! parent || parent == error_mark_node)
	    {
	      c_parser_error (parser, "base variable or SSA name undeclared"); 
	      return error_mark_node;
	    }
	  if (VECTOR_TYPE_P (TREE_TYPE (parent))
	      || TREE_CODE (TREE_TYPE (parent)) == COMPLEX_TYPE)
	    DECL_GIMPLE_REG_P (parent) = 1;
	  name = make_ssa_name_fn (cfun, parent,
				   gimple_build_nop (), version);
	}
    }

  return name;
}

/* Parse gimple postfix expression.

   gimple-postfix-expression:
     gimple-primary-expression
     gimple-primary-xpression [ gimple-primary-expression ]
     gimple-primary-expression ( gimple-argument-expression-list[opt] )
     postfix-expression . identifier
     postfix-expression -> identifier

   gimple-argument-expression-list:
     gimple-unary-expression
     gimple-argument-expression-list , gimple-unary-expression

   gimple-primary-expression:
     identifier
     constant
     string-literal

*/

static struct c_expr
c_parser_gimple_postfix_expression (c_parser *parser)
{
  location_t loc = c_parser_peek_token (parser)->location;
  source_range tok_range = c_parser_peek_token (parser)->get_range ();
  struct c_expr expr;
  expr.set_error ();
  switch (c_parser_peek_token (parser)->type)
    {
    case CPP_NUMBER:
      expr.value = c_parser_peek_token (parser)->value;
      set_c_expr_source_range (&expr, tok_range);
      loc = c_parser_peek_token (parser)->location;
      c_parser_consume_token (parser);
      break;
    case CPP_CHAR:
    case CPP_CHAR16:
    case CPP_CHAR32:
    case CPP_WCHAR:
      expr.value = c_parser_peek_token (parser)->value;
      set_c_expr_source_range (&expr, tok_range);
      c_parser_consume_token (parser);
      break;
    case CPP_STRING:
    case CPP_STRING16:
    case CPP_STRING32:
    case CPP_WSTRING:
    case CPP_UTF8STRING:
      expr.value = c_parser_peek_token (parser)->value;
      set_c_expr_source_range (&expr, tok_range);
      expr.original_code = STRING_CST;
      c_parser_consume_token (parser);
      break;
    case CPP_NAME:
      if (c_parser_peek_token (parser)->id_kind == C_ID_ID)
	{
	  tree id = c_parser_peek_token (parser)->value;
	  if (strcmp (IDENTIFIER_POINTER (id), "__MEM") == 0)
	    {
	      /* __MEM '<' type-name [ ',' number ] '>'
	               '(' [ '(' type-name ')' ] unary-expression
		           [ '+' number ] ')'  */
	      location_t loc = c_parser_peek_token (parser)->location;
	      c_parser_consume_token (parser);
	      struct c_type_name *type_name = NULL;
	      tree alignment = NULL_TREE;
	      if (c_parser_require (parser, CPP_LESS, "expected %<<%>"))
	        {
		  type_name = c_parser_type_name (parser);
		  /* Optional alignment.  */
		  if (c_parser_next_token_is (parser, CPP_COMMA))
		    {
		      c_parser_consume_token (parser);
		      alignment
			= c_parser_gimple_postfix_expression (parser).value;
		    }
		  c_parser_skip_until_found (parser,
					     CPP_GREATER, "expected %<>%>");
		}
	      struct c_expr ptr;
	      ptr.value = error_mark_node;
	      tree alias_off = NULL_TREE;
	      if (c_parser_require (parser, CPP_OPEN_PAREN, "expected %<(%>"))
		{
		  tree alias_type = NULL_TREE;
		  /* Optional alias-type cast.  */
		  if (c_parser_next_token_is (parser, CPP_OPEN_PAREN))
		    {
		      c_parser_consume_token (parser);
		      struct c_type_name *alias_type_name
			= c_parser_type_name (parser);
		      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
						 "expected %<)%>");
		      if (alias_type_name)
			{
			  tree tem;
			  alias_type = groktypename (alias_type_name,
						     &tem, NULL);
			}
		    }
		  ptr = c_parser_gimple_unary_expression (parser);
		  if (! alias_type)
		    alias_type = TREE_TYPE (ptr.value);
		  /* Optional constant offset.  */
		  if (c_parser_next_token_is (parser, CPP_PLUS))
		    {
		      c_parser_consume_token (parser);
		      alias_off
			= c_parser_gimple_postfix_expression (parser).value;
		      alias_off = fold_convert (alias_type, alias_off);
		    }
		  if (! alias_off)
		    alias_off = build_int_cst (alias_type, 0);
		  c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
					     "expected %<)%>");
		}
	      if (! type_name || c_parser_error (parser))
		{
		  c_parser_set_error (parser, false);
		  return expr;
		}
	      tree tem = NULL_TREE;
	      tree type = groktypename (type_name, &tem, NULL);
	      if (alignment)
		type = build_aligned_type (type, tree_to_uhwi (alignment));
	      expr.value = build2_loc (loc, MEM_REF,
				       type, ptr.value, alias_off);
	      break;
	    }
	  else if (strcmp (IDENTIFIER_POINTER (id), "_Literal") == 0)
	    {
	      /* _Literal '(' type-name ')' number  */
	      c_parser_consume_token (parser);
	      tree type = NULL_TREE;
	      if (c_parser_require (parser, CPP_OPEN_PAREN, "expected %<(%>"))
		{
		  struct c_type_name *type_name = c_parser_type_name (parser);
		  tree tem;
		  if (type_name)
		    type = groktypename (type_name, &tem, NULL);
		  c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
					     "expected %<)%>");
		}
	      tree val = c_parser_gimple_postfix_expression (parser).value;
	      if (! type
		  || ! val
		  || val == error_mark_node
		  || TREE_CODE (val) != INTEGER_CST)
		{
		  c_parser_error (parser, "invalid _Literal");
		  return expr;
		}
	      expr.value = fold_convert (type, val);
	      return expr;
	    }
	  else if (strcmp (IDENTIFIER_POINTER (id), "__FMA") == 0)
	    {
	      c_parser_consume_token (parser);
	      auto_vec<tree> args;

	      if (c_parser_require (parser, CPP_OPEN_PAREN, "expected %<(%>"))
		{
		  c_parser_gimple_expr_list (parser, &args);
		  c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
					     "expected %<)%>");
		}
	      if (args.length () != 3)
		{
		  error_at (loc, "invalid number of operands to __FMA");
		  expr.value = error_mark_node;
		  return expr;
		}
	      expr.value = build3_loc (loc, FMA_EXPR, TREE_TYPE (args[0]),
				       args[0], args[1], args[2]);
	      return expr;
	    }

	  /* SSA name.  */
	  unsigned version, ver_offset;
	  if (! lookup_name (id)
	      && c_parser_parse_ssa_name_id (id, &version, &ver_offset))
	    {
	      c_parser_consume_token (parser);
	      expr.value = c_parser_parse_ssa_name (parser, id, NULL_TREE,
						    version, ver_offset);
	      if (expr.value == error_mark_node)
		return expr;
	      set_c_expr_source_range (&expr, tok_range);
	      /* For default definition SSA names.  */
	      if (c_parser_next_token_is (parser, CPP_OPEN_PAREN)
		  && c_parser_peek_2nd_token (parser)->type == CPP_NAME
		  && strcmp ("D",
			     IDENTIFIER_POINTER
			       (c_parser_peek_2nd_token (parser)->value)) == 0
		  && c_parser_peek_nth_token (parser, 3)->type == CPP_CLOSE_PAREN)
		{
		  c_parser_consume_token (parser);
		  c_parser_consume_token (parser);
		  c_parser_consume_token (parser);
		  if (! SSA_NAME_IS_DEFAULT_DEF (expr.value))
		    {
		      if (!SSA_NAME_VAR (expr.value))
			{
			  error_at (loc, "anonymous SSA name cannot have"
				    " default definition");
			  expr.value = error_mark_node;
			  return expr;
			}
		      set_ssa_default_def (cfun, SSA_NAME_VAR (expr.value),
					   expr.value);
		      SSA_NAME_DEF_STMT (expr.value) = gimple_build_nop ();
		    }
		}
	    }
	  else
	    {
	      c_parser_consume_token (parser);
	      expr.value
		= build_external_ref (loc, id,
				      (c_parser_peek_token (parser)->type
				       == CPP_OPEN_PAREN), &expr.original_type);
	      set_c_expr_source_range (&expr, tok_range);
	    }
	  break;
	}
      else
	{
	  c_parser_error (parser, "expected expression");
	  expr.set_error ();
	  break;
	}
      break;
    default:
      c_parser_error (parser, "expected expression");
      expr.set_error ();
      break;
    }
  return c_parser_gimple_postfix_expression_after_primary
    (parser, EXPR_LOC_OR_LOC (expr.value, loc), expr);
}

/* Parse a gimple postfix expression after the initial primary or compound
   literal.  */

static struct c_expr
c_parser_gimple_postfix_expression_after_primary (c_parser *parser,
						  location_t expr_loc,
						  struct c_expr expr)
{
  location_t start;
  location_t finish;
  tree ident;
  location_t comp_loc;

  while (true)
    {
      location_t op_loc = c_parser_peek_token (parser)->location;
      switch (c_parser_peek_token (parser)->type)
	{
	case CPP_OPEN_SQUARE:
	  {
	    c_parser_consume_token (parser);
	    tree idx = c_parser_gimple_unary_expression (parser).value;

	    if (! c_parser_require (parser, CPP_CLOSE_SQUARE, "expected %<]%>"))
	      {
		c_parser_skip_until_found (parser, CPP_CLOSE_SQUARE, NULL);
		break;
	      }

	    start = expr.get_start ();
	    finish = c_parser_tokens_buf (parser, 0)->location;
	    expr.value = build_array_ref (op_loc, expr.value, idx);
	    set_c_expr_source_range (&expr, start, finish);

	    expr.original_code = ERROR_MARK;
	    expr.original_type = NULL;
	    break;
	  }
	case CPP_OPEN_PAREN:
	  {
	    /* Function call.  */
	    c_parser_consume_token (parser);
	    auto_vec<tree> exprlist;
	    if (! c_parser_next_token_is (parser, CPP_CLOSE_PAREN))
	      c_parser_gimple_expr_list (parser, &exprlist);
	    c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
				       "expected %<)%>");
	    expr.value = build_call_array_loc
		(expr_loc, TREE_TYPE (TREE_TYPE (expr.value)),
		 expr.value, exprlist.length (), exprlist.address ());
	    expr.original_code = ERROR_MARK;
	    expr.original_type = NULL;
	    break;
	  }
	case CPP_DOT:
	  {
	    /* Structure element reference.  */
	    c_parser_consume_token (parser);
	    if (c_parser_next_token_is (parser, CPP_NAME))
	      {
		c_token *comp_tok = c_parser_peek_token (parser);
		ident = comp_tok->value;
		comp_loc = comp_tok->location;
	      }
	    else
	      {
		c_parser_error (parser, "expected identifier");
		expr.set_error ();
		expr.original_code = ERROR_MARK;
		expr.original_type = NULL;
		return expr;
	      }
	    start = expr.get_start ();
	    finish = c_parser_peek_token (parser)->get_finish ();
	    c_parser_consume_token (parser);
	    expr.value = build_component_ref (op_loc, expr.value, ident,
					      comp_loc);
	    set_c_expr_source_range (&expr, start, finish);
	    expr.original_code = ERROR_MARK;
	    if (TREE_CODE (expr.value) != COMPONENT_REF)
	      expr.original_type = NULL;
	    else
	      {
		/* Remember the original type of a bitfield.  */
		tree field = TREE_OPERAND (expr.value, 1);
		if (TREE_CODE (field) != FIELD_DECL)
		  expr.original_type = NULL;
		else
		  expr.original_type = DECL_BIT_FIELD_TYPE (field);
	      }
	    break;
	  }
	case CPP_DEREF:
	  {
	    /* Structure element reference.  */
	    c_parser_consume_token (parser);
	    if (c_parser_next_token_is (parser, CPP_NAME))
	      {
		c_token *comp_tok = c_parser_peek_token (parser);
		ident = comp_tok->value;
		comp_loc = comp_tok->location;
	      }
	    else
	      {
		c_parser_error (parser, "expected identifier");
		expr.set_error ();
		expr.original_code = ERROR_MARK;
		expr.original_type = NULL;
		return expr;
	      }
	    start = expr.get_start ();
	    finish = c_parser_peek_token (parser)->get_finish ();
	    c_parser_consume_token (parser);
	    expr.value = build_component_ref (op_loc,
					      build_simple_mem_ref_loc
					        (op_loc, expr.value),
					      ident, comp_loc);
	    set_c_expr_source_range (&expr, start, finish);
	    expr.original_code = ERROR_MARK;
	    if (TREE_CODE (expr.value) != COMPONENT_REF)
	      expr.original_type = NULL;
	    else
	      {
		/* Remember the original type of a bitfield.  */
		tree field = TREE_OPERAND (expr.value, 1);
		if (TREE_CODE (field) != FIELD_DECL)
		  expr.original_type = NULL;
		else
		  expr.original_type = DECL_BIT_FIELD_TYPE (field);
	      }
	    break;
	  }
	default:
	  return expr;
	}
    }
}

/* Parse expression list.

   gimple-expr-list:
     gimple-unary-expression
     gimple-expr-list , gimple-unary-expression

 */

static void
c_parser_gimple_expr_list (c_parser *parser, vec<tree> *ret)
{
  struct c_expr expr;

  expr = c_parser_gimple_unary_expression (parser);
  ret->safe_push (expr.value);
  while (c_parser_next_token_is (parser, CPP_COMMA))
    {
      c_parser_consume_token (parser);
      expr = c_parser_gimple_unary_expression (parser);
      ret->safe_push (expr.value);
    }
}

/* Parse gimple label.

   gimple-label:
     identifier :
     case constant-expression :
     default :

*/

static void
c_parser_gimple_label (c_parser *parser, gimple_seq *seq)
{
  tree name = c_parser_peek_token (parser)->value;
  location_t loc1 = c_parser_peek_token (parser)->location;
  gcc_assert (c_parser_next_token_is (parser, CPP_NAME));
  c_parser_consume_token (parser);
  gcc_assert (c_parser_next_token_is (parser, CPP_COLON));
  c_parser_consume_token (parser);
  tree label = define_label (loc1, name);
  gimple_seq_add_stmt (seq, gimple_build_label (label));
  return;
}

/* Parse gimple/RTL pass list.

   gimple-or-rtl-pass-list:
     startwith("pass-name")
 */

char *
c_parser_gimple_or_rtl_pass_list (c_parser *parser)
{
  char *pass = NULL;

  /* Accept __GIMPLE/__RTL.  */
  if (c_parser_next_token_is_not (parser, CPP_OPEN_PAREN))
    return NULL;
  c_parser_consume_token (parser);

  if (c_parser_next_token_is (parser, CPP_NAME))
    {
      const char *op = IDENTIFIER_POINTER (c_parser_peek_token (parser)->value);
      c_parser_consume_token (parser);
      if (! strcmp (op, "startwith"))
	{
	  if (! c_parser_require (parser, CPP_OPEN_PAREN, "expected %<(%>"))
	    return NULL;
	  if (c_parser_next_token_is_not (parser, CPP_STRING))
	    {
	      error_at (c_parser_peek_token (parser)->location,
			"expected pass name");
	      return NULL;
	    }
	  pass = xstrdup (TREE_STRING_POINTER
				(c_parser_peek_token (parser)->value));
	  c_parser_consume_token (parser);
	  if (! c_parser_require (parser, CPP_CLOSE_PAREN, "expected %<)%>"))
	    return NULL;
	}
      else
	{
	  error_at (c_parser_peek_token (parser)->location,
		    "invalid operation");
	  return NULL;
	}
    }

  if (! c_parser_require (parser, CPP_CLOSE_PAREN, "expected %<)%>"))
    return NULL;

  return pass;
}

/* Parse gimple local declaration.

   declaration-specifiers:
     storage-class-specifier declaration-specifiers[opt]
     type-specifier declaration-specifiers[opt]
     type-qualifier declaration-specifiers[opt]
     function-specifier declaration-specifiers[opt]
     alignment-specifier declaration-specifiers[opt]

   storage-class-specifier:
     typedef
     extern
     static
     auto
     register

   type-specifier:
     void
     char
     short
     int
     long
     float
     double
     signed
     unsigned
     _Bool
     _Complex

   type-qualifier:
     const
     restrict
     volatile
     address-space-qualifier
     _Atomic

 */

static void
c_parser_gimple_declaration (c_parser *parser)
{
  struct c_declarator *declarator;
  struct c_declspecs *specs = build_null_declspecs ();
  c_parser_declspecs (parser, specs, true, true, true,
		      true, true, cla_nonabstract_decl);
  finish_declspecs (specs);

  /* Provide better error recovery.  Note that a type name here is usually
     better diagnosed as a redeclaration.  */
  if (c_parser_next_token_starts_declspecs (parser)
      && ! c_parser_next_token_is (parser, CPP_NAME))
    {
      c_parser_error (parser, "expected %<;%>");
      c_parser_set_error (parser, false);
      return;
    }

  bool dummy = false;
  declarator = c_parser_declarator (parser,
				    specs->typespec_kind != ctsk_none,
				    C_DTR_NORMAL, &dummy);

  if (c_parser_next_token_is (parser, CPP_SEMICOLON))
    {
      /* Handle SSA name decls specially, they do not go into the identifier
         table but we simply build the SSA name for later lookup.  */
      unsigned version, ver_offset;
      if (declarator->kind == cdk_id
	  && is_gimple_reg_type (specs->type)
	  && c_parser_parse_ssa_name_id (declarator->u.id,
					 &version, &ver_offset)
	  /* The following restricts it to unnamed anonymous SSA names
	     which fails parsing of named ones in dumps (we could
	     decide to not dump their name for -gimple).  */
	  && ver_offset == 0)
	c_parser_parse_ssa_name (parser, declarator->u.id, specs->type,
				 version, ver_offset);
      else
	{
	  tree postfix_attrs = NULL_TREE;
	  tree all_prefix_attrs = specs->attrs;
	  specs->attrs = NULL;
	  tree decl = start_decl (declarator, specs, false,
				  chainon (postfix_attrs, all_prefix_attrs));
	  if (decl)
	    finish_decl (decl, UNKNOWN_LOCATION, NULL_TREE, NULL_TREE,
			 NULL_TREE);
	}
    }
  else
    {
      c_parser_error (parser, "expected %<;%>");
      return;
    }
}

/* Parse gimple goto statement.  */

static void
c_parser_gimple_goto_stmt (location_t loc, tree label, gimple_seq *seq)
{
  tree decl = lookup_label_for_goto (loc, label);
  gimple_seq_add_stmt (seq, gimple_build_goto (decl));
  return;
}

/* Parse a parenthesized condition.
   gimple-condition:
     ( gimple-binary-expression )    */

static tree
c_parser_gimple_paren_condition (c_parser *parser)
{
  if (! c_parser_require (parser, CPP_OPEN_PAREN, "expected %<(%>"))
    return error_mark_node;
  tree cond = c_parser_gimple_binary_expression (parser).value;
  if (! c_parser_require (parser, CPP_CLOSE_PAREN, "expected %<)%>"))
    return error_mark_node;
  return cond;
}

/* Parse gimple if-else statement.

   if-statement:
     if ( gimple-binary-expression ) gimple-goto-statement
     if ( gimple-binary-expression ) gimple-goto-statement \
					else gimple-goto-statement
 */

static void
c_parser_gimple_if_stmt (c_parser *parser, gimple_seq *seq)
{
  tree t_label, f_label, label;
  location_t loc;
  c_parser_consume_token (parser);
  tree cond = c_parser_gimple_paren_condition (parser);

  if (c_parser_next_token_is_keyword (parser, RID_GOTO))
    {
      loc = c_parser_peek_token (parser)->location;
      c_parser_consume_token (parser);
      label = c_parser_peek_token (parser)->value;
      t_label = lookup_label_for_goto (loc, label);
      c_parser_consume_token (parser);
      if (! c_parser_require (parser, CPP_SEMICOLON, "expected %<;%>"))
	return;
    }
  else
    {
      c_parser_error (parser, "expected goto expression");
      return;
    }

  if (c_parser_next_token_is_keyword (parser, RID_ELSE))
    c_parser_consume_token (parser);
  else
    {
      c_parser_error (parser, "expected else statement");
      return;
    }

  if (c_parser_next_token_is_keyword (parser, RID_GOTO))
    {
      loc = c_parser_peek_token (parser)->location;
      c_parser_consume_token (parser);
      label = c_parser_peek_token (parser)->value;
      f_label = lookup_label_for_goto (loc, label);
      c_parser_consume_token (parser);
      if (! c_parser_require (parser, CPP_SEMICOLON, "expected %<;%>"))
	return;
    }
  else
    {
      c_parser_error (parser, "expected goto expression");
      return;
    }

  if (cond != error_mark_node)
    gimple_seq_add_stmt (seq, gimple_build_cond_from_tree (cond, t_label,
							   f_label));
}

/* Parse gimple switch-statement.

   gimple-switch-statement:
     switch (gimple-postfix-expression) gimple-case-statement

   gimple-case-statement:
     gimple-case-statement
     gimple-label-statement : gimple-goto-statment
*/

static void
c_parser_gimple_switch_stmt (c_parser *parser, gimple_seq *seq)
{
  c_expr cond_expr;
  tree case_label, label;
  auto_vec<tree> labels;
  tree default_label = NULL_TREE;
  gimple_seq switch_body = NULL;
  c_parser_consume_token (parser);

  if (! c_parser_require (parser, CPP_OPEN_PAREN, "expected %<(%>"))
    return;
  cond_expr = c_parser_gimple_postfix_expression (parser);
  if (! c_parser_require (parser, CPP_CLOSE_PAREN, "expected %<)%>"))
    return;

  if (! c_parser_require (parser, CPP_OPEN_BRACE, "expected %<{%>"))
    return;

  while (c_parser_next_token_is_not (parser, CPP_CLOSE_BRACE))
    {
      if (c_parser_next_token_is (parser, CPP_EOF))
	{
	  c_parser_error (parser, "expected statement");
	  return;
	}

      switch (c_parser_peek_token (parser)->keyword)
	{
	case RID_CASE:
	  {
	    c_expr exp1;
	    location_t loc = c_parser_peek_token (parser)->location;
	    c_parser_consume_token (parser);

	    if (c_parser_next_token_is (parser, CPP_NAME)
		|| c_parser_peek_token (parser)->type == CPP_NUMBER)
	      exp1 = c_parser_gimple_postfix_expression (parser);
	    else
	      {
		c_parser_error (parser, "expected expression");
		return;
	      }

	    if (c_parser_next_token_is (parser, CPP_COLON))
	      {
		c_parser_consume_token (parser);
		if (c_parser_next_token_is (parser, CPP_NAME))
		  {
		    label = c_parser_peek_token (parser)->value;
		    c_parser_consume_token (parser);
		    tree decl = lookup_label_for_goto (loc, label);
		    case_label = build_case_label (exp1.value, NULL_TREE,
						   decl);
		    labels.safe_push (case_label);
		    if (! c_parser_require (parser, CPP_SEMICOLON,
					    "expected %<;%>"))
		      return;
		  }
		else if (! c_parser_require (parser, CPP_NAME,
					     "expected label"))
		  return;
	      }
	    else if (! c_parser_require (parser, CPP_SEMICOLON,
					 "expected %<:%>"))
	      return;
	    break;
	  }
	case RID_DEFAULT:
	  {
	    location_t loc = c_parser_peek_token (parser)->location;
	    c_parser_consume_token (parser);
	    if (c_parser_next_token_is (parser, CPP_COLON))
	      {
		c_parser_consume_token (parser);
		if (c_parser_next_token_is (parser, CPP_NAME))
		  {
		    label = c_parser_peek_token (parser)->value;
		    c_parser_consume_token (parser);
		    tree decl = lookup_label_for_goto (loc, label);
		    default_label = build_case_label (NULL_TREE, NULL_TREE,
						      decl);
		    if (! c_parser_require (parser, CPP_SEMICOLON,
					    "expected %<;%>"))
		      return;
		  }
		else if (! c_parser_require (parser, CPP_NAME,
					     "expected label"))
		  return;
	      }
	    else if (! c_parser_require (parser, CPP_SEMICOLON,
					 "expected %<:%>"))
	      return;
	    break;
	  }
	case RID_GOTO:
	  {
	    location_t loc = c_parser_peek_token (parser)->location;
	    c_parser_consume_token (parser);
	    if (c_parser_next_token_is (parser, CPP_NAME))
	      {
		c_parser_gimple_goto_stmt (loc,
					   c_parser_peek_token
					   (parser)->value,
					   &switch_body);
		c_parser_consume_token (parser);
		if (c_parser_next_token_is (parser, CPP_SEMICOLON))
		  c_parser_consume_token (parser);
		else
		  {
		    c_parser_error (parser, "expected semicolon");
		    return;
		  }
	      }
	    else if (! c_parser_require (parser, CPP_NAME,
					 "expected label"))
	      return;
	    break;
	  }
	default:
	  c_parser_error (parser, "expected case label or goto statement");
	  return;
	}

    }
  if (! c_parser_require (parser, CPP_CLOSE_BRACE, "expected %<}%>"))
    return;

  if (cond_expr.value != error_mark_node)
    {
      gimple_seq_add_stmt (seq, gimple_build_switch (cond_expr.value,
						     default_label, labels));
      gimple_seq_add_seq (seq, switch_body);
    }
}

/* Parse gimple return statement.  */

static void
c_parser_gimple_return_stmt (c_parser *parser, gimple_seq *seq)
{
  location_t loc = c_parser_peek_token (parser)->location;
  gimple *ret = NULL;
  c_parser_consume_token (parser);
  if (c_parser_next_token_is (parser, CPP_SEMICOLON))
    {
      c_finish_gimple_return (loc, NULL_TREE);
      ret = gimple_build_return (NULL);
      gimple_seq_add_stmt (seq, ret);
    }
  else
    {
      location_t xloc = c_parser_peek_token (parser)->location;
      c_expr expr = c_parser_gimple_unary_expression (parser);
      if (expr.value != error_mark_node)
	{
	  c_finish_gimple_return (xloc, expr.value);
	  ret = gimple_build_return (expr.value);
	  gimple_seq_add_stmt (seq, ret);
	}
    }
}

/* Support function for c_parser_gimple_return_stmt.  */

static void
c_finish_gimple_return (location_t loc, tree retval)
{
  tree valtype = TREE_TYPE (TREE_TYPE (current_function_decl));

  /* Use the expansion point to handle cases such as returning NULL
     in a function returning void.  */
  source_location xloc = expansion_point_location_if_in_system_header (loc);

  if (TREE_THIS_VOLATILE (current_function_decl))
    warning_at (xloc, 0,
		"function declared %<noreturn%> has a %<return%> statement");

  if (! retval)
    current_function_returns_null = 1;
  else if (valtype == 0 || TREE_CODE (valtype) == VOID_TYPE)
    {
      current_function_returns_null = 1;
      if (TREE_CODE (TREE_TYPE (retval)) != VOID_TYPE)
	{
	  error_at
	    (xloc, "%<return%> with a value, in function returning void");
	  inform (DECL_SOURCE_LOCATION (current_function_decl),
		  "declared here");
	}
    }
  else if (TREE_CODE (valtype) != TREE_CODE (TREE_TYPE (retval)))
    {
      error_at
	(xloc, "invalid conversion in return statement");
      inform (DECL_SOURCE_LOCATION (current_function_decl),
	      "declared here");
    }
  return;
}
