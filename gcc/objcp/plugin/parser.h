/* Objective-C++ Parser plugin
   Copyright (C) 2000, 2001, 2002, 2003, 2004,
   2005, 2007, 2008, 2009, 2010, 2011  Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */


/* This isn't defined anywhere, yet, but it could be.  This will turn off
   the entire C++ Objective-C++ plugin.  */
#ifdef HIDE_OBJC

#include "c-family/c-objc.h"


/* Objective-C++ Productions */

static tree cp_parser_objc_message_receiver
  (cp_parser *);
static tree cp_parser_objc_message_args
  (cp_parser *);
static tree cp_parser_objc_message_expression
  (cp_parser *);
static tree cp_parser_objc_encode_expression
  (cp_parser *);
static tree cp_parser_objc_defs_expression
  (cp_parser *);
static tree cp_parser_objc_protocol_expression
  (cp_parser *);
static tree cp_parser_objc_selector_expression
  (cp_parser *);
static tree cp_parser_objc_expression
  (cp_parser *);
static bool cp_parser_objc_selector_p
  (enum cpp_ttype);
static tree cp_parser_objc_selector
  (cp_parser *);
static tree cp_parser_objc_protocol_refs_opt
  (cp_parser *);
static void cp_parser_objc_declaration
  (cp_parser *, tree);
static tree cp_parser_objc_statement
  (cp_parser *);
static bool cp_parser_objc_valid_prefix_attributes
  (cp_parser *, tree *);
static void cp_parser_objc_at_property_declaration 
  (cp_parser *) ;
static void cp_parser_objc_at_synthesize_declaration 
  (cp_parser *) ;
static void cp_parser_objc_at_dynamic_declaration
  (cp_parser *) ;
static tree cp_parser_objc_struct_declaration
  (cp_parser *) ;

#define PLUGIN_PRIMARY_EXPRESSION_3(parser) @(
  do {
    if (c_dialect_objc ())
      /* We have an Objective-C++ message. */
      return cp_parser_objc_expression (parser);
  } while (0)@)

#define PLUGIN_PRIMARY_EXPRESSION_2(parser, cp_parser_error) @(
  case CPP_OBJC_STRING:
    if (c_dialect_objc ())
      /* We have an Objective-C++ string literal. */
      return cp_parser_objc_expression (parser);
    cp_parser_error (parser, "expected primary-expression");
    return error_mark_node;@)

#define PLUGIN_PRIMARY_EXPRESSION_1(parser) @(
  /* Objective-C++ expressions.  */
  case RID_AT_ENCODE:
  case RID_AT_PROTOCOL:
  case RID_AT_SELECTOR:
    return cp_parser_objc_expression (parser);@)

#define PLUGIN_PRIMARY_EXPRESSION(parser, decl, cp_lexer_consume_token, cp_lexer_peek_token) @(
  do {
    /* In Objective-C++, we may have an Objective-C 2.0
       dot-syntax for classes here.  */
    if (c_dialect_objc ()
	&& cp_lexer_peek_token (parser->lexer)->type == CPP_DOT
	&& TREE_CODE (decl) == TYPE_DECL
	&& objc_is_class_name (decl))
      {
	tree component;
	cp_lexer_consume_token (parser->lexer);
	component = cp_parser_identifier (parser);
	if (component == error_mark_node)
	  return error_mark_node;

	return objc_build_class_component_ref (id_expression, component);
      }

    /* In Objective-C++, an instance variable (ivar) may be preferred
       to whatever cp_parser_lookup_name() found.  */
    decl = objc_lookup_ivar (decl, id_expression);
  } while (0)@)

#define PLUGIN_TOKEN_STARTS_CAST_EXPR @(
  do {
    /* '[' may start a primary-expression in obj-c++.  */
    return c_dialect_objc ();
  } while (0)@)

#define PLUGIN_STATEMENT @(
  /* Objective-C++ exception-handling constructs.  */
 case RID_AT_TRY:
 case RID_AT_CATCH:
 case RID_AT_FINALLY:
 case RID_AT_SYNCHRONIZED:
 case RID_AT_THROW:
   statement = cp_parser_objc_statement (parser);
   break;@)


#define PLUGIN_DECLARATION @(
  /* Objective-C++ declaration/definition.  */
  else if (c_dialect_objc () && OBJC_IS_AT_KEYWORD (token1.keyword))
    cp_parser_objc_declaration (parser, NULL_TREE);
  else if (c_dialect_objc ()
	   && token1.keyword == RID_ATTRIBUTE
	   && cp_parser_objc_valid_prefix_attributes (parser, &attributes))
    cp_parser_objc_declaration (parser, attributes);@)

#define PLUGIN_SIMPLE_TYPE_SPECIFIER @(
  do {
    /* See if TYPE is an Objective-C type, and if so, parse and
       accept any protocol references following it.  Do this before
       the cp_parser_check_for_invalid_template_id() call, because
       Objective-C types can be followed by '<...>' which would
       enclose protocol names rather than template arguments, and so
       everything is fine.  */
    if (c_dialect_objc () && !parser->scope
	&& (objc_is_id (type) || objc_is_class_name (type)))
      {
	tree protos = cp_parser_objc_protocol_refs_opt (parser);
	tree qual_type = objc_get_protocol_qualified_type (type, protos);

	/* Clobber the "unqualified" type previously entered into
	   DECL_SPECS with the new, improved protocol-qualified version.  */
	if (decl_specs)
	  decl_specs->type = qual_type;

	return qual_type;
      }
  } while (0)@)


#define PLUGIN_NONCLASS_NAME1 @(
  do {
    if (TREE_CODE (type_decl) != TYPE_DECL
	&& (objc_is_id (identifier) || objc_is_class_name (identifier)))
      {
	/* See if this is an Objective-C type.  */
	tree protos = cp_parser_objc_protocol_refs_opt (parser);
	tree type = objc_get_protocol_qualified_type (identifier, protos);
	if (type)
	  type_decl = TYPE_NAME (type);
      }
  } while (0)@)

#define PLUGIN_NONCLASS_NAME @(
  /* In Objective-C, we have the complication that class names are
     normally type names and start declarations (eg, the
     "NSObject" in "NSObject *object;"), but can be used in an
     Objective-C 2.0 dot-syntax (as in "NSObject.version") which
     is an expression.  So, a classname followed by a dot is not a
     valid type-name.  */
  || (objc_is_class_name (TREE_TYPE (type_decl))
      && cp_lexer_peek_token (parser->lexer)->type == CPP_DOT)@)

#define PLUGIN_CLASS_NAME @(
  /* In Objective-C 2.0, a classname followed by '.' starts a
     dot-syntax expression, and it's not a type-name.  */
  || (c_dialect_objc ()
      && cp_lexer_peek_token (parser->lexer)->type == CPP_DOT 
      && objc_is_class_name (decl))@)

#define PLUGIN_MEMBER_DECLARATION @(
  do {
    /* Check for @defs.  */
    if (cp_lexer_next_token_is_keyword (parser->lexer, RID_AT_DEFS))
      {
	tree ivar, member;
	tree ivar_chains = cp_parser_objc_defs_expression (parser);
	ivar = ivar_chains;
	while (ivar)
	  {
	    member = ivar;
	    ivar = TREE_CHAIN (member);
	    TREE_CHAIN (member) = NULL_TREE;
	    finish_member_declaration (member);
	  }
	return;
      }
  } while (0)@)

#else

#define PLUGIN_PRIMARY_EXPRESSION_3(parser)
#define PLUGIN_PRIMARY_EXPRESSION_2(parser, cp_parser_error)
#define PLUGIN_PRIMARY_EXPRESSION_1(parser)
#define PLUGIN_PRIMARY_EXPRESSION(parser, decl, cp_lexer_consume_token, cp_lexer_peek_token)
#define PLUGIN_TOKEN_STARTS_CAST_EXPR
#define PLUGIN_STATEMENT
#define PLUGIN_DECLARATION
#define PLUGIN_SIMPLE_TYPE_SPECIFIER
#define PLUGIN_NONCLASS_NAME1
#define PLUGIN_NONCLASS_NAME
#define PLUGIN_CLASS_NAME
#define PLUGIN_MEMBER_DECLARATION

#endif
