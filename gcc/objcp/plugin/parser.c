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


#ifndef HIDE_OBJC

/* Objective-C++ Productions */


/* Parse an Objective-C expression, which feeds into a primary-expression
   above.

   objc-expression:
     objc-message-expression
     objc-string-literal
     objc-encode-expression
     objc-protocol-expression
     objc-selector-expression

  Returns a tree representation of the expression.  */

static tree
cp_parser_objc_expression (cp_parser* parser)
{
  /* Try to figure out what kind of declaration is present.  */
  cp_token *kwd = cp_lexer_peek_token (parser->lexer);

  switch (kwd->type)
    {
    case CPP_OPEN_SQUARE:
      return cp_parser_objc_message_expression (parser);

    case CPP_OBJC_STRING:
      kwd = cp_lexer_consume_token (parser->lexer);
      return objc_build_string_object (kwd->u.value);

    case CPP_KEYWORD:
      switch (kwd->keyword)
	{
	case RID_AT_ENCODE:
	  return cp_parser_objc_encode_expression (parser);

	case RID_AT_PROTOCOL:
	  return cp_parser_objc_protocol_expression (parser);

	case RID_AT_SELECTOR:
	  return cp_parser_objc_selector_expression (parser);

	default:
	  break;
	}
    default:
      error_at (kwd->location,
		"misplaced %<@%D%> Objective-C++ construct",
		kwd->u.value);
      cp_parser_skip_to_end_of_block_or_statement (parser);
    }

  return error_mark_node;
}

/* Parse an Objective-C message expression.

   objc-message-expression:
     [ objc-message-receiver objc-message-args ]

   Returns a representation of an Objective-C message.  */

static tree
cp_parser_objc_message_expression (cp_parser* parser)
{
  tree receiver, messageargs;

  cp_lexer_consume_token (parser->lexer);  /* Eat '['.  */
  receiver = cp_parser_objc_message_receiver (parser);
  messageargs = cp_parser_objc_message_args (parser);
  cp_parser_require (parser, CPP_CLOSE_SQUARE, RT_CLOSE_SQUARE);

  return objc_build_message_expr (build_tree_list (receiver, messageargs));
}

/* Parse an objc-message-receiver.

   objc-message-receiver:
     expression
     simple-type-specifier

  Returns a representation of the type or expression.  */

static tree
cp_parser_objc_message_receiver (cp_parser* parser)
{
  tree rcv;

  /* An Objective-C message receiver may be either (1) a type
     or (2) an expression.  */
  cp_parser_parse_tentatively (parser);
  rcv = cp_parser_expression (parser, false, NULL);

  if (cp_parser_parse_definitely (parser))
    return rcv;

  rcv = cp_parser_simple_type_specifier (parser,
					 /*decl_specs=*/NULL,
					 CP_PARSER_FLAGS_NONE);

  return objc_get_class_reference (rcv);
}

/* Parse the arguments and selectors comprising an Objective-C message.

   objc-message-args:
     objc-selector
     objc-selector-args
     objc-selector-args , objc-comma-args

   objc-selector-args:
     objc-selector [opt] : assignment-expression
     objc-selector-args objc-selector [opt] : assignment-expression

   objc-comma-args:
     assignment-expression
     objc-comma-args , assignment-expression

   Returns a TREE_LIST, with TREE_PURPOSE containing a list of
   selector arguments and TREE_VALUE containing a list of comma
   arguments.  */

static tree
cp_parser_objc_message_args (cp_parser* parser)
{
  tree sel_args = NULL_TREE, addl_args = NULL_TREE;
  bool maybe_unary_selector_p = true;
  cp_token *token = cp_lexer_peek_token (parser->lexer);

  while (cp_parser_objc_selector_p (token->type) || token->type == CPP_COLON)
    {
      tree selector = NULL_TREE, arg;

      if (token->type != CPP_COLON)
	selector = cp_parser_objc_selector (parser);

      /* Detect if we have a unary selector.  */
      if (maybe_unary_selector_p
	  && cp_lexer_next_token_is_not (parser->lexer, CPP_COLON))
	return build_tree_list (selector, NULL_TREE);

      maybe_unary_selector_p = false;
      cp_parser_require (parser, CPP_COLON, RT_COLON);
      arg = cp_parser_assignment_expression (parser, false, NULL);

      sel_args
	= chainon (sel_args,
		   build_tree_list (selector, arg));

      token = cp_lexer_peek_token (parser->lexer);
    }

  /* Handle non-selector arguments, if any. */
  while (token->type == CPP_COMMA)
    {
      tree arg;

      cp_lexer_consume_token (parser->lexer);
      arg = cp_parser_assignment_expression (parser, false, NULL);

      addl_args
	= chainon (addl_args,
		   build_tree_list (NULL_TREE, arg));

      token = cp_lexer_peek_token (parser->lexer);
    }

  if (sel_args == NULL_TREE && addl_args == NULL_TREE)
    {
      cp_parser_error (parser, "objective-c++ message argument(s) are expected");
      return build_tree_list (error_mark_node, error_mark_node);
    }

  return build_tree_list (sel_args, addl_args);
}

/* Parse an Objective-C encode expression.

   objc-encode-expression:
     @encode objc-typename

   Returns an encoded representation of the type argument.  */

static tree
cp_parser_objc_encode_expression (cp_parser* parser)
{
  tree type;
  cp_token *token;

  cp_lexer_consume_token (parser->lexer);  /* Eat '@encode'.  */
  cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN);
  token = cp_lexer_peek_token (parser->lexer);
  type = complete_type (cp_parser_type_id (parser));
  cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN);

  if (!type)
    {
      error_at (token->location, 
		"%<@encode%> must specify a type as an argument");
      return error_mark_node;
    }

  /* This happens if we find @encode(T) (where T is a template
     typename or something dependent on a template typename) when
     parsing a template.  In that case, we can't compile it
     immediately, but we rather create an AT_ENCODE_EXPR which will
     need to be instantiated when the template is used.
  */
  if (dependent_type_p (type))
    {
      tree value = build_min (AT_ENCODE_EXPR, size_type_node, type);
      TREE_READONLY (value) = 1;
      return value;
    }

  return objc_build_encode_expr (type);
}

/* Parse an Objective-C @defs expression.  */

static tree
cp_parser_objc_defs_expression (cp_parser *parser)
{
  tree name;

  cp_lexer_consume_token (parser->lexer);  /* Eat '@defs'.  */
  cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN);
  name = cp_parser_identifier (parser);
  cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN);

  return objc_get_class_ivars (name);
}

/* Parse an Objective-C protocol expression.

  objc-protocol-expression:
    @protocol ( identifier )

  Returns a representation of the protocol expression.  */

static tree
cp_parser_objc_protocol_expression (cp_parser* parser)
{
  tree proto;

  cp_lexer_consume_token (parser->lexer);  /* Eat '@protocol'.  */
  cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN);
  proto = cp_parser_identifier (parser);
  cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN);

  return objc_build_protocol_expr (proto);
}

/* Parse an Objective-C selector expression.

   objc-selector-expression:
     @selector ( objc-method-signature )

   objc-method-signature:
     objc-selector
     objc-selector-seq

   objc-selector-seq:
     objc-selector :
     objc-selector-seq objc-selector :

  Returns a representation of the method selector.  */

static tree
cp_parser_objc_selector_expression (cp_parser* parser)
{
  tree sel_seq = NULL_TREE;
  bool maybe_unary_selector_p = true;
  cp_token *token;
  location_t loc = cp_lexer_peek_token (parser->lexer)->location;

  cp_lexer_consume_token (parser->lexer);  /* Eat '@selector'.  */
  cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN);
  token = cp_lexer_peek_token (parser->lexer);

  while (cp_parser_objc_selector_p (token->type) || token->type == CPP_COLON
	 || token->type == CPP_SCOPE)
    {
      tree selector = NULL_TREE;

      if (token->type != CPP_COLON
	  || token->type == CPP_SCOPE)
	selector = cp_parser_objc_selector (parser);

      if (cp_lexer_next_token_is_not (parser->lexer, CPP_COLON)
	  && cp_lexer_next_token_is_not (parser->lexer, CPP_SCOPE))
	{
	  /* Detect if we have a unary selector.  */
	  if (maybe_unary_selector_p)
	    {
	      sel_seq = selector;
	      goto finish_selector;
	    }
	  else
	    {
	      cp_parser_error (parser, "expected %<:%>");
	    }
	}
      maybe_unary_selector_p = false;
      token = cp_lexer_consume_token (parser->lexer);

      if (token->type == CPP_SCOPE)
	{
	  sel_seq
	    = chainon (sel_seq,
		       build_tree_list (selector, NULL_TREE));
	  sel_seq
	    = chainon (sel_seq,
		       build_tree_list (NULL_TREE, NULL_TREE));
	}
      else
	sel_seq
	  = chainon (sel_seq,
		     build_tree_list (selector, NULL_TREE));

      token = cp_lexer_peek_token (parser->lexer);
    }

 finish_selector:
  cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN);

  return objc_build_selector_expr (loc, sel_seq);
}

/* Parse a list of identifiers.

   objc-identifier-list:
     identifier
     objc-identifier-list , identifier

   Returns a TREE_LIST of identifier nodes.  */

static tree
cp_parser_objc_identifier_list (cp_parser* parser)
{
  tree identifier;
  tree list;
  cp_token *sep;

  identifier = cp_parser_identifier (parser);
  if (identifier == error_mark_node)
    return error_mark_node;      

  list = build_tree_list (NULL_TREE, identifier);
  sep = cp_lexer_peek_token (parser->lexer);

  while (sep->type == CPP_COMMA)
    {
      cp_lexer_consume_token (parser->lexer);  /* Eat ','.  */
      identifier = cp_parser_identifier (parser);
      if (identifier == error_mark_node)
	return list;

      list = chainon (list, build_tree_list (NULL_TREE,
					     identifier));
      sep = cp_lexer_peek_token (parser->lexer);
    }
  
  return list;
}

/* Parse an Objective-C alias declaration.

   objc-alias-declaration:
     @compatibility_alias identifier identifier ;

   This function registers the alias mapping with the Objective-C front end.
   It returns nothing.  */

static void
cp_parser_objc_alias_declaration (cp_parser* parser)
{
  tree alias, orig;

  cp_lexer_consume_token (parser->lexer);  /* Eat '@compatibility_alias'.  */
  alias = cp_parser_identifier (parser);
  orig = cp_parser_identifier (parser);
  objc_declare_alias (alias, orig);
  cp_parser_consume_semicolon_at_end_of_statement (parser);
}

/* Parse an Objective-C class forward-declaration.

   objc-class-declaration:
     @class objc-identifier-list ;

   The function registers the forward declarations with the Objective-C
   front end.  It returns nothing.  */

static void
cp_parser_objc_class_declaration (cp_parser* parser)
{
  cp_lexer_consume_token (parser->lexer);  /* Eat '@class'.  */
  objc_declare_class (cp_parser_objc_identifier_list (parser));
  cp_parser_consume_semicolon_at_end_of_statement (parser);
}

/* Parse a list of Objective-C protocol references.

   objc-protocol-refs-opt:
     objc-protocol-refs [opt]

   objc-protocol-refs:
     < objc-identifier-list >

   Returns a TREE_LIST of identifiers, if any.  */

static tree
cp_parser_objc_protocol_refs_opt (cp_parser* parser)
{
  tree protorefs = NULL_TREE;

  if(cp_lexer_next_token_is (parser->lexer, CPP_LESS))
    {
      cp_lexer_consume_token (parser->lexer);  /* Eat '<'.  */
      protorefs = cp_parser_objc_identifier_list (parser);
      cp_parser_require (parser, CPP_GREATER, RT_GREATER);
    }

  return protorefs;
}

/* Parse a Objective-C visibility specification.  */

static void
cp_parser_objc_visibility_spec (cp_parser* parser)
{
  cp_token *vis = cp_lexer_peek_token (parser->lexer);

  switch (vis->keyword)
    {
    case RID_AT_PRIVATE:
      objc_set_visibility (OBJC_IVAR_VIS_PRIVATE);
      break;
    case RID_AT_PROTECTED:
      objc_set_visibility (OBJC_IVAR_VIS_PROTECTED);
      break;
    case RID_AT_PUBLIC:
      objc_set_visibility (OBJC_IVAR_VIS_PUBLIC);
      break;
    case RID_AT_PACKAGE:
      objc_set_visibility (OBJC_IVAR_VIS_PACKAGE);
      break;
    default:
      return;
    }

  /* Eat '@private'/'@protected'/'@public'.  */
  cp_lexer_consume_token (parser->lexer);
}

/* Parse an Objective-C method type.  Return 'true' if it is a class
   (+) method, and 'false' if it is an instance (-) method.  */

static inline bool
cp_parser_objc_method_type (cp_parser* parser)
{
  if (cp_lexer_consume_token (parser->lexer)->type == CPP_PLUS)
    return true;
  else
    return false;
}

/* Parse an Objective-C protocol qualifier.  */

static tree
cp_parser_objc_protocol_qualifiers (cp_parser* parser)
{
  tree quals = NULL_TREE, node;
  cp_token *token = cp_lexer_peek_token (parser->lexer);

  node = token->u.value;

  while (node && TREE_CODE (node) == IDENTIFIER_NODE
	 && (node == ridpointers [(int) RID_IN]
	     || node == ridpointers [(int) RID_OUT]
	     || node == ridpointers [(int) RID_INOUT]
	     || node == ridpointers [(int) RID_BYCOPY]
	     || node == ridpointers [(int) RID_BYREF]
	     || node == ridpointers [(int) RID_ONEWAY]))
    {
      quals = tree_cons (NULL_TREE, node, quals);
      cp_lexer_consume_token (parser->lexer);
      token = cp_lexer_peek_token (parser->lexer);
      node = token->u.value;
    }

  return quals;
}

/* Parse an Objective-C typename.  */

static tree
cp_parser_objc_typename (cp_parser* parser)
{
  tree type_name = NULL_TREE;

  if (cp_lexer_next_token_is (parser->lexer, CPP_OPEN_PAREN))
    {
      tree proto_quals, cp_type = NULL_TREE;

      cp_lexer_consume_token (parser->lexer);  /* Eat '('.  */
      proto_quals = cp_parser_objc_protocol_qualifiers (parser);

      /* An ObjC type name may consist of just protocol qualifiers, in which
	 case the type shall default to 'id'.  */
      if (cp_lexer_next_token_is_not (parser->lexer, CPP_CLOSE_PAREN))
	{
	  cp_type = cp_parser_type_id (parser);
	  
	  /* If the type could not be parsed, an error has already
	     been produced.  For error recovery, behave as if it had
	     not been specified, which will use the default type
	     'id'.  */
	  if (cp_type == error_mark_node)
	    {
	      cp_type = NULL_TREE;
	      /* We need to skip to the closing parenthesis as
		 cp_parser_type_id() does not seem to do it for
		 us.  */
	      cp_parser_skip_to_closing_parenthesis (parser,
						     /*recovering=*/true,
						     /*or_comma=*/false,
						     /*consume_paren=*/false);
	    }
	}

      cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN);
      type_name = build_tree_list (proto_quals, cp_type);
    }

  return type_name;
}

/* Check to see if TYPE refers to an Objective-C selector name.  */

static bool
cp_parser_objc_selector_p (enum cpp_ttype type)
{
  return (type == CPP_NAME || type == CPP_KEYWORD
	  || type == CPP_AND_AND || type == CPP_AND_EQ || type == CPP_AND
	  || type == CPP_OR || type == CPP_COMPL || type == CPP_NOT
	  || type == CPP_NOT_EQ || type == CPP_OR_OR || type == CPP_OR_EQ
	  || type == CPP_XOR || type == CPP_XOR_EQ);
}

/* Parse an Objective-C selector.  */

static tree
cp_parser_objc_selector (cp_parser* parser)
{
  cp_token *token = cp_lexer_consume_token (parser->lexer);

  if (!cp_parser_objc_selector_p (token->type))
    {
      error_at (token->location, "invalid Objective-C++ selector name");
      return error_mark_node;
    }

  /* C++ operator names are allowed to appear in ObjC selectors.  */
  switch (token->type)
    {
    case CPP_AND_AND: return get_identifier ("and");
    case CPP_AND_EQ: return get_identifier ("and_eq");
    case CPP_AND: return get_identifier ("bitand");
    case CPP_OR: return get_identifier ("bitor");
    case CPP_COMPL: return get_identifier ("compl");
    case CPP_NOT: return get_identifier ("not");
    case CPP_NOT_EQ: return get_identifier ("not_eq");
    case CPP_OR_OR: return get_identifier ("or");
    case CPP_OR_EQ: return get_identifier ("or_eq");
    case CPP_XOR: return get_identifier ("xor");
    case CPP_XOR_EQ: return get_identifier ("xor_eq");
    default: return token->u.value;
    }
}

/* Parse an Objective-C params list.  */

static tree
cp_parser_objc_method_keyword_params (cp_parser* parser, tree* attributes)
{
  tree params = NULL_TREE;
  bool maybe_unary_selector_p = true;
  cp_token *token = cp_lexer_peek_token (parser->lexer);

  while (cp_parser_objc_selector_p (token->type) || token->type == CPP_COLON)
    {
      tree selector = NULL_TREE, type_name, identifier;
      tree parm_attr = NULL_TREE;

      if (token->keyword == RID_ATTRIBUTE)
	break;

      if (token->type != CPP_COLON)
	selector = cp_parser_objc_selector (parser);

      /* Detect if we have a unary selector.  */
      if (maybe_unary_selector_p
	  && cp_lexer_next_token_is_not (parser->lexer, CPP_COLON))
	{
	  params = selector; /* Might be followed by attributes.  */
	  break;
	}

      maybe_unary_selector_p = false;
      if (!cp_parser_require (parser, CPP_COLON, RT_COLON))
	{
	  /* Something went quite wrong.  There should be a colon
	     here, but there is not.  Stop parsing parameters.  */
	  break;
	}
      type_name = cp_parser_objc_typename (parser);
      /* New ObjC allows attributes on parameters too.  */
      if (cp_lexer_next_token_is_keyword (parser->lexer, RID_ATTRIBUTE))
	parm_attr = cp_parser_attributes_opt (parser);
      identifier = cp_parser_identifier (parser);

      params
	= chainon (params,
		   objc_build_keyword_decl (selector,
					    type_name,
					    identifier,
					    parm_attr));

      token = cp_lexer_peek_token (parser->lexer);
    }

  if (params == NULL_TREE)
    {
      cp_parser_error (parser, "objective-c++ method declaration is expected");
      return error_mark_node;
    }

  /* We allow tail attributes for the method.  */
  if (token->keyword == RID_ATTRIBUTE)
    {
      *attributes = cp_parser_attributes_opt (parser);
      if (cp_lexer_next_token_is (parser->lexer, CPP_SEMICOLON)
	  || cp_lexer_next_token_is (parser->lexer, CPP_OPEN_BRACE))
	return params;
      cp_parser_error (parser, 
		       "method attributes must be specified at the end");
      return error_mark_node;
    }

  if (params == NULL_TREE)
    {
      cp_parser_error (parser, "objective-c++ method declaration is expected");
      return error_mark_node;
    }
  return params;
}

/* Parse the non-keyword Objective-C params.  */

static tree
cp_parser_objc_method_tail_params_opt (cp_parser* parser, bool *ellipsisp, 
				       tree* attributes)
{
  tree params = make_node (TREE_LIST);
  cp_token *token = cp_lexer_peek_token (parser->lexer);
  *ellipsisp = false;  /* Initially, assume no ellipsis.  */

  while (token->type == CPP_COMMA)
    {
      cp_parameter_declarator *parmdecl;
      tree parm;

      cp_lexer_consume_token (parser->lexer);  /* Eat ','.  */
      token = cp_lexer_peek_token (parser->lexer);

      if (token->type == CPP_ELLIPSIS)
	{
	  cp_lexer_consume_token (parser->lexer);  /* Eat '...'.  */
	  *ellipsisp = true;
	  token = cp_lexer_peek_token (parser->lexer);
	  break;
	}

      /* TODO: parse attributes for tail parameters.  */
      parmdecl = cp_parser_parameter_declaration (parser, false, NULL);
      parm = grokdeclarator (parmdecl->declarator,
			     &parmdecl->decl_specifiers,
			     PARM, /*initialized=*/0,
			     /*attrlist=*/NULL);

      chainon (params, build_tree_list (NULL_TREE, parm));
      token = cp_lexer_peek_token (parser->lexer);
    }

  /* We allow tail attributes for the method.  */
  if (token->keyword == RID_ATTRIBUTE)
    {
      if (*attributes == NULL_TREE)
	{
	  *attributes = cp_parser_attributes_opt (parser);
	  if (cp_lexer_next_token_is (parser->lexer, CPP_SEMICOLON)
	      || cp_lexer_next_token_is (parser->lexer, CPP_OPEN_BRACE))
	    return params;
	}
      else        
	/* We have an error, but parse the attributes, so that we can 
	   carry on.  */
	*attributes = cp_parser_attributes_opt (parser);

      cp_parser_error (parser, 
		       "method attributes must be specified at the end");
      return error_mark_node;
    }

  return params;
}

/* Parse a linkage specification, a pragma, an extra semicolon or a block.  */

static void
cp_parser_objc_interstitial_code (cp_parser* parser)
{
  cp_token *token = cp_lexer_peek_token (parser->lexer);

  /* If the next token is `extern' and the following token is a string
     literal, then we have a linkage specification.  */
  if (token->keyword == RID_EXTERN
      && cp_parser_is_string_literal (cp_lexer_peek_nth_token (parser->lexer, 2)))
    cp_parser_linkage_specification (parser);
  /* Handle #pragma, if any.  */
  else if (token->type == CPP_PRAGMA)
    cp_parser_pragma (parser, pragma_external);
  /* Allow stray semicolons.  */
  else if (token->type == CPP_SEMICOLON)
    cp_lexer_consume_token (parser->lexer);
  /* Mark methods as optional or required, when building protocols.  */
  else if (token->keyword == RID_AT_OPTIONAL)
    {
      cp_lexer_consume_token (parser->lexer);
      objc_set_method_opt (true);
    }
  else if (token->keyword == RID_AT_REQUIRED)
    {
      cp_lexer_consume_token (parser->lexer);
      objc_set_method_opt (false);
    }
  else if (token->keyword == RID_NAMESPACE)
    cp_parser_namespace_definition (parser);
  /* Other stray characters must generate errors.  */
  else if (token->type == CPP_OPEN_BRACE || token->type == CPP_CLOSE_BRACE)
    {
      cp_lexer_consume_token (parser->lexer);
      error ("stray %qs between Objective-C++ methods",
	     token->type == CPP_OPEN_BRACE ? "{" : "}");
    }
  /* Finally, try to parse a block-declaration, or a function-definition.  */
  else
    cp_parser_block_declaration (parser, /*statement_p=*/false);
}

/* Parse a method signature.  */

static tree
cp_parser_objc_method_signature (cp_parser* parser, tree* attributes)
{
  tree rettype, kwdparms, optparms;
  bool ellipsis = false;
  bool is_class_method;

  is_class_method = cp_parser_objc_method_type (parser);
  rettype = cp_parser_objc_typename (parser);
  *attributes = NULL_TREE;
  kwdparms = cp_parser_objc_method_keyword_params (parser, attributes);
  if (kwdparms == error_mark_node)
    return error_mark_node;
  optparms = cp_parser_objc_method_tail_params_opt (parser, &ellipsis, attributes);
  if (optparms == error_mark_node)
    return error_mark_node;

  return objc_build_method_signature (is_class_method, rettype, kwdparms, optparms, ellipsis);
}

static bool
cp_parser_objc_method_maybe_bad_prefix_attributes (cp_parser* parser)
{
  tree tattr;  
  cp_lexer_save_tokens (parser->lexer);
  tattr = cp_parser_attributes_opt (parser);
  gcc_assert (tattr) ;
  
  /* If the attributes are followed by a method introducer, this is not allowed.
     Dump the attributes and flag the situation.  */
  if (cp_lexer_next_token_is (parser->lexer, CPP_PLUS)
      || cp_lexer_next_token_is (parser->lexer, CPP_MINUS))
    return true;

  /* Otherwise, the attributes introduce some interstitial code, possibly so
     rewind to allow that check.  */
  cp_lexer_rollback_tokens (parser->lexer);
  return false;  
}

/* Parse an Objective-C method prototype list.  */

static void
cp_parser_objc_method_prototype_list (cp_parser* parser)
{
  cp_token *token = cp_lexer_peek_token (parser->lexer);

  while (token->keyword != RID_AT_END && token->type != CPP_EOF)
    {
      if (token->type == CPP_PLUS || token->type == CPP_MINUS)
	{
	  tree attributes, sig;
	  bool is_class_method;
	  if (token->type == CPP_PLUS)
	    is_class_method = true;
	  else
	    is_class_method = false;
	  sig = cp_parser_objc_method_signature (parser, &attributes);
	  if (sig == error_mark_node)
	    {
	      cp_parser_skip_to_end_of_block_or_statement (parser);
	      token = cp_lexer_peek_token (parser->lexer);
	      continue;
	    }
	  objc_add_method_declaration (is_class_method, sig, attributes);
	  cp_parser_consume_semicolon_at_end_of_statement (parser);
	}
      else if (token->keyword == RID_AT_PROPERTY)
	cp_parser_objc_at_property_declaration (parser);
      else if (token->keyword == RID_ATTRIBUTE 
      	       && cp_parser_objc_method_maybe_bad_prefix_attributes(parser))
	warning_at (cp_lexer_peek_token (parser->lexer)->location, 
		    OPT_Wattributes, 
		    "prefix attributes are ignored for methods");
      else
	/* Allow for interspersed non-ObjC++ code.  */
	cp_parser_objc_interstitial_code (parser);

      token = cp_lexer_peek_token (parser->lexer);
    }

  if (token->type != CPP_EOF)
    cp_lexer_consume_token (parser->lexer);  /* Eat '@end'.  */
  else
    cp_parser_error (parser, "expected %<@end%>");

  objc_finish_interface ();
}

/* Parse an Objective-C method definition list.  */

static void
cp_parser_objc_method_definition_list (cp_parser* parser)
{
  cp_token *token = cp_lexer_peek_token (parser->lexer);

  while (token->keyword != RID_AT_END && token->type != CPP_EOF)
    {
      tree meth;

      if (token->type == CPP_PLUS || token->type == CPP_MINUS)
	{
	  cp_token *ptk;
	  tree sig, attribute;
	  bool is_class_method;
	  if (token->type == CPP_PLUS)
	    is_class_method = true;
	  else
	    is_class_method = false;
	  push_deferring_access_checks (dk_deferred);
	  sig = cp_parser_objc_method_signature (parser, &attribute);
	  if (sig == error_mark_node)
	    {
	      cp_parser_skip_to_end_of_block_or_statement (parser);
	      token = cp_lexer_peek_token (parser->lexer);
	      continue;
	    }
	  objc_start_method_definition (is_class_method, sig, attribute);

	  /* For historical reasons, we accept an optional semicolon.  */
	  if (cp_lexer_next_token_is (parser->lexer, CPP_SEMICOLON))
	    cp_lexer_consume_token (parser->lexer);

	  ptk = cp_lexer_peek_token (parser->lexer);
	  if (!(ptk->type == CPP_PLUS || ptk->type == CPP_MINUS 
		|| ptk->type == CPP_EOF || ptk->keyword == RID_AT_END))
	    {
	      perform_deferred_access_checks ();
	      stop_deferring_access_checks ();
	      meth = cp_parser_function_definition_after_declarator (parser,
								     false);
	      pop_deferring_access_checks ();
	      objc_finish_method_definition (meth);
	    }
	}
      /* The following case will be removed once @synthesize is
	 completely implemented.  */
      else if (token->keyword == RID_AT_PROPERTY)
	cp_parser_objc_at_property_declaration (parser);
      else if (token->keyword == RID_AT_SYNTHESIZE)
	cp_parser_objc_at_synthesize_declaration (parser);
      else if (token->keyword == RID_AT_DYNAMIC)
	cp_parser_objc_at_dynamic_declaration (parser);
      else if (token->keyword == RID_ATTRIBUTE 
      	       && cp_parser_objc_method_maybe_bad_prefix_attributes(parser))
	warning_at (token->location, OPT_Wattributes,
	       	    "prefix attributes are ignored for methods");
      else
	/* Allow for interspersed non-ObjC++ code.  */
	cp_parser_objc_interstitial_code (parser);

      token = cp_lexer_peek_token (parser->lexer);
    }

  if (token->type != CPP_EOF)
    cp_lexer_consume_token (parser->lexer);  /* Eat '@end'.  */
  else
    cp_parser_error (parser, "expected %<@end%>");

  objc_finish_implementation ();
}

/* Parse Objective-C ivars.  */

static void
cp_parser_objc_class_ivars (cp_parser* parser)
{
  cp_token *token = cp_lexer_peek_token (parser->lexer);

  if (token->type != CPP_OPEN_BRACE)
    return;	/* No ivars specified.  */

  cp_lexer_consume_token (parser->lexer);  /* Eat '{'.  */
  token = cp_lexer_peek_token (parser->lexer);

  while (token->type != CPP_CLOSE_BRACE 
	&& token->keyword != RID_AT_END && token->type != CPP_EOF)
    {
      cp_decl_specifier_seq declspecs;
      int decl_class_or_enum_p;
      tree prefix_attributes;

      cp_parser_objc_visibility_spec (parser);

      if (cp_lexer_next_token_is (parser->lexer, CPP_CLOSE_BRACE))
	break;

      cp_parser_decl_specifier_seq (parser,
				    CP_PARSER_FLAGS_OPTIONAL,
				    &declspecs,
				    &decl_class_or_enum_p);

      /* auto, register, static, extern, mutable.  */
      if (declspecs.storage_class != sc_none)
	{
	  cp_parser_error (parser, "invalid type for instance variable");	  
	  declspecs.storage_class = sc_none;
	}

      /* __thread.  */
      if (declspecs.specs[(int) ds_thread])
	{
	  cp_parser_error (parser, "invalid type for instance variable");
	  declspecs.specs[(int) ds_thread] = 0;
	}
      
      /* typedef.  */
      if (declspecs.specs[(int) ds_typedef])
	{
	  cp_parser_error (parser, "invalid type for instance variable");
	  declspecs.specs[(int) ds_typedef] = 0;
	}

      prefix_attributes = declspecs.attributes;
      declspecs.attributes = NULL_TREE;

      /* Keep going until we hit the `;' at the end of the
	 declaration.  */
      while (cp_lexer_next_token_is_not (parser->lexer, CPP_SEMICOLON))
	{
	  tree width = NULL_TREE, attributes, first_attribute, decl;
	  cp_declarator *declarator = NULL;
	  int ctor_dtor_or_conv_p;

	  /* Check for a (possibly unnamed) bitfield declaration.  */
	  token = cp_lexer_peek_token (parser->lexer);
	  if (token->type == CPP_COLON)
	    goto eat_colon;

	  if (token->type == CPP_NAME
	      && (cp_lexer_peek_nth_token (parser->lexer, 2)->type
		  == CPP_COLON))
	    {
	      /* Get the name of the bitfield.  */
	      declarator = make_id_declarator (NULL_TREE,
					       cp_parser_identifier (parser),
					       sfk_none);

	     eat_colon:
	      cp_lexer_consume_token (parser->lexer);  /* Eat ':'.  */
	      /* Get the width of the bitfield.  */
	      width
		= cp_parser_constant_expression (parser,
						 /*allow_non_constant=*/false,
						 NULL);
	    }
	  else
	    {
	      /* Parse the declarator.  */
	      declarator
		= cp_parser_declarator (parser, CP_PARSER_DECLARATOR_NAMED,
					&ctor_dtor_or_conv_p,
					/*parenthesized_p=*/NULL,
					/*member_p=*/false);
	    }

	  /* Look for attributes that apply to the ivar.  */
	  attributes = cp_parser_attributes_opt (parser);
	  /* Remember which attributes are prefix attributes and
	     which are not.  */
	  first_attribute = attributes;
	  /* Combine the attributes.  */
	  attributes = chainon (prefix_attributes, attributes);

	  if (width)
	      /* Create the bitfield declaration.  */
	      decl = grokbitfield (declarator, &declspecs,
				   width,
				   attributes);
	  else
	    decl = grokfield (declarator, &declspecs,
			      NULL_TREE, /*init_const_expr_p=*/false,
			      NULL_TREE, attributes);

	  /* Add the instance variable.  */
	  objc_add_instance_variable (decl);

	  /* Reset PREFIX_ATTRIBUTES.  */
	  while (attributes && TREE_CHAIN (attributes) != first_attribute)
	    attributes = TREE_CHAIN (attributes);
	  if (attributes)
	    TREE_CHAIN (attributes) = NULL_TREE;

	  token = cp_lexer_peek_token (parser->lexer);

	  if (token->type == CPP_COMMA)
	    {
	      cp_lexer_consume_token (parser->lexer);  /* Eat ','.  */
	      continue;
	    }
	  break;
	}

      cp_parser_consume_semicolon_at_end_of_statement (parser);
      token = cp_lexer_peek_token (parser->lexer);
    }

  if (token->keyword == RID_AT_END)
    cp_parser_error (parser, "expected %<}%>");

  /* Do not consume the RID_AT_END, so it will be read again as terminating
     the @interface of @implementation.  */ 
  if (token->keyword != RID_AT_END && token->type != CPP_EOF)
    cp_lexer_consume_token (parser->lexer);  /* Eat '}'.  */
    
  /* For historical reasons, we accept an optional semicolon.  */
  if (cp_lexer_next_token_is (parser->lexer, CPP_SEMICOLON))
    cp_lexer_consume_token (parser->lexer);
}

/* Parse an Objective-C protocol declaration.  */

static void
cp_parser_objc_protocol_declaration (cp_parser* parser, tree attributes)
{
  tree proto, protorefs;
  cp_token *tok;

  cp_lexer_consume_token (parser->lexer);  /* Eat '@protocol'.  */
  if (cp_lexer_next_token_is_not (parser->lexer, CPP_NAME))
    {
      tok = cp_lexer_peek_token (parser->lexer);
      error_at (tok->location, "identifier expected after %<@protocol%>");
      goto finish;
    }

  /* See if we have a forward declaration or a definition.  */
  tok = cp_lexer_peek_nth_token (parser->lexer, 2);

  /* Try a forward declaration first.  */
  if (tok->type == CPP_COMMA || tok->type == CPP_SEMICOLON)
    {
      objc_declare_protocols (cp_parser_objc_identifier_list (parser), 
			      attributes);
     finish:
      cp_parser_consume_semicolon_at_end_of_statement (parser);
    }

  /* Ok, we got a full-fledged definition (or at least should).  */
  else
    {
      proto = cp_parser_identifier (parser);
      protorefs = cp_parser_objc_protocol_refs_opt (parser);
      objc_start_protocol (proto, protorefs, attributes);
      cp_parser_objc_method_prototype_list (parser);
    }
}

/* Parse an Objective-C superclass or category.  */

static void
cp_parser_objc_superclass_or_category (cp_parser *parser, 
				       bool iface_p,
				       tree *super,
				       tree *categ, bool *is_class_extension)
{
  cp_token *next = cp_lexer_peek_token (parser->lexer);

  *super = *categ = NULL_TREE;
  *is_class_extension = false;
  if (next->type == CPP_COLON)
    {
      cp_lexer_consume_token (parser->lexer);  /* Eat ':'.  */
      *super = cp_parser_identifier (parser);
    }
  else if (next->type == CPP_OPEN_PAREN)
    {
      cp_lexer_consume_token (parser->lexer);  /* Eat '('.  */

      /* If there is no category name, and this is an @interface, we
	 have a class extension.  */
      if (iface_p && cp_lexer_next_token_is (parser->lexer, CPP_CLOSE_PAREN))
	{
	  *categ = NULL_TREE;
	  *is_class_extension = true;
	}
      else
	*categ = cp_parser_identifier (parser);

      cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN);
    }
}

/* Parse an Objective-C class interface.  */

static void
cp_parser_objc_class_interface (cp_parser* parser, tree attributes)
{
  tree name, super, categ, protos;
  bool is_class_extension;

  cp_lexer_consume_token (parser->lexer);  /* Eat '@interface'.  */
  name = cp_parser_identifier (parser);
  if (name == error_mark_node)
    {
      /* It's hard to recover because even if valid @interface stuff
	 is to follow, we can't compile it (or validate it) if we
	 don't even know which class it refers to.  Let's assume this
	 was a stray '@interface' token in the stream and skip it.
      */
      return;
    }
  cp_parser_objc_superclass_or_category (parser, true, &super, &categ,
					 &is_class_extension);
  protos = cp_parser_objc_protocol_refs_opt (parser);

  /* We have either a class or a category on our hands.  */
  if (categ || is_class_extension)
    objc_start_category_interface (name, categ, protos, attributes);
  else
    {
      objc_start_class_interface (name, super, protos, attributes);
      /* Handle instance variable declarations, if any.  */
      cp_parser_objc_class_ivars (parser);
      objc_continue_interface ();
    }

  cp_parser_objc_method_prototype_list (parser);
}

/* Parse an Objective-C class implementation.  */

static void
cp_parser_objc_class_implementation (cp_parser* parser)
{
  tree name, super, categ;
  bool is_class_extension;

  cp_lexer_consume_token (parser->lexer);  /* Eat '@implementation'.  */
  name = cp_parser_identifier (parser);
  if (name == error_mark_node)
    {
      /* It's hard to recover because even if valid @implementation
	 stuff is to follow, we can't compile it (or validate it) if
	 we don't even know which class it refers to.  Let's assume
	 this was a stray '@implementation' token in the stream and
	 skip it.
      */
      return;
    }
  cp_parser_objc_superclass_or_category (parser, false, &super, &categ,
					 &is_class_extension);

  /* We have either a class or a category on our hands.  */
  if (categ)
    objc_start_category_implementation (name, categ);
  else
    {
      objc_start_class_implementation (name, super);
      /* Handle instance variable declarations, if any.  */
      cp_parser_objc_class_ivars (parser);
      objc_continue_implementation ();
    }

  cp_parser_objc_method_definition_list (parser);
}

/* Consume the @end token and finish off the implementation.  */

static void
cp_parser_objc_end_implementation (cp_parser* parser)
{
  cp_lexer_consume_token (parser->lexer);  /* Eat '@end'.  */
  objc_finish_implementation ();
}

/* Parse an Objective-C declaration.  */

static void
cp_parser_objc_declaration (cp_parser* parser, tree attributes)
{
  /* Try to figure out what kind of declaration is present.  */
  cp_token *kwd = cp_lexer_peek_token (parser->lexer);

  if (attributes)
    switch (kwd->keyword)
      {
	case RID_AT_ALIAS:
	case RID_AT_CLASS:
	case RID_AT_END:
	  error_at (kwd->location, "attributes may not be specified before"
	            " the %<@%D%> Objective-C++ keyword",
		    kwd->u.value);
	  attributes = NULL;
	  break;
	case RID_AT_IMPLEMENTATION:
	  warning_at (kwd->location, OPT_Wattributes,
		      "prefix attributes are ignored before %<@%D%>",
		      kwd->u.value);
	  attributes = NULL;
	default:
	  break;
      }

  switch (kwd->keyword)
    {
    case RID_AT_ALIAS:
      cp_parser_objc_alias_declaration (parser);
      break;
    case RID_AT_CLASS:
      cp_parser_objc_class_declaration (parser);
      break;
    case RID_AT_PROTOCOL:
      cp_parser_objc_protocol_declaration (parser, attributes);
      break;
    case RID_AT_INTERFACE:
      cp_parser_objc_class_interface (parser, attributes);
      break;
    case RID_AT_IMPLEMENTATION:
      cp_parser_objc_class_implementation (parser);
      break;
    case RID_AT_END:
      cp_parser_objc_end_implementation (parser);
      break;
    default:
      error_at (kwd->location, "misplaced %<@%D%> Objective-C++ construct",
		kwd->u.value);
      cp_parser_skip_to_end_of_block_or_statement (parser);
    }
}

/* Parse an Objective-C try-catch-finally statement.

   objc-try-catch-finally-stmt:
     @try compound-statement objc-catch-clause-seq [opt]
       objc-finally-clause [opt]

   objc-catch-clause-seq:
     objc-catch-clause objc-catch-clause-seq [opt]

   objc-catch-clause:
     @catch ( objc-exception-declaration ) compound-statement

   objc-finally-clause:
     @finally compound-statement

   objc-exception-declaration:
     parameter-declaration
     '...'

   where '...' is to be interpreted literally, that is, it means CPP_ELLIPSIS.

   Returns NULL_TREE.

   PS: This function is identical to c_parser_objc_try_catch_finally_statement
   for C.  Keep them in sync.  */   

static tree
cp_parser_objc_try_catch_finally_statement (cp_parser *parser)
{
  location_t location;
  tree stmt;

  cp_parser_require_keyword (parser, RID_AT_TRY, RT_AT_TRY);
  location = cp_lexer_peek_token (parser->lexer)->location;
  objc_maybe_warn_exceptions (location);
  /* NB: The @try block needs to be wrapped in its own STATEMENT_LIST
     node, lest it get absorbed into the surrounding block.  */
  stmt = push_stmt_list ();
  cp_parser_compound_statement (parser, NULL, false);
  objc_begin_try_stmt (location, pop_stmt_list (stmt));

  while (cp_lexer_next_token_is_keyword (parser->lexer, RID_AT_CATCH))
    {
      cp_parameter_declarator *parm;
      tree parameter_declaration = error_mark_node;
      bool seen_open_paren = false;

      cp_lexer_consume_token (parser->lexer);
      if (cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN))
	seen_open_paren = true;
      if (cp_lexer_next_token_is (parser->lexer, CPP_ELLIPSIS))
	{
	  /* We have "@catch (...)" (where the '...' are literally
	     what is in the code).  Skip the '...'.
	     parameter_declaration is set to NULL_TREE, and
	     objc_being_catch_clauses() knows that that means
	     '...'.  */
	  cp_lexer_consume_token (parser->lexer);
	  parameter_declaration = NULL_TREE;
	}
      else
	{
	  /* We have "@catch (NSException *exception)" or something
	     like that.  Parse the parameter declaration.  */
	  parm = cp_parser_parameter_declaration (parser, false, NULL);
	  if (parm == NULL)
	    parameter_declaration = error_mark_node;
	  else
	    parameter_declaration = grokdeclarator (parm->declarator,
						    &parm->decl_specifiers,
						    PARM, /*initialized=*/0,
						    /*attrlist=*/NULL);
	}
      if (seen_open_paren)
	cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN);
      else
	{
	  /* If there was no open parenthesis, we are recovering from
	     an error, and we are trying to figure out what mistake
	     the user has made.  */

	  /* If there is an immediate closing parenthesis, the user
	     probably forgot the opening one (ie, they typed "@catch
	     NSException *e)".  Parse the closing parenthesis and keep
	     going.  */
	  if (cp_lexer_next_token_is (parser->lexer, CPP_CLOSE_PAREN))
	    cp_lexer_consume_token (parser->lexer);
	  
	  /* If these is no immediate closing parenthesis, the user
	     probably doesn't know that parenthesis are required at
	     all (ie, they typed "@catch NSException *e").  So, just
	     forget about the closing parenthesis and keep going.  */
	}
      objc_begin_catch_clause (parameter_declaration);
      cp_parser_compound_statement (parser, NULL, false);
      objc_finish_catch_clause ();
    }
  if (cp_lexer_next_token_is_keyword (parser->lexer, RID_AT_FINALLY))
    {
      cp_lexer_consume_token (parser->lexer);
      location = cp_lexer_peek_token (parser->lexer)->location;
      /* NB: The @finally block needs to be wrapped in its own STATEMENT_LIST
	 node, lest it get absorbed into the surrounding block.  */
      stmt = push_stmt_list ();
      cp_parser_compound_statement (parser, NULL, false);
      objc_build_finally_clause (location, pop_stmt_list (stmt));
    }

  return objc_finish_try_stmt ();
}

/* Parse an Objective-C synchronized statement.

   objc-synchronized-stmt:
     @synchronized ( expression ) compound-statement

   Returns NULL_TREE.  */

static tree
cp_parser_objc_synchronized_statement (cp_parser *parser)
{
  location_t location;
  tree lock, stmt;

  cp_parser_require_keyword (parser, RID_AT_SYNCHRONIZED, RT_AT_SYNCHRONIZED);

  location = cp_lexer_peek_token (parser->lexer)->location;
  objc_maybe_warn_exceptions (location);
  cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN);
  lock = cp_parser_expression (parser, false, NULL);
  cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN);

  /* NB: The @synchronized block needs to be wrapped in its own STATEMENT_LIST
     node, lest it get absorbed into the surrounding block.  */
  stmt = push_stmt_list ();
  cp_parser_compound_statement (parser, NULL, false);

  return objc_build_synchronized (location, lock, pop_stmt_list (stmt));
}

/* Parse an Objective-C throw statement.

   objc-throw-stmt:
     @throw assignment-expression [opt] ;

   Returns a constructed '@throw' statement.  */

static tree
cp_parser_objc_throw_statement (cp_parser *parser)
{
  tree expr = NULL_TREE;
  location_t loc = cp_lexer_peek_token (parser->lexer)->location;

  cp_parser_require_keyword (parser, RID_AT_THROW, RT_AT_THROW);

  if (cp_lexer_next_token_is_not (parser->lexer, CPP_SEMICOLON))
    expr = cp_parser_expression (parser, /*cast_p=*/false, NULL);

  cp_parser_consume_semicolon_at_end_of_statement (parser);

  return objc_build_throw_stmt (loc, expr);
}

/* Parse an Objective-C statement.  */

static tree
cp_parser_objc_statement (cp_parser * parser)
{
  /* Try to figure out what kind of declaration is present.  */
  cp_token *kwd = cp_lexer_peek_token (parser->lexer);

  switch (kwd->keyword)
    {
    case RID_AT_TRY:
      return cp_parser_objc_try_catch_finally_statement (parser);
    case RID_AT_SYNCHRONIZED:
      return cp_parser_objc_synchronized_statement (parser);
    case RID_AT_THROW:
      return cp_parser_objc_throw_statement (parser);
    default:
      error_at (kwd->location, "misplaced %<@%D%> Objective-C++ construct",
	       kwd->u.value);
      cp_parser_skip_to_end_of_block_or_statement (parser);
    }

  return error_mark_node;
}

/* If we are compiling ObjC++ and we see an __attribute__ we neeed to 
   look ahead to see if an objc keyword follows the attributes.  This
   is to detect the use of prefix attributes on ObjC @interface and 
   @protocol.  */

static bool
cp_parser_objc_valid_prefix_attributes (cp_parser* parser, tree *attrib)
{
  cp_lexer_save_tokens (parser->lexer);
  *attrib = cp_parser_attributes_opt (parser);
  gcc_assert (*attrib);
  if (OBJC_IS_AT_KEYWORD (cp_lexer_peek_token (parser->lexer)->keyword))
    {
      cp_lexer_commit_tokens (parser->lexer);
      return true;
    }
  cp_lexer_rollback_tokens (parser->lexer);
  return false;  
}

/* This routine is a minimal replacement for
   c_parser_struct_declaration () used when parsing the list of
   types/names or ObjC++ properties.  For example, when parsing the
   code

   @property (readonly) int a, b, c;

   this function is responsible for parsing "int a, int b, int c" and
   returning the declarations as CHAIN of DECLs.

   TODO: Share this code with cp_parser_objc_class_ivars.  It's very
   similar parsing.  */
static tree
cp_parser_objc_struct_declaration (cp_parser *parser)
{
  tree decls = NULL_TREE;
  cp_decl_specifier_seq declspecs;
  int decl_class_or_enum_p;
  tree prefix_attributes;

  cp_parser_decl_specifier_seq (parser,
				CP_PARSER_FLAGS_NONE,
				&declspecs,
				&decl_class_or_enum_p);

  if (declspecs.type == error_mark_node)
    return error_mark_node;

  /* auto, register, static, extern, mutable.  */
  if (declspecs.storage_class != sc_none)
    {
      cp_parser_error (parser, "invalid type for property");
      declspecs.storage_class = sc_none;
    }
  
  /* __thread.  */
  if (declspecs.specs[(int) ds_thread])
    {
      cp_parser_error (parser, "invalid type for property");
      declspecs.specs[(int) ds_thread] = 0;
    }
  
  /* typedef.  */
  if (declspecs.specs[(int) ds_typedef])
    {
      cp_parser_error (parser, "invalid type for property");
      declspecs.specs[(int) ds_typedef] = 0;
    }

  prefix_attributes = declspecs.attributes;
  declspecs.attributes = NULL_TREE;

  /* Keep going until we hit the `;' at the end of the declaration. */
  while (cp_lexer_next_token_is_not (parser->lexer, CPP_SEMICOLON))
    {
      tree attributes, first_attribute, decl;
      cp_declarator *declarator;
      cp_token *token;

      /* Parse the declarator.  */
      declarator = cp_parser_declarator (parser, CP_PARSER_DECLARATOR_NAMED,
					 NULL, NULL, false);

      /* Look for attributes that apply to the ivar.  */
      attributes = cp_parser_attributes_opt (parser);
      /* Remember which attributes are prefix attributes and
	 which are not.  */
      first_attribute = attributes;
      /* Combine the attributes.  */
      attributes = chainon (prefix_attributes, attributes);
      
      decl = grokfield (declarator, &declspecs,
			NULL_TREE, /*init_const_expr_p=*/false,
			NULL_TREE, attributes);

      if (decl == error_mark_node || decl == NULL_TREE)
	return error_mark_node;
      
      /* Reset PREFIX_ATTRIBUTES.  */
      while (attributes && TREE_CHAIN (attributes) != first_attribute)
	attributes = TREE_CHAIN (attributes);
      if (attributes)
	TREE_CHAIN (attributes) = NULL_TREE;

      DECL_CHAIN (decl) = decls;
      decls = decl;

      token = cp_lexer_peek_token (parser->lexer);
      if (token->type == CPP_COMMA)
	{
	  cp_lexer_consume_token (parser->lexer);  /* Eat ','.  */
	  continue;
	}
      else
	break;
    }
  return decls;
}

/* Parse an Objective-C @property declaration.  The syntax is:

   objc-property-declaration:
     '@property' objc-property-attributes[opt] struct-declaration ;

   objc-property-attributes:
    '(' objc-property-attribute-list ')'

   objc-property-attribute-list:
     objc-property-attribute
     objc-property-attribute-list, objc-property-attribute

   objc-property-attribute
     'getter' = identifier
     'setter' = identifier
     'readonly'
     'readwrite'
     'assign'
     'retain'
     'copy'
     'nonatomic'

  For example:
    @property NSString *name;
    @property (readonly) id object;
    @property (retain, nonatomic, getter=getTheName) id name;
    @property int a, b, c;

   PS: This function is identical to
   c_parser_objc_at_property_declaration for C.  Keep them in sync.  */
static void 
cp_parser_objc_at_property_declaration (cp_parser *parser)
{
  /* The following variables hold the attributes of the properties as
     parsed.  They are 'false' or 'NULL_TREE' if the attribute was not
     seen.  When we see an attribute, we set them to 'true' (if they
     are boolean properties) or to the identifier (if they have an
     argument, ie, for getter and setter).  Note that here we only
     parse the list of attributes, check the syntax and accumulate the
     attributes that we find.  objc_add_property_declaration() will
     then process the information.  */
  bool property_assign = false;
  bool property_copy = false;
  tree property_getter_ident = NULL_TREE;
  bool property_nonatomic = false;
  bool property_readonly = false;
  bool property_readwrite = false;
  bool property_retain = false;
  tree property_setter_ident = NULL_TREE;

  /* 'properties' is the list of properties that we read.  Usually a
     single one, but maybe more (eg, in "@property int a, b, c;" there
     are three).  */
  tree properties;
  location_t loc;

  loc = cp_lexer_peek_token (parser->lexer)->location;

  cp_lexer_consume_token (parser->lexer);  /* Eat '@property'.  */

  /* Parse the optional attribute list...  */
  if (cp_lexer_next_token_is (parser->lexer, CPP_OPEN_PAREN))
    {
      /* Eat the '('.  */
      cp_lexer_consume_token (parser->lexer);

      while (true)
	{
	  bool syntax_error = false;
	  cp_token *token = cp_lexer_peek_token (parser->lexer);
      	  enum rid keyword;

	  if (token->type != CPP_NAME)
	    {
	      cp_parser_error (parser, "expected identifier");
	      break;
	    }
	  keyword = C_RID_CODE (token->u.value);
	  cp_lexer_consume_token (parser->lexer);
	  switch (keyword)
	    {
	    case RID_ASSIGN:    property_assign = true;    break;
	    case RID_COPY:      property_copy = true;      break;
	    case RID_NONATOMIC: property_nonatomic = true; break;
	    case RID_READONLY:  property_readonly = true;  break;
	    case RID_READWRITE: property_readwrite = true; break;
	    case RID_RETAIN:    property_retain = true;    break;

	    case RID_GETTER:
	    case RID_SETTER:
	      if (cp_lexer_next_token_is_not (parser->lexer, CPP_EQ))
		{
		  if (keyword == RID_GETTER)
		    cp_parser_error (parser,
				     "missing %<=%> (after %<getter%> attribute)");
		  else
		    cp_parser_error (parser,
				     "missing %<=%> (after %<setter%> attribute)");
		  syntax_error = true;
		  break;
		}
	      cp_lexer_consume_token (parser->lexer); /* eat the = */
	      if (cp_lexer_next_token_is_not (parser->lexer, CPP_NAME))
		{
		  cp_parser_error (parser, "expected identifier");
		  syntax_error = true;
		  break;
		}
	      if (keyword == RID_SETTER)
		{
		  if (property_setter_ident != NULL_TREE)
		    cp_parser_error (parser, "the %<setter%> attribute may only be specified once");
		  else
		    property_setter_ident = cp_lexer_peek_token (parser->lexer)->u.value;
		  cp_lexer_consume_token (parser->lexer);
		  if (cp_lexer_next_token_is_not (parser->lexer, CPP_COLON))
		    cp_parser_error (parser, "setter name must terminate with %<:%>");
		  else
		    cp_lexer_consume_token (parser->lexer);
		}
	      else
		{
		  if (property_getter_ident != NULL_TREE)
		    cp_parser_error (parser, "the %<getter%> attribute may only be specified once");
		  else
		    property_getter_ident = cp_lexer_peek_token (parser->lexer)->u.value;
		  cp_lexer_consume_token (parser->lexer);
		}
	      break;
	    default:
	      cp_parser_error (parser, "unknown property attribute");
	      syntax_error = true;
	      break;
	    }

	  if (syntax_error)
	    break;

	  if (cp_lexer_next_token_is (parser->lexer, CPP_COMMA))
	    cp_lexer_consume_token (parser->lexer);
	  else
	    break;
	}

      /* FIXME: "@property (setter, assign);" will generate a spurious
	 "error: expected ‘)’ before ‘,’ token".  This is because
	 cp_parser_require, unlike the C counterpart, will produce an
	 error even if we are in error recovery.  */
      if (!cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN))
	{
	  cp_parser_skip_to_closing_parenthesis (parser,
						 /*recovering=*/true,
						 /*or_comma=*/false,
						 /*consume_paren=*/true);
	}
    }

  /* ... and the property declaration(s).  */
  properties = cp_parser_objc_struct_declaration (parser);

  if (properties == error_mark_node)
    {
      cp_parser_skip_to_end_of_statement (parser);
      /* If the next token is now a `;', consume it.  */
      if (cp_lexer_next_token_is (parser->lexer, CPP_SEMICOLON))
	cp_lexer_consume_token (parser->lexer);
      return;
    }

  if (properties == NULL_TREE)
    cp_parser_error (parser, "expected identifier");
  else
    {
      /* Comma-separated properties are chained together in
	 reverse order; add them one by one.  */
      properties = nreverse (properties);
      
      for (; properties; properties = TREE_CHAIN (properties))
	objc_add_property_declaration (loc, copy_node (properties),
				       property_readonly, property_readwrite,
				       property_assign, property_retain,
				       property_copy, property_nonatomic,
				       property_getter_ident, property_setter_ident);
    }
  
  cp_parser_consume_semicolon_at_end_of_statement (parser);
}

/* Parse an Objective-C++ @synthesize declaration.  The syntax is:

   objc-synthesize-declaration:
     @synthesize objc-synthesize-identifier-list ;

   objc-synthesize-identifier-list:
     objc-synthesize-identifier
     objc-synthesize-identifier-list, objc-synthesize-identifier

   objc-synthesize-identifier
     identifier
     identifier = identifier

  For example:
    @synthesize MyProperty;
    @synthesize OneProperty, AnotherProperty=MyIvar, YetAnotherProperty;

  PS: This function is identical to c_parser_objc_at_synthesize_declaration
  for C.  Keep them in sync.
*/
static void 
cp_parser_objc_at_synthesize_declaration (cp_parser *parser)
{
  tree list = NULL_TREE;
  location_t loc;
  loc = cp_lexer_peek_token (parser->lexer)->location;

  cp_lexer_consume_token (parser->lexer);  /* Eat '@synthesize'.  */
  while (true)
    {
      tree property, ivar;
      property = cp_parser_identifier (parser);
      if (property == error_mark_node)
	{
	  cp_parser_consume_semicolon_at_end_of_statement (parser);
	  return;
	}
      if (cp_lexer_next_token_is (parser->lexer, CPP_EQ))
	{
	  cp_lexer_consume_token (parser->lexer);
	  ivar = cp_parser_identifier (parser);
	  if (ivar == error_mark_node)
	    {
	      cp_parser_consume_semicolon_at_end_of_statement (parser);
	      return;
	    }
	}
      else
	ivar = NULL_TREE;
      list = chainon (list, build_tree_list (ivar, property));
      if (cp_lexer_next_token_is (parser->lexer, CPP_COMMA))
	cp_lexer_consume_token (parser->lexer);
      else
	break;
    }
  cp_parser_consume_semicolon_at_end_of_statement (parser);
  objc_add_synthesize_declaration (loc, list);
}

/* Parse an Objective-C++ @dynamic declaration.  The syntax is:

   objc-dynamic-declaration:
     @dynamic identifier-list ;

   For example:
     @dynamic MyProperty;
     @dynamic MyProperty, AnotherProperty;

  PS: This function is identical to c_parser_objc_at_dynamic_declaration
  for C.  Keep them in sync.
*/
static void 
cp_parser_objc_at_dynamic_declaration (cp_parser *parser)
{
  tree list = NULL_TREE;
  location_t loc;
  loc = cp_lexer_peek_token (parser->lexer)->location;

  cp_lexer_consume_token (parser->lexer);  /* Eat '@dynamic'.  */
  while (true)
    {
      tree property;
      property = cp_parser_identifier (parser);
      if (property == error_mark_node)
	{
	  cp_parser_consume_semicolon_at_end_of_statement (parser);
	  return;
	}
      list = chainon (list, build_tree_list (NULL, property));
      if (cp_lexer_next_token_is (parser->lexer, CPP_COMMA))
	cp_lexer_consume_token (parser->lexer);
      else
	break;
    }
  cp_parser_consume_semicolon_at_end_of_statement (parser);
  objc_add_dynamic_declaration (loc, list);
}

#endif
