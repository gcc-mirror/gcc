/* Parser for C and Objective-C.
   Copyright (C) 1987-2021 Free Software Foundation, Inc.

   Parser actions based on the old Bison parser; structure somewhat
   influenced by and fragments based on the C++ parser.

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

/* TODO:

   Make sure all relevant comments, and all relevant code from all
   actions, brought over from old parser.  Verify exact correspondence
   of syntax accepted.

   Add testcases covering every input symbol in every state in old and
   new parsers.

   Include full syntax for GNU C, including erroneous cases accepted
   with error messages, in syntax productions in comments.

   Make more diagnostics in the front end generally take an explicit
   location rather than implicitly using input_location.  */

#include "config.h"
#define INCLUDE_UNIQUE_PTR
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
#include "omp-general.h"
#include "omp-offload.h"
#include "builtins.h"
#include "gomp-constants.h"
#include "c-family/c-indentation.h"
#include "gimple-expr.h"
#include "context.h"
#include "gcc-rich-location.h"
#include "c-parser.h"
#include "gimple-parser.h"
#include "read-rtl-function.h"
#include "run-rtl-passes.h"
#include "intl.h"
#include "c-family/name-hint.h"
#include "tree-iterator.h"
#include "tree-pretty-print.h"
#include "memmodel.h"
#include "c-family/known-headers.h"

/* We need to walk over decls with incomplete struct/union/enum types
   after parsing the whole translation unit.
   In finish_decl(), if the decl is static, has incomplete
   struct/union/enum type, it is appended to incomplete_record_decls.
   In c_parser_translation_unit(), we iterate over incomplete_record_decls
   and report error if any of the decls are still incomplete.  */ 

vec<tree> incomplete_record_decls;

void
set_c_expr_source_range (c_expr *expr,
			 location_t start, location_t finish)
{
  expr->src_range.m_start = start;
  expr->src_range.m_finish = finish;
  if (expr->value)
    set_source_range (expr->value, start, finish);
}

void
set_c_expr_source_range (c_expr *expr,
			 source_range src_range)
{
  expr->src_range = src_range;
  if (expr->value)
    set_source_range (expr->value, src_range);
}


/* Initialization routine for this file.  */

void
c_parse_init (void)
{
  /* The only initialization required is of the reserved word
     identifiers.  */
  unsigned int i;
  tree id;
  int mask = 0;

  /* Make sure RID_MAX hasn't grown past the 8 bits used to hold the keyword in
     the c_token structure.  */
  gcc_assert (RID_MAX <= 255);

  mask |= D_CXXONLY;
  if (!flag_isoc99)
    mask |= D_C99;
  if (flag_no_asm)
    {
      mask |= D_ASM | D_EXT;
      if (!flag_isoc99)
	mask |= D_EXT89;
    }
  if (!c_dialect_objc ())
    mask |= D_OBJC | D_CXX_OBJC;

  ridpointers = ggc_cleared_vec_alloc<tree> ((int) RID_MAX);
  for (i = 0; i < num_c_common_reswords; i++)
    {
      /* If a keyword is disabled, do not enter it into the table
	 and so create a canonical spelling that isn't a keyword.  */
      if (c_common_reswords[i].disable & mask)
	{
	  if (warn_cxx_compat
	      && (c_common_reswords[i].disable & D_CXXWARN))
	    {
	      id = get_identifier (c_common_reswords[i].word);
	      C_SET_RID_CODE (id, RID_CXX_COMPAT_WARN);
	      C_IS_RESERVED_WORD (id) = 1;
	    }
	  continue;
	}

      id = get_identifier (c_common_reswords[i].word);
      C_SET_RID_CODE (id, c_common_reswords[i].rid);
      C_IS_RESERVED_WORD (id) = 1;
      ridpointers [(int) c_common_reswords[i].rid] = id;
    }

  for (i = 0; i < NUM_INT_N_ENTS; i++)
    {
      /* We always create the symbols but they aren't always supported.  */
      char name[50];
      sprintf (name, "__int%d", int_n_data[i].bitsize);
      id = get_identifier (name);
      C_SET_RID_CODE (id, RID_FIRST_INT_N + i);
      C_IS_RESERVED_WORD (id) = 1;

      sprintf (name, "__int%d__", int_n_data[i].bitsize);
      id = get_identifier (name);
      C_SET_RID_CODE (id, RID_FIRST_INT_N + i);
      C_IS_RESERVED_WORD (id) = 1;
    }
}

/* A parser structure recording information about the state and
   context of parsing.  Includes lexer information with up to two
   tokens of look-ahead; more are not needed for C.  */
struct GTY(()) c_parser {
  /* The look-ahead tokens.  */
  c_token * GTY((skip)) tokens;
  /* Buffer for look-ahead tokens.  */
  c_token tokens_buf[4];
  /* How many look-ahead tokens are available (0 - 4, or
     more if parsing from pre-lexed tokens).  */
  unsigned int tokens_avail;
  /* Raw look-ahead tokens, used only for checking in Objective-C
     whether '[[' starts attributes.  */
  vec<c_token, va_gc> *raw_tokens;
  /* The number of raw look-ahead tokens that have since been fully
     lexed.  */
  unsigned int raw_tokens_used;
  /* True if a syntax error is being recovered from; false otherwise.
     c_parser_error sets this flag.  It should clear this flag when
     enough tokens have been consumed to recover from the error.  */
  BOOL_BITFIELD error : 1;
  /* True if we're processing a pragma, and shouldn't automatically
     consume CPP_PRAGMA_EOL.  */
  BOOL_BITFIELD in_pragma : 1;
  /* True if we're parsing the outermost block of an if statement.  */
  BOOL_BITFIELD in_if_block : 1;
  /* True if we want to lex a translated, joined string (for an
     initial #pragma pch_preprocess).  Otherwise the parser is
     responsible for concatenating strings and translating to the
     execution character set as needed.  */
  BOOL_BITFIELD lex_joined_string : 1;
  /* True if, when the parser is concatenating string literals, it
     should translate them to the execution character set (false
     inside attributes).  */
  BOOL_BITFIELD translate_strings_p : 1;

  /* Objective-C specific parser/lexer information.  */

  /* True if we are in a context where the Objective-C "PQ" keywords
     are considered keywords.  */
  BOOL_BITFIELD objc_pq_context : 1;
  /* True if we are parsing a (potential) Objective-C foreach
     statement.  This is set to true after we parsed 'for (' and while
     we wait for 'in' or ';' to decide if it's a standard C for loop or an
     Objective-C foreach loop.  */
  BOOL_BITFIELD objc_could_be_foreach_context : 1;
  /* The following flag is needed to contextualize Objective-C lexical
     analysis.  In some cases (e.g., 'int NSObject;'), it is
     undesirable to bind an identifier to an Objective-C class, even
     if a class with that name exists.  */
  BOOL_BITFIELD objc_need_raw_identifier : 1;
  /* Nonzero if we're processing a __transaction statement.  The value
     is 1 | TM_STMT_ATTR_*.  */
  unsigned int in_transaction : 4;
  /* True if we are in a context where the Objective-C "Property attribute"
     keywords are valid.  */
  BOOL_BITFIELD objc_property_attr_context : 1;

  /* Whether we have just seen/constructed a string-literal.  Set when
     returning a string-literal from c_parser_string_literal.  Reset
     in consume_token.  Useful when we get a parse error and see an
     unknown token, which could have been a string-literal constant
     macro.  */
  BOOL_BITFIELD seen_string_literal : 1;

  /* Location of the last consumed token.  */
  location_t last_token_location;
};

/* Return a pointer to the Nth token in PARSERs tokens_buf.  */

c_token *
c_parser_tokens_buf (c_parser *parser, unsigned n)
{
  return &parser->tokens_buf[n];
}

/* Return the error state of PARSER.  */

bool
c_parser_error (c_parser *parser)
{
  return parser->error;
}

/* Set the error state of PARSER to ERR.  */

void
c_parser_set_error (c_parser *parser, bool err)
{
  parser->error = err;
}


/* The actual parser and external interface.  ??? Does this need to be
   garbage-collected?  */

static GTY (()) c_parser *the_parser;

/* Read in and lex a single token, storing it in *TOKEN.  If RAW,
   context-sensitive postprocessing of the token is not done.  */

static void
c_lex_one_token (c_parser *parser, c_token *token, bool raw = false)
{
  timevar_push (TV_LEX);

  if (raw || vec_safe_length (parser->raw_tokens) == 0)
    {
      token->type = c_lex_with_flags (&token->value, &token->location,
				      &token->flags,
				      (parser->lex_joined_string
				       ? 0 : C_LEX_STRING_NO_JOIN));
      token->id_kind = C_ID_NONE;
      token->keyword = RID_MAX;
      token->pragma_kind = PRAGMA_NONE;
    }
  else
    {
      /* Use a token previously lexed as a raw look-ahead token, and
	 complete the processing on it.  */
      *token = (*parser->raw_tokens)[parser->raw_tokens_used];
      ++parser->raw_tokens_used;
      if (parser->raw_tokens_used == vec_safe_length (parser->raw_tokens))
	{
	  vec_free (parser->raw_tokens);
	  parser->raw_tokens_used = 0;
	}
    }

  if (raw)
    goto out;

  switch (token->type)
    {
    case CPP_NAME:
      {
	tree decl;

	bool objc_force_identifier = parser->objc_need_raw_identifier;
	if (c_dialect_objc ())
	  parser->objc_need_raw_identifier = false;

	if (C_IS_RESERVED_WORD (token->value))
	  {
	    enum rid rid_code = C_RID_CODE (token->value);

	    if (rid_code == RID_CXX_COMPAT_WARN)
	      {
		warning_at (token->location,
			    OPT_Wc___compat,
			    "identifier %qE conflicts with C++ keyword",
			    token->value);
	      }
	    else if (rid_code >= RID_FIRST_ADDR_SPACE
		     && rid_code <= RID_LAST_ADDR_SPACE)
	      {
		addr_space_t as;
		as = (addr_space_t) (rid_code - RID_FIRST_ADDR_SPACE);
		targetm.addr_space.diagnose_usage (as, token->location);
		token->id_kind = C_ID_ADDRSPACE;
		token->keyword = rid_code;
		break;
	      }
	    else if (c_dialect_objc () && OBJC_IS_PQ_KEYWORD (rid_code))
	      {
		/* We found an Objective-C "pq" keyword (in, out,
		   inout, bycopy, byref, oneway).  They need special
		   care because the interpretation depends on the
		   context.  */
		if (parser->objc_pq_context)
		  {
		    token->type = CPP_KEYWORD;
		    token->keyword = rid_code;
		    break;
		  }
		else if (parser->objc_could_be_foreach_context
			 && rid_code == RID_IN)
		  {
		    /* We are in Objective-C, inside a (potential)
		       foreach context (which means after having
		       parsed 'for (', but before having parsed ';'),
		       and we found 'in'.  We consider it the keyword
		       which terminates the declaration at the
		       beginning of a foreach-statement.  Note that
		       this means you can't use 'in' for anything else
		       in that context; in particular, in Objective-C
		       you can't use 'in' as the name of the running
		       variable in a C for loop.  We could potentially
		       try to add code here to disambiguate, but it
		       seems a reasonable limitation.  */
		    token->type = CPP_KEYWORD;
		    token->keyword = rid_code;
		    break;
		  }
		/* Else, "pq" keywords outside of the "pq" context are
		   not keywords, and we fall through to the code for
		   normal tokens.  */
	      }
	    else if (c_dialect_objc () && OBJC_IS_PATTR_KEYWORD (rid_code))
	      {
		/* We found an Objective-C "property attribute"
		   keyword (getter, setter, readonly, etc). These are
		   only valid in the property context.  */
		if (parser->objc_property_attr_context)
		  {
		    token->type = CPP_KEYWORD;
		    token->keyword = rid_code;
		    break;
		  }
		/* Else they are not special keywords.
		*/
	      }
	    else if (c_dialect_objc () 
		     && (OBJC_IS_AT_KEYWORD (rid_code)
			 || OBJC_IS_CXX_KEYWORD (rid_code)))
	      {
		/* We found one of the Objective-C "@" keywords (defs,
		   selector, synchronized, etc) or one of the
		   Objective-C "cxx" keywords (class, private,
		   protected, public, try, catch, throw) without a
		   preceding '@' sign.  Do nothing and fall through to
		   the code for normal tokens (in C++ we would still
		   consider the CXX ones keywords, but not in C).  */
		;
	      }
	    else
	      {
		token->type = CPP_KEYWORD;
		token->keyword = rid_code;
		break;
	      }
	  }

	decl = lookup_name (token->value);
	if (decl)
	  {
	    if (TREE_CODE (decl) == TYPE_DECL)
	      {
		token->id_kind = C_ID_TYPENAME;
		break;
	      }
	  }
	else if (c_dialect_objc ())
	  {
	    tree objc_interface_decl = objc_is_class_name (token->value);
	    /* Objective-C class names are in the same namespace as
	       variables and typedefs, and hence are shadowed by local
	       declarations.  */
	    if (objc_interface_decl
                && (!objc_force_identifier || global_bindings_p ()))
	      {
		token->value = objc_interface_decl;
		token->id_kind = C_ID_CLASSNAME;
		break;
	      }
	  }
        token->id_kind = C_ID_ID;
      }
      break;
    case CPP_AT_NAME:
      /* This only happens in Objective-C; it must be a keyword.  */
      token->type = CPP_KEYWORD;
      switch (C_RID_CODE (token->value))
	{
	  /* Replace 'class' with '@class', 'private' with '@private',
	     etc.  This prevents confusion with the C++ keyword
	     'class', and makes the tokens consistent with other
	     Objective-C 'AT' keywords.  For example '@class' is
	     reported as RID_AT_CLASS which is consistent with
	     '@synchronized', which is reported as
	     RID_AT_SYNCHRONIZED.
	  */
	case RID_CLASS:     token->keyword = RID_AT_CLASS; break;
	case RID_PRIVATE:   token->keyword = RID_AT_PRIVATE; break;
	case RID_PROTECTED: token->keyword = RID_AT_PROTECTED; break;
	case RID_PUBLIC:    token->keyword = RID_AT_PUBLIC; break;
	case RID_THROW:     token->keyword = RID_AT_THROW; break;
	case RID_TRY:       token->keyword = RID_AT_TRY; break;
	case RID_CATCH:     token->keyword = RID_AT_CATCH; break;
	case RID_SYNCHRONIZED: token->keyword = RID_AT_SYNCHRONIZED; break;
	default:            token->keyword = C_RID_CODE (token->value);
	}
      break;
    case CPP_COLON:
    case CPP_COMMA:
    case CPP_CLOSE_PAREN:
    case CPP_SEMICOLON:
      /* These tokens may affect the interpretation of any identifiers
	 following, if doing Objective-C.  */
      if (c_dialect_objc ())
	parser->objc_need_raw_identifier = false;
      break;
    case CPP_PRAGMA:
      /* We smuggled the cpp_token->u.pragma value in an INTEGER_CST.  */
      token->pragma_kind = (enum pragma_kind) TREE_INT_CST_LOW (token->value);
      token->value = NULL;
      break;
    default:
      break;
    }
 out:
  timevar_pop (TV_LEX);
}

/* Return a pointer to the next token from PARSER, reading it in if
   necessary.  */

c_token *
c_parser_peek_token (c_parser *parser)
{
  if (parser->tokens_avail == 0)
    {
      c_lex_one_token (parser, &parser->tokens[0]);
      parser->tokens_avail = 1;
    }
  return &parser->tokens[0];
}

/* Return a pointer to the next-but-one token from PARSER, reading it
   in if necessary.  The next token is already read in.  */

c_token *
c_parser_peek_2nd_token (c_parser *parser)
{
  if (parser->tokens_avail >= 2)
    return &parser->tokens[1];
  gcc_assert (parser->tokens_avail == 1);
  gcc_assert (parser->tokens[0].type != CPP_EOF);
  gcc_assert (parser->tokens[0].type != CPP_PRAGMA_EOL);
  c_lex_one_token (parser, &parser->tokens[1]);
  parser->tokens_avail = 2;
  return &parser->tokens[1];
}

/* Return a pointer to the Nth token from PARSER, reading it
   in if necessary.  The N-1th token is already read in.  */

c_token *
c_parser_peek_nth_token (c_parser *parser, unsigned int n)
{
  /* N is 1-based, not zero-based.  */
  gcc_assert (n > 0);

  if (parser->tokens_avail >= n)
    return &parser->tokens[n - 1];
  gcc_assert (parser->tokens_avail == n - 1);
  c_lex_one_token (parser, &parser->tokens[n - 1]);
  parser->tokens_avail = n;
  return &parser->tokens[n - 1];
}

/* Return a pointer to the Nth token from PARSER, reading it in as a
   raw look-ahead token if necessary.  The N-1th token is already read
   in.  Raw look-ahead tokens remain available for when the non-raw
   functions above are called.  */

c_token *
c_parser_peek_nth_token_raw (c_parser *parser, unsigned int n)
{
  /* N is 1-based, not zero-based.  */
  gcc_assert (n > 0);

  if (parser->tokens_avail >= n)
    return &parser->tokens[n - 1];
  unsigned int raw_len = vec_safe_length (parser->raw_tokens);
  unsigned int raw_avail
    = parser->tokens_avail + raw_len - parser->raw_tokens_used;
  gcc_assert (raw_avail >= n - 1);
  if (raw_avail >= n)
    return &(*parser->raw_tokens)[parser->raw_tokens_used
				  + n - 1 - parser->tokens_avail];
  vec_safe_reserve (parser->raw_tokens, 1);
  parser->raw_tokens->quick_grow (raw_len + 1);
  c_lex_one_token (parser, &(*parser->raw_tokens)[raw_len], true);
  return &(*parser->raw_tokens)[raw_len];
}

bool
c_keyword_starts_typename (enum rid keyword)
{
  switch (keyword)
    {
    case RID_UNSIGNED:
    case RID_LONG:
    case RID_SHORT:
    case RID_SIGNED:
    case RID_COMPLEX:
    case RID_INT:
    case RID_CHAR:
    case RID_FLOAT:
    case RID_DOUBLE:
    case RID_VOID:
    case RID_DFLOAT32:
    case RID_DFLOAT64:
    case RID_DFLOAT128:
    CASE_RID_FLOATN_NX:
    case RID_BOOL:
    case RID_ENUM:
    case RID_STRUCT:
    case RID_UNION:
    case RID_TYPEOF:
    case RID_CONST:
    case RID_ATOMIC:
    case RID_VOLATILE:
    case RID_RESTRICT:
    case RID_ATTRIBUTE:
    case RID_FRACT:
    case RID_ACCUM:
    case RID_SAT:
    case RID_AUTO_TYPE:
    case RID_ALIGNAS:
      return true;
    default:
      if (keyword >= RID_FIRST_INT_N
	  && keyword < RID_FIRST_INT_N + NUM_INT_N_ENTS
	  && int_n_enabled_p[keyword - RID_FIRST_INT_N])
	return true;
      return false;
    }
}

/* Return true if TOKEN can start a type name,
   false otherwise.  */
bool
c_token_starts_typename (c_token *token)
{
  switch (token->type)
    {
    case CPP_NAME:
      switch (token->id_kind)
	{
	case C_ID_ID:
	  return false;
	case C_ID_ADDRSPACE:
	  return true;
	case C_ID_TYPENAME:
	  return true;
	case C_ID_CLASSNAME:
	  gcc_assert (c_dialect_objc ());
	  return true;
	default:
	  gcc_unreachable ();
	}
    case CPP_KEYWORD:
      return c_keyword_starts_typename (token->keyword);
    case CPP_LESS:
      if (c_dialect_objc ())
	return true;
      return false;
    default:
      return false;
    }
}

/* Return true if the next token from PARSER can start a type name,
   false otherwise.  LA specifies how to do lookahead in order to
   detect unknown type names.  If unsure, pick CLA_PREFER_ID.  */

static inline bool
c_parser_next_tokens_start_typename (c_parser *parser, enum c_lookahead_kind la)
{
  c_token *token = c_parser_peek_token (parser);
  if (c_token_starts_typename (token))
    return true;

  /* Try a bit harder to detect an unknown typename.  */
  if (la != cla_prefer_id
      && token->type == CPP_NAME
      && token->id_kind == C_ID_ID

      /* Do not try too hard when we could have "object in array".  */
      && !parser->objc_could_be_foreach_context

      && (la == cla_prefer_type
	  || c_parser_peek_2nd_token (parser)->type == CPP_NAME
	  || c_parser_peek_2nd_token (parser)->type == CPP_MULT)

      /* Only unknown identifiers.  */
      && !lookup_name (token->value))
    return true;

  return false;
}

/* Return true if TOKEN is a type qualifier, false otherwise.  */
static bool
c_token_is_qualifier (c_token *token)
{
  switch (token->type)
    {
    case CPP_NAME:
      switch (token->id_kind)
	{
	case C_ID_ADDRSPACE:
	  return true;
	default:
	  return false;
	}
    case CPP_KEYWORD:
      switch (token->keyword)
	{
	case RID_CONST:
	case RID_VOLATILE:
	case RID_RESTRICT:
	case RID_ATTRIBUTE:
	case RID_ATOMIC:
	  return true;
	default:
	  return false;
	}
    case CPP_LESS:
      return false;
    default:
      gcc_unreachable ();
    }
}

/* Return true if the next token from PARSER is a type qualifier,
   false otherwise.  */
static inline bool
c_parser_next_token_is_qualifier (c_parser *parser)
{
  c_token *token = c_parser_peek_token (parser);
  return c_token_is_qualifier (token);
}

/* Return true if TOKEN can start declaration specifiers (not
   including standard attributes), false otherwise.  */
static bool
c_token_starts_declspecs (c_token *token)
{
  switch (token->type)
    {
    case CPP_NAME:
      switch (token->id_kind)
	{
	case C_ID_ID:
	  return false;
	case C_ID_ADDRSPACE:
	  return true;
	case C_ID_TYPENAME:
	  return true;
	case C_ID_CLASSNAME:
	  gcc_assert (c_dialect_objc ());
	  return true;
	default:
	  gcc_unreachable ();
	}
    case CPP_KEYWORD:
      switch (token->keyword)
	{
	case RID_STATIC:
	case RID_EXTERN:
	case RID_REGISTER:
	case RID_TYPEDEF:
	case RID_INLINE:
	case RID_NORETURN:
	case RID_AUTO:
	case RID_THREAD:
	case RID_UNSIGNED:
	case RID_LONG:
	case RID_SHORT:
	case RID_SIGNED:
	case RID_COMPLEX:
	case RID_INT:
	case RID_CHAR:
	case RID_FLOAT:
	case RID_DOUBLE:
	case RID_VOID:
	case RID_DFLOAT32:
	case RID_DFLOAT64:
	case RID_DFLOAT128:
	CASE_RID_FLOATN_NX:
	case RID_BOOL:
	case RID_ENUM:
	case RID_STRUCT:
	case RID_UNION:
	case RID_TYPEOF:
	case RID_CONST:
	case RID_VOLATILE:
	case RID_RESTRICT:
	case RID_ATTRIBUTE:
	case RID_FRACT:
	case RID_ACCUM:
	case RID_SAT:
	case RID_ALIGNAS:
	case RID_ATOMIC:
	case RID_AUTO_TYPE:
	  return true;
	default:
	  if (token->keyword >= RID_FIRST_INT_N
	      && token->keyword < RID_FIRST_INT_N + NUM_INT_N_ENTS
	      && int_n_enabled_p[token->keyword - RID_FIRST_INT_N])
	    return true;
	  return false;
	}
    case CPP_LESS:
      if (c_dialect_objc ())
	return true;
      return false;
    default:
      return false;
    }
}


/* Return true if TOKEN can start declaration specifiers (not
   including standard attributes) or a static assertion, false
   otherwise.  */
static bool
c_token_starts_declaration (c_token *token)
{
  if (c_token_starts_declspecs (token)
      || token->keyword == RID_STATIC_ASSERT)
    return true;
  else
    return false;
}

/* Return true if the next token from PARSER can start declaration
   specifiers (not including standard attributes), false
   otherwise.  */
bool
c_parser_next_token_starts_declspecs (c_parser *parser)
{
  c_token *token = c_parser_peek_token (parser);

  /* In Objective-C, a classname normally starts a declspecs unless it
     is immediately followed by a dot.  In that case, it is the
     Objective-C 2.0 "dot-syntax" for class objects, ie, calls the
     setter/getter on the class.  c_token_starts_declspecs() can't
     differentiate between the two cases because it only checks the
     current token, so we have a special check here.  */
  if (c_dialect_objc () 
      && token->type == CPP_NAME
      && token->id_kind == C_ID_CLASSNAME 
      && c_parser_peek_2nd_token (parser)->type == CPP_DOT)
    return false;

  return c_token_starts_declspecs (token);
}

/* Return true if the next tokens from PARSER can start declaration
   specifiers (not including standard attributes) or a static
   assertion, false otherwise.  */
bool
c_parser_next_tokens_start_declaration (c_parser *parser)
{
  c_token *token = c_parser_peek_token (parser);

  /* Same as above.  */
  if (c_dialect_objc () 
      && token->type == CPP_NAME
      && token->id_kind == C_ID_CLASSNAME 
      && c_parser_peek_2nd_token (parser)->type == CPP_DOT)
    return false;

  /* Labels do not start declarations.  */
  if (token->type == CPP_NAME
      && c_parser_peek_2nd_token (parser)->type == CPP_COLON)
    return false;

  if (c_token_starts_declaration (token))
    return true;

  if (c_parser_next_tokens_start_typename (parser, cla_nonabstract_decl))
    return true;

  return false;
}

/* Consume the next token from PARSER.  */

void
c_parser_consume_token (c_parser *parser)
{
  gcc_assert (parser->tokens_avail >= 1);
  gcc_assert (parser->tokens[0].type != CPP_EOF);
  gcc_assert (!parser->in_pragma || parser->tokens[0].type != CPP_PRAGMA_EOL);
  gcc_assert (parser->error || parser->tokens[0].type != CPP_PRAGMA);
  parser->last_token_location = parser->tokens[0].location;
  if (parser->tokens != &parser->tokens_buf[0])
    parser->tokens++;
  else if (parser->tokens_avail >= 2)
    {
      parser->tokens[0] = parser->tokens[1];
      if (parser->tokens_avail >= 3)
        {
          parser->tokens[1] = parser->tokens[2];
          if (parser->tokens_avail >= 4)
            parser->tokens[2] = parser->tokens[3];
        }
    }
  parser->tokens_avail--;
  parser->seen_string_literal = false;
}

/* Expect the current token to be a #pragma.  Consume it and remember
   that we've begun parsing a pragma.  */

static void
c_parser_consume_pragma (c_parser *parser)
{
  gcc_assert (!parser->in_pragma);
  gcc_assert (parser->tokens_avail >= 1);
  gcc_assert (parser->tokens[0].type == CPP_PRAGMA);
  if (parser->tokens != &parser->tokens_buf[0])
    parser->tokens++;
  else if (parser->tokens_avail >= 2)
    {
      parser->tokens[0] = parser->tokens[1];
      if (parser->tokens_avail >= 3)
	parser->tokens[1] = parser->tokens[2];
    }
  parser->tokens_avail--;
  parser->in_pragma = true;
}

/* Update the global input_location from TOKEN.  */
static inline void
c_parser_set_source_position_from_token (c_token *token)
{
  if (token->type != CPP_EOF)
    {
      input_location = token->location;
    }
}

/* Helper function for c_parser_error.
   Having peeked a token of kind TOK1_KIND that might signify
   a conflict marker, peek successor tokens to determine
   if we actually do have a conflict marker.
   Specifically, we consider a run of 7 '<', '=' or '>' characters
   at the start of a line as a conflict marker.
   These come through the lexer as three pairs and a single,
   e.g. three CPP_LSHIFT ("<<") and a CPP_LESS ('<').
   If it returns true, *OUT_LOC is written to with the location/range
   of the marker.  */

static bool
c_parser_peek_conflict_marker (c_parser *parser, enum cpp_ttype tok1_kind,
			       location_t *out_loc)
{
  c_token *token2 = c_parser_peek_2nd_token (parser);
  if (token2->type != tok1_kind)
    return false;
  c_token *token3 = c_parser_peek_nth_token (parser, 3);
  if (token3->type != tok1_kind)
    return false;
  c_token *token4 = c_parser_peek_nth_token (parser, 4);
  if (token4->type != conflict_marker_get_final_tok_kind (tok1_kind))
    return false;

  /* It must be at the start of the line.  */
  location_t start_loc = c_parser_peek_token (parser)->location;
  if (LOCATION_COLUMN (start_loc) != 1)
    return false;

  /* We have a conflict marker.  Construct a location of the form:
       <<<<<<<
       ^~~~~~~
     with start == caret, finishing at the end of the marker.  */
  location_t finish_loc = get_finish (token4->location);
  *out_loc = make_location (start_loc, start_loc, finish_loc);

  return true;
}

/* Issue a diagnostic of the form
      FILE:LINE: MESSAGE before TOKEN
   where TOKEN is the next token in the input stream of PARSER.
   MESSAGE (specified by the caller) is usually of the form "expected
   OTHER-TOKEN".

   Use RICHLOC as the location of the diagnostic.

   Do not issue a diagnostic if still recovering from an error.

   Return true iff an error was actually emitted.

   ??? This is taken from the C++ parser, but building up messages in
   this way is not i18n-friendly and some other approach should be
   used.  */

static bool
c_parser_error_richloc (c_parser *parser, const char *gmsgid,
			rich_location *richloc)
{
  c_token *token = c_parser_peek_token (parser);
  if (parser->error)
    return false;
  parser->error = true;
  if (!gmsgid)
    return false;

  /* If this is actually a conflict marker, report it as such.  */
  if (token->type == CPP_LSHIFT
      || token->type == CPP_RSHIFT
      || token->type == CPP_EQ_EQ)
    {
      location_t loc;
      if (c_parser_peek_conflict_marker (parser, token->type, &loc))
	{
	  error_at (loc, "version control conflict marker in file");
	  return true;
	}
    }

  /* If we were parsing a string-literal and there is an unknown name
     token right after, then check to see if that could also have been
     a literal string by checking the name against a list of known
     standard string literal constants defined in header files. If
     there is one, then add that as an hint to the error message. */
  auto_diagnostic_group d;
  name_hint h;
  if (parser->seen_string_literal && token->type == CPP_NAME)
    {
      tree name = token->value;
      const char *token_name = IDENTIFIER_POINTER (name);
      const char *header_hint
	= get_c_stdlib_header_for_string_macro_name (token_name);
      if (header_hint != NULL)
	h = name_hint (NULL, new suggest_missing_header (token->location,
							 token_name,
							 header_hint));
    }

  c_parse_error (gmsgid,
		 /* Because c_parse_error does not understand
		    CPP_KEYWORD, keywords are treated like
		    identifiers.  */
		 (token->type == CPP_KEYWORD ? CPP_NAME : token->type),
		 /* ??? The C parser does not save the cpp flags of a
		    token, we need to pass 0 here and we will not get
		    the source spelling of some tokens but rather the
		    canonical spelling.  */
		 token->value, /*flags=*/0, richloc);
  return true;
}

/* As c_parser_error_richloc, but issue the message at the
   location of PARSER's next token, or at input_location
   if the next token is EOF.  */

bool
c_parser_error (c_parser *parser, const char *gmsgid)
{
  c_token *token = c_parser_peek_token (parser);
  c_parser_set_source_position_from_token (token);
  rich_location richloc (line_table, input_location);
  return c_parser_error_richloc (parser, gmsgid, &richloc);
}

/* Some tokens naturally come in pairs e.g.'(' and ')'.
   This class is for tracking such a matching pair of symbols.
   In particular, it tracks the location of the first token,
   so that if the second token is missing, we can highlight the
   location of the first token when notifying the user about the
   problem.  */

template <typename traits_t>
class token_pair
{
 public:
  /* token_pair's ctor.  */
  token_pair () : m_open_loc (UNKNOWN_LOCATION) {}

  /* If the next token is the opening symbol for this pair, consume it and
     return true.
     Otherwise, issue an error and return false.
     In either case, record the location of the opening token.  */

  bool require_open (c_parser *parser)
  {
    c_token *token = c_parser_peek_token (parser);
    if (token)
      m_open_loc = token->location;

    return c_parser_require (parser, traits_t::open_token_type,
			     traits_t::open_gmsgid);
  }

  /* Consume the next token from PARSER, recording its location as
     that of the opening token within the pair.  */

  void consume_open (c_parser *parser)
  {
    c_token *token = c_parser_peek_token (parser);
    gcc_assert (token->type == traits_t::open_token_type);
    m_open_loc = token->location;
    c_parser_consume_token (parser);
  }

  /* If the next token is the closing symbol for this pair, consume it
     and return true.
     Otherwise, issue an error, highlighting the location of the
     corresponding opening token, and return false.  */

  bool require_close (c_parser *parser) const
  {
    return c_parser_require (parser, traits_t::close_token_type,
			     traits_t::close_gmsgid, m_open_loc);
  }

  /* Like token_pair::require_close, except that tokens will be skipped
     until the desired token is found.  An error message is still produced
     if the next token is not as expected.  */

  void skip_until_found_close (c_parser *parser) const
  {
    c_parser_skip_until_found (parser, traits_t::close_token_type,
			       traits_t::close_gmsgid, m_open_loc);
  }

 private:
  location_t m_open_loc;
};

/* Traits for token_pair<T> for tracking matching pairs of parentheses.  */

struct matching_paren_traits
{
  static const enum cpp_ttype open_token_type = CPP_OPEN_PAREN;
  static const char * const open_gmsgid;
  static const enum cpp_ttype close_token_type = CPP_CLOSE_PAREN;
  static const char * const close_gmsgid;
};

const char * const matching_paren_traits::open_gmsgid = "expected %<(%>";
const char * const matching_paren_traits::close_gmsgid = "expected %<)%>";

/* "matching_parens" is a token_pair<T> class for tracking matching
   pairs of parentheses.  */

typedef token_pair<matching_paren_traits> matching_parens;

/* Traits for token_pair<T> for tracking matching pairs of braces.  */

struct matching_brace_traits
{
  static const enum cpp_ttype open_token_type = CPP_OPEN_BRACE;
  static const char * const open_gmsgid;
  static const enum cpp_ttype close_token_type = CPP_CLOSE_BRACE;
  static const char * const close_gmsgid;
};

const char * const matching_brace_traits::open_gmsgid = "expected %<{%>";
const char * const matching_brace_traits::close_gmsgid = "expected %<}%>";

/* "matching_braces" is a token_pair<T> class for tracking matching
   pairs of braces.  */

typedef token_pair<matching_brace_traits> matching_braces;

/* Get a description of the matching symbol to TYPE e.g. "(" for
   CPP_CLOSE_PAREN.  */

static const char *
get_matching_symbol (enum cpp_ttype type)
{
  switch (type)
    {
    default:
      gcc_unreachable ();
      return "";
    case CPP_CLOSE_PAREN:
      return "(";
    case CPP_CLOSE_BRACE:
      return "{";
    }
}

/* If the next token is of the indicated TYPE, consume it.  Otherwise,
   issue the error MSGID.  If MSGID is NULL then a message has already
   been produced and no message will be produced this time.  Returns
   true if found, false otherwise.

   If MATCHING_LOCATION is not UNKNOWN_LOCATION, then highlight it
   within any error as the location of an "opening" token matching
   the close token TYPE (e.g. the location of the '(' when TYPE is
   CPP_CLOSE_PAREN).

   If TYPE_IS_UNIQUE is true (the default) then msgid describes exactly
   one type (e.g. "expected %<)%>") and thus it may be reasonable to
   attempt to generate a fix-it hint for the problem.
   Otherwise msgid describes multiple token types (e.g.
   "expected %<;%>, %<,%> or %<)%>"), and thus we shouldn't attempt to
   generate a fix-it hint.  */

bool
c_parser_require (c_parser *parser,
		  enum cpp_ttype type,
		  const char *msgid,
		  location_t matching_location,
		  bool type_is_unique)
{
  if (c_parser_next_token_is (parser, type))
    {
      c_parser_consume_token (parser);
      return true;
    }
  else
    {
      location_t next_token_loc = c_parser_peek_token (parser)->location;
      gcc_rich_location richloc (next_token_loc);

      /* Potentially supply a fix-it hint, suggesting to add the
	 missing token immediately after the *previous* token.
	 This may move the primary location within richloc.  */
      if (!parser->error && type_is_unique)
	maybe_suggest_missing_token_insertion (&richloc, type,
					       parser->last_token_location);

      /* If matching_location != UNKNOWN_LOCATION, highlight it.
	 Attempt to consolidate diagnostics by printing it as a
	 secondary range within the main diagnostic.  */
      bool added_matching_location = false;
      if (matching_location != UNKNOWN_LOCATION)
	added_matching_location
	  = richloc.add_location_if_nearby (matching_location);

      if (c_parser_error_richloc (parser, msgid, &richloc))
	/* If we weren't able to consolidate matching_location, then
	   print it as a secondary diagnostic.  */
	if (matching_location != UNKNOWN_LOCATION && !added_matching_location)
	  inform (matching_location, "to match this %qs",
		  get_matching_symbol (type));

      return false;
    }
}

/* If the next token is the indicated keyword, consume it.  Otherwise,
   issue the error MSGID.  Returns true if found, false otherwise.  */

static bool
c_parser_require_keyword (c_parser *parser,
			  enum rid keyword,
			  const char *msgid)
{
  if (c_parser_next_token_is_keyword (parser, keyword))
    {
      c_parser_consume_token (parser);
      return true;
    }
  else
    {
      c_parser_error (parser, msgid);
      return false;
    }
}

/* Like c_parser_require, except that tokens will be skipped until the
   desired token is found.  An error message is still produced if the
   next token is not as expected.  If MSGID is NULL then a message has
   already been produced and no message will be produced this
   time.

   If MATCHING_LOCATION is not UNKNOWN_LOCATION, then highlight it
   within any error as the location of an "opening" token matching
   the close token TYPE (e.g. the location of the '(' when TYPE is
   CPP_CLOSE_PAREN).  */

void
c_parser_skip_until_found (c_parser *parser,
			   enum cpp_ttype type,
			   const char *msgid,
			   location_t matching_location)
{
  unsigned nesting_depth = 0;

  if (c_parser_require (parser, type, msgid, matching_location))
    return;

  /* Skip tokens until the desired token is found.  */
  while (true)
    {
      /* Peek at the next token.  */
      c_token *token = c_parser_peek_token (parser);
      /* If we've reached the token we want, consume it and stop.  */
      if (token->type == type && !nesting_depth)
	{
	  c_parser_consume_token (parser);
	  break;
	}

      /* If we've run out of tokens, stop.  */
      if (token->type == CPP_EOF)
	return;
      if (token->type == CPP_PRAGMA_EOL && parser->in_pragma)
	return;
      if (token->type == CPP_OPEN_BRACE
	  || token->type == CPP_OPEN_PAREN
	  || token->type == CPP_OPEN_SQUARE)
	++nesting_depth;
      else if (token->type == CPP_CLOSE_BRACE
	       || token->type == CPP_CLOSE_PAREN
	       || token->type == CPP_CLOSE_SQUARE)
	{
	  if (nesting_depth-- == 0)
	    break;
	}
      /* Consume this token.  */
      c_parser_consume_token (parser);
    }
  parser->error = false;
}

/* Skip tokens until the end of a parameter is found, but do not
   consume the comma, semicolon or closing delimiter.  */

static void
c_parser_skip_to_end_of_parameter (c_parser *parser)
{
  unsigned nesting_depth = 0;

  while (true)
    {
      c_token *token = c_parser_peek_token (parser);
      if ((token->type == CPP_COMMA || token->type == CPP_SEMICOLON)
	  && !nesting_depth)
	break;
      /* If we've run out of tokens, stop.  */
      if (token->type == CPP_EOF)
	return;
      if (token->type == CPP_PRAGMA_EOL && parser->in_pragma)
	return;
      if (token->type == CPP_OPEN_BRACE
	  || token->type == CPP_OPEN_PAREN
	  || token->type == CPP_OPEN_SQUARE)
	++nesting_depth;
      else if (token->type == CPP_CLOSE_BRACE
	       || token->type == CPP_CLOSE_PAREN
	       || token->type == CPP_CLOSE_SQUARE)
	{
	  if (nesting_depth-- == 0)
	    break;
	}
      /* Consume this token.  */
      c_parser_consume_token (parser);
    }
  parser->error = false;
}

/* Expect to be at the end of the pragma directive and consume an
   end of line marker.  */

static void
c_parser_skip_to_pragma_eol (c_parser *parser, bool error_if_not_eol = true)
{
  gcc_assert (parser->in_pragma);
  parser->in_pragma = false;

  if (error_if_not_eol && c_parser_peek_token (parser)->type != CPP_PRAGMA_EOL)
    c_parser_error (parser, "expected end of line");

  cpp_ttype token_type;
  do
    {
      c_token *token = c_parser_peek_token (parser);
      token_type = token->type;
      if (token_type == CPP_EOF)
	break;
      c_parser_consume_token (parser);
    }
  while (token_type != CPP_PRAGMA_EOL);

  parser->error = false;
}

/* Skip tokens until we have consumed an entire block, or until we
   have consumed a non-nested ';'.  */

static void
c_parser_skip_to_end_of_block_or_statement (c_parser *parser)
{
  unsigned nesting_depth = 0;
  bool save_error = parser->error;

  while (true)
    {
      c_token *token;

      /* Peek at the next token.  */
      token = c_parser_peek_token (parser);

      switch (token->type)
	{
	case CPP_EOF:
	  return;

	case CPP_PRAGMA_EOL:
	  if (parser->in_pragma)
	    return;
	  break;

	case CPP_SEMICOLON:
	  /* If the next token is a ';', we have reached the
	     end of the statement.  */
	  if (!nesting_depth)
	    {
	      /* Consume the ';'.  */
	      c_parser_consume_token (parser);
	      goto finished;
	    }
	  break;

	case CPP_CLOSE_BRACE:
	  /* If the next token is a non-nested '}', then we have
	     reached the end of the current block.  */
	  if (nesting_depth == 0 || --nesting_depth == 0)
	    {
	      c_parser_consume_token (parser);
	      goto finished;
	    }
	  break;

	case CPP_OPEN_BRACE:
	  /* If it the next token is a '{', then we are entering a new
	     block.  Consume the entire block.  */
	  ++nesting_depth;
	  break;

	case CPP_PRAGMA:
	  /* If we see a pragma, consume the whole thing at once.  We
	     have some safeguards against consuming pragmas willy-nilly.
	     Normally, we'd expect to be here with parser->error set,
	     which disables these safeguards.  But it's possible to get
	     here for secondary error recovery, after parser->error has
	     been cleared.  */
	  c_parser_consume_pragma (parser);
	  c_parser_skip_to_pragma_eol (parser);
	  parser->error = save_error;
	  continue;

	default:
	  break;
	}

      c_parser_consume_token (parser);
    }

 finished:
  parser->error = false;
}

/* CPP's options (initialized by c-opts.c).  */
extern cpp_options *cpp_opts;

/* Save the warning flags which are controlled by __extension__.  */

static inline int
disable_extension_diagnostics (void)
{
  int ret = (pedantic
	     | (warn_pointer_arith << 1)
	     | (warn_traditional << 2)
	     | (flag_iso << 3)
	     | (warn_long_long << 4)
	     | (warn_cxx_compat << 5)
	     | (warn_overlength_strings << 6)
	     /* warn_c90_c99_compat has three states: -1/0/1, so we must
		play tricks to properly restore it.  */
	     | ((warn_c90_c99_compat == 1) << 7)
	     | ((warn_c90_c99_compat == -1) << 8)
	     /* Similarly for warn_c99_c11_compat.  */
	     | ((warn_c99_c11_compat == 1) << 9)
	     | ((warn_c99_c11_compat == -1) << 10)
	     /* Similarly for warn_c11_c2x_compat.  */
	     | ((warn_c11_c2x_compat == 1) << 11)
	     | ((warn_c11_c2x_compat == -1) << 12)
	     );
  cpp_opts->cpp_pedantic = pedantic = 0;
  warn_pointer_arith = 0;
  cpp_opts->cpp_warn_traditional = warn_traditional = 0;
  flag_iso = 0;
  cpp_opts->cpp_warn_long_long = warn_long_long = 0;
  warn_cxx_compat = 0;
  warn_overlength_strings = 0;
  warn_c90_c99_compat = 0;
  warn_c99_c11_compat = 0;
  warn_c11_c2x_compat = 0;
  return ret;
}

/* Restore the warning flags which are controlled by __extension__.
   FLAGS is the return value from disable_extension_diagnostics.  */

static inline void
restore_extension_diagnostics (int flags)
{
  cpp_opts->cpp_pedantic = pedantic = flags & 1;
  warn_pointer_arith = (flags >> 1) & 1;
  cpp_opts->cpp_warn_traditional = warn_traditional = (flags >> 2) & 1;
  flag_iso = (flags >> 3) & 1;
  cpp_opts->cpp_warn_long_long = warn_long_long = (flags >> 4) & 1;
  warn_cxx_compat = (flags >> 5) & 1;
  warn_overlength_strings = (flags >> 6) & 1;
  /* See above for why is this needed.  */
  warn_c90_c99_compat = (flags >> 7) & 1 ? 1 : ((flags >> 8) & 1 ? -1 : 0);
  warn_c99_c11_compat = (flags >> 9) & 1 ? 1 : ((flags >> 10) & 1 ? -1 : 0);
  warn_c11_c2x_compat = (flags >> 11) & 1 ? 1 : ((flags >> 12) & 1 ? -1 : 0);
}

/* Helper data structure for parsing #pragma acc routine.  */
struct oacc_routine_data {
  bool error_seen; /* Set if error has been reported.  */
  bool fndecl_seen; /* Set if one fn decl/definition has been seen already.  */
  tree clauses;
  location_t loc;
};

/* Used for parsing objc foreach statements.  */
static tree objc_foreach_break_label, objc_foreach_continue_label;

static bool c_parser_nth_token_starts_std_attributes (c_parser *,
						      unsigned int);
static tree c_parser_std_attribute_specifier_sequence (c_parser *);
static void c_parser_external_declaration (c_parser *);
static void c_parser_asm_definition (c_parser *);
static void c_parser_declaration_or_fndef (c_parser *, bool, bool, bool,
					   bool, bool, tree *, vec<c_token>,
					   bool have_attrs = false,
					   tree attrs = NULL,
					   struct oacc_routine_data * = NULL,
					   bool * = NULL);
static void c_parser_static_assert_declaration_no_semi (c_parser *);
static void c_parser_static_assert_declaration (c_parser *);
static struct c_typespec c_parser_enum_specifier (c_parser *);
static struct c_typespec c_parser_struct_or_union_specifier (c_parser *);
static tree c_parser_struct_declaration (c_parser *);
static struct c_typespec c_parser_typeof_specifier (c_parser *);
static tree c_parser_alignas_specifier (c_parser *);
static struct c_declarator *c_parser_direct_declarator (c_parser *, bool,
							c_dtr_syn, bool *);
static struct c_declarator *c_parser_direct_declarator_inner (c_parser *,
							      bool,
							      struct c_declarator *);
static struct c_arg_info *c_parser_parms_declarator (c_parser *, bool, tree,
						     bool);
static struct c_arg_info *c_parser_parms_list_declarator (c_parser *, tree,
							  tree, bool);
static struct c_parm *c_parser_parameter_declaration (c_parser *, tree, bool);
static tree c_parser_simple_asm_expr (c_parser *);
static tree c_parser_gnu_attributes (c_parser *);
static struct c_expr c_parser_initializer (c_parser *);
static struct c_expr c_parser_braced_init (c_parser *, tree, bool,
					   struct obstack *);
static void c_parser_initelt (c_parser *, struct obstack *);
static void c_parser_initval (c_parser *, struct c_expr *,
			      struct obstack *);
static tree c_parser_compound_statement (c_parser *, location_t * = NULL);
static location_t c_parser_compound_statement_nostart (c_parser *);
static void c_parser_label (c_parser *, tree);
static void c_parser_statement (c_parser *, bool *, location_t * = NULL);
static void c_parser_statement_after_labels (c_parser *, bool *,
					     vec<tree> * = NULL);
static tree c_parser_c99_block_statement (c_parser *, bool *,
					  location_t * = NULL);
static void c_parser_if_statement (c_parser *, bool *, vec<tree> *);
static void c_parser_switch_statement (c_parser *, bool *);
static void c_parser_while_statement (c_parser *, bool, unsigned short, bool *);
static void c_parser_do_statement (c_parser *, bool, unsigned short);
static void c_parser_for_statement (c_parser *, bool, unsigned short, bool *);
static tree c_parser_asm_statement (c_parser *);
static tree c_parser_asm_operands (c_parser *);
static tree c_parser_asm_goto_operands (c_parser *);
static tree c_parser_asm_clobbers (c_parser *);
static struct c_expr c_parser_expr_no_commas (c_parser *, struct c_expr *,
					      tree = NULL_TREE);
static struct c_expr c_parser_conditional_expression (c_parser *,
						      struct c_expr *, tree);
static struct c_expr c_parser_binary_expression (c_parser *, struct c_expr *,
						 tree);
static struct c_expr c_parser_cast_expression (c_parser *, struct c_expr *);
static struct c_expr c_parser_unary_expression (c_parser *);
static struct c_expr c_parser_sizeof_expression (c_parser *);
static struct c_expr c_parser_alignof_expression (c_parser *);
static struct c_expr c_parser_postfix_expression (c_parser *);
static struct c_expr c_parser_postfix_expression_after_paren_type (c_parser *,
								   struct c_type_name *,
								   location_t);
static struct c_expr c_parser_postfix_expression_after_primary (c_parser *,
								location_t loc,
								struct c_expr);
static tree c_parser_transaction (c_parser *, enum rid);
static struct c_expr c_parser_transaction_expression (c_parser *, enum rid);
static tree c_parser_transaction_cancel (c_parser *);
static struct c_expr c_parser_expression (c_parser *);
static struct c_expr c_parser_expression_conv (c_parser *);
static vec<tree, va_gc> *c_parser_expr_list (c_parser *, bool, bool,
					     vec<tree, va_gc> **, location_t *,
					     tree *, vec<location_t> *,
					     unsigned int * = NULL);
static struct c_expr c_parser_has_attribute_expression (c_parser *);

static void c_parser_oacc_declare (c_parser *);
static void c_parser_oacc_enter_exit_data (c_parser *, bool);
static void c_parser_oacc_update (c_parser *);
static void c_parser_omp_construct (c_parser *, bool *);
static void c_parser_omp_threadprivate (c_parser *);
static void c_parser_omp_barrier (c_parser *);
static void c_parser_omp_depobj (c_parser *);
static void c_parser_omp_flush (c_parser *);
static tree c_parser_omp_for_loop (location_t, c_parser *, enum tree_code,
				   tree, tree *, bool *);
static void c_parser_omp_taskwait (c_parser *);
static void c_parser_omp_taskyield (c_parser *);
static void c_parser_omp_cancel (c_parser *);

enum pragma_context { pragma_external, pragma_struct, pragma_param,
		      pragma_stmt, pragma_compound };
static bool c_parser_pragma (c_parser *, enum pragma_context, bool *);
static void c_parser_omp_cancellation_point (c_parser *, enum pragma_context);
static bool c_parser_omp_target (c_parser *, enum pragma_context, bool *);
static void c_parser_omp_end_declare_target (c_parser *);
static void c_parser_omp_declare (c_parser *, enum pragma_context);
static void c_parser_omp_requires (c_parser *);
static bool c_parser_omp_ordered (c_parser *, enum pragma_context, bool *);
static void c_parser_oacc_routine (c_parser *, enum pragma_context);

/* These Objective-C parser functions are only ever called when
   compiling Objective-C.  */
static void c_parser_objc_class_definition (c_parser *, tree);
static void c_parser_objc_class_instance_variables (c_parser *);
static void c_parser_objc_class_declaration (c_parser *);
static void c_parser_objc_alias_declaration (c_parser *);
static void c_parser_objc_protocol_definition (c_parser *, tree);
static bool c_parser_objc_method_type (c_parser *);
static void c_parser_objc_method_definition (c_parser *);
static void c_parser_objc_methodprotolist (c_parser *);
static void c_parser_objc_methodproto (c_parser *);
static tree c_parser_objc_method_decl (c_parser *, bool, tree *, tree *);
static tree c_parser_objc_type_name (c_parser *);
static tree c_parser_objc_protocol_refs (c_parser *);
static void c_parser_objc_try_catch_finally_statement (c_parser *);
static void c_parser_objc_synchronized_statement (c_parser *);
static tree c_parser_objc_selector (c_parser *);
static tree c_parser_objc_selector_arg (c_parser *);
static tree c_parser_objc_receiver (c_parser *);
static tree c_parser_objc_message_args (c_parser *);
static tree c_parser_objc_keywordexpr (c_parser *);
static void c_parser_objc_at_property_declaration (c_parser *);
static void c_parser_objc_at_synthesize_declaration (c_parser *);
static void c_parser_objc_at_dynamic_declaration (c_parser *);
static bool c_parser_objc_diagnose_bad_element_prefix
  (c_parser *, struct c_declspecs *);
static location_t c_parser_parse_rtl_body (c_parser *, char *);

/* Parse a translation unit (C90 6.7, C99 6.9, C11 6.9).

   translation-unit:
     external-declarations

   external-declarations:
     external-declaration
     external-declarations external-declaration

   GNU extensions:

   translation-unit:
     empty
*/

static void
c_parser_translation_unit (c_parser *parser)
{
  if (c_parser_next_token_is (parser, CPP_EOF))
    {
      pedwarn (c_parser_peek_token (parser)->location, OPT_Wpedantic,
	       "ISO C forbids an empty translation unit");
    }
  else
    {
      void *obstack_position = obstack_alloc (&parser_obstack, 0);
      mark_valid_location_for_stdc_pragma (false);
      do
	{
	  ggc_collect ();
	  c_parser_external_declaration (parser);
	  obstack_free (&parser_obstack, obstack_position);
	}
      while (c_parser_next_token_is_not (parser, CPP_EOF));
    }

  unsigned int i;
  tree decl;
  FOR_EACH_VEC_ELT (incomplete_record_decls, i, decl)
    if (DECL_SIZE (decl) == NULL_TREE && TREE_TYPE (decl) != error_mark_node)
      error ("storage size of %q+D isn%'t known", decl);

  if (current_omp_declare_target_attribute)
    {
      if (!errorcount)
        error ("%<#pragma omp declare target%> without corresponding "
	       "%<#pragma omp end declare target%>");
      current_omp_declare_target_attribute = 0;
    }
}

/* Parse an external declaration (C90 6.7, C99 6.9, C11 6.9).

   external-declaration:
     function-definition
     declaration

   GNU extensions:

   external-declaration:
     asm-definition
     ;
     __extension__ external-declaration

   Objective-C:

   external-declaration:
     objc-class-definition
     objc-class-declaration
     objc-alias-declaration
     objc-protocol-definition
     objc-method-definition
     @end
*/

static void
c_parser_external_declaration (c_parser *parser)
{
  int ext;
  switch (c_parser_peek_token (parser)->type)
    {
    case CPP_KEYWORD:
      switch (c_parser_peek_token (parser)->keyword)
	{
	case RID_EXTENSION:
	  ext = disable_extension_diagnostics ();
	  c_parser_consume_token (parser);
	  c_parser_external_declaration (parser);
	  restore_extension_diagnostics (ext);
	  break;
	case RID_ASM:
	  c_parser_asm_definition (parser);
	  break;
	case RID_AT_INTERFACE:
	case RID_AT_IMPLEMENTATION:
	  gcc_assert (c_dialect_objc ());
	  c_parser_objc_class_definition (parser, NULL_TREE);
	  break;
	case RID_AT_CLASS:
	  gcc_assert (c_dialect_objc ());
	  c_parser_objc_class_declaration (parser);
	  break;
	case RID_AT_ALIAS:
	  gcc_assert (c_dialect_objc ());
	  c_parser_objc_alias_declaration (parser);
	  break;
	case RID_AT_PROTOCOL:
	  gcc_assert (c_dialect_objc ());
	  c_parser_objc_protocol_definition (parser, NULL_TREE);
	  break;
	case RID_AT_PROPERTY:
	  gcc_assert (c_dialect_objc ());
	  c_parser_objc_at_property_declaration (parser);
	  break;
	case RID_AT_SYNTHESIZE:
	  gcc_assert (c_dialect_objc ());
	  c_parser_objc_at_synthesize_declaration (parser);
	  break;
	case RID_AT_DYNAMIC:
	  gcc_assert (c_dialect_objc ());
	  c_parser_objc_at_dynamic_declaration (parser);
	  break;
	case RID_AT_END:
	  gcc_assert (c_dialect_objc ());
	  c_parser_consume_token (parser);
	  objc_finish_implementation ();
	  break;
	default:
	  goto decl_or_fndef;
	}
      break;
    case CPP_SEMICOLON:
      pedwarn (c_parser_peek_token (parser)->location, OPT_Wpedantic,
	       "ISO C does not allow extra %<;%> outside of a function");
      c_parser_consume_token (parser);
      break;
    case CPP_PRAGMA:
      mark_valid_location_for_stdc_pragma (true);
      c_parser_pragma (parser, pragma_external, NULL);
      mark_valid_location_for_stdc_pragma (false);
      break;
    case CPP_PLUS:
    case CPP_MINUS:
      if (c_dialect_objc ())
	{
	  c_parser_objc_method_definition (parser);
	  break;
	}
      /* Else fall through, and yield a syntax error trying to parse
	 as a declaration or function definition.  */
      /* FALLTHRU */
    default:
    decl_or_fndef:
      /* A declaration or a function definition (or, in Objective-C,
	 an @interface or @protocol with prefix attributes).  We can
	 only tell which after parsing the declaration specifiers, if
	 any, and the first declarator.  */
      c_parser_declaration_or_fndef (parser, true, true, true, false, true,
				     NULL, vNULL);
      break;
    }
}

static void c_finish_omp_declare_simd (c_parser *, tree, tree, vec<c_token>);
static void c_finish_oacc_routine (struct oacc_routine_data *, tree, bool);

/* Build and add a DEBUG_BEGIN_STMT statement with location LOC.  */

static void
add_debug_begin_stmt (location_t loc)
{
  /* Don't add DEBUG_BEGIN_STMTs outside of functions, see PR84721.  */
  if (!MAY_HAVE_DEBUG_MARKER_STMTS || !building_stmt_list_p ())
    return;

  tree stmt = build0 (DEBUG_BEGIN_STMT, void_type_node);
  SET_EXPR_LOCATION (stmt, loc);
  add_stmt (stmt);
}

/* Parse a declaration or function definition (C90 6.5, 6.7.1, C99
   6.7, 6.9.1, C11 6.7, 6.9.1).  If FNDEF_OK is true, a function definition
   is accepted; otherwise (old-style parameter declarations) only other
   declarations are accepted.  If STATIC_ASSERT_OK is true, a static
   assertion is accepted; otherwise (old-style parameter declarations)
   it is not.  If NESTED is true, we are inside a function or parsing
   old-style parameter declarations; any functions encountered are
   nested functions and declaration specifiers are required; otherwise
   we are at top level and functions are normal functions and
   declaration specifiers may be optional.  If EMPTY_OK is true, empty
   declarations are OK (subject to all other constraints); otherwise
   (old-style parameter declarations) they are diagnosed.  If
   START_ATTR_OK is true, the declaration specifiers may start with
   attributes (GNU or standard); otherwise they may not.
   OBJC_FOREACH_OBJECT_DECLARATION can be used to get back the parsed
   declaration when parsing an Objective-C foreach statement.
   FALLTHRU_ATTR_P is used to signal whether this function parsed
   "__attribute__((fallthrough));".  ATTRS are any standard attributes
   parsed in the caller (in contexts where such attributes had to be
   parsed to determine whether what follows is a declaration or a
   statement); HAVE_ATTRS says whether there were any such attributes
   (even empty).

   declaration:
     declaration-specifiers init-declarator-list[opt] ;
     static_assert-declaration

   function-definition:
     declaration-specifiers[opt] declarator declaration-list[opt]
       compound-statement

   declaration-list:
     declaration
     declaration-list declaration

   init-declarator-list:
     init-declarator
     init-declarator-list , init-declarator

   init-declarator:
     declarator simple-asm-expr[opt] gnu-attributes[opt]
     declarator simple-asm-expr[opt] gnu-attributes[opt] = initializer

   GNU extensions:

   nested-function-definition:
     declaration-specifiers declarator declaration-list[opt]
       compound-statement

   attribute ;

   Objective-C:
     gnu-attributes objc-class-definition
     gnu-attributes objc-category-definition
     gnu-attributes objc-protocol-definition

   The simple-asm-expr and gnu-attributes are GNU extensions.

   This function does not handle __extension__; that is handled in its
   callers.  ??? Following the old parser, __extension__ may start
   external declarations, declarations in functions and declarations
   at the start of "for" loops, but not old-style parameter
   declarations.

   C99 requires declaration specifiers in a function definition; the
   absence is diagnosed through the diagnosis of implicit int.  In GNU
   C we also allow but diagnose declarations without declaration
   specifiers, but only at top level (elsewhere they conflict with
   other syntax).

   In Objective-C, declarations of the looping variable in a foreach
   statement are exceptionally terminated by 'in' (for example, 'for
   (NSObject *object in array) { ... }').

   OpenMP:

   declaration:
     threadprivate-directive

   GIMPLE:

   gimple-function-definition:
     declaration-specifiers[opt] __GIMPLE (gimple-or-rtl-pass-list) declarator
       declaration-list[opt] compound-statement

   rtl-function-definition:
     declaration-specifiers[opt] __RTL (gimple-or-rtl-pass-list) declarator
       declaration-list[opt] compound-statement  */

static void
c_parser_declaration_or_fndef (c_parser *parser, bool fndef_ok,
			       bool static_assert_ok, bool empty_ok,
			       bool nested, bool start_attr_ok,
			       tree *objc_foreach_object_declaration,
			       vec<c_token> omp_declare_simd_clauses,
			       bool have_attrs, tree attrs,
			       struct oacc_routine_data *oacc_routine_data,
			       bool *fallthru_attr_p)
{
  struct c_declspecs *specs;
  tree prefix_attrs;
  tree all_prefix_attrs;
  bool diagnosed_no_specs = false;
  location_t here = c_parser_peek_token (parser)->location;

  add_debug_begin_stmt (c_parser_peek_token (parser)->location);

  if (static_assert_ok
      && c_parser_next_token_is_keyword (parser, RID_STATIC_ASSERT))
    {
      c_parser_static_assert_declaration (parser);
      return;
    }
  specs = build_null_declspecs ();

  /* Handle any standard attributes parsed in the caller.  */
  if (have_attrs)
    {
      declspecs_add_attrs (here, specs, attrs);
      specs->non_std_attrs_seen_p = false;
    }

  /* Try to detect an unknown type name when we have "A B" or "A *B".  */
  if (c_parser_peek_token (parser)->type == CPP_NAME
      && c_parser_peek_token (parser)->id_kind == C_ID_ID
      && (c_parser_peek_2nd_token (parser)->type == CPP_NAME
          || c_parser_peek_2nd_token (parser)->type == CPP_MULT)
      && (!nested || !lookup_name (c_parser_peek_token (parser)->value)))
    {
      tree name = c_parser_peek_token (parser)->value;

      /* Issue a warning about NAME being an unknown type name, perhaps
	 with some kind of hint.
	 If the user forgot a "struct" etc, suggest inserting
	 it.  Otherwise, attempt to look for misspellings.  */
      gcc_rich_location richloc (here);
      if (tag_exists_p (RECORD_TYPE, name))
	{
	  /* This is not C++ with its implicit typedef.  */
	  richloc.add_fixit_insert_before ("struct ");
	  error_at (&richloc,
		    "unknown type name %qE;"
		    " use %<struct%> keyword to refer to the type",
		    name);
	}
      else if (tag_exists_p (UNION_TYPE, name))
	{
	  richloc.add_fixit_insert_before ("union ");
	  error_at (&richloc,
		    "unknown type name %qE;"
		    " use %<union%> keyword to refer to the type",
		    name);
	}
      else if (tag_exists_p (ENUMERAL_TYPE, name))
	{
	  richloc.add_fixit_insert_before ("enum ");
	  error_at (&richloc,
		    "unknown type name %qE;"
		    " use %<enum%> keyword to refer to the type",
		    name);
	}
      else
	{
	  auto_diagnostic_group d;
	  name_hint hint = lookup_name_fuzzy (name, FUZZY_LOOKUP_TYPENAME,
					      here);
	  if (const char *suggestion = hint.suggestion ())
	    {
	      richloc.add_fixit_replace (suggestion);
	      error_at (&richloc,
			"unknown type name %qE; did you mean %qs?",
			name, suggestion);
	    }
	  else
	    error_at (here, "unknown type name %qE", name);
	}

      /* Parse declspecs normally to get a correct pointer type, but avoid
         a further "fails to be a type name" error.  Refuse nested functions
         since it is not how the user likely wants us to recover.  */
      c_parser_peek_token (parser)->type = CPP_KEYWORD;
      c_parser_peek_token (parser)->keyword = RID_VOID;
      c_parser_peek_token (parser)->value = error_mark_node;
      fndef_ok = !nested;
    }

  /* When there are standard attributes at the start of the
     declaration (to apply to the entity being declared), an
     init-declarator-list or function definition must be present.  */
  if (c_parser_nth_token_starts_std_attributes (parser, 1))
    have_attrs = true;

  c_parser_declspecs (parser, specs, true, true, start_attr_ok,
		      true, true, start_attr_ok, true, cla_nonabstract_decl);
  if (parser->error)
    {
      c_parser_skip_to_end_of_block_or_statement (parser);
      return;
    }
  if (nested && !specs->declspecs_seen_p)
    {
      c_parser_error (parser, "expected declaration specifiers");
      c_parser_skip_to_end_of_block_or_statement (parser);
      return;
    }

  finish_declspecs (specs);
  bool auto_type_p = specs->typespec_word == cts_auto_type;
  if (c_parser_next_token_is (parser, CPP_SEMICOLON))
    {
      if (auto_type_p)
	error_at (here, "%<__auto_type%> in empty declaration");
      else if (specs->typespec_kind == ctsk_none
	       && attribute_fallthrough_p (specs->attrs))
	{
	  if (fallthru_attr_p != NULL)
	    *fallthru_attr_p = true;
	  if (nested)
	    {
	      tree fn = build_call_expr_internal_loc (here, IFN_FALLTHROUGH,
						      void_type_node, 0);
	      add_stmt (fn);
	    }
	  else
	    pedwarn (here, OPT_Wattributes,
		     "%<fallthrough%> attribute at top level");
	}
      else if (empty_ok && !(have_attrs
			     && specs->non_std_attrs_seen_p))
	shadow_tag (specs);
      else
	{
	  shadow_tag_warned (specs, 1);
	  pedwarn (here, 0, "empty declaration");
	}
      c_parser_consume_token (parser);
      if (oacc_routine_data)
	c_finish_oacc_routine (oacc_routine_data, NULL_TREE, false);
      return;
    }

  /* Provide better error recovery.  Note that a type name here is usually
     better diagnosed as a redeclaration.  */
  if (empty_ok
      && specs->typespec_kind == ctsk_tagdef
      && c_parser_next_token_starts_declspecs (parser)
      && !c_parser_next_token_is (parser, CPP_NAME))
    {
      c_parser_error (parser, "expected %<;%>, identifier or %<(%>");
      parser->error = false;
      shadow_tag_warned (specs, 1);
      return;
    }
  else if (c_dialect_objc () && !auto_type_p)
    {
      /* Prefix attributes are an error on method decls.  */
      switch (c_parser_peek_token (parser)->type)
	{
	  case CPP_PLUS:
	  case CPP_MINUS:
	    if (c_parser_objc_diagnose_bad_element_prefix (parser, specs))
	      return;
	    if (specs->attrs)
	      {
		warning_at (c_parser_peek_token (parser)->location, 
			    OPT_Wattributes,
	       		    "prefix attributes are ignored for methods");
		specs->attrs = NULL_TREE;
	      }
	    if (fndef_ok)
	      c_parser_objc_method_definition (parser);
	    else
	      c_parser_objc_methodproto (parser);
	    return;
	    break;
	  default:
	    break;
	}
      /* This is where we parse 'attributes @interface ...',
	 'attributes @implementation ...', 'attributes @protocol ...'
	 (where attributes could be, for example, __attribute__
	 ((deprecated)).
      */
      switch (c_parser_peek_token (parser)->keyword)
	{
	case RID_AT_INTERFACE:
	  {
	    if (c_parser_objc_diagnose_bad_element_prefix (parser, specs))
	      return;
	    c_parser_objc_class_definition (parser, specs->attrs);
	    return;
	  }
	  break;
	case RID_AT_IMPLEMENTATION:
	  {
	    if (c_parser_objc_diagnose_bad_element_prefix (parser, specs))
	      return;
	    if (specs->attrs)
	      {
		warning_at (c_parser_peek_token (parser)->location, 
			OPT_Wattributes,
			"prefix attributes are ignored for implementations");
		specs->attrs = NULL_TREE;
	      }
	    c_parser_objc_class_definition (parser, NULL_TREE);	    
	    return;
	  }
	  break;
	case RID_AT_PROTOCOL:
	  {
	    if (c_parser_objc_diagnose_bad_element_prefix (parser, specs))
	      return;
	    c_parser_objc_protocol_definition (parser, specs->attrs);
	    return;
	  }
	  break;
	case RID_AT_ALIAS:
	case RID_AT_CLASS:
	case RID_AT_END:
	case RID_AT_PROPERTY:
	  if (specs->attrs)
	    {
	      c_parser_error (parser, "unexpected attribute");
	      specs->attrs = NULL;
	    }
	  break;
	default:
	  break;
	}
    }
  else if (attribute_fallthrough_p (specs->attrs))
    warning_at (here, OPT_Wattributes,
		"%<fallthrough%> attribute not followed by %<;%>");

  pending_xref_error ();
  prefix_attrs = specs->attrs;
  all_prefix_attrs = prefix_attrs;
  specs->attrs = NULL_TREE;
  while (true)
    {
      struct c_declarator *declarator;
      bool dummy = false;
      timevar_id_t tv;
      tree fnbody = NULL_TREE;
      /* Declaring either one or more declarators (in which case we
	 should diagnose if there were no declaration specifiers) or a
	 function definition (in which case the diagnostic for
	 implicit int suffices).  */
      declarator = c_parser_declarator (parser, 
					specs->typespec_kind != ctsk_none,
					C_DTR_NORMAL, &dummy);
      if (declarator == NULL)
	{
	  if (omp_declare_simd_clauses.exists ())
	    c_finish_omp_declare_simd (parser, NULL_TREE, NULL_TREE,
				       omp_declare_simd_clauses);
	  if (oacc_routine_data)
	    c_finish_oacc_routine (oacc_routine_data, NULL_TREE, false);
	  c_parser_skip_to_end_of_block_or_statement (parser);
	  return;
	}
      if (auto_type_p && declarator->kind != cdk_id)
	{
	  error_at (here,
		    "%<__auto_type%> requires a plain identifier"
		    " as declarator");
	  c_parser_skip_to_end_of_block_or_statement (parser);
	  return;
	}
      if (c_parser_next_token_is (parser, CPP_EQ)
	  || c_parser_next_token_is (parser, CPP_COMMA)
	  || c_parser_next_token_is (parser, CPP_SEMICOLON)
	  || c_parser_next_token_is_keyword (parser, RID_ASM)
	  || c_parser_next_token_is_keyword (parser, RID_ATTRIBUTE)
	  || c_parser_next_token_is_keyword (parser, RID_IN))
	{
	  tree asm_name = NULL_TREE;
	  tree postfix_attrs = NULL_TREE;
	  if (!diagnosed_no_specs && !specs->declspecs_seen_p)
	    {
	      diagnosed_no_specs = true;
	      pedwarn (here, 0, "data definition has no type or storage class");
	    }
	  /* Having seen a data definition, there cannot now be a
	     function definition.  */
	  fndef_ok = false;
	  if (c_parser_next_token_is_keyword (parser, RID_ASM))
	    asm_name = c_parser_simple_asm_expr (parser);
	  if (c_parser_next_token_is_keyword (parser, RID_ATTRIBUTE))
	    {
	      postfix_attrs = c_parser_gnu_attributes (parser);
	      if (c_parser_next_token_is (parser, CPP_OPEN_BRACE))
		{
		  /* This means there is an attribute specifier after
		     the declarator in a function definition.  Provide
		     some more information for the user.  */
		  error_at (here, "attributes should be specified before the "
			    "declarator in a function definition");
		  c_parser_skip_to_end_of_block_or_statement (parser);
		  return;
		}
	    }
	  if (c_parser_next_token_is (parser, CPP_EQ))
	    {
	      tree d;
	      struct c_expr init;
	      location_t init_loc;
	      c_parser_consume_token (parser);
	      if (auto_type_p)
		{
		  init_loc = c_parser_peek_token (parser)->location;
		  rich_location richloc (line_table, init_loc);
		  start_init (NULL_TREE, asm_name, global_bindings_p (), &richloc);
		  /* A parameter is initialized, which is invalid.  Don't
		     attempt to instrument the initializer.  */
		  int flag_sanitize_save = flag_sanitize;
		  if (nested && !empty_ok)
		    flag_sanitize = 0;
		  init = c_parser_expr_no_commas (parser, NULL);
		  flag_sanitize = flag_sanitize_save;
		  if (TREE_CODE (init.value) == COMPONENT_REF
		      && DECL_C_BIT_FIELD (TREE_OPERAND (init.value, 1)))
		    error_at (here,
			      "%<__auto_type%> used with a bit-field"
			      " initializer");
		  init = convert_lvalue_to_rvalue (init_loc, init, true, true);
		  tree init_type = TREE_TYPE (init.value);
		  bool vm_type = variably_modified_type_p (init_type,
							   NULL_TREE);
		  if (vm_type)
		    init.value = save_expr (init.value);
		  finish_init ();
		  specs->typespec_kind = ctsk_typeof;
		  specs->locations[cdw_typedef] = init_loc;
		  specs->typedef_p = true;
		  specs->type = init_type;
		  if (vm_type)
		    {
		      bool maybe_const = true;
		      tree type_expr = c_fully_fold (init.value, false,
						     &maybe_const);
		      specs->expr_const_operands &= maybe_const;
		      if (specs->expr)
			specs->expr = build2 (COMPOUND_EXPR,
					      TREE_TYPE (type_expr),
					      specs->expr, type_expr);
		      else
			specs->expr = type_expr;
		    }
		  d = start_decl (declarator, specs, true,
				  chainon (postfix_attrs, all_prefix_attrs));
		  if (!d)
		    d = error_mark_node;
		  if (omp_declare_simd_clauses.exists ())
		    c_finish_omp_declare_simd (parser, d, NULL_TREE,
					       omp_declare_simd_clauses);
		}
	      else
		{
		  /* The declaration of the variable is in effect while
		     its initializer is parsed.  */
		  d = start_decl (declarator, specs, true,
				  chainon (postfix_attrs, all_prefix_attrs));
		  if (!d)
		    d = error_mark_node;
		  if (omp_declare_simd_clauses.exists ())
		    c_finish_omp_declare_simd (parser, d, NULL_TREE,
					       omp_declare_simd_clauses);
		  init_loc = c_parser_peek_token (parser)->location;
		  rich_location richloc (line_table, init_loc);
		  start_init (d, asm_name, global_bindings_p (), &richloc);
		  /* A parameter is initialized, which is invalid.  Don't
		     attempt to instrument the initializer.  */
		  int flag_sanitize_save = flag_sanitize;
		  if (TREE_CODE (d) == PARM_DECL)
		    flag_sanitize = 0;
		  init = c_parser_initializer (parser);
		  flag_sanitize = flag_sanitize_save;
		  finish_init ();
		}
	      if (oacc_routine_data)
		c_finish_oacc_routine (oacc_routine_data, d, false);
	      if (d != error_mark_node)
		{
		  maybe_warn_string_init (init_loc, TREE_TYPE (d), init);
		  finish_decl (d, init_loc, init.value,
			       init.original_type, asm_name);
		}
	    }
	  else
	    {
	      if (auto_type_p)
		{
		  error_at (here,
			    "%<__auto_type%> requires an initialized "
			    "data declaration");
		  c_parser_skip_to_end_of_block_or_statement (parser);
		  return;
		}

	      location_t lastloc = UNKNOWN_LOCATION;
	      tree attrs = chainon (postfix_attrs, all_prefix_attrs);
	      tree d = start_decl (declarator, specs, false, attrs, &lastloc);
	      if (d && TREE_CODE (d) == FUNCTION_DECL)
		{
		  /* Find the innermost declarator that is neither cdk_id
		     nor cdk_attrs.  */
		  const struct c_declarator *decl = declarator;
		  const struct c_declarator *last_non_id_attrs = NULL;

		  while (decl)
		    switch (decl->kind)
		      {
		      case cdk_array:
		      case cdk_function:
		      case cdk_pointer:
			last_non_id_attrs = decl;
			decl = decl->declarator;
			break;

		      case cdk_attrs:
			decl = decl->declarator;
			break;

		      case cdk_id:
			decl = 0;
			break;

		      default:
			gcc_unreachable ();
		      }

		  /* If it exists and is cdk_function declaration whose
		     arguments have not been set yet, use its arguments.  */
		  if (last_non_id_attrs
		      && last_non_id_attrs->kind == cdk_function)
		    {
		      tree parms = last_non_id_attrs->u.arg_info->parms;
		      if (DECL_ARGUMENTS (d) == NULL_TREE
			  && DECL_INITIAL (d) == NULL_TREE)
			DECL_ARGUMENTS (d) = parms;

		      warn_parm_array_mismatch (lastloc, d, parms);
		    }
		}
	      if (omp_declare_simd_clauses.exists ())
		{
		  tree parms = NULL_TREE;
		  if (d && TREE_CODE (d) == FUNCTION_DECL)
		    {
		      struct c_declarator *ce = declarator;
		      while (ce != NULL)
			if (ce->kind == cdk_function)
			  {
			    parms = ce->u.arg_info->parms;
			    break;
			  }
			else
			  ce = ce->declarator;
		    }
		  if (parms)
		    temp_store_parm_decls (d, parms);
		  c_finish_omp_declare_simd (parser, d, parms,
					     omp_declare_simd_clauses);
		  if (parms)
		    temp_pop_parm_decls ();
		}
	      if (oacc_routine_data)
		c_finish_oacc_routine (oacc_routine_data, d, false);
	      if (d)
		finish_decl (d, UNKNOWN_LOCATION, NULL_TREE,
			     NULL_TREE, asm_name);

	      if (c_parser_next_token_is_keyword (parser, RID_IN))
		{
		  if (d)
		    *objc_foreach_object_declaration = d;
		  else
		    *objc_foreach_object_declaration = error_mark_node;		    
		}
	    }
	  if (c_parser_next_token_is (parser, CPP_COMMA))
	    {
	      if (auto_type_p)
		{
		  error_at (here,
			    "%<__auto_type%> may only be used with"
			    " a single declarator");
		  c_parser_skip_to_end_of_block_or_statement (parser);
		  return;
		}
	      c_parser_consume_token (parser);
	      if (c_parser_next_token_is_keyword (parser, RID_ATTRIBUTE))
		all_prefix_attrs = chainon (c_parser_gnu_attributes (parser),
					    prefix_attrs);
	      else
		all_prefix_attrs = prefix_attrs;
	      continue;
	    }
	  else if (c_parser_next_token_is (parser, CPP_SEMICOLON))
	    {
	      c_parser_consume_token (parser);
	      return;
	    }
	  else if (c_parser_next_token_is_keyword (parser, RID_IN))
	    {
	      /* This can only happen in Objective-C: we found the
		 'in' that terminates the declaration inside an
		 Objective-C foreach statement.  Do not consume the
		 token, so that the caller can use it to determine
		 that this indeed is a foreach context.  */
	      return;
	    }
	  else
	    {
	      c_parser_error (parser, "expected %<,%> or %<;%>");
	      c_parser_skip_to_end_of_block_or_statement (parser);
	      return;
	    }
	}
      else if (auto_type_p)
	{
	  error_at (here,
		    "%<__auto_type%> requires an initialized data declaration");
	  c_parser_skip_to_end_of_block_or_statement (parser);
	  return;
	}
      else if (!fndef_ok)
	{
	  c_parser_error (parser, "expected %<=%>, %<,%>, %<;%>, "
			  "%<asm%> or %<__attribute__%>");
	  c_parser_skip_to_end_of_block_or_statement (parser);
	  return;
	}
      /* Function definition (nested or otherwise).  */
      if (nested)
	{
	  pedwarn (here, OPT_Wpedantic, "ISO C forbids nested functions");
	  c_push_function_context ();
	}
      if (!start_function (specs, declarator, all_prefix_attrs))
	{
	  /* At this point we've consumed:
	       declaration-specifiers declarator
	     and the next token isn't CPP_EQ, CPP_COMMA, CPP_SEMICOLON,
	     RID_ASM, RID_ATTRIBUTE, or RID_IN,
	     but the
	       declaration-specifiers declarator
	     aren't grokkable as a function definition, so we have
	     an error.  */
	  gcc_assert (!c_parser_next_token_is (parser, CPP_SEMICOLON));
	  if (c_parser_next_token_starts_declspecs (parser))
	    {
	      /* If we have
		   declaration-specifiers declarator decl-specs
		 then assume we have a missing semicolon, which would
		 give us:
		   declaration-specifiers declarator  decl-specs
						    ^
						    ;
		   <~~~~~~~~~ declaration ~~~~~~~~~~>
		 Use c_parser_require to get an error with a fix-it hint.  */
	      c_parser_require (parser, CPP_SEMICOLON, "expected %<;%>");
	      parser->error = false;
	    }
	  else
	    {
	      /* This can appear in many cases looking nothing like a
		 function definition, so we don't give a more specific
		 error suggesting there was one.  */
	      c_parser_error (parser, "expected %<=%>, %<,%>, %<;%>, %<asm%> "
			      "or %<__attribute__%>");
	    }
	  if (nested)
	    c_pop_function_context ();
	  break;
	}

      if (DECL_DECLARED_INLINE_P (current_function_decl))
        tv = TV_PARSE_INLINE;
      else
        tv = TV_PARSE_FUNC;
      auto_timevar at (g_timer, tv);

      /* Parse old-style parameter declarations.  ??? Attributes are
	 not allowed to start declaration specifiers here because of a
	 syntax conflict between a function declaration with attribute
	 suffix and a function definition with an attribute prefix on
	 first old-style parameter declaration.  Following the old
	 parser, they are not accepted on subsequent old-style
	 parameter declarations either.  However, there is no
	 ambiguity after the first declaration, nor indeed on the
	 first as long as we don't allow postfix attributes after a
	 declarator with a nonempty identifier list in a definition;
	 and postfix attributes have never been accepted here in
	 function definitions either.  */
      while (c_parser_next_token_is_not (parser, CPP_EOF)
	     && c_parser_next_token_is_not (parser, CPP_OPEN_BRACE))
	c_parser_declaration_or_fndef (parser, false, false, false,
				       true, false, NULL, vNULL);
      store_parm_decls ();
      if (omp_declare_simd_clauses.exists ())
	c_finish_omp_declare_simd (parser, current_function_decl, NULL_TREE,
				   omp_declare_simd_clauses);
      if (oacc_routine_data)
	c_finish_oacc_routine (oacc_routine_data, current_function_decl, true);
      location_t startloc = c_parser_peek_token (parser)->location;
      DECL_STRUCT_FUNCTION (current_function_decl)->function_start_locus
	= startloc;
      location_t endloc = startloc;

      /* If the definition was marked with __RTL, use the RTL parser now,
	 consuming the function body.  */
      if (specs->declspec_il == cdil_rtl)
	{
	  endloc = c_parser_parse_rtl_body (parser, specs->gimple_or_rtl_pass);

	  /* Normally, store_parm_decls sets next_is_function_body,
	     anticipating a function body.  We need a push_scope/pop_scope
	     pair to flush out this state, or subsequent function parsing
	     will go wrong.  */
	  push_scope ();
	  pop_scope ();

	  finish_function (endloc);
	  return;
	}
      /* If the definition was marked with __GIMPLE then parse the
         function body as GIMPLE.  */
      else if (specs->declspec_il != cdil_none)
	{
	  bool saved = in_late_binary_op;
	  in_late_binary_op = true;
	  c_parser_parse_gimple_body (parser, specs->gimple_or_rtl_pass,
				      specs->declspec_il,
				      specs->entry_bb_count);
	  in_late_binary_op = saved;
	}
      else
	fnbody = c_parser_compound_statement (parser, &endloc);
      tree fndecl = current_function_decl;
      if (nested)
	{
	  tree decl = current_function_decl;
	  /* Mark nested functions as needing static-chain initially.
	     lower_nested_functions will recompute it but the
	     DECL_STATIC_CHAIN flag is also used before that happens,
	     by initializer_constant_valid_p.  See gcc.dg/nested-fn-2.c.  */
	  DECL_STATIC_CHAIN (decl) = 1;
	  add_stmt (fnbody);
	  finish_function (endloc);
	  c_pop_function_context ();
	  add_stmt (build_stmt (DECL_SOURCE_LOCATION (decl), DECL_EXPR, decl));
	}
      else
	{
	  if (fnbody)
	    add_stmt (fnbody);
	  finish_function (endloc);
	}
      /* Get rid of the empty stmt list for GIMPLE/RTL.  */
      if (specs->declspec_il != cdil_none)
	DECL_SAVED_TREE (fndecl) = NULL_TREE;

      break;
    }
}

/* Parse an asm-definition (asm() outside a function body).  This is a
   GNU extension.

   asm-definition:
     simple-asm-expr ;
*/

static void
c_parser_asm_definition (c_parser *parser)
{
  tree asm_str = c_parser_simple_asm_expr (parser);
  if (asm_str)
    symtab->finalize_toplevel_asm (asm_str);
  c_parser_skip_until_found (parser, CPP_SEMICOLON, "expected %<;%>");
}

/* Parse a static assertion (C11 6.7.10).

   static_assert-declaration:
     static_assert-declaration-no-semi ;
*/

static void
c_parser_static_assert_declaration (c_parser *parser)
{
  c_parser_static_assert_declaration_no_semi (parser);
  if (parser->error
      || !c_parser_require (parser, CPP_SEMICOLON, "expected %<;%>"))
    c_parser_skip_to_end_of_block_or_statement (parser);
}

/* Parse a static assertion (C11 6.7.10), without the trailing
   semicolon.

   static_assert-declaration-no-semi:
     _Static_assert ( constant-expression , string-literal )

   C2X:
   static_assert-declaration-no-semi:
     _Static_assert ( constant-expression )
*/

static void
c_parser_static_assert_declaration_no_semi (c_parser *parser)
{
  location_t assert_loc, value_loc;
  tree value;
  tree string = NULL_TREE;

  gcc_assert (c_parser_next_token_is_keyword (parser, RID_STATIC_ASSERT));
  assert_loc = c_parser_peek_token (parser)->location;
  if (flag_isoc99)
    pedwarn_c99 (assert_loc, OPT_Wpedantic,
		 "ISO C99 does not support %<_Static_assert%>");
  else
    pedwarn_c99 (assert_loc, OPT_Wpedantic,
		 "ISO C90 does not support %<_Static_assert%>");
  c_parser_consume_token (parser);
  matching_parens parens;
  if (!parens.require_open (parser))
    return;
  location_t value_tok_loc = c_parser_peek_token (parser)->location;
  value = c_parser_expr_no_commas (parser, NULL).value;
  value_loc = EXPR_LOC_OR_LOC (value, value_tok_loc);
  if (c_parser_next_token_is (parser, CPP_COMMA))
    {
      c_parser_consume_token (parser);
      switch (c_parser_peek_token (parser)->type)
	{
	case CPP_STRING:
	case CPP_STRING16:
	case CPP_STRING32:
	case CPP_WSTRING:
	case CPP_UTF8STRING:
	  string = c_parser_string_literal (parser, false, true).value;
	  break;
	default:
	  c_parser_error (parser, "expected string literal");
	  return;
	}
    }
  else if (flag_isoc11)
    /* If pedantic for pre-C11, the use of _Static_assert itself will
       have been diagnosed, so do not also diagnose the use of this
       new C2X feature of _Static_assert.  */
    pedwarn_c11 (assert_loc, OPT_Wpedantic,
		 "ISO C11 does not support omitting the string in "
		 "%<_Static_assert%>");
  parens.require_close (parser);

  if (!INTEGRAL_TYPE_P (TREE_TYPE (value)))
    {
      error_at (value_loc, "expression in static assertion is not an integer");
      return;
    }
  if (TREE_CODE (value) != INTEGER_CST)
    {
      value = c_fully_fold (value, false, NULL);
      /* Strip no-op conversions.  */
      STRIP_TYPE_NOPS (value);
      if (TREE_CODE (value) == INTEGER_CST)
	pedwarn (value_loc, OPT_Wpedantic, "expression in static assertion "
		 "is not an integer constant expression");
    }
  if (TREE_CODE (value) != INTEGER_CST)
    {
      error_at (value_loc, "expression in static assertion is not constant");
      return;
    }
  constant_expression_warning (value);
  if (integer_zerop (value))
    {
      if (string)
	error_at (assert_loc, "static assertion failed: %E", string);
      else
	error_at (assert_loc, "static assertion failed");
    }
}

/* Parse some declaration specifiers (possibly none) (C90 6.5, C99
   6.7, C11 6.7), adding them to SPECS (which may already include some).
   Storage class specifiers are accepted iff SCSPEC_OK; type
   specifiers are accepted iff TYPESPEC_OK; alignment specifiers are
   accepted iff ALIGNSPEC_OK; gnu-attributes are accepted at the start
   iff START_ATTR_OK; __auto_type is accepted iff AUTO_TYPE_OK.  In
   addition to the syntax shown, standard attributes are accepted at
   the start iff START_STD_ATTR_OK and at the end iff END_STD_ATTR_OK;
   unlike gnu-attributes, they are not accepted in the middle of the
   list.  (This combines various different syntax productions in the C
   standard, and in some cases gnu-attributes and standard attributes
   at the start may already have been parsed before this function is
   called.)

   declaration-specifiers:
     storage-class-specifier declaration-specifiers[opt]
     type-specifier declaration-specifiers[opt]
     type-qualifier declaration-specifiers[opt]
     function-specifier declaration-specifiers[opt]
     alignment-specifier declaration-specifiers[opt]

   Function specifiers (inline) are from C99, and are currently
   handled as storage class specifiers, as is __thread.  Alignment
   specifiers are from C11.

   C90 6.5.1, C99 6.7.1, C11 6.7.1:
   storage-class-specifier:
     typedef
     extern
     static
     auto
     register
     _Thread_local

   (_Thread_local is new in C11.)

   C99 6.7.4, C11 6.7.4:
   function-specifier:
     inline
     _Noreturn

   (_Noreturn is new in C11.)

   C90 6.5.2, C99 6.7.2, C11 6.7.2:
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
     [_Imaginary removed in C99 TC2]
     struct-or-union-specifier
     enum-specifier
     typedef-name
     atomic-type-specifier

   (_Bool and _Complex are new in C99.)
   (atomic-type-specifier is new in C11.)

   C90 6.5.3, C99 6.7.3, C11 6.7.3:

   type-qualifier:
     const
     restrict
     volatile
     address-space-qualifier
     _Atomic

   (restrict is new in C99.)
   (_Atomic is new in C11.)

   GNU extensions:

   declaration-specifiers:
     gnu-attributes declaration-specifiers[opt]

   type-qualifier:
     address-space

   address-space:
     identifier recognized by the target

   storage-class-specifier:
     __thread

   type-specifier:
     typeof-specifier
     __auto_type
     __intN
     _Decimal32
     _Decimal64
     _Decimal128
     _Fract
     _Accum
     _Sat

  (_Fract, _Accum, and _Sat are new from ISO/IEC DTR 18037:
   http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1169.pdf)

   atomic-type-specifier
    _Atomic ( type-name )

   Objective-C:

   type-specifier:
     class-name objc-protocol-refs[opt]
     typedef-name objc-protocol-refs
     objc-protocol-refs
*/

void
c_parser_declspecs (c_parser *parser, struct c_declspecs *specs,
		    bool scspec_ok, bool typespec_ok, bool start_attr_ok,
		    bool alignspec_ok, bool auto_type_ok,
		    bool start_std_attr_ok, bool end_std_attr_ok,
		    enum c_lookahead_kind la)
{
  bool attrs_ok = start_attr_ok;
  bool seen_type = specs->typespec_kind != ctsk_none;

  if (!typespec_ok)
    gcc_assert (la == cla_prefer_id);

  if (start_std_attr_ok
      && c_parser_nth_token_starts_std_attributes (parser, 1))
    {
      gcc_assert (!specs->non_std_attrs_seen_p);
      location_t loc = c_parser_peek_token (parser)->location;
      tree attrs = c_parser_std_attribute_specifier_sequence (parser);
      declspecs_add_attrs (loc, specs, attrs);
      specs->non_std_attrs_seen_p = false;
    }

  while (c_parser_next_token_is (parser, CPP_NAME)
	 || c_parser_next_token_is (parser, CPP_KEYWORD)
	 || (c_dialect_objc () && c_parser_next_token_is (parser, CPP_LESS)))
    {
      struct c_typespec t;
      tree attrs;
      tree align;
      location_t loc = c_parser_peek_token (parser)->location;

      /* If we cannot accept a type, exit if the next token must start
	 one.  Also, if we already have seen a tagged definition,
	 a typename would be an error anyway and likely the user
	 has simply forgotten a semicolon, so we exit.  */
      if ((!typespec_ok || specs->typespec_kind == ctsk_tagdef)
	  && c_parser_next_tokens_start_typename (parser, la)
	  && !c_parser_next_token_is_qualifier (parser)
	  && !c_parser_next_token_is_keyword (parser, RID_ALIGNAS))
	break;

      if (c_parser_next_token_is (parser, CPP_NAME))
	{
	  c_token *name_token = c_parser_peek_token (parser);
	  tree value = name_token->value;
	  c_id_kind kind = name_token->id_kind;

	  if (kind == C_ID_ADDRSPACE)
	    {
	      addr_space_t as
		= name_token->keyword - RID_FIRST_ADDR_SPACE;
	      declspecs_add_addrspace (name_token->location, specs, as);
	      c_parser_consume_token (parser);
	      attrs_ok = true;
	      continue;
	    }

	  gcc_assert (!c_parser_next_token_is_qualifier (parser));

	  /* If we cannot accept a type, and the next token must start one,
	     exit.  Do the same if we already have seen a tagged definition,
	     since it would be an error anyway and likely the user has simply
	     forgotten a semicolon.  */
	  if (seen_type || !c_parser_next_tokens_start_typename (parser, la))
	    break;

	  /* Now at an unknown typename (C_ID_ID), a C_ID_TYPENAME or
	     a C_ID_CLASSNAME.  */
	  c_parser_consume_token (parser);
	  seen_type = true;
	  attrs_ok = true;
	  if (kind == C_ID_ID)
	    {
	      error_at (loc, "unknown type name %qE", value);
	      t.kind = ctsk_typedef;
	      t.spec = error_mark_node;
	    }
	  else if (kind == C_ID_TYPENAME
	           && (!c_dialect_objc ()
	               || c_parser_next_token_is_not (parser, CPP_LESS)))
	    {
	      t.kind = ctsk_typedef;
	      /* For a typedef name, record the meaning, not the name.
		 In case of 'foo foo, bar;'.  */
	      t.spec = lookup_name (value);
	    }
	  else
	    {
	      tree proto = NULL_TREE;
	      gcc_assert (c_dialect_objc ());
	      t.kind = ctsk_objc;
	      if (c_parser_next_token_is (parser, CPP_LESS))
		proto = c_parser_objc_protocol_refs (parser);
	      t.spec = objc_get_protocol_qualified_type (value, proto);
	    }
	  t.expr = NULL_TREE;
	  t.expr_const_operands = true;
	  declspecs_add_type (name_token->location, specs, t);
	  continue;
	}
      if (c_parser_next_token_is (parser, CPP_LESS))
	{
	  /* Make "<SomeProtocol>" equivalent to "id <SomeProtocol>" -
	     nisse@lysator.liu.se.  */
	  tree proto;
	  gcc_assert (c_dialect_objc ());
	  if (!typespec_ok || seen_type)
	    break;
	  proto = c_parser_objc_protocol_refs (parser);
	  t.kind = ctsk_objc;
	  t.spec = objc_get_protocol_qualified_type (NULL_TREE, proto);
	  t.expr = NULL_TREE;
	  t.expr_const_operands = true;
	  declspecs_add_type (loc, specs, t);
	  continue;
	}
      gcc_assert (c_parser_next_token_is (parser, CPP_KEYWORD));
      switch (c_parser_peek_token (parser)->keyword)
	{
	case RID_STATIC:
	case RID_EXTERN:
	case RID_REGISTER:
	case RID_TYPEDEF:
	case RID_INLINE:
	case RID_NORETURN:
	case RID_AUTO:
	case RID_THREAD:
	  if (!scspec_ok)
	    goto out;
	  attrs_ok = true;
	  /* TODO: Distinguish between function specifiers (inline, noreturn)
	     and storage class specifiers, either here or in
	     declspecs_add_scspec.  */
	  declspecs_add_scspec (loc, specs,
				c_parser_peek_token (parser)->value);
	  c_parser_consume_token (parser);
	  break;
	case RID_AUTO_TYPE:
	  if (!auto_type_ok)
	    goto out;
	  /* Fall through.  */
	case RID_UNSIGNED:
	case RID_LONG:
	case RID_SHORT:
	case RID_SIGNED:
	case RID_COMPLEX:
	case RID_INT:
	case RID_CHAR:
	case RID_FLOAT:
	case RID_DOUBLE:
	case RID_VOID:
	case RID_DFLOAT32:
	case RID_DFLOAT64:
	case RID_DFLOAT128:
	CASE_RID_FLOATN_NX:
	case RID_BOOL:
	case RID_FRACT:
	case RID_ACCUM:
	case RID_SAT:
	case RID_INT_N_0:
	case RID_INT_N_1:
	case RID_INT_N_2:
	case RID_INT_N_3:
	  if (!typespec_ok)
	    goto out;
	  attrs_ok = true;
	  seen_type = true;
	  if (c_dialect_objc ())
	    parser->objc_need_raw_identifier = true;
	  t.kind = ctsk_resword;
	  t.spec = c_parser_peek_token (parser)->value;
	  t.expr = NULL_TREE;
	  t.expr_const_operands = true;
	  declspecs_add_type (loc, specs, t);
	  c_parser_consume_token (parser);
	  break;
	case RID_ENUM:
	  if (!typespec_ok)
	    goto out;
	  attrs_ok = true;
	  seen_type = true;
	  t = c_parser_enum_specifier (parser);
          invoke_plugin_callbacks (PLUGIN_FINISH_TYPE, t.spec);
	  declspecs_add_type (loc, specs, t);
	  break;
	case RID_STRUCT:
	case RID_UNION:
	  if (!typespec_ok)
	    goto out;
	  attrs_ok = true;
	  seen_type = true;
	  t = c_parser_struct_or_union_specifier (parser);
          invoke_plugin_callbacks (PLUGIN_FINISH_TYPE, t.spec);
	  declspecs_add_type (loc, specs, t);
	  break;
	case RID_TYPEOF:
	  /* ??? The old parser rejected typeof after other type
	     specifiers, but is a syntax error the best way of
	     handling this?  */
	  if (!typespec_ok || seen_type)
	    goto out;
	  attrs_ok = true;
	  seen_type = true;
	  t = c_parser_typeof_specifier (parser);
	  declspecs_add_type (loc, specs, t);
	  break;
	case RID_ATOMIC:
	  /* C parser handling of Objective-C constructs needs
	     checking for correct lvalue-to-rvalue conversions, and
	     the code in build_modify_expr handling various
	     Objective-C cases, and that in build_unary_op handling
	     Objective-C cases for increment / decrement, also needs
	     updating; uses of TYPE_MAIN_VARIANT in objc_compare_types
	     and objc_types_are_equivalent may also need updates.  */
	  if (c_dialect_objc ())
	    sorry ("%<_Atomic%> in Objective-C");
	  if (flag_isoc99)
	    pedwarn_c99 (loc, OPT_Wpedantic,
			 "ISO C99 does not support the %<_Atomic%> qualifier");
	  else
	    pedwarn_c99 (loc, OPT_Wpedantic,
			 "ISO C90 does not support the %<_Atomic%> qualifier");
	  attrs_ok = true;
	  tree value;
	  value = c_parser_peek_token (parser)->value;
	  c_parser_consume_token (parser);
	  if (typespec_ok && c_parser_next_token_is (parser, CPP_OPEN_PAREN))
	    {
	      /* _Atomic ( type-name ).  */
	      seen_type = true;
	      c_parser_consume_token (parser);
	      struct c_type_name *type = c_parser_type_name (parser);
	      t.kind = ctsk_typeof;
	      t.spec = error_mark_node;
	      t.expr = NULL_TREE;
	      t.expr_const_operands = true;
	      if (type != NULL)
		t.spec = groktypename (type, &t.expr,
				       &t.expr_const_operands);
	      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
					 "expected %<)%>");
	      if (t.spec != error_mark_node)
		{
		  if (TREE_CODE (t.spec) == ARRAY_TYPE)
		    error_at (loc, "%<_Atomic%>-qualified array type");
		  else if (TREE_CODE (t.spec) == FUNCTION_TYPE)
		    error_at (loc, "%<_Atomic%>-qualified function type");
		  else if (TYPE_QUALS (t.spec) != TYPE_UNQUALIFIED)
		    error_at (loc, "%<_Atomic%> applied to a qualified type");
		  else
		    t.spec = c_build_qualified_type (t.spec, TYPE_QUAL_ATOMIC);
		}
	      declspecs_add_type (loc, specs, t);
	    }
	  else
	    declspecs_add_qual (loc, specs, value);
	  break;
	case RID_CONST:
	case RID_VOLATILE:
	case RID_RESTRICT:
	  attrs_ok = true;
	  declspecs_add_qual (loc, specs, c_parser_peek_token (parser)->value);
	  c_parser_consume_token (parser);
	  break;
	case RID_ATTRIBUTE:
	  if (!attrs_ok)
	    goto out;
	  attrs = c_parser_gnu_attributes (parser);
	  declspecs_add_attrs (loc, specs, attrs);
	  break;
	case RID_ALIGNAS:
	  if (!alignspec_ok)
	    goto out;
	  align = c_parser_alignas_specifier (parser);
	  declspecs_add_alignas (loc, specs, align);
	  break;
	case RID_GIMPLE:
	  if (! flag_gimple)
	    error_at (loc, "%<__GIMPLE%> only valid with %<-fgimple%>");
	  c_parser_consume_token (parser);
	  specs->declspec_il = cdil_gimple;
	  specs->locations[cdw_gimple] = loc;
	  c_parser_gimple_or_rtl_pass_list (parser, specs);
	  break;
	case RID_RTL:
	  c_parser_consume_token (parser);
	  specs->declspec_il = cdil_rtl;
	  specs->locations[cdw_rtl] = loc;
	  c_parser_gimple_or_rtl_pass_list (parser, specs);
	  break;
	default:
	  goto out;
	}
    }
 out:
  if (end_std_attr_ok
      && c_parser_nth_token_starts_std_attributes (parser, 1))
    specs->postfix_attrs = c_parser_std_attribute_specifier_sequence (parser);
}

/* Parse an enum specifier (C90 6.5.2.2, C99 6.7.2.2, C11 6.7.2.2).

   enum-specifier:
     enum gnu-attributes[opt] identifier[opt] { enumerator-list }
       gnu-attributes[opt]
     enum gnu-attributes[opt] identifier[opt] { enumerator-list , }
       gnu-attributes[opt]
     enum gnu-attributes[opt] identifier

   The form with trailing comma is new in C99.  The forms with
   gnu-attributes are GNU extensions.  In GNU C, we accept any expression
   without commas in the syntax (assignment expressions, not just
   conditional expressions); assignment expressions will be diagnosed
   as non-constant.

   enumerator-list:
     enumerator
     enumerator-list , enumerator

   enumerator:
     enumeration-constant attribute-specifier-sequence[opt]
     enumeration-constant attribute-specifier-sequence[opt]
       = constant-expression

   GNU Extensions:

   enumerator:
     enumeration-constant attribute-specifier-sequence[opt] gnu-attributes[opt]
     enumeration-constant attribute-specifier-sequence[opt] gnu-attributes[opt]
       = constant-expression

*/

static struct c_typespec
c_parser_enum_specifier (c_parser *parser)
{
  struct c_typespec ret;
  bool have_std_attrs;
  tree std_attrs = NULL_TREE;
  tree attrs;
  tree ident = NULL_TREE;
  location_t enum_loc;
  location_t ident_loc = UNKNOWN_LOCATION;  /* Quiet warning.  */
  gcc_assert (c_parser_next_token_is_keyword (parser, RID_ENUM));
  c_parser_consume_token (parser);
  have_std_attrs = c_parser_nth_token_starts_std_attributes (parser, 1);
  if (have_std_attrs)
    std_attrs = c_parser_std_attribute_specifier_sequence (parser);
  attrs = c_parser_gnu_attributes (parser);
  enum_loc = c_parser_peek_token (parser)->location;
  /* Set the location in case we create a decl now.  */
  c_parser_set_source_position_from_token (c_parser_peek_token (parser));
  if (c_parser_next_token_is (parser, CPP_NAME))
    {
      ident = c_parser_peek_token (parser)->value;
      ident_loc = c_parser_peek_token (parser)->location;
      enum_loc = ident_loc;
      c_parser_consume_token (parser);
    }
  if (c_parser_next_token_is (parser, CPP_OPEN_BRACE))
    {
      /* Parse an enum definition.  */
      struct c_enum_contents the_enum;
      tree type;
      tree postfix_attrs;
      /* We chain the enumerators in reverse order, then put them in
	 forward order at the end.  */
      tree values;
      timevar_push (TV_PARSE_ENUM);
      type = start_enum (enum_loc, &the_enum, ident);
      values = NULL_TREE;
      c_parser_consume_token (parser);
      while (true)
	{
	  tree enum_id;
	  tree enum_value;
	  tree enum_decl;
	  bool seen_comma;
	  c_token *token;
	  location_t comma_loc = UNKNOWN_LOCATION;  /* Quiet warning.  */
	  location_t decl_loc, value_loc;
	  if (c_parser_next_token_is_not (parser, CPP_NAME))
	    {
	      /* Give a nicer error for "enum {}".  */
	      if (c_parser_next_token_is (parser, CPP_CLOSE_BRACE)
		  && !parser->error)
		{
		  error_at (c_parser_peek_token (parser)->location,
			    "empty enum is invalid");
		  parser->error = true;
		}
	      else
		c_parser_error (parser, "expected identifier");
	      c_parser_skip_until_found (parser, CPP_CLOSE_BRACE, NULL);
	      values = error_mark_node;
	      break;
	    }
	  token = c_parser_peek_token (parser);
	  enum_id = token->value;
	  /* Set the location in case we create a decl now.  */
	  c_parser_set_source_position_from_token (token);
	  decl_loc = value_loc = token->location;
	  c_parser_consume_token (parser);
	  /* Parse any specified attributes.  */
	  tree std_attrs = NULL_TREE;
	  if (c_parser_nth_token_starts_std_attributes (parser, 1))
	    std_attrs = c_parser_std_attribute_specifier_sequence (parser);
	  tree enum_attrs = chainon (std_attrs,
				     c_parser_gnu_attributes (parser));
	  if (c_parser_next_token_is (parser, CPP_EQ))
	    {
	      c_parser_consume_token (parser);
	      value_loc = c_parser_peek_token (parser)->location;
	      enum_value = c_parser_expr_no_commas (parser, NULL).value;
	    }
	  else
	    enum_value = NULL_TREE;
	  enum_decl = build_enumerator (decl_loc, value_loc,
					&the_enum, enum_id, enum_value);
	  if (enum_attrs)
	    decl_attributes (&TREE_PURPOSE (enum_decl), enum_attrs, 0);
	  TREE_CHAIN (enum_decl) = values;
	  values = enum_decl;
	  seen_comma = false;
	  if (c_parser_next_token_is (parser, CPP_COMMA))
	    {
	      comma_loc = c_parser_peek_token (parser)->location;
	      seen_comma = true;
	      c_parser_consume_token (parser);
	    }
	  if (c_parser_next_token_is (parser, CPP_CLOSE_BRACE))
	    {
	      if (seen_comma)
		pedwarn_c90 (comma_loc, OPT_Wpedantic,
			     "comma at end of enumerator list");
	      c_parser_consume_token (parser);
	      break;
	    }
	  if (!seen_comma)
	    {
	      c_parser_error (parser, "expected %<,%> or %<}%>");
	      c_parser_skip_until_found (parser, CPP_CLOSE_BRACE, NULL);
	      values = error_mark_node;
	      break;
	    }
	}
      postfix_attrs = c_parser_gnu_attributes (parser);
      ret.spec = finish_enum (type, nreverse (values),
			      chainon (std_attrs,
				       chainon (attrs, postfix_attrs)));
      ret.kind = ctsk_tagdef;
      ret.expr = NULL_TREE;
      ret.expr_const_operands = true;
      timevar_pop (TV_PARSE_ENUM);
      return ret;
    }
  else if (!ident)
    {
      c_parser_error (parser, "expected %<{%>");
      ret.spec = error_mark_node;
      ret.kind = ctsk_tagref;
      ret.expr = NULL_TREE;
      ret.expr_const_operands = true;
      return ret;
    }
  /* Attributes may only appear when the members are defined or in
     certain forward declarations (treat enum forward declarations in
     GNU C analogously to struct and union forward declarations in
     standard C).  */
  if (have_std_attrs && c_parser_next_token_is_not (parser, CPP_SEMICOLON))
    c_parser_error (parser, "expected %<;%>");
  ret = parser_xref_tag (ident_loc, ENUMERAL_TYPE, ident, have_std_attrs,
			 std_attrs);
  /* In ISO C, enumerated types can be referred to only if already
     defined.  */
  if (pedantic && !COMPLETE_TYPE_P (ret.spec))
    {
      gcc_assert (ident);
      pedwarn (enum_loc, OPT_Wpedantic,
	       "ISO C forbids forward references to %<enum%> types");
    }
  return ret;
}

/* Parse a struct or union specifier (C90 6.5.2.1, C99 6.7.2.1, C11 6.7.2.1).

   struct-or-union-specifier:
     struct-or-union attribute-specifier-sequence[opt] gnu-attributes[opt]
       identifier[opt] { struct-contents } gnu-attributes[opt]
     struct-or-union attribute-specifier-sequence[opt] gnu-attributes[opt]
       identifier

   struct-contents:
     struct-declaration-list

   struct-declaration-list:
     struct-declaration ;
     struct-declaration-list struct-declaration ;

   GNU extensions:

   struct-contents:
     empty
     struct-declaration
     struct-declaration-list struct-declaration

   struct-declaration-list:
     struct-declaration-list ;
     ;

   (Note that in the syntax here, unlike that in ISO C, the semicolons
   are included here rather than in struct-declaration, in order to
   describe the syntax with extra semicolons and missing semicolon at
   end.)

   Objective-C:

   struct-declaration-list:
     @defs ( class-name )

   (Note this does not include a trailing semicolon, but can be
   followed by further declarations, and gets a pedwarn-if-pedantic
   when followed by a semicolon.)  */

static struct c_typespec
c_parser_struct_or_union_specifier (c_parser *parser)
{
  struct c_typespec ret;
  bool have_std_attrs;
  tree std_attrs = NULL_TREE;
  tree attrs;
  tree ident = NULL_TREE;
  location_t struct_loc;
  location_t ident_loc = UNKNOWN_LOCATION;
  enum tree_code code;
  switch (c_parser_peek_token (parser)->keyword)
    {
    case RID_STRUCT:
      code = RECORD_TYPE;
      break;
    case RID_UNION:
      code = UNION_TYPE;
      break;
    default:
      gcc_unreachable ();
    }
  struct_loc = c_parser_peek_token (parser)->location;
  c_parser_consume_token (parser);
  have_std_attrs = c_parser_nth_token_starts_std_attributes (parser, 1);
  if (have_std_attrs)
    std_attrs = c_parser_std_attribute_specifier_sequence (parser);
  attrs = c_parser_gnu_attributes (parser);

  /* Set the location in case we create a decl now.  */
  c_parser_set_source_position_from_token (c_parser_peek_token (parser));

  if (c_parser_next_token_is (parser, CPP_NAME))
    {
      ident = c_parser_peek_token (parser)->value;
      ident_loc = c_parser_peek_token (parser)->location;
      struct_loc = ident_loc;
      c_parser_consume_token (parser);
    }
  if (c_parser_next_token_is (parser, CPP_OPEN_BRACE))
    {
      /* Parse a struct or union definition.  Start the scope of the
	 tag before parsing components.  */
      class c_struct_parse_info *struct_info;
      tree type = start_struct (struct_loc, code, ident, &struct_info);
      tree postfix_attrs;
      /* We chain the components in reverse order, then put them in
	 forward order at the end.  Each struct-declaration may
	 declare multiple components (comma-separated), so we must use
	 chainon to join them, although when parsing each
	 struct-declaration we can use TREE_CHAIN directly.

	 The theory behind all this is that there will be more
	 semicolon separated fields than comma separated fields, and
	 so we'll be minimizing the number of node traversals required
	 by chainon.  */
      tree contents;
      timevar_push (TV_PARSE_STRUCT);
      contents = NULL_TREE;
      c_parser_consume_token (parser);
      /* Handle the Objective-C @defs construct,
	 e.g. foo(sizeof(struct{ @defs(ClassName) }));.  */
      if (c_parser_next_token_is_keyword (parser, RID_AT_DEFS))
	{
	  tree name;
	  gcc_assert (c_dialect_objc ());
	  c_parser_consume_token (parser);
	  matching_parens parens;
	  if (!parens.require_open (parser))
	    goto end_at_defs;
	  if (c_parser_next_token_is (parser, CPP_NAME)
	      && c_parser_peek_token (parser)->id_kind == C_ID_CLASSNAME)
	    {
	      name = c_parser_peek_token (parser)->value;
	      c_parser_consume_token (parser);
	    }
	  else
	    {
	      c_parser_error (parser, "expected class name");
	      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, NULL);
	      goto end_at_defs;
	    }
	  parens.skip_until_found_close (parser);
	  contents = nreverse (objc_get_class_ivars (name));
	}
    end_at_defs:
      /* Parse the struct-declarations and semicolons.  Problems with
	 semicolons are diagnosed here; empty structures are diagnosed
	 elsewhere.  */
      while (true)
	{
	  tree decls;
	  /* Parse any stray semicolon.  */
	  if (c_parser_next_token_is (parser, CPP_SEMICOLON))
	    {
	      location_t semicolon_loc
		= c_parser_peek_token (parser)->location;
	      gcc_rich_location richloc (semicolon_loc);
	      richloc.add_fixit_remove ();
	      pedwarn (&richloc, OPT_Wpedantic,
		       "extra semicolon in struct or union specified");
	      c_parser_consume_token (parser);
	      continue;
	    }
	  /* Stop if at the end of the struct or union contents.  */
	  if (c_parser_next_token_is (parser, CPP_CLOSE_BRACE))
	    {
	      c_parser_consume_token (parser);
	      break;
	    }
	  /* Accept #pragmas at struct scope.  */
	  if (c_parser_next_token_is (parser, CPP_PRAGMA))
	    {
	      c_parser_pragma (parser, pragma_struct, NULL);
	      continue;
	    }
	  /* Parse some comma-separated declarations, but not the
	     trailing semicolon if any.  */
	  decls = c_parser_struct_declaration (parser);
	  contents = chainon (decls, contents);
	  /* If no semicolon follows, either we have a parse error or
	     are at the end of the struct or union and should
	     pedwarn.  */
	  if (c_parser_next_token_is (parser, CPP_SEMICOLON))
	    c_parser_consume_token (parser);
	  else
	    {
	      if (c_parser_next_token_is (parser, CPP_CLOSE_BRACE))
		pedwarn (c_parser_peek_token (parser)->location, 0,
			 "no semicolon at end of struct or union");
	      else if (parser->error
		       || !c_parser_next_token_starts_declspecs (parser))
		{
		  c_parser_error (parser, "expected %<;%>");
		  c_parser_skip_until_found (parser, CPP_CLOSE_BRACE, NULL);
		  break;
		}

	      /* If we come here, we have already emitted an error
		 for an expected `;', identifier or `(', and we also
	         recovered already.  Go on with the next field. */
	    }
	}
      postfix_attrs = c_parser_gnu_attributes (parser);
      ret.spec = finish_struct (struct_loc, type, nreverse (contents),
				chainon (std_attrs,
					 chainon (attrs, postfix_attrs)),
				struct_info);
      ret.kind = ctsk_tagdef;
      ret.expr = NULL_TREE;
      ret.expr_const_operands = true;
      timevar_pop (TV_PARSE_STRUCT);
      return ret;
    }
  else if (!ident)
    {
      c_parser_error (parser, "expected %<{%>");
      ret.spec = error_mark_node;
      ret.kind = ctsk_tagref;
      ret.expr = NULL_TREE;
      ret.expr_const_operands = true;
      return ret;
    }
  /* Attributes may only appear when the members are defined or in
     certain forward declarations.  */
  if (have_std_attrs && c_parser_next_token_is_not (parser, CPP_SEMICOLON))
    c_parser_error (parser, "expected %<;%>");
  /* ??? Existing practice is that GNU attributes are ignored after
     the struct or union keyword when not defining the members.  */
  ret = parser_xref_tag (ident_loc, code, ident, have_std_attrs, std_attrs);
  return ret;
}

/* Parse a struct-declaration (C90 6.5.2.1, C99 6.7.2.1, C11 6.7.2.1),
   *without* the trailing semicolon.

   struct-declaration:
     attribute-specifier-sequence[opt] specifier-qualifier-list
       attribute-specifier-sequence[opt] struct-declarator-list
     static_assert-declaration-no-semi

   specifier-qualifier-list:
     type-specifier specifier-qualifier-list[opt]
     type-qualifier specifier-qualifier-list[opt]
     alignment-specifier specifier-qualifier-list[opt]
     gnu-attributes specifier-qualifier-list[opt]

   struct-declarator-list:
     struct-declarator
     struct-declarator-list , gnu-attributes[opt] struct-declarator

   struct-declarator:
     declarator gnu-attributes[opt]
     declarator[opt] : constant-expression gnu-attributes[opt]

   GNU extensions:

   struct-declaration:
     __extension__ struct-declaration
     specifier-qualifier-list

   Unlike the ISO C syntax, semicolons are handled elsewhere.  The use
   of gnu-attributes where shown is a GNU extension.  In GNU C, we accept
   any expression without commas in the syntax (assignment
   expressions, not just conditional expressions); assignment
   expressions will be diagnosed as non-constant.  */

static tree
c_parser_struct_declaration (c_parser *parser)
{
  struct c_declspecs *specs;
  tree prefix_attrs;
  tree all_prefix_attrs;
  tree decls;
  location_t decl_loc;
  if (c_parser_next_token_is_keyword (parser, RID_EXTENSION))
    {
      int ext;
      tree decl;
      ext = disable_extension_diagnostics ();
      c_parser_consume_token (parser);
      decl = c_parser_struct_declaration (parser);
      restore_extension_diagnostics (ext);
      return decl;
    }
  if (c_parser_next_token_is_keyword (parser, RID_STATIC_ASSERT))
    {
      c_parser_static_assert_declaration_no_semi (parser);
      return NULL_TREE;
    }
  specs = build_null_declspecs ();
  decl_loc = c_parser_peek_token (parser)->location;
  /* Strictly by the standard, we shouldn't allow _Alignas here,
     but it appears to have been intended to allow it there, so
     we're keeping it as it is until WG14 reaches a conclusion
     of N1731.
     <http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1731.pdf>  */
  c_parser_declspecs (parser, specs, false, true, true,
		      true, false, true, true, cla_nonabstract_decl);
  if (parser->error)
    return NULL_TREE;
  if (!specs->declspecs_seen_p)
    {
      c_parser_error (parser, "expected specifier-qualifier-list");
      return NULL_TREE;
    }
  finish_declspecs (specs);
  if (c_parser_next_token_is (parser, CPP_SEMICOLON)
      || c_parser_next_token_is (parser, CPP_CLOSE_BRACE))
    {
      tree ret;
      if (specs->typespec_kind == ctsk_none)
	{
	  pedwarn (decl_loc, OPT_Wpedantic,
		   "ISO C forbids member declarations with no members");
	  shadow_tag_warned (specs, pedantic);
	  ret = NULL_TREE;
	}
      else
	{
	  /* Support for unnamed structs or unions as members of
	     structs or unions (which is [a] useful and [b] supports
	     MS P-SDK).  */
	  tree attrs = NULL;

	  ret = grokfield (c_parser_peek_token (parser)->location,
			   build_id_declarator (NULL_TREE), specs,
			   NULL_TREE, &attrs);
	  if (ret)
	    decl_attributes (&ret, attrs, 0);
	}
      return ret;
    }

  /* Provide better error recovery.  Note that a type name here is valid,
     and will be treated as a field name.  */
  if (specs->typespec_kind == ctsk_tagdef
      && TREE_CODE (specs->type) != ENUMERAL_TYPE
      && c_parser_next_token_starts_declspecs (parser)
      && !c_parser_next_token_is (parser, CPP_NAME))
    {
      c_parser_error (parser, "expected %<;%>, identifier or %<(%>");
      parser->error = false;
      return NULL_TREE;
    }

  pending_xref_error ();
  prefix_attrs = specs->attrs;
  all_prefix_attrs = prefix_attrs;
  specs->attrs = NULL_TREE;
  decls = NULL_TREE;
  while (true)
    {
      /* Declaring one or more declarators or un-named bit-fields.  */
      struct c_declarator *declarator;
      bool dummy = false;
      if (c_parser_next_token_is (parser, CPP_COLON))
	declarator = build_id_declarator (NULL_TREE);
      else
	declarator = c_parser_declarator (parser,
					  specs->typespec_kind != ctsk_none,
					  C_DTR_NORMAL, &dummy);
      if (declarator == NULL)
	{
	  c_parser_skip_to_end_of_block_or_statement (parser);
	  break;
	}
      if (c_parser_next_token_is (parser, CPP_COLON)
	  || c_parser_next_token_is (parser, CPP_COMMA)
	  || c_parser_next_token_is (parser, CPP_SEMICOLON)
	  || c_parser_next_token_is (parser, CPP_CLOSE_BRACE)
	  || c_parser_next_token_is_keyword (parser, RID_ATTRIBUTE))
	{
	  tree postfix_attrs = NULL_TREE;
	  tree width = NULL_TREE;
	  tree d;
	  if (c_parser_next_token_is (parser, CPP_COLON))
	    {
	      c_parser_consume_token (parser);
	      width = c_parser_expr_no_commas (parser, NULL).value;
	    }
	  if (c_parser_next_token_is_keyword (parser, RID_ATTRIBUTE))
	    postfix_attrs = c_parser_gnu_attributes (parser);
	  d = grokfield (c_parser_peek_token (parser)->location,
			 declarator, specs, width, &all_prefix_attrs);
	  decl_attributes (&d, chainon (postfix_attrs,
					all_prefix_attrs), 0);
	  DECL_CHAIN (d) = decls;
	  decls = d;
	  if (c_parser_next_token_is_keyword (parser, RID_ATTRIBUTE))
	    all_prefix_attrs = chainon (c_parser_gnu_attributes (parser),
					prefix_attrs);
	  else
	    all_prefix_attrs = prefix_attrs;
	  if (c_parser_next_token_is (parser, CPP_COMMA))
	    c_parser_consume_token (parser);
	  else if (c_parser_next_token_is (parser, CPP_SEMICOLON)
		   || c_parser_next_token_is (parser, CPP_CLOSE_BRACE))
	    {
	      /* Semicolon consumed in caller.  */
	      break;
	    }
	  else
	    {
	      c_parser_error (parser, "expected %<,%>, %<;%> or %<}%>");
	      break;
	    }
	}
      else
	{
	  c_parser_error (parser,
			  "expected %<:%>, %<,%>, %<;%>, %<}%> or "
			  "%<__attribute__%>");
	  break;
	}
    }
  return decls;
}

/* Parse a typeof specifier (a GNU extension).

   typeof-specifier:
     typeof ( expression )
     typeof ( type-name )
*/

static struct c_typespec
c_parser_typeof_specifier (c_parser *parser)
{
  struct c_typespec ret;
  ret.kind = ctsk_typeof;
  ret.spec = error_mark_node;
  ret.expr = NULL_TREE;
  ret.expr_const_operands = true;
  gcc_assert (c_parser_next_token_is_keyword (parser, RID_TYPEOF));
  c_parser_consume_token (parser);
  c_inhibit_evaluation_warnings++;
  in_typeof++;
  matching_parens parens;
  if (!parens.require_open (parser))
    {
      c_inhibit_evaluation_warnings--;
      in_typeof--;
      return ret;
    }
  if (c_parser_next_tokens_start_typename (parser, cla_prefer_id))
    {
      struct c_type_name *type = c_parser_type_name (parser);
      c_inhibit_evaluation_warnings--;
      in_typeof--;
      if (type != NULL)
	{
	  ret.spec = groktypename (type, &ret.expr, &ret.expr_const_operands);
	  pop_maybe_used (variably_modified_type_p (ret.spec, NULL_TREE));
	}
    }
  else
    {
      bool was_vm;
      location_t here = c_parser_peek_token (parser)->location;
      struct c_expr expr = c_parser_expression (parser);
      c_inhibit_evaluation_warnings--;
      in_typeof--;
      if (TREE_CODE (expr.value) == COMPONENT_REF
	  && DECL_C_BIT_FIELD (TREE_OPERAND (expr.value, 1)))
	error_at (here, "%<typeof%> applied to a bit-field");
      mark_exp_read (expr.value);
      ret.spec = TREE_TYPE (expr.value);
      was_vm = variably_modified_type_p (ret.spec, NULL_TREE);
      /* This is returned with the type so that when the type is
	 evaluated, this can be evaluated.  */
      if (was_vm)
	ret.expr = c_fully_fold (expr.value, false, &ret.expr_const_operands);
      pop_maybe_used (was_vm);
    }
  parens.skip_until_found_close (parser);
  return ret;
}

/* Parse an alignment-specifier.

   C11 6.7.5:

   alignment-specifier:
     _Alignas ( type-name )
     _Alignas ( constant-expression )
*/

static tree
c_parser_alignas_specifier (c_parser * parser)
{
  tree ret = error_mark_node;
  location_t loc = c_parser_peek_token (parser)->location;
  gcc_assert (c_parser_next_token_is_keyword (parser, RID_ALIGNAS));
  c_parser_consume_token (parser);
  if (flag_isoc99)
    pedwarn_c99 (loc, OPT_Wpedantic,
		 "ISO C99 does not support %<_Alignas%>");
  else
    pedwarn_c99 (loc, OPT_Wpedantic,
		 "ISO C90 does not support %<_Alignas%>");
  matching_parens parens;
  if (!parens.require_open (parser))
    return ret;
  if (c_parser_next_tokens_start_typename (parser, cla_prefer_id))
    {
      struct c_type_name *type = c_parser_type_name (parser);
      if (type != NULL)
	ret = c_sizeof_or_alignof_type (loc, groktypename (type, NULL, NULL),
					false, true, 1);
    }
  else
    ret = c_parser_expr_no_commas (parser, NULL).value;
  parens.skip_until_found_close (parser);
  return ret;
}

/* Parse a declarator, possibly an abstract declarator (C90 6.5.4,
   6.5.5, C99 6.7.5, 6.7.6, C11 6.7.6, 6.7.7).  If TYPE_SEEN_P then
   a typedef name may be redeclared; otherwise it may not.  KIND
   indicates which kind of declarator is wanted.  Returns a valid
   declarator except in the case of a syntax error in which case NULL is
   returned.  *SEEN_ID is set to true if an identifier being declared is
   seen; this is used to diagnose bad forms of abstract array declarators
   and to determine whether an identifier list is syntactically permitted.

   declarator:
     pointer[opt] direct-declarator

   direct-declarator:
     identifier
     ( gnu-attributes[opt] declarator )
     direct-declarator array-declarator
     direct-declarator ( parameter-type-list )
     direct-declarator ( identifier-list[opt] )

   pointer:
     * type-qualifier-list[opt]
     * type-qualifier-list[opt] pointer

   type-qualifier-list:
     type-qualifier
     gnu-attributes
     type-qualifier-list type-qualifier
     type-qualifier-list gnu-attributes

   array-declarator:
     [ type-qualifier-list[opt] assignment-expression[opt] ]
     [ static type-qualifier-list[opt] assignment-expression ]
     [ type-qualifier-list static assignment-expression ]
     [ type-qualifier-list[opt] * ]

   parameter-type-list:
     parameter-list
     parameter-list , ...

   parameter-list:
     parameter-declaration
     parameter-list , parameter-declaration

   parameter-declaration:
     declaration-specifiers declarator gnu-attributes[opt]
     declaration-specifiers abstract-declarator[opt] gnu-attributes[opt]

   identifier-list:
     identifier
     identifier-list , identifier

   abstract-declarator:
     pointer
     pointer[opt] direct-abstract-declarator

   direct-abstract-declarator:
     ( gnu-attributes[opt] abstract-declarator )
     direct-abstract-declarator[opt] array-declarator
     direct-abstract-declarator[opt] ( parameter-type-list[opt] )

   GNU extensions:

   direct-declarator:
     direct-declarator ( parameter-forward-declarations
			 parameter-type-list[opt] )

   direct-abstract-declarator:
     direct-abstract-declarator[opt] ( parameter-forward-declarations
				       parameter-type-list[opt] )

   parameter-forward-declarations:
     parameter-list ;
     parameter-forward-declarations parameter-list ;

   The uses of gnu-attributes shown above are GNU extensions.

   Some forms of array declarator are not included in C99 in the
   syntax for abstract declarators; these are disallowed elsewhere.
   This may be a defect (DR#289).

   This function also accepts an omitted abstract declarator as being
   an abstract declarator, although not part of the formal syntax.  */

struct c_declarator *
c_parser_declarator (c_parser *parser, bool type_seen_p, c_dtr_syn kind,
		     bool *seen_id)
{
  /* Parse any initial pointer part.  */
  if (c_parser_next_token_is (parser, CPP_MULT))
    {
      struct c_declspecs *quals_attrs = build_null_declspecs ();
      struct c_declarator *inner;
      c_parser_consume_token (parser);
      c_parser_declspecs (parser, quals_attrs, false, false, true,
			  false, false, true, false, cla_prefer_id);
      inner = c_parser_declarator (parser, type_seen_p, kind, seen_id);
      if (inner == NULL)
	return NULL;
      else
	return make_pointer_declarator (quals_attrs, inner);
    }
  /* Now we have a direct declarator, direct abstract declarator or
     nothing (which counts as a direct abstract declarator here).  */
  return c_parser_direct_declarator (parser, type_seen_p, kind, seen_id);
}

/* Parse a direct declarator or direct abstract declarator; arguments
   as c_parser_declarator.  */

static struct c_declarator *
c_parser_direct_declarator (c_parser *parser, bool type_seen_p, c_dtr_syn kind,
			    bool *seen_id)
{
  /* The direct declarator must start with an identifier (possibly
     omitted) or a parenthesized declarator (possibly abstract).  In
     an ordinary declarator, initial parentheses must start a
     parenthesized declarator.  In an abstract declarator or parameter
     declarator, they could start a parenthesized declarator or a
     parameter list.  To tell which, the open parenthesis and any
     following gnu-attributes must be read.  If a declaration
     specifier or standard attributes follow, then it is a parameter
     list; if the specifier is a typedef name, there might be an
     ambiguity about redeclaring it, which is resolved in the
     direction of treating it as a typedef name.  If a close
     parenthesis follows, it is also an empty parameter list, as the
     syntax does not permit empty abstract declarators.  Otherwise, it
     is a parenthesized declarator (in which case the analysis may be
     repeated inside it, recursively).

     ??? There is an ambiguity in a parameter declaration "int
     (__attribute__((foo)) x)", where x is not a typedef name: it
     could be an abstract declarator for a function, or declare x with
     parentheses.  The proper resolution of this ambiguity needs
     documenting.  At present we follow an accident of the old
     parser's implementation, whereby the first parameter must have
     some declaration specifiers other than just gnu-attributes.  Thus as
     a parameter declaration it is treated as a parenthesized
     parameter named x, and as an abstract declarator it is
     rejected.

     ??? Also following the old parser, gnu-attributes inside an empty
     parameter list are ignored, making it a list not yielding a
     prototype, rather than giving an error or making it have one
     parameter with implicit type int.

     ??? Also following the old parser, typedef names may be
     redeclared in declarators, but not Objective-C class names.  */

  if (kind != C_DTR_ABSTRACT
      && c_parser_next_token_is (parser, CPP_NAME)
      && ((type_seen_p
	   && (c_parser_peek_token (parser)->id_kind == C_ID_TYPENAME
	       || c_parser_peek_token (parser)->id_kind == C_ID_CLASSNAME))
	  || c_parser_peek_token (parser)->id_kind == C_ID_ID))
    {
      struct c_declarator *inner
	= build_id_declarator (c_parser_peek_token (parser)->value);
      *seen_id = true;
      inner->id_loc = c_parser_peek_token (parser)->location;
      c_parser_consume_token (parser);
      if (c_parser_nth_token_starts_std_attributes (parser, 1))
	inner->u.id.attrs = c_parser_std_attribute_specifier_sequence (parser);
      return c_parser_direct_declarator_inner (parser, *seen_id, inner);
    }

  if (kind != C_DTR_NORMAL
      && c_parser_next_token_is (parser, CPP_OPEN_SQUARE)
      && !c_parser_nth_token_starts_std_attributes (parser, 1))
    {
      struct c_declarator *inner = build_id_declarator (NULL_TREE);
      inner->id_loc = c_parser_peek_token (parser)->location;
      return c_parser_direct_declarator_inner (parser, *seen_id, inner);
    }

  /* Either we are at the end of an abstract declarator, or we have
     parentheses.  */

  if (c_parser_next_token_is (parser, CPP_OPEN_PAREN))
    {
      tree attrs;
      struct c_declarator *inner;
      c_parser_consume_token (parser);
      bool have_gnu_attrs = c_parser_next_token_is_keyword (parser,
							    RID_ATTRIBUTE);
      attrs = c_parser_gnu_attributes (parser);
      if (kind != C_DTR_NORMAL
	  && (c_parser_next_token_starts_declspecs (parser)
	      || (!have_gnu_attrs
		  && c_parser_nth_token_starts_std_attributes (parser, 1))
	      || c_parser_next_token_is (parser, CPP_CLOSE_PAREN)))
	{
	  struct c_arg_info *args
	    = c_parser_parms_declarator (parser, kind == C_DTR_NORMAL,
					 attrs, have_gnu_attrs);
	  if (args == NULL)
	    return NULL;
	  else
	    {
	      inner = build_id_declarator (NULL_TREE);
	      if (!(args->types
		    && args->types != error_mark_node
		    && TREE_CODE (TREE_VALUE (args->types)) == IDENTIFIER_NODE)
		  && c_parser_nth_token_starts_std_attributes (parser, 1))
		{
		  tree std_attrs
		    = c_parser_std_attribute_specifier_sequence (parser);
		  if (std_attrs)
		    inner = build_attrs_declarator (std_attrs, inner);
		}
	      inner = build_function_declarator (args, inner);
	      return c_parser_direct_declarator_inner (parser, *seen_id,
						       inner);
	    }
	}
      /* A parenthesized declarator.  */
      inner = c_parser_declarator (parser, type_seen_p, kind, seen_id);
      if (inner != NULL && attrs != NULL)
	inner = build_attrs_declarator (attrs, inner);
      if (c_parser_next_token_is (parser, CPP_CLOSE_PAREN))
	{
	  c_parser_consume_token (parser);
	  if (inner == NULL)
	    return NULL;
	  else
	    return c_parser_direct_declarator_inner (parser, *seen_id, inner);
	}
      else
	{
	  c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
				     "expected %<)%>");
	  return NULL;
	}
    }
  else
    {
      if (kind == C_DTR_NORMAL)
	{
	  c_parser_error (parser, "expected identifier or %<(%>");
	  return NULL;
	}
      else
	return build_id_declarator (NULL_TREE);
    }
}

/* Parse part of a direct declarator or direct abstract declarator,
   given that some (in INNER) has already been parsed; ID_PRESENT is
   true if an identifier is present, false for an abstract
   declarator.  */

static struct c_declarator *
c_parser_direct_declarator_inner (c_parser *parser, bool id_present,
				  struct c_declarator *inner)
{
  /* Parse a sequence of array declarators and parameter lists.  */
  if (c_parser_next_token_is (parser, CPP_OPEN_SQUARE)
      && !c_parser_nth_token_starts_std_attributes (parser, 1))
    {
      location_t brace_loc = c_parser_peek_token (parser)->location;
      struct c_declarator *declarator;
      struct c_declspecs *quals_attrs = build_null_declspecs ();
      bool static_seen;
      bool star_seen;
      struct c_expr dimen;
      dimen.value = NULL_TREE;
      dimen.original_code = ERROR_MARK;
      dimen.original_type = NULL_TREE;
      c_parser_consume_token (parser);
      c_parser_declspecs (parser, quals_attrs, false, false, true,
			  false, false, false, false, cla_prefer_id);
      static_seen = c_parser_next_token_is_keyword (parser, RID_STATIC);
      if (static_seen)
	c_parser_consume_token (parser);
      if (static_seen && !quals_attrs->declspecs_seen_p)
	c_parser_declspecs (parser, quals_attrs, false, false, true,
			    false, false, false, false, cla_prefer_id);
      if (!quals_attrs->declspecs_seen_p)
	quals_attrs = NULL;
      /* If "static" is present, there must be an array dimension.
	 Otherwise, there may be a dimension, "*", or no
	 dimension.  */
      if (static_seen)
	{
	  star_seen = false;
	  dimen = c_parser_expr_no_commas (parser, NULL);
	}
      else
	{
	  if (c_parser_next_token_is (parser, CPP_CLOSE_SQUARE))
	    {
	      dimen.value = NULL_TREE;
	      star_seen = false;
	    }
	  else if (c_parser_next_token_is (parser, CPP_MULT))
	    {
	      if (c_parser_peek_2nd_token (parser)->type == CPP_CLOSE_SQUARE)
		{
		  dimen.value = NULL_TREE;
		  star_seen = true;
		  c_parser_consume_token (parser);
		}
	      else
		{
		  star_seen = false;
		  dimen = c_parser_expr_no_commas (parser, NULL);
		}
	    }
	  else
	    {
	      star_seen = false;
	      dimen = c_parser_expr_no_commas (parser, NULL);
	    }
	}
      if (c_parser_next_token_is (parser, CPP_CLOSE_SQUARE))
	c_parser_consume_token (parser);
      else
	{
	  c_parser_skip_until_found (parser, CPP_CLOSE_SQUARE,
				     "expected %<]%>");
	  return NULL;
	}
      if (dimen.value)
	dimen = convert_lvalue_to_rvalue (brace_loc, dimen, true, true);
      declarator = build_array_declarator (brace_loc, dimen.value, quals_attrs,
					   static_seen, star_seen);
      if (declarator == NULL)
	return NULL;
      if (c_parser_nth_token_starts_std_attributes (parser, 1))
	{
	  tree std_attrs
	    = c_parser_std_attribute_specifier_sequence (parser);
	  if (std_attrs)
	    inner = build_attrs_declarator (std_attrs, inner);
	}
      inner = set_array_declarator_inner (declarator, inner);
      return c_parser_direct_declarator_inner (parser, id_present, inner);
    }
  else if (c_parser_next_token_is (parser, CPP_OPEN_PAREN))
    {
      tree attrs;
      struct c_arg_info *args;
      c_parser_consume_token (parser);
      bool have_gnu_attrs = c_parser_next_token_is_keyword (parser,
							    RID_ATTRIBUTE);
      attrs = c_parser_gnu_attributes (parser);
      args = c_parser_parms_declarator (parser, id_present, attrs,
					have_gnu_attrs);
      if (args == NULL)
	return NULL;
      else
	{
	  if (!(args->types
		&& args->types != error_mark_node
		&& TREE_CODE (TREE_VALUE (args->types)) == IDENTIFIER_NODE)
	      && c_parser_nth_token_starts_std_attributes (parser, 1))
	    {
	      tree std_attrs
		= c_parser_std_attribute_specifier_sequence (parser);
	      if (std_attrs)
		inner = build_attrs_declarator (std_attrs, inner);
	    }
	  inner = build_function_declarator (args, inner);
	  return c_parser_direct_declarator_inner (parser, id_present, inner);
	}
    }
  return inner;
}

/* Parse a parameter list or identifier list, including the closing
   parenthesis but not the opening one.  ATTRS are the gnu-attributes
   at the start of the list.  ID_LIST_OK is true if an identifier list
   is acceptable; such a list must not have attributes at the start.
   HAVE_GNU_ATTRS says whether any gnu-attributes (including empty
   attributes) were present (in which case standard attributes cannot
   occur).  */

static struct c_arg_info *
c_parser_parms_declarator (c_parser *parser, bool id_list_ok, tree attrs,
			   bool have_gnu_attrs)
{
  push_scope ();
  declare_parm_level ();
  /* If the list starts with an identifier, it is an identifier list.
     Otherwise, it is either a prototype list or an empty list.  */
  if (id_list_ok
      && !attrs
      && c_parser_next_token_is (parser, CPP_NAME)
      && c_parser_peek_token (parser)->id_kind == C_ID_ID
      
      /* Look ahead to detect typos in type names.  */
      && c_parser_peek_2nd_token (parser)->type != CPP_NAME
      && c_parser_peek_2nd_token (parser)->type != CPP_MULT
      && c_parser_peek_2nd_token (parser)->type != CPP_OPEN_PAREN
      && c_parser_peek_2nd_token (parser)->type != CPP_OPEN_SQUARE
      && c_parser_peek_2nd_token (parser)->type != CPP_KEYWORD)
    {
      tree list = NULL_TREE, *nextp = &list;
      while (c_parser_next_token_is (parser, CPP_NAME)
	     && c_parser_peek_token (parser)->id_kind == C_ID_ID)
	{
	  *nextp = build_tree_list (NULL_TREE,
				    c_parser_peek_token (parser)->value);
	  nextp = & TREE_CHAIN (*nextp);
	  c_parser_consume_token (parser);
	  if (c_parser_next_token_is_not (parser, CPP_COMMA))
	    break;
	  c_parser_consume_token (parser);
	  if (c_parser_next_token_is (parser, CPP_CLOSE_PAREN))
	    {
	      c_parser_error (parser, "expected identifier");
	      break;
	    }
	}
      if (c_parser_next_token_is (parser, CPP_CLOSE_PAREN))
	{
	  struct c_arg_info *ret = build_arg_info ();
	  ret->types = list;
	  c_parser_consume_token (parser);
	  pop_scope ();
	  return ret;
	}
      else
	{
	  c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
				     "expected %<)%>");
	  pop_scope ();
	  return NULL;
	}
    }
  else
    {
      struct c_arg_info *ret
	= c_parser_parms_list_declarator (parser, attrs, NULL, have_gnu_attrs);
      pop_scope ();
      return ret;
    }
}

/* Parse a parameter list (possibly empty), including the closing
   parenthesis but not the opening one.  ATTRS are the gnu-attributes
   at the start of the list; if HAVE_GNU_ATTRS, there were some such
   attributes (possibly empty, in which case ATTRS is NULL_TREE),
   which means standard attributes cannot start the list.  EXPR is
   NULL or an expression that needs to be evaluated for the side
   effects of array size expressions in the parameters.  */

static struct c_arg_info *
c_parser_parms_list_declarator (c_parser *parser, tree attrs, tree expr,
				bool have_gnu_attrs)
{
  bool bad_parm = false;

  /* ??? Following the old parser, forward parameter declarations may
     use abstract declarators, and if no real parameter declarations
     follow the forward declarations then this is not diagnosed.  Also
     note as above that gnu-attributes are ignored as the only contents of
     the parentheses, or as the only contents after forward
     declarations.  */
  if (c_parser_next_token_is (parser, CPP_CLOSE_PAREN))
    {
      struct c_arg_info *ret = build_arg_info ();
      c_parser_consume_token (parser);
      return ret;
    }
  if (c_parser_next_token_is (parser, CPP_ELLIPSIS))
    {
      struct c_arg_info *ret = build_arg_info ();

      if (flag_allow_parameterless_variadic_functions)
        {
          /* F (...) is allowed.  */
          ret->types = NULL_TREE;
        }
      else
        {
          /* Suppress -Wold-style-definition for this case.  */
          ret->types = error_mark_node;
          error_at (c_parser_peek_token (parser)->location,
                    "ISO C requires a named argument before %<...%>");
        }
      c_parser_consume_token (parser);
      if (c_parser_next_token_is (parser, CPP_CLOSE_PAREN))
	{
	  c_parser_consume_token (parser);
	  return ret;
	}
      else
	{
	  c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
				     "expected %<)%>");
	  return NULL;
	}
    }
  /* Nonempty list of parameters, either terminated with semicolon
     (forward declarations; recurse) or with close parenthesis (normal
     function) or with ", ... )" (variadic function).  */
  while (true)
    {
      /* Parse a parameter.  */
      struct c_parm *parm = c_parser_parameter_declaration (parser, attrs,
							    have_gnu_attrs);
      attrs = NULL_TREE;
      have_gnu_attrs = false;
      if (parm == NULL)
	bad_parm = true;
      else
	push_parm_decl (parm, &expr);
      if (c_parser_next_token_is (parser, CPP_SEMICOLON))
	{
	  tree new_attrs;
	  c_parser_consume_token (parser);
	  mark_forward_parm_decls ();
	  bool new_have_gnu_attrs
	    = c_parser_next_token_is_keyword (parser, RID_ATTRIBUTE);
	  new_attrs = c_parser_gnu_attributes (parser);
	  return c_parser_parms_list_declarator (parser, new_attrs, expr,
						 new_have_gnu_attrs);
	}
      if (c_parser_next_token_is (parser, CPP_CLOSE_PAREN))
	{
	  c_parser_consume_token (parser);
	  if (bad_parm)
	    return NULL;
	  else
	    return get_parm_info (false, expr);
	}
      if (!c_parser_require (parser, CPP_COMMA,
			     "expected %<;%>, %<,%> or %<)%>",
			     UNKNOWN_LOCATION, false))
	{
	  c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, NULL);
	  return NULL;
	}
      if (c_parser_next_token_is (parser, CPP_ELLIPSIS))
	{
	  c_parser_consume_token (parser);
	  if (c_parser_next_token_is (parser, CPP_CLOSE_PAREN))
	    {
	      c_parser_consume_token (parser);
	      if (bad_parm)
		return NULL;
	      else
		return get_parm_info (true, expr);
	    }
	  else
	    {
	      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
					 "expected %<)%>");
	      return NULL;
	    }
	}
    }
}

/* Parse a parameter declaration.  ATTRS are the gnu-attributes at the
   start of the declaration if it is the first parameter;
   HAVE_GNU_ATTRS is true if there were any gnu-attributes there (even
   empty) there.  */

static struct c_parm *
c_parser_parameter_declaration (c_parser *parser, tree attrs,
				bool have_gnu_attrs)
{
  struct c_declspecs *specs;
  struct c_declarator *declarator;
  tree prefix_attrs;
  tree postfix_attrs = NULL_TREE;
  bool dummy = false;

  /* Accept #pragmas between parameter declarations.  */
  while (c_parser_next_token_is (parser, CPP_PRAGMA))
    c_parser_pragma (parser, pragma_param, NULL);

  if (!c_parser_next_token_starts_declspecs (parser)
      && !c_parser_nth_token_starts_std_attributes (parser, 1))
    {
      c_token *token = c_parser_peek_token (parser);
      if (parser->error)
	return NULL;
      c_parser_set_source_position_from_token (token);
      if (c_parser_next_tokens_start_typename (parser, cla_prefer_type))
	{
	  auto_diagnostic_group d;
	  name_hint hint = lookup_name_fuzzy (token->value,
					      FUZZY_LOOKUP_TYPENAME,
					      token->location);
	  if (const char *suggestion = hint.suggestion ())
	    {
	      gcc_rich_location richloc (token->location);
	      richloc.add_fixit_replace (suggestion);
	      error_at (&richloc,
			"unknown type name %qE; did you mean %qs?",
			token->value, suggestion);
	    }
	  else
	    error_at (token->location, "unknown type name %qE", token->value);
	  parser->error = true;
	}
      /* ??? In some Objective-C cases '...' isn't applicable so there
	 should be a different message.  */
      else
	c_parser_error (parser,
			"expected declaration specifiers or %<...%>");
      c_parser_skip_to_end_of_parameter (parser);
      return NULL;
    }

  location_t start_loc = c_parser_peek_token (parser)->location;

  specs = build_null_declspecs ();
  if (attrs)
    {
      declspecs_add_attrs (input_location, specs, attrs);
      attrs = NULL_TREE;
    }
  c_parser_declspecs (parser, specs, true, true, true, true, false,
		      !have_gnu_attrs, true, cla_nonabstract_decl);
  finish_declspecs (specs);
  pending_xref_error ();
  prefix_attrs = specs->attrs;
  specs->attrs = NULL_TREE;
  declarator = c_parser_declarator (parser,
				    specs->typespec_kind != ctsk_none,
				    C_DTR_PARM, &dummy);
  if (declarator == NULL)
    {
      c_parser_skip_until_found (parser, CPP_COMMA, NULL);
      return NULL;
    }
  if (c_parser_next_token_is_keyword (parser, RID_ATTRIBUTE))
    postfix_attrs = c_parser_gnu_attributes (parser);

  /* Generate a location for the parameter, ranging from the start of the
     initial token to the end of the final token.

     If we have a identifier, then use it for the caret location, e.g.

       extern int callee (int one, int (*two)(int, int), float three);
                                   ~~~~~~^~~~~~~~~~~~~~

     otherwise, reuse the start location for the caret location e.g.:

       extern int callee (int one, int (*)(int, int), float three);
                                   ^~~~~~~~~~~~~~~~~
  */
  location_t end_loc = parser->last_token_location;

  /* Find any cdk_id declarator; determine if we have an identifier.  */
  c_declarator *id_declarator = declarator;
  while (id_declarator && id_declarator->kind != cdk_id)
    id_declarator = id_declarator->declarator;
  location_t caret_loc = (id_declarator->u.id.id
			  ? id_declarator->id_loc
			  : start_loc);
  location_t param_loc = make_location (caret_loc, start_loc, end_loc);

  return build_c_parm (specs, chainon (postfix_attrs, prefix_attrs),
		       declarator, param_loc);
}

/* Parse a string literal in an asm expression.  It should not be
   translated, and wide string literals are an error although
   permitted by the syntax.  This is a GNU extension.

   asm-string-literal:
     string-literal
*/

static tree
c_parser_asm_string_literal (c_parser *parser)
{
  tree str;
  int save_flag = warn_overlength_strings;
  warn_overlength_strings = 0;
  str = c_parser_string_literal (parser, false, false).value;
  warn_overlength_strings = save_flag;
  return str;
}

/* Parse a simple asm expression.  This is used in restricted
   contexts, where a full expression with inputs and outputs does not
   make sense.  This is a GNU extension.

   simple-asm-expr:
     asm ( asm-string-literal )
*/

static tree
c_parser_simple_asm_expr (c_parser *parser)
{
  tree str;
  gcc_assert (c_parser_next_token_is_keyword (parser, RID_ASM));
  c_parser_consume_token (parser);
  matching_parens parens;
  if (!parens.require_open (parser))
    return NULL_TREE;
  str = c_parser_asm_string_literal (parser);
  if (!parens.require_close (parser))
    {
      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, NULL);
      return NULL_TREE;
    }
  return str;
}

static tree
c_parser_gnu_attribute_any_word (c_parser *parser)
{
  tree attr_name = NULL_TREE;

  if (c_parser_next_token_is (parser, CPP_KEYWORD))
    {
      /* ??? See comment above about what keywords are accepted here.  */
      bool ok;
      switch (c_parser_peek_token (parser)->keyword)
	{
	case RID_STATIC:
	case RID_UNSIGNED:
	case RID_LONG:
	case RID_CONST:
	case RID_EXTERN:
	case RID_REGISTER:
	case RID_TYPEDEF:
	case RID_SHORT:
	case RID_INLINE:
	case RID_NORETURN:
	case RID_VOLATILE:
	case RID_SIGNED:
	case RID_AUTO:
	case RID_RESTRICT:
	case RID_COMPLEX:
	case RID_THREAD:
	case RID_INT:
	case RID_CHAR:
	case RID_FLOAT:
	case RID_DOUBLE:
	case RID_VOID:
	case RID_DFLOAT32:
	case RID_DFLOAT64:
	case RID_DFLOAT128:
	CASE_RID_FLOATN_NX:
	case RID_BOOL:
	case RID_FRACT:
	case RID_ACCUM:
	case RID_SAT:
	case RID_TRANSACTION_ATOMIC:
	case RID_TRANSACTION_CANCEL:
	case RID_ATOMIC:
	case RID_AUTO_TYPE:
	case RID_INT_N_0:
	case RID_INT_N_1:
	case RID_INT_N_2:
	case RID_INT_N_3:
	  ok = true;
	  break;
	default:
	  ok = false;
	  break;
	}
      if (!ok)
	return NULL_TREE;

      /* Accept __attribute__((__const)) as __attribute__((const)) etc.  */
      attr_name = ridpointers[(int) c_parser_peek_token (parser)->keyword];
    }
  else if (c_parser_next_token_is (parser, CPP_NAME))
    attr_name = c_parser_peek_token (parser)->value;

  return attr_name;
}

/* Parse attribute arguments.  This is a common form of syntax
   covering all currently valid GNU and standard attributes.

   gnu-attribute-arguments:
     identifier
     identifier , nonempty-expr-list
     expr-list

   where the "identifier" must not be declared as a type.  ??? Why not
   allow identifiers declared as types to start the arguments?  */

static tree
c_parser_attribute_arguments (c_parser *parser, bool takes_identifier,
			      bool require_string, bool allow_empty_args)
{
  vec<tree, va_gc> *expr_list;
  tree attr_args;
  /* Parse the attribute contents.  If they start with an
     identifier which is followed by a comma or close
     parenthesis, then the arguments start with that
     identifier; otherwise they are an expression list.
     In objective-c the identifier may be a classname.  */
  if (c_parser_next_token_is (parser, CPP_NAME)
      && (c_parser_peek_token (parser)->id_kind == C_ID_ID
	  || (c_dialect_objc ()
	      && c_parser_peek_token (parser)->id_kind
	      == C_ID_CLASSNAME))
      && ((c_parser_peek_2nd_token (parser)->type == CPP_COMMA)
	  || (c_parser_peek_2nd_token (parser)->type
	      == CPP_CLOSE_PAREN))
      && (takes_identifier
	  || (c_dialect_objc ()
	      && c_parser_peek_token (parser)->id_kind
	      == C_ID_CLASSNAME)))
    {
      tree arg1 = c_parser_peek_token (parser)->value;
      c_parser_consume_token (parser);
      if (c_parser_next_token_is (parser, CPP_CLOSE_PAREN))
	attr_args = build_tree_list (NULL_TREE, arg1);
      else
	{
	  tree tree_list;
	  c_parser_consume_token (parser);
	  expr_list = c_parser_expr_list (parser, false, true,
					  NULL, NULL, NULL, NULL);
	  tree_list = build_tree_list_vec (expr_list);
	  attr_args = tree_cons (NULL_TREE, arg1, tree_list);
	  release_tree_vector (expr_list);
	}
    }
  else
    {
      if (c_parser_next_token_is (parser, CPP_CLOSE_PAREN))
	{
	  if (!allow_empty_args)
	    error_at (c_parser_peek_token (parser)->location,
		      "parentheses must be omitted if "
		      "attribute argument list is empty");
	  attr_args = NULL_TREE;
	}
      else if (require_string)
	{
	  /* The only valid argument for this attribute is a string
	     literal.  Handle this specially here to avoid accepting
	     string literals with excess parentheses.  */
	  tree string = c_parser_string_literal (parser, false, true).value;
	  attr_args = build_tree_list (NULL_TREE, string);
	}
      else
	{
	  expr_list = c_parser_expr_list (parser, false, true,
					  NULL, NULL, NULL, NULL);
	  attr_args = build_tree_list_vec (expr_list);
	  release_tree_vector (expr_list);
	}
    }
  return attr_args;
}

/* Parse (possibly empty) gnu-attributes.  This is a GNU extension.

   gnu-attributes:
     empty
     gnu-attributes gnu-attribute

   gnu-attribute:
     __attribute__ ( ( gnu-attribute-list ) )

   gnu-attribute-list:
     gnu-attrib
     gnu-attribute_list , gnu-attrib

   gnu-attrib:
     empty
     any-word
     any-word ( gnu-attribute-arguments )

   where "any-word" may be any identifier (including one declared as a
   type), a reserved word storage class specifier, type specifier or
   type qualifier.  ??? This still leaves out most reserved keywords
   (following the old parser), shouldn't we include them?
   When EXPECT_COMMA is true, expect the attribute to be preceded
   by a comma and fail if it isn't.
   When EMPTY_OK is true, allow and consume any number of consecutive
   commas with no attributes in between.  */

static tree
c_parser_gnu_attribute (c_parser *parser, tree attrs,
			bool expect_comma = false, bool empty_ok = true)
{
  bool comma_first = c_parser_next_token_is (parser, CPP_COMMA);
  if (!comma_first
      && !c_parser_next_token_is (parser, CPP_NAME)
      && !c_parser_next_token_is (parser, CPP_KEYWORD))
    return NULL_TREE;

  while (c_parser_next_token_is (parser, CPP_COMMA))
    {
      c_parser_consume_token (parser);
      if (!empty_ok)
	return attrs;
    }

  tree attr_name = c_parser_gnu_attribute_any_word (parser);
  if (attr_name == NULL_TREE)
    return NULL_TREE;

  attr_name = canonicalize_attr_name (attr_name);
  c_parser_consume_token (parser);

  tree attr;
  if (c_parser_next_token_is_not (parser, CPP_OPEN_PAREN))
    {
      if (expect_comma && !comma_first)
	{
	  /* A comma is missing between the last attribute on the chain
	     and this one.  */
	  c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
				     "expected %<)%>");
	  return error_mark_node;
	}
      attr = build_tree_list (attr_name, NULL_TREE);
      /* Add this attribute to the list.  */
      attrs = chainon (attrs, attr);
      return attrs;
    }
  c_parser_consume_token (parser);

  tree attr_args
    = c_parser_attribute_arguments (parser,
				    attribute_takes_identifier_p (attr_name),
				    false, true);

  attr = build_tree_list (attr_name, attr_args);
  if (c_parser_next_token_is (parser, CPP_CLOSE_PAREN))
    c_parser_consume_token (parser);
  else
    {
      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
				 "expected %<)%>");
      return error_mark_node;
    }

  if (expect_comma && !comma_first)
    {
      /* A comma is missing between the last attribute on the chain
	 and this one.  */
      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
				 "expected %<)%>");
      return error_mark_node;
    }

  /* Add this attribute to the list.  */
  attrs = chainon (attrs, attr);
  return attrs;
}

static tree
c_parser_gnu_attributes (c_parser *parser)
{
  tree attrs = NULL_TREE;
  while (c_parser_next_token_is_keyword (parser, RID_ATTRIBUTE))
    {
      bool save_translate_strings_p = parser->translate_strings_p;
      parser->translate_strings_p = false;
      /* Consume the `__attribute__' keyword.  */
      c_parser_consume_token (parser);
      /* Look for the two `(' tokens.  */
      if (!c_parser_require (parser, CPP_OPEN_PAREN, "expected %<(%>"))
	{
	  parser->translate_strings_p = save_translate_strings_p;
	  return attrs;
	}
      if (!c_parser_require (parser, CPP_OPEN_PAREN, "expected %<(%>"))
	{
	  parser->translate_strings_p = save_translate_strings_p;
	  c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, NULL);
	  return attrs;
	}
      /* Parse the attribute list.  Require a comma between successive
	 (possibly empty) attributes.  */
      for (bool expect_comma = false; ; expect_comma = true)
	{
	  /* Parse a single attribute.  */
	  tree attr = c_parser_gnu_attribute (parser, attrs, expect_comma);
	  if (attr == error_mark_node)
	    return attrs;
	  if (!attr)
	    break;
	  attrs = attr;
      }

      /* Look for the two `)' tokens.  */
      if (c_parser_next_token_is (parser, CPP_CLOSE_PAREN))
	c_parser_consume_token (parser);
      else
	{
	  parser->translate_strings_p = save_translate_strings_p;
	  c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
				     "expected %<)%>");
	  return attrs;
	}
      if (c_parser_next_token_is (parser, CPP_CLOSE_PAREN))
	c_parser_consume_token (parser);
      else
	{
	  parser->translate_strings_p = save_translate_strings_p;
	  c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
				     "expected %<)%>");
	  return attrs;
	}
      parser->translate_strings_p = save_translate_strings_p;
    }

  return attrs;
}

/* Parse an optional balanced token sequence.

   balanced-token-sequence:
     balanced-token
     balanced-token-sequence balanced-token

   balanced-token:
     ( balanced-token-sequence[opt] )
     [ balanced-token-sequence[opt] ]
     { balanced-token-sequence[opt] }
     any token other than ()[]{}
*/

static void
c_parser_balanced_token_sequence (c_parser *parser)
{
  while (true)
    {
      c_token *token = c_parser_peek_token (parser);
      switch (token->type)
	{
	case CPP_OPEN_BRACE:
	  {
	    matching_braces braces;
	    braces.consume_open (parser);
	    c_parser_balanced_token_sequence (parser);
	    braces.require_close (parser);
	    break;
	  }

	case CPP_OPEN_PAREN:
	  {
	    matching_parens parens;
	    parens.consume_open (parser);
	    c_parser_balanced_token_sequence (parser);
	    parens.require_close (parser);
	    break;
	  }

	case CPP_OPEN_SQUARE:
	  c_parser_consume_token (parser);
	  c_parser_balanced_token_sequence (parser);
	  c_parser_require (parser, CPP_CLOSE_SQUARE, "expected %<]%>");
	  break;

	case CPP_CLOSE_BRACE:
	case CPP_CLOSE_PAREN:
	case CPP_CLOSE_SQUARE:
	case CPP_EOF:
	  return;

	default:
	  c_parser_consume_token (parser);
	  break;
	}
    }
}

/* Parse standard (C2X) attributes (including GNU attributes in the
   gnu:: namespace).

   attribute-specifier-sequence:
     attribute-specifier-sequence[opt] attribute-specifier

   attribute-specifier:
     [ [ attribute-list ] ]

   attribute-list:
     attribute[opt]
     attribute-list, attribute[opt]

   attribute:
     attribute-token attribute-argument-clause[opt]

   attribute-token:
     standard-attribute
     attribute-prefixed-token

   standard-attribute:
     identifier

   attribute-prefixed-token:
     attribute-prefix :: identifier

   attribute-prefix:
     identifier

   attribute-argument-clause:
     ( balanced-token-sequence[opt] )

   Keywords are accepted as identifiers for this purpose.
*/

static tree
c_parser_std_attribute (c_parser *parser, bool for_tm)
{
  c_token *token = c_parser_peek_token (parser);
  tree ns, name, attribute;

  /* Parse the attribute-token.  */
  if (token->type != CPP_NAME && token->type != CPP_KEYWORD)
    {
      c_parser_error (parser, "expected identifier");
      return error_mark_node;
    }
  name = canonicalize_attr_name (token->value);
  c_parser_consume_token (parser);
  if (c_parser_next_token_is (parser, CPP_SCOPE))
    {
      ns = name;
      c_parser_consume_token (parser);
      token = c_parser_peek_token (parser);
      if (token->type != CPP_NAME && token->type != CPP_KEYWORD)
	{
	  c_parser_error (parser, "expected identifier");
	  return error_mark_node;
	}
      name = canonicalize_attr_name (token->value);
      c_parser_consume_token (parser);
    }
  else
    ns = NULL_TREE;
  attribute = build_tree_list (build_tree_list (ns, name), NULL_TREE);

  /* Parse the arguments, if any.  */
  const attribute_spec *as = lookup_attribute_spec (TREE_PURPOSE (attribute));
  if (c_parser_next_token_is_not (parser, CPP_OPEN_PAREN))
    goto out;
  {
    location_t open_loc = c_parser_peek_token (parser)->location;
    matching_parens parens;
    parens.consume_open (parser);
    if ((as && as->max_length == 0)
	/* Special-case the transactional-memory attribute "outer",
	   which is specially handled but not registered as an
	   attribute, to avoid allowing arbitrary balanced token
	   sequences as arguments.  */
	|| is_attribute_p ("outer", name))
      {
	error_at (open_loc, "%qE attribute does not take any arguments", name);
	parens.skip_until_found_close (parser);
	return error_mark_node;
      }
    if (as)
      {
	bool takes_identifier
	  = (ns != NULL_TREE
	     && strcmp (IDENTIFIER_POINTER (ns), "gnu") == 0
	     && attribute_takes_identifier_p (name));
	bool require_string
	  = (ns == NULL_TREE
	     && (strcmp (IDENTIFIER_POINTER (name), "deprecated") == 0
		 || strcmp (IDENTIFIER_POINTER (name), "nodiscard") == 0));
	TREE_VALUE (attribute)
	  = c_parser_attribute_arguments (parser, takes_identifier,
					  require_string, false);
      }
    else
      c_parser_balanced_token_sequence (parser);
    parens.require_close (parser);
  }
 out:
  if (ns == NULL_TREE && !for_tm && !as)
    {
      /* An attribute with standard syntax and no namespace specified
	 is a constraint violation if it is not one of the known
	 standard attributes.  Diagnose it here with a pedwarn and
	 then discard it to prevent a duplicate warning later.  */
      pedwarn (input_location, OPT_Wattributes, "%qE attribute ignored",
	       name);
      return error_mark_node;
    }
  return attribute;
}

static tree
c_parser_std_attribute_specifier (c_parser *parser, bool for_tm)
{
  location_t loc = c_parser_peek_token (parser)->location;
  if (!c_parser_require (parser, CPP_OPEN_SQUARE, "expected %<[%>"))
    return NULL_TREE;
  if (!c_parser_require (parser, CPP_OPEN_SQUARE, "expected %<[%>"))
    {
      c_parser_skip_until_found (parser, CPP_CLOSE_SQUARE, "expected %<]%>");
      return NULL_TREE;
    }
  if (!for_tm)
    pedwarn_c11 (loc, OPT_Wpedantic,
		 "ISO C does not support %<[[]]%> attributes before C2X");
  tree attributes = NULL_TREE;
  while (true)
    {
      c_token *token = c_parser_peek_token (parser);
      if (token->type == CPP_CLOSE_SQUARE)
	break;
      if (token->type == CPP_COMMA)
	{
	  c_parser_consume_token (parser);
	  continue;
	}
      tree attribute = c_parser_std_attribute (parser, for_tm);
      if (attribute != error_mark_node)
	{
	  TREE_CHAIN (attribute) = attributes;
	  attributes = attribute;
	}
      if (c_parser_next_token_is_not (parser, CPP_COMMA))
	break;
    }
  c_parser_skip_until_found (parser, CPP_CLOSE_SQUARE, "expected %<]%>");
  c_parser_skip_until_found (parser, CPP_CLOSE_SQUARE, "expected %<]%>");
  return nreverse (attributes);
}

/* Look past an optional balanced token sequence of raw look-ahead
   tokens starting with the *Nth token.  *N is updated to point to the
   following token.  Return true if such a sequence was found, false
   if the tokens parsed were not balanced.  */

static bool
c_parser_check_balanced_raw_token_sequence (c_parser *parser, unsigned int *n)
{
  while (true)
    {
      c_token *token = c_parser_peek_nth_token_raw (parser, *n);
      switch (token->type)
	{
	case CPP_OPEN_BRACE:
	  {
	    ++*n;
	    if (c_parser_check_balanced_raw_token_sequence (parser, n))
	      {
		token = c_parser_peek_nth_token_raw (parser, *n);
		if (token->type == CPP_CLOSE_BRACE)
		  ++*n;
		else
		  return false;
	      }
	    else
	      return false;
	    break;
	  }

	case CPP_OPEN_PAREN:
	  {
	    ++*n;
	    if (c_parser_check_balanced_raw_token_sequence (parser, n))
	      {
		token = c_parser_peek_nth_token_raw (parser, *n);
		if (token->type == CPP_CLOSE_PAREN)
		  ++*n;
		else
		  return false;
	      }
	    else
	      return false;
	    break;
	  }

	case CPP_OPEN_SQUARE:
	  {
	    ++*n;
	    if (c_parser_check_balanced_raw_token_sequence (parser, n))
	      {
		token = c_parser_peek_nth_token_raw (parser, *n);
		if (token->type == CPP_CLOSE_SQUARE)
		  ++*n;
		else
		  return false;
	      }
	    else
	      return false;
	    break;
	  }

	case CPP_CLOSE_BRACE:
	case CPP_CLOSE_PAREN:
	case CPP_CLOSE_SQUARE:
	case CPP_EOF:
	  return true;

	default:
	  ++*n;
	  break;
	}
    }
}

/* Return whether standard attributes start with the Nth token.  */

static bool
c_parser_nth_token_starts_std_attributes (c_parser *parser, unsigned int n)
{
  if (!(c_parser_peek_nth_token (parser, n)->type == CPP_OPEN_SQUARE
	&& c_parser_peek_nth_token (parser, n + 1)->type == CPP_OPEN_SQUARE))
    return false;
  /* In C, '[[' must start attributes.  In Objective-C, we need to
     check whether '[[' is matched by ']]'.  */
  if (!c_dialect_objc ())
    return true;
  n += 2;
  if (!c_parser_check_balanced_raw_token_sequence (parser, &n))
    return false;
  c_token *token = c_parser_peek_nth_token_raw (parser, n);
  if (token->type != CPP_CLOSE_SQUARE)
    return false;
  token = c_parser_peek_nth_token_raw (parser, n + 1);
  return token->type == CPP_CLOSE_SQUARE;
}

static tree
c_parser_std_attribute_specifier_sequence (c_parser *parser)
{
  tree attributes = NULL_TREE;
  do
    {
      tree attrs = c_parser_std_attribute_specifier (parser, false);
      attributes = chainon (attributes, attrs);
    }
  while (c_parser_nth_token_starts_std_attributes (parser, 1));
  return attributes;
}

/* Parse a type name (C90 6.5.5, C99 6.7.6, C11 6.7.7).  ALIGNAS_OK
   says whether alignment specifiers are OK (only in cases that might
   be the type name of a compound literal).

   type-name:
     specifier-qualifier-list abstract-declarator[opt]
*/

struct c_type_name *
c_parser_type_name (c_parser *parser, bool alignas_ok)
{
  struct c_declspecs *specs = build_null_declspecs ();
  struct c_declarator *declarator;
  struct c_type_name *ret;
  bool dummy = false;
  c_parser_declspecs (parser, specs, false, true, true, alignas_ok, false,
		      false, true, cla_prefer_type);
  if (!specs->declspecs_seen_p)
    {
      c_parser_error (parser, "expected specifier-qualifier-list");
      return NULL;
    }
  if (specs->type != error_mark_node)
    {
      pending_xref_error ();
      finish_declspecs (specs);
    }
  declarator = c_parser_declarator (parser,
				    specs->typespec_kind != ctsk_none,
				    C_DTR_ABSTRACT, &dummy);
  if (declarator == NULL)
    return NULL;
  ret = XOBNEW (&parser_obstack, struct c_type_name);
  ret->specs = specs;
  ret->declarator = declarator;
  return ret;
}

/* Parse an initializer (C90 6.5.7, C99 6.7.8, C11 6.7.9).

   initializer:
     assignment-expression
     { initializer-list }
     { initializer-list , }

   initializer-list:
     designation[opt] initializer
     initializer-list , designation[opt] initializer

   designation:
     designator-list =

   designator-list:
     designator
     designator-list designator

   designator:
     array-designator
     . identifier

   array-designator:
     [ constant-expression ]

   GNU extensions:

   initializer:
     { }

   designation:
     array-designator
     identifier :

   array-designator:
     [ constant-expression ... constant-expression ]

   Any expression without commas is accepted in the syntax for the
   constant-expressions, with non-constant expressions rejected later.

   This function is only used for top-level initializers; for nested
   ones, see c_parser_initval.  */

static struct c_expr
c_parser_initializer (c_parser *parser)
{
  if (c_parser_next_token_is (parser, CPP_OPEN_BRACE))
    return c_parser_braced_init (parser, NULL_TREE, false, NULL);
  else
    {
      struct c_expr ret;
      location_t loc = c_parser_peek_token (parser)->location;
      ret = c_parser_expr_no_commas (parser, NULL);
      if (TREE_CODE (ret.value) != STRING_CST
	  && TREE_CODE (ret.value) != COMPOUND_LITERAL_EXPR)
	ret = convert_lvalue_to_rvalue (loc, ret, true, true);
      return ret;
    }
}

/* The location of the last comma within the current initializer list,
   or UNKNOWN_LOCATION if not within one.  */

location_t last_init_list_comma;

/* Parse a braced initializer list.  TYPE is the type specified for a
   compound literal, and NULL_TREE for other initializers and for
   nested braced lists.  NESTED_P is true for nested braced lists,
   false for the list of a compound literal or the list that is the
   top-level initializer in a declaration.  */

static struct c_expr
c_parser_braced_init (c_parser *parser, tree type, bool nested_p,
		      struct obstack *outer_obstack)
{
  struct c_expr ret;
  struct obstack braced_init_obstack;
  location_t brace_loc = c_parser_peek_token (parser)->location;
  gcc_obstack_init (&braced_init_obstack);
  gcc_assert (c_parser_next_token_is (parser, CPP_OPEN_BRACE));
  matching_braces braces;
  braces.consume_open (parser);
  if (nested_p)
    {
      finish_implicit_inits (brace_loc, outer_obstack);
      push_init_level (brace_loc, 0, &braced_init_obstack);
    }
  else
    really_start_incremental_init (type);
  if (c_parser_next_token_is (parser, CPP_CLOSE_BRACE))
    {
      pedwarn (brace_loc, OPT_Wpedantic, "ISO C forbids empty initializer braces");
    }
  else
    {
      /* Parse a non-empty initializer list, possibly with a trailing
	 comma.  */
      while (true)
	{
	  c_parser_initelt (parser, &braced_init_obstack);
	  if (parser->error)
	    break;
	  if (c_parser_next_token_is (parser, CPP_COMMA))
	    {
	      last_init_list_comma = c_parser_peek_token (parser)->location;
	      c_parser_consume_token (parser);
	    }
	  else
	    break;
	  if (c_parser_next_token_is (parser, CPP_CLOSE_BRACE))
	    break;
	}
    }
  c_token *next_tok = c_parser_peek_token (parser);
  if (next_tok->type != CPP_CLOSE_BRACE)
    {
      ret.set_error ();
      ret.original_code = ERROR_MARK;
      ret.original_type = NULL;
      braces.skip_until_found_close (parser);
      pop_init_level (brace_loc, 0, &braced_init_obstack, last_init_list_comma);
      obstack_free (&braced_init_obstack, NULL);
      return ret;
    }
  location_t close_loc = next_tok->location;
  c_parser_consume_token (parser);
  ret = pop_init_level (brace_loc, 0, &braced_init_obstack, close_loc);
  obstack_free (&braced_init_obstack, NULL);
  set_c_expr_source_range (&ret, brace_loc, close_loc);
  return ret;
}

/* Parse a nested initializer, including designators.  */

static void
c_parser_initelt (c_parser *parser, struct obstack * braced_init_obstack)
{
  /* Parse any designator or designator list.  A single array
     designator may have the subsequent "=" omitted in GNU C, but a
     longer list or a structure member designator may not.  */
  if (c_parser_next_token_is (parser, CPP_NAME)
      && c_parser_peek_2nd_token (parser)->type == CPP_COLON)
    {
      /* Old-style structure member designator.  */
      set_init_label (c_parser_peek_token (parser)->location,
		      c_parser_peek_token (parser)->value,
		      c_parser_peek_token (parser)->location,
		      braced_init_obstack);
      /* Use the colon as the error location.  */
      pedwarn (c_parser_peek_2nd_token (parser)->location, OPT_Wpedantic,
	       "obsolete use of designated initializer with %<:%>");
      c_parser_consume_token (parser);
      c_parser_consume_token (parser);
    }
  else
    {
      /* des_seen is 0 if there have been no designators, 1 if there
	 has been a single array designator and 2 otherwise.  */
      int des_seen = 0;
      /* Location of a designator.  */
      location_t des_loc = UNKNOWN_LOCATION;  /* Quiet warning.  */
      while (c_parser_next_token_is (parser, CPP_OPEN_SQUARE)
	     || c_parser_next_token_is (parser, CPP_DOT))
	{
	  int des_prev = des_seen;
	  if (!des_seen)
	    des_loc = c_parser_peek_token (parser)->location;
	  if (des_seen < 2)
	    des_seen++;
	  if (c_parser_next_token_is (parser, CPP_DOT))
	    {
	      des_seen = 2;
	      c_parser_consume_token (parser);
	      if (c_parser_next_token_is (parser, CPP_NAME))
		{
		  set_init_label (des_loc, c_parser_peek_token (parser)->value,
				  c_parser_peek_token (parser)->location,
				  braced_init_obstack);
		  c_parser_consume_token (parser);
		}
	      else
		{
		  struct c_expr init;
		  init.set_error ();
		  init.original_code = ERROR_MARK;
		  init.original_type = NULL;
		  c_parser_error (parser, "expected identifier");
		  c_parser_skip_until_found (parser, CPP_COMMA, NULL);
		  process_init_element (input_location, init, false,
					braced_init_obstack);
		  return;
		}
	    }
	  else
	    {
	      tree first, second;
	      location_t ellipsis_loc = UNKNOWN_LOCATION;  /* Quiet warning.  */
	      location_t array_index_loc = UNKNOWN_LOCATION;
	      /* ??? Following the old parser, [ objc-receiver
		 objc-message-args ] is accepted as an initializer,
		 being distinguished from a designator by what follows
		 the first assignment expression inside the square
		 brackets, but after a first array designator a
		 subsequent square bracket is for Objective-C taken to
		 start an expression, using the obsolete form of
		 designated initializer without '=', rather than
		 possibly being a second level of designation: in LALR
		 terms, the '[' is shifted rather than reducing
		 designator to designator-list.  */
	      if (des_prev == 1 && c_dialect_objc ())
		{
		  des_seen = des_prev;
		  break;
		}
	      if (des_prev == 0 && c_dialect_objc ())
		{
		  /* This might be an array designator or an
		     Objective-C message expression.  If the former,
		     continue parsing here; if the latter, parse the
		     remainder of the initializer given the starting
		     primary-expression.  ??? It might make sense to
		     distinguish when des_prev == 1 as well; see
		     previous comment.  */
		  tree rec, args;
		  struct c_expr mexpr;
		  c_parser_consume_token (parser);
		  if (c_parser_peek_token (parser)->type == CPP_NAME
		      && ((c_parser_peek_token (parser)->id_kind
			   == C_ID_TYPENAME)
			  || (c_parser_peek_token (parser)->id_kind
			      == C_ID_CLASSNAME)))
		    {
		      /* Type name receiver.  */
		      tree id = c_parser_peek_token (parser)->value;
		      c_parser_consume_token (parser);
		      rec = objc_get_class_reference (id);
		      goto parse_message_args;
		    }
		  first = c_parser_expr_no_commas (parser, NULL).value;
		  mark_exp_read (first);
		  if (c_parser_next_token_is (parser, CPP_ELLIPSIS)
		      || c_parser_next_token_is (parser, CPP_CLOSE_SQUARE))
		    goto array_desig_after_first;
		  /* Expression receiver.  So far only one part
		     without commas has been parsed; there might be
		     more of the expression.  */
		  rec = first;
		  while (c_parser_next_token_is (parser, CPP_COMMA))
		    {
		      struct c_expr next;
		      location_t comma_loc, exp_loc;
		      comma_loc = c_parser_peek_token (parser)->location;
		      c_parser_consume_token (parser);
		      exp_loc = c_parser_peek_token (parser)->location;
		      next = c_parser_expr_no_commas (parser, NULL);
		      next = convert_lvalue_to_rvalue (exp_loc, next,
						       true, true);
		      rec = build_compound_expr (comma_loc, rec, next.value);
		    }
		parse_message_args:
		  /* Now parse the objc-message-args.  */
		  args = c_parser_objc_message_args (parser);
		  c_parser_skip_until_found (parser, CPP_CLOSE_SQUARE,
					     "expected %<]%>");
		  mexpr.value
		    = objc_build_message_expr (rec, args);
		  mexpr.original_code = ERROR_MARK;
		  mexpr.original_type = NULL;
		  /* Now parse and process the remainder of the
		     initializer, starting with this message
		     expression as a primary-expression.  */
		  c_parser_initval (parser, &mexpr, braced_init_obstack);
		  return;
		}
	      c_parser_consume_token (parser);
	      array_index_loc = c_parser_peek_token (parser)->location;
	      first = c_parser_expr_no_commas (parser, NULL).value;
	      mark_exp_read (first);
	    array_desig_after_first:
	      if (c_parser_next_token_is (parser, CPP_ELLIPSIS))
		{
		  ellipsis_loc = c_parser_peek_token (parser)->location;
		  c_parser_consume_token (parser);
		  second = c_parser_expr_no_commas (parser, NULL).value;
		  mark_exp_read (second);
		}
	      else
		second = NULL_TREE;
	      if (c_parser_next_token_is (parser, CPP_CLOSE_SQUARE))
		{
		  c_parser_consume_token (parser);
		  set_init_index (array_index_loc, first, second,
				  braced_init_obstack);
		  if (second)
		    pedwarn (ellipsis_loc, OPT_Wpedantic,
			     "ISO C forbids specifying range of elements to initialize");
		}
	      else
		c_parser_skip_until_found (parser, CPP_CLOSE_SQUARE,
					   "expected %<]%>");
	    }
	}
      if (des_seen >= 1)
	{
	  if (c_parser_next_token_is (parser, CPP_EQ))
	    {
	      pedwarn_c90 (des_loc, OPT_Wpedantic,
			   "ISO C90 forbids specifying subobject "
			   "to initialize");
	      c_parser_consume_token (parser);
	    }
	  else
	    {
	      if (des_seen == 1)
		pedwarn (c_parser_peek_token (parser)->location, OPT_Wpedantic,
			 "obsolete use of designated initializer without %<=%>");
	      else
		{
		  struct c_expr init;
		  init.set_error ();
		  init.original_code = ERROR_MARK;
		  init.original_type = NULL;
		  c_parser_error (parser, "expected %<=%>");
		  c_parser_skip_until_found (parser, CPP_COMMA, NULL);
		  process_init_element (input_location, init, false,
					braced_init_obstack);
		  return;
		}
	    }
	}
    }
  c_parser_initval (parser, NULL, braced_init_obstack);
}

/* Parse a nested initializer; as c_parser_initializer but parses
   initializers within braced lists, after any designators have been
   applied.  If AFTER is not NULL then it is an Objective-C message
   expression which is the primary-expression starting the
   initializer.  */

static void
c_parser_initval (c_parser *parser, struct c_expr *after,
		  struct obstack * braced_init_obstack)
{
  struct c_expr init;
  gcc_assert (!after || c_dialect_objc ());
  location_t loc = c_parser_peek_token (parser)->location;

  if (c_parser_next_token_is (parser, CPP_OPEN_BRACE) && !after)
    init = c_parser_braced_init (parser, NULL_TREE, true,
				 braced_init_obstack);
  else
    {
      init = c_parser_expr_no_commas (parser, after);
      if (init.value != NULL_TREE
	  && TREE_CODE (init.value) != STRING_CST
	  && TREE_CODE (init.value) != COMPOUND_LITERAL_EXPR)
	init = convert_lvalue_to_rvalue (loc, init, true, true);
    }
  process_init_element (loc, init, false, braced_init_obstack);
}

/* Parse a compound statement (possibly a function body) (C90 6.6.2,
   C99 6.8.2, C11 6.8.2, C2X 6.8.2).

   compound-statement:
     { block-item-list[opt] }
     { label-declarations block-item-list }

   block-item-list:
     block-item
     block-item-list block-item

   block-item:
     label
     nested-declaration
     statement

   nested-declaration:
     declaration

   GNU extensions:

   compound-statement:
     { label-declarations block-item-list }

   nested-declaration:
     __extension__ nested-declaration
     nested-function-definition

   label-declarations:
     label-declaration
     label-declarations label-declaration

   label-declaration:
     __label__ identifier-list ;

   Allowing the mixing of declarations and code is new in C99.  The
   GNU syntax also permits (not shown above) labels at the end of
   compound statements, which yield an error.  We don't allow labels
   on declarations; this might seem like a natural extension, but
   there would be a conflict between gnu-attributes on the label and
   prefix gnu-attributes on the declaration.  ??? The syntax follows the
   old parser in requiring something after label declarations.
   Although they are erroneous if the labels declared aren't defined,
   is it useful for the syntax to be this way?

   OpenACC:

   block-item:
     openacc-directive

   openacc-directive:
     update-directive

   OpenMP:

   block-item:
     openmp-directive

   openmp-directive:
     barrier-directive
     flush-directive
     taskwait-directive
     taskyield-directive
     cancel-directive
     cancellation-point-directive  */

static tree
c_parser_compound_statement (c_parser *parser, location_t *endlocp)
{
  tree stmt;
  location_t brace_loc;
  brace_loc = c_parser_peek_token (parser)->location;
  if (!c_parser_require (parser, CPP_OPEN_BRACE, "expected %<{%>"))
    {
      /* Ensure a scope is entered and left anyway to avoid confusion
	 if we have just prepared to enter a function body.  */
      stmt = c_begin_compound_stmt (true);
      c_end_compound_stmt (brace_loc, stmt, true);
      return error_mark_node;
    }
  stmt = c_begin_compound_stmt (true);
  location_t end_loc = c_parser_compound_statement_nostart (parser);
  if (endlocp)
    *endlocp = end_loc;

  return c_end_compound_stmt (brace_loc, stmt, true);
}

/* Parse a compound statement except for the opening brace.  This is
   used for parsing both compound statements and statement expressions
   (which follow different paths to handling the opening).  */

static location_t
c_parser_compound_statement_nostart (c_parser *parser)
{
  bool last_stmt = false;
  bool last_label = false;
  bool save_valid_for_pragma = valid_location_for_stdc_pragma_p ();
  location_t label_loc = UNKNOWN_LOCATION;  /* Quiet warning.  */
  if (c_parser_next_token_is (parser, CPP_CLOSE_BRACE))
    {
      location_t endloc = c_parser_peek_token (parser)->location;
      add_debug_begin_stmt (endloc);
      c_parser_consume_token (parser);
      return endloc;
    }
  mark_valid_location_for_stdc_pragma (true);
  if (c_parser_next_token_is_keyword (parser, RID_LABEL))
    {
      /* Read zero or more forward-declarations for labels that nested
	 functions can jump to.  */
      mark_valid_location_for_stdc_pragma (false);
      while (c_parser_next_token_is_keyword (parser, RID_LABEL))
	{
	  label_loc = c_parser_peek_token (parser)->location;
	  c_parser_consume_token (parser);
	  /* Any identifiers, including those declared as type names,
	     are OK here.  */
	  while (true)
	    {
	      tree label;
	      if (c_parser_next_token_is_not (parser, CPP_NAME))
		{
		  c_parser_error (parser, "expected identifier");
		  break;
		}
	      label
		= declare_label (c_parser_peek_token (parser)->value);
	      C_DECLARED_LABEL_FLAG (label) = 1;
	      add_stmt (build_stmt (label_loc, DECL_EXPR, label));
	      c_parser_consume_token (parser);
	      if (c_parser_next_token_is (parser, CPP_COMMA))
		c_parser_consume_token (parser);
	      else
		break;
	    }
	  c_parser_skip_until_found (parser, CPP_SEMICOLON, "expected %<;%>");
	}
      pedwarn (label_loc, OPT_Wpedantic, "ISO C forbids label declarations");
    }
  /* We must now have at least one statement, label or declaration.  */
  if (c_parser_next_token_is (parser, CPP_CLOSE_BRACE))
    {
      mark_valid_location_for_stdc_pragma (save_valid_for_pragma);
      c_parser_error (parser, "expected declaration or statement");
      location_t endloc = c_parser_peek_token (parser)->location;
      c_parser_consume_token (parser);
      return endloc;
    }
  while (c_parser_next_token_is_not (parser, CPP_CLOSE_BRACE))
    {
      location_t loc = c_parser_peek_token (parser)->location;
      loc = expansion_point_location_if_in_system_header (loc);
      /* Standard attributes may start a label, statement or declaration.  */
      bool have_std_attrs
	= c_parser_nth_token_starts_std_attributes (parser, 1);
      tree std_attrs = NULL_TREE;
      if (have_std_attrs)
	std_attrs = c_parser_std_attribute_specifier_sequence (parser);
      if (c_parser_next_token_is_keyword (parser, RID_CASE)
	  || c_parser_next_token_is_keyword (parser, RID_DEFAULT)
	  || (c_parser_next_token_is (parser, CPP_NAME)
	      && c_parser_peek_2nd_token (parser)->type == CPP_COLON))
	{
	  if (c_parser_next_token_is_keyword (parser, RID_CASE))
	    label_loc = c_parser_peek_2nd_token (parser)->location;
	  else
	    label_loc = c_parser_peek_token (parser)->location;
	  last_label = true;
	  last_stmt = false;
	  mark_valid_location_for_stdc_pragma (false);
	  c_parser_label (parser, std_attrs);
	}
      else if (c_parser_next_tokens_start_declaration (parser)
	       || (have_std_attrs
		   && c_parser_next_token_is (parser, CPP_SEMICOLON)))
	{
	  if (last_label)
	    pedwarn_c11 (c_parser_peek_token (parser)->location, OPT_Wpedantic,
			 "a label can only be part of a statement and "
			 "a declaration is not a statement");

	  mark_valid_location_for_stdc_pragma (false);
	  bool fallthru_attr_p = false;
	  c_parser_declaration_or_fndef (parser, true, !have_std_attrs,
					 true, true, true, NULL,
					 vNULL, have_std_attrs, std_attrs,
					 NULL, &fallthru_attr_p);

	  if (last_stmt && !fallthru_attr_p)
	    pedwarn_c90 (loc, OPT_Wdeclaration_after_statement,
			 "ISO C90 forbids mixed declarations and code");
	  last_stmt = fallthru_attr_p;
	  last_label = false;
	}
      else if (c_parser_next_token_is_keyword (parser, RID_EXTENSION))
	{
	  /* __extension__ can start a declaration, but is also an
	     unary operator that can start an expression.  Consume all
	     but the last of a possible series of __extension__ to
	     determine which.  If standard attributes have already
	     been seen, it must start a statement, not a declaration,
	     but standard attributes starting a declaration may appear
	     after __extension__.  */
	  while (c_parser_peek_2nd_token (parser)->type == CPP_KEYWORD
		 && (c_parser_peek_2nd_token (parser)->keyword
		     == RID_EXTENSION))
	    c_parser_consume_token (parser);
	  if (!have_std_attrs
	      && (c_token_starts_declaration (c_parser_peek_2nd_token (parser))
		  || c_parser_nth_token_starts_std_attributes (parser, 2)))
	    {
	      int ext;
	      ext = disable_extension_diagnostics ();
	      c_parser_consume_token (parser);
	      last_label = false;
	      mark_valid_location_for_stdc_pragma (false);
	      c_parser_declaration_or_fndef (parser, true, true, true, true,
					     true, NULL, vNULL);
	      /* Following the old parser, __extension__ does not
		 disable this diagnostic.  */
	      restore_extension_diagnostics (ext);
	      if (last_stmt)
		pedwarn_c90 (loc, OPT_Wdeclaration_after_statement,
			     "ISO C90 forbids mixed declarations and code");
	      last_stmt = false;
	    }
	  else
	    goto statement;
	}
      else if (c_parser_next_token_is (parser, CPP_PRAGMA))
	{
	  if (have_std_attrs)
	    c_parser_error (parser, "expected declaration or statement");
	  /* External pragmas, and some omp pragmas, are not associated
	     with regular c code, and so are not to be considered statements
	     syntactically.  This ensures that the user doesn't put them
	     places that would turn into syntax errors if the directive
	     were ignored.  */
	  if (c_parser_pragma (parser,
			       last_label ? pragma_stmt : pragma_compound,
			       NULL))
	    last_label = false, last_stmt = true;
	}
      else if (c_parser_next_token_is (parser, CPP_EOF))
	{
	  mark_valid_location_for_stdc_pragma (save_valid_for_pragma);
	  c_parser_error (parser, "expected declaration or statement");
	  return c_parser_peek_token (parser)->location;
	}
      else if (c_parser_next_token_is_keyword (parser, RID_ELSE))
        {
          if (parser->in_if_block)
            {
	      mark_valid_location_for_stdc_pragma (save_valid_for_pragma);
	      error_at (loc, "expected %<}%> before %<else%>");
	      return c_parser_peek_token (parser)->location;
            }
          else
            {
              error_at (loc, "%<else%> without a previous %<if%>");
              c_parser_consume_token (parser);
              continue;
            }
        }
      else
	{
	statement:
	  c_warn_unused_attributes (std_attrs);
	  last_label = false;
	  last_stmt = true;
	  mark_valid_location_for_stdc_pragma (false);
	  c_parser_statement_after_labels (parser, NULL);
	}

      parser->error = false;
    }
  if (last_label)
    pedwarn_c11 (label_loc, OPT_Wpedantic, "label at end of compound statement");
  location_t endloc = c_parser_peek_token (parser)->location;
  c_parser_consume_token (parser);
  /* Restore the value we started with.  */
  mark_valid_location_for_stdc_pragma (save_valid_for_pragma);
  return endloc;
}

/* Parse all consecutive labels, possibly preceded by standard
   attributes.  In this context, a statement is required, not a
   declaration, so attributes must be followed by a statement that is
   not just a semicolon.  */

static void
c_parser_all_labels (c_parser *parser)
{
  tree std_attrs = NULL;
  if (c_parser_nth_token_starts_std_attributes (parser, 1))
    {
      std_attrs = c_parser_std_attribute_specifier_sequence (parser);
      if (c_parser_next_token_is (parser, CPP_SEMICOLON))
	c_parser_error (parser, "expected statement");
    }
  while (c_parser_next_token_is_keyword (parser, RID_CASE)
	 || c_parser_next_token_is_keyword (parser, RID_DEFAULT)
	 || (c_parser_next_token_is (parser, CPP_NAME)
	     && c_parser_peek_2nd_token (parser)->type == CPP_COLON))
    {
      c_parser_label (parser, std_attrs);
      std_attrs = NULL;
      if (c_parser_nth_token_starts_std_attributes (parser, 1))
	{
	  std_attrs = c_parser_std_attribute_specifier_sequence (parser);
	  if (c_parser_next_token_is (parser, CPP_SEMICOLON))
	    c_parser_error (parser, "expected statement");
	}
    }
   if (std_attrs)
     c_warn_unused_attributes (std_attrs);
}

/* Parse a label (C90 6.6.1, C99 6.8.1, C11 6.8.1).

   label:
     identifier : gnu-attributes[opt]
     case constant-expression :
     default :

   GNU extensions:

   label:
     case constant-expression ... constant-expression :

   The use of gnu-attributes on labels is a GNU extension.  The syntax in
   GNU C accepts any expressions without commas, non-constant
   expressions being rejected later.  Any standard
   attribute-specifier-sequence before the first label has been parsed
   in the caller, to distinguish statements from declarations.  Any
   attribute-specifier-sequence after the label is parsed in this
   function.  */
static void
c_parser_label (c_parser *parser, tree std_attrs)
{
  location_t loc1 = c_parser_peek_token (parser)->location;
  tree label = NULL_TREE;

  /* Remember whether this case or a user-defined label is allowed to fall
     through to.  */
  bool fallthrough_p = c_parser_peek_token (parser)->flags & PREV_FALLTHROUGH;

  if (c_parser_next_token_is_keyword (parser, RID_CASE))
    {
      tree exp1, exp2;
      c_parser_consume_token (parser);
      exp1 = c_parser_expr_no_commas (parser, NULL).value;
      if (c_parser_next_token_is (parser, CPP_COLON))
	{
	  c_parser_consume_token (parser);
	  label = do_case (loc1, exp1, NULL_TREE);
	}
      else if (c_parser_next_token_is (parser, CPP_ELLIPSIS))
	{
	  c_parser_consume_token (parser);
	  exp2 = c_parser_expr_no_commas (parser, NULL).value;
	  if (c_parser_require (parser, CPP_COLON, "expected %<:%>"))
	    label = do_case (loc1, exp1, exp2);
	}
      else
	c_parser_error (parser, "expected %<:%> or %<...%>");
    }
  else if (c_parser_next_token_is_keyword (parser, RID_DEFAULT))
    {
      c_parser_consume_token (parser);
      if (c_parser_require (parser, CPP_COLON, "expected %<:%>"))
	label = do_case (loc1, NULL_TREE, NULL_TREE);
    }
  else
    {
      tree name = c_parser_peek_token (parser)->value;
      tree tlab;
      tree attrs;
      location_t loc2 = c_parser_peek_token (parser)->location;
      gcc_assert (c_parser_next_token_is (parser, CPP_NAME));
      c_parser_consume_token (parser);
      gcc_assert (c_parser_next_token_is (parser, CPP_COLON));
      c_parser_consume_token (parser);
      attrs = c_parser_gnu_attributes (parser);
      tlab = define_label (loc2, name);
      if (tlab)
	{
	  decl_attributes (&tlab, attrs, 0);
	  decl_attributes (&tlab, std_attrs, 0);
	  label = add_stmt (build_stmt (loc1, LABEL_EXPR, tlab));
	}
      if (attrs
	  && c_parser_next_tokens_start_declaration (parser))
	  warning_at (loc2, OPT_Wattributes, "GNU-style attribute between"
		      " label and declaration appertains to the label");
    }
  if (label)
    {
      if (TREE_CODE (label) == LABEL_EXPR)
	FALLTHROUGH_LABEL_P (LABEL_EXPR_LABEL (label)) = fallthrough_p;
      else
	FALLTHROUGH_LABEL_P (CASE_LABEL (label)) = fallthrough_p;
    }
}

/* Parse a statement (C90 6.6, C99 6.8, C11 6.8).

   statement:
     labeled-statement
     attribute-specifier-sequence[opt] compound-statement
     expression-statement
     attribute-specifier-sequence[opt] selection-statement
     attribute-specifier-sequence[opt] iteration-statement
     attribute-specifier-sequence[opt] jump-statement

   labeled-statement:
     attribute-specifier-sequence[opt] label statement

   expression-statement:
     expression[opt] ;
     attribute-specifier-sequence expression ;

   selection-statement:
     if-statement
     switch-statement

   iteration-statement:
     while-statement
     do-statement
     for-statement

   jump-statement:
     goto identifier ;
     continue ;
     break ;
     return expression[opt] ;

   GNU extensions:

   statement:
     attribute-specifier-sequence[opt] asm-statement

   jump-statement:
     goto * expression ;

   expression-statement:
     gnu-attributes ;

   Objective-C:

   statement:
     attribute-specifier-sequence[opt] objc-throw-statement
     attribute-specifier-sequence[opt] objc-try-catch-statement
     attribute-specifier-sequence[opt] objc-synchronized-statement

   objc-throw-statement:
     @throw expression ;
     @throw ;

   OpenACC:

   statement:
     attribute-specifier-sequence[opt] openacc-construct

   openacc-construct:
     parallel-construct
     kernels-construct
     data-construct
     loop-construct

   parallel-construct:
     parallel-directive structured-block

   kernels-construct:
     kernels-directive structured-block

   data-construct:
     data-directive structured-block

   loop-construct:
     loop-directive structured-block

   OpenMP:

   statement:
     attribute-specifier-sequence[opt] openmp-construct

   openmp-construct:
     parallel-construct
     for-construct
     simd-construct
     for-simd-construct
     sections-construct
     single-construct
     parallel-for-construct
     parallel-for-simd-construct
     parallel-sections-construct
     master-construct
     critical-construct
     atomic-construct
     ordered-construct

   parallel-construct:
     parallel-directive structured-block

   for-construct:
     for-directive iteration-statement

   simd-construct:
     simd-directive iteration-statements

   for-simd-construct:
     for-simd-directive iteration-statements

   sections-construct:
     sections-directive section-scope

   single-construct:
     single-directive structured-block

   parallel-for-construct:
     parallel-for-directive iteration-statement

   parallel-for-simd-construct:
     parallel-for-simd-directive iteration-statement

   parallel-sections-construct:
     parallel-sections-directive section-scope

   master-construct:
     master-directive structured-block

   critical-construct:
     critical-directive structured-block

   atomic-construct:
     atomic-directive expression-statement

   ordered-construct:
     ordered-directive structured-block

   Transactional Memory:

   statement:
     attribute-specifier-sequence[opt] transaction-statement
     attribute-specifier-sequence[opt] transaction-cancel-statement

   IF_P is used to track whether there's a (possibly labeled) if statement
   which is not enclosed in braces and has an else clause.  This is used to
   implement -Wparentheses.  */

static void
c_parser_statement (c_parser *parser, bool *if_p, location_t *loc_after_labels)
{
  c_parser_all_labels (parser);
  if (loc_after_labels)
    *loc_after_labels = c_parser_peek_token (parser)->location;
  c_parser_statement_after_labels (parser, if_p, NULL);
}

/* Parse a statement, other than a labeled statement.  CHAIN is a vector
   of if-else-if conditions.  All labels and standard attributes have
   been parsed in the caller.

   IF_P is used to track whether there's a (possibly labeled) if statement
   which is not enclosed in braces and has an else clause.  This is used to
   implement -Wparentheses.  */

static void
c_parser_statement_after_labels (c_parser *parser, bool *if_p,
				 vec<tree> *chain)
{
  location_t loc = c_parser_peek_token (parser)->location;
  tree stmt = NULL_TREE;
  bool in_if_block = parser->in_if_block;
  parser->in_if_block = false;
  if (if_p != NULL)
    *if_p = false;

  if (c_parser_peek_token (parser)->type != CPP_OPEN_BRACE)
    add_debug_begin_stmt (loc);

  switch (c_parser_peek_token (parser)->type)
    {
    case CPP_OPEN_BRACE:
      add_stmt (c_parser_compound_statement (parser));
      break;
    case CPP_KEYWORD:
      switch (c_parser_peek_token (parser)->keyword)
	{
	case RID_IF:
	  c_parser_if_statement (parser, if_p, chain);
	  break;
	case RID_SWITCH:
	  c_parser_switch_statement (parser, if_p);
	  break;
	case RID_WHILE:
	  c_parser_while_statement (parser, false, 0, if_p);
	  break;
	case RID_DO:
	  c_parser_do_statement (parser, false, 0);
	  break;
	case RID_FOR:
	  c_parser_for_statement (parser, false, 0, if_p);
	  break;
	case RID_GOTO:
	  c_parser_consume_token (parser);
	  if (c_parser_next_token_is (parser, CPP_NAME))
	    {
	      stmt = c_finish_goto_label (loc,
					  c_parser_peek_token (parser)->value);
	      c_parser_consume_token (parser);
	    }
	  else if (c_parser_next_token_is (parser, CPP_MULT))
	    {
	      struct c_expr val;

	      c_parser_consume_token (parser);
	      val = c_parser_expression (parser);
	      val = convert_lvalue_to_rvalue (loc, val, false, true);
	      stmt = c_finish_goto_ptr (loc, val.value);
	    }
	  else
	    c_parser_error (parser, "expected identifier or %<*%>");
	  goto expect_semicolon;
	case RID_CONTINUE:
	  c_parser_consume_token (parser);
	  stmt = c_finish_bc_stmt (loc, objc_foreach_continue_label, false);
	  goto expect_semicolon;
	case RID_BREAK:
	  c_parser_consume_token (parser);
	  stmt = c_finish_bc_stmt (loc, objc_foreach_break_label, true);
	  goto expect_semicolon;
	case RID_RETURN:
	  c_parser_consume_token (parser);
	  if (c_parser_next_token_is (parser, CPP_SEMICOLON))
	    {
	      stmt = c_finish_return (loc, NULL_TREE, NULL_TREE);
	      c_parser_consume_token (parser);
	    }
	  else
	    {
	      location_t xloc = c_parser_peek_token (parser)->location;
	      struct c_expr expr = c_parser_expression_conv (parser);
	      mark_exp_read (expr.value);
	      stmt = c_finish_return (EXPR_LOC_OR_LOC (expr.value, xloc),
				      expr.value, expr.original_type);
	      goto expect_semicolon;
	    }
	  break;
	case RID_ASM:
	  stmt = c_parser_asm_statement (parser);
	  break;
	case RID_TRANSACTION_ATOMIC:
	case RID_TRANSACTION_RELAXED:
	  stmt = c_parser_transaction (parser,
	      c_parser_peek_token (parser)->keyword);
	  break;
	case RID_TRANSACTION_CANCEL:
	  stmt = c_parser_transaction_cancel (parser);
	  goto expect_semicolon;
	case RID_AT_THROW:
	  gcc_assert (c_dialect_objc ());
	  c_parser_consume_token (parser);
	  if (c_parser_next_token_is (parser, CPP_SEMICOLON))
	    {
	      stmt = objc_build_throw_stmt (loc, NULL_TREE);
	      c_parser_consume_token (parser);
	    }
	  else
	    {
	      struct c_expr expr = c_parser_expression (parser);
	      expr = convert_lvalue_to_rvalue (loc, expr, false, false);
	      expr.value = c_fully_fold (expr.value, false, NULL);
	      stmt = objc_build_throw_stmt (loc, expr.value);
	      goto expect_semicolon;
	    }
	  break;
	case RID_AT_TRY:
	  gcc_assert (c_dialect_objc ());
	  c_parser_objc_try_catch_finally_statement (parser);
	  break;
	case RID_AT_SYNCHRONIZED:
	  gcc_assert (c_dialect_objc ());
	  c_parser_objc_synchronized_statement (parser);
	  break;
	case RID_ATTRIBUTE:
	  {
	    /* Allow '__attribute__((fallthrough));'.  */
	    tree attrs = c_parser_gnu_attributes (parser);
	    if (attribute_fallthrough_p (attrs))
	      {
		if (c_parser_next_token_is (parser, CPP_SEMICOLON))
		  {
		    tree fn = build_call_expr_internal_loc (loc,
							    IFN_FALLTHROUGH,
							    void_type_node, 0);
		    add_stmt (fn);
		    /* Eat the ';'.  */
		    c_parser_consume_token (parser);
		  }
		else
		  warning_at (loc, OPT_Wattributes,
			      "%<fallthrough%> attribute not followed "
			      "by %<;%>");
	      }
	    else if (attrs != NULL_TREE)
	      warning_at (loc, OPT_Wattributes, "only attribute %<fallthrough%>"
			  " can be applied to a null statement");
	    break;
	  }
	default:
	  goto expr_stmt;
	}
      break;
    case CPP_SEMICOLON:
      c_parser_consume_token (parser);
      break;
    case CPP_CLOSE_PAREN:
    case CPP_CLOSE_SQUARE:
      /* Avoid infinite loop in error recovery:
	 c_parser_skip_until_found stops at a closing nesting
	 delimiter without consuming it, but here we need to consume
	 it to proceed further.  */
      c_parser_error (parser, "expected statement");
      c_parser_consume_token (parser);
      break;
    case CPP_PRAGMA:
      c_parser_pragma (parser, pragma_stmt, if_p);
      break;
    default:
    expr_stmt:
      stmt = c_finish_expr_stmt (loc, c_parser_expression_conv (parser).value);
    expect_semicolon:
      c_parser_skip_until_found (parser, CPP_SEMICOLON, "expected %<;%>");
      break;
    }
  /* Two cases cannot and do not have line numbers associated: If stmt
     is degenerate, such as "2;", then stmt is an INTEGER_CST, which
     cannot hold line numbers.  But that's OK because the statement
     will either be changed to a MODIFY_EXPR during gimplification of
     the statement expr, or discarded.  If stmt was compound, but
     without new variables, we will have skipped the creation of a
     BIND and will have a bare STATEMENT_LIST.  But that's OK because
     (recursively) all of the component statements should already have
     line numbers assigned.  ??? Can we discard no-op statements
     earlier?  */
  if (EXPR_LOCATION (stmt) == UNKNOWN_LOCATION)
    protected_set_expr_location (stmt, loc);

  parser->in_if_block = in_if_block;
}

/* Parse the condition from an if, do, while or for statements.  */

static tree
c_parser_condition (c_parser *parser)
{
  location_t loc = c_parser_peek_token (parser)->location;
  tree cond;
  cond = c_parser_expression_conv (parser).value;
  cond = c_objc_common_truthvalue_conversion (loc, cond);
  cond = c_fully_fold (cond, false, NULL);
  if (warn_sequence_point)
    verify_sequence_points (cond);
  return cond;
}

/* Parse a parenthesized condition from an if, do or while statement.

   condition:
     ( expression )
*/
static tree
c_parser_paren_condition (c_parser *parser)
{
  tree cond;
  matching_parens parens;
  if (!parens.require_open (parser))
    return error_mark_node;
  cond = c_parser_condition (parser);
  parens.skip_until_found_close (parser);
  return cond;
}

/* Parse a statement which is a block in C99.

   IF_P is used to track whether there's a (possibly labeled) if statement
   which is not enclosed in braces and has an else clause.  This is used to
   implement -Wparentheses.  */

static tree
c_parser_c99_block_statement (c_parser *parser, bool *if_p,
			      location_t *loc_after_labels)
{
  tree block = c_begin_compound_stmt (flag_isoc99);
  location_t loc = c_parser_peek_token (parser)->location;
  c_parser_statement (parser, if_p, loc_after_labels);
  return c_end_compound_stmt (loc, block, flag_isoc99);
}

/* Parse the body of an if statement.  This is just parsing a
   statement but (a) it is a block in C99, (b) we track whether the
   body is an if statement for the sake of -Wparentheses warnings, (c)
   we handle an empty body specially for the sake of -Wempty-body
   warnings, and (d) we call parser_compound_statement directly
   because c_parser_statement_after_labels resets
   parser->in_if_block.

   IF_P is used to track whether there's a (possibly labeled) if statement
   which is not enclosed in braces and has an else clause.  This is used to
   implement -Wparentheses.  */

static tree
c_parser_if_body (c_parser *parser, bool *if_p,
		  const token_indent_info &if_tinfo)
{
  tree block = c_begin_compound_stmt (flag_isoc99);
  location_t body_loc = c_parser_peek_token (parser)->location;
  location_t body_loc_after_labels = UNKNOWN_LOCATION;
  token_indent_info body_tinfo
    = get_token_indent_info (c_parser_peek_token (parser));

  c_parser_all_labels (parser);
  if (c_parser_next_token_is (parser, CPP_SEMICOLON))
    {
      location_t loc = c_parser_peek_token (parser)->location;
      add_stmt (build_empty_stmt (loc));
      c_parser_consume_token (parser);
      if (!c_parser_next_token_is_keyword (parser, RID_ELSE))
	warning_at (loc, OPT_Wempty_body,
		    "suggest braces around empty body in an %<if%> statement");
    }
  else if (c_parser_next_token_is (parser, CPP_OPEN_BRACE))
    add_stmt (c_parser_compound_statement (parser));
  else
    {
      body_loc_after_labels = c_parser_peek_token (parser)->location;
      c_parser_statement_after_labels (parser, if_p);
    }

  token_indent_info next_tinfo
    = get_token_indent_info (c_parser_peek_token (parser));
  warn_for_misleading_indentation (if_tinfo, body_tinfo, next_tinfo);
  if (body_loc_after_labels != UNKNOWN_LOCATION
      && next_tinfo.type != CPP_SEMICOLON)
    warn_for_multistatement_macros (body_loc_after_labels, next_tinfo.location,
				    if_tinfo.location, RID_IF);

  return c_end_compound_stmt (body_loc, block, flag_isoc99);
}

/* Parse the else body of an if statement.  This is just parsing a
   statement but (a) it is a block in C99, (b) we handle an empty body
   specially for the sake of -Wempty-body warnings.  CHAIN is a vector
   of if-else-if conditions.  */

static tree
c_parser_else_body (c_parser *parser, const token_indent_info &else_tinfo,
		    vec<tree> *chain)
{
  location_t body_loc = c_parser_peek_token (parser)->location;
  tree block = c_begin_compound_stmt (flag_isoc99);
  token_indent_info body_tinfo
    = get_token_indent_info (c_parser_peek_token (parser));
  location_t body_loc_after_labels = UNKNOWN_LOCATION;

  c_parser_all_labels (parser);
  if (c_parser_next_token_is (parser, CPP_SEMICOLON))
    {
      location_t loc = c_parser_peek_token (parser)->location;
      warning_at (loc,
		  OPT_Wempty_body,
	         "suggest braces around empty body in an %<else%> statement");
      add_stmt (build_empty_stmt (loc));
      c_parser_consume_token (parser);
    }
  else
    {
      if (!c_parser_next_token_is (parser, CPP_OPEN_BRACE))
	body_loc_after_labels = c_parser_peek_token (parser)->location;
      c_parser_statement_after_labels (parser, NULL, chain);
    }

  token_indent_info next_tinfo
    = get_token_indent_info (c_parser_peek_token (parser));
  warn_for_misleading_indentation (else_tinfo, body_tinfo, next_tinfo);
  if (body_loc_after_labels != UNKNOWN_LOCATION
      && next_tinfo.type != CPP_SEMICOLON)
    warn_for_multistatement_macros (body_loc_after_labels, next_tinfo.location,
				    else_tinfo.location, RID_ELSE);

  return c_end_compound_stmt (body_loc, block, flag_isoc99);
}

/* We might need to reclassify any previously-lexed identifier, e.g.
   when we've left a for loop with an if-statement without else in the
   body - we might have used a wrong scope for the token.  See PR67784.  */

static void
c_parser_maybe_reclassify_token (c_parser *parser)
{
  if (c_parser_next_token_is (parser, CPP_NAME))
    {
      c_token *token = c_parser_peek_token (parser);

      if (token->id_kind != C_ID_CLASSNAME)
	{
	  tree decl = lookup_name (token->value);

	  token->id_kind = C_ID_ID;
	  if (decl)
	    {
	      if (TREE_CODE (decl) == TYPE_DECL)
		token->id_kind = C_ID_TYPENAME;
	    }
	  else if (c_dialect_objc ())
	    {
	      tree objc_interface_decl = objc_is_class_name (token->value);
	      /* Objective-C class names are in the same namespace as
		 variables and typedefs, and hence are shadowed by local
		 declarations.  */
	      if (objc_interface_decl)
		{
		  token->value = objc_interface_decl;
		  token->id_kind = C_ID_CLASSNAME;
		}
	    }
	}
    }
}

/* Parse an if statement (C90 6.6.4, C99 6.8.4, C11 6.8.4).

   if-statement:
     if ( expression ) statement
     if ( expression ) statement else statement

   CHAIN is a vector of if-else-if conditions.
   IF_P is used to track whether there's a (possibly labeled) if statement
   which is not enclosed in braces and has an else clause.  This is used to
   implement -Wparentheses.  */

static void
c_parser_if_statement (c_parser *parser, bool *if_p, vec<tree> *chain)
{
  tree block;
  location_t loc;
  tree cond;
  bool nested_if = false;
  tree first_body, second_body;
  bool in_if_block;

  gcc_assert (c_parser_next_token_is_keyword (parser, RID_IF));
  token_indent_info if_tinfo
    = get_token_indent_info (c_parser_peek_token (parser));
  c_parser_consume_token (parser);
  block = c_begin_compound_stmt (flag_isoc99);
  loc = c_parser_peek_token (parser)->location;
  cond = c_parser_paren_condition (parser);
  in_if_block = parser->in_if_block;
  parser->in_if_block = true;
  first_body = c_parser_if_body (parser, &nested_if, if_tinfo);
  parser->in_if_block = in_if_block;

  if (warn_duplicated_cond)
    warn_duplicated_cond_add_or_warn (EXPR_LOCATION (cond), cond, &chain);

  if (c_parser_next_token_is_keyword (parser, RID_ELSE))
    {
      token_indent_info else_tinfo
	= get_token_indent_info (c_parser_peek_token (parser));
      c_parser_consume_token (parser);
      if (warn_duplicated_cond)
	{
	  if (c_parser_next_token_is_keyword (parser, RID_IF)
	      && chain == NULL)
	    {
	      /* We've got "if (COND) else if (COND2)".  Start the
		 condition chain and add COND as the first element.  */
	      chain = new vec<tree> ();
	      if (!CONSTANT_CLASS_P (cond) && !TREE_SIDE_EFFECTS (cond))
		chain->safe_push (cond);
	    }
	  else if (!c_parser_next_token_is_keyword (parser, RID_IF))
	    /* This is if-else without subsequent if.  Zap the condition
	       chain; we would have already warned at this point.  */
	    vec_free (chain);
	}
      second_body = c_parser_else_body (parser, else_tinfo, chain);
      /* Set IF_P to true to indicate that this if statement has an
	 else clause.  This may trigger the Wparentheses warning
	 below when we get back up to the parent if statement.  */
      if (if_p != NULL)
	*if_p = true;
    }
  else
    {
      second_body = NULL_TREE;

      /* Diagnose an ambiguous else if if-then-else is nested inside
	 if-then.  */
      if (nested_if)
	warning_at (loc, OPT_Wdangling_else,
		    "suggest explicit braces to avoid ambiguous %<else%>");

      if (warn_duplicated_cond)
	/* This if statement does not have an else clause.  We don't
	   need the condition chain anymore.  */
	vec_free (chain);
    }
  c_finish_if_stmt (loc, cond, first_body, second_body);
  add_stmt (c_end_compound_stmt (loc, block, flag_isoc99));

  c_parser_maybe_reclassify_token (parser);
}

/* Parse a switch statement (C90 6.6.4, C99 6.8.4, C11 6.8.4).

   switch-statement:
     switch (expression) statement
*/

static void
c_parser_switch_statement (c_parser *parser, bool *if_p)
{
  struct c_expr ce;
  tree block, expr, body;
  unsigned char save_in_statement;
  location_t switch_loc = c_parser_peek_token (parser)->location;
  location_t switch_cond_loc;
  gcc_assert (c_parser_next_token_is_keyword (parser, RID_SWITCH));
  c_parser_consume_token (parser);
  block = c_begin_compound_stmt (flag_isoc99);
  bool explicit_cast_p = false;
  matching_parens parens;
  if (parens.require_open (parser))
    {
      switch_cond_loc = c_parser_peek_token (parser)->location;
      if (c_parser_next_token_is (parser, CPP_OPEN_PAREN)
	  && c_token_starts_typename (c_parser_peek_2nd_token (parser)))
	explicit_cast_p = true;
      ce = c_parser_expression (parser);
      ce = convert_lvalue_to_rvalue (switch_cond_loc, ce, true, true);
      expr = ce.value;
      /* ??? expr has no valid location?  */
      parens.skip_until_found_close (parser);
    }
  else
    {
      switch_cond_loc = UNKNOWN_LOCATION;
      expr = error_mark_node;
      ce.original_type = error_mark_node;
    }
  c_start_switch (switch_loc, switch_cond_loc, expr, explicit_cast_p);
  save_in_statement = in_statement;
  in_statement |= IN_SWITCH_STMT;
  location_t loc_after_labels;
  bool open_brace_p = c_parser_peek_token (parser)->type == CPP_OPEN_BRACE;
  body = c_parser_c99_block_statement (parser, if_p, &loc_after_labels);
  location_t next_loc = c_parser_peek_token (parser)->location;
  if (!open_brace_p && c_parser_peek_token (parser)->type != CPP_SEMICOLON)
    warn_for_multistatement_macros (loc_after_labels, next_loc, switch_loc,
				    RID_SWITCH);
  c_finish_switch (body, ce.original_type);
  in_statement = save_in_statement;
  add_stmt (c_end_compound_stmt (switch_loc, block, flag_isoc99));
  c_parser_maybe_reclassify_token (parser);
}

/* Parse a while statement (C90 6.6.5, C99 6.8.5, C11 6.8.5).

   while-statement:
      while (expression) statement

   IF_P is used to track whether there's a (possibly labeled) if statement
   which is not enclosed in braces and has an else clause.  This is used to
   implement -Wparentheses.  */

static void
c_parser_while_statement (c_parser *parser, bool ivdep, unsigned short unroll,
			  bool *if_p)
{
  tree block, cond, body;
  unsigned char save_in_statement;
  location_t loc;
  gcc_assert (c_parser_next_token_is_keyword (parser, RID_WHILE));
  token_indent_info while_tinfo
    = get_token_indent_info (c_parser_peek_token (parser));
  c_parser_consume_token (parser);
  block = c_begin_compound_stmt (flag_isoc99);
  loc = c_parser_peek_token (parser)->location;
  cond = c_parser_paren_condition (parser);
  if (ivdep && cond != error_mark_node)
    cond = build3 (ANNOTATE_EXPR, TREE_TYPE (cond), cond,
		   build_int_cst (integer_type_node,
				  annot_expr_ivdep_kind),
		   integer_zero_node);
  if (unroll && cond != error_mark_node)
    cond = build3 (ANNOTATE_EXPR, TREE_TYPE (cond), cond,
		   build_int_cst (integer_type_node,
				  annot_expr_unroll_kind),
		   build_int_cst (integer_type_node, unroll));
  save_in_statement = in_statement;
  in_statement = IN_ITERATION_STMT;

  token_indent_info body_tinfo
    = get_token_indent_info (c_parser_peek_token (parser));

  location_t loc_after_labels;
  bool open_brace = c_parser_next_token_is (parser, CPP_OPEN_BRACE);
  body = c_parser_c99_block_statement (parser, if_p, &loc_after_labels);
  add_stmt (build_stmt (loc, WHILE_STMT, cond, body));
  add_stmt (c_end_compound_stmt (loc, block, flag_isoc99));
  c_parser_maybe_reclassify_token (parser);

  token_indent_info next_tinfo
    = get_token_indent_info (c_parser_peek_token (parser));
  warn_for_misleading_indentation (while_tinfo, body_tinfo, next_tinfo);

  if (next_tinfo.type != CPP_SEMICOLON && !open_brace)
    warn_for_multistatement_macros (loc_after_labels, next_tinfo.location,
				    while_tinfo.location, RID_WHILE);

  in_statement = save_in_statement;
}

/* Parse a do statement (C90 6.6.5, C99 6.8.5, C11 6.8.5).

   do-statement:
     do statement while ( expression ) ;
*/

static void
c_parser_do_statement (c_parser *parser, bool ivdep, unsigned short unroll)
{
  tree block, cond, body;
  unsigned char save_in_statement;
  location_t loc;
  gcc_assert (c_parser_next_token_is_keyword (parser, RID_DO));
  c_parser_consume_token (parser);
  if (c_parser_next_token_is (parser, CPP_SEMICOLON))
    warning_at (c_parser_peek_token (parser)->location,
		OPT_Wempty_body,
		"suggest braces around empty body in %<do%> statement");
  block = c_begin_compound_stmt (flag_isoc99);
  loc = c_parser_peek_token (parser)->location;
  save_in_statement = in_statement;
  in_statement = IN_ITERATION_STMT;
  body = c_parser_c99_block_statement (parser, NULL);
  c_parser_require_keyword (parser, RID_WHILE, "expected %<while%>");
  in_statement = save_in_statement;
  cond = c_parser_paren_condition (parser);
  if (ivdep && cond != error_mark_node)
    cond = build3 (ANNOTATE_EXPR, TREE_TYPE (cond), cond,
		   build_int_cst (integer_type_node,
				  annot_expr_ivdep_kind),
		   integer_zero_node);
  if (unroll && cond != error_mark_node)
    cond = build3 (ANNOTATE_EXPR, TREE_TYPE (cond), cond,
		   build_int_cst (integer_type_node,
				  annot_expr_unroll_kind),
 		   build_int_cst (integer_type_node, unroll));
  if (!c_parser_require (parser, CPP_SEMICOLON, "expected %<;%>"))
    c_parser_skip_to_end_of_block_or_statement (parser);

  add_stmt (build_stmt (loc, DO_STMT, cond, body));
  add_stmt (c_end_compound_stmt (loc, block, flag_isoc99));
}

/* Parse a for statement (C90 6.6.5, C99 6.8.5, C11 6.8.5).

   for-statement:
     for ( expression[opt] ; expression[opt] ; expression[opt] ) statement
     for ( nested-declaration expression[opt] ; expression[opt] ) statement

   The form with a declaration is new in C99.

   ??? In accordance with the old parser, the declaration may be a
   nested function, which is then rejected in check_for_loop_decls,
   but does it make any sense for this to be included in the grammar?
   Note in particular that the nested function does not include a
   trailing ';', whereas the "declaration" production includes one.
   Also, can we reject bad declarations earlier and cheaper than
   check_for_loop_decls?

   In Objective-C, there are two additional variants:

   foreach-statement:
     for ( expression in expresssion ) statement
     for ( declaration in expression ) statement

   This is inconsistent with C, because the second variant is allowed
   even if c99 is not enabled.

   The rest of the comment documents these Objective-C foreach-statement.

   Here is the canonical example of the first variant:
    for (object in array)    { do something with object }
   we call the first expression ("object") the "object_expression" and 
   the second expression ("array") the "collection_expression".
   object_expression must be an lvalue of type "id" (a generic Objective-C
   object) because the loop works by assigning to object_expression the
   various objects from the collection_expression.  collection_expression
   must evaluate to something of type "id" which responds to the method
   countByEnumeratingWithState:objects:count:.

   The canonical example of the second variant is:
    for (id object in array)    { do something with object }
   which is completely equivalent to
    {
      id object;
      for (object in array) { do something with object }
    }
   Note that initizializing 'object' in some way (eg, "for ((object =
   xxx) in array) { do something with object }") is possibly
   technically valid, but completely pointless as 'object' will be
   assigned to something else as soon as the loop starts.  We should
   most likely reject it (TODO).

   The beginning of the Objective-C foreach-statement looks exactly
   like the beginning of the for-statement, and we can tell it is a
   foreach-statement only because the initial declaration or
   expression is terminated by 'in' instead of ';'.

   IF_P is used to track whether there's a (possibly labeled) if statement
   which is not enclosed in braces and has an else clause.  This is used to
   implement -Wparentheses.  */

static void
c_parser_for_statement (c_parser *parser, bool ivdep, unsigned short unroll,
			bool *if_p)
{
  tree block, cond, incr, body;
  unsigned char save_in_statement;
  tree save_objc_foreach_break_label, save_objc_foreach_continue_label;
  /* The following are only used when parsing an ObjC foreach statement.  */
  tree object_expression;
  /* Silence the bogus uninitialized warning.  */
  tree collection_expression = NULL;
  location_t loc = c_parser_peek_token (parser)->location;
  location_t for_loc = loc;
  bool is_foreach_statement = false;
  gcc_assert (c_parser_next_token_is_keyword (parser, RID_FOR));
  token_indent_info for_tinfo
    = get_token_indent_info (c_parser_peek_token (parser));
  c_parser_consume_token (parser);
  /* Open a compound statement in Objective-C as well, just in case this is
     as foreach expression.  */
  block = c_begin_compound_stmt (flag_isoc99 || c_dialect_objc ());
  cond = error_mark_node;
  incr = error_mark_node;
  matching_parens parens;
  if (parens.require_open (parser))
    {
      /* Parse the initialization declaration or expression.  */
      object_expression = error_mark_node;
      parser->objc_could_be_foreach_context = c_dialect_objc ();
      if (c_parser_next_token_is (parser, CPP_SEMICOLON))
	{
	  parser->objc_could_be_foreach_context = false;
	  c_parser_consume_token (parser);
	  c_finish_expr_stmt (loc, NULL_TREE);
	}
      else if (c_parser_next_tokens_start_declaration (parser)
	       || c_parser_nth_token_starts_std_attributes (parser, 1))
	{
	  c_parser_declaration_or_fndef (parser, true, true, true, true, true, 
					 &object_expression, vNULL);
	  parser->objc_could_be_foreach_context = false;
	  
	  if (c_parser_next_token_is_keyword (parser, RID_IN))
	    {
	      c_parser_consume_token (parser);
	      is_foreach_statement = true;
	      if (check_for_loop_decls (for_loc, true) == NULL_TREE)
		c_parser_error (parser, "multiple iterating variables in "
					"fast enumeration");
	    }
	  else
	    check_for_loop_decls (for_loc, flag_isoc99);
	}
      else if (c_parser_next_token_is_keyword (parser, RID_EXTENSION))
	{
	  /* __extension__ can start a declaration, but is also an
	     unary operator that can start an expression.  Consume all
	     but the last of a possible series of __extension__ to
	     determine which.  */
	  while (c_parser_peek_2nd_token (parser)->type == CPP_KEYWORD
		 && (c_parser_peek_2nd_token (parser)->keyword
		     == RID_EXTENSION))
	    c_parser_consume_token (parser);
	  if (c_token_starts_declaration (c_parser_peek_2nd_token (parser))
	      || c_parser_nth_token_starts_std_attributes (parser, 2))
	    {
	      int ext;
	      ext = disable_extension_diagnostics ();
	      c_parser_consume_token (parser);
	      c_parser_declaration_or_fndef (parser, true, true, true, true,
					     true, &object_expression, vNULL);
	      parser->objc_could_be_foreach_context = false;
	      
	      restore_extension_diagnostics (ext);
	      if (c_parser_next_token_is_keyword (parser, RID_IN))
		{
		  c_parser_consume_token (parser);
		  is_foreach_statement = true;
		  if (check_for_loop_decls (for_loc, true) == NULL_TREE)
		    c_parser_error (parser, "multiple iterating variables in "
					    "fast enumeration");
		}
	      else
		check_for_loop_decls (for_loc, flag_isoc99);
	    }
	  else
	    goto init_expr;
	}
      else
	{
	init_expr:
	  {
	    struct c_expr ce;
	    tree init_expression;
	    ce = c_parser_expression (parser);
	    init_expression = ce.value;
	    parser->objc_could_be_foreach_context = false;
	    if (c_parser_next_token_is_keyword (parser, RID_IN))
	      {
		c_parser_consume_token (parser);
		is_foreach_statement = true;
		if (! lvalue_p (init_expression))
		  c_parser_error (parser, "invalid iterating variable in "
					  "fast enumeration");
		object_expression
		  = c_fully_fold (init_expression, false, NULL);
	      }
	    else
	      {
		ce = convert_lvalue_to_rvalue (loc, ce, true, false);
		init_expression = ce.value;
		c_finish_expr_stmt (loc, init_expression);
		c_parser_skip_until_found (parser, CPP_SEMICOLON,
					   "expected %<;%>");
	      }
	  }
	}
      /* Parse the loop condition.  In the case of a foreach
	 statement, there is no loop condition.  */
      gcc_assert (!parser->objc_could_be_foreach_context);
      if (!is_foreach_statement)
	{
	  if (c_parser_next_token_is (parser, CPP_SEMICOLON))
	    {
	      if (ivdep)
		{
		  c_parser_error (parser, "missing loop condition in loop "
					  "with %<GCC ivdep%> pragma");
		  cond = error_mark_node;
		}
	      else if (unroll)
		{
		  c_parser_error (parser, "missing loop condition in loop "
					  "with %<GCC unroll%> pragma");
		  cond = error_mark_node;
		}
	      else
		{
		  c_parser_consume_token (parser);
		  cond = NULL_TREE;
		}
	    }
	  else
	    {
	      cond = c_parser_condition (parser);
	      c_parser_skip_until_found (parser, CPP_SEMICOLON,
					 "expected %<;%>");
	    }
	  if (ivdep && cond != error_mark_node)
	    cond = build3 (ANNOTATE_EXPR, TREE_TYPE (cond), cond,
			   build_int_cst (integer_type_node,
					  annot_expr_ivdep_kind),
			   integer_zero_node);
	  if (unroll && cond != error_mark_node)
	    cond = build3 (ANNOTATE_EXPR, TREE_TYPE (cond), cond,
 			   build_int_cst (integer_type_node,
					  annot_expr_unroll_kind),
			   build_int_cst (integer_type_node, unroll));
	}
      /* Parse the increment expression (the third expression in a
	 for-statement).  In the case of a foreach-statement, this is
	 the expression that follows the 'in'.  */
      loc = c_parser_peek_token (parser)->location;
      if (c_parser_next_token_is (parser, CPP_CLOSE_PAREN))
	{
	  if (is_foreach_statement)
	    {
	      c_parser_error (parser,
			      "missing collection in fast enumeration");
	      collection_expression = error_mark_node;
	    }
	  else
	    incr = c_process_expr_stmt (loc, NULL_TREE);
	}
      else
	{
	  if (is_foreach_statement)
	    collection_expression
	      = c_fully_fold (c_parser_expression (parser).value, false, NULL);
	  else
	    {
	      struct c_expr ce = c_parser_expression (parser);
	      ce = convert_lvalue_to_rvalue (loc, ce, true, false);
	      incr = c_process_expr_stmt (loc, ce.value);
	    }
	}
      parens.skip_until_found_close (parser);
    }
  save_in_statement = in_statement;
  if (is_foreach_statement)
    {
      in_statement = IN_OBJC_FOREACH;
      save_objc_foreach_break_label = objc_foreach_break_label;
      save_objc_foreach_continue_label = objc_foreach_continue_label;
      objc_foreach_break_label = create_artificial_label (loc);
      objc_foreach_continue_label = create_artificial_label (loc);
    }
  else
    in_statement = IN_ITERATION_STMT;

  token_indent_info body_tinfo
    = get_token_indent_info (c_parser_peek_token (parser));

  location_t loc_after_labels;
  bool open_brace = c_parser_next_token_is (parser, CPP_OPEN_BRACE);
  body = c_parser_c99_block_statement (parser, if_p, &loc_after_labels);

  if (is_foreach_statement)
    objc_finish_foreach_loop (for_loc, object_expression,
			      collection_expression, body,
			      objc_foreach_break_label,
			      objc_foreach_continue_label);
  else
    add_stmt (build_stmt (for_loc, FOR_STMT, NULL_TREE, cond, incr,
			  body, NULL_TREE));
  add_stmt (c_end_compound_stmt (for_loc, block,
				 flag_isoc99 || c_dialect_objc ()));
  c_parser_maybe_reclassify_token (parser);

  token_indent_info next_tinfo
    = get_token_indent_info (c_parser_peek_token (parser));
  warn_for_misleading_indentation (for_tinfo, body_tinfo, next_tinfo);

  if (next_tinfo.type != CPP_SEMICOLON && !open_brace)
    warn_for_multistatement_macros (loc_after_labels, next_tinfo.location,
				    for_tinfo.location, RID_FOR);

  in_statement = save_in_statement;
  if (is_foreach_statement)
    {
      objc_foreach_break_label = save_objc_foreach_break_label;
      objc_foreach_continue_label = save_objc_foreach_continue_label;
    }
}

/* Parse an asm statement, a GNU extension.  This is a full-blown asm
   statement with inputs, outputs, clobbers, and volatile, inline, and goto
   tags allowed.

   asm-qualifier:
     volatile
     inline
     goto

   asm-qualifier-list:
     asm-qualifier-list asm-qualifier
     asm-qualifier

   asm-statement:
     asm asm-qualifier-list[opt] ( asm-argument ) ;

   asm-argument:
     asm-string-literal
     asm-string-literal : asm-operands[opt]
     asm-string-literal : asm-operands[opt] : asm-operands[opt]
     asm-string-literal : asm-operands[opt] : asm-operands[opt] \
       : asm-clobbers[opt]
     asm-string-literal : : asm-operands[opt] : asm-clobbers[opt] \
       : asm-goto-operands

   The form with asm-goto-operands is valid if and only if the
   asm-qualifier-list contains goto, and is the only allowed form in that case.
   Duplicate asm-qualifiers are not allowed.

   The :: token is considered equivalent to two consecutive : tokens.  */

static tree
c_parser_asm_statement (c_parser *parser)
{
  tree str, outputs, inputs, clobbers, labels, ret;
  bool simple;
  location_t asm_loc = c_parser_peek_token (parser)->location;
  int section, nsections;

  gcc_assert (c_parser_next_token_is_keyword (parser, RID_ASM));
  c_parser_consume_token (parser);

  /* Handle the asm-qualifier-list.  */
  location_t volatile_loc = UNKNOWN_LOCATION;
  location_t inline_loc = UNKNOWN_LOCATION;
  location_t goto_loc = UNKNOWN_LOCATION;
  for (;;)
    {
      c_token *token = c_parser_peek_token (parser);
      location_t loc = token->location;
      switch (token->keyword)
	{
	case RID_VOLATILE:
	  if (volatile_loc)
	    {
	      error_at (loc, "duplicate %<asm%> qualifier %qE", token->value);
	      inform (volatile_loc, "first seen here");
	    }
	  else
	    volatile_loc = loc;
	  c_parser_consume_token (parser);
	  continue;

	case RID_INLINE:
	  if (inline_loc)
	    {
	      error_at (loc, "duplicate %<asm%> qualifier %qE", token->value);
	      inform (inline_loc, "first seen here");
	    }
	  else
	    inline_loc = loc;
	  c_parser_consume_token (parser);
	  continue;

	case RID_GOTO:
	  if (goto_loc)
	    {
	      error_at (loc, "duplicate %<asm%> qualifier %qE", token->value);
	      inform (goto_loc, "first seen here");
	    }
	  else
	    goto_loc = loc;
	  c_parser_consume_token (parser);
	  continue;

	case RID_CONST:
	case RID_RESTRICT:
	  error_at (loc, "%qE is not a valid %<asm%> qualifier", token->value);
	  c_parser_consume_token (parser);
	  continue;

	default:
	  break;
	}
      break;
    }

  bool is_volatile = (volatile_loc != UNKNOWN_LOCATION);
  bool is_inline = (inline_loc != UNKNOWN_LOCATION);
  bool is_goto = (goto_loc != UNKNOWN_LOCATION);

  ret = NULL;

  matching_parens parens;
  if (!parens.require_open (parser))
    goto error;

  str = c_parser_asm_string_literal (parser);
  if (str == NULL_TREE)
    goto error_close_paren;

  simple = true;
  outputs = NULL_TREE;
  inputs = NULL_TREE;
  clobbers = NULL_TREE;
  labels = NULL_TREE;

  if (c_parser_next_token_is (parser, CPP_CLOSE_PAREN) && !is_goto)
    goto done_asm;

  /* Parse each colon-delimited section of operands.  */
  nsections = 3 + is_goto;
  for (section = 0; section < nsections; ++section)
    {
      if (c_parser_next_token_is (parser, CPP_SCOPE))
	{
	  ++section;
	  if (section == nsections)
	    {
	      c_parser_error (parser, "expected %<)%>");
	      goto error_close_paren;
	    }
	  c_parser_consume_token (parser);
	}
      else if (!c_parser_require (parser, CPP_COLON,
				  is_goto
				  ? G_("expected %<:%>")
				  : G_("expected %<:%> or %<)%>"),
				  UNKNOWN_LOCATION, is_goto))
	goto error_close_paren;

      /* Once past any colon, we're no longer a simple asm.  */
      simple = false;

      if ((!c_parser_next_token_is (parser, CPP_COLON)
	   && !c_parser_next_token_is (parser, CPP_SCOPE)
	   && !c_parser_next_token_is (parser, CPP_CLOSE_PAREN))
	  || section == 3)
	switch (section)
	  {
	  case 0:
	    outputs = c_parser_asm_operands (parser);
	    break;
	  case 1:
	    inputs = c_parser_asm_operands (parser);
	    break;
	  case 2:
	    clobbers = c_parser_asm_clobbers (parser);
	    break;
	  case 3:
	    labels = c_parser_asm_goto_operands (parser);
	    break;
	  default:
	    gcc_unreachable ();
	  }

      if (c_parser_next_token_is (parser, CPP_CLOSE_PAREN) && !is_goto)
	goto done_asm;
    }

 done_asm:
  if (!parens.require_close (parser))
    {
      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, NULL);
      goto error;
    }

  if (!c_parser_require (parser, CPP_SEMICOLON, "expected %<;%>"))
    c_parser_skip_to_end_of_block_or_statement (parser);

  ret = build_asm_stmt (is_volatile,
			build_asm_expr (asm_loc, str, outputs, inputs,
					clobbers, labels, simple, is_inline));

 error:
  return ret;

 error_close_paren:
  c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, NULL);
  goto error;
}

/* Parse asm operands, a GNU extension.

   asm-operands:
     asm-operand
     asm-operands , asm-operand

   asm-operand:
     asm-string-literal ( expression )
     [ identifier ] asm-string-literal ( expression )
*/

static tree
c_parser_asm_operands (c_parser *parser)
{
  tree list = NULL_TREE;
  while (true)
    {
      tree name, str;
      struct c_expr expr;
      if (c_parser_next_token_is (parser, CPP_OPEN_SQUARE))
	{
	  c_parser_consume_token (parser);
	  if (c_parser_next_token_is (parser, CPP_NAME))
	    {
	      tree id = c_parser_peek_token (parser)->value;
	      c_parser_consume_token (parser);
	      name = build_string (IDENTIFIER_LENGTH (id),
				   IDENTIFIER_POINTER (id));
	    }
	  else
	    {
	      c_parser_error (parser, "expected identifier");
	      c_parser_skip_until_found (parser, CPP_CLOSE_SQUARE, NULL);
	      return NULL_TREE;
	    }
	  c_parser_skip_until_found (parser, CPP_CLOSE_SQUARE,
				     "expected %<]%>");
	}
      else
	name = NULL_TREE;
      str = c_parser_asm_string_literal (parser);
      if (str == NULL_TREE)
	return NULL_TREE;
      matching_parens parens;
      if (!parens.require_open (parser))
	return NULL_TREE;
      expr = c_parser_expression (parser);
      mark_exp_read (expr.value);
      if (!parens.require_close (parser))
	{
	  c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, NULL);
	  return NULL_TREE;
	}
      list = chainon (list, build_tree_list (build_tree_list (name, str),
					     expr.value));
      if (c_parser_next_token_is (parser, CPP_COMMA))
	c_parser_consume_token (parser);
      else
	break;
    }
  return list;
}

/* Parse asm clobbers, a GNU extension.

   asm-clobbers:
     asm-string-literal
     asm-clobbers , asm-string-literal
*/

static tree
c_parser_asm_clobbers (c_parser *parser)
{
  tree list = NULL_TREE;
  while (true)
    {
      tree str = c_parser_asm_string_literal (parser);
      if (str)
	list = tree_cons (NULL_TREE, str, list);
      else
	return NULL_TREE;
      if (c_parser_next_token_is (parser, CPP_COMMA))
	c_parser_consume_token (parser);
      else
	break;
    }
  return list;
}

/* Parse asm goto labels, a GNU extension.

   asm-goto-operands:
     identifier
     asm-goto-operands , identifier
*/

static tree
c_parser_asm_goto_operands (c_parser *parser)
{
  tree list = NULL_TREE;
  while (true)
    {
      tree name, label;

      if (c_parser_next_token_is (parser, CPP_NAME))
	{
	  c_token *tok = c_parser_peek_token (parser);
	  name = tok->value;
	  label = lookup_label_for_goto (tok->location, name);
	  c_parser_consume_token (parser);
	  TREE_USED (label) = 1;
	}
      else
	{
	  c_parser_error (parser, "expected identifier");
	  return NULL_TREE;
	}

      name = build_string (IDENTIFIER_LENGTH (name),
			   IDENTIFIER_POINTER (name));
      list = tree_cons (name, label, list);
      if (c_parser_next_token_is (parser, CPP_COMMA))
	c_parser_consume_token (parser);
      else
	return nreverse (list);
    }
}

/* Parse a possibly concatenated sequence of string literals.
   TRANSLATE says whether to translate them to the execution character
   set; WIDE_OK says whether any kind of prefixed string literal is
   permitted in this context.  This code is based on that in
   lex_string.  */

struct c_expr
c_parser_string_literal (c_parser *parser, bool translate, bool wide_ok)
{
  struct c_expr ret;
  size_t count;
  struct obstack str_ob;
  struct obstack loc_ob;
  cpp_string str, istr, *strs;
  c_token *tok;
  location_t loc, last_tok_loc;
  enum cpp_ttype type;
  tree value, string_tree;

  tok = c_parser_peek_token (parser);
  loc = tok->location;
  last_tok_loc = linemap_resolve_location (line_table, loc,
					   LRK_MACRO_DEFINITION_LOCATION,
					   NULL);
  type = tok->type;
  switch (type)
    {
    case CPP_STRING:
    case CPP_WSTRING:
    case CPP_STRING16:
    case CPP_STRING32:
    case CPP_UTF8STRING:
      string_tree = tok->value;
      break;

    default:
      c_parser_error (parser, "expected string literal");
      ret.set_error ();
      ret.value = NULL_TREE;
      ret.original_code = ERROR_MARK;
      ret.original_type = NULL_TREE;
      return ret;
    }

  /* Try to avoid the overhead of creating and destroying an obstack
     for the common case of just one string.  */
  switch (c_parser_peek_2nd_token (parser)->type)
    {
    default:
      c_parser_consume_token (parser);
      str.text = (const unsigned char *) TREE_STRING_POINTER (string_tree);
      str.len = TREE_STRING_LENGTH (string_tree);
      count = 1;
      strs = &str;
      break;

    case CPP_STRING:
    case CPP_WSTRING:
    case CPP_STRING16:
    case CPP_STRING32:
    case CPP_UTF8STRING:
      gcc_obstack_init (&str_ob);
      gcc_obstack_init (&loc_ob);
      count = 0;
      do
	{
	  c_parser_consume_token (parser);
	  count++;
	  str.text = (const unsigned char *) TREE_STRING_POINTER (string_tree);
	  str.len = TREE_STRING_LENGTH (string_tree);
	  if (type != tok->type)
	    {
	      if (type == CPP_STRING)
		type = tok->type;
	      else if (tok->type != CPP_STRING)
		error ("unsupported non-standard concatenation "
		       "of string literals");
	    }
	  obstack_grow (&str_ob, &str, sizeof (cpp_string));
	  obstack_grow (&loc_ob, &last_tok_loc, sizeof (location_t));
	  tok = c_parser_peek_token (parser);
	  string_tree = tok->value;
	  last_tok_loc
	    = linemap_resolve_location (line_table, tok->location,
					LRK_MACRO_DEFINITION_LOCATION, NULL);
	}
      while (tok->type == CPP_STRING
	     || tok->type == CPP_WSTRING
	     || tok->type == CPP_STRING16
	     || tok->type == CPP_STRING32
	     || tok->type == CPP_UTF8STRING);
      strs = (cpp_string *) obstack_finish (&str_ob);
    }

  if (count > 1 && !in_system_header_at (input_location))
    warning (OPT_Wtraditional,
	     "traditional C rejects string constant concatenation");

  if ((type == CPP_STRING || wide_ok)
      && ((translate
	  ? cpp_interpret_string : cpp_interpret_string_notranslate)
	  (parse_in, strs, count, &istr, type)))
    {
      value = build_string (istr.len, (const char *) istr.text);
      free (CONST_CAST (unsigned char *, istr.text));
      if (count > 1)
	{
	  location_t *locs = (location_t *) obstack_finish (&loc_ob);
	  gcc_assert (g_string_concat_db);
	  g_string_concat_db->record_string_concatenation (count, locs);
	}
    }
  else
    {
      if (type != CPP_STRING && !wide_ok)
	{
	  error_at (loc, "a wide string is invalid in this context");
	  type = CPP_STRING;
	}
      /* Callers cannot generally handle error_mark_node in this
	 context, so return the empty string instead.  An error has
	 been issued, either above or from cpp_interpret_string.  */
      switch (type)
	{
	default:
	case CPP_STRING:
	case CPP_UTF8STRING:
	  value = build_string (1, "");
	  break;
	case CPP_STRING16:
	  value = build_string (TYPE_PRECISION (char16_type_node)
				/ TYPE_PRECISION (char_type_node),
				"\0");  /* char16_t is 16 bits */
	  break;
	case CPP_STRING32:
	  value = build_string (TYPE_PRECISION (char32_type_node)
				/ TYPE_PRECISION (char_type_node),
				"\0\0\0");  /* char32_t is 32 bits */
	  break;
	case CPP_WSTRING:
	  value = build_string (TYPE_PRECISION (wchar_type_node)
				/ TYPE_PRECISION (char_type_node),
				"\0\0\0");  /* widest supported wchar_t
					       is 32 bits */
	  break;
        }
    }

  switch (type)
    {
    default:
    case CPP_STRING:
    case CPP_UTF8STRING:
      TREE_TYPE (value) = char_array_type_node;
      break;
    case CPP_STRING16:
      TREE_TYPE (value) = char16_array_type_node;
      break;
    case CPP_STRING32:
      TREE_TYPE (value) = char32_array_type_node;
      break;
    case CPP_WSTRING:
      TREE_TYPE (value) = wchar_array_type_node;
    }
  value = fix_string_type (value);

  if (count > 1)
    {
      obstack_free (&str_ob, 0);
      obstack_free (&loc_ob, 0);
    }

  ret.value = value;
  ret.original_code = STRING_CST;
  ret.original_type = NULL_TREE;
  set_c_expr_source_range (&ret, get_range_from_loc (line_table, loc));
  parser->seen_string_literal = true;
  return ret;
}

/* Parse an expression other than a compound expression; that is, an
   assignment expression (C90 6.3.16, C99 6.5.16, C11 6.5.16).  If
   AFTER is not NULL then it is an Objective-C message expression which
   is the primary-expression starting the expression as an initializer.

   assignment-expression:
     conditional-expression
     unary-expression assignment-operator assignment-expression

   assignment-operator: one of
     = *= /= %= += -= <<= >>= &= ^= |=

   In GNU C we accept any conditional expression on the LHS and
   diagnose the invalid lvalue rather than producing a syntax
   error.  */

static struct c_expr
c_parser_expr_no_commas (c_parser *parser, struct c_expr *after,
			 tree omp_atomic_lhs)
{
  struct c_expr lhs, rhs, ret;
  enum tree_code code;
  location_t op_location, exp_location;
  bool save_in_omp_for = c_in_omp_for;
  c_in_omp_for = false;
  gcc_assert (!after || c_dialect_objc ());
  lhs = c_parser_conditional_expression (parser, after, omp_atomic_lhs);
  op_location = c_parser_peek_token (parser)->location;
  switch (c_parser_peek_token (parser)->type)
    {
    case CPP_EQ:
      code = NOP_EXPR;
      break;
    case CPP_MULT_EQ:
      code = MULT_EXPR;
      break;
    case CPP_DIV_EQ:
      code = TRUNC_DIV_EXPR;
      break;
    case CPP_MOD_EQ:
      code = TRUNC_MOD_EXPR;
      break;
    case CPP_PLUS_EQ:
      code = PLUS_EXPR;
      break;
    case CPP_MINUS_EQ:
      code = MINUS_EXPR;
      break;
    case CPP_LSHIFT_EQ:
      code = LSHIFT_EXPR;
      break;
    case CPP_RSHIFT_EQ:
      code = RSHIFT_EXPR;
      break;
    case CPP_AND_EQ:
      code = BIT_AND_EXPR;
      break;
    case CPP_XOR_EQ:
      code = BIT_XOR_EXPR;
      break;
    case CPP_OR_EQ:
      code = BIT_IOR_EXPR;
      break;
    default:
      c_in_omp_for = save_in_omp_for;
      return lhs;
    }
  c_parser_consume_token (parser);
  exp_location = c_parser_peek_token (parser)->location;
  rhs = c_parser_expr_no_commas (parser, NULL);
  rhs = convert_lvalue_to_rvalue (exp_location, rhs, true, true);
  
  ret.value = build_modify_expr (op_location, lhs.value, lhs.original_type,
				 code, exp_location, rhs.value,
				 rhs.original_type);
  set_c_expr_source_range (&ret, lhs.get_start (), rhs.get_finish ());
  if (code == NOP_EXPR)
    ret.original_code = MODIFY_EXPR;
  else
    {
      TREE_NO_WARNING (ret.value) = 1;
      ret.original_code = ERROR_MARK;
    }
  ret.original_type = NULL;
  c_in_omp_for = save_in_omp_for;
  return ret;
}

/* Parse a conditional expression (C90 6.3.15, C99 6.5.15, C11 6.5.15).  If
   AFTER is not NULL then it is an Objective-C message expression which is
   the primary-expression starting the expression as an initializer.

   conditional-expression:
     logical-OR-expression
     logical-OR-expression ? expression : conditional-expression

   GNU extensions:

   conditional-expression:
     logical-OR-expression ? : conditional-expression
*/

static struct c_expr
c_parser_conditional_expression (c_parser *parser, struct c_expr *after,
				 tree omp_atomic_lhs)
{
  struct c_expr cond, exp1, exp2, ret;
  location_t start, cond_loc, colon_loc;

  gcc_assert (!after || c_dialect_objc ());

  cond = c_parser_binary_expression (parser, after, omp_atomic_lhs);

  if (c_parser_next_token_is_not (parser, CPP_QUERY))
    return cond;
  if (cond.value != error_mark_node)
    start = cond.get_start ();
  else
    start = UNKNOWN_LOCATION;
  cond_loc = c_parser_peek_token (parser)->location;
  cond = convert_lvalue_to_rvalue (cond_loc, cond, true, true);
  c_parser_consume_token (parser);
  if (c_parser_next_token_is (parser, CPP_COLON))
    {
      tree eptype = NULL_TREE;

      location_t middle_loc = c_parser_peek_token (parser)->location;
      pedwarn (middle_loc, OPT_Wpedantic,
	       "ISO C forbids omitting the middle term of a %<?:%> expression");
      if (TREE_CODE (cond.value) == EXCESS_PRECISION_EXPR)
	{
	  eptype = TREE_TYPE (cond.value);
	  cond.value = TREE_OPERAND (cond.value, 0);
	}
      tree e = cond.value;
      while (TREE_CODE (e) == COMPOUND_EXPR)
	e = TREE_OPERAND (e, 1);
      warn_for_omitted_condop (middle_loc, e);
      /* Make sure first operand is calculated only once.  */
      exp1.value = save_expr (default_conversion (cond.value));
      if (eptype)
	exp1.value = build1 (EXCESS_PRECISION_EXPR, eptype, exp1.value);
      exp1.original_type = NULL;
      exp1.src_range = cond.src_range;
      cond.value = c_objc_common_truthvalue_conversion (cond_loc, exp1.value);
      c_inhibit_evaluation_warnings += cond.value == truthvalue_true_node;
    }
  else
    {
      cond.value
	= c_objc_common_truthvalue_conversion
	(cond_loc, default_conversion (cond.value));
      c_inhibit_evaluation_warnings += cond.value == truthvalue_false_node;
      exp1 = c_parser_expression_conv (parser);
      mark_exp_read (exp1.value);
      c_inhibit_evaluation_warnings +=
	((cond.value == truthvalue_true_node)
	 - (cond.value == truthvalue_false_node));
    }

  colon_loc = c_parser_peek_token (parser)->location;
  if (!c_parser_require (parser, CPP_COLON, "expected %<:%>"))
    {
      c_inhibit_evaluation_warnings -= cond.value == truthvalue_true_node;
      ret.set_error ();
      ret.original_code = ERROR_MARK;
      ret.original_type = NULL;
      return ret;
    }
  {
    location_t exp2_loc = c_parser_peek_token (parser)->location;
    exp2 = c_parser_conditional_expression (parser, NULL, NULL_TREE);
    exp2 = convert_lvalue_to_rvalue (exp2_loc, exp2, true, true);
  }
  c_inhibit_evaluation_warnings -= cond.value == truthvalue_true_node;
  location_t loc1 = make_location (exp1.get_start (), exp1.src_range);
  location_t loc2 = make_location (exp2.get_start (), exp2.src_range);
  ret.value = build_conditional_expr (colon_loc, cond.value,
				      cond.original_code == C_MAYBE_CONST_EXPR,
				      exp1.value, exp1.original_type, loc1,
				      exp2.value, exp2.original_type, loc2);
  ret.original_code = ERROR_MARK;
  if (exp1.value == error_mark_node || exp2.value == error_mark_node)
    ret.original_type = NULL;
  else
    {
      tree t1, t2;

      /* If both sides are enum type, the default conversion will have
	 made the type of the result be an integer type.  We want to
	 remember the enum types we started with.  */
      t1 = exp1.original_type ? exp1.original_type : TREE_TYPE (exp1.value);
      t2 = exp2.original_type ? exp2.original_type : TREE_TYPE (exp2.value);
      ret.original_type = ((t1 != error_mark_node
			    && t2 != error_mark_node
			    && (TYPE_MAIN_VARIANT (t1)
				== TYPE_MAIN_VARIANT (t2)))
			   ? t1
			   : NULL);
    }
  set_c_expr_source_range (&ret, start, exp2.get_finish ());
  return ret;
}

/* Parse a binary expression; that is, a logical-OR-expression (C90
   6.3.5-6.3.14, C99 6.5.5-6.5.14, C11 6.5.5-6.5.14).  If AFTER is not
   NULL then it is an Objective-C message expression which is the
   primary-expression starting the expression as an initializer.

   OMP_ATOMIC_LHS is NULL, unless parsing OpenMP #pragma omp atomic,
   when it should be the unfolded lhs.  In a valid OpenMP source,
   one of the operands of the toplevel binary expression must be equal
   to it.  In that case, just return a build2 created binary operation
   rather than result of parser_build_binary_op.

   multiplicative-expression:
     cast-expression
     multiplicative-expression * cast-expression
     multiplicative-expression / cast-expression
     multiplicative-expression % cast-expression

   additive-expression:
     multiplicative-expression
     additive-expression + multiplicative-expression
     additive-expression - multiplicative-expression

   shift-expression:
     additive-expression
     shift-expression << additive-expression
     shift-expression >> additive-expression

   relational-expression:
     shift-expression
     relational-expression < shift-expression
     relational-expression > shift-expression
     relational-expression <= shift-expression
     relational-expression >= shift-expression

   equality-expression:
     relational-expression
     equality-expression == relational-expression
     equality-expression != relational-expression

   AND-expression:
     equality-expression
     AND-expression & equality-expression

   exclusive-OR-expression:
     AND-expression
     exclusive-OR-expression ^ AND-expression

   inclusive-OR-expression:
     exclusive-OR-expression
     inclusive-OR-expression | exclusive-OR-expression

   logical-AND-expression:
     inclusive-OR-expression
     logical-AND-expression && inclusive-OR-expression

   logical-OR-expression:
     logical-AND-expression
     logical-OR-expression || logical-AND-expression
*/

static struct c_expr
c_parser_binary_expression (c_parser *parser, struct c_expr *after,
			    tree omp_atomic_lhs)
{
  /* A binary expression is parsed using operator-precedence parsing,
     with the operands being cast expressions.  All the binary
     operators are left-associative.  Thus a binary expression is of
     form:

     E0 op1 E1 op2 E2 ...

     which we represent on a stack.  On the stack, the precedence
     levels are strictly increasing.  When a new operator is
     encountered of higher precedence than that at the top of the
     stack, it is pushed; its LHS is the top expression, and its RHS
     is everything parsed until it is popped.  When a new operator is
     encountered with precedence less than or equal to that at the top
     of the stack, triples E[i-1] op[i] E[i] are popped and replaced
     by the result of the operation until the operator at the top of
     the stack has lower precedence than the new operator or there is
     only one element on the stack; then the top expression is the LHS
     of the new operator.  In the case of logical AND and OR
     expressions, we also need to adjust c_inhibit_evaluation_warnings
     as appropriate when the operators are pushed and popped.  */

  struct {
    /* The expression at this stack level.  */
    struct c_expr expr;
    /* The precedence of the operator on its left, PREC_NONE at the
       bottom of the stack.  */
    enum c_parser_prec prec;
    /* The operation on its left.  */
    enum tree_code op;
    /* The source location of this operation.  */
    location_t loc;
    /* The sizeof argument if expr.original_code == {PAREN_,}SIZEOF_EXPR.  */
    tree sizeof_arg;
  } stack[NUM_PRECS];
  int sp;
  /* Location of the binary operator.  */
  location_t binary_loc = UNKNOWN_LOCATION;  /* Quiet warning.  */
#define POP								      \
  do {									      \
    switch (stack[sp].op)						      \
      {									      \
      case TRUTH_ANDIF_EXPR:						      \
	c_inhibit_evaluation_warnings -= (stack[sp - 1].expr.value	      \
					  == truthvalue_false_node);	      \
	break;								      \
      case TRUTH_ORIF_EXPR:						      \
	c_inhibit_evaluation_warnings -= (stack[sp - 1].expr.value	      \
					  == truthvalue_true_node);	      \
	break;								      \
      case TRUNC_DIV_EXPR:						      \
	if ((stack[sp - 1].expr.original_code == SIZEOF_EXPR		      \
	     || stack[sp - 1].expr.original_code == PAREN_SIZEOF_EXPR)	      \
	    && (stack[sp].expr.original_code == SIZEOF_EXPR		      \
		|| stack[sp].expr.original_code == PAREN_SIZEOF_EXPR))	      \
	  {								      \
	    tree type0 = stack[sp - 1].sizeof_arg;			      \
	    tree type1 = stack[sp].sizeof_arg;				      \
	    tree first_arg = type0;					      \
	    if (!TYPE_P (type0))					      \
	      type0 = TREE_TYPE (type0);				      \
	    if (!TYPE_P (type1))					      \
	      type1 = TREE_TYPE (type1);				      \
	    if (POINTER_TYPE_P (type0)					      \
		&& comptypes (TREE_TYPE (type0), type1)			      \
		&& !(TREE_CODE (first_arg) == PARM_DECL			      \
		     && C_ARRAY_PARAMETER (first_arg)			      \
		     && warn_sizeof_array_argument))			      \
	      {								      \
		auto_diagnostic_group d;				      \
		if (warning_at (stack[sp].loc, OPT_Wsizeof_pointer_div,	      \
				  "division %<sizeof (%T) / sizeof (%T)%> "   \
				  "does not compute the number of array "     \
				  "elements",				      \
				  type0, type1))			      \
		  if (DECL_P (first_arg))				      \
		    inform (DECL_SOURCE_LOCATION (first_arg),		      \
			      "first %<sizeof%> operand was declared here");  \
	      }								      \
	    else if (TREE_CODE (type0) == ARRAY_TYPE			      \
		     && !char_type_p (TYPE_MAIN_VARIANT (TREE_TYPE (type0)))  \
		     && stack[sp].expr.original_code != PAREN_SIZEOF_EXPR)    \
	      maybe_warn_sizeof_array_div (stack[sp].loc, first_arg, type0,   \
					   stack[sp].sizeof_arg, type1);      \
	  }								      \
	break;								      \
      default:								      \
	break;								      \
      }									      \
    stack[sp - 1].expr							      \
      = convert_lvalue_to_rvalue (stack[sp - 1].loc,			      \
				  stack[sp - 1].expr, true, true);	      \
    stack[sp].expr							      \
      = convert_lvalue_to_rvalue (stack[sp].loc,			      \
				  stack[sp].expr, true, true);		      \
    if (__builtin_expect (omp_atomic_lhs != NULL_TREE, 0) && sp == 1	      \
	&& c_parser_peek_token (parser)->type == CPP_SEMICOLON		      \
	&& ((1 << stack[sp].prec)					      \
	    & ((1 << PREC_BITOR) | (1 << PREC_BITXOR) | (1 << PREC_BITAND)    \
	       | (1 << PREC_SHIFT) | (1 << PREC_ADD) | (1 << PREC_MULT)))     \
	&& stack[sp].op != TRUNC_MOD_EXPR				      \
	&& stack[0].expr.value != error_mark_node			      \
	&& stack[1].expr.value != error_mark_node			      \
	&& (c_tree_equal (stack[0].expr.value, omp_atomic_lhs)		      \
	    || c_tree_equal (stack[1].expr.value, omp_atomic_lhs)))	      \
      {									      \
	tree t = make_node (stack[1].op);				      \
	TREE_TYPE (t) = TREE_TYPE (stack[0].expr.value);		      \
	TREE_OPERAND (t, 0) = stack[0].expr.value;			      \
	TREE_OPERAND (t, 1) = stack[1].expr.value;			      \
	stack[0].expr.value = t;					      \
      }									      \
    else								      \
      stack[sp - 1].expr = parser_build_binary_op (stack[sp].loc,	      \
						   stack[sp].op,	      \
						   stack[sp - 1].expr,	      \
						   stack[sp].expr);	      \
    sp--;								      \
  } while (0)
  gcc_assert (!after || c_dialect_objc ());
  stack[0].loc = c_parser_peek_token (parser)->location;
  stack[0].expr = c_parser_cast_expression (parser, after);
  stack[0].prec = PREC_NONE;
  stack[0].sizeof_arg = c_last_sizeof_arg;
  sp = 0;
  while (true)
    {
      enum c_parser_prec oprec;
      enum tree_code ocode;
      source_range src_range;
      if (parser->error)
	goto out;
      switch (c_parser_peek_token (parser)->type)
	{
	case CPP_MULT:
	  oprec = PREC_MULT;
	  ocode = MULT_EXPR;
	  break;
	case CPP_DIV:
	  oprec = PREC_MULT;
	  ocode = TRUNC_DIV_EXPR;
	  break;
	case CPP_MOD:
	  oprec = PREC_MULT;
	  ocode = TRUNC_MOD_EXPR;
	  break;
	case CPP_PLUS:
	  oprec = PREC_ADD;
	  ocode = PLUS_EXPR;
	  break;
	case CPP_MINUS:
	  oprec = PREC_ADD;
	  ocode = MINUS_EXPR;
	  break;
	case CPP_LSHIFT:
	  oprec = PREC_SHIFT;
	  ocode = LSHIFT_EXPR;
	  break;
	case CPP_RSHIFT:
	  oprec = PREC_SHIFT;
	  ocode = RSHIFT_EXPR;
	  break;
	case CPP_LESS:
	  oprec = PREC_REL;
	  ocode = LT_EXPR;
	  break;
	case CPP_GREATER:
	  oprec = PREC_REL;
	  ocode = GT_EXPR;
	  break;
	case CPP_LESS_EQ:
	  oprec = PREC_REL;
	  ocode = LE_EXPR;
	  break;
	case CPP_GREATER_EQ:
	  oprec = PREC_REL;
	  ocode = GE_EXPR;
	  break;
	case CPP_EQ_EQ:
	  oprec = PREC_EQ;
	  ocode = EQ_EXPR;
	  break;
	case CPP_NOT_EQ:
	  oprec = PREC_EQ;
	  ocode = NE_EXPR;
	  break;
	case CPP_AND:
	  oprec = PREC_BITAND;
	  ocode = BIT_AND_EXPR;
	  break;
	case CPP_XOR:
	  oprec = PREC_BITXOR;
	  ocode = BIT_XOR_EXPR;
	  break;
	case CPP_OR:
	  oprec = PREC_BITOR;
	  ocode = BIT_IOR_EXPR;
	  break;
	case CPP_AND_AND:
	  oprec = PREC_LOGAND;
	  ocode = TRUTH_ANDIF_EXPR;
	  break;
	case CPP_OR_OR:
	  oprec = PREC_LOGOR;
	  ocode = TRUTH_ORIF_EXPR;
	  break;
	default:
	  /* Not a binary operator, so end of the binary
	     expression.  */
	  goto out;
	}
      binary_loc = c_parser_peek_token (parser)->location;
      while (oprec <= stack[sp].prec)
	POP;
      c_parser_consume_token (parser);
      switch (ocode)
	{
	case TRUTH_ANDIF_EXPR:
	  src_range = stack[sp].expr.src_range;
	  stack[sp].expr
	    = convert_lvalue_to_rvalue (stack[sp].loc,
					stack[sp].expr, true, true);
	  stack[sp].expr.value = c_objc_common_truthvalue_conversion
	    (stack[sp].loc, default_conversion (stack[sp].expr.value));
	  c_inhibit_evaluation_warnings += (stack[sp].expr.value
					    == truthvalue_false_node);
	  set_c_expr_source_range (&stack[sp].expr, src_range);
	  break;
	case TRUTH_ORIF_EXPR:
	  src_range = stack[sp].expr.src_range;
	  stack[sp].expr
	    = convert_lvalue_to_rvalue (stack[sp].loc,
					stack[sp].expr, true, true);
	  stack[sp].expr.value = c_objc_common_truthvalue_conversion
	    (stack[sp].loc, default_conversion (stack[sp].expr.value));
	  c_inhibit_evaluation_warnings += (stack[sp].expr.value
					    == truthvalue_true_node);
	  set_c_expr_source_range (&stack[sp].expr, src_range);
	  break;
	default:
	  break;
	}
      sp++;
      stack[sp].loc = binary_loc;
      stack[sp].expr = c_parser_cast_expression (parser, NULL);
      stack[sp].prec = oprec;
      stack[sp].op = ocode;
      stack[sp].sizeof_arg = c_last_sizeof_arg;
    }
 out:
  while (sp > 0)
    POP;
  return stack[0].expr;
#undef POP
}

/* Parse a cast expression (C90 6.3.4, C99 6.5.4, C11 6.5.4).  If AFTER
   is not NULL then it is an Objective-C message expression which is the
   primary-expression starting the expression as an initializer.

   cast-expression:
     unary-expression
     ( type-name ) unary-expression
*/

static struct c_expr
c_parser_cast_expression (c_parser *parser, struct c_expr *after)
{
  location_t cast_loc = c_parser_peek_token (parser)->location;
  gcc_assert (!after || c_dialect_objc ());
  if (after)
    return c_parser_postfix_expression_after_primary (parser,
						      cast_loc, *after);
  /* If the expression begins with a parenthesized type name, it may
     be either a cast or a compound literal; we need to see whether
     the next character is '{' to tell the difference.  If not, it is
     an unary expression.  Full detection of unknown typenames here
     would require a 3-token lookahead.  */
  if (c_parser_next_token_is (parser, CPP_OPEN_PAREN)
      && c_token_starts_typename (c_parser_peek_2nd_token (parser)))
    {
      struct c_type_name *type_name;
      struct c_expr ret;
      struct c_expr expr;
      matching_parens parens;
      parens.consume_open (parser);
      type_name = c_parser_type_name (parser, true);
      parens.skip_until_found_close (parser);
      if (type_name == NULL)
	{
	  ret.set_error ();
	  ret.original_code = ERROR_MARK;
	  ret.original_type = NULL;
	  return ret;
	}

      /* Save casted types in the function's used types hash table.  */
      used_types_insert (type_name->specs->type);

      if (c_parser_next_token_is (parser, CPP_OPEN_BRACE))
	return c_parser_postfix_expression_after_paren_type (parser, type_name,
							     cast_loc);
      if (type_name->specs->alignas_p)
	error_at (type_name->specs->locations[cdw_alignas],
		  "alignment specified for type name in cast");
      {
	location_t expr_loc = c_parser_peek_token (parser)->location;
	expr = c_parser_cast_expression (parser, NULL);
	expr = convert_lvalue_to_rvalue (expr_loc, expr, true, true);
      }
      ret.value = c_cast_expr (cast_loc, type_name, expr.value);
      if (ret.value && expr.value)
	set_c_expr_source_range (&ret, cast_loc, expr.get_finish ());
      ret.original_code = ERROR_MARK;
      ret.original_type = NULL;
      return ret;
    }
  else
    return c_parser_unary_expression (parser);
}

/* Parse an unary expression (C90 6.3.3, C99 6.5.3, C11 6.5.3).

   unary-expression:
     postfix-expression
     ++ unary-expression
     -- unary-expression
     unary-operator cast-expression
     sizeof unary-expression
     sizeof ( type-name )

   unary-operator: one of
     & * + - ~ !

   GNU extensions:

   unary-expression:
     __alignof__ unary-expression
     __alignof__ ( type-name )
     && identifier

   (C11 permits _Alignof with type names only.)

   unary-operator: one of
     __extension__ __real__ __imag__

   Transactional Memory:

   unary-expression:
     transaction-expression

   In addition, the GNU syntax treats ++ and -- as unary operators, so
   they may be applied to cast expressions with errors for non-lvalues
   given later.  */

static struct c_expr
c_parser_unary_expression (c_parser *parser)
{
  int ext;
  struct c_expr ret, op;
  location_t op_loc = c_parser_peek_token (parser)->location;
  location_t exp_loc;
  location_t finish;
  ret.original_code = ERROR_MARK;
  ret.original_type = NULL;
  switch (c_parser_peek_token (parser)->type)
    {
    case CPP_PLUS_PLUS:
      c_parser_consume_token (parser);
      exp_loc = c_parser_peek_token (parser)->location;
      op = c_parser_cast_expression (parser, NULL);

      op = default_function_array_read_conversion (exp_loc, op);
      return parser_build_unary_op (op_loc, PREINCREMENT_EXPR, op);
    case CPP_MINUS_MINUS:
      c_parser_consume_token (parser);
      exp_loc = c_parser_peek_token (parser)->location;
      op = c_parser_cast_expression (parser, NULL);
      
      op = default_function_array_read_conversion (exp_loc, op);
      return parser_build_unary_op (op_loc, PREDECREMENT_EXPR, op);
    case CPP_AND:
      c_parser_consume_token (parser);
      op = c_parser_cast_expression (parser, NULL);
      mark_exp_read (op.value);
      return parser_build_unary_op (op_loc, ADDR_EXPR, op);
    case CPP_MULT:
      {
	c_parser_consume_token (parser);
	exp_loc = c_parser_peek_token (parser)->location;
	op = c_parser_cast_expression (parser, NULL);
	finish = op.get_finish ();
	op = convert_lvalue_to_rvalue (exp_loc, op, true, true);
	location_t combined_loc = make_location (op_loc, op_loc, finish);
	ret.value = build_indirect_ref (combined_loc, op.value, RO_UNARY_STAR);
	ret.src_range.m_start = op_loc;
	ret.src_range.m_finish = finish;
	return ret;
      }
    case CPP_PLUS:
      if (!c_dialect_objc () && !in_system_header_at (input_location))
	warning_at (op_loc,
		    OPT_Wtraditional,
		    "traditional C rejects the unary plus operator");
      c_parser_consume_token (parser);
      exp_loc = c_parser_peek_token (parser)->location;
      op = c_parser_cast_expression (parser, NULL);
      op = convert_lvalue_to_rvalue (exp_loc, op, true, true);
      return parser_build_unary_op (op_loc, CONVERT_EXPR, op);
    case CPP_MINUS:
      c_parser_consume_token (parser);
      exp_loc = c_parser_peek_token (parser)->location;
      op = c_parser_cast_expression (parser, NULL);
      op = convert_lvalue_to_rvalue (exp_loc, op, true, true);
      return parser_build_unary_op (op_loc, NEGATE_EXPR, op);
    case CPP_COMPL:
      c_parser_consume_token (parser);
      exp_loc = c_parser_peek_token (parser)->location;
      op = c_parser_cast_expression (parser, NULL);
      op = convert_lvalue_to_rvalue (exp_loc, op, true, true);
      return parser_build_unary_op (op_loc, BIT_NOT_EXPR, op);
    case CPP_NOT:
      c_parser_consume_token (parser);
      exp_loc = c_parser_peek_token (parser)->location;
      op = c_parser_cast_expression (parser, NULL);
      op = convert_lvalue_to_rvalue (exp_loc, op, true, true);
      return parser_build_unary_op (op_loc, TRUTH_NOT_EXPR, op);
    case CPP_AND_AND:
      /* Refer to the address of a label as a pointer.  */
      c_parser_consume_token (parser);
      if (c_parser_next_token_is (parser, CPP_NAME))
	{
	  ret.value = finish_label_address_expr
	    (c_parser_peek_token (parser)->value, op_loc);
	  set_c_expr_source_range (&ret, op_loc,
				   c_parser_peek_token (parser)->get_finish ());
	  c_parser_consume_token (parser);
	}
      else
	{
	  c_parser_error (parser, "expected identifier");
	  ret.set_error ();
	}
      return ret;
    case CPP_KEYWORD:
      switch (c_parser_peek_token (parser)->keyword)
	{
	case RID_SIZEOF:
	  return c_parser_sizeof_expression (parser);
	case RID_ALIGNOF:
	  return c_parser_alignof_expression (parser);
	case RID_BUILTIN_HAS_ATTRIBUTE:
	  return c_parser_has_attribute_expression (parser);
	case RID_EXTENSION:
	  c_parser_consume_token (parser);
	  ext = disable_extension_diagnostics ();
	  ret = c_parser_cast_expression (parser, NULL);
	  restore_extension_diagnostics (ext);
	  return ret;
	case RID_REALPART:
	  c_parser_consume_token (parser);
	  exp_loc = c_parser_peek_token (parser)->location;
	  op = c_parser_cast_expression (parser, NULL);
	  op = default_function_array_conversion (exp_loc, op);
	  return parser_build_unary_op (op_loc, REALPART_EXPR, op);
	case RID_IMAGPART:
	  c_parser_consume_token (parser);
	  exp_loc = c_parser_peek_token (parser)->location;
	  op = c_parser_cast_expression (parser, NULL);
	  op = default_function_array_conversion (exp_loc, op);
	  return parser_build_unary_op (op_loc, IMAGPART_EXPR, op);
	case RID_TRANSACTION_ATOMIC:
	case RID_TRANSACTION_RELAXED:
	  return c_parser_transaction_expression (parser,
	      c_parser_peek_token (parser)->keyword);
	default:
	  return c_parser_postfix_expression (parser);
	}
    default:
      return c_parser_postfix_expression (parser);
    }
}

/* Parse a sizeof expression.  */

static struct c_expr
c_parser_sizeof_expression (c_parser *parser)
{
  struct c_expr expr;
  struct c_expr result;
  location_t expr_loc;
  gcc_assert (c_parser_next_token_is_keyword (parser, RID_SIZEOF));

  location_t start;
  location_t finish = UNKNOWN_LOCATION;

  start = c_parser_peek_token (parser)->location;

  c_parser_consume_token (parser);
  c_inhibit_evaluation_warnings++;
  in_sizeof++;
  if (c_parser_next_token_is (parser, CPP_OPEN_PAREN)
      && c_token_starts_typename (c_parser_peek_2nd_token (parser)))
    {
      /* Either sizeof ( type-name ) or sizeof unary-expression
	 starting with a compound literal.  */
      struct c_type_name *type_name;
      matching_parens parens;
      parens.consume_open (parser);
      expr_loc = c_parser_peek_token (parser)->location;
      type_name = c_parser_type_name (parser, true);
      parens.skip_until_found_close (parser);
      finish = parser->tokens_buf[0].location;
      if (type_name == NULL)
	{
	  struct c_expr ret;
	  c_inhibit_evaluation_warnings--;
	  in_sizeof--;
	  ret.set_error ();
	  ret.original_code = ERROR_MARK;
	  ret.original_type = NULL;
	  return ret;
	}
      if (c_parser_next_token_is (parser, CPP_OPEN_BRACE))
	{
	  expr = c_parser_postfix_expression_after_paren_type (parser,
							       type_name,
							       expr_loc);
	  finish = expr.get_finish ();
	  goto sizeof_expr;
	}
      /* sizeof ( type-name ).  */
      if (type_name->specs->alignas_p)
	error_at (type_name->specs->locations[cdw_alignas],
		  "alignment specified for type name in %<sizeof%>");
      c_inhibit_evaluation_warnings--;
      in_sizeof--;
      result = c_expr_sizeof_type (expr_loc, type_name);
    }
  else
    {
      expr_loc = c_parser_peek_token (parser)->location;
      expr = c_parser_unary_expression (parser);
      finish = expr.get_finish ();
    sizeof_expr:
      c_inhibit_evaluation_warnings--;
      in_sizeof--;
      mark_exp_read (expr.value);
      if (TREE_CODE (expr.value) == COMPONENT_REF
	  && DECL_C_BIT_FIELD (TREE_OPERAND (expr.value, 1)))
	error_at (expr_loc, "%<sizeof%> applied to a bit-field");
      result = c_expr_sizeof_expr (expr_loc, expr);
    }
  if (finish == UNKNOWN_LOCATION)
    finish = start;
  set_c_expr_source_range (&result, start, finish);
  return result;
}

/* Parse an alignof expression.  */

static struct c_expr
c_parser_alignof_expression (c_parser *parser)
{
  struct c_expr expr;
  location_t start_loc = c_parser_peek_token (parser)->location;
  location_t end_loc;
  tree alignof_spelling = c_parser_peek_token (parser)->value;
  gcc_assert (c_parser_next_token_is_keyword (parser, RID_ALIGNOF));
  bool is_c11_alignof = strcmp (IDENTIFIER_POINTER (alignof_spelling),
				"_Alignof") == 0;
  /* A diagnostic is not required for the use of this identifier in
     the implementation namespace; only diagnose it for the C11
     spelling because of existing code using the other spellings.  */
  if (is_c11_alignof)
    {
      if (flag_isoc99)
	pedwarn_c99 (start_loc, OPT_Wpedantic, "ISO C99 does not support %qE",
		     alignof_spelling);
      else
	pedwarn_c99 (start_loc, OPT_Wpedantic, "ISO C90 does not support %qE",
		     alignof_spelling);
    }
  c_parser_consume_token (parser);
  c_inhibit_evaluation_warnings++;
  in_alignof++;
  if (c_parser_next_token_is (parser, CPP_OPEN_PAREN)
      && c_token_starts_typename (c_parser_peek_2nd_token (parser)))
    {
      /* Either __alignof__ ( type-name ) or __alignof__
	 unary-expression starting with a compound literal.  */
      location_t loc;
      struct c_type_name *type_name;
      struct c_expr ret;
      matching_parens parens;
      parens.consume_open (parser);
      loc = c_parser_peek_token (parser)->location;
      type_name = c_parser_type_name (parser, true);
      end_loc = c_parser_peek_token (parser)->location;
      parens.skip_until_found_close (parser);
      if (type_name == NULL)
	{
	  struct c_expr ret;
	  c_inhibit_evaluation_warnings--;
	  in_alignof--;
	  ret.set_error ();
	  ret.original_code = ERROR_MARK;
	  ret.original_type = NULL;
	  return ret;
	}
      if (c_parser_next_token_is (parser, CPP_OPEN_BRACE))
	{
	  expr = c_parser_postfix_expression_after_paren_type (parser,
							       type_name,
							       loc);
	  goto alignof_expr;
	}
      /* alignof ( type-name ).  */
      if (type_name->specs->alignas_p)
	error_at (type_name->specs->locations[cdw_alignas],
		  "alignment specified for type name in %qE",
		  alignof_spelling);
      c_inhibit_evaluation_warnings--;
      in_alignof--;
      ret.value = c_sizeof_or_alignof_type (loc, groktypename (type_name,
							       NULL, NULL),
					    false, is_c11_alignof, 1);
      ret.original_code = ERROR_MARK;
      ret.original_type = NULL;
      set_c_expr_source_range (&ret, start_loc, end_loc);
      return ret;
    }
  else
    {
      struct c_expr ret;
      expr = c_parser_unary_expression (parser);
      end_loc = expr.src_range.m_finish;
    alignof_expr:
      mark_exp_read (expr.value);
      c_inhibit_evaluation_warnings--;
      in_alignof--;
      if (is_c11_alignof)
	pedwarn (start_loc,
		 OPT_Wpedantic, "ISO C does not allow %<%E (expression)%>",
		 alignof_spelling);
      ret.value = c_alignof_expr (start_loc, expr.value);
      ret.original_code = ERROR_MARK;
      ret.original_type = NULL;
      set_c_expr_source_range (&ret, start_loc, end_loc);
      return ret;
    }
}

/* Parse the __builtin_has_attribute ([expr|type], attribute-spec)
   expression.  */

static struct c_expr
c_parser_has_attribute_expression (c_parser *parser)
{
  gcc_assert (c_parser_next_token_is_keyword (parser,
					      RID_BUILTIN_HAS_ATTRIBUTE));
  c_parser_consume_token (parser);

  c_inhibit_evaluation_warnings++;

  matching_parens parens;
  if (!parens.require_open (parser))
    {
      c_inhibit_evaluation_warnings--;
      in_typeof--;

      struct c_expr result;
      result.set_error ();
      result.original_code = ERROR_MARK;
      result.original_type = NULL;
      return result;
    }

  /* Treat the type argument the same way as in typeof for the purposes
     of warnings.  FIXME: Generalize this so the warning refers to
     __builtin_has_attribute rather than typeof.  */
  in_typeof++;

  /* The first operand: one of DECL, EXPR, or TYPE.  */
  tree oper = NULL_TREE;
  if (c_parser_next_tokens_start_typename (parser, cla_prefer_id))
    {
      struct c_type_name *tname = c_parser_type_name (parser);
      in_typeof--;
      if (tname)
	{
	  oper = groktypename (tname, NULL, NULL);
	  pop_maybe_used (variably_modified_type_p (oper, NULL_TREE));
	}
    }
  else
    {
      struct c_expr cexpr = c_parser_expr_no_commas (parser, NULL);
      c_inhibit_evaluation_warnings--;
      in_typeof--;
      if (cexpr.value != error_mark_node)
	{
	  mark_exp_read (cexpr.value);
	  oper = cexpr.value;
	  tree etype = TREE_TYPE (oper);
	  bool was_vm = variably_modified_type_p (etype, NULL_TREE);
	  /* This is returned with the type so that when the type is
	     evaluated, this can be evaluated.  */
	  if (was_vm)
	    oper = c_fully_fold (oper, false, NULL);
	  pop_maybe_used (was_vm);
	}
    }

  struct c_expr result;
  result.original_code = ERROR_MARK;
  result.original_type = NULL;

  if (!c_parser_require (parser, CPP_COMMA, "expected %<,%>"))
    {
      /* Consume the closing parenthesis if that's the next token
	 in the likely case the built-in was invoked with fewer
	 than two arguments.  */
      if (c_parser_next_token_is (parser, CPP_CLOSE_PAREN))
	c_parser_consume_token (parser);
      c_inhibit_evaluation_warnings--;
      result.set_error ();
      return result;
    }

  bool save_translate_strings_p = parser->translate_strings_p;

  location_t atloc = c_parser_peek_token (parser)->location;
  /* Parse a single attribute.  Require no leading comma and do not
     allow empty attributes.  */
  tree attr = c_parser_gnu_attribute (parser, NULL_TREE, false, false);

  parser->translate_strings_p = save_translate_strings_p;

  if (c_parser_next_token_is (parser, CPP_CLOSE_PAREN))
    c_parser_consume_token (parser);
  else
    {
      c_parser_error (parser, "expected identifier");
      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, NULL);

      result.set_error ();
      return result;
    }

  if (!attr)
    {
      error_at (atloc, "expected identifier");
      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
				 "expected %<)%>");
      result.set_error ();
      return result;
    }

  result.original_code = INTEGER_CST;
  result.original_type = boolean_type_node;

  if (has_attribute (atloc, oper, attr, default_conversion))
    result.value = boolean_true_node;
  else
    result.value =  boolean_false_node;

  return result;
}

/* Helper function to read arguments of builtins which are interfaces
   for the middle-end nodes like COMPLEX_EXPR, VEC_PERM_EXPR and
   others.  The name of the builtin is passed using BNAME parameter.
   Function returns true if there were no errors while parsing and
   stores the arguments in CEXPR_LIST.  If it returns true,
   *OUT_CLOSE_PAREN_LOC is written to with the location of the closing
   parenthesis.  */
static bool
c_parser_get_builtin_args (c_parser *parser, const char *bname,
			   vec<c_expr_t, va_gc> **ret_cexpr_list,
			   bool choose_expr_p,
			   location_t *out_close_paren_loc)
{
  location_t loc = c_parser_peek_token (parser)->location;
  vec<c_expr_t, va_gc> *cexpr_list;
  c_expr_t expr;
  bool saved_force_folding_builtin_constant_p;

  *ret_cexpr_list = NULL;
  if (c_parser_next_token_is_not (parser, CPP_OPEN_PAREN))
    {
      error_at (loc, "cannot take address of %qs", bname);
      return false;
    }

  c_parser_consume_token (parser);

  if (c_parser_next_token_is (parser, CPP_CLOSE_PAREN))
    {
      *out_close_paren_loc = c_parser_peek_token (parser)->location;
      c_parser_consume_token (parser);
      return true;
    }

  saved_force_folding_builtin_constant_p
    = force_folding_builtin_constant_p;
  force_folding_builtin_constant_p |= choose_expr_p;
  expr = c_parser_expr_no_commas (parser, NULL);
  force_folding_builtin_constant_p
    = saved_force_folding_builtin_constant_p;
  vec_alloc (cexpr_list, 1);
  vec_safe_push (cexpr_list, expr);
  while (c_parser_next_token_is (parser, CPP_COMMA))
    {
      c_parser_consume_token (parser);
      expr = c_parser_expr_no_commas (parser, NULL);
      vec_safe_push (cexpr_list, expr);
    }

  *out_close_paren_loc = c_parser_peek_token (parser)->location;
  if (!c_parser_require (parser, CPP_CLOSE_PAREN, "expected %<)%>"))
    return false;

  *ret_cexpr_list = cexpr_list;
  return true;
}

/* This represents a single generic-association.  */

struct c_generic_association
{
  /* The location of the starting token of the type.  */
  location_t type_location;
  /* The association's type, or NULL_TREE for 'default'.  */
  tree type;
  /* The association's expression.  */
  struct c_expr expression;
};

/* Parse a generic-selection.  (C11 6.5.1.1).
   
   generic-selection:
     _Generic ( assignment-expression , generic-assoc-list )
     
   generic-assoc-list:
     generic-association
     generic-assoc-list , generic-association
   
   generic-association:
     type-name : assignment-expression
     default : assignment-expression
*/

static struct c_expr
c_parser_generic_selection (c_parser *parser)
{
  struct c_expr selector, error_expr;
  tree selector_type;
  struct c_generic_association matched_assoc;
  int match_found = -1;
  location_t generic_loc, selector_loc;

  error_expr.original_code = ERROR_MARK;
  error_expr.original_type = NULL;
  error_expr.set_error ();
  matched_assoc.type_location = UNKNOWN_LOCATION;
  matched_assoc.type = NULL_TREE;
  matched_assoc.expression = error_expr;

  gcc_assert (c_parser_next_token_is_keyword (parser, RID_GENERIC));
  generic_loc = c_parser_peek_token (parser)->location;
  c_parser_consume_token (parser);
  if (flag_isoc99)
    pedwarn_c99 (generic_loc, OPT_Wpedantic,
		 "ISO C99 does not support %<_Generic%>");
  else
    pedwarn_c99 (generic_loc, OPT_Wpedantic,
		 "ISO C90 does not support %<_Generic%>");

  matching_parens parens;
  if (!parens.require_open (parser))
    return error_expr;

  c_inhibit_evaluation_warnings++;
  selector_loc = c_parser_peek_token (parser)->location;
  selector = c_parser_expr_no_commas (parser, NULL);
  selector = default_function_array_conversion (selector_loc, selector);
  c_inhibit_evaluation_warnings--;

  if (selector.value == error_mark_node)
    {
      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, NULL);
      return selector;
    }
  mark_exp_read (selector.value);
  selector_type = TREE_TYPE (selector.value);
  /* In ISO C terms, rvalues (including the controlling expression of
     _Generic) do not have qualified types.  */
  if (TREE_CODE (selector_type) != ARRAY_TYPE)
    selector_type = TYPE_MAIN_VARIANT (selector_type);
  /* In ISO C terms, _Noreturn is not part of the type of expressions
     such as &abort, but in GCC it is represented internally as a type
     qualifier.  */
  if (FUNCTION_POINTER_TYPE_P (selector_type)
      && TYPE_QUALS (TREE_TYPE (selector_type)) != TYPE_UNQUALIFIED)
    selector_type
      = build_pointer_type (TYPE_MAIN_VARIANT (TREE_TYPE (selector_type)));

  if (!c_parser_require (parser, CPP_COMMA, "expected %<,%>"))
    {
      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, NULL);
      return error_expr;
    }

  auto_vec<c_generic_association> associations;
  while (1)
    {
      struct c_generic_association assoc, *iter;
      unsigned int ix;
      c_token *token = c_parser_peek_token (parser);

      assoc.type_location = token->location;
      if (token->type == CPP_KEYWORD && token->keyword == RID_DEFAULT)
	{
	  c_parser_consume_token (parser);
	  assoc.type = NULL_TREE;
	}
      else
	{
	  struct c_type_name *type_name;

	  type_name = c_parser_type_name (parser);
	  if (type_name == NULL)
	    {
	      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, NULL);
	      return error_expr;
	    }
	  assoc.type = groktypename (type_name, NULL, NULL);
	  if (assoc.type == error_mark_node)
	    {
	      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, NULL);
	      return error_expr;
	    }

	  if (TREE_CODE (assoc.type) == FUNCTION_TYPE)
	    error_at (assoc.type_location,
		      "%<_Generic%> association has function type");
	  else if (!COMPLETE_TYPE_P (assoc.type))
	    error_at (assoc.type_location,
		      "%<_Generic%> association has incomplete type");

	  if (variably_modified_type_p (assoc.type, NULL_TREE))
	    error_at (assoc.type_location,
		      "%<_Generic%> association has "
		      "variable length type");
	}

      if (!c_parser_require (parser, CPP_COLON, "expected %<:%>"))
	{
	  c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, NULL);
	  return error_expr;
	}

      assoc.expression = c_parser_expr_no_commas (parser, NULL);
      if (assoc.expression.value == error_mark_node)
	{
	  c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, NULL);
	  return error_expr;
	}

      for (ix = 0; associations.iterate (ix, &iter); ++ix)
	{
	  if (assoc.type == NULL_TREE)
	    {
	      if (iter->type == NULL_TREE)
		{
		  error_at (assoc.type_location,
			    "duplicate %<default%> case in %<_Generic%>");
		  inform (iter->type_location, "original %<default%> is here");
		}
	    }
	  else if (iter->type != NULL_TREE)
	    {
	      if (comptypes (assoc.type, iter->type))
		{
		  error_at (assoc.type_location,
			    "%<_Generic%> specifies two compatible types");
		  inform (iter->type_location, "compatible type is here");
		}
	    }
	}

      if (assoc.type == NULL_TREE)
	{
	  if (match_found < 0)
	    {
	      matched_assoc = assoc;
	      match_found = associations.length ();
	    }
	}
      else if (comptypes (assoc.type, selector_type))
	{
	  if (match_found < 0 || matched_assoc.type == NULL_TREE)
	    {
	      matched_assoc = assoc;
	      match_found = associations.length ();
	    }
	  else
	    {
	      error_at (assoc.type_location,
			"%<_Generic%> selector matches multiple associations");
	      inform (matched_assoc.type_location,
		      "other match is here");
	    }
	}

      associations.safe_push (assoc);

      if (c_parser_peek_token (parser)->type != CPP_COMMA)
	break;
      c_parser_consume_token (parser);
    }

  unsigned int ix;
  struct c_generic_association *iter;
  FOR_EACH_VEC_ELT (associations, ix, iter)
    if (ix != (unsigned) match_found)
      mark_exp_read (iter->expression.value);

  if (!parens.require_close (parser))
    {
      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, NULL);
      return error_expr;
    }

  if (match_found < 0)
    {
      error_at (selector_loc, "%<_Generic%> selector of type %qT is not "
		"compatible with any association",
		selector_type);
      return error_expr;
    }

  return matched_assoc.expression;
}

/* Check the validity of a function pointer argument *EXPR (argument
   position POS) to __builtin_tgmath.  Return the number of function
   arguments if possibly valid; return 0 having reported an error if
   not valid.  */

static unsigned int
check_tgmath_function (c_expr *expr, unsigned int pos)
{
  tree type = TREE_TYPE (expr->value);
  if (!FUNCTION_POINTER_TYPE_P (type))
    {
      error_at (expr->get_location (),
		"argument %u of %<__builtin_tgmath%> is not a function pointer",
		pos);
      return 0;
    }
  type = TREE_TYPE (type);
  if (!prototype_p (type))
    {
      error_at (expr->get_location (),
		"argument %u of %<__builtin_tgmath%> is unprototyped", pos);
      return 0;
    }
  if (stdarg_p (type))
    {
      error_at (expr->get_location (),
		"argument %u of %<__builtin_tgmath%> has variable arguments",
		pos);
      return 0;
    }
  unsigned int nargs = 0;
  function_args_iterator iter;
  tree t;
  FOREACH_FUNCTION_ARGS (type, t, iter)
    {
      if (t == void_type_node)
	break;
      nargs++;
    }
  if (nargs == 0)
    {
      error_at (expr->get_location (),
		"argument %u of %<__builtin_tgmath%> has no arguments", pos);
      return 0;
    }
  return nargs;
}

/* Ways in which a parameter or return value of a type-generic macro
   may vary between the different functions the macro may call.  */
enum tgmath_parm_kind
  {
    tgmath_fixed, tgmath_real, tgmath_complex
  };

/* Helper function for c_parser_postfix_expression.  Parse predefined
   identifiers.  */

static struct c_expr
c_parser_predefined_identifier (c_parser *parser)
{
  location_t loc = c_parser_peek_token (parser)->location;
  switch (c_parser_peek_token (parser)->keyword)
    {
    case RID_FUNCTION_NAME:
      pedwarn (loc, OPT_Wpedantic, "ISO C does not support %qs predefined "
	       "identifier", "__FUNCTION__");
      break;
    case RID_PRETTY_FUNCTION_NAME:
      pedwarn (loc, OPT_Wpedantic, "ISO C does not support %qs predefined "
	       "identifier", "__PRETTY_FUNCTION__");
      break;
    case RID_C99_FUNCTION_NAME:
      pedwarn_c90 (loc, OPT_Wpedantic, "ISO C90 does not support "
		   "%<__func__%> predefined identifier");
      break;
    default:
      gcc_unreachable ();
    }

  struct c_expr expr;
  expr.original_code = ERROR_MARK;
  expr.original_type = NULL;
  expr.value = fname_decl (loc, c_parser_peek_token (parser)->keyword,
			   c_parser_peek_token (parser)->value);
  set_c_expr_source_range (&expr, loc, loc);
  c_parser_consume_token (parser);
  return expr;
}

/* Parse a postfix expression (C90 6.3.1-6.3.2, C99 6.5.1-6.5.2,
   C11 6.5.1-6.5.2).  Compound literals aren't handled here; callers have to
   call c_parser_postfix_expression_after_paren_type on encountering them.

   postfix-expression:
     primary-expression
     postfix-expression [ expression ]
     postfix-expression ( argument-expression-list[opt] )
     postfix-expression . identifier
     postfix-expression -> identifier
     postfix-expression ++
     postfix-expression --
     ( type-name ) { initializer-list }
     ( type-name ) { initializer-list , }

   argument-expression-list:
     argument-expression
     argument-expression-list , argument-expression

   primary-expression:
     identifier
     constant
     string-literal
     ( expression )
     generic-selection

   GNU extensions:

   primary-expression:
     __func__
       (treated as a keyword in GNU C)
     __FUNCTION__
     __PRETTY_FUNCTION__
     ( compound-statement )
     __builtin_va_arg ( assignment-expression , type-name )
     __builtin_offsetof ( type-name , offsetof-member-designator )
     __builtin_choose_expr ( assignment-expression ,
			     assignment-expression ,
			     assignment-expression )
     __builtin_types_compatible_p ( type-name , type-name )
     __builtin_tgmath ( expr-list )
     __builtin_complex ( assignment-expression , assignment-expression )
     __builtin_shuffle ( assignment-expression , assignment-expression )
     __builtin_shuffle ( assignment-expression ,
			 assignment-expression ,
			 assignment-expression, )
     __builtin_convertvector ( assignment-expression , type-name )

   offsetof-member-designator:
     identifier
     offsetof-member-designator . identifier
     offsetof-member-designator [ expression ]

   Objective-C:

   primary-expression:
     [ objc-receiver objc-message-args ]
     @selector ( objc-selector-arg )
     @protocol ( identifier )
     @encode ( type-name )
     objc-string-literal
     Classname . identifier
*/

static struct c_expr
c_parser_postfix_expression (c_parser *parser)
{
  struct c_expr expr, e1;
  struct c_type_name *t1, *t2;
  location_t loc = c_parser_peek_token (parser)->location;
  source_range tok_range = c_parser_peek_token (parser)->get_range ();
  expr.original_code = ERROR_MARK;
  expr.original_type = NULL;
  switch (c_parser_peek_token (parser)->type)
    {
    case CPP_NUMBER:
      expr.value = c_parser_peek_token (parser)->value;
      set_c_expr_source_range (&expr, tok_range);
      loc = c_parser_peek_token (parser)->location;
      c_parser_consume_token (parser);
      if (TREE_CODE (expr.value) == FIXED_CST
	  && !targetm.fixed_point_supported_p ())
	{
	  error_at (loc, "fixed-point types not supported for this target");
	  expr.set_error ();
	}
      break;
    case CPP_CHAR:
    case CPP_CHAR16:
    case CPP_CHAR32:
    case CPP_UTF8CHAR:
    case CPP_WCHAR:
      expr.value = c_parser_peek_token (parser)->value;
      /* For the purpose of warning when a pointer is compared with
	 a zero character constant.  */
      expr.original_type = char_type_node;
      set_c_expr_source_range (&expr, tok_range);
      c_parser_consume_token (parser);
      break;
    case CPP_STRING:
    case CPP_STRING16:
    case CPP_STRING32:
    case CPP_WSTRING:
    case CPP_UTF8STRING:
      expr = c_parser_string_literal (parser, parser->translate_strings_p,
				      true);
      break;
    case CPP_OBJC_STRING:
      gcc_assert (c_dialect_objc ());
      expr.value
	= objc_build_string_object (c_parser_peek_token (parser)->value);
      set_c_expr_source_range (&expr, tok_range);
      c_parser_consume_token (parser);
      break;
    case CPP_NAME:
      switch (c_parser_peek_token (parser)->id_kind)
	{
	case C_ID_ID:
	  {
	    tree id = c_parser_peek_token (parser)->value;
	    c_parser_consume_token (parser);
	    expr.value = build_external_ref (loc, id,
					     (c_parser_peek_token (parser)->type
					      == CPP_OPEN_PAREN),
					     &expr.original_type);
	    set_c_expr_source_range (&expr, tok_range);
	    break;
	  }
	case C_ID_CLASSNAME:
	  {
	    /* Here we parse the Objective-C 2.0 Class.name dot
	       syntax.  */
	    tree class_name = c_parser_peek_token (parser)->value;
	    tree component;
	    c_parser_consume_token (parser);
	    gcc_assert (c_dialect_objc ());
	    if (!c_parser_require (parser, CPP_DOT, "expected %<.%>"))
	      {
		expr.set_error ();
		break;
	      }
	    if (c_parser_next_token_is_not (parser, CPP_NAME))
	      {
		c_parser_error (parser, "expected identifier");
		expr.set_error ();
		break;
	      }
	    c_token *component_tok = c_parser_peek_token (parser);
	    component = component_tok->value;
	    location_t end_loc = component_tok->get_finish ();
	    c_parser_consume_token (parser);
	    expr.value = objc_build_class_component_ref (class_name, 
							 component);
	    set_c_expr_source_range (&expr, loc, end_loc);
	    break;
	  }
	default:
	  c_parser_error (parser, "expected expression");
	  expr.set_error ();
	  break;
	}
      break;
    case CPP_OPEN_PAREN:
      /* A parenthesized expression, statement expression or compound
	 literal.  */
      if (c_parser_peek_2nd_token (parser)->type == CPP_OPEN_BRACE)
	{
	  /* A statement expression.  */
	  tree stmt;
	  location_t brace_loc;
	  c_parser_consume_token (parser);
	  brace_loc = c_parser_peek_token (parser)->location;
	  c_parser_consume_token (parser);
	  /* If we've not yet started the current function's statement list,
	     or we're in the parameter scope of an old-style function
	     declaration, statement expressions are not allowed.  */
	  if (!building_stmt_list_p () || old_style_parameter_scope ())
	    {
	      error_at (loc, "braced-group within expression allowed "
			"only inside a function");
	      parser->error = true;
	      c_parser_skip_until_found (parser, CPP_CLOSE_BRACE, NULL);
	      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, NULL);
	      expr.set_error ();
	      break;
	    }
	  stmt = c_begin_stmt_expr ();
	  c_parser_compound_statement_nostart (parser);
	  location_t close_loc = c_parser_peek_token (parser)->location;
	  c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
				     "expected %<)%>");
	  pedwarn (loc, OPT_Wpedantic,
		   "ISO C forbids braced-groups within expressions");
	  expr.value = c_finish_stmt_expr (brace_loc, stmt);
	  set_c_expr_source_range (&expr, loc, close_loc);
	  mark_exp_read (expr.value);
	}
      else
	{
	  /* A parenthesized expression.  */
	  location_t loc_open_paren = c_parser_peek_token (parser)->location;
	  c_parser_consume_token (parser);
	  expr = c_parser_expression (parser);
	  if (TREE_CODE (expr.value) == MODIFY_EXPR)
	    TREE_NO_WARNING (expr.value) = 1;
	  if (expr.original_code != C_MAYBE_CONST_EXPR
	      && expr.original_code != SIZEOF_EXPR)
	    expr.original_code = ERROR_MARK;
	  /* Remember that we saw ( ) around the sizeof.  */
	  if (expr.original_code == SIZEOF_EXPR)
	    expr.original_code = PAREN_SIZEOF_EXPR;
	  /* Don't change EXPR.ORIGINAL_TYPE.  */
	  location_t loc_close_paren = c_parser_peek_token (parser)->location;
	  set_c_expr_source_range (&expr, loc_open_paren, loc_close_paren);
	  c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
				     "expected %<)%>", loc_open_paren);
	}
      break;
    case CPP_KEYWORD:
      switch (c_parser_peek_token (parser)->keyword)
	{
	case RID_FUNCTION_NAME:
	case RID_PRETTY_FUNCTION_NAME:
	case RID_C99_FUNCTION_NAME:
	  expr = c_parser_predefined_identifier (parser);
	  break;
	case RID_VA_ARG:
	  {
	    location_t start_loc = loc;
	    c_parser_consume_token (parser);
	    matching_parens parens;
	    if (!parens.require_open (parser))
	      {
		expr.set_error ();
		break;
	      }
	    e1 = c_parser_expr_no_commas (parser, NULL);
	    mark_exp_read (e1.value);
	    e1.value = c_fully_fold (e1.value, false, NULL);
	    if (!c_parser_require (parser, CPP_COMMA, "expected %<,%>"))
	      {
		c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, NULL);
		expr.set_error ();
		break;
	      }
	    loc = c_parser_peek_token (parser)->location;
	    t1 = c_parser_type_name (parser);
	    location_t end_loc = c_parser_peek_token (parser)->get_finish ();
	    c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
				       "expected %<)%>");
	    if (t1 == NULL)
	      {
		expr.set_error ();
	      }
	    else
	      {
		tree type_expr = NULL_TREE;
		expr.value = c_build_va_arg (start_loc, e1.value, loc,
					     groktypename (t1, &type_expr, NULL));
		if (type_expr)
		  {
		    expr.value = build2 (C_MAYBE_CONST_EXPR,
					 TREE_TYPE (expr.value), type_expr,
					 expr.value);
		    C_MAYBE_CONST_EXPR_NON_CONST (expr.value) = true;
		  }
		set_c_expr_source_range (&expr, start_loc, end_loc);
	      }
	  }
	  break;
	case RID_OFFSETOF:
	  {
	    c_parser_consume_token (parser);
	    matching_parens parens;
	    if (!parens.require_open (parser))
	      {
		expr.set_error ();
		break;
	      }
	    t1 = c_parser_type_name (parser);
	    if (t1 == NULL)
	      parser->error = true;
	    if (!c_parser_require (parser, CPP_COMMA, "expected %<,%>"))
	      gcc_assert (parser->error);
	    if (parser->error)
	      {
		c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, NULL);
		expr.set_error ();
		break;
	      }
	    tree type = groktypename (t1, NULL, NULL);
	    tree offsetof_ref;
	    if (type == error_mark_node)
	      offsetof_ref = error_mark_node;
	    else
	      {
		offsetof_ref = build1 (INDIRECT_REF, type, null_pointer_node);
		SET_EXPR_LOCATION (offsetof_ref, loc);
	      }
	    /* Parse the second argument to __builtin_offsetof.  We
	       must have one identifier, and beyond that we want to
	       accept sub structure and sub array references.  */
	    if (c_parser_next_token_is (parser, CPP_NAME))
	      {
		c_token *comp_tok = c_parser_peek_token (parser);
		offsetof_ref = build_component_ref
		  (loc, offsetof_ref, comp_tok->value, comp_tok->location);
		c_parser_consume_token (parser);
		while (c_parser_next_token_is (parser, CPP_DOT)
		       || c_parser_next_token_is (parser,
						  CPP_OPEN_SQUARE)
		       || c_parser_next_token_is (parser,
						  CPP_DEREF))
		  {
		    if (c_parser_next_token_is (parser, CPP_DEREF))
		      {
			loc = c_parser_peek_token (parser)->location;
			offsetof_ref = build_array_ref (loc,
							offsetof_ref,
							integer_zero_node);
			goto do_dot;
		      }
		    else if (c_parser_next_token_is (parser, CPP_DOT))
		      {
		      do_dot:
			c_parser_consume_token (parser);
			if (c_parser_next_token_is_not (parser,
							CPP_NAME))
			  {
			    c_parser_error (parser, "expected identifier");
			    break;
			  }
			c_token *comp_tok = c_parser_peek_token (parser);
			offsetof_ref = build_component_ref
			  (loc, offsetof_ref, comp_tok->value,
			   comp_tok->location);
			c_parser_consume_token (parser);
		      }
		    else
		      {
			struct c_expr ce;
			tree idx;
			loc = c_parser_peek_token (parser)->location;
			c_parser_consume_token (parser);
			ce = c_parser_expression (parser);
			ce = convert_lvalue_to_rvalue (loc, ce, false, false);
			idx = ce.value;
			idx = c_fully_fold (idx, false, NULL);
			c_parser_skip_until_found (parser, CPP_CLOSE_SQUARE,
						   "expected %<]%>");
			offsetof_ref = build_array_ref (loc, offsetof_ref, idx);
		      }
		  }
	      }
	    else
	      c_parser_error (parser, "expected identifier");
	    location_t end_loc = c_parser_peek_token (parser)->get_finish ();
	    c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
				       "expected %<)%>");
	    expr.value = fold_offsetof (offsetof_ref);
	    set_c_expr_source_range (&expr, loc, end_loc);
	  }
	  break;
	case RID_CHOOSE_EXPR:
	  {
	    vec<c_expr_t, va_gc> *cexpr_list;
	    c_expr_t *e1_p, *e2_p, *e3_p;
	    tree c;
	    location_t close_paren_loc;

	    c_parser_consume_token (parser);
	    if (!c_parser_get_builtin_args (parser,
					    "__builtin_choose_expr",
					    &cexpr_list, true,
					    &close_paren_loc))
	      {
		expr.set_error ();
		break;
	      }

	    if (vec_safe_length (cexpr_list) != 3)
	      {
		error_at (loc, "wrong number of arguments to "
			       "%<__builtin_choose_expr%>");
		expr.set_error ();
		break;
	      }

	    e1_p = &(*cexpr_list)[0];
	    e2_p = &(*cexpr_list)[1];
	    e3_p = &(*cexpr_list)[2];

	    c = e1_p->value;
	    mark_exp_read (e2_p->value);
	    mark_exp_read (e3_p->value);
	    if (TREE_CODE (c) != INTEGER_CST
		|| !INTEGRAL_TYPE_P (TREE_TYPE (c)))
	      error_at (loc,
			"first argument to %<__builtin_choose_expr%> not"
			" a constant");
	    constant_expression_warning (c);
	    expr = integer_zerop (c) ? *e3_p : *e2_p;
	    set_c_expr_source_range (&expr, loc, close_paren_loc);
	    break;
	  }
	case RID_TYPES_COMPATIBLE_P:
	  {
	    c_parser_consume_token (parser);
	    matching_parens parens;
	    if (!parens.require_open (parser))
	      {
		expr.set_error ();
		break;
	      }
	    t1 = c_parser_type_name (parser);
	    if (t1 == NULL)
	      {
		expr.set_error ();
		break;
	      }
	    if (!c_parser_require (parser, CPP_COMMA, "expected %<,%>"))
	      {
		c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, NULL);
		expr.set_error ();
		break;
	      }
	    t2 = c_parser_type_name (parser);
	    if (t2 == NULL)
	      {
		expr.set_error ();
		break;
	      }
	    location_t close_paren_loc = c_parser_peek_token (parser)->location;
	    parens.skip_until_found_close (parser);
	    tree e1, e2;
	    e1 = groktypename (t1, NULL, NULL);
	    e2 = groktypename (t2, NULL, NULL);
	    if (e1 == error_mark_node || e2 == error_mark_node)
	      {
		expr.set_error ();
		break;
	      }

	    e1 = TYPE_MAIN_VARIANT (e1);
	    e2 = TYPE_MAIN_VARIANT (e2);

	    expr.value
	      = comptypes (e1, e2) ? integer_one_node : integer_zero_node;
	    set_c_expr_source_range (&expr, loc, close_paren_loc);
	  }
	  break;
	case RID_BUILTIN_TGMATH:
	  {
	    vec<c_expr_t, va_gc> *cexpr_list;
	    location_t close_paren_loc;

	    c_parser_consume_token (parser);
	    if (!c_parser_get_builtin_args (parser,
					    "__builtin_tgmath",
					    &cexpr_list, false,
					    &close_paren_loc))
	      {
		expr.set_error ();
		break;
	      }

	    if (vec_safe_length (cexpr_list) < 3)
	      {
		error_at (loc, "too few arguments to %<__builtin_tgmath%>");
		expr.set_error ();
		break;
	      }

	    unsigned int i;
	    c_expr_t *p;
	    FOR_EACH_VEC_ELT (*cexpr_list, i, p)
	      *p = convert_lvalue_to_rvalue (loc, *p, true, true);
	    unsigned int nargs = check_tgmath_function (&(*cexpr_list)[0], 1);
	    if (nargs == 0)
	      {
		expr.set_error ();
		break;
	      }
	    if (vec_safe_length (cexpr_list) < nargs)
	      {
		error_at (loc, "too few arguments to %<__builtin_tgmath%>");
		expr.set_error ();
		break;
	      }
	    unsigned int num_functions = vec_safe_length (cexpr_list) - nargs;
	    if (num_functions < 2)
	      {
		error_at (loc, "too few arguments to %<__builtin_tgmath%>");
		expr.set_error ();
		break;
	      }

	    /* The first NUM_FUNCTIONS expressions are the function
	       pointers.  The remaining NARGS expressions are the
	       arguments that are to be passed to one of those
	       functions, chosen following <tgmath.h> rules.  */
	    for (unsigned int j = 1; j < num_functions; j++)
	      {
		unsigned int this_nargs
		  = check_tgmath_function (&(*cexpr_list)[j], j + 1);
		if (this_nargs == 0)
		  {
		    expr.set_error ();
		    goto out;
		  }
		if (this_nargs != nargs)
		  {
		    error_at ((*cexpr_list)[j].get_location (),
			      "argument %u of %<__builtin_tgmath%> has "
			      "wrong number of arguments", j + 1);
		    expr.set_error ();
		    goto out;
		  }
	      }

	    /* The functions all have the same number of arguments.
	       Determine whether arguments and return types vary in
	       ways permitted for <tgmath.h> functions.  */
	    /* The first entry in each of these vectors is for the
	       return type, subsequent entries for parameter
	       types.  */
	    auto_vec<enum tgmath_parm_kind> parm_kind (nargs + 1);
	    auto_vec<tree> parm_first (nargs + 1);
	    auto_vec<bool> parm_complex (nargs + 1);
	    auto_vec<bool> parm_varies (nargs + 1);
	    tree first_type = TREE_TYPE (TREE_TYPE ((*cexpr_list)[0].value));
	    tree first_ret = TYPE_MAIN_VARIANT (TREE_TYPE (first_type));
	    parm_first.quick_push (first_ret);
	    parm_complex.quick_push (TREE_CODE (first_ret) == COMPLEX_TYPE);
	    parm_varies.quick_push (false);
	    function_args_iterator iter;
	    tree t;
	    unsigned int argpos;
	    FOREACH_FUNCTION_ARGS (first_type, t, iter)
	      {
		if (t == void_type_node)
		  break;
		parm_first.quick_push (TYPE_MAIN_VARIANT (t));
		parm_complex.quick_push (TREE_CODE (t) == COMPLEX_TYPE);
		parm_varies.quick_push (false);
	      }
	    for (unsigned int j = 1; j < num_functions; j++)
	      {
		tree type = TREE_TYPE (TREE_TYPE ((*cexpr_list)[j].value));
		tree ret = TYPE_MAIN_VARIANT (TREE_TYPE (type));
		if (ret != parm_first[0])
		  {
		    parm_varies[0] = true;
		    if (!SCALAR_FLOAT_TYPE_P (parm_first[0])
			&& !COMPLEX_FLOAT_TYPE_P (parm_first[0]))
		      {
			error_at ((*cexpr_list)[0].get_location (),
				  "invalid type-generic return type for "
				  "argument %u of %<__builtin_tgmath%>",
				  1);
			expr.set_error ();
			goto out;
		      }
		    if (!SCALAR_FLOAT_TYPE_P (ret)
			&& !COMPLEX_FLOAT_TYPE_P (ret))
		      {
			error_at ((*cexpr_list)[j].get_location (),
				  "invalid type-generic return type for "
				  "argument %u of %<__builtin_tgmath%>",
				  j + 1);
			expr.set_error ();
			goto out;
		      }
		  }
		if (TREE_CODE (ret) == COMPLEX_TYPE)
		  parm_complex[0] = true;
		argpos = 1;
		FOREACH_FUNCTION_ARGS (type, t, iter)
		  {
		    if (t == void_type_node)
		      break;
		    t = TYPE_MAIN_VARIANT (t);
		    if (t != parm_first[argpos])
		      {
			parm_varies[argpos] = true;
			if (!SCALAR_FLOAT_TYPE_P (parm_first[argpos])
			    && !COMPLEX_FLOAT_TYPE_P (parm_first[argpos]))
			  {
			    error_at ((*cexpr_list)[0].get_location (),
				      "invalid type-generic type for "
				      "argument %u of argument %u of "
				      "%<__builtin_tgmath%>", argpos, 1);
			    expr.set_error ();
			    goto out;
			  }
			if (!SCALAR_FLOAT_TYPE_P (t)
			    && !COMPLEX_FLOAT_TYPE_P (t))
			  {
			    error_at ((*cexpr_list)[j].get_location (),
				      "invalid type-generic type for "
				      "argument %u of argument %u of "
				      "%<__builtin_tgmath%>", argpos, j + 1);
			    expr.set_error ();
			    goto out;
			  }
		      }
		    if (TREE_CODE (t) == COMPLEX_TYPE)
		      parm_complex[argpos] = true;
		    argpos++;
		  }
	      }
	    enum tgmath_parm_kind max_variation = tgmath_fixed;
	    for (unsigned int j = 0; j <= nargs; j++)
	      {
		enum tgmath_parm_kind this_kind;
		if (parm_varies[j])
		  {
		    if (parm_complex[j])
		      max_variation = this_kind = tgmath_complex;
		    else
		      {
			this_kind = tgmath_real;
			if (max_variation != tgmath_complex)
			  max_variation = tgmath_real;
		      }
		  }
		else
		  this_kind = tgmath_fixed;
		parm_kind.quick_push (this_kind);
	      }
	    if (max_variation == tgmath_fixed)
	      {
		error_at (loc, "function arguments of %<__builtin_tgmath%> "
			  "all have the same type");
		expr.set_error ();
		break;
	      }

	    /* Identify a parameter (not the return type) that varies,
	       including with complex types if any variation includes
	       complex types; there must be at least one such
	       parameter.  */
	    unsigned int tgarg = 0;
	    for (unsigned int j = 1; j <= nargs; j++)
	      if (parm_kind[j] == max_variation)
		{
		  tgarg = j;
		  break;
		}
	    if (tgarg == 0)
	      {
		error_at (loc, "function arguments of %<__builtin_tgmath%> "
			  "lack type-generic parameter");
		expr.set_error ();
		break;
	      }

	    /* Determine the type of the relevant parameter for each
	       function.  */
	    auto_vec<tree> tg_type (num_functions);
	    for (unsigned int j = 0; j < num_functions; j++)
	      {
		tree type = TREE_TYPE (TREE_TYPE ((*cexpr_list)[j].value));
		argpos = 1;
		FOREACH_FUNCTION_ARGS (type, t, iter)
		  {
		    if (argpos == tgarg)
		      {
			tg_type.quick_push (TYPE_MAIN_VARIANT (t));
			break;
		      }
		    argpos++;
		  }
	      }

	    /* Verify that the corresponding types are different for
	       all the listed functions.  Also determine whether all
	       the types are complex, whether all the types are
	       standard or binary, and whether all the types are
	       decimal.  */
	    bool all_complex = true;
	    bool all_binary = true;
	    bool all_decimal = true;
	    hash_set<tree> tg_types;
	    FOR_EACH_VEC_ELT (tg_type, i, t)
	      {
		if (TREE_CODE (t) == COMPLEX_TYPE)
		  all_decimal = false;
		else
		  {
		    all_complex = false;
		    if (DECIMAL_FLOAT_TYPE_P (t))
		      all_binary = false;
		    else
		      all_decimal = false;
		  }
		if (tg_types.add (t))
		  {
		    error_at ((*cexpr_list)[i].get_location (),
			      "duplicate type-generic parameter type for "
			      "function argument %u of %<__builtin_tgmath%>",
			      i + 1);
		    expr.set_error ();
		    goto out;
		  }
	      }

	    /* Verify that other parameters and the return type whose
	       types vary have their types varying in the correct
	       way.  */
	    for (unsigned int j = 0; j < num_functions; j++)
	      {
		tree exp_type = tg_type[j];
		tree exp_real_type = exp_type;
		if (TREE_CODE (exp_type) == COMPLEX_TYPE)
		  exp_real_type = TREE_TYPE (exp_type);
		tree type = TREE_TYPE (TREE_TYPE ((*cexpr_list)[j].value));
		tree ret = TYPE_MAIN_VARIANT (TREE_TYPE (type));
		if ((parm_kind[0] == tgmath_complex && ret != exp_type)
		    || (parm_kind[0] == tgmath_real && ret != exp_real_type))
		  {
		    error_at ((*cexpr_list)[j].get_location (),
			      "bad return type for function argument %u "
			      "of %<__builtin_tgmath%>", j + 1);
		    expr.set_error ();
		    goto out;
		  }
		argpos = 1;
		FOREACH_FUNCTION_ARGS (type, t, iter)
		  {
		    if (t == void_type_node)
		      break;
		    t = TYPE_MAIN_VARIANT (t);
		    if ((parm_kind[argpos] == tgmath_complex
			 && t != exp_type)
			|| (parm_kind[argpos] == tgmath_real
			    && t != exp_real_type))
		      {
			error_at ((*cexpr_list)[j].get_location (),
				  "bad type for argument %u of "
				  "function argument %u of "
				  "%<__builtin_tgmath%>", argpos, j + 1);
			expr.set_error ();
			goto out;
		      }
		    argpos++;
		  }
	      }

	    /* The functions listed are a valid set of functions for a
	       <tgmath.h> macro to select between.  Identify the
	       matching function, if any.  First, the argument types
	       must be combined following <tgmath.h> rules.  Integer
	       types are treated as _Decimal64 if any type-generic
	       argument is decimal, or if the only alternatives for
	       type-generic arguments are of decimal types, and are
	       otherwise treated as double (or _Complex double for
	       complex integer types, or _Float64 or _Complex _Float64
	       if all the return types are the same _FloatN or
	       _FloatNx type).  After that adjustment, types are
	       combined following the usual arithmetic conversions.
	       If the function only accepts complex arguments, a
	       complex type is produced.  */
	    bool arg_complex = all_complex;
	    bool arg_binary = all_binary;
	    bool arg_int_decimal = all_decimal;
	    for (unsigned int j = 1; j <= nargs; j++)
	      {
		if (parm_kind[j] == tgmath_fixed)
		  continue;
		c_expr_t *ce = &(*cexpr_list)[num_functions + j - 1];
		tree type = TREE_TYPE (ce->value);
		if (!INTEGRAL_TYPE_P (type)
		    && !SCALAR_FLOAT_TYPE_P (type)
		    && TREE_CODE (type) != COMPLEX_TYPE)
		  {
		    error_at (ce->get_location (),
			      "invalid type of argument %u of type-generic "
			      "function", j);
		    expr.set_error ();
		    goto out;
		  }
		if (DECIMAL_FLOAT_TYPE_P (type))
		  {
		    arg_int_decimal = true;
		    if (all_complex)
		      {
			error_at (ce->get_location (),
				  "decimal floating-point argument %u to "
				  "complex-only type-generic function", j);
			expr.set_error ();
			goto out;
		      }
		    else if (all_binary)
		      {
			error_at (ce->get_location (),
				  "decimal floating-point argument %u to "
				  "binary-only type-generic function", j);
			expr.set_error ();
			goto out;
		      }
		    else if (arg_complex)
		      {
			error_at (ce->get_location (),
				  "both complex and decimal floating-point "
				  "arguments to type-generic function");
			expr.set_error ();
			goto out;
		      }
		    else if (arg_binary)
		      {
			error_at (ce->get_location (),
				  "both binary and decimal floating-point "
				  "arguments to type-generic function");
			expr.set_error ();
			goto out;
		      }
		  }
		else if (TREE_CODE (type) == COMPLEX_TYPE)
		  {
		    arg_complex = true;
		    if (COMPLEX_FLOAT_TYPE_P (type))
		      arg_binary = true;
		    if (all_decimal)
		      {
			error_at (ce->get_location (),
				  "complex argument %u to "
				  "decimal-only type-generic function", j);
			expr.set_error ();
			goto out;
		      }
		    else if (arg_int_decimal)
		      {
			error_at (ce->get_location (),
				  "both complex and decimal floating-point "
				  "arguments to type-generic function");
			expr.set_error ();
			goto out;
		      }
		  }
		else if (SCALAR_FLOAT_TYPE_P (type))
		  {
		    arg_binary = true;
		    if (all_decimal)
		      {
			error_at (ce->get_location (),
				  "binary argument %u to "
				  "decimal-only type-generic function", j);
			expr.set_error ();
			goto out;
		      }
		    else if (arg_int_decimal)
		      {
			error_at (ce->get_location (),
				  "both binary and decimal floating-point "
				  "arguments to type-generic function");
			expr.set_error ();
			goto out;
		      }
		  }
	      }
	    /* For a macro rounding its result to a narrower type, map
	       integer types to _Float64 not double if the return type
	       is a _FloatN or _FloatNx type.  */
	    bool arg_int_float64 = false;
	    if (parm_kind[0] == tgmath_fixed
		&& SCALAR_FLOAT_TYPE_P (parm_first[0])
		&& float64_type_node != NULL_TREE)
	      for (unsigned int j = 0; j < NUM_FLOATN_NX_TYPES; j++)
		if (parm_first[0] == FLOATN_TYPE_NODE (j))
		  {
		    arg_int_float64 = true;
		    break;
		  }
	    tree arg_real = NULL_TREE;
	    for (unsigned int j = 1; j <= nargs; j++)
	      {
		if (parm_kind[j] == tgmath_fixed)
		  continue;
		c_expr_t *ce = &(*cexpr_list)[num_functions + j - 1];
		tree type = TYPE_MAIN_VARIANT (TREE_TYPE (ce->value));
		if (TREE_CODE (type) == COMPLEX_TYPE)
		  type = TREE_TYPE (type);
		if (INTEGRAL_TYPE_P (type))
		  type = (arg_int_decimal
			  ? dfloat64_type_node
			  : arg_int_float64
			  ? float64_type_node
			  : double_type_node);
		if (arg_real == NULL_TREE)
		  arg_real = type;
		else
		  arg_real = common_type (arg_real, type);
		if (arg_real == error_mark_node)
		  {
		    expr.set_error ();
		    goto out;
		  }
	      }
	    tree arg_type = (arg_complex
			     ? build_complex_type (arg_real)
			     : arg_real);

	    /* Look for a function to call with type-generic parameter
	       type ARG_TYPE.  */
	    c_expr_t *fn = NULL;
	    for (unsigned int j = 0; j < num_functions; j++)
	      {
		if (tg_type[j] == arg_type)
		  {
		    fn = &(*cexpr_list)[j];
		    break;
		  }
	      }
	    if (fn == NULL
		&& parm_kind[0] == tgmath_fixed
		&& SCALAR_FLOAT_TYPE_P (parm_first[0]))
	      {
		/* Presume this is a macro that rounds its result to a
		   narrower type, and look for the first function with
		   at least the range and precision of the argument
		   type.  */
		for (unsigned int j = 0; j < num_functions; j++)
		  {
		    if (arg_complex
			!= (TREE_CODE (tg_type[j]) == COMPLEX_TYPE))
		      continue;
		    tree real_tg_type = (arg_complex
					 ? TREE_TYPE (tg_type[j])
					 : tg_type[j]);
		    if (DECIMAL_FLOAT_TYPE_P (arg_real)
			!= DECIMAL_FLOAT_TYPE_P (real_tg_type))
		      continue;
		    scalar_float_mode arg_mode
		      = SCALAR_FLOAT_TYPE_MODE (arg_real);
		    scalar_float_mode tg_mode
		      = SCALAR_FLOAT_TYPE_MODE (real_tg_type);
		    const real_format *arg_fmt = REAL_MODE_FORMAT (arg_mode);
		    const real_format *tg_fmt = REAL_MODE_FORMAT (tg_mode);
		    if (arg_fmt->b == tg_fmt->b
			&& arg_fmt->p <= tg_fmt->p
			&& arg_fmt->emax <= tg_fmt->emax
			&& (arg_fmt->emin - arg_fmt->p
			    >= tg_fmt->emin - tg_fmt->p))
		      {
			fn = &(*cexpr_list)[j];
			break;
		      }
		  }
	      }
	    if (fn == NULL)
	      {
		error_at (loc, "no matching function for type-generic call");
		expr.set_error ();
		break;
	      }

	    /* Construct a call to FN.  */
	    vec<tree, va_gc> *args;
	    vec_alloc (args, nargs);
	    vec<tree, va_gc> *origtypes;
	    vec_alloc (origtypes, nargs);
	    auto_vec<location_t> arg_loc (nargs);
	    for (unsigned int j = 0; j < nargs; j++)
	      {
		c_expr_t *ce = &(*cexpr_list)[num_functions + j];
		args->quick_push (ce->value);
		arg_loc.quick_push (ce->get_location ());
		origtypes->quick_push (ce->original_type);
	      }
	    expr.value = c_build_function_call_vec (loc, arg_loc, fn->value,
						    args, origtypes);
	    set_c_expr_source_range (&expr, loc, close_paren_loc);
	    break;
	  }
	case RID_BUILTIN_CALL_WITH_STATIC_CHAIN:
	  {
	    vec<c_expr_t, va_gc> *cexpr_list;
	    c_expr_t *e2_p;
	    tree chain_value;
	    location_t close_paren_loc;

	    c_parser_consume_token (parser);
	    if (!c_parser_get_builtin_args (parser,
					    "__builtin_call_with_static_chain",
					    &cexpr_list, false,
					    &close_paren_loc))
	      {
		expr.set_error ();
		break;
	      }
	    if (vec_safe_length (cexpr_list) != 2)
	      {
		error_at (loc, "wrong number of arguments to "
			       "%<__builtin_call_with_static_chain%>");
		expr.set_error ();
		break;
	      }

	    expr = (*cexpr_list)[0];
	    e2_p = &(*cexpr_list)[1];
	    *e2_p = convert_lvalue_to_rvalue (loc, *e2_p, true, true);
	    chain_value = e2_p->value;
	    mark_exp_read (chain_value);

	    if (TREE_CODE (expr.value) != CALL_EXPR)
	      error_at (loc, "first argument to "
			"%<__builtin_call_with_static_chain%> "
			"must be a call expression");
	    else if (TREE_CODE (TREE_TYPE (chain_value)) != POINTER_TYPE)
	      error_at (loc, "second argument to "
			"%<__builtin_call_with_static_chain%> "
			"must be a pointer type");
	    else
	      CALL_EXPR_STATIC_CHAIN (expr.value) = chain_value;
	    set_c_expr_source_range (&expr, loc, close_paren_loc);
	    break;
	  }
	case RID_BUILTIN_COMPLEX:
	  {
	    vec<c_expr_t, va_gc> *cexpr_list;
	    c_expr_t *e1_p, *e2_p;
	    location_t close_paren_loc;

	    c_parser_consume_token (parser);
	    if (!c_parser_get_builtin_args (parser,
					    "__builtin_complex",
					    &cexpr_list, false,
					    &close_paren_loc))
	      {
		expr.set_error ();
		break;
	      }

	    if (vec_safe_length (cexpr_list) != 2)
	      {
		error_at (loc, "wrong number of arguments to "
			       "%<__builtin_complex%>");
		expr.set_error ();
		break;
	      }

	    e1_p = &(*cexpr_list)[0];
	    e2_p = &(*cexpr_list)[1];

	    *e1_p = convert_lvalue_to_rvalue (loc, *e1_p, true, true);
	    if (TREE_CODE (e1_p->value) == EXCESS_PRECISION_EXPR)
	      e1_p->value = convert (TREE_TYPE (e1_p->value),
				     TREE_OPERAND (e1_p->value, 0));
	    *e2_p = convert_lvalue_to_rvalue (loc, *e2_p, true, true);
	    if (TREE_CODE (e2_p->value) == EXCESS_PRECISION_EXPR)
	      e2_p->value = convert (TREE_TYPE (e2_p->value),
				     TREE_OPERAND (e2_p->value, 0));
	    if (!SCALAR_FLOAT_TYPE_P (TREE_TYPE (e1_p->value))
		|| DECIMAL_FLOAT_TYPE_P (TREE_TYPE (e1_p->value))
		|| !SCALAR_FLOAT_TYPE_P (TREE_TYPE (e2_p->value))
		|| DECIMAL_FLOAT_TYPE_P (TREE_TYPE (e2_p->value)))
	      {
		error_at (loc, "%<__builtin_complex%> operand "
			  "not of real binary floating-point type");
		expr.set_error ();
		break;
	      }
	    if (TYPE_MAIN_VARIANT (TREE_TYPE (e1_p->value))
		!= TYPE_MAIN_VARIANT (TREE_TYPE (e2_p->value)))
	      {
		error_at (loc,
			  "%<__builtin_complex%> operands of different types");
		expr.set_error ();
		break;
	      }
	    pedwarn_c90 (loc, OPT_Wpedantic,
			 "ISO C90 does not support complex types");
	    expr.value = build2_loc (loc, COMPLEX_EXPR,
				     build_complex_type
				     (TYPE_MAIN_VARIANT
				      (TREE_TYPE (e1_p->value))),
				     e1_p->value, e2_p->value);
	    set_c_expr_source_range (&expr, loc, close_paren_loc);
	    break;
	  }
	case RID_BUILTIN_SHUFFLE:
	  {
	    vec<c_expr_t, va_gc> *cexpr_list;
	    unsigned int i;
	    c_expr_t *p;
	    location_t close_paren_loc;

	    c_parser_consume_token (parser);
	    if (!c_parser_get_builtin_args (parser,
					    "__builtin_shuffle",
					    &cexpr_list, false,
					    &close_paren_loc))
	      {
		expr.set_error ();
		break;
	      }

	    FOR_EACH_VEC_SAFE_ELT (cexpr_list, i, p)
	      *p = convert_lvalue_to_rvalue (loc, *p, true, true);

	    if (vec_safe_length (cexpr_list) == 2)
	      expr.value = c_build_vec_perm_expr (loc, (*cexpr_list)[0].value,
						  NULL_TREE,
						  (*cexpr_list)[1].value);

	    else if (vec_safe_length (cexpr_list) == 3)
	      expr.value = c_build_vec_perm_expr (loc, (*cexpr_list)[0].value,
						  (*cexpr_list)[1].value,
						  (*cexpr_list)[2].value);
	    else
	      {
		error_at (loc, "wrong number of arguments to "
			       "%<__builtin_shuffle%>");
		expr.set_error ();
	      }
	    set_c_expr_source_range (&expr, loc, close_paren_loc);
	    break;
	  }
	case RID_BUILTIN_CONVERTVECTOR:
	  {
	    location_t start_loc = loc;
	    c_parser_consume_token (parser);
	    matching_parens parens;
	    if (!parens.require_open (parser))
	      {
		expr.set_error ();
		break;
	      }
	    e1 = c_parser_expr_no_commas (parser, NULL);
	    mark_exp_read (e1.value);
	    if (!c_parser_require (parser, CPP_COMMA, "expected %<,%>"))
	      {
		c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, NULL);
		expr.set_error ();
		break;
	      }
	    loc = c_parser_peek_token (parser)->location;
	    t1 = c_parser_type_name (parser);
	    location_t end_loc = c_parser_peek_token (parser)->get_finish ();
	    c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
				       "expected %<)%>");
	    if (t1 == NULL)
	      expr.set_error ();
	    else
	      {
		tree type_expr = NULL_TREE;
		expr.value = c_build_vec_convert (start_loc, e1.value, loc,
						  groktypename (t1, &type_expr,
								NULL));
		set_c_expr_source_range (&expr, start_loc, end_loc);
	      }
	  }
	  break;
	case RID_AT_SELECTOR:
	  {
	    gcc_assert (c_dialect_objc ());
	    c_parser_consume_token (parser);
	    matching_parens parens;
	    if (!parens.require_open (parser))
	      {
		expr.set_error ();
		break;
	      }
	    tree sel = c_parser_objc_selector_arg (parser);
	    location_t close_loc = c_parser_peek_token (parser)->location;
	    parens.skip_until_found_close (parser);
	    expr.value = objc_build_selector_expr (loc, sel);
	    set_c_expr_source_range (&expr, loc, close_loc);
	  }
	  break;
	case RID_AT_PROTOCOL:
	  {
	    gcc_assert (c_dialect_objc ());
	    c_parser_consume_token (parser);
	    matching_parens parens;
	    if (!parens.require_open (parser))
	      {
		expr.set_error ();
		break;
	      }
	    if (c_parser_next_token_is_not (parser, CPP_NAME))
	      {
		c_parser_error (parser, "expected identifier");
		c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, NULL);
		expr.set_error ();
		break;
	      }
	    tree id = c_parser_peek_token (parser)->value;
	    c_parser_consume_token (parser);
	    location_t close_loc = c_parser_peek_token (parser)->location;
	    parens.skip_until_found_close (parser);
	    expr.value = objc_build_protocol_expr (id);
	    set_c_expr_source_range (&expr, loc, close_loc);
	  }
	  break;
	case RID_AT_ENCODE:
	  {
	    /* Extension to support C-structures in the archiver.  */
	    gcc_assert (c_dialect_objc ());
	    c_parser_consume_token (parser);
	    matching_parens parens;
	    if (!parens.require_open (parser))
	      {
		expr.set_error ();
		break;
	      }
	    t1 = c_parser_type_name (parser);
	    if (t1 == NULL)
	      {
		expr.set_error ();
		c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, NULL);
		break;
	      }
	    location_t close_loc = c_parser_peek_token (parser)->location;
	    parens.skip_until_found_close (parser);
	    tree type = groktypename (t1, NULL, NULL);
	    expr.value = objc_build_encode_expr (type);
	    set_c_expr_source_range (&expr, loc, close_loc);
	  }
	  break;
	case RID_GENERIC:
	  expr = c_parser_generic_selection (parser);
	  break;
	default:
	  c_parser_error (parser, "expected expression");
	  expr.set_error ();
	  break;
	}
      break;
    case CPP_OPEN_SQUARE:
      if (c_dialect_objc ())
	{
	  tree receiver, args;
	  c_parser_consume_token (parser);
	  receiver = c_parser_objc_receiver (parser);
	  args = c_parser_objc_message_args (parser);
	  location_t close_loc = c_parser_peek_token (parser)->location;
	  c_parser_skip_until_found (parser, CPP_CLOSE_SQUARE,
				     "expected %<]%>");
	  expr.value = objc_build_message_expr (receiver, args);
	  set_c_expr_source_range (&expr, loc, close_loc);
	  break;
	}
      /* Else fall through to report error.  */
      /* FALLTHRU */
    default:
      c_parser_error (parser, "expected expression");
      expr.set_error ();
      break;
    }
 out:
  return c_parser_postfix_expression_after_primary
    (parser, EXPR_LOC_OR_LOC (expr.value, loc), expr);
}

/* Parse a postfix expression after a parenthesized type name: the
   brace-enclosed initializer of a compound literal, possibly followed
   by some postfix operators.  This is separate because it is not
   possible to tell until after the type name whether a cast
   expression has a cast or a compound literal, or whether the operand
   of sizeof is a parenthesized type name or starts with a compound
   literal.  TYPE_LOC is the location where TYPE_NAME starts--the
   location of the first token after the parentheses around the type
   name.  */

static struct c_expr
c_parser_postfix_expression_after_paren_type (c_parser *parser,
					      struct c_type_name *type_name,
					      location_t type_loc)
{
  tree type;
  struct c_expr init;
  bool non_const;
  struct c_expr expr;
  location_t start_loc;
  tree type_expr = NULL_TREE;
  bool type_expr_const = true;
  check_compound_literal_type (type_loc, type_name);
  rich_location richloc (line_table, type_loc);
  start_init (NULL_TREE, NULL, 0, &richloc);
  type = groktypename (type_name, &type_expr, &type_expr_const);
  start_loc = c_parser_peek_token (parser)->location;
  if (type != error_mark_node && C_TYPE_VARIABLE_SIZE (type))
    {
      error_at (type_loc, "compound literal has variable size");
      type = error_mark_node;
    }
  init = c_parser_braced_init (parser, type, false, NULL);
  finish_init ();
  maybe_warn_string_init (type_loc, type, init);

  if (type != error_mark_node
      && !ADDR_SPACE_GENERIC_P (TYPE_ADDR_SPACE (type))
      && current_function_decl)
    {
      error ("compound literal qualified by address-space qualifier");
      type = error_mark_node;
    }

  pedwarn_c90 (start_loc, OPT_Wpedantic, "ISO C90 forbids compound literals");
  non_const = ((init.value && TREE_CODE (init.value) == CONSTRUCTOR)
	       ? CONSTRUCTOR_NON_CONST (init.value)
	       : init.original_code == C_MAYBE_CONST_EXPR);
  non_const |= !type_expr_const;
  unsigned int alignas_align = 0;
  if (type != error_mark_node
      && type_name->specs->align_log != -1)
    {
      alignas_align = 1U << type_name->specs->align_log;
      if (alignas_align < min_align_of_type (type))
	{
	  error_at (type_name->specs->locations[cdw_alignas],
		    "%<_Alignas%> specifiers cannot reduce "
		    "alignment of compound literal");
	  alignas_align = 0;
	}
    }
  expr.value = build_compound_literal (start_loc, type, init.value, non_const,
				       alignas_align);
  set_c_expr_source_range (&expr, init.src_range);
  expr.original_code = ERROR_MARK;
  expr.original_type = NULL;
  if (type != error_mark_node
      && expr.value != error_mark_node
      && type_expr)
    {
      if (TREE_CODE (expr.value) == C_MAYBE_CONST_EXPR)
	{
	  gcc_assert (C_MAYBE_CONST_EXPR_PRE (expr.value) == NULL_TREE);
	  C_MAYBE_CONST_EXPR_PRE (expr.value) = type_expr;
	}
      else
	{
	  gcc_assert (!non_const);
	  expr.value = build2 (C_MAYBE_CONST_EXPR, type,
			       type_expr, expr.value);
	}
    }
  return c_parser_postfix_expression_after_primary (parser, start_loc, expr);
}

/* Callback function for sizeof_pointer_memaccess_warning to compare
   types.  */

static bool
sizeof_ptr_memacc_comptypes (tree type1, tree type2)
{
  return comptypes (type1, type2) == 1;
}

/* Warn for patterns where abs-like function appears to be used incorrectly,
   gracefully ignore any non-abs-like function.  The warning location should
   be LOC.  FNDECL is the declaration of called function, it must be a
   BUILT_IN_NORMAL function.  ARG is the first and only argument of the
   call.  */

static void
warn_for_abs (location_t loc, tree fndecl, tree arg)
{
  /* Avoid warning in unreachable subexpressions.  */
  if (c_inhibit_evaluation_warnings)
    return;

  tree atype = TREE_TYPE (arg);

  /* Casts from pointers (and thus arrays and fndecls) will generate
     -Wint-conversion warnings.  Most other wrong types hopefully lead to type
     mismatch errors.  TODO: Think about what to do with FIXED_POINT_TYPE_P
     types and possibly other exotic types.  */
  if (!INTEGRAL_TYPE_P (atype)
      && !SCALAR_FLOAT_TYPE_P (atype)
      && TREE_CODE (atype) != COMPLEX_TYPE)
    return;

  enum built_in_function fcode = DECL_FUNCTION_CODE (fndecl);

  switch (fcode)
    {
    case BUILT_IN_ABS:
    case BUILT_IN_LABS:
    case BUILT_IN_LLABS:
    case BUILT_IN_IMAXABS:
      if (!INTEGRAL_TYPE_P (atype))
	{
	  if (SCALAR_FLOAT_TYPE_P (atype))
	    warning_at (loc, OPT_Wabsolute_value,
			"using integer absolute value function %qD when "
			"argument is of floating-point type %qT",
			fndecl, atype);
	  else if (TREE_CODE (atype) == COMPLEX_TYPE)
	    warning_at (loc, OPT_Wabsolute_value,
			"using integer absolute value function %qD when "
			"argument is of complex type %qT", fndecl, atype);
	  else
	    gcc_unreachable ();
	  return;
	}
      if (TYPE_UNSIGNED (atype))
	warning_at (loc, OPT_Wabsolute_value,
		    "taking the absolute value of unsigned type %qT "
		    "has no effect", atype);
      break;

    CASE_FLT_FN (BUILT_IN_FABS):
    CASE_FLT_FN_FLOATN_NX (BUILT_IN_FABS):
      if (!SCALAR_FLOAT_TYPE_P (atype)
	  || DECIMAL_FLOAT_MODE_P (TYPE_MODE (atype)))
	{
	  if (INTEGRAL_TYPE_P (atype))
	    warning_at (loc, OPT_Wabsolute_value,
			"using floating-point absolute value function %qD "
			"when argument is of integer type %qT", fndecl, atype);
	  else if (DECIMAL_FLOAT_TYPE_P (atype))
	    warning_at (loc, OPT_Wabsolute_value,
			"using floating-point absolute value function %qD "
			"when argument is of decimal floating-point type %qT",
			fndecl, atype);
	  else if (TREE_CODE (atype) == COMPLEX_TYPE)
	    warning_at (loc, OPT_Wabsolute_value,
			"using floating-point absolute value function %qD when "
			"argument is of complex type %qT", fndecl, atype);
	  else
	    gcc_unreachable ();
	  return;
	}
      break;

    CASE_FLT_FN (BUILT_IN_CABS):
      if (TREE_CODE (atype) != COMPLEX_TYPE)
	{
	  if (INTEGRAL_TYPE_P (atype))
	    warning_at (loc, OPT_Wabsolute_value,
			"using complex absolute value function %qD when "
			"argument is of integer type %qT", fndecl, atype);
	  else if (SCALAR_FLOAT_TYPE_P (atype))
	    warning_at (loc, OPT_Wabsolute_value,
			"using complex absolute value function %qD when "
			"argument is of floating-point type %qT",
			fndecl, atype);
	  else
	    gcc_unreachable ();

	  return;
	}
      break;

    case BUILT_IN_FABSD32:
    case BUILT_IN_FABSD64:
    case BUILT_IN_FABSD128:
      if (!DECIMAL_FLOAT_TYPE_P (atype))
	{
	  if (INTEGRAL_TYPE_P (atype))
	    warning_at (loc, OPT_Wabsolute_value,
			"using decimal floating-point absolute value "
			"function %qD when argument is of integer type %qT",
			fndecl, atype);
	  else if (SCALAR_FLOAT_TYPE_P (atype))
	    warning_at (loc, OPT_Wabsolute_value,
			"using decimal floating-point absolute value "
			"function %qD when argument is of floating-point "
			"type %qT", fndecl, atype);
	  else if (TREE_CODE (atype) == COMPLEX_TYPE)
	    warning_at (loc, OPT_Wabsolute_value,
			"using decimal floating-point absolute value "
			"function %qD when argument is of complex type %qT",
			fndecl, atype);
	  else
	    gcc_unreachable ();
	  return;
	}
      break;

    default:
      return;
    }

  if (!TYPE_ARG_TYPES (TREE_TYPE (fndecl)))
    return;

  tree ftype = TREE_VALUE (TYPE_ARG_TYPES (TREE_TYPE (fndecl)));
  if (TREE_CODE (atype) == COMPLEX_TYPE)
    {
      gcc_assert (TREE_CODE (ftype) == COMPLEX_TYPE);
      atype = TREE_TYPE (atype);
      ftype = TREE_TYPE (ftype);
    }

  if (TYPE_PRECISION (ftype) < TYPE_PRECISION (atype))
    warning_at (loc, OPT_Wabsolute_value,
		"absolute value function %qD given an argument of type %qT "
		"but has parameter of type %qT which may cause truncation "
		"of value", fndecl, atype, ftype);
}


/* Parse a postfix expression after the initial primary or compound
   literal; that is, parse a series of postfix operators.

   EXPR_LOC is the location of the primary expression.  */

static struct c_expr
c_parser_postfix_expression_after_primary (c_parser *parser,
					   location_t expr_loc,
					   struct c_expr expr)
{
  struct c_expr orig_expr;
  tree ident, idx;
  location_t sizeof_arg_loc[3], comp_loc;
  tree sizeof_arg[3];
  unsigned int literal_zero_mask;
  unsigned int i;
  vec<tree, va_gc> *exprlist;
  vec<tree, va_gc> *origtypes = NULL;
  vec<location_t> arg_loc = vNULL;
  location_t start;
  location_t finish;

  while (true)
    {
      location_t op_loc = c_parser_peek_token (parser)->location;
      switch (c_parser_peek_token (parser)->type)
	{
	case CPP_OPEN_SQUARE:
	  /* Array reference.  */
	  c_parser_consume_token (parser);
	  idx = c_parser_expression (parser).value;
	  c_parser_skip_until_found (parser, CPP_CLOSE_SQUARE,
				     "expected %<]%>");
	  start = expr.get_start ();
	  finish = parser->tokens_buf[0].location;
	  expr.value = build_array_ref (op_loc, expr.value, idx);
	  set_c_expr_source_range (&expr, start, finish);
	  expr.original_code = ERROR_MARK;
	  expr.original_type = NULL;
	  break;
	case CPP_OPEN_PAREN:
	  /* Function call.  */
	  {
	    matching_parens parens;
	    parens.consume_open (parser);
	    for (i = 0; i < 3; i++)
	      {
		sizeof_arg[i] = NULL_TREE;
		sizeof_arg_loc[i] = UNKNOWN_LOCATION;
	      }
	    literal_zero_mask = 0;
	    if (c_parser_next_token_is (parser, CPP_CLOSE_PAREN))
	      exprlist = NULL;
	    else
	      exprlist = c_parser_expr_list (parser, true, false, &origtypes,
					     sizeof_arg_loc, sizeof_arg,
					     &arg_loc, &literal_zero_mask);
	    parens.skip_until_found_close (parser);
	  }
	  orig_expr = expr;
	  mark_exp_read (expr.value);
	  if (warn_sizeof_pointer_memaccess)
	    sizeof_pointer_memaccess_warning (sizeof_arg_loc,
					      expr.value, exprlist,
					      sizeof_arg,
					      sizeof_ptr_memacc_comptypes);
	  if (TREE_CODE (expr.value) == FUNCTION_DECL)
	    {
	      if (fndecl_built_in_p (expr.value, BUILT_IN_MEMSET)
		  && vec_safe_length (exprlist) == 3)
		{
		  tree arg0 = (*exprlist)[0];
		  tree arg2 = (*exprlist)[2];
		  warn_for_memset (expr_loc, arg0, arg2, literal_zero_mask);
		}
	      if (warn_absolute_value
		  && fndecl_built_in_p (expr.value, BUILT_IN_NORMAL)
		  && vec_safe_length (exprlist) == 1)
		warn_for_abs (expr_loc, expr.value, (*exprlist)[0]);
	    }

	  start = expr.get_start ();
	  finish = parser->tokens_buf[0].get_finish ();
	  expr.value
	    = c_build_function_call_vec (expr_loc, arg_loc, expr.value,
					 exprlist, origtypes);
	  set_c_expr_source_range (&expr, start, finish);

	  expr.original_code = ERROR_MARK;
	  if (TREE_CODE (expr.value) == INTEGER_CST
	      && TREE_CODE (orig_expr.value) == FUNCTION_DECL
	      && fndecl_built_in_p (orig_expr.value, BUILT_IN_CONSTANT_P))
	    expr.original_code = C_MAYBE_CONST_EXPR;
	  expr.original_type = NULL;
	  if (exprlist)
	    {
	      release_tree_vector (exprlist);
	      release_tree_vector (origtypes);
	    }
	  arg_loc.release ();
	  break;
	case CPP_DOT:
	  /* Structure element reference.  */
	  c_parser_consume_token (parser);
	  expr = default_function_array_conversion (expr_loc, expr);
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
	case CPP_DEREF:
	  /* Structure element reference.  */
	  c_parser_consume_token (parser);
	  expr = convert_lvalue_to_rvalue (expr_loc, expr, true, false);
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
					    build_indirect_ref (op_loc,
								expr.value,
								RO_ARROW),
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
	case CPP_PLUS_PLUS:
	  /* Postincrement.  */
	  start = expr.get_start ();
	  finish = c_parser_peek_token (parser)->get_finish ();
	  c_parser_consume_token (parser);
	  expr = default_function_array_read_conversion (expr_loc, expr);
	  expr.value = build_unary_op (op_loc, POSTINCREMENT_EXPR,
				       expr.value, false);
	  set_c_expr_source_range (&expr, start, finish);
	  expr.original_code = ERROR_MARK;
	  expr.original_type = NULL;
	  break;
	case CPP_MINUS_MINUS:
	  /* Postdecrement.  */
	  start = expr.get_start ();
	  finish = c_parser_peek_token (parser)->get_finish ();
	  c_parser_consume_token (parser);
	  expr = default_function_array_read_conversion (expr_loc, expr);
	  expr.value = build_unary_op (op_loc, POSTDECREMENT_EXPR,
				       expr.value, false);
	  set_c_expr_source_range (&expr, start, finish);
	  expr.original_code = ERROR_MARK;
	  expr.original_type = NULL;
	  break;
	default:
	  return expr;
	}
    }
}

/* Parse an expression (C90 6.3.17, C99 6.5.17, C11 6.5.17).

   expression:
     assignment-expression
     expression , assignment-expression
*/

static struct c_expr
c_parser_expression (c_parser *parser)
{
  location_t tloc = c_parser_peek_token (parser)->location;
  struct c_expr expr;
  expr = c_parser_expr_no_commas (parser, NULL);
  if (c_parser_next_token_is (parser, CPP_COMMA))
    expr = convert_lvalue_to_rvalue (tloc, expr, true, false);
  while (c_parser_next_token_is (parser, CPP_COMMA))
    {
      struct c_expr next;
      tree lhsval;
      location_t loc = c_parser_peek_token (parser)->location;
      location_t expr_loc;
      c_parser_consume_token (parser);
      expr_loc = c_parser_peek_token (parser)->location;
      lhsval = expr.value;
      while (TREE_CODE (lhsval) == COMPOUND_EXPR
	     || TREE_CODE (lhsval) == NOP_EXPR)
	{
	  if (TREE_CODE (lhsval) == COMPOUND_EXPR)
	    lhsval = TREE_OPERAND (lhsval, 1);
	  else
	    lhsval = TREE_OPERAND (lhsval, 0);
	}
      if (DECL_P (lhsval) || handled_component_p (lhsval))
	mark_exp_read (lhsval);
      next = c_parser_expr_no_commas (parser, NULL);
      next = convert_lvalue_to_rvalue (expr_loc, next, true, false);
      expr.value = build_compound_expr (loc, expr.value, next.value);
      expr.original_code = COMPOUND_EXPR;
      expr.original_type = next.original_type;
    }
  return expr;
}

/* Parse an expression and convert functions or arrays to pointers and
   lvalues to rvalues.  */

static struct c_expr
c_parser_expression_conv (c_parser *parser)
{
  struct c_expr expr;
  location_t loc = c_parser_peek_token (parser)->location;
  expr = c_parser_expression (parser);
  expr = convert_lvalue_to_rvalue (loc, expr, true, false);
  return expr;
}

/* Helper function of c_parser_expr_list.  Check if IDXth (0 based)
   argument is a literal zero alone and if so, set it in literal_zero_mask.  */

static inline void
c_parser_check_literal_zero (c_parser *parser, unsigned *literal_zero_mask,
			     unsigned int idx)
{
  if (idx >= HOST_BITS_PER_INT)
    return;

  c_token *tok = c_parser_peek_token (parser);
  switch (tok->type)
    {
    case CPP_NUMBER:
    case CPP_CHAR:
    case CPP_WCHAR:
    case CPP_CHAR16:
    case CPP_CHAR32:
    case CPP_UTF8CHAR:
      /* If a parameter is literal zero alone, remember it
	 for -Wmemset-transposed-args warning.  */
      if (integer_zerop (tok->value)
	  && !TREE_OVERFLOW (tok->value)
	  && (c_parser_peek_2nd_token (parser)->type == CPP_COMMA
	      || c_parser_peek_2nd_token (parser)->type == CPP_CLOSE_PAREN))
	*literal_zero_mask |= 1U << idx;
    default:
      break;
    }
}

/* Parse a non-empty list of expressions.  If CONVERT_P, convert
   functions and arrays to pointers and lvalues to rvalues.  If
   FOLD_P, fold the expressions.  If LOCATIONS is non-NULL, save the
   locations of function arguments into this vector.

   nonempty-expr-list:
     assignment-expression
     nonempty-expr-list , assignment-expression
*/

static vec<tree, va_gc> *
c_parser_expr_list (c_parser *parser, bool convert_p, bool fold_p,
		    vec<tree, va_gc> **p_orig_types,
		    location_t *sizeof_arg_loc, tree *sizeof_arg,
		    vec<location_t> *locations,
		    unsigned int *literal_zero_mask)
{
  vec<tree, va_gc> *ret;
  vec<tree, va_gc> *orig_types;
  struct c_expr expr;
  unsigned int idx = 0;

  ret = make_tree_vector ();
  if (p_orig_types == NULL)
    orig_types = NULL;
  else
    orig_types = make_tree_vector ();

  if (literal_zero_mask)
    c_parser_check_literal_zero (parser, literal_zero_mask, 0);
  expr = c_parser_expr_no_commas (parser, NULL);
  if (convert_p)
    expr = convert_lvalue_to_rvalue (expr.get_location (), expr, true, true);
  if (fold_p)
    expr.value = c_fully_fold (expr.value, false, NULL);
  ret->quick_push (expr.value);
  if (orig_types)
    orig_types->quick_push (expr.original_type);
  if (locations)
    locations->safe_push (expr.get_location ());
  if (sizeof_arg != NULL
      && (expr.original_code == SIZEOF_EXPR
	  || expr.original_code == PAREN_SIZEOF_EXPR))
    {
      sizeof_arg[0] = c_last_sizeof_arg;
      sizeof_arg_loc[0] = c_last_sizeof_loc;
    }
  while (c_parser_next_token_is (parser, CPP_COMMA))
    {
      c_parser_consume_token (parser);
      if (literal_zero_mask)
	c_parser_check_literal_zero (parser, literal_zero_mask, idx + 1);
      expr = c_parser_expr_no_commas (parser, NULL);
      if (convert_p)
	expr = convert_lvalue_to_rvalue (expr.get_location (), expr, true,
					 true);
      if (fold_p)
	expr.value = c_fully_fold (expr.value, false, NULL);
      vec_safe_push (ret, expr.value);
      if (orig_types)
	vec_safe_push (orig_types, expr.original_type);
      if (locations)
	locations->safe_push (expr.get_location ());
      if (++idx < 3
	  && sizeof_arg != NULL
	  && (expr.original_code == SIZEOF_EXPR
	      || expr.original_code == PAREN_SIZEOF_EXPR))
	{
	  sizeof_arg[idx] = c_last_sizeof_arg;
	  sizeof_arg_loc[idx] = c_last_sizeof_loc;
	}
    }
  if (orig_types)
    *p_orig_types = orig_types;
  return ret;
}

/* Parse Objective-C-specific constructs.  */

/* Parse an objc-class-definition.

   objc-class-definition:
     @interface identifier objc-superclass[opt] objc-protocol-refs[opt]
       objc-class-instance-variables[opt] objc-methodprotolist @end
     @implementation identifier objc-superclass[opt]
       objc-class-instance-variables[opt]
     @interface identifier ( identifier ) objc-protocol-refs[opt]
       objc-methodprotolist @end
     @interface identifier ( ) objc-protocol-refs[opt]
       objc-methodprotolist @end
     @implementation identifier ( identifier )

   objc-superclass:
     : identifier

   "@interface identifier (" must start "@interface identifier (
   identifier ) ...": objc-methodprotolist in the first production may
   not start with a parenthesized identifier as a declarator of a data
   definition with no declaration specifiers if the objc-superclass,
   objc-protocol-refs and objc-class-instance-variables are omitted.  */

static void
c_parser_objc_class_definition (c_parser *parser, tree attributes)
{
  bool iface_p;
  tree id1;
  tree superclass;
  if (c_parser_next_token_is_keyword (parser, RID_AT_INTERFACE))
    iface_p = true;
  else if (c_parser_next_token_is_keyword (parser, RID_AT_IMPLEMENTATION))
    iface_p = false;
  else
    gcc_unreachable ();

  c_parser_consume_token (parser);
  if (c_parser_next_token_is_not (parser, CPP_NAME))
    {
      c_parser_error (parser, "expected identifier");
      return;
    }
  id1 = c_parser_peek_token (parser)->value;
  location_t loc1 = c_parser_peek_token (parser)->location;
  c_parser_consume_token (parser);
  if (c_parser_next_token_is (parser, CPP_OPEN_PAREN))
    {
      /* We have a category or class extension.  */
      tree id2;
      tree proto = NULL_TREE;
      matching_parens parens;
      parens.consume_open (parser);
      if (c_parser_next_token_is_not (parser, CPP_NAME))
	{
	  if (iface_p && c_parser_next_token_is (parser, CPP_CLOSE_PAREN))
	    {
	      /* We have a class extension.  */
	      id2 = NULL_TREE;
	    }
	  else
	    {
	      c_parser_error (parser, "expected identifier or %<)%>");
	      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, NULL);
	      return;
	    }
	}
      else
	{
	  id2 = c_parser_peek_token (parser)->value;
	  c_parser_consume_token (parser);
	}
      parens.skip_until_found_close (parser);
      if (!iface_p)
	{
	  objc_start_category_implementation (id1, id2);
	  return;
	}
      if (c_parser_next_token_is (parser, CPP_LESS))
	proto = c_parser_objc_protocol_refs (parser);
      objc_start_category_interface (id1, id2, proto, attributes);
      c_parser_objc_methodprotolist (parser);
      c_parser_require_keyword (parser, RID_AT_END, "expected %<@end%>");
      objc_finish_interface ();
      return;
    }
  if (c_parser_next_token_is (parser, CPP_COLON))
    {
      c_parser_consume_token (parser);
      if (c_parser_next_token_is_not (parser, CPP_NAME))
	{
	  c_parser_error (parser, "expected identifier");
	  return;
	}
      superclass = c_parser_peek_token (parser)->value;
      c_parser_consume_token (parser);
    }
  else
    superclass = NULL_TREE;
  if (iface_p)
    {
      tree proto = NULL_TREE;
      if (c_parser_next_token_is (parser, CPP_LESS))
	proto = c_parser_objc_protocol_refs (parser);
      objc_start_class_interface (id1, loc1, superclass, proto, attributes);
    }
  else
    objc_start_class_implementation (id1, superclass);
  if (c_parser_next_token_is (parser, CPP_OPEN_BRACE))
    c_parser_objc_class_instance_variables (parser);
  if (iface_p)
    {
      objc_continue_interface ();
      c_parser_objc_methodprotolist (parser);
      c_parser_require_keyword (parser, RID_AT_END, "expected %<@end%>");
      objc_finish_interface ();
    }
  else
    {
      objc_continue_implementation ();
      return;
    }
}

/* Parse objc-class-instance-variables.

   objc-class-instance-variables:
     { objc-instance-variable-decl-list[opt] }

   objc-instance-variable-decl-list:
     objc-visibility-spec
     objc-instance-variable-decl ;
     ;
     objc-instance-variable-decl-list objc-visibility-spec
     objc-instance-variable-decl-list objc-instance-variable-decl ;
     objc-instance-variable-decl-list ;

   objc-visibility-spec:
     @private
     @protected
     @public

   objc-instance-variable-decl:
     struct-declaration
*/

static void
c_parser_objc_class_instance_variables (c_parser *parser)
{
  gcc_assert (c_parser_next_token_is (parser, CPP_OPEN_BRACE));
  c_parser_consume_token (parser);
  while (c_parser_next_token_is_not (parser, CPP_EOF))
    {
      tree decls;
      /* Parse any stray semicolon.  */
      if (c_parser_next_token_is (parser, CPP_SEMICOLON))
	{
	  pedwarn (c_parser_peek_token (parser)->location, OPT_Wpedantic,
		   "extra semicolon");
	  c_parser_consume_token (parser);
	  continue;
	}
      /* Stop if at the end of the instance variables.  */
      if (c_parser_next_token_is (parser, CPP_CLOSE_BRACE))
	{
	  c_parser_consume_token (parser);
	  break;
	}
      /* Parse any objc-visibility-spec.  */
      if (c_parser_next_token_is_keyword (parser, RID_AT_PRIVATE))
	{
	  c_parser_consume_token (parser);
	  objc_set_visibility (OBJC_IVAR_VIS_PRIVATE);
	  continue;
	}
      else if (c_parser_next_token_is_keyword (parser, RID_AT_PROTECTED))
	{
	  c_parser_consume_token (parser);
	  objc_set_visibility (OBJC_IVAR_VIS_PROTECTED);
	  continue;
	}
      else if (c_parser_next_token_is_keyword (parser, RID_AT_PUBLIC))
	{
	  c_parser_consume_token (parser);
	  objc_set_visibility (OBJC_IVAR_VIS_PUBLIC);
	  continue;
	}
      else if (c_parser_next_token_is_keyword (parser, RID_AT_PACKAGE))
	{
	  c_parser_consume_token (parser);
	  objc_set_visibility (OBJC_IVAR_VIS_PACKAGE);
	  continue;
	}
      else if (c_parser_next_token_is (parser, CPP_PRAGMA))
	{
	  c_parser_pragma (parser, pragma_external, NULL);
	  continue;
	}

      /* Parse some comma-separated declarations.  */
      decls = c_parser_struct_declaration (parser);
      if (decls == NULL)
	{
	  /* There is a syntax error.  We want to skip the offending
	     tokens up to the next ';' (included) or '}'
	     (excluded).  */
	  
	  /* First, skip manually a ')' or ']'.  This is because they
	     reduce the nesting level, so c_parser_skip_until_found()
	     wouldn't be able to skip past them.  */
	  c_token *token = c_parser_peek_token (parser);
	  if (token->type == CPP_CLOSE_PAREN || token->type == CPP_CLOSE_SQUARE)
	    c_parser_consume_token (parser);

	  /* Then, do the standard skipping.  */
	  c_parser_skip_until_found (parser, CPP_SEMICOLON, NULL);

	  /* We hopefully recovered.  Start normal parsing again.  */
	  parser->error = false;
	  continue;
	}
      else
	{
	  /* Comma-separated instance variables are chained together
	     in reverse order; add them one by one.  */
	  tree ivar = nreverse (decls);
	  for (; ivar; ivar = DECL_CHAIN (ivar))
	    objc_add_instance_variable (copy_node (ivar));
	}
      c_parser_skip_until_found (parser, CPP_SEMICOLON, "expected %<;%>");
    }
}

/* Parse an objc-class-declaration.

   objc-class-declaration:
     @class identifier-list ;
*/

static void
c_parser_objc_class_declaration (c_parser *parser)
{
  gcc_assert (c_parser_next_token_is_keyword (parser, RID_AT_CLASS));
  c_parser_consume_token (parser);
  /* Any identifiers, including those declared as type names, are OK
     here.  */
  while (true)
    {
      tree id;
      if (c_parser_next_token_is_not (parser, CPP_NAME))
	{
	  c_parser_error (parser, "expected identifier");
	  c_parser_skip_until_found (parser, CPP_SEMICOLON, NULL);
	  parser->error = false;
	  return;
	}
      id = c_parser_peek_token (parser)->value;
      objc_declare_class (id);
      c_parser_consume_token (parser);
      if (c_parser_next_token_is (parser, CPP_COMMA))
	c_parser_consume_token (parser);
      else
	break;
    }
  c_parser_skip_until_found (parser, CPP_SEMICOLON, "expected %<;%>");
}

/* Parse an objc-alias-declaration.

   objc-alias-declaration:
     @compatibility_alias identifier identifier ;
*/

static void
c_parser_objc_alias_declaration (c_parser *parser)
{
  tree id1, id2;
  gcc_assert (c_parser_next_token_is_keyword (parser, RID_AT_ALIAS));
  c_parser_consume_token (parser);
  if (c_parser_next_token_is_not (parser, CPP_NAME))
    {
      c_parser_error (parser, "expected identifier");
      c_parser_skip_until_found (parser, CPP_SEMICOLON, NULL);
      return;
    }
  id1 = c_parser_peek_token (parser)->value;
  c_parser_consume_token (parser);
  if (c_parser_next_token_is_not (parser, CPP_NAME))
    {
      c_parser_error (parser, "expected identifier");
      c_parser_skip_until_found (parser, CPP_SEMICOLON, NULL);
      return;
    }
  id2 = c_parser_peek_token (parser)->value;
  c_parser_consume_token (parser);
  c_parser_skip_until_found (parser, CPP_SEMICOLON, "expected %<;%>");
  objc_declare_alias (id1, id2);
}

/* Parse an objc-protocol-definition.

   objc-protocol-definition:
     @protocol identifier objc-protocol-refs[opt] objc-methodprotolist @end
     @protocol identifier-list ;

   "@protocol identifier ;" should be resolved as "@protocol
   identifier-list ;": objc-methodprotolist may not start with a
   semicolon in the first alternative if objc-protocol-refs are
   omitted.  */

static void
c_parser_objc_protocol_definition (c_parser *parser, tree attributes)
{
  gcc_assert (c_parser_next_token_is_keyword (parser, RID_AT_PROTOCOL));

  c_parser_consume_token (parser);
  if (c_parser_next_token_is_not (parser, CPP_NAME))
    {
      c_parser_error (parser, "expected identifier");
      return;
    }
  if (c_parser_peek_2nd_token (parser)->type == CPP_COMMA
      || c_parser_peek_2nd_token (parser)->type == CPP_SEMICOLON)
    {
      /* Any identifiers, including those declared as type names, are
	 OK here.  */
      while (true)
	{
	  tree id;
	  if (c_parser_next_token_is_not (parser, CPP_NAME))
	    {
	      c_parser_error (parser, "expected identifier");
	      break;
	    }
	  id = c_parser_peek_token (parser)->value;
	  objc_declare_protocol (id, attributes);
	  c_parser_consume_token (parser);
	  if (c_parser_next_token_is (parser, CPP_COMMA))
	    c_parser_consume_token (parser);
	  else
	    break;
	}
      c_parser_skip_until_found (parser, CPP_SEMICOLON, "expected %<;%>");
    }
  else
    {
      tree id = c_parser_peek_token (parser)->value;
      tree proto = NULL_TREE;
      c_parser_consume_token (parser);
      if (c_parser_next_token_is (parser, CPP_LESS))
	proto = c_parser_objc_protocol_refs (parser);
      parser->objc_pq_context = true;
      objc_start_protocol (id, proto, attributes);
      c_parser_objc_methodprotolist (parser);
      c_parser_require_keyword (parser, RID_AT_END, "expected %<@end%>");
      parser->objc_pq_context = false;
      objc_finish_interface ();
    }
}

/* Parse an objc-method-type.

   objc-method-type:
     +
     -

   Return true if it is a class method (+) and false if it is
   an instance method (-).
*/
static inline bool
c_parser_objc_method_type (c_parser *parser)
{
  switch (c_parser_peek_token (parser)->type)
    {
    case CPP_PLUS:
      c_parser_consume_token (parser);
      return true;
    case CPP_MINUS:
      c_parser_consume_token (parser);
      return false;
    default:
      gcc_unreachable ();
    }
}

/* Parse an objc-method-definition.

   objc-method-definition:
     objc-method-type objc-method-decl ;[opt] compound-statement
*/

static void
c_parser_objc_method_definition (c_parser *parser)
{
  bool is_class_method = c_parser_objc_method_type (parser);
  tree decl, attributes = NULL_TREE, expr = NULL_TREE;
  parser->objc_pq_context = true;
  decl = c_parser_objc_method_decl (parser, is_class_method, &attributes,
				    &expr);
  if (decl == error_mark_node)
    return;  /* Bail here. */

  if (c_parser_next_token_is (parser, CPP_SEMICOLON))
    {
      c_parser_consume_token (parser);
      pedwarn (c_parser_peek_token (parser)->location, OPT_Wpedantic,
	       "extra semicolon in method definition specified");
    }

  if (!c_parser_next_token_is (parser, CPP_OPEN_BRACE))
    {
      c_parser_error (parser, "expected %<{%>");
      return;
    }

  parser->objc_pq_context = false;
  if (objc_start_method_definition (is_class_method, decl, attributes, expr))
    {
      add_stmt (c_parser_compound_statement (parser));
      objc_finish_method_definition (current_function_decl);
    }
  else
    {
      /* This code is executed when we find a method definition
	 outside of an @implementation context (or invalid for other
	 reasons).  Parse the method (to keep going) but do not emit
	 any code.
      */
      c_parser_compound_statement (parser);
    }
}

/* Parse an objc-methodprotolist.

   objc-methodprotolist:
     empty
     objc-methodprotolist objc-methodproto
     objc-methodprotolist declaration
     objc-methodprotolist ;
     @optional
     @required

   The declaration is a data definition, which may be missing
   declaration specifiers under the same rules and diagnostics as
   other data definitions outside functions, and the stray semicolon
   is diagnosed the same way as a stray semicolon outside a
   function.  */

static void
c_parser_objc_methodprotolist (c_parser *parser)
{
  while (true)
    {
      /* The list is terminated by @end.  */
      switch (c_parser_peek_token (parser)->type)
	{
	case CPP_SEMICOLON:
	  pedwarn (c_parser_peek_token (parser)->location, OPT_Wpedantic,
		   "ISO C does not allow extra %<;%> outside of a function");
	  c_parser_consume_token (parser);
	  break;
	case CPP_PLUS:
	case CPP_MINUS:
	  c_parser_objc_methodproto (parser);
	  break;
	case CPP_PRAGMA:
	  c_parser_pragma (parser, pragma_external, NULL);
	  break;
	case CPP_EOF:
	  return;
	default:
	  if (c_parser_next_token_is_keyword (parser, RID_AT_END))
	    return;
	  else if (c_parser_next_token_is_keyword (parser, RID_AT_PROPERTY))
	    c_parser_objc_at_property_declaration (parser);
	  else if (c_parser_next_token_is_keyword (parser, RID_AT_OPTIONAL))
	    {
	      objc_set_method_opt (true);
	      c_parser_consume_token (parser);
	    }
	  else if (c_parser_next_token_is_keyword (parser, RID_AT_REQUIRED))
	    {
	      objc_set_method_opt (false);
	      c_parser_consume_token (parser);
	    }
	  else
	    c_parser_declaration_or_fndef (parser, false, false, true,
					   false, true, NULL, vNULL);
	  break;
	}
    }
}

/* Parse an objc-methodproto.

   objc-methodproto:
     objc-method-type objc-method-decl ;
*/

static void
c_parser_objc_methodproto (c_parser *parser)
{
  bool is_class_method = c_parser_objc_method_type (parser);
  tree decl, attributes = NULL_TREE;

  /* Remember protocol qualifiers in prototypes.  */
  parser->objc_pq_context = true;
  decl = c_parser_objc_method_decl (parser, is_class_method, &attributes,
				    NULL);
  /* Forget protocol qualifiers now.  */
  parser->objc_pq_context = false;

  /* Do not allow the presence of attributes to hide an erroneous 
     method implementation in the interface section.  */
  if (!c_parser_next_token_is (parser, CPP_SEMICOLON))
    {
      c_parser_error (parser, "expected %<;%>");
      return;
    }
  
  if (decl != error_mark_node)
    objc_add_method_declaration (is_class_method, decl, attributes);

  c_parser_skip_until_found (parser, CPP_SEMICOLON, "expected %<;%>");
}

/* If we are at a position that method attributes may be present, check that 
   there are not any parsed already (a syntax error) and then collect any 
   specified at the current location.  Finally, if new attributes were present,
   check that the next token is legal ( ';' for decls and '{' for defs).  */
   
static bool 
c_parser_objc_maybe_method_attributes (c_parser* parser, tree* attributes)
{
  bool bad = false;
  if (*attributes)
    {
      c_parser_error (parser, 
		    "method attributes must be specified at the end only");
      *attributes = NULL_TREE;
      bad = true;
    }

  if (c_parser_next_token_is_keyword (parser, RID_ATTRIBUTE))
    *attributes = c_parser_gnu_attributes (parser);

  /* If there were no attributes here, just report any earlier error.  */
  if (*attributes == NULL_TREE || bad)
    return bad;

  /* If the attributes are followed by a ; or {, then just report any earlier
     error.  */
  if (c_parser_next_token_is (parser, CPP_SEMICOLON)
      || c_parser_next_token_is (parser, CPP_OPEN_BRACE))
    return bad;

  /* We've got attributes, but not at the end.  */
  c_parser_error (parser, 
		  "expected %<;%> or %<{%> after method attribute definition");
  return true;
}

/* Parse an objc-method-decl.

   objc-method-decl:
     ( objc-type-name ) objc-selector
     objc-selector
     ( objc-type-name ) objc-keyword-selector objc-optparmlist
     objc-keyword-selector objc-optparmlist
     gnu-attributes

   objc-keyword-selector:
     objc-keyword-decl
     objc-keyword-selector objc-keyword-decl

   objc-keyword-decl:
     objc-selector : ( objc-type-name ) identifier
     objc-selector : identifier
     : ( objc-type-name ) identifier
     : identifier

   objc-optparmlist:
     objc-optparms objc-optellipsis

   objc-optparms:
     empty
     objc-opt-parms , parameter-declaration

   objc-optellipsis:
     empty
     , ...
*/

static tree
c_parser_objc_method_decl (c_parser *parser, bool is_class_method,
			   tree *attributes, tree *expr)
{
  tree type = NULL_TREE;
  tree sel;
  tree parms = NULL_TREE;
  bool ellipsis = false;
  bool attr_err = false;

  *attributes = NULL_TREE;
  if (c_parser_next_token_is (parser, CPP_OPEN_PAREN))
    {
      matching_parens parens;
      parens.consume_open (parser);
      type = c_parser_objc_type_name (parser);
      parens.skip_until_found_close (parser);
    }
  sel = c_parser_objc_selector (parser);
  /* If there is no selector, or a colon follows, we have an
     objc-keyword-selector.  If there is a selector, and a colon does
     not follow, that selector ends the objc-method-decl.  */
  if (!sel || c_parser_next_token_is (parser, CPP_COLON))
    {
      tree tsel = sel;
      tree list = NULL_TREE;
      while (true)
	{
	  tree atype = NULL_TREE, id, keyworddecl;
	  tree param_attr = NULL_TREE;
	  if (!c_parser_require (parser, CPP_COLON, "expected %<:%>"))
	    break;
	  if (c_parser_next_token_is (parser, CPP_OPEN_PAREN))
	    {
	      c_parser_consume_token (parser);
	      atype = c_parser_objc_type_name (parser);
	      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
					 "expected %<)%>");
	    }
	  /* New ObjC allows attributes on method parameters.  */
	  if (c_parser_next_token_is_keyword (parser, RID_ATTRIBUTE))
	    param_attr = c_parser_gnu_attributes (parser);
	  if (c_parser_next_token_is_not (parser, CPP_NAME))
	    {
	      c_parser_error (parser, "expected identifier");
	      return error_mark_node;
	    }
	  id = c_parser_peek_token (parser)->value;
	  c_parser_consume_token (parser);
	  keyworddecl = objc_build_keyword_decl (tsel, atype, id, param_attr);
	  list = chainon (list, keyworddecl);
	  tsel = c_parser_objc_selector (parser);
	  if (!tsel && c_parser_next_token_is_not (parser, CPP_COLON))
	    break;
	}

      attr_err |= c_parser_objc_maybe_method_attributes (parser, attributes) ;

      /* Parse the optional parameter list.  Optional Objective-C
	 method parameters follow the C syntax, and may include '...'
	 to denote a variable number of arguments.  */
      parms = make_node (TREE_LIST);
      while (c_parser_next_token_is (parser, CPP_COMMA))
	{
	  struct c_parm *parm;
	  c_parser_consume_token (parser);
	  if (c_parser_next_token_is (parser, CPP_ELLIPSIS))
	    {
	      ellipsis = true;
	      c_parser_consume_token (parser);
	      attr_err |= c_parser_objc_maybe_method_attributes 
						(parser, attributes) ;
	      break;
	    }
	  parm = c_parser_parameter_declaration (parser, NULL_TREE, false);
	  if (parm == NULL)
	    break;
	  parms = chainon (parms,
			   build_tree_list (NULL_TREE, grokparm (parm, expr)));
	}
      sel = list;
    }
  else
    attr_err |= c_parser_objc_maybe_method_attributes (parser, attributes) ;

  if (sel == NULL)
    {
      c_parser_error (parser, "objective-c method declaration is expected");
      return error_mark_node;
    }

  if (attr_err)
    return error_mark_node;

  return objc_build_method_signature (is_class_method, type, sel, parms, ellipsis);
}

/* Parse an objc-type-name.

   objc-type-name:
     objc-type-qualifiers[opt] type-name
     objc-type-qualifiers[opt]

   objc-type-qualifiers:
     objc-type-qualifier
     objc-type-qualifiers objc-type-qualifier

   objc-type-qualifier: one of
     in out inout bycopy byref oneway
*/

static tree
c_parser_objc_type_name (c_parser *parser)
{
  tree quals = NULL_TREE;
  struct c_type_name *type_name = NULL;
  tree type = NULL_TREE;
  while (true)
    {
      c_token *token = c_parser_peek_token (parser);
      if (token->type == CPP_KEYWORD
	  && (token->keyword == RID_IN
	      || token->keyword == RID_OUT
	      || token->keyword == RID_INOUT
	      || token->keyword == RID_BYCOPY
	      || token->keyword == RID_BYREF
	      || token->keyword == RID_ONEWAY))
	{
	  quals = chainon (build_tree_list (NULL_TREE, token->value), quals);
	  c_parser_consume_token (parser);
	}
      else
	break;
    }
  if (c_parser_next_tokens_start_typename (parser, cla_prefer_type))
    type_name = c_parser_type_name (parser);
  if (type_name)
    type = groktypename (type_name, NULL, NULL);

  /* If the type is unknown, and error has already been produced and
     we need to recover from the error.  In that case, use NULL_TREE
     for the type, as if no type had been specified; this will use the
     default type ('id') which is good for error recovery.  */
  if (type == error_mark_node)
    type = NULL_TREE;

  return build_tree_list (quals, type);
}

/* Parse objc-protocol-refs.

   objc-protocol-refs:
     < identifier-list >
*/

static tree
c_parser_objc_protocol_refs (c_parser *parser)
{
  tree list = NULL_TREE;
  gcc_assert (c_parser_next_token_is (parser, CPP_LESS));
  c_parser_consume_token (parser);
  /* Any identifiers, including those declared as type names, are OK
     here.  */
  while (true)
    {
      tree id;
      if (c_parser_next_token_is_not (parser, CPP_NAME))
	{
	  c_parser_error (parser, "expected identifier");
	  break;
	}
      id = c_parser_peek_token (parser)->value;
      list = chainon (list, build_tree_list (NULL_TREE, id));
      c_parser_consume_token (parser);
      if (c_parser_next_token_is (parser, CPP_COMMA))
	c_parser_consume_token (parser);
      else
	break;
    }
  c_parser_require (parser, CPP_GREATER, "expected %<>%>");
  return list;
}

/* Parse an objc-try-catch-finally-statement.

   objc-try-catch-finally-statement:
     @try compound-statement objc-catch-list[opt]
     @try compound-statement objc-catch-list[opt] @finally compound-statement

   objc-catch-list:
     @catch ( objc-catch-parameter-declaration ) compound-statement
     objc-catch-list @catch ( objc-catch-parameter-declaration ) compound-statement

   objc-catch-parameter-declaration:
     parameter-declaration
     '...'

   where '...' is to be interpreted literally, that is, it means CPP_ELLIPSIS.

   PS: This function is identical to cp_parser_objc_try_catch_finally_statement
   for C++.  Keep them in sync.  */   

static void
c_parser_objc_try_catch_finally_statement (c_parser *parser)
{
  location_t location;
  tree stmt;

  gcc_assert (c_parser_next_token_is_keyword (parser, RID_AT_TRY));
  c_parser_consume_token (parser);
  location = c_parser_peek_token (parser)->location;
  objc_maybe_warn_exceptions (location);
  stmt = c_parser_compound_statement (parser);
  objc_begin_try_stmt (location, stmt);

  while (c_parser_next_token_is_keyword (parser, RID_AT_CATCH))
    {
      struct c_parm *parm;
      tree parameter_declaration = error_mark_node;
      bool seen_open_paren = false;

      c_parser_consume_token (parser);
      matching_parens parens;
      if (!parens.require_open (parser))
	seen_open_paren = true;
      if (c_parser_next_token_is (parser, CPP_ELLIPSIS))
	{
	  /* We have "@catch (...)" (where the '...' are literally
	     what is in the code).  Skip the '...'.
	     parameter_declaration is set to NULL_TREE, and
	     objc_being_catch_clauses() knows that that means
	     '...'.  */
	  c_parser_consume_token (parser);
	  parameter_declaration = NULL_TREE;
	}
      else
	{
	  /* We have "@catch (NSException *exception)" or something
	     like that.  Parse the parameter declaration.  */
	  parm = c_parser_parameter_declaration (parser, NULL_TREE, false);
	  if (parm == NULL)
	    parameter_declaration = error_mark_node;
	  else
	    parameter_declaration = grokparm (parm, NULL);
	}
      if (seen_open_paren)
	parens.require_close (parser);
      else
	{
	  /* If there was no open parenthesis, we are recovering from
	     an error, and we are trying to figure out what mistake
	     the user has made.  */

	  /* If there is an immediate closing parenthesis, the user
	     probably forgot the opening one (ie, they typed "@catch
	     NSException *e)".  Parse the closing parenthesis and keep
	     going.  */
	  if (c_parser_next_token_is (parser, CPP_CLOSE_PAREN))
	    c_parser_consume_token (parser);
	  
	  /* If these is no immediate closing parenthesis, the user
	     probably doesn't know that parenthesis are required at
	     all (ie, they typed "@catch NSException *e").  So, just
	     forget about the closing parenthesis and keep going.  */
	}
      objc_begin_catch_clause (parameter_declaration);
      if (c_parser_require (parser, CPP_OPEN_BRACE, "expected %<{%>"))
	c_parser_compound_statement_nostart (parser);
      objc_finish_catch_clause ();
    }
  if (c_parser_next_token_is_keyword (parser, RID_AT_FINALLY))
    {
      c_parser_consume_token (parser);
      location = c_parser_peek_token (parser)->location;
      stmt = c_parser_compound_statement (parser);
      objc_build_finally_clause (location, stmt);
    }
  objc_finish_try_stmt ();
}

/* Parse an objc-synchronized-statement.

   objc-synchronized-statement:
     @synchronized ( expression ) compound-statement
*/

static void
c_parser_objc_synchronized_statement (c_parser *parser)
{
  location_t loc;
  tree expr, stmt;
  gcc_assert (c_parser_next_token_is_keyword (parser, RID_AT_SYNCHRONIZED));
  c_parser_consume_token (parser);
  loc = c_parser_peek_token (parser)->location;
  objc_maybe_warn_exceptions (loc);
  matching_parens parens;
  if (parens.require_open (parser))
    {
      struct c_expr ce = c_parser_expression (parser);
      ce = convert_lvalue_to_rvalue (loc, ce, false, false);
      expr = ce.value;
      expr = c_fully_fold (expr, false, NULL);
      parens.skip_until_found_close (parser);
    }
  else
    expr = error_mark_node;
  stmt = c_parser_compound_statement (parser);
  objc_build_synchronized (loc, expr, stmt);
}

/* Parse an objc-selector; return NULL_TREE without an error if the
   next token is not an objc-selector.

   objc-selector:
     identifier
     one of
       enum struct union if else while do for switch case default
       break continue return goto asm sizeof typeof __alignof
       unsigned long const short volatile signed restrict _Complex
       in out inout bycopy byref oneway int char float double void _Bool
       _Atomic

   ??? Why this selection of keywords but not, for example, storage
   class specifiers?  */

static tree
c_parser_objc_selector (c_parser *parser)
{
  c_token *token = c_parser_peek_token (parser);
  tree value = token->value;
  if (token->type == CPP_NAME)
    {
      c_parser_consume_token (parser);
      return value;
    }
  if (token->type != CPP_KEYWORD)
    return NULL_TREE;
  switch (token->keyword)
    {
    case RID_ENUM:
    case RID_STRUCT:
    case RID_UNION:
    case RID_IF:
    case RID_ELSE:
    case RID_WHILE:
    case RID_DO:
    case RID_FOR:
    case RID_SWITCH:
    case RID_CASE:
    case RID_DEFAULT:
    case RID_BREAK:
    case RID_CONTINUE:
    case RID_RETURN:
    case RID_GOTO:
    case RID_ASM:
    case RID_SIZEOF:
    case RID_TYPEOF:
    case RID_ALIGNOF:
    case RID_UNSIGNED:
    case RID_LONG:
    case RID_CONST:
    case RID_SHORT:
    case RID_VOLATILE:
    case RID_SIGNED:
    case RID_RESTRICT:
    case RID_COMPLEX:
    case RID_IN:
    case RID_OUT:
    case RID_INOUT:
    case RID_BYCOPY:
    case RID_BYREF:
    case RID_ONEWAY:
    case RID_INT:
    case RID_CHAR:
    case RID_FLOAT:
    case RID_DOUBLE:
    CASE_RID_FLOATN_NX:
    case RID_VOID:
    case RID_BOOL:
    case RID_ATOMIC:
    case RID_AUTO_TYPE:
    case RID_INT_N_0:
    case RID_INT_N_1:
    case RID_INT_N_2:
    case RID_INT_N_3:
      c_parser_consume_token (parser);
      return value;
    default:
      return NULL_TREE;
    }
}

/* Parse an objc-selector-arg.

   objc-selector-arg:
     objc-selector
     objc-keywordname-list

   objc-keywordname-list:
     objc-keywordname
     objc-keywordname-list objc-keywordname

   objc-keywordname:
     objc-selector :
     :
*/

static tree
c_parser_objc_selector_arg (c_parser *parser)
{
  tree sel = c_parser_objc_selector (parser);
  tree list = NULL_TREE;
  if (sel
      && c_parser_next_token_is_not (parser, CPP_COLON)
      && c_parser_next_token_is_not (parser, CPP_SCOPE))
    return sel;
  while (true)
    {
      if (c_parser_next_token_is (parser, CPP_SCOPE))
	{
	  c_parser_consume_token (parser);
	  list = chainon (list, build_tree_list (sel, NULL_TREE));
	  list = chainon (list, build_tree_list (NULL_TREE, NULL_TREE));
	}
      else
	{
	  if (!c_parser_require (parser, CPP_COLON, "expected %<:%>"))
	    return list;
	  list = chainon (list, build_tree_list (sel, NULL_TREE));
	}
      sel = c_parser_objc_selector (parser);
      if (!sel
	  && c_parser_next_token_is_not (parser, CPP_COLON)
	  && c_parser_next_token_is_not (parser, CPP_SCOPE))
	break;
    }
  return list;
}

/* Parse an objc-receiver.

   objc-receiver:
     expression
     class-name
     type-name
*/

static tree
c_parser_objc_receiver (c_parser *parser)
{
  location_t loc = c_parser_peek_token (parser)->location;

  if (c_parser_peek_token (parser)->type == CPP_NAME
      && (c_parser_peek_token (parser)->id_kind == C_ID_TYPENAME
	  || c_parser_peek_token (parser)->id_kind == C_ID_CLASSNAME))
    {
      tree id = c_parser_peek_token (parser)->value;
      c_parser_consume_token (parser);
      return objc_get_class_reference (id);
    }
  struct c_expr ce = c_parser_expression (parser);
  ce = convert_lvalue_to_rvalue (loc, ce, false, false);
  return c_fully_fold (ce.value, false, NULL);
}

/* Parse objc-message-args.

   objc-message-args:
     objc-selector
     objc-keywordarg-list

   objc-keywordarg-list:
     objc-keywordarg
     objc-keywordarg-list objc-keywordarg

   objc-keywordarg:
     objc-selector : objc-keywordexpr
     : objc-keywordexpr
*/

static tree
c_parser_objc_message_args (c_parser *parser)
{
  tree sel = c_parser_objc_selector (parser);
  tree list = NULL_TREE;
  if (sel && c_parser_next_token_is_not (parser, CPP_COLON))
    return sel;
  while (true)
    {
      tree keywordexpr;
      if (!c_parser_require (parser, CPP_COLON, "expected %<:%>"))
	return error_mark_node;
      keywordexpr = c_parser_objc_keywordexpr (parser);
      list = chainon (list, build_tree_list (sel, keywordexpr));
      sel = c_parser_objc_selector (parser);
      if (!sel && c_parser_next_token_is_not (parser, CPP_COLON))
	break;
    }
  return list;
}

/* Parse an objc-keywordexpr.

   objc-keywordexpr:
     nonempty-expr-list
*/

static tree
c_parser_objc_keywordexpr (c_parser *parser)
{
  tree ret;
  vec<tree, va_gc> *expr_list = c_parser_expr_list (parser, true, true,
						NULL, NULL, NULL, NULL);
  if (vec_safe_length (expr_list) == 1)
    {
      /* Just return the expression, remove a level of
	 indirection.  */
      ret = (*expr_list)[0];
    }
  else
    {
      /* We have a comma expression, we will collapse later.  */
      ret = build_tree_list_vec (expr_list);
    }
  release_tree_vector (expr_list);
  return ret;
}

/* A check, needed in several places, that ObjC interface, implementation or
   method definitions are not prefixed by incorrect items.  */
static bool
c_parser_objc_diagnose_bad_element_prefix (c_parser *parser, 
					   struct c_declspecs *specs)
{
  if (!specs->declspecs_seen_p || specs->non_sc_seen_p
      || specs->typespec_kind != ctsk_none)
    {
      c_parser_error (parser, 
      		      "no type or storage class may be specified here,");
      c_parser_skip_to_end_of_block_or_statement (parser);
      return true;
    }
  return false;
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

  PS: This function is identical to cp_parser_objc_at_propery_declaration
  for C++.  Keep them in sync.  */
static void
c_parser_objc_at_property_declaration (c_parser *parser)
{
  gcc_assert (c_parser_next_token_is_keyword (parser, RID_AT_PROPERTY));
  location_t loc = c_parser_peek_token (parser)->location;
  c_parser_consume_token (parser);  /* Eat '@property'.  */

  /* Parse the optional attribute list.

     A list of parsed, but not verified, attributes.  */
  vec<property_attribute_info *> prop_attr_list = vNULL;

  bool syntax_error = false;
  if (c_parser_next_token_is (parser, CPP_OPEN_PAREN))
    {
      matching_parens parens;

      location_t attr_start = c_parser_peek_token (parser)->location;
      /* Eat the '(' */
      parens.consume_open (parser);

      /* Property attribute keywords are valid now.  */
      parser->objc_property_attr_context = true;

      /* Allow @property (), with a warning.  */
      location_t attr_end = c_parser_peek_token (parser)->location;

      if (c_parser_next_token_is (parser, CPP_CLOSE_PAREN))
	{
	  location_t attr_comb = make_location (attr_end, attr_start, attr_end);
	  warning_at (attr_comb, OPT_Wattributes,
		      "empty property attribute list");
	}
      else
	while (true)
	  {
	    c_token *token = c_parser_peek_token (parser);
	    attr_start = token->location;
	    attr_end = get_finish (token->location);
	    location_t attr_comb = make_location (attr_start, attr_start,
						  attr_end);

	    if (token->type == CPP_CLOSE_PAREN || token->type == CPP_COMMA)
	      {
		warning_at (attr_comb, OPT_Wattributes,
			    "missing property attribute");
		if (token->type == CPP_CLOSE_PAREN)
		  break;
		c_parser_consume_token (parser);
		continue;
	      }

	    tree attr_name = NULL_TREE;
	    enum rid keyword = RID_MAX; /* Not a valid property attribute.  */
	    bool add_at = false;
	    if (token->type == CPP_KEYWORD)
	      {
		keyword = token->keyword;
		if (OBJC_IS_AT_KEYWORD (keyword))
		  {
		    /* For '@' keywords the token value has the keyword,
		       prepend the '@' for diagnostics.  */
		    attr_name = token->value;
		    add_at = true;
		  }
		else
		  attr_name = ridpointers[(int)keyword];
	      }
	    else if (token->type == CPP_NAME)
	      attr_name = token->value;
	    c_parser_consume_token (parser);

	    enum objc_property_attribute_kind prop_kind
	      = objc_prop_attr_kind_for_rid (keyword);
	    property_attribute_info *prop
	      = new property_attribute_info (attr_name, attr_comb, prop_kind);
	    prop_attr_list.safe_push (prop);

	    tree meth_name;
	    switch (prop->prop_kind)
	      {
	      default: break;
	      case OBJC_PROPERTY_ATTR_UNKNOWN:
		if (attr_name)
		  error_at (attr_comb, "unknown property attribute %<%s%s%>",
			    add_at ? "@" : "", IDENTIFIER_POINTER (attr_name));
		else
		  error_at (attr_comb, "unknown property attribute");
		prop->parse_error = syntax_error = true;
		break;

	      case OBJC_PROPERTY_ATTR_GETTER:
	      case OBJC_PROPERTY_ATTR_SETTER:
		if (c_parser_next_token_is_not (parser, CPP_EQ))
		  {
		    attr_comb = make_location (attr_end, attr_start, attr_end);
		    error_at (attr_comb, "expected %<=%> after Objective-C %qE",
			      attr_name);
		    prop->parse_error = syntax_error = true;
		    break;
		  }
		token = c_parser_peek_token (parser);
		attr_end = token->location;
		c_parser_consume_token (parser); /* eat the = */
		if (c_parser_next_token_is_not (parser, CPP_NAME))
		  {
		    attr_comb = make_location (attr_end, attr_start, attr_end);
		    error_at (attr_comb, "expected %qE selector name",
			      attr_name);
		    prop->parse_error = syntax_error = true;
		    break;
		  }
		/* Get the end of the method name, and consume the name.  */
		token = c_parser_peek_token (parser);
		attr_end = get_finish (token->location);
		meth_name = token->value;
		c_parser_consume_token (parser);
		if (prop->prop_kind == OBJC_PROPERTY_ATTR_SETTER)
		  {
		    if (c_parser_next_token_is_not (parser, CPP_COLON))
		      {
			attr_comb = make_location (attr_end, attr_start,
						   attr_end);
			error_at (attr_comb, "setter method names must"
				  " terminate with %<:%>");
			prop->parse_error = syntax_error = true;
		      }
		    else
		      {
			attr_end = get_finish (c_parser_peek_token
					       (parser)->location);
			c_parser_consume_token (parser);
		      }
		    attr_comb = make_location (attr_start, attr_start,
					       attr_end);
		  }
		else
		  attr_comb = make_location (attr_start, attr_start,
					       attr_end);
		prop->ident = meth_name;
		/* Updated location including all that was successfully
		   parsed.  */
		prop->prop_loc = attr_comb;
		break;
	    }

	  /* If we see a comma here, then keep going - even if we already
	     saw a syntax error.  For simple mistakes e.g. (asign, getter=x)
	     this makes a more useful output and avoid spurious warnings about
	     missing attributes that are, in fact, specified after the one with
	     the syntax error.  */
	  if (c_parser_next_token_is (parser, CPP_COMMA))
	    c_parser_consume_token (parser);
	  else
	    break;
	}
      parser->objc_property_attr_context = false;

      if (syntax_error && c_parser_next_token_is_not (parser, CPP_CLOSE_PAREN))
	/* We don't really want to chew the whole of the file looking for a
	   matching closing parenthesis, so we will try to read the decl and
	   let the error handling for that close out the statement.  */
	;
      else
	syntax_error = false, parens.skip_until_found_close (parser);
    }

  /* 'properties' is the list of properties that we read.  Usually a
     single one, but maybe more (eg, in "@property int a, b, c;" there
     are three).  */
  tree properties = c_parser_struct_declaration (parser);

  if (properties == error_mark_node)
    c_parser_skip_until_found (parser, CPP_SEMICOLON, NULL);
  else
    {
      if (properties == NULL_TREE)
	c_parser_error (parser, "expected identifier");
      else
	{
	  /* Comma-separated properties are chained together in reverse order;
	     add them one by one.  */
	  properties = nreverse (properties);
	  for (; properties; properties = TREE_CHAIN (properties))
	    objc_add_property_declaration (loc, copy_node (properties),
					    prop_attr_list);
	}
      c_parser_skip_until_found (parser, CPP_SEMICOLON, "expected %<;%>");
    }

  while (!prop_attr_list.is_empty())
    delete prop_attr_list.pop ();
  prop_attr_list.release ();
  parser->error = false;
}

/* Parse an Objective-C @synthesize declaration.  The syntax is:

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

  PS: This function is identical to cp_parser_objc_at_synthesize_declaration
  for C++.  Keep them in sync.
*/
static void
c_parser_objc_at_synthesize_declaration (c_parser *parser)
{
  tree list = NULL_TREE;
  location_t loc;
  gcc_assert (c_parser_next_token_is_keyword (parser, RID_AT_SYNTHESIZE));
  loc = c_parser_peek_token (parser)->location;

  c_parser_consume_token (parser);
  while (true)
    {
      tree property, ivar;
      if (c_parser_next_token_is_not (parser, CPP_NAME))
	{
	  c_parser_error (parser, "expected identifier");
	  c_parser_skip_until_found (parser, CPP_SEMICOLON, NULL);
	  /* Once we find the semicolon, we can resume normal parsing.
	     We have to reset parser->error manually because
	     c_parser_skip_until_found() won't reset it for us if the
	     next token is precisely a semicolon.  */
	  parser->error = false;
	  return;
	}
      property = c_parser_peek_token (parser)->value;
      c_parser_consume_token (parser);
      if (c_parser_next_token_is (parser, CPP_EQ))
	{
	  c_parser_consume_token (parser);
	  if (c_parser_next_token_is_not (parser, CPP_NAME))
	    {
	      c_parser_error (parser, "expected identifier");
	      c_parser_skip_until_found (parser, CPP_SEMICOLON, NULL);
	      parser->error = false;
	      return;
	    }
	  ivar = c_parser_peek_token (parser)->value;
	  c_parser_consume_token (parser);
	}
      else
	ivar = NULL_TREE;
      list = chainon (list, build_tree_list (ivar, property));
      if (c_parser_next_token_is (parser, CPP_COMMA))
	c_parser_consume_token (parser);
      else
	break;
    }
  c_parser_skip_until_found (parser, CPP_SEMICOLON, "expected %<;%>");
  objc_add_synthesize_declaration (loc, list);
}

/* Parse an Objective-C @dynamic declaration.  The syntax is:

   objc-dynamic-declaration:
     @dynamic identifier-list ;

   For example:
     @dynamic MyProperty;
     @dynamic MyProperty, AnotherProperty;

  PS: This function is identical to cp_parser_objc_at_dynamic_declaration
  for C++.  Keep them in sync.
*/
static void
c_parser_objc_at_dynamic_declaration (c_parser *parser)
{
  tree list = NULL_TREE;
  location_t loc;
  gcc_assert (c_parser_next_token_is_keyword (parser, RID_AT_DYNAMIC));
  loc = c_parser_peek_token (parser)->location;

  c_parser_consume_token (parser);
  while (true)
    {
      tree property;
      if (c_parser_next_token_is_not (parser, CPP_NAME))
	{
	  c_parser_error (parser, "expected identifier");
	  c_parser_skip_until_found (parser, CPP_SEMICOLON, NULL);
	  parser->error = false;
	  return;
	}
      property = c_parser_peek_token (parser)->value;
      list = chainon (list, build_tree_list (NULL_TREE, property));
      c_parser_consume_token (parser);
      if (c_parser_next_token_is (parser, CPP_COMMA))
	c_parser_consume_token (parser);
      else
	break;
    }
  c_parser_skip_until_found (parser, CPP_SEMICOLON, "expected %<;%>");
  objc_add_dynamic_declaration (loc, list);
}


/* Parse a pragma GCC ivdep.  */

static bool
c_parse_pragma_ivdep (c_parser *parser)
{
  c_parser_consume_pragma (parser);
  c_parser_skip_to_pragma_eol (parser);
  return true;
}

/* Parse a pragma GCC unroll.  */

static unsigned short
c_parser_pragma_unroll (c_parser *parser)
{
  unsigned short unroll;
  c_parser_consume_pragma (parser);
  location_t location = c_parser_peek_token (parser)->location;
  tree expr = c_parser_expr_no_commas (parser, NULL).value;
  mark_exp_read (expr);
  expr = c_fully_fold (expr, false, NULL);
  HOST_WIDE_INT lunroll = 0;
  if (!INTEGRAL_TYPE_P (TREE_TYPE (expr))
      || TREE_CODE (expr) != INTEGER_CST
      || (lunroll = tree_to_shwi (expr)) < 0
      || lunroll >= USHRT_MAX)
    {
      error_at (location, "%<#pragma GCC unroll%> requires an"
		" assignment-expression that evaluates to a non-negative"
		" integral constant less than %u", USHRT_MAX);
      unroll = 0;
    }
  else
    {
      unroll = (unsigned short)lunroll;
      if (unroll == 0)
	unroll = 1;
    }

  c_parser_skip_to_pragma_eol (parser);
  return unroll;
}

/* Handle pragmas.  Some OpenMP pragmas are associated with, and therefore
   should be considered, statements.  ALLOW_STMT is true if we're within
   the context of a function and such pragmas are to be allowed.  Returns
   true if we actually parsed such a pragma.  */

static bool
c_parser_pragma (c_parser *parser, enum pragma_context context, bool *if_p)
{
  unsigned int id;
  const char *construct = NULL;

  id = c_parser_peek_token (parser)->pragma_kind;
  gcc_assert (id != PRAGMA_NONE);

  switch (id)
    {
    case PRAGMA_OACC_DECLARE:
      c_parser_oacc_declare (parser);
      return false;

    case PRAGMA_OACC_ENTER_DATA:
      if (context != pragma_compound)
	{
	  construct = "acc enter data";
	in_compound:
	  if (context == pragma_stmt)
	    {
	      error_at (c_parser_peek_token (parser)->location,
			"%<#pragma %s%> may only be used in compound "
			"statements", construct);
	      c_parser_skip_until_found (parser, CPP_PRAGMA_EOL, NULL);
	      return false;
	    }
	  goto bad_stmt;
	}
      c_parser_oacc_enter_exit_data (parser, true);
      return false;

    case PRAGMA_OACC_EXIT_DATA:
      if (context != pragma_compound)
	{
	  construct = "acc exit data";
	  goto in_compound;
	}
      c_parser_oacc_enter_exit_data (parser, false);
      return false;

    case PRAGMA_OACC_ROUTINE:
      if (context != pragma_external)
	{
	  error_at (c_parser_peek_token (parser)->location,
		    "%<#pragma acc routine%> must be at file scope");
	  c_parser_skip_until_found (parser, CPP_PRAGMA_EOL, NULL);
	  return false;
	}
      c_parser_oacc_routine (parser, context);
      return false;

    case PRAGMA_OACC_UPDATE:
      if (context != pragma_compound)
	{
	  construct = "acc update";
	  goto in_compound;
	}
      c_parser_oacc_update (parser);
      return false;

    case PRAGMA_OMP_BARRIER:
      if (context != pragma_compound)
	{
	  construct = "omp barrier";
	  goto in_compound;
	}
      c_parser_omp_barrier (parser);
      return false;

    case PRAGMA_OMP_DEPOBJ:
      if (context != pragma_compound)
	{
	  construct = "omp depobj";
	  goto in_compound;
	}
      c_parser_omp_depobj (parser);
      return false;

    case PRAGMA_OMP_FLUSH:
      if (context != pragma_compound)
	{
	  construct = "omp flush";
	  goto in_compound;
	}
      c_parser_omp_flush (parser);
      return false;

    case PRAGMA_OMP_TASKWAIT:
      if (context != pragma_compound)
	{
	  construct = "omp taskwait";
	  goto in_compound;
	}
      c_parser_omp_taskwait (parser);
      return false;

    case PRAGMA_OMP_TASKYIELD:
      if (context != pragma_compound)
	{
	  construct = "omp taskyield";
	  goto in_compound;
	}
      c_parser_omp_taskyield (parser);
      return false;

    case PRAGMA_OMP_CANCEL:
      if (context != pragma_compound)
	{
	  construct = "omp cancel";
	  goto in_compound;
	}
      c_parser_omp_cancel (parser);
      return false;

    case PRAGMA_OMP_CANCELLATION_POINT:
      c_parser_omp_cancellation_point (parser, context);
      return false;

    case PRAGMA_OMP_THREADPRIVATE:
      c_parser_omp_threadprivate (parser);
      return false;

    case PRAGMA_OMP_TARGET:
      return c_parser_omp_target (parser, context, if_p);

    case PRAGMA_OMP_END_DECLARE_TARGET:
      c_parser_omp_end_declare_target (parser);
      return false;

    case PRAGMA_OMP_SCAN:
      error_at (c_parser_peek_token (parser)->location,
		"%<#pragma omp scan%> may only be used in "
		"a loop construct with %<inscan%> %<reduction%> clause");
      c_parser_skip_until_found (parser, CPP_PRAGMA_EOL, NULL);
      return false;

    case PRAGMA_OMP_SECTION:
      error_at (c_parser_peek_token (parser)->location,
		"%<#pragma omp section%> may only be used in "
		"%<#pragma omp sections%> construct");
      c_parser_skip_until_found (parser, CPP_PRAGMA_EOL, NULL);
      return false;

    case PRAGMA_OMP_DECLARE:
      c_parser_omp_declare (parser, context);
      return false;

    case PRAGMA_OMP_REQUIRES:
      if (context != pragma_external)
	{
	  error_at (c_parser_peek_token (parser)->location,
		    "%<#pragma omp requires%> may only be used at file scope");
	  c_parser_skip_until_found (parser, CPP_PRAGMA_EOL, NULL);
	  return false;
	}
      c_parser_omp_requires (parser);
      return false;

    case PRAGMA_OMP_ORDERED:
      return c_parser_omp_ordered (parser, context, if_p);

    case PRAGMA_IVDEP:
      {
	const bool ivdep = c_parse_pragma_ivdep (parser);
	unsigned short unroll;
	if (c_parser_peek_token (parser)->pragma_kind == PRAGMA_UNROLL)
	  unroll = c_parser_pragma_unroll (parser);
	else
	  unroll = 0;
	if (!c_parser_next_token_is_keyword (parser, RID_FOR)
	    && !c_parser_next_token_is_keyword (parser, RID_WHILE)
	    && !c_parser_next_token_is_keyword (parser, RID_DO))
	  {
	    c_parser_error (parser, "for, while or do statement expected");
	    return false;
	  }
	if (c_parser_next_token_is_keyword (parser, RID_FOR))
	  c_parser_for_statement (parser, ivdep, unroll, if_p);
	else if (c_parser_next_token_is_keyword (parser, RID_WHILE))
	  c_parser_while_statement (parser, ivdep, unroll, if_p);
	else
	  c_parser_do_statement (parser, ivdep, unroll);
      }
      return false;

    case PRAGMA_UNROLL:
      {
	unsigned short unroll = c_parser_pragma_unroll (parser);
	bool ivdep;
	if (c_parser_peek_token (parser)->pragma_kind == PRAGMA_IVDEP)
	  ivdep = c_parse_pragma_ivdep (parser);
	else
	  ivdep = false;
	if (!c_parser_next_token_is_keyword (parser, RID_FOR)
	    && !c_parser_next_token_is_keyword (parser, RID_WHILE)
	    && !c_parser_next_token_is_keyword (parser, RID_DO))
	  {
	    c_parser_error (parser, "for, while or do statement expected");
	    return false;
	  }
	if (c_parser_next_token_is_keyword (parser, RID_FOR))
	  c_parser_for_statement (parser, ivdep, unroll, if_p);
	else if (c_parser_next_token_is_keyword (parser, RID_WHILE))
	  c_parser_while_statement (parser, ivdep, unroll, if_p);
	else
	  c_parser_do_statement (parser, ivdep, unroll);
      }
      return false;

    case PRAGMA_GCC_PCH_PREPROCESS:
      c_parser_error (parser, "%<#pragma GCC pch_preprocess%> must be first");
      c_parser_skip_until_found (parser, CPP_PRAGMA_EOL, NULL);
      return false;

    case PRAGMA_OACC_WAIT:
      if (context != pragma_compound)
	{
	  construct = "acc wait";
	  goto in_compound;
	}
	/* FALL THROUGH.  */

    default:
      if (id < PRAGMA_FIRST_EXTERNAL)
	{
	  if (context != pragma_stmt && context != pragma_compound)
	    {
	    bad_stmt:
	      c_parser_error (parser, "expected declaration specifiers");
	      c_parser_skip_until_found (parser, CPP_PRAGMA_EOL, NULL);
	      return false;
	    }
	  c_parser_omp_construct (parser, if_p);
	  return true;
	}
      break;
    }

  c_parser_consume_pragma (parser);
  c_invoke_pragma_handler (id);

  /* Skip to EOL, but suppress any error message.  Those will have been
     generated by the handler routine through calling error, as opposed
     to calling c_parser_error.  */
  parser->error = true;
  c_parser_skip_to_pragma_eol (parser);

  return false;
}

/* The interface the pragma parsers have to the lexer.  */

enum cpp_ttype
pragma_lex (tree *value, location_t *loc)
{
  c_token *tok = c_parser_peek_token (the_parser);
  enum cpp_ttype ret = tok->type;

  *value = tok->value;
  if (loc)
    *loc = tok->location;

  if (ret == CPP_PRAGMA_EOL || ret == CPP_EOF)
    ret = CPP_EOF;
  else if (ret == CPP_STRING)
    *value = c_parser_string_literal (the_parser, false, false).value;
  else
    {
      if (ret == CPP_KEYWORD)
	ret = CPP_NAME;
      c_parser_consume_token (the_parser);
    }

  return ret;
}

static void
c_parser_pragma_pch_preprocess (c_parser *parser)
{
  tree name = NULL;

  parser->lex_joined_string = true;
  c_parser_consume_pragma (parser);
  if (c_parser_next_token_is (parser, CPP_STRING))
    {
      name = c_parser_peek_token (parser)->value;
      c_parser_consume_token (parser);
    }
  else
    c_parser_error (parser, "expected string literal");
  c_parser_skip_to_pragma_eol (parser);
  parser->lex_joined_string = false;

  if (name)
    c_common_pch_pragma (parse_in, TREE_STRING_POINTER (name));
}

/* OpenACC and OpenMP parsing routines.  */

/* Returns name of the next clause.
   If the clause is not recognized PRAGMA_OMP_CLAUSE_NONE is returned and
   the token is not consumed.  Otherwise appropriate pragma_omp_clause is
   returned and the token is consumed.  */

static pragma_omp_clause
c_parser_omp_clause_name (c_parser *parser)
{
  pragma_omp_clause result = PRAGMA_OMP_CLAUSE_NONE;

  if (c_parser_next_token_is_keyword (parser, RID_AUTO))
    result = PRAGMA_OACC_CLAUSE_AUTO;
  else if (c_parser_next_token_is_keyword (parser, RID_IF))
    result = PRAGMA_OMP_CLAUSE_IF;
  else if (c_parser_next_token_is_keyword (parser, RID_DEFAULT))
    result = PRAGMA_OMP_CLAUSE_DEFAULT;
  else if (c_parser_next_token_is_keyword (parser, RID_FOR))
    result = PRAGMA_OMP_CLAUSE_FOR;
  else if (c_parser_next_token_is (parser, CPP_NAME))
    {
      const char *p = IDENTIFIER_POINTER (c_parser_peek_token (parser)->value);

      switch (p[0])
	{
	case 'a':
	  if (!strcmp ("aligned", p))
	    result = PRAGMA_OMP_CLAUSE_ALIGNED;
	  else if (!strcmp ("allocate", p))
	    result = PRAGMA_OMP_CLAUSE_ALLOCATE;
	  else if (!strcmp ("async", p))
	    result = PRAGMA_OACC_CLAUSE_ASYNC;
	  else if (!strcmp ("attach", p))
	    result = PRAGMA_OACC_CLAUSE_ATTACH;
	  break;
	case 'b':
	  if (!strcmp ("bind", p))
	    result = PRAGMA_OMP_CLAUSE_BIND;
	  break;
	case 'c':
	  if (!strcmp ("collapse", p))
	    result = PRAGMA_OMP_CLAUSE_COLLAPSE;
	  else if (!strcmp ("copy", p))
	    result = PRAGMA_OACC_CLAUSE_COPY;
	  else if (!strcmp ("copyin", p))
	    result = PRAGMA_OMP_CLAUSE_COPYIN;
	  else if (!strcmp ("copyout", p))
	    result = PRAGMA_OACC_CLAUSE_COPYOUT;
          else if (!strcmp ("copyprivate", p))
	    result = PRAGMA_OMP_CLAUSE_COPYPRIVATE;
	  else if (!strcmp ("create", p))
	    result = PRAGMA_OACC_CLAUSE_CREATE;
	  break;
	case 'd':
	  if (!strcmp ("defaultmap", p))
	    result = PRAGMA_OMP_CLAUSE_DEFAULTMAP;
	  else if (!strcmp ("delete", p))
	    result = PRAGMA_OACC_CLAUSE_DELETE;
	  else if (!strcmp ("depend", p))
	    result = PRAGMA_OMP_CLAUSE_DEPEND;
	  else if (!strcmp ("detach", p))
	    result = PRAGMA_OACC_CLAUSE_DETACH;
	  else if (!strcmp ("device", p))
	    result = PRAGMA_OMP_CLAUSE_DEVICE;
	  else if (!strcmp ("deviceptr", p))
	    result = PRAGMA_OACC_CLAUSE_DEVICEPTR;
	  else if (!strcmp ("device_resident", p))
	    result = PRAGMA_OACC_CLAUSE_DEVICE_RESIDENT;
	  else if (!strcmp ("device_type", p))
	    result = PRAGMA_OMP_CLAUSE_DEVICE_TYPE;
	  else if (!strcmp ("dist_schedule", p))
	    result = PRAGMA_OMP_CLAUSE_DIST_SCHEDULE;
	  break;
	case 'f':
	  if (!strcmp ("final", p))
	    result = PRAGMA_OMP_CLAUSE_FINAL;
	  else if (!strcmp ("finalize", p))
	    result = PRAGMA_OACC_CLAUSE_FINALIZE;
	  else if (!strcmp ("firstprivate", p))
	    result = PRAGMA_OMP_CLAUSE_FIRSTPRIVATE;
	  else if (!strcmp ("from", p))
	    result = PRAGMA_OMP_CLAUSE_FROM;
	  break;
	case 'g':
	  if (!strcmp ("gang", p))
	    result = PRAGMA_OACC_CLAUSE_GANG;
	  else if (!strcmp ("grainsize", p))
	    result = PRAGMA_OMP_CLAUSE_GRAINSIZE;
	  break;
	case 'h':
	  if (!strcmp ("hint", p))
	    result = PRAGMA_OMP_CLAUSE_HINT;
	  else if (!strcmp ("host", p))
	    result = PRAGMA_OACC_CLAUSE_HOST;
	  break;
	case 'i':
	  if (!strcmp ("if_present", p))
	    result = PRAGMA_OACC_CLAUSE_IF_PRESENT;
	  else if (!strcmp ("in_reduction", p))
	    result = PRAGMA_OMP_CLAUSE_IN_REDUCTION;
	  else if (!strcmp ("inbranch", p))
	    result = PRAGMA_OMP_CLAUSE_INBRANCH;
	  else if (!strcmp ("independent", p))
	    result = PRAGMA_OACC_CLAUSE_INDEPENDENT;
	  else if (!strcmp ("is_device_ptr", p))
	    result = PRAGMA_OMP_CLAUSE_IS_DEVICE_PTR;
	  break;
	case 'l':
	  if (!strcmp ("lastprivate", p))
	    result = PRAGMA_OMP_CLAUSE_LASTPRIVATE;
	  else if (!strcmp ("linear", p))
	    result = PRAGMA_OMP_CLAUSE_LINEAR;
	  else if (!strcmp ("link", p))
	    result = PRAGMA_OMP_CLAUSE_LINK;
	  break;
	case 'm':
	  if (!strcmp ("map", p))
	    result = PRAGMA_OMP_CLAUSE_MAP;
	  else if (!strcmp ("mergeable", p))
	    result = PRAGMA_OMP_CLAUSE_MERGEABLE;
	  break;
	case 'n':
	  if (!strcmp ("no_create", p))
	    result = PRAGMA_OACC_CLAUSE_NO_CREATE;
	  else if (!strcmp ("nogroup", p))
	    result = PRAGMA_OMP_CLAUSE_NOGROUP;
	  else if (!strcmp ("nontemporal", p))
	    result = PRAGMA_OMP_CLAUSE_NONTEMPORAL;
	  else if (!strcmp ("notinbranch", p))
	    result = PRAGMA_OMP_CLAUSE_NOTINBRANCH;
	  else if (!strcmp ("nowait", p))
	    result = PRAGMA_OMP_CLAUSE_NOWAIT;
	  else if (!strcmp ("num_gangs", p))
	    result = PRAGMA_OACC_CLAUSE_NUM_GANGS;
	  else if (!strcmp ("num_tasks", p))
	    result = PRAGMA_OMP_CLAUSE_NUM_TASKS;
	  else if (!strcmp ("num_teams", p))
	    result = PRAGMA_OMP_CLAUSE_NUM_TEAMS;
	  else if (!strcmp ("num_threads", p))
	    result = PRAGMA_OMP_CLAUSE_NUM_THREADS;
	  else if (!strcmp ("num_workers", p))
	    result = PRAGMA_OACC_CLAUSE_NUM_WORKERS;
	  break;
	case 'o':
	  if (!strcmp ("ordered", p))
	    result = PRAGMA_OMP_CLAUSE_ORDERED;
	  else if (!strcmp ("order", p))
	    result = PRAGMA_OMP_CLAUSE_ORDER;
	  break;
	case 'p':
	  if (!strcmp ("parallel", p))
	    result = PRAGMA_OMP_CLAUSE_PARALLEL;
	  else if (!strcmp ("present", p))
	    result = PRAGMA_OACC_CLAUSE_PRESENT;
	  /* As of OpenACC 2.5, these are now aliases of the non-present_or
	     clauses.  */
	  else if (!strcmp ("present_or_copy", p)
		   || !strcmp ("pcopy", p))
	    result = PRAGMA_OACC_CLAUSE_COPY;
	  else if (!strcmp ("present_or_copyin", p)
		   || !strcmp ("pcopyin", p))
	    result = PRAGMA_OACC_CLAUSE_COPYIN;
	  else if (!strcmp ("present_or_copyout", p)
		   || !strcmp ("pcopyout", p))
	    result = PRAGMA_OACC_CLAUSE_COPYOUT;
	  else if (!strcmp ("present_or_create", p)
		   || !strcmp ("pcreate", p))
	    result = PRAGMA_OACC_CLAUSE_CREATE;
	  else if (!strcmp ("priority", p))
	    result = PRAGMA_OMP_CLAUSE_PRIORITY;
	  else if (!strcmp ("private", p))
	    result = PRAGMA_OMP_CLAUSE_PRIVATE;
	  else if (!strcmp ("proc_bind", p))
	    result = PRAGMA_OMP_CLAUSE_PROC_BIND;
	  break;
	case 'r':
	  if (!strcmp ("reduction", p))
	    result = PRAGMA_OMP_CLAUSE_REDUCTION;
	  break;
	case 's':
	  if (!strcmp ("safelen", p))
	    result = PRAGMA_OMP_CLAUSE_SAFELEN;
	  else if (!strcmp ("schedule", p))
	    result = PRAGMA_OMP_CLAUSE_SCHEDULE;
	  else if (!strcmp ("sections", p))
	    result = PRAGMA_OMP_CLAUSE_SECTIONS;
	  else if (!strcmp ("self", p)) /* "self" is a synonym for "host".  */
	    result = PRAGMA_OACC_CLAUSE_HOST;
	  else if (!strcmp ("seq", p))
	    result = PRAGMA_OACC_CLAUSE_SEQ;
	  else if (!strcmp ("shared", p))
	    result = PRAGMA_OMP_CLAUSE_SHARED;
	  else if (!strcmp ("simd", p))
	    result = PRAGMA_OMP_CLAUSE_SIMD;
	  else if (!strcmp ("simdlen", p))
	    result = PRAGMA_OMP_CLAUSE_SIMDLEN;
	  break;
	case 't':
	  if (!strcmp ("task_reduction", p))
	    result = PRAGMA_OMP_CLAUSE_TASK_REDUCTION;
	  else if (!strcmp ("taskgroup", p))
	    result = PRAGMA_OMP_CLAUSE_TASKGROUP;
	  else if (!strcmp ("thread_limit", p))
	    result = PRAGMA_OMP_CLAUSE_THREAD_LIMIT;
	  else if (!strcmp ("threads", p))
	    result = PRAGMA_OMP_CLAUSE_THREADS;
	  else if (!strcmp ("tile", p))
	    result = PRAGMA_OACC_CLAUSE_TILE;
	  else if (!strcmp ("to", p))
	    result = PRAGMA_OMP_CLAUSE_TO;
	  break;
	case 'u':
	  if (!strcmp ("uniform", p))
	    result = PRAGMA_OMP_CLAUSE_UNIFORM;
	  else if (!strcmp ("untied", p))
	    result = PRAGMA_OMP_CLAUSE_UNTIED;
	  else if (!strcmp ("use_device", p))
	    result = PRAGMA_OACC_CLAUSE_USE_DEVICE;
	  else if (!strcmp ("use_device_addr", p))
	    result = PRAGMA_OMP_CLAUSE_USE_DEVICE_ADDR;
	  else if (!strcmp ("use_device_ptr", p))
	    result = PRAGMA_OMP_CLAUSE_USE_DEVICE_PTR;
	  break;
	case 'v':
	  if (!strcmp ("vector", p))
	    result = PRAGMA_OACC_CLAUSE_VECTOR;
	  else if (!strcmp ("vector_length", p))
	    result = PRAGMA_OACC_CLAUSE_VECTOR_LENGTH;
	  break;
	case 'w':
	  if (!strcmp ("wait", p))
	    result = PRAGMA_OACC_CLAUSE_WAIT;
	  else if (!strcmp ("worker", p))
	    result = PRAGMA_OACC_CLAUSE_WORKER;
	  break;
	}
    }

  if (result != PRAGMA_OMP_CLAUSE_NONE)
    c_parser_consume_token (parser);

  return result;
}

/* Validate that a clause of the given type does not already exist.  */

static void
check_no_duplicate_clause (tree clauses, enum omp_clause_code code,
			   const char *name)
{
  if (tree c = omp_find_clause (clauses, code))
    error_at (OMP_CLAUSE_LOCATION (c), "too many %qs clauses", name);
}

/* OpenACC 2.0
   Parse wait clause or wait directive parameters.  */

static tree
c_parser_oacc_wait_list (c_parser *parser, location_t clause_loc, tree list)
{
  vec<tree, va_gc> *args;
  tree t, args_tree;

  matching_parens parens;
  if (!parens.require_open (parser))
    return list;

  args = c_parser_expr_list (parser, false, true, NULL, NULL, NULL, NULL);
  args_tree = build_tree_list_vec (args);

  for (t = args_tree; t; t = TREE_CHAIN (t))
    {
      tree targ = TREE_VALUE (t);

      if (targ != error_mark_node)
	{
	  if (!INTEGRAL_TYPE_P (TREE_TYPE (targ)))
	    {
	      c_parser_error (parser, "expression must be integral");
	      targ = error_mark_node;
	    }
	  else
	    {
	      tree c = build_omp_clause (clause_loc, OMP_CLAUSE_WAIT);

	      OMP_CLAUSE_DECL (c) = targ;
	      OMP_CLAUSE_CHAIN (c) = list;
	      list = c;
	    }
	}
    }

  release_tree_vector (args);
  parens.require_close (parser);
  return list;
}

/* OpenACC 2.0, OpenMP 2.5:
   variable-list:
     identifier
     variable-list , identifier

   If KIND is nonzero, create the appropriate node and install the
   decl in OMP_CLAUSE_DECL and add the node to the head of the list.
   If KIND is nonzero, CLAUSE_LOC is the location of the clause.

   If KIND is zero, create a TREE_LIST with the decl in TREE_PURPOSE;
   return the list created.

   The optional ALLOW_DEREF argument is true if list items can use the deref
   (->) operator.  */

static tree
c_parser_omp_variable_list (c_parser *parser,
			    location_t clause_loc,
			    enum omp_clause_code kind, tree list,
			    bool allow_deref = false)
{
  auto_vec<c_token> tokens;
  unsigned int tokens_avail = 0;
  bool first = true;

  while (1)
    {
      bool array_section_p = false;
      if (kind == OMP_CLAUSE_DEPEND)
	{
	  if (c_parser_next_token_is_not (parser, CPP_NAME)
	      || c_parser_peek_token (parser)->id_kind != C_ID_ID)
	    {
	      struct c_expr expr = c_parser_expr_no_commas (parser, NULL);
	      if (expr.value != error_mark_node)
		{
		  tree u = build_omp_clause (clause_loc, kind);
		  OMP_CLAUSE_DECL (u) = expr.value;
		  OMP_CLAUSE_CHAIN (u) = list;
		  list = u;
		}

	      if (c_parser_next_token_is_not (parser, CPP_COMMA))
		break;

	      c_parser_consume_token (parser);
	      first = false;
	      continue;
	    }

	  tokens.truncate (0);
	  unsigned int nesting_depth = 0;
	  while (1)
	    {
	      c_token *token = c_parser_peek_token (parser);
	      switch (token->type)
		{
		case CPP_EOF:
		case CPP_PRAGMA_EOL:
		  break;
		case CPP_OPEN_BRACE:
		case CPP_OPEN_PAREN:
		case CPP_OPEN_SQUARE:
		  ++nesting_depth;
		  goto add;
		case CPP_CLOSE_BRACE:
		case CPP_CLOSE_PAREN:
		case CPP_CLOSE_SQUARE:
		  if (nesting_depth-- == 0)
		    break;
		  goto add;
		case CPP_COMMA:
		  if (nesting_depth == 0)
		    break;
		  goto add;
		default:
		add:
		  tokens.safe_push (*token);
		  c_parser_consume_token (parser);
		  continue;
		}
	      break;
	    }

	  /* Make sure nothing tries to read past the end of the tokens.  */
	  c_token eof_token;
	  memset (&eof_token, 0, sizeof (eof_token));
	  eof_token.type = CPP_EOF;
	  tokens.safe_push (eof_token);
	  tokens.safe_push (eof_token);

	  tokens_avail = parser->tokens_avail;
	  gcc_assert (parser->tokens == &parser->tokens_buf[0]);
	  parser->tokens = tokens.address ();
	  parser->tokens_avail = tokens.length ();
	}

      tree t = NULL_TREE;

      if (c_parser_next_token_is (parser, CPP_NAME)
	  && c_parser_peek_token (parser)->id_kind == C_ID_ID)
	{
	  t = lookup_name (c_parser_peek_token (parser)->value);

	  if (t == NULL_TREE)
	    {
	      undeclared_variable (c_parser_peek_token (parser)->location,
	      c_parser_peek_token (parser)->value);
	      t = error_mark_node;
	    }

	  c_parser_consume_token (parser);
	}
      else if (c_parser_next_token_is (parser, CPP_KEYWORD)
	       && (c_parser_peek_token (parser)->keyword == RID_FUNCTION_NAME
		   || (c_parser_peek_token (parser)->keyword
		       == RID_PRETTY_FUNCTION_NAME)
		   || (c_parser_peek_token (parser)->keyword
		       == RID_C99_FUNCTION_NAME)))
	t = c_parser_predefined_identifier (parser).value;
      else
	{
	  if (first)
	    c_parser_error (parser, "expected identifier");
	  break;
	}

      if (t == error_mark_node)
	;
      else if (kind != 0)
	{
	  switch (kind)
	    {
	    case OMP_CLAUSE__CACHE_:
	      /* The OpenACC cache directive explicitly only allows "array
		 elements or subarrays".  */
	      if (c_parser_peek_token (parser)->type != CPP_OPEN_SQUARE)
		{
		  c_parser_error (parser, "expected %<[%>");
		  t = error_mark_node;
		  break;
		}
	      /* FALLTHROUGH  */
	    case OMP_CLAUSE_MAP:
	    case OMP_CLAUSE_FROM:
	    case OMP_CLAUSE_TO:
	      while (c_parser_next_token_is (parser, CPP_DOT)
		     || (allow_deref
			 && c_parser_next_token_is (parser, CPP_DEREF)))
		{
		  location_t op_loc = c_parser_peek_token (parser)->location;
		  if (c_parser_next_token_is (parser, CPP_DEREF))
		    t = build_simple_mem_ref (t);
		  c_parser_consume_token (parser);
		  if (!c_parser_next_token_is (parser, CPP_NAME))
		    {
		      c_parser_error (parser, "expected identifier");
		      t = error_mark_node;
		      break;
		    }

		  c_token *comp_tok = c_parser_peek_token (parser);
		  tree ident = comp_tok->value;
		  location_t comp_loc = comp_tok->location;
		  c_parser_consume_token (parser);
		  t = build_component_ref (op_loc, t, ident, comp_loc);
		}
	      /* FALLTHROUGH  */
	    case OMP_CLAUSE_DEPEND:
	    case OMP_CLAUSE_REDUCTION:
	    case OMP_CLAUSE_IN_REDUCTION:
	    case OMP_CLAUSE_TASK_REDUCTION:
	      while (c_parser_next_token_is (parser, CPP_OPEN_SQUARE))
		{
		  tree low_bound = NULL_TREE, length = NULL_TREE;

		  c_parser_consume_token (parser);
		  if (!c_parser_next_token_is (parser, CPP_COLON))
		    {
		      location_t expr_loc
			= c_parser_peek_token (parser)->location;
		      c_expr expr = c_parser_expression (parser);
		      expr = convert_lvalue_to_rvalue (expr_loc, expr,
						       false, true);
		      low_bound = expr.value;
		    }
		  if (c_parser_next_token_is (parser, CPP_CLOSE_SQUARE))
		    length = integer_one_node;
		  else
		    {
		      /* Look for `:'.  */
		      if (!c_parser_require (parser, CPP_COLON,
					     "expected %<:%>"))
			{
			  t = error_mark_node;
			  break;
			}
		      array_section_p = true;
		      if (!c_parser_next_token_is (parser, CPP_CLOSE_SQUARE))
			{
			  location_t expr_loc
			    = c_parser_peek_token (parser)->location;
			  c_expr expr = c_parser_expression (parser);
			  expr = convert_lvalue_to_rvalue (expr_loc, expr,
							   false, true);
			  length = expr.value;
			}
		    }
		  /* Look for the closing `]'.  */
		  if (!c_parser_require (parser, CPP_CLOSE_SQUARE,
					 "expected %<]%>"))
		    {
		      t = error_mark_node;
		      break;
		    }

		  t = tree_cons (low_bound, length, t);
		}
	      if (kind == OMP_CLAUSE_DEPEND
		  && t != error_mark_node
		  && parser->tokens_avail != 2)
		{
		  if (array_section_p)
		    {
		      error_at (c_parser_peek_token (parser)->location,
				"expected %<)%> or %<,%>");
		      t = error_mark_node;
		    }
		  else
		    {
		      parser->tokens = tokens.address ();
		      parser->tokens_avail = tokens.length ();

		      t = c_parser_expr_no_commas (parser, NULL).value;
		      if (t != error_mark_node && parser->tokens_avail != 2)
			{
			  error_at (c_parser_peek_token (parser)->location,
				    "expected %<)%> or %<,%>");
			  t = error_mark_node;
			}
		    }
		}
	      break;
	    default:
	      break;
	    }

	  if (t != error_mark_node)
	    {
	      tree u = build_omp_clause (clause_loc, kind);
	      OMP_CLAUSE_DECL (u) = t;
	      OMP_CLAUSE_CHAIN (u) = list;
	      list = u;
	    }
	}
      else
	list = tree_cons (t, NULL_TREE, list);

      if (kind == OMP_CLAUSE_DEPEND)
	{
	  parser->tokens = &parser->tokens_buf[0];
	  parser->tokens_avail = tokens_avail;
	}
      if (c_parser_next_token_is_not (parser, CPP_COMMA))
	break;

      c_parser_consume_token (parser);
      first = false;
    }

  return list;
}

/* Similarly, but expect leading and trailing parenthesis.  This is a very
   common case for OpenACC and OpenMP clauses.  The optional ALLOW_DEREF
   argument is true if list items can use the deref (->) operator.  */

static tree
c_parser_omp_var_list_parens (c_parser *parser, enum omp_clause_code kind,
			      tree list, bool allow_deref = false)
{
  /* The clauses location.  */
  location_t loc = c_parser_peek_token (parser)->location;

  matching_parens parens;
  if (parens.require_open (parser))
    {
      list = c_parser_omp_variable_list (parser, loc, kind, list, allow_deref);
      parens.skip_until_found_close (parser);
    }
  return list;
}

/* OpenACC 2.0:
   copy ( variable-list )
   copyin ( variable-list )
   copyout ( variable-list )
   create ( variable-list )
   delete ( variable-list )
   present ( variable-list )

   OpenACC 2.6:
   no_create ( variable-list )
   attach ( variable-list )
   detach ( variable-list ) */

static tree
c_parser_oacc_data_clause (c_parser *parser, pragma_omp_clause c_kind,
			   tree list)
{
  enum gomp_map_kind kind;
  switch (c_kind)
    {
    case PRAGMA_OACC_CLAUSE_ATTACH:
      kind = GOMP_MAP_ATTACH;
      break;
    case PRAGMA_OACC_CLAUSE_COPY:
      kind = GOMP_MAP_TOFROM;
      break;
    case PRAGMA_OACC_CLAUSE_COPYIN:
      kind = GOMP_MAP_TO;
      break;
    case PRAGMA_OACC_CLAUSE_COPYOUT:
      kind = GOMP_MAP_FROM;
      break;
    case PRAGMA_OACC_CLAUSE_CREATE:
      kind = GOMP_MAP_ALLOC;
      break;
    case PRAGMA_OACC_CLAUSE_DELETE:
      kind = GOMP_MAP_RELEASE;
      break;
    case PRAGMA_OACC_CLAUSE_DETACH:
      kind = GOMP_MAP_DETACH;
      break;
    case PRAGMA_OACC_CLAUSE_DEVICE:
      kind = GOMP_MAP_FORCE_TO;
      break;
    case PRAGMA_OACC_CLAUSE_DEVICE_RESIDENT:
      kind = GOMP_MAP_DEVICE_RESIDENT;
      break;
    case PRAGMA_OACC_CLAUSE_HOST:
      kind = GOMP_MAP_FORCE_FROM;
      break;
    case PRAGMA_OACC_CLAUSE_LINK:
      kind = GOMP_MAP_LINK;
      break;
    case PRAGMA_OACC_CLAUSE_NO_CREATE:
      kind = GOMP_MAP_IF_PRESENT;
      break;
    case PRAGMA_OACC_CLAUSE_PRESENT:
      kind = GOMP_MAP_FORCE_PRESENT;
      break;
    default:
      gcc_unreachable ();
    }
  tree nl, c;
  nl = c_parser_omp_var_list_parens (parser, OMP_CLAUSE_MAP, list, true);

  for (c = nl; c != list; c = OMP_CLAUSE_CHAIN (c))
    OMP_CLAUSE_SET_MAP_KIND (c, kind);

  return nl;
}

/* OpenACC 2.0:
   deviceptr ( variable-list ) */

static tree
c_parser_oacc_data_clause_deviceptr (c_parser *parser, tree list)
{
  location_t loc = c_parser_peek_token (parser)->location;
  tree vars, t;

  /* Can't use OMP_CLAUSE_MAP here (that is, can't use the generic
     c_parser_oacc_data_clause), as for PRAGMA_OACC_CLAUSE_DEVICEPTR,
     variable-list must only allow for pointer variables.  */
  vars = c_parser_omp_var_list_parens (parser, OMP_CLAUSE_ERROR, NULL);
  for (t = vars; t && t; t = TREE_CHAIN (t))
    {
      tree v = TREE_PURPOSE (t);

      /* FIXME diagnostics: Ideally we should keep individual
	 locations for all the variables in the var list to make the
	 following errors more precise.  Perhaps
	 c_parser_omp_var_list_parens() should construct a list of
	 locations to go along with the var list.  */

      if (!VAR_P (v) && TREE_CODE (v) != PARM_DECL)
	error_at (loc, "%qD is not a variable", v);
      else if (TREE_TYPE (v) == error_mark_node)
	;
      else if (!POINTER_TYPE_P (TREE_TYPE (v)))
	error_at (loc, "%qD is not a pointer variable", v);

      tree u = build_omp_clause (loc, OMP_CLAUSE_MAP);
      OMP_CLAUSE_SET_MAP_KIND (u, GOMP_MAP_FORCE_DEVICEPTR);
      OMP_CLAUSE_DECL (u) = v;
      OMP_CLAUSE_CHAIN (u) = list;
      list = u;
    }

  return list;
}

/* OpenACC 2.0, OpenMP 3.0:
   collapse ( constant-expression ) */

static tree
c_parser_omp_clause_collapse (c_parser *parser, tree list)
{
  tree c, num = error_mark_node;
  HOST_WIDE_INT n;
  location_t loc;

  check_no_duplicate_clause (list, OMP_CLAUSE_COLLAPSE, "collapse");
  check_no_duplicate_clause (list, OMP_CLAUSE_TILE, "tile");

  loc = c_parser_peek_token (parser)->location;
  matching_parens parens;
  if (parens.require_open (parser))
    {
      num = c_parser_expr_no_commas (parser, NULL).value;
      parens.skip_until_found_close (parser);
    }
  if (num == error_mark_node)
    return list;
  mark_exp_read (num);
  num = c_fully_fold (num, false, NULL);
  if (!INTEGRAL_TYPE_P (TREE_TYPE (num))
      || !tree_fits_shwi_p (num)
      || (n = tree_to_shwi (num)) <= 0
      || (int) n != n)
    {
      error_at (loc,
		"collapse argument needs positive constant integer expression");
      return list;
    }
  c = build_omp_clause (loc, OMP_CLAUSE_COLLAPSE);
  OMP_CLAUSE_COLLAPSE_EXPR (c) = num;
  OMP_CLAUSE_CHAIN (c) = list;
  return c;
}

/* OpenMP 2.5:
   copyin ( variable-list ) */

static tree
c_parser_omp_clause_copyin (c_parser *parser, tree list)
{
  return c_parser_omp_var_list_parens (parser, OMP_CLAUSE_COPYIN, list);
}

/* OpenMP 2.5:
   copyprivate ( variable-list ) */

static tree
c_parser_omp_clause_copyprivate (c_parser *parser, tree list)
{
  return c_parser_omp_var_list_parens (parser, OMP_CLAUSE_COPYPRIVATE, list);
}

/* OpenMP 2.5:
   default ( none | shared )

   OpenACC:
   default ( none | present ) */

static tree
c_parser_omp_clause_default (c_parser *parser, tree list, bool is_oacc)
{
  enum omp_clause_default_kind kind = OMP_CLAUSE_DEFAULT_UNSPECIFIED;
  location_t loc = c_parser_peek_token (parser)->location;
  tree c;

  matching_parens parens;
  if (!parens.require_open (parser))
    return list;
  if (c_parser_next_token_is (parser, CPP_NAME))
    {
      const char *p = IDENTIFIER_POINTER (c_parser_peek_token (parser)->value);

      switch (p[0])
	{
	case 'n':
	  if (strcmp ("none", p) != 0)
	    goto invalid_kind;
	  kind = OMP_CLAUSE_DEFAULT_NONE;
	  break;

	case 'p':
	  if (strcmp ("present", p) != 0 || !is_oacc)
	    goto invalid_kind;
	  kind = OMP_CLAUSE_DEFAULT_PRESENT;
	  break;

	case 's':
	  if (strcmp ("shared", p) != 0 || is_oacc)
	    goto invalid_kind;
	  kind = OMP_CLAUSE_DEFAULT_SHARED;
	  break;

	default:
	  goto invalid_kind;
	}

      c_parser_consume_token (parser);
    }
  else
    {
    invalid_kind:
      if (is_oacc)
	c_parser_error (parser, "expected %<none%> or %<present%>");
      else
	c_parser_error (parser, "expected %<none%> or %<shared%>");
    }
  parens.skip_until_found_close (parser);

  if (kind == OMP_CLAUSE_DEFAULT_UNSPECIFIED)
    return list;

  check_no_duplicate_clause (list, OMP_CLAUSE_DEFAULT, "default");
  c = build_omp_clause (loc, OMP_CLAUSE_DEFAULT);
  OMP_CLAUSE_CHAIN (c) = list;
  OMP_CLAUSE_DEFAULT_KIND (c) = kind;

  return c;
}

/* OpenMP 2.5:
   firstprivate ( variable-list ) */

static tree
c_parser_omp_clause_firstprivate (c_parser *parser, tree list)
{
  return c_parser_omp_var_list_parens (parser, OMP_CLAUSE_FIRSTPRIVATE, list);
}

/* OpenMP 3.1:
   final ( expression ) */

static tree
c_parser_omp_clause_final (c_parser *parser, tree list)
{
  location_t loc = c_parser_peek_token (parser)->location;
  if (c_parser_next_token_is (parser, CPP_OPEN_PAREN))
    {
      matching_parens parens;
      tree t, c;
      if (!parens.require_open (parser))
	t = error_mark_node;
      else
	{
	  location_t eloc = c_parser_peek_token (parser)->location;
	  c_expr expr = c_parser_expr_no_commas (parser, NULL);
	  t = convert_lvalue_to_rvalue (eloc, expr, true, true).value;
	  t = c_objc_common_truthvalue_conversion (eloc, t);
	  t = c_fully_fold (t, false, NULL);
	  parens.skip_until_found_close (parser);
	}

      check_no_duplicate_clause (list, OMP_CLAUSE_FINAL, "final");

      c = build_omp_clause (loc, OMP_CLAUSE_FINAL);
      OMP_CLAUSE_FINAL_EXPR (c) = t;
      OMP_CLAUSE_CHAIN (c) = list;
      list = c;
    }
  else
    c_parser_error (parser, "expected %<(%>");

  return list;
}

/* OpenACC, OpenMP 2.5:
   if ( expression )

   OpenMP 4.5:
   if ( directive-name-modifier : expression )

   directive-name-modifier:
     parallel | task | taskloop | target data | target | target update
     | target enter data | target exit data

   OpenMP 5.0:
   directive-name-modifier:
     ... | simd | cancel  */

static tree
c_parser_omp_clause_if (c_parser *parser, tree list, bool is_omp)
{
  location_t location = c_parser_peek_token (parser)->location;
  enum tree_code if_modifier = ERROR_MARK;

  matching_parens parens;
  if (!parens.require_open (parser))
    return list;

  if (is_omp && c_parser_next_token_is (parser, CPP_NAME))
    {
      const char *p = IDENTIFIER_POINTER (c_parser_peek_token (parser)->value);
      int n = 2;
      if (strcmp (p, "cancel") == 0)
	if_modifier = VOID_CST;
      else if (strcmp (p, "parallel") == 0)
	if_modifier = OMP_PARALLEL;
      else if (strcmp (p, "simd") == 0)
	if_modifier = OMP_SIMD;
      else if (strcmp (p, "task") == 0)
	if_modifier = OMP_TASK;
      else if (strcmp (p, "taskloop") == 0)
	if_modifier = OMP_TASKLOOP;
      else if (strcmp (p, "target") == 0)
	{
	  if_modifier = OMP_TARGET;
	  if (c_parser_peek_2nd_token (parser)->type == CPP_NAME)
	    {
	      p = IDENTIFIER_POINTER (c_parser_peek_2nd_token (parser)->value);
	      if (strcmp ("data", p) == 0)
		if_modifier = OMP_TARGET_DATA;
	      else if (strcmp ("update", p) == 0)
		if_modifier = OMP_TARGET_UPDATE;
	      else if (strcmp ("enter", p) == 0)
		if_modifier = OMP_TARGET_ENTER_DATA;
	      else if (strcmp ("exit", p) == 0)
		if_modifier = OMP_TARGET_EXIT_DATA;
	      if (if_modifier != OMP_TARGET)
		{
		  n = 3;
		  c_parser_consume_token (parser);
		}
	      else
		{
		  location_t loc = c_parser_peek_2nd_token (parser)->location;
		  error_at (loc, "expected %<data%>, %<update%>, %<enter%> "
				 "or %<exit%>");
		  if_modifier = ERROR_MARK;
		}
	      if (if_modifier == OMP_TARGET_ENTER_DATA
		  || if_modifier == OMP_TARGET_EXIT_DATA)
		{
		  if (c_parser_peek_2nd_token (parser)->type == CPP_NAME)
		    {
		      p = IDENTIFIER_POINTER
				(c_parser_peek_2nd_token (parser)->value);
		      if (strcmp ("data", p) == 0)
			n = 4;
		    }
		  if (n == 4)
		    c_parser_consume_token (parser);
		  else
		    {
		      location_t loc
			= c_parser_peek_2nd_token (parser)->location;
		      error_at (loc, "expected %<data%>");
		      if_modifier = ERROR_MARK;
		    }
		}
	    }
	}
      if (if_modifier != ERROR_MARK)
	{
	  if (c_parser_peek_2nd_token (parser)->type == CPP_COLON)
	    {
	      c_parser_consume_token (parser);
	      c_parser_consume_token (parser);
	    }
	  else
	    {
	      if (n > 2)
		{
		  location_t loc = c_parser_peek_2nd_token (parser)->location;
		  error_at (loc, "expected %<:%>");
		}
	      if_modifier = ERROR_MARK;
	    }
	}
    }

  location_t loc = c_parser_peek_token (parser)->location;
  c_expr expr = c_parser_expr_no_commas (parser, NULL);
  expr = convert_lvalue_to_rvalue (loc, expr, true, true);
  tree t = c_objc_common_truthvalue_conversion (loc, expr.value), c;
  t = c_fully_fold (t, false, NULL);
  parens.skip_until_found_close (parser);

  for (c = list; c ; c = OMP_CLAUSE_CHAIN (c))
    if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_IF)
      {
	if (if_modifier != ERROR_MARK
	    && OMP_CLAUSE_IF_MODIFIER (c) == if_modifier)
	  {
	    const char *p = NULL;
	    switch (if_modifier)
	      {
	      case VOID_CST: p = "cancel"; break;
	      case OMP_PARALLEL: p = "parallel"; break;
	      case OMP_SIMD: p = "simd"; break;
	      case OMP_TASK: p = "task"; break;
	      case OMP_TASKLOOP: p = "taskloop"; break;
	      case OMP_TARGET_DATA: p = "target data"; break;
	      case OMP_TARGET: p = "target"; break;
	      case OMP_TARGET_UPDATE: p = "target update"; break;
	      case OMP_TARGET_ENTER_DATA: p = "target enter data"; break;
	      case OMP_TARGET_EXIT_DATA: p = "target exit data"; break;
	      default: gcc_unreachable ();
	      }
	    error_at (location, "too many %<if%> clauses with %qs modifier",
		      p);
	    return list;
	  }
	else if (OMP_CLAUSE_IF_MODIFIER (c) == if_modifier)
	  {
	    if (!is_omp)
	      error_at (location, "too many %<if%> clauses");
	    else
	      error_at (location, "too many %<if%> clauses without modifier");
	    return list;
	  }
	else if (if_modifier == ERROR_MARK
		 || OMP_CLAUSE_IF_MODIFIER (c) == ERROR_MARK)
	  {
	    error_at (location, "if any %<if%> clause has modifier, then all "
				"%<if%> clauses have to use modifier");
	    return list;
	  }
      }

  c = build_omp_clause (location, OMP_CLAUSE_IF);
  OMP_CLAUSE_IF_MODIFIER (c) = if_modifier;
  OMP_CLAUSE_IF_EXPR (c) = t;
  OMP_CLAUSE_CHAIN (c) = list;
  return c;
}

/* OpenMP 2.5:
   lastprivate ( variable-list )

   OpenMP 5.0:
   lastprivate ( [ lastprivate-modifier : ] variable-list ) */

static tree
c_parser_omp_clause_lastprivate (c_parser *parser, tree list)
{
  /* The clauses location.  */
  location_t loc = c_parser_peek_token (parser)->location;

  if (c_parser_require (parser, CPP_OPEN_PAREN, "expected %<(%>"))
    {
      bool conditional = false;
      if (c_parser_next_token_is (parser, CPP_NAME)
	  && c_parser_peek_2nd_token (parser)->type == CPP_COLON)
	{
	  const char *p
	    = IDENTIFIER_POINTER (c_parser_peek_token (parser)->value);
	  if (strcmp (p, "conditional") == 0)
	    {
	      conditional = true;
	      c_parser_consume_token (parser);
	      c_parser_consume_token (parser);
	    }
	}
      tree nlist = c_parser_omp_variable_list (parser, loc,
					       OMP_CLAUSE_LASTPRIVATE, list);
      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, "expected %<)%>");
      if (conditional)
	for (tree c = nlist; c != list; c = OMP_CLAUSE_CHAIN (c))
	  OMP_CLAUSE_LASTPRIVATE_CONDITIONAL (c) = 1;
      return nlist;
    }
  return list;
}

/* OpenMP 3.1:
   mergeable */

static tree
c_parser_omp_clause_mergeable (c_parser *parser ATTRIBUTE_UNUSED, tree list)
{
  tree c;

  /* FIXME: Should we allow duplicates?  */
  check_no_duplicate_clause (list, OMP_CLAUSE_MERGEABLE, "mergeable");

  c = build_omp_clause (c_parser_peek_token (parser)->location,
			OMP_CLAUSE_MERGEABLE);
  OMP_CLAUSE_CHAIN (c) = list;

  return c;
}

/* OpenMP 2.5:
   nowait */

static tree
c_parser_omp_clause_nowait (c_parser *parser ATTRIBUTE_UNUSED, tree list)
{
  tree c;
  location_t loc = c_parser_peek_token (parser)->location;

  check_no_duplicate_clause (list, OMP_CLAUSE_NOWAIT, "nowait");

  c = build_omp_clause (loc, OMP_CLAUSE_NOWAIT);
  OMP_CLAUSE_CHAIN (c) = list;
  return c;
}

/* OpenMP 2.5:
   num_threads ( expression ) */

static tree
c_parser_omp_clause_num_threads (c_parser *parser, tree list)
{
  location_t num_threads_loc = c_parser_peek_token (parser)->location;
  matching_parens parens;
  if (parens.require_open (parser))
    {
      location_t expr_loc = c_parser_peek_token (parser)->location;
      c_expr expr = c_parser_expr_no_commas (parser, NULL);
      expr = convert_lvalue_to_rvalue (expr_loc, expr, false, true);
      tree c, t = expr.value;
      t = c_fully_fold (t, false, NULL);

      parens.skip_until_found_close (parser);

      if (!INTEGRAL_TYPE_P (TREE_TYPE (t)))
	{
	  c_parser_error (parser, "expected integer expression");
	  return list;
	}

      /* Attempt to statically determine when the number isn't positive.  */
      c = fold_build2_loc (expr_loc, LE_EXPR, boolean_type_node, t,
		       build_int_cst (TREE_TYPE (t), 0));
      protected_set_expr_location (c, expr_loc);
      if (c == boolean_true_node)
	{
	  warning_at (expr_loc, 0,
		      "%<num_threads%> value must be positive");
	  t = integer_one_node;
	}

      check_no_duplicate_clause (list, OMP_CLAUSE_NUM_THREADS, "num_threads");

      c = build_omp_clause (num_threads_loc, OMP_CLAUSE_NUM_THREADS);
      OMP_CLAUSE_NUM_THREADS_EXPR (c) = t;
      OMP_CLAUSE_CHAIN (c) = list;
      list = c;
    }

  return list;
}

/* OpenMP 4.5:
   num_tasks ( expression ) */

static tree
c_parser_omp_clause_num_tasks (c_parser *parser, tree list)
{
  location_t num_tasks_loc = c_parser_peek_token (parser)->location;
  matching_parens parens;
  if (parens.require_open (parser))
    {
      location_t expr_loc = c_parser_peek_token (parser)->location;
      c_expr expr = c_parser_expr_no_commas (parser, NULL);
      expr = convert_lvalue_to_rvalue (expr_loc, expr, false, true);
      tree c, t = expr.value;
      t = c_fully_fold (t, false, NULL);

      parens.skip_until_found_close (parser);

      if (!INTEGRAL_TYPE_P (TREE_TYPE (t)))
	{
	  c_parser_error (parser, "expected integer expression");
	  return list;
	}

      /* Attempt to statically determine when the number isn't positive.  */
      c = fold_build2_loc (expr_loc, LE_EXPR, boolean_type_node, t,
			   build_int_cst (TREE_TYPE (t), 0));
      if (CAN_HAVE_LOCATION_P (c))
	SET_EXPR_LOCATION (c, expr_loc);
      if (c == boolean_true_node)
	{
	  warning_at (expr_loc, 0, "%<num_tasks%> value must be positive");
	  t = integer_one_node;
	}

      check_no_duplicate_clause (list, OMP_CLAUSE_NUM_TASKS, "num_tasks");

      c = build_omp_clause (num_tasks_loc, OMP_CLAUSE_NUM_TASKS);
      OMP_CLAUSE_NUM_TASKS_EXPR (c) = t;
      OMP_CLAUSE_CHAIN (c) = list;
      list = c;
    }

  return list;
}

/* OpenMP 4.5:
   grainsize ( expression ) */

static tree
c_parser_omp_clause_grainsize (c_parser *parser, tree list)
{
  location_t grainsize_loc = c_parser_peek_token (parser)->location;
  matching_parens parens;
  if (parens.require_open (parser))
    {
      location_t expr_loc = c_parser_peek_token (parser)->location;
      c_expr expr = c_parser_expr_no_commas (parser, NULL);
      expr = convert_lvalue_to_rvalue (expr_loc, expr, false, true);
      tree c, t = expr.value;
      t = c_fully_fold (t, false, NULL);

      parens.skip_until_found_close (parser);

      if (!INTEGRAL_TYPE_P (TREE_TYPE (t)))
	{
	  c_parser_error (parser, "expected integer expression");
	  return list;
	}

      /* Attempt to statically determine when the number isn't positive.  */
      c = fold_build2_loc (expr_loc, LE_EXPR, boolean_type_node, t,
			   build_int_cst (TREE_TYPE (t), 0));
      if (CAN_HAVE_LOCATION_P (c))
	SET_EXPR_LOCATION (c, expr_loc);
      if (c == boolean_true_node)
	{
	  warning_at (expr_loc, 0, "%<grainsize%> value must be positive");
	  t = integer_one_node;
	}

      check_no_duplicate_clause (list, OMP_CLAUSE_GRAINSIZE, "grainsize");

      c = build_omp_clause (grainsize_loc, OMP_CLAUSE_GRAINSIZE);
      OMP_CLAUSE_GRAINSIZE_EXPR (c) = t;
      OMP_CLAUSE_CHAIN (c) = list;
      list = c;
    }

  return list;
}

/* OpenMP 4.5:
   priority ( expression ) */

static tree
c_parser_omp_clause_priority (c_parser *parser, tree list)
{
  location_t priority_loc = c_parser_peek_token (parser)->location;
  matching_parens parens;
  if (parens.require_open (parser))
    {
      location_t expr_loc = c_parser_peek_token (parser)->location;
      c_expr expr = c_parser_expr_no_commas (parser, NULL);
      expr = convert_lvalue_to_rvalue (expr_loc, expr, false, true);
      tree c, t = expr.value;
      t = c_fully_fold (t, false, NULL);

      parens.skip_until_found_close (parser);

      if (!INTEGRAL_TYPE_P (TREE_TYPE (t)))
	{
	  c_parser_error (parser, "expected integer expression");
	  return list;
	}

      /* Attempt to statically determine when the number isn't
	 non-negative.  */
      c = fold_build2_loc (expr_loc, LT_EXPR, boolean_type_node, t,
			   build_int_cst (TREE_TYPE (t), 0));
      if (CAN_HAVE_LOCATION_P (c))
	SET_EXPR_LOCATION (c, expr_loc);
      if (c == boolean_true_node)
	{
	  warning_at (expr_loc, 0, "%<priority%> value must be non-negative");
	  t = integer_one_node;
	}

      check_no_duplicate_clause (list, OMP_CLAUSE_PRIORITY, "priority");

      c = build_omp_clause (priority_loc, OMP_CLAUSE_PRIORITY);
      OMP_CLAUSE_PRIORITY_EXPR (c) = t;
      OMP_CLAUSE_CHAIN (c) = list;
      list = c;
    }

  return list;
}

/* OpenMP 4.5:
   hint ( expression ) */

static tree
c_parser_omp_clause_hint (c_parser *parser, tree list)
{
  location_t hint_loc = c_parser_peek_token (parser)->location;
  matching_parens parens;
  if (parens.require_open (parser))
    {
      location_t expr_loc = c_parser_peek_token (parser)->location;
      c_expr expr = c_parser_expr_no_commas (parser, NULL);
      expr = convert_lvalue_to_rvalue (expr_loc, expr, false, true);
      tree c, t = expr.value;
      t = c_fully_fold (t, false, NULL);
      if (!INTEGRAL_TYPE_P (TREE_TYPE (t))
	  || TREE_CODE (t) != INTEGER_CST
	  || tree_int_cst_sgn (t) == -1)
	{
	  c_parser_error (parser, "expected constant integer expression "
				  "with valid sync-hint value");
	  return list;
	}
      parens.skip_until_found_close (parser);
      check_no_duplicate_clause (list, OMP_CLAUSE_HINT, "hint");

      c = build_omp_clause (hint_loc, OMP_CLAUSE_HINT);
      OMP_CLAUSE_HINT_EXPR (c) = t;
      OMP_CLAUSE_CHAIN (c) = list;
      list = c;
    }

  return list;
}

/* OpenMP 4.5:
   defaultmap ( tofrom : scalar )

   OpenMP 5.0:
   defaultmap ( implicit-behavior [ : variable-category ] ) */

static tree
c_parser_omp_clause_defaultmap (c_parser *parser, tree list)
{
  location_t loc = c_parser_peek_token (parser)->location;
  tree c;
  const char *p;
  enum omp_clause_defaultmap_kind behavior = OMP_CLAUSE_DEFAULTMAP_DEFAULT;
  enum omp_clause_defaultmap_kind category
    = OMP_CLAUSE_DEFAULTMAP_CATEGORY_UNSPECIFIED;

  matching_parens parens;
  if (!parens.require_open (parser))
    return list;
  if (c_parser_next_token_is_keyword (parser, RID_DEFAULT))
    p = "default";
  else if (!c_parser_next_token_is (parser, CPP_NAME))
    {
    invalid_behavior:
      c_parser_error (parser, "expected %<alloc%>, %<to%>, %<from%>, "
			      "%<tofrom%>, %<firstprivate%>, %<none%> "
			      "or %<default%>");
      goto out_err;
    }
  else
    p = IDENTIFIER_POINTER (c_parser_peek_token (parser)->value);

  switch (p[0])
    {
    case 'a':
      if (strcmp ("alloc", p) == 0)
	behavior = OMP_CLAUSE_DEFAULTMAP_ALLOC;
      else
	goto invalid_behavior;
      break;

    case 'd':
      if (strcmp ("default", p) == 0)
	behavior = OMP_CLAUSE_DEFAULTMAP_DEFAULT;
      else
	goto invalid_behavior;
      break;

    case 'f':
      if (strcmp ("firstprivate", p) == 0)
	behavior = OMP_CLAUSE_DEFAULTMAP_FIRSTPRIVATE;
      else if (strcmp ("from", p) == 0)
	behavior = OMP_CLAUSE_DEFAULTMAP_FROM;
      else
	goto invalid_behavior;
      break;

    case 'n':
      if (strcmp ("none", p) == 0)
	behavior = OMP_CLAUSE_DEFAULTMAP_NONE;
      else
	goto invalid_behavior;
      break;

    case 't':
      if (strcmp ("tofrom", p) == 0)
	behavior = OMP_CLAUSE_DEFAULTMAP_TOFROM;
      else if (strcmp ("to", p) == 0)
	behavior = OMP_CLAUSE_DEFAULTMAP_TO;
      else
	goto invalid_behavior;
      break;

    default:
      goto invalid_behavior;
    }
  c_parser_consume_token (parser);

  if (!c_parser_next_token_is (parser, CPP_CLOSE_PAREN))
    {
      if (!c_parser_require (parser, CPP_COLON, "expected %<:%>"))
	goto out_err;
      if (!c_parser_next_token_is (parser, CPP_NAME))
	{
	invalid_category:
	  c_parser_error (parser, "expected %<scalar%>, %<aggregate%> or "
				  "%<pointer%>");
	  goto out_err;
	}
      p = IDENTIFIER_POINTER (c_parser_peek_token (parser)->value);
      switch (p[0])
	{
	case 'a':
	  if (strcmp ("aggregate", p) == 0)
	    category = OMP_CLAUSE_DEFAULTMAP_CATEGORY_AGGREGATE;
	  else
	    goto invalid_category;
	  break;

	case 'p':
	  if (strcmp ("pointer", p) == 0)
	    category = OMP_CLAUSE_DEFAULTMAP_CATEGORY_POINTER;
	  else
	    goto invalid_category;
	  break;

	case 's':
	  if (strcmp ("scalar", p) == 0)
	    category = OMP_CLAUSE_DEFAULTMAP_CATEGORY_SCALAR;
	  else
	    goto invalid_category;
	  break;

	default:
	  goto invalid_category;
	}

      c_parser_consume_token (parser);
    }
  parens.skip_until_found_close (parser);

  for (c = list; c ; c = OMP_CLAUSE_CHAIN (c))
    if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_DEFAULTMAP
	&& (category == OMP_CLAUSE_DEFAULTMAP_CATEGORY_UNSPECIFIED
	    || OMP_CLAUSE_DEFAULTMAP_CATEGORY (c) == category
	    || (OMP_CLAUSE_DEFAULTMAP_CATEGORY (c)
		== OMP_CLAUSE_DEFAULTMAP_CATEGORY_UNSPECIFIED)))
      {
	enum omp_clause_defaultmap_kind cat = category;
	location_t loc = OMP_CLAUSE_LOCATION (c);
	if (cat == OMP_CLAUSE_DEFAULTMAP_CATEGORY_UNSPECIFIED)
	  cat = OMP_CLAUSE_DEFAULTMAP_CATEGORY (c);
	p = NULL;
	switch (cat)
	  {
	  case OMP_CLAUSE_DEFAULTMAP_CATEGORY_UNSPECIFIED:
	    p = NULL;
	    break;
	  case OMP_CLAUSE_DEFAULTMAP_CATEGORY_AGGREGATE:
	    p = "aggregate";
	    break;
	  case OMP_CLAUSE_DEFAULTMAP_CATEGORY_POINTER:
	    p = "pointer";
	    break;
	  case OMP_CLAUSE_DEFAULTMAP_CATEGORY_SCALAR:
	    p = "scalar";
	    break;
	  default:
	    gcc_unreachable ();
	  }
	if (p)
	  error_at (loc, "too many %<defaultmap%> clauses with %qs category",
		    p);
	else
	  error_at (loc, "too many %<defaultmap%> clauses with unspecified "
			 "category");
	break;
      }

  c = build_omp_clause (loc, OMP_CLAUSE_DEFAULTMAP);
  OMP_CLAUSE_DEFAULTMAP_SET_KIND (c, behavior, category);
  OMP_CLAUSE_CHAIN (c) = list;
  return c;

 out_err:
  parens.skip_until_found_close (parser);
  return list;
}

/* OpenACC 2.0:
   use_device ( variable-list )

   OpenMP 4.5:
   use_device_ptr ( variable-list ) */

static tree
c_parser_omp_clause_use_device_ptr (c_parser *parser, tree list)
{
  return c_parser_omp_var_list_parens (parser, OMP_CLAUSE_USE_DEVICE_PTR,
				       list);
}

/* OpenMP 5.0:
   use_device_addr ( variable-list ) */

static tree
c_parser_omp_clause_use_device_addr (c_parser *parser, tree list)
{
  return c_parser_omp_var_list_parens (parser, OMP_CLAUSE_USE_DEVICE_ADDR,
				       list);
}

/* OpenMP 4.5:
   is_device_ptr ( variable-list ) */

static tree
c_parser_omp_clause_is_device_ptr (c_parser *parser, tree list)
{
  return c_parser_omp_var_list_parens (parser, OMP_CLAUSE_IS_DEVICE_PTR, list);
}

/* OpenACC:
   num_gangs ( expression )
   num_workers ( expression )
   vector_length ( expression )  */

static tree
c_parser_oacc_single_int_clause (c_parser *parser, omp_clause_code code,
				 tree list)
{
  location_t loc = c_parser_peek_token (parser)->location;

  matching_parens parens;
  if (!parens.require_open (parser))
    return list;

  location_t expr_loc = c_parser_peek_token (parser)->location;
  c_expr expr = c_parser_expression (parser);
  expr = convert_lvalue_to_rvalue (expr_loc, expr, false, true);
  tree c, t = expr.value;
  t = c_fully_fold (t, false, NULL);

  parens.skip_until_found_close (parser);

  if (t == error_mark_node)
    return list;
  else if (!INTEGRAL_TYPE_P (TREE_TYPE (t)))
    {
      error_at (expr_loc, "%qs expression must be integral",
		omp_clause_code_name[code]);
      return list;
    }

  /* Attempt to statically determine when the number isn't positive.  */
  c = fold_build2_loc (expr_loc, LE_EXPR, boolean_type_node, t,
		       build_int_cst (TREE_TYPE (t), 0));
  protected_set_expr_location (c, expr_loc);
  if (c == boolean_true_node)
    {
      warning_at (expr_loc, 0,
		  "%qs value must be positive",
		  omp_clause_code_name[code]);
      t = integer_one_node;
    }

  check_no_duplicate_clause (list, code, omp_clause_code_name[code]);

  c = build_omp_clause (loc, code);
  OMP_CLAUSE_OPERAND (c, 0) = t;
  OMP_CLAUSE_CHAIN (c) = list;
  return c;
}

/* OpenACC:

    gang [( gang-arg-list )]
    worker [( [num:] int-expr )]
    vector [( [length:] int-expr )]

  where gang-arg is one of:

    [num:] int-expr
    static: size-expr

  and size-expr may be:

    *
    int-expr
*/

static tree
c_parser_oacc_shape_clause (c_parser *parser, location_t loc,
			    omp_clause_code kind,
			    const char *str, tree list)
{
  const char *id = "num";
  tree ops[2] = { NULL_TREE, NULL_TREE }, c;

  if (kind == OMP_CLAUSE_VECTOR)
    id = "length";

  if (c_parser_next_token_is (parser, CPP_OPEN_PAREN))
    {
      c_parser_consume_token (parser);

      do
	{
	  c_token *next = c_parser_peek_token (parser);
	  int idx = 0;

	  /* Gang static argument.  */
	  if (kind == OMP_CLAUSE_GANG
	      && c_parser_next_token_is_keyword (parser, RID_STATIC))
	    {
	      c_parser_consume_token (parser);

	      if (!c_parser_require (parser, CPP_COLON, "expected %<:%>"))
		goto cleanup_error;

	      idx = 1;
	      if (ops[idx] != NULL_TREE)
		{
		  c_parser_error (parser, "too many %<static%> arguments");
		  goto cleanup_error;
		}

	      /* Check for the '*' argument.  */
	      if (c_parser_next_token_is (parser, CPP_MULT)
		  && (c_parser_peek_2nd_token (parser)->type == CPP_COMMA
		      || c_parser_peek_2nd_token (parser)->type
		         == CPP_CLOSE_PAREN))
		{
		  c_parser_consume_token (parser);
		  ops[idx] = integer_minus_one_node;

		  if (c_parser_next_token_is (parser, CPP_COMMA))
		    {
		      c_parser_consume_token (parser);
		      continue;
		    }
		  else
		    break;
		}
	    }
	  /* Worker num: argument and vector length: arguments.  */
	  else if (c_parser_next_token_is (parser, CPP_NAME)
		   && strcmp (id, IDENTIFIER_POINTER (next->value)) == 0
		   && c_parser_peek_2nd_token (parser)->type == CPP_COLON)
	    {
	      c_parser_consume_token (parser);  /* id  */
	      c_parser_consume_token (parser);  /* ':'  */
	    }

	  /* Now collect the actual argument.  */
	  if (ops[idx] != NULL_TREE)
	    {
	      c_parser_error (parser, "unexpected argument");
	      goto cleanup_error;
	    }

	  location_t expr_loc = c_parser_peek_token (parser)->location;
	  c_expr cexpr = c_parser_expr_no_commas (parser, NULL);
	  cexpr = convert_lvalue_to_rvalue (expr_loc, cexpr, false, true);
	  tree expr = cexpr.value;
	  if (expr == error_mark_node)
	    goto cleanup_error;

	  expr = c_fully_fold (expr, false, NULL);

	  /* Attempt to statically determine when the number isn't a
	     positive integer.  */

	  if (!INTEGRAL_TYPE_P (TREE_TYPE (expr)))
	    {
	      c_parser_error (parser, "expected integer expression");
	      return list;
	    }

	  tree c = fold_build2_loc (expr_loc, LE_EXPR, boolean_type_node, expr,
				    build_int_cst (TREE_TYPE (expr), 0));
	  if (c == boolean_true_node)
	    {
	      warning_at (loc, 0,
			  "%qs value must be positive", str);
	      expr = integer_one_node;
	    }

	  ops[idx] = expr;

	  if (kind == OMP_CLAUSE_GANG
	      && c_parser_next_token_is (parser, CPP_COMMA))
	    {
	      c_parser_consume_token (parser);
	      continue;
	    }
	  break;
	}
      while (1);

      if (!c_parser_require (parser, CPP_CLOSE_PAREN, "expected %<)%>"))
	goto cleanup_error;
    }

  check_no_duplicate_clause (list, kind, str);

  c = build_omp_clause (loc, kind);

  if (ops[1])
    OMP_CLAUSE_OPERAND (c, 1) = ops[1];

  OMP_CLAUSE_OPERAND (c, 0) = ops[0];
  OMP_CLAUSE_CHAIN (c) = list;

  return c;

 cleanup_error:
  c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, 0);
  return list;
}

/* OpenACC 2.5:
   auto
   finalize
   independent
   nohost
   seq */

static tree
c_parser_oacc_simple_clause (location_t loc, enum omp_clause_code code,
			     tree list)
{
  check_no_duplicate_clause (list, code, omp_clause_code_name[code]);

  tree c = build_omp_clause (loc, code);
  OMP_CLAUSE_CHAIN (c) = list;

  return c;
}

/* OpenACC:
   async [( int-expr )] */

static tree
c_parser_oacc_clause_async (c_parser *parser, tree list)
{
  tree c, t;
  location_t loc = c_parser_peek_token (parser)->location;

  t = build_int_cst (integer_type_node, GOMP_ASYNC_NOVAL);

  if (c_parser_peek_token (parser)->type == CPP_OPEN_PAREN)
    {
      c_parser_consume_token (parser);

      t = c_parser_expr_no_commas (parser, NULL).value;
      if (!INTEGRAL_TYPE_P (TREE_TYPE (t)))
	c_parser_error (parser, "expected integer expression");
      else if (t == error_mark_node
	  || !c_parser_require (parser, CPP_CLOSE_PAREN, "expected %<)%>"))
	return list;
    }
  else
    t = c_fully_fold (t, false, NULL);

  check_no_duplicate_clause (list, OMP_CLAUSE_ASYNC, "async");

  c = build_omp_clause (loc, OMP_CLAUSE_ASYNC);
  OMP_CLAUSE_ASYNC_EXPR (c) = t;
  OMP_CLAUSE_CHAIN (c) = list;
  list = c;

  return list;
}

/* OpenACC 2.0:
   tile ( size-expr-list ) */

static tree
c_parser_oacc_clause_tile (c_parser *parser, tree list)
{
  tree c, expr = error_mark_node;
  location_t loc;
  tree tile = NULL_TREE;

  check_no_duplicate_clause (list, OMP_CLAUSE_TILE, "tile");
  check_no_duplicate_clause (list, OMP_CLAUSE_COLLAPSE, "collapse");

  loc = c_parser_peek_token (parser)->location;
  if (!c_parser_require (parser, CPP_OPEN_PAREN, "expected %<(%>"))
    return list;

  do
    {
      if (tile && !c_parser_require (parser, CPP_COMMA, "expected %<,%>"))
	return list;

      if (c_parser_next_token_is (parser, CPP_MULT)
	  && (c_parser_peek_2nd_token (parser)->type == CPP_COMMA
	      || c_parser_peek_2nd_token (parser)->type == CPP_CLOSE_PAREN))
	{
	  c_parser_consume_token (parser);
	  expr = integer_zero_node;
	}
      else
	{
	  location_t expr_loc = c_parser_peek_token (parser)->location;
	  c_expr cexpr = c_parser_expr_no_commas (parser, NULL);
	  cexpr = convert_lvalue_to_rvalue (expr_loc, cexpr, false, true);
	  expr = cexpr.value;

	  if (expr == error_mark_node)
	    {
	      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
					 "expected %<)%>");
	      return list;
	    }

	  expr = c_fully_fold (expr, false, NULL);

	  if (!INTEGRAL_TYPE_P (TREE_TYPE (expr))
	      || !tree_fits_shwi_p (expr)
	      || tree_to_shwi (expr) <= 0)
	    {
	      error_at (expr_loc, "%<tile%> argument needs positive"
			" integral constant");
	      expr = integer_zero_node;
	    }
	}

      tile = tree_cons (NULL_TREE, expr, tile);
    }
  while (c_parser_next_token_is_not (parser, CPP_CLOSE_PAREN));

  /* Consume the trailing ')'.  */
  c_parser_consume_token (parser);

  c = build_omp_clause (loc, OMP_CLAUSE_TILE);
  tile = nreverse (tile);
  OMP_CLAUSE_TILE_LIST (c) = tile;
  OMP_CLAUSE_CHAIN (c) = list;
  return c;
}

/* OpenACC:
   wait [( int-expr-list )] */

static tree
c_parser_oacc_clause_wait (c_parser *parser, tree list)
{
  location_t clause_loc = c_parser_peek_token (parser)->location;

  if (c_parser_peek_token (parser)->type == CPP_OPEN_PAREN)
    list = c_parser_oacc_wait_list (parser, clause_loc, list);
  else
    {
      tree c = build_omp_clause (clause_loc, OMP_CLAUSE_WAIT);

      OMP_CLAUSE_DECL (c) = build_int_cst (integer_type_node, GOMP_ASYNC_NOVAL);
      OMP_CLAUSE_CHAIN (c) = list;
      list = c;
    }

  return list;
}


/* OpenMP 5.0:
   order ( concurrent ) */

static tree
c_parser_omp_clause_order (c_parser *parser, tree list)
{
  location_t loc = c_parser_peek_token (parser)->location;
  tree c;
  const char *p;

  matching_parens parens;
  if (!parens.require_open (parser))
    return list;
  if (!c_parser_next_token_is (parser, CPP_NAME))
    {
      c_parser_error (parser, "expected %<concurrent%>");
      goto out_err;
    }
  p = IDENTIFIER_POINTER (c_parser_peek_token (parser)->value);
  if (strcmp (p, "concurrent") != 0)
    {
      c_parser_error (parser, "expected %<concurrent%>");
      goto out_err;
    }
  c_parser_consume_token (parser);
  parens.skip_until_found_close (parser);
  /* check_no_duplicate_clause (list, OMP_CLAUSE_ORDER, "order"); */
  c = build_omp_clause (loc, OMP_CLAUSE_ORDER);
  OMP_CLAUSE_CHAIN (c) = list;
  return c;

 out_err:
  parens.skip_until_found_close (parser);
  return list;
}


/* OpenMP 5.0:
   bind ( teams | parallel | thread ) */

static tree
c_parser_omp_clause_bind (c_parser *parser, tree list)
{
  location_t loc = c_parser_peek_token (parser)->location;
  tree c;
  const char *p;
  enum omp_clause_bind_kind kind = OMP_CLAUSE_BIND_THREAD;

  matching_parens parens;
  if (!parens.require_open (parser))
    return list;
  if (!c_parser_next_token_is (parser, CPP_NAME))
    {
     invalid:
      c_parser_error (parser,
		      "expected %<teams%>, %<parallel%> or %<thread%>");
      parens.skip_until_found_close (parser);
      return list;
    }
  p = IDENTIFIER_POINTER (c_parser_peek_token (parser)->value);
  if (strcmp (p, "teams") == 0)
    kind = OMP_CLAUSE_BIND_TEAMS;
  else if (strcmp (p, "parallel") == 0)
    kind = OMP_CLAUSE_BIND_PARALLEL;
  else if (strcmp (p, "thread") != 0)
    goto invalid;
  c_parser_consume_token (parser);
  parens.skip_until_found_close (parser);
  /* check_no_duplicate_clause (list, OMP_CLAUSE_BIND, "bind"); */
  c = build_omp_clause (loc, OMP_CLAUSE_BIND);
  OMP_CLAUSE_BIND_KIND (c) = kind;
  OMP_CLAUSE_CHAIN (c) = list;
  return c;
}


/* OpenMP 2.5:
   ordered

   OpenMP 4.5:
   ordered ( constant-expression ) */

static tree
c_parser_omp_clause_ordered (c_parser *parser, tree list)
{
  check_no_duplicate_clause (list, OMP_CLAUSE_ORDERED, "ordered");

  tree c, num = NULL_TREE;
  HOST_WIDE_INT n;
  location_t loc = c_parser_peek_token (parser)->location;
  if (c_parser_next_token_is (parser, CPP_OPEN_PAREN))
    {
      matching_parens parens;
      parens.consume_open (parser);
      num = c_parser_expr_no_commas (parser, NULL).value;
      parens.skip_until_found_close (parser);
    }
  if (num == error_mark_node)
    return list;
  if (num)
    {
      mark_exp_read (num);
      num = c_fully_fold (num, false, NULL);
      if (!INTEGRAL_TYPE_P (TREE_TYPE (num))
	  || !tree_fits_shwi_p (num)
	  || (n = tree_to_shwi (num)) <= 0
	  || (int) n != n)
	{
	  error_at (loc, "ordered argument needs positive "
			 "constant integer expression");
	  return list;
	}
    }
  c = build_omp_clause (loc, OMP_CLAUSE_ORDERED);
  OMP_CLAUSE_ORDERED_EXPR (c) = num;
  OMP_CLAUSE_CHAIN (c) = list;
  return c;
}

/* OpenMP 2.5:
   private ( variable-list ) */

static tree
c_parser_omp_clause_private (c_parser *parser, tree list)
{
  return c_parser_omp_var_list_parens (parser, OMP_CLAUSE_PRIVATE, list);
}

/* OpenMP 2.5:
   reduction ( reduction-operator : variable-list )

   reduction-operator:
     One of: + * - & ^ | && ||

   OpenMP 3.1:
   
   reduction-operator:
     One of: + * - & ^ | && || max min

   OpenMP 4.0:

   reduction-operator:
     One of: + * - & ^ | && ||
     identifier

   OpenMP 5.0:
   reduction ( reduction-modifier, reduction-operator : variable-list )
   in_reduction ( reduction-operator : variable-list )
   task_reduction ( reduction-operator : variable-list )  */

static tree
c_parser_omp_clause_reduction (c_parser *parser, enum omp_clause_code kind,
			       bool is_omp, tree list)
{
  location_t clause_loc = c_parser_peek_token (parser)->location;
  matching_parens parens;
  if (parens.require_open (parser))
    {
      bool task = false;
      bool inscan = false;
      enum tree_code code = ERROR_MARK;
      tree reduc_id = NULL_TREE;

      if (kind == OMP_CLAUSE_REDUCTION && is_omp)
	{
	  if (c_parser_next_token_is_keyword (parser, RID_DEFAULT)
	      && c_parser_peek_2nd_token (parser)->type == CPP_COMMA)
	    {
	      c_parser_consume_token (parser);
	      c_parser_consume_token (parser);
	    }
	  else if (c_parser_next_token_is (parser, CPP_NAME)
		   && c_parser_peek_2nd_token (parser)->type == CPP_COMMA)
	    {
	      const char *p
		= IDENTIFIER_POINTER (c_parser_peek_token (parser)->value);
	      if (strcmp (p, "task") == 0)
		task = true;
	      else if (strcmp (p, "inscan") == 0)
		inscan = true;
	      if (task || inscan)
		{
		  c_parser_consume_token (parser);
		  c_parser_consume_token (parser);
		}
	    }
	}

      switch (c_parser_peek_token (parser)->type)
	{
	case CPP_PLUS:
	  code = PLUS_EXPR;
	  break;
	case CPP_MULT:
	  code = MULT_EXPR;
	  break;
	case CPP_MINUS:
	  code = MINUS_EXPR;
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
	  code = TRUTH_ANDIF_EXPR;
	  break;
	case CPP_OR_OR:
	  code = TRUTH_ORIF_EXPR;
	  break;
        case CPP_NAME:
	  {
	    const char *p
	      = IDENTIFIER_POINTER (c_parser_peek_token (parser)->value);
	    if (strcmp (p, "min") == 0)
	      {
		code = MIN_EXPR;
		break;
	      }
	    if (strcmp (p, "max") == 0)
	      {
		code = MAX_EXPR;
		break;
	      }
	    reduc_id = c_parser_peek_token (parser)->value;
	    break;
	  }
	default:
	  c_parser_error (parser,
			  "expected %<+%>, %<*%>, %<-%>, %<&%>, "
			  "%<^%>, %<|%>, %<&&%>, %<||%> or identifier");
	  c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, 0);
	  return list;
	}
      c_parser_consume_token (parser);
      reduc_id = c_omp_reduction_id (code, reduc_id);
      if (c_parser_require (parser, CPP_COLON, "expected %<:%>"))
	{
	  tree nl, c;

	  nl = c_parser_omp_variable_list (parser, clause_loc, kind, list);
	  for (c = nl; c != list; c = OMP_CLAUSE_CHAIN (c))
	    {
	      tree d = OMP_CLAUSE_DECL (c), type;
	      if (TREE_CODE (d) != TREE_LIST)
		type = TREE_TYPE (d);
	      else
		{
		  int cnt = 0;
		  tree t;
		  for (t = d; TREE_CODE (t) == TREE_LIST; t = TREE_CHAIN (t))
		    cnt++;
		  type = TREE_TYPE (t);
		  while (cnt > 0)
		    {
		      if (TREE_CODE (type) != POINTER_TYPE
			  && TREE_CODE (type) != ARRAY_TYPE)
			break;
		      type = TREE_TYPE (type);
		      cnt--;
		    }
		}
	      while (TREE_CODE (type) == ARRAY_TYPE)
		type = TREE_TYPE (type);
	      OMP_CLAUSE_REDUCTION_CODE (c) = code;
	      if (task)
		OMP_CLAUSE_REDUCTION_TASK (c) = 1;
	      else if (inscan)
		OMP_CLAUSE_REDUCTION_INSCAN (c) = 1;
	      if (code == ERROR_MARK
		  || !(INTEGRAL_TYPE_P (type)
		       || TREE_CODE (type) == REAL_TYPE
		       || TREE_CODE (type) == COMPLEX_TYPE))
		OMP_CLAUSE_REDUCTION_PLACEHOLDER (c)
		  = c_omp_reduction_lookup (reduc_id,
					    TYPE_MAIN_VARIANT (type));
	    }

	  list = nl;
	}
      parens.skip_until_found_close (parser);
    }
  return list;
}

/* OpenMP 2.5:
   schedule ( schedule-kind )
   schedule ( schedule-kind , expression )

   schedule-kind:
     static | dynamic | guided | runtime | auto

   OpenMP 4.5:
   schedule ( schedule-modifier : schedule-kind )
   schedule ( schedule-modifier [ , schedule-modifier ] : schedule-kind , expression )

   schedule-modifier:
     simd
     monotonic
     nonmonotonic  */

static tree
c_parser_omp_clause_schedule (c_parser *parser, tree list)
{
  tree c, t;
  location_t loc = c_parser_peek_token (parser)->location;
  int modifiers = 0, nmodifiers = 0;

  matching_parens parens;
  if (!parens.require_open (parser))
    return list;

  c = build_omp_clause (loc, OMP_CLAUSE_SCHEDULE);

  location_t comma = UNKNOWN_LOCATION;
  while (c_parser_next_token_is (parser, CPP_NAME))
    {
      tree kind = c_parser_peek_token (parser)->value;
      const char *p = IDENTIFIER_POINTER (kind);
      if (strcmp ("simd", p) == 0)
	OMP_CLAUSE_SCHEDULE_SIMD (c) = 1;
      else if (strcmp ("monotonic", p) == 0)
	modifiers |= OMP_CLAUSE_SCHEDULE_MONOTONIC;
      else if (strcmp ("nonmonotonic", p) == 0)
	modifiers |= OMP_CLAUSE_SCHEDULE_NONMONOTONIC;
      else
	break;
      comma = UNKNOWN_LOCATION;
      c_parser_consume_token (parser);
      if (nmodifiers++ == 0
	  && c_parser_next_token_is (parser, CPP_COMMA))
	{
	  comma = c_parser_peek_token (parser)->location;
	  c_parser_consume_token (parser);
	}
      else
	{
	  c_parser_require (parser, CPP_COLON, "expected %<:%>");
	  break;
	}
    }
  if (comma != UNKNOWN_LOCATION)
    error_at (comma, "expected %<:%>");

  if ((modifiers & (OMP_CLAUSE_SCHEDULE_MONOTONIC
		    | OMP_CLAUSE_SCHEDULE_NONMONOTONIC))
      == (OMP_CLAUSE_SCHEDULE_MONOTONIC
	  | OMP_CLAUSE_SCHEDULE_NONMONOTONIC))
    {
      error_at (loc, "both %<monotonic%> and %<nonmonotonic%> modifiers "
		     "specified");
      modifiers = 0;
    }

  if (c_parser_next_token_is (parser, CPP_NAME))
    {
      tree kind = c_parser_peek_token (parser)->value;
      const char *p = IDENTIFIER_POINTER (kind);

      switch (p[0])
	{
	case 'd':
	  if (strcmp ("dynamic", p) != 0)
	    goto invalid_kind;
	  OMP_CLAUSE_SCHEDULE_KIND (c) = OMP_CLAUSE_SCHEDULE_DYNAMIC;
	  break;

        case 'g':
	  if (strcmp ("guided", p) != 0)
	    goto invalid_kind;
	  OMP_CLAUSE_SCHEDULE_KIND (c) = OMP_CLAUSE_SCHEDULE_GUIDED;
	  break;

	case 'r':
	  if (strcmp ("runtime", p) != 0)
	    goto invalid_kind;
	  OMP_CLAUSE_SCHEDULE_KIND (c) = OMP_CLAUSE_SCHEDULE_RUNTIME;
	  break;

	default:
	  goto invalid_kind;
	}
    }
  else if (c_parser_next_token_is_keyword (parser, RID_STATIC))
    OMP_CLAUSE_SCHEDULE_KIND (c) = OMP_CLAUSE_SCHEDULE_STATIC;
  else if (c_parser_next_token_is_keyword (parser, RID_AUTO))
    OMP_CLAUSE_SCHEDULE_KIND (c) = OMP_CLAUSE_SCHEDULE_AUTO;
  else
    goto invalid_kind;

  c_parser_consume_token (parser);
  if (c_parser_next_token_is (parser, CPP_COMMA))
    {
      location_t here;
      c_parser_consume_token (parser);

      here = c_parser_peek_token (parser)->location;
      c_expr expr = c_parser_expr_no_commas (parser, NULL);
      expr = convert_lvalue_to_rvalue (here, expr, false, true);
      t = expr.value;
      t = c_fully_fold (t, false, NULL);

      if (OMP_CLAUSE_SCHEDULE_KIND (c) == OMP_CLAUSE_SCHEDULE_RUNTIME)
	error_at (here, "schedule %<runtime%> does not take "
		  "a %<chunk_size%> parameter");
      else if (OMP_CLAUSE_SCHEDULE_KIND (c) == OMP_CLAUSE_SCHEDULE_AUTO)
	error_at (here,
		  "schedule %<auto%> does not take "
		  "a %<chunk_size%> parameter");
      else if (TREE_CODE (TREE_TYPE (t)) == INTEGER_TYPE)
	{
	  /* Attempt to statically determine when the number isn't
	     positive.  */
	  tree s = fold_build2_loc (loc, LE_EXPR, boolean_type_node, t,
				    build_int_cst (TREE_TYPE (t), 0));
	  protected_set_expr_location (s, loc);
	  if (s == boolean_true_node)
	    {
	      warning_at (loc, 0,
			  "chunk size value must be positive");
	      t = integer_one_node;
	    }
	  OMP_CLAUSE_SCHEDULE_CHUNK_EXPR (c) = t;
	}
      else
	c_parser_error (parser, "expected integer expression");

      parens.skip_until_found_close (parser);
    }
  else
    c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
			       "expected %<,%> or %<)%>");

  OMP_CLAUSE_SCHEDULE_KIND (c)
    = (enum omp_clause_schedule_kind)
      (OMP_CLAUSE_SCHEDULE_KIND (c) | modifiers);

  check_no_duplicate_clause (list, OMP_CLAUSE_SCHEDULE, "schedule");
  OMP_CLAUSE_CHAIN (c) = list;
  return c;

 invalid_kind:
  c_parser_error (parser, "invalid schedule kind");
  c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, 0);
  return list;
}

/* OpenMP 2.5:
   shared ( variable-list ) */

static tree
c_parser_omp_clause_shared (c_parser *parser, tree list)
{
  return c_parser_omp_var_list_parens (parser, OMP_CLAUSE_SHARED, list);
}

/* OpenMP 3.0:
   untied */

static tree
c_parser_omp_clause_untied (c_parser *parser ATTRIBUTE_UNUSED, tree list)
{
  tree c;

  /* FIXME: Should we allow duplicates?  */
  check_no_duplicate_clause (list, OMP_CLAUSE_UNTIED, "untied");

  c = build_omp_clause (c_parser_peek_token (parser)->location,
			OMP_CLAUSE_UNTIED);
  OMP_CLAUSE_CHAIN (c) = list;

  return c;
}

/* OpenMP 4.0:
   inbranch
   notinbranch */

static tree
c_parser_omp_clause_branch (c_parser *parser ATTRIBUTE_UNUSED,
			    enum omp_clause_code code, tree list)
{
  check_no_duplicate_clause (list, code, omp_clause_code_name[code]);

  tree c = build_omp_clause (c_parser_peek_token (parser)->location, code);
  OMP_CLAUSE_CHAIN (c) = list;

  return c;
}

/* OpenMP 4.0:
   parallel
   for
   sections
   taskgroup */

static tree
c_parser_omp_clause_cancelkind (c_parser *parser ATTRIBUTE_UNUSED,
				enum omp_clause_code code, tree list)
{
  tree c = build_omp_clause (c_parser_peek_token (parser)->location, code);
  OMP_CLAUSE_CHAIN (c) = list;

  return c;
}

/* OpenMP 4.5:
   nogroup */

static tree
c_parser_omp_clause_nogroup (c_parser *parser ATTRIBUTE_UNUSED, tree list)
{
  check_no_duplicate_clause (list, OMP_CLAUSE_NOGROUP, "nogroup");
  tree c = build_omp_clause (c_parser_peek_token (parser)->location,
			     OMP_CLAUSE_NOGROUP);
  OMP_CLAUSE_CHAIN (c) = list;
  return c;
}

/* OpenMP 4.5:
   simd
   threads */

static tree
c_parser_omp_clause_orderedkind (c_parser *parser ATTRIBUTE_UNUSED,
				 enum omp_clause_code code, tree list)
{
  check_no_duplicate_clause (list, code, omp_clause_code_name[code]);
  tree c = build_omp_clause (c_parser_peek_token (parser)->location, code);
  OMP_CLAUSE_CHAIN (c) = list;
  return c;
}

/* OpenMP 4.0:
   num_teams ( expression ) */

static tree
c_parser_omp_clause_num_teams (c_parser *parser, tree list)
{
  location_t num_teams_loc = c_parser_peek_token (parser)->location;
  matching_parens parens;
  if (parens.require_open (parser))
    {
      location_t expr_loc = c_parser_peek_token (parser)->location;
      c_expr expr = c_parser_expr_no_commas (parser, NULL);
      expr = convert_lvalue_to_rvalue (expr_loc, expr, false, true);
      tree c, t = expr.value;
      t = c_fully_fold (t, false, NULL);

      parens.skip_until_found_close (parser);

      if (!INTEGRAL_TYPE_P (TREE_TYPE (t)))
	{
	  c_parser_error (parser, "expected integer expression");
	  return list;
	}

      /* Attempt to statically determine when the number isn't positive.  */
      c = fold_build2_loc (expr_loc, LE_EXPR, boolean_type_node, t,
			   build_int_cst (TREE_TYPE (t), 0));
      protected_set_expr_location (c, expr_loc);
      if (c == boolean_true_node)
	{
	  warning_at (expr_loc, 0, "%<num_teams%> value must be positive");
	  t = integer_one_node;
	}

      check_no_duplicate_clause (list, OMP_CLAUSE_NUM_TEAMS, "num_teams");

      c = build_omp_clause (num_teams_loc, OMP_CLAUSE_NUM_TEAMS);
      OMP_CLAUSE_NUM_TEAMS_EXPR (c) = t;
      OMP_CLAUSE_CHAIN (c) = list;
      list = c;
    }

  return list;
}

/* OpenMP 4.0:
   thread_limit ( expression ) */

static tree
c_parser_omp_clause_thread_limit (c_parser *parser, tree list)
{
  location_t num_thread_limit_loc = c_parser_peek_token (parser)->location;
  matching_parens parens;
  if (parens.require_open (parser))
    {
      location_t expr_loc = c_parser_peek_token (parser)->location;
      c_expr expr = c_parser_expr_no_commas (parser, NULL);
      expr = convert_lvalue_to_rvalue (expr_loc, expr, false, true);
      tree c, t = expr.value;
      t = c_fully_fold (t, false, NULL);

      parens.skip_until_found_close (parser);

      if (!INTEGRAL_TYPE_P (TREE_TYPE (t)))
	{
	  c_parser_error (parser, "expected integer expression");
	  return list;
	}

      /* Attempt to statically determine when the number isn't positive.  */
      c = fold_build2_loc (expr_loc, LE_EXPR, boolean_type_node, t,
			   build_int_cst (TREE_TYPE (t), 0));
      protected_set_expr_location (c, expr_loc);
      if (c == boolean_true_node)
	{
	  warning_at (expr_loc, 0, "%<thread_limit%> value must be positive");
	  t = integer_one_node;
	}

      check_no_duplicate_clause (list, OMP_CLAUSE_THREAD_LIMIT,
				 "thread_limit");

      c = build_omp_clause (num_thread_limit_loc, OMP_CLAUSE_THREAD_LIMIT);
      OMP_CLAUSE_THREAD_LIMIT_EXPR (c) = t;
      OMP_CLAUSE_CHAIN (c) = list;
      list = c;
    }

  return list;
}

/* OpenMP 4.0:
   aligned ( variable-list )
   aligned ( variable-list : constant-expression ) */

static tree
c_parser_omp_clause_aligned (c_parser *parser, tree list)
{
  location_t clause_loc = c_parser_peek_token (parser)->location;
  tree nl, c;

  matching_parens parens;
  if (!parens.require_open (parser))
    return list;

  nl = c_parser_omp_variable_list (parser, clause_loc,
				   OMP_CLAUSE_ALIGNED, list);

  if (c_parser_next_token_is (parser, CPP_COLON))
    {
      c_parser_consume_token (parser);
      location_t expr_loc = c_parser_peek_token (parser)->location;
      c_expr expr = c_parser_expr_no_commas (parser, NULL);
      expr = convert_lvalue_to_rvalue (expr_loc, expr, false, true);
      tree alignment = expr.value;
      alignment = c_fully_fold (alignment, false, NULL);
      if (TREE_CODE (alignment) != INTEGER_CST
	  || !INTEGRAL_TYPE_P (TREE_TYPE (alignment))
	  || tree_int_cst_sgn (alignment) != 1)
	{
	  error_at (clause_loc, "%<aligned%> clause alignment expression must "
				"be positive constant integer expression");
	  alignment = NULL_TREE;
	}

      for (c = nl; c != list; c = OMP_CLAUSE_CHAIN (c))
	OMP_CLAUSE_ALIGNED_ALIGNMENT (c) = alignment;
    }

  parens.skip_until_found_close (parser);
  return nl;
}

/* OpenMP 5.0:
   allocate ( variable-list )
   allocate ( expression : variable-list ) */

static tree
c_parser_omp_clause_allocate (c_parser *parser, tree list)
{
  location_t clause_loc = c_parser_peek_token (parser)->location;
  tree nl, c;
  tree allocator = NULL_TREE;

  matching_parens parens;
  if (!parens.require_open (parser))
    return list;

  if ((c_parser_next_token_is_not (parser, CPP_NAME)
       && c_parser_next_token_is_not (parser, CPP_KEYWORD))
      || (c_parser_peek_2nd_token (parser)->type != CPP_COMMA
	  && c_parser_peek_2nd_token (parser)->type != CPP_CLOSE_PAREN))
    {
      location_t expr_loc = c_parser_peek_token (parser)->location;
      c_expr expr = c_parser_expr_no_commas (parser, NULL);
      expr = convert_lvalue_to_rvalue (expr_loc, expr, false, true);
      allocator = expr.value;
      allocator = c_fully_fold (allocator, false, NULL);
      tree orig_type
	= expr.original_type ? expr.original_type : TREE_TYPE (allocator);
      orig_type = TYPE_MAIN_VARIANT (orig_type);
      if (!INTEGRAL_TYPE_P (TREE_TYPE (allocator))
	  || TREE_CODE (orig_type) != ENUMERAL_TYPE
	  || TYPE_NAME (orig_type) != get_identifier ("omp_allocator_handle_t"))
        {
          error_at (clause_loc, "%<allocate%> clause allocator expression "
				"has type %qT rather than "
				"%<omp_allocator_handle_t%>",
				TREE_TYPE (allocator));
          allocator = NULL_TREE;
        }
      if (!c_parser_require (parser, CPP_COLON, "expected %<:%>"))
	{
	  parens.skip_until_found_close (parser);
	  return list;
	}
    }

  nl = c_parser_omp_variable_list (parser, clause_loc,
				   OMP_CLAUSE_ALLOCATE, list);

  if (allocator)
    for (c = nl; c != list; c = OMP_CLAUSE_CHAIN (c))
      OMP_CLAUSE_ALLOCATE_ALLOCATOR (c) = allocator;

  parens.skip_until_found_close (parser);
  return nl;
}

/* OpenMP 4.0:
   linear ( variable-list )
   linear ( variable-list : expression )

   OpenMP 4.5:
   linear ( modifier ( variable-list ) )
   linear ( modifier ( variable-list ) : expression ) */

static tree
c_parser_omp_clause_linear (c_parser *parser, tree list)
{
  location_t clause_loc = c_parser_peek_token (parser)->location;
  tree nl, c, step;
  enum omp_clause_linear_kind kind = OMP_CLAUSE_LINEAR_DEFAULT;

  matching_parens parens;
  if (!parens.require_open (parser))
    return list;

  if (c_parser_next_token_is (parser, CPP_NAME))
    {
      c_token *tok = c_parser_peek_token (parser);
      const char *p = IDENTIFIER_POINTER (tok->value);
      if (strcmp ("val", p) == 0)
	kind = OMP_CLAUSE_LINEAR_VAL;
      if (c_parser_peek_2nd_token (parser)->type != CPP_OPEN_PAREN)
	kind = OMP_CLAUSE_LINEAR_DEFAULT;
      if (kind != OMP_CLAUSE_LINEAR_DEFAULT)
	{
	  c_parser_consume_token (parser);
	  c_parser_consume_token (parser);
	}
    }

  nl = c_parser_omp_variable_list (parser, clause_loc,
				   OMP_CLAUSE_LINEAR, list);

  if (kind != OMP_CLAUSE_LINEAR_DEFAULT)
    parens.skip_until_found_close (parser);

  if (c_parser_next_token_is (parser, CPP_COLON))
    {
      c_parser_consume_token (parser);
      location_t expr_loc = c_parser_peek_token (parser)->location;
      c_expr expr = c_parser_expr_no_commas (parser, NULL);
      expr = convert_lvalue_to_rvalue (expr_loc, expr, false, true);
      step = expr.value;
      step = c_fully_fold (step, false, NULL);
      if (!INTEGRAL_TYPE_P (TREE_TYPE (step)))
	{
	  error_at (clause_loc, "%<linear%> clause step expression must "
				"be integral");
	  step = integer_one_node;
	}

    }
  else
    step = integer_one_node;

  for (c = nl; c != list; c = OMP_CLAUSE_CHAIN (c))
    {
      OMP_CLAUSE_LINEAR_STEP (c) = step;
      OMP_CLAUSE_LINEAR_KIND (c) = kind;
    }

  parens.skip_until_found_close (parser);
  return nl;
}

/* OpenMP 5.0:
   nontemporal ( variable-list ) */

static tree
c_parser_omp_clause_nontemporal (c_parser *parser, tree list)
{
  return c_parser_omp_var_list_parens (parser, OMP_CLAUSE_NONTEMPORAL, list);
}

/* OpenMP 4.0:
   safelen ( constant-expression ) */

static tree
c_parser_omp_clause_safelen (c_parser *parser, tree list)
{
  location_t clause_loc = c_parser_peek_token (parser)->location;
  tree c, t;

  matching_parens parens;
  if (!parens.require_open (parser))
    return list;

  location_t expr_loc = c_parser_peek_token (parser)->location;
  c_expr expr = c_parser_expr_no_commas (parser, NULL);
  expr = convert_lvalue_to_rvalue (expr_loc, expr, false, true);
  t = expr.value;
  t = c_fully_fold (t, false, NULL);
  if (TREE_CODE (t) != INTEGER_CST
      || !INTEGRAL_TYPE_P (TREE_TYPE (t))
      || tree_int_cst_sgn (t) != 1)
    {
      error_at (clause_loc, "%<safelen%> clause expression must "
			    "be positive constant integer expression");
      t = NULL_TREE;
    }

  parens.skip_until_found_close (parser);
  if (t == NULL_TREE || t == error_mark_node)
    return list;

  check_no_duplicate_clause (list, OMP_CLAUSE_SAFELEN, "safelen");

  c = build_omp_clause (clause_loc, OMP_CLAUSE_SAFELEN);
  OMP_CLAUSE_SAFELEN_EXPR (c) = t;
  OMP_CLAUSE_CHAIN (c) = list;
  return c;
}

/* OpenMP 4.0:
   simdlen ( constant-expression ) */

static tree
c_parser_omp_clause_simdlen (c_parser *parser, tree list)
{
  location_t clause_loc = c_parser_peek_token (parser)->location;
  tree c, t;

  matching_parens parens;
  if (!parens.require_open (parser))
    return list;

  location_t expr_loc = c_parser_peek_token (parser)->location;
  c_expr expr = c_parser_expr_no_commas (parser, NULL);
  expr = convert_lvalue_to_rvalue (expr_loc, expr, false, true);
  t = expr.value;
  t = c_fully_fold (t, false, NULL);
  if (TREE_CODE (t) != INTEGER_CST
      || !INTEGRAL_TYPE_P (TREE_TYPE (t))
      || tree_int_cst_sgn (t) != 1)
    {
      error_at (clause_loc, "%<simdlen%> clause expression must "
			    "be positive constant integer expression");
      t = NULL_TREE;
    }

  parens.skip_until_found_close (parser);
  if (t == NULL_TREE || t == error_mark_node)
    return list;

  check_no_duplicate_clause (list, OMP_CLAUSE_SIMDLEN, "simdlen");

  c = build_omp_clause (clause_loc, OMP_CLAUSE_SIMDLEN);
  OMP_CLAUSE_SIMDLEN_EXPR (c) = t;
  OMP_CLAUSE_CHAIN (c) = list;
  return c;
}

/* OpenMP 4.5:
   vec:
     identifier [+/- integer]
     vec , identifier [+/- integer]
*/

static tree
c_parser_omp_clause_depend_sink (c_parser *parser, location_t clause_loc,
				 tree list)
{
  tree vec = NULL;
  if (c_parser_next_token_is_not (parser, CPP_NAME)
      || c_parser_peek_token (parser)->id_kind != C_ID_ID)
    {
      c_parser_error (parser, "expected identifier");
      return list;
    }

  while (c_parser_next_token_is (parser, CPP_NAME)
	 && c_parser_peek_token (parser)->id_kind == C_ID_ID)
    {
      tree t = lookup_name (c_parser_peek_token (parser)->value);
      tree addend = NULL;

      if (t == NULL_TREE)
	{
	  undeclared_variable (c_parser_peek_token (parser)->location,
			       c_parser_peek_token (parser)->value);
	  t = error_mark_node;
	}

      c_parser_consume_token (parser);

      bool neg = false;
      if (c_parser_next_token_is (parser, CPP_MINUS))
	neg = true;
      else if (!c_parser_next_token_is (parser, CPP_PLUS))
	{
	  addend = integer_zero_node;
	  neg = false;
	  goto add_to_vector;
	}
      c_parser_consume_token (parser);

      if (c_parser_next_token_is_not (parser, CPP_NUMBER))
	{
	  c_parser_error (parser, "expected integer");
	  return list;
	}

      addend = c_parser_peek_token (parser)->value;
      if (TREE_CODE (addend) != INTEGER_CST)
	{
	  c_parser_error (parser, "expected integer");
	  return list;
	}
      c_parser_consume_token (parser);

    add_to_vector:
      if (t != error_mark_node)
	{
	  vec = tree_cons (addend, t, vec);
	  if (neg)
	    OMP_CLAUSE_DEPEND_SINK_NEGATIVE (vec) = 1;
	}

      if (c_parser_next_token_is_not (parser, CPP_COMMA))
	break;

      c_parser_consume_token (parser);
    }

  if (vec == NULL_TREE)
    return list;

  tree u = build_omp_clause (clause_loc, OMP_CLAUSE_DEPEND);
  OMP_CLAUSE_DEPEND_KIND (u) = OMP_CLAUSE_DEPEND_SINK;
  OMP_CLAUSE_DECL (u) = nreverse (vec);
  OMP_CLAUSE_CHAIN (u) = list;
  return u;
}

/* OpenMP 5.0:
   iterators ( iterators-definition )

   iterators-definition:
     iterator-specifier
     iterator-specifier , iterators-definition

   iterator-specifier:
     identifier = range-specification
     iterator-type identifier = range-specification

   range-specification:
     begin : end
     begin : end : step  */

static tree
c_parser_omp_iterators (c_parser *parser)
{
  tree ret = NULL_TREE, *last = &ret;
  c_parser_consume_token (parser);

  push_scope ();

  matching_parens parens;
  if (!parens.require_open (parser))
    return error_mark_node;

  do
    {
      tree iter_type = NULL_TREE, type_expr = NULL_TREE;
      if (c_parser_next_tokens_start_typename (parser, cla_prefer_id))
	{
	  struct c_type_name *type = c_parser_type_name (parser);
	  if (type != NULL)
	    iter_type = groktypename (type, &type_expr, NULL);
	}
      if (iter_type == NULL_TREE)
	iter_type = integer_type_node;

      location_t loc = c_parser_peek_token (parser)->location;
      if (!c_parser_next_token_is (parser, CPP_NAME))
	{
	  c_parser_error (parser, "expected identifier");
	  break;
	}

      tree id = c_parser_peek_token (parser)->value;
      c_parser_consume_token (parser);

      if (!c_parser_require (parser, CPP_EQ, "expected %<=%>"))
	break;

      location_t eloc = c_parser_peek_token (parser)->location;
      c_expr expr = c_parser_expr_no_commas (parser, NULL);
      expr = convert_lvalue_to_rvalue (eloc, expr, true, false);
      tree begin = expr.value;

      if (!c_parser_require (parser, CPP_COLON, "expected %<:%>"))
	break;

      eloc = c_parser_peek_token (parser)->location;
      expr = c_parser_expr_no_commas (parser, NULL);
      expr = convert_lvalue_to_rvalue (eloc, expr, true, false);
      tree end = expr.value;

      tree step = integer_one_node;
      if (c_parser_next_token_is (parser, CPP_COLON))
	{
	  c_parser_consume_token (parser);
	  eloc = c_parser_peek_token (parser)->location;
	  expr = c_parser_expr_no_commas (parser, NULL);
	  expr = convert_lvalue_to_rvalue (eloc, expr, true, false);
	  step = expr.value;
	}

      tree iter_var = build_decl (loc, VAR_DECL, id, iter_type);
      DECL_ARTIFICIAL (iter_var) = 1;
      DECL_CONTEXT (iter_var) = current_function_decl;
      pushdecl (iter_var);

      *last = make_tree_vec (6);
      TREE_VEC_ELT (*last, 0) = iter_var;
      TREE_VEC_ELT (*last, 1) = begin;
      TREE_VEC_ELT (*last, 2) = end;
      TREE_VEC_ELT (*last, 3) = step;
      last = &TREE_CHAIN (*last);

      if (c_parser_next_token_is (parser, CPP_COMMA))
	{
	  c_parser_consume_token (parser);
	  continue;
	}
      break;
    }
  while (1);

  parens.skip_until_found_close (parser);
  return ret ? ret : error_mark_node;
}

/* OpenMP 4.0:
   depend ( depend-kind: variable-list )

   depend-kind:
     in | out | inout

   OpenMP 4.5:
   depend ( source )

   depend ( sink  : vec )

   OpenMP 5.0:
   depend ( depend-modifier , depend-kind: variable-list )

   depend-kind:
     in | out | inout | mutexinoutset | depobj

   depend-modifier:
     iterator ( iterators-definition )  */

static tree
c_parser_omp_clause_depend (c_parser *parser, tree list)
{
  location_t clause_loc = c_parser_peek_token (parser)->location;
  enum omp_clause_depend_kind kind = OMP_CLAUSE_DEPEND_LAST;
  tree nl, c, iterators = NULL_TREE;

  matching_parens parens;
  if (!parens.require_open (parser))
    return list;

  do
    {
      if (c_parser_next_token_is_not (parser, CPP_NAME))
	goto invalid_kind;

      const char *p = IDENTIFIER_POINTER (c_parser_peek_token (parser)->value);
      if (strcmp ("iterator", p) == 0 && iterators == NULL_TREE)
	{
	  iterators = c_parser_omp_iterators (parser);
	  c_parser_require (parser, CPP_COMMA, "expected %<,%>");
	  continue;
	}
      if (strcmp ("in", p) == 0)
	kind = OMP_CLAUSE_DEPEND_IN;
      else if (strcmp ("inout", p) == 0)
	kind = OMP_CLAUSE_DEPEND_INOUT;
      else if (strcmp ("mutexinoutset", p) == 0)
	kind = OMP_CLAUSE_DEPEND_MUTEXINOUTSET;
      else if (strcmp ("out", p) == 0)
	kind = OMP_CLAUSE_DEPEND_OUT;
      else if (strcmp ("depobj", p) == 0)
	kind = OMP_CLAUSE_DEPEND_DEPOBJ;
      else if (strcmp ("sink", p) == 0)
	kind = OMP_CLAUSE_DEPEND_SINK;
      else if (strcmp ("source", p) == 0)
	kind = OMP_CLAUSE_DEPEND_SOURCE;
      else
	goto invalid_kind;
      break;
    }
  while (1);

  c_parser_consume_token (parser);

  if (iterators
      && (kind == OMP_CLAUSE_DEPEND_SOURCE || kind == OMP_CLAUSE_DEPEND_SINK))
    {
      pop_scope ();
      error_at (clause_loc, "%<iterator%> modifier incompatible with %qs",
		kind == OMP_CLAUSE_DEPEND_SOURCE ? "source" : "sink");
      iterators = NULL_TREE;
    }

  if (kind == OMP_CLAUSE_DEPEND_SOURCE)
    {
      c = build_omp_clause (clause_loc, OMP_CLAUSE_DEPEND);
      OMP_CLAUSE_DEPEND_KIND (c) = kind;
      OMP_CLAUSE_DECL (c) = NULL_TREE;
      OMP_CLAUSE_CHAIN (c) = list;
      parens.skip_until_found_close (parser);
      return c;
    }

  if (!c_parser_require (parser, CPP_COLON, "expected %<:%>"))
    goto resync_fail;

  if (kind == OMP_CLAUSE_DEPEND_SINK)
    nl = c_parser_omp_clause_depend_sink (parser, clause_loc, list);
  else
    {
      nl = c_parser_omp_variable_list (parser, clause_loc,
				       OMP_CLAUSE_DEPEND, list);

      if (iterators)
	{
	  tree block = pop_scope ();
	  if (iterators == error_mark_node)
	    iterators = NULL_TREE;
	  else
	    TREE_VEC_ELT (iterators, 5) = block;
	}

      for (c = nl; c != list; c = OMP_CLAUSE_CHAIN (c))
	{
	  OMP_CLAUSE_DEPEND_KIND (c) = kind;
	  if (iterators)
	    OMP_CLAUSE_DECL (c)
	      = build_tree_list (iterators, OMP_CLAUSE_DECL (c));
	}
    }

  parens.skip_until_found_close (parser);
  return nl;

 invalid_kind:
  c_parser_error (parser, "invalid depend kind");
 resync_fail:
  parens.skip_until_found_close (parser);
  if (iterators)
    pop_scope ();
  return list;
}

/* OpenMP 4.0:
   map ( map-kind: variable-list )
   map ( variable-list )

   map-kind:
     alloc | to | from | tofrom

   OpenMP 4.5:
   map-kind:
     alloc | to | from | tofrom | release | delete

   map ( always [,] map-kind: variable-list ) */

static tree
c_parser_omp_clause_map (c_parser *parser, tree list)
{
  location_t clause_loc = c_parser_peek_token (parser)->location;
  enum gomp_map_kind kind = GOMP_MAP_TOFROM;
  int always = 0;
  enum c_id_kind always_id_kind = C_ID_NONE;
  location_t always_loc = UNKNOWN_LOCATION;
  tree always_id = NULL_TREE;
  tree nl, c;

  matching_parens parens;
  if (!parens.require_open (parser))
    return list;

  if (c_parser_next_token_is (parser, CPP_NAME))
    {
      c_token *tok = c_parser_peek_token (parser);
      const char *p = IDENTIFIER_POINTER (tok->value);
      always_id_kind = tok->id_kind;
      always_loc = tok->location;
      always_id = tok->value;
      if (strcmp ("always", p) == 0)
	{
	  c_token *sectok = c_parser_peek_2nd_token (parser);
	  if (sectok->type == CPP_COMMA)
	    {
	      c_parser_consume_token (parser);
	      c_parser_consume_token (parser);
	      always = 2;
	    }
	  else if (sectok->type == CPP_NAME)
	    {
	      p = IDENTIFIER_POINTER (sectok->value);
	      if (strcmp ("alloc", p) == 0
		  || strcmp ("to", p) == 0
		  || strcmp ("from", p) == 0
		  || strcmp ("tofrom", p) == 0
		  || strcmp ("release", p) == 0
		  || strcmp ("delete", p) == 0)
		{
		  c_parser_consume_token (parser);
		  always = 1;
		}
	    }
	}
    }

  if (c_parser_next_token_is (parser, CPP_NAME)
      && c_parser_peek_2nd_token (parser)->type == CPP_COLON)
    {
      const char *p = IDENTIFIER_POINTER (c_parser_peek_token (parser)->value);
      if (strcmp ("alloc", p) == 0)
	kind = GOMP_MAP_ALLOC;
      else if (strcmp ("to", p) == 0)
	kind = always ? GOMP_MAP_ALWAYS_TO : GOMP_MAP_TO;
      else if (strcmp ("from", p) == 0)
	kind = always ? GOMP_MAP_ALWAYS_FROM : GOMP_MAP_FROM;
      else if (strcmp ("tofrom", p) == 0)
	kind = always ? GOMP_MAP_ALWAYS_TOFROM : GOMP_MAP_TOFROM;
      else if (strcmp ("release", p) == 0)
	kind = GOMP_MAP_RELEASE;
      else if (strcmp ("delete", p) == 0)
	kind = GOMP_MAP_DELETE;
      else
	{
	  c_parser_error (parser, "invalid map kind");
	  c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
				     "expected %<)%>");
	  return list;
	}
      c_parser_consume_token (parser);
      c_parser_consume_token (parser);
    }
  else if (always)
    {
      if (always_id_kind != C_ID_ID)
	{
	  c_parser_error (parser, "expected identifier");
	  parens.skip_until_found_close (parser);
	  return list;
	}

      tree t = lookup_name (always_id);
      if (t == NULL_TREE)
	{
	  undeclared_variable (always_loc, always_id);
	  t = error_mark_node;
	}
      if (t != error_mark_node)
	{
	  tree u = build_omp_clause (clause_loc, OMP_CLAUSE_MAP);
	  OMP_CLAUSE_DECL (u) = t;
	  OMP_CLAUSE_CHAIN (u) = list;
	  OMP_CLAUSE_SET_MAP_KIND (u, kind);
	  list = u;
	}
      if (always == 1)
	{
	  parens.skip_until_found_close (parser);
	  return list;
	}
    }

  nl = c_parser_omp_variable_list (parser, clause_loc, OMP_CLAUSE_MAP, list);

  for (c = nl; c != list; c = OMP_CLAUSE_CHAIN (c))
    OMP_CLAUSE_SET_MAP_KIND (c, kind);

  parens.skip_until_found_close (parser);
  return nl;
}

/* OpenMP 4.0:
   device ( expression ) */

static tree
c_parser_omp_clause_device (c_parser *parser, tree list)
{
  location_t clause_loc = c_parser_peek_token (parser)->location;
  matching_parens parens;
  if (parens.require_open (parser))
    {
      location_t expr_loc = c_parser_peek_token (parser)->location;
      c_expr expr = c_parser_expr_no_commas (parser, NULL);
      expr = convert_lvalue_to_rvalue (expr_loc, expr, false, true);
      tree c, t = expr.value;
      t = c_fully_fold (t, false, NULL);

      parens.skip_until_found_close (parser);

      if (!INTEGRAL_TYPE_P (TREE_TYPE (t)))
	{
	  c_parser_error (parser, "expected integer expression");
	  return list;
	}

      check_no_duplicate_clause (list, OMP_CLAUSE_DEVICE, "device");

      c = build_omp_clause (clause_loc, OMP_CLAUSE_DEVICE);
      OMP_CLAUSE_DEVICE_ID (c) = t;
      OMP_CLAUSE_CHAIN (c) = list;
      list = c;
    }

  return list;
}

/* OpenMP 4.0:
   dist_schedule ( static )
   dist_schedule ( static , expression ) */

static tree
c_parser_omp_clause_dist_schedule (c_parser *parser, tree list)
{
  tree c, t = NULL_TREE;
  location_t loc = c_parser_peek_token (parser)->location;

  matching_parens parens;
  if (!parens.require_open (parser))
    return list;

  if (!c_parser_next_token_is_keyword (parser, RID_STATIC))
    {
      c_parser_error (parser, "invalid dist_schedule kind");
      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
				 "expected %<)%>");
      return list;
    }

  c_parser_consume_token (parser);
  if (c_parser_next_token_is (parser, CPP_COMMA))
    {
      c_parser_consume_token (parser);

      location_t expr_loc = c_parser_peek_token (parser)->location;
      c_expr expr = c_parser_expr_no_commas (parser, NULL);
      expr = convert_lvalue_to_rvalue (expr_loc, expr, false, true);
      t = expr.value;
      t = c_fully_fold (t, false, NULL);
      parens.skip_until_found_close (parser);
    }
  else
    c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
			       "expected %<,%> or %<)%>");

  /* check_no_duplicate_clause (list, OMP_CLAUSE_DIST_SCHEDULE,
				"dist_schedule"); */
  if (omp_find_clause (list, OMP_CLAUSE_DIST_SCHEDULE))
    warning_at (loc, 0, "too many %qs clauses", "dist_schedule");
  if (t == error_mark_node)
    return list;

  c = build_omp_clause (loc, OMP_CLAUSE_DIST_SCHEDULE);
  OMP_CLAUSE_DIST_SCHEDULE_CHUNK_EXPR (c) = t;
  OMP_CLAUSE_CHAIN (c) = list;
  return c;
}

/* OpenMP 4.0:
   proc_bind ( proc-bind-kind )

   proc-bind-kind:
     master | close | spread  */

static tree
c_parser_omp_clause_proc_bind (c_parser *parser, tree list)
{
  location_t clause_loc = c_parser_peek_token (parser)->location;
  enum omp_clause_proc_bind_kind kind;
  tree c;

  matching_parens parens;
  if (!parens.require_open (parser))
    return list;

  if (c_parser_next_token_is (parser, CPP_NAME))
    {
      const char *p = IDENTIFIER_POINTER (c_parser_peek_token (parser)->value);
      if (strcmp ("master", p) == 0)
	kind = OMP_CLAUSE_PROC_BIND_MASTER;
      else if (strcmp ("close", p) == 0)
	kind = OMP_CLAUSE_PROC_BIND_CLOSE;
      else if (strcmp ("spread", p) == 0)
	kind = OMP_CLAUSE_PROC_BIND_SPREAD;
      else
	goto invalid_kind;
    }
  else
    goto invalid_kind;

  check_no_duplicate_clause (list, OMP_CLAUSE_PROC_BIND, "proc_bind");
  c_parser_consume_token (parser);
  parens.skip_until_found_close (parser);
  c = build_omp_clause (clause_loc, OMP_CLAUSE_PROC_BIND);
  OMP_CLAUSE_PROC_BIND_KIND (c) = kind;
  OMP_CLAUSE_CHAIN (c) = list;
  return c;

 invalid_kind:
  c_parser_error (parser, "invalid proc_bind kind");
  parens.skip_until_found_close (parser);
  return list;
}

/* OpenMP 5.0:
   device_type ( host | nohost | any )  */

static tree
c_parser_omp_clause_device_type (c_parser *parser, tree list)
{
  location_t clause_loc = c_parser_peek_token (parser)->location;
  enum omp_clause_device_type_kind kind;
  tree c;

  matching_parens parens;
  if (!parens.require_open (parser))
    return list;

  if (c_parser_next_token_is (parser, CPP_NAME))
    {
      const char *p = IDENTIFIER_POINTER (c_parser_peek_token (parser)->value);
      if (strcmp ("host", p) == 0)
	kind = OMP_CLAUSE_DEVICE_TYPE_HOST;
      else if (strcmp ("nohost", p) == 0)
	kind = OMP_CLAUSE_DEVICE_TYPE_NOHOST;
      else if (strcmp ("any", p) == 0)
	kind = OMP_CLAUSE_DEVICE_TYPE_ANY;
      else
	goto invalid_kind;
    }
  else
    goto invalid_kind;

  /* check_no_duplicate_clause (list, OMP_CLAUSE_DEVICE_TYPE,
				"device_type");  */
  c_parser_consume_token (parser);
  parens.skip_until_found_close (parser);
  c = build_omp_clause (clause_loc, OMP_CLAUSE_DEVICE_TYPE);
  OMP_CLAUSE_DEVICE_TYPE_KIND (c) = kind;
  OMP_CLAUSE_CHAIN (c) = list;
  return c;

 invalid_kind:
  c_parser_error (parser, "expected %<host%>, %<nohost%> or %<any%>");
  parens.skip_until_found_close (parser);
  return list;
}

/* OpenMP 4.0:
   to ( variable-list ) */

static tree
c_parser_omp_clause_to (c_parser *parser, tree list)
{
  return c_parser_omp_var_list_parens (parser, OMP_CLAUSE_TO, list);
}

/* OpenMP 4.0:
   from ( variable-list ) */

static tree
c_parser_omp_clause_from (c_parser *parser, tree list)
{
  return c_parser_omp_var_list_parens (parser, OMP_CLAUSE_FROM, list);
}

/* OpenMP 4.0:
   uniform ( variable-list ) */

static tree
c_parser_omp_clause_uniform (c_parser *parser, tree list)
{
  /* The clauses location.  */
  location_t loc = c_parser_peek_token (parser)->location;

  matching_parens parens;
  if (parens.require_open (parser))
    {
      list = c_parser_omp_variable_list (parser, loc, OMP_CLAUSE_UNIFORM,
					 list);
      parens.skip_until_found_close (parser);
    }
  return list;
}

/* OpenMP 5.0:
   detach ( event-handle ) */

static tree
c_parser_omp_clause_detach (c_parser *parser, tree list)
{
  matching_parens parens;
  location_t clause_loc = c_parser_peek_token (parser)->location;

  if (!parens.require_open (parser))
    return list;

  if (c_parser_next_token_is_not (parser, CPP_NAME)
      || c_parser_peek_token (parser)->id_kind != C_ID_ID)
    {
      c_parser_error (parser, "expected identifier");
      parens.skip_until_found_close (parser);
      return list;
    }

  tree t = lookup_name (c_parser_peek_token (parser)->value);
  if (t == NULL_TREE)
    {
      undeclared_variable (c_parser_peek_token (parser)->location,
			   c_parser_peek_token (parser)->value);
      parens.skip_until_found_close (parser);
      return list;
    }
  c_parser_consume_token (parser);

  tree type = TYPE_MAIN_VARIANT (TREE_TYPE (t));
  if (!INTEGRAL_TYPE_P (type)
      || TREE_CODE (type) != ENUMERAL_TYPE
      || TYPE_NAME (type) != get_identifier ("omp_event_handle_t"))
    {
      error_at (clause_loc, "%<detach%> clause event handle "
			    "has type %qT rather than "
			    "%<omp_event_handle_t%>",
			    type);
      parens.skip_until_found_close (parser);
      return list;
    }

  tree u = build_omp_clause (clause_loc, OMP_CLAUSE_DETACH);
  OMP_CLAUSE_DECL (u) = t;
  OMP_CLAUSE_CHAIN (u) = list;
  parens.skip_until_found_close (parser);
  return u;
}

/* Parse all OpenACC clauses.  The set clauses allowed by the directive
   is a bitmask in MASK.  Return the list of clauses found.  */

static tree
c_parser_oacc_all_clauses (c_parser *parser, omp_clause_mask mask,
			   const char *where, bool finish_p = true)
{
  tree clauses = NULL;
  bool first = true;

  while (c_parser_next_token_is_not (parser, CPP_PRAGMA_EOL))
    {
      location_t here;
      pragma_omp_clause c_kind;
      const char *c_name;
      tree prev = clauses;

      if (!first && c_parser_next_token_is (parser, CPP_COMMA))
	c_parser_consume_token (parser);

      here = c_parser_peek_token (parser)->location;
      c_kind = c_parser_omp_clause_name (parser);

      switch (c_kind)
	{
	case PRAGMA_OACC_CLAUSE_ASYNC:
	  clauses = c_parser_oacc_clause_async (parser, clauses);
	  c_name = "async";
	  break;
	case PRAGMA_OACC_CLAUSE_AUTO:
	  clauses = c_parser_oacc_simple_clause (here, OMP_CLAUSE_AUTO,
						 clauses);
	  c_name = "auto";
	  break;
	case PRAGMA_OACC_CLAUSE_ATTACH:
	  clauses = c_parser_oacc_data_clause (parser, c_kind, clauses);
	  c_name = "attach";
	  break;
	case PRAGMA_OACC_CLAUSE_COLLAPSE:
	  clauses = c_parser_omp_clause_collapse (parser, clauses);
	  c_name = "collapse";
	  break;
	case PRAGMA_OACC_CLAUSE_COPY:
	  clauses = c_parser_oacc_data_clause (parser, c_kind, clauses);
	  c_name = "copy";
	  break;
	case PRAGMA_OACC_CLAUSE_COPYIN:
	  clauses = c_parser_oacc_data_clause (parser, c_kind, clauses);
	  c_name = "copyin";
	  break;
	case PRAGMA_OACC_CLAUSE_COPYOUT:
	  clauses = c_parser_oacc_data_clause (parser, c_kind, clauses);
	  c_name = "copyout";
	  break;
	case PRAGMA_OACC_CLAUSE_CREATE:
	  clauses = c_parser_oacc_data_clause (parser, c_kind, clauses);
	  c_name = "create";
	  break;
	case PRAGMA_OACC_CLAUSE_DELETE:
	  clauses = c_parser_oacc_data_clause (parser, c_kind, clauses);
	  c_name = "delete";
	  break;
	case PRAGMA_OMP_CLAUSE_DEFAULT:
	  clauses = c_parser_omp_clause_default (parser, clauses, true);
	  c_name = "default";
	  break;
	case PRAGMA_OACC_CLAUSE_DETACH:
	  clauses = c_parser_oacc_data_clause (parser, c_kind, clauses);
	  c_name = "detach";
	  break;
	case PRAGMA_OACC_CLAUSE_DEVICE:
	  clauses = c_parser_oacc_data_clause (parser, c_kind, clauses);
	  c_name = "device";
	  break;
	case PRAGMA_OACC_CLAUSE_DEVICEPTR:
	  clauses = c_parser_oacc_data_clause_deviceptr (parser, clauses);
	  c_name = "deviceptr";
	  break;
	case PRAGMA_OACC_CLAUSE_DEVICE_RESIDENT:
	  clauses = c_parser_oacc_data_clause (parser, c_kind, clauses);
	  c_name = "device_resident";
	  break;
	case PRAGMA_OACC_CLAUSE_FINALIZE:
	  clauses = c_parser_oacc_simple_clause (here, OMP_CLAUSE_FINALIZE,
						 clauses);
	  c_name = "finalize";
	  break;
	case PRAGMA_OACC_CLAUSE_FIRSTPRIVATE:
	  clauses = c_parser_omp_clause_firstprivate (parser, clauses);
	  c_name = "firstprivate";
	  break;
	case PRAGMA_OACC_CLAUSE_GANG:
	  c_name = "gang";
	  clauses = c_parser_oacc_shape_clause (parser, here, OMP_CLAUSE_GANG,
						c_name, clauses);
	  break;
	case PRAGMA_OACC_CLAUSE_HOST:
	  clauses = c_parser_oacc_data_clause (parser, c_kind, clauses);
	  c_name = "host";
	  break;
	case PRAGMA_OACC_CLAUSE_IF:
	  clauses = c_parser_omp_clause_if (parser, clauses, false);
	  c_name = "if";
	  break;
	case PRAGMA_OACC_CLAUSE_IF_PRESENT:
	  clauses = c_parser_oacc_simple_clause (here, OMP_CLAUSE_IF_PRESENT,
						 clauses);
	  c_name = "if_present";
	  break;
	case PRAGMA_OACC_CLAUSE_INDEPENDENT:
	  clauses = c_parser_oacc_simple_clause (here, OMP_CLAUSE_INDEPENDENT,
						 clauses);
	  c_name = "independent";
	  break;
	case PRAGMA_OACC_CLAUSE_LINK:
	  clauses = c_parser_oacc_data_clause (parser, c_kind, clauses);
	  c_name = "link";
	  break;
	case PRAGMA_OACC_CLAUSE_NO_CREATE:
	  clauses = c_parser_oacc_data_clause (parser, c_kind, clauses);
	  c_name = "no_create";
	  break;
	case PRAGMA_OACC_CLAUSE_NUM_GANGS:
	  clauses = c_parser_oacc_single_int_clause (parser,
						     OMP_CLAUSE_NUM_GANGS,
						     clauses);
	  c_name = "num_gangs";
	  break;
	case PRAGMA_OACC_CLAUSE_NUM_WORKERS:
	  clauses = c_parser_oacc_single_int_clause (parser,
						     OMP_CLAUSE_NUM_WORKERS,
						     clauses);
	  c_name = "num_workers";
	  break;
	case PRAGMA_OACC_CLAUSE_PRESENT:
	  clauses = c_parser_oacc_data_clause (parser, c_kind, clauses);
	  c_name = "present";
	  break;
	case PRAGMA_OACC_CLAUSE_PRIVATE:
	  clauses = c_parser_omp_clause_private (parser, clauses);
	  c_name = "private";
	  break;
	case PRAGMA_OACC_CLAUSE_REDUCTION:
	  clauses
	    = c_parser_omp_clause_reduction (parser, OMP_CLAUSE_REDUCTION,
					     false, clauses);
	  c_name = "reduction";
	  break;
	case PRAGMA_OACC_CLAUSE_SEQ:
	  clauses = c_parser_oacc_simple_clause (here, OMP_CLAUSE_SEQ,
						 clauses);
	  c_name = "seq";
	  break;
	case PRAGMA_OACC_CLAUSE_TILE:
	  clauses = c_parser_oacc_clause_tile (parser, clauses);
	  c_name = "tile";
	  break;
	case PRAGMA_OACC_CLAUSE_USE_DEVICE:
	  clauses = c_parser_omp_clause_use_device_ptr (parser, clauses);
	  c_name = "use_device";
	  break;
	case PRAGMA_OACC_CLAUSE_VECTOR:
	  c_name = "vector";
	  clauses = c_parser_oacc_shape_clause (parser, here, OMP_CLAUSE_VECTOR,
						c_name,	clauses);
	  break;
	case PRAGMA_OACC_CLAUSE_VECTOR_LENGTH:
	  clauses = c_parser_oacc_single_int_clause (parser,
						     OMP_CLAUSE_VECTOR_LENGTH,
						     clauses);
	  c_name = "vector_length";
	  break;
	case PRAGMA_OACC_CLAUSE_WAIT:
	  clauses = c_parser_oacc_clause_wait (parser, clauses);
	  c_name = "wait";
	  break;
	case PRAGMA_OACC_CLAUSE_WORKER:
	  c_name = "worker";
	  clauses = c_parser_oacc_shape_clause (parser, here, OMP_CLAUSE_WORKER,
						c_name, clauses);
	  break;
	default:
	  c_parser_error (parser, "expected %<#pragma acc%> clause");
	  goto saw_error;
	}

      first = false;

      if (((mask >> c_kind) & 1) == 0)
	{
	  /* Remove the invalid clause(s) from the list to avoid
	     confusing the rest of the compiler.  */
	  clauses = prev;
	  error_at (here, "%qs is not valid for %qs", c_name, where);
	}
    }

 saw_error:
  c_parser_skip_to_pragma_eol (parser);

  if (finish_p)
    return c_finish_omp_clauses (clauses, C_ORT_ACC);

  return clauses;
}

/* Parse all OpenMP clauses.  The set clauses allowed by the directive
   is a bitmask in MASK.  Return the list of clauses found.
   FINISH_P set if c_finish_omp_clauses should be called.
   NESTED non-zero if clauses should be terminated by closing paren instead
   of end of pragma.  If it is 2, additionally commas are required in between
   the clauses.  */

static tree
c_parser_omp_all_clauses (c_parser *parser, omp_clause_mask mask,
			  const char *where, bool finish_p = true,
			  int nested = 0)
{
  tree clauses = NULL;
  bool first = true;

  while (c_parser_next_token_is_not (parser, CPP_PRAGMA_EOL))
    {
      location_t here;
      pragma_omp_clause c_kind;
      const char *c_name;
      tree prev = clauses;

      if (nested && c_parser_next_token_is (parser, CPP_CLOSE_PAREN))
	break;

      if (!first)
	{
	  if (c_parser_next_token_is (parser, CPP_COMMA))
	    c_parser_consume_token (parser);
	  else if (nested == 2)
	    error_at (c_parser_peek_token (parser)->location,
		      "clauses in %<simd%> trait should be separated "
		      "by %<,%>");
	}

      here = c_parser_peek_token (parser)->location;
      c_kind = c_parser_omp_clause_name (parser);

      switch (c_kind)
	{
	case PRAGMA_OMP_CLAUSE_BIND:
	  clauses = c_parser_omp_clause_bind (parser, clauses);
	  c_name = "bind";
	  break;
	case PRAGMA_OMP_CLAUSE_COLLAPSE:
	  clauses = c_parser_omp_clause_collapse (parser, clauses);
	  c_name = "collapse";
	  break;
	case PRAGMA_OMP_CLAUSE_COPYIN:
	  clauses = c_parser_omp_clause_copyin (parser, clauses);
	  c_name = "copyin";
	  break;
	case PRAGMA_OMP_CLAUSE_COPYPRIVATE:
	  clauses = c_parser_omp_clause_copyprivate (parser, clauses);
	  c_name = "copyprivate";
	  break;
	case PRAGMA_OMP_CLAUSE_DEFAULT:
	  clauses = c_parser_omp_clause_default (parser, clauses, false);
	  c_name = "default";
	  break;
	case PRAGMA_OMP_CLAUSE_DETACH:
	  clauses = c_parser_omp_clause_detach (parser, clauses);
	  c_name = "detach";
	  break;
	case PRAGMA_OMP_CLAUSE_FIRSTPRIVATE:
	  clauses = c_parser_omp_clause_firstprivate (parser, clauses);
	  c_name = "firstprivate";
	  break;
	case PRAGMA_OMP_CLAUSE_FINAL:
	  clauses = c_parser_omp_clause_final (parser, clauses);
	  c_name = "final";
	  break;
	case PRAGMA_OMP_CLAUSE_GRAINSIZE:
	  clauses = c_parser_omp_clause_grainsize (parser, clauses);
	  c_name = "grainsize";
	  break;
	case PRAGMA_OMP_CLAUSE_HINT:
	  clauses = c_parser_omp_clause_hint (parser, clauses);
	  c_name = "hint";
	  break;
	case PRAGMA_OMP_CLAUSE_DEFAULTMAP:
	  clauses = c_parser_omp_clause_defaultmap (parser, clauses);
	  c_name = "defaultmap";
	  break;
	case PRAGMA_OMP_CLAUSE_IF:
	  clauses = c_parser_omp_clause_if (parser, clauses, true);
	  c_name = "if";
	  break;
	case PRAGMA_OMP_CLAUSE_IN_REDUCTION:
	  clauses
	    = c_parser_omp_clause_reduction (parser, OMP_CLAUSE_IN_REDUCTION,
					     true, clauses);
	  c_name = "in_reduction";
	  break;
	case PRAGMA_OMP_CLAUSE_LASTPRIVATE:
	  clauses = c_parser_omp_clause_lastprivate (parser, clauses);
	  c_name = "lastprivate";
	  break;
	case PRAGMA_OMP_CLAUSE_MERGEABLE:
	  clauses = c_parser_omp_clause_mergeable (parser, clauses);
	  c_name = "mergeable";
	  break;
	case PRAGMA_OMP_CLAUSE_NOWAIT:
	  clauses = c_parser_omp_clause_nowait (parser, clauses);
	  c_name = "nowait";
	  break;
	case PRAGMA_OMP_CLAUSE_NUM_TASKS:
	  clauses = c_parser_omp_clause_num_tasks (parser, clauses);
	  c_name = "num_tasks";
	  break;
	case PRAGMA_OMP_CLAUSE_NUM_THREADS:
	  clauses = c_parser_omp_clause_num_threads (parser, clauses);
	  c_name = "num_threads";
	  break;
	case PRAGMA_OMP_CLAUSE_ORDER:
	  clauses = c_parser_omp_clause_order (parser, clauses);
	  c_name = "order";
	  break;
	case PRAGMA_OMP_CLAUSE_ORDERED:
	  clauses = c_parser_omp_clause_ordered (parser, clauses);
	  c_name = "ordered";
	  break;
	case PRAGMA_OMP_CLAUSE_PRIORITY:
	  clauses = c_parser_omp_clause_priority (parser, clauses);
	  c_name = "priority";
	  break;
	case PRAGMA_OMP_CLAUSE_PRIVATE:
	  clauses = c_parser_omp_clause_private (parser, clauses);
	  c_name = "private";
	  break;
	case PRAGMA_OMP_CLAUSE_REDUCTION:
	  clauses
	    = c_parser_omp_clause_reduction (parser, OMP_CLAUSE_REDUCTION,
					     true, clauses);
	  c_name = "reduction";
	  break;
	case PRAGMA_OMP_CLAUSE_SCHEDULE:
	  clauses = c_parser_omp_clause_schedule (parser, clauses);
	  c_name = "schedule";
	  break;
	case PRAGMA_OMP_CLAUSE_SHARED:
	  clauses = c_parser_omp_clause_shared (parser, clauses);
	  c_name = "shared";
	  break;
	case PRAGMA_OMP_CLAUSE_TASK_REDUCTION:
	  clauses
	    = c_parser_omp_clause_reduction (parser, OMP_CLAUSE_TASK_REDUCTION,
					     true, clauses);
	  c_name = "task_reduction";
	  break;
	case PRAGMA_OMP_CLAUSE_UNTIED:
	  clauses = c_parser_omp_clause_untied (parser, clauses);
	  c_name = "untied";
	  break;
	case PRAGMA_OMP_CLAUSE_INBRANCH:
	  clauses = c_parser_omp_clause_branch (parser, OMP_CLAUSE_INBRANCH,
						clauses);
	  c_name = "inbranch";
	  break;
	case PRAGMA_OMP_CLAUSE_NONTEMPORAL:
	  clauses = c_parser_omp_clause_nontemporal (parser, clauses);
	  c_name = "nontemporal";
	  break;
	case PRAGMA_OMP_CLAUSE_NOTINBRANCH:
	  clauses = c_parser_omp_clause_branch (parser, OMP_CLAUSE_NOTINBRANCH,
						clauses);
	  c_name = "notinbranch";
	  break;
	case PRAGMA_OMP_CLAUSE_PARALLEL:
	  clauses
	    = c_parser_omp_clause_cancelkind (parser, OMP_CLAUSE_PARALLEL,
					      clauses);
	  c_name = "parallel";
	  if (!first)
	    {
	     clause_not_first:
	      error_at (here, "%qs must be the first clause of %qs",
			c_name, where);
	      clauses = prev;
	    }
	  break;
	case PRAGMA_OMP_CLAUSE_FOR:
	  clauses
	    = c_parser_omp_clause_cancelkind (parser, OMP_CLAUSE_FOR,
					      clauses);
	  c_name = "for";
	  if (!first)
	    goto clause_not_first;
	  break;
	case PRAGMA_OMP_CLAUSE_SECTIONS:
	  clauses
	    = c_parser_omp_clause_cancelkind (parser, OMP_CLAUSE_SECTIONS,
					      clauses);
	  c_name = "sections";
	  if (!first)
	    goto clause_not_first;
	  break;
	case PRAGMA_OMP_CLAUSE_TASKGROUP:
	  clauses
	    = c_parser_omp_clause_cancelkind (parser, OMP_CLAUSE_TASKGROUP,
					      clauses);
	  c_name = "taskgroup";
	  if (!first)
	    goto clause_not_first;
	  break;
	case PRAGMA_OMP_CLAUSE_LINK:
	  clauses
	    = c_parser_omp_var_list_parens (parser, OMP_CLAUSE_LINK, clauses);
	  c_name = "link";
	  break;
	case PRAGMA_OMP_CLAUSE_TO:
	  if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_LINK)) != 0)
	    clauses
	      = c_parser_omp_var_list_parens (parser, OMP_CLAUSE_TO_DECLARE,
					      clauses);
	  else
	    clauses = c_parser_omp_clause_to (parser, clauses);
	  c_name = "to";
	  break;
	case PRAGMA_OMP_CLAUSE_FROM:
	  clauses = c_parser_omp_clause_from (parser, clauses);
	  c_name = "from";
	  break;
	case PRAGMA_OMP_CLAUSE_UNIFORM:
	  clauses = c_parser_omp_clause_uniform (parser, clauses);
	  c_name = "uniform";
	  break;
	case PRAGMA_OMP_CLAUSE_NUM_TEAMS:
	  clauses = c_parser_omp_clause_num_teams (parser, clauses);
	  c_name = "num_teams";
	  break;
	case PRAGMA_OMP_CLAUSE_THREAD_LIMIT:
	  clauses = c_parser_omp_clause_thread_limit (parser, clauses);
	  c_name = "thread_limit";
	  break;
	case PRAGMA_OMP_CLAUSE_ALIGNED:
	  clauses = c_parser_omp_clause_aligned (parser, clauses);
	  c_name = "aligned";
	  break;
	case PRAGMA_OMP_CLAUSE_ALLOCATE:
	  clauses = c_parser_omp_clause_allocate (parser, clauses);
	  c_name = "allocate";
	  break;
	case PRAGMA_OMP_CLAUSE_LINEAR: 
	  clauses = c_parser_omp_clause_linear (parser, clauses); 
	  c_name = "linear";
	  break;
	case PRAGMA_OMP_CLAUSE_DEPEND:
	  clauses = c_parser_omp_clause_depend (parser, clauses);
	  c_name = "depend";
	  break;
	case PRAGMA_OMP_CLAUSE_MAP:
	  clauses = c_parser_omp_clause_map (parser, clauses);
	  c_name = "map";
	  break;
	case PRAGMA_OMP_CLAUSE_USE_DEVICE_PTR:
	  clauses = c_parser_omp_clause_use_device_ptr (parser, clauses);
	  c_name = "use_device_ptr";
	  break;
	case PRAGMA_OMP_CLAUSE_USE_DEVICE_ADDR:
	  clauses = c_parser_omp_clause_use_device_addr (parser, clauses);
	  c_name = "use_device_addr";
	  break;
	case PRAGMA_OMP_CLAUSE_IS_DEVICE_PTR:
	  clauses = c_parser_omp_clause_is_device_ptr (parser, clauses);
	  c_name = "is_device_ptr";
	  break;
	case PRAGMA_OMP_CLAUSE_DEVICE:
	  clauses = c_parser_omp_clause_device (parser, clauses);
	  c_name = "device";
	  break;
	case PRAGMA_OMP_CLAUSE_DIST_SCHEDULE:
	  clauses = c_parser_omp_clause_dist_schedule (parser, clauses);
	  c_name = "dist_schedule";
	  break;
	case PRAGMA_OMP_CLAUSE_PROC_BIND:
	  clauses = c_parser_omp_clause_proc_bind (parser, clauses);
	  c_name = "proc_bind";
	  break;
	case PRAGMA_OMP_CLAUSE_DEVICE_TYPE:
	  clauses = c_parser_omp_clause_device_type (parser, clauses);
	  c_name = "device_type";
	  break;
	case PRAGMA_OMP_CLAUSE_SAFELEN:
	  clauses = c_parser_omp_clause_safelen (parser, clauses);
	  c_name = "safelen";
	  break;
	case PRAGMA_OMP_CLAUSE_SIMDLEN:
	  clauses = c_parser_omp_clause_simdlen (parser, clauses);
	  c_name = "simdlen";
	  break;
	case PRAGMA_OMP_CLAUSE_NOGROUP:
	  clauses = c_parser_omp_clause_nogroup (parser, clauses);
	  c_name = "nogroup";
	  break;
	case PRAGMA_OMP_CLAUSE_THREADS:
	  clauses
	    = c_parser_omp_clause_orderedkind (parser, OMP_CLAUSE_THREADS,
					       clauses);
	  c_name = "threads";
	  break;
	case PRAGMA_OMP_CLAUSE_SIMD:
	  clauses
	    = c_parser_omp_clause_orderedkind (parser, OMP_CLAUSE_SIMD,
					       clauses);
	  c_name = "simd";
	  break;
	default:
	  c_parser_error (parser, "expected %<#pragma omp%> clause");
	  goto saw_error;
	}

      first = false;

      if (((mask >> c_kind) & 1) == 0)
	{
	  /* Remove the invalid clause(s) from the list to avoid
	     confusing the rest of the compiler.  */
	  clauses = prev;
	  error_at (here, "%qs is not valid for %qs", c_name, where);
	}
    }

 saw_error:
  if (!nested)
    c_parser_skip_to_pragma_eol (parser);

  if (finish_p)
    {
      if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_UNIFORM)) != 0)
	return c_finish_omp_clauses (clauses, C_ORT_OMP_DECLARE_SIMD);
      return c_finish_omp_clauses (clauses, C_ORT_OMP);
    }

  return clauses;
}

/* OpenACC 2.0, OpenMP 2.5:
   structured-block:
     statement

   In practice, we're also interested in adding the statement to an
   outer node.  So it is convenient if we work around the fact that
   c_parser_statement calls add_stmt.  */

static tree
c_parser_omp_structured_block (c_parser *parser, bool *if_p)
{
  tree stmt = push_stmt_list ();
  c_parser_statement (parser, if_p);
  return pop_stmt_list (stmt);
}

/* OpenACC 2.0:
   # pragma acc cache (variable-list) new-line

   LOC is the location of the #pragma token.
*/

static tree
c_parser_oacc_cache (location_t loc, c_parser *parser)
{
  tree stmt, clauses;

  clauses = c_parser_omp_var_list_parens (parser, OMP_CLAUSE__CACHE_, NULL);
  clauses = c_finish_omp_clauses (clauses, C_ORT_ACC);

  c_parser_skip_to_pragma_eol (parser);

  stmt = make_node (OACC_CACHE);
  TREE_TYPE (stmt) = void_type_node;
  OACC_CACHE_CLAUSES (stmt) = clauses;
  SET_EXPR_LOCATION (stmt, loc);
  add_stmt (stmt);

  return stmt;
}

/* OpenACC 2.0:
   # pragma acc data oacc-data-clause[optseq] new-line
     structured-block

   LOC is the location of the #pragma token.
*/

#define OACC_DATA_CLAUSE_MASK						\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_ATTACH)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_COPY)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_COPYIN)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_COPYOUT)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_CREATE)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_DEVICEPTR)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_IF)			\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_NO_CREATE)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_PRESENT))

static tree
c_parser_oacc_data (location_t loc, c_parser *parser, bool *if_p)
{
  tree stmt, clauses, block;

  clauses = c_parser_oacc_all_clauses (parser, OACC_DATA_CLAUSE_MASK,
				       "#pragma acc data");

  block = c_begin_omp_parallel ();
  add_stmt (c_parser_omp_structured_block (parser, if_p));

  stmt = c_finish_oacc_data (loc, clauses, block);

  return stmt;
}

/* OpenACC 2.0:
   # pragma acc declare oacc-data-clause[optseq] new-line
*/

#define OACC_DECLARE_CLAUSE_MASK					\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_COPY)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_COPYIN)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_COPYOUT)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_CREATE)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_DEVICEPTR)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_DEVICE_RESIDENT)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_LINK)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_PRESENT))

static void
c_parser_oacc_declare (c_parser *parser)
{
  location_t pragma_loc = c_parser_peek_token (parser)->location;
  tree clauses, stmt, t, decl;

  bool error = false;

  c_parser_consume_pragma (parser);

  clauses = c_parser_oacc_all_clauses (parser, OACC_DECLARE_CLAUSE_MASK,
				       "#pragma acc declare");
  if (!clauses)
    {
      error_at (pragma_loc,
		"no valid clauses specified in %<#pragma acc declare%>");
      return;
    }

  for (t = clauses; t; t = OMP_CLAUSE_CHAIN (t))
    {
      location_t loc = OMP_CLAUSE_LOCATION (t);
      decl = OMP_CLAUSE_DECL (t);
      if (!DECL_P (decl))
	{
	  error_at (loc, "array section in %<#pragma acc declare%>");
	  error = true;
	  continue;
	}

      switch (OMP_CLAUSE_MAP_KIND (t))
	{
	case GOMP_MAP_FIRSTPRIVATE_POINTER:
	case GOMP_MAP_ALLOC:
	case GOMP_MAP_TO:
	case GOMP_MAP_FORCE_DEVICEPTR:
	case GOMP_MAP_DEVICE_RESIDENT:
	  break;

	case GOMP_MAP_LINK:
	  if (!global_bindings_p ()
	      && (TREE_STATIC (decl)
	       || !DECL_EXTERNAL (decl)))
	    {
	      error_at (loc,
			"%qD must be a global variable in "
			"%<#pragma acc declare link%>",
			decl);
	      error = true;
	      continue;
	    }
	  break;

	default:
	  if (global_bindings_p ())
	    {
	      error_at (loc, "invalid OpenACC clause at file scope");
	      error = true;
	      continue;
	    }
	  if (DECL_EXTERNAL (decl))
	    {
	      error_at (loc,
			"invalid use of %<extern%> variable %qD "
			"in %<#pragma acc declare%>", decl);
	      error = true;
	      continue;
	    }
	  else if (TREE_PUBLIC (decl))
	    {
	      error_at (loc,
			"invalid use of %<global%> variable %qD "
			"in %<#pragma acc declare%>", decl);
	      error = true;
	      continue;
	    }
	  break;
	}

      if (!c_check_in_current_scope (decl))
	{
	  error_at (loc,
		    "%qD must be a variable declared in the same scope as "
		    "%<#pragma acc declare%>", decl);
	  error = true;
	  continue;
	}

      if (lookup_attribute ("omp declare target", DECL_ATTRIBUTES (decl))
	  || lookup_attribute ("omp declare target link",
			       DECL_ATTRIBUTES (decl)))
	{
	  error_at (loc, "variable %qD used more than once with "
		    "%<#pragma acc declare%>", decl);
	  error = true;
	  continue;
	}

      if (!error)
	{
	  tree id;

	  if (OMP_CLAUSE_MAP_KIND (t) == GOMP_MAP_LINK)
	    id = get_identifier ("omp declare target link");
	  else
	    id = get_identifier ("omp declare target");

	  DECL_ATTRIBUTES (decl)
			   = tree_cons (id, NULL_TREE, DECL_ATTRIBUTES (decl));

	  if (global_bindings_p ())
	    {
	      symtab_node *node = symtab_node::get (decl);
	      if (node != NULL)
		{
		  node->offloadable = 1;
		  if (ENABLE_OFFLOADING)
		    {
		      g->have_offload = true;
		      if (is_a <varpool_node *> (node))
			vec_safe_push (offload_vars, decl);
		    }
		}
	    }
	}
    }

  if (error || global_bindings_p ())
    return;

  stmt = make_node (OACC_DECLARE);
  TREE_TYPE (stmt) = void_type_node;
  OACC_DECLARE_CLAUSES (stmt) = clauses;
  SET_EXPR_LOCATION (stmt, pragma_loc);

  add_stmt (stmt);

  return;
}

/* OpenACC 2.0:
   # pragma acc enter data oacc-enter-data-clause[optseq] new-line

   or

   # pragma acc exit data oacc-exit-data-clause[optseq] new-line


   LOC is the location of the #pragma token.
*/

#define OACC_ENTER_DATA_CLAUSE_MASK					\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_IF)			\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_ASYNC)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_ATTACH)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_COPYIN)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_CREATE)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_WAIT) )

#define OACC_EXIT_DATA_CLAUSE_MASK					\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_IF)			\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_ASYNC)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_COPYOUT)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_DELETE) 		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_DETACH) 		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_FINALIZE) 		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_WAIT) )

static void
c_parser_oacc_enter_exit_data (c_parser *parser, bool enter)
{
  location_t loc = c_parser_peek_token (parser)->location;
  tree clauses, stmt;
  const char *p = "";

  c_parser_consume_pragma (parser);

  if (c_parser_next_token_is (parser, CPP_NAME))
    {
      p = IDENTIFIER_POINTER (c_parser_peek_token (parser)->value);
      c_parser_consume_token (parser);
    }

  if (strcmp (p, "data") != 0)
    {
      error_at (loc, "expected %<data%> after %<#pragma acc %s%>",
		enter ? "enter" : "exit");
      parser->error = true;
      c_parser_skip_to_pragma_eol (parser);
      return;
    }

  if (enter)
    clauses = c_parser_oacc_all_clauses (parser, OACC_ENTER_DATA_CLAUSE_MASK,
					 "#pragma acc enter data");
  else
    clauses = c_parser_oacc_all_clauses (parser, OACC_EXIT_DATA_CLAUSE_MASK,
					 "#pragma acc exit data");

  if (omp_find_clause (clauses, OMP_CLAUSE_MAP) == NULL_TREE)
    {
      error_at (loc, "%<#pragma acc %s data%> has no data movement clause",
		enter ? "enter" : "exit");
      return;
    }

  stmt = enter ? make_node (OACC_ENTER_DATA) : make_node (OACC_EXIT_DATA);
  TREE_TYPE (stmt) = void_type_node;
  OMP_STANDALONE_CLAUSES (stmt) = clauses;
  SET_EXPR_LOCATION (stmt, loc);
  add_stmt (stmt);
}


/* OpenACC 2.0:
   # pragma acc host_data oacc-data-clause[optseq] new-line
     structured-block
*/

#define OACC_HOST_DATA_CLAUSE_MASK					\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_USE_DEVICE)          \
	 | (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_IF)                  \
	 | (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_IF_PRESENT) )

static tree
c_parser_oacc_host_data (location_t loc, c_parser *parser, bool *if_p)
{
  tree stmt, clauses, block;

  clauses = c_parser_oacc_all_clauses (parser, OACC_HOST_DATA_CLAUSE_MASK,
				       "#pragma acc host_data");

  block = c_begin_omp_parallel ();
  add_stmt (c_parser_omp_structured_block (parser, if_p));
  stmt = c_finish_oacc_host_data (loc, clauses, block);
  return stmt;
}


/* OpenACC 2.0:

   # pragma acc loop oacc-loop-clause[optseq] new-line
     structured-block

   LOC is the location of the #pragma token.
*/

#define OACC_LOOP_CLAUSE_MASK						\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_COLLAPSE)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_PRIVATE)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_REDUCTION)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_GANG)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_WORKER)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_VECTOR)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_AUTO)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_INDEPENDENT) 	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_SEQ)			\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_TILE) )
static tree
c_parser_oacc_loop (location_t loc, c_parser *parser, char *p_name,
		    omp_clause_mask mask, tree *cclauses, bool *if_p)
{
  bool is_parallel = ((mask >> PRAGMA_OACC_CLAUSE_REDUCTION) & 1) == 1;

  strcat (p_name, " loop");
  mask |= OACC_LOOP_CLAUSE_MASK;

  tree clauses = c_parser_oacc_all_clauses (parser, mask, p_name,
					    cclauses == NULL);
  if (cclauses)
    {
      clauses = c_oacc_split_loop_clauses (clauses, cclauses, is_parallel);
      if (*cclauses)
	*cclauses = c_finish_omp_clauses (*cclauses, C_ORT_ACC);
      if (clauses)
	clauses = c_finish_omp_clauses (clauses, C_ORT_ACC);
    }

  tree block = c_begin_compound_stmt (true);
  tree stmt = c_parser_omp_for_loop (loc, parser, OACC_LOOP, clauses, NULL,
				     if_p);
  block = c_end_compound_stmt (loc, block, true);
  add_stmt (block);

  return stmt;
}

/* OpenACC 2.0:
   # pragma acc kernels oacc-kernels-clause[optseq] new-line
     structured-block

   or

   # pragma acc parallel oacc-parallel-clause[optseq] new-line
     structured-block

   OpenACC 2.6:

   # pragma acc serial oacc-serial-clause[optseq] new-line
     structured-block

   LOC is the location of the #pragma token.
*/

#define OACC_KERNELS_CLAUSE_MASK					\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_ASYNC)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_ATTACH)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_COPY)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_COPYIN)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_COPYOUT)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_CREATE)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_DEFAULT)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_DEVICEPTR)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_IF)			\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_NO_CREATE)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_NUM_GANGS)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_NUM_WORKERS)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_PRESENT)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_VECTOR_LENGTH)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_WAIT) )

#define OACC_PARALLEL_CLAUSE_MASK					\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_ASYNC)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_ATTACH)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_COPY)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_COPYIN)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_COPYOUT)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_CREATE)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_DEFAULT)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_DEVICEPTR)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_IF)			\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_NO_CREATE)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_PRIVATE)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_FIRSTPRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_NUM_GANGS)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_NUM_WORKERS)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_PRESENT)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_REDUCTION)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_VECTOR_LENGTH)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_WAIT) )

#define OACC_SERIAL_CLAUSE_MASK					\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_ASYNC)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_ATTACH)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_COPY)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_COPYIN)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_COPYOUT)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_CREATE)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_DEFAULT)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_DEVICEPTR)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_IF)			\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_NO_CREATE)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_PRIVATE)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_FIRSTPRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_PRESENT)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_REDUCTION)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_WAIT) )

static tree
c_parser_oacc_compute (location_t loc, c_parser *parser,
		       enum pragma_kind p_kind, char *p_name, bool *if_p)
{
  omp_clause_mask mask;
  enum tree_code code;
  switch (p_kind)
    {
    case PRAGMA_OACC_KERNELS:
      strcat (p_name, " kernels");
      mask = OACC_KERNELS_CLAUSE_MASK;
      code = OACC_KERNELS;
      break;
    case PRAGMA_OACC_PARALLEL:
      strcat (p_name, " parallel");
      mask = OACC_PARALLEL_CLAUSE_MASK;
      code = OACC_PARALLEL;
      break;
    case PRAGMA_OACC_SERIAL:
      strcat (p_name, " serial");
      mask = OACC_SERIAL_CLAUSE_MASK;
      code = OACC_SERIAL;
      break;
    default:
      gcc_unreachable ();
    }

  if (c_parser_next_token_is (parser, CPP_NAME))
    {
      const char *p = IDENTIFIER_POINTER (c_parser_peek_token (parser)->value);
      if (strcmp (p, "loop") == 0)
	{
	  c_parser_consume_token (parser);
	  tree block = c_begin_omp_parallel ();
	  tree clauses;
	  c_parser_oacc_loop (loc, parser, p_name, mask, &clauses, if_p);
	  return c_finish_omp_construct (loc, code, block, clauses);
	}
    }

  tree clauses = c_parser_oacc_all_clauses (parser, mask, p_name);

  tree block = c_begin_omp_parallel ();
  add_stmt (c_parser_omp_structured_block (parser, if_p));

  return c_finish_omp_construct (loc, code, block, clauses);
}

/* OpenACC 2.0:
   # pragma acc routine oacc-routine-clause[optseq] new-line
     function-definition

   # pragma acc routine ( name ) oacc-routine-clause[optseq] new-line
*/

#define OACC_ROUTINE_CLAUSE_MASK					\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_GANG)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_WORKER)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_VECTOR)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_SEQ) )

/* Parse an OpenACC routine directive.  For named directives, we apply
   immediately to the named function.  For unnamed ones we then parse
   a declaration or definition, which must be for a function.  */

static void
c_parser_oacc_routine (c_parser *parser, enum pragma_context context)
{
  gcc_checking_assert (context == pragma_external);

  oacc_routine_data data;
  data.error_seen = false;
  data.fndecl_seen = false;
  data.loc = c_parser_peek_token (parser)->location;

  c_parser_consume_pragma (parser);

  /* Look for optional '( name )'.  */
  if (c_parser_next_token_is (parser, CPP_OPEN_PAREN))
    {
      c_parser_consume_token (parser); /* '(' */

      tree decl = NULL_TREE;
      c_token *name_token = c_parser_peek_token (parser);
      location_t name_loc = name_token->location;
      if (name_token->type == CPP_NAME
	  && (name_token->id_kind == C_ID_ID
	      || name_token->id_kind == C_ID_TYPENAME))
	{
	  decl = lookup_name (name_token->value);
	  if (!decl)
	    error_at (name_loc,
		      "%qE has not been declared", name_token->value);
	  c_parser_consume_token (parser);
	}
      else
	c_parser_error (parser, "expected function name");

      if (!decl
	  || !c_parser_require (parser, CPP_CLOSE_PAREN, "expected %<)%>"))
	{
	  c_parser_skip_to_pragma_eol (parser, false);
	  return;
	}

      data.clauses
	= c_parser_oacc_all_clauses (parser, OACC_ROUTINE_CLAUSE_MASK,
				     "#pragma acc routine");
      /* The clauses are in reverse order; fix that to make later diagnostic
	 emission easier.  */
      data.clauses = nreverse (data.clauses);

      if (TREE_CODE (decl) != FUNCTION_DECL)
	{
	  error_at (name_loc, "%qD does not refer to a function", decl);
	  return;
	}

      c_finish_oacc_routine (&data, decl, false);
    }
  else /* No optional '( name )'.  */
    {
      data.clauses
	= c_parser_oacc_all_clauses (parser, OACC_ROUTINE_CLAUSE_MASK,
				     "#pragma acc routine");
      /* The clauses are in reverse order; fix that to make later diagnostic
	 emission easier.  */
      data.clauses = nreverse (data.clauses);

      /* Emit a helpful diagnostic if there's another pragma following this
	 one.  Also don't allow a static assertion declaration, as in the
	 following we'll just parse a *single* "declaration or function
	 definition", and the static assertion counts an one.  */
      if (c_parser_next_token_is (parser, CPP_PRAGMA)
	  || c_parser_next_token_is_keyword (parser, RID_STATIC_ASSERT))
	{
	  error_at (data.loc,
		    "%<#pragma acc routine%> not immediately followed by"
		    " function declaration or definition");
	  /* ..., and then just keep going.  */
	  return;
	}

      /* We only have to consider the pragma_external case here.  */
      if (c_parser_next_token_is (parser, CPP_KEYWORD)
	  && c_parser_peek_token (parser)->keyword == RID_EXTENSION)
	{
	  int ext = disable_extension_diagnostics ();
	  do
	    c_parser_consume_token (parser);
	  while (c_parser_next_token_is (parser, CPP_KEYWORD)
		 && c_parser_peek_token (parser)->keyword == RID_EXTENSION);
	  c_parser_declaration_or_fndef (parser, true, true, true, false, true,
					 NULL, vNULL, false, NULL, &data);
	  restore_extension_diagnostics (ext);
	}
      else
	c_parser_declaration_or_fndef (parser, true, true, true, false, true,
				       NULL, vNULL, false, NULL, &data);
    }
}

/* Finalize an OpenACC routine pragma, applying it to FNDECL.
   IS_DEFN is true if we're applying it to the definition.  */

static void
c_finish_oacc_routine (struct oacc_routine_data *data, tree fndecl,
		       bool is_defn)
{
  /* Keep going if we're in error reporting mode.  */
  if (data->error_seen
      || fndecl == error_mark_node)
    return;

  if (data->fndecl_seen)
    {
      error_at (data->loc,
		"%<#pragma acc routine%> not immediately followed by"
		" a single function declaration or definition");
      data->error_seen = true;
      return;
    }
  if (fndecl == NULL_TREE || TREE_CODE (fndecl) != FUNCTION_DECL)
    {
      error_at (data->loc,
		"%<#pragma acc routine%> not immediately followed by"
		" function declaration or definition");
      data->error_seen = true;
      return;
    }

  int compatible
    = oacc_verify_routine_clauses (fndecl, &data->clauses, data->loc,
				   "#pragma acc routine");
  if (compatible < 0)
    {
      data->error_seen = true;
      return;
    }
  if (compatible > 0)
    {
    }
  else
    {
      if (TREE_USED (fndecl) || (!is_defn && DECL_SAVED_TREE (fndecl)))
	{
	  error_at (data->loc,
		    TREE_USED (fndecl)
		    ? G_("%<#pragma acc routine%> must be applied before use")
		    : G_("%<#pragma acc routine%> must be applied before"
			 " definition"));
	  data->error_seen = true;
	  return;
	}

      /* Set the routine's level of parallelism.  */
      tree dims = oacc_build_routine_dims (data->clauses);
      oacc_replace_fn_attrib (fndecl, dims);

      /* Add an "omp declare target" attribute.  */
      DECL_ATTRIBUTES (fndecl)
	= tree_cons (get_identifier ("omp declare target"),
		     data->clauses, DECL_ATTRIBUTES (fndecl));
    }

  /* Remember that we've used this "#pragma acc routine".  */
  data->fndecl_seen = true;
}

/* OpenACC 2.0:
   # pragma acc update oacc-update-clause[optseq] new-line
*/

#define OACC_UPDATE_CLAUSE_MASK						\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_ASYNC)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_DEVICE)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_HOST)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_IF)			\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_IF_PRESENT)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_WAIT) )

static void
c_parser_oacc_update (c_parser *parser)
{
  location_t loc = c_parser_peek_token (parser)->location;

  c_parser_consume_pragma (parser);

  tree clauses = c_parser_oacc_all_clauses (parser, OACC_UPDATE_CLAUSE_MASK,
					    "#pragma acc update");
  if (omp_find_clause (clauses, OMP_CLAUSE_MAP) == NULL_TREE)
    {
      error_at (loc,
		"%<#pragma acc update%> must contain at least one "
		"%<device%> or %<host%> or %<self%> clause");
      return;
    }

  if (parser->error)
    return;

  tree stmt = make_node (OACC_UPDATE);
  TREE_TYPE (stmt) = void_type_node;
  OACC_UPDATE_CLAUSES (stmt) = clauses;
  SET_EXPR_LOCATION (stmt, loc);
  add_stmt (stmt);
}

/* OpenACC 2.0:
   # pragma acc wait [(intseq)] oacc-wait-clause[optseq] new-line

   LOC is the location of the #pragma token.
*/

#define OACC_WAIT_CLAUSE_MASK						\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OACC_CLAUSE_ASYNC) )

static tree
c_parser_oacc_wait (location_t loc, c_parser *parser, char *p_name)
{
  tree clauses, list = NULL_TREE, stmt = NULL_TREE;

  if (c_parser_peek_token (parser)->type == CPP_OPEN_PAREN)
    list = c_parser_oacc_wait_list (parser, loc, list);

  strcpy (p_name, " wait");
  clauses = c_parser_oacc_all_clauses (parser, OACC_WAIT_CLAUSE_MASK, p_name);
  stmt = c_finish_oacc_wait (loc, list, clauses);
  add_stmt (stmt);

  return stmt;
}

/* OpenMP 5.0:
   # pragma omp allocate (list)  [allocator(allocator)]  */

static void
c_parser_omp_allocate (location_t loc, c_parser *parser)
{
  tree allocator = NULL_TREE;
  tree nl = c_parser_omp_var_list_parens (parser, OMP_CLAUSE_ALLOCATE, NULL_TREE);
  if (c_parser_next_token_is (parser, CPP_NAME))
    {
      matching_parens parens;
      const char *p = IDENTIFIER_POINTER (c_parser_peek_token (parser)->value);
      c_parser_consume_token (parser);
      if (strcmp ("allocator", p) != 0)
	error_at (c_parser_peek_token (parser)->location,
		  "expected %<allocator%>");
      else if (parens.require_open (parser))
	{
	  location_t expr_loc = c_parser_peek_token (parser)->location;
	  c_expr expr = c_parser_expr_no_commas (parser, NULL);
	  expr = convert_lvalue_to_rvalue (expr_loc, expr, false, true);
	  allocator = expr.value;
	  allocator = c_fully_fold (allocator, false, NULL);
	  tree orig_type
	    = expr.original_type ? expr.original_type : TREE_TYPE (allocator);
	  orig_type = TYPE_MAIN_VARIANT (orig_type);
	  if (!INTEGRAL_TYPE_P (TREE_TYPE (allocator))
	      || TREE_CODE (orig_type) != ENUMERAL_TYPE
	      || TYPE_NAME (orig_type)
		 != get_identifier ("omp_allocator_handle_t"))
	    {
	      error_at (expr_loc, "%<allocator%> clause allocator expression "
				"has type %qT rather than "
				"%<omp_allocator_handle_t%>",
				TREE_TYPE (allocator));
	      allocator = NULL_TREE;
	    }
	  parens.skip_until_found_close (parser);
	}
    }
  c_parser_skip_to_pragma_eol (parser);

  if (allocator)
    for (tree c = nl; c != NULL_TREE; c = OMP_CLAUSE_CHAIN (c))
      OMP_CLAUSE_ALLOCATE_ALLOCATOR (c) = allocator;

  sorry_at (loc, "%<#pragma omp allocate%> not yet supported");
}

/* OpenMP 2.5:
   # pragma omp atomic new-line
     expression-stmt

   expression-stmt:
     x binop= expr | x++ | ++x | x-- | --x
   binop:
     +, *, -, /, &, ^, |, <<, >>

  where x is an lvalue expression with scalar type.

   OpenMP 3.1:
   # pragma omp atomic new-line
     update-stmt

   # pragma omp atomic read new-line
     read-stmt

   # pragma omp atomic write new-line
     write-stmt

   # pragma omp atomic update new-line
     update-stmt

   # pragma omp atomic capture new-line
     capture-stmt

   # pragma omp atomic capture new-line
     capture-block

   read-stmt:
     v = x
   write-stmt:
     x = expr
   update-stmt:
     expression-stmt | x = x binop expr
   capture-stmt:
     v = expression-stmt
   capture-block:
     { v = x; update-stmt; } | { update-stmt; v = x; }

   OpenMP 4.0:
   update-stmt:
     expression-stmt | x = x binop expr | x = expr binop x
   capture-stmt:
     v = update-stmt
   capture-block:
     { v = x; update-stmt; } | { update-stmt; v = x; } | { v = x; x = expr; }

  where x and v are lvalue expressions with scalar type.

  LOC is the location of the #pragma token.  */

static void
c_parser_omp_atomic (location_t loc, c_parser *parser, bool openacc)
{
  tree lhs = NULL_TREE, rhs = NULL_TREE, v = NULL_TREE;
  tree lhs1 = NULL_TREE, rhs1 = NULL_TREE;
  tree stmt, orig_lhs, unfolded_lhs = NULL_TREE, unfolded_lhs1 = NULL_TREE;
  enum tree_code code = ERROR_MARK, opcode = NOP_EXPR;
  enum omp_memory_order memory_order = OMP_MEMORY_ORDER_UNSPECIFIED;
  struct c_expr expr;
  location_t eloc;
  bool structured_block = false;
  bool swapped = false;
  bool non_lvalue_p;
  bool first = true;
  tree clauses = NULL_TREE;

  while (c_parser_next_token_is_not (parser, CPP_PRAGMA_EOL))
    {
      if (!first && c_parser_next_token_is (parser, CPP_COMMA))
	c_parser_consume_token (parser);

      first = false;

      if (c_parser_next_token_is (parser, CPP_NAME))
	{
	  const char *p
	    = IDENTIFIER_POINTER (c_parser_peek_token (parser)->value);
	  location_t cloc = c_parser_peek_token (parser)->location;
	  enum tree_code new_code = ERROR_MARK;
	  enum omp_memory_order new_memory_order
	    = OMP_MEMORY_ORDER_UNSPECIFIED;

	  if (!strcmp (p, "read"))
	    new_code = OMP_ATOMIC_READ;
	  else if (!strcmp (p, "write"))
	    new_code = NOP_EXPR;
	  else if (!strcmp (p, "update"))
	    new_code = OMP_ATOMIC;
	  else if (!strcmp (p, "capture"))
	    new_code = OMP_ATOMIC_CAPTURE_NEW;
	  else if (openacc)
	    {
	      p = NULL;
	      error_at (cloc, "expected %<read%>, %<write%>, %<update%>, "
			      "or %<capture%> clause");
	    }
	  else if (!strcmp (p, "seq_cst"))
	    new_memory_order = OMP_MEMORY_ORDER_SEQ_CST;
	  else if (!strcmp (p, "acq_rel"))
	    new_memory_order = OMP_MEMORY_ORDER_ACQ_REL;
	  else if (!strcmp (p, "release"))
	    new_memory_order = OMP_MEMORY_ORDER_RELEASE;
	  else if (!strcmp (p, "acquire"))
	    new_memory_order = OMP_MEMORY_ORDER_ACQUIRE;
	  else if (!strcmp (p, "relaxed"))
	    new_memory_order = OMP_MEMORY_ORDER_RELAXED;
	  else if (!strcmp (p, "hint"))
	    {
	      c_parser_consume_token (parser);
	      clauses = c_parser_omp_clause_hint (parser, clauses);
	      continue;
	    }
	  else
	    {
	      p = NULL;
	      error_at (cloc, "expected %<read%>, %<write%>, %<update%>, "
			      "%<capture%>, %<seq_cst%>, %<acq_rel%>, "
			      "%<release%>, %<relaxed%> or %<hint%> clause");
	    }
	  if (p)
	    {
	      if (new_code != ERROR_MARK)
		{
		  /* OpenACC permits 'update capture'.  */
		  if (openacc
		      && code == OMP_ATOMIC
		      && new_code == OMP_ATOMIC_CAPTURE_NEW)
		    code = new_code;
		  else if (code != ERROR_MARK)
		    error_at (cloc, "too many atomic clauses");
		  else
		    code = new_code;
		}
	      else if (new_memory_order != OMP_MEMORY_ORDER_UNSPECIFIED)
		{
		  if (memory_order != OMP_MEMORY_ORDER_UNSPECIFIED)
		    error_at (cloc, "too many memory order clauses");
		  else
		    memory_order = new_memory_order;
		}
	      c_parser_consume_token (parser);
	      continue;
	    }
	}
      break;
    }
  c_parser_skip_to_pragma_eol (parser);

  if (code == ERROR_MARK)
    code = OMP_ATOMIC;
  if (openacc)
    memory_order = OMP_MEMORY_ORDER_RELAXED;
  else if (memory_order == OMP_MEMORY_ORDER_UNSPECIFIED)
    {
      omp_requires_mask
	= (enum omp_requires) (omp_requires_mask
			       | OMP_REQUIRES_ATOMIC_DEFAULT_MEM_ORDER_USED);
      switch ((enum omp_memory_order)
	      (omp_requires_mask & OMP_REQUIRES_ATOMIC_DEFAULT_MEM_ORDER))
	{
	case OMP_MEMORY_ORDER_UNSPECIFIED:
	case OMP_MEMORY_ORDER_RELAXED:
	  memory_order = OMP_MEMORY_ORDER_RELAXED;
	  break;
	case OMP_MEMORY_ORDER_SEQ_CST:
	  memory_order = OMP_MEMORY_ORDER_SEQ_CST;
	  break;
	case OMP_MEMORY_ORDER_ACQ_REL:
	  switch (code)
	    {
	    case OMP_ATOMIC_READ:
	      memory_order = OMP_MEMORY_ORDER_ACQUIRE;
	      break;
	    case NOP_EXPR: /* atomic write */
	    case OMP_ATOMIC:
	      memory_order = OMP_MEMORY_ORDER_RELEASE;
	      break;
	    default:
	      memory_order = OMP_MEMORY_ORDER_ACQ_REL;
	      break;
	    }
	  break;
	default:
	  gcc_unreachable ();
	}
    }
  else
    switch (code)
      {
      case OMP_ATOMIC_READ:
	if (memory_order == OMP_MEMORY_ORDER_ACQ_REL
	    || memory_order == OMP_MEMORY_ORDER_RELEASE)
	  {
	    error_at (loc, "%<#pragma omp atomic read%> incompatible with "
			   "%<acq_rel%> or %<release%> clauses");
	    memory_order = OMP_MEMORY_ORDER_SEQ_CST;
	  }
	break;
      case NOP_EXPR: /* atomic write */
	if (memory_order == OMP_MEMORY_ORDER_ACQ_REL
	    || memory_order == OMP_MEMORY_ORDER_ACQUIRE)
	  {
	    error_at (loc, "%<#pragma omp atomic write%> incompatible with "
			   "%<acq_rel%> or %<acquire%> clauses");
	    memory_order = OMP_MEMORY_ORDER_SEQ_CST;
	  }
	break;
      case OMP_ATOMIC:
     /* case OMP_ATOMIC_CAPTURE_NEW: - or update to OpenMP 5.1 */
	if (memory_order == OMP_MEMORY_ORDER_ACQ_REL
	    || memory_order == OMP_MEMORY_ORDER_ACQUIRE)
	  {
	    error_at (loc, "%<#pragma omp atomic update%> incompatible with "
			   "%<acq_rel%> or %<acquire%> clauses");
	    memory_order = OMP_MEMORY_ORDER_SEQ_CST;
	  }
	break;
      default:
	break;
      }

  switch (code)
    {
    case OMP_ATOMIC_READ:
    case NOP_EXPR: /* atomic write */
      v = c_parser_cast_expression (parser, NULL).value;
      non_lvalue_p = !lvalue_p (v);
      v = c_fully_fold (v, false, NULL, true);
      if (v == error_mark_node)
	goto saw_error;
      if (non_lvalue_p)
	v = non_lvalue (v);
      loc = c_parser_peek_token (parser)->location;
      if (!c_parser_require (parser, CPP_EQ, "expected %<=%>"))
	goto saw_error;
      if (code == NOP_EXPR)
	{
	  lhs = c_parser_expression (parser).value;
	  lhs = c_fully_fold (lhs, false, NULL);
	  if (lhs == error_mark_node)
	    goto saw_error;
	}
      else
	{
	  lhs = c_parser_cast_expression (parser, NULL).value;
	  non_lvalue_p = !lvalue_p (lhs);
	  lhs = c_fully_fold (lhs, false, NULL, true);
	  if (lhs == error_mark_node)
	    goto saw_error;
	  if (non_lvalue_p)
	    lhs = non_lvalue (lhs);
	}
      if (code == NOP_EXPR)
	{
	  /* atomic write is represented by OMP_ATOMIC with NOP_EXPR
	     opcode.  */
	  code = OMP_ATOMIC;
	  rhs = lhs;
	  lhs = v;
	  v = NULL_TREE;
	}
      goto done;
    case OMP_ATOMIC_CAPTURE_NEW:
      if (c_parser_next_token_is (parser, CPP_OPEN_BRACE))
	{
	  c_parser_consume_token (parser);
	  structured_block = true;
	}
      else
	{
	  v = c_parser_cast_expression (parser, NULL).value;
	  non_lvalue_p = !lvalue_p (v);
	  v = c_fully_fold (v, false, NULL, true);
	  if (v == error_mark_node)
	    goto saw_error;
	  if (non_lvalue_p)
	    v = non_lvalue (v);
	  if (!c_parser_require (parser, CPP_EQ, "expected %<=%>"))
	    goto saw_error;
	}
      break;
    default:
      break;
    }

  /* For structured_block case we don't know yet whether
     old or new x should be captured.  */
restart:
  eloc = c_parser_peek_token (parser)->location;
  expr = c_parser_cast_expression (parser, NULL);
  lhs = expr.value;
  expr = default_function_array_conversion (eloc, expr);
  unfolded_lhs = expr.value;
  lhs = c_fully_fold (lhs, false, NULL, true);
  orig_lhs = lhs;
  switch (TREE_CODE (lhs))
    {
    case ERROR_MARK:
    saw_error:
      c_parser_skip_to_end_of_block_or_statement (parser);
      if (structured_block)
	{
	  if (c_parser_next_token_is (parser, CPP_CLOSE_BRACE))
	    c_parser_consume_token (parser);
	  else if (code == OMP_ATOMIC_CAPTURE_NEW)
	    {
	      c_parser_skip_to_end_of_block_or_statement (parser);
	      if (c_parser_next_token_is (parser, CPP_CLOSE_BRACE))
		c_parser_consume_token (parser);
	    }
	}
      return;

    case POSTINCREMENT_EXPR:
      if (code == OMP_ATOMIC_CAPTURE_NEW && !structured_block)
	code = OMP_ATOMIC_CAPTURE_OLD;
      /* FALLTHROUGH */
    case PREINCREMENT_EXPR:
      lhs = TREE_OPERAND (lhs, 0);
      unfolded_lhs = NULL_TREE;
      opcode = PLUS_EXPR;
      rhs = integer_one_node;
      break;

    case POSTDECREMENT_EXPR:
      if (code == OMP_ATOMIC_CAPTURE_NEW && !structured_block)
	code = OMP_ATOMIC_CAPTURE_OLD;
      /* FALLTHROUGH */
    case PREDECREMENT_EXPR:
      lhs = TREE_OPERAND (lhs, 0);
      unfolded_lhs = NULL_TREE;
      opcode = MINUS_EXPR;
      rhs = integer_one_node;
      break;

    case COMPOUND_EXPR:
      if (TREE_CODE (TREE_OPERAND (lhs, 0)) == SAVE_EXPR
	  && TREE_CODE (TREE_OPERAND (lhs, 1)) == COMPOUND_EXPR
	  && TREE_CODE (TREE_OPERAND (TREE_OPERAND (lhs, 1), 0)) == MODIFY_EXPR
	  && TREE_OPERAND (TREE_OPERAND (lhs, 1), 1) == TREE_OPERAND (lhs, 0)
	  && TREE_CODE (TREE_TYPE (TREE_OPERAND (TREE_OPERAND
					      (TREE_OPERAND (lhs, 1), 0), 0)))
	     == BOOLEAN_TYPE)
	/* Undo effects of boolean_increment for post {in,de}crement.  */
	lhs = TREE_OPERAND (TREE_OPERAND (lhs, 1), 0);
      /* FALLTHRU */
    case MODIFY_EXPR:
      if (TREE_CODE (lhs) == MODIFY_EXPR
	  && TREE_CODE (TREE_TYPE (TREE_OPERAND (lhs, 0))) == BOOLEAN_TYPE)
	{
	  /* Undo effects of boolean_increment.  */
	  if (integer_onep (TREE_OPERAND (lhs, 1)))
	    {
	      /* This is pre or post increment.  */
	      rhs = TREE_OPERAND (lhs, 1);
	      lhs = TREE_OPERAND (lhs, 0);
	      unfolded_lhs = NULL_TREE;
	      opcode = NOP_EXPR;
	      if (code == OMP_ATOMIC_CAPTURE_NEW
		  && !structured_block
		  && TREE_CODE (orig_lhs) == COMPOUND_EXPR)
		code = OMP_ATOMIC_CAPTURE_OLD;
	      break;
	    }
	  if (TREE_CODE (TREE_OPERAND (lhs, 1)) == TRUTH_NOT_EXPR
	      && TREE_OPERAND (lhs, 0)
		 == TREE_OPERAND (TREE_OPERAND (lhs, 1), 0))
	    {
	      /* This is pre or post decrement.  */
	      rhs = TREE_OPERAND (lhs, 1);
	      lhs = TREE_OPERAND (lhs, 0);
	      unfolded_lhs = NULL_TREE;
	      opcode = NOP_EXPR;
	      if (code == OMP_ATOMIC_CAPTURE_NEW
		  && !structured_block
		  && TREE_CODE (orig_lhs) == COMPOUND_EXPR)
		code = OMP_ATOMIC_CAPTURE_OLD;
	      break;
	    }
	}
      /* FALLTHRU */
    default:
      if (!lvalue_p (unfolded_lhs))
	lhs = non_lvalue (lhs);
      switch (c_parser_peek_token (parser)->type)
	{
	case CPP_MULT_EQ:
	  opcode = MULT_EXPR;
	  break;
	case CPP_DIV_EQ:
	  opcode = TRUNC_DIV_EXPR;
	  break;
	case CPP_PLUS_EQ:
	  opcode = PLUS_EXPR;
	  break;
	case CPP_MINUS_EQ:
	  opcode = MINUS_EXPR;
	  break;
	case CPP_LSHIFT_EQ:
	  opcode = LSHIFT_EXPR;
	  break;
	case CPP_RSHIFT_EQ:
	  opcode = RSHIFT_EXPR;
	  break;
	case CPP_AND_EQ:
	  opcode = BIT_AND_EXPR;
	  break;
	case CPP_OR_EQ:
	  opcode = BIT_IOR_EXPR;
	  break;
	case CPP_XOR_EQ:
	  opcode = BIT_XOR_EXPR;
	  break;
	case CPP_EQ:
	  c_parser_consume_token (parser);
	  eloc = c_parser_peek_token (parser)->location;
	  expr = c_parser_expr_no_commas (parser, NULL, unfolded_lhs);
	  rhs1 = expr.value;
	  switch (TREE_CODE (rhs1))
	    {
	    case MULT_EXPR:
	    case TRUNC_DIV_EXPR:
	    case RDIV_EXPR:
	    case PLUS_EXPR:
	    case MINUS_EXPR:
	    case LSHIFT_EXPR:
	    case RSHIFT_EXPR:
	    case BIT_AND_EXPR:
	    case BIT_IOR_EXPR:
	    case BIT_XOR_EXPR:
	      if (c_tree_equal (TREE_OPERAND (rhs1, 0), unfolded_lhs))
		{
		  opcode = TREE_CODE (rhs1);
		  rhs = c_fully_fold (TREE_OPERAND (rhs1, 1), false, NULL,
				      true);
		  rhs1 = c_fully_fold (TREE_OPERAND (rhs1, 0), false, NULL,
				       true);
		  goto stmt_done;
		}
	      if (c_tree_equal (TREE_OPERAND (rhs1, 1), unfolded_lhs))
		{
		  opcode = TREE_CODE (rhs1);
		  rhs = c_fully_fold (TREE_OPERAND (rhs1, 0), false, NULL,
				      true);
		  rhs1 = c_fully_fold (TREE_OPERAND (rhs1, 1), false, NULL,
				       true);
		  swapped = !commutative_tree_code (opcode);
		  goto stmt_done;
		}
	      break;
	    case ERROR_MARK:
	      goto saw_error;
	    default:
	      break;
	    }
	  if (c_parser_peek_token (parser)->type == CPP_SEMICOLON)
	    {
	      if (structured_block && code == OMP_ATOMIC_CAPTURE_NEW)
		{
		  code = OMP_ATOMIC_CAPTURE_OLD;
		  v = lhs;
		  lhs = NULL_TREE;
		  expr = default_function_array_read_conversion (eloc, expr);
		  unfolded_lhs1 = expr.value;
		  lhs1 = c_fully_fold (unfolded_lhs1, false, NULL, true);
		  rhs1 = NULL_TREE;
		  c_parser_consume_token (parser);
		  goto restart;
		}
	      if (structured_block)
		{
		  opcode = NOP_EXPR;
		  expr = default_function_array_read_conversion (eloc, expr);
		  rhs = c_fully_fold (expr.value, false, NULL, true);
		  rhs1 = NULL_TREE;
		  goto stmt_done;
		}
	    }
	  c_parser_error (parser, "invalid form of %<#pragma omp atomic%>");
	  goto saw_error;
	default:
	  c_parser_error (parser,
			  "invalid operator for %<#pragma omp atomic%>");
	  goto saw_error;
	}

      /* Arrange to pass the location of the assignment operator to
	 c_finish_omp_atomic.  */
      loc = c_parser_peek_token (parser)->location;
      c_parser_consume_token (parser);
      eloc = c_parser_peek_token (parser)->location;
      expr = c_parser_expression (parser);
      expr = default_function_array_read_conversion (eloc, expr);
      rhs = expr.value;
      rhs = c_fully_fold (rhs, false, NULL, true);
      break;
    }
stmt_done:
  if (structured_block && code == OMP_ATOMIC_CAPTURE_NEW)
    {
      if (!c_parser_require (parser, CPP_SEMICOLON, "expected %<;%>"))
	goto saw_error;
      v = c_parser_cast_expression (parser, NULL).value;
      non_lvalue_p = !lvalue_p (v);
      v = c_fully_fold (v, false, NULL, true);
      if (v == error_mark_node)
	goto saw_error;
      if (non_lvalue_p)
	v = non_lvalue (v);
      if (!c_parser_require (parser, CPP_EQ, "expected %<=%>"))
	goto saw_error;
      eloc = c_parser_peek_token (parser)->location;
      expr = c_parser_cast_expression (parser, NULL);
      lhs1 = expr.value;
      expr = default_function_array_read_conversion (eloc, expr);
      unfolded_lhs1 = expr.value;
      lhs1 = c_fully_fold (lhs1, false, NULL, true);
      if (lhs1 == error_mark_node)
	goto saw_error;
      if (!lvalue_p (unfolded_lhs1))
	lhs1 = non_lvalue (lhs1);
    }
  if (structured_block)
    {
      c_parser_skip_until_found (parser, CPP_SEMICOLON, "expected %<;%>");
      c_parser_require (parser, CPP_CLOSE_BRACE, "expected %<}%>");
    }
done:
  if (unfolded_lhs && unfolded_lhs1
      && !c_tree_equal (unfolded_lhs, unfolded_lhs1))
    {
      error ("%<#pragma omp atomic capture%> uses two different "
	     "expressions for memory");
      stmt = error_mark_node;
    }
  else
    stmt = c_finish_omp_atomic (loc, code, opcode, lhs, rhs, v, lhs1, rhs1,
				swapped, memory_order);
  if (stmt != error_mark_node)
    add_stmt (stmt);

  if (!structured_block)
    c_parser_skip_until_found (parser, CPP_SEMICOLON, "expected %<;%>");
}


/* OpenMP 2.5:
   # pragma omp barrier new-line
*/

static void
c_parser_omp_barrier (c_parser *parser)
{
  location_t loc = c_parser_peek_token (parser)->location;
  c_parser_consume_pragma (parser);
  c_parser_skip_to_pragma_eol (parser);

  c_finish_omp_barrier (loc);
}

/* OpenMP 2.5:
   # pragma omp critical [(name)] new-line
     structured-block

   OpenMP 4.5:
   # pragma omp critical [(name) [hint(expression)]] new-line

  LOC is the location of the #pragma itself.  */

#define OMP_CRITICAL_CLAUSE_MASK		\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_HINT) )

static tree
c_parser_omp_critical (location_t loc, c_parser *parser, bool *if_p)
{
  tree stmt, name = NULL_TREE, clauses = NULL_TREE;

  if (c_parser_next_token_is (parser, CPP_OPEN_PAREN))
    {
      c_parser_consume_token (parser);
      if (c_parser_next_token_is (parser, CPP_NAME))
	{
	  name = c_parser_peek_token (parser)->value;
	  c_parser_consume_token (parser);
	  c_parser_require (parser, CPP_CLOSE_PAREN, "expected %<)%>");
	}
      else
	c_parser_error (parser, "expected identifier");

      if (c_parser_next_token_is (parser, CPP_COMMA)
	  && c_parser_peek_2nd_token (parser)->type == CPP_NAME)
	c_parser_consume_token (parser);
    }
  clauses = c_parser_omp_all_clauses (parser, OMP_CRITICAL_CLAUSE_MASK,
				      "#pragma omp critical");
  stmt = c_parser_omp_structured_block (parser, if_p);
  return c_finish_omp_critical (loc, stmt, name, clauses);
}

/* OpenMP 5.0:
   # pragma omp depobj ( depobj ) depobj-clause new-line

   depobj-clause:
     depend (dependence-type : locator)
     destroy
     update (dependence-type)

   dependence-type:
     in
     out
     inout
     mutexinout  */

static void
c_parser_omp_depobj (c_parser *parser)
{
  location_t loc = c_parser_peek_token (parser)->location;
  c_parser_consume_pragma (parser);
  matching_parens parens;
  if (!parens.require_open (parser))
    {
      c_parser_skip_to_pragma_eol (parser);
      return;
    }

  tree depobj = c_parser_expr_no_commas (parser, NULL).value;
  if (depobj != error_mark_node)
    {
      if (!lvalue_p (depobj))
	{
	  error_at (EXPR_LOC_OR_LOC (depobj, loc),
		    "%<depobj%> expression is not lvalue expression");
	  depobj = error_mark_node;
	}
      else
	{
	  tree addr = build_unary_op (EXPR_LOC_OR_LOC (depobj, loc), ADDR_EXPR,
				      depobj, false);
	  if (addr == error_mark_node)
	    depobj = error_mark_node;
	  else
	    depobj = build_indirect_ref (EXPR_LOC_OR_LOC (depobj, loc),
					 addr, RO_UNARY_STAR);
	}
    }

  parens.skip_until_found_close (parser);
  tree clause = NULL_TREE;
  enum omp_clause_depend_kind kind = OMP_CLAUSE_DEPEND_SOURCE;
  location_t c_loc = c_parser_peek_token (parser)->location;
  if (c_parser_next_token_is (parser, CPP_NAME))
    {
      const char *p = IDENTIFIER_POINTER (c_parser_peek_token (parser)->value);

      c_parser_consume_token (parser);
      if (!strcmp ("depend", p))
	{
	  clause = c_parser_omp_clause_depend (parser, NULL_TREE);
	  clause = c_finish_omp_clauses (clause, C_ORT_OMP);
	  if (!clause)
	    clause = error_mark_node;
	}
      else if (!strcmp ("destroy", p))
	kind = OMP_CLAUSE_DEPEND_LAST;
      else if (!strcmp ("update", p))
	{
	  matching_parens c_parens;
	  if (c_parens.require_open (parser))
	    {
	      location_t c2_loc = c_parser_peek_token (parser)->location;
	      if (c_parser_next_token_is (parser, CPP_NAME))
		{
		  const char *p2
		    = IDENTIFIER_POINTER (c_parser_peek_token (parser)->value);

		  c_parser_consume_token (parser);
		  if (!strcmp ("in", p2))
		    kind = OMP_CLAUSE_DEPEND_IN;
		  else if (!strcmp ("out", p2))
		    kind = OMP_CLAUSE_DEPEND_OUT;
		  else if (!strcmp ("inout", p2))
		    kind = OMP_CLAUSE_DEPEND_INOUT;
		  else if (!strcmp ("mutexinoutset", p2))
		    kind = OMP_CLAUSE_DEPEND_MUTEXINOUTSET;
		}
	      if (kind == OMP_CLAUSE_DEPEND_SOURCE)
		{
		  clause = error_mark_node;
		  error_at (c2_loc, "expected %<in%>, %<out%>, %<inout%> or "
				    "%<mutexinoutset%>");
		}
	      c_parens.skip_until_found_close (parser);
	    }
	  else
	    clause = error_mark_node;
	}
    }
  if (!clause && kind == OMP_CLAUSE_DEPEND_SOURCE)
    {
      clause = error_mark_node;
      error_at (c_loc, "expected %<depend%>, %<destroy%> or %<update%> clause");
    }
  c_parser_skip_to_pragma_eol (parser);

  c_finish_omp_depobj (loc, depobj, kind, clause);
}


/* OpenMP 2.5:
   # pragma omp flush flush-vars[opt] new-line

   flush-vars:
     ( variable-list )

   OpenMP 5.0:
   # pragma omp flush memory-order-clause new-line  */

static void
c_parser_omp_flush (c_parser *parser)
{
  location_t loc = c_parser_peek_token (parser)->location;
  c_parser_consume_pragma (parser);
  enum memmodel mo = MEMMODEL_LAST;
  if (c_parser_next_token_is (parser, CPP_NAME))
    {
      const char *p
	= IDENTIFIER_POINTER (c_parser_peek_token (parser)->value);

      if (!strcmp (p, "acq_rel"))
	mo = MEMMODEL_ACQ_REL;
      else if (!strcmp (p, "release"))
	mo = MEMMODEL_RELEASE;
      else if (!strcmp (p, "acquire"))
	mo = MEMMODEL_ACQUIRE;
      else
	error_at (c_parser_peek_token (parser)->location,
		  "expected %<acq_rel%>, %<release%> or %<acquire%>");
      c_parser_consume_token (parser);
    }
  if (c_parser_next_token_is (parser, CPP_OPEN_PAREN))
    {
      if (mo != MEMMODEL_LAST)
	error_at (c_parser_peek_token (parser)->location,
		  "%<flush%> list specified together with memory order "
		  "clause");
      c_parser_omp_var_list_parens (parser, OMP_CLAUSE_ERROR, NULL);
    }
  else if (c_parser_next_token_is_not (parser, CPP_PRAGMA_EOL))
    c_parser_error (parser, "expected %<(%> or end of line");
  c_parser_skip_to_pragma_eol (parser);

  c_finish_omp_flush (loc, mo);
}

/* OpenMP 5.0:

   scan-loop-body:
     { structured-block scan-directive structured-block }  */

static void
c_parser_omp_scan_loop_body (c_parser *parser, bool open_brace_parsed)
{
  tree substmt;
  location_t loc;
  tree clauses = NULL_TREE;

  loc = c_parser_peek_token (parser)->location;
  if (!open_brace_parsed
      && !c_parser_require (parser, CPP_OPEN_BRACE, "expected %<{%>"))
    {
      /* Avoid skipping until the end of the block.  */
      parser->error = false;
      return;
    }

  substmt = c_parser_omp_structured_block (parser, NULL);
  substmt = build2 (OMP_SCAN, void_type_node, substmt, NULL_TREE);
  SET_EXPR_LOCATION (substmt, loc);
  add_stmt (substmt);

  loc = c_parser_peek_token (parser)->location;
  if (c_parser_peek_token (parser)->pragma_kind == PRAGMA_OMP_SCAN)
    {
      enum omp_clause_code clause = OMP_CLAUSE_ERROR;

      c_parser_consume_pragma (parser);

      if (c_parser_next_token_is (parser, CPP_NAME))
	{
	  const char *p
	    = IDENTIFIER_POINTER (c_parser_peek_token (parser)->value);
	  if (strcmp (p, "inclusive") == 0)
	    clause = OMP_CLAUSE_INCLUSIVE;
	  else if (strcmp (p, "exclusive") == 0)
	    clause = OMP_CLAUSE_EXCLUSIVE;
	}
      if (clause != OMP_CLAUSE_ERROR)
	{
	  c_parser_consume_token (parser);
	  clauses = c_parser_omp_var_list_parens (parser, clause, NULL_TREE);
	}
      else
	c_parser_error (parser, "expected %<inclusive%> or "
				"%<exclusive%> clause");
      c_parser_skip_to_pragma_eol (parser);
    }
  else
    error ("expected %<#pragma omp scan%>");

  clauses = c_finish_omp_clauses (clauses, C_ORT_OMP);
  substmt = c_parser_omp_structured_block (parser, NULL);
  substmt = build2 (OMP_SCAN, void_type_node, substmt, clauses);
  SET_EXPR_LOCATION (substmt, loc);
  add_stmt (substmt);

  c_parser_skip_until_found (parser, CPP_CLOSE_BRACE,
			     "expected %<}%>");
}

/* Parse the restricted form of loop statements allowed by OpenACC and OpenMP.
   The real trick here is to determine the loop control variable early
   so that we can push a new decl if necessary to make it private.
   LOC is the location of the "acc" or "omp" in "#pragma acc" or "#pragma omp",
   respectively.  */

static tree
c_parser_omp_for_loop (location_t loc, c_parser *parser, enum tree_code code,
		       tree clauses, tree *cclauses, bool *if_p)
{
  tree decl, cond, incr, body, init, stmt, cl;
  unsigned char save_in_statement;
  tree declv, condv, incrv, initv, ret = NULL_TREE;
  tree pre_body = NULL_TREE, this_pre_body;
  tree ordered_cl = NULL_TREE;
  bool fail = false, open_brace_parsed = false;
  int i, collapse = 1, ordered = 0, count, nbraces = 0;
  location_t for_loc;
  bool tiling = false;
  bool inscan = false;
  vec<tree, va_gc> *for_block = make_tree_vector ();

  for (cl = clauses; cl; cl = OMP_CLAUSE_CHAIN (cl))
    if (OMP_CLAUSE_CODE (cl) == OMP_CLAUSE_COLLAPSE)
      collapse = tree_to_shwi (OMP_CLAUSE_COLLAPSE_EXPR (cl));
    else if (OMP_CLAUSE_CODE (cl) == OMP_CLAUSE_TILE)
      {
	tiling = true;
	collapse = list_length (OMP_CLAUSE_TILE_LIST (cl));
      }
    else if (OMP_CLAUSE_CODE (cl) == OMP_CLAUSE_ORDERED
	     && OMP_CLAUSE_ORDERED_EXPR (cl))
      {
	ordered_cl = cl;
	ordered = tree_to_shwi (OMP_CLAUSE_ORDERED_EXPR (cl));
      }
    else if (OMP_CLAUSE_CODE (cl) == OMP_CLAUSE_REDUCTION
	     && OMP_CLAUSE_REDUCTION_INSCAN (cl)
	     && (code == OMP_SIMD || code == OMP_FOR))
      inscan = true;

  if (ordered && ordered < collapse)
    {
      error_at (OMP_CLAUSE_LOCATION (ordered_cl),
		"%<ordered%> clause parameter is less than %<collapse%>");
      OMP_CLAUSE_ORDERED_EXPR (ordered_cl)
	= build_int_cst (NULL_TREE, collapse);
      ordered = collapse;
    }
  if (ordered)
    {
      for (tree *pc = &clauses; *pc; )
	if (OMP_CLAUSE_CODE (*pc) == OMP_CLAUSE_LINEAR)
	  {
	    error_at (OMP_CLAUSE_LOCATION (*pc),
		      "%<linear%> clause may not be specified together "
		      "with %<ordered%> clause with a parameter");
	    *pc = OMP_CLAUSE_CHAIN (*pc);
	  }
	else
	  pc = &OMP_CLAUSE_CHAIN (*pc);
    }

  gcc_assert (tiling || (collapse >= 1 && ordered >= 0));
  count = ordered ? ordered : collapse;

  declv = make_tree_vec (count);
  initv = make_tree_vec (count);
  condv = make_tree_vec (count);
  incrv = make_tree_vec (count);

  if (!c_parser_next_token_is_keyword (parser, RID_FOR))
    {
      c_parser_error (parser, "for statement expected");
      return NULL;
    }
  for_loc = c_parser_peek_token (parser)->location;
  c_parser_consume_token (parser);

  /* Forbid break/continue in the loop initializer, condition, and
     increment expressions.  */
  save_in_statement = in_statement;
  in_statement = IN_OMP_BLOCK;

  for (i = 0; i < count; i++)
    {
      int bracecount = 0;

      matching_parens parens;
      if (!parens.require_open (parser))
	goto pop_scopes;

      /* Parse the initialization declaration or expression.  */
      if (c_parser_next_tokens_start_declaration (parser))
	{
	  if (i > 0)
	    vec_safe_push (for_block, c_begin_compound_stmt (true));
	  this_pre_body = push_stmt_list ();
	  c_in_omp_for = true;
	  c_parser_declaration_or_fndef (parser, true, true, true, true, true,
					 NULL, vNULL);
	  c_in_omp_for = false;
	  if (this_pre_body)
	    {
	      this_pre_body = pop_stmt_list (this_pre_body);
	      if (pre_body)
		{
		  tree t = pre_body;   
		  pre_body = push_stmt_list ();
		  add_stmt (t);
		  add_stmt (this_pre_body);
		  pre_body = pop_stmt_list (pre_body);
		}
	      else
		pre_body = this_pre_body;
	    }
	  decl = check_for_loop_decls (for_loc, flag_isoc99);
	  if (decl == NULL)
	    goto error_init;
	  if (DECL_INITIAL (decl) == error_mark_node)
	    decl = error_mark_node;
	  init = decl;
	}
      else if (c_parser_next_token_is (parser, CPP_NAME)
	       && c_parser_peek_2nd_token (parser)->type == CPP_EQ)
	{
	  struct c_expr decl_exp;
	  struct c_expr init_exp;
	  location_t init_loc;

	  decl_exp = c_parser_postfix_expression (parser);
	  decl = decl_exp.value;

	  c_parser_require (parser, CPP_EQ, "expected %<=%>");

	  init_loc = c_parser_peek_token (parser)->location;
	  init_exp = c_parser_expr_no_commas (parser, NULL);
	  init_exp = default_function_array_read_conversion (init_loc,
							     init_exp);
	  c_in_omp_for = true;
	  init = build_modify_expr (init_loc, decl, decl_exp.original_type,
				    NOP_EXPR, init_loc, init_exp.value,
				    init_exp.original_type);
	  c_in_omp_for = false;
	  init = c_process_expr_stmt (init_loc, init);

	  c_parser_skip_until_found (parser, CPP_SEMICOLON, "expected %<;%>");
	}
      else
	{
	error_init:
	  c_parser_error (parser,
			  "expected iteration declaration or initialization");
	  c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
				     "expected %<)%>");
	  fail = true;
	  goto parse_next;
	}

      /* Parse the loop condition.  */
      cond = NULL_TREE;
      if (c_parser_next_token_is_not (parser, CPP_SEMICOLON))
	{
	  location_t cond_loc = c_parser_peek_token (parser)->location;
	  c_in_omp_for = true;
	  struct c_expr cond_expr
	    = c_parser_binary_expression (parser, NULL, NULL_TREE);
          c_in_omp_for = false;

	  cond = cond_expr.value;
	  cond = c_objc_common_truthvalue_conversion (cond_loc, cond);
	  switch (cond_expr.original_code)
	    {
	    case GT_EXPR:
	    case GE_EXPR:
	    case LT_EXPR:
	    case LE_EXPR:
	      break;
	    case NE_EXPR:
	      if (code != OACC_LOOP)
		break;
	      /* FALLTHRU.  */
	    default:
	      /* Can't be cond = error_mark_node, because we want to preserve
		 the location until c_finish_omp_for.  */
	      cond = build1 (NOP_EXPR, boolean_type_node, error_mark_node);
	      break;
	    }
	  protected_set_expr_location (cond, cond_loc);
	}
      c_parser_skip_until_found (parser, CPP_SEMICOLON, "expected %<;%>");

      /* Parse the increment expression.  */
      incr = NULL_TREE;
      if (c_parser_next_token_is_not (parser, CPP_CLOSE_PAREN))
	{
	  location_t incr_loc = c_parser_peek_token (parser)->location;

	  incr = c_process_expr_stmt (incr_loc,
				      c_parser_expression (parser).value);
	}
      parens.skip_until_found_close (parser);

      if (decl == NULL || decl == error_mark_node || init == error_mark_node)
	fail = true;
      else
	{
	  TREE_VEC_ELT (declv, i) = decl;
	  TREE_VEC_ELT (initv, i) = init;
	  TREE_VEC_ELT (condv, i) = cond;
	  TREE_VEC_ELT (incrv, i) = incr;
	}

    parse_next:
      if (i == count - 1)
	break;

      /* FIXME: OpenMP 3.0 draft isn't very clear on what exactly is allowed
	 in between the collapsed for loops to be still considered perfectly
	 nested.  Hopefully the final version clarifies this.
	 For now handle (multiple) {'s and empty statements.  */
      do
	{
	  if (c_parser_next_token_is_keyword (parser, RID_FOR))
	    {
	      c_parser_consume_token (parser);
	      break;
	    }
	  else if (c_parser_next_token_is (parser, CPP_OPEN_BRACE))
	    {
	      c_parser_consume_token (parser);
	      bracecount++;
	    }
	  else if (bracecount
		   && c_parser_next_token_is (parser, CPP_SEMICOLON))
	    c_parser_consume_token (parser);
	  else
	    {
	      c_parser_error (parser, "not enough perfectly nested loops");
	      if (bracecount)
		{
		  open_brace_parsed = true;
		  bracecount--;
		}
	      fail = true;
	      count = 0;
	      break;
	    }
	}
      while (1);

      nbraces += bracecount;
    }

  if (nbraces)
    if_p = NULL;

  in_statement = IN_OMP_FOR;
  body = push_stmt_list ();

  if (inscan)
    c_parser_omp_scan_loop_body (parser, open_brace_parsed);
  else if (open_brace_parsed)
    {
      location_t here = c_parser_peek_token (parser)->location;
      stmt = c_begin_compound_stmt (true);
      c_parser_compound_statement_nostart (parser);
      add_stmt (c_end_compound_stmt (here, stmt, true));
    }
  else
    add_stmt (c_parser_c99_block_statement (parser, if_p));

  body = pop_stmt_list (body);
  in_statement = save_in_statement;

  while (nbraces)
    {
      if (c_parser_next_token_is (parser, CPP_CLOSE_BRACE))
	{
	  c_parser_consume_token (parser);
	  nbraces--;
	}
      else if (c_parser_next_token_is (parser, CPP_SEMICOLON))
	c_parser_consume_token (parser);
      else
	{
	  c_parser_error (parser, "collapsed loops not perfectly nested");
	  while (nbraces)
	    {
	      location_t here = c_parser_peek_token (parser)->location;
	      stmt = c_begin_compound_stmt (true);
	      add_stmt (body);
	      c_parser_compound_statement_nostart (parser);
	      body = c_end_compound_stmt (here, stmt, true);
	      nbraces--;
	    }
	  goto pop_scopes;
	}
    }

  /* Only bother calling c_finish_omp_for if we haven't already generated
     an error from the initialization parsing.  */
  if (!fail)
    {
      c_in_omp_for = true;
      stmt = c_finish_omp_for (loc, code, declv, NULL, initv, condv,
			       incrv, body, pre_body, true);
      c_in_omp_for = false;

      /* Check for iterators appearing in lb, b or incr expressions.  */
      if (stmt && !c_omp_check_loop_iv (stmt, declv, NULL))
	stmt = NULL_TREE;

      if (stmt)
	{
	  add_stmt (stmt);

	  for (i = 0; i < TREE_VEC_LENGTH (OMP_FOR_INIT (stmt)); i++)
	    {
	      tree init = TREE_VEC_ELT (OMP_FOR_INIT (stmt), i);
	      gcc_assert (TREE_CODE (init) == MODIFY_EXPR);
	      tree decl = TREE_OPERAND (init, 0);
	      tree cond = TREE_VEC_ELT (OMP_FOR_COND (stmt), i);
	      gcc_assert (COMPARISON_CLASS_P (cond));
	      gcc_assert (TREE_OPERAND (cond, 0) == decl);

	      tree op0 = TREE_OPERAND (init, 1);
	      if (!OMP_FOR_NON_RECTANGULAR (stmt)
		  || TREE_CODE (op0) != TREE_VEC)
		TREE_OPERAND (init, 1) = c_fully_fold (op0, false, NULL);
	      else
		{
		  TREE_VEC_ELT (op0, 1)
		    = c_fully_fold (TREE_VEC_ELT (op0, 1), false, NULL);
		  TREE_VEC_ELT (op0, 2)
		    = c_fully_fold (TREE_VEC_ELT (op0, 2), false, NULL);
		}

	      tree op1 = TREE_OPERAND (cond, 1);
	      if (!OMP_FOR_NON_RECTANGULAR (stmt)
		  || TREE_CODE (op1) != TREE_VEC)
		TREE_OPERAND (cond, 1) = c_fully_fold (op1, false, NULL);
	      else
		{
		  TREE_VEC_ELT (op1, 1)
		    = c_fully_fold (TREE_VEC_ELT (op1, 1), false, NULL);
		  TREE_VEC_ELT (op1, 2)
		    = c_fully_fold (TREE_VEC_ELT (op1, 2), false, NULL);
		}
	    }

	  if (cclauses != NULL
	      && cclauses[C_OMP_CLAUSE_SPLIT_PARALLEL] != NULL)
	    {
	      tree *c;
	      for (c = &cclauses[C_OMP_CLAUSE_SPLIT_PARALLEL]; *c ; )
		if (OMP_CLAUSE_CODE (*c) != OMP_CLAUSE_FIRSTPRIVATE
		    && OMP_CLAUSE_CODE (*c) != OMP_CLAUSE_LASTPRIVATE)
		  c = &OMP_CLAUSE_CHAIN (*c);
		else
		  {
		    for (i = 0; i < count; i++)
		      if (TREE_VEC_ELT (declv, i) == OMP_CLAUSE_DECL (*c))
			break;
		    if (i == count)
		      c = &OMP_CLAUSE_CHAIN (*c);
		    else if (OMP_CLAUSE_CODE (*c) == OMP_CLAUSE_FIRSTPRIVATE)
		      {
			error_at (loc,
				  "iteration variable %qD should not be firstprivate",
				  OMP_CLAUSE_DECL (*c));
			*c = OMP_CLAUSE_CHAIN (*c);
		      }
		    else
		      {
			/* Move lastprivate (decl) clause to OMP_FOR_CLAUSES.  */
			tree l = *c;
			*c = OMP_CLAUSE_CHAIN (*c);
			if (code == OMP_SIMD)
			  {
			    OMP_CLAUSE_CHAIN (l)
			      = cclauses[C_OMP_CLAUSE_SPLIT_FOR];
			    cclauses[C_OMP_CLAUSE_SPLIT_FOR] = l;
			  }
			else
			  {
			    OMP_CLAUSE_CHAIN (l) = clauses;
			    clauses = l;
			  }
		      }
		  }
	    }
	  OMP_FOR_CLAUSES (stmt) = clauses;
	}
      ret = stmt;
    }
pop_scopes:
  while (!for_block->is_empty ())
    {
      /* FIXME diagnostics: LOC below should be the actual location of
	 this particular for block.  We need to build a list of
	 locations to go along with FOR_BLOCK.  */
      stmt = c_end_compound_stmt (loc, for_block->pop (), true);
      add_stmt (stmt);
    }
  release_tree_vector (for_block);
  return ret;
}

/* Helper function for OpenMP parsing, split clauses and call
   finish_omp_clauses on each of the set of clauses afterwards.  */

static void
omp_split_clauses (location_t loc, enum tree_code code,
		   omp_clause_mask mask, tree clauses, tree *cclauses)
{
  int i;
  c_omp_split_clauses (loc, code, mask, clauses, cclauses);
  for (i = 0; i < C_OMP_CLAUSE_SPLIT_COUNT; i++)
    if (cclauses[i])
      cclauses[i] = c_finish_omp_clauses (cclauses[i], C_ORT_OMP);
}

/* OpenMP 5.0:
   #pragma omp loop loop-clause[optseq] new-line
     for-loop

   LOC is the location of the #pragma token.
*/

#define OMP_LOOP_CLAUSE_MASK					\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_PRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_LASTPRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_REDUCTION)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_COLLAPSE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_BIND)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_ORDER))

static tree
c_parser_omp_loop (location_t loc, c_parser *parser,
		   char *p_name, omp_clause_mask mask, tree *cclauses,
		   bool *if_p)
{
  tree block, clauses, ret;

  strcat (p_name, " loop");
  mask |= OMP_LOOP_CLAUSE_MASK;

  clauses = c_parser_omp_all_clauses (parser, mask, p_name, cclauses == NULL);
  if (cclauses)
    {
      omp_split_clauses (loc, OMP_LOOP, mask, clauses, cclauses);
      clauses = cclauses[C_OMP_CLAUSE_SPLIT_LOOP];
    }

  block = c_begin_compound_stmt (true);
  ret = c_parser_omp_for_loop (loc, parser, OMP_LOOP, clauses, cclauses, if_p);
  block = c_end_compound_stmt (loc, block, true);
  add_stmt (block);

  return ret;
}

/* OpenMP 4.0:
   #pragma omp simd simd-clause[optseq] new-line
     for-loop

   LOC is the location of the #pragma token.
*/

#define OMP_SIMD_CLAUSE_MASK					\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_SAFELEN)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_SIMDLEN)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_LINEAR)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_ALIGNED)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_PRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_LASTPRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_REDUCTION)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_COLLAPSE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_IF)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NONTEMPORAL)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_ORDER))

static tree
c_parser_omp_simd (location_t loc, c_parser *parser,
		   char *p_name, omp_clause_mask mask, tree *cclauses,
		   bool *if_p)
{
  tree block, clauses, ret;

  strcat (p_name, " simd");
  mask |= OMP_SIMD_CLAUSE_MASK;

  clauses = c_parser_omp_all_clauses (parser, mask, p_name, cclauses == NULL);
  if (cclauses)
    {
      omp_split_clauses (loc, OMP_SIMD, mask, clauses, cclauses);
      clauses = cclauses[C_OMP_CLAUSE_SPLIT_SIMD];
      tree c = omp_find_clause (cclauses[C_OMP_CLAUSE_SPLIT_FOR],
				OMP_CLAUSE_ORDERED);
      if (c && OMP_CLAUSE_ORDERED_EXPR (c))
	{
	  error_at (OMP_CLAUSE_LOCATION (c),
		    "%<ordered%> clause with parameter may not be specified "
		    "on %qs construct", p_name);
	  OMP_CLAUSE_ORDERED_EXPR (c) = NULL_TREE;
	}
    }

  block = c_begin_compound_stmt (true);
  ret = c_parser_omp_for_loop (loc, parser, OMP_SIMD, clauses, cclauses, if_p);
  block = c_end_compound_stmt (loc, block, true);
  add_stmt (block);

  return ret;
}

/* OpenMP 2.5:
   #pragma omp for for-clause[optseq] new-line
     for-loop

   OpenMP 4.0:
   #pragma omp for simd for-simd-clause[optseq] new-line
     for-loop

   LOC is the location of the #pragma token.
*/

#define OMP_FOR_CLAUSE_MASK					\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_PRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_FIRSTPRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_LASTPRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_LINEAR)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_REDUCTION)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_ORDERED)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_SCHEDULE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_COLLAPSE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NOWAIT)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_ALLOCATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_ORDER))

static tree
c_parser_omp_for (location_t loc, c_parser *parser,
		  char *p_name, omp_clause_mask mask, tree *cclauses,
		  bool *if_p)
{
  tree block, clauses, ret;

  strcat (p_name, " for");
  mask |= OMP_FOR_CLAUSE_MASK;
  /* parallel for{, simd} disallows nowait clause, but for
     target {teams distribute ,}parallel for{, simd} it should be accepted.  */
  if (cclauses && (mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_MAP)) == 0)
    mask &= ~(OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NOWAIT);
  /* Composite distribute parallel for{, simd} disallows ordered clause.  */
  if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_DIST_SCHEDULE)) != 0)
    mask &= ~(OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_ORDERED);

  if (c_parser_next_token_is (parser, CPP_NAME))
    {
      const char *p = IDENTIFIER_POINTER (c_parser_peek_token (parser)->value);

      if (strcmp (p, "simd") == 0)
	{
	  tree cclauses_buf[C_OMP_CLAUSE_SPLIT_COUNT];
	  if (cclauses == NULL)
	    cclauses = cclauses_buf;

	  c_parser_consume_token (parser);
	  if (!flag_openmp)  /* flag_openmp_simd  */
	    return c_parser_omp_simd (loc, parser, p_name, mask, cclauses,
				      if_p);
	  block = c_begin_compound_stmt (true);
	  ret = c_parser_omp_simd (loc, parser, p_name, mask, cclauses, if_p);
	  block = c_end_compound_stmt (loc, block, true);
	  if (ret == NULL_TREE)
	    return ret;
	  ret = make_node (OMP_FOR);
	  TREE_TYPE (ret) = void_type_node;
	  OMP_FOR_BODY (ret) = block;
	  OMP_FOR_CLAUSES (ret) = cclauses[C_OMP_CLAUSE_SPLIT_FOR];
	  SET_EXPR_LOCATION (ret, loc);
	  add_stmt (ret);
	  return ret;
	}
    }
  if (!flag_openmp)  /* flag_openmp_simd  */
    {
      c_parser_skip_to_pragma_eol (parser, false);
      return NULL_TREE;
    }

  /* Composite distribute parallel for disallows linear clause.  */
  if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_DIST_SCHEDULE)) != 0)
    mask &= ~(OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_LINEAR);

  clauses = c_parser_omp_all_clauses (parser, mask, p_name, cclauses == NULL);
  if (cclauses)
    {
      omp_split_clauses (loc, OMP_FOR, mask, clauses, cclauses);
      clauses = cclauses[C_OMP_CLAUSE_SPLIT_FOR];
    }

  block = c_begin_compound_stmt (true);
  ret = c_parser_omp_for_loop (loc, parser, OMP_FOR, clauses, cclauses, if_p);
  block = c_end_compound_stmt (loc, block, true);
  add_stmt (block);

  return ret;
}

static tree c_parser_omp_taskloop (location_t, c_parser *, char *,
				   omp_clause_mask, tree *, bool *);

/* OpenMP 2.5:
   # pragma omp master new-line
     structured-block

   LOC is the location of the #pragma token.
*/

static tree
c_parser_omp_master (location_t loc, c_parser *parser,
		     char *p_name, omp_clause_mask mask, tree *cclauses,
		     bool *if_p)
{
  tree block, clauses, ret;

  strcat (p_name, " master");

  if (c_parser_next_token_is (parser, CPP_NAME))
    {
      const char *p = IDENTIFIER_POINTER (c_parser_peek_token (parser)->value);

      if (strcmp (p, "taskloop") == 0)
	{
	  tree cclauses_buf[C_OMP_CLAUSE_SPLIT_COUNT];
	  if (cclauses == NULL)
	    cclauses = cclauses_buf;

	  c_parser_consume_token (parser);
	  if (!flag_openmp)  /* flag_openmp_simd  */
	    return c_parser_omp_taskloop (loc, parser, p_name, mask, cclauses,
					  if_p);
	  block = c_begin_compound_stmt (true);
	  ret = c_parser_omp_taskloop (loc, parser, p_name, mask, cclauses,
				       if_p);
	  block = c_end_compound_stmt (loc, block, true);
	  if (ret == NULL_TREE)
	    return ret;
	  ret = c_finish_omp_master (loc, block);
	  return ret;
	}
    }
  if (!flag_openmp)  /* flag_openmp_simd  */
    {
      c_parser_skip_to_pragma_eol (parser, false);
      return NULL_TREE;
    }

  if (cclauses)
    {
      clauses = c_parser_omp_all_clauses (parser, mask, p_name, false);
      omp_split_clauses (loc, OMP_MASTER, mask, clauses, cclauses);
    }
  else
    c_parser_skip_to_pragma_eol (parser);

  return c_finish_omp_master (loc, c_parser_omp_structured_block (parser,
								  if_p));
}

/* OpenMP 2.5:
   # pragma omp ordered new-line
     structured-block

   OpenMP 4.5:
   # pragma omp ordered ordered-clauses new-line
     structured-block

   # pragma omp ordered depend-clauses new-line  */

#define OMP_ORDERED_CLAUSE_MASK					\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_THREADS)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_SIMD))

#define OMP_ORDERED_DEPEND_CLAUSE_MASK				\
	(OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_DEPEND)

static bool
c_parser_omp_ordered (c_parser *parser, enum pragma_context context,
		      bool *if_p)
{
  location_t loc = c_parser_peek_token (parser)->location;
  c_parser_consume_pragma (parser);

  if (context != pragma_stmt && context != pragma_compound)
    {
      c_parser_error (parser, "expected declaration specifiers");
      c_parser_skip_to_pragma_eol (parser, false);
      return false;
    }

  if (c_parser_next_token_is (parser, CPP_NAME))
    {
      const char *p = IDENTIFIER_POINTER (c_parser_peek_token (parser)->value);

      if (!strcmp ("depend", p))
	{
	  if (!flag_openmp)	/* flag_openmp_simd  */
	    {
	      c_parser_skip_to_pragma_eol (parser, false);
	      return false;
	    }
	  if (context == pragma_stmt)
	    {
	      error_at (loc,
			"%<#pragma omp ordered%> with %<depend%> clause may "
			"only be used in compound statements");
	      c_parser_skip_to_pragma_eol (parser, false);
	      return false;
	    }

	  tree clauses
	    = c_parser_omp_all_clauses (parser,
					OMP_ORDERED_DEPEND_CLAUSE_MASK,
					"#pragma omp ordered");
	  c_finish_omp_ordered (loc, clauses, NULL_TREE);
	  return false;
	}
    }

  tree clauses = c_parser_omp_all_clauses (parser, OMP_ORDERED_CLAUSE_MASK,
					   "#pragma omp ordered");

  if (!flag_openmp	/* flag_openmp_simd  */
      && omp_find_clause (clauses, OMP_CLAUSE_SIMD) == NULL_TREE)
    return false;

  c_finish_omp_ordered (loc, clauses,
			c_parser_omp_structured_block (parser, if_p));
  return true;
}

/* OpenMP 2.5:

   section-scope:
     { section-sequence }

   section-sequence:
     section-directive[opt] structured-block
     section-sequence section-directive structured-block

    SECTIONS_LOC is the location of the #pragma omp sections.  */

static tree
c_parser_omp_sections_scope (location_t sections_loc, c_parser *parser)
{
  tree stmt, substmt;
  bool error_suppress = false;
  location_t loc;

  loc = c_parser_peek_token (parser)->location;
  if (!c_parser_require (parser, CPP_OPEN_BRACE, "expected %<{%>"))
    {
      /* Avoid skipping until the end of the block.  */
      parser->error = false;
      return NULL_TREE;
    }

  stmt = push_stmt_list ();

  if (c_parser_peek_token (parser)->pragma_kind != PRAGMA_OMP_SECTION)
    {
      substmt = c_parser_omp_structured_block (parser, NULL);
      substmt = build1 (OMP_SECTION, void_type_node, substmt);
      SET_EXPR_LOCATION (substmt, loc);
      add_stmt (substmt);
    }

  while (1)
    {
      if (c_parser_next_token_is (parser, CPP_CLOSE_BRACE))
	break;
      if (c_parser_next_token_is (parser, CPP_EOF))
	break;

      loc = c_parser_peek_token (parser)->location;
      if (c_parser_peek_token (parser)->pragma_kind == PRAGMA_OMP_SECTION)
	{
	  c_parser_consume_pragma (parser);
	  c_parser_skip_to_pragma_eol (parser);
	  error_suppress = false;
	}
      else if (!error_suppress)
	{
	  error_at (loc, "expected %<#pragma omp section%> or %<}%>");
	  error_suppress = true;
	}

      substmt = c_parser_omp_structured_block (parser, NULL);
      substmt = build1 (OMP_SECTION, void_type_node, substmt);
      SET_EXPR_LOCATION (substmt, loc);
      add_stmt (substmt);
    }
  c_parser_skip_until_found (parser, CPP_CLOSE_BRACE,
			     "expected %<#pragma omp section%> or %<}%>");

  substmt = pop_stmt_list (stmt);

  stmt = make_node (OMP_SECTIONS);
  SET_EXPR_LOCATION (stmt, sections_loc);
  TREE_TYPE (stmt) = void_type_node;
  OMP_SECTIONS_BODY (stmt) = substmt;

  return add_stmt (stmt);
}

/* OpenMP 2.5:
   # pragma omp sections sections-clause[optseq] newline
     sections-scope

   LOC is the location of the #pragma token.
*/

#define OMP_SECTIONS_CLAUSE_MASK				\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_PRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_FIRSTPRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_LASTPRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_REDUCTION)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_ALLOCATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NOWAIT))

static tree
c_parser_omp_sections (location_t loc, c_parser *parser,
		       char *p_name, omp_clause_mask mask, tree *cclauses)
{
  tree block, clauses, ret;

  strcat (p_name, " sections");
  mask |= OMP_SECTIONS_CLAUSE_MASK;
  if (cclauses)
    mask &= ~(OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NOWAIT);

  clauses = c_parser_omp_all_clauses (parser, mask, p_name, cclauses == NULL);
  if (cclauses)
    {
      omp_split_clauses (loc, OMP_SECTIONS, mask, clauses, cclauses);
      clauses = cclauses[C_OMP_CLAUSE_SPLIT_SECTIONS];
    }

  block = c_begin_compound_stmt (true);
  ret = c_parser_omp_sections_scope (loc, parser);
  if (ret)
    OMP_SECTIONS_CLAUSES (ret) = clauses;
  block = c_end_compound_stmt (loc, block, true);
  add_stmt (block);

  return ret;
}

/* OpenMP 2.5:
   # pragma omp parallel parallel-clause[optseq] new-line
     structured-block
   # pragma omp parallel for parallel-for-clause[optseq] new-line
     structured-block
   # pragma omp parallel sections parallel-sections-clause[optseq] new-line
     structured-block

   OpenMP 4.0:
   # pragma omp parallel for simd parallel-for-simd-clause[optseq] new-line
     structured-block

   LOC is the location of the #pragma token.
*/

#define OMP_PARALLEL_CLAUSE_MASK				\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_IF)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_PRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_FIRSTPRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_DEFAULT)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_SHARED)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_COPYIN)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_REDUCTION)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NUM_THREADS)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_ALLOCATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_PROC_BIND))

static tree
c_parser_omp_parallel (location_t loc, c_parser *parser,
		       char *p_name, omp_clause_mask mask, tree *cclauses,
		       bool *if_p)
{
  tree stmt, clauses, block;

  strcat (p_name, " parallel");
  mask |= OMP_PARALLEL_CLAUSE_MASK;
  /* #pragma omp target parallel{, for, for simd} disallow copyin clause.  */
  if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_MAP)) != 0
      && (mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_DIST_SCHEDULE)) == 0)
    mask &= ~(OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_COPYIN);

  if (c_parser_next_token_is_keyword (parser, RID_FOR))
    {
      tree cclauses_buf[C_OMP_CLAUSE_SPLIT_COUNT];
      if (cclauses == NULL)
	cclauses = cclauses_buf;

      c_parser_consume_token (parser);
      if (!flag_openmp)  /* flag_openmp_simd  */
	return c_parser_omp_for (loc, parser, p_name, mask, cclauses, if_p);
      block = c_begin_omp_parallel ();
      tree ret = c_parser_omp_for (loc, parser, p_name, mask, cclauses, if_p);
      stmt
	= c_finish_omp_parallel (loc, cclauses[C_OMP_CLAUSE_SPLIT_PARALLEL],
				 block);
      if (ret == NULL_TREE)
	return ret;
      OMP_PARALLEL_COMBINED (stmt) = 1;
      return stmt;
    }
  /* When combined with distribute, parallel has to be followed by for.
     #pragma omp target parallel is allowed though.  */
  else if (cclauses
	   && (mask & (OMP_CLAUSE_MASK_1
		       << PRAGMA_OMP_CLAUSE_DIST_SCHEDULE)) != 0)
    {
      error_at (loc, "expected %<for%> after %qs", p_name);
      c_parser_skip_to_pragma_eol (parser);
      return NULL_TREE;
    }
  else if (c_parser_next_token_is (parser, CPP_NAME))
    {
      const char *p = IDENTIFIER_POINTER (c_parser_peek_token (parser)->value);
      if (cclauses == NULL && strcmp (p, "master") == 0)
	{
	  tree cclauses_buf[C_OMP_CLAUSE_SPLIT_COUNT];
	  cclauses = cclauses_buf;

	  c_parser_consume_token (parser);
	  if (!flag_openmp)  /* flag_openmp_simd  */
	    return c_parser_omp_master (loc, parser, p_name, mask, cclauses,
					if_p);
	  block = c_begin_omp_parallel ();
	  tree ret = c_parser_omp_master (loc, parser, p_name, mask, cclauses,
					  if_p);
	  stmt = c_finish_omp_parallel (loc,
					cclauses[C_OMP_CLAUSE_SPLIT_PARALLEL],
					block);
	  if (ret == NULL)
	    return ret;
	  OMP_PARALLEL_COMBINED (stmt) = 1;
	  return stmt;
	}
      else if (strcmp (p, "loop") == 0)
	{
	  tree cclauses_buf[C_OMP_CLAUSE_SPLIT_COUNT];
	  if (cclauses == NULL)
	    cclauses = cclauses_buf;

	  c_parser_consume_token (parser);
	  if (!flag_openmp)  /* flag_openmp_simd  */
	    return c_parser_omp_loop (loc, parser, p_name, mask, cclauses,
				      if_p);
	  block = c_begin_omp_parallel ();
	  tree ret = c_parser_omp_loop (loc, parser, p_name, mask, cclauses,
					if_p);
	  stmt
	    = c_finish_omp_parallel (loc,
				     cclauses[C_OMP_CLAUSE_SPLIT_PARALLEL],
				     block);
	  if (ret == NULL_TREE)
	    return ret;
	  OMP_PARALLEL_COMBINED (stmt) = 1;
	  return stmt;
	}
      else if (!flag_openmp)  /* flag_openmp_simd  */
	{
	  c_parser_skip_to_pragma_eol (parser, false);
	  return NULL_TREE;
	}
      else if (cclauses == NULL && strcmp (p, "sections") == 0)
	{
	  tree cclauses_buf[C_OMP_CLAUSE_SPLIT_COUNT];
	  cclauses = cclauses_buf;

	  c_parser_consume_token (parser);
	  block = c_begin_omp_parallel ();
	  c_parser_omp_sections (loc, parser, p_name, mask, cclauses);
	  stmt = c_finish_omp_parallel (loc,
					cclauses[C_OMP_CLAUSE_SPLIT_PARALLEL],
					block);
	  OMP_PARALLEL_COMBINED (stmt) = 1;
	  return stmt;
	}
    }
  else if (!flag_openmp)  /* flag_openmp_simd  */
    {
      c_parser_skip_to_pragma_eol (parser, false);
      return NULL_TREE;
    }

  clauses = c_parser_omp_all_clauses (parser, mask, p_name, cclauses == NULL);
  if (cclauses)
    {
      omp_split_clauses (loc, OMP_PARALLEL, mask, clauses, cclauses);
      clauses = cclauses[C_OMP_CLAUSE_SPLIT_PARALLEL];
    }

  block = c_begin_omp_parallel ();
  c_parser_statement (parser, if_p);
  stmt = c_finish_omp_parallel (loc, clauses, block);

  return stmt;
}

/* OpenMP 2.5:
   # pragma omp single single-clause[optseq] new-line
     structured-block

   LOC is the location of the #pragma.
*/

#define OMP_SINGLE_CLAUSE_MASK					\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_PRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_FIRSTPRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_COPYPRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_ALLOCATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NOWAIT))

static tree
c_parser_omp_single (location_t loc, c_parser *parser, bool *if_p)
{
  tree stmt = make_node (OMP_SINGLE);
  SET_EXPR_LOCATION (stmt, loc);
  TREE_TYPE (stmt) = void_type_node;

  OMP_SINGLE_CLAUSES (stmt)
    = c_parser_omp_all_clauses (parser, OMP_SINGLE_CLAUSE_MASK,
				"#pragma omp single");
  OMP_SINGLE_BODY (stmt) = c_parser_omp_structured_block (parser, if_p);

  return add_stmt (stmt);
}

/* OpenMP 3.0:
   # pragma omp task task-clause[optseq] new-line

   LOC is the location of the #pragma.
*/

#define OMP_TASK_CLAUSE_MASK					\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_IF)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_UNTIED)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_DEFAULT)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_PRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_FIRSTPRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_SHARED)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_FINAL)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_MERGEABLE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_DEPEND)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_PRIORITY)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_ALLOCATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_IN_REDUCTION)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_DETACH))

static tree
c_parser_omp_task (location_t loc, c_parser *parser, bool *if_p)
{
  tree clauses, block;

  clauses = c_parser_omp_all_clauses (parser, OMP_TASK_CLAUSE_MASK,
				      "#pragma omp task");

  block = c_begin_omp_task ();
  c_parser_statement (parser, if_p);
  return c_finish_omp_task (loc, clauses, block);
}

/* OpenMP 3.0:
   # pragma omp taskwait new-line

   OpenMP 5.0:
   # pragma omp taskwait taskwait-clause[optseq] new-line
*/

#define OMP_TASKWAIT_CLAUSE_MASK					\
	(OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_DEPEND)

static void
c_parser_omp_taskwait (c_parser *parser)
{
  location_t loc = c_parser_peek_token (parser)->location;
  c_parser_consume_pragma (parser);

  tree clauses
    = c_parser_omp_all_clauses (parser, OMP_TASKWAIT_CLAUSE_MASK,
				"#pragma omp taskwait");

  if (clauses)
    {
      tree stmt = make_node (OMP_TASK);
      TREE_TYPE (stmt) = void_node;
      OMP_TASK_CLAUSES (stmt) = clauses;
      OMP_TASK_BODY (stmt) = NULL_TREE;
      SET_EXPR_LOCATION (stmt, loc);
      add_stmt (stmt);
    }
  else
    c_finish_omp_taskwait (loc);
}

/* OpenMP 3.1:
   # pragma omp taskyield new-line
*/

static void
c_parser_omp_taskyield (c_parser *parser)
{
  location_t loc = c_parser_peek_token (parser)->location;
  c_parser_consume_pragma (parser);
  c_parser_skip_to_pragma_eol (parser);

  c_finish_omp_taskyield (loc);
}

/* OpenMP 4.0:
   # pragma omp taskgroup new-line

   OpenMP 5.0:
   # pragma omp taskgroup taskgroup-clause[optseq] new-line
*/

#define OMP_TASKGROUP_CLAUSE_MASK				\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_ALLOCATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_TASK_REDUCTION))

static tree
c_parser_omp_taskgroup (location_t loc, c_parser *parser, bool *if_p)
{
  tree clauses = c_parser_omp_all_clauses (parser, OMP_TASKGROUP_CLAUSE_MASK,
					   "#pragma omp taskgroup");

  tree body = c_parser_omp_structured_block (parser, if_p);
  return c_finish_omp_taskgroup (loc, body, clauses);
}

/* OpenMP 4.0:
   # pragma omp cancel cancel-clause[optseq] new-line

   LOC is the location of the #pragma.
*/

#define OMP_CANCEL_CLAUSE_MASK					\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_PARALLEL)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_FOR)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_SECTIONS)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_TASKGROUP)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_IF))

static void
c_parser_omp_cancel (c_parser *parser)
{
  location_t loc = c_parser_peek_token (parser)->location;

  c_parser_consume_pragma (parser);
  tree clauses = c_parser_omp_all_clauses (parser, OMP_CANCEL_CLAUSE_MASK,
					   "#pragma omp cancel");

  c_finish_omp_cancel (loc, clauses);
}

/* OpenMP 4.0:
   # pragma omp cancellation point cancelpt-clause[optseq] new-line

   LOC is the location of the #pragma.
*/

#define OMP_CANCELLATION_POINT_CLAUSE_MASK			\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_PARALLEL)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_FOR)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_SECTIONS)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_TASKGROUP))

static void
c_parser_omp_cancellation_point (c_parser *parser, enum pragma_context context)
{
  location_t loc = c_parser_peek_token (parser)->location;
  tree clauses;
  bool point_seen = false;

  c_parser_consume_pragma (parser);
  if (c_parser_next_token_is (parser, CPP_NAME))
    {
      const char *p = IDENTIFIER_POINTER (c_parser_peek_token (parser)->value);
      if (strcmp (p, "point") == 0)
	{
	  c_parser_consume_token (parser);
	  point_seen = true;
	}
    }
  if (!point_seen)
    {
      c_parser_error (parser, "expected %<point%>");
      c_parser_skip_to_pragma_eol (parser);
      return;
    }

  if (context != pragma_compound)
    {
      if (context == pragma_stmt)
	error_at (loc,
		  "%<#pragma %s%> may only be used in compound statements",
		  "omp cancellation point");
      else
	c_parser_error (parser, "expected declaration specifiers");
      c_parser_skip_to_pragma_eol (parser, false);
      return;
    }

  clauses
    = c_parser_omp_all_clauses (parser, OMP_CANCELLATION_POINT_CLAUSE_MASK,
				"#pragma omp cancellation point");

  c_finish_omp_cancellation_point (loc, clauses);
}

/* OpenMP 4.0:
   #pragma omp distribute distribute-clause[optseq] new-line
     for-loop  */

#define OMP_DISTRIBUTE_CLAUSE_MASK				\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_PRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_FIRSTPRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_LASTPRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_DIST_SCHEDULE)\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_ALLOCATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_COLLAPSE))

static tree
c_parser_omp_distribute (location_t loc, c_parser *parser,
			 char *p_name, omp_clause_mask mask, tree *cclauses,
			 bool *if_p)
{
  tree clauses, block, ret;

  strcat (p_name, " distribute");
  mask |= OMP_DISTRIBUTE_CLAUSE_MASK;

  if (c_parser_next_token_is (parser, CPP_NAME))
    {
      const char *p = IDENTIFIER_POINTER (c_parser_peek_token (parser)->value);
      bool simd = false;
      bool parallel = false;

      if (strcmp (p, "simd") == 0)
	simd = true;
      else
	parallel = strcmp (p, "parallel") == 0;
      if (parallel || simd)
	{
	  tree cclauses_buf[C_OMP_CLAUSE_SPLIT_COUNT];
	  if (cclauses == NULL)
	    cclauses = cclauses_buf;
	  c_parser_consume_token (parser);
	  if (!flag_openmp)  /* flag_openmp_simd  */
	    {
	      if (simd)
		return c_parser_omp_simd (loc, parser, p_name, mask, cclauses,
					  if_p);
	      else
		return c_parser_omp_parallel (loc, parser, p_name, mask,
					      cclauses, if_p);
	    }
	  block = c_begin_compound_stmt (true);
	  if (simd)
	    ret = c_parser_omp_simd (loc, parser, p_name, mask, cclauses,
				     if_p);
	  else
	    ret = c_parser_omp_parallel (loc, parser, p_name, mask, cclauses,
					 if_p);
	  block = c_end_compound_stmt (loc, block, true);
	  if (ret == NULL)
	    return ret;
	  ret = make_node (OMP_DISTRIBUTE);
	  TREE_TYPE (ret) = void_type_node;
	  OMP_FOR_BODY (ret) = block;
	  OMP_FOR_CLAUSES (ret) = cclauses[C_OMP_CLAUSE_SPLIT_DISTRIBUTE];
	  SET_EXPR_LOCATION (ret, loc);
	  add_stmt (ret);
	  return ret;
	}
    }
  if (!flag_openmp)  /* flag_openmp_simd  */
    {
      c_parser_skip_to_pragma_eol (parser, false);
      return NULL_TREE;
    }

  clauses = c_parser_omp_all_clauses (parser, mask, p_name, cclauses == NULL);
  if (cclauses)
    {
      omp_split_clauses (loc, OMP_DISTRIBUTE, mask, clauses, cclauses);
      clauses = cclauses[C_OMP_CLAUSE_SPLIT_DISTRIBUTE];
    }

  block = c_begin_compound_stmt (true);
  ret = c_parser_omp_for_loop (loc, parser, OMP_DISTRIBUTE, clauses, NULL,
			       if_p);
  block = c_end_compound_stmt (loc, block, true);
  add_stmt (block);

  return ret;
}

/* OpenMP 4.0:
   # pragma omp teams teams-clause[optseq] new-line
     structured-block  */

#define OMP_TEAMS_CLAUSE_MASK					\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_PRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_FIRSTPRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_SHARED)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_REDUCTION)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NUM_TEAMS)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_THREAD_LIMIT)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_ALLOCATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_DEFAULT))

static tree
c_parser_omp_teams (location_t loc, c_parser *parser,
		    char *p_name, omp_clause_mask mask, tree *cclauses,
		    bool *if_p)
{
  tree clauses, block, ret;

  strcat (p_name, " teams");
  mask |= OMP_TEAMS_CLAUSE_MASK;

  if (c_parser_next_token_is (parser, CPP_NAME))
    {
      const char *p = IDENTIFIER_POINTER (c_parser_peek_token (parser)->value);
      if (strcmp (p, "distribute") == 0)
	{
	  tree cclauses_buf[C_OMP_CLAUSE_SPLIT_COUNT];
	  if (cclauses == NULL)
	    cclauses = cclauses_buf;

	  c_parser_consume_token (parser);
	  if (!flag_openmp)  /* flag_openmp_simd  */
	    return c_parser_omp_distribute (loc, parser, p_name, mask,
					    cclauses, if_p);
	  block = c_begin_omp_parallel ();
	  ret = c_parser_omp_distribute (loc, parser, p_name, mask, cclauses,
					 if_p);
	  block = c_end_compound_stmt (loc, block, true);
	  if (ret == NULL)
	    return ret;
	  clauses = cclauses[C_OMP_CLAUSE_SPLIT_TEAMS];
	  ret = make_node (OMP_TEAMS);
	  TREE_TYPE (ret) = void_type_node;
	  OMP_TEAMS_CLAUSES (ret) = clauses;
	  OMP_TEAMS_BODY (ret) = block;
	  OMP_TEAMS_COMBINED (ret) = 1;
	  SET_EXPR_LOCATION (ret, loc);
	  return add_stmt (ret);
	}
      else if (strcmp (p, "loop") == 0)
	{
	  tree cclauses_buf[C_OMP_CLAUSE_SPLIT_COUNT];
	  if (cclauses == NULL)
	    cclauses = cclauses_buf;

	  c_parser_consume_token (parser);
	  if (!flag_openmp)  /* flag_openmp_simd  */
	    return c_parser_omp_loop (loc, parser, p_name, mask, cclauses,
				      if_p);
	  block = c_begin_omp_parallel ();
	  ret = c_parser_omp_loop (loc, parser, p_name, mask, cclauses, if_p);
	  block = c_end_compound_stmt (loc, block, true);
	  if (ret == NULL)
	    return ret;
	  clauses = cclauses[C_OMP_CLAUSE_SPLIT_TEAMS];
	  ret = make_node (OMP_TEAMS);
	  TREE_TYPE (ret) = void_type_node;
	  OMP_TEAMS_CLAUSES (ret) = clauses;
	  OMP_TEAMS_BODY (ret) = block;
	  OMP_TEAMS_COMBINED (ret) = 1;
	  SET_EXPR_LOCATION (ret, loc);
	  return add_stmt (ret);
	}
    }
  if (!flag_openmp)  /* flag_openmp_simd  */
    {
      c_parser_skip_to_pragma_eol (parser, false);
      return NULL_TREE;
    }

  clauses = c_parser_omp_all_clauses (parser, mask, p_name, cclauses == NULL);
  if (cclauses)
    {
      omp_split_clauses (loc, OMP_TEAMS, mask, clauses, cclauses);
      clauses = cclauses[C_OMP_CLAUSE_SPLIT_TEAMS];
    }

  tree stmt = make_node (OMP_TEAMS);
  TREE_TYPE (stmt) = void_type_node;
  OMP_TEAMS_CLAUSES (stmt) = clauses;
  block = c_begin_omp_parallel ();
  add_stmt (c_parser_omp_structured_block (parser, if_p));
  OMP_TEAMS_BODY (stmt) = c_end_compound_stmt (loc, block, true);
  SET_EXPR_LOCATION (stmt, loc);

  return add_stmt (stmt);
}

/* OpenMP 4.0:
   # pragma omp target data target-data-clause[optseq] new-line
     structured-block  */

#define OMP_TARGET_DATA_CLAUSE_MASK				\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_DEVICE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_MAP)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_IF)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_USE_DEVICE_PTR) \
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_USE_DEVICE_ADDR))

static tree
c_parser_omp_target_data (location_t loc, c_parser *parser, bool *if_p)
{
  tree clauses
    = c_parser_omp_all_clauses (parser, OMP_TARGET_DATA_CLAUSE_MASK,
				"#pragma omp target data");
  c_omp_adjust_map_clauses (clauses, false);
  int map_seen = 0;
  for (tree *pc = &clauses; *pc;)
    {
      if (OMP_CLAUSE_CODE (*pc) == OMP_CLAUSE_MAP)
	switch (OMP_CLAUSE_MAP_KIND (*pc))
	  {
	  case GOMP_MAP_TO:
	  case GOMP_MAP_ALWAYS_TO:
	  case GOMP_MAP_FROM:
	  case GOMP_MAP_ALWAYS_FROM:
	  case GOMP_MAP_TOFROM:
	  case GOMP_MAP_ALWAYS_TOFROM:
	  case GOMP_MAP_ALLOC:
	    map_seen = 3;
	    break;
	  case GOMP_MAP_FIRSTPRIVATE_POINTER:
	  case GOMP_MAP_ALWAYS_POINTER:
	  case GOMP_MAP_ATTACH_DETACH:
	    break;
	  default:
	    map_seen |= 1;
	    error_at (OMP_CLAUSE_LOCATION (*pc),
		      "%<#pragma omp target data%> with map-type other "
		      "than %<to%>, %<from%>, %<tofrom%> or %<alloc%> "
		      "on %<map%> clause");
	    *pc = OMP_CLAUSE_CHAIN (*pc);
	    continue;
	  }
      else if (OMP_CLAUSE_CODE (*pc) == OMP_CLAUSE_USE_DEVICE_PTR
	       || OMP_CLAUSE_CODE (*pc) == OMP_CLAUSE_USE_DEVICE_ADDR)
	map_seen = 3;
      pc = &OMP_CLAUSE_CHAIN (*pc);
    }

  if (map_seen != 3)
    {
      if (map_seen == 0)
	error_at (loc,
		  "%<#pragma omp target data%> must contain at least "
		  "one %<map%>, %<use_device_ptr%> or %<use_device_addr%> "
		  "clause");
      return NULL_TREE;
    }

  tree stmt = make_node (OMP_TARGET_DATA);
  TREE_TYPE (stmt) = void_type_node;
  OMP_TARGET_DATA_CLAUSES (stmt) = clauses;
  keep_next_level ();
  tree block = c_begin_compound_stmt (true);
  add_stmt (c_parser_omp_structured_block (parser, if_p));
  OMP_TARGET_DATA_BODY (stmt) = c_end_compound_stmt (loc, block, true);

  SET_EXPR_LOCATION (stmt, loc);
  return add_stmt (stmt);
}

/* OpenMP 4.0:
   # pragma omp target update target-update-clause[optseq] new-line */

#define OMP_TARGET_UPDATE_CLAUSE_MASK				\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_FROM)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_TO)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_DEVICE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_IF)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_DEPEND)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NOWAIT))

static bool
c_parser_omp_target_update (location_t loc, c_parser *parser,
			    enum pragma_context context)
{
  if (context == pragma_stmt)
    {
      error_at (loc, "%<#pragma %s%> may only be used in compound statements",
		"omp target update");
      c_parser_skip_to_pragma_eol (parser, false);
      return false;
    }

  tree clauses
    = c_parser_omp_all_clauses (parser, OMP_TARGET_UPDATE_CLAUSE_MASK,
				"#pragma omp target update");
  if (omp_find_clause (clauses, OMP_CLAUSE_TO) == NULL_TREE
      && omp_find_clause (clauses, OMP_CLAUSE_FROM) == NULL_TREE)
    {
      error_at (loc,
		"%<#pragma omp target update%> must contain at least one "
		"%<from%> or %<to%> clauses");
      return false;
    }

  tree stmt = make_node (OMP_TARGET_UPDATE);
  TREE_TYPE (stmt) = void_type_node;
  OMP_TARGET_UPDATE_CLAUSES (stmt) = clauses;
  SET_EXPR_LOCATION (stmt, loc);
  add_stmt (stmt);
  return false;
}

/* OpenMP 4.5:
   # pragma omp target enter data target-data-clause[optseq] new-line  */

#define OMP_TARGET_ENTER_DATA_CLAUSE_MASK			\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_DEVICE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_MAP)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_IF)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_DEPEND)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NOWAIT))

static tree
c_parser_omp_target_enter_data (location_t loc, c_parser *parser,
				enum pragma_context context)
{
  bool data_seen = false;
  if (c_parser_next_token_is (parser, CPP_NAME))
    {
      const char *p = IDENTIFIER_POINTER (c_parser_peek_token (parser)->value);
      if (strcmp (p, "data") == 0)
	{
	  c_parser_consume_token (parser);
	  data_seen = true;
	}
    }
  if (!data_seen)
    {
      c_parser_error (parser, "expected %<data%>");
      c_parser_skip_to_pragma_eol (parser);
      return NULL_TREE;
    }

  if (context == pragma_stmt)
    {
      error_at (loc, "%<#pragma %s%> may only be used in compound statements",
		"omp target enter data");
      c_parser_skip_to_pragma_eol (parser, false);
      return NULL_TREE;
    }

  tree clauses
    = c_parser_omp_all_clauses (parser, OMP_TARGET_ENTER_DATA_CLAUSE_MASK,
				"#pragma omp target enter data");
  c_omp_adjust_map_clauses (clauses, false);
  int map_seen = 0;
  for (tree *pc = &clauses; *pc;)
    {
      if (OMP_CLAUSE_CODE (*pc) == OMP_CLAUSE_MAP)
	switch (OMP_CLAUSE_MAP_KIND (*pc))
	  {
	  case GOMP_MAP_TO:
	  case GOMP_MAP_ALWAYS_TO:
	  case GOMP_MAP_ALLOC:
	    map_seen = 3;
	    break;
	  case GOMP_MAP_FIRSTPRIVATE_POINTER:
	  case GOMP_MAP_ALWAYS_POINTER:
	  case GOMP_MAP_ATTACH_DETACH:
	    break;
	  default:
	    map_seen |= 1;
	    error_at (OMP_CLAUSE_LOCATION (*pc),
		      "%<#pragma omp target enter data%> with map-type other "
		      "than %<to%> or %<alloc%> on %<map%> clause");
	    *pc = OMP_CLAUSE_CHAIN (*pc);
	    continue;
	  }
      pc = &OMP_CLAUSE_CHAIN (*pc);
    }

  if (map_seen != 3)
    {
      if (map_seen == 0)
	error_at (loc,
		  "%<#pragma omp target enter data%> must contain at least "
		  "one %<map%> clause");
      return NULL_TREE;
    }

  tree stmt = make_node (OMP_TARGET_ENTER_DATA);
  TREE_TYPE (stmt) = void_type_node;
  OMP_TARGET_ENTER_DATA_CLAUSES (stmt) = clauses;
  SET_EXPR_LOCATION (stmt, loc);
  add_stmt (stmt);
  return stmt;
}

/* OpenMP 4.5:
   # pragma omp target exit data target-data-clause[optseq] new-line  */

#define OMP_TARGET_EXIT_DATA_CLAUSE_MASK			\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_DEVICE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_MAP)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_IF)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_DEPEND)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NOWAIT))

static tree
c_parser_omp_target_exit_data (location_t loc, c_parser *parser,
			       enum pragma_context context)
{
  bool data_seen = false;
  if (c_parser_next_token_is (parser, CPP_NAME))
    {
      const char *p = IDENTIFIER_POINTER (c_parser_peek_token (parser)->value);
      if (strcmp (p, "data") == 0)
	{
	  c_parser_consume_token (parser);
	  data_seen = true;
	}
    }
  if (!data_seen)
    {
      c_parser_error (parser, "expected %<data%>");
      c_parser_skip_to_pragma_eol (parser);
      return NULL_TREE;
    }

  if (context == pragma_stmt)
    {
      error_at (loc, "%<#pragma %s%> may only be used in compound statements",
		"omp target exit data");
      c_parser_skip_to_pragma_eol (parser, false);
      return NULL_TREE;
    }

  tree clauses
    = c_parser_omp_all_clauses (parser, OMP_TARGET_EXIT_DATA_CLAUSE_MASK,
				"#pragma omp target exit data");
  c_omp_adjust_map_clauses (clauses, false);
  int map_seen = 0;
  for (tree *pc = &clauses; *pc;)
    {
      if (OMP_CLAUSE_CODE (*pc) == OMP_CLAUSE_MAP)
	switch (OMP_CLAUSE_MAP_KIND (*pc))
	  {
	  case GOMP_MAP_FROM:
	  case GOMP_MAP_ALWAYS_FROM:
	  case GOMP_MAP_RELEASE:
	  case GOMP_MAP_DELETE:
	    map_seen = 3;
	    break;
	  case GOMP_MAP_FIRSTPRIVATE_POINTER:
	  case GOMP_MAP_ALWAYS_POINTER:
	  case GOMP_MAP_ATTACH_DETACH:
	    break;
	  default:
	    map_seen |= 1;
	    error_at (OMP_CLAUSE_LOCATION (*pc),
		      "%<#pragma omp target exit data%> with map-type other "
		      "than %<from%>, %<release%> or %<delete%> on %<map%>"
		      " clause");
	    *pc = OMP_CLAUSE_CHAIN (*pc);
	    continue;
	  }
      pc = &OMP_CLAUSE_CHAIN (*pc);
    }

  if (map_seen != 3)
    {
      if (map_seen == 0)
	error_at (loc,
		  "%<#pragma omp target exit data%> must contain at least one "
		  "%<map%> clause");
      return NULL_TREE;
    }

  tree stmt = make_node (OMP_TARGET_EXIT_DATA);
  TREE_TYPE (stmt) = void_type_node;
  OMP_TARGET_EXIT_DATA_CLAUSES (stmt) = clauses;
  SET_EXPR_LOCATION (stmt, loc);
  add_stmt (stmt);
  return stmt;
}

/* OpenMP 4.0:
   # pragma omp target target-clause[optseq] new-line
     structured-block  */

#define OMP_TARGET_CLAUSE_MASK					\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_DEVICE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_MAP)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_IF)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_DEPEND)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NOWAIT)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_PRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_FIRSTPRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_ALLOCATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_DEFAULTMAP)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_IS_DEVICE_PTR))

static bool
c_parser_omp_target (c_parser *parser, enum pragma_context context, bool *if_p)
{
  location_t loc = c_parser_peek_token (parser)->location;
  c_parser_consume_pragma (parser);
  tree *pc = NULL, stmt, block;

  if (context != pragma_stmt && context != pragma_compound)
    {
      c_parser_error (parser, "expected declaration specifiers");
      c_parser_skip_to_pragma_eol (parser);
      return false;
    }

  if (flag_openmp)
    omp_requires_mask
      = (enum omp_requires) (omp_requires_mask | OMP_REQUIRES_TARGET_USED);

  if (c_parser_next_token_is (parser, CPP_NAME))
    {
      const char *p = IDENTIFIER_POINTER (c_parser_peek_token (parser)->value);
      enum tree_code ccode = ERROR_MARK;

      if (strcmp (p, "teams") == 0)
	ccode = OMP_TEAMS;
      else if (strcmp (p, "parallel") == 0)
	ccode = OMP_PARALLEL;
      else if (strcmp (p, "simd") == 0)
	ccode = OMP_SIMD;
      if (ccode != ERROR_MARK)
	{
	  tree cclauses[C_OMP_CLAUSE_SPLIT_COUNT];
	  char p_name[sizeof ("#pragma omp target teams distribute "
			      "parallel for simd")];

	  c_parser_consume_token (parser);
	  strcpy (p_name, "#pragma omp target");
	  if (!flag_openmp)  /* flag_openmp_simd  */
	    {
	      tree stmt;
	      switch (ccode)
		{
		case OMP_TEAMS:
		  stmt = c_parser_omp_teams (loc, parser, p_name,
					     OMP_TARGET_CLAUSE_MASK,
					     cclauses, if_p);
		  break;
		case OMP_PARALLEL:
		  stmt = c_parser_omp_parallel (loc, parser, p_name,
						OMP_TARGET_CLAUSE_MASK,
						cclauses, if_p);
		  break;
		case OMP_SIMD:
		  stmt = c_parser_omp_simd (loc, parser, p_name,
					    OMP_TARGET_CLAUSE_MASK,
					    cclauses, if_p);
		  break;
		default:
		  gcc_unreachable ();
		}
	      return stmt != NULL_TREE;
	    }
	  keep_next_level ();
	  tree block = c_begin_compound_stmt (true), ret;
	  switch (ccode)
	    {
	    case OMP_TEAMS:
	      ret = c_parser_omp_teams (loc, parser, p_name,
					OMP_TARGET_CLAUSE_MASK, cclauses,
					if_p);
	      break;
	    case OMP_PARALLEL:
	      ret = c_parser_omp_parallel (loc, parser, p_name,
					   OMP_TARGET_CLAUSE_MASK, cclauses,
					   if_p);
	      break;
	    case OMP_SIMD:
	      ret = c_parser_omp_simd (loc, parser, p_name,
				       OMP_TARGET_CLAUSE_MASK, cclauses,
				       if_p);
	      break;
	    default:
	      gcc_unreachable ();
	    }
	  block = c_end_compound_stmt (loc, block, true);
	  if (ret == NULL_TREE)
	    return false;
	  if (ccode == OMP_TEAMS)
	    {
	      /* For combined target teams, ensure the num_teams and
		 thread_limit clause expressions are evaluated on the host,
		 before entering the target construct.  */
	      tree c;
	      for (c = cclauses[C_OMP_CLAUSE_SPLIT_TEAMS];
		   c; c = OMP_CLAUSE_CHAIN (c))
		if ((OMP_CLAUSE_CODE (c) == OMP_CLAUSE_NUM_TEAMS
		     || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_THREAD_LIMIT)
		    && TREE_CODE (OMP_CLAUSE_OPERAND (c, 0)) != INTEGER_CST)
		  {
		    tree expr = OMP_CLAUSE_OPERAND (c, 0);
		    tree tmp = create_tmp_var_raw (TREE_TYPE (expr));
		    expr = build4 (TARGET_EXPR, TREE_TYPE (expr), tmp,
				   expr, NULL_TREE, NULL_TREE);
		    add_stmt (expr);
		    OMP_CLAUSE_OPERAND (c, 0) = expr;
		    tree tc = build_omp_clause (OMP_CLAUSE_LOCATION (c),
						OMP_CLAUSE_FIRSTPRIVATE);
		    OMP_CLAUSE_DECL (tc) = tmp;
		    OMP_CLAUSE_CHAIN (tc)
		      = cclauses[C_OMP_CLAUSE_SPLIT_TARGET];
		    cclauses[C_OMP_CLAUSE_SPLIT_TARGET] = tc;
		  }
	    }
	  tree stmt = make_node (OMP_TARGET);
	  TREE_TYPE (stmt) = void_type_node;
	  OMP_TARGET_CLAUSES (stmt) = cclauses[C_OMP_CLAUSE_SPLIT_TARGET];
	  c_omp_adjust_map_clauses (OMP_TARGET_CLAUSES (stmt), true);
	  OMP_TARGET_BODY (stmt) = block;
	  OMP_TARGET_COMBINED (stmt) = 1;
	  SET_EXPR_LOCATION (stmt, loc);
	  add_stmt (stmt);
	  pc = &OMP_TARGET_CLAUSES (stmt);
	  goto check_clauses;
	}
      else if (!flag_openmp)  /* flag_openmp_simd  */
	{
	  c_parser_skip_to_pragma_eol (parser, false);
	  return false;
	}
      else if (strcmp (p, "data") == 0)
	{
	  c_parser_consume_token (parser);
	  c_parser_omp_target_data (loc, parser, if_p);
	  return true;
	}
      else if (strcmp (p, "enter") == 0)
	{
	  c_parser_consume_token (parser);
	  c_parser_omp_target_enter_data (loc, parser, context);
	  return false;
	}
      else if (strcmp (p, "exit") == 0)
	{
	  c_parser_consume_token (parser);
	  c_parser_omp_target_exit_data (loc, parser, context);
	  return false;
	}
      else if (strcmp (p, "update") == 0)
	{
	  c_parser_consume_token (parser);
	  return c_parser_omp_target_update (loc, parser, context);
	}
    }
  if (!flag_openmp) /* flag_openmp_simd  */
    {
      c_parser_skip_to_pragma_eol (parser, false);
      return false;
    }

  stmt = make_node (OMP_TARGET);
  TREE_TYPE (stmt) = void_type_node;

  OMP_TARGET_CLAUSES (stmt)
    = c_parser_omp_all_clauses (parser, OMP_TARGET_CLAUSE_MASK,
				"#pragma omp target");
  c_omp_adjust_map_clauses (OMP_TARGET_CLAUSES (stmt), true);

  pc = &OMP_TARGET_CLAUSES (stmt);
  keep_next_level ();
  block = c_begin_compound_stmt (true);
  add_stmt (c_parser_omp_structured_block (parser, if_p));
  OMP_TARGET_BODY (stmt) = c_end_compound_stmt (loc, block, true);

  SET_EXPR_LOCATION (stmt, loc);
  add_stmt (stmt);

check_clauses:
  while (*pc)
    {
      if (OMP_CLAUSE_CODE (*pc) == OMP_CLAUSE_MAP)
	switch (OMP_CLAUSE_MAP_KIND (*pc))
	  {
	  case GOMP_MAP_TO:
	  case GOMP_MAP_ALWAYS_TO:
	  case GOMP_MAP_FROM:
	  case GOMP_MAP_ALWAYS_FROM:
	  case GOMP_MAP_TOFROM:
	  case GOMP_MAP_ALWAYS_TOFROM:
	  case GOMP_MAP_ALLOC:
	  case GOMP_MAP_FIRSTPRIVATE_POINTER:
	  case GOMP_MAP_ALWAYS_POINTER:
	  case GOMP_MAP_ATTACH_DETACH:
	    break;
	  default:
	    error_at (OMP_CLAUSE_LOCATION (*pc),
		      "%<#pragma omp target%> with map-type other "
		      "than %<to%>, %<from%>, %<tofrom%> or %<alloc%> "
		      "on %<map%> clause");
	    *pc = OMP_CLAUSE_CHAIN (*pc);
	    continue;
	  }
      pc = &OMP_CLAUSE_CHAIN (*pc);
    }
  cfun->has_omp_target = true;
  return true;
}

/* OpenMP 4.0:
   # pragma omp declare simd declare-simd-clauses[optseq] new-line

   OpenMP 5.0:
   # pragma omp declare variant (identifier) match(context-selector) new-line
   */

#define OMP_DECLARE_SIMD_CLAUSE_MASK				\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_SIMDLEN)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_LINEAR)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_ALIGNED)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_UNIFORM)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_INBRANCH)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NOTINBRANCH))

static void
c_parser_omp_declare_simd (c_parser *parser, enum pragma_context context)
{
  c_token *token = c_parser_peek_token (parser);
  gcc_assert (token->type == CPP_NAME);
  tree kind = token->value;
  gcc_assert (strcmp (IDENTIFIER_POINTER (kind), "simd") == 0
	      || strcmp (IDENTIFIER_POINTER (kind), "variant") == 0);

  auto_vec<c_token> clauses;
  while (c_parser_next_token_is_not (parser, CPP_PRAGMA_EOL))
    {
      c_token *token = c_parser_peek_token (parser);
      if (token->type == CPP_EOF)
	{
	  c_parser_skip_to_pragma_eol (parser);
	  return;
	}
      clauses.safe_push (*token);
      c_parser_consume_token (parser);
    }
  clauses.safe_push (*c_parser_peek_token (parser));
  c_parser_skip_to_pragma_eol (parser);

  while (c_parser_next_token_is (parser, CPP_PRAGMA))
    {
      if (c_parser_peek_token (parser)->pragma_kind != PRAGMA_OMP_DECLARE
	  || c_parser_peek_2nd_token (parser)->type != CPP_NAME
	  || c_parser_peek_2nd_token (parser)->value != kind)
	{
	  error ("%<#pragma omp declare %s%> must be followed by "
		 "function declaration or definition or another "
		 "%<#pragma omp declare %s%>",
		 IDENTIFIER_POINTER (kind), IDENTIFIER_POINTER (kind));
	  return;
	}
      c_parser_consume_pragma (parser);
      while (c_parser_next_token_is_not (parser, CPP_PRAGMA_EOL))
	{
	  c_token *token = c_parser_peek_token (parser);
	  if (token->type == CPP_EOF)
	    {
	      c_parser_skip_to_pragma_eol (parser);
	      return;
	    }
	  clauses.safe_push (*token);
	  c_parser_consume_token (parser);
	}
      clauses.safe_push (*c_parser_peek_token (parser));
      c_parser_skip_to_pragma_eol (parser);
    }

  /* Make sure nothing tries to read past the end of the tokens.  */
  c_token eof_token;
  memset (&eof_token, 0, sizeof (eof_token));
  eof_token.type = CPP_EOF;
  clauses.safe_push (eof_token);
  clauses.safe_push (eof_token);

  switch (context)
    {
    case pragma_external:
      if (c_parser_next_token_is (parser, CPP_KEYWORD)
	  && c_parser_peek_token (parser)->keyword == RID_EXTENSION)
	{
	  int ext = disable_extension_diagnostics ();
	  do
	    c_parser_consume_token (parser);
	  while (c_parser_next_token_is (parser, CPP_KEYWORD)
		 && c_parser_peek_token (parser)->keyword == RID_EXTENSION);
	  c_parser_declaration_or_fndef (parser, true, true, true, false, true,
					 NULL, clauses);
	  restore_extension_diagnostics (ext);
	}
      else
	c_parser_declaration_or_fndef (parser, true, true, true, false, true,
				       NULL, clauses);
      break;
    case pragma_struct:
    case pragma_param:
    case pragma_stmt:
      error ("%<#pragma omp declare %s%> must be followed by "
	     "function declaration or definition",
	     IDENTIFIER_POINTER (kind));
      break;
    case pragma_compound:
      if (c_parser_next_token_is (parser, CPP_KEYWORD)
	  && c_parser_peek_token (parser)->keyword == RID_EXTENSION)
	{
	  int ext = disable_extension_diagnostics ();
	  do
	    c_parser_consume_token (parser);
	  while (c_parser_next_token_is (parser, CPP_KEYWORD)
		 && c_parser_peek_token (parser)->keyword == RID_EXTENSION);
	  if (c_parser_next_tokens_start_declaration (parser))
	    {
	      c_parser_declaration_or_fndef (parser, true, true, true, true,
					     true, NULL, clauses);
	      restore_extension_diagnostics (ext);
	      break;
	    }
	  restore_extension_diagnostics (ext);
	}
      else if (c_parser_next_tokens_start_declaration (parser))
	{
	  c_parser_declaration_or_fndef (parser, true, true, true, true, true,
					 NULL, clauses);
	  break;
	}
      error ("%<#pragma omp declare %s%> must be followed by "
	     "function declaration or definition",
	     IDENTIFIER_POINTER (kind));
      break;
    default:
      gcc_unreachable ();
    }
}

static const char *const omp_construct_selectors[] = {
  "simd", "target", "teams", "parallel", "for", NULL };
static const char *const omp_device_selectors[] = {
  "kind", "isa", "arch", NULL };
static const char *const omp_implementation_selectors[] = {
  "vendor", "extension", "atomic_default_mem_order", "unified_address",
  "unified_shared_memory", "dynamic_allocators", "reverse_offload", NULL };
static const char *const omp_user_selectors[] = {
  "condition", NULL };

/* OpenMP 5.0:

   trait-selector:
     trait-selector-name[([trait-score:]trait-property[,trait-property[,...]])]

   trait-score:
     score(score-expression)  */

static tree
c_parser_omp_context_selector (c_parser *parser, tree set, tree parms)
{
  tree ret = NULL_TREE;
  do
    {
      tree selector;
      if (c_parser_next_token_is (parser, CPP_KEYWORD)
	  || c_parser_next_token_is (parser, CPP_NAME))
	selector = c_parser_peek_token (parser)->value;
      else
	{
	  c_parser_error (parser, "expected trait selector name");
	  return error_mark_node;
	}

      tree properties = NULL_TREE;
      const char *const *selectors = NULL;
      bool allow_score = true;
      bool allow_user = false;
      int property_limit = 0;
      enum { CTX_PROPERTY_NONE, CTX_PROPERTY_USER, CTX_PROPERTY_NAME_LIST,
	     CTX_PROPERTY_ID, CTX_PROPERTY_EXPR,
	     CTX_PROPERTY_SIMD } property_kind = CTX_PROPERTY_NONE;
      switch (IDENTIFIER_POINTER (set)[0])
	{
	case 'c': /* construct */
	  selectors = omp_construct_selectors;
	  allow_score = false;
	  property_limit = 1;
	  property_kind = CTX_PROPERTY_SIMD;
	  break;
	case 'd': /* device */
	  selectors = omp_device_selectors;
	  allow_score = false;
	  allow_user = true;
	  property_limit = 3;
	  property_kind = CTX_PROPERTY_NAME_LIST;
	  break;
	case 'i': /* implementation */
	  selectors = omp_implementation_selectors;
	  allow_user = true;
	  property_limit = 3;
	  property_kind = CTX_PROPERTY_NAME_LIST;
	  break;
	case 'u': /* user */
	  selectors = omp_user_selectors;
	  property_limit = 1;
	  property_kind = CTX_PROPERTY_EXPR;
	  break;
	default:
	  gcc_unreachable ();
	}
      for (int i = 0; ; i++)
	{
	  if (selectors[i] == NULL)
	    {
	      if (allow_user)
		{
		  property_kind = CTX_PROPERTY_USER;
		  break;
		}
	      else
		{
		  error_at (c_parser_peek_token (parser)->location,
			    "selector %qs not allowed for context selector "
			    "set %qs", IDENTIFIER_POINTER (selector),
			    IDENTIFIER_POINTER (set));
		  c_parser_consume_token (parser);
		  return error_mark_node;
		}
	    }
	  if (i == property_limit)
	    property_kind = CTX_PROPERTY_NONE;
	  if (strcmp (selectors[i], IDENTIFIER_POINTER (selector)) == 0)
	    break;
	}
      if (property_kind == CTX_PROPERTY_NAME_LIST
	  && IDENTIFIER_POINTER (set)[0] == 'i'
	  && strcmp (IDENTIFIER_POINTER (selector),
		     "atomic_default_mem_order") == 0)
	property_kind = CTX_PROPERTY_ID;

      c_parser_consume_token (parser);

      if (c_parser_next_token_is (parser, CPP_OPEN_PAREN))
	{
	  if (property_kind == CTX_PROPERTY_NONE)
	    {
	      error_at (c_parser_peek_token (parser)->location,
			"selector %qs does not accept any properties",
			IDENTIFIER_POINTER (selector));
	      return error_mark_node;
	    }

	  matching_parens parens;
	  parens.require_open (parser);

	  c_token *token = c_parser_peek_token (parser);
	  if (allow_score
	      && c_parser_next_token_is (parser, CPP_NAME)
	      && strcmp (IDENTIFIER_POINTER (token->value), "score") == 0
	      && c_parser_peek_2nd_token (parser)->type == CPP_OPEN_PAREN)
	    {
	      c_parser_consume_token (parser);

	      matching_parens parens2;
	      parens2.require_open (parser);
	      tree score = c_parser_expr_no_commas (parser, NULL).value;
	      parens2.skip_until_found_close (parser);
	      c_parser_require (parser, CPP_COLON, "expected %<:%>");
	      if (score != error_mark_node)
		{
		  mark_exp_read (score);
		  score = c_fully_fold (score, false, NULL);
		  if (!INTEGRAL_TYPE_P (TREE_TYPE (score))
		      || TREE_CODE (score) != INTEGER_CST)
		    error_at (token->location, "score argument must be "
			      "constant integer expression");
		  else if (tree_int_cst_sgn (score) < 0)
		    error_at (token->location, "score argument must be "
			      "non-negative");
		  else
		    properties = tree_cons (get_identifier (" score"),
					    score, properties);
		}
	      token = c_parser_peek_token (parser);
	    }

	  switch (property_kind)
	    {
	      tree t;
	    case CTX_PROPERTY_USER:
	      do
		{
		  t = c_parser_expr_no_commas (parser, NULL).value;
		  if (TREE_CODE (t) == STRING_CST)
		    properties = tree_cons (NULL_TREE, t, properties);
		  else if (t != error_mark_node)
		    {
		      mark_exp_read (t);
		      t = c_fully_fold (t, false, NULL);
		      if (!INTEGRAL_TYPE_P (TREE_TYPE (t))
			  || !tree_fits_shwi_p (t))
			error_at (token->location, "property must be "
				  "constant integer expression or string "
				  "literal");
		      else
			properties = tree_cons (NULL_TREE, t, properties);
		    }
		  else
		    return error_mark_node;

		  if (c_parser_next_token_is (parser, CPP_COMMA))
		    c_parser_consume_token (parser);
		  else
		    break;
		}
	      while (1);
	      break;
	    case CTX_PROPERTY_ID:
	      if (c_parser_next_token_is (parser, CPP_KEYWORD)
		  || c_parser_next_token_is (parser, CPP_NAME))
		{
		  tree prop = c_parser_peek_token (parser)->value;
		  c_parser_consume_token (parser);
		  properties = tree_cons (prop, NULL_TREE, properties);
		}
	      else
		{
		  c_parser_error (parser, "expected identifier");
		  return error_mark_node;
		}
	      break;
	    case CTX_PROPERTY_NAME_LIST:
	      do
		{
		  tree prop = NULL_TREE, value = NULL_TREE;
		  if (c_parser_next_token_is (parser, CPP_KEYWORD)
		      || c_parser_next_token_is (parser, CPP_NAME))
		    {
		      prop = c_parser_peek_token (parser)->value;
		      c_parser_consume_token (parser);
		    }
		  else if (c_parser_next_token_is (parser, CPP_STRING))
		    value = c_parser_string_literal (parser, false,
						     false).value;
		  else
		    {
		      c_parser_error (parser, "expected identifier or "
					      "string literal");
		      return error_mark_node;
		    }

		  properties = tree_cons (prop, value, properties);

		  if (c_parser_next_token_is (parser, CPP_COMMA))
		    c_parser_consume_token (parser);
		  else
		    break;
		}
	      while (1);
	      break;
	    case CTX_PROPERTY_EXPR:
	      t = c_parser_expr_no_commas (parser, NULL).value;
	      if (t != error_mark_node)
		{
		  mark_exp_read (t);
		  t = c_fully_fold (t, false, NULL);
		  if (!INTEGRAL_TYPE_P (TREE_TYPE (t))
		      || !tree_fits_shwi_p (t))
		    error_at (token->location, "property must be "
			      "constant integer expression");
		  else
		    properties = tree_cons (NULL_TREE, t, properties);
		}
	      else
		return error_mark_node;
	      break;
	    case CTX_PROPERTY_SIMD:
	      if (parms == NULL_TREE)
		{
		  error_at (token->location, "properties for %<simd%> "
			    "selector may not be specified in "
			    "%<metadirective%>");
		  return error_mark_node;
		}
	      tree c;
	      c = c_parser_omp_all_clauses (parser,
					    OMP_DECLARE_SIMD_CLAUSE_MASK,
					    "simd", true, 2);
	      c = c_omp_declare_simd_clauses_to_numbers (parms
							 == error_mark_node
							 ? NULL_TREE : parms,
							 c);
	      properties = c;
	      break;
	    default:
	      gcc_unreachable ();
	    }

	  parens.skip_until_found_close (parser);
	  properties = nreverse (properties);
	}
      else if (property_kind == CTX_PROPERTY_NAME_LIST
	       || property_kind == CTX_PROPERTY_ID
	       || property_kind == CTX_PROPERTY_EXPR)
	{
	  c_parser_require (parser, CPP_OPEN_PAREN, "expected %<(%>");
	  return error_mark_node;
	}

      ret = tree_cons (selector, properties, ret);

      if (c_parser_next_token_is (parser, CPP_COMMA))
	c_parser_consume_token (parser);
      else
	break;
    }
  while (1);

  return nreverse (ret);
}

/* OpenMP 5.0:

   trait-set-selector[,trait-set-selector[,...]]

   trait-set-selector:
     trait-set-selector-name = { trait-selector[, trait-selector[, ...]] }

   trait-set-selector-name:
     constructor
     device
     implementation
     user  */

static tree
c_parser_omp_context_selector_specification (c_parser *parser, tree parms)
{
  tree ret = NULL_TREE;
  do
    {
      const char *setp = "";
      if (c_parser_next_token_is (parser, CPP_NAME))
	setp = IDENTIFIER_POINTER (c_parser_peek_token (parser)->value);
      switch (setp[0])
	{
	case 'c':
	  if (strcmp (setp, "construct") == 0)
	    setp = NULL;
	  break;
	case 'd':
	  if (strcmp (setp, "device") == 0)
	    setp = NULL;
	  break;
	case 'i':
	  if (strcmp (setp, "implementation") == 0)
	    setp = NULL;
	  break;
	case 'u':
	  if (strcmp (setp, "user") == 0)
	    setp = NULL;
	  break;
	default:
	  break;
	}
      if (setp)
	{
	  c_parser_error (parser, "expected %<construct%>, %<device%>, "
				  "%<implementation%> or %<user%>");
	  return error_mark_node;
	}

      tree set = c_parser_peek_token (parser)->value;
      c_parser_consume_token (parser);

      if (!c_parser_require (parser, CPP_EQ, "expected %<=%>"))
	return error_mark_node;

      matching_braces braces;
      if (!braces.require_open (parser))
	return error_mark_node;

      tree selectors = c_parser_omp_context_selector (parser, set, parms);
      if (selectors == error_mark_node)
	ret = error_mark_node;
      else if (ret != error_mark_node)
	ret = tree_cons (set, selectors, ret);

      braces.skip_until_found_close (parser);

      if (c_parser_next_token_is (parser, CPP_COMMA))
	c_parser_consume_token (parser);
      else
	break;
    }
  while (1);

  if (ret == error_mark_node)
    return ret;
  return nreverse (ret);
}

/* Finalize #pragma omp declare variant after FNDECL has been parsed, and put
   that into "omp declare variant base" attribute.  */

static void
c_finish_omp_declare_variant (c_parser *parser, tree fndecl, tree parms)
{
  matching_parens parens;
  if (!parens.require_open (parser))
    {
     fail:
      c_parser_skip_to_pragma_eol (parser, false);
      return;
    }

  if (c_parser_next_token_is_not (parser, CPP_NAME)
      || c_parser_peek_token (parser)->id_kind != C_ID_ID)
    {
      c_parser_error (parser, "expected identifier");
      goto fail;
    }

  c_token *token = c_parser_peek_token (parser);
  tree variant = lookup_name (token->value);

  if (variant == NULL_TREE)
    {
      undeclared_variable (token->location, token->value);
      variant = error_mark_node;
    }

  c_parser_consume_token (parser);

  parens.require_close (parser);

  const char *clause = "";
  location_t match_loc = c_parser_peek_token (parser)->location;
  if (c_parser_next_token_is (parser, CPP_NAME))
    clause = IDENTIFIER_POINTER (c_parser_peek_token (parser)->value);
  if (strcmp (clause, "match"))
    {
      c_parser_error (parser, "expected %<match%>");
      goto fail;
    }

  c_parser_consume_token (parser);

  if (!parens.require_open (parser))
    goto fail;

  if (parms == NULL_TREE)
    parms = error_mark_node;

  tree ctx = c_parser_omp_context_selector_specification (parser, parms);
  if (ctx == error_mark_node)
    goto fail;
  ctx = c_omp_check_context_selector (match_loc, ctx);
  if (ctx != error_mark_node && variant != error_mark_node)
    {
      if (TREE_CODE (variant) != FUNCTION_DECL)
	{
	  error_at (token->location, "variant %qD is not a function", variant);
	  variant = error_mark_node;
	}
      else if (omp_get_context_selector (ctx, "construct", "simd") == NULL_TREE
	       && !comptypes (TREE_TYPE (fndecl), TREE_TYPE (variant)))
	{
	  error_at (token->location, "variant %qD and base %qD have "
				     "incompatible types", variant, fndecl);
	  variant = error_mark_node;
	}
      else if (fndecl_built_in_p (variant)
	       && (strncmp (IDENTIFIER_POINTER (DECL_NAME (variant)),
			    "__builtin_", strlen ("__builtin_")) == 0
		   || strncmp (IDENTIFIER_POINTER (DECL_NAME (variant)),
			       "__sync_", strlen ("__sync_")) == 0
		   || strncmp (IDENTIFIER_POINTER (DECL_NAME (variant)),
			       "__atomic_", strlen ("__atomic_")) == 0))
	{
	  error_at (token->location, "variant %qD is a built-in", variant);
	  variant = error_mark_node;
	}
      if (variant != error_mark_node)
	{
	  C_DECL_USED (variant) = 1;
	  tree construct = omp_get_context_selector (ctx, "construct", NULL);
	  c_omp_mark_declare_variant (match_loc, variant, construct);
	  if (omp_context_selector_matches (ctx))
	    {
	      tree attr
		= tree_cons (get_identifier ("omp declare variant base"),
			     build_tree_list (variant, ctx),
			     DECL_ATTRIBUTES (fndecl));
	      DECL_ATTRIBUTES (fndecl) = attr;
	    }
	}
    }

  parens.require_close (parser);
  c_parser_skip_to_pragma_eol (parser);
}

/* Finalize #pragma omp declare simd or #pragma omp declare variant
   clauses after FNDECL has been parsed, and put that into "omp declare simd"
   or "omp declare variant base" attribute.  */

static void
c_finish_omp_declare_simd (c_parser *parser, tree fndecl, tree parms,
			   vec<c_token> clauses)
{
  /* Normally first token is CPP_NAME "simd" or "variant".  CPP_EOF there
     indicates error has been reported and CPP_PRAGMA that
     c_finish_omp_declare_simd has already processed the tokens.  */
  if (clauses.exists () && clauses[0].type == CPP_EOF)
    return;
  const char *kind = "simd";
  if (clauses.exists ()
      && (clauses[0].type == CPP_NAME || clauses[0].type == CPP_PRAGMA))
    kind = IDENTIFIER_POINTER (clauses[0].value);
  gcc_assert (strcmp (kind, "simd") == 0 || strcmp (kind, "variant") == 0);
  if (fndecl == NULL_TREE || TREE_CODE (fndecl) != FUNCTION_DECL)
    {
      error ("%<#pragma omp declare %s%> not immediately followed by "
	     "a function declaration or definition", kind);
      clauses[0].type = CPP_EOF;
      return;
    }
  if (clauses.exists () && clauses[0].type != CPP_NAME)
    {
      error_at (DECL_SOURCE_LOCATION (fndecl),
		"%<#pragma omp declare %s%> not immediately followed by "
		"a single function declaration or definition", kind);
      clauses[0].type = CPP_EOF;
      return;
    }

  if (parms == NULL_TREE)
    parms = DECL_ARGUMENTS (fndecl);

  unsigned int tokens_avail = parser->tokens_avail;
  gcc_assert (parser->tokens == &parser->tokens_buf[0]);

  parser->tokens = clauses.address ();
  parser->tokens_avail = clauses.length ();
  
  /* c_parser_omp_declare_simd pushed 2 extra CPP_EOF tokens at the end.  */
  while (parser->tokens_avail > 3)
    {
      c_token *token = c_parser_peek_token (parser);
      gcc_assert (token->type == CPP_NAME
		  && strcmp (IDENTIFIER_POINTER (token->value), kind) == 0);
      c_parser_consume_token (parser);
      parser->in_pragma = true;

      if (strcmp (kind, "simd") == 0)
	{
	  tree c;
	  c = c_parser_omp_all_clauses (parser, OMP_DECLARE_SIMD_CLAUSE_MASK,
					"#pragma omp declare simd");
	  c = c_omp_declare_simd_clauses_to_numbers (parms, c);
	  if (c != NULL_TREE)
	    c = tree_cons (NULL_TREE, c, NULL_TREE);
	  c = build_tree_list (get_identifier ("omp declare simd"), c);
	  TREE_CHAIN (c) = DECL_ATTRIBUTES (fndecl);
	  DECL_ATTRIBUTES (fndecl) = c;
	}
      else
	{
	  gcc_assert (strcmp (kind, "variant") == 0);
	  c_finish_omp_declare_variant (parser, fndecl, parms);
	}
    }

  parser->tokens = &parser->tokens_buf[0];
  parser->tokens_avail = tokens_avail;
  if (clauses.exists ())
    clauses[0].type = CPP_PRAGMA;
}


/* OpenMP 4.0:
   # pragma omp declare target new-line
   declarations and definitions
   # pragma omp end declare target new-line

   OpenMP 4.5:
   # pragma omp declare target ( extended-list ) new-line

   # pragma omp declare target declare-target-clauses[seq] new-line  */

#define OMP_DECLARE_TARGET_CLAUSE_MASK				\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_TO)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_LINK)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_DEVICE_TYPE))

static void
c_parser_omp_declare_target (c_parser *parser)
{
  tree clauses = NULL_TREE;
  int device_type = 0;
  bool only_device_type = true;
  if (c_parser_next_token_is (parser, CPP_NAME))
    clauses = c_parser_omp_all_clauses (parser, OMP_DECLARE_TARGET_CLAUSE_MASK,
					"#pragma omp declare target");
  else if (c_parser_next_token_is (parser, CPP_OPEN_PAREN))
    {
      clauses = c_parser_omp_var_list_parens (parser, OMP_CLAUSE_TO_DECLARE,
					      clauses);
      clauses = c_finish_omp_clauses (clauses, C_ORT_OMP);
      c_parser_skip_to_pragma_eol (parser);
    }
  else
    {
      c_parser_skip_to_pragma_eol (parser);
      current_omp_declare_target_attribute++;
      return;
    }
  for (tree c = clauses; c; c = OMP_CLAUSE_CHAIN (c))
    if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_DEVICE_TYPE)
      device_type |= OMP_CLAUSE_DEVICE_TYPE_KIND (c);
  for (tree c = clauses; c; c = OMP_CLAUSE_CHAIN (c))
    {
      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_DEVICE_TYPE)
	continue;
      tree t = OMP_CLAUSE_DECL (c), id;
      tree at1 = lookup_attribute ("omp declare target", DECL_ATTRIBUTES (t));
      tree at2 = lookup_attribute ("omp declare target link",
				   DECL_ATTRIBUTES (t));
      only_device_type = false;
      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LINK)
	{
	  id = get_identifier ("omp declare target link");
	  std::swap (at1, at2);
	}
      else
	id = get_identifier ("omp declare target");
      if (at2)
	{
	  error_at (OMP_CLAUSE_LOCATION (c),
		    "%qD specified both in declare target %<link%> and %<to%>"
		    " clauses", t);
	  continue;
	}
      if (!at1)
	{
	  DECL_ATTRIBUTES (t) = tree_cons (id, NULL_TREE, DECL_ATTRIBUTES (t));
	  if (TREE_CODE (t) != FUNCTION_DECL && !is_global_var (t))
	    continue;

	  symtab_node *node = symtab_node::get (t);
	  if (node != NULL)
	    {
	      node->offloadable = 1;
	      if (ENABLE_OFFLOADING)
		{
		  g->have_offload = true;
		  if (is_a <varpool_node *> (node))
		    vec_safe_push (offload_vars, t);
		}
	    }
	}
      if (TREE_CODE (t) != FUNCTION_DECL)
	continue;
      if ((device_type & OMP_CLAUSE_DEVICE_TYPE_HOST) != 0)
	{
	  tree at3 = lookup_attribute ("omp declare target host",
				       DECL_ATTRIBUTES (t));
	  if (at3 == NULL_TREE)
	    {
	      id = get_identifier ("omp declare target host");
	      DECL_ATTRIBUTES (t)
		= tree_cons (id, NULL_TREE, DECL_ATTRIBUTES (t));
	    }
	}
      if ((device_type & OMP_CLAUSE_DEVICE_TYPE_NOHOST) != 0)
	{
	  tree at3 = lookup_attribute ("omp declare target nohost",
				       DECL_ATTRIBUTES (t));
	  if (at3 == NULL_TREE)
	    {
	      id = get_identifier ("omp declare target nohost");
	      DECL_ATTRIBUTES (t)
		= tree_cons (id, NULL_TREE, DECL_ATTRIBUTES (t));
	    }
	}
    }
  if (device_type && only_device_type)
    warning_at (OMP_CLAUSE_LOCATION (clauses), 0,
		"directive with only %<device_type%> clauses ignored");
}

static void
c_parser_omp_end_declare_target (c_parser *parser)
{
  location_t loc = c_parser_peek_token (parser)->location;
  c_parser_consume_pragma (parser);
  if (c_parser_next_token_is (parser, CPP_NAME)
      && strcmp (IDENTIFIER_POINTER (c_parser_peek_token (parser)->value),
		 "declare") == 0)
    {
      c_parser_consume_token (parser);
      if (c_parser_next_token_is (parser, CPP_NAME)
	  && strcmp (IDENTIFIER_POINTER (c_parser_peek_token (parser)->value),
		     "target") == 0)
	c_parser_consume_token (parser);
      else
	{
	  c_parser_error (parser, "expected %<target%>");
	  c_parser_skip_to_pragma_eol (parser);
	  return;
	}
    }
  else
    {
      c_parser_error (parser, "expected %<declare%>");
      c_parser_skip_to_pragma_eol (parser);
      return;
    }
  c_parser_skip_to_pragma_eol (parser);
  if (!current_omp_declare_target_attribute)
    error_at (loc, "%<#pragma omp end declare target%> without corresponding "
		   "%<#pragma omp declare target%>");
  else
    current_omp_declare_target_attribute--;
}


/* OpenMP 4.0
   #pragma omp declare reduction (reduction-id : typename-list : expression) \
      initializer-clause[opt] new-line

   initializer-clause:
      initializer (omp_priv = initializer)
      initializer (function-name (argument-list))  */

static void
c_parser_omp_declare_reduction (c_parser *parser, enum pragma_context context)
{
  unsigned int tokens_avail = 0, i;
  vec<tree> types = vNULL;
  vec<c_token> clauses = vNULL;
  enum tree_code reduc_code = ERROR_MARK;
  tree reduc_id = NULL_TREE;
  tree type;
  location_t rloc = c_parser_peek_token (parser)->location;

  if (context == pragma_struct || context == pragma_param)
    {
      error ("%<#pragma omp declare reduction%> not at file or block scope");
      goto fail;
    }

  if (!c_parser_require (parser, CPP_OPEN_PAREN, "expected %<(%>"))
    goto fail;

  switch (c_parser_peek_token (parser)->type)
    {
    case CPP_PLUS:
      reduc_code = PLUS_EXPR;
      break;
    case CPP_MULT:
      reduc_code = MULT_EXPR;
      break;
    case CPP_MINUS:
      reduc_code = MINUS_EXPR;
      break;
    case CPP_AND:
      reduc_code = BIT_AND_EXPR;
      break;
    case CPP_XOR:
      reduc_code = BIT_XOR_EXPR;
      break;
    case CPP_OR:
      reduc_code = BIT_IOR_EXPR;
      break;
    case CPP_AND_AND:
      reduc_code = TRUTH_ANDIF_EXPR;
      break;
    case CPP_OR_OR:
      reduc_code = TRUTH_ORIF_EXPR;
      break;
    case CPP_NAME:
      const char *p;
      p = IDENTIFIER_POINTER (c_parser_peek_token (parser)->value);
      if (strcmp (p, "min") == 0)
	{
	  reduc_code = MIN_EXPR;
	  break;
	}
      if (strcmp (p, "max") == 0)
	{
	  reduc_code = MAX_EXPR;
	  break;
	}
      reduc_id = c_parser_peek_token (parser)->value;
      break;
    default:
      c_parser_error (parser,
		      "expected %<+%>, %<*%>, %<-%>, %<&%>, "
		      "%<^%>, %<|%>, %<&&%>, %<||%> or identifier");
      goto fail;
    }

  tree orig_reduc_id, reduc_decl;
  orig_reduc_id = reduc_id;
  reduc_id = c_omp_reduction_id (reduc_code, reduc_id);
  reduc_decl = c_omp_reduction_decl (reduc_id);
  c_parser_consume_token (parser);

  if (!c_parser_require (parser, CPP_COLON, "expected %<:%>"))
    goto fail;

  while (true)
    {
      location_t loc = c_parser_peek_token (parser)->location;
      struct c_type_name *ctype = c_parser_type_name (parser);
      if (ctype != NULL)
	{
	  type = groktypename (ctype, NULL, NULL);
	  if (type == error_mark_node)
	    ;
	  else if ((INTEGRAL_TYPE_P (type)
		    || TREE_CODE (type) == REAL_TYPE
		    || TREE_CODE (type) == COMPLEX_TYPE)
		   && orig_reduc_id == NULL_TREE)
	    error_at (loc, "predeclared arithmetic type in "
			   "%<#pragma omp declare reduction%>");
	  else if (TREE_CODE (type) == FUNCTION_TYPE
		   || TREE_CODE (type) == ARRAY_TYPE)
	    error_at (loc, "function or array type in "
		      "%<#pragma omp declare reduction%>");
	  else if (TYPE_ATOMIC (type))
	    error_at (loc, "%<_Atomic%> qualified type in "
			   "%<#pragma omp declare reduction%>");
	  else if (TYPE_QUALS_NO_ADDR_SPACE (type))
	    error_at (loc, "const, volatile or restrict qualified type in "
			   "%<#pragma omp declare reduction%>");
	  else
	    {
	      tree t;
	      for (t = DECL_INITIAL (reduc_decl); t; t = TREE_CHAIN (t))
		if (comptypes (TREE_PURPOSE (t), type))
		  {
		    error_at (loc, "redeclaration of %qs "
				   "%<#pragma omp declare reduction%> for "
				   "type %qT",
				   IDENTIFIER_POINTER (reduc_id)
				   + sizeof ("omp declare reduction ") - 1,
				   type);
		    location_t ploc
		      = DECL_SOURCE_LOCATION (TREE_VEC_ELT (TREE_VALUE (t),
							    0));
		    error_at (ploc, "previous %<#pragma omp declare "
				    "reduction%>");
		    break;
		  }
	      if (t == NULL_TREE)
		types.safe_push (type);
	    }
	  if (c_parser_next_token_is (parser, CPP_COMMA))
	    c_parser_consume_token (parser);
	  else
	    break;
	}
      else
	break;
    }

  if (!c_parser_require (parser, CPP_COLON, "expected %<:%>")
      || types.is_empty ())
    {
     fail:
      clauses.release ();
      types.release ();
      while (true)
	{
	  c_token *token = c_parser_peek_token (parser);
	  if (token->type == CPP_EOF || token->type == CPP_PRAGMA_EOL)
	    break;
	  c_parser_consume_token (parser);
	}
      c_parser_skip_to_pragma_eol (parser);
      return;
    }

  if (types.length () > 1)
    {
      while (c_parser_next_token_is_not (parser, CPP_PRAGMA_EOL))
	{
	  c_token *token = c_parser_peek_token (parser);
	  if (token->type == CPP_EOF)
	    goto fail;
	  clauses.safe_push (*token);
	  c_parser_consume_token (parser);
	}
      clauses.safe_push (*c_parser_peek_token (parser));
      c_parser_skip_to_pragma_eol (parser);

      /* Make sure nothing tries to read past the end of the tokens.  */
      c_token eof_token;
      memset (&eof_token, 0, sizeof (eof_token));
      eof_token.type = CPP_EOF;
      clauses.safe_push (eof_token);
      clauses.safe_push (eof_token);
    }

  int errs = errorcount;
  FOR_EACH_VEC_ELT (types, i, type)
    {
      tokens_avail = parser->tokens_avail;
      gcc_assert (parser->tokens == &parser->tokens_buf[0]);
      if (!clauses.is_empty ())
	{
	  parser->tokens = clauses.address ();
	  parser->tokens_avail = clauses.length ();
	  parser->in_pragma = true;
	}

      bool nested = current_function_decl != NULL_TREE;
      if (nested)
	c_push_function_context ();
      tree fndecl = build_decl (BUILTINS_LOCATION, FUNCTION_DECL,
				reduc_id, default_function_type);
      current_function_decl = fndecl;
      allocate_struct_function (fndecl, true);
      push_scope ();
      tree stmt = push_stmt_list ();
      /* Intentionally BUILTINS_LOCATION, so that -Wshadow doesn't
	 warn about these.  */
      tree omp_out = build_decl (BUILTINS_LOCATION, VAR_DECL,
				 get_identifier ("omp_out"), type);
      DECL_ARTIFICIAL (omp_out) = 1;
      DECL_CONTEXT (omp_out) = fndecl;
      pushdecl (omp_out);
      tree omp_in = build_decl (BUILTINS_LOCATION, VAR_DECL,
				get_identifier ("omp_in"), type);
      DECL_ARTIFICIAL (omp_in) = 1;
      DECL_CONTEXT (omp_in) = fndecl;
      pushdecl (omp_in);
      struct c_expr combiner = c_parser_expression (parser);
      struct c_expr initializer;
      tree omp_priv = NULL_TREE, omp_orig = NULL_TREE;
      bool bad = false;
      initializer.set_error ();
      if (!c_parser_require (parser, CPP_CLOSE_PAREN, "expected %<)%>"))
	bad = true;
      else if (c_parser_next_token_is (parser, CPP_NAME)
	       && strcmp (IDENTIFIER_POINTER
				(c_parser_peek_token (parser)->value),
			  "initializer") == 0)
	{
	  c_parser_consume_token (parser);
	  pop_scope ();
	  push_scope ();
	  omp_priv = build_decl (BUILTINS_LOCATION, VAR_DECL,
				 get_identifier ("omp_priv"), type);
	  DECL_ARTIFICIAL (omp_priv) = 1;
	  DECL_INITIAL (omp_priv) = error_mark_node;
	  DECL_CONTEXT (omp_priv) = fndecl;
	  pushdecl (omp_priv);
	  omp_orig = build_decl (BUILTINS_LOCATION, VAR_DECL,
				 get_identifier ("omp_orig"), type);
	  DECL_ARTIFICIAL (omp_orig) = 1;
	  DECL_CONTEXT (omp_orig) = fndecl;
	  pushdecl (omp_orig);
	  if (!c_parser_require (parser, CPP_OPEN_PAREN, "expected %<(%>"))
	    bad = true;
	  else if (!c_parser_next_token_is (parser, CPP_NAME))
	    {
	      c_parser_error (parser, "expected %<omp_priv%> or "
				      "function-name");
	      bad = true;
	    }
	  else if (strcmp (IDENTIFIER_POINTER
				(c_parser_peek_token (parser)->value),
			   "omp_priv") != 0)
	    {
	      if (c_parser_peek_2nd_token (parser)->type != CPP_OPEN_PAREN
		  || c_parser_peek_token (parser)->id_kind != C_ID_ID)
		{
		  c_parser_error (parser, "expected function-name %<(%>");
		  bad = true;
		}
	      else
		initializer = c_parser_postfix_expression (parser);
	      if (initializer.value
		  && TREE_CODE (initializer.value) == CALL_EXPR)
		{
		  int j;
		  tree c = initializer.value;
		  for (j = 0; j < call_expr_nargs (c); j++)
		    {
		      tree a = CALL_EXPR_ARG (c, j);
		      STRIP_NOPS (a);
		      if (TREE_CODE (a) == ADDR_EXPR
			  && TREE_OPERAND (a, 0) == omp_priv)
			break;
		    }
		  if (j == call_expr_nargs (c))
		    error ("one of the initializer call arguments should be "
			   "%<&omp_priv%>");
		}
	    }
	  else
	    {
	      c_parser_consume_token (parser);
	      if (!c_parser_require (parser, CPP_EQ, "expected %<=%>"))
		bad = true;
	      else
		{
		  tree st = push_stmt_list ();
		  location_t loc = c_parser_peek_token (parser)->location;
		  rich_location richloc (line_table, loc);
		  start_init (omp_priv, NULL_TREE, 0, &richloc);
		  struct c_expr init = c_parser_initializer (parser);
		  finish_init ();
		  finish_decl (omp_priv, loc, init.value,
		      	       init.original_type, NULL_TREE);
		  pop_stmt_list (st);
		}
	    }
	  if (!bad
	      && !c_parser_require (parser, CPP_CLOSE_PAREN, "expected %<)%>"))
	    bad = true;
	}

      if (!bad)
	{
	  c_parser_skip_to_pragma_eol (parser);

	  tree t = tree_cons (type, make_tree_vec (omp_priv ? 6 : 3),
			      DECL_INITIAL (reduc_decl));
	  DECL_INITIAL (reduc_decl) = t;
	  DECL_SOURCE_LOCATION (omp_out) = rloc;
	  TREE_VEC_ELT (TREE_VALUE (t), 0) = omp_out;
	  TREE_VEC_ELT (TREE_VALUE (t), 1) = omp_in;
	  TREE_VEC_ELT (TREE_VALUE (t), 2) = combiner.value;
	  walk_tree (&combiner.value, c_check_omp_declare_reduction_r,
		     &TREE_VEC_ELT (TREE_VALUE (t), 0), NULL);
	  if (omp_priv)
	    {
	      DECL_SOURCE_LOCATION (omp_priv) = rloc;
	      TREE_VEC_ELT (TREE_VALUE (t), 3) = omp_priv;
	      TREE_VEC_ELT (TREE_VALUE (t), 4) = omp_orig;
	      TREE_VEC_ELT (TREE_VALUE (t), 5) = initializer.value;
	      walk_tree (&initializer.value, c_check_omp_declare_reduction_r,
			 &TREE_VEC_ELT (TREE_VALUE (t), 3), NULL);
	      walk_tree (&DECL_INITIAL (omp_priv),
			 c_check_omp_declare_reduction_r,
			 &TREE_VEC_ELT (TREE_VALUE (t), 3), NULL);
	    }
	}

      pop_stmt_list (stmt);
      pop_scope ();
      if (cfun->language != NULL)
	{
	  ggc_free (cfun->language);
	  cfun->language = NULL;
	}
      set_cfun (NULL);
      current_function_decl = NULL_TREE;
      if (nested)
	c_pop_function_context ();

      if (!clauses.is_empty ())
	{
	  parser->tokens = &parser->tokens_buf[0];
	  parser->tokens_avail = tokens_avail;
	}
      if (bad)
	goto fail;
      if (errs != errorcount)
	break;
    }

  clauses.release ();
  types.release ();
}


/* OpenMP 4.0
   #pragma omp declare simd declare-simd-clauses[optseq] new-line
   #pragma omp declare reduction (reduction-id : typename-list : expression) \
      initializer-clause[opt] new-line
   #pragma omp declare target new-line

   OpenMP 5.0
   #pragma omp declare variant (identifier) match (context-selector)  */

static void
c_parser_omp_declare (c_parser *parser, enum pragma_context context)
{
  c_parser_consume_pragma (parser);
  if (c_parser_next_token_is (parser, CPP_NAME))
    {
      const char *p = IDENTIFIER_POINTER (c_parser_peek_token (parser)->value);
      if (strcmp (p, "simd") == 0)
	{
	  /* c_parser_consume_token (parser); done in
	     c_parser_omp_declare_simd.  */
	  c_parser_omp_declare_simd (parser, context);
	  return;
	}
      if (strcmp (p, "reduction") == 0)
	{
	  c_parser_consume_token (parser);
	  c_parser_omp_declare_reduction (parser, context);
	  return;
	}
      if (!flag_openmp)  /* flag_openmp_simd  */
	{
	  c_parser_skip_to_pragma_eol (parser, false);
	  return;
	}
      if (strcmp (p, "target") == 0)
	{
	  c_parser_consume_token (parser);
	  c_parser_omp_declare_target (parser);
	  return;
	}
      if (strcmp (p, "variant") == 0)
	{
	  /* c_parser_consume_token (parser); done in
	     c_parser_omp_declare_simd.  */
	  c_parser_omp_declare_simd (parser, context);
	  return;
	}
    }

  c_parser_error (parser, "expected %<simd%>, %<reduction%>, "
			  "%<target%> or %<variant%>");
  c_parser_skip_to_pragma_eol (parser);
}

/* OpenMP 5.0
   #pragma omp requires clauses[optseq] new-line  */

static void
c_parser_omp_requires (c_parser *parser)
{
  bool first = true;
  enum omp_requires new_req = (enum omp_requires) 0;

  c_parser_consume_pragma (parser);

  location_t loc = c_parser_peek_token (parser)->location;
  while (c_parser_next_token_is_not (parser, CPP_PRAGMA_EOL))
    {
      if (!first && c_parser_next_token_is (parser, CPP_COMMA))
	c_parser_consume_token (parser);

      first = false;

      if (c_parser_next_token_is (parser, CPP_NAME))
	{
	  const char *p
	    = IDENTIFIER_POINTER (c_parser_peek_token (parser)->value);
	  location_t cloc = c_parser_peek_token (parser)->location;
	  enum omp_requires this_req = (enum omp_requires) 0;

	  if (!strcmp (p, "unified_address"))
	    this_req = OMP_REQUIRES_UNIFIED_ADDRESS;
	  else if (!strcmp (p, "unified_shared_memory"))
	    this_req = OMP_REQUIRES_UNIFIED_SHARED_MEMORY;
	  else if (!strcmp (p, "dynamic_allocators"))
	    this_req = OMP_REQUIRES_DYNAMIC_ALLOCATORS;
	  else if (!strcmp (p, "reverse_offload"))
	    this_req = OMP_REQUIRES_REVERSE_OFFLOAD;
	  else if (!strcmp (p, "atomic_default_mem_order"))
	    {
	      c_parser_consume_token (parser);

	      matching_parens parens;
	      if (parens.require_open (parser))
		{
		  if (c_parser_next_token_is (parser, CPP_NAME))
		    {
		      tree v = c_parser_peek_token (parser)->value;
		      p = IDENTIFIER_POINTER (v);

		      if (!strcmp (p, "seq_cst"))
			this_req
			  = (enum omp_requires) OMP_MEMORY_ORDER_SEQ_CST;
		      else if (!strcmp (p, "relaxed"))
			this_req
			  = (enum omp_requires) OMP_MEMORY_ORDER_RELAXED;
		      else if (!strcmp (p, "acq_rel"))
			this_req
			  = (enum omp_requires) OMP_MEMORY_ORDER_ACQ_REL;
		    }
		  if (this_req == 0)
		    {
		      error_at (c_parser_peek_token (parser)->location,
				"expected %<seq_cst%>, %<relaxed%> or "
				"%<acq_rel%>");
		      if (c_parser_peek_2nd_token (parser)->type
			  == CPP_CLOSE_PAREN)
			c_parser_consume_token (parser);
		    }
		  else
		    c_parser_consume_token (parser);

		  parens.skip_until_found_close (parser);
		  if (this_req == 0)
		    {
		      c_parser_skip_to_pragma_eol (parser, false);
		      return;
		    }
		}
	      p = NULL;
	    }
	  else
	    {
	      error_at (cloc, "expected %<unified_address%>, "
			      "%<unified_shared_memory%>, "
			      "%<dynamic_allocators%>, "
			       "%<reverse_offload%> "
			       "or %<atomic_default_mem_order%> clause");
	      c_parser_skip_to_pragma_eol (parser, false);
	      return;
	    }
	  if (p)
	    sorry_at (cloc, "%qs clause on %<requires%> directive not "
			    "supported yet", p);
	  if (p)
	    c_parser_consume_token (parser);
	  if (this_req)
	    {
	      if ((this_req & ~OMP_REQUIRES_ATOMIC_DEFAULT_MEM_ORDER) != 0)
		{
		  if ((this_req & new_req) != 0)
		    error_at (cloc, "too many %qs clauses", p);
		  if (this_req != OMP_REQUIRES_DYNAMIC_ALLOCATORS
		      && (omp_requires_mask & OMP_REQUIRES_TARGET_USED) != 0)
		    error_at (cloc, "%qs clause used lexically after first "
				    "target construct or offloading API", p);
		}
	      else if ((new_req & OMP_REQUIRES_ATOMIC_DEFAULT_MEM_ORDER) != 0)
		{
		  error_at (cloc, "too many %qs clauses",
			    "atomic_default_mem_order");
		  this_req = (enum omp_requires) 0;
		}
	      else if ((omp_requires_mask
			& OMP_REQUIRES_ATOMIC_DEFAULT_MEM_ORDER) != 0)
		{
		  error_at (cloc, "more than one %<atomic_default_mem_order%>"
				  " clause in a single compilation unit");
		  this_req
		    = (enum omp_requires)
		       (omp_requires_mask
			& OMP_REQUIRES_ATOMIC_DEFAULT_MEM_ORDER);
		}
	      else if ((omp_requires_mask
			& OMP_REQUIRES_ATOMIC_DEFAULT_MEM_ORDER_USED) != 0)
		error_at (cloc, "%<atomic_default_mem_order%> clause used "
				"lexically after first %<atomic%> construct "
				"without memory order clause");
	      new_req = (enum omp_requires) (new_req | this_req);
	      omp_requires_mask
		= (enum omp_requires) (omp_requires_mask | this_req);
	      continue;
	    }
	}
      break;
    }
  c_parser_skip_to_pragma_eol (parser);

  if (new_req == 0)
    error_at (loc, "%<pragma omp requires%> requires at least one clause");
}

/* Helper function for c_parser_omp_taskloop.
   Disallow zero sized or potentially zero sized task reductions.  */

static tree
c_finish_taskloop_clauses (tree clauses)
{
  tree *pc = &clauses;
  for (tree c = clauses; c; c = *pc)
    {
      bool remove = false;
      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_REDUCTION)
	{
	  tree type = strip_array_types (TREE_TYPE (OMP_CLAUSE_DECL (c)));
	  if (integer_zerop (TYPE_SIZE_UNIT (type)))
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"zero sized type %qT in %<reduction%> clause", type);
	      remove = true;
	    }
	  else if (TREE_CODE (TYPE_SIZE_UNIT (type)) != INTEGER_CST)
	    {
	      error_at (OMP_CLAUSE_LOCATION (c),
			"variable sized type %qT in %<reduction%> clause",
			type);
	      remove = true;
	    }
	}
      if (remove)
	*pc = OMP_CLAUSE_CHAIN (c);
      else
	pc = &OMP_CLAUSE_CHAIN (c);
    }
  return clauses;
}

/* OpenMP 4.5:
   #pragma omp taskloop taskloop-clause[optseq] new-line
     for-loop

   #pragma omp taskloop simd taskloop-simd-clause[optseq] new-line
     for-loop  */

#define OMP_TASKLOOP_CLAUSE_MASK				\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_SHARED)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_PRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_FIRSTPRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_LASTPRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_DEFAULT)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_GRAINSIZE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NUM_TASKS)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_COLLAPSE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_UNTIED)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_IF)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_FINAL)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_MERGEABLE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NOGROUP)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_PRIORITY)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_ALLOCATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_REDUCTION)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_IN_REDUCTION))

static tree
c_parser_omp_taskloop (location_t loc, c_parser *parser,
		       char *p_name, omp_clause_mask mask, tree *cclauses,
		       bool *if_p)
{
  tree clauses, block, ret;

  strcat (p_name, " taskloop");
  mask |= OMP_TASKLOOP_CLAUSE_MASK;
  /* #pragma omp parallel master taskloop{, simd} disallow in_reduction
     clause.  */
  if ((mask & (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NUM_THREADS)) != 0)
    mask &= ~(OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_IN_REDUCTION);

  if (c_parser_next_token_is (parser, CPP_NAME))
    {
      const char *p = IDENTIFIER_POINTER (c_parser_peek_token (parser)->value);

      if (strcmp (p, "simd") == 0)
	{
	  tree cclauses_buf[C_OMP_CLAUSE_SPLIT_COUNT];
	  if (cclauses == NULL)
	    cclauses = cclauses_buf;
	  c_parser_consume_token (parser);
	  if (!flag_openmp)  /* flag_openmp_simd  */
	    return c_parser_omp_simd (loc, parser, p_name, mask, cclauses,
				      if_p);
	  block = c_begin_compound_stmt (true);
	  ret = c_parser_omp_simd (loc, parser, p_name, mask, cclauses, if_p);
	  block = c_end_compound_stmt (loc, block, true);
	  if (ret == NULL)
	    return ret;
	  ret = make_node (OMP_TASKLOOP);
	  TREE_TYPE (ret) = void_type_node;
	  OMP_FOR_BODY (ret) = block;
	  OMP_FOR_CLAUSES (ret) = cclauses[C_OMP_CLAUSE_SPLIT_TASKLOOP];
	  OMP_FOR_CLAUSES (ret)
	    = c_finish_taskloop_clauses (OMP_FOR_CLAUSES (ret));
	  SET_EXPR_LOCATION (ret, loc);
	  add_stmt (ret);
	  return ret;
	}
    }
  if (!flag_openmp)  /* flag_openmp_simd  */
    {
      c_parser_skip_to_pragma_eol (parser, false);
      return NULL_TREE;
    }

  clauses = c_parser_omp_all_clauses (parser, mask, p_name, cclauses == NULL);
  if (cclauses)
    {
      omp_split_clauses (loc, OMP_TASKLOOP, mask, clauses, cclauses);
      clauses = cclauses[C_OMP_CLAUSE_SPLIT_TASKLOOP];
    }

  clauses = c_finish_taskloop_clauses (clauses);
  block = c_begin_compound_stmt (true);
  ret = c_parser_omp_for_loop (loc, parser, OMP_TASKLOOP, clauses, NULL, if_p);
  block = c_end_compound_stmt (loc, block, true);
  add_stmt (block);

  return ret;
}

/* Main entry point to parsing most OpenMP pragmas.  */

static void
c_parser_omp_construct (c_parser *parser, bool *if_p)
{
  enum pragma_kind p_kind;
  location_t loc;
  tree stmt;
  char p_name[sizeof "#pragma omp teams distribute parallel for simd"];
  omp_clause_mask mask (0);

  loc = c_parser_peek_token (parser)->location;
  p_kind = c_parser_peek_token (parser)->pragma_kind;
  c_parser_consume_pragma (parser);

  switch (p_kind)
    {
    case PRAGMA_OACC_ATOMIC:
      c_parser_omp_atomic (loc, parser, true);
      return;
    case PRAGMA_OACC_CACHE:
      strcpy (p_name, "#pragma acc");
      stmt = c_parser_oacc_cache (loc, parser);
      break;
    case PRAGMA_OACC_DATA:
      stmt = c_parser_oacc_data (loc, parser, if_p);
      break;
    case PRAGMA_OACC_HOST_DATA:
      stmt = c_parser_oacc_host_data (loc, parser, if_p);
      break;
    case PRAGMA_OACC_KERNELS:
    case PRAGMA_OACC_PARALLEL:
    case PRAGMA_OACC_SERIAL:
      strcpy (p_name, "#pragma acc");
      stmt = c_parser_oacc_compute (loc, parser, p_kind, p_name, if_p);
      break;
    case PRAGMA_OACC_LOOP:
      strcpy (p_name, "#pragma acc");
      stmt = c_parser_oacc_loop (loc, parser, p_name, mask, NULL, if_p);
      break;
    case PRAGMA_OACC_WAIT:
      strcpy (p_name, "#pragma wait");
      stmt = c_parser_oacc_wait (loc, parser, p_name);
      break;
    case PRAGMA_OMP_ALLOCATE:
      c_parser_omp_allocate (loc, parser);
      return;
    case PRAGMA_OMP_ATOMIC:
      c_parser_omp_atomic (loc, parser, false);
      return;
    case PRAGMA_OMP_CRITICAL:
      stmt = c_parser_omp_critical (loc, parser, if_p);
      break;
    case PRAGMA_OMP_DISTRIBUTE:
      strcpy (p_name, "#pragma omp");
      stmt = c_parser_omp_distribute (loc, parser, p_name, mask, NULL, if_p);
      break;
    case PRAGMA_OMP_FOR:
      strcpy (p_name, "#pragma omp");
      stmt = c_parser_omp_for (loc, parser, p_name, mask, NULL, if_p);
      break;
    case PRAGMA_OMP_LOOP:
      strcpy (p_name, "#pragma omp");
      stmt = c_parser_omp_loop (loc, parser, p_name, mask, NULL, if_p);
      break;
    case PRAGMA_OMP_MASTER:
      strcpy (p_name, "#pragma omp");
      stmt = c_parser_omp_master (loc, parser, p_name, mask, NULL, if_p);
      break;
    case PRAGMA_OMP_PARALLEL:
      strcpy (p_name, "#pragma omp");
      stmt = c_parser_omp_parallel (loc, parser, p_name, mask, NULL, if_p);
      break;
    case PRAGMA_OMP_SECTIONS:
      strcpy (p_name, "#pragma omp");
      stmt = c_parser_omp_sections (loc, parser, p_name, mask, NULL);
      break;
    case PRAGMA_OMP_SIMD:
      strcpy (p_name, "#pragma omp");
      stmt = c_parser_omp_simd (loc, parser, p_name, mask, NULL, if_p);
      break;
    case PRAGMA_OMP_SINGLE:
      stmt = c_parser_omp_single (loc, parser, if_p);
      break;
    case PRAGMA_OMP_TASK:
      stmt = c_parser_omp_task (loc, parser, if_p);
      break;
    case PRAGMA_OMP_TASKGROUP:
      stmt = c_parser_omp_taskgroup (loc, parser, if_p);
      break;
    case PRAGMA_OMP_TASKLOOP:
      strcpy (p_name, "#pragma omp");
      stmt = c_parser_omp_taskloop (loc, parser, p_name, mask, NULL, if_p);
      break;
    case PRAGMA_OMP_TEAMS:
      strcpy (p_name, "#pragma omp");
      stmt = c_parser_omp_teams (loc, parser, p_name, mask, NULL, if_p);
      break;
    default:
      gcc_unreachable ();
    }

  if (stmt && stmt != error_mark_node)
    gcc_assert (EXPR_LOCATION (stmt) != UNKNOWN_LOCATION);
}


/* OpenMP 2.5:
   # pragma omp threadprivate (variable-list) */

static void
c_parser_omp_threadprivate (c_parser *parser)
{
  tree vars, t;
  location_t loc;

  c_parser_consume_pragma (parser);
  loc = c_parser_peek_token (parser)->location;
  vars = c_parser_omp_var_list_parens (parser, OMP_CLAUSE_ERROR, NULL);

  /* Mark every variable in VARS to be assigned thread local storage.  */
  for (t = vars; t; t = TREE_CHAIN (t))
    {
      tree v = TREE_PURPOSE (t);

      /* FIXME diagnostics: Ideally we should keep individual
	 locations for all the variables in the var list to make the
	 following errors more precise.  Perhaps
	 c_parser_omp_var_list_parens() should construct a list of
	 locations to go along with the var list.  */

      /* If V had already been marked threadprivate, it doesn't matter
	 whether it had been used prior to this point.  */
      if (!VAR_P (v))
	error_at (loc, "%qD is not a variable", v);
      else if (TREE_USED (v) && !C_DECL_THREADPRIVATE_P (v))
	error_at (loc, "%qE declared %<threadprivate%> after first use", v);
      else if (! is_global_var (v))
	error_at (loc, "automatic variable %qE cannot be %<threadprivate%>", v);
      else if (TREE_TYPE (v) == error_mark_node)
	;
      else if (! COMPLETE_TYPE_P (TREE_TYPE (v)))
	error_at (loc, "%<threadprivate%> %qE has incomplete type", v);
      else
	{
	  if (! DECL_THREAD_LOCAL_P (v))
	    {
	      set_decl_tls_model (v, decl_default_tls_model (v));
	      /* If rtl has been already set for this var, call
		 make_decl_rtl once again, so that encode_section_info
		 has a chance to look at the new decl flags.  */
	      if (DECL_RTL_SET_P (v))
		make_decl_rtl (v);
	    }
	  C_DECL_THREADPRIVATE_P (v) = 1;
	}
    }

  c_parser_skip_to_pragma_eol (parser);
}

/* Parse a transaction attribute (GCC Extension).

   transaction-attribute:
     gnu-attributes
     attribute-specifier
*/

static tree
c_parser_transaction_attributes (c_parser *parser)
{
  if (c_parser_next_token_is_keyword (parser, RID_ATTRIBUTE))
    return c_parser_gnu_attributes (parser);

  if (!c_parser_next_token_is (parser, CPP_OPEN_SQUARE))
    return NULL_TREE;
  return c_parser_std_attribute_specifier (parser, true);
}

/* Parse a __transaction_atomic or __transaction_relaxed statement
   (GCC Extension).

   transaction-statement:
     __transaction_atomic transaction-attribute[opt] compound-statement
     __transaction_relaxed compound-statement

   Note that the only valid attribute is: "outer".
*/

static tree
c_parser_transaction (c_parser *parser, enum rid keyword)
{
  unsigned int old_in = parser->in_transaction;
  unsigned int this_in = 1, new_in;
  location_t loc = c_parser_peek_token (parser)->location;
  tree stmt, attrs;

  gcc_assert ((keyword == RID_TRANSACTION_ATOMIC
      || keyword == RID_TRANSACTION_RELAXED)
      && c_parser_next_token_is_keyword (parser, keyword));
  c_parser_consume_token (parser);

  if (keyword == RID_TRANSACTION_RELAXED)
    this_in |= TM_STMT_ATTR_RELAXED;
  else
    {
      attrs = c_parser_transaction_attributes (parser);
      if (attrs)
	this_in |= parse_tm_stmt_attr (attrs, TM_STMT_ATTR_OUTER);
    }

  /* Keep track if we're in the lexical scope of an outer transaction.  */
  new_in = this_in | (old_in & TM_STMT_ATTR_OUTER);

  parser->in_transaction = new_in;
  stmt = c_parser_compound_statement (parser);
  parser->in_transaction = old_in;

  if (flag_tm)
    stmt = c_finish_transaction (loc, stmt, this_in);
  else
    error_at (loc, (keyword == RID_TRANSACTION_ATOMIC ?
	"%<__transaction_atomic%> without transactional memory support enabled"
	: "%<__transaction_relaxed %> "
	"without transactional memory support enabled"));

  return stmt;
}

/* Parse a __transaction_atomic or __transaction_relaxed expression
   (GCC Extension).

   transaction-expression:
     __transaction_atomic ( expression )
     __transaction_relaxed ( expression )
*/

static struct c_expr
c_parser_transaction_expression (c_parser *parser, enum rid keyword)
{
  struct c_expr ret;
  unsigned int old_in = parser->in_transaction;
  unsigned int this_in = 1;
  location_t loc = c_parser_peek_token (parser)->location;
  tree attrs;

  gcc_assert ((keyword == RID_TRANSACTION_ATOMIC
      || keyword == RID_TRANSACTION_RELAXED)
      && c_parser_next_token_is_keyword (parser, keyword));
  c_parser_consume_token (parser);

  if (keyword == RID_TRANSACTION_RELAXED)
    this_in |= TM_STMT_ATTR_RELAXED;
  else
    {
      attrs = c_parser_transaction_attributes (parser);
      if (attrs)
	this_in |= parse_tm_stmt_attr (attrs, 0);
    }

  parser->in_transaction = this_in;
  matching_parens parens;
  if (parens.require_open (parser))
    {
      tree expr = c_parser_expression (parser).value;
      ret.original_type = TREE_TYPE (expr);
      ret.value = build1 (TRANSACTION_EXPR, ret.original_type, expr);
      if (this_in & TM_STMT_ATTR_RELAXED)
	TRANSACTION_EXPR_RELAXED (ret.value) = 1;
      SET_EXPR_LOCATION (ret.value, loc);
      ret.original_code = TRANSACTION_EXPR;
      if (!parens.require_close (parser))
	{
	  c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, NULL);
	  goto error;
	}
    }
  else
    {
     error:
      ret.set_error ();
      ret.original_code = ERROR_MARK;
      ret.original_type = NULL;
    }
  parser->in_transaction = old_in;

  if (!flag_tm)
    error_at (loc, (keyword == RID_TRANSACTION_ATOMIC ?
	"%<__transaction_atomic%> without transactional memory support enabled"
	: "%<__transaction_relaxed %> "
	"without transactional memory support enabled"));

  set_c_expr_source_range (&ret, loc, loc);

  return ret;
}

/* Parse a __transaction_cancel statement (GCC Extension).

   transaction-cancel-statement:
     __transaction_cancel transaction-attribute[opt] ;

   Note that the only valid attribute is "outer".
*/

static tree
c_parser_transaction_cancel (c_parser *parser)
{
  location_t loc = c_parser_peek_token (parser)->location;
  tree attrs;
  bool is_outer = false;

  gcc_assert (c_parser_next_token_is_keyword (parser, RID_TRANSACTION_CANCEL));
  c_parser_consume_token (parser);

  attrs = c_parser_transaction_attributes (parser);
  if (attrs)
    is_outer = (parse_tm_stmt_attr (attrs, TM_STMT_ATTR_OUTER) != 0);

  if (!flag_tm)
    {
      error_at (loc, "%<__transaction_cancel%> without "
		"transactional memory support enabled");
      goto ret_error;
    }
  else if (parser->in_transaction & TM_STMT_ATTR_RELAXED)
    {
      error_at (loc, "%<__transaction_cancel%> within a "
		"%<__transaction_relaxed%>");
      goto ret_error;
    }
  else if (is_outer)
    {
      if ((parser->in_transaction & TM_STMT_ATTR_OUTER) == 0
	  && !is_tm_may_cancel_outer (current_function_decl))
	{
	  error_at (loc, "outer %<__transaction_cancel%> not "
		    "within outer %<__transaction_atomic%> or "
		    "a %<transaction_may_cancel_outer%> function");
	  goto ret_error;
	}
    }
  else if (parser->in_transaction == 0)
    {
      error_at (loc, "%<__transaction_cancel%> not within "
		"%<__transaction_atomic%>");
      goto ret_error;
    }

  return add_stmt (build_tm_abort_call (loc, is_outer));

 ret_error:
  return build1 (NOP_EXPR, void_type_node, error_mark_node);
}

/* Parse a single source file.  */

void
c_parse_file (void)
{
  /* Use local storage to begin.  If the first token is a pragma, parse it.
     If it is #pragma GCC pch_preprocess, then this will load a PCH file
     which will cause garbage collection.  */
  c_parser tparser;

  memset (&tparser, 0, sizeof tparser);
  tparser.translate_strings_p = true;
  tparser.tokens = &tparser.tokens_buf[0];
  the_parser = &tparser;

  if (c_parser_peek_token (&tparser)->pragma_kind == PRAGMA_GCC_PCH_PREPROCESS)
    c_parser_pragma_pch_preprocess (&tparser);
  else
    c_common_no_more_pch ();

  the_parser = ggc_alloc<c_parser> ();
  *the_parser = tparser;
  if (tparser.tokens == &tparser.tokens_buf[0])
    the_parser->tokens = &the_parser->tokens_buf[0];

  /* Initialize EH, if we've been told to do so.  */
  if (flag_exceptions)
    using_eh_for_cleanups ();

  c_parser_translation_unit (the_parser);
  the_parser = NULL;
}

/* Parse the body of a function declaration marked with "__RTL".

   The RTL parser works on the level of characters read from a
   FILE *, whereas c_parser works at the level of tokens.
   Square this circle by consuming all of the tokens up to and
   including the closing brace, recording the start/end of the RTL
   fragment, and reopening the file and re-reading the relevant
   lines within the RTL parser.

   This requires the opening and closing braces of the C function
   to be on separate lines from the RTL they wrap.

   Take ownership of START_WITH_PASS, if non-NULL.  */

location_t
c_parser_parse_rtl_body (c_parser *parser, char *start_with_pass)
{
  if (!c_parser_require (parser, CPP_OPEN_BRACE, "expected %<{%>"))
    {
      free (start_with_pass);
      return c_parser_peek_token (parser)->location;
    }

  location_t start_loc = c_parser_peek_token (parser)->location;

  /* Consume all tokens, up to the closing brace, handling
     matching pairs of braces in the rtl dump.  */
  int num_open_braces = 1;
  while (1)
    {
      switch (c_parser_peek_token (parser)->type)
	{
	case CPP_OPEN_BRACE:
	  num_open_braces++;
	  break;
	case CPP_CLOSE_BRACE:
	  if (--num_open_braces == 0)
	    goto found_closing_brace;
	  break;
	case CPP_EOF:
	  error_at (start_loc, "no closing brace");
	  free (start_with_pass);
	  return c_parser_peek_token (parser)->location;
	default:
	  break;
	}
      c_parser_consume_token (parser);
    }

 found_closing_brace:
  /* At the closing brace; record its location.  */
  location_t end_loc = c_parser_peek_token (parser)->location;

  /* Consume the closing brace.  */
  c_parser_consume_token (parser);

  /* Invoke the RTL parser.  */
  if (!read_rtl_function_body_from_file_range (start_loc, end_loc))
    {
      free (start_with_pass);
      return end_loc;
    }

 /*  Run the backend on the cfun created above, transferring ownership of
     START_WITH_PASS.  */
  run_rtl_passes (start_with_pass);
  return end_loc;
}

#include "gt-c-c-parser.h"
