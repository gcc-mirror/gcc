/* Parser for C and Objective-C.
   Copyright (C) 1987, 1988, 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

   Parser actions based on the old Bison parser; structure somewhat
   influenced by and fragments based on the C++ parser.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

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
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "langhooks.h"
#include "input.h"
#include "cpplib.h"
#include "timevar.h"
#include "c-pragma.h"
#include "c-tree.h"
#include "flags.h"
#include "output.h"
#include "toplev.h"
#include "ggc.h"
#include "c-common.h"


/* Miscellaneous data and functions needed for the parser.  */

int yydebug;

/* Objective-C specific parser/lexer information.  */

static int objc_pq_context = 0;

/* The following flag is needed to contextualize Objective-C lexical
   analysis.  In some cases (e.g., 'int NSObject;'), it is undesirable
   to bind an identifier to an Objective-C class, even if a class with
   that name exists.  */
static int objc_need_raw_identifier = 0;
#define OBJC_NEED_RAW_IDENTIFIER(VAL)		\
  do {						\
    if (c_dialect_objc ())			\
      objc_need_raw_identifier = VAL;		\
  } while (0)

/* The reserved keyword table.  */
struct resword
{
  const char *word;
  ENUM_BITFIELD(rid) rid : 16;
  unsigned int disable   : 16;
};

/* Disable mask.  Keywords are disabled if (reswords[i].disable &
   mask) is _true_.  */
#define D_C89	0x01	/* not in C89 */
#define D_EXT	0x02	/* GCC extension */
#define D_EXT89	0x04	/* GCC extension incorporated in C99 */
#define D_OBJC	0x08	/* Objective C only */

static const struct resword reswords[] =
{
  { "_Bool",		RID_BOOL,	0 },
  { "_Complex",		RID_COMPLEX,	0 },
  { "__FUNCTION__",	RID_FUNCTION_NAME, 0 },
  { "__PRETTY_FUNCTION__", RID_PRETTY_FUNCTION_NAME, 0 },
  { "__alignof",	RID_ALIGNOF,	0 },
  { "__alignof__",	RID_ALIGNOF,	0 },
  { "__asm",		RID_ASM,	0 },
  { "__asm__",		RID_ASM,	0 },
  { "__attribute",	RID_ATTRIBUTE,	0 },
  { "__attribute__",	RID_ATTRIBUTE,	0 },
  { "__builtin_choose_expr", RID_CHOOSE_EXPR, 0 },
  { "__builtin_offsetof", RID_OFFSETOF, 0 },
  { "__builtin_types_compatible_p", RID_TYPES_COMPATIBLE_P, 0 },
  { "__builtin_va_arg",	RID_VA_ARG,	0 },
  { "__complex",	RID_COMPLEX,	0 },
  { "__complex__",	RID_COMPLEX,	0 },
  { "__const",		RID_CONST,	0 },
  { "__const__",	RID_CONST,	0 },
  { "__extension__",	RID_EXTENSION,	0 },
  { "__func__",		RID_C99_FUNCTION_NAME, 0 },
  { "__imag",		RID_IMAGPART,	0 },
  { "__imag__",		RID_IMAGPART,	0 },
  { "__inline",		RID_INLINE,	0 },
  { "__inline__",	RID_INLINE,	0 },
  { "__label__",	RID_LABEL,	0 },
  { "__real",		RID_REALPART,	0 },
  { "__real__",		RID_REALPART,	0 },
  { "__restrict",	RID_RESTRICT,	0 },
  { "__restrict__",	RID_RESTRICT,	0 },
  { "__signed",		RID_SIGNED,	0 },
  { "__signed__",	RID_SIGNED,	0 },
  { "__thread",		RID_THREAD,	0 },
  { "__typeof",		RID_TYPEOF,	0 },
  { "__typeof__",	RID_TYPEOF,	0 },
  { "__volatile",	RID_VOLATILE,	0 },
  { "__volatile__",	RID_VOLATILE,	0 },
  { "asm",		RID_ASM,	D_EXT },
  { "auto",		RID_AUTO,	0 },
  { "break",		RID_BREAK,	0 },
  { "case",		RID_CASE,	0 },
  { "char",		RID_CHAR,	0 },
  { "const",		RID_CONST,	0 },
  { "continue",		RID_CONTINUE,	0 },
  { "default",		RID_DEFAULT,	0 },
  { "do",		RID_DO,		0 },
  { "double",		RID_DOUBLE,	0 },
  { "else",		RID_ELSE,	0 },
  { "enum",		RID_ENUM,	0 },
  { "extern",		RID_EXTERN,	0 },
  { "float",		RID_FLOAT,	0 },
  { "for",		RID_FOR,	0 },
  { "goto",		RID_GOTO,	0 },
  { "if",		RID_IF,		0 },
  { "inline",		RID_INLINE,	D_EXT89 },
  { "int",		RID_INT,	0 },
  { "long",		RID_LONG,	0 },
  { "register",		RID_REGISTER,	0 },
  { "restrict",		RID_RESTRICT,	D_C89 },
  { "return",		RID_RETURN,	0 },
  { "short",		RID_SHORT,	0 },
  { "signed",		RID_SIGNED,	0 },
  { "sizeof",		RID_SIZEOF,	0 },
  { "static",		RID_STATIC,	0 },
  { "struct",		RID_STRUCT,	0 },
  { "switch",		RID_SWITCH,	0 },
  { "typedef",		RID_TYPEDEF,	0 },
  { "typeof",		RID_TYPEOF,	D_EXT },
  { "union",		RID_UNION,	0 },
  { "unsigned",		RID_UNSIGNED,	0 },
  { "void",		RID_VOID,	0 },
  { "volatile",		RID_VOLATILE,	0 },
  { "while",		RID_WHILE,	0 },
  /* These Objective-C keywords are recognized only immediately after
     an '@'.  */
  { "class",		RID_AT_CLASS,		D_OBJC },
  { "compatibility_alias", RID_AT_ALIAS,	D_OBJC },
  { "defs",		RID_AT_DEFS,		D_OBJC },
  { "encode",		RID_AT_ENCODE,		D_OBJC },
  { "end",		RID_AT_END,		D_OBJC },
  { "implementation",	RID_AT_IMPLEMENTATION,	D_OBJC },
  { "interface",	RID_AT_INTERFACE,	D_OBJC },
  { "private",		RID_AT_PRIVATE,		D_OBJC },
  { "protected",	RID_AT_PROTECTED,	D_OBJC },
  { "protocol",		RID_AT_PROTOCOL,	D_OBJC },
  { "public",		RID_AT_PUBLIC,		D_OBJC },
  { "selector",		RID_AT_SELECTOR,	D_OBJC },
  { "throw",		RID_AT_THROW,		D_OBJC },
  { "try",		RID_AT_TRY,		D_OBJC },
  { "catch",		RID_AT_CATCH,		D_OBJC },
  { "finally",		RID_AT_FINALLY,		D_OBJC },
  { "synchronized",	RID_AT_SYNCHRONIZED,	D_OBJC },
  /* These are recognized only in protocol-qualifier context
     (see above) */
  { "bycopy",		RID_BYCOPY,		D_OBJC },
  { "byref",		RID_BYREF,		D_OBJC },
  { "in",		RID_IN,			D_OBJC },
  { "inout",		RID_INOUT,		D_OBJC },
  { "oneway",		RID_ONEWAY,		D_OBJC },
  { "out",		RID_OUT,		D_OBJC },
};
#define N_reswords (sizeof reswords / sizeof (struct resword))

/* Initialization routine for this file.  */

void
c_parse_init (void)
{
  /* The only initialization required is of the reserved word
     identifiers.  */
  unsigned int i;
  tree id;
  int mask = (flag_isoc99 ? 0 : D_C89)
	      | (flag_no_asm ? (flag_isoc99 ? D_EXT : D_EXT|D_EXT89) : 0);

  if (!c_dialect_objc ())
     mask |= D_OBJC;

  ridpointers = GGC_CNEWVEC (tree, (int) RID_MAX);
  for (i = 0; i < N_reswords; i++)
    {
      /* If a keyword is disabled, do not enter it into the table
	 and so create a canonical spelling that isn't a keyword.  */
      if (reswords[i].disable & mask)
	continue;

      id = get_identifier (reswords[i].word);
      C_RID_CODE (id) = reswords[i].rid;
      C_IS_RESERVED_WORD (id) = 1;
      ridpointers [(int) reswords[i].rid] = id;
    }
}

/* The C lexer intermediates between the lexer in cpplib and c-lex.c
   and the C parser.  Unlike the C++ lexer, the parser structure
   stores the lexer information instead of using a separate structure.
   Identifiers are separated into ordinary identifiers, type names,
   keywords and some other Objective-C types of identifiers, and some
   look-ahead is maintained.

   ??? It might be a good idea to lex the whole file up front (as for
   C++).  It would then be possible to share more of the C and C++
   lexer code, if desired.  */

/* The following local token type is used.  */

/* A keyword.  */
#define CPP_KEYWORD ((enum cpp_ttype) (N_TTYPES + 1))

/* More information about the type of a CPP_NAME token.  */
typedef enum c_id_kind {
  /* An ordinary identifier.  */
  C_ID_ID,
  /* An identifier declared as a typedef name.  */
  C_ID_TYPENAME,
  /* An identifier declared as an Objective-C class name.  */
  C_ID_CLASSNAME,
  /* Not an identifier.  */
  C_ID_NONE
} c_id_kind;

/* A single C token after string literal concatenation and conversion
   of preprocessing tokens to tokens.  */
typedef struct c_token GTY (())
{
  /* The kind of token.  */
  ENUM_BITFIELD (cpp_ttype) type : 8;
  /* If this token is a CPP_NAME, this value indicates whether also
     declared as some kind of type.  Otherwise, it is C_ID_NONE.  */
  ENUM_BITFIELD (c_id_kind) id_kind : 8;
  /* If this token is a keyword, this value indicates which keyword.
     Otherwise, this value is RID_MAX.  */
  ENUM_BITFIELD (rid) keyword : 8;
  /* True if this token is from a system header.  */
  BOOL_BITFIELD in_system_header : 1;
  /* The value associated with this token, if any.  */
  tree value;
  /* The location at which this token was found.  */
  location_t location;
} c_token;

/* A parser structure recording information about the state and
   context of parsing.  Includes lexer information with up to two
   tokens of look-ahead; more are not needed for C.  */
typedef struct c_parser GTY(())
{
  /* The look-ahead tokens.  */
  c_token tokens[2];
  /* How many look-ahead tokens are available (0, 1 or 2).  */
  short tokens_avail;
  /* True if a syntax error is being recovered from; false otherwise.
     c_parser_error sets this flag.  It should clear this flag when
     enough tokens have been consumed to recover from the error.  */
  BOOL_BITFIELD error : 1;
} c_parser;

/* Read in and lex a single token, storing it in *TOKEN.  */

static void
c_lex_one_token (c_token *token)
{
  timevar_push (TV_LEX);
  token->type = c_lex_with_flags (&token->value, &token->location, NULL);
  token->in_system_header = in_system_header;
  switch (token->type)
    {
    case CPP_NAME:
      token->id_kind = C_ID_NONE;
      token->keyword = RID_MAX;
      {
	tree decl;

	int objc_force_identifier = objc_need_raw_identifier;
	OBJC_NEED_RAW_IDENTIFIER (0);

	if (C_IS_RESERVED_WORD (token->value))
	  {
	    enum rid rid_code = C_RID_CODE (token->value);

	    if (c_dialect_objc ())
	      {
		if (!OBJC_IS_AT_KEYWORD (rid_code)
		    && (!OBJC_IS_PQ_KEYWORD (rid_code) || objc_pq_context))
		  {
		    /* Return the canonical spelling for this keyword.  */
		    token->value = ridpointers[(int) rid_code];
		    token->type = CPP_KEYWORD;
		    token->keyword = rid_code;
		    break;
		  }
	      }
	    else
	      {
		/* Return the canonical spelling for this keyword.  */
		token->value = ridpointers[(int) rid_code];
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
		&& (global_bindings_p ()
		    || (!objc_force_identifier && !decl)))
	      {
		token->value = objc_interface_decl;
		token->id_kind = C_ID_CLASSNAME;
		break;
	      }
	  }
      }
      token->id_kind = C_ID_ID;
      break;
    case CPP_AT_NAME:
      /* This only happens in Objective-C; it must be a keyword.  */
      token->type = CPP_KEYWORD;
      token->id_kind = C_ID_NONE;
      token->keyword = C_RID_CODE (token->value);
      break;
    case CPP_COLON:
    case CPP_COMMA:
    case CPP_CLOSE_PAREN:
    case CPP_SEMICOLON:
      /* These tokens may affect the interpretation of any identifiers
	 following, if doing Objective-C.  */
      OBJC_NEED_RAW_IDENTIFIER (0);
      token->id_kind = C_ID_NONE;
      token->keyword = RID_MAX;
      break;
    default:
      token->id_kind = C_ID_NONE;
      token->keyword = RID_MAX;
      break;
    }
  timevar_pop (TV_LEX);
}

/* Return a pointer to the next token from PARSER, reading it in if
   necessary.  */

static inline c_token *
c_parser_peek_token (c_parser *parser)
{
  if (parser->tokens_avail == 0)
    {
      c_lex_one_token (&parser->tokens[0]);
      parser->tokens_avail = 1;
    }
  return &parser->tokens[0];
}

/* Return true if the next token from PARSER has the indicated
   TYPE.  */

static inline bool
c_parser_next_token_is (c_parser *parser, enum cpp_ttype type)
{
  return c_parser_peek_token (parser)->type == type;
}

/* Return true if the next token from PARSER does not have the
   indicated TYPE.  */

static inline bool
c_parser_next_token_is_not (c_parser *parser, enum cpp_ttype type)
{
  return !c_parser_next_token_is (parser, type);
}

/* Return true if the next token from PARSER is the indicated
   KEYWORD.  */

static inline bool
c_parser_next_token_is_keyword (c_parser *parser, enum rid keyword)
{
  c_token *token;

  /* Peek at the next token.  */
  token = c_parser_peek_token (parser);
  /* Check to see if it is the indicated keyword.  */
  return token->keyword == keyword;
}

/* Return true if TOKEN can start a type name,
   false otherwise.  */
static bool
c_token_starts_typename (c_token *token)
{
  switch (token->type)
    {
    case CPP_NAME:
      switch (token->id_kind)
	{
	case C_ID_ID:
	  return false;
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
	case RID_BOOL:
	case RID_ENUM:
	case RID_STRUCT:
	case RID_UNION:
	case RID_TYPEOF:
	case RID_CONST:
	case RID_VOLATILE:
	case RID_RESTRICT:
	case RID_ATTRIBUTE:
	  return true;
	default:
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

/* Return true if the next token from PARSER can start a type name,
   false otherwise.  */
static inline bool
c_parser_next_token_starts_typename (c_parser *parser)
{
  c_token *token = c_parser_peek_token (parser);
  return c_token_starts_typename (token);
}

/* Return true if TOKEN can start declaration specifiers, false
   otherwise.  */
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
	case RID_BOOL:
	case RID_ENUM:
	case RID_STRUCT:
	case RID_UNION:
	case RID_TYPEOF:
	case RID_CONST:
	case RID_VOLATILE:
	case RID_RESTRICT:
	case RID_ATTRIBUTE:
	  return true;
	default:
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

/* Return true if the next token from PARSER can start declaration
   specifiers, false otherwise.  */
static inline bool
c_parser_next_token_starts_declspecs (c_parser *parser)
{
  c_token *token = c_parser_peek_token (parser);
  return c_token_starts_declspecs (token);
}

/* Return a pointer to the next-but-one token from PARSER, reading it
   in if necessary.  The next token is already read in.  */

static c_token *
c_parser_peek_2nd_token (c_parser *parser)
{
  if (parser->tokens_avail >= 2)
    return &parser->tokens[1];
  gcc_assert (parser->tokens_avail == 1);
  gcc_assert (parser->tokens[0].type != CPP_EOF);
  c_lex_one_token (&parser->tokens[1]);
  parser->tokens_avail = 2;
  return &parser->tokens[1];
}

/* Consume the next token from PARSER.  */

static void
c_parser_consume_token (c_parser *parser)
{
  if (parser->tokens_avail == 2)
    parser->tokens[0] = parser->tokens[1];
  else
    {
      gcc_assert (parser->tokens_avail == 1);
      gcc_assert (parser->tokens[0].type != CPP_EOF);
    }
  parser->tokens_avail--;
}

/* Update the globals input_location and in_system_header from
   TOKEN.  */
static inline void
c_parser_set_source_position_from_token (c_token *token)
{
  if (token->type != CPP_EOF)
    {
      input_location = token->location;
      in_system_header = token->in_system_header;
    }
}

/* Allocate a new parser.  */

static c_parser *
c_parser_new (void)
{
  /* Use local storage to lex the first token because loading a PCH
     file may cause garbage collection.  */
  c_parser tparser;
  c_parser *ret;
  memset (&tparser, 0, sizeof tparser);
  c_lex_one_token (&tparser.tokens[0]);
  tparser.tokens_avail = 1;
  ret = GGC_NEW (c_parser);
  memcpy (ret, &tparser, sizeof tparser);
  return ret;
}

/* Issue a diagnostic of the form
      FILE:LINE: MESSAGE before TOKEN
   where TOKEN is the next token in the input stream of PARSER.
   MESSAGE (specified by the caller) is usually of the form "expected
   OTHER-TOKEN".

   Do not issue a diagnostic if still recovering from an error.

   ??? This is taken from the C++ parser, but building up messages in
   this way is not i18n-friendly and some other approach should be
   used.  */

static void
c_parser_error (c_parser *parser, const char *msgid)
{
  c_token *token = c_parser_peek_token (parser);
  if (parser->error)
    return;
  parser->error = true;
  if (!msgid)
    return;
  /* This diagnostic makes more sense if it is tagged to the line of
     the token we just peeked at.  */
  c_parser_set_source_position_from_token (token);
  c_parse_error (msgid,
		 /* Because c_parse_error does not understand
		    CPP_KEYWORD, keywords are treated like
		    identifiers.  */
		 (token->type == CPP_KEYWORD ? CPP_NAME : token->type),
		 token->value);
}

/* If the next token is of the indicated TYPE, consume it.  Otherwise,
   issue the error MSGID.  If MSGID is NULL then a message has already
   been produced and no message will be produced this time.  Returns
   true if found, false otherwise.  */

static bool
c_parser_require (c_parser *parser,
		  enum cpp_ttype type,
		  const char *msgid)
{
  if (c_parser_next_token_is (parser, type))
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
   time.  */

static void
c_parser_skip_until_found (c_parser *parser,
			   enum cpp_ttype type,
			   const char *msgid)
{
  unsigned nesting_depth = 0;

  if (c_parser_require (parser, type, msgid))
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

/* Skip tokens until we have consumed an entire block, or until we
   have consumed a non-nested ';'.  */

static void
c_parser_skip_to_end_of_block_or_statement (c_parser *parser)
{
  unsigned nesting_depth = 0;

  while (true)
    {
      c_token *token;

      /* Peek at the next token.  */
      token = c_parser_peek_token (parser);
      /* If we've run out of tokens, stop.  */
      if (token->type == CPP_EOF)
	return;
      /* If the next token is a ';', we have reached the end of the
	 statement.  */
      if (token->type == CPP_SEMICOLON && !nesting_depth)
	{
	  /* Consume the ';'.  */
	  c_parser_consume_token (parser);
	  break;
	}
      /* If the next token is a non-nested '}', then we have reached
	 the end of the current block.  */
      if (token->type == CPP_CLOSE_BRACE
	  && (nesting_depth == 0 || --nesting_depth == 0))
	{
	  c_parser_consume_token (parser);
	  break;
	}
      /* If it the next token is a '{', then we are entering a new
	 block.  Consume the entire block.  */
      if (token->type == CPP_OPEN_BRACE)
	++nesting_depth;
      c_parser_consume_token (parser);
    }
  parser->error = false;
}


/* Save the warning flags which are controlled by __extension__.  */

static inline int
disable_extension_diagnostics (void)
{
  int ret = (pedantic
	     | (warn_pointer_arith << 1)
	     | (warn_traditional << 2)
	     | (flag_iso << 3));
  pedantic = 0;
  warn_pointer_arith = 0;
  warn_traditional = 0;
  flag_iso = 0;
  return ret;
}

/* Restore the warning flags which are controlled by __extension__.
   FLAGS is the return value from disable_extension_diagnostics.  */

static inline void
restore_extension_diagnostics (int flags)
{
  pedantic = flags & 1;
  warn_pointer_arith = (flags >> 1) & 1;
  warn_traditional = (flags >> 2) & 1;
  flag_iso = (flags >> 3) & 1;
}

/* Possibly kinds of declarator to parse.  */
typedef enum c_dtr_syn {
  /* A normal declarator with an identifier.  */
  C_DTR_NORMAL,
  /* An abstract declarator (maybe empty).  */
  C_DTR_ABSTRACT,
  /* A parameter declarator: may be either, but after a type name does
     not redeclare a typedef name as an identifier if it can
     alternatively be interpreted as a typedef name; see DR#009,
     applied in C90 TC1, omitted from C99 and reapplied in C99 TC2
     following DR#249.  For example, given a typedef T, "int T" and
     "int *T" are valid parameter declarations redeclaring T, while
     "int (T)" and "int * (T)" and "int (T[])" and "int (T (int))" are
     abstract declarators rather than involving redundant parentheses;
     the same applies with attributes inside the parentheses before
     "T".  */
  C_DTR_PARM
} c_dtr_syn;

static void c_parser_external_declaration (c_parser *);
static void c_parser_asm_definition (c_parser *);
static void c_parser_declaration_or_fndef (c_parser *, bool, bool, bool, bool);
static void c_parser_declspecs (c_parser *, struct c_declspecs *, bool, bool,
				bool);
static struct c_typespec c_parser_enum_specifier (c_parser *);
static struct c_typespec c_parser_struct_or_union_specifier (c_parser *);
static tree c_parser_struct_declaration (c_parser *);
static struct c_typespec c_parser_typeof_specifier (c_parser *);
static struct c_declarator *c_parser_declarator (c_parser *, bool, c_dtr_syn,
						 bool *);
static struct c_declarator *c_parser_direct_declarator (c_parser *, bool,
							c_dtr_syn, bool *);
static struct c_declarator *c_parser_direct_declarator_inner (c_parser *,
							      bool,
							      struct c_declarator *);
static struct c_arg_info *c_parser_parms_declarator (c_parser *, bool, tree);
static struct c_arg_info *c_parser_parms_list_declarator (c_parser *, tree);
static struct c_parm *c_parser_parameter_declaration (c_parser *, tree);
static tree c_parser_simple_asm_expr (c_parser *);
static tree c_parser_attributes (c_parser *);
static struct c_type_name *c_parser_type_name (c_parser *);
static struct c_expr c_parser_initializer (c_parser *);
static struct c_expr c_parser_braced_init (c_parser *, tree, bool);
static void c_parser_initelt (c_parser *);
static void c_parser_initval (c_parser *, struct c_expr *);
static tree c_parser_compound_statement (c_parser *);
static void c_parser_compound_statement_nostart (c_parser *);
static void c_parser_label (c_parser *);
static void c_parser_statement (c_parser *);
static void c_parser_statement_after_labels (c_parser *);
static void c_parser_if_statement (c_parser *);
static void c_parser_switch_statement (c_parser *);
static void c_parser_while_statement (c_parser *);
static void c_parser_do_statement (c_parser *);
static void c_parser_for_statement (c_parser *);
static tree c_parser_asm_statement (c_parser *);
static tree c_parser_asm_operands (c_parser *);
static tree c_parser_asm_clobbers (c_parser *);
static struct c_expr c_parser_expr_no_commas (c_parser *, struct c_expr *);
static struct c_expr c_parser_conditional_expression (c_parser *,
						      struct c_expr *);
static struct c_expr c_parser_binary_expression (c_parser *, struct c_expr *);
static struct c_expr c_parser_cast_expression (c_parser *, struct c_expr *);
static struct c_expr c_parser_unary_expression (c_parser *);
static struct c_expr c_parser_sizeof_expression (c_parser *);
static struct c_expr c_parser_alignof_expression (c_parser *);
static struct c_expr c_parser_postfix_expression (c_parser *);
static struct c_expr c_parser_postfix_expression_after_paren_type (c_parser *,
								   struct c_type_name *);
static struct c_expr c_parser_postfix_expression_after_primary (c_parser *,
								struct c_expr);
static struct c_expr c_parser_expression (c_parser *);
static tree c_parser_expr_list (c_parser *);

/* These Objective-C parser functions are only ever called when
   compiling Objective-C.  */
static void c_parser_objc_class_definition (c_parser *);
static void c_parser_objc_class_instance_variables (c_parser *);
static void c_parser_objc_class_declaration (c_parser *);
static void c_parser_objc_alias_declaration (c_parser *);
static void c_parser_objc_protocol_definition (c_parser *);
static enum tree_code c_parser_objc_method_type (c_parser *);
static void c_parser_objc_method_definition (c_parser *);
static void c_parser_objc_methodprotolist (c_parser *);
static void c_parser_objc_methodproto (c_parser *);
static tree c_parser_objc_method_decl (c_parser *);
static tree c_parser_objc_type_name (c_parser *);
static tree c_parser_objc_protocol_refs (c_parser *);
static void c_parser_objc_try_catch_statement (c_parser *);
static void c_parser_objc_synchronized_statement (c_parser *);
static tree c_parser_objc_selector (c_parser *);
static tree c_parser_objc_selector_arg (c_parser *);
static tree c_parser_objc_receiver (c_parser *);
static tree c_parser_objc_message_args (c_parser *);
static tree c_parser_objc_keywordexpr (c_parser *);

/* Parse a translation unit (C90 6.7, C99 6.9).

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
      if (pedantic)
	pedwarn ("ISO C forbids an empty source file");
    }
  else
    {
      void *obstack_position = obstack_alloc (&parser_obstack, 0);
      do
	{
	  ggc_collect ();
	  c_parser_external_declaration (parser);
	  obstack_free (&parser_obstack, obstack_position);
	}
      while (c_parser_next_token_is_not (parser, CPP_EOF));
    }
}

/* Parse an external declaration (C90 6.7, C99 6.9).

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
	  c_parser_objc_class_definition (parser);
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
	  c_parser_objc_protocol_definition (parser);
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
      if (pedantic)
	pedwarn ("ISO C does not allow extra %<;%> outside of a function");
      c_parser_consume_token (parser);
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
    default:
    decl_or_fndef:
      /* A declaration or a function definition.  We can only tell
	 which after parsing the declaration specifiers, if any, and
	 the first declarator.  */
      c_parser_declaration_or_fndef (parser, true, true, false, true);
      break;
    }
}

/* Parse a declaration or function definition (C90 6.5, 6.7.1, C99
   6.7, 6.9.1).  If FNDEF_OK is true, a function definition is
   accepted; otherwise (old-style parameter declarations) only other
   declarations are accepted.  If NESTED is true, we are inside a
   function or parsing old-style parameter declarations; any functions
   encountered are nested functions and declaration specifiers are
   required; otherwise we are at top level and functions are normal
   functions and declaration specifiers may be optional.  If EMPTY_OK
   is true, empty declarations are OK (subject to all other
   constraints); otherwise (old-style parameter declarations) they are
   diagnosed.  If START_ATTR_OK is true, the declaration specifiers
   may start with attributes; otherwise they may not.

   declaration:
     declaration-specifiers init-declarator-list[opt] ;

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
     declarator simple-asm-expr[opt] attributes[opt]
     declarator simple-asm-expr[opt] attributes[opt] = initializer

   GNU extensions:

   nested-function-definition:
     declaration-specifiers declarator declaration-list[opt]
       compound-statement

   The simple-asm-expr and attributes are GNU extensions.

   This function does not handle __extension__; that is handled in its
   callers.  ??? Following the old parser, __extension__ may start
   external declarations, declarations in functions and declarations
   at the start of "for" loops, but not old-style parameter
   declarations.

   C99 requires declaration specifiers in a function definition; the
   absence is diagnosed through the diagnosis of implicit int.  In GNU
   C we also allow but diagnose declarations without declaration
   specifiers, but only at top level (elsewhere they conflict with
   other syntax).  */

static void
c_parser_declaration_or_fndef (c_parser *parser, bool fndef_ok, bool empty_ok,
			       bool nested, bool start_attr_ok)
{
  struct c_declspecs *specs;
  tree prefix_attrs;
  tree all_prefix_attrs;
  bool diagnosed_no_specs = false;
  specs = build_null_declspecs ();
  c_parser_declspecs (parser, specs, true, true, start_attr_ok);
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
  if (c_parser_next_token_is (parser, CPP_SEMICOLON))
    {
      if (empty_ok)
	shadow_tag (specs);
      else
	{
	  shadow_tag_warned (specs, 1);
	  pedwarn ("empty declaration");
	}
      c_parser_consume_token (parser);
      return;
    }
  pending_xref_error ();
  prefix_attrs = specs->attrs;
  all_prefix_attrs = prefix_attrs;
  specs->attrs = NULL_TREE;
  while (true)
    {
      struct c_declarator *declarator;
      bool dummy = false;
      tree fnbody;
      /* Declaring either one or more declarators (in which case we
	 should diagnose if there were no declaration specifiers) or a
	 function definition (in which case the diagnostic for
	 implicit int suffices).  */
      declarator = c_parser_declarator (parser, specs->type_seen_p,
					C_DTR_NORMAL, &dummy);
      if (declarator == NULL)
	{
	  c_parser_skip_to_end_of_block_or_statement (parser);
	  return;
	}
      if (c_parser_next_token_is (parser, CPP_EQ)
	  || c_parser_next_token_is (parser, CPP_COMMA)
	  || c_parser_next_token_is (parser, CPP_SEMICOLON)
	  || c_parser_next_token_is_keyword (parser, RID_ASM)
	  || c_parser_next_token_is_keyword (parser, RID_ATTRIBUTE))
	{
	  tree asm_name = NULL_TREE;
	  tree postfix_attrs = NULL_TREE;
	  if (!diagnosed_no_specs && !specs->declspecs_seen_p)
	    {
	      diagnosed_no_specs = true;
	      pedwarn ("data definition has no type or storage class");
	    }
	  /* Having seen a data definition, there cannot now be a
	     function definition.  */
	  fndef_ok = false;
	  if (c_parser_next_token_is_keyword (parser, RID_ASM))
	    asm_name = c_parser_simple_asm_expr (parser);
	  if (c_parser_next_token_is_keyword (parser, RID_ATTRIBUTE))
	    postfix_attrs = c_parser_attributes (parser);
	  if (c_parser_next_token_is (parser, CPP_EQ))
	    {
	      tree d;
	      struct c_expr init;
	      c_parser_consume_token (parser);
	      /* The declaration of the variable is in effect while
		 its initializer is parsed.  */
	      d = start_decl (declarator, specs, true,
			      chainon (postfix_attrs, all_prefix_attrs));
	      if (!d)
		d = error_mark_node;
	      start_init (d, asm_name, global_bindings_p ());
	      init = c_parser_initializer (parser);
	      finish_init ();
	      if (d != error_mark_node)
		{
		  maybe_warn_string_init (TREE_TYPE (d), init);
		  finish_decl (d, init.value, asm_name);
		}
	    }
	  else
	    {
	      tree d = start_decl (declarator, specs, false,
				   chainon (postfix_attrs,
					    all_prefix_attrs));
	      if (d)
		finish_decl (d, NULL_TREE, asm_name);
	    }
	  if (c_parser_next_token_is (parser, CPP_COMMA))
	    {
	      c_parser_consume_token (parser);
	      if (c_parser_next_token_is_keyword (parser, RID_ATTRIBUTE))
		all_prefix_attrs = chainon (c_parser_attributes (parser),
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
	  else
	    {
	      c_parser_error (parser, "expected %<,%> or %<;%>");
	      c_parser_skip_to_end_of_block_or_statement (parser);
	      return;
	    }
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
	  if (pedantic)
	    pedwarn ("ISO C forbids nested functions");
	  push_function_context ();
	}
      if (!start_function (specs, declarator, all_prefix_attrs))
	{
	  /* This can appear in many cases looking nothing like a
	     function definition, so we don't give a more specific
	     error suggesting there was one.  */
	  c_parser_error (parser, "expected %<=%>, %<,%>, %<;%>, %<asm%> "
			  "or %<__attribute__%>");
	  if (nested)
	    pop_function_context ();
	  break;
	}
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
	c_parser_declaration_or_fndef (parser, false, false, true, false);
      DECL_SOURCE_LOCATION (current_function_decl)
	= c_parser_peek_token (parser)->location;
      store_parm_decls ();
      fnbody = c_parser_compound_statement (parser);
      if (nested)
	{
	  tree decl = current_function_decl;
	  add_stmt (fnbody);
	  finish_function ();
	  pop_function_context ();
	  add_stmt (build_stmt (DECL_EXPR, decl));
	}
      else
	{
	  add_stmt (fnbody);
	  finish_function ();
	}
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
  /* ??? This only works sensibly in the presence of
     -fno-unit-at-a-time; file-scope asms really need to be passed to
     cgraph which needs to preserve the order of functions and
     file-scope asms.  */
  if (asm_str)
    assemble_asm (asm_str);
  c_parser_skip_until_found (parser, CPP_SEMICOLON, "expected %<;%>");
}

/* Parse some declaration specifiers (possibly none) (C90 6.5, C99
   6.7), adding them to SPECS (which may already include some).
   Storage class specifiers are accepted iff SCSPEC_OK; type
   specifiers are accepted iff TYPESPEC_OK; attributes are accepted at
   the start iff START_ATTR_OK.

   declaration-specifiers:
     storage-class-specifier declaration-specifiers[opt]
     type-specifier declaration-specifiers[opt]
     type-qualifier declaration-specifiers[opt]
     function-specifier declaration-specifiers[opt]

   Function specifiers (inline) are from C99, and are currently
   handled as storage class specifiers, as is __thread.

   C90 6.5.1, C99 6.7.1:
   storage-class-specifier:
     typedef
     extern
     static
     auto
     register

   C99 6.7.4:
   function-specifier:
     inline

   C90 6.5.2, C99 6.7.2:
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

   (_Bool and _Complex are new in C99.)

   C90 6.5.3, C99 6.7.3:

   type-qualifier:
     const
     restrict
     volatile

   (restrict is new in C99.)

   GNU extensions:

   declaration-specifiers:
     attributes declaration-specifiers[opt]

   storage-class-specifier:
     __thread

   type-specifier:
     typeof-specifier

   Objective-C:

   type-specifier:
     class-name objc-protocol-refs[opt]
     typedef-name objc-protocol-refs
     objc-protocol-refs
*/

static void
c_parser_declspecs (c_parser *parser, struct c_declspecs *specs,
		    bool scspec_ok, bool typespec_ok, bool start_attr_ok)
{
  bool attrs_ok = start_attr_ok;
  bool seen_type = specs->type_seen_p;
  while (c_parser_next_token_is (parser, CPP_NAME)
	 || c_parser_next_token_is (parser, CPP_KEYWORD)
	 || (c_dialect_objc () && c_parser_next_token_is (parser, CPP_LESS)))
    {
      struct c_typespec t;
      tree attrs;
      if (c_parser_next_token_is (parser, CPP_NAME))
	{
	  tree value = c_parser_peek_token (parser)->value;
	  c_id_kind kind = c_parser_peek_token (parser)->id_kind;
	  /* This finishes the specifiers unless a type name is OK, it
	     is declared as a type name and a type name hasn't yet
	     been seen.  */
	  if (!typespec_ok || seen_type
	      || (kind != C_ID_TYPENAME && kind != C_ID_CLASSNAME))
	    break;
	  c_parser_consume_token (parser);
	  seen_type = true;
	  attrs_ok = true;
	  if (kind == C_ID_TYPENAME
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
	  declspecs_add_type (specs, t);
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
	  declspecs_add_type (specs, t);
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
	case RID_AUTO:
	case RID_THREAD:
	  if (!scspec_ok)
	    goto out;
	  attrs_ok = true;
	  /* TODO: Distinguish between function specifiers (inline)
	     and storage class specifiers, either here or in
	     declspecs_add_scspec.  */
	  declspecs_add_scspec (specs, c_parser_peek_token (parser)->value);
	  c_parser_consume_token (parser);
	  break;
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
	case RID_BOOL:
	  if (!typespec_ok)
	    goto out;
	  attrs_ok = true;
	  seen_type = true;
	  OBJC_NEED_RAW_IDENTIFIER (1);
	  t.kind = ctsk_resword;
	  t.spec = c_parser_peek_token (parser)->value;
	  declspecs_add_type (specs, t);
	  c_parser_consume_token (parser);
	  break;
	case RID_ENUM:
	  if (!typespec_ok)
	    goto out;
	  attrs_ok = true;
	  seen_type = true;
	  t = c_parser_enum_specifier (parser);
	  declspecs_add_type (specs, t);
	  break;
	case RID_STRUCT:
	case RID_UNION:
	  if (!typespec_ok)
	    goto out;
	  attrs_ok = true;
	  seen_type = true;
	  t = c_parser_struct_or_union_specifier (parser);
	  declspecs_add_type (specs, t);
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
	  declspecs_add_type (specs, t);
	  break;
	case RID_CONST:
	case RID_VOLATILE:
	case RID_RESTRICT:
	  attrs_ok = true;
	  declspecs_add_qual (specs, c_parser_peek_token (parser)->value);
	  c_parser_consume_token (parser);
	  break;
	case RID_ATTRIBUTE:
	  if (!attrs_ok)
	    goto out;
	  attrs = c_parser_attributes (parser);
	  declspecs_add_attrs (specs, attrs);
	  break;
	default:
	  goto out;
	}
    }
 out: ;
}

/* Parse an enum specifier (C90 6.5.2.2, C99 6.7.2.2).

   enum-specifier:
     enum attributes[opt] identifier[opt] { enumerator-list } attributes[opt]
     enum attributes[opt] identifier[opt] { enumerator-list , } attributes[opt]
     enum attributes[opt] identifier

   The form with trailing comma is new in C99.  The forms with
   attributes are GNU extensions.  In GNU C, we accept any expression
   without commas in the syntax (assignment expressions, not just
   conditional expressions); assignment expressions will be diagnosed
   as non-constant.

   enumerator-list:
     enumerator
     enumerator-list , enumerator

   enumerator:
     enumeration-constant
     enumeration-constant = constant-expression
*/

static struct c_typespec
c_parser_enum_specifier (c_parser *parser)
{
  struct c_typespec ret;
  tree attrs;
  tree ident = NULL_TREE;
  gcc_assert (c_parser_next_token_is_keyword (parser, RID_ENUM));
  c_parser_consume_token (parser);
  attrs = c_parser_attributes (parser);
  if (c_parser_next_token_is (parser, CPP_NAME))
    {
      ident = c_parser_peek_token (parser)->value;
      c_parser_consume_token (parser);
    }
  if (c_parser_next_token_is (parser, CPP_OPEN_BRACE))
    {
      /* Parse an enum definition.  */
      tree type = start_enum (ident);
      tree postfix_attrs;
      /* We chain the enumerators in reverse order, then put them in
	 forward order at the end.  */
      tree values = NULL_TREE;
      c_parser_consume_token (parser);
      while (true)
	{
	  tree enum_id;
	  tree enum_value;
	  tree enum_decl;
	  bool seen_comma;
	  if (c_parser_next_token_is_not (parser, CPP_NAME))
	    {
	      c_parser_error (parser, "expected identifier");
	      c_parser_skip_until_found (parser, CPP_CLOSE_BRACE, NULL);
	      values = error_mark_node;
	      break;
	    }
	  enum_id = c_parser_peek_token (parser)->value;
	  c_parser_consume_token (parser);
	  if (c_parser_next_token_is (parser, CPP_EQ))
	    {
	      c_parser_consume_token (parser);
	      enum_value = c_parser_expr_no_commas (parser, NULL).value;
	    }
	  else
	    enum_value = NULL_TREE;
	  enum_decl = build_enumerator (enum_id, enum_value);
	  TREE_CHAIN (enum_decl) = values;
	  values = enum_decl;
	  seen_comma = false;
	  if (c_parser_next_token_is (parser, CPP_COMMA))
	    {
	      seen_comma = true;
	      c_parser_consume_token (parser);
	    }
	  if (c_parser_next_token_is (parser, CPP_CLOSE_BRACE))
	    {
	      if (seen_comma && pedantic && !flag_isoc99)
		pedwarn ("comma at end of enumerator list");
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
      postfix_attrs = c_parser_attributes (parser);
      ret.spec = finish_enum (type, nreverse (values),
			      chainon (attrs, postfix_attrs));
      ret.kind = ctsk_tagdef;
      return ret;
    }
  else if (!ident)
    {
      c_parser_error (parser, "expected %<{%>");
      ret.spec = error_mark_node;
      ret.kind = ctsk_tagref;
      return ret;
    }
  ret = parser_xref_tag (ENUMERAL_TYPE, ident);
  /* In ISO C, enumerated types can be referred to only if already
     defined.  */
  if (pedantic && !COMPLETE_TYPE_P (ret.spec))
    pedwarn ("ISO C forbids forward references to %<enum%> types");
  return ret;
}

/* Parse a struct or union specifier (C90 6.5.2.1, C99 6.7.2.1).

   struct-or-union-specifier:
     struct-or-union attributes[opt] identifier[opt]
       { struct-contents } attributes[opt]
     struct-or-union attributes[opt] identifier

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
  tree attrs;
  tree ident = NULL_TREE;
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
  c_parser_consume_token (parser);
  attrs = c_parser_attributes (parser);
  if (c_parser_next_token_is (parser, CPP_NAME))
    {
      ident = c_parser_peek_token (parser)->value;
      c_parser_consume_token (parser);
    }
  if (c_parser_next_token_is (parser, CPP_OPEN_BRACE))
    {
      /* Parse a struct or union definition.  Start the scope of the
	 tag before parsing components.  */
      tree type = start_struct (code, ident);
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
      tree contents = NULL_TREE;
      c_parser_consume_token (parser);
      /* Handle the Objective-C @defs construct,
	 e.g. foo(sizeof(struct{ @defs(ClassName) }));.  */
      if (c_parser_next_token_is_keyword (parser, RID_AT_DEFS))
	{
	  tree name;
	  gcc_assert (c_dialect_objc ());
	  c_parser_consume_token (parser);
	  if (!c_parser_require (parser, CPP_OPEN_PAREN, "expected %<(%>"))
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
	  c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
				     "expected %<)%>");
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
	      if (pedantic)
		pedwarn ("extra semicolon in struct or union specified");
	      c_parser_consume_token (parser);
	      continue;
	    }
	  /* Stop if at the end of the struct or union contents.  */
	  if (c_parser_next_token_is (parser, CPP_CLOSE_BRACE))
	    {
	      c_parser_consume_token (parser);
	      break;
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
		pedwarn ("no semicolon at end of struct or union");
	      else
		{
		  c_parser_error (parser, "expected %<;%>");
		  c_parser_skip_until_found (parser, CPP_CLOSE_BRACE, NULL);
		  break;
		}
	    }
	}
      postfix_attrs = c_parser_attributes (parser);
      ret.spec = finish_struct (type, nreverse (contents),
				chainon (attrs, postfix_attrs));
      ret.kind = ctsk_tagdef;
      return ret;
    }
  else if (!ident)
    {
      c_parser_error (parser, "expected %<{%>");
      ret.spec = error_mark_node;
      ret.kind = ctsk_tagref;
      return ret;
    }
  ret = parser_xref_tag (code, ident);
  return ret;
}

/* Parse a struct-declaration (C90 6.5.2.1, C99 6.7.2.1), *without*
   the trailing semicolon.

   struct-declaration:
     specifier-qualifier-list struct-declarator-list

   specifier-qualifier-list:
     type-specifier specifier-qualifier-list[opt]
     type-qualifier specifier-qualifier-list[opt]
     attributes specifier-qualifier-list[opt]

   struct-declarator-list:
     struct-declarator
     struct-declarator-list , attributes[opt] struct-declarator

   struct-declarator:
     declarator attributes[opt]
     declarator[opt] : constant-expression attributes[opt]

   GNU extensions:

   struct-declaration:
     __extension__ struct-declaration
     specifier-qualifier-list

   Unlike the ISO C syntax, semicolons are handled elsewhere.  The use
   of attributes where shown is a GNU extension.  In GNU C, we accept
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
  specs = build_null_declspecs ();
  c_parser_declspecs (parser, specs, false, true, true);
  if (parser->error)
    return NULL_TREE;
  if (!specs->declspecs_seen_p)
    {
      c_parser_error (parser, "expected specifier-qualifier-list");
      return NULL_TREE;
    }
  finish_declspecs (specs);
  if (c_parser_next_token_is (parser, CPP_SEMICOLON))
    {
      tree ret;
      if (!specs->type_seen_p)
	{
	  if (pedantic)
	    pedwarn ("ISO C forbids member declarations with no members");
	  shadow_tag_warned (specs, pedantic);
	  ret = NULL_TREE;
	}
      else
	{
	  /* Support for unnamed structs or unions as members of
	     structs or unions (which is [a] useful and [b] supports
	     MS P-SDK).  */
	  ret = grokfield (build_id_declarator (NULL_TREE), specs, NULL_TREE);
	}
      return ret;
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
	declarator = c_parser_declarator (parser, specs->type_seen_p,
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
	    postfix_attrs = c_parser_attributes (parser);
	  d = grokfield (declarator, specs, width);
	  decl_attributes (&d, chainon (postfix_attrs,
					all_prefix_attrs), 0);
	  TREE_CHAIN (d) = decls;
	  decls = d;
	  if (c_parser_next_token_is_keyword (parser, RID_ATTRIBUTE))
	    all_prefix_attrs = chainon (c_parser_attributes (parser),
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
  gcc_assert (c_parser_next_token_is_keyword (parser, RID_TYPEOF));
  c_parser_consume_token (parser);
  skip_evaluation++;
  in_typeof++;
  if (!c_parser_require (parser, CPP_OPEN_PAREN, "expected %<(%>"))
    {
      skip_evaluation--;
      in_typeof--;
      return ret;
    }
  if (c_parser_next_token_starts_typename (parser))
    {
      struct c_type_name *type = c_parser_type_name (parser);
      skip_evaluation--;
      in_typeof--;
      if (type != NULL)
	{
	  ret.spec = groktypename (type);
	  pop_maybe_used (variably_modified_type_p (ret.spec, NULL_TREE));
	}
    }
  else
    {
      struct c_expr expr = c_parser_expression (parser);
      skip_evaluation--;
      in_typeof--;
      if (TREE_CODE (expr.value) == COMPONENT_REF
	  && DECL_C_BIT_FIELD (TREE_OPERAND (expr.value, 1)))
	error ("%<typeof%> applied to a bit-field");
      ret.spec = TREE_TYPE (expr.value);
      pop_maybe_used (variably_modified_type_p (ret.spec, NULL_TREE));
    }
  c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, "expected %<)%>");
  return ret;
}

/* Parse a declarator, possibly an abstract declarator (C90 6.5.4,
   6.5.5, C99 6.7.5, 6.7.6).  If TYPE_SEEN_P then a typedef name may
   be redeclared; otherwise it may not.  KIND indicates which kind of
   declarator is wanted.  Returns a valid declarator except in the
   case of a syntax error in which case NULL is returned.  *SEEN_ID is
   set to true if an identifier being declared is seen; this is used
   to diagnose bad forms of abstract array declarators and to
   determine whether an identifier list is syntactically permitted.

   declarator:
     pointer[opt] direct-declarator

   direct-declarator:
     identifier
     ( attributes[opt] declarator )
     direct-declarator array-declarator
     direct-declarator ( parameter-type-list )
     direct-declarator ( identifier-list[opt] )

   pointer:
     * type-qualifier-list[opt]
     * type-qualifier-list[opt] pointer

   type-qualifier-list:
     type-qualifier
     attributes
     type-qualifier-list type-qualifier
     type-qualifier-list attributes

   parameter-type-list:
     parameter-list
     parameter-list , ...

   parameter-list:
     parameter-declaration
     parameter-list , parameter-declaration

   parameter-declaration:
     declaration-specifiers declarator attributes[opt]
     declaration-specifiers abstract-declarator[opt] attributes[opt]

   identifier-list:
     identifier
     identifier-list , identifier

   abstract-declarator:
     pointer
     pointer[opt] direct-abstract-declarator

   direct-abstract-declarator:
     ( attributes[opt] abstract-declarator )
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

   The uses of attributes shown above are GNU extensions.

   Some forms of array declarator are not included in C99 in the
   syntax for abstract declarators; these are disallowed elsewhere.
   This may be a defect (DR#289).

   This function also accepts an omitted abstract declarator as being
   an abstract declarator, although not part of the formal syntax.  */

static struct c_declarator *
c_parser_declarator (c_parser *parser, bool type_seen_p, c_dtr_syn kind,
		     bool *seen_id)
{
  /* Parse any initial pointer part.  */
  if (c_parser_next_token_is (parser, CPP_MULT))
    {
      struct c_declspecs *quals_attrs = build_null_declspecs ();
      struct c_declarator *inner;
      c_parser_consume_token (parser);
      c_parser_declspecs (parser, quals_attrs, false, false, true);
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
     following attributes must be read.  If a declaration specifier
     follows, then it is a parameter list; if the specifier is a
     typedef name, there might be an ambiguity about redeclaring it,
     which is resolved in the direction of treating it as a typedef
     name.  If a close parenthesis follows, it is also an empty
     parameter list, as the syntax does not permit empty abstract
     declarators.  Otherwise, it is a parenthesised declarator (in
     which case the analysis may be repeated inside it, recursively).

     ??? There is an ambiguity in a parameter declaration "int
     (__attribute__((foo)) x)", where x is not a typedef name: it
     could be an abstract declarator for a function, or declare x with
     parentheses.  The proper resolution of this ambiguity needs
     documenting.  At present we follow an accident of the old
     parser's implementation, whereby the first parameter must have
     some declaration specifiers other than just attributes.  Thus as
     a parameter declaration it is treated as a parenthesised
     parameter named x, and as an abstract declarator it is
     rejected.

     ??? Also following the old parser, attributes inside an empty
     parameter list are ignored, making it a list not yielding a
     prototype, rather than giving an error or making it have one
     parameter with implicit type int.

     ??? Also following the old parser, typedef names may be
     redeclared in declarators, but not Objective-C class names.  */

  if (kind != C_DTR_ABSTRACT
      && c_parser_next_token_is (parser, CPP_NAME)
      && ((type_seen_p
	   && c_parser_peek_token (parser)->id_kind == C_ID_TYPENAME)
	  || c_parser_peek_token (parser)->id_kind == C_ID_ID))
    {
      struct c_declarator *inner
	= build_id_declarator (c_parser_peek_token (parser)->value);
      *seen_id = true;
      inner->id_loc = c_parser_peek_token (parser)->location;
      c_parser_consume_token (parser);
      return c_parser_direct_declarator_inner (parser, *seen_id, inner);
    }

  if (kind != C_DTR_NORMAL
      && c_parser_next_token_is (parser, CPP_OPEN_SQUARE))
    {
      struct c_declarator *inner = build_id_declarator (NULL_TREE);
      return c_parser_direct_declarator_inner (parser, *seen_id, inner);
    }

  /* Either we are at the end of an abstract declarator, or we have
     parentheses.  */

  if (c_parser_next_token_is (parser, CPP_OPEN_PAREN))
    {
      tree attrs;
      struct c_declarator *inner;
      c_parser_consume_token (parser);
      attrs = c_parser_attributes (parser);
      if (kind != C_DTR_NORMAL
	  && (c_parser_next_token_starts_declspecs (parser)
	      || c_parser_next_token_is (parser, CPP_CLOSE_PAREN)))
	{
	  struct c_arg_info *args
	    = c_parser_parms_declarator (parser, kind == C_DTR_NORMAL,
					 attrs);
	  if (args == NULL)
	    return NULL;
	  else
	    {
	      inner
		= build_function_declarator (args,
					     build_id_declarator (NULL_TREE));
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
  if (c_parser_next_token_is (parser, CPP_OPEN_SQUARE))
    {
      struct c_declarator *declarator;
      struct c_declspecs *quals_attrs = build_null_declspecs ();
      bool static_seen;
      bool star_seen;
      tree dimen;
      c_parser_consume_token (parser);
      c_parser_declspecs (parser, quals_attrs, false, false, true);
      static_seen = c_parser_next_token_is_keyword (parser, RID_STATIC);
      if (static_seen)
	c_parser_consume_token (parser);
      if (static_seen && !quals_attrs->declspecs_seen_p)
	c_parser_declspecs (parser, quals_attrs, false, false, true);
      if (!quals_attrs->declspecs_seen_p)
	quals_attrs = NULL;
      /* If "static" is present, there must be an array dimension.
	 Otherwise, there may be a dimension, "*", or no
	 dimension.  */
      if (static_seen)
	{
	  star_seen = false;
	  dimen = c_parser_expr_no_commas (parser, NULL).value;
	}
      else
	{
	  if (c_parser_next_token_is (parser, CPP_CLOSE_SQUARE))
	    {
	      dimen = NULL_TREE;
	      star_seen = false;
	    }
	  else if (c_parser_next_token_is (parser, CPP_MULT))
	    {
	      if (c_parser_peek_2nd_token (parser)->type == CPP_CLOSE_SQUARE)
		{
		  dimen = NULL_TREE;
		  star_seen = true;
		  c_parser_consume_token (parser);
		}
	      else
		{
		  star_seen = false;
		  dimen = c_parser_expr_no_commas (parser, NULL).value;
		}
	    }
	  else
	    {
	      star_seen = false;
	      dimen = c_parser_expr_no_commas (parser, NULL).value;
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
      declarator = build_array_declarator (dimen, quals_attrs, static_seen,
					   star_seen);
      inner = set_array_declarator_inner (declarator, inner, !id_present);
      return c_parser_direct_declarator_inner (parser, id_present, inner);
    }
  else if (c_parser_next_token_is (parser, CPP_OPEN_PAREN))
    {
      tree attrs;
      struct c_arg_info *args;
      c_parser_consume_token (parser);
      attrs = c_parser_attributes (parser);
      args = c_parser_parms_declarator (parser, id_present, attrs);
      if (args == NULL)
	return NULL;
      else
	{
	  inner = build_function_declarator (args, inner);
	  return c_parser_direct_declarator_inner (parser, id_present, inner);
	}
    }
  return inner;
}

/* Parse a parameter list or identifier list, including the closing
   parenthesis but not the opening one.  ATTRS are the attributes at
   the start of the list.  ID_LIST_OK is true if an identifier list is
   acceptable; such a list must not have attributes at the start.  */

static struct c_arg_info *
c_parser_parms_declarator (c_parser *parser, bool id_list_ok, tree attrs)
{
  push_scope ();
  declare_parm_level ();
  /* If the list starts with an identifier, it is an identifier list.
     Otherwise, it is either a prototype list or an empty list.  */
  if (id_list_ok
      && !attrs
      && c_parser_next_token_is (parser, CPP_NAME)
      && c_parser_peek_token (parser)->id_kind == C_ID_ID)
    {
      tree list = NULL_TREE;
      while (c_parser_next_token_is (parser, CPP_NAME)
	     && c_parser_peek_token (parser)->id_kind == C_ID_ID)
	{
	  list = chainon (list, build_tree_list (NULL_TREE,
						 c_parser_peek_token (parser)->value));
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
	  struct c_arg_info *ret = XOBNEW (&parser_obstack, struct c_arg_info);
	  ret->parms = 0;
	  ret->tags = 0;
	  ret->types = list;
	  ret->others = 0;
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
      struct c_arg_info *ret = c_parser_parms_list_declarator (parser, attrs);
      pop_scope ();
      return ret;
    }
}

/* Parse a parameter list (possibly empty), including the closing
   parenthesis but not the opening one.  ATTRS are the attributes at
   the start of the list.  */

static struct c_arg_info *
c_parser_parms_list_declarator (c_parser *parser, tree attrs)
{
  bool good_parm = false;
  /* ??? Following the old parser, forward parameter declarations may
     use abstract declarators, and if no real parameter declarations
     follow the forward declarations then this is not diagnosed.  Also
     note as above that attributes are ignored as the only contents of
     the parentheses, or as the only contents after forward
     declarations.  */
  if (c_parser_next_token_is (parser, CPP_CLOSE_PAREN))
    {
      struct c_arg_info *ret = XOBNEW (&parser_obstack, struct c_arg_info);
      ret->parms = 0;
      ret->tags = 0;
      ret->types = 0;
      ret->others = 0;
      c_parser_consume_token (parser);
      return ret;
    }
  if (c_parser_next_token_is (parser, CPP_ELLIPSIS))
    {
      struct c_arg_info *ret = XOBNEW (&parser_obstack, struct c_arg_info);
      ret->parms = 0;
      ret->tags = 0;
      ret->others = 0;
      /* Suppress -Wold-style-definition for this case.  */
      ret->types = error_mark_node;
      error ("ISO C requires a named argument before %<...%>");
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
      struct c_parm *parm = c_parser_parameter_declaration (parser, attrs);
      attrs = NULL_TREE;
      if (parm != NULL)
	{
	  good_parm = true;
	  push_parm_decl (parm);
	}
      if (c_parser_next_token_is (parser, CPP_SEMICOLON))
	{
	  tree new_attrs;
	  c_parser_consume_token (parser);
	  new_attrs = c_parser_attributes (parser);
	  return c_parser_parms_list_declarator (parser, new_attrs);
	}
      if (c_parser_next_token_is (parser, CPP_CLOSE_PAREN))
	{
	  c_parser_consume_token (parser);
	  if (good_parm)
	    return get_parm_info (false);
	  else
	    {
	      struct c_arg_info *ret
		= XOBNEW (&parser_obstack, struct c_arg_info);
	      ret->parms = 0;
	      ret->tags = 0;
	      ret->types = 0;
	      ret->others = 0;
	      return ret;
	    }
	}
      if (!c_parser_require (parser, CPP_COMMA,
			     "expected %<;%>, %<,%> or %<)%>"))
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
	      if (good_parm)
		return get_parm_info (true);
	      else
		{
		  struct c_arg_info *ret
		    = XOBNEW (&parser_obstack, struct c_arg_info);
		  ret->parms = 0;
		  ret->tags = 0;
		  ret->types = 0;
		  ret->others = 0;
		  return ret;
		}
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

/* Parse a parameter declaration.  ATTRS are the attributes at the
   start of the declaration if it is the first parameter.  */

static struct c_parm *
c_parser_parameter_declaration (c_parser *parser, tree attrs)
{
  struct c_declspecs *specs;
  struct c_declarator *declarator;
  tree prefix_attrs;
  tree postfix_attrs = NULL_TREE;
  bool dummy = false;
  if (!c_parser_next_token_starts_declspecs (parser))
    {
      /* ??? In some Objective-C cases '...' isn't applicable so there
	 should be a different message.  */
      c_parser_error (parser,
		      "expected declaration specifiers or %<...%>");
      c_parser_skip_to_end_of_parameter (parser);
      return NULL;
    }
  specs = build_null_declspecs ();
  if (attrs)
    {
      declspecs_add_attrs (specs, attrs);
      attrs = NULL_TREE;
    }
  c_parser_declspecs (parser, specs, true, true, true);
  finish_declspecs (specs);
  pending_xref_error ();
  prefix_attrs = specs->attrs;
  specs->attrs = NULL_TREE;
  declarator = c_parser_declarator (parser, specs->type_seen_p,
				    C_DTR_PARM, &dummy);
  if (declarator == NULL)
    {
      c_parser_skip_until_found (parser, CPP_COMMA, NULL);
      return NULL;
    }
  if (c_parser_next_token_is_keyword (parser, RID_ATTRIBUTE))
    postfix_attrs = c_parser_attributes (parser);
  return build_c_parm (specs, chainon (postfix_attrs, prefix_attrs),
		       declarator);
}

/* Parse a string literal in an asm expression.  It should not be
   translated, and wide string literals are an error although
   permitted by the syntax.  This is a GNU extension.

   asm-string-literal:
     string-literal

   ??? At present, following the old parser, the caller needs to have
   set c_lex_string_translate to 0.  It would be better to follow the
   C++ parser rather than using the c_lex_string_translate kludge.  */

static tree
c_parser_asm_string_literal (c_parser *parser)
{
  tree str;
  if (c_parser_next_token_is (parser, CPP_STRING))
    {
      str = c_parser_peek_token (parser)->value;
      c_parser_consume_token (parser);
    }
  else if (c_parser_next_token_is (parser, CPP_WSTRING))
    {
      error ("wide string literal in %<asm%>");
      str = build_string (1, "");
      c_parser_consume_token (parser);
    }
  else
    {
      c_parser_error (parser, "expected string literal");
      str = NULL_TREE;
    }
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
  /* ??? Follow the C++ parser rather than using the
     c_lex_string_translate kludge.  */
  c_lex_string_translate = 0;
  c_parser_consume_token (parser);
  if (!c_parser_require (parser, CPP_OPEN_PAREN, "expected %<(%>"))
    {
      c_lex_string_translate = 1;
      return NULL_TREE;
    }
  str = c_parser_asm_string_literal (parser);
  c_lex_string_translate = 1;
  if (!c_parser_require (parser, CPP_CLOSE_PAREN, "expected %<)%>"))
    {
      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, NULL);
      return NULL_TREE;
    }
  return str;
}

/* Parse (possibly empty) attributes.  This is a GNU extension.

   attributes:
     empty
     attributes attribute

   attribute:
     __attribute__ ( ( attribute-list ) )

   attribute-list:
     attrib
     attribute_list , attrib

   attrib:
     empty
     any-word
     any-word ( identifier )
     any-word ( identifier , nonempty-expr-list )
     any-word ( expr-list )

   where the "identifier" must not be declared as a type, and
   "any-word" may be any identifier (including one declared as a
   type), a reserved word storage class specifier, type specifier or
   type qualifier.  ??? This still leaves out most reserved keywords
   (following the old parser), shouldn't we include them, and why not
   allow identifiers declared as types to start the arguments?  */

static tree
c_parser_attributes (c_parser *parser)
{
  tree attrs = NULL_TREE;
  while (c_parser_next_token_is_keyword (parser, RID_ATTRIBUTE))
    {
      /* ??? Follow the C++ parser rather than using the
	 c_lex_string_translate kludge.  */
      c_lex_string_translate = 0;
      c_parser_consume_token (parser);
      if (!c_parser_require (parser, CPP_OPEN_PAREN, "expected %<(%>"))
	{
	  c_lex_string_translate = 1;
	  return attrs;
	}
      if (!c_parser_require (parser, CPP_OPEN_PAREN, "expected %<(%>"))
	{
	  c_lex_string_translate = 1;
	  c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, NULL);
	  return attrs;
	}
      /* Parse the attribute list.  */
      while (c_parser_next_token_is (parser, CPP_COMMA)
	     || c_parser_next_token_is (parser, CPP_NAME)
	     || c_parser_next_token_is (parser, CPP_KEYWORD))
	{
	  tree attr, attr_name, attr_args;
	  if (c_parser_next_token_is (parser, CPP_COMMA))
	    {
	      c_parser_consume_token (parser);
	      continue;
	    }
	  if (c_parser_next_token_is (parser, CPP_KEYWORD))
	    {
	      /* ??? See comment above about what keywords are
		 accepted here.  */
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
		case RID_BOOL:
		  ok = true;
		  break;
		default:
		  ok = false;
		  break;
		}
	      if (!ok)
		break;
	    }
	  attr_name = c_parser_peek_token (parser)->value;
	  c_parser_consume_token (parser);
	  if (c_parser_next_token_is_not (parser, CPP_OPEN_PAREN))
	    {
	      attr = build_tree_list (attr_name, NULL_TREE);
	      attrs = chainon (attrs, attr);
	      continue;
	    }
	  c_parser_consume_token (parser);
	  /* Parse the attribute contents.  If they start with an
	     identifier which is followed by a comma or close
	     parenthesis, then the arguments start with that
	     identifier; otherwise they are an expression list.  */
	  if (c_parser_next_token_is (parser, CPP_NAME)
	      && c_parser_peek_token (parser)->id_kind == C_ID_ID
	      && ((c_parser_peek_2nd_token (parser)->type == CPP_COMMA)
		  || (c_parser_peek_2nd_token (parser)->type
		      == CPP_CLOSE_PAREN)))
	    {
	      tree arg1 = c_parser_peek_token (parser)->value;
	      c_parser_consume_token (parser);
	      if (c_parser_next_token_is (parser, CPP_CLOSE_PAREN))
		attr_args = build_tree_list (NULL_TREE, arg1);
	      else
		{
		  c_parser_consume_token (parser);
		  attr_args = tree_cons (NULL_TREE, arg1,
					 c_parser_expr_list (parser));
		}
	    }
	  else
	    {
	      if (c_parser_next_token_is (parser, CPP_CLOSE_PAREN))
		attr_args = NULL_TREE;
	      else
		attr_args = c_parser_expr_list (parser);
	    }
	  attr = build_tree_list (attr_name, attr_args);
	  if (c_parser_next_token_is (parser, CPP_CLOSE_PAREN))
	    c_parser_consume_token (parser);
	  else
	    {
	      c_lex_string_translate = 1;
	      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
					 "expected %<)%>");
	      return attrs;
	    }
	  attrs = chainon (attrs, attr);
	}
      if (c_parser_next_token_is (parser, CPP_CLOSE_PAREN))
	c_parser_consume_token (parser);
      else
	{
	  c_lex_string_translate = 1;
	  c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
				     "expected %<)%>");
	  return attrs;
	}
      if (c_parser_next_token_is (parser, CPP_CLOSE_PAREN))
	c_parser_consume_token (parser);
      else
	{
	  c_lex_string_translate = 1;
	  c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
				     "expected %<)%>");
	  return attrs;
	}
      c_lex_string_translate = 1;
    }
  return attrs;
}

/* Parse a type name (C90 6.5.5, C99 6.7.6).

   type-name:
     specifier-qualifier-list abstract-declarator[opt]
*/

static struct c_type_name *
c_parser_type_name (c_parser *parser)
{
  struct c_declspecs *specs = build_null_declspecs ();
  struct c_declarator *declarator;
  struct c_type_name *ret;
  bool dummy = false;
  c_parser_declspecs (parser, specs, false, true, true);
  if (!specs->declspecs_seen_p)
    {
      c_parser_error (parser, "expected specifier-qualifier-list");
      return NULL;
    }
  pending_xref_error ();
  finish_declspecs (specs);
  declarator = c_parser_declarator (parser, specs->type_seen_p,
				    C_DTR_ABSTRACT, &dummy);
  if (declarator == NULL)
    return NULL;
  ret = XOBNEW (&parser_obstack, struct c_type_name);
  ret->specs = specs;
  ret->declarator = declarator;
  return ret;
}

/* Parse an initializer (C90 6.5.7, C99 6.7.8).

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
    return c_parser_braced_init (parser, NULL_TREE, false);
  else
    return c_parser_expr_no_commas (parser, NULL);
}

/* Parse a braced initializer list.  TYPE is the type specified for a
   compound literal, and NULL_TREE for other initializers and for
   nested braced lists.  NESTED_P is true for nested braced lists,
   false for the list of a compound literal or the list that is the
   top-level initializer in a declaration.  */

static struct c_expr
c_parser_braced_init (c_parser *parser, tree type, bool nested_p)
{
  gcc_assert (c_parser_next_token_is (parser, CPP_OPEN_BRACE));
  c_parser_consume_token (parser);
  if (nested_p)
    push_init_level (0);
  else
    really_start_incremental_init (type);
  if (c_parser_next_token_is (parser, CPP_CLOSE_BRACE))
    {
      if (pedantic)
	pedwarn ("ISO C forbids empty initializer braces");
    }
  else
    {
      /* Parse a non-empty initializer list, possibly with a trailing
	 comma.  */
      while (true)
	{
	  c_parser_initelt (parser);
	  if (parser->error)
	    break;
	  if (c_parser_next_token_is (parser, CPP_COMMA))
	    c_parser_consume_token (parser);
	  else
	    break;
	  if (c_parser_next_token_is (parser, CPP_CLOSE_BRACE))
	    break;
	}
    }
  if (c_parser_next_token_is_not (parser, CPP_CLOSE_BRACE))
    {
      struct c_expr ret;
      ret.value = error_mark_node;
      ret.original_code = ERROR_MARK;
      c_parser_skip_until_found (parser, CPP_CLOSE_BRACE, "expected %<}%>");
      return ret;
    }
  c_parser_consume_token (parser);
  return pop_init_level (0);
}

/* Parse a nested initializer, including designators.  */

static void
c_parser_initelt (c_parser *parser)
{
  /* Parse any designator or designator list.  A single array
     designator may have the subsequent "=" omitted in GNU C, but a
     longer list or a structure member designator may not.  */
  if (c_parser_next_token_is (parser, CPP_NAME)
      && c_parser_peek_2nd_token (parser)->type == CPP_COLON)
    {
      /* Old-style structure member designator.  */
      set_init_label (c_parser_peek_token (parser)->value);
      if (pedantic)
	pedwarn ("obsolete use of designated initializer with %<:%>");
      c_parser_consume_token (parser);
      c_parser_consume_token (parser);
    }
  else
    {
      /* des_seen is 0 if there have been no designators, 1 if there
	 has been a single array designator and 2 otherwise.  */
      int des_seen = 0;
      while (c_parser_next_token_is (parser, CPP_OPEN_SQUARE)
	     || c_parser_next_token_is (parser, CPP_DOT))
	{
	  int des_prev = des_seen;
	  if (des_seen < 2)
	    des_seen++;
	  if (c_parser_next_token_is (parser, CPP_DOT))
	    {
	      des_seen = 2;
	      c_parser_consume_token (parser);
	      if (c_parser_next_token_is (parser, CPP_NAME))
		{
		  set_init_label (c_parser_peek_token (parser)->value);
		  c_parser_consume_token (parser);
		}
	      else
		{
		  struct c_expr init;
		  init.value = error_mark_node;
		  init.original_code = ERROR_MARK;
		  c_parser_error (parser, "expected identifier");
		  c_parser_skip_until_found (parser, CPP_COMMA, NULL);
		  process_init_element (init);
		  return;
		}
	    }
	  else
	    {
	      tree first, second;
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
		  if (c_parser_next_token_is (parser, CPP_ELLIPSIS)
		      || c_parser_next_token_is (parser, CPP_CLOSE_SQUARE))
		    goto array_desig_after_first;
		  /* Expression receiver.  So far only one part
		     without commas has been parsed; there might be
		     more of the expression.  */
		  rec = first;
		  while (c_parser_next_token_is (parser, CPP_COMMA))
		    {
		      tree next;
		      c_parser_consume_token (parser);
		      next = c_parser_expr_no_commas (parser, NULL).value;
		      rec = build_compound_expr (rec, next);
		    }
		parse_message_args:
		  /* Now parse the objc-message-args.  */
		  args = c_parser_objc_message_args (parser);
		  c_parser_skip_until_found (parser, CPP_CLOSE_SQUARE,
					     "expected %<]%>");
		  mexpr.value
		    = objc_build_message_expr (build_tree_list (rec, args));
		  mexpr.original_code = ERROR_MARK;
		  /* Now parse and process the remainder of the
		     initializer, starting with this message
		     expression as a primary-expression.  */
		  c_parser_initval (parser, &mexpr);
		  return;
		}
	      c_parser_consume_token (parser);
	      first = c_parser_expr_no_commas (parser, NULL).value;
	    array_desig_after_first:
	      if (c_parser_next_token_is (parser, CPP_ELLIPSIS))
		{
		  c_parser_consume_token (parser);
		  second = c_parser_expr_no_commas (parser, NULL).value;
		}
	      else
		second = NULL_TREE;
	      if (c_parser_next_token_is (parser, CPP_CLOSE_SQUARE))
		{
		  c_parser_consume_token (parser);
		  set_init_index (first, second);
		  if (pedantic && second)
		    pedwarn ("ISO C forbids specifying range of "
			     "elements to initialize");
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
	      if (pedantic && !flag_isoc99)
		pedwarn ("ISO C90 forbids specifying subobject to initialize");
	      c_parser_consume_token (parser);
	    }
	  else
	    {
	      if (des_seen == 1)
		{
		  if (pedantic)
		    pedwarn ("obsolete use of designated initializer "
			     "without %<=%>");
		}
	      else
		{
		  struct c_expr init;
		  init.value = error_mark_node;
		  init.original_code = ERROR_MARK;
		  c_parser_error (parser, "expected %<=%>");
		  c_parser_skip_until_found (parser, CPP_COMMA, NULL);
		  process_init_element (init);
		  return;
		}
	    }
	}
    }
  c_parser_initval (parser, NULL);
}

/* Parse a nested initializer; as c_parser_initializer but parses
   initializers within braced lists, after any designators have been
   applied.  If AFTER is not NULL then it is an Objective-C message
   expression which is the primary-expression starting the
   initializer.  */

static void
c_parser_initval (c_parser *parser, struct c_expr *after)
{
  struct c_expr init;
  gcc_assert (!after || c_dialect_objc ());
  if (c_parser_next_token_is (parser, CPP_OPEN_BRACE) && !after)
    init = c_parser_braced_init (parser, NULL_TREE, true);
  else
    init = c_parser_expr_no_commas (parser, after);
  process_init_element (init);
}

/* Parse a compound statement (possibly a function body) (C90 6.6.2,
   C99 6.8.2).

   compound-statement:
     { block-item-list[opt] }
     { label-declarations block-item-list }

   block-item-list:
     block-item
     block-item-list block-item

   block-item:
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
   there would be a conflict between attributes on the label and
   prefix attributes on the declaration.  ??? The syntax follows the
   old parser in requiring something after label declarations.
   Although they are erroneous if the labels declared aren't defined,
   is it useful for the syntax to be this way?  */

static tree
c_parser_compound_statement (c_parser *parser)
{
  tree stmt;
  if (!c_parser_require (parser, CPP_OPEN_BRACE, "expected %<{%>"))
    return error_mark_node;
  stmt = c_begin_compound_stmt (true);
  c_parser_compound_statement_nostart (parser);
  return c_end_compound_stmt (stmt, true);
}

/* Parse a compound statement except for the opening brace.  This is
   used for parsing both compound statements and statement expressions
   (which follow different paths to handling the opening).  */

static void
c_parser_compound_statement_nostart (c_parser *parser)
{
  bool last_stmt = false;
  bool last_label = false;
  if (c_parser_next_token_is (parser, CPP_CLOSE_BRACE))
    {
      c_parser_consume_token (parser);
      return;
    }
  if (c_parser_next_token_is_keyword (parser, RID_LABEL))
    {
      /* Read zero or more forward-declarations for labels that nested
	 functions can jump to.  */
      while (c_parser_next_token_is_keyword (parser, RID_LABEL))
	{
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
	      add_stmt (build_stmt (DECL_EXPR, label));
	      c_parser_consume_token (parser);
	      if (c_parser_next_token_is (parser, CPP_COMMA))
		c_parser_consume_token (parser);
	      else
		break;
	    }
	  c_parser_skip_until_found (parser, CPP_SEMICOLON, "expected %<;%>");
	}
      /* ??? Locating this diagnostic on the token after the
	 declarations end follows the old parser, but it might be
	 better to locate it where the declarations start instead.  */
      if (pedantic)
	pedwarn ("ISO C forbids label declarations");
    }
  /* We must now have at least one statement, label or declaration.  */
  if (c_parser_next_token_is (parser, CPP_CLOSE_BRACE))
    {
      c_parser_error (parser, "expected declaration or statement");
      c_parser_consume_token (parser);
      return;
    }
  while (c_parser_next_token_is_not (parser, CPP_CLOSE_BRACE))
    {
      location_t loc = c_parser_peek_token (parser)->location;
      if (c_parser_next_token_is (parser, CPP_EOF))
	{
	  c_parser_error (parser, "expected declaration or statement");
	  return;
	}
      if (c_parser_next_token_is_keyword (parser, RID_CASE)
	  || c_parser_next_token_is_keyword (parser, RID_DEFAULT)
	  || (c_parser_next_token_is (parser, CPP_NAME)
	      && c_parser_peek_2nd_token (parser)->type == CPP_COLON))
	{
	  last_label = true;
	  last_stmt = false;
	  c_parser_label (parser);
	}
      else if (!last_label
	       && c_parser_next_token_starts_declspecs (parser))
	{
	  last_label = false;
	  c_parser_declaration_or_fndef (parser, true, true, true, true);
	  if (last_stmt
	      && ((pedantic && !flag_isoc99)
		  || warn_declaration_after_statement))
	    pedwarn_c90 ("%HISO C90 forbids mixed declarations and code",
			 &loc);
	  last_stmt = false;
	}
      else if (!last_label
	       && c_parser_next_token_is_keyword (parser, RID_EXTENSION))
	{
	  /* __extension__ can start a declaration, but is also an
	     unary operator that can start an expression.  Consume all
	     but the last of a possible series of __extension__ to
	     determine which.  */
	  while (c_parser_peek_2nd_token (parser)->type == CPP_KEYWORD
		 && (c_parser_peek_2nd_token (parser)->keyword
		     == RID_EXTENSION))
	    c_parser_consume_token (parser);
	  if (c_token_starts_declspecs (c_parser_peek_2nd_token (parser)))
	    {
	      int ext;
	      ext = disable_extension_diagnostics ();
	      c_parser_consume_token (parser);
	      last_label = false;
	      c_parser_declaration_or_fndef (parser, true, true, true, true);
	      /* Following the old parser, __extension__ does not
		 disable this diagnostic.  */
	      restore_extension_diagnostics (ext);
	      if (last_stmt
		  && ((pedantic && !flag_isoc99)
		      || warn_declaration_after_statement))
		pedwarn_c90 ("%HISO C90 forbids mixed declarations and code",
			     &loc);
	      last_stmt = false;
	    }
	  else
	    goto statement;
	}
      else
	{
	statement:
	  last_label = false;
	  last_stmt = true;
	  c_parser_statement_after_labels (parser);
	}
    }
  if (last_label)
    error ("label at end of compound statement");
  c_parser_consume_token (parser);
}

/* Parse a label (C90 6.6.1, C99 6.8.1).

   label:
     identifier : attributes[opt]
     case constant-expression :
     default :

   GNU extensions:

   label:
     case constant-expression ... constant-expression :

   The use of attributes on labels is a GNU extension.  The syntax in
   GNU C accepts any expressions without commas, non-constant
   expressions being rejected later.  */

static void
c_parser_label (c_parser *parser)
{
  location_t loc1 = c_parser_peek_token (parser)->location;
  tree label = NULL_TREE;
  if (c_parser_next_token_is_keyword (parser, RID_CASE))
    {
      tree exp1, exp2;
      c_parser_consume_token (parser);
      exp1 = c_parser_expr_no_commas (parser, NULL).value;
      if (c_parser_next_token_is (parser, CPP_COLON))
	{
	  c_parser_consume_token (parser);
	  label = do_case (exp1, NULL_TREE);
	}
      else if (c_parser_next_token_is (parser, CPP_ELLIPSIS))
	{
	  c_parser_consume_token (parser);
	  exp2 = c_parser_expr_no_commas (parser, NULL).value;
	  if (c_parser_require (parser, CPP_COLON, "expected %<:%>"))
	    label = do_case (exp1, exp2);
	}
      else
	c_parser_error (parser, "expected %<:%> or %<...%>");
    }
  else if (c_parser_next_token_is_keyword (parser, RID_DEFAULT))
    {
      c_parser_consume_token (parser);
      if (c_parser_require (parser, CPP_COLON, "expected %<:%>"))
	label = do_case (NULL_TREE, NULL_TREE);
    }
  else
    {
      tree name = c_parser_peek_token (parser)->value;
      tree tlab;
      location_t loc2;
      tree attrs;
      gcc_assert (c_parser_next_token_is (parser, CPP_NAME));
      c_parser_consume_token (parser);
      gcc_assert (c_parser_next_token_is (parser, CPP_COLON));
      loc2 = c_parser_peek_token (parser)->location;
      c_parser_consume_token (parser);
      attrs = c_parser_attributes (parser);
      tlab = define_label (loc2, name);
      if (tlab)
	{
	  decl_attributes (&tlab, attrs, 0);
	  label = add_stmt (build_stmt (LABEL_EXPR, tlab));
	}
    }
  if (label)
    SET_EXPR_LOCATION (label, loc1);
}

/* Parse a statement (C90 6.6, C99 6.8).

   statement:
     labeled-statement
     compound-statement
     expression-statement
     selection-statement
     iteration-statement
     jump-statement

   labeled-statement:
     label statement

   expression-statement:
     expression[opt] ;

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
     asm-statement

   jump-statement:
     goto * expression ;

   Objective-C:

   statement:
     objc-throw-statement
     objc-try-catch-statement
     objc-synchronized-statement

   objc-throw-statement:
     @throw expression ;
     @throw ;
*/

static void
c_parser_statement (c_parser *parser)
{
  while (c_parser_next_token_is_keyword (parser, RID_CASE)
	 || c_parser_next_token_is_keyword (parser, RID_DEFAULT)
	 || (c_parser_next_token_is (parser, CPP_NAME)
	     && c_parser_peek_2nd_token (parser)->type == CPP_COLON))
    c_parser_label (parser);
  c_parser_statement_after_labels (parser);
}

/* Parse a statement, other than a labeled statement.  */

static void
c_parser_statement_after_labels (c_parser *parser)
{
  location_t loc = c_parser_peek_token (parser)->location;
  tree stmt = NULL_TREE;
  switch (c_parser_peek_token (parser)->type)
    {
    case CPP_OPEN_BRACE:
      add_stmt (c_parser_compound_statement (parser));
      break;
    case CPP_KEYWORD:
      switch (c_parser_peek_token (parser)->keyword)
	{
	case RID_IF:
	  c_parser_if_statement (parser);
	  break;
	case RID_SWITCH:
	  c_parser_switch_statement (parser);
	  break;
	case RID_WHILE:
	  c_parser_while_statement (parser);
	  break;
	case RID_DO:
	  c_parser_do_statement (parser);
	  break;
	case RID_FOR:
	  c_parser_for_statement (parser);
	  break;
	case RID_GOTO:
	  c_parser_consume_token (parser);
	  if (c_parser_next_token_is (parser, CPP_NAME))
	    {
	      stmt = c_finish_goto_label (c_parser_peek_token (parser)->value);
	      c_parser_consume_token (parser);
	    }
	  else if (c_parser_next_token_is (parser, CPP_MULT))
	    {
	      c_parser_consume_token (parser);
	      stmt = c_finish_goto_ptr (c_parser_expression (parser).value);
	    }
	  else
	    c_parser_error (parser, "expected identifier or %<*%>");
	  goto expect_semicolon;
	case RID_CONTINUE:
	  c_parser_consume_token (parser);
	  stmt = c_finish_bc_stmt (&c_cont_label, false);
	  goto expect_semicolon;
	case RID_BREAK:
	  c_parser_consume_token (parser);
	  stmt = c_finish_bc_stmt (&c_break_label, true);
	  goto expect_semicolon;
	case RID_RETURN:
	  c_parser_consume_token (parser);
	  if (c_parser_next_token_is (parser, CPP_SEMICOLON))
	    {
	      stmt = c_finish_return (NULL_TREE);
	      c_parser_consume_token (parser);
	    }
	  else
	    {
	      stmt = c_finish_return (c_parser_expression (parser).value);
	      goto expect_semicolon;
	    }
	  break;
	case RID_ASM:
	  stmt = c_parser_asm_statement (parser);
	  break;
	case RID_AT_THROW:
	  gcc_assert (c_dialect_objc ());
	  c_parser_consume_token (parser);
	  if (c_parser_next_token_is (parser, CPP_SEMICOLON))
	    {
	      stmt = objc_build_throw_stmt (NULL_TREE);
	      c_parser_consume_token (parser);
	    }
	  else
	    {
	      stmt
		= objc_build_throw_stmt (c_parser_expression (parser).value);
	      goto expect_semicolon;
	    }
	  break;
	case RID_AT_TRY:
	  gcc_assert (c_dialect_objc ());
	  c_parser_objc_try_catch_statement (parser);
	  break;
	case RID_AT_SYNCHRONIZED:
	  gcc_assert (c_dialect_objc ());
	  c_parser_objc_synchronized_statement (parser);
	  break;
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
    default:
    expr_stmt:
      stmt = c_finish_expr_stmt (c_parser_expression (parser).value);
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
  if (stmt && EXPR_P (stmt))
    SET_EXPR_LOCATION (stmt, loc);
}

/* Parse a parenthesized condition from an if, do or while statement.

   condition:
     ( expression )
*/
static tree
c_parser_paren_condition (c_parser *parser)
{
  location_t loc;
  tree cond;
  if (!c_parser_require (parser, CPP_OPEN_PAREN, "expected %<(%>"))
    return error_mark_node;
  loc = c_parser_peek_token (parser)->location;
  cond = c_objc_common_truthvalue_conversion
    (c_parser_expression (parser).value);
  if (EXPR_P (cond))
    SET_EXPR_LOCATION (cond, loc);
  c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, "expected %<)%>");
  return cond;
}

/* Parse a statement which is a block in C99.  */

static tree
c_parser_c99_block_statement (c_parser *parser)
{
  tree block = c_begin_compound_stmt (flag_isoc99);
  c_parser_statement (parser);
  return c_end_compound_stmt (block, flag_isoc99);
}

/* Parse the body of an if statement or the else half thereof.  This
   is just parsing a statement but (a) it is a block in C99, (b) we
   track whether the body is an if statement for the sake of
   -Wparentheses warnings, (c) we handle an empty body specially for
   the sake of -Wextra warnings.  */

static tree
c_parser_if_body (c_parser *parser, bool *if_p)
{
  tree block = c_begin_compound_stmt (flag_isoc99);
  while (c_parser_next_token_is_keyword (parser, RID_CASE)
	 || c_parser_next_token_is_keyword (parser, RID_DEFAULT)
	 || (c_parser_next_token_is (parser, CPP_NAME)
	     && c_parser_peek_2nd_token (parser)->type == CPP_COLON))
    c_parser_label (parser);
  *if_p = c_parser_next_token_is_keyword (parser, RID_IF);
  if (extra_warnings && c_parser_next_token_is (parser, CPP_SEMICOLON))
    add_stmt (build (NOP_EXPR, NULL_TREE, NULL_TREE));
  c_parser_statement_after_labels (parser);
  return c_end_compound_stmt (block, flag_isoc99);
}

/* Parse an if statement (C90 6.6.4, C99 6.8.4).

   if-statement:
     if ( expression ) statement
     if ( expression ) statement else statement
*/

static void
c_parser_if_statement (c_parser *parser)
{
  tree block;
  location_t loc;
  tree cond;
  bool first_if = false, second_if = false;
  tree first_body, second_body;
  gcc_assert (c_parser_next_token_is_keyword (parser, RID_IF));
  c_parser_consume_token (parser);
  block = c_begin_compound_stmt (flag_isoc99);
  loc = c_parser_peek_token (parser)->location;
  cond = c_parser_paren_condition (parser);
  first_body = c_parser_if_body (parser, &first_if);
  if (c_parser_next_token_is_keyword (parser, RID_ELSE))
    {
      c_parser_consume_token (parser);
      second_body = c_parser_if_body (parser, &second_if);
    }
  else
    second_body = NULL_TREE;
  c_finish_if_stmt (loc, cond, first_body, second_body, first_if);
  add_stmt (c_end_compound_stmt (block, flag_isoc99));
}

/* Parse a switch statement (C90 6.6.4, C99 6.8.4).

   switch-statement:
     switch (expression) statement
*/

static void
c_parser_switch_statement (c_parser *parser)
{
  tree block, expr, body, save_break;
  gcc_assert (c_parser_next_token_is_keyword (parser, RID_SWITCH));
  c_parser_consume_token (parser);
  block = c_begin_compound_stmt (flag_isoc99);
  if (c_parser_require (parser, CPP_OPEN_PAREN, "expected %<(%>"))
    {
      expr = c_parser_expression (parser).value;
      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, "expected %<)%>");
    }
  else
    expr = error_mark_node;
  c_start_case (expr);
  save_break = c_break_label;
  c_break_label = NULL_TREE;
  body = c_parser_c99_block_statement (parser);
  c_finish_case (body);
  if (c_break_label)
    add_stmt (build (LABEL_EXPR, void_type_node, c_break_label));
  c_break_label = save_break;
  add_stmt (c_end_compound_stmt (block, flag_isoc99));
}

/* Parse a while statement (C90 6.6.5, C99 6.8.5).

   while-statement:
      while (expression) statement
*/

static void
c_parser_while_statement (c_parser *parser)
{
  tree block, cond, body, save_break, save_cont;
  location_t loc;
  gcc_assert (c_parser_next_token_is_keyword (parser, RID_WHILE));
  c_parser_consume_token (parser);
  block = c_begin_compound_stmt (flag_isoc99);
  loc = c_parser_peek_token (parser)->location;
  cond = c_parser_paren_condition (parser);
  save_break = c_break_label;
  c_break_label = NULL_TREE;
  save_cont = c_cont_label;
  c_cont_label = NULL_TREE;
  body = c_parser_c99_block_statement (parser);
  c_finish_loop (loc, cond, NULL, body, c_break_label, c_cont_label, true);
  add_stmt (c_end_compound_stmt (block, flag_isoc99));
  c_break_label = save_break;
  c_cont_label = save_cont;
}

/* Parse a do statement (C90 6.6.5, C99 6.8.5).

   do-statement:
     do statement while ( expression ) ;
*/

static void
c_parser_do_statement (c_parser *parser)
{
  tree block, cond, body, save_break, save_cont, new_break, new_cont;
  location_t loc;
  gcc_assert (c_parser_next_token_is_keyword (parser, RID_DO));
  c_parser_consume_token (parser);
  block = c_begin_compound_stmt (flag_isoc99);
  loc = c_parser_peek_token (parser)->location;
  save_break = c_break_label;
  c_break_label = NULL_TREE;
  save_cont = c_cont_label;
  c_cont_label = NULL_TREE;
  body = c_parser_c99_block_statement (parser);
  c_parser_require_keyword (parser, RID_WHILE, "expected %<while%>");
  new_break = c_break_label;
  c_break_label = save_break;
  new_cont = c_cont_label;
  c_cont_label = save_cont;
  cond = c_parser_paren_condition (parser);
  if (!c_parser_require (parser, CPP_SEMICOLON, "expected %<;%>"))
    c_parser_skip_to_end_of_block_or_statement (parser);
  c_finish_loop (loc, cond, NULL, body, new_break, new_cont, false);
  add_stmt (c_end_compound_stmt (block, flag_isoc99));
}

/* Parse a for statement (C90 6.6.5, C99 6.8.5).

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
   check_for_loop_decls?  */

static void
c_parser_for_statement (c_parser *parser)
{
  tree block, cond, incr, save_break, save_cont, body;
  location_t loc = UNKNOWN_LOCATION;
  gcc_assert (c_parser_next_token_is_keyword (parser, RID_FOR));
  c_parser_consume_token (parser);
  block = c_begin_compound_stmt (flag_isoc99);
  if (c_parser_require (parser, CPP_OPEN_PAREN, "expected %<(%>"))
    {
      /* Parse the initialization declaration or expression.  */
      if (c_parser_next_token_is (parser, CPP_SEMICOLON))
	{
	  c_parser_consume_token (parser);
	  c_finish_expr_stmt (NULL_TREE);
	}
      else if (c_parser_next_token_starts_declspecs (parser))
	{
	  c_parser_declaration_or_fndef (parser, true, true, true, true);
	  check_for_loop_decls ();
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
	  if (c_token_starts_declspecs (c_parser_peek_2nd_token (parser)))
	    {
	      int ext;
	      ext = disable_extension_diagnostics ();
	      c_parser_consume_token (parser);
	      c_parser_declaration_or_fndef (parser, true, true, true, true);
	      restore_extension_diagnostics (ext);
	      check_for_loop_decls ();
	    }
	  else
	    goto init_expr;
	}
      else
	{
	init_expr:
	  c_finish_expr_stmt (c_parser_expression (parser).value);
	  c_parser_skip_until_found (parser, CPP_SEMICOLON, "expected %<;%>");
	}
      /* Parse the loop condition.  */
      loc = c_parser_peek_token (parser)->location;
      if (c_parser_next_token_is (parser, CPP_SEMICOLON))
	{
	  c_parser_consume_token (parser);
	  cond = NULL_TREE;
	}
      else
	{
	  tree ocond = c_parser_expression (parser).value;
	  cond = c_objc_common_truthvalue_conversion (ocond);
	  if (EXPR_P (cond))
	    SET_EXPR_LOCATION (cond, loc);
	  c_parser_skip_until_found (parser, CPP_SEMICOLON, "expected %<;%>");
	}
      /* Parse the increment expression.  */
      if (c_parser_next_token_is (parser, CPP_CLOSE_PAREN))
	incr = c_process_expr_stmt (NULL_TREE);
      else
	incr = c_process_expr_stmt (c_parser_expression (parser).value);
      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, "expected %<)%>");
    }
  else
    {
      cond = error_mark_node;
      incr = error_mark_node;
    }
  save_break = c_break_label;
  c_break_label = NULL_TREE;
  save_cont = c_cont_label;
  c_cont_label = NULL_TREE;
  body = c_parser_c99_block_statement (parser);
  c_finish_loop (loc, cond, incr, body, c_break_label, c_cont_label, true);
  add_stmt (c_end_compound_stmt (block, flag_isoc99));
  c_break_label = save_break;
  c_cont_label = save_cont;
}

/* Parse an asm statement, a GNU extension.  This is a full-blown asm
   statement with inputs, outputs, clobbers, and volatile tag
   allowed.

   asm-statement:
     asm type-qualifier[opt] ( asm-argument ) ;

   asm-argument:
     asm-string-literal
     asm-string-literal : asm-operands[opt]
     asm-string-literal : asm-operands[opt] : asm-operands[opt]
     asm-string-literal : asm-operands[opt] : asm-operands[opt] : asm-clobbers

   Qualifiers other than volatile are accepted in the syntax but
   warned for.  */

static tree
c_parser_asm_statement (c_parser *parser)
{
  tree quals, str, outputs, inputs, clobbers, ret;
  bool simple;
  gcc_assert (c_parser_next_token_is_keyword (parser, RID_ASM));
  c_parser_consume_token (parser);
  if (c_parser_next_token_is_keyword (parser, RID_VOLATILE))
    {
      quals = c_parser_peek_token (parser)->value;
      c_parser_consume_token (parser);
    }
  else if (c_parser_next_token_is_keyword (parser, RID_CONST)
	   || c_parser_next_token_is_keyword (parser, RID_RESTRICT))
    {
      warning (0, "%E qualifier ignored on asm",
	       c_parser_peek_token (parser)->value);
      quals = NULL_TREE;
      c_parser_consume_token (parser);
    }
  else
    quals = NULL_TREE;
  /* ??? Follow the C++ parser rather than using the
     c_lex_string_translate kludge.  */
  c_lex_string_translate = 0;
  if (!c_parser_require (parser, CPP_OPEN_PAREN, "expected %<(%>"))
    {
      c_lex_string_translate = 1;
      return NULL_TREE;
    }
  str = c_parser_asm_string_literal (parser);
  if (c_parser_next_token_is (parser, CPP_CLOSE_PAREN))
    {
      simple = true;
      outputs = NULL_TREE;
      inputs = NULL_TREE;
      clobbers = NULL_TREE;
      goto done_asm;
    }
  if (!c_parser_require (parser, CPP_COLON, "expected %<:%> or %<)%>"))
    {
      c_lex_string_translate = 1;
      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, NULL);
      return NULL_TREE;
    }
  simple = false;
  /* Parse outputs.  */
  if (c_parser_next_token_is (parser, CPP_COLON)
      || c_parser_next_token_is (parser, CPP_CLOSE_PAREN))
    outputs = NULL_TREE;
  else
    outputs = c_parser_asm_operands (parser);
  if (c_parser_next_token_is (parser, CPP_CLOSE_PAREN))
    {
      inputs = NULL_TREE;
      clobbers = NULL_TREE;
      goto done_asm;
    }
  if (!c_parser_require (parser, CPP_COLON, "expected %<:%> or %<)%>"))
    {
      c_lex_string_translate = 1;
      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, NULL);
      return NULL_TREE;
    }
  /* Parse inputs.  */
  if (c_parser_next_token_is (parser, CPP_COLON)
      || c_parser_next_token_is (parser, CPP_CLOSE_PAREN))
    inputs = NULL_TREE;
  else
    inputs = c_parser_asm_operands (parser);
  if (c_parser_next_token_is (parser, CPP_CLOSE_PAREN))
    {
      clobbers = NULL_TREE;
      goto done_asm;
    }
  if (!c_parser_require (parser, CPP_COLON, "expected %<:%> or %<)%>"))
    {
      c_lex_string_translate = 1;
      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, NULL);
      return NULL_TREE;
    }
  /* Parse clobbers.  */
  clobbers = c_parser_asm_clobbers (parser);
 done_asm:
  c_lex_string_translate = 1;
  if (!c_parser_require (parser, CPP_CLOSE_PAREN, "expected %<)%>"))
    {
      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, NULL);
      return NULL_TREE;
    }
  if (!c_parser_require (parser, CPP_SEMICOLON, "expected %<;%>"))
    c_parser_skip_to_end_of_block_or_statement (parser);
  ret = build_asm_stmt (quals, build_asm_expr (str, outputs, inputs,
					       clobbers, simple));
  return ret;
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
      tree name, str, expr;
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
      c_lex_string_translate = 1;
      if (!c_parser_require (parser, CPP_OPEN_PAREN, "expected %<(%>"))
	{
	  c_lex_string_translate = 0;
	  return NULL_TREE;
	}
      expr = c_parser_expression (parser).value;
      c_lex_string_translate = 0;
      if (!c_parser_require (parser, CPP_CLOSE_PAREN, "expected %<)%>"))
	{
	  c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, NULL);
	  return NULL_TREE;
	}
      list = chainon (list, build_tree_list (build_tree_list (name, str),
					     expr));
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

/* Parse an expression other than a compound expression; that is, an
   assignment expression (C90 6.3.16, C99 6.5.16).  If AFTER is not
   NULL then it is an Objective-C message expression which is the
   primary-expression starting the expression as an initializer.

   assignment-expression:
     conditional-expression
     unary-expression assignment-operator assignment-expression

   assignment-operator: one of
     = *= /= %= += -= <<= >>= &= ^= |=

   In GNU C we accept any conditional expression on the LHS and
   diagnose the invalid lvalue rather than producing a syntax
   error.  */

static struct c_expr
c_parser_expr_no_commas (c_parser *parser, struct c_expr *after)
{
  struct c_expr lhs, rhs, ret;
  enum tree_code code;
  gcc_assert (!after || c_dialect_objc ());
  lhs = c_parser_conditional_expression (parser, after);
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
      return lhs;
    }
  c_parser_consume_token (parser);
  rhs = c_parser_expr_no_commas (parser, NULL);
  ret.value = build_modify_expr (lhs.value, code, rhs.value);
  if (code == NOP_EXPR)
    ret.original_code = MODIFY_EXPR;
  else
    {
      TREE_NO_WARNING (ret.value) = 1;
      ret.original_code = ERROR_MARK;
    }
  return ret;
}

/* Parse a conditional expression (C90 6.3.15, C99 6.5.15).  If AFTER
   is not NULL then it is an Objective-C message expression which is
   the primary-expression starting the expression as an initializer.

   conditional-expression:
     logical-OR-expression
     logical-OR-expression ? expression : conditional-expression

   GNU extensions:

   conditional-expression:
     logical-OR-expression ? : conditional-expression
*/

static struct c_expr
c_parser_conditional_expression (c_parser *parser, struct c_expr *after)
{
  struct c_expr cond, exp1, exp2, ret;
  gcc_assert (!after || c_dialect_objc ());
  cond = c_parser_binary_expression (parser, after);
  if (c_parser_next_token_is_not (parser, CPP_QUERY))
    return cond;
  c_parser_consume_token (parser);
  if (c_parser_next_token_is (parser, CPP_COLON))
    {
      if (pedantic)
	pedwarn ("ISO C forbids omitting the middle term of a ?: expression");
      /* Make sure first operand is calculated only once.  */
      exp1.value = save_expr (default_conversion (cond.value));
      cond.value = c_objc_common_truthvalue_conversion (exp1.value);
      skip_evaluation += cond.value == truthvalue_true_node;
    }
  else
    {
      cond.value
	= c_objc_common_truthvalue_conversion
	(default_conversion (cond.value));
      skip_evaluation += cond.value == truthvalue_false_node;
      exp1 = c_parser_expression (parser);
      skip_evaluation += ((cond.value == truthvalue_true_node)
			  - (cond.value == truthvalue_false_node));
    }
  if (!c_parser_require (parser, CPP_COLON, "expected %<:%>"))
    {
      skip_evaluation -= cond.value == truthvalue_true_node;
      ret.value = error_mark_node;
      ret.original_code = ERROR_MARK;
      return ret;
    }
  exp2 = c_parser_conditional_expression (parser, NULL);
  skip_evaluation -= cond.value == truthvalue_true_node;
  ret.value = build_conditional_expr (cond.value, exp1.value, exp2.value);
  ret.original_code = ERROR_MARK;
  return ret;
}

/* Parse a binary expression; that is, a logical-OR-expression (C90
   6.3.5-6.3.14, C99 6.5.5-6.5.14).  If AFTER is not NULL then it is
   an Objective-C message expression which is the primary-expression
   starting the expression as an initializer.

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
c_parser_binary_expression (c_parser *parser, struct c_expr *after)
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
     expressions, we also need to adjust skip_evaluation as
     appropriate when the operators are pushed and popped.  */

  /* The precedence levels, where 0 is a dummy lowest level used for
     the bottom of the stack.  */
  enum prec {
    PREC_NONE,
    PREC_LOGOR,
    PREC_LOGAND,
    PREC_BITOR,
    PREC_BITXOR,
    PREC_BITAND,
    PREC_EQ,
    PREC_REL,
    PREC_SHIFT,
    PREC_ADD,
    PREC_MULT,
    NUM_PRECS
  };
  struct {
    /* The expression at this stack level.  */
    struct c_expr expr;
    /* The precedence of the operator on its left, PREC_NONE at the
       bottom of the stack.  */
    enum prec prec;
    /* The operation on its left.  */
    enum tree_code op;
  } stack[NUM_PRECS];
  int sp;
#define POP								      \
  do {									      \
    switch (stack[sp].op)						      \
      {									      \
      case TRUTH_ANDIF_EXPR:						      \
	skip_evaluation -= stack[sp - 1].expr.value == truthvalue_false_node; \
	break;								      \
      case TRUTH_ORIF_EXPR:						      \
	skip_evaluation -= stack[sp - 1].expr.value == truthvalue_true_node;  \
	break;								      \
      default:								      \
	break;								      \
      }									      \
    stack[sp - 1].expr = parser_build_binary_op (stack[sp].op,		      \
						 stack[sp - 1].expr,	      \
						 stack[sp].expr);	      \
    sp--;								      \
  } while (0)
  gcc_assert (!after || c_dialect_objc ());
  stack[0].expr = c_parser_cast_expression (parser, after);
  stack[0].prec = PREC_NONE;
  sp = 0;
  while (true)
    {
      enum prec oprec;
      enum tree_code ocode;
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
      c_parser_consume_token (parser);
      while (oprec <= stack[sp].prec)
	POP;
      switch (ocode)
	{
	case TRUTH_ANDIF_EXPR:
	  stack[sp].expr.value = c_objc_common_truthvalue_conversion
	    (default_conversion (stack[sp].expr.value));
	  skip_evaluation += stack[sp].expr.value == truthvalue_false_node;
	  break;
	case TRUTH_ORIF_EXPR:
	  stack[sp].expr.value = c_objc_common_truthvalue_conversion
	    (default_conversion (stack[sp].expr.value));
	  skip_evaluation += stack[sp].expr.value == truthvalue_true_node;
	  break;
	default:
	  break;
	}
      sp++;
      stack[sp].expr = c_parser_cast_expression (parser, NULL);
      stack[sp].prec = oprec;
      stack[sp].op = ocode;
    }
 out:
  while (sp > 0)
    POP;
  return stack[0].expr;
#undef POP
}

/* Parse a cast expression (C90 6.3.4, C99 6.5.4).  If AFTER is not
   NULL then it is an Objective-C message expression which is the
   primary-expression starting the expression as an initializer.

   cast-expression:
     unary-expression
     ( type-name ) unary-expression
*/

static struct c_expr
c_parser_cast_expression (c_parser *parser, struct c_expr *after)
{
  gcc_assert (!after || c_dialect_objc ());
  if (after)
    return c_parser_postfix_expression_after_primary (parser, *after);
  /* If the expression begins with a parenthesized type name, it may
     be either a cast or a compound literal; we need to see whether
     the next character is '{' to tell the difference.  If not, it is
     an unary expression.  */
  if (c_parser_next_token_is (parser, CPP_OPEN_PAREN)
      && c_token_starts_typename (c_parser_peek_2nd_token (parser)))
    {
      struct c_type_name *type_name;
      struct c_expr ret;
      tree expr;
      c_parser_consume_token (parser);
      type_name = c_parser_type_name (parser);
      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, "expected %<)%>");
      if (type_name == NULL)
	{
	  ret.value = error_mark_node;
	  ret.original_code = ERROR_MARK;
	  return ret;
	}
      if (c_parser_next_token_is (parser, CPP_OPEN_BRACE))
	return c_parser_postfix_expression_after_paren_type (parser,
							     type_name);
      expr = c_parser_cast_expression (parser, NULL).value;
      ret.value = c_cast_expr (type_name, expr);
      ret.original_code = ERROR_MARK;
      return ret;
    }
  else
    return c_parser_unary_expression (parser);
}

/* Parse an unary expression (C90 6.3.3, C99 6.5.3).

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

   unary-operator: one of
     __extension__ __real__ __imag__

   In addition, the GNU syntax treats ++ and -- as unary operators, so
   they may be applied to cast expressions with errors for non-lvalues
   given later.  */

static struct c_expr
c_parser_unary_expression (c_parser *parser)
{
  int ext;
  struct c_expr ret;
  switch (c_parser_peek_token (parser)->type)
    {
    case CPP_PLUS_PLUS:
      c_parser_consume_token (parser);
      return parser_build_unary_op (PREINCREMENT_EXPR,
				    c_parser_cast_expression (parser, NULL));
    case CPP_MINUS_MINUS:
      c_parser_consume_token (parser);
      return parser_build_unary_op (PREDECREMENT_EXPR,
				    c_parser_cast_expression (parser, NULL));
    case CPP_AND:
      c_parser_consume_token (parser);
      return parser_build_unary_op (ADDR_EXPR,
				    c_parser_cast_expression (parser, NULL));
    case CPP_MULT:
      c_parser_consume_token (parser);
      ret.value
	= build_indirect_ref (c_parser_cast_expression (parser, NULL).value,
			      "unary *");
      ret.original_code = ERROR_MARK;
      return ret;
    case CPP_PLUS:
      c_parser_consume_token (parser);
      if (!c_dialect_objc () && warn_traditional && !in_system_header)
	warning (0, "traditional C rejects the unary plus operator");
      return parser_build_unary_op (CONVERT_EXPR,
				    c_parser_cast_expression (parser, NULL));
    case CPP_MINUS:
      c_parser_consume_token (parser);
      return parser_build_unary_op (NEGATE_EXPR,
				    c_parser_cast_expression (parser, NULL));
    case CPP_COMPL:
      c_parser_consume_token (parser);
      return parser_build_unary_op (BIT_NOT_EXPR,
				    c_parser_cast_expression (parser, NULL));
    case CPP_NOT:
      c_parser_consume_token (parser);
      return parser_build_unary_op (TRUTH_NOT_EXPR,
				    c_parser_cast_expression (parser, NULL));
    case CPP_AND_AND:
      /* Refer to the address of a label as a pointer.  */
      c_parser_consume_token (parser);
      if (c_parser_next_token_is (parser, CPP_NAME))
	{
	  ret.value = finish_label_address_expr
	    (c_parser_peek_token (parser)->value);
	  c_parser_consume_token (parser);
	}
      else
	{
	  c_parser_error (parser, "expected identifier");
	  ret.value = error_mark_node;
	}
	ret.original_code = ERROR_MARK;
	return ret;
    case CPP_KEYWORD:
      switch (c_parser_peek_token (parser)->keyword)
	{
	case RID_SIZEOF:
	  return c_parser_sizeof_expression (parser);
	case RID_ALIGNOF:
	  return c_parser_alignof_expression (parser);
	case RID_EXTENSION:
	  c_parser_consume_token (parser);
	  ext = disable_extension_diagnostics ();
	  ret = c_parser_cast_expression (parser, NULL);
	  restore_extension_diagnostics (ext);
	  return ret;
	case RID_REALPART:
	  c_parser_consume_token (parser);
	  return parser_build_unary_op (REALPART_EXPR,
					c_parser_cast_expression (parser,
								  NULL));
	case RID_IMAGPART:
	  c_parser_consume_token (parser);
	  return parser_build_unary_op (IMAGPART_EXPR,
					c_parser_cast_expression (parser,
								  NULL));
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
  gcc_assert (c_parser_next_token_is_keyword (parser, RID_SIZEOF));
  c_parser_consume_token (parser);
  skip_evaluation++;
  in_sizeof++;
  if (c_parser_next_token_is (parser, CPP_OPEN_PAREN)
      && c_token_starts_typename (c_parser_peek_2nd_token (parser)))
    {
      /* Either sizeof ( type-name ) or sizeof unary-expression
	 starting with a compound literal.  */
      struct c_type_name *type_name;
      c_parser_consume_token (parser);
      type_name = c_parser_type_name (parser);
      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, "expected %<)%>");
      if (type_name == NULL)
	{
	  struct c_expr ret;
	  skip_evaluation--;
	  in_sizeof--;
	  ret.value = error_mark_node;
	  ret.original_code = ERROR_MARK;
	  return ret;
	}
      if (c_parser_next_token_is (parser, CPP_OPEN_BRACE))
	{
	  expr = c_parser_postfix_expression_after_paren_type (parser,
							       type_name);
	  goto sizeof_expr;
	}
      /* sizeof ( type-name ).  */
      skip_evaluation--;
      in_sizeof--;
      return c_expr_sizeof_type (type_name);
    }
  else
    {
      expr = c_parser_unary_expression (parser);
    sizeof_expr:
      skip_evaluation--;
      in_sizeof--;
      if (TREE_CODE (expr.value) == COMPONENT_REF
	  && DECL_C_BIT_FIELD (TREE_OPERAND (expr.value, 1)))
	error ("%<sizeof%> applied to a bit-field");
      return c_expr_sizeof_expr (expr);
    }
}

/* Parse an alignof expression.  */

static struct c_expr
c_parser_alignof_expression (c_parser *parser)
{
  struct c_expr expr;
  gcc_assert (c_parser_next_token_is_keyword (parser, RID_ALIGNOF));
  c_parser_consume_token (parser);
  skip_evaluation++;
  in_alignof++;
  if (c_parser_next_token_is (parser, CPP_OPEN_PAREN)
      && c_token_starts_typename (c_parser_peek_2nd_token (parser)))
    {
      /* Either __alignof__ ( type-name ) or __alignof__
	 unary-expression starting with a compound literal.  */
      struct c_type_name *type_name;
      struct c_expr ret;
      c_parser_consume_token (parser);
      type_name = c_parser_type_name (parser);
      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, "expected %<)%>");
      if (type_name == NULL)
	{
	  struct c_expr ret;
	  skip_evaluation--;
	  in_alignof--;
	  ret.value = error_mark_node;
	  ret.original_code = ERROR_MARK;
	  return ret;
	}
      if (c_parser_next_token_is (parser, CPP_OPEN_BRACE))
	{
	  expr = c_parser_postfix_expression_after_paren_type (parser,
							       type_name);
	  goto alignof_expr;
	}
      /* alignof ( type-name ).  */
      skip_evaluation--;
      in_alignof--;
      ret.value = c_alignof (groktypename (type_name));
      ret.original_code = ERROR_MARK;
      return ret;
    }
  else
    {
      struct c_expr ret;
      expr = c_parser_unary_expression (parser);
    alignof_expr:
      skip_evaluation--;
      in_alignof--;
      ret.value = c_alignof_expr (expr.value);
      ret.original_code = ERROR_MARK;
      return ret;
    }
}

/* Parse a postfix expression (C90 6.3.1-6.3.2, C99 6.5.1-6.5.2).

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
*/

static struct c_expr
c_parser_postfix_expression (c_parser *parser)
{
  struct c_expr expr, e1, e2, e3;
  struct c_type_name *t1, *t2;
  switch (c_parser_peek_token (parser)->type)
    {
    case CPP_NUMBER:
    case CPP_CHAR:
    case CPP_WCHAR:
      expr.value = c_parser_peek_token (parser)->value;
      expr.original_code = ERROR_MARK;
      c_parser_consume_token (parser);
      break;
    case CPP_STRING:
    case CPP_WSTRING:
      expr.value = c_parser_peek_token (parser)->value;
      expr.original_code = STRING_CST;
      c_parser_consume_token (parser);
      break;
    case CPP_OBJC_STRING:
      gcc_assert (c_dialect_objc ());
      expr.value
	= objc_build_string_object (c_parser_peek_token (parser)->value);
      expr.original_code = ERROR_MARK;
      c_parser_consume_token (parser);
      break;
    case CPP_NAME:
      if (c_parser_peek_token (parser)->id_kind != C_ID_ID)
	{
	  c_parser_error (parser, "expected expression");
	  expr.value = error_mark_node;
	  expr.original_code = ERROR_MARK;
	  break;
	}
      {
	tree id = c_parser_peek_token (parser)->value;
	location_t loc = c_parser_peek_token (parser)->location;
	c_parser_consume_token (parser);
	expr.value = build_external_ref (id,
					 (c_parser_peek_token (parser)->type
					  == CPP_OPEN_PAREN), loc);
	expr.original_code = ERROR_MARK;
      }
      break;
    case CPP_OPEN_PAREN:
      /* A parenthesized expression, statement expression or compound
	 literal.  */
      if (c_parser_peek_2nd_token (parser)->type == CPP_OPEN_BRACE)
	{
	  /* A statement expression.  */
	  tree stmt;
	  c_parser_consume_token (parser);
	  c_parser_consume_token (parser);
	  if (cur_stmt_list == NULL)
	    {
	      error ("braced-group within expression allowed "
		     "only inside a function");
	      parser->error = true;
	      c_parser_skip_until_found (parser, CPP_CLOSE_BRACE, NULL);
	      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, NULL);
	      expr.value = error_mark_node;
	      expr.original_code = ERROR_MARK;
	      break;
	    }
	  stmt = c_begin_stmt_expr ();
	  c_parser_compound_statement_nostart (parser);
	  c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
				     "expected %<)%>");
	  if (pedantic)
	    pedwarn ("ISO C forbids braced-groups within expressions");
	  expr.value = c_finish_stmt_expr (stmt);
	  expr.original_code = ERROR_MARK;
	}
      else if (c_token_starts_typename (c_parser_peek_2nd_token (parser)))
	{
	  /* A compound literal.  ??? Can we actually get here rather
	     than going directly to
	     c_parser_postfix_expression_after_paren_type from
	     elsewhere?  */
	  struct c_type_name *type_name;
	  c_parser_consume_token (parser);
	  type_name = c_parser_type_name (parser);
	  c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
				     "expected %<)%>");
	  if (type_name == NULL)
	    {
	      expr.value = error_mark_node;
	      expr.original_code = ERROR_MARK;
	    }
	  else
	    expr = c_parser_postfix_expression_after_paren_type (parser,
								 type_name);
	}
      else
	{
	  /* A parenthesized expression.  */
	  c_parser_consume_token (parser);
	  expr = c_parser_expression (parser);
	  if (TREE_CODE (expr.value) == MODIFY_EXPR)
	    TREE_NO_WARNING (expr.value) = 1;
	  expr.original_code = ERROR_MARK;
	  c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
				     "expected %<)%>");
	}
      break;
    case CPP_KEYWORD:
      switch (c_parser_peek_token (parser)->keyword)
	{
	case RID_FUNCTION_NAME:
	case RID_PRETTY_FUNCTION_NAME:
	case RID_C99_FUNCTION_NAME:
	  expr.value = fname_decl (c_parser_peek_token (parser)->keyword,
				   c_parser_peek_token (parser)->value);
	  expr.original_code = ERROR_MARK;
	  c_parser_consume_token (parser);
	  break;
	case RID_VA_ARG:
	  c_parser_consume_token (parser);
	  if (!c_parser_require (parser, CPP_OPEN_PAREN, "expected %<(%>"))
	    {
	      expr.value = error_mark_node;
	      expr.original_code = ERROR_MARK;
	      break;
	    }
	  e1 = c_parser_expr_no_commas (parser, NULL);
	  if (!c_parser_require (parser, CPP_COMMA, "expected %<,%>"))
	    {
	      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, NULL);
	      expr.value = error_mark_node;
	      expr.original_code = ERROR_MARK;
	      break;
	    }
	  t1 = c_parser_type_name (parser);
	  c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
				     "expected %<)%>");
	  if (t1 == NULL)
	    {
	      expr.value = error_mark_node;
	      expr.original_code = ERROR_MARK;
	    }
	  else
	    {
	      expr.value = build_va_arg (e1.value, groktypename (t1));
	      expr.original_code = ERROR_MARK;
	    }
	  break;
	case RID_OFFSETOF:
	  c_parser_consume_token (parser);
	  if (!c_parser_require (parser, CPP_OPEN_PAREN, "expected %<(%>"))
	    {
	      expr.value = error_mark_node;
	      expr.original_code = ERROR_MARK;
	      break;
	    }
	  t1 = c_parser_type_name (parser);
	  if (t1 == NULL)
	    {
	      expr.value = error_mark_node;
	      expr.original_code = ERROR_MARK;
	      break;
	    }
	  if (!c_parser_require (parser, CPP_COMMA, "expected %<,%>"))
	    {
	      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, NULL);
	      expr.value = error_mark_node;
	      expr.original_code = ERROR_MARK;
	      break;
	    }
	  {
	    tree type = groktypename (t1);
	    tree offsetof_ref;
	    if (type == error_mark_node)
	      offsetof_ref = error_mark_node;
	    else
	      offsetof_ref = build1 (INDIRECT_REF, type, NULL);
	    /* Parse the second argument to __builtin_offsetof.  We
	       must have one identifier, and beyond that we want to
	       accept sub structure and sub array references.  */
	    if (c_parser_next_token_is (parser, CPP_NAME))
	      {
		offsetof_ref = build_component_ref
		  (offsetof_ref, c_parser_peek_token (parser)->value);
		c_parser_consume_token (parser);
		while (c_parser_next_token_is (parser, CPP_DOT)
		       || c_parser_next_token_is (parser,
						  CPP_OPEN_SQUARE))
		  {
		    if (c_parser_next_token_is (parser, CPP_DOT))
		      {
			c_parser_consume_token (parser);
			if (c_parser_next_token_is_not (parser,
							CPP_NAME))
			  {
			    c_parser_error (parser, "expected identifier");
			    break;
			  }
			offsetof_ref = build_component_ref
			  (offsetof_ref,
			   c_parser_peek_token (parser)->value);
			c_parser_consume_token (parser);
		      }
		    else
		      {
			tree idx;
			c_parser_consume_token (parser);
			idx = c_parser_expression (parser).value;
			c_parser_skip_until_found (parser, CPP_CLOSE_SQUARE,
						   "expected %<]%>");
			offsetof_ref = build_array_ref (offsetof_ref, idx);
		      }
		  }
	      }
	    else
	      c_parser_error (parser, "expected identifier");
	    c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
				       "expected %<)%>");
	    expr.value = fold_offsetof (offsetof_ref);
	    expr.original_code = ERROR_MARK;
	  }
	  break;
	case RID_CHOOSE_EXPR:
	  c_parser_consume_token (parser);
	  if (!c_parser_require (parser, CPP_OPEN_PAREN, "expected %<(%>"))
	    {
	      expr.value = error_mark_node;
	      expr.original_code = ERROR_MARK;
	      break;
	    }
	  e1 = c_parser_expr_no_commas (parser, NULL);
	  if (!c_parser_require (parser, CPP_COMMA, "expected %<,%>"))
	    {
	      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, NULL);
	      expr.value = error_mark_node;
	      expr.original_code = ERROR_MARK;
	      break;
	    }
	  e2 = c_parser_expr_no_commas (parser, NULL);
	  if (!c_parser_require (parser, CPP_COMMA, "expected %<,%>"))
	    {
	      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, NULL);
	      expr.value = error_mark_node;
	      expr.original_code = ERROR_MARK;
	      break;
	    }
	  e3 = c_parser_expr_no_commas (parser, NULL);
	  c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
				     "expected %<)%>");
	  {
	    tree c;

	    c = fold (e1.value);
	    if (TREE_CODE (c) != INTEGER_CST)
	      error ("first argument to %<__builtin_choose_expr%> not"
		     " a constant");
	    expr = integer_zerop (c) ? e3 : e2;
	  }
	  break;
	case RID_TYPES_COMPATIBLE_P:
	  c_parser_consume_token (parser);
	  if (!c_parser_require (parser, CPP_OPEN_PAREN, "expected %<(%>"))
	    {
	      expr.value = error_mark_node;
	      expr.original_code = ERROR_MARK;
	      break;
	    }
	  t1 = c_parser_type_name (parser);
	  if (t1 == NULL)
	    {
	      expr.value = error_mark_node;
	      expr.original_code = ERROR_MARK;
	      break;
	    }
	  if (!c_parser_require (parser, CPP_COMMA, "expected %<,%>"))
	    {
	      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, NULL);
	      expr.value = error_mark_node;
	      expr.original_code = ERROR_MARK;
	      break;
	    }
	  t2 = c_parser_type_name (parser);
	  if (t2 == NULL)
	    {
	      expr.value = error_mark_node;
	      expr.original_code = ERROR_MARK;
	      break;
	    }
	  c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
				     "expected %<)%>");
	  {
	    tree e1, e2;

	    e1 = TYPE_MAIN_VARIANT (groktypename (t1));
	    e2 = TYPE_MAIN_VARIANT (groktypename (t2));

	    expr.value = comptypes (e1, e2)
	      ? build_int_cst (NULL_TREE, 1)
	      : build_int_cst (NULL_TREE, 0);
	    expr.original_code = ERROR_MARK;
	  }
	  break;
	case RID_AT_SELECTOR:
	  gcc_assert (c_dialect_objc ());
	  c_parser_consume_token (parser);
	  if (!c_parser_require (parser, CPP_OPEN_PAREN, "expected %<(%>"))
	    {
	      expr.value = error_mark_node;
	      expr.original_code = ERROR_MARK;
	      break;
	    }
	  {
	    tree sel = c_parser_objc_selector_arg (parser);
	    c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
				       "expected %<)%>");
	    expr.value = objc_build_selector_expr (sel);
	    expr.original_code = ERROR_MARK;
	  }
	  break;
	case RID_AT_PROTOCOL:
	  gcc_assert (c_dialect_objc ());
	  c_parser_consume_token (parser);
	  if (!c_parser_require (parser, CPP_OPEN_PAREN, "expected %<(%>"))
	    {
	      expr.value = error_mark_node;
	      expr.original_code = ERROR_MARK;
	      break;
	    }
	  if (c_parser_next_token_is_not (parser, CPP_NAME))
	    {
	      c_parser_error (parser, "expected identifier");
	      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, NULL);
	      expr.value = error_mark_node;
	      expr.original_code = ERROR_MARK;
	      break;
	    }
	  {
	    tree id = c_parser_peek_token (parser)->value;
	    c_parser_consume_token (parser);
	    c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
				       "expected %<)%>");
	    expr.value = objc_build_protocol_expr (id);
	    expr.original_code = ERROR_MARK;
	  }
	  break;
	case RID_AT_ENCODE:
	  /* Extension to support C-structures in the archiver.  */
	  gcc_assert (c_dialect_objc ());
	  c_parser_consume_token (parser);
	  if (!c_parser_require (parser, CPP_OPEN_PAREN, "expected %<(%>"))
	    {
	      expr.value = error_mark_node;
	      expr.original_code = ERROR_MARK;
	      break;
	    }
	  t1 = c_parser_type_name (parser);
	  if (t1 == NULL)
	    {
	      expr.value = error_mark_node;
	      expr.original_code = ERROR_MARK;
	      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, NULL);
	      break;
	    }
	  c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
				     "expected %<)%>");
	  {
	    tree type = groktypename (t1);
	    expr.value = objc_build_encode_expr (type);
	    expr.original_code = ERROR_MARK;
	  }
	  break;
	default:
	  c_parser_error (parser, "expected expression");
	  expr.value = error_mark_node;
	  expr.original_code = ERROR_MARK;
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
	  c_parser_skip_until_found (parser, CPP_CLOSE_SQUARE,
				     "expected %<]%>");
	  expr.value = objc_build_message_expr (build_tree_list (receiver,
								 args));
	  expr.original_code = ERROR_MARK;
	  break;
	}
      /* Else fall through to report error.  */
    default:
      c_parser_error (parser, "expected expression");
      expr.value = error_mark_node;
      expr.original_code = ERROR_MARK;
      break;
    }
  return c_parser_postfix_expression_after_primary (parser, expr);
}

/* Parse a postfix expression after a parenthesized type name: the
   brace-enclosed initializer of a compound literal, possibly followed
   by some postfix operators.  This is separate because it is not
   possible to tell until after the type name whether a cast
   expression has a cast or a compound literal, or whether the operand
   of sizeof is a parenthesized type name or starts with a compound
   literal.  */

static struct c_expr
c_parser_postfix_expression_after_paren_type (c_parser *parser,
					      struct c_type_name *type_name)
{
  tree type;
  struct c_expr init;
  struct c_expr expr;
  start_init (NULL_TREE, NULL, 0);
  type = groktypename (type_name);
  if (C_TYPE_VARIABLE_SIZE (type))
    {
      error ("compound literal has variable size");
      type = error_mark_node;
    }
  init = c_parser_braced_init (parser, type, false);
  finish_init ();
  maybe_warn_string_init (type, init);

  if (pedantic && !flag_isoc99)
    pedwarn ("ISO C90 forbids compound literals");
  expr.value = build_compound_literal (type, init.value);
  expr.original_code = ERROR_MARK;
  return c_parser_postfix_expression_after_primary (parser, expr);
}

/* Parse a postfix expression after the initial primary or compound
   literal; that is, parse a series of postfix operators.  */

static struct c_expr
c_parser_postfix_expression_after_primary (c_parser *parser,
					   struct c_expr expr)
{
  tree ident, idx, exprlist;
  while (true)
    {
      switch (c_parser_peek_token (parser)->type)
	{
	case CPP_OPEN_SQUARE:
	  /* Array reference.  */
	  c_parser_consume_token (parser);
	  idx = c_parser_expression (parser).value;
	  c_parser_skip_until_found (parser, CPP_CLOSE_SQUARE,
				     "expected %<]%>");
	  expr.value = build_array_ref (expr.value, idx);
	  expr.original_code = ERROR_MARK;
	  break;
	case CPP_OPEN_PAREN:
	  /* Function call.  */
	  c_parser_consume_token (parser);
	  if (c_parser_next_token_is (parser, CPP_CLOSE_PAREN))
	    exprlist = NULL_TREE;
	  else
	    exprlist = c_parser_expr_list (parser);
	  c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
				     "expected %<)%>");
	  expr.value = build_function_call (expr.value, exprlist);
	  expr.original_code = ERROR_MARK;
	  break;
	case CPP_DOT:
	  /* Structure element reference.  */
	  c_parser_consume_token (parser);
	  if (c_parser_next_token_is (parser, CPP_NAME))
	    ident = c_parser_peek_token (parser)->value;
	  else
	    {
	      c_parser_error (parser, "expected identifier");
	      expr.value = error_mark_node;
	      expr.original_code = ERROR_MARK;
	      return expr;
	    }
	  c_parser_consume_token (parser);
	  expr.value = build_component_ref (expr.value, ident);
	  expr.original_code = ERROR_MARK;
	  break;
	case CPP_DEREF:
	  /* Structure element reference.  */
	  c_parser_consume_token (parser);
	  if (c_parser_next_token_is (parser, CPP_NAME))
	    ident = c_parser_peek_token (parser)->value;
	  else
	    {
	      c_parser_error (parser, "expected identifier");
	      expr.value = error_mark_node;
	      expr.original_code = ERROR_MARK;
	      return expr;
	    }
	  c_parser_consume_token (parser);
	  expr.value = build_component_ref (build_indirect_ref (expr.value,
								"->"), ident);
	  expr.original_code = ERROR_MARK;
	  break;
	case CPP_PLUS_PLUS:
	  /* Postincrement.  */
	  c_parser_consume_token (parser);
	  expr.value = build_unary_op (POSTINCREMENT_EXPR, expr.value, 0);
	  expr.original_code = ERROR_MARK;
	  break;
	case CPP_MINUS_MINUS:
	  /* Postdecrement.  */
	  c_parser_consume_token (parser);
	  expr.value = build_unary_op (POSTDECREMENT_EXPR, expr.value, 0);
	  expr.original_code = ERROR_MARK;
	  break;
	default:
	  return expr;
	}
    }
}

/* Parse an expression (C90 6.3.17, C99 6.5.17).

   expression:
     assignment-expression
     expression , assignment-expression
*/

static struct c_expr
c_parser_expression (c_parser *parser)
{
  struct c_expr expr;
  expr = c_parser_expr_no_commas (parser, NULL);
  while (c_parser_next_token_is (parser, CPP_COMMA))
    {
      struct c_expr next;
      c_parser_consume_token (parser);
      next = c_parser_expr_no_commas (parser, NULL);
      expr.value = build_compound_expr (expr.value, next.value);
      expr.original_code = COMPOUND_EXPR;
    }
  return expr;
}

/* Parse a non-empty list of expressions.

   nonempty-expr-list:
     assignment-expression
     nonempty-expr-list , assignment-expression
*/

static tree
c_parser_expr_list (c_parser *parser)
{
  struct c_expr expr;
  tree ret;
  expr = c_parser_expr_no_commas (parser, NULL);
  ret = build_tree_list (NULL_TREE, expr.value);
  while (c_parser_next_token_is (parser, CPP_COMMA))
    {
      c_parser_consume_token (parser);
      expr = c_parser_expr_no_commas (parser, NULL);
      ret = chainon (ret, build_tree_list (NULL_TREE, expr.value));
    }
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
     @implementation identifier ( identifier )

   objc-superclass:
     : identifier

   "@interface identifier (" must start "@interface identifier (
   identifier ) ...": objc-methodprotolist in the first production may
   not start with a parenthesised identifier as a declarator of a data
   definition with no declaration specifiers if the objc-superclass,
   objc-protocol-refs and objc-class-instance-variables are omitted.  */

static void
c_parser_objc_class_definition (c_parser *parser)
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
  c_parser_consume_token (parser);
  if (c_parser_next_token_is (parser, CPP_OPEN_PAREN))
    {
      tree id2;
      tree proto = NULL_TREE;
      c_parser_consume_token (parser);
      if (c_parser_next_token_is_not (parser, CPP_NAME))
	{
	  c_parser_error (parser, "expected identifier");
	  c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, NULL);
	  return;
	}
      id2 = c_parser_peek_token (parser)->value;
      c_parser_consume_token (parser);
      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, "expected %<)%>");
      if (!iface_p)
	{
	  objc_start_category_implementation (id1, id2);
	  return;
	}
      if (c_parser_next_token_is (parser, CPP_LESS))
	proto = c_parser_objc_protocol_refs (parser);
      objc_start_category_interface (id1, id2, proto);
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
      objc_start_class_interface (id1, superclass, proto);
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
	  if (pedantic)
	    pedwarn ("extra semicolon in struct or union specified");
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
	  objc_set_visibility (2);
	  continue;
	}
      else if (c_parser_next_token_is_keyword (parser, RID_AT_PROTECTED))
	{
	  c_parser_consume_token (parser);
	  objc_set_visibility (0);
	  continue;
	}
      else if (c_parser_next_token_is_keyword (parser, RID_AT_PUBLIC))
	{
	  c_parser_consume_token (parser);
	  objc_set_visibility (1);
	  continue;
	}
      /* Parse some comma-separated declarations.  */
      decls = c_parser_struct_declaration (parser);
      {
	/* Comma-separated instance variables are chained together in
	   reverse order; add them one by one.  */
	tree ivar = nreverse (decls);
	for (; ivar; ivar = TREE_CHAIN (ivar))
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
  tree list = NULL_TREE;
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
  c_parser_skip_until_found (parser, CPP_SEMICOLON, "expected %<;%>");
  objc_declare_class (list);
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
c_parser_objc_protocol_definition (c_parser *parser)
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
      tree list = NULL_TREE;
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
	  list = chainon (list, build_tree_list (NULL_TREE, id));
	  c_parser_consume_token (parser);
	  if (c_parser_next_token_is (parser, CPP_COMMA))
	    c_parser_consume_token (parser);
	  else
	    break;
	}
      c_parser_skip_until_found (parser, CPP_SEMICOLON, "expected %<;%>");
      objc_declare_protocols (list);
    }
  else
    {
      tree id = c_parser_peek_token (parser)->value;
      tree proto = NULL_TREE;
      c_parser_consume_token (parser);
      if (c_parser_next_token_is (parser, CPP_LESS))
	proto = c_parser_objc_protocol_refs (parser);
      objc_pq_context = 1;
      objc_start_protocol (id, proto);
      c_parser_objc_methodprotolist (parser);
      c_parser_require_keyword (parser, RID_AT_END, "expected %<@end%>");
      objc_pq_context = 0;
      objc_finish_interface ();
    }
}

/* Parse an objc-method-type.

   objc-method-type:
     +
     -
*/

static enum tree_code
c_parser_objc_method_type (c_parser *parser)
{
  switch (c_parser_peek_token (parser)->type)
    {
    case CPP_PLUS:
      c_parser_consume_token (parser);
      return PLUS_EXPR;
    case CPP_MINUS:
      c_parser_consume_token (parser);
      return MINUS_EXPR;
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
  enum tree_code type = c_parser_objc_method_type (parser);
  tree decl;
  objc_set_method_type (type);
  objc_pq_context = 1;
  decl = c_parser_objc_method_decl (parser);
  if (c_parser_next_token_is (parser, CPP_SEMICOLON))
    {
      c_parser_consume_token (parser);
      if (pedantic)
	pedwarn ("extra semicolon in method definition specified");
    }
  objc_pq_context = 0;
  objc_start_method_definition (decl);
  add_stmt (c_parser_compound_statement (parser));
  objc_finish_method_definition (current_function_decl);
}

/* Parse an objc-methodprotolist.

   objc-methodprotolist:
     empty
     objc-methodprotolist objc-methodproto
     objc-methodprotolist declaration
     objc-methodprotolist ;

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
	  if (pedantic)
	    pedwarn ("ISO C does not allow extra %<;%> outside of a function");
	  c_parser_consume_token (parser);
	  break;
	case CPP_PLUS:
	case CPP_MINUS:
	  c_parser_objc_methodproto (parser);
	  break;
	case CPP_EOF:
	  return;
	default:
	  if (c_parser_next_token_is_keyword (parser, RID_AT_END))
	    return;
	  c_parser_declaration_or_fndef (parser, false, true, false, true);
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
  enum tree_code type = c_parser_objc_method_type (parser);
  tree decl;
  objc_set_method_type (type);
  /* Remember protocol qualifiers in prototypes.  */
  objc_pq_context = 1;
  decl = c_parser_objc_method_decl (parser);
  /* Forget protocol qualifiers here.  */
  objc_pq_context = 0;
  objc_add_method_declaration (decl);
  c_parser_skip_until_found (parser, CPP_SEMICOLON, "expected %<;%>");
}

/* Parse an objc-method-decl.

   objc-method-decl:
     ( objc-type-name ) objc-selector
     objc-selector
     ( objc-type-name ) objc-keyword-selector objc-optparmlist
     objc-keyword-selector objc-optparmlist

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
c_parser_objc_method_decl (c_parser *parser)
{
  tree type = NULL_TREE;
  tree sel;
  tree parms = NULL_TREE;
  bool ellipsis = false;

  if (c_parser_next_token_is (parser, CPP_OPEN_PAREN))
    {
      c_parser_consume_token (parser);
      type = c_parser_objc_type_name (parser);
      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, "expected %<)%>");
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
	  if (!c_parser_require (parser, CPP_COLON, "expected %<:%>"))
	    break;
	  if (c_parser_next_token_is (parser, CPP_OPEN_PAREN))
	    {
	      c_parser_consume_token (parser);
	      atype = c_parser_objc_type_name (parser);
	      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN,
					 "expected %<)%>");
	    }
	  if (c_parser_next_token_is_not (parser, CPP_NAME))
	    {
	      c_parser_error (parser, "expected identifier");
	      return error_mark_node;
	    }
	  id = c_parser_peek_token (parser)->value;
	  c_parser_consume_token (parser);
	  keyworddecl = objc_build_keyword_decl (tsel, atype, id);
	  list = chainon (list, keyworddecl);
	  tsel = c_parser_objc_selector (parser);
	  if (!tsel && c_parser_next_token_is_not (parser, CPP_COLON))
	    break;
	}
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
	      break;
	    }
	  parm = c_parser_parameter_declaration (parser, NULL_TREE);
	  if (parm == NULL)
	    break;
	  parms = chainon (parms,
			   build_tree_list (NULL_TREE, grokparm (parm)));
	}
      sel = list;
    }
  return objc_build_method_signature (type, sel, parms, ellipsis);
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
  struct c_type_name *typename = NULL;
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
	  quals = chainon (quals, build_tree_list (NULL_TREE, token->value));
	  c_parser_consume_token (parser);
	}
      else
	break;
    }
  if (c_parser_next_token_starts_typename (parser))
    typename = c_parser_type_name (parser);
  if (typename)
    type = groktypename (typename);
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

/* Parse an objc-try-catch-statement.

   objc-try-catch-statement:
     @try compound-statement objc-catch-list[opt]
     @try compound-statement objc-catch-list[opt] @finally compound-statement

   objc-catch-list:
     @catch ( parameter-declaration ) compound-statement
     objc-catch-list @catch ( parameter-declaration ) compound-statement
*/

static void
c_parser_objc_try_catch_statement (c_parser *parser)
{
  location_t loc;
  tree stmt;
  gcc_assert (c_parser_next_token_is_keyword (parser, RID_AT_TRY));
  c_parser_consume_token (parser);
  loc = c_parser_peek_token (parser)->location;
  stmt = c_parser_compound_statement (parser);
  objc_begin_try_stmt (loc, stmt);
  while (c_parser_next_token_is_keyword (parser, RID_AT_CATCH))
    {
      struct c_parm *parm;
      c_parser_consume_token (parser);
      if (!c_parser_require (parser, CPP_OPEN_PAREN, "expected %<(%>"))
	break;
      parm = c_parser_parameter_declaration (parser, NULL_TREE);
      if (parm == NULL)
	{
	  c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, NULL);
	  break;
	}
      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, "expected %<)%>");
      objc_begin_catch_clause (grokparm (parm));
      if (c_parser_require (parser, CPP_OPEN_BRACE, "expected %<{%>"))
	c_parser_compound_statement_nostart (parser);
      objc_finish_catch_clause ();
    }
  if (c_parser_next_token_is_keyword (parser, RID_AT_FINALLY))
    {
      location_t finloc;
      tree finstmt;
      c_parser_consume_token (parser);
      finloc = c_parser_peek_token (parser)->location;
      finstmt = c_parser_compound_statement (parser);
      objc_build_finally_clause (finloc, finstmt);
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
  if (c_parser_require (parser, CPP_OPEN_PAREN, "expected %<(%>"))
    {
      expr = c_parser_expression (parser).value;
      c_parser_skip_until_found (parser, CPP_CLOSE_PAREN, "expected %<)%>");
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
    case RID_VOID:
    case RID_BOOL:
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
  if (sel && c_parser_next_token_is_not (parser, CPP_COLON))
    return sel;
  while (true)
    {
      if (!c_parser_require (parser, CPP_COLON, "expected %<:%>"))
	return list;
      list = chainon (list, build_tree_list (sel, NULL_TREE));
      sel = c_parser_objc_selector (parser);
      if (!sel && c_parser_next_token_is_not (parser, CPP_COLON))
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
  if (c_parser_peek_token (parser)->type == CPP_NAME
      && (c_parser_peek_token (parser)->id_kind == C_ID_TYPENAME
	  || c_parser_peek_token (parser)->id_kind == C_ID_CLASSNAME))
    {
      tree id = c_parser_peek_token (parser)->value;
      c_parser_consume_token (parser);
      return objc_get_class_reference (id);
    }
  return c_parser_expression (parser).value;
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
	return list;
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
  tree list = c_parser_expr_list (parser);
  if (TREE_CHAIN (list) == NULL_TREE)
    {
      /* Just return the expression, remove a level of
	 indirection.  */
      return TREE_VALUE (list);
    }
  else
    {
      /* We have a comma expression, we will collapse later.  */
      return list;
    }
}


/* The actual parser and external interface.  ??? Does this need to be
   garbage-collected?  */

static GTY (()) c_parser *the_parser;

/* Parse a single source file.  */

void
c_parse_file (void)
{
  the_parser = c_parser_new ();
  c_parser_translation_unit (the_parser);
  the_parser = NULL;
}

#include "gt-c-parser.h"
