/* C++ Parser.
   Copyright (C) 2000, 2001, 2002, 2003, 2004,
   2005, 2007, 2008, 2009  Free Software Foundation, Inc.
   Written by Mark Mitchell <mark@codesourcery.com>.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "dyn-string.h"
#include "varray.h"
#include "cpplib.h"
#include "tree.h"
#include "cp-tree.h"
#include "intl.h"
#include "c-pragma.h"
#include "decl.h"
#include "flags.h"
#include "diagnostic.h"
#include "toplev.h"
#include "output.h"
#include "target.h"
#include "cgraph.h"
#include "c-common.h"
#include "plugin.h"


/* The lexer.  */

/* The cp_lexer_* routines mediate between the lexer proper (in libcpp
   and c-lex.c) and the C++ parser.  */

/* A token's value and its associated deferred access checks and
   qualifying scope.  */

struct GTY(()) tree_check {
  /* The value associated with the token.  */
  tree value;
  /* The checks that have been associated with value.  */
  VEC (deferred_access_check, gc)* checks;
  /* The token's qualifying scope (used when it is a
     CPP_NESTED_NAME_SPECIFIER).  */
  tree qualifying_scope;
};

/* A C++ token.  */

typedef struct GTY (()) cp_token {
  /* The kind of token.  */
  ENUM_BITFIELD (cpp_ttype) type : 8;
  /* If this token is a keyword, this value indicates which keyword.
     Otherwise, this value is RID_MAX.  */
  ENUM_BITFIELD (rid) keyword : 8;
  /* Token flags.  */
  unsigned char flags;
  /* Identifier for the pragma.  */
  ENUM_BITFIELD (pragma_kind) pragma_kind : 6;
  /* True if this token is from a context where it is implicitly extern "C" */
  BOOL_BITFIELD implicit_extern_c : 1;
  /* True for a CPP_NAME token that is not a keyword (i.e., for which
     KEYWORD is RID_MAX) iff this name was looked up and found to be
     ambiguous.  An error has already been reported.  */
  BOOL_BITFIELD ambiguous_p : 1;
  /* The location at which this token was found.  */
  location_t location;
  /* The value associated with this token, if any.  */
  union cp_token_value {
    /* Used for CPP_NESTED_NAME_SPECIFIER and CPP_TEMPLATE_ID.  */
    struct tree_check* GTY((tag ("1"))) tree_check_value;
    /* Use for all other tokens.  */
    tree GTY((tag ("0"))) value;
  } GTY((desc ("(%1.type == CPP_TEMPLATE_ID) || (%1.type == CPP_NESTED_NAME_SPECIFIER)"))) u;
} cp_token;

/* We use a stack of token pointer for saving token sets.  */
typedef struct cp_token *cp_token_position;
DEF_VEC_P (cp_token_position);
DEF_VEC_ALLOC_P (cp_token_position,heap);

static cp_token eof_token =
{
  CPP_EOF, RID_MAX, 0, PRAGMA_NONE, false, 0, 0, { NULL }
};

/* The cp_lexer structure represents the C++ lexer.  It is responsible
   for managing the token stream from the preprocessor and supplying
   it to the parser.  Tokens are never added to the cp_lexer after
   it is created.  */

typedef struct GTY (()) cp_lexer {
  /* The memory allocated for the buffer.  NULL if this lexer does not
     own the token buffer.  */
  cp_token * GTY ((length ("%h.buffer_length"))) buffer;
  /* If the lexer owns the buffer, this is the number of tokens in the
     buffer.  */
  size_t buffer_length;

  /* A pointer just past the last available token.  The tokens
     in this lexer are [buffer, last_token).  */
  cp_token_position GTY ((skip)) last_token;

  /* The next available token.  If NEXT_TOKEN is &eof_token, then there are
     no more available tokens.  */
  cp_token_position GTY ((skip)) next_token;

  /* A stack indicating positions at which cp_lexer_save_tokens was
     called.  The top entry is the most recent position at which we
     began saving tokens.  If the stack is non-empty, we are saving
     tokens.  */
  VEC(cp_token_position,heap) *GTY ((skip)) saved_tokens;

  /* The next lexer in a linked list of lexers.  */
  struct cp_lexer *next;

  /* True if we should output debugging information.  */
  bool debugging_p;

  /* True if we're in the context of parsing a pragma, and should not
     increment past the end-of-line marker.  */
  bool in_pragma;
} cp_lexer;

/* cp_token_cache is a range of tokens.  There is no need to represent
   allocate heap memory for it, since tokens are never removed from the
   lexer's array.  There is also no need for the GC to walk through
   a cp_token_cache, since everything in here is referenced through
   a lexer.  */

typedef struct GTY(()) cp_token_cache {
  /* The beginning of the token range.  */
  cp_token * GTY((skip)) first;

  /* Points immediately after the last token in the range.  */
  cp_token * GTY ((skip)) last;
} cp_token_cache;

/* Prototypes.  */

static cp_lexer *cp_lexer_new_main
  (void);
static cp_lexer *cp_lexer_new_from_tokens
  (cp_token_cache *tokens);
static void cp_lexer_destroy
  (cp_lexer *);
static int cp_lexer_saving_tokens
  (const cp_lexer *);
static cp_token_position cp_lexer_token_position
  (cp_lexer *, bool);
static cp_token *cp_lexer_token_at
  (cp_lexer *, cp_token_position);
static void cp_lexer_get_preprocessor_token
  (cp_lexer *, cp_token *);
static inline cp_token *cp_lexer_peek_token
  (cp_lexer *);
static cp_token *cp_lexer_peek_nth_token
  (cp_lexer *, size_t);
static inline bool cp_lexer_next_token_is
  (cp_lexer *, enum cpp_ttype);
static bool cp_lexer_next_token_is_not
  (cp_lexer *, enum cpp_ttype);
static bool cp_lexer_next_token_is_keyword
  (cp_lexer *, enum rid);
static cp_token *cp_lexer_consume_token
  (cp_lexer *);
static void cp_lexer_purge_token
  (cp_lexer *);
static void cp_lexer_purge_tokens_after
  (cp_lexer *, cp_token_position);
static void cp_lexer_save_tokens
  (cp_lexer *);
static void cp_lexer_commit_tokens
  (cp_lexer *);
static void cp_lexer_rollback_tokens
  (cp_lexer *);
#ifdef ENABLE_CHECKING
static void cp_lexer_print_token
  (FILE *, cp_token *);
static inline bool cp_lexer_debugging_p
  (cp_lexer *);
static void cp_lexer_start_debugging
  (cp_lexer *) ATTRIBUTE_UNUSED;
static void cp_lexer_stop_debugging
  (cp_lexer *) ATTRIBUTE_UNUSED;
#else
/* If we define cp_lexer_debug_stream to NULL it will provoke warnings
   about passing NULL to functions that require non-NULL arguments
   (fputs, fprintf).  It will never be used, so all we need is a value
   of the right type that's guaranteed not to be NULL.  */
#define cp_lexer_debug_stream stdout
#define cp_lexer_print_token(str, tok) (void) 0
#define cp_lexer_debugging_p(lexer) 0
#endif /* ENABLE_CHECKING */

static cp_token_cache *cp_token_cache_new
  (cp_token *, cp_token *);

static void cp_parser_initial_pragma
  (cp_token *);

/* Manifest constants.  */
#define CP_LEXER_BUFFER_SIZE ((256 * 1024) / sizeof (cp_token))
#define CP_SAVED_TOKEN_STACK 5

/* A token type for keywords, as opposed to ordinary identifiers.  */
#define CPP_KEYWORD ((enum cpp_ttype) (N_TTYPES + 1))

/* A token type for template-ids.  If a template-id is processed while
   parsing tentatively, it is replaced with a CPP_TEMPLATE_ID token;
   the value of the CPP_TEMPLATE_ID is whatever was returned by
   cp_parser_template_id.  */
#define CPP_TEMPLATE_ID ((enum cpp_ttype) (CPP_KEYWORD + 1))

/* A token type for nested-name-specifiers.  If a
   nested-name-specifier is processed while parsing tentatively, it is
   replaced with a CPP_NESTED_NAME_SPECIFIER token; the value of the
   CPP_NESTED_NAME_SPECIFIER is whatever was returned by
   cp_parser_nested_name_specifier_opt.  */
#define CPP_NESTED_NAME_SPECIFIER ((enum cpp_ttype) (CPP_TEMPLATE_ID + 1))

/* A token type for tokens that are not tokens at all; these are used
   to represent slots in the array where there used to be a token
   that has now been deleted.  */
#define CPP_PURGED ((enum cpp_ttype) (CPP_NESTED_NAME_SPECIFIER + 1))

/* The number of token types, including C++-specific ones.  */
#define N_CP_TTYPES ((int) (CPP_PURGED + 1))

/* Variables.  */

#ifdef ENABLE_CHECKING
/* The stream to which debugging output should be written.  */
static FILE *cp_lexer_debug_stream;
#endif /* ENABLE_CHECKING */

/* Nonzero if we are parsing an unevaluated operand: an operand to
   sizeof, typeof, or alignof.  */
int cp_unevaluated_operand;

/* Create a new main C++ lexer, the lexer that gets tokens from the
   preprocessor.  */

static cp_lexer *
cp_lexer_new_main (void)
{
  cp_token first_token;
  cp_lexer *lexer;
  cp_token *pos;
  size_t alloc;
  size_t space;
  cp_token *buffer;

  /* It's possible that parsing the first pragma will load a PCH file,
     which is a GC collection point.  So we have to do that before
     allocating any memory.  */
  cp_parser_initial_pragma (&first_token);

  c_common_no_more_pch ();

  /* Allocate the memory.  */
  lexer = GGC_CNEW (cp_lexer);

#ifdef ENABLE_CHECKING
  /* Initially we are not debugging.  */
  lexer->debugging_p = false;
#endif /* ENABLE_CHECKING */
  lexer->saved_tokens = VEC_alloc (cp_token_position, heap,
				   CP_SAVED_TOKEN_STACK);

  /* Create the buffer.  */
  alloc = CP_LEXER_BUFFER_SIZE;
  buffer = GGC_NEWVEC (cp_token, alloc);

  /* Put the first token in the buffer.  */
  space = alloc;
  pos = buffer;
  *pos = first_token;

  /* Get the remaining tokens from the preprocessor.  */
  while (pos->type != CPP_EOF)
    {
      pos++;
      if (!--space)
	{
	  space = alloc;
	  alloc *= 2;
	  buffer = GGC_RESIZEVEC (cp_token, buffer, alloc);
	  pos = buffer + space;
	}
      cp_lexer_get_preprocessor_token (lexer, pos);
    }
  lexer->buffer = buffer;
  lexer->buffer_length = alloc - space;
  lexer->last_token = pos;
  lexer->next_token = lexer->buffer_length ? buffer : &eof_token;

  /* Subsequent preprocessor diagnostics should use compiler
     diagnostic functions to get the compiler source location.  */
  done_lexing = true;

  gcc_assert (lexer->next_token->type != CPP_PURGED);
  return lexer;
}

/* Create a new lexer whose token stream is primed with the tokens in
   CACHE.  When these tokens are exhausted, no new tokens will be read.  */

static cp_lexer *
cp_lexer_new_from_tokens (cp_token_cache *cache)
{
  cp_token *first = cache->first;
  cp_token *last = cache->last;
  cp_lexer *lexer = GGC_CNEW (cp_lexer);

  /* We do not own the buffer.  */
  lexer->buffer = NULL;
  lexer->buffer_length = 0;
  lexer->next_token = first == last ? &eof_token : first;
  lexer->last_token = last;

  lexer->saved_tokens = VEC_alloc (cp_token_position, heap,
				   CP_SAVED_TOKEN_STACK);

#ifdef ENABLE_CHECKING
  /* Initially we are not debugging.  */
  lexer->debugging_p = false;
#endif

  gcc_assert (lexer->next_token->type != CPP_PURGED);
  return lexer;
}

/* Frees all resources associated with LEXER.  */

static void
cp_lexer_destroy (cp_lexer *lexer)
{
  if (lexer->buffer)
    ggc_free (lexer->buffer);
  VEC_free (cp_token_position, heap, lexer->saved_tokens);
  ggc_free (lexer);
}

/* Returns nonzero if debugging information should be output.  */

#ifdef ENABLE_CHECKING

static inline bool
cp_lexer_debugging_p (cp_lexer *lexer)
{
  return lexer->debugging_p;
}

#endif /* ENABLE_CHECKING */

static inline cp_token_position
cp_lexer_token_position (cp_lexer *lexer, bool previous_p)
{
  gcc_assert (!previous_p || lexer->next_token != &eof_token);

  return lexer->next_token - previous_p;
}

static inline cp_token *
cp_lexer_token_at (cp_lexer *lexer ATTRIBUTE_UNUSED, cp_token_position pos)
{
  return pos;
}

/* nonzero if we are presently saving tokens.  */

static inline int
cp_lexer_saving_tokens (const cp_lexer* lexer)
{
  return VEC_length (cp_token_position, lexer->saved_tokens) != 0;
}

/* Store the next token from the preprocessor in *TOKEN.  Return true
   if we reach EOF.  If LEXER is NULL, assume we are handling an
   initial #pragma pch_preprocess, and thus want the lexer to return
   processed strings.  */

static void
cp_lexer_get_preprocessor_token (cp_lexer *lexer, cp_token *token)
{
  static int is_extern_c = 0;

   /* Get a new token from the preprocessor.  */
  token->type
    = c_lex_with_flags (&token->u.value, &token->location, &token->flags,
			lexer == NULL ? 0 : C_LEX_STRING_NO_JOIN);
  token->keyword = RID_MAX;
  token->pragma_kind = PRAGMA_NONE;

  /* On some systems, some header files are surrounded by an
     implicit extern "C" block.  Set a flag in the token if it
     comes from such a header.  */
  is_extern_c += pending_lang_change;
  pending_lang_change = 0;
  token->implicit_extern_c = is_extern_c > 0;

  /* Check to see if this token is a keyword.  */
  if (token->type == CPP_NAME)
    {
      if (C_IS_RESERVED_WORD (token->u.value))
	{
	  /* Mark this token as a keyword.  */
	  token->type = CPP_KEYWORD;
	  /* Record which keyword.  */
	  token->keyword = C_RID_CODE (token->u.value);
	}
      else
	{
          if (warn_cxx0x_compat
              && C_RID_CODE (token->u.value) >= RID_FIRST_CXX0X
              && C_RID_CODE (token->u.value) <= RID_LAST_CXX0X)
            {
              /* Warn about the C++0x keyword (but still treat it as
                 an identifier).  */
              warning (OPT_Wc__0x_compat, 
                       "identifier %qE will become a keyword in C++0x",
                       token->u.value);

              /* Clear out the C_RID_CODE so we don't warn about this
                 particular identifier-turned-keyword again.  */
              C_SET_RID_CODE (token->u.value, RID_MAX);
            }

	  token->ambiguous_p = false;
	  token->keyword = RID_MAX;
	}
    }
  /* Handle Objective-C++ keywords.  */
  else if (token->type == CPP_AT_NAME)
    {
      token->type = CPP_KEYWORD;
      switch (C_RID_CODE (token->u.value))
	{
	/* Map 'class' to '@class', 'private' to '@private', etc.  */
	case RID_CLASS: token->keyword = RID_AT_CLASS; break;
	case RID_PRIVATE: token->keyword = RID_AT_PRIVATE; break;
	case RID_PROTECTED: token->keyword = RID_AT_PROTECTED; break;
	case RID_PUBLIC: token->keyword = RID_AT_PUBLIC; break;
	case RID_THROW: token->keyword = RID_AT_THROW; break;
	case RID_TRY: token->keyword = RID_AT_TRY; break;
	case RID_CATCH: token->keyword = RID_AT_CATCH; break;
	default: token->keyword = C_RID_CODE (token->u.value);
	}
    }
  else if (token->type == CPP_PRAGMA)
    {
      /* We smuggled the cpp_token->u.pragma value in an INTEGER_CST.  */
      token->pragma_kind = ((enum pragma_kind)
			    TREE_INT_CST_LOW (token->u.value));
      token->u.value = NULL_TREE;
    }
}

/* Update the globals input_location and the input file stack from TOKEN.  */
static inline void
cp_lexer_set_source_position_from_token (cp_token *token)
{
  if (token->type != CPP_EOF)
    {
      input_location = token->location;
    }
}

/* Return a pointer to the next token in the token stream, but do not
   consume it.  */

static inline cp_token *
cp_lexer_peek_token (cp_lexer *lexer)
{
  if (cp_lexer_debugging_p (lexer))
    {
      fputs ("cp_lexer: peeking at token: ", cp_lexer_debug_stream);
      cp_lexer_print_token (cp_lexer_debug_stream, lexer->next_token);
      putc ('\n', cp_lexer_debug_stream);
    }
  return lexer->next_token;
}

/* Return true if the next token has the indicated TYPE.  */

static inline bool
cp_lexer_next_token_is (cp_lexer* lexer, enum cpp_ttype type)
{
  return cp_lexer_peek_token (lexer)->type == type;
}

/* Return true if the next token does not have the indicated TYPE.  */

static inline bool
cp_lexer_next_token_is_not (cp_lexer* lexer, enum cpp_ttype type)
{
  return !cp_lexer_next_token_is (lexer, type);
}

/* Return true if the next token is the indicated KEYWORD.  */

static inline bool
cp_lexer_next_token_is_keyword (cp_lexer* lexer, enum rid keyword)
{
  return cp_lexer_peek_token (lexer)->keyword == keyword;
}

/* Return true if the next token is not the indicated KEYWORD.  */

static inline bool
cp_lexer_next_token_is_not_keyword (cp_lexer* lexer, enum rid keyword)
{
  return cp_lexer_peek_token (lexer)->keyword != keyword;
}

/* Return true if the next token is a keyword for a decl-specifier.  */

static bool
cp_lexer_next_token_is_decl_specifier_keyword (cp_lexer *lexer)
{
  cp_token *token;

  token = cp_lexer_peek_token (lexer);
  switch (token->keyword) 
    {
      /* auto specifier: storage-class-specifier in C++,
         simple-type-specifier in C++0x.  */
    case RID_AUTO:
      /* Storage classes.  */
    case RID_REGISTER:
    case RID_STATIC:
    case RID_EXTERN:
    case RID_MUTABLE:
    case RID_THREAD:
      /* Elaborated type specifiers.  */
    case RID_ENUM:
    case RID_CLASS:
    case RID_STRUCT:
    case RID_UNION:
    case RID_TYPENAME:
      /* Simple type specifiers.  */
    case RID_CHAR:
    case RID_CHAR16:
    case RID_CHAR32:
    case RID_WCHAR:
    case RID_BOOL:
    case RID_SHORT:
    case RID_INT:
    case RID_LONG:
    case RID_SIGNED:
    case RID_UNSIGNED:
    case RID_FLOAT:
    case RID_DOUBLE:
    case RID_VOID:
      /* GNU extensions.  */ 
    case RID_ATTRIBUTE:
    case RID_TYPEOF:
      /* C++0x extensions.  */
    case RID_DECLTYPE:
      return true;

    default:
      return false;
    }
}

/* Return a pointer to the Nth token in the token stream.  If N is 1,
   then this is precisely equivalent to cp_lexer_peek_token (except
   that it is not inline).  One would like to disallow that case, but
   there is one case (cp_parser_nth_token_starts_template_id) where
   the caller passes a variable for N and it might be 1.  */

static cp_token *
cp_lexer_peek_nth_token (cp_lexer* lexer, size_t n)
{
  cp_token *token;

  /* N is 1-based, not zero-based.  */
  gcc_assert (n > 0);

  if (cp_lexer_debugging_p (lexer))
    fprintf (cp_lexer_debug_stream,
	     "cp_lexer: peeking ahead %ld at token: ", (long)n);

  --n;
  token = lexer->next_token;
  gcc_assert (!n || token != &eof_token);
  while (n != 0)
    {
      ++token;
      if (token == lexer->last_token)
	{
	  token = &eof_token;
	  break;
	}

      if (token->type != CPP_PURGED)
	--n;
    }

  if (cp_lexer_debugging_p (lexer))
    {
      cp_lexer_print_token (cp_lexer_debug_stream, token);
      putc ('\n', cp_lexer_debug_stream);
    }

  return token;
}

/* Return the next token, and advance the lexer's next_token pointer
   to point to the next non-purged token.  */

static cp_token *
cp_lexer_consume_token (cp_lexer* lexer)
{
  cp_token *token = lexer->next_token;

  gcc_assert (token != &eof_token);
  gcc_assert (!lexer->in_pragma || token->type != CPP_PRAGMA_EOL);

  do
    {
      lexer->next_token++;
      if (lexer->next_token == lexer->last_token)
	{
	  lexer->next_token = &eof_token;
	  break;
	}

    }
  while (lexer->next_token->type == CPP_PURGED);

  cp_lexer_set_source_position_from_token (token);

  /* Provide debugging output.  */
  if (cp_lexer_debugging_p (lexer))
    {
      fputs ("cp_lexer: consuming token: ", cp_lexer_debug_stream);
      cp_lexer_print_token (cp_lexer_debug_stream, token);
      putc ('\n', cp_lexer_debug_stream);
    }

  return token;
}

/* Permanently remove the next token from the token stream, and
   advance the next_token pointer to refer to the next non-purged
   token.  */

static void
cp_lexer_purge_token (cp_lexer *lexer)
{
  cp_token *tok = lexer->next_token;

  gcc_assert (tok != &eof_token);
  tok->type = CPP_PURGED;
  tok->location = UNKNOWN_LOCATION;
  tok->u.value = NULL_TREE;
  tok->keyword = RID_MAX;

  do
    {
      tok++;
      if (tok == lexer->last_token)
	{
	  tok = &eof_token;
	  break;
	}
    }
  while (tok->type == CPP_PURGED);
  lexer->next_token = tok;
}

/* Permanently remove all tokens after TOK, up to, but not
   including, the token that will be returned next by
   cp_lexer_peek_token.  */

static void
cp_lexer_purge_tokens_after (cp_lexer *lexer, cp_token *tok)
{
  cp_token *peek = lexer->next_token;

  if (peek == &eof_token)
    peek = lexer->last_token;

  gcc_assert (tok < peek);

  for ( tok += 1; tok != peek; tok += 1)
    {
      tok->type = CPP_PURGED;
      tok->location = UNKNOWN_LOCATION;
      tok->u.value = NULL_TREE;
      tok->keyword = RID_MAX;
    }
}

/* Begin saving tokens.  All tokens consumed after this point will be
   preserved.  */

static void
cp_lexer_save_tokens (cp_lexer* lexer)
{
  /* Provide debugging output.  */
  if (cp_lexer_debugging_p (lexer))
    fprintf (cp_lexer_debug_stream, "cp_lexer: saving tokens\n");

  VEC_safe_push (cp_token_position, heap,
		 lexer->saved_tokens, lexer->next_token);
}

/* Commit to the portion of the token stream most recently saved.  */

static void
cp_lexer_commit_tokens (cp_lexer* lexer)
{
  /* Provide debugging output.  */
  if (cp_lexer_debugging_p (lexer))
    fprintf (cp_lexer_debug_stream, "cp_lexer: committing tokens\n");

  VEC_pop (cp_token_position, lexer->saved_tokens);
}

/* Return all tokens saved since the last call to cp_lexer_save_tokens
   to the token stream.  Stop saving tokens.  */

static void
cp_lexer_rollback_tokens (cp_lexer* lexer)
{
  /* Provide debugging output.  */
  if (cp_lexer_debugging_p (lexer))
    fprintf (cp_lexer_debug_stream, "cp_lexer: restoring tokens\n");

  lexer->next_token = VEC_pop (cp_token_position, lexer->saved_tokens);
}

/* Print a representation of the TOKEN on the STREAM.  */

#ifdef ENABLE_CHECKING

static void
cp_lexer_print_token (FILE * stream, cp_token *token)
{
  /* We don't use cpp_type2name here because the parser defines
     a few tokens of its own.  */
  static const char *const token_names[] = {
    /* cpplib-defined token types */
#define OP(e, s) #e,
#define TK(e, s) #e,
    TTYPE_TABLE
#undef OP
#undef TK
    /* C++ parser token types - see "Manifest constants", above.  */
    "KEYWORD",
    "TEMPLATE_ID",
    "NESTED_NAME_SPECIFIER",
    "PURGED"
  };

  /* If we have a name for the token, print it out.  Otherwise, we
     simply give the numeric code.  */
  gcc_assert (token->type < ARRAY_SIZE(token_names));
  fputs (token_names[token->type], stream);

  /* For some tokens, print the associated data.  */
  switch (token->type)
    {
    case CPP_KEYWORD:
      /* Some keywords have a value that is not an IDENTIFIER_NODE.
	 For example, `struct' is mapped to an INTEGER_CST.  */
      if (TREE_CODE (token->u.value) != IDENTIFIER_NODE)
	break;
      /* else fall through */
    case CPP_NAME:
      fputs (IDENTIFIER_POINTER (token->u.value), stream);
      break;

    case CPP_STRING:
    case CPP_STRING16:
    case CPP_STRING32:
    case CPP_WSTRING:
    case CPP_UTF8STRING:
      fprintf (stream, " \"%s\"", TREE_STRING_POINTER (token->u.value));
      break;

    default:
      break;
    }
}

/* Start emitting debugging information.  */

static void
cp_lexer_start_debugging (cp_lexer* lexer)
{
  lexer->debugging_p = true;
}

/* Stop emitting debugging information.  */

static void
cp_lexer_stop_debugging (cp_lexer* lexer)
{
  lexer->debugging_p = false;
}

#endif /* ENABLE_CHECKING */

/* Create a new cp_token_cache, representing a range of tokens.  */

static cp_token_cache *
cp_token_cache_new (cp_token *first, cp_token *last)
{
  cp_token_cache *cache = GGC_NEW (cp_token_cache);
  cache->first = first;
  cache->last = last;
  return cache;
}


/* Decl-specifiers.  */

/* Set *DECL_SPECS to represent an empty decl-specifier-seq.  */

static void
clear_decl_specs (cp_decl_specifier_seq *decl_specs)
{
  memset (decl_specs, 0, sizeof (cp_decl_specifier_seq));
}

/* Declarators.  */

/* Nothing other than the parser should be creating declarators;
   declarators are a semi-syntactic representation of C++ entities.
   Other parts of the front end that need to create entities (like
   VAR_DECLs or FUNCTION_DECLs) should do that directly.  */

static cp_declarator *make_call_declarator
  (cp_declarator *, tree, cp_cv_quals, tree, tree);
static cp_declarator *make_array_declarator
  (cp_declarator *, tree);
static cp_declarator *make_pointer_declarator
  (cp_cv_quals, cp_declarator *);
static cp_declarator *make_reference_declarator
  (cp_cv_quals, cp_declarator *, bool);
static cp_parameter_declarator *make_parameter_declarator
  (cp_decl_specifier_seq *, cp_declarator *, tree);
static cp_declarator *make_ptrmem_declarator
  (cp_cv_quals, tree, cp_declarator *);

/* An erroneous declarator.  */
static cp_declarator *cp_error_declarator;

/* The obstack on which declarators and related data structures are
   allocated.  */
static struct obstack declarator_obstack;

/* Alloc BYTES from the declarator memory pool.  */

static inline void *
alloc_declarator (size_t bytes)
{
  return obstack_alloc (&declarator_obstack, bytes);
}

/* Allocate a declarator of the indicated KIND.  Clear fields that are
   common to all declarators.  */

static cp_declarator *
make_declarator (cp_declarator_kind kind)
{
  cp_declarator *declarator;

  declarator = (cp_declarator *) alloc_declarator (sizeof (cp_declarator));
  declarator->kind = kind;
  declarator->attributes = NULL_TREE;
  declarator->declarator = NULL;
  declarator->parameter_pack_p = false;

  return declarator;
}

/* Make a declarator for a generalized identifier.  If
   QUALIFYING_SCOPE is non-NULL, the identifier is
   QUALIFYING_SCOPE::UNQUALIFIED_NAME; otherwise, it is just
   UNQUALIFIED_NAME.  SFK indicates the kind of special function this
   is, if any.   */

static cp_declarator *
make_id_declarator (tree qualifying_scope, tree unqualified_name,
		    special_function_kind sfk)
{
  cp_declarator *declarator;

  /* It is valid to write:

       class C { void f(); };
       typedef C D;
       void D::f();

     The standard is not clear about whether `typedef const C D' is
     legal; as of 2002-09-15 the committee is considering that
     question.  EDG 3.0 allows that syntax.  Therefore, we do as
     well.  */
  if (qualifying_scope && TYPE_P (qualifying_scope))
    qualifying_scope = TYPE_MAIN_VARIANT (qualifying_scope);

  gcc_assert (TREE_CODE (unqualified_name) == IDENTIFIER_NODE
	      || TREE_CODE (unqualified_name) == BIT_NOT_EXPR
	      || TREE_CODE (unqualified_name) == TEMPLATE_ID_EXPR);

  declarator = make_declarator (cdk_id);
  declarator->u.id.qualifying_scope = qualifying_scope;
  declarator->u.id.unqualified_name = unqualified_name;
  declarator->u.id.sfk = sfk;
  
  return declarator;
}

/* Make a declarator for a pointer to TARGET.  CV_QUALIFIERS is a list
   of modifiers such as const or volatile to apply to the pointer
   type, represented as identifiers.  */

cp_declarator *
make_pointer_declarator (cp_cv_quals cv_qualifiers, cp_declarator *target)
{
  cp_declarator *declarator;

  declarator = make_declarator (cdk_pointer);
  declarator->declarator = target;
  declarator->u.pointer.qualifiers = cv_qualifiers;
  declarator->u.pointer.class_type = NULL_TREE;
  if (target)
    {
      declarator->parameter_pack_p = target->parameter_pack_p;
      target->parameter_pack_p = false;
    }
  else
    declarator->parameter_pack_p = false;

  return declarator;
}

/* Like make_pointer_declarator -- but for references.  */

cp_declarator *
make_reference_declarator (cp_cv_quals cv_qualifiers, cp_declarator *target,
			   bool rvalue_ref)
{
  cp_declarator *declarator;

  declarator = make_declarator (cdk_reference);
  declarator->declarator = target;
  declarator->u.reference.qualifiers = cv_qualifiers;
  declarator->u.reference.rvalue_ref = rvalue_ref;
  if (target)
    {
      declarator->parameter_pack_p = target->parameter_pack_p;
      target->parameter_pack_p = false;
    }
  else
    declarator->parameter_pack_p = false;

  return declarator;
}

/* Like make_pointer_declarator -- but for a pointer to a non-static
   member of CLASS_TYPE.  */

cp_declarator *
make_ptrmem_declarator (cp_cv_quals cv_qualifiers, tree class_type,
			cp_declarator *pointee)
{
  cp_declarator *declarator;

  declarator = make_declarator (cdk_ptrmem);
  declarator->declarator = pointee;
  declarator->u.pointer.qualifiers = cv_qualifiers;
  declarator->u.pointer.class_type = class_type;

  if (pointee)
    {
      declarator->parameter_pack_p = pointee->parameter_pack_p;
      pointee->parameter_pack_p = false;
    }
  else
    declarator->parameter_pack_p = false;

  return declarator;
}

/* Make a declarator for the function given by TARGET, with the
   indicated PARMS.  The CV_QUALIFIERS aply to the function, as in
   "const"-qualified member function.  The EXCEPTION_SPECIFICATION
   indicates what exceptions can be thrown.  */

cp_declarator *
make_call_declarator (cp_declarator *target,
		      tree parms,
		      cp_cv_quals cv_qualifiers,
		      tree exception_specification,
		      tree late_return_type)
{
  cp_declarator *declarator;

  declarator = make_declarator (cdk_function);
  declarator->declarator = target;
  declarator->u.function.parameters = parms;
  declarator->u.function.qualifiers = cv_qualifiers;
  declarator->u.function.exception_specification = exception_specification;
  declarator->u.function.late_return_type = late_return_type;
  if (target)
    {
      declarator->parameter_pack_p = target->parameter_pack_p;
      target->parameter_pack_p = false;
    }
  else
    declarator->parameter_pack_p = false;

  return declarator;
}

/* Make a declarator for an array of BOUNDS elements, each of which is
   defined by ELEMENT.  */

cp_declarator *
make_array_declarator (cp_declarator *element, tree bounds)
{
  cp_declarator *declarator;

  declarator = make_declarator (cdk_array);
  declarator->declarator = element;
  declarator->u.array.bounds = bounds;
  if (element)
    {
      declarator->parameter_pack_p = element->parameter_pack_p;
      element->parameter_pack_p = false;
    }
  else
    declarator->parameter_pack_p = false;

  return declarator;
}

/* Determine whether the declarator we've seen so far can be a
   parameter pack, when followed by an ellipsis.  */
static bool 
declarator_can_be_parameter_pack (cp_declarator *declarator)
{
  /* Search for a declarator name, or any other declarator that goes
     after the point where the ellipsis could appear in a parameter
     pack. If we find any of these, then this declarator can not be
     made into a parameter pack.  */
  bool found = false;
  while (declarator && !found)
    {
      switch ((int)declarator->kind)
	{
	case cdk_id:
	case cdk_array:
	  found = true;
	  break;

	case cdk_error:
	  return true;

	default:
	  declarator = declarator->declarator;
	  break;
	}
    }

  return !found;
}

cp_parameter_declarator *no_parameters;

/* Create a parameter declarator with the indicated DECL_SPECIFIERS,
   DECLARATOR and DEFAULT_ARGUMENT.  */

cp_parameter_declarator *
make_parameter_declarator (cp_decl_specifier_seq *decl_specifiers,
			   cp_declarator *declarator,
			   tree default_argument)
{
  cp_parameter_declarator *parameter;

  parameter = ((cp_parameter_declarator *)
	       alloc_declarator (sizeof (cp_parameter_declarator)));
  parameter->next = NULL;
  if (decl_specifiers)
    parameter->decl_specifiers = *decl_specifiers;
  else
    clear_decl_specs (&parameter->decl_specifiers);
  parameter->declarator = declarator;
  parameter->default_argument = default_argument;
  parameter->ellipsis_p = false;

  return parameter;
}

/* Returns true iff DECLARATOR  is a declaration for a function.  */

static bool
function_declarator_p (const cp_declarator *declarator)
{
  while (declarator)
    {
      if (declarator->kind == cdk_function
	  && declarator->declarator->kind == cdk_id)
	return true;
      if (declarator->kind == cdk_id
	  || declarator->kind == cdk_error)
	return false;
      declarator = declarator->declarator;
    }
  return false;
}
 
/* The parser.  */

/* Overview
   --------

   A cp_parser parses the token stream as specified by the C++
   grammar.  Its job is purely parsing, not semantic analysis.  For
   example, the parser breaks the token stream into declarators,
   expressions, statements, and other similar syntactic constructs.
   It does not check that the types of the expressions on either side
   of an assignment-statement are compatible, or that a function is
   not declared with a parameter of type `void'.

   The parser invokes routines elsewhere in the compiler to perform
   semantic analysis and to build up the abstract syntax tree for the
   code processed.

   The parser (and the template instantiation code, which is, in a
   way, a close relative of parsing) are the only parts of the
   compiler that should be calling push_scope and pop_scope, or
   related functions.  The parser (and template instantiation code)
   keeps track of what scope is presently active; everything else
   should simply honor that.  (The code that generates static
   initializers may also need to set the scope, in order to check
   access control correctly when emitting the initializers.)

   Methodology
   -----------

   The parser is of the standard recursive-descent variety.  Upcoming
   tokens in the token stream are examined in order to determine which
   production to use when parsing a non-terminal.  Some C++ constructs
   require arbitrary look ahead to disambiguate.  For example, it is
   impossible, in the general case, to tell whether a statement is an
   expression or declaration without scanning the entire statement.
   Therefore, the parser is capable of "parsing tentatively."  When the
   parser is not sure what construct comes next, it enters this mode.
   Then, while we attempt to parse the construct, the parser queues up
   error messages, rather than issuing them immediately, and saves the
   tokens it consumes.  If the construct is parsed successfully, the
   parser "commits", i.e., it issues any queued error messages and
   the tokens that were being preserved are permanently discarded.
   If, however, the construct is not parsed successfully, the parser
   rolls back its state completely so that it can resume parsing using
   a different alternative.

   Future Improvements
   -------------------

   The performance of the parser could probably be improved substantially.
   We could often eliminate the need to parse tentatively by looking ahead
   a little bit.  In some places, this approach might not entirely eliminate
   the need to parse tentatively, but it might still speed up the average
   case.  */

/* Flags that are passed to some parsing functions.  These values can
   be bitwise-ored together.  */

enum
{
  /* No flags.  */
  CP_PARSER_FLAGS_NONE = 0x0,
  /* The construct is optional.  If it is not present, then no error
     should be issued.  */
  CP_PARSER_FLAGS_OPTIONAL = 0x1,
  /* When parsing a type-specifier, treat user-defined type-names
     as non-type identifiers.  */
  CP_PARSER_FLAGS_NO_USER_DEFINED_TYPES = 0x2,
  /* When parsing a type-specifier, do not try to parse a class-specifier
     or enum-specifier.  */
  CP_PARSER_FLAGS_NO_TYPE_DEFINITIONS = 0x4
};

/* This type is used for parameters and variables which hold
   combinations of the above flags.  */
typedef int cp_parser_flags;

/* The different kinds of declarators we want to parse.  */

typedef enum cp_parser_declarator_kind
{
  /* We want an abstract declarator.  */
  CP_PARSER_DECLARATOR_ABSTRACT,
  /* We want a named declarator.  */
  CP_PARSER_DECLARATOR_NAMED,
  /* We don't mind, but the name must be an unqualified-id.  */
  CP_PARSER_DECLARATOR_EITHER
} cp_parser_declarator_kind;

/* The precedence values used to parse binary expressions.  The minimum value
   of PREC must be 1, because zero is reserved to quickly discriminate
   binary operators from other tokens.  */

enum cp_parser_prec
{
  PREC_NOT_OPERATOR,
  PREC_LOGICAL_OR_EXPRESSION,
  PREC_LOGICAL_AND_EXPRESSION,
  PREC_INCLUSIVE_OR_EXPRESSION,
  PREC_EXCLUSIVE_OR_EXPRESSION,
  PREC_AND_EXPRESSION,
  PREC_EQUALITY_EXPRESSION,
  PREC_RELATIONAL_EXPRESSION,
  PREC_SHIFT_EXPRESSION,
  PREC_ADDITIVE_EXPRESSION,
  PREC_MULTIPLICATIVE_EXPRESSION,
  PREC_PM_EXPRESSION,
  NUM_PREC_VALUES = PREC_PM_EXPRESSION
};

/* A mapping from a token type to a corresponding tree node type, with a
   precedence value.  */

typedef struct cp_parser_binary_operations_map_node
{
  /* The token type.  */
  enum cpp_ttype token_type;
  /* The corresponding tree code.  */
  enum tree_code tree_type;
  /* The precedence of this operator.  */
  enum cp_parser_prec prec;
} cp_parser_binary_operations_map_node;

/* The status of a tentative parse.  */

typedef enum cp_parser_status_kind
{
  /* No errors have occurred.  */
  CP_PARSER_STATUS_KIND_NO_ERROR,
  /* An error has occurred.  */
  CP_PARSER_STATUS_KIND_ERROR,
  /* We are committed to this tentative parse, whether or not an error
     has occurred.  */
  CP_PARSER_STATUS_KIND_COMMITTED
} cp_parser_status_kind;

typedef struct cp_parser_expression_stack_entry
{
  /* Left hand side of the binary operation we are currently
     parsing.  */
  tree lhs;
  /* Original tree code for left hand side, if it was a binary
     expression itself (used for -Wparentheses).  */
  enum tree_code lhs_type;
  /* Tree code for the binary operation we are parsing.  */
  enum tree_code tree_type;
  /* Precedence of the binary operation we are parsing.  */
  enum cp_parser_prec prec;
} cp_parser_expression_stack_entry;

/* The stack for storing partial expressions.  We only need NUM_PREC_VALUES
   entries because precedence levels on the stack are monotonically
   increasing.  */
typedef struct cp_parser_expression_stack_entry
  cp_parser_expression_stack[NUM_PREC_VALUES];

/* Context that is saved and restored when parsing tentatively.  */
typedef struct GTY (()) cp_parser_context {
  /* If this is a tentative parsing context, the status of the
     tentative parse.  */
  enum cp_parser_status_kind status;
  /* If non-NULL, we have just seen a `x->' or `x.' expression.  Names
     that are looked up in this context must be looked up both in the
     scope given by OBJECT_TYPE (the type of `x' or `*x') and also in
     the context of the containing expression.  */
  tree object_type;

  /* The next parsing context in the stack.  */
  struct cp_parser_context *next;
} cp_parser_context;

/* Prototypes.  */

/* Constructors and destructors.  */

static cp_parser_context *cp_parser_context_new
  (cp_parser_context *);

/* Class variables.  */

static GTY((deletable)) cp_parser_context* cp_parser_context_free_list;

/* The operator-precedence table used by cp_parser_binary_expression.
   Transformed into an associative array (binops_by_token) by
   cp_parser_new.  */

static const cp_parser_binary_operations_map_node binops[] = {
  { CPP_DEREF_STAR, MEMBER_REF, PREC_PM_EXPRESSION },
  { CPP_DOT_STAR, DOTSTAR_EXPR, PREC_PM_EXPRESSION },

  { CPP_MULT, MULT_EXPR, PREC_MULTIPLICATIVE_EXPRESSION },
  { CPP_DIV, TRUNC_DIV_EXPR, PREC_MULTIPLICATIVE_EXPRESSION },
  { CPP_MOD, TRUNC_MOD_EXPR, PREC_MULTIPLICATIVE_EXPRESSION },

  { CPP_PLUS, PLUS_EXPR, PREC_ADDITIVE_EXPRESSION },
  { CPP_MINUS, MINUS_EXPR, PREC_ADDITIVE_EXPRESSION },

  { CPP_LSHIFT, LSHIFT_EXPR, PREC_SHIFT_EXPRESSION },
  { CPP_RSHIFT, RSHIFT_EXPR, PREC_SHIFT_EXPRESSION },

  { CPP_LESS, LT_EXPR, PREC_RELATIONAL_EXPRESSION },
  { CPP_GREATER, GT_EXPR, PREC_RELATIONAL_EXPRESSION },
  { CPP_LESS_EQ, LE_EXPR, PREC_RELATIONAL_EXPRESSION },
  { CPP_GREATER_EQ, GE_EXPR, PREC_RELATIONAL_EXPRESSION },

  { CPP_EQ_EQ, EQ_EXPR, PREC_EQUALITY_EXPRESSION },
  { CPP_NOT_EQ, NE_EXPR, PREC_EQUALITY_EXPRESSION },

  { CPP_AND, BIT_AND_EXPR, PREC_AND_EXPRESSION },

  { CPP_XOR, BIT_XOR_EXPR, PREC_EXCLUSIVE_OR_EXPRESSION },

  { CPP_OR, BIT_IOR_EXPR, PREC_INCLUSIVE_OR_EXPRESSION },

  { CPP_AND_AND, TRUTH_ANDIF_EXPR, PREC_LOGICAL_AND_EXPRESSION },

  { CPP_OR_OR, TRUTH_ORIF_EXPR, PREC_LOGICAL_OR_EXPRESSION }
};

/* The same as binops, but initialized by cp_parser_new so that
   binops_by_token[N].token_type == N.  Used in cp_parser_binary_expression
   for speed.  */
static cp_parser_binary_operations_map_node binops_by_token[N_CP_TTYPES];

/* Constructors and destructors.  */

/* Construct a new context.  The context below this one on the stack
   is given by NEXT.  */

static cp_parser_context *
cp_parser_context_new (cp_parser_context* next)
{
  cp_parser_context *context;

  /* Allocate the storage.  */
  if (cp_parser_context_free_list != NULL)
    {
      /* Pull the first entry from the free list.  */
      context = cp_parser_context_free_list;
      cp_parser_context_free_list = context->next;
      memset (context, 0, sizeof (*context));
    }
  else
    context = GGC_CNEW (cp_parser_context);

  /* No errors have occurred yet in this context.  */
  context->status = CP_PARSER_STATUS_KIND_NO_ERROR;
  /* If this is not the bottommost context, copy information that we
     need from the previous context.  */
  if (next)
    {
      /* If, in the NEXT context, we are parsing an `x->' or `x.'
	 expression, then we are parsing one in this context, too.  */
      context->object_type = next->object_type;
      /* Thread the stack.  */
      context->next = next;
    }

  return context;
}

/* The cp_parser structure represents the C++ parser.  */

typedef struct GTY(()) cp_parser {
  /* The lexer from which we are obtaining tokens.  */
  cp_lexer *lexer;

  /* The scope in which names should be looked up.  If NULL_TREE, then
     we look up names in the scope that is currently open in the
     source program.  If non-NULL, this is either a TYPE or
     NAMESPACE_DECL for the scope in which we should look.  It can
     also be ERROR_MARK, when we've parsed a bogus scope.

     This value is not cleared automatically after a name is looked
     up, so we must be careful to clear it before starting a new look
     up sequence.  (If it is not cleared, then `X::Y' followed by `Z'
     will look up `Z' in the scope of `X', rather than the current
     scope.)  Unfortunately, it is difficult to tell when name lookup
     is complete, because we sometimes peek at a token, look it up,
     and then decide not to consume it.   */
  tree scope;

  /* OBJECT_SCOPE and QUALIFYING_SCOPE give the scopes in which the
     last lookup took place.  OBJECT_SCOPE is used if an expression
     like "x->y" or "x.y" was used; it gives the type of "*x" or "x",
     respectively.  QUALIFYING_SCOPE is used for an expression of the
     form "X::Y"; it refers to X.  */
  tree object_scope;
  tree qualifying_scope;

  /* A stack of parsing contexts.  All but the bottom entry on the
     stack will be tentative contexts.

     We parse tentatively in order to determine which construct is in
     use in some situations.  For example, in order to determine
     whether a statement is an expression-statement or a
     declaration-statement we parse it tentatively as a
     declaration-statement.  If that fails, we then reparse the same
     token stream as an expression-statement.  */
  cp_parser_context *context;

  /* True if we are parsing GNU C++.  If this flag is not set, then
     GNU extensions are not recognized.  */
  bool allow_gnu_extensions_p;

  /* TRUE if the `>' token should be interpreted as the greater-than
     operator.  FALSE if it is the end of a template-id or
     template-parameter-list. In C++0x mode, this flag also applies to
     `>>' tokens, which are viewed as two consecutive `>' tokens when
     this flag is FALSE.  */
  bool greater_than_is_operator_p;

  /* TRUE if default arguments are allowed within a parameter list
     that starts at this point. FALSE if only a gnu extension makes
     them permissible.  */
  bool default_arg_ok_p;

  /* TRUE if we are parsing an integral constant-expression.  See
     [expr.const] for a precise definition.  */
  bool integral_constant_expression_p;

  /* TRUE if we are parsing an integral constant-expression -- but a
     non-constant expression should be permitted as well.  This flag
     is used when parsing an array bound so that GNU variable-length
     arrays are tolerated.  */
  bool allow_non_integral_constant_expression_p;

  /* TRUE if ALLOW_NON_CONSTANT_EXPRESSION_P is TRUE and something has
     been seen that makes the expression non-constant.  */
  bool non_integral_constant_expression_p;

  /* TRUE if local variable names and `this' are forbidden in the
     current context.  */
  bool local_variables_forbidden_p;

  /* TRUE if the declaration we are parsing is part of a
     linkage-specification of the form `extern string-literal
     declaration'.  */
  bool in_unbraced_linkage_specification_p;

  /* TRUE if we are presently parsing a declarator, after the
     direct-declarator.  */
  bool in_declarator_p;

  /* TRUE if we are presently parsing a template-argument-list.  */
  bool in_template_argument_list_p;

  /* Set to IN_ITERATION_STMT if parsing an iteration-statement,
     to IN_OMP_BLOCK if parsing OpenMP structured block and
     IN_OMP_FOR if parsing OpenMP loop.  If parsing a switch statement,
     this is bitwise ORed with IN_SWITCH_STMT, unless parsing an
     iteration-statement, OpenMP block or loop within that switch.  */
#define IN_SWITCH_STMT		1
#define IN_ITERATION_STMT	2
#define IN_OMP_BLOCK		4
#define IN_OMP_FOR		8
#define IN_IF_STMT             16
  unsigned char in_statement;

  /* TRUE if we are presently parsing the body of a switch statement.
     Note that this doesn't quite overlap with in_statement above.
     The difference relates to giving the right sets of error messages:
     "case not in switch" vs "break statement used with OpenMP...".  */
  bool in_switch_statement_p;

  /* TRUE if we are parsing a type-id in an expression context.  In
     such a situation, both "type (expr)" and "type (type)" are valid
     alternatives.  */
  bool in_type_id_in_expr_p;

  /* TRUE if we are currently in a header file where declarations are
     implicitly extern "C".  */
  bool implicit_extern_c;

  /* TRUE if strings in expressions should be translated to the execution
     character set.  */
  bool translate_strings_p;

  /* TRUE if we are presently parsing the body of a function, but not
     a local class.  */
  bool in_function_body;

  /* If non-NULL, then we are parsing a construct where new type
     definitions are not permitted.  The string stored here will be
     issued as an error message if a type is defined.  */
  const char *type_definition_forbidden_message;

  /* A list of lists. The outer list is a stack, used for member
     functions of local classes. At each level there are two sub-list,
     one on TREE_VALUE and one on TREE_PURPOSE. Each of those
     sub-lists has a FUNCTION_DECL or TEMPLATE_DECL on their
     TREE_VALUE's. The functions are chained in reverse declaration
     order.

     The TREE_PURPOSE sublist contains those functions with default
     arguments that need post processing, and the TREE_VALUE sublist
     contains those functions with definitions that need post
     processing.

     These lists can only be processed once the outermost class being
     defined is complete.  */
  tree unparsed_functions_queues;

  /* The number of classes whose definitions are currently in
     progress.  */
  unsigned num_classes_being_defined;

  /* The number of template parameter lists that apply directly to the
     current declaration.  */
  unsigned num_template_parameter_lists;
} cp_parser;

/* Prototypes.  */

/* Constructors and destructors.  */

static cp_parser *cp_parser_new
  (void);

/* Routines to parse various constructs.

   Those that return `tree' will return the error_mark_node (rather
   than NULL_TREE) if a parse error occurs, unless otherwise noted.
   Sometimes, they will return an ordinary node if error-recovery was
   attempted, even though a parse error occurred.  So, to check
   whether or not a parse error occurred, you should always use
   cp_parser_error_occurred.  If the construct is optional (indicated
   either by an `_opt' in the name of the function that does the
   parsing or via a FLAGS parameter), then NULL_TREE is returned if
   the construct is not present.  */

/* Lexical conventions [gram.lex]  */

static tree cp_parser_identifier
  (cp_parser *);
static tree cp_parser_string_literal
  (cp_parser *, bool, bool);

/* Basic concepts [gram.basic]  */

static bool cp_parser_translation_unit
  (cp_parser *);

/* Expressions [gram.expr]  */

static tree cp_parser_primary_expression
  (cp_parser *, bool, bool, bool, cp_id_kind *);
static tree cp_parser_id_expression
  (cp_parser *, bool, bool, bool *, bool, bool);
static tree cp_parser_unqualified_id
  (cp_parser *, bool, bool, bool, bool);
static tree cp_parser_nested_name_specifier_opt
  (cp_parser *, bool, bool, bool, bool);
static tree cp_parser_nested_name_specifier
  (cp_parser *, bool, bool, bool, bool);
static tree cp_parser_qualifying_entity
  (cp_parser *, bool, bool, bool, bool, bool);
static tree cp_parser_postfix_expression
  (cp_parser *, bool, bool, bool, cp_id_kind *);
static tree cp_parser_postfix_open_square_expression
  (cp_parser *, tree, bool);
static tree cp_parser_postfix_dot_deref_expression
  (cp_parser *, enum cpp_ttype, tree, bool, cp_id_kind *, location_t);
static VEC(tree,gc) *cp_parser_parenthesized_expression_list
  (cp_parser *, bool, bool, bool, bool *);
static void cp_parser_pseudo_destructor_name
  (cp_parser *, tree *, tree *);
static tree cp_parser_unary_expression
  (cp_parser *, bool, bool, cp_id_kind *);
static enum tree_code cp_parser_unary_operator
  (cp_token *);
static tree cp_parser_new_expression
  (cp_parser *);
static VEC(tree,gc) *cp_parser_new_placement
  (cp_parser *);
static tree cp_parser_new_type_id
  (cp_parser *, tree *);
static cp_declarator *cp_parser_new_declarator_opt
  (cp_parser *);
static cp_declarator *cp_parser_direct_new_declarator
  (cp_parser *);
static VEC(tree,gc) *cp_parser_new_initializer
  (cp_parser *);
static tree cp_parser_delete_expression
  (cp_parser *);
static tree cp_parser_cast_expression
  (cp_parser *, bool, bool, cp_id_kind *);
static tree cp_parser_binary_expression
  (cp_parser *, bool, bool, enum cp_parser_prec, cp_id_kind *);
static tree cp_parser_question_colon_clause
  (cp_parser *, tree);
static tree cp_parser_assignment_expression
  (cp_parser *, bool, cp_id_kind *);
static enum tree_code cp_parser_assignment_operator_opt
  (cp_parser *);
static tree cp_parser_expression
  (cp_parser *, bool, cp_id_kind *);
static tree cp_parser_constant_expression
  (cp_parser *, bool, bool *);
static tree cp_parser_builtin_offsetof
  (cp_parser *);
static tree cp_parser_lambda_expression
  (cp_parser *);
static void cp_parser_lambda_introducer
  (cp_parser *, tree);
static void cp_parser_lambda_declarator_opt
  (cp_parser *, tree);
static void cp_parser_lambda_body
  (cp_parser *, tree);

/* Statements [gram.stmt.stmt]  */

static void cp_parser_statement
  (cp_parser *, tree, bool, bool *);
static void cp_parser_label_for_labeled_statement
  (cp_parser *);
static tree cp_parser_expression_statement
  (cp_parser *, tree);
static tree cp_parser_compound_statement
  (cp_parser *, tree, bool);
static void cp_parser_statement_seq_opt
  (cp_parser *, tree);
static tree cp_parser_selection_statement
  (cp_parser *, bool *);
static tree cp_parser_condition
  (cp_parser *);
static tree cp_parser_iteration_statement
  (cp_parser *);
static void cp_parser_for_init_statement
  (cp_parser *);
static tree cp_parser_jump_statement
  (cp_parser *);
static void cp_parser_declaration_statement
  (cp_parser *);

static tree cp_parser_implicitly_scoped_statement
  (cp_parser *, bool *);
static void cp_parser_already_scoped_statement
  (cp_parser *);

/* Declarations [gram.dcl.dcl] */

static void cp_parser_declaration_seq_opt
  (cp_parser *);
static void cp_parser_declaration
  (cp_parser *);
static void cp_parser_block_declaration
  (cp_parser *, bool);
static void cp_parser_simple_declaration
  (cp_parser *, bool);
static void cp_parser_decl_specifier_seq
  (cp_parser *, cp_parser_flags, cp_decl_specifier_seq *, int *);
static tree cp_parser_storage_class_specifier_opt
  (cp_parser *);
static tree cp_parser_function_specifier_opt
  (cp_parser *, cp_decl_specifier_seq *);
static tree cp_parser_type_specifier
  (cp_parser *, cp_parser_flags, cp_decl_specifier_seq *, bool,
   int *, bool *);
static tree cp_parser_simple_type_specifier
  (cp_parser *, cp_decl_specifier_seq *, cp_parser_flags);
static tree cp_parser_type_name
  (cp_parser *);
static tree cp_parser_nonclass_name 
  (cp_parser* parser);
static tree cp_parser_elaborated_type_specifier
  (cp_parser *, bool, bool);
static tree cp_parser_enum_specifier
  (cp_parser *);
static void cp_parser_enumerator_list
  (cp_parser *, tree);
static void cp_parser_enumerator_definition
  (cp_parser *, tree);
static tree cp_parser_namespace_name
  (cp_parser *);
static void cp_parser_namespace_definition
  (cp_parser *);
static void cp_parser_namespace_body
  (cp_parser *);
static tree cp_parser_qualified_namespace_specifier
  (cp_parser *);
static void cp_parser_namespace_alias_definition
  (cp_parser *);
static bool cp_parser_using_declaration
  (cp_parser *, bool);
static void cp_parser_using_directive
  (cp_parser *);
static void cp_parser_asm_definition
  (cp_parser *);
static void cp_parser_linkage_specification
  (cp_parser *);
static void cp_parser_static_assert
  (cp_parser *, bool);
static tree cp_parser_decltype
  (cp_parser *);

/* Declarators [gram.dcl.decl] */

static tree cp_parser_init_declarator
  (cp_parser *, cp_decl_specifier_seq *, VEC (deferred_access_check,gc)*, bool, bool, int, bool *);
static cp_declarator *cp_parser_declarator
  (cp_parser *, cp_parser_declarator_kind, int *, bool *, bool);
static cp_declarator *cp_parser_direct_declarator
  (cp_parser *, cp_parser_declarator_kind, int *, bool);
static enum tree_code cp_parser_ptr_operator
  (cp_parser *, tree *, cp_cv_quals *);
static cp_cv_quals cp_parser_cv_qualifier_seq_opt
  (cp_parser *);
static tree cp_parser_late_return_type_opt
  (cp_parser *);
static tree cp_parser_declarator_id
  (cp_parser *, bool);
static tree cp_parser_type_id
  (cp_parser *);
static tree cp_parser_template_type_arg
  (cp_parser *);
static tree cp_parser_trailing_type_id (cp_parser *);
static tree cp_parser_type_id_1
  (cp_parser *, bool, bool);
static void cp_parser_type_specifier_seq
  (cp_parser *, bool, bool, cp_decl_specifier_seq *);
static tree cp_parser_parameter_declaration_clause
  (cp_parser *);
static tree cp_parser_parameter_declaration_list
  (cp_parser *, bool *);
static cp_parameter_declarator *cp_parser_parameter_declaration
  (cp_parser *, bool, bool *);
static tree cp_parser_default_argument 
  (cp_parser *, bool);
static void cp_parser_function_body
  (cp_parser *);
static tree cp_parser_initializer
  (cp_parser *, bool *, bool *);
static tree cp_parser_initializer_clause
  (cp_parser *, bool *);
static tree cp_parser_braced_list
  (cp_parser*, bool*);
static VEC(constructor_elt,gc) *cp_parser_initializer_list
  (cp_parser *, bool *);

static bool cp_parser_ctor_initializer_opt_and_function_body
  (cp_parser *);

/* Classes [gram.class] */

static tree cp_parser_class_name
  (cp_parser *, bool, bool, enum tag_types, bool, bool, bool);
static tree cp_parser_class_specifier
  (cp_parser *);
static tree cp_parser_class_head
  (cp_parser *, bool *, tree *, tree *);
static enum tag_types cp_parser_class_key
  (cp_parser *);
static void cp_parser_member_specification_opt
  (cp_parser *);
static void cp_parser_member_declaration
  (cp_parser *);
static tree cp_parser_pure_specifier
  (cp_parser *);
static tree cp_parser_constant_initializer
  (cp_parser *);

/* Derived classes [gram.class.derived] */

static tree cp_parser_base_clause
  (cp_parser *);
static tree cp_parser_base_specifier
  (cp_parser *);

/* Special member functions [gram.special] */

static tree cp_parser_conversion_function_id
  (cp_parser *);
static tree cp_parser_conversion_type_id
  (cp_parser *);
static cp_declarator *cp_parser_conversion_declarator_opt
  (cp_parser *);
static bool cp_parser_ctor_initializer_opt
  (cp_parser *);
static void cp_parser_mem_initializer_list
  (cp_parser *);
static tree cp_parser_mem_initializer
  (cp_parser *);
static tree cp_parser_mem_initializer_id
  (cp_parser *);

/* Overloading [gram.over] */

static tree cp_parser_operator_function_id
  (cp_parser *);
static tree cp_parser_operator
  (cp_parser *);

/* Templates [gram.temp] */

static void cp_parser_template_declaration
  (cp_parser *, bool);
static tree cp_parser_template_parameter_list
  (cp_parser *);
static tree cp_parser_template_parameter
  (cp_parser *, bool *, bool *);
static tree cp_parser_type_parameter
  (cp_parser *, bool *);
static tree cp_parser_template_id
  (cp_parser *, bool, bool, bool);
static tree cp_parser_template_name
  (cp_parser *, bool, bool, bool, bool *);
static tree cp_parser_template_argument_list
  (cp_parser *);
static tree cp_parser_template_argument
  (cp_parser *);
static void cp_parser_explicit_instantiation
  (cp_parser *);
static void cp_parser_explicit_specialization
  (cp_parser *);

/* Exception handling [gram.exception] */

static tree cp_parser_try_block
  (cp_parser *);
static bool cp_parser_function_try_block
  (cp_parser *);
static void cp_parser_handler_seq
  (cp_parser *);
static void cp_parser_handler
  (cp_parser *);
static tree cp_parser_exception_declaration
  (cp_parser *);
static tree cp_parser_throw_expression
  (cp_parser *);
static tree cp_parser_exception_specification_opt
  (cp_parser *);
static tree cp_parser_type_id_list
  (cp_parser *);

/* GNU Extensions */

static tree cp_parser_asm_specification_opt
  (cp_parser *);
static tree cp_parser_asm_operand_list
  (cp_parser *);
static tree cp_parser_asm_clobber_list
  (cp_parser *);
static tree cp_parser_asm_label_list
  (cp_parser *);
static tree cp_parser_attributes_opt
  (cp_parser *);
static tree cp_parser_attribute_list
  (cp_parser *);
static bool cp_parser_extension_opt
  (cp_parser *, int *);
static void cp_parser_label_declaration
  (cp_parser *);

enum pragma_context { pragma_external, pragma_stmt, pragma_compound };
static bool cp_parser_pragma
  (cp_parser *, enum pragma_context);

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
  (cp_parser *);
static tree cp_parser_objc_statement
  (cp_parser *);

/* Utility Routines */

static tree cp_parser_lookup_name
  (cp_parser *, tree, enum tag_types, bool, bool, bool, tree *, location_t);
static tree cp_parser_lookup_name_simple
  (cp_parser *, tree, location_t);
static tree cp_parser_maybe_treat_template_as_class
  (tree, bool);
static bool cp_parser_check_declarator_template_parameters
  (cp_parser *, cp_declarator *, location_t);
static bool cp_parser_check_template_parameters
  (cp_parser *, unsigned, location_t, cp_declarator *);
static tree cp_parser_simple_cast_expression
  (cp_parser *);
static tree cp_parser_global_scope_opt
  (cp_parser *, bool);
static bool cp_parser_constructor_declarator_p
  (cp_parser *, bool);
static tree cp_parser_function_definition_from_specifiers_and_declarator
  (cp_parser *, cp_decl_specifier_seq *, tree, const cp_declarator *);
static tree cp_parser_function_definition_after_declarator
  (cp_parser *, bool);
static void cp_parser_template_declaration_after_export
  (cp_parser *, bool);
static void cp_parser_perform_template_parameter_access_checks
  (VEC (deferred_access_check,gc)*);
static tree cp_parser_single_declaration
  (cp_parser *, VEC (deferred_access_check,gc)*, bool, bool, bool *);
static tree cp_parser_functional_cast
  (cp_parser *, tree);
static tree cp_parser_save_member_function_body
  (cp_parser *, cp_decl_specifier_seq *, cp_declarator *, tree);
static tree cp_parser_enclosed_template_argument_list
  (cp_parser *);
static void cp_parser_save_default_args
  (cp_parser *, tree);
static void cp_parser_late_parsing_for_member
  (cp_parser *, tree);
static void cp_parser_late_parsing_default_args
  (cp_parser *, tree);
static tree cp_parser_sizeof_operand
  (cp_parser *, enum rid);
static tree cp_parser_trait_expr
  (cp_parser *, enum rid);
static bool cp_parser_declares_only_class_p
  (cp_parser *);
static void cp_parser_set_storage_class
  (cp_parser *, cp_decl_specifier_seq *, enum rid, location_t);
static void cp_parser_set_decl_spec_type
  (cp_decl_specifier_seq *, tree, location_t, bool);
static bool cp_parser_friend_p
  (const cp_decl_specifier_seq *);
static cp_token *cp_parser_require
  (cp_parser *, enum cpp_ttype, const char *);
static cp_token *cp_parser_require_keyword
  (cp_parser *, enum rid, const char *);
static bool cp_parser_token_starts_function_definition_p
  (cp_token *);
static bool cp_parser_next_token_starts_class_definition_p
  (cp_parser *);
static bool cp_parser_next_token_ends_template_argument_p
  (cp_parser *);
static bool cp_parser_nth_token_starts_template_argument_list_p
  (cp_parser *, size_t);
static enum tag_types cp_parser_token_is_class_key
  (cp_token *);
static void cp_parser_check_class_key
  (enum tag_types, tree type);
static void cp_parser_check_access_in_redeclaration
  (tree type, location_t location);
static bool cp_parser_optional_template_keyword
  (cp_parser *);
static void cp_parser_pre_parsed_nested_name_specifier
  (cp_parser *);
static bool cp_parser_cache_group
  (cp_parser *, enum cpp_ttype, unsigned);
static void cp_parser_parse_tentatively
  (cp_parser *);
static void cp_parser_commit_to_tentative_parse
  (cp_parser *);
static void cp_parser_abort_tentative_parse
  (cp_parser *);
static bool cp_parser_parse_definitely
  (cp_parser *);
static inline bool cp_parser_parsing_tentatively
  (cp_parser *);
static bool cp_parser_uncommitted_to_tentative_parse_p
  (cp_parser *);
static void cp_parser_error
  (cp_parser *, const char *);
static void cp_parser_name_lookup_error
  (cp_parser *, tree, tree, const char *, location_t);
static bool cp_parser_simulate_error
  (cp_parser *);
static bool cp_parser_check_type_definition
  (cp_parser *);
static void cp_parser_check_for_definition_in_return_type
  (cp_declarator *, tree, location_t type_location);
static void cp_parser_check_for_invalid_template_id
  (cp_parser *, tree, location_t location);
static bool cp_parser_non_integral_constant_expression
  (cp_parser *, const char *);
static void cp_parser_diagnose_invalid_type_name
  (cp_parser *, tree, tree, location_t);
static bool cp_parser_parse_and_diagnose_invalid_type_name
  (cp_parser *);
static int cp_parser_skip_to_closing_parenthesis
  (cp_parser *, bool, bool, bool);
static void cp_parser_skip_to_end_of_statement
  (cp_parser *);
static void cp_parser_consume_semicolon_at_end_of_statement
  (cp_parser *);
static void cp_parser_skip_to_end_of_block_or_statement
  (cp_parser *);
static bool cp_parser_skip_to_closing_brace
  (cp_parser *);
static void cp_parser_skip_to_end_of_template_parameter_list
  (cp_parser *);
static void cp_parser_skip_to_pragma_eol
  (cp_parser*, cp_token *);
static bool cp_parser_error_occurred
  (cp_parser *);
static bool cp_parser_allow_gnu_extensions_p
  (cp_parser *);
static bool cp_parser_is_string_literal
  (cp_token *);
static bool cp_parser_is_keyword
  (cp_token *, enum rid);
static tree cp_parser_make_typename_type
  (cp_parser *, tree, tree, location_t location);
static cp_declarator * cp_parser_make_indirect_declarator
  (enum tree_code, tree, cp_cv_quals, cp_declarator *);

/* Returns nonzero if we are parsing tentatively.  */

static inline bool
cp_parser_parsing_tentatively (cp_parser* parser)
{
  return parser->context->next != NULL;
}

/* Returns nonzero if TOKEN is a string literal.  */

static bool
cp_parser_is_string_literal (cp_token* token)
{
  return (token->type == CPP_STRING ||
	  token->type == CPP_STRING16 ||
	  token->type == CPP_STRING32 ||
	  token->type == CPP_WSTRING ||
	  token->type == CPP_UTF8STRING);
}

/* Returns nonzero if TOKEN is the indicated KEYWORD.  */

static bool
cp_parser_is_keyword (cp_token* token, enum rid keyword)
{
  return token->keyword == keyword;
}

/* If not parsing tentatively, issue a diagnostic of the form
      FILE:LINE: MESSAGE before TOKEN
   where TOKEN is the next token in the input stream.  MESSAGE
   (specified by the caller) is usually of the form "expected
   OTHER-TOKEN".  */

static void
cp_parser_error (cp_parser* parser, const char* message)
{
  if (!cp_parser_simulate_error (parser))
    {
      cp_token *token = cp_lexer_peek_token (parser->lexer);
      /* This diagnostic makes more sense if it is tagged to the line
	 of the token we just peeked at.  */
      cp_lexer_set_source_position_from_token (token);

      if (token->type == CPP_PRAGMA)
	{
	  error_at (token->location,
		    "%<#pragma%> is not allowed here");
	  cp_parser_skip_to_pragma_eol (parser, token);
	  return;
	}

      c_parse_error (message,
		     /* Because c_parser_error does not understand
			CPP_KEYWORD, keywords are treated like
			identifiers.  */
		     (token->type == CPP_KEYWORD ? CPP_NAME : token->type),
		     token->u.value, token->flags);
    }
}

/* Issue an error about name-lookup failing.  NAME is the
   IDENTIFIER_NODE DECL is the result of
   the lookup (as returned from cp_parser_lookup_name).  DESIRED is
   the thing that we hoped to find.  */

static void
cp_parser_name_lookup_error (cp_parser* parser,
			     tree name,
			     tree decl,
			     const char* desired,
			     location_t location)
{
  /* If name lookup completely failed, tell the user that NAME was not
     declared.  */
  if (decl == error_mark_node)
    {
      if (parser->scope && parser->scope != global_namespace)
	error_at (location, "%<%E::%E%> has not been declared",
		  parser->scope, name);
      else if (parser->scope == global_namespace)
	error_at (location, "%<::%E%> has not been declared", name);
      else if (parser->object_scope
	       && !CLASS_TYPE_P (parser->object_scope))
	error_at (location, "request for member %qE in non-class type %qT",
		  name, parser->object_scope);
      else if (parser->object_scope)
	error_at (location, "%<%T::%E%> has not been declared",
		  parser->object_scope, name);
      else
	error_at (location, "%qE has not been declared", name);
    }
  else if (parser->scope && parser->scope != global_namespace)
    error_at (location, "%<%E::%E%> %s", parser->scope, name, desired);
  else if (parser->scope == global_namespace)
    error_at (location, "%<::%E%> %s", name, desired);
  else
    error_at (location, "%qE %s", name, desired);
}

/* If we are parsing tentatively, remember that an error has occurred
   during this tentative parse.  Returns true if the error was
   simulated; false if a message should be issued by the caller.  */

static bool
cp_parser_simulate_error (cp_parser* parser)
{
  if (cp_parser_uncommitted_to_tentative_parse_p (parser))
    {
      parser->context->status = CP_PARSER_STATUS_KIND_ERROR;
      return true;
    }
  return false;
}

/* Check for repeated decl-specifiers.  */

static void
cp_parser_check_decl_spec (cp_decl_specifier_seq *decl_specs,
			   location_t location)
{
  int ds;

  for (ds = ds_first; ds != ds_last; ++ds)
    {
      unsigned count = decl_specs->specs[ds];
      if (count < 2)
	continue;
      /* The "long" specifier is a special case because of "long long".  */
      if (ds == ds_long)
	{
	  if (count > 2)
	    error_at (location, "%<long long long%> is too long for GCC");
	  else 
	    pedwarn_cxx98 (location, OPT_Wlong_long, 
			   "ISO C++ 1998 does not support %<long long%>");
	}
      else if (count > 1)
	{
	  static const char *const decl_spec_names[] = {
	    "signed",
	    "unsigned",
	    "short",
	    "long",
	    "const",
	    "volatile",
	    "restrict",
	    "inline",
	    "virtual",
	    "explicit",
	    "friend",
	    "typedef",
            "constexpr",
	    "__complex",
	    "__thread"
	  };
	  error_at (location, "duplicate %qs", decl_spec_names[ds]);
	}
    }
}

/* This function is called when a type is defined.  If type
   definitions are forbidden at this point, an error message is
   issued.  */

static bool
cp_parser_check_type_definition (cp_parser* parser)
{
  /* If types are forbidden here, issue a message.  */
  if (parser->type_definition_forbidden_message)
    {
      /* Don't use `%s' to print the string, because quotations (`%<', `%>')
	 in the message need to be interpreted.  */
      error (parser->type_definition_forbidden_message);
      return false;
    }
  return true;
}

/* This function is called when the DECLARATOR is processed.  The TYPE
   was a type defined in the decl-specifiers.  If it is invalid to
   define a type in the decl-specifiers for DECLARATOR, an error is
   issued. TYPE_LOCATION is the location of TYPE and is used
   for error reporting.  */

static void
cp_parser_check_for_definition_in_return_type (cp_declarator *declarator,
					       tree type, location_t type_location)
{
  /* [dcl.fct] forbids type definitions in return types.
     Unfortunately, it's not easy to know whether or not we are
     processing a return type until after the fact.  */
  while (declarator
	 && (declarator->kind == cdk_pointer
	     || declarator->kind == cdk_reference
	     || declarator->kind == cdk_ptrmem))
    declarator = declarator->declarator;
  if (declarator
      && declarator->kind == cdk_function)
    {
      error_at (type_location,
		"new types may not be defined in a return type");
      inform (type_location, 
	      "(perhaps a semicolon is missing after the definition of %qT)",
	      type);
    }
}

/* A type-specifier (TYPE) has been parsed which cannot be followed by
   "<" in any valid C++ program.  If the next token is indeed "<",
   issue a message warning the user about what appears to be an
   invalid attempt to form a template-id. LOCATION is the location
   of the type-specifier (TYPE) */

static void
cp_parser_check_for_invalid_template_id (cp_parser* parser,
					 tree type, location_t location)
{
  cp_token_position start = 0;

  if (cp_lexer_next_token_is (parser->lexer, CPP_LESS))
    {
      if (TYPE_P (type))
	error_at (location, "%qT is not a template", type);
      else if (TREE_CODE (type) == IDENTIFIER_NODE)
	error_at (location, "%qE is not a template", type);
      else
	error_at (location, "invalid template-id");
      /* Remember the location of the invalid "<".  */
      if (cp_parser_uncommitted_to_tentative_parse_p (parser))
	start = cp_lexer_token_position (parser->lexer, true);
      /* Consume the "<".  */
      cp_lexer_consume_token (parser->lexer);
      /* Parse the template arguments.  */
      cp_parser_enclosed_template_argument_list (parser);
      /* Permanently remove the invalid template arguments so that
	 this error message is not issued again.  */
      if (start)
	cp_lexer_purge_tokens_after (parser->lexer, start);
    }
}

/* If parsing an integral constant-expression, issue an error message
   about the fact that THING appeared and return true.  Otherwise,
   return false.  In either case, set
   PARSER->NON_INTEGRAL_CONSTANT_EXPRESSION_P.  */

static bool
cp_parser_non_integral_constant_expression (cp_parser  *parser,
					    const char *thing)
{
  parser->non_integral_constant_expression_p = true;
  if (parser->integral_constant_expression_p)
    {
      if (!parser->allow_non_integral_constant_expression_p)
	{
	  /* Don't use `%s' to print THING, because quotations (`%<', `%>')
	     in the message need to be interpreted.  */
	  char *message = concat (thing,
				  " cannot appear in a constant-expression",
				  NULL);
	  error (message);
	  free (message);
	  return true;
	}
    }
  return false;
}

/* Emit a diagnostic for an invalid type name.  SCOPE is the
   qualifying scope (or NULL, if none) for ID.  This function commits
   to the current active tentative parse, if any.  (Otherwise, the
   problematic construct might be encountered again later, resulting
   in duplicate error messages.) LOCATION is the location of ID.  */

static void
cp_parser_diagnose_invalid_type_name (cp_parser *parser,
				      tree scope, tree id,
				      location_t location)
{
  tree decl, old_scope;
  /* Try to lookup the identifier.  */
  old_scope = parser->scope;
  parser->scope = scope;
  decl = cp_parser_lookup_name_simple (parser, id, location);
  parser->scope = old_scope;
  /* If the lookup found a template-name, it means that the user forgot
  to specify an argument list. Emit a useful error message.  */
  if (TREE_CODE (decl) == TEMPLATE_DECL)
    error_at (location,
	      "invalid use of template-name %qE without an argument list",
	      decl);
  else if (TREE_CODE (id) == BIT_NOT_EXPR)
    error_at (location, "invalid use of destructor %qD as a type", id);
  else if (TREE_CODE (decl) == TYPE_DECL)
    /* Something like 'unsigned A a;'  */
    error_at (location, "invalid combination of multiple type-specifiers");
  else if (!parser->scope)
    {
      /* Issue an error message.  */
      error_at (location, "%qE does not name a type", id);
      /* If we're in a template class, it's possible that the user was
	 referring to a type from a base class.  For example:

	   template <typename T> struct A { typedef T X; };
	   template <typename T> struct B : public A<T> { X x; };

	 The user should have said "typename A<T>::X".  */
      if (processing_template_decl && current_class_type
	  && TYPE_BINFO (current_class_type))
	{
	  tree b;

	  for (b = TREE_CHAIN (TYPE_BINFO (current_class_type));
	       b;
	       b = TREE_CHAIN (b))
	    {
	      tree base_type = BINFO_TYPE (b);
	      if (CLASS_TYPE_P (base_type)
		  && dependent_type_p (base_type))
		{
		  tree field;
		  /* Go from a particular instantiation of the
		     template (which will have an empty TYPE_FIELDs),
		     to the main version.  */
		  base_type = CLASSTYPE_PRIMARY_TEMPLATE_TYPE (base_type);
		  for (field = TYPE_FIELDS (base_type);
		       field;
		       field = TREE_CHAIN (field))
		    if (TREE_CODE (field) == TYPE_DECL
			&& DECL_NAME (field) == id)
		      {
			inform (location, 
				"(perhaps %<typename %T::%E%> was intended)",
				BINFO_TYPE (b), id);
			break;
		      }
		  if (field)
		    break;
		}
	    }
	}
    }
  /* Here we diagnose qualified-ids where the scope is actually correct,
     but the identifier does not resolve to a valid type name.  */
  else if (parser->scope != error_mark_node)
    {
      if (TREE_CODE (parser->scope) == NAMESPACE_DECL)
	error_at (location, "%qE in namespace %qE does not name a type",
		  id, parser->scope);
      else if (CLASS_TYPE_P (parser->scope)
	       && constructor_name_p (id, parser->scope))
	{
	  /* A<T>::A<T>() */
	  error_at (location, "%<%T::%E%> names the constructor, not"
		    " the type", parser->scope, id);
	  if (cp_lexer_next_token_is (parser->lexer, CPP_LESS))
	    error_at (location, "and %qT has no template constructors",
		      parser->scope);
	}
      else if (TYPE_P (parser->scope)
	       && dependent_scope_p (parser->scope))
	error_at (location, "need %<typename%> before %<%T::%E%> because "
		  "%qT is a dependent scope",
		  parser->scope, id, parser->scope);
      else if (TYPE_P (parser->scope))
	error_at (location, "%qE in class %qT does not name a type",
		  id, parser->scope);
      else
	gcc_unreachable ();
    }
  cp_parser_commit_to_tentative_parse (parser);
}

/* Check for a common situation where a type-name should be present,
   but is not, and issue a sensible error message.  Returns true if an
   invalid type-name was detected.

   The situation handled by this function are variable declarations of the
   form `ID a', where `ID' is an id-expression and `a' is a plain identifier.
   Usually, `ID' should name a type, but if we got here it means that it
   does not. We try to emit the best possible error message depending on
   how exactly the id-expression looks like.  */

static bool
cp_parser_parse_and_diagnose_invalid_type_name (cp_parser *parser)
{
  tree id;
  cp_token *token = cp_lexer_peek_token (parser->lexer);

  /* Avoid duplicate error about ambiguous lookup.  */
  if (token->type == CPP_NESTED_NAME_SPECIFIER)
    {
      cp_token *next = cp_lexer_peek_nth_token (parser->lexer, 2);
      if (next->type == CPP_NAME && next->ambiguous_p)
	goto out;
    }

  cp_parser_parse_tentatively (parser);
  id = cp_parser_id_expression (parser,
				/*template_keyword_p=*/false,
				/*check_dependency_p=*/true,
				/*template_p=*/NULL,
				/*declarator_p=*/true,
				/*optional_p=*/false);
  /* If the next token is a (, this is a function with no explicit return
     type, i.e. constructor, destructor or conversion op.  */
  if (cp_lexer_next_token_is (parser->lexer, CPP_OPEN_PAREN)
      || TREE_CODE (id) == TYPE_DECL)
    {
      cp_parser_abort_tentative_parse (parser);
      return false;
    }
  if (!cp_parser_parse_definitely (parser))
    return false;

  /* Emit a diagnostic for the invalid type.  */
  cp_parser_diagnose_invalid_type_name (parser, parser->scope,
					id, token->location);
 out:
  /* If we aren't in the middle of a declarator (i.e. in a
     parameter-declaration-clause), skip to the end of the declaration;
     there's no point in trying to process it.  */
  if (!parser->in_declarator_p)
    cp_parser_skip_to_end_of_block_or_statement (parser);
  return true;
}

/* Consume tokens up to, and including, the next non-nested closing `)'.
   Returns 1 iff we found a closing `)'.  RECOVERING is true, if we
   are doing error recovery. Returns -1 if OR_COMMA is true and we
   found an unnested comma.  */

static int
cp_parser_skip_to_closing_parenthesis (cp_parser *parser,
				       bool recovering,
				       bool or_comma,
				       bool consume_paren)
{
  unsigned paren_depth = 0;
  unsigned brace_depth = 0;
  unsigned square_depth = 0;

  if (recovering && !or_comma
      && cp_parser_uncommitted_to_tentative_parse_p (parser))
    return 0;

  while (true)
    {
      cp_token * token = cp_lexer_peek_token (parser->lexer);

      switch (token->type)
	{
	case CPP_EOF:
	case CPP_PRAGMA_EOL:
	  /* If we've run out of tokens, then there is no closing `)'.  */
	  return 0;

        /* This is good for lambda expression capture-lists.  */
        case CPP_OPEN_SQUARE:
          ++square_depth;
          break;
        case CPP_CLOSE_SQUARE:
          if (!square_depth--)
            return 0;
          break;

	case CPP_SEMICOLON:
	  /* This matches the processing in skip_to_end_of_statement.  */
	  if (!brace_depth)
	    return 0;
	  break;

	case CPP_OPEN_BRACE:
	  ++brace_depth;
	  break;
	case CPP_CLOSE_BRACE:
	  if (!brace_depth--)
	    return 0;
	  break;

	case CPP_COMMA:
	  if (recovering && or_comma && !brace_depth && !paren_depth
	      && !square_depth)
	    return -1;
	  break;

	case CPP_OPEN_PAREN:
	  if (!brace_depth)
	    ++paren_depth;
	  break;

	case CPP_CLOSE_PAREN:
	  if (!brace_depth && !paren_depth--)
	    {
	      if (consume_paren)
		cp_lexer_consume_token (parser->lexer);
	      return 1;
	    }
	  break;

	default:
	  break;
	}

      /* Consume the token.  */
      cp_lexer_consume_token (parser->lexer);
    }
}

/* Consume tokens until we reach the end of the current statement.
   Normally, that will be just before consuming a `;'.  However, if a
   non-nested `}' comes first, then we stop before consuming that.  */

static void
cp_parser_skip_to_end_of_statement (cp_parser* parser)
{
  unsigned nesting_depth = 0;

  while (true)
    {
      cp_token *token = cp_lexer_peek_token (parser->lexer);

      switch (token->type)
	{
	case CPP_EOF:
	case CPP_PRAGMA_EOL:
	  /* If we've run out of tokens, stop.  */
	  return;

	case CPP_SEMICOLON:
	  /* If the next token is a `;', we have reached the end of the
	     statement.  */
	  if (!nesting_depth)
	    return;
	  break;

	case CPP_CLOSE_BRACE:
	  /* If this is a non-nested '}', stop before consuming it.
	     That way, when confronted with something like:

	       { 3 + }

	     we stop before consuming the closing '}', even though we
	     have not yet reached a `;'.  */
	  if (nesting_depth == 0)
	    return;

	  /* If it is the closing '}' for a block that we have
	     scanned, stop -- but only after consuming the token.
	     That way given:

		void f g () { ... }
		typedef int I;

	     we will stop after the body of the erroneously declared
	     function, but before consuming the following `typedef'
	     declaration.  */
	  if (--nesting_depth == 0)
	    {
	      cp_lexer_consume_token (parser->lexer);
	      return;
	    }

	case CPP_OPEN_BRACE:
	  ++nesting_depth;
	  break;

	default:
	  break;
	}

      /* Consume the token.  */
      cp_lexer_consume_token (parser->lexer);
    }
}

/* This function is called at the end of a statement or declaration.
   If the next token is a semicolon, it is consumed; otherwise, error
   recovery is attempted.  */

static void
cp_parser_consume_semicolon_at_end_of_statement (cp_parser *parser)
{
  /* Look for the trailing `;'.  */
  if (!cp_parser_require (parser, CPP_SEMICOLON, "%<;%>"))
    {
      /* If there is additional (erroneous) input, skip to the end of
	 the statement.  */
      cp_parser_skip_to_end_of_statement (parser);
      /* If the next token is now a `;', consume it.  */
      if (cp_lexer_next_token_is (parser->lexer, CPP_SEMICOLON))
	cp_lexer_consume_token (parser->lexer);
    }
}

/* Skip tokens until we have consumed an entire block, or until we
   have consumed a non-nested `;'.  */

static void
cp_parser_skip_to_end_of_block_or_statement (cp_parser* parser)
{
  int nesting_depth = 0;

  while (nesting_depth >= 0)
    {
      cp_token *token = cp_lexer_peek_token (parser->lexer);

      switch (token->type)
	{
	case CPP_EOF:
	case CPP_PRAGMA_EOL:
	  /* If we've run out of tokens, stop.  */
	  return;

	case CPP_SEMICOLON:
	  /* Stop if this is an unnested ';'. */
	  if (!nesting_depth)
	    nesting_depth = -1;
	  break;

	case CPP_CLOSE_BRACE:
	  /* Stop if this is an unnested '}', or closes the outermost
	     nesting level.  */
	  nesting_depth--;
	  if (nesting_depth < 0)
	    return;
	  if (!nesting_depth)
	    nesting_depth = -1;
	  break;

	case CPP_OPEN_BRACE:
	  /* Nest. */
	  nesting_depth++;
	  break;

	default:
	  break;
	}

      /* Consume the token.  */
      cp_lexer_consume_token (parser->lexer);
    }
}

/* Skip tokens until a non-nested closing curly brace is the next
   token, or there are no more tokens. Return true in the first case,
   false otherwise.  */

static bool
cp_parser_skip_to_closing_brace (cp_parser *parser)
{
  unsigned nesting_depth = 0;

  while (true)
    {
      cp_token *token = cp_lexer_peek_token (parser->lexer);

      switch (token->type)
	{
	case CPP_EOF:
	case CPP_PRAGMA_EOL:
	  /* If we've run out of tokens, stop.  */
	  return false;

	case CPP_CLOSE_BRACE:
	  /* If the next token is a non-nested `}', then we have reached
	     the end of the current block.  */
	  if (nesting_depth-- == 0)
	    return true;
	  break;

	case CPP_OPEN_BRACE:
	  /* If it the next token is a `{', then we are entering a new
	     block.  Consume the entire block.  */
	  ++nesting_depth;
	  break;

	default:
	  break;
	}

      /* Consume the token.  */
      cp_lexer_consume_token (parser->lexer);
    }
}

/* Consume tokens until we reach the end of the pragma.  The PRAGMA_TOK
   parameter is the PRAGMA token, allowing us to purge the entire pragma
   sequence.  */

static void
cp_parser_skip_to_pragma_eol (cp_parser* parser, cp_token *pragma_tok)
{
  cp_token *token;

  parser->lexer->in_pragma = false;

  do
    token = cp_lexer_consume_token (parser->lexer);
  while (token->type != CPP_PRAGMA_EOL && token->type != CPP_EOF);

  /* Ensure that the pragma is not parsed again.  */
  cp_lexer_purge_tokens_after (parser->lexer, pragma_tok);
}

/* Require pragma end of line, resyncing with it as necessary.  The
   arguments are as for cp_parser_skip_to_pragma_eol.  */

static void
cp_parser_require_pragma_eol (cp_parser *parser, cp_token *pragma_tok)
{
  parser->lexer->in_pragma = false;
  if (!cp_parser_require (parser, CPP_PRAGMA_EOL, "end of line"))
    cp_parser_skip_to_pragma_eol (parser, pragma_tok);
}

/* This is a simple wrapper around make_typename_type. When the id is
   an unresolved identifier node, we can provide a superior diagnostic
   using cp_parser_diagnose_invalid_type_name.  */

static tree
cp_parser_make_typename_type (cp_parser *parser, tree scope,
			      tree id, location_t id_location)
{
  tree result;
  if (TREE_CODE (id) == IDENTIFIER_NODE)
    {
      result = make_typename_type (scope, id, typename_type,
				   /*complain=*/tf_none);
      if (result == error_mark_node)
	cp_parser_diagnose_invalid_type_name (parser, scope, id, id_location);
      return result;
    }
  return make_typename_type (scope, id, typename_type, tf_error);
}

/* This is a wrapper around the
   make_{pointer,ptrmem,reference}_declarator functions that decides
   which one to call based on the CODE and CLASS_TYPE arguments. The
   CODE argument should be one of the values returned by
   cp_parser_ptr_operator. */
static cp_declarator *
cp_parser_make_indirect_declarator (enum tree_code code, tree class_type,
				    cp_cv_quals cv_qualifiers,
				    cp_declarator *target)
{
  if (code == ERROR_MARK)
    return cp_error_declarator;

  if (code == INDIRECT_REF)
    if (class_type == NULL_TREE)
      return make_pointer_declarator (cv_qualifiers, target);
    else
      return make_ptrmem_declarator (cv_qualifiers, class_type, target);
  else if (code == ADDR_EXPR && class_type == NULL_TREE)
    return make_reference_declarator (cv_qualifiers, target, false);
  else if (code == NON_LVALUE_EXPR && class_type == NULL_TREE)
    return make_reference_declarator (cv_qualifiers, target, true);
  gcc_unreachable ();
}

/* Create a new C++ parser.  */

static cp_parser *
cp_parser_new (void)
{
  cp_parser *parser;
  cp_lexer *lexer;
  unsigned i;

  /* cp_lexer_new_main is called before calling ggc_alloc because
     cp_lexer_new_main might load a PCH file.  */
  lexer = cp_lexer_new_main ();

  /* Initialize the binops_by_token so that we can get the tree
     directly from the token.  */
  for (i = 0; i < sizeof (binops) / sizeof (binops[0]); i++)
    binops_by_token[binops[i].token_type] = binops[i];

  parser = GGC_CNEW (cp_parser);
  parser->lexer = lexer;
  parser->context = cp_parser_context_new (NULL);

  /* For now, we always accept GNU extensions.  */
  parser->allow_gnu_extensions_p = 1;

  /* The `>' token is a greater-than operator, not the end of a
     template-id.  */
  parser->greater_than_is_operator_p = true;

  parser->default_arg_ok_p = true;

  /* We are not parsing a constant-expression.  */
  parser->integral_constant_expression_p = false;
  parser->allow_non_integral_constant_expression_p = false;
  parser->non_integral_constant_expression_p = false;

  /* Local variable names are not forbidden.  */
  parser->local_variables_forbidden_p = false;

  /* We are not processing an `extern "C"' declaration.  */
  parser->in_unbraced_linkage_specification_p = false;

  /* We are not processing a declarator.  */
  parser->in_declarator_p = false;

  /* We are not processing a template-argument-list.  */
  parser->in_template_argument_list_p = false;

  /* We are not in an iteration statement.  */
  parser->in_statement = 0;

  /* We are not in a switch statement.  */
  parser->in_switch_statement_p = false;

  /* We are not parsing a type-id inside an expression.  */
  parser->in_type_id_in_expr_p = false;

  /* Declarations aren't implicitly extern "C".  */
  parser->implicit_extern_c = false;

  /* String literals should be translated to the execution character set.  */
  parser->translate_strings_p = true;

  /* We are not parsing a function body.  */
  parser->in_function_body = false;

  /* The unparsed function queue is empty.  */
  parser->unparsed_functions_queues = build_tree_list (NULL_TREE, NULL_TREE);

  /* There are no classes being defined.  */
  parser->num_classes_being_defined = 0;

  /* No template parameters apply.  */
  parser->num_template_parameter_lists = 0;

  return parser;
}

/* Create a cp_lexer structure which will emit the tokens in CACHE
   and push it onto the parser's lexer stack.  This is used for delayed
   parsing of in-class method bodies and default arguments, and should
   not be confused with tentative parsing.  */
static void
cp_parser_push_lexer_for_tokens (cp_parser *parser, cp_token_cache *cache)
{
  cp_lexer *lexer = cp_lexer_new_from_tokens (cache);
  lexer->next = parser->lexer;
  parser->lexer = lexer;

  /* Move the current source position to that of the first token in the
     new lexer.  */
  cp_lexer_set_source_position_from_token (lexer->next_token);
}

/* Pop the top lexer off the parser stack.  This is never used for the
   "main" lexer, only for those pushed by cp_parser_push_lexer_for_tokens.  */
static void
cp_parser_pop_lexer (cp_parser *parser)
{
  cp_lexer *lexer = parser->lexer;
  parser->lexer = lexer->next;
  cp_lexer_destroy (lexer);

  /* Put the current source position back where it was before this
     lexer was pushed.  */
  cp_lexer_set_source_position_from_token (parser->lexer->next_token);
}

/* Lexical conventions [gram.lex]  */

/* Parse an identifier.  Returns an IDENTIFIER_NODE representing the
   identifier.  */

static tree
cp_parser_identifier (cp_parser* parser)
{
  cp_token *token;

  /* Look for the identifier.  */
  token = cp_parser_require (parser, CPP_NAME, "identifier");
  /* Return the value.  */
  return token ? token->u.value : error_mark_node;
}

/* Parse a sequence of adjacent string constants.  Returns a
   TREE_STRING representing the combined, nul-terminated string
   constant.  If TRANSLATE is true, translate the string to the
   execution character set.  If WIDE_OK is true, a wide string is
   invalid here.

   C++98 [lex.string] says that if a narrow string literal token is
   adjacent to a wide string literal token, the behavior is undefined.
   However, C99 6.4.5p4 says that this results in a wide string literal.
   We follow C99 here, for consistency with the C front end.

   This code is largely lifted from lex_string() in c-lex.c.

   FUTURE: ObjC++ will need to handle @-strings here.  */
static tree
cp_parser_string_literal (cp_parser *parser, bool translate, bool wide_ok)
{
  tree value;
  size_t count;
  struct obstack str_ob;
  cpp_string str, istr, *strs;
  cp_token *tok;
  enum cpp_ttype type;

  tok = cp_lexer_peek_token (parser->lexer);
  if (!cp_parser_is_string_literal (tok))
    {
      cp_parser_error (parser, "expected string-literal");
      return error_mark_node;
    }

  type = tok->type;

  /* Try to avoid the overhead of creating and destroying an obstack
     for the common case of just one string.  */
  if (!cp_parser_is_string_literal
      (cp_lexer_peek_nth_token (parser->lexer, 2)))
    {
      cp_lexer_consume_token (parser->lexer);

      str.text = (const unsigned char *)TREE_STRING_POINTER (tok->u.value);
      str.len = TREE_STRING_LENGTH (tok->u.value);
      count = 1;

      strs = &str;
    }
  else
    {
      gcc_obstack_init (&str_ob);
      count = 0;

      do
	{
	  cp_lexer_consume_token (parser->lexer);
	  count++;
	  str.text = (const unsigned char *)TREE_STRING_POINTER (tok->u.value);
	  str.len = TREE_STRING_LENGTH (tok->u.value);

	  if (type != tok->type)
	    {
	      if (type == CPP_STRING)
		type = tok->type;
	      else if (tok->type != CPP_STRING)
		error_at (tok->location,
			  "unsupported non-standard concatenation "
			  "of string literals");
	    }

	  obstack_grow (&str_ob, &str, sizeof (cpp_string));

	  tok = cp_lexer_peek_token (parser->lexer);
	}
      while (cp_parser_is_string_literal (tok));

      strs = (cpp_string *) obstack_finish (&str_ob);
    }

  if (type != CPP_STRING && !wide_ok)
    {
      cp_parser_error (parser, "a wide string is invalid in this context");
      type = CPP_STRING;
    }

  if ((translate ? cpp_interpret_string : cpp_interpret_string_notranslate)
      (parse_in, strs, count, &istr, type))
    {
      value = build_string (istr.len, (const char *)istr.text);
      free (CONST_CAST (unsigned char *, istr.text));

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
	  break;
	}

      value = fix_string_type (value);
    }
  else
    /* cpp_interpret_string has issued an error.  */
    value = error_mark_node;

  if (count > 1)
    obstack_free (&str_ob, 0);

  return value;
}


/* Basic concepts [gram.basic]  */

/* Parse a translation-unit.

   translation-unit:
     declaration-seq [opt]

   Returns TRUE if all went well.  */

static bool
cp_parser_translation_unit (cp_parser* parser)
{
  /* The address of the first non-permanent object on the declarator
     obstack.  */
  static void *declarator_obstack_base;

  bool success;

  /* Create the declarator obstack, if necessary.  */
  if (!cp_error_declarator)
    {
      gcc_obstack_init (&declarator_obstack);
      /* Create the error declarator.  */
      cp_error_declarator = make_declarator (cdk_error);
      /* Create the empty parameter list.  */
      no_parameters = make_parameter_declarator (NULL, NULL, NULL_TREE);
      /* Remember where the base of the declarator obstack lies.  */
      declarator_obstack_base = obstack_next_free (&declarator_obstack);
    }

  cp_parser_declaration_seq_opt (parser);

  /* If there are no tokens left then all went well.  */
  if (cp_lexer_next_token_is (parser->lexer, CPP_EOF))
    {
      /* Get rid of the token array; we don't need it any more.  */
      cp_lexer_destroy (parser->lexer);
      parser->lexer = NULL;

      /* This file might have been a context that's implicitly extern
	 "C".  If so, pop the lang context.  (Only relevant for PCH.) */
      if (parser->implicit_extern_c)
	{
	  pop_lang_context ();
	  parser->implicit_extern_c = false;
	}

      /* Finish up.  */
      finish_translation_unit ();

      success = true;
    }
  else
    {
      cp_parser_error (parser, "expected declaration");
      success = false;
    }

  /* Make sure the declarator obstack was fully cleaned up.  */
  gcc_assert (obstack_next_free (&declarator_obstack)
	      == declarator_obstack_base);

  /* All went well.  */
  return success;
}

/* Expressions [gram.expr] */

/* Parse a primary-expression.

   primary-expression:
     literal
     this
     ( expression )
     id-expression

   GNU Extensions:

   primary-expression:
     ( compound-statement )
     __builtin_va_arg ( assignment-expression , type-id )
     __builtin_offsetof ( type-id , offsetof-expression )

   C++ Extensions:
     __has_nothrow_assign ( type-id )   
     __has_nothrow_constructor ( type-id )
     __has_nothrow_copy ( type-id )
     __has_trivial_assign ( type-id )   
     __has_trivial_constructor ( type-id )
     __has_trivial_copy ( type-id )
     __has_trivial_destructor ( type-id )
     __has_virtual_destructor ( type-id )     
     __is_abstract ( type-id )
     __is_base_of ( type-id , type-id )
     __is_class ( type-id )
     __is_convertible_to ( type-id , type-id )     
     __is_empty ( type-id )
     __is_enum ( type-id )
     __is_pod ( type-id )
     __is_polymorphic ( type-id )
     __is_union ( type-id )

   Objective-C++ Extension:

   primary-expression:
     objc-expression

   literal:
     __null

   ADDRESS_P is true iff this expression was immediately preceded by
   "&" and therefore might denote a pointer-to-member.  CAST_P is true
   iff this expression is the target of a cast.  TEMPLATE_ARG_P is
   true iff this expression is a template argument.

   Returns a representation of the expression.  Upon return, *IDK
   indicates what kind of id-expression (if any) was present.  */

static tree
cp_parser_primary_expression (cp_parser *parser,
			      bool address_p,
			      bool cast_p,
			      bool template_arg_p,
			      cp_id_kind *idk)
{
  cp_token *token = NULL;

  /* Assume the primary expression is not an id-expression.  */
  *idk = CP_ID_KIND_NONE;

  /* Peek at the next token.  */
  token = cp_lexer_peek_token (parser->lexer);
  switch (token->type)
    {
      /* literal:
	   integer-literal
	   character-literal
	   floating-literal
	   string-literal
	   boolean-literal  */
    case CPP_CHAR:
    case CPP_CHAR16:
    case CPP_CHAR32:
    case CPP_WCHAR:
    case CPP_NUMBER:
      token = cp_lexer_consume_token (parser->lexer);
      if (TREE_CODE (token->u.value) == FIXED_CST)
	{
	  error_at (token->location,
		    "fixed-point types not supported in C++");
	  return error_mark_node;
	}
      /* Floating-point literals are only allowed in an integral
	 constant expression if they are cast to an integral or
	 enumeration type.  */
      if (TREE_CODE (token->u.value) == REAL_CST
	  && parser->integral_constant_expression_p
	  && pedantic)
	{
	  /* CAST_P will be set even in invalid code like "int(2.7 +
	     ...)".   Therefore, we have to check that the next token
	     is sure to end the cast.  */
	  if (cast_p)
	    {
	      cp_token *next_token;

	      next_token = cp_lexer_peek_token (parser->lexer);
	      if (/* The comma at the end of an
		     enumerator-definition.  */
		  next_token->type != CPP_COMMA
		  /* The curly brace at the end of an enum-specifier.  */
		  && next_token->type != CPP_CLOSE_BRACE
		  /* The end of a statement.  */
		  && next_token->type != CPP_SEMICOLON
		  /* The end of the cast-expression.  */
		  && next_token->type != CPP_CLOSE_PAREN
		  /* The end of an array bound.  */
		  && next_token->type != CPP_CLOSE_SQUARE
		  /* The closing ">" in a template-argument-list.  */
		  && (next_token->type != CPP_GREATER
		      || parser->greater_than_is_operator_p)
		  /* C++0x only: A ">>" treated like two ">" tokens,
                     in a template-argument-list.  */
		  && (next_token->type != CPP_RSHIFT
                      || (cxx_dialect == cxx98)
		      || parser->greater_than_is_operator_p))
		cast_p = false;
	    }

	  /* If we are within a cast, then the constraint that the
	     cast is to an integral or enumeration type will be
	     checked at that point.  If we are not within a cast, then
	     this code is invalid.  */
	  if (!cast_p)
	    cp_parser_non_integral_constant_expression
	      (parser, "floating-point literal");
	}
      return token->u.value;

    case CPP_STRING:
    case CPP_STRING16:
    case CPP_STRING32:
    case CPP_WSTRING:
    case CPP_UTF8STRING:
      /* ??? Should wide strings be allowed when parser->translate_strings_p
	 is false (i.e. in attributes)?  If not, we can kill the third
	 argument to cp_parser_string_literal.  */
      return cp_parser_string_literal (parser,
				       parser->translate_strings_p,
				       true);

    case CPP_OPEN_PAREN:
      {
	tree expr;
	bool saved_greater_than_is_operator_p;

	/* Consume the `('.  */
	cp_lexer_consume_token (parser->lexer);
	/* Within a parenthesized expression, a `>' token is always
	   the greater-than operator.  */
	saved_greater_than_is_operator_p
	  = parser->greater_than_is_operator_p;
	parser->greater_than_is_operator_p = true;
	/* If we see `( { ' then we are looking at the beginning of
	   a GNU statement-expression.  */
	if (cp_parser_allow_gnu_extensions_p (parser)
	    && cp_lexer_next_token_is (parser->lexer, CPP_OPEN_BRACE))
	  {
	    /* Statement-expressions are not allowed by the standard.  */
	    pedwarn (token->location, OPT_pedantic, 
		     "ISO C++ forbids braced-groups within expressions");

	    /* And they're not allowed outside of a function-body; you
	       cannot, for example, write:

		 int i = ({ int j = 3; j + 1; });

	       at class or namespace scope.  */
	    if (!parser->in_function_body
		|| parser->in_template_argument_list_p)
	      {
		error_at (token->location,
			  "statement-expressions are not allowed outside "
			  "functions nor in template-argument lists");
		cp_parser_skip_to_end_of_block_or_statement (parser);
		expr = error_mark_node;
	      }
	    else
	      {
		/* Start the statement-expression.  */
		expr = begin_stmt_expr ();
		/* Parse the compound-statement.  */
		cp_parser_compound_statement (parser, expr, false);
		/* Finish up.  */
		expr = finish_stmt_expr (expr, false);
	      }
	  }
	else
	  {
	    /* Parse the parenthesized expression.  */
	    expr = cp_parser_expression (parser, cast_p, idk);
	    /* Let the front end know that this expression was
	       enclosed in parentheses. This matters in case, for
	       example, the expression is of the form `A::B', since
	       `&A::B' might be a pointer-to-member, but `&(A::B)' is
	       not.  */
	    finish_parenthesized_expr (expr);
	  }
	/* The `>' token might be the end of a template-id or
	   template-parameter-list now.  */
	parser->greater_than_is_operator_p
	  = saved_greater_than_is_operator_p;
	/* Consume the `)'.  */
	if (!cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>"))
	  cp_parser_skip_to_end_of_statement (parser);

	return expr;
      }

    case CPP_OPEN_SQUARE:
      if (c_dialect_objc ())
        /* We have an Objective-C++ message. */
        return cp_parser_objc_expression (parser);
      maybe_warn_cpp0x (CPP0X_LAMBDA_EXPR);
      return cp_parser_lambda_expression (parser);

    case CPP_OBJC_STRING:
      if (c_dialect_objc ())
	/* We have an Objective-C++ string literal. */
        return cp_parser_objc_expression (parser);
      cp_parser_error (parser, "expected primary-expression");
      return error_mark_node;

    case CPP_KEYWORD:
      switch (token->keyword)
	{
	  /* These two are the boolean literals.  */
	case RID_TRUE:
	  cp_lexer_consume_token (parser->lexer);
	  return boolean_true_node;
	case RID_FALSE:
	  cp_lexer_consume_token (parser->lexer);
	  return boolean_false_node;

	  /* The `__null' literal.  */
	case RID_NULL:
	  cp_lexer_consume_token (parser->lexer);
	  return null_node;

	  /* Recognize the `this' keyword.  */
	case RID_THIS:
	  cp_lexer_consume_token (parser->lexer);
	  if (parser->local_variables_forbidden_p)
	    {
	      error_at (token->location,
			"%<this%> may not be used in this context");
	      return error_mark_node;
	    }
	  /* Pointers cannot appear in constant-expressions.  */
	  if (cp_parser_non_integral_constant_expression (parser, "%<this%>"))
	    return error_mark_node;
	  return finish_this_expr ();

	  /* The `operator' keyword can be the beginning of an
	     id-expression.  */
	case RID_OPERATOR:
	  goto id_expression;

	case RID_FUNCTION_NAME:
	case RID_PRETTY_FUNCTION_NAME:
	case RID_C99_FUNCTION_NAME:
	  {
	    const char *name;

	    /* The symbols __FUNCTION__, __PRETTY_FUNCTION__, and
	       __func__ are the names of variables -- but they are
	       treated specially.  Therefore, they are handled here,
	       rather than relying on the generic id-expression logic
	       below.  Grammatically, these names are id-expressions.

	       Consume the token.  */
	    token = cp_lexer_consume_token (parser->lexer);

	    switch (token->keyword)
	      {
	      case RID_FUNCTION_NAME:
		name = "%<__FUNCTION__%>";
		break;
	      case RID_PRETTY_FUNCTION_NAME:
		name = "%<__PRETTY_FUNCTION__%>";
		break;
	      case RID_C99_FUNCTION_NAME:
		name = "%<__func__%>";
		break;
	      default:
		gcc_unreachable ();
	      }

	    if (cp_parser_non_integral_constant_expression (parser, name))
	      return error_mark_node;

	    /* Look up the name.  */
	    return finish_fname (token->u.value);
	  }

	case RID_VA_ARG:
	  {
	    tree expression;
	    tree type;

	    /* The `__builtin_va_arg' construct is used to handle
	       `va_arg'.  Consume the `__builtin_va_arg' token.  */
	    cp_lexer_consume_token (parser->lexer);
	    /* Look for the opening `('.  */
	    cp_parser_require (parser, CPP_OPEN_PAREN, "%<(%>");
	    /* Now, parse the assignment-expression.  */
	    expression = cp_parser_assignment_expression (parser,
							  /*cast_p=*/false, NULL);
	    /* Look for the `,'.  */
	    cp_parser_require (parser, CPP_COMMA, "%<,%>");
	    /* Parse the type-id.  */
	    type = cp_parser_type_id (parser);
	    /* Look for the closing `)'.  */
	    cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>");
	    /* Using `va_arg' in a constant-expression is not
	       allowed.  */
	    if (cp_parser_non_integral_constant_expression (parser,
							    "%<va_arg%>"))
	      return error_mark_node;
	    return build_x_va_arg (expression, type);
	  }

	case RID_OFFSETOF:
	  return cp_parser_builtin_offsetof (parser);

	case RID_HAS_NOTHROW_ASSIGN:
	case RID_HAS_NOTHROW_CONSTRUCTOR:
	case RID_HAS_NOTHROW_COPY:	  
	case RID_HAS_TRIVIAL_ASSIGN:
	case RID_HAS_TRIVIAL_CONSTRUCTOR:
	case RID_HAS_TRIVIAL_COPY:	  
	case RID_HAS_TRIVIAL_DESTRUCTOR:
	case RID_HAS_VIRTUAL_DESTRUCTOR:
	case RID_IS_ABSTRACT:
	case RID_IS_BASE_OF:
	case RID_IS_CLASS:
	case RID_IS_CONVERTIBLE_TO:
	case RID_IS_EMPTY:
	case RID_IS_ENUM:
	case RID_IS_POD:
	case RID_IS_POLYMORPHIC:
	case RID_IS_STD_LAYOUT:
	case RID_IS_TRIVIAL:
	case RID_IS_UNION:
	  return cp_parser_trait_expr (parser, token->keyword);

	/* Objective-C++ expressions.  */
	case RID_AT_ENCODE:
	case RID_AT_PROTOCOL:
	case RID_AT_SELECTOR:
	  return cp_parser_objc_expression (parser);

	default:
	  cp_parser_error (parser, "expected primary-expression");
	  return error_mark_node;
	}

      /* An id-expression can start with either an identifier, a
	 `::' as the beginning of a qualified-id, or the "operator"
	 keyword.  */
    case CPP_NAME:
    case CPP_SCOPE:
    case CPP_TEMPLATE_ID:
    case CPP_NESTED_NAME_SPECIFIER:
      {
	tree id_expression;
	tree decl;
	const char *error_msg;
	bool template_p;
	bool done;
	cp_token *id_expr_token;

      id_expression:
	/* Parse the id-expression.  */
	id_expression
	  = cp_parser_id_expression (parser,
				     /*template_keyword_p=*/false,
				     /*check_dependency_p=*/true,
				     &template_p,
				     /*declarator_p=*/false,
				     /*optional_p=*/false);
	if (id_expression == error_mark_node)
	  return error_mark_node;
	id_expr_token = token;
	token = cp_lexer_peek_token (parser->lexer);
	done = (token->type != CPP_OPEN_SQUARE
		&& token->type != CPP_OPEN_PAREN
		&& token->type != CPP_DOT
		&& token->type != CPP_DEREF
		&& token->type != CPP_PLUS_PLUS
		&& token->type != CPP_MINUS_MINUS);
	/* If we have a template-id, then no further lookup is
	   required.  If the template-id was for a template-class, we
	   will sometimes have a TYPE_DECL at this point.  */
	if (TREE_CODE (id_expression) == TEMPLATE_ID_EXPR
		 || TREE_CODE (id_expression) == TYPE_DECL)
	  decl = id_expression;
	/* Look up the name.  */
	else
	  {
	    tree ambiguous_decls;

	    /* If we already know that this lookup is ambiguous, then
	       we've already issued an error message; there's no reason
	       to check again.  */
	    if (id_expr_token->type == CPP_NAME
		&& id_expr_token->ambiguous_p)
	      {
		cp_parser_simulate_error (parser);
		return error_mark_node;
	      }

	    decl = cp_parser_lookup_name (parser, id_expression,
					  none_type,
					  template_p,
					  /*is_namespace=*/false,
					  /*check_dependency=*/true,
					  &ambiguous_decls,
					  id_expr_token->location);
	    /* If the lookup was ambiguous, an error will already have
	       been issued.  */
	    if (ambiguous_decls)
	      return error_mark_node;

	    /* In Objective-C++, an instance variable (ivar) may be preferred
	       to whatever cp_parser_lookup_name() found.  */
	    decl = objc_lookup_ivar (decl, id_expression);

	    /* If name lookup gives us a SCOPE_REF, then the
	       qualifying scope was dependent.  */
	    if (TREE_CODE (decl) == SCOPE_REF)
	      {
		/* At this point, we do not know if DECL is a valid
		   integral constant expression.  We assume that it is
		   in fact such an expression, so that code like:

		      template <int N> struct A {
			int a[B<N>::i];
		      };
		     
		   is accepted.  At template-instantiation time, we
		   will check that B<N>::i is actually a constant.  */
		return decl;
	      }
	    /* Check to see if DECL is a local variable in a context
	       where that is forbidden.  */
	    if (parser->local_variables_forbidden_p
		&& local_variable_p (decl))
	      {
		/* It might be that we only found DECL because we are
		   trying to be generous with pre-ISO scoping rules.
		   For example, consider:

		     int i;
		     void g() {
		       for (int i = 0; i < 10; ++i) {}
		       extern void f(int j = i);
		     }

		   Here, name look up will originally find the out
		   of scope `i'.  We need to issue a warning message,
		   but then use the global `i'.  */
		decl = check_for_out_of_scope_variable (decl);
		if (local_variable_p (decl))
		  {
		    error_at (id_expr_token->location,
			      "local variable %qD may not appear in this context",
			      decl);
		    return error_mark_node;
		  }
	      }
	  }

	decl = (finish_id_expression
		(id_expression, decl, parser->scope,
		 idk,
		 parser->integral_constant_expression_p,
		 parser->allow_non_integral_constant_expression_p,
		 &parser->non_integral_constant_expression_p,
		 template_p, done, address_p,
		 template_arg_p,
		 &error_msg,
                 id_expr_token->location));
	if (error_msg)
	  cp_parser_error (parser, error_msg);
	return decl;
      }

      /* Anything else is an error.  */
    default:
      cp_parser_error (parser, "expected primary-expression");
      return error_mark_node;
    }
}

/* Parse an id-expression.

   id-expression:
     unqualified-id
     qualified-id

   qualified-id:
     :: [opt] nested-name-specifier template [opt] unqualified-id
     :: identifier
     :: operator-function-id
     :: template-id

   Return a representation of the unqualified portion of the
   identifier.  Sets PARSER->SCOPE to the qualifying scope if there is
   a `::' or nested-name-specifier.

   Often, if the id-expression was a qualified-id, the caller will
   want to make a SCOPE_REF to represent the qualified-id.  This
   function does not do this in order to avoid wastefully creating
   SCOPE_REFs when they are not required.

   If TEMPLATE_KEYWORD_P is true, then we have just seen the
   `template' keyword.

   If CHECK_DEPENDENCY_P is false, then names are looked up inside
   uninstantiated templates.

   If *TEMPLATE_P is non-NULL, it is set to true iff the
   `template' keyword is used to explicitly indicate that the entity
   named is a template.

   If DECLARATOR_P is true, the id-expression is appearing as part of
   a declarator, rather than as part of an expression.  */

static tree
cp_parser_id_expression (cp_parser *parser,
			 bool template_keyword_p,
			 bool check_dependency_p,
			 bool *template_p,
			 bool declarator_p,
			 bool optional_p)
{
  bool global_scope_p;
  bool nested_name_specifier_p;

  /* Assume the `template' keyword was not used.  */
  if (template_p)
    *template_p = template_keyword_p;

  /* Look for the optional `::' operator.  */
  global_scope_p
    = (cp_parser_global_scope_opt (parser, /*current_scope_valid_p=*/false)
       != NULL_TREE);
  /* Look for the optional nested-name-specifier.  */
  nested_name_specifier_p
    = (cp_parser_nested_name_specifier_opt (parser,
					    /*typename_keyword_p=*/false,
					    check_dependency_p,
					    /*type_p=*/false,
					    declarator_p)
       != NULL_TREE);
  /* If there is a nested-name-specifier, then we are looking at
     the first qualified-id production.  */
  if (nested_name_specifier_p)
    {
      tree saved_scope;
      tree saved_object_scope;
      tree saved_qualifying_scope;
      tree unqualified_id;
      bool is_template;

      /* See if the next token is the `template' keyword.  */
      if (!template_p)
	template_p = &is_template;
      *template_p = cp_parser_optional_template_keyword (parser);
      /* Name lookup we do during the processing of the
	 unqualified-id might obliterate SCOPE.  */
      saved_scope = parser->scope;
      saved_object_scope = parser->object_scope;
      saved_qualifying_scope = parser->qualifying_scope;
      /* Process the final unqualified-id.  */
      unqualified_id = cp_parser_unqualified_id (parser, *template_p,
						 check_dependency_p,
						 declarator_p,
						 /*optional_p=*/false);
      /* Restore the SAVED_SCOPE for our caller.  */
      parser->scope = saved_scope;
      parser->object_scope = saved_object_scope;
      parser->qualifying_scope = saved_qualifying_scope;

      return unqualified_id;
    }
  /* Otherwise, if we are in global scope, then we are looking at one
     of the other qualified-id productions.  */
  else if (global_scope_p)
    {
      cp_token *token;
      tree id;

      /* Peek at the next token.  */
      token = cp_lexer_peek_token (parser->lexer);

      /* If it's an identifier, and the next token is not a "<", then
	 we can avoid the template-id case.  This is an optimization
	 for this common case.  */
      if (token->type == CPP_NAME
	  && !cp_parser_nth_token_starts_template_argument_list_p
	       (parser, 2))
	return cp_parser_identifier (parser);

      cp_parser_parse_tentatively (parser);
      /* Try a template-id.  */
      id = cp_parser_template_id (parser,
				  /*template_keyword_p=*/false,
				  /*check_dependency_p=*/true,
				  declarator_p);
      /* If that worked, we're done.  */
      if (cp_parser_parse_definitely (parser))
	return id;

      /* Peek at the next token.  (Changes in the token buffer may
	 have invalidated the pointer obtained above.)  */
      token = cp_lexer_peek_token (parser->lexer);

      switch (token->type)
	{
	case CPP_NAME:
	  return cp_parser_identifier (parser);

	case CPP_KEYWORD:
	  if (token->keyword == RID_OPERATOR)
	    return cp_parser_operator_function_id (parser);
	  /* Fall through.  */

	default:
	  cp_parser_error (parser, "expected id-expression");
	  return error_mark_node;
	}
    }
  else
    return cp_parser_unqualified_id (parser, template_keyword_p,
				     /*check_dependency_p=*/true,
				     declarator_p,
				     optional_p);
}

/* Parse an unqualified-id.

   unqualified-id:
     identifier
     operator-function-id
     conversion-function-id
     ~ class-name
     template-id

   If TEMPLATE_KEYWORD_P is TRUE, we have just seen the `template'
   keyword, in a construct like `A::template ...'.

   Returns a representation of unqualified-id.  For the `identifier'
   production, an IDENTIFIER_NODE is returned.  For the `~ class-name'
   production a BIT_NOT_EXPR is returned; the operand of the
   BIT_NOT_EXPR is an IDENTIFIER_NODE for the class-name.  For the
   other productions, see the documentation accompanying the
   corresponding parsing functions.  If CHECK_DEPENDENCY_P is false,
   names are looked up in uninstantiated templates.  If DECLARATOR_P
   is true, the unqualified-id is appearing as part of a declarator,
   rather than as part of an expression.  */

static tree
cp_parser_unqualified_id (cp_parser* parser,
			  bool template_keyword_p,
			  bool check_dependency_p,
			  bool declarator_p,
			  bool optional_p)
{
  cp_token *token;

  /* Peek at the next token.  */
  token = cp_lexer_peek_token (parser->lexer);

  switch (token->type)
    {
    case CPP_NAME:
      {
	tree id;

	/* We don't know yet whether or not this will be a
	   template-id.  */
	cp_parser_parse_tentatively (parser);
	/* Try a template-id.  */
	id = cp_parser_template_id (parser, template_keyword_p,
				    check_dependency_p,
				    declarator_p);
	/* If it worked, we're done.  */
	if (cp_parser_parse_definitely (parser))
	  return id;
	/* Otherwise, it's an ordinary identifier.  */
	return cp_parser_identifier (parser);
      }

    case CPP_TEMPLATE_ID:
      return cp_parser_template_id (parser, template_keyword_p,
				    check_dependency_p,
				    declarator_p);

    case CPP_COMPL:
      {
	tree type_decl;
	tree qualifying_scope;
	tree object_scope;
	tree scope;
	bool done;

	/* Consume the `~' token.  */
	cp_lexer_consume_token (parser->lexer);
	/* Parse the class-name.  The standard, as written, seems to
	   say that:

	     template <typename T> struct S { ~S (); };
	     template <typename T> S<T>::~S() {}

	   is invalid, since `~' must be followed by a class-name, but
	   `S<T>' is dependent, and so not known to be a class.
	   That's not right; we need to look in uninstantiated
	   templates.  A further complication arises from:

	     template <typename T> void f(T t) {
	       t.T::~T();
	     }

	   Here, it is not possible to look up `T' in the scope of `T'
	   itself.  We must look in both the current scope, and the
	   scope of the containing complete expression.

	   Yet another issue is:

	     struct S {
	       int S;
	       ~S();
	     };

	     S::~S() {}

	   The standard does not seem to say that the `S' in `~S'
	   should refer to the type `S' and not the data member
	   `S::S'.  */

	/* DR 244 says that we look up the name after the "~" in the
	   same scope as we looked up the qualifying name.  That idea
	   isn't fully worked out; it's more complicated than that.  */
	scope = parser->scope;
	object_scope = parser->object_scope;
	qualifying_scope = parser->qualifying_scope;

	/* Check for invalid scopes.  */
	if (scope == error_mark_node)
	  {
	    if (cp_lexer_next_token_is (parser->lexer, CPP_NAME))
	      cp_lexer_consume_token (parser->lexer);
	    return error_mark_node;
	  }
	if (scope && TREE_CODE (scope) == NAMESPACE_DECL)
	  {
	    if (!cp_parser_uncommitted_to_tentative_parse_p (parser))
	      error_at (token->location,
			"scope %qT before %<~%> is not a class-name",
			scope);
	    cp_parser_simulate_error (parser);
	    if (cp_lexer_next_token_is (parser->lexer, CPP_NAME))
	      cp_lexer_consume_token (parser->lexer);
	    return error_mark_node;
	  }
	gcc_assert (!scope || TYPE_P (scope));

	/* If the name is of the form "X::~X" it's OK.  */
	token = cp_lexer_peek_token (parser->lexer);
	if (scope
	    && token->type == CPP_NAME
	    && (cp_lexer_peek_nth_token (parser->lexer, 2)->type
		!= CPP_LESS)
	    && constructor_name_p (token->u.value, scope))
	  {
	    cp_lexer_consume_token (parser->lexer);
	    return build_nt (BIT_NOT_EXPR, scope);
	  }

	/* If there was an explicit qualification (S::~T), first look
	   in the scope given by the qualification (i.e., S).

	   Note: in the calls to cp_parser_class_name below we pass
	   typename_type so that lookup finds the injected-class-name
	   rather than the constructor.  */
	done = false;
	type_decl = NULL_TREE;
	if (scope)
	  {
	    cp_parser_parse_tentatively (parser);
	    type_decl = cp_parser_class_name (parser,
					      /*typename_keyword_p=*/false,
					      /*template_keyword_p=*/false,
					      typename_type,
					      /*check_dependency=*/false,
					      /*class_head_p=*/false,
					      declarator_p);
	    if (cp_parser_parse_definitely (parser))
	      done = true;
	  }
	/* In "N::S::~S", look in "N" as well.  */
	if (!done && scope && qualifying_scope)
	  {
	    cp_parser_parse_tentatively (parser);
	    parser->scope = qualifying_scope;
	    parser->object_scope = NULL_TREE;
	    parser->qualifying_scope = NULL_TREE;
	    type_decl
	      = cp_parser_class_name (parser,
				      /*typename_keyword_p=*/false,
				      /*template_keyword_p=*/false,
				      typename_type,
				      /*check_dependency=*/false,
				      /*class_head_p=*/false,
				      declarator_p);
	    if (cp_parser_parse_definitely (parser))
	      done = true;
	  }
	/* In "p->S::~T", look in the scope given by "*p" as well.  */
	else if (!done && object_scope)
	  {
	    cp_parser_parse_tentatively (parser);
	    parser->scope = object_scope;
	    parser->object_scope = NULL_TREE;
	    parser->qualifying_scope = NULL_TREE;
	    type_decl
	      = cp_parser_class_name (parser,
				      /*typename_keyword_p=*/false,
				      /*template_keyword_p=*/false,
				      typename_type,
				      /*check_dependency=*/false,
				      /*class_head_p=*/false,
				      declarator_p);
	    if (cp_parser_parse_definitely (parser))
	      done = true;
	  }
	/* Look in the surrounding context.  */
	if (!done)
	  {
	    parser->scope = NULL_TREE;
	    parser->object_scope = NULL_TREE;
	    parser->qualifying_scope = NULL_TREE;
	    if (processing_template_decl)
	      cp_parser_parse_tentatively (parser);
	    type_decl
	      = cp_parser_class_name (parser,
				      /*typename_keyword_p=*/false,
				      /*template_keyword_p=*/false,
				      typename_type,
				      /*check_dependency=*/false,
				      /*class_head_p=*/false,
				      declarator_p);
	    if (processing_template_decl
		&& ! cp_parser_parse_definitely (parser))
	      {
		/* We couldn't find a type with this name, so just accept
		   it and check for a match at instantiation time.  */
		type_decl = cp_parser_identifier (parser);
		if (type_decl != error_mark_node)
		  type_decl = build_nt (BIT_NOT_EXPR, type_decl);
		return type_decl;
	      }
	  }
	/* If an error occurred, assume that the name of the
	   destructor is the same as the name of the qualifying
	   class.  That allows us to keep parsing after running
	   into ill-formed destructor names.  */
	if (type_decl == error_mark_node && scope)
	  return build_nt (BIT_NOT_EXPR, scope);
	else if (type_decl == error_mark_node)
	  return error_mark_node;

	/* Check that destructor name and scope match.  */
	if (declarator_p && scope && !check_dtor_name (scope, type_decl))
	  {
	    if (!cp_parser_uncommitted_to_tentative_parse_p (parser))
	      error_at (token->location,
			"declaration of %<~%T%> as member of %qT",
			type_decl, scope);
	    cp_parser_simulate_error (parser);
	    return error_mark_node;
	  }

	/* [class.dtor]

	   A typedef-name that names a class shall not be used as the
	   identifier in the declarator for a destructor declaration.  */
	if (declarator_p
	    && !DECL_IMPLICIT_TYPEDEF_P (type_decl)
	    && !DECL_SELF_REFERENCE_P (type_decl)
	    && !cp_parser_uncommitted_to_tentative_parse_p (parser))
	  error_at (token->location,
		    "typedef-name %qD used as destructor declarator",
		    type_decl);

	return build_nt (BIT_NOT_EXPR, TREE_TYPE (type_decl));
      }

    case CPP_KEYWORD:
      if (token->keyword == RID_OPERATOR)
	{
	  tree id;

	  /* This could be a template-id, so we try that first.  */
	  cp_parser_parse_tentatively (parser);
	  /* Try a template-id.  */
	  id = cp_parser_template_id (parser, template_keyword_p,
				      /*check_dependency_p=*/true,
				      declarator_p);
	  /* If that worked, we're done.  */
	  if (cp_parser_parse_definitely (parser))
	    return id;
	  /* We still don't know whether we're looking at an
	     operator-function-id or a conversion-function-id.  */
	  cp_parser_parse_tentatively (parser);
	  /* Try an operator-function-id.  */
	  id = cp_parser_operator_function_id (parser);
	  /* If that didn't work, try a conversion-function-id.  */
	  if (!cp_parser_parse_definitely (parser))
	    id = cp_parser_conversion_function_id (parser);

	  return id;
	}
      /* Fall through.  */

    default:
      if (optional_p)
	return NULL_TREE;
      cp_parser_error (parser, "expected unqualified-id");
      return error_mark_node;
    }
}

/* Parse an (optional) nested-name-specifier.

   nested-name-specifier: [C++98]
     class-or-namespace-name :: nested-name-specifier [opt]
     class-or-namespace-name :: template nested-name-specifier [opt]

   nested-name-specifier: [C++0x]
     type-name ::
     namespace-name ::
     nested-name-specifier identifier ::
     nested-name-specifier template [opt] simple-template-id ::

   PARSER->SCOPE should be set appropriately before this function is
   called.  TYPENAME_KEYWORD_P is TRUE if the `typename' keyword is in
   effect.  TYPE_P is TRUE if we non-type bindings should be ignored
   in name lookups.

   Sets PARSER->SCOPE to the class (TYPE) or namespace
   (NAMESPACE_DECL) specified by the nested-name-specifier, or leaves
   it unchanged if there is no nested-name-specifier.  Returns the new
   scope iff there is a nested-name-specifier, or NULL_TREE otherwise.

   If IS_DECLARATION is TRUE, the nested-name-specifier is known to be
   part of a declaration and/or decl-specifier.  */

static tree
cp_parser_nested_name_specifier_opt (cp_parser *parser,
				     bool typename_keyword_p,
				     bool check_dependency_p,
				     bool type_p,
				     bool is_declaration)
{
  bool success = false;
  cp_token_position start = 0;
  cp_token *token;

  /* Remember where the nested-name-specifier starts.  */
  if (cp_parser_uncommitted_to_tentative_parse_p (parser))
    {
      start = cp_lexer_token_position (parser->lexer, false);
      push_deferring_access_checks (dk_deferred);
    }

  while (true)
    {
      tree new_scope;
      tree old_scope;
      tree saved_qualifying_scope;
      bool template_keyword_p;

      /* Spot cases that cannot be the beginning of a
	 nested-name-specifier.  */
      token = cp_lexer_peek_token (parser->lexer);

      /* If the next token is CPP_NESTED_NAME_SPECIFIER, just process
	 the already parsed nested-name-specifier.  */
      if (token->type == CPP_NESTED_NAME_SPECIFIER)
	{
	  /* Grab the nested-name-specifier and continue the loop.  */
	  cp_parser_pre_parsed_nested_name_specifier (parser);
	  /* If we originally encountered this nested-name-specifier
	     with IS_DECLARATION set to false, we will not have
	     resolved TYPENAME_TYPEs, so we must do so here.  */
	  if (is_declaration
	      && TREE_CODE (parser->scope) == TYPENAME_TYPE)
	    {
	      new_scope = resolve_typename_type (parser->scope,
						 /*only_current_p=*/false);
	      if (TREE_CODE (new_scope) != TYPENAME_TYPE)
		parser->scope = new_scope;
	    }
	  success = true;
	  continue;
	}

      /* Spot cases that cannot be the beginning of a
	 nested-name-specifier.  On the second and subsequent times
	 through the loop, we look for the `template' keyword.  */
      if (success && token->keyword == RID_TEMPLATE)
	;
      /* A template-id can start a nested-name-specifier.  */
      else if (token->type == CPP_TEMPLATE_ID)
	;
      else
	{
	  /* If the next token is not an identifier, then it is
	     definitely not a type-name or namespace-name.  */
	  if (token->type != CPP_NAME)
	    break;
	  /* If the following token is neither a `<' (to begin a
	     template-id), nor a `::', then we are not looking at a
	     nested-name-specifier.  */
	  token = cp_lexer_peek_nth_token (parser->lexer, 2);
	  if (token->type != CPP_SCOPE
	      && !cp_parser_nth_token_starts_template_argument_list_p
		  (parser, 2))
	    break;
	}

      /* The nested-name-specifier is optional, so we parse
	 tentatively.  */
      cp_parser_parse_tentatively (parser);

      /* Look for the optional `template' keyword, if this isn't the
	 first time through the loop.  */
      if (success)
	template_keyword_p = cp_parser_optional_template_keyword (parser);
      else
	template_keyword_p = false;

      /* Save the old scope since the name lookup we are about to do
	 might destroy it.  */
      old_scope = parser->scope;
      saved_qualifying_scope = parser->qualifying_scope;
      /* In a declarator-id like "X<T>::I::Y<T>" we must be able to
	 look up names in "X<T>::I" in order to determine that "Y" is
	 a template.  So, if we have a typename at this point, we make
	 an effort to look through it.  */
      if (is_declaration
	  && !typename_keyword_p
	  && parser->scope
	  && TREE_CODE (parser->scope) == TYPENAME_TYPE)
	parser->scope = resolve_typename_type (parser->scope,
					       /*only_current_p=*/false);
      /* Parse the qualifying entity.  */
      new_scope
	= cp_parser_qualifying_entity (parser,
                                       typename_keyword_p,
                                       template_keyword_p,
                                       check_dependency_p,
                                       type_p,
                                       is_declaration);
      /* Look for the `::' token.  */
      cp_parser_require (parser, CPP_SCOPE, "%<::%>");

      /* If we found what we wanted, we keep going; otherwise, we're
	 done.  */
      if (!cp_parser_parse_definitely (parser))
	{
	  bool error_p = false;

	  /* Restore the OLD_SCOPE since it was valid before the
	     failed attempt at finding the last
	     class-or-namespace-name.  */
	  parser->scope = old_scope;
	  parser->qualifying_scope = saved_qualifying_scope;
	  if (cp_parser_uncommitted_to_tentative_parse_p (parser))
	    break;
	  /* If the next token is an identifier, and the one after
	     that is a `::', then any valid interpretation would have
	     found a class-or-namespace-name.  */
	  while (cp_lexer_next_token_is (parser->lexer, CPP_NAME)
		 && (cp_lexer_peek_nth_token (parser->lexer, 2)->type
		     == CPP_SCOPE)
		 && (cp_lexer_peek_nth_token (parser->lexer, 3)->type
		     != CPP_COMPL))
	    {
	      token = cp_lexer_consume_token (parser->lexer);
	      if (!error_p)
		{
		  if (!token->ambiguous_p)
		    {
		      tree decl;
		      tree ambiguous_decls;

		      decl = cp_parser_lookup_name (parser, token->u.value,
						    none_type,
						    /*is_template=*/false,
						    /*is_namespace=*/false,
						    /*check_dependency=*/true,
						    &ambiguous_decls,
						    token->location);
		      if (TREE_CODE (decl) == TEMPLATE_DECL)
			error_at (token->location,
				  "%qD used without template parameters",
				  decl);
		      else if (ambiguous_decls)
			{
			  error_at (token->location,
				    "reference to %qD is ambiguous",
				    token->u.value);
			  print_candidates (ambiguous_decls);
			  decl = error_mark_node;
			}
		      else
                        {
                          const char* msg = "is not a class or namespace";
                          if (cxx_dialect != cxx98)
                            msg = "is not a class, namespace, or enumeration";
                          cp_parser_name_lookup_error
                            (parser, token->u.value, decl, msg,
	  		     token->location);
                        }
		    }
		  parser->scope = error_mark_node;
		  error_p = true;
		  /* Treat this as a successful nested-name-specifier
		     due to:

		     [basic.lookup.qual]

		     If the name found is not a class-name (clause
		     _class_) or namespace-name (_namespace.def_), the
		     program is ill-formed.  */
		  success = true;
		}
	      cp_lexer_consume_token (parser->lexer);
	    }
	  break;
	}
      /* We've found one valid nested-name-specifier.  */
      success = true;
      /* Name lookup always gives us a DECL.  */
      if (TREE_CODE (new_scope) == TYPE_DECL)
	new_scope = TREE_TYPE (new_scope);
      /* Uses of "template" must be followed by actual templates.  */
      if (template_keyword_p
	  && !(CLASS_TYPE_P (new_scope)
	       && ((CLASSTYPE_USE_TEMPLATE (new_scope)
		    && PRIMARY_TEMPLATE_P (CLASSTYPE_TI_TEMPLATE (new_scope)))
		   || CLASSTYPE_IS_TEMPLATE (new_scope)))
	  && !(TREE_CODE (new_scope) == TYPENAME_TYPE
	       && (TREE_CODE (TYPENAME_TYPE_FULLNAME (new_scope))
		   == TEMPLATE_ID_EXPR)))
	permerror (input_location, TYPE_P (new_scope)
		   ? "%qT is not a template"
		   : "%qD is not a template",
		   new_scope);
      /* If it is a class scope, try to complete it; we are about to
	 be looking up names inside the class.  */
      if (TYPE_P (new_scope)
	  /* Since checking types for dependency can be expensive,
	     avoid doing it if the type is already complete.  */
	  && !COMPLETE_TYPE_P (new_scope)
	  /* Do not try to complete dependent types.  */
	  && !dependent_type_p (new_scope))
	{
	  new_scope = complete_type (new_scope);
	  /* If it is a typedef to current class, use the current
	     class instead, as the typedef won't have any names inside
	     it yet.  */
	  if (!COMPLETE_TYPE_P (new_scope)
	      && currently_open_class (new_scope))
	    new_scope = TYPE_MAIN_VARIANT (new_scope);
	}
      /* Make sure we look in the right scope the next time through
	 the loop.  */
      parser->scope = new_scope;
    }

  /* If parsing tentatively, replace the sequence of tokens that makes
     up the nested-name-specifier with a CPP_NESTED_NAME_SPECIFIER
     token.  That way, should we re-parse the token stream, we will
     not have to repeat the effort required to do the parse, nor will
     we issue duplicate error messages.  */
  if (success && start)
    {
      cp_token *token;

      token = cp_lexer_token_at (parser->lexer, start);
      /* Reset the contents of the START token.  */
      token->type = CPP_NESTED_NAME_SPECIFIER;
      /* Retrieve any deferred checks.  Do not pop this access checks yet
	 so the memory will not be reclaimed during token replacing below.  */
      token->u.tree_check_value = GGC_CNEW (struct tree_check);
      token->u.tree_check_value->value = parser->scope;
      token->u.tree_check_value->checks = get_deferred_access_checks ();
      token->u.tree_check_value->qualifying_scope =
	parser->qualifying_scope;
      token->keyword = RID_MAX;

      /* Purge all subsequent tokens.  */
      cp_lexer_purge_tokens_after (parser->lexer, start);
    }

  if (start)
    pop_to_parent_deferring_access_checks ();

  return success ? parser->scope : NULL_TREE;
}

/* Parse a nested-name-specifier.  See
   cp_parser_nested_name_specifier_opt for details.  This function
   behaves identically, except that it will an issue an error if no
   nested-name-specifier is present.  */

static tree
cp_parser_nested_name_specifier (cp_parser *parser,
				 bool typename_keyword_p,
				 bool check_dependency_p,
				 bool type_p,
				 bool is_declaration)
{
  tree scope;

  /* Look for the nested-name-specifier.  */
  scope = cp_parser_nested_name_specifier_opt (parser,
					       typename_keyword_p,
					       check_dependency_p,
					       type_p,
					       is_declaration);
  /* If it was not present, issue an error message.  */
  if (!scope)
    {
      cp_parser_error (parser, "expected nested-name-specifier");
      parser->scope = NULL_TREE;
    }

  return scope;
}

/* Parse the qualifying entity in a nested-name-specifier. For C++98,
   this is either a class-name or a namespace-name (which corresponds
   to the class-or-namespace-name production in the grammar). For
   C++0x, it can also be a type-name that refers to an enumeration
   type.

   TYPENAME_KEYWORD_P is TRUE iff the `typename' keyword is in effect.
   TEMPLATE_KEYWORD_P is TRUE iff the `template' keyword is in effect.
   CHECK_DEPENDENCY_P is FALSE iff dependent names should be looked up.
   TYPE_P is TRUE iff the next name should be taken as a class-name,
   even the same name is declared to be another entity in the same
   scope.

   Returns the class (TYPE_DECL) or namespace (NAMESPACE_DECL)
   specified by the class-or-namespace-name.  If neither is found the
   ERROR_MARK_NODE is returned.  */

static tree
cp_parser_qualifying_entity (cp_parser *parser,
			     bool typename_keyword_p,
			     bool template_keyword_p,
			     bool check_dependency_p,
			     bool type_p,
			     bool is_declaration)
{
  tree saved_scope;
  tree saved_qualifying_scope;
  tree saved_object_scope;
  tree scope;
  bool only_class_p;
  bool successful_parse_p;

  /* Before we try to parse the class-name, we must save away the
     current PARSER->SCOPE since cp_parser_class_name will destroy
     it.  */
  saved_scope = parser->scope;
  saved_qualifying_scope = parser->qualifying_scope;
  saved_object_scope = parser->object_scope;
  /* Try for a class-name first.  If the SAVED_SCOPE is a type, then
     there is no need to look for a namespace-name.  */
  only_class_p = template_keyword_p 
    || (saved_scope && TYPE_P (saved_scope) && cxx_dialect == cxx98);
  if (!only_class_p)
    cp_parser_parse_tentatively (parser);
  scope = cp_parser_class_name (parser,
				typename_keyword_p,
				template_keyword_p,
				type_p ? class_type : none_type,
				check_dependency_p,
				/*class_head_p=*/false,
				is_declaration);
  successful_parse_p = only_class_p || cp_parser_parse_definitely (parser);
  /* If that didn't work and we're in C++0x mode, try for a type-name.  */
  if (!only_class_p 
      && cxx_dialect != cxx98
      && !successful_parse_p)
    {
      /* Restore the saved scope.  */
      parser->scope = saved_scope;
      parser->qualifying_scope = saved_qualifying_scope;
      parser->object_scope = saved_object_scope;

      /* Parse tentatively.  */
      cp_parser_parse_tentatively (parser);
     
      /* Parse a typedef-name or enum-name.  */
      scope = cp_parser_nonclass_name (parser);
      successful_parse_p = cp_parser_parse_definitely (parser);
    }
  /* If that didn't work, try for a namespace-name.  */
  if (!only_class_p && !successful_parse_p)
    {
      /* Restore the saved scope.  */
      parser->scope = saved_scope;
      parser->qualifying_scope = saved_qualifying_scope;
      parser->object_scope = saved_object_scope;
      /* If we are not looking at an identifier followed by the scope
	 resolution operator, then this is not part of a
	 nested-name-specifier.  (Note that this function is only used
	 to parse the components of a nested-name-specifier.)  */
      if (cp_lexer_next_token_is_not (parser->lexer, CPP_NAME)
	  || cp_lexer_peek_nth_token (parser->lexer, 2)->type != CPP_SCOPE)
	return error_mark_node;
      scope = cp_parser_namespace_name (parser);
    }

  return scope;
}

/* Parse a postfix-expression.

   postfix-expression:
     primary-expression
     postfix-expression [ expression ]
     postfix-expression ( expression-list [opt] )
     simple-type-specifier ( expression-list [opt] )
     typename :: [opt] nested-name-specifier identifier
       ( expression-list [opt] )
     typename :: [opt] nested-name-specifier template [opt] template-id
       ( expression-list [opt] )
     postfix-expression . template [opt] id-expression
     postfix-expression -> template [opt] id-expression
     postfix-expression . pseudo-destructor-name
     postfix-expression -> pseudo-destructor-name
     postfix-expression ++
     postfix-expression --
     dynamic_cast < type-id > ( expression )
     static_cast < type-id > ( expression )
     reinterpret_cast < type-id > ( expression )
     const_cast < type-id > ( expression )
     typeid ( expression )
     typeid ( type-id )

   GNU Extension:

   postfix-expression:
     ( type-id ) { initializer-list , [opt] }

   This extension is a GNU version of the C99 compound-literal
   construct.  (The C99 grammar uses `type-name' instead of `type-id',
   but they are essentially the same concept.)

   If ADDRESS_P is true, the postfix expression is the operand of the
   `&' operator.  CAST_P is true if this expression is the target of a
   cast.

   If MEMBER_ACCESS_ONLY_P, we only allow postfix expressions that are
   class member access expressions [expr.ref].

   Returns a representation of the expression.  */

static tree
cp_parser_postfix_expression (cp_parser *parser, bool address_p, bool cast_p,
                              bool member_access_only_p,
			      cp_id_kind * pidk_return)
{
  cp_token *token;
  enum rid keyword;
  cp_id_kind idk = CP_ID_KIND_NONE;
  tree postfix_expression = NULL_TREE;
  bool is_member_access = false;

  /* Peek at the next token.  */
  token = cp_lexer_peek_token (parser->lexer);
  /* Some of the productions are determined by keywords.  */
  keyword = token->keyword;
  switch (keyword)
    {
    case RID_DYNCAST:
    case RID_STATCAST:
    case RID_REINTCAST:
    case RID_CONSTCAST:
      {
	tree type;
	tree expression;
	const char *saved_message;

	/* All of these can be handled in the same way from the point
	   of view of parsing.  Begin by consuming the token
	   identifying the cast.  */
	cp_lexer_consume_token (parser->lexer);

	/* New types cannot be defined in the cast.  */
	saved_message = parser->type_definition_forbidden_message;
	parser->type_definition_forbidden_message
	  = G_("types may not be defined in casts");

	/* Look for the opening `<'.  */
	cp_parser_require (parser, CPP_LESS, "%<<%>");
	/* Parse the type to which we are casting.  */
	type = cp_parser_type_id (parser);
	/* Look for the closing `>'.  */
	cp_parser_require (parser, CPP_GREATER, "%<>%>");
	/* Restore the old message.  */
	parser->type_definition_forbidden_message = saved_message;

	/* And the expression which is being cast.  */
	cp_parser_require (parser, CPP_OPEN_PAREN, "%<(%>");
	expression = cp_parser_expression (parser, /*cast_p=*/true, & idk);
	cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>");

	/* Only type conversions to integral or enumeration types
	   can be used in constant-expressions.  */
	if (!cast_valid_in_integral_constant_expression_p (type)
	    && (cp_parser_non_integral_constant_expression
		(parser,
		 "a cast to a type other than an integral or "
		 "enumeration type")))
	  return error_mark_node;

	switch (keyword)
	  {
	  case RID_DYNCAST:
	    postfix_expression
	      = build_dynamic_cast (type, expression, tf_warning_or_error);
	    break;
	  case RID_STATCAST:
	    postfix_expression
	      = build_static_cast (type, expression, tf_warning_or_error);
	    break;
	  case RID_REINTCAST:
	    postfix_expression
	      = build_reinterpret_cast (type, expression, 
                                        tf_warning_or_error);
	    break;
	  case RID_CONSTCAST:
	    postfix_expression
	      = build_const_cast (type, expression, tf_warning_or_error);
	    break;
	  default:
	    gcc_unreachable ();
	  }
      }
      break;

    case RID_TYPEID:
      {
	tree type;
	const char *saved_message;
	bool saved_in_type_id_in_expr_p;

	/* Consume the `typeid' token.  */
	cp_lexer_consume_token (parser->lexer);
	/* Look for the `(' token.  */
	cp_parser_require (parser, CPP_OPEN_PAREN, "%<(%>");
	/* Types cannot be defined in a `typeid' expression.  */
	saved_message = parser->type_definition_forbidden_message;
	parser->type_definition_forbidden_message
	  = G_("types may not be defined in a %<typeid%> expression");
	/* We can't be sure yet whether we're looking at a type-id or an
	   expression.  */
	cp_parser_parse_tentatively (parser);
	/* Try a type-id first.  */
	saved_in_type_id_in_expr_p = parser->in_type_id_in_expr_p;
	parser->in_type_id_in_expr_p = true;
	type = cp_parser_type_id (parser);
	parser->in_type_id_in_expr_p = saved_in_type_id_in_expr_p;
	/* Look for the `)' token.  Otherwise, we can't be sure that
	   we're not looking at an expression: consider `typeid (int
	   (3))', for example.  */
	cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>");
	/* If all went well, simply lookup the type-id.  */
	if (cp_parser_parse_definitely (parser))
	  postfix_expression = get_typeid (type);
	/* Otherwise, fall back to the expression variant.  */
	else
	  {
	    tree expression;

	    /* Look for an expression.  */
	    expression = cp_parser_expression (parser, /*cast_p=*/false, & idk);
	    /* Compute its typeid.  */
	    postfix_expression = build_typeid (expression);
	    /* Look for the `)' token.  */
	    cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>");
	  }
	/* Restore the saved message.  */
	parser->type_definition_forbidden_message = saved_message;
	/* `typeid' may not appear in an integral constant expression.  */
	if (cp_parser_non_integral_constant_expression(parser,
						       "%<typeid%> operator"))
	  return error_mark_node;
      }
      break;

    case RID_TYPENAME:
      {
	tree type;
	/* The syntax permitted here is the same permitted for an
	   elaborated-type-specifier.  */
	type = cp_parser_elaborated_type_specifier (parser,
						    /*is_friend=*/false,
						    /*is_declaration=*/false);
	postfix_expression = cp_parser_functional_cast (parser, type);
      }
      break;

    default:
      {
	tree type;

	/* If the next thing is a simple-type-specifier, we may be
	   looking at a functional cast.  We could also be looking at
	   an id-expression.  So, we try the functional cast, and if
	   that doesn't work we fall back to the primary-expression.  */
	cp_parser_parse_tentatively (parser);
	/* Look for the simple-type-specifier.  */
	type = cp_parser_simple_type_specifier (parser,
						/*decl_specs=*/NULL,
						CP_PARSER_FLAGS_NONE);
	/* Parse the cast itself.  */
	if (!cp_parser_error_occurred (parser))
	  postfix_expression
	    = cp_parser_functional_cast (parser, type);
	/* If that worked, we're done.  */
	if (cp_parser_parse_definitely (parser))
	  break;

	/* If the functional-cast didn't work out, try a
	   compound-literal.  */
	if (cp_parser_allow_gnu_extensions_p (parser)
	    && cp_lexer_next_token_is (parser->lexer, CPP_OPEN_PAREN))
	  {
	    VEC(constructor_elt,gc) *initializer_list = NULL;
	    bool saved_in_type_id_in_expr_p;

	    cp_parser_parse_tentatively (parser);
	    /* Consume the `('.  */
	    cp_lexer_consume_token (parser->lexer);
	    /* Parse the type.  */
	    saved_in_type_id_in_expr_p = parser->in_type_id_in_expr_p;
	    parser->in_type_id_in_expr_p = true;
	    type = cp_parser_type_id (parser);
	    parser->in_type_id_in_expr_p = saved_in_type_id_in_expr_p;
	    /* Look for the `)'.  */
	    cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>");
	    /* Look for the `{'.  */
	    cp_parser_require (parser, CPP_OPEN_BRACE, "%<{%>");
	    /* If things aren't going well, there's no need to
	       keep going.  */
	    if (!cp_parser_error_occurred (parser))
	      {
		bool non_constant_p;
		/* Parse the initializer-list.  */
		initializer_list
		  = cp_parser_initializer_list (parser, &non_constant_p);
		/* Allow a trailing `,'.  */
		if (cp_lexer_next_token_is (parser->lexer, CPP_COMMA))
		  cp_lexer_consume_token (parser->lexer);
		/* Look for the final `}'.  */
		cp_parser_require (parser, CPP_CLOSE_BRACE, "%<}%>");
	      }
	    /* If that worked, we're definitely looking at a
	       compound-literal expression.  */
	    if (cp_parser_parse_definitely (parser))
	      {
		/* Warn the user that a compound literal is not
		   allowed in standard C++.  */
		pedwarn (input_location, OPT_pedantic, "ISO C++ forbids compound-literals");
		/* For simplicity, we disallow compound literals in
		   constant-expressions.  We could
		   allow compound literals of integer type, whose
		   initializer was a constant, in constant
		   expressions.  Permitting that usage, as a further
		   extension, would not change the meaning of any
		   currently accepted programs.  (Of course, as
		   compound literals are not part of ISO C++, the
		   standard has nothing to say.)  */
		if (cp_parser_non_integral_constant_expression 
		    (parser, "non-constant compound literals"))
		  {
		    postfix_expression = error_mark_node;
		    break;
		  }
		/* Form the representation of the compound-literal.  */
		postfix_expression
		  = (finish_compound_literal
		     (type, build_constructor (init_list_type_node,
					       initializer_list)));
		break;
	      }
	  }

	/* It must be a primary-expression.  */
	postfix_expression
	  = cp_parser_primary_expression (parser, address_p, cast_p,
					  /*template_arg_p=*/false,
					  &idk);
      }
      break;
    }

  /* Keep looping until the postfix-expression is complete.  */
  while (true)
    {
      if (idk == CP_ID_KIND_UNQUALIFIED
	  && TREE_CODE (postfix_expression) == IDENTIFIER_NODE
	  && cp_lexer_next_token_is_not (parser->lexer, CPP_OPEN_PAREN))
	/* It is not a Koenig lookup function call.  */
	postfix_expression
	  = unqualified_name_lookup_error (postfix_expression);

      /* Peek at the next token.  */
      token = cp_lexer_peek_token (parser->lexer);

      switch (token->type)
	{
	case CPP_OPEN_SQUARE:
	  postfix_expression
	    = cp_parser_postfix_open_square_expression (parser,
							postfix_expression,
							false);
	  idk = CP_ID_KIND_NONE;
          is_member_access = false;
	  break;

	case CPP_OPEN_PAREN:
	  /* postfix-expression ( expression-list [opt] ) */
	  {
	    bool koenig_p;
	    bool is_builtin_constant_p;
	    bool saved_integral_constant_expression_p = false;
	    bool saved_non_integral_constant_expression_p = false;
	    VEC(tree,gc) *args;

            is_member_access = false;

	    is_builtin_constant_p
	      = DECL_IS_BUILTIN_CONSTANT_P (postfix_expression);
	    if (is_builtin_constant_p)
	      {
		/* The whole point of __builtin_constant_p is to allow
		   non-constant expressions to appear as arguments.  */
		saved_integral_constant_expression_p
		  = parser->integral_constant_expression_p;
		saved_non_integral_constant_expression_p
		  = parser->non_integral_constant_expression_p;
		parser->integral_constant_expression_p = false;
	      }
	    args = (cp_parser_parenthesized_expression_list
		    (parser, /*is_attribute_list=*/false,
		     /*cast_p=*/false, /*allow_expansion_p=*/true,
		     /*non_constant_p=*/NULL));
	    if (is_builtin_constant_p)
	      {
		parser->integral_constant_expression_p
		  = saved_integral_constant_expression_p;
		parser->non_integral_constant_expression_p
		  = saved_non_integral_constant_expression_p;
	      }

	    if (args == NULL)
	      {
		postfix_expression = error_mark_node;
		break;
	      }

	    /* Function calls are not permitted in
	       constant-expressions.  */
	    if (! builtin_valid_in_constant_expr_p (postfix_expression)
		&& cp_parser_non_integral_constant_expression (parser,
							       "a function call"))
	      {
		postfix_expression = error_mark_node;
		release_tree_vector (args);
		break;
	      }

	    koenig_p = false;
	    if (idk == CP_ID_KIND_UNQUALIFIED
		|| idk == CP_ID_KIND_TEMPLATE_ID)
	      {
		if (TREE_CODE (postfix_expression) == IDENTIFIER_NODE)
		  {
		    if (!VEC_empty (tree, args))
		      {
			koenig_p = true;
			if (!any_type_dependent_arguments_p (args))
			  postfix_expression
			    = perform_koenig_lookup (postfix_expression, args);
		      }
		    else
		      postfix_expression
			= unqualified_fn_lookup_error (postfix_expression);
		  }
		/* We do not perform argument-dependent lookup if
		   normal lookup finds a non-function, in accordance
		   with the expected resolution of DR 218.  */
		else if (!VEC_empty (tree, args)
			 && is_overloaded_fn (postfix_expression))
		  {
		    tree fn = get_first_fn (postfix_expression);
		    fn = STRIP_TEMPLATE (fn);

		    /* Do not do argument dependent lookup if regular
		       lookup finds a member function or a block-scope
		       function declaration.  [basic.lookup.argdep]/3  */
		    if (!DECL_FUNCTION_MEMBER_P (fn)
			&& !DECL_LOCAL_FUNCTION_P (fn))
		      {
			koenig_p = true;
			if (!any_type_dependent_arguments_p (args))
			  postfix_expression
			    = perform_koenig_lookup (postfix_expression, args);
		      }
		  }
	      }

	    if (TREE_CODE (postfix_expression) == COMPONENT_REF)
	      {
		tree instance = TREE_OPERAND (postfix_expression, 0);
		tree fn = TREE_OPERAND (postfix_expression, 1);

		if (processing_template_decl
		    && (type_dependent_expression_p (instance)
			|| (!BASELINK_P (fn)
			    && TREE_CODE (fn) != FIELD_DECL)
			|| type_dependent_expression_p (fn)
			|| any_type_dependent_arguments_p (args)))
		  {
		    postfix_expression
		      = build_nt_call_vec (postfix_expression, args);
		    release_tree_vector (args);
		    break;
		  }

		if (BASELINK_P (fn))
		  {
		  postfix_expression
		    = (build_new_method_call
		       (instance, fn, &args, NULL_TREE,
			(idk == CP_ID_KIND_QUALIFIED
			 ? LOOKUP_NONVIRTUAL : LOOKUP_NORMAL),
			/*fn_p=*/NULL,
			tf_warning_or_error));
		  }
		else
		  postfix_expression
		    = finish_call_expr (postfix_expression, &args,
					/*disallow_virtual=*/false,
					/*koenig_p=*/false,
					tf_warning_or_error);
	      }
	    else if (TREE_CODE (postfix_expression) == OFFSET_REF
		     || TREE_CODE (postfix_expression) == MEMBER_REF
		     || TREE_CODE (postfix_expression) == DOTSTAR_EXPR)
	      postfix_expression = (build_offset_ref_call_from_tree
				    (postfix_expression, &args));
	    else if (idk == CP_ID_KIND_QUALIFIED)
	      /* A call to a static class member, or a namespace-scope
		 function.  */
	      postfix_expression
		= finish_call_expr (postfix_expression, &args,
				    /*disallow_virtual=*/true,
				    koenig_p,
				    tf_warning_or_error);
	    else
	      /* All other function calls.  */
	      postfix_expression
		= finish_call_expr (postfix_expression, &args,
				    /*disallow_virtual=*/false,
				    koenig_p,
				    tf_warning_or_error);

	    /* The POSTFIX_EXPRESSION is certainly no longer an id.  */
	    idk = CP_ID_KIND_NONE;

	    release_tree_vector (args);
	  }
	  break;

	case CPP_DOT:
	case CPP_DEREF:
	  /* postfix-expression . template [opt] id-expression
	     postfix-expression . pseudo-destructor-name
	     postfix-expression -> template [opt] id-expression
	     postfix-expression -> pseudo-destructor-name */

	  /* Consume the `.' or `->' operator.  */
	  cp_lexer_consume_token (parser->lexer);

	  postfix_expression
	    = cp_parser_postfix_dot_deref_expression (parser, token->type,
						      postfix_expression,
						      false, &idk,
						      token->location);

          is_member_access = true;
	  break;

	case CPP_PLUS_PLUS:
	  /* postfix-expression ++  */
	  /* Consume the `++' token.  */
	  cp_lexer_consume_token (parser->lexer);
	  /* Generate a representation for the complete expression.  */
	  postfix_expression
	    = finish_increment_expr (postfix_expression,
				     POSTINCREMENT_EXPR);
	  /* Increments may not appear in constant-expressions.  */
	  if (cp_parser_non_integral_constant_expression (parser,
							  "an increment"))
	    postfix_expression = error_mark_node;
	  idk = CP_ID_KIND_NONE;
          is_member_access = false;
	  break;

	case CPP_MINUS_MINUS:
	  /* postfix-expression -- */
	  /* Consume the `--' token.  */
	  cp_lexer_consume_token (parser->lexer);
	  /* Generate a representation for the complete expression.  */
	  postfix_expression
	    = finish_increment_expr (postfix_expression,
				     POSTDECREMENT_EXPR);
	  /* Decrements may not appear in constant-expressions.  */
	  if (cp_parser_non_integral_constant_expression (parser,
							  "a decrement"))
	    postfix_expression = error_mark_node;
	  idk = CP_ID_KIND_NONE;
          is_member_access = false;
	  break;

	default:
	  if (pidk_return != NULL)
	    * pidk_return = idk;
          if (member_access_only_p)
            return is_member_access? postfix_expression : error_mark_node;
          else
            return postfix_expression;
	}
    }

  /* We should never get here.  */
  gcc_unreachable ();
  return error_mark_node;
}

/* A subroutine of cp_parser_postfix_expression that also gets hijacked
   by cp_parser_builtin_offsetof.  We're looking for

     postfix-expression [ expression ]

   FOR_OFFSETOF is set if we're being called in that context, which
   changes how we deal with integer constant expressions.  */

static tree
cp_parser_postfix_open_square_expression (cp_parser *parser,
					  tree postfix_expression,
					  bool for_offsetof)
{
  tree index;

  /* Consume the `[' token.  */
  cp_lexer_consume_token (parser->lexer);

  /* Parse the index expression.  */
  /* ??? For offsetof, there is a question of what to allow here.  If
     offsetof is not being used in an integral constant expression context,
     then we *could* get the right answer by computing the value at runtime.
     If we are in an integral constant expression context, then we might
     could accept any constant expression; hard to say without analysis.
     Rather than open the barn door too wide right away, allow only integer
     constant expressions here.  */
  if (for_offsetof)
    index = cp_parser_constant_expression (parser, false, NULL);
  else
    index = cp_parser_expression (parser, /*cast_p=*/false, NULL);

  /* Look for the closing `]'.  */
  cp_parser_require (parser, CPP_CLOSE_SQUARE, "%<]%>");

  /* Build the ARRAY_REF.  */
  postfix_expression = grok_array_decl (postfix_expression, index);

  /* When not doing offsetof, array references are not permitted in
     constant-expressions.  */
  if (!for_offsetof
      && (cp_parser_non_integral_constant_expression
	  (parser, "an array reference")))
    postfix_expression = error_mark_node;

  return postfix_expression;
}

/* A subroutine of cp_parser_postfix_expression that also gets hijacked
   by cp_parser_builtin_offsetof.  We're looking for

     postfix-expression . template [opt] id-expression
     postfix-expression . pseudo-destructor-name
     postfix-expression -> template [opt] id-expression
     postfix-expression -> pseudo-destructor-name

   FOR_OFFSETOF is set if we're being called in that context.  That sorta
   limits what of the above we'll actually accept, but nevermind.
   TOKEN_TYPE is the "." or "->" token, which will already have been
   removed from the stream.  */

static tree
cp_parser_postfix_dot_deref_expression (cp_parser *parser,
					enum cpp_ttype token_type,
					tree postfix_expression,
					bool for_offsetof, cp_id_kind *idk,
					location_t location)
{
  tree name;
  bool dependent_p;
  bool pseudo_destructor_p;
  tree scope = NULL_TREE;

  /* If this is a `->' operator, dereference the pointer.  */
  if (token_type == CPP_DEREF)
    postfix_expression = build_x_arrow (postfix_expression);
  /* Check to see whether or not the expression is type-dependent.  */
  dependent_p = type_dependent_expression_p (postfix_expression);
  /* The identifier following the `->' or `.' is not qualified.  */
  parser->scope = NULL_TREE;
  parser->qualifying_scope = NULL_TREE;
  parser->object_scope = NULL_TREE;
  *idk = CP_ID_KIND_NONE;

  /* Enter the scope corresponding to the type of the object
     given by the POSTFIX_EXPRESSION.  */
  if (!dependent_p && TREE_TYPE (postfix_expression) != NULL_TREE)
    {
      scope = TREE_TYPE (postfix_expression);
      /* According to the standard, no expression should ever have
	 reference type.  Unfortunately, we do not currently match
	 the standard in this respect in that our internal representation
	 of an expression may have reference type even when the standard
	 says it does not.  Therefore, we have to manually obtain the
	 underlying type here.  */
      scope = non_reference (scope);
      /* The type of the POSTFIX_EXPRESSION must be complete.  */
      if (scope == unknown_type_node)
	{
	  error_at (location, "%qE does not have class type",
		    postfix_expression);
	  scope = NULL_TREE;
	}
      else
	scope = complete_type_or_else (scope, NULL_TREE);
      /* Let the name lookup machinery know that we are processing a
	 class member access expression.  */
      parser->context->object_type = scope;
      /* If something went wrong, we want to be able to discern that case,
	 as opposed to the case where there was no SCOPE due to the type
	 of expression being dependent.  */
      if (!scope)
	scope = error_mark_node;
      /* If the SCOPE was erroneous, make the various semantic analysis
	 functions exit quickly -- and without issuing additional error
	 messages.  */
      if (scope == error_mark_node)
	postfix_expression = error_mark_node;
    }

  /* Assume this expression is not a pseudo-destructor access.  */
  pseudo_destructor_p = false;

  /* If the SCOPE is a scalar type, then, if this is a valid program,
     we must be looking at a pseudo-destructor-name.  If POSTFIX_EXPRESSION
     is type dependent, it can be pseudo-destructor-name or something else.
     Try to parse it as pseudo-destructor-name first.  */
  if ((scope && SCALAR_TYPE_P (scope)) || dependent_p)
    {
      tree s;
      tree type;

      cp_parser_parse_tentatively (parser);
      /* Parse the pseudo-destructor-name.  */
      s = NULL_TREE;
      cp_parser_pseudo_destructor_name (parser, &s, &type);
      if (dependent_p
	  && (cp_parser_error_occurred (parser)
	      || TREE_CODE (type) != TYPE_DECL
	      || !SCALAR_TYPE_P (TREE_TYPE (type))))
	cp_parser_abort_tentative_parse (parser);
      else if (cp_parser_parse_definitely (parser))
	{
	  pseudo_destructor_p = true;
	  postfix_expression
	    = finish_pseudo_destructor_expr (postfix_expression,
					     s, TREE_TYPE (type));
	}
    }

  if (!pseudo_destructor_p)
    {
      /* If the SCOPE is not a scalar type, we are looking at an
	 ordinary class member access expression, rather than a
	 pseudo-destructor-name.  */
      bool template_p;
      cp_token *token = cp_lexer_peek_token (parser->lexer);
      /* Parse the id-expression.  */
      name = (cp_parser_id_expression
	      (parser,
	       cp_parser_optional_template_keyword (parser),
	       /*check_dependency_p=*/true,
	       &template_p,
	       /*declarator_p=*/false,
	       /*optional_p=*/false));
      /* In general, build a SCOPE_REF if the member name is qualified.
	 However, if the name was not dependent and has already been
	 resolved; there is no need to build the SCOPE_REF.  For example;

	     struct X { void f(); };
	     template <typename T> void f(T* t) { t->X::f(); }

	 Even though "t" is dependent, "X::f" is not and has been resolved
	 to a BASELINK; there is no need to include scope information.  */

      /* But we do need to remember that there was an explicit scope for
	 virtual function calls.  */
      if (parser->scope)
	*idk = CP_ID_KIND_QUALIFIED;

      /* If the name is a template-id that names a type, we will get a
	 TYPE_DECL here.  That is invalid code.  */
      if (TREE_CODE (name) == TYPE_DECL)
	{
	  error_at (token->location, "invalid use of %qD", name);
	  postfix_expression = error_mark_node;
	}
      else
	{
	  if (name != error_mark_node && !BASELINK_P (name) && parser->scope)
	    {
	      name = build_qualified_name (/*type=*/NULL_TREE,
					   parser->scope,
					   name,
					   template_p);
	      parser->scope = NULL_TREE;
	      parser->qualifying_scope = NULL_TREE;
	      parser->object_scope = NULL_TREE;
	    }
	  if (scope && name && BASELINK_P (name))
	    adjust_result_of_qualified_name_lookup
	      (name, BINFO_TYPE (BASELINK_ACCESS_BINFO (name)), scope);
	  postfix_expression
	    = finish_class_member_access_expr (postfix_expression, name,
					       template_p, 
					       tf_warning_or_error);
	}
    }

  /* We no longer need to look up names in the scope of the object on
     the left-hand side of the `.' or `->' operator.  */
  parser->context->object_type = NULL_TREE;

  /* Outside of offsetof, these operators may not appear in
     constant-expressions.  */
  if (!for_offsetof
      && (cp_parser_non_integral_constant_expression
	  (parser, token_type == CPP_DEREF ? "%<->%>" : "%<.%>")))
    postfix_expression = error_mark_node;

  return postfix_expression;
}

/* Parse a parenthesized expression-list.

   expression-list:
     assignment-expression
     expression-list, assignment-expression

   attribute-list:
     expression-list
     identifier
     identifier, expression-list

   CAST_P is true if this expression is the target of a cast.

   ALLOW_EXPANSION_P is true if this expression allows expansion of an
   argument pack.

   Returns a vector of trees.  Each element is a representation of an
   assignment-expression.  NULL is returned if the ( and or ) are
   missing.  An empty, but allocated, vector is returned on no
   expressions.  The parentheses are eaten.  IS_ATTRIBUTE_LIST is true
   if this is really an attribute list being parsed.  If
   NON_CONSTANT_P is non-NULL, *NON_CONSTANT_P indicates whether or
   not all of the expressions in the list were constant.  */

static VEC(tree,gc) *
cp_parser_parenthesized_expression_list (cp_parser* parser,
					 bool is_attribute_list,
					 bool cast_p,
                                         bool allow_expansion_p,
					 bool *non_constant_p)
{
  VEC(tree,gc) *expression_list;
  bool fold_expr_p = is_attribute_list;
  tree identifier = NULL_TREE;
  bool saved_greater_than_is_operator_p;

  /* Assume all the expressions will be constant.  */
  if (non_constant_p)
    *non_constant_p = false;

  if (!cp_parser_require (parser, CPP_OPEN_PAREN, "%<(%>"))
    return NULL;

  expression_list = make_tree_vector ();

  /* Within a parenthesized expression, a `>' token is always
     the greater-than operator.  */
  saved_greater_than_is_operator_p
    = parser->greater_than_is_operator_p;
  parser->greater_than_is_operator_p = true;

  /* Consume expressions until there are no more.  */
  if (cp_lexer_next_token_is_not (parser->lexer, CPP_CLOSE_PAREN))
    while (true)
      {
	tree expr;

	/* At the beginning of attribute lists, check to see if the
	   next token is an identifier.  */
	if (is_attribute_list
	    && cp_lexer_peek_token (parser->lexer)->type == CPP_NAME)
	  {
	    cp_token *token;

	    /* Consume the identifier.  */
	    token = cp_lexer_consume_token (parser->lexer);
	    /* Save the identifier.  */
	    identifier = token->u.value;
	  }
	else
	  {
	    bool expr_non_constant_p;

	    /* Parse the next assignment-expression.  */
	    if (cp_lexer_next_token_is (parser->lexer, CPP_OPEN_BRACE))
	      {
		/* A braced-init-list.  */
		maybe_warn_cpp0x (CPP0X_INITIALIZER_LISTS);
		expr = cp_parser_braced_list (parser, &expr_non_constant_p);
		if (non_constant_p && expr_non_constant_p)
		  *non_constant_p = true;
	      }
	    else if (non_constant_p)
	      {
		expr = (cp_parser_constant_expression
			(parser, /*allow_non_constant_p=*/true,
			 &expr_non_constant_p));
		if (expr_non_constant_p)
		  *non_constant_p = true;
	      }
	    else
	      expr = cp_parser_assignment_expression (parser, cast_p, NULL);

	    if (fold_expr_p)
	      expr = fold_non_dependent_expr (expr);

            /* If we have an ellipsis, then this is an expression
	       expansion.  */
            if (allow_expansion_p
                && cp_lexer_next_token_is (parser->lexer, CPP_ELLIPSIS))
              {
                /* Consume the `...'.  */
                cp_lexer_consume_token (parser->lexer);

                /* Build the argument pack.  */
                expr = make_pack_expansion (expr);
              }

	     /* Add it to the list.  We add error_mark_node
		expressions to the list, so that we can still tell if
		the correct form for a parenthesized expression-list
		is found. That gives better errors.  */
	    VEC_safe_push (tree, gc, expression_list, expr);

	    if (expr == error_mark_node)
	      goto skip_comma;
	  }

	/* After the first item, attribute lists look the same as
	   expression lists.  */
	is_attribute_list = false;

      get_comma:;
	/* If the next token isn't a `,', then we are done.  */
	if (cp_lexer_next_token_is_not (parser->lexer, CPP_COMMA))
	  break;

	/* Otherwise, consume the `,' and keep going.  */
	cp_lexer_consume_token (parser->lexer);
      }

  if (!cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>"))
    {
      int ending;

    skip_comma:;
      /* We try and resync to an unnested comma, as that will give the
	 user better diagnostics.  */
      ending = cp_parser_skip_to_closing_parenthesis (parser,
						      /*recovering=*/true,
						      /*or_comma=*/true,
						      /*consume_paren=*/true);
      if (ending < 0)
	goto get_comma;
      if (!ending)
	{
	  parser->greater_than_is_operator_p
	    = saved_greater_than_is_operator_p;
	  return NULL;
	}
    }

  parser->greater_than_is_operator_p
    = saved_greater_than_is_operator_p;

  if (identifier)
    VEC_safe_insert (tree, gc, expression_list, 0, identifier);

  return expression_list;
}

/* Parse a pseudo-destructor-name.

   pseudo-destructor-name:
     :: [opt] nested-name-specifier [opt] type-name :: ~ type-name
     :: [opt] nested-name-specifier template template-id :: ~ type-name
     :: [opt] nested-name-specifier [opt] ~ type-name

   If either of the first two productions is used, sets *SCOPE to the
   TYPE specified before the final `::'.  Otherwise, *SCOPE is set to
   NULL_TREE.  *TYPE is set to the TYPE_DECL for the final type-name,
   or ERROR_MARK_NODE if the parse fails.  */

static void
cp_parser_pseudo_destructor_name (cp_parser* parser,
				  tree* scope,
				  tree* type)
{
  bool nested_name_specifier_p;

  /* Assume that things will not work out.  */
  *type = error_mark_node;

  /* Look for the optional `::' operator.  */
  cp_parser_global_scope_opt (parser, /*current_scope_valid_p=*/true);
  /* Look for the optional nested-name-specifier.  */
  nested_name_specifier_p
    = (cp_parser_nested_name_specifier_opt (parser,
					    /*typename_keyword_p=*/false,
					    /*check_dependency_p=*/true,
					    /*type_p=*/false,
					    /*is_declaration=*/false)
       != NULL_TREE);
  /* Now, if we saw a nested-name-specifier, we might be doing the
     second production.  */
  if (nested_name_specifier_p
      && cp_lexer_next_token_is_keyword (parser->lexer, RID_TEMPLATE))
    {
      /* Consume the `template' keyword.  */
      cp_lexer_consume_token (parser->lexer);
      /* Parse the template-id.  */
      cp_parser_template_id (parser,
			     /*template_keyword_p=*/true,
			     /*check_dependency_p=*/false,
			     /*is_declaration=*/true);
      /* Look for the `::' token.  */
      cp_parser_require (parser, CPP_SCOPE, "%<::%>");
    }
  /* If the next token is not a `~', then there might be some
     additional qualification.  */
  else if (cp_lexer_next_token_is_not (parser->lexer, CPP_COMPL))
    {
      /* At this point, we're looking for "type-name :: ~".  The type-name
	 must not be a class-name, since this is a pseudo-destructor.  So,
	 it must be either an enum-name, or a typedef-name -- both of which
	 are just identifiers.  So, we peek ahead to check that the "::"
	 and "~" tokens are present; if they are not, then we can avoid
	 calling type_name.  */
      if (cp_lexer_peek_token (parser->lexer)->type != CPP_NAME
	  || cp_lexer_peek_nth_token (parser->lexer, 2)->type != CPP_SCOPE
	  || cp_lexer_peek_nth_token (parser->lexer, 3)->type != CPP_COMPL)
	{
	  cp_parser_error (parser, "non-scalar type");
	  return;
	}

      /* Look for the type-name.  */
      *scope = TREE_TYPE (cp_parser_nonclass_name (parser));
      if (*scope == error_mark_node)
	return;

      /* Look for the `::' token.  */
      cp_parser_require (parser, CPP_SCOPE, "%<::%>");
    }
  else
    *scope = NULL_TREE;

  /* Look for the `~'.  */
  cp_parser_require (parser, CPP_COMPL, "%<~%>");
  /* Look for the type-name again.  We are not responsible for
     checking that it matches the first type-name.  */
  *type = cp_parser_nonclass_name (parser);
}

/* Parse a unary-expression.

   unary-expression:
     postfix-expression
     ++ cast-expression
     -- cast-expression
     unary-operator cast-expression
     sizeof unary-expression
     sizeof ( type-id )
     new-expression
     delete-expression

   GNU Extensions:

   unary-expression:
     __extension__ cast-expression
     __alignof__ unary-expression
     __alignof__ ( type-id )
     __real__ cast-expression
     __imag__ cast-expression
     && identifier

   ADDRESS_P is true iff the unary-expression is appearing as the
   operand of the `&' operator.   CAST_P is true if this expression is
   the target of a cast.

   Returns a representation of the expression.  */

static tree
cp_parser_unary_expression (cp_parser *parser, bool address_p, bool cast_p,
			    cp_id_kind * pidk)
{
  cp_token *token;
  enum tree_code unary_operator;

  /* Peek at the next token.  */
  token = cp_lexer_peek_token (parser->lexer);
  /* Some keywords give away the kind of expression.  */
  if (token->type == CPP_KEYWORD)
    {
      enum rid keyword = token->keyword;

      switch (keyword)
	{
	case RID_ALIGNOF:
	case RID_SIZEOF:
	  {
	    tree operand;
	    enum tree_code op;

	    op = keyword == RID_ALIGNOF ? ALIGNOF_EXPR : SIZEOF_EXPR;
	    /* Consume the token.  */
	    cp_lexer_consume_token (parser->lexer);
	    /* Parse the operand.  */
	    operand = cp_parser_sizeof_operand (parser, keyword);

	    if (TYPE_P (operand))
	      return cxx_sizeof_or_alignof_type (operand, op, true);
	    else
	      return cxx_sizeof_or_alignof_expr (operand, op, true);
	  }

	case RID_NEW:
	  return cp_parser_new_expression (parser);

	case RID_DELETE:
	  return cp_parser_delete_expression (parser);

	case RID_EXTENSION:
	  {
	    /* The saved value of the PEDANTIC flag.  */
	    int saved_pedantic;
	    tree expr;

	    /* Save away the PEDANTIC flag.  */
	    cp_parser_extension_opt (parser, &saved_pedantic);
	    /* Parse the cast-expression.  */
	    expr = cp_parser_simple_cast_expression (parser);
	    /* Restore the PEDANTIC flag.  */
	    pedantic = saved_pedantic;

	    return expr;
	  }

	case RID_REALPART:
	case RID_IMAGPART:
	  {
	    tree expression;

	    /* Consume the `__real__' or `__imag__' token.  */
	    cp_lexer_consume_token (parser->lexer);
	    /* Parse the cast-expression.  */
	    expression = cp_parser_simple_cast_expression (parser);
	    /* Create the complete representation.  */
	    return build_x_unary_op ((keyword == RID_REALPART
				      ? REALPART_EXPR : IMAGPART_EXPR),
				     expression,
                                     tf_warning_or_error);
	  }
	  break;

	default:
	  break;
	}
    }

  /* Look for the `:: new' and `:: delete', which also signal the
     beginning of a new-expression, or delete-expression,
     respectively.  If the next token is `::', then it might be one of
     these.  */
  if (cp_lexer_next_token_is (parser->lexer, CPP_SCOPE))
    {
      enum rid keyword;

      /* See if the token after the `::' is one of the keywords in
	 which we're interested.  */
      keyword = cp_lexer_peek_nth_token (parser->lexer, 2)->keyword;
      /* If it's `new', we have a new-expression.  */
      if (keyword == RID_NEW)
	return cp_parser_new_expression (parser);
      /* Similarly, for `delete'.  */
      else if (keyword == RID_DELETE)
	return cp_parser_delete_expression (parser);
    }

  /* Look for a unary operator.  */
  unary_operator = cp_parser_unary_operator (token);
  /* The `++' and `--' operators can be handled similarly, even though
     they are not technically unary-operators in the grammar.  */
  if (unary_operator == ERROR_MARK)
    {
      if (token->type == CPP_PLUS_PLUS)
	unary_operator = PREINCREMENT_EXPR;
      else if (token->type == CPP_MINUS_MINUS)
	unary_operator = PREDECREMENT_EXPR;
      /* Handle the GNU address-of-label extension.  */
      else if (cp_parser_allow_gnu_extensions_p (parser)
	       && token->type == CPP_AND_AND)
	{
	  tree identifier;
	  tree expression;
	  location_t loc = cp_lexer_peek_token (parser->lexer)->location;

	  /* Consume the '&&' token.  */
	  cp_lexer_consume_token (parser->lexer);
	  /* Look for the identifier.  */
	  identifier = cp_parser_identifier (parser);
	  /* Create an expression representing the address.  */
	  expression = finish_label_address_expr (identifier, loc);
	  if (cp_parser_non_integral_constant_expression (parser,
						"the address of a label"))
	    expression = error_mark_node;
	  return expression;
	}
    }
  if (unary_operator != ERROR_MARK)
    {
      tree cast_expression;
      tree expression = error_mark_node;
      const char *non_constant_p = NULL;

      /* Consume the operator token.  */
      token = cp_lexer_consume_token (parser->lexer);
      /* Parse the cast-expression.  */
      cast_expression
	= cp_parser_cast_expression (parser,
				     unary_operator == ADDR_EXPR,
				     /*cast_p=*/false, pidk);
      /* Now, build an appropriate representation.  */
      switch (unary_operator)
	{
	case INDIRECT_REF:
	  non_constant_p = "%<*%>";
	  expression = build_x_indirect_ref (cast_expression, RO_UNARY_STAR,
                                             tf_warning_or_error);
	  break;

	case ADDR_EXPR:
	  non_constant_p = "%<&%>";
	  /* Fall through.  */
	case BIT_NOT_EXPR:
	  expression = build_x_unary_op (unary_operator, cast_expression,
                                         tf_warning_or_error);
	  break;

	case PREINCREMENT_EXPR:
	case PREDECREMENT_EXPR:
	  non_constant_p = (unary_operator == PREINCREMENT_EXPR
			    ? "%<++%>" : "%<--%>");
	  /* Fall through.  */
	case UNARY_PLUS_EXPR:
	case NEGATE_EXPR:
	case TRUTH_NOT_EXPR:
	  expression = finish_unary_op_expr (unary_operator, cast_expression);
	  break;

	default:
	  gcc_unreachable ();
	}

      if (non_constant_p
	  && cp_parser_non_integral_constant_expression (parser,
							 non_constant_p))
	expression = error_mark_node;

      return expression;
    }

  return cp_parser_postfix_expression (parser, address_p, cast_p,
                                       /*member_access_only_p=*/false,
				       pidk);
}

/* Returns ERROR_MARK if TOKEN is not a unary-operator.  If TOKEN is a
   unary-operator, the corresponding tree code is returned.  */

static enum tree_code
cp_parser_unary_operator (cp_token* token)
{
  switch (token->type)
    {
    case CPP_MULT:
      return INDIRECT_REF;

    case CPP_AND:
      return ADDR_EXPR;

    case CPP_PLUS:
      return UNARY_PLUS_EXPR;

    case CPP_MINUS:
      return NEGATE_EXPR;

    case CPP_NOT:
      return TRUTH_NOT_EXPR;

    case CPP_COMPL:
      return BIT_NOT_EXPR;

    default:
      return ERROR_MARK;
    }
}

/* Parse a new-expression.

   new-expression:
     :: [opt] new new-placement [opt] new-type-id new-initializer [opt]
     :: [opt] new new-placement [opt] ( type-id ) new-initializer [opt]

   Returns a representation of the expression.  */

static tree
cp_parser_new_expression (cp_parser* parser)
{
  bool global_scope_p;
  VEC(tree,gc) *placement;
  tree type;
  VEC(tree,gc) *initializer;
  tree nelts;
  tree ret;

  /* Look for the optional `::' operator.  */
  global_scope_p
    = (cp_parser_global_scope_opt (parser,
				   /*current_scope_valid_p=*/false)
       != NULL_TREE);
  /* Look for the `new' operator.  */
  cp_parser_require_keyword (parser, RID_NEW, "%<new%>");
  /* There's no easy way to tell a new-placement from the
     `( type-id )' construct.  */
  cp_parser_parse_tentatively (parser);
  /* Look for a new-placement.  */
  placement = cp_parser_new_placement (parser);
  /* If that didn't work out, there's no new-placement.  */
  if (!cp_parser_parse_definitely (parser))
    {
      if (placement != NULL)
	release_tree_vector (placement);
      placement = NULL;
    }

  /* If the next token is a `(', then we have a parenthesized
     type-id.  */
  if (cp_lexer_next_token_is (parser->lexer, CPP_OPEN_PAREN))
    {
      cp_token *token;
      /* Consume the `('.  */
      cp_lexer_consume_token (parser->lexer);
      /* Parse the type-id.  */
      type = cp_parser_type_id (parser);
      /* Look for the closing `)'.  */
      cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>");
      token = cp_lexer_peek_token (parser->lexer);
      /* There should not be a direct-new-declarator in this production,
	 but GCC used to allowed this, so we check and emit a sensible error
	 message for this case.  */
      if (cp_lexer_next_token_is (parser->lexer, CPP_OPEN_SQUARE))
	{
	  error_at (token->location,
		    "array bound forbidden after parenthesized type-id");
	  inform (token->location, 
		  "try removing the parentheses around the type-id");
	  cp_parser_direct_new_declarator (parser);
	}
      nelts = NULL_TREE;
    }
  /* Otherwise, there must be a new-type-id.  */
  else
    type = cp_parser_new_type_id (parser, &nelts);

  /* If the next token is a `(' or '{', then we have a new-initializer.  */
  if (cp_lexer_next_token_is (parser->lexer, CPP_OPEN_PAREN)
      || cp_lexer_next_token_is (parser->lexer, CPP_OPEN_BRACE))
    initializer = cp_parser_new_initializer (parser);
  else
    initializer = NULL;

  /* A new-expression may not appear in an integral constant
     expression.  */
  if (cp_parser_non_integral_constant_expression (parser, "%<new%>"))
    ret = error_mark_node;
  else
    {
      /* Create a representation of the new-expression.  */
      ret = build_new (&placement, type, nelts, &initializer, global_scope_p,
		       tf_warning_or_error);
    }

  if (placement != NULL)
    release_tree_vector (placement);
  if (initializer != NULL)
    release_tree_vector (initializer);

  return ret;
}

/* Parse a new-placement.

   new-placement:
     ( expression-list )

   Returns the same representation as for an expression-list.  */

static VEC(tree,gc) *
cp_parser_new_placement (cp_parser* parser)
{
  VEC(tree,gc) *expression_list;

  /* Parse the expression-list.  */
  expression_list = (cp_parser_parenthesized_expression_list
		     (parser, false, /*cast_p=*/false, /*allow_expansion_p=*/true,
		      /*non_constant_p=*/NULL));

  return expression_list;
}

/* Parse a new-type-id.

   new-type-id:
     type-specifier-seq new-declarator [opt]

   Returns the TYPE allocated.  If the new-type-id indicates an array
   type, *NELTS is set to the number of elements in the last array
   bound; the TYPE will not include the last array bound.  */

static tree
cp_parser_new_type_id (cp_parser* parser, tree *nelts)
{
  cp_decl_specifier_seq type_specifier_seq;
  cp_declarator *new_declarator;
  cp_declarator *declarator;
  cp_declarator *outer_declarator;
  const char *saved_message;
  tree type;

  /* The type-specifier sequence must not contain type definitions.
     (It cannot contain declarations of new types either, but if they
     are not definitions we will catch that because they are not
     complete.)  */
  saved_message = parser->type_definition_forbidden_message;
  parser->type_definition_forbidden_message
    = G_("types may not be defined in a new-type-id");
  /* Parse the type-specifier-seq.  */
  cp_parser_type_specifier_seq (parser, /*is_declaration=*/false,
				/*is_trailing_return=*/false,
				&type_specifier_seq);
  /* Restore the old message.  */
  parser->type_definition_forbidden_message = saved_message;
  /* Parse the new-declarator.  */
  new_declarator = cp_parser_new_declarator_opt (parser);

  /* Determine the number of elements in the last array dimension, if
     any.  */
  *nelts = NULL_TREE;
  /* Skip down to the last array dimension.  */
  declarator = new_declarator;
  outer_declarator = NULL;
  while (declarator && (declarator->kind == cdk_pointer
			|| declarator->kind == cdk_ptrmem))
    {
      outer_declarator = declarator;
      declarator = declarator->declarator;
    }
  while (declarator
	 && declarator->kind == cdk_array
	 && declarator->declarator
	 && declarator->declarator->kind == cdk_array)
    {
      outer_declarator = declarator;
      declarator = declarator->declarator;
    }

  if (declarator && declarator->kind == cdk_array)
    {
      *nelts = declarator->u.array.bounds;
      if (*nelts == error_mark_node)
	*nelts = integer_one_node;

      if (outer_declarator)
	outer_declarator->declarator = declarator->declarator;
      else
	new_declarator = NULL;
    }

  type = groktypename (&type_specifier_seq, new_declarator, false);
  return type;
}

/* Parse an (optional) new-declarator.

   new-declarator:
     ptr-operator new-declarator [opt]
     direct-new-declarator

   Returns the declarator.  */

static cp_declarator *
cp_parser_new_declarator_opt (cp_parser* parser)
{
  enum tree_code code;
  tree type;
  cp_cv_quals cv_quals;

  /* We don't know if there's a ptr-operator next, or not.  */
  cp_parser_parse_tentatively (parser);
  /* Look for a ptr-operator.  */
  code = cp_parser_ptr_operator (parser, &type, &cv_quals);
  /* If that worked, look for more new-declarators.  */
  if (cp_parser_parse_definitely (parser))
    {
      cp_declarator *declarator;

      /* Parse another optional declarator.  */
      declarator = cp_parser_new_declarator_opt (parser);

      return cp_parser_make_indirect_declarator
	(code, type, cv_quals, declarator);
    }

  /* If the next token is a `[', there is a direct-new-declarator.  */
  if (cp_lexer_next_token_is (parser->lexer, CPP_OPEN_SQUARE))
    return cp_parser_direct_new_declarator (parser);

  return NULL;
}

/* Parse a direct-new-declarator.

   direct-new-declarator:
     [ expression ]
     direct-new-declarator [constant-expression]

   */

static cp_declarator *
cp_parser_direct_new_declarator (cp_parser* parser)
{
  cp_declarator *declarator = NULL;

  while (true)
    {
      tree expression;

      /* Look for the opening `['.  */
      cp_parser_require (parser, CPP_OPEN_SQUARE, "%<[%>");
      /* The first expression is not required to be constant.  */
      if (!declarator)
	{
	  cp_token *token = cp_lexer_peek_token (parser->lexer);
	  expression = cp_parser_expression (parser, /*cast_p=*/false, NULL);
	  /* The standard requires that the expression have integral
	     type.  DR 74 adds enumeration types.  We believe that the
	     real intent is that these expressions be handled like the
	     expression in a `switch' condition, which also allows
	     classes with a single conversion to integral or
	     enumeration type.  */
	  if (!processing_template_decl)
	    {
	      expression
		= build_expr_type_conversion (WANT_INT | WANT_ENUM,
					      expression,
					      /*complain=*/true);
	      if (!expression)
		{
		  error_at (token->location,
			    "expression in new-declarator must have integral "
			    "or enumeration type");
		  expression = error_mark_node;
		}
	    }
	}
      /* But all the other expressions must be.  */
      else
	expression
	  = cp_parser_constant_expression (parser,
					   /*allow_non_constant=*/false,
					   NULL);
      /* Look for the closing `]'.  */
      cp_parser_require (parser, CPP_CLOSE_SQUARE, "%<]%>");

      /* Add this bound to the declarator.  */
      declarator = make_array_declarator (declarator, expression);

      /* If the next token is not a `[', then there are no more
	 bounds.  */
      if (cp_lexer_next_token_is_not (parser->lexer, CPP_OPEN_SQUARE))
	break;
    }

  return declarator;
}

/* Parse a new-initializer.

   new-initializer:
     ( expression-list [opt] )
     braced-init-list

   Returns a representation of the expression-list.  */

static VEC(tree,gc) *
cp_parser_new_initializer (cp_parser* parser)
{
  VEC(tree,gc) *expression_list;

  if (cp_lexer_next_token_is (parser->lexer, CPP_OPEN_BRACE))
    {
      tree t;
      bool expr_non_constant_p;
      maybe_warn_cpp0x (CPP0X_INITIALIZER_LISTS);
      t = cp_parser_braced_list (parser, &expr_non_constant_p);
      CONSTRUCTOR_IS_DIRECT_INIT (t) = 1;
      expression_list = make_tree_vector_single (t);
    }
  else
    expression_list = (cp_parser_parenthesized_expression_list
		       (parser, false, /*cast_p=*/false, /*allow_expansion_p=*/true,
			/*non_constant_p=*/NULL));

  return expression_list;
}

/* Parse a delete-expression.

   delete-expression:
     :: [opt] delete cast-expression
     :: [opt] delete [ ] cast-expression

   Returns a representation of the expression.  */

static tree
cp_parser_delete_expression (cp_parser* parser)
{
  bool global_scope_p;
  bool array_p;
  tree expression;

  /* Look for the optional `::' operator.  */
  global_scope_p
    = (cp_parser_global_scope_opt (parser,
				   /*current_scope_valid_p=*/false)
       != NULL_TREE);
  /* Look for the `delete' keyword.  */
  cp_parser_require_keyword (parser, RID_DELETE, "%<delete%>");
  /* See if the array syntax is in use.  */
  if (cp_lexer_next_token_is (parser->lexer, CPP_OPEN_SQUARE))
    {
      /* Consume the `[' token.  */
      cp_lexer_consume_token (parser->lexer);
      /* Look for the `]' token.  */
      cp_parser_require (parser, CPP_CLOSE_SQUARE, "%<]%>");
      /* Remember that this is the `[]' construct.  */
      array_p = true;
    }
  else
    array_p = false;

  /* Parse the cast-expression.  */
  expression = cp_parser_simple_cast_expression (parser);

  /* A delete-expression may not appear in an integral constant
     expression.  */
  if (cp_parser_non_integral_constant_expression (parser, "%<delete%>"))
    return error_mark_node;

  return delete_sanity (expression, NULL_TREE, array_p, global_scope_p);
}

/* Returns true if TOKEN may start a cast-expression and false
   otherwise.  */

static bool
cp_parser_token_starts_cast_expression (cp_token *token)
{
  switch (token->type)
    {
    case CPP_COMMA:
    case CPP_SEMICOLON:
    case CPP_QUERY:
    case CPP_COLON:
    case CPP_CLOSE_SQUARE:
    case CPP_CLOSE_PAREN:
    case CPP_CLOSE_BRACE:
    case CPP_DOT:
    case CPP_DOT_STAR:
    case CPP_DEREF:
    case CPP_DEREF_STAR:
    case CPP_DIV:
    case CPP_MOD:
    case CPP_LSHIFT:
    case CPP_RSHIFT:
    case CPP_LESS:
    case CPP_GREATER:
    case CPP_LESS_EQ:
    case CPP_GREATER_EQ:
    case CPP_EQ_EQ:
    case CPP_NOT_EQ:
    case CPP_EQ:
    case CPP_MULT_EQ:
    case CPP_DIV_EQ:
    case CPP_MOD_EQ:
    case CPP_PLUS_EQ:
    case CPP_MINUS_EQ:
    case CPP_RSHIFT_EQ:
    case CPP_LSHIFT_EQ:
    case CPP_AND_EQ:
    case CPP_XOR_EQ:
    case CPP_OR_EQ:
    case CPP_XOR:
    case CPP_OR:
    case CPP_OR_OR:
    case CPP_EOF:
      return false;

      /* '[' may start a primary-expression in obj-c++.  */
    case CPP_OPEN_SQUARE:
      return c_dialect_objc ();

    default:
      return true;
    }
}

/* Parse a cast-expression.

   cast-expression:
     unary-expression
     ( type-id ) cast-expression

   ADDRESS_P is true iff the unary-expression is appearing as the
   operand of the `&' operator.   CAST_P is true if this expression is
   the target of a cast.

   Returns a representation of the expression.  */

static tree
cp_parser_cast_expression (cp_parser *parser, bool address_p, bool cast_p,
			   cp_id_kind * pidk)
{
  /* If it's a `(', then we might be looking at a cast.  */
  if (cp_lexer_next_token_is (parser->lexer, CPP_OPEN_PAREN))
    {
      tree type = NULL_TREE;
      tree expr = NULL_TREE;
      bool compound_literal_p;
      const char *saved_message;

      /* There's no way to know yet whether or not this is a cast.
	 For example, `(int (3))' is a unary-expression, while `(int)
	 3' is a cast.  So, we resort to parsing tentatively.  */
      cp_parser_parse_tentatively (parser);
      /* Types may not be defined in a cast.  */
      saved_message = parser->type_definition_forbidden_message;
      parser->type_definition_forbidden_message
	= G_("types may not be defined in casts");
      /* Consume the `('.  */
      cp_lexer_consume_token (parser->lexer);
      /* A very tricky bit is that `(struct S) { 3 }' is a
	 compound-literal (which we permit in C++ as an extension).
	 But, that construct is not a cast-expression -- it is a
	 postfix-expression.  (The reason is that `(struct S) { 3 }.i'
	 is legal; if the compound-literal were a cast-expression,
	 you'd need an extra set of parentheses.)  But, if we parse
	 the type-id, and it happens to be a class-specifier, then we
	 will commit to the parse at that point, because we cannot
	 undo the action that is done when creating a new class.  So,
	 then we cannot back up and do a postfix-expression.

	 Therefore, we scan ahead to the closing `)', and check to see
	 if the token after the `)' is a `{'.  If so, we are not
	 looking at a cast-expression.

	 Save tokens so that we can put them back.  */
      cp_lexer_save_tokens (parser->lexer);
      /* Skip tokens until the next token is a closing parenthesis.
	 If we find the closing `)', and the next token is a `{', then
	 we are looking at a compound-literal.  */
      compound_literal_p
	= (cp_parser_skip_to_closing_parenthesis (parser, false, false,
						  /*consume_paren=*/true)
	   && cp_lexer_next_token_is (parser->lexer, CPP_OPEN_BRACE));
      /* Roll back the tokens we skipped.  */
      cp_lexer_rollback_tokens (parser->lexer);
      /* If we were looking at a compound-literal, simulate an error
	 so that the call to cp_parser_parse_definitely below will
	 fail.  */
      if (compound_literal_p)
	cp_parser_simulate_error (parser);
      else
	{
	  bool saved_in_type_id_in_expr_p = parser->in_type_id_in_expr_p;
	  parser->in_type_id_in_expr_p = true;
	  /* Look for the type-id.  */
	  type = cp_parser_type_id (parser);
	  /* Look for the closing `)'.  */
	  cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>");
	  parser->in_type_id_in_expr_p = saved_in_type_id_in_expr_p;
	}

      /* Restore the saved message.  */
      parser->type_definition_forbidden_message = saved_message;

      /* At this point this can only be either a cast or a
	 parenthesized ctor such as `(T ())' that looks like a cast to
	 function returning T.  */
      if (!cp_parser_error_occurred (parser)
	  && cp_parser_token_starts_cast_expression (cp_lexer_peek_token
						     (parser->lexer)))
	{
	  cp_parser_parse_definitely (parser);
	  expr = cp_parser_cast_expression (parser,
					    /*address_p=*/false,
					    /*cast_p=*/true, pidk);

	  /* Warn about old-style casts, if so requested.  */
	  if (warn_old_style_cast
	      && !in_system_header
	      && !VOID_TYPE_P (type)
	      && current_lang_name != lang_name_c)
	    warning (OPT_Wold_style_cast, "use of old-style cast");

	  /* Only type conversions to integral or enumeration types
	     can be used in constant-expressions.  */
	  if (!cast_valid_in_integral_constant_expression_p (type)
	      && (cp_parser_non_integral_constant_expression
		  (parser,
		   "a cast to a type other than an integral or "
		   "enumeration type")))
	    return error_mark_node;

	  /* Perform the cast.  */
	  expr = build_c_cast (input_location, type, expr);
	  return expr;
	}
      else 
        cp_parser_abort_tentative_parse (parser);
    }

  /* If we get here, then it's not a cast, so it must be a
     unary-expression.  */
  return cp_parser_unary_expression (parser, address_p, cast_p, pidk);
}

/* Parse a binary expression of the general form:

   pm-expression:
     cast-expression
     pm-expression .* cast-expression
     pm-expression ->* cast-expression

   multiplicative-expression:
     pm-expression
     multiplicative-expression * pm-expression
     multiplicative-expression / pm-expression
     multiplicative-expression % pm-expression

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

  GNU Extension:

   relational-expression:
     relational-expression <? shift-expression
     relational-expression >? shift-expression

   equality-expression:
     relational-expression
     equality-expression == relational-expression
     equality-expression != relational-expression

   and-expression:
     equality-expression
     and-expression & equality-expression

   exclusive-or-expression:
     and-expression
     exclusive-or-expression ^ and-expression

   inclusive-or-expression:
     exclusive-or-expression
     inclusive-or-expression | exclusive-or-expression

   logical-and-expression:
     inclusive-or-expression
     logical-and-expression && inclusive-or-expression

   logical-or-expression:
     logical-and-expression
     logical-or-expression || logical-and-expression

   All these are implemented with a single function like:

   binary-expression:
     simple-cast-expression
     binary-expression <token> binary-expression

   CAST_P is true if this expression is the target of a cast.

   The binops_by_token map is used to get the tree codes for each <token> type.
   binary-expressions are associated according to a precedence table.  */

#define TOKEN_PRECEDENCE(token)				     \
(((token->type == CPP_GREATER				     \
   || ((cxx_dialect != cxx98) && token->type == CPP_RSHIFT)) \
  && !parser->greater_than_is_operator_p)		     \
 ? PREC_NOT_OPERATOR					     \
 : binops_by_token[token->type].prec)

static tree
cp_parser_binary_expression (cp_parser* parser, bool cast_p,
			     bool no_toplevel_fold_p,
			     enum cp_parser_prec prec,
			     cp_id_kind * pidk)
{
  cp_parser_expression_stack stack;
  cp_parser_expression_stack_entry *sp = &stack[0];
  tree lhs, rhs;
  cp_token *token;
  enum tree_code tree_type, lhs_type, rhs_type;
  enum cp_parser_prec new_prec, lookahead_prec;
  bool overloaded_p;

  /* Parse the first expression.  */
  lhs = cp_parser_cast_expression (parser, /*address_p=*/false, cast_p, pidk);
  lhs_type = ERROR_MARK;

  for (;;)
    {
      /* Get an operator token.  */
      token = cp_lexer_peek_token (parser->lexer);

      if (warn_cxx0x_compat
          && token->type == CPP_RSHIFT
          && !parser->greater_than_is_operator_p)
        {
          if (warning_at (token->location, OPT_Wc__0x_compat, 
			  "%<>>%> operator will be treated as"
			  " two right angle brackets in C++0x"))
	    inform (token->location,
		    "suggest parentheses around %<>>%> expression");
        }

      new_prec = TOKEN_PRECEDENCE (token);

      /* Popping an entry off the stack means we completed a subexpression:
	 - either we found a token which is not an operator (`>' where it is not
	   an operator, or prec == PREC_NOT_OPERATOR), in which case popping
	   will happen repeatedly;
	 - or, we found an operator which has lower priority.  This is the case
	   where the recursive descent *ascends*, as in `3 * 4 + 5' after
	   parsing `3 * 4'.  */
      if (new_prec <= prec)
	{
	  if (sp == stack)
	    break;
	  else
	    goto pop;
	}

     get_rhs:
      tree_type = binops_by_token[token->type].tree_type;

      /* We used the operator token.  */
      cp_lexer_consume_token (parser->lexer);

      /* For "false && x" or "true || x", x will never be executed;
	 disable warnings while evaluating it.  */
      if (tree_type == TRUTH_ANDIF_EXPR)
	c_inhibit_evaluation_warnings += lhs == truthvalue_false_node;
      else if (tree_type == TRUTH_ORIF_EXPR)
	c_inhibit_evaluation_warnings += lhs == truthvalue_true_node;

      /* Extract another operand.  It may be the RHS of this expression
	 or the LHS of a new, higher priority expression.  */
      rhs = cp_parser_simple_cast_expression (parser);
      rhs_type = ERROR_MARK;

      /* Get another operator token.  Look up its precedence to avoid
	 building a useless (immediately popped) stack entry for common
	 cases such as 3 + 4 + 5 or 3 * 4 + 5.  */
      token = cp_lexer_peek_token (parser->lexer);
      lookahead_prec = TOKEN_PRECEDENCE (token);
      if (lookahead_prec > new_prec)
	{
	  /* ... and prepare to parse the RHS of the new, higher priority
	     expression.  Since precedence levels on the stack are
	     monotonically increasing, we do not have to care about
	     stack overflows.  */
	  sp->prec = prec;
	  sp->tree_type = tree_type;
	  sp->lhs = lhs;
	  sp->lhs_type = lhs_type;
	  sp++;
	  lhs = rhs;
	  lhs_type = rhs_type;
	  prec = new_prec;
	  new_prec = lookahead_prec;
	  goto get_rhs;

	 pop:
	  lookahead_prec = new_prec;
	  /* If the stack is not empty, we have parsed into LHS the right side
	     (`4' in the example above) of an expression we had suspended.
	     We can use the information on the stack to recover the LHS (`3')
	     from the stack together with the tree code (`MULT_EXPR'), and
	     the precedence of the higher level subexpression
	     (`PREC_ADDITIVE_EXPRESSION').  TOKEN is the CPP_PLUS token,
	     which will be used to actually build the additive expression.  */
	  --sp;
	  prec = sp->prec;
	  tree_type = sp->tree_type;
	  rhs = lhs;
	  rhs_type = lhs_type;
	  lhs = sp->lhs;
	  lhs_type = sp->lhs_type;
	}

      /* Undo the disabling of warnings done above.  */
      if (tree_type == TRUTH_ANDIF_EXPR)
	c_inhibit_evaluation_warnings -= lhs == truthvalue_false_node;
      else if (tree_type == TRUTH_ORIF_EXPR)
	c_inhibit_evaluation_warnings -= lhs == truthvalue_true_node;

      overloaded_p = false;
      /* ??? Currently we pass lhs_type == ERROR_MARK and rhs_type ==
	 ERROR_MARK for everything that is not a binary expression.
	 This makes warn_about_parentheses miss some warnings that
	 involve unary operators.  For unary expressions we should
	 pass the correct tree_code unless the unary expression was
	 surrounded by parentheses.
      */
      if (no_toplevel_fold_p
	  && lookahead_prec <= prec
	  && sp == stack
	  && TREE_CODE_CLASS (tree_type) == tcc_comparison)
	lhs = build2 (tree_type, boolean_type_node, lhs, rhs);
      else
	lhs = build_x_binary_op (tree_type, lhs, lhs_type, rhs, rhs_type,
				 &overloaded_p, tf_warning_or_error);
      lhs_type = tree_type;

      /* If the binary operator required the use of an overloaded operator,
	 then this expression cannot be an integral constant-expression.
	 An overloaded operator can be used even if both operands are
	 otherwise permissible in an integral constant-expression if at
	 least one of the operands is of enumeration type.  */

      if (overloaded_p
	  && (cp_parser_non_integral_constant_expression
	      (parser, "calls to overloaded operators")))
	return error_mark_node;
    }

  return lhs;
}


/* Parse the `? expression : assignment-expression' part of a
   conditional-expression.  The LOGICAL_OR_EXPR is the
   logical-or-expression that started the conditional-expression.
   Returns a representation of the entire conditional-expression.

   This routine is used by cp_parser_assignment_expression.

     ? expression : assignment-expression

   GNU Extensions:

     ? : assignment-expression */

static tree
cp_parser_question_colon_clause (cp_parser* parser, tree logical_or_expr)
{
  tree expr;
  tree assignment_expr;

  /* Consume the `?' token.  */
  cp_lexer_consume_token (parser->lexer);
  if (cp_parser_allow_gnu_extensions_p (parser)
      && cp_lexer_next_token_is (parser->lexer, CPP_COLON))
    {
      /* Implicit true clause.  */
      expr = NULL_TREE;
      c_inhibit_evaluation_warnings += logical_or_expr == truthvalue_true_node;
    }
  else
    {
      /* Parse the expression.  */
      c_inhibit_evaluation_warnings += logical_or_expr == truthvalue_false_node;
      expr = cp_parser_expression (parser, /*cast_p=*/false, NULL);
      c_inhibit_evaluation_warnings +=
	((logical_or_expr == truthvalue_true_node)
	 - (logical_or_expr == truthvalue_false_node));
    }

  /* The next token should be a `:'.  */
  cp_parser_require (parser, CPP_COLON, "%<:%>");
  /* Parse the assignment-expression.  */
  assignment_expr = cp_parser_assignment_expression (parser, /*cast_p=*/false, NULL);
  c_inhibit_evaluation_warnings -= logical_or_expr == truthvalue_true_node;

  /* Build the conditional-expression.  */
  return build_x_conditional_expr (logical_or_expr,
				   expr,
				   assignment_expr,
                                   tf_warning_or_error);
}

/* Parse an assignment-expression.

   assignment-expression:
     conditional-expression
     logical-or-expression assignment-operator assignment_expression
     throw-expression

   CAST_P is true if this expression is the target of a cast.

   Returns a representation for the expression.  */

static tree
cp_parser_assignment_expression (cp_parser* parser, bool cast_p,
				 cp_id_kind * pidk)
{
  tree expr;

  /* If the next token is the `throw' keyword, then we're looking at
     a throw-expression.  */
  if (cp_lexer_next_token_is_keyword (parser->lexer, RID_THROW))
    expr = cp_parser_throw_expression (parser);
  /* Otherwise, it must be that we are looking at a
     logical-or-expression.  */
  else
    {
      /* Parse the binary expressions (logical-or-expression).  */
      expr = cp_parser_binary_expression (parser, cast_p, false,
					  PREC_NOT_OPERATOR, pidk);
      /* If the next token is a `?' then we're actually looking at a
	 conditional-expression.  */
      if (cp_lexer_next_token_is (parser->lexer, CPP_QUERY))
	return cp_parser_question_colon_clause (parser, expr);
      else
	{
	  enum tree_code assignment_operator;

	  /* If it's an assignment-operator, we're using the second
	     production.  */
	  assignment_operator
	    = cp_parser_assignment_operator_opt (parser);
	  if (assignment_operator != ERROR_MARK)
	    {
	      bool non_constant_p;

	      /* Parse the right-hand side of the assignment.  */
	      tree rhs = cp_parser_initializer_clause (parser, &non_constant_p);

	      if (BRACE_ENCLOSED_INITIALIZER_P (rhs))
		maybe_warn_cpp0x (CPP0X_INITIALIZER_LISTS);

	      /* An assignment may not appear in a
		 constant-expression.  */
	      if (cp_parser_non_integral_constant_expression (parser,
							      "an assignment"))
		return error_mark_node;
	      /* Build the assignment expression.  */
	      expr = build_x_modify_expr (expr,
					  assignment_operator,
					  rhs,
					  tf_warning_or_error);
	    }
	}
    }

  return expr;
}

/* Parse an (optional) assignment-operator.

   assignment-operator: one of
     = *= /= %= += -= >>= <<= &= ^= |=

   GNU Extension:

   assignment-operator: one of
     <?= >?=

   If the next token is an assignment operator, the corresponding tree
   code is returned, and the token is consumed.  For example, for
   `+=', PLUS_EXPR is returned.  For `=' itself, the code returned is
   NOP_EXPR.  For `/', TRUNC_DIV_EXPR is returned; for `%',
   TRUNC_MOD_EXPR is returned.  If TOKEN is not an assignment
   operator, ERROR_MARK is returned.  */

static enum tree_code
cp_parser_assignment_operator_opt (cp_parser* parser)
{
  enum tree_code op;
  cp_token *token;

  /* Peek at the next token.  */
  token = cp_lexer_peek_token (parser->lexer);

  switch (token->type)
    {
    case CPP_EQ:
      op = NOP_EXPR;
      break;

    case CPP_MULT_EQ:
      op = MULT_EXPR;
      break;

    case CPP_DIV_EQ:
      op = TRUNC_DIV_EXPR;
      break;

    case CPP_MOD_EQ:
      op = TRUNC_MOD_EXPR;
      break;

    case CPP_PLUS_EQ:
      op = PLUS_EXPR;
      break;

    case CPP_MINUS_EQ:
      op = MINUS_EXPR;
      break;

    case CPP_RSHIFT_EQ:
      op = RSHIFT_EXPR;
      break;

    case CPP_LSHIFT_EQ:
      op = LSHIFT_EXPR;
      break;

    case CPP_AND_EQ:
      op = BIT_AND_EXPR;
      break;

    case CPP_XOR_EQ:
      op = BIT_XOR_EXPR;
      break;

    case CPP_OR_EQ:
      op = BIT_IOR_EXPR;
      break;

    default:
      /* Nothing else is an assignment operator.  */
      op = ERROR_MARK;
    }

  /* If it was an assignment operator, consume it.  */
  if (op != ERROR_MARK)
    cp_lexer_consume_token (parser->lexer);

  return op;
}

/* Parse an expression.

   expression:
     assignment-expression
     expression , assignment-expression

   CAST_P is true if this expression is the target of a cast.

   Returns a representation of the expression.  */

static tree
cp_parser_expression (cp_parser* parser, bool cast_p, cp_id_kind * pidk)
{
  tree expression = NULL_TREE;

  while (true)
    {
      tree assignment_expression;

      /* Parse the next assignment-expression.  */
      assignment_expression
	= cp_parser_assignment_expression (parser, cast_p, pidk);
      /* If this is the first assignment-expression, we can just
	 save it away.  */
      if (!expression)
	expression = assignment_expression;
      else
	expression = build_x_compound_expr (expression,
					    assignment_expression,
                                            tf_warning_or_error);
      /* If the next token is not a comma, then we are done with the
	 expression.  */
      if (cp_lexer_next_token_is_not (parser->lexer, CPP_COMMA))
	break;
      /* Consume the `,'.  */
      cp_lexer_consume_token (parser->lexer);
      /* A comma operator cannot appear in a constant-expression.  */
      if (cp_parser_non_integral_constant_expression (parser,
						      "a comma operator"))
	expression = error_mark_node;
    }

  return expression;
}

/* Parse a constant-expression.

   constant-expression:
     conditional-expression

  If ALLOW_NON_CONSTANT_P a non-constant expression is silently
  accepted.  If ALLOW_NON_CONSTANT_P is true and the expression is not
  constant, *NON_CONSTANT_P is set to TRUE.  If ALLOW_NON_CONSTANT_P
  is false, NON_CONSTANT_P should be NULL.  */

static tree
cp_parser_constant_expression (cp_parser* parser,
			       bool allow_non_constant_p,
			       bool *non_constant_p)
{
  bool saved_integral_constant_expression_p;
  bool saved_allow_non_integral_constant_expression_p;
  bool saved_non_integral_constant_expression_p;
  tree expression;

  /* It might seem that we could simply parse the
     conditional-expression, and then check to see if it were
     TREE_CONSTANT.  However, an expression that is TREE_CONSTANT is
     one that the compiler can figure out is constant, possibly after
     doing some simplifications or optimizations.  The standard has a
     precise definition of constant-expression, and we must honor
     that, even though it is somewhat more restrictive.

     For example:

       int i[(2, 3)];

     is not a legal declaration, because `(2, 3)' is not a
     constant-expression.  The `,' operator is forbidden in a
     constant-expression.  However, GCC's constant-folding machinery
     will fold this operation to an INTEGER_CST for `3'.  */

  /* Save the old settings.  */
  saved_integral_constant_expression_p = parser->integral_constant_expression_p;
  saved_allow_non_integral_constant_expression_p
    = parser->allow_non_integral_constant_expression_p;
  saved_non_integral_constant_expression_p = parser->non_integral_constant_expression_p;
  /* We are now parsing a constant-expression.  */
  parser->integral_constant_expression_p = true;
  parser->allow_non_integral_constant_expression_p = allow_non_constant_p;
  parser->non_integral_constant_expression_p = false;
  /* Although the grammar says "conditional-expression", we parse an
     "assignment-expression", which also permits "throw-expression"
     and the use of assignment operators.  In the case that
     ALLOW_NON_CONSTANT_P is false, we get better errors than we would
     otherwise.  In the case that ALLOW_NON_CONSTANT_P is true, it is
     actually essential that we look for an assignment-expression.
     For example, cp_parser_initializer_clauses uses this function to
     determine whether a particular assignment-expression is in fact
     constant.  */
  expression = cp_parser_assignment_expression (parser, /*cast_p=*/false, NULL);
  /* Restore the old settings.  */
  parser->integral_constant_expression_p
    = saved_integral_constant_expression_p;
  parser->allow_non_integral_constant_expression_p
    = saved_allow_non_integral_constant_expression_p;
  if (allow_non_constant_p)
    *non_constant_p = parser->non_integral_constant_expression_p;
  else if (parser->non_integral_constant_expression_p)
    expression = error_mark_node;
  parser->non_integral_constant_expression_p
    = saved_non_integral_constant_expression_p;

  return expression;
}

/* Parse __builtin_offsetof.

   offsetof-expression:
     "__builtin_offsetof" "(" type-id "," offsetof-member-designator ")"

   offsetof-member-designator:
     id-expression
     | offsetof-member-designator "." id-expression
     | offsetof-member-designator "[" expression "]"
     | offsetof-member-designator "->" id-expression  */

static tree
cp_parser_builtin_offsetof (cp_parser *parser)
{
  int save_ice_p, save_non_ice_p;
  tree type, expr;
  cp_id_kind dummy;
  cp_token *token;

  /* We're about to accept non-integral-constant things, but will
     definitely yield an integral constant expression.  Save and
     restore these values around our local parsing.  */
  save_ice_p = parser->integral_constant_expression_p;
  save_non_ice_p = parser->non_integral_constant_expression_p;

  /* Consume the "__builtin_offsetof" token.  */
  cp_lexer_consume_token (parser->lexer);
  /* Consume the opening `('.  */
  cp_parser_require (parser, CPP_OPEN_PAREN, "%<(%>");
  /* Parse the type-id.  */
  type = cp_parser_type_id (parser);
  /* Look for the `,'.  */
  cp_parser_require (parser, CPP_COMMA, "%<,%>");
  token = cp_lexer_peek_token (parser->lexer);

  /* Build the (type *)null that begins the traditional offsetof macro.  */
  expr = build_static_cast (build_pointer_type (type), null_pointer_node,
                            tf_warning_or_error);

  /* Parse the offsetof-member-designator.  We begin as if we saw "expr->".  */
  expr = cp_parser_postfix_dot_deref_expression (parser, CPP_DEREF, expr,
						 true, &dummy, token->location);
  while (true)
    {
      token = cp_lexer_peek_token (parser->lexer);
      switch (token->type)
	{
	case CPP_OPEN_SQUARE:
	  /* offsetof-member-designator "[" expression "]" */
	  expr = cp_parser_postfix_open_square_expression (parser, expr, true);
	  break;

	case CPP_DEREF:
	  /* offsetof-member-designator "->" identifier */
	  expr = grok_array_decl (expr, integer_zero_node);
	  /* FALLTHRU */

	case CPP_DOT:
	  /* offsetof-member-designator "." identifier */
	  cp_lexer_consume_token (parser->lexer);
	  expr = cp_parser_postfix_dot_deref_expression (parser, CPP_DOT,
							 expr, true, &dummy,
							 token->location);
	  break;

	case CPP_CLOSE_PAREN:
	  /* Consume the ")" token.  */
	  cp_lexer_consume_token (parser->lexer);
	  goto success;

	default:
	  /* Error.  We know the following require will fail, but
	     that gives the proper error message.  */
	  cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>");
	  cp_parser_skip_to_closing_parenthesis (parser, true, false, true);
	  expr = error_mark_node;
	  goto failure;
	}
    }

 success:
  /* If we're processing a template, we can't finish the semantics yet.
     Otherwise we can fold the entire expression now.  */
  if (processing_template_decl)
    expr = build1 (OFFSETOF_EXPR, size_type_node, expr);
  else
    expr = finish_offsetof (expr);

 failure:
  parser->integral_constant_expression_p = save_ice_p;
  parser->non_integral_constant_expression_p = save_non_ice_p;

  return expr;
}

/* Parse a trait expression.  */

static tree
cp_parser_trait_expr (cp_parser* parser, enum rid keyword)
{
  cp_trait_kind kind;
  tree type1, type2 = NULL_TREE;
  bool binary = false;
  cp_decl_specifier_seq decl_specs;

  switch (keyword)
    {
    case RID_HAS_NOTHROW_ASSIGN:
      kind = CPTK_HAS_NOTHROW_ASSIGN;
      break;
    case RID_HAS_NOTHROW_CONSTRUCTOR:
      kind = CPTK_HAS_NOTHROW_CONSTRUCTOR;
      break;
    case RID_HAS_NOTHROW_COPY:
      kind = CPTK_HAS_NOTHROW_COPY;
      break;
    case RID_HAS_TRIVIAL_ASSIGN:
      kind = CPTK_HAS_TRIVIAL_ASSIGN;
      break;
    case RID_HAS_TRIVIAL_CONSTRUCTOR:
      kind = CPTK_HAS_TRIVIAL_CONSTRUCTOR;
      break;
    case RID_HAS_TRIVIAL_COPY:
      kind = CPTK_HAS_TRIVIAL_COPY;
      break;
    case RID_HAS_TRIVIAL_DESTRUCTOR:
      kind = CPTK_HAS_TRIVIAL_DESTRUCTOR;
      break;
    case RID_HAS_VIRTUAL_DESTRUCTOR:
      kind = CPTK_HAS_VIRTUAL_DESTRUCTOR;
      break;
    case RID_IS_ABSTRACT:
      kind = CPTK_IS_ABSTRACT;
      break;
    case RID_IS_BASE_OF:
      kind = CPTK_IS_BASE_OF;
      binary = true;
      break;
    case RID_IS_CLASS:
      kind = CPTK_IS_CLASS;
      break;
    case RID_IS_CONVERTIBLE_TO:
      kind = CPTK_IS_CONVERTIBLE_TO;
      binary = true;
      break;
    case RID_IS_EMPTY:
      kind = CPTK_IS_EMPTY;
      break;
    case RID_IS_ENUM:
      kind = CPTK_IS_ENUM;
      break;
    case RID_IS_POD:
      kind = CPTK_IS_POD;
      break;
    case RID_IS_POLYMORPHIC:
      kind = CPTK_IS_POLYMORPHIC;
      break;
    case RID_IS_STD_LAYOUT:
      kind = CPTK_IS_STD_LAYOUT;
      break;
    case RID_IS_TRIVIAL:
      kind = CPTK_IS_TRIVIAL;
      break;
    case RID_IS_UNION:
      kind = CPTK_IS_UNION;
      break;
    default:
      gcc_unreachable ();
    }

  /* Consume the token.  */
  cp_lexer_consume_token (parser->lexer);

  cp_parser_require (parser, CPP_OPEN_PAREN, "%<(%>");

  type1 = cp_parser_type_id (parser);

  if (type1 == error_mark_node)
    return error_mark_node;

  /* Build a trivial decl-specifier-seq.  */
  clear_decl_specs (&decl_specs);
  decl_specs.type = type1;

  /* Call grokdeclarator to figure out what type this is.  */
  type1 = grokdeclarator (NULL, &decl_specs, TYPENAME,
			  /*initialized=*/0, /*attrlist=*/NULL);

  if (binary)
    {
      cp_parser_require (parser, CPP_COMMA, "%<,%>");
 
      type2 = cp_parser_type_id (parser);

      if (type2 == error_mark_node)
	return error_mark_node;

      /* Build a trivial decl-specifier-seq.  */
      clear_decl_specs (&decl_specs);
      decl_specs.type = type2;

      /* Call grokdeclarator to figure out what type this is.  */
      type2 = grokdeclarator (NULL, &decl_specs, TYPENAME,
			      /*initialized=*/0, /*attrlist=*/NULL);
    }

  cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>");

  /* Complete the trait expression, which may mean either processing
     the trait expr now or saving it for template instantiation.  */
  return finish_trait_expr (kind, type1, type2);
}

/* Lambdas that appear in variable initializer or default argument scope
   get that in their mangling, so we need to record it.  We might as well
   use the count for function and namespace scopes as well.  */
static GTY(()) tree lambda_scope;
static GTY(()) int lambda_count;
typedef struct GTY(()) tree_int
{
  tree t;
  int i;
} tree_int;
DEF_VEC_O(tree_int);
DEF_VEC_ALLOC_O(tree_int,gc);
static GTY(()) VEC(tree_int,gc) *lambda_scope_stack;

static void
start_lambda_scope (tree decl)
{
  tree_int ti;
  gcc_assert (decl);
  /* Once we're inside a function, we ignore other scopes and just push
     the function again so that popping works properly.  */
  if (current_function_decl && TREE_CODE (decl) != FUNCTION_DECL)
    decl = current_function_decl;
  ti.t = lambda_scope;
  ti.i = lambda_count;
  VEC_safe_push (tree_int, gc, lambda_scope_stack, &ti);
  if (lambda_scope != decl)
    {
      /* Don't reset the count if we're still in the same function.  */
      lambda_scope = decl;
      lambda_count = 0;
    }
}

static void
record_lambda_scope (tree lambda)
{
  LAMBDA_EXPR_EXTRA_SCOPE (lambda) = lambda_scope;
  LAMBDA_EXPR_DISCRIMINATOR (lambda) = lambda_count++;
}

static void
finish_lambda_scope (void)
{
  tree_int *p = VEC_last (tree_int, lambda_scope_stack);
  if (lambda_scope != p->t)
    {
      lambda_scope = p->t;
      lambda_count = p->i;
    }
  VEC_pop (tree_int, lambda_scope_stack);
}

/* Parse a lambda expression.

   lambda-expression:
     lambda-introducer lambda-declarator [opt] compound-statement

   Returns a representation of the expression.  */

static tree
cp_parser_lambda_expression (cp_parser* parser)
{
  tree lambda_expr = build_lambda_expr ();
  tree type;

  LAMBDA_EXPR_LOCATION (lambda_expr)
    = cp_lexer_peek_token (parser->lexer)->location;

  /* We may be in the middle of deferred access check.  Disable
     it now.  */
  push_deferring_access_checks (dk_no_deferred);

  cp_parser_lambda_introducer (parser, lambda_expr);

  type = begin_lambda_type (lambda_expr);

  record_lambda_scope (lambda_expr);

  /* Do this again now that LAMBDA_EXPR_EXTRA_SCOPE is set.  */
  determine_visibility (TYPE_NAME (type));

  /* Now that we've started the type, add the capture fields for any
     explicit captures.  */
  register_capture_members (LAMBDA_EXPR_CAPTURE_LIST (lambda_expr));

  {
    /* Inside the class, surrounding template-parameter-lists do not apply.  */
    unsigned int saved_num_template_parameter_lists
        = parser->num_template_parameter_lists;

    parser->num_template_parameter_lists = 0;

    /* By virtue of defining a local class, a lambda expression has access to
       the private variables of enclosing classes.  */

    cp_parser_lambda_declarator_opt (parser, lambda_expr);

    cp_parser_lambda_body (parser, lambda_expr);

    /* The capture list was built up in reverse order; fix that now.  */
    {
      tree newlist = NULL_TREE;
      tree elt, next;

      for (elt = LAMBDA_EXPR_CAPTURE_LIST (lambda_expr);
	   elt; elt = next)
	{
	  tree field = TREE_PURPOSE (elt);
	  char *buf;

	  next = TREE_CHAIN (elt);
	  TREE_CHAIN (elt) = newlist;
	  newlist = elt;

	  /* Also add __ to the beginning of the field name so that code
	     outside the lambda body can't see the captured name.  We could
	     just remove the name entirely, but this is more useful for
	     debugging.  */
	  if (field == LAMBDA_EXPR_THIS_CAPTURE (lambda_expr))
	    /* The 'this' capture already starts with __.  */
	    continue;

	  buf = (char *) alloca (IDENTIFIER_LENGTH (DECL_NAME (field)) + 3);
	  buf[1] = buf[0] = '_';
	  memcpy (buf + 2, IDENTIFIER_POINTER (DECL_NAME (field)),
		  IDENTIFIER_LENGTH (DECL_NAME (field)) + 1);
	  DECL_NAME (field) = get_identifier (buf);
	}
      LAMBDA_EXPR_CAPTURE_LIST (lambda_expr) = newlist;
    }

    maybe_add_lambda_conv_op (type);

    type = finish_struct (type, /*attributes=*/NULL_TREE);

    parser->num_template_parameter_lists = saved_num_template_parameter_lists;
  }

  pop_deferring_access_checks ();

  return build_lambda_object (lambda_expr);
}

/* Parse the beginning of a lambda expression.

   lambda-introducer:
     [ lambda-capture [opt] ]

   LAMBDA_EXPR is the current representation of the lambda expression.  */

static void
cp_parser_lambda_introducer (cp_parser* parser, tree lambda_expr)
{
  /* Need commas after the first capture.  */
  bool first = true;

  /* Eat the leading `['.  */
  cp_parser_require (parser, CPP_OPEN_SQUARE, "%<[%>");

  /* Record default capture mode.  "[&" "[=" "[&," "[=,"  */
  if (cp_lexer_next_token_is (parser->lexer, CPP_AND)
      && cp_lexer_peek_nth_token (parser->lexer, 2)->type != CPP_NAME)
    LAMBDA_EXPR_DEFAULT_CAPTURE_MODE (lambda_expr) = CPLD_REFERENCE;
  else if (cp_lexer_next_token_is (parser->lexer, CPP_EQ))
    LAMBDA_EXPR_DEFAULT_CAPTURE_MODE (lambda_expr) = CPLD_COPY;

  if (LAMBDA_EXPR_DEFAULT_CAPTURE_MODE (lambda_expr) != CPLD_NONE)
    {
      cp_lexer_consume_token (parser->lexer);
      first = false;
    }

  while (cp_lexer_next_token_is_not (parser->lexer, CPP_CLOSE_SQUARE))
    {
      cp_token* capture_token;
      tree capture_id;
      tree capture_init_expr;
      cp_id_kind idk = CP_ID_KIND_NONE;
      bool explicit_init_p = false;

      enum capture_kind_type
      {
	BY_COPY,
	BY_REFERENCE
      };
      enum capture_kind_type capture_kind = BY_COPY;

      if (cp_lexer_next_token_is (parser->lexer, CPP_EOF))
	{
	  error ("expected end of capture-list");
	  return;
	}

      if (first)
	first = false;
      else
	cp_parser_require (parser, CPP_COMMA, "%<,%>");

      /* Possibly capture `this'.  */
      if (cp_lexer_next_token_is_keyword (parser->lexer, RID_THIS))
	{
	  cp_lexer_consume_token (parser->lexer);
	  add_capture (lambda_expr,
		       /*id=*/get_identifier ("__this"),
		       /*initializer=*/finish_this_expr(),
		       /*by_reference_p=*/false,
		       explicit_init_p);
	  continue;
	}

      /* Remember whether we want to capture as a reference or not.  */
      if (cp_lexer_next_token_is (parser->lexer, CPP_AND))
	{
	  capture_kind = BY_REFERENCE;
	  cp_lexer_consume_token (parser->lexer);
	}

      /* Get the identifier.  */
      capture_token = cp_lexer_peek_token (parser->lexer);
      capture_id = cp_parser_identifier (parser);

      if (capture_id == error_mark_node)
	/* Would be nice to have a cp_parser_skip_to_closing_x for general
           delimiters, but I modified this to stop on unnested ']' as well.  It
           was already changed to stop on unnested '}', so the
           "closing_parenthesis" name is no more misleading with my change.  */
	{
	  cp_parser_skip_to_closing_parenthesis (parser,
						 /*recovering=*/true,
						 /*or_comma=*/true,
						 /*consume_paren=*/true);
	  break;
	}

      /* Find the initializer for this capture.  */
      if (cp_lexer_next_token_is (parser->lexer, CPP_EQ))
	{
	  /* An explicit expression exists.  */
	  cp_lexer_consume_token (parser->lexer);
          pedwarn (input_location, OPT_pedantic,
                   "ISO C++ does not allow initializers "
                   "in lambda expression capture lists");
	  capture_init_expr = cp_parser_assignment_expression (parser,
							       /*cast_p=*/true,
							       &idk);
	  explicit_init_p = true;
	}
      else
	{
	  const char* error_msg;

	  /* Turn the identifier into an id-expression.  */
	  capture_init_expr
            = cp_parser_lookup_name
                (parser,
		 capture_id,
                 none_type,
                 /*is_template=*/false,
                 /*is_namespace=*/false,
                 /*check_dependency=*/true,
                 /*ambiguous_decls=*/NULL,
                 capture_token->location);

	  capture_init_expr
            = finish_id_expression
                (capture_id,
		 capture_init_expr,
                 parser->scope,
                 &idk,
                 /*integral_constant_expression_p=*/false,
                 /*allow_non_integral_constant_expression_p=*/false,
                 /*non_integral_constant_expression_p=*/NULL,
                 /*template_p=*/false,
                 /*done=*/true,
                 /*address_p=*/false,
                 /*template_arg_p=*/false,
                 &error_msg,
                 capture_token->location);
	}

      if (TREE_CODE (capture_init_expr) == IDENTIFIER_NODE)
	capture_init_expr
	  = unqualified_name_lookup_error (capture_init_expr);

      add_capture (lambda_expr,
		   capture_id,
		   capture_init_expr,
		   /*by_reference_p=*/capture_kind == BY_REFERENCE,
		   explicit_init_p);
    }

  cp_parser_require (parser, CPP_CLOSE_SQUARE, "%<]%>");
}

/* Parse the (optional) middle of a lambda expression.

   lambda-declarator:
     ( parameter-declaration-clause [opt] )
       attribute-specifier [opt]
       mutable [opt]
       exception-specification [opt]
       lambda-return-type-clause [opt]

   LAMBDA_EXPR is the current representation of the lambda expression.  */

static void
cp_parser_lambda_declarator_opt (cp_parser* parser, tree lambda_expr)
{
  /* 5.1.1.4 of the standard says:
       If a lambda-expression does not include a lambda-declarator, it is as if
       the lambda-declarator were ().
     This means an empty parameter list, no attributes, and no exception
     specification.  */
  tree param_list = void_list_node;
  tree attributes = NULL_TREE;
  tree exception_spec = NULL_TREE;
  tree t;

  /* The lambda-declarator is optional, but must begin with an opening
     parenthesis if present.  */
  if (cp_lexer_next_token_is (parser->lexer, CPP_OPEN_PAREN))
    {
      cp_lexer_consume_token (parser->lexer);

      begin_scope (sk_function_parms, /*entity=*/NULL_TREE);

      /* Parse parameters.  */
      param_list = cp_parser_parameter_declaration_clause (parser);

      /* Default arguments shall not be specified in the
	 parameter-declaration-clause of a lambda-declarator.  */
      for (t = param_list; t; t = TREE_CHAIN (t))
	if (TREE_PURPOSE (t))
	  pedwarn (DECL_SOURCE_LOCATION (TREE_VALUE (t)), OPT_pedantic,
		   "default argument specified for lambda parameter");

      cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>");

      attributes = cp_parser_attributes_opt (parser);

      /* Parse optional `mutable' keyword.  */
      if (cp_lexer_next_token_is_keyword (parser->lexer, RID_MUTABLE))
        {
          cp_lexer_consume_token (parser->lexer);
          LAMBDA_EXPR_MUTABLE_P (lambda_expr) = 1;
        }

      /* Parse optional exception specification.  */
      exception_spec = cp_parser_exception_specification_opt (parser);

      /* Parse optional trailing return type.  */
      if (cp_lexer_next_token_is (parser->lexer, CPP_DEREF))
        {
          cp_lexer_consume_token (parser->lexer);
          LAMBDA_EXPR_RETURN_TYPE (lambda_expr) = cp_parser_type_id (parser);
        }

      /* The function parameters must be in scope all the way until after the
         trailing-return-type in case of decltype.  */
      for (t = current_binding_level->names; t; t = TREE_CHAIN (t))
	pop_binding (DECL_NAME (t), t);

      leave_scope ();
    }

  /* Create the function call operator.

     Messing with declarators like this is no uglier than building up the
     FUNCTION_DECL by hand, and this is less likely to get out of sync with
     other code.  */
  {
    cp_decl_specifier_seq return_type_specs;
    cp_declarator* declarator;
    tree fco;
    int quals;
    void *p;

    clear_decl_specs (&return_type_specs);
    if (LAMBDA_EXPR_RETURN_TYPE (lambda_expr))
      return_type_specs.type = LAMBDA_EXPR_RETURN_TYPE (lambda_expr);
    else
      /* Maybe we will deduce the return type later, but we can use void
	 as a placeholder return type anyways.  */
      return_type_specs.type = void_type_node;

    p = obstack_alloc (&declarator_obstack, 0);

    declarator = make_id_declarator (NULL_TREE, ansi_opname (CALL_EXPR),
				     sfk_none);

    quals = (LAMBDA_EXPR_MUTABLE_P (lambda_expr)
	     ? TYPE_UNQUALIFIED : TYPE_QUAL_CONST);
    declarator = make_call_declarator (declarator, param_list, quals,
				       exception_spec,
                                       /*late_return_type=*/NULL_TREE);

    fco = grokmethod (&return_type_specs,
		      declarator,
		      attributes);
    DECL_INITIALIZED_IN_CLASS_P (fco) = 1;
    DECL_ARTIFICIAL (fco) = 1;

    finish_member_declaration (fco);

    obstack_free (&declarator_obstack, p);
  }
}

/* Parse the body of a lambda expression, which is simply

   compound-statement

   but which requires special handling.
   LAMBDA_EXPR is the current representation of the lambda expression.  */

static void
cp_parser_lambda_body (cp_parser* parser, tree lambda_expr)
{
  bool nested = (current_function_decl != NULL_TREE);
  if (nested)
    push_function_context ();

  /* Finish the function call operator
     - class_specifier
     + late_parsing_for_member
     + function_definition_after_declarator
     + ctor_initializer_opt_and_function_body  */
  {
    tree fco = lambda_function (lambda_expr);
    tree body;
    bool done = false;

    /* Let the front end know that we are going to be defining this
       function.  */
    start_preparsed_function (fco,
			      NULL_TREE,
			      SF_PRE_PARSED | SF_INCLASS_INLINE);

    start_lambda_scope (fco);
    body = begin_function_body ();

    /* 5.1.1.4 of the standard says:
         If a lambda-expression does not include a trailing-return-type, it
         is as if the trailing-return-type denotes the following type:
	  * if the compound-statement is of the form
               { return attribute-specifier [opt] expression ; }
             the type of the returned expression after lvalue-to-rvalue
             conversion (_conv.lval_ 4.1), array-to-pointer conversion
             (_conv.array_ 4.2), and function-to-pointer conversion
             (_conv.func_ 4.3);
          * otherwise, void.  */

    /* In a lambda that has neither a lambda-return-type-clause
       nor a deducible form, errors should be reported for return statements
       in the body.  Since we used void as the placeholder return type, parsing
       the body as usual will give such desired behavior.  */
    if (!LAMBDA_EXPR_RETURN_TYPE (lambda_expr)
        && cp_lexer_next_token_is (parser->lexer, CPP_OPEN_BRACE)
        && cp_lexer_peek_nth_token (parser->lexer, 2)->keyword == RID_RETURN
        && cp_lexer_peek_nth_token (parser->lexer, 3)->type != CPP_SEMICOLON)
      {
	tree compound_stmt;
	tree expr = NULL_TREE;
	cp_id_kind idk = CP_ID_KIND_NONE;

	/* Parse tentatively in case there's more after the initial return
	   statement.  */
	cp_parser_parse_tentatively (parser);

	cp_parser_require (parser, CPP_OPEN_BRACE, "%<{%>");
	cp_parser_require_keyword (parser, RID_RETURN, "%<return%>");

	expr = cp_parser_expression (parser, /*cast_p=*/false, &idk);

	cp_parser_require (parser, CPP_SEMICOLON, "%<;%>");
	cp_parser_require (parser, CPP_CLOSE_BRACE, "%<}%>");

	if (cp_parser_parse_definitely (parser))
	  {
	    apply_lambda_return_type (lambda_expr, lambda_return_type (expr));

	    compound_stmt = begin_compound_stmt (0);
	    /* Will get error here if type not deduced yet.  */
	    finish_return_stmt (expr);
	    finish_compound_stmt (compound_stmt);

	    done = true;
	  }
      }

    if (!done)
      {
	if (!LAMBDA_EXPR_RETURN_TYPE (lambda_expr))
	  LAMBDA_EXPR_DEDUCE_RETURN_TYPE_P (lambda_expr) = true;
	/* TODO: does begin_compound_stmt want BCS_FN_BODY?
	   cp_parser_compound_stmt does not pass it.  */
	cp_parser_function_body (parser);
	LAMBDA_EXPR_DEDUCE_RETURN_TYPE_P (lambda_expr) = false;
      }

    finish_function_body (body);
    finish_lambda_scope ();

    /* Finish the function and generate code for it if necessary.  */
    expand_or_defer_fn (finish_function (/*inline*/2));
  }

  if (nested)
    pop_function_context();
}

/* Statements [gram.stmt.stmt]  */

/* Parse a statement.

   statement:
     labeled-statement
     expression-statement
     compound-statement
     selection-statement
     iteration-statement
     jump-statement
     declaration-statement
     try-block

  IN_COMPOUND is true when the statement is nested inside a
  cp_parser_compound_statement; this matters for certain pragmas.

  If IF_P is not NULL, *IF_P is set to indicate whether the statement
  is a (possibly labeled) if statement which is not enclosed in braces
  and has an else clause.  This is used to implement -Wparentheses.  */

static void
cp_parser_statement (cp_parser* parser, tree in_statement_expr,
		     bool in_compound, bool *if_p)
{
  tree statement;
  cp_token *token;
  location_t statement_location;

 restart:
  if (if_p != NULL)
    *if_p = false;
  /* There is no statement yet.  */
  statement = NULL_TREE;
  /* Peek at the next token.  */
  token = cp_lexer_peek_token (parser->lexer);
  /* Remember the location of the first token in the statement.  */
  statement_location = token->location;
  /* If this is a keyword, then that will often determine what kind of
     statement we have.  */
  if (token->type == CPP_KEYWORD)
    {
      enum rid keyword = token->keyword;

      switch (keyword)
	{
	case RID_CASE:
	case RID_DEFAULT:
	  /* Looks like a labeled-statement with a case label.
	     Parse the label, and then use tail recursion to parse
	     the statement.  */
	  cp_parser_label_for_labeled_statement (parser);
	  goto restart;

	case RID_IF:
	case RID_SWITCH:
	  statement = cp_parser_selection_statement (parser, if_p);
	  break;

	case RID_WHILE:
	case RID_DO:
	case RID_FOR:
	  statement = cp_parser_iteration_statement (parser);
	  break;

	case RID_BREAK:
	case RID_CONTINUE:
	case RID_RETURN:
	case RID_GOTO:
	  statement = cp_parser_jump_statement (parser);
	  break;

	  /* Objective-C++ exception-handling constructs.  */
	case RID_AT_TRY:
	case RID_AT_CATCH:
	case RID_AT_FINALLY:
	case RID_AT_SYNCHRONIZED:
	case RID_AT_THROW:
	  statement = cp_parser_objc_statement (parser);
	  break;

	case RID_TRY:
	  statement = cp_parser_try_block (parser);
	  break;

	case RID_NAMESPACE:
	  /* This must be a namespace alias definition.  */
	  cp_parser_declaration_statement (parser);
	  return;
	  
	default:
	  /* It might be a keyword like `int' that can start a
	     declaration-statement.  */
	  break;
	}
    }
  else if (token->type == CPP_NAME)
    {
      /* If the next token is a `:', then we are looking at a
	 labeled-statement.  */
      token = cp_lexer_peek_nth_token (parser->lexer, 2);
      if (token->type == CPP_COLON)
	{
	  /* Looks like a labeled-statement with an ordinary label.
	     Parse the label, and then use tail recursion to parse
	     the statement.  */
	  cp_parser_label_for_labeled_statement (parser);
	  goto restart;
	}
    }
  /* Anything that starts with a `{' must be a compound-statement.  */
  else if (token->type == CPP_OPEN_BRACE)
    statement = cp_parser_compound_statement (parser, NULL, false);
  /* CPP_PRAGMA is a #pragma inside a function body, which constitutes
     a statement all its own.  */
  else if (token->type == CPP_PRAGMA)
    {
      /* Only certain OpenMP pragmas are attached to statements, and thus
	 are considered statements themselves.  All others are not.  In
	 the context of a compound, accept the pragma as a "statement" and
	 return so that we can check for a close brace.  Otherwise we
	 require a real statement and must go back and read one.  */
      if (in_compound)
	cp_parser_pragma (parser, pragma_compound);
      else if (!cp_parser_pragma (parser, pragma_stmt))
	goto restart;
      return;
    }
  else if (token->type == CPP_EOF)
    {
      cp_parser_error (parser, "expected statement");
      return;
    }

  /* Everything else must be a declaration-statement or an
     expression-statement.  Try for the declaration-statement
     first, unless we are looking at a `;', in which case we know that
     we have an expression-statement.  */
  if (!statement)
    {
      if (cp_lexer_next_token_is_not (parser->lexer, CPP_SEMICOLON))
	{
	  cp_parser_parse_tentatively (parser);
	  /* Try to parse the declaration-statement.  */
	  cp_parser_declaration_statement (parser);
	  /* If that worked, we're done.  */
	  if (cp_parser_parse_definitely (parser))
	    return;
	}
      /* Look for an expression-statement instead.  */
      statement = cp_parser_expression_statement (parser, in_statement_expr);
    }

  /* Set the line number for the statement.  */
  if (statement && STATEMENT_CODE_P (TREE_CODE (statement)))
    SET_EXPR_LOCATION (statement, statement_location);
}

/* Parse the label for a labeled-statement, i.e.

   identifier :
   case constant-expression :
   default :

   GNU Extension:
   case constant-expression ... constant-expression : statement

   When a label is parsed without errors, the label is added to the
   parse tree by the finish_* functions, so this function doesn't
   have to return the label.  */

static void
cp_parser_label_for_labeled_statement (cp_parser* parser)
{
  cp_token *token;
  tree label = NULL_TREE;

  /* The next token should be an identifier.  */
  token = cp_lexer_peek_token (parser->lexer);
  if (token->type != CPP_NAME
      && token->type != CPP_KEYWORD)
    {
      cp_parser_error (parser, "expected labeled-statement");
      return;
    }

  switch (token->keyword)
    {
    case RID_CASE:
      {
	tree expr, expr_hi;
	cp_token *ellipsis;

	/* Consume the `case' token.  */
	cp_lexer_consume_token (parser->lexer);
	/* Parse the constant-expression.  */
	expr = cp_parser_constant_expression (parser,
					      /*allow_non_constant_p=*/false,
					      NULL);

	ellipsis = cp_lexer_peek_token (parser->lexer);
	if (ellipsis->type == CPP_ELLIPSIS)
	  {
	    /* Consume the `...' token.  */
	    cp_lexer_consume_token (parser->lexer);
	    expr_hi =
	      cp_parser_constant_expression (parser,
					     /*allow_non_constant_p=*/false,
					     NULL);
	    /* We don't need to emit warnings here, as the common code
	       will do this for us.  */
	  }
	else
	  expr_hi = NULL_TREE;

	if (parser->in_switch_statement_p)
	  finish_case_label (token->location, expr, expr_hi);
	else
	  error_at (token->location,
		    "case label %qE not within a switch statement",
		    expr);
      }
      break;

    case RID_DEFAULT:
      /* Consume the `default' token.  */
      cp_lexer_consume_token (parser->lexer);

      if (parser->in_switch_statement_p)
	finish_case_label (token->location, NULL_TREE, NULL_TREE);
      else
	error_at (token->location, "case label not within a switch statement");
      break;

    default:
      /* Anything else must be an ordinary label.  */
      label = finish_label_stmt (cp_parser_identifier (parser));
      break;
    }

  /* Require the `:' token.  */
  cp_parser_require (parser, CPP_COLON, "%<:%>");

  /* An ordinary label may optionally be followed by attributes.
     However, this is only permitted if the attributes are then
     followed by a semicolon.  This is because, for backward
     compatibility, when parsing
       lab: __attribute__ ((unused)) int i;
     we want the attribute to attach to "i", not "lab".  */
  if (label != NULL_TREE
      && cp_lexer_next_token_is_keyword (parser->lexer, RID_ATTRIBUTE))
    {
      tree attrs;

      cp_parser_parse_tentatively (parser);
      attrs = cp_parser_attributes_opt (parser);
      if (attrs == NULL_TREE
	  || cp_lexer_next_token_is_not (parser->lexer, CPP_SEMICOLON))
	cp_parser_abort_tentative_parse (parser);
      else if (!cp_parser_parse_definitely (parser))
	;
      else
	cplus_decl_attributes (&label, attrs, 0);
    }
}

/* Parse an expression-statement.

   expression-statement:
     expression [opt] ;

   Returns the new EXPR_STMT -- or NULL_TREE if the expression
   statement consists of nothing more than an `;'. IN_STATEMENT_EXPR_P
   indicates whether this expression-statement is part of an
   expression statement.  */

static tree
cp_parser_expression_statement (cp_parser* parser, tree in_statement_expr)
{
  tree statement = NULL_TREE;
  cp_token *token = cp_lexer_peek_token (parser->lexer);

  /* If the next token is a ';', then there is no expression
     statement.  */
  if (cp_lexer_next_token_is_not (parser->lexer, CPP_SEMICOLON))
    statement = cp_parser_expression (parser, /*cast_p=*/false, NULL);

  /* Give a helpful message for "A<T>::type t;" and the like.  */
  if (cp_lexer_next_token_is_not (parser->lexer, CPP_SEMICOLON)
      && !cp_parser_uncommitted_to_tentative_parse_p (parser))
    {
      if (TREE_CODE (statement) == SCOPE_REF)
	error_at (token->location, "need %<typename%> before %qE because "
		  "%qT is a dependent scope",
		  statement, TREE_OPERAND (statement, 0));
      else if (is_overloaded_fn (statement)
	       && DECL_CONSTRUCTOR_P (get_first_fn (statement)))
	{
	  /* A::A a; */
	  tree fn = get_first_fn (statement);
	  error_at (token->location,
		    "%<%T::%D%> names the constructor, not the type",
		    DECL_CONTEXT (fn), DECL_NAME (fn));
	}
    }

  /* Consume the final `;'.  */
  cp_parser_consume_semicolon_at_end_of_statement (parser);

  if (in_statement_expr
      && cp_lexer_next_token_is (parser->lexer, CPP_CLOSE_BRACE))
    /* This is the final expression statement of a statement
       expression.  */
    statement = finish_stmt_expr_expr (statement, in_statement_expr);
  else if (statement)
    statement = finish_expr_stmt (statement);
  else
    finish_stmt ();

  return statement;
}

/* Parse a compound-statement.

   compound-statement:
     { statement-seq [opt] }

   GNU extension:

   compound-statement:
     { label-declaration-seq [opt] statement-seq [opt] }

   label-declaration-seq:
     label-declaration
     label-declaration-seq label-declaration

   Returns a tree representing the statement.  */

static tree
cp_parser_compound_statement (cp_parser *parser, tree in_statement_expr,
			      bool in_try)
{
  tree compound_stmt;

  /* Consume the `{'.  */
  if (!cp_parser_require (parser, CPP_OPEN_BRACE, "%<{%>"))
    return error_mark_node;
  /* Begin the compound-statement.  */
  compound_stmt = begin_compound_stmt (in_try ? BCS_TRY_BLOCK : 0);
  /* If the next keyword is `__label__' we have a label declaration.  */
  while (cp_lexer_next_token_is_keyword (parser->lexer, RID_LABEL))
    cp_parser_label_declaration (parser);
  /* Parse an (optional) statement-seq.  */
  cp_parser_statement_seq_opt (parser, in_statement_expr);
  /* Finish the compound-statement.  */
  finish_compound_stmt (compound_stmt);
  /* Consume the `}'.  */
  cp_parser_require (parser, CPP_CLOSE_BRACE, "%<}%>");

  return compound_stmt;
}

/* Parse an (optional) statement-seq.

   statement-seq:
     statement
     statement-seq [opt] statement  */

static void
cp_parser_statement_seq_opt (cp_parser* parser, tree in_statement_expr)
{
  /* Scan statements until there aren't any more.  */
  while (true)
    {
      cp_token *token = cp_lexer_peek_token (parser->lexer);

      /* If we're looking at a `}', then we've run out of statements.  */
      if (token->type == CPP_CLOSE_BRACE
	  || token->type == CPP_EOF
	  || token->type == CPP_PRAGMA_EOL)
	break;
      
      /* If we are in a compound statement and find 'else' then
	 something went wrong.  */
      else if (token->type == CPP_KEYWORD && token->keyword == RID_ELSE)
	{
	  if (parser->in_statement & IN_IF_STMT) 
	    break;
	  else
	    {
	      token = cp_lexer_consume_token (parser->lexer);
	      error_at (token->location, "%<else%> without a previous %<if%>");
	    }
	}

      /* Parse the statement.  */
      cp_parser_statement (parser, in_statement_expr, true, NULL);
    }
}

/* Parse a selection-statement.

   selection-statement:
     if ( condition ) statement
     if ( condition ) statement else statement
     switch ( condition ) statement

   Returns the new IF_STMT or SWITCH_STMT.

   If IF_P is not NULL, *IF_P is set to indicate whether the statement
   is a (possibly labeled) if statement which is not enclosed in
   braces and has an else clause.  This is used to implement
   -Wparentheses.  */

static tree
cp_parser_selection_statement (cp_parser* parser, bool *if_p)
{
  cp_token *token;
  enum rid keyword;

  if (if_p != NULL)
    *if_p = false;

  /* Peek at the next token.  */
  token = cp_parser_require (parser, CPP_KEYWORD, "selection-statement");

  /* See what kind of keyword it is.  */
  keyword = token->keyword;
  switch (keyword)
    {
    case RID_IF:
    case RID_SWITCH:
      {
	tree statement;
	tree condition;

	/* Look for the `('.  */
	if (!cp_parser_require (parser, CPP_OPEN_PAREN, "%<(%>"))
	  {
	    cp_parser_skip_to_end_of_statement (parser);
	    return error_mark_node;
	  }

	/* Begin the selection-statement.  */
	if (keyword == RID_IF)
	  statement = begin_if_stmt ();
	else
	  statement = begin_switch_stmt ();

	/* Parse the condition.  */
	condition = cp_parser_condition (parser);
	/* Look for the `)'.  */
	if (!cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>"))
	  cp_parser_skip_to_closing_parenthesis (parser, true, false,
						 /*consume_paren=*/true);

	if (keyword == RID_IF)
	  {
	    bool nested_if;
	    unsigned char in_statement;

	    /* Add the condition.  */
	    finish_if_stmt_cond (condition, statement);

	    /* Parse the then-clause.  */
	    in_statement = parser->in_statement;
	    parser->in_statement |= IN_IF_STMT;
	    if (cp_lexer_next_token_is (parser->lexer, CPP_SEMICOLON))
	      {
	        location_t loc = cp_lexer_peek_token (parser->lexer)->location;
		add_stmt (build_empty_stmt (loc));
		cp_lexer_consume_token (parser->lexer);
	        if (!cp_lexer_next_token_is_keyword (parser->lexer, RID_ELSE))
		  warning_at (loc, OPT_Wempty_body, "suggest braces around "
			      "empty body in an %<if%> statement");
		nested_if = false;
	      }
	    else
	      cp_parser_implicitly_scoped_statement (parser, &nested_if);
	    parser->in_statement = in_statement;

	    finish_then_clause (statement);

	    /* If the next token is `else', parse the else-clause.  */
	    if (cp_lexer_next_token_is_keyword (parser->lexer,
						RID_ELSE))
	      {
		/* Consume the `else' keyword.  */
		cp_lexer_consume_token (parser->lexer);
		begin_else_clause (statement);
		/* Parse the else-clause.  */
	        if (cp_lexer_next_token_is (parser->lexer, CPP_SEMICOLON))
	          {
		    location_t loc;
		    loc = cp_lexer_peek_token (parser->lexer)->location;
		    warning_at (loc,
				OPT_Wempty_body, "suggest braces around "
			        "empty body in an %<else%> statement");
		    add_stmt (build_empty_stmt (loc));
		    cp_lexer_consume_token (parser->lexer);
		  }
		else
		  cp_parser_implicitly_scoped_statement (parser, NULL);

		finish_else_clause (statement);

		/* If we are currently parsing a then-clause, then
		   IF_P will not be NULL.  We set it to true to
		   indicate that this if statement has an else clause.
		   This may trigger the Wparentheses warning below
		   when we get back up to the parent if statement.  */
		if (if_p != NULL)
		  *if_p = true;
	      }
	    else
	      {
		/* This if statement does not have an else clause.  If
		   NESTED_IF is true, then the then-clause is an if
		   statement which does have an else clause.  We warn
		   about the potential ambiguity.  */
		if (nested_if)
		  warning_at (EXPR_LOCATION (statement), OPT_Wparentheses,
			      "suggest explicit braces to avoid ambiguous"
			      " %<else%>");
	      }

	    /* Now we're all done with the if-statement.  */
	    finish_if_stmt (statement);
	  }
	else
	  {
	    bool in_switch_statement_p;
	    unsigned char in_statement;

	    /* Add the condition.  */
	    finish_switch_cond (condition, statement);

	    /* Parse the body of the switch-statement.  */
	    in_switch_statement_p = parser->in_switch_statement_p;
	    in_statement = parser->in_statement;
	    parser->in_switch_statement_p = true;
	    parser->in_statement |= IN_SWITCH_STMT;
	    cp_parser_implicitly_scoped_statement (parser, NULL);
	    parser->in_switch_statement_p = in_switch_statement_p;
	    parser->in_statement = in_statement;

	    /* Now we're all done with the switch-statement.  */
	    finish_switch_stmt (statement);
	  }

	return statement;
      }
      break;

    default:
      cp_parser_error (parser, "expected selection-statement");
      return error_mark_node;
    }
}

/* Parse a condition.

   condition:
     expression
     type-specifier-seq declarator = initializer-clause
     type-specifier-seq declarator braced-init-list

   GNU Extension:

   condition:
     type-specifier-seq declarator asm-specification [opt]
       attributes [opt] = assignment-expression

   Returns the expression that should be tested.  */

static tree
cp_parser_condition (cp_parser* parser)
{
  cp_decl_specifier_seq type_specifiers;
  const char *saved_message;

  /* Try the declaration first.  */
  cp_parser_parse_tentatively (parser);
  /* New types are not allowed in the type-specifier-seq for a
     condition.  */
  saved_message = parser->type_definition_forbidden_message;
  parser->type_definition_forbidden_message
    = G_("types may not be defined in conditions");
  /* Parse the type-specifier-seq.  */
  cp_parser_type_specifier_seq (parser, /*is_declaration==*/true,
				/*is_trailing_return=*/false,
				&type_specifiers);
  /* Restore the saved message.  */
  parser->type_definition_forbidden_message = saved_message;
  /* If all is well, we might be looking at a declaration.  */
  if (!cp_parser_error_occurred (parser))
    {
      tree decl;
      tree asm_specification;
      tree attributes;
      cp_declarator *declarator;
      tree initializer = NULL_TREE;

      /* Parse the declarator.  */
      declarator = cp_parser_declarator (parser, CP_PARSER_DECLARATOR_NAMED,
					 /*ctor_dtor_or_conv_p=*/NULL,
					 /*parenthesized_p=*/NULL,
					 /*member_p=*/false);
      /* Parse the attributes.  */
      attributes = cp_parser_attributes_opt (parser);
      /* Parse the asm-specification.  */
      asm_specification = cp_parser_asm_specification_opt (parser);
      /* If the next token is not an `=' or '{', then we might still be
	 looking at an expression.  For example:

	   if (A(a).x)

	 looks like a decl-specifier-seq and a declarator -- but then
	 there is no `=', so this is an expression.  */
      if (cp_lexer_next_token_is_not (parser->lexer, CPP_EQ)
	  && cp_lexer_next_token_is_not (parser->lexer, CPP_OPEN_BRACE))
	cp_parser_simulate_error (parser);
	
      /* If we did see an `=' or '{', then we are looking at a declaration
	 for sure.  */
      if (cp_parser_parse_definitely (parser))
	{
	  tree pushed_scope;
	  bool non_constant_p;
	  bool flags = LOOKUP_ONLYCONVERTING;

	  /* Create the declaration.  */
	  decl = start_decl (declarator, &type_specifiers,
			     /*initialized_p=*/true,
			     attributes, /*prefix_attributes=*/NULL_TREE,
			     &pushed_scope);

	  /* Parse the initializer.  */
	  if (cp_lexer_next_token_is (parser->lexer, CPP_OPEN_BRACE))
	    {
	      initializer = cp_parser_braced_list (parser, &non_constant_p);
	      CONSTRUCTOR_IS_DIRECT_INIT (initializer) = 1;
	      flags = 0;
	    }
	  else
	    {
	      /* Consume the `='.  */
	      cp_parser_require (parser, CPP_EQ, "%<=%>");
	      initializer = cp_parser_initializer_clause (parser, &non_constant_p);
	    }
	  if (BRACE_ENCLOSED_INITIALIZER_P (initializer))
	    maybe_warn_cpp0x (CPP0X_INITIALIZER_LISTS);

	  if (!non_constant_p)
	    initializer = fold_non_dependent_expr (initializer);

	  /* Process the initializer.  */
	  cp_finish_decl (decl,
			  initializer, !non_constant_p,
			  asm_specification,
			  flags);

	  if (pushed_scope)
	    pop_scope (pushed_scope);

	  return convert_from_reference (decl);
	}
    }
  /* If we didn't even get past the declarator successfully, we are
     definitely not looking at a declaration.  */
  else
    cp_parser_abort_tentative_parse (parser);

  /* Otherwise, we are looking at an expression.  */
  return cp_parser_expression (parser, /*cast_p=*/false, NULL);
}

/* Parse an iteration-statement.

   iteration-statement:
     while ( condition ) statement
     do statement while ( expression ) ;
     for ( for-init-statement condition [opt] ; expression [opt] )
       statement

   Returns the new WHILE_STMT, DO_STMT, or FOR_STMT.  */

static tree
cp_parser_iteration_statement (cp_parser* parser)
{
  cp_token *token;
  enum rid keyword;
  tree statement;
  unsigned char in_statement;

  /* Peek at the next token.  */
  token = cp_parser_require (parser, CPP_KEYWORD, "iteration-statement");
  if (!token)
    return error_mark_node;

  /* Remember whether or not we are already within an iteration
     statement.  */
  in_statement = parser->in_statement;

  /* See what kind of keyword it is.  */
  keyword = token->keyword;
  switch (keyword)
    {
    case RID_WHILE:
      {
	tree condition;

	/* Begin the while-statement.  */
	statement = begin_while_stmt ();
	/* Look for the `('.  */
	cp_parser_require (parser, CPP_OPEN_PAREN, "%<(%>");
	/* Parse the condition.  */
	condition = cp_parser_condition (parser);
	finish_while_stmt_cond (condition, statement);
	/* Look for the `)'.  */
	cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>");
	/* Parse the dependent statement.  */
	parser->in_statement = IN_ITERATION_STMT;
	cp_parser_already_scoped_statement (parser);
	parser->in_statement = in_statement;
	/* We're done with the while-statement.  */
	finish_while_stmt (statement);
      }
      break;

    case RID_DO:
      {
	tree expression;

	/* Begin the do-statement.  */
	statement = begin_do_stmt ();
	/* Parse the body of the do-statement.  */
	parser->in_statement = IN_ITERATION_STMT;
	cp_parser_implicitly_scoped_statement (parser, NULL);
	parser->in_statement = in_statement;
	finish_do_body (statement);
	/* Look for the `while' keyword.  */
	cp_parser_require_keyword (parser, RID_WHILE, "%<while%>");
	/* Look for the `('.  */
	cp_parser_require (parser, CPP_OPEN_PAREN, "%<(%>");
	/* Parse the expression.  */
	expression = cp_parser_expression (parser, /*cast_p=*/false, NULL);
	/* We're done with the do-statement.  */
	finish_do_stmt (expression, statement);
	/* Look for the `)'.  */
	cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>");
	/* Look for the `;'.  */
	cp_parser_require (parser, CPP_SEMICOLON, "%<;%>");
      }
      break;

    case RID_FOR:
      {
	tree condition = NULL_TREE;
	tree expression = NULL_TREE;

	/* Begin the for-statement.  */
	statement = begin_for_stmt ();
	/* Look for the `('.  */
	cp_parser_require (parser, CPP_OPEN_PAREN, "%<(%>");
	/* Parse the initialization.  */
	cp_parser_for_init_statement (parser);
	finish_for_init_stmt (statement);

	/* If there's a condition, process it.  */
	if (cp_lexer_next_token_is_not (parser->lexer, CPP_SEMICOLON))
	  condition = cp_parser_condition (parser);
	finish_for_cond (condition, statement);
	/* Look for the `;'.  */
	cp_parser_require (parser, CPP_SEMICOLON, "%<;%>");

	/* If there's an expression, process it.  */
	if (cp_lexer_next_token_is_not (parser->lexer, CPP_CLOSE_PAREN))
	  expression = cp_parser_expression (parser, /*cast_p=*/false, NULL);
	finish_for_expr (expression, statement);
	/* Look for the `)'.  */
	cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>");

	/* Parse the body of the for-statement.  */
	parser->in_statement = IN_ITERATION_STMT;
	cp_parser_already_scoped_statement (parser);
	parser->in_statement = in_statement;

	/* We're done with the for-statement.  */
	finish_for_stmt (statement);
      }
      break;

    default:
      cp_parser_error (parser, "expected iteration-statement");
      statement = error_mark_node;
      break;
    }

  return statement;
}

/* Parse a for-init-statement.

   for-init-statement:
     expression-statement
     simple-declaration  */

static void
cp_parser_for_init_statement (cp_parser* parser)
{
  /* If the next token is a `;', then we have an empty
     expression-statement.  Grammatically, this is also a
     simple-declaration, but an invalid one, because it does not
     declare anything.  Therefore, if we did not handle this case
     specially, we would issue an error message about an invalid
     declaration.  */
  if (cp_lexer_next_token_is_not (parser->lexer, CPP_SEMICOLON))
    {
      /* We're going to speculatively look for a declaration, falling back
	 to an expression, if necessary.  */
      cp_parser_parse_tentatively (parser);
      /* Parse the declaration.  */
      cp_parser_simple_declaration (parser,
				    /*function_definition_allowed_p=*/false);
      /* If the tentative parse failed, then we shall need to look for an
	 expression-statement.  */
      if (cp_parser_parse_definitely (parser))
	return;
    }

  cp_parser_expression_statement (parser, NULL_TREE);
}

/* Parse a jump-statement.

   jump-statement:
     break ;
     continue ;
     return expression [opt] ;
     return braced-init-list ;
     goto identifier ;

   GNU extension:

   jump-statement:
     goto * expression ;

   Returns the new BREAK_STMT, CONTINUE_STMT, RETURN_EXPR, or GOTO_EXPR.  */

static tree
cp_parser_jump_statement (cp_parser* parser)
{
  tree statement = error_mark_node;
  cp_token *token;
  enum rid keyword;
  unsigned char in_statement;

  /* Peek at the next token.  */
  token = cp_parser_require (parser, CPP_KEYWORD, "jump-statement");
  if (!token)
    return error_mark_node;

  /* See what kind of keyword it is.  */
  keyword = token->keyword;
  switch (keyword)
    {
    case RID_BREAK:
      in_statement = parser->in_statement & ~IN_IF_STMT;      
      switch (in_statement)
	{
	case 0:
	  error_at (token->location, "break statement not within loop or switch");
	  break;
	default:
	  gcc_assert ((in_statement & IN_SWITCH_STMT)
		      || in_statement == IN_ITERATION_STMT);
	  statement = finish_break_stmt ();
	  break;
	case IN_OMP_BLOCK:
	  error_at (token->location, "invalid exit from OpenMP structured block");
	  break;
	case IN_OMP_FOR:
	  error_at (token->location, "break statement used with OpenMP for loop");
	  break;
	}
      cp_parser_require (parser, CPP_SEMICOLON, "%<;%>");
      break;

    case RID_CONTINUE:
      switch (parser->in_statement & ~(IN_SWITCH_STMT | IN_IF_STMT))
	{
	case 0:
	  error_at (token->location, "continue statement not within a loop");
	  break;
	case IN_ITERATION_STMT:
	case IN_OMP_FOR:
	  statement = finish_continue_stmt ();
	  break;
	case IN_OMP_BLOCK:
	  error_at (token->location, "invalid exit from OpenMP structured block");
	  break;
	default:
	  gcc_unreachable ();
	}
      cp_parser_require (parser, CPP_SEMICOLON, "%<;%>");
      break;

    case RID_RETURN:
      {
	tree expr;
	bool expr_non_constant_p;

	if (cp_lexer_next_token_is (parser->lexer, CPP_OPEN_BRACE))
	  {
	    maybe_warn_cpp0x (CPP0X_INITIALIZER_LISTS);
	    expr = cp_parser_braced_list (parser, &expr_non_constant_p);
	  }
	else if (cp_lexer_next_token_is_not (parser->lexer, CPP_SEMICOLON))
	  expr = cp_parser_expression (parser, /*cast_p=*/false, NULL);
	else
	  /* If the next token is a `;', then there is no
	     expression.  */
	  expr = NULL_TREE;
	/* Build the return-statement.  */
	statement = finish_return_stmt (expr);
	/* Look for the final `;'.  */
	cp_parser_require (parser, CPP_SEMICOLON, "%<;%>");
      }
      break;

    case RID_GOTO:
      /* Create the goto-statement.  */
      if (cp_lexer_next_token_is (parser->lexer, CPP_MULT))
	{
	  /* Issue a warning about this use of a GNU extension.  */
	  pedwarn (token->location, OPT_pedantic, "ISO C++ forbids computed gotos");
	  /* Consume the '*' token.  */
	  cp_lexer_consume_token (parser->lexer);
	  /* Parse the dependent expression.  */
	  finish_goto_stmt (cp_parser_expression (parser, /*cast_p=*/false, NULL));
	}
      else
	finish_goto_stmt (cp_parser_identifier (parser));
      /* Look for the final `;'.  */
      cp_parser_require (parser, CPP_SEMICOLON, "%<;%>");
      break;

    default:
      cp_parser_error (parser, "expected jump-statement");
      break;
    }

  return statement;
}

/* Parse a declaration-statement.

   declaration-statement:
     block-declaration  */

static void
cp_parser_declaration_statement (cp_parser* parser)
{
  void *p;

  /* Get the high-water mark for the DECLARATOR_OBSTACK.  */
  p = obstack_alloc (&declarator_obstack, 0);

 /* Parse the block-declaration.  */
  cp_parser_block_declaration (parser, /*statement_p=*/true);

  /* Free any declarators allocated.  */
  obstack_free (&declarator_obstack, p);

  /* Finish off the statement.  */
  finish_stmt ();
}

/* Some dependent statements (like `if (cond) statement'), are
   implicitly in their own scope.  In other words, if the statement is
   a single statement (as opposed to a compound-statement), it is
   none-the-less treated as if it were enclosed in braces.  Any
   declarations appearing in the dependent statement are out of scope
   after control passes that point.  This function parses a statement,
   but ensures that is in its own scope, even if it is not a
   compound-statement.

   If IF_P is not NULL, *IF_P is set to indicate whether the statement
   is a (possibly labeled) if statement which is not enclosed in
   braces and has an else clause.  This is used to implement
   -Wparentheses.

   Returns the new statement.  */

static tree
cp_parser_implicitly_scoped_statement (cp_parser* parser, bool *if_p)
{
  tree statement;

  if (if_p != NULL)
    *if_p = false;

  /* Mark if () ; with a special NOP_EXPR.  */
  if (cp_lexer_next_token_is (parser->lexer, CPP_SEMICOLON))
    {
      location_t loc = cp_lexer_peek_token (parser->lexer)->location;
      cp_lexer_consume_token (parser->lexer);
      statement = add_stmt (build_empty_stmt (loc));
    }
  /* if a compound is opened, we simply parse the statement directly.  */
  else if (cp_lexer_next_token_is (parser->lexer, CPP_OPEN_BRACE))
    statement = cp_parser_compound_statement (parser, NULL, false);
  /* If the token is not a `{', then we must take special action.  */
  else
    {
      /* Create a compound-statement.  */
      statement = begin_compound_stmt (0);
      /* Parse the dependent-statement.  */
      cp_parser_statement (parser, NULL_TREE, false, if_p);
      /* Finish the dummy compound-statement.  */
      finish_compound_stmt (statement);
    }

  /* Return the statement.  */
  return statement;
}

/* For some dependent statements (like `while (cond) statement'), we
   have already created a scope.  Therefore, even if the dependent
   statement is a compound-statement, we do not want to create another
   scope.  */

static void
cp_parser_already_scoped_statement (cp_parser* parser)
{
  /* If the token is a `{', then we must take special action.  */
  if (cp_lexer_next_token_is_not (parser->lexer, CPP_OPEN_BRACE))
    cp_parser_statement (parser, NULL_TREE, false, NULL);
  else
    {
      /* Avoid calling cp_parser_compound_statement, so that we
	 don't create a new scope.  Do everything else by hand.  */
      cp_parser_require (parser, CPP_OPEN_BRACE, "%<{%>");
      /* If the next keyword is `__label__' we have a label declaration.  */
      while (cp_lexer_next_token_is_keyword (parser->lexer, RID_LABEL))
	cp_parser_label_declaration (parser);
      /* Parse an (optional) statement-seq.  */
      cp_parser_statement_seq_opt (parser, NULL_TREE);
      cp_parser_require (parser, CPP_CLOSE_BRACE, "%<}%>");
    }
}

/* Declarations [gram.dcl.dcl] */

/* Parse an optional declaration-sequence.

   declaration-seq:
     declaration
     declaration-seq declaration  */

static void
cp_parser_declaration_seq_opt (cp_parser* parser)
{
  while (true)
    {
      cp_token *token;

      token = cp_lexer_peek_token (parser->lexer);

      if (token->type == CPP_CLOSE_BRACE
	  || token->type == CPP_EOF
	  || token->type == CPP_PRAGMA_EOL)
	break;

      if (token->type == CPP_SEMICOLON)
	{
	  /* A declaration consisting of a single semicolon is
	     invalid.  Allow it unless we're being pedantic.  */
	  cp_lexer_consume_token (parser->lexer);
	  if (!in_system_header)
	    pedwarn (input_location, OPT_pedantic, "extra %<;%>");
	  continue;
	}

      /* If we're entering or exiting a region that's implicitly
	 extern "C", modify the lang context appropriately.  */
      if (!parser->implicit_extern_c && token->implicit_extern_c)
	{
	  push_lang_context (lang_name_c);
	  parser->implicit_extern_c = true;
	}
      else if (parser->implicit_extern_c && !token->implicit_extern_c)
	{
	  pop_lang_context ();
	  parser->implicit_extern_c = false;
	}

      if (token->type == CPP_PRAGMA)
	{
	  /* A top-level declaration can consist solely of a #pragma.
	     A nested declaration cannot, so this is done here and not
	     in cp_parser_declaration.  (A #pragma at block scope is
	     handled in cp_parser_statement.)  */
	  cp_parser_pragma (parser, pragma_external);
	  continue;
	}

      /* Parse the declaration itself.  */
      cp_parser_declaration (parser);
    }
}

/* Parse a declaration.

   declaration:
     block-declaration
     function-definition
     template-declaration
     explicit-instantiation
     explicit-specialization
     linkage-specification
     namespace-definition

   GNU extension:

   declaration:
      __extension__ declaration */

static void
cp_parser_declaration (cp_parser* parser)
{
  cp_token token1;
  cp_token token2;
  int saved_pedantic;
  void *p;

  /* Check for the `__extension__' keyword.  */
  if (cp_parser_extension_opt (parser, &saved_pedantic))
    {
      /* Parse the qualified declaration.  */
      cp_parser_declaration (parser);
      /* Restore the PEDANTIC flag.  */
      pedantic = saved_pedantic;

      return;
    }

  /* Try to figure out what kind of declaration is present.  */
  token1 = *cp_lexer_peek_token (parser->lexer);

  if (token1.type != CPP_EOF)
    token2 = *cp_lexer_peek_nth_token (parser->lexer, 2);
  else
    {
      token2.type = CPP_EOF;
      token2.keyword = RID_MAX;
    }

  /* Get the high-water mark for the DECLARATOR_OBSTACK.  */
  p = obstack_alloc (&declarator_obstack, 0);

  /* If the next token is `extern' and the following token is a string
     literal, then we have a linkage specification.  */
  if (token1.keyword == RID_EXTERN
      && cp_parser_is_string_literal (&token2))
    cp_parser_linkage_specification (parser);
  /* If the next token is `template', then we have either a template
     declaration, an explicit instantiation, or an explicit
     specialization.  */
  else if (token1.keyword == RID_TEMPLATE)
    {
      /* `template <>' indicates a template specialization.  */
      if (token2.type == CPP_LESS
	  && cp_lexer_peek_nth_token (parser->lexer, 3)->type == CPP_GREATER)
	cp_parser_explicit_specialization (parser);
      /* `template <' indicates a template declaration.  */
      else if (token2.type == CPP_LESS)
	cp_parser_template_declaration (parser, /*member_p=*/false);
      /* Anything else must be an explicit instantiation.  */
      else
	cp_parser_explicit_instantiation (parser);
    }
  /* If the next token is `export', then we have a template
     declaration.  */
  else if (token1.keyword == RID_EXPORT)
    cp_parser_template_declaration (parser, /*member_p=*/false);
  /* If the next token is `extern', 'static' or 'inline' and the one
     after that is `template', we have a GNU extended explicit
     instantiation directive.  */
  else if (cp_parser_allow_gnu_extensions_p (parser)
	   && (token1.keyword == RID_EXTERN
	       || token1.keyword == RID_STATIC
	       || token1.keyword == RID_INLINE)
	   && token2.keyword == RID_TEMPLATE)
    cp_parser_explicit_instantiation (parser);
  /* If the next token is `namespace', check for a named or unnamed
     namespace definition.  */
  else if (token1.keyword == RID_NAMESPACE
	   && (/* A named namespace definition.  */
	       (token2.type == CPP_NAME
		&& (cp_lexer_peek_nth_token (parser->lexer, 3)->type
		    != CPP_EQ))
	       /* An unnamed namespace definition.  */
	       || token2.type == CPP_OPEN_BRACE
	       || token2.keyword == RID_ATTRIBUTE))
    cp_parser_namespace_definition (parser);
  /* An inline (associated) namespace definition.  */
  else if (token1.keyword == RID_INLINE
	   && token2.keyword == RID_NAMESPACE)
    cp_parser_namespace_definition (parser);
  /* Objective-C++ declaration/definition.  */
  else if (c_dialect_objc () && OBJC_IS_AT_KEYWORD (token1.keyword))
    cp_parser_objc_declaration (parser);
  /* We must have either a block declaration or a function
     definition.  */
  else
    /* Try to parse a block-declaration, or a function-definition.  */
    cp_parser_block_declaration (parser, /*statement_p=*/false);

  /* Free any declarators allocated.  */
  obstack_free (&declarator_obstack, p);
}

/* Parse a block-declaration.

   block-declaration:
     simple-declaration
     asm-definition
     namespace-alias-definition
     using-declaration
     using-directive

   GNU Extension:

   block-declaration:
     __extension__ block-declaration

   C++0x Extension:

   block-declaration:
     static_assert-declaration

   If STATEMENT_P is TRUE, then this block-declaration is occurring as
   part of a declaration-statement.  */

static void
cp_parser_block_declaration (cp_parser *parser,
			     bool      statement_p)
{
  cp_token *token1;
  int saved_pedantic;

  /* Check for the `__extension__' keyword.  */
  if (cp_parser_extension_opt (parser, &saved_pedantic))
    {
      /* Parse the qualified declaration.  */
      cp_parser_block_declaration (parser, statement_p);
      /* Restore the PEDANTIC flag.  */
      pedantic = saved_pedantic;

      return;
    }

  /* Peek at the next token to figure out which kind of declaration is
     present.  */
  token1 = cp_lexer_peek_token (parser->lexer);

  /* If the next keyword is `asm', we have an asm-definition.  */
  if (token1->keyword == RID_ASM)
    {
      if (statement_p)
	cp_parser_commit_to_tentative_parse (parser);
      cp_parser_asm_definition (parser);
    }
  /* If the next keyword is `namespace', we have a
     namespace-alias-definition.  */
  else if (token1->keyword == RID_NAMESPACE)
    cp_parser_namespace_alias_definition (parser);
  /* If the next keyword is `using', we have either a
     using-declaration or a using-directive.  */
  else if (token1->keyword == RID_USING)
    {
      cp_token *token2;

      if (statement_p)
	cp_parser_commit_to_tentative_parse (parser);
      /* If the token after `using' is `namespace', then we have a
	 using-directive.  */
      token2 = cp_lexer_peek_nth_token (parser->lexer, 2);
      if (token2->keyword == RID_NAMESPACE)
	cp_parser_using_directive (parser);
      /* Otherwise, it's a using-declaration.  */
      else
	cp_parser_using_declaration (parser,
				     /*access_declaration_p=*/false);
    }
  /* If the next keyword is `__label__' we have a misplaced label
     declaration.  */
  else if (token1->keyword == RID_LABEL)
    {
      cp_lexer_consume_token (parser->lexer);
      error_at (token1->location, "%<__label__%> not at the beginning of a block");
      cp_parser_skip_to_end_of_statement (parser);
      /* If the next token is now a `;', consume it.  */
      if (cp_lexer_next_token_is (parser->lexer, CPP_SEMICOLON))
	cp_lexer_consume_token (parser->lexer);
    }
  /* If the next token is `static_assert' we have a static assertion.  */
  else if (token1->keyword == RID_STATIC_ASSERT)
    cp_parser_static_assert (parser, /*member_p=*/false);
  /* Anything else must be a simple-declaration.  */
  else
    cp_parser_simple_declaration (parser, !statement_p);
}

/* Parse a simple-declaration.

   simple-declaration:
     decl-specifier-seq [opt] init-declarator-list [opt] ;

   init-declarator-list:
     init-declarator
     init-declarator-list , init-declarator

   If FUNCTION_DEFINITION_ALLOWED_P is TRUE, then we also recognize a
   function-definition as a simple-declaration.  */

static void
cp_parser_simple_declaration (cp_parser* parser,
			      bool function_definition_allowed_p)
{
  cp_decl_specifier_seq decl_specifiers;
  int declares_class_or_enum;
  bool saw_declarator;

  /* Defer access checks until we know what is being declared; the
     checks for names appearing in the decl-specifier-seq should be
     done as if we were in the scope of the thing being declared.  */
  push_deferring_access_checks (dk_deferred);

  /* Parse the decl-specifier-seq.  We have to keep track of whether
     or not the decl-specifier-seq declares a named class or
     enumeration type, since that is the only case in which the
     init-declarator-list is allowed to be empty.

     [dcl.dcl]

     In a simple-declaration, the optional init-declarator-list can be
     omitted only when declaring a class or enumeration, that is when
     the decl-specifier-seq contains either a class-specifier, an
     elaborated-type-specifier, or an enum-specifier.  */
  cp_parser_decl_specifier_seq (parser,
				CP_PARSER_FLAGS_OPTIONAL,
				&decl_specifiers,
				&declares_class_or_enum);
  /* We no longer need to defer access checks.  */
  stop_deferring_access_checks ();

  /* In a block scope, a valid declaration must always have a
     decl-specifier-seq.  By not trying to parse declarators, we can
     resolve the declaration/expression ambiguity more quickly.  */
  if (!function_definition_allowed_p
      && !decl_specifiers.any_specifiers_p)
    {
      cp_parser_error (parser, "expected declaration");
      goto done;
    }

  /* If the next two tokens are both identifiers, the code is
     erroneous. The usual cause of this situation is code like:

       T t;

     where "T" should name a type -- but does not.  */
  if (!decl_specifiers.any_type_specifiers_p
      && cp_parser_parse_and_diagnose_invalid_type_name (parser))
    {
      /* If parsing tentatively, we should commit; we really are
	 looking at a declaration.  */
      cp_parser_commit_to_tentative_parse (parser);
      /* Give up.  */
      goto done;
    }

  /* If we have seen at least one decl-specifier, and the next token
     is not a parenthesis, then we must be looking at a declaration.
     (After "int (" we might be looking at a functional cast.)  */
  if (decl_specifiers.any_specifiers_p
      && cp_lexer_next_token_is_not (parser->lexer, CPP_OPEN_PAREN)
      && cp_lexer_next_token_is_not (parser->lexer, CPP_OPEN_BRACE)
      && !cp_parser_error_occurred (parser))
    cp_parser_commit_to_tentative_parse (parser);

  /* Keep going until we hit the `;' at the end of the simple
     declaration.  */
  saw_declarator = false;
  while (cp_lexer_next_token_is_not (parser->lexer,
				     CPP_SEMICOLON))
    {
      cp_token *token;
      bool function_definition_p;
      tree decl;

      if (saw_declarator)
	{
	  /* If we are processing next declarator, coma is expected */
	  token = cp_lexer_peek_token (parser->lexer);
	  gcc_assert (token->type == CPP_COMMA);
	  cp_lexer_consume_token (parser->lexer);
	}
      else
	saw_declarator = true;

      /* Parse the init-declarator.  */
      decl = cp_parser_init_declarator (parser, &decl_specifiers,
					/*checks=*/NULL,
					function_definition_allowed_p,
					/*member_p=*/false,
					declares_class_or_enum,
					&function_definition_p);
      /* If an error occurred while parsing tentatively, exit quickly.
	 (That usually happens when in the body of a function; each
	 statement is treated as a declaration-statement until proven
	 otherwise.)  */
      if (cp_parser_error_occurred (parser))
	goto done;
      /* Handle function definitions specially.  */
      if (function_definition_p)
	{
	  /* If the next token is a `,', then we are probably
	     processing something like:

	       void f() {}, *p;

	     which is erroneous.  */
	  if (cp_lexer_next_token_is (parser->lexer, CPP_COMMA))
	    {
	      cp_token *token = cp_lexer_peek_token (parser->lexer);
	      error_at (token->location,
			"mixing"
			" declarations and function-definitions is forbidden");
	    }
	  /* Otherwise, we're done with the list of declarators.  */
	  else
	    {
	      pop_deferring_access_checks ();
	      return;
	    }
	}
      /* The next token should be either a `,' or a `;'.  */
      token = cp_lexer_peek_token (parser->lexer);
      /* If it's a `,', there are more declarators to come.  */
      if (token->type == CPP_COMMA)
	/* will be consumed next time around */;
      /* If it's a `;', we are done.  */
      else if (token->type == CPP_SEMICOLON)
	break;
      /* Anything else is an error.  */
      else
	{
	  /* If we have already issued an error message we don't need
	     to issue another one.  */
	  if (decl != error_mark_node
	      || cp_parser_uncommitted_to_tentative_parse_p (parser))
	    cp_parser_error (parser, "expected %<,%> or %<;%>");
	  /* Skip tokens until we reach the end of the statement.  */
	  cp_parser_skip_to_end_of_statement (parser);
	  /* If the next token is now a `;', consume it.  */
	  if (cp_lexer_next_token_is (parser->lexer, CPP_SEMICOLON))
	    cp_lexer_consume_token (parser->lexer);
	  goto done;
	}
      /* After the first time around, a function-definition is not
	 allowed -- even if it was OK at first.  For example:

	   int i, f() {}

	 is not valid.  */
      function_definition_allowed_p = false;
    }

  /* Issue an error message if no declarators are present, and the
     decl-specifier-seq does not itself declare a class or
     enumeration.  */
  if (!saw_declarator)
    {
      if (cp_parser_declares_only_class_p (parser))
	shadow_tag (&decl_specifiers);
      /* Perform any deferred access checks.  */
      perform_deferred_access_checks ();
    }

  /* Consume the `;'.  */
  cp_parser_require (parser, CPP_SEMICOLON, "%<;%>");

 done:
  pop_deferring_access_checks ();
}

/* Parse a decl-specifier-seq.

   decl-specifier-seq:
     decl-specifier-seq [opt] decl-specifier

   decl-specifier:
     storage-class-specifier
     type-specifier
     function-specifier
     friend
     typedef

   GNU Extension:

   decl-specifier:
     attributes

   Set *DECL_SPECS to a representation of the decl-specifier-seq.

   The parser flags FLAGS is used to control type-specifier parsing.

   *DECLARES_CLASS_OR_ENUM is set to the bitwise or of the following
   flags:

     1: one of the decl-specifiers is an elaborated-type-specifier
	(i.e., a type declaration)
     2: one of the decl-specifiers is an enum-specifier or a
	class-specifier (i.e., a type definition)

   */

static void
cp_parser_decl_specifier_seq (cp_parser* parser,
			      cp_parser_flags flags,
			      cp_decl_specifier_seq *decl_specs,
			      int* declares_class_or_enum)
{
  bool constructor_possible_p = !parser->in_declarator_p;
  cp_token *start_token = NULL;

  /* Clear DECL_SPECS.  */
  clear_decl_specs (decl_specs);

  /* Assume no class or enumeration type is declared.  */
  *declares_class_or_enum = 0;

  /* Keep reading specifiers until there are no more to read.  */
  while (true)
    {
      bool constructor_p;
      bool found_decl_spec;
      cp_token *token;

      /* Peek at the next token.  */
      token = cp_lexer_peek_token (parser->lexer);

      /* Save the first token of the decl spec list for error
         reporting.  */
      if (!start_token)
	start_token = token;
      /* Handle attributes.  */
      if (token->keyword == RID_ATTRIBUTE)
	{
	  /* Parse the attributes.  */
	  decl_specs->attributes
	    = chainon (decl_specs->attributes,
		       cp_parser_attributes_opt (parser));
	  continue;
	}
      /* Assume we will find a decl-specifier keyword.  */
      found_decl_spec = true;
      /* If the next token is an appropriate keyword, we can simply
	 add it to the list.  */
      switch (token->keyword)
	{
	  /* decl-specifier:
	       friend
               constexpr */
	case RID_FRIEND:
	  if (!at_class_scope_p ())
	    {
	      error_at (token->location, "%<friend%> used outside of class");
	      cp_lexer_purge_token (parser->lexer);
	    }
	  else
	    {
	      ++decl_specs->specs[(int) ds_friend];
	      /* Consume the token.  */
	      cp_lexer_consume_token (parser->lexer);
	    }
	  break;

        case RID_CONSTEXPR:
          ++decl_specs->specs[(int) ds_constexpr];
          cp_lexer_consume_token (parser->lexer);
          break;

	  /* function-specifier:
	       inline
	       virtual
	       explicit  */
	case RID_INLINE:
	case RID_VIRTUAL:
	case RID_EXPLICIT:
	  cp_parser_function_specifier_opt (parser, decl_specs);
	  break;

	  /* decl-specifier:
	       typedef  */
	case RID_TYPEDEF:
	  ++decl_specs->specs[(int) ds_typedef];
	  /* Consume the token.  */
	  cp_lexer_consume_token (parser->lexer);
	  /* A constructor declarator cannot appear in a typedef.  */
	  constructor_possible_p = false;
	  /* The "typedef" keyword can only occur in a declaration; we
	     may as well commit at this point.  */
	  cp_parser_commit_to_tentative_parse (parser);

          if (decl_specs->storage_class != sc_none)
            decl_specs->conflicting_specifiers_p = true;
	  break;

	  /* storage-class-specifier:
	       auto
	       register
	       static
	       extern
	       mutable

	     GNU Extension:
	       thread  */
	case RID_AUTO:
          if (cxx_dialect == cxx98) 
            {
	      /* Consume the token.  */
	      cp_lexer_consume_token (parser->lexer);

              /* Complain about `auto' as a storage specifier, if
                 we're complaining about C++0x compatibility.  */
              warning_at (token->location, OPT_Wc__0x_compat, "%<auto%>"
			  " will change meaning in C++0x; please remove it");

              /* Set the storage class anyway.  */
              cp_parser_set_storage_class (parser, decl_specs, RID_AUTO,
					   token->location);
            }
          else
	    /* C++0x auto type-specifier.  */
	    found_decl_spec = false;
          break;

	case RID_REGISTER:
	case RID_STATIC:
	case RID_EXTERN:
	case RID_MUTABLE:
	  /* Consume the token.  */
	  cp_lexer_consume_token (parser->lexer);
          cp_parser_set_storage_class (parser, decl_specs, token->keyword,
				       token->location);
	  break;
	case RID_THREAD:
	  /* Consume the token.  */
	  cp_lexer_consume_token (parser->lexer);
	  ++decl_specs->specs[(int) ds_thread];
	  break;

	default:
	  /* We did not yet find a decl-specifier yet.  */
	  found_decl_spec = false;
	  break;
	}

      /* Constructors are a special case.  The `S' in `S()' is not a
	 decl-specifier; it is the beginning of the declarator.  */
      constructor_p
	= (!found_decl_spec
	   && constructor_possible_p
	   && (cp_parser_constructor_declarator_p
	       (parser, decl_specs->specs[(int) ds_friend] != 0)));

      /* If we don't have a DECL_SPEC yet, then we must be looking at
	 a type-specifier.  */
      if (!found_decl_spec && !constructor_p)
	{
	  int decl_spec_declares_class_or_enum;
	  bool is_cv_qualifier;
	  tree type_spec;

	  type_spec
	    = cp_parser_type_specifier (parser, flags,
					decl_specs,
					/*is_declaration=*/true,
					&decl_spec_declares_class_or_enum,
					&is_cv_qualifier);
	  *declares_class_or_enum |= decl_spec_declares_class_or_enum;

	  /* If this type-specifier referenced a user-defined type
	     (a typedef, class-name, etc.), then we can't allow any
	     more such type-specifiers henceforth.

	     [dcl.spec]

	     The longest sequence of decl-specifiers that could
	     possibly be a type name is taken as the
	     decl-specifier-seq of a declaration.  The sequence shall
	     be self-consistent as described below.

	     [dcl.type]

	     As a general rule, at most one type-specifier is allowed
	     in the complete decl-specifier-seq of a declaration.  The
	     only exceptions are the following:

	     -- const or volatile can be combined with any other
		type-specifier.

	     -- signed or unsigned can be combined with char, long,
		short, or int.

	     -- ..

	     Example:

	       typedef char* Pc;
	       void g (const int Pc);

	     Here, Pc is *not* part of the decl-specifier seq; it's
	     the declarator.  Therefore, once we see a type-specifier
	     (other than a cv-qualifier), we forbid any additional
	     user-defined types.  We *do* still allow things like `int
	     int' to be considered a decl-specifier-seq, and issue the
	     error message later.  */
	  if (type_spec && !is_cv_qualifier)
	    flags |= CP_PARSER_FLAGS_NO_USER_DEFINED_TYPES;
	  /* A constructor declarator cannot follow a type-specifier.  */
	  if (type_spec)
	    {
	      constructor_possible_p = false;
	      found_decl_spec = true;
	      if (!is_cv_qualifier)
		decl_specs->any_type_specifiers_p = true;
	    }
	}

      /* If we still do not have a DECL_SPEC, then there are no more
	 decl-specifiers.  */
      if (!found_decl_spec)
	break;

      decl_specs->any_specifiers_p = true;
      /* After we see one decl-specifier, further decl-specifiers are
	 always optional.  */
      flags |= CP_PARSER_FLAGS_OPTIONAL;
    }

  cp_parser_check_decl_spec (decl_specs, start_token->location);

  /* Don't allow a friend specifier with a class definition.  */
  if (decl_specs->specs[(int) ds_friend] != 0
      && (*declares_class_or_enum & 2))
    error_at (start_token->location,
	      "class definition may not be declared a friend");
}

/* Parse an (optional) storage-class-specifier.

   storage-class-specifier:
     auto
     register
     static
     extern
     mutable

   GNU Extension:

   storage-class-specifier:
     thread

   Returns an IDENTIFIER_NODE corresponding to the keyword used.  */

static tree
cp_parser_storage_class_specifier_opt (cp_parser* parser)
{
  switch (cp_lexer_peek_token (parser->lexer)->keyword)
    {
    case RID_AUTO:
      if (cxx_dialect != cxx98)
        return NULL_TREE;
      /* Fall through for C++98.  */

    case RID_REGISTER:
    case RID_STATIC:
    case RID_EXTERN:
    case RID_MUTABLE:
    case RID_THREAD:
      /* Consume the token.  */
      return cp_lexer_consume_token (parser->lexer)->u.value;

    default:
      return NULL_TREE;
    }
}

/* Parse an (optional) function-specifier.

   function-specifier:
     inline
     virtual
     explicit

   Returns an IDENTIFIER_NODE corresponding to the keyword used.
   Updates DECL_SPECS, if it is non-NULL.  */

static tree
cp_parser_function_specifier_opt (cp_parser* parser,
				  cp_decl_specifier_seq *decl_specs)
{
  cp_token *token = cp_lexer_peek_token (parser->lexer);
  switch (token->keyword)
    {
    case RID_INLINE:
      if (decl_specs)
	++decl_specs->specs[(int) ds_inline];
      break;

    case RID_VIRTUAL:
      /* 14.5.2.3 [temp.mem]

	 A member function template shall not be virtual.  */
      if (PROCESSING_REAL_TEMPLATE_DECL_P ())
	error_at (token->location, "templates may not be %<virtual%>");
      else if (decl_specs)
	++decl_specs->specs[(int) ds_virtual];
      break;

    case RID_EXPLICIT:
      if (decl_specs)
	++decl_specs->specs[(int) ds_explicit];
      break;

    default:
      return NULL_TREE;
    }

  /* Consume the token.  */
  return cp_lexer_consume_token (parser->lexer)->u.value;
}

/* Parse a linkage-specification.

   linkage-specification:
     extern string-literal { declaration-seq [opt] }
     extern string-literal declaration  */

static void
cp_parser_linkage_specification (cp_parser* parser)
{
  tree linkage;

  /* Look for the `extern' keyword.  */
  cp_parser_require_keyword (parser, RID_EXTERN, "%<extern%>");

  /* Look for the string-literal.  */
  linkage = cp_parser_string_literal (parser, false, false);

  /* Transform the literal into an identifier.  If the literal is a
     wide-character string, or contains embedded NULs, then we can't
     handle it as the user wants.  */
  if (strlen (TREE_STRING_POINTER (linkage))
      != (size_t) (TREE_STRING_LENGTH (linkage) - 1))
    {
      cp_parser_error (parser, "invalid linkage-specification");
      /* Assume C++ linkage.  */
      linkage = lang_name_cplusplus;
    }
  else
    linkage = get_identifier (TREE_STRING_POINTER (linkage));

  /* We're now using the new linkage.  */
  push_lang_context (linkage);

  /* If the next token is a `{', then we're using the first
     production.  */
  if (cp_lexer_next_token_is (parser->lexer, CPP_OPEN_BRACE))
    {
      /* Consume the `{' token.  */
      cp_lexer_consume_token (parser->lexer);
      /* Parse the declarations.  */
      cp_parser_declaration_seq_opt (parser);
      /* Look for the closing `}'.  */
      cp_parser_require (parser, CPP_CLOSE_BRACE, "%<}%>");
    }
  /* Otherwise, there's just one declaration.  */
  else
    {
      bool saved_in_unbraced_linkage_specification_p;

      saved_in_unbraced_linkage_specification_p
	= parser->in_unbraced_linkage_specification_p;
      parser->in_unbraced_linkage_specification_p = true;
      cp_parser_declaration (parser);
      parser->in_unbraced_linkage_specification_p
	= saved_in_unbraced_linkage_specification_p;
    }

  /* We're done with the linkage-specification.  */
  pop_lang_context ();
}

/* Parse a static_assert-declaration.

   static_assert-declaration:
     static_assert ( constant-expression , string-literal ) ; 

   If MEMBER_P, this static_assert is a class member.  */

static void 
cp_parser_static_assert(cp_parser *parser, bool member_p)
{
  tree condition;
  tree message;
  cp_token *token;
  location_t saved_loc;

  /* Peek at the `static_assert' token so we can keep track of exactly
     where the static assertion started.  */
  token = cp_lexer_peek_token (parser->lexer);
  saved_loc = token->location;

  /* Look for the `static_assert' keyword.  */
  if (!cp_parser_require_keyword (parser, RID_STATIC_ASSERT, 
                                  "%<static_assert%>"))
    return;

  /*  We know we are in a static assertion; commit to any tentative
      parse.  */
  if (cp_parser_parsing_tentatively (parser))
    cp_parser_commit_to_tentative_parse (parser);

  /* Parse the `(' starting the static assertion condition.  */
  cp_parser_require (parser, CPP_OPEN_PAREN, "%<(%>");

  /* Parse the constant-expression.  */
  condition = 
    cp_parser_constant_expression (parser,
                                   /*allow_non_constant_p=*/false,
                                   /*non_constant_p=*/NULL);

  /* Parse the separating `,'.  */
  cp_parser_require (parser, CPP_COMMA, "%<,%>");

  /* Parse the string-literal message.  */
  message = cp_parser_string_literal (parser, 
                                      /*translate=*/false,
                                      /*wide_ok=*/true);

  /* A `)' completes the static assertion.  */
  if (!cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>"))
    cp_parser_skip_to_closing_parenthesis (parser, 
                                           /*recovering=*/true, 
                                           /*or_comma=*/false,
					   /*consume_paren=*/true);

  /* A semicolon terminates the declaration.  */
  cp_parser_require (parser, CPP_SEMICOLON, "%<;%>");

  /* Complete the static assertion, which may mean either processing 
     the static assert now or saving it for template instantiation.  */
  finish_static_assert (condition, message, saved_loc, member_p);
}

/* Parse a `decltype' type. Returns the type. 

   simple-type-specifier:
     decltype ( expression )  */

static tree
cp_parser_decltype (cp_parser *parser)
{
  tree expr;
  bool id_expression_or_member_access_p = false;
  const char *saved_message;
  bool saved_integral_constant_expression_p;
  bool saved_non_integral_constant_expression_p;
  cp_token *id_expr_start_token;

  /* Look for the `decltype' token.  */
  if (!cp_parser_require_keyword (parser, RID_DECLTYPE, "%<decltype%>"))
    return error_mark_node;

  /* Types cannot be defined in a `decltype' expression.  Save away the
     old message.  */
  saved_message = parser->type_definition_forbidden_message;

  /* And create the new one.  */
  parser->type_definition_forbidden_message
    = G_("types may not be defined in %<decltype%> expressions");

  /* The restrictions on constant-expressions do not apply inside
     decltype expressions.  */
  saved_integral_constant_expression_p
    = parser->integral_constant_expression_p;
  saved_non_integral_constant_expression_p
    = parser->non_integral_constant_expression_p;
  parser->integral_constant_expression_p = false;

  /* Do not actually evaluate the expression.  */
  ++cp_unevaluated_operand;

  /* Do not warn about problems with the expression.  */
  ++c_inhibit_evaluation_warnings;

  /* Parse the opening `('.  */
  if (!cp_parser_require (parser, CPP_OPEN_PAREN, "%<(%>"))
    return error_mark_node;
  
  /* First, try parsing an id-expression.  */
  id_expr_start_token = cp_lexer_peek_token (parser->lexer);
  cp_parser_parse_tentatively (parser);
  expr = cp_parser_id_expression (parser,
                                  /*template_keyword_p=*/false,
                                  /*check_dependency_p=*/true,
                                  /*template_p=*/NULL,
                                  /*declarator_p=*/false,
                                  /*optional_p=*/false);

  if (!cp_parser_error_occurred (parser) && expr != error_mark_node)
    {
      bool non_integral_constant_expression_p = false;
      tree id_expression = expr;
      cp_id_kind idk;
      const char *error_msg;

      if (TREE_CODE (expr) == IDENTIFIER_NODE)
	/* Lookup the name we got back from the id-expression.  */
	expr = cp_parser_lookup_name (parser, expr,
				      none_type,
				      /*is_template=*/false,
				      /*is_namespace=*/false,
				      /*check_dependency=*/true,
				      /*ambiguous_decls=*/NULL,
				      id_expr_start_token->location);

      if (expr
          && expr != error_mark_node
          && TREE_CODE (expr) != TEMPLATE_ID_EXPR
          && TREE_CODE (expr) != TYPE_DECL
	  && (TREE_CODE (expr) != BIT_NOT_EXPR
	      || !TYPE_P (TREE_OPERAND (expr, 0)))
          && cp_lexer_peek_token (parser->lexer)->type == CPP_CLOSE_PAREN)
        {
          /* Complete lookup of the id-expression.  */
          expr = (finish_id_expression
                  (id_expression, expr, parser->scope, &idk,
                   /*integral_constant_expression_p=*/false,
                   /*allow_non_integral_constant_expression_p=*/true,
                   &non_integral_constant_expression_p,
                   /*template_p=*/false,
                   /*done=*/true,
                   /*address_p=*/false,
                   /*template_arg_p=*/false,
                   &error_msg,
		   id_expr_start_token->location));

          if (expr == error_mark_node)
            /* We found an id-expression, but it was something that we
               should not have found. This is an error, not something
               we can recover from, so note that we found an
               id-expression and we'll recover as gracefully as
               possible.  */
            id_expression_or_member_access_p = true;
        }

      if (expr 
          && expr != error_mark_node
          && cp_lexer_peek_token (parser->lexer)->type == CPP_CLOSE_PAREN)
        /* We have an id-expression.  */
        id_expression_or_member_access_p = true;
    }

  if (!id_expression_or_member_access_p)
    {
      /* Abort the id-expression parse.  */
      cp_parser_abort_tentative_parse (parser);

      /* Parsing tentatively, again.  */
      cp_parser_parse_tentatively (parser);

      /* Parse a class member access.  */
      expr = cp_parser_postfix_expression (parser, /*address_p=*/false,
                                           /*cast_p=*/false,
                                           /*member_access_only_p=*/true, NULL);

      if (expr 
          && expr != error_mark_node
          && cp_lexer_peek_token (parser->lexer)->type == CPP_CLOSE_PAREN)
        /* We have an id-expression.  */
        id_expression_or_member_access_p = true;
    }

  if (id_expression_or_member_access_p)
    /* We have parsed the complete id-expression or member access.  */
    cp_parser_parse_definitely (parser);
  else
    {
      bool saved_greater_than_is_operator_p;

      /* Abort our attempt to parse an id-expression or member access
         expression.  */
      cp_parser_abort_tentative_parse (parser);

      /* Within a parenthesized expression, a `>' token is always
	 the greater-than operator.  */
      saved_greater_than_is_operator_p
	= parser->greater_than_is_operator_p;
      parser->greater_than_is_operator_p = true;

      /* Parse a full expression.  */
      expr = cp_parser_expression (parser, /*cast_p=*/false, NULL);

      /* The `>' token might be the end of a template-id or
	 template-parameter-list now.  */
      parser->greater_than_is_operator_p
	= saved_greater_than_is_operator_p;
    }

  /* Go back to evaluating expressions.  */
  --cp_unevaluated_operand;
  --c_inhibit_evaluation_warnings;

  /* Restore the old message and the integral constant expression
     flags.  */
  parser->type_definition_forbidden_message = saved_message;
  parser->integral_constant_expression_p
    = saved_integral_constant_expression_p;
  parser->non_integral_constant_expression_p
    = saved_non_integral_constant_expression_p;

  if (expr == error_mark_node)
    {
      /* Skip everything up to the closing `)'.  */
      cp_parser_skip_to_closing_parenthesis (parser, true, false,
                                             /*consume_paren=*/true);
      return error_mark_node;
    }
  
  /* Parse to the closing `)'.  */
  if (!cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>"))
    {
      cp_parser_skip_to_closing_parenthesis (parser, true, false,
					     /*consume_paren=*/true);
      return error_mark_node;
    }

  return finish_decltype_type (expr, id_expression_or_member_access_p);
}

/* Special member functions [gram.special] */

/* Parse a conversion-function-id.

   conversion-function-id:
     operator conversion-type-id

   Returns an IDENTIFIER_NODE representing the operator.  */

static tree
cp_parser_conversion_function_id (cp_parser* parser)
{
  tree type;
  tree saved_scope;
  tree saved_qualifying_scope;
  tree saved_object_scope;
  tree pushed_scope = NULL_TREE;

  /* Look for the `operator' token.  */
  if (!cp_parser_require_keyword (parser, RID_OPERATOR, "%<operator%>"))
    return error_mark_node;
  /* When we parse the conversion-type-id, the current scope will be
     reset.  However, we need that information in able to look up the
     conversion function later, so we save it here.  */
  saved_scope = parser->scope;
  saved_qualifying_scope = parser->qualifying_scope;
  saved_object_scope = parser->object_scope;
  /* We must enter the scope of the class so that the names of
     entities declared within the class are available in the
     conversion-type-id.  For example, consider:

       struct S {
	 typedef int I;
	 operator I();
       };

       S::operator I() { ... }

     In order to see that `I' is a type-name in the definition, we
     must be in the scope of `S'.  */
  if (saved_scope)
    pushed_scope = push_scope (saved_scope);
  /* Parse the conversion-type-id.  */
  type = cp_parser_conversion_type_id (parser);
  /* Leave the scope of the class, if any.  */
  if (pushed_scope)
    pop_scope (pushed_scope);
  /* Restore the saved scope.  */
  parser->scope = saved_scope;
  parser->qualifying_scope = saved_qualifying_scope;
  parser->object_scope = saved_object_scope;
  /* If the TYPE is invalid, indicate failure.  */
  if (type == error_mark_node)
    return error_mark_node;
  return mangle_conv_op_name_for_type (type);
}

/* Parse a conversion-type-id:

   conversion-type-id:
     type-specifier-seq conversion-declarator [opt]

   Returns the TYPE specified.  */

static tree
cp_parser_conversion_type_id (cp_parser* parser)
{
  tree attributes;
  cp_decl_specifier_seq type_specifiers;
  cp_declarator *declarator;
  tree type_specified;

  /* Parse the attributes.  */
  attributes = cp_parser_attributes_opt (parser);
  /* Parse the type-specifiers.  */
  cp_parser_type_specifier_seq (parser, /*is_declaration=*/false,
				/*is_trailing_return=*/false,
				&type_specifiers);
  /* If that didn't work, stop.  */
  if (type_specifiers.type == error_mark_node)
    return error_mark_node;
  /* Parse the conversion-declarator.  */
  declarator = cp_parser_conversion_declarator_opt (parser);

  type_specified =  grokdeclarator (declarator, &type_specifiers, TYPENAME,
				    /*initialized=*/0, &attributes);
  if (attributes)
    cplus_decl_attributes (&type_specified, attributes, /*flags=*/0);

  /* Don't give this error when parsing tentatively.  This happens to
     work because we always parse this definitively once.  */
  if (! cp_parser_uncommitted_to_tentative_parse_p (parser)
      && type_uses_auto (type_specified))
    {
      error ("invalid use of %<auto%> in conversion operator");
      return error_mark_node;
    }

  return type_specified;
}

/* Parse an (optional) conversion-declarator.

   conversion-declarator:
     ptr-operator conversion-declarator [opt]

   */

static cp_declarator *
cp_parser_conversion_declarator_opt (cp_parser* parser)
{
  enum tree_code code;
  tree class_type;
  cp_cv_quals cv_quals;

  /* We don't know if there's a ptr-operator next, or not.  */
  cp_parser_parse_tentatively (parser);
  /* Try the ptr-operator.  */
  code = cp_parser_ptr_operator (parser, &class_type, &cv_quals);
  /* If it worked, look for more conversion-declarators.  */
  if (cp_parser_parse_definitely (parser))
    {
      cp_declarator *declarator;

      /* Parse another optional declarator.  */
      declarator = cp_parser_conversion_declarator_opt (parser);

      return cp_parser_make_indirect_declarator
	(code, class_type, cv_quals, declarator);
   }

  return NULL;
}

/* Parse an (optional) ctor-initializer.

   ctor-initializer:
     : mem-initializer-list

   Returns TRUE iff the ctor-initializer was actually present.  */

static bool
cp_parser_ctor_initializer_opt (cp_parser* parser)
{
  /* If the next token is not a `:', then there is no
     ctor-initializer.  */
  if (cp_lexer_next_token_is_not (parser->lexer, CPP_COLON))
    {
      /* Do default initialization of any bases and members.  */
      if (DECL_CONSTRUCTOR_P (current_function_decl))
	finish_mem_initializers (NULL_TREE);

      return false;
    }

  /* Consume the `:' token.  */
  cp_lexer_consume_token (parser->lexer);
  /* And the mem-initializer-list.  */
  cp_parser_mem_initializer_list (parser);

  return true;
}

/* Parse a mem-initializer-list.

   mem-initializer-list:
     mem-initializer ... [opt]
     mem-initializer ... [opt] , mem-initializer-list  */

static void
cp_parser_mem_initializer_list (cp_parser* parser)
{
  tree mem_initializer_list = NULL_TREE;
  cp_token *token = cp_lexer_peek_token (parser->lexer);

  /* Let the semantic analysis code know that we are starting the
     mem-initializer-list.  */
  if (!DECL_CONSTRUCTOR_P (current_function_decl))
    error_at (token->location,
	      "only constructors take base initializers");

  /* Loop through the list.  */
  while (true)
    {
      tree mem_initializer;

      token = cp_lexer_peek_token (parser->lexer);
      /* Parse the mem-initializer.  */
      mem_initializer = cp_parser_mem_initializer (parser);
      /* If the next token is a `...', we're expanding member initializers. */
      if (cp_lexer_next_token_is (parser->lexer, CPP_ELLIPSIS))
        {
          /* Consume the `...'. */
          cp_lexer_consume_token (parser->lexer);

          /* The TREE_PURPOSE must be a _TYPE, because base-specifiers
             can be expanded but members cannot. */
          if (mem_initializer != error_mark_node
              && !TYPE_P (TREE_PURPOSE (mem_initializer)))
            {
              error_at (token->location,
			"cannot expand initializer for member %<%D%>",
			TREE_PURPOSE (mem_initializer));
              mem_initializer = error_mark_node;
            }

          /* Construct the pack expansion type. */
          if (mem_initializer != error_mark_node)
            mem_initializer = make_pack_expansion (mem_initializer);
        }
      /* Add it to the list, unless it was erroneous.  */
      if (mem_initializer != error_mark_node)
	{
	  TREE_CHAIN (mem_initializer) = mem_initializer_list;
	  mem_initializer_list = mem_initializer;
	}
      /* If the next token is not a `,', we're done.  */
      if (cp_lexer_next_token_is_not (parser->lexer, CPP_COMMA))
	break;
      /* Consume the `,' token.  */
      cp_lexer_consume_token (parser->lexer);
    }

  /* Perform semantic analysis.  */
  if (DECL_CONSTRUCTOR_P (current_function_decl))
    finish_mem_initializers (mem_initializer_list);
}

/* Parse a mem-initializer.

   mem-initializer:
     mem-initializer-id ( expression-list [opt] )
     mem-initializer-id braced-init-list

   GNU extension:

   mem-initializer:
     ( expression-list [opt] )

   Returns a TREE_LIST.  The TREE_PURPOSE is the TYPE (for a base
   class) or FIELD_DECL (for a non-static data member) to initialize;
   the TREE_VALUE is the expression-list.  An empty initialization
   list is represented by void_list_node.  */

static tree
cp_parser_mem_initializer (cp_parser* parser)
{
  tree mem_initializer_id;
  tree expression_list;
  tree member;
  cp_token *token = cp_lexer_peek_token (parser->lexer);

  /* Find out what is being initialized.  */
  if (cp_lexer_next_token_is (parser->lexer, CPP_OPEN_PAREN))
    {
      permerror (token->location,
		 "anachronistic old-style base class initializer");
      mem_initializer_id = NULL_TREE;
    }
  else
    {
      mem_initializer_id = cp_parser_mem_initializer_id (parser);
      if (mem_initializer_id == error_mark_node)
	return mem_initializer_id;
    }
  member = expand_member_init (mem_initializer_id);
  if (member && !DECL_P (member))
    in_base_initializer = 1;

  if (cp_lexer_next_token_is (parser->lexer, CPP_OPEN_BRACE))
    {
      bool expr_non_constant_p;
      maybe_warn_cpp0x (CPP0X_INITIALIZER_LISTS);
      expression_list = cp_parser_braced_list (parser, &expr_non_constant_p);
      CONSTRUCTOR_IS_DIRECT_INIT (expression_list) = 1;
      expression_list = build_tree_list (NULL_TREE, expression_list);
    }
  else
    {
      VEC(tree,gc)* vec;
      vec = cp_parser_parenthesized_expression_list (parser, false,
						     /*cast_p=*/false,
						     /*allow_expansion_p=*/true,
						     /*non_constant_p=*/NULL);
      if (vec == NULL)
	return error_mark_node;
      expression_list = build_tree_list_vec (vec);
      release_tree_vector (vec);
    }

  if (expression_list == error_mark_node)
    return error_mark_node;
  if (!expression_list)
    expression_list = void_type_node;

  in_base_initializer = 0;

  return member ? build_tree_list (member, expression_list) : error_mark_node;
}

/* Parse a mem-initializer-id.

   mem-initializer-id:
     :: [opt] nested-name-specifier [opt] class-name
     identifier

   Returns a TYPE indicating the class to be initializer for the first
   production.  Returns an IDENTIFIER_NODE indicating the data member
   to be initialized for the second production.  */

static tree
cp_parser_mem_initializer_id (cp_parser* parser)
{
  bool global_scope_p;
  bool nested_name_specifier_p;
  bool template_p = false;
  tree id;

  cp_token *token = cp_lexer_peek_token (parser->lexer);

  /* `typename' is not allowed in this context ([temp.res]).  */
  if (cp_lexer_next_token_is_keyword (parser->lexer, RID_TYPENAME))
    {
      error_at (token->location, 
		"keyword %<typename%> not allowed in this context (a qualified "
		"member initializer is implicitly a type)");
      cp_lexer_consume_token (parser->lexer);
    }
  /* Look for the optional `::' operator.  */
  global_scope_p
    = (cp_parser_global_scope_opt (parser,
				   /*current_scope_valid_p=*/false)
       != NULL_TREE);
  /* Look for the optional nested-name-specifier.  The simplest way to
     implement:

       [temp.res]

       The keyword `typename' is not permitted in a base-specifier or
       mem-initializer; in these contexts a qualified name that
       depends on a template-parameter is implicitly assumed to be a
       type name.

     is to assume that we have seen the `typename' keyword at this
     point.  */
  nested_name_specifier_p
    = (cp_parser_nested_name_specifier_opt (parser,
					    /*typename_keyword_p=*/true,
					    /*check_dependency_p=*/true,
					    /*type_p=*/true,
					    /*is_declaration=*/true)
       != NULL_TREE);
  if (nested_name_specifier_p)
    template_p = cp_parser_optional_template_keyword (parser);
  /* If there is a `::' operator or a nested-name-specifier, then we
     are definitely looking for a class-name.  */
  if (global_scope_p || nested_name_specifier_p)
    return cp_parser_class_name (parser,
				 /*typename_keyword_p=*/true,
				 /*template_keyword_p=*/template_p,
				 typename_type,
				 /*check_dependency_p=*/true,
				 /*class_head_p=*/false,
				 /*is_declaration=*/true);
  /* Otherwise, we could also be looking for an ordinary identifier.  */
  cp_parser_parse_tentatively (parser);
  /* Try a class-name.  */
  id = cp_parser_class_name (parser,
			     /*typename_keyword_p=*/true,
			     /*template_keyword_p=*/false,
			     none_type,
			     /*check_dependency_p=*/true,
			     /*class_head_p=*/false,
			     /*is_declaration=*/true);
  /* If we found one, we're done.  */
  if (cp_parser_parse_definitely (parser))
    return id;
  /* Otherwise, look for an ordinary identifier.  */
  return cp_parser_identifier (parser);
}

/* Overloading [gram.over] */

/* Parse an operator-function-id.

   operator-function-id:
     operator operator

   Returns an IDENTIFIER_NODE for the operator which is a
   human-readable spelling of the identifier, e.g., `operator +'.  */

static tree
cp_parser_operator_function_id (cp_parser* parser)
{
  /* Look for the `operator' keyword.  */
  if (!cp_parser_require_keyword (parser, RID_OPERATOR, "%<operator%>"))
    return error_mark_node;
  /* And then the name of the operator itself.  */
  return cp_parser_operator (parser);
}

/* Parse an operator.

   operator:
     new delete new[] delete[] + - * / % ^ & | ~ ! = < >
     += -= *= /= %= ^= &= |= << >> >>= <<= == != <= >= &&
     || ++ -- , ->* -> () []

   GNU Extensions:

   operator:
     <? >? <?= >?=

   Returns an IDENTIFIER_NODE for the operator which is a
   human-readable spelling of the identifier, e.g., `operator +'.  */

static tree
cp_parser_operator (cp_parser* parser)
{
  tree id = NULL_TREE;
  cp_token *token;

  /* Peek at the next token.  */
  token = cp_lexer_peek_token (parser->lexer);
  /* Figure out which operator we have.  */
  switch (token->type)
    {
    case CPP_KEYWORD:
      {
	enum tree_code op;

	/* The keyword should be either `new' or `delete'.  */
	if (token->keyword == RID_NEW)
	  op = NEW_EXPR;
	else if (token->keyword == RID_DELETE)
	  op = DELETE_EXPR;
	else
	  break;

	/* Consume the `new' or `delete' token.  */
	cp_lexer_consume_token (parser->lexer);

	/* Peek at the next token.  */
	token = cp_lexer_peek_token (parser->lexer);
	/* If it's a `[' token then this is the array variant of the
	   operator.  */
	if (token->type == CPP_OPEN_SQUARE)
	  {
	    /* Consume the `[' token.  */
	    cp_lexer_consume_token (parser->lexer);
	    /* Look for the `]' token.  */
	    cp_parser_require (parser, CPP_CLOSE_SQUARE, "%<]%>");
	    id = ansi_opname (op == NEW_EXPR
			      ? VEC_NEW_EXPR : VEC_DELETE_EXPR);
	  }
	/* Otherwise, we have the non-array variant.  */
	else
	  id = ansi_opname (op);

	return id;
      }

    case CPP_PLUS:
      id = ansi_opname (PLUS_EXPR);
      break;

    case CPP_MINUS:
      id = ansi_opname (MINUS_EXPR);
      break;

    case CPP_MULT:
      id = ansi_opname (MULT_EXPR);
      break;

    case CPP_DIV:
      id = ansi_opname (TRUNC_DIV_EXPR);
      break;

    case CPP_MOD:
      id = ansi_opname (TRUNC_MOD_EXPR);
      break;

    case CPP_XOR:
      id = ansi_opname (BIT_XOR_EXPR);
      break;

    case CPP_AND:
      id = ansi_opname (BIT_AND_EXPR);
      break;

    case CPP_OR:
      id = ansi_opname (BIT_IOR_EXPR);
      break;

    case CPP_COMPL:
      id = ansi_opname (BIT_NOT_EXPR);
      break;

    case CPP_NOT:
      id = ansi_opname (TRUTH_NOT_EXPR);
      break;

    case CPP_EQ:
      id = ansi_assopname (NOP_EXPR);
      break;

    case CPP_LESS:
      id = ansi_opname (LT_EXPR);
      break;

    case CPP_GREATER:
      id = ansi_opname (GT_EXPR);
      break;

    case CPP_PLUS_EQ:
      id = ansi_assopname (PLUS_EXPR);
      break;

    case CPP_MINUS_EQ:
      id = ansi_assopname (MINUS_EXPR);
      break;

    case CPP_MULT_EQ:
      id = ansi_assopname (MULT_EXPR);
      break;

    case CPP_DIV_EQ:
      id = ansi_assopname (TRUNC_DIV_EXPR);
      break;

    case CPP_MOD_EQ:
      id = ansi_assopname (TRUNC_MOD_EXPR);
      break;

    case CPP_XOR_EQ:
      id = ansi_assopname (BIT_XOR_EXPR);
      break;

    case CPP_AND_EQ:
      id = ansi_assopname (BIT_AND_EXPR);
      break;

    case CPP_OR_EQ:
      id = ansi_assopname (BIT_IOR_EXPR);
      break;

    case CPP_LSHIFT:
      id = ansi_opname (LSHIFT_EXPR);
      break;

    case CPP_RSHIFT:
      id = ansi_opname (RSHIFT_EXPR);
      break;

    case CPP_LSHIFT_EQ:
      id = ansi_assopname (LSHIFT_EXPR);
      break;

    case CPP_RSHIFT_EQ:
      id = ansi_assopname (RSHIFT_EXPR);
      break;

    case CPP_EQ_EQ:
      id = ansi_opname (EQ_EXPR);
      break;

    case CPP_NOT_EQ:
      id = ansi_opname (NE_EXPR);
      break;

    case CPP_LESS_EQ:
      id = ansi_opname (LE_EXPR);
      break;

    case CPP_GREATER_EQ:
      id = ansi_opname (GE_EXPR);
      break;

    case CPP_AND_AND:
      id = ansi_opname (TRUTH_ANDIF_EXPR);
      break;

    case CPP_OR_OR:
      id = ansi_opname (TRUTH_ORIF_EXPR);
      break;

    case CPP_PLUS_PLUS:
      id = ansi_opname (POSTINCREMENT_EXPR);
      break;

    case CPP_MINUS_MINUS:
      id = ansi_opname (PREDECREMENT_EXPR);
      break;

    case CPP_COMMA:
      id = ansi_opname (COMPOUND_EXPR);
      break;

    case CPP_DEREF_STAR:
      id = ansi_opname (MEMBER_REF);
      break;

    case CPP_DEREF:
      id = ansi_opname (COMPONENT_REF);
      break;

    case CPP_OPEN_PAREN:
      /* Consume the `('.  */
      cp_lexer_consume_token (parser->lexer);
      /* Look for the matching `)'.  */
      cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>");
      return ansi_opname (CALL_EXPR);

    case CPP_OPEN_SQUARE:
      /* Consume the `['.  */
      cp_lexer_consume_token (parser->lexer);
      /* Look for the matching `]'.  */
      cp_parser_require (parser, CPP_CLOSE_SQUARE, "%<]%>");
      return ansi_opname (ARRAY_REF);

    default:
      /* Anything else is an error.  */
      break;
    }

  /* If we have selected an identifier, we need to consume the
     operator token.  */
  if (id)
    cp_lexer_consume_token (parser->lexer);
  /* Otherwise, no valid operator name was present.  */
  else
    {
      cp_parser_error (parser, "expected operator");
      id = error_mark_node;
    }

  return id;
}

/* Parse a template-declaration.

   template-declaration:
     export [opt] template < template-parameter-list > declaration

   If MEMBER_P is TRUE, this template-declaration occurs within a
   class-specifier.

   The grammar rule given by the standard isn't correct.  What
   is really meant is:

   template-declaration:
     export [opt] template-parameter-list-seq
       decl-specifier-seq [opt] init-declarator [opt] ;
     export [opt] template-parameter-list-seq
       function-definition

   template-parameter-list-seq:
     template-parameter-list-seq [opt]
     template < template-parameter-list >  */

static void
cp_parser_template_declaration (cp_parser* parser, bool member_p)
{
  /* Check for `export'.  */
  if (cp_lexer_next_token_is_keyword (parser->lexer, RID_EXPORT))
    {
      /* Consume the `export' token.  */
      cp_lexer_consume_token (parser->lexer);
      /* Warn that we do not support `export'.  */
      warning (0, "keyword %<export%> not implemented, and will be ignored");
    }

  cp_parser_template_declaration_after_export (parser, member_p);
}

/* Parse a template-parameter-list.

   template-parameter-list:
     template-parameter
     template-parameter-list , template-parameter

   Returns a TREE_LIST.  Each node represents a template parameter.
   The nodes are connected via their TREE_CHAINs.  */

static tree
cp_parser_template_parameter_list (cp_parser* parser)
{
  tree parameter_list = NULL_TREE;

  begin_template_parm_list ();
  while (true)
    {
      tree parameter;
      bool is_non_type;
      bool is_parameter_pack;
      location_t parm_loc;

      /* Parse the template-parameter.  */
      parm_loc = cp_lexer_peek_token (parser->lexer)->location;
      parameter = cp_parser_template_parameter (parser, 
                                                &is_non_type,
                                                &is_parameter_pack);
      /* Add it to the list.  */
      if (parameter != error_mark_node)
	parameter_list = process_template_parm (parameter_list,
						parm_loc,
						parameter,
						is_non_type,
                                                is_parameter_pack);
      else
       {
         tree err_parm = build_tree_list (parameter, parameter);
         TREE_VALUE (err_parm) = error_mark_node;
         parameter_list = chainon (parameter_list, err_parm);
       }

      /* If the next token is not a `,', we're done.  */
      if (cp_lexer_next_token_is_not (parser->lexer, CPP_COMMA))
	break;
      /* Otherwise, consume the `,' token.  */
      cp_lexer_consume_token (parser->lexer);
    }

  return end_template_parm_list (parameter_list);
}

/* Parse a template-parameter.

   template-parameter:
     type-parameter
     parameter-declaration

   If all goes well, returns a TREE_LIST.  The TREE_VALUE represents
   the parameter.  The TREE_PURPOSE is the default value, if any.
   Returns ERROR_MARK_NODE on failure.  *IS_NON_TYPE is set to true
   iff this parameter is a non-type parameter.  *IS_PARAMETER_PACK is
   set to true iff this parameter is a parameter pack. */

static tree
cp_parser_template_parameter (cp_parser* parser, bool *is_non_type,
                              bool *is_parameter_pack)
{
  cp_token *token;
  cp_parameter_declarator *parameter_declarator;
  cp_declarator *id_declarator;
  tree parm;

  /* Assume it is a type parameter or a template parameter.  */
  *is_non_type = false;
  /* Assume it not a parameter pack. */
  *is_parameter_pack = false;
  /* Peek at the next token.  */
  token = cp_lexer_peek_token (parser->lexer);
  /* If it is `class' or `template', we have a type-parameter.  */
  if (token->keyword == RID_TEMPLATE)
    return cp_parser_type_parameter (parser, is_parameter_pack);
  /* If it is `class' or `typename' we do not know yet whether it is a
     type parameter or a non-type parameter.  Consider:

       template <typename T, typename T::X X> ...

     or:

       template <class C, class D*> ...

     Here, the first parameter is a type parameter, and the second is
     a non-type parameter.  We can tell by looking at the token after
     the identifier -- if it is a `,', `=', or `>' then we have a type
     parameter.  */
  if (token->keyword == RID_TYPENAME || token->keyword == RID_CLASS)
    {
      /* Peek at the token after `class' or `typename'.  */
      token = cp_lexer_peek_nth_token (parser->lexer, 2);
      /* If it's an ellipsis, we have a template type parameter
         pack. */
      if (token->type == CPP_ELLIPSIS)
        return cp_parser_type_parameter (parser, is_parameter_pack);
      /* If it's an identifier, skip it.  */
      if (token->type == CPP_NAME)
	token = cp_lexer_peek_nth_token (parser->lexer, 3);
      /* Now, see if the token looks like the end of a template
	 parameter.  */
      if (token->type == CPP_COMMA
	  || token->type == CPP_EQ
	  || token->type == CPP_GREATER)
	return cp_parser_type_parameter (parser, is_parameter_pack);
    }

  /* Otherwise, it is a non-type parameter.

     [temp.param]

     When parsing a default template-argument for a non-type
     template-parameter, the first non-nested `>' is taken as the end
     of the template parameter-list rather than a greater-than
     operator.  */
  *is_non_type = true;
  parameter_declarator
     = cp_parser_parameter_declaration (parser, /*template_parm_p=*/true,
					/*parenthesized_p=*/NULL);

  /* If the parameter declaration is marked as a parameter pack, set
     *IS_PARAMETER_PACK to notify the caller. Also, unmark the
     declarator's PACK_EXPANSION_P, otherwise we'll get errors from
     grokdeclarator. */
  if (parameter_declarator
      && parameter_declarator->declarator
      && parameter_declarator->declarator->parameter_pack_p)
    {
      *is_parameter_pack = true;
      parameter_declarator->declarator->parameter_pack_p = false;
    }

  /* If the next token is an ellipsis, and we don't already have it
     marked as a parameter pack, then we have a parameter pack (that
     has no declarator).  */
  if (!*is_parameter_pack
      && cp_lexer_next_token_is (parser->lexer, CPP_ELLIPSIS)
      && declarator_can_be_parameter_pack (parameter_declarator->declarator))
    {
      /* Consume the `...'.  */
      cp_lexer_consume_token (parser->lexer);
      maybe_warn_variadic_templates ();
      
      *is_parameter_pack = true;
    }
  /* We might end up with a pack expansion as the type of the non-type
     template parameter, in which case this is a non-type template
     parameter pack.  */
  else if (parameter_declarator
	   && parameter_declarator->decl_specifiers.type
	   && PACK_EXPANSION_P (parameter_declarator->decl_specifiers.type))
    {
      *is_parameter_pack = true;
      parameter_declarator->decl_specifiers.type = 
	PACK_EXPANSION_PATTERN (parameter_declarator->decl_specifiers.type);
    }

  if (*is_parameter_pack && cp_lexer_next_token_is (parser->lexer, CPP_EQ))
    {
      /* Parameter packs cannot have default arguments.  However, a
	 user may try to do so, so we'll parse them and give an
	 appropriate diagnostic here.  */

      /* Consume the `='.  */
      cp_token *start_token = cp_lexer_peek_token (parser->lexer);
      cp_lexer_consume_token (parser->lexer);
      
      /* Find the name of the parameter pack.  */     
      id_declarator = parameter_declarator->declarator;
      while (id_declarator && id_declarator->kind != cdk_id)
	id_declarator = id_declarator->declarator;
      
      if (id_declarator && id_declarator->kind == cdk_id)
	error_at (start_token->location,
		  "template parameter pack %qD cannot have a default argument",
		  id_declarator->u.id.unqualified_name);
      else
	error_at (start_token->location,
		  "template parameter pack cannot have a default argument");
      
      /* Parse the default argument, but throw away the result.  */
      cp_parser_default_argument (parser, /*template_parm_p=*/true);
    }

  parm = grokdeclarator (parameter_declarator->declarator,
			 &parameter_declarator->decl_specifiers,
			 TPARM, /*initialized=*/0,
			 /*attrlist=*/NULL);
  if (parm == error_mark_node)
    return error_mark_node;

  return build_tree_list (parameter_declarator->default_argument, parm);
}

/* Parse a type-parameter.

   type-parameter:
     class identifier [opt]
     class identifier [opt] = type-id
     typename identifier [opt]
     typename identifier [opt] = type-id
     template < template-parameter-list > class identifier [opt]
     template < template-parameter-list > class identifier [opt]
       = id-expression

   GNU Extension (variadic templates):

   type-parameter:
     class ... identifier [opt]
     typename ... identifier [opt]

   Returns a TREE_LIST.  The TREE_VALUE is itself a TREE_LIST.  The
   TREE_PURPOSE is the default-argument, if any.  The TREE_VALUE is
   the declaration of the parameter.

   Sets *IS_PARAMETER_PACK if this is a template parameter pack. */

static tree
cp_parser_type_parameter (cp_parser* parser, bool *is_parameter_pack)
{
  cp_token *token;
  tree parameter;

  /* Look for a keyword to tell us what kind of parameter this is.  */
  token = cp_parser_require (parser, CPP_KEYWORD,
			     "%<class%>, %<typename%>, or %<template%>");
  if (!token)
    return error_mark_node;

  switch (token->keyword)
    {
    case RID_CLASS:
    case RID_TYPENAME:
      {
	tree identifier;
	tree default_argument;

        /* If the next token is an ellipsis, we have a template
           argument pack. */
        if (cp_lexer_next_token_is (parser->lexer, CPP_ELLIPSIS))
          {
            /* Consume the `...' token. */
            cp_lexer_consume_token (parser->lexer);
            maybe_warn_variadic_templates ();

            *is_parameter_pack = true;
          }

	/* If the next token is an identifier, then it names the
	   parameter.  */
	if (cp_lexer_next_token_is (parser->lexer, CPP_NAME))
	  identifier = cp_parser_identifier (parser);
	else
	  identifier = NULL_TREE;

	/* Create the parameter.  */
	parameter = finish_template_type_parm (class_type_node, identifier);

	/* If the next token is an `=', we have a default argument.  */
	if (cp_lexer_next_token_is (parser->lexer, CPP_EQ))
	  {
	    /* Consume the `=' token.  */
	    cp_lexer_consume_token (parser->lexer);
	    /* Parse the default-argument.  */
	    push_deferring_access_checks (dk_no_deferred);
	    default_argument = cp_parser_type_id (parser);

            /* Template parameter packs cannot have default
               arguments. */
            if (*is_parameter_pack)
              {
                if (identifier)
                  error_at (token->location,
			    "template parameter pack %qD cannot have a "
			    "default argument", identifier);
                else
                  error_at (token->location,
			    "template parameter packs cannot have "
			    "default arguments");
                default_argument = NULL_TREE;
              }
	    pop_deferring_access_checks ();
	  }
	else
	  default_argument = NULL_TREE;

	/* Create the combined representation of the parameter and the
	   default argument.  */
	parameter = build_tree_list (default_argument, parameter);
      }
      break;

    case RID_TEMPLATE:
      {
	tree identifier;
	tree default_argument;

	/* Look for the `<'.  */
	cp_parser_require (parser, CPP_LESS, "%<<%>");
	/* Parse the template-parameter-list.  */
	cp_parser_template_parameter_list (parser);
	/* Look for the `>'.  */
	cp_parser_require (parser, CPP_GREATER, "%<>%>");
	/* Look for the `class' keyword.  */
	cp_parser_require_keyword (parser, RID_CLASS, "%<class%>");
        /* If the next token is an ellipsis, we have a template
           argument pack. */
        if (cp_lexer_next_token_is (parser->lexer, CPP_ELLIPSIS))
          {
            /* Consume the `...' token. */
            cp_lexer_consume_token (parser->lexer);
            maybe_warn_variadic_templates ();

            *is_parameter_pack = true;
          }
	/* If the next token is an `=', then there is a
	   default-argument.  If the next token is a `>', we are at
	   the end of the parameter-list.  If the next token is a `,',
	   then we are at the end of this parameter.  */
	if (cp_lexer_next_token_is_not (parser->lexer, CPP_EQ)
	    && cp_lexer_next_token_is_not (parser->lexer, CPP_GREATER)
	    && cp_lexer_next_token_is_not (parser->lexer, CPP_COMMA))
	  {
	    identifier = cp_parser_identifier (parser);
	    /* Treat invalid names as if the parameter were nameless.  */
	    if (identifier == error_mark_node)
	      identifier = NULL_TREE;
	  }
	else
	  identifier = NULL_TREE;

	/* Create the template parameter.  */
	parameter = finish_template_template_parm (class_type_node,
						   identifier);

	/* If the next token is an `=', then there is a
	   default-argument.  */
	if (cp_lexer_next_token_is (parser->lexer, CPP_EQ))
	  {
	    bool is_template;

	    /* Consume the `='.  */
	    cp_lexer_consume_token (parser->lexer);
	    /* Parse the id-expression.  */
	    push_deferring_access_checks (dk_no_deferred);
	    /* save token before parsing the id-expression, for error
	       reporting */
	    token = cp_lexer_peek_token (parser->lexer);
	    default_argument
	      = cp_parser_id_expression (parser,
					 /*template_keyword_p=*/false,
					 /*check_dependency_p=*/true,
					 /*template_p=*/&is_template,
					 /*declarator_p=*/false,
					 /*optional_p=*/false);
	    if (TREE_CODE (default_argument) == TYPE_DECL)
	      /* If the id-expression was a template-id that refers to
		 a template-class, we already have the declaration here,
		 so no further lookup is needed.  */
		 ;
	    else
	      /* Look up the name.  */
	      default_argument
		= cp_parser_lookup_name (parser, default_argument,
					 none_type,
					 /*is_template=*/is_template,
					 /*is_namespace=*/false,
					 /*check_dependency=*/true,
					 /*ambiguous_decls=*/NULL,
					 token->location);
	    /* See if the default argument is valid.  */
	    default_argument
	      = check_template_template_default_arg (default_argument);

            /* Template parameter packs cannot have default
               arguments. */
            if (*is_parameter_pack)
              {
                if (identifier)
                  error_at (token->location,
			    "template parameter pack %qD cannot "
			    "have a default argument",
			    identifier);
                else
                  error_at (token->location, "template parameter packs cannot "
			    "have default arguments");
                default_argument = NULL_TREE;
              }
	    pop_deferring_access_checks ();
	  }
	else
	  default_argument = NULL_TREE;

	/* Create the combined representation of the parameter and the
	   default argument.  */
	parameter = build_tree_list (default_argument, parameter);
      }
      break;

    default:
      gcc_unreachable ();
      break;
    }

  return parameter;
}

/* Parse a template-id.

   template-id:
     template-name < template-argument-list [opt] >

   If TEMPLATE_KEYWORD_P is TRUE, then we have just seen the
   `template' keyword.  In this case, a TEMPLATE_ID_EXPR will be
   returned.  Otherwise, if the template-name names a function, or set
   of functions, returns a TEMPLATE_ID_EXPR.  If the template-name
   names a class, returns a TYPE_DECL for the specialization.

   If CHECK_DEPENDENCY_P is FALSE, names are looked up in
   uninstantiated templates.  */

static tree
cp_parser_template_id (cp_parser *parser,
		       bool template_keyword_p,
		       bool check_dependency_p,
		       bool is_declaration)
{
  int i;
  tree templ;
  tree arguments;
  tree template_id;
  cp_token_position start_of_id = 0;
  deferred_access_check *chk;
  VEC (deferred_access_check,gc) *access_check;
  cp_token *next_token = NULL, *next_token_2 = NULL;
  bool is_identifier;

  /* If the next token corresponds to a template-id, there is no need
     to reparse it.  */
  next_token = cp_lexer_peek_token (parser->lexer);
  if (next_token->type == CPP_TEMPLATE_ID)
    {
      struct tree_check *check_value;

      /* Get the stored value.  */
      check_value = cp_lexer_consume_token (parser->lexer)->u.tree_check_value;
      /* Perform any access checks that were deferred.  */
      access_check = check_value->checks;
      if (access_check)
	{
	  for (i = 0 ;
	       VEC_iterate (deferred_access_check, access_check, i, chk) ;
	       ++i)
	    {
	      perform_or_defer_access_check (chk->binfo,
					     chk->decl,
					     chk->diag_decl);
	    }
	}
      /* Return the stored value.  */
      return check_value->value;
    }

  /* Avoid performing name lookup if there is no possibility of
     finding a template-id.  */
  if ((next_token->type != CPP_NAME && next_token->keyword != RID_OPERATOR)
      || (next_token->type == CPP_NAME
	  && !cp_parser_nth_token_starts_template_argument_list_p
	       (parser, 2)))
    {
      cp_parser_error (parser, "expected template-id");
      return error_mark_node;
    }

  /* Remember where the template-id starts.  */
  if (cp_parser_uncommitted_to_tentative_parse_p (parser))
    start_of_id = cp_lexer_token_position (parser->lexer, false);

  push_deferring_access_checks (dk_deferred);

  /* Parse the template-name.  */
  is_identifier = false;
  templ = cp_parser_template_name (parser, template_keyword_p,
				   check_dependency_p,
				   is_declaration,
				   &is_identifier);
  if (templ == error_mark_node || is_identifier)
    {
      pop_deferring_access_checks ();
      return templ;
    }

  /* If we find the sequence `[:' after a template-name, it's probably
     a digraph-typo for `< ::'. Substitute the tokens and check if we can
     parse correctly the argument list.  */
  next_token = cp_lexer_peek_token (parser->lexer);
  next_token_2 = cp_lexer_peek_nth_token (parser->lexer, 2);
  if (next_token->type == CPP_OPEN_SQUARE
      && next_token->flags & DIGRAPH
      && next_token_2->type == CPP_COLON
      && !(next_token_2->flags & PREV_WHITE))
    {
      cp_parser_parse_tentatively (parser);
      /* Change `:' into `::'.  */
      next_token_2->type = CPP_SCOPE;
      /* Consume the first token (CPP_OPEN_SQUARE - which we pretend it is
	 CPP_LESS.  */
      cp_lexer_consume_token (parser->lexer);

      /* Parse the arguments.  */
      arguments = cp_parser_enclosed_template_argument_list (parser);
      if (!cp_parser_parse_definitely (parser))
	{
	  /* If we couldn't parse an argument list, then we revert our changes
	     and return simply an error. Maybe this is not a template-id
	     after all.  */
	  next_token_2->type = CPP_COLON;
	  cp_parser_error (parser, "expected %<<%>");
	  pop_deferring_access_checks ();
	  return error_mark_node;
	}
      /* Otherwise, emit an error about the invalid digraph, but continue
	 parsing because we got our argument list.  */
      if (permerror (next_token->location,
		     "%<<::%> cannot begin a template-argument list"))
	{
	  static bool hint = false;
	  inform (next_token->location,
		  "%<<:%> is an alternate spelling for %<[%>."
		  " Insert whitespace between %<<%> and %<::%>");
	  if (!hint && !flag_permissive)
	    {
	      inform (next_token->location, "(if you use %<-fpermissive%>"
		      " G++ will accept your code)");
	      hint = true;
	    }
	}
    }
  else
    {
      /* Look for the `<' that starts the template-argument-list.  */
      if (!cp_parser_require (parser, CPP_LESS, "%<<%>"))
	{
	  pop_deferring_access_checks ();
	  return error_mark_node;
	}
      /* Parse the arguments.  */
      arguments = cp_parser_enclosed_template_argument_list (parser);
    }

  /* Build a representation of the specialization.  */
  if (TREE_CODE (templ) == IDENTIFIER_NODE)
    template_id = build_min_nt (TEMPLATE_ID_EXPR, templ, arguments);
  else if (DECL_CLASS_TEMPLATE_P (templ)
	   || DECL_TEMPLATE_TEMPLATE_PARM_P (templ))
    {
      bool entering_scope;
      /* In "template <typename T> ... A<T>::", A<T> is the abstract A
	 template (rather than some instantiation thereof) only if
	 is not nested within some other construct.  For example, in
	 "template <typename T> void f(T) { A<T>::", A<T> is just an
	 instantiation of A.  */
      entering_scope = (template_parm_scope_p ()
			&& cp_lexer_next_token_is (parser->lexer,
						   CPP_SCOPE));
      template_id
	= finish_template_type (templ, arguments, entering_scope);
    }
  else
    {
      /* If it's not a class-template or a template-template, it should be
	 a function-template.  */
      gcc_assert ((DECL_FUNCTION_TEMPLATE_P (templ)
		   || TREE_CODE (templ) == OVERLOAD
		   || BASELINK_P (templ)));

      template_id = lookup_template_function (templ, arguments);
    }

  /* If parsing tentatively, replace the sequence of tokens that makes
     up the template-id with a CPP_TEMPLATE_ID token.  That way,
     should we re-parse the token stream, we will not have to repeat
     the effort required to do the parse, nor will we issue duplicate
     error messages about problems during instantiation of the
     template.  */
  if (start_of_id)
    {
      cp_token *token = cp_lexer_token_at (parser->lexer, start_of_id);

      /* Reset the contents of the START_OF_ID token.  */
      token->type = CPP_TEMPLATE_ID;
      /* Retrieve any deferred checks.  Do not pop this access checks yet
	 so the memory will not be reclaimed during token replacing below.  */
      token->u.tree_check_value = GGC_CNEW (struct tree_check);
      token->u.tree_check_value->value = template_id;
      token->u.tree_check_value->checks = get_deferred_access_checks ();
      token->keyword = RID_MAX;

      /* Purge all subsequent tokens.  */
      cp_lexer_purge_tokens_after (parser->lexer, start_of_id);

      /* ??? Can we actually assume that, if template_id ==
	 error_mark_node, we will have issued a diagnostic to the
	 user, as opposed to simply marking the tentative parse as
	 failed?  */
      if (cp_parser_error_occurred (parser) && template_id != error_mark_node)
	error_at (token->location, "parse error in template argument list");
    }

  pop_deferring_access_checks ();
  return template_id;
}

/* Parse a template-name.

   template-name:
     identifier

   The standard should actually say:

   template-name:
     identifier
     operator-function-id

   A defect report has been filed about this issue.

   A conversion-function-id cannot be a template name because they cannot
   be part of a template-id. In fact, looking at this code:

   a.operator K<int>()

   the conversion-function-id is "operator K<int>", and K<int> is a type-id.
   It is impossible to call a templated conversion-function-id with an
   explicit argument list, since the only allowed template parameter is
   the type to which it is converting.

   If TEMPLATE_KEYWORD_P is true, then we have just seen the
   `template' keyword, in a construction like:

     T::template f<3>()

   In that case `f' is taken to be a template-name, even though there
   is no way of knowing for sure.

   Returns the TEMPLATE_DECL for the template, or an OVERLOAD if the
   name refers to a set of overloaded functions, at least one of which
   is a template, or an IDENTIFIER_NODE with the name of the template,
   if TEMPLATE_KEYWORD_P is true.  If CHECK_DEPENDENCY_P is FALSE,
   names are looked up inside uninstantiated templates.  */

static tree
cp_parser_template_name (cp_parser* parser,
			 bool template_keyword_p,
			 bool check_dependency_p,
			 bool is_declaration,
			 bool *is_identifier)
{
  tree identifier;
  tree decl;
  tree fns;
  cp_token *token = cp_lexer_peek_token (parser->lexer);

  /* If the next token is `operator', then we have either an
     operator-function-id or a conversion-function-id.  */
  if (cp_lexer_next_token_is_keyword (parser->lexer, RID_OPERATOR))
    {
      /* We don't know whether we're looking at an
	 operator-function-id or a conversion-function-id.  */
      cp_parser_parse_tentatively (parser);
      /* Try an operator-function-id.  */
      identifier = cp_parser_operator_function_id (parser);
      /* If that didn't work, try a conversion-function-id.  */
      if (!cp_parser_parse_definitely (parser))
	{
	  cp_parser_error (parser, "expected template-name");
	  return error_mark_node;
	}
    }
  /* Look for the identifier.  */
  else
    identifier = cp_parser_identifier (parser);

  /* If we didn't find an identifier, we don't have a template-id.  */
  if (identifier == error_mark_node)
    return error_mark_node;

  /* If the name immediately followed the `template' keyword, then it
     is a template-name.  However, if the next token is not `<', then
     we do not treat it as a template-name, since it is not being used
     as part of a template-id.  This enables us to handle constructs
     like:

       template <typename T> struct S { S(); };
       template <typename T> S<T>::S();

     correctly.  We would treat `S' as a template -- if it were `S<T>'
     -- but we do not if there is no `<'.  */

  if (processing_template_decl
      && cp_parser_nth_token_starts_template_argument_list_p (parser, 1))
    {
      /* In a declaration, in a dependent context, we pretend that the
	 "template" keyword was present in order to improve error
	 recovery.  For example, given:

	   template <typename T> void f(T::X<int>);

	 we want to treat "X<int>" as a template-id.  */
      if (is_declaration
	  && !template_keyword_p
	  && parser->scope && TYPE_P (parser->scope)
	  && check_dependency_p
	  && dependent_scope_p (parser->scope)
	  /* Do not do this for dtors (or ctors), since they never
	     need the template keyword before their name.  */
	  && !constructor_name_p (identifier, parser->scope))
	{
	  cp_token_position start = 0;

	  /* Explain what went wrong.  */
	  error_at (token->location, "non-template %qD used as template",
		    identifier);
	  inform (token->location, "use %<%T::template %D%> to indicate that it is a template",
		  parser->scope, identifier);
	  /* If parsing tentatively, find the location of the "<" token.  */
	  if (cp_parser_simulate_error (parser))
	    start = cp_lexer_token_position (parser->lexer, true);
	  /* Parse the template arguments so that we can issue error
	     messages about them.  */
	  cp_lexer_consume_token (parser->lexer);
	  cp_parser_enclosed_template_argument_list (parser);
	  /* Skip tokens until we find a good place from which to
	     continue parsing.  */
	  cp_parser_skip_to_closing_parenthesis (parser,
						 /*recovering=*/true,
						 /*or_comma=*/true,
						 /*consume_paren=*/false);
	  /* If parsing tentatively, permanently remove the
	     template argument list.  That will prevent duplicate
	     error messages from being issued about the missing
	     "template" keyword.  */
	  if (start)
	    cp_lexer_purge_tokens_after (parser->lexer, start);
	  if (is_identifier)
	    *is_identifier = true;
	  return identifier;
	}

      /* If the "template" keyword is present, then there is generally
	 no point in doing name-lookup, so we just return IDENTIFIER.
	 But, if the qualifying scope is non-dependent then we can
	 (and must) do name-lookup normally.  */
      if (template_keyword_p
	  && (!parser->scope
	      || (TYPE_P (parser->scope)
		  && dependent_type_p (parser->scope))))
	return identifier;
    }

  /* Look up the name.  */
  decl = cp_parser_lookup_name (parser, identifier,
				none_type,
				/*is_template=*/true,
				/*is_namespace=*/false,
				check_dependency_p,
				/*ambiguous_decls=*/NULL,
				token->location);

  /* If DECL is a template, then the name was a template-name.  */
  if (TREE_CODE (decl) == TEMPLATE_DECL)
    ;
  else
    {
      tree fn = NULL_TREE;

      /* The standard does not explicitly indicate whether a name that
	 names a set of overloaded declarations, some of which are
	 templates, is a template-name.  However, such a name should
	 be a template-name; otherwise, there is no way to form a
	 template-id for the overloaded templates.  */
      fns = BASELINK_P (decl) ? BASELINK_FUNCTIONS (decl) : decl;
      if (TREE_CODE (fns) == OVERLOAD)
	for (fn = fns; fn; fn = OVL_NEXT (fn))
	  if (TREE_CODE (OVL_CURRENT (fn)) == TEMPLATE_DECL)
	    break;

      if (!fn)
	{
	  /* The name does not name a template.  */
	  cp_parser_error (parser, "expected template-name");
	  return error_mark_node;
	}
    }

  /* If DECL is dependent, and refers to a function, then just return
     its name; we will look it up again during template instantiation.  */
  if (DECL_FUNCTION_TEMPLATE_P (decl) || !DECL_P (decl))
    {
      tree scope = CP_DECL_CONTEXT (get_first_fn (decl));
      if (TYPE_P (scope) && dependent_type_p (scope))
	return identifier;
    }

  return decl;
}

/* Parse a template-argument-list.

   template-argument-list:
     template-argument ... [opt]
     template-argument-list , template-argument ... [opt]

   Returns a TREE_VEC containing the arguments.  */

static tree
cp_parser_template_argument_list (cp_parser* parser)
{
  tree fixed_args[10];
  unsigned n_args = 0;
  unsigned alloced = 10;
  tree *arg_ary = fixed_args;
  tree vec;
  bool saved_in_template_argument_list_p;
  bool saved_ice_p;
  bool saved_non_ice_p;

  saved_in_template_argument_list_p = parser->in_template_argument_list_p;
  parser->in_template_argument_list_p = true;
  /* Even if the template-id appears in an integral
     constant-expression, the contents of the argument list do
     not.  */
  saved_ice_p = parser->integral_constant_expression_p;
  parser->integral_constant_expression_p = false;
  saved_non_ice_p = parser->non_integral_constant_expression_p;
  parser->non_integral_constant_expression_p = false;
  /* Parse the arguments.  */
  do
    {
      tree argument;

      if (n_args)
	/* Consume the comma.  */
	cp_lexer_consume_token (parser->lexer);

      /* Parse the template-argument.  */
      argument = cp_parser_template_argument (parser);

      /* If the next token is an ellipsis, we're expanding a template
         argument pack. */
      if (cp_lexer_next_token_is (parser->lexer, CPP_ELLIPSIS))
        {
	  if (argument == error_mark_node)
	    {
	      cp_token *token = cp_lexer_peek_token (parser->lexer);
	      error_at (token->location,
			"expected parameter pack before %<...%>");
	    }
          /* Consume the `...' token. */
          cp_lexer_consume_token (parser->lexer);

          /* Make the argument into a TYPE_PACK_EXPANSION or
             EXPR_PACK_EXPANSION. */
          argument = make_pack_expansion (argument);
        }

      if (n_args == alloced)
	{
	  alloced *= 2;

	  if (arg_ary == fixed_args)
	    {
	      arg_ary = XNEWVEC (tree, alloced);
	      memcpy (arg_ary, fixed_args, sizeof (tree) * n_args);
	    }
	  else
	    arg_ary = XRESIZEVEC (tree, arg_ary, alloced);
	}
      arg_ary[n_args++] = argument;
    }
  while (cp_lexer_next_token_is (parser->lexer, CPP_COMMA));

  vec = make_tree_vec (n_args);

  while (n_args--)
    TREE_VEC_ELT (vec, n_args) = arg_ary[n_args];

  if (arg_ary != fixed_args)
    free (arg_ary);
  parser->non_integral_constant_expression_p = saved_non_ice_p;
  parser->integral_constant_expression_p = saved_ice_p;
  parser->in_template_argument_list_p = saved_in_template_argument_list_p;
#ifdef ENABLE_CHECKING
  SET_NON_DEFAULT_TEMPLATE_ARGS_COUNT (vec, TREE_VEC_LENGTH (vec));
#endif
  return vec;
}

/* Parse a template-argument.

   template-argument:
     assignment-expression
     type-id
     id-expression

   The representation is that of an assignment-expression, type-id, or
   id-expression -- except that the qualified id-expression is
   evaluated, so that the value returned is either a DECL or an
   OVERLOAD.

   Although the standard says "assignment-expression", it forbids
   throw-expressions or assignments in the template argument.
   Therefore, we use "conditional-expression" instead.  */

static tree
cp_parser_template_argument (cp_parser* parser)
{
  tree argument;
  bool template_p;
  bool address_p;
  bool maybe_type_id = false;
  cp_token *token = NULL, *argument_start_token = NULL;
  cp_id_kind idk;

  /* There's really no way to know what we're looking at, so we just
     try each alternative in order.

       [temp.arg]

       In a template-argument, an ambiguity between a type-id and an
       expression is resolved to a type-id, regardless of the form of
       the corresponding template-parameter.

     Therefore, we try a type-id first.  */
  cp_parser_parse_tentatively (parser);
  argument = cp_parser_template_type_arg (parser);
  /* If there was no error parsing the type-id but the next token is a
     '>>', our behavior depends on which dialect of C++ we're
     parsing. In C++98, we probably found a typo for '> >'. But there
     are type-id which are also valid expressions. For instance:

     struct X { int operator >> (int); };
     template <int V> struct Foo {};
     Foo<X () >> 5> r;

     Here 'X()' is a valid type-id of a function type, but the user just
     wanted to write the expression "X() >> 5". Thus, we remember that we
     found a valid type-id, but we still try to parse the argument as an
     expression to see what happens. 

     In C++0x, the '>>' will be considered two separate '>'
     tokens.  */
  if (!cp_parser_error_occurred (parser)
      && cxx_dialect == cxx98
      && cp_lexer_next_token_is (parser->lexer, CPP_RSHIFT))
    {
      maybe_type_id = true;
      cp_parser_abort_tentative_parse (parser);
    }
  else
    {
      /* If the next token isn't a `,' or a `>', then this argument wasn't
      really finished. This means that the argument is not a valid
      type-id.  */
      if (!cp_parser_next_token_ends_template_argument_p (parser))
	cp_parser_error (parser, "expected template-argument");
      /* If that worked, we're done.  */
      if (cp_parser_parse_definitely (parser))
	return argument;
    }
  /* We're still not sure what the argument will be.  */
  cp_parser_parse_tentatively (parser);
  /* Try a template.  */
  argument_start_token = cp_lexer_peek_token (parser->lexer);
  argument = cp_parser_id_expression (parser,
				      /*template_keyword_p=*/false,
				      /*check_dependency_p=*/true,
				      &template_p,
				      /*declarator_p=*/false,
				      /*optional_p=*/false);
  /* If the next token isn't a `,' or a `>', then this argument wasn't
     really finished.  */
  if (!cp_parser_next_token_ends_template_argument_p (parser))
    cp_parser_error (parser, "expected template-argument");
  if (!cp_parser_error_occurred (parser))
    {
      /* Figure out what is being referred to.  If the id-expression
	 was for a class template specialization, then we will have a
	 TYPE_DECL at this point.  There is no need to do name lookup
	 at this point in that case.  */
      if (TREE_CODE (argument) != TYPE_DECL)
	argument = cp_parser_lookup_name (parser, argument,
					  none_type,
					  /*is_template=*/template_p,
					  /*is_namespace=*/false,
					  /*check_dependency=*/true,
					  /*ambiguous_decls=*/NULL,
					  argument_start_token->location);
      if (TREE_CODE (argument) != TEMPLATE_DECL
	  && TREE_CODE (argument) != UNBOUND_CLASS_TEMPLATE)
	cp_parser_error (parser, "expected template-name");
    }
  if (cp_parser_parse_definitely (parser))
    return argument;
  /* It must be a non-type argument.  There permitted cases are given
     in [temp.arg.nontype]:

     -- an integral constant-expression of integral or enumeration
	type; or

     -- the name of a non-type template-parameter; or

     -- the name of an object or function with external linkage...

     -- the address of an object or function with external linkage...

     -- a pointer to member...  */
  /* Look for a non-type template parameter.  */
  if (cp_lexer_next_token_is (parser->lexer, CPP_NAME))
    {
      cp_parser_parse_tentatively (parser);
      argument = cp_parser_primary_expression (parser,
					       /*address_p=*/false,
					       /*cast_p=*/false,
					       /*template_arg_p=*/true,
					       &idk);
      if (TREE_CODE (argument) != TEMPLATE_PARM_INDEX
	  || !cp_parser_next_token_ends_template_argument_p (parser))
	cp_parser_simulate_error (parser);
      if (cp_parser_parse_definitely (parser))
	return argument;
    }

  /* If the next token is "&", the argument must be the address of an
     object or function with external linkage.  */
  address_p = cp_lexer_next_token_is (parser->lexer, CPP_AND);
  if (address_p)
    cp_lexer_consume_token (parser->lexer);
  /* See if we might have an id-expression.  */
  token = cp_lexer_peek_token (parser->lexer);
  if (token->type == CPP_NAME
      || token->keyword == RID_OPERATOR
      || token->type == CPP_SCOPE
      || token->type == CPP_TEMPLATE_ID
      || token->type == CPP_NESTED_NAME_SPECIFIER)
    {
      cp_parser_parse_tentatively (parser);
      argument = cp_parser_primary_expression (parser,
					       address_p,
					       /*cast_p=*/false,
					       /*template_arg_p=*/true,
					       &idk);
      if (cp_parser_error_occurred (parser)
	  || !cp_parser_next_token_ends_template_argument_p (parser))
	cp_parser_abort_tentative_parse (parser);
      else
	{
	  tree probe;

	  if (TREE_CODE (argument) == INDIRECT_REF)
	    {
	      gcc_assert (REFERENCE_REF_P (argument));
	      argument = TREE_OPERAND (argument, 0);
	    }

	  /* If we're in a template, we represent a qualified-id referring
	     to a static data member as a SCOPE_REF even if the scope isn't
	     dependent so that we can check access control later.  */
	  probe = argument;
	  if (TREE_CODE (probe) == SCOPE_REF)
	    probe = TREE_OPERAND (probe, 1);
	  if (TREE_CODE (probe) == VAR_DECL)
	    {
	      /* A variable without external linkage might still be a
		 valid constant-expression, so no error is issued here
		 if the external-linkage check fails.  */
	      if (!address_p && !DECL_EXTERNAL_LINKAGE_P (probe))
		cp_parser_simulate_error (parser);
	    }
	  else if (is_overloaded_fn (argument))
	    /* All overloaded functions are allowed; if the external
	       linkage test does not pass, an error will be issued
	       later.  */
	    ;
	  else if (address_p
		   && (TREE_CODE (argument) == OFFSET_REF
		       || TREE_CODE (argument) == SCOPE_REF))
	    /* A pointer-to-member.  */
	    ;
	  else if (TREE_CODE (argument) == TEMPLATE_PARM_INDEX)
	    ;
	  else
	    cp_parser_simulate_error (parser);

	  if (cp_parser_parse_definitely (parser))
	    {
	      if (address_p)
		argument = build_x_unary_op (ADDR_EXPR, argument,
                                             tf_warning_or_error);
	      return argument;
	    }
	}
    }
  /* If the argument started with "&", there are no other valid
     alternatives at this point.  */
  if (address_p)
    {
      cp_parser_error (parser, "invalid non-type template argument");
      return error_mark_node;
    }

  /* If the argument wasn't successfully parsed as a type-id followed
     by '>>', the argument can only be a constant expression now.
     Otherwise, we try parsing the constant-expression tentatively,
     because the argument could really be a type-id.  */
  if (maybe_type_id)
    cp_parser_parse_tentatively (parser);
  argument = cp_parser_constant_expression (parser,
					    /*allow_non_constant_p=*/false,
					    /*non_constant_p=*/NULL);
  argument = fold_non_dependent_expr (argument);
  if (!maybe_type_id)
    return argument;
  if (!cp_parser_next_token_ends_template_argument_p (parser))
    cp_parser_error (parser, "expected template-argument");
  if (cp_parser_parse_definitely (parser))
    return argument;
  /* We did our best to parse the argument as a non type-id, but that
     was the only alternative that matched (albeit with a '>' after
     it). We can assume it's just a typo from the user, and a
     diagnostic will then be issued.  */
  return cp_parser_template_type_arg (parser);
}

/* Parse an explicit-instantiation.

   explicit-instantiation:
     template declaration

   Although the standard says `declaration', what it really means is:

   explicit-instantiation:
     template decl-specifier-seq [opt] declarator [opt] ;

   Things like `template int S<int>::i = 5, int S<double>::j;' are not
   supposed to be allowed.  A defect report has been filed about this
   issue.

   GNU Extension:

   explicit-instantiation:
     storage-class-specifier template
       decl-specifier-seq [opt] declarator [opt] ;
     function-specifier template
       decl-specifier-seq [opt] declarator [opt] ;  */

static void
cp_parser_explicit_instantiation (cp_parser* parser)
{
  int declares_class_or_enum;
  cp_decl_specifier_seq decl_specifiers;
  tree extension_specifier = NULL_TREE;

  /* Look for an (optional) storage-class-specifier or
     function-specifier.  */
  if (cp_parser_allow_gnu_extensions_p (parser))
    {
      extension_specifier
	= cp_parser_storage_class_specifier_opt (parser);
      if (!extension_specifier)
	extension_specifier
	  = cp_parser_function_specifier_opt (parser,
					      /*decl_specs=*/NULL);
    }

  /* Look for the `template' keyword.  */
  cp_parser_require_keyword (parser, RID_TEMPLATE, "%<template%>");
  /* Let the front end know that we are processing an explicit
     instantiation.  */
  begin_explicit_instantiation ();
  /* [temp.explicit] says that we are supposed to ignore access
     control while processing explicit instantiation directives.  */
  push_deferring_access_checks (dk_no_check);
  /* Parse a decl-specifier-seq.  */
  cp_parser_decl_specifier_seq (parser,
				CP_PARSER_FLAGS_OPTIONAL,
				&decl_specifiers,
				&declares_class_or_enum);
  /* If there was exactly one decl-specifier, and it declared a class,
     and there's no declarator, then we have an explicit type
     instantiation.  */
  if (declares_class_or_enum && cp_parser_declares_only_class_p (parser))
    {
      tree type;

      type = check_tag_decl (&decl_specifiers);
      /* Turn access control back on for names used during
	 template instantiation.  */
      pop_deferring_access_checks ();
      if (type)
	do_type_instantiation (type, extension_specifier,
			       /*complain=*/tf_error);
    }
  else
    {
      cp_declarator *declarator;
      tree decl;

      /* Parse the declarator.  */
      declarator
	= cp_parser_declarator (parser, CP_PARSER_DECLARATOR_NAMED,
				/*ctor_dtor_or_conv_p=*/NULL,
				/*parenthesized_p=*/NULL,
				/*member_p=*/false);
      if (declares_class_or_enum & 2)
	cp_parser_check_for_definition_in_return_type (declarator,
						       decl_specifiers.type,
						       decl_specifiers.type_location);
      if (declarator != cp_error_declarator)
	{
	  decl = grokdeclarator (declarator, &decl_specifiers,
				 NORMAL, 0, &decl_specifiers.attributes);
	  /* Turn access control back on for names used during
	     template instantiation.  */
	  pop_deferring_access_checks ();
	  /* Do the explicit instantiation.  */
	  do_decl_instantiation (decl, extension_specifier);
	}
      else
	{
	  pop_deferring_access_checks ();
	  /* Skip the body of the explicit instantiation.  */
	  cp_parser_skip_to_end_of_statement (parser);
	}
    }
  /* We're done with the instantiation.  */
  end_explicit_instantiation ();

  cp_parser_consume_semicolon_at_end_of_statement (parser);
}

/* Parse an explicit-specialization.

   explicit-specialization:
     template < > declaration

   Although the standard says `declaration', what it really means is:

   explicit-specialization:
     template <> decl-specifier [opt] init-declarator [opt] ;
     template <> function-definition
     template <> explicit-specialization
     template <> template-declaration  */

static void
cp_parser_explicit_specialization (cp_parser* parser)
{
  bool need_lang_pop;
  cp_token *token = cp_lexer_peek_token (parser->lexer);

  /* Look for the `template' keyword.  */
  cp_parser_require_keyword (parser, RID_TEMPLATE, "%<template%>");
  /* Look for the `<'.  */
  cp_parser_require (parser, CPP_LESS, "%<<%>");
  /* Look for the `>'.  */
  cp_parser_require (parser, CPP_GREATER, "%<>%>");
  /* We have processed another parameter list.  */
  ++parser->num_template_parameter_lists;
  /* [temp]

     A template ... explicit specialization ... shall not have C
     linkage.  */
  if (current_lang_name == lang_name_c)
    {
      error_at (token->location, "template specialization with C linkage");
      /* Give it C++ linkage to avoid confusing other parts of the
	 front end.  */
      push_lang_context (lang_name_cplusplus);
      need_lang_pop = true;
    }
  else
    need_lang_pop = false;
  /* Let the front end know that we are beginning a specialization.  */
  if (!begin_specialization ())
    {
      end_specialization ();
      return;
    }

  /* If the next keyword is `template', we need to figure out whether
     or not we're looking a template-declaration.  */
  if (cp_lexer_next_token_is_keyword (parser->lexer, RID_TEMPLATE))
    {
      if (cp_lexer_peek_nth_token (parser->lexer, 2)->type == CPP_LESS
	  && cp_lexer_peek_nth_token (parser->lexer, 3)->type != CPP_GREATER)
	cp_parser_template_declaration_after_export (parser,
						     /*member_p=*/false);
      else
	cp_parser_explicit_specialization (parser);
    }
  else
    /* Parse the dependent declaration.  */
    cp_parser_single_declaration (parser,
				  /*checks=*/NULL,
				  /*member_p=*/false,
                                  /*explicit_specialization_p=*/true,
				  /*friend_p=*/NULL);
  /* We're done with the specialization.  */
  end_specialization ();
  /* For the erroneous case of a template with C linkage, we pushed an
     implicit C++ linkage scope; exit that scope now.  */
  if (need_lang_pop)
    pop_lang_context ();
  /* We're done with this parameter list.  */
  --parser->num_template_parameter_lists;
}

/* Parse a type-specifier.

   type-specifier:
     simple-type-specifier
     class-specifier
     enum-specifier
     elaborated-type-specifier
     cv-qualifier

   GNU Extension:

   type-specifier:
     __complex__

   Returns a representation of the type-specifier.  For a
   class-specifier, enum-specifier, or elaborated-type-specifier, a
   TREE_TYPE is returned; otherwise, a TYPE_DECL is returned.

   The parser flags FLAGS is used to control type-specifier parsing.

   If IS_DECLARATION is TRUE, then this type-specifier is appearing
   in a decl-specifier-seq.

   If DECLARES_CLASS_OR_ENUM is non-NULL, and the type-specifier is a
   class-specifier, enum-specifier, or elaborated-type-specifier, then
   *DECLARES_CLASS_OR_ENUM is set to a nonzero value.  The value is 1
   if a type is declared; 2 if it is defined.  Otherwise, it is set to
   zero.

   If IS_CV_QUALIFIER is non-NULL, and the type-specifier is a
   cv-qualifier, then IS_CV_QUALIFIER is set to TRUE.  Otherwise, it
   is set to FALSE.  */

static tree
cp_parser_type_specifier (cp_parser* parser,
			  cp_parser_flags flags,
			  cp_decl_specifier_seq *decl_specs,
			  bool is_declaration,
			  int* declares_class_or_enum,
			  bool* is_cv_qualifier)
{
  tree type_spec = NULL_TREE;
  cp_token *token;
  enum rid keyword;
  cp_decl_spec ds = ds_last;

  /* Assume this type-specifier does not declare a new type.  */
  if (declares_class_or_enum)
    *declares_class_or_enum = 0;
  /* And that it does not specify a cv-qualifier.  */
  if (is_cv_qualifier)
    *is_cv_qualifier = false;
  /* Peek at the next token.  */
  token = cp_lexer_peek_token (parser->lexer);

  /* If we're looking at a keyword, we can use that to guide the
     production we choose.  */
  keyword = token->keyword;
  switch (keyword)
    {
    case RID_ENUM:
      if ((flags & CP_PARSER_FLAGS_NO_TYPE_DEFINITIONS))
	goto elaborated_type_specifier;

      /* Look for the enum-specifier.  */
      type_spec = cp_parser_enum_specifier (parser);
      /* If that worked, we're done.  */
      if (type_spec)
	{
	  if (declares_class_or_enum)
	    *declares_class_or_enum = 2;
	  if (decl_specs)
	    cp_parser_set_decl_spec_type (decl_specs,
					  type_spec,
					  token->location,
					  /*user_defined_p=*/true);
	  return type_spec;
	}
      else
	goto elaborated_type_specifier;

      /* Any of these indicate either a class-specifier, or an
	 elaborated-type-specifier.  */
    case RID_CLASS:
    case RID_STRUCT:
    case RID_UNION:
      if ((flags & CP_PARSER_FLAGS_NO_TYPE_DEFINITIONS))
	goto elaborated_type_specifier;

      /* Parse tentatively so that we can back up if we don't find a
	 class-specifier.  */
      cp_parser_parse_tentatively (parser);
      /* Look for the class-specifier.  */
      type_spec = cp_parser_class_specifier (parser);
      invoke_plugin_callbacks (PLUGIN_FINISH_TYPE, type_spec);
      /* If that worked, we're done.  */
      if (cp_parser_parse_definitely (parser))
	{
	  if (declares_class_or_enum)
	    *declares_class_or_enum = 2;
	  if (decl_specs)
	    cp_parser_set_decl_spec_type (decl_specs,
					  type_spec,
					  token->location,
					  /*user_defined_p=*/true);
	  return type_spec;
	}

      /* Fall through.  */
    elaborated_type_specifier:
      /* We're declaring (not defining) a class or enum.  */
      if (declares_class_or_enum)
	*declares_class_or_enum = 1;

      /* Fall through.  */
    case RID_TYPENAME:
      /* Look for an elaborated-type-specifier.  */
      type_spec
	= (cp_parser_elaborated_type_specifier
	   (parser,
	    decl_specs && decl_specs->specs[(int) ds_friend],
	    is_declaration));
      if (decl_specs)
	cp_parser_set_decl_spec_type (decl_specs,
				      type_spec,
				      token->location,
				      /*user_defined_p=*/true);
      return type_spec;

    case RID_CONST:
      ds = ds_const;
      if (is_cv_qualifier)
	*is_cv_qualifier = true;
      break;

    case RID_VOLATILE:
      ds = ds_volatile;
      if (is_cv_qualifier)
	*is_cv_qualifier = true;
      break;

    case RID_RESTRICT:
      ds = ds_restrict;
      if (is_cv_qualifier)
	*is_cv_qualifier = true;
      break;

    case RID_COMPLEX:
      /* The `__complex__' keyword is a GNU extension.  */
      ds = ds_complex;
      break;

    default:
      break;
    }

  /* Handle simple keywords.  */
  if (ds != ds_last)
    {
      if (decl_specs)
	{
	  ++decl_specs->specs[(int)ds];
	  decl_specs->any_specifiers_p = true;
	}
      return cp_lexer_consume_token (parser->lexer)->u.value;
    }

  /* If we do not already have a type-specifier, assume we are looking
     at a simple-type-specifier.  */
  type_spec = cp_parser_simple_type_specifier (parser,
					       decl_specs,
					       flags);

  /* If we didn't find a type-specifier, and a type-specifier was not
     optional in this context, issue an error message.  */
  if (!type_spec && !(flags & CP_PARSER_FLAGS_OPTIONAL))
    {
      cp_parser_error (parser, "expected type specifier");
      return error_mark_node;
    }

  return type_spec;
}

/* Parse a simple-type-specifier.

   simple-type-specifier:
     :: [opt] nested-name-specifier [opt] type-name
     :: [opt] nested-name-specifier template template-id
     char
     wchar_t
     bool
     short
     int
     long
     signed
     unsigned
     float
     double
     void

   C++0x Extension:

   simple-type-specifier:
     auto
     decltype ( expression )   
     char16_t
     char32_t

   GNU Extension:

   simple-type-specifier:
     __typeof__ unary-expression
     __typeof__ ( type-id )

   Returns the indicated TYPE_DECL.  If DECL_SPECS is not NULL, it is
   appropriately updated.  */

static tree
cp_parser_simple_type_specifier (cp_parser* parser,
				 cp_decl_specifier_seq *decl_specs,
				 cp_parser_flags flags)
{
  tree type = NULL_TREE;
  cp_token *token;

  /* Peek at the next token.  */
  token = cp_lexer_peek_token (parser->lexer);

  /* If we're looking at a keyword, things are easy.  */
  switch (token->keyword)
    {
    case RID_CHAR:
      if (decl_specs)
	decl_specs->explicit_char_p = true;
      type = char_type_node;
      break;
    case RID_CHAR16:
      type = char16_type_node;
      break;
    case RID_CHAR32:
      type = char32_type_node;
      break;
    case RID_WCHAR:
      type = wchar_type_node;
      break;
    case RID_BOOL:
      type = boolean_type_node;
      break;
    case RID_SHORT:
      if (decl_specs)
	++decl_specs->specs[(int) ds_short];
      type = short_integer_type_node;
      break;
    case RID_INT:
      if (decl_specs)
	decl_specs->explicit_int_p = true;
      type = integer_type_node;
      break;
    case RID_LONG:
      if (decl_specs)
	++decl_specs->specs[(int) ds_long];
      type = long_integer_type_node;
      break;
    case RID_SIGNED:
      if (decl_specs)
	++decl_specs->specs[(int) ds_signed];
      type = integer_type_node;
      break;
    case RID_UNSIGNED:
      if (decl_specs)
	++decl_specs->specs[(int) ds_unsigned];
      type = unsigned_type_node;
      break;
    case RID_FLOAT:
      type = float_type_node;
      break;
    case RID_DOUBLE:
      type = double_type_node;
      break;
    case RID_VOID:
      type = void_type_node;
      break;
      
    case RID_AUTO:
      maybe_warn_cpp0x (CPP0X_AUTO);
      type = make_auto ();
      break;

    case RID_DECLTYPE:
      /* Parse the `decltype' type.  */
      type = cp_parser_decltype (parser);

      if (decl_specs)
	cp_parser_set_decl_spec_type (decl_specs, type,
				      token->location,
				      /*user_defined_p=*/true);

      return type;

    case RID_TYPEOF:
      /* Consume the `typeof' token.  */
      cp_lexer_consume_token (parser->lexer);
      /* Parse the operand to `typeof'.  */
      type = cp_parser_sizeof_operand (parser, RID_TYPEOF);
      /* If it is not already a TYPE, take its type.  */
      if (!TYPE_P (type))
	type = finish_typeof (type);

      if (decl_specs)
	cp_parser_set_decl_spec_type (decl_specs, type,
				      token->location,
				      /*user_defined_p=*/true);

      return type;

    default:
      break;
    }

  /* If the type-specifier was for a built-in type, we're done.  */
  if (type)
    {
      /* Record the type.  */
      if (decl_specs
	  && (token->keyword != RID_SIGNED
	      && token->keyword != RID_UNSIGNED
	      && token->keyword != RID_SHORT
	      && token->keyword != RID_LONG))
	cp_parser_set_decl_spec_type (decl_specs,
				      type,
				      token->location,
				      /*user_defined=*/false);
      if (decl_specs)
	decl_specs->any_specifiers_p = true;

      /* Consume the token.  */
      cp_lexer_consume_token (parser->lexer);

      /* There is no valid C++ program where a non-template type is
	 followed by a "<".  That usually indicates that the user thought
	 that the type was a template.  */
      cp_parser_check_for_invalid_template_id (parser, type, token->location);

      return TYPE_NAME (type);
    }

  /* The type-specifier must be a user-defined type.  */
  if (!(flags & CP_PARSER_FLAGS_NO_USER_DEFINED_TYPES))
    {
      bool qualified_p;
      bool global_p;

      /* Don't gobble tokens or issue error messages if this is an
	 optional type-specifier.  */
      if (flags & CP_PARSER_FLAGS_OPTIONAL)
	cp_parser_parse_tentatively (parser);

      /* Look for the optional `::' operator.  */
      global_p
	= (cp_parser_global_scope_opt (parser,
				       /*current_scope_valid_p=*/false)
	   != NULL_TREE);
      /* Look for the nested-name specifier.  */
      qualified_p
	= (cp_parser_nested_name_specifier_opt (parser,
						/*typename_keyword_p=*/false,
						/*check_dependency_p=*/true,
						/*type_p=*/false,
						/*is_declaration=*/false)
	   != NULL_TREE);
      token = cp_lexer_peek_token (parser->lexer);
      /* If we have seen a nested-name-specifier, and the next token
	 is `template', then we are using the template-id production.  */
      if (parser->scope
	  && cp_parser_optional_template_keyword (parser))
	{
	  /* Look for the template-id.  */
	  type = cp_parser_template_id (parser,
					/*template_keyword_p=*/true,
					/*check_dependency_p=*/true,
					/*is_declaration=*/false);
	  /* If the template-id did not name a type, we are out of
	     luck.  */
	  if (TREE_CODE (type) != TYPE_DECL)
	    {
	      cp_parser_error (parser, "expected template-id for type");
	      type = NULL_TREE;
	    }
	}
      /* Otherwise, look for a type-name.  */
      else
	type = cp_parser_type_name (parser);
      /* Keep track of all name-lookups performed in class scopes.  */
      if (type
	  && !global_p
	  && !qualified_p
	  && TREE_CODE (type) == TYPE_DECL
	  && TREE_CODE (DECL_NAME (type)) == IDENTIFIER_NODE)
	maybe_note_name_used_in_class (DECL_NAME (type), type);
      /* If it didn't work out, we don't have a TYPE.  */
      if ((flags & CP_PARSER_FLAGS_OPTIONAL)
	  && !cp_parser_parse_definitely (parser))
	type = NULL_TREE;
      if (type && decl_specs)
	cp_parser_set_decl_spec_type (decl_specs, type,
				      token->location,
				      /*user_defined=*/true);
    }

  /* If we didn't get a type-name, issue an error message.  */
  if (!type && !(flags & CP_PARSER_FLAGS_OPTIONAL))
    {
      cp_parser_error (parser, "expected type-name");
      return error_mark_node;
    }

  /* There is no valid C++ program where a non-template type is
     followed by a "<".  That usually indicates that the user thought
     that the type was a template.  */
  if (type && type != error_mark_node)
    {
      /* As a last-ditch effort, see if TYPE is an Objective-C type.
	 If it is, then the '<'...'>' enclose protocol names rather than
	 template arguments, and so everything is fine.  */
      if (c_dialect_objc ()
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

      cp_parser_check_for_invalid_template_id (parser, TREE_TYPE (type),
					       token->location);
    }

  return type;
}

/* Parse a type-name.

   type-name:
     class-name
     enum-name
     typedef-name

   enum-name:
     identifier

   typedef-name:
     identifier

   Returns a TYPE_DECL for the type.  */

static tree
cp_parser_type_name (cp_parser* parser)
{
  tree type_decl;

  /* We can't know yet whether it is a class-name or not.  */
  cp_parser_parse_tentatively (parser);
  /* Try a class-name.  */
  type_decl = cp_parser_class_name (parser,
				    /*typename_keyword_p=*/false,
				    /*template_keyword_p=*/false,
				    none_type,
				    /*check_dependency_p=*/true,
				    /*class_head_p=*/false,
				    /*is_declaration=*/false);
  /* If it's not a class-name, keep looking.  */
  if (!cp_parser_parse_definitely (parser))
    {
      /* It must be a typedef-name or an enum-name.  */
      return cp_parser_nonclass_name (parser);
    }

  return type_decl;
}

/* Parse a non-class type-name, that is, either an enum-name or a typedef-name.

   enum-name:
     identifier

   typedef-name:
     identifier

   Returns a TYPE_DECL for the type.  */

static tree
cp_parser_nonclass_name (cp_parser* parser)
{
  tree type_decl;
  tree identifier;

  cp_token *token = cp_lexer_peek_token (parser->lexer);
  identifier = cp_parser_identifier (parser);
  if (identifier == error_mark_node)
    return error_mark_node;

  /* Look up the type-name.  */
  type_decl = cp_parser_lookup_name_simple (parser, identifier, token->location);

  if (TREE_CODE (type_decl) != TYPE_DECL
      && (objc_is_id (identifier) || objc_is_class_name (identifier)))
    {
      /* See if this is an Objective-C type.  */
      tree protos = cp_parser_objc_protocol_refs_opt (parser);
      tree type = objc_get_protocol_qualified_type (identifier, protos);
      if (type)
	type_decl = TYPE_NAME (type);
    }
  
  /* Issue an error if we did not find a type-name.  */
  if (TREE_CODE (type_decl) != TYPE_DECL)
    {
      if (!cp_parser_simulate_error (parser))
	cp_parser_name_lookup_error (parser, identifier, type_decl,
				     "is not a type", token->location);
      return error_mark_node;
    }
  /* Remember that the name was used in the definition of the
     current class so that we can check later to see if the
     meaning would have been different after the class was
     entirely defined.  */
  else if (type_decl != error_mark_node
	   && !parser->scope)
    maybe_note_name_used_in_class (identifier, type_decl);
  
  return type_decl;
}

/* Parse an elaborated-type-specifier.  Note that the grammar given
   here incorporates the resolution to DR68.

   elaborated-type-specifier:
     class-key :: [opt] nested-name-specifier [opt] identifier
     class-key :: [opt] nested-name-specifier [opt] template [opt] template-id
     enum-key :: [opt] nested-name-specifier [opt] identifier
     typename :: [opt] nested-name-specifier identifier
     typename :: [opt] nested-name-specifier template [opt]
       template-id

   GNU extension:

   elaborated-type-specifier:
     class-key attributes :: [opt] nested-name-specifier [opt] identifier
     class-key attributes :: [opt] nested-name-specifier [opt]
	       template [opt] template-id
     enum attributes :: [opt] nested-name-specifier [opt] identifier

   If IS_FRIEND is TRUE, then this elaborated-type-specifier is being
   declared `friend'.  If IS_DECLARATION is TRUE, then this
   elaborated-type-specifier appears in a decl-specifiers-seq, i.e.,
   something is being declared.

   Returns the TYPE specified.  */

static tree
cp_parser_elaborated_type_specifier (cp_parser* parser,
				     bool is_friend,
				     bool is_declaration)
{
  enum tag_types tag_type;
  tree identifier;
  tree type = NULL_TREE;
  tree attributes = NULL_TREE;
  tree globalscope;
  cp_token *token = NULL;

  /* See if we're looking at the `enum' keyword.  */
  if (cp_lexer_next_token_is_keyword (parser->lexer, RID_ENUM))
    {
      /* Consume the `enum' token.  */
      cp_lexer_consume_token (parser->lexer);
      /* Remember that it's an enumeration type.  */
      tag_type = enum_type;
      /* Parse the optional `struct' or `class' key (for C++0x scoped
         enums).  */
      if (cp_lexer_next_token_is_keyword (parser->lexer, RID_CLASS)
          || cp_lexer_next_token_is_keyword (parser->lexer, RID_STRUCT))
        {
          if (cxx_dialect == cxx98)
            maybe_warn_cpp0x (CPP0X_SCOPED_ENUMS);

          /* Consume the `struct' or `class'.  */
          cp_lexer_consume_token (parser->lexer);
        }
      /* Parse the attributes.  */
      attributes = cp_parser_attributes_opt (parser);
    }
  /* Or, it might be `typename'.  */
  else if (cp_lexer_next_token_is_keyword (parser->lexer,
					   RID_TYPENAME))
    {
      /* Consume the `typename' token.  */
      cp_lexer_consume_token (parser->lexer);
      /* Remember that it's a `typename' type.  */
      tag_type = typename_type;
    }
  /* Otherwise it must be a class-key.  */
  else
    {
      tag_type = cp_parser_class_key (parser);
      if (tag_type == none_type)
	return error_mark_node;
      /* Parse the attributes.  */
      attributes = cp_parser_attributes_opt (parser);
    }

  /* Look for the `::' operator.  */
  globalscope =  cp_parser_global_scope_opt (parser,
					     /*current_scope_valid_p=*/false);
  /* Look for the nested-name-specifier.  */
  if (tag_type == typename_type && !globalscope)
    {
      if (!cp_parser_nested_name_specifier (parser,
					   /*typename_keyword_p=*/true,
					   /*check_dependency_p=*/true,
					   /*type_p=*/true,
					    is_declaration))
	return error_mark_node;
    }
  else
    /* Even though `typename' is not present, the proposed resolution
       to Core Issue 180 says that in `class A<T>::B', `B' should be
       considered a type-name, even if `A<T>' is dependent.  */
    cp_parser_nested_name_specifier_opt (parser,
					 /*typename_keyword_p=*/true,
					 /*check_dependency_p=*/true,
					 /*type_p=*/true,
					 is_declaration);
 /* For everything but enumeration types, consider a template-id.
    For an enumeration type, consider only a plain identifier.  */
  if (tag_type != enum_type)
    {
      bool template_p = false;
      tree decl;

      /* Allow the `template' keyword.  */
      template_p = cp_parser_optional_template_keyword (parser);
      /* If we didn't see `template', we don't know if there's a
	 template-id or not.  */
      if (!template_p)
	cp_parser_parse_tentatively (parser);
      /* Parse the template-id.  */
      token = cp_lexer_peek_token (parser->lexer);
      decl = cp_parser_template_id (parser, template_p,
				    /*check_dependency_p=*/true,
				    is_declaration);
      /* If we didn't find a template-id, look for an ordinary
	 identifier.  */
      if (!template_p && !cp_parser_parse_definitely (parser))
	;
      /* If DECL is a TEMPLATE_ID_EXPR, and the `typename' keyword is
	 in effect, then we must assume that, upon instantiation, the
	 template will correspond to a class.  */
      else if (TREE_CODE (decl) == TEMPLATE_ID_EXPR
	       && tag_type == typename_type)
	type = make_typename_type (parser->scope, decl,
				   typename_type,
				   /*complain=*/tf_error);
      /* If the `typename' keyword is in effect and DECL is not a type
	 decl. Then type is non existant.   */
      else if (tag_type == typename_type && TREE_CODE (decl) != TYPE_DECL)
        type = NULL_TREE; 
      else 
	type = TREE_TYPE (decl);
    }

  if (!type)
    {
      token = cp_lexer_peek_token (parser->lexer);
      identifier = cp_parser_identifier (parser);

      if (identifier == error_mark_node)
	{
	  parser->scope = NULL_TREE;
	  return error_mark_node;
	}

      /* For a `typename', we needn't call xref_tag.  */
      if (tag_type == typename_type
	  && TREE_CODE (parser->scope) != NAMESPACE_DECL)
	return cp_parser_make_typename_type (parser, parser->scope,
					     identifier,
					     token->location);
      /* Look up a qualified name in the usual way.  */
      if (parser->scope)
	{
	  tree decl;
	  tree ambiguous_decls;

	  decl = cp_parser_lookup_name (parser, identifier,
					tag_type,
					/*is_template=*/false,
					/*is_namespace=*/false,
					/*check_dependency=*/true,
					&ambiguous_decls,
					token->location);

	  /* If the lookup was ambiguous, an error will already have been
	     issued.  */
	  if (ambiguous_decls)
	    return error_mark_node;

	  /* If we are parsing friend declaration, DECL may be a
	     TEMPLATE_DECL tree node here.  However, we need to check
	     whether this TEMPLATE_DECL results in valid code.  Consider
	     the following example:

	       namespace N {
		 template <class T> class C {};
	       }
	       class X {
		 template <class T> friend class N::C; // #1, valid code
	       };
	       template <class T> class Y {
		 friend class N::C;		       // #2, invalid code
	       };

	     For both case #1 and #2, we arrive at a TEMPLATE_DECL after
	     name lookup of `N::C'.  We see that friend declaration must
	     be template for the code to be valid.  Note that
	     processing_template_decl does not work here since it is
	     always 1 for the above two cases.  */

	  decl = (cp_parser_maybe_treat_template_as_class
		  (decl, /*tag_name_p=*/is_friend
			 && parser->num_template_parameter_lists));

	  if (TREE_CODE (decl) != TYPE_DECL)
	    {
	      cp_parser_diagnose_invalid_type_name (parser,
						    parser->scope,
						    identifier,
						    token->location);
	      return error_mark_node;
	    }

	  if (TREE_CODE (TREE_TYPE (decl)) != TYPENAME_TYPE)
            {
              bool allow_template = (parser->num_template_parameter_lists
		                      || DECL_SELF_REFERENCE_P (decl));
              type = check_elaborated_type_specifier (tag_type, decl, 
                                                      allow_template);

              if (type == error_mark_node)
                return error_mark_node;
            }

          /* Forward declarations of nested types, such as

               class C1::C2;
               class C1::C2::C3;

             are invalid unless all components preceding the final '::'
             are complete.  If all enclosing types are complete, these
             declarations become merely pointless.

             Invalid forward declarations of nested types are errors
             caught elsewhere in parsing.  Those that are pointless arrive
             here.  */

          if (cp_lexer_next_token_is (parser->lexer, CPP_SEMICOLON)
              && !is_friend && !processing_explicit_instantiation)
            warning (0, "declaration %qD does not declare anything", decl);

	  type = TREE_TYPE (decl);
	}
      else
	{
	  /* An elaborated-type-specifier sometimes introduces a new type and
	     sometimes names an existing type.  Normally, the rule is that it
	     introduces a new type only if there is not an existing type of
	     the same name already in scope.  For example, given:

	       struct S {};
	       void f() { struct S s; }

	     the `struct S' in the body of `f' is the same `struct S' as in
	     the global scope; the existing definition is used.  However, if
	     there were no global declaration, this would introduce a new
	     local class named `S'.

	     An exception to this rule applies to the following code:

	       namespace N { struct S; }

	     Here, the elaborated-type-specifier names a new type
	     unconditionally; even if there is already an `S' in the
	     containing scope this declaration names a new type.
	     This exception only applies if the elaborated-type-specifier
	     forms the complete declaration:

	       [class.name]

	       A declaration consisting solely of `class-key identifier ;' is
	       either a redeclaration of the name in the current scope or a
	       forward declaration of the identifier as a class name.  It
	       introduces the name into the current scope.

	     We are in this situation precisely when the next token is a `;'.

	     An exception to the exception is that a `friend' declaration does
	     *not* name a new type; i.e., given:

	       struct S { friend struct T; };

	     `T' is not a new type in the scope of `S'.

	     Also, `new struct S' or `sizeof (struct S)' never results in the
	     definition of a new type; a new type can only be declared in a
	     declaration context.  */

	  tag_scope ts;
	  bool template_p;

	  if (is_friend)
	    /* Friends have special name lookup rules.  */
	    ts = ts_within_enclosing_non_class;
	  else if (is_declaration
		   && cp_lexer_next_token_is (parser->lexer,
					      CPP_SEMICOLON))
	    /* This is a `class-key identifier ;' */
	    ts = ts_current;
	  else
	    ts = ts_global;

	  template_p =
	    (parser->num_template_parameter_lists
	     && (cp_parser_next_token_starts_class_definition_p (parser)
		 || cp_lexer_next_token_is (parser->lexer, CPP_SEMICOLON)));
	  /* An unqualified name was used to reference this type, so
	     there were no qualifying templates.  */
	  if (!cp_parser_check_template_parameters (parser,
						    /*num_templates=*/0,
						    token->location,
						    /*declarator=*/NULL))
	    return error_mark_node;
	  type = xref_tag (tag_type, identifier, ts, template_p);
	}
    }

  if (type == error_mark_node)
    return error_mark_node;

  /* Allow attributes on forward declarations of classes.  */
  if (attributes)
    {
      if (TREE_CODE (type) == TYPENAME_TYPE)
	warning (OPT_Wattributes,
		 "attributes ignored on uninstantiated type");
      else if (tag_type != enum_type && CLASSTYPE_TEMPLATE_INSTANTIATION (type)
	       && ! processing_explicit_instantiation)
	warning (OPT_Wattributes,
		 "attributes ignored on template instantiation");
      else if (is_declaration && cp_parser_declares_only_class_p (parser))
	cplus_decl_attributes (&type, attributes, (int) ATTR_FLAG_TYPE_IN_PLACE);
      else
	warning (OPT_Wattributes,
		 "attributes ignored on elaborated-type-specifier that is not a forward declaration");
    }

  if (tag_type != enum_type)
    cp_parser_check_class_key (tag_type, type);

  /* A "<" cannot follow an elaborated type specifier.  If that
     happens, the user was probably trying to form a template-id.  */
  cp_parser_check_for_invalid_template_id (parser, type, token->location);

  return type;
}

/* Parse an enum-specifier.

   enum-specifier:
     enum-key identifier [opt] enum-base [opt] { enumerator-list [opt] }

   enum-key:
     enum
     enum class   [C++0x]
     enum struct  [C++0x]

   enum-base:   [C++0x]
     : type-specifier-seq

   GNU Extensions:
     enum-key attributes[opt] identifier [opt] enum-base [opt] 
       { enumerator-list [opt] }attributes[opt]

   Returns an ENUM_TYPE representing the enumeration, or NULL_TREE
   if the token stream isn't an enum-specifier after all.  */

static tree
cp_parser_enum_specifier (cp_parser* parser)
{
  tree identifier;
  tree type;
  tree attributes;
  bool scoped_enum_p = false;
  bool has_underlying_type = false;
  tree underlying_type = NULL_TREE;

  /* Parse tentatively so that we can back up if we don't find a
     enum-specifier.  */
  cp_parser_parse_tentatively (parser);

  /* Caller guarantees that the current token is 'enum', an identifier
     possibly follows, and the token after that is an opening brace.
     If we don't have an identifier, fabricate an anonymous name for
     the enumeration being defined.  */
  cp_lexer_consume_token (parser->lexer);

  /* Parse the "class" or "struct", which indicates a scoped
     enumeration type in C++0x.  */
  if (cp_lexer_next_token_is_keyword (parser->lexer, RID_CLASS)
      || cp_lexer_next_token_is_keyword (parser->lexer, RID_STRUCT))
    {
      if (cxx_dialect == cxx98)
        maybe_warn_cpp0x (CPP0X_SCOPED_ENUMS);

      /* Consume the `struct' or `class' token.  */
      cp_lexer_consume_token (parser->lexer);

      scoped_enum_p = true;
    }

  attributes = cp_parser_attributes_opt (parser);

  if (cp_lexer_next_token_is (parser->lexer, CPP_NAME))
    identifier = cp_parser_identifier (parser);
  else
    identifier = make_anon_name ();

  /* Check for the `:' that denotes a specified underlying type in C++0x.
     Note that a ':' could also indicate a bitfield width, however.  */
  if (cp_lexer_next_token_is (parser->lexer, CPP_COLON))
    {
      cp_decl_specifier_seq type_specifiers;

      /* Consume the `:'.  */
      cp_lexer_consume_token (parser->lexer);

      /* Parse the type-specifier-seq.  */
      cp_parser_type_specifier_seq (parser, /*is_declaration=*/false,
				    /*is_trailing_return=*/false,
                                    &type_specifiers);

      /* At this point this is surely not elaborated type specifier.  */
      if (!cp_parser_parse_definitely (parser))
	return NULL_TREE;

      if (cxx_dialect == cxx98)
        maybe_warn_cpp0x (CPP0X_SCOPED_ENUMS);

      has_underlying_type = true;

      /* If that didn't work, stop.  */
      if (type_specifiers.type != error_mark_node)
        {
          underlying_type = grokdeclarator (NULL, &type_specifiers, TYPENAME,
                                            /*initialized=*/0, NULL);
          if (underlying_type == error_mark_node)
            underlying_type = NULL_TREE;
        }
    }

  /* Look for the `{' but don't consume it yet.  */
  if (!cp_lexer_next_token_is (parser->lexer, CPP_OPEN_BRACE))
    {
      cp_parser_error (parser, "expected %<{%>");
      if (has_underlying_type)
	return NULL_TREE;
    }

  if (!has_underlying_type && !cp_parser_parse_definitely (parser))
    return NULL_TREE;

  /* Issue an error message if type-definitions are forbidden here.  */
  if (!cp_parser_check_type_definition (parser))
    type = error_mark_node;
  else
    /* Create the new type.  We do this before consuming the opening
       brace so the enum will be recorded as being on the line of its
       tag (or the 'enum' keyword, if there is no tag).  */
    type = start_enum (identifier, underlying_type, scoped_enum_p);
  
  /* Consume the opening brace.  */
  cp_lexer_consume_token (parser->lexer);

  if (type == error_mark_node)
    {
      cp_parser_skip_to_end_of_block_or_statement (parser);
      return error_mark_node;
    }

  /* If the next token is not '}', then there are some enumerators.  */
  if (cp_lexer_next_token_is_not (parser->lexer, CPP_CLOSE_BRACE))
    cp_parser_enumerator_list (parser, type);

  /* Consume the final '}'.  */
  cp_parser_require (parser, CPP_CLOSE_BRACE, "%<}%>");

  /* Look for trailing attributes to apply to this enumeration, and
     apply them if appropriate.  */
  if (cp_parser_allow_gnu_extensions_p (parser))
    {
      tree trailing_attr = cp_parser_attributes_opt (parser);
      trailing_attr = chainon (trailing_attr, attributes);
      cplus_decl_attributes (&type,
			     trailing_attr,
			     (int) ATTR_FLAG_TYPE_IN_PLACE);
    }

  /* Finish up the enumeration.  */
  finish_enum (type);

  return type;
}

/* Parse an enumerator-list.  The enumerators all have the indicated
   TYPE.

   enumerator-list:
     enumerator-definition
     enumerator-list , enumerator-definition  */

static void
cp_parser_enumerator_list (cp_parser* parser, tree type)
{
  while (true)
    {
      /* Parse an enumerator-definition.  */
      cp_parser_enumerator_definition (parser, type);

      /* If the next token is not a ',', we've reached the end of
	 the list.  */
      if (cp_lexer_next_token_is_not (parser->lexer, CPP_COMMA))
	break;
      /* Otherwise, consume the `,' and keep going.  */
      cp_lexer_consume_token (parser->lexer);
      /* If the next token is a `}', there is a trailing comma.  */
      if (cp_lexer_next_token_is (parser->lexer, CPP_CLOSE_BRACE))
	{
	  if (!in_system_header)
	    pedwarn (input_location, OPT_pedantic, "comma at end of enumerator list");
	  break;
	}
    }
}

/* Parse an enumerator-definition.  The enumerator has the indicated
   TYPE.

   enumerator-definition:
     enumerator
     enumerator = constant-expression

   enumerator:
     identifier  */

static void
cp_parser_enumerator_definition (cp_parser* parser, tree type)
{
  tree identifier;
  tree value;

  /* Look for the identifier.  */
  identifier = cp_parser_identifier (parser);
  if (identifier == error_mark_node)
    return;

  /* If the next token is an '=', then there is an explicit value.  */
  if (cp_lexer_next_token_is (parser->lexer, CPP_EQ))
    {
      /* Consume the `=' token.  */
      cp_lexer_consume_token (parser->lexer);
      /* Parse the value.  */
      value = cp_parser_constant_expression (parser,
					     /*allow_non_constant_p=*/false,
					     NULL);
    }
  else
    value = NULL_TREE;

  /* If we are processing a template, make sure the initializer of the
     enumerator doesn't contain any bare template parameter pack.  */
  if (check_for_bare_parameter_packs (value))
    value = error_mark_node;

  /* Create the enumerator.  */
  build_enumerator (identifier, value, type);
}

/* Parse a namespace-name.

   namespace-name:
     original-namespace-name
     namespace-alias

   Returns the NAMESPACE_DECL for the namespace.  */

static tree
cp_parser_namespace_name (cp_parser* parser)
{
  tree identifier;
  tree namespace_decl;

  cp_token *token = cp_lexer_peek_token (parser->lexer);

  /* Get the name of the namespace.  */
  identifier = cp_parser_identifier (parser);
  if (identifier == error_mark_node)
    return error_mark_node;

  /* Look up the identifier in the currently active scope.  Look only
     for namespaces, due to:

       [basic.lookup.udir]

       When looking up a namespace-name in a using-directive or alias
       definition, only namespace names are considered.

     And:

       [basic.lookup.qual]

       During the lookup of a name preceding the :: scope resolution
       operator, object, function, and enumerator names are ignored.

     (Note that cp_parser_qualifying_entity only calls this
     function if the token after the name is the scope resolution
     operator.)  */
  namespace_decl = cp_parser_lookup_name (parser, identifier,
					  none_type,
					  /*is_template=*/false,
					  /*is_namespace=*/true,
					  /*check_dependency=*/true,
					  /*ambiguous_decls=*/NULL,
					  token->location);
  /* If it's not a namespace, issue an error.  */
  if (namespace_decl == error_mark_node
      || TREE_CODE (namespace_decl) != NAMESPACE_DECL)
    {
      if (!cp_parser_uncommitted_to_tentative_parse_p (parser))
	error_at (token->location, "%qD is not a namespace-name", identifier);
      cp_parser_error (parser, "expected namespace-name");
      namespace_decl = error_mark_node;
    }

  return namespace_decl;
}

/* Parse a namespace-definition.

   namespace-definition:
     named-namespace-definition
     unnamed-namespace-definition

   named-namespace-definition:
     original-namespace-definition
     extension-namespace-definition

   original-namespace-definition:
     namespace identifier { namespace-body }

   extension-namespace-definition:
     namespace original-namespace-name { namespace-body }

   unnamed-namespace-definition:
     namespace { namespace-body } */

static void
cp_parser_namespace_definition (cp_parser* parser)
{
  tree identifier, attribs;
  bool has_visibility;
  bool is_inline;

  if (cp_lexer_next_token_is_keyword (parser->lexer, RID_INLINE))
    {
      is_inline = true;
      cp_lexer_consume_token (parser->lexer);
    }
  else
    is_inline = false;

  /* Look for the `namespace' keyword.  */
  cp_parser_require_keyword (parser, RID_NAMESPACE, "%<namespace%>");

  /* Get the name of the namespace.  We do not attempt to distinguish
     between an original-namespace-definition and an
     extension-namespace-definition at this point.  The semantic
     analysis routines are responsible for that.  */
  if (cp_lexer_next_token_is (parser->lexer, CPP_NAME))
    identifier = cp_parser_identifier (parser);
  else
    identifier = NULL_TREE;

  /* Parse any specified attributes.  */
  attribs = cp_parser_attributes_opt (parser);

  /* Look for the `{' to start the namespace.  */
  cp_parser_require (parser, CPP_OPEN_BRACE, "%<{%>");
  /* Start the namespace.  */
  push_namespace (identifier);

  /* "inline namespace" is equivalent to a stub namespace definition
     followed by a strong using directive.  */
  if (is_inline)
    {
      tree name_space = current_namespace;
      /* Set up namespace association.  */
      DECL_NAMESPACE_ASSOCIATIONS (name_space)
	= tree_cons (CP_DECL_CONTEXT (name_space), NULL_TREE,
		     DECL_NAMESPACE_ASSOCIATIONS (name_space));
      /* Import the contents of the inline namespace.  */
      pop_namespace ();
      do_using_directive (name_space);
      push_namespace (identifier);
    }

  has_visibility = handle_namespace_attrs (current_namespace, attribs);

  /* Parse the body of the namespace.  */
  cp_parser_namespace_body (parser);

#ifdef HANDLE_PRAGMA_VISIBILITY
  if (has_visibility)
    pop_visibility (1);
#endif

  /* Finish the namespace.  */
  pop_namespace ();
  /* Look for the final `}'.  */
  cp_parser_require (parser, CPP_CLOSE_BRACE, "%<}%>");
}

/* Parse a namespace-body.

   namespace-body:
     declaration-seq [opt]  */

static void
cp_parser_namespace_body (cp_parser* parser)
{
  cp_parser_declaration_seq_opt (parser);
}

/* Parse a namespace-alias-definition.

   namespace-alias-definition:
     namespace identifier = qualified-namespace-specifier ;  */

static void
cp_parser_namespace_alias_definition (cp_parser* parser)
{
  tree identifier;
  tree namespace_specifier;

  cp_token *token = cp_lexer_peek_token (parser->lexer);

  /* Look for the `namespace' keyword.  */
  cp_parser_require_keyword (parser, RID_NAMESPACE, "%<namespace%>");
  /* Look for the identifier.  */
  identifier = cp_parser_identifier (parser);
  if (identifier == error_mark_node)
    return;
  /* Look for the `=' token.  */
  if (!cp_parser_uncommitted_to_tentative_parse_p (parser)
      && cp_lexer_next_token_is (parser->lexer, CPP_OPEN_BRACE)) 
    {
      error_at (token->location, "%<namespace%> definition is not allowed here");
      /* Skip the definition.  */
      cp_lexer_consume_token (parser->lexer);
      if (cp_parser_skip_to_closing_brace (parser))
	cp_lexer_consume_token (parser->lexer);
      return;
    }
  cp_parser_require (parser, CPP_EQ, "%<=%>");
  /* Look for the qualified-namespace-specifier.  */
  namespace_specifier
    = cp_parser_qualified_namespace_specifier (parser);
  /* Look for the `;' token.  */
  cp_parser_require (parser, CPP_SEMICOLON, "%<;%>");

  /* Register the alias in the symbol table.  */
  do_namespace_alias (identifier, namespace_specifier);
}

/* Parse a qualified-namespace-specifier.

   qualified-namespace-specifier:
     :: [opt] nested-name-specifier [opt] namespace-name

   Returns a NAMESPACE_DECL corresponding to the specified
   namespace.  */

static tree
cp_parser_qualified_namespace_specifier (cp_parser* parser)
{
  /* Look for the optional `::'.  */
  cp_parser_global_scope_opt (parser,
			      /*current_scope_valid_p=*/false);

  /* Look for the optional nested-name-specifier.  */
  cp_parser_nested_name_specifier_opt (parser,
				       /*typename_keyword_p=*/false,
				       /*check_dependency_p=*/true,
				       /*type_p=*/false,
				       /*is_declaration=*/true);

  return cp_parser_namespace_name (parser);
}

/* Parse a using-declaration, or, if ACCESS_DECLARATION_P is true, an
   access declaration.

   using-declaration:
     using typename [opt] :: [opt] nested-name-specifier unqualified-id ;
     using :: unqualified-id ;  

   access-declaration:
     qualified-id ;  

   */

static bool
cp_parser_using_declaration (cp_parser* parser, 
			     bool access_declaration_p)
{
  cp_token *token;
  bool typename_p = false;
  bool global_scope_p;
  tree decl;
  tree identifier;
  tree qscope;

  if (access_declaration_p)
    cp_parser_parse_tentatively (parser);
  else
    {
      /* Look for the `using' keyword.  */
      cp_parser_require_keyword (parser, RID_USING, "%<using%>");
      
      /* Peek at the next token.  */
      token = cp_lexer_peek_token (parser->lexer);
      /* See if it's `typename'.  */
      if (token->keyword == RID_TYPENAME)
	{
	  /* Remember that we've seen it.  */
	  typename_p = true;
	  /* Consume the `typename' token.  */
	  cp_lexer_consume_token (parser->lexer);
	}
    }

  /* Look for the optional global scope qualification.  */
  global_scope_p
    = (cp_parser_global_scope_opt (parser,
				   /*current_scope_valid_p=*/false)
       != NULL_TREE);

  /* If we saw `typename', or didn't see `::', then there must be a
     nested-name-specifier present.  */
  if (typename_p || !global_scope_p)
    qscope = cp_parser_nested_name_specifier (parser, typename_p,
					      /*check_dependency_p=*/true,
					      /*type_p=*/false,
					      /*is_declaration=*/true);
  /* Otherwise, we could be in either of the two productions.  In that
     case, treat the nested-name-specifier as optional.  */
  else
    qscope = cp_parser_nested_name_specifier_opt (parser,
						  /*typename_keyword_p=*/false,
						  /*check_dependency_p=*/true,
						  /*type_p=*/false,
						  /*is_declaration=*/true);
  if (!qscope)
    qscope = global_namespace;

  if (access_declaration_p && cp_parser_error_occurred (parser))
    /* Something has already gone wrong; there's no need to parse
       further.  Since an error has occurred, the return value of
       cp_parser_parse_definitely will be false, as required.  */
    return cp_parser_parse_definitely (parser);

  token = cp_lexer_peek_token (parser->lexer);
  /* Parse the unqualified-id.  */
  identifier = cp_parser_unqualified_id (parser,
					 /*template_keyword_p=*/false,
					 /*check_dependency_p=*/true,
					 /*declarator_p=*/true,
					 /*optional_p=*/false);

  if (access_declaration_p)
    {
      if (cp_lexer_next_token_is_not (parser->lexer, CPP_SEMICOLON))
	cp_parser_simulate_error (parser);
      if (!cp_parser_parse_definitely (parser))
	return false;
    }

  /* The function we call to handle a using-declaration is different
     depending on what scope we are in.  */
  if (qscope == error_mark_node || identifier == error_mark_node)
    ;
  else if (TREE_CODE (identifier) != IDENTIFIER_NODE
	   && TREE_CODE (identifier) != BIT_NOT_EXPR)
    /* [namespace.udecl]

       A using declaration shall not name a template-id.  */
    error_at (token->location,
	      "a template-id may not appear in a using-declaration");
  else
    {
      if (at_class_scope_p ())
	{
	  /* Create the USING_DECL.  */
	  decl = do_class_using_decl (parser->scope, identifier);

	  if (check_for_bare_parameter_packs (decl))
            return false;
          else
	    /* Add it to the list of members in this class.  */
	    finish_member_declaration (decl);
	}
      else
	{
	  decl = cp_parser_lookup_name_simple (parser,
					       identifier,
					       token->location);
	  if (decl == error_mark_node)
	    cp_parser_name_lookup_error (parser, identifier,
					 decl, NULL,
					 token->location);
	  else if (check_for_bare_parameter_packs (decl))
	    return false;
	  else if (!at_namespace_scope_p ())
	    do_local_using_decl (decl, qscope, identifier);
	  else
	    do_toplevel_using_decl (decl, qscope, identifier);
	}
    }

  /* Look for the final `;'.  */
  cp_parser_require (parser, CPP_SEMICOLON, "%<;%>");
  
  return true;
}

/* Parse a using-directive.

   using-directive:
     using namespace :: [opt] nested-name-specifier [opt]
       namespace-name ;  */

static void
cp_parser_using_directive (cp_parser* parser)
{
  tree namespace_decl;
  tree attribs;

  /* Look for the `using' keyword.  */
  cp_parser_require_keyword (parser, RID_USING, "%<using%>");
  /* And the `namespace' keyword.  */
  cp_parser_require_keyword (parser, RID_NAMESPACE, "%<namespace%>");
  /* Look for the optional `::' operator.  */
  cp_parser_global_scope_opt (parser, /*current_scope_valid_p=*/false);
  /* And the optional nested-name-specifier.  */
  cp_parser_nested_name_specifier_opt (parser,
				       /*typename_keyword_p=*/false,
				       /*check_dependency_p=*/true,
				       /*type_p=*/false,
				       /*is_declaration=*/true);
  /* Get the namespace being used.  */
  namespace_decl = cp_parser_namespace_name (parser);
  /* And any specified attributes.  */
  attribs = cp_parser_attributes_opt (parser);
  /* Update the symbol table.  */
  parse_using_directive (namespace_decl, attribs);
  /* Look for the final `;'.  */
  cp_parser_require (parser, CPP_SEMICOLON, "%<;%>");
}

/* Parse an asm-definition.

   asm-definition:
     asm ( string-literal ) ;

   GNU Extension:

   asm-definition:
     asm volatile [opt] ( string-literal ) ;
     asm volatile [opt] ( string-literal : asm-operand-list [opt] ) ;
     asm volatile [opt] ( string-literal : asm-operand-list [opt]
			  : asm-operand-list [opt] ) ;
     asm volatile [opt] ( string-literal : asm-operand-list [opt]
			  : asm-operand-list [opt]
			  : asm-clobber-list [opt] ) ;
     asm volatile [opt] goto ( string-literal : : asm-operand-list [opt]
			       : asm-clobber-list [opt]
			       : asm-goto-list ) ;  */

static void
cp_parser_asm_definition (cp_parser* parser)
{
  tree string;
  tree outputs = NULL_TREE;
  tree inputs = NULL_TREE;
  tree clobbers = NULL_TREE;
  tree labels = NULL_TREE;
  tree asm_stmt;
  bool volatile_p = false;
  bool extended_p = false;
  bool invalid_inputs_p = false;
  bool invalid_outputs_p = false;
  bool goto_p = false;
  const char *missing = NULL;

  /* Look for the `asm' keyword.  */
  cp_parser_require_keyword (parser, RID_ASM, "%<asm%>");
  /* See if the next token is `volatile'.  */
  if (cp_parser_allow_gnu_extensions_p (parser)
      && cp_lexer_next_token_is_keyword (parser->lexer, RID_VOLATILE))
    {
      /* Remember that we saw the `volatile' keyword.  */
      volatile_p = true;
      /* Consume the token.  */
      cp_lexer_consume_token (parser->lexer);
    }
  if (cp_parser_allow_gnu_extensions_p (parser)
      && parser->in_function_body
      && cp_lexer_next_token_is_keyword (parser->lexer, RID_GOTO))
    {
      /* Remember that we saw the `goto' keyword.  */
      goto_p = true;
      /* Consume the token.  */
      cp_lexer_consume_token (parser->lexer);
    }
  /* Look for the opening `('.  */
  if (!cp_parser_require (parser, CPP_OPEN_PAREN, "%<(%>"))
    return;
  /* Look for the string.  */
  string = cp_parser_string_literal (parser, false, false);
  if (string == error_mark_node)
    {
      cp_parser_skip_to_closing_parenthesis (parser, true, false,
					     /*consume_paren=*/true);
      return;
    }

  /* If we're allowing GNU extensions, check for the extended assembly
     syntax.  Unfortunately, the `:' tokens need not be separated by
     a space in C, and so, for compatibility, we tolerate that here
     too.  Doing that means that we have to treat the `::' operator as
     two `:' tokens.  */
  if (cp_parser_allow_gnu_extensions_p (parser)
      && parser->in_function_body
      && (cp_lexer_next_token_is (parser->lexer, CPP_COLON)
	  || cp_lexer_next_token_is (parser->lexer, CPP_SCOPE)))
    {
      bool inputs_p = false;
      bool clobbers_p = false;
      bool labels_p = false;

      /* The extended syntax was used.  */
      extended_p = true;

      /* Look for outputs.  */
      if (cp_lexer_next_token_is (parser->lexer, CPP_COLON))
	{
	  /* Consume the `:'.  */
	  cp_lexer_consume_token (parser->lexer);
	  /* Parse the output-operands.  */
	  if (cp_lexer_next_token_is_not (parser->lexer,
					  CPP_COLON)
	      && cp_lexer_next_token_is_not (parser->lexer,
					     CPP_SCOPE)
	      && cp_lexer_next_token_is_not (parser->lexer,
					     CPP_CLOSE_PAREN)
	      && !goto_p)
	    outputs = cp_parser_asm_operand_list (parser);

	    if (outputs == error_mark_node)
	      invalid_outputs_p = true;
	}
      /* If the next token is `::', there are no outputs, and the
	 next token is the beginning of the inputs.  */
      else if (cp_lexer_next_token_is (parser->lexer, CPP_SCOPE))
	/* The inputs are coming next.  */
	inputs_p = true;

      /* Look for inputs.  */
      if (inputs_p
	  || cp_lexer_next_token_is (parser->lexer, CPP_COLON))
	{
	  /* Consume the `:' or `::'.  */
	  cp_lexer_consume_token (parser->lexer);
	  /* Parse the output-operands.  */
	  if (cp_lexer_next_token_is_not (parser->lexer,
					  CPP_COLON)
	      && cp_lexer_next_token_is_not (parser->lexer,
					     CPP_SCOPE)
	      && cp_lexer_next_token_is_not (parser->lexer,
					     CPP_CLOSE_PAREN))
	    inputs = cp_parser_asm_operand_list (parser);

	    if (inputs == error_mark_node)
	      invalid_inputs_p = true;
	}
      else if (cp_lexer_next_token_is (parser->lexer, CPP_SCOPE))
	/* The clobbers are coming next.  */
	clobbers_p = true;

      /* Look for clobbers.  */
      if (clobbers_p
	  || cp_lexer_next_token_is (parser->lexer, CPP_COLON))
	{
	  clobbers_p = true;
	  /* Consume the `:' or `::'.  */
	  cp_lexer_consume_token (parser->lexer);
	  /* Parse the clobbers.  */
	  if (cp_lexer_next_token_is_not (parser->lexer,
					  CPP_COLON)
	      && cp_lexer_next_token_is_not (parser->lexer,
					     CPP_CLOSE_PAREN))
	    clobbers = cp_parser_asm_clobber_list (parser);
	}
      else if (goto_p
	       && cp_lexer_next_token_is (parser->lexer, CPP_SCOPE))
	/* The labels are coming next.  */
	labels_p = true;

      /* Look for labels.  */
      if (labels_p
	  || (goto_p && cp_lexer_next_token_is (parser->lexer, CPP_COLON)))
	{
	  labels_p = true;
	  /* Consume the `:' or `::'.  */
	  cp_lexer_consume_token (parser->lexer);
	  /* Parse the labels.  */
	  labels = cp_parser_asm_label_list (parser);
	}

      if (goto_p && !labels_p)
	missing = clobbers_p ? "%<:%>" : "%<:%> or %<::%>";
    }
  else if (goto_p)
    missing = "%<:%> or %<::%>";

  /* Look for the closing `)'.  */
  if (!cp_parser_require (parser, missing ? CPP_COLON : CPP_CLOSE_PAREN,
			  missing ? missing : "%<)%>"))
    cp_parser_skip_to_closing_parenthesis (parser, true, false,
					   /*consume_paren=*/true);
  cp_parser_require (parser, CPP_SEMICOLON, "%<;%>");

  if (!invalid_inputs_p && !invalid_outputs_p)
    {
      /* Create the ASM_EXPR.  */
      if (parser->in_function_body)
	{
	  asm_stmt = finish_asm_stmt (volatile_p, string, outputs,
				      inputs, clobbers, labels);
	  /* If the extended syntax was not used, mark the ASM_EXPR.  */
	  if (!extended_p)
	    {
	      tree temp = asm_stmt;
	      if (TREE_CODE (temp) == CLEANUP_POINT_EXPR)
		temp = TREE_OPERAND (temp, 0);

	      ASM_INPUT_P (temp) = 1;
	    }
	}
      else
	cgraph_add_asm_node (string);
    }
}

/* Declarators [gram.dcl.decl] */

/* Parse an init-declarator.

   init-declarator:
     declarator initializer [opt]

   GNU Extension:

   init-declarator:
     declarator asm-specification [opt] attributes [opt] initializer [opt]

   function-definition:
     decl-specifier-seq [opt] declarator ctor-initializer [opt]
       function-body
     decl-specifier-seq [opt] declarator function-try-block

   GNU Extension:

   function-definition:
     __extension__ function-definition

   The DECL_SPECIFIERS apply to this declarator.  Returns a
   representation of the entity declared.  If MEMBER_P is TRUE, then
   this declarator appears in a class scope.  The new DECL created by
   this declarator is returned.

   The CHECKS are access checks that should be performed once we know
   what entity is being declared (and, therefore, what classes have
   befriended it).

   If FUNCTION_DEFINITION_ALLOWED_P then we handle the declarator and
   for a function-definition here as well.  If the declarator is a
   declarator for a function-definition, *FUNCTION_DEFINITION_P will
   be TRUE upon return.  By that point, the function-definition will
   have been completely parsed.

   FUNCTION_DEFINITION_P may be NULL if FUNCTION_DEFINITION_ALLOWED_P
   is FALSE.  */

static tree
cp_parser_init_declarator (cp_parser* parser,
			   cp_decl_specifier_seq *decl_specifiers,
			   VEC (deferred_access_check,gc)* checks,
			   bool function_definition_allowed_p,
			   bool member_p,
			   int declares_class_or_enum,
			   bool* function_definition_p)
{
  cp_token *token = NULL, *asm_spec_start_token = NULL,
           *attributes_start_token = NULL;
  cp_declarator *declarator;
  tree prefix_attributes;
  tree attributes;
  tree asm_specification;
  tree initializer;
  tree decl = NULL_TREE;
  tree scope;
  int is_initialized;
  /* Only valid if IS_INITIALIZED is true.  In that case, CPP_EQ if
     initialized with "= ..", CPP_OPEN_PAREN if initialized with
     "(...)".  */
  enum cpp_ttype initialization_kind;
  bool is_direct_init = false;
  bool is_non_constant_init;
  int ctor_dtor_or_conv_p;
  bool friend_p;
  tree pushed_scope = NULL;

  /* Gather the attributes that were provided with the
     decl-specifiers.  */
  prefix_attributes = decl_specifiers->attributes;

  /* Assume that this is not the declarator for a function
     definition.  */
  if (function_definition_p)
    *function_definition_p = false;

  /* Defer access checks while parsing the declarator; we cannot know
     what names are accessible until we know what is being
     declared.  */
  resume_deferring_access_checks ();

  /* Parse the declarator.  */
  token = cp_lexer_peek_token (parser->lexer);
  declarator
    = cp_parser_declarator (parser, CP_PARSER_DECLARATOR_NAMED,
			    &ctor_dtor_or_conv_p,
			    /*parenthesized_p=*/NULL,
			    /*member_p=*/false);
  /* Gather up the deferred checks.  */
  stop_deferring_access_checks ();

  /* If the DECLARATOR was erroneous, there's no need to go
     further.  */
  if (declarator == cp_error_declarator)
    return error_mark_node;

  /* Check that the number of template-parameter-lists is OK.  */
  if (!cp_parser_check_declarator_template_parameters (parser, declarator,
						       token->location))
    return error_mark_node;

  if (declares_class_or_enum & 2)
    cp_parser_check_for_definition_in_return_type (declarator,
						   decl_specifiers->type,
						   decl_specifiers->type_location);

  /* Figure out what scope the entity declared by the DECLARATOR is
     located in.  `grokdeclarator' sometimes changes the scope, so
     we compute it now.  */
  scope = get_scope_of_declarator (declarator);

  /* Perform any lookups in the declared type which were thought to be
     dependent, but are not in the scope of the declarator.  */
  decl_specifiers->type
    = maybe_update_decl_type (decl_specifiers->type, scope);

  /* If we're allowing GNU extensions, look for an asm-specification
     and attributes.  */
  if (cp_parser_allow_gnu_extensions_p (parser))
    {
      /* Look for an asm-specification.  */
      asm_spec_start_token = cp_lexer_peek_token (parser->lexer);
      asm_specification = cp_parser_asm_specification_opt (parser);
      /* And attributes.  */
      attributes_start_token = cp_lexer_peek_token (parser->lexer);
      attributes = cp_parser_attributes_opt (parser);
    }
  else
    {
      asm_specification = NULL_TREE;
      attributes = NULL_TREE;
    }

  /* Peek at the next token.  */
  token = cp_lexer_peek_token (parser->lexer);
  /* Check to see if the token indicates the start of a
     function-definition.  */
  if (function_declarator_p (declarator)
      && cp_parser_token_starts_function_definition_p (token))
    {
      if (!function_definition_allowed_p)
	{
	  /* If a function-definition should not appear here, issue an
	     error message.  */
	  cp_parser_error (parser,
			   "a function-definition is not allowed here");
	  return error_mark_node;
	}
      else
	{
	  location_t func_brace_location
	    = cp_lexer_peek_token (parser->lexer)->location;

	  /* Neither attributes nor an asm-specification are allowed
	     on a function-definition.  */
	  if (asm_specification)
	    error_at (asm_spec_start_token->location,
		      "an asm-specification is not allowed "
		      "on a function-definition");
	  if (attributes)
	    error_at (attributes_start_token->location,
		      "attributes are not allowed on a function-definition");
	  /* This is a function-definition.  */
	  *function_definition_p = true;

	  /* Parse the function definition.  */
	  if (member_p)
	    decl = cp_parser_save_member_function_body (parser,
							decl_specifiers,
							declarator,
							prefix_attributes);
	  else
	    decl
	      = (cp_parser_function_definition_from_specifiers_and_declarator
		 (parser, decl_specifiers, prefix_attributes, declarator));

	  if (decl != error_mark_node && DECL_STRUCT_FUNCTION (decl))
	    {
	      /* This is where the prologue starts...  */
	      DECL_STRUCT_FUNCTION (decl)->function_start_locus
		= func_brace_location;
	    }

	  return decl;
	}
    }

  /* [dcl.dcl]

     Only in function declarations for constructors, destructors, and
     type conversions can the decl-specifier-seq be omitted.

     We explicitly postpone this check past the point where we handle
     function-definitions because we tolerate function-definitions
     that are missing their return types in some modes.  */
  if (!decl_specifiers->any_specifiers_p && ctor_dtor_or_conv_p <= 0)
    {
      cp_parser_error (parser,
		       "expected constructor, destructor, or type conversion");
      return error_mark_node;
    }

  /* An `=' or an `(', or an '{' in C++0x, indicates an initializer.  */
  if (token->type == CPP_EQ
      || token->type == CPP_OPEN_PAREN
      || token->type == CPP_OPEN_BRACE)
    {
      is_initialized = SD_INITIALIZED;
      initialization_kind = token->type;

      if (token->type == CPP_EQ
	  && function_declarator_p (declarator))
	{
	  cp_token *t2 = cp_lexer_peek_nth_token (parser->lexer, 2);
	  if (t2->keyword == RID_DEFAULT)
	    is_initialized = SD_DEFAULTED;
	  else if (t2->keyword == RID_DELETE)
	    is_initialized = SD_DELETED;
	}
    }
  else
    {
      /* If the init-declarator isn't initialized and isn't followed by a
	 `,' or `;', it's not a valid init-declarator.  */
      if (token->type != CPP_COMMA
	  && token->type != CPP_SEMICOLON)
	{
	  cp_parser_error (parser, "expected initializer");
	  return error_mark_node;
	}
      is_initialized = SD_UNINITIALIZED;
      initialization_kind = CPP_EOF;
    }

  /* Because start_decl has side-effects, we should only call it if we
     know we're going ahead.  By this point, we know that we cannot
     possibly be looking at any other construct.  */
  cp_parser_commit_to_tentative_parse (parser);

  /* If the decl specifiers were bad, issue an error now that we're
     sure this was intended to be a declarator.  Then continue
     declaring the variable(s), as int, to try to cut down on further
     errors.  */
  if (decl_specifiers->any_specifiers_p
      && decl_specifiers->type == error_mark_node)
    {
      cp_parser_error (parser, "invalid type in declaration");
      decl_specifiers->type = integer_type_node;
    }

  /* Check to see whether or not this declaration is a friend.  */
  friend_p = cp_parser_friend_p (decl_specifiers);

  /* Enter the newly declared entry in the symbol table.  If we're
     processing a declaration in a class-specifier, we wait until
     after processing the initializer.  */
  if (!member_p)
    {
      if (parser->in_unbraced_linkage_specification_p)
	decl_specifiers->storage_class = sc_extern;
      decl = start_decl (declarator, decl_specifiers,
			 is_initialized, attributes, prefix_attributes,
			 &pushed_scope);
    }
  else if (scope)
    /* Enter the SCOPE.  That way unqualified names appearing in the
       initializer will be looked up in SCOPE.  */
    pushed_scope = push_scope (scope);

  /* Perform deferred access control checks, now that we know in which
     SCOPE the declared entity resides.  */
  if (!member_p && decl)
    {
      tree saved_current_function_decl = NULL_TREE;

      /* If the entity being declared is a function, pretend that we
	 are in its scope.  If it is a `friend', it may have access to
	 things that would not otherwise be accessible.  */
      if (TREE_CODE (decl) == FUNCTION_DECL)
	{
	  saved_current_function_decl = current_function_decl;
	  current_function_decl = decl;
	}

      /* Perform access checks for template parameters.  */
      cp_parser_perform_template_parameter_access_checks (checks);

      /* Perform the access control checks for the declarator and the
	 decl-specifiers.  */
      perform_deferred_access_checks ();

      /* Restore the saved value.  */
      if (TREE_CODE (decl) == FUNCTION_DECL)
	current_function_decl = saved_current_function_decl;
    }

  /* Parse the initializer.  */
  initializer = NULL_TREE;
  is_direct_init = false;
  is_non_constant_init = true;
  if (is_initialized)
    {
      if (function_declarator_p (declarator))
	{
	  cp_token *initializer_start_token = cp_lexer_peek_token (parser->lexer);
	   if (initialization_kind == CPP_EQ)
	     initializer = cp_parser_pure_specifier (parser);
	   else
	     {
	       /* If the declaration was erroneous, we don't really
		  know what the user intended, so just silently
		  consume the initializer.  */
	       if (decl != error_mark_node)
		 error_at (initializer_start_token->location,
			   "initializer provided for function");
	       cp_parser_skip_to_closing_parenthesis (parser,
						      /*recovering=*/true,
						      /*or_comma=*/false,
						      /*consume_paren=*/true);
	     }
	}
      else
	{
	  /* We want to record the extra mangling scope for in-class
	     initializers of class members and initializers of static data
	     member templates.  The former is a C++0x feature which isn't
	     implemented yet, and I expect it will involve deferring
	     parsing of the initializer until end of class as with default
	     arguments.  So right here we only handle the latter.  */
	  if (!member_p && processing_template_decl)
	    start_lambda_scope (decl);
	  initializer = cp_parser_initializer (parser,
					       &is_direct_init,
					       &is_non_constant_init);
	  if (!member_p && processing_template_decl)
	    finish_lambda_scope ();
	}
    }

  /* The old parser allows attributes to appear after a parenthesized
     initializer.  Mark Mitchell proposed removing this functionality
     on the GCC mailing lists on 2002-08-13.  This parser accepts the
     attributes -- but ignores them.  */
  if (cp_parser_allow_gnu_extensions_p (parser)
      && initialization_kind == CPP_OPEN_PAREN)
    if (cp_parser_attributes_opt (parser))
      warning (OPT_Wattributes,
	       "attributes after parenthesized initializer ignored");

  /* For an in-class declaration, use `grokfield' to create the
     declaration.  */
  if (member_p)
    {
      if (pushed_scope)
	{
	  pop_scope (pushed_scope);
	  pushed_scope = false;
	}
      decl = grokfield (declarator, decl_specifiers,
			initializer, !is_non_constant_init,
			/*asmspec=*/NULL_TREE,
			prefix_attributes);
      if (decl && TREE_CODE (decl) == FUNCTION_DECL)
	cp_parser_save_default_args (parser, decl);
    }

  /* Finish processing the declaration.  But, skip friend
     declarations.  */
  if (!friend_p && decl && decl != error_mark_node)
    {
      cp_finish_decl (decl,
		      initializer, !is_non_constant_init,
		      asm_specification,
		      /* If the initializer is in parentheses, then this is
			 a direct-initialization, which means that an
			 `explicit' constructor is OK.  Otherwise, an
			 `explicit' constructor cannot be used.  */
		      ((is_direct_init || !is_initialized)
		       ? 0 : LOOKUP_ONLYCONVERTING));
    }
  else if ((cxx_dialect != cxx98) && friend_p
	   && decl && TREE_CODE (decl) == FUNCTION_DECL)
    /* Core issue #226 (C++0x only): A default template-argument
       shall not be specified in a friend class template
       declaration. */
    check_default_tmpl_args (decl, current_template_parms, /*is_primary=*/1, 
                             /*is_partial=*/0, /*is_friend_decl=*/1);

  if (!friend_p && pushed_scope)
    pop_scope (pushed_scope);

  return decl;
}

/* Parse a declarator.

   declarator:
     direct-declarator
     ptr-operator declarator

   abstract-declarator:
     ptr-operator abstract-declarator [opt]
     direct-abstract-declarator

   GNU Extensions:

   declarator:
     attributes [opt] direct-declarator
     attributes [opt] ptr-operator declarator

   abstract-declarator:
     attributes [opt] ptr-operator abstract-declarator [opt]
     attributes [opt] direct-abstract-declarator

   If CTOR_DTOR_OR_CONV_P is not NULL, *CTOR_DTOR_OR_CONV_P is used to
   detect constructor, destructor or conversion operators. It is set
   to -1 if the declarator is a name, and +1 if it is a
   function. Otherwise it is set to zero. Usually you just want to
   test for >0, but internally the negative value is used.

   (The reason for CTOR_DTOR_OR_CONV_P is that a declaration must have
   a decl-specifier-seq unless it declares a constructor, destructor,
   or conversion.  It might seem that we could check this condition in
   semantic analysis, rather than parsing, but that makes it difficult
   to handle something like `f()'.  We want to notice that there are
   no decl-specifiers, and therefore realize that this is an
   expression, not a declaration.)

   If PARENTHESIZED_P is non-NULL, *PARENTHESIZED_P is set to true iff
   the declarator is a direct-declarator of the form "(...)".

   MEMBER_P is true iff this declarator is a member-declarator.  */

static cp_declarator *
cp_parser_declarator (cp_parser* parser,
		      cp_parser_declarator_kind dcl_kind,
		      int* ctor_dtor_or_conv_p,
		      bool* parenthesized_p,
		      bool member_p)
{
  cp_declarator *declarator;
  enum tree_code code;
  cp_cv_quals cv_quals;
  tree class_type;
  tree attributes = NULL_TREE;

  /* Assume this is not a constructor, destructor, or type-conversion
     operator.  */
  if (ctor_dtor_or_conv_p)
    *ctor_dtor_or_conv_p = 0;

  if (cp_parser_allow_gnu_extensions_p (parser))
    attributes = cp_parser_attributes_opt (parser);

  /* Check for the ptr-operator production.  */
  cp_parser_parse_tentatively (parser);
  /* Parse the ptr-operator.  */
  code = cp_parser_ptr_operator (parser,
				 &class_type,
				 &cv_quals);
  /* If that worked, then we have a ptr-operator.  */
  if (cp_parser_parse_definitely (parser))
    {
      /* If a ptr-operator was found, then this declarator was not
	 parenthesized.  */
      if (parenthesized_p)
	*parenthesized_p = true;
      /* The dependent declarator is optional if we are parsing an
	 abstract-declarator.  */
      if (dcl_kind != CP_PARSER_DECLARATOR_NAMED)
	cp_parser_parse_tentatively (parser);

      /* Parse the dependent declarator.  */
      declarator = cp_parser_declarator (parser, dcl_kind,
					 /*ctor_dtor_or_conv_p=*/NULL,
					 /*parenthesized_p=*/NULL,
					 /*member_p=*/false);

      /* If we are parsing an abstract-declarator, we must handle the
	 case where the dependent declarator is absent.  */
      if (dcl_kind != CP_PARSER_DECLARATOR_NAMED
	  && !cp_parser_parse_definitely (parser))
	declarator = NULL;

      declarator = cp_parser_make_indirect_declarator
	(code, class_type, cv_quals, declarator);
    }
  /* Everything else is a direct-declarator.  */
  else
    {
      if (parenthesized_p)
	*parenthesized_p = cp_lexer_next_token_is (parser->lexer,
						   CPP_OPEN_PAREN);
      declarator = cp_parser_direct_declarator (parser, dcl_kind,
						ctor_dtor_or_conv_p,
						member_p);
    }

  if (attributes && declarator && declarator != cp_error_declarator)
    declarator->attributes = attributes;

  return declarator;
}

/* Parse a direct-declarator or direct-abstract-declarator.

   direct-declarator:
     declarator-id
     direct-declarator ( parameter-declaration-clause )
       cv-qualifier-seq [opt]
       exception-specification [opt]
     direct-declarator [ constant-expression [opt] ]
     ( declarator )

   direct-abstract-declarator:
     direct-abstract-declarator [opt]
       ( parameter-declaration-clause )
       cv-qualifier-seq [opt]
       exception-specification [opt]
     direct-abstract-declarator [opt] [ constant-expression [opt] ]
     ( abstract-declarator )

   Returns a representation of the declarator.  DCL_KIND is
   CP_PARSER_DECLARATOR_ABSTRACT, if we are parsing a
   direct-abstract-declarator.  It is CP_PARSER_DECLARATOR_NAMED, if
   we are parsing a direct-declarator.  It is
   CP_PARSER_DECLARATOR_EITHER, if we can accept either - in the case
   of ambiguity we prefer an abstract declarator, as per
   [dcl.ambig.res].  CTOR_DTOR_OR_CONV_P and MEMBER_P are as for
   cp_parser_declarator.  */

static cp_declarator *
cp_parser_direct_declarator (cp_parser* parser,
			     cp_parser_declarator_kind dcl_kind,
			     int* ctor_dtor_or_conv_p,
			     bool member_p)
{
  cp_token *token;
  cp_declarator *declarator = NULL;
  tree scope = NULL_TREE;
  bool saved_default_arg_ok_p = parser->default_arg_ok_p;
  bool saved_in_declarator_p = parser->in_declarator_p;
  bool first = true;
  tree pushed_scope = NULL_TREE;

  while (true)
    {
      /* Peek at the next token.  */
      token = cp_lexer_peek_token (parser->lexer);
      if (token->type == CPP_OPEN_PAREN)
	{
	  /* This is either a parameter-declaration-clause, or a
	     parenthesized declarator. When we know we are parsing a
	     named declarator, it must be a parenthesized declarator
	     if FIRST is true. For instance, `(int)' is a
	     parameter-declaration-clause, with an omitted
	     direct-abstract-declarator. But `((*))', is a
	     parenthesized abstract declarator. Finally, when T is a
	     template parameter `(T)' is a
	     parameter-declaration-clause, and not a parenthesized
	     named declarator.

	     We first try and parse a parameter-declaration-clause,
	     and then try a nested declarator (if FIRST is true).

	     It is not an error for it not to be a
	     parameter-declaration-clause, even when FIRST is
	     false. Consider,

	       int i (int);
	       int i (3);

	     The first is the declaration of a function while the
	     second is the definition of a variable, including its
	     initializer.

	     Having seen only the parenthesis, we cannot know which of
	     these two alternatives should be selected.  Even more
	     complex are examples like:

	       int i (int (a));
	       int i (int (3));

	     The former is a function-declaration; the latter is a
	     variable initialization.

	     Thus again, we try a parameter-declaration-clause, and if
	     that fails, we back out and return.  */

	  if (!first || dcl_kind != CP_PARSER_DECLARATOR_NAMED)
	    {
	      tree params;
	      unsigned saved_num_template_parameter_lists;
	      bool is_declarator = false;
	      tree t;

	      /* In a member-declarator, the only valid interpretation
		 of a parenthesis is the start of a
		 parameter-declaration-clause.  (It is invalid to
		 initialize a static data member with a parenthesized
		 initializer; only the "=" form of initialization is
		 permitted.)  */
	      if (!member_p)
		cp_parser_parse_tentatively (parser);

	      /* Consume the `('.  */
	      cp_lexer_consume_token (parser->lexer);
	      if (first)
		{
		  /* If this is going to be an abstract declarator, we're
		     in a declarator and we can't have default args.  */
		  parser->default_arg_ok_p = false;
		  parser->in_declarator_p = true;
		}

	      /* Inside the function parameter list, surrounding
		 template-parameter-lists do not apply.  */
	      saved_num_template_parameter_lists
		= parser->num_template_parameter_lists;
	      parser->num_template_parameter_lists = 0;

	      begin_scope (sk_function_parms, NULL_TREE);

	      /* Parse the parameter-declaration-clause.  */
	      params = cp_parser_parameter_declaration_clause (parser);

	      parser->num_template_parameter_lists
		= saved_num_template_parameter_lists;

	      /* If all went well, parse the cv-qualifier-seq and the
		 exception-specification.  */
	      if (member_p || cp_parser_parse_definitely (parser))
		{
		  cp_cv_quals cv_quals;
		  tree exception_specification;
		  tree late_return;

		  is_declarator = true;

		  if (ctor_dtor_or_conv_p)
		    *ctor_dtor_or_conv_p = *ctor_dtor_or_conv_p < 0;
		  first = false;
		  /* Consume the `)'.  */
		  cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>");

		  /* Parse the cv-qualifier-seq.  */
		  cv_quals = cp_parser_cv_qualifier_seq_opt (parser);
		  /* And the exception-specification.  */
		  exception_specification
		    = cp_parser_exception_specification_opt (parser);

		  late_return
		    = cp_parser_late_return_type_opt (parser);

		  /* Create the function-declarator.  */
		  declarator = make_call_declarator (declarator,
						     params,
						     cv_quals,
						     exception_specification,
						     late_return);
		  /* Any subsequent parameter lists are to do with
		     return type, so are not those of the declared
		     function.  */
		  parser->default_arg_ok_p = false;
		}

	      /* Remove the function parms from scope.  */
	      for (t = current_binding_level->names; t; t = TREE_CHAIN (t))
		pop_binding (DECL_NAME (t), t);
	      leave_scope();

	      if (is_declarator)
		/* Repeat the main loop.  */
		continue;
	    }

	  /* If this is the first, we can try a parenthesized
	     declarator.  */
	  if (first)
	    {
	      bool saved_in_type_id_in_expr_p;

	      parser->default_arg_ok_p = saved_default_arg_ok_p;
	      parser->in_declarator_p = saved_in_declarator_p;

	      /* Consume the `('.  */
	      cp_lexer_consume_token (parser->lexer);
	      /* Parse the nested declarator.  */
	      saved_in_type_id_in_expr_p = parser->in_type_id_in_expr_p;
	      parser->in_type_id_in_expr_p = true;
	      declarator
		= cp_parser_declarator (parser, dcl_kind, ctor_dtor_or_conv_p,
					/*parenthesized_p=*/NULL,
					member_p);
	      parser->in_type_id_in_expr_p = saved_in_type_id_in_expr_p;
	      first = false;
	      /* Expect a `)'.  */
	      if (!cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>"))
		declarator = cp_error_declarator;
	      if (declarator == cp_error_declarator)
		break;

	      goto handle_declarator;
	    }
	  /* Otherwise, we must be done.  */
	  else
	    break;
	}
      else if ((!first || dcl_kind != CP_PARSER_DECLARATOR_NAMED)
	       && token->type == CPP_OPEN_SQUARE)
	{
	  /* Parse an array-declarator.  */
	  tree bounds;

	  if (ctor_dtor_or_conv_p)
	    *ctor_dtor_or_conv_p = 0;

	  first = false;
	  parser->default_arg_ok_p = false;
	  parser->in_declarator_p = true;
	  /* Consume the `['.  */
	  cp_lexer_consume_token (parser->lexer);
	  /* Peek at the next token.  */
	  token = cp_lexer_peek_token (parser->lexer);
	  /* If the next token is `]', then there is no
	     constant-expression.  */
	  if (token->type != CPP_CLOSE_SQUARE)
	    {
	      bool non_constant_p;

	      bounds
		= cp_parser_constant_expression (parser,
						 /*allow_non_constant=*/true,
						 &non_constant_p);
	      if (!non_constant_p)
		bounds = fold_non_dependent_expr (bounds);
	      /* Normally, the array bound must be an integral constant
		 expression.  However, as an extension, we allow VLAs
		 in function scopes.  */
	      else if (!parser->in_function_body)
		{
		  error_at (token->location,
			    "array bound is not an integer constant");
		  bounds = error_mark_node;
		}
	      else if (processing_template_decl && !error_operand_p (bounds))
		{
		  /* Remember this wasn't a constant-expression.  */
		  bounds = build_nop (TREE_TYPE (bounds), bounds);
		  TREE_SIDE_EFFECTS (bounds) = 1;
		}
	    }
	  else
	    bounds = NULL_TREE;
	  /* Look for the closing `]'.  */
	  if (!cp_parser_require (parser, CPP_CLOSE_SQUARE, "%<]%>"))
	    {
	      declarator = cp_error_declarator;
	      break;
	    }

	  declarator = make_array_declarator (declarator, bounds);
	}
      else if (first && dcl_kind != CP_PARSER_DECLARATOR_ABSTRACT)
	{
	  {
	    tree qualifying_scope;
	    tree unqualified_name;
	    special_function_kind sfk;
	    bool abstract_ok;
	    bool pack_expansion_p = false;
	    cp_token *declarator_id_start_token;

	    /* Parse a declarator-id */
	    abstract_ok = (dcl_kind == CP_PARSER_DECLARATOR_EITHER);
	    if (abstract_ok)
	      {
		cp_parser_parse_tentatively (parser);

		/* If we see an ellipsis, we should be looking at a
		   parameter pack. */
		if (token->type == CPP_ELLIPSIS)
		  {
		    /* Consume the `...' */
		    cp_lexer_consume_token (parser->lexer);

		    pack_expansion_p = true;
		  }
	      }

	    declarator_id_start_token = cp_lexer_peek_token (parser->lexer);
	    unqualified_name
	      = cp_parser_declarator_id (parser, /*optional_p=*/abstract_ok);
	    qualifying_scope = parser->scope;
	    if (abstract_ok)
	      {
		bool okay = false;

		if (!unqualified_name && pack_expansion_p)
		  {
		    /* Check whether an error occurred. */
		    okay = !cp_parser_error_occurred (parser);

		    /* We already consumed the ellipsis to mark a
		       parameter pack, but we have no way to report it,
		       so abort the tentative parse. We will be exiting
		       immediately anyway. */
		    cp_parser_abort_tentative_parse (parser);
		  }
		else
		  okay = cp_parser_parse_definitely (parser);

		if (!okay)
		  unqualified_name = error_mark_node;
		else if (unqualified_name
			 && (qualifying_scope
			     || (TREE_CODE (unqualified_name)
				 != IDENTIFIER_NODE)))
		  {
		    cp_parser_error (parser, "expected unqualified-id");
		    unqualified_name = error_mark_node;
		  }
	      }

	    if (!unqualified_name)
	      return NULL;
	    if (unqualified_name == error_mark_node)
	      {
		declarator = cp_error_declarator;
		pack_expansion_p = false;
		declarator->parameter_pack_p = false;
		break;
	      }

	    if (qualifying_scope && at_namespace_scope_p ()
		&& TREE_CODE (qualifying_scope) == TYPENAME_TYPE)
	      {
		/* In the declaration of a member of a template class
		   outside of the class itself, the SCOPE will sometimes
		   be a TYPENAME_TYPE.  For example, given:

		   template <typename T>
		   int S<T>::R::i = 3;

		   the SCOPE will be a TYPENAME_TYPE for `S<T>::R'.  In
		   this context, we must resolve S<T>::R to an ordinary
		   type, rather than a typename type.

		   The reason we normally avoid resolving TYPENAME_TYPEs
		   is that a specialization of `S' might render
		   `S<T>::R' not a type.  However, if `S' is
		   specialized, then this `i' will not be used, so there
		   is no harm in resolving the types here.  */
		tree type;

		/* Resolve the TYPENAME_TYPE.  */
		type = resolve_typename_type (qualifying_scope,
					      /*only_current_p=*/false);
		/* If that failed, the declarator is invalid.  */
		if (TREE_CODE (type) == TYPENAME_TYPE)
		  {
		    if (typedef_variant_p (type))
		      error_at (declarator_id_start_token->location,
				"cannot define member of dependent typedef "
				"%qT", type);
		    else
		      error_at (declarator_id_start_token->location,
				"%<%T::%E%> is not a type",
				TYPE_CONTEXT (qualifying_scope),
				TYPE_IDENTIFIER (qualifying_scope));
		  }
		qualifying_scope = type;
	      }

	    sfk = sfk_none;

	    if (unqualified_name)
	      {
		tree class_type;

		if (qualifying_scope
		    && CLASS_TYPE_P (qualifying_scope))
		  class_type = qualifying_scope;
		else
		  class_type = current_class_type;

		if (TREE_CODE (unqualified_name) == TYPE_DECL)
		  {
		    tree name_type = TREE_TYPE (unqualified_name);
		    if (class_type && same_type_p (name_type, class_type))
		      {
			if (qualifying_scope
			    && CLASSTYPE_USE_TEMPLATE (name_type))
			  {
			    error_at (declarator_id_start_token->location,
				      "invalid use of constructor as a template");
			    inform (declarator_id_start_token->location,
				    "use %<%T::%D%> instead of %<%T::%D%> to "
				    "name the constructor in a qualified name",
				    class_type,
				    DECL_NAME (TYPE_TI_TEMPLATE (class_type)),
				    class_type, name_type);
			    declarator = cp_error_declarator;
			    break;
			  }
			else
			  unqualified_name = constructor_name (class_type);
		      }
		    else
		      {
			/* We do not attempt to print the declarator
			   here because we do not have enough
			   information about its original syntactic
			   form.  */
			cp_parser_error (parser, "invalid declarator");
			declarator = cp_error_declarator;
			break;
		      }
		  }

		if (class_type)
		  {
		    if (TREE_CODE (unqualified_name) == BIT_NOT_EXPR)
		      sfk = sfk_destructor;
		    else if (IDENTIFIER_TYPENAME_P (unqualified_name))
		      sfk = sfk_conversion;
		    else if (/* There's no way to declare a constructor
				for an anonymous type, even if the type
				got a name for linkage purposes.  */
			     !TYPE_WAS_ANONYMOUS (class_type)
			     && constructor_name_p (unqualified_name,
						    class_type))
		      {
			unqualified_name = constructor_name (class_type);
			sfk = sfk_constructor;
		      }
		    else if (is_overloaded_fn (unqualified_name)
			     && DECL_CONSTRUCTOR_P (get_first_fn
						    (unqualified_name)))
		      sfk = sfk_constructor;

		    if (ctor_dtor_or_conv_p && sfk != sfk_none)
		      *ctor_dtor_or_conv_p = -1;
		  }
	      }
	    declarator = make_id_declarator (qualifying_scope,
					     unqualified_name,
					     sfk);
	    declarator->id_loc = token->location;
	    declarator->parameter_pack_p = pack_expansion_p;

	    if (pack_expansion_p)
	      maybe_warn_variadic_templates ();
	  }

	handle_declarator:;
	  scope = get_scope_of_declarator (declarator);
	  if (scope)
	    /* Any names that appear after the declarator-id for a
	       member are looked up in the containing scope.  */
	    pushed_scope = push_scope (scope);
	  parser->in_declarator_p = true;
	  if ((ctor_dtor_or_conv_p && *ctor_dtor_or_conv_p)
	      || (declarator && declarator->kind == cdk_id))
	    /* Default args are only allowed on function
	       declarations.  */
	    parser->default_arg_ok_p = saved_default_arg_ok_p;
	  else
	    parser->default_arg_ok_p = false;

	  first = false;
	}
      /* We're done.  */
      else
	break;
    }

  /* For an abstract declarator, we might wind up with nothing at this
     point.  That's an error; the declarator is not optional.  */
  if (!declarator)
    cp_parser_error (parser, "expected declarator");

  /* If we entered a scope, we must exit it now.  */
  if (pushed_scope)
    pop_scope (pushed_scope);

  parser->default_arg_ok_p = saved_default_arg_ok_p;
  parser->in_declarator_p = saved_in_declarator_p;

  return declarator;
}

/* Parse a ptr-operator.

   ptr-operator:
     * cv-qualifier-seq [opt]
     &
     :: [opt] nested-name-specifier * cv-qualifier-seq [opt]

   GNU Extension:

   ptr-operator:
     & cv-qualifier-seq [opt]

   Returns INDIRECT_REF if a pointer, or pointer-to-member, was used.
   Returns ADDR_EXPR if a reference was used, or NON_LVALUE_EXPR for
   an rvalue reference. In the case of a pointer-to-member, *TYPE is
   filled in with the TYPE containing the member.  *CV_QUALS is
   filled in with the cv-qualifier-seq, or TYPE_UNQUALIFIED, if there
   are no cv-qualifiers.  Returns ERROR_MARK if an error occurred.
   Note that the tree codes returned by this function have nothing
   to do with the types of trees that will be eventually be created
   to represent the pointer or reference type being parsed. They are
   just constants with suggestive names. */
static enum tree_code
cp_parser_ptr_operator (cp_parser* parser,
			tree* type,
			cp_cv_quals *cv_quals)
{
  enum tree_code code = ERROR_MARK;
  cp_token *token;

  /* Assume that it's not a pointer-to-member.  */
  *type = NULL_TREE;
  /* And that there are no cv-qualifiers.  */
  *cv_quals = TYPE_UNQUALIFIED;

  /* Peek at the next token.  */
  token = cp_lexer_peek_token (parser->lexer);

  /* If it's a `*', `&' or `&&' we have a pointer or reference.  */
  if (token->type == CPP_MULT)
    code = INDIRECT_REF;
  else if (token->type == CPP_AND)
    code = ADDR_EXPR;
  else if ((cxx_dialect != cxx98) &&
	   token->type == CPP_AND_AND) /* C++0x only */
    code = NON_LVALUE_EXPR;

  if (code != ERROR_MARK)
    {
      /* Consume the `*', `&' or `&&'.  */
      cp_lexer_consume_token (parser->lexer);

      /* A `*' can be followed by a cv-qualifier-seq, and so can a
	 `&', if we are allowing GNU extensions.  (The only qualifier
	 that can legally appear after `&' is `restrict', but that is
	 enforced during semantic analysis.  */
      if (code == INDIRECT_REF
	  || cp_parser_allow_gnu_extensions_p (parser))
	*cv_quals = cp_parser_cv_qualifier_seq_opt (parser);
    }
  else
    {
      /* Try the pointer-to-member case.  */
      cp_parser_parse_tentatively (parser);
      /* Look for the optional `::' operator.  */
      cp_parser_global_scope_opt (parser,
				  /*current_scope_valid_p=*/false);
      /* Look for the nested-name specifier.  */
      token = cp_lexer_peek_token (parser->lexer);
      cp_parser_nested_name_specifier (parser,
				       /*typename_keyword_p=*/false,
				       /*check_dependency_p=*/true,
				       /*type_p=*/false,
				       /*is_declaration=*/false);
      /* If we found it, and the next token is a `*', then we are
	 indeed looking at a pointer-to-member operator.  */
      if (!cp_parser_error_occurred (parser)
	  && cp_parser_require (parser, CPP_MULT, "%<*%>"))
	{
	  /* Indicate that the `*' operator was used.  */
	  code = INDIRECT_REF;

	  if (TREE_CODE (parser->scope) == NAMESPACE_DECL)
	    error_at (token->location, "%qD is a namespace", parser->scope);
	  else
	    {
	      /* The type of which the member is a member is given by the
		 current SCOPE.  */
	      *type = parser->scope;
	      /* The next name will not be qualified.  */
	      parser->scope = NULL_TREE;
	      parser->qualifying_scope = NULL_TREE;
	      parser->object_scope = NULL_TREE;
	      /* Look for the optional cv-qualifier-seq.  */
	      *cv_quals = cp_parser_cv_qualifier_seq_opt (parser);
	    }
	}
      /* If that didn't work we don't have a ptr-operator.  */
      if (!cp_parser_parse_definitely (parser))
	cp_parser_error (parser, "expected ptr-operator");
    }

  return code;
}

/* Parse an (optional) cv-qualifier-seq.

   cv-qualifier-seq:
     cv-qualifier cv-qualifier-seq [opt]

   cv-qualifier:
     const
     volatile

   GNU Extension:

   cv-qualifier:
     __restrict__

   Returns a bitmask representing the cv-qualifiers.  */

static cp_cv_quals
cp_parser_cv_qualifier_seq_opt (cp_parser* parser)
{
  cp_cv_quals cv_quals = TYPE_UNQUALIFIED;

  while (true)
    {
      cp_token *token;
      cp_cv_quals cv_qualifier;

      /* Peek at the next token.  */
      token = cp_lexer_peek_token (parser->lexer);
      /* See if it's a cv-qualifier.  */
      switch (token->keyword)
	{
	case RID_CONST:
	  cv_qualifier = TYPE_QUAL_CONST;
	  break;

	case RID_VOLATILE:
	  cv_qualifier = TYPE_QUAL_VOLATILE;
	  break;

	case RID_RESTRICT:
	  cv_qualifier = TYPE_QUAL_RESTRICT;
	  break;

	default:
	  cv_qualifier = TYPE_UNQUALIFIED;
	  break;
	}

      if (!cv_qualifier)
	break;

      if (cv_quals & cv_qualifier)
	{
	  error_at (token->location, "duplicate cv-qualifier");
	  cp_lexer_purge_token (parser->lexer);
	}
      else
	{
	  cp_lexer_consume_token (parser->lexer);
	  cv_quals |= cv_qualifier;
	}
    }

  return cv_quals;
}

/* Parse a late-specified return type, if any.  This is not a separate
   non-terminal, but part of a function declarator, which looks like

   -> trailing-type-specifier-seq abstract-declarator(opt)

   Returns the type indicated by the type-id.  */

static tree
cp_parser_late_return_type_opt (cp_parser* parser)
{
  cp_token *token;

  /* Peek at the next token.  */
  token = cp_lexer_peek_token (parser->lexer);
  /* A late-specified return type is indicated by an initial '->'. */
  if (token->type != CPP_DEREF)
    return NULL_TREE;

  /* Consume the ->.  */
  cp_lexer_consume_token (parser->lexer);

  return cp_parser_trailing_type_id (parser);
}

/* Parse a declarator-id.

   declarator-id:
     id-expression
     :: [opt] nested-name-specifier [opt] type-name

   In the `id-expression' case, the value returned is as for
   cp_parser_id_expression if the id-expression was an unqualified-id.
   If the id-expression was a qualified-id, then a SCOPE_REF is
   returned.  The first operand is the scope (either a NAMESPACE_DECL
   or TREE_TYPE), but the second is still just a representation of an
   unqualified-id.  */

static tree
cp_parser_declarator_id (cp_parser* parser, bool optional_p)
{
  tree id;
  /* The expression must be an id-expression.  Assume that qualified
     names are the names of types so that:

       template <class T>
       int S<T>::R::i = 3;

     will work; we must treat `S<T>::R' as the name of a type.
     Similarly, assume that qualified names are templates, where
     required, so that:

       template <class T>
       int S<T>::R<T>::i = 3;

     will work, too.  */
  id = cp_parser_id_expression (parser,
				/*template_keyword_p=*/false,
				/*check_dependency_p=*/false,
				/*template_p=*/NULL,
				/*declarator_p=*/true,
				optional_p);
  if (id && BASELINK_P (id))
    id = BASELINK_FUNCTIONS (id);
  return id;
}

/* Parse a type-id.

   type-id:
     type-specifier-seq abstract-declarator [opt]

   Returns the TYPE specified.  */

static tree
cp_parser_type_id_1 (cp_parser* parser, bool is_template_arg,
		     bool is_trailing_return)
{
  cp_decl_specifier_seq type_specifier_seq;
  cp_declarator *abstract_declarator;

  /* Parse the type-specifier-seq.  */
  cp_parser_type_specifier_seq (parser, /*is_declaration=*/false,
				is_trailing_return,
				&type_specifier_seq);
  if (type_specifier_seq.type == error_mark_node)
    return error_mark_node;

  /* There might or might not be an abstract declarator.  */
  cp_parser_parse_tentatively (parser);
  /* Look for the declarator.  */
  abstract_declarator
    = cp_parser_declarator (parser, CP_PARSER_DECLARATOR_ABSTRACT, NULL,
			    /*parenthesized_p=*/NULL,
			    /*member_p=*/false);
  /* Check to see if there really was a declarator.  */
  if (!cp_parser_parse_definitely (parser))
    abstract_declarator = NULL;

  if (type_specifier_seq.type
      && type_uses_auto (type_specifier_seq.type))
    {
      /* A type-id with type 'auto' is only ok if the abstract declarator
	 is a function declarator with a late-specified return type.  */
      if (abstract_declarator
	  && abstract_declarator->kind == cdk_function
	  && abstract_declarator->u.function.late_return_type)
	/* OK */;
      else
	{
	  error ("invalid use of %<auto%>");
	  return error_mark_node;
	}
    }
  
  return groktypename (&type_specifier_seq, abstract_declarator,
		       is_template_arg);
}

static tree cp_parser_type_id (cp_parser *parser)
{
  return cp_parser_type_id_1 (parser, false, false);
}

static tree cp_parser_template_type_arg (cp_parser *parser)
{
  return cp_parser_type_id_1 (parser, true, false);
}

static tree cp_parser_trailing_type_id (cp_parser *parser)
{
  return cp_parser_type_id_1 (parser, false, true);
}

/* Parse a type-specifier-seq.

   type-specifier-seq:
     type-specifier type-specifier-seq [opt]

   GNU extension:

   type-specifier-seq:
     attributes type-specifier-seq [opt]

   If IS_DECLARATION is true, we are at the start of a "condition" or
   exception-declaration, so we might be followed by a declarator-id.

   If IS_TRAILING_RETURN is true, we are in a trailing-return-type,
   i.e. we've just seen "->".

   Sets *TYPE_SPECIFIER_SEQ to represent the sequence.  */

static void
cp_parser_type_specifier_seq (cp_parser* parser,
			      bool is_declaration,
			      bool is_trailing_return,
			      cp_decl_specifier_seq *type_specifier_seq)
{
  bool seen_type_specifier = false;
  cp_parser_flags flags = CP_PARSER_FLAGS_OPTIONAL;
  cp_token *start_token = NULL;

  /* Clear the TYPE_SPECIFIER_SEQ.  */
  clear_decl_specs (type_specifier_seq);

  /* In the context of a trailing return type, enum E { } is an
     elaborated-type-specifier followed by a function-body, not an
     enum-specifier.  */
  if (is_trailing_return)
    flags |= CP_PARSER_FLAGS_NO_TYPE_DEFINITIONS;

  /* Parse the type-specifiers and attributes.  */
  while (true)
    {
      tree type_specifier;
      bool is_cv_qualifier;

      /* Check for attributes first.  */
      if (cp_lexer_next_token_is_keyword (parser->lexer, RID_ATTRIBUTE))
	{
	  type_specifier_seq->attributes =
	    chainon (type_specifier_seq->attributes,
		     cp_parser_attributes_opt (parser));
	  continue;
	}

      /* record the token of the beginning of the type specifier seq,
         for error reporting purposes*/
     if (!start_token)
       start_token = cp_lexer_peek_token (parser->lexer);

      /* Look for the type-specifier.  */
      type_specifier = cp_parser_type_specifier (parser,
						 flags,
						 type_specifier_seq,
						 /*is_declaration=*/false,
						 NULL,
						 &is_cv_qualifier);
      if (!type_specifier)
	{
	  /* If the first type-specifier could not be found, this is not a
	     type-specifier-seq at all.  */
	  if (!seen_type_specifier)
	    {
	      cp_parser_error (parser, "expected type-specifier");
	      type_specifier_seq->type = error_mark_node;
	      return;
	    }
	  /* If subsequent type-specifiers could not be found, the
	     type-specifier-seq is complete.  */
	  break;
	}

      seen_type_specifier = true;
      /* The standard says that a condition can be:

	    type-specifier-seq declarator = assignment-expression

	 However, given:

	   struct S {};
	   if (int S = ...)

	 we should treat the "S" as a declarator, not as a
	 type-specifier.  The standard doesn't say that explicitly for
	 type-specifier-seq, but it does say that for
	 decl-specifier-seq in an ordinary declaration.  Perhaps it
	 would be clearer just to allow a decl-specifier-seq here, and
	 then add a semantic restriction that if any decl-specifiers
	 that are not type-specifiers appear, the program is invalid.  */
      if (is_declaration && !is_cv_qualifier)
	flags |= CP_PARSER_FLAGS_NO_USER_DEFINED_TYPES;
    }

  cp_parser_check_decl_spec (type_specifier_seq, start_token->location);
}

/* Parse a parameter-declaration-clause.

   parameter-declaration-clause:
     parameter-declaration-list [opt] ... [opt]
     parameter-declaration-list , ...

   Returns a representation for the parameter declarations.  A return
   value of NULL indicates a parameter-declaration-clause consisting
   only of an ellipsis.  */

static tree
cp_parser_parameter_declaration_clause (cp_parser* parser)
{
  tree parameters;
  cp_token *token;
  bool ellipsis_p;
  bool is_error;

  /* Peek at the next token.  */
  token = cp_lexer_peek_token (parser->lexer);
  /* Check for trivial parameter-declaration-clauses.  */
  if (token->type == CPP_ELLIPSIS)
    {
      /* Consume the `...' token.  */
      cp_lexer_consume_token (parser->lexer);
      return NULL_TREE;
    }
  else if (token->type == CPP_CLOSE_PAREN)
    /* There are no parameters.  */
    {
#ifndef NO_IMPLICIT_EXTERN_C
      if (in_system_header && current_class_type == NULL
	  && current_lang_name == lang_name_c)
	return NULL_TREE;
      else
#endif
	return void_list_node;
    }
  /* Check for `(void)', too, which is a special case.  */
  else if (token->keyword == RID_VOID
	   && (cp_lexer_peek_nth_token (parser->lexer, 2)->type
	       == CPP_CLOSE_PAREN))
    {
      /* Consume the `void' token.  */
      cp_lexer_consume_token (parser->lexer);
      /* There are no parameters.  */
      return void_list_node;
    }

  /* Parse the parameter-declaration-list.  */
  parameters = cp_parser_parameter_declaration_list (parser, &is_error);
  /* If a parse error occurred while parsing the
     parameter-declaration-list, then the entire
     parameter-declaration-clause is erroneous.  */
  if (is_error)
    return NULL;

  /* Peek at the next token.  */
  token = cp_lexer_peek_token (parser->lexer);
  /* If it's a `,', the clause should terminate with an ellipsis.  */
  if (token->type == CPP_COMMA)
    {
      /* Consume the `,'.  */
      cp_lexer_consume_token (parser->lexer);
      /* Expect an ellipsis.  */
      ellipsis_p
	= (cp_parser_require (parser, CPP_ELLIPSIS, "%<...%>") != NULL);
    }
  /* It might also be `...' if the optional trailing `,' was
     omitted.  */
  else if (token->type == CPP_ELLIPSIS)
    {
      /* Consume the `...' token.  */
      cp_lexer_consume_token (parser->lexer);
      /* And remember that we saw it.  */
      ellipsis_p = true;
    }
  else
    ellipsis_p = false;

  /* Finish the parameter list.  */
  if (!ellipsis_p)
    parameters = chainon (parameters, void_list_node);

  return parameters;
}

/* Parse a parameter-declaration-list.

   parameter-declaration-list:
     parameter-declaration
     parameter-declaration-list , parameter-declaration

   Returns a representation of the parameter-declaration-list, as for
   cp_parser_parameter_declaration_clause.  However, the
   `void_list_node' is never appended to the list.  Upon return,
   *IS_ERROR will be true iff an error occurred.  */

static tree
cp_parser_parameter_declaration_list (cp_parser* parser, bool *is_error)
{
  tree parameters = NULL_TREE;
  tree *tail = &parameters; 
  bool saved_in_unbraced_linkage_specification_p;
  int index = 0;

  /* Assume all will go well.  */
  *is_error = false;
  /* The special considerations that apply to a function within an
     unbraced linkage specifications do not apply to the parameters
     to the function.  */
  saved_in_unbraced_linkage_specification_p 
    = parser->in_unbraced_linkage_specification_p;
  parser->in_unbraced_linkage_specification_p = false;

  /* Look for more parameters.  */
  while (true)
    {
      cp_parameter_declarator *parameter;
      tree decl = error_mark_node;
      bool parenthesized_p;
      /* Parse the parameter.  */
      parameter
	= cp_parser_parameter_declaration (parser,
					   /*template_parm_p=*/false,
					   &parenthesized_p);

      /* We don't know yet if the enclosing context is deprecated, so wait
	 and warn in grokparms if appropriate.  */
      deprecated_state = DEPRECATED_SUPPRESS;

      if (parameter)
	decl = grokdeclarator (parameter->declarator,
			       &parameter->decl_specifiers,
			       PARM,
			       parameter->default_argument != NULL_TREE,
			       &parameter->decl_specifiers.attributes);

      deprecated_state = DEPRECATED_NORMAL;

      /* If a parse error occurred parsing the parameter declaration,
	 then the entire parameter-declaration-list is erroneous.  */
      if (decl == error_mark_node)
	{
	  *is_error = true;
	  parameters = error_mark_node;
	  break;
	}

      if (parameter->decl_specifiers.attributes)
	cplus_decl_attributes (&decl,
			       parameter->decl_specifiers.attributes,
			       0);
      if (DECL_NAME (decl))
	decl = pushdecl (decl);

      if (decl != error_mark_node)
	{
	  retrofit_lang_decl (decl);
	  DECL_PARM_INDEX (decl) = ++index;
	}

      /* Add the new parameter to the list.  */
      *tail = build_tree_list (parameter->default_argument, decl);
      tail = &TREE_CHAIN (*tail);

      /* Peek at the next token.  */
      if (cp_lexer_next_token_is (parser->lexer, CPP_CLOSE_PAREN)
	  || cp_lexer_next_token_is (parser->lexer, CPP_ELLIPSIS)
	  /* These are for Objective-C++ */
	  || cp_lexer_next_token_is (parser->lexer, CPP_SEMICOLON)
	  || cp_lexer_next_token_is (parser->lexer, CPP_OPEN_BRACE))
	/* The parameter-declaration-list is complete.  */
	break;
      else if (cp_lexer_next_token_is (parser->lexer, CPP_COMMA))
	{
	  cp_token *token;

	  /* Peek at the next token.  */
	  token = cp_lexer_peek_nth_token (parser->lexer, 2);
	  /* If it's an ellipsis, then the list is complete.  */
	  if (token->type == CPP_ELLIPSIS)
	    break;
	  /* Otherwise, there must be more parameters.  Consume the
	     `,'.  */
	  cp_lexer_consume_token (parser->lexer);
	  /* When parsing something like:

		int i(float f, double d)

	     we can tell after seeing the declaration for "f" that we
	     are not looking at an initialization of a variable "i",
	     but rather at the declaration of a function "i".

	     Due to the fact that the parsing of template arguments
	     (as specified to a template-id) requires backtracking we
	     cannot use this technique when inside a template argument
	     list.  */
	  if (!parser->in_template_argument_list_p
	      && !parser->in_type_id_in_expr_p
	      && cp_parser_uncommitted_to_tentative_parse_p (parser)
	      /* However, a parameter-declaration of the form
		 "foat(f)" (which is a valid declaration of a
		 parameter "f") can also be interpreted as an
		 expression (the conversion of "f" to "float").  */
	      && !parenthesized_p)
	    cp_parser_commit_to_tentative_parse (parser);
	}
      else
	{
	  cp_parser_error (parser, "expected %<,%> or %<...%>");
	  if (!cp_parser_uncommitted_to_tentative_parse_p (parser))
	    cp_parser_skip_to_closing_parenthesis (parser,
						   /*recovering=*/true,
						   /*or_comma=*/false,
						   /*consume_paren=*/false);
	  break;
	}
    }

  parser->in_unbraced_linkage_specification_p
    = saved_in_unbraced_linkage_specification_p;

  return parameters;
}

/* Parse a parameter declaration.

   parameter-declaration:
     decl-specifier-seq ... [opt] declarator
     decl-specifier-seq declarator = assignment-expression
     decl-specifier-seq ... [opt] abstract-declarator [opt]
     decl-specifier-seq abstract-declarator [opt] = assignment-expression

   If TEMPLATE_PARM_P is TRUE, then this parameter-declaration
   declares a template parameter.  (In that case, a non-nested `>'
   token encountered during the parsing of the assignment-expression
   is not interpreted as a greater-than operator.)

   Returns a representation of the parameter, or NULL if an error
   occurs.  If PARENTHESIZED_P is non-NULL, *PARENTHESIZED_P is set to
   true iff the declarator is of the form "(p)".  */

static cp_parameter_declarator *
cp_parser_parameter_declaration (cp_parser *parser,
				 bool template_parm_p,
				 bool *parenthesized_p)
{
  int declares_class_or_enum;
  cp_decl_specifier_seq decl_specifiers;
  cp_declarator *declarator;
  tree default_argument;
  cp_token *token = NULL, *declarator_token_start = NULL;
  const char *saved_message;

  /* In a template parameter, `>' is not an operator.

     [temp.param]

     When parsing a default template-argument for a non-type
     template-parameter, the first non-nested `>' is taken as the end
     of the template parameter-list rather than a greater-than
     operator.  */

  /* Type definitions may not appear in parameter types.  */
  saved_message = parser->type_definition_forbidden_message;
  parser->type_definition_forbidden_message
    = G_("types may not be defined in parameter types");

  /* Parse the declaration-specifiers.  */
  cp_parser_decl_specifier_seq (parser,
				CP_PARSER_FLAGS_NONE,
				&decl_specifiers,
				&declares_class_or_enum);

  /* Complain about missing 'typename' or other invalid type names.  */
  if (!decl_specifiers.any_type_specifiers_p)
    cp_parser_parse_and_diagnose_invalid_type_name (parser);

  /* If an error occurred, there's no reason to attempt to parse the
     rest of the declaration.  */
  if (cp_parser_error_occurred (parser))
    {
      parser->type_definition_forbidden_message = saved_message;
      return NULL;
    }

  /* Peek at the next token.  */
  token = cp_lexer_peek_token (parser->lexer);

  /* If the next token is a `)', `,', `=', `>', or `...', then there
     is no declarator. However, when variadic templates are enabled,
     there may be a declarator following `...'.  */
  if (token->type == CPP_CLOSE_PAREN
      || token->type == CPP_COMMA
      || token->type == CPP_EQ
      || token->type == CPP_GREATER)
    {
      declarator = NULL;
      if (parenthesized_p)
	*parenthesized_p = false;
    }
  /* Otherwise, there should be a declarator.  */
  else
    {
      bool saved_default_arg_ok_p = parser->default_arg_ok_p;
      parser->default_arg_ok_p = false;

      /* After seeing a decl-specifier-seq, if the next token is not a
	 "(", there is no possibility that the code is a valid
	 expression.  Therefore, if parsing tentatively, we commit at
	 this point.  */
      if (!parser->in_template_argument_list_p
	  /* In an expression context, having seen:

	       (int((char ...

	     we cannot be sure whether we are looking at a
	     function-type (taking a "char" as a parameter) or a cast
	     of some object of type "char" to "int".  */
	  && !parser->in_type_id_in_expr_p
	  && cp_parser_uncommitted_to_tentative_parse_p (parser)
	  && cp_lexer_next_token_is_not (parser->lexer, CPP_OPEN_PAREN))
	cp_parser_commit_to_tentative_parse (parser);
      /* Parse the declarator.  */
      declarator_token_start = token;
      declarator = cp_parser_declarator (parser,
					 CP_PARSER_DECLARATOR_EITHER,
					 /*ctor_dtor_or_conv_p=*/NULL,
					 parenthesized_p,
					 /*member_p=*/false);
      parser->default_arg_ok_p = saved_default_arg_ok_p;
      /* After the declarator, allow more attributes.  */
      decl_specifiers.attributes
	= chainon (decl_specifiers.attributes,
		   cp_parser_attributes_opt (parser));
    }

  /* If the next token is an ellipsis, and we have not seen a
     declarator name, and the type of the declarator contains parameter
     packs but it is not a TYPE_PACK_EXPANSION, then we actually have
     a parameter pack expansion expression. Otherwise, leave the
     ellipsis for a C-style variadic function. */
  token = cp_lexer_peek_token (parser->lexer);
  if (cp_lexer_next_token_is (parser->lexer, CPP_ELLIPSIS))
    {
      tree type = decl_specifiers.type;

      if (type && DECL_P (type))
        type = TREE_TYPE (type);

      if (type
	  && TREE_CODE (type) != TYPE_PACK_EXPANSION
	  && declarator_can_be_parameter_pack (declarator)
          && (!declarator || !declarator->parameter_pack_p)
          && uses_parameter_packs (type))
        {
	  /* Consume the `...'. */
	  cp_lexer_consume_token (parser->lexer);
	  maybe_warn_variadic_templates ();
	  
	  /* Build a pack expansion type */
	  if (declarator)
	    declarator->parameter_pack_p = true;
	  else
	    decl_specifiers.type = make_pack_expansion (type);
	}
    }

  /* The restriction on defining new types applies only to the type
     of the parameter, not to the default argument.  */
  parser->type_definition_forbidden_message = saved_message;

  /* If the next token is `=', then process a default argument.  */
  if (cp_lexer_next_token_is (parser->lexer, CPP_EQ))
    {
      /* Consume the `='.  */
      cp_lexer_consume_token (parser->lexer);

      /* If we are defining a class, then the tokens that make up the
	 default argument must be saved and processed later.  */
      if (!template_parm_p && at_class_scope_p ()
	  && TYPE_BEING_DEFINED (current_class_type)
	  && !LAMBDA_TYPE_P (current_class_type))
	{
	  unsigned depth = 0;
	  int maybe_template_id = 0;
	  cp_token *first_token;
	  cp_token *token;

	  /* Add tokens until we have processed the entire default
	     argument.  We add the range [first_token, token).  */
	  first_token = cp_lexer_peek_token (parser->lexer);
	  while (true)
	    {
	      bool done = false;

	      /* Peek at the next token.  */
	      token = cp_lexer_peek_token (parser->lexer);
	      /* What we do depends on what token we have.  */
	      switch (token->type)
		{
		  /* In valid code, a default argument must be
		     immediately followed by a `,' `)', or `...'.  */
		case CPP_COMMA:
		  if (depth == 0 && maybe_template_id)
		    {
		      /* If we've seen a '<', we might be in a
			 template-argument-list.  Until Core issue 325 is
			 resolved, we don't know how this situation ought
			 to be handled, so try to DTRT.  We check whether
			 what comes after the comma is a valid parameter
			 declaration list.  If it is, then the comma ends
			 the default argument; otherwise the default
			 argument continues.  */
		      bool error = false;

		      /* Set ITALP so cp_parser_parameter_declaration_list
			 doesn't decide to commit to this parse.  */
		      bool saved_italp = parser->in_template_argument_list_p;
		      parser->in_template_argument_list_p = true;

		      cp_parser_parse_tentatively (parser);
		      cp_lexer_consume_token (parser->lexer);
		      cp_parser_parameter_declaration_list (parser, &error);
		      if (!cp_parser_error_occurred (parser) && !error)
			done = true;
		      cp_parser_abort_tentative_parse (parser);

		      parser->in_template_argument_list_p = saved_italp;
		      break;
		    }
		case CPP_CLOSE_PAREN:
		case CPP_ELLIPSIS:
		  /* If we run into a non-nested `;', `}', or `]',
		     then the code is invalid -- but the default
		     argument is certainly over.  */
		case CPP_SEMICOLON:
		case CPP_CLOSE_BRACE:
		case CPP_CLOSE_SQUARE:
		  if (depth == 0)
		    done = true;
		  /* Update DEPTH, if necessary.  */
		  else if (token->type == CPP_CLOSE_PAREN
			   || token->type == CPP_CLOSE_BRACE
			   || token->type == CPP_CLOSE_SQUARE)
		    --depth;
		  break;

		case CPP_OPEN_PAREN:
		case CPP_OPEN_SQUARE:
		case CPP_OPEN_BRACE:
		  ++depth;
		  break;

		case CPP_LESS:
		  if (depth == 0)
		    /* This might be the comparison operator, or it might
		       start a template argument list.  */
		    ++maybe_template_id;
		  break;

                case CPP_RSHIFT:
                  if (cxx_dialect == cxx98)
                    break;
                  /* Fall through for C++0x, which treats the `>>'
                     operator like two `>' tokens in certain
                     cases.  */

		case CPP_GREATER:
		  if (depth == 0)
		    {
		      /* This might be an operator, or it might close a
			 template argument list.  But if a previous '<'
			 started a template argument list, this will have
			 closed it, so we can't be in one anymore.  */
		      maybe_template_id -= 1 + (token->type == CPP_RSHIFT);
		      if (maybe_template_id < 0)
			maybe_template_id = 0;
		    }
		  break;

		  /* If we run out of tokens, issue an error message.  */
		case CPP_EOF:
		case CPP_PRAGMA_EOL:
		  error_at (token->location, "file ends in default argument");
		  done = true;
		  break;

		case CPP_NAME:
		case CPP_SCOPE:
		  /* In these cases, we should look for template-ids.
		     For example, if the default argument is
		     `X<int, double>()', we need to do name lookup to
		     figure out whether or not `X' is a template; if
		     so, the `,' does not end the default argument.

		     That is not yet done.  */
		  break;

		default:
		  break;
		}

	      /* If we've reached the end, stop.  */
	      if (done)
		break;

	      /* Add the token to the token block.  */
	      token = cp_lexer_consume_token (parser->lexer);
	    }

	  /* Create a DEFAULT_ARG to represent the unparsed default
	     argument.  */
	  default_argument = make_node (DEFAULT_ARG);
	  DEFARG_TOKENS (default_argument)
	    = cp_token_cache_new (first_token, token);
	  DEFARG_INSTANTIATIONS (default_argument) = NULL;
	}
      /* Outside of a class definition, we can just parse the
	 assignment-expression.  */
      else
	{
	  token = cp_lexer_peek_token (parser->lexer);
	  default_argument 
	    = cp_parser_default_argument (parser, template_parm_p);
	}

      if (!parser->default_arg_ok_p)
	{
	  if (flag_permissive)
	    warning (0, "deprecated use of default argument for parameter of non-function");
	  else
	    {
	      error_at (token->location,
			"default arguments are only "
			"permitted for function parameters");
	      default_argument = NULL_TREE;
	    }
	}
      else if ((declarator && declarator->parameter_pack_p)
	       || (decl_specifiers.type
		   && PACK_EXPANSION_P (decl_specifiers.type)))
	{
	  /* Find the name of the parameter pack.  */     
	  cp_declarator *id_declarator = declarator;
	  while (id_declarator && id_declarator->kind != cdk_id)
	    id_declarator = id_declarator->declarator;
	  
	  if (id_declarator && id_declarator->kind == cdk_id)
	    error_at (declarator_token_start->location,
		      template_parm_p 
		      ? "template parameter pack %qD"
		      " cannot have a default argument"
		      : "parameter pack %qD cannot have a default argument",
		      id_declarator->u.id.unqualified_name);
	  else
	    error_at (declarator_token_start->location,
		      template_parm_p 
		      ? "template parameter pack cannot have a default argument"
		      : "parameter pack cannot have a default argument");
	  
	  default_argument = NULL_TREE;
	}
    }
  else
    default_argument = NULL_TREE;

  return make_parameter_declarator (&decl_specifiers,
				    declarator,
				    default_argument);
}

/* Parse a default argument and return it.

   TEMPLATE_PARM_P is true if this is a default argument for a
   non-type template parameter.  */
static tree
cp_parser_default_argument (cp_parser *parser, bool template_parm_p)
{
  tree default_argument = NULL_TREE;
  bool saved_greater_than_is_operator_p;
  bool saved_local_variables_forbidden_p;

  /* Make sure that PARSER->GREATER_THAN_IS_OPERATOR_P is
     set correctly.  */
  saved_greater_than_is_operator_p = parser->greater_than_is_operator_p;
  parser->greater_than_is_operator_p = !template_parm_p;
  /* Local variable names (and the `this' keyword) may not
     appear in a default argument.  */
  saved_local_variables_forbidden_p = parser->local_variables_forbidden_p;
  parser->local_variables_forbidden_p = true;
  /* Parse the assignment-expression.  */
  if (template_parm_p)
    push_deferring_access_checks (dk_no_deferred);
  default_argument
    = cp_parser_assignment_expression (parser, /*cast_p=*/false, NULL);
  if (template_parm_p)
    pop_deferring_access_checks ();
  parser->greater_than_is_operator_p = saved_greater_than_is_operator_p;
  parser->local_variables_forbidden_p = saved_local_variables_forbidden_p;

  return default_argument;
}

/* Parse a function-body.

   function-body:
     compound_statement  */

static void
cp_parser_function_body (cp_parser *parser)
{
  cp_parser_compound_statement (parser, NULL, false);
}

/* Parse a ctor-initializer-opt followed by a function-body.  Return
   true if a ctor-initializer was present.  */

static bool
cp_parser_ctor_initializer_opt_and_function_body (cp_parser *parser)
{
  tree body;
  bool ctor_initializer_p;

  /* Begin the function body.  */
  body = begin_function_body ();
  /* Parse the optional ctor-initializer.  */
  ctor_initializer_p = cp_parser_ctor_initializer_opt (parser);
  /* Parse the function-body.  */
  cp_parser_function_body (parser);
  /* Finish the function body.  */
  finish_function_body (body);

  return ctor_initializer_p;
}

/* Parse an initializer.

   initializer:
     = initializer-clause
     ( expression-list )

   Returns an expression representing the initializer.  If no
   initializer is present, NULL_TREE is returned.

   *IS_DIRECT_INIT is set to FALSE if the `= initializer-clause'
   production is used, and TRUE otherwise.  *IS_DIRECT_INIT is
   set to TRUE if there is no initializer present.  If there is an
   initializer, and it is not a constant-expression, *NON_CONSTANT_P
   is set to true; otherwise it is set to false.  */

static tree
cp_parser_initializer (cp_parser* parser, bool* is_direct_init,
		       bool* non_constant_p)
{
  cp_token *token;
  tree init;

  /* Peek at the next token.  */
  token = cp_lexer_peek_token (parser->lexer);

  /* Let our caller know whether or not this initializer was
     parenthesized.  */
  *is_direct_init = (token->type != CPP_EQ);
  /* Assume that the initializer is constant.  */
  *non_constant_p = false;

  if (token->type == CPP_EQ)
    {
      /* Consume the `='.  */
      cp_lexer_consume_token (parser->lexer);
      /* Parse the initializer-clause.  */
      init = cp_parser_initializer_clause (parser, non_constant_p);
    }
  else if (token->type == CPP_OPEN_PAREN)
    {
      VEC(tree,gc) *vec;
      vec = cp_parser_parenthesized_expression_list (parser, false,
						     /*cast_p=*/false,
						     /*allow_expansion_p=*/true,
						     non_constant_p);
      if (vec == NULL)
	return error_mark_node;
      init = build_tree_list_vec (vec);
      release_tree_vector (vec);
    }
  else if (token->type == CPP_OPEN_BRACE)
    {
      maybe_warn_cpp0x (CPP0X_INITIALIZER_LISTS);
      init = cp_parser_braced_list (parser, non_constant_p);
      CONSTRUCTOR_IS_DIRECT_INIT (init) = 1;
    }
  else
    {
      /* Anything else is an error.  */
      cp_parser_error (parser, "expected initializer");
      init = error_mark_node;
    }

  return init;
}

/* Parse an initializer-clause.

   initializer-clause:
     assignment-expression
     braced-init-list

   Returns an expression representing the initializer.

   If the `assignment-expression' production is used the value
   returned is simply a representation for the expression.

   Otherwise, calls cp_parser_braced_list.  */

static tree
cp_parser_initializer_clause (cp_parser* parser, bool* non_constant_p)
{
  tree initializer;

  /* Assume the expression is constant.  */
  *non_constant_p = false;

  /* If it is not a `{', then we are looking at an
     assignment-expression.  */
  if (cp_lexer_next_token_is_not (parser->lexer, CPP_OPEN_BRACE))
    {
      initializer
	= cp_parser_constant_expression (parser,
					/*allow_non_constant_p=*/true,
					non_constant_p);
      if (!*non_constant_p)
	initializer = fold_non_dependent_expr (initializer);
    }
  else
    initializer = cp_parser_braced_list (parser, non_constant_p);

  return initializer;
}

/* Parse a brace-enclosed initializer list.

   braced-init-list:
     { initializer-list , [opt] }
     { }

   Returns a CONSTRUCTOR.  The CONSTRUCTOR_ELTS will be
   the elements of the initializer-list (or NULL, if the last
   production is used).  The TREE_TYPE for the CONSTRUCTOR will be
   NULL_TREE.  There is no way to detect whether or not the optional
   trailing `,' was provided.  NON_CONSTANT_P is as for
   cp_parser_initializer.  */     

static tree
cp_parser_braced_list (cp_parser* parser, bool* non_constant_p)
{
  tree initializer;

  /* Consume the `{' token.  */
  cp_lexer_consume_token (parser->lexer);
  /* Create a CONSTRUCTOR to represent the braced-initializer.  */
  initializer = make_node (CONSTRUCTOR);
  /* If it's not a `}', then there is a non-trivial initializer.  */
  if (cp_lexer_next_token_is_not (parser->lexer, CPP_CLOSE_BRACE))
    {
      /* Parse the initializer list.  */
      CONSTRUCTOR_ELTS (initializer)
	= cp_parser_initializer_list (parser, non_constant_p);
      /* A trailing `,' token is allowed.  */
      if (cp_lexer_next_token_is (parser->lexer, CPP_COMMA))
	cp_lexer_consume_token (parser->lexer);
    }
  /* Now, there should be a trailing `}'.  */
  cp_parser_require (parser, CPP_CLOSE_BRACE, "%<}%>");
  TREE_TYPE (initializer) = init_list_type_node;
  return initializer;
}

/* Parse an initializer-list.

   initializer-list:
     initializer-clause ... [opt]
     initializer-list , initializer-clause ... [opt]

   GNU Extension:

   initializer-list:
     identifier : initializer-clause
     initializer-list, identifier : initializer-clause

   Returns a VEC of constructor_elt.  The VALUE of each elt is an expression
   for the initializer.  If the INDEX of the elt is non-NULL, it is the
   IDENTIFIER_NODE naming the field to initialize.  NON_CONSTANT_P is
   as for cp_parser_initializer.  */

static VEC(constructor_elt,gc) *
cp_parser_initializer_list (cp_parser* parser, bool* non_constant_p)
{
  VEC(constructor_elt,gc) *v = NULL;

  /* Assume all of the expressions are constant.  */
  *non_constant_p = false;

  /* Parse the rest of the list.  */
  while (true)
    {
      cp_token *token;
      tree identifier;
      tree initializer;
      bool clause_non_constant_p;

      /* If the next token is an identifier and the following one is a
	 colon, we are looking at the GNU designated-initializer
	 syntax.  */
      if (cp_parser_allow_gnu_extensions_p (parser)
	  && cp_lexer_next_token_is (parser->lexer, CPP_NAME)
	  && cp_lexer_peek_nth_token (parser->lexer, 2)->type == CPP_COLON)
	{
	  /* Warn the user that they are using an extension.  */
	  pedwarn (input_location, OPT_pedantic, 
		   "ISO C++ does not allow designated initializers");
	  /* Consume the identifier.  */
	  identifier = cp_lexer_consume_token (parser->lexer)->u.value;
	  /* Consume the `:'.  */
	  cp_lexer_consume_token (parser->lexer);
	}
      else
	identifier = NULL_TREE;

      /* Parse the initializer.  */
      initializer = cp_parser_initializer_clause (parser,
						  &clause_non_constant_p);
      /* If any clause is non-constant, so is the entire initializer.  */
      if (clause_non_constant_p)
	*non_constant_p = true;

      /* If we have an ellipsis, this is an initializer pack
	 expansion.  */
      if (cp_lexer_next_token_is (parser->lexer, CPP_ELLIPSIS))
        {
          /* Consume the `...'.  */
          cp_lexer_consume_token (parser->lexer);

          /* Turn the initializer into an initializer expansion.  */
          initializer = make_pack_expansion (initializer);
        }

      /* Add it to the vector.  */
      CONSTRUCTOR_APPEND_ELT(v, identifier, initializer);

      /* If the next token is not a comma, we have reached the end of
	 the list.  */
      if (cp_lexer_next_token_is_not (parser->lexer, CPP_COMMA))
	break;

      /* Peek at the next token.  */
      token = cp_lexer_peek_nth_token (parser->lexer, 2);
      /* If the next token is a `}', then we're still done.  An
	 initializer-clause can have a trailing `,' after the
	 initializer-list and before the closing `}'.  */
      if (token->type == CPP_CLOSE_BRACE)
	break;

      /* Consume the `,' token.  */
      cp_lexer_consume_token (parser->lexer);
    }

  return v;
}

/* Classes [gram.class] */

/* Parse a class-name.

   class-name:
     identifier
     template-id

   TYPENAME_KEYWORD_P is true iff the `typename' keyword has been used
   to indicate that names looked up in dependent types should be
   assumed to be types.  TEMPLATE_KEYWORD_P is true iff the `template'
   keyword has been used to indicate that the name that appears next
   is a template.  TAG_TYPE indicates the explicit tag given before
   the type name, if any.  If CHECK_DEPENDENCY_P is FALSE, names are
   looked up in dependent scopes.  If CLASS_HEAD_P is TRUE, this class
   is the class being defined in a class-head.

   Returns the TYPE_DECL representing the class.  */

static tree
cp_parser_class_name (cp_parser *parser,
		      bool typename_keyword_p,
		      bool template_keyword_p,
		      enum tag_types tag_type,
		      bool check_dependency_p,
		      bool class_head_p,
		      bool is_declaration)
{
  tree decl;
  tree scope;
  bool typename_p;
  cp_token *token;
  tree identifier = NULL_TREE;

  /* All class-names start with an identifier.  */
  token = cp_lexer_peek_token (parser->lexer);
  if (token->type != CPP_NAME && token->type != CPP_TEMPLATE_ID)
    {
      cp_parser_error (parser, "expected class-name");
      return error_mark_node;
    }

  /* PARSER->SCOPE can be cleared when parsing the template-arguments
     to a template-id, so we save it here.  */
  scope = parser->scope;
  if (scope == error_mark_node)
    return error_mark_node;

  /* Any name names a type if we're following the `typename' keyword
     in a qualified name where the enclosing scope is type-dependent.  */
  typename_p = (typename_keyword_p && scope && TYPE_P (scope)
		&& dependent_type_p (scope));
  /* Handle the common case (an identifier, but not a template-id)
     efficiently.  */
  if (token->type == CPP_NAME
      && !cp_parser_nth_token_starts_template_argument_list_p (parser, 2))
    {
      cp_token *identifier_token;
      bool ambiguous_p;

      /* Look for the identifier.  */
      identifier_token = cp_lexer_peek_token (parser->lexer);
      ambiguous_p = identifier_token->ambiguous_p;
      identifier = cp_parser_identifier (parser);
      /* If the next token isn't an identifier, we are certainly not
	 looking at a class-name.  */
      if (identifier == error_mark_node)
	decl = error_mark_node;
      /* If we know this is a type-name, there's no need to look it
	 up.  */
      else if (typename_p)
	decl = identifier;
      else
	{
	  tree ambiguous_decls;
	  /* If we already know that this lookup is ambiguous, then
	     we've already issued an error message; there's no reason
	     to check again.  */
	  if (ambiguous_p)
	    {
	      cp_parser_simulate_error (parser);
	      return error_mark_node;
	    }
	  /* If the next token is a `::', then the name must be a type
	     name.

	     [basic.lookup.qual]

	     During the lookup for a name preceding the :: scope
	     resolution operator, object, function, and enumerator
	     names are ignored.  */
	  if (cp_lexer_next_token_is (parser->lexer, CPP_SCOPE))
	    tag_type = typename_type;
	  /* Look up the name.  */
	  decl = cp_parser_lookup_name (parser, identifier,
					tag_type,
					/*is_template=*/false,
					/*is_namespace=*/false,
					check_dependency_p,
					&ambiguous_decls,
					identifier_token->location);
	  if (ambiguous_decls)
	    {
	      if (cp_parser_parsing_tentatively (parser))
		cp_parser_simulate_error (parser);
	      return error_mark_node;
	    }
	}
    }
  else
    {
      /* Try a template-id.  */
      decl = cp_parser_template_id (parser, template_keyword_p,
				    check_dependency_p,
				    is_declaration);
      if (decl == error_mark_node)
	return error_mark_node;
    }

  decl = cp_parser_maybe_treat_template_as_class (decl, class_head_p);

  /* If this is a typename, create a TYPENAME_TYPE.  */
  if (typename_p && decl != error_mark_node)
    {
      decl = make_typename_type (scope, decl, typename_type,
				 /*complain=*/tf_error);
      if (decl != error_mark_node)
	decl = TYPE_NAME (decl);
    }

  /* Check to see that it is really the name of a class.  */
  if (TREE_CODE (decl) == TEMPLATE_ID_EXPR
      && TREE_CODE (TREE_OPERAND (decl, 0)) == IDENTIFIER_NODE
      && cp_lexer_next_token_is (parser->lexer, CPP_SCOPE))
    /* Situations like this:

	 template <typename T> struct A {
	   typename T::template X<int>::I i;
	 };

       are problematic.  Is `T::template X<int>' a class-name?  The
       standard does not seem to be definitive, but there is no other
       valid interpretation of the following `::'.  Therefore, those
       names are considered class-names.  */
    {
      decl = make_typename_type (scope, decl, tag_type, tf_error);
      if (decl != error_mark_node)
	decl = TYPE_NAME (decl);
    }
  else if (TREE_CODE (decl) != TYPE_DECL
	   || TREE_TYPE (decl) == error_mark_node
	   || !MAYBE_CLASS_TYPE_P (TREE_TYPE (decl)))
    decl = error_mark_node;

  if (decl == error_mark_node)
    cp_parser_error (parser, "expected class-name");
  else if (identifier && !parser->scope)
    maybe_note_name_used_in_class (identifier, decl);

  return decl;
}

/* Parse a class-specifier.

   class-specifier:
     class-head { member-specification [opt] }

   Returns the TREE_TYPE representing the class.  */

static tree
cp_parser_class_specifier (cp_parser* parser)
{
  tree type;
  tree attributes = NULL_TREE;
  bool nested_name_specifier_p;
  unsigned saved_num_template_parameter_lists;
  bool saved_in_function_body;
  bool saved_in_unbraced_linkage_specification_p;
  tree old_scope = NULL_TREE;
  tree scope = NULL_TREE;
  tree bases;

  push_deferring_access_checks (dk_no_deferred);

  /* Parse the class-head.  */
  type = cp_parser_class_head (parser,
			       &nested_name_specifier_p,
			       &attributes,
			       &bases);
  /* If the class-head was a semantic disaster, skip the entire body
     of the class.  */
  if (!type)
    {
      cp_parser_skip_to_end_of_block_or_statement (parser);
      pop_deferring_access_checks ();
      return error_mark_node;
    }

  /* Look for the `{'.  */
  if (!cp_parser_require (parser, CPP_OPEN_BRACE, "%<{%>"))
    {
      pop_deferring_access_checks ();
      return error_mark_node;
    }

  /* Process the base classes. If they're invalid, skip the 
     entire class body.  */
  if (!xref_basetypes (type, bases))
    {
      /* Consuming the closing brace yields better error messages
         later on.  */
      if (cp_parser_skip_to_closing_brace (parser))
	cp_lexer_consume_token (parser->lexer);
      pop_deferring_access_checks ();
      return error_mark_node;
    }

  /* Issue an error message if type-definitions are forbidden here.  */
  cp_parser_check_type_definition (parser);
  /* Remember that we are defining one more class.  */
  ++parser->num_classes_being_defined;
  /* Inside the class, surrounding template-parameter-lists do not
     apply.  */
  saved_num_template_parameter_lists
    = parser->num_template_parameter_lists;
  parser->num_template_parameter_lists = 0;
  /* We are not in a function body.  */
  saved_in_function_body = parser->in_function_body;
  parser->in_function_body = false;
  /* We are not immediately inside an extern "lang" block.  */
  saved_in_unbraced_linkage_specification_p
    = parser->in_unbraced_linkage_specification_p;
  parser->in_unbraced_linkage_specification_p = false;

  /* Start the class.  */
  if (nested_name_specifier_p)
    {
      scope = CP_DECL_CONTEXT (TYPE_MAIN_DECL (type));
      old_scope = push_inner_scope (scope);
    }
  type = begin_class_definition (type, attributes);

  if (type == error_mark_node)
    /* If the type is erroneous, skip the entire body of the class.  */
    cp_parser_skip_to_closing_brace (parser);
  else
    /* Parse the member-specification.  */
    cp_parser_member_specification_opt (parser);

  /* Look for the trailing `}'.  */
  cp_parser_require (parser, CPP_CLOSE_BRACE, "%<}%>");
  /* Look for trailing attributes to apply to this class.  */
  if (cp_parser_allow_gnu_extensions_p (parser))
    attributes = cp_parser_attributes_opt (parser);
  if (type != error_mark_node)
    type = finish_struct (type, attributes);
  if (nested_name_specifier_p)
    pop_inner_scope (old_scope, scope);
  /* If this class is not itself within the scope of another class,
     then we need to parse the bodies of all of the queued function
     definitions.  Note that the queued functions defined in a class
     are not always processed immediately following the
     class-specifier for that class.  Consider:

       struct A {
	 struct B { void f() { sizeof (A); } };
       };

     If `f' were processed before the processing of `A' were
     completed, there would be no way to compute the size of `A'.
     Note that the nesting we are interested in here is lexical --
     not the semantic nesting given by TYPE_CONTEXT.  In particular,
     for:

       struct A { struct B; };
       struct A::B { void f() { } };

     there is no need to delay the parsing of `A::B::f'.  */
  if (--parser->num_classes_being_defined == 0)
    {
      tree queue_entry;
      tree fn;
      tree class_type = NULL_TREE;
      tree pushed_scope = NULL_TREE;

      /* In a first pass, parse default arguments to the functions.
	 Then, in a second pass, parse the bodies of the functions.
	 This two-phased approach handles cases like:

	    struct S {
	      void f() { g(); }
	      void g(int i = 3);
	    };

	 */
      for (TREE_PURPOSE (parser->unparsed_functions_queues)
	     = nreverse (TREE_PURPOSE (parser->unparsed_functions_queues));
	   (queue_entry = TREE_PURPOSE (parser->unparsed_functions_queues));
	   TREE_PURPOSE (parser->unparsed_functions_queues)
	     = TREE_CHAIN (TREE_PURPOSE (parser->unparsed_functions_queues)))
	{
	  fn = TREE_VALUE (queue_entry);
	  /* If there are default arguments that have not yet been processed,
	     take care of them now.  */
	  if (class_type != TREE_PURPOSE (queue_entry))
	    {
	      if (pushed_scope)
		pop_scope (pushed_scope);
	      class_type = TREE_PURPOSE (queue_entry);
	      pushed_scope = push_scope (class_type);
	    }
	  /* Make sure that any template parameters are in scope.  */
	  maybe_begin_member_template_processing (fn);
	  /* Parse the default argument expressions.  */
	  cp_parser_late_parsing_default_args (parser, fn);
	  /* Remove any template parameters from the symbol table.  */
	  maybe_end_member_template_processing ();
	}
      if (pushed_scope)
	pop_scope (pushed_scope);
      /* Now parse the body of the functions.  */
      for (TREE_VALUE (parser->unparsed_functions_queues)
	     = nreverse (TREE_VALUE (parser->unparsed_functions_queues));
	   (queue_entry = TREE_VALUE (parser->unparsed_functions_queues));
	   TREE_VALUE (parser->unparsed_functions_queues)
	     = TREE_CHAIN (TREE_VALUE (parser->unparsed_functions_queues)))
	{
	  /* Figure out which function we need to process.  */
	  fn = TREE_VALUE (queue_entry);
	  /* Parse the function.  */
	  cp_parser_late_parsing_for_member (parser, fn);
	}
    }

  /* Put back any saved access checks.  */
  pop_deferring_access_checks ();

  /* Restore saved state.  */
  parser->in_function_body = saved_in_function_body;
  parser->num_template_parameter_lists
    = saved_num_template_parameter_lists;
  parser->in_unbraced_linkage_specification_p
    = saved_in_unbraced_linkage_specification_p;

  return type;
}

/* Parse a class-head.

   class-head:
     class-key identifier [opt] base-clause [opt]
     class-key nested-name-specifier identifier base-clause [opt]
     class-key nested-name-specifier [opt] template-id
       base-clause [opt]

   GNU Extensions:
     class-key attributes identifier [opt] base-clause [opt]
     class-key attributes nested-name-specifier identifier base-clause [opt]
     class-key attributes nested-name-specifier [opt] template-id
       base-clause [opt]

   Upon return BASES is initialized to the list of base classes (or
   NULL, if there are none) in the same form returned by
   cp_parser_base_clause.

   Returns the TYPE of the indicated class.  Sets
   *NESTED_NAME_SPECIFIER_P to TRUE iff one of the productions
   involving a nested-name-specifier was used, and FALSE otherwise.

   Returns error_mark_node if this is not a class-head.

   Returns NULL_TREE if the class-head is syntactically valid, but
   semantically invalid in a way that means we should skip the entire
   body of the class.  */

static tree
cp_parser_class_head (cp_parser* parser,
		      bool* nested_name_specifier_p,
		      tree *attributes_p,
		      tree *bases)
{
  tree nested_name_specifier;
  enum tag_types class_key;
  tree id = NULL_TREE;
  tree type = NULL_TREE;
  tree attributes;
  bool template_id_p = false;
  bool qualified_p = false;
  bool invalid_nested_name_p = false;
  bool invalid_explicit_specialization_p = false;
  tree pushed_scope = NULL_TREE;
  unsigned num_templates;
  cp_token *type_start_token = NULL, *nested_name_specifier_token_start = NULL;
  /* Assume no nested-name-specifier will be present.  */
  *nested_name_specifier_p = false;
  /* Assume no template parameter lists will be used in defining the
     type.  */
  num_templates = 0;

  *bases = NULL_TREE;

  /* Look for the class-key.  */
  class_key = cp_parser_class_key (parser);
  if (class_key == none_type)
    return error_mark_node;

  /* Parse the attributes.  */
  attributes = cp_parser_attributes_opt (parser);

  /* If the next token is `::', that is invalid -- but sometimes
     people do try to write:

       struct ::S {};

     Handle this gracefully by accepting the extra qualifier, and then
     issuing an error about it later if this really is a
     class-head.  If it turns out just to be an elaborated type
     specifier, remain silent.  */
  if (cp_parser_global_scope_opt (parser, /*current_scope_valid_p=*/false))
    qualified_p = true;

  push_deferring_access_checks (dk_no_check);

  /* Determine the name of the class.  Begin by looking for an
     optional nested-name-specifier.  */
  nested_name_specifier_token_start = cp_lexer_peek_token (parser->lexer);
  nested_name_specifier
    = cp_parser_nested_name_specifier_opt (parser,
					   /*typename_keyword_p=*/false,
					   /*check_dependency_p=*/false,
					   /*type_p=*/false,
					   /*is_declaration=*/false);
  /* If there was a nested-name-specifier, then there *must* be an
     identifier.  */
  if (nested_name_specifier)
    {
      type_start_token = cp_lexer_peek_token (parser->lexer);
      /* Although the grammar says `identifier', it really means
	 `class-name' or `template-name'.  You are only allowed to
	 define a class that has already been declared with this
	 syntax.

	 The proposed resolution for Core Issue 180 says that wherever
	 you see `class T::X' you should treat `X' as a type-name.

	 It is OK to define an inaccessible class; for example:

	   class A { class B; };
	   class A::B {};

	 We do not know if we will see a class-name, or a
	 template-name.  We look for a class-name first, in case the
	 class-name is a template-id; if we looked for the
	 template-name first we would stop after the template-name.  */
      cp_parser_parse_tentatively (parser);
      type = cp_parser_class_name (parser,
				   /*typename_keyword_p=*/false,
				   /*template_keyword_p=*/false,
				   class_type,
				   /*check_dependency_p=*/false,
				   /*class_head_p=*/true,
				   /*is_declaration=*/false);
      /* If that didn't work, ignore the nested-name-specifier.  */
      if (!cp_parser_parse_definitely (parser))
	{
	  invalid_nested_name_p = true;
	  type_start_token = cp_lexer_peek_token (parser->lexer);
	  id = cp_parser_identifier (parser);
	  if (id == error_mark_node)
	    id = NULL_TREE;
	}
      /* If we could not find a corresponding TYPE, treat this
	 declaration like an unqualified declaration.  */
      if (type == error_mark_node)
	nested_name_specifier = NULL_TREE;
      /* Otherwise, count the number of templates used in TYPE and its
	 containing scopes.  */
      else
	{
	  tree scope;

	  for (scope = TREE_TYPE (type);
	       scope && TREE_CODE (scope) != NAMESPACE_DECL;
	       scope = (TYPE_P (scope)
			? TYPE_CONTEXT (scope)
			: DECL_CONTEXT (scope)))
	    if (TYPE_P (scope)
		&& CLASS_TYPE_P (scope)
		&& CLASSTYPE_TEMPLATE_INFO (scope)
		&& PRIMARY_TEMPLATE_P (CLASSTYPE_TI_TEMPLATE (scope))
		&& !CLASSTYPE_TEMPLATE_SPECIALIZATION (scope))
	      ++num_templates;
	}
    }
  /* Otherwise, the identifier is optional.  */
  else
    {
      /* We don't know whether what comes next is a template-id,
	 an identifier, or nothing at all.  */
      cp_parser_parse_tentatively (parser);
      /* Check for a template-id.  */
      type_start_token = cp_lexer_peek_token (parser->lexer);
      id = cp_parser_template_id (parser,
				  /*template_keyword_p=*/false,
				  /*check_dependency_p=*/true,
				  /*is_declaration=*/true);
      /* If that didn't work, it could still be an identifier.  */
      if (!cp_parser_parse_definitely (parser))
	{
	  if (cp_lexer_next_token_is (parser->lexer, CPP_NAME))
	    {
	      type_start_token = cp_lexer_peek_token (parser->lexer);
	      id = cp_parser_identifier (parser);
	    }
	  else
	    id = NULL_TREE;
	}
      else
	{
	  template_id_p = true;
	  ++num_templates;
	}
    }

  pop_deferring_access_checks ();

  if (id)
    cp_parser_check_for_invalid_template_id (parser, id,
					     type_start_token->location);

  /* If it's not a `:' or a `{' then we can't really be looking at a
     class-head, since a class-head only appears as part of a
     class-specifier.  We have to detect this situation before calling
     xref_tag, since that has irreversible side-effects.  */
  if (!cp_parser_next_token_starts_class_definition_p (parser))
    {
      cp_parser_error (parser, "expected %<{%> or %<:%>");
      return error_mark_node;
    }

  /* At this point, we're going ahead with the class-specifier, even
     if some other problem occurs.  */
  cp_parser_commit_to_tentative_parse (parser);
  /* Issue the error about the overly-qualified name now.  */
  if (qualified_p)
    {
      cp_parser_error (parser,
		       "global qualification of class name is invalid");
      return error_mark_node;
    }
  else if (invalid_nested_name_p)
    {
      cp_parser_error (parser,
		       "qualified name does not name a class");
      return error_mark_node;
    }
  else if (nested_name_specifier)
    {
      tree scope;

      /* Reject typedef-names in class heads.  */
      if (!DECL_IMPLICIT_TYPEDEF_P (type))
	{
	  error_at (type_start_token->location,
		    "invalid class name in declaration of %qD",
		    type);
	  type = NULL_TREE;
	  goto done;
	}

      /* Figure out in what scope the declaration is being placed.  */
      scope = current_scope ();
      /* If that scope does not contain the scope in which the
	 class was originally declared, the program is invalid.  */
      if (scope && !is_ancestor (scope, nested_name_specifier))
	{
	  if (at_namespace_scope_p ())
	    error_at (type_start_token->location,
		      "declaration of %qD in namespace %qD which does not "
		      "enclose %qD",
		      type, scope, nested_name_specifier);
	  else
	    error_at (type_start_token->location,
		      "declaration of %qD in %qD which does not enclose %qD",
		      type, scope, nested_name_specifier);
	  type = NULL_TREE;
	  goto done;
	}
      /* [dcl.meaning]

	 A declarator-id shall not be qualified except for the
	 definition of a ... nested class outside of its class
	 ... [or] the definition or explicit instantiation of a
	 class member of a namespace outside of its namespace.  */
      if (scope == nested_name_specifier)
	{
	  permerror (nested_name_specifier_token_start->location,
		     "extra qualification not allowed");
	  nested_name_specifier = NULL_TREE;
	  num_templates = 0;
	}
    }
  /* An explicit-specialization must be preceded by "template <>".  If
     it is not, try to recover gracefully.  */
  if (at_namespace_scope_p ()
      && parser->num_template_parameter_lists == 0
      && template_id_p)
    {
      error_at (type_start_token->location,
		"an explicit specialization must be preceded by %<template <>%>");
      invalid_explicit_specialization_p = true;
      /* Take the same action that would have been taken by
	 cp_parser_explicit_specialization.  */
      ++parser->num_template_parameter_lists;
      begin_specialization ();
    }
  /* There must be no "return" statements between this point and the
     end of this function; set "type "to the correct return value and
     use "goto done;" to return.  */
  /* Make sure that the right number of template parameters were
     present.  */
  if (!cp_parser_check_template_parameters (parser, num_templates,
					    type_start_token->location,
					    /*declarator=*/NULL))
    {
      /* If something went wrong, there is no point in even trying to
	 process the class-definition.  */
      type = NULL_TREE;
      goto done;
    }

  /* Look up the type.  */
  if (template_id_p)
    {
      if (TREE_CODE (id) == TEMPLATE_ID_EXPR
	  && (DECL_FUNCTION_TEMPLATE_P (TREE_OPERAND (id, 0))
	      || TREE_CODE (TREE_OPERAND (id, 0)) == OVERLOAD))
	{
	  error_at (type_start_token->location,
		    "function template %qD redeclared as a class template", id);
	  type = error_mark_node;
	}
      else
	{
	  type = TREE_TYPE (id);
	  type = maybe_process_partial_specialization (type);
	}
      if (nested_name_specifier)
	pushed_scope = push_scope (nested_name_specifier);
    }
  else if (nested_name_specifier)
    {
      tree class_type;

      /* Given:

	    template <typename T> struct S { struct T };
	    template <typename T> struct S<T>::T { };

	 we will get a TYPENAME_TYPE when processing the definition of
	 `S::T'.  We need to resolve it to the actual type before we
	 try to define it.  */
      if (TREE_CODE (TREE_TYPE (type)) == TYPENAME_TYPE)
	{
	  class_type = resolve_typename_type (TREE_TYPE (type),
					      /*only_current_p=*/false);
	  if (TREE_CODE (class_type) != TYPENAME_TYPE)
	    type = TYPE_NAME (class_type);
	  else
	    {
	      cp_parser_error (parser, "could not resolve typename type");
	      type = error_mark_node;
	    }
	}

      if (maybe_process_partial_specialization (TREE_TYPE (type))
	  == error_mark_node)
	{
	  type = NULL_TREE;
	  goto done;
	}

      class_type = current_class_type;
      /* Enter the scope indicated by the nested-name-specifier.  */
      pushed_scope = push_scope (nested_name_specifier);
      /* Get the canonical version of this type.  */
      type = TYPE_MAIN_DECL (TREE_TYPE (type));
      if (PROCESSING_REAL_TEMPLATE_DECL_P ()
	  && !CLASSTYPE_TEMPLATE_SPECIALIZATION (TREE_TYPE (type)))
	{
	  type = push_template_decl (type);
	  if (type == error_mark_node)
	    {
	      type = NULL_TREE;
	      goto done;
	    }
	}

      type = TREE_TYPE (type);
      *nested_name_specifier_p = true;
    }
  else      /* The name is not a nested name.  */
    {
      /* If the class was unnamed, create a dummy name.  */
      if (!id)
	id = make_anon_name ();
      type = xref_tag (class_key, id, /*tag_scope=*/ts_current,
		       parser->num_template_parameter_lists);
    }

  /* Indicate whether this class was declared as a `class' or as a
     `struct'.  */
  if (TREE_CODE (type) == RECORD_TYPE)
    CLASSTYPE_DECLARED_CLASS (type) = (class_key == class_type);
  cp_parser_check_class_key (class_key, type);

  /* If this type was already complete, and we see another definition,
     that's an error.  */
  if (type != error_mark_node && COMPLETE_TYPE_P (type))
    {
      error_at (type_start_token->location, "redefinition of %q#T",
		type);
      error_at (type_start_token->location, "previous definition of %q+#T",
		type);
      type = NULL_TREE;
      goto done;
    }
  else if (type == error_mark_node)
    type = NULL_TREE;

  /* We will have entered the scope containing the class; the names of
     base classes should be looked up in that context.  For example:

       struct A { struct B {}; struct C; };
       struct A::C : B {};

     is valid.  */

  /* Get the list of base-classes, if there is one.  */
  if (cp_lexer_next_token_is (parser->lexer, CPP_COLON))
    *bases = cp_parser_base_clause (parser);

 done:
  /* Leave the scope given by the nested-name-specifier.  We will
     enter the class scope itself while processing the members.  */
  if (pushed_scope)
    pop_scope (pushed_scope);

  if (invalid_explicit_specialization_p)
    {
      end_specialization ();
      --parser->num_template_parameter_lists;
    }
  *attributes_p = attributes;
  return type;
}

/* Parse a class-key.

   class-key:
     class
     struct
     union

   Returns the kind of class-key specified, or none_type to indicate
   error.  */

static enum tag_types
cp_parser_class_key (cp_parser* parser)
{
  cp_token *token;
  enum tag_types tag_type;

  /* Look for the class-key.  */
  token = cp_parser_require (parser, CPP_KEYWORD, "class-key");
  if (!token)
    return none_type;

  /* Check to see if the TOKEN is a class-key.  */
  tag_type = cp_parser_token_is_class_key (token);
  if (!tag_type)
    cp_parser_error (parser, "expected class-key");
  return tag_type;
}

/* Parse an (optional) member-specification.

   member-specification:
     member-declaration member-specification [opt]
     access-specifier : member-specification [opt]  */

static void
cp_parser_member_specification_opt (cp_parser* parser)
{
  while (true)
    {
      cp_token *token;
      enum rid keyword;

      /* Peek at the next token.  */
      token = cp_lexer_peek_token (parser->lexer);
      /* If it's a `}', or EOF then we've seen all the members.  */
      if (token->type == CPP_CLOSE_BRACE
	  || token->type == CPP_EOF
	  || token->type == CPP_PRAGMA_EOL)
	break;

      /* See if this token is a keyword.  */
      keyword = token->keyword;
      switch (keyword)
	{
	case RID_PUBLIC:
	case RID_PROTECTED:
	case RID_PRIVATE:
	  /* Consume the access-specifier.  */
	  cp_lexer_consume_token (parser->lexer);
	  /* Remember which access-specifier is active.  */
	  current_access_specifier = token->u.value;
	  /* Look for the `:'.  */
	  cp_parser_require (parser, CPP_COLON, "%<:%>");
	  break;

	default:
	  /* Accept #pragmas at class scope.  */
	  if (token->type == CPP_PRAGMA)
	    {
	      cp_parser_pragma (parser, pragma_external);
	      break;
	    }

	  /* Otherwise, the next construction must be a
	     member-declaration.  */
	  cp_parser_member_declaration (parser);
	}
    }
}

/* Parse a member-declaration.

   member-declaration:
     decl-specifier-seq [opt] member-declarator-list [opt] ;
     function-definition ; [opt]
     :: [opt] nested-name-specifier template [opt] unqualified-id ;
     using-declaration
     template-declaration

   member-declarator-list:
     member-declarator
     member-declarator-list , member-declarator

   member-declarator:
     declarator pure-specifier [opt]
     declarator constant-initializer [opt]
     identifier [opt] : constant-expression

   GNU Extensions:

   member-declaration:
     __extension__ member-declaration

   member-declarator:
     declarator attributes [opt] pure-specifier [opt]
     declarator attributes [opt] constant-initializer [opt]
     identifier [opt] attributes [opt] : constant-expression  

   C++0x Extensions:

   member-declaration:
     static_assert-declaration  */

static void
cp_parser_member_declaration (cp_parser* parser)
{
  cp_decl_specifier_seq decl_specifiers;
  tree prefix_attributes;
  tree decl;
  int declares_class_or_enum;
  bool friend_p;
  cp_token *token = NULL;
  cp_token *decl_spec_token_start = NULL;
  cp_token *initializer_token_start = NULL;
  int saved_pedantic;

  /* Check for the `__extension__' keyword.  */
  if (cp_parser_extension_opt (parser, &saved_pedantic))
    {
      /* Recurse.  */
      cp_parser_member_declaration (parser);
      /* Restore the old value of the PEDANTIC flag.  */
      pedantic = saved_pedantic;

      return;
    }

  /* Check for a template-declaration.  */
  if (cp_lexer_next_token_is_keyword (parser->lexer, RID_TEMPLATE))
    {
      /* An explicit specialization here is an error condition, and we
	 expect the specialization handler to detect and report this.  */
      if (cp_lexer_peek_nth_token (parser->lexer, 2)->type == CPP_LESS
	  && cp_lexer_peek_nth_token (parser->lexer, 3)->type == CPP_GREATER)
	cp_parser_explicit_specialization (parser);
      else
	cp_parser_template_declaration (parser, /*member_p=*/true);

      return;
    }

  /* Check for a using-declaration.  */
  if (cp_lexer_next_token_is_keyword (parser->lexer, RID_USING))
    {
      /* Parse the using-declaration.  */
      cp_parser_using_declaration (parser,
				   /*access_declaration_p=*/false);
      return;
    }

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

  /* If the next token is `static_assert' we have a static assertion.  */
  if (cp_lexer_next_token_is_keyword (parser->lexer, RID_STATIC_ASSERT))
    {
      cp_parser_static_assert (parser, /*member_p=*/true);
      return;
    }

  if (cp_parser_using_declaration (parser, /*access_declaration=*/true))
    return;

  /* Parse the decl-specifier-seq.  */
  decl_spec_token_start = cp_lexer_peek_token (parser->lexer);
  cp_parser_decl_specifier_seq (parser,
				CP_PARSER_FLAGS_OPTIONAL,
				&decl_specifiers,
				&declares_class_or_enum);
  prefix_attributes = decl_specifiers.attributes;
  decl_specifiers.attributes = NULL_TREE;
  /* Check for an invalid type-name.  */
  if (!decl_specifiers.any_type_specifiers_p
      && cp_parser_parse_and_diagnose_invalid_type_name (parser))
    return;
  /* If there is no declarator, then the decl-specifier-seq should
     specify a type.  */
  if (cp_lexer_next_token_is (parser->lexer, CPP_SEMICOLON))
    {
      /* If there was no decl-specifier-seq, and the next token is a
	 `;', then we have something like:

	   struct S { ; };

	 [class.mem]

	 Each member-declaration shall declare at least one member
	 name of the class.  */
      if (!decl_specifiers.any_specifiers_p)
	{
	  cp_token *token = cp_lexer_peek_token (parser->lexer);
	  if (!in_system_header_at (token->location))
	    pedwarn (token->location, OPT_pedantic, "extra %<;%>");
	}
      else
	{
	  tree type;

	  /* See if this declaration is a friend.  */
	  friend_p = cp_parser_friend_p (&decl_specifiers);
	  /* If there were decl-specifiers, check to see if there was
	     a class-declaration.  */
	  type = check_tag_decl (&decl_specifiers);
	  /* Nested classes have already been added to the class, but
	     a `friend' needs to be explicitly registered.  */
	  if (friend_p)
	    {
	      /* If the `friend' keyword was present, the friend must
		 be introduced with a class-key.  */
	       if (!declares_class_or_enum)
		 error_at (decl_spec_token_start->location,
			   "a class-key must be used when declaring a friend");
	       /* In this case:

		    template <typename T> struct A {
		      friend struct A<T>::B;
		    };

		  A<T>::B will be represented by a TYPENAME_TYPE, and
		  therefore not recognized by check_tag_decl.  */
	       if (!type
		   && decl_specifiers.type
		   && TYPE_P (decl_specifiers.type))
		 type = decl_specifiers.type;
	       if (!type || !TYPE_P (type))
		 error_at (decl_spec_token_start->location,
			   "friend declaration does not name a class or "
			   "function");
	       else
		 make_friend_class (current_class_type, type,
				    /*complain=*/true);
	    }
	  /* If there is no TYPE, an error message will already have
	     been issued.  */
	  else if (!type || type == error_mark_node)
	    ;
	  /* An anonymous aggregate has to be handled specially; such
	     a declaration really declares a data member (with a
	     particular type), as opposed to a nested class.  */
	  else if (ANON_AGGR_TYPE_P (type))
	    {
	      /* Remove constructors and such from TYPE, now that we
		 know it is an anonymous aggregate.  */
	      fixup_anonymous_aggr (type);
	      /* And make the corresponding data member.  */
	      decl = build_decl (decl_spec_token_start->location,
				 FIELD_DECL, NULL_TREE, type);
	      /* Add it to the class.  */
	      finish_member_declaration (decl);
	    }
	  else
	    cp_parser_check_access_in_redeclaration
					      (TYPE_NAME (type),
					       decl_spec_token_start->location);
	}
    }
  else
    {
      /* See if these declarations will be friends.  */
      friend_p = cp_parser_friend_p (&decl_specifiers);

      /* Keep going until we hit the `;' at the end of the
	 declaration.  */
      while (cp_lexer_next_token_is_not (parser->lexer, CPP_SEMICOLON))
	{
	  tree attributes = NULL_TREE;
	  tree first_attribute;

	  /* Peek at the next token.  */
	  token = cp_lexer_peek_token (parser->lexer);

	  /* Check for a bitfield declaration.  */
	  if (token->type == CPP_COLON
	      || (token->type == CPP_NAME
		  && cp_lexer_peek_nth_token (parser->lexer, 2)->type
		  == CPP_COLON))
	    {
	      tree identifier;
	      tree width;

	      /* Get the name of the bitfield.  Note that we cannot just
		 check TOKEN here because it may have been invalidated by
		 the call to cp_lexer_peek_nth_token above.  */
	      if (cp_lexer_peek_token (parser->lexer)->type != CPP_COLON)
		identifier = cp_parser_identifier (parser);
	      else
		identifier = NULL_TREE;

	      /* Consume the `:' token.  */
	      cp_lexer_consume_token (parser->lexer);
	      /* Get the width of the bitfield.  */
	      width
		= cp_parser_constant_expression (parser,
						 /*allow_non_constant=*/false,
						 NULL);

	      /* Look for attributes that apply to the bitfield.  */
	      attributes = cp_parser_attributes_opt (parser);
	      /* Remember which attributes are prefix attributes and
		 which are not.  */
	      first_attribute = attributes;
	      /* Combine the attributes.  */
	      attributes = chainon (prefix_attributes, attributes);

	      /* Create the bitfield declaration.  */
	      decl = grokbitfield (identifier
				   ? make_id_declarator (NULL_TREE,
							 identifier,
							 sfk_none)
				   : NULL,
				   &decl_specifiers,
				   width,
				   attributes);
	    }
	  else
	    {
	      cp_declarator *declarator;
	      tree initializer;
	      tree asm_specification;
	      int ctor_dtor_or_conv_p;

	      /* Parse the declarator.  */
	      declarator
		= cp_parser_declarator (parser, CP_PARSER_DECLARATOR_NAMED,
					&ctor_dtor_or_conv_p,
					/*parenthesized_p=*/NULL,
					/*member_p=*/true);

	      /* If something went wrong parsing the declarator, make sure
		 that we at least consume some tokens.  */
	      if (declarator == cp_error_declarator)
		{
		  /* Skip to the end of the statement.  */
		  cp_parser_skip_to_end_of_statement (parser);
		  /* If the next token is not a semicolon, that is
		     probably because we just skipped over the body of
		     a function.  So, we consume a semicolon if
		     present, but do not issue an error message if it
		     is not present.  */
		  if (cp_lexer_next_token_is (parser->lexer,
					      CPP_SEMICOLON))
		    cp_lexer_consume_token (parser->lexer);
		  return;
		}

	      if (declares_class_or_enum & 2)
		cp_parser_check_for_definition_in_return_type
					    (declarator, decl_specifiers.type,
					     decl_specifiers.type_location);

	      /* Look for an asm-specification.  */
	      asm_specification = cp_parser_asm_specification_opt (parser);
	      /* Look for attributes that apply to the declaration.  */
	      attributes = cp_parser_attributes_opt (parser);
	      /* Remember which attributes are prefix attributes and
		 which are not.  */
	      first_attribute = attributes;
	      /* Combine the attributes.  */
	      attributes = chainon (prefix_attributes, attributes);

	      /* If it's an `=', then we have a constant-initializer or a
		 pure-specifier.  It is not correct to parse the
		 initializer before registering the member declaration
		 since the member declaration should be in scope while
		 its initializer is processed.  However, the rest of the
		 front end does not yet provide an interface that allows
		 us to handle this correctly.  */
	      if (cp_lexer_next_token_is (parser->lexer, CPP_EQ))
		{
		  /* In [class.mem]:

		     A pure-specifier shall be used only in the declaration of
		     a virtual function.

		     A member-declarator can contain a constant-initializer
		     only if it declares a static member of integral or
		     enumeration type.

		     Therefore, if the DECLARATOR is for a function, we look
		     for a pure-specifier; otherwise, we look for a
		     constant-initializer.  When we call `grokfield', it will
		     perform more stringent semantics checks.  */
		  initializer_token_start = cp_lexer_peek_token (parser->lexer);
		  if (function_declarator_p (declarator))
		    initializer = cp_parser_pure_specifier (parser);
		  else
		    /* Parse the initializer.  */
		    initializer = cp_parser_constant_initializer (parser);
		}
	      /* Otherwise, there is no initializer.  */
	      else
		initializer = NULL_TREE;

	      /* See if we are probably looking at a function
		 definition.  We are certainly not looking at a
		 member-declarator.  Calling `grokfield' has
		 side-effects, so we must not do it unless we are sure
		 that we are looking at a member-declarator.  */
	      if (cp_parser_token_starts_function_definition_p
		  (cp_lexer_peek_token (parser->lexer)))
		{
		  /* The grammar does not allow a pure-specifier to be
		     used when a member function is defined.  (It is
		     possible that this fact is an oversight in the
		     standard, since a pure function may be defined
		     outside of the class-specifier.  */
		  if (initializer)
		    error_at (initializer_token_start->location,
			      "pure-specifier on function-definition");
		  decl = cp_parser_save_member_function_body (parser,
							      &decl_specifiers,
							      declarator,
							      attributes);
		  /* If the member was not a friend, declare it here.  */
		  if (!friend_p)
		    finish_member_declaration (decl);
		  /* Peek at the next token.  */
		  token = cp_lexer_peek_token (parser->lexer);
		  /* If the next token is a semicolon, consume it.  */
		  if (token->type == CPP_SEMICOLON)
		    cp_lexer_consume_token (parser->lexer);
		  return;
		}
	      else
		if (declarator->kind == cdk_function)
		  declarator->id_loc = token->location;
		/* Create the declaration.  */
		decl = grokfield (declarator, &decl_specifiers,
				  initializer, /*init_const_expr_p=*/true,
				  asm_specification,
				  attributes);
	    }

	  /* Reset PREFIX_ATTRIBUTES.  */
	  while (attributes && TREE_CHAIN (attributes) != first_attribute)
	    attributes = TREE_CHAIN (attributes);
	  if (attributes)
	    TREE_CHAIN (attributes) = NULL_TREE;

	  /* If there is any qualification still in effect, clear it
	     now; we will be starting fresh with the next declarator.  */
	  parser->scope = NULL_TREE;
	  parser->qualifying_scope = NULL_TREE;
	  parser->object_scope = NULL_TREE;
	  /* If it's a `,', then there are more declarators.  */
	  if (cp_lexer_next_token_is (parser->lexer, CPP_COMMA))
	    cp_lexer_consume_token (parser->lexer);
	  /* If the next token isn't a `;', then we have a parse error.  */
	  else if (cp_lexer_next_token_is_not (parser->lexer,
					       CPP_SEMICOLON))
	    {
	      cp_parser_error (parser, "expected %<;%>");
	      /* Skip tokens until we find a `;'.  */
	      cp_parser_skip_to_end_of_statement (parser);

	      break;
	    }

	  if (decl)
	    {
	      /* Add DECL to the list of members.  */
	      if (!friend_p)
		finish_member_declaration (decl);

	      if (TREE_CODE (decl) == FUNCTION_DECL)
		cp_parser_save_default_args (parser, decl);
	    }
	}
    }

  cp_parser_require (parser, CPP_SEMICOLON, "%<;%>");
}

/* Parse a pure-specifier.

   pure-specifier:
     = 0

   Returns INTEGER_ZERO_NODE if a pure specifier is found.
   Otherwise, ERROR_MARK_NODE is returned.  */

static tree
cp_parser_pure_specifier (cp_parser* parser)
{
  cp_token *token;

  /* Look for the `=' token.  */
  if (!cp_parser_require (parser, CPP_EQ, "%<=%>"))
    return error_mark_node;
  /* Look for the `0' token.  */
  token = cp_lexer_peek_token (parser->lexer);

  if (token->type == CPP_EOF
      || token->type == CPP_PRAGMA_EOL)
    return error_mark_node;

  cp_lexer_consume_token (parser->lexer);

  /* Accept = default or = delete in c++0x mode.  */
  if (token->keyword == RID_DEFAULT
      || token->keyword == RID_DELETE)
    {
      maybe_warn_cpp0x (CPP0X_DEFAULTED_DELETED);
      return token->u.value;
    }

  /* c_lex_with_flags marks a single digit '0' with PURE_ZERO.  */
  if (token->type != CPP_NUMBER || !(token->flags & PURE_ZERO))
    {
      cp_parser_error (parser,
		       "invalid pure specifier (only %<= 0%> is allowed)");
      cp_parser_skip_to_end_of_statement (parser);
      return error_mark_node;
    }
  if (PROCESSING_REAL_TEMPLATE_DECL_P ())
    {
      error_at (token->location, "templates may not be %<virtual%>");
      return error_mark_node;
    }

  return integer_zero_node;
}

/* Parse a constant-initializer.

   constant-initializer:
     = constant-expression

   Returns a representation of the constant-expression.  */

static tree
cp_parser_constant_initializer (cp_parser* parser)
{
  /* Look for the `=' token.  */
  if (!cp_parser_require (parser, CPP_EQ, "%<=%>"))
    return error_mark_node;

  /* It is invalid to write:

       struct S { static const int i = { 7 }; };

     */
  if (cp_lexer_next_token_is (parser->lexer, CPP_OPEN_BRACE))
    {
      cp_parser_error (parser,
		       "a brace-enclosed initializer is not allowed here");
      /* Consume the opening brace.  */
      cp_lexer_consume_token (parser->lexer);
      /* Skip the initializer.  */
      cp_parser_skip_to_closing_brace (parser);
      /* Look for the trailing `}'.  */
      cp_parser_require (parser, CPP_CLOSE_BRACE, "%<}%>");

      return error_mark_node;
    }

  return cp_parser_constant_expression (parser,
					/*allow_non_constant=*/false,
					NULL);
}

/* Derived classes [gram.class.derived] */

/* Parse a base-clause.

   base-clause:
     : base-specifier-list

   base-specifier-list:
     base-specifier ... [opt]
     base-specifier-list , base-specifier ... [opt]

   Returns a TREE_LIST representing the base-classes, in the order in
   which they were declared.  The representation of each node is as
   described by cp_parser_base_specifier.

   In the case that no bases are specified, this function will return
   NULL_TREE, not ERROR_MARK_NODE.  */

static tree
cp_parser_base_clause (cp_parser* parser)
{
  tree bases = NULL_TREE;

  /* Look for the `:' that begins the list.  */
  cp_parser_require (parser, CPP_COLON, "%<:%>");

  /* Scan the base-specifier-list.  */
  while (true)
    {
      cp_token *token;
      tree base;
      bool pack_expansion_p = false;

      /* Look for the base-specifier.  */
      base = cp_parser_base_specifier (parser);
      /* Look for the (optional) ellipsis. */
      if (cp_lexer_next_token_is (parser->lexer, CPP_ELLIPSIS))
        {
          /* Consume the `...'. */
          cp_lexer_consume_token (parser->lexer);

          pack_expansion_p = true;
        }

      /* Add BASE to the front of the list.  */
      if (base != error_mark_node)
	{
          if (pack_expansion_p)
            /* Make this a pack expansion type. */
            TREE_VALUE (base) = make_pack_expansion (TREE_VALUE (base));
          

          if (!check_for_bare_parameter_packs (TREE_VALUE (base)))
            {
              TREE_CHAIN (base) = bases;
              bases = base;
            }
	}
      /* Peek at the next token.  */
      token = cp_lexer_peek_token (parser->lexer);
      /* If it's not a comma, then the list is complete.  */
      if (token->type != CPP_COMMA)
	break;
      /* Consume the `,'.  */
      cp_lexer_consume_token (parser->lexer);
    }

  /* PARSER->SCOPE may still be non-NULL at this point, if the last
     base class had a qualified name.  However, the next name that
     appears is certainly not qualified.  */
  parser->scope = NULL_TREE;
  parser->qualifying_scope = NULL_TREE;
  parser->object_scope = NULL_TREE;

  return nreverse (bases);
}

/* Parse a base-specifier.

   base-specifier:
     :: [opt] nested-name-specifier [opt] class-name
     virtual access-specifier [opt] :: [opt] nested-name-specifier
       [opt] class-name
     access-specifier virtual [opt] :: [opt] nested-name-specifier
       [opt] class-name

   Returns a TREE_LIST.  The TREE_PURPOSE will be one of
   ACCESS_{DEFAULT,PUBLIC,PROTECTED,PRIVATE}_[VIRTUAL]_NODE to
   indicate the specifiers provided.  The TREE_VALUE will be a TYPE
   (or the ERROR_MARK_NODE) indicating the type that was specified.  */

static tree
cp_parser_base_specifier (cp_parser* parser)
{
  cp_token *token;
  bool done = false;
  bool virtual_p = false;
  bool duplicate_virtual_error_issued_p = false;
  bool duplicate_access_error_issued_p = false;
  bool class_scope_p, template_p;
  tree access = access_default_node;
  tree type;

  /* Process the optional `virtual' and `access-specifier'.  */
  while (!done)
    {
      /* Peek at the next token.  */
      token = cp_lexer_peek_token (parser->lexer);
      /* Process `virtual'.  */
      switch (token->keyword)
	{
	case RID_VIRTUAL:
	  /* If `virtual' appears more than once, issue an error.  */
	  if (virtual_p && !duplicate_virtual_error_issued_p)
	    {
	      cp_parser_error (parser,
			       "%<virtual%> specified more than once in base-specified");
	      duplicate_virtual_error_issued_p = true;
	    }

	  virtual_p = true;

	  /* Consume the `virtual' token.  */
	  cp_lexer_consume_token (parser->lexer);

	  break;

	case RID_PUBLIC:
	case RID_PROTECTED:
	case RID_PRIVATE:
	  /* If more than one access specifier appears, issue an
	     error.  */
	  if (access != access_default_node
	      && !duplicate_access_error_issued_p)
	    {
	      cp_parser_error (parser,
			       "more than one access specifier in base-specified");
	      duplicate_access_error_issued_p = true;
	    }

	  access = ridpointers[(int) token->keyword];

	  /* Consume the access-specifier.  */
	  cp_lexer_consume_token (parser->lexer);

	  break;

	default:
	  done = true;
	  break;
	}
    }
  /* It is not uncommon to see programs mechanically, erroneously, use
     the 'typename' keyword to denote (dependent) qualified types
     as base classes.  */
  if (cp_lexer_next_token_is_keyword (parser->lexer, RID_TYPENAME))
    {
      token = cp_lexer_peek_token (parser->lexer);
      if (!processing_template_decl)
	error_at (token->location,
		  "keyword %<typename%> not allowed outside of templates");
      else
	error_at (token->location,
		  "keyword %<typename%> not allowed in this context "
		  "(the base class is implicitly a type)");
      cp_lexer_consume_token (parser->lexer);
    }

  /* Look for the optional `::' operator.  */
  cp_parser_global_scope_opt (parser, /*current_scope_valid_p=*/false);
  /* Look for the nested-name-specifier.  The simplest way to
     implement:

       [temp.res]

       The keyword `typename' is not permitted in a base-specifier or
       mem-initializer; in these contexts a qualified name that
       depends on a template-parameter is implicitly assumed to be a
       type name.

     is to pretend that we have seen the `typename' keyword at this
     point.  */
  cp_parser_nested_name_specifier_opt (parser,
				       /*typename_keyword_p=*/true,
				       /*check_dependency_p=*/true,
				       typename_type,
				       /*is_declaration=*/true);
  /* If the base class is given by a qualified name, assume that names
     we see are type names or templates, as appropriate.  */
  class_scope_p = (parser->scope && TYPE_P (parser->scope));
  template_p = class_scope_p && cp_parser_optional_template_keyword (parser);

  /* Finally, look for the class-name.  */
  type = cp_parser_class_name (parser,
			       class_scope_p,
			       template_p,
			       typename_type,
			       /*check_dependency_p=*/true,
			       /*class_head_p=*/false,
			       /*is_declaration=*/true);

  if (type == error_mark_node)
    return error_mark_node;

  return finish_base_specifier (TREE_TYPE (type), access, virtual_p);
}

/* Exception handling [gram.exception] */

/* Parse an (optional) exception-specification.

   exception-specification:
     throw ( type-id-list [opt] )

   Returns a TREE_LIST representing the exception-specification.  The
   TREE_VALUE of each node is a type.  */

static tree
cp_parser_exception_specification_opt (cp_parser* parser)
{
  cp_token *token;
  tree type_id_list;

  /* Peek at the next token.  */
  token = cp_lexer_peek_token (parser->lexer);
  /* If it's not `throw', then there's no exception-specification.  */
  if (!cp_parser_is_keyword (token, RID_THROW))
    return NULL_TREE;

  /* Consume the `throw'.  */
  cp_lexer_consume_token (parser->lexer);

  /* Look for the `('.  */
  cp_parser_require (parser, CPP_OPEN_PAREN, "%<(%>");

  /* Peek at the next token.  */
  token = cp_lexer_peek_token (parser->lexer);
  /* If it's not a `)', then there is a type-id-list.  */
  if (token->type != CPP_CLOSE_PAREN)
    {
      const char *saved_message;

      /* Types may not be defined in an exception-specification.  */
      saved_message = parser->type_definition_forbidden_message;
      parser->type_definition_forbidden_message
	= G_("types may not be defined in an exception-specification");
      /* Parse the type-id-list.  */
      type_id_list = cp_parser_type_id_list (parser);
      /* Restore the saved message.  */
      parser->type_definition_forbidden_message = saved_message;
    }
  else
    type_id_list = empty_except_spec;

  /* Look for the `)'.  */
  cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>");

  return type_id_list;
}

/* Parse an (optional) type-id-list.

   type-id-list:
     type-id ... [opt]
     type-id-list , type-id ... [opt]

   Returns a TREE_LIST.  The TREE_VALUE of each node is a TYPE,
   in the order that the types were presented.  */

static tree
cp_parser_type_id_list (cp_parser* parser)
{
  tree types = NULL_TREE;

  while (true)
    {
      cp_token *token;
      tree type;

      /* Get the next type-id.  */
      type = cp_parser_type_id (parser);
      /* Parse the optional ellipsis. */
      if (cp_lexer_next_token_is (parser->lexer, CPP_ELLIPSIS))
        {
          /* Consume the `...'. */
          cp_lexer_consume_token (parser->lexer);

          /* Turn the type into a pack expansion expression. */
          type = make_pack_expansion (type);
        }
      /* Add it to the list.  */
      types = add_exception_specifier (types, type, /*complain=*/1);
      /* Peek at the next token.  */
      token = cp_lexer_peek_token (parser->lexer);
      /* If it is not a `,', we are done.  */
      if (token->type != CPP_COMMA)
	break;
      /* Consume the `,'.  */
      cp_lexer_consume_token (parser->lexer);
    }

  return nreverse (types);
}

/* Parse a try-block.

   try-block:
     try compound-statement handler-seq  */

static tree
cp_parser_try_block (cp_parser* parser)
{
  tree try_block;

  cp_parser_require_keyword (parser, RID_TRY, "%<try%>");
  try_block = begin_try_block ();
  cp_parser_compound_statement (parser, NULL, true);
  finish_try_block (try_block);
  cp_parser_handler_seq (parser);
  finish_handler_sequence (try_block);

  return try_block;
}

/* Parse a function-try-block.

   function-try-block:
     try ctor-initializer [opt] function-body handler-seq  */

static bool
cp_parser_function_try_block (cp_parser* parser)
{
  tree compound_stmt;
  tree try_block;
  bool ctor_initializer_p;

  /* Look for the `try' keyword.  */
  if (!cp_parser_require_keyword (parser, RID_TRY, "%<try%>"))
    return false;
  /* Let the rest of the front end know where we are.  */
  try_block = begin_function_try_block (&compound_stmt);
  /* Parse the function-body.  */
  ctor_initializer_p
    = cp_parser_ctor_initializer_opt_and_function_body (parser);
  /* We're done with the `try' part.  */
  finish_function_try_block (try_block);
  /* Parse the handlers.  */
  cp_parser_handler_seq (parser);
  /* We're done with the handlers.  */
  finish_function_handler_sequence (try_block, compound_stmt);

  return ctor_initializer_p;
}

/* Parse a handler-seq.

   handler-seq:
     handler handler-seq [opt]  */

static void
cp_parser_handler_seq (cp_parser* parser)
{
  while (true)
    {
      cp_token *token;

      /* Parse the handler.  */
      cp_parser_handler (parser);
      /* Peek at the next token.  */
      token = cp_lexer_peek_token (parser->lexer);
      /* If it's not `catch' then there are no more handlers.  */
      if (!cp_parser_is_keyword (token, RID_CATCH))
	break;
    }
}

/* Parse a handler.

   handler:
     catch ( exception-declaration ) compound-statement  */

static void
cp_parser_handler (cp_parser* parser)
{
  tree handler;
  tree declaration;

  cp_parser_require_keyword (parser, RID_CATCH, "%<catch%>");
  handler = begin_handler ();
  cp_parser_require (parser, CPP_OPEN_PAREN, "%<(%>");
  declaration = cp_parser_exception_declaration (parser);
  finish_handler_parms (declaration, handler);
  cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>");
  cp_parser_compound_statement (parser, NULL, false);
  finish_handler (handler);
}

/* Parse an exception-declaration.

   exception-declaration:
     type-specifier-seq declarator
     type-specifier-seq abstract-declarator
     type-specifier-seq
     ...

   Returns a VAR_DECL for the declaration, or NULL_TREE if the
   ellipsis variant is used.  */

static tree
cp_parser_exception_declaration (cp_parser* parser)
{
  cp_decl_specifier_seq type_specifiers;
  cp_declarator *declarator;
  const char *saved_message;

  /* If it's an ellipsis, it's easy to handle.  */
  if (cp_lexer_next_token_is (parser->lexer, CPP_ELLIPSIS))
    {
      /* Consume the `...' token.  */
      cp_lexer_consume_token (parser->lexer);
      return NULL_TREE;
    }

  /* Types may not be defined in exception-declarations.  */
  saved_message = parser->type_definition_forbidden_message;
  parser->type_definition_forbidden_message
    = G_("types may not be defined in exception-declarations");

  /* Parse the type-specifier-seq.  */
  cp_parser_type_specifier_seq (parser, /*is_declaration=*/true,
				/*is_trailing_return=*/false,
				&type_specifiers);
  /* If it's a `)', then there is no declarator.  */
  if (cp_lexer_next_token_is (parser->lexer, CPP_CLOSE_PAREN))
    declarator = NULL;
  else
    declarator = cp_parser_declarator (parser, CP_PARSER_DECLARATOR_EITHER,
				       /*ctor_dtor_or_conv_p=*/NULL,
				       /*parenthesized_p=*/NULL,
				       /*member_p=*/false);

  /* Restore the saved message.  */
  parser->type_definition_forbidden_message = saved_message;

  if (!type_specifiers.any_specifiers_p)
    return error_mark_node;

  return grokdeclarator (declarator, &type_specifiers, CATCHPARM, 1, NULL);
}

/* Parse a throw-expression.

   throw-expression:
     throw assignment-expression [opt]

   Returns a THROW_EXPR representing the throw-expression.  */

static tree
cp_parser_throw_expression (cp_parser* parser)
{
  tree expression;
  cp_token* token;

  cp_parser_require_keyword (parser, RID_THROW, "%<throw%>");
  token = cp_lexer_peek_token (parser->lexer);
  /* Figure out whether or not there is an assignment-expression
     following the "throw" keyword.  */
  if (token->type == CPP_COMMA
      || token->type == CPP_SEMICOLON
      || token->type == CPP_CLOSE_PAREN
      || token->type == CPP_CLOSE_SQUARE
      || token->type == CPP_CLOSE_BRACE
      || token->type == CPP_COLON)
    expression = NULL_TREE;
  else
    expression = cp_parser_assignment_expression (parser,
						  /*cast_p=*/false, NULL);

  return build_throw (expression);
}

/* GNU Extensions */

/* Parse an (optional) asm-specification.

   asm-specification:
     asm ( string-literal )

   If the asm-specification is present, returns a STRING_CST
   corresponding to the string-literal.  Otherwise, returns
   NULL_TREE.  */

static tree
cp_parser_asm_specification_opt (cp_parser* parser)
{
  cp_token *token;
  tree asm_specification;

  /* Peek at the next token.  */
  token = cp_lexer_peek_token (parser->lexer);
  /* If the next token isn't the `asm' keyword, then there's no
     asm-specification.  */
  if (!cp_parser_is_keyword (token, RID_ASM))
    return NULL_TREE;

  /* Consume the `asm' token.  */
  cp_lexer_consume_token (parser->lexer);
  /* Look for the `('.  */
  cp_parser_require (parser, CPP_OPEN_PAREN, "%<(%>");

  /* Look for the string-literal.  */
  asm_specification = cp_parser_string_literal (parser, false, false);

  /* Look for the `)'.  */
  cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>");

  return asm_specification;
}

/* Parse an asm-operand-list.

   asm-operand-list:
     asm-operand
     asm-operand-list , asm-operand

   asm-operand:
     string-literal ( expression )
     [ string-literal ] string-literal ( expression )

   Returns a TREE_LIST representing the operands.  The TREE_VALUE of
   each node is the expression.  The TREE_PURPOSE is itself a
   TREE_LIST whose TREE_PURPOSE is a STRING_CST for the bracketed
   string-literal (or NULL_TREE if not present) and whose TREE_VALUE
   is a STRING_CST for the string literal before the parenthesis. Returns
   ERROR_MARK_NODE if any of the operands are invalid.  */

static tree
cp_parser_asm_operand_list (cp_parser* parser)
{
  tree asm_operands = NULL_TREE;
  bool invalid_operands = false;

  while (true)
    {
      tree string_literal;
      tree expression;
      tree name;

      if (cp_lexer_next_token_is (parser->lexer, CPP_OPEN_SQUARE))
	{
	  /* Consume the `[' token.  */
	  cp_lexer_consume_token (parser->lexer);
	  /* Read the operand name.  */
	  name = cp_parser_identifier (parser);
	  if (name != error_mark_node)
	    name = build_string (IDENTIFIER_LENGTH (name),
				 IDENTIFIER_POINTER (name));
	  /* Look for the closing `]'.  */
	  cp_parser_require (parser, CPP_CLOSE_SQUARE, "%<]%>");
	}
      else
	name = NULL_TREE;
      /* Look for the string-literal.  */
      string_literal = cp_parser_string_literal (parser, false, false);

      /* Look for the `('.  */
      cp_parser_require (parser, CPP_OPEN_PAREN, "%<(%>");
      /* Parse the expression.  */
      expression = cp_parser_expression (parser, /*cast_p=*/false, NULL);
      /* Look for the `)'.  */
      cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>");

      if (name == error_mark_node 
	  || string_literal == error_mark_node 
	  || expression == error_mark_node)
        invalid_operands = true;

      /* Add this operand to the list.  */
      asm_operands = tree_cons (build_tree_list (name, string_literal),
				expression,
				asm_operands);
      /* If the next token is not a `,', there are no more
	 operands.  */
      if (cp_lexer_next_token_is_not (parser->lexer, CPP_COMMA))
	break;
      /* Consume the `,'.  */
      cp_lexer_consume_token (parser->lexer);
    }

  return invalid_operands ? error_mark_node : nreverse (asm_operands);
}

/* Parse an asm-clobber-list.

   asm-clobber-list:
     string-literal
     asm-clobber-list , string-literal

   Returns a TREE_LIST, indicating the clobbers in the order that they
   appeared.  The TREE_VALUE of each node is a STRING_CST.  */

static tree
cp_parser_asm_clobber_list (cp_parser* parser)
{
  tree clobbers = NULL_TREE;

  while (true)
    {
      tree string_literal;

      /* Look for the string literal.  */
      string_literal = cp_parser_string_literal (parser, false, false);
      /* Add it to the list.  */
      clobbers = tree_cons (NULL_TREE, string_literal, clobbers);
      /* If the next token is not a `,', then the list is
	 complete.  */
      if (cp_lexer_next_token_is_not (parser->lexer, CPP_COMMA))
	break;
      /* Consume the `,' token.  */
      cp_lexer_consume_token (parser->lexer);
    }

  return clobbers;
}

/* Parse an asm-label-list.

   asm-label-list:
     identifier
     asm-label-list , identifier

   Returns a TREE_LIST, indicating the labels in the order that they
   appeared.  The TREE_VALUE of each node is a label.  */

static tree
cp_parser_asm_label_list (cp_parser* parser)
{
  tree labels = NULL_TREE;

  while (true)
    {
      tree identifier, label, name;

      /* Look for the identifier.  */
      identifier = cp_parser_identifier (parser);
      if (!error_operand_p (identifier))
        {
	  label = lookup_label (identifier);
	  if (TREE_CODE (label) == LABEL_DECL)
	    {
	      TREE_USED (label) = 1;
	      check_goto (label);
	      name = build_string (IDENTIFIER_LENGTH (identifier),
				   IDENTIFIER_POINTER (identifier));
	      labels = tree_cons (name, label, labels);
	    }
	}
      /* If the next token is not a `,', then the list is
	 complete.  */
      if (cp_lexer_next_token_is_not (parser->lexer, CPP_COMMA))
	break;
      /* Consume the `,' token.  */
      cp_lexer_consume_token (parser->lexer);
    }

  return nreverse (labels);
}

/* Parse an (optional) series of attributes.

   attributes:
     attributes attribute

   attribute:
     __attribute__ (( attribute-list [opt] ))

   The return value is as for cp_parser_attribute_list.  */

static tree
cp_parser_attributes_opt (cp_parser* parser)
{
  tree attributes = NULL_TREE;

  while (true)
    {
      cp_token *token;
      tree attribute_list;

      /* Peek at the next token.  */
      token = cp_lexer_peek_token (parser->lexer);
      /* If it's not `__attribute__', then we're done.  */
      if (token->keyword != RID_ATTRIBUTE)
	break;

      /* Consume the `__attribute__' keyword.  */
      cp_lexer_consume_token (parser->lexer);
      /* Look for the two `(' tokens.  */
      cp_parser_require (parser, CPP_OPEN_PAREN, "%<(%>");
      cp_parser_require (parser, CPP_OPEN_PAREN, "%<(%>");

      /* Peek at the next token.  */
      token = cp_lexer_peek_token (parser->lexer);
      if (token->type != CPP_CLOSE_PAREN)
	/* Parse the attribute-list.  */
	attribute_list = cp_parser_attribute_list (parser);
      else
	/* If the next token is a `)', then there is no attribute
	   list.  */
	attribute_list = NULL;

      /* Look for the two `)' tokens.  */
      cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>");
      cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>");

      /* Add these new attributes to the list.  */
      attributes = chainon (attributes, attribute_list);
    }

  return attributes;
}

/* Parse an attribute-list.

   attribute-list:
     attribute
     attribute-list , attribute

   attribute:
     identifier
     identifier ( identifier )
     identifier ( identifier , expression-list )
     identifier ( expression-list )

   Returns a TREE_LIST, or NULL_TREE on error.  Each node corresponds
   to an attribute.  The TREE_PURPOSE of each node is the identifier
   indicating which attribute is in use.  The TREE_VALUE represents
   the arguments, if any.  */

static tree
cp_parser_attribute_list (cp_parser* parser)
{
  tree attribute_list = NULL_TREE;
  bool save_translate_strings_p = parser->translate_strings_p;

  parser->translate_strings_p = false;
  while (true)
    {
      cp_token *token;
      tree identifier;
      tree attribute;

      /* Look for the identifier.  We also allow keywords here; for
	 example `__attribute__ ((const))' is legal.  */
      token = cp_lexer_peek_token (parser->lexer);
      if (token->type == CPP_NAME
	  || token->type == CPP_KEYWORD)
	{
	  tree arguments = NULL_TREE;

	  /* Consume the token.  */
	  token = cp_lexer_consume_token (parser->lexer);

	  /* Save away the identifier that indicates which attribute
	     this is.  */
	  identifier = (token->type == CPP_KEYWORD) 
	    /* For keywords, use the canonical spelling, not the
	       parsed identifier.  */
	    ? ridpointers[(int) token->keyword]
	    : token->u.value;
	  
	  attribute = build_tree_list (identifier, NULL_TREE);

	  /* Peek at the next token.  */
	  token = cp_lexer_peek_token (parser->lexer);
	  /* If it's an `(', then parse the attribute arguments.  */
	  if (token->type == CPP_OPEN_PAREN)
	    {
	      VEC(tree,gc) *vec;
	      vec = cp_parser_parenthesized_expression_list
		    (parser, true, /*cast_p=*/false,
		     /*allow_expansion_p=*/false,
		     /*non_constant_p=*/NULL);
	      if (vec == NULL)
		arguments = error_mark_node;
	      else
		{
		  arguments = build_tree_list_vec (vec);
		  release_tree_vector (vec);
		}
	      /* Save the arguments away.  */
	      TREE_VALUE (attribute) = arguments;
	    }

	  if (arguments != error_mark_node)
	    {
	      /* Add this attribute to the list.  */
	      TREE_CHAIN (attribute) = attribute_list;
	      attribute_list = attribute;
	    }

	  token = cp_lexer_peek_token (parser->lexer);
	}
      /* Now, look for more attributes.  If the next token isn't a
	 `,', we're done.  */
      if (token->type != CPP_COMMA)
	break;

      /* Consume the comma and keep going.  */
      cp_lexer_consume_token (parser->lexer);
    }
  parser->translate_strings_p = save_translate_strings_p;

  /* We built up the list in reverse order.  */
  return nreverse (attribute_list);
}

/* Parse an optional `__extension__' keyword.  Returns TRUE if it is
   present, and FALSE otherwise.  *SAVED_PEDANTIC is set to the
   current value of the PEDANTIC flag, regardless of whether or not
   the `__extension__' keyword is present.  The caller is responsible
   for restoring the value of the PEDANTIC flag.  */

static bool
cp_parser_extension_opt (cp_parser* parser, int* saved_pedantic)
{
  /* Save the old value of the PEDANTIC flag.  */
  *saved_pedantic = pedantic;

  if (cp_lexer_next_token_is_keyword (parser->lexer, RID_EXTENSION))
    {
      /* Consume the `__extension__' token.  */
      cp_lexer_consume_token (parser->lexer);
      /* We're not being pedantic while the `__extension__' keyword is
	 in effect.  */
      pedantic = 0;

      return true;
    }

  return false;
}

/* Parse a label declaration.

   label-declaration:
     __label__ label-declarator-seq ;

   label-declarator-seq:
     identifier , label-declarator-seq
     identifier  */

static void
cp_parser_label_declaration (cp_parser* parser)
{
  /* Look for the `__label__' keyword.  */
  cp_parser_require_keyword (parser, RID_LABEL, "%<__label__%>");

  while (true)
    {
      tree identifier;

      /* Look for an identifier.  */
      identifier = cp_parser_identifier (parser);
      /* If we failed, stop.  */
      if (identifier == error_mark_node)
	break;
      /* Declare it as a label.  */
      finish_label_decl (identifier);
      /* If the next token is a `;', stop.  */
      if (cp_lexer_next_token_is (parser->lexer, CPP_SEMICOLON))
	break;
      /* Look for the `,' separating the label declarations.  */
      cp_parser_require (parser, CPP_COMMA, "%<,%>");
    }

  /* Look for the final `;'.  */
  cp_parser_require (parser, CPP_SEMICOLON, "%<;%>");
}

/* Support Functions */

/* Looks up NAME in the current scope, as given by PARSER->SCOPE.
   NAME should have one of the representations used for an
   id-expression.  If NAME is the ERROR_MARK_NODE, the ERROR_MARK_NODE
   is returned.  If PARSER->SCOPE is a dependent type, then a
   SCOPE_REF is returned.

   If NAME is a TEMPLATE_ID_EXPR, then it will be immediately
   returned; the name was already resolved when the TEMPLATE_ID_EXPR
   was formed.  Abstractly, such entities should not be passed to this
   function, because they do not need to be looked up, but it is
   simpler to check for this special case here, rather than at the
   call-sites.

   In cases not explicitly covered above, this function returns a
   DECL, OVERLOAD, or baselink representing the result of the lookup.
   If there was no entity with the indicated NAME, the ERROR_MARK_NODE
   is returned.

   If TAG_TYPE is not NONE_TYPE, it indicates an explicit type keyword
   (e.g., "struct") that was used.  In that case bindings that do not
   refer to types are ignored.

   If IS_TEMPLATE is TRUE, bindings that do not refer to templates are
   ignored.

   If IS_NAMESPACE is TRUE, bindings that do not refer to namespaces
   are ignored.

   If CHECK_DEPENDENCY is TRUE, names are not looked up in dependent
   types.

   If AMBIGUOUS_DECLS is non-NULL, *AMBIGUOUS_DECLS is set to a
   TREE_LIST of candidates if name-lookup results in an ambiguity, and
   NULL_TREE otherwise.  */

static tree
cp_parser_lookup_name (cp_parser *parser, tree name,
		       enum tag_types tag_type,
		       bool is_template,
		       bool is_namespace,
		       bool check_dependency,
		       tree *ambiguous_decls,
		       location_t name_location)
{
  int flags = 0;
  tree decl;
  tree object_type = parser->context->object_type;

  if (!cp_parser_uncommitted_to_tentative_parse_p (parser))
    flags |= LOOKUP_COMPLAIN;

  /* Assume that the lookup will be unambiguous.  */
  if (ambiguous_decls)
    *ambiguous_decls = NULL_TREE;

  /* Now that we have looked up the name, the OBJECT_TYPE (if any) is
     no longer valid.  Note that if we are parsing tentatively, and
     the parse fails, OBJECT_TYPE will be automatically restored.  */
  parser->context->object_type = NULL_TREE;

  if (name == error_mark_node)
    return error_mark_node;

  /* A template-id has already been resolved; there is no lookup to
     do.  */
  if (TREE_CODE (name) == TEMPLATE_ID_EXPR)
    return name;
  if (BASELINK_P (name))
    {
      gcc_assert (TREE_CODE (BASELINK_FUNCTIONS (name))
		  == TEMPLATE_ID_EXPR);
      return name;
    }

  /* A BIT_NOT_EXPR is used to represent a destructor.  By this point,
     it should already have been checked to make sure that the name
     used matches the type being destroyed.  */
  if (TREE_CODE (name) == BIT_NOT_EXPR)
    {
      tree type;

      /* Figure out to which type this destructor applies.  */
      if (parser->scope)
	type = parser->scope;
      else if (object_type)
	type = object_type;
      else
	type = current_class_type;
      /* If that's not a class type, there is no destructor.  */
      if (!type || !CLASS_TYPE_P (type))
	return error_mark_node;
      if (CLASSTYPE_LAZY_DESTRUCTOR (type))
	lazily_declare_fn (sfk_destructor, type);
      if (!CLASSTYPE_DESTRUCTORS (type))
	  return error_mark_node;
      /* If it was a class type, return the destructor.  */
      return CLASSTYPE_DESTRUCTORS (type);
    }

  /* By this point, the NAME should be an ordinary identifier.  If
     the id-expression was a qualified name, the qualifying scope is
     stored in PARSER->SCOPE at this point.  */
  gcc_assert (TREE_CODE (name) == IDENTIFIER_NODE);

  /* Perform the lookup.  */
  if (parser->scope)
    {
      bool dependent_p;

      if (parser->scope == error_mark_node)
	return error_mark_node;

      /* If the SCOPE is dependent, the lookup must be deferred until
	 the template is instantiated -- unless we are explicitly
	 looking up names in uninstantiated templates.  Even then, we
	 cannot look up the name if the scope is not a class type; it
	 might, for example, be a template type parameter.  */
      dependent_p = (TYPE_P (parser->scope)
		     && dependent_scope_p (parser->scope));
      if ((check_dependency || !CLASS_TYPE_P (parser->scope))
	  && dependent_p)
	/* Defer lookup.  */
	decl = error_mark_node;
      else
	{
	  tree pushed_scope = NULL_TREE;

	  /* If PARSER->SCOPE is a dependent type, then it must be a
	     class type, and we must not be checking dependencies;
	     otherwise, we would have processed this lookup above.  So
	     that PARSER->SCOPE is not considered a dependent base by
	     lookup_member, we must enter the scope here.  */
	  if (dependent_p)
	    pushed_scope = push_scope (parser->scope);

	  /* 3.4.3.1: In a lookup in which the constructor is an acceptable
	     lookup result and the nested-name-specifier nominates a class C:
	       * if the name specified after the nested-name-specifier, when
	       looked up in C, is the injected-class-name of C (Clause 9), or
	       * if the name specified after the nested-name-specifier is the
	       same as the identifier or the simple-template-id's template-
	       name in the last component of the nested-name-specifier,
	     the name is instead considered to name the constructor of
	     class C. [ Note: for example, the constructor is not an
	     acceptable lookup result in an elaborated-type-specifier so
	     the constructor would not be used in place of the
	     injected-class-name. --end note ] Such a constructor name
	     shall be used only in the declarator-id of a declaration that
	     names a constructor or in a using-declaration.  */
	  if (tag_type == none_type
	      && CLASS_TYPE_P (parser->scope)
	      && constructor_name_p (name, parser->scope))
	    name = ctor_identifier;

	  /* If the PARSER->SCOPE is a template specialization, it
	     may be instantiated during name lookup.  In that case,
	     errors may be issued.  Even if we rollback the current
	     tentative parse, those errors are valid.  */
	  decl = lookup_qualified_name (parser->scope, name,
					tag_type != none_type,
					/*complain=*/true);

	  /* If we have a single function from a using decl, pull it out.  */
	  if (TREE_CODE (decl) == OVERLOAD
	      && !really_overloaded_fn (decl))
	    decl = OVL_FUNCTION (decl);

	  if (pushed_scope)
	    pop_scope (pushed_scope);
	}

      /* If the scope is a dependent type and either we deferred lookup or
	 we did lookup but didn't find the name, rememeber the name.  */
      if (decl == error_mark_node && TYPE_P (parser->scope)
	  && dependent_type_p (parser->scope))
	{
	  if (tag_type)
	    {
	      tree type;

	      /* The resolution to Core Issue 180 says that `struct
		 A::B' should be considered a type-name, even if `A'
		 is dependent.  */
	      type = make_typename_type (parser->scope, name, tag_type,
					 /*complain=*/tf_error);
	      decl = TYPE_NAME (type);
	    }
	  else if (is_template
		   && (cp_parser_next_token_ends_template_argument_p (parser)
		       || cp_lexer_next_token_is (parser->lexer,
						  CPP_CLOSE_PAREN)))
	    decl = make_unbound_class_template (parser->scope,
						name, NULL_TREE,
						/*complain=*/tf_error);
	  else
	    decl = build_qualified_name (/*type=*/NULL_TREE,
					 parser->scope, name,
					 is_template);
	}
      parser->qualifying_scope = parser->scope;
      parser->object_scope = NULL_TREE;
    }
  else if (object_type)
    {
      tree object_decl = NULL_TREE;
      /* Look up the name in the scope of the OBJECT_TYPE, unless the
	 OBJECT_TYPE is not a class.  */
      if (CLASS_TYPE_P (object_type))
	/* If the OBJECT_TYPE is a template specialization, it may
	   be instantiated during name lookup.  In that case, errors
	   may be issued.  Even if we rollback the current tentative
	   parse, those errors are valid.  */
	object_decl = lookup_member (object_type,
				     name,
				     /*protect=*/0,
				     tag_type != none_type);
      /* Look it up in the enclosing context, too.  */
      decl = lookup_name_real (name, tag_type != none_type,
			       /*nonclass=*/0,
			       /*block_p=*/true, is_namespace, flags);
      parser->object_scope = object_type;
      parser->qualifying_scope = NULL_TREE;
      if (object_decl)
	decl = object_decl;
    }
  else
    {
      decl = lookup_name_real (name, tag_type != none_type,
			       /*nonclass=*/0,
			       /*block_p=*/true, is_namespace, flags);
      parser->qualifying_scope = NULL_TREE;
      parser->object_scope = NULL_TREE;
    }

  /* If the lookup failed, let our caller know.  */
  if (!decl || decl == error_mark_node)
    return error_mark_node;

  /* Pull out the template from an injected-class-name (or multiple).  */
  if (is_template)
    decl = maybe_get_template_decl_from_type_decl (decl);

  /* If it's a TREE_LIST, the result of the lookup was ambiguous.  */
  if (TREE_CODE (decl) == TREE_LIST)
    {
      if (ambiguous_decls)
	*ambiguous_decls = decl;
      /* The error message we have to print is too complicated for
	 cp_parser_error, so we incorporate its actions directly.  */
      if (!cp_parser_simulate_error (parser))
	{
	  error_at (name_location, "reference to %qD is ambiguous",
		    name);
	  print_candidates (decl);
	}
      return error_mark_node;
    }

  gcc_assert (DECL_P (decl)
	      || TREE_CODE (decl) == OVERLOAD
	      || TREE_CODE (decl) == SCOPE_REF
	      || TREE_CODE (decl) == UNBOUND_CLASS_TEMPLATE
	      || BASELINK_P (decl));

  /* If we have resolved the name of a member declaration, check to
     see if the declaration is accessible.  When the name resolves to
     set of overloaded functions, accessibility is checked when
     overload resolution is done.

     During an explicit instantiation, access is not checked at all,
     as per [temp.explicit].  */
  if (DECL_P (decl))
    check_accessibility_of_qualified_id (decl, object_type, parser->scope);

  return decl;
}

/* Like cp_parser_lookup_name, but for use in the typical case where
   CHECK_ACCESS is TRUE, IS_TYPE is FALSE, IS_TEMPLATE is FALSE,
   IS_NAMESPACE is FALSE, and CHECK_DEPENDENCY is TRUE.  */

static tree
cp_parser_lookup_name_simple (cp_parser* parser, tree name, location_t location)
{
  return cp_parser_lookup_name (parser, name,
				none_type,
				/*is_template=*/false,
				/*is_namespace=*/false,
				/*check_dependency=*/true,
				/*ambiguous_decls=*/NULL,
				location);
}

/* If DECL is a TEMPLATE_DECL that can be treated like a TYPE_DECL in
   the current context, return the TYPE_DECL.  If TAG_NAME_P is
   true, the DECL indicates the class being defined in a class-head,
   or declared in an elaborated-type-specifier.

   Otherwise, return DECL.  */

static tree
cp_parser_maybe_treat_template_as_class (tree decl, bool tag_name_p)
{
  /* If the TEMPLATE_DECL is being declared as part of a class-head,
     the translation from TEMPLATE_DECL to TYPE_DECL occurs:

       struct A {
	 template <typename T> struct B;
       };

       template <typename T> struct A::B {};

     Similarly, in an elaborated-type-specifier:

       namespace N { struct X{}; }

       struct A {
	 template <typename T> friend struct N::X;
       };

     However, if the DECL refers to a class type, and we are in
     the scope of the class, then the name lookup automatically
     finds the TYPE_DECL created by build_self_reference rather
     than a TEMPLATE_DECL.  For example, in:

       template <class T> struct S {
	 S s;
       };

     there is no need to handle such case.  */

  if (DECL_CLASS_TEMPLATE_P (decl) && tag_name_p)
    return DECL_TEMPLATE_RESULT (decl);

  return decl;
}

/* If too many, or too few, template-parameter lists apply to the
   declarator, issue an error message.  Returns TRUE if all went well,
   and FALSE otherwise.  */

static bool
cp_parser_check_declarator_template_parameters (cp_parser* parser,
						cp_declarator *declarator,
						location_t declarator_location)
{
  unsigned num_templates;

  /* We haven't seen any classes that involve template parameters yet.  */
  num_templates = 0;

  switch (declarator->kind)
    {
    case cdk_id:
      if (declarator->u.id.qualifying_scope)
	{
	  tree scope;

	  scope = declarator->u.id.qualifying_scope;

	  while (scope && CLASS_TYPE_P (scope))
	    {
	      /* You're supposed to have one `template <...>'
		 for every template class, but you don't need one
		 for a full specialization.  For example:

		 template <class T> struct S{};
		 template <> struct S<int> { void f(); };
		 void S<int>::f () {}

		 is correct; there shouldn't be a `template <>' for
		 the definition of `S<int>::f'.  */
	      if (!CLASSTYPE_TEMPLATE_INFO (scope))
		/* If SCOPE does not have template information of any
		   kind, then it is not a template, nor is it nested
		   within a template.  */
		break;
	      if (explicit_class_specialization_p (scope))
		break;
	      if (PRIMARY_TEMPLATE_P (CLASSTYPE_TI_TEMPLATE (scope)))
		++num_templates;

	      scope = TYPE_CONTEXT (scope);
	    }
	}
      else if (TREE_CODE (declarator->u.id.unqualified_name)
	       == TEMPLATE_ID_EXPR)
	/* If the DECLARATOR has the form `X<y>' then it uses one
	   additional level of template parameters.  */
	++num_templates;

      return cp_parser_check_template_parameters 
	(parser, num_templates, declarator_location, declarator);


    case cdk_function:
    case cdk_array:
    case cdk_pointer:
    case cdk_reference:
    case cdk_ptrmem:
      return (cp_parser_check_declarator_template_parameters
	      (parser, declarator->declarator, declarator_location));

    case cdk_error:
      return true;

    default:
      gcc_unreachable ();
    }
  return false;
}

/* NUM_TEMPLATES were used in the current declaration.  If that is
   invalid, return FALSE and issue an error messages.  Otherwise,
   return TRUE.  If DECLARATOR is non-NULL, then we are checking a
   declarator and we can print more accurate diagnostics.  */

static bool
cp_parser_check_template_parameters (cp_parser* parser,
				     unsigned num_templates,
				     location_t location,
				     cp_declarator *declarator)
{
  /* If there are the same number of template classes and parameter
     lists, that's OK.  */
  if (parser->num_template_parameter_lists == num_templates)
    return true;
  /* If there are more, but only one more, then we are referring to a
     member template.  That's OK too.  */
  if (parser->num_template_parameter_lists == num_templates + 1)
    return true;
  /* If there are more template classes than parameter lists, we have
     something like:

       template <class T> void S<T>::R<T>::f ();  */
  if (parser->num_template_parameter_lists < num_templates)
    {
      if (declarator && !current_function_decl)
	error_at (location, "specializing member %<%T::%E%> "
		  "requires %<template<>%> syntax", 
		  declarator->u.id.qualifying_scope,
		  declarator->u.id.unqualified_name);
      else if (declarator)
	error_at (location, "invalid declaration of %<%T::%E%>",
		  declarator->u.id.qualifying_scope,
		  declarator->u.id.unqualified_name);
      else 
	error_at (location, "too few template-parameter-lists");
      return false;
    }
  /* Otherwise, there are too many template parameter lists.  We have
     something like:

     template <class T> template <class U> void S::f();  */
  error_at (location, "too many template-parameter-lists");
  return false;
}

/* Parse an optional `::' token indicating that the following name is
   from the global namespace.  If so, PARSER->SCOPE is set to the
   GLOBAL_NAMESPACE. Otherwise, PARSER->SCOPE is set to NULL_TREE,
   unless CURRENT_SCOPE_VALID_P is TRUE, in which case it is left alone.
   Returns the new value of PARSER->SCOPE, if the `::' token is
   present, and NULL_TREE otherwise.  */

static tree
cp_parser_global_scope_opt (cp_parser* parser, bool current_scope_valid_p)
{
  cp_token *token;

  /* Peek at the next token.  */
  token = cp_lexer_peek_token (parser->lexer);
  /* If we're looking at a `::' token then we're starting from the
     global namespace, not our current location.  */
  if (token->type == CPP_SCOPE)
    {
      /* Consume the `::' token.  */
      cp_lexer_consume_token (parser->lexer);
      /* Set the SCOPE so that we know where to start the lookup.  */
      parser->scope = global_namespace;
      parser->qualifying_scope = global_namespace;
      parser->object_scope = NULL_TREE;

      return parser->scope;
    }
  else if (!current_scope_valid_p)
    {
      parser->scope = NULL_TREE;
      parser->qualifying_scope = NULL_TREE;
      parser->object_scope = NULL_TREE;
    }

  return NULL_TREE;
}

/* Returns TRUE if the upcoming token sequence is the start of a
   constructor declarator.  If FRIEND_P is true, the declarator is
   preceded by the `friend' specifier.  */

static bool
cp_parser_constructor_declarator_p (cp_parser *parser, bool friend_p)
{
  bool constructor_p;
  tree nested_name_specifier;
  cp_token *next_token;

  /* The common case is that this is not a constructor declarator, so
     try to avoid doing lots of work if at all possible.  It's not
     valid declare a constructor at function scope.  */
  if (parser->in_function_body)
    return false;
  /* And only certain tokens can begin a constructor declarator.  */
  next_token = cp_lexer_peek_token (parser->lexer);
  if (next_token->type != CPP_NAME
      && next_token->type != CPP_SCOPE
      && next_token->type != CPP_NESTED_NAME_SPECIFIER
      && next_token->type != CPP_TEMPLATE_ID)
    return false;

  /* Parse tentatively; we are going to roll back all of the tokens
     consumed here.  */
  cp_parser_parse_tentatively (parser);
  /* Assume that we are looking at a constructor declarator.  */
  constructor_p = true;

  /* Look for the optional `::' operator.  */
  cp_parser_global_scope_opt (parser,
			      /*current_scope_valid_p=*/false);
  /* Look for the nested-name-specifier.  */
  nested_name_specifier
    = (cp_parser_nested_name_specifier_opt (parser,
					    /*typename_keyword_p=*/false,
					    /*check_dependency_p=*/false,
					    /*type_p=*/false,
					    /*is_declaration=*/false));
  /* Outside of a class-specifier, there must be a
     nested-name-specifier.  */
  if (!nested_name_specifier &&
      (!at_class_scope_p () || !TYPE_BEING_DEFINED (current_class_type)
       || friend_p))
    constructor_p = false;
  else if (nested_name_specifier == error_mark_node)
    constructor_p = false;

  /* If we have a class scope, this is easy; DR 147 says that S::S always
     names the constructor, and no other qualified name could.  */
  if (constructor_p && nested_name_specifier
      && TYPE_P (nested_name_specifier))
    {
      tree id = cp_parser_unqualified_id (parser,
					  /*template_keyword_p=*/false,
					  /*check_dependency_p=*/false,
					  /*declarator_p=*/true,
					  /*optional_p=*/false);
      if (is_overloaded_fn (id))
	id = DECL_NAME (get_first_fn (id));
      if (!constructor_name_p (id, nested_name_specifier))
	constructor_p = false;
    }
  /* If we still think that this might be a constructor-declarator,
     look for a class-name.  */
  else if (constructor_p)
    {
      /* If we have:

	   template <typename T> struct S {
	     S();
	   };

	 we must recognize that the nested `S' names a class.  */
      tree type_decl;
      type_decl = cp_parser_class_name (parser,
					/*typename_keyword_p=*/false,
					/*template_keyword_p=*/false,
					none_type,
					/*check_dependency_p=*/false,
					/*class_head_p=*/false,
					/*is_declaration=*/false);
      /* If there was no class-name, then this is not a constructor.  */
      constructor_p = !cp_parser_error_occurred (parser);

      /* If we're still considering a constructor, we have to see a `(',
	 to begin the parameter-declaration-clause, followed by either a
	 `)', an `...', or a decl-specifier.  We need to check for a
	 type-specifier to avoid being fooled into thinking that:

	   S (f) (int);

	 is a constructor.  (It is actually a function named `f' that
	 takes one parameter (of type `int') and returns a value of type
	 `S'.  */
      if (constructor_p
	  && !cp_parser_require (parser, CPP_OPEN_PAREN, "%<(%>"))
	constructor_p = false;

      if (constructor_p
	  && cp_lexer_next_token_is_not (parser->lexer, CPP_CLOSE_PAREN)
	  && cp_lexer_next_token_is_not (parser->lexer, CPP_ELLIPSIS)
	  /* A parameter declaration begins with a decl-specifier,
	     which is either the "attribute" keyword, a storage class
	     specifier, or (usually) a type-specifier.  */
	  && !cp_lexer_next_token_is_decl_specifier_keyword (parser->lexer))
	{
	  tree type;
	  tree pushed_scope = NULL_TREE;
	  unsigned saved_num_template_parameter_lists;

	  /* Names appearing in the type-specifier should be looked up
	     in the scope of the class.  */
	  if (current_class_type)
	    type = NULL_TREE;
	  else
	    {
	      type = TREE_TYPE (type_decl);
	      if (TREE_CODE (type) == TYPENAME_TYPE)
		{
		  type = resolve_typename_type (type,
						/*only_current_p=*/false);
		  if (TREE_CODE (type) == TYPENAME_TYPE)
		    {
		      cp_parser_abort_tentative_parse (parser);
		      return false;
		    }
		}
	      pushed_scope = push_scope (type);
	    }

	  /* Inside the constructor parameter list, surrounding
	     template-parameter-lists do not apply.  */
	  saved_num_template_parameter_lists
	    = parser->num_template_parameter_lists;
	  parser->num_template_parameter_lists = 0;

	  /* Look for the type-specifier.  */
	  cp_parser_type_specifier (parser,
				    CP_PARSER_FLAGS_NONE,
				    /*decl_specs=*/NULL,
				    /*is_declarator=*/true,
				    /*declares_class_or_enum=*/NULL,
				    /*is_cv_qualifier=*/NULL);

	  parser->num_template_parameter_lists
	    = saved_num_template_parameter_lists;

	  /* Leave the scope of the class.  */
	  if (pushed_scope)
	    pop_scope (pushed_scope);

	  constructor_p = !cp_parser_error_occurred (parser);
	}
    }

  /* We did not really want to consume any tokens.  */
  cp_parser_abort_tentative_parse (parser);

  return constructor_p;
}

/* Parse the definition of the function given by the DECL_SPECIFIERS,
   ATTRIBUTES, and DECLARATOR.  The access checks have been deferred;
   they must be performed once we are in the scope of the function.

   Returns the function defined.  */

static tree
cp_parser_function_definition_from_specifiers_and_declarator
  (cp_parser* parser,
   cp_decl_specifier_seq *decl_specifiers,
   tree attributes,
   const cp_declarator *declarator)
{
  tree fn;
  bool success_p;

  /* Begin the function-definition.  */
  success_p = start_function (decl_specifiers, declarator, attributes);

  /* The things we're about to see are not directly qualified by any
     template headers we've seen thus far.  */
  reset_specialization ();

  /* If there were names looked up in the decl-specifier-seq that we
     did not check, check them now.  We must wait until we are in the
     scope of the function to perform the checks, since the function
     might be a friend.  */
  perform_deferred_access_checks ();

  if (!success_p)
    {
      /* Skip the entire function.  */
      cp_parser_skip_to_end_of_block_or_statement (parser);
      fn = error_mark_node;
    }
  else if (DECL_INITIAL (current_function_decl) != error_mark_node)
    {
      /* Seen already, skip it.  An error message has already been output.  */
      cp_parser_skip_to_end_of_block_or_statement (parser);
      fn = current_function_decl;
      current_function_decl = NULL_TREE;
      /* If this is a function from a class, pop the nested class.  */
      if (current_class_name)
	pop_nested_class ();
    }
  else
    fn = cp_parser_function_definition_after_declarator (parser,
							 /*inline_p=*/false);

  return fn;
}

/* Parse the part of a function-definition that follows the
   declarator.  INLINE_P is TRUE iff this function is an inline
   function defined within a class-specifier.

   Returns the function defined.  */

static tree
cp_parser_function_definition_after_declarator (cp_parser* parser,
						bool inline_p)
{
  tree fn;
  bool ctor_initializer_p = false;
  bool saved_in_unbraced_linkage_specification_p;
  bool saved_in_function_body;
  unsigned saved_num_template_parameter_lists;
  cp_token *token;

  saved_in_function_body = parser->in_function_body;
  parser->in_function_body = true;
  /* If the next token is `return', then the code may be trying to
     make use of the "named return value" extension that G++ used to
     support.  */
  token = cp_lexer_peek_token (parser->lexer);
  if (cp_lexer_next_token_is_keyword (parser->lexer, RID_RETURN))
    {
      /* Consume the `return' keyword.  */
      cp_lexer_consume_token (parser->lexer);
      /* Look for the identifier that indicates what value is to be
	 returned.  */
      cp_parser_identifier (parser);
      /* Issue an error message.  */
      error_at (token->location,
		"named return values are no longer supported");
      /* Skip tokens until we reach the start of the function body.  */
      while (true)
	{
	  cp_token *token = cp_lexer_peek_token (parser->lexer);
	  if (token->type == CPP_OPEN_BRACE
	      || token->type == CPP_EOF
	      || token->type == CPP_PRAGMA_EOL)
	    break;
	  cp_lexer_consume_token (parser->lexer);
	}
    }
  /* The `extern' in `extern "C" void f () { ... }' does not apply to
     anything declared inside `f'.  */
  saved_in_unbraced_linkage_specification_p
    = parser->in_unbraced_linkage_specification_p;
  parser->in_unbraced_linkage_specification_p = false;
  /* Inside the function, surrounding template-parameter-lists do not
     apply.  */
  saved_num_template_parameter_lists
    = parser->num_template_parameter_lists;
  parser->num_template_parameter_lists = 0;

  start_lambda_scope (current_function_decl);

  /* If the next token is `try', then we are looking at a
     function-try-block.  */
  if (cp_lexer_next_token_is_keyword (parser->lexer, RID_TRY))
    ctor_initializer_p = cp_parser_function_try_block (parser);
  /* A function-try-block includes the function-body, so we only do
     this next part if we're not processing a function-try-block.  */
  else
    ctor_initializer_p
      = cp_parser_ctor_initializer_opt_and_function_body (parser);

  finish_lambda_scope ();

  /* Finish the function.  */
  fn = finish_function ((ctor_initializer_p ? 1 : 0) |
			(inline_p ? 2 : 0));
  /* Generate code for it, if necessary.  */
  expand_or_defer_fn (fn);
  /* Restore the saved values.  */
  parser->in_unbraced_linkage_specification_p
    = saved_in_unbraced_linkage_specification_p;
  parser->num_template_parameter_lists
    = saved_num_template_parameter_lists;
  parser->in_function_body = saved_in_function_body;

  return fn;
}

/* Parse a template-declaration, assuming that the `export' (and
   `extern') keywords, if present, has already been scanned.  MEMBER_P
   is as for cp_parser_template_declaration.  */

static void
cp_parser_template_declaration_after_export (cp_parser* parser, bool member_p)
{
  tree decl = NULL_TREE;
  VEC (deferred_access_check,gc) *checks;
  tree parameter_list;
  bool friend_p = false;
  bool need_lang_pop;
  cp_token *token;

  /* Look for the `template' keyword.  */
  token = cp_lexer_peek_token (parser->lexer);
  if (!cp_parser_require_keyword (parser, RID_TEMPLATE, "%<template%>"))
    return;

  /* And the `<'.  */
  if (!cp_parser_require (parser, CPP_LESS, "%<<%>"))
    return;
  if (at_class_scope_p () && current_function_decl)
    {
      /* 14.5.2.2 [temp.mem]

         A local class shall not have member templates.  */
      error_at (token->location,
		"invalid declaration of member template in local class");
      cp_parser_skip_to_end_of_block_or_statement (parser);
      return;
    }
  /* [temp]

     A template ... shall not have C linkage.  */
  if (current_lang_name == lang_name_c)
    {
      error_at (token->location, "template with C linkage");
      /* Give it C++ linkage to avoid confusing other parts of the
	 front end.  */
      push_lang_context (lang_name_cplusplus);
      need_lang_pop = true;
    }
  else
    need_lang_pop = false;

  /* We cannot perform access checks on the template parameter
     declarations until we know what is being declared, just as we
     cannot check the decl-specifier list.  */
  push_deferring_access_checks (dk_deferred);

  /* If the next token is `>', then we have an invalid
     specialization.  Rather than complain about an invalid template
     parameter, issue an error message here.  */
  if (cp_lexer_next_token_is (parser->lexer, CPP_GREATER))
    {
      cp_parser_error (parser, "invalid explicit specialization");
      begin_specialization ();
      parameter_list = NULL_TREE;
    }
  else
    /* Parse the template parameters.  */
    parameter_list = cp_parser_template_parameter_list (parser);

  /* Get the deferred access checks from the parameter list.  These
     will be checked once we know what is being declared, as for a
     member template the checks must be performed in the scope of the
     class containing the member.  */
  checks = get_deferred_access_checks ();

  /* Look for the `>'.  */
  cp_parser_skip_to_end_of_template_parameter_list (parser);
  /* We just processed one more parameter list.  */
  ++parser->num_template_parameter_lists;
  /* If the next token is `template', there are more template
     parameters.  */
  if (cp_lexer_next_token_is_keyword (parser->lexer,
				      RID_TEMPLATE))
    cp_parser_template_declaration_after_export (parser, member_p);
  else
    {
      /* There are no access checks when parsing a template, as we do not
	 know if a specialization will be a friend.  */
      push_deferring_access_checks (dk_no_check);
      token = cp_lexer_peek_token (parser->lexer);
      decl = cp_parser_single_declaration (parser,
					   checks,
					   member_p,
                                           /*explicit_specialization_p=*/false,
					   &friend_p);
      pop_deferring_access_checks ();

      /* If this is a member template declaration, let the front
	 end know.  */
      if (member_p && !friend_p && decl)
	{
	  if (TREE_CODE (decl) == TYPE_DECL)
	    cp_parser_check_access_in_redeclaration (decl, token->location);

	  decl = finish_member_template_decl (decl);
	}
      else if (friend_p && decl && TREE_CODE (decl) == TYPE_DECL)
	make_friend_class (current_class_type, TREE_TYPE (decl),
			   /*complain=*/true);
    }
  /* We are done with the current parameter list.  */
  --parser->num_template_parameter_lists;

  pop_deferring_access_checks ();

  /* Finish up.  */
  finish_template_decl (parameter_list);

  /* Register member declarations.  */
  if (member_p && !friend_p && decl && !DECL_CLASS_TEMPLATE_P (decl))
    finish_member_declaration (decl);
  /* For the erroneous case of a template with C linkage, we pushed an
     implicit C++ linkage scope; exit that scope now.  */
  if (need_lang_pop)
    pop_lang_context ();
  /* If DECL is a function template, we must return to parse it later.
     (Even though there is no definition, there might be default
     arguments that need handling.)  */
  if (member_p && decl
      && (TREE_CODE (decl) == FUNCTION_DECL
	  || DECL_FUNCTION_TEMPLATE_P (decl)))
    TREE_VALUE (parser->unparsed_functions_queues)
      = tree_cons (NULL_TREE, decl,
		   TREE_VALUE (parser->unparsed_functions_queues));
}

/* Perform the deferred access checks from a template-parameter-list.
   CHECKS is a TREE_LIST of access checks, as returned by
   get_deferred_access_checks.  */

static void
cp_parser_perform_template_parameter_access_checks (VEC (deferred_access_check,gc)* checks)
{
  ++processing_template_parmlist;
  perform_access_checks (checks);
  --processing_template_parmlist;
}

/* Parse a `decl-specifier-seq [opt] init-declarator [opt] ;' or
   `function-definition' sequence.  MEMBER_P is true, this declaration
   appears in a class scope.

   Returns the DECL for the declared entity.  If FRIEND_P is non-NULL,
   *FRIEND_P is set to TRUE iff the declaration is a friend.  */

static tree
cp_parser_single_declaration (cp_parser* parser,
			      VEC (deferred_access_check,gc)* checks,
			      bool member_p,
                              bool explicit_specialization_p,
			      bool* friend_p)
{
  int declares_class_or_enum;
  tree decl = NULL_TREE;
  cp_decl_specifier_seq decl_specifiers;
  bool function_definition_p = false;
  cp_token *decl_spec_token_start;

  /* This function is only used when processing a template
     declaration.  */
  gcc_assert (innermost_scope_kind () == sk_template_parms
	      || innermost_scope_kind () == sk_template_spec);

  /* Defer access checks until we know what is being declared.  */
  push_deferring_access_checks (dk_deferred);

  /* Try the `decl-specifier-seq [opt] init-declarator [opt]'
     alternative.  */
  decl_spec_token_start = cp_lexer_peek_token (parser->lexer);
  cp_parser_decl_specifier_seq (parser,
				CP_PARSER_FLAGS_OPTIONAL,
				&decl_specifiers,
				&declares_class_or_enum);
  if (friend_p)
    *friend_p = cp_parser_friend_p (&decl_specifiers);

  /* There are no template typedefs.  */
  if (decl_specifiers.specs[(int) ds_typedef])
    {
      error_at (decl_spec_token_start->location,
		"template declaration of %<typedef%>");
      decl = error_mark_node;
    }

  /* Gather up the access checks that occurred the
     decl-specifier-seq.  */
  stop_deferring_access_checks ();

  /* Check for the declaration of a template class.  */
  if (declares_class_or_enum)
    {
      if (cp_parser_declares_only_class_p (parser))
	{
	  decl = shadow_tag (&decl_specifiers);

	  /* In this case:

	       struct C {
		 friend template <typename T> struct A<T>::B;
	       };

	     A<T>::B will be represented by a TYPENAME_TYPE, and
	     therefore not recognized by shadow_tag.  */
	  if (friend_p && *friend_p
	      && !decl
	      && decl_specifiers.type
	      && TYPE_P (decl_specifiers.type))
	    decl = decl_specifiers.type;

	  if (decl && decl != error_mark_node)
	    decl = TYPE_NAME (decl);
	  else
	    decl = error_mark_node;

	  /* Perform access checks for template parameters.  */
	  cp_parser_perform_template_parameter_access_checks (checks);
	}
    }

  /* Complain about missing 'typename' or other invalid type names.  */
  if (!decl_specifiers.any_type_specifiers_p)
    cp_parser_parse_and_diagnose_invalid_type_name (parser);

  /* If it's not a template class, try for a template function.  If
     the next token is a `;', then this declaration does not declare
     anything.  But, if there were errors in the decl-specifiers, then
     the error might well have come from an attempted class-specifier.
     In that case, there's no need to warn about a missing declarator.  */
  if (!decl
      && (cp_lexer_next_token_is_not (parser->lexer, CPP_SEMICOLON)
	  || decl_specifiers.type != error_mark_node))
    {
      decl = cp_parser_init_declarator (parser,
				        &decl_specifiers,
				        checks,
				        /*function_definition_allowed_p=*/true,
				        member_p,
				        declares_class_or_enum,
				        &function_definition_p);

    /* 7.1.1-1 [dcl.stc]

       A storage-class-specifier shall not be specified in an explicit
       specialization...  */
    if (decl
        && explicit_specialization_p
        && decl_specifiers.storage_class != sc_none)
      {
        error_at (decl_spec_token_start->location,
		  "explicit template specialization cannot have a storage class");
        decl = error_mark_node;
      }
    }

  pop_deferring_access_checks ();

  /* Clear any current qualification; whatever comes next is the start
     of something new.  */
  parser->scope = NULL_TREE;
  parser->qualifying_scope = NULL_TREE;
  parser->object_scope = NULL_TREE;
  /* Look for a trailing `;' after the declaration.  */
  if (!function_definition_p
      && (decl == error_mark_node
	  || !cp_parser_require (parser, CPP_SEMICOLON, "%<;%>")))
    cp_parser_skip_to_end_of_block_or_statement (parser);

  return decl;
}

/* Parse a cast-expression that is not the operand of a unary "&".  */

static tree
cp_parser_simple_cast_expression (cp_parser *parser)
{
  return cp_parser_cast_expression (parser, /*address_p=*/false,
				    /*cast_p=*/false, NULL);
}

/* Parse a functional cast to TYPE.  Returns an expression
   representing the cast.  */

static tree
cp_parser_functional_cast (cp_parser* parser, tree type)
{
  VEC(tree,gc) *vec;
  tree expression_list;
  tree cast;
  bool nonconst_p;

  if (cp_lexer_next_token_is (parser->lexer, CPP_OPEN_BRACE))
    {
      maybe_warn_cpp0x (CPP0X_INITIALIZER_LISTS);
      expression_list = cp_parser_braced_list (parser, &nonconst_p);
      CONSTRUCTOR_IS_DIRECT_INIT (expression_list) = 1;
      if (TREE_CODE (type) == TYPE_DECL)
	type = TREE_TYPE (type);
      return finish_compound_literal (type, expression_list);
    }


  vec = cp_parser_parenthesized_expression_list (parser, false,
						 /*cast_p=*/true,
						 /*allow_expansion_p=*/true,
						 /*non_constant_p=*/NULL);
  if (vec == NULL)
    expression_list = error_mark_node;
  else
    {
      expression_list = build_tree_list_vec (vec);
      release_tree_vector (vec);
    }

  cast = build_functional_cast (type, expression_list,
                                tf_warning_or_error);
  /* [expr.const]/1: In an integral constant expression "only type
     conversions to integral or enumeration type can be used".  */
  if (TREE_CODE (type) == TYPE_DECL)
    type = TREE_TYPE (type);
  if (cast != error_mark_node
      && !cast_valid_in_integral_constant_expression_p (type)
      && (cp_parser_non_integral_constant_expression
	  (parser, "a call to a constructor")))
    return error_mark_node;
  return cast;
}

/* Save the tokens that make up the body of a member function defined
   in a class-specifier.  The DECL_SPECIFIERS and DECLARATOR have
   already been parsed.  The ATTRIBUTES are any GNU "__attribute__"
   specifiers applied to the declaration.  Returns the FUNCTION_DECL
   for the member function.  */

static tree
cp_parser_save_member_function_body (cp_parser* parser,
				     cp_decl_specifier_seq *decl_specifiers,
				     cp_declarator *declarator,
				     tree attributes)
{
  cp_token *first;
  cp_token *last;
  tree fn;

  /* Create the FUNCTION_DECL.  */
  fn = grokmethod (decl_specifiers, declarator, attributes);
  /* If something went badly wrong, bail out now.  */
  if (fn == error_mark_node)
    {
      /* If there's a function-body, skip it.  */
      if (cp_parser_token_starts_function_definition_p
	  (cp_lexer_peek_token (parser->lexer)))
	cp_parser_skip_to_end_of_block_or_statement (parser);
      return error_mark_node;
    }

  /* Remember it, if there default args to post process.  */
  cp_parser_save_default_args (parser, fn);

  /* Save away the tokens that make up the body of the
     function.  */
  first = parser->lexer->next_token;
  /* We can have braced-init-list mem-initializers before the fn body.  */
  if (cp_lexer_next_token_is (parser->lexer, CPP_COLON))
    {
      cp_lexer_consume_token (parser->lexer);
      while (cp_lexer_next_token_is_not (parser->lexer, CPP_OPEN_BRACE)
	     && cp_lexer_next_token_is_not_keyword (parser->lexer, RID_TRY))
	{
	  /* cache_group will stop after an un-nested { } pair, too.  */
	  if (cp_parser_cache_group (parser, CPP_CLOSE_PAREN, /*depth=*/0))
	    break;

	  /* variadic mem-inits have ... after the ')'.  */
	  if (cp_lexer_next_token_is (parser->lexer, CPP_ELLIPSIS))
	    cp_lexer_consume_token (parser->lexer);
	}
    }
  cp_parser_cache_group (parser, CPP_CLOSE_BRACE, /*depth=*/0);
  /* Handle function try blocks.  */
  while (cp_lexer_next_token_is_keyword (parser->lexer, RID_CATCH))
    cp_parser_cache_group (parser, CPP_CLOSE_BRACE, /*depth=*/0);
  last = parser->lexer->next_token;

  /* Save away the inline definition; we will process it when the
     class is complete.  */
  DECL_PENDING_INLINE_INFO (fn) = cp_token_cache_new (first, last);
  DECL_PENDING_INLINE_P (fn) = 1;

  /* We need to know that this was defined in the class, so that
     friend templates are handled correctly.  */
  DECL_INITIALIZED_IN_CLASS_P (fn) = 1;

  /* Add FN to the queue of functions to be parsed later.  */
  TREE_VALUE (parser->unparsed_functions_queues)
    = tree_cons (NULL_TREE, fn,
		 TREE_VALUE (parser->unparsed_functions_queues));

  return fn;
}

/* Parse a template-argument-list, as well as the trailing ">" (but
   not the opening ">").  See cp_parser_template_argument_list for the
   return value.  */

static tree
cp_parser_enclosed_template_argument_list (cp_parser* parser)
{
  tree arguments;
  tree saved_scope;
  tree saved_qualifying_scope;
  tree saved_object_scope;
  bool saved_greater_than_is_operator_p;
  int saved_unevaluated_operand;
  int saved_inhibit_evaluation_warnings;

  /* [temp.names]

     When parsing a template-id, the first non-nested `>' is taken as
     the end of the template-argument-list rather than a greater-than
     operator.  */
  saved_greater_than_is_operator_p
    = parser->greater_than_is_operator_p;
  parser->greater_than_is_operator_p = false;
  /* Parsing the argument list may modify SCOPE, so we save it
     here.  */
  saved_scope = parser->scope;
  saved_qualifying_scope = parser->qualifying_scope;
  saved_object_scope = parser->object_scope;
  /* We need to evaluate the template arguments, even though this
     template-id may be nested within a "sizeof".  */
  saved_unevaluated_operand = cp_unevaluated_operand;
  cp_unevaluated_operand = 0;
  saved_inhibit_evaluation_warnings = c_inhibit_evaluation_warnings;
  c_inhibit_evaluation_warnings = 0;
  /* Parse the template-argument-list itself.  */
  if (cp_lexer_next_token_is (parser->lexer, CPP_GREATER)
      || cp_lexer_next_token_is (parser->lexer, CPP_RSHIFT))
    arguments = NULL_TREE;
  else
    arguments = cp_parser_template_argument_list (parser);
  /* Look for the `>' that ends the template-argument-list. If we find
     a '>>' instead, it's probably just a typo.  */
  if (cp_lexer_next_token_is (parser->lexer, CPP_RSHIFT))
    {
      if (cxx_dialect != cxx98)
        {
          /* In C++0x, a `>>' in a template argument list or cast
             expression is considered to be two separate `>'
             tokens. So, change the current token to a `>', but don't
             consume it: it will be consumed later when the outer
             template argument list (or cast expression) is parsed.
             Note that this replacement of `>' for `>>' is necessary
             even if we are parsing tentatively: in the tentative
             case, after calling
             cp_parser_enclosed_template_argument_list we will always
             throw away all of the template arguments and the first
             closing `>', either because the template argument list
             was erroneous or because we are replacing those tokens
             with a CPP_TEMPLATE_ID token.  The second `>' (which will
             not have been thrown away) is needed either to close an
             outer template argument list or to complete a new-style
             cast.  */
	  cp_token *token = cp_lexer_peek_token (parser->lexer);
          token->type = CPP_GREATER;
        }
      else if (!saved_greater_than_is_operator_p)
	{
	  /* If we're in a nested template argument list, the '>>' has
	    to be a typo for '> >'. We emit the error message, but we
	    continue parsing and we push a '>' as next token, so that
	    the argument list will be parsed correctly.  Note that the
	    global source location is still on the token before the
	    '>>', so we need to say explicitly where we want it.  */
	  cp_token *token = cp_lexer_peek_token (parser->lexer);
	  error_at (token->location, "%<>>%> should be %<> >%> "
		    "within a nested template argument list");

	  token->type = CPP_GREATER;
	}
      else
	{
	  /* If this is not a nested template argument list, the '>>'
	    is a typo for '>'. Emit an error message and continue.
	    Same deal about the token location, but here we can get it
	    right by consuming the '>>' before issuing the diagnostic.  */
	  cp_token *token = cp_lexer_consume_token (parser->lexer);
	  error_at (token->location,
		    "spurious %<>>%>, use %<>%> to terminate "
		    "a template argument list");
	}
    }
  else
    cp_parser_skip_to_end_of_template_parameter_list (parser);
  /* The `>' token might be a greater-than operator again now.  */
  parser->greater_than_is_operator_p
    = saved_greater_than_is_operator_p;
  /* Restore the SAVED_SCOPE.  */
  parser->scope = saved_scope;
  parser->qualifying_scope = saved_qualifying_scope;
  parser->object_scope = saved_object_scope;
  cp_unevaluated_operand = saved_unevaluated_operand;
  c_inhibit_evaluation_warnings = saved_inhibit_evaluation_warnings;

  return arguments;
}

/* MEMBER_FUNCTION is a member function, or a friend.  If default
   arguments, or the body of the function have not yet been parsed,
   parse them now.  */

static void
cp_parser_late_parsing_for_member (cp_parser* parser, tree member_function)
{
  /* If this member is a template, get the underlying
     FUNCTION_DECL.  */
  if (DECL_FUNCTION_TEMPLATE_P (member_function))
    member_function = DECL_TEMPLATE_RESULT (member_function);

  /* There should not be any class definitions in progress at this
     point; the bodies of members are only parsed outside of all class
     definitions.  */
  gcc_assert (parser->num_classes_being_defined == 0);
  /* While we're parsing the member functions we might encounter more
     classes.  We want to handle them right away, but we don't want
     them getting mixed up with functions that are currently in the
     queue.  */
  parser->unparsed_functions_queues
    = tree_cons (NULL_TREE, NULL_TREE, parser->unparsed_functions_queues);

  /* Make sure that any template parameters are in scope.  */
  maybe_begin_member_template_processing (member_function);

  /* If the body of the function has not yet been parsed, parse it
     now.  */
  if (DECL_PENDING_INLINE_P (member_function))
    {
      tree function_scope;
      cp_token_cache *tokens;

      /* The function is no longer pending; we are processing it.  */
      tokens = DECL_PENDING_INLINE_INFO (member_function);
      DECL_PENDING_INLINE_INFO (member_function) = NULL;
      DECL_PENDING_INLINE_P (member_function) = 0;

      /* If this is a local class, enter the scope of the containing
	 function.  */
      function_scope = current_function_decl;
      if (function_scope)
	push_function_context ();

      /* Push the body of the function onto the lexer stack.  */
      cp_parser_push_lexer_for_tokens (parser, tokens);

      /* Let the front end know that we going to be defining this
	 function.  */
      start_preparsed_function (member_function, NULL_TREE,
				SF_PRE_PARSED | SF_INCLASS_INLINE);

      /* Don't do access checking if it is a templated function.  */
      if (processing_template_decl)
	push_deferring_access_checks (dk_no_check);

      /* Now, parse the body of the function.  */
      cp_parser_function_definition_after_declarator (parser,
						      /*inline_p=*/true);

      if (processing_template_decl)
	pop_deferring_access_checks ();

      /* Leave the scope of the containing function.  */
      if (function_scope)
	pop_function_context ();
      cp_parser_pop_lexer (parser);
    }

  /* Remove any template parameters from the symbol table.  */
  maybe_end_member_template_processing ();

  /* Restore the queue.  */
  parser->unparsed_functions_queues
    = TREE_CHAIN (parser->unparsed_functions_queues);
}

/* If DECL contains any default args, remember it on the unparsed
   functions queue.  */

static void
cp_parser_save_default_args (cp_parser* parser, tree decl)
{
  tree probe;

  for (probe = TYPE_ARG_TYPES (TREE_TYPE (decl));
       probe;
       probe = TREE_CHAIN (probe))
    if (TREE_PURPOSE (probe))
      {
	TREE_PURPOSE (parser->unparsed_functions_queues)
	  = tree_cons (current_class_type, decl,
		       TREE_PURPOSE (parser->unparsed_functions_queues));
	break;
      }
}

/* FN is a FUNCTION_DECL which may contains a parameter with an
   unparsed DEFAULT_ARG.  Parse the default args now.  This function
   assumes that the current scope is the scope in which the default
   argument should be processed.  */

static void
cp_parser_late_parsing_default_args (cp_parser *parser, tree fn)
{
  bool saved_local_variables_forbidden_p;
  tree parm, parmdecl;

  /* While we're parsing the default args, we might (due to the
     statement expression extension) encounter more classes.  We want
     to handle them right away, but we don't want them getting mixed
     up with default args that are currently in the queue.  */
  parser->unparsed_functions_queues
    = tree_cons (NULL_TREE, NULL_TREE, parser->unparsed_functions_queues);

  /* Local variable names (and the `this' keyword) may not appear
     in a default argument.  */
  saved_local_variables_forbidden_p = parser->local_variables_forbidden_p;
  parser->local_variables_forbidden_p = true;

  for (parm = TYPE_ARG_TYPES (TREE_TYPE (fn)),
	 parmdecl = DECL_ARGUMENTS (fn);
       parm && parm != void_list_node;
       parm = TREE_CHAIN (parm),
	 parmdecl = TREE_CHAIN (parmdecl))
    {
      cp_token_cache *tokens;
      tree default_arg = TREE_PURPOSE (parm);
      tree parsed_arg;
      VEC(tree,gc) *insts;
      tree copy;
      unsigned ix;

      if (!default_arg)
	continue;

      if (TREE_CODE (default_arg) != DEFAULT_ARG)
	/* This can happen for a friend declaration for a function
	   already declared with default arguments.  */
	continue;

       /* Push the saved tokens for the default argument onto the parser's
	  lexer stack.  */
      tokens = DEFARG_TOKENS (default_arg);
      cp_parser_push_lexer_for_tokens (parser, tokens);

      start_lambda_scope (parmdecl);

      /* Parse the assignment-expression.  */
      parsed_arg = cp_parser_assignment_expression (parser, /*cast_p=*/false, NULL);
      if (parsed_arg == error_mark_node)
	{
	  cp_parser_pop_lexer (parser);
	  continue;
	}

      if (!processing_template_decl)
	parsed_arg = check_default_argument (TREE_VALUE (parm), parsed_arg);

      TREE_PURPOSE (parm) = parsed_arg;

      /* Update any instantiations we've already created.  */
      for (insts = DEFARG_INSTANTIATIONS (default_arg), ix = 0;
	   VEC_iterate (tree, insts, ix, copy); ix++)
	TREE_PURPOSE (copy) = parsed_arg;

      finish_lambda_scope ();

      /* If the token stream has not been completely used up, then
	 there was extra junk after the end of the default
	 argument.  */
      if (!cp_lexer_next_token_is (parser->lexer, CPP_EOF))
	cp_parser_error (parser, "expected %<,%>");

      /* Revert to the main lexer.  */
      cp_parser_pop_lexer (parser);
    }

  /* Make sure no default arg is missing.  */
  check_default_args (fn);

  /* Restore the state of local_variables_forbidden_p.  */
  parser->local_variables_forbidden_p = saved_local_variables_forbidden_p;

  /* Restore the queue.  */
  parser->unparsed_functions_queues
    = TREE_CHAIN (parser->unparsed_functions_queues);
}

/* Parse the operand of `sizeof' (or a similar operator).  Returns
   either a TYPE or an expression, depending on the form of the
   input.  The KEYWORD indicates which kind of expression we have
   encountered.  */

static tree
cp_parser_sizeof_operand (cp_parser* parser, enum rid keyword)
{
  tree expr = NULL_TREE;
  const char *saved_message;
  char *tmp;
  bool saved_integral_constant_expression_p;
  bool saved_non_integral_constant_expression_p;
  bool pack_expansion_p = false;

  /* Types cannot be defined in a `sizeof' expression.  Save away the
     old message.  */
  saved_message = parser->type_definition_forbidden_message;
  /* And create the new one.  */
  tmp = concat ("types may not be defined in %<",
		IDENTIFIER_POINTER (ridpointers[keyword]),
		"%> expressions", NULL);
  parser->type_definition_forbidden_message = tmp;

  /* The restrictions on constant-expressions do not apply inside
     sizeof expressions.  */
  saved_integral_constant_expression_p
    = parser->integral_constant_expression_p;
  saved_non_integral_constant_expression_p
    = parser->non_integral_constant_expression_p;
  parser->integral_constant_expression_p = false;

  /* If it's a `...', then we are computing the length of a parameter
     pack.  */
  if (keyword == RID_SIZEOF
      && cp_lexer_next_token_is (parser->lexer, CPP_ELLIPSIS))
    {
      /* Consume the `...'.  */
      cp_lexer_consume_token (parser->lexer);
      maybe_warn_variadic_templates ();

      /* Note that this is an expansion.  */
      pack_expansion_p = true;
    }

  /* Do not actually evaluate the expression.  */
  ++cp_unevaluated_operand;
  ++c_inhibit_evaluation_warnings;
  /* If it's a `(', then we might be looking at the type-id
     construction.  */
  if (cp_lexer_next_token_is (parser->lexer, CPP_OPEN_PAREN))
    {
      tree type;
      bool saved_in_type_id_in_expr_p;

      /* We can't be sure yet whether we're looking at a type-id or an
	 expression.  */
      cp_parser_parse_tentatively (parser);
      /* Consume the `('.  */
      cp_lexer_consume_token (parser->lexer);
      /* Parse the type-id.  */
      saved_in_type_id_in_expr_p = parser->in_type_id_in_expr_p;
      parser->in_type_id_in_expr_p = true;
      type = cp_parser_type_id (parser);
      parser->in_type_id_in_expr_p = saved_in_type_id_in_expr_p;
      /* Now, look for the trailing `)'.  */
      cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>");
      /* If all went well, then we're done.  */
      if (cp_parser_parse_definitely (parser))
	{
	  cp_decl_specifier_seq decl_specs;

	  /* Build a trivial decl-specifier-seq.  */
	  clear_decl_specs (&decl_specs);
	  decl_specs.type = type;

	  /* Call grokdeclarator to figure out what type this is.  */
	  expr = grokdeclarator (NULL,
				 &decl_specs,
				 TYPENAME,
				 /*initialized=*/0,
				 /*attrlist=*/NULL);
	}
    }

  /* If the type-id production did not work out, then we must be
     looking at the unary-expression production.  */
  if (!expr)
    expr = cp_parser_unary_expression (parser, /*address_p=*/false,
				       /*cast_p=*/false, NULL);

  if (pack_expansion_p)
    /* Build a pack expansion. */
    expr = make_pack_expansion (expr);

  /* Go back to evaluating expressions.  */
  --cp_unevaluated_operand;
  --c_inhibit_evaluation_warnings;

  /* Free the message we created.  */
  free (tmp);
  /* And restore the old one.  */
  parser->type_definition_forbidden_message = saved_message;
  parser->integral_constant_expression_p
    = saved_integral_constant_expression_p;
  parser->non_integral_constant_expression_p
    = saved_non_integral_constant_expression_p;

  return expr;
}

/* If the current declaration has no declarator, return true.  */

static bool
cp_parser_declares_only_class_p (cp_parser *parser)
{
  /* If the next token is a `;' or a `,' then there is no
     declarator.  */
  return (cp_lexer_next_token_is (parser->lexer, CPP_SEMICOLON)
	  || cp_lexer_next_token_is (parser->lexer, CPP_COMMA));
}

/* Update the DECL_SPECS to reflect the storage class indicated by
   KEYWORD.  */

static void
cp_parser_set_storage_class (cp_parser *parser,
			     cp_decl_specifier_seq *decl_specs,
			     enum rid keyword,
			     location_t location)
{
  cp_storage_class storage_class;

  if (parser->in_unbraced_linkage_specification_p)
    {
      error_at (location, "invalid use of %qD in linkage specification",
		ridpointers[keyword]);
      return;
    }
  else if (decl_specs->storage_class != sc_none)
    {
      decl_specs->conflicting_specifiers_p = true;
      return;
    }

  if ((keyword == RID_EXTERN || keyword == RID_STATIC)
      && decl_specs->specs[(int) ds_thread])
    {
      error_at (location, "%<__thread%> before %qD", ridpointers[keyword]);
      decl_specs->specs[(int) ds_thread] = 0;
    }

  switch (keyword)
    {
    case RID_AUTO:
      storage_class = sc_auto;
      break;
    case RID_REGISTER:
      storage_class = sc_register;
      break;
    case RID_STATIC:
      storage_class = sc_static;
      break;
    case RID_EXTERN:
      storage_class = sc_extern;
      break;
    case RID_MUTABLE:
      storage_class = sc_mutable;
      break;
    default:
      gcc_unreachable ();
    }
  decl_specs->storage_class = storage_class;

  /* A storage class specifier cannot be applied alongside a typedef 
     specifier. If there is a typedef specifier present then set 
     conflicting_specifiers_p which will trigger an error later
     on in grokdeclarator. */
  if (decl_specs->specs[(int)ds_typedef])
    decl_specs->conflicting_specifiers_p = true;
}

/* Update the DECL_SPECS to reflect the TYPE_SPEC.  If USER_DEFINED_P
   is true, the type is a user-defined type; otherwise it is a
   built-in type specified by a keyword.  */

static void
cp_parser_set_decl_spec_type (cp_decl_specifier_seq *decl_specs,
			      tree type_spec,
			      location_t location,
			      bool user_defined_p)
{
  decl_specs->any_specifiers_p = true;

  /* If the user tries to redeclare bool, char16_t, char32_t, or wchar_t
     (with, for example, in "typedef int wchar_t;") we remember that
     this is what happened.  In system headers, we ignore these
     declarations so that G++ can work with system headers that are not
     C++-safe.  */
  if (decl_specs->specs[(int) ds_typedef]
      && !user_defined_p
      && (type_spec == boolean_type_node
	  || type_spec == char16_type_node
	  || type_spec == char32_type_node
	  || type_spec == wchar_type_node)
      && (decl_specs->type
	  || decl_specs->specs[(int) ds_long]
	  || decl_specs->specs[(int) ds_short]
	  || decl_specs->specs[(int) ds_unsigned]
	  || decl_specs->specs[(int) ds_signed]))
    {
      decl_specs->redefined_builtin_type = type_spec;
      if (!decl_specs->type)
	{
	  decl_specs->type = type_spec;
	  decl_specs->user_defined_type_p = false;
	  decl_specs->type_location = location;
	}
    }
  else if (decl_specs->type)
    decl_specs->multiple_types_p = true;
  else
    {
      decl_specs->type = type_spec;
      decl_specs->user_defined_type_p = user_defined_p;
      decl_specs->redefined_builtin_type = NULL_TREE;
      decl_specs->type_location = location;
    }
}

/* DECL_SPECIFIERS is the representation of a decl-specifier-seq.
   Returns TRUE iff `friend' appears among the DECL_SPECIFIERS.  */

static bool
cp_parser_friend_p (const cp_decl_specifier_seq *decl_specifiers)
{
  return decl_specifiers->specs[(int) ds_friend] != 0;
}

/* If the next token is of the indicated TYPE, consume it.  Otherwise,
   issue an error message indicating that TOKEN_DESC was expected.

   Returns the token consumed, if the token had the appropriate type.
   Otherwise, returns NULL.  */

static cp_token *
cp_parser_require (cp_parser* parser,
		   enum cpp_ttype type,
		   const char* token_desc)
{
  if (cp_lexer_next_token_is (parser->lexer, type))
    return cp_lexer_consume_token (parser->lexer);
  else
    {
      /* Output the MESSAGE -- unless we're parsing tentatively.  */
      if (!cp_parser_simulate_error (parser))
	{
	  char *message = concat ("expected ", token_desc, NULL);
	  cp_parser_error (parser, message);
	  free (message);
	}
      return NULL;
    }
}

/* An error message is produced if the next token is not '>'.
   All further tokens are skipped until the desired token is
   found or '{', '}', ';' or an unbalanced ')' or ']'.  */

static void
cp_parser_skip_to_end_of_template_parameter_list (cp_parser* parser)
{
  /* Current level of '< ... >'.  */
  unsigned level = 0;
  /* Ignore '<' and '>' nested inside '( ... )' or '[ ... ]'.  */
  unsigned nesting_depth = 0;

  /* Are we ready, yet?  If not, issue error message.  */
  if (cp_parser_require (parser, CPP_GREATER, "%<>%>"))
    return;

  /* Skip tokens until the desired token is found.  */
  while (true)
    {
      /* Peek at the next token.  */
      switch (cp_lexer_peek_token (parser->lexer)->type)
	{
	case CPP_LESS:
	  if (!nesting_depth)
	    ++level;
	  break;

        case CPP_RSHIFT:
          if (cxx_dialect == cxx98)
            /* C++0x views the `>>' operator as two `>' tokens, but
               C++98 does not. */
            break;
          else if (!nesting_depth && level-- == 0)
	    {
              /* We've hit a `>>' where the first `>' closes the
                 template argument list, and the second `>' is
                 spurious.  Just consume the `>>' and stop; we've
                 already produced at least one error.  */
	      cp_lexer_consume_token (parser->lexer);
	      return;
	    }
          /* Fall through for C++0x, so we handle the second `>' in
             the `>>'.  */

	case CPP_GREATER:
	  if (!nesting_depth && level-- == 0)
	    {
	      /* We've reached the token we want, consume it and stop.  */
	      cp_lexer_consume_token (parser->lexer);
	      return;
	    }
	  break;

	case CPP_OPEN_PAREN:
	case CPP_OPEN_SQUARE:
	  ++nesting_depth;
	  break;

	case CPP_CLOSE_PAREN:
	case CPP_CLOSE_SQUARE:
	  if (nesting_depth-- == 0)
	    return;
	  break;

	case CPP_EOF:
	case CPP_PRAGMA_EOL:
	case CPP_SEMICOLON:
	case CPP_OPEN_BRACE:
	case CPP_CLOSE_BRACE:
	  /* The '>' was probably forgotten, don't look further.  */
	  return;

	default:
	  break;
	}

      /* Consume this token.  */
      cp_lexer_consume_token (parser->lexer);
    }
}

/* If the next token is the indicated keyword, consume it.  Otherwise,
   issue an error message indicating that TOKEN_DESC was expected.

   Returns the token consumed, if the token had the appropriate type.
   Otherwise, returns NULL.  */

static cp_token *
cp_parser_require_keyword (cp_parser* parser,
			   enum rid keyword,
			   const char* token_desc)
{
  cp_token *token = cp_parser_require (parser, CPP_KEYWORD, token_desc);

  if (token && token->keyword != keyword)
    {
      dyn_string_t error_msg;

      /* Format the error message.  */
      error_msg = dyn_string_new (0);
      dyn_string_append_cstr (error_msg, "expected ");
      dyn_string_append_cstr (error_msg, token_desc);
      cp_parser_error (parser, error_msg->s);
      dyn_string_delete (error_msg);
      return NULL;
    }

  return token;
}

/* Returns TRUE iff TOKEN is a token that can begin the body of a
   function-definition.  */

static bool
cp_parser_token_starts_function_definition_p (cp_token* token)
{
  return (/* An ordinary function-body begins with an `{'.  */
	  token->type == CPP_OPEN_BRACE
	  /* A ctor-initializer begins with a `:'.  */
	  || token->type == CPP_COLON
	  /* A function-try-block begins with `try'.  */
	  || token->keyword == RID_TRY
	  /* The named return value extension begins with `return'.  */
	  || token->keyword == RID_RETURN);
}

/* Returns TRUE iff the next token is the ":" or "{" beginning a class
   definition.  */

static bool
cp_parser_next_token_starts_class_definition_p (cp_parser *parser)
{
  cp_token *token;

  token = cp_lexer_peek_token (parser->lexer);
  return (token->type == CPP_OPEN_BRACE || token->type == CPP_COLON);
}

/* Returns TRUE iff the next token is the "," or ">" (or `>>', in
   C++0x) ending a template-argument.  */

static bool
cp_parser_next_token_ends_template_argument_p (cp_parser *parser)
{
  cp_token *token;

  token = cp_lexer_peek_token (parser->lexer);
  return (token->type == CPP_COMMA 
          || token->type == CPP_GREATER
          || token->type == CPP_ELLIPSIS
	  || ((cxx_dialect != cxx98) && token->type == CPP_RSHIFT));
}

/* Returns TRUE iff the n-th token is a "<", or the n-th is a "[" and the
   (n+1)-th is a ":" (which is a possible digraph typo for "< ::").  */

static bool
cp_parser_nth_token_starts_template_argument_list_p (cp_parser * parser,
						     size_t n)
{
  cp_token *token;

  token = cp_lexer_peek_nth_token (parser->lexer, n);
  if (token->type == CPP_LESS)
    return true;
  /* Check for the sequence `<::' in the original code. It would be lexed as
     `[:', where `[' is a digraph, and there is no whitespace before
     `:'.  */
  if (token->type == CPP_OPEN_SQUARE && token->flags & DIGRAPH)
    {
      cp_token *token2;
      token2 = cp_lexer_peek_nth_token (parser->lexer, n+1);
      if (token2->type == CPP_COLON && !(token2->flags & PREV_WHITE))
	return true;
    }
  return false;
}

/* Returns the kind of tag indicated by TOKEN, if it is a class-key,
   or none_type otherwise.  */

static enum tag_types
cp_parser_token_is_class_key (cp_token* token)
{
  switch (token->keyword)
    {
    case RID_CLASS:
      return class_type;
    case RID_STRUCT:
      return record_type;
    case RID_UNION:
      return union_type;

    default:
      return none_type;
    }
}

/* Issue an error message if the CLASS_KEY does not match the TYPE.  */

static void
cp_parser_check_class_key (enum tag_types class_key, tree type)
{
  if ((TREE_CODE (type) == UNION_TYPE) != (class_key == union_type))
    permerror (input_location, "%qs tag used in naming %q#T",
	    class_key == union_type ? "union"
	     : class_key == record_type ? "struct" : "class",
	     type);
}

/* Issue an error message if DECL is redeclared with different
   access than its original declaration [class.access.spec/3].
   This applies to nested classes and nested class templates.
   [class.mem/1].  */

static void
cp_parser_check_access_in_redeclaration (tree decl, location_t location)
{
  if (!decl || !CLASS_TYPE_P (TREE_TYPE (decl)))
    return;

  if ((TREE_PRIVATE (decl)
       != (current_access_specifier == access_private_node))
      || (TREE_PROTECTED (decl)
	  != (current_access_specifier == access_protected_node)))
    error_at (location, "%qD redeclared with different access", decl);
}

/* Look for the `template' keyword, as a syntactic disambiguator.
   Return TRUE iff it is present, in which case it will be
   consumed.  */

static bool
cp_parser_optional_template_keyword (cp_parser *parser)
{
  if (cp_lexer_next_token_is_keyword (parser->lexer, RID_TEMPLATE))
    {
      /* The `template' keyword can only be used within templates;
	 outside templates the parser can always figure out what is a
	 template and what is not.  */
      if (!processing_template_decl)
	{
	  cp_token *token = cp_lexer_peek_token (parser->lexer);
	  error_at (token->location,
		    "%<template%> (as a disambiguator) is only allowed "
		    "within templates");
	  /* If this part of the token stream is rescanned, the same
	     error message would be generated.  So, we purge the token
	     from the stream.  */
	  cp_lexer_purge_token (parser->lexer);
	  return false;
	}
      else
	{
	  /* Consume the `template' keyword.  */
	  cp_lexer_consume_token (parser->lexer);
	  return true;
	}
    }

  return false;
}

/* The next token is a CPP_NESTED_NAME_SPECIFIER.  Consume the token,
   set PARSER->SCOPE, and perform other related actions.  */

static void
cp_parser_pre_parsed_nested_name_specifier (cp_parser *parser)
{
  int i;
  struct tree_check *check_value;
  deferred_access_check *chk;
  VEC (deferred_access_check,gc) *checks;

  /* Get the stored value.  */
  check_value = cp_lexer_consume_token (parser->lexer)->u.tree_check_value;
  /* Perform any access checks that were deferred.  */
  checks = check_value->checks;
  if (checks)
    {
      for (i = 0 ;
	   VEC_iterate (deferred_access_check, checks, i, chk) ;
	   ++i)
	{
	  perform_or_defer_access_check (chk->binfo,
					 chk->decl,
					 chk->diag_decl);
	}
    }
  /* Set the scope from the stored value.  */
  parser->scope = check_value->value;
  parser->qualifying_scope = check_value->qualifying_scope;
  parser->object_scope = NULL_TREE;
}

/* Consume tokens up through a non-nested END token.  Returns TRUE if we
   encounter the end of a block before what we were looking for.  */

static bool
cp_parser_cache_group (cp_parser *parser,
		       enum cpp_ttype end,
		       unsigned depth)
{
  while (true)
    {
      cp_token *token = cp_lexer_peek_token (parser->lexer);

      /* Abort a parenthesized expression if we encounter a semicolon.  */
      if ((end == CPP_CLOSE_PAREN || depth == 0)
	  && token->type == CPP_SEMICOLON)
	return true;
      /* If we've reached the end of the file, stop.  */
      if (token->type == CPP_EOF
	  || (end != CPP_PRAGMA_EOL
	      && token->type == CPP_PRAGMA_EOL))
	return true;
      if (token->type == CPP_CLOSE_BRACE && depth == 0)
	/* We've hit the end of an enclosing block, so there's been some
	   kind of syntax error.  */
	return true;

      /* Consume the token.  */
      cp_lexer_consume_token (parser->lexer);
      /* See if it starts a new group.  */
      if (token->type == CPP_OPEN_BRACE)
	{
	  cp_parser_cache_group (parser, CPP_CLOSE_BRACE, depth + 1);
	  /* In theory this should probably check end == '}', but
	     cp_parser_save_member_function_body needs it to exit
	     after either '}' or ')' when called with ')'.  */
	  if (depth == 0)
	    return false;
	}
      else if (token->type == CPP_OPEN_PAREN)
	{
	  cp_parser_cache_group (parser, CPP_CLOSE_PAREN, depth + 1);
	  if (depth == 0 && end == CPP_CLOSE_PAREN)
	    return false;
	}
      else if (token->type == CPP_PRAGMA)
	cp_parser_cache_group (parser, CPP_PRAGMA_EOL, depth + 1);
      else if (token->type == end)
	return false;
    }
}

/* Begin parsing tentatively.  We always save tokens while parsing
   tentatively so that if the tentative parsing fails we can restore the
   tokens.  */

static void
cp_parser_parse_tentatively (cp_parser* parser)
{
  /* Enter a new parsing context.  */
  parser->context = cp_parser_context_new (parser->context);
  /* Begin saving tokens.  */
  cp_lexer_save_tokens (parser->lexer);
  /* In order to avoid repetitive access control error messages,
     access checks are queued up until we are no longer parsing
     tentatively.  */
  push_deferring_access_checks (dk_deferred);
}

/* Commit to the currently active tentative parse.  */

static void
cp_parser_commit_to_tentative_parse (cp_parser* parser)
{
  cp_parser_context *context;
  cp_lexer *lexer;

  /* Mark all of the levels as committed.  */
  lexer = parser->lexer;
  for (context = parser->context; context->next; context = context->next)
    {
      if (context->status == CP_PARSER_STATUS_KIND_COMMITTED)
	break;
      context->status = CP_PARSER_STATUS_KIND_COMMITTED;
      while (!cp_lexer_saving_tokens (lexer))
	lexer = lexer->next;
      cp_lexer_commit_tokens (lexer);
    }
}

/* Abort the currently active tentative parse.  All consumed tokens
   will be rolled back, and no diagnostics will be issued.  */

static void
cp_parser_abort_tentative_parse (cp_parser* parser)
{
  cp_parser_simulate_error (parser);
  /* Now, pretend that we want to see if the construct was
     successfully parsed.  */
  cp_parser_parse_definitely (parser);
}

/* Stop parsing tentatively.  If a parse error has occurred, restore the
   token stream.  Otherwise, commit to the tokens we have consumed.
   Returns true if no error occurred; false otherwise.  */

static bool
cp_parser_parse_definitely (cp_parser* parser)
{
  bool error_occurred;
  cp_parser_context *context;

  /* Remember whether or not an error occurred, since we are about to
     destroy that information.  */
  error_occurred = cp_parser_error_occurred (parser);
  /* Remove the topmost context from the stack.  */
  context = parser->context;
  parser->context = context->next;
  /* If no parse errors occurred, commit to the tentative parse.  */
  if (!error_occurred)
    {
      /* Commit to the tokens read tentatively, unless that was
	 already done.  */
      if (context->status != CP_PARSER_STATUS_KIND_COMMITTED)
	cp_lexer_commit_tokens (parser->lexer);

      pop_to_parent_deferring_access_checks ();
    }
  /* Otherwise, if errors occurred, roll back our state so that things
     are just as they were before we began the tentative parse.  */
  else
    {
      cp_lexer_rollback_tokens (parser->lexer);
      pop_deferring_access_checks ();
    }
  /* Add the context to the front of the free list.  */
  context->next = cp_parser_context_free_list;
  cp_parser_context_free_list = context;

  return !error_occurred;
}

/* Returns true if we are parsing tentatively and are not committed to
   this tentative parse.  */

static bool
cp_parser_uncommitted_to_tentative_parse_p (cp_parser* parser)
{
  return (cp_parser_parsing_tentatively (parser)
	  && parser->context->status != CP_PARSER_STATUS_KIND_COMMITTED);
}

/* Returns nonzero iff an error has occurred during the most recent
   tentative parse.  */

static bool
cp_parser_error_occurred (cp_parser* parser)
{
  return (cp_parser_parsing_tentatively (parser)
	  && parser->context->status == CP_PARSER_STATUS_KIND_ERROR);
}

/* Returns nonzero if GNU extensions are allowed.  */

static bool
cp_parser_allow_gnu_extensions_p (cp_parser* parser)
{
  return parser->allow_gnu_extensions_p;
}

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
  cp_parser_require (parser, CPP_CLOSE_SQUARE, "%<]%>");

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
      cp_parser_require (parser, CPP_COLON, "%<:%>");
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
  cp_parser_require (parser, CPP_OPEN_PAREN, "%<(%>");
  token = cp_lexer_peek_token (parser->lexer);
  type = complete_type (cp_parser_type_id (parser));
  cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>");

  if (!type)
    {
      error_at (token->location, 
		"%<@encode%> must specify a type as an argument");
      return error_mark_node;
    }

  return objc_build_encode_expr (type);
}

/* Parse an Objective-C @defs expression.  */

static tree
cp_parser_objc_defs_expression (cp_parser *parser)
{
  tree name;

  cp_lexer_consume_token (parser->lexer);  /* Eat '@defs'.  */
  cp_parser_require (parser, CPP_OPEN_PAREN, "%<(%>");
  name = cp_parser_identifier (parser);
  cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>");

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
  cp_parser_require (parser, CPP_OPEN_PAREN, "%<(%>");
  proto = cp_parser_identifier (parser);
  cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>");

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
  cp_parser_require (parser, CPP_OPEN_PAREN, "%<(%>");
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
  cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>");

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
  tree list = build_tree_list (NULL_TREE, cp_parser_identifier (parser));
  cp_token *sep = cp_lexer_peek_token (parser->lexer);

  while (sep->type == CPP_COMMA)
    {
      cp_lexer_consume_token (parser->lexer);  /* Eat ','.  */
      list = chainon (list,
		      build_tree_list (NULL_TREE,
				       cp_parser_identifier (parser)));
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
      cp_parser_require (parser, CPP_GREATER, "%<>%>");
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
      objc_set_visibility (2);
      break;
    case RID_AT_PROTECTED:
      objc_set_visibility (0);
      break;
    case RID_AT_PUBLIC:
      objc_set_visibility (1);
      break;
    default:
      return;
    }

  /* Eat '@private'/'@protected'/'@public'.  */
  cp_lexer_consume_token (parser->lexer);
}

/* Parse an Objective-C method type.  */

static void
cp_parser_objc_method_type (cp_parser* parser)
{
  objc_set_method_type
   (cp_lexer_consume_token (parser->lexer)->type == CPP_PLUS
    ? PLUS_EXPR
    : MINUS_EXPR);
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
	cp_type = cp_parser_type_id (parser);

      cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>");
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
cp_parser_objc_method_keyword_params (cp_parser* parser)
{
  tree params = NULL_TREE;
  bool maybe_unary_selector_p = true;
  cp_token *token = cp_lexer_peek_token (parser->lexer);

  while (cp_parser_objc_selector_p (token->type) || token->type == CPP_COLON)
    {
      tree selector = NULL_TREE, type_name, identifier;

      if (token->type != CPP_COLON)
	selector = cp_parser_objc_selector (parser);

      /* Detect if we have a unary selector.  */
      if (maybe_unary_selector_p
	  && cp_lexer_next_token_is_not (parser->lexer, CPP_COLON))
	return selector;

      maybe_unary_selector_p = false;
      cp_parser_require (parser, CPP_COLON, "%<:%>");
      type_name = cp_parser_objc_typename (parser);
      identifier = cp_parser_identifier (parser);

      params
	= chainon (params,
		   objc_build_keyword_decl (selector,
					    type_name,
					    identifier));

      token = cp_lexer_peek_token (parser->lexer);
    }

  return params;
}

/* Parse the non-keyword Objective-C params.  */

static tree
cp_parser_objc_method_tail_params_opt (cp_parser* parser, bool *ellipsisp)
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
	  break;
	}

      parmdecl = cp_parser_parameter_declaration (parser, false, NULL);
      parm = grokdeclarator (parmdecl->declarator,
			     &parmdecl->decl_specifiers,
			     PARM, /*initialized=*/0,
			     /*attrlist=*/NULL);

      chainon (params, build_tree_list (NULL_TREE, parm));
      token = cp_lexer_peek_token (parser->lexer);
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
  /* Finally, try to parse a block-declaration, or a function-definition.  */
  else
    cp_parser_block_declaration (parser, /*statement_p=*/false);
}

/* Parse a method signature.  */

static tree
cp_parser_objc_method_signature (cp_parser* parser)
{
  tree rettype, kwdparms, optparms;
  bool ellipsis = false;

  cp_parser_objc_method_type (parser);
  rettype = cp_parser_objc_typename (parser);
  kwdparms = cp_parser_objc_method_keyword_params (parser);
  optparms = cp_parser_objc_method_tail_params_opt (parser, &ellipsis);

  return objc_build_method_signature (rettype, kwdparms, optparms, ellipsis);
}

/* Pars an Objective-C method prototype list.  */

static void
cp_parser_objc_method_prototype_list (cp_parser* parser)
{
  cp_token *token = cp_lexer_peek_token (parser->lexer);

  while (token->keyword != RID_AT_END)
    {
      if (token->type == CPP_PLUS || token->type == CPP_MINUS)
	{
	  objc_add_method_declaration
	   (cp_parser_objc_method_signature (parser));
	  cp_parser_consume_semicolon_at_end_of_statement (parser);
	}
      else
	/* Allow for interspersed non-ObjC++ code.  */
	cp_parser_objc_interstitial_code (parser);

      token = cp_lexer_peek_token (parser->lexer);
    }

  cp_lexer_consume_token (parser->lexer);  /* Eat '@end'.  */
  objc_finish_interface ();
}

/* Parse an Objective-C method definition list.  */

static void
cp_parser_objc_method_definition_list (cp_parser* parser)
{
  cp_token *token = cp_lexer_peek_token (parser->lexer);

  while (token->keyword != RID_AT_END)
    {
      tree meth;

      if (token->type == CPP_PLUS || token->type == CPP_MINUS)
	{
	  push_deferring_access_checks (dk_deferred);
	  objc_start_method_definition
	   (cp_parser_objc_method_signature (parser));

	  /* For historical reasons, we accept an optional semicolon.  */
	  if (cp_lexer_next_token_is (parser->lexer, CPP_SEMICOLON))
	    cp_lexer_consume_token (parser->lexer);

	  perform_deferred_access_checks ();
	  stop_deferring_access_checks ();
	  meth = cp_parser_function_definition_after_declarator (parser,
								 false);
	  pop_deferring_access_checks ();
	  objc_finish_method_definition (meth);
	}
      else
	/* Allow for interspersed non-ObjC++ code.  */
	cp_parser_objc_interstitial_code (parser);

      token = cp_lexer_peek_token (parser->lexer);
    }

  cp_lexer_consume_token (parser->lexer);  /* Eat '@end'.  */
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

  while (token->type != CPP_CLOSE_BRACE)
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

  cp_lexer_consume_token (parser->lexer);  /* Eat '}'.  */
  /* For historical reasons, we accept an optional semicolon.  */
  if (cp_lexer_next_token_is (parser->lexer, CPP_SEMICOLON))
    cp_lexer_consume_token (parser->lexer);
}

/* Parse an Objective-C protocol declaration.  */

static void
cp_parser_objc_protocol_declaration (cp_parser* parser)
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
      objc_declare_protocols (cp_parser_objc_identifier_list (parser));
     finish:
      cp_parser_consume_semicolon_at_end_of_statement (parser);
    }

  /* Ok, we got a full-fledged definition (or at least should).  */
  else
    {
      proto = cp_parser_identifier (parser);
      protorefs = cp_parser_objc_protocol_refs_opt (parser);
      objc_start_protocol (proto, protorefs);
      cp_parser_objc_method_prototype_list (parser);
    }
}

/* Parse an Objective-C superclass or category.  */

static void
cp_parser_objc_superclass_or_category (cp_parser *parser, tree *super,
							  tree *categ)
{
  cp_token *next = cp_lexer_peek_token (parser->lexer);

  *super = *categ = NULL_TREE;
  if (next->type == CPP_COLON)
    {
      cp_lexer_consume_token (parser->lexer);  /* Eat ':'.  */
      *super = cp_parser_identifier (parser);
    }
  else if (next->type == CPP_OPEN_PAREN)
    {
      cp_lexer_consume_token (parser->lexer);  /* Eat '('.  */
      *categ = cp_parser_identifier (parser);
      cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>");
    }
}

/* Parse an Objective-C class interface.  */

static void
cp_parser_objc_class_interface (cp_parser* parser)
{
  tree name, super, categ, protos;

  cp_lexer_consume_token (parser->lexer);  /* Eat '@interface'.  */
  name = cp_parser_identifier (parser);
  cp_parser_objc_superclass_or_category (parser, &super, &categ);
  protos = cp_parser_objc_protocol_refs_opt (parser);

  /* We have either a class or a category on our hands.  */
  if (categ)
    objc_start_category_interface (name, categ, protos);
  else
    {
      objc_start_class_interface (name, super, protos);
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

  cp_lexer_consume_token (parser->lexer);  /* Eat '@implementation'.  */
  name = cp_parser_identifier (parser);
  cp_parser_objc_superclass_or_category (parser, &super, &categ);

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
cp_parser_objc_declaration (cp_parser* parser)
{
  /* Try to figure out what kind of declaration is present.  */
  cp_token *kwd = cp_lexer_peek_token (parser->lexer);

  switch (kwd->keyword)
    {
    case RID_AT_ALIAS:
      cp_parser_objc_alias_declaration (parser);
      break;
    case RID_AT_CLASS:
      cp_parser_objc_class_declaration (parser);
      break;
    case RID_AT_PROTOCOL:
      cp_parser_objc_protocol_declaration (parser);
      break;
    case RID_AT_INTERFACE:
      cp_parser_objc_class_interface (parser);
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
     @catch ( exception-declaration ) compound-statement

   objc-finally-clause
     @finally compound-statement

   Returns NULL_TREE.  */

static tree
cp_parser_objc_try_catch_finally_statement (cp_parser *parser) {
  location_t location;
  tree stmt;

  cp_parser_require_keyword (parser, RID_AT_TRY, "%<@try%>");
  location = cp_lexer_peek_token (parser->lexer)->location;
  /* NB: The @try block needs to be wrapped in its own STATEMENT_LIST
     node, lest it get absorbed into the surrounding block.  */
  stmt = push_stmt_list ();
  cp_parser_compound_statement (parser, NULL, false);
  objc_begin_try_stmt (location, pop_stmt_list (stmt));

  while (cp_lexer_next_token_is_keyword (parser->lexer, RID_AT_CATCH))
    {
      cp_parameter_declarator *parmdecl;
      tree parm;

      cp_lexer_consume_token (parser->lexer);
      cp_parser_require (parser, CPP_OPEN_PAREN, "%<(%>");
      parmdecl = cp_parser_parameter_declaration (parser, false, NULL);
      parm = grokdeclarator (parmdecl->declarator,
			     &parmdecl->decl_specifiers,
			     PARM, /*initialized=*/0,
			     /*attrlist=*/NULL);
      cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>");
      objc_begin_catch_clause (parm);
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
cp_parser_objc_synchronized_statement (cp_parser *parser) {
  location_t location;
  tree lock, stmt;

  cp_parser_require_keyword (parser, RID_AT_SYNCHRONIZED, "%<@synchronized%>");

  location = cp_lexer_peek_token (parser->lexer)->location;
  cp_parser_require (parser, CPP_OPEN_PAREN, "%<(%>");
  lock = cp_parser_expression (parser, false, NULL);
  cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>");

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
cp_parser_objc_throw_statement (cp_parser *parser) {
  tree expr = NULL_TREE;
  location_t loc = cp_lexer_peek_token (parser->lexer)->location;

  cp_parser_require_keyword (parser, RID_AT_THROW, "%<@throw%>");

  if (cp_lexer_next_token_is_not (parser->lexer, CPP_SEMICOLON))
    expr = cp_parser_assignment_expression (parser, false, NULL);

  cp_parser_consume_semicolon_at_end_of_statement (parser);

  return objc_build_throw_stmt (loc, expr);
}

/* Parse an Objective-C statement.  */

static tree
cp_parser_objc_statement (cp_parser * parser) {
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

/* OpenMP 2.5 parsing routines.  */

/* Returns name of the next clause.
   If the clause is not recognized PRAGMA_OMP_CLAUSE_NONE is returned and
   the token is not consumed.  Otherwise appropriate pragma_omp_clause is
   returned and the token is consumed.  */

static pragma_omp_clause
cp_parser_omp_clause_name (cp_parser *parser)
{
  pragma_omp_clause result = PRAGMA_OMP_CLAUSE_NONE;

  if (cp_lexer_next_token_is_keyword (parser->lexer, RID_IF))
    result = PRAGMA_OMP_CLAUSE_IF;
  else if (cp_lexer_next_token_is_keyword (parser->lexer, RID_DEFAULT))
    result = PRAGMA_OMP_CLAUSE_DEFAULT;
  else if (cp_lexer_next_token_is_keyword (parser->lexer, RID_PRIVATE))
    result = PRAGMA_OMP_CLAUSE_PRIVATE;
  else if (cp_lexer_next_token_is (parser->lexer, CPP_NAME))
    {
      tree id = cp_lexer_peek_token (parser->lexer)->u.value;
      const char *p = IDENTIFIER_POINTER (id);

      switch (p[0])
	{
	case 'c':
	  if (!strcmp ("collapse", p))
	    result = PRAGMA_OMP_CLAUSE_COLLAPSE;
	  else if (!strcmp ("copyin", p))
	    result = PRAGMA_OMP_CLAUSE_COPYIN;
	  else if (!strcmp ("copyprivate", p))
	    result = PRAGMA_OMP_CLAUSE_COPYPRIVATE;
	  break;
	case 'f':
	  if (!strcmp ("firstprivate", p))
	    result = PRAGMA_OMP_CLAUSE_FIRSTPRIVATE;
	  break;
	case 'l':
	  if (!strcmp ("lastprivate", p))
	    result = PRAGMA_OMP_CLAUSE_LASTPRIVATE;
	  break;
	case 'n':
	  if (!strcmp ("nowait", p))
	    result = PRAGMA_OMP_CLAUSE_NOWAIT;
	  else if (!strcmp ("num_threads", p))
	    result = PRAGMA_OMP_CLAUSE_NUM_THREADS;
	  break;
	case 'o':
	  if (!strcmp ("ordered", p))
	    result = PRAGMA_OMP_CLAUSE_ORDERED;
	  break;
	case 'r':
	  if (!strcmp ("reduction", p))
	    result = PRAGMA_OMP_CLAUSE_REDUCTION;
	  break;
	case 's':
	  if (!strcmp ("schedule", p))
	    result = PRAGMA_OMP_CLAUSE_SCHEDULE;
	  else if (!strcmp ("shared", p))
	    result = PRAGMA_OMP_CLAUSE_SHARED;
	  break;
	case 'u':
	  if (!strcmp ("untied", p))
	    result = PRAGMA_OMP_CLAUSE_UNTIED;
	  break;
	}
    }

  if (result != PRAGMA_OMP_CLAUSE_NONE)
    cp_lexer_consume_token (parser->lexer);

  return result;
}

/* Validate that a clause of the given type does not already exist.  */

static void
check_no_duplicate_clause (tree clauses, enum omp_clause_code code,
			   const char *name, location_t location)
{
  tree c;

  for (c = clauses; c ; c = OMP_CLAUSE_CHAIN (c))
    if (OMP_CLAUSE_CODE (c) == code)
      {
	error_at (location, "too many %qs clauses", name);
	break;
      }
}

/* OpenMP 2.5:
   variable-list:
     identifier
     variable-list , identifier

   In addition, we match a closing parenthesis.  An opening parenthesis
   will have been consumed by the caller.

   If KIND is nonzero, create the appropriate node and install the decl
   in OMP_CLAUSE_DECL and add the node to the head of the list.

   If KIND is zero, create a TREE_LIST with the decl in TREE_PURPOSE;
   return the list created.  */

static tree
cp_parser_omp_var_list_no_open (cp_parser *parser, enum omp_clause_code kind,
				tree list)
{
  cp_token *token;
  while (1)
    {
      tree name, decl;

      token = cp_lexer_peek_token (parser->lexer);
      name = cp_parser_id_expression (parser, /*template_p=*/false,
				      /*check_dependency_p=*/true,
				      /*template_p=*/NULL,
				      /*declarator_p=*/false,
				      /*optional_p=*/false);
      if (name == error_mark_node)
	goto skip_comma;

      decl = cp_parser_lookup_name_simple (parser, name, token->location);
      if (decl == error_mark_node)
	cp_parser_name_lookup_error (parser, name, decl, NULL, token->location);
      else if (kind != 0)
	{
	  tree u = build_omp_clause (token->location, kind);
	  OMP_CLAUSE_DECL (u) = decl;
	  OMP_CLAUSE_CHAIN (u) = list;
	  list = u;
	}
      else
	list = tree_cons (decl, NULL_TREE, list);

    get_comma:
      if (cp_lexer_next_token_is_not (parser->lexer, CPP_COMMA))
	break;
      cp_lexer_consume_token (parser->lexer);
    }

  if (!cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>"))
    {
      int ending;

      /* Try to resync to an unnested comma.  Copied from
	 cp_parser_parenthesized_expression_list.  */
    skip_comma:
      ending = cp_parser_skip_to_closing_parenthesis (parser,
						      /*recovering=*/true,
						      /*or_comma=*/true,
						      /*consume_paren=*/true);
      if (ending < 0)
	goto get_comma;
    }

  return list;
}

/* Similarly, but expect leading and trailing parenthesis.  This is a very
   common case for omp clauses.  */

static tree
cp_parser_omp_var_list (cp_parser *parser, enum omp_clause_code kind, tree list)
{
  if (cp_parser_require (parser, CPP_OPEN_PAREN, "%<(%>"))
    return cp_parser_omp_var_list_no_open (parser, kind, list);
  return list;
}

/* OpenMP 3.0:
   collapse ( constant-expression ) */

static tree
cp_parser_omp_clause_collapse (cp_parser *parser, tree list, location_t location)
{
  tree c, num;
  location_t loc;
  HOST_WIDE_INT n;

  loc = cp_lexer_peek_token (parser->lexer)->location;
  if (!cp_parser_require (parser, CPP_OPEN_PAREN, "%<(%>"))
    return list;

  num = cp_parser_constant_expression (parser, false, NULL);

  if (!cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>"))
    cp_parser_skip_to_closing_parenthesis (parser, /*recovering=*/true,
					   /*or_comma=*/false,
					   /*consume_paren=*/true);

  if (num == error_mark_node)
    return list;
  num = fold_non_dependent_expr (num);
  if (!INTEGRAL_TYPE_P (TREE_TYPE (num))
      || !host_integerp (num, 0)
      || (n = tree_low_cst (num, 0)) <= 0
      || (int) n != n)
    {
      error_at (loc, "collapse argument needs positive constant integer expression");
      return list;
    }

  check_no_duplicate_clause (list, OMP_CLAUSE_COLLAPSE, "collapse", location);
  c = build_omp_clause (loc, OMP_CLAUSE_COLLAPSE);
  OMP_CLAUSE_CHAIN (c) = list;
  OMP_CLAUSE_COLLAPSE_EXPR (c) = num;

  return c;
}

/* OpenMP 2.5:
   default ( shared | none ) */

static tree
cp_parser_omp_clause_default (cp_parser *parser, tree list, location_t location)
{
  enum omp_clause_default_kind kind = OMP_CLAUSE_DEFAULT_UNSPECIFIED;
  tree c;

  if (!cp_parser_require (parser, CPP_OPEN_PAREN, "%<(%>"))
    return list;
  if (cp_lexer_next_token_is (parser->lexer, CPP_NAME))
    {
      tree id = cp_lexer_peek_token (parser->lexer)->u.value;
      const char *p = IDENTIFIER_POINTER (id);

      switch (p[0])
	{
	case 'n':
	  if (strcmp ("none", p) != 0)
	    goto invalid_kind;
	  kind = OMP_CLAUSE_DEFAULT_NONE;
	  break;

	case 's':
	  if (strcmp ("shared", p) != 0)
	    goto invalid_kind;
	  kind = OMP_CLAUSE_DEFAULT_SHARED;
	  break;

	default:
	  goto invalid_kind;
	}

      cp_lexer_consume_token (parser->lexer);
    }
  else
    {
    invalid_kind:
      cp_parser_error (parser, "expected %<none%> or %<shared%>");
    }

  if (!cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>"))
    cp_parser_skip_to_closing_parenthesis (parser, /*recovering=*/true,
					   /*or_comma=*/false,
					   /*consume_paren=*/true);

  if (kind == OMP_CLAUSE_DEFAULT_UNSPECIFIED)
    return list;

  check_no_duplicate_clause (list, OMP_CLAUSE_DEFAULT, "default", location);
  c = build_omp_clause (location, OMP_CLAUSE_DEFAULT);
  OMP_CLAUSE_CHAIN (c) = list;
  OMP_CLAUSE_DEFAULT_KIND (c) = kind;

  return c;
}

/* OpenMP 2.5:
   if ( expression ) */

static tree
cp_parser_omp_clause_if (cp_parser *parser, tree list, location_t location)
{
  tree t, c;

  if (!cp_parser_require (parser, CPP_OPEN_PAREN, "%<(%>"))
    return list;

  t = cp_parser_condition (parser);

  if (t == error_mark_node
      || !cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>"))
    cp_parser_skip_to_closing_parenthesis (parser, /*recovering=*/true,
					   /*or_comma=*/false,
					   /*consume_paren=*/true);

  check_no_duplicate_clause (list, OMP_CLAUSE_IF, "if", location);

  c = build_omp_clause (location, OMP_CLAUSE_IF);
  OMP_CLAUSE_IF_EXPR (c) = t;
  OMP_CLAUSE_CHAIN (c) = list;

  return c;
}

/* OpenMP 2.5:
   nowait */

static tree
cp_parser_omp_clause_nowait (cp_parser *parser ATTRIBUTE_UNUSED,
			     tree list, location_t location)
{
  tree c;

  check_no_duplicate_clause (list, OMP_CLAUSE_NOWAIT, "nowait", location);

  c = build_omp_clause (location, OMP_CLAUSE_NOWAIT);
  OMP_CLAUSE_CHAIN (c) = list;
  return c;
}

/* OpenMP 2.5:
   num_threads ( expression ) */

static tree
cp_parser_omp_clause_num_threads (cp_parser *parser, tree list,
				  location_t location)
{
  tree t, c;

  if (!cp_parser_require (parser, CPP_OPEN_PAREN, "%<(%>"))
    return list;

  t = cp_parser_expression (parser, false, NULL);

  if (t == error_mark_node
      || !cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>"))
    cp_parser_skip_to_closing_parenthesis (parser, /*recovering=*/true,
					   /*or_comma=*/false,
					   /*consume_paren=*/true);

  check_no_duplicate_clause (list, OMP_CLAUSE_NUM_THREADS,
			     "num_threads", location);

  c = build_omp_clause (location, OMP_CLAUSE_NUM_THREADS);
  OMP_CLAUSE_NUM_THREADS_EXPR (c) = t;
  OMP_CLAUSE_CHAIN (c) = list;

  return c;
}

/* OpenMP 2.5:
   ordered */

static tree
cp_parser_omp_clause_ordered (cp_parser *parser ATTRIBUTE_UNUSED,
			      tree list, location_t location)
{
  tree c;

  check_no_duplicate_clause (list, OMP_CLAUSE_ORDERED,
			     "ordered", location);

  c = build_omp_clause (location, OMP_CLAUSE_ORDERED);
  OMP_CLAUSE_CHAIN (c) = list;
  return c;
}

/* OpenMP 2.5:
   reduction ( reduction-operator : variable-list )

   reduction-operator:
     One of: + * - & ^ | && || */

static tree
cp_parser_omp_clause_reduction (cp_parser *parser, tree list)
{
  enum tree_code code;
  tree nlist, c;

  if (!cp_parser_require (parser, CPP_OPEN_PAREN, "%<(%>"))
    return list;

  switch (cp_lexer_peek_token (parser->lexer)->type)
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
    default:
      cp_parser_error (parser, "expected %<+%>, %<*%>, %<-%>, %<&%>, %<^%>, "
			       "%<|%>, %<&&%>, or %<||%>");
    resync_fail:
      cp_parser_skip_to_closing_parenthesis (parser, /*recovering=*/true,
					     /*or_comma=*/false,
					     /*consume_paren=*/true);
      return list;
    }
  cp_lexer_consume_token (parser->lexer);

  if (!cp_parser_require (parser, CPP_COLON, "%<:%>"))
    goto resync_fail;

  nlist = cp_parser_omp_var_list_no_open (parser, OMP_CLAUSE_REDUCTION, list);
  for (c = nlist; c != list; c = OMP_CLAUSE_CHAIN (c))
    OMP_CLAUSE_REDUCTION_CODE (c) = code;

  return nlist;
}

/* OpenMP 2.5:
   schedule ( schedule-kind )
   schedule ( schedule-kind , expression )

   schedule-kind:
     static | dynamic | guided | runtime | auto  */

static tree
cp_parser_omp_clause_schedule (cp_parser *parser, tree list, location_t location)
{
  tree c, t;

  if (!cp_parser_require (parser, CPP_OPEN_PAREN, "%<(%>"))
    return list;

  c = build_omp_clause (location, OMP_CLAUSE_SCHEDULE);

  if (cp_lexer_next_token_is (parser->lexer, CPP_NAME))
    {
      tree id = cp_lexer_peek_token (parser->lexer)->u.value;
      const char *p = IDENTIFIER_POINTER (id);

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
  else if (cp_lexer_next_token_is_keyword (parser->lexer, RID_STATIC))
    OMP_CLAUSE_SCHEDULE_KIND (c) = OMP_CLAUSE_SCHEDULE_STATIC;
  else if (cp_lexer_next_token_is_keyword (parser->lexer, RID_AUTO))
    OMP_CLAUSE_SCHEDULE_KIND (c) = OMP_CLAUSE_SCHEDULE_AUTO;
  else
    goto invalid_kind;
  cp_lexer_consume_token (parser->lexer);

  if (cp_lexer_next_token_is (parser->lexer, CPP_COMMA))
    {
      cp_token *token;
      cp_lexer_consume_token (parser->lexer);

      token = cp_lexer_peek_token (parser->lexer);
      t = cp_parser_assignment_expression (parser, false, NULL);

      if (t == error_mark_node)
	goto resync_fail;
      else if (OMP_CLAUSE_SCHEDULE_KIND (c) == OMP_CLAUSE_SCHEDULE_RUNTIME)
	error_at (token->location, "schedule %<runtime%> does not take "
		  "a %<chunk_size%> parameter");
      else if (OMP_CLAUSE_SCHEDULE_KIND (c) == OMP_CLAUSE_SCHEDULE_AUTO)
	error_at (token->location, "schedule %<auto%> does not take "
		  "a %<chunk_size%> parameter");
      else
	OMP_CLAUSE_SCHEDULE_CHUNK_EXPR (c) = t;

      if (!cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>"))
	goto resync_fail;
    }
  else if (!cp_parser_require (parser, CPP_CLOSE_PAREN, "%<,%> or %<)%>"))
    goto resync_fail;

  check_no_duplicate_clause (list, OMP_CLAUSE_SCHEDULE, "schedule", location);
  OMP_CLAUSE_CHAIN (c) = list;
  return c;

 invalid_kind:
  cp_parser_error (parser, "invalid schedule kind");
 resync_fail:
  cp_parser_skip_to_closing_parenthesis (parser, /*recovering=*/true,
					 /*or_comma=*/false,
					 /*consume_paren=*/true);
  return list;
}

/* OpenMP 3.0:
   untied */

static tree
cp_parser_omp_clause_untied (cp_parser *parser ATTRIBUTE_UNUSED,
			     tree list, location_t location)
{
  tree c;

  check_no_duplicate_clause (list, OMP_CLAUSE_UNTIED, "untied", location);

  c = build_omp_clause (location, OMP_CLAUSE_UNTIED);
  OMP_CLAUSE_CHAIN (c) = list;
  return c;
}

/* Parse all OpenMP clauses.  The set clauses allowed by the directive
   is a bitmask in MASK.  Return the list of clauses found; the result
   of clause default goes in *pdefault.  */

static tree
cp_parser_omp_all_clauses (cp_parser *parser, unsigned int mask,
			   const char *where, cp_token *pragma_tok)
{
  tree clauses = NULL;
  bool first = true;
  cp_token *token = NULL;

  while (cp_lexer_next_token_is_not (parser->lexer, CPP_PRAGMA_EOL))
    {
      pragma_omp_clause c_kind;
      const char *c_name;
      tree prev = clauses;

      if (!first && cp_lexer_next_token_is (parser->lexer, CPP_COMMA))
	cp_lexer_consume_token (parser->lexer);

      token = cp_lexer_peek_token (parser->lexer);
      c_kind = cp_parser_omp_clause_name (parser);
      first = false;

      switch (c_kind)
	{
	case PRAGMA_OMP_CLAUSE_COLLAPSE:
	  clauses = cp_parser_omp_clause_collapse (parser, clauses,
						   token->location);
	  c_name = "collapse";
	  break;
	case PRAGMA_OMP_CLAUSE_COPYIN:
	  clauses = cp_parser_omp_var_list (parser, OMP_CLAUSE_COPYIN, clauses);
	  c_name = "copyin";
	  break;
	case PRAGMA_OMP_CLAUSE_COPYPRIVATE:
	  clauses = cp_parser_omp_var_list (parser, OMP_CLAUSE_COPYPRIVATE,
					    clauses);
	  c_name = "copyprivate";
	  break;
	case PRAGMA_OMP_CLAUSE_DEFAULT:
	  clauses = cp_parser_omp_clause_default (parser, clauses,
						  token->location);
	  c_name = "default";
	  break;
	case PRAGMA_OMP_CLAUSE_FIRSTPRIVATE:
	  clauses = cp_parser_omp_var_list (parser, OMP_CLAUSE_FIRSTPRIVATE,
					    clauses);
	  c_name = "firstprivate";
	  break;
	case PRAGMA_OMP_CLAUSE_IF:
	  clauses = cp_parser_omp_clause_if (parser, clauses, token->location);
	  c_name = "if";
	  break;
	case PRAGMA_OMP_CLAUSE_LASTPRIVATE:
	  clauses = cp_parser_omp_var_list (parser, OMP_CLAUSE_LASTPRIVATE,
					    clauses);
	  c_name = "lastprivate";
	  break;
	case PRAGMA_OMP_CLAUSE_NOWAIT:
	  clauses = cp_parser_omp_clause_nowait (parser, clauses, token->location);
	  c_name = "nowait";
	  break;
	case PRAGMA_OMP_CLAUSE_NUM_THREADS:
	  clauses = cp_parser_omp_clause_num_threads (parser, clauses,
						      token->location);
	  c_name = "num_threads";
	  break;
	case PRAGMA_OMP_CLAUSE_ORDERED:
	  clauses = cp_parser_omp_clause_ordered (parser, clauses,
						  token->location);
	  c_name = "ordered";
	  break;
	case PRAGMA_OMP_CLAUSE_PRIVATE:
	  clauses = cp_parser_omp_var_list (parser, OMP_CLAUSE_PRIVATE,
					    clauses);
	  c_name = "private";
	  break;
	case PRAGMA_OMP_CLAUSE_REDUCTION:
	  clauses = cp_parser_omp_clause_reduction (parser, clauses);
	  c_name = "reduction";
	  break;
	case PRAGMA_OMP_CLAUSE_SCHEDULE:
	  clauses = cp_parser_omp_clause_schedule (parser, clauses,
						   token->location);
	  c_name = "schedule";
	  break;
	case PRAGMA_OMP_CLAUSE_SHARED:
	  clauses = cp_parser_omp_var_list (parser, OMP_CLAUSE_SHARED,
					    clauses);
	  c_name = "shared";
	  break;
	case PRAGMA_OMP_CLAUSE_UNTIED:
	  clauses = cp_parser_omp_clause_untied (parser, clauses,
						 token->location);
	  c_name = "nowait";
	  break;
	default:
	  cp_parser_error (parser, "expected %<#pragma omp%> clause");
	  goto saw_error;
	}

      if (((mask >> c_kind) & 1) == 0)
	{
	  /* Remove the invalid clause(s) from the list to avoid
	     confusing the rest of the compiler.  */
	  clauses = prev;
	  error_at (token->location, "%qs is not valid for %qs", c_name, where);
	}
    }
 saw_error:
  cp_parser_skip_to_pragma_eol (parser, pragma_tok);
  return finish_omp_clauses (clauses);
}

/* OpenMP 2.5:
   structured-block:
     statement

   In practice, we're also interested in adding the statement to an
   outer node.  So it is convenient if we work around the fact that
   cp_parser_statement calls add_stmt.  */

static unsigned
cp_parser_begin_omp_structured_block (cp_parser *parser)
{
  unsigned save = parser->in_statement;

  /* Only move the values to IN_OMP_BLOCK if they weren't false.
     This preserves the "not within loop or switch" style error messages
     for nonsense cases like
	void foo() {
	#pragma omp single
	  break;
	}
  */
  if (parser->in_statement)
    parser->in_statement = IN_OMP_BLOCK;

  return save;
}

static void
cp_parser_end_omp_structured_block (cp_parser *parser, unsigned save)
{
  parser->in_statement = save;
}

static tree
cp_parser_omp_structured_block (cp_parser *parser)
{
  tree stmt = begin_omp_structured_block ();
  unsigned int save = cp_parser_begin_omp_structured_block (parser);

  cp_parser_statement (parser, NULL_TREE, false, NULL);

  cp_parser_end_omp_structured_block (parser, save);
  return finish_omp_structured_block (stmt);
}

/* OpenMP 2.5:
   # pragma omp atomic new-line
     expression-stmt

   expression-stmt:
     x binop= expr | x++ | ++x | x-- | --x
   binop:
     +, *, -, /, &, ^, |, <<, >>

  where x is an lvalue expression with scalar type.  */

static void
cp_parser_omp_atomic (cp_parser *parser, cp_token *pragma_tok)
{
  tree lhs, rhs;
  enum tree_code code;

  cp_parser_require_pragma_eol (parser, pragma_tok);

  lhs = cp_parser_unary_expression (parser, /*address_p=*/false,
				    /*cast_p=*/false, NULL);
  switch (TREE_CODE (lhs))
    {
    case ERROR_MARK:
      goto saw_error;

    case PREINCREMENT_EXPR:
    case POSTINCREMENT_EXPR:
      lhs = TREE_OPERAND (lhs, 0);
      code = PLUS_EXPR;
      rhs = integer_one_node;
      break;

    case PREDECREMENT_EXPR:
    case POSTDECREMENT_EXPR:
      lhs = TREE_OPERAND (lhs, 0);
      code = MINUS_EXPR;
      rhs = integer_one_node;
      break;

    default:
      switch (cp_lexer_peek_token (parser->lexer)->type)
	{
	case CPP_MULT_EQ:
	  code = MULT_EXPR;
	  break;
	case CPP_DIV_EQ:
	  code = TRUNC_DIV_EXPR;
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
	case CPP_OR_EQ:
	  code = BIT_IOR_EXPR;
	  break;
	case CPP_XOR_EQ:
	  code = BIT_XOR_EXPR;
	  break;
	default:
	  cp_parser_error (parser,
			   "invalid operator for %<#pragma omp atomic%>");
	  goto saw_error;
	}
      cp_lexer_consume_token (parser->lexer);

      rhs = cp_parser_expression (parser, false, NULL);
      if (rhs == error_mark_node)
	goto saw_error;
      break;
    }
  finish_omp_atomic (code, lhs, rhs);
  cp_parser_consume_semicolon_at_end_of_statement (parser);
  return;

 saw_error:
  cp_parser_skip_to_end_of_block_or_statement (parser);
}


/* OpenMP 2.5:
   # pragma omp barrier new-line  */

static void
cp_parser_omp_barrier (cp_parser *parser, cp_token *pragma_tok)
{
  cp_parser_require_pragma_eol (parser, pragma_tok);
  finish_omp_barrier ();
}

/* OpenMP 2.5:
   # pragma omp critical [(name)] new-line
     structured-block  */

static tree
cp_parser_omp_critical (cp_parser *parser, cp_token *pragma_tok)
{
  tree stmt, name = NULL;

  if (cp_lexer_next_token_is (parser->lexer, CPP_OPEN_PAREN))
    {
      cp_lexer_consume_token (parser->lexer);

      name = cp_parser_identifier (parser);

      if (name == error_mark_node
	  || !cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>"))
	cp_parser_skip_to_closing_parenthesis (parser, /*recovering=*/true,
					       /*or_comma=*/false,
					       /*consume_paren=*/true);
      if (name == error_mark_node)
	name = NULL;
    }
  cp_parser_require_pragma_eol (parser, pragma_tok);

  stmt = cp_parser_omp_structured_block (parser);
  return c_finish_omp_critical (input_location, stmt, name);
}

/* OpenMP 2.5:
   # pragma omp flush flush-vars[opt] new-line

   flush-vars:
     ( variable-list ) */

static void
cp_parser_omp_flush (cp_parser *parser, cp_token *pragma_tok)
{
  if (cp_lexer_next_token_is (parser->lexer, CPP_OPEN_PAREN))
    (void) cp_parser_omp_var_list (parser, OMP_CLAUSE_ERROR, NULL);
  cp_parser_require_pragma_eol (parser, pragma_tok);

  finish_omp_flush ();
}

/* Helper function, to parse omp for increment expression.  */

static tree
cp_parser_omp_for_cond (cp_parser *parser, tree decl)
{
  tree cond = cp_parser_binary_expression (parser, false, true,
					   PREC_NOT_OPERATOR, NULL);
  bool overloaded_p;

  if (cond == error_mark_node
      || cp_lexer_next_token_is_not (parser->lexer, CPP_SEMICOLON))
    {
      cp_parser_skip_to_end_of_statement (parser);
      return error_mark_node;
    }

  switch (TREE_CODE (cond))
    {
    case GT_EXPR:
    case GE_EXPR:
    case LT_EXPR:
    case LE_EXPR:
      break;
    default:
      return error_mark_node;
    }

  /* If decl is an iterator, preserve LHS and RHS of the relational
     expr until finish_omp_for.  */
  if (decl
      && (type_dependent_expression_p (decl)
	  || CLASS_TYPE_P (TREE_TYPE (decl))))
    return cond;

  return build_x_binary_op (TREE_CODE (cond),
			    TREE_OPERAND (cond, 0), ERROR_MARK,
			    TREE_OPERAND (cond, 1), ERROR_MARK,
			    &overloaded_p, tf_warning_or_error);
}

/* Helper function, to parse omp for increment expression.  */

static tree
cp_parser_omp_for_incr (cp_parser *parser, tree decl)
{
  cp_token *token = cp_lexer_peek_token (parser->lexer);
  enum tree_code op;
  tree lhs, rhs;
  cp_id_kind idk;
  bool decl_first;

  if (token->type == CPP_PLUS_PLUS || token->type == CPP_MINUS_MINUS)
    {
      op = (token->type == CPP_PLUS_PLUS
	    ? PREINCREMENT_EXPR : PREDECREMENT_EXPR);
      cp_lexer_consume_token (parser->lexer);
      lhs = cp_parser_cast_expression (parser, false, false, NULL);
      if (lhs != decl)
	return error_mark_node;
      return build2 (op, TREE_TYPE (decl), decl, NULL_TREE);
    }

  lhs = cp_parser_primary_expression (parser, false, false, false, &idk);
  if (lhs != decl)
    return error_mark_node;

  token = cp_lexer_peek_token (parser->lexer);
  if (token->type == CPP_PLUS_PLUS || token->type == CPP_MINUS_MINUS)
    {
      op = (token->type == CPP_PLUS_PLUS
	    ? POSTINCREMENT_EXPR : POSTDECREMENT_EXPR);
      cp_lexer_consume_token (parser->lexer);
      return build2 (op, TREE_TYPE (decl), decl, NULL_TREE);
    }

  op = cp_parser_assignment_operator_opt (parser);
  if (op == ERROR_MARK)
    return error_mark_node;

  if (op != NOP_EXPR)
    {
      rhs = cp_parser_assignment_expression (parser, false, NULL);
      rhs = build2 (op, TREE_TYPE (decl), decl, rhs);
      return build2 (MODIFY_EXPR, TREE_TYPE (decl), decl, rhs);
    }

  lhs = cp_parser_binary_expression (parser, false, false,
				     PREC_ADDITIVE_EXPRESSION, NULL);
  token = cp_lexer_peek_token (parser->lexer);
  decl_first = lhs == decl;
  if (decl_first)
    lhs = NULL_TREE;
  if (token->type != CPP_PLUS
      && token->type != CPP_MINUS)
    return error_mark_node;

  do
    {
      op = token->type == CPP_PLUS ? PLUS_EXPR : MINUS_EXPR;
      cp_lexer_consume_token (parser->lexer);
      rhs = cp_parser_binary_expression (parser, false, false,
					 PREC_ADDITIVE_EXPRESSION, NULL);
      token = cp_lexer_peek_token (parser->lexer);
      if (token->type == CPP_PLUS || token->type == CPP_MINUS || decl_first)
	{
	  if (lhs == NULL_TREE)
	    {
	      if (op == PLUS_EXPR)
		lhs = rhs;
	      else
		lhs = build_x_unary_op (NEGATE_EXPR, rhs, tf_warning_or_error);
	    }
	  else
	    lhs = build_x_binary_op (op, lhs, ERROR_MARK, rhs, ERROR_MARK,
				     NULL, tf_warning_or_error);
	}
    }
  while (token->type == CPP_PLUS || token->type == CPP_MINUS);

  if (!decl_first)
    {
      if (rhs != decl || op == MINUS_EXPR)
	return error_mark_node;
      rhs = build2 (op, TREE_TYPE (decl), lhs, decl);
    }
  else
    rhs = build2 (PLUS_EXPR, TREE_TYPE (decl), decl, lhs);

  return build2 (MODIFY_EXPR, TREE_TYPE (decl), decl, rhs);
}

/* Parse the restricted form of the for statement allowed by OpenMP.  */

static tree
cp_parser_omp_for_loop (cp_parser *parser, tree clauses, tree *par_clauses)
{
  tree init, cond, incr, body, decl, pre_body = NULL_TREE, ret;
  tree for_block = NULL_TREE, real_decl, initv, condv, incrv, declv;
  tree this_pre_body, cl;
  location_t loc_first;
  bool collapse_err = false;
  int i, collapse = 1, nbraces = 0;

  for (cl = clauses; cl; cl = OMP_CLAUSE_CHAIN (cl))
    if (OMP_CLAUSE_CODE (cl) == OMP_CLAUSE_COLLAPSE)
      collapse = tree_low_cst (OMP_CLAUSE_COLLAPSE_EXPR (cl), 0);

  gcc_assert (collapse >= 1);

  declv = make_tree_vec (collapse);
  initv = make_tree_vec (collapse);
  condv = make_tree_vec (collapse);
  incrv = make_tree_vec (collapse);

  loc_first = cp_lexer_peek_token (parser->lexer)->location;

  for (i = 0; i < collapse; i++)
    {
      int bracecount = 0;
      bool add_private_clause = false;
      location_t loc;

      if (!cp_lexer_next_token_is_keyword (parser->lexer, RID_FOR))
	{
	  cp_parser_error (parser, "for statement expected");
	  return NULL;
	}
      loc = cp_lexer_consume_token (parser->lexer)->location;

      if (!cp_parser_require (parser, CPP_OPEN_PAREN, "%<(%>"))
	return NULL;

      init = decl = real_decl = NULL;
      this_pre_body = push_stmt_list ();
      if (cp_lexer_next_token_is_not (parser->lexer, CPP_SEMICOLON))
	{
	  /* See 2.5.1 (in OpenMP 3.0, similar wording is in 2.5 standard too):

	     init-expr:
	               var = lb
		       integer-type var = lb
		       random-access-iterator-type var = lb
		       pointer-type var = lb
	  */
	  cp_decl_specifier_seq type_specifiers;

	  /* First, try to parse as an initialized declaration.  See
	     cp_parser_condition, from whence the bulk of this is copied.  */

	  cp_parser_parse_tentatively (parser);
	  cp_parser_type_specifier_seq (parser, /*is_declaration=*/true,
					/*is_trailing_return=*/false,
					&type_specifiers);
	  if (cp_parser_parse_definitely (parser))
	    {
	      /* If parsing a type specifier seq succeeded, then this
		 MUST be a initialized declaration.  */
	      tree asm_specification, attributes;
	      cp_declarator *declarator;

	      declarator = cp_parser_declarator (parser,
						 CP_PARSER_DECLARATOR_NAMED,
						 /*ctor_dtor_or_conv_p=*/NULL,
						 /*parenthesized_p=*/NULL,
						 /*member_p=*/false);
	      attributes = cp_parser_attributes_opt (parser);
	      asm_specification = cp_parser_asm_specification_opt (parser);

	      if (declarator == cp_error_declarator) 
		cp_parser_skip_to_end_of_statement (parser);

	      else 
		{
		  tree pushed_scope, auto_node;

		  decl = start_decl (declarator, &type_specifiers,
				     SD_INITIALIZED, attributes,
				     /*prefix_attributes=*/NULL_TREE,
				     &pushed_scope);

		  auto_node = type_uses_auto (TREE_TYPE (decl));
		  if (cp_lexer_next_token_is_not (parser->lexer, CPP_EQ))
		    {
		      if (cp_lexer_next_token_is (parser->lexer, 
						  CPP_OPEN_PAREN))
			error ("parenthesized initialization is not allowed in "
			       "OpenMP %<for%> loop");
		      else
			/* Trigger an error.  */
			cp_parser_require (parser, CPP_EQ, "%<=%>");

		      init = error_mark_node;
		      cp_parser_skip_to_end_of_statement (parser);
		    }
		  else if (CLASS_TYPE_P (TREE_TYPE (decl))
			   || type_dependent_expression_p (decl)
			   || auto_node)
		    {
		      bool is_direct_init, is_non_constant_init;

		      init = cp_parser_initializer (parser,
						    &is_direct_init,
						    &is_non_constant_init);

		      if (auto_node && describable_type (init))
			{
			  TREE_TYPE (decl)
			    = do_auto_deduction (TREE_TYPE (decl), init,
						 auto_node);

			  if (!CLASS_TYPE_P (TREE_TYPE (decl))
			      && !type_dependent_expression_p (decl))
			    goto non_class;
			}
		      
		      cp_finish_decl (decl, init, !is_non_constant_init,
				      asm_specification,
				      LOOKUP_ONLYCONVERTING);
		      if (CLASS_TYPE_P (TREE_TYPE (decl)))
			{
			  for_block
			    = tree_cons (NULL, this_pre_body, for_block);
			  init = NULL_TREE;
			}
		      else
			init = pop_stmt_list (this_pre_body);
		      this_pre_body = NULL_TREE;
		    }
		  else
		    {
		      /* Consume '='.  */
		      cp_lexer_consume_token (parser->lexer);
		      init = cp_parser_assignment_expression (parser, false, NULL);

		    non_class:
		      if (TREE_CODE (TREE_TYPE (decl)) == REFERENCE_TYPE)
			init = error_mark_node;
		      else
			cp_finish_decl (decl, NULL_TREE,
					/*init_const_expr_p=*/false,
					asm_specification,
					LOOKUP_ONLYCONVERTING);
		    }

		  if (pushed_scope)
		    pop_scope (pushed_scope);
		}
	    }
	  else 
	    {
	      cp_id_kind idk;
	      /* If parsing a type specifier sequence failed, then
		 this MUST be a simple expression.  */
	      cp_parser_parse_tentatively (parser);
	      decl = cp_parser_primary_expression (parser, false, false,
						   false, &idk);
	      if (!cp_parser_error_occurred (parser)
		  && decl
		  && DECL_P (decl)
		  && CLASS_TYPE_P (TREE_TYPE (decl)))
		{
		  tree rhs;

		  cp_parser_parse_definitely (parser);
		  cp_parser_require (parser, CPP_EQ, "%<=%>");
		  rhs = cp_parser_assignment_expression (parser, false, NULL);
		  finish_expr_stmt (build_x_modify_expr (decl, NOP_EXPR,
							 rhs,
							 tf_warning_or_error));
		  add_private_clause = true;
		}
	      else
		{
		  decl = NULL;
		  cp_parser_abort_tentative_parse (parser);
		  init = cp_parser_expression (parser, false, NULL);
		  if (init)
		    {
		      if (TREE_CODE (init) == MODIFY_EXPR
			  || TREE_CODE (init) == MODOP_EXPR)
			real_decl = TREE_OPERAND (init, 0);
		    }
		}
	    }
	}
      cp_parser_require (parser, CPP_SEMICOLON, "%<;%>");
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

      if (decl)
	real_decl = decl;
      if (par_clauses != NULL && real_decl != NULL_TREE)
	{
	  tree *c;
	  for (c = par_clauses; *c ; )
	    if (OMP_CLAUSE_CODE (*c) == OMP_CLAUSE_FIRSTPRIVATE
		&& OMP_CLAUSE_DECL (*c) == real_decl)
	      {
		error_at (loc, "iteration variable %qD"
			  " should not be firstprivate", real_decl);
		*c = OMP_CLAUSE_CHAIN (*c);
	      }
	    else if (OMP_CLAUSE_CODE (*c) == OMP_CLAUSE_LASTPRIVATE
		     && OMP_CLAUSE_DECL (*c) == real_decl)
	      {
		/* Add lastprivate (decl) clause to OMP_FOR_CLAUSES,
		   change it to shared (decl) in OMP_PARALLEL_CLAUSES.  */
		tree l = build_omp_clause (loc, OMP_CLAUSE_LASTPRIVATE);
		OMP_CLAUSE_DECL (l) = real_decl;
		OMP_CLAUSE_CHAIN (l) = clauses;
		CP_OMP_CLAUSE_INFO (l) = CP_OMP_CLAUSE_INFO (*c);
		clauses = l;
		OMP_CLAUSE_SET_CODE (*c, OMP_CLAUSE_SHARED);
		CP_OMP_CLAUSE_INFO (*c) = NULL;
		add_private_clause = false;
	      }
	    else
	      {
		if (OMP_CLAUSE_CODE (*c) == OMP_CLAUSE_PRIVATE
		    && OMP_CLAUSE_DECL (*c) == real_decl)
		  add_private_clause = false;
		c = &OMP_CLAUSE_CHAIN (*c);
	      }
	}

      if (add_private_clause)
	{
	  tree c;
	  for (c = clauses; c ; c = OMP_CLAUSE_CHAIN (c))
	    {
	      if ((OMP_CLAUSE_CODE (c) == OMP_CLAUSE_PRIVATE
		   || OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LASTPRIVATE)
		  && OMP_CLAUSE_DECL (c) == decl)
		break;
	      else if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_FIRSTPRIVATE
		       && OMP_CLAUSE_DECL (c) == decl)
		error_at (loc, "iteration variable %qD "
			  "should not be firstprivate",
			  decl);
	      else if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_REDUCTION
		       && OMP_CLAUSE_DECL (c) == decl)
		error_at (loc, "iteration variable %qD should not be reduction",
			  decl);
	    }
	  if (c == NULL)
	    {
	      c = build_omp_clause (loc, OMP_CLAUSE_PRIVATE);
	      OMP_CLAUSE_DECL (c) = decl;
	      c = finish_omp_clauses (c);
	      if (c)
		{
		  OMP_CLAUSE_CHAIN (c) = clauses;
		  clauses = c;
		}
	    }
	}

      cond = NULL;
      if (cp_lexer_next_token_is_not (parser->lexer, CPP_SEMICOLON))
	cond = cp_parser_omp_for_cond (parser, decl);
      cp_parser_require (parser, CPP_SEMICOLON, "%<;%>");

      incr = NULL;
      if (cp_lexer_next_token_is_not (parser->lexer, CPP_CLOSE_PAREN))
	{
	  /* If decl is an iterator, preserve the operator on decl
	     until finish_omp_for.  */
	  if (decl
	      && (type_dependent_expression_p (decl)
		  || CLASS_TYPE_P (TREE_TYPE (decl))))
	    incr = cp_parser_omp_for_incr (parser, decl);
	  else
	    incr = cp_parser_expression (parser, false, NULL);
	}

      if (!cp_parser_require (parser, CPP_CLOSE_PAREN, "%<)%>"))
	cp_parser_skip_to_closing_parenthesis (parser, /*recovering=*/true,
					       /*or_comma=*/false,
					       /*consume_paren=*/true);

      TREE_VEC_ELT (declv, i) = decl;
      TREE_VEC_ELT (initv, i) = init;
      TREE_VEC_ELT (condv, i) = cond;
      TREE_VEC_ELT (incrv, i) = incr;

      if (i == collapse - 1)
	break;

      /* FIXME: OpenMP 3.0 draft isn't very clear on what exactly is allowed
	 in between the collapsed for loops to be still considered perfectly
	 nested.  Hopefully the final version clarifies this.
	 For now handle (multiple) {'s and empty statements.  */
      cp_parser_parse_tentatively (parser);
      do
	{
	  if (cp_lexer_next_token_is_keyword (parser->lexer, RID_FOR))
	    break;
	  else if (cp_lexer_next_token_is (parser->lexer, CPP_OPEN_BRACE))
	    {
	      cp_lexer_consume_token (parser->lexer);
	      bracecount++;
	    }
	  else if (bracecount
		   && cp_lexer_next_token_is (parser->lexer, CPP_SEMICOLON))
	    cp_lexer_consume_token (parser->lexer);
	  else
	    {
	      loc = cp_lexer_peek_token (parser->lexer)->location;
	      error_at (loc, "not enough collapsed for loops");
	      collapse_err = true;
	      cp_parser_abort_tentative_parse (parser);
	      declv = NULL_TREE;
	      break;
	    }
	}
      while (1);

      if (declv)
	{
	  cp_parser_parse_definitely (parser);
	  nbraces += bracecount;
	}
    }

  /* Note that we saved the original contents of this flag when we entered
     the structured block, and so we don't need to re-save it here.  */
  parser->in_statement = IN_OMP_FOR;

  /* Note that the grammar doesn't call for a structured block here,
     though the loop as a whole is a structured block.  */
  body = push_stmt_list ();
  cp_parser_statement (parser, NULL_TREE, false, NULL);
  body = pop_stmt_list (body);

  if (declv == NULL_TREE)
    ret = NULL_TREE;
  else
    ret = finish_omp_for (loc_first, declv, initv, condv, incrv, body,
			  pre_body, clauses);

  while (nbraces)
    {
      if (cp_lexer_next_token_is (parser->lexer, CPP_CLOSE_BRACE))
	{
	  cp_lexer_consume_token (parser->lexer);
	  nbraces--;
	}
      else if (cp_lexer_next_token_is (parser->lexer, CPP_SEMICOLON))
	cp_lexer_consume_token (parser->lexer);
      else
	{
	  if (!collapse_err)
	    {
	      error_at (cp_lexer_peek_token (parser->lexer)->location,
			"collapsed loops not perfectly nested");
	    }
	  collapse_err = true;
	  cp_parser_statement_seq_opt (parser, NULL);
	  if (cp_lexer_next_token_is (parser->lexer, CPP_EOF))
	    break;
	}
    }

  while (for_block)
    {
      add_stmt (pop_stmt_list (TREE_VALUE (for_block)));
      for_block = TREE_CHAIN (for_block);
    }

  return ret;
}

/* OpenMP 2.5:
   #pragma omp for for-clause[optseq] new-line
     for-loop  */

#define OMP_FOR_CLAUSE_MASK				\
	( (1u << PRAGMA_OMP_CLAUSE_PRIVATE)		\
	| (1u << PRAGMA_OMP_CLAUSE_FIRSTPRIVATE)	\
	| (1u << PRAGMA_OMP_CLAUSE_LASTPRIVATE)		\
	| (1u << PRAGMA_OMP_CLAUSE_REDUCTION)		\
	| (1u << PRAGMA_OMP_CLAUSE_ORDERED)		\
	| (1u << PRAGMA_OMP_CLAUSE_SCHEDULE)		\
	| (1u << PRAGMA_OMP_CLAUSE_NOWAIT)		\
	| (1u << PRAGMA_OMP_CLAUSE_COLLAPSE))

static tree
cp_parser_omp_for (cp_parser *parser, cp_token *pragma_tok)
{
  tree clauses, sb, ret;
  unsigned int save;

  clauses = cp_parser_omp_all_clauses (parser, OMP_FOR_CLAUSE_MASK,
				       "#pragma omp for", pragma_tok);

  sb = begin_omp_structured_block ();
  save = cp_parser_begin_omp_structured_block (parser);

  ret = cp_parser_omp_for_loop (parser, clauses, NULL);

  cp_parser_end_omp_structured_block (parser, save);
  add_stmt (finish_omp_structured_block (sb));

  return ret;
}

/* OpenMP 2.5:
   # pragma omp master new-line
     structured-block  */

static tree
cp_parser_omp_master (cp_parser *parser, cp_token *pragma_tok)
{
  cp_parser_require_pragma_eol (parser, pragma_tok);
  return c_finish_omp_master (input_location,
			      cp_parser_omp_structured_block (parser));
}

/* OpenMP 2.5:
   # pragma omp ordered new-line
     structured-block  */

static tree
cp_parser_omp_ordered (cp_parser *parser, cp_token *pragma_tok)
{
  location_t loc = cp_lexer_peek_token (parser->lexer)->location;
  cp_parser_require_pragma_eol (parser, pragma_tok);
  return c_finish_omp_ordered (loc, cp_parser_omp_structured_block (parser));
}

/* OpenMP 2.5:

   section-scope:
     { section-sequence }

   section-sequence:
     section-directive[opt] structured-block
     section-sequence section-directive structured-block  */

static tree
cp_parser_omp_sections_scope (cp_parser *parser)
{
  tree stmt, substmt;
  bool error_suppress = false;
  cp_token *tok;

  if (!cp_parser_require (parser, CPP_OPEN_BRACE, "%<{%>"))
    return NULL_TREE;

  stmt = push_stmt_list ();

  if (cp_lexer_peek_token (parser->lexer)->pragma_kind != PRAGMA_OMP_SECTION)
    {
      unsigned save;

      substmt = begin_omp_structured_block ();
      save = cp_parser_begin_omp_structured_block (parser);

      while (1)
	{
	  cp_parser_statement (parser, NULL_TREE, false, NULL);

	  tok = cp_lexer_peek_token (parser->lexer);
	  if (tok->pragma_kind == PRAGMA_OMP_SECTION)
	    break;
	  if (tok->type == CPP_CLOSE_BRACE)
	    break;
	  if (tok->type == CPP_EOF)
	    break;
	}

      cp_parser_end_omp_structured_block (parser, save);
      substmt = finish_omp_structured_block (substmt);
      substmt = build1 (OMP_SECTION, void_type_node, substmt);
      add_stmt (substmt);
    }

  while (1)
    {
      tok = cp_lexer_peek_token (parser->lexer);
      if (tok->type == CPP_CLOSE_BRACE)
	break;
      if (tok->type == CPP_EOF)
	break;

      if (tok->pragma_kind == PRAGMA_OMP_SECTION)
	{
	  cp_lexer_consume_token (parser->lexer);
	  cp_parser_require_pragma_eol (parser, tok);
	  error_suppress = false;
	}
      else if (!error_suppress)
	{
	  cp_parser_error (parser, "expected %<#pragma omp section%> or %<}%>");
	  error_suppress = true;
	}

      substmt = cp_parser_omp_structured_block (parser);
      substmt = build1 (OMP_SECTION, void_type_node, substmt);
      add_stmt (substmt);
    }
  cp_parser_require (parser, CPP_CLOSE_BRACE, "%<}%>");

  substmt = pop_stmt_list (stmt);

  stmt = make_node (OMP_SECTIONS);
  TREE_TYPE (stmt) = void_type_node;
  OMP_SECTIONS_BODY (stmt) = substmt;

  add_stmt (stmt);
  return stmt;
}

/* OpenMP 2.5:
   # pragma omp sections sections-clause[optseq] newline
     sections-scope  */

#define OMP_SECTIONS_CLAUSE_MASK			\
	( (1u << PRAGMA_OMP_CLAUSE_PRIVATE)		\
	| (1u << PRAGMA_OMP_CLAUSE_FIRSTPRIVATE)	\
	| (1u << PRAGMA_OMP_CLAUSE_LASTPRIVATE)		\
	| (1u << PRAGMA_OMP_CLAUSE_REDUCTION)		\
	| (1u << PRAGMA_OMP_CLAUSE_NOWAIT))

static tree
cp_parser_omp_sections (cp_parser *parser, cp_token *pragma_tok)
{
  tree clauses, ret;

  clauses = cp_parser_omp_all_clauses (parser, OMP_SECTIONS_CLAUSE_MASK,
				       "#pragma omp sections", pragma_tok);

  ret = cp_parser_omp_sections_scope (parser);
  if (ret)
    OMP_SECTIONS_CLAUSES (ret) = clauses;

  return ret;
}

/* OpenMP 2.5:
   # pragma parallel parallel-clause new-line
   # pragma parallel for parallel-for-clause new-line
   # pragma parallel sections parallel-sections-clause new-line  */

#define OMP_PARALLEL_CLAUSE_MASK			\
	( (1u << PRAGMA_OMP_CLAUSE_IF)			\
	| (1u << PRAGMA_OMP_CLAUSE_PRIVATE)		\
	| (1u << PRAGMA_OMP_CLAUSE_FIRSTPRIVATE)	\
	| (1u << PRAGMA_OMP_CLAUSE_DEFAULT)		\
	| (1u << PRAGMA_OMP_CLAUSE_SHARED)		\
	| (1u << PRAGMA_OMP_CLAUSE_COPYIN)		\
	| (1u << PRAGMA_OMP_CLAUSE_REDUCTION)		\
	| (1u << PRAGMA_OMP_CLAUSE_NUM_THREADS))

static tree
cp_parser_omp_parallel (cp_parser *parser, cp_token *pragma_tok)
{
  enum pragma_kind p_kind = PRAGMA_OMP_PARALLEL;
  const char *p_name = "#pragma omp parallel";
  tree stmt, clauses, par_clause, ws_clause, block;
  unsigned int mask = OMP_PARALLEL_CLAUSE_MASK;
  unsigned int save;
  location_t loc = cp_lexer_peek_token (parser->lexer)->location;

  if (cp_lexer_next_token_is_keyword (parser->lexer, RID_FOR))
    {
      cp_lexer_consume_token (parser->lexer);
      p_kind = PRAGMA_OMP_PARALLEL_FOR;
      p_name = "#pragma omp parallel for";
      mask |= OMP_FOR_CLAUSE_MASK;
      mask &= ~(1u << PRAGMA_OMP_CLAUSE_NOWAIT);
    }
  else if (cp_lexer_next_token_is (parser->lexer, CPP_NAME))
    {
      tree id = cp_lexer_peek_token (parser->lexer)->u.value;
      const char *p = IDENTIFIER_POINTER (id);
      if (strcmp (p, "sections") == 0)
	{
	  cp_lexer_consume_token (parser->lexer);
	  p_kind = PRAGMA_OMP_PARALLEL_SECTIONS;
	  p_name = "#pragma omp parallel sections";
	  mask |= OMP_SECTIONS_CLAUSE_MASK;
	  mask &= ~(1u << PRAGMA_OMP_CLAUSE_NOWAIT);
	}
    }

  clauses = cp_parser_omp_all_clauses (parser, mask, p_name, pragma_tok);
  block = begin_omp_parallel ();
  save = cp_parser_begin_omp_structured_block (parser);

  switch (p_kind)
    {
    case PRAGMA_OMP_PARALLEL:
      cp_parser_statement (parser, NULL_TREE, false, NULL);
      par_clause = clauses;
      break;

    case PRAGMA_OMP_PARALLEL_FOR:
      c_split_parallel_clauses (loc, clauses, &par_clause, &ws_clause);
      cp_parser_omp_for_loop (parser, ws_clause, &par_clause);
      break;

    case PRAGMA_OMP_PARALLEL_SECTIONS:
      c_split_parallel_clauses (loc, clauses, &par_clause, &ws_clause);
      stmt = cp_parser_omp_sections_scope (parser);
      if (stmt)
	OMP_SECTIONS_CLAUSES (stmt) = ws_clause;
      break;

    default:
      gcc_unreachable ();
    }

  cp_parser_end_omp_structured_block (parser, save);
  stmt = finish_omp_parallel (par_clause, block);
  if (p_kind != PRAGMA_OMP_PARALLEL)
    OMP_PARALLEL_COMBINED (stmt) = 1;
  return stmt;
}

/* OpenMP 2.5:
   # pragma omp single single-clause[optseq] new-line
     structured-block  */

#define OMP_SINGLE_CLAUSE_MASK				\
	( (1u << PRAGMA_OMP_CLAUSE_PRIVATE)		\
	| (1u << PRAGMA_OMP_CLAUSE_FIRSTPRIVATE)	\
	| (1u << PRAGMA_OMP_CLAUSE_COPYPRIVATE)		\
	| (1u << PRAGMA_OMP_CLAUSE_NOWAIT))

static tree
cp_parser_omp_single (cp_parser *parser, cp_token *pragma_tok)
{
  tree stmt = make_node (OMP_SINGLE);
  TREE_TYPE (stmt) = void_type_node;

  OMP_SINGLE_CLAUSES (stmt)
    = cp_parser_omp_all_clauses (parser, OMP_SINGLE_CLAUSE_MASK,
				 "#pragma omp single", pragma_tok);
  OMP_SINGLE_BODY (stmt) = cp_parser_omp_structured_block (parser);

  return add_stmt (stmt);
}

/* OpenMP 3.0:
   # pragma omp task task-clause[optseq] new-line
     structured-block  */

#define OMP_TASK_CLAUSE_MASK				\
	( (1u << PRAGMA_OMP_CLAUSE_IF)			\
	| (1u << PRAGMA_OMP_CLAUSE_UNTIED)		\
	| (1u << PRAGMA_OMP_CLAUSE_DEFAULT)		\
	| (1u << PRAGMA_OMP_CLAUSE_PRIVATE)		\
	| (1u << PRAGMA_OMP_CLAUSE_FIRSTPRIVATE)	\
	| (1u << PRAGMA_OMP_CLAUSE_SHARED))

static tree
cp_parser_omp_task (cp_parser *parser, cp_token *pragma_tok)
{
  tree clauses, block;
  unsigned int save;

  clauses = cp_parser_omp_all_clauses (parser, OMP_TASK_CLAUSE_MASK,
				       "#pragma omp task", pragma_tok);
  block = begin_omp_task ();
  save = cp_parser_begin_omp_structured_block (parser);
  cp_parser_statement (parser, NULL_TREE, false, NULL);
  cp_parser_end_omp_structured_block (parser, save);
  return finish_omp_task (clauses, block);
}

/* OpenMP 3.0:
   # pragma omp taskwait new-line  */

static void
cp_parser_omp_taskwait (cp_parser *parser, cp_token *pragma_tok)
{
  cp_parser_require_pragma_eol (parser, pragma_tok);
  finish_omp_taskwait ();
}

/* OpenMP 2.5:
   # pragma omp threadprivate (variable-list) */

static void
cp_parser_omp_threadprivate (cp_parser *parser, cp_token *pragma_tok)
{
  tree vars;

  vars = cp_parser_omp_var_list (parser, OMP_CLAUSE_ERROR, NULL);
  cp_parser_require_pragma_eol (parser, pragma_tok);

  finish_omp_threadprivate (vars);
}

/* Main entry point to OpenMP statement pragmas.  */

static void
cp_parser_omp_construct (cp_parser *parser, cp_token *pragma_tok)
{
  tree stmt;

  switch (pragma_tok->pragma_kind)
    {
    case PRAGMA_OMP_ATOMIC:
      cp_parser_omp_atomic (parser, pragma_tok);
      return;
    case PRAGMA_OMP_CRITICAL:
      stmt = cp_parser_omp_critical (parser, pragma_tok);
      break;
    case PRAGMA_OMP_FOR:
      stmt = cp_parser_omp_for (parser, pragma_tok);
      break;
    case PRAGMA_OMP_MASTER:
      stmt = cp_parser_omp_master (parser, pragma_tok);
      break;
    case PRAGMA_OMP_ORDERED:
      stmt = cp_parser_omp_ordered (parser, pragma_tok);
      break;
    case PRAGMA_OMP_PARALLEL:
      stmt = cp_parser_omp_parallel (parser, pragma_tok);
      break;
    case PRAGMA_OMP_SECTIONS:
      stmt = cp_parser_omp_sections (parser, pragma_tok);
      break;
    case PRAGMA_OMP_SINGLE:
      stmt = cp_parser_omp_single (parser, pragma_tok);
      break;
    case PRAGMA_OMP_TASK:
      stmt = cp_parser_omp_task (parser, pragma_tok);
      break;
    default:
      gcc_unreachable ();
    }

  if (stmt)
    SET_EXPR_LOCATION (stmt, pragma_tok->location);
}

/* The parser.  */

static GTY (()) cp_parser *the_parser;


/* Special handling for the first token or line in the file.  The first
   thing in the file might be #pragma GCC pch_preprocess, which loads a
   PCH file, which is a GC collection point.  So we need to handle this
   first pragma without benefit of an existing lexer structure.

   Always returns one token to the caller in *FIRST_TOKEN.  This is
   either the true first token of the file, or the first token after
   the initial pragma.  */

static void
cp_parser_initial_pragma (cp_token *first_token)
{
  tree name = NULL;

  cp_lexer_get_preprocessor_token (NULL, first_token);
  if (first_token->pragma_kind != PRAGMA_GCC_PCH_PREPROCESS)
    return;

  cp_lexer_get_preprocessor_token (NULL, first_token);
  if (first_token->type == CPP_STRING)
    {
      name = first_token->u.value;

      cp_lexer_get_preprocessor_token (NULL, first_token);
      if (first_token->type != CPP_PRAGMA_EOL)
	error_at (first_token->location,
		  "junk at end of %<#pragma GCC pch_preprocess%>");
    }
  else
    error_at (first_token->location, "expected string literal");

  /* Skip to the end of the pragma.  */
  while (first_token->type != CPP_PRAGMA_EOL && first_token->type != CPP_EOF)
    cp_lexer_get_preprocessor_token (NULL, first_token);

  /* Now actually load the PCH file.  */
  if (name)
    c_common_pch_pragma (parse_in, TREE_STRING_POINTER (name));

  /* Read one more token to return to our caller.  We have to do this
     after reading the PCH file in, since its pointers have to be
     live.  */
  cp_lexer_get_preprocessor_token (NULL, first_token);
}

/* Normal parsing of a pragma token.  Here we can (and must) use the
   regular lexer.  */

static bool
cp_parser_pragma (cp_parser *parser, enum pragma_context context)
{
  cp_token *pragma_tok;
  unsigned int id;

  pragma_tok = cp_lexer_consume_token (parser->lexer);
  gcc_assert (pragma_tok->type == CPP_PRAGMA);
  parser->lexer->in_pragma = true;

  id = pragma_tok->pragma_kind;
  switch (id)
    {
    case PRAGMA_GCC_PCH_PREPROCESS:
      error_at (pragma_tok->location,
		"%<#pragma GCC pch_preprocess%> must be first");
      break;

    case PRAGMA_OMP_BARRIER:
      switch (context)
	{
	case pragma_compound:
	  cp_parser_omp_barrier (parser, pragma_tok);
	  return false;
	case pragma_stmt:
	  error_at (pragma_tok->location, "%<#pragma omp barrier%> may only be "
		    "used in compound statements");
	  break;
	default:
	  goto bad_stmt;
	}
      break;

    case PRAGMA_OMP_FLUSH:
      switch (context)
	{
	case pragma_compound:
	  cp_parser_omp_flush (parser, pragma_tok);
	  return false;
	case pragma_stmt:
	  error_at (pragma_tok->location, "%<#pragma omp flush%> may only be "
		    "used in compound statements");
	  break;
	default:
	  goto bad_stmt;
	}
      break;

    case PRAGMA_OMP_TASKWAIT:
      switch (context)
	{
	case pragma_compound:
	  cp_parser_omp_taskwait (parser, pragma_tok);
	  return false;
	case pragma_stmt:
	  error_at (pragma_tok->location,
		    "%<#pragma omp taskwait%> may only be "
		    "used in compound statements");
	  break;
	default:
	  goto bad_stmt;
	}
      break;

    case PRAGMA_OMP_THREADPRIVATE:
      cp_parser_omp_threadprivate (parser, pragma_tok);
      return false;

    case PRAGMA_OMP_ATOMIC:
    case PRAGMA_OMP_CRITICAL:
    case PRAGMA_OMP_FOR:
    case PRAGMA_OMP_MASTER:
    case PRAGMA_OMP_ORDERED:
    case PRAGMA_OMP_PARALLEL:
    case PRAGMA_OMP_SECTIONS:
    case PRAGMA_OMP_SINGLE:
    case PRAGMA_OMP_TASK:
      if (context == pragma_external)
	goto bad_stmt;
      cp_parser_omp_construct (parser, pragma_tok);
      return true;

    case PRAGMA_OMP_SECTION:
      error_at (pragma_tok->location, 
		"%<#pragma omp section%> may only be used in "
		"%<#pragma omp sections%> construct");
      break;

    default:
      gcc_assert (id >= PRAGMA_FIRST_EXTERNAL);
      c_invoke_pragma_handler (id);
      break;

    bad_stmt:
      cp_parser_error (parser, "expected declaration specifiers");
      break;
    }

  cp_parser_skip_to_pragma_eol (parser, pragma_tok);
  return false;
}

/* The interface the pragma parsers have to the lexer.  */

enum cpp_ttype
pragma_lex (tree *value)
{
  cp_token *tok;
  enum cpp_ttype ret;

  tok = cp_lexer_peek_token (the_parser->lexer);

  ret = tok->type;
  *value = tok->u.value;

  if (ret == CPP_PRAGMA_EOL || ret == CPP_EOF)
    ret = CPP_EOF;
  else if (ret == CPP_STRING)
    *value = cp_parser_string_literal (the_parser, false, false);
  else
    {
      cp_lexer_consume_token (the_parser->lexer);
      if (ret == CPP_KEYWORD)
	ret = CPP_NAME;
    }

  return ret;
}


/* External interface.  */

/* Parse one entire translation unit.  */

void
c_parse_file (void)
{
  static bool already_called = false;

  if (already_called)
    {
      sorry ("inter-module optimizations not implemented for C++");
      return;
    }
  already_called = true;

  the_parser = cp_parser_new ();
  push_deferring_access_checks (flag_access_control
				? dk_no_deferred : dk_no_check);
  cp_parser_translation_unit (the_parser);
  the_parser = NULL;
}

#include "gt-cp-parser.h"
