/* C++ Parser.
   Copyright (C) 2000-2014 Free Software Foundation, Inc.
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
#include "timevar.h"
#include "cpplib.h"
#include "tree.h"
#include "print-tree.h"
#include "stringpool.h"
#include "attribs.h"
#include "trans-mem.h"
#include "cp-tree.h"
#include "intl.h"
#include "c-family/c-pragma.h"
#include "decl.h"
#include "flags.h"
#include "diagnostic-core.h"
#include "target.h"
#include "cgraph.h"
#include "c-family/c-common.h"
#include "c-family/c-objc.h"
#include "plugin.h"
#include "tree-pretty-print.h"
#include "parser.h"
#include "type-utils.h"
#include "omp-low.h"


/* The lexer.  */

/* The cp_lexer_* routines mediate between the lexer proper (in libcpp
   and c-lex.c) and the C++ parser.  */

static cp_token eof_token =
{
  CPP_EOF, RID_MAX, 0, PRAGMA_NONE, false, false, false, 0, { NULL }
};

/* The various kinds of non integral constant we encounter. */
typedef enum non_integral_constant {
  NIC_NONE,
  /* floating-point literal */
  NIC_FLOAT,
  /* %<this%> */
  NIC_THIS,
  /* %<__FUNCTION__%> */
  NIC_FUNC_NAME,
  /* %<__PRETTY_FUNCTION__%> */
  NIC_PRETTY_FUNC,
  /* %<__func__%> */
  NIC_C99_FUNC,
  /* "%<va_arg%> */
  NIC_VA_ARG,
  /* a cast */
  NIC_CAST,
  /* %<typeid%> operator */
  NIC_TYPEID,
  /* non-constant compound literals */
  NIC_NCC,
  /* a function call */
  NIC_FUNC_CALL,
  /* an increment */
  NIC_INC,
  /* an decrement */
  NIC_DEC,
  /* an array reference */
  NIC_ARRAY_REF,
  /* %<->%> */
  NIC_ARROW,
  /* %<.%> */
  NIC_POINT,
  /* the address of a label */
  NIC_ADDR_LABEL,
  /* %<*%> */
  NIC_STAR,
  /* %<&%> */
  NIC_ADDR,
  /* %<++%> */
  NIC_PREINCREMENT,
  /* %<--%> */
  NIC_PREDECREMENT,
  /* %<new%> */
  NIC_NEW,
  /* %<delete%> */
  NIC_DEL,
  /* calls to overloaded operators */
  NIC_OVERLOADED,
  /* an assignment */
  NIC_ASSIGNMENT,
  /* a comma operator */
  NIC_COMMA,
  /* a call to a constructor */
  NIC_CONSTRUCTOR,
  /* a transaction expression */
  NIC_TRANSACTION
} non_integral_constant;

/* The various kinds of errors about name-lookup failing. */
typedef enum name_lookup_error {
  /* NULL */
  NLE_NULL,
  /* is not a type */
  NLE_TYPE,
  /* is not a class or namespace */
  NLE_CXX98,
  /* is not a class, namespace, or enumeration */
  NLE_NOT_CXX98
} name_lookup_error;

/* The various kinds of required token */
typedef enum required_token {
  RT_NONE,
  RT_SEMICOLON,  /* ';' */
  RT_OPEN_PAREN, /* '(' */
  RT_CLOSE_BRACE, /* '}' */
  RT_OPEN_BRACE,  /* '{' */
  RT_CLOSE_SQUARE, /* ']' */
  RT_OPEN_SQUARE,  /* '[' */
  RT_COMMA, /* ',' */
  RT_SCOPE, /* '::' */
  RT_LESS, /* '<' */
  RT_GREATER, /* '>' */
  RT_EQ, /* '=' */
  RT_ELLIPSIS, /* '...' */
  RT_MULT, /* '*' */
  RT_COMPL, /* '~' */
  RT_COLON, /* ':' */
  RT_COLON_SCOPE, /* ':' or '::' */
  RT_CLOSE_PAREN, /* ')' */
  RT_COMMA_CLOSE_PAREN, /* ',' or ')' */
  RT_PRAGMA_EOL, /* end of line */
  RT_NAME, /* identifier */

  /* The type is CPP_KEYWORD */
  RT_NEW, /* new */
  RT_DELETE, /* delete */
  RT_RETURN, /* return */
  RT_WHILE, /* while */
  RT_EXTERN, /* extern */
  RT_STATIC_ASSERT, /* static_assert */
  RT_DECLTYPE, /* decltype */
  RT_OPERATOR, /* operator */
  RT_CLASS, /* class */
  RT_TEMPLATE, /* template */
  RT_NAMESPACE, /* namespace */
  RT_USING, /* using */
  RT_ASM, /* asm */
  RT_TRY, /* try */
  RT_CATCH, /* catch */
  RT_THROW, /* throw */
  RT_LABEL, /* __label__ */
  RT_AT_TRY, /* @try */
  RT_AT_SYNCHRONIZED, /* @synchronized */
  RT_AT_THROW, /* @throw */

  RT_SELECT,  /* selection-statement */
  RT_INTERATION, /* iteration-statement */
  RT_JUMP, /* jump-statement */
  RT_CLASS_KEY, /* class-key */
  RT_CLASS_TYPENAME_TEMPLATE, /* class, typename, or template */
  RT_TRANSACTION_ATOMIC, /* __transaction_atomic */
  RT_TRANSACTION_RELAXED, /* __transaction_relaxed */
  RT_TRANSACTION_CANCEL /* __transaction_cancel */
} required_token;

/* Prototypes.  */

static cp_lexer *cp_lexer_new_main
  (void);
static cp_lexer *cp_lexer_new_from_tokens
  (cp_token_cache *tokens);
static void cp_lexer_destroy
  (cp_lexer *);
static int cp_lexer_saving_tokens
  (const cp_lexer *);
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
static void cp_lexer_print_token
  (FILE *, cp_token *);
static inline bool cp_lexer_debugging_p
  (cp_lexer *);
static void cp_lexer_start_debugging
  (cp_lexer *) ATTRIBUTE_UNUSED;
static void cp_lexer_stop_debugging
  (cp_lexer *) ATTRIBUTE_UNUSED;

static cp_token_cache *cp_token_cache_new
  (cp_token *, cp_token *);

static void cp_parser_initial_pragma
  (cp_token *);

static tree cp_literal_operator_id
  (const char *);

static void cp_parser_cilk_simd
  (cp_parser *, cp_token *);
static bool cp_parser_omp_declare_reduction_exprs
  (tree, cp_parser *);
static tree cp_parser_cilk_simd_vectorlength 
  (cp_parser *, tree, bool);

/* Manifest constants.  */
#define CP_LEXER_BUFFER_SIZE ((256 * 1024) / sizeof (cp_token))
#define CP_SAVED_TOKEN_STACK 5

/* Variables.  */

/* The stream to which debugging output should be written.  */
static FILE *cp_lexer_debug_stream;

/* Nonzero if we are parsing an unevaluated operand: an operand to
   sizeof, typeof, or alignof.  */
int cp_unevaluated_operand;

/* Dump up to NUM tokens in BUFFER to FILE starting with token
   START_TOKEN.  If START_TOKEN is NULL, the dump starts with the
   first token in BUFFER.  If NUM is 0, dump all the tokens.  If
   CURR_TOKEN is set and it is one of the tokens in BUFFER, it will be
   highlighted by surrounding it in [[ ]].  */

static void
cp_lexer_dump_tokens (FILE *file, vec<cp_token, va_gc> *buffer,
		      cp_token *start_token, unsigned num,
		      cp_token *curr_token)
{
  unsigned i, nprinted;
  cp_token *token;
  bool do_print;

  fprintf (file, "%u tokens\n", vec_safe_length (buffer));

  if (buffer == NULL)
    return;

  if (num == 0)
    num = buffer->length ();

  if (start_token == NULL)
    start_token = buffer->address ();

  if (start_token > buffer->address ())
    {
      cp_lexer_print_token (file, &(*buffer)[0]);
      fprintf (file, " ... ");
    }

  do_print = false;
  nprinted = 0;
  for (i = 0; buffer->iterate (i, &token) && nprinted < num; i++)
    {
      if (token == start_token)
	do_print = true;

      if (!do_print)
	continue;

      nprinted++;
      if (token == curr_token)
	fprintf (file, "[[");

      cp_lexer_print_token (file, token);

      if (token == curr_token)
	fprintf (file, "]]");

      switch (token->type)
	{
	  case CPP_SEMICOLON:
	  case CPP_OPEN_BRACE:
	  case CPP_CLOSE_BRACE:
	  case CPP_EOF:
	    fputc ('\n', file);
	    break;

	  default:
	    fputc (' ', file);
	}
    }

  if (i == num && i < buffer->length ())
    {
      fprintf (file, " ... ");
      cp_lexer_print_token (file, &buffer->last ());
    }

  fprintf (file, "\n");
}


/* Dump all tokens in BUFFER to stderr.  */

void
cp_lexer_debug_tokens (vec<cp_token, va_gc> *buffer)
{
  cp_lexer_dump_tokens (stderr, buffer, NULL, 0, NULL);
}

DEBUG_FUNCTION void
debug (vec<cp_token, va_gc> &ref)
{
  cp_lexer_dump_tokens (stderr, &ref, NULL, 0, NULL);
}

DEBUG_FUNCTION void
debug (vec<cp_token, va_gc> *ptr)
{
  if (ptr)
    debug (*ptr);
  else
    fprintf (stderr, "<nil>\n");
}


/* Dump the cp_parser tree field T to FILE if T is non-NULL.  DESC is the
   description for T.  */

static void
cp_debug_print_tree_if_set (FILE *file, const char *desc, tree t)
{
  if (t)
    {
      fprintf (file, "%s: ", desc);
      print_node_brief (file, "", t, 0);
    }
}


/* Dump parser context C to FILE.  */

static void
cp_debug_print_context (FILE *file, cp_parser_context *c)
{
  const char *status_s[] = { "OK", "ERROR", "COMMITTED" };
  fprintf (file, "{ status = %s, scope = ", status_s[c->status]);
  print_node_brief (file, "", c->object_type, 0);
  fprintf (file, "}\n");
}


/* Print the stack of parsing contexts to FILE starting with FIRST.  */

static void
cp_debug_print_context_stack (FILE *file, cp_parser_context *first)
{
  unsigned i;
  cp_parser_context *c;

  fprintf (file, "Parsing context stack:\n");
  for (i = 0, c = first; c; c = c->next, i++)
    {
      fprintf (file, "\t#%u: ", i);
      cp_debug_print_context (file, c);
    }
}


/* Print the value of FLAG to FILE.  DESC is a string describing the flag.  */

static void
cp_debug_print_flag (FILE *file, const char *desc, bool flag)
{
  if (flag)
    fprintf (file, "%s: true\n", desc);
}


/* Print an unparsed function entry UF to FILE.  */

static void
cp_debug_print_unparsed_function (FILE *file, cp_unparsed_functions_entry *uf)
{
  unsigned i;
  cp_default_arg_entry *default_arg_fn;
  tree fn;

  fprintf (file, "\tFunctions with default args:\n");
  for (i = 0;
       vec_safe_iterate (uf->funs_with_default_args, i, &default_arg_fn);
       i++)
    {
      fprintf (file, "\t\tClass type: ");
      print_node_brief (file, "", default_arg_fn->class_type, 0);
      fprintf (file, "\t\tDeclaration: ");
      print_node_brief (file, "", default_arg_fn->decl, 0);
      fprintf (file, "\n");
    }

  fprintf (file, "\n\tFunctions with definitions that require "
	   "post-processing\n\t\t");
  for (i = 0; vec_safe_iterate (uf->funs_with_definitions, i, &fn); i++)
    {
      print_node_brief (file, "", fn, 0);
      fprintf (file, " ");
    }
  fprintf (file, "\n");

  fprintf (file, "\n\tNon-static data members with initializers that require "
           "post-processing\n\t\t");
  for (i = 0; vec_safe_iterate (uf->nsdmis, i, &fn); i++)
    {
      print_node_brief (file, "", fn, 0);
      fprintf (file, " ");
    }
  fprintf (file, "\n");
}


/* Print the stack of unparsed member functions S to FILE.  */

static void
cp_debug_print_unparsed_queues (FILE *file,
				vec<cp_unparsed_functions_entry, va_gc> *s)
{
  unsigned i;
  cp_unparsed_functions_entry *uf;

  fprintf (file, "Unparsed functions\n");
  for (i = 0; vec_safe_iterate (s, i, &uf); i++)
    {
      fprintf (file, "#%u:\n", i);
      cp_debug_print_unparsed_function (file, uf);
    }
}


/* Dump the tokens in a window of size WINDOW_SIZE around the next_token for
   the given PARSER.  If FILE is NULL, the output is printed on stderr. */

static void
cp_debug_parser_tokens (FILE *file, cp_parser *parser, int window_size)
{
  cp_token *next_token, *first_token, *start_token;

  if (file == NULL)
    file = stderr;

  next_token = parser->lexer->next_token;
  first_token = parser->lexer->buffer->address ();
  start_token = (next_token > first_token + window_size / 2)
		? next_token - window_size / 2
		: first_token;
  cp_lexer_dump_tokens (file, parser->lexer->buffer, start_token, window_size,
			next_token);
}


/* Dump debugging information for the given PARSER.  If FILE is NULL,
   the output is printed on stderr.  */

void
cp_debug_parser (FILE *file, cp_parser *parser)
{
  const size_t window_size = 20;
  cp_token *token;
  expanded_location eloc;

  if (file == NULL)
    file = stderr;

  fprintf (file, "Parser state\n\n");
  fprintf (file, "Number of tokens: %u\n",
	   vec_safe_length (parser->lexer->buffer));
  cp_debug_print_tree_if_set (file, "Lookup scope", parser->scope);
  cp_debug_print_tree_if_set (file, "Object scope",
				     parser->object_scope);
  cp_debug_print_tree_if_set (file, "Qualifying scope",
				     parser->qualifying_scope);
  cp_debug_print_context_stack (file, parser->context);
  cp_debug_print_flag (file, "Allow GNU extensions",
			      parser->allow_gnu_extensions_p);
  cp_debug_print_flag (file, "'>' token is greater-than",
			      parser->greater_than_is_operator_p);
  cp_debug_print_flag (file, "Default args allowed in current "
			      "parameter list", parser->default_arg_ok_p);
  cp_debug_print_flag (file, "Parsing integral constant-expression",
			      parser->integral_constant_expression_p);
  cp_debug_print_flag (file, "Allow non-constant expression in current "
			      "constant-expression",
			      parser->allow_non_integral_constant_expression_p);
  cp_debug_print_flag (file, "Seen non-constant expression",
			      parser->non_integral_constant_expression_p);
  cp_debug_print_flag (file, "Local names and 'this' forbidden in "
			      "current context",
			      parser->local_variables_forbidden_p);
  cp_debug_print_flag (file, "In unbraced linkage specification",
			      parser->in_unbraced_linkage_specification_p);
  cp_debug_print_flag (file, "Parsing a declarator",
			      parser->in_declarator_p);
  cp_debug_print_flag (file, "In template argument list",
			      parser->in_template_argument_list_p);
  cp_debug_print_flag (file, "Parsing an iteration statement",
			      parser->in_statement & IN_ITERATION_STMT);
  cp_debug_print_flag (file, "Parsing a switch statement",
			      parser->in_statement & IN_SWITCH_STMT);
  cp_debug_print_flag (file, "Parsing a structured OpenMP block",
			      parser->in_statement & IN_OMP_BLOCK);
  cp_debug_print_flag (file, "Parsing a Cilk Plus for loop",
			      parser->in_statement & IN_CILK_SIMD_FOR);
  cp_debug_print_flag (file, "Parsing a an OpenMP loop",
			      parser->in_statement & IN_OMP_FOR);
  cp_debug_print_flag (file, "Parsing an if statement",
			      parser->in_statement & IN_IF_STMT);
  cp_debug_print_flag (file, "Parsing a type-id in an expression "
			      "context", parser->in_type_id_in_expr_p);
  cp_debug_print_flag (file, "Declarations are implicitly extern \"C\"",
			      parser->implicit_extern_c);
  cp_debug_print_flag (file, "String expressions should be translated "
			      "to execution character set",
			      parser->translate_strings_p);
  cp_debug_print_flag (file, "Parsing function body outside of a "
			      "local class", parser->in_function_body);
  cp_debug_print_flag (file, "Auto correct a colon to a scope operator",
			      parser->colon_corrects_to_scope_p);
  cp_debug_print_flag (file, "Colon doesn't start a class definition",
			      parser->colon_doesnt_start_class_def_p);
  if (parser->type_definition_forbidden_message)
    fprintf (file, "Error message for forbidden type definitions: %s\n",
	     parser->type_definition_forbidden_message);
  cp_debug_print_unparsed_queues (file, parser->unparsed_queues);
  fprintf (file, "Number of class definitions in progress: %u\n",
	   parser->num_classes_being_defined);
  fprintf (file, "Number of template parameter lists for the current "
	   "declaration: %u\n", parser->num_template_parameter_lists);
  cp_debug_parser_tokens (file, parser, window_size);
  token = parser->lexer->next_token;
  fprintf (file, "Next token to parse:\n");
  fprintf (file, "\tToken:  ");
  cp_lexer_print_token (file, token);
  eloc = expand_location (token->location);
  fprintf (file, "\n\tFile:   %s\n", eloc.file);
  fprintf (file, "\tLine:   %d\n", eloc.line);
  fprintf (file, "\tColumn: %d\n", eloc.column);
}

DEBUG_FUNCTION void
debug (cp_parser &ref)
{
  cp_debug_parser (stderr, &ref);
}

DEBUG_FUNCTION void
debug (cp_parser *ptr)
{
  if (ptr)
    debug (*ptr);
  else
    fprintf (stderr, "<nil>\n");
}

/* Allocate memory for a new lexer object and return it.  */

static cp_lexer *
cp_lexer_alloc (void)
{
  cp_lexer *lexer;

  c_common_no_more_pch ();

  /* Allocate the memory.  */
  lexer = ggc_alloc_cleared_cp_lexer ();

  /* Initially we are not debugging.  */
  lexer->debugging_p = false;

  lexer->saved_tokens.create (CP_SAVED_TOKEN_STACK);

  /* Create the buffer.  */
  vec_alloc (lexer->buffer, CP_LEXER_BUFFER_SIZE);

  return lexer;
}


/* Create a new main C++ lexer, the lexer that gets tokens from the
   preprocessor.  */

static cp_lexer *
cp_lexer_new_main (void)
{
  cp_lexer *lexer;
  cp_token token;

  /* It's possible that parsing the first pragma will load a PCH file,
     which is a GC collection point.  So we have to do that before
     allocating any memory.  */
  cp_parser_initial_pragma (&token);

  lexer = cp_lexer_alloc ();

  /* Put the first token in the buffer.  */
  lexer->buffer->quick_push (token);

  /* Get the remaining tokens from the preprocessor.  */
  while (token.type != CPP_EOF)
    {
      cp_lexer_get_preprocessor_token (lexer, &token);
      vec_safe_push (lexer->buffer, token);
    }

  lexer->last_token = lexer->buffer->address ()
                      + lexer->buffer->length ()
		      - 1;
  lexer->next_token = lexer->buffer->length ()
		      ? lexer->buffer->address ()
		      : &eof_token;

  /* Subsequent preprocessor diagnostics should use compiler
     diagnostic functions to get the compiler source location.  */
  done_lexing = true;

  gcc_assert (!lexer->next_token->purged_p);
  return lexer;
}

/* Create a new lexer whose token stream is primed with the tokens in
   CACHE.  When these tokens are exhausted, no new tokens will be read.  */

static cp_lexer *
cp_lexer_new_from_tokens (cp_token_cache *cache)
{
  cp_token *first = cache->first;
  cp_token *last = cache->last;
  cp_lexer *lexer = ggc_alloc_cleared_cp_lexer ();

  /* We do not own the buffer.  */
  lexer->buffer = NULL;
  lexer->next_token = first == last ? &eof_token : first;
  lexer->last_token = last;

  lexer->saved_tokens.create (CP_SAVED_TOKEN_STACK);

  /* Initially we are not debugging.  */
  lexer->debugging_p = false;

  gcc_assert (!lexer->next_token->purged_p);
  return lexer;
}

/* Frees all resources associated with LEXER.  */

static void
cp_lexer_destroy (cp_lexer *lexer)
{
  vec_free (lexer->buffer);
  lexer->saved_tokens.release ();
  ggc_free (lexer);
}

/* Returns nonzero if debugging information should be output.  */

static inline bool
cp_lexer_debugging_p (cp_lexer *lexer)
{
  return lexer->debugging_p;
}


static inline cp_token_position
cp_lexer_token_position (cp_lexer *lexer, bool previous_p)
{
  gcc_assert (!previous_p || lexer->next_token != &eof_token);

  return lexer->next_token - previous_p;
}

static inline cp_token *
cp_lexer_token_at (cp_lexer * /*lexer*/, cp_token_position pos)
{
  return pos;
}

static inline void
cp_lexer_set_token_position (cp_lexer *lexer, cp_token_position pos)
{
  lexer->next_token = cp_lexer_token_at (lexer, pos);
}

static inline cp_token_position
cp_lexer_previous_token_position (cp_lexer *lexer)
{
  if (lexer->next_token == &eof_token)
    return lexer->last_token - 1;
  else
    return cp_lexer_token_position (lexer, true);
}

static inline cp_token *
cp_lexer_previous_token (cp_lexer *lexer)
{
  cp_token_position tp = cp_lexer_previous_token_position (lexer);

  return cp_lexer_token_at (lexer, tp);
}

/* nonzero if we are presently saving tokens.  */

static inline int
cp_lexer_saving_tokens (const cp_lexer* lexer)
{
  return lexer->saved_tokens.length () != 0;
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
  token->purged_p = false;

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
                       "identifier %qE is a keyword in C++11",
                       token->u.value);

              /* Clear out the C_RID_CODE so we don't warn about this
                 particular identifier-turned-keyword again.  */
              C_SET_RID_CODE (token->u.value, RID_MAX);
            }

	  token->ambiguous_p = false;
	  token->keyword = RID_MAX;
	}
    }
  else if (token->type == CPP_AT_NAME)
    {
      /* This only happens in Objective-C++; it must be a keyword.  */
      token->type = CPP_KEYWORD;
      switch (C_RID_CODE (token->u.value))
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
	default:            token->keyword = C_RID_CODE (token->u.value);
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

static inline bool
cp_lexer_nth_token_is_keyword (cp_lexer* lexer, size_t n, enum rid keyword)
{
  return cp_lexer_peek_nth_token (lexer, n)->keyword == keyword;
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
    case RID_INT128:
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
    case RID_UNDERLYING_TYPE:
      return true;

    default:
      return false;
    }
}

/* Returns TRUE iff the token T begins a decltype type.  */

static bool
token_is_decltype (cp_token *t)
{
  return (t->keyword == RID_DECLTYPE
	  || t->type == CPP_DECLTYPE);
}

/* Returns TRUE iff the next token begins a decltype type.  */

static bool
cp_lexer_next_token_is_decltype (cp_lexer *lexer)
{
  cp_token *t = cp_lexer_peek_token (lexer);
  return token_is_decltype (t);
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

      if (!token->purged_p)
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
  while (lexer->next_token->purged_p);

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
  tok->purged_p = true;
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
  while (tok->purged_p);
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
      tok->purged_p = true;
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

  lexer->saved_tokens.safe_push (lexer->next_token);
}

/* Commit to the portion of the token stream most recently saved.  */

static void
cp_lexer_commit_tokens (cp_lexer* lexer)
{
  /* Provide debugging output.  */
  if (cp_lexer_debugging_p (lexer))
    fprintf (cp_lexer_debug_stream, "cp_lexer: committing tokens\n");

  lexer->saved_tokens.pop ();
}

/* Return all tokens saved since the last call to cp_lexer_save_tokens
   to the token stream.  Stop saving tokens.  */

static void
cp_lexer_rollback_tokens (cp_lexer* lexer)
{
  /* Provide debugging output.  */
  if (cp_lexer_debugging_p (lexer))
    fprintf (cp_lexer_debug_stream, "cp_lexer: restoring tokens\n");

  lexer->next_token = lexer->saved_tokens.pop ();
}

/* Print a representation of the TOKEN on the STREAM.  */

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
  };

  /* For some tokens, print the associated data.  */
  switch (token->type)
    {
    case CPP_KEYWORD:
      /* Some keywords have a value that is not an IDENTIFIER_NODE.
	 For example, `struct' is mapped to an INTEGER_CST.  */
      if (!identifier_p (token->u.value))
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

    case CPP_NUMBER:
      print_generic_expr (stream, token->u.value, 0);
      break;

    default:
      /* If we have a name for the token, print it out.  Otherwise, we
	 simply give the numeric code.  */
      if (token->type < ARRAY_SIZE(token_names))
	fputs (token_names[token->type], stream);
      else
	fprintf (stream, "[%d]", token->type);
      break;
    }
}

DEBUG_FUNCTION void
debug (cp_token &ref)
{
  cp_lexer_print_token (stderr, &ref);
  fprintf (stderr, "\n");
}

DEBUG_FUNCTION void
debug (cp_token *ptr)
{
  if (ptr)
    debug (*ptr);
  else
    fprintf (stderr, "<nil>\n");
}


/* Start emitting debugging information.  */

static void
cp_lexer_start_debugging (cp_lexer* lexer)
{
  lexer->debugging_p = true;
  cp_lexer_debug_stream = stderr;
}

/* Stop emitting debugging information.  */

static void
cp_lexer_stop_debugging (cp_lexer* lexer)
{
  lexer->debugging_p = false;
  cp_lexer_debug_stream = NULL;
}

/* Create a new cp_token_cache, representing a range of tokens.  */

static cp_token_cache *
cp_token_cache_new (cp_token *first, cp_token *last)
{
  cp_token_cache *cache = ggc_alloc_cp_token_cache ();
  cache->first = first;
  cache->last = last;
  return cache;
}

/* Diagnose if #pragma omp declare simd isn't followed immediately
   by function declaration or definition.  */

static inline void
cp_ensure_no_omp_declare_simd (cp_parser *parser)
{
  if (parser->omp_declare_simd && !parser->omp_declare_simd->error_seen)
    {
      error ("%<#pragma omp declare simd%> not immediately followed by "
	     "function declaration or definition");
      parser->omp_declare_simd = NULL;
    }
}

/* Finalize #pragma omp declare simd clauses after FNDECL has been parsed,
   and put that into "omp declare simd" attribute.  */

static inline void
cp_finalize_omp_declare_simd (cp_parser *parser, tree fndecl)
{
  if (__builtin_expect (parser->omp_declare_simd != NULL, 0))
    {
      if (fndecl == error_mark_node)
	{
	  parser->omp_declare_simd = NULL;
	  return;
	}
      if (TREE_CODE (fndecl) != FUNCTION_DECL)
	{
	  cp_ensure_no_omp_declare_simd (parser);
	  return;
	}
    }
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
  (cp_declarator *, tree, cp_cv_quals, cp_virt_specifiers, cp_ref_qualifier, tree, tree);
static cp_declarator *make_array_declarator
  (cp_declarator *, tree);
static cp_declarator *make_pointer_declarator
  (cp_cv_quals, cp_declarator *, tree);
static cp_declarator *make_reference_declarator
  (cp_cv_quals, cp_declarator *, bool, tree);
static cp_parameter_declarator *make_parameter_declarator
  (cp_decl_specifier_seq *, cp_declarator *, tree);
static cp_declarator *make_ptrmem_declarator
  (cp_cv_quals, tree, cp_declarator *, tree);

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
  declarator->std_attributes = NULL_TREE;
  declarator->declarator = NULL;
  declarator->parameter_pack_p = false;
  declarator->id_loc = UNKNOWN_LOCATION;

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

  gcc_assert (identifier_p (unqualified_name)
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
   type, represented as identifiers.  ATTRIBUTES represent the attributes that
   appertain to the pointer or reference.  */

cp_declarator *
make_pointer_declarator (cp_cv_quals cv_qualifiers, cp_declarator *target,
			 tree attributes)
{
  cp_declarator *declarator;

  declarator = make_declarator (cdk_pointer);
  declarator->declarator = target;
  declarator->u.pointer.qualifiers = cv_qualifiers;
  declarator->u.pointer.class_type = NULL_TREE;
  if (target)
    {
      declarator->id_loc = target->id_loc;
      declarator->parameter_pack_p = target->parameter_pack_p;
      target->parameter_pack_p = false;
    }
  else
    declarator->parameter_pack_p = false;

  declarator->std_attributes = attributes;

  return declarator;
}

/* Like make_pointer_declarator -- but for references.  ATTRIBUTES
   represent the attributes that appertain to the pointer or
   reference.  */

cp_declarator *
make_reference_declarator (cp_cv_quals cv_qualifiers, cp_declarator *target,
			   bool rvalue_ref, tree attributes)
{
  cp_declarator *declarator;

  declarator = make_declarator (cdk_reference);
  declarator->declarator = target;
  declarator->u.reference.qualifiers = cv_qualifiers;
  declarator->u.reference.rvalue_ref = rvalue_ref;
  if (target)
    {
      declarator->id_loc = target->id_loc;
      declarator->parameter_pack_p = target->parameter_pack_p;
      target->parameter_pack_p = false;
    }
  else
    declarator->parameter_pack_p = false;

  declarator->std_attributes = attributes;

  return declarator;
}

/* Like make_pointer_declarator -- but for a pointer to a non-static
   member of CLASS_TYPE.  ATTRIBUTES represent the attributes that
   appertain to the pointer or reference.  */

cp_declarator *
make_ptrmem_declarator (cp_cv_quals cv_qualifiers, tree class_type,
			cp_declarator *pointee,
			tree attributes)
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

  declarator->std_attributes = attributes;

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
		      cp_virt_specifiers virt_specifiers,
		      cp_ref_qualifier ref_qualifier,
		      tree exception_specification,
		      tree late_return_type)
{
  cp_declarator *declarator;

  declarator = make_declarator (cdk_function);
  declarator->declarator = target;
  declarator->u.function.parameters = parms;
  declarator->u.function.qualifiers = cv_qualifiers;
  declarator->u.function.virt_specifiers = virt_specifiers;
  declarator->u.function.ref_qualifier = ref_qualifier;
  declarator->u.function.exception_specification = exception_specification;
  declarator->u.function.late_return_type = late_return_type;
  if (target)
    {
      declarator->id_loc = target->id_loc;
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
      declarator->id_loc = element->id_loc;
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
  CP_PARSER_FLAGS_NO_TYPE_DEFINITIONS = 0x4,
  /* When parsing a decl-specifier-seq, only allow type-specifier or
     constexpr.  */
  CP_PARSER_FLAGS_ONLY_TYPE_OR_CONSTEXPR = 0x8
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
  /* Location of the binary operation we are parsing.  */
  location_t loc;
} cp_parser_expression_stack_entry;

/* The stack for storing partial expressions.  We only need NUM_PREC_VALUES
   entries because precedence levels on the stack are monotonically
   increasing.  */
typedef struct cp_parser_expression_stack_entry
  cp_parser_expression_stack[NUM_PREC_VALUES];

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
    context = ggc_alloc_cleared_cp_parser_context ();

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

/* Managing the unparsed function queues.  */

#define unparsed_funs_with_default_args \
  parser->unparsed_queues->last ().funs_with_default_args
#define unparsed_funs_with_definitions \
  parser->unparsed_queues->last ().funs_with_definitions
#define unparsed_nsdmis \
  parser->unparsed_queues->last ().nsdmis

static void
push_unparsed_function_queues (cp_parser *parser)
{
  cp_unparsed_functions_entry e = {NULL, make_tree_vector (), NULL};
  vec_safe_push (parser->unparsed_queues, e);
}

static void
pop_unparsed_function_queues (cp_parser *parser)
{
  release_tree_vector (unparsed_funs_with_definitions);
  parser->unparsed_queues->pop ();
}

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
static tree cp_parser_userdef_char_literal
  (cp_parser *);
static tree cp_parser_userdef_string_literal
  (cp_token *);
static tree cp_parser_userdef_numeric_literal
  (cp_parser *);

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
  (cp_parser *, bool, bool, bool, bool, cp_id_kind *);
static tree cp_parser_postfix_open_square_expression
  (cp_parser *, tree, bool, bool);
static tree cp_parser_postfix_dot_deref_expression
  (cp_parser *, enum cpp_ttype, tree, bool, cp_id_kind *, location_t);
static vec<tree, va_gc> *cp_parser_parenthesized_expression_list
  (cp_parser *, int, bool, bool, bool *);
/* Values for the second parameter of cp_parser_parenthesized_expression_list.  */
enum { non_attr = 0, normal_attr = 1, id_attr = 2 };
static void cp_parser_pseudo_destructor_name
  (cp_parser *, tree, tree *, tree *);
static tree cp_parser_unary_expression
  (cp_parser *, bool, bool, cp_id_kind *);
static enum tree_code cp_parser_unary_operator
  (cp_token *);
static tree cp_parser_new_expression
  (cp_parser *);
static vec<tree, va_gc> *cp_parser_new_placement
  (cp_parser *);
static tree cp_parser_new_type_id
  (cp_parser *, tree *);
static cp_declarator *cp_parser_new_declarator_opt
  (cp_parser *);
static cp_declarator *cp_parser_direct_new_declarator
  (cp_parser *);
static vec<tree, va_gc> *cp_parser_new_initializer
  (cp_parser *);
static tree cp_parser_delete_expression
  (cp_parser *);
static tree cp_parser_cast_expression
  (cp_parser *, bool, bool, bool, cp_id_kind *);
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
static tree cp_parser_expression
  (cp_parser *, bool, bool, cp_id_kind *);
static tree cp_parser_constant_expression
  (cp_parser *, bool, bool *);
static tree cp_parser_builtin_offsetof
  (cp_parser *);
static tree cp_parser_lambda_expression
  (cp_parser *);
static void cp_parser_lambda_introducer
  (cp_parser *, tree);
static bool cp_parser_lambda_declarator_opt
  (cp_parser *, tree);
static void cp_parser_lambda_body
  (cp_parser *, tree);

/* Statements [gram.stmt.stmt]  */

static void cp_parser_statement
  (cp_parser *, tree, bool, bool *);
static void cp_parser_label_for_labeled_statement
(cp_parser *, tree);
static tree cp_parser_expression_statement
  (cp_parser *, tree);
static tree cp_parser_compound_statement
  (cp_parser *, tree, bool, bool);
static void cp_parser_statement_seq_opt
  (cp_parser *, tree);
static tree cp_parser_selection_statement
  (cp_parser *, bool *);
static tree cp_parser_condition
  (cp_parser *);
static tree cp_parser_iteration_statement
  (cp_parser *, bool);
static bool cp_parser_for_init_statement
  (cp_parser *, tree *decl);
static tree cp_parser_for
  (cp_parser *, bool);
static tree cp_parser_c_for
  (cp_parser *, tree, tree, bool);
static tree cp_parser_range_for
  (cp_parser *, tree, tree, tree, bool);
static void do_range_for_auto_deduction
  (tree, tree);
static tree cp_parser_perform_range_for_lookup
  (tree, tree *, tree *);
static tree cp_parser_range_for_member_function
  (tree, tree);
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
  (cp_parser *, bool, tree *);
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
static tree cp_parser_alias_declaration
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
  (cp_parser *, cp_decl_specifier_seq *, vec<deferred_access_check, va_gc> *, bool, bool, int, bool *, tree *);
static cp_declarator *cp_parser_declarator
  (cp_parser *, cp_parser_declarator_kind, int *, bool *, bool);
static cp_declarator *cp_parser_direct_declarator
  (cp_parser *, cp_parser_declarator_kind, int *, bool);
static enum tree_code cp_parser_ptr_operator
  (cp_parser *, tree *, cp_cv_quals *, tree *);
static cp_cv_quals cp_parser_cv_qualifier_seq_opt
  (cp_parser *);
static cp_virt_specifiers cp_parser_virt_specifier_seq_opt
  (cp_parser *);
static cp_ref_qualifier cp_parser_ref_qualifier_opt
  (cp_parser *);
static tree cp_parser_late_return_type_opt
  (cp_parser *, cp_declarator *, cp_cv_quals);
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
  (cp_parser *, bool);
static tree cp_parser_initializer
  (cp_parser *, bool *, bool *);
static tree cp_parser_initializer_clause
  (cp_parser *, bool *);
static tree cp_parser_braced_list
  (cp_parser*, bool*);
static vec<constructor_elt, va_gc> *cp_parser_initializer_list
  (cp_parser *, bool *);

static bool cp_parser_ctor_initializer_opt_and_function_body
  (cp_parser *, bool);

static tree cp_parser_late_parsing_omp_declare_simd
  (cp_parser *, tree);

static tree cp_parser_late_parsing_cilk_simd_fn_info
  (cp_parser *, tree);

static tree synthesize_implicit_template_parm
  (cp_parser *);
static tree finish_fully_implicit_template
  (cp_parser *, tree);

/* Classes [gram.class] */

static tree cp_parser_class_name
  (cp_parser *, bool, bool, enum tag_types, bool, bool, bool);
static tree cp_parser_class_specifier
  (cp_parser *);
static tree cp_parser_class_head
  (cp_parser *, bool *);
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
  (cp_parser *, bool, bool, enum tag_types, bool);
static tree cp_parser_template_name
  (cp_parser *, bool, bool, bool, enum tag_types, bool *);
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
static bool cp_next_tokens_can_be_attribute_p
  (cp_parser *);
static bool cp_next_tokens_can_be_gnu_attribute_p
  (cp_parser *);
static bool cp_next_tokens_can_be_std_attribute_p
  (cp_parser *);
static bool cp_nth_tokens_can_be_std_attribute_p
  (cp_parser *, size_t);
static bool cp_nth_tokens_can_be_gnu_attribute_p
  (cp_parser *, size_t);
static bool cp_nth_tokens_can_be_attribute_p
  (cp_parser *, size_t);
static tree cp_parser_attributes_opt
  (cp_parser *);
static tree cp_parser_gnu_attributes_opt
  (cp_parser *);
static tree cp_parser_gnu_attribute_list
  (cp_parser *);
static tree cp_parser_std_attribute
  (cp_parser *);
static tree cp_parser_std_attribute_spec
  (cp_parser *);
static tree cp_parser_std_attribute_spec_seq
  (cp_parser *);
static bool cp_parser_extension_opt
  (cp_parser *, int *);
static void cp_parser_label_declaration
  (cp_parser *);

/* Transactional Memory Extensions */

static tree cp_parser_transaction
  (cp_parser *, enum rid);
static tree cp_parser_transaction_expression
  (cp_parser *, enum rid);
static bool cp_parser_function_transaction
  (cp_parser *, enum rid);
static tree cp_parser_transaction_cancel
  (cp_parser *);

enum pragma_context {
  pragma_external,
  pragma_member,
  pragma_objc_icode,
  pragma_stmt,
  pragma_compound
};
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
  (vec<deferred_access_check, va_gc> *);
static tree cp_parser_single_declaration
  (cp_parser *, vec<deferred_access_check, va_gc> *, bool, bool, bool *);
static tree cp_parser_functional_cast
  (cp_parser *, tree);
static tree cp_parser_save_member_function_body
  (cp_parser *, cp_decl_specifier_seq *, cp_declarator *, tree);
static tree cp_parser_save_nsdmi
  (cp_parser *);
static tree cp_parser_enclosed_template_argument_list
  (cp_parser *);
static void cp_parser_save_default_args
  (cp_parser *, tree);
static void cp_parser_late_parsing_for_member
  (cp_parser *, tree);
static tree cp_parser_late_parse_one_default_arg
  (cp_parser *, tree, tree, tree);
static void cp_parser_late_parsing_nsdmi
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
  (cp_parser *, cp_decl_specifier_seq *, enum rid, cp_token *);
static void cp_parser_set_decl_spec_type
  (cp_decl_specifier_seq *, tree, cp_token *, bool);
static void set_and_check_decl_spec_loc
  (cp_decl_specifier_seq *decl_specs,
   cp_decl_spec ds, cp_token *);
static bool cp_parser_friend_p
  (const cp_decl_specifier_seq *);
static void cp_parser_required_error
  (cp_parser *, required_token, bool);
static cp_token *cp_parser_require
  (cp_parser *, enum cpp_ttype, required_token);
static cp_token *cp_parser_require_keyword
  (cp_parser *, enum rid, required_token);
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
static tree cp_parser_cache_defarg
  (cp_parser *parser, bool nsdmi);
static void cp_parser_parse_tentatively
  (cp_parser *);
static void cp_parser_commit_to_tentative_parse
  (cp_parser *);
static void cp_parser_commit_to_topmost_tentative_parse
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
  (cp_parser *, tree, tree, name_lookup_error, location_t);
static bool cp_parser_simulate_error
  (cp_parser *);
static bool cp_parser_check_type_definition
  (cp_parser *);
static void cp_parser_check_for_definition_in_return_type
  (cp_declarator *, tree, location_t type_location);
static void cp_parser_check_for_invalid_template_id
  (cp_parser *, tree, enum tag_types, location_t location);
static bool cp_parser_non_integral_constant_expression
  (cp_parser *, non_integral_constant);
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
static bool cp_parser_is_pure_string_literal
  (cp_token *);
static bool cp_parser_is_string_literal
  (cp_token *);
static bool cp_parser_is_keyword
  (cp_token *, enum rid);
static tree cp_parser_make_typename_type
  (cp_parser *, tree, tree, location_t location);
static cp_declarator * cp_parser_make_indirect_declarator
 (enum tree_code, tree, cp_cv_quals, cp_declarator *, tree);

/* Returns nonzero if we are parsing tentatively.  */

static inline bool
cp_parser_parsing_tentatively (cp_parser* parser)
{
  return parser->context->next != NULL;
}

/* Returns nonzero if TOKEN is a string literal.  */

static bool
cp_parser_is_pure_string_literal (cp_token* token)
{
  return (token->type == CPP_STRING ||
	  token->type == CPP_STRING16 ||
	  token->type == CPP_STRING32 ||
	  token->type == CPP_WSTRING ||
	  token->type == CPP_UTF8STRING);
}

/* Returns nonzero if TOKEN is a string literal
   of a user-defined string literal.  */

static bool
cp_parser_is_string_literal (cp_token* token)
{
  return (cp_parser_is_pure_string_literal (token) ||
	  token->type == CPP_STRING_USERDEF ||
	  token->type == CPP_STRING16_USERDEF ||
	  token->type == CPP_STRING32_USERDEF ||
	  token->type == CPP_WSTRING_USERDEF ||
	  token->type == CPP_UTF8STRING_USERDEF);
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
cp_parser_error (cp_parser* parser, const char* gmsgid)
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

      c_parse_error (gmsgid,
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
			     name_lookup_error desired,
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
    {
      switch (desired)
	{
	  case NLE_TYPE:
	    error_at (location, "%<%E::%E%> is not a type",
	    			parser->scope, name);
	    break;
	  case NLE_CXX98:
	    error_at (location, "%<%E::%E%> is not a class or namespace",
	    			parser->scope, name);
	    break;
	  case NLE_NOT_CXX98:
	    error_at (location,
	    	      "%<%E::%E%> is not a class, namespace, or enumeration",
		      parser->scope, name);
	    break;
	  default:
	    gcc_unreachable ();
	    
	}
    }
  else if (parser->scope == global_namespace)
    {
      switch (desired)
	{
	  case NLE_TYPE:
	    error_at (location, "%<::%E%> is not a type", name);
	    break;
	  case NLE_CXX98:
	    error_at (location, "%<::%E%> is not a class or namespace", name);
	    break;
	  case NLE_NOT_CXX98:
	    error_at (location,
		      "%<::%E%> is not a class, namespace, or enumeration",
		      name);
	    break;
	  default:
	    gcc_unreachable ();
	}
    }
  else
    {
      switch (desired)
	{
	  case NLE_TYPE:
	    error_at (location, "%qE is not a type", name);
	    break;
	  case NLE_CXX98:
	    error_at (location, "%qE is not a class or namespace", name);
	    break;
	  case NLE_NOT_CXX98:
	    error_at (location,
		      "%qE is not a class, namespace, or enumeration", name);
	    break;
	  default:
	    gcc_unreachable ();
	}
    }
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
					 tree type,
					 enum tag_types tag_type,
					 location_t location)
{
  cp_token_position start = 0;

  if (cp_lexer_next_token_is (parser->lexer, CPP_LESS))
    {
      if (TYPE_P (type))
	error_at (location, "%qT is not a template", type);
      else if (identifier_p (type))
	{
	  if (tag_type != none_type)
	    error_at (location, "%qE is not a class template", type);
	  else
	    error_at (location, "%qE is not a template", type);
	}
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
					    non_integral_constant thing)
{
  parser->non_integral_constant_expression_p = true;
  if (parser->integral_constant_expression_p)
    {
      if (!parser->allow_non_integral_constant_expression_p)
	{
	  const char *msg = NULL;
	  switch (thing)
	    {
  	      case NIC_FLOAT:
		error ("floating-point literal "
		       "cannot appear in a constant-expression");
		return true;
	      case NIC_CAST:
		error ("a cast to a type other than an integral or "
		       "enumeration type cannot appear in a "
		       "constant-expression");
		return true;
	      case NIC_TYPEID:
		error ("%<typeid%> operator "
		       "cannot appear in a constant-expression");
		return true;
	      case NIC_NCC:
		error ("non-constant compound literals "
		       "cannot appear in a constant-expression");
		return true;
	      case NIC_FUNC_CALL:
		error ("a function call "
		       "cannot appear in a constant-expression");
		return true;
	      case NIC_INC:
		error ("an increment "
		       "cannot appear in a constant-expression");
		return true;
	      case NIC_DEC:
		error ("an decrement "
		       "cannot appear in a constant-expression");
		return true;
	      case NIC_ARRAY_REF:
		error ("an array reference "
		       "cannot appear in a constant-expression");
		return true;
	      case NIC_ADDR_LABEL:
		error ("the address of a label "
		       "cannot appear in a constant-expression");
		return true;
	      case NIC_OVERLOADED:
		error ("calls to overloaded operators "
		       "cannot appear in a constant-expression");
		return true;
	      case NIC_ASSIGNMENT:
		error ("an assignment cannot appear in a constant-expression");
		return true;
	      case NIC_COMMA:
		error ("a comma operator "
		       "cannot appear in a constant-expression");
		return true;
	      case NIC_CONSTRUCTOR:
		error ("a call to a constructor "
		       "cannot appear in a constant-expression");
		return true;
	      case NIC_TRANSACTION:
		error ("a transaction expression "
		       "cannot appear in a constant-expression");
		return true;
	      case NIC_THIS:
		msg = "this";
		break;
	      case NIC_FUNC_NAME:
		msg = "__FUNCTION__";
		break;
  	      case NIC_PRETTY_FUNC:
		msg = "__PRETTY_FUNCTION__";
		break;
	      case NIC_C99_FUNC:
		msg = "__func__";
		break;
	      case NIC_VA_ARG:
		msg = "va_arg";
		break;
	      case NIC_ARROW:
		msg = "->";
		break;
	      case NIC_POINT:
		msg = ".";
		break;
	      case NIC_STAR:
		msg = "*";
		break;
	      case NIC_ADDR:
		msg = "&";
		break;
	      case NIC_PREINCREMENT:
		msg = "++";
		break;
	      case NIC_PREDECREMENT:
		msg = "--";
		break;
	      case NIC_NEW:
		msg = "new";
		break;
	      case NIC_DEL:
		msg = "delete";
		break;
	      default:
		gcc_unreachable ();
	    }
	  if (msg)
	    error ("%qs cannot appear in a constant-expression", msg);
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
  cp_parser_commit_to_tentative_parse (parser);
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
      if (cxx_dialect < cxx11 && id == ridpointers[(int)RID_CONSTEXPR])
	inform (location, "C++11 %<constexpr%> only available with "
		"-std=c++11 or -std=gnu++11");
      else if (processing_template_decl && current_class_type
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
		       field = DECL_CHAIN (field))
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
	{
	  if (cp_lexer_next_token_is (parser->lexer, CPP_LESS))
	    error_at (location_of (id),
		      "%qE in namespace %qE does not name a template type",
		      id, parser->scope);
	  else
	    error_at (location_of (id),
		      "%qE in namespace %qE does not name a type",
		      id, parser->scope);
	}
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
	{
	  if (cp_lexer_next_token_is (parser->lexer, CPP_LESS))
	    error_at (location_of (id),
		      "%qE in %q#T does not name a template type",
		      id, parser->scope);
	  else
	    error_at (location_of (id),
		      "%qE in %q#T does not name a type",
		      id, parser->scope);
	}
      else
	gcc_unreachable ();
    }
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
  if (!cp_parser_require (parser, CPP_SEMICOLON, RT_SEMICOLON))
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
  if (!cp_parser_require (parser, CPP_PRAGMA_EOL, RT_PRAGMA_EOL))
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
  if (identifier_p (id))
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
   cp_parser_ptr_operator.  ATTRIBUTES represent the attributes that
   appertain to the pointer or reference.  */

static cp_declarator *
cp_parser_make_indirect_declarator (enum tree_code code, tree class_type,
				    cp_cv_quals cv_qualifiers,
				    cp_declarator *target,
				    tree attributes)
{
  if (code == ERROR_MARK)
    return cp_error_declarator;

  if (code == INDIRECT_REF)
    if (class_type == NULL_TREE)
      return make_pointer_declarator (cv_qualifiers, target, attributes);
    else
      return make_ptrmem_declarator (cv_qualifiers, class_type,
				     target, attributes);
  else if (code == ADDR_EXPR && class_type == NULL_TREE)
    return make_reference_declarator (cv_qualifiers, target,
				      false, attributes);
  else if (code == NON_LVALUE_EXPR && class_type == NULL_TREE)
    return make_reference_declarator (cv_qualifiers, target,
				      true, attributes);
  gcc_unreachable ();
}

/* Create a new C++ parser.  */

static cp_parser *
cp_parser_new (void)
{
  cp_parser *parser;
  cp_lexer *lexer;
  unsigned i;

  /* cp_lexer_new_main is called before doing GC allocation because
     cp_lexer_new_main might load a PCH file.  */
  lexer = cp_lexer_new_main ();

  /* Initialize the binops_by_token so that we can get the tree
     directly from the token.  */
  for (i = 0; i < sizeof (binops) / sizeof (binops[0]); i++)
    binops_by_token[binops[i].token_type] = binops[i];

  parser = ggc_alloc_cleared_cp_parser ();
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

  /* We can correct until told otherwise.  */
  parser->colon_corrects_to_scope_p = true;

  /* The unparsed function queue is empty.  */
  push_unparsed_function_queues (parser);

  /* There are no classes being defined.  */
  parser->num_classes_being_defined = 0;

  /* No template parameters apply.  */
  parser->num_template_parameter_lists = 0;

  /* Not declaring an implicit function template.  */
  parser->auto_is_implicit_function_template_parm_p = false;
  parser->fully_implicit_function_template_p = false;
  parser->implicit_template_parms = 0;
  parser->implicit_template_scope = 0;

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
  token = cp_parser_require (parser, CPP_NAME, RT_NAME);
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
  enum cpp_ttype type, curr_type;
  int have_suffix_p = 0;
  tree string_tree;
  tree suffix_id = NULL_TREE;
  bool curr_tok_is_userdef_p = false;

  tok = cp_lexer_peek_token (parser->lexer);
  if (!cp_parser_is_string_literal (tok))
    {
      cp_parser_error (parser, "expected string-literal");
      return error_mark_node;
    }

  if (cpp_userdef_string_p (tok->type))
    {
      string_tree = USERDEF_LITERAL_VALUE (tok->u.value);
      curr_type = cpp_userdef_string_remove_type (tok->type);
      curr_tok_is_userdef_p = true;
    }
  else
    {
      string_tree = tok->u.value;
      curr_type = tok->type;
    }
  type = curr_type;

  /* Try to avoid the overhead of creating and destroying an obstack
     for the common case of just one string.  */
  if (!cp_parser_is_string_literal
      (cp_lexer_peek_nth_token (parser->lexer, 2)))
    {
      cp_lexer_consume_token (parser->lexer);

      str.text = (const unsigned char *)TREE_STRING_POINTER (string_tree);
      str.len = TREE_STRING_LENGTH (string_tree);
      count = 1;

      if (curr_tok_is_userdef_p)
	{
	  suffix_id = USERDEF_LITERAL_SUFFIX_ID (tok->u.value);
	  have_suffix_p = 1;
	  curr_type = cpp_userdef_string_remove_type (tok->type);
	}
      else
	curr_type = tok->type;

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
	  str.text = (const unsigned char *)TREE_STRING_POINTER (string_tree);
	  str.len = TREE_STRING_LENGTH (string_tree);

	  if (curr_tok_is_userdef_p)
	    {
	      tree curr_suffix_id = USERDEF_LITERAL_SUFFIX_ID (tok->u.value);
	      if (have_suffix_p == 0)
		{
		  suffix_id = curr_suffix_id;
		  have_suffix_p = 1;
		}
	      else if (have_suffix_p == 1
		       && curr_suffix_id != suffix_id)
		{
		  error ("inconsistent user-defined literal suffixes"
			 " %qD and %qD in string literal",
			 suffix_id, curr_suffix_id);
		  have_suffix_p = -1;
		}
	      curr_type = cpp_userdef_string_remove_type (tok->type);
	    }
	  else
	    curr_type = tok->type;

	  if (type != curr_type)
	    {
	      if (type == CPP_STRING)
		type = curr_type;
	      else if (curr_type != CPP_STRING)
		error_at (tok->location,
			  "unsupported non-standard concatenation "
			  "of string literals");
	    }

	  obstack_grow (&str_ob, &str, sizeof (cpp_string));

	  tok = cp_lexer_peek_token (parser->lexer);
	  if (cpp_userdef_string_p (tok->type))
	    {
	      string_tree = USERDEF_LITERAL_VALUE (tok->u.value);
	      curr_type = cpp_userdef_string_remove_type (tok->type);
	      curr_tok_is_userdef_p = true;
	    }
	  else
	    {
	      string_tree = tok->u.value;
	      curr_type = tok->type;
	      curr_tok_is_userdef_p = false;
	    }
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

      if (have_suffix_p)
	{
	  tree literal = build_userdef_literal (suffix_id, value,
						OT_NONE, NULL_TREE);
	  tok->u.value = literal;
	  return cp_parser_userdef_string_literal (tok);
	}
    }
  else
    /* cpp_interpret_string has issued an error.  */
    value = error_mark_node;

  if (count > 1)
    obstack_free (&str_ob, 0);

  return value;
}

/* Look up a literal operator with the name and the exact arguments.  */

static tree
lookup_literal_operator (tree name, vec<tree, va_gc> *args)
{
  tree decl, fns;
  decl = lookup_name (name);
  if (!decl || !is_overloaded_fn (decl))
    return error_mark_node;

  for (fns = decl; fns; fns = OVL_NEXT (fns))
    {
      unsigned int ix;
      bool found = true;
      tree fn = OVL_CURRENT (fns);
      tree parmtypes = TYPE_ARG_TYPES (TREE_TYPE (fn));
      if (parmtypes != NULL_TREE)
	{
	  for (ix = 0; ix < vec_safe_length (args) && parmtypes != NULL_TREE;
	       ++ix, parmtypes = TREE_CHAIN (parmtypes))
	    {
	      tree tparm = TREE_VALUE (parmtypes);
	      tree targ = TREE_TYPE ((*args)[ix]);
	      bool ptr = TYPE_PTR_P (tparm);
	      bool arr = TREE_CODE (targ) == ARRAY_TYPE;
	      if ((ptr || arr || !same_type_p (tparm, targ))
		  && (!ptr || !arr
		      || !same_type_p (TREE_TYPE (tparm),
				       TREE_TYPE (targ))))
		found = false;
	    }
	  if (found
	      && ix == vec_safe_length (args)
	      /* May be this should be sufficient_parms_p instead,
		 depending on how exactly should user-defined literals
		 work in presence of default arguments on the literal
		 operator parameters.  */
	      && parmtypes == void_list_node)
	    return fn;
	}
    }

  return error_mark_node;
}

/* Parse a user-defined char constant.  Returns a call to a user-defined
   literal operator taking the character as an argument.  */

static tree
cp_parser_userdef_char_literal (cp_parser *parser)
{
  cp_token *token = cp_lexer_consume_token (parser->lexer);
  tree literal = token->u.value;
  tree suffix_id = USERDEF_LITERAL_SUFFIX_ID (literal);
  tree value = USERDEF_LITERAL_VALUE (literal);
  tree name = cp_literal_operator_id (IDENTIFIER_POINTER (suffix_id));
  tree decl, result;

  /* Build up a call to the user-defined operator  */
  /* Lookup the name we got back from the id-expression.  */
  vec<tree, va_gc> *args = make_tree_vector ();
  vec_safe_push (args, value);
  decl = lookup_literal_operator (name, args);
  if (!decl || decl == error_mark_node)
    {
      error ("unable to find character literal operator %qD with %qT argument",
	     name, TREE_TYPE (value));
      release_tree_vector (args);
      return error_mark_node;
    }
  result = finish_call_expr (decl, &args, false, true, tf_warning_or_error);
  release_tree_vector (args);
  if (result != error_mark_node)
    return result;

  error ("unable to find character literal operator %qD with %qT argument",
	 name, TREE_TYPE (value));
  return error_mark_node;
}

/* A subroutine of cp_parser_userdef_numeric_literal to
   create a char... template parameter pack from a string node.  */

static tree
make_char_string_pack (tree value)
{
  tree charvec;
  tree argpack = make_node (NONTYPE_ARGUMENT_PACK);
  const char *str = TREE_STRING_POINTER (value);
  int i, len = TREE_STRING_LENGTH (value) - 1;
  tree argvec = make_tree_vec (1);

  /* Fill in CHARVEC with all of the parameters.  */
  charvec = make_tree_vec (len);
  for (i = 0; i < len; ++i)
    TREE_VEC_ELT (charvec, i) = build_int_cst (char_type_node, str[i]);

  /* Build the argument packs.  */
  SET_ARGUMENT_PACK_ARGS (argpack, charvec);
  TREE_TYPE (argpack) = char_type_node;

  TREE_VEC_ELT (argvec, 0) = argpack;

  return argvec;
}

/* A subroutine of cp_parser_userdef_numeric_literal to
   create a char... template parameter pack from a string node.  */

static tree
make_string_pack (tree value)
{
  tree charvec;
  tree argpack = make_node (NONTYPE_ARGUMENT_PACK);
  const unsigned char *str
    = (const unsigned char *) TREE_STRING_POINTER (value);
  int sz = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (value))));
  int len = TREE_STRING_LENGTH (value) / sz - 1;
  tree argvec = make_tree_vec (2);

  tree str_char_type_node = TREE_TYPE (TREE_TYPE (value));
  str_char_type_node = TYPE_MAIN_VARIANT (str_char_type_node);

  /* First template parm is character type.  */
  TREE_VEC_ELT (argvec, 0) = str_char_type_node;

  /* Fill in CHARVEC with all of the parameters.  */
  charvec = make_tree_vec (len);
  for (int i = 0; i < len; ++i)
    TREE_VEC_ELT (charvec, i)
      = double_int_to_tree (str_char_type_node,
			    double_int::from_buffer (str + i * sz, sz));

  /* Build the argument packs.  */
  SET_ARGUMENT_PACK_ARGS (argpack, charvec);
  TREE_TYPE (argpack) = str_char_type_node;

  TREE_VEC_ELT (argvec, 1) = argpack;

  return argvec;
}

/* Parse a user-defined numeric constant.  returns a call to a user-defined
   literal operator.  */

static tree
cp_parser_userdef_numeric_literal (cp_parser *parser)
{
  cp_token *token = cp_lexer_consume_token (parser->lexer);
  tree literal = token->u.value;
  tree suffix_id = USERDEF_LITERAL_SUFFIX_ID (literal);
  tree value = USERDEF_LITERAL_VALUE (literal);
  int overflow = USERDEF_LITERAL_OVERFLOW (literal);
  tree num_string = USERDEF_LITERAL_NUM_STRING (literal);
  tree name = cp_literal_operator_id (IDENTIFIER_POINTER (suffix_id));
  tree decl, result;
  vec<tree, va_gc> *args;

  /* Look for a literal operator taking the exact type of numeric argument
     as the literal value.  */
  args = make_tree_vector ();
  vec_safe_push (args, value);
  decl = lookup_literal_operator (name, args);
  if (decl && decl != error_mark_node)
    {
      result = finish_call_expr (decl, &args, false, true, tf_none);
      if (result != error_mark_node)
	{
	  if (TREE_CODE (TREE_TYPE (value)) == INTEGER_TYPE && overflow > 0)
	    warning_at (token->location, OPT_Woverflow,
		        "integer literal exceeds range of %qT type",
		        long_long_unsigned_type_node);
	  else
	    {
	      if (overflow > 0)
		warning_at (token->location, OPT_Woverflow,
			    "floating literal exceeds range of %qT type",
			    long_double_type_node);
	      else if (overflow < 0)
		warning_at (token->location, OPT_Woverflow,
			    "floating literal truncated to zero");
	    }
	  release_tree_vector (args);
	  return result;
	}
    }
  release_tree_vector (args);

  /* If the numeric argument didn't work, look for a raw literal
     operator taking a const char* argument consisting of the number
     in string format.  */
  args = make_tree_vector ();
  vec_safe_push (args, num_string);
  decl = lookup_literal_operator (name, args);
  if (decl && decl != error_mark_node)
    {
      result = finish_call_expr (decl, &args, false, true, tf_none);
      if (result != error_mark_node)
	{
	  release_tree_vector (args);
	  return result;
	}
    }
  release_tree_vector (args);

  /* If the raw literal didn't work, look for a non-type template
     function with parameter pack char....  Call the function with
     template parameter characters representing the number.  */
  args = make_tree_vector ();
  decl = lookup_literal_operator (name, args);
  if (decl && decl != error_mark_node)
    {
      tree tmpl_args = make_char_string_pack (num_string);
      decl = lookup_template_function (decl, tmpl_args);
      result = finish_call_expr (decl, &args, false, true, tf_none);
      if (result != error_mark_node)
	{
	  release_tree_vector (args);
	  return result;
	}
    }
  release_tree_vector (args);

  error ("unable to find numeric literal operator %qD", name);
  if (!cpp_get_options (parse_in)->ext_numeric_literals)
    inform (token->location, "use -std=gnu++11 or -fext-numeric-literals "
	    "to enable more built-in suffixes");
  return error_mark_node;
}

/* Parse a user-defined string constant.  Returns a call to a user-defined
   literal operator taking a character pointer and the length of the string
   as arguments.  */

static tree
cp_parser_userdef_string_literal (cp_token *token)
{
  tree literal = token->u.value;
  tree suffix_id = USERDEF_LITERAL_SUFFIX_ID (literal);
  tree name = cp_literal_operator_id (IDENTIFIER_POINTER (suffix_id));
  tree value = USERDEF_LITERAL_VALUE (literal);
  int len = TREE_STRING_LENGTH (value)
	/ TREE_INT_CST_LOW (TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (value)))) - 1;
  tree decl, result;
  vec<tree, va_gc> *args;

  /* Look for a template function with typename parameter CharT
     and parameter pack CharT...  Call the function with
     template parameter characters representing the string.  */
  args = make_tree_vector ();
  decl = lookup_literal_operator (name, args);
  if (decl && decl != error_mark_node)
    {
      tree tmpl_args = make_string_pack (value);
      decl = lookup_template_function (decl, tmpl_args);
      result = finish_call_expr (decl, &args, false, true, tf_none);
      if (result != error_mark_node)
	{
	  release_tree_vector (args);
	  return result;
	}
    }
  release_tree_vector (args);

  /* Build up a call to the user-defined operator  */
  /* Lookup the name we got back from the id-expression.  */
  args = make_tree_vector ();
  vec_safe_push (args, value);
  vec_safe_push (args, build_int_cst (size_type_node, len));
  decl = lookup_name (name);
  if (!decl || decl == error_mark_node)
    {
      error ("unable to find string literal operator %qD", name);
      release_tree_vector (args);
      return error_mark_node;
    }
  result = finish_call_expr (decl, &args, false, true, tf_none);
  release_tree_vector (args);
  if (result != error_mark_node)
    return result;

  error ("unable to find string literal operator %qD with %qT, %qT arguments",
	 name, TREE_TYPE (value), size_type_node);
  return error_mark_node;
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

/* Return the appropriate tsubst flags for parsing, possibly in N3276
   decltype context.  */

static inline tsubst_flags_t
complain_flags (bool decltype_p)
{
  tsubst_flags_t complain = tf_warning_or_error;
  if (decltype_p)
    complain |= tf_decltype;
  return complain;
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
     __is_final ( type-id )
     __is_literal_type ( type-id )
     __is_pod ( type-id )
     __is_polymorphic ( type-id )
     __is_std_layout ( type-id )
     __is_trivial ( type-id )
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
			      bool decltype_p,
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
	   boolean-literal
	   pointer-literal
	   user-defined-literal  */
    case CPP_CHAR:
    case CPP_CHAR16:
    case CPP_CHAR32:
    case CPP_WCHAR:
    case CPP_NUMBER:
      if (TREE_CODE (token->u.value) == USERDEF_LITERAL)
	return cp_parser_userdef_numeric_literal (parser);
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
	    cp_parser_non_integral_constant_expression (parser, NIC_FLOAT);
	}
      return token->u.value;

    case CPP_CHAR_USERDEF:
    case CPP_CHAR16_USERDEF:
    case CPP_CHAR32_USERDEF:
    case CPP_WCHAR_USERDEF:
      return cp_parser_userdef_char_literal (parser);

    case CPP_STRING:
    case CPP_STRING16:
    case CPP_STRING32:
    case CPP_WSTRING:
    case CPP_UTF8STRING:
    case CPP_STRING_USERDEF:
    case CPP_STRING16_USERDEF:
    case CPP_STRING32_USERDEF:
    case CPP_WSTRING_USERDEF:
    case CPP_UTF8STRING_USERDEF:
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
	    pedwarn (token->location, OPT_Wpedantic, 
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
		cp_parser_compound_statement (parser, expr, false, false);
		/* Finish up.  */
		expr = finish_stmt_expr (expr, false);
	      }
	  }
	else
	  {
	    /* Parse the parenthesized expression.  */
	    expr = cp_parser_expression (parser, cast_p, decltype_p, idk);
	    /* Let the front end know that this expression was
	       enclosed in parentheses. This matters in case, for
	       example, the expression is of the form `A::B', since
	       `&A::B' might be a pointer-to-member, but `&(A::B)' is
	       not.  */
	    expr = finish_parenthesized_expr (expr);
	    /* DR 705: Wrapping an unqualified name in parentheses
	       suppresses arg-dependent lookup.  We want to pass back
	       CP_ID_KIND_QUALIFIED for suppressing vtable lookup
	       (c++/37862), but none of the others.  */
	    if (*idk != CP_ID_KIND_QUALIFIED)
	      *idk = CP_ID_KIND_NONE;
	  }
	/* The `>' token might be the end of a template-id or
	   template-parameter-list now.  */
	parser->greater_than_is_operator_p
	  = saved_greater_than_is_operator_p;
	/* Consume the `)'.  */
	if (!cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN))
	  cp_parser_skip_to_end_of_statement (parser);

	return expr;
      }

    case CPP_OPEN_SQUARE:
      if (c_dialect_objc ())
        /* We have an Objective-C++ message. */
        return cp_parser_objc_expression (parser);
      {
	tree lam = cp_parser_lambda_expression (parser);
	/* Don't warn about a failed tentative parse.  */
	if (cp_parser_error_occurred (parser))
	  return error_mark_node;
	maybe_warn_cpp0x (CPP0X_LAMBDA_EXPR);
	return lam;
      }

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

	  /* The `nullptr' literal.  */
	case RID_NULLPTR:
	  cp_lexer_consume_token (parser->lexer);
	  return nullptr_node;

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
	  if (cp_parser_non_integral_constant_expression (parser, NIC_THIS))
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
	    non_integral_constant name;

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
		name = NIC_FUNC_NAME;
		break;
	      case RID_PRETTY_FUNCTION_NAME:
		name = NIC_PRETTY_FUNC;
		break;
	      case RID_C99_FUNCTION_NAME:
		name = NIC_C99_FUNC;
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
	    source_location type_location;

	    /* The `__builtin_va_arg' construct is used to handle
	       `va_arg'.  Consume the `__builtin_va_arg' token.  */
	    cp_lexer_consume_token (parser->lexer);
	    /* Look for the opening `('.  */
	    cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN);
	    /* Now, parse the assignment-expression.  */
	    expression = cp_parser_assignment_expression (parser,
							  /*cast_p=*/false, NULL);
	    /* Look for the `,'.  */
	    cp_parser_require (parser, CPP_COMMA, RT_COMMA);
	    type_location = cp_lexer_peek_token (parser->lexer)->location;
	    /* Parse the type-id.  */
	    type = cp_parser_type_id (parser);
	    /* Look for the closing `)'.  */
	    cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN);
	    /* Using `va_arg' in a constant-expression is not
	       allowed.  */
	    if (cp_parser_non_integral_constant_expression (parser,
							    NIC_VA_ARG))
	      return error_mark_node;
	    return build_x_va_arg (type_location, expression, type);
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
	case RID_IS_FINAL:
	case RID_IS_LITERAL_TYPE:
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

	case RID_TEMPLATE:
	  if (parser->in_function_body
	      && (cp_lexer_peek_nth_token (parser->lexer, 2)->type
	      	  == CPP_LESS))
	    {
	      error_at (token->location,
			"a template declaration cannot appear at block scope");
	      cp_parser_skip_to_end_of_block_or_statement (parser);
	      return error_mark_node;
	    }
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

static inline tree
cp_parser_primary_expression (cp_parser *parser,
			      bool address_p,
			      bool cast_p,
			      bool template_arg_p,
			      cp_id_kind *idk)
{
  return cp_parser_primary_expression (parser, address_p, cast_p, template_arg_p,
				       /*decltype*/false, idk);
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
				  none_type,
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
				    none_type,
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
				    none_type,
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

	/* If the name is of the form "X::~X" it's OK even if X is a
	   typedef.  */
	token = cp_lexer_peek_token (parser->lexer);
	if (scope
	    && token->type == CPP_NAME
	    && (cp_lexer_peek_nth_token (parser->lexer, 2)->type
		!= CPP_LESS)
	    && (token->u.value == TYPE_IDENTIFIER (scope)
		|| (CLASS_TYPE_P (scope)
		    && constructor_name_p (token->u.value, scope))))
	  {
	    cp_lexer_consume_token (parser->lexer);
	    return build_nt (BIT_NOT_EXPR, scope);
	  }

	/* ~auto means the destructor of whatever the object is.  */
	if (cp_parser_is_keyword (token, RID_AUTO))
	  {
	    if (cxx_dialect < cxx1y)
	      pedwarn (input_location, 0,
		       "%<~auto%> only available with "
		       "-std=c++1y or -std=gnu++1y");
	    cp_lexer_consume_token (parser->lexer);
	    return build_nt (BIT_NOT_EXPR, make_auto ());
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
				      none_type,
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
	  else if (UDLIT_OPER_P (id))
	    {
	      /* 17.6.3.3.5  */
	      const char *name = UDLIT_OP_SUFFIX (id);
	      if (name[0] != '_' && !in_system_header_at (input_location)
		  && declarator_p)
		warning (0, "literal operator suffixes not preceded by %<_%>"
			    " are reserved for future standardization");
	    }

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
      /* DR 743: decltype can be used in a nested-name-specifier.  */
      else if (token_is_decltype (token))
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

	  if (token->type == CPP_COLON
	      && parser->colon_corrects_to_scope_p
	      && cp_lexer_peek_nth_token (parser->lexer, 3)->type == CPP_NAME)
	    {
	      error_at (token->location,
			"found %<:%> in nested-name-specifier, expected %<::%>");
	      token->type = CPP_SCOPE;
	    }

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
      cp_parser_require (parser, CPP_SCOPE, RT_SCOPE);

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

	  /* If the next token is a decltype, and the one after that is a
	     `::', then the decltype has failed to resolve to a class or
	     enumeration type.  Give this error even when parsing
	     tentatively since it can't possibly be valid--and we're going
	     to replace it with a CPP_NESTED_NAME_SPECIFIER below, so we
	     won't get another chance.*/
	  if (cp_lexer_next_token_is (parser->lexer, CPP_DECLTYPE)
	      && (cp_lexer_peek_nth_token (parser->lexer, 2)->type
		  == CPP_SCOPE))
	    {
	      token = cp_lexer_consume_token (parser->lexer);
	      error_at (token->location, "decltype evaluates to %qT, "
			"which is not a class or enumeration type",
			token->u.value);
	      parser->scope = error_mark_node;
	      error_p = true;
	      /* As below.  */
	      success = true;
	      cp_lexer_consume_token (parser->lexer);
	    }

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
			  // cp_parser_lookup_name has the same diagnostic,
			  // thus make sure to emit it at most once.
			  if (cp_parser_uncommitted_to_tentative_parse_p
			      (parser))
			    {
			      error_at (token->location,
					"reference to %qD is ambiguous",
					token->u.value);
			      print_candidates (ambiguous_decls);
			    }
			  decl = error_mark_node;
			}
		      else
                        {
                          if (cxx_dialect != cxx98)
                            cp_parser_name_lookup_error
                            (parser, token->u.value, decl, NLE_NOT_CXX98,
	  		     token->location);
			  else
			    cp_parser_name_lookup_error
			    (parser, token->u.value, decl, NLE_CXX98,
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
		   ? G_("%qT is not a template")
		   : G_("%qD is not a template"),
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
      token->u.tree_check_value = ggc_alloc_cleared_tree_check ();
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
   type or a simple-template-id.

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

  /* DR 743: decltype can appear in a nested-name-specifier.  */
  if (cp_lexer_next_token_is_decltype (parser->lexer))
    {
      scope = cp_parser_decltype (parser);
      if (TREE_CODE (scope) != ENUMERAL_TYPE
	  && !MAYBE_CLASS_TYPE_P (scope))
	{
	  cp_parser_simulate_error (parser);
	  return error_mark_node;
	}
      if (TYPE_NAME (scope))
	scope = TYPE_NAME (scope);
      return scope;
    }

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
     
      /* Parse a type-name  */
      scope = cp_parser_type_name (parser);

      /* "If the name found does not designate a namespace or a class,
	 enumeration, or dependent type, the program is ill-formed."

         We cover classes and dependent types above and namespaces below,
         so this code is only looking for enums.  */
      if (!scope || TREE_CODE (scope) != TYPE_DECL
	  || TREE_CODE (TREE_TYPE (scope)) != ENUMERAL_TYPE)
	cp_parser_simulate_error (parser);

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
                              bool member_access_only_p, bool decltype_p,
			      cp_id_kind * pidk_return)
{
  cp_token *token;
  location_t loc;
  enum rid keyword;
  cp_id_kind idk = CP_ID_KIND_NONE;
  tree postfix_expression = NULL_TREE;
  bool is_member_access = false;
  int saved_in_statement = -1;

  /* Peek at the next token.  */
  token = cp_lexer_peek_token (parser->lexer);
  loc = token->location;
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
	bool saved_in_type_id_in_expr_p;

	/* All of these can be handled in the same way from the point
	   of view of parsing.  Begin by consuming the token
	   identifying the cast.  */
	cp_lexer_consume_token (parser->lexer);

	/* New types cannot be defined in the cast.  */
	saved_message = parser->type_definition_forbidden_message;
	parser->type_definition_forbidden_message
	  = G_("types may not be defined in casts");

	/* Look for the opening `<'.  */
	cp_parser_require (parser, CPP_LESS, RT_LESS);
	/* Parse the type to which we are casting.  */
	saved_in_type_id_in_expr_p = parser->in_type_id_in_expr_p;
	parser->in_type_id_in_expr_p = true;
	type = cp_parser_type_id (parser);
	parser->in_type_id_in_expr_p = saved_in_type_id_in_expr_p;
	/* Look for the closing `>'.  */
	cp_parser_require (parser, CPP_GREATER, RT_GREATER);
	/* Restore the old message.  */
	parser->type_definition_forbidden_message = saved_message;

	bool saved_greater_than_is_operator_p
	  = parser->greater_than_is_operator_p;
	parser->greater_than_is_operator_p = true;

	/* And the expression which is being cast.  */
	cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN);
	expression = cp_parser_expression (parser, /*cast_p=*/true, & idk);
	cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN);

	parser->greater_than_is_operator_p
	  = saved_greater_than_is_operator_p;

	/* Only type conversions to integral or enumeration types
	   can be used in constant-expressions.  */
	if (!cast_valid_in_integral_constant_expression_p (type)
	    && cp_parser_non_integral_constant_expression (parser, NIC_CAST))
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
	cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN);
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
	cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN);
	/* If all went well, simply lookup the type-id.  */
	if (cp_parser_parse_definitely (parser))
	  postfix_expression = get_typeid (type, tf_warning_or_error);
	/* Otherwise, fall back to the expression variant.  */
	else
	  {
	    tree expression;

	    /* Look for an expression.  */
	    expression = cp_parser_expression (parser, /*cast_p=*/false, & idk);
	    /* Compute its typeid.  */
	    postfix_expression = build_typeid (expression, tf_warning_or_error);
	    /* Look for the `)' token.  */
	    cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN);
	  }
	/* Restore the saved message.  */
	parser->type_definition_forbidden_message = saved_message;
	/* `typeid' may not appear in an integral constant expression.  */
	if (cp_parser_non_integral_constant_expression (parser, NIC_TYPEID))
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

    case RID_CILK_SPAWN:
      {
	cp_lexer_consume_token (parser->lexer);
	token = cp_lexer_peek_token (parser->lexer);
	if (token->type == CPP_SEMICOLON)
	  {
	    error_at (token->location, "%<_Cilk_spawn%> must be followed by "
		      "an expression");
	    postfix_expression = error_mark_node;
	    break;
	  }
	else if (!current_function_decl)
	  {
	    error_at (token->location, "%<_Cilk_spawn%> may only be used "
		      "inside a function");
	    postfix_expression = error_mark_node;
	    break;
	  }
	else
	  {
	    /* Consecutive _Cilk_spawns are not allowed in a statement.  */
	    saved_in_statement = parser->in_statement;
	    parser->in_statement |= IN_CILK_SPAWN;
	  }
	cfun->calls_cilk_spawn = 1;
	postfix_expression = 
	  cp_parser_postfix_expression (parser, false, false, 
					false, false, &idk);
	if (!flag_cilkplus)
	  {
	    error_at (token->location, "-fcilkplus must be enabled to use"
		      " %<_Cilk_spawn%>");
	    cfun->calls_cilk_spawn = 0;
	  }
	else if (saved_in_statement & IN_CILK_SPAWN)
	  {
	    error_at (token->location, "consecutive %<_Cilk_spawn%> keywords "
		      "are not permitted");
	    postfix_expression = error_mark_node;
	    cfun->calls_cilk_spawn = 0; 
	  }
	else
	  {
	    postfix_expression = build_cilk_spawn (token->location, 
						   postfix_expression);
	    if (postfix_expression != error_mark_node) 
	      SET_EXPR_LOCATION (postfix_expression, input_location);
	    parser->in_statement = parser->in_statement & ~IN_CILK_SPAWN;
	  }
	break;
      }
      
    case RID_CILK_SYNC:
      if (flag_cilkplus)
	{ 
	  tree sync_expr = build_cilk_sync ();
	  SET_EXPR_LOCATION (sync_expr, 
			     cp_lexer_peek_token (parser->lexer)->location);
	  finish_expr_stmt (sync_expr);
	}
      else
	error_at (token->location, "-fcilkplus must be enabled to use" 
		  " %<_Cilk_sync%>");
      cp_lexer_consume_token (parser->lexer);
      break;

    case RID_BUILTIN_SHUFFLE:
      {
	vec<tree, va_gc> *vec;
	unsigned int i;
	tree p;

	cp_lexer_consume_token (parser->lexer);
	vec = cp_parser_parenthesized_expression_list (parser, non_attr,
		    /*cast_p=*/false, /*allow_expansion_p=*/true,
		    /*non_constant_p=*/NULL);
	if (vec == NULL)
	  return error_mark_node;

	FOR_EACH_VEC_ELT (*vec, i, p)
	  mark_exp_read (p);

	if (vec->length () == 2)
	  return build_x_vec_perm_expr (loc, (*vec)[0], NULL_TREE, (*vec)[1],
					 tf_warning_or_error);
	else if (vec->length () == 3)
	  return build_x_vec_perm_expr (loc, (*vec)[0], (*vec)[1], (*vec)[2],
					 tf_warning_or_error);
	else
	{
	  error_at (loc, "wrong number of arguments to "
	      "%<__builtin_shuffle%>");
	  return error_mark_node;
	}
	break;
      }

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
	    tree initializer = NULL_TREE;
	    bool compound_literal_p;

	    cp_parser_parse_tentatively (parser);
	    /* Consume the `('.  */
	    cp_lexer_consume_token (parser->lexer);

	    /* Avoid calling cp_parser_type_id pointlessly, see comment
	       in cp_parser_cast_expression about c++/29234.  */
	    cp_lexer_save_tokens (parser->lexer);

	    compound_literal_p
	      = (cp_parser_skip_to_closing_parenthesis (parser, false, false,
							/*consume_paren=*/true)
		 && cp_lexer_next_token_is (parser->lexer, CPP_OPEN_BRACE));

	    /* Roll back the tokens we skipped.  */
	    cp_lexer_rollback_tokens (parser->lexer);

	    if (!compound_literal_p)
	      cp_parser_simulate_error (parser);
	    else
	      {
		/* Parse the type.  */
		bool saved_in_type_id_in_expr_p = parser->in_type_id_in_expr_p;
		parser->in_type_id_in_expr_p = true;
		type = cp_parser_type_id (parser);
		parser->in_type_id_in_expr_p = saved_in_type_id_in_expr_p;
		/* Look for the `)'.  */
		cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN);
	      }

	    /* If things aren't going well, there's no need to
	       keep going.  */
	    if (!cp_parser_error_occurred (parser))
	      {
		bool non_constant_p;
		/* Parse the brace-enclosed initializer list.  */
		initializer = cp_parser_braced_list (parser,
						     &non_constant_p);
	      }
	    /* If that worked, we're definitely looking at a
	       compound-literal expression.  */
	    if (cp_parser_parse_definitely (parser))
	      {
		/* Warn the user that a compound literal is not
		   allowed in standard C++.  */
		pedwarn (input_location, OPT_Wpedantic,
			 "ISO C++ forbids compound-literals");
		/* For simplicity, we disallow compound literals in
		   constant-expressions.  We could
		   allow compound literals of integer type, whose
		   initializer was a constant, in constant
		   expressions.  Permitting that usage, as a further
		   extension, would not change the meaning of any
		   currently accepted programs.  (Of course, as
		   compound literals are not part of ISO C++, the
		   standard has nothing to say.)  */
		if (cp_parser_non_integral_constant_expression (parser,
								NIC_NCC))
		  {
		    postfix_expression = error_mark_node;
		    break;
		  }
		/* Form the representation of the compound-literal.  */
		postfix_expression
		  = finish_compound_literal (type, initializer,
					     tf_warning_or_error);
		break;
	      }
	  }

	/* It must be a primary-expression.  */
	postfix_expression
	  = cp_parser_primary_expression (parser, address_p, cast_p,
					  /*template_arg_p=*/false,
					  decltype_p,
					  &idk);
      }
      break;
    }

  /* Note that we don't need to worry about calling build_cplus_new on a
     class-valued CALL_EXPR in decltype when it isn't the end of the
     postfix-expression; unary_complex_lvalue will take care of that for
     all these cases.  */

  /* Keep looping until the postfix-expression is complete.  */
  while (true)
    {
      if (idk == CP_ID_KIND_UNQUALIFIED
	  && identifier_p (postfix_expression)
	  && cp_lexer_next_token_is_not (parser->lexer, CPP_OPEN_PAREN))
	/* It is not a Koenig lookup function call.  */
	postfix_expression
	  = unqualified_name_lookup_error (postfix_expression);

      /* Peek at the next token.  */
      token = cp_lexer_peek_token (parser->lexer);

      switch (token->type)
	{
	case CPP_OPEN_SQUARE:
	  if (cp_next_tokens_can_be_std_attribute_p (parser))
	    {
	      cp_parser_error (parser,
			       "two consecutive %<[%> shall "
			       "only introduce an attribute");
	      return error_mark_node;
	    }
	  postfix_expression
	    = cp_parser_postfix_open_square_expression (parser,
							postfix_expression,
							false,
							decltype_p);
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
	    tsubst_flags_t complain = complain_flags (decltype_p);
	    vec<tree, va_gc> *args;

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
		    (parser, non_attr,
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
							       NIC_FUNC_CALL))
	      {
		postfix_expression = error_mark_node;
		release_tree_vector (args);
		break;
	      }

	    koenig_p = false;
	    if (idk == CP_ID_KIND_UNQUALIFIED
		|| idk == CP_ID_KIND_TEMPLATE_ID)
	      {
		if (identifier_p (postfix_expression))
		  {
		    if (!args->is_empty ())
		      {
			koenig_p = true;
			if (!any_type_dependent_arguments_p (args))
			  postfix_expression
			    = perform_koenig_lookup (postfix_expression, args,
						     complain);
		      }
		    else
		      postfix_expression
			= unqualified_fn_lookup_error (postfix_expression);
		  }
		/* We do not perform argument-dependent lookup if
		   normal lookup finds a non-function, in accordance
		   with the expected resolution of DR 218.  */
		else if (!args->is_empty ()
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
			    = perform_koenig_lookup (postfix_expression, args,
						     complain);
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
			 ? LOOKUP_NORMAL|LOOKUP_NONVIRTUAL
			 : LOOKUP_NORMAL),
			/*fn_p=*/NULL,
			complain));
		  }
		else
		  postfix_expression
		    = finish_call_expr (postfix_expression, &args,
					/*disallow_virtual=*/false,
					/*koenig_p=*/false,
					complain);
	      }
	    else if (TREE_CODE (postfix_expression) == OFFSET_REF
		     || TREE_CODE (postfix_expression) == MEMBER_REF
		     || TREE_CODE (postfix_expression) == DOTSTAR_EXPR)
	      postfix_expression = (build_offset_ref_call_from_tree
				    (postfix_expression, &args,
				     complain));
	    else if (idk == CP_ID_KIND_QUALIFIED)
	      /* A call to a static class member, or a namespace-scope
		 function.  */
	      postfix_expression
		= finish_call_expr (postfix_expression, &args,
				    /*disallow_virtual=*/true,
				    koenig_p,
				    complain);
	    else
	      /* All other function calls.  */
	      postfix_expression
		= finish_call_expr (postfix_expression, &args,
				    /*disallow_virtual=*/false,
				    koenig_p,
				    complain);

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
						      false, &idk, loc);

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
	  if (cp_parser_non_integral_constant_expression (parser, NIC_INC))
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
	  if (cp_parser_non_integral_constant_expression (parser, NIC_DEC))
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

/* This function parses Cilk Plus array notations.  If a normal array expr. is
   parsed then the array index is passed back to the caller through *INIT_INDEX 
   and the function returns a NULL_TREE.  If array notation expr. is parsed, 
   then *INIT_INDEX is ignored by the caller and the function returns 
   a tree of type ARRAY_NOTATION_REF.  If some error occurred it returns 
   error_mark_node.  */

static tree
cp_parser_array_notation (location_t loc, cp_parser *parser, tree *init_index,
			  tree array_value)
{
  cp_token *token = NULL;
  tree length_index, stride = NULL_TREE, value_tree, array_type;
  if (!array_value || array_value == error_mark_node)
    {
      cp_parser_skip_to_end_of_statement (parser);
      return error_mark_node;
    }

  array_type = TREE_TYPE (array_value);
  
  bool saved_colon_corrects = parser->colon_corrects_to_scope_p;
  parser->colon_corrects_to_scope_p = false;
  token = cp_lexer_peek_token (parser->lexer);
  
  if (!token)
    {
      cp_parser_error (parser, "expected %<:%> or numeral");
      return error_mark_node;
    }
  else if (token->type == CPP_COLON)
    {
      /* Consume the ':'.  */
      cp_lexer_consume_token (parser->lexer);
      
      /* If we are here, then we have a case like this A[:].  */
      if (cp_lexer_peek_token (parser->lexer)->type != CPP_CLOSE_SQUARE)
	{
	  cp_parser_error (parser, "expected %<]%>");
	  cp_parser_skip_to_end_of_statement (parser);
	  return error_mark_node;
	}
      *init_index = NULL_TREE;
      stride = NULL_TREE;
      length_index = NULL_TREE;
    }
  else
    {
      /* If we are here, then there are three valid possibilities:
	 1. ARRAY [ EXP ]
	 2. ARRAY [ EXP : EXP ]
	 3. ARRAY [ EXP : EXP : EXP ]  */

      *init_index = cp_parser_expression (parser, false, NULL);	
      if (cp_lexer_peek_token (parser->lexer)->type != CPP_COLON)
	{  
	  /* This indicates that we have a normal array expression.  */
	  parser->colon_corrects_to_scope_p = saved_colon_corrects;
	  return NULL_TREE;
	}
      
      /* Consume the ':'.  */
      cp_lexer_consume_token (parser->lexer);
      length_index = cp_parser_expression (parser, false, NULL);
      if (cp_lexer_peek_token (parser->lexer)->type == CPP_COLON)
	{
	  cp_lexer_consume_token (parser->lexer);
	  stride = cp_parser_expression (parser, false, NULL);
	}
    }
  parser->colon_corrects_to_scope_p = saved_colon_corrects;

  if (*init_index == error_mark_node || length_index == error_mark_node
      || stride == error_mark_node)
    {
      if (cp_lexer_peek_token (parser->lexer)->type == CPP_CLOSE_SQUARE)
	cp_lexer_consume_token (parser->lexer);
      return error_mark_node;
    }
  cp_parser_require (parser, CPP_CLOSE_SQUARE, RT_CLOSE_SQUARE);

  value_tree = build_array_notation_ref (loc, array_value, *init_index, 
					 length_index, stride, array_type);
  return value_tree;
}

/* A subroutine of cp_parser_postfix_expression that also gets hijacked
   by cp_parser_builtin_offsetof.  We're looking for

     postfix-expression [ expression ]
     postfix-expression [ braced-init-list ] (C++11)

   FOR_OFFSETOF is set if we're being called in that context, which
   changes how we deal with integer constant expressions.  */

static tree
cp_parser_postfix_open_square_expression (cp_parser *parser,
					  tree postfix_expression,
					  bool for_offsetof,
					  bool decltype_p)
{
  tree index = NULL_TREE;
  location_t loc = cp_lexer_peek_token (parser->lexer)->location;
  bool saved_greater_than_is_operator_p;

  /* Consume the `[' token.  */
  cp_lexer_consume_token (parser->lexer);

  saved_greater_than_is_operator_p = parser->greater_than_is_operator_p;
  parser->greater_than_is_operator_p = true;

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
    {
      if (cp_lexer_next_token_is (parser->lexer, CPP_OPEN_BRACE))
	{
	  bool expr_nonconst_p;
	  cp_token *token = cp_lexer_peek_token (parser->lexer);
	  cp_lexer_set_source_position_from_token (token);
	  maybe_warn_cpp0x (CPP0X_INITIALIZER_LISTS);
	  index = cp_parser_braced_list (parser, &expr_nonconst_p);
	  if (flag_cilkplus
	      && cp_lexer_peek_token (parser->lexer)->type == CPP_COLON)
	    {
	      error_at (cp_lexer_peek_token (parser->lexer)->location,
			"braced list index is not allowed with array "
			"notation");
	      cp_parser_skip_to_end_of_statement (parser);
	      return error_mark_node;
	    }
	}
      else if (flag_cilkplus)
	{
	  /* Here are have these two options:
	     ARRAY[EXP : EXP]        - Array notation expr with default
	     stride of 1.
	     ARRAY[EXP : EXP : EXP] - Array Notation with user-defined
	     stride.  */
	  tree an_exp = cp_parser_array_notation (loc, parser, &index, 
						  postfix_expression);
	  if (an_exp)
	    return an_exp;
	}
      else
	index = cp_parser_expression (parser, /*cast_p=*/false, NULL);
    }

  parser->greater_than_is_operator_p = saved_greater_than_is_operator_p;

  /* Look for the closing `]'.  */
  cp_parser_require (parser, CPP_CLOSE_SQUARE, RT_CLOSE_SQUARE);

  /* Build the ARRAY_REF.  */
  postfix_expression = grok_array_decl (loc, postfix_expression,
					index, decltype_p);

  /* When not doing offsetof, array references are not permitted in
     constant-expressions.  */
  if (!for_offsetof
      && (cp_parser_non_integral_constant_expression (parser, NIC_ARRAY_REF)))
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
    postfix_expression = build_x_arrow (location, postfix_expression,
					tf_warning_or_error);
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
      /* Unlike the object expression in other contexts, *this is not
	 required to be of complete type for purposes of class member
	 access (5.2.5) outside the member function body.  */
      else if (postfix_expression != current_class_ref
	       && !(processing_template_decl && scope == current_class_type))
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
      cp_parser_pseudo_destructor_name (parser, postfix_expression,
					&s, &type);
      if (dependent_p
	  && (cp_parser_error_occurred (parser)
	      || !SCALAR_TYPE_P (type)))
	cp_parser_abort_tentative_parse (parser);
      else if (cp_parser_parse_definitely (parser))
	{
	  pseudo_destructor_p = true;
	  postfix_expression
	    = finish_pseudo_destructor_expr (postfix_expression,
					     s, type, location);
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
	      if (TREE_CODE (parser->scope) == NAMESPACE_DECL)
		{
		  error_at (token->location, "%<%D::%D%> is not a class member",
			    parser->scope, name);
		  postfix_expression = error_mark_node;
		}
	      else
		name = build_qualified_name (/*type=*/NULL_TREE,
					     parser->scope,
					     name,
					     template_p);
	      parser->scope = NULL_TREE;
	      parser->qualifying_scope = NULL_TREE;
	      parser->object_scope = NULL_TREE;
	    }
	  if (parser->scope && name && BASELINK_P (name))
	    adjust_result_of_qualified_name_lookup
	      (name, parser->scope, scope);
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
	  (parser, token_type == CPP_DEREF ? NIC_ARROW : NIC_POINT)))
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
   expressions.  The parentheses are eaten.  IS_ATTRIBUTE_LIST is id_attr
   if we are parsing an attribute list for an attribute that wants a
   plain identifier argument, normal_attr for an attribute that wants
   an expression, or non_attr if we aren't parsing an attribute list.  If
   NON_CONSTANT_P is non-NULL, *NON_CONSTANT_P indicates whether or
   not all of the expressions in the list were constant.  */

static vec<tree, va_gc> *
cp_parser_parenthesized_expression_list (cp_parser* parser,
					 int is_attribute_list,
					 bool cast_p,
                                         bool allow_expansion_p,
					 bool *non_constant_p)
{
  vec<tree, va_gc> *expression_list;
  bool fold_expr_p = is_attribute_list != non_attr;
  tree identifier = NULL_TREE;
  bool saved_greater_than_is_operator_p;

  /* Assume all the expressions will be constant.  */
  if (non_constant_p)
    *non_constant_p = false;

  if (!cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN))
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
	if (is_attribute_list == id_attr
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
		cp_token *token = cp_lexer_peek_token (parser->lexer);
		cp_lexer_set_source_position_from_token (token);
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
	    vec_safe_push (expression_list, expr);

	    if (expr == error_mark_node)
	      goto skip_comma;
	  }

	/* After the first item, attribute lists look the same as
	   expression lists.  */
	is_attribute_list = non_attr;

      get_comma:;
	/* If the next token isn't a `,', then we are done.  */
	if (cp_lexer_next_token_is_not (parser->lexer, CPP_COMMA))
	  break;

	/* Otherwise, consume the `,' and keep going.  */
	cp_lexer_consume_token (parser->lexer);
      }

  if (!cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN))
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
    vec_safe_insert (expression_list, 0, identifier);

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
				  tree object,
				  tree* scope,
				  tree* type)
{
  bool nested_name_specifier_p;

  /* Handle ~auto.  */
  if (cp_lexer_next_token_is (parser->lexer, CPP_COMPL)
      && cp_lexer_nth_token_is_keyword (parser->lexer, 2, RID_AUTO)
      && !type_dependent_expression_p (object))
    {
      if (cxx_dialect < cxx1y)
	pedwarn (input_location, 0,
		 "%<~auto%> only available with "
		 "-std=c++1y or -std=gnu++1y");
      cp_lexer_consume_token (parser->lexer);
      cp_lexer_consume_token (parser->lexer);
      *scope = NULL_TREE;
      *type = TREE_TYPE (object);
      return;
    }

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
			     class_type,
			     /*is_declaration=*/true);
      /* Look for the `::' token.  */
      cp_parser_require (parser, CPP_SCOPE, RT_SCOPE);
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
      cp_parser_require (parser, CPP_SCOPE, RT_SCOPE);
    }
  else
    *scope = NULL_TREE;

  /* Look for the `~'.  */
  cp_parser_require (parser, CPP_COMPL, RT_COMPL);

  /* Once we see the ~, this has to be a pseudo-destructor.  */
  if (!processing_template_decl && !cp_parser_error_occurred (parser))
    cp_parser_commit_to_topmost_tentative_parse (parser);

  /* Look for the type-name again.  We are not responsible for
     checking that it matches the first type-name.  */
  *type = TREE_TYPE (cp_parser_nonclass_name (parser));
}

/* Parse a unary-expression.

   unary-expression:
     postfix-expression
     ++ cast-expression
     -- cast-expression
     unary-operator cast-expression
     sizeof unary-expression
     sizeof ( type-id )
     alignof ( type-id )  [C++0x]
     new-expression
     delete-expression

   GNU Extensions:

   unary-expression:
     __extension__ cast-expression
     __alignof__ unary-expression
     __alignof__ ( type-id )
     alignof unary-expression  [C++0x]
     __real__ cast-expression
     __imag__ cast-expression
     && identifier
     sizeof ( type-id ) { initializer-list , [opt] }
     alignof ( type-id ) { initializer-list , [opt] } [C++0x]
     __alignof__ ( type-id ) { initializer-list , [opt] }

   ADDRESS_P is true iff the unary-expression is appearing as the
   operand of the `&' operator.   CAST_P is true if this expression is
   the target of a cast.

   Returns a representation of the expression.  */

static tree
cp_parser_unary_expression (cp_parser *parser, bool address_p, bool cast_p,
			    bool decltype_p, cp_id_kind * pidk)
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
	    tree operand, ret;
	    enum tree_code op;
	    location_t first_loc;

	    op = keyword == RID_ALIGNOF ? ALIGNOF_EXPR : SIZEOF_EXPR;
	    /* Consume the token.  */
	    cp_lexer_consume_token (parser->lexer);
	    first_loc = cp_lexer_peek_token (parser->lexer)->location;
	    /* Parse the operand.  */
	    operand = cp_parser_sizeof_operand (parser, keyword);

	    if (TYPE_P (operand))
	      ret = cxx_sizeof_or_alignof_type (operand, op, true);
	    else
	      {
		/* ISO C++ defines alignof only with types, not with
		   expressions. So pedwarn if alignof is used with a non-
		   type expression. However, __alignof__ is ok.  */
		if (!strcmp (IDENTIFIER_POINTER (token->u.value), "alignof"))
		  pedwarn (token->location, OPT_Wpedantic,
			   "ISO C++ does not allow %<alignof%> "
			   "with a non-type");

		ret = cxx_sizeof_or_alignof_expr (operand, op, true);
	      }
	    /* For SIZEOF_EXPR, just issue diagnostics, but keep
	       SIZEOF_EXPR with the original operand.  */
	    if (op == SIZEOF_EXPR && ret != error_mark_node)
	      {
		if (TREE_CODE (ret) != SIZEOF_EXPR || TYPE_P (operand))
		  {
		    if (!processing_template_decl && TYPE_P (operand))
		      {
			ret = build_min (SIZEOF_EXPR, size_type_node,
					 build1 (NOP_EXPR, operand,
						 error_mark_node));
			SIZEOF_EXPR_TYPE_P (ret) = 1;
		      }
		    else
		      ret = build_min (SIZEOF_EXPR, size_type_node, operand);
		    TREE_SIDE_EFFECTS (ret) = 0;
		    TREE_READONLY (ret) = 1;
		  }
		SET_EXPR_LOCATION (ret, first_loc);
	      }
	    return ret;
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
	    return build_x_unary_op (token->location,
				     (keyword == RID_REALPART
				      ? REALPART_EXPR : IMAGPART_EXPR),
				     expression,
                                     tf_warning_or_error);
	  }
	  break;

	case RID_TRANSACTION_ATOMIC:
	case RID_TRANSACTION_RELAXED:
	  return cp_parser_transaction_expression (parser, keyword);

	case RID_NOEXCEPT:
	  {
	    tree expr;
	    const char *saved_message;
	    bool saved_integral_constant_expression_p;
	    bool saved_non_integral_constant_expression_p;
	    bool saved_greater_than_is_operator_p;

	    cp_lexer_consume_token (parser->lexer);
	    cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN);

	    saved_message = parser->type_definition_forbidden_message;
	    parser->type_definition_forbidden_message
	      = G_("types may not be defined in %<noexcept%> expressions");

	    saved_integral_constant_expression_p
	      = parser->integral_constant_expression_p;
	    saved_non_integral_constant_expression_p
	      = parser->non_integral_constant_expression_p;
	    parser->integral_constant_expression_p = false;

	    saved_greater_than_is_operator_p
	      = parser->greater_than_is_operator_p;
	    parser->greater_than_is_operator_p = true;

	    ++cp_unevaluated_operand;
	    ++c_inhibit_evaluation_warnings;
	    expr = cp_parser_expression (parser, false, NULL);
	    --c_inhibit_evaluation_warnings;
	    --cp_unevaluated_operand;

	    parser->greater_than_is_operator_p
	      = saved_greater_than_is_operator_p;

	    parser->integral_constant_expression_p
	      = saved_integral_constant_expression_p;
	    parser->non_integral_constant_expression_p
	      = saved_non_integral_constant_expression_p;

	    parser->type_definition_forbidden_message = saved_message;

	    cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN);
	    return finish_noexcept_expr (expr, tf_warning_or_error);
	  }

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
	  location_t loc = token->location;

	  /* Consume the '&&' token.  */
	  cp_lexer_consume_token (parser->lexer);
	  /* Look for the identifier.  */
	  identifier = cp_parser_identifier (parser);
	  /* Create an expression representing the address.  */
	  expression = finish_label_address_expr (identifier, loc);
	  if (cp_parser_non_integral_constant_expression (parser,
							  NIC_ADDR_LABEL))
	    expression = error_mark_node;
	  return expression;
	}
    }
  if (unary_operator != ERROR_MARK)
    {
      tree cast_expression;
      tree expression = error_mark_node;
      non_integral_constant non_constant_p = NIC_NONE;
      location_t loc = token->location;
      tsubst_flags_t complain = complain_flags (decltype_p);

      /* Consume the operator token.  */
      token = cp_lexer_consume_token (parser->lexer);
      /* Parse the cast-expression.  */
      cast_expression
	= cp_parser_cast_expression (parser,
				     unary_operator == ADDR_EXPR,
				     /*cast_p=*/false,
				     /*decltype*/false,
				     pidk);
      /* Now, build an appropriate representation.  */
      switch (unary_operator)
	{
	case INDIRECT_REF:
	  non_constant_p = NIC_STAR;
	  expression = build_x_indirect_ref (loc, cast_expression,
					     RO_UNARY_STAR,
                                             complain);
	  break;

	case ADDR_EXPR:
	   non_constant_p = NIC_ADDR;
	  /* Fall through.  */
	case BIT_NOT_EXPR:
	  expression = build_x_unary_op (loc, unary_operator,
					 cast_expression,
                                         complain);
	  break;

	case PREINCREMENT_EXPR:
	case PREDECREMENT_EXPR:
	  non_constant_p = unary_operator == PREINCREMENT_EXPR
			   ? NIC_PREINCREMENT : NIC_PREDECREMENT;
	  /* Fall through.  */
	case UNARY_PLUS_EXPR:
	case NEGATE_EXPR:
	case TRUTH_NOT_EXPR:
	  expression = finish_unary_op_expr (loc, unary_operator,
					     cast_expression, complain);
	  break;

	default:
	  gcc_unreachable ();
	}

      if (non_constant_p != NIC_NONE
	  && cp_parser_non_integral_constant_expression (parser,
							 non_constant_p))
	expression = error_mark_node;

      return expression;
    }

  return cp_parser_postfix_expression (parser, address_p, cast_p,
                                       /*member_access_only_p=*/false,
				       decltype_p,
				       pidk);
}

static inline tree
cp_parser_unary_expression (cp_parser *parser, bool address_p, bool cast_p,
			    cp_id_kind * pidk)
{
  return cp_parser_unary_expression (parser, address_p, cast_p,
				     /*decltype*/false, pidk);
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
  vec<tree, va_gc> *placement;
  tree type;
  vec<tree, va_gc> *initializer;
  tree nelts = NULL_TREE;
  tree ret;

  /* Look for the optional `::' operator.  */
  global_scope_p
    = (cp_parser_global_scope_opt (parser,
				   /*current_scope_valid_p=*/false)
       != NULL_TREE);
  /* Look for the `new' operator.  */
  cp_parser_require_keyword (parser, RID_NEW, RT_NEW);
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
      const char *saved_message = parser->type_definition_forbidden_message;

      /* Consume the `('.  */
      cp_lexer_consume_token (parser->lexer);

      /* Parse the type-id.  */
      parser->type_definition_forbidden_message
	= G_("types may not be defined in a new-expression");
      type = cp_parser_type_id (parser);
      parser->type_definition_forbidden_message = saved_message;

      /* Look for the closing `)'.  */
      cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN);
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
  if (cp_parser_non_integral_constant_expression (parser, NIC_NEW))
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

static vec<tree, va_gc> *
cp_parser_new_placement (cp_parser* parser)
{
  vec<tree, va_gc> *expression_list;

  /* Parse the expression-list.  */
  expression_list = (cp_parser_parenthesized_expression_list
		     (parser, non_attr, /*cast_p=*/false,
		      /*allow_expansion_p=*/true,
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

  if (type_specifier_seq.type == error_mark_node)
    return error_mark_node;

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

  return groktypename (&type_specifier_seq, new_declarator, false);
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
  tree type, std_attributes = NULL_TREE;
  cp_cv_quals cv_quals;  

  /* We don't know if there's a ptr-operator next, or not.  */
  cp_parser_parse_tentatively (parser);
  /* Look for a ptr-operator.  */
  code = cp_parser_ptr_operator (parser, &type, &cv_quals, &std_attributes);
  /* If that worked, look for more new-declarators.  */
  if (cp_parser_parse_definitely (parser))
    {
      cp_declarator *declarator;

      /* Parse another optional declarator.  */
      declarator = cp_parser_new_declarator_opt (parser);

      declarator = cp_parser_make_indirect_declarator
	(code, type, cv_quals, declarator, std_attributes);

      return declarator;
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
      cp_token *token;

      /* Look for the opening `['.  */
      cp_parser_require (parser, CPP_OPEN_SQUARE, RT_OPEN_SQUARE);

      token = cp_lexer_peek_token (parser->lexer);
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

      /* Look for the closing `]'.  */
      cp_parser_require (parser, CPP_CLOSE_SQUARE, RT_CLOSE_SQUARE);

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

static vec<tree, va_gc> *
cp_parser_new_initializer (cp_parser* parser)
{
  vec<tree, va_gc> *expression_list;

  if (cp_lexer_next_token_is (parser->lexer, CPP_OPEN_BRACE))
    {
      tree t;
      bool expr_non_constant_p;
      cp_token *token = cp_lexer_peek_token (parser->lexer);
      cp_lexer_set_source_position_from_token (token);
      maybe_warn_cpp0x (CPP0X_INITIALIZER_LISTS);
      t = cp_parser_braced_list (parser, &expr_non_constant_p);
      CONSTRUCTOR_IS_DIRECT_INIT (t) = 1;
      expression_list = make_tree_vector_single (t);
    }
  else
    expression_list = (cp_parser_parenthesized_expression_list
		       (parser, non_attr, /*cast_p=*/false,
			/*allow_expansion_p=*/true,
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
  cp_parser_require_keyword (parser, RID_DELETE, RT_DELETE);
  /* See if the array syntax is in use.  */
  if (cp_lexer_next_token_is (parser->lexer, CPP_OPEN_SQUARE))
    {
      /* Consume the `[' token.  */
      cp_lexer_consume_token (parser->lexer);
      /* Look for the `]' token.  */
      cp_parser_require (parser, CPP_CLOSE_SQUARE, RT_CLOSE_SQUARE);
      /* Remember that this is the `[]' construct.  */
      array_p = true;
    }
  else
    array_p = false;

  /* Parse the cast-expression.  */
  expression = cp_parser_simple_cast_expression (parser);

  /* A delete-expression may not appear in an integral constant
     expression.  */
  if (cp_parser_non_integral_constant_expression (parser, NIC_DEL))
    return error_mark_node;

  return delete_sanity (expression, NULL_TREE, array_p, global_scope_p,
			tf_warning_or_error);
}

/* Returns true if TOKEN may start a cast-expression and false
   otherwise.  */

static bool
cp_parser_tokens_start_cast_expression (cp_parser *parser)
{
  cp_token *token = cp_lexer_peek_token (parser->lexer);
  switch (token->type)
    {
    case CPP_COMMA:
    case CPP_SEMICOLON:
    case CPP_QUERY:
    case CPP_COLON:
    case CPP_CLOSE_SQUARE:
    case CPP_CLOSE_PAREN:
    case CPP_CLOSE_BRACE:
    case CPP_OPEN_BRACE:
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

    case CPP_OPEN_PAREN:
      /* In ((type ()) () the last () isn't a valid cast-expression,
	 so the whole must be parsed as postfix-expression.  */
      return cp_lexer_peek_nth_token (parser->lexer, 2)->type
	     != CPP_CLOSE_PAREN;

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
			   bool decltype_p, cp_id_kind * pidk)
{
  /* If it's a `(', then we might be looking at a cast.  */
  if (cp_lexer_next_token_is (parser->lexer, CPP_OPEN_PAREN))
    {
      tree type = NULL_TREE;
      tree expr = NULL_TREE;
      bool cast_expression_p;
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
	 Another tricky case is the following (c++/29234):

         struct S { void operator () (); };

         void foo ()
         {
           ( S()() );
         }

	 As a type-id we parse the parenthesized S()() as a function
	 returning a function, groktypename complains and we cannot
	 back up in this case either.

	 Therefore, we scan ahead to the closing `)', and check to see
	 if the tokens after the `)' can start a cast-expression.  Otherwise
	 we are dealing with an unary-expression, a postfix-expression
	 or something else.

	 Save tokens so that we can put them back.  */
      cp_lexer_save_tokens (parser->lexer);

      /* We may be looking at a cast-expression.  */
      cast_expression_p
	= (cp_parser_skip_to_closing_parenthesis (parser, false, false,
						  /*consume_paren=*/true)
	   && cp_parser_tokens_start_cast_expression (parser));

      /* Roll back the tokens we skipped.  */
      cp_lexer_rollback_tokens (parser->lexer);
      /* If we aren't looking at a cast-expression, simulate an error so
	 that the call to cp_parser_parse_definitely below will fail.  */
      if (!cast_expression_p)
	cp_parser_simulate_error (parser);
      else
	{
	  bool saved_in_type_id_in_expr_p = parser->in_type_id_in_expr_p;
	  parser->in_type_id_in_expr_p = true;
	  /* Look for the type-id.  */
	  type = cp_parser_type_id (parser);
	  /* Look for the closing `)'.  */
	  cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN);
	  parser->in_type_id_in_expr_p = saved_in_type_id_in_expr_p;
	}

      /* Restore the saved message.  */
      parser->type_definition_forbidden_message = saved_message;

      /* At this point this can only be either a cast or a
	 parenthesized ctor such as `(T ())' that looks like a cast to
	 function returning T.  */
      if (!cp_parser_error_occurred (parser))
	{
	  cp_parser_parse_definitely (parser);
	  expr = cp_parser_cast_expression (parser,
					    /*address_p=*/false,
					    /*cast_p=*/true,
					    /*decltype_p=*/false,
					    pidk);

	  /* Warn about old-style casts, if so requested.  */
	  if (warn_old_style_cast
	      && !in_system_header_at (input_location)
	      && !VOID_TYPE_P (type)
	      && current_lang_name != lang_name_c)
	    warning (OPT_Wold_style_cast, "use of old-style cast");

	  /* Only type conversions to integral or enumeration types
	     can be used in constant-expressions.  */
	  if (!cast_valid_in_integral_constant_expression_p (type)
	      && cp_parser_non_integral_constant_expression (parser,
							     NIC_CAST))
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
  return cp_parser_unary_expression (parser, address_p, cast_p,
				     decltype_p, pidk);
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
			     bool decltype_p,
			     enum cp_parser_prec prec,
			     cp_id_kind * pidk)
{
  cp_parser_expression_stack stack;
  cp_parser_expression_stack_entry *sp = &stack[0];
  cp_parser_expression_stack_entry current;
  tree rhs;
  cp_token *token;
  enum tree_code rhs_type;
  enum cp_parser_prec new_prec, lookahead_prec;
  tree overload;

  /* Parse the first expression.  */
  current.lhs = cp_parser_cast_expression (parser, /*address_p=*/false,
					   cast_p, decltype_p, pidk);
  current.lhs_type = ERROR_MARK;
  current.prec = prec;

  if (cp_parser_error_occurred (parser))
    return error_mark_node;

  for (;;)
    {
      /* Get an operator token.  */
      token = cp_lexer_peek_token (parser->lexer);

      if (warn_cxx0x_compat
          && token->type == CPP_RSHIFT
          && !parser->greater_than_is_operator_p)
        {
          if (warning_at (token->location, OPT_Wc__0x_compat,
			  "%<>>%> operator is treated"
			  " as two right angle brackets in C++11"))
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
      if (new_prec <= current.prec)
	{
	  if (sp == stack)
	    break;
	  else
	    goto pop;
	}

     get_rhs:
      current.tree_type = binops_by_token[token->type].tree_type;
      current.loc = token->location;

      /* We used the operator token.  */
      cp_lexer_consume_token (parser->lexer);

      /* For "false && x" or "true || x", x will never be executed;
	 disable warnings while evaluating it.  */
      if (current.tree_type == TRUTH_ANDIF_EXPR)
	c_inhibit_evaluation_warnings += current.lhs == truthvalue_false_node;
      else if (current.tree_type == TRUTH_ORIF_EXPR)
	c_inhibit_evaluation_warnings += current.lhs == truthvalue_true_node;

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
	  *sp = current;
	  ++sp;
	  current.lhs = rhs;
	  current.lhs_type = rhs_type;
	  current.prec = new_prec;
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
	  rhs = current.lhs;
	  rhs_type = current.lhs_type;
	  --sp;
	  current = *sp;
	}

      /* Undo the disabling of warnings done above.  */
      if (current.tree_type == TRUTH_ANDIF_EXPR)
	c_inhibit_evaluation_warnings -= current.lhs == truthvalue_false_node;
      else if (current.tree_type == TRUTH_ORIF_EXPR)
	c_inhibit_evaluation_warnings -= current.lhs == truthvalue_true_node;

      overload = NULL;
      /* ??? Currently we pass lhs_type == ERROR_MARK and rhs_type ==
	 ERROR_MARK for everything that is not a binary expression.
	 This makes warn_about_parentheses miss some warnings that
	 involve unary operators.  For unary expressions we should
	 pass the correct tree_code unless the unary expression was
	 surrounded by parentheses.
      */
      if (no_toplevel_fold_p
	  && lookahead_prec <= current.prec
	  && sp == stack)
	current.lhs = build2 (current.tree_type,
			      TREE_CODE_CLASS (current.tree_type)
			      == tcc_comparison
			      ? boolean_type_node : TREE_TYPE (current.lhs),
			      current.lhs, rhs);
      else
	current.lhs = build_x_binary_op (current.loc, current.tree_type,
					 current.lhs, current.lhs_type,
					 rhs, rhs_type, &overload,
					 complain_flags (decltype_p));
      current.lhs_type = current.tree_type;
      if (EXPR_P (current.lhs))
	SET_EXPR_LOCATION (current.lhs, current.loc);

      /* If the binary operator required the use of an overloaded operator,
	 then this expression cannot be an integral constant-expression.
	 An overloaded operator can be used even if both operands are
	 otherwise permissible in an integral constant-expression if at
	 least one of the operands is of enumeration type.  */

      if (overload
	  && cp_parser_non_integral_constant_expression (parser,
							 NIC_OVERLOADED))
	return error_mark_node;
    }

  return current.lhs;
}

static tree
cp_parser_binary_expression (cp_parser* parser, bool cast_p,
			     bool no_toplevel_fold_p,
			     enum cp_parser_prec prec,
			     cp_id_kind * pidk)
{
  return cp_parser_binary_expression (parser, cast_p, no_toplevel_fold_p,
				      /*decltype*/false, prec, pidk);
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
  struct cp_token *token;
  location_t loc = cp_lexer_peek_token (parser->lexer)->location;

  /* Consume the `?' token.  */
  cp_lexer_consume_token (parser->lexer);
  token = cp_lexer_peek_token (parser->lexer);
  if (cp_parser_allow_gnu_extensions_p (parser)
      && token->type == CPP_COLON)
    {
      pedwarn (token->location, OPT_Wpedantic, 
               "ISO C++ does not allow ?: with omitted middle operand");
      /* Implicit true clause.  */
      expr = NULL_TREE;
      c_inhibit_evaluation_warnings += logical_or_expr == truthvalue_true_node;
      warn_for_omitted_condop (token->location, logical_or_expr);
    }
  else
    {
      bool saved_colon_corrects_to_scope_p = parser->colon_corrects_to_scope_p;
      parser->colon_corrects_to_scope_p = false;
      /* Parse the expression.  */
      c_inhibit_evaluation_warnings += logical_or_expr == truthvalue_false_node;
      expr = cp_parser_expression (parser, /*cast_p=*/false, NULL);
      c_inhibit_evaluation_warnings +=
	((logical_or_expr == truthvalue_true_node)
	 - (logical_or_expr == truthvalue_false_node));
      parser->colon_corrects_to_scope_p = saved_colon_corrects_to_scope_p;
    }

  /* The next token should be a `:'.  */
  cp_parser_require (parser, CPP_COLON, RT_COLON);
  /* Parse the assignment-expression.  */
  assignment_expr = cp_parser_assignment_expression (parser, /*cast_p=*/false, NULL);
  c_inhibit_evaluation_warnings -= logical_or_expr == truthvalue_true_node;

  /* Build the conditional-expression.  */
  return build_x_conditional_expr (loc, logical_or_expr,
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
   DECLTYPE_P is true if this expression is the operand of decltype.

   Returns a representation for the expression.  */

static tree
cp_parser_assignment_expression (cp_parser* parser, bool cast_p,
				 bool decltype_p, cp_id_kind * pidk)
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
					  decltype_p,
					  PREC_NOT_OPERATOR, pidk);
      /* If the next token is a `?' then we're actually looking at a
	 conditional-expression.  */
      if (cp_lexer_next_token_is (parser->lexer, CPP_QUERY))
	return cp_parser_question_colon_clause (parser, expr);
      else
	{
	  location_t loc = cp_lexer_peek_token (parser->lexer)->location;

	  /* If it's an assignment-operator, we're using the second
	     production.  */
	  enum tree_code assignment_operator
	    = cp_parser_assignment_operator_opt (parser);
	  if (assignment_operator != ERROR_MARK)
	    {
	      bool non_constant_p;
	      location_t saved_input_location;

	      /* Parse the right-hand side of the assignment.  */
	      tree rhs = cp_parser_initializer_clause (parser, &non_constant_p);

	      if (BRACE_ENCLOSED_INITIALIZER_P (rhs))
		maybe_warn_cpp0x (CPP0X_INITIALIZER_LISTS);

	      /* An assignment may not appear in a
		 constant-expression.  */
	      if (cp_parser_non_integral_constant_expression (parser,
							      NIC_ASSIGNMENT))
		return error_mark_node;
	      /* Build the assignment expression.  Its default
		 location is the location of the '=' token.  */
	      saved_input_location = input_location;
	      input_location = loc;
	      expr = build_x_modify_expr (loc, expr,
					  assignment_operator,
					  rhs,
					  complain_flags (decltype_p));
	      input_location = saved_input_location;
	    }
	}
    }

  return expr;
}

static tree
cp_parser_assignment_expression (cp_parser* parser, bool cast_p,
				 cp_id_kind * pidk)
{
  return cp_parser_assignment_expression (parser, cast_p,
					  /*decltype*/false, pidk);
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
   DECLTYPE_P is true if this expression is the immediate operand of decltype,
     except possibly parenthesized or on the RHS of a comma (N3276).

   Returns a representation of the expression.  */

static tree
cp_parser_expression (cp_parser* parser, bool cast_p, bool decltype_p,
		      cp_id_kind * pidk)
{
  tree expression = NULL_TREE;
  location_t loc = UNKNOWN_LOCATION;

  while (true)
    {
      tree assignment_expression;

      /* Parse the next assignment-expression.  */
      assignment_expression
	= cp_parser_assignment_expression (parser, cast_p, decltype_p, pidk);

      /* We don't create a temporary for a call that is the immediate operand
	 of decltype or on the RHS of a comma.  But when we see a comma, we
	 need to create a temporary for a call on the LHS.  */
      if (decltype_p && !processing_template_decl
	  && TREE_CODE (assignment_expression) == CALL_EXPR
	  && CLASS_TYPE_P (TREE_TYPE (assignment_expression))
	  && cp_lexer_next_token_is (parser->lexer, CPP_COMMA))
	assignment_expression
	  = build_cplus_new (TREE_TYPE (assignment_expression),
			     assignment_expression, tf_warning_or_error);

      /* If this is the first assignment-expression, we can just
	 save it away.  */
      if (!expression)
	expression = assignment_expression;
      else
	expression = build_x_compound_expr (loc, expression,
					    assignment_expression,
					    complain_flags (decltype_p));
      /* If the next token is not a comma, then we are done with the
	 expression.  */
      if (cp_lexer_next_token_is_not (parser->lexer, CPP_COMMA))
	break;
      /* Consume the `,'.  */
      loc = cp_lexer_peek_token (parser->lexer)->location;
      cp_lexer_consume_token (parser->lexer);
      /* A comma operator cannot appear in a constant-expression.  */
      if (cp_parser_non_integral_constant_expression (parser, NIC_COMMA))
	expression = error_mark_node;
    }

  return expression;
}

static inline tree
cp_parser_expression (cp_parser* parser, bool cast_p, cp_id_kind * pidk)
{
  return cp_parser_expression (parser, cast_p, /*decltype*/false, pidk);
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
  parser->allow_non_integral_constant_expression_p
    = (allow_non_constant_p || cxx_dialect >= cxx11);
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
  if (cxx_dialect >= cxx11)
    {
      /* Require an rvalue constant expression here; that's what our
	 callers expect.  Reference constant expressions are handled
	 separately in e.g. cp_parser_template_argument.  */
      bool is_const = potential_rvalue_constant_expression (expression);
      parser->non_integral_constant_expression_p = !is_const;
      if (!is_const && !allow_non_constant_p)
	require_potential_rvalue_constant_expression (expression);
    }
  if (allow_non_constant_p)
    *non_constant_p = parser->non_integral_constant_expression_p;
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
  cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN);
  /* Parse the type-id.  */
  type = cp_parser_type_id (parser);
  /* Look for the `,'.  */
  cp_parser_require (parser, CPP_COMMA, RT_COMMA);
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
	  expr = cp_parser_postfix_open_square_expression (parser, expr,
							   true, false);
	  break;

	case CPP_DEREF:
	  /* offsetof-member-designator "->" identifier */
	  expr = grok_array_decl (token->location, expr,
				  integer_zero_node, false);
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
	  cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN);
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

/* Parse a trait expression.

   Returns a representation of the expression, the underlying type
   of the type at issue when KEYWORD is RID_UNDERLYING_TYPE.  */

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
    case RID_IS_FINAL:
      kind = CPTK_IS_FINAL;
      break;
    case RID_IS_LITERAL_TYPE:
      kind = CPTK_IS_LITERAL_TYPE;
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
    case RID_UNDERLYING_TYPE:
      kind = CPTK_UNDERLYING_TYPE;
      break;
    case RID_BASES:
      kind = CPTK_BASES;
      break;
    case RID_DIRECT_BASES:
      kind = CPTK_DIRECT_BASES;
      break;
    default:
      gcc_unreachable ();
    }

  /* Consume the token.  */
  cp_lexer_consume_token (parser->lexer);

  cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN);

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
      cp_parser_require (parser, CPP_COMMA, RT_COMMA);
 
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

  cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN);

  /* Complete the trait expression, which may mean either processing
     the trait expr now or saving it for template instantiation.  */
  switch(kind)
    {
    case CPTK_UNDERLYING_TYPE:
      return finish_underlying_type (type1);
    case CPTK_BASES:
      return finish_bases (type1, false);
    case CPTK_DIRECT_BASES:
      return finish_bases (type1, true);
    default:
      return finish_trait_expr (kind, type1, type2);
    }
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
static GTY(()) vec<tree_int, va_gc> *lambda_scope_stack;

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
  vec_safe_push (lambda_scope_stack, ti);
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
  tree_int *p = &lambda_scope_stack->last ();
  if (lambda_scope != p->t)
    {
      lambda_scope = p->t;
      lambda_count = p->i;
    }
  lambda_scope_stack->pop ();
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
  bool ok;

  LAMBDA_EXPR_LOCATION (lambda_expr)
    = cp_lexer_peek_token (parser->lexer)->location;

  if (cp_unevaluated_operand)
    error_at (LAMBDA_EXPR_LOCATION (lambda_expr),
	      "lambda-expression in unevaluated context");

  /* We may be in the middle of deferred access check.  Disable
     it now.  */
  push_deferring_access_checks (dk_no_deferred);

  cp_parser_lambda_introducer (parser, lambda_expr);

  type = begin_lambda_type (lambda_expr);
  if (type == error_mark_node)
    return error_mark_node;

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
    unsigned char in_statement = parser->in_statement;
    bool in_switch_statement_p = parser->in_switch_statement_p;
    bool fully_implicit_function_template_p
        = parser->fully_implicit_function_template_p;
    tree implicit_template_parms = parser->implicit_template_parms;
    cp_binding_level* implicit_template_scope = parser->implicit_template_scope;
    bool auto_is_implicit_function_template_parm_p
        = parser->auto_is_implicit_function_template_parm_p;

    parser->num_template_parameter_lists = 0;
    parser->in_statement = 0;
    parser->in_switch_statement_p = false;
    parser->fully_implicit_function_template_p = false;
    parser->implicit_template_parms = 0;
    parser->implicit_template_scope = 0;
    parser->auto_is_implicit_function_template_parm_p = false;

    /* By virtue of defining a local class, a lambda expression has access to
       the private variables of enclosing classes.  */

    ok = cp_parser_lambda_declarator_opt (parser, lambda_expr);

    if (ok)
      cp_parser_lambda_body (parser, lambda_expr);
    else if (cp_parser_require (parser, CPP_OPEN_BRACE, RT_OPEN_BRACE))
      cp_parser_skip_to_end_of_block_or_statement (parser);

    /* The capture list was built up in reverse order; fix that now.  */
    LAMBDA_EXPR_CAPTURE_LIST (lambda_expr)
      = nreverse (LAMBDA_EXPR_CAPTURE_LIST (lambda_expr));

    if (ok)
      maybe_add_lambda_conv_op (type);

    type = finish_struct (type, /*attributes=*/NULL_TREE);

    parser->num_template_parameter_lists = saved_num_template_parameter_lists;
    parser->in_statement = in_statement;
    parser->in_switch_statement_p = in_switch_statement_p;
    parser->fully_implicit_function_template_p
	= fully_implicit_function_template_p;
    parser->implicit_template_parms = implicit_template_parms;
    parser->implicit_template_scope = implicit_template_scope;
    parser->auto_is_implicit_function_template_parm_p
	= auto_is_implicit_function_template_parm_p;
  }

  pop_deferring_access_checks ();

  /* This field is only used during parsing of the lambda.  */
  LAMBDA_EXPR_THIS_CAPTURE (lambda_expr) = NULL_TREE;

  /* This lambda shouldn't have any proxies left at this point.  */
  gcc_assert (LAMBDA_EXPR_PENDING_PROXIES (lambda_expr) == NULL);
  /* And now that we're done, push proxies for an enclosing lambda.  */
  insert_pending_capture_proxies ();

  if (ok)
    return build_lambda_object (lambda_expr);
  else
    return error_mark_node;
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
  cp_parser_require (parser, CPP_OPEN_SQUARE, RT_OPEN_SQUARE);

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
	cp_parser_require (parser, CPP_COMMA, RT_COMMA);

      /* Possibly capture `this'.  */
      if (cp_lexer_next_token_is_keyword (parser->lexer, RID_THIS))
	{
	  location_t loc = cp_lexer_peek_token (parser->lexer)->location;
	  if (LAMBDA_EXPR_DEFAULT_CAPTURE_MODE (lambda_expr) == CPLD_COPY)
	    pedwarn (loc, 0, "explicit by-copy capture of %<this%> redundant "
		     "with by-copy capture default");
	  cp_lexer_consume_token (parser->lexer);
	  add_capture (lambda_expr,
		       /*id=*/this_identifier,
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
      if (cp_lexer_next_token_is (parser->lexer, CPP_EQ)
	  || cp_lexer_next_token_is (parser->lexer, CPP_OPEN_PAREN)
	  || cp_lexer_next_token_is (parser->lexer, CPP_OPEN_BRACE))
	{
	  bool direct, non_constant;
	  /* An explicit initializer exists.  */
	  if (cxx_dialect < cxx1y)
	    pedwarn (input_location, 0,
		     "lambda capture initializers "
		     "only available with -std=c++1y or -std=gnu++1y");
	  capture_init_expr = cp_parser_initializer (parser, &direct,
						     &non_constant);
	  explicit_init_p = true;
	  if (capture_init_expr == NULL_TREE)
	    {
	      error ("empty initializer for lambda init-capture");
	      capture_init_expr = error_mark_node;
	    }
	}
      else
	{
	  const char* error_msg;

	  /* Turn the identifier into an id-expression.  */
	  capture_init_expr
	    = cp_parser_lookup_name_simple (parser, capture_id,
					    capture_token->location);

	  if (capture_init_expr == error_mark_node)
	    {
	      unqualified_name_lookup_error (capture_id);
	      continue;
	    }
	  else if (DECL_P (capture_init_expr)
		   && (!VAR_P (capture_init_expr)
		       && TREE_CODE (capture_init_expr) != PARM_DECL))
	    {
	      error_at (capture_token->location,
			"capture of non-variable %qD ",
			capture_init_expr);
	      inform (0, "%q+#D declared here", capture_init_expr);
	      continue;
	    }
	  if (VAR_P (capture_init_expr)
	      && decl_storage_duration (capture_init_expr) != dk_auto)
	    {
	      pedwarn (capture_token->location, 0, "capture of variable "
		       "%qD with non-automatic storage duration",
		       capture_init_expr);
	      inform (0, "%q+#D declared here", capture_init_expr);
	      continue;
	    }

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

	  if (cp_lexer_next_token_is (parser->lexer, CPP_ELLIPSIS))
	    {
	      cp_lexer_consume_token (parser->lexer);
	      capture_init_expr = make_pack_expansion (capture_init_expr);
	    }
	  else
	    check_for_bare_parameter_packs (capture_init_expr);
	}

      if (LAMBDA_EXPR_DEFAULT_CAPTURE_MODE (lambda_expr) != CPLD_NONE
	  && !explicit_init_p)
	{
	  if (LAMBDA_EXPR_DEFAULT_CAPTURE_MODE (lambda_expr) == CPLD_COPY
	      && capture_kind == BY_COPY)
	    pedwarn (capture_token->location, 0, "explicit by-copy capture "
		     "of %qD redundant with by-copy capture default",
		     capture_id);
	  if (LAMBDA_EXPR_DEFAULT_CAPTURE_MODE (lambda_expr) == CPLD_REFERENCE
	      && capture_kind == BY_REFERENCE)
	    pedwarn (capture_token->location, 0, "explicit by-reference "
		     "capture of %qD redundant with by-reference capture "
		     "default", capture_id);
	}

      add_capture (lambda_expr,
		   capture_id,
		   capture_init_expr,
		   /*by_reference_p=*/capture_kind == BY_REFERENCE,
		   explicit_init_p);
    }

  cp_parser_require (parser, CPP_CLOSE_SQUARE, RT_CLOSE_SQUARE);
}

/* Parse the (optional) middle of a lambda expression.

   lambda-declarator:
     < template-parameter-list [opt] >
     ( parameter-declaration-clause [opt] )
       attribute-specifier [opt]
       mutable [opt]
       exception-specification [opt]
       lambda-return-type-clause [opt]

   LAMBDA_EXPR is the current representation of the lambda expression.  */

static bool
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
  tree template_param_list = NULL_TREE;

  /* The template-parameter-list is optional, but must begin with
     an opening angle if present.  */
  if (cp_lexer_next_token_is (parser->lexer, CPP_LESS))
    {
      if (cxx_dialect < cxx1y)
	pedwarn (parser->lexer->next_token->location, 0,
		 "lambda templates are only available with "
		 "-std=c++1y or -std=gnu++1y");

      cp_lexer_consume_token (parser->lexer);

      template_param_list = cp_parser_template_parameter_list (parser);

      cp_parser_skip_to_end_of_template_parameter_list (parser);

      /* We just processed one more parameter list.  */
      ++parser->num_template_parameter_lists;
    }

  /* The parameter-declaration-clause is optional (unless
     template-parameter-list was given), but must begin with an
     opening parenthesis if present.  */
  if (cp_lexer_next_token_is (parser->lexer, CPP_OPEN_PAREN))
    {
      cp_lexer_consume_token (parser->lexer);

      begin_scope (sk_function_parms, /*entity=*/NULL_TREE);

      /* Parse parameters.  */
      param_list = cp_parser_parameter_declaration_clause (parser);

      /* Default arguments shall not be specified in the
	 parameter-declaration-clause of a lambda-declarator.  */
      for (tree t = param_list; t; t = TREE_CHAIN (t))
	if (TREE_PURPOSE (t))
	  pedwarn (DECL_SOURCE_LOCATION (TREE_VALUE (t)), OPT_Wpedantic,
		   "default argument specified for lambda parameter");

      cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN);

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
          LAMBDA_EXPR_RETURN_TYPE (lambda_expr)
	    = cp_parser_trailing_type_id (parser);
        }

      /* The function parameters must be in scope all the way until after the
         trailing-return-type in case of decltype.  */
      pop_bindings_and_leave_scope ();
    }
  else if (template_param_list != NULL_TREE) // generate diagnostic
    cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN);

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
      /* Maybe we will deduce the return type later.  */
      return_type_specs.type = make_auto ();

    p = obstack_alloc (&declarator_obstack, 0);

    declarator = make_id_declarator (NULL_TREE, ansi_opname (CALL_EXPR),
				     sfk_none);

    quals = (LAMBDA_EXPR_MUTABLE_P (lambda_expr)
	     ? TYPE_UNQUALIFIED : TYPE_QUAL_CONST);
    declarator = make_call_declarator (declarator, param_list, quals,
				       VIRT_SPEC_UNSPECIFIED,
                                       REF_QUAL_NONE,
				       exception_spec,
                                       /*late_return_type=*/NULL_TREE);
    declarator->id_loc = LAMBDA_EXPR_LOCATION (lambda_expr);

    fco = grokmethod (&return_type_specs,
		      declarator,
		      attributes);
    if (fco != error_mark_node)
      {
	DECL_INITIALIZED_IN_CLASS_P (fco) = 1;
	DECL_ARTIFICIAL (fco) = 1;
	/* Give the object parameter a different name.  */
	DECL_NAME (DECL_ARGUMENTS (fco)) = get_identifier ("__closure");
      }
    if (template_param_list)
      {
	fco = finish_member_template_decl (fco);
	finish_template_decl (template_param_list);
	--parser->num_template_parameter_lists;
      }
    else if (parser->fully_implicit_function_template_p)
      fco = finish_fully_implicit_template (parser, fco);

    finish_member_declaration (fco);

    obstack_free (&declarator_obstack, p);

    return (fco != error_mark_node);
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
  bool local_variables_forbidden_p = parser->local_variables_forbidden_p;
  if (nested)
    push_function_context ();
  else
    /* Still increment function_depth so that we don't GC in the
       middle of an expression.  */
    ++function_depth;
  /* Clear this in case we're in the middle of a default argument.  */
  parser->local_variables_forbidden_p = false;

  /* Finish the function call operator
     - class_specifier
     + late_parsing_for_member
     + function_definition_after_declarator
     + ctor_initializer_opt_and_function_body  */
  {
    tree fco = lambda_function (lambda_expr);
    tree body;
    bool done = false;
    tree compound_stmt;
    tree cap;

    /* Let the front end know that we are going to be defining this
       function.  */
    start_preparsed_function (fco,
			      NULL_TREE,
			      SF_PRE_PARSED | SF_INCLASS_INLINE);

    start_lambda_scope (fco);
    body = begin_function_body ();

    if (!cp_parser_require (parser, CPP_OPEN_BRACE, RT_OPEN_BRACE))
      goto out;

    /* Push the proxies for any explicit captures.  */
    for (cap = LAMBDA_EXPR_CAPTURE_LIST (lambda_expr); cap;
	 cap = TREE_CHAIN (cap))
      build_capture_proxy (TREE_PURPOSE (cap));

    compound_stmt = begin_compound_stmt (0);

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
        && cp_lexer_peek_nth_token (parser->lexer, 1)->keyword == RID_RETURN
        && cp_lexer_peek_nth_token (parser->lexer, 2)->type != CPP_SEMICOLON)
      {
	tree expr = NULL_TREE;
	cp_id_kind idk = CP_ID_KIND_NONE;

	/* Parse tentatively in case there's more after the initial return
	   statement.  */
	cp_parser_parse_tentatively (parser);

	cp_parser_require_keyword (parser, RID_RETURN, RT_RETURN);

	expr = cp_parser_expression (parser, /*cast_p=*/false, &idk);

	cp_parser_require (parser, CPP_SEMICOLON, RT_SEMICOLON);
	cp_parser_require (parser, CPP_CLOSE_BRACE, RT_CLOSE_BRACE);

	if (cp_parser_parse_definitely (parser))
	  {
	    if (!processing_template_decl)
	      apply_deduced_return_type (fco, lambda_return_type (expr));

	    /* Will get error here if type not deduced yet.  */
	    finish_return_stmt (expr);

	    done = true;
	  }
      }

    if (!done)
      {
	while (cp_lexer_next_token_is_keyword (parser->lexer, RID_LABEL))
	  cp_parser_label_declaration (parser);
	cp_parser_statement_seq_opt (parser, NULL_TREE);
	cp_parser_require (parser, CPP_CLOSE_BRACE, RT_CLOSE_BRACE);
      }

    finish_compound_stmt (compound_stmt);

  out:
    finish_function_body (body);
    finish_lambda_scope ();

    /* Finish the function and generate code for it if necessary.  */
    tree fn = finish_function (/*inline*/2);

    /* Only expand if the call op is not a template.  */
    if (!DECL_TEMPLATE_INFO (fco))
      expand_or_defer_fn (fn);
  }

  parser->local_variables_forbidden_p = local_variables_forbidden_p;
  if (nested)
    pop_function_context();
  else
    --function_depth;
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

  C++11:

  statement:
    labeled-statement
    attribute-specifier-seq (opt) expression-statement
    attribute-specifier-seq (opt) compound-statement
    attribute-specifier-seq (opt) selection-statement
    attribute-specifier-seq (opt) iteration-statement
    attribute-specifier-seq (opt) jump-statement
    declaration-statement
    attribute-specifier-seq (opt) try-block

  TM Extension:

   statement:
     atomic-statement

  IN_COMPOUND is true when the statement is nested inside a
  cp_parser_compound_statement; this matters for certain pragmas.

  If IF_P is not NULL, *IF_P is set to indicate whether the statement
  is a (possibly labeled) if statement which is not enclosed in braces
  and has an else clause.  This is used to implement -Wparentheses.  */

static void
cp_parser_statement (cp_parser* parser, tree in_statement_expr,
		     bool in_compound, bool *if_p)
{
  tree statement, std_attrs = NULL_TREE;
  cp_token *token;
  location_t statement_location, attrs_location;

 restart:
  if (if_p != NULL)
    *if_p = false;
  /* There is no statement yet.  */
  statement = NULL_TREE;

  cp_lexer_save_tokens (parser->lexer);
  attrs_location = cp_lexer_peek_token (parser->lexer)->location;
  if (c_dialect_objc ())
    /* In obj-c++, seeing '[[' might be the either the beginning of
       c++11 attributes, or a nested objc-message-expression.  So
       let's parse the c++11 attributes tentatively.  */
    cp_parser_parse_tentatively (parser);
  std_attrs = cp_parser_std_attribute_spec_seq (parser);
  if (c_dialect_objc ())
    {
      if (!cp_parser_parse_definitely (parser))
	std_attrs = NULL_TREE;
    }

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
	  cp_parser_label_for_labeled_statement (parser, std_attrs);
	  goto restart;

	case RID_IF:
	case RID_SWITCH:
	  statement = cp_parser_selection_statement (parser, if_p);
	  break;

	case RID_WHILE:
	case RID_DO:
	case RID_FOR:
	  statement = cp_parser_iteration_statement (parser, false);
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
	  
	case RID_TRANSACTION_ATOMIC:
	case RID_TRANSACTION_RELAXED:
	  statement = cp_parser_transaction (parser, keyword);
	  break;
	case RID_TRANSACTION_CANCEL:
	  statement = cp_parser_transaction_cancel (parser);
	  break;

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

	  cp_parser_label_for_labeled_statement (parser, std_attrs);
	  goto restart;
	}
    }
  /* Anything that starts with a `{' must be a compound-statement.  */
  else if (token->type == CPP_OPEN_BRACE)
    statement = cp_parser_compound_statement (parser, NULL, false, false);
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
	  if (std_attrs != NULL_TREE)
	    {
	      /*  Attributes should be parsed as part of the the
		  declaration, so let's un-parse them.  */
	      cp_lexer_rollback_tokens (parser->lexer);
	      std_attrs = NULL_TREE;
	    }

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

  /* Note that for now, we don't do anything with c++11 statements
     parsed at this level.  */
  if (std_attrs != NULL_TREE)
    warning_at (attrs_location,
		OPT_Wattributes,
		"attributes at the beginning of statement are ignored");
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
cp_parser_label_for_labeled_statement (cp_parser* parser, tree attributes)
{
  cp_token *token;
  tree label = NULL_TREE;
  bool saved_colon_corrects_to_scope_p = parser->colon_corrects_to_scope_p;

  /* The next token should be an identifier.  */
  token = cp_lexer_peek_token (parser->lexer);
  if (token->type != CPP_NAME
      && token->type != CPP_KEYWORD)
    {
      cp_parser_error (parser, "expected labeled-statement");
      return;
    }

  parser->colon_corrects_to_scope_p = false;
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
  cp_parser_require (parser, CPP_COLON, RT_COLON);

  /* An ordinary label may optionally be followed by attributes.
     However, this is only permitted if the attributes are then
     followed by a semicolon.  This is because, for backward
     compatibility, when parsing
       lab: __attribute__ ((unused)) int i;
     we want the attribute to attach to "i", not "lab".  */
  if (label != NULL_TREE
      && cp_next_tokens_can_be_gnu_attribute_p (parser))
    {
      tree attrs;
      cp_parser_parse_tentatively (parser);
      attrs = cp_parser_gnu_attributes_opt (parser);
      if (attrs == NULL_TREE
	  || cp_lexer_next_token_is_not (parser->lexer, CPP_SEMICOLON))
	cp_parser_abort_tentative_parse (parser);
      else if (!cp_parser_parse_definitely (parser))
	;
      else
	attributes = chainon (attributes, attrs);
    }

  if (attributes != NULL_TREE)
    cplus_decl_attributes (&label, attributes, 0);

  parser->colon_corrects_to_scope_p = saved_colon_corrects_to_scope_p;
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
    {
      statement = cp_parser_expression (parser, /*cast_p=*/false, NULL);
      if (statement == error_mark_node
	  && !cp_parser_uncommitted_to_tentative_parse_p (parser))
	{
	  cp_parser_skip_to_end_of_block_or_statement (parser);
	  return error_mark_node;
	}
    }

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
			      bool in_try, bool function_body)
{
  tree compound_stmt;

  /* Consume the `{'.  */
  if (!cp_parser_require (parser, CPP_OPEN_BRACE, RT_OPEN_BRACE))
    return error_mark_node;
  if (DECL_DECLARED_CONSTEXPR_P (current_function_decl)
      && !function_body)
    pedwarn (input_location, OPT_Wpedantic,
	     "compound-statement in constexpr function");
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
  cp_parser_require (parser, CPP_CLOSE_BRACE, RT_CLOSE_BRACE);

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

      /* If we are looking at a `}', then we have run out of
	 statements; the same is true if we have reached the end
	 of file, or have stumbled upon a stray '@end'.  */
      if (token->type == CPP_CLOSE_BRACE
	  || token->type == CPP_EOF
	  || token->type == CPP_PRAGMA_EOL
	  || (token->type == CPP_KEYWORD && token->keyword == RID_AT_END))
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
  token = cp_parser_require (parser, CPP_KEYWORD, RT_SELECT);

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
	if (!cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN))
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
	if (!cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN))
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
  int declares_class_or_enum;

  /* Try the declaration first.  */
  cp_parser_parse_tentatively (parser);
  /* New types are not allowed in the type-specifier-seq for a
     condition.  */
  saved_message = parser->type_definition_forbidden_message;
  parser->type_definition_forbidden_message
    = G_("types may not be defined in conditions");
  /* Parse the type-specifier-seq.  */
  cp_parser_decl_specifier_seq (parser,
				CP_PARSER_FLAGS_ONLY_TYPE_OR_CONSTEXPR,
				&type_specifiers,
				&declares_class_or_enum);
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
	      cp_parser_require (parser, CPP_EQ, RT_EQ);
	      initializer = cp_parser_initializer_clause (parser, &non_constant_p);
	    }
	  if (BRACE_ENCLOSED_INITIALIZER_P (initializer))
	    maybe_warn_cpp0x (CPP0X_INITIALIZER_LISTS);

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

/* Parses a for-statement or range-for-statement until the closing ')',
   not included. */

static tree
cp_parser_for (cp_parser *parser, bool ivdep)
{
  tree init, scope, decl;
  bool is_range_for;

  /* Begin the for-statement.  */
  scope = begin_for_scope (&init);

  /* Parse the initialization.  */
  is_range_for = cp_parser_for_init_statement (parser, &decl);

  if (is_range_for)
    return cp_parser_range_for (parser, scope, init, decl, ivdep);
  else
    return cp_parser_c_for (parser, scope, init, ivdep);
}

static tree
cp_parser_c_for (cp_parser *parser, tree scope, tree init, bool ivdep)
{
  /* Normal for loop */
  tree condition = NULL_TREE;
  tree expression = NULL_TREE;
  tree stmt;

  stmt = begin_for_stmt (scope, init);
  /* The for-init-statement has already been parsed in
     cp_parser_for_init_statement, so no work is needed here.  */
  finish_for_init_stmt (stmt);

  /* If there's a condition, process it.  */
  if (cp_lexer_next_token_is_not (parser->lexer, CPP_SEMICOLON))
    condition = cp_parser_condition (parser);
  else if (ivdep)
    {
      cp_parser_error (parser, "missing loop condition in loop with "
		       "%<GCC ivdep%> pragma");
      condition = error_mark_node;
    }
  finish_for_cond (condition, stmt, ivdep);
  /* Look for the `;'.  */
  cp_parser_require (parser, CPP_SEMICOLON, RT_SEMICOLON);

  /* If there's an expression, process it.  */
  if (cp_lexer_next_token_is_not (parser->lexer, CPP_CLOSE_PAREN))
    expression = cp_parser_expression (parser, /*cast_p=*/false, NULL);
  finish_for_expr (expression, stmt);

  return stmt;
}

/* Tries to parse a range-based for-statement:

  range-based-for:
    decl-specifier-seq declarator : expression

  The decl-specifier-seq declarator and the `:' are already parsed by
  cp_parser_for_init_statement. If processing_template_decl it returns a
  newly created RANGE_FOR_STMT; if not, it is converted to a
  regular FOR_STMT.  */

static tree
cp_parser_range_for (cp_parser *parser, tree scope, tree init, tree range_decl,
		     bool ivdep)
{
  tree stmt, range_expr;

  if (cp_lexer_next_token_is (parser->lexer, CPP_OPEN_BRACE))
    {
      bool expr_non_constant_p;
      range_expr = cp_parser_braced_list (parser, &expr_non_constant_p);
    }
  else
    range_expr = cp_parser_expression (parser, /*cast_p=*/false, NULL);

  /* If in template, STMT is converted to a normal for-statement
     at instantiation. If not, it is done just ahead. */
  if (processing_template_decl)
    {
      if (check_for_bare_parameter_packs (range_expr))
	range_expr = error_mark_node;
      stmt = begin_range_for_stmt (scope, init);
      if (ivdep)
	RANGE_FOR_IVDEP (stmt) = 1;
      finish_range_for_decl (stmt, range_decl, range_expr);
      if (!type_dependent_expression_p (range_expr)
	  /* do_auto_deduction doesn't mess with template init-lists.  */
	  && !BRACE_ENCLOSED_INITIALIZER_P (range_expr))
	do_range_for_auto_deduction (range_decl, range_expr);
    }
  else
    {
      stmt = begin_for_stmt (scope, init);
      stmt = cp_convert_range_for (stmt, range_decl, range_expr, ivdep);
    }
  return stmt;
}

/* Subroutine of cp_convert_range_for: given the initializer expression,
   builds up the range temporary.  */

static tree
build_range_temp (tree range_expr)
{
  tree range_type, range_temp;

  /* Find out the type deduced by the declaration
     `auto &&__range = range_expr'.  */
  range_type = cp_build_reference_type (make_auto (), true);
  range_type = do_auto_deduction (range_type, range_expr,
				  type_uses_auto (range_type));

  /* Create the __range variable.  */
  range_temp = build_decl (input_location, VAR_DECL,
			   get_identifier ("__for_range"), range_type);
  TREE_USED (range_temp) = 1;
  DECL_ARTIFICIAL (range_temp) = 1;

  return range_temp;
}

/* Used by cp_parser_range_for in template context: we aren't going to
   do a full conversion yet, but we still need to resolve auto in the
   type of the for-range-declaration if present.  This is basically
   a shortcut version of cp_convert_range_for.  */

static void
do_range_for_auto_deduction (tree decl, tree range_expr)
{
  tree auto_node = type_uses_auto (TREE_TYPE (decl));
  if (auto_node)
    {
      tree begin_dummy, end_dummy, range_temp, iter_type, iter_decl;
      range_temp = convert_from_reference (build_range_temp (range_expr));
      iter_type = (cp_parser_perform_range_for_lookup
		   (range_temp, &begin_dummy, &end_dummy));
      if (iter_type)
	{
	  iter_decl = build_decl (input_location, VAR_DECL, NULL_TREE,
				  iter_type);
	  iter_decl = build_x_indirect_ref (input_location, iter_decl, RO_NULL,
					    tf_warning_or_error);
	  TREE_TYPE (decl) = do_auto_deduction (TREE_TYPE (decl),
						iter_decl, auto_node);
	}
    }
}

/* Converts a range-based for-statement into a normal
   for-statement, as per the definition.

      for (RANGE_DECL : RANGE_EXPR)
	BLOCK

   should be equivalent to:

      {
	auto &&__range = RANGE_EXPR;
	for (auto __begin = BEGIN_EXPR, end = END_EXPR;
	      __begin != __end;
	      ++__begin)
	  {
	      RANGE_DECL = *__begin;
	      BLOCK
	  }
      }

   If RANGE_EXPR is an array:
	BEGIN_EXPR = __range
	END_EXPR = __range + ARRAY_SIZE(__range)
   Else if RANGE_EXPR has a member 'begin' or 'end':
	BEGIN_EXPR = __range.begin()
	END_EXPR = __range.end()
   Else:
	BEGIN_EXPR = begin(__range)
	END_EXPR = end(__range);

   If __range has a member 'begin' but not 'end', or vice versa, we must
   still use the second alternative (it will surely fail, however).
   When calling begin()/end() in the third alternative we must use
   argument dependent lookup, but always considering 'std' as an associated
   namespace.  */

tree
cp_convert_range_for (tree statement, tree range_decl, tree range_expr,
		      bool ivdep)
{
  tree begin, end;
  tree iter_type, begin_expr, end_expr;
  tree condition, expression;

  if (range_decl == error_mark_node || range_expr == error_mark_node)
    /* If an error happened previously do nothing or else a lot of
       unhelpful errors would be issued.  */
    begin_expr = end_expr = iter_type = error_mark_node;
  else
    {
      tree range_temp;

      if (TREE_CODE (range_expr) == VAR_DECL
	  && array_of_runtime_bound_p (TREE_TYPE (range_expr)))
	/* Can't bind a reference to an array of runtime bound.  */
	range_temp = range_expr;
      else
	{
	  range_temp = build_range_temp (range_expr);
	  pushdecl (range_temp);
	  cp_finish_decl (range_temp, range_expr,
			  /*is_constant_init*/false, NULL_TREE,
			  LOOKUP_ONLYCONVERTING);
	  range_temp = convert_from_reference (range_temp);
	}
      iter_type = cp_parser_perform_range_for_lookup (range_temp,
						      &begin_expr, &end_expr);
    }

  /* The new for initialization statement.  */
  begin = build_decl (input_location, VAR_DECL,
		      get_identifier ("__for_begin"), iter_type);
  TREE_USED (begin) = 1;
  DECL_ARTIFICIAL (begin) = 1;
  pushdecl (begin);
  cp_finish_decl (begin, begin_expr,
		  /*is_constant_init*/false, NULL_TREE,
		  LOOKUP_ONLYCONVERTING);

  end = build_decl (input_location, VAR_DECL,
		    get_identifier ("__for_end"), iter_type);
  TREE_USED (end) = 1;
  DECL_ARTIFICIAL (end) = 1;
  pushdecl (end);
  cp_finish_decl (end, end_expr,
		  /*is_constant_init*/false, NULL_TREE,
		  LOOKUP_ONLYCONVERTING);

  finish_for_init_stmt (statement);

  /* The new for condition.  */
  condition = build_x_binary_op (input_location, NE_EXPR,
				 begin, ERROR_MARK,
				 end, ERROR_MARK,
				 NULL, tf_warning_or_error);
  finish_for_cond (condition, statement, ivdep);

  /* The new increment expression.  */
  expression = finish_unary_op_expr (input_location,
				     PREINCREMENT_EXPR, begin,
				     tf_warning_or_error);
  finish_for_expr (expression, statement);

  /* The declaration is initialized with *__begin inside the loop body.  */
  cp_finish_decl (range_decl,
		  build_x_indirect_ref (input_location, begin, RO_NULL,
					tf_warning_or_error),
		  /*is_constant_init*/false, NULL_TREE,
		  LOOKUP_ONLYCONVERTING);

  return statement;
}

/* Solves BEGIN_EXPR and END_EXPR as described in cp_convert_range_for.
   We need to solve both at the same time because the method used
   depends on the existence of members begin or end.
   Returns the type deduced for the iterator expression.  */

static tree
cp_parser_perform_range_for_lookup (tree range, tree *begin, tree *end)
{
  if (error_operand_p (range))
    {
      *begin = *end = error_mark_node;
      return error_mark_node;
    }

  if (!COMPLETE_TYPE_P (complete_type (TREE_TYPE (range))))
    {
      error ("range-based %<for%> expression of type %qT "
	     "has incomplete type", TREE_TYPE (range));
      *begin = *end = error_mark_node;
      return error_mark_node;
    }
  if (TREE_CODE (TREE_TYPE (range)) == ARRAY_TYPE)
    {
      /* If RANGE is an array, we will use pointer arithmetic.  */
      *begin = range;
      *end = build_binary_op (input_location, PLUS_EXPR,
			      range,
			      array_type_nelts_top (TREE_TYPE (range)),
			      0);
      return build_pointer_type (TREE_TYPE (TREE_TYPE (range)));
    }
  else
    {
      /* If it is not an array, we must do a bit of magic.  */
      tree id_begin, id_end;
      tree member_begin, member_end;

      *begin = *end = error_mark_node;

      id_begin = get_identifier ("begin");
      id_end = get_identifier ("end");
      member_begin = lookup_member (TREE_TYPE (range), id_begin,
				    /*protect=*/2, /*want_type=*/false,
				    tf_warning_or_error);
      member_end = lookup_member (TREE_TYPE (range), id_end,
				  /*protect=*/2, /*want_type=*/false,
				  tf_warning_or_error);

      if (member_begin != NULL_TREE || member_end != NULL_TREE)
	{
	  /* Use the member functions.  */
	  if (member_begin != NULL_TREE)
	    *begin = cp_parser_range_for_member_function (range, id_begin);
	  else
	    error ("range-based %<for%> expression of type %qT has an "
		   "%<end%> member but not a %<begin%>", TREE_TYPE (range));

	  if (member_end != NULL_TREE)
	    *end = cp_parser_range_for_member_function (range, id_end);
	  else
	    error ("range-based %<for%> expression of type %qT has a "
		   "%<begin%> member but not an %<end%>", TREE_TYPE (range));
	}
      else
	{
	  /* Use global functions with ADL.  */
	  vec<tree, va_gc> *vec;
	  vec = make_tree_vector ();

	  vec_safe_push (vec, range);

	  member_begin = perform_koenig_lookup (id_begin, vec,
						tf_warning_or_error);
	  *begin = finish_call_expr (member_begin, &vec, false, true,
				     tf_warning_or_error);
	  member_end = perform_koenig_lookup (id_end, vec,
					      tf_warning_or_error);
	  *end = finish_call_expr (member_end, &vec, false, true,
				   tf_warning_or_error);

	  release_tree_vector (vec);
	}

      /* Last common checks.  */
      if (*begin == error_mark_node || *end == error_mark_node)
	{
	  /* If one of the expressions is an error do no more checks.  */
	  *begin = *end = error_mark_node;
	  return error_mark_node;
	}
      else if (type_dependent_expression_p (*begin)
	       || type_dependent_expression_p (*end))
	/* Can happen, when, eg, in a template context, Koenig lookup
	   can't resolve begin/end (c++/58503).  */
	return NULL_TREE;
      else
	{
	  tree iter_type = cv_unqualified (TREE_TYPE (*begin));
	  /* The unqualified type of the __begin and __end temporaries should
	     be the same, as required by the multiple auto declaration.  */
	  if (!same_type_p (iter_type, cv_unqualified (TREE_TYPE (*end))))
	    error ("inconsistent begin/end types in range-based %<for%> "
		   "statement: %qT and %qT",
		   TREE_TYPE (*begin), TREE_TYPE (*end));
	  return iter_type;
	}
    }
}

/* Helper function for cp_parser_perform_range_for_lookup.
   Builds a tree for RANGE.IDENTIFIER().  */

static tree
cp_parser_range_for_member_function (tree range, tree identifier)
{
  tree member, res;
  vec<tree, va_gc> *vec;

  member = finish_class_member_access_expr (range, identifier,
					    false, tf_warning_or_error);
  if (member == error_mark_node)
    return error_mark_node;

  vec = make_tree_vector ();
  res = finish_call_expr (member, &vec,
			  /*disallow_virtual=*/false,
			  /*koenig_p=*/false,
			  tf_warning_or_error);
  release_tree_vector (vec);
  return res;
}

/* Parse an iteration-statement.

   iteration-statement:
     while ( condition ) statement
     do statement while ( expression ) ;
     for ( for-init-statement condition [opt] ; expression [opt] )
       statement

   Returns the new WHILE_STMT, DO_STMT, FOR_STMT or RANGE_FOR_STMT.  */

static tree
cp_parser_iteration_statement (cp_parser* parser, bool ivdep)
{
  cp_token *token;
  enum rid keyword;
  tree statement;
  unsigned char in_statement;

  /* Peek at the next token.  */
  token = cp_parser_require (parser, CPP_KEYWORD, RT_INTERATION);
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
	cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN);
	/* Parse the condition.  */
	condition = cp_parser_condition (parser);
	finish_while_stmt_cond (condition, statement, ivdep);
	/* Look for the `)'.  */
	cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN);
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
	cp_parser_require_keyword (parser, RID_WHILE, RT_WHILE);
	/* Look for the `('.  */
	cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN);
	/* Parse the expression.  */
	expression = cp_parser_expression (parser, /*cast_p=*/false, NULL);
	/* We're done with the do-statement.  */
	finish_do_stmt (expression, statement, ivdep);
	/* Look for the `)'.  */
	cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN);
	/* Look for the `;'.  */
	cp_parser_require (parser, CPP_SEMICOLON, RT_SEMICOLON);
      }
      break;

    case RID_FOR:
      {
	/* Look for the `('.  */
	cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN);

	statement = cp_parser_for (parser, ivdep);

	/* Look for the `)'.  */
	cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN);

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

/* Parse a for-init-statement or the declarator of a range-based-for.
   Returns true if a range-based-for declaration is seen.

   for-init-statement:
     expression-statement
     simple-declaration  */

static bool
cp_parser_for_init_statement (cp_parser* parser, tree *decl)
{
  /* If the next token is a `;', then we have an empty
     expression-statement.  Grammatically, this is also a
     simple-declaration, but an invalid one, because it does not
     declare anything.  Therefore, if we did not handle this case
     specially, we would issue an error message about an invalid
     declaration.  */
  if (cp_lexer_next_token_is_not (parser->lexer, CPP_SEMICOLON))
    {
      bool is_range_for = false;
      bool saved_colon_corrects_to_scope_p = parser->colon_corrects_to_scope_p;

      parser->colon_corrects_to_scope_p = false;

      /* We're going to speculatively look for a declaration, falling back
	 to an expression, if necessary.  */
      cp_parser_parse_tentatively (parser);
      /* Parse the declaration.  */
      cp_parser_simple_declaration (parser,
				    /*function_definition_allowed_p=*/false,
				    decl);
      parser->colon_corrects_to_scope_p = saved_colon_corrects_to_scope_p;
      if (cp_lexer_next_token_is (parser->lexer, CPP_COLON))
	{
	  /* It is a range-for, consume the ':' */
	  cp_lexer_consume_token (parser->lexer);
	  is_range_for = true;
	  if (cxx_dialect < cxx11)
	    {
	      error_at (cp_lexer_peek_token (parser->lexer)->location,
			"range-based %<for%> loops are not allowed "
			"in C++98 mode");
	      *decl = error_mark_node;
	    }
	}
      else
	  /* The ';' is not consumed yet because we told
	     cp_parser_simple_declaration not to.  */
	  cp_parser_require (parser, CPP_SEMICOLON, RT_SEMICOLON);

      if (cp_parser_parse_definitely (parser))
	return is_range_for;
      /* If the tentative parse failed, then we shall need to look for an
	 expression-statement.  */
    }
  /* If we are here, it is an expression-statement.  */
  cp_parser_expression_statement (parser, NULL_TREE);
  return false;
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
  token = cp_parser_require (parser, CPP_KEYWORD, RT_JUMP);
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
	  if (in_statement == IN_ITERATION_STMT)
	    break_maybe_infinite_loop ();
	  break;
	case IN_OMP_BLOCK:
	  error_at (token->location, "invalid exit from OpenMP structured block");
	  break;
	case IN_OMP_FOR:
	  error_at (token->location, "break statement used with OpenMP for loop");
	  break;
	case IN_CILK_SIMD_FOR:
	  error_at (token->location, "break statement used with Cilk Plus for loop");
	  break;
	}
      cp_parser_require (parser, CPP_SEMICOLON, RT_SEMICOLON);
      break;

    case RID_CONTINUE:
      switch (parser->in_statement & ~(IN_SWITCH_STMT | IN_IF_STMT))
	{
	case 0:
	  error_at (token->location, "continue statement not within a loop");
	  break;
	case IN_CILK_SIMD_FOR:
	  error_at (token->location,
		    "continue statement within %<#pragma simd%> loop body");
	  /* Fall through.  */
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
      cp_parser_require (parser, CPP_SEMICOLON, RT_SEMICOLON);
      break;

    case RID_RETURN:
      {
	tree expr;
	bool expr_non_constant_p;

	if (cp_lexer_next_token_is (parser->lexer, CPP_OPEN_BRACE))
	  {
	    cp_token *token = cp_lexer_peek_token (parser->lexer);
	    cp_lexer_set_source_position_from_token (token);
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
	cp_parser_require (parser, CPP_SEMICOLON, RT_SEMICOLON);
      }
      break;

    case RID_GOTO:
      /* Create the goto-statement.  */
      if (cp_lexer_next_token_is (parser->lexer, CPP_MULT))
	{
	  /* Issue a warning about this use of a GNU extension.  */
	  pedwarn (token->location, OPT_Wpedantic, "ISO C++ forbids computed gotos");
	  /* Consume the '*' token.  */
	  cp_lexer_consume_token (parser->lexer);
	  /* Parse the dependent expression.  */
	  finish_goto_stmt (cp_parser_expression (parser, /*cast_p=*/false, NULL));
	}
      else
	finish_goto_stmt (cp_parser_identifier (parser));
      /* Look for the final `;'.  */
      cp_parser_require (parser, CPP_SEMICOLON, RT_SEMICOLON);
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
    statement = cp_parser_compound_statement (parser, NULL, false, false);
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
      cp_parser_require (parser, CPP_OPEN_BRACE, RT_OPEN_BRACE);
      /* If the next keyword is `__label__' we have a label declaration.  */
      while (cp_lexer_next_token_is_keyword (parser->lexer, RID_LABEL))
	cp_parser_label_declaration (parser);
      /* Parse an (optional) statement-seq.  */
      cp_parser_statement_seq_opt (parser, NULL_TREE);
      cp_parser_require (parser, CPP_CLOSE_BRACE, RT_CLOSE_BRACE);
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
	  if (!in_system_header_at (input_location))
	    pedwarn (input_location, OPT_Wpedantic, "extra %<;%>");
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
  tree attributes = NULL_TREE;

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
      && cp_parser_is_pure_string_literal (&token2))
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
    cp_parser_objc_declaration (parser, NULL_TREE);
  else if (c_dialect_objc ()
	   && token1.keyword == RID_ATTRIBUTE
	   && cp_parser_objc_valid_prefix_attributes (parser, &attributes))
    cp_parser_objc_declaration (parser, attributes);
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
  /* If the next keyword is `using', we have a
     using-declaration, a using-directive, or an alias-declaration.  */
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
      /* If the second token after 'using' is '=', then we have an
	 alias-declaration.  */
      else if (cxx_dialect >= cxx11
	       && token2->type == CPP_NAME
	       && ((cp_lexer_peek_nth_token (parser->lexer, 3)->type == CPP_EQ)
		   || (cp_nth_tokens_can_be_attribute_p (parser, 3))))
	cp_parser_alias_declaration (parser);
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
    cp_parser_simple_declaration (parser, !statement_p,
				  /*maybe_range_for_decl*/NULL);
}

/* Parse a simple-declaration.

   simple-declaration:
     decl-specifier-seq [opt] init-declarator-list [opt] ;

   init-declarator-list:
     init-declarator
     init-declarator-list , init-declarator

   If FUNCTION_DEFINITION_ALLOWED_P is TRUE, then we also recognize a
   function-definition as a simple-declaration.

   If MAYBE_RANGE_FOR_DECL is not NULL, the pointed tree will be set to the
   parsed declaration if it is an uninitialized single declarator not followed
   by a `;', or to error_mark_node otherwise. Either way, the trailing `;',
   if present, will not be consumed.  */

static void
cp_parser_simple_declaration (cp_parser* parser,
			      bool function_definition_allowed_p,
			      tree *maybe_range_for_decl)
{
  cp_decl_specifier_seq decl_specifiers;
  int declares_class_or_enum;
  bool saw_declarator;

  if (maybe_range_for_decl)
    *maybe_range_for_decl = NULL_TREE;

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
	  if (maybe_range_for_decl)
	    *maybe_range_for_decl = error_mark_node;
	}
      else
	saw_declarator = true;

      /* Parse the init-declarator.  */
      decl = cp_parser_init_declarator (parser, &decl_specifiers,
					/*checks=*/NULL,
					function_definition_allowed_p,
					/*member_p=*/false,
					declares_class_or_enum,
					&function_definition_p,
					maybe_range_for_decl);
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
      if (maybe_range_for_decl && *maybe_range_for_decl == NULL_TREE)
	*maybe_range_for_decl = decl;
      /* The next token should be either a `,' or a `;'.  */
      token = cp_lexer_peek_token (parser->lexer);
      /* If it's a `,', there are more declarators to come.  */
      if (token->type == CPP_COMMA)
	/* will be consumed next time around */;
      /* If it's a `;', we are done.  */
      else if (token->type == CPP_SEMICOLON || maybe_range_for_decl)
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
     enumeration: [dcl.dcl]/3.  */
  if (!saw_declarator)
    {
      if (cp_parser_declares_only_class_p (parser))
	{
	  if (!declares_class_or_enum
	      && decl_specifiers.type
	      && OVERLOAD_TYPE_P (decl_specifiers.type))
	    /* Ensure an error is issued anyway when finish_decltype_type,
	       called via cp_parser_decl_specifier_seq, returns a class or
	       an enumeration (c++/51786).  */
	    decl_specifiers.type = NULL_TREE;
	  shadow_tag (&decl_specifiers);
	}
      /* Perform any deferred access checks.  */
      perform_deferred_access_checks (tf_warning_or_error);
    }

  /* Consume the `;'.  */
  if (!maybe_range_for_decl)
      cp_parser_require (parser, CPP_SEMICOLON, RT_SEMICOLON);

 done:
  pop_deferring_access_checks ();
}

/* Parse a decl-specifier-seq.

   decl-specifier-seq:
     decl-specifier-seq [opt] decl-specifier
     decl-specifier attribute-specifier-seq [opt] (C++11)

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
  bool found_decl_spec = false;
  cp_token *start_token = NULL;
  cp_decl_spec ds;

  /* Clear DECL_SPECS.  */
  clear_decl_specs (decl_specs);

  /* Assume no class or enumeration type is declared.  */
  *declares_class_or_enum = 0;

  /* Keep reading specifiers until there are no more to read.  */
  while (true)
    {
      bool constructor_p;
      cp_token *token;
      ds = ds_last;

      /* Peek at the next token.  */
      token = cp_lexer_peek_token (parser->lexer);

      /* Save the first token of the decl spec list for error
         reporting.  */
      if (!start_token)
	start_token = token;
      /* Handle attributes.  */
      if (cp_next_tokens_can_be_attribute_p (parser))
	{
	  /* Parse the attributes.  */
	  tree attrs = cp_parser_attributes_opt (parser);

	  /* In a sequence of declaration specifiers, c++11 attributes
	     appertain to the type that precede them. In that case
	     [dcl.spec]/1 says:

	         The attribute-specifier-seq affects the type only for
		 the declaration it appears in, not other declarations
		 involving the same type.

             But for now let's force the user to position the
             attribute either at the beginning of the declaration or
             after the declarator-id, which would clearly mean that it
             applies to the declarator.  */
	  if (cxx11_attribute_p (attrs))
	    {
	      if (!found_decl_spec)
		/* The c++11 attribute is at the beginning of the
		   declaration.  It appertains to the entity being
		   declared.  */;
	      else
		{
		  if (decl_specs->type && CLASS_TYPE_P (decl_specs->type))
		    {
		      /*  This is an attribute following a
			  class-specifier.  */
		      if (decl_specs->type_definition_p)
			warn_misplaced_attr_for_class_type (token->location,
							    decl_specs->type);
		      attrs = NULL_TREE;
		    }
		  else
		    {
		      decl_specs->std_attributes
			= chainon (decl_specs->std_attributes,
				   attrs);
		      if (decl_specs->locations[ds_std_attribute] == 0)
			decl_specs->locations[ds_std_attribute] = token->location;
		    }
		  continue;
		}
	    }

	    decl_specs->attributes
	      = chainon (decl_specs->attributes,
			 attrs);
	  if (decl_specs->locations[ds_attribute] == 0)
	    decl_specs->locations[ds_attribute] = token->location;
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
	      ds = ds_friend;
	      /* Consume the token.  */
	      cp_lexer_consume_token (parser->lexer);
	    }
	  break;

        case RID_CONSTEXPR:
	  ds = ds_constexpr;
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
	  ds = ds_typedef;
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
			  " changes meaning in C++11; please remove it");

              /* Set the storage class anyway.  */
              cp_parser_set_storage_class (parser, decl_specs, RID_AUTO,
					   token);
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
				       token);
	  break;
	case RID_THREAD:
	  /* Consume the token.  */
	  ds = ds_thread;
	  cp_lexer_consume_token (parser->lexer);
	  break;

	default:
	  /* We did not yet find a decl-specifier yet.  */
	  found_decl_spec = false;
	  break;
	}

      if (found_decl_spec
	  && (flags & CP_PARSER_FLAGS_ONLY_TYPE_OR_CONSTEXPR)
	  && token->keyword != RID_CONSTEXPR)
	error ("decl-specifier invalid in condition");

      if (ds != ds_last)
	set_and_check_decl_spec_loc (decl_specs, ds, token);

      /* Constructors are a special case.  The `S' in `S()' is not a
	 decl-specifier; it is the beginning of the declarator.  */
      constructor_p
	= (!found_decl_spec
	   && constructor_possible_p
	   && (cp_parser_constructor_declarator_p
	       (parser, decl_spec_seq_has_spec_p (decl_specs, ds_friend))));

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

  /* Don't allow a friend specifier with a class definition.  */
  if (decl_spec_seq_has_spec_p (decl_specs, ds_friend)
      && (*declares_class_or_enum & 2))
    error_at (decl_specs->locations[ds_friend],
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
      set_and_check_decl_spec_loc (decl_specs, ds_inline, token);
      break;

    case RID_VIRTUAL:
      /* 14.5.2.3 [temp.mem]

	 A member function template shall not be virtual.  */
      if (PROCESSING_REAL_TEMPLATE_DECL_P ())
	error_at (token->location, "templates may not be %<virtual%>");
      else
	set_and_check_decl_spec_loc (decl_specs, ds_virtual, token);
      break;

    case RID_EXPLICIT:
      set_and_check_decl_spec_loc (decl_specs, ds_explicit, token);
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
  cp_parser_require_keyword (parser, RID_EXTERN, RT_EXTERN);

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
      cp_ensure_no_omp_declare_simd (parser);

      /* Consume the `{' token.  */
      cp_lexer_consume_token (parser->lexer);
      /* Parse the declarations.  */
      cp_parser_declaration_seq_opt (parser);
      /* Look for the closing `}'.  */
      cp_parser_require (parser, CPP_CLOSE_BRACE, RT_CLOSE_BRACE);
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
  bool dummy;

  /* Peek at the `static_assert' token so we can keep track of exactly
     where the static assertion started.  */
  token = cp_lexer_peek_token (parser->lexer);
  saved_loc = token->location;

  /* Look for the `static_assert' keyword.  */
  if (!cp_parser_require_keyword (parser, RID_STATIC_ASSERT, 
                                  RT_STATIC_ASSERT))
    return;

  /*  We know we are in a static assertion; commit to any tentative
      parse.  */
  if (cp_parser_parsing_tentatively (parser))
    cp_parser_commit_to_tentative_parse (parser);

  /* Parse the `(' starting the static assertion condition.  */
  cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN);

  /* Parse the constant-expression.  Allow a non-constant expression
     here in order to give better diagnostics in finish_static_assert.  */
  condition = 
    cp_parser_constant_expression (parser,
                                   /*allow_non_constant_p=*/true,
                                   /*non_constant_p=*/&dummy);

  /* Parse the separating `,'.  */
  cp_parser_require (parser, CPP_COMMA, RT_COMMA);

  /* Parse the string-literal message.  */
  message = cp_parser_string_literal (parser, 
                                      /*translate=*/false,
                                      /*wide_ok=*/true);

  /* A `)' completes the static assertion.  */
  if (!cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN))
    cp_parser_skip_to_closing_parenthesis (parser, 
                                           /*recovering=*/true, 
                                           /*or_comma=*/false,
					   /*consume_paren=*/true);

  /* A semicolon terminates the declaration.  */
  cp_parser_require (parser, CPP_SEMICOLON, RT_SEMICOLON);

  /* Complete the static assertion, which may mean either processing 
     the static assert now or saving it for template instantiation.  */
  finish_static_assert (condition, message, saved_loc, member_p);
}

/* Parse the expression in decltype ( expression ).  */

static tree
cp_parser_decltype_expr (cp_parser *parser,
			 bool &id_expression_or_member_access_p)
{
  cp_token *id_expr_start_token;
  tree expr;

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

      if (identifier_p (expr))
	/* Lookup the name we got back from the id-expression.  */
	expr = cp_parser_lookup_name_simple (parser, expr,
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
                                           /*cast_p=*/false, /*decltype*/true,
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
      /* Abort our attempt to parse an id-expression or member access
         expression.  */
      cp_parser_abort_tentative_parse (parser);

      /* Parse a full expression.  */
      expr = cp_parser_expression (parser, /*cast_p=*/false,
				   /*decltype*/true, NULL);
    }

  return expr;
}

/* Parse a `decltype' type. Returns the type.

   simple-type-specifier:
     decltype ( expression )
   C++14 proposal:
     decltype ( auto )  */

static tree
cp_parser_decltype (cp_parser *parser)
{
  tree expr;
  bool id_expression_or_member_access_p = false;
  const char *saved_message;
  bool saved_integral_constant_expression_p;
  bool saved_non_integral_constant_expression_p;
  bool saved_greater_than_is_operator_p;
  cp_token *start_token = cp_lexer_peek_token (parser->lexer);

  if (start_token->type == CPP_DECLTYPE)
    {
      /* Already parsed.  */
      cp_lexer_consume_token (parser->lexer);
      return start_token->u.value;
    }

  /* Look for the `decltype' token.  */
  if (!cp_parser_require_keyword (parser, RID_DECLTYPE, RT_DECLTYPE))
    return error_mark_node;

  /* Parse the opening `('.  */
  if (!cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN))
    return error_mark_node;

  /* decltype (auto) */
  if (cxx_dialect >= cxx1y
      && cp_lexer_next_token_is_keyword (parser->lexer, RID_AUTO))
    {
      cp_lexer_consume_token (parser->lexer);
      if (!cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN))
	return error_mark_node;
      expr = make_decltype_auto ();
      AUTO_IS_DECLTYPE (expr) = true;
      goto rewrite;
    }

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

  /* Within a parenthesized expression, a `>' token is always
     the greater-than operator.  */
  saved_greater_than_is_operator_p
    = parser->greater_than_is_operator_p;
  parser->greater_than_is_operator_p = true;

  /* Do not actually evaluate the expression.  */
  ++cp_unevaluated_operand;

  /* Do not warn about problems with the expression.  */
  ++c_inhibit_evaluation_warnings;

  expr = cp_parser_decltype_expr (parser, id_expression_or_member_access_p);

  /* Go back to evaluating expressions.  */
  --cp_unevaluated_operand;
  --c_inhibit_evaluation_warnings;

  /* The `>' token might be the end of a template-id or
     template-parameter-list now.  */
  parser->greater_than_is_operator_p
    = saved_greater_than_is_operator_p;

  /* Restore the old message and the integral constant expression
     flags.  */
  parser->type_definition_forbidden_message = saved_message;
  parser->integral_constant_expression_p
    = saved_integral_constant_expression_p;
  parser->non_integral_constant_expression_p
    = saved_non_integral_constant_expression_p;

  /* Parse to the closing `)'.  */
  if (!cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN))
    {
      cp_parser_skip_to_closing_parenthesis (parser, true, false,
					     /*consume_paren=*/true);
      return error_mark_node;
    }

  expr = finish_decltype_type (expr, id_expression_or_member_access_p,
			       tf_warning_or_error);

 rewrite:
  /* Replace the decltype with a CPP_DECLTYPE so we don't need to parse
     it again.  */
  start_token->type = CPP_DECLTYPE;
  start_token->u.value = expr;
  start_token->keyword = RID_MAX;
  cp_lexer_purge_tokens_after (parser->lexer, start_token);

  return expr;
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
  if (!cp_parser_require_keyword (parser, RID_OPERATOR, RT_OPERATOR))
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
  const char *saved_message;

  /* Parse the attributes.  */
  attributes = cp_parser_attributes_opt (parser);

  saved_message = parser->type_definition_forbidden_message;
  parser->type_definition_forbidden_message
    = G_("types may not be defined in a conversion-type-id");

  /* Parse the type-specifiers.  */
  cp_parser_type_specifier_seq (parser, /*is_declaration=*/false,
				/*is_trailing_return=*/false,
				&type_specifiers);

  parser->type_definition_forbidden_message = saved_message;

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
      if (cxx_dialect < cxx1y)
	{
	  error ("invalid use of %<auto%> in conversion operator");
	  return error_mark_node;
	}
      else if (template_parm_scope_p ())
	warning (0, "use of %<auto%> in member template "
		 "conversion operator can never be deduced");
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
  tree class_type, std_attributes = NULL_TREE;
  cp_cv_quals cv_quals;

  /* We don't know if there's a ptr-operator next, or not.  */
  cp_parser_parse_tentatively (parser);
  /* Try the ptr-operator.  */
  code = cp_parser_ptr_operator (parser, &class_type, &cv_quals,
				 &std_attributes);
  /* If it worked, look for more conversion-declarators.  */
  if (cp_parser_parse_definitely (parser))
    {
      cp_declarator *declarator;

      /* Parse another optional declarator.  */
      declarator = cp_parser_conversion_declarator_opt (parser);

      declarator = cp_parser_make_indirect_declarator
	(code, class_type, cv_quals, declarator, std_attributes);

      return declarator;
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
  tree target_ctor = error_mark_node;
  cp_token *token = cp_lexer_peek_token (parser->lexer);

  /* Let the semantic analysis code know that we are starting the
     mem-initializer-list.  */
  if (!DECL_CONSTRUCTOR_P (current_function_decl))
    error_at (token->location,
	      "only constructors take member initializers");

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
      if (target_ctor != error_mark_node
	  && mem_initializer != error_mark_node)
	{
	  error ("mem-initializer for %qD follows constructor delegation",
		 TREE_PURPOSE (mem_initializer));
	  mem_initializer = error_mark_node;
	}
      /* Look for a target constructor. */
      if (mem_initializer != error_mark_node
	  && CLASS_TYPE_P (TREE_PURPOSE (mem_initializer))
	  && same_type_p (TREE_PURPOSE (mem_initializer), current_class_type))
	{
	  maybe_warn_cpp0x (CPP0X_DELEGATING_CTORS);
	  if (mem_initializer_list)
	    {
	      error ("constructor delegation follows mem-initializer for %qD",
		     TREE_PURPOSE (mem_initializer_list));
	      mem_initializer = error_mark_node;
	    }
	  target_ctor = mem_initializer;
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
      cp_token *token = cp_lexer_peek_token (parser->lexer);
      cp_lexer_set_source_position_from_token (token);
      maybe_warn_cpp0x (CPP0X_INITIALIZER_LISTS);
      expression_list = cp_parser_braced_list (parser, &expr_non_constant_p);
      CONSTRUCTOR_IS_DIRECT_INIT (expression_list) = 1;
      expression_list = build_tree_list (NULL_TREE, expression_list);
    }
  else
    {
      vec<tree, va_gc> *vec;
      vec = cp_parser_parenthesized_expression_list (parser, non_attr,
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
  if (!cp_parser_require_keyword (parser, RID_OPERATOR, RT_OPERATOR))
    return error_mark_node;
  /* And then the name of the operator itself.  */
  return cp_parser_operator (parser);
}

/* Return an identifier node for a user-defined literal operator.
   The suffix identifier is chained to the operator name identifier.  */

static tree
cp_literal_operator_id (const char* name)
{
  tree identifier;
  char *buffer = XNEWVEC (char, strlen (UDLIT_OP_ANSI_PREFIX)
			      + strlen (name) + 10);
  sprintf (buffer, UDLIT_OP_ANSI_FORMAT, name);
  identifier = get_identifier (buffer);

  return identifier;
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
  bool bad_encoding_prefix = false;

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
	    cp_parser_require (parser, CPP_CLOSE_SQUARE, RT_CLOSE_SQUARE);
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
      cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN);
      return ansi_opname (CALL_EXPR);

    case CPP_OPEN_SQUARE:
      /* Consume the `['.  */
      cp_lexer_consume_token (parser->lexer);
      /* Look for the matching `]'.  */
      cp_parser_require (parser, CPP_CLOSE_SQUARE, RT_CLOSE_SQUARE);
      return ansi_opname (ARRAY_REF);

    case CPP_WSTRING:
    case CPP_STRING16:
    case CPP_STRING32:
    case CPP_UTF8STRING:
     bad_encoding_prefix = true;
      /* Fall through.  */

    case CPP_STRING:
      if (cxx_dialect == cxx98)
	maybe_warn_cpp0x (CPP0X_USER_DEFINED_LITERALS);
      if (bad_encoding_prefix)
	{
	  error ("invalid encoding prefix in literal operator");
	  return error_mark_node;
	}
      if (TREE_STRING_LENGTH (token->u.value) > 2)
	{
	  error ("expected empty string after %<operator%> keyword");
	  return error_mark_node;
	}
      /* Consume the string.  */
      cp_lexer_consume_token (parser->lexer);
      /* Look for the suffix identifier.  */
      token = cp_lexer_peek_token (parser->lexer);
      if (token->type == CPP_NAME)
	{
	  id = cp_parser_identifier (parser);
	  if (id != error_mark_node)
	    {
	      const char *name = IDENTIFIER_POINTER (id);
	      return cp_literal_operator_id (name);
	    }
	}
      else if (token->type == CPP_KEYWORD)
	{
	  error ("unexpected keyword;"
		 " remove space between quotes and suffix identifier");
	  return error_mark_node;
	}
      else
	{
	  error ("expected suffix identifier");
	  return error_mark_node;
	}

    case CPP_WSTRING_USERDEF:
    case CPP_STRING16_USERDEF:
    case CPP_STRING32_USERDEF:
    case CPP_UTF8STRING_USERDEF:
      bad_encoding_prefix = true;
      /* Fall through.  */

    case CPP_STRING_USERDEF:
      if (cxx_dialect == cxx98)
	maybe_warn_cpp0x (CPP0X_USER_DEFINED_LITERALS);
      if (bad_encoding_prefix)
	{
	  error ("invalid encoding prefix in literal operator");
	  return error_mark_node;
	}
      {
	tree string_tree = USERDEF_LITERAL_VALUE (token->u.value);
	if (TREE_STRING_LENGTH (string_tree) > 2)
	  {
	    error ("expected empty string after %<operator%> keyword");
	    return error_mark_node;
	  }
	id = USERDEF_LITERAL_SUFFIX_ID (token->u.value);
	/* Consume the user-defined string literal.  */
	cp_lexer_consume_token (parser->lexer);
	if (id != error_mark_node)
	  {
	    const char *name = IDENTIFIER_POINTER (id);
	    return cp_literal_operator_id (name);
	  }
	else
	  return error_mark_node;
      }

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

  /* The loop below parses the template parms.  We first need to know
     the total number of template parms to be able to compute proper
     canonical types of each dependent type. So after the loop, when
     we know the total number of template parms,
     end_template_parm_list computes the proper canonical types and
     fixes up the dependent types accordingly.  */
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

  if (!parameter_declarator)
    return error_mark_node;

  /* If the parameter declaration is marked as a parameter pack, set
     *IS_PARAMETER_PACK to notify the caller. Also, unmark the
     declarator's PACK_EXPANSION_P, otherwise we'll get errors from
     grokdeclarator. */
  if (parameter_declarator->declarator
      && parameter_declarator->declarator->parameter_pack_p)
    {
      *is_parameter_pack = true;
      parameter_declarator->declarator->parameter_pack_p = false;
    }

  if (parameter_declarator->default_argument)
    {
      /* Can happen in some cases of erroneous input (c++/34892).  */
      if (cp_lexer_next_token_is (parser->lexer, CPP_ELLIPSIS))
	/* Consume the `...' for better error recovery.  */
	cp_lexer_consume_token (parser->lexer);
    }
  /* If the next token is an ellipsis, and we don't already have it
     marked as a parameter pack, then we have a parameter pack (that
     has no declarator).  */
  else if (!*is_parameter_pack
	   && cp_lexer_next_token_is (parser->lexer, CPP_ELLIPSIS)
	   && (declarator_can_be_parameter_pack
	       (parameter_declarator->declarator)))
    {
      /* Consume the `...'.  */
      cp_lexer_consume_token (parser->lexer);
      maybe_warn_variadic_templates ();
      
      *is_parameter_pack = true;
    }
  /* We might end up with a pack expansion as the type of the non-type
     template parameter, in which case this is a non-type template
     parameter pack.  */
  else if (parameter_declarator->decl_specifiers.type
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

      cp_token *start_token = cp_lexer_peek_token (parser->lexer);
      
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
  token = cp_parser_require (parser, CPP_KEYWORD, RT_CLASS_TYPENAME_TEMPLATE);
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
	cp_parser_require (parser, CPP_LESS, RT_LESS);
	/* Parse the template-parameter-list.  */
	cp_parser_template_parameter_list (parser);
	/* Look for the `>'.  */
	cp_parser_require (parser, CPP_GREATER, RT_GREATER);
	/* Look for the `class' keyword.  */
	cp_parser_require_keyword (parser, RID_CLASS, RT_CLASS);
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
		       enum tag_types tag_type,
		       bool is_declaration)
{
  int i;
  tree templ;
  tree arguments;
  tree template_id;
  cp_token_position start_of_id = 0;
  deferred_access_check *chk;
  vec<deferred_access_check, va_gc> *access_check;
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
	  FOR_EACH_VEC_ELT (*access_check, i, chk)
	    perform_or_defer_access_check (chk->binfo,
					   chk->decl,
					   chk->diag_decl,
					   tf_warning_or_error);
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
				   tag_type,
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
	      inform (next_token->location, "(if you use %<-fpermissive%> "
		      "or %<-std=c++11%>, or %<-std=gnu++11%> G++ will "
		      "accept your code)");
	      hint = true;
	    }
	}
    }
  else
    {
      /* Look for the `<' that starts the template-argument-list.  */
      if (!cp_parser_require (parser, CPP_LESS, RT_LESS))
	{
	  pop_deferring_access_checks ();
	  return error_mark_node;
	}
      /* Parse the arguments.  */
      arguments = cp_parser_enclosed_template_argument_list (parser);
    }

  /* Build a representation of the specialization.  */
  if (identifier_p (templ))
    template_id = build_min_nt_loc (next_token->location,
				    TEMPLATE_ID_EXPR,
				    templ, arguments);
  else if (DECL_TYPE_TEMPLATE_P (templ)
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
      token->u.tree_check_value = ggc_alloc_cleared_tree_check ();
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

  pop_to_parent_deferring_access_checks ();
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
			 enum tag_types tag_type,
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
				tag_type,
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
      tree scope = ovl_scope (decl);
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
  location_t loc = 0;
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
    {
      loc = cp_lexer_peek_token (parser->lexer)->location;
      cp_lexer_consume_token (parser->lexer);
    }
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

	  if (INDIRECT_REF_P (argument))
	    {
	      /* Strip the dereference temporarily.  */
	      gcc_assert (REFERENCE_REF_P (argument));
	      argument = TREE_OPERAND (argument, 0);
	    }

	  /* If we're in a template, we represent a qualified-id referring
	     to a static data member as a SCOPE_REF even if the scope isn't
	     dependent so that we can check access control later.  */
	  probe = argument;
	  if (TREE_CODE (probe) == SCOPE_REF)
	    probe = TREE_OPERAND (probe, 1);
	  if (VAR_P (probe))
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
		argument = build_x_unary_op (loc, ADDR_EXPR, argument,
					     tf_warning_or_error);
	      else
		argument = convert_from_reference (argument);
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

  timevar_push (TV_TEMPLATE_INST);

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
  cp_parser_require_keyword (parser, RID_TEMPLATE, RT_TEMPLATE);
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

      type = check_tag_decl (&decl_specifiers,
			     /*explicit_type_instantiation_p=*/true);
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
						       decl_specifiers.locations[ds_type_spec]);
      if (declarator != cp_error_declarator)
	{
	  if (decl_spec_seq_has_spec_p (&decl_specifiers, ds_inline))
	    permerror (decl_specifiers.locations[ds_inline],
		       "explicit instantiation shall not use"
		       " %<inline%> specifier");
	  if (decl_spec_seq_has_spec_p (&decl_specifiers, ds_constexpr))
	    permerror (decl_specifiers.locations[ds_constexpr],
		       "explicit instantiation shall not use"
		       " %<constexpr%> specifier");

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

  timevar_pop (TV_TEMPLATE_INST);
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
  cp_parser_require_keyword (parser, RID_TEMPLATE, RT_TEMPLATE);
  /* Look for the `<'.  */
  cp_parser_require (parser, CPP_LESS, RT_LESS);
  /* Look for the `>'.  */
  cp_parser_require (parser, CPP_GREATER, RT_GREATER);
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
					  token,
					  /*type_definition_p=*/true);
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
					  token,
					  /*type_definition_p=*/true);
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
	    decl_spec_seq_has_spec_p (decl_specs, ds_friend),
	    is_declaration));
      if (decl_specs)
	cp_parser_set_decl_spec_type (decl_specs,
				      type_spec,
				      token,
				      /*type_definition_p=*/false);
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
	  set_and_check_decl_spec_loc (decl_specs, ds, token);
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
     __underlying_type ( type-id )

   GNU Extension:

   simple-type-specifier:
     __int128
     __typeof__ unary-expression
     __typeof__ ( type-id )
     __typeof__ ( type-id ) { initializer-list , [opt] }

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
      set_and_check_decl_spec_loc (decl_specs, ds_short, token);
      type = short_integer_type_node;
      break;
    case RID_INT:
      if (decl_specs)
	decl_specs->explicit_int_p = true;
      type = integer_type_node;
      break;
    case RID_INT128:
      if (!int128_integer_type_node)
	break;
      if (decl_specs)
        decl_specs->explicit_int128_p = true;
      type = int128_integer_type_node;
      break;
    case RID_LONG:
      if (decl_specs)
	set_and_check_decl_spec_loc (decl_specs, ds_long, token);
      type = long_integer_type_node;
      break;
    case RID_SIGNED:
      set_and_check_decl_spec_loc (decl_specs, ds_signed, token);
      type = integer_type_node;
      break;
    case RID_UNSIGNED:
      set_and_check_decl_spec_loc (decl_specs, ds_unsigned, token);
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
      if (parser->auto_is_implicit_function_template_parm_p)
	{
	  type = synthesize_implicit_template_parm (parser);

	  if (current_class_type && LAMBDA_TYPE_P (current_class_type))
	    {
	      if (cxx_dialect < cxx1y)
		pedwarn (location_of (type), 0,
			 "use of %<auto%> in lambda parameter declaration "
			 "only available with "
			 "-std=c++1y or -std=gnu++1y");
	    }
	  else if (cxx_dialect < cxx1y)
	    pedwarn (location_of (type), 0,
		     "use of %<auto%> in parameter declaration "
		     "only available with "
		     "-std=c++1y or -std=gnu++1y");
	  else
	    pedwarn (location_of (type), OPT_Wpedantic,
		     "ISO C++ forbids use of %<auto%> in parameter "
		     "declaration");
	}
      else
	type = make_auto ();
      break;

    case RID_DECLTYPE:
      /* Since DR 743, decltype can either be a simple-type-specifier by
	 itself or begin a nested-name-specifier.  Parsing it will replace
	 it with a CPP_DECLTYPE, so just rewind and let the CPP_DECLTYPE
	 handling below decide what to do.  */
      cp_parser_decltype (parser);
      cp_lexer_set_token_position (parser->lexer, token);
      break;

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
				      token,
				      /*type_definition_p=*/false);

      return type;

    case RID_UNDERLYING_TYPE:
      type = cp_parser_trait_expr (parser, RID_UNDERLYING_TYPE);
      if (decl_specs)
	cp_parser_set_decl_spec_type (decl_specs, type,
				      token,
				      /*type_definition_p=*/false);

      return type;

    case RID_BASES:
    case RID_DIRECT_BASES:
      type = cp_parser_trait_expr (parser, token->keyword);
      if (decl_specs)
       cp_parser_set_decl_spec_type (decl_specs, type,
                                     token,
                                     /*type_definition_p=*/false);
      return type;
    default:
      break;
    }

  /* If token is an already-parsed decltype not followed by ::,
     it's a simple-type-specifier.  */
  if (token->type == CPP_DECLTYPE
      && cp_lexer_peek_nth_token (parser->lexer, 2)->type != CPP_SCOPE)
    {
      type = token->u.value;
      if (decl_specs)
	cp_parser_set_decl_spec_type (decl_specs, type,
				      token,
				      /*type_definition_p=*/false);
      cp_lexer_consume_token (parser->lexer);
      return type;
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
				      token,
				      /*type_definition_p=*/false);
      if (decl_specs)
	decl_specs->any_specifiers_p = true;

      /* Consume the token.  */
      cp_lexer_consume_token (parser->lexer);

      /* There is no valid C++ program where a non-template type is
	 followed by a "<".  That usually indicates that the user thought
	 that the type was a template.  */
      cp_parser_check_for_invalid_template_id (parser, type, none_type,
					       token->location);

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
					none_type,
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
	  && identifier_p (DECL_NAME (type)))
	maybe_note_name_used_in_class (DECL_NAME (type), type);
      /* If it didn't work out, we don't have a TYPE.  */
      if ((flags & CP_PARSER_FLAGS_OPTIONAL)
	  && !cp_parser_parse_definitely (parser))
	type = NULL_TREE;
      if (type && decl_specs)
	cp_parser_set_decl_spec_type (decl_specs, type,
				      token,
				      /*type_definition_p=*/false);
    }

  /* If we didn't get a type-name, issue an error message.  */
  if (!type && !(flags & CP_PARSER_FLAGS_OPTIONAL))
    {
      cp_parser_error (parser, "expected type-name");
      return error_mark_node;
    }

  if (type && type != error_mark_node)
    {
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

      /* There is no valid C++ program where a non-template type is
	 followed by a "<".  That usually indicates that the user
	 thought that the type was a template.  */
      cp_parser_check_for_invalid_template_id (parser, TREE_TYPE (type),
					       none_type,
					       token->location);
    }

  return type;
}

/* Parse a type-name.

   type-name:
     class-name
     enum-name
     typedef-name
     simple-template-id [in c++0x]

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
      if (cxx_dialect < cxx11)
	/* It must be a typedef-name or an enum-name.  */
	return cp_parser_nonclass_name (parser);

      cp_parser_parse_tentatively (parser);
      /* It is either a simple-template-id representing an
	 instantiation of an alias template...  */
      type_decl = cp_parser_template_id (parser,
					 /*template_keyword_p=*/false,
					 /*check_dependency_p=*/true,
					 none_type,
					 /*is_declaration=*/false);
      /* Note that this must be an instantiation of an alias template
	 because [temp.names]/6 says:
	 
	     A template-id that names an alias template specialization
	     is a type-name.

	 Whereas [temp.names]/7 says:
	 
	     A simple-template-id that names a class template
	     specialization is a class-name.  */
      if (type_decl != NULL_TREE
	  && TREE_CODE (type_decl) == TYPE_DECL
	  && TYPE_DECL_ALIAS_P (type_decl))
	gcc_assert (DECL_TEMPLATE_INSTANTIATION (type_decl));
      else
	cp_parser_simulate_error (parser);

      if (!cp_parser_parse_definitely (parser))
	/* ... Or a typedef-name or an enum-name.  */
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

  type_decl = strip_using_decl (type_decl);
  
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
  if (TREE_CODE (type_decl) != TYPE_DECL
      /* In Objective-C, we have the complication that class names are
	 normally type names and start declarations (eg, the
	 "NSObject" in "NSObject *object;"), but can be used in an
	 Objective-C 2.0 dot-syntax (as in "NSObject.version") which
	 is an expression.  So, a classname followed by a dot is not a
	 valid type-name.  */
      || (objc_is_class_name (TREE_TYPE (type_decl))
	  && cp_lexer_peek_token (parser->lexer)->type == CPP_DOT))
    {
      if (!cp_parser_simulate_error (parser))
	cp_parser_name_lookup_error (parser, identifier, type_decl,
				     NLE_TYPE, token->location);
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
      /* Issue a warning if the `struct' or `class' key (for C++0x scoped
	 enums) is used here.  */
      if (cp_lexer_next_token_is_keyword (parser->lexer, RID_CLASS)
	  || cp_lexer_next_token_is_keyword (parser->lexer, RID_STRUCT))
	{
	    pedwarn (input_location, 0, "elaborated-type-specifier "
		      "for a scoped enum must not use the %<%D%> keyword",
		      cp_lexer_peek_token (parser->lexer)->u.value);
	  /* Consume the `struct' or `class' and parse it anyway.  */
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
				    tag_type,
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
	 decl, then type is non existent.   */
      else if (tag_type == typename_type && TREE_CODE (decl) != TYPE_DECL)
        ; 
      else if (TREE_CODE (decl) == TYPE_DECL)
        type = check_elaborated_type_specifier (tag_type, decl,
						/*allow_template_p=*/true);
      else if (decl == error_mark_node)
	type = error_mark_node; 
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
    {
      /* Indicate whether this class was declared as a `class' or as a
	 `struct'.  */
      if (TREE_CODE (type) == RECORD_TYPE)
	CLASSTYPE_DECLARED_CLASS (type) = (tag_type == class_type);
      cp_parser_check_class_key (tag_type, type);
    }

  /* A "<" cannot follow an elaborated type specifier.  If that
     happens, the user was probably trying to form a template-id.  */
  cp_parser_check_for_invalid_template_id (parser, type, tag_type,
					   token->location);

  return type;
}

/* Parse an enum-specifier.

   enum-specifier:
     enum-head { enumerator-list [opt] }
     enum-head { enumerator-list , } [C++0x]

   enum-head:
     enum-key identifier [opt] enum-base [opt]
     enum-key nested-name-specifier identifier enum-base [opt]

   enum-key:
     enum
     enum class   [C++0x]
     enum struct  [C++0x]

   enum-base:   [C++0x]
     : type-specifier-seq

   opaque-enum-specifier:
     enum-key identifier enum-base [opt] ;

   GNU Extensions:
     enum-key attributes[opt] identifier [opt] enum-base [opt] 
       { enumerator-list [opt] }attributes[opt]
     enum-key attributes[opt] identifier [opt] enum-base [opt]
       { enumerator-list, }attributes[opt] [C++0x]

   Returns an ENUM_TYPE representing the enumeration, or NULL_TREE
   if the token stream isn't an enum-specifier after all.  */

static tree
cp_parser_enum_specifier (cp_parser* parser)
{
  tree identifier;
  tree type = NULL_TREE;
  tree prev_scope;
  tree nested_name_specifier = NULL_TREE;
  tree attributes;
  bool scoped_enum_p = false;
  bool has_underlying_type = false;
  bool nested_being_defined = false;
  bool new_value_list = false;
  bool is_new_type = false;
  bool is_anonymous = false;
  tree underlying_type = NULL_TREE;
  cp_token *type_start_token = NULL;
  bool saved_colon_corrects_to_scope_p = parser->colon_corrects_to_scope_p;

  parser->colon_corrects_to_scope_p = false;

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
      if (cxx_dialect < cxx11)
        maybe_warn_cpp0x (CPP0X_SCOPED_ENUMS);

      /* Consume the `struct' or `class' token.  */
      cp_lexer_consume_token (parser->lexer);

      scoped_enum_p = true;
    }

  attributes = cp_parser_attributes_opt (parser);

  /* Clear the qualification.  */
  parser->scope = NULL_TREE;
  parser->qualifying_scope = NULL_TREE;
  parser->object_scope = NULL_TREE;

  /* Figure out in what scope the declaration is being placed.  */
  prev_scope = current_scope ();

  type_start_token = cp_lexer_peek_token (parser->lexer);

  push_deferring_access_checks (dk_no_check);
  nested_name_specifier
      = cp_parser_nested_name_specifier_opt (parser,
					     /*typename_keyword_p=*/true,
					     /*check_dependency_p=*/false,
					     /*type_p=*/false,
					     /*is_declaration=*/false);

  if (nested_name_specifier)
    {
      tree name;

      identifier = cp_parser_identifier (parser);
      name =  cp_parser_lookup_name (parser, identifier,
				     enum_type,
				     /*is_template=*/false,
				     /*is_namespace=*/false,
				     /*check_dependency=*/true,
				     /*ambiguous_decls=*/NULL,
				     input_location);
      if (name && name != error_mark_node)
	{
	  type = TREE_TYPE (name);
	  if (TREE_CODE (type) == TYPENAME_TYPE)
	    {
	      /* Are template enums allowed in ISO? */
	      if (template_parm_scope_p ())
		pedwarn (type_start_token->location, OPT_Wpedantic,
			 "%qD is an enumeration template", name);
	      /* ignore a typename reference, for it will be solved by name
	         in start_enum.  */
	      type = NULL_TREE;
	    }
	}
      else if (nested_name_specifier == error_mark_node)
	/* We already issued an error.  */;
      else
	error_at (type_start_token->location,
		  "%qD is not an enumerator-name", identifier);
    }
  else
    {
      if (cp_lexer_next_token_is (parser->lexer, CPP_NAME))
	identifier = cp_parser_identifier (parser);
      else
	{
	  identifier = make_anon_name ();
	  is_anonymous = true;
	  if (scoped_enum_p)
	    error_at (type_start_token->location,
		      "anonymous scoped enum is not allowed");
	}
    }
  pop_deferring_access_checks ();

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

      if (cxx_dialect < cxx11)
        maybe_warn_cpp0x (CPP0X_SCOPED_ENUMS);

      has_underlying_type = true;

      /* If that didn't work, stop.  */
      if (type_specifiers.type != error_mark_node)
        {
          underlying_type = grokdeclarator (NULL, &type_specifiers, TYPENAME,
                                            /*initialized=*/0, NULL);
          if (underlying_type == error_mark_node
	      || check_for_bare_parameter_packs (underlying_type))
            underlying_type = NULL_TREE;
        }
    }

  /* Look for the `{' but don't consume it yet.  */
  if (!cp_lexer_next_token_is (parser->lexer, CPP_OPEN_BRACE))
    {
      if (cxx_dialect < cxx11 || (!scoped_enum_p && !underlying_type))
	{
	  cp_parser_error (parser, "expected %<{%>");
	  if (has_underlying_type)
	    {
	      type = NULL_TREE;
	      goto out;
	    }
	}
      /* An opaque-enum-specifier must have a ';' here.  */
      if ((scoped_enum_p || underlying_type)
	  && cp_lexer_next_token_is_not (parser->lexer, CPP_SEMICOLON))
	{
	  cp_parser_error (parser, "expected %<;%> or %<{%>");
	  if (has_underlying_type)
	    {
	      type = NULL_TREE;
	      goto out;
	    }
	}
    }

  if (!has_underlying_type && !cp_parser_parse_definitely (parser))
    return NULL_TREE;

  if (nested_name_specifier)
    {
      if (CLASS_TYPE_P (nested_name_specifier))
	{
	  nested_being_defined = TYPE_BEING_DEFINED (nested_name_specifier);
	  TYPE_BEING_DEFINED (nested_name_specifier) = 1;
	  push_scope (nested_name_specifier);
	}
      else if (TREE_CODE (nested_name_specifier) == NAMESPACE_DECL)
	{
	  push_nested_namespace (nested_name_specifier);
	}
    }

  /* Issue an error message if type-definitions are forbidden here.  */
  if (!cp_parser_check_type_definition (parser))
    type = error_mark_node;
  else
    /* Create the new type.  We do this before consuming the opening
       brace so the enum will be recorded as being on the line of its
       tag (or the 'enum' keyword, if there is no tag).  */
    type = start_enum (identifier, type, underlying_type,
		       scoped_enum_p, &is_new_type);

  /* If the next token is not '{' it is an opaque-enum-specifier or an
     elaborated-type-specifier.  */
  if (cp_lexer_next_token_is (parser->lexer, CPP_OPEN_BRACE))
    {
      timevar_push (TV_PARSE_ENUM);
      if (nested_name_specifier
	  && nested_name_specifier != error_mark_node)
	{
	  /* The following catches invalid code such as:
	     enum class S<int>::E { A, B, C }; */
	  if (!processing_specialization
	      && CLASS_TYPE_P (nested_name_specifier)
	      && CLASSTYPE_USE_TEMPLATE (nested_name_specifier))
	    error_at (type_start_token->location, "cannot add an enumerator "
		      "list to a template instantiation");

	  if (TREE_CODE (nested_name_specifier) == TYPENAME_TYPE)
	    {
	      error_at (type_start_token->location,
			"%<%T::%E%> has not been declared",
			TYPE_CONTEXT (nested_name_specifier),
			nested_name_specifier);
	      type = error_mark_node;
	    }
	  /* If that scope does not contain the scope in which the
	     class was originally declared, the program is invalid.  */
	  else if (prev_scope && !is_ancestor (prev_scope,
					       nested_name_specifier))
	    {
	      if (at_namespace_scope_p ())
		error_at (type_start_token->location,
			  "declaration of %qD in namespace %qD which does not "
			  "enclose %qD",
			  type, prev_scope, nested_name_specifier);
	      else
		error_at (type_start_token->location,
			  "declaration of %qD in %qD which does not "
			  "enclose %qD",
			  type, prev_scope, nested_name_specifier);
	      type = error_mark_node;
	    }
	}

      if (scoped_enum_p)
	begin_scope (sk_scoped_enum, type);

      /* Consume the opening brace.  */
      cp_lexer_consume_token (parser->lexer);

      if (type == error_mark_node)
	; /* Nothing to add */
      else if (OPAQUE_ENUM_P (type)
	       || (cxx_dialect > cxx98 && processing_specialization))
	{
	  new_value_list = true;
	  SET_OPAQUE_ENUM_P (type, false);
	  DECL_SOURCE_LOCATION (TYPE_NAME (type)) = type_start_token->location;
	}
      else
	{
	  error_at (type_start_token->location, "multiple definition of %q#T", type);
	  error_at (DECL_SOURCE_LOCATION (TYPE_MAIN_DECL (type)),
		    "previous definition here");
	  type = error_mark_node;
	}

      if (type == error_mark_node)
	cp_parser_skip_to_end_of_block_or_statement (parser);
      /* If the next token is not '}', then there are some enumerators.  */
      else if (cp_lexer_next_token_is (parser->lexer, CPP_CLOSE_BRACE))
	{
	  if (is_anonymous && !scoped_enum_p)
	    pedwarn (type_start_token->location, OPT_Wpedantic,
		     "ISO C++ forbids empty anonymous enum");
	}
      else
	cp_parser_enumerator_list (parser, type);

      /* Consume the final '}'.  */
      cp_parser_require (parser, CPP_CLOSE_BRACE, RT_CLOSE_BRACE);

      if (scoped_enum_p)
	finish_scope ();
      timevar_pop (TV_PARSE_ENUM);
    }
  else
    {
      /* If a ';' follows, then it is an opaque-enum-specifier
	and additional restrictions apply.  */
      if (cp_lexer_next_token_is (parser->lexer, CPP_SEMICOLON))
	{
	  if (is_anonymous)
	    error_at (type_start_token->location,
		      "opaque-enum-specifier without name");
	  else if (nested_name_specifier)
	    error_at (type_start_token->location,
		      "opaque-enum-specifier must use a simple identifier");
	}
    }

  /* Look for trailing attributes to apply to this enumeration, and
     apply them if appropriate.  */
  if (cp_parser_allow_gnu_extensions_p (parser))
    {
      tree trailing_attr = cp_parser_gnu_attributes_opt (parser);
      trailing_attr = chainon (trailing_attr, attributes);
      cplus_decl_attributes (&type,
			     trailing_attr,
			     (int) ATTR_FLAG_TYPE_IN_PLACE);
    }

  /* Finish up the enumeration.  */
  if (type != error_mark_node)
    {
      if (new_value_list)
	finish_enum_value_list (type);
      if (is_new_type)
	finish_enum (type);
    }

  if (nested_name_specifier)
    {
      if (CLASS_TYPE_P (nested_name_specifier))
	{
	  TYPE_BEING_DEFINED (nested_name_specifier) = nested_being_defined;
	  pop_scope (nested_name_specifier);
	}
      else if (TREE_CODE (nested_name_specifier) == NAMESPACE_DECL)
	{
	  pop_nested_namespace (nested_name_specifier);
	}
    }
 out:
  parser->colon_corrects_to_scope_p = saved_colon_corrects_to_scope_p;
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
	  if (cxx_dialect < cxx11 && !in_system_header_at (input_location))
	    pedwarn (input_location, OPT_Wpedantic,
                     "comma at end of enumerator list");
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
  location_t loc;

  /* Save the input location because we are interested in the location
     of the identifier and not the location of the explicit value.  */
  loc = cp_lexer_peek_token (parser->lexer)->location;

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

  /* integral_constant_value will pull out this expression, so make sure
     it's folded as appropriate.  */
  value = fold_non_dependent_expr (value);

  /* Create the enumerator.  */
  build_enumerator (identifier, value, type, loc);
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

  cp_ensure_no_omp_declare_simd (parser);
  if (cp_lexer_next_token_is_keyword (parser->lexer, RID_INLINE))
    {
      maybe_warn_cpp0x (CPP0X_INLINE_NAMESPACES);
      is_inline = true;
      cp_lexer_consume_token (parser->lexer);
    }
  else
    is_inline = false;

  /* Look for the `namespace' keyword.  */
  cp_parser_require_keyword (parser, RID_NAMESPACE, RT_NAMESPACE);

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
  cp_parser_require (parser, CPP_OPEN_BRACE, RT_OPEN_BRACE);
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

  if (has_visibility)
    pop_visibility (1);

  /* Finish the namespace.  */
  pop_namespace ();
  /* Look for the final `}'.  */
  cp_parser_require (parser, CPP_CLOSE_BRACE, RT_CLOSE_BRACE);
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
  cp_parser_require_keyword (parser, RID_NAMESPACE, RT_NAMESPACE);
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
  cp_parser_require (parser, CPP_EQ, RT_EQ);
  /* Look for the qualified-namespace-specifier.  */
  namespace_specifier
    = cp_parser_qualified_namespace_specifier (parser);
  /* Look for the `;' token.  */
  cp_parser_require (parser, CPP_SEMICOLON, RT_SEMICOLON);

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
  int oldcount = errorcount;
  cp_token *diag_token = NULL;

  if (access_declaration_p)
    {
      diag_token = cp_lexer_peek_token (parser->lexer);
      cp_parser_parse_tentatively (parser);
    }
  else
    {
      /* Look for the `using' keyword.  */
      cp_parser_require_keyword (parser, RID_USING, RT_USING);
      
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
    {
      qscope = cp_parser_nested_name_specifier (parser, typename_p,
						/*check_dependency_p=*/true,
						/*type_p=*/false,
						/*is_declaration=*/true);
      if (!qscope && !cp_parser_uncommitted_to_tentative_parse_p (parser))
	{
	  cp_parser_skip_to_end_of_block_or_statement (parser);
	  return false;
	}
    }
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
  else if (!identifier_p (identifier)
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

	  if (decl && typename_p)
	    USING_DECL_TYPENAME_P (decl) = 1;

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
					 decl, NLE_NULL,
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
  cp_parser_require (parser, CPP_SEMICOLON, RT_SEMICOLON);

  if (access_declaration_p && errorcount == oldcount)
    warning_at (diag_token->location, OPT_Wdeprecated,
		"access declarations are deprecated "
		"in favour of using-declarations; "
		"suggestion: add the %<using%> keyword");

  return true;
}

/* Parse an alias-declaration.

   alias-declaration:
     using identifier attribute-specifier-seq [opt] = type-id  */

static tree
cp_parser_alias_declaration (cp_parser* parser)
{
  tree id, type, decl, pushed_scope = NULL_TREE, attributes;
  location_t id_location;
  cp_declarator *declarator;
  cp_decl_specifier_seq decl_specs;
  bool member_p;
  const char *saved_message = NULL;

  /* Look for the `using' keyword.  */
  cp_token *using_token
    = cp_parser_require_keyword (parser, RID_USING, RT_USING);
  if (using_token == NULL)
    return error_mark_node;

  id_location = cp_lexer_peek_token (parser->lexer)->location;
  id = cp_parser_identifier (parser);
  if (id == error_mark_node)
    return error_mark_node;

  cp_token *attrs_token = cp_lexer_peek_token (parser->lexer);
  attributes = cp_parser_attributes_opt (parser);
  if (attributes == error_mark_node)
    return error_mark_node;

  cp_parser_require (parser, CPP_EQ, RT_EQ);

  if (cp_parser_error_occurred (parser))
    return error_mark_node;

  cp_parser_commit_to_tentative_parse (parser);

  /* Now we are going to parse the type-id of the declaration.  */

  /*
    [dcl.type]/3 says:

	"A type-specifier-seq shall not define a class or enumeration
	 unless it appears in the type-id of an alias-declaration (7.1.3) that
	 is not the declaration of a template-declaration."

    In other words, if we currently are in an alias template, the
    type-id should not define a type.

    So let's set parser->type_definition_forbidden_message in that
    case; cp_parser_check_type_definition (called by
    cp_parser_class_specifier) will then emit an error if a type is
    defined in the type-id.  */
  if (parser->num_template_parameter_lists)
    {
      saved_message = parser->type_definition_forbidden_message;
      parser->type_definition_forbidden_message =
	G_("types may not be defined in alias template declarations");
    }

  type = cp_parser_type_id (parser);

  /* Restore the error message if need be.  */
  if (parser->num_template_parameter_lists)
    parser->type_definition_forbidden_message = saved_message;

  if (type == error_mark_node)
    {
      cp_parser_skip_to_end_of_block_or_statement (parser);
      return error_mark_node;
    }

  cp_parser_require (parser, CPP_SEMICOLON, RT_SEMICOLON);

  if (cp_parser_error_occurred (parser))
    {
      cp_parser_skip_to_end_of_block_or_statement (parser);
      return error_mark_node;
    }

  /* A typedef-name can also be introduced by an alias-declaration. The
     identifier following the using keyword becomes a typedef-name. It has
     the same semantics as if it were introduced by the typedef
     specifier. In particular, it does not define a new type and it shall
     not appear in the type-id.  */

  clear_decl_specs (&decl_specs);
  decl_specs.type = type;
  if (attributes != NULL_TREE)
    {
      decl_specs.attributes = attributes;
      set_and_check_decl_spec_loc (&decl_specs,
				   ds_attribute,
				   attrs_token);
    }
  set_and_check_decl_spec_loc (&decl_specs,
			       ds_typedef,
			       using_token);
  set_and_check_decl_spec_loc (&decl_specs,
			       ds_alias,
			       using_token);

  declarator = make_id_declarator (NULL_TREE, id, sfk_none);
  declarator->id_loc = id_location;

  member_p = at_class_scope_p ();
  if (member_p)
    decl = grokfield (declarator, &decl_specs, NULL_TREE, false,
		      NULL_TREE, attributes);
  else
    decl = start_decl (declarator, &decl_specs, 0,
		       attributes, NULL_TREE, &pushed_scope);
  if (decl == error_mark_node)
    return decl;

  cp_finish_decl (decl, NULL_TREE, 0, NULL_TREE, 0);

  if (pushed_scope)
    pop_scope (pushed_scope);

  /* If decl is a template, return its TEMPLATE_DECL so that it gets
     added into the symbol table; otherwise, return the TYPE_DECL.  */
  if (DECL_LANG_SPECIFIC (decl)
      && DECL_TEMPLATE_INFO (decl)
      && PRIMARY_TEMPLATE_P (DECL_TI_TEMPLATE (decl)))
    {
      decl = DECL_TI_TEMPLATE (decl);
      if (member_p)
	check_member_template (decl);
    }

  return decl;
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
  cp_parser_require_keyword (parser, RID_USING, RT_USING);
  /* And the `namespace' keyword.  */
  cp_parser_require_keyword (parser, RID_NAMESPACE, RT_NAMESPACE);
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
  cp_parser_require (parser, CPP_SEMICOLON, RT_SEMICOLON);
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
  required_token missing = RT_NONE;

  /* Look for the `asm' keyword.  */
  cp_parser_require_keyword (parser, RID_ASM, RT_ASM);
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
  if (!cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN))
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
	missing = clobbers_p ? RT_COLON : RT_COLON_SCOPE;
    }
  else if (goto_p)
    missing = RT_COLON_SCOPE;

  /* Look for the closing `)'.  */
  if (!cp_parser_require (parser, missing ? CPP_COLON : CPP_CLOSE_PAREN,
			  missing ? missing : RT_CLOSE_PAREN))
    cp_parser_skip_to_closing_parenthesis (parser, true, false,
					   /*consume_paren=*/true);
  cp_parser_require (parser, CPP_SEMICOLON, RT_SEMICOLON);

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
	add_asm_node (string);
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

   TM Extension:

   function-definition:
     decl-specifier-seq [opt] declarator function-transaction-block

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
   is FALSE.

   If MAYBE_RANGE_FOR_DECL is not NULL, the pointed tree will be set to the
   parsed declaration if it is an uninitialized single declarator not followed
   by a `;', or to error_mark_node otherwise. Either way, the trailing `;',
   if present, will not be consumed.  If returned, this declarator will be
   created with SD_INITIALIZED but will not call cp_finish_decl.  */

static tree
cp_parser_init_declarator (cp_parser* parser,
			   cp_decl_specifier_seq *decl_specifiers,
			   vec<deferred_access_check, va_gc> *checks,
			   bool function_definition_allowed_p,
			   bool member_p,
			   int declares_class_or_enum,
			   bool* function_definition_p,
			   tree* maybe_range_for_decl)
{
  cp_token *token = NULL, *asm_spec_start_token = NULL,
           *attributes_start_token = NULL;
  cp_declarator *declarator;
  tree prefix_attributes;
  tree attributes = NULL;
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
  tree pushed_scope = NULL_TREE;
  bool range_for_decl_p = false;
  bool saved_default_arg_ok_p = parser->default_arg_ok_p;

  /* Gather the attributes that were provided with the
     decl-specifiers.  */
  prefix_attributes = decl_specifiers->attributes;

  /* Assume that this is not the declarator for a function
     definition.  */
  if (function_definition_p)
    *function_definition_p = false;

  /* Default arguments are only permitted for function parameters.  */
  if (decl_spec_seq_has_spec_p (decl_specifiers, ds_typedef))
    parser->default_arg_ok_p = false;

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
			    member_p);
  /* Gather up the deferred checks.  */
  stop_deferring_access_checks ();

  parser->default_arg_ok_p = saved_default_arg_ok_p;

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
						   decl_specifiers->locations[ds_type_spec]);

  /* Figure out what scope the entity declared by the DECLARATOR is
     located in.  `grokdeclarator' sometimes changes the scope, so
     we compute it now.  */
  scope = get_scope_of_declarator (declarator);

  /* Perform any lookups in the declared type which were thought to be
     dependent, but are not in the scope of the declarator.  */
  decl_specifiers->type
    = maybe_update_decl_type (decl_specifiers->type, scope);

  /* If we're allowing GNU extensions, look for an
     asm-specification.  */
  if (cp_parser_allow_gnu_extensions_p (parser))
    {
      /* Look for an asm-specification.  */
      asm_spec_start_token = cp_lexer_peek_token (parser->lexer);
      asm_specification = cp_parser_asm_specification_opt (parser);
    }
  else
    asm_specification = NULL_TREE;

  /* Look for attributes.  */
  attributes_start_token = cp_lexer_peek_token (parser->lexer);
  attributes = cp_parser_attributes_opt (parser);

  /* Peek at the next token.  */
  token = cp_lexer_peek_token (parser->lexer);

  if (function_declarator_p (declarator))
    {
      /* Check to see if the token indicates the start of a
	 function-definition.  */
      if (cp_parser_token_starts_function_definition_p (token))
	{
	  if (!function_definition_allowed_p)
	    {
	      /* If a function-definition should not appear here, issue an
		 error message.  */
	      cp_parser_error (parser,
			       "a function-definition is not allowed here");
	      return error_mark_node;
	    }

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
		      "attributes are not allowed "
		      "on a function-definition");
	  /* This is a function-definition.  */
	  *function_definition_p = true;

	  /* Parse the function definition.  */
	  if (member_p)
	    decl = cp_parser_save_member_function_body (parser,
							decl_specifiers,
							declarator,
							prefix_attributes);
	  else
	    decl =
	      (cp_parser_function_definition_from_specifiers_and_declarator
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
      if (maybe_range_for_decl)
	*maybe_range_for_decl = error_mark_node;

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
	  if (maybe_range_for_decl && *maybe_range_for_decl != error_mark_node)
	    range_for_decl_p = true;
	  else
	    {
	      cp_parser_error (parser, "expected initializer");
	      return error_mark_node;
	    }
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
			 range_for_decl_p? SD_INITIALIZED : is_initialized,
			 attributes, prefix_attributes, &pushed_scope);
      cp_finalize_omp_declare_simd (parser, decl);
      /* Adjust location of decl if declarator->id_loc is more appropriate:
	 set, and decl wasn't merged with another decl, in which case its
	 location would be different from input_location, and more accurate.  */
      if (DECL_P (decl)
	  && declarator->id_loc != UNKNOWN_LOCATION
	  && DECL_SOURCE_LOCATION (decl) == input_location)
	DECL_SOURCE_LOCATION (decl) = declarator->id_loc;
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
      perform_deferred_access_checks (tf_warning_or_error);

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
	     member templates.  The former involves deferring
	     parsing of the initializer until end of class as with default
	     arguments.  So right here we only handle the latter.  */
	  if (!member_p && processing_template_decl)
	    start_lambda_scope (decl);
	  initializer = cp_parser_initializer (parser,
					       &is_direct_init,
					       &is_non_constant_init);
	  if (!member_p && processing_template_decl)
	    finish_lambda_scope ();
	  if (initializer == error_mark_node)
	    cp_parser_skip_to_end_of_statement (parser);
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

  /* A non-template declaration involving a function parameter list containing
     an implicit template parameter will have been made into a template.  If it
     turns out that the resulting declaration is not an actual function then
     finish the template declaration here.  An error message will already have
     been issued.  */
  if (parser->fully_implicit_function_template_p)
    if (!function_declarator_p (declarator))
      finish_fully_implicit_template (parser, /*member_decl_opt=*/0);

  /* For an in-class declaration, use `grokfield' to create the
     declaration.  */
  if (member_p)
    {
      if (pushed_scope)
	{
	  pop_scope (pushed_scope);
	  pushed_scope = NULL_TREE;
	}
      decl = grokfield (declarator, decl_specifiers,
			initializer, !is_non_constant_init,
			/*asmspec=*/NULL_TREE,
			chainon (attributes, prefix_attributes));
      if (decl && TREE_CODE (decl) == FUNCTION_DECL)
	cp_parser_save_default_args (parser, decl);
      cp_finalize_omp_declare_simd (parser, decl);
    }

  /* Finish processing the declaration.  But, skip member
     declarations.  */
  if (!member_p && decl && decl != error_mark_node && !range_for_decl_p)
    {
      cp_finish_decl (decl,
		      initializer, !is_non_constant_init,
		      asm_specification,
		      /* If the initializer is in parentheses, then this is
			 a direct-initialization, which means that an
			 `explicit' constructor is OK.  Otherwise, an
			 `explicit' constructor cannot be used.  */
		      ((is_direct_init || !is_initialized)
		       ? LOOKUP_NORMAL : LOOKUP_IMPLICIT));
    }
  else if ((cxx_dialect != cxx98) && friend_p
	   && decl && TREE_CODE (decl) == FUNCTION_DECL)
    /* Core issue #226 (C++0x only): A default template-argument
       shall not be specified in a friend class template
       declaration. */
    check_default_tmpl_args (decl, current_template_parms, /*is_primary=*/true, 
                             /*is_partial=*/false, /*is_friend_decl=*/1);

  if (!friend_p && pushed_scope)
    pop_scope (pushed_scope);

  if (function_declarator_p (declarator)
      && parser->fully_implicit_function_template_p)
    {
      if (member_p)
	decl = finish_fully_implicit_template (parser, decl);
      else
	finish_fully_implicit_template (parser, /*member_decl_opt=*/0);
    }

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
  tree gnu_attributes = NULL_TREE, std_attributes = NULL_TREE;

  /* Assume this is not a constructor, destructor, or type-conversion
     operator.  */
  if (ctor_dtor_or_conv_p)
    *ctor_dtor_or_conv_p = 0;

  if (cp_parser_allow_gnu_extensions_p (parser))
    gnu_attributes = cp_parser_gnu_attributes_opt (parser);

  /* Check for the ptr-operator production.  */
  cp_parser_parse_tentatively (parser);
  /* Parse the ptr-operator.  */
  code = cp_parser_ptr_operator (parser,
				 &class_type,
				 &cv_quals,
				 &std_attributes);

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
	(code, class_type, cv_quals, declarator, std_attributes);
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

  if (gnu_attributes && declarator && declarator != cp_error_declarator)
    declarator->attributes = gnu_attributes;
  return declarator;
}

/* Parse a direct-declarator or direct-abstract-declarator.

   direct-declarator:
     declarator-id
     direct-declarator ( parameter-declaration-clause )
       cv-qualifier-seq [opt]
       ref-qualifier [opt]
       exception-specification [opt]
     direct-declarator [ constant-expression [opt] ]
     ( declarator )

   direct-abstract-declarator:
     direct-abstract-declarator [opt]
       ( parameter-declaration-clause )
       cv-qualifier-seq [opt]
       ref-qualifier [opt]
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
	      bool is_declarator = false;

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

	      begin_scope (sk_function_parms, NULL_TREE);

	      /* Parse the parameter-declaration-clause.  */
	      params = cp_parser_parameter_declaration_clause (parser);

	      /* Consume the `)'.  */
	      cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN);

	      /* If all went well, parse the cv-qualifier-seq,
		 ref-qualifier and the exception-specification.  */
	      if (member_p || cp_parser_parse_definitely (parser))
		{
		  cp_cv_quals cv_quals;
		  cp_virt_specifiers virt_specifiers;
		  cp_ref_qualifier ref_qual;
		  tree exception_specification;
		  tree late_return;
		  tree attrs;
		  bool memfn = (member_p || (pushed_scope
					     && CLASS_TYPE_P (pushed_scope)));

		  is_declarator = true;

		  if (ctor_dtor_or_conv_p)
		    *ctor_dtor_or_conv_p = *ctor_dtor_or_conv_p < 0;
		  first = false;

		  /* Parse the cv-qualifier-seq.  */
		  cv_quals = cp_parser_cv_qualifier_seq_opt (parser);
		  /* Parse the ref-qualifier. */
		  ref_qual = cp_parser_ref_qualifier_opt (parser);
		  /* And the exception-specification.  */
		  exception_specification
		    = cp_parser_exception_specification_opt (parser);

		  attrs = cp_parser_std_attribute_spec_seq (parser);

		  /* In here, we handle cases where attribute is used after
		     the function declaration.  For example:
		     void func (int x) __attribute__((vector(..)));  */
		  if (flag_cilkplus
		      && cp_next_tokens_can_be_gnu_attribute_p (parser))
		    {
		      cp_parser_parse_tentatively (parser);
		      tree attr = cp_parser_gnu_attributes_opt (parser);
		      if (cp_lexer_next_token_is_not (parser->lexer,
						      CPP_SEMICOLON)
			  && cp_lexer_next_token_is_not (parser->lexer,
							 CPP_OPEN_BRACE))
			cp_parser_abort_tentative_parse (parser);
		      else if (!cp_parser_parse_definitely (parser))
			;
		      else
			attrs = chainon (attr, attrs);
		    }
		  late_return = (cp_parser_late_return_type_opt
				 (parser, declarator,
				  memfn ? cv_quals : -1));


		  /* Parse the virt-specifier-seq.  */
		  virt_specifiers = cp_parser_virt_specifier_seq_opt (parser);

		  /* Create the function-declarator.  */
		  declarator = make_call_declarator (declarator,
						     params,
						     cv_quals,
						     virt_specifiers,
						     ref_qual,
						     exception_specification,
						     late_return);
		  declarator->std_attributes = attrs;
		  /* Any subsequent parameter lists are to do with
		     return type, so are not those of the declared
		     function.  */
		  parser->default_arg_ok_p = false;
		}

	      /* Remove the function parms from scope.  */
	      pop_bindings_and_leave_scope ();

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
	      if (!cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN))
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
	       && token->type == CPP_OPEN_SQUARE
	       && !cp_next_tokens_can_be_attribute_p (parser))
	{
	  /* Parse an array-declarator.  */
	  tree bounds, attrs;

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
		/* OK */;
	      else if (error_operand_p (bounds))
		/* Already gave an error.  */;
	      else if (!parser->in_function_body
		       || current_binding_level->kind == sk_function_parms)
		{
		  /* Normally, the array bound must be an integral constant
		     expression.  However, as an extension, we allow VLAs
		     in function scopes as long as they aren't part of a
		     parameter declaration.  */
		  cp_parser_error (parser,
				   "array bound is not an integer constant");
		  bounds = error_mark_node;
		}
	      else if (processing_template_decl
		       && !type_dependent_expression_p (bounds))
		{
		  /* Remember this wasn't a constant-expression.  */
		  bounds = build_nop (TREE_TYPE (bounds), bounds);
		  TREE_SIDE_EFFECTS (bounds) = 1;
		}
	    }
	  else
	    bounds = NULL_TREE;
	  /* Look for the closing `]'.  */
	  if (!cp_parser_require (parser, CPP_CLOSE_SQUARE, RT_CLOSE_SQUARE))
	    {
	      declarator = cp_error_declarator;
	      break;
	    }

	  attrs = cp_parser_std_attribute_spec_seq (parser);
	  declarator = make_array_declarator (declarator, bounds);
	  declarator->std_attributes = attrs;
	}
      else if (first && dcl_kind != CP_PARSER_DECLARATOR_ABSTRACT)
	{
	  {
	    tree qualifying_scope;
	    tree unqualified_name;
	    tree attrs;
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
			     || (!identifier_p (unqualified_name))))
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

	    attrs = cp_parser_std_attribute_spec_seq (parser);

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
	    declarator->std_attributes = attrs;
	    declarator->id_loc = token->location;
	    declarator->parameter_pack_p = pack_expansion_p;

	    if (pack_expansion_p)
	      maybe_warn_variadic_templates ();
	  }

	handle_declarator:;
	  scope = get_scope_of_declarator (declarator);
	  if (scope)
	    {
	      /* Any names that appear after the declarator-id for a
		 member are looked up in the containing scope.  */
	      if (at_function_scope_p ())
		{
		  /* But declarations with qualified-ids can't appear in a
		     function.  */
		  cp_parser_error (parser, "qualified-id in declaration");
		  declarator = cp_error_declarator;
		  break;
		}
	      pushed_scope = push_scope (scope);
	    }
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
     * attribute-specifier-seq [opt] cv-qualifier-seq [opt] (C++11)
     * cv-qualifier-seq [opt]
     &
     :: [opt] nested-name-specifier * cv-qualifier-seq [opt]
     nested-name-specifier * attribute-specifier-seq [opt] cv-qualifier-seq [opt] (C++11)

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
			cp_cv_quals *cv_quals,
			tree *attributes)
{
  enum tree_code code = ERROR_MARK;
  cp_token *token;
  tree attrs = NULL_TREE;

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

      attrs = cp_parser_std_attribute_spec_seq (parser);
      if (attributes != NULL)
	*attributes = attrs;
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
	  && cp_parser_require (parser, CPP_MULT, RT_MULT))
	{
	  /* Indicate that the `*' operator was used.  */
	  code = INDIRECT_REF;

	  if (TREE_CODE (parser->scope) == NAMESPACE_DECL)
	    error_at (token->location, "%qD is a namespace", parser->scope);
	  else if (TREE_CODE (parser->scope) == ENUMERAL_TYPE)
	    error_at (token->location, "cannot form pointer to member of "
		      "non-class %q#T", parser->scope);
	  else
	    {
	      /* The type of which the member is a member is given by the
		 current SCOPE.  */
	      *type = parser->scope;
	      /* The next name will not be qualified.  */
	      parser->scope = NULL_TREE;
	      parser->qualifying_scope = NULL_TREE;
	      parser->object_scope = NULL_TREE;
	      /* Look for optional c++11 attributes.  */
	      attrs = cp_parser_std_attribute_spec_seq (parser);
	      if (attributes != NULL)
		*attributes = attrs;
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

/* Parse an (optional) ref-qualifier

   ref-qualifier:
     &
     &&

   Returns cp_ref_qualifier representing ref-qualifier. */

static cp_ref_qualifier
cp_parser_ref_qualifier_opt (cp_parser* parser)
{
  cp_ref_qualifier ref_qual = REF_QUAL_NONE;

  /* Don't try to parse bitwise '&' as a ref-qualifier (c++/57532).  */
  if (cxx_dialect < cxx11 && cp_parser_parsing_tentatively (parser))
    return ref_qual;

  while (true)
    {
      cp_ref_qualifier curr_ref_qual = REF_QUAL_NONE;
      cp_token *token = cp_lexer_peek_token (parser->lexer);

      switch (token->type)
	{
	case CPP_AND:
	  curr_ref_qual = REF_QUAL_LVALUE;
	  break;

	case CPP_AND_AND:
	  curr_ref_qual = REF_QUAL_RVALUE;
	  break;

	default:
	  curr_ref_qual = REF_QUAL_NONE;
	  break;
	}

      if (!curr_ref_qual)
	break;
      else if (ref_qual)
	{
	  error_at (token->location, "multiple ref-qualifiers");
	  cp_lexer_purge_token (parser->lexer);
	}
      else
	{
	  ref_qual = curr_ref_qual;
	  cp_lexer_consume_token (parser->lexer);
	}
    }

  return ref_qual;
}

/* Parse an (optional) virt-specifier-seq.

   virt-specifier-seq:
     virt-specifier virt-specifier-seq [opt]

   virt-specifier:
     override
     final

   Returns a bitmask representing the virt-specifiers.  */

static cp_virt_specifiers
cp_parser_virt_specifier_seq_opt (cp_parser* parser)
{
  cp_virt_specifiers virt_specifiers = VIRT_SPEC_UNSPECIFIED;

  while (true)
    {
      cp_token *token;
      cp_virt_specifiers virt_specifier;

      /* Peek at the next token.  */
      token = cp_lexer_peek_token (parser->lexer);
      /* See if it's a virt-specifier-qualifier.  */
      if (token->type != CPP_NAME)
        break;
      if (!strcmp (IDENTIFIER_POINTER(token->u.value), "override"))
        {
          maybe_warn_cpp0x (CPP0X_OVERRIDE_CONTROLS);
          virt_specifier = VIRT_SPEC_OVERRIDE;
        }
      else if (!strcmp (IDENTIFIER_POINTER(token->u.value), "final"))
        {
          maybe_warn_cpp0x (CPP0X_OVERRIDE_CONTROLS);
          virt_specifier = VIRT_SPEC_FINAL;
        }
      else if (!strcmp (IDENTIFIER_POINTER(token->u.value), "__final"))
        {
          virt_specifier = VIRT_SPEC_FINAL;
        }
      else
	break;

      if (virt_specifiers & virt_specifier)
	{
	  error_at (token->location, "duplicate virt-specifier");
	  cp_lexer_purge_token (parser->lexer);
	}
      else
	{
	  cp_lexer_consume_token (parser->lexer);
	  virt_specifiers |= virt_specifier;
	}
    }
  return virt_specifiers;
}

/* Used by handling of trailing-return-types and NSDMI, in which 'this'
   is in scope even though it isn't real.  */

static void
inject_this_parameter (tree ctype, cp_cv_quals quals)
{
  tree this_parm;

  if (current_class_ptr)
    {
      /* We don't clear this between NSDMIs.  Is it already what we want?  */
      tree type = TREE_TYPE (TREE_TYPE (current_class_ptr));
      if (same_type_ignoring_top_level_qualifiers_p (ctype, type)
	  && cp_type_quals (type) == quals)
	return;
    }

  this_parm = build_this_parm (ctype, quals);
  /* Clear this first to avoid shortcut in cp_build_indirect_ref.  */
  current_class_ptr = NULL_TREE;
  current_class_ref
    = cp_build_indirect_ref (this_parm, RO_NULL, tf_warning_or_error);
  current_class_ptr = this_parm;
}

/* Return true iff our current scope is a non-static data member
   initializer.  */

bool
parsing_nsdmi (void)
{
  /* We recognize NSDMI context by the context-less 'this' pointer set up
     by the function above.  */
  if (current_class_ptr && DECL_CONTEXT (current_class_ptr) == NULL_TREE)
    return true;
  return false;
}

/* Parse a late-specified return type, if any.  This is not a separate
   non-terminal, but part of a function declarator, which looks like

   -> trailing-type-specifier-seq abstract-declarator(opt)

   Returns the type indicated by the type-id.

   In addition to this this parses any queued up omp declare simd
   clauses and Cilk Plus SIMD-enabled function's vector attributes.

   QUALS is either a bitmask of cv_qualifiers or -1 for a non-member
   function.  */

static tree
cp_parser_late_return_type_opt (cp_parser* parser, cp_declarator *declarator,
				cp_cv_quals quals)
{
  cp_token *token;
  tree type = NULL_TREE;
  bool declare_simd_p = (parser->omp_declare_simd
			 && declarator
			 && declarator->kind == cdk_id);

  bool cilk_simd_fn_vector_p = (parser->cilk_simd_fn_info 
				&& declarator && declarator->kind == cdk_id);
  
  /* Peek at the next token.  */
  token = cp_lexer_peek_token (parser->lexer);
  /* A late-specified return type is indicated by an initial '->'. */
  if (token->type != CPP_DEREF && !(declare_simd_p || cilk_simd_fn_vector_p))
    return NULL_TREE;

  tree save_ccp = current_class_ptr;
  tree save_ccr = current_class_ref;
  if (quals >= 0)
    {
      /* DR 1207: 'this' is in scope in the trailing return type.  */
      inject_this_parameter (current_class_type, quals);
    }

  if (token->type == CPP_DEREF)
    {
      /* Consume the ->.  */
      cp_lexer_consume_token (parser->lexer);

      type = cp_parser_trailing_type_id (parser);
    }

  if (cilk_simd_fn_vector_p)
    declarator->std_attributes
      = cp_parser_late_parsing_cilk_simd_fn_info (parser,
						  declarator->std_attributes);
  if (declare_simd_p)
    declarator->std_attributes
      = cp_parser_late_parsing_omp_declare_simd (parser,
						 declarator->std_attributes);

  if (quals >= 0)
    {
      current_class_ptr = save_ccp;
      current_class_ref = save_ccr;
    }

  return type;
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
      && cxx_dialect < cxx1y
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
  tree r;
  const char *saved_message = parser->type_definition_forbidden_message;
  parser->type_definition_forbidden_message
    = G_("types may not be defined in template arguments");
  r = cp_parser_type_id_1 (parser, true, false);
  parser->type_definition_forbidden_message = saved_message;
  if (cxx_dialect >= cxx1y && type_uses_auto (r))
    {
      error ("invalid use of %<auto%> in template argument");
      r = error_mark_node;
    }
  return r;
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
      if (cp_next_tokens_can_be_attribute_p (parser))
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
	      /* Set in_declarator_p to avoid skipping to the semicolon.  */
	      int in_decl = parser->in_declarator_p;
	      parser->in_declarator_p = true;

	      if (cp_parser_uncommitted_to_tentative_parse_p (parser)
		  || !cp_parser_parse_and_diagnose_invalid_type_name (parser))
		cp_parser_error (parser, "expected type-specifier");

	      parser->in_declarator_p = in_decl;

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
}

/* Return whether the function currently being declared has an associated
   template parameter list.  */

static bool
function_being_declared_is_template_p (cp_parser* parser)
{
  if (!current_template_parms || processing_template_parmlist)
    return false;

  if (parser->implicit_template_scope)
    return true;

  if (at_class_scope_p ()
      && TYPE_BEING_DEFINED (current_class_type))
    return parser->num_template_parameter_lists != 0;

  return ((int) parser->num_template_parameter_lists > template_class_depth
	  (current_class_type));
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

  struct cleanup {
    cp_parser* parser;
    int auto_is_implicit_function_template_parm_p;
    ~cleanup() {
      parser->auto_is_implicit_function_template_parm_p
	= auto_is_implicit_function_template_parm_p;
    }
  } cleanup = { parser, parser->auto_is_implicit_function_template_parm_p };

  (void) cleanup;

  if (!processing_specialization && !processing_template_parmlist)
    if (!current_function_decl
	|| (current_class_type && LAMBDA_TYPE_P (current_class_type)))
      parser->auto_is_implicit_function_template_parm_p = true;

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
      if (in_system_header_at (input_location)
	  && current_class_type == NULL
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
    {
      /* Unwind generic function template scope if necessary.  */
      if (parser->fully_implicit_function_template_p)
	finish_fully_implicit_template (parser, /*member_decl_opt=*/0);
      return NULL;
    }

  /* Peek at the next token.  */
  token = cp_lexer_peek_token (parser->lexer);
  /* If it's a `,', the clause should terminate with an ellipsis.  */
  if (token->type == CPP_COMMA)
    {
      /* Consume the `,'.  */
      cp_lexer_consume_token (parser->lexer);
      /* Expect an ellipsis.  */
      ellipsis_p
	= (cp_parser_require (parser, CPP_ELLIPSIS, RT_ELLIPSIS) != NULL);
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
      bool parenthesized_p = false;
      int template_parm_idx = (function_being_declared_is_template_p (parser)?
			       TREE_VEC_LENGTH (INNERMOST_TEMPLATE_PARMS
						(current_template_parms)) : 0);

      /* Parse the parameter.  */
      parameter
	= cp_parser_parameter_declaration (parser,
					   /*template_parm_p=*/false,
					   &parenthesized_p);

      /* We don't know yet if the enclosing context is deprecated, so wait
	 and warn in grokparms if appropriate.  */
      deprecated_state = DEPRECATED_SUPPRESS;

      if (parameter)
	{
	  /* If a function parameter pack was specified and an implicit template
	     parameter was introduced during cp_parser_parameter_declaration,
	     change any implicit parameters introduced into packs.  */
	  if (parser->implicit_template_parms
	      && parameter->declarator
	      && parameter->declarator->parameter_pack_p)
	    {
	      int latest_template_parm_idx = TREE_VEC_LENGTH
		(INNERMOST_TEMPLATE_PARMS (current_template_parms));

	      if (latest_template_parm_idx != template_parm_idx)
		parameter->decl_specifiers.type = convert_generic_types_to_packs
		  (parameter->decl_specifiers.type,
		   template_parm_idx, latest_template_parm_idx);
	    }

	  decl = grokdeclarator (parameter->declarator,
				 &parameter->decl_specifiers,
				 PARM,
				 parameter->default_argument != NULL_TREE,
				 &parameter->decl_specifiers.attributes);
	}

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
	  DECL_PARM_LEVEL (decl) = function_parm_depth ();
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
		 "float(f)" (which is a valid declaration of a
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

  /* Reset implicit_template_scope if we are about to leave the function
     parameter list that introduced it.  Note that for out-of-line member
     definitions, there will be one or more class scopes before we get to
     the template parameter scope.  */

  if (cp_binding_level *its = parser->implicit_template_scope)
    if (cp_binding_level *maybe_its = current_binding_level->level_chain)
      {
	while (maybe_its->kind == sk_class)
	  maybe_its = maybe_its->level_chain;
	if (maybe_its == its)
	  {
	    parser->implicit_template_parms = 0;
	    parser->implicit_template_scope = 0;
	  }
      }

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
  if (!decl_specifiers.any_type_specifiers_p
      && cp_parser_parse_and_diagnose_invalid_type_name (parser))
    decl_specifiers.type = error_mark_node;

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
	  && cp_lexer_next_token_is_not (parser->lexer, CPP_OPEN_BRACE)
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
      token = cp_lexer_peek_token (parser->lexer);
      /* If we are defining a class, then the tokens that make up the
	 default argument must be saved and processed later.  */
      if (!template_parm_p && at_class_scope_p ()
	  && TYPE_BEING_DEFINED (current_class_type)
	  && !LAMBDA_TYPE_P (current_class_type))
	default_argument = cp_parser_cache_defarg (parser, /*nsdmi=*/false);
      /* Outside of a class definition, we can just parse the
	 assignment-expression.  */
      else
	default_argument
	  = cp_parser_default_argument (parser, template_parm_p);

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
		      ? G_("template parameter pack %qD "
			   "cannot have a default argument")
		      : G_("parameter pack %qD cannot have "
			   "a default argument"),
		      id_declarator->u.id.unqualified_name);
	  else
	    error_at (declarator_token_start->location,
		      template_parm_p
		      ? G_("template parameter pack cannot have "
			   "a default argument")
		      : G_("parameter pack cannot have a "
			   "default argument"));

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
  bool non_constant_p, is_direct_init;

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
  tree saved_class_ptr = NULL_TREE;
  tree saved_class_ref = NULL_TREE;
  /* The "this" pointer is not valid in a default argument.  */
  if (cfun)
    {
      saved_class_ptr = current_class_ptr;
      cp_function_chain->x_current_class_ptr = NULL_TREE;
      saved_class_ref = current_class_ref;
      cp_function_chain->x_current_class_ref = NULL_TREE;
    }
  default_argument
    = cp_parser_initializer (parser, &is_direct_init, &non_constant_p);
  /* Restore the "this" pointer.  */
  if (cfun)
    {
      cp_function_chain->x_current_class_ptr = saved_class_ptr;
      cp_function_chain->x_current_class_ref = saved_class_ref;
    }
  if (BRACE_ENCLOSED_INITIALIZER_P (default_argument))
    maybe_warn_cpp0x (CPP0X_INITIALIZER_LISTS);
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
cp_parser_function_body (cp_parser *parser, bool in_function_try_block)
{
  cp_parser_compound_statement (parser, NULL, in_function_try_block, true);
}

/* Parse a ctor-initializer-opt followed by a function-body.  Return
   true if a ctor-initializer was present.  When IN_FUNCTION_TRY_BLOCK
   is true we are parsing a function-try-block.  */

static bool
cp_parser_ctor_initializer_opt_and_function_body (cp_parser *parser,
						  bool in_function_try_block)
{
  tree body, list;
  bool ctor_initializer_p;
  const bool check_body_p =
     DECL_CONSTRUCTOR_P (current_function_decl)
     && DECL_DECLARED_CONSTEXPR_P (current_function_decl);
  tree last = NULL;

  /* Begin the function body.  */
  body = begin_function_body ();
  /* Parse the optional ctor-initializer.  */
  ctor_initializer_p = cp_parser_ctor_initializer_opt (parser);

  /* If we're parsing a constexpr constructor definition, we need
     to check that the constructor body is indeed empty.  However,
     before we get to cp_parser_function_body lot of junk has been
     generated, so we can't just check that we have an empty block.
     Rather we take a snapshot of the outermost block, and check whether
     cp_parser_function_body changed its state.  */
  if (check_body_p)
    {
      list = cur_stmt_list;
      if (STATEMENT_LIST_TAIL (list))
	last = STATEMENT_LIST_TAIL (list)->stmt;
    }
  /* Parse the function-body.  */
  cp_parser_function_body (parser, in_function_try_block);
  if (check_body_p)
    check_constexpr_ctor_body (last, list);
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
      vec<tree, va_gc> *vec;
      vec = cp_parser_parenthesized_expression_list (parser, non_attr,
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
      cp_token *token = cp_lexer_peek_token (parser->lexer);
      cp_lexer_set_source_position_from_token (token);
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
  else
    *non_constant_p = false;
  /* Now, there should be a trailing `}'.  */
  cp_parser_require (parser, CPP_CLOSE_BRACE, RT_CLOSE_BRACE);
  TREE_TYPE (initializer) = init_list_type_node;
  return initializer;
}

/* Parse an initializer-list.

   initializer-list:
     initializer-clause ... [opt]
     initializer-list , initializer-clause ... [opt]

   GNU Extension:

   initializer-list:
     designation initializer-clause ...[opt]
     initializer-list , designation initializer-clause ...[opt]

   designation:
     . identifier =
     identifier :
     [ constant-expression ] =

   Returns a vec of constructor_elt.  The VALUE of each elt is an expression
   for the initializer.  If the INDEX of the elt is non-NULL, it is the
   IDENTIFIER_NODE naming the field to initialize.  NON_CONSTANT_P is
   as for cp_parser_initializer.  */

static vec<constructor_elt, va_gc> *
cp_parser_initializer_list (cp_parser* parser, bool* non_constant_p)
{
  vec<constructor_elt, va_gc> *v = NULL;

  /* Assume all of the expressions are constant.  */
  *non_constant_p = false;

  /* Parse the rest of the list.  */
  while (true)
    {
      cp_token *token;
      tree designator;
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
	  pedwarn (input_location, OPT_Wpedantic, 
		   "ISO C++ does not allow designated initializers");
	  /* Consume the identifier.  */
	  designator = cp_lexer_consume_token (parser->lexer)->u.value;
	  /* Consume the `:'.  */
	  cp_lexer_consume_token (parser->lexer);
	}
      /* Also handle the C99 syntax, '. id ='.  */
      else if (cp_parser_allow_gnu_extensions_p (parser)
	       && cp_lexer_next_token_is (parser->lexer, CPP_DOT)
	       && cp_lexer_peek_nth_token (parser->lexer, 2)->type == CPP_NAME
	       && cp_lexer_peek_nth_token (parser->lexer, 3)->type == CPP_EQ)
	{
	  /* Warn the user that they are using an extension.  */
	  pedwarn (input_location, OPT_Wpedantic,
		   "ISO C++ does not allow C99 designated initializers");
	  /* Consume the `.'.  */
	  cp_lexer_consume_token (parser->lexer);
	  /* Consume the identifier.  */
	  designator = cp_lexer_consume_token (parser->lexer)->u.value;
	  /* Consume the `='.  */
	  cp_lexer_consume_token (parser->lexer);
	}
      /* Also handle C99 array designators, '[ const ] ='.  */
      else if (cp_parser_allow_gnu_extensions_p (parser)
	       && !c_dialect_objc ()
	       && cp_lexer_next_token_is (parser->lexer, CPP_OPEN_SQUARE))
	{
	  /* In C++11, [ could start a lambda-introducer.  */
	  bool non_const = false;

	  cp_parser_parse_tentatively (parser);
	  cp_lexer_consume_token (parser->lexer);
	  designator = cp_parser_constant_expression (parser, true, &non_const);
	  cp_parser_require (parser, CPP_CLOSE_SQUARE, RT_CLOSE_SQUARE);
	  cp_parser_require (parser, CPP_EQ, RT_EQ);
	  if (!cp_parser_parse_definitely (parser))
	    designator = NULL_TREE;
	  else if (non_const)
	    require_potential_rvalue_constant_expression (designator);
	}
      else
	designator = NULL_TREE;

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
      CONSTRUCTOR_APPEND_ELT (v, designator, initializer);

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
				    tag_type,
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

  decl = strip_using_decl (decl);

  /* Check to see that it is really the name of a class.  */
  if (TREE_CODE (decl) == TEMPLATE_ID_EXPR
      && identifier_p (TREE_OPERAND (decl, 0))
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
	   || !MAYBE_CLASS_TYPE_P (TREE_TYPE (decl))
	   /* In Objective-C 2.0, a classname followed by '.' starts a
	      dot-syntax expression, and it's not a type-name.  */
	   || (c_dialect_objc ()
	       && cp_lexer_peek_token (parser->lexer)->type == CPP_DOT 
	       && objc_is_class_name (decl)))
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
cp_parser_class_specifier_1 (cp_parser* parser)
{
  tree type;
  tree attributes = NULL_TREE;
  bool nested_name_specifier_p;
  unsigned saved_num_template_parameter_lists;
  bool saved_in_function_body;
  unsigned char in_statement;
  bool in_switch_statement_p;
  bool saved_in_unbraced_linkage_specification_p;
  tree old_scope = NULL_TREE;
  tree scope = NULL_TREE;
  cp_token *closing_brace;

  push_deferring_access_checks (dk_no_deferred);

  /* Parse the class-head.  */
  type = cp_parser_class_head (parser,
			       &nested_name_specifier_p);
  /* If the class-head was a semantic disaster, skip the entire body
     of the class.  */
  if (!type)
    {
      cp_parser_skip_to_end_of_block_or_statement (parser);
      pop_deferring_access_checks ();
      return error_mark_node;
    }

  /* Look for the `{'.  */
  if (!cp_parser_require (parser, CPP_OPEN_BRACE, RT_OPEN_BRACE))
    {
      pop_deferring_access_checks ();
      return error_mark_node;
    }

  cp_ensure_no_omp_declare_simd (parser);

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
  /* Or in a loop.  */
  in_statement = parser->in_statement;
  parser->in_statement = 0;
  /* Or in a switch.  */
  in_switch_statement_p = parser->in_switch_statement_p;
  parser->in_switch_statement_p = false;
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
  type = begin_class_definition (type);

  if (type == error_mark_node)
    /* If the type is erroneous, skip the entire body of the class.  */
    cp_parser_skip_to_closing_brace (parser);
  else
    /* Parse the member-specification.  */
    cp_parser_member_specification_opt (parser);

  /* Look for the trailing `}'.  */
  closing_brace = cp_parser_require (parser, CPP_CLOSE_BRACE, RT_CLOSE_BRACE);
  /* Look for trailing attributes to apply to this class.  */
  if (cp_parser_allow_gnu_extensions_p (parser))
    attributes = cp_parser_gnu_attributes_opt (parser);
  if (type != error_mark_node)
    type = finish_struct (type, attributes);
  if (nested_name_specifier_p)
    pop_inner_scope (old_scope, scope);

  /* We've finished a type definition.  Check for the common syntax
     error of forgetting a semicolon after the definition.  We need to
     be careful, as we can't just check for not-a-semicolon and be done
     with it; the user might have typed:

     class X { } c = ...;
     class X { } *p = ...;

     and so forth.  Instead, enumerate all the possible tokens that
     might follow this production; if we don't see one of them, then
     complain and silently insert the semicolon.  */
  {
    cp_token *token = cp_lexer_peek_token (parser->lexer);
    bool want_semicolon = true;

    if (cp_next_tokens_can_be_std_attribute_p (parser))
      /* Don't try to parse c++11 attributes here.  As per the
	 grammar, that should be a task for
	 cp_parser_decl_specifier_seq.  */
      want_semicolon = false;

    switch (token->type)
      {
      case CPP_NAME:
      case CPP_SEMICOLON:
      case CPP_MULT:
      case CPP_AND:
      case CPP_OPEN_PAREN:
      case CPP_CLOSE_PAREN:
      case CPP_COMMA:
        want_semicolon = false;
        break;

        /* While it's legal for type qualifiers and storage class
           specifiers to follow type definitions in the grammar, only
           compiler testsuites contain code like that.  Assume that if
           we see such code, then what we're really seeing is a case
           like:

           class X { }
           const <type> var = ...;

           or

           class Y { }
           static <type> func (...) ...

           i.e. the qualifier or specifier applies to the next
           declaration.  To do so, however, we need to look ahead one
           more token to see if *that* token is a type specifier.

	   This code could be improved to handle:

	   class Z { }
	   static const <type> var = ...;  */
      case CPP_KEYWORD:
	if (keyword_is_decl_specifier (token->keyword))
	  {
	    cp_token *lookahead = cp_lexer_peek_nth_token (parser->lexer, 2);

	    /* Handling user-defined types here would be nice, but very
	       tricky.  */
	    want_semicolon
	      = (lookahead->type == CPP_KEYWORD
		 && keyword_begins_type_specifier (lookahead->keyword));
	  }
	break;
      default:
	break;
      }

    /* If we don't have a type, then something is very wrong and we
       shouldn't try to do anything clever.  Likewise for not seeing the
       closing brace.  */
    if (closing_brace && TYPE_P (type) && want_semicolon)
      {
	cp_token_position prev
	  = cp_lexer_previous_token_position (parser->lexer);
	cp_token *prev_token = cp_lexer_token_at (parser->lexer, prev);
	location_t loc = prev_token->location;

	if (CLASSTYPE_DECLARED_CLASS (type))
	  error_at (loc, "expected %<;%> after class definition");
	else if (TREE_CODE (type) == RECORD_TYPE)
	  error_at (loc, "expected %<;%> after struct definition");
	else if (TREE_CODE (type) == UNION_TYPE)
	  error_at (loc, "expected %<;%> after union definition");
	else
	  gcc_unreachable ();

	/* Unget one token and smash it to look as though we encountered
	   a semicolon in the input stream.  */
	cp_lexer_set_token_position (parser->lexer, prev);
	token = cp_lexer_peek_token (parser->lexer);
	token->type = CPP_SEMICOLON;
	token->keyword = RID_MAX;
      }
  }

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
      tree decl;
      tree class_type = NULL_TREE;
      tree pushed_scope = NULL_TREE;
      unsigned ix;
      cp_default_arg_entry *e;
      tree save_ccp, save_ccr;

      /* In a first pass, parse default arguments to the functions.
	 Then, in a second pass, parse the bodies of the functions.
	 This two-phased approach handles cases like:

	    struct S {
	      void f() { g(); }
	      void g(int i = 3);
	    };

	 */
      FOR_EACH_VEC_SAFE_ELT (unparsed_funs_with_default_args, ix, e)
	{
	  decl = e->decl;
	  /* If there are default arguments that have not yet been processed,
	     take care of them now.  */
	  if (class_type != e->class_type)
	    {
	      if (pushed_scope)
		pop_scope (pushed_scope);
	      class_type = e->class_type;
	      pushed_scope = push_scope (class_type);
	    }
	  /* Make sure that any template parameters are in scope.  */
	  maybe_begin_member_template_processing (decl);
	  /* Parse the default argument expressions.  */
	  cp_parser_late_parsing_default_args (parser, decl);
	  /* Remove any template parameters from the symbol table.  */
	  maybe_end_member_template_processing ();
	}
      vec_safe_truncate (unparsed_funs_with_default_args, 0);
      /* Now parse any NSDMIs.  */
      save_ccp = current_class_ptr;
      save_ccr = current_class_ref;
      FOR_EACH_VEC_SAFE_ELT (unparsed_nsdmis, ix, decl)
	{
	  if (class_type != DECL_CONTEXT (decl))
	    {
	      if (pushed_scope)
		pop_scope (pushed_scope);
	      class_type = DECL_CONTEXT (decl);
	      pushed_scope = push_scope (class_type);
	    }
	  inject_this_parameter (class_type, TYPE_UNQUALIFIED);
	  cp_parser_late_parsing_nsdmi (parser, decl);
	}
      vec_safe_truncate (unparsed_nsdmis, 0);
      current_class_ptr = save_ccp;
      current_class_ref = save_ccr;
      if (pushed_scope)
	pop_scope (pushed_scope);
      /* Now parse the body of the functions.  */
      if (flag_openmp)
	{
	  /* OpenMP UDRs need to be parsed before all other functions.  */
	  FOR_EACH_VEC_SAFE_ELT (unparsed_funs_with_definitions, ix, decl)
	    if (DECL_OMP_DECLARE_REDUCTION_P (decl))
	      cp_parser_late_parsing_for_member (parser, decl);
	  FOR_EACH_VEC_SAFE_ELT (unparsed_funs_with_definitions, ix, decl)
	    if (!DECL_OMP_DECLARE_REDUCTION_P (decl))
	      cp_parser_late_parsing_for_member (parser, decl);
	}
      else
	FOR_EACH_VEC_SAFE_ELT (unparsed_funs_with_definitions, ix, decl)
	  cp_parser_late_parsing_for_member (parser, decl);
      vec_safe_truncate (unparsed_funs_with_definitions, 0);
    }

  /* Put back any saved access checks.  */
  pop_deferring_access_checks ();

  /* Restore saved state.  */
  parser->in_switch_statement_p = in_switch_statement_p;
  parser->in_statement = in_statement;
  parser->in_function_body = saved_in_function_body;
  parser->num_template_parameter_lists
    = saved_num_template_parameter_lists;
  parser->in_unbraced_linkage_specification_p
    = saved_in_unbraced_linkage_specification_p;

  return type;
}

static tree
cp_parser_class_specifier (cp_parser* parser)
{
  tree ret;
  timevar_push (TV_PARSE_STRUCT);
  ret = cp_parser_class_specifier_1 (parser);
  timevar_pop (TV_PARSE_STRUCT);
  return ret;
}

/* Parse a class-head.

   class-head:
     class-key identifier [opt] base-clause [opt]
     class-key nested-name-specifier identifier class-virt-specifier [opt] base-clause [opt]
     class-key nested-name-specifier [opt] template-id
       base-clause [opt]

   class-virt-specifier:
     final

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
		      bool* nested_name_specifier_p)
{
  tree nested_name_specifier;
  enum tag_types class_key;
  tree id = NULL_TREE;
  tree type = NULL_TREE;
  tree attributes;
  tree bases;
  cp_virt_specifiers virt_specifiers = VIRT_SPEC_UNSPECIFIED;
  bool template_id_p = false;
  bool qualified_p = false;
  bool invalid_nested_name_p = false;
  bool invalid_explicit_specialization_p = false;
  bool saved_colon_corrects_to_scope_p = parser->colon_corrects_to_scope_p;
  tree pushed_scope = NULL_TREE;
  unsigned num_templates;
  cp_token *type_start_token = NULL, *nested_name_specifier_token_start = NULL;
  /* Assume no nested-name-specifier will be present.  */
  *nested_name_specifier_p = false;
  /* Assume no template parameter lists will be used in defining the
     type.  */
  num_templates = 0;
  parser->colon_corrects_to_scope_p = false;

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
					   /*type_p=*/true,
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
	       scope = get_containing_scope (scope))
	    if (TYPE_P (scope)
		&& CLASS_TYPE_P (scope)
		&& CLASSTYPE_TEMPLATE_INFO (scope)
		&& PRIMARY_TEMPLATE_P (CLASSTYPE_TI_TEMPLATE (scope))
		&& (!CLASSTYPE_TEMPLATE_SPECIALIZATION (scope)
		    || uses_template_parms (CLASSTYPE_TI_ARGS (scope))))
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
				  class_key,
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
    {
      cp_parser_check_for_invalid_template_id (parser, id,
					       class_key,
                                               type_start_token->location);
    }
  virt_specifiers = cp_parser_virt_specifier_seq_opt (parser);

  /* If it's not a `:' or a `{' then we can't really be looking at a
     class-head, since a class-head only appears as part of a
     class-specifier.  We have to detect this situation before calling
     xref_tag, since that has irreversible side-effects.  */
  if (!cp_parser_next_token_starts_class_definition_p (parser))
    {
      cp_parser_error (parser, "expected %<{%> or %<:%>");
      type = error_mark_node;
      goto out;
    }

  /* At this point, we're going ahead with the class-specifier, even
     if some other problem occurs.  */
  cp_parser_commit_to_tentative_parse (parser);
  if (virt_specifiers & VIRT_SPEC_OVERRIDE)
    {
      cp_parser_error (parser,
                       "cannot specify %<override%> for a class");
      type = error_mark_node;
      goto out;
    }
  /* Issue the error about the overly-qualified name now.  */
  if (qualified_p)
    {
      cp_parser_error (parser,
		       "global qualification of class name is invalid");
      type = error_mark_node;
      goto out;
    }
  else if (invalid_nested_name_p)
    {
      cp_parser_error (parser,
		       "qualified name does not name a class");
      type = error_mark_node;
      goto out;
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

  if (type)
    {
      /* Apply attributes now, before any use of the class as a template
	 argument in its base list.  */
      cplus_decl_attributes (&type, attributes, (int)ATTR_FLAG_TYPE_IN_PLACE);
      fixup_attribute_variants (type);
    }

  /* We will have entered the scope containing the class; the names of
     base classes should be looked up in that context.  For example:

       struct A { struct B {}; struct C; };
       struct A::C : B {};

     is valid.  */

  /* Get the list of base-classes, if there is one.  */
  if (cp_lexer_next_token_is (parser->lexer, CPP_COLON))
    {
      /* PR59482: enter the class scope so that base-specifiers are looked
	 up correctly.  */
      if (type)
	pushclass (type);
      bases = cp_parser_base_clause (parser);
      /* PR59482: get out of the previously pushed class scope so that the
	 subsequent pops pop the right thing.  */
      if (type)
	popclass ();
    }
  else
    bases = NULL_TREE;

  /* If we're really defining a class, process the base classes.
     If they're invalid, fail.  */
  if (type && cp_lexer_next_token_is (parser->lexer, CPP_OPEN_BRACE)
      && !xref_basetypes (type, bases))
    type = NULL_TREE;

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

  if (type)
    DECL_SOURCE_LOCATION (TYPE_NAME (type)) = type_start_token->location;
  if (type && (virt_specifiers & VIRT_SPEC_FINAL))
    CLASSTYPE_FINAL (type) = 1;
 out:
  parser->colon_corrects_to_scope_p = saved_colon_corrects_to_scope_p;
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
  token = cp_parser_require (parser, CPP_KEYWORD, RT_CLASS_KEY);
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
	  cp_parser_require (parser, CPP_COLON, RT_COLON);
	  break;

	default:
	  /* Accept #pragmas at class scope.  */
	  if (token->type == CPP_PRAGMA)
	    {
	      cp_parser_pragma (parser, pragma_member);
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
     alias-declaration

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
  bool saved_colon_corrects_to_scope_p = parser->colon_corrects_to_scope_p;

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
      if (cxx_dialect < cxx11)
	{
	  /* Parse the using-declaration.  */
	  cp_parser_using_declaration (parser,
				       /*access_declaration_p=*/false);
	  return;
	}
      else
	{
	  tree decl;
	  bool alias_decl_expected;
	  cp_parser_parse_tentatively (parser);
	  decl = cp_parser_alias_declaration (parser);
	  /* Note that if we actually see the '=' token after the
	     identifier, cp_parser_alias_declaration commits the
	     tentative parse.  In that case, we really expects an
	     alias-declaration.  Otherwise, we expect a using
	     declaration.  */
	  alias_decl_expected =
	    !cp_parser_uncommitted_to_tentative_parse_p (parser);
	  cp_parser_parse_definitely (parser);

	  if (alias_decl_expected)
	    finish_member_declaration (decl);
	  else
	    cp_parser_using_declaration (parser,
					 /*access_declaration_p=*/false);
	  return;
	}
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

  parser->colon_corrects_to_scope_p = false;

  if (cp_parser_using_declaration (parser, /*access_declaration=*/true))
      goto out;

  /* Parse the decl-specifier-seq.  */
  decl_spec_token_start = cp_lexer_peek_token (parser->lexer);
  cp_parser_decl_specifier_seq (parser,
				CP_PARSER_FLAGS_OPTIONAL,
				&decl_specifiers,
				&declares_class_or_enum);
  /* Check for an invalid type-name.  */
  if (!decl_specifiers.any_type_specifiers_p
      && cp_parser_parse_and_diagnose_invalid_type_name (parser))
    goto out;
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
	    pedwarn (token->location, OPT_Wpedantic, "extra %<;%>");
	}
      else
	{
	  tree type;

	  /* See if this declaration is a friend.  */
	  friend_p = cp_parser_friend_p (&decl_specifiers);
	  /* If there were decl-specifiers, check to see if there was
	     a class-declaration.  */
	  type = check_tag_decl (&decl_specifiers,
				 /*explicit_type_instantiation_p=*/false);
	  /* Nested classes have already been added to the class, but
	     a `friend' needs to be explicitly registered.  */
	  if (friend_p)
	    {
	      /* If the `friend' keyword was present, the friend must
		 be introduced with a class-key.  */
	       if (!declares_class_or_enum && cxx_dialect < cxx11)
		 pedwarn (decl_spec_token_start->location, OPT_Wpedantic,
			  "in C++03 a class-key must be used "
			  "when declaring a friend");
	       /* In this case:

		    template <typename T> struct A {
		      friend struct A<T>::B;
		    };

		  A<T>::B will be represented by a TYPENAME_TYPE, and
		  therefore not recognized by check_tag_decl.  */
	       if (!type)
		 {
		   type = decl_specifiers.type;
		   if (type && TREE_CODE (type) == TYPE_DECL)
		     type = TREE_TYPE (type);
		 }
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
	      /* C++11 9.5/6.  */
	      if (decl_specifiers.storage_class != sc_none)
		error_at (decl_spec_token_start->location,
			  "a storage class on an anonymous aggregate "
			  "in class scope is not allowed");

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
      bool assume_semicolon = false;

      /* Clear attributes from the decl_specifiers but keep them
	 around as prefix attributes that apply them to the entity
	 being declared.  */
      prefix_attributes = decl_specifiers.attributes;
      decl_specifiers.attributes = NULL_TREE;

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
		  goto out;
		}

	      if (declares_class_or_enum & 2)
		cp_parser_check_for_definition_in_return_type
					    (declarator, decl_specifiers.type,
					     decl_specifiers.locations[ds_type_spec]);

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
		  if (function_declarator_p (declarator)
		      || (decl_specifiers.type
			  && TREE_CODE (decl_specifiers.type) == TYPE_DECL
			  && declarator->kind == cdk_id
			  && (TREE_CODE (TREE_TYPE (decl_specifiers.type))
			      == FUNCTION_TYPE)))
		    initializer = cp_parser_pure_specifier (parser);
		  else if (decl_specifiers.storage_class != sc_static)
		    initializer = cp_parser_save_nsdmi (parser);
		  else if (cxx_dialect >= cxx11)
		    {
		      bool nonconst;
		      /* Don't require a constant rvalue in C++11, since we
			 might want a reference constant.  We'll enforce
		         constancy later.  */
		      cp_lexer_consume_token (parser->lexer);
		      /* Parse the initializer.  */
		      initializer = cp_parser_initializer_clause (parser,
								  &nonconst);
		    }
		  else
		    /* Parse the initializer.  */
		    initializer = cp_parser_constant_initializer (parser);
		}
	      else if (cp_lexer_next_token_is (parser->lexer, CPP_OPEN_BRACE)
		       && !function_declarator_p (declarator))
		{
		  bool x;
		  if (decl_specifiers.storage_class != sc_static)
		    initializer = cp_parser_save_nsdmi (parser);
		  else
		    initializer = cp_parser_initializer (parser, &x, &x);
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
		  if (initializer && initializer_token_start)
		    error_at (initializer_token_start->location,
			      "pure-specifier on function-definition");
		  decl = cp_parser_save_member_function_body (parser,
							      &decl_specifiers,
							      declarator,
							      attributes);
		  if (parser->fully_implicit_function_template_p)
		    decl = finish_fully_implicit_template (parser, decl);
		  /* If the member was not a friend, declare it here.  */
		  if (!friend_p)
		    finish_member_declaration (decl);
		  /* Peek at the next token.  */
		  token = cp_lexer_peek_token (parser->lexer);
		  /* If the next token is a semicolon, consume it.  */
		  if (token->type == CPP_SEMICOLON)
		    cp_lexer_consume_token (parser->lexer);
		  goto out;
		}
	      else
		if (declarator->kind == cdk_function)
		  declarator->id_loc = token->location;
	      /* Create the declaration.  */
	      decl = grokfield (declarator, &decl_specifiers,
				initializer, /*init_const_expr_p=*/true,
				asm_specification, attributes);
		if (parser->fully_implicit_function_template_p)
		  decl = finish_fully_implicit_template (parser, decl);
	    }

	  cp_finalize_omp_declare_simd (parser, decl);

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
	    {
	      cp_lexer_consume_token (parser->lexer);
	      if (cp_lexer_next_token_is (parser->lexer, CPP_SEMICOLON))
		{
		  cp_token *token = cp_lexer_previous_token (parser->lexer);
		  error_at (token->location,
			    "stray %<,%> at end of member declaration");
		}
	    }
	  /* If the next token isn't a `;', then we have a parse error.  */
	  else if (cp_lexer_next_token_is_not (parser->lexer,
					       CPP_SEMICOLON))
	    {
	      /* The next token might be a ways away from where the
		 actual semicolon is missing.  Find the previous token
		 and use that for our error position.  */
	      cp_token *token = cp_lexer_previous_token (parser->lexer);
	      error_at (token->location,
			"expected %<;%> at end of member declaration");

	      /* Assume that the user meant to provide a semicolon.  If
		 we were to cp_parser_skip_to_end_of_statement, we might
		 skip to a semicolon inside a member function definition
		 and issue nonsensical error messages.  */
	      assume_semicolon = true;
	    }

	  if (decl)
	    {
	      /* Add DECL to the list of members.  */
	      if (!friend_p)
		finish_member_declaration (decl);

	      if (TREE_CODE (decl) == FUNCTION_DECL)
		cp_parser_save_default_args (parser, decl);
	      else if (TREE_CODE (decl) == FIELD_DECL
		       && !DECL_C_BIT_FIELD (decl)
		       && DECL_INITIAL (decl))
		/* Add DECL to the queue of NSDMI to be parsed later.  */
		vec_safe_push (unparsed_nsdmis, decl);
	    }

	  if (assume_semicolon)
	    goto out;
	}
    }

  cp_parser_require (parser, CPP_SEMICOLON, RT_SEMICOLON);
 out:
  parser->colon_corrects_to_scope_p = saved_colon_corrects_to_scope_p;
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
  if (!cp_parser_require (parser, CPP_EQ, RT_EQ))
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
  if (!cp_parser_require (parser, CPP_EQ, RT_EQ))
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
      cp_parser_require (parser, CPP_CLOSE_BRACE, RT_CLOSE_BRACE);

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
  cp_parser_require (parser, CPP_COLON, RT_COLON);

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
      if (base && base != error_mark_node)
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

  if (!parser->scope
      && cp_lexer_next_token_is_decltype (parser->lexer))
    /* DR 950 allows decltype as a base-specifier.  */
    type = cp_parser_decltype (parser);
  else
    {
      /* Otherwise, look for the class-name.  */
      type = cp_parser_class_name (parser,
				   class_scope_p,
				   template_p,
				   typename_type,
				   /*check_dependency_p=*/true,
				   /*class_head_p=*/false,
				   /*is_declaration=*/true);
      type = TREE_TYPE (type);
    }

  if (type == error_mark_node)
    return error_mark_node;

  return finish_base_specifier (type, access, virtual_p);
}

/* Exception handling [gram.exception] */

/* Parse an (optional) noexcept-specification.

   noexcept-specification:
     noexcept ( constant-expression ) [opt]

   If no noexcept-specification is present, returns NULL_TREE.
   Otherwise, if REQUIRE_CONSTEXPR is false, then either parse and return any
   expression if parentheses follow noexcept, or return BOOLEAN_TRUE_NODE if
   there are no parentheses.  CONSUMED_EXPR will be set accordingly.
   Otherwise, returns a noexcept specification unless RETURN_COND is true,
   in which case a boolean condition is returned instead.  */

static tree
cp_parser_noexcept_specification_opt (cp_parser* parser,
				      bool require_constexpr,
				      bool* consumed_expr,
				      bool return_cond)
{
  cp_token *token;
  const char *saved_message;

  /* Peek at the next token.  */
  token = cp_lexer_peek_token (parser->lexer);

  /* Is it a noexcept-specification?  */
  if (cp_parser_is_keyword (token, RID_NOEXCEPT))
    {
      tree expr;
      cp_lexer_consume_token (parser->lexer);

      if (cp_lexer_peek_token (parser->lexer)->type == CPP_OPEN_PAREN)
	{
	  cp_lexer_consume_token (parser->lexer);

	  if (require_constexpr)
	    {
	      /* Types may not be defined in an exception-specification.  */
	      saved_message = parser->type_definition_forbidden_message;
	      parser->type_definition_forbidden_message
	      = G_("types may not be defined in an exception-specification");

	      expr = cp_parser_constant_expression (parser, false, NULL);

	      /* Restore the saved message.  */
	      parser->type_definition_forbidden_message = saved_message;
	    }
	  else
	    {
	      expr = cp_parser_expression (parser, false, NULL);
	      *consumed_expr = true;
	    }

	  cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN);
	}
      else
	{
	  expr = boolean_true_node;
	  if (!require_constexpr)
	    *consumed_expr = false;
	}

      /* We cannot build a noexcept-spec right away because this will check
	 that expr is a constexpr.  */
      if (!return_cond)
	return build_noexcept_spec (expr, tf_warning_or_error);
      else
	return expr;
    }
  else
    return NULL_TREE;
}

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
  const char *saved_message;

  /* Peek at the next token.  */
  token = cp_lexer_peek_token (parser->lexer);

  /* Is it a noexcept-specification?  */
  type_id_list = cp_parser_noexcept_specification_opt(parser, true, NULL,
						      false);
  if (type_id_list != NULL_TREE)
    return type_id_list;

  /* If it's not `throw', then there's no exception-specification.  */
  if (!cp_parser_is_keyword (token, RID_THROW))
    return NULL_TREE;

#if 0
  /* Enable this once a lot of code has transitioned to noexcept?  */
  if (cxx_dialect >= cxx11 && !in_system_header_at (input_location))
    warning (OPT_Wdeprecated, "dynamic exception specifications are "
	     "deprecated in C++0x; use %<noexcept%> instead");
#endif

  /* Consume the `throw'.  */
  cp_lexer_consume_token (parser->lexer);

  /* Look for the `('.  */
  cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN);

  /* Peek at the next token.  */
  token = cp_lexer_peek_token (parser->lexer);
  /* If it's not a `)', then there is a type-id-list.  */
  if (token->type != CPP_CLOSE_PAREN)
    {
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
  cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN);

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

  cp_parser_require_keyword (parser, RID_TRY, RT_TRY);
  try_block = begin_try_block ();
  cp_parser_compound_statement (parser, NULL, true, false);
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
  if (!cp_parser_require_keyword (parser, RID_TRY, RT_TRY))
    return false;
  /* Let the rest of the front end know where we are.  */
  try_block = begin_function_try_block (&compound_stmt);
  /* Parse the function-body.  */
  ctor_initializer_p = cp_parser_ctor_initializer_opt_and_function_body
    (parser, /*in_function_try_block=*/true);
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

  cp_parser_require_keyword (parser, RID_CATCH, RT_CATCH);
  handler = begin_handler ();
  cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN);
  declaration = cp_parser_exception_declaration (parser);
  finish_handler_parms (declaration, handler);
  cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN);
  cp_parser_compound_statement (parser, NULL, false, false);
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

  cp_parser_require_keyword (parser, RID_THROW, RT_THROW);
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
  cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN);

  /* Look for the string-literal.  */
  asm_specification = cp_parser_string_literal (parser, false, false);

  /* Look for the `)'.  */
  cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN);

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
	  cp_parser_require (parser, CPP_CLOSE_SQUARE, RT_CLOSE_SQUARE);
	}
      else
	name = NULL_TREE;
      /* Look for the string-literal.  */
      string_literal = cp_parser_string_literal (parser, false, false);

      /* Look for the `('.  */
      cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN);
      /* Parse the expression.  */
      expression = cp_parser_expression (parser, /*cast_p=*/false, NULL);
      /* Look for the `)'.  */
      cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN);

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

/* Return TRUE iff the next tokens in the stream are possibly the
   beginning of a GNU extension attribute. */

static bool
cp_next_tokens_can_be_gnu_attribute_p (cp_parser *parser)
{
  return cp_nth_tokens_can_be_gnu_attribute_p (parser, 1);
}

/* Return TRUE iff the next tokens in the stream are possibly the
   beginning of a standard C++-11 attribute specifier.  */

static bool
cp_next_tokens_can_be_std_attribute_p (cp_parser *parser)
{
  return cp_nth_tokens_can_be_std_attribute_p (parser, 1);
}

/* Return TRUE iff the next Nth tokens in the stream are possibly the
   beginning of a standard C++-11 attribute specifier.  */

static bool
cp_nth_tokens_can_be_std_attribute_p (cp_parser *parser, size_t n)
{
  cp_token *token = cp_lexer_peek_nth_token (parser->lexer, n);

  return (cxx_dialect >= cxx11
	  && ((token->type == CPP_KEYWORD && token->keyword == RID_ALIGNAS)
	      || (token->type == CPP_OPEN_SQUARE
		  && (token = cp_lexer_peek_nth_token (parser->lexer, n + 1))
		  && token->type == CPP_OPEN_SQUARE)));
}

/* Return TRUE iff the next Nth tokens in the stream are possibly the
   beginning of a GNU extension attribute.  */

static bool
cp_nth_tokens_can_be_gnu_attribute_p (cp_parser *parser, size_t n)
{
  cp_token *token = cp_lexer_peek_nth_token (parser->lexer, n);

  return token->type == CPP_KEYWORD && token->keyword == RID_ATTRIBUTE;
}

/* Return true iff the next tokens can be the beginning of either a
   GNU attribute list, or a standard C++11 attribute sequence.  */

static bool
cp_next_tokens_can_be_attribute_p (cp_parser *parser)
{
  return (cp_next_tokens_can_be_gnu_attribute_p (parser)
	  || cp_next_tokens_can_be_std_attribute_p (parser));
}

/* Return true iff the next Nth tokens can be the beginning of either
   a GNU attribute list, or a standard C++11 attribute sequence.  */

static bool
cp_nth_tokens_can_be_attribute_p (cp_parser *parser, size_t n)
{
  return (cp_nth_tokens_can_be_gnu_attribute_p (parser, n)
	  || cp_nth_tokens_can_be_std_attribute_p (parser, n));
}

/* Parse either a standard C++-11 attribute-specifier-seq, or a series
   of GNU attributes, or return NULL.  */

static tree
cp_parser_attributes_opt (cp_parser *parser)
{
  if (cp_next_tokens_can_be_gnu_attribute_p (parser))
      return cp_parser_gnu_attributes_opt (parser);
  return cp_parser_std_attribute_spec_seq (parser);
}

#define CILK_SIMD_FN_CLAUSE_MASK				  \
	((OMP_CLAUSE_MASK_1 << PRAGMA_CILK_CLAUSE_VECTORLENGTH)	  \
	 | (OMP_CLAUSE_MASK_1 << PRAGMA_CILK_CLAUSE_LINEAR)	  \
	 | (OMP_CLAUSE_MASK_1 << PRAGMA_CILK_CLAUSE_UNIFORM)	  \
	 | (OMP_CLAUSE_MASK_1 << PRAGMA_CILK_CLAUSE_MASK)	  \
	 | (OMP_CLAUSE_MASK_1 << PRAGMA_CILK_CLAUSE_NOMASK))

/* Parses the Cilk Plus SIMD-enabled function's attribute.  Syntax:
   vector [(<clauses>)]  */

static void
cp_parser_cilk_simd_fn_vector_attrs (cp_parser *parser, cp_token *v_token)
{  
  bool first_p = parser->cilk_simd_fn_info == NULL;
  cp_token *token = v_token;
  if (first_p)
    {
      parser->cilk_simd_fn_info = XNEW (cp_omp_declare_simd_data);
      parser->cilk_simd_fn_info->error_seen = false;
      parser->cilk_simd_fn_info->fndecl_seen = false;
      parser->cilk_simd_fn_info->tokens = vNULL;
    }
  int paren_scope = 0;
  if (cp_lexer_next_token_is (parser->lexer, CPP_OPEN_PAREN))
    {
      cp_lexer_consume_token (parser->lexer);
      v_token = cp_lexer_peek_token (parser->lexer);
      paren_scope++;
    }
  while (paren_scope > 0)
    {
      token = cp_lexer_peek_token (parser->lexer);
      if (token->type == CPP_OPEN_PAREN)
	paren_scope++;
      else if (token->type == CPP_CLOSE_PAREN)
	paren_scope--;
      /* Do not push the last ')'  */
      if (!(token->type == CPP_CLOSE_PAREN && paren_scope == 0))
	cp_lexer_consume_token (parser->lexer);
    }

  token->type = CPP_PRAGMA_EOL;
  parser->lexer->next_token = token;
  cp_lexer_consume_token (parser->lexer);

  struct cp_token_cache *cp
    = cp_token_cache_new (v_token, cp_lexer_peek_token (parser->lexer));
  parser->cilk_simd_fn_info->tokens.safe_push (cp);
}

/* Parse an (optional) series of attributes.

   attributes:
     attributes attribute

   attribute:
     __attribute__ (( attribute-list [opt] ))

   The return value is as for cp_parser_gnu_attribute_list.  */

static tree
cp_parser_gnu_attributes_opt (cp_parser* parser)
{
  tree attributes = NULL_TREE;

  while (true)
    {
      cp_token *token;
      tree attribute_list;
      bool ok = true;

      /* Peek at the next token.  */
      token = cp_lexer_peek_token (parser->lexer);
      /* If it's not `__attribute__', then we're done.  */
      if (token->keyword != RID_ATTRIBUTE)
	break;

      /* Consume the `__attribute__' keyword.  */
      cp_lexer_consume_token (parser->lexer);
      /* Look for the two `(' tokens.  */
      cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN);
      cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN);

      /* Peek at the next token.  */
      token = cp_lexer_peek_token (parser->lexer);
      if (token->type != CPP_CLOSE_PAREN)
	/* Parse the attribute-list.  */
	attribute_list = cp_parser_gnu_attribute_list (parser);
      else
	/* If the next token is a `)', then there is no attribute
	   list.  */
	attribute_list = NULL;

      /* Look for the two `)' tokens.  */
      if (!cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN))
	ok = false;
      if (!cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN))
	ok = false;
      if (!ok)
	cp_parser_skip_to_end_of_statement (parser);

      /* Add these new attributes to the list.  */
      attributes = chainon (attributes, attribute_list);
    }

  return attributes;
}

/* Returns true of NAME is an IDENTIFIER_NODE with identiifer "vector,"
   "__vector" or "__vector__."  */

static inline bool
is_cilkplus_vector_p (tree name)
{ 
  if (flag_cilkplus && is_attribute_p ("vector", name)) 
    return true;
  return false;
}

/* Parse a GNU attribute-list.

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
cp_parser_gnu_attribute_list (cp_parser* parser)
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

	  /* Consume the token, but save it since we need it for the
	     SIMD enabled function parsing.  */
	  cp_token *id_token = cp_lexer_consume_token (parser->lexer);

	  /* Save away the identifier that indicates which attribute
	     this is.  */
	  identifier = (token->type == CPP_KEYWORD) 
	    /* For keywords, use the canonical spelling, not the
	       parsed identifier.  */
	    ? ridpointers[(int) token->keyword]
	    : id_token->u.value;
	  
	  attribute = build_tree_list (identifier, NULL_TREE);

	  /* Peek at the next token.  */
	  token = cp_lexer_peek_token (parser->lexer);
	  /* If it's an `(', then parse the attribute arguments.  */
	  if (token->type == CPP_OPEN_PAREN)
	    {
	      vec<tree, va_gc> *vec;
	      int attr_flag = (attribute_takes_identifier_p (identifier)
			       ? id_attr : normal_attr);
	      if (is_cilkplus_vector_p (identifier))
		{
		  cp_parser_cilk_simd_fn_vector_attrs (parser, id_token);
		  continue;
		}
	      else
		vec = cp_parser_parenthesized_expression_list 
		  (parser, attr_flag, /*cast_p=*/false, 
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
	  else if (is_cilkplus_vector_p (identifier))
	    {
	      cp_parser_cilk_simd_fn_vector_attrs (parser, id_token);
	      continue;
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

/*  Parse a standard C++11 attribute.

    The returned representation is a TREE_LIST which TREE_PURPOSE is
    the scoped name of the attribute, and the TREE_VALUE is its
    arguments list.

    Note that the scoped name of the attribute is itself a TREE_LIST
    which TREE_PURPOSE is the namespace of the attribute, and
    TREE_VALUE its name.  This is unlike a GNU attribute -- as parsed
    by cp_parser_gnu_attribute_list -- that doesn't have any namespace
    and which TREE_PURPOSE is directly the attribute name.

    Clients of the attribute code should use get_attribute_namespace
    and get_attribute_name to get the actual namespace and name of
    attributes, regardless of their being GNU or C++11 attributes.

    attribute:
      attribute-token attribute-argument-clause [opt]

    attribute-token:
      identifier
      attribute-scoped-token

    attribute-scoped-token:
      attribute-namespace :: identifier

    attribute-namespace:
      identifier

    attribute-argument-clause:
      ( balanced-token-seq )

    balanced-token-seq:
      balanced-token [opt]
      balanced-token-seq balanced-token

    balanced-token:
      ( balanced-token-seq )
      [ balanced-token-seq ]
      { balanced-token-seq }.  */

static tree
cp_parser_std_attribute (cp_parser *parser)
{
  tree attribute, attr_ns = NULL_TREE, attr_id = NULL_TREE, arguments;
  cp_token *token;

  /* First, parse name of the the attribute, a.k.a
     attribute-token.  */

  token = cp_lexer_peek_token (parser->lexer);
  if (token->type == CPP_NAME)
    attr_id = token->u.value;
  else if (token->type == CPP_KEYWORD)
    attr_id = ridpointers[(int) token->keyword];
  else if (token->flags & NAMED_OP)
    attr_id = get_identifier (cpp_type2name (token->type, token->flags));

  if (attr_id == NULL_TREE)
    return NULL_TREE;

  cp_lexer_consume_token (parser->lexer);

  token = cp_lexer_peek_token (parser->lexer);
  if (token->type == CPP_SCOPE)
    {
      /* We are seeing a scoped attribute token.  */

      cp_lexer_consume_token (parser->lexer);
      attr_ns = attr_id;

      token = cp_lexer_consume_token (parser->lexer);
      if (token->type == CPP_NAME)
	attr_id = token->u.value;
      else if (token->type == CPP_KEYWORD)
	attr_id = ridpointers[(int) token->keyword];
      else
	{
	  error_at (token->location,
		    "expected an identifier for the attribute name");
	  return error_mark_node;
	}
      attribute = build_tree_list (build_tree_list (attr_ns, attr_id),
				   NULL_TREE);
      token = cp_lexer_peek_token (parser->lexer);
    }
  else
    {
      attribute = build_tree_list (build_tree_list (NULL_TREE, attr_id),
				   NULL_TREE);
      /* C++11 noreturn attribute is equivalent to GNU's.  */
      if (is_attribute_p ("noreturn", attr_id))
	TREE_PURPOSE (TREE_PURPOSE (attribute)) = get_identifier ("gnu");
      /* C++14 deprecated attribute is equivalent to GNU's.  */
      else if (cxx_dialect >= cxx1y && is_attribute_p ("deprecated", attr_id))
	TREE_PURPOSE (TREE_PURPOSE (attribute)) = get_identifier ("gnu");
    }

  /* Now parse the optional argument clause of the attribute.  */

  if (token->type != CPP_OPEN_PAREN)
    return attribute;

  {
    vec<tree, va_gc> *vec;
    int attr_flag = normal_attr;

    if (attr_ns == get_identifier ("gnu")
	&& attribute_takes_identifier_p (attr_id))
      /* A GNU attribute that takes an identifier in parameter.  */
      attr_flag = id_attr;

    vec = cp_parser_parenthesized_expression_list
      (parser, attr_flag, /*cast_p=*/false,
       /*allow_expansion_p=*/true,
       /*non_constant_p=*/NULL);
    if (vec == NULL)
      arguments = error_mark_node;
    else
      {
	arguments = build_tree_list_vec (vec);
	release_tree_vector (vec);
      }

    if (arguments == error_mark_node)
      attribute = error_mark_node;
    else
      TREE_VALUE (attribute) = arguments;
  }

  return attribute;
}

/* Parse a list of standard C++-11 attributes.

   attribute-list:
     attribute [opt]
     attribute-list , attribute[opt]
     attribute ...
     attribute-list , attribute ...
*/

static tree
cp_parser_std_attribute_list (cp_parser *parser)
{
  tree attributes = NULL_TREE, attribute = NULL_TREE;
  cp_token *token = NULL;

  while (true)
    {
      attribute = cp_parser_std_attribute (parser);
      if (attribute == error_mark_node)
	break;
      if (attribute != NULL_TREE)
	{
	  TREE_CHAIN (attribute) = attributes;
	  attributes = attribute;
	}
      token = cp_lexer_peek_token (parser->lexer);
      if (token->type != CPP_COMMA)
	break;
      cp_lexer_consume_token (parser->lexer);
    }
  attributes = nreverse (attributes);
  return attributes;
}

/* Parse a standard C++-11 attribute specifier.

   attribute-specifier:
     [ [ attribute-list ] ]
     alignment-specifier

   alignment-specifier:
     alignas ( type-id ... [opt] )
     alignas ( alignment-expression ... [opt] ).  */

static tree
cp_parser_std_attribute_spec (cp_parser *parser)
{
  tree attributes = NULL_TREE;
  cp_token *token = cp_lexer_peek_token (parser->lexer);

  if (token->type == CPP_OPEN_SQUARE
      && cp_lexer_peek_nth_token (parser->lexer, 2)->type == CPP_OPEN_SQUARE)
    {
      cp_lexer_consume_token (parser->lexer);
      cp_lexer_consume_token (parser->lexer);

      attributes = cp_parser_std_attribute_list (parser);

      if (!cp_parser_require (parser, CPP_CLOSE_SQUARE, RT_CLOSE_SQUARE)
	  || !cp_parser_require (parser, CPP_CLOSE_SQUARE, RT_CLOSE_SQUARE))
	cp_parser_skip_to_end_of_statement (parser);
      else
	/* Warn about parsing c++11 attribute in non-c++1 mode, only
	   when we are sure that we have actually parsed them.  */
	maybe_warn_cpp0x (CPP0X_ATTRIBUTES);
    }
  else
    {
      tree alignas_expr;

      /* Look for an alignment-specifier.  */

      token = cp_lexer_peek_token (parser->lexer);

      if (token->type != CPP_KEYWORD
	  || token->keyword != RID_ALIGNAS)
	return NULL_TREE;

      cp_lexer_consume_token (parser->lexer);
      maybe_warn_cpp0x (CPP0X_ATTRIBUTES);

      if (cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN) == NULL)
	{
	  cp_parser_error (parser, "expected %<(%>");
	  return error_mark_node;
	}

      cp_parser_parse_tentatively (parser);
      alignas_expr = cp_parser_type_id (parser);

      if (!cp_parser_parse_definitely (parser))
	{
	  gcc_assert (alignas_expr == error_mark_node
		      || alignas_expr == NULL_TREE);

	  alignas_expr =
	    cp_parser_assignment_expression (parser, /*cast_p=*/false,
					     /**cp_id_kind=*/NULL);
	  if (alignas_expr == error_mark_node)
	    cp_parser_skip_to_end_of_statement (parser);
	  if (alignas_expr == NULL_TREE
	      || alignas_expr == error_mark_node)
	    return alignas_expr;
	}

      if (cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN) == NULL)
	{
	  cp_parser_error (parser, "expected %<)%>");
	  return error_mark_node;
	}

      alignas_expr = cxx_alignas_expr (alignas_expr);

      /* Build the C++-11 representation of an 'aligned'
	 attribute.  */
      attributes =
	build_tree_list (build_tree_list (get_identifier ("gnu"),
					  get_identifier ("aligned")),
			 build_tree_list (NULL_TREE, alignas_expr));
    }

  return attributes;
}

/* Parse a standard C++-11 attribute-specifier-seq.

   attribute-specifier-seq:
     attribute-specifier-seq [opt] attribute-specifier
 */

static tree
cp_parser_std_attribute_spec_seq (cp_parser *parser)
{
  tree attr_specs = NULL;

  while (true)
    {
      tree attr_spec = cp_parser_std_attribute_spec (parser);
      if (attr_spec == NULL_TREE)
	break;
      if (attr_spec == error_mark_node)
	return error_mark_node;

      TREE_CHAIN (attr_spec) = attr_specs;
      attr_specs = attr_spec;
    }

  attr_specs = nreverse (attr_specs);
  return attr_specs;
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
  cp_parser_require_keyword (parser, RID_LABEL, RT_LABEL);

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
      cp_parser_require (parser, CPP_COMMA, RT_COMMA);
    }

  /* Look for the final `;'.  */
  cp_parser_require (parser, CPP_SEMICOLON, RT_SEMICOLON);
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
  tree decl;
  tree object_type = parser->context->object_type;

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
  gcc_assert (identifier_p (name));

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

	  /* If the PARSER->SCOPE is a template specialization, it
	     may be instantiated during name lookup.  In that case,
	     errors may be issued.  Even if we rollback the current
	     tentative parse, those errors are valid.  */
	  decl = lookup_qualified_name (parser->scope, name,
					tag_type != none_type,
					/*complain=*/true);

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
	      && DECL_SELF_REFERENCE_P (decl)
	      && same_type_p (DECL_CONTEXT (decl), parser->scope))
	    decl = lookup_qualified_name (parser->scope, ctor_identifier,
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
	      if (type != error_mark_node)
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
      /* Look up the name in the scope of the OBJECT_TYPE, unless the
	 OBJECT_TYPE is not a class.  */
      if (CLASS_TYPE_P (object_type))
	/* If the OBJECT_TYPE is a template specialization, it may
	   be instantiated during name lookup.  In that case, errors
	   may be issued.  Even if we rollback the current tentative
	   parse, those errors are valid.  */
	decl = lookup_member (object_type,
			      name,
			      /*protect=*/0,
			      tag_type != none_type,
			      tf_warning_or_error);
      else
	decl = NULL_TREE;

      if (!decl)
	/* Look it up in the enclosing context.  */
	decl = lookup_name_real (name, tag_type != none_type,
				 /*nonclass=*/0,
				 /*block_p=*/true, is_namespace, 0);
      parser->object_scope = object_type;
      parser->qualifying_scope = NULL_TREE;
    }
  else
    {
      decl = lookup_name_real (name, tag_type != none_type,
			       /*nonclass=*/0,
			       /*block_p=*/true, is_namespace, 0);
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

  maybe_record_typedef_use (decl);

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
  switch (declarator->kind)
    {
    case cdk_id:
      {
	unsigned num_templates = 0;
	tree scope = declarator->u.id.qualifying_scope;

	if (scope)
	  num_templates = num_template_headers_for_class (scope);
	else if (TREE_CODE (declarator->u.id.unqualified_name)
		 == TEMPLATE_ID_EXPR)
	  /* If the DECLARATOR has the form `X<y>' then it uses one
	     additional level of template parameters.  */
	  ++num_templates;

	return cp_parser_check_template_parameters 
	  (parser, num_templates, declarator_location, declarator);
      }

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
  bool outside_class_specifier_p;
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

  outside_class_specifier_p = (!at_class_scope_p ()
			       || !TYPE_BEING_DEFINED (current_class_type)
			       || friend_p);

  /* Outside of a class-specifier, there must be a
     nested-name-specifier.  */
  if (!nested_name_specifier && outside_class_specifier_p)
    constructor_p = false;
  else if (nested_name_specifier == error_mark_node)
    constructor_p = false;

  /* If we have a class scope, this is easy; DR 147 says that S::S always
     names the constructor, and no other qualified name could.  */
  if (constructor_p && nested_name_specifier
      && CLASS_TYPE_P (nested_name_specifier))
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
      /* If there was no class-name, then this is not a constructor.
	 Otherwise, if we are in a class-specifier and we aren't
	 handling a friend declaration, check that its type matches
	 current_class_type (c++/38313).  Note: error_mark_node
	 is left alone for error recovery purposes.  */
      constructor_p = (!cp_parser_error_occurred (parser)
		       && (outside_class_specifier_p
			   || type_decl == error_mark_node
			   || same_type_p (current_class_type,
					   TREE_TYPE (type_decl))));

      /* If we're still considering a constructor, we have to see a `(',
	 to begin the parameter-declaration-clause, followed by either a
	 `)', an `...', or a decl-specifier.  We need to check for a
	 type-specifier to avoid being fooled into thinking that:

	   S (f) (int);

	 is a constructor.  (It is actually a function named `f' that
	 takes one parameter (of type `int') and returns a value of type
	 `S'.  */
      if (constructor_p
	  && !cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN))
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
  perform_deferred_access_checks (tf_warning_or_error);

  if (success_p)
    {
      cp_finalize_omp_declare_simd (parser, current_function_decl);
      parser->omp_declare_simd = NULL;
    }

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
    {
      timevar_id_t tv;
      if (DECL_DECLARED_INLINE_P (current_function_decl))
        tv = TV_PARSE_INLINE;
      else
        tv = TV_PARSE_FUNC;
      timevar_push (tv);
      fn = cp_parser_function_definition_after_declarator (parser,
							 /*inline_p=*/false);
      timevar_pop (tv);
    }

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
  bool fully_implicit_function_template_p
    = parser->fully_implicit_function_template_p;
  parser->fully_implicit_function_template_p = false;
  tree implicit_template_parms
    = parser->implicit_template_parms;
  parser->implicit_template_parms = 0;
  cp_binding_level* implicit_template_scope
    = parser->implicit_template_scope;
  parser->implicit_template_scope = 0;

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

  /* If the next token is `try', `__transaction_atomic', or
     `__transaction_relaxed`, then we are looking at either function-try-block
     or function-transaction-block.  Note that all of these include the
     function-body.  */
  if (cp_lexer_next_token_is_keyword (parser->lexer, RID_TRANSACTION_ATOMIC))
    ctor_initializer_p = cp_parser_function_transaction (parser,
	RID_TRANSACTION_ATOMIC);
  else if (cp_lexer_next_token_is_keyword (parser->lexer,
      RID_TRANSACTION_RELAXED))
    ctor_initializer_p = cp_parser_function_transaction (parser,
	RID_TRANSACTION_RELAXED);
  else if (cp_lexer_next_token_is_keyword (parser->lexer, RID_TRY))
    ctor_initializer_p = cp_parser_function_try_block (parser);
  else
    ctor_initializer_p = cp_parser_ctor_initializer_opt_and_function_body
      (parser, /*in_function_try_block=*/false);

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

  parser->fully_implicit_function_template_p
    = fully_implicit_function_template_p;
  parser->implicit_template_parms
    = implicit_template_parms;
  parser->implicit_template_scope
    = implicit_template_scope;

  if (parser->fully_implicit_function_template_p)
    finish_fully_implicit_template (parser, /*member_decl_opt=*/0);

  return fn;
}

/* Parse a template-declaration, assuming that the `export' (and
   `extern') keywords, if present, has already been scanned.  MEMBER_P
   is as for cp_parser_template_declaration.  */

static void
cp_parser_template_declaration_after_export (cp_parser* parser, bool member_p)
{
  tree decl = NULL_TREE;
  vec<deferred_access_check, va_gc> *checks;
  tree parameter_list;
  bool friend_p = false;
  bool need_lang_pop;
  cp_token *token;

  /* Look for the `template' keyword.  */
  token = cp_lexer_peek_token (parser->lexer);
  if (!cp_parser_require_keyword (parser, RID_TEMPLATE, RT_TEMPLATE))
    return;

  /* And the `<'.  */
  if (!cp_parser_require (parser, CPP_LESS, RT_LESS))
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
    {
      /* Parse the template parameters.  */
      parameter_list = cp_parser_template_parameter_list (parser);
    }

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
  else if (cxx_dialect >= cxx11
	   && cp_lexer_next_token_is_keyword (parser->lexer, RID_USING))
    decl = cp_parser_alias_declaration (parser);
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
      else if (friend_p && decl
	       && DECL_DECLARES_TYPE_P (decl))
	make_friend_class (current_class_type, TREE_TYPE (decl),
			   /*complain=*/true);
    }
  /* We are done with the current parameter list.  */
  --parser->num_template_parameter_lists;

  pop_deferring_access_checks ();

  /* Finish up.  */
  finish_template_decl (parameter_list);

  /* Check the template arguments for a literal operator template.  */
  if (decl
      && DECL_DECLARES_FUNCTION_P (decl)
      && UDLIT_OPER_P (DECL_NAME (decl)))
    {
      bool ok = true;
      if (parameter_list == NULL_TREE)
	ok = false;
      else
	{
	  int num_parms = TREE_VEC_LENGTH (parameter_list);
	  if (num_parms == 1)
	    {
	      tree parm_list = TREE_VEC_ELT (parameter_list, 0);
	      tree parm = INNERMOST_TEMPLATE_PARMS (parm_list);
	      if (TREE_TYPE (parm) != char_type_node
		  || !TEMPLATE_PARM_PARAMETER_PACK (DECL_INITIAL (parm)))
		ok = false;
	    }
	  else if (num_parms == 2 && cxx_dialect >= cxx1y)
	    {
	      tree parm_type = TREE_VEC_ELT (parameter_list, 0);
	      tree type = INNERMOST_TEMPLATE_PARMS (parm_type);
	      tree parm_list = TREE_VEC_ELT (parameter_list, 1);
	      tree parm = INNERMOST_TEMPLATE_PARMS (parm_list);
	      if (TREE_TYPE (parm) != TREE_TYPE (type)
		  || !TEMPLATE_PARM_PARAMETER_PACK (DECL_INITIAL (parm)))
		ok = false;
	    }
	  else
	    ok = false;
	}
      if (!ok)
	error ("literal operator template %qD has invalid parameter list."
	       "  Expected non-type template argument pack <char...>"
	       " or <typename CharT, CharT...>",
	       decl);
    }
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
      && DECL_DECLARES_FUNCTION_P (decl))
    vec_safe_push (unparsed_funs_with_definitions, decl);
}

/* Perform the deferred access checks from a template-parameter-list.
   CHECKS is a TREE_LIST of access checks, as returned by
   get_deferred_access_checks.  */

static void
cp_parser_perform_template_parameter_access_checks (vec<deferred_access_check, va_gc> *checks)
{
  ++processing_template_parmlist;
  perform_access_checks (checks, tf_warning_or_error);
  --processing_template_parmlist;
}

/* Parse a `decl-specifier-seq [opt] init-declarator [opt] ;' or
   `function-definition' sequence that follows a template header.
   If MEMBER_P is true, this declaration appears in a class scope.

   Returns the DECL for the declared entity.  If FRIEND_P is non-NULL,
   *FRIEND_P is set to TRUE iff the declaration is a friend.  */

static tree
cp_parser_single_declaration (cp_parser* parser,
			      vec<deferred_access_check, va_gc> *checks,
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
  if (decl_spec_seq_has_spec_p (&decl_specifiers, ds_typedef))
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
  if (!decl_specifiers.any_type_specifiers_p
      && cp_parser_parse_and_diagnose_invalid_type_name (parser))
    {
      /* cp_parser_parse_and_diagnose_invalid_type_name calls
	 cp_parser_skip_to_end_of_block_or_statement, so don't try to parse
	 the rest of this declaration.  */
      decl = error_mark_node;
      goto out;
    }

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
				        &function_definition_p,
					NULL);

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

    if (decl && VAR_P (decl))
      check_template_variable (decl);
    }

  /* Look for a trailing `;' after the declaration.  */
  if (!function_definition_p
      && (decl == error_mark_node
	  || !cp_parser_require (parser, CPP_SEMICOLON, RT_SEMICOLON)))
    cp_parser_skip_to_end_of_block_or_statement (parser);

 out:
  pop_deferring_access_checks ();

  /* Clear any current qualification; whatever comes next is the start
     of something new.  */
  parser->scope = NULL_TREE;
  parser->qualifying_scope = NULL_TREE;
  parser->object_scope = NULL_TREE;

  return decl;
}

/* Parse a cast-expression that is not the operand of a unary "&".  */

static tree
cp_parser_simple_cast_expression (cp_parser *parser)
{
  return cp_parser_cast_expression (parser, /*address_p=*/false,
				    /*cast_p=*/false, /*decltype*/false, NULL);
}

/* Parse a functional cast to TYPE.  Returns an expression
   representing the cast.  */

static tree
cp_parser_functional_cast (cp_parser* parser, tree type)
{
  vec<tree, va_gc> *vec;
  tree expression_list;
  tree cast;
  bool nonconst_p;

  if (!type)
    type = error_mark_node;

  if (cp_lexer_next_token_is (parser->lexer, CPP_OPEN_BRACE))
    {
      cp_token *token = cp_lexer_peek_token (parser->lexer);
      cp_lexer_set_source_position_from_token (token);
      maybe_warn_cpp0x (CPP0X_INITIALIZER_LISTS);
      expression_list = cp_parser_braced_list (parser, &nonconst_p);
      CONSTRUCTOR_IS_DIRECT_INIT (expression_list) = 1;
      if (TREE_CODE (type) == TYPE_DECL)
	type = TREE_TYPE (type);
      return finish_compound_literal (type, expression_list,
				      tf_warning_or_error);
    }


  vec = cp_parser_parenthesized_expression_list (parser, non_attr,
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
      && cp_parser_non_integral_constant_expression (parser,
						     NIC_CONSTRUCTOR))
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
  cp_finalize_omp_declare_simd (parser, fn);
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
  /* Handle function try blocks.  */
  if (cp_lexer_next_token_is_keyword (parser->lexer, RID_TRY))
    cp_lexer_consume_token (parser->lexer);
  /* We can have braced-init-list mem-initializers before the fn body.  */
  if (cp_lexer_next_token_is (parser->lexer, CPP_COLON))
    {
      cp_lexer_consume_token (parser->lexer);
      while (cp_lexer_next_token_is_not (parser->lexer, CPP_OPEN_BRACE))
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
  vec_safe_push (unparsed_funs_with_definitions, fn);

  return fn;
}

/* Save the tokens that make up the in-class initializer for a non-static
   data member.  Returns a DEFAULT_ARG.  */

static tree
cp_parser_save_nsdmi (cp_parser* parser)
{
  return cp_parser_cache_defarg (parser, /*nsdmi=*/true);
}

/* Parse a template-argument-list, as well as the trailing ">" (but
   not the opening "<").  See cp_parser_template_argument_list for the
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
  timevar_push (TV_PARSE_INMETH);
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
  push_unparsed_function_queues (parser);

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

      /* #pragma omp declare reduction needs special parsing.  */
      if (DECL_OMP_DECLARE_REDUCTION_P (member_function))
	{
	  parser->lexer->in_pragma = true;
	  cp_parser_omp_declare_reduction_exprs (member_function, parser);
	  finish_function (/*inline*/2);
	  cp_check_omp_declare_reduction (member_function);
	}
      else
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
  pop_unparsed_function_queues (parser);
  timevar_pop (TV_PARSE_INMETH);
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
	cp_default_arg_entry entry = {current_class_type, decl};
	vec_safe_push (unparsed_funs_with_default_args, entry);
	break;
      }
}

/* DEFAULT_ARG contains the saved tokens for the initializer of DECL,
   which is either a FIELD_DECL or PARM_DECL.  Parse it and return
   the result.  For a PARM_DECL, PARMTYPE is the corresponding type
   from the parameter-type-list.  */

static tree
cp_parser_late_parse_one_default_arg (cp_parser *parser, tree decl,
				      tree default_arg, tree parmtype)
{
  cp_token_cache *tokens;
  tree parsed_arg;
  bool dummy;

  if (default_arg == error_mark_node)
    return error_mark_node;

  /* Push the saved tokens for the default argument onto the parser's
     lexer stack.  */
  tokens = DEFARG_TOKENS (default_arg);
  cp_parser_push_lexer_for_tokens (parser, tokens);

  start_lambda_scope (decl);

  /* Parse the default argument.  */
  parsed_arg = cp_parser_initializer (parser, &dummy, &dummy);
  if (BRACE_ENCLOSED_INITIALIZER_P (parsed_arg))
    maybe_warn_cpp0x (CPP0X_INITIALIZER_LISTS);

  finish_lambda_scope ();

  if (parsed_arg == error_mark_node)
    cp_parser_skip_to_end_of_statement (parser);

  if (!processing_template_decl)
    {
      /* In a non-template class, check conversions now.  In a template,
	 we'll wait and instantiate these as needed.  */
      if (TREE_CODE (decl) == PARM_DECL)
	parsed_arg = check_default_argument (parmtype, parsed_arg,
					     tf_warning_or_error);
      else
	{
	  int flags = LOOKUP_IMPLICIT;
	  if (BRACE_ENCLOSED_INITIALIZER_P (parsed_arg)
	      && CONSTRUCTOR_IS_DIRECT_INIT (parsed_arg))
	    flags = LOOKUP_NORMAL;
	  parsed_arg = digest_init_flags (TREE_TYPE (decl), parsed_arg, flags);
	  if (TREE_CODE (parsed_arg) == TARGET_EXPR)
	    /* This represents the whole initialization.  */
	    TARGET_EXPR_DIRECT_INIT_P (parsed_arg) = true;
	}
    }

  /* If the token stream has not been completely used up, then
     there was extra junk after the end of the default
     argument.  */
  if (!cp_lexer_next_token_is (parser->lexer, CPP_EOF))
    {
      if (TREE_CODE (decl) == PARM_DECL)
	cp_parser_error (parser, "expected %<,%>");
      else
	cp_parser_error (parser, "expected %<;%>");
    }

  /* Revert to the main lexer.  */
  cp_parser_pop_lexer (parser);

  return parsed_arg;
}

/* FIELD is a non-static data member with an initializer which we saved for
   later; parse it now.  */

static void
cp_parser_late_parsing_nsdmi (cp_parser *parser, tree field)
{
  tree def;

  maybe_begin_member_template_processing (field);

  push_unparsed_function_queues (parser);
  def = cp_parser_late_parse_one_default_arg (parser, field,
					      DECL_INITIAL (field),
					      NULL_TREE);
  pop_unparsed_function_queues (parser);

  maybe_end_member_template_processing ();

  DECL_INITIAL (field) = def;
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
  push_unparsed_function_queues (parser);

  /* Local variable names (and the `this' keyword) may not appear
     in a default argument.  */
  saved_local_variables_forbidden_p = parser->local_variables_forbidden_p;
  parser->local_variables_forbidden_p = true;

  push_defarg_context (fn);

  for (parm = TYPE_ARG_TYPES (TREE_TYPE (fn)),
	 parmdecl = DECL_ARGUMENTS (fn);
       parm && parm != void_list_node;
       parm = TREE_CHAIN (parm),
	 parmdecl = DECL_CHAIN (parmdecl))
    {
      tree default_arg = TREE_PURPOSE (parm);
      tree parsed_arg;
      vec<tree, va_gc> *insts;
      tree copy;
      unsigned ix;

      if (!default_arg)
	continue;

      if (TREE_CODE (default_arg) != DEFAULT_ARG)
	/* This can happen for a friend declaration for a function
	   already declared with default arguments.  */
	continue;

      parsed_arg
	= cp_parser_late_parse_one_default_arg (parser, parmdecl,
						default_arg,
						TREE_VALUE (parm));
      if (parsed_arg == error_mark_node)
	{
	  continue;
	}

      TREE_PURPOSE (parm) = parsed_arg;

      /* Update any instantiations we've already created.  */
      for (insts = DEFARG_INSTANTIATIONS (default_arg), ix = 0;
	   vec_safe_iterate (insts, ix, &copy); ix++)
	TREE_PURPOSE (copy) = parsed_arg;
    }

  pop_defarg_context ();

  /* Make sure no default arg is missing.  */
  check_default_args (fn);

  /* Restore the state of local_variables_forbidden_p.  */
  parser->local_variables_forbidden_p = saved_local_variables_forbidden_p;

  /* Restore the queue.  */
  pop_unparsed_function_queues (parser);
}

/* Subroutine of cp_parser_sizeof_operand, for handling C++11

     sizeof ... ( identifier )

   where the 'sizeof' token has already been consumed.  */

static tree
cp_parser_sizeof_pack (cp_parser *parser)
{
  /* Consume the `...'.  */
  cp_lexer_consume_token (parser->lexer);
  maybe_warn_variadic_templates ();

  bool paren = cp_lexer_next_token_is (parser->lexer, CPP_OPEN_PAREN);
  if (paren)
    cp_lexer_consume_token (parser->lexer);
  else
    permerror (cp_lexer_peek_token (parser->lexer)->location,
	       "%<sizeof...%> argument must be surrounded by parentheses");

  cp_token *token = cp_lexer_peek_token (parser->lexer);
  tree name = cp_parser_identifier (parser);
  if (name == error_mark_node)
    return error_mark_node;
  /* The name is not qualified.  */
  parser->scope = NULL_TREE;
  parser->qualifying_scope = NULL_TREE;
  parser->object_scope = NULL_TREE;
  tree expr = cp_parser_lookup_name_simple (parser, name, token->location);
  if (expr == error_mark_node)
    cp_parser_name_lookup_error (parser, name, expr, NLE_NULL,
				 token->location);
  if (TREE_CODE (expr) == TYPE_DECL)
    expr = TREE_TYPE (expr);
  else if (TREE_CODE (expr) == CONST_DECL)
    expr = DECL_INITIAL (expr);
  expr = make_pack_expansion (expr);

  if (paren)
    cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN);

  return expr;
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

  /* If it's a `...', then we are computing the length of a parameter
     pack.  */
  if (keyword == RID_SIZEOF
      && cp_lexer_next_token_is (parser->lexer, CPP_ELLIPSIS))
    return cp_parser_sizeof_pack (parser);

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

  /* Do not actually evaluate the expression.  */
  ++cp_unevaluated_operand;
  ++c_inhibit_evaluation_warnings;
  /* If it's a `(', then we might be looking at the type-id
     construction.  */
  if (cp_lexer_next_token_is (parser->lexer, CPP_OPEN_PAREN))
    {
      tree type = NULL_TREE;
      bool compound_literal_p;

      /* We can't be sure yet whether we're looking at a type-id or an
	 expression.  */
      cp_parser_parse_tentatively (parser);
      /* Consume the `('.  */
      cp_lexer_consume_token (parser->lexer);
      /* Note: as a GNU Extension, compound literals are considered
	 postfix-expressions as they are in C99, so they are valid
	 arguments to sizeof.  See comment in cp_parser_cast_expression
	 for details.  */
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
	  cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN);
	  parser->in_type_id_in_expr_p = saved_in_type_id_in_expr_p;
	}

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
			     cp_token *token)
{
  cp_storage_class storage_class;

  if (parser->in_unbraced_linkage_specification_p)
    {
      error_at (token->location, "invalid use of %qD in linkage specification",
		ridpointers[keyword]);
      return;
    }
  else if (decl_specs->storage_class != sc_none)
    {
      decl_specs->conflicting_specifiers_p = true;
      return;
    }

  if ((keyword == RID_EXTERN || keyword == RID_STATIC)
      && decl_spec_seq_has_spec_p (decl_specs, ds_thread)
      && decl_specs->gnu_thread_keyword_p)
    {
      pedwarn (decl_specs->locations[ds_thread], 0,
		"%<__thread%> before %qD", ridpointers[keyword]);
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
  set_and_check_decl_spec_loc (decl_specs, ds_storage_class, token);

  /* A storage class specifier cannot be applied alongside a typedef 
     specifier. If there is a typedef specifier present then set 
     conflicting_specifiers_p which will trigger an error later
     on in grokdeclarator. */
  if (decl_spec_seq_has_spec_p (decl_specs, ds_typedef))
    decl_specs->conflicting_specifiers_p = true;
}

/* Update the DECL_SPECS to reflect the TYPE_SPEC.  If TYPE_DEFINITION_P
   is true, the type is a class or enum definition.  */

static void
cp_parser_set_decl_spec_type (cp_decl_specifier_seq *decl_specs,
			      tree type_spec,
			      cp_token *token,
			      bool type_definition_p)
{
  decl_specs->any_specifiers_p = true;

  /* If the user tries to redeclare bool, char16_t, char32_t, or wchar_t
     (with, for example, in "typedef int wchar_t;") we remember that
     this is what happened.  In system headers, we ignore these
     declarations so that G++ can work with system headers that are not
     C++-safe.  */
  if (decl_spec_seq_has_spec_p (decl_specs, ds_typedef)
      && !type_definition_p
      && (type_spec == boolean_type_node
	  || type_spec == char16_type_node
	  || type_spec == char32_type_node
	  || type_spec == wchar_type_node)
      && (decl_specs->type
	  || decl_spec_seq_has_spec_p (decl_specs, ds_long)
	  || decl_spec_seq_has_spec_p (decl_specs, ds_short)
	  || decl_spec_seq_has_spec_p (decl_specs, ds_unsigned)
	  || decl_spec_seq_has_spec_p (decl_specs, ds_signed)))
    {
      decl_specs->redefined_builtin_type = type_spec;
      set_and_check_decl_spec_loc (decl_specs,
				   ds_redefined_builtin_type_spec,
				   token);
      if (!decl_specs->type)
	{
	  decl_specs->type = type_spec;
	  decl_specs->type_definition_p = false;
	  set_and_check_decl_spec_loc (decl_specs,ds_type_spec, token);
	}
    }
  else if (decl_specs->type)
    decl_specs->multiple_types_p = true;
  else
    {
      decl_specs->type = type_spec;
      decl_specs->type_definition_p = type_definition_p;
      decl_specs->redefined_builtin_type = NULL_TREE;
      set_and_check_decl_spec_loc (decl_specs, ds_type_spec, token);
    }
}

/* True iff TOKEN is the GNU keyword __thread.  */

static bool
token_is__thread (cp_token *token)
{
  gcc_assert (token->keyword == RID_THREAD);
  return !strcmp (IDENTIFIER_POINTER (token->u.value), "__thread");
}

/* Set the location for a declarator specifier and check if it is
   duplicated.

   DECL_SPECS is the sequence of declarator specifiers onto which to
   set the location.

   DS is the single declarator specifier to set which location  is to
   be set onto the existing sequence of declarators.

   LOCATION is the location for the declarator specifier to
   consider.  */

static void
set_and_check_decl_spec_loc (cp_decl_specifier_seq *decl_specs,
			     cp_decl_spec ds, cp_token *token)
{
  gcc_assert (ds < ds_last);

  if (decl_specs == NULL)
    return;

  source_location location = token->location;

  if (decl_specs->locations[ds] == 0)
    {
      decl_specs->locations[ds] = location;
      if (ds == ds_thread)
	decl_specs->gnu_thread_keyword_p = token_is__thread (token);
    }
  else
    {
      if (ds == ds_long)
	{
	  if (decl_specs->locations[ds_long_long] != 0)
	    error_at (location,
		      "%<long long long%> is too long for GCC");
	  else
	    {
	      decl_specs->locations[ds_long_long] = location;
	      pedwarn_cxx98 (location,
			     OPT_Wlong_long, 
			     "ISO C++ 1998 does not support %<long long%>");
	    }
	}
      else if (ds == ds_thread)
	{
	  bool gnu = token_is__thread (token);
	  if (gnu != decl_specs->gnu_thread_keyword_p)
	    error_at (location,
		      "both %<__thread%> and %<thread_local%> specified");
	  else
	    error_at (location, "duplicate %qD", token->u.value);
	}
      else
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
	    "using",
            "constexpr",
	    "__complex"
	  };
	  error_at (location,
		    "duplicate %qs", decl_spec_names[ds]);
	}
    }
}

/* Return true iff the declarator specifier DS is present in the
   sequence of declarator specifiers DECL_SPECS.  */

bool
decl_spec_seq_has_spec_p (const cp_decl_specifier_seq * decl_specs,
			  cp_decl_spec ds)
{
  gcc_assert (ds < ds_last);

  if (decl_specs == NULL)
    return false;

  return decl_specs->locations[ds] != 0;
}

/* DECL_SPECIFIERS is the representation of a decl-specifier-seq.
   Returns TRUE iff `friend' appears among the DECL_SPECIFIERS.  */

static bool
cp_parser_friend_p (const cp_decl_specifier_seq *decl_specifiers)
{
  return decl_spec_seq_has_spec_p (decl_specifiers, ds_friend);
}

/* Issue an error message indicating that TOKEN_DESC was expected.
   If KEYWORD is true, it indicated this function is called by
   cp_parser_require_keword and the required token can only be
   a indicated keyword. */

static void
cp_parser_required_error (cp_parser *parser,
			  required_token token_desc,
			  bool keyword)
{
  switch (token_desc)
    {
      case RT_NEW:
	cp_parser_error (parser, "expected %<new%>");
	return;
      case RT_DELETE:
	cp_parser_error (parser, "expected %<delete%>");
	return;
      case RT_RETURN:
	cp_parser_error (parser, "expected %<return%>");
	return;
      case RT_WHILE:
	cp_parser_error (parser, "expected %<while%>");
	return;
      case RT_EXTERN:
	cp_parser_error (parser, "expected %<extern%>");
	return;
      case RT_STATIC_ASSERT:
	cp_parser_error (parser, "expected %<static_assert%>");
	return;
      case RT_DECLTYPE:
	cp_parser_error (parser, "expected %<decltype%>");
	return;
      case RT_OPERATOR:
	cp_parser_error (parser, "expected %<operator%>");
	return;
      case RT_CLASS:
	cp_parser_error (parser, "expected %<class%>");
	return;
      case RT_TEMPLATE:
	cp_parser_error (parser, "expected %<template%>");
	return;
      case RT_NAMESPACE:
	cp_parser_error (parser, "expected %<namespace%>");
	return;
      case RT_USING:
	cp_parser_error (parser, "expected %<using%>");
	return;
      case RT_ASM:
	cp_parser_error (parser, "expected %<asm%>");
	return;
      case RT_TRY:
	cp_parser_error (parser, "expected %<try%>");
	return;
      case RT_CATCH:
	cp_parser_error (parser, "expected %<catch%>");
	return;
      case RT_THROW:
	cp_parser_error (parser, "expected %<throw%>");
	return;
      case RT_LABEL:
	cp_parser_error (parser, "expected %<__label__%>");
	return;
      case RT_AT_TRY:
	cp_parser_error (parser, "expected %<@try%>");
	return;
      case RT_AT_SYNCHRONIZED:
	cp_parser_error (parser, "expected %<@synchronized%>");
	return;
      case RT_AT_THROW:
	cp_parser_error (parser, "expected %<@throw%>");
	return;
      case RT_TRANSACTION_ATOMIC:
	cp_parser_error (parser, "expected %<__transaction_atomic%>");
	return;
      case RT_TRANSACTION_RELAXED:
	cp_parser_error (parser, "expected %<__transaction_relaxed%>");
	return;
      default:
	break;
    }
  if (!keyword)
    {
      switch (token_desc)
        {
	  case RT_SEMICOLON:
	    cp_parser_error (parser, "expected %<;%>");
	    return;
	  case RT_OPEN_PAREN:
	    cp_parser_error (parser, "expected %<(%>");
	    return;
	  case RT_CLOSE_BRACE:
	    cp_parser_error (parser, "expected %<}%>");
	    return;
	  case RT_OPEN_BRACE:
	    cp_parser_error (parser, "expected %<{%>");
	    return;
	  case RT_CLOSE_SQUARE:
	    cp_parser_error (parser, "expected %<]%>");
	    return;
	  case RT_OPEN_SQUARE:
	    cp_parser_error (parser, "expected %<[%>");
	    return;
	  case RT_COMMA:
	    cp_parser_error (parser, "expected %<,%>");
	    return;
	  case RT_SCOPE:
	    cp_parser_error (parser, "expected %<::%>");
	    return;
	  case RT_LESS:
	    cp_parser_error (parser, "expected %<<%>");
	    return;
	  case RT_GREATER:
	    cp_parser_error (parser, "expected %<>%>");
	    return;
	  case RT_EQ:
	    cp_parser_error (parser, "expected %<=%>");
	    return;
	  case RT_ELLIPSIS:
	    cp_parser_error (parser, "expected %<...%>");
	    return;
	  case RT_MULT:
	    cp_parser_error (parser, "expected %<*%>");
	    return;
	  case RT_COMPL:
	    cp_parser_error (parser, "expected %<~%>");
	    return;
	  case RT_COLON:
	    cp_parser_error (parser, "expected %<:%>");
	    return;
	  case RT_COLON_SCOPE:
	    cp_parser_error (parser, "expected %<:%> or %<::%>");
	    return;
	  case RT_CLOSE_PAREN:
	    cp_parser_error (parser, "expected %<)%>");
	    return;
	  case RT_COMMA_CLOSE_PAREN:
	    cp_parser_error (parser, "expected %<,%> or %<)%>");
	    return;
	  case RT_PRAGMA_EOL:
	    cp_parser_error (parser, "expected end of line");
	    return;
	  case RT_NAME:
	    cp_parser_error (parser, "expected identifier");
	    return;
	  case RT_SELECT:
	    cp_parser_error (parser, "expected selection-statement");
	    return;
	  case RT_INTERATION:
	    cp_parser_error (parser, "expected iteration-statement");
	    return;
	  case RT_JUMP:
	    cp_parser_error (parser, "expected jump-statement");
	    return;
	  case RT_CLASS_KEY:
	    cp_parser_error (parser, "expected class-key");
	    return;
	  case RT_CLASS_TYPENAME_TEMPLATE:
	    cp_parser_error (parser,
	  	 "expected %<class%>, %<typename%>, or %<template%>");
	    return;
	  default:
	    gcc_unreachable ();
	}
    }
  else
    gcc_unreachable ();
}



/* If the next token is of the indicated TYPE, consume it.  Otherwise,
   issue an error message indicating that TOKEN_DESC was expected.

   Returns the token consumed, if the token had the appropriate type.
   Otherwise, returns NULL.  */

static cp_token *
cp_parser_require (cp_parser* parser,
		   enum cpp_ttype type,
		   required_token token_desc)
{
  if (cp_lexer_next_token_is (parser->lexer, type))
    return cp_lexer_consume_token (parser->lexer);
  else
    {
      /* Output the MESSAGE -- unless we're parsing tentatively.  */
      if (!cp_parser_simulate_error (parser))
	cp_parser_required_error (parser, token_desc, /*keyword=*/false);
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
  if (cp_parser_require (parser, CPP_GREATER, RT_GREATER))
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
			   required_token token_desc)
{
  cp_token *token = cp_parser_require (parser, CPP_KEYWORD, token_desc);

  if (token && token->keyword != keyword)
    {
      cp_parser_required_error (parser, token_desc, /*keyword=*/true); 
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
	  /* A function-transaction-block begins with `__transaction_atomic'
	     or `__transaction_relaxed'.  */
	  || token->keyword == RID_TRANSACTION_ATOMIC
	  || token->keyword == RID_TRANSACTION_RELAXED
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
  return (token->type == CPP_OPEN_BRACE
	  || (token->type == CPP_COLON
	      && !parser->colon_doesnt_start_class_def_p));
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
  if (type == error_mark_node)
    return;
  if ((TREE_CODE (type) == UNION_TYPE) != (class_key == union_type))
    {
      if (permerror (input_location, "%qs tag used in naming %q#T",
		     class_key == union_type ? "union"
		     : class_key == record_type ? "struct" : "class",
		     type))
	inform (DECL_SOURCE_LOCATION (TYPE_NAME (type)),
		"%q#T was previously declared here", type);
    }
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
      /* In C++98 the `template' keyword can only be used within templates;
	 outside templates the parser can always figure out what is a
	 template and what is not.  In C++11,  per the resolution of DR 468,
	 `template' is allowed in cases where it is not strictly necessary.  */
      if (!processing_template_decl
	  && pedantic && cxx_dialect == cxx98)
	{
	  cp_token *token = cp_lexer_peek_token (parser->lexer);
	  pedwarn (token->location, OPT_Wpedantic,
		   "in C++98 %<template%> (as a disambiguator) is only "
		   "allowed within templates");
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
  vec<deferred_access_check, va_gc> *checks;

  /* Get the stored value.  */
  check_value = cp_lexer_consume_token (parser->lexer)->u.tree_check_value;
  /* Perform any access checks that were deferred.  */
  checks = check_value->checks;
  if (checks)
    {
      FOR_EACH_VEC_SAFE_ELT (checks, i, chk)
	perform_or_defer_access_check (chk->binfo,
				       chk->decl,
				       chk->diag_decl, tf_warning_or_error);
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

/* Like above, for caching a default argument or NSDMI.  Both of these are
   terminated by a non-nested comma, but it can be unclear whether or not a
   comma is nested in a template argument list unless we do more parsing.
   In order to handle this ambiguity, when we encounter a ',' after a '<'
   we try to parse what follows as a parameter-declaration-list (in the
   case of a default argument) or a member-declarator (in the case of an
   NSDMI).  If that succeeds, then we stop caching.  */

static tree
cp_parser_cache_defarg (cp_parser *parser, bool nsdmi)
{
  unsigned depth = 0;
  int maybe_template_id = 0;
  cp_token *first_token;
  cp_token *token;
  tree default_argument;

  /* Add tokens until we have processed the entire default
     argument.  We add the range [first_token, token).  */
  first_token = cp_lexer_peek_token (parser->lexer);
  if (first_token->type == CPP_OPEN_BRACE)
    {
      /* For list-initialization, this is straightforward.  */
      cp_parser_cache_group (parser, CPP_CLOSE_BRACE, /*depth=*/0);
      token = cp_lexer_peek_token (parser->lexer);
    }
  else while (true)
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

	      if (nsdmi)
		{
		  int ctor_dtor_or_conv_p;
		  cp_parser_declarator (parser, CP_PARSER_DECLARATOR_NAMED,
					&ctor_dtor_or_conv_p,
					/*parenthesized_p=*/NULL,
					/*member_p=*/true);
		}
	      else
		{
		  begin_scope (sk_function_parms, NULL_TREE);
		  cp_parser_parameter_declaration_list (parser, &error);
		  pop_bindings_and_leave_scope ();
		}
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
	  if (depth == 0
	      /* Handle correctly int n = sizeof ... ( p );  */
	      && token->type != CPP_ELLIPSIS)
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

  return default_argument;
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

/* Commit to the topmost currently active tentative parse.

   Note that this function shouldn't be called when there are
   irreversible side-effects while in a tentative state.  For
   example, we shouldn't create a permanent entry in the symbol
   table, or issue an error message that might not apply if the
   tentative parse is aborted.  */

static void
cp_parser_commit_to_topmost_tentative_parse (cp_parser* parser)
{
  cp_parser_context *context = parser->context;
  cp_lexer *lexer = parser->lexer;

  if (context)
    {
      if (context->status == CP_PARSER_STATUS_KIND_COMMITTED)
	return;
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
  gcc_assert (parser->context->status != CP_PARSER_STATUS_KIND_COMMITTED
	      || errorcount > 0);
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
  cp_parser_require (parser, CPP_CLOSE_SQUARE, RT_CLOSE_SQUARE);

  return objc_build_message_expr (receiver, messageargs);
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
  while (true)
    {
      tree id;
      
      id = cp_parser_identifier (parser);
      if (id == error_mark_node)
	break;
      
      objc_declare_class (id);

      if (cp_lexer_next_token_is (parser->lexer, CPP_COMMA))
	cp_lexer_consume_token (parser->lexer);
      else
	break;
    }
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

  while (node && identifier_p (node)
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
      && cp_parser_is_pure_string_literal
	 (cp_lexer_peek_nth_token (parser->lexer, 2)))
    cp_parser_linkage_specification (parser);
  /* Handle #pragma, if any.  */
  else if (token->type == CPP_PRAGMA)
    cp_parser_pragma (parser, pragma_objc_icode);
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
	  objc_start_method_definition (is_class_method, sig, attribute,
					NULL_TREE);

	  /* For historical reasons, we accept an optional semicolon.  */
	  if (cp_lexer_next_token_is (parser->lexer, CPP_SEMICOLON))
	    cp_lexer_consume_token (parser->lexer);

	  ptk = cp_lexer_peek_token (parser->lexer);
	  if (!(ptk->type == CPP_PLUS || ptk->type == CPP_MINUS 
		|| ptk->type == CPP_EOF || ptk->keyword == RID_AT_END))
	    {
	      perform_deferred_access_checks (tf_warning_or_error);
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

      /* thread_local.  */
      if (decl_spec_seq_has_spec_p (&declspecs, ds_thread))
	{
	  cp_parser_error (parser, "invalid type for instance variable");
	  declspecs.locations[ds_thread] = 0;
	}
      
      /* typedef.  */
      if (decl_spec_seq_has_spec_p (&declspecs, ds_typedef))
	{
	  cp_parser_error (parser, "invalid type for instance variable");
	  declspecs.locations[ds_typedef] = 0;
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
	  if (decl != error_mark_node && decl != NULL_TREE)
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
      cp_parser_consume_semicolon_at_end_of_statement (parser);
      return;
    }

  /* See if we have a forward declaration or a definition.  */
  tok = cp_lexer_peek_nth_token (parser->lexer, 2);

  /* Try a forward declaration first.  */
  if (tok->type == CPP_COMMA || tok->type == CPP_SEMICOLON)
    {
      while (true)
	{
	  tree id;
	  
	  id = cp_parser_identifier (parser);
	  if (id == error_mark_node)
	    break;
	  
	  objc_declare_protocol (id, attributes);
	  
	  if(cp_lexer_next_token_is (parser->lexer, CPP_COMMA))
	    cp_lexer_consume_token (parser->lexer);
	  else
	    break;
	}
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
  cp_parser_compound_statement (parser, NULL, false, false);
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
      cp_parser_compound_statement (parser, NULL, false, false);
      objc_finish_catch_clause ();
    }
  if (cp_lexer_next_token_is_keyword (parser->lexer, RID_AT_FINALLY))
    {
      cp_lexer_consume_token (parser->lexer);
      location = cp_lexer_peek_token (parser->lexer)->location;
      /* NB: The @finally block needs to be wrapped in its own STATEMENT_LIST
	 node, lest it get absorbed into the surrounding block.  */
      stmt = push_stmt_list ();
      cp_parser_compound_statement (parser, NULL, false, false);
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
  cp_parser_compound_statement (parser, NULL, false, false);

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
  
  /* thread_local.  */
  if (decl_spec_seq_has_spec_p (&declspecs, ds_thread))
    {
      cp_parser_error (parser, "invalid type for property");
      declspecs.locations[ds_thread] = 0;
    }
  
  /* typedef.  */
  if (decl_spec_seq_has_spec_p (&declspecs, ds_typedef))
    {
      cp_parser_error (parser, "invalid type for property");
      declspecs.locations[ds_typedef] = 0;
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
	      if (!cp_parser_objc_selector_p (cp_lexer_peek_token (parser->lexer)->type))
		{
		  cp_parser_error (parser, "expected identifier");
		  syntax_error = true;
		  break;
		}
	      if (keyword == RID_SETTER)
		{
		  if (property_setter_ident != NULL_TREE)
		    {
		      cp_parser_error (parser, "the %<setter%> attribute may only be specified once");
		      cp_lexer_consume_token (parser->lexer);
		    }
		  else
		    property_setter_ident = cp_parser_objc_selector (parser);
		  if (cp_lexer_next_token_is_not (parser->lexer, CPP_COLON))
		    cp_parser_error (parser, "setter name must terminate with %<:%>");
		  else
		    cp_lexer_consume_token (parser->lexer);
		}
	      else
		{
		  if (property_getter_ident != NULL_TREE)
		    {
		      cp_parser_error (parser, "the %<getter%> attribute may only be specified once");
		      cp_lexer_consume_token (parser->lexer);
		    }
		  else
		    property_getter_ident = cp_parser_objc_selector (parser);
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


/* OpenMP 2.5 / 3.0 / 3.1 / 4.0 parsing routines.  */

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
  else if (cp_lexer_next_token_is_keyword (parser->lexer, RID_FOR))
    result = PRAGMA_OMP_CLAUSE_FOR;
  else if (cp_lexer_next_token_is (parser->lexer, CPP_NAME))
    {
      tree id = cp_lexer_peek_token (parser->lexer)->u.value;
      const char *p = IDENTIFIER_POINTER (id);

      switch (p[0])
	{
	case 'a':
	  if (!strcmp ("aligned", p))
	    result = PRAGMA_OMP_CLAUSE_ALIGNED;
	  break;
	case 'c':
	  if (!strcmp ("collapse", p))
	    result = PRAGMA_OMP_CLAUSE_COLLAPSE;
	  else if (!strcmp ("copyin", p))
	    result = PRAGMA_OMP_CLAUSE_COPYIN;
	  else if (!strcmp ("copyprivate", p))
	    result = PRAGMA_OMP_CLAUSE_COPYPRIVATE;
	  break;
	case 'd':
	  if (!strcmp ("depend", p))
	    result = PRAGMA_OMP_CLAUSE_DEPEND;
	  else if (!strcmp ("device", p))
	    result = PRAGMA_OMP_CLAUSE_DEVICE;
	  else if (!strcmp ("dist_schedule", p))
	    result = PRAGMA_OMP_CLAUSE_DIST_SCHEDULE;
	  break;
	case 'f':
	  if (!strcmp ("final", p))
	    result = PRAGMA_OMP_CLAUSE_FINAL;
	  else if (!strcmp ("firstprivate", p))
	    result = PRAGMA_OMP_CLAUSE_FIRSTPRIVATE;
	  else if (!strcmp ("from", p))
	    result = PRAGMA_OMP_CLAUSE_FROM;
	  break;
	case 'i':
	  if (!strcmp ("inbranch", p))
	    result = PRAGMA_OMP_CLAUSE_INBRANCH;
	  break;
	case 'l':
	  if (!strcmp ("lastprivate", p))
	    result = PRAGMA_OMP_CLAUSE_LASTPRIVATE;
	  else if (!strcmp ("linear", p))
	    result = PRAGMA_OMP_CLAUSE_LINEAR;
	  break;
	case 'm':
	  if (!strcmp ("map", p))
	    result = PRAGMA_OMP_CLAUSE_MAP;
	  else if (!strcmp ("mergeable", p))
	    result = PRAGMA_OMP_CLAUSE_MERGEABLE;
	  else if (flag_cilkplus && !strcmp ("mask", p))
	    result = PRAGMA_CILK_CLAUSE_MASK;
	  break;
	case 'n':
	  if (!strcmp ("notinbranch", p))
	    result = PRAGMA_OMP_CLAUSE_NOTINBRANCH;
	  else if (!strcmp ("nowait", p))
	    result = PRAGMA_OMP_CLAUSE_NOWAIT;
	  else if (flag_cilkplus && !strcmp ("nomask", p))
	    result = PRAGMA_CILK_CLAUSE_NOMASK;
	  else if (!strcmp ("num_teams", p))
	    result = PRAGMA_OMP_CLAUSE_NUM_TEAMS;
	  else if (!strcmp ("num_threads", p))
	    result = PRAGMA_OMP_CLAUSE_NUM_THREADS;
	  break;
	case 'o':
	  if (!strcmp ("ordered", p))
	    result = PRAGMA_OMP_CLAUSE_ORDERED;
	  break;
	case 'p':
	  if (!strcmp ("parallel", p))
	    result = PRAGMA_OMP_CLAUSE_PARALLEL;
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
	  else if (!strcmp ("shared", p))
	    result = PRAGMA_OMP_CLAUSE_SHARED;
	  else if (!strcmp ("simdlen", p))
	    result = PRAGMA_OMP_CLAUSE_SIMDLEN;
	  break;
	case 't':
	  if (!strcmp ("taskgroup", p))
	    result = PRAGMA_OMP_CLAUSE_TASKGROUP;
	  else if (!strcmp ("thread_limit", p))
	    result = PRAGMA_OMP_CLAUSE_THREAD_LIMIT;
	  else if (!strcmp ("to", p))
	    result = PRAGMA_OMP_CLAUSE_TO;
	  break;
	case 'u':
	  if (!strcmp ("uniform", p))
	    result = PRAGMA_OMP_CLAUSE_UNIFORM;
	  else if (!strcmp ("untied", p))
	    result = PRAGMA_OMP_CLAUSE_UNTIED;
	  break;
	case 'v':
	  if (flag_cilkplus && !strcmp ("vectorlength", p))
	    result = PRAGMA_CILK_CLAUSE_VECTORLENGTH;
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

   In addition, we match a closing parenthesis (or, if COLON is non-NULL,
   colon).  An opening parenthesis will have been consumed by the caller.

   If KIND is nonzero, create the appropriate node and install the decl
   in OMP_CLAUSE_DECL and add the node to the head of the list.

   If KIND is zero, create a TREE_LIST with the decl in TREE_PURPOSE;
   return the list created.

   COLON can be NULL if only closing parenthesis should end the list,
   or pointer to bool which will receive false if the list is terminated
   by closing parenthesis or true if the list is terminated by colon.  */

static tree
cp_parser_omp_var_list_no_open (cp_parser *parser, enum omp_clause_code kind,
				tree list, bool *colon)
{
  cp_token *token;
  bool saved_colon_corrects_to_scope_p = parser->colon_corrects_to_scope_p;
  if (colon)
    {
      parser->colon_corrects_to_scope_p = false;
      *colon = false;
    }
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
	cp_parser_name_lookup_error (parser, name, decl, NLE_NULL,
				     token->location);
      else if (kind != 0)
	{
	  switch (kind)
	    {
	    case OMP_CLAUSE_MAP:
	    case OMP_CLAUSE_FROM:
	    case OMP_CLAUSE_TO:
	    case OMP_CLAUSE_DEPEND:
	      while (cp_lexer_next_token_is (parser->lexer, CPP_OPEN_SQUARE))
		{
		  tree low_bound = NULL_TREE, length = NULL_TREE;

		  parser->colon_corrects_to_scope_p = false;
		  cp_lexer_consume_token (parser->lexer);
		  if (!cp_lexer_next_token_is (parser->lexer, CPP_COLON))
		    low_bound = cp_parser_expression (parser, /*cast_p=*/false,
						      NULL);
		  if (!colon)
		    parser->colon_corrects_to_scope_p
		      = saved_colon_corrects_to_scope_p;
		  if (cp_lexer_next_token_is (parser->lexer, CPP_CLOSE_SQUARE))
		    length = integer_one_node;
		  else
		    {
		      /* Look for `:'.  */
		      if (!cp_parser_require (parser, CPP_COLON, RT_COLON))
			goto skip_comma;
		      if (!cp_lexer_next_token_is (parser->lexer,
						   CPP_CLOSE_SQUARE))
			length = cp_parser_expression (parser,
						       /*cast_p=*/false,
						       NULL);
		    }
		  /* Look for the closing `]'.  */
		  if (!cp_parser_require (parser, CPP_CLOSE_SQUARE,
					  RT_CLOSE_SQUARE))
		    goto skip_comma;
		  decl = tree_cons (low_bound, length, decl);
		}
	      break;
	    default:
	      break;
	    }

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

  if (colon)
    parser->colon_corrects_to_scope_p = saved_colon_corrects_to_scope_p;

  if (colon != NULL && cp_lexer_next_token_is (parser->lexer, CPP_COLON))
    {
      *colon = true;
      cp_parser_require (parser, CPP_COLON, RT_COLON);
      return list;
    }

  if (!cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN))
    {
      int ending;

      /* Try to resync to an unnested comma.  Copied from
	 cp_parser_parenthesized_expression_list.  */
    skip_comma:
      if (colon)
	parser->colon_corrects_to_scope_p = saved_colon_corrects_to_scope_p;
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
  if (cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN))
    return cp_parser_omp_var_list_no_open (parser, kind, list, NULL);
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
  if (!cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN))
    return list;

  num = cp_parser_constant_expression (parser, false, NULL);

  if (!cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN))
    cp_parser_skip_to_closing_parenthesis (parser, /*recovering=*/true,
					   /*or_comma=*/false,
					   /*consume_paren=*/true);

  if (num == error_mark_node)
    return list;
  num = fold_non_dependent_expr (num);
  if (!INTEGRAL_TYPE_P (TREE_TYPE (num))
      || !tree_fits_shwi_p (num)
      || (n = tree_to_shwi (num)) <= 0
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

  if (!cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN))
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

  if (!cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN))
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

/* OpenMP 3.1:
   final ( expression ) */

static tree
cp_parser_omp_clause_final (cp_parser *parser, tree list, location_t location)
{
  tree t, c;

  if (!cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN))
    return list;

  t = cp_parser_condition (parser);

  if (t == error_mark_node
      || !cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN))
    cp_parser_skip_to_closing_parenthesis (parser, /*recovering=*/true,
					   /*or_comma=*/false,
					   /*consume_paren=*/true);

  check_no_duplicate_clause (list, OMP_CLAUSE_FINAL, "final", location);

  c = build_omp_clause (location, OMP_CLAUSE_FINAL);
  OMP_CLAUSE_FINAL_EXPR (c) = t;
  OMP_CLAUSE_CHAIN (c) = list;

  return c;
}

/* OpenMP 2.5:
   if ( expression ) */

static tree
cp_parser_omp_clause_if (cp_parser *parser, tree list, location_t location)
{
  tree t, c;

  if (!cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN))
    return list;

  t = cp_parser_condition (parser);

  if (t == error_mark_node
      || !cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN))
    cp_parser_skip_to_closing_parenthesis (parser, /*recovering=*/true,
					   /*or_comma=*/false,
					   /*consume_paren=*/true);

  check_no_duplicate_clause (list, OMP_CLAUSE_IF, "if", location);

  c = build_omp_clause (location, OMP_CLAUSE_IF);
  OMP_CLAUSE_IF_EXPR (c) = t;
  OMP_CLAUSE_CHAIN (c) = list;

  return c;
}

/* OpenMP 3.1:
   mergeable */

static tree
cp_parser_omp_clause_mergeable (cp_parser * /*parser*/,
				tree list, location_t location)
{
  tree c;

  check_no_duplicate_clause (list, OMP_CLAUSE_MERGEABLE, "mergeable",
			     location);

  c = build_omp_clause (location, OMP_CLAUSE_MERGEABLE);
  OMP_CLAUSE_CHAIN (c) = list;
  return c;
}

/* OpenMP 2.5:
   nowait */

static tree
cp_parser_omp_clause_nowait (cp_parser * /*parser*/,
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

  if (!cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN))
    return list;

  t = cp_parser_expression (parser, false, NULL);

  if (t == error_mark_node
      || !cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN))
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
cp_parser_omp_clause_ordered (cp_parser * /*parser*/,
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
     One of: + * - & ^ | && ||

   OpenMP 3.1:

   reduction-operator:
     One of: + * - & ^ | && || min max

   OpenMP 4.0:

   reduction-operator:
     One of: + * - & ^ | && ||
     id-expression  */

static tree
cp_parser_omp_clause_reduction (cp_parser *parser, tree list)
{
  enum tree_code code = ERROR_MARK;
  tree nlist, c, id = NULL_TREE;

  if (!cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN))
    return list;

  switch (cp_lexer_peek_token (parser->lexer)->type)
    {
    case CPP_PLUS: code = PLUS_EXPR; break;
    case CPP_MULT: code = MULT_EXPR; break;
    case CPP_MINUS: code = MINUS_EXPR; break;
    case CPP_AND: code = BIT_AND_EXPR; break;
    case CPP_XOR: code = BIT_XOR_EXPR; break;
    case CPP_OR: code = BIT_IOR_EXPR; break;
    case CPP_AND_AND: code = TRUTH_ANDIF_EXPR; break;
    case CPP_OR_OR: code = TRUTH_ORIF_EXPR; break;
    default: break;
    }

  if (code != ERROR_MARK)
    cp_lexer_consume_token (parser->lexer);
  else
    {
      bool saved_colon_corrects_to_scope_p;
      saved_colon_corrects_to_scope_p = parser->colon_corrects_to_scope_p;
      parser->colon_corrects_to_scope_p = false;
      id = cp_parser_id_expression (parser, /*template_p=*/false,
				    /*check_dependency_p=*/true,
				    /*template_p=*/NULL,
				    /*declarator_p=*/false,
				    /*optional_p=*/false);
      parser->colon_corrects_to_scope_p = saved_colon_corrects_to_scope_p;
      if (identifier_p (id))
	{
	  const char *p = IDENTIFIER_POINTER (id);

	  if (strcmp (p, "min") == 0)
	    code = MIN_EXPR;
	  else if (strcmp (p, "max") == 0)
	    code = MAX_EXPR;
	  else if (id == ansi_opname (PLUS_EXPR))
	    code = PLUS_EXPR;
	  else if (id == ansi_opname (MULT_EXPR))
	    code = MULT_EXPR;
	  else if (id == ansi_opname (MINUS_EXPR))
	    code = MINUS_EXPR;
	  else if (id == ansi_opname (BIT_AND_EXPR))
	    code = BIT_AND_EXPR;
	  else if (id == ansi_opname (BIT_IOR_EXPR))
	    code = BIT_IOR_EXPR;
	  else if (id == ansi_opname (BIT_XOR_EXPR))
	    code = BIT_XOR_EXPR;
	  else if (id == ansi_opname (TRUTH_ANDIF_EXPR))
	    code = TRUTH_ANDIF_EXPR;
	  else if (id == ansi_opname (TRUTH_ORIF_EXPR))
	    code = TRUTH_ORIF_EXPR;
	  id = omp_reduction_id (code, id, NULL_TREE);
	  tree scope = parser->scope;
	  if (scope)
	    id = build_qualified_name (NULL_TREE, scope, id, false);
	  parser->scope = NULL_TREE;
	  parser->qualifying_scope = NULL_TREE;
	  parser->object_scope = NULL_TREE;
	}
      else
	{
	  error ("invalid reduction-identifier");
	 resync_fail:
	  cp_parser_skip_to_closing_parenthesis (parser, /*recovering=*/true,
						 /*or_comma=*/false,
						 /*consume_paren=*/true);
	  return list;
	}
    }

  if (!cp_parser_require (parser, CPP_COLON, RT_COLON))
    goto resync_fail;

  nlist = cp_parser_omp_var_list_no_open (parser, OMP_CLAUSE_REDUCTION, list,
					  NULL);
  for (c = nlist; c != list; c = OMP_CLAUSE_CHAIN (c))
    {
      OMP_CLAUSE_REDUCTION_CODE (c) = code;
      OMP_CLAUSE_REDUCTION_PLACEHOLDER (c) = id;
    }

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

  if (!cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN))
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

      if (!cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN))
	goto resync_fail;
    }
  else if (!cp_parser_require (parser, CPP_CLOSE_PAREN, RT_COMMA_CLOSE_PAREN))
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
cp_parser_omp_clause_untied (cp_parser * /*parser*/,
			     tree list, location_t location)
{
  tree c;

  check_no_duplicate_clause (list, OMP_CLAUSE_UNTIED, "untied", location);

  c = build_omp_clause (location, OMP_CLAUSE_UNTIED);
  OMP_CLAUSE_CHAIN (c) = list;
  return c;
}

/* OpenMP 4.0:
   inbranch
   notinbranch */

static tree
cp_parser_omp_clause_branch (cp_parser * /*parser*/, enum omp_clause_code code,
			     tree list, location_t location)
{
  check_no_duplicate_clause (list, code, omp_clause_code_name[code], location);
  tree c = build_omp_clause (location, code);
  OMP_CLAUSE_CHAIN (c) = list;
  return c;
}

/* OpenMP 4.0:
   parallel
   for
   sections
   taskgroup */

static tree
cp_parser_omp_clause_cancelkind (cp_parser * /*parser*/,
				 enum omp_clause_code code,
				 tree list, location_t location)
{
  tree c = build_omp_clause (location, code);
  OMP_CLAUSE_CHAIN (c) = list;
  return c;
}

/* OpenMP 4.0:
   num_teams ( expression ) */

static tree
cp_parser_omp_clause_num_teams (cp_parser *parser, tree list,
				location_t location)
{
  tree t, c;

  if (!cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN))
    return list;

  t = cp_parser_expression (parser, false, NULL);

  if (t == error_mark_node
      || !cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN))
    cp_parser_skip_to_closing_parenthesis (parser, /*recovering=*/true,
					   /*or_comma=*/false,
					   /*consume_paren=*/true);

  check_no_duplicate_clause (list, OMP_CLAUSE_NUM_TEAMS,
			     "num_teams", location);

  c = build_omp_clause (location, OMP_CLAUSE_NUM_TEAMS);
  OMP_CLAUSE_NUM_TEAMS_EXPR (c) = t;
  OMP_CLAUSE_CHAIN (c) = list;

  return c;
}

/* OpenMP 4.0:
   thread_limit ( expression ) */

static tree
cp_parser_omp_clause_thread_limit (cp_parser *parser, tree list,
				   location_t location)
{
  tree t, c;

  if (!cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN))
    return list;

  t = cp_parser_expression (parser, false, NULL);

  if (t == error_mark_node
      || !cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN))
    cp_parser_skip_to_closing_parenthesis (parser, /*recovering=*/true,
					   /*or_comma=*/false,
					   /*consume_paren=*/true);

  check_no_duplicate_clause (list, OMP_CLAUSE_THREAD_LIMIT,
			     "thread_limit", location);

  c = build_omp_clause (location, OMP_CLAUSE_THREAD_LIMIT);
  OMP_CLAUSE_THREAD_LIMIT_EXPR (c) = t;
  OMP_CLAUSE_CHAIN (c) = list;

  return c;
}

/* OpenMP 4.0:
   aligned ( variable-list )
   aligned ( variable-list : constant-expression )  */

static tree
cp_parser_omp_clause_aligned (cp_parser *parser, tree list)
{
  tree nlist, c, alignment = NULL_TREE;
  bool colon;

  if (!cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN))
    return list;

  nlist = cp_parser_omp_var_list_no_open (parser, OMP_CLAUSE_ALIGNED, list,
					  &colon);

  if (colon)
    {
      alignment = cp_parser_constant_expression (parser, false, NULL);

      if (!cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN))
	cp_parser_skip_to_closing_parenthesis (parser, /*recovering=*/true,
					       /*or_comma=*/false,
					       /*consume_paren=*/true);

      if (alignment == error_mark_node)
	alignment = NULL_TREE;
    }

  for (c = nlist; c != list; c = OMP_CLAUSE_CHAIN (c))
    OMP_CLAUSE_ALIGNED_ALIGNMENT (c) = alignment;

  return nlist;
}

/* OpenMP 4.0:
   linear ( variable-list )
   linear ( variable-list : expression )  */

static tree
cp_parser_omp_clause_linear (cp_parser *parser, tree list, 
			     bool is_cilk_simd_fn)
{
  tree nlist, c, step = integer_one_node;
  bool colon;

  if (!cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN))
    return list;

  nlist = cp_parser_omp_var_list_no_open (parser, OMP_CLAUSE_LINEAR, list,
					  &colon);

  if (colon)
    {
      step = cp_parser_expression (parser, false, NULL);

      if (is_cilk_simd_fn && TREE_CODE (step) == PARM_DECL)
	{
	  sorry ("using parameters for %<linear%> step is not supported yet");
	  step = integer_one_node;
	}
      if (!cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN))
	cp_parser_skip_to_closing_parenthesis (parser, /*recovering=*/true,
					       /*or_comma=*/false,
					       /*consume_paren=*/true);

      if (step == error_mark_node)
	return list;
    }

  for (c = nlist; c != list; c = OMP_CLAUSE_CHAIN (c))
    OMP_CLAUSE_LINEAR_STEP (c) = step;

  return nlist;
}

/* OpenMP 4.0:
   safelen ( constant-expression )  */

static tree
cp_parser_omp_clause_safelen (cp_parser *parser, tree list,
			      location_t location)
{
  tree t, c;

  if (!cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN))
    return list;

  t = cp_parser_constant_expression (parser, false, NULL);

  if (t == error_mark_node
      || !cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN))
    cp_parser_skip_to_closing_parenthesis (parser, /*recovering=*/true,
					   /*or_comma=*/false,
					   /*consume_paren=*/true);

  check_no_duplicate_clause (list, OMP_CLAUSE_SAFELEN, "safelen", location);

  c = build_omp_clause (location, OMP_CLAUSE_SAFELEN);
  OMP_CLAUSE_SAFELEN_EXPR (c) = t;
  OMP_CLAUSE_CHAIN (c) = list;

  return c;
}

/* OpenMP 4.0:
   simdlen ( constant-expression )  */

static tree
cp_parser_omp_clause_simdlen (cp_parser *parser, tree list,
			      location_t location)
{
  tree t, c;

  if (!cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN))
    return list;

  t = cp_parser_constant_expression (parser, false, NULL);

  if (t == error_mark_node
      || !cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN))
    cp_parser_skip_to_closing_parenthesis (parser, /*recovering=*/true,
					   /*or_comma=*/false,
					   /*consume_paren=*/true);

  check_no_duplicate_clause (list, OMP_CLAUSE_SIMDLEN, "simdlen", location);

  c = build_omp_clause (location, OMP_CLAUSE_SIMDLEN);
  OMP_CLAUSE_SIMDLEN_EXPR (c) = t;
  OMP_CLAUSE_CHAIN (c) = list;

  return c;
}

/* OpenMP 4.0:
   depend ( depend-kind : variable-list )

   depend-kind:
     in | out | inout  */

static tree
cp_parser_omp_clause_depend (cp_parser *parser, tree list)
{
  tree nlist, c;
  enum omp_clause_depend_kind kind = OMP_CLAUSE_DEPEND_INOUT;

  if (!cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN))
    return list;

  if (cp_lexer_next_token_is (parser->lexer, CPP_NAME))
    {
      tree id = cp_lexer_peek_token (parser->lexer)->u.value;
      const char *p = IDENTIFIER_POINTER (id);

      if (strcmp ("in", p) == 0)
	kind = OMP_CLAUSE_DEPEND_IN;
      else if (strcmp ("inout", p) == 0)
	kind = OMP_CLAUSE_DEPEND_INOUT;
      else if (strcmp ("out", p) == 0)
	kind = OMP_CLAUSE_DEPEND_OUT;
      else
	goto invalid_kind;
    }
  else
    goto invalid_kind;

  cp_lexer_consume_token (parser->lexer);
  if (!cp_parser_require (parser, CPP_COLON, RT_COLON))
    goto resync_fail;

  nlist = cp_parser_omp_var_list_no_open (parser, OMP_CLAUSE_DEPEND, list,
					  NULL);

  for (c = nlist; c != list; c = OMP_CLAUSE_CHAIN (c))
    OMP_CLAUSE_DEPEND_KIND (c) = kind;

  return nlist;

 invalid_kind:
  cp_parser_error (parser, "invalid depend kind");
 resync_fail:
  cp_parser_skip_to_closing_parenthesis (parser, /*recovering=*/true,
					 /*or_comma=*/false,
					 /*consume_paren=*/true);
  return list;
}

/* OpenMP 4.0:
   map ( map-kind : variable-list )
   map ( variable-list )

   map-kind:
     alloc | to | from | tofrom  */

static tree
cp_parser_omp_clause_map (cp_parser *parser, tree list)
{
  tree nlist, c;
  enum omp_clause_map_kind kind = OMP_CLAUSE_MAP_TOFROM;

  if (!cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN))
    return list;

  if (cp_lexer_next_token_is (parser->lexer, CPP_NAME)
      && cp_lexer_peek_nth_token (parser->lexer, 2)->type == CPP_COLON)
    {
      tree id = cp_lexer_peek_token (parser->lexer)->u.value;
      const char *p = IDENTIFIER_POINTER (id);

      if (strcmp ("alloc", p) == 0)
	kind = OMP_CLAUSE_MAP_ALLOC;
      else if (strcmp ("to", p) == 0)
	kind = OMP_CLAUSE_MAP_TO;
      else if (strcmp ("from", p) == 0)
	kind = OMP_CLAUSE_MAP_FROM;
      else if (strcmp ("tofrom", p) == 0)
	kind = OMP_CLAUSE_MAP_TOFROM;
      else
	{
	  cp_parser_error (parser, "invalid map kind");
	  cp_parser_skip_to_closing_parenthesis (parser, /*recovering=*/true,
						 /*or_comma=*/false,
						 /*consume_paren=*/true);
	  return list;
	}
      cp_lexer_consume_token (parser->lexer);
      cp_lexer_consume_token (parser->lexer);
    }

  nlist = cp_parser_omp_var_list_no_open (parser, OMP_CLAUSE_MAP, list,
					  NULL);

  for (c = nlist; c != list; c = OMP_CLAUSE_CHAIN (c))
    OMP_CLAUSE_MAP_KIND (c) = kind;

  return nlist;
}

/* OpenMP 4.0:
   device ( expression ) */

static tree
cp_parser_omp_clause_device (cp_parser *parser, tree list,
			     location_t location)
{
  tree t, c;

  if (!cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN))
    return list;

  t = cp_parser_expression (parser, false, NULL);

  if (t == error_mark_node
      || !cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN))
    cp_parser_skip_to_closing_parenthesis (parser, /*recovering=*/true,
					   /*or_comma=*/false,
					   /*consume_paren=*/true);

  check_no_duplicate_clause (list, OMP_CLAUSE_DEVICE,
			     "device", location);

  c = build_omp_clause (location, OMP_CLAUSE_DEVICE);
  OMP_CLAUSE_DEVICE_ID (c) = t;
  OMP_CLAUSE_CHAIN (c) = list;

  return c;
}

/* OpenMP 4.0:
   dist_schedule ( static )
   dist_schedule ( static , expression )  */

static tree
cp_parser_omp_clause_dist_schedule (cp_parser *parser, tree list,
				    location_t location)
{
  tree c, t;

  if (!cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN))
    return list;

  c = build_omp_clause (location, OMP_CLAUSE_DIST_SCHEDULE);

  if (!cp_lexer_next_token_is_keyword (parser->lexer, RID_STATIC))
    goto invalid_kind;
  cp_lexer_consume_token (parser->lexer);

  if (cp_lexer_next_token_is (parser->lexer, CPP_COMMA))
    {
      cp_lexer_consume_token (parser->lexer);

      t = cp_parser_assignment_expression (parser, false, NULL);

      if (t == error_mark_node)
	goto resync_fail;
      OMP_CLAUSE_DIST_SCHEDULE_CHUNK_EXPR (c) = t;

      if (!cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN))
	goto resync_fail;
    }
  else if (!cp_parser_require (parser, CPP_CLOSE_PAREN, RT_COMMA_CLOSE_PAREN))
    goto resync_fail;

  check_no_duplicate_clause (list, OMP_CLAUSE_DIST_SCHEDULE, "dist_schedule",
			     location);
  OMP_CLAUSE_CHAIN (c) = list;
  return c;

 invalid_kind:
  cp_parser_error (parser, "invalid dist_schedule kind");
 resync_fail:
  cp_parser_skip_to_closing_parenthesis (parser, /*recovering=*/true,
					 /*or_comma=*/false,
					 /*consume_paren=*/true);
  return list;
}

/* OpenMP 4.0:
   proc_bind ( proc-bind-kind )

   proc-bind-kind:
     master | close | spread  */

static tree
cp_parser_omp_clause_proc_bind (cp_parser *parser, tree list,
				location_t location)
{
  tree c;
  enum omp_clause_proc_bind_kind kind;

  if (!cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN))
    return list;

  if (cp_lexer_next_token_is (parser->lexer, CPP_NAME))
    {
      tree id = cp_lexer_peek_token (parser->lexer)->u.value;
      const char *p = IDENTIFIER_POINTER (id);

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

  cp_lexer_consume_token (parser->lexer);
  if (!cp_parser_require (parser, CPP_CLOSE_PAREN, RT_COMMA_CLOSE_PAREN))
    goto resync_fail;

  c = build_omp_clause (location, OMP_CLAUSE_PROC_BIND);
  check_no_duplicate_clause (list, OMP_CLAUSE_PROC_BIND, "proc_bind",
			     location);
  OMP_CLAUSE_PROC_BIND_KIND (c) = kind;
  OMP_CLAUSE_CHAIN (c) = list;
  return c;

 invalid_kind:
  cp_parser_error (parser, "invalid depend kind");
 resync_fail:
  cp_parser_skip_to_closing_parenthesis (parser, /*recovering=*/true,
					 /*or_comma=*/false,
					 /*consume_paren=*/true);
  return list;
}

/* Parse all OpenMP clauses.  The set clauses allowed by the directive
   is a bitmask in MASK.  Return the list of clauses found; the result
   of clause default goes in *pdefault.  */

static tree
cp_parser_omp_all_clauses (cp_parser *parser, omp_clause_mask mask,
			   const char *where, cp_token *pragma_tok,
			   bool finish_p = true)
{
  tree clauses = NULL;
  bool first = true;
  cp_token *token = NULL;
  bool cilk_simd_fn = false;

  while (cp_lexer_next_token_is_not (parser->lexer, CPP_PRAGMA_EOL))
    {
      pragma_omp_clause c_kind;
      const char *c_name;
      tree prev = clauses;

      if (!first && cp_lexer_next_token_is (parser->lexer, CPP_COMMA))
	cp_lexer_consume_token (parser->lexer);

      token = cp_lexer_peek_token (parser->lexer);
      c_kind = cp_parser_omp_clause_name (parser);

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
	case PRAGMA_OMP_CLAUSE_FINAL:
	  clauses = cp_parser_omp_clause_final (parser, clauses, token->location);
	  c_name = "final";
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
	case PRAGMA_OMP_CLAUSE_MERGEABLE:
	  clauses = cp_parser_omp_clause_mergeable (parser, clauses,
						    token->location);
	  c_name = "mergeable";
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
	  c_name = "untied";
	  break;
	case PRAGMA_OMP_CLAUSE_INBRANCH:
	case PRAGMA_CILK_CLAUSE_MASK:
	  clauses = cp_parser_omp_clause_branch (parser, OMP_CLAUSE_INBRANCH,
						 clauses, token->location);
	  c_name = "inbranch";
	  break;
	case PRAGMA_OMP_CLAUSE_NOTINBRANCH:
	case PRAGMA_CILK_CLAUSE_NOMASK:
	  clauses = cp_parser_omp_clause_branch (parser,
						 OMP_CLAUSE_NOTINBRANCH,
						 clauses, token->location);
	  c_name = "notinbranch";
	  break;
	case PRAGMA_OMP_CLAUSE_PARALLEL:
	  clauses = cp_parser_omp_clause_cancelkind (parser, OMP_CLAUSE_PARALLEL,
						     clauses, token->location);
	  c_name = "parallel";
	  if (!first)
	    {
	     clause_not_first:
	      error_at (token->location, "%qs must be the first clause of %qs",
			c_name, where);
	      clauses = prev;
	    }
	  break;
	case PRAGMA_OMP_CLAUSE_FOR:
	  clauses = cp_parser_omp_clause_cancelkind (parser, OMP_CLAUSE_FOR,
						     clauses, token->location);
	  c_name = "for";
	  if (!first)
	    goto clause_not_first;
	  break;
	case PRAGMA_OMP_CLAUSE_SECTIONS:
	  clauses = cp_parser_omp_clause_cancelkind (parser, OMP_CLAUSE_SECTIONS,
						     clauses, token->location);
	  c_name = "sections";
	  if (!first)
	    goto clause_not_first;
	  break;
	case PRAGMA_OMP_CLAUSE_TASKGROUP:
	  clauses = cp_parser_omp_clause_cancelkind (parser, OMP_CLAUSE_TASKGROUP,
						     clauses, token->location);
	  c_name = "taskgroup";
	  if (!first)
	    goto clause_not_first;
	  break;
	case PRAGMA_OMP_CLAUSE_TO:
	  clauses = cp_parser_omp_var_list (parser, OMP_CLAUSE_TO,
					    clauses);
	  c_name = "to";
	  break;
	case PRAGMA_OMP_CLAUSE_FROM:
	  clauses = cp_parser_omp_var_list (parser, OMP_CLAUSE_FROM,
					    clauses);
	  c_name = "from";
	  break;
	case PRAGMA_OMP_CLAUSE_UNIFORM:
	  clauses = cp_parser_omp_var_list (parser, OMP_CLAUSE_UNIFORM,
					    clauses);
	  c_name = "uniform";
	  break;
	case PRAGMA_OMP_CLAUSE_NUM_TEAMS:
	  clauses = cp_parser_omp_clause_num_teams (parser, clauses,
						    token->location);
	  c_name = "num_teams";
	  break;
	case PRAGMA_OMP_CLAUSE_THREAD_LIMIT:
	  clauses = cp_parser_omp_clause_thread_limit (parser, clauses,
						       token->location);
	  c_name = "thread_limit";
	  break;
	case PRAGMA_OMP_CLAUSE_ALIGNED:
	  clauses = cp_parser_omp_clause_aligned (parser, clauses);
	  c_name = "aligned";
	  break;
	case PRAGMA_OMP_CLAUSE_LINEAR:
	  if (((mask >> PRAGMA_CILK_CLAUSE_VECTORLENGTH) & 1) != 0)
	    cilk_simd_fn = true;
	  clauses = cp_parser_omp_clause_linear (parser, clauses, cilk_simd_fn);
	  c_name = "linear";
	  break;
	case PRAGMA_OMP_CLAUSE_DEPEND:
	  clauses = cp_parser_omp_clause_depend (parser, clauses);
	  c_name = "depend";
	  break;
	case PRAGMA_OMP_CLAUSE_MAP:
	  clauses = cp_parser_omp_clause_map (parser, clauses);
	  c_name = "map";
	  break;
	case PRAGMA_OMP_CLAUSE_DEVICE:
	  clauses = cp_parser_omp_clause_device (parser, clauses,
						 token->location);
	  c_name = "device";
	  break;
	case PRAGMA_OMP_CLAUSE_DIST_SCHEDULE:
	  clauses = cp_parser_omp_clause_dist_schedule (parser, clauses,
							token->location);
	  c_name = "dist_schedule";
	  break;
	case PRAGMA_OMP_CLAUSE_PROC_BIND:
	  clauses = cp_parser_omp_clause_proc_bind (parser, clauses,
						    token->location);
	  c_name = "proc_bind";
	  break;
	case PRAGMA_OMP_CLAUSE_SAFELEN:
	  clauses = cp_parser_omp_clause_safelen (parser, clauses,
						  token->location);
	  c_name = "safelen";
	  break;
	case PRAGMA_OMP_CLAUSE_SIMDLEN:
	  clauses = cp_parser_omp_clause_simdlen (parser, clauses,
						  token->location);
	  c_name = "simdlen";
	  break;
	case PRAGMA_CILK_CLAUSE_VECTORLENGTH:
	  clauses = cp_parser_cilk_simd_vectorlength (parser, clauses, true);
	  c_name = "simdlen";
	  break;
	default:
	  cp_parser_error (parser, "expected %<#pragma omp%> clause");
	  goto saw_error;
	}

      first = false;

      if (((mask >> c_kind) & 1) == 0)
	{
	  /* Remove the invalid clause(s) from the list to avoid
	     confusing the rest of the compiler.  */
	  clauses = prev;
	  error_at (token->location, "%qs is not valid for %qs", c_name, where);
	}
    }
 saw_error:
  /* In Cilk Plus SIMD enabled functions there is no pragma_token, so
     no reason to skip to the end.  */
  if (!(flag_cilkplus && pragma_tok == NULL))
    cp_parser_skip_to_pragma_eol (parser, pragma_tok);
  if (finish_p)
    return finish_omp_clauses (clauses);
  return clauses;
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

  where x and v are lvalue expressions with scalar type.  */

static void
cp_parser_omp_atomic (cp_parser *parser, cp_token *pragma_tok)
{
  tree lhs = NULL_TREE, rhs = NULL_TREE, v = NULL_TREE, lhs1 = NULL_TREE;
  tree rhs1 = NULL_TREE, orig_lhs;
  enum tree_code code = OMP_ATOMIC, opcode = NOP_EXPR;
  bool structured_block = false;
  bool seq_cst = false;

  if (cp_lexer_next_token_is (parser->lexer, CPP_NAME))
    {
      tree id = cp_lexer_peek_token (parser->lexer)->u.value;
      const char *p = IDENTIFIER_POINTER (id);

      if (!strcmp (p, "read"))
	code = OMP_ATOMIC_READ;
      else if (!strcmp (p, "write"))
	code = NOP_EXPR;
      else if (!strcmp (p, "update"))
	code = OMP_ATOMIC;
      else if (!strcmp (p, "capture"))
	code = OMP_ATOMIC_CAPTURE_NEW;
      else
	p = NULL;
      if (p)
	cp_lexer_consume_token (parser->lexer);
    }

  if (cp_lexer_next_token_is (parser->lexer, CPP_NAME))
    {
      tree id = cp_lexer_peek_token (parser->lexer)->u.value;
      const char *p = IDENTIFIER_POINTER (id);

      if (!strcmp (p, "seq_cst"))
	{
	  seq_cst = true;
	  cp_lexer_consume_token (parser->lexer);
	}
    }
  cp_parser_require_pragma_eol (parser, pragma_tok);

  switch (code)
    {
    case OMP_ATOMIC_READ:
    case NOP_EXPR: /* atomic write */
      v = cp_parser_unary_expression (parser, /*address_p=*/false,
				      /*cast_p=*/false, NULL);
      if (v == error_mark_node)
	goto saw_error;
      if (!cp_parser_require (parser, CPP_EQ, RT_EQ))
	goto saw_error;
      if (code == NOP_EXPR)
	lhs = cp_parser_expression (parser, /*cast_p=*/false, NULL);
      else
	lhs = cp_parser_unary_expression (parser, /*address_p=*/false,
					  /*cast_p=*/false, NULL);
      if (lhs == error_mark_node)
	goto saw_error;
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
      if (cp_lexer_next_token_is (parser->lexer, CPP_OPEN_BRACE))
	{
	  cp_lexer_consume_token (parser->lexer);
	  structured_block = true;
	}
      else
	{
	  v = cp_parser_unary_expression (parser, /*address_p=*/false,
					  /*cast_p=*/false, NULL);
	  if (v == error_mark_node)
	    goto saw_error;
	  if (!cp_parser_require (parser, CPP_EQ, RT_EQ))
	    goto saw_error;
	}
    default:
      break;
    }

restart:
  lhs = cp_parser_unary_expression (parser, /*address_p=*/false,
				    /*cast_p=*/false, NULL);
  orig_lhs = lhs;
  switch (TREE_CODE (lhs))
    {
    case ERROR_MARK:
      goto saw_error;

    case POSTINCREMENT_EXPR:
      if (code == OMP_ATOMIC_CAPTURE_NEW && !structured_block)
	code = OMP_ATOMIC_CAPTURE_OLD;
      /* FALLTHROUGH */
    case PREINCREMENT_EXPR:
      lhs = TREE_OPERAND (lhs, 0);
      opcode = PLUS_EXPR;
      rhs = integer_one_node;
      break;

    case POSTDECREMENT_EXPR:
      if (code == OMP_ATOMIC_CAPTURE_NEW && !structured_block)
	code = OMP_ATOMIC_CAPTURE_OLD;
      /* FALLTHROUGH */
    case PREDECREMENT_EXPR:
      lhs = TREE_OPERAND (lhs, 0);
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
      switch (cp_lexer_peek_token (parser->lexer)->type)
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
	  enum cp_parser_prec oprec;
	  cp_token *token;
	  cp_lexer_consume_token (parser->lexer);
	  cp_parser_parse_tentatively (parser);
	  rhs1 = cp_parser_simple_cast_expression (parser);
	  if (rhs1 == error_mark_node)
	    {
	      cp_parser_abort_tentative_parse (parser);
	      cp_parser_simple_cast_expression (parser);
	      goto saw_error;
	    }
	  token = cp_lexer_peek_token (parser->lexer);
	  if (token->type != CPP_SEMICOLON && !cp_tree_equal (lhs, rhs1))
	    {
	      cp_parser_abort_tentative_parse (parser);
	      cp_parser_parse_tentatively (parser);
	      rhs = cp_parser_binary_expression (parser, false, true,
						 PREC_NOT_OPERATOR, NULL);
	      if (rhs == error_mark_node)
		{
		  cp_parser_abort_tentative_parse (parser);
		  cp_parser_binary_expression (parser, false, true,
					       PREC_NOT_OPERATOR, NULL);
		  goto saw_error;
		}
	      switch (TREE_CODE (rhs))
		{
		case MULT_EXPR:
		case TRUNC_DIV_EXPR:
		case PLUS_EXPR:
		case MINUS_EXPR:
		case LSHIFT_EXPR:
		case RSHIFT_EXPR:
		case BIT_AND_EXPR:
		case BIT_IOR_EXPR:
		case BIT_XOR_EXPR:
		  if (cp_tree_equal (lhs, TREE_OPERAND (rhs, 1)))
		    {
		      if (cp_parser_parse_definitely (parser))
			{
			  opcode = TREE_CODE (rhs);
			  rhs1 = TREE_OPERAND (rhs, 0);
			  rhs = TREE_OPERAND (rhs, 1);
			  goto stmt_done;
			}
		      else
			goto saw_error;
		    }
		  break;
		default:
		  break;
		}
	      cp_parser_abort_tentative_parse (parser);
	      if (structured_block && code == OMP_ATOMIC_CAPTURE_OLD)
		{
		  rhs = cp_parser_expression (parser, /*cast_p=*/false, NULL);
		  if (rhs == error_mark_node)
		    goto saw_error;
		  opcode = NOP_EXPR;
		  rhs1 = NULL_TREE;
		  goto stmt_done;
		}
	      cp_parser_error (parser,
			       "invalid form of %<#pragma omp atomic%>");
	      goto saw_error;
	    }
	  if (!cp_parser_parse_definitely (parser))
	    goto saw_error;
	  switch (token->type)
	    {
	    case CPP_SEMICOLON:
	      if (structured_block && code == OMP_ATOMIC_CAPTURE_NEW)
		{
		  code = OMP_ATOMIC_CAPTURE_OLD;
		  v = lhs;
		  lhs = NULL_TREE;
		  lhs1 = rhs1;
		  rhs1 = NULL_TREE;
		  cp_lexer_consume_token (parser->lexer);
		  goto restart;
		}
	      else if (structured_block)
		{
		  opcode = NOP_EXPR;
		  rhs = rhs1;
		  rhs1 = NULL_TREE;
		  goto stmt_done;
		}
	      cp_parser_error (parser,
			       "invalid form of %<#pragma omp atomic%>");
	      goto saw_error;
	    case CPP_MULT:
	      opcode = MULT_EXPR;
	      break;
	    case CPP_DIV:
	      opcode = TRUNC_DIV_EXPR;
	      break;
	    case CPP_PLUS:
	      opcode = PLUS_EXPR;
	      break;
	    case CPP_MINUS:
	      opcode = MINUS_EXPR;
	      break;
	    case CPP_LSHIFT:
	      opcode = LSHIFT_EXPR;
	      break;
	    case CPP_RSHIFT:
	      opcode = RSHIFT_EXPR;
	      break;
	    case CPP_AND:
	      opcode = BIT_AND_EXPR;
	      break;
	    case CPP_OR:
	      opcode = BIT_IOR_EXPR;
	      break;
	    case CPP_XOR:
	      opcode = BIT_XOR_EXPR;
	      break;
	    default:
	      cp_parser_error (parser,
			       "invalid operator for %<#pragma omp atomic%>");
	      goto saw_error;
	    }
	  oprec = TOKEN_PRECEDENCE (token);
	  gcc_assert (oprec != PREC_NOT_OPERATOR);
	  if (commutative_tree_code (opcode))
	    oprec = (enum cp_parser_prec) (oprec - 1);
	  cp_lexer_consume_token (parser->lexer);
	  rhs = cp_parser_binary_expression (parser, false, false,
					     oprec, NULL);
	  if (rhs == error_mark_node)
	    goto saw_error;
	  goto stmt_done;
	  /* FALLTHROUGH */
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
stmt_done:
  if (structured_block && code == OMP_ATOMIC_CAPTURE_NEW)
    {
      if (!cp_parser_require (parser, CPP_SEMICOLON, RT_SEMICOLON))
	goto saw_error;
      v = cp_parser_unary_expression (parser, /*address_p=*/false,
				      /*cast_p=*/false, NULL);
      if (v == error_mark_node)
	goto saw_error;
      if (!cp_parser_require (parser, CPP_EQ, RT_EQ))
	goto saw_error;
      lhs1 = cp_parser_unary_expression (parser, /*address_p=*/false,
					 /*cast_p=*/false, NULL);
      if (lhs1 == error_mark_node)
	goto saw_error;
    }
  if (structured_block)
    {
      cp_parser_consume_semicolon_at_end_of_statement (parser);
      cp_parser_require (parser, CPP_CLOSE_BRACE, RT_CLOSE_BRACE);
    }
done:
  finish_omp_atomic (code, opcode, lhs, rhs, v, lhs1, rhs1, seq_cst);
  if (!structured_block)
    cp_parser_consume_semicolon_at_end_of_statement (parser);
  return;

 saw_error:
  cp_parser_skip_to_end_of_block_or_statement (parser);
  if (structured_block)
    {
      if (cp_lexer_next_token_is (parser->lexer, CPP_CLOSE_BRACE))
        cp_lexer_consume_token (parser->lexer);
      else if (code == OMP_ATOMIC_CAPTURE_NEW)
	{
	  cp_parser_skip_to_end_of_block_or_statement (parser);
	  if (cp_lexer_next_token_is (parser->lexer, CPP_CLOSE_BRACE))
	    cp_lexer_consume_token (parser->lexer);
	}
    }
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
	  || !cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN))
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
cp_parser_omp_for_cond (cp_parser *parser, tree decl, enum tree_code code)
{
  tree cond = cp_parser_binary_expression (parser, false, true,
					   PREC_NOT_OPERATOR, NULL);
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
    case NE_EXPR:
      if (code == CILK_SIMD)
	break;
      /* Fall through: OpenMP disallows NE_EXPR.  */
    default:
      return error_mark_node;
    }

  /* If decl is an iterator, preserve LHS and RHS of the relational
     expr until finish_omp_for.  */
  if (decl
      && (type_dependent_expression_p (decl)
	  || CLASS_TYPE_P (TREE_TYPE (decl))))
    return cond;

  return build_x_binary_op (input_location, TREE_CODE (cond),
			    TREE_OPERAND (cond, 0), ERROR_MARK,
			    TREE_OPERAND (cond, 1), ERROR_MARK,
			    /*overload=*/NULL, tf_warning_or_error);
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
      lhs = cp_parser_simple_cast_expression (parser);
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
		lhs = build_x_unary_op (input_location, NEGATE_EXPR, rhs,
					tf_warning_or_error);
	    }
	  else
	    lhs = build_x_binary_op (input_location, op, lhs, ERROR_MARK, rhs,
				     ERROR_MARK, NULL, tf_warning_or_error);
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

/* Parse the initialization statement of either an OpenMP for loop or
   a Cilk Plus for loop.

   PARSING_OPENMP is true if parsing OpenMP, or false if parsing Cilk
   Plus.

   Return true if the resulting construct should have an
   OMP_CLAUSE_PRIVATE added to it.  */

static bool
cp_parser_omp_for_loop_init (cp_parser *parser,
			     bool parsing_openmp,
			     tree &this_pre_body,
			     vec<tree, va_gc> *for_block,
			     tree &init,
			     tree &decl,
			     tree &real_decl)
{
  if (cp_lexer_next_token_is (parser->lexer, CPP_SEMICOLON))
    return false;

  bool add_private_clause = false;

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
		{
		  if (parsing_openmp)
		    error ("parenthesized initialization is not allowed in "
			   "OpenMP %<for%> loop");
		  else
		    error ("parenthesized initialization is "
			   "not allowed in for-loop");
		}
	      else
		/* Trigger an error.  */
		cp_parser_require (parser, CPP_EQ, RT_EQ);

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

	      if (auto_node)
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
		  vec_safe_push (for_block, this_pre_body);
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
	  cp_parser_require (parser, CPP_EQ, RT_EQ);
	  rhs = cp_parser_assignment_expression (parser, false, NULL);
	  finish_expr_stmt (build_x_modify_expr (EXPR_LOCATION (rhs),
						 decl, NOP_EXPR,
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
  return add_private_clause;
}

/* Parse the restricted form of the for statement allowed by OpenMP.  */

static tree
cp_parser_omp_for_loop (cp_parser *parser, enum tree_code code, tree clauses,
			tree *cclauses)
{
  tree init, cond, incr, body, decl, pre_body = NULL_TREE, ret;
  tree real_decl, initv, condv, incrv, declv;
  tree this_pre_body, cl;
  location_t loc_first;
  bool collapse_err = false;
  int i, collapse = 1, nbraces = 0;
  vec<tree, va_gc> *for_block = make_tree_vector ();

  for (cl = clauses; cl; cl = OMP_CLAUSE_CHAIN (cl))
    if (OMP_CLAUSE_CODE (cl) == OMP_CLAUSE_COLLAPSE)
      collapse = tree_to_shwi (OMP_CLAUSE_COLLAPSE_EXPR (cl));

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

      if (!cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN))
	return NULL;

      init = decl = real_decl = NULL;
      this_pre_body = push_stmt_list ();

      add_private_clause
	|= cp_parser_omp_for_loop_init (parser,
					/*parsing_openmp=*/code != CILK_SIMD,
					this_pre_body, for_block,
					init, decl, real_decl);

      cp_parser_require (parser, CPP_SEMICOLON, RT_SEMICOLON);
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
      if (cclauses != NULL
	  && cclauses[C_OMP_CLAUSE_SPLIT_PARALLEL] != NULL
	  && real_decl != NULL_TREE)
	{
	  tree *c;
	  for (c = &cclauses[C_OMP_CLAUSE_SPLIT_PARALLEL]; *c ; )
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
	cond = cp_parser_omp_for_cond (parser, decl, code);
      cp_parser_require (parser, CPP_SEMICOLON, RT_SEMICOLON);

      incr = NULL;
      if (cp_lexer_next_token_is_not (parser->lexer, CPP_CLOSE_PAREN))
	{
	  /* If decl is an iterator, preserve the operator on decl
	     until finish_omp_for.  */
	  if (real_decl
	      && ((processing_template_decl
		   && !POINTER_TYPE_P (TREE_TYPE (real_decl)))
		  || CLASS_TYPE_P (TREE_TYPE (real_decl))))
	    incr = cp_parser_omp_for_incr (parser, real_decl);
	  else
	    incr = cp_parser_expression (parser, false, NULL);
	  if (CAN_HAVE_LOCATION_P (incr) && !EXPR_HAS_LOCATION (incr))
	    SET_EXPR_LOCATION (incr, input_location);
	}

      if (!cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN))
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
  if (code == CILK_SIMD)
    parser->in_statement = IN_CILK_SIMD_FOR;
  else
    parser->in_statement = IN_OMP_FOR;

  /* Note that the grammar doesn't call for a structured block here,
     though the loop as a whole is a structured block.  */
  body = push_stmt_list ();
  cp_parser_statement (parser, NULL_TREE, false, NULL);
  body = pop_stmt_list (body);

  if (declv == NULL_TREE)
    ret = NULL_TREE;
  else
    ret = finish_omp_for (loc_first, code, declv, initv, condv, incrv, body,
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

  while (!for_block->is_empty ())
    add_stmt (pop_stmt_list (for_block->pop ()));
  release_tree_vector (for_block);

  return ret;
}

/* Helper function for OpenMP parsing, split clauses and call
   finish_omp_clauses on each of the set of clauses afterwards.  */

static void
cp_omp_split_clauses (location_t loc, enum tree_code code,
		      omp_clause_mask mask, tree clauses, tree *cclauses)
{
  int i;
  c_omp_split_clauses (loc, code, mask, clauses, cclauses);
  for (i = 0; i < C_OMP_CLAUSE_SPLIT_COUNT; i++)
    if (cclauses[i])
      cclauses[i] = finish_omp_clauses (cclauses[i]);
}

/* OpenMP 4.0:
   #pragma omp simd simd-clause[optseq] new-line
     for-loop  */

#define OMP_SIMD_CLAUSE_MASK					\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_SAFELEN)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_LINEAR)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_ALIGNED)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_PRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_LASTPRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_REDUCTION)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_COLLAPSE))

static tree
cp_parser_omp_simd (cp_parser *parser, cp_token *pragma_tok,
		    char *p_name, omp_clause_mask mask, tree *cclauses)
{
  tree clauses, sb, ret;
  unsigned int save;
  location_t loc = cp_lexer_peek_token (parser->lexer)->location;

  strcat (p_name, " simd");
  mask |= OMP_SIMD_CLAUSE_MASK;
  mask &= ~(OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_ORDERED);

  clauses = cp_parser_omp_all_clauses (parser, mask, p_name, pragma_tok,
				       cclauses == NULL);
  if (cclauses)
    {
      cp_omp_split_clauses (loc, OMP_SIMD, mask, clauses, cclauses);
      clauses = cclauses[C_OMP_CLAUSE_SPLIT_SIMD];
    }

  sb = begin_omp_structured_block ();
  save = cp_parser_begin_omp_structured_block (parser);

  ret = cp_parser_omp_for_loop (parser, OMP_SIMD, clauses, cclauses);

  cp_parser_end_omp_structured_block (parser, save);
  add_stmt (finish_omp_structured_block (sb));

  return ret;
}

/* OpenMP 2.5:
   #pragma omp for for-clause[optseq] new-line
     for-loop

   OpenMP 4.0:
   #pragma omp for simd for-simd-clause[optseq] new-line
     for-loop  */

#define OMP_FOR_CLAUSE_MASK					\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_PRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_FIRSTPRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_LASTPRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_REDUCTION)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_ORDERED)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_SCHEDULE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NOWAIT)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_COLLAPSE))

static tree
cp_parser_omp_for (cp_parser *parser, cp_token *pragma_tok,
		   char *p_name, omp_clause_mask mask, tree *cclauses)
{
  tree clauses, sb, ret;
  unsigned int save;
  location_t loc = cp_lexer_peek_token (parser->lexer)->location;

  strcat (p_name, " for");
  mask |= OMP_FOR_CLAUSE_MASK;
  if (cclauses)
    mask &= ~(OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NOWAIT);

  if (cp_lexer_next_token_is (parser->lexer, CPP_NAME))
    {
      tree id = cp_lexer_peek_token (parser->lexer)->u.value;
      const char *p = IDENTIFIER_POINTER (id);

      if (strcmp (p, "simd") == 0)
	{
	  tree cclauses_buf[C_OMP_CLAUSE_SPLIT_COUNT];
	  if (cclauses == NULL)
	    cclauses = cclauses_buf;

	  cp_lexer_consume_token (parser->lexer);
	  if (!flag_openmp)  /* flag_openmp_simd  */
	    return cp_parser_omp_simd (parser, pragma_tok, p_name, mask,
				       cclauses);
	  sb = begin_omp_structured_block ();
	  save = cp_parser_begin_omp_structured_block (parser);
	  ret = cp_parser_omp_simd (parser, pragma_tok, p_name, mask,
				    cclauses);
	  cp_parser_end_omp_structured_block (parser, save);
	  tree body = finish_omp_structured_block (sb);
	  if (ret == NULL)
	    return ret;
	  ret = make_node (OMP_FOR);
	  TREE_TYPE (ret) = void_type_node;
	  OMP_FOR_BODY (ret) = body;
	  OMP_FOR_CLAUSES (ret) = cclauses[C_OMP_CLAUSE_SPLIT_FOR];
	  SET_EXPR_LOCATION (ret, loc);
	  add_stmt (ret);
	  return ret;
	}
    }
  if (!flag_openmp)  /* flag_openmp_simd  */
    {
      cp_parser_require_pragma_eol (parser, pragma_tok);
      return NULL_TREE;
    }

  clauses = cp_parser_omp_all_clauses (parser, mask, p_name, pragma_tok,
				       cclauses == NULL);
  if (cclauses)
    {
      cp_omp_split_clauses (loc, OMP_FOR, mask, clauses, cclauses);
      clauses = cclauses[C_OMP_CLAUSE_SPLIT_FOR];
    }

  sb = begin_omp_structured_block ();
  save = cp_parser_begin_omp_structured_block (parser);

  ret = cp_parser_omp_for_loop (parser, OMP_FOR, clauses, cclauses);

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

  if (!cp_parser_require (parser, CPP_OPEN_BRACE, RT_OPEN_BRACE))
    return NULL_TREE;

  stmt = push_stmt_list ();

  if (cp_lexer_peek_token (parser->lexer)->pragma_kind != PRAGMA_OMP_SECTION)
    {
      substmt = cp_parser_omp_structured_block (parser);
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
  cp_parser_require (parser, CPP_CLOSE_BRACE, RT_CLOSE_BRACE);

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

#define OMP_SECTIONS_CLAUSE_MASK				\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_PRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_FIRSTPRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_LASTPRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_REDUCTION)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NOWAIT))

static tree
cp_parser_omp_sections (cp_parser *parser, cp_token *pragma_tok,
			char *p_name, omp_clause_mask mask, tree *cclauses)
{
  tree clauses, ret;
  location_t loc = cp_lexer_peek_token (parser->lexer)->location;

  strcat (p_name, " sections");
  mask |= OMP_SECTIONS_CLAUSE_MASK;
  if (cclauses)
    mask &= ~(OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NOWAIT);

  clauses = cp_parser_omp_all_clauses (parser, mask, p_name, pragma_tok,
				       cclauses == NULL);
  if (cclauses)
    {
      cp_omp_split_clauses (loc, OMP_SECTIONS, mask, clauses, cclauses);
      clauses = cclauses[C_OMP_CLAUSE_SPLIT_SECTIONS];
    }

  ret = cp_parser_omp_sections_scope (parser);
  if (ret)
    OMP_SECTIONS_CLAUSES (ret) = clauses;

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
     structured-block */

#define OMP_PARALLEL_CLAUSE_MASK				\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_IF)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_PRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_FIRSTPRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_DEFAULT)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_SHARED)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_COPYIN)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_REDUCTION)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NUM_THREADS)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_PROC_BIND))

static tree
cp_parser_omp_parallel (cp_parser *parser, cp_token *pragma_tok,
			char *p_name, omp_clause_mask mask, tree *cclauses)
{
  tree stmt, clauses, block;
  unsigned int save;
  location_t loc = cp_lexer_peek_token (parser->lexer)->location;

  strcat (p_name, " parallel");
  mask |= OMP_PARALLEL_CLAUSE_MASK;

  if (cp_lexer_next_token_is_keyword (parser->lexer, RID_FOR))
    {
      tree cclauses_buf[C_OMP_CLAUSE_SPLIT_COUNT];
      if (cclauses == NULL)
	cclauses = cclauses_buf;

      cp_lexer_consume_token (parser->lexer);
      if (!flag_openmp)  /* flag_openmp_simd  */
	return cp_parser_omp_for (parser, pragma_tok, p_name, mask, cclauses);
      block = begin_omp_parallel ();
      save = cp_parser_begin_omp_structured_block (parser);
      cp_parser_omp_for (parser, pragma_tok, p_name, mask, cclauses);
      cp_parser_end_omp_structured_block (parser, save);
      stmt = finish_omp_parallel (cclauses[C_OMP_CLAUSE_SPLIT_PARALLEL],
				  block);
      OMP_PARALLEL_COMBINED (stmt) = 1;
      return stmt;
    }
  else if (cclauses)
    {
      error_at (loc, "expected %<for%> after %qs", p_name);
      cp_parser_skip_to_pragma_eol (parser, pragma_tok);
      return NULL_TREE;
    }
  else if (!flag_openmp)  /* flag_openmp_simd  */
    {
      cp_parser_require_pragma_eol (parser, pragma_tok);
      return NULL_TREE;
    }
  else if (cp_lexer_next_token_is (parser->lexer, CPP_NAME))
    {
      tree id = cp_lexer_peek_token (parser->lexer)->u.value;
      const char *p = IDENTIFIER_POINTER (id);
      if (strcmp (p, "sections") == 0)
	{
	  tree cclauses_buf[C_OMP_CLAUSE_SPLIT_COUNT];
	  cclauses = cclauses_buf;

	  cp_lexer_consume_token (parser->lexer);
	  block = begin_omp_parallel ();
	  save = cp_parser_begin_omp_structured_block (parser);
	  cp_parser_omp_sections (parser, pragma_tok, p_name, mask, cclauses);
	  cp_parser_end_omp_structured_block (parser, save);
	  stmt = finish_omp_parallel (cclauses[C_OMP_CLAUSE_SPLIT_PARALLEL],
				      block);
	  OMP_PARALLEL_COMBINED (stmt) = 1;
	  return stmt;
	}
    }

  clauses = cp_parser_omp_all_clauses (parser, mask, p_name, pragma_tok);

  block = begin_omp_parallel ();
  save = cp_parser_begin_omp_structured_block (parser);
  cp_parser_statement (parser, NULL_TREE, false, NULL);
  cp_parser_end_omp_structured_block (parser, save);
  stmt = finish_omp_parallel (clauses, block);
  return stmt;
}

/* OpenMP 2.5:
   # pragma omp single single-clause[optseq] new-line
     structured-block  */

#define OMP_SINGLE_CLAUSE_MASK					\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_PRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_FIRSTPRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_COPYPRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NOWAIT))

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

#define OMP_TASK_CLAUSE_MASK					\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_IF)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_UNTIED)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_DEFAULT)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_PRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_FIRSTPRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_SHARED)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_FINAL)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_MERGEABLE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_DEPEND))

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

/* OpenMP 3.1:
   # pragma omp taskyield new-line  */

static void
cp_parser_omp_taskyield (cp_parser *parser, cp_token *pragma_tok)
{
  cp_parser_require_pragma_eol (parser, pragma_tok);
  finish_omp_taskyield ();
}

/* OpenMP 4.0:
   # pragma omp taskgroup new-line
     structured-block  */

static tree
cp_parser_omp_taskgroup (cp_parser *parser, cp_token *pragma_tok)
{
  cp_parser_require_pragma_eol (parser, pragma_tok);
  return c_finish_omp_taskgroup (input_location,
				 cp_parser_omp_structured_block (parser));
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

/* OpenMP 4.0:
   # pragma omp cancel cancel-clause[optseq] new-line  */

#define OMP_CANCEL_CLAUSE_MASK					\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_PARALLEL)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_FOR)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_SECTIONS)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_TASKGROUP)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_IF))

static void
cp_parser_omp_cancel (cp_parser *parser, cp_token *pragma_tok)
{
  tree clauses = cp_parser_omp_all_clauses (parser, OMP_CANCEL_CLAUSE_MASK,
					    "#pragma omp cancel", pragma_tok);
  finish_omp_cancel (clauses);
}

/* OpenMP 4.0:
   # pragma omp cancellation point cancelpt-clause[optseq] new-line  */

#define OMP_CANCELLATION_POINT_CLAUSE_MASK			\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_PARALLEL)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_FOR)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_SECTIONS)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_TASKGROUP))

static void
cp_parser_omp_cancellation_point (cp_parser *parser, cp_token *pragma_tok)
{
  tree clauses;
  bool point_seen = false;

  if (cp_lexer_next_token_is (parser->lexer, CPP_NAME))
    {
      tree id = cp_lexer_peek_token (parser->lexer)->u.value;
      const char *p = IDENTIFIER_POINTER (id);

      if (strcmp (p, "point") == 0)
	{
	  cp_lexer_consume_token (parser->lexer);
	  point_seen = true;
	}
    }
  if (!point_seen)
    {
      cp_parser_error (parser, "expected %<point%>");
      cp_parser_require_pragma_eol (parser, pragma_tok);
      return;
    }

  clauses = cp_parser_omp_all_clauses (parser,
				       OMP_CANCELLATION_POINT_CLAUSE_MASK,
				       "#pragma omp cancellation point",
				       pragma_tok);
  finish_omp_cancellation_point (clauses);
}

/* OpenMP 4.0:
   #pragma omp distribute distribute-clause[optseq] new-line
     for-loop  */

#define OMP_DISTRIBUTE_CLAUSE_MASK				\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_PRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_FIRSTPRIVATE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_DIST_SCHEDULE)\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_COLLAPSE))

static tree
cp_parser_omp_distribute (cp_parser *parser, cp_token *pragma_tok,
			  char *p_name, omp_clause_mask mask, tree *cclauses)
{
  tree clauses, sb, ret;
  unsigned int save;
  location_t loc = cp_lexer_peek_token (parser->lexer)->location;

  strcat (p_name, " distribute");
  mask |= OMP_DISTRIBUTE_CLAUSE_MASK;

  if (cp_lexer_next_token_is (parser->lexer, CPP_NAME))
    {
      tree id = cp_lexer_peek_token (parser->lexer)->u.value;
      const char *p = IDENTIFIER_POINTER (id);
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
	  cp_lexer_consume_token (parser->lexer);
	  if (!flag_openmp)  /* flag_openmp_simd  */
	    {
	      if (simd)
		return cp_parser_omp_simd (parser, pragma_tok, p_name, mask,
					   cclauses);
	      else
		return cp_parser_omp_parallel (parser, pragma_tok, p_name, mask,
					       cclauses);
	    }
	  sb = begin_omp_structured_block ();
	  save = cp_parser_begin_omp_structured_block (parser);
	  if (simd)
	    ret = cp_parser_omp_simd (parser, pragma_tok, p_name, mask,
				      cclauses);
	  else
	    ret = cp_parser_omp_parallel (parser, pragma_tok, p_name, mask,
					  cclauses);
	  cp_parser_end_omp_structured_block (parser, save);
	  tree body = finish_omp_structured_block (sb);
	  if (ret == NULL)
	    return ret;
	  ret = make_node (OMP_DISTRIBUTE);
	  TREE_TYPE (ret) = void_type_node;
	  OMP_FOR_BODY (ret) = body;
	  OMP_FOR_CLAUSES (ret) = cclauses[C_OMP_CLAUSE_SPLIT_DISTRIBUTE];
	  SET_EXPR_LOCATION (ret, loc);
	  add_stmt (ret);
	  return ret;
	}
    }
  if (!flag_openmp)  /* flag_openmp_simd  */
    {
      cp_parser_require_pragma_eol (parser, pragma_tok);
      return NULL_TREE;
    }

  clauses = cp_parser_omp_all_clauses (parser, mask, p_name, pragma_tok,
				       cclauses == NULL);
  if (cclauses)
    {
      cp_omp_split_clauses (loc, OMP_DISTRIBUTE, mask, clauses, cclauses);
      clauses = cclauses[C_OMP_CLAUSE_SPLIT_DISTRIBUTE];
    }

  sb = begin_omp_structured_block ();
  save = cp_parser_begin_omp_structured_block (parser);

  ret = cp_parser_omp_for_loop (parser, OMP_DISTRIBUTE, clauses, NULL);

  cp_parser_end_omp_structured_block (parser, save);
  add_stmt (finish_omp_structured_block (sb));

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
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_DEFAULT))

static tree
cp_parser_omp_teams (cp_parser *parser, cp_token *pragma_tok,
		     char *p_name, omp_clause_mask mask, tree *cclauses)
{
  tree clauses, sb, ret;
  unsigned int save;
  location_t loc = cp_lexer_peek_token (parser->lexer)->location;

  strcat (p_name, " teams");
  mask |= OMP_TEAMS_CLAUSE_MASK;

  if (cp_lexer_next_token_is (parser->lexer, CPP_NAME))
    {
      tree id = cp_lexer_peek_token (parser->lexer)->u.value;
      const char *p = IDENTIFIER_POINTER (id);
      if (strcmp (p, "distribute") == 0)
	{
	  tree cclauses_buf[C_OMP_CLAUSE_SPLIT_COUNT];
	  if (cclauses == NULL)
	    cclauses = cclauses_buf;

	  cp_lexer_consume_token (parser->lexer);
	  if (!flag_openmp)  /* flag_openmp_simd  */
	    return cp_parser_omp_distribute (parser, pragma_tok, p_name, mask,
					     cclauses);
	  sb = begin_omp_structured_block ();
	  save = cp_parser_begin_omp_structured_block (parser);
	  ret = cp_parser_omp_distribute (parser, pragma_tok, p_name, mask,
					  cclauses);
	  cp_parser_end_omp_structured_block (parser, save);
	  tree body = finish_omp_structured_block (sb);
	  if (ret == NULL)
	    return ret;
	  clauses = cclauses[C_OMP_CLAUSE_SPLIT_TEAMS];
	  ret = make_node (OMP_TEAMS);
	  TREE_TYPE (ret) = void_type_node;
	  OMP_TEAMS_CLAUSES (ret) = clauses;
	  OMP_TEAMS_BODY (ret) = body;
	  return add_stmt (ret);
	}
    }
  if (!flag_openmp)  /* flag_openmp_simd  */
    {
      cp_parser_require_pragma_eol (parser, pragma_tok);
      return NULL_TREE;
    }

  clauses = cp_parser_omp_all_clauses (parser, mask, p_name, pragma_tok,
				       cclauses == NULL);
  if (cclauses)
    {
      cp_omp_split_clauses (loc, OMP_TEAMS, mask, clauses, cclauses);
      clauses = cclauses[C_OMP_CLAUSE_SPLIT_TEAMS];
    }

  tree stmt = make_node (OMP_TEAMS);
  TREE_TYPE (stmt) = void_type_node;
  OMP_TEAMS_CLAUSES (stmt) = clauses;
  OMP_TEAMS_BODY (stmt) = cp_parser_omp_structured_block (parser);

  return add_stmt (stmt);
}

/* OpenMP 4.0:
   # pragma omp target data target-data-clause[optseq] new-line
     structured-block  */

#define OMP_TARGET_DATA_CLAUSE_MASK				\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_DEVICE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_MAP)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_IF))

static tree
cp_parser_omp_target_data (cp_parser *parser, cp_token *pragma_tok)
{
  tree stmt = make_node (OMP_TARGET_DATA);
  TREE_TYPE (stmt) = void_type_node;

  OMP_TARGET_DATA_CLAUSES (stmt)
    = cp_parser_omp_all_clauses (parser, OMP_TARGET_DATA_CLAUSE_MASK,
				 "#pragma omp target data", pragma_tok);
  keep_next_level (true);
  OMP_TARGET_DATA_BODY (stmt) = cp_parser_omp_structured_block (parser);

  SET_EXPR_LOCATION (stmt, pragma_tok->location);
  return add_stmt (stmt);
}

/* OpenMP 4.0:
   # pragma omp target update target-update-clause[optseq] new-line */

#define OMP_TARGET_UPDATE_CLAUSE_MASK				\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_FROM)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_TO)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_DEVICE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_IF))

static bool
cp_parser_omp_target_update (cp_parser *parser, cp_token *pragma_tok,
			     enum pragma_context context)
{
  if (context == pragma_stmt)
    {
      error_at (pragma_tok->location,
		"%<#pragma omp target update%> may only be "
		"used in compound statements");
      cp_parser_skip_to_pragma_eol (parser, pragma_tok);
      return false;
    }

  tree clauses
    = cp_parser_omp_all_clauses (parser, OMP_TARGET_UPDATE_CLAUSE_MASK,
				 "#pragma omp target update", pragma_tok);
  if (find_omp_clause (clauses, OMP_CLAUSE_TO) == NULL_TREE
      && find_omp_clause (clauses, OMP_CLAUSE_FROM) == NULL_TREE)
    {
      error_at (pragma_tok->location,
		"%<#pragma omp target update must contain at least one "
		"%<from%> or %<to%> clauses");
      return false;
    }

  tree stmt = make_node (OMP_TARGET_UPDATE);
  TREE_TYPE (stmt) = void_type_node;
  OMP_TARGET_UPDATE_CLAUSES (stmt) = clauses;
  SET_EXPR_LOCATION (stmt, pragma_tok->location);
  add_stmt (stmt);
  return false;
}

/* OpenMP 4.0:
   # pragma omp target target-clause[optseq] new-line
     structured-block  */

#define OMP_TARGET_CLAUSE_MASK					\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_DEVICE)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_MAP)		\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_IF))

static bool
cp_parser_omp_target (cp_parser *parser, cp_token *pragma_tok,
		      enum pragma_context context)
{
  if (context != pragma_stmt && context != pragma_compound)
    {
      cp_parser_error (parser, "expected declaration specifiers");
      cp_parser_skip_to_pragma_eol (parser, pragma_tok);
      return false;
    }

  if (cp_lexer_next_token_is (parser->lexer, CPP_NAME))
    {
      tree id = cp_lexer_peek_token (parser->lexer)->u.value;
      const char *p = IDENTIFIER_POINTER (id);

      if (strcmp (p, "teams") == 0)
	{
	  tree cclauses[C_OMP_CLAUSE_SPLIT_COUNT];
	  char p_name[sizeof ("#pragma omp target teams distribute "
			      "parallel for simd")];

	  cp_lexer_consume_token (parser->lexer);
	  strcpy (p_name, "#pragma omp target");
	  if (!flag_openmp)  /* flag_openmp_simd  */
	    return cp_parser_omp_teams (parser, pragma_tok, p_name,
					OMP_TARGET_CLAUSE_MASK, cclauses);
	  keep_next_level (true);
	  tree sb = begin_omp_structured_block ();
	  unsigned save = cp_parser_begin_omp_structured_block (parser);
	  tree ret = cp_parser_omp_teams (parser, pragma_tok, p_name,
					  OMP_TARGET_CLAUSE_MASK, cclauses);
	  cp_parser_end_omp_structured_block (parser, save);
	  tree body = finish_omp_structured_block (sb);
	  if (ret == NULL)
	    return ret;
	  tree stmt = make_node (OMP_TARGET);
	  TREE_TYPE (stmt) = void_type_node;
	  OMP_TARGET_CLAUSES (stmt) = cclauses[C_OMP_CLAUSE_SPLIT_TARGET];
	  OMP_TARGET_BODY (stmt) = body;
	  add_stmt (stmt);
	  return true;
	}
      else if (!flag_openmp)  /* flag_openmp_simd  */
	{
	  cp_parser_require_pragma_eol (parser, pragma_tok);
	  return NULL_TREE;
	}
      else if (strcmp (p, "data") == 0)
	{
	  cp_lexer_consume_token (parser->lexer);
	  cp_parser_omp_target_data (parser, pragma_tok);
	  return true;
	}
      else if (strcmp (p, "update") == 0)
	{
	  cp_lexer_consume_token (parser->lexer);
	  return cp_parser_omp_target_update (parser, pragma_tok, context);
	}
    }

  tree stmt = make_node (OMP_TARGET);
  TREE_TYPE (stmt) = void_type_node;

  OMP_TARGET_CLAUSES (stmt)
    = cp_parser_omp_all_clauses (parser, OMP_TARGET_CLAUSE_MASK,
				 "#pragma omp target", pragma_tok);
  keep_next_level (true);
  OMP_TARGET_BODY (stmt) = cp_parser_omp_structured_block (parser);

  SET_EXPR_LOCATION (stmt, pragma_tok->location);
  add_stmt (stmt);
  return true;
}

/* OpenMP 4.0:
   # pragma omp declare simd declare-simd-clauses[optseq] new-line  */

#define OMP_DECLARE_SIMD_CLAUSE_MASK				\
	( (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_SIMDLEN)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_LINEAR)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_ALIGNED)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_UNIFORM)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_INBRANCH)	\
	| (OMP_CLAUSE_MASK_1 << PRAGMA_OMP_CLAUSE_NOTINBRANCH))

static void
cp_parser_omp_declare_simd (cp_parser *parser, cp_token *pragma_tok,
			    enum pragma_context context)
{
  bool first_p = parser->omp_declare_simd == NULL;
  cp_omp_declare_simd_data data;
  if (first_p)
    {
      data.error_seen = false;
      data.fndecl_seen = false;
      data.tokens = vNULL;
      parser->omp_declare_simd = &data;
    }
  while (cp_lexer_next_token_is_not (parser->lexer, CPP_PRAGMA_EOL)
	 && cp_lexer_next_token_is_not (parser->lexer, CPP_EOF))
    cp_lexer_consume_token (parser->lexer);
  if (cp_lexer_next_token_is_not (parser->lexer, CPP_PRAGMA_EOL))
    parser->omp_declare_simd->error_seen = true;
  cp_parser_require_pragma_eol (parser, pragma_tok);
  struct cp_token_cache *cp
    = cp_token_cache_new (pragma_tok, cp_lexer_peek_token (parser->lexer));
  parser->omp_declare_simd->tokens.safe_push (cp);
  if (first_p)
    {
      while (cp_lexer_next_token_is (parser->lexer, CPP_PRAGMA))
	cp_parser_pragma (parser, context);
      switch (context)
	{
	case pragma_external:
	  cp_parser_declaration (parser);
	  break;
	case pragma_member:
	  cp_parser_member_declaration (parser);
	  break;
	case pragma_objc_icode:
	  cp_parser_block_declaration (parser, /*statement_p=*/false);
	  break;
	default:
	  cp_parser_declaration_statement (parser);
	  break;
	}
      if (parser->omp_declare_simd
	  && !parser->omp_declare_simd->error_seen
	  && !parser->omp_declare_simd->fndecl_seen)
	error_at (pragma_tok->location,
		  "%<#pragma omp declare simd%> not immediately followed by "
		  "function declaration or definition");
      data.tokens.release ();
      parser->omp_declare_simd = NULL;
    }
}

/* Handles the delayed parsing of the Cilk Plus SIMD-enabled function.  
   This function is modelled similar to the late parsing of omp declare 
   simd.  */

static tree
cp_parser_late_parsing_cilk_simd_fn_info (cp_parser *parser, tree attrs)
{
  struct cp_token_cache *ce;
  cp_omp_declare_simd_data *info = parser->cilk_simd_fn_info;
  int ii = 0;

  if (parser->omp_declare_simd != NULL)
    {
      error ("%<#pragma omp declare simd%> cannot be used in the same function"
	     " marked as a Cilk Plus SIMD-enabled function");
      XDELETE (parser->cilk_simd_fn_info);
      parser->cilk_simd_fn_info = NULL;
      return attrs;
    }
  if (!info->error_seen && info->fndecl_seen)
    {
      error ("vector attribute not immediately followed by a single function"
	     " declaration or definition");
      info->error_seen = true;
    }
  if (info->error_seen)
    return attrs;

  FOR_EACH_VEC_ELT (info->tokens, ii, ce)
    {
      tree c, cl;

      cp_parser_push_lexer_for_tokens (parser, ce);
      parser->lexer->in_pragma = true;
      cl = cp_parser_omp_all_clauses (parser, CILK_SIMD_FN_CLAUSE_MASK,
				      "SIMD-enabled functions attribute", 
				      NULL);
      cp_parser_pop_lexer (parser);
      if (cl)
	cl = tree_cons (NULL_TREE, cl, NULL_TREE);

      c = build_tree_list (get_identifier ("cilk simd function"), NULL_TREE);
      TREE_CHAIN (c) = attrs;
      attrs = c;

      c = build_tree_list (get_identifier ("omp declare simd"), cl);
      TREE_CHAIN (c) = attrs;
      if (processing_template_decl)
	ATTR_IS_DEPENDENT (c) = 1;
      attrs = c;
    }
  info->fndecl_seen = true;
  XDELETE (parser->cilk_simd_fn_info);
  parser->cilk_simd_fn_info = NULL;
  return attrs;
}

/* Finalize #pragma omp declare simd clauses after direct declarator has
   been parsed, and put that into "omp declare simd" attribute.  */

static tree
cp_parser_late_parsing_omp_declare_simd (cp_parser *parser, tree attrs)
{
  struct cp_token_cache *ce;
  cp_omp_declare_simd_data *data = parser->omp_declare_simd;
  int i;

  if (!data->error_seen && data->fndecl_seen)
    {
      error ("%<#pragma omp declare simd%> not immediately followed by "
	     "a single function declaration or definition");
      data->error_seen = true;
      return attrs;
    }
  if (data->error_seen)
    return attrs;

  FOR_EACH_VEC_ELT (data->tokens, i, ce)
    {
      tree c, cl;

      cp_parser_push_lexer_for_tokens (parser, ce);
      parser->lexer->in_pragma = true;
      gcc_assert (cp_lexer_peek_token (parser->lexer)->type == CPP_PRAGMA);
      cp_token *pragma_tok = cp_lexer_consume_token (parser->lexer);
      cp_lexer_consume_token (parser->lexer);
      cl = cp_parser_omp_all_clauses (parser, OMP_DECLARE_SIMD_CLAUSE_MASK,
				      "#pragma omp declare simd", pragma_tok);
      cp_parser_pop_lexer (parser);
      if (cl)
	cl = tree_cons (NULL_TREE, cl, NULL_TREE);
      c = build_tree_list (get_identifier ("omp declare simd"), cl);
      TREE_CHAIN (c) = attrs;
      if (processing_template_decl)
	ATTR_IS_DEPENDENT (c) = 1;
      attrs = c;
    }

  data->fndecl_seen = true;
  return attrs;
}


/* OpenMP 4.0:
   # pragma omp declare target new-line
   declarations and definitions
   # pragma omp end declare target new-line  */

static void
cp_parser_omp_declare_target (cp_parser *parser, cp_token *pragma_tok)
{
  cp_parser_skip_to_pragma_eol (parser, pragma_tok);
  scope_chain->omp_declare_target_attribute++;
}

static void
cp_parser_omp_end_declare_target (cp_parser *parser, cp_token *pragma_tok)
{
  const char *p = "";
  if (cp_lexer_next_token_is (parser->lexer, CPP_NAME))
    {
      tree id = cp_lexer_peek_token (parser->lexer)->u.value;
      p = IDENTIFIER_POINTER (id);
    }
  if (strcmp (p, "declare") == 0)
    {
      cp_lexer_consume_token (parser->lexer);
      p = "";
      if (cp_lexer_next_token_is (parser->lexer, CPP_NAME))
	{
	  tree id = cp_lexer_peek_token (parser->lexer)->u.value;
	  p = IDENTIFIER_POINTER (id);
	}
      if (strcmp (p, "target") == 0)
	cp_lexer_consume_token (parser->lexer);
      else
	{
	  cp_parser_error (parser, "expected %<target%>");
	  cp_parser_skip_to_pragma_eol (parser, pragma_tok);
	  return;
	}
    }
  else
    {
      cp_parser_error (parser, "expected %<declare%>");
      cp_parser_skip_to_pragma_eol (parser, pragma_tok);
      return;
    }
  cp_parser_skip_to_pragma_eol (parser, pragma_tok);
  if (!scope_chain->omp_declare_target_attribute)
    error_at (pragma_tok->location,
	      "%<#pragma omp end declare target%> without corresponding "
	      "%<#pragma omp declare target%>");
  else
    scope_chain->omp_declare_target_attribute--;
}

/* Helper function of cp_parser_omp_declare_reduction.  Parse the combiner
   expression and optional initializer clause of
   #pragma omp declare reduction.  We store the expression(s) as
   either 3, 6 or 7 special statements inside of the artificial function's
   body.  The first two statements are DECL_EXPRs for the artificial
   OMP_OUT resp. OMP_IN variables, followed by a statement with the combiner
   expression that uses those variables.
   If there was any INITIALIZER clause, this is followed by further statements,
   the fourth and fifth statements are DECL_EXPRs for the artificial
   OMP_PRIV resp. OMP_ORIG variables.  If the INITIALIZER clause wasn't the
   constructor variant (first token after open paren is not omp_priv),
   then the sixth statement is a statement with the function call expression
   that uses the OMP_PRIV and optionally OMP_ORIG variable.
   Otherwise, the sixth statement is whatever statement cp_finish_decl emits
   to initialize the OMP_PRIV artificial variable and there is seventh
   statement, a DECL_EXPR of the OMP_PRIV statement again.  */

static bool
cp_parser_omp_declare_reduction_exprs (tree fndecl, cp_parser *parser)
{
  tree type = TREE_VALUE (TYPE_ARG_TYPES (TREE_TYPE (fndecl)));
  gcc_assert (TREE_CODE (type) == REFERENCE_TYPE);
  type = TREE_TYPE (type);
  tree omp_out = build_lang_decl (VAR_DECL, get_identifier ("omp_out"), type);
  DECL_ARTIFICIAL (omp_out) = 1;
  pushdecl (omp_out);
  add_decl_expr (omp_out);
  tree omp_in = build_lang_decl (VAR_DECL, get_identifier ("omp_in"), type);
  DECL_ARTIFICIAL (omp_in) = 1;
  pushdecl (omp_in);
  add_decl_expr (omp_in);
  tree combiner;
  tree omp_priv = NULL_TREE, omp_orig = NULL_TREE, initializer = NULL_TREE;

  keep_next_level (true);
  tree block = begin_omp_structured_block ();
  combiner = cp_parser_expression (parser, false, NULL);
  finish_expr_stmt (combiner);
  block = finish_omp_structured_block (block);
  add_stmt (block);

  if (!cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN))
    return false;

  const char *p = "";
  if (cp_lexer_next_token_is (parser->lexer, CPP_NAME))
    {
      tree id = cp_lexer_peek_token (parser->lexer)->u.value;
      p = IDENTIFIER_POINTER (id);
    }

  if (strcmp (p, "initializer") == 0)
    {
      cp_lexer_consume_token (parser->lexer);
      if (!cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN))
	return false;

      p = "";
      if (cp_lexer_next_token_is (parser->lexer, CPP_NAME))
	{
	  tree id = cp_lexer_peek_token (parser->lexer)->u.value;
	  p = IDENTIFIER_POINTER (id);
	}

      omp_priv = build_lang_decl (VAR_DECL, get_identifier ("omp_priv"), type);
      DECL_ARTIFICIAL (omp_priv) = 1;
      pushdecl (omp_priv);
      add_decl_expr (omp_priv);
      omp_orig = build_lang_decl (VAR_DECL, get_identifier ("omp_orig"), type);
      DECL_ARTIFICIAL (omp_orig) = 1;
      pushdecl (omp_orig);
      add_decl_expr (omp_orig);

      keep_next_level (true);
      block = begin_omp_structured_block ();

      bool ctor = false;
      if (strcmp (p, "omp_priv") == 0)
	{
	  bool is_direct_init, is_non_constant_init;
	  ctor = true;
	  cp_lexer_consume_token (parser->lexer);
	  /* Reject initializer (omp_priv) and initializer (omp_priv ()).  */
	  if (cp_lexer_next_token_is (parser->lexer, CPP_CLOSE_PAREN)
	      || (cp_lexer_next_token_is (parser->lexer, CPP_OPEN_PAREN)
		  && cp_lexer_peek_nth_token (parser->lexer, 2)->type
		     == CPP_CLOSE_PAREN
		  && cp_lexer_peek_nth_token (parser->lexer, 3)->type
		     == CPP_CLOSE_PAREN))
	    {
	      finish_omp_structured_block (block);
	      error ("invalid initializer clause");
	      return false;
	    }
	  initializer = cp_parser_initializer (parser, &is_direct_init,
					       &is_non_constant_init);
	  cp_finish_decl (omp_priv, initializer, !is_non_constant_init,
			  NULL_TREE, LOOKUP_ONLYCONVERTING);
	}
      else
	{
	  cp_parser_parse_tentatively (parser);
	  tree fn_name = cp_parser_id_expression (parser, /*template_p=*/false,
						  /*check_dependency_p=*/true,
						  /*template_p=*/NULL,
						  /*declarator_p=*/false,
						  /*optional_p=*/false);
	  vec<tree, va_gc> *args;
	  if (fn_name == error_mark_node
	      || cp_parser_error_occurred (parser)
	      || !cp_lexer_next_token_is (parser->lexer, CPP_OPEN_PAREN)
	      || ((args = cp_parser_parenthesized_expression_list
				(parser, non_attr, /*cast_p=*/false,
				 /*allow_expansion_p=*/true,
				 /*non_constant_p=*/NULL)),
		  cp_parser_error_occurred (parser)))
	    {
	      finish_omp_structured_block (block);
	      cp_parser_abort_tentative_parse (parser);
	      cp_parser_error (parser, "expected id-expression (arguments)");
	      return false;
	    }
	  unsigned int i;
	  tree arg;
	  FOR_EACH_VEC_SAFE_ELT (args, i, arg)
	    if (arg == omp_priv
		|| (TREE_CODE (arg) == ADDR_EXPR
		    && TREE_OPERAND (arg, 0) == omp_priv))
	      break;
	  cp_parser_abort_tentative_parse (parser);
	  if (arg == NULL_TREE)
	    error ("one of the initializer call arguments should be %<omp_priv%>"
		   " or %<&omp_priv%>");
	  initializer = cp_parser_postfix_expression (parser, false, false, false,
						      false, NULL);
	  finish_expr_stmt (initializer);
	}

      block = finish_omp_structured_block (block);
      cp_walk_tree (&block, cp_remove_omp_priv_cleanup_stmt, omp_priv, NULL);
      finish_expr_stmt (block);

      if (ctor)
	add_decl_expr (omp_orig);

      if (!cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN))
	return false;
    }

  if (!cp_lexer_next_token_is (parser->lexer, CPP_PRAGMA_EOL))
    cp_parser_required_error (parser, RT_PRAGMA_EOL, /*keyword=*/false);

  return true;
}

/* OpenMP 4.0
   #pragma omp declare reduction (reduction-id : typename-list : expression) \
      initializer-clause[opt] new-line

   initializer-clause:
      initializer (omp_priv initializer)
      initializer (function-name (argument-list))  */

static void
cp_parser_omp_declare_reduction (cp_parser *parser, cp_token *pragma_tok,
				 enum pragma_context)
{
  auto_vec<tree> types;
  enum tree_code reduc_code = ERROR_MARK;
  tree reduc_id = NULL_TREE, orig_reduc_id = NULL_TREE, type;
  unsigned int i;
  cp_token *first_token;
  cp_token_cache *cp;
  int errs;
  void *p;
    
  /* Get the high-water mark for the DECLARATOR_OBSTACK.  */
  p = obstack_alloc (&declarator_obstack, 0);

  if (!cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN))
    goto fail;

  switch (cp_lexer_peek_token (parser->lexer)->type)
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
      reduc_id = orig_reduc_id = cp_parser_identifier (parser);
      break;
    default:
      cp_parser_error (parser, "expected %<+%>, %<*%>, %<-%>, %<&%>, %<^%>, "
			       "%<|%>, %<&&%>, %<||%> or identifier");
      goto fail;
    }

  if (reduc_code != ERROR_MARK)
    cp_lexer_consume_token (parser->lexer);

  reduc_id = omp_reduction_id (reduc_code, reduc_id, NULL_TREE);
  if (reduc_id == error_mark_node)
    goto fail;

  if (!cp_parser_require (parser, CPP_COLON, RT_COLON))
    goto fail;

  /* Types may not be defined in declare reduction type list.  */
  const char *saved_message;
  saved_message = parser->type_definition_forbidden_message;
  parser->type_definition_forbidden_message
    = G_("types may not be defined in declare reduction type list");
  bool saved_colon_corrects_to_scope_p;
  saved_colon_corrects_to_scope_p = parser->colon_corrects_to_scope_p;
  parser->colon_corrects_to_scope_p = false;
  bool saved_colon_doesnt_start_class_def_p;
  saved_colon_doesnt_start_class_def_p
    = parser->colon_doesnt_start_class_def_p;
  parser->colon_doesnt_start_class_def_p = true;

  while (true)
    {
      location_t loc = cp_lexer_peek_token (parser->lexer)->location;
      type = cp_parser_type_id (parser);
      if (type == error_mark_node)
	;
      else if (ARITHMETIC_TYPE_P (type)
	       && (orig_reduc_id == NULL_TREE
		   || (TREE_CODE (type) != COMPLEX_TYPE
		       && (strcmp (IDENTIFIER_POINTER (orig_reduc_id),
				   "min") == 0
			   || strcmp (IDENTIFIER_POINTER (orig_reduc_id),
				      "max") == 0))))
	error_at (loc, "predeclared arithmetic type %qT in "
		       "%<#pragma omp declare reduction%>", type);
      else if (TREE_CODE (type) == FUNCTION_TYPE
	       || TREE_CODE (type) == METHOD_TYPE
	       || TREE_CODE (type) == ARRAY_TYPE)
	error_at (loc, "function or array type %qT in "
		       "%<#pragma omp declare reduction%>", type);
      else if (TREE_CODE (type) == REFERENCE_TYPE)
	error_at (loc, "reference type %qT in "
		       "%<#pragma omp declare reduction%>", type);
      else if (TYPE_QUALS_NO_ADDR_SPACE (type))
	error_at (loc, "const, volatile or __restrict qualified type %qT in "
		       "%<#pragma omp declare reduction%>", type);
      else
	types.safe_push (type);

      if (cp_lexer_next_token_is (parser->lexer, CPP_COMMA))
	cp_lexer_consume_token (parser->lexer);
      else
	break;
    }

  /* Restore the saved message.  */
  parser->type_definition_forbidden_message = saved_message;
  parser->colon_corrects_to_scope_p = saved_colon_corrects_to_scope_p;
  parser->colon_doesnt_start_class_def_p
    = saved_colon_doesnt_start_class_def_p;

  if (!cp_parser_require (parser, CPP_COLON, RT_COLON)
      || types.is_empty ())
    {
     fail:
      cp_parser_skip_to_pragma_eol (parser, pragma_tok);
      goto done;
    }

  first_token = cp_lexer_peek_token (parser->lexer);
  cp = NULL;
  errs = errorcount;
  FOR_EACH_VEC_ELT (types, i, type)
    {
      tree fntype
	= build_function_type_list (void_type_node,
				    cp_build_reference_type (type, false),
				    NULL_TREE);
      tree this_reduc_id = reduc_id;
      if (!dependent_type_p (type))
	this_reduc_id = omp_reduction_id (ERROR_MARK, reduc_id, type);
      tree fndecl = build_lang_decl (FUNCTION_DECL, this_reduc_id, fntype);
      DECL_SOURCE_LOCATION (fndecl) = pragma_tok->location;
      DECL_ARTIFICIAL (fndecl) = 1;
      DECL_EXTERNAL (fndecl) = 1;
      DECL_DECLARED_INLINE_P (fndecl) = 1;
      DECL_IGNORED_P (fndecl) = 1;
      DECL_OMP_DECLARE_REDUCTION_P (fndecl) = 1;
      DECL_ATTRIBUTES (fndecl)
	= tree_cons (get_identifier ("gnu_inline"), NULL_TREE,
		     DECL_ATTRIBUTES (fndecl));
      if (processing_template_decl)
	fndecl = push_template_decl (fndecl);
      bool block_scope = false;
      tree block = NULL_TREE;
      if (current_function_decl)
	{
	  block_scope = true;
	  DECL_CONTEXT (fndecl) = global_namespace;
	  if (!processing_template_decl)
	    pushdecl (fndecl);
	}
      else if (current_class_type)
	{
	  if (cp == NULL)
	    {
	      while (cp_lexer_next_token_is_not (parser->lexer, CPP_PRAGMA_EOL)
		     && cp_lexer_next_token_is_not (parser->lexer, CPP_EOF))
		cp_lexer_consume_token (parser->lexer);
	      if (cp_lexer_next_token_is_not (parser->lexer, CPP_PRAGMA_EOL))
		goto fail;
	      cp = cp_token_cache_new (first_token,
				       cp_lexer_peek_nth_token (parser->lexer,
								2));
	    }
	  DECL_STATIC_FUNCTION_P (fndecl) = 1;
	  finish_member_declaration (fndecl);
	  DECL_PENDING_INLINE_INFO (fndecl) = cp;
	  DECL_PENDING_INLINE_P (fndecl) = 1;
	  vec_safe_push (unparsed_funs_with_definitions, fndecl);
	  continue;
	}
      else
	{
	  DECL_CONTEXT (fndecl) = current_namespace;
	  pushdecl (fndecl);
	}
      if (!block_scope)
	start_preparsed_function (fndecl, NULL_TREE, SF_PRE_PARSED);
      else
	block = begin_omp_structured_block ();
      if (cp)
	{
	  cp_parser_push_lexer_for_tokens (parser, cp);
	  parser->lexer->in_pragma = true;
	}
      if (!cp_parser_omp_declare_reduction_exprs (fndecl, parser))
	{
	  if (!block_scope)
	    finish_function (0);
	  else
	    DECL_CONTEXT (fndecl) = current_function_decl;
	  if (cp)
	    cp_parser_pop_lexer (parser);
	  goto fail;
	}
      if (cp)
	cp_parser_pop_lexer (parser);
      if (!block_scope)
	finish_function (0);
      else
	{
	  DECL_CONTEXT (fndecl) = current_function_decl;
	  block = finish_omp_structured_block (block);
	  if (TREE_CODE (block) == BIND_EXPR)
	    DECL_SAVED_TREE (fndecl) = BIND_EXPR_BODY (block);
	  else if (TREE_CODE (block) == STATEMENT_LIST)
	    DECL_SAVED_TREE (fndecl) = block;
	  if (processing_template_decl)
	    add_decl_expr (fndecl);
	}
      cp_check_omp_declare_reduction (fndecl);
      if (cp == NULL && types.length () > 1)
	cp = cp_token_cache_new (first_token,
				 cp_lexer_peek_nth_token (parser->lexer, 2));
      if (errs != errorcount)
	break;
    }

  cp_parser_require_pragma_eol (parser, pragma_tok);

 done:
  /* Free any declarators allocated.  */
  obstack_free (&declarator_obstack, p);
}

/* OpenMP 4.0
   #pragma omp declare simd declare-simd-clauses[optseq] new-line
   #pragma omp declare reduction (reduction-id : typename-list : expression) \
      initializer-clause[opt] new-line
   #pragma omp declare target new-line  */

static void
cp_parser_omp_declare (cp_parser *parser, cp_token *pragma_tok,
		       enum pragma_context context)
{
  if (cp_lexer_next_token_is (parser->lexer, CPP_NAME))
    {
      tree id = cp_lexer_peek_token (parser->lexer)->u.value;
      const char *p = IDENTIFIER_POINTER (id);

      if (strcmp (p, "simd") == 0)
	{
	  cp_lexer_consume_token (parser->lexer);
	  cp_parser_omp_declare_simd (parser, pragma_tok,
				      context);
	  return;
	}
      cp_ensure_no_omp_declare_simd (parser);
      if (strcmp (p, "reduction") == 0)
	{
	  cp_lexer_consume_token (parser->lexer);
	  cp_parser_omp_declare_reduction (parser, pragma_tok,
					   context);
	  return;
	}
      if (!flag_openmp)  /* flag_openmp_simd  */
	{
	  cp_parser_require_pragma_eol (parser, pragma_tok);
	  return;
	}
      if (strcmp (p, "target") == 0)
	{
	  cp_lexer_consume_token (parser->lexer);
	  cp_parser_omp_declare_target (parser, pragma_tok);
	  return;
	}
    }
  cp_parser_error (parser, "expected %<simd%> or %<reduction%> "
			   "or %<target%>");
  cp_parser_require_pragma_eol (parser, pragma_tok);
}

/* Main entry point to OpenMP statement pragmas.  */

static void
cp_parser_omp_construct (cp_parser *parser, cp_token *pragma_tok)
{
  tree stmt;
  char p_name[sizeof "#pragma omp teams distribute parallel for simd"];
  omp_clause_mask mask (0);

  switch (pragma_tok->pragma_kind)
    {
    case PRAGMA_OMP_ATOMIC:
      cp_parser_omp_atomic (parser, pragma_tok);
      return;
    case PRAGMA_OMP_CRITICAL:
      stmt = cp_parser_omp_critical (parser, pragma_tok);
      break;
    case PRAGMA_OMP_DISTRIBUTE:
      strcpy (p_name, "#pragma omp");
      stmt = cp_parser_omp_distribute (parser, pragma_tok, p_name, mask, NULL);
      break;
    case PRAGMA_OMP_FOR:
      strcpy (p_name, "#pragma omp");
      stmt = cp_parser_omp_for (parser, pragma_tok, p_name, mask, NULL);
      break;
    case PRAGMA_OMP_MASTER:
      stmt = cp_parser_omp_master (parser, pragma_tok);
      break;
    case PRAGMA_OMP_ORDERED:
      stmt = cp_parser_omp_ordered (parser, pragma_tok);
      break;
    case PRAGMA_OMP_PARALLEL:
      strcpy (p_name, "#pragma omp");
      stmt = cp_parser_omp_parallel (parser, pragma_tok, p_name, mask, NULL);
      break;
    case PRAGMA_OMP_SECTIONS:
      strcpy (p_name, "#pragma omp");
      stmt = cp_parser_omp_sections (parser, pragma_tok, p_name, mask, NULL);
      break;
    case PRAGMA_OMP_SIMD:
      strcpy (p_name, "#pragma omp");
      stmt = cp_parser_omp_simd (parser, pragma_tok, p_name, mask, NULL);
      break;
    case PRAGMA_OMP_SINGLE:
      stmt = cp_parser_omp_single (parser, pragma_tok);
      break;
    case PRAGMA_OMP_TASK:
      stmt = cp_parser_omp_task (parser, pragma_tok);
      break;
    case PRAGMA_OMP_TASKGROUP:
      stmt = cp_parser_omp_taskgroup (parser, pragma_tok);
      break;
    case PRAGMA_OMP_TEAMS:
      strcpy (p_name, "#pragma omp");
      stmt = cp_parser_omp_teams (parser, pragma_tok, p_name, mask, NULL);
      break;
    default:
      gcc_unreachable ();
    }

  if (stmt)
    SET_EXPR_LOCATION (stmt, pragma_tok->location);
}

/* Transactional Memory parsing routines.  */

/* Parse a transaction attribute.

   txn-attribute:
	attribute
	[ [ identifier ] ]

   ??? Simplify this when C++0x bracket attributes are
   implemented properly.  */

static tree
cp_parser_txn_attribute_opt (cp_parser *parser)
{
  cp_token *token;
  tree attr_name, attr = NULL;

  if (cp_lexer_next_token_is_keyword (parser->lexer, RID_ATTRIBUTE))
    return cp_parser_attributes_opt (parser);

  if (cp_lexer_next_token_is_not (parser->lexer, CPP_OPEN_SQUARE))
    return NULL_TREE;
  cp_lexer_consume_token (parser->lexer);
  if (!cp_parser_require (parser, CPP_OPEN_SQUARE, RT_OPEN_SQUARE))
    goto error1;

  token = cp_lexer_peek_token (parser->lexer);
  if (token->type == CPP_NAME || token->type == CPP_KEYWORD)
    {
      token = cp_lexer_consume_token (parser->lexer);

      attr_name = (token->type == CPP_KEYWORD
		   /* For keywords, use the canonical spelling,
		      not the parsed identifier.  */
		   ? ridpointers[(int) token->keyword]
		   : token->u.value);
      attr = build_tree_list (attr_name, NULL_TREE);
    }
  else
    cp_parser_error (parser, "expected identifier");

  cp_parser_require (parser, CPP_CLOSE_SQUARE, RT_CLOSE_SQUARE);
 error1:
  cp_parser_require (parser, CPP_CLOSE_SQUARE, RT_CLOSE_SQUARE);
  return attr;
}

/* Parse a __transaction_atomic or __transaction_relaxed statement.

   transaction-statement:
     __transaction_atomic txn-attribute[opt] txn-noexcept-spec[opt]
       compound-statement
     __transaction_relaxed txn-noexcept-spec[opt] compound-statement
*/

static tree
cp_parser_transaction (cp_parser *parser, enum rid keyword)
{
  unsigned char old_in = parser->in_transaction;
  unsigned char this_in = 1, new_in;
  cp_token *token;
  tree stmt, attrs, noex;

  gcc_assert (keyword == RID_TRANSACTION_ATOMIC
      || keyword == RID_TRANSACTION_RELAXED);
  token = cp_parser_require_keyword (parser, keyword,
      (keyword == RID_TRANSACTION_ATOMIC ? RT_TRANSACTION_ATOMIC
	  : RT_TRANSACTION_RELAXED));
  gcc_assert (token != NULL);

  if (keyword == RID_TRANSACTION_RELAXED)
    this_in |= TM_STMT_ATTR_RELAXED;
  else
    {
      attrs = cp_parser_txn_attribute_opt (parser);
      if (attrs)
	this_in |= parse_tm_stmt_attr (attrs, TM_STMT_ATTR_OUTER);
    }

  /* Parse a noexcept specification.  */
  noex = cp_parser_noexcept_specification_opt (parser, true, NULL, true);

  /* Keep track if we're in the lexical scope of an outer transaction.  */
  new_in = this_in | (old_in & TM_STMT_ATTR_OUTER);

  stmt = begin_transaction_stmt (token->location, NULL, this_in);

  parser->in_transaction = new_in;
  cp_parser_compound_statement (parser, NULL, false, false);
  parser->in_transaction = old_in;

  finish_transaction_stmt (stmt, NULL, this_in, noex);

  return stmt;
}

/* Parse a __transaction_atomic or __transaction_relaxed expression.

   transaction-expression:
     __transaction_atomic txn-noexcept-spec[opt] ( expression )
     __transaction_relaxed txn-noexcept-spec[opt] ( expression )
*/

static tree
cp_parser_transaction_expression (cp_parser *parser, enum rid keyword)
{
  unsigned char old_in = parser->in_transaction;
  unsigned char this_in = 1;
  cp_token *token;
  tree expr, noex;
  bool noex_expr;

  gcc_assert (keyword == RID_TRANSACTION_ATOMIC
      || keyword == RID_TRANSACTION_RELAXED);

  if (!flag_tm)
    error (keyword == RID_TRANSACTION_RELAXED
	   ? G_("%<__transaction_relaxed%> without transactional memory "
		"support enabled")
	   : G_("%<__transaction_atomic%> without transactional memory "
		"support enabled"));

  token = cp_parser_require_keyword (parser, keyword,
      (keyword == RID_TRANSACTION_ATOMIC ? RT_TRANSACTION_ATOMIC
	  : RT_TRANSACTION_RELAXED));
  gcc_assert (token != NULL);

  if (keyword == RID_TRANSACTION_RELAXED)
    this_in |= TM_STMT_ATTR_RELAXED;

  /* Set this early.  This might mean that we allow transaction_cancel in
     an expression that we find out later actually has to be a constexpr.
     However, we expect that cxx_constant_value will be able to deal with
     this; also, if the noexcept has no constexpr, then what we parse next
     really is a transaction's body.  */
  parser->in_transaction = this_in;

  /* Parse a noexcept specification.  */
  noex = cp_parser_noexcept_specification_opt (parser, false, &noex_expr,
					       true);

  if (!noex || !noex_expr
      || cp_lexer_peek_token (parser->lexer)->type == CPP_OPEN_PAREN)
    {
      cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN);

      expr = cp_parser_expression (parser, /*cast_p=*/false, NULL);
      expr = finish_parenthesized_expr (expr);

      cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN);
    }
  else
    {
      /* The only expression that is available got parsed for the noexcept
         already.  noexcept is true then.  */
      expr = noex;
      noex = boolean_true_node;
    }

  expr = build_transaction_expr (token->location, expr, this_in, noex);
  parser->in_transaction = old_in;

  if (cp_parser_non_integral_constant_expression (parser, NIC_TRANSACTION))
    return error_mark_node;

  return (flag_tm ? expr : error_mark_node);
}

/* Parse a function-transaction-block.

   function-transaction-block:
     __transaction_atomic txn-attribute[opt] ctor-initializer[opt]
	 function-body
     __transaction_atomic txn-attribute[opt] function-try-block
     __transaction_relaxed ctor-initializer[opt] function-body
     __transaction_relaxed function-try-block
*/

static bool
cp_parser_function_transaction (cp_parser *parser, enum rid keyword)
{
  unsigned char old_in = parser->in_transaction;
  unsigned char new_in = 1;
  tree compound_stmt, stmt, attrs;
  bool ctor_initializer_p;
  cp_token *token;

  gcc_assert (keyword == RID_TRANSACTION_ATOMIC
      || keyword == RID_TRANSACTION_RELAXED);
  token = cp_parser_require_keyword (parser, keyword,
      (keyword == RID_TRANSACTION_ATOMIC ? RT_TRANSACTION_ATOMIC
	  : RT_TRANSACTION_RELAXED));
  gcc_assert (token != NULL);

  if (keyword == RID_TRANSACTION_RELAXED)
    new_in |= TM_STMT_ATTR_RELAXED;
  else
    {
      attrs = cp_parser_txn_attribute_opt (parser);
      if (attrs)
	new_in |= parse_tm_stmt_attr (attrs, TM_STMT_ATTR_OUTER);
    }

  stmt = begin_transaction_stmt (token->location, &compound_stmt, new_in);

  parser->in_transaction = new_in;

  if (cp_lexer_next_token_is_keyword (parser->lexer, RID_TRY))
    ctor_initializer_p = cp_parser_function_try_block (parser);
  else
    ctor_initializer_p = cp_parser_ctor_initializer_opt_and_function_body
      (parser, /*in_function_try_block=*/false);

  parser->in_transaction = old_in;

  finish_transaction_stmt (stmt, compound_stmt, new_in, NULL_TREE);

  return ctor_initializer_p;
}

/* Parse a __transaction_cancel statement.

   cancel-statement:
     __transaction_cancel txn-attribute[opt] ;
     __transaction_cancel txn-attribute[opt] throw-expression ;

   ??? Cancel and throw is not yet implemented.  */

static tree
cp_parser_transaction_cancel (cp_parser *parser)
{
  cp_token *token;
  bool is_outer = false;
  tree stmt, attrs;

  token = cp_parser_require_keyword (parser, RID_TRANSACTION_CANCEL,
				     RT_TRANSACTION_CANCEL);
  gcc_assert (token != NULL);

  attrs = cp_parser_txn_attribute_opt (parser);
  if (attrs)
    is_outer = (parse_tm_stmt_attr (attrs, TM_STMT_ATTR_OUTER) != 0);

  /* ??? Parse cancel-and-throw here.  */

  cp_parser_require (parser, CPP_SEMICOLON, RT_SEMICOLON);

  if (!flag_tm)
    {
      error_at (token->location, "%<__transaction_cancel%> without "
		"transactional memory support enabled");
      return error_mark_node;
    }
  else if (parser->in_transaction & TM_STMT_ATTR_RELAXED)
    {
      error_at (token->location, "%<__transaction_cancel%> within a "
		"%<__transaction_relaxed%>");
      return error_mark_node;
    }
  else if (is_outer)
    {
      if ((parser->in_transaction & TM_STMT_ATTR_OUTER) == 0
	  && !is_tm_may_cancel_outer (current_function_decl))
	{
	  error_at (token->location, "outer %<__transaction_cancel%> not "
		    "within outer %<__transaction_atomic%>");
	  error_at (token->location,
		    "  or a %<transaction_may_cancel_outer%> function");
	  return error_mark_node;
	}
    }
  else if (parser->in_transaction == 0)
    {
      error_at (token->location, "%<__transaction_cancel%> not within "
		"%<__transaction_atomic%>");
      return error_mark_node;
    }

  stmt = build_tm_abort_call (token->location, is_outer);
  add_stmt (stmt);

  return stmt;
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
  if (id != PRAGMA_OMP_DECLARE_REDUCTION)
    cp_ensure_no_omp_declare_simd (parser);
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

    case PRAGMA_OMP_TASKYIELD:
      switch (context)
	{
	case pragma_compound:
	  cp_parser_omp_taskyield (parser, pragma_tok);
	  return false;
	case pragma_stmt:
	  error_at (pragma_tok->location,
		    "%<#pragma omp taskyield%> may only be "
		    "used in compound statements");
	  break;
	default:
	  goto bad_stmt;
	}
      break;

    case PRAGMA_OMP_CANCEL:
      switch (context)
	{
	case pragma_compound:
	  cp_parser_omp_cancel (parser, pragma_tok);
	  return false;
	case pragma_stmt:
	  error_at (pragma_tok->location,
		    "%<#pragma omp cancel%> may only be "
		    "used in compound statements");
	  break;
	default:
	  goto bad_stmt;
	}
      break;

    case PRAGMA_OMP_CANCELLATION_POINT:
      switch (context)
	{
	case pragma_compound:
	  cp_parser_omp_cancellation_point (parser, pragma_tok);
	  return false;
	case pragma_stmt:
	  error_at (pragma_tok->location,
		    "%<#pragma omp cancellation point%> may only be "
		    "used in compound statements");
	  break;
	default:
	  goto bad_stmt;
	}
      break;

    case PRAGMA_OMP_THREADPRIVATE:
      cp_parser_omp_threadprivate (parser, pragma_tok);
      return false;

    case PRAGMA_OMP_DECLARE_REDUCTION:
      cp_parser_omp_declare (parser, pragma_tok, context);
      return false;

    case PRAGMA_OMP_ATOMIC:
    case PRAGMA_OMP_CRITICAL:
    case PRAGMA_OMP_DISTRIBUTE:
    case PRAGMA_OMP_FOR:
    case PRAGMA_OMP_MASTER:
    case PRAGMA_OMP_ORDERED:
    case PRAGMA_OMP_PARALLEL:
    case PRAGMA_OMP_SECTIONS:
    case PRAGMA_OMP_SIMD:
    case PRAGMA_OMP_SINGLE:
    case PRAGMA_OMP_TASK:
    case PRAGMA_OMP_TASKGROUP:
    case PRAGMA_OMP_TEAMS:
      if (context != pragma_stmt && context != pragma_compound)
	goto bad_stmt;
      cp_parser_omp_construct (parser, pragma_tok);
      return true;

    case PRAGMA_OMP_TARGET:
      return cp_parser_omp_target (parser, pragma_tok, context);

    case PRAGMA_OMP_END_DECLARE_TARGET:
      cp_parser_omp_end_declare_target (parser, pragma_tok);
      return false;

    case PRAGMA_OMP_SECTION:
      error_at (pragma_tok->location, 
		"%<#pragma omp section%> may only be used in "
		"%<#pragma omp sections%> construct");
      break;

    case PRAGMA_IVDEP:
      {
	cp_parser_skip_to_pragma_eol (parser, pragma_tok);
	cp_token *tok;
	tok = cp_lexer_peek_token (the_parser->lexer);
	if (tok->type != CPP_KEYWORD
	    || (tok->keyword != RID_FOR && tok->keyword != RID_WHILE
		&& tok->keyword != RID_DO))
	  {
	    cp_parser_error (parser, "for, while or do statement expected");
	    return false;
	  }
	cp_parser_iteration_statement (parser, true);
	return true;
      }

    case PRAGMA_CILK_SIMD:
      if (context == pragma_external)
	{
	  error_at (pragma_tok->location,
		    "%<#pragma simd%> must be inside a function");
	  break;
	}
      cp_parser_cilk_simd (parser, pragma_tok);
      return true;

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

/* Parses the Cilk Plus #pragma simd and SIMD-enabled function attribute's 
   vectorlength clause:
   Syntax:
   vectorlength ( constant-expression )  */

static tree
cp_parser_cilk_simd_vectorlength (cp_parser *parser, tree clauses,
				  bool is_simd_fn)
{
  location_t loc = cp_lexer_peek_token (parser->lexer)->location;
  tree expr;
  /* The vectorlength clause in #pragma simd behaves exactly like OpenMP's
     safelen clause.  Thus, vectorlength is represented as OMP 4.0
     safelen.  For SIMD-enabled function it is represented by OMP 4.0
     simdlen.  */
  if (!is_simd_fn)
    check_no_duplicate_clause (clauses, OMP_CLAUSE_SAFELEN, "vectorlength", 
			       loc);
  else
    check_no_duplicate_clause (clauses, OMP_CLAUSE_SIMDLEN, "vectorlength",
			       loc);

  if (!cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN))
    return error_mark_node;
  
  expr = cp_parser_constant_expression (parser, false, NULL);
  expr = maybe_constant_value (expr);
  
  /* If expr == error_mark_node, then don't emit any errors nor
     create a clause.  if any of the above functions returns
     error mark node then they would have emitted an error message.  */
  if (expr == error_mark_node)
    ;
  else if (!TREE_TYPE (expr)
	   || !TREE_CONSTANT (expr)
	   || !INTEGRAL_TYPE_P (TREE_TYPE (expr)))
    error_at (loc, "vectorlength must be an integer constant");
  else if (TREE_CONSTANT (expr)
	   && exact_log2 (TREE_INT_CST_LOW (expr)) == -1)
    error_at (loc, "vectorlength must be a power of 2");
  else 
    {
      tree c;
      if (!is_simd_fn)
	{ 
	  c = build_omp_clause (loc, OMP_CLAUSE_SAFELEN); 
	  OMP_CLAUSE_SAFELEN_EXPR (c) = expr; 
	  OMP_CLAUSE_CHAIN (c) = clauses; 
	  clauses = c;
	}
      else
	{
	  c = build_omp_clause (loc, OMP_CLAUSE_SIMDLEN);
	  OMP_CLAUSE_SIMDLEN_EXPR (c) = expr;
	  OMP_CLAUSE_CHAIN (c) = clauses;
	  clauses = c;
	}
    }

  if (!cp_parser_require (parser, CPP_CLOSE_PAREN, RT_CLOSE_PAREN))
    return error_mark_node;
  return clauses;
}

/* Handles the Cilk Plus #pragma simd linear clause.
   Syntax:
   linear ( simd-linear-variable-list )

   simd-linear-variable-list:
     simd-linear-variable
     simd-linear-variable-list , simd-linear-variable

   simd-linear-variable:
     id-expression
     id-expression : simd-linear-step

   simd-linear-step:
   conditional-expression */

static tree
cp_parser_cilk_simd_linear (cp_parser *parser, tree clauses)
{
  location_t loc = cp_lexer_peek_token (parser->lexer)->location;

  if (!cp_parser_require (parser, CPP_OPEN_PAREN, RT_OPEN_PAREN))
    return clauses;
  if (cp_lexer_next_token_is_not (parser->lexer, CPP_NAME))
    {
      cp_parser_error (parser, "expected identifier");
      cp_parser_skip_to_closing_parenthesis (parser, false, false, true);
      return error_mark_node;
    }

  bool saved_colon_corrects_to_scope_p = parser->colon_corrects_to_scope_p;
  parser->colon_corrects_to_scope_p = false;
  while (1)
    {
      cp_token *token = cp_lexer_peek_token (parser->lexer);
      if (cp_lexer_next_token_is_not (parser->lexer, CPP_NAME))
	{
	  cp_parser_error (parser, "expected variable-name");
	  clauses = error_mark_node;
	  break;
	}

      tree var_name = cp_parser_id_expression (parser, false, true, NULL,
					       false, false);
      tree decl = cp_parser_lookup_name_simple (parser, var_name,
						token->location);
      if (decl == error_mark_node)
	{
	  cp_parser_name_lookup_error (parser, var_name, decl, NLE_NULL,
				       token->location);
	  clauses = error_mark_node;
	}
      else
	{
	  tree e = NULL_TREE;
	  tree step_size = integer_one_node;

	  /* If present, parse the linear step.  Otherwise, assume the default
	     value of 1.  */
	  if (cp_lexer_peek_token (parser->lexer)->type == CPP_COLON)
	    {
	      cp_lexer_consume_token (parser->lexer);

	      e = cp_parser_assignment_expression (parser, false, NULL);
	      e = maybe_constant_value (e);

	      if (e == error_mark_node)
		{
		  /* If an error has occurred,  then the whole pragma is
		     considered ill-formed.  Thus, no reason to keep
		     parsing.  */
		  clauses = error_mark_node;
		  break;
		}
	      else if (type_dependent_expression_p (e)
		       || value_dependent_expression_p (e)
		       || (TREE_TYPE (e)
			   && INTEGRAL_TYPE_P (TREE_TYPE (e))
			   && (TREE_CONSTANT (e)
			       || DECL_P (e))))
		step_size = e;
	      else
		cp_parser_error (parser,
				 "step size must be an integer constant "
				 "expression or an integer variable");
	    }

	  /* Use the OMP_CLAUSE_LINEAR,  which has the same semantics.  */
	  tree l = build_omp_clause (loc, OMP_CLAUSE_LINEAR);
	  OMP_CLAUSE_DECL (l) = decl;
	  OMP_CLAUSE_LINEAR_STEP (l) = step_size;
	  OMP_CLAUSE_CHAIN (l) = clauses;
	  clauses = l;
	}
      if (cp_lexer_next_token_is (parser->lexer, CPP_COMMA))
	cp_lexer_consume_token (parser->lexer);
      else if (cp_lexer_next_token_is (parser->lexer, CPP_CLOSE_PAREN))
	break;
      else
	{
	  error_at (cp_lexer_peek_token (parser->lexer)->location,
		    "expected %<,%> or %<)%> after %qE", decl);
	  clauses = error_mark_node;
	  break;
	}
    }
  parser->colon_corrects_to_scope_p = saved_colon_corrects_to_scope_p;
  cp_parser_skip_to_closing_parenthesis (parser, false, false, true);
  return clauses;
}

/* Returns the name of the next clause.  If the clause is not
   recognized, then PRAGMA_CILK_CLAUSE_NONE is returned and the next
   token is not consumed.  Otherwise, the appropriate enum from the
   pragma_simd_clause is returned and the token is consumed.  */

static pragma_omp_clause
cp_parser_cilk_simd_clause_name (cp_parser *parser)
{
  pragma_omp_clause clause_type;
  cp_token *token = cp_lexer_peek_token (parser->lexer);

  if (token->keyword == RID_PRIVATE)
    clause_type = PRAGMA_CILK_CLAUSE_PRIVATE;
  else if (!token->u.value || token->type != CPP_NAME)
    return PRAGMA_CILK_CLAUSE_NONE;
  else if (!strcmp (IDENTIFIER_POINTER (token->u.value), "vectorlength"))
    clause_type = PRAGMA_CILK_CLAUSE_VECTORLENGTH;
  else if (!strcmp (IDENTIFIER_POINTER (token->u.value), "linear"))
    clause_type = PRAGMA_CILK_CLAUSE_LINEAR;
  else if (!strcmp (IDENTIFIER_POINTER (token->u.value), "firstprivate"))
    clause_type = PRAGMA_CILK_CLAUSE_FIRSTPRIVATE;
  else if (!strcmp (IDENTIFIER_POINTER (token->u.value), "lastprivate"))
    clause_type = PRAGMA_CILK_CLAUSE_LASTPRIVATE;
  else if (!strcmp (IDENTIFIER_POINTER (token->u.value), "reduction"))
    clause_type = PRAGMA_CILK_CLAUSE_REDUCTION;
  else
    return PRAGMA_CILK_CLAUSE_NONE;

  cp_lexer_consume_token (parser->lexer);
  return clause_type;
}

/* Parses all the #pragma simd clauses.  Returns a list of clauses found.  */

static tree
cp_parser_cilk_simd_all_clauses (cp_parser *parser, cp_token *pragma_token)
{
  tree clauses = NULL_TREE;

  while (cp_lexer_next_token_is_not (parser->lexer, CPP_PRAGMA_EOL)
	 && clauses != error_mark_node)
    {
      pragma_omp_clause c_kind;
      c_kind = cp_parser_cilk_simd_clause_name (parser);
      if (c_kind == PRAGMA_CILK_CLAUSE_VECTORLENGTH)
	clauses = cp_parser_cilk_simd_vectorlength (parser, clauses, false);
      else if (c_kind == PRAGMA_CILK_CLAUSE_LINEAR)
	clauses = cp_parser_cilk_simd_linear (parser, clauses);
      else if (c_kind == PRAGMA_CILK_CLAUSE_PRIVATE)
	/* Use the OpenMP 4.0 equivalent function.  */
	clauses = cp_parser_omp_var_list (parser, OMP_CLAUSE_PRIVATE, clauses);
      else if (c_kind == PRAGMA_CILK_CLAUSE_FIRSTPRIVATE)
	/* Use the OpenMP 4.0 equivalent function.  */
	clauses = cp_parser_omp_var_list (parser, OMP_CLAUSE_FIRSTPRIVATE,
					  clauses);
      else if (c_kind == PRAGMA_CILK_CLAUSE_LASTPRIVATE)
	/* Use the OMP 4.0 equivalent function.  */
	clauses = cp_parser_omp_var_list (parser, OMP_CLAUSE_LASTPRIVATE,
					  clauses);
      else if (c_kind == PRAGMA_CILK_CLAUSE_REDUCTION)
	/* Use the OMP 4.0 equivalent function.  */
	clauses = cp_parser_omp_clause_reduction (parser, clauses);
      else
	{
	  clauses = error_mark_node;
	  cp_parser_error (parser, "expected %<#pragma simd%> clause");
	  break;
	}
    }

  cp_parser_skip_to_pragma_eol (parser, pragma_token);

  if (clauses == error_mark_node)
    return error_mark_node;
  else
    return c_finish_cilk_clauses (clauses);
}

/* Main entry-point for parsing Cilk Plus <#pragma simd> for loops.  */

static void
cp_parser_cilk_simd (cp_parser *parser, cp_token *pragma_token)
{
  tree clauses = cp_parser_cilk_simd_all_clauses (parser, pragma_token);

  if (clauses == error_mark_node)
    return;
  
  if (cp_lexer_next_token_is_not_keyword (parser->lexer, RID_FOR))
    {
      error_at (cp_lexer_peek_token (parser->lexer)->location,
		"for statement expected");
      return;
    }

  tree sb = begin_omp_structured_block ();
  int save = cp_parser_begin_omp_structured_block (parser);
  tree ret = cp_parser_omp_for_loop (parser, CILK_SIMD, clauses, NULL);
  if (ret)
    cpp_validate_cilk_plus_loop (OMP_FOR_BODY (ret));
  cp_parser_end_omp_structured_block (parser, save);
  add_stmt (finish_omp_structured_block (sb));
  return;
}

/* Create an identifier for a generic parameter type (a synthesized
   template parameter implied by `auto' or a concept identifier). */

static GTY(()) int generic_parm_count;
static tree
make_generic_type_name ()
{
  char buf[32];
  sprintf (buf, "auto:%d", ++generic_parm_count);
  return get_identifier (buf);
}

/* Predicate that behaves as is_auto_or_concept but matches the parent
   node of the generic type rather than the generic type itself.  This
   allows for type transformation in add_implicit_template_parms.  */

static inline bool
tree_type_is_auto_or_concept (const_tree t)
{
  return TREE_TYPE (t) && is_auto_or_concept (TREE_TYPE (t));
}

/* Add an implicit template type parameter to the CURRENT_TEMPLATE_PARMS
   (creating a new template parameter list if necessary).  Returns the newly
   created template type parm.  */

tree
synthesize_implicit_template_parm  (cp_parser *parser)
{
  gcc_assert (current_binding_level->kind == sk_function_parms);

  /* We are either continuing a function template that already contains implicit
     template parameters, creating a new fully-implicit function template, or
     extending an existing explicit function template with implicit template
     parameters.  */

  cp_binding_level *const entry_scope = current_binding_level;

  bool become_template = false;
  cp_binding_level *parent_scope = 0;

  if (parser->implicit_template_scope)
    {
      gcc_assert (parser->implicit_template_parms);

      current_binding_level = parser->implicit_template_scope;
    }
  else
    {
      /* Roll back to the existing template parameter scope (in the case of
	 extending an explicit function template) or introduce a new template
	 parameter scope ahead of the function parameter scope (or class scope
	 in the case of out-of-line member definitions).  The function scope is
	 added back after template parameter synthesis below.  */

      cp_binding_level *scope = entry_scope;

      while (scope->kind == sk_function_parms)
	{
	  parent_scope = scope;
	  scope = scope->level_chain;
	}
      if (current_class_type && !LAMBDA_TYPE_P (current_class_type)
	  && parser->num_classes_being_defined == 0)
	while (scope->kind == sk_class)
	  {
	    parent_scope = scope;
	    scope = scope->level_chain;
	  }

      current_binding_level = scope;

      if (scope->kind != sk_template_parms
	  || !function_being_declared_is_template_p (parser))
	{
	  /* Introduce a new template parameter list for implicit template
	     parameters.  */

	  become_template = true;

	  parser->implicit_template_scope
	      = begin_scope (sk_template_parms, NULL);

	  ++processing_template_decl;

	  parser->fully_implicit_function_template_p = true;
	  ++parser->num_template_parameter_lists;
	}
      else
	{
	  /* Synthesize implicit template parameters at the end of the explicit
	     template parameter list.  */

	  gcc_assert (current_template_parms);

	  parser->implicit_template_scope = scope;

	  tree v = INNERMOST_TEMPLATE_PARMS (current_template_parms);
	  parser->implicit_template_parms
	    = TREE_VEC_ELT (v, TREE_VEC_LENGTH (v) - 1);
	}
    }

  /* Synthesize a new template parameter and track the current template
     parameter chain with implicit_template_parms.  */

  tree synth_id = make_generic_type_name ();
  tree synth_tmpl_parm = finish_template_type_parm (class_type_node,
						    synth_id);
  tree new_parm
    = process_template_parm (parser->implicit_template_parms,
			     input_location,
			     build_tree_list (NULL_TREE, synth_tmpl_parm),
			     /*non_type=*/false,
			     /*param_pack=*/false);


  if (parser->implicit_template_parms)
    parser->implicit_template_parms
      = TREE_CHAIN (parser->implicit_template_parms);
  else
    parser->implicit_template_parms = new_parm;

  tree new_type = TREE_TYPE (getdecls ());

  /* If creating a fully implicit function template, start the new implicit
     template parameter list with this synthesized type, otherwise grow the
     current template parameter list.  */

  if (become_template)
    {
      parent_scope->level_chain = current_binding_level;

      tree new_parms = make_tree_vec (1);
      TREE_VEC_ELT (new_parms, 0) = parser->implicit_template_parms;
      current_template_parms = tree_cons (size_int (processing_template_decl),
					  new_parms, current_template_parms);
    }
  else
    {
      tree& new_parms = INNERMOST_TEMPLATE_PARMS (current_template_parms);
      int new_parm_idx = TREE_VEC_LENGTH (new_parms);
      new_parms = grow_tree_vec (new_parms, new_parm_idx + 1);
      TREE_VEC_ELT (new_parms, new_parm_idx) = parser->implicit_template_parms;
    }

  current_binding_level = entry_scope;

  return new_type;
}

/* Finish the declaration of a fully implicit function template.  Such a
   template has no explicit template parameter list so has not been through the
   normal template head and tail processing.  synthesize_implicit_template_parm
   tries to do the head; this tries to do the tail.  MEMBER_DECL_OPT should be
   provided if the declaration is a class member such that its template
   declaration can be completed.  If MEMBER_DECL_OPT is provided the finished
   form is returned.  Otherwise NULL_TREE is returned. */

tree
finish_fully_implicit_template (cp_parser *parser, tree member_decl_opt)
{
  gcc_assert (parser->fully_implicit_function_template_p);

  if (member_decl_opt && member_decl_opt != error_mark_node
      && DECL_VIRTUAL_P (member_decl_opt))
    {
      error_at (DECL_SOURCE_LOCATION (member_decl_opt),
		"implicit templates may not be %<virtual%>");
      DECL_VIRTUAL_P (member_decl_opt) = false;
    }

  if (member_decl_opt)
    member_decl_opt = finish_member_template_decl (member_decl_opt);
  end_template_decl ();

  parser->fully_implicit_function_template_p = false;
  --parser->num_template_parameter_lists;

  return member_decl_opt;
}

#include "gt-cp-parser.h"
