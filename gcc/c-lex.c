/* Lexical analyzer for C and Objective C.
   Copyright (C) 1987, 88, 89, 92, 94-98, 1999 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"

#include "rtl.h"
#include "tree.h"
#include "input.h"
#include "output.h"
#include "c-lex.h"
#include "c-tree.h"
#include "flags.h"
#include "c-parse.h"
#include "c-pragma.h"
#include "toplev.h"
#include "intl.h"
#include "ggc.h"
#include "tm_p.h"

/* MULTIBYTE_CHARS support only works for native compilers.
   ??? Ideally what we want is to model widechar support after
   the current floating point support.  */
#ifdef CROSS_COMPILE
#undef MULTIBYTE_CHARS
#endif

#ifdef MULTIBYTE_CHARS
#include "mbchar.h"
#include <locale.h>
#endif /* MULTIBYTE_CHARS */
#ifndef GET_ENVIRONMENT
#define GET_ENVIRONMENT(ENV_VALUE,ENV_NAME) ((ENV_VALUE) = getenv (ENV_NAME))
#endif

#if USE_CPPLIB
#include "cpplib.h"
extern cpp_reader  parse_in;
extern cpp_options parse_options;
#else
/* Stream for reading from the input file.  */
FILE *finput;
#endif

extern void yyprint			PROTO((FILE *, int, YYSTYPE));

/* The elements of `ridpointers' are identifier nodes
   for the reserved type names and storage classes.
   It is indexed by a RID_... value.  */
tree ridpointers[(int) RID_MAX];

/* Cause the `yydebug' variable to be defined.  */
#define YYDEBUG 1

#if USE_CPPLIB
extern unsigned char *yy_cur, *yy_lim;
extern enum cpp_token cpp_token;

extern int yy_get_token ();

#define GETC() (yy_cur < yy_lim ? *yy_cur++ : yy_get_token ())
#define UNGETC(c) ((c) == EOF ? 0 : yy_cur--)

#else /* ! USE_CPPLIB */

#define GETC() getch ()
#define UNGETC(c) put_back (c)

struct putback_buffer {
  char *buffer;
  int   buffer_size;
  int   index;
};

static struct putback_buffer putback = {NULL, 0, -1};

static inline int getch PROTO ((void));

static inline int
getch ()
{
  if (putback.index != -1)
    {
      int ch = putback.buffer[putback.index];
      --putback.index;
      return ch;
    }
  return getc (finput);
}

static inline void put_back PROTO ((int));

static inline void
put_back (ch)
     int ch;
{
  if (ch != EOF)
    {
      if (putback.index == putback.buffer_size - 1)
	{
	  putback.buffer_size += 16;
	  putback.buffer = xrealloc (putback.buffer, putback.buffer_size);
	}
      putback.buffer[++putback.index] = ch;
    }
}
#endif /* ! USE_CPPLIB */

int linemode;

/* the declaration found for the last IDENTIFIER token read in.
   yylex must look this up to detect typedefs, which get token type TYPENAME,
   so it is left around in case the identifier is not a typedef but is
   used in a context which makes it a reference to a variable.  */
tree lastiddecl;

/* Nonzero enables objc features.  */

int doing_objc_thang;

extern int yydebug;

/* File used for outputting assembler code.  */
extern FILE *asm_out_file;

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE TYPE_PRECISION (wchar_type_node)

/* Number of bytes in a wide character.  */
#define WCHAR_BYTES (WCHAR_TYPE_SIZE / BITS_PER_UNIT)

static int maxtoken;		/* Current nominal length of token buffer.  */
char *token_buffer;	/* Pointer to token buffer.
			   Actual allocated length is maxtoken + 2.
			   This is not static because objc-parse.y uses it.  */

static int indent_level;        /* Number of { minus number of }. */

/* Nonzero tells yylex to ignore \ in string constants.  */
static int ignore_escape_flag;

/* Nonzero if end-of-file has been seen on input.  */
static int end_of_file;

#ifdef HANDLE_GENERIC_PRAGMAS
static int handle_generic_pragma	PROTO((int));
#endif /* HANDLE_GENERIC_PRAGMAS */
static int whitespace_cr		PROTO((int));
static int skip_white_space		PROTO((int));
static char *extend_token_buffer	PROTO((const char *));
static int readescape			PROTO((int *));
static void parse_float			PROTO((PTR));
static void extend_token_buffer_to	PROTO((int));
static int read_line_number		PROTO((int *));

/* Do not insert generated code into the source, instead, include it.
   This allows us to build gcc automatically even for targets that
   need to add or modify the reserved keyword lists.  */
#include "c-gperf.h"

/* Return something to represent absolute declarators containing a *.
   TARGET is the absolute declarator that the * contains.
   TYPE_QUALS is a list of modifiers such as const or volatile
   to apply to the pointer type, represented as identifiers.

   We return an INDIRECT_REF whose "contents" are TARGET
   and whose type is the modifier list.  */

tree
make_pointer_declarator (type_quals, target)
     tree type_quals, target;
{
  return build1 (INDIRECT_REF, type_quals, target);
}

void
forget_protocol_qualifiers ()
{
  int i, n = sizeof wordlist / sizeof (struct resword);

  for (i = 0; i < n; i++)
    if ((int) wordlist[i].rid >= (int) RID_IN
        && (int) wordlist[i].rid <= (int) RID_ONEWAY)
      wordlist[i].name = "";
}

void
remember_protocol_qualifiers ()
{
  int i, n = sizeof wordlist / sizeof (struct resword);

  for (i = 0; i < n; i++)
    if (wordlist[i].rid == RID_IN)
      wordlist[i].name = "in";
    else if (wordlist[i].rid == RID_OUT)
      wordlist[i].name = "out";
    else if (wordlist[i].rid == RID_INOUT)
      wordlist[i].name = "inout";
    else if (wordlist[i].rid == RID_BYCOPY)
      wordlist[i].name = "bycopy";
    else if (wordlist[i].rid == RID_BYREF)
      wordlist[i].name = "byref";
    else if (wordlist[i].rid == RID_ONEWAY)
      wordlist[i].name = "oneway";
}

char *
init_parse (filename)
     char *filename;
{
#if !USE_CPPLIB
  /* Open input file.  */
  if (filename == 0 || !strcmp (filename, "-"))
    {
      finput = stdin;
      filename = "stdin";
    }
  else
    finput = fopen (filename, "r");
  if (finput == 0)
    pfatal_with_name (filename);

#ifdef IO_BUFFER_SIZE
  setvbuf (finput, (char *) xmalloc (IO_BUFFER_SIZE), _IOFBF, IO_BUFFER_SIZE);
#endif
#else /* !USE_CPPLIB */
  parse_in.show_column = 1;
  if (! cpp_start_read (&parse_in, filename))
    abort ();

  if (filename == 0 || !strcmp (filename, "-"))
    filename = "stdin";

  /* cpp_start_read always puts at least one line directive into the
     token buffer.  We must arrange to read it out here. */
  yy_cur = parse_in.token_buffer;
  yy_lim = CPP_PWRITTEN (&parse_in);
  cpp_token = CPP_DIRECTIVE;
#endif

  init_lex ();
  init_pragma ();

  return filename;
}

void
finish_parse ()
{
#if USE_CPPLIB
  cpp_finish (&parse_in);
  errorcount += parse_in.errors;
#else
  fclose (finput);
#endif
}

void
init_lex ()
{
  /* Make identifier nodes long enough for the language-specific slots.  */
  set_identifier_size (sizeof (struct lang_identifier));

  /* Start it at 0, because check_newline is called at the very beginning
     and will increment it to 1.  */
  lineno = 0;

#ifdef MULTIBYTE_CHARS
  /* Change to the native locale for multibyte conversions.  */
  setlocale (LC_CTYPE, "");
  GET_ENVIRONMENT (literal_codeset, "LANG");
#endif

  maxtoken = 40;
  token_buffer = (char *) xmalloc (maxtoken + 2);

  ridpointers[(int) RID_INT] = get_identifier ("int");
  ridpointers[(int) RID_CHAR] = get_identifier ("char");
  ridpointers[(int) RID_VOID] = get_identifier ("void");
  ridpointers[(int) RID_FLOAT] = get_identifier ("float");
  ridpointers[(int) RID_DOUBLE] = get_identifier ("double");
  ridpointers[(int) RID_SHORT] = get_identifier ("short");
  ridpointers[(int) RID_LONG] = get_identifier ("long");
  ridpointers[(int) RID_UNSIGNED] = get_identifier ("unsigned");
  ridpointers[(int) RID_SIGNED] = get_identifier ("signed");
  ridpointers[(int) RID_INLINE] = get_identifier ("inline");
  ridpointers[(int) RID_CONST] = get_identifier ("const");
  ridpointers[(int) RID_RESTRICT] = get_identifier ("restrict");
  ridpointers[(int) RID_VOLATILE] = get_identifier ("volatile");
  ridpointers[(int) RID_AUTO] = get_identifier ("auto");
  ridpointers[(int) RID_STATIC] = get_identifier ("static");
  ridpointers[(int) RID_EXTERN] = get_identifier ("extern");
  ridpointers[(int) RID_TYPEDEF] = get_identifier ("typedef");
  ridpointers[(int) RID_REGISTER] = get_identifier ("register");
  ridpointers[(int) RID_ITERATOR] = get_identifier ("iterator");
  ridpointers[(int) RID_COMPLEX] = get_identifier ("complex");
  ridpointers[(int) RID_ID] = get_identifier ("id");
  ridpointers[(int) RID_IN] = get_identifier ("in");
  ridpointers[(int) RID_OUT] = get_identifier ("out");
  ridpointers[(int) RID_INOUT] = get_identifier ("inout");
  ridpointers[(int) RID_BYCOPY] = get_identifier ("bycopy");
  ridpointers[(int) RID_BYREF] = get_identifier ("byref");
  ridpointers[(int) RID_ONEWAY] = get_identifier ("oneway");
  forget_protocol_qualifiers();

  /* Some options inhibit certain reserved words.
     Clear those words out of the hash table so they won't be recognized.  */
#define UNSET_RESERVED_WORD(STRING) \
  do { struct resword *s = is_reserved_word (STRING, sizeof (STRING) - 1); \
       if (s) s->name = ""; } while (0)

  if (! doing_objc_thang)
    UNSET_RESERVED_WORD ("id");

  if (flag_traditional)
    {
      UNSET_RESERVED_WORD ("const");
      UNSET_RESERVED_WORD ("restrict");
      UNSET_RESERVED_WORD ("volatile");
      UNSET_RESERVED_WORD ("typeof");
      UNSET_RESERVED_WORD ("signed");
      UNSET_RESERVED_WORD ("inline");
      UNSET_RESERVED_WORD ("iterator");
      UNSET_RESERVED_WORD ("complex");
    }
  else if (!flag_isoc9x)
    UNSET_RESERVED_WORD ("restrict");

  if (flag_no_asm)
    {
      UNSET_RESERVED_WORD ("asm");
      UNSET_RESERVED_WORD ("typeof");
      UNSET_RESERVED_WORD ("inline");
      UNSET_RESERVED_WORD ("iterator");
      UNSET_RESERVED_WORD ("complex");
    }
}

void
reinit_parse_for_function ()
{
}

/* Function used when yydebug is set, to print a token in more detail.  */

void
yyprint (file, yychar, yylval)
     FILE *file;
     int yychar;
     YYSTYPE yylval;
{
  tree t;
  switch (yychar)
    {
    case IDENTIFIER:
    case TYPENAME:
    case OBJECTNAME:
      t = yylval.ttype;
      if (IDENTIFIER_POINTER (t))
	fprintf (file, " `%s'", IDENTIFIER_POINTER (t));
      break;

    case CONSTANT:
      t = yylval.ttype;
      if (TREE_CODE (t) == INTEGER_CST)
	fprintf (file,
#if HOST_BITS_PER_WIDE_INT == 64
#if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_INT
		 " 0x%x%016x",
#else
#if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_LONG
		 " 0x%lx%016lx",
#else
		 " 0x%llx%016llx",
#endif
#endif
#else
#if HOST_BITS_PER_WIDE_INT != HOST_BITS_PER_INT
		 " 0x%lx%08lx",
#else
		 " 0x%x%08x",
#endif
#endif
		 TREE_INT_CST_HIGH (t), TREE_INT_CST_LOW (t));
      break;
    }
}

/* Iff C is a carriage return, warn about it - if appropriate -
   and return nonzero.  */
static int
whitespace_cr (c)
     int c;
{
  static int newline_warning = 0;

  if (c == '\r')
    {
      /* ANSI C says the effects of a carriage return in a source file
	 are undefined.  */
      if (pedantic && !newline_warning)
	{
	  warning ("carriage return in source file");
	  warning ("(we only warn about the first carriage return)");
	  newline_warning = 1;
	}
      return 1;
    }
  return 0;
}

/* If C is not whitespace, return C.
   Otherwise skip whitespace and return first nonwhite char read.  */

static int
skip_white_space (c)
     register int c;
{
  for (;;)
    {
      switch (c)
	{
	  /* We don't recognize comments here, because
	     cpp output can include / and * consecutively as operators.
	     Also, there's no need, since cpp removes all comments.  */

	case '\n':
	  if (linemode)
	    {
	      UNGETC (c);
	      return EOF;
	    }
	  c = check_newline ();
	  break;

	case ' ':
	case '\t':
	case '\f':
	case '\v':
	case '\b':
#if USE_CPPLIB
	  /* While processing a # directive we don't get CPP_HSPACE
	     tokens, so we also need to handle whitespace the normal way.  */
	  if (cpp_token == CPP_HSPACE)
	    c = yy_get_token ();
	  else
#endif
	    c = GETC();
	  break;

	case '\r':
	  whitespace_cr (c);
	  c = GETC();
	  break;

	case '\\':
	  c = GETC();
	  if (c == '\n')
	    lineno++;
	  else
	    error ("stray '\\' in program");
	  c = GETC();
	  break;

	default:
	  return (c);
	}
    }
}

/* Skips all of the white space at the current location in the input file.  */

void
position_after_white_space ()
{
  register int c;

  c = GETC();

  UNGETC (skip_white_space (c));
}

/* Make the token buffer longer, preserving the data in it.
   P should point to just beyond the last valid character in the old buffer.
   The value we return is a pointer to the new buffer
   at a place corresponding to P.  */

static void
extend_token_buffer_to (size)
     int size;
{
  do
    maxtoken = maxtoken * 2 + 10;
  while (maxtoken < size);
  token_buffer = (char *) xrealloc (token_buffer, maxtoken + 2);
}

static char *
extend_token_buffer (p)
     const char *p;
{
  int offset = p - token_buffer;
  extend_token_buffer_to (offset);
  return token_buffer + offset;
}

#if defined HANDLE_PRAGMA
/* Local versions of these macros, that can be passed as function pointers.  */
static int
pragma_getc ()
{
  return GETC ();
}

static void
pragma_ungetc (arg)
     int arg;
{
  UNGETC (arg);
}
#endif

static int
read_line_number (num)
     int *num;
{
  register int token = yylex ();

  if (token == CONSTANT
      && TREE_CODE (yylval.ttype) == INTEGER_CST)
    {
      *num = TREE_INT_CST_LOW (yylval.ttype);
      return 1;
    }
  else
    {
      if (token != END_OF_LINE)
	error ("invalid #-line");
      return 0;
    }
}
  
/* At the beginning of a line, increment the line number
   and process any #-directive on this line.
   If the line is a #-directive, read the entire line and return a newline.
   Otherwise, return the line's first non-whitespace character.

   Note that in the case of USE_CPPLIB, we get the whole line as one
   CPP_DIRECTIVE token.  */

int
check_newline ()
{
  register int c;
  register int token;
  int saw_line;
  enum { act_none, act_push, act_pop } action;
  int old_lineno, action_number, l;

 restart:
  /* Read first nonwhite char on the line.  */

#ifdef USE_CPPLIB
  c = GETC ();
  /* In some cases where we're leaving an include file, we can get multiple
     CPP_HSPACE tokens in a row, so we need to loop.  */
  while (cpp_token == CPP_HSPACE)
    c = yy_get_token ();
#else
  do
    c = GETC ();
  while (c == ' ' || c == '\t');
#endif

  lineno++;

  if (c != '#')
    {
      /* Sequences of multiple newlines are very common; optimize them.  */
      if (c == '\n')
	goto restart;

      /* If not #, return it so caller will use it.  */
      return c;
    }

  /* Don't read beyond this line.  */
  saw_line = 0;
  linemode = 1;
  
#if USE_CPPLIB
  if (cpp_token == CPP_VSPACE)
    {
      /* Format is "<space> <line number> <filename> <newline>".
	 Only the line number is interesting, and even that
	 we can get more efficiently than scanning the line.  */
      yy_cur = yy_lim - 1;
      lineno = parse_in.lineno - 1;
      goto skipline;
    }
#endif

  token = yylex ();

  if (token == IDENTIFIER)
    {
      /* If a letter follows, then if the word here is `line', skip
	 it and ignore it; otherwise, ignore the line, with an error
	 if the word isn't `pragma'.  */

      const char *name = IDENTIFIER_POINTER (yylval.ttype);

      if (!strcmp (name, "pragma"))
	{
	  token = yylex ();
	  if (token != IDENTIFIER
	      || TREE_CODE (yylval.ttype) != IDENTIFIER_NODE)
	    goto skipline;

#ifdef HANDLE_PRAGMA
	  /* We invoke HANDLE_PRAGMA before HANDLE_GENERIC_PRAGMAS
	     (if both are defined), in order to give the back
	     end a chance to override the interpretation of
	     SYSV style pragmas.  */
	  if (HANDLE_PRAGMA (pragma_getc, pragma_ungetc,
			     IDENTIFIER_POINTER (yylval.ttype)))
	    goto skipline;
#endif /* HANDLE_PRAGMA */
	      
#ifdef HANDLE_GENERIC_PRAGMAS
	  if (handle_generic_pragma (token))
	    goto skipline;
#endif /* HANDLE_GENERIC_PRAGMAS */

	  /* Issue a warning message if we have been asked to do so.
	     Ignoring unknown pragmas in system header file unless
	     an explcit -Wunknown-pragmas has been given. */
	  if (warn_unknown_pragmas > 1
	      || (warn_unknown_pragmas && ! in_system_header))
	    warning ("ignoring pragma: %s", token_buffer);

	  goto skipline;
	}
      else if (!strcmp (name, "define"))
	{
	  debug_define (lineno, GET_DIRECTIVE_LINE ());
	  goto skipline;
	}
      else if (!strcmp (name, "undef"))
	{
	  debug_undef (lineno, GET_DIRECTIVE_LINE ());
	  goto skipline;
	}
      else if (!strcmp (name, "line"))
	{
	  saw_line = 1;
	  token = yylex ();
	  goto linenum;
	}
      else if (!strcmp (name, "ident"))
	{
	  /* #ident.  The pedantic warning is now in cccp.c.  */

	  /* Here we have just seen `#ident '.
	     A string constant should follow.  */

	  token = yylex ();
	  if (token == END_OF_LINE)
	    goto skipline;
	  if (token != STRING
	      || TREE_CODE (yylval.ttype) != STRING_CST)
	    {
	      error ("invalid #ident");
	      goto skipline;
	    }

	  if (! flag_no_ident)
	    {
#ifdef ASM_OUTPUT_IDENT
	      ASM_OUTPUT_IDENT (asm_out_file,
				TREE_STRING_POINTER (yylval.ttype));
#endif
	    }

	  /* Skip the rest of this line.  */
	  goto skipline;
	}

      error ("undefined or invalid # directive `%s'", name);
      goto skipline;
    }

  /* If the # is the only nonwhite char on the line,
     just ignore it.  Check the new newline.  */
  if (token == END_OF_LINE)
    goto skipline;

linenum:
  /* Here we have either `#line' or `# <nonletter>'.
     In either case, it should be a line number; a digit should follow.  */

  if (token != CONSTANT
      || TREE_CODE (yylval.ttype) != INTEGER_CST)
    {
      error ("invalid #-line");
      goto skipline;
    }

  /* subtract one, because it is the following line that
     gets the specified number */

  l = TREE_INT_CST_LOW (yylval.ttype) - 1;

  /* More follows: it must be a string constant (filename).
     It would be neat to use cpplib to quickly process the string, but
     (1) we don't have a handy tokenization of the string, and
     (2) I don't know how well that would work in the presense
     of filenames that contain wide characters.  */

  if (saw_line)
    {
      /* Don't treat \ as special if we are processing #line 1 "...".
	 If you want it to be treated specially, use # 1 "...".  */
      ignore_escape_flag = 1;
    }

  /* Read the string constant.  */
  token = yylex ();

  ignore_escape_flag = 0;

  if (token == END_OF_LINE)
    {
      /* No more: store the line number and check following line.  */
      lineno = l;
      goto skipline;
    }

  if (token != STRING || TREE_CODE (yylval.ttype) != STRING_CST)
    {
      error ("invalid #line");
      goto skipline;
    }

  if (! ggc_p && !TREE_PERMANENT (yylval.ttype))
    {
      input_filename
	= (char *) permalloc (TREE_STRING_LENGTH (yylval.ttype) + 1);
      strcpy (input_filename, TREE_STRING_POINTER (yylval.ttype));
    }
  else
    input_filename = TREE_STRING_POINTER (yylval.ttype);

  if (main_input_filename == 0)
    main_input_filename = input_filename;

  old_lineno = lineno;
  action = act_none;
  action_number = 0;
  lineno = l;

  /* Each change of file name
     reinitializes whether we are now in a system header.  */
  in_system_header = 0;

  if (!read_line_number (&action_number))
    {
      /* Update the name in the top element of input_file_stack.  */
      if (input_file_stack)
	input_file_stack->name = input_filename;
    }

  /* `1' after file name means entering new file.
     `2' after file name means just left a file.  */

  if (action_number == 1)
    {
      action = act_push;
      read_line_number (&action_number);
    }
  else if (action_number == 2)
    {
      action = act_pop;
      read_line_number (&action_number);
    }
  if (action_number == 3)
    {
      /* `3' after file name means this is a system header file.  */
      in_system_header = 1;
      read_line_number (&action_number);
    }

  /* Do the actions implied by the preceding numbers.  */

  if (action == act_push)
    {
      /* Pushing to a new file.  */
      struct file_stack *p
	= (struct file_stack *) xmalloc (sizeof (struct file_stack));
      input_file_stack->line = old_lineno;
      p->next = input_file_stack;
      p->name = input_filename;
      p->indent_level = indent_level;
      input_file_stack = p;
      input_file_stack_tick++;
      debug_start_source_file (input_filename);
    }
  else if (action == act_pop)
    {
      /* Popping out of a file.  */
      if (input_file_stack->next)
	{
	  struct file_stack *p = input_file_stack;
	  if (indent_level != p->indent_level)
	    {
	      warning_with_file_and_line
		(p->name, old_lineno,
		 "This file contains more `%c's than `%c's.",
		 indent_level > p->indent_level ? '{' : '}',
		 indent_level > p->indent_level ? '}' : '{');
	    }
	  input_file_stack = p->next;
	  free (p);
	  input_file_stack_tick++;
	  debug_end_source_file (input_file_stack->line);
	}
      else
	error ("#-lines for entering and leaving files don't match");
    }

  /* Now that we've pushed or popped the input stack,
     update the name in the top element.  */
  if (input_file_stack)
    input_file_stack->name = input_filename;

  /* skip the rest of this line.  */
 skipline:
  linemode = 0;
  end_of_file = 0;

  do
    c = GETC();
  while (c != '\n' && c != EOF);
  return c;
}

#ifdef HANDLE_GENERIC_PRAGMAS

/* Handle a #pragma directive.
   TOKEN is the token we read after `#pragma'.  Processes the entire input
   line and return non-zero iff the pragma has been successfully parsed.  */

/* This function has to be in this file, in order to get at
   the token types.  */

static int
handle_generic_pragma (token)
     register int token;
{
  for (;;)
    {
      switch (token)
	{
	case IDENTIFIER:
	case TYPENAME:
	case STRING:
	case CONSTANT:
	  handle_pragma_token (token_buffer, yylval.ttype);
	  break;

	case END_OF_LINE:
	  return handle_pragma_token (NULL_PTR, NULL_TREE);

	default:
	  handle_pragma_token (token_buffer, NULL);
	}

      token = yylex ();
    }
}

#endif /* HANDLE_GENERIC_PRAGMAS */

#define ENDFILE -1  /* token that represents end-of-file */

/* Read an escape sequence, returning its equivalent as a character,
   or store 1 in *ignore_ptr if it is backslash-newline.  */

static int
readescape (ignore_ptr)
     int *ignore_ptr;
{
  register int c = GETC();
  register int code;
  register unsigned count;
  unsigned firstdig = 0;
  int nonnull;

  switch (c)
    {
    case 'x':
      if (warn_traditional)
	warning ("the meaning of `\\x' varies with -traditional");

      if (flag_traditional)
	return c;

      code = 0;
      count = 0;
      nonnull = 0;
      while (1)
	{
	  c = GETC();
	  if (! ISXDIGIT (c))
	    {
	      UNGETC (c);
	      break;
	    }
	  code *= 16;
	  if (c >= 'a' && c <= 'f')
	    code += c - 'a' + 10;
	  if (c >= 'A' && c <= 'F')
	    code += c - 'A' + 10;
	  if (c >= '0' && c <= '9')
	    code += c - '0';
	  if (code != 0 || count != 0)
	    {
	      if (count == 0)
		firstdig = code;
	      count++;
	    }
	  nonnull = 1;
	}
      if (! nonnull)
	error ("\\x used with no following hex digits");
      else if (count == 0)
	/* Digits are all 0's.  Ok.  */
	;
      else if ((count - 1) * 4 >= TYPE_PRECISION (integer_type_node)
	       || (count > 1
		   && (((unsigned)1
			<< (TYPE_PRECISION (integer_type_node)
			    - (count - 1) * 4))
		       <= firstdig)))
	pedwarn ("hex escape out of range");
      return code;

    case '0':  case '1':  case '2':  case '3':  case '4':
    case '5':  case '6':  case '7':
      code = 0;
      count = 0;
      while ((c <= '7') && (c >= '0') && (count++ < 3))
	{
	  code = (code * 8) + (c - '0');
	  c = GETC();
	}
      UNGETC (c);
      return code;

    case '\\': case '\'': case '"':
      return c;

    case '\n':
      lineno++;
      *ignore_ptr = 1;
      return 0;

    case 'n':
      return TARGET_NEWLINE;

    case 't':
      return TARGET_TAB;

    case 'r':
      return TARGET_CR;

    case 'f':
      return TARGET_FF;

    case 'b':
      return TARGET_BS;

    case 'a':
      if (warn_traditional)
	warning ("the meaning of `\\a' varies with -traditional");

      if (flag_traditional)
	return c;
      return TARGET_BELL;

    case 'v':
#if 0 /* Vertical tab is present in common usage compilers.  */
      if (flag_traditional)
	return c;
#endif
      return TARGET_VT;

    case 'e':
    case 'E':
      if (pedantic)
	pedwarn ("non-ANSI-standard escape sequence, `\\%c'", c);
      return TARGET_ESC;

    case '?':
      return c;

      /* `\(', etc, are used at beginning of line to avoid confusing Emacs.  */
    case '(':
    case '{':
    case '[':
      /* `\%' is used to prevent SCCS from getting confused.  */
    case '%':
      if (pedantic)
	pedwarn ("unknown escape sequence `\\%c'", c);
      return c;
    }
  if (ISGRAPH (c))
    pedwarn ("unknown escape sequence `\\%c'", c);
  else
    pedwarn ("unknown escape sequence: `\\' followed by char code 0x%x", c);
  return c;
}

void
yyerror (msgid)
     const char *msgid;
{
  const char *string = _(msgid);

  /* We can't print string and character constants well
     because the token_buffer contains the result of processing escapes.  */
  if (end_of_file)
    error ("%s at end of input", string);
  else if (token_buffer[0] == 0)
    error ("%s at null character", string);
  else if (token_buffer[0] == '"')
    error ("%s before string constant", string);
  else if (token_buffer[0] == '\'')
    error ("%s before character constant", string);
  else if (!ISGRAPH(token_buffer[0]))
    error ("%s before character 0%o", string, (unsigned char) token_buffer[0]);
  else
    error ("%s before `%s'", string, token_buffer);
}

#if 0

struct try_type
{
  tree *node_var;
  char unsigned_flag;
  char long_flag;
  char long_long_flag;
};

struct try_type type_sequence[] =
{
  { &integer_type_node, 0, 0, 0},
  { &unsigned_type_node, 1, 0, 0},
  { &long_integer_type_node, 0, 1, 0},
  { &long_unsigned_type_node, 1, 1, 0},
  { &long_long_integer_type_node, 0, 1, 1},
  { &long_long_unsigned_type_node, 1, 1, 1}
};
#endif /* 0 */

struct pf_args
{
  /* Input */
  int base;
  char * p;
  /* I/O */
  int c;
  /* Output */
  int imag;
  tree type;
  int conversion_errno;
  REAL_VALUE_TYPE value;
};
 
static void
parse_float (data)
  PTR data;
{
  struct pf_args * args = (struct pf_args *) data;
  int fflag = 0, lflag = 0;
  /* Copy token_buffer now, while it has just the number
     and not the suffixes; once we add `f' or `i',
     REAL_VALUE_ATOF may not work any more.  */
  char *copy = (char *) alloca (args->p - token_buffer + 1);
  bcopy (token_buffer, copy, args->p - token_buffer + 1);
  args->imag = 0;
  args->conversion_errno = 0;
  args->type = double_type_node;

  while (1)
    {
      int lose = 0;

      /* Read the suffixes to choose a data type.  */
      switch (args->c)
	{
	case 'f': case 'F':
	  if (fflag)
	    error ("more than one `f' in numeric constant");
	  fflag = 1;
	  break;

	case 'l': case 'L':
	  if (lflag)
	    error ("more than one `l' in numeric constant");
	  lflag = 1;
	  break;

	case 'i': case 'I':
	  if (args->imag)
	    error ("more than one `i' or `j' in numeric constant");
	  else if (pedantic)
	    pedwarn ("ANSI C forbids imaginary numeric constants");
	  args->imag = 1;
	  break;

	default:
	  lose = 1;
	}

      if (lose)
	break;

      if (args->p >= token_buffer + maxtoken - 3)
	args->p = extend_token_buffer (args->p);
      *(args->p++) = args->c;
      *(args->p) = 0;
      args->c = GETC();
    }

  /* The second argument, machine_mode, of REAL_VALUE_ATOF
     tells the desired precision of the binary result
     of decimal-to-binary conversion.  */

  if (fflag)
    {
      if (lflag)
	error ("both `f' and `l' in floating constant");

      args->type = float_type_node;
      errno = 0;
      if (args->base == 16)
	args->value = REAL_VALUE_HTOF (copy, TYPE_MODE (args->type));
      else
	args->value = REAL_VALUE_ATOF (copy, TYPE_MODE (args->type));
      args->conversion_errno = errno;
      /* A diagnostic is required here by some ANSI C testsuites.
	 This is not pedwarn, because some people don't want
	 an error for this.  */
      if (REAL_VALUE_ISINF (args->value) && pedantic)
	warning ("floating point number exceeds range of `float'");
    }
  else if (lflag)
    {
      args->type = long_double_type_node;
      errno = 0;
      if (args->base == 16)
	args->value = REAL_VALUE_HTOF (copy, TYPE_MODE (args->type));
      else
	args->value = REAL_VALUE_ATOF (copy, TYPE_MODE (args->type));
      args->conversion_errno = errno;
      if (REAL_VALUE_ISINF (args->value) && pedantic)
	warning ("floating point number exceeds range of `long double'");
    }
  else
    {
      errno = 0;
      if (args->base == 16)
	args->value = REAL_VALUE_HTOF (copy, TYPE_MODE (args->type));
      else
	args->value = REAL_VALUE_ATOF (copy, TYPE_MODE (args->type));
      args->conversion_errno = errno;
      if (REAL_VALUE_ISINF (args->value) && pedantic)
	warning ("floating point number exceeds range of `double'");
    }
}
 
/* Get the next character, staying within the current token if possible.
   If we're lexing a token, we don't want to look beyond the end of the
   token cpplib has prepared for us; otherwise, we end up reading in the
   next token, which screws up feed_input.  So just return a null
   character.  */

static inline int token_getch PROTO ((void));

static inline int
token_getch ()
{
#if USE_CPPLIB
  if (yy_cur == yy_lim)
    return '\0';
#endif
  return GETC ();
}

static inline void token_put_back PROTO ((int));

static inline void
token_put_back (ch)
     int ch;
{
#if USE_CPPLIB
  if (ch == '\0')
    return;
#endif
  UNGETC (ch);
}

/* Read a single token from the input stream, and assign it lexical
   semantics.  */

int
yylex ()
{
  register int c;
  register char *p;
  register int value;
  int wide_flag = 0;
  int objc_flag = 0;

  c = GETC();

  /* Effectively do c = skip_white_space (c)
     but do it faster in the usual cases.  */
  while (1)
    switch (c)
      {
      case ' ':
      case '\t':
      case '\f':
      case '\v':
      case '\b':
#if USE_CPPLIB
	if (cpp_token == CPP_HSPACE)
	  c = yy_get_token ();
	else
#endif
	  c = GETC();
	break;

      case '\r':
	/* Call skip_white_space so we can warn if appropriate.  */

      case '\n':
      case '/':
      case '\\':
	c = skip_white_space (c);
      default:
	goto found_nonwhite;
      }
 found_nonwhite:

  token_buffer[0] = c;
  token_buffer[1] = 0;

/*  yylloc.first_line = lineno; */

  switch (c)
    {
    case EOF:
      end_of_file = 1;
      token_buffer[0] = 0;
      if (linemode)
	value = END_OF_LINE;
      else
	value = ENDFILE;
      break;

    case 'L':
#if USE_CPPLIB
      if (cpp_token == CPP_NAME)
	goto letter;
#endif
      /* Capital L may start a wide-string or wide-character constant.  */
      {
	register int c = token_getch();
	if (c == '\'')
	  {
	    wide_flag = 1;
	    goto char_constant;
	  }
	if (c == '"')
	  {
	    wide_flag = 1;
	    goto string_constant;
	  }
	token_put_back (c);
      }
      goto letter;

    case '@':
      if (!doing_objc_thang)
	{
	  value = c;
	  break;
	}
      else
	{
	  /* '@' may start a constant string object.  */
	  register int c = token_getch ();
	  if (c == '"')
	    {
	      objc_flag = 1;
	      goto string_constant;
	    }
	  token_put_back (c);
	  /* Fall through to treat '@' as the start of an identifier.  */
	}

    case 'A':  case 'B':  case 'C':  case 'D':  case 'E':
    case 'F':  case 'G':  case 'H':  case 'I':  case 'J':
    case 'K':		  case 'M':  case 'N':  case 'O':
    case 'P':  case 'Q':  case 'R':  case 'S':  case 'T':
    case 'U':  case 'V':  case 'W':  case 'X':  case 'Y':
    case 'Z':
    case 'a':  case 'b':  case 'c':  case 'd':  case 'e':
    case 'f':  case 'g':  case 'h':  case 'i':  case 'j':
    case 'k':  case 'l':  case 'm':  case 'n':  case 'o':
    case 'p':  case 'q':  case 'r':  case 's':  case 't':
    case 'u':  case 'v':  case 'w':  case 'x':  case 'y':
    case 'z':
    case '_':
    case '$':
    letter:
#if USE_CPPLIB
      if (cpp_token == CPP_NAME)
	{
	  /* Note that one character has already been read from
	     yy_cur into token_buffer.  Also, cpplib complains about
	     $ in identifiers, so we don't have to.  */

	  int len = yy_lim - yy_cur + 1;
	  if (len >= maxtoken)
	    extend_token_buffer_to (len + 1);
	  memcpy (token_buffer + 1, yy_cur, len);
	  p = token_buffer + len;
	  yy_cur = yy_lim;
	}
      else
#endif
	{
	  p = token_buffer;
	  while (ISALNUM (c) || c == '_' || c == '$' || c == '@')
	    {
	      /* Make sure this char really belongs in an identifier.  */
	      if (c == '$')
		{
		  if (! dollars_in_ident)
		    error ("`$' in identifier");
		  else if (pedantic)
		    pedwarn ("`$' in identifier");
		}

	      if (p >= token_buffer + maxtoken)
		p = extend_token_buffer (p);

	      *p++ = c;
	      c = token_getch();
	    }

	  *p = 0;
	  token_put_back (c);
	}

      value = IDENTIFIER;
      yylval.itype = 0;

      /* Try to recognize a keyword.  Uses minimum-perfect hash function */

      {
	register struct resword *ptr;

	if ((ptr = is_reserved_word (token_buffer, p - token_buffer)))
	  {
	    if (ptr->rid)
	      yylval.ttype = ridpointers[(int) ptr->rid];
	    value = (int) ptr->token;

	    /* Only return OBJECTNAME if it is a typedef.  */
	    if (doing_objc_thang && value == OBJECTNAME)
	      {
		lastiddecl = lookup_name(yylval.ttype);

		if (lastiddecl == NULL_TREE
		    || TREE_CODE (lastiddecl) != TYPE_DECL)
		  value = IDENTIFIER;
	      }

	    /* Even if we decided to recognize asm, still perhaps warn.  */
	    if (pedantic
		&& (value == ASM_KEYWORD || value == TYPEOF
		    || ptr->rid == RID_INLINE)
		&& token_buffer[0] != '_')
	      pedwarn ("ANSI does not permit the keyword `%s'",
		       token_buffer);
	  }
      }

      /* If we did not find a keyword, look for an identifier
	 (or a typename).  */

      if (value == IDENTIFIER)
	{
 	  if (token_buffer[0] == '@')
	    error("invalid identifier `%s'", token_buffer);

          yylval.ttype = get_identifier (token_buffer);
	  lastiddecl = lookup_name (yylval.ttype);

	  if (lastiddecl != 0 && TREE_CODE (lastiddecl) == TYPE_DECL)
	    value = TYPENAME;
	  /* A user-invisible read-only initialized variable
	     should be replaced by its value.
	     We handle only strings since that's the only case used in C.  */
	  else if (lastiddecl != 0 && TREE_CODE (lastiddecl) == VAR_DECL
		   && DECL_IGNORED_P (lastiddecl)
		   && TREE_READONLY (lastiddecl)
		   && DECL_INITIAL (lastiddecl) != 0
		   && TREE_CODE (DECL_INITIAL (lastiddecl)) == STRING_CST)
	    {
	      tree stringval = DECL_INITIAL (lastiddecl);

	      /* Copy the string value so that we won't clobber anything
		 if we put something in the TREE_CHAIN of this one.  */
	      yylval.ttype = build_string (TREE_STRING_LENGTH (stringval),
					   TREE_STRING_POINTER (stringval));
	      value = STRING;
	    }
          else if (doing_objc_thang)
            {
	      tree objc_interface_decl = is_class_name (yylval.ttype);

	      if (objc_interface_decl)
		{
		  value = CLASSNAME;
		  yylval.ttype = objc_interface_decl;
		}
	    }
	}

      break;

    case '.':
#if USE_CPPLIB
      if (yy_cur < yy_lim)
#endif
	{
	  /* It's hard to preserve tokenization on '.' because
	     it could be a symbol by itself, or it could be the
	     start of a floating point number and cpp won't tell us.  */
	  register int c1 = token_getch ();
	  token_buffer[1] = c1;
	  if (c1 == '.')
	    {
	      c1 = token_getch ();
	      if (c1 == '.')
		{
		  token_buffer[2] = c1;
		  token_buffer[3] = 0;
		  value = ELLIPSIS;
		  goto done;
		}
	      error ("parse error at `..'");
	    }
	  if (ISDIGIT (c1))
	    {
	      token_put_back (c1);
	      goto number;
	    }
	  token_put_back (c1);
	}
      value = '.';
      token_buffer[1] = 0;
      break;

    case '0':  case '1':
      /* Optimize for most frequent case.  */
      {
	register int cond;

#if USE_CPPLIB
	cond = (yy_cur == yy_lim);
#else
	register int c1 = token_getch ();
	token_put_back (c1);
	cond = (! ISALNUM (c1) && c1 != '.');
#endif
	if (cond)
	  {
	    yylval.ttype = (c == '0') ? integer_zero_node : integer_one_node;
	    value = CONSTANT;
	    break;
	  }
	/*FALLTHRU*/
      }
    case '2':  case '3':  case '4':
    case '5':  case '6':  case '7':  case '8':  case '9':
    number:
      {
	int base = 10;
	int count = 0;
	int largest_digit = 0;
	int numdigits = 0;
	int overflow = 0;

	/* We actually store only HOST_BITS_PER_CHAR bits in each part.
	   The code below which fills the parts array assumes that a host
	   int is at least twice as wide as a host char, and that 
	   HOST_BITS_PER_WIDE_INT is an even multiple of HOST_BITS_PER_CHAR.
	   Two HOST_WIDE_INTs is the largest int literal we can store.
	   In order to detect overflow below, the number of parts (TOTAL_PARTS)
	   must be exactly the number of parts needed to hold the bits
	   of two HOST_WIDE_INTs. */
#define TOTAL_PARTS ((HOST_BITS_PER_WIDE_INT / HOST_BITS_PER_CHAR) * 2)
	unsigned int parts[TOTAL_PARTS];

	enum anon1 { NOT_FLOAT, AFTER_POINT, TOO_MANY_POINTS, AFTER_EXPON}
	  floatflag = NOT_FLOAT;

	for (count = 0; count < TOTAL_PARTS; count++)
	  parts[count] = 0;

	p = token_buffer;
	*p++ = c;

	if (c == '0')
	  {
	    *p++ = (c = token_getch());
	    if ((c == 'x') || (c == 'X'))
	      {
		base = 16;
		*p++ = (c = token_getch());
	      }
	    /* Leading 0 forces octal unless the 0 is the only digit.  */
	    else if (c >= '0' && c <= '9')
	      {
		base = 8;
		numdigits++;
	      }
	    else
	      numdigits++;
	  }

	/* Read all the digits-and-decimal-points.  */

	while (c == '.'
	       || (ISALNUM (c) && c != 'l' && c != 'L'
		   && c != 'u' && c != 'U'
		   && c != 'i' && c != 'I' && c != 'j' && c != 'J'
		   && (floatflag == NOT_FLOAT
		       || ((base != 16) && (c != 'f') && (c != 'F'))
		       || base == 16)))   
	  {
	    if (c == '.')
	      {
		if (base == 16 && pedantic)
		  pedwarn ("floating constant may not be in radix 16");
		if (floatflag == TOO_MANY_POINTS)
		  /* We have already emitted an error.  Don't need another.  */
		  ;
		else if (floatflag == AFTER_POINT || floatflag == AFTER_EXPON)
		  {
		    error ("malformed floating constant");
		    floatflag = TOO_MANY_POINTS;
		    /* Avoid another error from atof by forcing all characters
		       from here on to be ignored.  */
		    p[-1] = '\0';
		  }
		else
		  floatflag = AFTER_POINT;

		if (base == 8)
		  base = 10;
		*p++ = c = token_getch();
		/* Accept '.' as the start of a floating-point number
		   only when it is followed by a digit.  */
		if (p == token_buffer + 2 && !ISDIGIT (c))
		  abort ();
	      }
	    else
	      {
		/* It is not a decimal point.
		   It should be a digit (perhaps a hex digit).  */

		if (ISDIGIT (c))
		  {
		    c = c - '0';
		  }
		else if (base <= 10)
		  {
		    if (c == 'e' || c == 'E')
		      {
			base = 10;
			floatflag = AFTER_EXPON;
			break;   /* start of exponent */
		      }
		    error ("nondigits in number and not hexadecimal");
		    c = 0;
		  }
		else if (base == 16 && (c == 'p' || c == 'P'))
		  {
		    floatflag = AFTER_EXPON;
		    break;   /* start of exponent */
		  }
		else if (c >= 'a')
		  {
		    c = c - 'a' + 10;
		  }
		else
		  {
		    c = c - 'A' + 10;
		  }
		if (c >= largest_digit)
		  largest_digit = c;
		numdigits++;

		for (count = 0; count < TOTAL_PARTS; count++)
		  {
		    parts[count] *= base;
		    if (count)
		      {
			parts[count]
			  += (parts[count-1] >> HOST_BITS_PER_CHAR);
			parts[count-1]
			  &= (1 << HOST_BITS_PER_CHAR) - 1;
		      }
		    else
		      parts[0] += c;
		  }

		/* If the highest-order part overflows (gets larger than
		   a host char will hold) then the whole number has 
		   overflowed.  Record this and truncate the highest-order
		   part. */
		if (parts[TOTAL_PARTS - 1] >> HOST_BITS_PER_CHAR)
		  {
		    overflow = 1;
		    parts[TOTAL_PARTS - 1] &= (1 << HOST_BITS_PER_CHAR) - 1;
		  }

		if (p >= token_buffer + maxtoken - 3)
		  p = extend_token_buffer (p);
		*p++ = (c = token_getch());
	      }
	  }

	/* This can happen on input like `int i = 0x;' */
	if (numdigits == 0)
	  error ("numeric constant with no digits");

	if (largest_digit >= base)
	  error ("numeric constant contains digits beyond the radix");

	/* Remove terminating char from the token buffer and delimit the
           string.  */
	*--p = 0;

	if (floatflag != NOT_FLOAT)
	  {
	    tree type;
	    int imag, conversion_errno;
	    REAL_VALUE_TYPE value;
	    struct pf_args args;

	    /* Read explicit exponent if any, and put it in tokenbuf.  */

	    if ((base == 10 && ((c == 'e') || (c == 'E')))
		|| (base == 16 && (c == 'p' || c == 'P')))
	      {
		if (p >= token_buffer + maxtoken - 3)
		  p = extend_token_buffer (p);
		*p++ = c;
		c = token_getch();
		if ((c == '+') || (c == '-'))
		  {
		    *p++ = c;
		    c = token_getch();
		  }
		/* Exponent is decimal, even if string is a hex float.  */
		if (! ISDIGIT (c))
		  error ("floating constant exponent has no digits");
		while (ISDIGIT (c))
		  {
		    if (p >= token_buffer + maxtoken - 3)
		      p = extend_token_buffer (p);
		    *p++ = c;
		    c = token_getch ();
		  }
	      }
	    if (base == 16 && floatflag != AFTER_EXPON)
	      error ("hexadecimal floating constant has no exponent");

	    *p = 0;

	    /* Setup input for parse_float() */
	    args.base = base;
	    args.p = p;
	    args.c = c;

	    /* Convert string to a double, checking for overflow.  */
	    if (do_float_handler (parse_float, (PTR) &args))
	      {
		/* Receive output from parse_float() */
		value = args.value;
	      }
	    else
	      {
		/* We got an exception from parse_float() */
		error ("floating constant out of range");
		value = dconst0;
	      }

	    /* Receive output from parse_float() */
	    c = args.c;
	    imag = args.imag;
	    type = args.type;
	    conversion_errno = args.conversion_errno;
	    
#ifdef ERANGE
	    /* ERANGE is also reported for underflow,
	       so test the value to distinguish overflow from that.  */
	    if (conversion_errno == ERANGE && !flag_traditional && pedantic
		&& (REAL_VALUES_LESS (dconst1, value)
		    || REAL_VALUES_LESS (value, dconstm1)))
	      warning ("floating point number exceeds range of `double'");
#endif

	    /* If the result is not a number, assume it must have been
	       due to some error message above, so silently convert
	       it to a zero.  */
	    if (REAL_VALUE_ISNAN (value))
	      value = dconst0;

	    /* Create a node with determined type and value.  */
	    if (imag)
	      yylval.ttype = build_complex (NULL_TREE,
					    convert (type, integer_zero_node),
					    build_real (type, value));
	    else
	      yylval.ttype = build_real (type, value);
	  }
	else
	  {
	    tree traditional_type, ansi_type, type;
	    HOST_WIDE_INT high, low;
	    int spec_unsigned = 0;
	    int spec_long = 0;
	    int spec_long_long = 0;
	    int spec_imag = 0;
	    int warn = 0, i;

	    traditional_type = ansi_type = type = NULL_TREE;
	    while (1)
	      {
		if (c == 'u' || c == 'U')
		  {
		    if (spec_unsigned)
		      error ("two `u's in integer constant");
		    spec_unsigned = 1;
		  }
		else if (c == 'l' || c == 'L')
		  {
		    if (spec_long)
		      {
			if (spec_long_long)
			  error ("three `l's in integer constant");
			else if (pedantic && ! in_system_header && warn_long_long)
			  pedwarn ("ANSI C forbids long long integer constants");
			spec_long_long = 1;
		      }
		    spec_long = 1;
		  }
		else if (c == 'i' || c == 'j' || c == 'I' || c == 'J')
		  {
		    if (spec_imag)
		      error ("more than one `i' or `j' in numeric constant");
		    else if (pedantic)
		      pedwarn ("ANSI C forbids imaginary numeric constants");
		    spec_imag = 1;
		  }
		else
		  break;
		if (p >= token_buffer + maxtoken - 3)
		  p = extend_token_buffer (p);
		*p++ = c;
		c = token_getch();
	      }

	    /* If the literal overflowed, pedwarn about it now. */
	    if (overflow)
	      {
		warn = 1;
		pedwarn ("integer constant is too large for this configuration of the compiler - truncated to %d bits", HOST_BITS_PER_WIDE_INT * 2);
	      }

	    /* This is simplified by the fact that our constant
	       is always positive.  */

	    high = low = 0;

	    for (i = 0; i < HOST_BITS_PER_WIDE_INT / HOST_BITS_PER_CHAR; i++)
	      {
		high |= ((HOST_WIDE_INT) parts[i + (HOST_BITS_PER_WIDE_INT
						    / HOST_BITS_PER_CHAR)]
			 << (i * HOST_BITS_PER_CHAR));
		low |= (HOST_WIDE_INT) parts[i] << (i * HOST_BITS_PER_CHAR);
	      }

	    yylval.ttype = build_int_2 (low, high);
	    TREE_TYPE (yylval.ttype) = long_long_unsigned_type_node;

	    /* If warn_traditional, calculate both the ANSI type and the
	       traditional type, then see if they disagree.
	       Otherwise, calculate only the type for the dialect in use.  */
	    if (warn_traditional || flag_traditional)
	      {
		/* Calculate the traditional type.  */
		/* Traditionally, any constant is signed;
		   but if unsigned is specified explicitly, obey that.
		   Use the smallest size with the right number of bits,
		   except for one special case with decimal constants.  */
		if (! spec_long && base != 10
		    && int_fits_type_p (yylval.ttype, unsigned_type_node))
		  traditional_type = (spec_unsigned ? unsigned_type_node
				      : integer_type_node);
		/* A decimal constant must be long
		   if it does not fit in type int.
		   I think this is independent of whether
		   the constant is signed.  */
		else if (! spec_long && base == 10
			 && int_fits_type_p (yylval.ttype, integer_type_node))
		  traditional_type = (spec_unsigned ? unsigned_type_node
				      : integer_type_node);
		else if (! spec_long_long)
		  traditional_type = (spec_unsigned ? long_unsigned_type_node
				      : long_integer_type_node);
		else if (int_fits_type_p (yylval.ttype,
					  spec_unsigned 
					  ? long_long_unsigned_type_node
					  : long_long_integer_type_node)) 
		  traditional_type = (spec_unsigned
				      ? long_long_unsigned_type_node
				      : long_long_integer_type_node);
		else
		  traditional_type = (spec_unsigned
				      ? widest_unsigned_literal_type_node
				      : widest_integer_literal_type_node);
	      }
	    if (warn_traditional || ! flag_traditional)
	      {
		/* Calculate the ANSI type.  */
		if (! spec_long && ! spec_unsigned
		    && int_fits_type_p (yylval.ttype, integer_type_node))
		  ansi_type = integer_type_node;
		else if (! spec_long && (base != 10 || spec_unsigned)
			 && int_fits_type_p (yylval.ttype, unsigned_type_node))
		  ansi_type = unsigned_type_node;
		else if (! spec_unsigned && !spec_long_long
			 && int_fits_type_p (yylval.ttype, long_integer_type_node))
		  ansi_type = long_integer_type_node;
		else if (! spec_long_long
			 && int_fits_type_p (yylval.ttype,
					     long_unsigned_type_node))
		  ansi_type = long_unsigned_type_node;
		else if (! spec_unsigned
			 && int_fits_type_p (yylval.ttype,
					     long_long_integer_type_node))
		  ansi_type = long_long_integer_type_node;
		else if (int_fits_type_p (yylval.ttype,
					  long_long_unsigned_type_node))
		  ansi_type = long_long_unsigned_type_node;
		else if (! spec_unsigned
			 && int_fits_type_p (yylval.ttype,
					     widest_integer_literal_type_node))
		  ansi_type = widest_integer_literal_type_node;
		else
		  ansi_type = widest_unsigned_literal_type_node;
	      }

	    type = flag_traditional ? traditional_type : ansi_type;

	    /* We assume that constants specified in a non-decimal
	       base are bit patterns, and that the programmer really
	       meant what they wrote.  */
	    if (warn_traditional && base == 10
		&& traditional_type != ansi_type)
	      {
		if (TYPE_PRECISION (traditional_type)
		    != TYPE_PRECISION (ansi_type))
		  warning ("width of integer constant changes with -traditional");
		else if (TREE_UNSIGNED (traditional_type)
			 != TREE_UNSIGNED (ansi_type))
		  warning ("integer constant is unsigned in ANSI C, signed with -traditional");
		else
		  warning ("width of integer constant may change on other systems with -traditional");
	      }

	    if (pedantic && !flag_traditional && !spec_long_long && !warn
		&& (TYPE_PRECISION (long_integer_type_node)
		    < TYPE_PRECISION (type)))
	      {
		warn = 1;
		pedwarn ("integer constant larger than the maximum value of an unsigned long int");
	      }

	    if (base == 10 && ! spec_unsigned && TREE_UNSIGNED (type))
	      warning ("decimal constant is so large that it is unsigned");

	    if (spec_imag)
	      {
		if (TYPE_PRECISION (type)
		    <= TYPE_PRECISION (integer_type_node))
		  yylval.ttype
		    = build_complex (NULL_TREE, integer_zero_node,
				     convert (integer_type_node,
					      yylval.ttype));
		else
		  error ("complex integer constant is too wide for `complex int'");
	      }
	    else if (flag_traditional && !int_fits_type_p (yylval.ttype, type))
	      /* The traditional constant 0x80000000 is signed
		 but doesn't fit in the range of int.
		 This will change it to -0x80000000, which does fit.  */
	      {
		TREE_TYPE (yylval.ttype) = unsigned_type (type);
		yylval.ttype = convert (type, yylval.ttype);
		TREE_OVERFLOW (yylval.ttype)
		  = TREE_CONSTANT_OVERFLOW (yylval.ttype) = 0;
	      }
	    else
	      TREE_TYPE (yylval.ttype) = type;


	    /* If it's still an integer (not a complex), and it doesn't
	       fit in the type we choose for it, then pedwarn. */

	    if (! warn
		&& TREE_CODE (TREE_TYPE (yylval.ttype)) == INTEGER_TYPE
		&& ! int_fits_type_p (yylval.ttype, TREE_TYPE (yylval.ttype)))
	      pedwarn ("integer constant is larger than the maximum value for its type");
	  }

	token_put_back (c);
	*p = 0;

	if (ISALNUM (c) || c == '.' || c == '_' || c == '$'
	    || (!flag_traditional && (c == '-' || c == '+')
		&& (p[-1] == 'e' || p[-1] == 'E')))
	  error ("missing white space after number `%s'", token_buffer);

	value = CONSTANT; break;
      }

    case '\'':
    char_constant:
      {
	register int result = 0;
	register int num_chars = 0;
	int chars_seen = 0;
	unsigned width = TYPE_PRECISION (char_type_node);
	int max_chars;
#ifdef MULTIBYTE_CHARS
	int longest_char = local_mb_cur_max ();
	(void) local_mbtowc (NULL_PTR, NULL_PTR, 0);
#endif

	max_chars = TYPE_PRECISION (integer_type_node) / width;
	if (wide_flag)
	  width = WCHAR_TYPE_SIZE;

	while (1)
	  {
	  tryagain:
	    c = token_getch();

	    if (c == '\'' || c == EOF)
	      break;

	    ++chars_seen;
	    if (c == '\\')
	      {
		int ignore = 0;
		c = readescape (&ignore);
		if (ignore)
		  goto tryagain;
		if (width < HOST_BITS_PER_INT
		    && (unsigned) c >= ((unsigned)1 << width))
		  pedwarn ("escape sequence out of range for character");
#ifdef MAP_CHARACTER
		if (ISPRINT (c))
		  c = MAP_CHARACTER (c);
#endif
	      }
	    else if (c == '\n')
	      {
		if (pedantic)
		  pedwarn ("ANSI C forbids newline in character constant");
		lineno++;
	      }
	    else
	      {
#ifdef MULTIBYTE_CHARS
		wchar_t wc;
		int i;
		int char_len = -1;
		for (i = 1; i <= longest_char; ++i)
		  {
		    if (i > maxtoken - 4)
		      extend_token_buffer (token_buffer);

		    token_buffer[i] = c;
		    char_len = local_mbtowc (& wc,
					     token_buffer + 1,
					     i);
		    if (char_len != -1)
		      break;
		    c = token_getch ();
		  }
		if (char_len > 1)
		  {
		    /* mbtowc sometimes needs an extra char before accepting */
		    if (char_len < i)
		      token_put_back (c);
		    if (! wide_flag)
		      {
			/* Merge character into result; ignore excess chars.  */
			for (i = 1; i <= char_len; ++i)
			  {
			    if (i > max_chars)
			      break;
			    if (width < HOST_BITS_PER_INT)
			      result = (result << width)
				| (token_buffer[i]
				   & ((1 << width) - 1));
			    else
			      result = token_buffer[i];
			  }
			num_chars += char_len;
			goto tryagain;
		      }
		    c = wc;
		  }
		else
		  {
		    if (char_len == -1)
		      {
			warning ("Ignoring invalid multibyte character");
			/* Replace all but the first byte.  */
			for (--i; i > 1; --i)
			  token_put_back (token_buffer[i]);
			wc = token_buffer[1];
		      }
#ifdef MAP_CHARACTER
		      c = MAP_CHARACTER (wc);
#else
		      c = wc;
#endif
		  }
#else /* ! MULTIBYTE_CHARS */
#ifdef MAP_CHARACTER
		c = MAP_CHARACTER (c);
#endif
#endif /* ! MULTIBYTE_CHARS */
	      }

	    if (wide_flag)
	      {
		if (chars_seen == 1) /* only keep the first one */
		  result = c;
		goto tryagain;
	      }

	    /* Merge character into result; ignore excess chars.  */
	    num_chars += (width / TYPE_PRECISION (char_type_node));
	    if (num_chars < max_chars + 1)
	      {
		if (width < HOST_BITS_PER_INT)
		  result = (result << width) | (c & ((1 << width) - 1));
		else
		  result = c;
	      }
	  }

	if (c != '\'')
	  error ("malformed character constant");
	else if (chars_seen == 0)
	  error ("empty character constant");
	else if (num_chars > max_chars)
	  {
	    num_chars = max_chars;
	    error ("character constant too long");
	  }
	else if (chars_seen != 1 && ! flag_traditional && warn_multichar)
	  warning ("multi-character character constant");

	/* If char type is signed, sign-extend the constant.  */
	if (! wide_flag)
	  {
	    int num_bits = num_chars * width;
	    if (num_bits == 0)
	      /* We already got an error; avoid invalid shift.  */
	      yylval.ttype = build_int_2 (0, 0);
	    else if (TREE_UNSIGNED (char_type_node)
		     || ((result >> (num_bits - 1)) & 1) == 0)
	      yylval.ttype
		= build_int_2 (result & (~(unsigned HOST_WIDE_INT) 0
					 >> (HOST_BITS_PER_WIDE_INT - num_bits)),
			       0);
	    else
	      yylval.ttype
		= build_int_2 (result | ~(~(unsigned HOST_WIDE_INT) 0
					  >> (HOST_BITS_PER_WIDE_INT - num_bits)),
			       -1);
	    TREE_TYPE (yylval.ttype) = integer_type_node;
	  }
	else
	  {
	    yylval.ttype = build_int_2 (result, 0);
	    TREE_TYPE (yylval.ttype) = wchar_type_node;
	  }

	value = CONSTANT;
	break;
      }

    case '"':
    string_constant:
      {
	unsigned width = wide_flag ? WCHAR_TYPE_SIZE
	                           : TYPE_PRECISION (char_type_node);
#ifdef MULTIBYTE_CHARS
	int longest_char = local_mb_cur_max ();
	(void) local_mbtowc (NULL_PTR, NULL_PTR, 0);
#endif
	c = token_getch ();
	p = token_buffer + 1;

	while (c != '"' && c != EOF)
	  {
	    /* ignore_escape_flag is set for reading the filename in #line.  */
	    if (!ignore_escape_flag && c == '\\')
	      {
		int ignore = 0;
		c = readescape (&ignore);
		if (ignore)
		  goto skipnewline;
		if (width < HOST_BITS_PER_INT
		    && (unsigned) c >= ((unsigned)1 << width))
		  pedwarn ("escape sequence out of range for character");
	      }
	    else if (c == '\n')
	      {
		if (pedantic)
		  pedwarn ("ANSI C forbids newline in string constant");
		lineno++;
	      }
	    else
	      {
#ifdef MULTIBYTE_CHARS
		wchar_t wc;
		int i;
		int char_len = -1;
		for (i = 0; i < longest_char; ++i)
		  {
		    if (p + i >= token_buffer + maxtoken)
		      p = extend_token_buffer (p);
		    p[i] = c;

		    char_len = local_mbtowc (& wc, p, i + 1);
		    if (char_len != -1)
		      break;
		    c = token_getch ();
		  }
		if (char_len == -1)
		  {
		    warning ("Ignoring invalid multibyte character");
		    /* Replace all except the first byte.  */
		    token_put_back (c);
		    for (--i; i > 0; --i)
		      token_put_back (p[i]);
		    char_len = 1;
		  }
		/* mbtowc sometimes needs an extra char before accepting */
		if (char_len <= i)
		  token_put_back (c);
		if (! wide_flag)
		  {
		    p += (i + 1);
		    c = token_getch ();
		    continue;
		  }
		c = wc;
#endif /* MULTIBYTE_CHARS */
	      }

	    /* Add this single character into the buffer either as a wchar_t
	       or as a single byte.  */
	    if (wide_flag)
	      {
		unsigned width = TYPE_PRECISION (char_type_node);
		unsigned bytemask = (1 << width) - 1;
		int byte;

		if (p + WCHAR_BYTES > token_buffer + maxtoken)
		  p = extend_token_buffer (p);

		for (byte = 0; byte < WCHAR_BYTES; ++byte)
		  {
		    int value;
		    if (byte >= (int) sizeof (c))
		      value = 0;
		    else
		      value = (c >> (byte * width)) & bytemask;
		    if (BYTES_BIG_ENDIAN)
		      p[WCHAR_BYTES - byte - 1] = value;
		    else
		      p[byte] = value;
		  }
		p += WCHAR_BYTES;
	      }
	    else
	      {
		if (p >= token_buffer + maxtoken)
		  p = extend_token_buffer (p);
		*p++ = c;
	      }

	  skipnewline:
	    c = token_getch ();
	  }

	/* Terminate the string value, either with a single byte zero
	   or with a wide zero.  */
	if (wide_flag)
	  {
	    if (p + WCHAR_BYTES > token_buffer + maxtoken)
	      p = extend_token_buffer (p);
	    bzero (p, WCHAR_BYTES);
	    p += WCHAR_BYTES;
	  }
	else
	  {
	    if (p >= token_buffer + maxtoken)
	      p = extend_token_buffer (p);
	    *p++ = 0;
	  }

	if (c == EOF)
	  error ("Unterminated string constant");

	/* We have read the entire constant.
	   Construct a STRING_CST for the result.  */

	if (wide_flag)
	  {
	    yylval.ttype = build_string (p - (token_buffer + 1),
					 token_buffer + 1);
	    TREE_TYPE (yylval.ttype) = wchar_array_type_node;
	    value = STRING;
	  }
	else if (objc_flag)
	  {
	    /* Return an Objective-C @"..." constant string object.  */
	    yylval.ttype = build_objc_string (p - (token_buffer + 1),
					      token_buffer + 1);
	    TREE_TYPE (yylval.ttype) = char_array_type_node;
	    value = OBJC_STRING;
	  }
	else
	  {
	    yylval.ttype = build_string (p - (token_buffer + 1),
					 token_buffer + 1);
	    TREE_TYPE (yylval.ttype) = char_array_type_node;
	    value = STRING;
	  }

	break;
      }

    case '+':
    case '-':
    case '&':
    case '|':
    case ':':
    case '<':
    case '>':
    case '*':
    case '/':
    case '%':
    case '^':
    case '!':
    case '=':
      {
	register int c1;

      combine:

	switch (c)
	  {
	  case '+':
	    yylval.code = PLUS_EXPR; break;
	  case '-':
	    yylval.code = MINUS_EXPR; break;
	  case '&':
	    yylval.code = BIT_AND_EXPR; break;
	  case '|':
	    yylval.code = BIT_IOR_EXPR; break;
	  case '*':
	    yylval.code = MULT_EXPR; break;
	  case '/':
	    yylval.code = TRUNC_DIV_EXPR; break;
	  case '%':
	    yylval.code = TRUNC_MOD_EXPR; break;
	  case '^':
	    yylval.code = BIT_XOR_EXPR; break;
	  case LSHIFT:
	    yylval.code = LSHIFT_EXPR; break;
	  case RSHIFT:
	    yylval.code = RSHIFT_EXPR; break;
	  case '<':
	    yylval.code = LT_EXPR; break;
	  case '>':
	    yylval.code = GT_EXPR; break;
	  }

	token_buffer[1] = c1 = token_getch();
	token_buffer[2] = 0;

	if (c1 == '=')
	  {
	    switch (c)
	      {
	      case '<':
		value = ARITHCOMPARE; yylval.code = LE_EXPR; goto done;
	      case '>':
		value = ARITHCOMPARE; yylval.code = GE_EXPR; goto done;
	      case '!':
		value = EQCOMPARE; yylval.code = NE_EXPR; goto done;
	      case '=':
		value = EQCOMPARE; yylval.code = EQ_EXPR; goto done;
	      }
	    value = ASSIGN; goto done;
	  }
	else if (c == c1)
	  switch (c)
	    {
	    case '+':
	      value = PLUSPLUS; goto done;
	    case '-':
	      value = MINUSMINUS; goto done;
	    case '&':
	      value = ANDAND; goto done;
	    case '|':
	      value = OROR; goto done;
	    case '<':
	      c = LSHIFT;
	      goto combine;
	    case '>':
	      c = RSHIFT;
	      goto combine;
	    }
	else
	  switch (c)
	    {
	    case '-':
	      if (c1 == '>')
		{ value = POINTSAT; goto done; }
	      break;

	      /* digraphs */
	    case ':':
	      if (c1 == '>')
		{ value = ']'; goto done; }
	      break;
	    case '<':
	      if (c1 == '%')
		{ value = '{'; indent_level++; goto done; }
	      if (c1 == ':')
		{ value = '['; goto done; }
	      break;
	    case '%':
	      if (c1 == '>')
		{ value = '}'; indent_level--; goto done; }
	      break;
	    }

	token_put_back (c1);
	token_buffer[1] = 0;

	if ((c == '<') || (c == '>'))
	  value = ARITHCOMPARE;
	else value = c;
	break;
      }

    case 0:
      /* Don't make yyparse think this is eof.  */
      value = 1;
      break;

    case '{':
      indent_level++;
      value = c;
      break;

    case '}':
      indent_level--;
      value = c;
      break;

    default:
      value = c;
    }

done:
/*  yylloc.last_line = lineno; */

  return value;
}

/* Sets the value of the 'yydebug' variable to VALUE.
   This is a function so we don't have to have YYDEBUG defined
   in order to build the compiler.  */

void
set_yydebug (value)
     int value;
{
#if YYDEBUG != 0
  yydebug = value;
#else
  warning ("YYDEBUG not defined.");
#endif
}
