/* CPP Library. (Directive handling.)
   Copyright (C) 1986, 1987, 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001 Free Software Foundation, Inc.
   Contributed by Per Bothner, 1994-95.
   Based on CCCP program by Paul Rubin, June 1986
   Adapted to ANSI C, Richard Stallman, Jan 1987

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"

#include "cpplib.h"
#include "cpphash.h"
#include "intl.h"
#include "obstack.h"

/* Chained list of answers to an assertion.  */
struct answer
{
  struct answer *next;
  unsigned int count;
  cpp_token first[1];
};

/* Stack of conditionals currently in progress
   (including both successful and failing conditionals).  */

struct if_stack
{
  struct if_stack *next;
  cpp_lexer_pos pos;		/* line and column where condition started */
  const cpp_hashnode *mi_cmacro;/* macro name for #ifndef around entire file */
  unsigned char was_skipping;	/* Value of pfile->skipping before this if.  */
  int type;			/* type of last directive seen in this group */
};

/* Values for the origin field of struct directive.  KANDR directives
   come from traditional (K&R) C.  STDC89 directives come from the
   1989 C standard.  EXTENSION directives are extensions.  */
#define KANDR		0
#define STDC89		1
#define EXTENSION	2

/* Values for the flags field of struct directive.  COND indicates a
   conditional; IF_COND an opening conditional.  INCL means to treat
   "..." and <...> as q-char and h-char sequences respectively.  IN_I
   means this directive should be handled even if -fpreprocessed is in
   effect (these are the directives with callback hooks).  */
#define COND		(1 << 0)
#define IF_COND		(1 << 1)
#define INCL		(1 << 2)
#define IN_I		(1 << 3)

/* Defines one #-directive, including how to handle it.  */
typedef void (*directive_handler) PARAMS ((cpp_reader *));
typedef struct directive directive;
struct directive
{
  directive_handler handler;	/* Function to handle directive.  */
  const U_CHAR *name;		/* Name of directive.  */
  unsigned short length;	/* Length of name.  */
  unsigned char origin;		/* Origin of directive.  */
  unsigned char flags;	        /* Flags describing this directive.  */
};

/* Forward declarations.  */

static void skip_rest_of_line	PARAMS ((cpp_reader *));
static void check_eol		PARAMS ((cpp_reader *));
static void start_directive	PARAMS ((cpp_reader *));
static void end_directive	PARAMS ((cpp_reader *, int));
static void run_directive	PARAMS ((cpp_reader *, int,
					 enum cpp_buffer_type,
					 const char *, size_t));
static int glue_header_name	PARAMS ((cpp_reader *, cpp_token *));
static int  parse_include	PARAMS ((cpp_reader *, cpp_token *));
static void push_conditional	PARAMS ((cpp_reader *, int, int,
					 const cpp_hashnode *));
static unsigned int read_flag	PARAMS ((cpp_reader *, unsigned int));
static int  strtoul_for_line	PARAMS ((const U_CHAR *, unsigned int,
					 unsigned long *));
static void do_diagnostic	PARAMS ((cpp_reader *, enum error_type, int));
static cpp_hashnode *lex_macro_node	PARAMS ((cpp_reader *));
static void do_include_common	PARAMS ((cpp_reader *, enum include_type));
static void do_pragma_once	PARAMS ((cpp_reader *));
static void do_pragma_poison	PARAMS ((cpp_reader *));
static void do_pragma_system_header	PARAMS ((cpp_reader *));
static void do_pragma_dependency	PARAMS ((cpp_reader *));
static int get__Pragma_string	PARAMS ((cpp_reader *, cpp_token *));
static unsigned char *destringize	PARAMS ((const cpp_string *,
						 unsigned int *));
static int parse_answer PARAMS ((cpp_reader *, struct answer **, int));
static cpp_hashnode *parse_assertion PARAMS ((cpp_reader *, struct answer **,
					      int));
static struct answer ** find_answer PARAMS ((cpp_hashnode *,
					     const struct answer *));
static void handle_assertion	PARAMS ((cpp_reader *, const char *, int));

/* This is the table of directive handlers.  It is ordered by
   frequency of occurrence; the numbers at the end are directive
   counts from all the source code I have lying around (egcs and libc
   CVS as of 1999-05-18, plus grub-0.5.91, linux-2.2.9, and
   pcmcia-cs-3.0.9).  This is no longer important as directive lookup
   is now O(1).  All extensions other than #warning and #include_next
   are deprecated.  The name is where the extension appears to have
   come from.  */

#define DIRECTIVE_TABLE							\
D(define,	T_DEFINE = 0,	KANDR,     IN_I)	   /* 270554 */ \
D(include,	T_INCLUDE,	KANDR,     INCL)	   /*  52262 */ \
D(endif,	T_ENDIF,	KANDR,     COND)	   /*  45855 */ \
D(ifdef,	T_IFDEF,	KANDR,     COND | IF_COND) /*  22000 */ \
D(if,		T_IF,		KANDR,     COND | IF_COND) /*  18162 */ \
D(else,		T_ELSE,		KANDR,     COND)	   /*   9863 */ \
D(ifndef,	T_IFNDEF,	KANDR,     COND | IF_COND) /*   9675 */ \
D(undef,	T_UNDEF,	KANDR,     IN_I)	   /*   4837 */ \
D(line,		T_LINE,		KANDR,     IN_I)	   /*   2465 */ \
D(elif,		T_ELIF,		STDC89,    COND)	   /*    610 */ \
D(error,	T_ERROR,	STDC89,    0)		   /*    475 */ \
D(pragma,	T_PRAGMA,	STDC89,    IN_I)	   /*    195 */ \
D(warning,	T_WARNING,	EXTENSION, 0)		   /*     22 */ \
D(include_next,	T_INCLUDE_NEXT,	EXTENSION, INCL)	   /*     19 */ \
D(ident,	T_IDENT,	EXTENSION, IN_I)	   /*     11 */ \
D(import,	T_IMPORT,	EXTENSION, INCL)	   /* 0 ObjC */	\
D(assert,	T_ASSERT,	EXTENSION, 0)		   /* 0 SVR4 */	\
D(unassert,	T_UNASSERT,	EXTENSION, 0)		   /* 0 SVR4 */	\
SCCS_ENTRY						   /* 0 SVR4? */

/* #sccs is not always recognized.  */
#ifdef SCCS_DIRECTIVE
# define SCCS_ENTRY D(sccs, T_SCCS, EXTENSION, 0)
#else
# define SCCS_ENTRY /* nothing */
#endif

/* Use the table to generate a series of prototypes, an enum for the
   directive names, and an array of directive handlers.  */

/* The directive-processing functions are declared to return int
   instead of void, because some old compilers have trouble with
   pointers to functions returning void.  */

/* Don't invoke CONCAT2 with any whitespace or K&R cc will fail. */
#define D(name, t, o, f) static void CONCAT2(do_,name) PARAMS ((cpp_reader *));
DIRECTIVE_TABLE
#undef D

#define D(n, tag, o, f) tag,
enum
{
  DIRECTIVE_TABLE
  N_DIRECTIVES
};
#undef D

/* Don't invoke CONCAT2 with any whitespace or K&R cc will fail. */
#define D(name, t, origin, flags) \
{ CONCAT2(do_,name), (const U_CHAR *) STRINGX(name), \
  sizeof STRINGX(name) - 1, origin, flags },
static const directive dtable[] =
{
DIRECTIVE_TABLE
};
#undef D
#undef DIRECTIVE_TABLE

/* Skip any remaining tokens in a directive.  */
static void
skip_rest_of_line (pfile)
     cpp_reader *pfile;
{
  cpp_token token;

  /* Discard all input lookaheads.  */
  while (pfile->la_read)
    _cpp_release_lookahead (pfile);

  /* Discard all stacked contexts.  */
  while (pfile->context != &pfile->base_context)
    _cpp_pop_context (pfile);

  /* Sweep up all tokens remaining on the line.  */
  pfile->state.prevent_expansion++;
  while (!pfile->state.next_bol)
    _cpp_lex_token (pfile, &token);
  pfile->state.prevent_expansion--;
}

/* Ensure there are no stray tokens at the end of a directive.  */
static void
check_eol (pfile)
     cpp_reader *pfile;
{
  if (!pfile->state.next_bol)
    {
      cpp_token token;

      _cpp_lex_token (pfile, &token);
      if (token.type != CPP_EOF)
	cpp_pedwarn (pfile, "extra tokens at end of #%s directive",
		     pfile->directive->name);
    }
}

/* Called when entering a directive, _Pragma or command-line directive.  */
static void
start_directive (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *buffer = pfile->buffer;

  /* Setup in-directive state.  */
  pfile->state.in_directive = 1;
  pfile->state.save_comments = 0;

  /* Some handlers need the position of the # for diagnostics.  */
  pfile->directive_pos = pfile->lexer_pos;

  /* Don't save directive tokens for external clients.  */
  pfile->la_saved = pfile->la_write;
  pfile->la_write = 0;

  /* Turn off skipping.  */
  buffer->was_skipping = pfile->skipping;
  pfile->skipping = 0;
}

/* Called when leaving a directive, _Pragma or command-line directive.  */
static void
end_directive (pfile, skip_line)
     cpp_reader *pfile;
     int skip_line;
{
  cpp_buffer *buffer = pfile->buffer;

  /* Restore pfile->skipping before skip_rest_of_line, so that e.g.
     __VA_ARGS__ in the rest of the directive doesn't warn.  */
  pfile->skipping = buffer->was_skipping;

  /* We don't skip for an assembler #.  */
  if (skip_line)
    skip_rest_of_line (pfile);

  /* Restore state.  */
  pfile->la_write = pfile->la_saved;
  pfile->state.save_comments = ! CPP_OPTION (pfile, discard_comments);
  pfile->state.in_directive = 0;
  pfile->state.angled_headers = 0;
  pfile->state.line_extension = 0;
  pfile->directive = 0;
}

/* Check if a token's name matches that of a known directive.  Put in
   this file to save exporting dtable and other unneeded information.  */
int
_cpp_handle_directive (pfile, indented)
     cpp_reader *pfile;
     int indented;
{
  cpp_buffer *buffer = pfile->buffer;
  const directive *dir = 0;
  cpp_token dname;
  int skip = 1;

  start_directive (pfile);

  /* Lex the directive name directly.  */
  _cpp_lex_token (pfile, &dname);

  if (dname.type == CPP_NAME)
    {
      unsigned int index = dname.val.node->directive_index;
      if (index)
	dir = &dtable[index - 1];
    }
  else if (dname.type == CPP_NUMBER)
    {
      /* # followed by a number is equivalent to #line.  Do not
	 recognize this form in assembly language source files or
	 skipped conditional groups.  Complain about this form if
	 we're being pedantic, but not if this is regurgitated input
	 (preprocessed or fed back in by the C++ frontend).  */
      if (! buffer->was_skipping && CPP_OPTION (pfile, lang) != CLK_ASM)
	{
	  dir = &dtable[T_LINE];
	  pfile->state.line_extension = 1;
	  _cpp_push_token (pfile, &dname, &pfile->directive_pos);
	  if (CPP_PEDANTIC (pfile) && ! CPP_OPTION (pfile, preprocessed))
	    cpp_pedwarn (pfile, "# followed by integer");
	}
    }

  pfile->directive = dir;
  if (dir)
    {
      /* Make sure we lex headers correctly, whether skipping or not.  */
      pfile->state.angled_headers = dir->flags & INCL;

      /* If we are rescanning preprocessed input, only directives tagged
	 with IN_I are honored, and the warnings below are suppressed.  */
      if (CPP_OPTION (pfile, preprocessed))
	{
	  /* Kluge alert.  In order to be sure that code like this
	     #define HASH #
	     HASH define foo bar
	     does not cause '#define foo bar' to get executed when
	     compiled with -save-temps, we recognize directives in
	     -fpreprocessed mode only if the # is in column 1 and the
	     directive name starts in column 2.  This output can only
	     be generated by the directive callbacks in cppmain.c (see
	     also the special case in scan_buffer).  */
	  if (dir->flags & IN_I && !indented && !(dname.flags & PREV_WHITE))
	    (*dir->handler) (pfile);
	  /* That check misses '# 123' linemarkers.  Let them through too.  */
	  else if (dname.type == CPP_NUMBER)
	    (*dir->handler) (pfile);
	  else
	    {
	      /* We don't want to process this directive.  Put back the
		 tokens so caller will see them (and issue an error,
		 probably).  */
	      _cpp_push_token (pfile, &dname, &pfile->directive_pos);
	      skip = 0;
	    }
	}
      else
	{
	  /* Traditionally, a directive is ignored unless its # is in
	     column 1.  Therefore in code intended to work with K+R
	     compilers, directives added by C89 must have their #
	     indented, and directives present in traditional C must
	     not.  This is true even of directives in skipped
	     conditional blocks.  */
	  if (CPP_WTRADITIONAL (pfile))
	    {
	      if (dir == &dtable[T_ELIF])
		cpp_warning (pfile,
			     "suggest not using #elif in traditional C");
	      else if (indented && dir->origin == KANDR)
		cpp_warning (pfile,
			     "traditional C ignores #%s with the # indented",
			     dir->name);
	      else if (!indented && dir->origin != KANDR)
		cpp_warning (pfile,
	     "suggest hiding #%s from traditional C with an indented #",
			     dir->name);
	    }

	  /* If we are skipping a failed conditional group, all
	     non-conditional directives are ignored.  */
	  if (! buffer->was_skipping || (dir->flags & COND))
	    {
	      /* Issue -pedantic warnings for extensions.   */
	      if (CPP_PEDANTIC (pfile) && dir->origin == EXTENSION)
		cpp_pedwarn (pfile, "#%s is a GCC extension", dir->name);

	      /* If we have a directive that is not an opening
		 conditional, invalidate any control macro.  */
	      if (! (dir->flags & IF_COND))
		pfile->mi_state = MI_FAILED;

	      (*dir->handler) (pfile);
	    }
	}
    }
  else if (dname.type != CPP_EOF && ! buffer->was_skipping)
    {
      /* An unknown directive.  Don't complain about it in assembly
	 source: we don't know where the comments are, and # may
	 introduce assembler pseudo-ops.  Don't complain about invalid
	 directives in skipped conditional groups (6.10 p4).  */
      if (CPP_OPTION (pfile, lang) == CLK_ASM)
	{
	  /* Output the # and lookahead token for the assembler.  */
	  _cpp_push_token (pfile, &dname, &pfile->directive_pos);
	  skip = 0;
	}
      else
	cpp_error (pfile, "invalid preprocessing directive #%s",
		   cpp_token_as_text (pfile, &dname));
    }

  end_directive (pfile, skip);
  return skip;
}

/* Directive handler wrapper used by the command line option
   processor.  */
static void
run_directive (pfile, dir_no, type, buf, count)
     cpp_reader *pfile;
     int dir_no;
     enum cpp_buffer_type type;
     const char *buf;
     size_t count;
{
  unsigned int output_line = pfile->lexer_pos.output_line;
  cpp_buffer *buffer;

  buffer = cpp_push_buffer (pfile, (const U_CHAR *) buf, count, type, 0);

  if (dir_no == T_PRAGMA)
    {
      /* A kludge to avoid line markers for _Pragma.  */
      pfile->lexer_pos.output_line = output_line;
      /* Avoid interpretation of directives in a _Pragma string.  */
      pfile->state.next_bol = 0;
    }

  start_directive (pfile);
  pfile->state.prevent_expansion++;
  pfile->directive = &dtable[dir_no];
  (void) (*pfile->directive->handler) (pfile);
  pfile->state.prevent_expansion--;
  check_eol (pfile);
  end_directive (pfile, 1);

  cpp_pop_buffer (pfile);
}

/* Checks for validity the macro name in #define, #undef, #ifdef and
   #ifndef directives.  */
static cpp_hashnode *
lex_macro_node (pfile)
     cpp_reader *pfile;
{
  cpp_token token;

  /* Lex the macro name directly.  */
  _cpp_lex_token (pfile, &token);

  /* The token immediately after #define must be an identifier.  That
     identifier is not allowed to be "defined".  See predefined macro
     names (6.10.8.4).  In C++, it is not allowed to be any of the
     <iso646.h> macro names (which are keywords in C++) either.  */

  if (token.type != CPP_NAME)
    {
      if (token.type == CPP_EOF)
	cpp_error (pfile, "no macro name given in #%s directive",
		   pfile->directive->name);
      else if (token.flags & NAMED_OP)
	cpp_error (pfile,
		   "\"%s\" cannot be used as a macro name as it is an operator in C++",
		   NODE_NAME (token.val.node));
      else
	cpp_error (pfile, "macro names must be identifiers");
    }
  else
    {
      cpp_hashnode *node = token.val.node;

      /* In Objective C, some keywords begin with '@', but general
	 identifiers do not, and you're not allowed to #define them.  */
      if (node == pfile->spec_nodes.n_defined || NODE_NAME (node)[0] == '@')
	cpp_error (pfile, "\"%s\" cannot be used as a macro name",
		   NODE_NAME (node));
      else if (!(node->flags & NODE_POISONED))
	return node;
    }

  return 0;
}

/* Process a #define directive.  Most work is done in cppmacro.c.  */
static void
do_define (pfile)
     cpp_reader *pfile;
{
  cpp_hashnode *node = lex_macro_node (pfile);

  if (node)
    {
      if (_cpp_create_definition (pfile, node))
	if (pfile->cb.define)
	  (*pfile->cb.define) (pfile, node);
    }
}

/* Handle #undef.  Marks the identifier NT_VOID in the hash table.  */
static void
do_undef (pfile)
     cpp_reader *pfile;
{
  cpp_hashnode *node = lex_macro_node (pfile);  

  /* 6.10.3.5 paragraph 2: [#undef] is ignored if the specified identifier
     is not currently defined as a macro name.  */
  if (node && node->type == NT_MACRO)
    {
      if (pfile->cb.undef)
	(*pfile->cb.undef) (pfile, node);

      if (node->flags & NODE_WARN)
	cpp_warning (pfile, "undefining \"%s\"", NODE_NAME (node));

      _cpp_free_definition (node);
    }
  check_eol (pfile);
}

/* Helper routine used by parse_include.  Reinterpret the current line
   as an h-char-sequence (< ... >); we are looking at the first token
   after the <.  Returns zero on success.  */
static int
glue_header_name (pfile, header)
     cpp_reader *pfile;
     cpp_token *header;
{
  cpp_token token;
  unsigned char *buffer, *token_mem;
  size_t len, total_len = 0, capacity = 1024;

  /* To avoid lexed tokens overwriting our glued name, we can only
     allocate from the string pool once we've lexed everything.  */

  buffer = (unsigned char *) xmalloc (capacity);
  for (;;)
    {
      cpp_get_token (pfile, &token);

      if (token.type == CPP_GREATER || token.type == CPP_EOF)
	break;

      len = cpp_token_len (&token);
      if (total_len + len > capacity)
	{
	  capacity = (capacity + len) * 2;
	  buffer = (unsigned char *) xrealloc (buffer, capacity);
	}

      if (token.flags & PREV_WHITE)
	buffer[total_len++] = ' ';

      total_len = cpp_spell_token (pfile, &token, &buffer[total_len]) - buffer;
    }

  if (token.type == CPP_EOF)
    cpp_error (pfile, "missing terminating > character");
  else
    {
      token_mem = _cpp_pool_alloc (&pfile->ident_pool, total_len + 1);
      memcpy (token_mem, buffer, total_len);
      token_mem[total_len] = '\0';

      header->type = CPP_HEADER_NAME;
      header->flags &= ~PREV_WHITE;
      header->val.str.len = total_len;
      header->val.str.text = token_mem;
    }

  free ((PTR) buffer);
  return token.type == CPP_EOF;
}

/* Parse the header name of #include, #include_next, #import and
   #pragma dependency.  Returns zero on success.  */
static int
parse_include (pfile, header)
     cpp_reader *pfile;
     cpp_token *header;
{
  int is_pragma = pfile->directive == &dtable[T_PRAGMA];
  const unsigned char *dir;

  if (is_pragma)
    dir = U"pragma dependency";
  else
    dir = pfile->directive->name;

  /* Allow macro expansion.  */
  cpp_get_token (pfile, header);
  if (header->type != CPP_STRING && header->type != CPP_HEADER_NAME)
    {
      if (header->type != CPP_LESS)
	{
	  cpp_error (pfile, "#%s expects \"FILENAME\" or <FILENAME>", dir);
	  return 1;
	}
      if (glue_header_name (pfile, header))
	return 1;
    }

  if (header->val.str.len == 0)
    {
      cpp_error (pfile, "empty file name in #%s", dir);
      return 1;
    }

  if (!is_pragma)
    {
      check_eol (pfile);
      /* Get out of macro context, if we are.  */
      skip_rest_of_line (pfile);
      if (pfile->cb.include)
	(*pfile->cb.include) (pfile, dir, header);
    }

  return 0;
}

/* Handle #include, #include_next and #import.  */
static void
do_include_common (pfile, type)
     cpp_reader *pfile;
     enum include_type type;
{
  cpp_token header;

  if (!parse_include (pfile, &header))
    {
      /* Prevent #include recursion.  */
      if (pfile->buffer_stack_depth >= CPP_STACK_MAX)
	cpp_fatal (pfile, "#include nested too deeply");
      else if (pfile->context->prev)
	cpp_ice (pfile, "attempt to push file buffer with contexts stacked");
      else
	{
	  /* For #include_next, if this is the primary source file,
	     warn and use the normal search logic.  */
	  if (type == IT_INCLUDE_NEXT && ! pfile->buffer->prev)
	    {
	      cpp_warning (pfile, "#include_next in primary source file");
	      type = IT_INCLUDE;
	    }

	  _cpp_execute_include (pfile, &header, type);
	}
    }
}

static void
do_include (pfile)
     cpp_reader *pfile;
{
  do_include_common (pfile, IT_INCLUDE);
}

static void
do_import (pfile)
     cpp_reader *pfile;
{
  if (!pfile->import_warning && CPP_OPTION (pfile, warn_import))
    {
      pfile->import_warning = 1;
      cpp_warning (pfile,
	   "#import is obsolete, use an #ifndef wrapper in the header file");
    }

  do_include_common (pfile, IT_IMPORT);
}

static void
do_include_next (pfile)
     cpp_reader *pfile;
{
  do_include_common (pfile, IT_INCLUDE_NEXT);
}

/* Subroutine of do_line.  Read possible flags after file name.  LAST
   is the last flag seen; 0 if this is the first flag. Return the flag
   if it is valid, 0 at the end of the directive. Otherwise complain.  */

static unsigned int
read_flag (pfile, last)
     cpp_reader *pfile;
     unsigned int last;
{
  cpp_token token;

  _cpp_lex_token (pfile, &token);
  if (token.type == CPP_NUMBER && token.val.str.len == 1)
    {
      unsigned int flag = token.val.str.text[0] - '0';

      if (flag > last && flag <= 4
	  && (flag != 4 || last == 3)
	  && (flag != 2 || last == 0))
	return flag;
    }

  if (token.type != CPP_EOF)
    cpp_error (pfile, "invalid flag \"%s\" in line directive",
	       cpp_token_as_text (pfile, &token));
  return 0;
}

/* Another subroutine of do_line.  Convert a number in STR, of length
   LEN, to binary; store it in NUMP, and return 0 if the number was
   well-formed, 1 if not.  Temporary, hopefully.  */
static int
strtoul_for_line (str, len, nump)
     const U_CHAR *str;
     unsigned int len;
     unsigned long *nump;
{
  unsigned long reg = 0;
  U_CHAR c;
  while (len--)
    {
      c = *str++;
      if (!ISDIGIT (c))
	return 1;
      reg *= 10;
      reg += c - '0';
    }
  *nump = reg;
  return 0;
}

/* Interpret #line command.
   Note that the filename string (if any) is treated as if it were an
   include filename.  That means no escape handling.  */

static void
do_line (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *buffer = pfile->buffer;
  const char *filename = buffer->nominal_fname;
  unsigned int lineno = buffer->lineno;
  enum cpp_fc_reason reason = FC_RENAME;
  unsigned long new_lineno;
  unsigned int cap;
  cpp_token token;

  /* C99 raised the minimum limit on #line numbers.  */
  cap = CPP_OPTION (pfile, c99) ? 2147483647 : 32767;

  /* #line commands expand macros.  */
  cpp_get_token (pfile, &token);
  if (token.type != CPP_NUMBER
      || strtoul_for_line (token.val.str.text, token.val.str.len, &new_lineno))
    {
      cpp_error (pfile, "\"%s\" after #line is not a positive integer",
		 cpp_token_as_text (pfile, &token));
      return;
    }      

  if (CPP_PEDANTIC (pfile) && (new_lineno == 0 || new_lineno > cap))
    cpp_pedwarn (pfile, "line number out of range");

  cpp_get_token (pfile, &token);
  if (token.type == CPP_STRING)
    {
      const char *fname = (const char *) token.val.str.text;

      /* Only accept flags for the # 55 form.  */
      if (! pfile->state.line_extension)
	check_eol (pfile);
      else
	{
	  int flag = 0, sysp = 0;

	  flag = read_flag (pfile, flag);
	  if (flag == 1)
	    {
	      reason = FC_ENTER;
	      flag = read_flag (pfile, flag);
	    }
	  else if (flag == 2)
	    {
	      reason = FC_LEAVE;
	      flag = read_flag (pfile, flag);
	    }
	  if (flag == 3)
	    {
	      sysp = 1;
	      flag = read_flag (pfile, flag);
	      if (flag == 4)
		sysp = 2, read_flag (pfile, flag);
	    }

	  if (reason == FC_ENTER)
	    {
	      /* Fake a buffer stack for diagnostics.  */
	      cpp_push_buffer (pfile, 0, 0, BUF_FAKE, fname);
	      /* Fake an include for cpp_included.  */
	      _cpp_fake_include (pfile, fname);
	      buffer = pfile->buffer;
	    }
	  else if (reason == FC_LEAVE)
	    {
	      if (buffer->type != BUF_FAKE)
		cpp_warning (pfile, "file \"%s\" left but not entered",
			     buffer->nominal_fname);
	      else
		{
		  cpp_pop_buffer (pfile);
		  buffer = pfile->buffer;
#ifdef ENABLE_CHECKING
		  if (strcmp (buffer->nominal_fname, fname))
		    cpp_warning (pfile, "expected to return to file \"%s\"",
				 buffer->nominal_fname);
		  if (buffer->lineno + 1 != new_lineno)
		    cpp_warning (pfile, "expected to return to line number %u",
				 buffer->lineno + 1);
		  if (buffer->sysp != sysp)
		    cpp_warning (pfile, "header flags for \"%s\" have changed",
				 buffer->nominal_fname);
#endif
		}
	    }
	  buffer->sysp = sysp;
	}
      buffer->nominal_fname = fname;
    }
  else if (token.type != CPP_EOF)
    {
      cpp_error (pfile, "\"%s\" is not a valid filename",
		 cpp_token_as_text (pfile, &token));
      return;
    }

  /* Our line number is incremented after the directive is processed.  */
  buffer->lineno = new_lineno - 1;
  _cpp_do_file_change (pfile, reason, filename, lineno);
}

/* Arrange the file_change callback.  */
void
_cpp_do_file_change (pfile, reason, from_file, from_lineno)
     cpp_reader *pfile;
     enum cpp_fc_reason reason;
     const char *from_file;
     unsigned int from_lineno;
{
  if (pfile->cb.file_change)
    {
      cpp_file_change fc;
      cpp_buffer *buffer = pfile->buffer;

      fc.reason = reason;
      fc.to.filename = buffer->nominal_fname;
      fc.to.lineno = buffer->lineno + 1;
      fc.sysp = buffer->sysp;
      fc.externc = CPP_OPTION (pfile, cplusplus) && buffer->sysp == 2;

      /* Caller doesn't need to handle FC_ENTER.  */
      if (reason == FC_ENTER)
	{
	  if (buffer->prev)
	    {
	      from_file = buffer->prev->nominal_fname;
	      from_lineno = buffer->prev->lineno;
	    }
	  else
	    from_file = 0;
	}
      /* Special case for file "foo.i" with "# 1 foo.c" on first line.  */
      else if (reason == FC_RENAME && ! buffer->prev
	       && pfile->directive_pos.line == 1)
	from_file = 0;

      fc.from.filename = from_file;
      fc.from.lineno = from_lineno;
      pfile->cb.file_change (pfile, &fc);
    }
}

/*
 * Report a warning or error detected by the program we are
 * processing.  Use the directive's tokens in the error message.
 */

static void
do_diagnostic (pfile, code, print_dir)
     cpp_reader *pfile;
     enum error_type code;
     int print_dir;
{
  if (_cpp_begin_message (pfile, code, NULL, 0))
    {
      if (print_dir)
	fprintf (stderr, "#%s ", pfile->directive->name);
      pfile->state.prevent_expansion++;
      cpp_output_line (pfile, stderr);
      pfile->state.prevent_expansion--;
    }
}

static void
do_error (pfile)
     cpp_reader *pfile;
{
  do_diagnostic (pfile, ERROR, 1);
}

static void
do_warning (pfile)
     cpp_reader *pfile;
{
  /* We want #warning diagnostics to be emitted in system headers too.  */
  do_diagnostic (pfile, WARNING_SYSHDR, 1);
}

/* Report program identification.  */

static void
do_ident (pfile)
     cpp_reader *pfile;
{
  cpp_token str;

  cpp_get_token (pfile, &str);
  if (str.type != CPP_STRING)
    cpp_error (pfile, "invalid #ident");
  else if (pfile->cb.ident)
    (*pfile->cb.ident) (pfile, &str.val.str);

  check_eol (pfile);
}

/* Pragmata handling.  We handle some of these, and pass the rest on
   to the front end.  C99 defines three pragmas and says that no macro
   expansion is to be performed on them; whether or not macro
   expansion happens for other pragmas is implementation defined.
   This implementation never macro-expands the text after #pragma.  */

/* Sub-handlers for the pragmas needing treatment here.
   They return 1 if the token buffer is to be popped, 0 if not. */
struct pragma_entry
{
  struct pragma_entry *next;
  const char *name;
  size_t len;
  int isnspace;
  union {
    void (*handler) PARAMS ((cpp_reader *));
    struct pragma_entry *space;
  } u;
};

void
cpp_register_pragma (pfile, space, name, handler)
     cpp_reader *pfile;
     const char *space;
     const char *name;
     void (*handler) PARAMS ((cpp_reader *));
{
  struct pragma_entry **x, *new;
  size_t len;

  x = &pfile->pragmas;
  if (space)
    {
      struct pragma_entry *p = pfile->pragmas;
      len = strlen (space);
      while (p)
	{
	  if (p->isnspace && p->len == len && !memcmp (p->name, space, len))
	    {
	      x = &p->u.space;
	      goto found;
	    }
	  p = p->next;
	}
      cpp_ice (pfile, "unknown #pragma namespace %s", space);
      return;
    }

 found:
  new = xnew (struct pragma_entry);
  new->name = name;
  new->len = strlen (name);
  new->isnspace = 0;
  new->u.handler = handler;

  new->next = *x;
  *x = new;
}

void
cpp_register_pragma_space (pfile, space)
     cpp_reader *pfile;
     const char *space;
{
  struct pragma_entry *new;
  const struct pragma_entry *p = pfile->pragmas;
  size_t len = strlen (space);

  while (p)
    {
      if (p->isnspace && p->len == len && !memcmp (p->name, space, len))
	/* Multiple different callers are allowed to register the same
	   namespace.  */
	return;
      p = p->next;
    }

  new = xnew (struct pragma_entry);
  new->name = space;
  new->len = len;
  new->isnspace = 1;
  new->u.space = 0;

  new->next = pfile->pragmas;
  pfile->pragmas = new;
}
  
void
_cpp_init_internal_pragmas (pfile)
     cpp_reader *pfile;
{
  /* top level */
  cpp_register_pragma (pfile, 0, "poison", do_pragma_poison);
  cpp_register_pragma (pfile, 0, "once", do_pragma_once);

  /* GCC namespace */
  cpp_register_pragma_space (pfile, "GCC");

  cpp_register_pragma (pfile, "GCC", "poison", do_pragma_poison);
  cpp_register_pragma (pfile, "GCC", "system_header", do_pragma_system_header);
  cpp_register_pragma (pfile, "GCC", "dependency", do_pragma_dependency);
}

static void
do_pragma (pfile)
     cpp_reader *pfile;
{
  const struct pragma_entry *p;
  cpp_token tok;
  int drop = 0;

  p = pfile->pragmas;
  pfile->state.prevent_expansion++;
  cpp_start_lookahead (pfile);

 new_space:
  cpp_get_token (pfile, &tok);
  if (tok.type == CPP_NAME)
    {
      const cpp_hashnode *node = tok.val.node;
      size_t len = NODE_LEN (node);

      while (p)
	{
	  if (strlen (p->name) == len
	      && !memcmp (p->name, NODE_NAME (node), len))
	    {
	      if (p->isnspace)
		{
		  p = p->u.space;
		  goto new_space;
		}
	      else
		{
		  (*p->u.handler) (pfile);
		  drop = 1;
		  break;
		}
	    }
	  p = p->next;
	}
    }

  cpp_stop_lookahead (pfile, drop);
  pfile->state.prevent_expansion--;

  if (!drop && pfile->cb.def_pragma)
    (*pfile->cb.def_pragma) (pfile);
}

static void
do_pragma_once (pfile)
     cpp_reader *pfile;
{
  cpp_warning (pfile, "#pragma once is obsolete");
 
  if (pfile->buffer->prev == NULL)
    cpp_warning (pfile, "#pragma once in main file");
  else
    _cpp_never_reread (pfile->buffer->inc);

  check_eol (pfile);
}

static void
do_pragma_poison (pfile)
     cpp_reader *pfile;
{
  /* Poison these symbols so that all subsequent usage produces an
     error message.  */
  cpp_token tok;
  cpp_hashnode *hp;

  pfile->state.poisoned_ok = 1;
  for (;;)
    {
      _cpp_lex_token (pfile, &tok);
      if (tok.type == CPP_EOF)
	break;
      if (tok.type != CPP_NAME)
	{
	  cpp_error (pfile, "invalid #pragma GCC poison directive");
	  break;
	}

      hp = tok.val.node;
      if (hp->flags & NODE_POISONED)
	continue;

      if (hp->type == NT_MACRO)
	cpp_warning (pfile, "poisoning existing macro \"%s\"", NODE_NAME (hp));
      _cpp_free_definition (hp);
      hp->flags |= NODE_POISONED | NODE_DIAGNOSTIC;
    }
  pfile->state.poisoned_ok = 0;

#if 0				/* Doesn't quite work yet.  */
  if (tok.type == CPP_EOF && pfile->cb.poison)
    (*pfile->cb.poison) (pfile);
#endif
}

/* Mark the current header as a system header.  This will suppress
   some categories of warnings (notably those from -pedantic).  It is
   intended for use in system libraries that cannot be implemented in
   conforming C, but cannot be certain that their headers appear in a
   system include directory.  To prevent abuse, it is rejected in the
   primary source file.  */
static void
do_pragma_system_header (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *buffer = pfile->buffer;

  if (buffer->prev == 0)
    cpp_warning (pfile, "#pragma system_header ignored outside include file");
  else
    cpp_make_system_header (pfile, 1, 0);

  check_eol (pfile);
}

/* Check the modified date of the current include file against a specified
   file. Issue a diagnostic, if the specified file is newer. We use this to
   determine if a fixed header should be refixed.  */
static void
do_pragma_dependency (pfile)
     cpp_reader *pfile;
{
  cpp_token header, msg;
  int ordering;
 
  if (parse_include (pfile, &header))
    return;

  ordering = _cpp_compare_file_date (pfile, &header);
  if (ordering < 0)
    cpp_warning (pfile, "cannot find source %s",
		 cpp_token_as_text (pfile, &header));
  else if (ordering > 0)
    {
      cpp_warning (pfile, "current file is older than %s",
		   cpp_token_as_text (pfile, &header));
      cpp_start_lookahead (pfile);
      cpp_get_token (pfile, &msg);
      cpp_stop_lookahead (pfile, msg.type == CPP_EOF);
      if (msg.type != CPP_EOF)
	do_diagnostic (pfile, WARNING, 0);
    }
}

/* Check syntax is "(string-literal)".  Returns 0 on success.  */
static int
get__Pragma_string (pfile, string)
     cpp_reader *pfile;
     cpp_token *string;
{
  cpp_token paren;

  cpp_get_token (pfile, &paren);
  if (paren.type != CPP_OPEN_PAREN)
    return 1;

  cpp_get_token (pfile, string);
  if (string->type != CPP_STRING && string->type != CPP_WSTRING)
    return 1;

  cpp_get_token (pfile, &paren);
  return paren.type != CPP_CLOSE_PAREN;
}

/* Returns a malloced buffer containing a destringized cpp_string by
   removing the first \ of \" and \\ sequences.  */
static unsigned char *
destringize (in, len)
     const cpp_string *in;
     unsigned int *len;
{
  const unsigned char *src, *limit;
  unsigned char *dest, *result;

  dest = result = (unsigned char *) xmalloc (in->len);
  for (src = in->text, limit = src + in->len; src < limit;)
    {
      /* We know there is a character following the backslash.  */
      if (*src == '\\' && (src[1] == '\\' || src[1] == '"'))
	src++;
      *dest++ = *src++;
    }

  *len = dest - result;
  return result;
}

void
_cpp_do__Pragma (pfile)
     cpp_reader *pfile;
{
  cpp_token string;
  unsigned char *buffer;
  unsigned int len;

  if (get__Pragma_string (pfile, &string))
    {
      cpp_error (pfile, "_Pragma takes a parenthesized string literal");
      return;
    }

  buffer = destringize (&string.val.str, &len);
  run_directive (pfile, T_PRAGMA, BUF_PRAGMA, (char *) buffer, len);
  free ((PTR) buffer);
}

/* Just ignore #sccs, on systems where we define it at all.  */
#ifdef SCCS_DIRECTIVE
static void
do_sccs (pfile)
     cpp_reader *pfile ATTRIBUTE_UNUSED;
{
}
#endif

static void
do_ifdef (pfile)
     cpp_reader *pfile;
{
  int skip = 1;

  if (! pfile->buffer->was_skipping)
    {
      const cpp_hashnode *node = lex_macro_node (pfile);

      if (node)
	skip = node->type != NT_MACRO;

      if (node)
	check_eol (pfile);
    }

  push_conditional (pfile, skip, T_IFDEF, 0);
}

static void
do_ifndef (pfile)
     cpp_reader *pfile;
{
  int skip = 1;
  const cpp_hashnode *node = 0;

  if (! pfile->buffer->was_skipping)
    {
      node = lex_macro_node (pfile);
      if (node)
	skip = node->type == NT_MACRO;

      if (node)
	check_eol (pfile);
    }

  push_conditional (pfile, skip, T_IFNDEF, node);
}

/* #if cooperates with parse_defined to handle multiple-include
   optimisations.  If macro expansions or identifiers appear in the
   expression, we cannot treat it as a controlling conditional, since
   their values could change in the future.  */

static void
do_if (pfile)
     cpp_reader *pfile;
{
  int skip = 1;
  const cpp_hashnode *cmacro = 0;

  if (! pfile->buffer->was_skipping)
    {
      /* Controlling macro of #if ! defined ()  */
      pfile->mi_ind_cmacro = 0;
      skip = _cpp_parse_expr (pfile) == 0;
      cmacro = pfile->mi_ind_cmacro;
    }

  push_conditional (pfile, skip, T_IF, cmacro);
}

/* Flip skipping state if appropriate and continue without changing
   if_stack; this is so that the error message for missing #endif's
   etc. will point to the original #if.  */

static void
do_else (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *buffer = pfile->buffer;
  struct if_stack *ifs = buffer->if_stack;

  if (ifs == NULL)
    cpp_error (pfile, "#else without #if");
  else
    {
      if (ifs->type == T_ELSE)
	{
	  cpp_error (pfile, "#else after #else");
	  cpp_error_with_line (pfile, ifs->pos.line, ifs->pos.col,
			       "the conditional began here");
	}
      ifs->type = T_ELSE;

      /* Buffer->was_skipping is 1 if all conditionals in this chain
	 have been false, 2 if a conditional has been true.  */
      if (! ifs->was_skipping && buffer->was_skipping != 2)
	buffer->was_skipping = ! buffer->was_skipping;

      /* Invalidate any controlling macro.  */
      ifs->mi_cmacro = 0;
    }

  check_eol (pfile);
}

/* handle a #elif directive by not changing if_stack either.  see the
   comment above do_else.  */

static void
do_elif (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *buffer = pfile->buffer;
  struct if_stack *ifs = buffer->if_stack;

  if (ifs == NULL)
    cpp_error (pfile, "#elif without #if");
  else
    {
      if (ifs->type == T_ELSE)
	{
	  cpp_error (pfile, "#elif after #else");
	  cpp_error_with_line (pfile, ifs->pos.line, ifs->pos.col,
			       "the conditional began here");
	}
      ifs->type = T_ELIF;

      /* Don't evaluate #elif if our higher level is skipping.  */
      if (! ifs->was_skipping)
	{
	  /* Buffer->was_skipping is 1 if all conditionals in this
	     chain have been false, 2 if a conditional has been true.  */
	  if (buffer->was_skipping == 1)
	    buffer->was_skipping = ! _cpp_parse_expr (pfile);
	  else
	    buffer->was_skipping = 2;

	  /* Invalidate any controlling macro.  */
	  ifs->mi_cmacro = 0;
	}
    }
}

/* #endif pops the if stack and resets pfile->skipping.  */

static void
do_endif (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *buffer = pfile->buffer;
  struct if_stack *ifs = buffer->if_stack;

  if (ifs == NULL)
    cpp_error (pfile, "#endif without #if");
  else
    {
      /* If potential control macro, we go back outside again.  */
      if (ifs->next == 0 && ifs->mi_cmacro)
	{
	  pfile->mi_state = MI_OUTSIDE;
	  pfile->mi_cmacro = ifs->mi_cmacro;
	}

      buffer->if_stack = ifs->next;
      buffer->was_skipping = ifs->was_skipping;
      obstack_free (&pfile->buffer_ob, ifs);
    }

  check_eol (pfile);
}

/* Push an if_stack entry and set pfile->skipping accordingly.
   If this is a #ifndef starting at the beginning of a file,
   CMACRO is the macro name tested by the #ifndef.  */

static void
push_conditional (pfile, skip, type, cmacro)
     cpp_reader *pfile;
     int skip;
     int type;
     const cpp_hashnode *cmacro;
{
  struct if_stack *ifs;
  cpp_buffer *buffer = pfile->buffer;

  ifs = xobnew (&pfile->buffer_ob, struct if_stack);
  ifs->pos = pfile->directive_pos;
  ifs->next = buffer->if_stack;
  ifs->was_skipping = buffer->was_skipping;
  ifs->type = type;
  if (pfile->mi_state == MI_OUTSIDE && pfile->mi_cmacro == 0)
    ifs->mi_cmacro = cmacro;
  else
    ifs->mi_cmacro = 0;

  buffer->was_skipping = skip;
  buffer->if_stack = ifs;
}

/* Read the tokens of the answer into the macro pool.  Only commit the
   memory if we intend it as permanent storage, i.e. the #assert case.
   Returns 0 on success.  */

static int
parse_answer (pfile, answerp, type)
     cpp_reader *pfile;
     struct answer **answerp;
     int type;
{
  cpp_token paren, *token;
  struct answer *answer;

  if (POOL_FRONT (&pfile->macro_pool) + sizeof (struct answer) >
      POOL_LIMIT (&pfile->macro_pool))
    _cpp_next_chunk (&pfile->macro_pool, sizeof (struct answer), 0);
  answer = (struct answer *) POOL_FRONT (&pfile->macro_pool);
  answer->count = 0;

  /* In a conditional, it is legal to not have an open paren.  We
     should save the following token in this case.  */
  if (type == T_IF)
    cpp_start_lookahead (pfile);
  cpp_get_token (pfile, &paren);
  if (type == T_IF)
    cpp_stop_lookahead (pfile, paren.type == CPP_OPEN_PAREN);

  /* If not a paren, see if we're OK.  */
  if (paren.type != CPP_OPEN_PAREN)
    {
      /* In a conditional no answer is a test for any answer.  It
         could be followed by any token.  */
      if (type == T_IF)
	return 0;

      /* #unassert with no answer is valid - it removes all answers.  */
      if (type == T_UNASSERT && paren.type == CPP_EOF)
	return 0;

      cpp_error (pfile, "missing '(' after predicate");
      return 1;
    }

  for (;;)
    {
      token = &answer->first[answer->count];
      /* Check we have room for the token.  */
      if ((unsigned char *) (token + 1) >= POOL_LIMIT (&pfile->macro_pool))
	{
	  _cpp_next_chunk (&pfile->macro_pool, sizeof (cpp_token),
			   (unsigned char **) &answer);
	  token = &answer->first[answer->count];
	}

      cpp_get_token (pfile, token);
      if (token->type == CPP_CLOSE_PAREN)
	break;

      if (token->type == CPP_EOF)
	{
	  cpp_error (pfile, "missing ')' to complete answer");
	  return 1;
	}
      answer->count++;
    }

  if (answer->count == 0)
    {
      cpp_error (pfile, "predicate's answer is empty");
      return 1;
    }

  /* Drop whitespace at start.  */
  answer->first->flags &= ~PREV_WHITE;
  *answerp = answer;

  if (type == T_ASSERT || type == T_UNASSERT)
    check_eol (pfile);
  return 0;
}

/* Parses an assertion, returning a pointer to the hash node of the
   predicate, or 0 on error.  If an answer was supplied, it is placed
   in ANSWERP, otherwise it is set to 0.  */
static cpp_hashnode *
parse_assertion (pfile, answerp, type)
     cpp_reader *pfile;
     struct answer **answerp;
     int type;
{
  cpp_hashnode *result = 0;
  cpp_token predicate;

  /* We don't expand predicates or answers.  */
  pfile->state.prevent_expansion++;

  *answerp = 0;
  cpp_get_token (pfile, &predicate);
  if (predicate.type == CPP_EOF)
    cpp_error (pfile, "assertion without predicate");
  else if (predicate.type != CPP_NAME)
    cpp_error (pfile, "predicate must be an identifier");
  else if (parse_answer (pfile, answerp, type) == 0)
    {
      unsigned int len = NODE_LEN (predicate.val.node);
      unsigned char *sym = alloca (len + 1);

      /* Prefix '#' to get it out of macro namespace.  */
      sym[0] = '#';
      memcpy (sym + 1, NODE_NAME (predicate.val.node), len);
      result = cpp_lookup (pfile, sym, len + 1);
    }

  pfile->state.prevent_expansion--;
  return result;
}

/* Returns a pointer to the pointer to the answer in the answer chain,
   or a pointer to NULL if the answer is not in the chain.  */
static struct answer **
find_answer (node, candidate)
     cpp_hashnode *node;
     const struct answer *candidate;
{
  unsigned int i;
  struct answer **result;

  for (result = &node->value.answers; *result; result = &(*result)->next)
    {
      struct answer *answer = *result;

      if (answer->count == candidate->count)
	{
	  for (i = 0; i < answer->count; i++)
	    if (! _cpp_equiv_tokens (&answer->first[i], &candidate->first[i]))
	      break;

	  if (i == answer->count)
	    break;
	}
    }

  return result;
}

/* Test an assertion within a preprocessor conditional.  Returns
   non-zero on failure, zero on success.  On success, the result of
   the test is written into VALUE.  */
int
_cpp_test_assertion (pfile, value)
     cpp_reader *pfile;
     int *value;
{
  struct answer *answer;
  cpp_hashnode *node;

  node = parse_assertion (pfile, &answer, T_IF);
  if (node)
    *value = (node->type == NT_ASSERTION &&
	      (answer == 0 || *find_answer (node, answer) != 0));

  /* We don't commit the memory for the answer - it's temporary only.  */
  return node == 0;
}

static void
do_assert (pfile)
     cpp_reader *pfile;
{
  struct answer *new_answer;
  cpp_hashnode *node;
  
  node = parse_assertion (pfile, &new_answer, T_ASSERT);
  if (node)
    {
      /* Place the new answer in the answer list.  First check there
         is not a duplicate.  */
      new_answer->next = 0;
      if (node->type == NT_ASSERTION)
	{
	  if (*find_answer (node, new_answer))
	    {
	      cpp_warning (pfile, "\"%s\" re-asserted", NODE_NAME (node) + 1);
	      return;
	    }
	  new_answer->next = node->value.answers;
	}
      node->type = NT_ASSERTION;
      node->value.answers = new_answer;
      POOL_COMMIT (&pfile->macro_pool, (sizeof (struct answer)
					+ (new_answer->count - 1)
					* sizeof (cpp_token)));
    }
}

static void
do_unassert (pfile)
     cpp_reader *pfile;
{
  cpp_hashnode *node;
  struct answer *answer;
  
  node = parse_assertion (pfile, &answer, T_UNASSERT);
  /* It isn't an error to #unassert something that isn't asserted.  */
  if (node && node->type == NT_ASSERTION)
    {
      if (answer)
	{
	  struct answer **p = find_answer (node, answer), *temp;

	  /* Remove the answer from the list.  */
	  temp = *p;
	  if (temp)
	    *p = temp->next;

	  /* Did we free the last answer?  */
	  if (node->value.answers == 0)
	    node->type = NT_VOID;
	}
      else
	_cpp_free_definition (node);
    }

  /* We don't commit the memory for the answer - it's temporary only.  */
}

/* These are for -D, -U, -A.  */

/* Process the string STR as if it appeared as the body of a #define.
   If STR is just an identifier, define it with value 1.
   If STR has anything after the identifier, then it should
   be identifier=definition. */

void
cpp_define (pfile, str)
     cpp_reader *pfile;
     const char *str;
{
  char *buf, *p;
  size_t count;

  /* Copy the entire option so we can modify it. 
     Change the first "=" in the string to a space.  If there is none,
     tack " 1" on the end.  */

  /* Length including the null.  */  
  count = strlen (str);
  buf = (char *) alloca (count + 2);
  memcpy (buf, str, count);

  p = strchr (str, '=');
  if (p)
    buf[p - str] = ' ';
  else
    {
      buf[count++] = ' ';
      buf[count++] = '1';
    }

  run_directive (pfile, T_DEFINE, BUF_CL_OPTION, buf, count);
}

/* Slight variant of the above for use by initialize_builtins.  */
void
_cpp_define_builtin (pfile, str)
     cpp_reader *pfile;
     const char *str;
{
  run_directive (pfile, T_DEFINE, BUF_BUILTIN, str, strlen (str));
}

/* Process MACRO as if it appeared as the body of an #undef.  */
void
cpp_undef (pfile, macro)
     cpp_reader *pfile;
     const char *macro;
{
  run_directive (pfile, T_UNDEF, BUF_CL_OPTION, macro, strlen (macro));
}

/* Process the string STR as if it appeared as the body of a #assert. */
void
cpp_assert (pfile, str)
     cpp_reader *pfile;
     const char *str;
{
  handle_assertion (pfile, str, T_ASSERT);
}

/* Process STR as if it appeared as the body of an #unassert. */
void
cpp_unassert (pfile, str)
     cpp_reader *pfile;
     const char *str;
{
  handle_assertion (pfile, str, T_UNASSERT);
}  

/* Common code for cpp_assert (-A) and cpp_unassert (-A-).  */
static void
handle_assertion (pfile, str, type)
     cpp_reader *pfile;
     const char *str;
     int type;
{
  size_t count = strlen (str);
  const char *p = strchr (str, '=');

  if (p)
    {
      /* Copy the entire option so we can modify it.  Change the first
	 "=" in the string to a '(', and tack a ')' on the end.  */
      char *buf = (char *) alloca (count + 1);

      memcpy (buf, str, count);
      buf[p - str] = '(';
      buf[count++] = ')';
      str = buf;
    }

  run_directive (pfile, type, BUF_CL_OPTION, str, count);
}

/* The number of errors for a given reader.  */
unsigned int
cpp_errors (pfile)
     cpp_reader *pfile;
{
  return pfile->errors;
}

/* The options structure.  */
cpp_options *
cpp_get_options (pfile)
     cpp_reader *pfile;
{
  return &pfile->opts;
}

/* The callbacks structure.  */
cpp_callbacks *
cpp_get_callbacks (pfile)
     cpp_reader *pfile;
{
  return &pfile->cb;
}

/* Copy the given callbacks structure to our own.  */
void
cpp_set_callbacks (pfile, cb)
     cpp_reader *pfile;
     cpp_callbacks *cb;
{
  pfile->cb = *cb;
}

/* Push a new buffer on the buffer stack.  Returns the new buffer; it
   doesn't fail.  It does not generate a file change call back; that
   is the responsibility of the caller.  */
cpp_buffer *
cpp_push_buffer (pfile, buffer, len, type, filename)
     cpp_reader *pfile;
     const U_CHAR *buffer;
     size_t len;
     enum cpp_buffer_type type;
     const char *filename;
{
  cpp_buffer *new = xobnew (&pfile->buffer_ob, cpp_buffer);

  if (type == BUF_FAKE)
    {
      /* A copy of the current buffer, just with a new name and type.  */
      memcpy (new, pfile->buffer, sizeof (cpp_buffer));
      new->type = BUF_FAKE;
    }
  else
    {
      if (type == BUF_BUILTIN)
	filename = _("<builtin>");
      else if (type == BUF_CL_OPTION)
	filename = _("<command line>");
      else if (type == BUF_PRAGMA)
	filename = "<_Pragma>";

      /* Clears, amongst other things, if_stack and mi_cmacro.  */
      memset (new, 0, sizeof (cpp_buffer));

      new->line_base = new->buf = new->cur = buffer;
      new->rlimit = buffer + len;
      new->sysp = 0;

      /* No read ahead or extra char initially.  */
      new->read_ahead = EOF;
      new->extra_char = EOF;

      /* Preprocessed files, builtins, _Pragma and command line
	 options don't do trigraph and escaped newline processing.  */
      new->from_stage3 = type != BUF_FILE || CPP_OPTION (pfile, preprocessed);

      pfile->lexer_pos.output_line = 1;
    }

  if (*filename == '\0')
    new->nominal_fname = _("<stdin>");
  else
    new->nominal_fname = filename;
  new->type = type;
  new->prev = pfile->buffer;
  new->pfile = pfile;
  new->include_stack_listed = 0;
  new->lineno = 1;

  pfile->state.next_bol = 1;
  pfile->buffer_stack_depth++;
  pfile->buffer = new;

  return new;
}

/* If called from do_line, pops a single buffer.  Otherwise pops all
   buffers until a real file is reached.  Generates appropriate
   call-backs.  */
cpp_buffer *
cpp_pop_buffer (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *buffer;
  struct if_stack *ifs;

  for (;;)
    {
      buffer = pfile->buffer;
      /* Walk back up the conditional stack till we reach its level at
	 entry to this file, issuing error messages.  */
      for (ifs = buffer->if_stack; ifs; ifs = ifs->next)
	cpp_error_with_line (pfile, ifs->pos.line, ifs->pos.col,
			     "unterminated #%s", dtable[ifs->type].name);

      if (buffer->type == BUF_FAKE)
	buffer->prev->cur = buffer->cur;
      else if (buffer->type == BUF_FILE)
	_cpp_pop_file_buffer (pfile, buffer);

      pfile->buffer = buffer->prev;
      pfile->buffer_stack_depth--;

      /* Callbacks only generated for faked or real files.  */
      if (buffer->type != BUF_FILE && buffer->type != BUF_FAKE)
	break;
	  
      /* No callback for EOF of last file.  */
      if (!pfile->buffer)
	break;

      /* do_line does its own call backs.  */
      pfile->buffer->include_stack_listed = 0;
      if (pfile->directive == &dtable[T_LINE])
	break;

      _cpp_do_file_change (pfile, FC_LEAVE, buffer->nominal_fname,
			   buffer->lineno);
      if (pfile->buffer->type == BUF_FILE)
	break;

      cpp_warning (pfile, "file \"%s\" entered but not left",
		   buffer->nominal_fname);
    }

  obstack_free (&pfile->buffer_ob, buffer);
  return pfile->buffer;
}

void
_cpp_init_directives (pfile)
     cpp_reader *pfile;
{
  unsigned int i;
  cpp_hashnode *node;

  /* Register the directives.  */
  for (i = 0; i < (unsigned int) N_DIRECTIVES; i++)
    {
      node = cpp_lookup (pfile, dtable[i].name, dtable[i].length);
      node->directive_index = i + 1;
    }
}
