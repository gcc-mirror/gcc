/* CPP Library.
   Copyright (C) 1986, 1987, 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000 Free Software Foundation, Inc.
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
#include "hashtab.h"
#include "intl.h"

#define PEEKN(N) (CPP_BUFFER (pfile)->rlimit - CPP_BUFFER (pfile)->cur >= (N) \
		  ? CPP_BUFFER (pfile)->cur[N] : EOF)
#define FORWARD(N) CPP_FORWARD (CPP_BUFFER (pfile), (N))
#define GETC() CPP_BUF_GET (CPP_BUFFER (pfile))
#define PEEKC() CPP_BUF_PEEK (CPP_BUFFER (pfile))

/* `struct directive' defines one #-directive, including how to handle it.  */

struct directive
{
  unsigned int length;		/* Length of name */
  const char *name;		/* Name of directive */
  int (*func)			/* Function to handle directive */
    PARAMS ((cpp_reader *));
};

/* Stack of conditionals currently in progress
   (including both successful and failing conditionals).  */

struct if_stack
{
  struct if_stack *next;
  int lineno;			/* line number where condition started */
  int if_succeeded;		/* truth of last condition in this group */
  const U_CHAR *control_macro;	/* macro name for #ifndef around entire file */
  int type;			/* type of last directive seen in this group */
};
typedef struct if_stack IF_STACK;

/* Forward declarations.  */

static void validate_else		PARAMS ((cpp_reader *, const char *));
static int parse_ifdef			PARAMS ((cpp_reader *, const char *));
static unsigned int parse_include	PARAMS ((cpp_reader *, const char *));
static void conditional_skip		PARAMS ((cpp_reader *, int, int,
						 U_CHAR *));
static void skip_if_group		PARAMS ((cpp_reader *));
static void pass_thru_directive		PARAMS ((const U_CHAR *, size_t,
						 cpp_reader *, int));
static int read_line_number		PARAMS ((cpp_reader *, int *));
static U_CHAR *detect_if_not_defined	PARAMS ((cpp_reader *));
static int consider_directive_while_skipping
					PARAMS ((cpp_reader *, IF_STACK *));
static int get_macro_name		PARAMS ((cpp_reader *));

/* This is the table of directive handlers.  It is ordered by
   frequency of occurrence; the numbers at the end are directive
   counts from all the source code I have lying around (egcs and libc
   CVS as of 1999-05-18, plus grub-0.5.91, linux-2.2.9, and
   pcmcia-cs-3.0.9).

   The entries with a dash and a name after the count are extensions,
   of which all but #warning and #include_next are deprecated.  The name
   is where the extension appears to have come from.  */

#define DIRECTIVE_TABLE							\
D("define",	  do_define,	   T_DEFINE)		/* 270554 */	\
D("include",	  do_include,	   T_INCLUDE)		/*  52262 */	\
D("endif",	  do_endif,	   T_ENDIF)		/*  45855 */	\
D("ifdef",	  do_ifdef,	   T_IFDEF)		/*  22000 */	\
D("if",		  do_if,	   T_IF)		/*  18162 */	\
D("else",	  do_else,	   T_ELSE)		/*   9863 */	\
D("ifndef",	  do_ifndef,	   T_IFNDEF)		/*   9675 */	\
D("undef",	  do_undef,	   T_UNDEF)		/*   4837 */	\
D("line",	  do_line,	   T_LINE)		/*   2465 */	\
D("elif",	  do_elif,	   T_ELIF)		/*    610 */	\
D("error",	  do_error,	   T_ERROR)		/*    475 */	\
D("pragma",	  do_pragma,	   T_PRAGMA)		/*    195 */	\
D("warning",	  do_warning,	   T_WARNING)		/*     22 - GNU   */ \
D("include_next", do_include_next, T_INCLUDE_NEXT)	/*     19 - GNU   */ \
D("ident",	  do_ident,	   T_IDENT)		/*     11 - SVR4  */ \
D("import",	  do_import,	   T_IMPORT)		/*      0 - ObjC  */ \
D("assert",	  do_assert,	   T_ASSERT)		/*      0 - SVR4  */ \
D("unassert",	  do_unassert,	   T_UNASSERT)		/*      0 - SVR4  */ \
D("sccs",	  do_sccs,	   T_SCCS)		/*      0 - SVR2? */

/* Use the table to generate a series of prototypes, an enum for the
   directive names, and an array of directive handlers.  */

/* The directive-processing functions are declared to return int
   instead of void, because some old compilers have trouble with
   pointers to functions returning void.  */

#define D(name, fun, tag) static int fun PARAMS ((cpp_reader *));
DIRECTIVE_TABLE
#undef D

#define D(name, fun, tag) tag,
enum
{
  DIRECTIVE_TABLE
  N_DIRECTIVES
};
#undef D

#define D(name, fun, tag) { sizeof name - 1, name, fun },
static const struct directive dtable[] =
{
DIRECTIVE_TABLE
};
#undef D
#undef DIRECTIVE_TABLE

/* Handle a possible # directive.
   '#' has already been read.  */

int
_cpp_handle_directive (pfile)
     cpp_reader *pfile;
{
  int c, i;
  unsigned int len;
  U_CHAR *ident;
  long old_written = CPP_WRITTEN (pfile);

  if (CPP_IS_MACRO_BUFFER (CPP_BUFFER (pfile)))
    {
      cpp_ice (pfile, "handle_directive called on macro buffer");
      return 0;
    }

  _cpp_skip_hspace (pfile);

  c = PEEKC ();
  /* # followed by a number is equivalent to #line.  Do not recognize
     this form in assembly language source files.  Complain about this
     form if we're being pedantic, but not if this is regurgitated
     input (preprocessed or fed back in by the C++ frontend).  */
  if (c >= '0' && c <= '9')
    {
      if (CPP_OPTIONS (pfile)->lang_asm)
	return 0;

      if (CPP_PEDANTIC (pfile)
	  && ! CPP_OPTIONS (pfile)->preprocessed
	  && ! CPP_BUFFER (pfile)->manual_pop)
	cpp_pedwarn (pfile, "`#' followed by integer");
      do_line (pfile);
      return 1;
    }

  /* If we are rescanning preprocessed input, don't obey any directives
     other than # nnn.  */
  if (CPP_OPTIONS (pfile)->preprocessed)
    return 0;

  /* Now find the directive name.  */
  CPP_PUTC (pfile, '#');
  _cpp_parse_name (pfile, GETC());
  ident = pfile->token_buffer + old_written + 1;
  len = CPP_PWRITTEN (pfile) - ident;
  if (len == 0)
    {
      /* A line of just `#' becomes blank.  A line with something
	 other than an identifier after the # is reparsed as a non-
	 directive line.  */
      CPP_SET_WRITTEN (pfile, old_written);
      return (PEEKC() == '\n');
    }

  /* Decode the keyword and call the appropriate expansion routine.  */
  for (i = 0; i < N_DIRECTIVES; i++)
    {
      if (dtable[i].length == len
	  && !strncmp (dtable[i].name, ident, len)) 
	break;
    }
  if (i == N_DIRECTIVES)
    /* # identifier, but not a legit directive.  Pass onward as a
       CPP_DIRECTIVE token anyway - let the consumer worry about it.  */
    return 1;

  CPP_SET_WRITTEN (pfile, old_written);

  if (pfile->no_directives)
    {
      cpp_error (pfile, "`#%s' may not be used inside a macro argument",
		 dtable[i].name);
      _cpp_skip_rest_of_line (pfile);
    }
  else
    (*dtable[i].func) (pfile);

  return 1;
}

/* Pass a directive through to the output file.
   BUF points to the contents of the directive, as a contiguous string.
   LEN is the length of the string pointed to by BUF.
   KEYWORD is the keyword-table entry for the directive.  */

static void
pass_thru_directive (buf, len, pfile, keyword)
     const U_CHAR *buf;
     size_t len;
     cpp_reader *pfile;
     int keyword;
{
  const struct directive *kt = &dtable[keyword];
  register unsigned klen = kt->length;

  CPP_RESERVE (pfile, 1 + klen + len);
  CPP_PUTC_Q (pfile, '#');
  CPP_PUTS_Q (pfile, kt->name, klen);
  if (len != 0 && buf[0] != ' ')
    CPP_PUTC_Q (pfile, ' ');
  CPP_PUTS_Q (pfile, buf, len);
}

/* Subroutine of do_define: determine the name of the macro to be
   defined.  */

static int
get_macro_name (pfile)
     cpp_reader *pfile;
{
  long here, len;

  here = CPP_WRITTEN (pfile);
  pfile->no_macro_expand++;
  if (_cpp_get_directive_token (pfile) != CPP_NAME)
    {
      cpp_error (pfile, "`#define' must be followed by an identifier");
      goto invalid;
    }

  len = CPP_WRITTEN (pfile) - here;
  if (len == 7 && !strncmp (pfile->token_buffer + here, "defined", 7))
    {
      cpp_error (pfile, "`defined' is not a legal macro name");
      goto invalid;
    }

  pfile->no_macro_expand--;
  return len;

 invalid:
  _cpp_skip_rest_of_line (pfile);
  pfile->no_macro_expand--;
  return 0;
}

/* Process a #define command.  */

static int
do_define (pfile)
     cpp_reader *pfile;
{
  HASHNODE **slot;
  DEFINITION *def;
  long here;
  unsigned long hash;
  int len, c;
  int funlike = 0;
  U_CHAR *sym;

  here = CPP_WRITTEN (pfile);
  len = get_macro_name (pfile);
  if (len == 0)
    return 0;

  /* Copy out the name so we can pop the token buffer.  */
  len = CPP_WRITTEN (pfile) - here;
  sym = (U_CHAR *) alloca (len + 1);
  memcpy (sym, pfile->token_buffer + here, len);
  sym[len] = '\0';
  CPP_SET_WRITTEN (pfile, here);

  /* If the next character, with no intervening whitespace, is '(',
     then this is a function-like macro.  */
  c = PEEKC ();
  if (c == '(')
    funlike = 1;
  else if (c != '\n' && !is_hspace (c))
    /* Otherwise, C99 requires white space after the name.  We treat it
       as an object-like macro if this happens, with a warning.  */
    cpp_pedwarn (pfile, "missing white space after `#define %.*s'", len, sym);

  def = _cpp_create_definition (pfile, funlike);
  if (def == 0)
    return 0;

  slot = _cpp_lookup_slot (pfile, sym, len, 1, &hash);
  if (*slot)
    {
      int ok;
      HASHNODE *hp = *slot;

      /* Redefining a macro is ok if the definitions are the same.  */
      if (hp->type == T_MACRO)
	ok = ! _cpp_compare_defs (pfile, def, hp->value.defn);
      /* Redefining a constant is ok with -D.  */
      else if (hp->type == T_CONST || hp->type == T_STDC)
        ok = ! CPP_OPTIONS (pfile)->done_initializing;
      /* Otherwise it's not ok.  */
      else
	ok = 0;
      /* Print the warning or error if it's not ok.  */
      if (! ok)
	{
	  if (hp->type == T_POISON)
	    cpp_error (pfile, "redefining poisoned `%.*s'", len, sym);
	  else
	    cpp_pedwarn (pfile, "`%.*s' redefined", len, sym);
	  if (hp->type == T_MACRO && CPP_OPTIONS (pfile)->done_initializing)
	    {
	      DEFINITION *d = hp->value.defn;
	      cpp_pedwarn_with_file_and_line (pfile, d->file, d->line, d->col,
			"this is the location of the previous definition");
	    }
	}
      if (hp->type != T_POISON)
	{
	  /* Replace the old definition.  */
	  if (hp->type == T_MACRO)
	    _cpp_free_definition (hp->value.defn);
	  hp->type = T_MACRO;
	  hp->value.defn = def;
	}
    }
  else
    {
      HASHNODE *hp = _cpp_make_hashnode (sym, len, T_MACRO, hash);
      hp->value.defn = def;
      *slot = hp;
    }

  if (CPP_OPTIONS (pfile)->debug_output
      || CPP_OPTIONS (pfile)->dump_macros == dump_definitions)
    _cpp_dump_definition (pfile, sym, len, def);
  else if (CPP_OPTIONS (pfile)->dump_macros == dump_names)
    pass_thru_directive (sym, len, pfile, T_DEFINE);

  return 0;
}

/*
 * write out a #line command, for instance, after an #include file.
 * FILE_CHANGE says whether we are entering a file, leaving, or neither.
 */

void
_cpp_output_line_command (pfile, file_change)
     cpp_reader *pfile;
     enum file_change_code file_change;
{
  long line;
  cpp_buffer *ip;

  if (CPP_OPTIONS (pfile)->no_line_commands
      || CPP_OPTIONS (pfile)->no_output)
    return;

  ip = cpp_file_buffer (pfile);
  cpp_buf_line_and_col (ip, &line, NULL);

  /* If the current file has not changed, we omit the #line if it would
     appear to be a no-op, and we output a few newlines instead
     if we want to increase the line number by a small amount.
     We cannot do this if pfile->lineno is zero, because that means we
     haven't output any line commands yet.  (The very first line command
     output is a `same_file' command.)  */
  if (file_change == same_file && pfile->lineno != 0)
    {
      if (line == pfile->lineno)
	return;

      /* If the inherited line number is a little too small,
	 output some newlines instead of a #line command.  */
      if (line > pfile->lineno && line < pfile->lineno + 8)
	{
	  CPP_RESERVE (pfile, 20);
	  while (line > pfile->lineno)
	    {
	      CPP_PUTC_Q (pfile, '\n');
	      pfile->lineno++;
	    }
	  return;
	}
    }

  CPP_RESERVE (pfile, 4 * strlen (ip->nominal_fname) + 50);
  CPP_PUTS_Q (pfile, "# ", 2);

  sprintf ((char *) CPP_PWRITTEN (pfile), "%ld ", line);
  CPP_ADJUST_WRITTEN (pfile, strlen (CPP_PWRITTEN (pfile)));

  _cpp_quote_string (pfile, ip->nominal_fname); 
  if (file_change != same_file && file_change != rename_file)
    {
      CPP_PUTC_Q (pfile, ' ');
      CPP_PUTC_Q (pfile, file_change == enter_file ? '1' : '2');
    }
  /* Tell cc1 if following text comes from a system header file.  */
  if (ip->system_header_p)
    {
      CPP_PUTC_Q (pfile, ' ');
      CPP_PUTC_Q (pfile, '3');
    }
#ifndef NO_IMPLICIT_EXTERN_C
  /* Tell cc1plus if following text should be treated as C.  */
  if (ip->system_header_p == 2 && CPP_OPTIONS (pfile)->cplusplus)
    {
      CPP_PUTC_Q (pfile, ' ');
      CPP_PUTC_Q (pfile, '4');
    }
#endif
  CPP_PUTC_Q (pfile, '\n');
  pfile->lineno = line;
}

/* Handle #include and #import.  */

static unsigned int
parse_include (pfile, name)
     cpp_reader *pfile;
     const char *name;
{
  long old_written = CPP_WRITTEN (pfile);
  enum cpp_token token;
  int len;

  pfile->parsing_include_directive++;
  token = _cpp_get_directive_token (pfile);
  pfile->parsing_include_directive--;

  len = CPP_WRITTEN (pfile) - old_written;

  if (token == CPP_STRING)
    ; /* No special treatment required.  */
#ifdef VMS
  else if (token == CPP_NAME)
    {
      /* Support '#include xyz' like VAX-C.  It is taken as
         '#include <xyz.h>' and generates a warning.  */
      cpp_warning (pfile, "#%s filename is obsolete, use #%s <filename.h>",
		   name, name);

      /* Rewrite the token to <xyz.h>.  */
      CPP_RESERVE (pfile, 4);
      len += 4;
      memmove (pfile->token_buffer + old_written + 1,
	       pfile->token_buffer + old_written,
	       CPP_WRITTEN (pfile) - old_written);
      pfile->token_buffer[old_written] = '<';
      CPP_PUTS_Q (pfile, ".h>", 2);
    }
#endif
  else
    {
      cpp_error (pfile, "`#%s' expects \"FILENAME\" or <FILENAME>", name);
      CPP_SET_WRITTEN (pfile, old_written);
      _cpp_skip_rest_of_line (pfile);
      return 0;
    }

  CPP_NUL_TERMINATE (pfile);
  CPP_ADJUST_WRITTEN (pfile, 1);

  if (_cpp_get_directive_token (pfile) != CPP_VSPACE)
    {
      cpp_error (pfile, "junk at end of `#%s'", name);
      _cpp_skip_rest_of_line (pfile);
    }

  CPP_SET_WRITTEN (pfile, old_written);

  if (len == 0)
    cpp_error (pfile, "empty file name in `#%s'", name);

  return len;
}

static int
do_include (pfile)
     cpp_reader *pfile;
{
  unsigned int len;
  char *token;

  len = parse_include (pfile, dtable[T_INCLUDE].name);
  if (len == 0)
    return 0;
  token = alloca (len + 1);
  strcpy (token, CPP_PWRITTEN (pfile));
  
  if (CPP_OPTIONS (pfile)->dump_includes)
    pass_thru_directive (token, len, pfile, T_INCLUDE);

  _cpp_execute_include (pfile, token, len, 0, 0);
  return 0;
}

static int
do_import (pfile)
     cpp_reader *pfile;
{
  unsigned int len;
  char *token;

  if (CPP_PEDANTIC (pfile))
    cpp_pedwarn (pfile, "ANSI C does not allow `#import'");

  if (CPP_OPTIONS (pfile)->warn_import
      && !CPP_BUFFER (pfile)->system_header_p && !pfile->import_warning)
    {
      pfile->import_warning = 1;
      cpp_warning (pfile,
	   "#import is obsolete, use an #ifndef wrapper in the header file");
    }

  len = parse_include (pfile, dtable[T_IMPORT].name);
  if (len == 0)
    return 0;
  token = alloca (len + 1);
  strcpy (token, CPP_PWRITTEN (pfile));
  
  if (CPP_OPTIONS (pfile)->dump_includes)
    pass_thru_directive (token, len, pfile, T_IMPORT);

  _cpp_execute_include (pfile, token, len, 1, 0);
  return 0;
}

static int
do_include_next (pfile)
     cpp_reader *pfile;
{
  unsigned int len;
  char *token;
  struct file_name_list *search_start = 0;

  if (CPP_PEDANTIC (pfile))
    cpp_pedwarn (pfile, "ANSI C does not allow `#include_next'");
  
  len = parse_include (pfile, dtable[T_INCLUDE_NEXT].name);
  if (len == 0)
    return 0;
  token = alloca (len + 1);
  strcpy (token, CPP_PWRITTEN (pfile));
  
  if (CPP_OPTIONS (pfile)->dump_includes)
    pass_thru_directive (token, len, pfile, T_INCLUDE_NEXT);

  /* For #include_next, skip in the search path past the dir in which the
     containing file was found.  Treat files specified using an absolute path
     as if there are no more directories to search.  Treat the primary source
     file like any other included source, but generate a warning.  */
  if (CPP_PREV_BUFFER (CPP_BUFFER (pfile)))
    {
      if (CPP_BUFFER (pfile)->ihash->foundhere != ABSOLUTE_PATH)
	search_start = CPP_BUFFER (pfile)->ihash->foundhere->next;
    }
  else
    cpp_warning (pfile, "#include_next in primary source file");

  _cpp_execute_include (pfile, token, len, 0, search_start);
  return 0;
}

/* Subroutine of do_line.  Read next token from PFILE without adding it to
   the output buffer.  If it is a number between 1 and 4, store it in *NUM
   and return 1; otherwise, return 0 and complain if we aren't at the end
   of the directive.  */

static int
read_line_number (pfile, num)
     cpp_reader *pfile;
     int *num;
{
  long save_written = CPP_WRITTEN (pfile);
  U_CHAR *p;
  enum cpp_token token = _cpp_get_directive_token (pfile);
  CPP_SET_WRITTEN (pfile, save_written);
  p = pfile->token_buffer + save_written;

  if (token == CPP_NUMBER && *p >= '1' && *p <= '4' && p[1] == '\0')
    {
      *num = p[0] - '0';
      return 1;
    }
  else
    {
      if (token != CPP_VSPACE && token != CPP_EOF)
	cpp_error (pfile, "invalid format `#line' command");
      return 0;
    }
}

/* Interpret #line command.
   Note that the filename string (if any) is treated as if it were an
   include filename.  That means no escape handling.  */

static int
do_line (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *ip = CPP_BUFFER (pfile);
  int new_lineno;
  long old_written = CPP_WRITTEN (pfile);
  enum file_change_code file_change = same_file;
  enum cpp_token token;
  char *x;

  token = _cpp_get_directive_token (pfile);

  if (token != CPP_NUMBER)
    {
      cpp_error (pfile, "token after `#line' is not an integer");
      goto bad_line_directive;
    }

  new_lineno = strtol (pfile->token_buffer + old_written, &x, 10);
  if (x[0] != '\0')
    {
      cpp_error (pfile, "token after `#line' is not an integer");
      goto bad_line_directive;
    }      
  CPP_SET_WRITTEN (pfile, old_written);

  if (CPP_PEDANTIC (pfile) && (new_lineno <= 0 || new_lineno > 32767))
    cpp_pedwarn (pfile, "line number out of range in `#line' command");

  token = _cpp_get_directive_token (pfile);

  if (token == CPP_STRING)
    {
      U_CHAR *fname = pfile->token_buffer + old_written + 1;
      U_CHAR *end_name = CPP_PWRITTEN (pfile) - 1;
      int action_number = 0;

      file_change = rename_file;

      if (read_line_number (pfile, &action_number))
	{
	  if (CPP_PEDANTIC (pfile))
	    cpp_pedwarn (pfile, "garbage at end of `#line' command");

	  if (action_number == 1)
	    {
	      file_change = enter_file;
	      read_line_number (pfile, &action_number);
	    }
	  else if (action_number == 2)
	    {
	      file_change = leave_file;
	      read_line_number (pfile, &action_number);
	    }
	  if (action_number == 3)
	    {
	      ip->system_header_p = 1;
	      read_line_number (pfile, &action_number);
	    }
	  if (action_number == 4)
	    {
	      ip->system_header_p = 2;
	      read_line_number (pfile, &action_number);
	    }
	}
      
      *end_name = '\0';
      
      if (strcmp (fname, ip->nominal_fname))
	{
	  const char *newname, *oldname;
	  if (!strcmp (fname, ip->ihash->name))
	    newname = ip->ihash->name;
	  else if (ip->last_nominal_fname
		   && !strcmp (fname, ip->last_nominal_fname))
	    newname = ip->last_nominal_fname;
	  else
	    newname = xstrdup (fname);

	  oldname = ip->nominal_fname;
	  ip->nominal_fname = newname;

	  if (ip->last_nominal_fname
	      && ip->last_nominal_fname != oldname
	      && ip->last_nominal_fname != newname
	      && ip->last_nominal_fname != ip->ihash->name)
	    free ((void *) ip->last_nominal_fname);

	  if (newname == ip->ihash->name)
	    ip->last_nominal_fname = NULL;
	  else
	    ip->last_nominal_fname = oldname;
	} 
    }
  else if (token != CPP_VSPACE && token != CPP_EOF)
    {
      cpp_error (pfile, "token after `#line %d' is not a string", new_lineno);
      goto bad_line_directive;
    }

  /* The Newline at the end of this line remains to be processed.
     To put the next line at the specified line number,
     we must store a line number now that is one less.  */
  ip->lineno = new_lineno - 1;
  CPP_SET_WRITTEN (pfile, old_written);
  _cpp_output_line_command (pfile, file_change);
  return 0;

 bad_line_directive:
  _cpp_skip_rest_of_line (pfile);
  CPP_SET_WRITTEN (pfile, old_written);
  return 0;
}

/* Remove the definition of a symbol from the symbol table.
   According to the C standard, it is not an error to undef
   something that has no definitions. */
static int
do_undef (pfile)
     cpp_reader *pfile;
{
  int len;
  HASHNODE **slot;
  U_CHAR *buf, *name, *limit;
  int c;
  long here = CPP_WRITTEN (pfile);
  enum cpp_token token;

  _cpp_skip_hspace (pfile);
  c = GETC();
  if (! is_idstart(c))
  {
      cpp_error (pfile, "token after #undef is not an identifier");
      _cpp_skip_rest_of_line (pfile);
      return 1;
  }

  _cpp_parse_name (pfile, c);
  buf = pfile->token_buffer + here;
  limit = CPP_PWRITTEN(pfile);

  /* Copy out the token so we can pop the token buffer. */
  len = limit - buf;
  name = (U_CHAR *) alloca (len + 1);
  memcpy (name, buf, len);
  name[len] = '\0';

  token = _cpp_get_directive_token (pfile);
  if (token != CPP_VSPACE)
  {
      cpp_pedwarn (pfile, "junk on line after #undef");
      _cpp_skip_rest_of_line (pfile);
  }
  CPP_SET_WRITTEN (pfile, here);

  slot = _cpp_lookup_slot (pfile, name, len, 0, 0);
  if (slot)
    {
      HASHNODE *hp = *slot;
      /* If we are generating additional info for debugging (with -g) we
	 need to pass through all effective #undef commands.  */
      if (CPP_OPTIONS (pfile)->debug_output)
	pass_thru_directive (name, len, pfile, T_UNDEF);
      if (hp->type == T_POISON)
	cpp_error (pfile, "cannot undefine poisoned `%s'", hp->name);
      else 
	{
	  if (hp->type != T_MACRO)
	    cpp_warning (pfile, "undefining `%s'", hp->name);

	  htab_clear_slot (pfile->hashtab, (void **)slot);
	}
    }

  return 0;
}

/*
 * Report an error detected by the program we are processing.
 * Use the text of the line in the error message.
 * (We use error because it prints the filename & line#.)
 */

static int
do_error (pfile)
     cpp_reader *pfile;
{
  const U_CHAR *text, *limit;

  _cpp_skip_hspace (pfile);
  text = CPP_BUFFER (pfile)->cur;
  _cpp_skip_rest_of_line (pfile);
  limit = CPP_BUFFER (pfile)->cur;

  cpp_error (pfile, "#error %.*s", (int)(limit - text), text);
  return 0;
}

/*
 * Report a warning detected by the program we are processing.
 * Use the text of the line in the warning message, then continue.
 */

static int
do_warning (pfile)
     cpp_reader *pfile;
{
  const U_CHAR *text, *limit;

  _cpp_skip_hspace (pfile);
  text = CPP_BUFFER (pfile)->cur;
  _cpp_skip_rest_of_line (pfile);
  limit = CPP_BUFFER (pfile)->cur;

  if (CPP_PEDANTIC (pfile))
    cpp_pedwarn (pfile, "ANSI C does not allow `#warning'");

  cpp_warning (pfile, "#warning %.*s", (int)(limit - text), text);
  return 0;
}

/* Report program identification.  */

static int
do_ident (pfile)
     cpp_reader *pfile;
{
  long old_written = CPP_WRITTEN (pfile);

  /* Allow #ident in system headers, since that's not user's fault.  */
  if (CPP_PEDANTIC (pfile))
    cpp_pedwarn (pfile, "ANSI C does not allow `#ident'");

  CPP_PUTS (pfile, "#ident ", 7);

  /* Next token should be a string constant.  */
  if (_cpp_get_directive_token (pfile) == CPP_STRING)
    /* And then a newline.  */
    if (_cpp_get_directive_token (pfile) == CPP_VSPACE)
      /* Good - ship it.  */
      return 0;

  cpp_error (pfile, "invalid #ident");
  _cpp_skip_rest_of_line (pfile);
  CPP_SET_WRITTEN (pfile, old_written);  /* discard directive */

  return 0;
}

/* Pragmata handling.  We handle some of these, and pass the rest on
   to the front end.  C99 defines three pragmas and says that no macro
   expansion is to be performed on them; whether or not macro
   expansion happens for other pragmas is implementation defined.
   This implementation never macro-expands the text after #pragma.

   We currently do not support the _Pragma operator.  Support for that
   has to be coordinated with the front end.  Proposed implementation:
   both #pragma blah blah and _Pragma("blah blah") become
   __builtin_pragma(blah blah) and we teach the parser about that.  */

/* Sub-handlers for the pragmas needing treatment here.
   They return 1 if the token buffer is to be popped, 0 if not. */
static int do_pragma_once		PARAMS ((cpp_reader *));
static int do_pragma_implementation	PARAMS ((cpp_reader *));
static int do_pragma_poison		PARAMS ((cpp_reader *));
static int do_pragma_default		PARAMS ((cpp_reader *));

static int
do_pragma (pfile)
     cpp_reader *pfile;
{
  long here, key;
  U_CHAR *buf;
  int pop;
  enum cpp_token token;

  here = CPP_WRITTEN (pfile);
  CPP_PUTS (pfile, "#pragma ", 8);

  key = CPP_WRITTEN (pfile);
  pfile->no_macro_expand++;
  token = _cpp_get_directive_token (pfile);
  if (token != CPP_NAME)
    {
      if (token == CPP_VSPACE)
	goto empty;
      else
	goto skip;
    }

  buf = pfile->token_buffer + key;
  CPP_PUTC (pfile, ' ');

#define tokis(x) !strncmp((char *) buf, x, sizeof(x) - 1)
  if (tokis ("once"))
    pop = do_pragma_once (pfile);
  else if (tokis ("implementation"))
    pop = do_pragma_implementation (pfile);
  else if (tokis ("poison"))
    pop = do_pragma_poison (pfile);
  else
    pop = do_pragma_default (pfile);
#undef tokis

  if (_cpp_get_directive_token (pfile) != CPP_VSPACE)
    goto skip;

  if (pop)
    CPP_SET_WRITTEN (pfile, here);
  pfile->no_macro_expand--;
  return 0;

 skip:
  cpp_error (pfile, "malformed #pragma directive");
  _cpp_skip_rest_of_line (pfile);
 empty:
  CPP_SET_WRITTEN (pfile, here);
  pfile->no_macro_expand--;
  return 0;
}

static int
do_pragma_default (pfile)
     cpp_reader *pfile;
{
  while (_cpp_get_directive_token (pfile) != CPP_VSPACE)
    CPP_PUTC (pfile, ' ');
  return 0;
}

static int
do_pragma_once (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *ip = CPP_BUFFER (pfile);

  /* Allow #pragma once in system headers, since that's not the user's
     fault.  */
  if (!ip->system_header_p)
    cpp_warning (pfile, "`#pragma once' is obsolete");
      
  if (CPP_PREV_BUFFER (ip) == NULL)
    cpp_warning (pfile, "`#pragma once' outside include file");
  else
    ip->ihash->control_macro = (const U_CHAR *) "";  /* never repeat */

  return 1;
}

static int
do_pragma_implementation (pfile)
     cpp_reader *pfile;
{
  /* Be quiet about `#pragma implementation' for a file only if it hasn't
     been included yet.  */
  enum cpp_token token;
  long written = CPP_WRITTEN (pfile);
  U_CHAR *name;
  U_CHAR *copy;
  size_t len;

  token = _cpp_get_directive_token (pfile);
  if (token == CPP_VSPACE)
    return 0;
  else if (token != CPP_STRING)
    {
      cpp_error (pfile, "malformed #pragma implementation");
      return 1;
    }

  /* Trim the leading and trailing quote marks from the string.  */
  name = pfile->token_buffer + written + 1;
  len = CPP_PWRITTEN (pfile) - name;
  copy = (U_CHAR *) alloca (len);
  memcpy (copy, name, len - 1);
  copy[len - 1] = '\0';
  
  if (cpp_included (pfile, copy))
    cpp_warning (pfile,
	 "`#pragma implementation' for `%s' appears after file is included",
		 copy);
  return 0;
}

static int
do_pragma_poison (pfile)
     cpp_reader *pfile;
{
  /* Poison these symbols so that all subsequent usage produces an
     error message.  */
  U_CHAR *p;
  HASHNODE **slot;
  long written;
  size_t len;
  enum cpp_token token;
  int writeit;
  unsigned long hash;

  /* As a rule, don't include #pragma poison commands in output,  
     unless the user asks for them.  */
  writeit = (CPP_OPTIONS (pfile)->debug_output
	     || CPP_OPTIONS (pfile)->dump_macros == dump_definitions
	     || CPP_OPTIONS (pfile)->dump_macros == dump_names);

  for (;;)
    {
      written = CPP_WRITTEN (pfile);
      token = _cpp_get_directive_token (pfile);
      if (token == CPP_VSPACE)
	break;
      if (token != CPP_NAME)
	{
	  cpp_error (pfile, "invalid #pragma poison directive");
	  _cpp_skip_rest_of_line (pfile);
	  return 1;
	}

      p = pfile->token_buffer + written;
      len = strlen (p);
      slot = _cpp_lookup_slot (pfile, p, len, 1, &hash);
      if (*slot)
	{
	  HASHNODE *hp = *slot;
	  if (hp->type != T_POISON)
	    {
	      cpp_warning (pfile, "poisoning existing macro `%s'", p);
	      if (hp->type == T_MACRO)
		_cpp_free_definition (hp->value.defn);
	      hp->value.defn = 0;
	      hp->type = T_POISON;
	    }
	}
      else
	{
	  HASHNODE *hp = _cpp_make_hashnode (p, len, T_POISON, hash);
	  hp->value.cpval = 0;
	  *slot = hp;
	}
      if (writeit)
	CPP_PUTC (pfile, ' ');
    }
  return !writeit;
}
 
/* Just ignore #sccs, on systems where we define it at all.  */
static int
do_sccs (pfile)
     cpp_reader *pfile;
{
#ifdef SCCS_DIRECTIVE
  if (CPP_PEDANTIC (pfile))
    cpp_pedwarn (pfile, "ANSI C does not allow `#sccs'");
#else
  cpp_error (pfile, "undefined or invalid # directive `sccs'");
#endif
  _cpp_skip_rest_of_line (pfile);
  return 0;
}


/* We've found an `#if' directive.  If the only thing before it in
   this file is white space, and if it is of the form
   `#if ! defined SYMBOL', then SYMBOL is a possible controlling macro
   for inclusion of this file.  (See redundant_include_p in cppfiles.c
   for an explanation of controlling macros.)  If so, return a
   malloc'd copy of SYMBOL.  Otherwise, return NULL.  */

static U_CHAR *
detect_if_not_defined (pfile)
     cpp_reader *pfile;
{
  U_CHAR *control_macro = 0;

  if (pfile->only_seen_white == 2)
    {
      U_CHAR *ident;
      enum cpp_token token;
      int base_offset;
      int token_offset;
      int need_rparen = 0;

      /* Save state required for restore.  */
      pfile->no_macro_expand++;
      CPP_SET_MARK (pfile);
      base_offset = CPP_WRITTEN (pfile);

      /* Look for `!', */
      if (_cpp_get_directive_token (pfile) != CPP_OTHER
	  || CPP_WRITTEN (pfile) != (size_t) base_offset + 1
	  || CPP_PWRITTEN (pfile)[-1] != '!')
	goto restore;

      /* ...then `defined', */
      token_offset = CPP_WRITTEN (pfile);
      token = _cpp_get_directive_token (pfile);
      if (token != CPP_NAME)
	goto restore;
      ident = pfile->token_buffer + token_offset;
      CPP_NUL_TERMINATE (pfile);
      if (strcmp (ident, "defined"))
	goto restore;

      /* ...then an optional '(' and the name, */
      token_offset = CPP_WRITTEN (pfile);
      token = _cpp_get_directive_token (pfile);
      if (token == CPP_LPAREN)
	{
	  token_offset = CPP_WRITTEN (pfile);
	  token = _cpp_get_directive_token (pfile);
	  if (token != CPP_NAME)
	    goto restore;
	  need_rparen = 1;
	}
      else if (token != CPP_NAME)
	goto restore;

      ident = pfile->token_buffer + token_offset;
      CPP_NUL_TERMINATE (pfile);

      /* ...then the ')', if necessary, */
      if ((!need_rparen || _cpp_get_directive_token (pfile) == CPP_RPAREN)
	  /* ...and make sure there's nothing else on the line.  */
	  && _cpp_get_directive_token (pfile) == CPP_VSPACE)
	control_macro = (U_CHAR *) xstrdup (ident);

    restore:
      CPP_SET_WRITTEN (pfile, base_offset);
      pfile->no_macro_expand--;
      CPP_GOTO_MARK (pfile);
    }

  return control_macro;
}

/*
 * #if is straightforward; just call _cpp_parse_expr, then conditional_skip.
 * Also, check for a reinclude preventer of the form #if !defined (MACRO).
 */

static int
do_if (pfile)
     cpp_reader *pfile;
{
  U_CHAR *control_macro = detect_if_not_defined (pfile);
  int value = _cpp_parse_expr (pfile);
  conditional_skip (pfile, value == 0, T_IF, control_macro);
  return 0;
}

/*
 * handle a #elif directive by not changing  if_stack  either.
 * see the comment above do_else.
 */

static int
do_elif (pfile)
     cpp_reader *pfile;
{
  if (pfile->if_stack == CPP_BUFFER (pfile)->if_stack)
    {
      cpp_error (pfile, "`#elif' not within a conditional");
      return 0;
    }
  else
    {
      if (pfile->if_stack->type == T_ELSE)
	{
	  cpp_error (pfile, "`#elif' after `#else'");
	  cpp_error_with_line (pfile, pfile->if_stack->lineno, -1,
			       "the conditional began here");
	}
      pfile->if_stack->type = T_ELIF;
    }

  if (pfile->if_stack->if_succeeded)
    skip_if_group (pfile);
  else
    {
      if (_cpp_parse_expr (pfile) == 0)
	skip_if_group (pfile);
      else
	{
	  ++pfile->if_stack->if_succeeded;	/* continue processing input */
	  _cpp_output_line_command (pfile, same_file);
	}
    }
  return 0;
}

/* Parse an #ifdef or #ifndef directive.  Returns 1 for defined, 0 for
   not defined; the macro tested is left in the token buffer (but
   popped).  */

static int
parse_ifdef (pfile, name)
     cpp_reader *pfile;
     const char *name;
{
  U_CHAR *ident;
  unsigned int len;
  enum cpp_token token;
  long old_written = CPP_WRITTEN (pfile);
  int defined;

  pfile->no_macro_expand++;
  token = _cpp_get_directive_token (pfile);
  pfile->no_macro_expand--;

  ident = pfile->token_buffer + old_written;
  len = CPP_WRITTEN (pfile) - old_written;

  if (token == CPP_VSPACE)
    {
      if (! CPP_TRADITIONAL (pfile))
	cpp_pedwarn (pfile, "`#%s' with no argument", name);
      defined = 0;
      goto done;
    }
  else if (token == CPP_NAME)
    {
      defined = cpp_defined (pfile, ident, len);
      CPP_NUL_TERMINATE (pfile);
      CPP_ADJUST_WRITTEN (pfile, 1);
    }
  else
    {
      defined = 0;
      if (! CPP_TRADITIONAL (pfile))
	cpp_error (pfile, "`#%s' with invalid argument", name);
    }

  if (!CPP_TRADITIONAL (pfile))
    {
      if (_cpp_get_directive_token (pfile) == CPP_VSPACE)
	goto done;
      
      cpp_pedwarn (pfile, "garbage at end of `#%s' argument", name);
    }
  _cpp_skip_rest_of_line (pfile);
  
 done:
  CPP_SET_WRITTEN (pfile, old_written); /* Pop */
  return defined;
}

/* #ifdef is dead simple.  */

static int
do_ifdef (pfile)
     cpp_reader *pfile;
{
  int skip = ! parse_ifdef (pfile, dtable[T_IFDEF].name);
  conditional_skip (pfile, skip, T_IFDEF, 0);
  return 0;
}

/* #ifndef is a tad more complex, because we need to check for a
   no-reinclusion wrapper.  */

static int
do_ifndef (pfile)
     cpp_reader *pfile;
{
  int start_of_file, skip;
  U_CHAR *control_macro = 0;

  start_of_file = pfile->only_seen_white == 2;
  skip = parse_ifdef (pfile, dtable[T_IFNDEF].name);

  if (start_of_file && !skip)
    control_macro = xstrdup (CPP_PWRITTEN (pfile));

  conditional_skip (pfile, skip, T_IFNDEF, control_macro);
  return 0;
}

/* Push TYPE on stack; then, if SKIP is nonzero, skip ahead.
   If this is a #ifndef starting at the beginning of a file,
   CONTROL_MACRO is the macro name tested by the #ifndef.
   Otherwise, CONTROL_MACRO is 0.  */

static void
conditional_skip (pfile, skip, type, control_macro)
     cpp_reader *pfile;
     int skip;
     int type;
     U_CHAR *control_macro;
{
  IF_STACK *temp;

  temp = (IF_STACK *) xcalloc (1, sizeof (IF_STACK));
  temp->lineno = CPP_BUFFER (pfile)->lineno;
  temp->next = pfile->if_stack;
  temp->control_macro = control_macro;
  pfile->if_stack = temp;

  pfile->if_stack->type = type;

  if (skip != 0) {
    skip_if_group (pfile);
    return;
  } else {
    ++pfile->if_stack->if_succeeded;
    _cpp_output_line_command (pfile, same_file);
  }
}

/* Subroutine of skip_if_group.	 Examine one preprocessing directive and
   return 0 if skipping should continue, 1 if it should halt.  Also
   adjusts the if_stack as appropriate.
   The `#' has been read, but not the identifier. */

static int
consider_directive_while_skipping (pfile, stack)
    cpp_reader *pfile;
    IF_STACK *stack; 
{
  long ident;
  const struct directive *kt;
  int i;
  unsigned int len;
  IF_STACK *temp;
    
  _cpp_skip_hspace (pfile);

  ident = CPP_WRITTEN (pfile);
  _cpp_parse_name (pfile, GETC());
  len = CPP_WRITTEN (pfile) - ident;

  CPP_SET_WRITTEN (pfile, ident);

  for (i = 0; i < N_DIRECTIVES; i++)
    {
      kt = &dtable[i];
      if (kt->length == len
	  && strncmp (pfile->token_buffer + ident, kt->name, kt->length) == 0)
	switch (i)
	  {
	  case T_IF:
	  case T_IFDEF:
	  case T_IFNDEF:
	    temp = (IF_STACK *) xmalloc (sizeof (IF_STACK));
	    temp->next = pfile->if_stack;
	    pfile->if_stack = temp;
	    temp->type = i;
	    return 0;

	  case T_ELSE:
	    if (pfile->if_stack != stack)
	      validate_else (pfile, dtable[i].name);
	    /* fall through */
	  case T_ELIF:
	    if (pfile->if_stack == stack)
	      return 1;
	    else
	      {
		pfile->if_stack->type = i;
		return 0;
	      }

	  case T_ENDIF:
	    if (pfile->if_stack != stack)
	      validate_else (pfile, dtable[i].name);

	    if (pfile->if_stack == stack)
	      return 1;
		    
	    temp = pfile->if_stack;
	    pfile->if_stack = temp->next;
	    free (temp);
	    return 0;

	  default:
	    return 0;
	  }
    }

    /* Don't let erroneous code go by.	*/
    if (!CPP_OPTIONS (pfile)->lang_asm && CPP_PEDANTIC (pfile))
	cpp_pedwarn (pfile, "invalid preprocessor directive name");
    return 0;
}

/* skip to #endif, #else, or #elif.  adjust line numbers, etc.
 * leaves input ptr at the sharp sign found.
 */
static void
skip_if_group (pfile)
    cpp_reader *pfile;
{
  int c;
  IF_STACK *save_if_stack = pfile->if_stack; /* don't pop past here */
  const U_CHAR *beg_of_line;
  long old_written;

  old_written = CPP_WRITTEN (pfile);
  
  for (;;)
    {
      beg_of_line = CPP_BUFFER (pfile)->cur;

      if (! CPP_TRADITIONAL (pfile))
	_cpp_skip_hspace (pfile);
      c = GETC();
      if (c == '\n')
	{
	  CPP_BUMP_LINE (pfile);
	  continue;
	}
      else if (c == '#')
	{
	  if (consider_directive_while_skipping (pfile, save_if_stack))
	    break;
	}
      else if (c == EOF)
	return;	 /* Caller will issue error. */

      FORWARD(-1);
      _cpp_skip_rest_of_line (pfile);

      c = GETC();
      if (c == EOF)
	return;	 /* Caller will issue error. */
      else
	CPP_BUMP_LINE (pfile);
    }	  

  /* Back up to the beginning of this line.  Caller will process the
     directive. */
  CPP_BUFFER (pfile)->cur = beg_of_line;
  pfile->only_seen_white = 1;
}

/*
 * handle a #else directive.  Do this by just continuing processing
 * without changing  if_stack ;  this is so that the error message
 * for missing #endif's etc. will point to the original #if.  It
 * is possible that something different would be better.
 */

static int
do_else (pfile)
     cpp_reader *pfile;
{
  validate_else (pfile, dtable[T_ELSE].name);
  _cpp_skip_rest_of_line (pfile);

  if (pfile->if_stack == CPP_BUFFER (pfile)->if_stack)
    {
      cpp_error (pfile, "`#else' not within a conditional");
      return 0;
    }
  else
    {
      /* #ifndef can't have its special treatment for containing the whole file
	 if it has a #else clause.  */
      pfile->if_stack->control_macro = 0;

      if (pfile->if_stack->type == T_ELSE)
	{
	  cpp_error (pfile, "`#else' after `#else'");
	  cpp_error_with_line (pfile, pfile->if_stack->lineno, -1,
			       "the conditional began here");
	}
      pfile->if_stack->type = T_ELSE;
    }

  if (pfile->if_stack->if_succeeded)
    skip_if_group (pfile);
  else
    {
      ++pfile->if_stack->if_succeeded;	/* continue processing input */
      _cpp_output_line_command (pfile, same_file);
    }
  return 0;
}

/*
 * unstack after #endif command
 */

static int
do_endif (pfile)
     cpp_reader *pfile;
{
  validate_else (pfile, dtable[T_ENDIF].name);
  _cpp_skip_rest_of_line (pfile);

  if (pfile->if_stack == CPP_BUFFER (pfile)->if_stack)
    cpp_error (pfile, "`#endif' not within a conditional");
  else
    {
      IF_STACK *temp = pfile->if_stack;
      pfile->if_stack = temp->next;
      if (temp->control_macro != 0)
	{
	  /* This #endif matched a #ifndef at the start of the file.
	     See if it is at the end of the file.  */
	  int c;

	  CPP_SET_MARK (pfile);

	  for (;;)
	    {
	      _cpp_skip_hspace (pfile);
	      c = GETC ();
	      if (c != '\n')
		break;
	    }
	  CPP_GOTO_MARK (pfile);

	  if (c == EOF)
	    {
	      /* This #endif ends a #ifndef
		 that contains all of the file (aside from whitespace).
		 Arrange not to include the file again
		 if the macro that was tested is defined. */
	      CPP_BUFFER (pfile)->ihash->control_macro = temp->control_macro;
	    }
        }
      free (temp);
      _cpp_output_line_command (pfile, same_file);
    }
  return 0;
}

/* Issue -pedantic warning for text which is not a comment following
   an #else or #endif.  Do not warn in system headers, as this is harmless
   and very common on old systems.  */

static void
validate_else (pfile, directive)
     cpp_reader *pfile;
     const char *directive;
{
  if (! CPP_PEDANTIC (pfile))
    return;

  _cpp_skip_hspace (pfile);
  if (PEEKC () != '\n')
    cpp_pedwarn (pfile,
		 "text following `#%s' violates ANSI standard", directive);
}

void
_cpp_handle_eof (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *next_buf = CPP_PREV_BUFFER (CPP_BUFFER (pfile));
  struct if_stack *ifs, *nifs;

  /* Unwind the conditional stack and generate error messages.  */
  for (ifs = pfile->if_stack;
       ifs != CPP_BUFFER (pfile)->if_stack;
       ifs = nifs)
    {
      cpp_error_with_line (pfile, ifs->lineno, -1,
			   "unterminated `#%s' conditional",
			   dtable[ifs->type].name);

      nifs = ifs->next;
      free (ifs);
    }
  pfile->if_stack = ifs;

  if (CPP_BUFFER (pfile)->nominal_fname && next_buf != NULL)
    {
      /* We're about to return from an #include file.
	 Emit #line information now (as part of the CPP_POP) result.
	 But the #line refers to the file we will pop to.  */
      cpp_buffer *cur_buffer = CPP_BUFFER (pfile);
      CPP_BUFFER (pfile) = next_buf;
      pfile->input_stack_listing_current = 0;
      _cpp_output_line_command (pfile, leave_file);
      CPP_BUFFER (pfile) = cur_buffer;
    }

  CPP_BUFFER (pfile)->seen_eof = 1;
}

static int
do_assert (pfile)
     cpp_reader *pfile;
{
  U_CHAR *sym;
  int ret, c;
  HASHNODE *base, *this;
  HASHNODE **bslot, **tslot;
  size_t blen, tlen;
  unsigned long bhash, thash;

  if (CPP_PEDANTIC (pfile) && CPP_OPTIONS (pfile)->done_initializing)
    cpp_pedwarn (pfile, "ANSI C does not allow `#assert'");

  _cpp_skip_hspace (pfile);
  sym = CPP_PWRITTEN (pfile);	/* remember where it starts */
  ret = _cpp_parse_assertion (pfile);
  if (ret == 0)
    goto error;
  else if (ret == 1)
    {
      cpp_error (pfile, "missing token-sequence in `#assert'");
      goto error;
    }

  _cpp_skip_hspace (pfile);
  c = PEEKC();
  if (c != EOF && c != '\n')
    {
      cpp_error (pfile, "junk at end of `#assert'");
      goto error;
    }

  tlen = strlen (sym);
  blen = (U_CHAR *) strchr (sym, '(') - sym;
  tslot = _cpp_lookup_slot (pfile, sym, tlen, 1, &thash);
  if (*tslot)
    {
      cpp_warning (pfile, "`%s' re-asserted", sym);
      goto error;
    }

  bslot = _cpp_lookup_slot (pfile, sym, blen, 1, &bhash);
  if (! *bslot)
    *bslot = base = _cpp_make_hashnode (sym, blen, T_ASSERT, bhash);
  else
    {
      base = *bslot;
      if (base->type != T_ASSERT)
	{
	  /* Token clash - but with what?! */
	  cpp_ice (pfile, "base->type != T_ASSERT in do_assert");
	  goto error;
	}
    }
  *tslot = this = _cpp_make_hashnode (sym, tlen, T_ASSERT, thash);
  this->value.aschain = base->value.aschain;
  base->value.aschain = this;
  
  pfile->limit = sym;		/* Pop */
  return 0;

 error:
  _cpp_skip_rest_of_line (pfile);
  pfile->limit = sym;		/* Pop */
  return 0;
}

static int
do_unassert (pfile)
     cpp_reader *pfile;
{
  int c, ret;
  U_CHAR *sym;
  long baselen, thislen;
  HASHNODE *base, *this, *next;
  
  if (CPP_PEDANTIC (pfile) && CPP_OPTIONS (pfile)->done_initializing)
    cpp_pedwarn (pfile, "ANSI C does not allow `#unassert'");

  _cpp_skip_hspace (pfile);

  sym = CPP_PWRITTEN (pfile);	/* remember where it starts */
  ret = _cpp_parse_assertion (pfile);
  if (ret == 0)
    goto error;
  
  _cpp_skip_hspace (pfile);
  c = PEEKC ();
  if (c != EOF && c != '\n')
      cpp_error (pfile, "junk at end of `#unassert'");

  thislen = strlen (sym);
  if (ret == 1)
    {
      base = _cpp_lookup (pfile, sym, thislen);
      if (! base)
	goto error;  /* It isn't an error to #undef what isn't #defined,
			so it isn't an error to #unassert what isn't
			#asserted either. */
      
      for (this = base->value.aschain; this; this = next)
        {
	  next = this->value.aschain;
	  htab_remove_elt (pfile->hashtab, this);
	}
      htab_remove_elt (pfile->hashtab, base);
    }
  else
    {
      baselen = (U_CHAR *) strchr (sym, '(') - sym;
      base = _cpp_lookup (pfile, sym, baselen);
      if (! base) goto error;
      this = _cpp_lookup (pfile, sym, thislen);
      if (! this) goto error;

      next = base;
      while (next->value.aschain != this)
	next = next->value.aschain;

      next->value.aschain = this->value.aschain;
      htab_remove_elt (pfile->hashtab, this);

      if (base->value.aschain == NULL)
	/* Last answer for this predicate deleted. */
	htab_remove_elt (pfile->hashtab, base);
    }
  
  pfile->limit = sym;		/* Pop */
  return 0;
 error:
  _cpp_skip_rest_of_line (pfile);
  pfile->limit = sym;		/* Pop */
  return 0;
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

  p = strchr (str, '=');
  /* Copy the entire option so we can modify it. 
     Change the first "=" in the string to a space.  If there is none,
     tack " 1" on the end.  Then add a newline and a NUL.  */
  
  if (p)
    {
      count = strlen (str) + 2;
      buf = alloca (count);
      memcpy (buf, str, count - 2);
      buf[p - str] = ' ';
      buf[count - 2] = '\n';
      buf[count - 1] = '\0';
    }
  else
    {
      count = strlen (str) + 4;
      buf = alloca (count);
      memcpy (buf, str, count - 4);
      strcpy (&buf[count-4], " 1\n");
    }

  if (cpp_push_buffer (pfile, buf, count - 1) != NULL)
    {
      do_define (pfile);
      cpp_pop_buffer (pfile);
    }
}

/* Process MACRO as if it appeared as the body of an #undef.  */
void
cpp_undef (pfile, macro)
     cpp_reader *pfile;
     const char *macro;
{
  /* Copy the string so we can append a newline.  */
  size_t len = strlen (macro);
  char *buf = alloca (len + 2);
  memcpy (buf, macro, len);
  buf[len]     = '\n';
  buf[len + 1] = '\0';
  if (cpp_push_buffer (pfile, buf, len + 1))
    {
      do_undef (pfile);
      cpp_pop_buffer (pfile);
    }
}

/* Process the string STR as if it appeared as the body of a #assert. */
void
cpp_assert (pfile, str)
     cpp_reader *pfile;
     const char *str;
{
  if (cpp_push_buffer (pfile, str, strlen (str)) != NULL)
    {
      do_assert (pfile);
      cpp_pop_buffer (pfile);
    }
}

/* Process STR as if it appeared as the body of an #unassert. */
void
cpp_unassert (pfile, str)
     cpp_reader *pfile;
     const char *str;
{
  if (cpp_push_buffer (pfile, str, strlen (str)) != NULL)
    {
      do_unassert (pfile);
      cpp_pop_buffer (pfile);
    }
}  

/* Determine whether the identifier ID, of length LEN, is a defined macro.  */
int
cpp_defined (pfile, id, len)
     cpp_reader *pfile;
     const U_CHAR *id;
     int len;
{
  HASHNODE *hp = _cpp_lookup (pfile, id, len);
  if (hp && hp->type == T_POISON)
    {
      cpp_error (pfile, "attempt to use poisoned `%s'", hp->name);
      return 0;
    }
  return (hp != NULL);
}
