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
#include "intl.h"
#include "obstack.h"
#include "symcat.h"

/* Stack of conditionals currently in progress
   (including both successful and failing conditionals).  */

struct if_stack
{
  struct if_stack *next;
  unsigned int lineno;		/* line number where condition started */
  unsigned int colno;		/* and column */
  int was_skipping;		/* value of pfile->skipping before this if */
  const cpp_hashnode *cmacro;	/* macro name for #ifndef around entire file */
  int type;			/* type of last directive seen in this group */
};

/* Forward declarations.  */

static void validate_else	PARAMS ((cpp_reader *, const U_CHAR *));
static int  parse_include	PARAMS ((cpp_reader *, const U_CHAR *, int,
					 const U_CHAR **, unsigned int *,
					 int *));
static void push_conditional	PARAMS ((cpp_reader *, int, int,
					 const cpp_hashnode *));
static int  read_line_number	PARAMS ((cpp_reader *, int *));
static int  strtoul_for_line	PARAMS ((const U_CHAR *, unsigned int,
					 unsigned long *));

static const cpp_hashnode *
	    parse_ifdef		PARAMS ((cpp_reader *, const U_CHAR *));
static const cpp_hashnode *
	    detect_if_not_defined PARAMS ((cpp_reader *));
static cpp_hashnode *
	    get_define_node	PARAMS ((cpp_reader *));
static void unwind_if_stack	PARAMS ((cpp_reader *, cpp_buffer *));

/* This is the table of directive handlers.  It is ordered by
   frequency of occurrence; the numbers at the end are directive
   counts from all the source code I have lying around (egcs and libc
   CVS as of 1999-05-18, plus grub-0.5.91, linux-2.2.9, and
   pcmcia-cs-3.0.9).

   The entries with a dash and a name after the count are extensions,
   of which all but #warning and #include_next are deprecated.  The name
   is where the extension appears to have come from.  */

/* #sccs is not always recognized.  */
#ifdef SCCS_DIRECTIVE
# define SCCS_ENTRY D(sccs, T_SCCS, EXTENSION, 0)
#else
# define SCCS_ENTRY /* nothing */
#endif

#define DIRECTIVE_TABLE							\
D(define,	T_DEFINE = 0,	KANDR,     COMMENTS | IN_I)/* 270554 */ \
D(include,	T_INCLUDE,	KANDR,     EXPAND | INCL)  /*  52262 */ \
D(endif,	T_ENDIF,	KANDR,     COND)	   /*  45855 */ \
D(ifdef,	T_IFDEF,	KANDR,     COND)	   /*  22000 */ \
D(if,		T_IF,		KANDR,     COND | EXPAND)  /*  18162 */ \
D(else,		T_ELSE,		KANDR,     COND)	   /*   9863 */ \
D(ifndef,	T_IFNDEF,	KANDR,     COND)	   /*   9675 */ \
D(undef,	T_UNDEF,	KANDR,     IN_I)	   /*   4837 */ \
D(line,		T_LINE,		KANDR,     EXPAND)    	   /*   2465 */ \
D(elif,		T_ELIF,		KANDR,     COND | EXPAND)  /*    610 */ \
D(error,	T_ERROR,	STDC89,    0)		   /*    475 */ \
D(pragma,	T_PRAGMA,	STDC89,    IN_I)	   /*    195 */ \
D(warning,	T_WARNING,	EXTENSION, 0)		   /*     22 GNU   */ \
D(include_next,	T_INCLUDE_NEXT,	EXTENSION, EXPAND | INCL)  /*     19 GNU   */ \
D(ident,	T_IDENT,	EXTENSION, IN_I)	   /*     11 SVR4  */ \
D(import,	T_IMPORT,	EXTENSION, EXPAND | INCL)  /*      0 ObjC  */ \
D(assert,	T_ASSERT,	EXTENSION, 0)  		   /*      0 SVR4  */ \
D(unassert,	T_UNASSERT,	EXTENSION, 0)  		   /*      0 SVR4  */ \
SCCS_ENTRY						   /*      0 SVR2? */

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
static const struct directive dtable[] =
{
DIRECTIVE_TABLE
};
#undef D
#undef DIRECTIVE_TABLE

/* Check if a token's name matches that of a known directive.  Put in
   this file to save exporting dtable and other unneeded information.  */
const struct directive *
_cpp_check_directive (pfile, token)
     cpp_reader *pfile;
     const cpp_token *token;
{
  unsigned int i;

  if (token->type != CPP_NAME)
    {
      if (token->type == CPP_EOF && CPP_WTRADITIONAL (pfile)
	  && pfile->state.indented)
	cpp_warning (pfile, "traditional C ignores #\\n with the # indented");

      return 0;
    }

  for (i = 0; i < N_DIRECTIVES; i++)
    if (pfile->spec_nodes->dirs[i] == token->val.node)
      break;

  if (i == N_DIRECTIVES)
    return 0;

  /* We should lex headers correctly, regardless of whether we're
     skipping or not.  */
  pfile->state.angled_headers = dtable[i].flags & INCL;

  /* If we are rescanning preprocessed input, only directives tagged
     with IN_I are honored, and the warnings below are suppressed.  */
  if (CPP_OPTION (pfile, preprocessed))
    {
      if (!dtable[i].flags & IN_I)
	return 0;
    }
  else
    {
      /* Traditionally, a directive is ignored unless its # is in
	 column 1.  Therefore in code intended to work with K+R
	 compilers, directives added by C89 must have their #
	 indented, and directives present in traditional C must not.
	 This is true even of directives in skipped conditional
	 blocks.  */
      if (CPP_WTRADITIONAL (pfile))
	{
	  if (pfile->state.indented && dtable[i].origin == KANDR)
	    cpp_warning (pfile, 
			 "traditional C ignores #%s with the # indented",
			 dtable[i].name);

	  else if (!pfile->state.indented && dtable[i].origin != KANDR)
	    cpp_warning (pfile,
		 "suggest hiding #%s from traditional C with an indented #",
			 dtable[i].name);
	}

      /* If we are skipping a failed conditional group, all non-conditional
	 directives are ignored.  */
      if (pfile->skipping && !(dtable[i].flags & COND))
	return 0;

      /* Issue -pedantic warnings for extended directives.   */
      if (CPP_PEDANTIC (pfile) && dtable[i].origin == EXTENSION)
	cpp_pedwarn (pfile, "ISO C does not allow #%s", dtable[i].name);
    }

  /* Only flag to save comments if we process the directive.  */
  pfile->state.save_comments = (! CPP_OPTION (pfile, discard_comments)
				&& (dtable[i].flags & COMMENTS));

  return &dtable[i];
}

const struct directive *
_cpp_check_linemarker (pfile, token)
     cpp_reader *pfile;
     const cpp_token *token ATTRIBUTE_UNUSED;
{
  /* # followed by a number is equivalent to #line.  Do not recognize
     this form in assembly language source files or skipped
     conditional groups.  Complain about this form if we're being
     pedantic, but not if this is regurgitated input (preprocessed or
     fed back in by the C++ frontend).  */
  if (pfile->skipping || CPP_OPTION (pfile, lang_asm))
    return 0;

  if (CPP_PEDANTIC (pfile) && CPP_BUFFER (pfile)->inc
      && ! CPP_OPTION (pfile, preprocessed))
    cpp_pedwarn (pfile, "# followed by integer");

  /* In -traditional mode, a directive is ignored unless its #
     is in column 1.  */
  if (pfile->state.indented && CPP_WTRADITIONAL (pfile))
    cpp_warning (pfile, "traditional C ignores #%s with the # indented",
		 dtable[T_LINE].name);

  return &dtable[T_LINE];
}  

static cpp_hashnode *
get_define_node (pfile)
     cpp_reader *pfile;
{
  const cpp_token *token;

  /* Skip any -C comments.  */
  while ((token = _cpp_get_token (pfile))->type == CPP_COMMENT)
    ;

  /* The token immediately after #define must be an identifier.  That
     identifier is not allowed to be "defined".  See predefined macro
     names (6.10.8.4).  In C++, it is not allowed to be any of the
     <iso646.h> macro names (which are keywords in C++) either.  */

  if (token->type != CPP_NAME)
    {
      if (token->type == CPP_DEFINED)
	cpp_error_with_line (pfile, token->line, token->col,
			     "\"defined\" cannot be used as a macro name");
      else if (token->flags & NAMED_OP)
	cpp_error_with_line (pfile, token->line, token->col,
			     "\"%s\" cannot be used as a macro name in C++",
			     token->val.node->name);
      else
	cpp_error_with_line (pfile, token->line, token->col,
			   "macro names must be identifiers");
      return 0;
    }

  /* In Objective C, some keywords begin with '@', but general identifiers
     do not, and you're not allowed to #define them.  */
  if (token->val.node->name[0] == '@')
    {
      cpp_error_with_line (pfile, token->line, token->col,
			   "\"%s\" cannot be used as a macro name",
			   token->val.node->name);
      return 0;
    }

  /* Check for poisoned identifiers now.  */
  if (token->val.node->type == T_POISON)
    {
      cpp_error_with_line (pfile, token->line, token->col,
			   "attempt to use poisoned \"%s\"",
			   token->val.node->name);
      return 0;
    }

  return token->val.node;
}

/* Process a #define command.  */
static void
do_define (pfile)
     cpp_reader *pfile;
{
  cpp_hashnode *node;

  if ((node = get_define_node (pfile)))
    if (_cpp_create_definition (pfile, node))
      if (pfile->cb.define)
	(*pfile->cb.define) (pfile, node);
}

/* Remove the definition of a symbol from the symbol table.  */
static void
do_undef (pfile)
     cpp_reader *pfile;
{
  cpp_hashnode *node = get_define_node (pfile);  

  if (_cpp_get_token (pfile)->type != CPP_EOF)
    cpp_pedwarn (pfile, "junk on line after #undef");

  /* 6.10.3.5 paragraph 2: [#undef] is ignored if the specified identifier
     is not currently defined as a macro name.  */
  if (node && node->type != T_VOID)
    {
      if (pfile->cb.undef)
	(*pfile->cb.undef) (pfile, node);

      if (node->type != T_MACRO)
	cpp_warning (pfile, "undefining \"%s\"", node->name);

      _cpp_free_definition (node);
    }
}


/* Handle #include and #import.  */

static int
parse_include (pfile, dir, trail, strp, lenp, abp)
     cpp_reader *pfile;
     const U_CHAR *dir;
     int trail;
     const U_CHAR **strp;
     unsigned int *lenp;
     int *abp;
{
  const cpp_token *name = _cpp_get_token (pfile);

  if (name->type != CPP_STRING && name->type != CPP_HEADER_NAME)
    {
      if (name->type == CPP_LESS)
	name = _cpp_glue_header_name (pfile);
      else
	{
	  cpp_error (pfile, "#%s expects \"FILENAME\" or <FILENAME>", dir);
	  return 1;
	}
    }
  if (name->val.str.len == 0)
    {
      cpp_error (pfile, "empty file name in #%s", dir);
      return 1;
    }

  if (!trail && _cpp_get_token (pfile)->type != CPP_EOF)
    cpp_error (pfile, "junk at end of #%s", dir);

  *lenp = name->val.str.len;
  *strp = name->val.str.text;
  *abp = (name->type == CPP_HEADER_NAME);

  if (pfile->cb.include)
    (*pfile->cb.include) (pfile, dir, *strp, *lenp, *abp);
  return 0;
}

static void
do_include (pfile)
     cpp_reader *pfile;
{
  unsigned int len;
  const U_CHAR *str;
  int ab;

  if (parse_include (pfile, dtable[T_INCLUDE].name, 0, &str, &len, &ab))
    return;

  _cpp_execute_include (pfile, str, len, 0, 0, ab);
}

static void
do_import (pfile)
     cpp_reader *pfile;
{
  unsigned int len;
  const U_CHAR *str;
  int ab;

  if (CPP_OPTION (pfile, warn_import)
      && !CPP_IN_SYSTEM_HEADER (pfile) && !pfile->import_warning)
    {
      pfile->import_warning = 1;
      cpp_warning (pfile,
	   "#import is obsolete, use an #ifndef wrapper in the header file");
    }

  if (parse_include (pfile, dtable[T_IMPORT].name, 0, &str, &len, &ab))
    return;

  _cpp_execute_include (pfile, str, len, 1, 0, ab);
}

static void
do_include_next (pfile)
     cpp_reader *pfile;
{
  unsigned int len;
  const U_CHAR *str;
  struct file_name_list *search_start = 0;
  int ab;

  if (parse_include (pfile, dtable[T_INCLUDE_NEXT].name, 0, &str, &len, &ab))
    return;

  /* For #include_next, skip in the search path past the dir in which
     the current file was found.  If this is the last directory in the
     search path, don't include anything.  If the current file was
     specified with an absolute path, use the normal search logic.  If
     this is the primary source file, use the normal search logic and
     generate a warning.  */
  if (CPP_PREV_BUFFER (CPP_BUFFER (pfile)))
    {
      if (CPP_BUFFER (pfile)->inc->foundhere)
	{
	  search_start = CPP_BUFFER (pfile)->inc->foundhere->next;
	  if (!search_start)
	    return;
	}
    }
  else
    cpp_warning (pfile, "#include_next in primary source file");

  _cpp_execute_include (pfile, str, len, 0, search_start, ab);
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
  const cpp_token *tok = _cpp_get_token (pfile);
  enum cpp_ttype type = tok->type;
  const U_CHAR *p = tok->val.str.text;
  unsigned int len = tok->val.str.len;

  if (type == CPP_NUMBER && len == 1 && p[0] >= '1' && p[0] <= '4')
    {
      *num = p[0] - '0';
      return 1;
    }
  else
    {
      if (type != CPP_EOF)
	cpp_error (pfile, "invalid format #line");
      return 0;
    }
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
  cpp_buffer *ip = CPP_BUFFER (pfile);
  unsigned long new_lineno, old_lineno;
  /* C99 raised the minimum limit on #line numbers.  */
  unsigned int cap = CPP_OPTION (pfile, c99) ? 2147483647 : 32767;
  int action_number = 0;
  int enter = 0, leave = 0, rename = 0;
  enum cpp_ttype type;
  const U_CHAR *str;
  char *fname;
  unsigned int len;
  const cpp_token *tok;

  tok = _cpp_get_token (pfile);
  type = tok->type;
  str = tok->val.str.text;
  len = tok->val.str.len;

  if (type != CPP_NUMBER || strtoul_for_line (str, len, &new_lineno))
    {
      cpp_error (pfile, "token after #line is not a positive integer");
      return;
    }      

  if (CPP_PEDANTIC (pfile) && (new_lineno == 0 || new_lineno > cap))
    cpp_pedwarn (pfile, "line number out of range");

  old_lineno = ip->lineno;
  ip->lineno = new_lineno;
  tok = _cpp_get_token (pfile);
  type = tok->type;
  str = tok->val.str.text;
  len = tok->val.str.len;

  if (type == CPP_EOF)
    goto done;
  else if (type != CPP_STRING)
    {
      cpp_error (pfile, "second token after #line is not a string");
      ip->lineno = old_lineno;  /* malformed #line should have no effect */
      return;
    }

  fname = alloca (len + 1);
  memcpy (fname, str, len);
  fname[len] = '\0';
    
  if (strcmp (fname, ip->nominal_fname))
    {
      rename = 1;
      if (!strcmp (fname, ip->inc->name))
	ip->nominal_fname = ip->inc->name;
      else
	ip->nominal_fname = _cpp_fake_include (pfile, fname);
    }

  if (read_line_number (pfile, &action_number) == 0)
    goto done;

  if (CPP_PEDANTIC (pfile))
    cpp_pedwarn (pfile, "garbage at end of #line");

  if (action_number == 1)
    {
      enter = 1;
      cpp_make_system_header (pfile, ip, 0);
      read_line_number (pfile, &action_number);
    }
  else if (action_number == 2)
    {
      leave = 1;
      cpp_make_system_header (pfile, ip, 0);
      read_line_number (pfile, &action_number);
    }
  if (action_number == 3)
    {
      cpp_make_system_header (pfile, ip, 1);
      read_line_number (pfile, &action_number);
    }
  if (action_number == 4)
    {
      cpp_make_system_header (pfile, ip, 2);
      read_line_number (pfile, &action_number);
    }

 done:
  if (enter && pfile->cb.enter_file)
    (*pfile->cb.enter_file) (pfile);
  if (leave && pfile->cb.leave_file)
    (*pfile->cb.leave_file) (pfile);
  if (rename && pfile->cb.rename_file)
    (*pfile->cb.rename_file) (pfile);
}

/*
 * Report an error detected by the program we are processing.
 * Use the text of the line in the error message.
 * (We use error because it prints the filename & line#.)
 */

static void
do_error (pfile)
     cpp_reader *pfile;
{
  if (_cpp_begin_message (pfile, ERROR, NULL, 0, 0))
    {
      cpp_output_list (pfile, stderr, &pfile->token_list,
		       pfile->first_directive_token);
      putc ('\n', stderr);
    }
}

/*
 * Report a warning detected by the program we are processing.
 * Use the text of the line in the warning message, then continue.
 */

static void
do_warning (pfile)
     cpp_reader *pfile;
{
  if (_cpp_begin_message (pfile, WARNING, NULL, 0, 0))
    {
      cpp_output_list (pfile, stderr, &pfile->token_list,
		       pfile->first_directive_token);
      putc ('\n', stderr);
    }
}

/* Report program identification.  */

static void
do_ident (pfile)
     cpp_reader *pfile;
{
  const cpp_token *str = _cpp_get_token (pfile);

  if (str->type == CPP_STRING && _cpp_get_token (pfile)->type == CPP_EOF)
    {
      if (pfile->cb.ident)
	(*pfile->cb.ident) (pfile, str->val.str.text, str->val.str.len);
      return;
    }

  cpp_error (pfile, "invalid #ident");
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
  
static void do_pragma_once		PARAMS ((cpp_reader *));
static void do_pragma_poison		PARAMS ((cpp_reader *));
static void do_pragma_system_header	PARAMS ((cpp_reader *));
static void do_pragma_dependency	PARAMS ((cpp_reader *));

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
  const cpp_token *tok;
  const cpp_hashnode *node;
  const U_CHAR *name;
  size_t len;

  p = pfile->pragmas;

 new_space:
  tok = _cpp_get_token (pfile);
  if (tok->type == CPP_EOF)
    return;

  if (tok->type != CPP_NAME)
    {
      cpp_error (pfile, "malformed #pragma directive");
      return;
    }

  node = tok->val.node;
  name = node->name;
  len = node->length;
  while (p)
    {
      if (strlen (p->name) == len && !memcmp (p->name, name, len))
	{
	  if (p->isnspace)
	    {
	      p = p->u.space;
	      goto new_space;
	    }
	  else
	    {
	      (*p->u.handler) (pfile);
	      return;
	    }
	}
      p = p->next;
    }

  if (pfile->cb.def_pragma)
    (*pfile->cb.def_pragma) (pfile);
}

static void
do_pragma_once (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *ip = CPP_BUFFER (pfile);

  /* Allow #pragma once in system headers, since that's not the user's
     fault.  */
  if (!CPP_IN_SYSTEM_HEADER (pfile))
    cpp_warning (pfile, "#pragma once is obsolete");
      
  if (CPP_PREV_BUFFER (ip) == NULL)
    cpp_warning (pfile, "#pragma once outside include file");
  else
    ip->inc->cmacro = NEVER_REREAD;
}

static void
do_pragma_poison (pfile)
     cpp_reader *pfile;
{
  /* Poison these symbols so that all subsequent usage produces an
     error message.  */
  const cpp_token *tok;
  cpp_hashnode *hp;

  for (;;)
    {
      tok = _cpp_get_token (pfile);
      if (tok->type == CPP_EOF)
	break;
      if (tok->type != CPP_NAME)
	{
	  cpp_error (pfile, "invalid #pragma poison directive");
	  return;
	}

      hp = tok->val.node;
      if (hp->type == T_POISON)
	;  /* It is allowed to poison the same identifier twice.  */
      else
	{
	  if (hp->type != T_VOID)
	    cpp_warning (pfile, "poisoning existing macro \"%s\"", hp->name);
	  _cpp_free_definition (hp);
	  hp->type = T_POISON;
	}
    }

  if (pfile->cb.poison)
    (*pfile->cb.poison) (pfile);
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
  cpp_buffer *ip = CPP_BUFFER (pfile);
  if (CPP_PREV_BUFFER (ip) == NULL)
    cpp_warning (pfile, "#pragma system_header outside include file");
  else
    cpp_make_system_header (pfile, ip, 1);
}

/* Check the modified date of the current include file against a specified
   file. Issue a diagnostic, if the specified file is newer. We use this to
   determine if a fixed header should be refixed.  */
static void
do_pragma_dependency (pfile)
     cpp_reader *pfile;
{
  const U_CHAR *name;
  unsigned int len;
  int ordering, ab;
  char left, right;
 
  if (parse_include (pfile, U"pragma dependency", 1, &name, &len, &ab))
    return;

  left = ab ? '<' : '"';
  right = ab ? '>' : '"';
 
  ordering = _cpp_compare_file_date (pfile, name, len, ab);
  if (ordering < 0)
    cpp_warning (pfile, "cannot find source %c%s%c", left, name, right);
  else if (ordering > 0)
    {
      const cpp_token *msg = _cpp_get_token (pfile);
      
      cpp_warning (pfile, "current file is older than %c%.*s%c",
		   left, (int)len, name, right);
      if (msg->type != CPP_EOF
	  && _cpp_begin_message (pfile, WARNING, NULL, msg->line, msg->col))
	{
	  cpp_output_list (pfile, stderr, &pfile->token_list, msg);
	  putc ('\n', stderr);
	}
    }
}

/* Just ignore #sccs, on systems where we define it at all.  */
#ifdef SCCS_DIRECTIVE
static void
do_sccs (pfile)
     cpp_reader *pfile ATTRIBUTE_UNUSED;
{
}
#endif

/* We've found an `#if' directive.  If the only thing before it in
   this file is white space, and if it is of the form
   `#if ! defined SYMBOL', then SYMBOL is a possible controlling macro
   for inclusion of this file.  (See redundant_include_p in cppfiles.c
   for an explanation of controlling macros.)  If so, return the
   hash node for SYMBOL.  Otherwise, return NULL.  */

static const cpp_hashnode *
detect_if_not_defined (pfile)
     cpp_reader *pfile;
{
  const cpp_token *token;
  cpp_hashnode *cmacro = 0;

  /* We are guaranteed that tokens are consecutive and end in CPP_EOF.  */
  token = pfile->first_directive_token + 2;

  if (token->type != CPP_NOT)
    return 0;

  token++;
  if (token->type != CPP_DEFINED)
    return 0;

  token++;
  if (token->type == CPP_OPEN_PAREN)
    token++;

  if (token->type != CPP_NAME)
    return 0;

  cmacro = token->val.node;

  if (token[-1].type == CPP_OPEN_PAREN)
    {
      token++;
      if (token->type != CPP_CLOSE_PAREN)
	return 0;
    }

  token++;
  if (token->type != CPP_EOF)
    return 0;

  return cmacro;
}

/* Parse an #ifdef or #ifndef directive.  Returns the hash node of the
   macro being tested, and issues various error messages.  */

static const cpp_hashnode *
parse_ifdef (pfile, name)
     cpp_reader *pfile;
     const U_CHAR *name;
{
  enum cpp_ttype type;
  const cpp_hashnode *node = 0;

  const cpp_token *token = _cpp_get_token (pfile);
  type = token->type;

  if (type == CPP_EOF)
    cpp_pedwarn (pfile, "#%s with no argument", name);
  else if (type != CPP_NAME)
    cpp_pedwarn (pfile, "#%s with invalid argument", name);
  else if (_cpp_get_token (pfile)->type != CPP_EOF)
    cpp_pedwarn (pfile, "garbage at end of #%s", name);

  if (type == CPP_NAME)
    node = token->val.node;
  if (node && node->type == T_POISON)
    {
      cpp_error (pfile, "attempt to use poisoned identifier \"%s\"",
		 node->name);
      node = 0;
    }

  return node;
}

/* #ifdef is dead simple.  */

static void
do_ifdef (pfile)
     cpp_reader *pfile;
{
  const cpp_hashnode *node = 0;

  if (! pfile->skipping)
    node = parse_ifdef (pfile, dtable[T_IFDEF].name);

  push_conditional (pfile, !(node && node->type != T_VOID), T_IFDEF, 0);
}

/* #ifndef is a tad more complex, because we need to check for a
   no-reinclusion wrapper.  */

static void
do_ifndef (pfile)
     cpp_reader *pfile;
{
  int start_of_file = 0;
  const cpp_hashnode *node = 0;

  if (! pfile->skipping)
    {
      start_of_file = (pfile->token_list.flags & BEG_OF_FILE);
      node = parse_ifdef (pfile, dtable[T_IFNDEF].name);
    }

  push_conditional (pfile, node && node->type != T_VOID,
		    T_IFNDEF, start_of_file ? node : 0);
}

/* #if is straightforward; just call _cpp_parse_expr, then conditional_skip.
   Also, check for a reinclude preventer of the form #if !defined (MACRO).  */

static void
do_if (pfile)
     cpp_reader *pfile;
{
  const cpp_hashnode *cmacro = 0;
  int value = 0;

  if (! pfile->skipping)
    {
      if (pfile->token_list.flags & BEG_OF_FILE)
	cmacro = detect_if_not_defined (pfile);
      value = _cpp_parse_expr (pfile);
    }
  push_conditional (pfile, value == 0, T_IF, cmacro);
}

/* #else flips pfile->skipping and continues without changing
   if_stack; this is so that the error message for missing #endif's
   etc. will point to the original #if.  */

static void
do_else (pfile)
     cpp_reader *pfile;
{
  struct if_stack *ifs = CPP_BUFFER (pfile)->if_stack;
  validate_else (pfile, dtable[T_ELSE].name);

  if (ifs == NULL)
    {
      cpp_error (pfile, "#else without #if");
      return;
    }
  if (ifs->type == T_ELSE)
    {
      cpp_error (pfile, "#else after #else");
      cpp_error_with_line (pfile, ifs->lineno, ifs->colno,
			   "the conditional began here");
    }

  /* #ifndef can't have its special treatment for containing the whole file
     if it has a #else clause.  */
  ifs->cmacro = 0;
  ifs->type = T_ELSE;
  if (! ifs->was_skipping)
    {
      /* If pfile->skipping is 2, one of the blocks in an #if/#elif/... chain
	 succeeded, so we mustn't do the else block.  */
      if (pfile->skipping < 2)
	pfile->skipping = ! pfile->skipping;
    }
}

/*
 * handle a #elif directive by not changing if_stack either.
 * see the comment above do_else.
 */

static void
do_elif (pfile)
     cpp_reader *pfile;
{
  struct if_stack *ifs = CPP_BUFFER (pfile)->if_stack;

  if (ifs == NULL)
    {
      cpp_error (pfile, "#elif without #if");
      return;
    }
  if (ifs->type == T_ELSE)
    {
      cpp_error (pfile, "#elif after #else");
      cpp_error_with_line (pfile, ifs->lineno, ifs->colno,
			   "the conditional began here");
    }

  ifs->type = T_ELIF;
  if (ifs->was_skipping)
    return;  /* Don't evaluate a nested #if */

  if (pfile->skipping != 1)
    {
      pfile->skipping = 2;  /* one block succeeded, so don't do any others */
      return;
    }

  pfile->skipping = ! _cpp_parse_expr (pfile);
}

/* #endif pops the if stack and resets pfile->skipping.  */

static void
do_endif (pfile)
     cpp_reader *pfile;
{
  struct if_stack *ifs = CPP_BUFFER (pfile)->if_stack;

  validate_else (pfile, dtable[T_ENDIF].name);

  if (ifs == NULL)
    cpp_error (pfile, "#endif without #if");
  else
    {
      CPP_BUFFER (pfile)->if_stack = ifs->next;
      pfile->skipping = ifs->was_skipping;
      pfile->potential_control_macro = ifs->cmacro;
      obstack_free (pfile->buffer_ob, ifs);
    }
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

  ifs = xobnew (pfile->buffer_ob, struct if_stack);
  ifs->lineno = _cpp_get_line (pfile, &ifs->colno);
  ifs->next = CPP_BUFFER (pfile)->if_stack;
  ifs->cmacro = cmacro;
  ifs->was_skipping = pfile->skipping;
  ifs->type = type;

  if (!pfile->skipping)
    pfile->skipping = skip;

  CPP_BUFFER (pfile)->if_stack = ifs;
}

/* Issue -pedantic warning for text which is not a comment following
   an #else or #endif.  */

static void
validate_else (pfile, directive)
     cpp_reader *pfile;
     const U_CHAR *directive;
{
  if (CPP_PEDANTIC (pfile) && _cpp_get_token (pfile)->type != CPP_EOF)
    cpp_pedwarn (pfile, "ISO C forbids text after #%s", directive);
}

/* Called when we reach the end of a file.  Walk back up the
   conditional stack till we reach its level at entry to this file,
   issuing error messages.  Then force skipping off.  */
static void
unwind_if_stack (pfile, pbuf)
     cpp_reader *pfile;
     cpp_buffer *pbuf;
{
  struct if_stack *ifs, *nifs;

  for (ifs = pbuf->if_stack; ifs; ifs = nifs)
    {
      cpp_error_with_line (pfile, ifs->lineno, ifs->colno, "unterminated #%s",
			   dtable[ifs->type].name);
      nifs = ifs->next;
      /* No need to free - they'll all go away with the buffer.  */
    }
  pfile->skipping = 0;
}

/* Parses an assertion, returning a pointer to the hash node of the
   predicate, or 0 on error.  If an answer was supplied, it is
   allocated and placed in ANSWERP, otherwise it is set to 0.  We use
   _cpp_get_raw_token, since we cannot assume tokens are consecutive
   in a #if statement (we may be in a macro), and we don't want to
   macro expand.  */
cpp_hashnode *
_cpp_parse_assertion (pfile, answerp)
     cpp_reader *pfile;
     struct answer **answerp;
{
  struct answer *answer = 0;
  cpp_toklist *list;
  U_CHAR *sym;
  const cpp_token *token, *predicate;
  const struct directive *d = pfile->token_list.directive;
  unsigned int len = 0;

  predicate = _cpp_get_raw_token (pfile);
  if (predicate->type == CPP_EOF)
    {
      cpp_error (pfile, "assertion without predicate");
      return 0;
    }
  else if (predicate->type != CPP_NAME)
    {
      cpp_error (pfile, "predicate must be an identifier");
      return 0;
    }

  token = _cpp_get_raw_token (pfile);
  if (token->type != CPP_OPEN_PAREN)
    {
      /* #unassert and #if are OK without predicate.  */
      if (d == &dtable[T_UNASSERT])
	{
	  if (token->type == CPP_EOF)
	    goto lookup_node;
	}
      else if (d != &dtable[T_ASSERT])
	{
	  _cpp_push_token (pfile, token);
	  goto lookup_node;
	}
      cpp_error (pfile, "missing '(' after predicate");
      return 0;
    }

  /* Allocate a struct answer, and copy the answer to it.  */
  answer = (struct answer *) xmalloc (sizeof (struct answer));
  list = &answer->list;
  _cpp_init_toklist (list, 1);	/* Empty.  */

  for (;;)
    {
      cpp_token *dest;

      token = _cpp_get_raw_token (pfile);

      if (token->type == CPP_EOF)
	{
	  cpp_error (pfile, "missing ')' to complete answer");
	  goto error;
	}
      if (token->type == CPP_CLOSE_PAREN)
	break;

      /* Copy the token.  */
      _cpp_expand_token_space (list, 1);
      dest = &list->tokens[list->tokens_used++];
      *dest = *token;

      if (TOKEN_SPELL (token) == SPELL_STRING)
	{
	  _cpp_expand_name_space (list, token->val.str.len);
	  dest->val.str.text = list->namebuf + list->name_used;
	  memcpy (list->namebuf + list->name_used,
		  token->val.str.text, token->val.str.len);
	  list->name_used += token->val.str.len;
	}
    }

  if (list->tokens_used == 0)
    {
      cpp_error (pfile, "predicate's answer is empty");
      goto error;
    }

  /* Drop whitespace at start.  */
  list->tokens[0].flags &= ~PREV_WHITE;

  if ((d == &dtable[T_ASSERT] || d == &dtable[T_UNASSERT])
      && token[1].type != CPP_EOF)
    {
      cpp_error (pfile, "junk at end of assertion");
      goto error;
    }

 lookup_node:
  *answerp = answer;
  len = predicate->val.node->length;
  sym = alloca (len + 1);

  /* Prefix '#' to get it out of macro namespace.  */
  sym[0] = '#';
  memcpy (sym + 1, predicate->val.node->name, len);
  return cpp_lookup (pfile, sym, len + 1);

 error:
  FREE_ANSWER (answer);
  return 0;
}

/* Returns a pointer to the pointer to the answer in the answer chain,
   or a pointer to NULL if the answer is not in the chain.  */
struct answer **
_cpp_find_answer (node, candidate)
     cpp_hashnode *node;
     const cpp_toklist *candidate;
{
  struct answer **result;

  for (result = &node->value.answers; *result; result = &(*result)->next)
    if (_cpp_equiv_toklists (&(*result)->list, candidate))
      break;

  return result;
}

static void
do_assert (pfile)
     cpp_reader *pfile;
{
  struct answer *new_answer;
  cpp_hashnode *node;
  
  node = _cpp_parse_assertion (pfile, &new_answer);
  if (node)
    {
      new_answer->next = 0;
      new_answer->list.file = pfile->token_list.file;

      if (node->type == T_ASSERTION)
	{
	  if (*_cpp_find_answer (node, &new_answer->list))
	    goto err;
	  new_answer->next = node->value.answers;
	}
      node->type = T_ASSERTION;
      node->value.answers = new_answer;
    }
  return;

 err:
  cpp_warning (pfile, "\"%s\" re-asserted", node->name + 1);
  FREE_ANSWER (new_answer);
}

static void
do_unassert (pfile)
     cpp_reader *pfile;
{
  cpp_hashnode *node;
  struct answer *answer, *temp;
  
  node = _cpp_parse_assertion (pfile, &answer);
  if (node)
    {
      /* It isn't an error to #unassert something that isn't asserted.  */
      if (node->type == T_ASSERTION)
	{
	  if (answer)
	    {
	      struct answer **p = _cpp_find_answer (node, &answer->list);

	      temp = *p;
	      if (temp)
		{
		  *p = temp->next;
		  FREE_ANSWER (temp);
		}
	      if (node->value.answers == 0)
		node->type = T_VOID;
	    }
	  else
	    _cpp_free_definition (node);
	}

      if (answer)
	FREE_ANSWER (answer);
    }
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
      buf = (char *) alloca (count);
      memcpy (buf, str, count - 2);
      buf[p - str] = ' ';
      buf[count - 2] = '\n';
      buf[count - 1] = '\0';
    }
  else
    {
      count = strlen (str) + 4;
      buf = (char *) alloca (count);
      memcpy (buf, str, count - 4);
      strcpy (&buf[count-4], " 1\n");
    }

  _cpp_run_directive (pfile, &dtable[T_DEFINE], buf, count - 1, 0);
}

/* Slight variant of the above for use by initialize_builtins, which (a)
   knows how to set up the buffer itself, (b) needs a different "filename"
   tag.  */
void
_cpp_define_builtin (pfile, str)
     cpp_reader *pfile;
     const char *str;
{
  _cpp_run_directive (pfile, &dtable[T_DEFINE],
		      str, strlen (str),
		      _("<builtin>"));
}

/* Process MACRO as if it appeared as the body of an #undef.  */
void
cpp_undef (pfile, macro)
     cpp_reader *pfile;
     const char *macro;
{
  _cpp_run_directive (pfile, &dtable[T_UNDEF], macro, strlen (macro), 0);
}

/* Process the string STR as if it appeared as the body of a #assert. */
void
cpp_assert (pfile, str)
     cpp_reader *pfile;
     const char *str;
{
  _cpp_run_directive (pfile, &dtable[T_ASSERT], str, strlen (str), 0);
}

/* Process STR as if it appeared as the body of an #unassert. */
void
cpp_unassert (pfile, str)
     cpp_reader *pfile;
     const char *str;
{
  _cpp_run_directive (pfile, &dtable[T_UNASSERT], str, strlen (str), 0);
}  

/* Determine whether the identifier ID, of length LEN, is a defined macro.  */
int
cpp_defined (pfile, id, len)
     cpp_reader *pfile;
     const U_CHAR *id;
     int len;
{
  cpp_hashnode *hp = cpp_lookup (pfile, id, len);
  if (hp->type == T_POISON)
    {
      cpp_error (pfile, "attempt to use poisoned \"%s\"", hp->name);
      return 0;
    }
  return (hp->type != T_VOID);
}

/* Allocate a new cpp_buffer for PFILE, and push it on the input buffer stack.
   If BUFFER != NULL, then use the LENGTH characters in BUFFER
   as the new input buffer.
   Return the new buffer, or NULL on failure.  */

cpp_buffer *
cpp_push_buffer (pfile, buffer, length)
     cpp_reader *pfile;
     const U_CHAR *buffer;
     long length;
{
  cpp_buffer *buf = CPP_BUFFER (pfile);
  cpp_buffer *new;
  if (++pfile->buffer_stack_depth == CPP_STACK_MAX)
    {
      cpp_fatal (pfile, "#include nested too deep");
      return NULL;
    }
  if (pfile->cur_context > 0)
    {
      cpp_ice (pfile, "buffer pushed with contexts stacked");
      _cpp_skip_rest_of_line (pfile);
    }

  new = xobnew (pfile->buffer_ob, cpp_buffer);
  memset (new, 0, sizeof (cpp_buffer));

  new->line_base = new->buf = new->cur = buffer;
  new->rlimit = buffer + length;
  new->prev = buf;
  new->pfile = pfile;
  /* No read ahead or extra char initially.  */
  new->read_ahead = EOF;
  new->extra_char = EOF;

  CPP_BUFFER (pfile) = new;
  return new;
}

cpp_buffer *
cpp_pop_buffer (pfile)
     cpp_reader *pfile;
{
  int wfb;
  cpp_buffer *buf = CPP_BUFFER (pfile);

  unwind_if_stack (pfile, buf);
  wfb = (buf->inc != 0);
  if (wfb)
    _cpp_pop_file_buffer (pfile, buf);

  CPP_BUFFER (pfile) = CPP_PREV_BUFFER (buf);
  obstack_free (pfile->buffer_ob, buf);
  pfile->buffer_stack_depth--;

  if (wfb && pfile->cb.leave_file && CPP_BUFFER (pfile))
    (*pfile->cb.leave_file) (pfile);
  
  return CPP_BUFFER (pfile);
}

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free
#define DSC(x) U x, sizeof x - 1
void
_cpp_init_stacks (pfile)
     cpp_reader *pfile;
{
  int i;
  struct spec_nodes *s;

  pfile->buffer_ob = xnew (struct obstack);
  obstack_init (pfile->buffer_ob);

  /* Perhaps not the ideal place to put this.  */
  pfile->spec_nodes = s = xnew (struct spec_nodes);
  s->n_L                = cpp_lookup (pfile, DSC("L"));
  s->n__STRICT_ANSI__   = cpp_lookup (pfile, DSC("__STRICT_ANSI__"));
  s->n__CHAR_UNSIGNED__ = cpp_lookup (pfile, DSC("__CHAR_UNSIGNED__"));
  s->n__VA_ARGS__       = cpp_lookup (pfile, DSC("__VA_ARGS__"));
  for (i = 0; i < N_DIRECTIVES; i++)
    s->dirs[i] = cpp_lookup (pfile, dtable[i].name, dtable[i].length);
}

void
_cpp_cleanup_stacks (pfile)
     cpp_reader *pfile;
{
  obstack_free (pfile->buffer_ob, 0);
  free (pfile->buffer_ob);
}
