/* CPP Library. (Directive handling.)
   Copyright (C) 1986, 1987, 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002 Free Software Foundation, Inc.
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
  unsigned int line;		/* Line where condition started.  */
  const cpp_hashnode *mi_cmacro;/* macro name for #ifndef around entire file */
  bool skip_elses;		/* Can future #else / #elif be skipped?  */
  bool was_skipping;		/* If were skipping on entry.  */
  int type;			/* Most recent conditional, for diagnostics.  */
};

/* Contains a registered pragma or pragma namespace.  */
typedef void (*pragma_cb) PARAMS ((cpp_reader *));
struct pragma_entry
{
  struct pragma_entry *next;
  const cpp_hashnode *pragma;	/* Name and length.  */
  int is_nspace;
  union {
    pragma_cb handler;
    struct pragma_entry *space;
  } u;
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
static void directive_diagnostics
	PARAMS ((cpp_reader *, const directive *, int));
static void run_directive	PARAMS ((cpp_reader *, int,
					 const char *, size_t));
static const cpp_token *glue_header_name PARAMS ((cpp_reader *));
static const cpp_token *parse_include PARAMS ((cpp_reader *));
static void push_conditional	PARAMS ((cpp_reader *, int, int,
					 const cpp_hashnode *));
static unsigned int read_flag	PARAMS ((cpp_reader *, unsigned int));
static U_CHAR *dequote_string	PARAMS ((cpp_reader *, const U_CHAR *,
					 unsigned int));
static int  strtoul_for_line	PARAMS ((const U_CHAR *, unsigned int,
					 unsigned long *));
static void do_diagnostic	PARAMS ((cpp_reader *, enum error_type, int));
static cpp_hashnode *lex_macro_node	PARAMS ((cpp_reader *));
static void do_include_common	PARAMS ((cpp_reader *, enum include_type));
static struct pragma_entry *lookup_pragma_entry
  PARAMS ((struct pragma_entry *, const cpp_hashnode *pragma));
static struct pragma_entry *insert_pragma_entry
  PARAMS ((cpp_reader *, struct pragma_entry **, const cpp_hashnode *,
	   pragma_cb));
static void do_pragma_once	PARAMS ((cpp_reader *));
static void do_pragma_poison	PARAMS ((cpp_reader *));
static void do_pragma_system_header	PARAMS ((cpp_reader *));
static void do_pragma_dependency	PARAMS ((cpp_reader *));
static void do_linemarker		PARAMS ((cpp_reader *));
static const cpp_token *get_token_no_padding PARAMS ((cpp_reader *));
static const cpp_token *get__Pragma_string PARAMS ((cpp_reader *));
static void destringize_and_run PARAMS ((cpp_reader *, const cpp_string *));
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
D(line,		T_LINE,		KANDR,     0)		   /*   2465 */ \
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

/* Don't invoke CONCAT2 with any whitespace or K&R cc will fail.  */
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

/* Don't invoke CONCAT2 with any whitespace or K&R cc will fail.  */
#define D(name, t, origin, flags) \
{ CONCAT2(do_,name), (const U_CHAR *) STRINGX(name), \
  sizeof STRINGX(name) - 1, origin, flags },
static const directive dtable[] =
{
DIRECTIVE_TABLE
};
#undef D
#undef DIRECTIVE_TABLE

/* Wrapper struct directive for linemarkers.
   The origin is more or less true - the original K+R cpp
   did use this notation in its preprocessed output.  */
static const directive linemarker_dir =
{
  do_linemarker, U"#", 1, KANDR, IN_I
};

#define SEEN_EOL() (pfile->cur_token[-1].type == CPP_EOF)

/* Skip any remaining tokens in a directive.  */
static void
skip_rest_of_line (pfile)
     cpp_reader *pfile;
{
  /* Discard all stacked contexts.  */
  while (pfile->context != &pfile->base_context)
    _cpp_pop_context (pfile);

  /* Sweep up all tokens remaining on the line.  */
  if (! SEEN_EOL ())
    while (_cpp_lex_token (pfile)->type != CPP_EOF)
      ;
}

/* Ensure there are no stray tokens at the end of a directive.  */
static void
check_eol (pfile)
     cpp_reader *pfile;
{
  if (! SEEN_EOL () && _cpp_lex_token (pfile)->type != CPP_EOF)
    cpp_pedwarn (pfile, "extra tokens at end of #%s directive",
		 pfile->directive->name);
}

/* Called when entering a directive, _Pragma or command-line directive.  */
static void
start_directive (pfile)
     cpp_reader *pfile;
{
  /* Setup in-directive state.  */
  pfile->state.in_directive = 1;
  pfile->state.save_comments = 0;

  /* Some handlers need the position of the # for diagnostics.  */
  pfile->directive_line = pfile->line;
}

/* Called when leaving a directive, _Pragma or command-line directive.  */
static void
end_directive (pfile, skip_line)
     cpp_reader *pfile;
     int skip_line;
{
  /* We don't skip for an assembler #.  */
  if (skip_line)
    {
      skip_rest_of_line (pfile);
      if (!pfile->keep_tokens)
	{
	  pfile->cur_run = &pfile->base_run;
	  pfile->cur_token = pfile->base_run.base;
	}
    }

  /* Restore state.  */
  pfile->state.save_comments = ! CPP_OPTION (pfile, discard_comments);
  pfile->state.in_directive = 0;
  pfile->state.angled_headers = 0;
  pfile->directive = 0;
}

/* Output diagnostics for a directive DIR.  INDENTED is non-zero if
   the '#' was indented.  */
static void
directive_diagnostics (pfile, dir, indented)
     cpp_reader *pfile;
     const directive *dir;
     int indented;
{
  /* Issue -pedantic warnings for extensions.  */
  if (CPP_PEDANTIC (pfile)
      && ! pfile->state.skipping
      && dir->origin == EXTENSION)
    cpp_pedwarn (pfile, "#%s is a GCC extension", dir->name);

  /* Traditionally, a directive is ignored unless its # is in
     column 1.  Therefore in code intended to work with K+R
     compilers, directives added by C89 must have their #
     indented, and directives present in traditional C must not.
     This is true even of directives in skipped conditional
     blocks.  #elif cannot be used at all.  */
  if (CPP_WTRADITIONAL (pfile))
    {
      if (dir == &dtable[T_ELIF])
	cpp_warning (pfile, "suggest not using #elif in traditional C");
      else if (indented && dir->origin == KANDR)
	cpp_warning (pfile,
		     "traditional C ignores #%s with the # indented",
		     dir->name);
      else if (!indented && dir->origin != KANDR)
	cpp_warning (pfile,
		     "suggest hiding #%s from traditional C with an indented #",
		     dir->name);
    }
}

/* Check if we have a known directive.  INDENTED is non-zero if the
   '#' of the directive was indented.  This function is in this file
   to save unnecessarily exporting dtable etc. to cpplex.c.  Returns
   non-zero if the line of tokens has been handled, zero if we should
   continue processing the line.  */
int
_cpp_handle_directive (pfile, indented)
     cpp_reader *pfile;
     int indented;
{
  const directive *dir = 0;
  const cpp_token *dname;
  int skip = 1;

  start_directive (pfile);
  dname = _cpp_lex_token (pfile);

  if (dname->type == CPP_NAME)
    {
      if (dname->val.node->directive_index)
	dir = &dtable[dname->val.node->directive_index - 1];
    }
  /* We do not recognise the # followed by a number extension in
     assembler code.  */
  else if (dname->type == CPP_NUMBER && CPP_OPTION (pfile, lang) != CLK_ASM)
    {
      dir = &linemarker_dir;
      if (CPP_PEDANTIC (pfile) && ! CPP_OPTION (pfile, preprocessed)
	  && ! pfile->state.skipping)
	cpp_pedwarn (pfile, "style of line directive is a GCC extension");
    }

  if (dir)
    {
      /* If we have a directive that is not an opening conditional,
	 invalidate any control macro.  */
      if (! (dir->flags & IF_COND))
	pfile->mi_valid = false;

      /* Kluge alert.  In order to be sure that code like this

	 #define HASH #
	 HASH define foo bar

	 does not cause '#define foo bar' to get executed when
	 compiled with -save-temps, we recognize directives in
	 -fpreprocessed mode only if the # is in column 1.  cppmacro.c
	 puts a space in front of any '#' at the start of a macro.  */
      if (CPP_OPTION (pfile, preprocessed)
	  && (indented || !(dir->flags & IN_I)))
	{
	  skip = 0;
	  dir = 0;
	}
      else
	{
	  /* In failed conditional groups, all non-conditional
	     directives are ignored.  Before doing that, whether
	     skipping or not, we should lex angle-bracketed headers
	     correctly, and maybe output some diagnostics.  */
	  pfile->state.angled_headers = dir->flags & INCL;
	  if (! CPP_OPTION (pfile, preprocessed))
	    directive_diagnostics (pfile, dir, indented);
	  if (pfile->state.skipping && !(dir->flags & COND))
	    dir = 0;
	}
    }
  else if (dname->type == CPP_EOF)
    ;	/* CPP_EOF is the "null directive".  */
  else
    {
      /* An unknown directive.  Don't complain about it in assembly
	 source: we don't know where the comments are, and # may
	 introduce assembler pseudo-ops.  Don't complain about invalid
	 directives in skipped conditional groups (6.10 p4).  */
      if (CPP_OPTION (pfile, lang) == CLK_ASM)
	skip = 0;
      else if (!pfile->state.skipping)
	cpp_error (pfile, "invalid preprocessing directive #%s",
		   cpp_token_as_text (pfile, dname));
    }

  if (dir)
    {
      pfile->directive = dir;
      (*pfile->directive->handler) (pfile);
    }
  else if (skip == 0)
    _cpp_backup_tokens (pfile, 1);

  end_directive (pfile, skip);
  return skip;
}

/* Directive handler wrapper used by the command line option
   processor.  */
static void
run_directive (pfile, dir_no, buf, count)
     cpp_reader *pfile;
     int dir_no;
     const char *buf;
     size_t count;
{
  cpp_push_buffer (pfile, (const U_CHAR *) buf, count,
		   /* from_stage3 */ true, 1);
  start_directive (pfile);
  /* We don't want a leading # to be interpreted as a directive.  */
  pfile->buffer->saved_flags = 0;
  pfile->directive = &dtable[dir_no];
  (void) (*pfile->directive->handler) (pfile);
  end_directive (pfile, 1);
  _cpp_pop_buffer (pfile);
}

/* Checks for validity the macro name in #define, #undef, #ifdef and
   #ifndef directives.  */
static cpp_hashnode *
lex_macro_node (pfile)
     cpp_reader *pfile;
{
  cpp_hashnode *node;
  const cpp_token *token = _cpp_lex_token (pfile);

  /* The token immediately after #define must be an identifier.  That
     identifier may not be "defined", per C99 6.10.8p4.
     In C++, it may not be any of the "named operators" either,
     per C++98 [lex.digraph], [lex.key].
     Finally, the identifier may not have been poisoned.  (In that case
     the lexer has issued the error message for us.)  */

  if (token->type != CPP_NAME)
    {
      if (token->type == CPP_EOF)
	cpp_error (pfile, "no macro name given in #%s directive",
		   pfile->directive->name);
      else if (token->flags & NAMED_OP)
	cpp_error (pfile,
	   "\"%s\" cannot be used as a macro name as it is an operator in C++",
		   NODE_NAME (token->val.node));
      else
	cpp_error (pfile, "macro names must be identifiers");

      return 0;
    }

  node = token->val.node;
  if (node->flags & NODE_POISONED)
    return 0;

  if (node == pfile->spec_nodes.n_defined)
    {
      cpp_error (pfile, "\"%s\" cannot be used as a macro name",
		 NODE_NAME (node));
      return 0;
    }

  return node;
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
	  (*pfile->cb.define) (pfile, pfile->directive_line, node);
    }
}

/* Handle #undef.  Mark the identifier NT_VOID in the hash table.  */
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
	(*pfile->cb.undef) (pfile, pfile->directive_line, node);

      if (node->flags & NODE_WARN)
	cpp_warning (pfile, "undefining \"%s\"", NODE_NAME (node));

      _cpp_free_definition (node);
    }
  check_eol (pfile);
}

/* Helper routine used by parse_include.  Reinterpret the current line
   as an h-char-sequence (< ... >); we are looking at the first token
   after the <.  Returns the header as a token, or NULL on failure.  */
static const cpp_token *
glue_header_name (pfile)
     cpp_reader *pfile;
{
  cpp_token *header = NULL;
  const cpp_token *token;
  unsigned char *buffer;
  size_t len, total_len = 0, capacity = 1024;

  /* To avoid lexed tokens overwriting our glued name, we can only
     allocate from the string pool once we've lexed everything.  */
  buffer = (unsigned char *) xmalloc (capacity);
  for (;;)
    {
      token = cpp_get_token (pfile);

      if (token->type == CPP_GREATER || token->type == CPP_EOF)
	break;

      len = cpp_token_len (token);
      if (total_len + len > capacity)
	{
	  capacity = (capacity + len) * 2;
	  buffer = (unsigned char *) xrealloc (buffer, capacity);
	}

      if (token->flags & PREV_WHITE)
	buffer[total_len++] = ' ';

      total_len = cpp_spell_token (pfile, token, &buffer[total_len]) - buffer;
    }

  if (token->type == CPP_EOF)
    cpp_error (pfile, "missing terminating > character");
  else
    {
      unsigned char *token_mem = _cpp_unaligned_alloc (pfile, total_len + 1);
      memcpy (token_mem, buffer, total_len);
      token_mem[total_len] = '\0';

      header = _cpp_temp_token (pfile);
      header->type = CPP_HEADER_NAME;
      header->flags = 0;
      header->val.str.len = total_len;
      header->val.str.text = token_mem;
    }

  free ((PTR) buffer);
  return header;
}

/* Returns the header string of #include, #include_next, #import and
   #pragma dependency.  Returns NULL on error.  */
static const cpp_token *
parse_include (pfile)
     cpp_reader *pfile;
{
  const unsigned char *dir;
  const cpp_token *header;

  if (pfile->directive == &dtable[T_PRAGMA])
    dir = U"pragma dependency";
  else
    dir = pfile->directive->name;

  /* Allow macro expansion.  */
  header = cpp_get_token (pfile);
  if (header->type != CPP_STRING && header->type != CPP_HEADER_NAME)
    {
      if (header->type != CPP_LESS)
	{
	  cpp_error (pfile, "#%s expects \"FILENAME\" or <FILENAME>", dir);
	  return NULL;
	}

      header = glue_header_name (pfile);
      if (header == NULL)
	return header;
    }

  if (header->val.str.len == 0)
    {
      cpp_error (pfile, "empty file name in #%s", dir);
      return NULL;
    }

  return header;
}

/* Handle #include, #include_next and #import.  */
static void
do_include_common (pfile, type)
     cpp_reader *pfile;
     enum include_type type;
{
  const cpp_token *header;

  /* For #include_next, if this is the primary source file, warn and
     use the normal search logic.  */
  if (type == IT_INCLUDE_NEXT && ! pfile->buffer->prev)
    {
      cpp_warning (pfile, "#include_next in primary source file");
      type = IT_INCLUDE;
    }
  else if (type == IT_IMPORT && CPP_OPTION (pfile, warn_import))
    {
      CPP_OPTION (pfile, warn_import) = 0;
      cpp_warning (pfile,
	   "#import is obsolete, use an #ifndef wrapper in the header file");
    }

  header = parse_include (pfile);
  if (header)
    {
      /* Prevent #include recursion.  */
      if (pfile->line_maps.depth >= CPP_STACK_MAX)
	cpp_fatal (pfile, "#include nested too deeply");
      else
	{
	  check_eol (pfile);
	  /* Get out of macro context, if we are.  */
	  skip_rest_of_line (pfile);
	  if (pfile->cb.include)
	    (*pfile->cb.include) (pfile, pfile->directive_line,
				  pfile->directive->name, header);

	  _cpp_execute_include (pfile, header, type);
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
  do_include_common (pfile, IT_IMPORT);
}

static void
do_include_next (pfile)
     cpp_reader *pfile;
{
  do_include_common (pfile, IT_INCLUDE_NEXT);
}

/* Subroutine of do_linemarker.  Read possible flags after file name.
   LAST is the last flag seen; 0 if this is the first flag. Return the
   flag if it is valid, 0 at the end of the directive. Otherwise
   complain.  */
static unsigned int
read_flag (pfile, last)
     cpp_reader *pfile;
     unsigned int last;
{
  const cpp_token *token = _cpp_lex_token (pfile);

  if (token->type == CPP_NUMBER && token->val.str.len == 1)
    {
      unsigned int flag = token->val.str.text[0] - '0';

      if (flag > last && flag <= 4
	  && (flag != 4 || last == 3)
	  && (flag != 2 || last == 0))
	return flag;
    }

  if (token->type != CPP_EOF)
    cpp_error (pfile, "invalid flag \"%s\" in line directive",
	       cpp_token_as_text (pfile, token));
  return 0;
}

/* Subroutine of do_line and do_linemarker.  Returns a version of STR
   which has a NUL terminator and all escape sequences converted to
   their equivalents.  Temporary, hopefully.  */
static U_CHAR *
dequote_string (pfile, str, len)
     cpp_reader *pfile;
     const U_CHAR *str;
     unsigned int len;
{
  U_CHAR *result = _cpp_unaligned_alloc (pfile, len + 1);
  U_CHAR *dst = result;
  const U_CHAR *limit = str + len;
  unsigned int c;
  unsigned HOST_WIDE_INT mask;

  /* We need the mask to match the host's 'unsigned char', not the
     target's.  */
  if (CHAR_BIT < HOST_BITS_PER_WIDE_INT)
    mask = ((unsigned HOST_WIDE_INT) 1 << CHAR_BIT) - 1;
  else
    mask = ~(unsigned HOST_WIDE_INT)0;
  
  while (str < limit)
    {
      c = *str++;
      if (c != '\\')
	*dst++ = c;
      else
	*dst++ = cpp_parse_escape (pfile, (const U_CHAR **)&str, limit, mask, 0);
    }
  *dst++ = '\0';
  return result;
}

/* Subroutine of do_line and do_linemarker.  Convert a number in STR,
   of length LEN, to binary; store it in NUMP, and return 0 if the
   number was well-formed, 1 if not.  Temporary, hopefully.  */
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
   Note that the filename string (if any) is a true string constant
   (escapes are interpreted), unlike in #line.  */
static void
do_line (pfile)
     cpp_reader *pfile;
{
  const cpp_token *token;
  const char *new_file = pfile->map->to_file;
  unsigned long new_lineno;

  /* C99 raised the minimum limit on #line numbers.  */
  unsigned int cap = CPP_OPTION (pfile, c99) ? 2147483647 : 32767;

  /* #line commands expand macros.  */
  token = cpp_get_token (pfile);
  if (token->type != CPP_NUMBER
      || strtoul_for_line (token->val.str.text, token->val.str.len,
			   &new_lineno))
    {
      cpp_error (pfile, "\"%s\" after #line is not a positive integer",
		 cpp_token_as_text (pfile, token));
      return;
    }      

  if (CPP_PEDANTIC (pfile) && (new_lineno == 0 || new_lineno > cap))
    cpp_pedwarn (pfile, "line number out of range");

  token = cpp_get_token (pfile);
  if (token->type == CPP_STRING)
    {
      new_file = (const char *) dequote_string (pfile, token->val.str.text,
						token->val.str.len);
      check_eol (pfile);
    }
  else if (token->type != CPP_EOF)
    {
      cpp_error (pfile, "\"%s\" is not a valid filename",
		 cpp_token_as_text (pfile, token));
      return;
    }

  skip_rest_of_line (pfile);
  _cpp_do_file_change (pfile, LC_RENAME, new_file, new_lineno,
		       pfile->map->sysp);
}

/* Interpret the # 44 "file" [flags] notation, which has slightly
   different syntax and semantics from #line:  Flags are allowed,
   and we never complain about the line number being too big.  */
static void
do_linemarker (pfile)
     cpp_reader *pfile;
{
  const cpp_token *token;
  const char *new_file = pfile->map->to_file;
  unsigned long new_lineno;
  unsigned int new_sysp = pfile->map->sysp;
  enum lc_reason reason = LC_RENAME;
  int flag;

  /* Back up so we can get the number again.  Putting this in
     _cpp_handle_directive risks two calls to _cpp_backup_tokens in
     some circumstances, which can segfault.  */
  _cpp_backup_tokens (pfile, 1);

  /* #line commands expand macros.  */
  token = cpp_get_token (pfile);
  if (token->type != CPP_NUMBER
      || strtoul_for_line (token->val.str.text, token->val.str.len,
			   &new_lineno))
    {
      cpp_error (pfile, "\"%s\" after # is not a positive integer",
		 cpp_token_as_text (pfile, token));
      return;
    }      

  token = cpp_get_token (pfile);
  if (token->type == CPP_STRING)
    {
      new_file = (const char *) dequote_string (pfile, token->val.str.text,
						token->val.str.len);
      new_sysp = 0;
      flag = read_flag (pfile, 0);
      if (flag == 1)
	{
	  reason = LC_ENTER;
	  /* Fake an include for cpp_included ().  */
	  _cpp_fake_include (pfile, new_file);
	  flag = read_flag (pfile, flag);
	}
      else if (flag == 2)
	{
	  reason = LC_LEAVE;
	  flag = read_flag (pfile, flag);
	}
      if (flag == 3)
	{
	  new_sysp = 1;
	  flag = read_flag (pfile, flag);
	  if (flag == 4)
	    new_sysp = 2;
	}

      check_eol (pfile);
    }
  else if (token->type != CPP_EOF)
    {
      cpp_error (pfile, "\"%s\" is not a valid filename",
		 cpp_token_as_text (pfile, token));
      return;
    }

  skip_rest_of_line (pfile);
  _cpp_do_file_change (pfile, reason, new_file, new_lineno, new_sysp);
}

/* Arrange the file_change callback.  pfile->line has changed to
   FILE_LINE of TO_FILE, for reason REASON.  SYSP is 1 for a system
   header, 2 for a system header that needs to be extern "C" protected,
   and zero otherwise.  */
void
_cpp_do_file_change (pfile, reason, to_file, file_line, sysp)
     cpp_reader *pfile;
     enum lc_reason reason;
     const char *to_file;
     unsigned int file_line;
     unsigned int sysp;
{
  pfile->map = add_line_map (&pfile->line_maps, reason, sysp,
			     pfile->line, to_file, file_line);

  if (pfile->cb.file_change)
    (*pfile->cb.file_change) (pfile, pfile->map);
}

/* Report a warning or error detected by the program we are
   processing.  Use the directive's tokens in the error message.  */
static void
do_diagnostic (pfile, code, print_dir)
     cpp_reader *pfile;
     enum error_type code;
     int print_dir;
{
  if (_cpp_begin_message (pfile, code, 0, 0))
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
  const cpp_token *str = cpp_get_token (pfile);

  if (str->type != CPP_STRING)
    cpp_error (pfile, "invalid #ident directive");
  else if (pfile->cb.ident)
    (*pfile->cb.ident) (pfile, pfile->directive_line, &str->val.str);

  check_eol (pfile);
}

/* Lookup a PRAGMA name in a singly-linked CHAIN.  Returns the
   matching entry, or NULL if none is found.  The returned entry could
   be the start of a namespace chain, or a pragma.  */
static struct pragma_entry *
lookup_pragma_entry (chain, pragma)
     struct pragma_entry *chain;
     const cpp_hashnode *pragma;
{
  while (chain && chain->pragma != pragma)
    chain = chain->next;

  return chain;
}

/* Create and insert a pragma entry for NAME at the beginning of a
   singly-linked CHAIN.  If handler is NULL, it is a namespace,
   otherwise it is a pragma and its handler.  */
static struct pragma_entry *
insert_pragma_entry (pfile, chain, pragma, handler)
     cpp_reader *pfile;
     struct pragma_entry **chain;
     const cpp_hashnode *pragma;
     pragma_cb handler;
{
  struct pragma_entry *new;

  new = (struct pragma_entry *)
    _cpp_aligned_alloc (pfile, sizeof (struct pragma_entry));
  new->pragma = pragma;
  if (handler)
    {
      new->is_nspace = 0;
      new->u.handler = handler;
    }
  else
    {
      new->is_nspace = 1;
      new->u.space = NULL;
    }

  new->next = *chain;
  *chain = new;
  return new;
}

/* Register a pragma NAME in namespace SPACE.  If SPACE is null, it
   goes in the global namespace.  HANDLER is the handler it will call,
   which must be non-NULL.  */
void
cpp_register_pragma (pfile, space, name, handler)
     cpp_reader *pfile;
     const char *space;
     const char *name;
     pragma_cb handler;
{
  struct pragma_entry **chain = &pfile->pragmas;
  struct pragma_entry *entry;
  const cpp_hashnode *node;

  if (!handler)
    abort ();

  if (space)
    {
      node = cpp_lookup (pfile, U space, strlen (space));
      entry = lookup_pragma_entry (*chain, node);
      if (!entry)
	entry = insert_pragma_entry (pfile, chain, node, NULL);
      else if (!entry->is_nspace)
	goto clash;
      chain = &entry->u.space;
    }

  /* Check for duplicates.  */
  node = cpp_lookup (pfile, U name, strlen (name));
  entry = lookup_pragma_entry (*chain, node);
  if (entry)
    {
      if (entry->is_nspace)
	clash:
	cpp_ice (pfile,
		 "registering \"%s\" as both a pragma and a pragma namespace",
		 NODE_NAME (node));
      else if (space)
	cpp_ice (pfile, "#pragma %s %s is already registered", space, name);
      else
	cpp_ice (pfile, "#pragma %s is already registered", name);
    }
  else
    insert_pragma_entry (pfile, chain, node, handler);
}

/* Register the pragmas the preprocessor itself handles.  */
void
_cpp_init_internal_pragmas (pfile)
     cpp_reader *pfile;
{
  /* Pragmas in the global namespace.  */
  cpp_register_pragma (pfile, 0, "poison", do_pragma_poison);
  cpp_register_pragma (pfile, 0, "once", do_pragma_once);

  /* New GCC-specific pragmas should be put in the GCC namespace.  */
  cpp_register_pragma (pfile, "GCC", "poison", do_pragma_poison);
  cpp_register_pragma (pfile, "GCC", "system_header", do_pragma_system_header);
  cpp_register_pragma (pfile, "GCC", "dependency", do_pragma_dependency);
}

/* Pragmata handling.  We handle some, and pass the rest on to the
   front end.  C99 defines three pragmas and says that no macro
   expansion is to be performed on them; whether or not macro
   expansion happens for other pragmas is implementation defined.
   This implementation never macro-expands the text after #pragma.  */
static void
do_pragma (pfile)
     cpp_reader *pfile;
{
  const struct pragma_entry *p = NULL;
  const cpp_token *token;
  unsigned int count = 1;

  pfile->state.prevent_expansion++;

  token = cpp_get_token (pfile);
  if (token->type == CPP_NAME)
    {
      p = lookup_pragma_entry (pfile->pragmas, token->val.node);
      if (p && p->is_nspace)
	{
	  count = 2;
	  token = cpp_get_token (pfile);
	  if (token->type == CPP_NAME)
	    p = lookup_pragma_entry (p->u.space, token->val.node);
	  else
	    p = NULL;
	}
    }

  /* FIXME.  This is an awful kludge to get the front ends to update
     their notion of line number for diagnostic purposes.  The line
     number should be passed to the handler and they should do it
     themselves.  Stand-alone CPP must ignore us, otherwise it will
     prefix the directive with spaces, hence the 1.  Ugh.  */
  if (pfile->cb.line_change)
    (*pfile->cb.line_change)(pfile, token, 1);

  if (p)
    (*p->u.handler) (pfile);
  else if (pfile->cb.def_pragma)
    {
      _cpp_backup_tokens (pfile, count);
      (*pfile->cb.def_pragma) (pfile, pfile->directive_line);
    }

  pfile->state.prevent_expansion--;
}

/* Handle #pragma once.  */
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

/* Handle #pragma poison, to poison one or more identifiers so that
   the lexer produces a hard error for each subsequent usage.  */
static void
do_pragma_poison (pfile)
     cpp_reader *pfile;
{
  const cpp_token *tok;
  cpp_hashnode *hp;

  pfile->state.poisoned_ok = 1;
  for (;;)
    {
      tok = _cpp_lex_token (pfile);
      if (tok->type == CPP_EOF)
	break;
      if (tok->type != CPP_NAME)
	{
	  cpp_error (pfile, "invalid #pragma GCC poison directive");
	  break;
	}

      hp = tok->val.node;
      if (hp->flags & NODE_POISONED)
	continue;

      if (hp->type == NT_MACRO)
	cpp_warning (pfile, "poisoning existing macro \"%s\"", NODE_NAME (hp));
      _cpp_free_definition (hp);
      hp->flags |= NODE_POISONED | NODE_DIAGNOSTIC;
    }
  pfile->state.poisoned_ok = 0;
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
    {
      check_eol (pfile);
      skip_rest_of_line (pfile);
      cpp_make_system_header (pfile, 1, 0);
    }
}

/* Check the modified date of the current include file against a specified
   file. Issue a diagnostic, if the specified file is newer. We use this to
   determine if a fixed header should be refixed.  */
static void
do_pragma_dependency (pfile)
     cpp_reader *pfile;
{
  const cpp_token *header;
  int ordering;
 
  header = parse_include (pfile);
  if (!header)
    return;

  ordering = _cpp_compare_file_date (pfile, header);
  if (ordering < 0)
    cpp_warning (pfile, "cannot find source %s",
		 cpp_token_as_text (pfile, header));
  else if (ordering > 0)
    {
      cpp_warning (pfile, "current file is older than %s",
		   cpp_token_as_text (pfile, header));
      if (cpp_get_token (pfile)->type != CPP_EOF)
	{
	  _cpp_backup_tokens (pfile, 1);
	  do_diagnostic (pfile, WARNING, 0);
	}
    }
}

/* Get a token but skip padding.  */
static const cpp_token *
get_token_no_padding (pfile)
     cpp_reader *pfile;
{
  for (;;)
    {
      const cpp_token *result = cpp_get_token (pfile);
      if (result->type != CPP_PADDING)
	return result;
    }
}

/* Check syntax is "(string-literal)".  Returns the string on success,
   or NULL on failure.  */
static const cpp_token *
get__Pragma_string (pfile)
     cpp_reader *pfile;
{
  const cpp_token *string;

  if (get_token_no_padding (pfile)->type != CPP_OPEN_PAREN)
    return NULL;

  string = get_token_no_padding (pfile);
  if (string->type != CPP_STRING && string->type != CPP_WSTRING)
    return NULL;

  if (get_token_no_padding (pfile)->type != CPP_CLOSE_PAREN)
    return NULL;

  return string;
}

/* Destringize IN into a temporary buffer, by removing the first \ of
   \" and \\ sequences, and process the result as a #pragma directive.  */
static void
destringize_and_run (pfile, in)
     cpp_reader *pfile;
     const cpp_string *in;
{
  const unsigned char *src, *limit;
  char *dest, *result;

  dest = result = alloca (in->len + 1);
  for (src = in->text, limit = src + in->len; src < limit;)
    {
      /* We know there is a character following the backslash.  */
      if (*src == '\\' && (src[1] == '\\' || src[1] == '"'))
	src++;
      *dest++ = *src++;
    }
  *dest = '\0';

  run_directive (pfile, T_PRAGMA, result, dest - result);
}

/* Handle the _Pragma operator.  */
void
_cpp_do__Pragma (pfile)
     cpp_reader *pfile;
{
  const cpp_token *string = get__Pragma_string (pfile);

  if (!string)
    cpp_error (pfile, "_Pragma takes a parenthesized string literal");
  else
    {
      /* Ideally, we'd like
			token1 _Pragma ("foo") token2
	 to be output as
			token1
			# 7 "file.c"
			#pragma foo
			# 7 "file.c"
					       token2
	 Getting these correct line markers is a little tricky.  */

      unsigned int orig_line = pfile->line;
      destringize_and_run (pfile, &string->val.str);
      pfile->line = orig_line;
      pfile->buffer->saved_flags = BOL;
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

/* Handle #ifdef.  */
static void
do_ifdef (pfile)
     cpp_reader *pfile;
{
  int skip = 1;

  if (! pfile->state.skipping)
    {
      const cpp_hashnode *node = lex_macro_node (pfile);

      if (node)
	skip = node->type != NT_MACRO;

      if (node)
	check_eol (pfile);
    }

  push_conditional (pfile, skip, T_IFDEF, 0);
}

/* Handle #ifndef.  */
static void
do_ifndef (pfile)
     cpp_reader *pfile;
{
  int skip = 1;
  const cpp_hashnode *node = 0;

  if (! pfile->state.skipping)
    {
      node = lex_macro_node (pfile);
      if (node)
	skip = node->type == NT_MACRO;

      if (node)
	check_eol (pfile);
    }

  push_conditional (pfile, skip, T_IFNDEF, node);
}

/* _cpp_parse_expr puts a macro in a "#if !defined ()" expression in
   pfile->mi_ind_cmacro so we can handle multiple-include
   optimisations.  If macro expansion occurs in the expression, we
   cannot treat it as a controlling conditional, since the expansion
   could change in the future.  That is handled by cpp_get_token.  */
static void
do_if (pfile)
     cpp_reader *pfile;
{
  int skip = 1;

  if (! pfile->state.skipping)
    skip = _cpp_parse_expr (pfile) == 0;

  push_conditional (pfile, skip, T_IF, pfile->mi_ind_cmacro);
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
	  cpp_error_with_line (pfile, ifs->line, 0,
			       "the conditional began here");
	}
      ifs->type = T_ELSE;

      /* Skip any future (erroneous) #elses or #elifs.  */
      pfile->state.skipping = ifs->skip_elses;
      ifs->skip_elses = true;

      /* Invalidate any controlling macro.  */
      ifs->mi_cmacro = 0;

      /* Only check EOL if was not originally skipping.  */
      if (!ifs->was_skipping)
	check_eol (pfile);
    }
}

/* Handle a #elif directive by not changing if_stack either.  See the
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
	  cpp_error_with_line (pfile, ifs->line, 0,
			       "the conditional began here");
	}
      ifs->type = T_ELIF;

      /* Only evaluate this if we aren't skipping elses.  During
	 evaluation, set skipping to false to get lexer warnings.  */
      if (ifs->skip_elses)
	pfile->state.skipping = 1;
      else
	{
	  pfile->state.skipping = 0;
	  pfile->state.skipping = ! _cpp_parse_expr (pfile);
	  ifs->skip_elses = ! pfile->state.skipping;
	}

      /* Invalidate any controlling macro.  */
      ifs->mi_cmacro = 0;
    }
}

/* #endif pops the if stack and resets pfile->state.skipping.  */
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
      /* Only check EOL if was not originally skipping.  */
      if (!ifs->was_skipping)
	check_eol (pfile);

      /* If potential control macro, we go back outside again.  */
      if (ifs->next == 0 && ifs->mi_cmacro)
	{
	  pfile->mi_valid = true;
	  pfile->mi_cmacro = ifs->mi_cmacro;
	}

      buffer->if_stack = ifs->next;
      pfile->state.skipping = ifs->was_skipping;
      obstack_free (&pfile->buffer_ob, ifs);
    }
}

/* Push an if_stack entry for a preprocessor conditional, and set
   pfile->state.skipping to SKIP.  If TYPE indicates the conditional
   is #if or #ifndef, CMACRO is a potentially controlling macro, and
   we need to check here that we are at the top of the file.  */
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
  ifs->line = pfile->directive_line;
  ifs->next = buffer->if_stack;
  ifs->skip_elses = pfile->state.skipping || !skip;
  ifs->was_skipping = pfile->state.skipping;
  ifs->type = type;
  /* This condition is effectively a test for top-of-file.  */
  if (pfile->mi_valid && pfile->mi_cmacro == 0)
    ifs->mi_cmacro = cmacro;
  else
    ifs->mi_cmacro = 0;

  pfile->state.skipping = skip;
  buffer->if_stack = ifs;
}

/* Read the tokens of the answer into the macro pool, in a directive
   of type TYPE.  Only commit the memory if we intend it as permanent
   storage, i.e. the #assert case.  Returns 0 on success, and sets
   ANSWERP to point to the answer.  */
static int
parse_answer (pfile, answerp, type)
     cpp_reader *pfile;
     struct answer **answerp;
     int type;
{
  const cpp_token *paren;
  struct answer *answer;
  unsigned int acount;

  /* In a conditional, it is legal to not have an open paren.  We
     should save the following token in this case.  */
  paren = cpp_get_token (pfile);

  /* If not a paren, see if we're OK.  */
  if (paren->type != CPP_OPEN_PAREN)
    {
      /* In a conditional no answer is a test for any answer.  It
         could be followed by any token.  */
      if (type == T_IF)
	{
	  _cpp_backup_tokens (pfile, 1);
	  return 0;
	}

      /* #unassert with no answer is valid - it removes all answers.  */
      if (type == T_UNASSERT && paren->type == CPP_EOF)
	return 0;

      cpp_error (pfile, "missing '(' after predicate");
      return 1;
    }

  for (acount = 0;; acount++)
    {
      size_t room_needed;
      const cpp_token *token = cpp_get_token (pfile);
      cpp_token *dest;

      if (token->type == CPP_CLOSE_PAREN)
	break;

      if (token->type == CPP_EOF)
	{
	  cpp_error (pfile, "missing ')' to complete answer");
	  return 1;
	}

      /* struct answer includes the space for one token.  */
      room_needed = (sizeof (struct answer) + acount * sizeof (cpp_token));

      if (BUFF_ROOM (pfile->a_buff) < room_needed)
	_cpp_extend_buff (pfile, &pfile->a_buff, sizeof (struct answer));

      dest = &((struct answer *) BUFF_FRONT (pfile->a_buff))->first[acount];
      *dest = *token;

      /* Drop whitespace at start, for answer equivalence purposes.  */
      if (acount == 0)
	dest->flags &= ~PREV_WHITE;
    }

  if (acount == 0)
    {
      cpp_error (pfile, "predicate's answer is empty");
      return 1;
    }

  answer = (struct answer *) BUFF_FRONT (pfile->a_buff);
  answer->count = acount;
  answer->next = NULL;
  *answerp = answer;

  return 0;
}

/* Parses an assertion directive of type TYPE, returning a pointer to
   the hash node of the predicate, or 0 on error.  If an answer was
   supplied, it is placed in ANSWERP, otherwise it is set to 0.  */
static cpp_hashnode *
parse_assertion (pfile, answerp, type)
     cpp_reader *pfile;
     struct answer **answerp;
     int type;
{
  cpp_hashnode *result = 0;
  const cpp_token *predicate;

  /* We don't expand predicates or answers.  */
  pfile->state.prevent_expansion++;

  *answerp = 0;
  predicate = cpp_get_token (pfile);
  if (predicate->type == CPP_EOF)
    cpp_error (pfile, "assertion without predicate");
  else if (predicate->type != CPP_NAME)
    cpp_error (pfile, "predicate must be an identifier");
  else if (parse_answer (pfile, answerp, type) == 0)
    {
      unsigned int len = NODE_LEN (predicate->val.node);
      unsigned char *sym = alloca (len + 1);

      /* Prefix '#' to get it out of macro namespace.  */
      sym[0] = '#';
      memcpy (sym + 1, NODE_NAME (predicate->val.node), len);
      result = cpp_lookup (pfile, sym, len + 1);
    }

  pfile->state.prevent_expansion--;
  return result;
}

/* Returns a pointer to the pointer to CANDIDATE in the answer chain,
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

/* Handle #assert.  */
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
      BUFF_FRONT (pfile->a_buff) += (sizeof (struct answer)
				     + (new_answer->count - 1)
				     * sizeof (cpp_token));
      check_eol (pfile);
    }
}

/* Handle #unassert.  */
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

	  check_eol (pfile);
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
   be identifier=definition.  */
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

  count = strlen (str);
  buf = (char *) alloca (count + 3);
  memcpy (buf, str, count);

  p = strchr (str, '=');
  if (p)
    buf[p - str] = ' ';
  else
    {
      buf[count++] = ' ';
      buf[count++] = '1';
    }
  buf[count] = '\0';

  run_directive (pfile, T_DEFINE, buf, count);
}

/* Slight variant of the above for use by initialize_builtins.  */
void
_cpp_define_builtin (pfile, str)
     cpp_reader *pfile;
     const char *str;
{
  run_directive (pfile, T_DEFINE, str, strlen (str));
}

/* Process MACRO as if it appeared as the body of an #undef.  */
void
cpp_undef (pfile, macro)
     cpp_reader *pfile;
     const char *macro;
{
  run_directive (pfile, T_UNDEF, macro, strlen (macro));
}

/* Process the string STR as if it appeared as the body of a #assert.  */
void
cpp_assert (pfile, str)
     cpp_reader *pfile;
     const char *str;
{
  handle_assertion (pfile, str, T_ASSERT);
}

/* Process STR as if it appeared as the body of an #unassert.  */
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
      char *buf = (char *) alloca (count + 2);

      memcpy (buf, str, count);
      buf[p - str] = '(';
      buf[count++] = ')';
      buf[count] = '\0';
      str = buf;
    }

  run_directive (pfile, type, str, count);
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

/* The line map set.  */
const struct line_maps *
cpp_get_line_maps (pfile)
     cpp_reader *pfile;
{
  return &pfile->line_maps;
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
cpp_push_buffer (pfile, buffer, len, from_stage3, return_at_eof)
     cpp_reader *pfile;
     const U_CHAR *buffer;
     size_t len;
     int from_stage3;
     int return_at_eof;
{
  cpp_buffer *new = xobnew (&pfile->buffer_ob, cpp_buffer);

  /* Clears, amongst other things, if_stack and mi_cmacro.  */
  memset (new, 0, sizeof (cpp_buffer));

  new->line_base = new->buf = new->cur = buffer;
  new->rlimit = buffer + len;
  new->from_stage3 = from_stage3;
  new->prev = pfile->buffer;
  new->return_at_eof = return_at_eof;
  new->saved_flags = BOL;

  pfile->buffer = new;

  return new;
}

/* If called from do_line, pops a single buffer.  Otherwise pops all
   buffers until a real file is reached.  Generates appropriate
   call-backs.  */
void
_cpp_pop_buffer (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *buffer = pfile->buffer;
  struct if_stack *ifs;
  bool pushed = false;

  /* Walk back up the conditional stack till we reach its level at
     entry to this file, issuing error messages.  */
  for (ifs = buffer->if_stack; ifs; ifs = ifs->next)
    cpp_error_with_line (pfile, ifs->line, 0,
			 "unterminated #%s", dtable[ifs->type].name);

  /* In case of a missing #endif.  */
  pfile->state.skipping = 0;

  /* Update the reader's buffer before _cpp_do_file_change.  */
  pfile->buffer = buffer->prev;

  if (buffer->inc)
    pushed = _cpp_pop_file_buffer (pfile, buffer->inc);

  if (!pushed)
    obstack_free (&pfile->buffer_ob, buffer);
}

/* Enter all recognised directives in the hash table.  */
void
_cpp_init_directives (pfile)
     cpp_reader *pfile;
{
  unsigned int i;
  cpp_hashnode *node;

  for (i = 0; i < (unsigned int) N_DIRECTIVES; i++)
    {
      node = cpp_lookup (pfile, dtable[i].name, dtable[i].length);
      node->directive_index = i + 1;
    }
}
