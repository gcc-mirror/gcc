/* CPP Library - non-diagnostic output.
   Copyright (C) 2000 Free Software Foundation, Inc.
   Contributed by Per Bothner, 1994-95.
   Based on CCCP program by Paul Rubin, June 1986
   Adapted to ANSI C, Richard Stallman, Jan 1987
   Broken out to separate file, Sep 2000

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
#include "intl.h"
#include "cpplib.h"
#include "cpphash.h"

static void output_line_command PARAMS ((cpp_reader *, cpp_printer *,
					 unsigned int));
static void output_token PARAMS ((cpp_reader *, FILE *, const cpp_token *,
				  const cpp_token *, int));
static void dump_macro_args PARAMS ((FILE *, const cpp_toklist *));
static void dump_param_spelling PARAMS ((FILE *, const cpp_toklist *,
					 unsigned int));

/* Scan until CPP_BUFFER (PFILE) is exhausted, discarding output.  Used
   for handling -imacros, -dM, -M and -MM.  */
void
cpp_scan_buffer_nooutput (pfile)
     cpp_reader *pfile;
{
  cpp_buffer *stop = CPP_PREV_BUFFER (CPP_BUFFER (pfile));
  const cpp_token *token;

  /* In no-output mode, we can ignore everything but directives.  */
  for (;;)
    {
      token = _cpp_get_token (pfile);

      if (token->type == CPP_EOF)
	{
	  cpp_pop_buffer (pfile);
	  if (CPP_BUFFER (pfile) == stop)
	    break;
	}

      if (token->type == CPP_HASH && token->flags & BOL
	  && pfile->token_list.directive)
	{
	  _cpp_process_directive (pfile, token);
	  continue;
	}

      _cpp_skip_rest_of_line (pfile);
    }
}

/* Scan until CPP_BUFFER (pfile) is exhausted, writing output to PRINT.  */
void
cpp_scan_buffer (pfile, print)
     cpp_reader *pfile;
     cpp_printer *print;
{
  cpp_buffer *stop = CPP_PREV_BUFFER (CPP_BUFFER (pfile));
  const cpp_token *token, *prev = 0;

  for (;;)
    {
      token = _cpp_get_token (pfile);
      if (token->type == CPP_EOF)
	{
	  cpp_pop_buffer (pfile);

	  if (CPP_BUFFER (pfile) == stop)
	    return;

	  prev = 0;
	  continue;
	}

      if (token->flags & BOL)
	{
	  output_line_command (pfile, print, token->line);
	  prev = 0;

	  if (token->type == CPP_HASH && pfile->token_list.directive)
	    {
	      _cpp_process_directive (pfile, token);
	      continue;
	    }
	}

      if (token->type != CPP_PLACEMARKER)
	{
	  output_token (pfile, print->outf, token, prev, 1);
	  pfile->need_newline = 1;
	}

      prev = token;
    }
}

/* Notify the compiler proper that the current line number has jumped,
   or the current file name has changed.  */
static void
output_line_command (pfile, print, line)
     cpp_reader *pfile;
     cpp_printer *print;
     unsigned int line;
{
  cpp_buffer *ip = CPP_BUFFER (pfile);

  if (line == 0)
    return;

  /* End the previous line of text.  */
  if (pfile->need_newline)
    {
      putc ('\n', print->outf);
      print->lineno++;
    }
  pfile->need_newline = 0;

  if (CPP_OPTION (pfile, no_line_commands))
    return;

  /* If the current file has not changed, we can output a few newlines
     instead if we want to increase the line number by a small amount.
     We cannot do this if print->lineno is zero, because that means we
     haven't output any line commands yet.  (The very first line
     command output is a `same_file' command.)

     'nominal_fname' values are unique, so they can be compared by
     comparing pointers.  */
  if (ip->nominal_fname == print->last_fname && print->lineno > 0
      && line >= print->lineno && line < print->lineno + 8)
    {
      while (line > print->lineno)
	{
	  putc ('\n', print->outf);
	  print->lineno++;
	}
      return;
    }

  fprintf (print->outf, "# %u \"%s\"%s\n", line, ip->nominal_fname,
	   cpp_syshdr_flags (pfile, ip));

  print->last_fname = ip->nominal_fname;
  print->lineno = line;
}

/* Output all the tokens of LIST, starting at TOKEN, to FP.  */
void
cpp_output_list (pfile, fp, list, token)
     cpp_reader *pfile;
     FILE *fp;
     const cpp_toklist *list;
     const cpp_token *token;
{
  const cpp_token *limit = list->tokens + list->tokens_used;
  const cpp_token *prev = 0;
  int white = 0;

  while (token < limit)
    {
      /* XXX Find some way we can write macro args from inside
	 output_token/spell_token.  */
      if (token->type == CPP_MACRO_ARG)
	{
	  if (white && token->flags & PREV_WHITE)
	    putc (' ', fp);
	  if (token->flags & STRINGIFY_ARG)
	    putc ('#', fp);
	  dump_param_spelling (fp, list, token->val.aux);
	}
      else
	output_token (pfile, fp, token, prev, white);
      if (token->flags & PASTE_LEFT)
	fputs (" ##", fp);
      prev = token;
      token++;
      white = 1;
    }
}

/* Write the spelling of a token TOKEN, with any appropriate
   whitespace before it, to FP.  PREV is the previous token, which
   is used to determine if we need to shove in an extra space in order
   to avoid accidental token paste.  If WHITE is 0, do not insert any
   leading whitespace.  */
static void
output_token (pfile, fp, token, prev, white)
     cpp_reader *pfile;
     FILE *fp;
     const cpp_token *token, *prev;
     int white;
{
  if (white)
    {
      int dummy;

      if (token->col && (token->flags & BOL))
	{
	  /* Supply enough whitespace to put this token in its original
	     column.  Don't bother trying to reconstruct tabs; we can't
	     get it right in general, and nothing ought to care.  (Yes,
	     some things do care; the fault lies with them.)  */
	  unsigned int spaces = token->col - 1;
      
	  while (spaces--)
	    putc (' ', fp);
	}
      else if (token->flags & PREV_WHITE)
	putc (' ', fp);
      else
      /* Check for and prevent accidental token pasting.
	 In addition to the cases handled by _cpp_can_paste, consider

	 a + ++b - if there is not a space between the + and ++, it
	 will be misparsed as a++ + b.  But + ## ++ doesn't produce
	 a valid token.  */
	if (prev
	    && (_cpp_can_paste (pfile, prev, token, &dummy) != CPP_EOF
		|| (prev->type == CPP_PLUS && token->type == CPP_PLUS_PLUS)
		|| (prev->type == CPP_MINUS && token->type == CPP_MINUS_MINUS)))
	putc (' ', fp);
    }

  switch (TOKEN_SPELL (token))
    {
    case SPELL_OPERATOR:
      {
	const unsigned char *spelling;

	if (token->flags & DIGRAPH)
	  spelling = _cpp_digraph_spellings[token->type - CPP_FIRST_DIGRAPH];
	else if (token->flags & NAMED_OP)
	  goto spell_ident;
	else
	  spelling = TOKEN_NAME (token);

	ufputs (spelling, fp);
      }
      break;

    case SPELL_IDENT:
      spell_ident:
      ufputs (token->val.node->name, fp);
      break;

    case SPELL_STRING:
      {
	int left, right, tag;
	switch (token->type)
	  {
	  case CPP_STRING:	left = '"';  right = '"';  tag = '\0'; break;
	  case CPP_WSTRING:	left = '"';  right = '"';  tag = 'L';  break;
	  case CPP_OSTRING:	left = '"';  right = '"';  tag = '@';  break;
	  case CPP_CHAR:	left = '\''; right = '\''; tag = '\0'; break;
    	  case CPP_WCHAR:	left = '\''; right = '\''; tag = 'L';  break;
	  case CPP_HEADER_NAME:	left = '<';  right = '>';  tag = '\0'; break;
	  default:		left = '\0'; right = '\0'; tag = '\0'; break;
	  }
	if (tag) putc (tag, fp);
	if (left) putc (left, fp);
	fwrite (token->val.str.text, 1, token->val.str.len, fp);
	if (right) putc (right, fp);
      }
      break;

    case SPELL_CHAR:
      putc (token->val.aux, fp);
      break;

    case SPELL_NONE:
      /* Placemarker or EOF - no output.  (Macro args are handled
         elsewhere.  */
      break;
    }
}

/* Dump the original user's spelling of argument index ARG_NO to the
   macro whose expansion is LIST.  */
static void
dump_param_spelling (fp, list, arg_no)
     FILE *fp;
     const cpp_toklist *list;
     unsigned int arg_no;
{
  const U_CHAR *param = list->namebuf;

  while (arg_no--)
    param += ustrlen (param) + 1;
  ufputs (param, fp);
}

/* Dump the definition of macro MACRO on FP.  The format is suitable
   to be read back in again.  Caller is expected to generate the
   "#define NAME" bit.  */

void
cpp_dump_definition (pfile, fp, hp)
     cpp_reader *pfile;
     FILE *fp;
     const cpp_hashnode *hp;
{
  const cpp_toklist *list = hp->value.expansion;

  if (hp->type != T_MACRO)
    {
      cpp_ice (pfile, "invalid hash type %d in dump_definition", hp->type);
      return;
    }

  if (list->paramc >= 0)
    dump_macro_args (fp, list);

  putc (' ', fp);
  cpp_output_list (pfile, fp, list, list->tokens);
}

static void
dump_macro_args (fp, list)
     FILE *fp;
     const cpp_toklist *list;
{
  int i;
  const U_CHAR *param = list->namebuf;

  putc ('(', fp);
  for (i = 0; i++ < list->paramc;)
    {
      unsigned int len;

      len = ustrlen (param);
      if (!(list->flags & VAR_ARGS) || ustrcmp (param, U"__VA_ARGS__"))
	ufputs (param, fp);
      if (i < list->paramc)
	fputs (", ", fp);
      else if (list->flags & VAR_ARGS)
	fputs ("...", fp);

      param += len + 1;
    }
  putc (')', fp);
}

/* Like fprintf, but writes to a printer object.  You should be sure
   always to generate a complete line when you use this function.  */
void
cpp_printf VPARAMS ((cpp_reader *pfile, cpp_printer *print,
		     const char *fmt, ...))
{
  va_list ap;
#ifndef ANSI_PROTOTYPES
  cpp_reader *pfile;
  cpp_printer *print;
  const char *fmt;
#endif

  VA_START (ap, fmt);

#ifndef ANSI_PROTOTYPES
  pfile = va_arg (ap, cpp_reader *);
  print = va_arg (ap, cpp_printer *);
  fmt = va_arg (ap, const char *);
#endif

  /* End the previous line of text.  */
  if (pfile->need_newline)
    {
      putc ('\n', print->outf);
      print->lineno++;
    }
  pfile->need_newline = 0;

  vfprintf (print->outf, fmt, ap);
  va_end (ap);
}
