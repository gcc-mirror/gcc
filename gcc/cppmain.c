/* Preprocess only, using cpplib.
   Copyright (C) 1995, 1997, 1998, 1999, 2000, 2001, 2002
   Free Software Foundation, Inc.
   Written by Per Bothner, 1994-95.

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
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

 In other words, you are welcome to use, share and improve this program.
 You are forbidden to forbid anyone else to use, share and improve
 what you give them.   Help stamp out software-hoarding!  */

#include "config.h"
#include "system.h"
#include "cpplib.h"
#include "cpphash.h"

static void setup_callbacks PARAMS ((cpp_reader *));

/* General output routines.  */
static void scan_translation_unit PARAMS ((cpp_reader *));
static void scan_translation_unit_trad PARAMS ((cpp_reader *));
static void account_for_newlines PARAMS ((cpp_reader *, const uchar *,
					  size_t));
static int dump_macro PARAMS ((cpp_reader *, cpp_hashnode *, void *));

static void print_line PARAMS ((cpp_reader *, const struct line_map *,
				unsigned int, const char *));
static void maybe_print_line PARAMS ((cpp_reader *, const struct line_map *,
				      unsigned int));

/* Callback routines for the parser.   Most of these are active only
   in specific modes.  */
static void cb_line_change PARAMS ((cpp_reader *, const cpp_token *, int));
static void cb_define	PARAMS ((cpp_reader *, unsigned int, cpp_hashnode *));
static void cb_undef	PARAMS ((cpp_reader *, unsigned int, cpp_hashnode *));
static void cb_include	PARAMS ((cpp_reader *, unsigned int,
				 const unsigned char *, const cpp_token *));
static void cb_ident	  PARAMS ((cpp_reader *, unsigned int,
				   const cpp_string *));
static void cb_file_change PARAMS ((cpp_reader *, const struct line_map *));
static void cb_def_pragma PARAMS ((cpp_reader *, unsigned int));

/* Preprocess and output.  */
void
cpp_preprocess_file (pfile, in_fname, out_stream)
     cpp_reader *pfile;
     const char *in_fname;
     FILE *out_stream;
{
  /* Initialize the printer structure.  Setting print.line to -1 here
     is a trick to guarantee that the first token of the file will
     cause a linemarker to be output by maybe_print_line.  */
  pfile->print.line = (unsigned int) -1;
  pfile->print.printed = 0;
  pfile->print.prev = 0;
  pfile->print.map = 0;
  pfile->print.outf = out_stream;

  setup_callbacks (pfile);

  if (cpp_read_main_file (pfile, in_fname, NULL))
    {
      cpp_options *options = &pfile->opts;
      cpp_finish_options (pfile);

      /* A successful cpp_read_main_file guarantees that we can call
	 cpp_scan_nooutput or cpp_get_token next.  */
      if (options->no_output)
	{
	  /* Scan -included buffers, then the main file.  */
	  while (pfile->buffer->prev)
	    cpp_scan_nooutput (pfile);
	  cpp_scan_nooutput (pfile);
	}
      else if (options->traditional)
	scan_translation_unit_trad (pfile);
      else
	scan_translation_unit (pfile);

      /* -dM command line option.  Should this be in cpp_finish?  */
      if (options->dump_macros == dump_only)
	cpp_forall_identifiers (pfile, dump_macro, NULL);
    }

  /* Flush any pending output.  */
  if (pfile->print.printed)
    putc ('\n', pfile->print.outf);
}

/* Set up the callbacks as appropriate.  */
static void
setup_callbacks (pfile)
     cpp_reader *pfile;
{
  cpp_options *options = &pfile->opts;
  cpp_callbacks *cb = cpp_get_callbacks (pfile);

  if (! options->no_output)
    {
      cb->line_change = cb_line_change;
      /* Don't emit #pragma or #ident directives if we are processing
	 assembly language; the assembler may choke on them.  */
      if (options->lang != CLK_ASM)
	{
	  cb->ident      = cb_ident;
	  cb->def_pragma = cb_def_pragma;
	}
      if (! options->no_line_commands)
	cb->file_change = cb_file_change;
    }

  if (options->dump_includes)
    cb->include  = cb_include;

  if (options->dump_macros == dump_names
      || options->dump_macros == dump_definitions)
    {
      cb->define = cb_define;
      cb->undef  = cb_undef;
    }
}

/* Writes out the preprocessed file, handling spacing and paste
   avoidance issues.  */
static void
scan_translation_unit (pfile)
     cpp_reader *pfile;
{
  bool avoid_paste = false;

  pfile->print.source = NULL;
  for (;;)
    {
      const cpp_token *token = cpp_get_token (pfile);

      if (token->type == CPP_PADDING)
	{
	  avoid_paste = true;
	  if (pfile->print.source == NULL
	      || (!(pfile->print.source->flags & PREV_WHITE)
		  && token->val.source == NULL))
	    pfile->print.source = token->val.source;
	  continue;
	}

      if (token->type == CPP_EOF)
	break;

      /* Subtle logic to output a space if and only if necessary.  */
      if (avoid_paste)
	{
	  if (pfile->print.source == NULL)
	    pfile->print.source = token;
	  if (pfile->print.source->flags & PREV_WHITE
	      || (pfile->print.prev
		  && cpp_avoid_paste (pfile, pfile->print.prev, token))
	      || (pfile->print.prev == NULL && token->type == CPP_HASH))
	    putc (' ', pfile->print.outf);
	}
      else if (token->flags & PREV_WHITE)
	putc (' ', pfile->print.outf);

      avoid_paste = false;
      pfile->print.source = NULL;
      pfile->print.prev = token;
      cpp_output_token (token, pfile->print.outf);

      if (token->type == CPP_COMMENT)
	account_for_newlines (pfile, token->val.str.text, token->val.str.len);
    }
}

/* Adjust pfile->print.line for newlines embedded in output.  */
static void
account_for_newlines (pfile, str, len)
     cpp_reader *pfile;
     const uchar *str;
     size_t len;
{
  while (len--)
    if (*str++ == '\n')
      pfile->print.line++;
}

/* Writes out a traditionally preprocessed file.  */
static void
scan_translation_unit_trad (pfile)
     cpp_reader *pfile;
{
  while (_cpp_read_logical_line_trad (pfile))
    {
      size_t len = pfile->out.cur - pfile->out.base;
      maybe_print_line (pfile, pfile->print.map, pfile->out.first_line);
      fwrite (pfile->out.base, 1, len, pfile->print.outf);
      pfile->print.printed = 1;
      if (!CPP_OPTION (pfile, discard_comments))
	account_for_newlines (pfile, pfile->out.base, len);
    }
}

/* If the token read on logical line LINE needs to be output on a
   different line to the current one, output the required newlines or
   a line marker, and return 1.  Otherwise return 0.  */
static void
maybe_print_line (pfile, map, line)
     cpp_reader *pfile;
     const struct line_map *map;
     unsigned int line;
{
  /* End the previous line of text.  */
  if (pfile->print.printed)
    {
      putc ('\n', pfile->print.outf);
      pfile->print.line++;
      pfile->print.printed = 0;
    }

  if (line >= pfile->print.line && line < pfile->print.line + 8)
    {
      while (line > pfile->print.line)
	{
	  putc ('\n', pfile->print.outf);
	  pfile->print.line++;
	}
    }
  else
    print_line (pfile, map, line, "");
}

/* Output a line marker for logical line LINE.  Special flags are "1"
   or "2" indicating entering or leaving a file.  */
static void
print_line (pfile, map, line, special_flags)
     cpp_reader *pfile;
     const struct line_map *map;
     unsigned int line;
     const char *special_flags;
{
  /* End any previous line of text.  */
  if (pfile->print.printed)
    putc ('\n', pfile->print.outf);
  pfile->print.printed = 0;

  pfile->print.line = line;
  if (! CPP_OPTION (pfile, no_line_commands))
    {
      size_t to_file_len = strlen (map->to_file);
      unsigned char *to_file_quoted = alloca (to_file_len * 4 + 1);
      unsigned char *p;

      /* cpp_quote_string does not nul-terminate, so we have to do it
	 ourselves.  */
      p = cpp_quote_string (to_file_quoted,
			    (unsigned char *)map->to_file, to_file_len);
      *p = '\0';
      fprintf (pfile->print.outf, "# %u \"%s\"%s",
	       SOURCE_LINE (map, pfile->print.line),
	       to_file_quoted, special_flags);

      if (map->sysp == 2)
	fputs (" 3 4", pfile->print.outf);
      else if (map->sysp == 1)
	fputs (" 3", pfile->print.outf);

      putc ('\n', pfile->print.outf);
    }
}

/* Called when a line of output is started.  TOKEN is the first token
   of the line, and at end of file will be CPP_EOF.  */
static void
cb_line_change (pfile, token, parsing_args)
     cpp_reader *pfile;
     const cpp_token *token;
     int parsing_args;
{
  if (token->type == CPP_EOF || parsing_args)
    return;

  maybe_print_line (pfile, pfile->print.map, token->line);
  pfile->print.prev = 0;
  pfile->print.source = 0;

  /* Supply enough spaces to put this token in its original column,
     one space per column greater than 2, since scan_translation_unit
     will provide a space if PREV_WHITE.  Don't bother trying to
     reconstruct tabs; we can't get it right in general, and nothing
     ought to care.  Some things do care; the fault lies with them.  */
  if (!CPP_OPTION (pfile, traditional))
    {
      pfile->print.printed = 1;
      if (token->col > 2)
	{
	  unsigned int spaces = token->col - 2;

	  while (spaces--)
	    putc (' ', pfile->print.outf);
	}
    }
}

static void
cb_ident (pfile, line, str)
     cpp_reader *pfile;
     unsigned int line;
     const cpp_string * str;
{
  maybe_print_line (pfile, pfile->print.map, line);
  fprintf (pfile->print.outf, "#ident \"%s\"\n", str->text);
  pfile->print.line++;
}

static void
cb_define (pfile, line, node)
     cpp_reader *pfile;
     unsigned int line;
     cpp_hashnode *node;
{
  maybe_print_line (pfile, pfile->print.map, line);
  fputs ("#define ", pfile->print.outf);

  /* -dD command line option.  */
  if (CPP_OPTION (pfile, dump_macros) == dump_definitions)
    fputs ((const char *) cpp_macro_definition (pfile, node),
	   pfile->print.outf);
  else
    fputs ((const char *) NODE_NAME (node), pfile->print.outf);

  putc ('\n', pfile->print.outf);
  pfile->print.line++;
}

static void
cb_undef (pfile, line, node)
     cpp_reader *pfile;
     unsigned int line;
     cpp_hashnode *node;
{
  maybe_print_line (pfile, pfile->print.map, line);
  fprintf (pfile->print.outf, "#undef %s\n", NODE_NAME (node));
  pfile->print.line++;
}

static void
cb_include (pfile, line, dir, header)
     cpp_reader *pfile;
     unsigned int line;
     const unsigned char *dir;
     const cpp_token *header;
{
  maybe_print_line (pfile, pfile->print.map, line);
  fprintf (pfile->print.outf, "#%s %s\n", dir,
	   cpp_token_as_text (pfile, header));
  pfile->print.line++;
}

/* The file name, line number or system header flags have changed, as
   described in MAP.  From this point on, the old pfile->print.map might be
   pointing to freed memory, and so must not be dereferenced.  */

static void
cb_file_change (pfile, map)
     cpp_reader *pfile;
     const struct line_map *map;
{
  const char *flags = "";

  /* First time?  */
  if (pfile->print.map == NULL)
    {
      /* Avoid printing foo.i when the main file is foo.c.  */
      if (!CPP_OPTION (pfile, preprocessed))
	print_line (pfile, map, map->from_line, flags);
    }
  else
    {
      /* Bring current file to correct line when entering a new file.  */
      if (map->reason == LC_ENTER)
	maybe_print_line (pfile, map - 1, map->from_line - 1);

      if (map->reason == LC_ENTER)
	flags = " 1";
      else if (map->reason == LC_LEAVE)
	flags = " 2";
      print_line (pfile, map, map->from_line, flags);
    }

  pfile->print.map = map;
}

/* Copy a #pragma directive to the preprocessed output.  */
static void
cb_def_pragma (pfile, line)
     cpp_reader *pfile;
     unsigned int line;
{
  maybe_print_line (pfile, pfile->print.map, line);
  fputs ("#pragma ", pfile->print.outf);
  cpp_output_line (pfile, pfile->print.outf);
  pfile->print.line++;
}

/* Dump out the hash table.  */
static int
dump_macro (pfile, node, v)
     cpp_reader *pfile;
     cpp_hashnode *node;
     void *v ATTRIBUTE_UNUSED;
{
  if (node->type == NT_MACRO && !(node->flags & NODE_BUILTIN))
    {
      fputs ("#define ", pfile->print.outf);
      fputs ((const char *) cpp_macro_definition (pfile, node),
	     pfile->print.outf);
      putc ('\n', pfile->print.outf);
      pfile->print.line++;
    }

  return 1;
}
