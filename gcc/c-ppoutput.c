/* Preprocess only, using cpplib.
   Copyright (C) 1995, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004
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
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "cpplib.h"
#include "cpphash.h"
#include "tree.h"
#include "c-common.h"		/* For flags.  */
#include "c-pragma.h"		/* For parse_in.  */

/* Encapsulates state used to convert a stream of tokens into a text
   file.  */
static struct
{
  FILE *outf;			/* Stream to write to.  */
  const struct line_map *map;	/* Logical to physical line mappings.  */
  const cpp_token *prev;	/* Previous token.  */
  const cpp_token *source;	/* Source token for spacing.  */
  fileline line;		/* Line currently being written.  */
  unsigned char printed;	/* Nonzero if something output at line.  */
} print;

/* General output routines.  */
static void scan_translation_unit (cpp_reader *);
static void scan_translation_unit_trad (cpp_reader *);
static void account_for_newlines (const unsigned char *, size_t);
static int dump_macro (cpp_reader *, cpp_hashnode *, void *);

static void print_line (const struct line_map *, fileline, const char *);
static void maybe_print_line (const struct line_map *, fileline);

/* Callback routines for the parser.   Most of these are active only
   in specific modes.  */
static void cb_line_change (cpp_reader *, const cpp_token *, int);
static void cb_define (cpp_reader *, fileline, cpp_hashnode *);
static void cb_undef (cpp_reader *, fileline, cpp_hashnode *);
static void cb_include (cpp_reader *, fileline, const unsigned char *,
			const char *, int);
static void cb_ident (cpp_reader *, fileline, const cpp_string *);
static void cb_def_pragma (cpp_reader *, fileline);

/* Preprocess and output.  */
void
preprocess_file (cpp_reader *pfile)
{
  /* A successful cpp_read_main_file guarantees that we can call
     cpp_scan_nooutput or cpp_get_token next.  */
  if (flag_no_output)
    {
      /* Scan -included buffers, then the main file.  */
      while (pfile->buffer->prev)
	cpp_scan_nooutput (pfile);
      cpp_scan_nooutput (pfile);
    }
  else if (cpp_get_options (pfile)->traditional)
    scan_translation_unit_trad (pfile);
  else
    scan_translation_unit (pfile);

  /* -dM command line option.  Should this be elsewhere?  */
  if (flag_dump_macros == 'M')
    cpp_forall_identifiers (pfile, dump_macro, NULL);

  /* Flush any pending output.  */
  if (print.printed)
    putc ('\n', print.outf);
}

/* Set up the callbacks as appropriate.  */
void
init_pp_output (FILE *out_stream)
{
  cpp_callbacks *cb = cpp_get_callbacks (parse_in);

  if (!flag_no_output)
    {
      cb->line_change = cb_line_change;
      /* Don't emit #pragma or #ident directives if we are processing
	 assembly language; the assembler may choke on them.  */
      if (cpp_get_options (parse_in)->lang != CLK_ASM)
	{
	  cb->ident      = cb_ident;
	  cb->def_pragma = cb_def_pragma;
	}
    }

  if (flag_dump_includes)
    cb->include  = cb_include;

  if (flag_dump_macros == 'N' || flag_dump_macros == 'D')
    {
      cb->define = cb_define;
      cb->undef  = cb_undef;
    }

  /* Initialize the print structure.  Setting print.line to -1 here is
     a trick to guarantee that the first token of the file will cause
     a linemarker to be output by maybe_print_line.  */
  print.line = (fileline) -1;
  print.printed = 0;
  print.prev = 0;
  print.map = 0;
  print.outf = out_stream;
}

/* Writes out the preprocessed file, handling spacing and paste
   avoidance issues.  */
static void
scan_translation_unit (cpp_reader *pfile)
{
  bool avoid_paste = false;

  print.source = NULL;
  for (;;)
    {
      const cpp_token *token = cpp_get_token (pfile);

      if (token->type == CPP_PADDING)
	{
	  avoid_paste = true;
	  if (print.source == NULL
	      || (!(print.source->flags & PREV_WHITE)
		  && token->val.source == NULL))
	    print.source = token->val.source;
	  continue;
	}

      if (token->type == CPP_EOF)
	break;

      /* Subtle logic to output a space if and only if necessary.  */
      if (avoid_paste)
	{
	  if (print.source == NULL)
	    print.source = token;
	  if (print.source->flags & PREV_WHITE
	      || (print.prev
		  && cpp_avoid_paste (pfile, print.prev, token))
	      || (print.prev == NULL && token->type == CPP_HASH))
	    putc (' ', print.outf);
	}
      else if (token->flags & PREV_WHITE)
	putc (' ', print.outf);

      avoid_paste = false;
      print.source = NULL;
      print.prev = token;
      cpp_output_token (token, print.outf);

      if (token->type == CPP_COMMENT)
	account_for_newlines (token->val.str.text, token->val.str.len);
    }
}

/* Adjust print.line for newlines embedded in output.  */
static void
account_for_newlines (const unsigned char *str, size_t len)
{
  while (len--)
    if (*str++ == '\n')
      print.line++;
}

/* Writes out a traditionally preprocessed file.  */
static void
scan_translation_unit_trad (cpp_reader *pfile)
{
  while (_cpp_read_logical_line_trad (pfile))
    {
      size_t len = pfile->out.cur - pfile->out.base;
      maybe_print_line (print.map, pfile->out.first_line);
      fwrite (pfile->out.base, 1, len, print.outf);
      print.printed = 1;
      if (!CPP_OPTION (pfile, discard_comments))
	account_for_newlines (pfile->out.base, len);
    }
}

/* If the token read on logical line LINE needs to be output on a
   different line to the current one, output the required newlines or
   a line marker, and return 1.  Otherwise return 0.  */
static void
maybe_print_line (const struct line_map *map, fileline line)
{
  /* End the previous line of text.  */
  if (print.printed)
    {
      putc ('\n', print.outf);
      print.line++;
      print.printed = 0;
    }

  if (line >= print.line && line < print.line + 8)
    {
      while (line > print.line)
	{
	  putc ('\n', print.outf);
	  print.line++;
	}
    }
  else
    print_line (map, line, "");
}

/* Output a line marker for logical line LINE.  Special flags are "1"
   or "2" indicating entering or leaving a file.  */
static void
print_line (const struct line_map *map, fileline line, const char *special_flags)
{
  /* End any previous line of text.  */
  if (print.printed)
    putc ('\n', print.outf);
  print.printed = 0;

  print.line = line;
  if (!flag_no_line_commands)
    {
      size_t to_file_len = strlen (map->to_file);
      unsigned char *to_file_quoted = alloca (to_file_len * 4 + 1);
      unsigned char *p;

      /* cpp_quote_string does not nul-terminate, so we have to do it
	 ourselves.  */
      p = cpp_quote_string (to_file_quoted,
			    (unsigned char *)map->to_file, to_file_len);
      *p = '\0';
      fprintf (print.outf, "# %u \"%s\"%s",
	       SOURCE_LINE (map, print.line),
	       to_file_quoted, special_flags);

      if (map->sysp == 2)
	fputs (" 3 4", print.outf);
      else if (map->sysp == 1)
	fputs (" 3", print.outf);

      putc ('\n', print.outf);
    }
}

/* Called when a line of output is started.  TOKEN is the first token
   of the line, and at end of file will be CPP_EOF.  */
static void
cb_line_change (cpp_reader *pfile, const cpp_token *token,
		int parsing_args)
{
  if (token->type == CPP_EOF || parsing_args)
    return;

  maybe_print_line (print.map, token->line);
  print.prev = 0;
  print.source = 0;

  /* Supply enough spaces to put this token in its original column,
     one space per column greater than 2, since scan_translation_unit
     will provide a space if PREV_WHITE.  Don't bother trying to
     reconstruct tabs; we can't get it right in general, and nothing
     ought to care.  Some things do care; the fault lies with them.  */
  if (!CPP_OPTION (pfile, traditional))
    {
      print.printed = 1;
      if (token->col > 2)
	{
	  unsigned int spaces = token->col - 2;

	  while (spaces--)
	    putc (' ', print.outf);
	}
    }
}

static void
cb_ident (cpp_reader *pfile ATTRIBUTE_UNUSED, fileline line,
	  const cpp_string *str)
{
  maybe_print_line (print.map, line);
  fprintf (print.outf, "#ident %s\n", str->text);
  print.line++;
}

static void
cb_define (cpp_reader *pfile, fileline line, cpp_hashnode *node)
{
  maybe_print_line (print.map, line);
  fputs ("#define ", print.outf);

  /* 'D' is whole definition; 'N' is name only.  */
  if (flag_dump_macros == 'D')
    fputs ((const char *) cpp_macro_definition (pfile, node),
	   print.outf);
  else
    fputs ((const char *) NODE_NAME (node), print.outf);

  putc ('\n', print.outf);
  print.line++;
}

static void
cb_undef (cpp_reader *pfile ATTRIBUTE_UNUSED, fileline line,
	  cpp_hashnode *node)
{
  maybe_print_line (print.map, line);
  fprintf (print.outf, "#undef %s\n", NODE_NAME (node));
  print.line++;
}

static void
cb_include (cpp_reader *pfile ATTRIBUTE_UNUSED, fileline line,
	    const unsigned char *dir, const char *header, int angle_brackets)
{
  maybe_print_line (print.map, line);
  if (angle_brackets)
    fprintf (print.outf, "#%s <%s>\n", dir, header);
  else
    fprintf (print.outf, "#%s \"%s\"\n", dir, header);
  print.line++;
}

/* Callback called when -fworking-director and -E to emit working
   diretory in cpp output file. */

void
pp_dir_change (cpp_reader *pfile ATTRIBUTE_UNUSED, const char *dir)
{
  size_t to_file_len = strlen (dir);
  unsigned char *to_file_quoted = alloca (to_file_len * 4 + 1);
  unsigned char *p;

  /* cpp_quote_string does not nul-terminate, so we have to do it ourselves. */
  p = cpp_quote_string (to_file_quoted, (unsigned char *) dir, to_file_len);
  *p = '\0';
  fprintf (print.outf, "# 1 \"%s//\"\n", to_file_quoted);
}

/* The file name, line number or system header flags have changed, as
   described in MAP.  From this point on, the old print.map might be
   pointing to freed memory, and so must not be dereferenced.  */

void
pp_file_change (const struct line_map *map)
{
  const char *flags = "";

  if (flag_no_line_commands || flag_no_output)
    return;

  if (map != NULL)
    {
      /* First time?  */
      if (print.map == NULL)
	{
	  /* Avoid printing foo.i when the main file is foo.c.  */
	  if (!cpp_get_options (parse_in)->preprocessed)
	    print_line (map, map->from_line, flags);
	}
      else
	{
	  /* Bring current file to correct line when entering a new file.  */
	  if (map->reason == LC_ENTER)
	    maybe_print_line (map - 1, map->from_line - 1);

	  if (map->reason == LC_ENTER)
	    flags = " 1";
	  else if (map->reason == LC_LEAVE)
	    flags = " 2";
	  print_line (map, map->from_line, flags);
	}
    }

  print.map = map;
}

/* Copy a #pragma directive to the preprocessed output.  */
static void
cb_def_pragma (cpp_reader *pfile, fileline line)
{
  maybe_print_line (print.map, line);
  fputs ("#pragma ", print.outf);
  cpp_output_line (pfile, print.outf);
  print.line++;
}

/* Dump out the hash table.  */
static int
dump_macro (cpp_reader *pfile, cpp_hashnode *node, void *v ATTRIBUTE_UNUSED)
{
  if (node->type == NT_MACRO && !(node->flags & NODE_BUILTIN))
    {
      fputs ("#define ", print.outf);
      fputs ((const char *) cpp_macro_definition (pfile, node),
	     print.outf);
      putc ('\n', print.outf);
      print.line++;
    }

  return 1;
}
