/* CPP main program, using CPP Library.
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
#include "intl.h"

/* Encapsulates state used to convert the stream of tokens coming from
   cpp_get_token back into a text file.  */
struct printer
{
  FILE *outf;			/* Stream to write to.  */
  const struct line_map *map;	/* Logical to physical line mappings.  */
  const cpp_token *prev;	/* Previous token.  */
  const cpp_token *source;	/* Source token for spacing.  */
  unsigned int line;		/* Line currently being written.  */
  unsigned char printed;	/* Nonzero if something output at line.  */
};

int main		PARAMS ((int, char **));
static void general_init PARAMS ((const char *));
static void do_preprocessing PARAMS ((int, char **));
static void setup_callbacks PARAMS ((void));

/* General output routines.  */
static void scan_translation_unit PARAMS ((cpp_reader *));
static void check_multiline_token PARAMS ((const cpp_string *));
static int dump_macro PARAMS ((cpp_reader *, cpp_hashnode *, void *));

static void print_line PARAMS ((const struct line_map *, unsigned int,
				const char *));
static void maybe_print_line PARAMS ((const struct line_map *, unsigned int));

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

const char *progname;		/* Needs to be global.  */
static cpp_reader *pfile;	/* An opaque handle.  */
static cpp_options *options;	/* Options of pfile.  */
static struct printer print;

int
main (argc, argv)
     int argc;
     char **argv;
{
  general_init (argv[0]);

  /* Construct a reader with default language GNU C89.  */
  pfile = cpp_create_reader (CLK_GNUC89);
  options = cpp_get_options (pfile);
  
  do_preprocessing (argc, argv);

  if (cpp_destroy (pfile))
    return FATAL_EXIT_CODE;

  return SUCCESS_EXIT_CODE;
}

/* Store the program name, and set the locale.  */
static void
general_init (argv0)
     const char *argv0;
{
  progname = argv0 + strlen (argv0);

  while (progname != argv0 && ! IS_DIR_SEPARATOR (progname[-1]))
    --progname;

  xmalloc_set_program_name (progname);

  hex_init ();
  gcc_init_libintl ();
}

/* Handle switches, preprocess and output.  */
static void
do_preprocessing (argc, argv)
     int argc;
     char **argv;
{
  int argi = 1;  /* Next argument to handle.  */

  argi += cpp_handle_options (pfile, argc - argi , argv + argi);
  if (CPP_FATAL_ERRORS (pfile))
    return;

  if (argi < argc)
    {
      cpp_fatal (pfile, "invalid option %s", argv[argi]);
      return;
    }

  cpp_post_options (pfile);
  if (CPP_FATAL_ERRORS (pfile))
    return;

  /* If cpp_handle_options saw --help or --version on the command
     line, it will have set pfile->help_only to indicate this.  Exit
     successfully.  [The library does not exit itself, because
     e.g. cc1 needs to print its own --help message at this point.]  */
  if (options->help_only)
    return;

  /* Initialize the printer structure.  Setting print.line to -1 here
     is a trick to guarantee that the first token of the file will
     cause a linemarker to be output by maybe_print_line.  */
  print.line = (unsigned int) -1;
  print.printed = 0;
  print.prev = 0;
  print.map = 0;
  
  /* Open the output now.  We must do so even if no_output is on,
     because there may be other output than from the actual
     preprocessing (e.g. from -dM).  */
  if (options->out_fname[0] == '\0')
    print.outf = stdout;
  else
    {
      print.outf = fopen (options->out_fname, "w");
      if (print.outf == NULL)
	{
	  cpp_notice_from_errno (pfile, options->out_fname);
	  return;
	}
    }

  setup_callbacks ();

  if (cpp_read_main_file (pfile, options->in_fname, NULL))
    {
      cpp_finish_options (pfile);

      /* A successful cpp_read_main_file guarantees that we can call
	 cpp_scan_nooutput or cpp_get_token next.  */
      if (options->no_output)
	cpp_scan_nooutput (pfile);
      else
	scan_translation_unit (pfile);

      /* -dM command line option.  Should this be in cpp_finish?  */
      if (options->dump_macros == dump_only)
	cpp_forall_identifiers (pfile, dump_macro, NULL);

      cpp_finish (pfile);
    }

  /* Flush any pending output.  */
  if (print.printed)
    putc ('\n', print.outf);

  if (ferror (print.outf) || fclose (print.outf))
    cpp_notice_from_errno (pfile, options->out_fname);
}

/* Set up the callbacks as appropriate.  */
static void
setup_callbacks ()
{
  cpp_callbacks *cb = cpp_get_callbacks (pfile);

  if (! options->no_output)
    {
      cb->line_change = cb_line_change;
      cb->ident      = cb_ident;
      cb->def_pragma = cb_def_pragma;
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
	      || (print.prev && cpp_avoid_paste (pfile, print.prev, token))
	      || (print.prev == NULL && token->type == CPP_HASH))
	    putc (' ', print.outf);
	}
      else if (token->flags & PREV_WHITE)
	putc (' ', print.outf);

      avoid_paste = false;
      print.source = NULL;
      print.prev = token;
      cpp_output_token (token, print.outf);

      if (token->type == CPP_STRING || token->type == CPP_WSTRING
	  || token->type == CPP_COMMENT)
	check_multiline_token (&token->val.str);
    }
}

/* Adjust print.line for newlines embedded in tokens.  */
static void
check_multiline_token (str)
     const cpp_string *str;
{
  unsigned int i;

  for (i = 0; i < str->len; i++)
    if (str->text[i] == '\n')
      print.line++;
}

/* If the token read on logical line LINE needs to be output on a
   different line to the current one, output the required newlines or
   a line marker, and return 1.  Otherwise return 0.  */
static void
maybe_print_line (map, line)
     const struct line_map *map;
     unsigned int line;
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
print_line (map, line, special_flags)
     const struct line_map *map;
     unsigned int line;
     const char *special_flags;
{
  /* End any previous line of text.  */
  if (print.printed)
    putc ('\n', print.outf);
  print.printed = 0;

  print.line = line;
  if (! options->no_line_commands)
    {
      fprintf (print.outf, "# %u \"%s\"%s",
	       SOURCE_LINE (map, print.line), map->to_file, special_flags);

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
cb_line_change (pfile, token, parsing_args)
     cpp_reader *pfile ATTRIBUTE_UNUSED;
     const cpp_token *token;
     int parsing_args;
{
  if (token->type == CPP_EOF || parsing_args)
    return;

  maybe_print_line (print.map, token->line);
  print.printed = 1;
  print.prev = 0;
  print.source = 0;

  /* Supply enough spaces to put this token in its original column,
     one space per column greater than 2, since scan_translation_unit
     will provide a space if PREV_WHITE.  Don't bother trying to
     reconstruct tabs; we can't get it right in general, and nothing
     ought to care.  Some things do care; the fault lies with them.  */
  if (token->col > 2)
    {
      unsigned int spaces = token->col - 2;

      while (spaces--)
	putc (' ', print.outf);
    }
}

static void
cb_ident (pfile, line, str)
     cpp_reader *pfile ATTRIBUTE_UNUSED;
     unsigned int line;
     const cpp_string * str;
{
  maybe_print_line (print.map, line);
  fprintf (print.outf, "#ident \"%s\"\n", str->text);
  print.line++;
}

static void
cb_define (pfile, line, node)
     cpp_reader *pfile;
     unsigned int line;
     cpp_hashnode *node;
{
  maybe_print_line (print.map, line);
  fputs ("#define ", print.outf);

  /* -dD command line option.  */
  if (options->dump_macros == dump_definitions)
    fputs ((const char *) cpp_macro_definition (pfile, node), print.outf);
  else
    fputs ((const char *) NODE_NAME (node), print.outf);

  putc ('\n', print.outf);
  print.line++;
}

static void
cb_undef (pfile, line, node)
     cpp_reader *pfile ATTRIBUTE_UNUSED;
     unsigned int line;
     cpp_hashnode *node;
{
  maybe_print_line (print.map, line);
  fprintf (print.outf, "#undef %s\n", NODE_NAME (node));
  print.line++;
}

static void
cb_include (pfile, line, dir, header)
     cpp_reader *pfile;
     unsigned int line;
     const unsigned char *dir;
     const cpp_token *header;
{
  maybe_print_line (print.map, line);
  fprintf (print.outf, "#%s %s\n", dir, cpp_token_as_text (pfile, header));
  print.line++;
}

/* The file name, line number or system header flags have changed, as
   described in MAP.  From this point on, the old print.map might be
   pointing to freed memory, and so must not be dereferenced.  */

static void
cb_file_change (pfile, map)
     cpp_reader *pfile ATTRIBUTE_UNUSED;
     const struct line_map *map;
{
  const char *flags = "";

  /* First time?  */
  if (print.map == NULL)
    {
      /* Avoid printing foo.i when the main file is foo.c.  */
      if (!options->preprocessed)
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

  print.map = map;
}

/* Copy a #pragma directive to the preprocessed output.  */
static void
cb_def_pragma (pfile, line)
     cpp_reader *pfile;
     unsigned int line;
{
  maybe_print_line (print.map, line);
  fputs ("#pragma ", print.outf);
  cpp_output_line (pfile, print.outf);
  print.line++;
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
      fputs ("#define ", print.outf);
      fputs ((const char *) cpp_macro_definition (pfile, node), print.outf);
      putc ('\n', print.outf);
      print.line++;
    }

  return 1;
}
