/* CPP main program, using CPP Library.
   Copyright (C) 1995, 1997, 1998, 1999, 2000, 2001
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
  const char *syshdr_flags;	/* System header flags, if any.  */
  struct line_map *map;		/* Logical to physical line mappings.  */
  unsigned int line;		/* Line currently being written.  */
  unsigned char printed;	/* Nonzero if something output at line.  */
};

int main		PARAMS ((int, char **));
static void general_init PARAMS ((const char *));
static void do_preprocessing PARAMS ((int, char **));
static void setup_callbacks PARAMS ((void));

/* General output routines.  */
static void scan_translation_unit PARAMS ((cpp_reader *));
static void check_multiline_token PARAMS ((cpp_string *));
static int dump_macro PARAMS ((cpp_reader *, cpp_hashnode *, void *));

static void print_line PARAMS ((struct line_map *, unsigned int,
				const char *));
static void maybe_print_line PARAMS ((struct line_map *, unsigned int));

/* Callback routines for the parser.   Most of these are active only
   in specific modes.  */
static void cb_define	PARAMS ((cpp_reader *, unsigned int, cpp_hashnode *));
static void cb_undef	PARAMS ((cpp_reader *, unsigned int, cpp_hashnode *));
static void cb_include	PARAMS ((cpp_reader *, unsigned int,
				 const unsigned char *, const cpp_token *));
static void cb_ident	  PARAMS ((cpp_reader *, unsigned int,
				   const cpp_string *));
static void cb_file_change PARAMS ((cpp_reader *, const cpp_file_change *));
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

  /* Contruct a reader with default language GNU C89.  */
  pfile = cpp_create_reader (NULL, CLK_GNUC89);
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

/* LC_CTYPE determines the character set used by the terminal so it
   has to be set to output messages correctly.  */

#ifdef HAVE_LC_MESSAGES
  setlocale (LC_CTYPE, "");
  setlocale (LC_MESSAGES, "");
#else
  setlocale (LC_ALL, "");
#endif

  (void) bindtextdomain (PACKAGE, localedir);
  (void) textdomain (PACKAGE);
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
    cpp_fatal (pfile, "Invalid option %s", argv[argi]);
  else
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

  if (cpp_start_read (pfile, options->in_fname))
    {
      /* A successful cpp_start_read guarantees that we can call
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

/* Writes out the preprocessed file.  Alternates between two tokens,
   so that we can avoid accidental token pasting.  */
static void
scan_translation_unit (pfile)
     cpp_reader *pfile;
{
  unsigned int index, line;
  cpp_token tokens[2], *token;

  for (index = 0;; index = 1 - index)
    {
      token = &tokens[index];
      cpp_get_token (pfile, token);

      if (token->type == CPP_EOF)
	break;

      line = cpp_get_line (pfile)->output_line;
      if (print.line != line)
	{
	  unsigned int col = cpp_get_line (pfile)->col;

	  /* Supply enough whitespace to put this token in its original
	     column.  Don't bother trying to reconstruct tabs; we can't
	     get it right in general, and nothing ought to care.  (Yes,
	     some things do care; the fault lies with them.)  */
	  maybe_print_line (print.map, line);
	  if (col > 1)
	    {
	      if (token->flags & PREV_WHITE)
		col--;
	      while (--col)
		putc (' ', print.outf);
	    }
	}
      else if ((token->flags & (PREV_WHITE | AVOID_LPASTE))
	       == AVOID_LPASTE
	       && cpp_avoid_paste (pfile, &tokens[1 - index], token))
	token->flags |= PREV_WHITE;
      /* Special case '# <directive name>': insert a space between
	 the # and the token.  This will prevent it from being
	 treated as a directive when this code is re-preprocessed.
	 XXX Should do this only at the beginning of a line, but how?  */
      else if (token->type == CPP_NAME && token->val.node->directive_index
	       && tokens[1 - index].type == CPP_HASH)
	token->flags |= PREV_WHITE;

      cpp_output_token (token, print.outf);
      print.printed = 1;
      if (token->type == CPP_STRING || token->type == CPP_WSTRING
	  || token->type == CPP_COMMENT)
	check_multiline_token (&token->val.str);
    }
}

/* Adjust print.line for newlines embedded in tokens.  */
static void
check_multiline_token (str)
     cpp_string *str;
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
     struct line_map *map;
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

static void
print_line (map, line, special_flags)
     struct line_map *map;
     unsigned int line;
     const char *special_flags;
{
  /* End any previous line of text.  */
  if (print.printed)
    putc ('\n', print.outf);
  print.printed = 0;

  print.line = line;
  if (! options->no_line_commands)
    fprintf (print.outf, "# %u \"%s\"%s%s\n",
	     SOURCE_LINE (map, print.line), map->to_file,
	     special_flags, print.syshdr_flags);
}

/* Callbacks.  */

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
   described in FC.  NB: the old print.map must be considered invalid.  */

static void
cb_file_change (pfile, fc)
     cpp_reader *pfile ATTRIBUTE_UNUSED;
     const cpp_file_change *fc;
{
  bool first_time = print.map == NULL;

  /* Bring current file to correct line.  We handle the first file
     change callback specially, so that a first line of "# 1 "foo.c"
     in file foo.i outputs just the foo.c line, and not a foo.i line.  */
  if (fc->reason == LC_ENTER && !first_time)
    maybe_print_line (fc->map - 1, fc->line - 1);

  print.map = fc->map;
  if (fc->externc)
    print.syshdr_flags = " 3 4";
  else if (fc->sysp)
    print.syshdr_flags = " 3";
  else
    print.syshdr_flags = "";

  if (!first_time)
    {
      const char *flags = "";

      if (fc->reason == LC_ENTER)
	flags = " 1";
      else if (fc->reason == LC_LEAVE)
	flags = " 2";

      print_line (print.map, fc->line, flags);
    }
}

/* Copy a #pragma directive to the preprocessed output.  LINE is the
   line of the current source file, not the logical line.  */
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
