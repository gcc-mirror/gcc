/* Main for jv-scan
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.
   Contributed by Alexandre Petit-Bianco (apbianco@cygnus.com)

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "intl.h"

#include "obstack.h"		/* We use obstacks in lex.c */

#include "version.h"

#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif

#ifdef HAVE_LANGINFO_CODESET
#include <langinfo.h>
#endif

#include <getopt.h>

extern void fatal_error (const char *gmsgid, ...)
     ATTRIBUTE_PRINTF_1 ATTRIBUTE_NORETURN;
void warning (int opt, const char *gmsgid, ...) ATTRIBUTE_PRINTF_2;
void warning0 (const char *gmsgid, ...) ATTRIBUTE_PRINTF_1;
void report (void);

static void usage (void) ATTRIBUTE_NORETURN;
static void help (void) ATTRIBUTE_NORETURN;
static void version (void) ATTRIBUTE_NORETURN;

#define JC1_LITE
#include "jcf.h"
#include "parse.h"

/* Current input file and output file IO streams.  */
FILE *finput, *out;

/* Executable name.  */
char *exec_name;

struct line_maps line_table;

/* Flags matching command line options.  */
int flag_find_main = 0;
int flag_dump_class = 0;
int flag_list_filename = 0;
int flag_complexity = 0;
int flag_assert = 1;

int pedantic = 0;



/* This is used to mark options with no short value.  */
#define LONG_OPT(Num)  ((Num) + 128)

#define OPT_HELP      LONG_OPT (0)
#define OPT_VERSION   LONG_OPT (1)
#define OPT_ENCODING  LONG_OPT (2)

static const struct option options[] =
{
  { "help",      no_argument,       NULL, OPT_HELP },
  { "version",   no_argument,       NULL, OPT_VERSION },
  { "print-main", no_argument,      &flag_find_main, 1 },
  { "list-filename", no_argument,   &flag_list_filename, 1 },
  { "list-class", no_argument,      &flag_dump_class, 1 },
  { "encoding",  required_argument, NULL, OPT_ENCODING },
  { "complexity", no_argument,	    &flag_complexity, 1 },
  { "no-assert", no_argument,       &flag_assert, 0 },
  { "assert",    no_argument,       &flag_assert, 1 },
  { NULL,        no_argument,       NULL, 0 }
};

static void
usage (void)
{
  fprintf (stderr, _("Try 'jv-scan --help' for more information.\n"));
  exit (1);
}

static void
help (void)
{
  printf (_("Usage: jv-scan [OPTION]... FILE...\n\n"));
  printf (_("Print useful information read from Java source files.\n\n"));
  printf (_("  --no-assert             Don't recognize the assert keyword\n"));
  printf (_("  --complexity            Print cyclomatic complexity of input file\n"));
  printf (_("  --encoding NAME         Specify encoding of input file\n"));
  printf (_("  --print-main            Print name of class containing 'main'\n"));
  printf (_("  --list-class            List all classes defined in file\n"));
  printf (_("  --list-filename         Print input filename when listing class names\n"));
  printf (_("  -o FILE                 Set output file name\n"));
  printf ("\n");
  printf (_("  --help                  Print this help, then exit\n"));
  printf (_("  --version               Print version number, then exit\n"));
  printf ("\n");
  printf (_("For bug reporting instructions, please see:\n"
	    "%s.\n"), bug_report_url);
  exit (0);
}

static void
version (void)
{
  printf ("jv-scan (GCC) %s\n\n", version_string);
  printf ("Copyright %s 2006 Free Software Foundation, Inc.\n", _("(C)"));
  printf (_("This is free software; see the source for copying conditions.  There is NO\n"
	    "warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n\n"));
  exit (0);
}

/* jc1-lite main entry point */
int
main (int argc, char **argv)
{
  int i = 1;
  const char *output_file = NULL;
  const char *encoding = NULL;
  long ft;
  int opt;

  exec_name = argv[0];

  /* Default for output */
  out = stdout;

  /* Unlock the stdio streams.  */
  unlock_std_streams ();

  gcc_init_libintl ();

  /* Process options first.  We use getopt_long and not
     getopt_long_only because we only support `--' long options here.  */
  while ((opt = getopt_long (argc, argv, "o:", options, NULL)) != -1)
    {
      switch (opt)
	{
	case 0:
	  /* Already handled.  */
	  break;

	case 'o':
	  output_file = optarg;
	  break;

	case OPT_HELP:
	  help ();
	  break;

	case OPT_VERSION:
	  version ();
	  break;

	case OPT_ENCODING:
	  encoding = optarg;
	  break;

	default:
	  usage ();
	  break;
	}
    }

  /* No flags? Do nothing */
  if (! flag_find_main && ! flag_dump_class && ! flag_complexity)
    return 0;

  /* Check on bad usage */
  if (flag_find_main + flag_dump_class + flag_complexity > 1)
    fatal_error
      ("only one of '--print-main', '--list-class', and '--complexity' allowed");

  if (output_file && !(out = fopen (output_file, "w")))
    fatal_error ("can't open output file '%s'", output_file);

  ft = ftell (out);

  gcc_obstack_init (&temporary_obstack);
  java_push_parser_context ();

  for ( i = optind; i < argc; i++ )
    if (argv [i])
      {
	char *filename = argv[i];
	if ( (finput = fopen (filename, "r")) )
	  {
	    /* There's no point in trying to find the current encoding
	       unless we are going to do something intelligent with it
	       -- hence the test for iconv.  */
#if defined (HAVE_LOCALE_H) && defined (HAVE_ICONV) && defined (HAVE_LANGINFO_CODESET)
	    setlocale (LC_CTYPE, "");
	    if (encoding == NULL)
	      encoding = nl_langinfo (CODESET);
#endif  
	    if (encoding == NULL || *encoding == '\0')
	      encoding = DEFAULT_ENCODING;

            main_input_filename = filename;
	    java_init_lex (finput, encoding);
	    ctxp->filename = filename;
	    yyparse ();
	    report ();
	    if (ftell (out) != ft)
	      fputc ('\n', out);
	    ft = ftell (out);
	    fclose (finput);
	    reset_report ();
	  }
	else
	  fatal_error ("file not found '%s'", argv [i]);
      }

  /* Flush and close */
  if (ftell (out) != ft)
    fputc ('\n', out);
  if (!output_file)
    fclose (out);

  return 0;
}



/* Error report, memory, obstack initialization and other utility
   functions.  Use actually c-format msgid, but as functions with
   the same name elsewhere use gcc-internal-format, assume all users
   here use intersection between c-format and gcc-internal-format.  */

void
fatal_error (const char *gmsgid, ...)
{
  va_list ap;
  va_start (ap, gmsgid);
  fprintf (stderr, _("%s: error: "), exec_name);
  vfprintf (stderr, _(gmsgid), ap);
  fputc ('\n', stderr);
  va_end (ap);
  exit (1);
}

void
warning (int opt ATTRIBUTE_UNUSED, const char *gmsgid, ...)
{
  va_list ap;
  va_start (ap, gmsgid);
  fprintf (stderr, _("%s: warning: "), exec_name);
  vfprintf (stderr, _(gmsgid), ap);
  fputc ('\n', stderr);
  va_end (ap);
}

void
warning0 (const char *gmsgid, ...)
{
  va_list ap;
  va_start (ap, gmsgid);
  fprintf (stderr, _("%s: warning: "), exec_name);
  vfprintf (stderr, _(gmsgid), ap);
  fputc ('\n', stderr);
  va_end (ap);
}

void
fancy_abort (const char *file, int line, const char *func)
{
  fatal_error ("abort in %s, at %s:%d", func, file, line);
}
