/* Main for jv-scan
   Copyright (C) 1998, 1999, 2000 Free Software Foundation, Inc.
   Contributed by Alexandre Petit-Bianco (apbianco@cygnus.com)

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"

#include "obstack.h"		/* We use obstacks in lex.c */

#include "version.c"

#include <getopt.h>

void fatal PARAMS ((const char *s, ...)) ATTRIBUTE_PRINTF_1 ATTRIBUTE_NORETURN;
void warning PARAMS ((const char *s, ...)) ATTRIBUTE_PRINTF_1;
void gcc_obstack_init PARAMS ((struct obstack *obstack));

static void usage PARAMS ((void)) ATTRIBUTE_NORETURN;
static void help PARAMS ((void)) ATTRIBUTE_NORETURN;
static void version PARAMS ((void)) ATTRIBUTE_NORETURN;

#define JC1_LITE
#include "jcf.h"
#include "parse.h"

/* Current input file and output file IO streams.  */
FILE *finput, *out;

/* Current input filename.  */
char *input_filename;

/* Executable name.  */
char *exec_name;

/* Flags matching command line options.  */
int flag_find_main = 0;
int flag_dump_class = 0;
int flag_list_filename = 0;



/* This is used to mark options with no short value.  */
#define LONG_OPT(Num)  ((Num) + 128)

#define OPT_HELP      LONG_OPT (0)
#define OPT_VERSION   LONG_OPT (1)

static struct option options[] =
{
  { "help",      no_argument,       NULL, OPT_HELP },
  { "version",   no_argument,       NULL, OPT_VERSION },
  { "print-main", no_argument,      &flag_find_main, 1 },
  { "list-filename", no_argument,   &flag_list_filename, 1 },
  { "list-class", no_argument,      &flag_dump_class, 1 },
  { NULL,        no_argument,       NULL, 0 }
};

static void
usage ()
{
  fprintf (stderr, "Try `jv-scan --help' for more information.\n");
  exit (1);
}

static void
help ()
{
  printf ("Usage: jv-scan [OPTION]... FILE...\n\n");
  printf ("Print useful information read from Java source files.\n\n");
  printf ("  --print-main            Print name of class containing `main'\n");
  printf ("  --list-class            List all classes defined in file\n");
  printf ("  --list-filename         Print input filename when listing class names\n");
  printf ("  -o FILE                 Set output file name\n");
  printf ("\n");
  printf ("  --help                  Print this help, then exit\n");
  printf ("  --version               Print version number, then exit\n");
  printf ("\n");
  printf ("For bug reporting instructions, please see:\n");
  printf ("<URL:http://www.gnu.org/software/gcc/faq.html#bugreport>.\n");
  exit (0);
}

static void
version ()
{
  printf ("jv-scan (%s)\n\n", version_string);
  printf ("Copyright (C) 1998, 1999 Free Software Foundation, Inc.\n");
  printf ("This is free software; see the source for copying conditions.  There is NO\n");
  printf ("warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n\n");
  exit (0);
}

/* jc1-lite main entry point */
int
DEFUN (main, (argc, argv),
       int argc AND char **argv)
{
  int i = 1;
  const char *output_file = NULL;
  long ft;
  int opt;

  exec_name = argv[0];

  /* Default for output */
  out = stdout;

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

	default:
	  usage ();
	  break;
	}
    }

  /* No flags? Do nothing */
  if (!flag_find_main && !flag_dump_class)
    return 0;

  /* Check on bad usage */
  if (flag_find_main && flag_dump_class)
    fatal ("Options `--print-main' and `--list-class' can't be turned on at the same time");

  if (output_file && !(out = fopen (output_file, "w")))
    fatal ("Can't open output file `%s'", output_file);

  ft = ftell (out);

  gcc_obstack_init (&temporary_obstack);
  java_push_parser_context ();

  for ( i = optind; i < argc; i++ )
    if (argv [i])
      {
	input_filename = argv [i];
	if ( (finput = fopen (argv [i], "r")) )
	  {
	    java_init_lex ();
	    yyparse ();
	    if (ftell (out) != ft)
	      fputc ('\n', out);
	    ft = ftell (out);
	    fclose (finput);
	    reset_report ();
	  }
	else
	  fatal ("File not found `%s'", argv [i]);
      }

  /* Flush and close */
  if (ftell (out) != ft)
    fputc ('\n', out);
  if (!output_file)
    fclose (out);

  return 0;
}



/* Error report, memory, obstack initialization and other utility
   functions */

void
fatal VPARAMS ((const char *s, ...))
{
#ifndef ANSI_PROTOTYPES
  const char *s;
#endif
  va_list ap;

  VA_START (ap, s);

#ifndef ANSI_PROTOTYPES
  s = va_arg (ap, const char *);
#endif

  fprintf (stderr, "%s: error: ", exec_name);
  vfprintf (stderr, s, ap);
  fputc ('\n', stderr);
  va_end (ap);
  exit (1);
}

void
warning VPARAMS ((const char *s, ...))
{
#ifndef ANSI_PROTOTYPES
  const char *s;
#endif
  va_list ap;

  VA_START (ap, s);

#ifndef ANSI_PROTOTYPES
  s = va_arg (ap, const char *);
#endif

  fprintf (stderr, "%s: warning: ", exec_name);
  vfprintf (stderr, s, ap);
  fputc ('\n', stderr);
  va_end (ap);
}

void
gcc_obstack_init (obstack)
     struct obstack *obstack;
{
  /* Let particular systems override the size of a chunk.  */
#ifndef OBSTACK_CHUNK_SIZE
#define OBSTACK_CHUNK_SIZE 0
#endif
  /* Let them override the alloc and free routines too.  */
#ifndef OBSTACK_CHUNK_ALLOC
#define OBSTACK_CHUNK_ALLOC xmalloc
#endif
#ifndef OBSTACK_CHUNK_FREE
#define OBSTACK_CHUNK_FREE free
#endif
  _obstack_begin (obstack, OBSTACK_CHUNK_SIZE, 0,
		  (void *(*) (long)) OBSTACK_CHUNK_ALLOC,
		  (void (*) (void *)) OBSTACK_CHUNK_FREE);
}
