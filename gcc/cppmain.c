/* CPP main program, using CPP Library.
   Copyright (C) 1995, 1997, 1998, 1999 Free Software Foundation, Inc.
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

#ifndef EMACS
#include "config.h"
#include "system.h"
#else
#include <stdio.h>

extern char *getenv ();
#endif /* not EMACS */

#include "cpplib.h"
#include "intl.h"

char *progname;

cpp_reader parse_in;
cpp_options options;


int
main (argc, argv)
     int argc;
     char **argv;
{
  char *p;
  int argi = 1;  /* Next argument to handle.  */
  struct cpp_options *opts = &options;
  enum cpp_token kind;

  p = argv[0] + strlen (argv[0]);
  while (p != argv[0] && p[-1] != '/') --p;
  progname = p;

#ifdef HAVE_LC_MESSAGES
  setlocale (LC_MESSAGES, "");
#endif
  (void) bindtextdomain (PACKAGE, localedir);
  (void) textdomain (PACKAGE);

  cpp_reader_init (&parse_in);
  parse_in.opts = opts;

  cpp_options_init (opts);
  
  argi += cpp_handle_options (&parse_in, argc - argi , argv + argi);
  if (argi < argc && ! CPP_FATAL_ERRORS (&parse_in))
    cpp_fatal (&parse_in, "Invalid option `%s'", argv[argi]);
  if (CPP_FATAL_ERRORS (&parse_in))
    exit (FATAL_EXIT_CODE);
      
  parse_in.show_column = 1;

  if (! cpp_start_read (&parse_in, opts->in_fname))
    exit (FATAL_EXIT_CODE);

  /* Now that we know the input file is valid, open the output.  */

  if (!opts->out_fname || !strcmp (opts->out_fname, ""))
    opts->out_fname = "stdout";
  else if (! freopen (opts->out_fname, "w", stdout))
    cpp_pfatal_with_name (&parse_in, opts->out_fname);

  do
    {
      kind = cpp_get_token (&parse_in);
      if (CPP_WRITTEN (&parse_in) >= BUFSIZ || kind == CPP_EOF)
	{
	  if (! opts->no_output)
	    {
	      size_t rem, count = CPP_WRITTEN (&parse_in);

	      rem = fwrite (parse_in.token_buffer, 1, count, stdout);
	      if (rem < count)
		/* Write error. */
		cpp_pfatal_with_name (&parse_in, opts->out_fname);
	    }

	  CPP_SET_WRITTEN (&parse_in, 0);
	}
    }
  while (kind != CPP_EOF);

  cpp_finish (&parse_in);
  if (fwrite (parse_in.token_buffer, 1, CPP_WRITTEN (&parse_in), stdout)
      < CPP_WRITTEN (&parse_in))
    cpp_pfatal_with_name (&parse_in, opts->out_fname);

  if (parse_in.errors)
    exit (FATAL_EXIT_CODE);
  exit (SUCCESS_EXIT_CODE);
}
