/* CPP main program, using CPP Library.
   Copyright (C) 1995, 1997, 1998, 1999, 2000 Free Software Foundation, Inc.
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

const char *progname;

cpp_reader parse_in;


extern int main				PARAMS ((int, char **));
int
main (argc, argv)
     int argc;
     char **argv;
{
  char *p;
  cpp_reader *pfile = &parse_in;
  int argi = 1;  /* Next argument to handle.  */
  enum cpp_ttype kind;
  FILE *out;
  const char *out_fname;

  p = argv[0] + strlen (argv[0]);
  while (p != argv[0] && p[-1] != '/') --p;
  progname = p;

  xmalloc_set_program_name (progname);

#ifdef HAVE_LC_MESSAGES
  setlocale (LC_MESSAGES, "");
#endif
  (void) bindtextdomain (PACKAGE, localedir);
  (void) textdomain (PACKAGE);

  cpp_reader_init (pfile);
  
  argi += cpp_handle_options (pfile, argc - argi , argv + argi);
  if (argi < argc && ! CPP_FATAL_ERRORS (pfile))
    cpp_fatal (pfile, "Invalid option %s", argv[argi]);
  if (CPP_FATAL_ERRORS (pfile))
    return (FATAL_EXIT_CODE);

  if (! cpp_start_read (pfile, CPP_OPTION (pfile, in_fname)))
    return (FATAL_EXIT_CODE);

  /* Now that we know the input file is valid, open the output.  */
  out_fname = CPP_OPTION (pfile, out_fname);
  if (*out_fname == '\0')
    {
      out_fname = "stdout";
      out = stdout;
    }
  else
    {
      out = fopen (out_fname, "w");
      if (!out)
	{
	  cpp_notice_from_errno (pfile, CPP_OPTION (pfile, out_fname));
	  return (FATAL_EXIT_CODE);
	}
    }

  if (! CPP_OPTION (pfile, no_output))
    {
      do
	{
	  kind = cpp_get_token (pfile);
	  if (CPP_WRITTEN (pfile) >= BUFSIZ || kind == CPP_EOF)
	    {
	      size_t rem, count = CPP_WRITTEN (pfile);

	      rem = fwrite (parse_in.token_buffer, 1, count, out);
	      if (rem < count)
		/* Write error. */
		cpp_notice_from_errno (pfile, CPP_OPTION (pfile, out_fname));

	      CPP_SET_WRITTEN (pfile, 0);
	    }
	}
      while (kind != CPP_EOF);
    }
  else
    {
      do
	{
	  cpp_scan_buffer (pfile);
	  kind = cpp_get_token (pfile);
	}
      while (kind != CPP_EOF);
      CPP_SET_WRITTEN (pfile, 0);
    }

  cpp_finish (pfile);
  if (fwrite (parse_in.token_buffer, 1, CPP_WRITTEN (pfile), out)
      < CPP_WRITTEN (pfile))
    cpp_notice_from_errno (pfile, CPP_OPTION (pfile, out_fname));

  if (ferror (out) || fclose (out))
    cpp_notice_from_errno (pfile, CPP_OPTION (pfile, out_fname));

  cpp_cleanup (pfile);

  if (parse_in.errors)
    return (FATAL_EXIT_CODE);
  return (SUCCESS_EXIT_CODE);
}
