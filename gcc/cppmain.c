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
cpp_printer parse_out;


extern int main				PARAMS ((int, char **));
int
main (argc, argv)
     int argc;
     char **argv;
{
  char *p;
  cpp_reader *pfile = &parse_in;
  cpp_printer *print;
  int argi = 1;  /* Next argument to handle.  */

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

  /* Open the output now.  We must do so even if no_output is on,
     because there may be other output than from the actual
     preprocessing (e.g. from -dM).  */
  print = cpp_printer_init (pfile, &parse_out);
  if (! print)
    return (FATAL_EXIT_CODE);

  if (! cpp_start_read (pfile, print, CPP_OPTION (pfile, in_fname)))
    return (FATAL_EXIT_CODE);

  if (CPP_OPTION (pfile, no_output))
    while (CPP_BUFFER (pfile) != NULL)
      cpp_scan_buffer_nooutput (pfile);
  else
    while (CPP_BUFFER (pfile) != NULL)
      cpp_scan_buffer (pfile, print);

  cpp_finish (pfile, print);
  cpp_cleanup (pfile);

  if (parse_in.errors)
    return (FATAL_EXIT_CODE);
  return (SUCCESS_EXIT_CODE);
}
