/* GNU Fortran
   Copyright (C) 1995 Free Software Foundation, Inc.
   Contributed by James Craig Burley.

This file is part of GNU Fortran.

GNU Fortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Fortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Fortran; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.	 */

#include "proj.h"
#include "top.h"
#include "com.h"
#include "where.h"
#include "version.h"
#if FFECOM_targetCURRENT == FFECOM_targetGCC
#include "flags.j"
#endif

#define NAME_OF_STDIN "<stdin>"

#if FFECOM_targetCURRENT == FFECOM_targetFFE
void
main (int argc, char *argv[])
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
FILE *finput;

int
yyparse ()
#else
#error
#endif
{
  ffewhereFile wf;

  if (ffe_is_version ())
    fprintf (stderr, "GNU Fortran Front End version %s\n", ffe_version_string);

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  ffe_init_0 ();

  {
    int strings_processed;
    for (--argc, ++argv; argc > 0; argc -= strings_processed, argv += strings_processed)
      {
	strings_processed = ffe_decode_option (argc, argv);
	if (strings_processed == 0)
	  {
	    fprintf (stderr, "Unrecognized option: %s\n", argv[0]);
	    strings_processed = 1;
	  }
      }
  }
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  if (!ffe_is_pedantic ())
    ffe_set_is_pedantic (pedantic);
#else
#error
#endif

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  wf = ffewhere_file_new (NAME_OF_STDIN, strlen (NAME_OF_STDIN));
  ffecom_file (NAME_OF_STDIN);
  ffe_file (wf, stdin);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  wf = ffewhere_file_new (main_input_filename, strlen (main_input_filename));
  ffecom_file (main_input_filename);
  ffe_file (wf, finput);
#else
#error
#endif

#if FFECOM_targetCURRENT == FFECOM_targetGCC
  ffecom_finish_compile ();

  return 0;
#elif FFECOM_targetCURRENT == FFECOM_targetFFE
  ffe_terminate_0 ();

  exit (0);
#else
#error
#endif
}
