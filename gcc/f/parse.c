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
#include "flags.h"

extern FILE *finput;

void
ffe_parse_file (set_yydebug)
     int set_yydebug ATTRIBUTE_UNUSED;
{
  ffewhereFile wf;

  if (ffe_is_version ())
    fprintf (stderr, "GNU Fortran Front End version %s\n", version_string);

  if (!ffe_is_pedantic ())
    ffe_set_is_pedantic (pedantic);

  wf = ffewhere_file_new (main_input_filename, strlen (main_input_filename));
  ffecom_file (main_input_filename);
  ffe_file (wf, finput);

  ffecom_finish_compile ();
}
