/* data.h -- Public #include File (module.h template V1.0)
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
02111-1307, USA.

   Owning Modules:
      data.c

   Modifications:
*/

/* Allow multiple inclusion to work. */

#ifndef _H_f_data
#define _H_f_data

/* Simple definitions and enumerations. */


/* Typedefs. */


/* Include files needed by this one. */

#include "bld.h"
#include "lex.h"
#include "storag.h"

/* Structure definitions. */


/* Global objects accessed by users of this module. */


/* Declare functions with prototypes. */

void ffedata_begin (ffebld list);
bool ffedata_end (bool report_errors, ffelexToken t);
void ffedata_gather (ffestorag st);
bool ffedata_value (ffetargetIntegerDefault rpt, ffebld value,
		    ffelexToken value_token);

/* Define macros. */

#define ffedata_init_0()
#define ffedata_init_1()
#define ffedata_init_2()
#define ffedata_init_3()
#define ffedata_init_4()
#define ffedata_terminate_0()
#define ffedata_terminate_1()
#define ffedata_terminate_2()
#define ffedata_terminate_3()
#define ffedata_terminate_4()

/* End of #include file. */

#endif
