/* str.c -- Implementation File (module.c template V1.0)
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

   Related Modules:
      None

   Description:
      Handles recognition of keywords.

   Modifications:
*/

/* Include files. */

#include "proj.h"
#include "src.h"
#include "str.h"
#include "lex.h"

/* Externals defined here. */


/* Simple definitions and enumerations. */


/* Internal typedefs. */


/* Private include files. */


/* Internal structure definitions. */


/* Static objects accessed by functions in this module. */


/* Static functions (internal). */


/* Internal macros. */


/* ffestr_first -- Look up the first names in a statement

   ffestrFirst kw;
   ffelexToken t;
   kw = ffestr_first(t);

   Returns FFESTR_firstNone if no matches, else FFESTR_firstXYZ if the
   NAME or NAMES token matches XYZ.  t must be a NAME or NAMES token or this
   routine will crash.

   This routine's code is actually written by a utility called FINI, itself
   written specifically for the Gnu Fortran project.  FINI takes an input
   file, in this case "ffe_first.fini", consisting primarily of a
   list of statements (ASSIGN, IF, DO, DOWHILE), and outputs a C file,
   "str-1t.j", that contains the definition of the
   ffestr_first function.  We #include that file here.

   30-Jan-90  JCB  2.0
      Updated for Fortran 90.
*/

#ifndef MAKING_DEPENDENCIES
#include "str-1t.j"
#endif
/* ffestr_format -- Look up format names in a statement

   ffestrFormat kw;
   ffelexToken t;
   kw = ffestr_format(t);

   Returns FFESTR_formatNone if no matches, else FFESTR_formatXYZ if the
   NAME or NAMES token matches XYZ.  t must be a NAME or NAMES token or this
   routine will crash.

   This routine's code is actually written by a utility called FINI, itself
   written specifically for the Gnu Fortran project.  FINI takes an input
   file, in this case "ffe_format.fini", consisting primarily of a
   list of format keywords (I, F, TL, TR), and outputs a C file,
   "str-fo.j", that contains the definition of the
   ffestr_format function.  We #include that file here.

*/

#ifndef MAKING_DEPENDENCIES
#include "str-fo.j"
#endif
/* ffestr_genio -- Look up genio names in a statement

   ffestrGenio kw;
   ffelexToken t;
   kw = ffestr_genio(t);

   Returns FFESTR_genioNone if no matches, else FFESTR_genioXYZ if the
   NAME or NAMES token matches XYZ.  t must be a NAME or NAMES token or this
   routine will crash.

   This routine's code is actually written by a utility called FINI, itself
   written specifically for the Gnu Fortran project.  FINI takes an input
   file, in this case "ffe_genio.fini", consisting primarily of a
   list of statement keywords (TO, FUNCTION), and outputs a C file,
   "str-io.j", that contains the definition of the
   ffestr_genio function.  We #include that file here.

*/

#ifndef MAKING_DEPENDENCIES
#include "str-io.j"
#endif
/* ffestr_inquire -- Look up inquire names in a statement

   ffestrInquire kw;
   ffelexToken t;
   kw = ffestr_inquire(t);

   Returns FFESTR_inquireNone if no matches, else FFESTR_inquireXYZ if the
   NAME or NAMES token matches XYZ.  t must be a NAME or NAMES token or this
   routine will crash.

   This routine's code is actually written by a utility called FINI, itself
   written specifically for the Gnu Fortran project.  FINI takes an input
   file, in this case "ffe_inquire.fini", consisting primarily of a
   list of statement keywords (TO, FUNCTION), and outputs a C file,
   "str-nq.j", that contains the definition of the
   ffestr_inquire function.  We #include that file here.

*/

#ifndef MAKING_DEPENDENCIES
#include "str-nq.j"
#endif
/* ffestr_open -- Look up open names in a statement

   ffestrOpen kw;
   ffelexToken t;
   kw = ffestr_open(t);

   Returns FFESTR_openNone if no matches, else FFESTR_openXYZ if the
   NAME or NAMES token matches XYZ.  t must be a NAME or NAMES token or this
   routine will crash.

   This routine's code is actually written by a utility called FINI, itself
   written specifically for the Gnu Fortran project.  FINI takes an input
   file, in this case "ffe_open.fini", consisting primarily of a
   list of statement keywords (TO, FUNCTION), and outputs a C file,
   "str-op.j", that contains the definition of the
   ffestr_open function.  We #include that file here.

*/

#ifndef MAKING_DEPENDENCIES
#include "str-op.j"
#endif
/* ffestr_other -- Look up other names in a statement

   ffestrOther kw;
   ffelexToken t;
   kw = ffestr_other(t);

   Returns FFESTR_otherNone if no matches, else FFESTR_otherXYZ if the
   NAME or NAMES token matches XYZ.  t must be a NAME or NAMES token or this
   routine will crash.

   This routine's code is actually written by a utility called FINI, itself
   written specifically for the Gnu Fortran project.  FINI takes an input
   file, in this case "ffe_other.fini", consisting primarily of a
   list of statement keywords (TO, FUNCTION), and outputs a C file,
   "str-ot.j", that contains the definition of the
   ffestr_other function.  We #include that file here.

*/

#ifndef MAKING_DEPENDENCIES
#include "str-ot.j"
#endif
/* ffestr_second -- Look up the second name in a statement

   ffestrSecond kw;
   ffelexToken t;
   kw = ffestr_second(t);

   Returns FFESTR_secondNone if no matches, else FFESTR_secondXYZ if the
   NAME or NAMES token matches XYZ.  t must be a NAME or NAMES token or this
   routine will crash.

   This routine's code is actually written by a utility called FINI, itself
   written specifically for the Gnu Fortran project.  FINI takes an input
   file, in this case "ffe_second.fini", consisting primarily of a
   list of statement keywords (TO, FUNCTION), and outputs a C file,
   "str-2t.j", that contains the definition of the
   ffestr_second function.  We #include that file here.

*/

#ifndef MAKING_DEPENDENCIES
#include "str-2t.j"
#endif
