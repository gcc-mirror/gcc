/* str.h -- Private #include File (module.h template V1.0)
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
      str.c

   Modifications:
*/

/* Allow multiple inclusion to work. */

#ifndef _H_f_str
#define _H_f_str

/* Simple definitions and enumerations. */

#define FFESTR_F90 0		/* Unsupported F90 stuff. */
#define FFESTR_VXT 0		/* Unsupported VXT stuff. */

/* Typedefs. */


/* Include files needed by this one. */

#include "lex.h"
#ifndef MAKING_DEPENDENCIES
#include "str-1t.h"
#include "str-fo.h"
#include "str-io.h"
#include "str-nq.h"
#include "str-ot.h"
#include "str-op.h"
#include "str-2t.h"
#endif

/* Structure definitions. */


/* Global objects accessed by users of this module. */


/* Declare functions with prototypes. */

ffestrFirst ffestr_first (ffelexToken t);
ffestrFormat ffestr_format (ffelexToken t);
ffestrGenio ffestr_genio (ffelexToken t);
ffestrInquire ffestr_inquire (ffelexToken t);
ffestrOpen ffestr_open (ffelexToken t);
ffestrOther ffestr_other (ffelexToken t);
ffestrSecond ffestr_second (ffelexToken t);

/* Define macros. */

#define ffestr_init_0()
#define ffestr_init_1()
#define ffestr_init_2()
#define ffestr_init_3()
#define ffestr_init_4()
#define ffestr_terminate_0()
#define ffestr_terminate_1()
#define ffestr_terminate_2()
#define ffestr_terminate_3()
#define ffestr_terminate_4()

/* End of #include file. */

#endif
