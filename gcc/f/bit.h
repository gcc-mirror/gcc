/* bit.h -- Public #include File (module.h template V1.0)
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
      bit.c

   Modifications:
*/

/* Allow multiple inclusion to work. */

#ifndef GCC_F_BIT_H
#define GCC_F_BIT_H

/* Simple definitions and enumerations. */


/* Typedefs. */

typedef struct _ffebit_ *ffebit;
typedef unsigned long ffebitCount;
#define ffebitCount_f "l"

/* Include files needed by this one. */

#include "malloc.h"

/* Structure definitions. */

struct _ffebit_
  {
    mallocPool pool;
    ffebitCount size;
    unsigned char bits[1];
  };

/* Global objects accessed by users of this module. */


/* Declare functions with prototypes. */

void ffebit_count (ffebit b, ffebitCount offset, bool value, ffebitCount range,
		   ffebitCount *number);
void ffebit_kill (ffebit b);
ffebit ffebit_new (mallocPool pool, ffebitCount size);
void ffebit_set (ffebit b, ffebitCount offset, bool value, ffebitCount length);
void ffebit_test (ffebit b, ffebitCount offset, bool *value, ffebitCount *length);

/* Define macros. */

#define ffebit_init_0()
#define ffebit_init_1()
#define ffebit_init_2()
#define ffebit_init_3()
#define ffebit_init_4()
#define ffebit_pool(b) ((b)->pool)
#define ffebit_size(b) ((b)->size)
#define ffebit_terminate_0()
#define ffebit_terminate_1()
#define ffebit_terminate_2()
#define ffebit_terminate_3()
#define ffebit_terminate_4()

/* End of #include file. */

#endif /* ! GCC_F_BIT_H */
