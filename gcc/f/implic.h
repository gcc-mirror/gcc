/* implic.h -- Public #include File (module.h template V1.0)
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
      implic.c

   Modifications:
*/

/* Allow multiple inclusion to work. */

#ifndef _H_f_implic
#define _H_f_implic

/* Simple definitions and enumerations. */


/* Typedefs. */


/* Include files needed by this one. */

#include "info.h"
#include "symbol.h"
#include "target.h"

/* Structure definitions. */


/* Global objects accessed by users of this module. */


/* Declare functions with prototypes. */

bool ffeimplic_establish_initial (char c, ffeinfoBasictype basic_type,
		    ffeinfoKindtype kind_type, ffetargetCharacterSize size);
bool ffeimplic_establish_symbol (ffesymbol s);
void ffeimplic_init_2 (void);
void ffeimplic_none (void);
ffeinfoBasictype ffeimplic_peek_symbol_type (ffesymbol s, const char *name);
void ffeimplic_terminate_2 (void);

/* Define macros. */

#define ffeimplic_init_0()
#define ffeimplic_init_1()
#define ffeimplic_init_3()
#define ffeimplic_init_4()
#define ffeimplic_terminate_0()
#define ffeimplic_terminate_1()
#define ffeimplic_terminate_3()
#define ffeimplic_terminate_4()

/* End of #include file. */

#endif
