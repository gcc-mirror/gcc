/* st.h -- Public #include File (module.h template V1.0)
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
      st.c

   Modifications:
*/

/* Allow multiple inclusion to work. */

#ifndef GCC_F_ST_H
#define GCC_F_ST_H

/* Simple definitions and enumerations. */


/* Typedefs. */


/* Include files needed by this one. */

#include "bad.h"
#include "lex.h"
#include "symbol.h"

/* Structure definitions. */


/* Global objects accessed by users of this module. */


/* Declare functions with prototypes. */

void ffest_confirmed (void);
void ffest_eof (void);
bool ffest_ffebad_start (ffebad errnum);
void ffest_ffebad_here_current_stmt (ffebadIndex i);
void ffest_ffebad_here_doiter (ffebadIndex i, ffesymbol s);
ffelexHandler ffest_first (ffelexToken t);
void ffest_init_0 (void);
void ffest_init_1 (void);
void ffest_init_2 (void);
void ffest_init_3 (void);
void ffest_init_4 (void);
bool ffest_is_entry_valid (void);
bool ffest_is_inhibited (void);
bool ffest_seen_first_exec (void);
void ffest_shutdown (void);
ffesymbol ffest_sym_end_transition (ffesymbol s);
ffesymbol ffest_sym_exec_transition (ffesymbol s);
void ffest_terminate_0 (void);
void ffest_terminate_1 (void);
void ffest_terminate_2 (void);
void ffest_terminate_3 (void);
void ffest_terminate_4 (void);

/* Define macros. */


/* End of #include file. */

#endif /* ! GCC_F_ST_H */
