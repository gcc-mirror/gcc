/* name.h -- Public #include File (module.h template V1.0)
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
      name.c

   Modifications:
*/

/* Allow multiple inclusion to work. */

#ifndef GCC_F_NAME_H
#define GCC_F_NAME_H

/* Simple definitions and enumerations. */


/* Typedefs. */

typedef struct _ffename_ *ffename;
typedef struct _ffename_space_ *ffenameSpace;

/* Include files needed by this one. */

#include "global.h"
#include "lex.h"
#include "malloc.h"
#include "symbol.h"

/* Structure definitions. */

struct _ffename_
  {
    ffename next;
    ffename previous;
    ffelexToken t;
    union
      {
	ffesymbol s;
	ffeglobal g;
      }
    u;
  };

struct _ffename_space_
  {
    ffename first;
    ffename last;
    mallocPool pool;
  };

/* Global objects accessed by users of this module. */


/* Declare functions with prototypes. */

ffename ffename_find (ffenameSpace ns, ffelexToken t);
void ffename_kill (ffenameSpace ns, ffename n);
ffename ffename_lookup (ffenameSpace ns, ffelexToken t);
void ffename_space_drive_global (ffenameSpace ns, ffeglobal (*fn) (ffeglobal));
void ffename_space_drive_symbol (ffenameSpace ns, ffesymbol (*fn) (ffesymbol));
void ffename_space_kill (ffenameSpace ns);
ffenameSpace ffename_space_new (mallocPool pool);

/* Define macros. */

#define ffename_first_token(n) ((n)->t)
#define ffename_global(n) ((n)->u.g)
#define ffename_init_0()
#define ffename_init_1()
#define ffename_init_2()
#define ffename_init_3()
#define ffename_init_4()
#define ffename_set_global(n,glob) ((n)->u.g = (glob))
#define ffename_set_symbol(n,sym) ((n)->u.s = (sym))
#define ffename_symbol(n) ((n)->u.s)
#define ffename_terminate_0()
#define ffename_terminate_1()
#define ffename_terminate_2()
#define ffename_terminate_3()
#define ffename_terminate_4()
#define ffename_text(n) ffelex_token_text((n)->t)
#define ffename_token(n) ((n)->t)
#define ffename_where_filename(n) ffelex_token_where_filename((n)->t)
#define ffename_where_filelinenum(n) ffelex_token_where_filelinenum((n)->t)
#define ffename_where_line(n) ffelex_token_where_line((n)->t)
#define ffename_where_column(n) ffelex_token_where_column((n)->t)

/* End of #include file. */

#endif /* ! GCC_F_NAME_H */
