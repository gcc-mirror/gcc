/* equiv.h -- Public #include File (module.h template V1.0)
   Copyright (C) 1995, 1996 Free Software Foundation, Inc.
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
      equiv.c

   Modifications:
*/

/* Allow multiple inclusion to work. */

#ifndef GCC_F_EQUIV_H
#define GCC_F_EQUIV_H

/* Simple definitions and enumerations. */


/* Typedefs. */

typedef struct _ffeequiv_ *ffeequiv;

/* Include files needed by this one. */

#include "bld.h"
#include "lex.h"
#include "storag.h"
#include "symbol.h"

/* Structure definitions. */

struct _ffeequiv_
  {
    ffeequiv next;
    ffeequiv previous;
    ffesymbol common;		/* Common area for this equiv, if any. */
    ffebld list;		/* List of lists of equiv exprs. */
    bool is_save;		/* Any SAVEd members? */
    bool is_init;		/* Any initialized members? */
  };

/* Global objects accessed by users of this module. */


/* Declare functions with prototypes. */

void ffeequiv_add (ffeequiv eq, ffebld list, ffelexToken t);
void ffeequiv_exec_transition (void);
void ffeequiv_init_2 (void);
void ffeequiv_kill (ffeequiv victim);
bool ffeequiv_layout_cblock (ffestorag st);
ffeequiv ffeequiv_merge (ffeequiv eq1, ffeequiv eq2, ffelexToken t);
ffeequiv ffeequiv_new (void);
ffesymbol ffeequiv_symbol (ffebld expr);
void ffeequiv_update_init (ffeequiv eq);
void ffeequiv_update_save (ffeequiv eq);

/* Define macros. */

#define ffeequiv_common(e) ((e)->common)
#define ffeequiv_init_0()
#define ffeequiv_init_1()
#define ffeequiv_init_3()
#define ffeequiv_init_4()
#define ffeequiv_is_init(e) ((e)->is_init)
#define ffeequiv_is_save(e) ((e)->is_save)
#define ffeequiv_list(e) ((e)->list)
#define ffeequiv_next(e) ((e)->next)
#define ffeequiv_previous(e) ((e)->previous)
#define ffeequiv_set_common(e,c) ((e)->common = (c))
#define ffeequiv_set_init(e,i) ((e)->init = (i))
#define ffeequiv_set_is_init(e,in) ((e)->is_init = (in))
#define ffeequiv_set_is_save(e,sa) ((e)->is_save = (sa))
#define ffeequiv_set_list(e,l) ((e)->list = (l))
#define ffeequiv_terminate_0()
#define ffeequiv_terminate_1()
#define ffeequiv_terminate_2()
#define ffeequiv_terminate_3()
#define ffeequiv_terminate_4()

/* End of #include file. */

#endif /* ! GCC_F_EQUIV_H */
