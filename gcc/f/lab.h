/* lab.h -- Public #include File (module.h template V1.0)
   Copyright (C) 1995, 2003 Free Software Foundation, Inc.
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
      lab.c

   Modifications:
      22-Aug-89	 JCB  1.1
	 Change for new ffewhere interface.
*/

/* Allow multiple inclusion to work. */

#ifndef GCC_F_LAB_H
#define GCC_F_LAB_H

/* Simple definitions and enumerations. */

typedef enum
  {
    FFELAB_typeUNKNOWN,		/* No info yet on label. */
    FFELAB_typeANY,		/* Label valid for anything, no msgs. */
    FFELAB_typeUSELESS,		/* No valid way to reference this label. */
    FFELAB_typeASSIGNABLE,	/* Target of ASSIGN: so FORMAT or BRANCH. */
    FFELAB_typeFORMAT,		/* FORMAT label. */
    FFELAB_typeLOOPEND,		/* Target of a labeled DO statement. */
    FFELAB_typeNOTLOOP,		/* Branch target statement not valid DO
				   target. */
    FFELAB_typeENDIF,		/* END IF label. */
    FFELAB_type
  } ffelabType;

#define FFELAB_valueNONE 0
#define FFELAB_valueMAX 99999

/* Typedefs. */

typedef struct _ffelab_ *ffelab;
typedef ffelab ffelabHandle;
typedef unsigned long ffelabNumber;	/* Count of new labels. */
#define ffelabNumber_f "l"
typedef unsigned long ffelabValue;
#define ffelabValue_f "l"

/* Include files needed by this one. */

#include "com.h"
#include "where.h"

/* Structure definitions. */

struct _ffelab_
  {
    ffelab next;
    ffecomLabel hook;
    ffelabValue value;		/* 1 through 99999, or 100000+ for temp
				   labels. */
    unsigned long blocknum;	/* Managed entirely by user of module. */
    ffewhereLine firstref_line;
    ffewhereColumn firstref_col;
    ffewhereLine doref_line;
    ffewhereColumn doref_col;
    ffewhereLine definition_line;	/* ffewhere_line_unknown() if not
					   defined. */
    ffewhereColumn definition_col;
    ffelabType type;
  };

/* Global objects accessed by users of this module. */

extern ffelab ffelab_list_;
extern ffelabNumber ffelab_num_news_;

/* Declare functions with prototypes. */

ffelab ffelab_find (ffelabValue v);
void ffelab_finish (void);
void ffelab_init_3 (void);
ffelab ffelab_new (ffelabValue v);

/* Define macros. */

#define ffelab_blocknum(l) ((l)->blocknum)
#define ffelab_definition_column(l) ((l)->definition_col)
#define ffelab_definition_filename(l) \
      ffewhere_line_filename((l)->definition_line)
#define ffelab_definition_filelinenum(l) \
      ffewhere_line_filelinenum((l)->definition_line)
#define ffelab_definition_line(l) ((l)->definition_line)
#define ffelab_definition_line_number(l) \
      ffewhere_line_number((l)->definition_line)
#define ffelab_doref_column(l) ((l)->doref_col)
#define ffelab_doref_filename(l) ffewhere_line_filename((l)->doref_line)
#define ffelab_doref_filelinenum(l) ffewhere_line_filelinenum((l)->doref_line)
#define ffelab_doref_line(l) ((l)->doref_line)
#define ffelab_doref_line_number(l) ffewhere_line_number((l)->doref_line)
#define ffelab_firstref_column(l) ((l)->firstref_col)
#define ffelab_firstref_filename(l) ffewhere_line_filename((l)->firstref_line)
#define ffelab_firstref_filelinenum(l) \
      ffewhere_line_filelinenum((l)->firstref_line)
#define ffelab_firstref_line(l) ((l)->firstref_line)
#define ffelab_firstref_line_number(l) ffewhere_line_number((l)->firstref_line)
#define ffelab_handle_done(h)
#define ffelab_handle_first() ((ffelabHandle) ffelab_list_)
#define ffelab_handle_next(h) ((ffelabHandle) (((ffelab) h)->next))
#define ffelab_handle_target(h) ((ffelab) h)
#define ffelab_hook(l) ((l)->hook)
#define ffelab_init_0()
#define ffelab_init_1()
#define ffelab_init_2()
#define ffelab_init_4()
#define ffelab_kill(l) ffelab_set_value(l,FFELAB_valueNONE);
#define ffelab_new_generated() (ffelab_new(ffelab_generated_++))
#define ffelab_number() (ffelab_num_news_)
#define ffelab_set_blocknum(l,b) ((l)->blocknum = (b))
#define ffelab_set_definition_column(l,cn) ((l)->definition_col = (cn))
#define ffelab_set_definition_line(l,ln) ((l)->definition_line = (ln))
#define ffelab_set_doref_column(l,cn) ((l)->doref_col = (cn))
#define ffelab_set_doref_line(l,ln) ((l)->doref_line = (ln))
#define ffelab_set_firstref_column(l,cn) ((l)->firstref_col = (cn))
#define ffelab_set_firstref_line(l,ln) ((l)->firstref_line = (ln))
#define ffelab_set_hook(l,h) ((l)->hook = (h))
#define ffelab_set_type(l,t) ((l)->type = (t))
#define ffelab_terminate_0()
#define ffelab_terminate_1()
#define ffelab_terminate_2()
#define ffelab_terminate_3()
#define ffelab_terminate_4()
#define ffelab_type(l) ((l)->type)
#define ffelab_value(l) ((l)->value)

/* End of #include file. */

#endif /* ! GCC_F_LAB_H */
