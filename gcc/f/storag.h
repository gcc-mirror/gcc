/* storag.h -- Public #include File (module.h template V1.0)
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
      storag.c

   Modifications:
*/

/* Allow multiple inclusion to work. */

#ifndef GCC_F_STORAG_H
#define GCC_F_STORAG_H

/* Simple definitions and enumerations. */

typedef enum
  {
    FFESTORAG_typeNONE,
    FFESTORAG_typeCBLOCK,	/* A COMMON block. */
    FFESTORAG_typeCOMMON,	/* A COMMON variable. */
    FFESTORAG_typeLOCAL,	/* A local entity (var/array/equivalence). */
    FFESTORAG_typeEQUIV,	/* An entity equivalenced into a COMMON/LOCAL
				   entity. */
    FFESTORAG_type
  } ffestoragType;

/* Typedefs. */

typedef struct _ffestorag_ *ffestorag;
typedef struct _ffestorag_list_ *ffestoragList;
typedef struct _ffestorag_list_ ffestoragList_;

/* Include files needed by this one. */

#include "bld.h"
#include "info.h"
#include "symbol.h"
#include "target.h"

/* Structure definitions. */

struct _ffestorag_list_
  {
    ffestorag first;		/* First storage area in list. */
    ffestorag last;		/* Last storage area in list. */
  };

struct _ffestorag_
  {
    ffestorag next;		/* Next storage area in list. */
    ffestorag previous;		/* Previous storage area in list. */
    ffestorag parent;		/* Parent who holds aggregate
				   initializations. */
    ffebld init;		/* Initialization expression. */
    ffebld accretion;		/* Initializations seen so far for aggregate. */
    ffetargetOffset accretes;	/* # inits needed to fill entire aggregate. */
    ffesymbol symbol;		/* NULL if typeLOCAL and non-NULL equivs
				   and the first "rooted" symbol not known. */
    ffestoragList_ equivs_;	/* NULL if typeLOCAL and not an EQUIVALENCE
				   area. */
    ffetargetOffset size;	/* Size of area. */
    ffetargetOffset offset;	/* Offset of entity within area, 0 for CBLOCK
				   and non-equivalence LOCAL, <= 0 for equivalence
				   LOCAL. */
    ffetargetAlign alignment;	/* Initial alignment for entity. */
    ffetargetAlign modulo;	/* Modulo within alignment. */
#ifdef FFECOM_storageHOOK
    ffecomStorage hook;		/* Whatever the backend needs here. */
#endif
    ffestoragType type;
    ffeinfoBasictype basic_type;/* NONE= >1 non-CHARACTER; ANY=
				   CHAR+non-CHAR. */
    ffeinfoKindtype kind_type;	/* NONE= >1 kind type or NONE/ANY basic_type. */
    ffesymbol type_symbol;	/* First symbol for basic_type/kind_type. */
    bool is_save;		/* SAVE flag set for this storage area. */
    bool is_init;		/* INIT flag set for this storage area. */
  };

/* Global objects accessed by users of this module. */

extern ffestoragList_ ffestorag_list_;

/* Declare functions with prototypes. */

void ffestorag_drive (ffestoragList sl, void (*fn) (ffestorag mst, ffestorag st),
		      ffestorag mst);
void ffestorag_dump (ffestorag s);
void ffestorag_end_layout (ffesymbol s);
void ffestorag_exec_layout (ffesymbol s);
void ffestorag_init_2 (void);
ffestorag ffestorag_new (ffestoragList sl);
void ffestorag_report (void);
void ffestorag_update (ffestorag s, ffesymbol sym, ffeinfoBasictype bt,
		       ffeinfoKindtype kt);
void ffestorag_update_init (ffestorag s);
void ffestorag_update_save (ffestorag s);

/* Define macros. */

#define ffestorag_accretes(s) ((s)->accretes)
#define ffestorag_accretion(s) ((s)->accretion)
#define ffestorag_alignment(s) ((s)->alignment)
#define ffestorag_basictype(s) ((s)->basic_type)
#define ffestorag_hook(s) ((s)->hook)
#define ffestorag_init(s) ((s)->init)
#define ffestorag_init_0()
#define ffestorag_init_1()
#define ffestorag_init_3()
#define ffestorag_init_4()
#define ffestorag_is_init(s) ((s)->is_init)
#define ffestorag_is_save(s) ((s)->is_save)
#define ffestorag_kindtype(s) ((s)->kind_type)
#define ffestorag_list_equivs(s) (&(s)->equivs_)
#define ffestorag_list_master() (&ffestorag_list_)
#define ffestorag_modulo(s) ((s)->modulo)
#define ffestorag_offset(s) ((s)->offset)
#define ffestorag_parent(s) ((s)->parent)
#define ffestorag_ptr_to_alignment(s) (&(s)->alignment)
#define ffestorag_ptr_to_modulo(s) (&(s)->modulo)
#define ffestorag_set_accretes(s,a) ((s)->accretes = (a))
#define ffestorag_set_accretion(s,a) ((s)->accretion = (a))
#define ffestorag_set_alignment(s,a) ((s)->alignment = (a))
#define ffestorag_set_basictype(s,b) ((s)->basic_type = (b))
#define ffestorag_set_hook(s,h) ((s)->hook = (h))
#define ffestorag_set_init(s,i) ((s)->init = (i))
#define ffestorag_set_is_init(s,in) ((s)->is_init = (in))
#define ffestorag_set_is_save(s,sa) ((s)->is_save = (sa))
#define ffestorag_set_kindtype(s,k) ((s)->kind_type = (k))
#define ffestorag_set_modulo(s,m) ((s)->modulo = (m))
#define ffestorag_set_offset(s,o) ((s)->offset = (o))
#define ffestorag_set_parent(s,p) ((s)->parent = (p))
#define ffestorag_set_size(s,si) ((s)->size = (si))
#define ffestorag_set_symbol(s,sy) ((s)->symbol = (sy))
#define ffestorag_set_type(s,t) ((s)->type = (t))
#define ffestorag_set_typesymbol(s,sy) ((s)->type_symbol = (sy))
#define ffestorag_size(s) ((s)->size)
#define ffestorag_symbol(s) ((s)->symbol)
#define ffestorag_terminate_0()
#define ffestorag_terminate_1()
#define ffestorag_terminate_2()
#define ffestorag_terminate_3()
#define ffestorag_terminate_4()
#define ffestorag_type(s) ((s)->type)
#define ffestorag_typesymbol(s) ((s)->type_symbol)

/* End of #include file. */

#endif /* ! GCC_F_STORAG_H */
