/* info.h -- Public #include File (module.h template V1.0)
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
      info.c

   Modifications:
      30-Aug-90	 JCB  2.0
	 Extensive rewrite for new cleaner approach.
*/

/* Allow multiple inclusion to work. */

#ifndef _H_f_info
#define _H_f_info

/* Simple definitions and enumerations. */

typedef enum
  {
#define FFEINFO_BASICTYPE(KWD,LNAM,SNAM) KWD,
#include "info-b.def"
#undef FFEINFO_BASICTYPE
    FFEINFO_basictype
  } ffeinfoBasictype;

typedef enum
  {				/* If these kindtypes aren't in size order,
				   change _kindtype_max. */
    FFEINFO_kindtypeNONE,
    FFEINFO_kindtypeINTEGER1,
    FFEINFO_kindtypeINTEGER2,
    FFEINFO_kindtypeINTEGER3,
    FFEINFO_kindtypeINTEGER4,
    FFEINFO_kindtypeINTEGER5,
    FFEINFO_kindtypeINTEGER6,
    FFEINFO_kindtypeINTEGER7,
    FFEINFO_kindtypeINTEGER8,
    FFEINFO_kindtypeLOGICAL1 = 1,	/* Ok to omit, but ok to overlap. */
    FFEINFO_kindtypeLOGICAL2,
    FFEINFO_kindtypeLOGICAL3,
    FFEINFO_kindtypeLOGICAL4,
    FFEINFO_kindtypeLOGICAL5,
    FFEINFO_kindtypeLOGICAL6,
    FFEINFO_kindtypeLOGICAL7,
    FFEINFO_kindtypeLOGICAL8,
    FFEINFO_kindtypeREAL1 = 1,	/* Ok to omit, but ok to overlap. */
    FFEINFO_kindtypeREAL2,
    FFEINFO_kindtypeREAL3,
    FFEINFO_kindtypeREAL4,
    FFEINFO_kindtypeREAL5,
    FFEINFO_kindtypeREAL6,
    FFEINFO_kindtypeREAL7,
    FFEINFO_kindtypeREAL8,
    FFEINFO_kindtypeCHARACTER1 = 1,	/* Ok to omit, but ok to overlap. */
    FFEINFO_kindtypeCHARACTER2,
    FFEINFO_kindtypeCHARACTER3,
    FFEINFO_kindtypeCHARACTER4,
    FFEINFO_kindtypeCHARACTER5,
    FFEINFO_kindtypeCHARACTER6,
    FFEINFO_kindtypeCHARACTER7,
    FFEINFO_kindtypeCHARACTER8,
    FFEINFO_kindtypeANY,
    FFEINFO_kindtype
  } ffeinfoKindtype;

typedef enum
  {
#define FFEINFO_KIND(KWD,LNAM,SNAM) KWD,
#include "info-k.def"
#undef FFEINFO_KIND
    FFEINFO_kind
  } ffeinfoKind;

typedef enum
  {
#define FFEINFO_WHERE(KWD,LNAM,SNAM) KWD,
#include "info-w.def"
#undef FFEINFO_WHERE
    FFEINFO_where
  } ffeinfoWhere;

/* Typedefs. */

typedef struct _ffeinfo_ ffeinfo;
typedef char ffeinfoRank;

/* Include files needed by this one. */

#include "target.h"
#include "type.h"

/* Structure definitions. */

struct _ffeinfo_
  {
    ffeinfoBasictype basictype;
    ffeinfoKindtype kindtype;
    ffeinfoRank rank;
    ffeinfoKind kind;
    ffeinfoWhere where;
    ffetargetCharacterSize size;
  };

/* Global objects accessed by users of this module. */


/* Declare functions with prototypes. */

ffeinfoBasictype ffeinfo_basictype_combine (ffeinfoBasictype l,
					    ffeinfoBasictype r);
const char *ffeinfo_basictype_string (ffeinfoBasictype basictype);
void ffeinfo_init_0 (void);
const char *ffeinfo_kind_message (ffeinfoKind kind);
const char *ffeinfo_kind_string (ffeinfoKind kind);
ffeinfoKindtype ffeinfo_kindtype_max(ffeinfoBasictype bt,
				     ffeinfoKindtype k1,
				     ffeinfoKindtype k2);
const char *ffeinfo_kindtype_string (ffeinfoKindtype kind_type);
const char *ffeinfo_where_string (ffeinfoWhere where);
ffeinfo ffeinfo_new (ffeinfoBasictype basictype, ffeinfoKindtype kindtype,
		     ffeinfoRank rank, ffeinfoKind kind, ffeinfoWhere where,
		     ffetargetCharacterSize size);
void ffeinfo_set_type (ffeinfoBasictype basictype, ffeinfoKindtype kindtype,
		       ffetype type);
ffetype ffeinfo_type (ffeinfoBasictype basictype, ffeinfoKindtype kindtype);

/* Define macros. */

#define ffeinfo_basictype(i) (i.basictype)
#define ffeinfo_init_1()
#define ffeinfo_init_2()
#define ffeinfo_init_3()
#define ffeinfo_init_4()
#define ffeinfo_kind(i) (i.kind)
#define ffeinfo_kindtype(i) (i.kindtype)
#ifdef __GNUC__
#define ffeinfo_new(bt,kt,r,k,w,sz) \
  ((ffeinfo) {(bt), (kt), (r), (k), (w), (sz)})
#endif
#define ffeinfo_new_any()						      \
  ffeinfo_new (FFEINFO_basictypeANY, FFEINFO_kindtypeANY, 0,		      \
	       FFEINFO_kindANY, FFEINFO_whereANY,			      \
	       FFETARGET_charactersizeNONE)
#define ffeinfo_new_null()						      \
  ffeinfo_new (FFEINFO_basictypeNONE, FFEINFO_kindtypeNONE, 0,		      \
	       FFEINFO_kindNONE, FFEINFO_whereNONE,			      \
	       FFETARGET_charactersizeNONE)
#define ffeinfo_rank(i) (i.rank)
#define ffeinfo_size(i) (i.size)
#define ffeinfo_terminate_0()
#define ffeinfo_terminate_1()
#define ffeinfo_terminate_2()
#define ffeinfo_terminate_3()
#define ffeinfo_terminate_4()
#define ffeinfo_use(i) i
#define ffeinfo_where(i) (i.where)

#define FFEINFO_kindtypeINTEGERDEFAULT FFEINFO_kindtypeINTEGER1
#define FFEINFO_kindtypeLOGICALDEFAULT FFEINFO_kindtypeLOGICAL1
#define FFEINFO_kindtypeREALDEFAULT FFEINFO_kindtypeREAL1
#define FFEINFO_kindtypeREALDOUBLE FFEINFO_kindtypeREAL2
#define FFEINFO_kindtypeREALQUAD FFEINFO_kindtypeREAL3
#define FFEINFO_kindtypeCHARACTERDEFAULT FFEINFO_kindtypeCHARACTER1

/* End of #include file. */

#endif
