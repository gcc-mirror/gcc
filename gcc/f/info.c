/* info.c -- Implementation File (module.c template V1.0)
   Copyright (C) 1995, 2002, 2003 Free Software Foundation, Inc.
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
      An abstraction for information maintained on a per-operator and per-
      operand basis in expression trees.

   Modifications:
      30-Aug-90	 JCB  2.0
	 Extensive rewrite for new cleaner approach.
*/

/* Include files. */

#include "proj.h"
#include "info.h"
#include "target.h"
#include "type.h"

/* Externals defined here. */


/* Simple definitions and enumerations. */


/* Internal typedefs. */


/* Private include files. */


/* Internal structure definitions. */


/* Static objects accessed by functions in this module.	 */

static const char *const ffeinfo_basictype_string_[]
=
{
#define FFEINFO_BASICTYPE(KWD,LNAM,SNAM) SNAM,
#include "info-b.def"
#undef FFEINFO_BASICTYPE
};
static const char *const ffeinfo_kind_message_[]
=
{
#define FFEINFO_KIND(kwd,msgid,snam) msgid,
#include "info-k.def"
#undef FFEINFO_KIND
};
static const char *const ffeinfo_kind_string_[]
=
{
#define FFEINFO_KIND(KWD,LNAM,SNAM) SNAM,
#include "info-k.def"
#undef FFEINFO_KIND
};
static ffeinfoBasictype ffeinfo_combine_[FFEINFO_basictype][FFEINFO_basictype];
static const char *const ffeinfo_kindtype_string_[]
=
{
  "",
  "1",
  "2",
  "3",
  "4",
  "5",
  "6",
  "7",
  "8",
  "*",
};
static const char *const ffeinfo_where_string_[]
=
{
#define FFEINFO_WHERE(KWD,LNAM,SNAM) SNAM,
#include "info-w.def"
#undef FFEINFO_WHERE
};
static ffetype ffeinfo_types_[FFEINFO_basictype][FFEINFO_kindtype];

/* Static functions (internal). */


/* Internal macros. */


/* ffeinfo_basictype_combine -- Combine two basictypes into highest rank type

   ffeinfoBasictype i, j, k;
   k = ffeinfo_basictype_combine(i,j);

   Returns a type based on "standard" operation between two given types.  */

ffeinfoBasictype
ffeinfo_basictype_combine (ffeinfoBasictype l, ffeinfoBasictype r)
{
  assert (l < FFEINFO_basictype);
  assert (r < FFEINFO_basictype);
  return ffeinfo_combine_[l][r];
}

/* ffeinfo_basictype_string -- Return tiny string showing the basictype

   ffeinfoBasictype i;
   printf("%s",ffeinfo_basictype_string(dt));

   Returns the string based on the basic type.	*/

const char *
ffeinfo_basictype_string (ffeinfoBasictype basictype)
{
  if (basictype >= ARRAY_SIZE (ffeinfo_basictype_string_))
    return "?\?\?";
  return ffeinfo_basictype_string_[basictype];
}

/* ffeinfo_init_0 -- Initialize

   ffeinfo_init_0();  */

void
ffeinfo_init_0 (void)
{
  ffeinfoBasictype i;
  ffeinfoBasictype j;

  assert (FFEINFO_basictype == ARRAY_SIZE (ffeinfo_basictype_string_));
  assert (FFEINFO_kind == ARRAY_SIZE (ffeinfo_kind_message_));
  assert (FFEINFO_kind == ARRAY_SIZE (ffeinfo_kind_string_));
  assert (FFEINFO_kindtype == ARRAY_SIZE (ffeinfo_kindtype_string_));
  assert (FFEINFO_where == ARRAY_SIZE (ffeinfo_where_string_));

  /* Make array that, given two basic types, produces resulting basic type. */

  for (i = 0; i < FFEINFO_basictype; ++i)
    for (j = 0; j < FFEINFO_basictype; ++j)
      if ((i == FFEINFO_basictypeANY) || (j == FFEINFO_basictypeANY))
	ffeinfo_combine_[i][j] = FFEINFO_basictypeANY;
      else
	ffeinfo_combine_[i][j] = FFEINFO_basictypeNONE;

#define same(bt) ffeinfo_combine_[bt][bt] = bt
#define use2(bt1,bt2) ffeinfo_combine_[bt1][bt2]  \
      = ffeinfo_combine_[bt2][bt1] = bt2

  same (FFEINFO_basictypeINTEGER);
  same (FFEINFO_basictypeLOGICAL);
  same (FFEINFO_basictypeREAL);
  same (FFEINFO_basictypeCOMPLEX);
  same (FFEINFO_basictypeCHARACTER);
  use2 (FFEINFO_basictypeINTEGER, FFEINFO_basictypeREAL);
  use2 (FFEINFO_basictypeINTEGER, FFEINFO_basictypeCOMPLEX);
  use2 (FFEINFO_basictypeREAL, FFEINFO_basictypeCOMPLEX);

#undef same
#undef use2
}

/* ffeinfo_kind_message -- Return helpful string showing the kind

   ffeinfoKind kind;
   printf("%s",ffeinfo_kind_message(kind));

   Returns the string based on the kind.  */

const char *
ffeinfo_kind_message (ffeinfoKind kind)
{
  if (kind >= ARRAY_SIZE (ffeinfo_kind_message_))
    return "?\?\?";
  return ffeinfo_kind_message_[kind];
}

/* ffeinfo_kind_string -- Return tiny string showing the kind

   ffeinfoKind kind;
   printf("%s",ffeinfo_kind_string(kind));

   Returns the string based on the kind.  */

const char *
ffeinfo_kind_string (ffeinfoKind kind)
{
  if (kind >= ARRAY_SIZE (ffeinfo_kind_string_))
    return "?\?\?";
  return ffeinfo_kind_string_[kind];
}

ffeinfoKindtype
ffeinfo_kindtype_max(ffeinfoBasictype bt,
		     ffeinfoKindtype k1,
		     ffeinfoKindtype k2)
{
  if ((bt == FFEINFO_basictypeANY)
      || (k1 == FFEINFO_kindtypeANY)
      || (k2 == FFEINFO_kindtypeANY))
    return FFEINFO_kindtypeANY;

  if (ffetype_size (ffeinfo_types_[bt][k1])
      > ffetype_size (ffeinfo_types_[bt][k2]))
    return k1;
  return k2;
}

/* ffeinfo_kindtype_string -- Return tiny string showing the kind type

   ffeinfoKindtype kind_type;
   printf("%s",ffeinfo_kindtype_string(kind));

   Returns the string based on the kind type.  */

const char *
ffeinfo_kindtype_string (ffeinfoKindtype kind_type)
{
  if (kind_type >= ARRAY_SIZE (ffeinfo_kindtype_string_))
    return "?\?\?";
  return ffeinfo_kindtype_string_[kind_type];
}

void
ffeinfo_set_type (ffeinfoBasictype basictype, ffeinfoKindtype kindtype,
		  ffetype type)
{
  assert (basictype < FFEINFO_basictype);
  assert (kindtype < FFEINFO_kindtype);
  assert (ffeinfo_types_[basictype][kindtype] == NULL);

  ffeinfo_types_[basictype][kindtype] = type;
}

ffetype
ffeinfo_type (ffeinfoBasictype basictype, ffeinfoKindtype kindtype)
{
  assert (basictype < FFEINFO_basictype);
  assert (kindtype < FFEINFO_kindtype);

  return ffeinfo_types_[basictype][kindtype];
}

/* ffeinfo_where_string -- Return tiny string showing the where

   ffeinfoWhere where;
   printf("%s",ffeinfo_where_string(where));

   Returns the string based on the where.  */

const char *
ffeinfo_where_string (ffeinfoWhere where)
{
  if (where >= ARRAY_SIZE (ffeinfo_where_string_))
    return "?\?\?";
  return ffeinfo_where_string_[where];
}

/* ffeinfo_new -- Return object representing datatype, kind, and where info

   ffeinfo i;
   i = ffeinfo_new(FFEINFO_datatypeINTEGER,FFEINFO_kindSCALAR,
       FFEINFO_whereLOCAL);

   Returns the string based on the data type.  */

#ifndef __GNUC__
ffeinfo
ffeinfo_new (ffeinfoBasictype basictype, ffeinfoKindtype kindtype,
	     ffeinfoRank rank, ffeinfoKind kind, ffeinfoWhere where,
	     ffetargetCharacterSize size)
{
  ffeinfo i;

  i.basictype = basictype;
  i.kindtype = kindtype;
  i.rank = rank;
  i.size = size;
  i.kind = kind;
  i.where = where;
  i.size = size;

  return i;
}
#endif
