/* global.h -- Public #include File (module.h template V1.0)
   Copyright (C) 1995, 1997, 2003 Free Software Foundation, Inc.
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
      global.c

   Modifications:
*/

/* Allow multiple inclusion to work. */

#ifndef GCC_F_GLOBAL_H
#define GCC_F_GLOBAL_H

/* Simple definitions and enumerations. */

typedef enum
  {
    FFEGLOBAL_typeNONE,
    FFEGLOBAL_typeMAIN,
    FFEGLOBAL_typeEXT,		/* EXTERNAL is all we know. */
    FFEGLOBAL_typeSUBR,
    FFEGLOBAL_typeFUNC,
    FFEGLOBAL_typeBDATA,
    FFEGLOBAL_typeCOMMON,
    FFEGLOBAL_typeANY,		/* Confusion reigns, so just ignore. */
    FFEGLOBAL_type
  } ffeglobalType;

typedef enum
  {
    FFEGLOBAL_argsummaryNONE,	/* No arg present. */
    FFEGLOBAL_argsummaryVAL,	/* Pass-by-value. */
    FFEGLOBAL_argsummaryREF,	/* Pass-by-reference. */
    FFEGLOBAL_argsummaryDESCR,	/* Pass-by-descriptor. */
    FFEGLOBAL_argsummaryPROC,	/* Procedure (intrinsic, external). */
    FFEGLOBAL_argsummarySUBR,	/* Subroutine (intrinsic, external). */
    FFEGLOBAL_argsummaryFUNC,	/* Function (intrinsic, external). */
    FFEGLOBAL_argsummaryALTRTN,	/* Alternate-return (label). */
    FFEGLOBAL_argsummaryANY,
    FFEGLOBAL_argsummary
  } ffeglobalArgSummary;

/* Typedefs. */

typedef struct _ffeglobal_arginfo_ *ffeglobalArgInfo_;
typedef struct _ffeglobal_ *ffeglobal;

/* Include files needed by this one. */

#include "info.h"
#include "lex.h"
#include "name.h"
#include "symbol.h"
#include "target.h"
#include "top.h"

/* Structure definitions. */

struct _ffeglobal_arginfo_
{
  ffelexToken t;	/* Different from master token when difference is important. */
  char *name;		/* Name of dummy arg, or NULL if not yet known. */
  ffeglobalArgSummary as;
  ffeinfoBasictype bt;
  ffeinfoKindtype kt;
  bool array;
};

struct _ffeglobal_
{
  ffelexToken t;
  ffename n;
  ffecomGlobal hook;
  ffeCounter tick;		/* Recent transition in this progunit. */
  ffeglobalType type;
  bool intrinsic;		/* Known as intrinsic? */
  bool explicit_intrinsic;	/* Explicit intrinsic? */
  union {
    struct {
      ffelexToken initt;	/* First initial value. */
      bool have_pad;		/* Padding info avail for COMMON? */
      ffetargetAlign pad;	/* Initial padding for COMMON. */
      ffewhereLine pad_where_line;
      ffewhereColumn pad_where_col;
      bool have_save;		/* Save info avail for COMMON? */
      bool save;		/* Save info for COMMON. */
      ffewhereLine save_where_line;
      ffewhereColumn save_where_col;
      bool have_size;		/* Size info avail for COMMON? */
      ffetargetOffset size;	/* Size info for COMMON. */
      bool blank;		/* TRUE if blank COMMON. */
    } common;
    struct {
      bool defined;		/* Seen actual code yet? */
      ffeinfoBasictype bt;	/* NONE for non-function. */
      ffeinfoKindtype kt;	/* NONE for non-function. */
      ffetargetCharacterSize sz;
      int n_args;		/* 0 for main/blockdata. */
      ffelexToken other_t;	/* Location of reference. */
      ffeglobalArgInfo_ arg_info;	/* Info on each argument. */
    } proc;
  } u;
};

/* Global objects accessed by users of this module. */


/* Declare functions with prototypes. */

void ffeglobal_drive (ffeglobal (*fn) (ffeglobal));
void ffeglobal_init_1 (void);
void ffeglobal_init_common (ffesymbol s, ffelexToken t);
void ffeglobal_new_progunit_ (ffesymbol s, ffelexToken t, ffeglobalType type);
void ffeglobal_new_common (ffesymbol s, ffelexToken t, bool blank);
void ffeglobal_pad_common (ffesymbol s, ffetargetAlign pad, ffewhereLine wl,
			   ffewhereColumn wc);
void ffeglobal_proc_def_arg (ffesymbol s, int argno, const char *name, ffeglobalArgSummary as,
			     ffeinfoBasictype bt, ffeinfoKindtype kt,
			     bool array);
void ffeglobal_proc_def_nargs (ffesymbol s, int n_args);
bool ffeglobal_proc_ref_arg (ffesymbol s, int argno, ffeglobalArgSummary as,
			     ffeinfoBasictype bt, ffeinfoKindtype kt,
			     bool array, ffelexToken t);
bool ffeglobal_proc_ref_nargs (ffesymbol s, int n_args, ffelexToken t);
ffeglobal ffeglobal_promoted (ffesymbol s);
void ffeglobal_ref_intrinsic (ffesymbol s, ffelexToken t, bool explicit);
bool ffeglobal_ref_progunit_ (ffesymbol s, ffelexToken t, ffeglobalType type);
void ffeglobal_save_common (ffesymbol s, bool save, ffewhereLine wl,
			    ffewhereColumn wc);
bool ffeglobal_size_common (ffesymbol s, ffetargetOffset size);
void ffeglobal_terminate_1 (void);

/* Define macros. */

#define FFEGLOBAL_ENABLED 1

#define ffeglobal_common_init(g) ((g)->tick != 0)
#define ffeglobal_common_have_pad(g) ((g)->u.common.have_pad)
#define ffeglobal_common_have_size(g) ((g)->u.common.have_size)
#define ffeglobal_common_pad(g) ((g)->u.common.pad)
#define ffeglobal_common_size(g) ((g)->u.common.size)
#define ffeglobal_hook(g) ((g)->hook)
#define ffeglobal_init_0()
#define ffeglobal_init_2()
#define ffeglobal_init_3()
#define ffeglobal_init_4()
#define ffeglobal_new_blockdata(s,t) \
      ffeglobal_new_progunit_(s,t,FFEGLOBAL_typeBDATA)
#define ffeglobal_new_function(s,t) \
      ffeglobal_new_progunit_(s,t,FFEGLOBAL_typeFUNC)
#define ffeglobal_new_program(s,t) \
      ffeglobal_new_progunit_(s,t,FFEGLOBAL_typeMAIN)
#define ffeglobal_new_subroutine(s,t) \
      ffeglobal_new_progunit_(s,t,FFEGLOBAL_typeSUBR)
#define ffeglobal_ref_blockdata(s,t) \
      ffeglobal_ref_progunit_(s,t,FFEGLOBAL_typeBDATA)
#define ffeglobal_ref_external(s,t) \
      ffeglobal_ref_progunit_(s,t,FFEGLOBAL_typeEXT)
#define ffeglobal_ref_function(s,t) \
      ffeglobal_ref_progunit_(s,t,FFEGLOBAL_typeFUNC)
#define ffeglobal_ref_subroutine(s,t) \
      ffeglobal_ref_progunit_(s,t,FFEGLOBAL_typeSUBR)
#define ffeglobal_set_hook(g,h) ((g)->hook = (h))
#define ffeglobal_terminate_0()
#define ffeglobal_terminate_2()
#define ffeglobal_terminate_3()
#define ffeglobal_terminate_4()
#define ffeglobal_text(g) ffename_text((g)->n)
#define ffeglobal_type(g) ((g)->type)

/* End of #include file. */

#endif /* ! GCC_F_GLOBAL_H */

