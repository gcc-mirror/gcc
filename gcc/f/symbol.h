/* Interface definitions for Fortran symbol manager
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
02111-1307, USA.  */

#ifndef _H_f_symbol
#define _H_f_symbol

/* The main symbol type.  */

typedef struct _ffesymbol_ *ffesymbol;

/* State of understanding about what the symbol represents.  */

enum _ffesymbol_state_
  {
/* See ffesymbol_state_is_exec() macro below when making changes.  */
    FFESYMBOL_stateNONE,	/* Never before seen. */
    FFESYMBOL_stateSEEN,	/* Seen before exec transition and not yet
				   understood (info not filled in, etc). */
    FFESYMBOL_stateUNCERTAIN,	/* Almost understood (info partly filled in). */
    FFESYMBOL_stateUNDERSTOOD,	/* Fully understood (info filled in). */
    FFESYMBOL_state
  };
typedef enum _ffesymbol_state_ ffesymbolState;
#define ffesymbolState_f ""

/* Attributes.  Symbols acquire attributes while their state is SEEN.
   These attributes are basically ignored once the symbol becomes
   UNDERSTOOD.  */

typedef long int ffesymbolAttrs;/* Holds set of attributes. */
#define ffesymbolAttrs_f "l"

enum _ffesymbol_attr_
  {
#define DEFATTR(ATTR,ATTRS,NAME) ATTR,
#include "symbol.def"
#undef DEFATTR
    FFESYMBOL_attr
  };				/* A given attribute. */
typedef enum _ffesymbol_attr_ ffesymbolAttr;
#define ffesymbolAttr_f ""

#define FFESYMBOL_attrsetNONE 0
#define FFESYMBOL_attrsetALL (((ffesymbolAttrs) 1 << FFESYMBOL_attr) - 1)

/* This is just for avoiding complaining about, e.g., "I = IABS(3)", that
   IABS doesn't meet the requirements for a user-defined symbol name as
   a result of, say, --symbol-case-lower, if IABS turns out to indeed be
   a reference to the intrinsic IABS (in which case it's a Fortran keyword
   like CALL) and not a user-defined name.  */

enum _ffesymbol_checkstate_
  {
    FFESYMBOL_checkstateNONE_,	/* Not checked/never necessary to check. */
    FFESYMBOL_checkstateINHIBITED_,	/* Bad name, but inhibited. */
    FFESYMBOL_checkstatePENDING_,	/* Bad name, might be intrinsic. */
    FFESYMBOL_checkstateCHECKED_,	/* Ok name, intrinsic, or bad name
					   reported. */
    FFESYMBOL_checkstate_
  };
typedef enum _ffesymbol_checkstate_ ffesymbolCheckState_;
#define ffesymbolCheckState_f_ ""

#include "bld.h"
#include "com.h"
#include "equiv.h"
#include "global.h"
#include "info.h"
#include "intrin.h"
#include "lex.h"
#include "malloc.h"
#include "name.h"
#include "storag.h"
#include "target.h"
#include "top.h"
#include "where.h"

struct _ffesymbol_
  {
    ffename name;
    ffename other_space_name;	/* For dual-space objects. */
    ffeglobal global;		/* In filewide name space. */
    ffesymbolAttrs attrs;	/* What kind of symbol am I? */
    ffesymbolState state;	/* What state am I in? */
    ffeinfo info;		/* Info filled in when _stateUNDERSTOOD. */
    ffebld dims;		/* Dimension list expression. */
    ffebld extents;		/* Extents list expression. */
    ffebld dim_syms;		/* List of SYMTERs of all symbols in dims. */
    ffebld array_size;		/* Size as an expression involving some of
				   dims. */
    ffebld init;		/* Initialization expression or expr list or
				   PARAMETER value. */
    ffebld accretion;		/* Initializations seen so far for
				   array/substr. */
    ffetargetOffset accretes;	/* # inits needed to fill entire array. */
    ffebld dummy_args;		/* For functions, subroutines, and entry
				   points. */
    ffebld namelist;		/* List of symbols in NML. */
    ffebld common_list;		/* List of entities in BCB/NCB. */
    ffebld sfunc_expr;		/* SFN's expression. */
    ffebldListBottom list_bottom;	/* For BCB, NCB, NML. */
    ffesymbol common;		/* Who is my containing COMMON area? */
    ffeequiv equiv;		/* Who have I been equivalenced with? */
    ffestorag storage;		/* Where am I in relation to my outside
				   world? */
#ifdef FFECOM_symbolHOOK
    ffecomSymbol hook;		/* Whatever the compiler/backend wants! */
#endif
    ffesymbol sfa_dummy_parent;	/* "X" outside sfunc "CIRC(X) = 3.14 * X". */
    ffesymbol func_result;	/* FUN sym's corresponding RES sym, & vice
				   versa. */
    ffetargetIntegerDefault value;	/* IMMEDIATE (DATA impdo) value. */
    ffesymbolCheckState_ check_state;	/* Valid name? */
    ffelexToken check_token;	/* checkstatePENDING_ only. */
    int max_entry_num;		/* For detecting dummy arg listed twice/IMPDO
				   iterator nesting violation; also for id of
				   sfunc dummy arg. */
    int num_entries;		/* Number of entry points in which this
				   symbol appears as a dummy arg; helps
				   determine whether arg might not be passed,
				   for example.  */
    ffeintrinGen generic;	/* Generic intrinsic id, if any. */
    ffeintrinSpec specific;	/* Specific intrinsic id, if any. */
    ffeintrinImp implementation;/* Implementation id, if any. */
    bool is_save;		/* SAVE flag set for this symbol (see also
				   ffe_is_saveall()). */
    bool is_init;		/* INIT flag set for this symbol. */
    bool do_iter;		/* Is currently a DO-loop iter (can't be
				   changed in loop). */
    bool reported;		/* (Debug) TRUE if the latest version has
				   been reported. */
    bool have_old;		/* TRUE if old copy of this symbol saved
				   away. */
    bool explicit_where;	/* TRUE if INTRINSIC/EXTERNAL explicit. */
    bool namelisted;		/* TRUE if in NAMELIST (needs static alloc). */
    bool assigned;		/* TRUE if ever ASSIGNed to.  */
  };

#define ffesymbol_accretes(s) ((s)->accretes)
#define ffesymbol_accretion(s) ((s)->accretion)
#define ffesymbol_arraysize(s) ((s)->array_size)
#define ffesymbol_assigned(s) ((s)->assigned)
#define ffesymbol_attr(s,a) ((s)->attrs & ((ffesymbolAttrs) 1 << (a)))
#define ffesymbol_attrs(s) ((s)->attrs)
const char *ffesymbol_attrs_string (ffesymbolAttrs attrs);
#define ffesymbol_basictype(s) ffeinfo_basictype((s)->info)
void ffesymbol_check (ffesymbol s, ffelexToken t, bool maybe_intrin);
#define ffesymbol_common(s) ((s)->common)
#define ffesymbol_commonlist(s) ((s)->common_list)
ffesymbol ffesymbol_declare_blockdataunit (ffelexToken t, ffewhereLine wl,
					   ffewhereColumn wc);
ffesymbol ffesymbol_declare_cblock (ffelexToken t, ffewhereLine wl,
				    ffewhereColumn wc);
ffesymbol ffesymbol_declare_funcnotresunit (ffelexToken t);
ffesymbol ffesymbol_declare_funcresult (ffelexToken t);
ffesymbol ffesymbol_declare_funcunit (ffelexToken t);
ffesymbol ffesymbol_declare_local (ffelexToken t, bool maybe_intrin);
ffesymbol ffesymbol_declare_programunit (ffelexToken t, ffewhereLine wl,
					 ffewhereColumn wc);
ffesymbol ffesymbol_declare_sfdummy (ffelexToken t);
ffesymbol ffesymbol_declare_subrunit (ffelexToken t);
#define ffesymbol_dims(s) ((s)->dims)
#define ffesymbol_dim_syms(s) ((s)->dim_syms)
void ffesymbol_drive (ffesymbol (*fn) (ffesymbol));
void ffesymbol_drive_sfnames (ffesymbol (*fn) (ffesymbol));
#define ffesymbol_dummyargs(s) ((s)->dummy_args)
#if FFECOM_targetCURRENT == FFECOM_targetFFE
void ffesymbol_dump (ffesymbol s);
#endif
void ffesymbol_error (ffesymbol s, ffelexToken t);
#define ffesymbol_equiv(s) ((s)->equiv)
#define ffesymbol_explicitwhere(s) ((s)->explicit_where)
#define ffesymbol_extents(s) ((s)->extents)
#define ffesymbol_first_token(s) ((s)->name == NULL ? NULL  \
      : ffename_first_token((s)->name))
#define ffesymbol_funcresult(s) ((s)->func_result)
#define ffesymbol_generic(s) ((s)->generic)
#define ffesymbol_global(s) ((s)->global)
#define ffesymbol_hook(s) ((s)->hook)
#define ffesymbol_implementation(s) ((s)->implementation)
#define ffesymbol_info(s) ((s)->info)
#define ffesymbol_init(s) ((s)->init)
void ffesymbol_init_0 (void);
void ffesymbol_init_1 (void);
void ffesymbol_init_2 (void);
void ffesymbol_init_3 (void);
void ffesymbol_init_4 (void);
#define ffesymbol_is_doiter(s) ((s)->do_iter)
#define ffesymbol_is_dualspace(s) ((s)->other_space_name != NULL)
#define ffesymbol_is_f2c(s) (ffe_is_f2c())
#define ffesymbol_is_init(s) ((s)->is_init)
#define ffesymbol_is_reported(s) ((s)->reported)
#define ffesymbol_is_save(s) ((s)->is_save)
#define ffesymbol_is_specable(s) ffesymbol_state_is_specable(s->state)
#define ffesymbol_kindtype(s) ffeinfo_kindtype((s)->info)
#define ffesymbol_kind(s) ffeinfo_kind((s)->info)
ffesymbol ffesymbol_lookup_local (ffelexToken t);
#define ffesymbol_maxentrynum(s) ((s)->max_entry_num)
#define ffesymbol_name(s) ((s)->name)
#define ffesymbol_namelist(s) ((s)->namelist)
#define ffesymbol_namelisted(s) ((s)->namelisted)
#define ffesymbol_numentries(s) ((s)->num_entries)
#define ffesymbol_ptr_to_commonlist(s) (&(s)->common_list)
#define ffesymbol_ptr_to_listbottom(s) (&(s)->list_bottom)
#define ffesymbol_ptr_to_namelist(s) (&(s)->namelist)
#define ffesymbol_rank(s) ffeinfo_rank((s)->info)
void ffesymbol_reference (ffesymbol s, ffelexToken t, bool explicit);
#if FFECOM_targetCURRENT == FFECOM_targetFFE
ffesymbol ffesymbol_report (ffesymbol s);
void ffesymbol_report_all (void);
#endif
void ffesymbol_resolve_intrin (ffesymbol s);
void ffesymbol_retract (bool retract);
bool ffesymbol_retractable (void);
#define ffesymbol_set_accretes(s,a) ((s)->accretes = (a))
#define ffesymbol_set_accretion(s,a) ((s)->accretion = (a))
#define ffesymbol_set_arraysize(s,a) ((s)->array_size = (a))
#define ffesymbol_set_assigned(s,a) ((s)->assigned = (a))
#define ffesymbol_set_attr(s,a) ((s)->attrs |= ((ffesymbolAttrs) 1 << (a)))
#define ffesymbol_set_attrs(s,a) ((s)->attrs = (a))
#define ffesymbol_set_common(s,c) ((s)->common = (c))
#define ffesymbol_set_commonlist(s,c) ((s)->common_list = (c))
#define ffesymbol_set_dims(s,d) ((s)->dims = (d))
#define ffesymbol_set_dim_syms(s,d) ((s)->dim_syms = (d))
#define ffesymbol_set_dummyargs(s,d) ((s)->dummy_args = (d))
#define ffesymbol_set_equiv(s,e) ((s)->equiv = (e))
#define ffesymbol_set_explicitwhere(s,e) ((s)->explicit_where = (e))
#define ffesymbol_set_extents(s,e) ((s)->extents = (e))
#define ffesymbol_set_funcresult(s,f) ((s)->func_result = (f))
#define ffesymbol_set_generic(s,g) ((s)->generic = (g))
#define ffesymbol_set_global(s,g) ((s)->global = (g))
#define ffesymbol_set_hook(s,h) ((s)->hook = (h))
#define ffesymbol_set_implementation(s,im) ((s)->implementation = (im))
#define ffesymbol_set_init(s,i) ((s)->init = (i))
#define ffesymbol_set_info(s,i) ((s)->info = (i))
#define ffesymbol_set_is_doiter(s,f) ((s)->do_iter = (f))
#define ffesymbol_set_is_init(s,in) ((s)->is_init = (in))
#define ffesymbol_set_is_save(s,sa) ((s)->is_save = (sa))
#define ffesymbol_set_maxentrynum(s,m) ((s)->max_entry_num = (m))
#define ffesymbol_set_namelist(s,n) ((s)->namelist = (n))
#define ffesymbol_set_namelisted(s,n) ((s)->namelisted = (n))
#define ffesymbol_set_numentries(s,n) ((s)->num_entries = (n))
void ffesymbol_set_retractable (mallocPool pool);
#define ffesymbol_set_sfexpr(s,e) ((s)->sfunc_expr = (e))
#define ffesymbol_set_specific(s,sp) ((s)->specific = (sp))
#define ffesymbol_set_state(s,st) ((s)->state = (st))
#define ffesymbol_set_storage(s,st) ((s)->storage = (st))
#define ffesymbol_set_value(s,v) ((s)->value = (v))
#define ffesymbol_sfdummyparent(s) ((s)->sfa_dummy_parent)
#define ffesymbol_sfexpr(s) ((s)->sfunc_expr)
void ffesymbol_signal_change (ffesymbol s);
#define ffesymbol_signal_unreported(s) ((s)->reported = FALSE)
#define ffesymbol_size(s) ffeinfo_size((s)->info)
#define ffesymbol_specific(s) ((s)->specific)
#define ffesymbol_state(s) ((s)->state)
#define ffesymbol_state_is_specable(s) ((s) <= FFESYMBOL_stateSEEN)
const char *ffesymbol_state_string (ffesymbolState state);
#define ffesymbol_storage(s) ((s)->storage)
void ffesymbol_terminate_0 (void);
void ffesymbol_terminate_1 (void);
void ffesymbol_terminate_2 (void);
void ffesymbol_terminate_3 (void);
void ffesymbol_terminate_4 (void);
#define ffesymbol_text(s) (((s)->name == NULL) ? "<->" : ffename_text((s)->name))
void ffesymbol_update_init (ffesymbol s);
void ffesymbol_update_save (ffesymbol s);
#define ffesymbol_value(s) ((s)->value)
#define ffesymbol_where(s) ffeinfo_where((s)->info)
#define ffesymbol_where_column(s) (((s)->name == NULL)	\
      ? ffewhere_column_unknown() : ffename_where_column((s)->name))
#define ffesymbol_where_filename(s) \
      ffewhere_line_filename(ffesymbol_where_line(s))
#define ffesymbol_where_filelinenum(s) \
      ffewhere_line_filelinenum(ffesymbol_where_line(s))
#define ffesymbol_where_line(s) (((s)->name == NULL) ? ffewhere_line_unknown() \
      : ffename_where_line((s)->name))

#endif
