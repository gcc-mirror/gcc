/* stw.h -- Private #include File (module.h template V1.0)
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
      stw.c

   Modifications:
*/

/* Allow multiple inclusion to work. */

#ifndef _H_f_stw
#define _H_f_stw

/* Simple definitions and enumerations. */


/* Typedefs. */

typedef struct _ffestw_ *ffestw;
typedef struct _ffestw_case_ *ffestwCase;
typedef struct _ffestw_select_ *ffestwSelect;
typedef void (*ffestwShriek) (bool ok);

/* Include files needed by this one. */

#include "bld.h"
#include "com.h"
#include "info.h"
#include "lab.h"
#include "lex.h"
#include "malloc.h"
#include "stv.h"
#include "symbol.h"
#include "where.h"

/* Structure definitions. */

struct _ffestw_
  {
    ffestw next_;		/* Next (unused) block, or NULL. */
    ffestw previous_;		/* Previous block, NULL if this is NIL state. */
    ffestw top_do_;		/* Previous or current DO state, or NULL. */
    unsigned long blocknum_;	/* Block # w/in procedure/program. */
    ffestwShriek shriek_;	/* Call me to pop block in a hurry. */
    ffesymbol sym_;		/* Related symbol (if there is one). */
    ffelexToken name_;		/* Construct name (IFTHEN, SELECT, DO only). */
    ffestwSelect select_;	/* Info for SELECT CASE blocks. */
    ffelab label_;		/* For DO blocks w/labels, the target label. */
    ffesymbol do_iter_var_;	/* For iter DO blocks, the iter var or NULL. */
    ffelexToken do_iter_var_t_;	/* The token for do_iter_var. */
    ffewhereLine line_;		/* Where first token of statement triggering
				   state */
    ffewhereColumn col_;	/* was seen in source file. */
    int uses_;			/* # uses (new+use-kill calls). */
    ffestvState state_;
    int substate_;		/* Used on a per-block-state basis. */
#if FFECOM_targetCURRENT == FFECOM_targetGCC
    struct nesting *do_hook_;	/* backend id for given loop (EXIT/CYCLE). */
    tree do_tvar_;		/* tree form of do_iter_var. */
    tree do_incr_saved_;	/* tree SAVED_EXPR of incr expr. */
    tree do_count_var_;		/* tree of countdown variable. */
    tree select_texpr_;		/* tree for end case. */
    bool select_break_;		/* TRUE when CASE should start with gen
				   "break;". */
    int ifthen_fake_else_;	/* Number of fake `else' introductions.  */
#endif	/* FFECOM_targetCURRENT == FFECOM_targetGCC*/
  };

struct _ffestw_case_
  {
    ffestwCase next_rel;	/* Next case range in relational order. */
    ffestwCase previous_rel;	/* Previous case range in relational order. */
    ffestwCase next_stmt;	/* Next range in stmt or first in next stmt. */
    ffestwCase previous_stmt;	/* Previous range. */
    ffebldConstant low;		/* Low value in range. */
    ffebldConstant high;	/* High value in range. */
    unsigned long casenum;	/* CASE stmt index for this range/value. */
    ffelexToken t;		/* Token for this range/value; ffestc only. */
  };

struct _ffestw_select_
  {
    ffestwCase first_rel;	/* First CASE range (after low) in order. */
    ffestwCase last_rel;	/* Last CASE range (before high) in order. */
    ffestwCase first_stmt;	/* First range in first CASE stmt. */
    ffestwCase last_stmt;	/* Last range in last CASE stmt. */
    mallocPool pool;		/* Pool in which this and all cases are
				   allocated. */
    unsigned long cases;	/* Number of CASE stmts seen so far. */
    ffelexToken t;		/* First token of selected expression; ffestc
				   only. */
    ffeinfoBasictype type;	/* Basic type (integer, character, or
				   logical). */
    ffeinfoKindtype kindtype;	/* Kind type. */
  };

/* Global objects accessed by users of this module. */

extern ffestw ffestw_stack_top_;

/* Declare functions with prototypes. */

void ffestw_display_state (void);
void ffestw_kill (ffestw block);
void ffestw_init_0 (void);
ffestw ffestw_new (void);
ffestw ffestw_pop (void);
ffestw ffestw_push (ffestw block);
ffestw ffestw_update (ffestw block);
ffestw ffestw_use (ffestw block);

/* Define macros. */

#define ffestw_blocknum(b) ((b)->blocknum_)
#define ffestw_col(b) ((b)->col_)
#define ffestw_do_count_var(b) ((b)->do_count_var_)
#define ffestw_do_hook(b) ((b)->do_hook_)
#define ffestw_do_incr_saved(b) ((b)->do_incr_saved_)
#define ffestw_do_iter_var(b) ((b)->do_iter_var_)
#define ffestw_do_iter_var_t(b) ((b)->do_iter_var_t_)
#define ffestw_do_tvar(b) ((b)->do_tvar_)
#define ffestw_ifthen_fake_else(b) ((b)->ifthen_fake_else_)
#define ffestw_init_1()
#define ffestw_init_2()
#define ffestw_init_3()
#define ffestw_init_4()
#define ffestw_label(b) ((b)->label_)
#define ffestw_line(b) ((b)->line_)
#define ffestw_name(b) ((b)->name_)
#define ffestw_previous(b) ((b)->previous_)
#define ffestw_select(b) ((b)->select_)
#define ffestw_select_break(b) ((b)->select_break_)
#define ffestw_select_texpr(b) ((b)->select_texpr_)
#define ffestw_set_blocknum(b,bl) ((b)->blocknum_ = (bl))
#define ffestw_set_col(b,c) ((b)->col_ = (c))
#define ffestw_set_do_count_var(b,d) ((b)->do_count_var_ = (d))
#define ffestw_set_do_hook(b,d) ((b)->do_hook_ = (d))
#define ffestw_set_do_incr_saved(b,d) ((b)->do_incr_saved_ = (d))
#define ffestw_set_do_iter_var(b,v) ((b)->do_iter_var_ = (v))
#define ffestw_set_do_iter_var_t(b,t) ((b)->do_iter_var_t_ = (t))
#define ffestw_set_do_tvar(b,d) ((b)->do_tvar_ = (d))
#define ffestw_set_ifthen_fake_else(b,e) ((b)->ifthen_fake_else_ = (e))
#define ffestw_set_label(b,l) ((b)->label_ = (l))
#define ffestw_set_line(b,l) ((b)->line_ = (l))
#define ffestw_set_name(b,n) ((b)->name_ = (n))
#define ffestw_set_select(b,s) ((b)->select_ = (s))
#define ffestw_set_select_break(b,br) ((b)->select_break_ = (br))
#define ffestw_set_select_texpr(b,t) ((b)->select_texpr_ = (t))
#define ffestw_set_shriek(b,s) ((b)->shriek_ = (s))
#define ffestw_set_state(b,s) ((b)->state_ = (s))
#define ffestw_set_substate(b,s) ((b)->substate_ = (s))
#define ffestw_set_sym(b,s) ((b)->sym_= (s))
#define ffestw_set_top_do(b,t) ((b)->top_do_ = (t))
#define ffestw_shriek(b) ((b)->shriek_)
#define ffestw_stack_top() ffestw_stack_top_
#define ffestw_state(b) ((b)->state_)
#define ffestw_substate(b) ((b)->substate_)
#define ffestw_sym(b) ((b)->sym_)
#define ffestw_terminate_0()
#define ffestw_terminate_1()
#define ffestw_terminate_2()
#define ffestw_terminate_3()
#define ffestw_terminate_4()
#define ffestw_top_do(b) ((b)->top_do_)

/* End of #include file. */

#endif
