/* stb.c -- Implementation File (module.c template V1.0)
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

   Related Modules:
      st.c

   Description:
      Parses the proper form for statements, builds up expression trees for
      them, but does not actually implement them.  Uses ffebad (primarily via
      ffesta_ffebad_start) to indicate errors in form.	In many cases, an invalid
      statement form indicates another possible statement needs to be looked at
      by ffest.	 In a few cases, a valid statement form might not completely
      determine the nature of the statement, as in REALFUNCTIONA(B), which is
      a valid form for either the first statement of a function named A taking
      an argument named B or for the declaration of a real array named FUNCTIONA
      with an adjustable size of B.  A similar (though somewhat easier) choice
      must be made for the statement-function-def vs. assignment forms, as in
      the case of FOO(A) = A+2.0.

      A given parser consists of one or more state handlers, the first of which
      is the initial state, and the last of which (for any given input) returns
      control to a final state handler (ffesta_zero or ffesta_two, explained
      below).  The functions handling the states for a given parser usually have
      the same names, differing only in the final number, as in ffestb_foo_
      (handles the initial state), ffestb_foo_1_, ffestb_foo_2_ (handle
      subsequent states), although liberties sometimes are taken with the "foo"
      part either when keywords are clarified into given statements or are
      transferred into other possible areas.  (For example, the type-name
      states can hop over to _dummy_ functions when the FUNCTION or RECURSIVE
      keywords are seen, though this kind of thing is kept to a minimum.)  Only
      the names without numbers are exported to the rest of ffest; the others
      are local (static).

      Each initial state is provided with the first token in ffesta_tokens[0],
      which will be killed upon return to the final state (ffesta_zero or
      ffelex_swallow_tokens passed through to ffesta_zero), so while it may
      be changed to another token, a valid token must be left there to be
      killed.  Also, a "convenient" array of tokens are left in
      ffesta_tokens[1..FFESTA_tokensMAX].  The initial state of this set of
      elements is undefined, thus, if tokens are stored here, they must be
      killed before returning to the final state.  Any parser may also use
      cross-state local variables by sticking a structure containing storage
      for those variables in the local union ffestb_local_ (unless the union
      goes on strike).	Furthermore, parsers that handle more than one first or
      second tokens (like _varlist_, which handles EXTERNAL, INTENT, INTRINSIC,
      OPTIONAL,
      PUBLIC, or PRIVATE, and _endxyz_, which handles ENDBLOCK, ENDBLOCKDATA,
      ENDDO, ENDIF, and so on) may expect arguments from ffest in the
      ffest-wide union ffest_args_, the substructure specific to the parser.

      A parser's responsibility is: to call either ffesta_confirmed or
      ffest_ffebad_start before returning to the final state; to be the only
      parser that can possibly call ffesta_confirmed for a given statement;
      to call ffest_ffebad_start immediately upon recognizing a bad token
      (specifically one that another statement parser might confirm upon);
      to call ffestc functions only after calling ffesta_confirmed and only
      when ffesta_is_inhibited returns FALSE; and to call ffesta_is_inhibited
      only after calling ffesta_confirmed.  Confirm as early as reasonably
      possible, even when only one ffestc function is called for the statement
      later on, because early confirmation can enhance the error-reporting
      capabilities if a subsequent error is detected and this parser isn't
      the first possibility for the statement.

      To assist the parser, functions like ffesta_ffebad_1t and _1p_ have
      been provided to make use of ffest_ffebad_start fairly easy.

   Modifications:
*/

/* Include files. */

#include "proj.h"
#include "stb.h"
#include "bad.h"
#include "expr.h"
#include "lex.h"
#include "malloc.h"
#include "src.h"
#include "sta.h"
#include "stc.h"
#include "stp.h"
#include "str.h"

/* Externals defined here. */

struct _ffestb_args_ ffestb_args;

/* Simple definitions and enumerations. */

#define FFESTB_KILL_EASY_ 1	/* 1 for only one _subr_kill_xyz_ fn. */

/* Internal typedefs. */

union ffestb_subrargs_u_
  {
    struct
      {
	ffesttTokenList labels;	/* Input arg, must not be NULL. */
	ffelexHandler handler;	/* Input arg, call me when done. */
	bool ok;		/* Output arg, TRUE if list ended in
				   CLOSE_PAREN. */
      }
    label_list;
    struct
      {
	ffesttDimList dims;	/* Input arg, must not be NULL. */
	ffelexHandler handler;	/* Input arg, call me when done. */
	mallocPool pool;	/* Pool to allocate into. */
	bool ok;		/* Output arg, TRUE if list ended in
				   CLOSE_PAREN. */
	ffeexprContext ctx;	/* DIMLIST or DIMLISTCOMMON. */
#ifdef FFECOM_dimensionsMAX
	int ndims;		/* For backends that really can't have
				   infinite dims. */
#endif
      }
    dim_list;
    struct
      {
	ffesttTokenList args;	/* Input arg, must not be NULL. */
	ffelexHandler handler;	/* Input arg, call me when done. */
	ffelexToken close_paren;/* Output arg if ok, CLOSE_PAREN token. */
	bool is_subr;		/* Input arg, TRUE if list in subr-def
				   context. */
	bool ok;		/* Output arg, TRUE if list ended in
				   CLOSE_PAREN. */
	bool names;		/* Do ffelex_set_names(TRUE) before return. */
      }
    name_list;
  };

union ffestb_local_u_
  {
    struct
      {
	ffebld expr;
      }
    call_stmt;
    struct
      {
	ffebld expr;
      }
    go_to;
    struct
      {
	ffebld dest;
	bool vxtparam;		/* If assignment might really be VXT
				   PARAMETER stmt. */
      }
    let;
    struct
      {
	ffebld expr;
      }
    if_stmt;
    struct
      {
	ffebld expr;
      }
    else_stmt;
    struct
      {
	ffebld expr;
      }
    dowhile;
    struct
      {
	ffebld var;
	ffebld start;
	ffebld end;
      }
    do_stmt;
    struct
      {
	bool is_cblock;
      }
    R522;
    struct
      {
	ffebld expr;
	bool started;
      }
    parameter;
    struct
      {
	ffesttExprList exprs;
	bool started;
      }
    equivalence;
    struct
      {
	ffebld expr;
	bool started;
      }
    data;
    struct
      {
	ffestrOther kw;
      }
    varlist;
#if FFESTR_F90
    struct
      {
	ffestrOther kw;
      }
    type;
#endif
    struct
      {
	ffelexHandler next;
      }
    construct;
    struct
      {
	ffesttFormatList f;
	ffestpFormatType current;	/* What we're currently working on. */
	ffelexToken t;		/* Token of what we're currently working on. */
	ffesttFormatValue pre;
	ffesttFormatValue post;
	ffesttFormatValue dot;
	ffesttFormatValue exp;
	bool sign;		/* _3_, pos/neg; elsewhere, signed/unsigned. */
	bool complained;	/* If run-time expr seen in nonexec context. */
      }
    format;
#if FFESTR_F90
    struct
      {
	bool started;
      }
    moduleprocedure;
#endif
    struct
      {
	ffebld expr;
      }
    selectcase;
    struct
      {
	ffesttCaseList cases;
      }
    case_stmt;
#if FFESTR_F90
    struct
      {
	ffesttExprList exprs;
	ffebld expr;
      }
    heap;
#endif
#if FFESTR_F90
    struct
      {
	ffesttExprList exprs;
      }
    R624;
#endif
#if FFESTR_F90
    struct
      {
	ffestpDefinedOperator operator;
	bool assignment;	/* TRUE for INTERFACE ASSIGNMENT, FALSE for
				   ...OPERATOR. */
	bool slash;		/* TRUE if OPEN_ARRAY, FALSE if OPEN_PAREN. */
      }
    interface;
#endif
    struct
      {
	bool is_cblock;
      }
    V014;
#if FFESTR_VXT
    struct
      {
	bool started;
	ffebld u;
	ffebld m;
	ffebld n;
	ffebld asv;
      }
    V025;
#endif
    struct
      {
	ffestpBeruIx ix;
	bool label;
	bool left;
	ffeexprContext context;
      }
    beru;
    struct
      {
	ffestpCloseIx ix;
	bool label;
	bool left;
	ffeexprContext context;
      }
    close;
    struct
      {
	ffestpDeleteIx ix;
	bool label;
	bool left;
	ffeexprContext context;
      }
    delete;
    struct
      {
	ffestpDeleteIx ix;
	bool label;
	bool left;
	ffeexprContext context;
      }
    find;
    struct
      {
	ffestpInquireIx ix;
	bool label;
	bool left;
	ffeexprContext context;
	bool may_be_iolength;
      }
    inquire;
    struct
      {
	ffestpOpenIx ix;
	bool label;
	bool left;
	ffeexprContext context;
      }
    open;
    struct
      {
	ffestpReadIx ix;
	bool label;
	bool left;
	ffeexprContext context;
      }
    read;
    struct
      {
	ffestpRewriteIx ix;
	bool label;
	bool left;
	ffeexprContext context;
      }
    rewrite;
    struct
      {
	ffestpWriteIx ix;
	bool label;
	bool left;
	ffeexprContext context;
      }
    vxtcode;
    struct
      {
	ffestpWriteIx ix;
	bool label;
	bool left;
	ffeexprContext context;
      }
    write;
#if FFESTR_F90
    struct
      {
	bool started;
      }
    structure;
#endif
    struct
      {
	bool started;
      }
    common;
    struct
      {
	bool started;
      }
    dimension;
    struct
      {
	bool started;
      }
    dimlist;
    struct
      {
	const char *badname;
	ffestrFirst first_kw;
	bool is_subr;
      }
    dummy;
    struct
      {
	ffebld kind;		/* Kind type parameter, if any. */
	ffelexToken kindt;	/* Kind type first token, if any. */
	ffebld len;		/* Length type parameter, if any. */
	ffelexToken lent;	/* Length type parameter, if any. */
	ffelexHandler handler;
	ffelexToken recursive;
	ffebld expr;
	ffesttTokenList toklist;/* For ambiguity resolution. */
	ffesttImpList imps;	/* List of IMPLICIT letters. */
	ffelexHandler imp_handler;	/* Call if paren list wasn't letters. */
	const char *badname;
	ffestrOther kw;		/* INTENT(IN/OUT/INOUT). */
	ffestpType type;
	bool parameter;		/* If PARAMETER attribute seen (governs =expr
				   context). */
	bool coloncolon;	/* If COLONCOLON seen (allows =expr). */
	bool aster_after;	/* "*" seen after, not before,
				   [RECURSIVE]FUNCTIONxyz. */
	bool empty;		/* Ambig function dummy arg list empty so
				   far? */
	bool imp_started;	/* Started IMPLICIT statement already. */
	bool imp_seen_comma;	/* TRUE if next COMMA within parens means not
				   R541. */
      }
    decl;
    struct
      {
	bool started;
      }
    vxtparam;
  };				/* Merge with the one in ffestb later. */

/* Private include files. */


/* Internal structure definitions. */


/* Static objects accessed by functions in this module. */

static union ffestb_subrargs_u_ ffestb_subrargs_;
static union ffestb_local_u_ ffestb_local_;

/* Static functions (internal). */

static void ffestb_subr_ambig_to_ents_ (void);
static ffelexHandler ffestb_subr_ambig_nope_ (ffelexToken t);
static ffelexHandler ffestb_subr_dimlist_ (ffelexToken ft, ffebld expr,
					   ffelexToken t);
static ffelexHandler ffestb_subr_dimlist_1_ (ffelexToken ft, ffebld expr,
					     ffelexToken t);
static ffelexHandler ffestb_subr_dimlist_2_ (ffelexToken ft, ffebld expr,
					     ffelexToken t);
static ffelexHandler ffestb_subr_name_list_ (ffelexToken t);
static ffelexHandler ffestb_subr_name_list_1_ (ffelexToken t);
static void ffestb_subr_R1001_append_p_ (void);
static ffelexHandler ffestb_decl_kindparam_ (ffelexToken t);
static ffelexHandler ffestb_decl_kindparam_1_ (ffelexToken t);
static ffelexHandler ffestb_decl_kindparam_2_ (ffelexToken ft, ffebld expr,
					       ffelexToken t);
static ffelexHandler ffestb_decl_starkind_ (ffelexToken t);
static ffelexHandler ffestb_decl_starlen_ (ffelexToken t);
static ffelexHandler ffestb_decl_starlen_1_ (ffelexToken ft, ffebld expr,
					     ffelexToken t);
static ffelexHandler ffestb_decl_typeparams_ (ffelexToken t);
static ffelexHandler ffestb_decl_typeparams_1_ (ffelexToken t);
static ffelexHandler ffestb_decl_typeparams_2_ (ffelexToken ft, ffebld expr,
						ffelexToken t);
static ffelexHandler ffestb_decl_typeparams_3_ (ffelexToken ft, ffebld expr,
						ffelexToken t);
#if FFESTR_F90
static ffelexHandler ffestb_decl_typetype1_ (ffelexToken t);
static ffelexHandler ffestb_decl_typetype2_ (ffelexToken t);
#endif
static ffelexHandler ffestb_subr_label_list_ (ffelexToken t);
static ffelexHandler ffestb_subr_label_list_1_ (ffelexToken t);
static ffelexHandler ffestb_do1_ (ffelexToken t);
static ffelexHandler ffestb_do2_ (ffelexToken t);
static ffelexHandler ffestb_do3_ (ffelexToken t);
static ffelexHandler ffestb_do4_ (ffelexToken ft, ffebld expr,
				  ffelexToken t);
static ffelexHandler ffestb_do5_ (ffelexToken t);
static ffelexHandler ffestb_do6_ (ffelexToken ft, ffebld expr,
				  ffelexToken t);
static ffelexHandler ffestb_do7_ (ffelexToken ft, ffebld expr,
				  ffelexToken t);
static ffelexHandler ffestb_do8_ (ffelexToken ft, ffebld expr,
				  ffelexToken t);
static ffelexHandler ffestb_do9_ (ffelexToken ft, ffebld expr,
				  ffelexToken t);
static ffelexHandler ffestb_else1_ (ffelexToken t);
static ffelexHandler ffestb_else2_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_else3_ (ffelexToken t);
static ffelexHandler ffestb_else4_ (ffelexToken t);
static ffelexHandler ffestb_else5_ (ffelexToken t);
static ffelexHandler ffestb_end1_ (ffelexToken t);
static ffelexHandler ffestb_end2_ (ffelexToken t);
static ffelexHandler ffestb_end3_ (ffelexToken t);
static ffelexHandler ffestb_goto1_ (ffelexToken t);
static ffelexHandler ffestb_goto2_ (ffelexToken t);
static ffelexHandler ffestb_goto3_ (ffelexToken t);
static ffelexHandler ffestb_goto4_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_goto5_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_goto6_ (ffelexToken t);
static ffelexHandler ffestb_goto7_ (ffelexToken t);
static ffelexHandler ffestb_halt1_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_if1_ (ffelexToken ft, ffebld expr,
				  ffelexToken t);
static ffelexHandler ffestb_if2_ (ffelexToken t);
static ffelexHandler ffestb_if3_ (ffelexToken t);
static ffelexHandler ffestb_let1_ (ffelexToken ft, ffebld expr,
				   ffelexToken t);
static ffelexHandler ffestb_let2_ (ffelexToken ft, ffebld expr,
				   ffelexToken t);
#if FFESTR_F90
static ffelexHandler ffestb_type1_ (ffelexToken t);
static ffelexHandler ffestb_type2_ (ffelexToken t);
static ffelexHandler ffestb_type3_ (ffelexToken t);
static ffelexHandler ffestb_type4_ (ffelexToken t);
#endif
#if FFESTR_F90
static ffelexHandler ffestb_varlist1_ (ffelexToken t);
static ffelexHandler ffestb_varlist2_ (ffelexToken t);
static ffelexHandler ffestb_varlist3_ (ffelexToken t);
static ffelexHandler ffestb_varlist4_ (ffelexToken t);
#endif
static ffelexHandler ffestb_varlist5_ (ffelexToken t);
static ffelexHandler ffestb_varlist6_ (ffelexToken t);
#if FFESTR_F90
static ffelexHandler ffestb_where1_ (ffelexToken ft, ffebld expr,
				     ffelexToken t);
static ffelexHandler ffestb_where2_ (ffelexToken t);
static ffelexHandler ffestb_where3_ (ffelexToken t);
#endif
static ffelexHandler ffestb_R5221_ (ffelexToken t);
static ffelexHandler ffestb_R5222_ (ffelexToken t);
static ffelexHandler ffestb_R5223_ (ffelexToken t);
static ffelexHandler ffestb_R5224_ (ffelexToken t);
static ffelexHandler ffestb_R5281_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_R5282_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_R5283_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_R5284_ (ffelexToken t);
static ffelexHandler ffestb_R5371_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_R5372_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_R5373_ (ffelexToken t);
static ffelexHandler ffestb_R5421_ (ffelexToken t);
static ffelexHandler ffestb_R5422_ (ffelexToken t);
static ffelexHandler ffestb_R5423_ (ffelexToken t);
static ffelexHandler ffestb_R5424_ (ffelexToken t);
static ffelexHandler ffestb_R5425_ (ffelexToken t);
static ffelexHandler ffestb_R5441_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_R5442_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_R5443_ (ffelexToken t);
static ffelexHandler ffestb_R5444_ (ffelexToken t);
static ffelexHandler ffestb_R8341_ (ffelexToken t);
static ffelexHandler ffestb_R8351_ (ffelexToken t);
static ffelexHandler ffestb_R8381_ (ffelexToken t);
static ffelexHandler ffestb_R8382_ (ffelexToken t);
static ffelexHandler ffestb_R8383_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_R8401_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_R8402_ (ffelexToken t);
static ffelexHandler ffestb_R8403_ (ffelexToken t);
static ffelexHandler ffestb_R8404_ (ffelexToken t);
static ffelexHandler ffestb_R8405_ (ffelexToken t);
static ffelexHandler ffestb_R8406_ (ffelexToken t);
static ffelexHandler ffestb_R8407_ (ffelexToken t);
static ffelexHandler ffestb_R11021_ (ffelexToken t);
static ffelexHandler ffestb_R1111_1_ (ffelexToken t);
static ffelexHandler ffestb_R1111_2_ (ffelexToken t);
static ffelexHandler ffestb_R12121_ (ffelexToken ft, ffebld expr,
				     ffelexToken t);
static ffelexHandler ffestb_R12271_ (ffelexToken ft, ffebld expr,
				     ffelexToken t);
static ffelexHandler ffestb_construct1_ (ffelexToken t);
static ffelexHandler ffestb_construct2_ (ffelexToken t);
#if FFESTR_F90
static ffelexHandler ffestb_heap1_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_heap2_ (ffelexToken t);
static ffelexHandler ffestb_heap3_ (ffelexToken t);
static ffelexHandler ffestb_heap4_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_heap5_ (ffelexToken t);
#endif
#if FFESTR_F90
static ffelexHandler ffestb_module1_ (ffelexToken t);
static ffelexHandler ffestb_module2_ (ffelexToken t);
static ffelexHandler ffestb_module3_ (ffelexToken t);
#endif
static ffelexHandler ffestb_R8091_ (ffelexToken t);
static ffelexHandler ffestb_R8092_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_R8093_ (ffelexToken t);
static ffelexHandler ffestb_R8101_ (ffelexToken t);
static ffelexHandler ffestb_R8102_ (ffelexToken t);
static ffelexHandler ffestb_R8103_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_R8104_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_R10011_ (ffelexToken t);
static ffelexHandler ffestb_R10012_ (ffelexToken t);
static ffelexHandler ffestb_R10013_ (ffelexToken t);
static ffelexHandler ffestb_R10014_ (ffelexToken t);
static ffelexHandler ffestb_R10015_ (ffelexToken t);
static ffelexHandler ffestb_R10016_ (ffelexToken t);
static ffelexHandler ffestb_R10017_ (ffelexToken t);
static ffelexHandler ffestb_R10018_ (ffelexToken t);
static ffelexHandler ffestb_R10019_ (ffelexToken t);
static ffelexHandler ffestb_R100110_ (ffelexToken t);
static ffelexHandler ffestb_R100111_ (ffelexToken t);
static ffelexHandler ffestb_R100112_ (ffelexToken t);
static ffelexHandler ffestb_R100113_ (ffelexToken t);
static ffelexHandler ffestb_R100114_ (ffelexToken t);
static ffelexHandler ffestb_R100115_ (ffelexToken ft, ffebld expr,
				      ffelexToken t);
static ffelexHandler ffestb_R100116_ (ffelexToken ft, ffebld expr,
				      ffelexToken t);
static ffelexHandler ffestb_R100117_ (ffelexToken ft, ffebld expr,
				      ffelexToken t);
static ffelexHandler ffestb_R100118_ (ffelexToken ft, ffebld expr,
				      ffelexToken t);
#if FFESTR_F90
static ffelexHandler ffestb_R11071_ (ffelexToken t);
static ffelexHandler ffestb_R11072_ (ffelexToken t);
static ffelexHandler ffestb_R11073_ (ffelexToken t);
static ffelexHandler ffestb_R11074_ (ffelexToken t);
static ffelexHandler ffestb_R11075_ (ffelexToken t);
static ffelexHandler ffestb_R11076_ (ffelexToken t);
static ffelexHandler ffestb_R11077_ (ffelexToken t);
static ffelexHandler ffestb_R11078_ (ffelexToken t);
static ffelexHandler ffestb_R11079_ (ffelexToken t);
static ffelexHandler ffestb_R110710_ (ffelexToken t);
static ffelexHandler ffestb_R110711_ (ffelexToken t);
static ffelexHandler ffestb_R110712_ (ffelexToken t);
#endif
#if FFESTR_F90
static ffelexHandler ffestb_R12021_ (ffelexToken t);
static ffelexHandler ffestb_R12022_ (ffelexToken t);
static ffelexHandler ffestb_R12023_ (ffelexToken t);
static ffelexHandler ffestb_R12024_ (ffelexToken t);
static ffelexHandler ffestb_R12025_ (ffelexToken t);
static ffelexHandler ffestb_R12026_ (ffelexToken t);
#endif
static ffelexHandler ffestb_S3P41_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_V0141_ (ffelexToken t);
static ffelexHandler ffestb_V0142_ (ffelexToken t);
static ffelexHandler ffestb_V0143_ (ffelexToken t);
static ffelexHandler ffestb_V0144_ (ffelexToken t);
#if FFESTR_VXT
static ffelexHandler ffestb_V0251_ (ffelexToken t);
static ffelexHandler ffestb_V0252_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_V0253_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_V0254_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_V0255_ (ffelexToken t);
static ffelexHandler ffestb_V0256_ (ffelexToken t);
static ffelexHandler ffestb_V0257_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_V0258_ (ffelexToken t);
#endif
#if FFESTB_KILL_EASY_
static void ffestb_subr_kill_easy_ (ffestpInquireIx max);
#else
static void ffestb_subr_kill_accept_ (void);
static void ffestb_subr_kill_beru_ (void);
static void ffestb_subr_kill_close_ (void);
static void ffestb_subr_kill_delete_ (void);
static void ffestb_subr_kill_find_ (void);	/* Not written yet. */
static void ffestb_subr_kill_inquire_ (void);
static void ffestb_subr_kill_open_ (void);
static void ffestb_subr_kill_print_ (void);
static void ffestb_subr_kill_read_ (void);
static void ffestb_subr_kill_rewrite_ (void);
static void ffestb_subr_kill_type_ (void);
static void ffestb_subr_kill_vxtcode_ (void);	/* Not written yet. */
static void ffestb_subr_kill_write_ (void);
#endif
static ffelexHandler ffestb_beru1_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_beru2_ (ffelexToken t);
static ffelexHandler ffestb_beru3_ (ffelexToken t);
static ffelexHandler ffestb_beru4_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_beru5_ (ffelexToken t);
static ffelexHandler ffestb_beru6_ (ffelexToken t);
static ffelexHandler ffestb_beru7_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_beru8_ (ffelexToken t);
static ffelexHandler ffestb_beru9_ (ffelexToken t);
static ffelexHandler ffestb_beru10_ (ffelexToken t);
#if FFESTR_VXT
static ffelexHandler ffestb_vxtcode1_ (ffelexToken ft, ffebld expr,
				       ffelexToken t);
static ffelexHandler ffestb_vxtcode2_ (ffelexToken ft, ffebld expr,
				       ffelexToken t);
static ffelexHandler ffestb_vxtcode3_ (ffelexToken ft, ffebld expr,
				       ffelexToken t);
static ffelexHandler ffestb_vxtcode4_ (ffelexToken t);
static ffelexHandler ffestb_vxtcode5_ (ffelexToken t);
static ffelexHandler ffestb_vxtcode6_ (ffelexToken ft, ffebld expr,
				       ffelexToken t);
static ffelexHandler ffestb_vxtcode7_ (ffelexToken t);
static ffelexHandler ffestb_vxtcode8_ (ffelexToken t);
static ffelexHandler ffestb_vxtcode9_ (ffelexToken t);
static ffelexHandler ffestb_vxtcode10_ (ffelexToken ft, ffebld expr,
					ffelexToken t);
#endif
static ffelexHandler ffestb_R9041_ (ffelexToken t);
static ffelexHandler ffestb_R9042_ (ffelexToken t);
static ffelexHandler ffestb_R9043_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_R9044_ (ffelexToken t);
static ffelexHandler ffestb_R9045_ (ffelexToken t);
static ffelexHandler ffestb_R9046_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_R9047_ (ffelexToken t);
static ffelexHandler ffestb_R9048_ (ffelexToken t);
static ffelexHandler ffestb_R9049_ (ffelexToken t);
static ffelexHandler ffestb_R9071_ (ffelexToken t);
static ffelexHandler ffestb_R9072_ (ffelexToken t);
static ffelexHandler ffestb_R9073_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_R9074_ (ffelexToken t);
static ffelexHandler ffestb_R9075_ (ffelexToken t);
static ffelexHandler ffestb_R9076_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_R9077_ (ffelexToken t);
static ffelexHandler ffestb_R9078_ (ffelexToken t);
static ffelexHandler ffestb_R9079_ (ffelexToken t);
static ffelexHandler ffestb_R9091_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_R9092_ (ffelexToken t);
static ffelexHandler ffestb_R9093_ (ffelexToken t);
static ffelexHandler ffestb_R9094_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_R9095_ (ffelexToken t);
static ffelexHandler ffestb_R9096_ (ffelexToken t);
static ffelexHandler ffestb_R9097_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_R9098_ (ffelexToken t);
static ffelexHandler ffestb_R9099_ (ffelexToken t);
static ffelexHandler ffestb_R90910_ (ffelexToken ft, ffebld expr,
				     ffelexToken t);
static ffelexHandler ffestb_R90911_ (ffelexToken t);
static ffelexHandler ffestb_R90912_ (ffelexToken t);
static ffelexHandler ffestb_R90913_ (ffelexToken t);
static ffelexHandler ffestb_R90914_ (ffelexToken ft, ffebld expr,
				     ffelexToken t);
static ffelexHandler ffestb_R90915_ (ffelexToken ft, ffebld expr,
				     ffelexToken t);
static ffelexHandler ffestb_R9101_ (ffelexToken t);
static ffelexHandler ffestb_R9102_ (ffelexToken t);
static ffelexHandler ffestb_R9103_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_R9104_ (ffelexToken t);
static ffelexHandler ffestb_R9105_ (ffelexToken t);
static ffelexHandler ffestb_R9106_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_R9107_ (ffelexToken t);
static ffelexHandler ffestb_R9108_ (ffelexToken t);
static ffelexHandler ffestb_R9109_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_R91010_ (ffelexToken t);
static ffelexHandler ffestb_R91011_ (ffelexToken t);
static ffelexHandler ffestb_R91012_ (ffelexToken t);
static ffelexHandler ffestb_R91013_ (ffelexToken ft, ffebld expr,
				     ffelexToken t);
static ffelexHandler ffestb_R91014_ (ffelexToken ft, ffebld expr,
				     ffelexToken t);
static ffelexHandler ffestb_R9111_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_R9112_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_R9231_ (ffelexToken t);
static ffelexHandler ffestb_R9232_ (ffelexToken t);
static ffelexHandler ffestb_R9233_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_R9234_ (ffelexToken t);
static ffelexHandler ffestb_R9235_ (ffelexToken t);
static ffelexHandler ffestb_R9236_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_R9237_ (ffelexToken t);
static ffelexHandler ffestb_R9238_ (ffelexToken t);
static ffelexHandler ffestb_R9239_ (ffelexToken t);
static ffelexHandler ffestb_R92310_ (ffelexToken t);
static ffelexHandler ffestb_R92311_ (ffelexToken ft, ffebld expr,
				     ffelexToken t);
#if FFESTR_VXT
static ffelexHandler ffestb_V0181_ (ffelexToken t);
static ffelexHandler ffestb_V0182_ (ffelexToken t);
static ffelexHandler ffestb_V0183_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_V0184_ (ffelexToken t);
static ffelexHandler ffestb_V0185_ (ffelexToken t);
static ffelexHandler ffestb_V0186_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_V0187_ (ffelexToken t);
static ffelexHandler ffestb_V0188_ (ffelexToken t);
static ffelexHandler ffestb_V0189_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_V01810_ (ffelexToken t);
static ffelexHandler ffestb_V01811_ (ffelexToken t);
static ffelexHandler ffestb_V01812_ (ffelexToken t);
static ffelexHandler ffestb_V01813_ (ffelexToken ft, ffebld expr,
				     ffelexToken t);
static ffelexHandler ffestb_V0191_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_V0192_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
#endif
static ffelexHandler ffestb_V0201_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_V0202_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
#if FFESTR_VXT
static ffelexHandler ffestb_V0211_ (ffelexToken t);
static ffelexHandler ffestb_V0212_ (ffelexToken t);
static ffelexHandler ffestb_V0213_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_V0214_ (ffelexToken t);
static ffelexHandler ffestb_V0215_ (ffelexToken t);
static ffelexHandler ffestb_V0216_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_V0217_ (ffelexToken t);
static ffelexHandler ffestb_V0218_ (ffelexToken t);
static ffelexHandler ffestb_V0219_ (ffelexToken t);
static ffelexHandler ffestb_V0261_ (ffelexToken t);
static ffelexHandler ffestb_V0262_ (ffelexToken t);
static ffelexHandler ffestb_V0263_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_V0264_ (ffelexToken t);
static ffelexHandler ffestb_V0265_ (ffelexToken t);
static ffelexHandler ffestb_V0266_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_V0267_ (ffelexToken t);
static ffelexHandler ffestb_V0268_ (ffelexToken t);
static ffelexHandler ffestb_V0269_ (ffelexToken t);
#endif
#if FFESTR_F90
static ffelexHandler ffestb_dimlist1_ (ffelexToken t);
static ffelexHandler ffestb_dimlist2_ (ffelexToken t);
static ffelexHandler ffestb_dimlist3_ (ffelexToken t);
static ffelexHandler ffestb_dimlist4_ (ffelexToken t);
#endif
static ffelexHandler ffestb_dummy1_ (ffelexToken t);
static ffelexHandler ffestb_dummy2_ (ffelexToken t);
static ffelexHandler ffestb_R5241_ (ffelexToken t);
static ffelexHandler ffestb_R5242_ (ffelexToken t);
static ffelexHandler ffestb_R5243_ (ffelexToken t);
static ffelexHandler ffestb_R5244_ (ffelexToken t);
static ffelexHandler ffestb_R5471_ (ffelexToken t);
static ffelexHandler ffestb_R5472_ (ffelexToken t);
static ffelexHandler ffestb_R5473_ (ffelexToken t);
static ffelexHandler ffestb_R5474_ (ffelexToken t);
static ffelexHandler ffestb_R5475_ (ffelexToken t);
static ffelexHandler ffestb_R5476_ (ffelexToken t);
static ffelexHandler ffestb_R5477_ (ffelexToken t);
#if FFESTR_F90
static ffelexHandler ffestb_R6241_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_R6242_ (ffelexToken t);
#endif
static ffelexHandler ffestb_R12291_ (ffelexToken t);
static ffelexHandler ffestb_R12292_ (ffelexToken ft, ffebld expr,
				     ffelexToken t);
static ffelexHandler ffestb_decl_chartype1_ (ffelexToken t);
#if FFESTR_F90
static ffelexHandler ffestb_decl_recursive1_ (ffelexToken t);
static ffelexHandler ffestb_decl_recursive2_ (ffelexToken t);
static ffelexHandler ffestb_decl_recursive3_ (ffelexToken t);
static ffelexHandler ffestb_decl_recursive4_ (ffelexToken t);
#endif
static ffelexHandler ffestb_decl_attrs_ (ffelexToken t);
static ffelexHandler ffestb_decl_attrs_1_ (ffelexToken t);
static ffelexHandler ffestb_decl_attrs_2_ (ffelexToken t);
#if FFESTR_F90
static ffelexHandler ffestb_decl_attrs_3_ (ffelexToken t);
static ffelexHandler ffestb_decl_attrs_4_ (ffelexToken t);
static ffelexHandler ffestb_decl_attrs_5_ (ffelexToken t);
static ffelexHandler ffestb_decl_attrs_6_ (ffelexToken t);
#endif
static ffelexHandler ffestb_decl_attrs_7_ (ffelexToken t);
static ffelexHandler ffestb_decl_attrsp_ (ffelexToken t);
static ffelexHandler ffestb_decl_ents_ (ffelexToken t);
static ffelexHandler ffestb_decl_ents_1_ (ffelexToken t);
static ffelexHandler ffestb_decl_ents_2_ (ffelexToken t);
static ffelexHandler ffestb_decl_ents_3_ (ffelexToken t);
static ffelexHandler ffestb_decl_ents_4_ (ffelexToken t);
static ffelexHandler ffestb_decl_ents_5_ (ffelexToken t);
static ffelexHandler ffestb_decl_ents_6_ (ffelexToken ft, ffebld expr,
					  ffelexToken t);
static ffelexHandler ffestb_decl_ents_7_ (ffelexToken t);
static ffelexHandler ffestb_decl_ents_8_ (ffelexToken ft, ffebld expr,
					  ffelexToken t);
static ffelexHandler ffestb_decl_ents_9_ (ffelexToken ft, ffebld expr,
					  ffelexToken t);
static ffelexHandler ffestb_decl_ents_10_ (ffelexToken ft, ffebld expr,
					   ffelexToken t);
static ffelexHandler ffestb_decl_ents_11_ (ffelexToken t);
static ffelexHandler ffestb_decl_entsp_ (ffelexToken t);
static ffelexHandler ffestb_decl_entsp_1_ (ffelexToken t);
static ffelexHandler ffestb_decl_entsp_2_ (ffelexToken t);
static ffelexHandler ffestb_decl_entsp_3_ (ffelexToken t);
static ffelexHandler ffestb_decl_entsp_4_ (ffelexToken ft, ffebld expr,
					   ffelexToken t);
static ffelexHandler ffestb_decl_entsp_5_ (ffelexToken t);
static ffelexHandler ffestb_decl_entsp_6_ (ffelexToken t);
static ffelexHandler ffestb_decl_entsp_7_ (ffelexToken t);
static ffelexHandler ffestb_decl_entsp_8_ (ffelexToken t);
#if FFESTR_F90
static ffelexHandler ffestb_decl_func_ (ffelexToken t);
#endif
static ffelexHandler ffestb_decl_funcname_ (ffelexToken t);
static ffelexHandler ffestb_decl_funcname_1_ (ffelexToken t);
static ffelexHandler ffestb_decl_funcname_2_ (ffelexToken t);
static ffelexHandler ffestb_decl_funcname_3_ (ffelexToken ft, ffebld expr,
					      ffelexToken t);
static ffelexHandler ffestb_decl_funcname_4_ (ffelexToken t);
static ffelexHandler ffestb_decl_funcname_5_ (ffelexToken t);
static ffelexHandler ffestb_decl_funcname_6_ (ffelexToken t);
static ffelexHandler ffestb_decl_funcname_7_ (ffelexToken t);
static ffelexHandler ffestb_decl_funcname_8_ (ffelexToken t);
static ffelexHandler ffestb_decl_funcname_9_ (ffelexToken t);
#if FFESTR_VXT
static ffelexHandler ffestb_V0031_ (ffelexToken t);
static ffelexHandler ffestb_V0032_ (ffelexToken t);
static ffelexHandler ffestb_V0033_ (ffelexToken t);
static ffelexHandler ffestb_V0034_ (ffelexToken t);
static ffelexHandler ffestb_V0035_ (ffelexToken t);
static ffelexHandler ffestb_V0036_ (ffelexToken t);
static ffelexHandler ffestb_V0161_ (ffelexToken t);
static ffelexHandler ffestb_V0162_ (ffelexToken t);
static ffelexHandler ffestb_V0163_ (ffelexToken t);
static ffelexHandler ffestb_V0164_ (ffelexToken t);
static ffelexHandler ffestb_V0165_ (ffelexToken t);
static ffelexHandler ffestb_V0166_ (ffelexToken t);
#endif
static ffelexHandler ffestb_V0271_ (ffelexToken t);
static ffelexHandler ffestb_V0272_ (ffelexToken ft, ffebld expr,
				    ffelexToken t);
static ffelexHandler ffestb_V0273_ (ffelexToken t);
static ffelexHandler ffestb_decl_R5391_ (ffelexToken t);
static ffelexHandler ffestb_decl_R5392_ (ffelexToken t);
#if FFESTR_F90
static ffelexHandler ffestb_decl_R5393_ (ffelexToken t);
#endif
static ffelexHandler ffestb_decl_R5394_ (ffelexToken t);
static ffelexHandler ffestb_decl_R5395_ (ffelexToken t);
static ffelexHandler ffestb_decl_R539letters_ (ffelexToken t);
static ffelexHandler ffestb_decl_R539letters_1_ (ffelexToken t);
static ffelexHandler ffestb_decl_R539letters_2_ (ffelexToken t);
static ffelexHandler ffestb_decl_R539letters_3_ (ffelexToken t);
static ffelexHandler ffestb_decl_R539letters_4_ (ffelexToken t);
static ffelexHandler ffestb_decl_R539letters_5_ (ffelexToken t);
static ffelexHandler ffestb_decl_R539maybe_ (ffelexToken t);
static ffelexHandler ffestb_decl_R539maybe_1_ (ffelexToken t);
static ffelexHandler ffestb_decl_R539maybe_2_ (ffelexToken t);
static ffelexHandler ffestb_decl_R539maybe_3_ (ffelexToken t);
static ffelexHandler ffestb_decl_R539maybe_4_ (ffelexToken t);
static ffelexHandler ffestb_decl_R539maybe_5_ (ffelexToken t);

/* Internal macros. */

#if FFESTB_KILL_EASY_
#define ffestb_subr_kill_accept_() \
      ffestb_subr_kill_easy_((ffestpInquireIx) FFESTP_acceptix)
#define ffestb_subr_kill_beru_() \
      ffestb_subr_kill_easy_((ffestpInquireIx) FFESTP_beruix)
#define ffestb_subr_kill_close_() \
      ffestb_subr_kill_easy_((ffestpInquireIx) FFESTP_closeix)
#define ffestb_subr_kill_delete_() \
      ffestb_subr_kill_easy_((ffestpInquireIx) FFESTP_deleteix)
#define ffestb_subr_kill_find_() \
      ffestb_subr_kill_easy_((ffestpInquireIx) FFESTP_findix)
#define ffestb_subr_kill_inquire_() \
      ffestb_subr_kill_easy_((ffestpInquireIx) FFESTP_inquireix)
#define ffestb_subr_kill_open_() \
      ffestb_subr_kill_easy_((ffestpInquireIx) FFESTP_openix)
#define ffestb_subr_kill_print_() \
      ffestb_subr_kill_easy_((ffestpInquireIx) FFESTP_printix)
#define ffestb_subr_kill_read_() \
      ffestb_subr_kill_easy_((ffestpInquireIx) FFESTP_readix)
#define ffestb_subr_kill_rewrite_() \
      ffestb_subr_kill_easy_((ffestpInquireIx) FFESTP_rewriteix)
#define ffestb_subr_kill_type_() \
      ffestb_subr_kill_easy_((ffestpInquireIx) FFESTP_typeix)
#define ffestb_subr_kill_vxtcode_() \
      ffestb_subr_kill_easy_((ffestpInquireIx) FFESTP_vxtcodeix)
#define ffestb_subr_kill_write_() \
      ffestb_subr_kill_easy_((ffestpInquireIx) FFESTP_writeix)
#endif

/* ffestb_subr_ambig_nope_ -- Cleans up and aborts ambig w/o confirming

   ffestb_subr_ambig_nope_();

   Switch from ambiguity handling in _entsp_ functions to handling entities
   in _ents_ (perform housekeeping tasks).  */

static ffelexHandler
ffestb_subr_ambig_nope_ (ffelexToken t)
{
  if (ffestb_local_.decl.recursive != NULL)
    ffelex_token_kill (ffestb_local_.decl.recursive);
  if (ffestb_local_.decl.kindt != NULL)
    ffelex_token_kill (ffestb_local_.decl.kindt);
  if (ffestb_local_.decl.lent != NULL)
    ffelex_token_kill (ffestb_local_.decl.lent);
  ffelex_token_kill (ffesta_tokens[1]);
  ffelex_token_kill (ffesta_tokens[2]);
  ffestt_tokenlist_kill (ffestb_local_.decl.toklist);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FUNCTION", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_subr_ambig_to_ents_ -- Switches from ambiguity to entity decl

   ffestb_subr_ambig_to_ents_();

   Switch from ambiguity handling in _entsp_ functions to handling entities
   in _ents_ (perform housekeeping tasks).  */

static void
ffestb_subr_ambig_to_ents_ ()
{
  ffelexToken nt;

  nt = ffelex_token_name_from_names (ffesta_tokens[1], 0, 0);
  ffelex_token_kill (ffesta_tokens[1]);
  ffelex_token_kill (ffesta_tokens[2]);
  ffesta_tokens[1] = nt;
  if (ffestb_local_.decl.recursive != NULL)
    ffelex_token_kill (ffestb_local_.decl.recursive);
  if (!ffestb_local_.decl.aster_after)
    {
      if (ffestb_local_.decl.type == FFESTP_typeCHARACTER)
	{
	  if (!ffesta_is_inhibited ())
	    ffestc_decl_start (ffestb_local_.decl.type, ffesta_tokens[0],
			  ffestb_local_.decl.kind, ffestb_local_.decl.kindt,
			   ffestb_local_.decl.len, ffestb_local_.decl.lent);
	  if (ffestb_local_.decl.kindt != NULL)
	    {
	      ffelex_token_kill (ffestb_local_.decl.kindt);
	      ffestb_local_.decl.kind = NULL;
	      ffestb_local_.decl.kindt = NULL;
	    }
	  if (ffestb_local_.decl.lent != NULL)
	    {
	      ffelex_token_kill (ffestb_local_.decl.lent);
	      ffestb_local_.decl.len = NULL;
	      ffestb_local_.decl.lent = NULL;
	    }
	}
      else
	{
	  if (!ffesta_is_inhibited ())
	    ffestc_decl_start (ffestb_local_.decl.type, ffesta_tokens[0],
		    ffestb_local_.decl.kind, ffestb_local_.decl.kindt, NULL,
			       NULL);
	  if (ffestb_local_.decl.kindt != NULL)
	    {
	      ffelex_token_kill (ffestb_local_.decl.kindt);
	      ffestb_local_.decl.kind = NULL;
	      ffestb_local_.decl.kindt = NULL;
	    }
	}
      return;
    }
  if (ffestb_local_.decl.type == FFESTP_typeCHARACTER)
    {
      if (!ffesta_is_inhibited ())
	ffestc_decl_start (ffestb_local_.decl.type, ffesta_tokens[0],
	     ffestb_local_.decl.kind, ffestb_local_.decl.kindt, NULL, NULL);
      if (ffestb_local_.decl.kindt != NULL)
	{
	  ffelex_token_kill (ffestb_local_.decl.kindt);
	  ffestb_local_.decl.kind = NULL;
	  ffestb_local_.decl.kindt = NULL;
	}
    }
  else if (!ffesta_is_inhibited ())
    ffestc_decl_start (ffestb_local_.decl.type, ffesta_tokens[0],
		       NULL, NULL, NULL, NULL);
  /* NAME/NAMES token already in ffesta_tokens[1]. */
}

/* ffestb_subr_dimlist_ -- OPEN_PAREN expr

   (ffestb_subr_dimlist_)  // to expression handler

   Deal with a dimension list.

   19-Dec-90  JCB  1.1
      Detect too many dimensions if backend wants it.  */

static ffelexHandler
ffestb_subr_dimlist_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCLOSE_PAREN:
      if (expr == NULL)
	break;
#ifdef FFECOM_dimensionsMAX
      if (ffestb_subrargs_.dim_list.ndims++ == FFECOM_dimensionsMAX)
	{
	  ffesta_ffebad_1t (FFEBAD_TOO_MANY_DIMS, ft);
	  ffestb_subrargs_.dim_list.ok = TRUE;	/* Not a parse error, really. */
	  return (ffelexHandler) ffestb_subrargs_.dim_list.handler;
	}
#endif
      ffestt_dimlist_append (ffestb_subrargs_.dim_list.dims, NULL, expr,
			     ffelex_token_use (t));
      ffestb_subrargs_.dim_list.ok = TRUE;
      return (ffelexHandler) ffestb_subrargs_.dim_list.handler;

    case FFELEX_typeCOMMA:
      if ((expr != NULL) && (ffebld_op (expr) == FFEBLD_opSTAR))
	break;
#ifdef FFECOM_dimensionsMAX
      if (ffestb_subrargs_.dim_list.ndims++ == FFECOM_dimensionsMAX)
	{
	  ffesta_ffebad_1t (FFEBAD_TOO_MANY_DIMS, ft);
	  return (ffelexHandler) ffeexpr_rhs (ffestb_subrargs_.dim_list.pool,
					      ffestb_subrargs_.dim_list.ctx,
				  (ffeexprCallback) ffestb_subr_dimlist_2_);
	}
#endif
      ffestt_dimlist_append (ffestb_subrargs_.dim_list.dims, NULL, expr,
			     ffelex_token_use (t));
      return (ffelexHandler) ffeexpr_rhs (ffestb_subrargs_.dim_list.pool,
					  ffestb_subrargs_.dim_list.ctx,
				    (ffeexprCallback) ffestb_subr_dimlist_);

    case FFELEX_typeCOLON:
      if ((expr != NULL) && (ffebld_op (expr) == FFEBLD_opSTAR))
	break;
#ifdef FFECOM_dimensionsMAX
      if (ffestb_subrargs_.dim_list.ndims++ == FFECOM_dimensionsMAX)
	{
	  ffesta_ffebad_1t (FFEBAD_TOO_MANY_DIMS, ft);
	  return (ffelexHandler) ffeexpr_rhs (ffestb_subrargs_.dim_list.pool,
					      ffestb_subrargs_.dim_list.ctx,
				  (ffeexprCallback) ffestb_subr_dimlist_2_);
	}
#endif
      ffestt_dimlist_append (ffestb_subrargs_.dim_list.dims, expr, NULL,
			     ffelex_token_use (t));	/* NULL second expr for
							   now, just plug in. */
      return (ffelexHandler) ffeexpr_rhs (ffestb_subrargs_.dim_list.pool,
					  ffestb_subrargs_.dim_list.ctx,
				  (ffeexprCallback) ffestb_subr_dimlist_1_);

    default:
      break;
    }

  ffestb_subrargs_.dim_list.ok = FALSE;
  return (ffelexHandler) ffestb_subrargs_.dim_list.handler (t);
}

/* ffestb_subr_dimlist_1_ -- OPEN_PAREN expr COLON expr

   (ffestb_subr_dimlist_1_)  // to expression handler

   Get the upper bound.	 */

static ffelexHandler
ffestb_subr_dimlist_1_ (ffelexToken ft UNUSED, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCLOSE_PAREN:
      ffestb_subrargs_.dim_list.dims->previous->upper = expr;
      ffestb_subrargs_.dim_list.ok = TRUE;
      return (ffelexHandler) ffestb_subrargs_.dim_list.handler;

    case FFELEX_typeCOMMA:
      if ((expr != NULL) && (ffebld_op (expr) == FFEBLD_opSTAR))
	break;
      ffestb_subrargs_.dim_list.dims->previous->upper = expr;
      return (ffelexHandler) ffeexpr_rhs (ffestb_subrargs_.dim_list.pool,
      ffestb_subrargs_.dim_list.ctx, (ffeexprCallback) ffestb_subr_dimlist_);

    default:
      break;
    }

  ffestb_subrargs_.dim_list.ok = FALSE;
  return (ffelexHandler) ffestb_subrargs_.dim_list.handler (t);
}

/* ffestb_subr_dimlist_2_ -- OPEN_PAREN too-many-dim-exprs

   (ffestb_subr_dimlist_2_)  // to expression handler

   Get the upper bound.	 */

static ffelexHandler
ffestb_subr_dimlist_2_ (ffelexToken ft UNUSED, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCLOSE_PAREN:
      ffestb_subrargs_.dim_list.ok = TRUE;	/* Not a parse error, really. */
      return (ffelexHandler) ffestb_subrargs_.dim_list.handler;

    case FFELEX_typeCOMMA:
    case FFELEX_typeCOLON:
      if ((expr != NULL) && (ffebld_op (expr) == FFEBLD_opSTAR))
	break;
      return (ffelexHandler) ffeexpr_rhs (ffestb_subrargs_.dim_list.pool,
					  ffestb_subrargs_.dim_list.ctx,
				  (ffeexprCallback) ffestb_subr_dimlist_2_);

    default:
      break;
    }

  ffestb_subrargs_.dim_list.ok = FALSE;
  return (ffelexHandler) ffestb_subrargs_.dim_list.handler (t);
}

/* ffestb_subr_name_list_ -- Collect a list of name args and close-paren

   return ffestb_subr_name_list_;  // to lexer after seeing OPEN_PAREN

   This implements R1224 in the Fortran 90 spec.  The arg list may be
   empty, or be a comma-separated list (an optional trailing comma currently
   results in a warning but no other effect) of arguments.  For functions,
   however, "*" is invalid (we implement dummy-arg-name, rather than R1224
   dummy-arg, which itself is either dummy-arg-name or "*").  */

static ffelexHandler
ffestb_subr_name_list_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCLOSE_PAREN:
      if (ffestt_tokenlist_count (ffestb_subrargs_.name_list.args) != 0)
	{			/* Trailing comma, warn. */
	  ffebad_start (FFEBAD_TRAILING_COMMA);
	  ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_finish ();
	}
      ffestb_subrargs_.name_list.ok = TRUE;
      ffestb_subrargs_.name_list.close_paren = ffelex_token_use (t);
      if (ffestb_subrargs_.name_list.names)
	ffelex_set_names (TRUE);
      return (ffelexHandler) ffestb_subrargs_.name_list.handler;

    case FFELEX_typeASTERISK:
      if (!ffestb_subrargs_.name_list.is_subr)
	break;

    case FFELEX_typeNAME:
      ffestt_tokenlist_append (ffestb_subrargs_.name_list.args,
			       ffelex_token_use (t));
      return (ffelexHandler) ffestb_subr_name_list_1_;

    default:
      break;
    }

  ffestb_subrargs_.name_list.ok = FALSE;
  ffestb_subrargs_.name_list.close_paren = ffelex_token_use (t);
  if (ffestb_subrargs_.name_list.names)
    ffelex_set_names (TRUE);
  return (ffelexHandler) (*ffestb_subrargs_.name_list.handler) (t);
}

/* ffestb_subr_name_list_1_ -- NAME or ASTERISK

   return ffestb_subr_name_list_1_;  // to lexer

   The next token must be COMMA or CLOSE_PAREN, either way go to original
   state, but only after adding the appropriate name list item.	 */

static ffelexHandler
ffestb_subr_name_list_1_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      return (ffelexHandler) ffestb_subr_name_list_;

    case FFELEX_typeCLOSE_PAREN:
      ffestb_subrargs_.name_list.ok = TRUE;
      ffestb_subrargs_.name_list.close_paren = ffelex_token_use (t);
      if (ffestb_subrargs_.name_list.names)
	ffelex_set_names (TRUE);
      return (ffelexHandler) ffestb_subrargs_.name_list.handler;

    default:
      ffestb_subrargs_.name_list.ok = FALSE;
      ffestb_subrargs_.name_list.close_paren = ffelex_token_use (t);
      if (ffestb_subrargs_.name_list.names)
	ffelex_set_names (TRUE);
      return (ffelexHandler) (*ffestb_subrargs_.name_list.handler) (t);
    }
}

static void
ffestb_subr_R1001_append_p_ (void)
{
  ffesttFormatList f;

  if (!ffestb_local_.format.pre.present)
    {
      ffesta_ffebad_1t (FFEBAD_FORMAT_BAD_P_SPEC, ffestb_local_.format.t);
      ffelex_token_kill (ffestb_local_.format.t);
      return;
    }

  f = ffestt_formatlist_append (ffestb_local_.format.f);
  f->type = FFESTP_formattypeP;
  f->t = ffestb_local_.format.t;
  f->u.R1010.val = ffestb_local_.format.pre;
}

/* ffestb_decl_kindparam_ -- "type" OPEN_PAREN

   return ffestb_decl_kindparam_;  // to lexer

   Handle "[KIND=]expr)".  */

static ffelexHandler
ffestb_decl_kindparam_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_decl_kindparam_1_;

    default:
      return (ffelexHandler) (*((ffelexHandler)
				ffeexpr_rhs (ffesta_output_pool,
					     FFEEXPR_contextKINDTYPE,
			       (ffeexprCallback) ffestb_decl_kindparam_2_)))
	(t);
    }
}

/* ffestb_decl_kindparam_1_ -- "type" OPEN_PAREN NAME

   return ffestb_decl_kindparam_1_;  // to lexer

   Handle "[KIND=]expr)".  */

static ffelexHandler
ffestb_decl_kindparam_1_ (ffelexToken t)
{
  ffelexHandler next;
  ffelexToken nt;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEQUALS:
      ffesta_confirmed ();
      if (ffestr_other (ffesta_tokens[1]) != FFESTR_otherKIND)
	break;
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
       FFEEXPR_contextKINDTYPE, (ffeexprCallback) ffestb_decl_kindparam_2_);

    default:
      nt = ffesta_tokens[1];
      next = (ffelexHandler) (*((ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
      FFEEXPR_contextKINDTYPE, (ffeexprCallback) ffestb_decl_kindparam_2_)))
	(nt);
      ffelex_token_kill (nt);
      return (ffelexHandler) (*next) (t);
    }

  if (ffestb_local_.decl.recursive != NULL)
    ffelex_token_kill (ffestb_local_.decl.recursive);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM,
		     ffestb_local_.decl.badname,
		     ffesta_tokens[1]);
  ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_kindparam_2_ -- "type" OPEN_PAREN ["KIND="] expr

   (ffestb_decl_kindparam_2_)  // to expression handler

   Handle "[KIND=]expr)".  */

static ffelexHandler
ffestb_decl_kindparam_2_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCLOSE_PAREN:
      ffestb_local_.decl.kind = expr;
      ffestb_local_.decl.kindt = ffelex_token_use (ft);
      ffestb_local_.decl.len = NULL;
      ffestb_local_.decl.lent = NULL;
      ffelex_set_names (TRUE);
      return (ffelexHandler) ffestb_local_.decl.handler;

    default:
      break;
    }

  if (ffestb_local_.decl.recursive != NULL)
    ffelex_token_kill (ffestb_local_.decl.recursive);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM,
		    ffestb_local_.decl.badname,
		    t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_starkind_ -- "type" ASTERISK

   return ffestb_decl_starkind_;  // to lexer

   Handle NUMBER.  */

static ffelexHandler
ffestb_decl_starkind_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNUMBER:
      ffestb_local_.decl.kindt = ffelex_token_use (t);
      ffestb_local_.decl.kind = NULL;
      ffestb_local_.decl.len = NULL;
      ffestb_local_.decl.lent = NULL;
      ffelex_set_names (TRUE);
      return (ffelexHandler) ffestb_local_.decl.handler;

    default:
      break;
    }

  if (ffestb_local_.decl.recursive != NULL)
    ffelex_token_kill (ffestb_local_.decl.recursive);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM,
		    ffestb_local_.decl.badname,
		    t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_starlen_ -- "CHARACTER" ASTERISK

   return ffestb_decl_starlen_;	 // to lexer

   Handle NUMBER.  */

static ffelexHandler
ffestb_decl_starlen_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNUMBER:
      ffestb_local_.decl.kind = NULL;
      ffestb_local_.decl.kindt = NULL;
      ffestb_local_.decl.len = NULL;
      ffestb_local_.decl.lent = ffelex_token_use (t);
      ffelex_set_names (TRUE);
      return (ffelexHandler) ffestb_local_.decl.handler;

    case FFELEX_typeOPEN_PAREN:
      ffestb_local_.decl.kind = NULL;
      ffestb_local_.decl.kindt = NULL;
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
					  FFEEXPR_contextCHARACTERSIZE,
				  (ffeexprCallback) ffestb_decl_starlen_1_);

    default:
      break;
    }

  if (ffestb_local_.decl.recursive != NULL)
    ffelex_token_kill (ffestb_local_.decl.recursive);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM,
		    ffestb_local_.decl.badname,
		    t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_starlen_1_ -- "CHARACTER" ASTERISK OPEN_PAREN expr

   (ffestb_decl_starlen_1_)  // to expression handler

   Handle CLOSE_PAREN.	*/

static ffelexHandler
ffestb_decl_starlen_1_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCLOSE_PAREN:
      if (expr == NULL)
	break;
      ffestb_local_.decl.len = expr;
      ffestb_local_.decl.lent = ffelex_token_use (ft);
      ffelex_set_names (TRUE);
      return (ffelexHandler) ffestb_local_.decl.handler;

    default:
      break;
    }

  if (ffestb_local_.decl.recursive != NULL)
    ffelex_token_kill (ffestb_local_.decl.recursive);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM,
		    ffestb_local_.decl.badname,
		    t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_typeparams_ -- "CHARACTER" OPEN_PAREN

   return ffestb_decl_typeparams_;  // to lexer

   Handle "[KIND=]expr)".  */

static ffelexHandler
ffestb_decl_typeparams_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_decl_typeparams_1_;

    default:
      if (ffestb_local_.decl.lent == NULL)
	return (ffelexHandler) (*((ffelexHandler)
				  ffeexpr_rhs (ffesta_output_pool,
					       FFEEXPR_contextCHARACTERSIZE,
			      (ffeexprCallback) ffestb_decl_typeparams_2_)))
	  (t);
      if (ffestb_local_.decl.kindt != NULL)
	break;
      return (ffelexHandler) (*((ffelexHandler)
				ffeexpr_rhs (ffesta_output_pool,
					     FFEEXPR_contextKINDTYPE,
			      (ffeexprCallback) ffestb_decl_typeparams_3_)))
	(t);
    }

  if (ffestb_local_.decl.recursive != NULL)
    ffelex_token_kill (ffestb_local_.decl.recursive);
  if (ffestb_local_.decl.kindt != NULL)
    ffelex_token_kill (ffestb_local_.decl.kindt);
  if (ffestb_local_.decl.lent != NULL)
    ffelex_token_kill (ffestb_local_.decl.lent);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM,
		    ffestb_local_.decl.badname,
		    t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_typeparams_1_ -- "CHARACTER" OPEN_PAREN NAME

   return ffestb_decl_typeparams_1_;  // to lexer

   Handle "[KIND=]expr)".  */

static ffelexHandler
ffestb_decl_typeparams_1_ (ffelexToken t)
{
  ffelexHandler next;
  ffelexToken nt;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEQUALS:
      ffesta_confirmed ();
      switch (ffestr_other (ffesta_tokens[1]))
	{
	case FFESTR_otherLEN:
	  if (ffestb_local_.decl.lent != NULL)
	    break;
	  ffelex_token_kill (ffesta_tokens[1]);
	  return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
					      FFEEXPR_contextCHARACTERSIZE,
			       (ffeexprCallback) ffestb_decl_typeparams_2_);

	case FFESTR_otherKIND:
	  if (ffestb_local_.decl.kindt != NULL)
	    break;
	  ffelex_token_kill (ffesta_tokens[1]);
	  return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
					      FFEEXPR_contextKINDTYPE,
			       (ffeexprCallback) ffestb_decl_typeparams_3_);

	default:
	  break;
	}
      break;

    default:
      nt = ffesta_tokens[1];
      if (ffestb_local_.decl.lent == NULL)
	next = (ffelexHandler) (*((ffelexHandler)
				  ffeexpr_rhs (ffesta_output_pool,
					       FFEEXPR_contextCHARACTERSIZE,
			      (ffeexprCallback) ffestb_decl_typeparams_2_)))
	  (nt);
      else if (ffestb_local_.decl.kindt == NULL)
	next = (ffelexHandler) (*((ffelexHandler)
				  ffeexpr_rhs (ffesta_output_pool,
					       FFEEXPR_contextKINDTYPE,
			      (ffeexprCallback) ffestb_decl_typeparams_3_)))
	  (nt);
      else
	{
	  ffesta_tokens[1] = nt;
	  break;
	}
      ffelex_token_kill (nt);
      return (ffelexHandler) (*next) (t);
    }

  if (ffestb_local_.decl.recursive != NULL)
    ffelex_token_kill (ffestb_local_.decl.recursive);
  if (ffestb_local_.decl.kindt != NULL)
    ffelex_token_kill (ffestb_local_.decl.kindt);
  if (ffestb_local_.decl.lent != NULL)
    ffelex_token_kill (ffestb_local_.decl.lent);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM,
		    ffestb_local_.decl.badname,
		    ffesta_tokens[1]);
  ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_typeparams_2_ -- "CHARACTER" OPEN_PAREN ["LEN="] expr

   (ffestb_decl_typeparams_2_)	// to expression handler

   Handle "[LEN=]expr)".  */

static ffelexHandler
ffestb_decl_typeparams_2_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCLOSE_PAREN:
      ffestb_local_.decl.len = expr;
      ffestb_local_.decl.lent = ffelex_token_use (ft);
      ffelex_set_names (TRUE);
      return (ffelexHandler) ffestb_local_.decl.handler;

    case FFELEX_typeCOMMA:
      ffestb_local_.decl.len = expr;
      ffestb_local_.decl.lent = ffelex_token_use (ft);
      return (ffelexHandler) ffestb_decl_typeparams_;

    default:
      break;
    }

  if (ffestb_local_.decl.recursive != NULL)
    ffelex_token_kill (ffestb_local_.decl.recursive);
  if (ffestb_local_.decl.kindt != NULL)
    ffelex_token_kill (ffestb_local_.decl.kindt);
  if (ffestb_local_.decl.lent != NULL)
    ffelex_token_kill (ffestb_local_.decl.lent);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM,
		    ffestb_local_.decl.badname,
		    t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_typeparams_3_ -- "CHARACTER" OPEN_PAREN ["KIND="] expr

   (ffestb_decl_typeparams_3_)	// to expression handler

   Handle "[KIND=]expr)".  */

static ffelexHandler
ffestb_decl_typeparams_3_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCLOSE_PAREN:
      ffestb_local_.decl.kind = expr;
      ffestb_local_.decl.kindt = ffelex_token_use (ft);
      ffelex_set_names (TRUE);
      return (ffelexHandler) ffestb_local_.decl.handler;

    case FFELEX_typeCOMMA:
      ffestb_local_.decl.kind = expr;
      ffestb_local_.decl.kindt = ffelex_token_use (ft);
      return (ffelexHandler) ffestb_decl_typeparams_;

    default:
      break;
    }

  if (ffestb_local_.decl.recursive != NULL)
    ffelex_token_kill (ffestb_local_.decl.recursive);
  if (ffestb_local_.decl.kindt != NULL)
    ffelex_token_kill (ffestb_local_.decl.kindt);
  if (ffestb_local_.decl.lent != NULL)
    ffelex_token_kill (ffestb_local_.decl.lent);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM,
		    ffestb_local_.decl.badname,
		    t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_typetype1_ -- "TYPE" OPEN_PAREN

   return ffestb_decl_typetype1_;  // to lexer

   Handle NAME.	 */

#if FFESTR_F90
static ffelexHandler
ffestb_decl_typetype1_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffestb_local_.decl.kindt = ffelex_token_use (t);
      return (ffelexHandler) ffestb_decl_typetype2_;

    default:
      break;
    }

  if (ffestb_local_.decl.recursive != NULL)
    ffelex_token_kill (ffestb_local_.decl.recursive);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM,
		    ffestb_local_.decl.badname,
		    t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_typetype2_ -- "TYPE" OPEN_PAREN NAME

   return ffestb_decl_typetype2_;  // to lexer

   Handle CLOSE_PAREN.	*/

static ffelexHandler
ffestb_decl_typetype2_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCLOSE_PAREN:
      ffestb_local_.decl.type = FFESTP_typeTYPE;
      ffestb_local_.decl.kind = NULL;
      ffestb_local_.decl.len = NULL;
      ffestb_local_.decl.lent = NULL;
      ffelex_set_names (TRUE);
      return (ffelexHandler) ffestb_local_.decl.handler;

    default:
      break;
    }

  if (ffestb_local_.decl.recursive != NULL)
    ffelex_token_kill (ffestb_local_.decl.recursive);
  ffelex_token_kill (ffestb_local_.decl.kindt);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM,
		    ffestb_local_.decl.badname,
		    t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

#endif
/* ffestb_subr_label_list_ -- Collect a tokenlist of labels and close-paren

   return ffestb_subr_label_list_;  // to lexer after seeing OPEN_PAREN

   First token must be a NUMBER.  Must be followed by zero or more COMMA
   NUMBER pairs.  Must then be followed by a CLOSE_PAREN.  If all ok, put
   the NUMBER tokens in a token list and return via the handler for the
   token after CLOSE_PAREN.  Else return via
   same handler, but with the ok return value set FALSE.  */

static ffelexHandler
ffestb_subr_label_list_ (ffelexToken t)
{
  if (ffelex_token_type (t) == FFELEX_typeNUMBER)
    {
      ffestt_tokenlist_append (ffestb_subrargs_.label_list.labels,
			       ffelex_token_use (t));
      return (ffelexHandler) ffestb_subr_label_list_1_;
    }

  ffestb_subrargs_.label_list.ok = FALSE;
  return (ffelexHandler) (*ffestb_subrargs_.label_list.handler) (t);
}

/* ffestb_subr_label_list_1_ -- NUMBER

   return ffestb_subr_label_list_1_;  // to lexer after seeing NUMBER

   The next token must be COMMA, in which case go back to
   ffestb_subr_label_list_, or CLOSE_PAREN, in which case set ok to TRUE
   and go to the handler.  */

static ffelexHandler
ffestb_subr_label_list_1_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      return (ffelexHandler) ffestb_subr_label_list_;

    case FFELEX_typeCLOSE_PAREN:
      ffestb_subrargs_.label_list.ok = TRUE;
      return (ffelexHandler) ffestb_subrargs_.label_list.handler;

    default:
      ffestb_subrargs_.label_list.ok = FALSE;
      return (ffelexHandler) (*ffestb_subrargs_.label_list.handler) (t);
    }
}

/* ffestb_do -- Parse the DO statement

   return ffestb_do;  // to lexer

   Make sure the statement has a valid form for the DO statement.  If it
   does, implement the statement.  */

ffelexHandler
ffestb_do (ffelexToken t)
{
  ffeTokenLength i;
  unsigned const char *p;
  ffelexHandler next;
  ffelexToken nt;
  ffestrSecond kw;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstDO)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeNUMBER:
	  ffesta_confirmed ();
	  ffesta_tokens[1] = ffelex_token_use (t);
	  return (ffelexHandler) ffestb_do1_;

	case FFELEX_typeCOMMA:
	  ffesta_confirmed ();
	  ffesta_tokens[1] = NULL;
	  return (ffelexHandler) ffestb_do2_;

	case FFELEX_typeNAME:
	  ffesta_confirmed ();
	  ffesta_tokens[1] = NULL;
	  ffesta_tokens[2] = ffelex_token_use (t);
	  return (ffelexHandler) ffestb_do3_;

	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  ffesta_confirmed ();
	  ffesta_tokens[1] = NULL;
	  return (ffelexHandler) ffestb_do1_ (t);

	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */
	}

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstDO)
	goto bad_0;		/* :::::::::::::::::::: */
      p = ffelex_token_text (ffesta_tokens[0]) + (i = FFESTR_firstlDO);
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeOPEN_PAREN:	/* Must be "DO" label "WHILE". */
	  if (! ISDIGIT (*p))
	    goto bad_i;		/* :::::::::::::::::::: */
	  ffesta_tokens[1] = ffelex_token_number_from_names (ffesta_tokens[0],
							     i);
	  p += ffelex_token_length (ffesta_tokens[1]);
	  i += ffelex_token_length (ffesta_tokens[1]);
	  if (((*p) != 'W') && ((*p) != 'w'))
	    goto bad_i1;	/* :::::::::::::::::::: */
	  nt = ffelex_token_name_from_names (ffesta_tokens[0], i, 0);
	  kw = ffestr_second (nt);
	  ffelex_token_kill (nt);
	  if (kw != FFESTR_secondWHILE)
	    goto bad_i1;	/* :::::::::::::::::::: */
	  return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		     FFEEXPR_contextDOWHILE, (ffeexprCallback) ffestb_do4_);

	case FFELEX_typeCOMMA:
	  ffesta_confirmed ();
	  if (*p == '\0')
	    {
	      ffesta_tokens[1] = NULL;
	      return (ffelexHandler) ffestb_do2_;
	    }
	  if (! ISDIGIT (*p))
	    goto bad_i;		/* :::::::::::::::::::: */
	  ffesta_tokens[1] = ffelex_token_number_from_names (ffesta_tokens[0],
							     i);
	  p += ffelex_token_length (ffesta_tokens[1]);
	  i += ffelex_token_length (ffesta_tokens[1]);
	  if (*p != '\0')
	    goto bad_i1;	/* :::::::::::::::::::: */
	  return (ffelexHandler) ffestb_do2_;

	case FFELEX_typeEQUALS:
	  if (ISDIGIT (*p))
	    {
	      ffesta_tokens[1]
		= ffelex_token_number_from_names (ffesta_tokens[0], i);
	      p += ffelex_token_length (ffesta_tokens[1]);
	      i += ffelex_token_length (ffesta_tokens[1]);
	    }
	  else
	    ffesta_tokens[1] = NULL;
	  if (!ffesrc_is_name_init (*p))
	    goto bad_i1;	/* :::::::::::::::::::: */
	  nt = ffelex_token_name_from_names (ffesta_tokens[0], i, 0);
	  next = (ffelexHandler) (*((ffelexHandler) ffeexpr_lhs
				    (ffesta_output_pool, FFEEXPR_contextDO,
				     (ffeexprCallback) ffestb_do6_)))
	    (nt);
	  ffelex_token_kill (nt);	/* Will get it back in _6_... */
	  return (ffelexHandler) (*next) (t);

	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  ffesta_confirmed ();
	  if (ISDIGIT (*p))
	    {
	      ffesta_tokens[1]
		= ffelex_token_number_from_names (ffesta_tokens[0], i);
	      p += ffelex_token_length (ffesta_tokens[1]);
	      i += ffelex_token_length (ffesta_tokens[1]);
	    }
	  else
	    ffesta_tokens[1] = NULL;
	  if (*p != '\0')
	    goto bad_i1;	/* :::::::::::::::::::: */
	  return (ffelexHandler) ffestb_do1_ (t);
	}

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  if (ffesta_construct_name != NULL)
    {
      ffelex_token_kill (ffesta_construct_name);
      ffesta_construct_name = NULL;
    }
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "DO", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  if (ffesta_construct_name != NULL)
    {
      ffelex_token_kill (ffesta_construct_name);
      ffesta_construct_name = NULL;
    }
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "DO", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */

bad_i1:			/* :::::::::::::::::::: */
  if (ffesta_tokens[1])
    ffelex_token_kill (ffesta_tokens[1]);

bad_i:				/* :::::::::::::::::::: */
  if (ffesta_construct_name != NULL)
    {
      ffelex_token_kill (ffesta_construct_name);
      ffesta_construct_name = NULL;
    }
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, "DO", ffesta_tokens[0], i, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_dowhile -- Parse the DOWHILE statement

   return ffestb_dowhile;  // to lexer

   Make sure the statement has a valid form for the DOWHILE statement.	If it
   does, implement the statement.  */

ffelexHandler
ffestb_dowhile (ffelexToken t)
{
  ffeTokenLength i;
  const char *p;
  ffelexHandler next;
  ffelexToken nt;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstDOWHILE)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeOPEN_PAREN:
	  p = ffelex_token_text (ffesta_tokens[0]) + (i = FFESTR_firstlDOWHILE);
	  if (*p != '\0')
	    goto bad_i;		/* :::::::::::::::::::: */
	  ffesta_tokens[1] = NULL;
	  return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		     FFEEXPR_contextDOWHILE, (ffeexprCallback) ffestb_do4_);

	case FFELEX_typeEQUALS:/* Not really DOWHILE, but DOWHILExyz=.... */
	  ffesta_tokens[1] = NULL;
	  nt = ffelex_token_name_from_names (ffesta_tokens[0], FFESTR_firstlDO,
					     0);
	  next = (ffelexHandler) (*((ffelexHandler) ffeexpr_lhs
				    (ffesta_output_pool, FFEEXPR_contextDO,
				     (ffeexprCallback) ffestb_do6_)))
	    (nt);
	  ffelex_token_kill (nt);	/* Will get it back in _6_... */
	  return (ffelexHandler) (*next) (t);
	}

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  if (ffesta_construct_name != NULL)
    {
      ffelex_token_kill (ffesta_construct_name);
      ffesta_construct_name = NULL;
    }
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "DO", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  if (ffesta_construct_name != NULL)
    {
      ffelex_token_kill (ffesta_construct_name);
      ffesta_construct_name = NULL;
    }
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "DO", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */

bad_i:				/* :::::::::::::::::::: */
  if (ffesta_construct_name != NULL)
    {
      ffelex_token_kill (ffesta_construct_name);
      ffesta_construct_name = NULL;
    }
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, "DO", ffesta_tokens[0], i, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_do1_ -- "DO" [label]

   return ffestb_do1_;	// to lexer

   Make sure the statement has a valid form for the DO statement.  If it
   does, implement the statement.  */

static ffelexHandler
ffestb_do1_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      ffesta_confirmed ();
      return (ffelexHandler) ffestb_do2_;

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	{
	  if (ffesta_tokens[1] != NULL)
	    ffestc_R819B (ffesta_construct_name, ffesta_tokens[1], NULL,
			  NULL);
	  else
	    ffestc_R820B (ffesta_construct_name, NULL, NULL);
	}
      if (ffesta_tokens[1] != NULL)
	ffelex_token_kill (ffesta_tokens[1]);
      if (ffesta_construct_name != NULL)
	{
	  ffelex_token_kill (ffesta_construct_name);
	  ffesta_construct_name = NULL;
	}
      return (ffelexHandler) ffesta_zero (t);

    case FFELEX_typeNAME:
      return (ffelexHandler) ffestb_do2_ (t);

    default:
      break;
    }

  if (ffesta_tokens[1] != NULL)
    ffelex_token_kill (ffesta_tokens[1]);
  if (ffesta_construct_name != NULL)
    {
      ffelex_token_kill (ffesta_construct_name);
      ffesta_construct_name = NULL;
    }
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "DO", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_do2_ -- "DO" [label] [,]

   return ffestb_do2_;	// to lexer

   Make sure the statement has a valid form for the DO statement.  If it
   does, implement the statement.  */

static ffelexHandler
ffestb_do2_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_tokens[2] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_do3_;

    default:
      break;
    }

  if (ffesta_tokens[1] != NULL)
    ffelex_token_kill (ffesta_tokens[1]);
  if (ffesta_construct_name != NULL)
    {
      ffelex_token_kill (ffesta_construct_name);
      ffesta_construct_name = NULL;
    }
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "DO", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_do3_ -- "DO" [label] [,] NAME

   return ffestb_do3_;	// to lexer

   Make sure the statement has a valid form for the DO statement.  If it
   does, implement the statement.  */

static ffelexHandler
ffestb_do3_ (ffelexToken t)
{
  ffelexHandler next;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEQUALS:
      next = (ffelexHandler) (*((ffelexHandler) ffeexpr_lhs (ffesta_output_pool,
			 FFEEXPR_contextDO, (ffeexprCallback) ffestb_do6_)))
	(ffesta_tokens[2]);
      ffelex_token_kill (ffesta_tokens[2]);	/* Will get it back in _6_... */
      return (ffelexHandler) (*next) (t);

    case FFELEX_typeOPEN_PAREN:
      if (ffestr_second (ffesta_tokens[2]) != FFESTR_secondWHILE)
	{
	  if (ffesta_tokens[1] != NULL)
	    ffelex_token_kill (ffesta_tokens[1]);
	  if (ffesta_construct_name != NULL)
	    {
	      ffelex_token_kill (ffesta_construct_name);
	      ffesta_construct_name = NULL;
	    }
	  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "DO", ffesta_tokens[2]);
	  ffelex_token_kill (ffesta_tokens[2]);
	  return (ffelexHandler) ffelex_swallow_tokens (t,
					       (ffelexHandler) ffesta_zero);	/* Invalid token. */
	}
      ffelex_token_kill (ffesta_tokens[2]);
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		     FFEEXPR_contextDOWHILE, (ffeexprCallback) ffestb_do4_);

    default:
      break;
    }

  ffelex_token_kill (ffesta_tokens[2]);
  if (ffesta_tokens[1] != NULL)
    ffelex_token_kill (ffesta_tokens[1]);
  if (ffesta_construct_name != NULL)
    {
      ffelex_token_kill (ffesta_construct_name);
      ffesta_construct_name = NULL;
    }
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "DO", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_do4_ -- "DO" [label] [,] "WHILE" OPEN_PAREN expr

   (ffestb_do4_)  // to expression handler

   Make sure the statement has a valid form for the DO statement.  If it
   does, implement the statement.  */

static ffelexHandler
ffestb_do4_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCLOSE_PAREN:
      if (expr == NULL)
	break;
      ffesta_tokens[2] = ffelex_token_use (ft);
      ffestb_local_.dowhile.expr = expr;
      return (ffelexHandler) ffestb_do5_;

    default:
      break;
    }

  if (ffesta_tokens[1] != NULL)
    ffelex_token_kill (ffesta_tokens[1]);
  if (ffesta_construct_name != NULL)
    {
      ffelex_token_kill (ffesta_construct_name);
      ffesta_construct_name = NULL;
    }
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "DO", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_do5_ -- "DO" [label] [,] "WHILE" OPEN_PAREN expr CLOSE_PAREN

   return ffestb_do5_;	// to lexer

   Make sure the statement has a valid form for the DO statement.  If it
   does, implement the statement.  */

static ffelexHandler
ffestb_do5_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	{
	  if (ffesta_tokens[1] != NULL)
	    ffestc_R819B (ffesta_construct_name, ffesta_tokens[1],
			  ffestb_local_.dowhile.expr, ffesta_tokens[2]);
	  else
	    ffestc_R820B (ffesta_construct_name, ffestb_local_.dowhile.expr,
			  ffesta_tokens[2]);
	}
      ffelex_token_kill (ffesta_tokens[2]);
      if (ffesta_tokens[1] != NULL)
	ffelex_token_kill (ffesta_tokens[1]);
      if (ffesta_construct_name != NULL)
	{
	  ffelex_token_kill (ffesta_construct_name);
	  ffesta_construct_name = NULL;
	}
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  ffelex_token_kill (ffesta_tokens[2]);
  if (ffesta_tokens[1] != NULL)
    ffelex_token_kill (ffesta_tokens[1]);
  if (ffesta_construct_name != NULL)
    {
      ffelex_token_kill (ffesta_construct_name);
      ffesta_construct_name = NULL;
    }
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "DO", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_do6_ -- "DO" [label] [,] var-expr

   (ffestb_do6_)  // to expression handler

   Make sure the statement has a valid form for the DO statement.  If it
   does, implement the statement.  */

static ffelexHandler
ffestb_do6_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  /* _3_ already ensured that this would be an EQUALS token.  If not, it is a
     bug in the FFE. */

  assert (ffelex_token_type (t) == FFELEX_typeEQUALS);

  ffesta_tokens[2] = ffelex_token_use (ft);
  ffestb_local_.do_stmt.var = expr;
  return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
			  FFEEXPR_contextDO, (ffeexprCallback) ffestb_do7_);
}

/* ffestb_do7_ -- "DO" [label] [,] var-expr EQUALS expr

   (ffestb_do7_)  // to expression handler

   Make sure the statement has a valid form for the DO statement.  If it
   does, implement the statement.  */

static ffelexHandler
ffestb_do7_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      ffesta_confirmed ();
      if (expr == NULL)
	break;
      ffesta_tokens[3] = ffelex_token_use (ft);
      ffestb_local_.do_stmt.start = expr;
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
			  FFEEXPR_contextDO, (ffeexprCallback) ffestb_do8_);

    default:
      break;
    }

  ffelex_token_kill (ffesta_tokens[2]);
  if (ffesta_tokens[1] != NULL)
    ffelex_token_kill (ffesta_tokens[1]);
  if (ffesta_construct_name != NULL)
    {
      ffelex_token_kill (ffesta_construct_name);
      ffesta_construct_name = NULL;
    }
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "DO", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_do8_ -- "DO" [label] [,] var-expr EQUALS expr COMMA expr

   (ffestb_do8_)  // to expression handler

   Make sure the statement has a valid form for the DO statement.  If it
   does, implement the statement.  */

static ffelexHandler
ffestb_do8_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      if (expr == NULL)
	break;
      ffesta_tokens[4] = ffelex_token_use (ft);
      ffestb_local_.do_stmt.end = expr;
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
			  FFEEXPR_contextDO, (ffeexprCallback) ffestb_do9_);

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (expr == NULL)
	break;
      ffesta_tokens[4] = ffelex_token_use (ft);
      ffestb_local_.do_stmt.end = expr;
      return (ffelexHandler) ffestb_do9_ (NULL, NULL, t);

    default:
      break;
    }

  ffelex_token_kill (ffesta_tokens[3]);
  ffelex_token_kill (ffesta_tokens[2]);
  if (ffesta_tokens[1] != NULL)
    ffelex_token_kill (ffesta_tokens[1]);
  if (ffesta_construct_name != NULL)
    {
      ffelex_token_kill (ffesta_construct_name);
      ffesta_construct_name = NULL;
    }
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "DO", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_do9_ -- "DO" [label] [,] var-expr EQUALS expr COMMA expr
		  [COMMA expr]

   (ffestb_do9_)  // to expression handler

   Make sure the statement has a valid form for the DO statement.  If it
   does, implement the statement.  */

static ffelexHandler
ffestb_do9_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if ((expr == NULL) && (ft != NULL))
	break;
      if (!ffesta_is_inhibited ())
	{
	  if (ffesta_tokens[1] != NULL)
	    ffestc_R819A (ffesta_construct_name, ffesta_tokens[1],
			  ffestb_local_.do_stmt.var, ffesta_tokens[2],
			  ffestb_local_.do_stmt.start, ffesta_tokens[3],
		     ffestb_local_.do_stmt.end, ffesta_tokens[4], expr, ft);
	  else
	    ffestc_R820A (ffesta_construct_name, ffestb_local_.do_stmt.var,
			  ffesta_tokens[2], ffestb_local_.do_stmt.start,
			  ffesta_tokens[3], ffestb_local_.do_stmt.end,
			  ffesta_tokens[4], expr, ft);
	}
      ffelex_token_kill (ffesta_tokens[4]);
      ffelex_token_kill (ffesta_tokens[3]);
      ffelex_token_kill (ffesta_tokens[2]);
      if (ffesta_tokens[1] != NULL)
	ffelex_token_kill (ffesta_tokens[1]);
      if (ffesta_construct_name != NULL)
	{
	  ffelex_token_kill (ffesta_construct_name);
	  ffesta_construct_name = NULL;
	}

      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  ffelex_token_kill (ffesta_tokens[4]);
  ffelex_token_kill (ffesta_tokens[3]);
  ffelex_token_kill (ffesta_tokens[2]);
  if (ffesta_tokens[1] != NULL)
    ffelex_token_kill (ffesta_tokens[1]);
  if (ffesta_construct_name != NULL)
    {
      ffelex_token_kill (ffesta_construct_name);
      ffesta_construct_name = NULL;
    }
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "DO", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_else -- Parse the ELSE statement

   return ffestb_else;	// to lexer

   Make sure the statement has a valid form for the ELSE statement.  If it
   does, implement the statement.  */

ffelexHandler
ffestb_else (ffelexToken t)
{
  ffeTokenLength i;
  unsigned const char *p;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstELSE)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  ffesta_confirmed ();
	  ffesta_tokens[1] = NULL;
	  ffestb_args.elsexyz.second = FFESTR_secondNone;
	  return (ffelexHandler) ffestb_else1_ (t);

	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeNAME:
	  break;
	}

      ffesta_confirmed ();
      ffestb_args.elsexyz.second = ffesta_second_kw;
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_else1_;

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstELSE)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  break;
	}
      ffesta_confirmed ();
      if (ffelex_token_length (ffesta_tokens[0]) != FFESTR_firstlELSE)
	{
	  p = ffelex_token_text (ffesta_tokens[0]) + (i = FFESTR_firstlELSE);
	  if (!ffesrc_is_name_init (*p))
	    goto bad_i;		/* :::::::::::::::::::: */
	  ffesta_tokens[1]
	    = ffelex_token_name_from_names (ffesta_tokens[0], i, 0);
	}
      else
	ffesta_tokens[1] = NULL;
      ffestb_args.elsexyz.second = FFESTR_secondNone;
      return (ffelexHandler) ffestb_else1_ (t);

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "ELSE", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "ELSE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */

bad_i:				/* :::::::::::::::::::: */
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, "ELSE", ffesta_tokens[0], i, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_elsexyz -- Parse an ELSEIF/ELSEWHERE statement

   return ffestb_elsexyz;  // to lexer

   Expects len and second to be set in ffestb_args.elsexyz to the length
   of the ELSExyz keyword involved and the corresponding ffestrSecond value.  */

ffelexHandler
ffestb_elsexyz (ffelexToken t)
{
  ffeTokenLength i;
  const char *p;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  if (ffesta_first_kw == FFESTR_firstELSEIF)
	    goto bad_0;		/* :::::::::::::::::::: */
	  ffesta_confirmed ();
	  ffesta_tokens[1] = NULL;
	  return (ffelexHandler) ffestb_else1_ (t);

	case FFELEX_typeNAME:
	  ffesta_confirmed ();
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeOPEN_PAREN:
	  if (ffesta_first_kw != FFESTR_firstELSEIF)
	    goto bad_0;		/* :::::::::::::::::::: */
	  ffesta_tokens[1] = NULL;
	  return (ffelexHandler) ffestb_else1_ (t);

	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */
	}

    case FFELEX_typeNAMES:
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeOPEN_PAREN:
	  if (ffesta_first_kw != FFESTR_firstELSEIF)
	    goto bad_1;		/* :::::::::::::::::::: */
	  if (ffelex_token_length (ffesta_tokens[0]) != FFESTR_firstlELSEIF)
	    {
	      i = FFESTR_firstlELSEIF;
	      goto bad_i;	/* :::::::::::::::::::: */
	    }
	  ffesta_tokens[1] = NULL;
	  return (ffelexHandler) ffestb_else1_ (t);

	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  break;
	}
      ffesta_confirmed ();
      p = ffelex_token_text (ffesta_tokens[0]) + (i = FFESTR_firstlELSE);
      ffesta_tokens[1]
	= ffelex_token_name_from_names (ffesta_tokens[0], i, 0);
#if FFESTR_F90
      if ((ffestb_args.elsexyz.second == FFESTR_secondWHERE)
	  && (ffelex_token_length (ffesta_tokens[1]) != FFESTR_secondlWHERE))
	ffestb_args.elsexyz.second = FFESTR_secondNone;
#endif
      return (ffelexHandler) ffestb_else1_ (t);

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "ELSE", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "ELSE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */

bad_i:				/* :::::::::::::::::::: */
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, "ELSE IF", ffesta_tokens[0], i, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_else1_ -- "ELSE" (NAME)

   return ffestb_else1_;  // to lexer

   If EOS/SEMICOLON, implement the appropriate statement (keep in mind that
   "ELSE WHERE" is ambiguous at the syntactic level).  If OPEN_PAREN, start
   expression analysis with callback at _2_.  */

static ffelexHandler
ffestb_else1_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeOPEN_PAREN:
      if (ffestb_args.elsexyz.second == FFESTR_secondIF)
	{
	  if (ffesta_tokens[1] != NULL)
	    ffelex_token_kill (ffesta_tokens[1]);
	  return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
			FFEEXPR_contextIF, (ffeexprCallback) ffestb_else2_);
	}
      /* Fall through. */
    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "ELSE", t);
      if (ffesta_tokens[1] != NULL)
	ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffelex_swallow_tokens (t,
					       (ffelexHandler) ffesta_zero);

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      break;

    }

  switch (ffestb_args.elsexyz.second)
    {
#if FFESTR_F90
    case FFESTR_secondWHERE:
      if (!ffesta_is_inhibited ())
	if ((ffesta_first_kw == FFESTR_firstELSEWHERE)
	    && (ffelex_token_type (ffesta_tokens[0]) == FFELEX_typeNAME))
	  ffestc_R744 ();
	else
	  ffestc_elsewhere (ffesta_tokens[1]);	/* R744 or R805. */
      break;
#endif

    default:
      if (!ffesta_is_inhibited ())
	ffestc_R805 (ffesta_tokens[1]);
      break;
    }

  if (ffesta_tokens[1] != NULL)
    ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffesta_zero (t);
}

/* ffestb_else2_ -- "ELSE" "IF" OPEN_PAREN expr

   (ffestb_else2_)  // to expression handler

   Make sure the next token is CLOSE_PAREN.  */

static ffelexHandler
ffestb_else2_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  ffestb_local_.else_stmt.expr = expr;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCLOSE_PAREN:
      if (expr == NULL)
	break;
      ffesta_tokens[1] = ffelex_token_use (ft);
      ffelex_set_names (TRUE);
      return (ffelexHandler) ffestb_else3_;

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "ELSE IF", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_else3_ -- "ELSE" "IF" OPEN_PAREN expr CLOSE_PAREN

   return ffestb_else3_;  // to lexer

   Make sure the next token is "THEN".	*/

static ffelexHandler
ffestb_else3_ (ffelexToken t)
{
  ffeTokenLength i;
  unsigned const char *p;

  ffelex_set_names (FALSE);

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_confirmed ();
      if (ffestr_first (t) == FFESTR_firstTHEN)
	return (ffelexHandler) ffestb_else4_;
      break;

    case FFELEX_typeNAMES:
      ffesta_confirmed ();
      if (ffestr_first (t) != FFESTR_firstTHEN)
	break;
      if (ffelex_token_length (t) == FFESTR_firstlTHEN)
	return (ffelexHandler) ffestb_else4_;
      p = ffelex_token_text (t) + (i = FFESTR_firstlTHEN);
      if (!ffesrc_is_name_init (*p))
	goto bad_i;		/* :::::::::::::::::::: */
      ffesta_tokens[2] = ffelex_token_name_from_names (t, i, 0);
      return (ffelexHandler) ffestb_else5_;

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "ELSE IF", t);
  ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_i:				/* :::::::::::::::::::: */
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, "ELSE IF", t, i, NULL);
  ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_else4_ -- "ELSE" "IF" OPEN_PAREN expr CLOSE_PAREN "THEN"

   return ffestb_else4_;  // to lexer

   Handle a NAME or EOS/SEMICOLON, then go to state _5_.  */

static ffelexHandler
ffestb_else4_ (ffelexToken t)
{
  ffelex_set_names (FALSE);

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_tokens[2] = NULL;
      return (ffelexHandler) ffestb_else5_ (t);

    case FFELEX_typeNAME:
      ffesta_tokens[2] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_else5_;

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "ELSE IF", t);
  ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_else5_ -- "ELSE" "IF" OPEN_PAREN expr CLOSE_PAREN "THEN"

   return ffestb_else5_;  // to lexer

   Make sure the next token is EOS or SEMICOLON; implement R804.  */

static ffelexHandler
ffestb_else5_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (!ffesta_is_inhibited ())
	ffestc_R804 (ffestb_local_.else_stmt.expr, ffesta_tokens[1],
		     ffesta_tokens[2]);
      ffelex_token_kill (ffesta_tokens[1]);
      if (ffesta_tokens[2] != NULL)
	ffelex_token_kill (ffesta_tokens[2]);
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "ELSE IF", t);
  ffelex_token_kill (ffesta_tokens[1]);
  if (ffesta_tokens[2] != NULL)
    ffelex_token_kill (ffesta_tokens[2]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_end -- Parse the END statement

   return ffestb_end;  // to lexer

   Make sure the statement has a valid form for the END statement.  If it
   does, implement the statement.  */

ffelexHandler
ffestb_end (ffelexToken t)
{
  ffeTokenLength i;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstEND)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  ffesta_tokens[1] = NULL;
	  ffestb_args.endxyz.second = FFESTR_secondNone;
	  return (ffelexHandler) ffestb_end3_ (t);

	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeNAME:
	  break;
	}

      ffesta_confirmed ();
      ffestb_args.endxyz.second = ffesta_second_kw;
      switch (ffesta_second_kw)
	{
	case FFESTR_secondFILE:
	  ffestb_args.beru.badname = "ENDFILE";
	  return (ffelexHandler) ffestb_beru;

	case FFESTR_secondBLOCK:
	  return (ffelexHandler) ffestb_end1_;

#if FFESTR_F90
	case FFESTR_secondINTERFACE:
#endif
#if FFESTR_VXT
	case FFESTR_secondMAP:
	case FFESTR_secondSTRUCTURE:
	case FFESTR_secondUNION:
#endif
#if FFESTR_F90
	case FFESTR_secondWHERE:
	  ffesta_tokens[1] = NULL;
	  return (ffelexHandler) ffestb_end3_;
#endif

	case FFESTR_secondNone:
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  return (ffelexHandler) ffestb_end2_;
	}

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstEND)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  break;
	}
      ffesta_confirmed ();
      if (ffelex_token_length (ffesta_tokens[0]) != FFESTR_firstlEND)
	{
	  i = FFESTR_firstlEND;
	  goto bad_i;		/* :::::::::::::::::::: */
	}
      ffesta_tokens[1] = NULL;
      ffestb_args.endxyz.second = FFESTR_secondNone;
      return (ffelexHandler) ffestb_end3_ (t);

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "END", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "END", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */

bad_i:				/* :::::::::::::::::::: */
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, "END", ffesta_tokens[0], i, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_endxyz -- Parse an ENDxyz statement

   return ffestb_endxyz;  // to lexer

   Expects len and second to be set in ffestb_args.endxyz to the length
   of the ENDxyz keyword involved and the corresponding ffestrSecond value.  */

ffelexHandler
ffestb_endxyz (ffelexToken t)
{
  ffeTokenLength i;
  unsigned const char *p;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  ffesta_confirmed ();
	  ffesta_tokens[1] = NULL;
	  return (ffelexHandler) ffestb_end3_ (t);

	case FFELEX_typeNAME:
	  ffesta_confirmed ();
	  switch (ffestb_args.endxyz.second)
	    {
#if FFESTR_F90
	    case FFESTR_secondINTERFACE:
#endif
#if FFESTR_VXT
	    case FFESTR_secondMAP:
	    case FFESTR_secondSTRUCTURE:
	    case FFESTR_secondUNION:
#endif
#if FFESTR_F90
	    case FFESTR_secondWHERE:
	      goto bad_1;	/* :::::::::::::::::::: */
#endif

	    case FFESTR_secondBLOCK:
	      if (ffesta_second_kw != FFESTR_secondDATA)
		goto bad_1;	/* :::::::::::::::::::: */
	      return (ffelexHandler) ffestb_end2_;

	    default:
	      return (ffelexHandler) ffestb_end2_ (t);
	    }

	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */
	}

    case FFELEX_typeNAMES:
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  break;
	}
      ffesta_confirmed ();
      if (ffestb_args.endxyz.second == FFESTR_secondBLOCK)
	{
	  i = FFESTR_firstlEND;
	  goto bad_i;		/* :::::::::::::::::::: */
	}
      if (ffelex_token_length (ffesta_tokens[0]) != ffestb_args.endxyz.len)
	{
	  p = ffelex_token_text (ffesta_tokens[0])
	    + (i = ffestb_args.endxyz.len);
	  switch (ffestb_args.endxyz.second)
	    {
#if FFESTR_F90
	    case FFESTR_secondINTERFACE:
#endif
#if FFESTR_VXT
	    case FFESTR_secondMAP:
	    case FFESTR_secondSTRUCTURE:
	    case FFESTR_secondUNION:
#endif
#if FFESTR_F90
	    case FFESTR_secondWHERE:
	      goto bad_i;	/* :::::::::::::::::::: */
#endif

	    default:
	      break;
	    }
	  if (!ffesrc_is_name_init (*p))
	    goto bad_i;		/* :::::::::::::::::::: */
	  ffesta_tokens[1]
	    = ffelex_token_name_from_names (ffesta_tokens[0], i, 0);
	  return (ffelexHandler) ffestb_end3_ (t);
	}
      ffesta_tokens[1] = NULL;
      return (ffelexHandler) ffestb_end3_ (t);

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "END", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "END", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */

bad_i:				/* :::::::::::::::::::: */
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, "END", ffesta_tokens[0], i, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_end1_ -- "END" "BLOCK"

   return ffestb_end1_;	 // to lexer

   Make sure the next token is "DATA".	*/

static ffelexHandler
ffestb_end1_ (ffelexToken t)
{
  if ((ffelex_token_type (t) == FFELEX_typeNAME)
      && (ffesrc_strcmp_2c (ffe_case_match (), ffelex_token_text (t), "DATA",
			    "data", "Data")
	  == 0))
    {
      return (ffelexHandler) ffestb_end2_;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "END", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_end2_ -- "END" <unit-kind>

   return ffestb_end2_;	 // to lexer

   Make sure the next token is a NAME or EOS.  */

static ffelexHandler
ffestb_end2_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_end3_;

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_tokens[1] = NULL;
      return (ffelexHandler) ffestb_end3_ (t);

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "END", t);
      return (ffelexHandler) ffelex_swallow_tokens (t,
					       (ffelexHandler) ffesta_zero);
    }
}

/* ffestb_end3_ -- "END" <unit-kind> (NAME)

   return ffestb_end3_;	 // to lexer

   Make sure the next token is an EOS, then implement the statement.  */

static ffelexHandler
ffestb_end3_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "END", t);
      if (ffesta_tokens[1] != NULL)
	ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffelex_swallow_tokens (t,
					       (ffelexHandler) ffesta_zero);

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (ffestb_args.endxyz.second == FFESTR_secondNone)
	{
	  if (!ffesta_is_inhibited ())
	    ffestc_end ();
	  return (ffelexHandler) ffesta_zero (t);
	}
      break;
    }

  switch (ffestb_args.endxyz.second)
    {
#if FFESTR_F90
    case FFESTR_secondTYPE:
      if (!ffesta_is_inhibited ())
	ffestc_R425 (ffesta_tokens[1]);
      break;
#endif

#if FFESTR_F90
    case FFESTR_secondWHERE:
      if (!ffesta_is_inhibited ())
	ffestc_R745 ();
      break;
#endif

    case FFESTR_secondIF:
      if (!ffesta_is_inhibited ())
	ffestc_R806 (ffesta_tokens[1]);
      break;

    case FFESTR_secondSELECT:
      if (!ffesta_is_inhibited ())
	ffestc_R811 (ffesta_tokens[1]);
      break;

    case FFESTR_secondDO:
      if (!ffesta_is_inhibited ())
	ffestc_R825 (ffesta_tokens[1]);
      break;

    case FFESTR_secondPROGRAM:
      if (!ffesta_is_inhibited ())
	ffestc_R1103 (ffesta_tokens[1]);
      break;

#if FFESTR_F90
    case FFESTR_secondMODULE:
      if (!ffesta_is_inhibited ())
	ffestc_R1106 (ffesta_tokens[1]);
      break;
#endif
    case FFESTR_secondBLOCK:
    case FFESTR_secondBLOCKDATA:
      if (!ffesta_is_inhibited ())
	ffestc_R1112 (ffesta_tokens[1]);
      break;

#if FFESTR_F90
    case FFESTR_secondINTERFACE:
      if (!ffesta_is_inhibited ())
	ffestc_R1203 ();
      break;
#endif

    case FFESTR_secondFUNCTION:
      if (!ffesta_is_inhibited ())
	ffestc_R1221 (ffesta_tokens[1]);
      break;

    case FFESTR_secondSUBROUTINE:
      if (!ffesta_is_inhibited ())
	ffestc_R1225 (ffesta_tokens[1]);
      break;

#if FFESTR_VXT
    case FFESTR_secondSTRUCTURE:
      if (!ffesta_is_inhibited ())
	ffestc_V004 ();
      break;
#endif

#if FFESTR_VXT
    case FFESTR_secondUNION:
      if (!ffesta_is_inhibited ())
	ffestc_V010 ();
      break;
#endif

#if FFESTR_VXT
    case FFESTR_secondMAP:
      if (!ffesta_is_inhibited ())
	ffestc_V013 ();
      break;
#endif

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "END", ffesta_tokens[0]);
      if (ffesta_tokens[1] != NULL)
	ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffelex_swallow_tokens (t,
					       (ffelexHandler) ffesta_zero);
    }

  if (ffesta_tokens[1] != NULL)
    ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffesta_zero (t);
}

/* ffestb_goto -- Parse the GOTO statement

   return ffestb_goto;	// to lexer

   Make sure the statement has a valid form for the GOTO statement.  If it
   does, implement the statement.  */

ffelexHandler
ffestb_goto (ffelexToken t)
{
  ffeTokenLength i;
  unsigned const char *p;
  ffelexHandler next;
  ffelexToken nt;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      switch (ffesta_first_kw)
	{
	case FFESTR_firstGO:
	  if ((ffelex_token_type (t) != FFELEX_typeNAME)
	      || (ffesta_second_kw != FFESTR_secondTO))
	    goto bad_1;		/* :::::::::::::::::::: */
	  ffesta_confirmed ();
	  return (ffelexHandler) ffestb_goto1_;

	case FFESTR_firstGOTO:
	  return (ffelexHandler) ffestb_goto1_ (t);

	default:
	  goto bad_0;		/* :::::::::::::::::::: */
	}

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstGOTO)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeOPEN_PAREN:
	case FFELEX_typePERCENT:	/* Since GOTO I%J is apparently valid
					   in '90. */
	case FFELEX_typeCOMMA:
	  break;

	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  ffesta_confirmed ();
	  break;
	}
      if (ffelex_token_length (ffesta_tokens[0]) != FFESTR_firstlGOTO)
	{
	  p = ffelex_token_text (ffesta_tokens[0]) + (i = FFESTR_firstlGOTO);
	  if (ISDIGIT (*p))
	    {
	      nt = ffelex_token_number_from_names (ffesta_tokens[0], i);
	      p += ffelex_token_length (nt);
	      i += ffelex_token_length (nt);
	      if (*p != '\0')
		{
		  ffelex_token_kill (nt);
		  goto bad_i;	/* :::::::::::::::::::: */
		}
	    }
	  else if (ffesrc_is_name_init (*p))
	    {
	      nt = ffelex_token_name_from_names (ffesta_tokens[0], i, 0);
	    }
	  else
	    goto bad_i;		/* :::::::::::::::::::: */
	  next = (ffelexHandler) ffestb_goto1_ (nt);
	  ffelex_token_kill (nt);
	  return (ffelexHandler) (*next) (t);
	}
      return (ffelexHandler) ffestb_goto1_ (t);

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "GO TO", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "GO TO", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */

bad_i:				/* :::::::::::::::::::: */
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, "GO TO", ffesta_tokens[0], i, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_goto1_ -- "GOTO" or "GO" "TO"

   return ffestb_goto1_;  // to lexer

   Make sure the statement has a valid form for the GOTO statement.  If it
   does, implement the statement.  */

static ffelexHandler
ffestb_goto1_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNUMBER:
      if (ffelex_token_type (ffesta_tokens[0]) == FFELEX_typeNAME)
	ffesta_confirmed ();
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_goto2_;

    case FFELEX_typeOPEN_PAREN:
      ffesta_tokens[1] = ffelex_token_use (t);
      ffestb_subrargs_.label_list.labels = ffestt_tokenlist_create ();
      ffestb_subrargs_.label_list.handler = (ffelexHandler) ffestb_goto3_;
      return (ffelexHandler) ffestb_subr_label_list_;

    case FFELEX_typeNAME:
      if (ffelex_token_type (ffesta_tokens[0]) == FFELEX_typeNAME)
	ffesta_confirmed ();
      return (ffelexHandler) (*((ffelexHandler)
				ffeexpr_lhs (ffesta_output_pool,
					     FFEEXPR_contextAGOTO,
					  (ffeexprCallback) ffestb_goto4_)))
	(t);

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
    case FFELEX_typeCOMMA:
    case FFELEX_typeCOLONCOLON:
      ffesta_confirmed ();	/* Error, but clearly intended. */
      break;

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "GO TO", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_goto2_ -- "GO/TO" NUMBER

   return ffestb_goto2_;  // to lexer

   Make sure the statement has a valid form for the GOTO statement.  If it
   does, implement the statement.  */

static ffelexHandler
ffestb_goto2_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	ffestc_R836 (ffesta_tokens[1]);
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "GO TO", t);
  ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_goto3_ -- "GO/TO" OPEN_PAREN label-list CLOSE_PAREN

   return ffestb_goto3_;  // to lexer

   Make sure the statement has a valid form for the GOTO statement.  If it
   does, implement the statement.  */

static ffelexHandler
ffestb_goto3_ (ffelexToken t)
{
  if (!ffestb_subrargs_.label_list.ok)
    goto bad;			/* :::::::::::::::::::: */

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      ffesta_confirmed ();
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool, FFEEXPR_contextCGOTO,
					  (ffeexprCallback) ffestb_goto5_);

    case FFELEX_typeEQUALS:
    case FFELEX_typePOINTS:
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      break;

    default:
      ffesta_confirmed ();
      /* Fall through. */
    case FFELEX_typeOPEN_PAREN:	/* Could still be assignment!! */
      return (ffelexHandler) (*((ffelexHandler)
		      ffeexpr_rhs (ffesta_output_pool, FFEEXPR_contextCGOTO,
				   (ffeexprCallback) ffestb_goto5_)))
	(t);
    }

bad:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "computed-GOTO", t);
  ffelex_token_kill (ffesta_tokens[1]);
  ffestt_tokenlist_kill (ffestb_subrargs_.label_list.labels);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_goto4_ -- "GO/TO" expr

   (ffestb_goto4_)  // to expression handler

   Make sure the statement has a valid form for the GOTO statement.  If it
   does, implement the statement.  */

static ffelexHandler
ffestb_goto4_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      ffesta_confirmed ();
      if (expr == NULL)
	break;
      ffesta_tokens[1] = ffelex_token_use (ft);
      ffestb_local_.go_to.expr = expr;
      return (ffelexHandler) ffestb_goto6_;

    case FFELEX_typeOPEN_PAREN:
      if (expr == NULL)
	break;
      ffesta_tokens[1] = ffelex_token_use (ft);
      ffestb_local_.go_to.expr = expr;
      return (ffelexHandler) ffestb_goto6_ (t);

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (expr == NULL)
	break;
      if (!ffesta_is_inhibited ())
	ffestc_R839 (expr, ft, NULL);
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "assigned-GOTO", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_goto5_ -- "GO/TO" OPEN_PAREN label-list CLOSE_PAREN (COMMA) expr

   (ffestb_goto5_)  // to expression handler

   Make sure the statement has a valid form for the GOTO statement.  If it
   does, implement the statement.  */

static ffelexHandler
ffestb_goto5_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (expr == NULL)
	break;
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	ffestc_R837 (ffestb_subrargs_.label_list.labels, expr, ft);
      ffelex_token_kill (ffesta_tokens[1]);
      ffestt_tokenlist_kill (ffestb_subrargs_.label_list.labels);
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "computed-GOTO", t);
  ffelex_token_kill (ffesta_tokens[1]);
  ffestt_tokenlist_kill (ffestb_subrargs_.label_list.labels);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_goto6_ -- "GO/TO" expr (COMMA)

   return ffestb_goto6_;  // to lexer

   Make sure the statement has a valid form for the GOTO statement.  If it
   does, implement the statement.  */

static ffelexHandler
ffestb_goto6_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeOPEN_PAREN:
      ffesta_tokens[2] = ffelex_token_use (t);
      ffestb_subrargs_.label_list.labels = ffestt_tokenlist_create ();
      ffestb_subrargs_.label_list.handler = (ffelexHandler) ffestb_goto7_;
      return (ffelexHandler) ffestb_subr_label_list_;

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "assigned-GOTO", t);
  ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_goto7_ -- "GO/TO" expr (COMMA) OPEN_PAREN label-list CLOSE_PAREN

   return ffestb_goto7_;  // to lexer

   Make sure the statement has a valid form for the GOTO statement.  If it
   does, implement the statement.  */

static ffelexHandler
ffestb_goto7_ (ffelexToken t)
{
  if (!ffestb_subrargs_.label_list.ok)
    goto bad;			/* :::::::::::::::::::: */

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	ffestc_R839 (ffestb_local_.go_to.expr, ffesta_tokens[1],
		     ffestb_subrargs_.label_list.labels);
      ffelex_token_kill (ffesta_tokens[1]);
      ffelex_token_kill (ffesta_tokens[2]);
      ffestt_tokenlist_kill (ffestb_subrargs_.label_list.labels);
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

bad:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "assigned-GOTO", t);
  ffelex_token_kill (ffesta_tokens[1]);
  ffelex_token_kill (ffesta_tokens[2]);
  ffestt_tokenlist_kill (ffestb_subrargs_.label_list.labels);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_halt -- Parse the STOP/PAUSE statement

   return ffestb_halt;	// to lexer

   Make sure the statement has a valid form for the STOP/PAUSE statement.  If
   it does, implement the statement.  */

ffelexHandler
ffestb_halt (ffelexToken t)
{
  ffelexHandler next;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	case FFELEX_typeNAME:
	case FFELEX_typeNUMBER:
	case FFELEX_typeAPOSTROPHE:
	case FFELEX_typeQUOTE:
	  ffesta_confirmed ();
	  break;
	}

      return (ffelexHandler) (*((ffelexHandler)
				ffeexpr_rhs (ffesta_output_pool,
					     FFEEXPR_contextSTOP,
					  (ffeexprCallback) ffestb_halt1_)))
	(t);

    case FFELEX_typeNAMES:
      switch (ffelex_token_type (t))
	{
	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	case FFELEX_typeNAME:
	case FFELEX_typeNUMBER:
	case FFELEX_typeAPOSTROPHE:
	case FFELEX_typeQUOTE:
	  ffesta_confirmed ();
	  break;
	}
      next = (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
					  FFEEXPR_contextSTOP,
					  (ffeexprCallback) ffestb_halt1_);
      next = (ffelexHandler) ffelex_splice_tokens (next, ffesta_tokens[0],
						   ffestb_args.halt.len);
      if (next == NULL)
	return (ffelexHandler) ffelex_swallow_tokens (t,
					       (ffelexHandler) ffesta_zero);
      return (ffelexHandler) (*next) (t);

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM,
		     (ffesta_first_kw == FFESTR_firstSTOP)
		     ? "STOP" : "PAUSE",
		     ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM,
		     (ffesta_first_kw == FFESTR_firstSTOP)
		     ? "STOP" : "PAUSE",
		     t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */
}

/* ffestb_halt1_ -- "STOP/PAUSE" expr

   (ffestb_halt1_)  // to expression handler

   Make sure the next token is an EOS or SEMICOLON.  */

static ffelexHandler
ffestb_halt1_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	{
	  if (ffesta_first_kw == FFESTR_firstSTOP)
	    ffestc_R842 (expr, ft);
	  else
	    ffestc_R843 (expr, ft);
	}
      return (ffelexHandler) ffesta_zero (t);

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM,
			 (ffesta_first_kw == FFESTR_firstSTOP)
			 ? "STOP" : "PAUSE",
			 t);
      break;
    }

  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_if -- Parse an IF statement

   return ffestb_if;  // to lexer

   Make sure the statement has a valid form for an IF statement.
   If it does, implement the statement.	 */

ffelexHandler
ffestb_if (ffelexToken t)
{
  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstIF)
	goto bad_0;		/* :::::::::::::::::::: */
      break;

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstIF)
	goto bad_0;		/* :::::::::::::::::::: */
      if (ffelex_token_length (ffesta_tokens[0]) != FFESTR_firstlIF)
	goto bad_0;		/* :::::::::::::::::::: */
      break;

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeOPEN_PAREN:
      break;

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
    case FFELEX_typeCOMMA:
    case FFELEX_typeCOLONCOLON:
      ffesta_confirmed ();	/* Error, but clearly intended. */
      goto bad_1;		/* :::::::::::::::::::: */

    default:
      goto bad_1;		/* :::::::::::::::::::: */
    }

  return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool, FFEEXPR_contextIF,
				      (ffeexprCallback) ffestb_if1_);

bad_0:				/* :::::::::::::::::::: */
  if (ffesta_construct_name != NULL)
    {
      ffelex_token_kill (ffesta_construct_name);
      ffesta_construct_name = NULL;
    }
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "IF", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  if (ffesta_construct_name != NULL)
    {
      ffelex_token_kill (ffesta_construct_name);
      ffesta_construct_name = NULL;
    }
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "IF", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */
}

/* ffestb_if1_ -- "IF" OPEN_PAREN expr

   (ffestb_if1_)  // to expression handler

   Make sure the next token is CLOSE_PAREN.  */

static ffelexHandler
ffestb_if1_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  ffestb_local_.if_stmt.expr = expr;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCLOSE_PAREN:
      if (expr == NULL)
	break;
      ffesta_tokens[1] = ffelex_token_use (ft);
      ffelex_set_names (TRUE);
      return (ffelexHandler) ffestb_if2_;

    default:
      break;
    }

  if (ffesta_construct_name != NULL)
    {
      ffelex_token_kill (ffesta_construct_name);
      ffesta_construct_name = NULL;
    }
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "IF", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_if2_ -- "IF" OPEN_PAREN expr CLOSE_PAREN

   return ffestb_if2_;	// to lexer

   Make sure the next token is NAME.  */

static ffelexHandler
ffestb_if2_ (ffelexToken t)
{
  ffelex_set_names (FALSE);

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
    case FFELEX_typeNAMES:
      ffesta_confirmed ();
      ffesta_tokens[2] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_if3_;

    default:
      break;
    }

  ffelex_token_kill (ffesta_tokens[1]);
  if ((ffesta_construct_name == NULL)
      || (ffelex_token_type (t) != FFELEX_typeNUMBER))
    ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "IF", t);
  else
    ffesta_ffebad_2st (FFEBAD_INVALID_STMT_FORM, "CONSTRUCT",
		       ffesta_construct_name, t);
  if (ffesta_construct_name != NULL)
    {
      ffelex_token_kill (ffesta_construct_name);
      ffesta_construct_name = NULL;
    }
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_if3_ -- "IF" OPEN_PAREN expr CLOSE_PAREN NAME

   return ffestb_if3_;	// to lexer

   If the next token is EOS or SEMICOLON and the preceding NAME was "THEN",
   implement R803.  Else, implement R807 and send the preceding NAME followed
   by the current token.  */

static ffelexHandler
ffestb_if3_ (ffelexToken t)
{
  ffelexHandler next;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (ffestr_first (ffesta_tokens[2]) == FFESTR_firstTHEN)
	{
	  if (!ffesta_is_inhibited ())
	    ffestc_R803 (ffesta_construct_name, ffestb_local_.if_stmt.expr,
			 ffesta_tokens[1]);
	  ffelex_token_kill (ffesta_tokens[1]);
	  ffelex_token_kill (ffesta_tokens[2]);
	  if (ffesta_construct_name != NULL)
	    {
	      ffelex_token_kill (ffesta_construct_name);
	      ffesta_construct_name = NULL;
	    }
	  return (ffelexHandler) ffesta_zero (t);
	}
      break;

    default:
      break;
    }

  if (ffesta_construct_name != NULL)
    {
      if (!ffesta_is_inhibited ())
	ffesta_ffebad_2st (FFEBAD_INVALID_STMT_FORM, "CONSTRUCT",
			   ffesta_construct_name, ffesta_tokens[2]);
      ffelex_token_kill (ffesta_construct_name);
      ffesta_construct_name = NULL;
      ffelex_token_kill (ffesta_tokens[1]);
      ffelex_token_kill (ffesta_tokens[2]);
      return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
    }

  if (!ffesta_is_inhibited ())
    ffestc_R807 (ffestb_local_.if_stmt.expr, ffesta_tokens[1]);
  ffelex_token_kill (ffesta_tokens[1]);
  {
    ffelexToken my_2 = ffesta_tokens[2];

    next = (ffelexHandler) ffesta_two (my_2, t);
    ffelex_token_kill (my_2);
  }
  return (ffelexHandler) next;
}

/* ffestb_where -- Parse a WHERE statement

   return ffestb_where;	 // to lexer

   Make sure the statement has a valid form for a WHERE statement.
   If it does, implement the statement.	 */

#if FFESTR_F90
ffelexHandler
ffestb_where (ffelexToken t)
{
  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstWHERE)
	goto bad_0;		/* :::::::::::::::::::: */
      break;

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstWHERE)
	goto bad_0;		/* :::::::::::::::::::: */
      if (ffelex_token_length (ffesta_tokens[0]) != FFESTR_firstlWHERE)
	goto bad_0;		/* :::::::::::::::::::: */
      break;

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeOPEN_PAREN:
      break;

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
    case FFELEX_typeCOMMA:
    case FFELEX_typeCOLONCOLON:
      ffesta_confirmed ();	/* Error, but clearly intended. */
      goto bad_1;		/* :::::::::::::::::::: */

    default:
      goto bad_1;		/* :::::::::::::::::::: */
    }

  return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool, FFEEXPR_contextWHERE,
				      (ffeexprCallback) ffestb_where1_);

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "WHERE", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "WHERE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */
}

#endif
/* ffestb_where1_ -- "WHERE" OPEN_PAREN expr

   (ffestb_where1_)  // to expression handler

   Make sure the next token is CLOSE_PAREN.  */

#if FFESTR_F90
static ffelexHandler
ffestb_where1_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  ffestb_local_.if_stmt.expr = expr;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCLOSE_PAREN:
      if (expr == NULL)
	break;
      ffesta_tokens[1] = ffelex_token_use (ft);
      ffelex_set_names (TRUE);
      return (ffelexHandler) ffestb_where2_;

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "WHERE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

#endif
/* ffestb_where2_ -- "WHERE" OPEN_PAREN expr CLOSE_PAREN

   return ffestb_where2_;  // to lexer

   Make sure the next token is NAME.  */

#if FFESTR_F90
static ffelexHandler
ffestb_where2_ (ffelexToken t)
{
  ffelex_set_names (FALSE);

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
    case FFELEX_typeNAMES:
      ffesta_confirmed ();
      ffesta_tokens[2] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_where3_;

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	ffestc_R742 (ffestb_local_.if_stmt.expr, ffesta_tokens[1]);
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "WHERE", t);
  ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

#endif
/* ffestb_where3_ -- "WHERE" OPEN_PAREN expr CLOSE_PAREN NAME

   return ffestb_where3_;  // to lexer

   Implement R742.  */

#if FFESTR_F90
static ffelexHandler
ffestb_where3_ (ffelexToken t)
{
  ffelexHandler next;
  ffelexToken my_2 = ffesta_tokens[2];

  if (!ffesta_is_inhibited ())
    ffestc_R740 (ffestb_local_.if_stmt.expr, ffesta_tokens[1]);
  ffelex_token_kill (ffesta_tokens[1]);
  next = (ffelexHandler) ffesta_two (my_2, t);
  ffelex_token_kill (my_2);
  return (ffelexHandler) next;
}

#endif
/* ffestb_let -- Parse an assignment statement

   return ffestb_let;  // to lexer

   Make sure the statement has a valid form for an assignment statement.  If
   it does, implement the statement.  */

ffelexHandler
ffestb_let (ffelexToken t)
{
  ffelexHandler next;
  bool vxtparam;		/* TRUE if it might really be a VXT PARAMETER
				   stmt. */
  unsigned const char *p;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      vxtparam = FALSE;
      break;

    case FFELEX_typeNAMES:
      vxtparam = TRUE;
      break;

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeOPEN_PAREN:
    case FFELEX_typePERCENT:
    case FFELEX_typePOINTS:
      ffestb_local_.let.vxtparam = FALSE;
      break;

    case FFELEX_typeEQUALS:
      if (!vxtparam || (ffesta_first_kw != FFESTR_firstPARAMETER))
	{
	  ffestb_local_.let.vxtparam = FALSE;
	  break;
	}
      p = ffelex_token_text (ffesta_tokens[0]) + FFESTR_firstlPARAMETER;
      ffestb_local_.let.vxtparam = ffesrc_is_name_init (*p);
      break;

    default:
      goto bad_1;		/* :::::::::::::::::::: */
    }

  next = (ffelexHandler) (*((ffelexHandler)
			    ffeexpr_lhs (ffesta_output_pool,
					 FFEEXPR_contextLET,
					 (ffeexprCallback) ffestb_let1_)))
    (ffesta_tokens[0]);
  return (ffelexHandler) (*next) (t);

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "assignment", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "assignment", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */
}

/* ffestb_let1_ -- expr

   (ffestb_let1_)  // to expression handler

   Make sure the next token is EQUALS or POINTS.  */

static ffelexHandler
ffestb_let1_ (ffelexToken ft UNUSED, ffebld expr, ffelexToken t)
{
  ffestb_local_.let.dest = expr;

  switch (ffelex_token_type (t))
    {
#if FFESTR_F90
    case FFELEX_typePOINTS:
#endif
    case FFELEX_typeEQUALS:
      if (expr == NULL)
	break;
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
			FFEEXPR_contextLET, (ffeexprCallback) ffestb_let2_);

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "assignment", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_let2_ -- expr EQUALS/POINTS expr

   (ffestb_end2_)  // to expression handler

   Make sure the next token is EOS or SEMICOLON; implement the statement.  */

static ffelexHandler
ffestb_let2_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (expr == NULL)
	break;
      if (ffestb_local_.let.vxtparam && !ffestc_is_let_not_V027 ())
	break;
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
#if FFESTR_F90
	if (ffelex_token_type (ffesta_tokens[1]) == FFELEX_typeEQUALS)
#endif
	  ffestc_let (ffestb_local_.let.dest, expr, ft);
#if FFESTR_F90
	else
	  ffestc_R738 (ffestb_local_.let.dest, expr, ft);
#endif
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM,
		     (ffelex_token_type (ffesta_tokens[1]) == FFELEX_typeEQUALS)
		     ? "assignment" : "pointer-assignment",
		     t);
  ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_type -- Parse the TYPE statement

   return ffestb_type;	// to lexer

   Make sure the statement has a valid form for the TYPE statement.  If
   it does, implement the statement.  */

#if FFESTR_F90
ffelexHandler
ffestb_type (ffelexToken t)
{
  ffeTokenLength i;
  const char *p;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstTYPE)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeCOMMA:
	  ffesta_confirmed ();
	  return (ffelexHandler) ffestb_type1_;

	case FFELEX_typeNAME:	/* No confirm here, because ambig w/V020 VXT
				   TYPE. */
	  ffesta_tokens[1] = NULL;
	  ffesta_tokens[2] = ffelex_token_use (t);
	  return (ffelexHandler) ffestb_type4_;
	}

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstTYPE)
	goto bad_0;		/* :::::::::::::::::::: */
      p = ffelex_token_text (ffesta_tokens[0]) + (i = FFESTR_firstlTYPE);
      switch (ffelex_token_type (t))
	{
	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeCOMMA:
	  if (*p != '\0')
	    goto bad_i;		/* :::::::::::::::::::: */
	  ffesta_confirmed ();
	  ffelex_set_names (TRUE);
	  return (ffelexHandler) ffestb_type1_;

	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  break;
	}
      if (!ffesrc_is_name_init (*p))
	goto bad_i;		/* :::::::::::::::::::: */
      ffesta_tokens[1] = NULL;
      ffesta_tokens[2]
	= ffelex_token_name_from_names (ffesta_tokens[0], i, 0);
      return (ffelexHandler) ffestb_type4_ (t);

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "TYPE", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "TYPE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */

bad_i:				/* :::::::::::::::::::: */
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, "TYPE", ffesta_tokens[0], i, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_type1_ -- "TYPE" COMMA

   return ffestb_type1_;  // to lexer

   Make sure the next token is a NAME.	*/

static ffelexHandler
ffestb_type1_ (ffelexToken t)
{
  ffeTokenLength i;
  const char *p;

  ffelex_set_names (FALSE);

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_tokens[1] = ffelex_token_use (t);
      ffestb_local_.type.kw = ffestr_other (t);
      switch (ffestb_local_.varlist.kw)
	{
	case FFESTR_otherPUBLIC:
	case FFESTR_otherPRIVATE:
	  return (ffelexHandler) ffestb_type2_;

	default:
	  ffelex_token_kill (ffesta_tokens[1]);
	  break;
	}
      break;

    case FFELEX_typeNAMES:
      ffesta_tokens[1] = ffelex_token_use (t);
      ffestb_local_.type.kw = ffestr_other (t);
      switch (ffestb_local_.varlist.kw)
	{
	case FFESTR_otherPUBLIC:
	  p = ffelex_token_text (t) + (i = FFESTR_otherlPUBLIC);
	  if (*p == '\0')
	    return (ffelexHandler) ffestb_type2_;
	  if (!ffesrc_is_name_init (*p))
	    goto bad_i1;	/* :::::::::::::::::::: */
	  ffesta_tokens[2] = ffelex_token_name_from_names (t, i, 0);
	  return (ffelexHandler) ffestb_type4_;

	case FFESTR_otherPRIVATE:
	  p = ffelex_token_text (t) + (i = FFESTR_otherlPRIVATE);
	  if (*p == '\0')
	    return (ffelexHandler) ffestb_type2_;
	  if (!ffesrc_is_name_init (*p))
	    goto bad_i1;	/* :::::::::::::::::::: */
	  ffesta_tokens[2] = ffelex_token_name_from_names (t, i, 0);
	  return (ffelexHandler) ffestb_type4_;

	default:
	  ffelex_token_kill (ffesta_tokens[1]);
	  break;
	}
      break;

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "TYPE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_i1:			/* :::::::::::::::::::: */
  ffelex_token_kill (ffesta_tokens[1]);
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, "TYPE", t, i, NULL);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_type2_ -- "TYPE" COMMA NAME

   return ffestb_type2_;  // to lexer

   Handle COLONCOLON or NAME.  */

static ffelexHandler
ffestb_type2_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOLONCOLON:
      return (ffelexHandler) ffestb_type3_;

    case FFELEX_typeNAME:
      return (ffelexHandler) ffestb_type3_ (t);

    default:
      break;
    }

  if (ffesta_tokens[1] != NULL)
    ffelex_token_kill (ffesta_tokens[1]);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "TYPE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_type3_ -- "TYPE" [COMMA NAME [COLONCOLON]]

   return ffestb_type3_;  // to lexer

   Make sure the next token is a NAME.	*/

static ffelexHandler
ffestb_type3_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_tokens[2] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_type4_;

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "TYPE", t);
      break;
    }

  if (ffesta_tokens[1] != NULL)
    ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_type4_ -- "TYPE" [COMMA NAME [COLONCOLON]] NAME

   return ffestb_type4_;  // to lexer

   Make sure the next token is an EOS or SEMICOLON.  */

static ffelexHandler
ffestb_type4_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	ffestc_R424 (ffesta_tokens[1], ffestb_local_.type.kw,
		     ffesta_tokens[2]);
      if (ffesta_tokens[1] != NULL)
	ffelex_token_kill (ffesta_tokens[1]);
      ffelex_token_kill (ffesta_tokens[2]);
      return (ffelexHandler) ffesta_zero (t);

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "TYPE", t);
      break;
    }

  if (ffesta_tokens[1] != NULL)
    ffelex_token_kill (ffesta_tokens[1]);
  ffelex_token_kill (ffesta_tokens[2]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

#endif
/* ffestb_varlist -- Parse EXTERNAL/INTENT/INTRINSIC/OPTIONAL/PUBLIC/PRIVATE
		     statement

   return ffestb_varlist;  // to lexer

   Make sure the statement has a valid form.  If it
   does, implement the statement.  */

ffelexHandler
ffestb_varlist (ffelexToken t)
{
  ffeTokenLength i;
  unsigned const char *p;
  ffelexToken nt;
  ffelexHandler next;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  ffesta_confirmed ();
	  switch (ffesta_first_kw)
	    {
#if FFESTR_F90
	    case FFESTR_firstPUBLIC:
	      if (!ffesta_is_inhibited ())
		ffestc_R521A ();
	      return (ffelexHandler) ffesta_zero (t);

	    case FFESTR_firstPRIVATE:
	      if (!ffesta_is_inhibited ())
		ffestc_private ();	/* Either R523A or R521B. */
	      return (ffelexHandler) ffesta_zero (t);
#endif

	    default:
	      goto bad_1;	/* :::::::::::::::::::: */
	    }

	case FFELEX_typeCOMMA:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();
	  switch (ffesta_first_kw)
	    {
#if FFESTR_F90
	    case FFESTR_firstOPTIONAL:
	      if (!ffesta_is_inhibited ())
		ffestc_R520_start ();
	      break;

	    case FFESTR_firstPUBLIC:
	      if (!ffesta_is_inhibited ())
		ffestc_R521Astart ();
	      break;

	    case FFESTR_firstPRIVATE:
	      if (!ffesta_is_inhibited ())
		ffestc_R521Bstart ();
	      break;
#endif

	    default:
	      ffesta_confirmed ();	/* Error, but clearly intended. */
	      goto bad_1;	/* :::::::::::::::::::: */
	    }
	  return (ffelexHandler) ffestb_varlist5_;

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeOPEN_PAREN:
	  switch (ffesta_first_kw)
	    {
#if FFESTR_F90
	    case FFESTR_firstINTENT:
	      return (ffelexHandler) ffestb_varlist1_;
#endif

	    default:
	      goto bad_1;	/* :::::::::::::::::::: */
	    }

	case FFELEX_typeNAME:
	  ffesta_confirmed ();
	  switch (ffesta_first_kw)
	    {
	    case FFESTR_firstEXTERNAL:
	      if (!ffesta_is_inhibited ())
		ffestc_R1207_start ();
	      break;

#if FFESTR_F90
	    case FFESTR_firstINTENT:
	      goto bad_1;	/* :::::::::::::::::::: */
#endif

	    case FFESTR_firstINTRINSIC:
	      if (!ffesta_is_inhibited ())
		ffestc_R1208_start ();
	      break;

#if FFESTR_F90
	    case FFESTR_firstOPTIONAL:
	      if (!ffesta_is_inhibited ())
		ffestc_R520_start ();
	      break;
#endif

#if FFESTR_F90
	    case FFESTR_firstPUBLIC:
	      if (!ffesta_is_inhibited ())
		ffestc_R521Astart ();
	      break;

	    case FFESTR_firstPRIVATE:
	      if (!ffesta_is_inhibited ())
		ffestc_R521Bstart ();
	      break;
#endif

	    default:
	      break;
	    }
	  return (ffelexHandler) ffestb_varlist5_ (t);
	}

    case FFELEX_typeNAMES:
      p = ffelex_token_text (ffesta_tokens[0]) + (i = ffestb_args.varlist.len);
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  ffesta_confirmed ();
	  switch (ffesta_first_kw)
	    {
#if FFESTR_F90
	    case FFESTR_firstINTENT:
	      goto bad_1;	/* :::::::::::::::::::: */
#endif

	    default:
	      break;
	    }
	  if (*p != '\0')
	    break;
	  switch (ffesta_first_kw)
	    {
#if FFESTR_F90
	    case FFESTR_firstPUBLIC:
	      if (!ffesta_is_inhibited ())
		ffestc_R521A ();
	      return (ffelexHandler) ffesta_zero (t);

	    case FFESTR_firstPRIVATE:
	      if (!ffesta_is_inhibited ())
		ffestc_private ();	/* Either R423A or R521B. */
	      return (ffelexHandler) ffesta_zero (t);
#endif

	    default:
	      goto bad_1;	/* :::::::::::::::::::: */
	    }

	case FFELEX_typeCOMMA:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  switch (ffesta_first_kw)
	    {
#if FFESTR_F90
	    case FFESTR_firstINTENT:
	      goto bad_1;	/* :::::::::::::::::::: */
#endif

	    default:
	      break;
	    }
	  if (*p != '\0')
	    break;
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();
	  switch (ffesta_first_kw)
	    {
#if FFESTR_F90
	    case FFESTR_firstOPTIONAL:
	      if (!ffesta_is_inhibited ())
		ffestc_R520_start ();
	      break;
#endif

#if FFESTR_F90
	    case FFESTR_firstPUBLIC:
	      if (!ffesta_is_inhibited ())
		ffestc_R521Astart ();
	      break;

	    case FFESTR_firstPRIVATE:
	      if (!ffesta_is_inhibited ())
		ffestc_R521Bstart ();
	      break;
#endif

	    default:
	      goto bad_1;	/* :::::::::::::::::::: */
	    }
	  return (ffelexHandler) ffestb_varlist5_;

	case FFELEX_typeOPEN_PAREN:
	  switch (ffesta_first_kw)
	    {
#if FFESTR_F90
	    case FFESTR_firstINTENT:
	      if (*p != '\0')
		goto bad_1;	/* :::::::::::::::::::: */
	      return (ffelexHandler) ffestb_varlist1_;
#endif

	    default:
	      goto bad_1;	/* :::::::::::::::::::: */
	    }

	case FFELEX_typeNAME:
	  ffesta_confirmed ();
	  switch (ffesta_first_kw)
	    {
	    case FFESTR_firstEXTERNAL:
	      if (!ffesta_is_inhibited ())
		ffestc_R1207_start ();
	      break;

#if FFESTR_F90
	    case FFESTR_firstINTENT:
	      goto bad_1;	/* :::::::::::::::::::: */
#endif

	    case FFESTR_firstINTRINSIC:
	      if (!ffesta_is_inhibited ())
		ffestc_R1208_start ();
	      break;

#if FFESTR_F90
	    case FFESTR_firstOPTIONAL:
	      if (!ffesta_is_inhibited ())
		ffestc_R520_start ();
	      break;
#endif

#if FFESTR_F90
	    case FFESTR_firstPUBLIC:
	      if (!ffesta_is_inhibited ())
		ffestc_R521Astart ();
	      break;

	    case FFESTR_firstPRIVATE:
	      if (!ffesta_is_inhibited ())
		ffestc_R521Bstart ();
	      break;
#endif

	    default:
	      break;
	    }
	  return (ffelexHandler) ffestb_varlist5_ (t);

	default:
	  goto bad_1;		/* :::::::::::::::::::: */
	}

      /* Here, we have at least one char after the first keyword and t is
	 COMMA or EOS/SEMICOLON.  Also we know that this form is valid for
	 only the statements reaching here (specifically, INTENT won't reach
	 here). */

      if (!ffesrc_is_name_init (*p))
	goto bad_i;		/* :::::::::::::::::::: */
      nt = ffelex_token_name_from_names (ffesta_tokens[0], i, 0);
      if (!ffesta_is_inhibited ())
	{
	  switch (ffesta_first_kw)
	    {
	    case FFESTR_firstEXTERNAL:
	      ffestc_R1207_start ();
	      break;

	    case FFESTR_firstINTRINSIC:
	      ffestc_R1208_start ();
	      break;

#if FFESTR_F90
	    case FFESTR_firstOPTIONAL:
	      ffestc_R520_start ();
	      break;
#endif

#if FFESTR_F90
	    case FFESTR_firstPUBLIC:
	      ffestc_R521Astart ();
	      break;

	    case FFESTR_firstPRIVATE:
	      ffestc_R521Bstart ();
	      break;
#endif

	    default:
	      assert (FALSE);
	    }
	}
      next = (ffelexHandler) ffestb_varlist5_ (nt);
      ffelex_token_kill (nt);
      return (ffelexHandler) (*next) (t);

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.varlist.badname, ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.varlist.badname, t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */

bad_i:				/* :::::::::::::::::::: */
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, ffestb_args.varlist.badname, ffesta_tokens[0], i, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_varlist1_ -- "INTENT" OPEN_PAREN

   return ffestb_varlist1_;  // to lexer

   Handle NAME.	 */

#if FFESTR_F90
static ffelexHandler
ffestb_varlist1_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_tokens[1] = ffelex_token_use (t);
      ffestb_local_.varlist.kw = ffestr_other (t);
      switch (ffestb_local_.varlist.kw)
	{
	case FFESTR_otherIN:
	  return (ffelexHandler) ffestb_varlist2_;

	case FFESTR_otherINOUT:
	  return (ffelexHandler) ffestb_varlist3_;

	case FFESTR_otherOUT:
	  return (ffelexHandler) ffestb_varlist3_;

	default:
	  ffelex_token_kill (ffesta_tokens[1]);
	  break;
	}
      break;

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.varlist.badname, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_varlist2_ -- "INTENT" OPEN_PAREN "IN"

   return ffestb_varlist2_;  // to lexer

   Handle NAME.	 */

static ffelexHandler
ffestb_varlist2_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      switch (ffestr_other (t))
	{
	case FFESTR_otherOUT:
	  ffestb_local_.varlist.kw = FFESTR_otherINOUT;
	  return (ffelexHandler) ffestb_varlist3_;

	default:
	  break;
	}
      break;

    case FFELEX_typeCLOSE_PAREN:
      return (ffelexHandler) ffestb_varlist4_;

    default:
      break;
    }

  ffelex_token_kill (ffesta_tokens[1]);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.varlist.badname, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_varlist3_ -- "INTENT" OPEN_PAREN NAME ["OUT"]

   return ffestb_varlist3_;  // to lexer

   Handle CLOSE_PAREN.	*/

static ffelexHandler
ffestb_varlist3_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCLOSE_PAREN:
      return (ffelexHandler) ffestb_varlist4_;

    default:
      break;
    }

  ffelex_token_kill (ffesta_tokens[1]);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.varlist.badname, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_varlist4_ -- "INTENT" OPEN_PAREN NAME ["OUT"] CLOSE_PAREN

   return ffestb_varlist4_;  // to lexer

   Handle COLONCOLON or NAME.  */

static ffelexHandler
ffestb_varlist4_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOLONCOLON:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	ffestc_R519_start (ffesta_tokens[1], ffestb_local_.varlist.kw);
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffestb_varlist5_;

    case FFELEX_typeNAME:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	ffestc_R519_start (ffesta_tokens[1], ffestb_local_.varlist.kw);
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffestb_varlist5_ (t);

    default:
      break;
    }

  ffelex_token_kill (ffesta_tokens[1]);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.varlist.badname, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

#endif
/* ffestb_varlist5_ -- Handles the list of variable names

   return ffestb_varlist5_;  // to lexer

   Handle NAME.	 */

static ffelexHandler
ffestb_varlist5_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_varlist6_;

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.varlist.badname, t);
      break;
    }

  if (!ffesta_is_inhibited ())
    {
      switch (ffesta_first_kw)
	{
	case FFESTR_firstEXTERNAL:
	  ffestc_R1207_finish ();
	  break;

#if FFESTR_F90
	case FFESTR_firstINTENT:
	  ffestc_R519_finish ();
	  break;
#endif

	case FFESTR_firstINTRINSIC:
	  ffestc_R1208_finish ();
	  break;

#if FFESTR_F90
	case FFESTR_firstOPTIONAL:
	  ffestc_R520_finish ();
	  break;
#endif

#if FFESTR_F90
	case FFESTR_firstPUBLIC:
	  ffestc_R521Afinish ();
	  break;

	case FFESTR_firstPRIVATE:
	  ffestc_R521Bfinish ();
	  break;
#endif

	default:
	  assert (FALSE);
	}
    }
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_varlist6_ -- (whatever) NAME

   return ffestb_varlist6_;  // to lexer

   Handle COMMA or EOS/SEMICOLON.  */

static ffelexHandler
ffestb_varlist6_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      if (!ffesta_is_inhibited ())
	{
	  switch (ffesta_first_kw)
	    {
	    case FFESTR_firstEXTERNAL:
	      ffestc_R1207_item (ffesta_tokens[1]);
	      break;

#if FFESTR_F90
	    case FFESTR_firstINTENT:
	      ffestc_R519_item (ffesta_tokens[1]);
	      break;
#endif

	    case FFESTR_firstINTRINSIC:
	      ffestc_R1208_item (ffesta_tokens[1]);
	      break;

#if FFESTR_F90
	    case FFESTR_firstOPTIONAL:
	      ffestc_R520_item (ffesta_tokens[1]);
	      break;
#endif

#if FFESTR_F90
	    case FFESTR_firstPUBLIC:
	      ffestc_R521Aitem (ffesta_tokens[1]);
	      break;

	    case FFESTR_firstPRIVATE:
	      ffestc_R521Bitem (ffesta_tokens[1]);
	      break;
#endif

	    default:
	      assert (FALSE);
	    }
	}
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffestb_varlist5_;

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (!ffesta_is_inhibited ())
	{
	  switch (ffesta_first_kw)
	    {
	    case FFESTR_firstEXTERNAL:
	      ffestc_R1207_item (ffesta_tokens[1]);
	      ffestc_R1207_finish ();
	      break;

#if FFESTR_F90
	    case FFESTR_firstINTENT:
	      ffestc_R519_item (ffesta_tokens[1]);
	      ffestc_R519_finish ();
	      break;
#endif

	    case FFESTR_firstINTRINSIC:
	      ffestc_R1208_item (ffesta_tokens[1]);
	      ffestc_R1208_finish ();
	      break;

#if FFESTR_F90
	    case FFESTR_firstOPTIONAL:
	      ffestc_R520_item (ffesta_tokens[1]);
	      ffestc_R520_finish ();
	      break;
#endif

#if FFESTR_F90
	    case FFESTR_firstPUBLIC:
	      ffestc_R521Aitem (ffesta_tokens[1]);
	      ffestc_R521Afinish ();
	      break;

	    case FFESTR_firstPRIVATE:
	      ffestc_R521Bitem (ffesta_tokens[1]);
	      ffestc_R521Bfinish ();
	      break;
#endif

	    default:
	      assert (FALSE);
	    }
	}
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffesta_zero (t);

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.varlist.badname, t);
      break;
    }

  if (!ffesta_is_inhibited ())
    {
      switch (ffesta_first_kw)
	{
	case FFESTR_firstEXTERNAL:
	  ffestc_R1207_finish ();
	  break;

#if FFESTR_F90
	case FFESTR_firstINTENT:
	  ffestc_R519_finish ();
	  break;
#endif

	case FFESTR_firstINTRINSIC:
	  ffestc_R1208_finish ();
	  break;

#if FFESTR_F90
	case FFESTR_firstOPTIONAL:
	  ffestc_R520_finish ();
	  break;
#endif

#if FFESTR_F90
	case FFESTR_firstPUBLIC:
	  ffestc_R521Afinish ();
	  break;

	case FFESTR_firstPRIVATE:
	  ffestc_R521Bfinish ();
	  break;
#endif

	default:
	  assert (FALSE);
	}
    }
  ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R423B -- Parse the SEQUENCE statement

   return ffestb_R423B;	 // to lexer

   Make sure the statement has a valid form for the SEQUENCE statement.	 If
   it does, implement the statement.  */

#if FFESTR_F90
ffelexHandler
ffestb_R423B (ffelexToken t)
{
  const char *p;
  ffeTokenLength i;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstSEQUENCE)
	goto bad_0;		/* :::::::::::::::::::: */
      break;

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstSEQUENCE)
	goto bad_0;		/* :::::::::::::::::::: */
      if (ffelex_token_length (ffesta_tokens[0]) != FFESTR_firstlSEQUENCE)
	{
	  p = ffelex_token_text (ffesta_tokens[0]) + (i = FFESTR_firstlSEQUENCE);
	  goto bad_i;		/* :::::::::::::::::::: */
	}
      break;

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	ffestc_R423B ();
      return (ffelexHandler) ffesta_zero (t);

    case FFELEX_typeCOMMA:
    case FFELEX_typeCOLONCOLON:
      ffesta_confirmed ();	/* Error, but clearly intended. */
      goto bad_1;		/* :::::::::::::::::::: */

    default:
      goto bad_1;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "SEQUENCE", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid first token. */

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "SEQUENCE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */

bad_i:				/* :::::::::::::::::::: */
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, "SEQUENCE", ffesta_tokens[0], i, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

#endif
/* ffestb_R522 -- Parse the SAVE statement

   return ffestb_R522;	// to lexer

   Make sure the statement has a valid form for the SAVE statement.  If it
   does, implement the statement.  */

ffelexHandler
ffestb_R522 (ffelexToken t)
{
  ffeTokenLength i;
  unsigned const char *p;
  ffelexToken nt;
  ffelexHandler next;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstSAVE)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeCOMMA:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  ffesta_confirmed ();
	  if (!ffesta_is_inhibited ())
	    ffestc_R522 ();
	  return (ffelexHandler) ffesta_zero (t);

	case FFELEX_typeNAME:
	case FFELEX_typeSLASH:
	  ffesta_confirmed ();
	  if (!ffesta_is_inhibited ())
	    ffestc_R522start ();
	  return (ffelexHandler) ffestb_R5221_ (t);

	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();
	  if (!ffesta_is_inhibited ())
	    ffestc_R522start ();
	  return (ffelexHandler) ffestb_R5221_;
	}

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstSAVE)
	goto bad_0;		/* :::::::::::::::::::: */
      p = ffelex_token_text (ffesta_tokens[0]) + (i = FFESTR_firstlSAVE);
      switch (ffelex_token_type (t))
	{
	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeCOMMA:
	  ffesta_confirmed ();
	  break;

	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  ffesta_confirmed ();
	  if (*p != '\0')
	    break;
	  if (!ffesta_is_inhibited ())
	    ffestc_R522 ();
	  return (ffelexHandler) ffesta_zero (t);

	case FFELEX_typeSLASH:
	  ffesta_confirmed ();
	  if (*p != '\0')
	    goto bad_i;		/* :::::::::::::::::::: */
	  if (!ffesta_is_inhibited ())
	    ffestc_R522start ();
	  return (ffelexHandler) ffestb_R5221_ (t);

	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();
	  if (*p != '\0')
	    goto bad_i;		/* :::::::::::::::::::: */
	  if (!ffesta_is_inhibited ())
	    ffestc_R522start ();
	  return (ffelexHandler) ffestb_R5221_;
	}

      /* Here, we have at least one char after "SAVE" and t is COMMA or
	 EOS/SEMICOLON. */

      if (!ffesrc_is_name_init (*p))
	goto bad_i;		/* :::::::::::::::::::: */
      nt = ffelex_token_name_from_names (ffesta_tokens[0], i, 0);
      if (!ffesta_is_inhibited ())
	ffestc_R522start ();
      next = (ffelexHandler) ffestb_R5221_ (nt);
      ffelex_token_kill (nt);
      return (ffelexHandler) (*next) (t);

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "SAVE", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "SAVE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */

bad_i:				/* :::::::::::::::::::: */
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, "SAVE", ffesta_tokens[0], i, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R5221_ -- "SAVE" [COLONCOLON]

   return ffestb_R5221_;  // to lexer

   Handle NAME or SLASH.  */

static ffelexHandler
ffestb_R5221_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffestb_local_.R522.is_cblock = FALSE;
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_R5224_;

    case FFELEX_typeSLASH:
      ffestb_local_.R522.is_cblock = TRUE;
      return (ffelexHandler) ffestb_R5222_;

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "SAVE", t);
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_R522finish ();
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R5222_ -- "SAVE" [COLONCOLON] SLASH

   return ffestb_R5222_;  // to lexer

   Handle NAME.	 */

static ffelexHandler
ffestb_R5222_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_R5223_;

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "SAVE", t);
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_R522finish ();
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R5223_ -- "SAVE" [COLONCOLON] SLASH NAME

   return ffestb_R5223_;  // to lexer

   Handle SLASH.  */

static ffelexHandler
ffestb_R5223_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeSLASH:
      return (ffelexHandler) ffestb_R5224_;

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "SAVE", t);
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_R522finish ();
  ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R5224_ -- "SAVE" [COLONCOLON] R523

   return ffestb_R5224_;  // to lexer

   Handle COMMA or EOS/SEMICOLON.  */

static ffelexHandler
ffestb_R5224_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      if (!ffesta_is_inhibited ())
	{
	  if (ffestb_local_.R522.is_cblock)
	    ffestc_R522item_cblock (ffesta_tokens[1]);
	  else
	    ffestc_R522item_object (ffesta_tokens[1]);
	}
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffestb_R5221_;

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (!ffesta_is_inhibited ())
	{
	  if (ffestb_local_.R522.is_cblock)
	    ffestc_R522item_cblock (ffesta_tokens[1]);
	  else
	    ffestc_R522item_object (ffesta_tokens[1]);
	  ffestc_R522finish ();
	}
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffesta_zero (t);

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "SAVE", t);
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_R522finish ();
  ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R528 -- Parse the DATA statement

   return ffestb_R528;	// to lexer

   Make sure the statement has a valid form for the DATA statement.  If it
   does, implement the statement.  */

ffelexHandler
ffestb_R528 (ffelexToken t)
{
  unsigned const char *p;
  ffeTokenLength i;
  ffelexToken nt;
  ffelexHandler next;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstDATA)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeCOMMA:
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	case FFELEX_typeSLASH:
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeNAME:
	  ffesta_confirmed ();
	  break;

	case FFELEX_typeOPEN_PAREN:
	  break;
	}
      ffestb_local_.data.started = FALSE;
      return (ffelexHandler) (*((ffelexHandler)
				ffeexpr_lhs (ffesta_output_pool,
					     FFEEXPR_contextDATA,
					  (ffeexprCallback) ffestb_R5281_)))
	(t);

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstDATA)
	goto bad_0;		/* :::::::::::::::::::: */
      p = ffelex_token_text (ffesta_tokens[0]) + (i = FFESTR_firstlDATA);
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeOPEN_PAREN:
	  if (*p == '\0')
	    {
	      ffestb_local_.data.started = FALSE;
	      return (ffelexHandler) (*((ffelexHandler)
					ffeexpr_lhs (ffesta_output_pool,
						     FFEEXPR_contextDATA,
						     (ffeexprCallback)
						     ffestb_R5281_)))
		(t);
	    }
	  break;

	case FFELEX_typeCOMMA:
	case FFELEX_typeSLASH:
	  ffesta_confirmed ();
	  break;
	}
      if (!ffesrc_is_name_init (*p))
	goto bad_i;		/* :::::::::::::::::::: */
      ffestb_local_.data.started = FALSE;
      nt = ffelex_token_name_from_names (ffesta_tokens[0], i, 0);
      next = (ffelexHandler) (*((ffelexHandler)
				ffeexpr_lhs (ffesta_output_pool,
					     FFEEXPR_contextDATA,
					  (ffeexprCallback) ffestb_R5281_)))
	(nt);
      ffelex_token_kill (nt);
      return (ffelexHandler) (*next) (t);

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "DATA", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "DATA", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */

bad_i:				/* :::::::::::::::::::: */
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, "DATA", ffesta_tokens[0], i, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R5281_ -- "DATA" expr-list

   (ffestb_R5281_)  // to expression handler

   Handle COMMA or SLASH.  */

static ffelexHandler
ffestb_R5281_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      ffesta_confirmed ();
      if (expr == NULL)
	break;
      if (!ffesta_is_inhibited ())
	{
	  if (!ffestb_local_.data.started)
	    {
	      ffestc_R528_start ();
	      ffestb_local_.data.started = TRUE;
	    }
	  ffestc_R528_item_object (expr, ft);
	}
      return (ffelexHandler) ffeexpr_lhs (ffesta_output_pool,
					  FFEEXPR_contextDATA,
					  (ffeexprCallback) ffestb_R5281_);

    case FFELEX_typeSLASH:
      ffesta_confirmed ();
      if (expr == NULL)
	break;
      if (!ffesta_is_inhibited ())
	{
	  if (!ffestb_local_.data.started)
	    {
	      ffestc_R528_start ();
	      ffestb_local_.data.started = TRUE;
	    }
	  ffestc_R528_item_object (expr, ft);
	  ffestc_R528_item_startvals ();
	}
      return (ffelexHandler) ffeexpr_rhs
	(ffesta_output_pool, FFEEXPR_contextDATA,
	 (ffeexprCallback) ffestb_R5282_);

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "DATA", t);
      break;
    }

  if (ffestb_local_.data.started && !ffesta_is_inhibited ())
    ffestc_R528_finish ();
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R5282_ -- "DATA" expr-list SLASH expr-list

   (ffestb_R5282_)  // to expression handler

   Handle ASTERISK, COMMA, or SLASH.  */

static ffelexHandler
ffestb_R5282_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      if (expr == NULL)
	break;
      if (!ffesta_is_inhibited ())
	ffestc_R528_item_value (NULL, NULL, expr, ft);
      return (ffelexHandler) ffeexpr_rhs
	(ffesta_output_pool, FFEEXPR_contextDATA,
	 (ffeexprCallback) ffestb_R5282_);

    case FFELEX_typeASTERISK:
      if (expr == NULL)
	break;
      ffestb_local_.data.expr = ffeexpr_convert (expr, ft, t,
						 FFEINFO_basictypeINTEGER,
						 FFEINFO_kindtypeINTEGER1,
						 0,
						 FFETARGET_charactersizeNONE,
						 FFEEXPR_contextLET);
      ffesta_tokens[1] = ffelex_token_use (ft);
      return (ffelexHandler) ffeexpr_rhs
	(ffesta_output_pool, FFEEXPR_contextDATA,
	 (ffeexprCallback) ffestb_R5283_);

    case FFELEX_typeSLASH:
      if (expr == NULL)
	break;
      if (!ffesta_is_inhibited ())
	{
	  ffestc_R528_item_value (NULL, NULL, expr, ft);
	  ffestc_R528_item_endvals (t);
	}
      return (ffelexHandler) ffestb_R5284_;

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "DATA", t);
      break;
    }

  if (!ffesta_is_inhibited ())
    {
      ffestc_R528_item_endvals (t);
      ffestc_R528_finish ();
    }
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R5283_ -- "DATA" expr-list SLASH expr ASTERISK expr

   (ffestb_R5283_)  // to expression handler

   Handle COMMA or SLASH.  */

static ffelexHandler
ffestb_R5283_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      if (expr == NULL)
	break;
      if (!ffesta_is_inhibited ())
	ffestc_R528_item_value (ffestb_local_.data.expr, ffesta_tokens[1],
				expr, ft);
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffeexpr_rhs
	(ffesta_output_pool, FFEEXPR_contextDATA,
	 (ffeexprCallback) ffestb_R5282_);

    case FFELEX_typeSLASH:
      if (expr == NULL)
	break;
      if (!ffesta_is_inhibited ())
	{
	  ffestc_R528_item_value (ffestb_local_.data.expr, ffesta_tokens[1],
				  expr, ft);
	  ffestc_R528_item_endvals (t);
	}
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffestb_R5284_;

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "DATA", t);
      break;
    }

  if (!ffesta_is_inhibited ())
    {
      ffestc_R528_item_endvals (t);
      ffestc_R528_finish ();
    }
  ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R5284_ -- "DATA" expr-list SLASH expr-list SLASH

   return ffestb_R5284_;  // to lexer

   Handle [COMMA] NAME or EOS/SEMICOLON.  */

static ffelexHandler
ffestb_R5284_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      return (ffelexHandler) ffeexpr_lhs (ffesta_output_pool,
					  FFEEXPR_contextDATA,
					  (ffeexprCallback) ffestb_R5281_);

    case FFELEX_typeNAME:
    case FFELEX_typeOPEN_PAREN:
      return (ffelexHandler) (*((ffelexHandler)
				ffeexpr_lhs (ffesta_output_pool,
					     FFEEXPR_contextDATA,
					  (ffeexprCallback) ffestb_R5281_)))
	(t);

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (!ffesta_is_inhibited ())
	ffestc_R528_finish ();
      return (ffelexHandler) ffesta_zero (t);

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "DATA", t);
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_R528_finish ();
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R537 -- Parse a PARAMETER statement

   return ffestb_R537;	// to lexer

   Make sure the statement has a valid form for an PARAMETER statement.
   If it does, implement the statement.	 */

ffelexHandler
ffestb_R537 (ffelexToken t)
{
  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstPARAMETER)
	goto bad_0;		/* :::::::::::::::::::: */
      break;

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstPARAMETER)
	goto bad_0;		/* :::::::::::::::::::: */
      if (ffelex_token_length (ffesta_tokens[0]) != FFESTR_firstlPARAMETER)
	goto bad_0;		/* :::::::::::::::::::: */
      break;

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeOPEN_PAREN:
      break;

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
    case FFELEX_typeCOMMA:
    case FFELEX_typeCOLONCOLON:
      ffesta_confirmed ();	/* Error, but clearly intended. */
      goto bad_1;		/* :::::::::::::::::::: */

    default:
      goto bad_1;		/* :::::::::::::::::::: */
    }

  ffestb_local_.parameter.started = FALSE;
  return (ffelexHandler) ffeexpr_lhs (ffesta_output_pool,
				      FFEEXPR_contextPARAMETER,
				      (ffeexprCallback) ffestb_R5371_);

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "PARAMETER", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "PARAMETER", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */
}

/* ffestb_R5371_ -- "PARAMETER" OPEN_PAREN expr

   (ffestb_R5371_)  // to expression handler

   Make sure the next token is EQUALS.	*/

static ffelexHandler
ffestb_R5371_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  ffestb_local_.parameter.expr = expr;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEQUALS:
      ffesta_confirmed ();
      if (expr == NULL)
	break;
      ffesta_tokens[1] = ffelex_token_use (ft);
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		 FFEEXPR_contextPARAMETER, (ffeexprCallback) ffestb_R5372_);

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "PARAMETER", t);
  if (ffestb_local_.parameter.started)
    ffestc_R537_finish ();
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R5372_ -- "PARAMETER" OPEN_PAREN expr EQUALS expr

   (ffestb_R5372_)  // to expression handler

   Make sure the next token is COMMA or CLOSE_PAREN.  */

static ffelexHandler
ffestb_R5372_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      if (expr == NULL)
	break;
      if (!ffesta_is_inhibited ())
	{
	  if (!ffestb_local_.parameter.started)
	    {
	      ffestc_R537_start ();
	      ffestb_local_.parameter.started = TRUE;
	    }
	  ffestc_R537_item (ffestb_local_.parameter.expr, ffesta_tokens[1],
			    expr, ft);
	}
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffeexpr_lhs (ffesta_output_pool,
					  FFEEXPR_contextPARAMETER,
					  (ffeexprCallback) ffestb_R5371_);

    case FFELEX_typeCLOSE_PAREN:
      if (expr == NULL)
	break;
      if (!ffesta_is_inhibited ())
	{
	  if (!ffestb_local_.parameter.started)
	    {
	      ffestc_R537_start ();
	      ffestb_local_.parameter.started = TRUE;
	    }
	  ffestc_R537_item (ffestb_local_.parameter.expr, ffesta_tokens[1],
			    expr, ft);
	  ffestc_R537_finish ();
	}
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffestb_R5373_;

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "PARAMETER", t);
  if (ffestb_local_.parameter.started)
    ffestc_R537_finish ();
  ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R5373_ -- "PARAMETER" OPEN_PAREN expr EQUALS expr CLOSE_PAREN

   return ffestb_R5373_;  // to lexer

   Make sure the next token is EOS or SEMICOLON, or generate an error.	All
   cleanup has already been done, by the way.  */

static ffelexHandler
ffestb_R5373_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "PARAMETER", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R542 -- Parse the NAMELIST statement

   return ffestb_R542;	// to lexer

   Make sure the statement has a valid form for the NAMELIST statement.	 If it
   does, implement the statement.  */

ffelexHandler
ffestb_R542 (ffelexToken t)
{
  const char *p;
  ffeTokenLength i;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstNAMELIST)
	goto bad_0;		/* :::::::::::::::::::: */
      break;

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstNAMELIST)
	goto bad_0;		/* :::::::::::::::::::: */
      p = ffelex_token_text (ffesta_tokens[0]) + (i = FFESTR_firstlNAMELIST);
      if (*p != '\0')
	goto bad_i;		/* :::::::::::::::::::: */
      break;

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
    case FFELEX_typeCOLONCOLON:
      ffesta_confirmed ();	/* Error, but clearly intended. */
      goto bad_1;		/* :::::::::::::::::::: */

    default:
      goto bad_1;		/* :::::::::::::::::::: */

    case FFELEX_typeSLASH:
      break;
    }

  ffesta_confirmed ();
  if (!ffesta_is_inhibited ())
    ffestc_R542_start ();
  return (ffelexHandler) ffestb_R5421_;

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "NAMELIST", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "NAMELIST", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */

bad_i:				/* :::::::::::::::::::: */
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, "NAMELIST", ffesta_tokens[0], i, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R5421_ -- "NAMELIST" SLASH

   return ffestb_R5421_;  // to lexer

   Handle NAME.	 */

static ffelexHandler
ffestb_R5421_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      if (!ffesta_is_inhibited ())
	ffestc_R542_item_nlist (t);
      return (ffelexHandler) ffestb_R5422_;

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "NAMELIST", t);
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_R542_finish ();
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R5422_ -- "NAMELIST" SLASH NAME

   return ffestb_R5422_;  // to lexer

   Handle SLASH.  */

static ffelexHandler
ffestb_R5422_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeSLASH:
      return (ffelexHandler) ffestb_R5423_;

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "NAMELIST", t);
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_R542_finish ();
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R5423_ -- "NAMELIST" SLASH NAME SLASH

   return ffestb_R5423_;  // to lexer

   Handle NAME.	 */

static ffelexHandler
ffestb_R5423_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      if (!ffesta_is_inhibited ())
	ffestc_R542_item_nitem (t);
      return (ffelexHandler) ffestb_R5424_;

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "NAMELIST", t);
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_R542_finish ();
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R5424_ -- "NAMELIST" SLASH NAME SLASH NAME

   return ffestb_R5424_;  // to lexer

   Handle COMMA, EOS/SEMICOLON, or SLASH.  */

static ffelexHandler
ffestb_R5424_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      return (ffelexHandler) ffestb_R5425_;

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (!ffesta_is_inhibited ())
	ffestc_R542_finish ();
      return (ffelexHandler) ffesta_zero (t);

    case FFELEX_typeSLASH:
      return (ffelexHandler) ffestb_R5421_;

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "NAMELIST", t);
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_R542_finish ();
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R5425_ -- "NAMELIST" SLASH NAME SLASH NAME COMMA

   return ffestb_R5425_;  // to lexer

   Handle NAME or SLASH.  */

static ffelexHandler
ffestb_R5425_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      if (!ffesta_is_inhibited ())
	ffestc_R542_item_nitem (t);
      return (ffelexHandler) ffestb_R5424_;

    case FFELEX_typeSLASH:
      return (ffelexHandler) ffestb_R5421_;

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "NAMELIST", t);
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_R542_finish ();
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R544 -- Parse an EQUIVALENCE statement

   return ffestb_R544;	// to lexer

   Make sure the statement has a valid form for an EQUIVALENCE statement.
   If it does, implement the statement.	 */

ffelexHandler
ffestb_R544 (ffelexToken t)
{
  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstEQUIVALENCE)
	goto bad_0;		/* :::::::::::::::::::: */
      break;

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstEQUIVALENCE)
	goto bad_0;		/* :::::::::::::::::::: */
      if (ffelex_token_length (ffesta_tokens[0]) != FFESTR_firstlEQUIVALENCE)
	goto bad_0;		/* :::::::::::::::::::: */
      break;

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeOPEN_PAREN:
      break;

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
    case FFELEX_typeCOMMA:
    case FFELEX_typeCOLONCOLON:
      ffesta_confirmed ();	/* Error, but clearly intended. */
      goto bad_1;		/* :::::::::::::::::::: */

    default:
      goto bad_1;		/* :::::::::::::::::::: */
    }

  ffestb_local_.equivalence.started = FALSE;
  return (ffelexHandler) ffeexpr_lhs (ffesta_output_pool,
				      FFEEXPR_contextEQUIVALENCE,
				      (ffeexprCallback) ffestb_R5441_);

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "EQUIVALENCE", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "EQUIVALENCE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */
}

/* ffestb_R5441_ -- "EQUIVALENCE" OPEN_PAREN expr

   (ffestb_R5441_)  // to expression handler

   Make sure the next token is COMMA.  */

static ffelexHandler
ffestb_R5441_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      if (expr == NULL)
	break;
      ffestb_local_.equivalence.exprs = ffestt_exprlist_create ();
      ffestt_exprlist_append (ffestb_local_.equivalence.exprs, expr,
			      ffelex_token_use (ft));
      return (ffelexHandler) ffeexpr_lhs (ffesta_output_pool,
					  FFEEXPR_contextEQUIVALENCE,
					  (ffeexprCallback) ffestb_R5442_);

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "EQUIVALENCE", t);
  if (ffestb_local_.equivalence.started)
    ffestc_R544_finish ();
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R5442_ -- "EQUIVALENCE" OPEN_PAREN expr COMMA expr

   (ffestb_R5442_)  // to expression handler

   Make sure the next token is COMMA or CLOSE_PAREN.  For COMMA, we just
   append the expression to our list and continue; for CLOSE_PAREN, we
   append the expression and move to _3_.  */

static ffelexHandler
ffestb_R5442_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      if (expr == NULL)
	break;
      ffestt_exprlist_append (ffestb_local_.equivalence.exprs, expr,
			      ffelex_token_use (ft));
      return (ffelexHandler) ffeexpr_lhs (ffesta_output_pool,
					  FFEEXPR_contextEQUIVALENCE,
					  (ffeexprCallback) ffestb_R5442_);

    case FFELEX_typeCLOSE_PAREN:
      if (expr == NULL)
	break;
      ffestt_exprlist_append (ffestb_local_.equivalence.exprs, expr,
			      ffelex_token_use (ft));
      return (ffelexHandler) ffestb_R5443_;

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "EQUIVALENCE", t);
  if (ffestb_local_.equivalence.started)
    ffestc_R544_finish ();
  ffestt_exprlist_kill (ffestb_local_.equivalence.exprs);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R5443_ -- "EQUIVALENCE" OPEN_PAREN expr COMMA expr CLOSE_PAREN

   return ffestb_R5443_;  // to lexer

   Make sure the next token is COMMA or EOS/SEMICOLON.	*/

static ffelexHandler
ffestb_R5443_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	{
	  if (!ffestb_local_.equivalence.started)
	    {
	      ffestc_R544_start ();
	      ffestb_local_.equivalence.started = TRUE;
	    }
	  ffestc_R544_item (ffestb_local_.equivalence.exprs);
	}
      ffestt_exprlist_kill (ffestb_local_.equivalence.exprs);
      return (ffelexHandler) ffestb_R5444_;

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	{
	  if (!ffestb_local_.equivalence.started)
	    {
	      ffestc_R544_start ();
	      ffestb_local_.equivalence.started = TRUE;
	    }
	  ffestc_R544_item (ffestb_local_.equivalence.exprs);
	  ffestc_R544_finish ();
	}
      ffestt_exprlist_kill (ffestb_local_.equivalence.exprs);
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "EQUIVALENCE", t);
  if (ffestb_local_.equivalence.started)
    ffestc_R544_finish ();
  ffestt_exprlist_kill (ffestb_local_.equivalence.exprs);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R5444_ -- "EQUIVALENCE" OPEN_PAREN expr COMMA expr CLOSE_PAREN COMMA

   return ffestb_R5444_;  // to lexer

   Make sure the next token is OPEN_PAREN, or generate an error.  */

static ffelexHandler
ffestb_R5444_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeOPEN_PAREN:
      return (ffelexHandler) ffeexpr_lhs (ffesta_output_pool,
					  FFEEXPR_contextEQUIVALENCE,
					  (ffeexprCallback) ffestb_R5441_);

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "EQUIVALENCE", t);
  if (ffestb_local_.equivalence.started)
    ffestc_R544_finish ();
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R834 -- Parse the CYCLE statement

   return ffestb_R834;	// to lexer

   Make sure the statement has a valid form for the CYCLE statement.  If
   it does, implement the statement.  */

ffelexHandler
ffestb_R834 (ffelexToken t)
{
  ffeTokenLength i;
  unsigned const char *p;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstCYCLE)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeNAME:
	  ffesta_confirmed ();
	  ffesta_tokens[1] = ffelex_token_use (t);
	  return (ffelexHandler) ffestb_R8341_;

	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  ffesta_confirmed ();
	  ffesta_tokens[1] = NULL;
	  return (ffelexHandler) ffestb_R8341_ (t);
	}

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstCYCLE)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  break;
	}
      ffesta_confirmed ();
      p = ffelex_token_text (ffesta_tokens[0]) + (i = FFESTR_firstlCYCLE);
      if (*p == '\0')
	{
	  ffesta_tokens[1] = NULL;
	}
      else
	{
	  if (!ffesrc_is_name_init (*p))
	    goto bad_i;		/* :::::::::::::::::::: */
	  ffesta_tokens[1]
	    = ffelex_token_name_from_names (ffesta_tokens[0], i, 0);
	}
      return (ffelexHandler) ffestb_R8341_ (t);

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "CYCLE", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "CYCLE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */

bad_i:				/* :::::::::::::::::::: */
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, "CYCLE", ffesta_tokens[0], i, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R8341_ -- "CYCLE" [NAME]

   return ffestb_R8341_;  // to lexer

   Make sure the next token is an EOS or SEMICOLON.  */

static ffelexHandler
ffestb_R8341_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	ffestc_R834 (ffesta_tokens[1]);
      if (ffesta_tokens[1] != NULL)
	ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffesta_zero (t);

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "CYCLE", t);
      break;
    }

  if (ffesta_tokens[1] != NULL)
    ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R835 -- Parse the EXIT statement

   return ffestb_R835;	// to lexer

   Make sure the statement has a valid form for the EXIT statement.  If
   it does, implement the statement.  */

ffelexHandler
ffestb_R835 (ffelexToken t)
{
  ffeTokenLength i;
  unsigned const char *p;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstEXIT)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeNAME:
	  ffesta_confirmed ();
	  ffesta_tokens[1] = ffelex_token_use (t);
	  return (ffelexHandler) ffestb_R8351_;

	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  ffesta_confirmed ();
	  ffesta_tokens[1] = NULL;
	  return (ffelexHandler) ffestb_R8351_ (t);
	}

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstEXIT)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  break;
	}
      ffesta_confirmed ();
      p = ffelex_token_text (ffesta_tokens[0]) + (i = FFESTR_firstlEXIT);
      if (*p == '\0')
	{
	  ffesta_tokens[1] = NULL;
	}
      else
	{
	  if (!ffesrc_is_name_init (*p))
	    goto bad_i;		/* :::::::::::::::::::: */
	  ffesta_tokens[1]
	    = ffelex_token_name_from_names (ffesta_tokens[0], i, 0);
	}
      return (ffelexHandler) ffestb_R8351_ (t);

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "EXIT", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "EXIT", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */

bad_i:				/* :::::::::::::::::::: */
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, "EXIT", ffesta_tokens[0], i, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R8351_ -- "EXIT" [NAME]

   return ffestb_R8351_;  // to lexer

   Make sure the next token is an EOS or SEMICOLON.  */

static ffelexHandler
ffestb_R8351_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	ffestc_R835 (ffesta_tokens[1]);
      if (ffesta_tokens[1] != NULL)
	ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffesta_zero (t);

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "EXIT", t);
      break;
    }

  if (ffesta_tokens[1] != NULL)
    ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R838 -- Parse the ASSIGN statement

   return ffestb_R838;	// to lexer

   Make sure the statement has a valid form for the ASSIGN statement.  If it
   does, implement the statement.  */

ffelexHandler
ffestb_R838 (ffelexToken t)
{
  unsigned const char *p;
  ffeTokenLength i;
  ffelexHandler next;
  ffelexToken et;		/* First token in target. */

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstASSIGN)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeNUMBER:
	  break;
	}
      ffesta_tokens[1] = ffelex_token_use (t);
      ffesta_confirmed ();
      return (ffelexHandler) ffestb_R8381_;

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstASSIGN)
	goto bad_0;		/* :::::::::::::::::::: */

      switch (ffelex_token_type (t))
	{
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  ffesta_confirmed ();
	  /* Fall through. */
	case FFELEX_typePERCENT:
	case FFELEX_typeOPEN_PAREN:
	  p = ffelex_token_text (ffesta_tokens[0]) + (i = FFESTR_firstlASSIGN);
	  if (! ISDIGIT (*p))
	    goto bad_i;		/* :::::::::::::::::::: */
	  ffesta_tokens[1]
	    = ffelex_token_number_from_names (ffesta_tokens[0], i);
	  p += ffelex_token_length (ffesta_tokens[1]);	/* Skip to "TO". */
	  i += ffelex_token_length (ffesta_tokens[1]);
	  if (!ffesrc_char_match_init (*p, 'T', 't')	/* "TO". */
	      || (++i, !ffesrc_char_match_noninit (*++p, 'O', 'o')))
	    {
	    bad_i_1:		/* :::::::::::::::::::: */
	      ffelex_token_kill (ffesta_tokens[1]);
	      goto bad_i;	/* :::::::::::::::::::: */
	    }
	  ++p, ++i;
	  if (!ffesrc_is_name_init (*p))
	    goto bad_i_1;	/* :::::::::::::::::::: */
	  et = ffelex_token_name_from_names (ffesta_tokens[0], i, 0);
	  next = (ffelexHandler)
	    (*((ffelexHandler)
	       ffeexpr_lhs (ffesta_output_pool,
			    FFEEXPR_contextASSIGN,
			    (ffeexprCallback)
			    ffestb_R8383_)))
	    (et);
	  ffelex_token_kill (et);
	  return (ffelexHandler) (*next) (t);

	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */
	}

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "ASSIGN", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid first token. */

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "ASSIGN", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */

bad_i:				/* :::::::::::::::::::: */
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, "ASSIGN", ffesta_tokens[0], i, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R8381_ -- "ASSIGN" NUMBER

   return ffestb_R8381_;  // to lexer

   Make sure the next token is "TO".  */

static ffelexHandler
ffestb_R8381_ (ffelexToken t)
{
  if ((ffelex_token_type (t) == FFELEX_typeNAME)
  && (ffesrc_strcmp_2c (ffe_case_match (), ffelex_token_text (t), "TO", "to",
			"To") == 0))
    {
      return (ffelexHandler) ffestb_R8382_;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "ASSIGN", t);
  if (ffelex_token_type (t) == FFELEX_typeNAME)
    return (ffelexHandler) ffestb_R8382_ (t);	/* Maybe user forgot "TO". */

  ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R8382_ -- "ASSIGN" NUMBER ("TO")

   return ffestb_R8382_;  // to lexer

   Make sure the next token is a name, then pass it along to the expression
   evaluator as an LHS expression.  The callback function is _3_.  */

static ffelexHandler
ffestb_R8382_ (ffelexToken t)
{
  if (ffelex_token_type (t) == FFELEX_typeNAME)
    {
      return (ffelexHandler)
      (*((ffelexHandler)
	 ffeexpr_lhs (ffesta_output_pool, FFEEXPR_contextASSIGN,
		      (ffeexprCallback) ffestb_R8383_)))
      (t);
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "ASSIGN", t);
  ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R8383_ -- "ASSIGN" NUMBER ("TO") expression

   (ffestb_R8383_)  // to expression handler

   Make sure the next token is an EOS or SEMICOLON.  */

static ffelexHandler
ffestb_R8383_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (expr == NULL)
	break;
      if (!ffesta_is_inhibited ())
	ffestc_R838 (ffesta_tokens[1], expr, ft);
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffesta_zero (t);

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "ASSIGN", t);
      break;
    }

  ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R840 -- Parse an arithmetic-IF statement

   return ffestb_R840;	// to lexer

   Make sure the statement has a valid form for an arithmetic-IF statement.
   If it does, implement the statement.	 */

ffelexHandler
ffestb_R840 (ffelexToken t)
{
  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffelex_token_length (ffesta_tokens[0]) != FFESTR_firstlIF)
	goto bad_0;		/* :::::::::::::::::::: */
      if (ffesta_first_kw != FFESTR_firstIF)
	goto bad_0;		/* :::::::::::::::::::: */
      break;

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstIF)
	goto bad_0;		/* :::::::::::::::::::: */
      break;

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeOPEN_PAREN:
      break;

    default:
      goto bad_1;		/* :::::::::::::::::::: */
    }

  return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool, FFEEXPR_contextARITHIF,
				      (ffeexprCallback) ffestb_R8401_);

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "arithmetic-IF", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "arithmetic-IF", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */
}

/* ffestb_R8401_ -- "IF" OPEN_PAREN expr

   (ffestb_R8401_)  // to expression handler

   Make sure the next token is CLOSE_PAREN.  */

static ffelexHandler
ffestb_R8401_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  ffestb_local_.if_stmt.expr = expr;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCLOSE_PAREN:
      if (expr == NULL)
	break;
      ffesta_tokens[1] = ffelex_token_use (ft);
      ffelex_set_names (TRUE);	/* In case it's a logical IF instead. */
      return (ffelexHandler) ffestb_R8402_;

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "arithmetic-IF", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R8402_ -- "IF" OPEN_PAREN expr CLOSE_PAREN

   return ffestb_R8402_;  // to lexer

   Make sure the next token is NUMBER.	*/

static ffelexHandler
ffestb_R8402_ (ffelexToken t)
{
  ffelex_set_names (FALSE);

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNUMBER:
      ffesta_confirmed ();
      ffesta_tokens[2] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_R8403_;

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "arithmetic-IF", t);
  ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R8403_ -- "IF" OPEN_PAREN expr CLOSE_PAREN NUMBER

   return ffestb_R8403_;  // to lexer

   Make sure the next token is COMMA.  */

static ffelexHandler
ffestb_R8403_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      return (ffelexHandler) ffestb_R8404_;

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "arithmetic-IF", t);
  ffelex_token_kill (ffesta_tokens[1]);
  ffelex_token_kill (ffesta_tokens[2]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R8404_ -- "IF" OPEN_PAREN expr CLOSE_PAREN NUMBER COMMA

   return ffestb_R8404_;  // to lexer

   Make sure the next token is NUMBER.	*/

static ffelexHandler
ffestb_R8404_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNUMBER:
      ffesta_tokens[3] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_R8405_;

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "arithmetic-IF", t);
  ffelex_token_kill (ffesta_tokens[1]);
  ffelex_token_kill (ffesta_tokens[2]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R8405_ -- "IF" OPEN_PAREN expr CLOSE_PAREN NUMBER COMMA NUMBER

   return ffestb_R8405_;  // to lexer

   Make sure the next token is COMMA.  */

static ffelexHandler
ffestb_R8405_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      return (ffelexHandler) ffestb_R8406_;

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "arithmetic-IF", t);
  ffelex_token_kill (ffesta_tokens[1]);
  ffelex_token_kill (ffesta_tokens[2]);
  ffelex_token_kill (ffesta_tokens[3]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R8406_ -- "IF" OPEN_PAREN expr CLOSE_PAREN NUMBER COMMA NUMBER COMMA

   return ffestb_R8406_;  // to lexer

   Make sure the next token is NUMBER.	*/

static ffelexHandler
ffestb_R8406_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNUMBER:
      ffesta_tokens[4] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_R8407_;

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "arithmetic-IF", t);
  ffelex_token_kill (ffesta_tokens[1]);
  ffelex_token_kill (ffesta_tokens[2]);
  ffelex_token_kill (ffesta_tokens[3]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R8407_ -- "IF" OPEN_PAREN expr CLOSE_PAREN NUMBER COMMA NUMBER COMMA
		    NUMBER

   return ffestb_R8407_;  // to lexer

   Make sure the next token is EOS or SEMICOLON.  */

static ffelexHandler
ffestb_R8407_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (!ffesta_is_inhibited ())
	ffestc_R840 (ffestb_local_.if_stmt.expr, ffesta_tokens[1],
		     ffesta_tokens[2], ffesta_tokens[3], ffesta_tokens[4]);
      ffelex_token_kill (ffesta_tokens[1]);
      ffelex_token_kill (ffesta_tokens[2]);
      ffelex_token_kill (ffesta_tokens[3]);
      ffelex_token_kill (ffesta_tokens[4]);
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "arithmetic-IF", t);
  ffelex_token_kill (ffesta_tokens[1]);
  ffelex_token_kill (ffesta_tokens[2]);
  ffelex_token_kill (ffesta_tokens[3]);
  ffelex_token_kill (ffesta_tokens[4]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R841 -- Parse the CONTINUE statement

   return ffestb_R841;	// to lexer

   Make sure the statement has a valid form for the CONTINUE statement.	 If
   it does, implement the statement.  */

ffelexHandler
ffestb_R841 (ffelexToken t)
{
  const char *p;
  ffeTokenLength i;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstCONTINUE)
	goto bad_0;		/* :::::::::::::::::::: */
      break;

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstCONTINUE)
	goto bad_0;		/* :::::::::::::::::::: */
      if (ffelex_token_length (ffesta_tokens[0]) != FFESTR_firstlCONTINUE)
	{
	  p = ffelex_token_text (ffesta_tokens[0]) + (i = FFESTR_firstlCONTINUE);
	  goto bad_i;		/* :::::::::::::::::::: */
	}
      break;

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	ffestc_R841 ();
      return (ffelexHandler) ffesta_zero (t);

    case FFELEX_typeCOMMA:
    case FFELEX_typeCOLONCOLON:
      ffesta_confirmed ();	/* Error, but clearly intended. */
      goto bad_1;		/* :::::::::::::::::::: */

    default:
      goto bad_1;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "CONTINUE", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid first token. */

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "CONTINUE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */

bad_i:				/* :::::::::::::::::::: */
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, "CONTINUE", ffesta_tokens[0], i, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R1102 -- Parse the PROGRAM statement

   return ffestb_R1102;	 // to lexer

   Make sure the statement has a valid form for the PROGRAM statement.	If it
   does, implement the statement.  */

ffelexHandler
ffestb_R1102 (ffelexToken t)
{
  ffeTokenLength i;
  unsigned const char *p;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstPROGRAM)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeNAME:
	  break;
	}

      ffesta_confirmed ();
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_R11021_;

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstPROGRAM)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  break;
	}
      ffesta_confirmed ();
      p = ffelex_token_text (ffesta_tokens[0]) + (i = FFESTR_firstlPROGRAM);
      if (!ffesrc_is_name_init (*p))
	goto bad_i;		/* :::::::::::::::::::: */
      ffesta_tokens[1]
	= ffelex_token_name_from_names (ffesta_tokens[0], i, 0);
      return (ffelexHandler) ffestb_R11021_ (t);

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "PROGRAM", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "PROGRAM", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */

bad_i:				/* :::::::::::::::::::: */
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, "PROGRAM", ffesta_tokens[0], i, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R11021_ -- "PROGRAM" NAME

   return ffestb_R11021_;  // to lexer

   Make sure the next token is an EOS or SEMICOLON.  */

static ffelexHandler
ffestb_R11021_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	ffestc_R1102 (ffesta_tokens[1]);
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffesta_zero (t);

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "PROGRAM", t);
      break;
    }

  ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_block -- Parse the BLOCK DATA statement

   return ffestb_block;	 // to lexer

   Make sure the statement has a valid form for the BLOCK DATA statement.  If
   it does, implement the statement.  */

ffelexHandler
ffestb_block (ffelexToken t)
{
  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstBLOCK)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeNAME:
	  if (ffesta_second_kw != FFESTR_secondDATA)
	    goto bad_1;		/* :::::::::::::::::::: */
	  break;
	}

      ffesta_confirmed ();
      return (ffelexHandler) ffestb_R1111_1_;

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "BLOCK DATA", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "BLOCK DATA", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */
}

/* ffestb_blockdata -- Parse the BLOCKDATA statement

   return ffestb_blockdata;  // to lexer

   Make sure the statement has a valid form for the BLOCKDATA statement.  If
   it does, implement the statement.  */

ffelexHandler
ffestb_blockdata (ffelexToken t)
{
  ffeTokenLength i;
  unsigned const char *p;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstBLOCKDATA)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeNAME:
	  ffesta_confirmed ();
	  ffesta_tokens[1] = ffelex_token_use (t);
	  return (ffelexHandler) ffestb_R1111_2_;

	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  ffesta_confirmed ();
	  ffesta_tokens[1] = NULL;
	  return (ffelexHandler) ffestb_R1111_2_ (t);
	}

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstBLOCKDATA)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  break;
	}
      ffesta_confirmed ();
      p = ffelex_token_text (ffesta_tokens[0]) + (i = FFESTR_firstlBLOCKDATA);
      if (*p == '\0')
	{
	  ffesta_tokens[1] = NULL;
	}
      else
	{
	  if (!ffesrc_is_name_init (*p))
	    goto bad_i;		/* :::::::::::::::::::: */
	  ffesta_tokens[1]
	    = ffelex_token_name_from_names (ffesta_tokens[0], i, 0);
	}
      return (ffelexHandler) ffestb_R1111_2_ (t);

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "BLOCK DATA", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "BLOCK DATA", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */

bad_i:				/* :::::::::::::::::::: */
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, "BLOCK DATA", ffesta_tokens[0], i, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R1111_1_ -- "BLOCK" "DATA"

   return ffestb_R1111_1_;  // to lexer

   Make sure the next token is a NAME, EOS, or SEMICOLON token.	 */

static ffelexHandler
ffestb_R1111_1_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_R1111_2_;

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_tokens[1] = NULL;
      return (ffelexHandler) ffestb_R1111_2_ (t);

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "BLOCK DATA", t);
      break;
    }

  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R1111_2_ -- "BLOCK/DATA" NAME

   return ffestb_R1111_2_;  // to lexer

   Make sure the next token is an EOS or SEMICOLON.  */

static ffelexHandler
ffestb_R1111_2_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	ffestc_R1111 (ffesta_tokens[1]);
      if (ffesta_tokens[1] != NULL)
	ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffesta_zero (t);

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "BLOCK DATA", t);
      break;
    }

  if (ffesta_tokens[1] != NULL)
    ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R1212 -- Parse the CALL statement

   return ffestb_R1212;	 // to lexer

   Make sure the statement has a valid form for the CALL statement.  If it
   does, implement the statement.  */

ffelexHandler
ffestb_R1212 (ffelexToken t)
{
  ffeTokenLength i;
  unsigned const char *p;
  ffelexHandler next;
  ffelexToken nt;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstCALL)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeNAME:
	  break;
	}
      ffesta_confirmed ();
      return (ffelexHandler)
	(*((ffelexHandler)
	   ffeexpr_lhs (ffesta_output_pool, FFEEXPR_contextSUBROUTINEREF,
			(ffeexprCallback) ffestb_R12121_)))
	(t);

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstCALL)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeCOLONCOLON:
	case FFELEX_typeCOMMA:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeOPEN_PAREN:
	  break;

	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  ffesta_confirmed ();
	  break;
	}
      p = ffelex_token_text (ffesta_tokens[0]) + (i = FFESTR_firstlCALL);
      if (!ffesrc_is_name_init (*p))
	goto bad_i;		/* :::::::::::::::::::: */
      nt = ffelex_token_name_from_names (ffesta_tokens[0], i, 0);
      next = (ffelexHandler)
	(*((ffelexHandler)
	   ffeexpr_lhs (ffesta_output_pool, FFEEXPR_contextSUBROUTINEREF,
			(ffeexprCallback) ffestb_R12121_)))
	(nt);
      ffelex_token_kill (nt);
      return (ffelexHandler) (*next) (t);

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "CALL", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "CALL", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */

bad_i:				/* :::::::::::::::::::: */
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, "CALL", ffesta_tokens[0], i, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R12121_ -- "CALL" expr

   (ffestb_R12121_)  // to expression handler

   Make sure the statement has a valid form for the CALL statement.  If it
   does, implement the statement.  */

static ffelexHandler
ffestb_R12121_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (expr == NULL)
	break;
      if (!ffesta_is_inhibited ())
	ffestc_R1212 (expr, ft);
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "CALL", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R1227 -- Parse the RETURN statement

   return ffestb_R1227;	 // to lexer

   Make sure the statement has a valid form for the RETURN statement.  If it
   does, implement the statement.  */

ffelexHandler
ffestb_R1227 (ffelexToken t)
{
  ffelexHandler next;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstRETURN)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeEQUALS:
	case FFELEX_typePOINTS:
	case FFELEX_typeCOLON:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	case FFELEX_typeNAME:
	case FFELEX_typeNUMBER:
	  ffesta_confirmed ();
	  break;

	default:
	  break;
	}

      return (ffelexHandler) (*((ffelexHandler)
		     ffeexpr_rhs (ffesta_output_pool, FFEEXPR_contextRETURN,
				  (ffeexprCallback) ffestb_R12271_)))
	(t);

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstRETURN)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeEQUALS:
	case FFELEX_typePOINTS:
	case FFELEX_typeCOLON:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  ffesta_confirmed ();
	  break;

	default:
	  break;
	}
      next = (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		   FFEEXPR_contextRETURN, (ffeexprCallback) ffestb_R12271_);
      next = (ffelexHandler) ffelex_splice_tokens (next, ffesta_tokens[0],
						   FFESTR_firstlRETURN);
      if (next == NULL)
	return (ffelexHandler) ffelex_swallow_tokens (t,
					       (ffelexHandler) ffesta_zero);
      return (ffelexHandler) (*next) (t);

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "RETURN", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "RETURN", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */
}

/* ffestb_R12271_ -- "RETURN" expr

   (ffestb_R12271_)  // to expression handler

   Make sure the next token is an EOS or SEMICOLON.  */

static ffelexHandler
ffestb_R12271_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	ffestc_R1227 (expr, ft);
      return (ffelexHandler) ffesta_zero (t);

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "RETURN", t);
      break;
    }

  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R1228 -- Parse the CONTAINS statement

   return ffestb_R1228;	 // to lexer

   Make sure the statement has a valid form for the CONTAINS statement.	 If
   it does, implement the statement.  */

#if FFESTR_F90
ffelexHandler
ffestb_R1228 (ffelexToken t)
{
  const char *p;
  ffeTokenLength i;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstCONTAINS)
	goto bad_0;		/* :::::::::::::::::::: */
      break;

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstCONTAINS)
	goto bad_0;		/* :::::::::::::::::::: */
      if (ffelex_token_length (ffesta_tokens[0]) != FFESTR_firstlCONTAINS)
	{
	  p = ffelex_token_text (ffesta_tokens[0]) + (i = FFESTR_firstlCONTAINS);
	  goto bad_i;		/* :::::::::::::::::::: */
	}
      break;

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	ffestc_R1228 ();
      return (ffelexHandler) ffesta_zero (t);

    case FFELEX_typeCOMMA:
    case FFELEX_typeCOLONCOLON:
      ffesta_confirmed ();	/* Error, but clearly intended. */
      goto bad_1;		/* :::::::::::::::::::: */

    default:
      goto bad_1;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "CONTAINS", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid first token. */

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "CONTAINS", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */

bad_i:				/* :::::::::::::::::::: */
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, "CONTAINS", ffesta_tokens[0], i, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

#endif
/* ffestb_V009 -- Parse the UNION statement

   return ffestb_V009;	// to lexer

   Make sure the statement has a valid form for the UNION statement.  If
   it does, implement the statement.  */

#if FFESTR_VXT
ffelexHandler
ffestb_V009 (ffelexToken t)
{
  const char *p;
  ffeTokenLength i;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstUNION)
	goto bad_0;		/* :::::::::::::::::::: */
      break;

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstUNION)
	goto bad_0;		/* :::::::::::::::::::: */
      if (ffelex_token_length (ffesta_tokens[0]) != FFESTR_firstlUNION)
	{
	  p = ffelex_token_text (ffesta_tokens[0]) + (i = FFESTR_firstlUNION);
	  goto bad_i;		/* :::::::::::::::::::: */
	}
      break;

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	ffestc_V009 ();
      return (ffelexHandler) ffesta_zero (t);

    case FFELEX_typeCOMMA:
    case FFELEX_typeCOLONCOLON:
      ffesta_confirmed ();	/* Error, but clearly intended. */
      goto bad_1;		/* :::::::::::::::::::: */

    default:
      goto bad_1;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "UNION", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid first token. */

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "UNION", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */

bad_i:				/* :::::::::::::::::::: */
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, "UNION", ffesta_tokens[0], i, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

#endif
/* ffestb_construct -- Parse a construct name

   return ffestb_construct;  // to lexer

   Make sure the statement can have a construct name (if-then-stmt, do-stmt,
   select-case-stmt).  */

ffelexHandler
ffestb_construct (ffelexToken t UNUSED)
{
  /* This handler gets invoked only when token 0 is NAME/NAMES and token 1 is
     COLON. */

  ffesta_confirmed ();
  ffelex_set_names (TRUE);
  return (ffelexHandler) ffestb_construct1_;
}

/* ffestb_construct1_ -- NAME COLON

   return ffestb_construct1_;  // to lexer

   Make sure we've got a NAME that is DO, DOWHILE, IF, SELECT, or SELECTCASE.  */

static ffelexHandler
ffestb_construct1_ (ffelexToken t)
{
  ffelex_set_names (FALSE);

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_first_kw = ffestr_first (t);
      switch (ffesta_first_kw)
	{
	case FFESTR_firstIF:
	  ffestb_local_.construct.next = (ffelexHandler) ffestb_if;
	  break;

	case FFESTR_firstDO:
	  ffestb_local_.construct.next = (ffelexHandler) ffestb_do;
	  break;

	case FFESTR_firstDOWHILE:
	  ffestb_local_.construct.next = (ffelexHandler) ffestb_dowhile;
	  break;

	case FFESTR_firstSELECT:
	case FFESTR_firstSELECTCASE:
	  ffestb_local_.construct.next = (ffelexHandler) ffestb_R809;
	  break;

	default:
	  goto bad;		/* :::::::::::::::::::: */
	}
      ffesta_construct_name = ffesta_tokens[0];
      ffesta_tokens[0] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_construct2_;

    case FFELEX_typeNAMES:
      ffesta_first_kw = ffestr_first (t);
      switch (ffesta_first_kw)
	{
	case FFESTR_firstIF:
	  if (ffelex_token_length (t) != FFESTR_firstlIF)
	    goto bad;		/* :::::::::::::::::::: */
	  ffestb_local_.construct.next = (ffelexHandler) ffestb_if;
	  break;

	case FFESTR_firstDO:
	  ffestb_local_.construct.next = (ffelexHandler) ffestb_do;
	  break;

	case FFESTR_firstDOWHILE:
	  if (ffelex_token_length (t) != FFESTR_firstlDOWHILE)
	    goto bad;		/* :::::::::::::::::::: */
	  ffestb_local_.construct.next = (ffelexHandler) ffestb_dowhile;
	  break;

	case FFESTR_firstSELECTCASE:
	  if (ffelex_token_length (t) != FFESTR_firstlSELECTCASE)
	    goto bad;		/* :::::::::::::::::::: */
	  ffestb_local_.construct.next = (ffelexHandler) ffestb_R809;
	  break;

	default:
	  goto bad;		/* :::::::::::::::::::: */
	}
      ffesta_construct_name = ffesta_tokens[0];
      ffesta_tokens[0] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_construct2_;

    default:
      break;
    }

bad:				/* :::::::::::::::::::: */
  ffesta_ffebad_2st (FFEBAD_INVALID_STMT_FORM, "CONSTRUCT",
		     ffesta_tokens[0], t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_construct2_ -- NAME COLON "DO/DOWHILE/IF/SELECT/SELECTCASE"

   return ffestb_construct2_;  // to lexer

   This extra step is needed to set ffesta_second_kw if the second token
   (here) is a NAME, so DO and SELECT can continue to expect it.  */

static ffelexHandler
ffestb_construct2_ (ffelexToken t)
{
  if (ffelex_token_type (t) == FFELEX_typeNAME)
    ffesta_second_kw = ffestr_second (t);
  return (ffelexHandler) (*ffestb_local_.construct.next) (t);
}

/* ffestb_heap -- Parse an ALLOCATE/DEALLOCATE statement

   return ffestb_heap;	// to lexer

   Make sure the statement has a valid form for an ALLOCATE/DEALLOCATE
   statement.  If it does, implement the statement.  */

#if FFESTR_F90
ffelexHandler
ffestb_heap (ffelexToken t)
{
  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      break;

    case FFELEX_typeNAMES:
      if (ffelex_token_length (ffesta_tokens[0]) != ffestb_args.heap.len)
	goto bad_0;		/* :::::::::::::::::::: */
      break;

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeOPEN_PAREN:
      break;

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
    case FFELEX_typeCOMMA:
    case FFELEX_typeCOLONCOLON:
      ffesta_confirmed ();	/* Error, but clearly intended. */
      goto bad_1;		/* :::::::::::::::::::: */

    default:
      goto bad_1;		/* :::::::::::::::::::: */
    }

  ffestb_local_.heap.exprs = ffestt_exprlist_create ();
  return (ffelexHandler) ffeexpr_lhs (ffesta_output_pool,
				      ffestb_args.heap.ctx,
				      (ffeexprCallback) ffestb_heap1_);

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.heap.badname, ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.heap.badname, t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */
}

/* ffestb_heap1_ -- "ALLOCATE/DEALLOCATE" OPEN_PAREN expr

   (ffestb_heap1_)  // to expression handler

   Make sure the next token is COMMA.  */

static ffelexHandler
ffestb_heap1_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      if (expr == NULL)
	break;
      ffestt_exprlist_append (ffestb_local_.heap.exprs, expr,
			      ffelex_token_use (t));
      return (ffelexHandler) ffestb_heap2_;

    case FFELEX_typeCLOSE_PAREN:
      if (expr == NULL)
	break;
      ffestt_exprlist_append (ffestb_local_.heap.exprs, expr,
			      ffelex_token_use (t));
      ffesta_tokens[1] = NULL;
      ffestb_local_.heap.expr = NULL;
      return (ffelexHandler) ffestb_heap5_;

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.heap.badname, t);
  ffestt_exprlist_kill (ffestb_local_.heap.exprs);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_heap2_ -- "ALLOCATE/DEALLOCATE" OPEN_PAREN expr COMMA

   return ffestb_heap2_;  // to lexer

   Make sure the next token is NAME.  */

static ffelexHandler
ffestb_heap2_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_heap3_;

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.heap.badname, t);
  ffestt_exprlist_kill (ffestb_local_.heap.exprs);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_heap3_ -- "ALLOCATE/DEALLOCATE" OPEN_PAREN expr COMMA NAME

   return ffestb_heap3_;  // to lexer

   If token is EQUALS, make sure NAME was "STAT" and handle STAT variable;
   else pass NAME and token to expression handler.  */

static ffelexHandler
ffestb_heap3_ (ffelexToken t)
{
  ffelexHandler next;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEQUALS:
      ffesta_confirmed ();
      if (ffestr_other (ffesta_tokens[1]) != FFESTR_otherSTAT)
	break;
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffeexpr_lhs (ffesta_output_pool,
					  FFEEXPR_contextHEAPSTAT,
					  (ffeexprCallback) ffestb_heap4_);

    default:
      next = (ffelexHandler)
	(*((ffelexHandler) ffeexpr_lhs (ffesta_output_pool,
					ffestb_args.heap.ctx,
					(ffeexprCallback) ffestb_heap1_)))
	(ffesta_tokens[1]);
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) (*next) (t);
    }

  ffelex_token_kill (ffesta_tokens[1]);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.heap.badname, t);
  ffestt_exprlist_kill (ffestb_local_.heap.exprs);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_heap4_ -- "ALLOCATE/DEALLOCATE" OPEN_PAREN ... COMMA "STAT" EQUALS
		    expr

   (ffestb_heap4_)  // to expression handler

   Make sure the next token is CLOSE_PAREN.  */

static ffelexHandler
ffestb_heap4_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCLOSE_PAREN:
      if (expr == NULL)
	break;
      ffesta_tokens[1] = ffelex_token_use (ft);
      ffestb_local_.heap.expr = expr;
      return (ffelexHandler) ffestb_heap5_;

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.heap.badname, t);
  ffestt_exprlist_kill (ffestb_local_.heap.exprs);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_heap5_ -- "ALLOCATE/DEALLOCATE" OPEN_PAREN ... CLOSE_PAREN

   return ffestb_heap5_;  // to lexer

   Make sure the next token is EOS/SEMICOLON.  */

static ffelexHandler
ffestb_heap5_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	if (ffesta_first_kw == FFESTR_firstALLOCATE)
	  ffestc_R620 (ffestb_local_.heap.exprs, ffestb_local_.heap.expr,
		       ffesta_tokens[1]);
	else
	  ffestc_R625 (ffestb_local_.heap.exprs, ffestb_local_.heap.expr,
		       ffesta_tokens[1]);
      ffestt_exprlist_kill (ffestb_local_.heap.exprs);
      if (ffesta_tokens[1] != NULL)
	ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.heap.badname, t);
  ffestt_exprlist_kill (ffestb_local_.heap.exprs);
  if (ffesta_tokens[1] != NULL)
    ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

#endif
/* ffestb_module -- Parse the MODULEPROCEDURE statement

   return ffestb_module;  // to lexer

   Make sure the statement has a valid form for the MODULEPROCEDURE statement.
   If it does, implement the statement.

   31-May-90  JCB  1.1
      Confirm NAME==MODULE followed by standard four invalid tokens, so we
      get decent message if somebody forgets that MODULE requires a name.  */

#if FFESTR_F90
ffelexHandler
ffestb_module (ffelexToken t)
{
  ffeTokenLength i;
  const char *p;
  ffelexToken nt;
  ffelexToken mt;		/* Name in MODULE PROCEDUREname, i.e.
				   includes "PROCEDURE". */

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstMODULE)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeNAME:
	  break;

	case FFELEX_typeCOLONCOLON:
	case FFELEX_typeCOMMA:
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  ffesta_confirmed ();
	  goto bad_1m;		/* :::::::::::::::::::: */

	default:
	  goto bad_1m;		/* :::::::::::::::::::: */
	}

      ffesta_confirmed ();
      if (ffesta_second_kw != FFESTR_secondPROCEDURE)
	{
	  ffesta_tokens[1] = ffelex_token_use (t);
	  return (ffelexHandler) ffestb_module3_;
	}
      ffestb_local_.moduleprocedure.started = FALSE;
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_module1_;

    case FFELEX_typeNAMES:
      p = ffelex_token_text (ffesta_tokens[0])
	+ (i = FFESTR_firstlMODULEPROCEDURE);
      if ((ffesta_first_kw == FFESTR_firstMODULE)
	  || ((ffesta_first_kw == FFESTR_firstMODULEPROCEDURE)
	      && !ffesrc_is_name_init (*p)))
	{			/* Definitely not "MODULE PROCEDURE name". */
	  switch (ffelex_token_type (t))
	    {
	    case FFELEX_typeCOMMA:
	    case FFELEX_typeCOLONCOLON:
	      ffesta_confirmed ();	/* Error, but clearly intended. */
	      goto bad_1m;	/* :::::::::::::::::::: */

	    default:
	      goto bad_1m;	/* :::::::::::::::::::: */

	    case FFELEX_typeEOS:
	    case FFELEX_typeSEMICOLON:
	      ffesta_confirmed ();
	      break;
	    }
	  p = ffelex_token_text (ffesta_tokens[0]) + (i = FFESTR_firstlMODULE);
	  if (!ffesrc_is_name_init (*p))
	    goto bad_im;	/* :::::::::::::::::::: */
	  nt = ffelex_token_name_from_names (ffesta_tokens[0], i, 0);
	  if (!ffesta_is_inhibited ())
	    ffestc_R1105 (nt);
	  ffelex_token_kill (nt);
	  return (ffelexHandler) ffesta_zero (t);
	}

      /* Here we know that we're indeed looking at a MODULEPROCEDURE
	 statement rather than MODULE and that the character following
	 MODULEPROCEDURE in the NAMES token is a valid first character for a
	 NAME.	This means that unless the second token is COMMA, we have an
	 ambiguous statement that can be read either as MODULE PROCEDURE name
	 or MODULE PROCEDUREname, the former being an R1205, the latter an
	 R1105. */

      if (ffesta_first_kw != FFESTR_firstMODULEPROCEDURE)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeCOMMA:	/* Aha, clearly not MODULE PROCEDUREname. */
	  ffesta_confirmed ();
	  ffestb_local_.moduleprocedure.started = FALSE;
	  ffesta_tokens[1]
	    = ffelex_token_name_from_names (ffesta_tokens[0], i, 0);
	  return (ffelexHandler) ffestb_module2_ (t);

	case FFELEX_typeEOS:	/* MODULE PROCEDURE name or MODULE
				   PROCEDUREname. */
	case FFELEX_typeSEMICOLON:
	  ffesta_confirmed ();
	  break;
	}
      nt = ffelex_token_name_from_names (ffesta_tokens[0], i, 0);
      mt = ffelex_token_name_from_names (ffesta_tokens[0], FFESTR_firstlMODULE,
					 0);
      if (!ffesta_is_inhibited ())
	ffestc_module (mt, nt);	/* Implement ambiguous statement. */
      ffelex_token_kill (nt);
      ffelex_token_kill (mt);
      return (ffelexHandler) ffesta_zero (t);

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "MODULE PROCEDURE", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "MODULE PROCEDURE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */

bad_1m:			/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "MODULE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */

bad_im:			/* :::::::::::::::::::: */
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, "MODULE", ffesta_tokens[0], i, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_module1_ -- "MODULEPROCEDURE" or "MODULE" "PROCEDURE"

   return ffestb_module1_;  // to lexer

   Make sure the statement has a valid form for the MODULEPROCEDURE statement.	If it
   does, implement the statement.  */

static ffelexHandler
ffestb_module1_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      if (!ffestb_local_.moduleprocedure.started
	  && (ffelex_token_type (ffesta_tokens[0]) == FFELEX_typeNAME))
	{
	  ffesta_confirmed ();
	  ffelex_token_kill (ffesta_tokens[1]);
	}
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_module2_;

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (ffestb_local_.moduleprocedure.started)
	break;			/* Error if we've already seen NAME COMMA. */
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	ffestc_R1105 (ffesta_tokens[1]);
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffesta_zero (t);

    case FFELEX_typeCOMMA:
    case FFELEX_typeCOLONCOLON:
      ffesta_confirmed ();	/* Error, but clearly intended. */
      break;

    default:
      break;
    }

  if (ffestb_local_.moduleprocedure.started && !ffesta_is_inhibited ())
    ffestc_R1205_finish ();
  else if (!ffestb_local_.moduleprocedure.started)
    ffelex_token_kill (ffesta_tokens[1]);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "MODULE PROCEDURE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_module2_ -- "MODULE/PROCEDURE" NAME

   return ffestb_module2_;  // to lexer

   Make sure the statement has a valid form for the MODULEPROCEDURE statement.	If it
   does, implement the statement.  */

static ffelexHandler
ffestb_module2_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (!ffestb_local_.moduleprocedure.started)
	{
	  ffesta_confirmed ();
	  if (!ffesta_is_inhibited ())
	    ffestc_R1205_start ();
	}
      if (!ffesta_is_inhibited ())
	{
	  ffestc_R1205_item (ffesta_tokens[1]);
	  ffestc_R1205_finish ();
	}
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffesta_zero (t);

    case FFELEX_typeCOMMA:
      if (!ffestb_local_.moduleprocedure.started)
	{
	  ffestb_local_.moduleprocedure.started = TRUE;
	  ffesta_confirmed ();
	  if (!ffesta_is_inhibited ())
	    ffestc_R1205_start ();
	}
      if (!ffesta_is_inhibited ())
	ffestc_R1205_item (ffesta_tokens[1]);
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffestb_module1_;

    default:
      break;
    }

  if (ffestb_local_.moduleprocedure.started && !ffesta_is_inhibited ())
    ffestc_R1205_finish ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "MODULE PROCEDURE", t);
  ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_module3_ -- "MODULE" NAME

   return ffestb_module3_;  // to lexer

   Make sure the statement has a valid form for the MODULE statement.  If it
   does, implement the statement.  */

static ffelexHandler
ffestb_module3_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (!ffesta_is_inhibited ())
	ffestc_R1105 (ffesta_tokens[1]);
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "MODULE", t);
  ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

#endif
/* ffestb_R809 -- Parse the SELECTCASE statement

   return ffestb_R809;	// to lexer

   Make sure the statement has a valid form for the SELECTCASE statement.
   If it does, implement the statement.	 */

ffelexHandler
ffestb_R809 (ffelexToken t)
{
  ffeTokenLength i;
  const char *p;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      switch (ffesta_first_kw)
	{
	case FFESTR_firstSELECT:
	  if ((ffelex_token_type (t) != FFELEX_typeNAME)
	      || (ffesta_second_kw != FFESTR_secondCASE))
	    goto bad_1;		/* :::::::::::::::::::: */
	  ffesta_confirmed ();
	  return (ffelexHandler) ffestb_R8091_;

	case FFESTR_firstSELECTCASE:
	  return (ffelexHandler) ffestb_R8091_ (t);

	default:
	  goto bad_0;		/* :::::::::::::::::::: */
	}

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstSELECTCASE)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeCOMMA:
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeOPEN_PAREN:
	  break;
	}
      p = ffelex_token_text (ffesta_tokens[0]) + (i = FFESTR_firstlSELECTCASE);
      if (*p != '\0')
	goto bad_i;		/* :::::::::::::::::::: */
      return (ffelexHandler) ffestb_R8091_ (t);

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  if (ffesta_construct_name != NULL)
    {
      ffelex_token_kill (ffesta_construct_name);
      ffesta_construct_name = NULL;
    }
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "SELECT CASE", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  if (ffesta_construct_name != NULL)
    {
      ffelex_token_kill (ffesta_construct_name);
      ffesta_construct_name = NULL;
    }
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "SELECT CASE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */

bad_i:				/* :::::::::::::::::::: */
  if (ffesta_construct_name != NULL)
    {
      ffelex_token_kill (ffesta_construct_name);
      ffesta_construct_name = NULL;
    }
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, "SELECT CASE", ffesta_tokens[0], i, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R8091_ -- "SELECTCASE" or "SELECT" "CASE"

   return ffestb_R8091_;  // to lexer

   Make sure the statement has a valid form for the SELECTCASE statement.  If it
   does, implement the statement.  */

static ffelexHandler
ffestb_R8091_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeOPEN_PAREN:
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		FFEEXPR_contextSELECTCASE, (ffeexprCallback) ffestb_R8092_);

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
    case FFELEX_typeCOMMA:
    case FFELEX_typeCOLONCOLON:
      ffesta_confirmed ();	/* Error, but clearly intended. */
      break;

    default:
      break;
    }

  if (ffesta_construct_name != NULL)
    {
      ffelex_token_kill (ffesta_construct_name);
      ffesta_construct_name = NULL;
    }
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "SELECT CASE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R8092_ -- "SELECT/CASE" OPEN_PAREN expr

   (ffestb_R8092_)  // to expression handler

   Make sure the statement has a valid form for the SELECTCASE statement.  If it
   does, implement the statement.  */

static ffelexHandler
ffestb_R8092_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCLOSE_PAREN:
      if (expr == NULL)
	break;
      ffesta_tokens[1] = ffelex_token_use (ft);
      ffestb_local_.selectcase.expr = expr;
      return (ffelexHandler) ffestb_R8093_;

    default:
      break;
    }

  if (ffesta_construct_name != NULL)
    {
      ffelex_token_kill (ffesta_construct_name);
      ffesta_construct_name = NULL;
    }
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "SELECT CASE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R8093_ -- "SELECT/CASE" OPEN_PAREN expr CLOSE_PAREN

   return ffestb_R8093_;  // to lexer

   Make sure the statement has a valid form for the SELECTCASE statement.  If it
   does, implement the statement.  */

static ffelexHandler
ffestb_R8093_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	ffestc_R809 (ffesta_construct_name, ffestb_local_.selectcase.expr,
		     ffesta_tokens[1]);
      ffelex_token_kill (ffesta_tokens[1]);
      if (ffesta_construct_name != NULL)
	{
	  ffelex_token_kill (ffesta_construct_name);
	  ffesta_construct_name = NULL;
	}
      return ffesta_zero (t);

    case FFELEX_typeCOMMA:
    case FFELEX_typeCOLONCOLON:
      ffesta_confirmed ();	/* Error, but clearly intended. */
      break;

    default:
      break;
    }

  ffelex_token_kill (ffesta_tokens[1]);
  if (ffesta_construct_name != NULL)
    {
      ffelex_token_kill (ffesta_construct_name);
      ffesta_construct_name = NULL;
    }
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "SELECT CASE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R810 -- Parse the CASE statement

   return ffestb_R810;	// to lexer

   Make sure the statement has a valid form for the CASE statement.
   If it does, implement the statement.	 */

ffelexHandler
ffestb_R810 (ffelexToken t)
{
  ffeTokenLength i;
  unsigned const char *p;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstCASE)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeCOMMA:
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeNAME:
	  ffesta_confirmed ();
	  if (ffesta_second_kw != FFESTR_secondDEFAULT)
	    goto bad_1;		/* :::::::::::::::::::: */
	  ffestb_local_.case_stmt.cases = NULL;
	  return (ffelexHandler) ffestb_R8101_;

	case FFELEX_typeOPEN_PAREN:
	  ffestb_local_.case_stmt.cases = ffestt_caselist_create ();
	  return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		      FFEEXPR_contextCASE, (ffeexprCallback) ffestb_R8103_);
	}

    case FFELEX_typeNAMES:
      switch (ffesta_first_kw)
	{
	case FFESTR_firstCASEDEFAULT:
	  switch (ffelex_token_type (t))
	    {
	    case FFELEX_typeCOMMA:
	    case FFELEX_typeCOLONCOLON:
	      ffesta_confirmed ();	/* Error, but clearly intended. */
	      goto bad_1;	/* :::::::::::::::::::: */

	    default:
	      goto bad_1;	/* :::::::::::::::::::: */

	    case FFELEX_typeEOS:
	    case FFELEX_typeSEMICOLON:
	      ffesta_confirmed ();
	      break;
	    }
	  ffestb_local_.case_stmt.cases = NULL;
	  p = ffelex_token_text (ffesta_tokens[0])
	    + (i = FFESTR_firstlCASEDEFAULT);
	  if (*p == '\0')
	    return (ffelexHandler) ffestb_R8101_ (t);
	  if (!ffesrc_is_name_init (*p))
	    goto bad_i;		/* :::::::::::::::::::: */
	  ffesta_tokens[1] = ffelex_token_name_from_names (ffesta_tokens[0], i,
							   0);
	  return (ffelexHandler) ffestb_R8102_ (t);

	case FFESTR_firstCASE:
	  break;

	default:
	  goto bad_0;		/* :::::::::::::::::::: */
	}

      switch (ffelex_token_type (t))
	{
	case FFELEX_typeCOMMA:
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeOPEN_PAREN:
	  break;
	}
      p = ffelex_token_text (ffesta_tokens[0]) + (i = FFESTR_firstlCASE);
      if (*p != '\0')
	goto bad_i;		/* :::::::::::::::::::: */
      ffestb_local_.case_stmt.cases = ffestt_caselist_create ();
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		      FFEEXPR_contextCASE, (ffeexprCallback) ffestb_R8103_);

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "CASE", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "CASE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */

bad_i:				/* :::::::::::::::::::: */
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, "CASE", ffesta_tokens[0], i, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R8101_ -- "CASE" case-selector

   return ffestb_R8101_;  // to lexer

   Make sure the statement has a valid form for the CASE statement.  If it
   does, implement the statement.  */

static ffelexHandler
ffestb_R8101_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_R8102_;

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_tokens[1] = NULL;
      return (ffelexHandler) ffestb_R8102_ (t);

    case FFELEX_typeCOMMA:
    case FFELEX_typeCOLONCOLON:
      ffesta_confirmed ();	/* Error, but clearly intended. */
      break;

    default:
      break;
    }

  if (ffestb_local_.case_stmt.cases != NULL)
    ffestt_caselist_kill (ffestb_local_.case_stmt.cases);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "CASE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R8102_ -- "CASE" case-selector [NAME]

   return ffestb_R8102_;  // to lexer

   Make sure the statement has a valid form for the CASE statement.  If it
   does, implement the statement.  */

static ffelexHandler
ffestb_R8102_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	ffestc_R810 (ffestb_local_.case_stmt.cases, ffesta_tokens[1]);
      if (ffestb_local_.case_stmt.cases != NULL)
	ffestt_caselist_kill (ffestb_local_.case_stmt.cases);
      if (ffesta_tokens[1] != NULL)
	ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffesta_zero (t);

    case FFELEX_typeCOMMA:
    case FFELEX_typeCOLONCOLON:
      ffesta_confirmed ();	/* Error, but clearly intended. */
      break;

    default:
      break;
    }

  if (ffestb_local_.case_stmt.cases != NULL)
    ffestt_caselist_kill (ffestb_local_.case_stmt.cases);
  if (ffesta_tokens[1] != NULL)
    ffelex_token_kill (ffesta_tokens[1]);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "CASE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R8103_ -- "CASE" OPEN_PAREN expr

   (ffestb_R8103_)  // to expression handler

   Make sure the statement has a valid form for the CASE statement.  If it
   does, implement the statement.  */

static ffelexHandler
ffestb_R8103_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCLOSE_PAREN:
      ffestt_caselist_append (ffestb_local_.case_stmt.cases, FALSE, expr, NULL,
			      ffelex_token_use (ft));
      return (ffelexHandler) ffestb_R8101_;

    case FFELEX_typeCOMMA:
      ffestt_caselist_append (ffestb_local_.case_stmt.cases, FALSE, expr, NULL,
			      ffelex_token_use (ft));
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		      FFEEXPR_contextCASE, (ffeexprCallback) ffestb_R8103_);

    case FFELEX_typeCOLON:
      ffestt_caselist_append (ffestb_local_.case_stmt.cases, TRUE, expr, NULL,
			      ffelex_token_use (ft));	/* NULL second expr for
							   now, just plug in. */
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		      FFEEXPR_contextCASE, (ffeexprCallback) ffestb_R8104_);

    default:
      break;
    }

  ffestt_caselist_kill (ffestb_local_.case_stmt.cases);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "CASE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R8104_ -- "CASE" OPEN_PAREN expr COLON expr

   (ffestb_R8104_)  // to expression handler

   Make sure the statement has a valid form for the CASE statement.  If it
   does, implement the statement.  */

static ffelexHandler
ffestb_R8104_ (ffelexToken ft UNUSED, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCLOSE_PAREN:
      ffestb_local_.case_stmt.cases->previous->expr2 = expr;
      return (ffelexHandler) ffestb_R8101_;

    case FFELEX_typeCOMMA:
      ffestb_local_.case_stmt.cases->previous->expr2 = expr;
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		      FFEEXPR_contextCASE, (ffeexprCallback) ffestb_R8103_);

    default:
      break;
    }

  ffestt_caselist_kill (ffestb_local_.case_stmt.cases);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "CASE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R1001 -- Parse a FORMAT statement

   return ffestb_R1001;	 // to lexer

   Make sure the statement has a valid form for an FORMAT statement.
   If it does, implement the statement.	 */

ffelexHandler
ffestb_R1001 (ffelexToken t)
{
  ffesttFormatList f;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstFORMAT)
	goto bad_0;		/* :::::::::::::::::::: */
      break;

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstFORMAT)
	goto bad_0;		/* :::::::::::::::::::: */
      if (ffelex_token_length (ffesta_tokens[0]) != FFESTR_firstlFORMAT)
	goto bad_0;		/* :::::::::::::::::::: */
      break;

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeOPEN_PAREN:
      ffestb_local_.format.complained = FALSE;
      ffestb_local_.format.f = NULL;	/* No parent yet. */
      ffestb_local_.format.f = ffestt_formatlist_create (NULL,
						      ffelex_token_use (t));
      ffelex_set_names_pure (TRUE);	/* Have even free-form lexer give us
					   NAMES. */
      return (ffelexHandler) ffestb_R10011_;

    case FFELEX_typeOPEN_ARRAY:/* "(/". */
      ffesta_confirmed ();
      ffestb_local_.format.complained = FALSE;
      ffestb_local_.format.f = ffestt_formatlist_create (NULL,
						      ffelex_token_use (t));
      f = ffestt_formatlist_append (ffestb_local_.format.f);
      f->type = FFESTP_formattypeSLASH;
      f->t = ffelex_token_use (t);
      f->u.R1010.val.present = FALSE;
      f->u.R1010.val.rtexpr = FALSE;
      f->u.R1010.val.t = NULL;
      f->u.R1010.val.u.unsigned_val = 1;
      ffelex_set_names_pure (TRUE);	/* Have even free-form lexer give us
					   NAMES. */
      return (ffelexHandler) ffestb_R100112_;

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
    case FFELEX_typeCOMMA:
    case FFELEX_typeCOLONCOLON:
      ffesta_confirmed ();	/* Error, but clearly intended. */
      goto bad_1;		/* :::::::::::::::::::: */

    default:
      goto bad_1;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FORMAT", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FORMAT", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */
}

/* ffestb_R10011_ -- "FORMAT" OPEN_PAREN expr

   return ffestb_R10011_;  // to lexer

   For CLOSE_PAREN, wrap up the format list and if it is the top-level one,
   exit.  For anything else, pass it to _2_.  */

static ffelexHandler
ffestb_R10011_ (ffelexToken t)
{
  ffesttFormatList f;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCLOSE_PAREN:
      break;

    default:
      return (ffelexHandler) ffestb_R10012_ (t);
    }

  /* If we have a format we're working on, continue working on it. */

  f = ffestb_local_.format.f->u.root.parent;

  if (f != NULL)
    {
      ffestb_local_.format.f = f->next;
      return (ffelexHandler) ffestb_R100111_;
    }

  return (ffelexHandler) ffestb_R100114_;
}

/* ffestb_R10012_ -- "FORMAT" OPEN_PAREN [format-item-list]

   return ffestb_R10012_;  // to lexer

   The initial state for a format-item.	 Here, just handle the initial
   number, sign for number, or run-time expression.  Also handle spurious
   comma, close-paren (indicating spurious comma), close-array (like
   close-paren but preceded by slash), and quoted strings.  */

static ffelexHandler
ffestb_R10012_ (ffelexToken t)
{
  unsigned long unsigned_val;
  ffesttFormatList f;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeOPEN_ANGLE:
      ffesta_confirmed ();
      ffestb_local_.format.pre.t = ffelex_token_use (t);
      ffelex_set_names_pure (FALSE);
      if (!ffesta_seen_first_exec && !ffestb_local_.format.complained)
	{
	  ffestb_local_.format.complained = TRUE;
	  ffebad_start (FFEBAD_FORMAT_EXPR_SPEC);
	  ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_finish ();
	}
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		  FFEEXPR_contextFORMAT, (ffeexprCallback) ffestb_R100115_);

    case FFELEX_typeNUMBER:
      ffestb_local_.format.sign = FALSE;	/* No sign present. */
      ffestb_local_.format.pre.present = TRUE;
      ffestb_local_.format.pre.rtexpr = FALSE;
      ffestb_local_.format.pre.t = ffelex_token_use (t);
      ffestb_local_.format.pre.u.unsigned_val = unsigned_val
	= strtoul (ffelex_token_text (t), NULL, 10);
      ffelex_set_expecting_hollerith (unsigned_val, '\0',
				      ffelex_token_where_line (t),
				      ffelex_token_where_column (t));
      return (ffelexHandler) ffestb_R10014_;

    case FFELEX_typePLUS:
      ffestb_local_.format.sign = TRUE;	/* Positive. */
      ffestb_local_.format.pre.t = ffelex_token_use (t);
      return (ffelexHandler) ffestb_R10013_;

    case FFELEX_typeMINUS:
      ffestb_local_.format.sign = FALSE;	/* Negative. */
      ffestb_local_.format.pre.t = ffelex_token_use (t);
      return (ffelexHandler) ffestb_R10013_;

    case FFELEX_typeCOLON:
    case FFELEX_typeCOLONCOLON:/* "::". */
    case FFELEX_typeSLASH:
    case FFELEX_typeCONCAT:	/* "//". */
    case FFELEX_typeNAMES:
    case FFELEX_typeDOLLAR:
    case FFELEX_typeOPEN_PAREN:
    case FFELEX_typeOPEN_ARRAY:/* "(/". */
      ffestb_local_.format.sign = FALSE;	/* No sign present. */
      ffestb_local_.format.pre.present = FALSE;
      ffestb_local_.format.pre.rtexpr = FALSE;
      ffestb_local_.format.pre.t = NULL;
      ffestb_local_.format.pre.u.unsigned_val = 1;
      return (ffelexHandler) ffestb_R10014_ (t);

    case FFELEX_typeCOMMA:
      ffebad_start (FFEBAD_FORMAT_EXTRA_COMMA);
      ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
      ffebad_finish ();
      return (ffelexHandler) ffestb_R10012_;

    case FFELEX_typeCLOSE_PAREN:
      ffebad_start (FFEBAD_FORMAT_EXTRA_COMMA);
      ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
      ffebad_finish ();
      f = ffestb_local_.format.f->u.root.parent;
      if (f == NULL)
	return (ffelexHandler) ffestb_R100114_;
      ffestb_local_.format.f = f->next;
      return (ffelexHandler) ffestb_R100111_;

    case FFELEX_typeCLOSE_ARRAY:	/* "/)". */
      f = ffestt_formatlist_append (ffestb_local_.format.f);
      f->type = FFESTP_formattypeSLASH;
      f->t = ffelex_token_use (t);
      f->u.R1010.val.present = FALSE;
      f->u.R1010.val.rtexpr = FALSE;
      f->u.R1010.val.t = NULL;
      f->u.R1010.val.u.unsigned_val = 1;
      f = ffestb_local_.format.f->u.root.parent;
      if (f == NULL)
	return (ffelexHandler) ffestb_R100114_;
      ffestb_local_.format.f = f->next;
      return (ffelexHandler) ffestb_R100111_;

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      ffesta_ffebad_1t (FFEBAD_FORMAT_MISSING_PAREN, t);
      for (f = ffestb_local_.format.f;
	   f->u.root.parent != NULL;
	   f = f->u.root.parent->next)
	;
      ffestb_local_.format.f = f;
      return (ffelexHandler) ffestb_R100114_ (t);

    case FFELEX_typeQUOTE:
      if (ffe_is_vxt ())
	break;			/* Error, probably something like FORMAT("17)
				   = X. */
      ffelex_set_expecting_hollerith (-1, '\"',
				      ffelex_token_where_line (t),
				      ffelex_token_where_column (t));	/* Don't have to unset
									   this one. */
      return (ffelexHandler) ffestb_R100113_;

    case FFELEX_typeAPOSTROPHE:
#if 0				/* No apparent need for this, and not killed
				   anywhere. */
      ffesta_tokens[1] = ffelex_token_use (t);
#endif
      ffelex_set_expecting_hollerith (-1, '\'',
				      ffelex_token_where_line (t),
				      ffelex_token_where_column (t));	/* Don't have to unset
									   this one. */
      return (ffelexHandler) ffestb_R100113_;

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FORMAT", t);
  ffestt_formatlist_kill (ffestb_local_.format.f);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R10013_ -- "FORMAT" OPEN_PAREN [format-item-list] PLUS/MINUS

   return ffestb_R10013_;  // to lexer

   Expect a NUMBER or complain about and then ignore the PLUS/MINUS.  */

static ffelexHandler
ffestb_R10013_ (ffelexToken t)
{
  unsigned long unsigned_val;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNUMBER:
      ffestb_local_.format.pre.present = TRUE;
      ffestb_local_.format.pre.rtexpr = FALSE;
      unsigned_val = strtoul (ffelex_token_text (t), NULL, 10);
      ffestb_local_.format.pre.u.signed_val = ffestb_local_.format.sign
	? unsigned_val : -unsigned_val;
      ffestb_local_.format.sign = TRUE;	/* Sign present. */
      return (ffelexHandler) ffestb_R10014_;

    default:
      ffebad_start (FFEBAD_FORMAT_SPURIOUS_SIGN);
      ffebad_here (0, ffelex_token_where_line (ffestb_local_.format.pre.t),
		   ffelex_token_where_column (ffestb_local_.format.pre.t));
      ffebad_finish ();
      ffelex_token_kill (ffestb_local_.format.pre.t);
      return (ffelexHandler) ffestb_R10012_ (t);
    }
}

/* ffestb_R10014_ -- "FORMAT" OPEN_PAREN [format-item-list] [[+/-] NUMBER]

   return ffestb_R10014_;  // to lexer

   Here is where we expect to see the actual NAMES, COLON, SLASH, OPEN_PAREN,
   OPEN_ARRAY, COLONCOLON, CONCAT, DOLLAR, or HOLLERITH that identifies what
   kind of format-item we're dealing with.  But if we see a NUMBER instead, it
   means free-form spaces number like "5 6 X", so scale the current number
   accordingly and reenter this state.	(I really wouldn't be surprised if
   they change this spacing rule in the F90 spec so that you can't embed
   spaces within numbers or within keywords like BN in a free-source-form
   program.)  */

static ffelexHandler
ffestb_R10014_ (ffelexToken t)
{
  ffesttFormatList f;
  ffeTokenLength i;
  const char *p;
  ffestrFormat kw;

  ffelex_set_expecting_hollerith (0, '\0',
				  ffewhere_line_unknown (),
				  ffewhere_column_unknown ());

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeHOLLERITH:
      f = ffestt_formatlist_append (ffestb_local_.format.f);
      f->type = FFESTP_formattypeR1016;
      f->t = ffelex_token_use (t);
      ffelex_token_kill (ffestb_local_.format.pre.t);	/* It WAS present! */
      return (ffelexHandler) ffestb_R100111_;

    case FFELEX_typeNUMBER:
      assert (ffestb_local_.format.pre.present);
      ffesta_confirmed ();
      if (ffestb_local_.format.pre.rtexpr)
	{
	  ffebad_start (FFEBAD_FORMAT_SPURIOUS_NUMBER);
	  ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_finish ();
	  return (ffelexHandler) ffestb_R10014_;
	}
      if (ffestb_local_.format.sign)
	{
	  for (i = ffelex_token_length (t) + 1; i > 0; --i)
	    ffestb_local_.format.pre.u.signed_val *= 10;
	  ffestb_local_.format.pre.u.signed_val += strtoul (ffelex_token_text (t),
							    NULL, 10);
	}
      else
	{
	  for (i = ffelex_token_length (t) + 1; i > 0; --i)
	    ffestb_local_.format.pre.u.unsigned_val *= 10;
	  ffestb_local_.format.pre.u.unsigned_val += strtoul (ffelex_token_text (t),
							      NULL, 10);
	  ffelex_set_expecting_hollerith (ffestb_local_.format.pre.u.unsigned_val,
					  '\0',
					  ffelex_token_where_line (t),
					  ffelex_token_where_column (t));
	}
      return (ffelexHandler) ffestb_R10014_;

    case FFELEX_typeCOLONCOLON:	/* "::". */
      if (ffestb_local_.format.pre.present)
	{
	  ffesta_ffebad_1t (FFEBAD_FORMAT_BAD_COLON_SPEC,
			    ffestb_local_.format.pre.t);
	  ffelex_token_kill (ffestb_local_.format.pre.t);
	  ffestb_local_.format.pre.present = FALSE;
	}
      else
	{
	  f = ffestt_formatlist_append (ffestb_local_.format.f);
	  f->type = FFESTP_formattypeCOLON;
	  f->t = ffelex_token_use (t);
	  f->u.R1010.val.present = FALSE;
	  f->u.R1010.val.rtexpr = FALSE;
	  f->u.R1010.val.t = NULL;
	  f->u.R1010.val.u.unsigned_val = 1;
	}
      f = ffestt_formatlist_append (ffestb_local_.format.f);
      f->type = FFESTP_formattypeCOLON;
      f->t = ffelex_token_use (t);
      f->u.R1010.val.present = FALSE;
      f->u.R1010.val.rtexpr = FALSE;
      f->u.R1010.val.t = NULL;
      f->u.R1010.val.u.unsigned_val = 1;
      return (ffelexHandler) ffestb_R100112_;

    case FFELEX_typeCOLON:
      if (ffestb_local_.format.pre.present)
	{
	  ffesta_ffebad_1t (FFEBAD_FORMAT_BAD_COLON_SPEC,
			    ffestb_local_.format.pre.t);
	  ffelex_token_kill (ffestb_local_.format.pre.t);
	  return (ffelexHandler) ffestb_R100112_;
	}
      f = ffestt_formatlist_append (ffestb_local_.format.f);
      f->type = FFESTP_formattypeCOLON;
      f->t = ffelex_token_use (t);
      f->u.R1010.val.present = FALSE;
      f->u.R1010.val.rtexpr = FALSE;
      f->u.R1010.val.t = NULL;
      f->u.R1010.val.u.unsigned_val = 1;
      return (ffelexHandler) ffestb_R100112_;

    case FFELEX_typeCONCAT:	/* "//". */
      if (ffestb_local_.format.sign)
	{
	  ffebad_start (FFEBAD_FORMAT_SPURIOUS_SIGN);
	  ffebad_here (0, ffelex_token_where_line (ffestb_local_.format.pre.t),
		    ffelex_token_where_column (ffestb_local_.format.pre.t));
	  ffebad_finish ();
	  ffestb_local_.format.pre.u.unsigned_val
	    = (ffestb_local_.format.pre.u.signed_val < 0)
	    ? -ffestb_local_.format.pre.u.signed_val
	    : ffestb_local_.format.pre.u.signed_val;
	}
      f = ffestt_formatlist_append (ffestb_local_.format.f);
      f->type = FFESTP_formattypeSLASH;
      f->t = ffelex_token_use (t);
      f->u.R1010.val = ffestb_local_.format.pre;
      ffestb_local_.format.pre.present = FALSE;
      ffestb_local_.format.pre.rtexpr = FALSE;
      ffestb_local_.format.pre.t = NULL;
      ffestb_local_.format.pre.u.unsigned_val = 1;
      f = ffestt_formatlist_append (ffestb_local_.format.f);
      f->type = FFESTP_formattypeSLASH;
      f->t = ffelex_token_use (t);
      f->u.R1010.val = ffestb_local_.format.pre;
      return (ffelexHandler) ffestb_R100112_;

    case FFELEX_typeSLASH:
      if (ffestb_local_.format.sign)
	{
	  ffebad_start (FFEBAD_FORMAT_SPURIOUS_SIGN);
	  ffebad_here (0, ffelex_token_where_line (ffestb_local_.format.pre.t),
		    ffelex_token_where_column (ffestb_local_.format.pre.t));
	  ffebad_finish ();
	  ffestb_local_.format.pre.u.unsigned_val
	    = (ffestb_local_.format.pre.u.signed_val < 0)
	    ? -ffestb_local_.format.pre.u.signed_val
	    : ffestb_local_.format.pre.u.signed_val;
	}
      f = ffestt_formatlist_append (ffestb_local_.format.f);
      f->type = FFESTP_formattypeSLASH;
      f->t = ffelex_token_use (t);
      f->u.R1010.val = ffestb_local_.format.pre;
      return (ffelexHandler) ffestb_R100112_;

    case FFELEX_typeOPEN_PAREN:
      if (ffestb_local_.format.sign)
	{
	  ffebad_start (FFEBAD_FORMAT_SPURIOUS_SIGN);
	  ffebad_here (0, ffelex_token_where_line (ffestb_local_.format.pre.t),
		    ffelex_token_where_column (ffestb_local_.format.pre.t));
	  ffebad_finish ();
	  ffestb_local_.format.pre.u.unsigned_val
	    = (ffestb_local_.format.pre.u.signed_val < 0)
	    ? -ffestb_local_.format.pre.u.signed_val
	    : ffestb_local_.format.pre.u.signed_val;
	}
      f = ffestt_formatlist_append (ffestb_local_.format.f);
      f->type = FFESTP_formattypeFORMAT;
      f->t = ffelex_token_use (t);
      f->u.R1003D.R1004 = ffestb_local_.format.pre;
      f->u.R1003D.format = ffestb_local_.format.f
	= ffestt_formatlist_create (f, ffelex_token_use (t));
      return (ffelexHandler) ffestb_R10011_;

    case FFELEX_typeOPEN_ARRAY:/* "(/". */
      if (ffestb_local_.format.sign)
	{
	  ffebad_start (FFEBAD_FORMAT_SPURIOUS_SIGN);
	  ffebad_here (0, ffelex_token_where_line (ffestb_local_.format.pre.t),
		    ffelex_token_where_column (ffestb_local_.format.pre.t));
	  ffebad_finish ();
	  ffestb_local_.format.pre.u.unsigned_val
	    = (ffestb_local_.format.pre.u.signed_val < 0)
	    ? -ffestb_local_.format.pre.u.signed_val
	    : ffestb_local_.format.pre.u.signed_val;
	}
      f = ffestt_formatlist_append (ffestb_local_.format.f);
      f->type = FFESTP_formattypeFORMAT;
      f->t = ffelex_token_use (t);
      f->u.R1003D.R1004 = ffestb_local_.format.pre;
      f->u.R1003D.format = ffestb_local_.format.f
	= ffestt_formatlist_create (f, ffelex_token_use (t));
      f = ffestt_formatlist_append (ffestb_local_.format.f);
      f->type = FFESTP_formattypeSLASH;
      f->t = ffelex_token_use (t);
      f->u.R1010.val.present = FALSE;
      f->u.R1010.val.rtexpr = FALSE;
      f->u.R1010.val.t = NULL;
      f->u.R1010.val.u.unsigned_val = 1;
      return (ffelexHandler) ffestb_R100112_;

    case FFELEX_typeCLOSE_ARRAY:	/* "/)". */
      f = ffestt_formatlist_append (ffestb_local_.format.f);
      f->type = FFESTP_formattypeSLASH;
      f->t = ffelex_token_use (t);
      f->u.R1010.val = ffestb_local_.format.pre;
      f = ffestb_local_.format.f->u.root.parent;
      if (f == NULL)
	return (ffelexHandler) ffestb_R100114_;
      ffestb_local_.format.f = f->next;
      return (ffelexHandler) ffestb_R100111_;

    case FFELEX_typeQUOTE:
      if (ffe_is_vxt ())
	break;			/* A totally bad character in a VXT FORMAT. */
      ffebad_start (FFEBAD_FORMAT_SPURIOUS_NUMBER);
      ffebad_here (0, ffelex_token_where_line (ffestb_local_.format.pre.t),
		   ffelex_token_where_column (ffestb_local_.format.pre.t));
      ffebad_finish ();
      ffelex_token_kill (ffestb_local_.format.pre.t);
      ffesta_confirmed ();
#if 0				/* No apparent need for this, and not killed
				   anywhere. */
      ffesta_tokens[1] = ffelex_token_use (t);
#endif
      ffelex_set_expecting_hollerith (-1, '\"',
				      ffelex_token_where_line (t),
				      ffelex_token_where_column (t));	/* Don't have to unset
									   this one. */
      return (ffelexHandler) ffestb_R100113_;

    case FFELEX_typeAPOSTROPHE:
      ffesta_confirmed ();
      ffebad_start (FFEBAD_FORMAT_SPURIOUS_NUMBER);
      ffebad_here (0, ffelex_token_where_line (ffestb_local_.format.pre.t),
		   ffelex_token_where_column (ffestb_local_.format.pre.t));
      ffebad_finish ();
      ffelex_token_kill (ffestb_local_.format.pre.t);
#if 0				/* No apparent need for this, and not killed
				   anywhere. */
      ffesta_tokens[1] = ffelex_token_use (t);
#endif
      ffelex_set_expecting_hollerith (-1, '\'', ffelex_token_where_line (t),
				      ffelex_token_where_column (t));	/* Don't have to unset
									   this one. */
      return (ffelexHandler) ffestb_R100113_;

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      ffesta_ffebad_1t (FFEBAD_FORMAT_MISSING_PAREN, t);
      for (f = ffestb_local_.format.f;
	   f->u.root.parent != NULL;
	   f = f->u.root.parent->next)
	;
      ffestb_local_.format.f = f;
      ffelex_token_kill (ffestb_local_.format.pre.t);
      return (ffelexHandler) ffestb_R100114_ (t);

    case FFELEX_typeDOLLAR:
      ffestb_local_.format.t = ffelex_token_use (t);
      if (ffestb_local_.format.pre.present)
	ffesta_confirmed ();	/* Number preceding this invalid elsewhere. */
      ffestb_local_.format.current = FFESTP_formattypeDOLLAR;
      return (ffelexHandler) ffestb_R10015_;

    case FFELEX_typeNAMES:
      kw = ffestr_format (t);
      ffestb_local_.format.t = ffelex_token_use (t);
      switch (kw)
	{
	case FFESTR_formatI:
	  if (ffestb_local_.format.pre.present)
	    ffesta_confirmed ();/* Number preceding this invalid elsewhere. */
	  ffestb_local_.format.current = FFESTP_formattypeI;
	  i = FFESTR_formatlI;
	  break;

	case FFESTR_formatB:
	  if (ffestb_local_.format.pre.present)
	    ffesta_confirmed ();/* Number preceding this invalid elsewhere. */
	  ffestb_local_.format.current = FFESTP_formattypeB;
	  i = FFESTR_formatlB;
	  break;

	case FFESTR_formatO:
	  if (ffestb_local_.format.pre.present)
	    ffesta_confirmed ();/* Number preceding this invalid elsewhere. */
	  ffestb_local_.format.current = FFESTP_formattypeO;
	  i = FFESTR_formatlO;
	  break;

	case FFESTR_formatZ:
	  if (ffestb_local_.format.pre.present)
	    ffesta_confirmed ();/* Number preceding this invalid elsewhere. */
	  ffestb_local_.format.current = FFESTP_formattypeZ;
	  i = FFESTR_formatlZ;
	  break;

	case FFESTR_formatF:
	  if (ffestb_local_.format.pre.present)
	    ffesta_confirmed ();/* Number preceding this invalid elsewhere. */
	  ffestb_local_.format.current = FFESTP_formattypeF;
	  i = FFESTR_formatlF;
	  break;

	case FFESTR_formatE:
	  ffestb_local_.format.current = FFESTP_formattypeE;
	  i = FFESTR_formatlE;
	  break;

	case FFESTR_formatEN:
	  if (ffestb_local_.format.pre.present)
	    ffesta_confirmed ();/* Number preceding this invalid elsewhere. */
	  ffestb_local_.format.current = FFESTP_formattypeEN;
	  i = FFESTR_formatlEN;
	  break;

	case FFESTR_formatG:
	  if (ffestb_local_.format.pre.present)
	    ffesta_confirmed ();/* Number preceding this invalid elsewhere. */
	  ffestb_local_.format.current = FFESTP_formattypeG;
	  i = FFESTR_formatlG;
	  break;

	case FFESTR_formatL:
	  if (ffestb_local_.format.pre.present)
	    ffesta_confirmed ();/* Number preceding this invalid elsewhere. */
	  ffestb_local_.format.current = FFESTP_formattypeL;
	  i = FFESTR_formatlL;
	  break;

	case FFESTR_formatA:
	  if (ffestb_local_.format.pre.present)
	    ffesta_confirmed ();/* Number preceding this invalid elsewhere. */
	  ffestb_local_.format.current = FFESTP_formattypeA;
	  i = FFESTR_formatlA;
	  break;

	case FFESTR_formatD:
	  ffestb_local_.format.current = FFESTP_formattypeD;
	  i = FFESTR_formatlD;
	  break;

	case FFESTR_formatQ:
	  ffestb_local_.format.current = FFESTP_formattypeQ;
	  i = FFESTR_formatlQ;
	  break;

	case FFESTR_formatDOLLAR:
	  if (ffestb_local_.format.pre.present)
	    ffesta_confirmed ();/* Number preceding this invalid elsewhere. */
	  ffestb_local_.format.current = FFESTP_formattypeDOLLAR;
	  i = FFESTR_formatlDOLLAR;
	  break;

	case FFESTR_formatP:
	  if (ffestb_local_.format.pre.present)
	    ffesta_confirmed ();/* Number preceding this invalid elsewhere. */
	  ffestb_local_.format.current = FFESTP_formattypeP;
	  i = FFESTR_formatlP;
	  break;

	case FFESTR_formatT:
	  if (ffestb_local_.format.pre.present)
	    ffesta_confirmed ();/* Number preceding this invalid elsewhere. */
	  ffestb_local_.format.current = FFESTP_formattypeT;
	  i = FFESTR_formatlT;
	  break;

	case FFESTR_formatTL:
	  if (ffestb_local_.format.pre.present)
	    ffesta_confirmed ();/* Number preceding this invalid elsewhere. */
	  ffestb_local_.format.current = FFESTP_formattypeTL;
	  i = FFESTR_formatlTL;
	  break;

	case FFESTR_formatTR:
	  if (ffestb_local_.format.pre.present)
	    ffesta_confirmed ();/* Number preceding this invalid elsewhere. */
	  ffestb_local_.format.current = FFESTP_formattypeTR;
	  i = FFESTR_formatlTR;
	  break;

	case FFESTR_formatX:
	  if (ffestb_local_.format.pre.present)
	    ffesta_confirmed ();/* Number preceding this invalid elsewhere. */
	  ffestb_local_.format.current = FFESTP_formattypeX;
	  i = FFESTR_formatlX;
	  break;

	case FFESTR_formatS:
	  if (ffestb_local_.format.pre.present)
	    ffesta_confirmed ();/* Number preceding this invalid elsewhere. */
	  ffestb_local_.format.current = FFESTP_formattypeS;
	  i = FFESTR_formatlS;
	  break;

	case FFESTR_formatSP:
	  if (ffestb_local_.format.pre.present)
	    ffesta_confirmed ();/* Number preceding this invalid elsewhere. */
	  ffestb_local_.format.current = FFESTP_formattypeSP;
	  i = FFESTR_formatlSP;
	  break;

	case FFESTR_formatSS:
	  if (ffestb_local_.format.pre.present)
	    ffesta_confirmed ();/* Number preceding this invalid elsewhere. */
	  ffestb_local_.format.current = FFESTP_formattypeSS;
	  i = FFESTR_formatlSS;
	  break;

	case FFESTR_formatBN:
	  if (ffestb_local_.format.pre.present)
	    ffesta_confirmed ();/* Number preceding this invalid elsewhere. */
	  ffestb_local_.format.current = FFESTP_formattypeBN;
	  i = FFESTR_formatlBN;
	  break;

	case FFESTR_formatBZ:
	  if (ffestb_local_.format.pre.present)
	    ffesta_confirmed ();/* Number preceding this invalid elsewhere. */
	  ffestb_local_.format.current = FFESTP_formattypeBZ;
	  i = FFESTR_formatlBZ;
	  break;

	case FFESTR_formatH:	/* Error, either "H" or "<expr>H". */
	  if (ffestb_local_.format.pre.present)
	    ffesta_confirmed ();/* Number preceding this invalid elsewhere. */
	  ffestb_local_.format.current = FFESTP_formattypeH;
	  i = FFESTR_formatlH;
	  break;

	case FFESTR_formatPD:
	  if (ffestb_local_.format.pre.present)
	    ffesta_confirmed ();/* Number preceding this invalid elsewhere. */
	  ffestb_subr_R1001_append_p_ ();
	  ffestb_local_.format.t = ffelex_token_name_from_names (t,
							FFESTR_formatlP, 1);
	  ffestb_local_.format.sign = FALSE;
	  ffestb_local_.format.pre.present = FALSE;
	  ffestb_local_.format.pre.rtexpr = FALSE;
	  ffestb_local_.format.pre.t = NULL;
	  ffestb_local_.format.pre.u.unsigned_val = 1;
	  ffestb_local_.format.current = FFESTP_formattypeD;
	  i = FFESTR_formatlPD;
	  break;

	case FFESTR_formatPE:
	  if (ffestb_local_.format.pre.present)
	    ffesta_confirmed ();/* Number preceding this invalid elsewhere. */
	  ffestb_subr_R1001_append_p_ ();
	  ffestb_local_.format.t = ffelex_token_name_from_names (t,
							FFESTR_formatlP, 1);
	  ffestb_local_.format.sign = FALSE;
	  ffestb_local_.format.pre.present = FALSE;
	  ffestb_local_.format.pre.rtexpr = FALSE;
	  ffestb_local_.format.pre.t = NULL;
	  ffestb_local_.format.pre.u.unsigned_val = 1;
	  ffestb_local_.format.current = FFESTP_formattypeE;
	  i = FFESTR_formatlPE;
	  break;

	case FFESTR_formatPEN:
	  if (ffestb_local_.format.pre.present)
	    ffesta_confirmed ();/* Number preceding this invalid elsewhere. */
	  ffestb_subr_R1001_append_p_ ();
	  ffestb_local_.format.t = ffelex_token_name_from_names (t,
							FFESTR_formatlP, 1);
	  ffestb_local_.format.sign = FALSE;
	  ffestb_local_.format.pre.present = FALSE;
	  ffestb_local_.format.pre.rtexpr = FALSE;
	  ffestb_local_.format.pre.t = NULL;
	  ffestb_local_.format.pre.u.unsigned_val = 1;
	  ffestb_local_.format.current = FFESTP_formattypeEN;
	  i = FFESTR_formatlPEN;
	  break;

	case FFESTR_formatPF:
	  if (ffestb_local_.format.pre.present)
	    ffesta_confirmed ();/* Number preceding this invalid elsewhere. */
	  ffestb_subr_R1001_append_p_ ();
	  ffestb_local_.format.t = ffelex_token_name_from_names (t,
							FFESTR_formatlP, 1);
	  ffestb_local_.format.sign = FALSE;
	  ffestb_local_.format.pre.present = FALSE;
	  ffestb_local_.format.pre.rtexpr = FALSE;
	  ffestb_local_.format.pre.t = NULL;
	  ffestb_local_.format.pre.u.unsigned_val = 1;
	  ffestb_local_.format.current = FFESTP_formattypeF;
	  i = FFESTR_formatlPF;
	  break;

	case FFESTR_formatPG:
	  if (ffestb_local_.format.pre.present)
	    ffesta_confirmed ();/* Number preceding this invalid elsewhere. */
	  ffestb_subr_R1001_append_p_ ();
	  ffestb_local_.format.t = ffelex_token_name_from_names (t,
							FFESTR_formatlP, 1);
	  ffestb_local_.format.sign = FALSE;
	  ffestb_local_.format.pre.present = FALSE;
	  ffestb_local_.format.pre.rtexpr = FALSE;
	  ffestb_local_.format.pre.t = NULL;
	  ffestb_local_.format.pre.u.unsigned_val = 1;
	  ffestb_local_.format.current = FFESTP_formattypeG;
	  i = FFESTR_formatlPG;
	  break;

	default:
	  if (ffestb_local_.format.pre.present)
	    ffesta_confirmed ();/* Number preceding this invalid elsewhere. */
	  ffestb_local_.format.current = FFESTP_formattypeNone;
	  p = strpbrk (ffelex_token_text (t), "0123456789");
	  if (p == NULL)
	    i = ffelex_token_length (t);
	  else
	    i = p - ffelex_token_text (t);
	  break;
	}
      p = ffelex_token_text (t) + i;
      if (*p == '\0')
	return (ffelexHandler) ffestb_R10015_;
      if (! ISDIGIT (*p))
	{
	  if (ffestb_local_.format.current == FFESTP_formattypeH)
	    p = strpbrk (p, "0123456789");
	  else
	    {
	      p = NULL;
	      ffestb_local_.format.current = FFESTP_formattypeNone;
	    }
	  if (p == NULL)
	    return (ffelexHandler) ffestb_R10015_;
	  i = p - ffelex_token_text (t);	/* Collect digits. */
	}
      ffestb_local_.format.post.present = TRUE;
      ffestb_local_.format.post.rtexpr = FALSE;
      ffestb_local_.format.post.t = ffelex_token_number_from_names (t, i);
      ffestb_local_.format.post.u.unsigned_val
	= strtoul (ffelex_token_text (ffestb_local_.format.post.t), NULL, 10);
      p += ffelex_token_length (ffestb_local_.format.post.t);
      i += ffelex_token_length (ffestb_local_.format.post.t);
      if (*p == '\0')
	return (ffelexHandler) ffestb_R10016_;
      if ((kw != FFESTR_formatP) ||
	  !ffelex_is_firstnamechar ((unsigned char)*p))
	{
	  if (ffestb_local_.format.current != FFESTP_formattypeH)
	    ffesta_ffebad_1p (FFEBAD_FORMAT_TEXT_IN_NUMBER, t, i, NULL);
	  return (ffelexHandler) ffestb_R10016_;
	}

      /* Here we have [number]P[number][text].	Treat as
	 [number]P,[number][text]. */

      ffestb_subr_R1001_append_p_ ();
      t = ffestb_local_.format.t = ffelex_token_names_from_names (t, i, 0);
      ffestb_local_.format.sign = FALSE;
      ffestb_local_.format.pre = ffestb_local_.format.post;
      kw = ffestr_format (t);
      switch (kw)
	{			/* Only a few possibilities here. */
	case FFESTR_formatD:
	  ffestb_local_.format.current = FFESTP_formattypeD;
	  i = FFESTR_formatlD;
	  break;

	case FFESTR_formatE:
	  ffestb_local_.format.current = FFESTP_formattypeE;
	  i = FFESTR_formatlE;
	  break;

	case FFESTR_formatEN:
	  ffestb_local_.format.current = FFESTP_formattypeEN;
	  i = FFESTR_formatlEN;
	  break;

	case FFESTR_formatF:
	  ffestb_local_.format.current = FFESTP_formattypeF;
	  i = FFESTR_formatlF;
	  break;

	case FFESTR_formatG:
	  ffestb_local_.format.current = FFESTP_formattypeG;
	  i = FFESTR_formatlG;
	  break;

	default:
	  ffebad_start (FFEBAD_FORMAT_P_NOCOMMA);
	  ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_finish ();
	  ffestb_local_.format.current = FFESTP_formattypeNone;
	  p = strpbrk (ffelex_token_text (t), "0123456789");
	  if (p == NULL)
	    i = ffelex_token_length (t);
	  else
	    i = p - ffelex_token_text (t);
	}
      p = ffelex_token_text (t) + i;
      if (*p == '\0')
	return (ffelexHandler) ffestb_R10015_;
      if (! ISDIGIT (*p))
	{
	  ffestb_local_.format.current = FFESTP_formattypeNone;
	  p = strpbrk (p, "0123456789");
	  if (p == NULL)
	    return (ffelexHandler) ffestb_R10015_;
	  i = p - ffelex_token_text (t);	/* Collect digits anyway. */
	}
      ffestb_local_.format.post.present = TRUE;
      ffestb_local_.format.post.rtexpr = FALSE;
      ffestb_local_.format.post.t = ffelex_token_number_from_names (t, i);
      ffestb_local_.format.post.u.unsigned_val
	= strtoul (ffelex_token_text (ffestb_local_.format.post.t), NULL, 10);
      p += ffelex_token_length (ffestb_local_.format.post.t);
      i += ffelex_token_length (ffestb_local_.format.post.t);
      if (*p == '\0')
	return (ffelexHandler) ffestb_R10016_;
      ffesta_ffebad_1p (FFEBAD_FORMAT_TEXT_IN_NUMBER, t, i, NULL);
      return (ffelexHandler) ffestb_R10016_;

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FORMAT", t);
  if (ffestb_local_.format.pre.present)
    ffelex_token_kill (ffestb_local_.format.pre.t);
  ffestt_formatlist_kill (ffestb_local_.format.f);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);
}

/* ffestb_R10015_ -- [[+/-] NUMBER] NAMES

   return ffestb_R10015_;  // to lexer

   Here we've gotten at least the initial mnemonic for the edit descriptor.
   We expect either a NUMBER, for the post-mnemonic value, a NAMES, for
   further clarification (in free-form only, sigh) of the mnemonic, or
   anything else.  In all cases we go to _6_, with the difference that for
   NUMBER and NAMES we send the next token rather than the current token.  */

static ffelexHandler
ffestb_R10015_ (ffelexToken t)
{
  bool split_pea;		/* New NAMES requires splitting kP from new
				   edit desc. */
  ffestrFormat kw;
  const char *p;
  ffeTokenLength i;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeOPEN_ANGLE:
      ffesta_confirmed ();
      ffestb_local_.format.post.t = ffelex_token_use (t);
      ffelex_set_names_pure (FALSE);
      if (!ffesta_seen_first_exec && !ffestb_local_.format.complained)
	{
	  ffestb_local_.format.complained = TRUE;
	  ffebad_start (FFEBAD_FORMAT_EXPR_SPEC);
	  ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_finish ();
	}
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		  FFEEXPR_contextFORMAT, (ffeexprCallback) ffestb_R100116_);

    case FFELEX_typeNUMBER:
      ffestb_local_.format.post.present = TRUE;
      ffestb_local_.format.post.rtexpr = FALSE;
      ffestb_local_.format.post.t = ffelex_token_use (t);
      ffestb_local_.format.post.u.unsigned_val
	= strtoul (ffelex_token_text (t), NULL, 10);
      return (ffelexHandler) ffestb_R10016_;

    case FFELEX_typeNAMES:
      ffesta_confirmed ();	/* NAMES " " NAMES invalid elsewhere in
				   free-form. */
      kw = ffestr_format (t);
      switch (ffestb_local_.format.current)
	{
	case FFESTP_formattypeP:
	  split_pea = TRUE;
	  break;

	case FFESTP_formattypeH:	/* An error, maintain this indicator. */
	  kw = FFESTR_formatNone;
	  split_pea = FALSE;
	  break;

	default:
	  split_pea = FALSE;
	  break;
	}

      switch (kw)
	{
	case FFESTR_formatF:
	  switch (ffestb_local_.format.current)
	    {
	    case FFESTP_formattypeP:
	      ffestb_local_.format.current = FFESTP_formattypeF;
	      break;

	    default:
	      ffestb_local_.format.current = FFESTP_formattypeNone;
	      break;
	    }
	  i = FFESTR_formatlF;
	  break;

	case FFESTR_formatE:
	  switch (ffestb_local_.format.current)
	    {
	    case FFESTP_formattypeP:
	      ffestb_local_.format.current = FFESTP_formattypeE;
	      break;

	    default:
	      ffestb_local_.format.current = FFESTP_formattypeNone;
	      break;
	    }
	  i = FFESTR_formatlE;
	  break;

	case FFESTR_formatEN:
	  switch (ffestb_local_.format.current)
	    {
	    case FFESTP_formattypeP:
	      ffestb_local_.format.current = FFESTP_formattypeEN;
	      break;

	    default:
	      ffestb_local_.format.current = FFESTP_formattypeNone;
	      break;
	    }
	  i = FFESTR_formatlEN;
	  break;

	case FFESTR_formatG:
	  switch (ffestb_local_.format.current)
	    {
	    case FFESTP_formattypeP:
	      ffestb_local_.format.current = FFESTP_formattypeG;
	      break;

	    default:
	      ffestb_local_.format.current = FFESTP_formattypeNone;
	      break;
	    }
	  i = FFESTR_formatlG;
	  break;

	case FFESTR_formatL:
	  switch (ffestb_local_.format.current)
	    {
	    case FFESTP_formattypeT:
	      ffestb_local_.format.current = FFESTP_formattypeTL;
	      break;

	    default:
	      ffestb_local_.format.current = FFESTP_formattypeNone;
	      break;
	    }
	  i = FFESTR_formatlL;
	  break;

	case FFESTR_formatD:
	  switch (ffestb_local_.format.current)
	    {
	    case FFESTP_formattypeP:
	      ffestb_local_.format.current = FFESTP_formattypeD;
	      break;

	    default:
	      ffestb_local_.format.current = FFESTP_formattypeNone;
	      break;
	    }
	  i = FFESTR_formatlD;
	  break;

	case FFESTR_formatS:
	  switch (ffestb_local_.format.current)
	    {
	    case FFESTP_formattypeS:
	      ffestb_local_.format.current = FFESTP_formattypeSS;
	      break;

	    default:
	      ffestb_local_.format.current = FFESTP_formattypeNone;
	      break;
	    }
	  i = FFESTR_formatlS;
	  break;

	case FFESTR_formatP:
	  switch (ffestb_local_.format.current)
	    {
	    case FFESTP_formattypeS:
	      ffestb_local_.format.current = FFESTP_formattypeSP;
	      break;

	    default:
	      ffestb_local_.format.current = FFESTP_formattypeNone;
	      break;
	    }
	  i = FFESTR_formatlP;
	  break;

	case FFESTR_formatR:
	  switch (ffestb_local_.format.current)
	    {
	    case FFESTP_formattypeT:
	      ffestb_local_.format.current = FFESTP_formattypeTR;
	      break;

	    default:
	      ffestb_local_.format.current = FFESTP_formattypeNone;
	      break;
	    }
	  i = FFESTR_formatlR;
	  break;

	case FFESTR_formatZ:
	  switch (ffestb_local_.format.current)
	    {
	    case FFESTP_formattypeB:
	      ffestb_local_.format.current = FFESTP_formattypeBZ;
	      break;

	    default:
	      ffestb_local_.format.current = FFESTP_formattypeNone;
	      break;
	    }
	  i = FFESTR_formatlZ;
	  break;

	case FFESTR_formatN:
	  switch (ffestb_local_.format.current)
	    {
	    case FFESTP_formattypeE:
	      ffestb_local_.format.current = FFESTP_formattypeEN;
	      break;

	    case FFESTP_formattypeB:
	      ffestb_local_.format.current = FFESTP_formattypeBN;
	      break;

	    default:
	      ffestb_local_.format.current = FFESTP_formattypeNone;
	      break;
	    }
	  i = FFESTR_formatlN;
	  break;

	default:
	  if (ffestb_local_.format.current != FFESTP_formattypeH)
	    ffestb_local_.format.current = FFESTP_formattypeNone;
	  split_pea = FALSE;	/* Go ahead and let the P be in the party. */
	  p = strpbrk (ffelex_token_text (t), "0123456789");
	  if (p == NULL)
	    i = ffelex_token_length (t);
	  else
	    i = p - ffelex_token_text (t);
	}

      if (split_pea)
	{
	  ffestb_subr_R1001_append_p_ ();
	  ffestb_local_.format.t = ffelex_token_use (t);
	  ffestb_local_.format.sign = FALSE;
	  ffestb_local_.format.pre.present = FALSE;
	  ffestb_local_.format.pre.rtexpr = FALSE;
	  ffestb_local_.format.pre.t = NULL;
	  ffestb_local_.format.pre.u.unsigned_val = 1;
	}

      p = ffelex_token_text (t) + i;
      if (*p == '\0')
	return (ffelexHandler) ffestb_R10015_;
      if (! ISDIGIT (*p))
	{
	  ffestb_local_.format.current = FFESTP_formattypeNone;
	  p = strpbrk (p, "0123456789");
	  if (p == NULL)
	    return (ffelexHandler) ffestb_R10015_;
	  i = p - ffelex_token_text (t);	/* Collect digits anyway. */
	}
      ffestb_local_.format.post.present = TRUE;
      ffestb_local_.format.post.rtexpr = FALSE;
      ffestb_local_.format.post.t = ffelex_token_number_from_names (t, i);
      ffestb_local_.format.post.u.unsigned_val
	= strtoul (ffelex_token_text (ffestb_local_.format.post.t), NULL, 10);
      p += ffelex_token_length (ffestb_local_.format.post.t);
      i += ffelex_token_length (ffestb_local_.format.post.t);
      if (*p == '\0')
	return (ffelexHandler) ffestb_R10016_;
      ffesta_ffebad_1p (FFEBAD_FORMAT_TEXT_IN_NUMBER, t, i, NULL);
      return (ffelexHandler) ffestb_R10016_;

    default:
      ffestb_local_.format.post.present = FALSE;
      ffestb_local_.format.post.rtexpr = FALSE;
      ffestb_local_.format.post.t = NULL;
      ffestb_local_.format.post.u.unsigned_val = 1;
      return (ffelexHandler) ffestb_R10016_ (t);
    }
}

/* ffestb_R10016_ -- [[+/-] NUMBER] NAMES NUMBER

   return ffestb_R10016_;  // to lexer

   Expect a PERIOD here.  Maybe find a NUMBER to append to the current
   number, in which case return to this state.	Maybe find a NAMES to switch
   from a kP descriptor to a new descriptor (else the NAMES is spurious),
   in which case generator the P item and go to state _4_.  Anything
   else, pass token on to state _8_.  */

static ffelexHandler
ffestb_R10016_ (ffelexToken t)
{
  ffeTokenLength i;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typePERIOD:
      return (ffelexHandler) ffestb_R10017_;

    case FFELEX_typeNUMBER:
      assert (ffestb_local_.format.post.present);
      ffesta_confirmed ();
      if (ffestb_local_.format.post.rtexpr)
	{
	  ffebad_start (FFEBAD_FORMAT_SPURIOUS_NUMBER);
	  ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_finish ();
	  return (ffelexHandler) ffestb_R10016_;
	}
      for (i = ffelex_token_length (t) + 1; i > 0; --i)
	ffestb_local_.format.post.u.unsigned_val *= 10;
      ffestb_local_.format.post.u.unsigned_val += strtoul (ffelex_token_text (t),
							   NULL, 10);
      return (ffelexHandler) ffestb_R10016_;

    case FFELEX_typeNAMES:
      ffesta_confirmed ();	/* NUMBER " " NAMES invalid elsewhere. */
      if (ffestb_local_.format.current != FFESTP_formattypeP)
	{
	  ffesta_ffebad_1t (FFEBAD_FORMAT_TEXT_IN_NUMBER, t);
	  return (ffelexHandler) ffestb_R10016_;
	}
      ffestb_subr_R1001_append_p_ ();
      ffestb_local_.format.sign = FALSE;
      ffestb_local_.format.pre = ffestb_local_.format.post;
      return (ffelexHandler) ffestb_R10014_ (t);

    default:
      ffestb_local_.format.dot.present = FALSE;
      ffestb_local_.format.dot.rtexpr = FALSE;
      ffestb_local_.format.dot.t = NULL;
      ffestb_local_.format.dot.u.unsigned_val = 1;
      return (ffelexHandler) ffestb_R10018_ (t);
    }
}

/* ffestb_R10017_ -- [[+/-] NUMBER] NAMES NUMBER PERIOD

   return ffestb_R10017_;  // to lexer

   Here we've gotten the period following the edit descriptor.
   We expect either a NUMBER, for the dot value, or something else, which
   probably means we're not even close to being in a real FORMAT statement.  */

static ffelexHandler
ffestb_R10017_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeOPEN_ANGLE:
      ffestb_local_.format.dot.t = ffelex_token_use (t);
      ffelex_set_names_pure (FALSE);
      if (!ffesta_seen_first_exec && !ffestb_local_.format.complained)
	{
	  ffestb_local_.format.complained = TRUE;
	  ffebad_start (FFEBAD_FORMAT_EXPR_SPEC);
	  ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_finish ();
	}
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		  FFEEXPR_contextFORMAT, (ffeexprCallback) ffestb_R100117_);

    case FFELEX_typeNUMBER:
      ffestb_local_.format.dot.present = TRUE;
      ffestb_local_.format.dot.rtexpr = FALSE;
      ffestb_local_.format.dot.t = ffelex_token_use (t);
      ffestb_local_.format.dot.u.unsigned_val
	= strtoul (ffelex_token_text (t), NULL, 10);
      return (ffelexHandler) ffestb_R10018_;

    default:
      ffelex_token_kill (ffestb_local_.format.t);
      if (ffestb_local_.format.pre.present)
	ffelex_token_kill (ffestb_local_.format.pre.t);
      if (ffestb_local_.format.post.present)
	ffelex_token_kill (ffestb_local_.format.post.t);
      ffesta_ffebad_1t (FFEBAD_FORMAT_MISSING_DOT, t);
      ffestt_formatlist_kill (ffestb_local_.format.f);
      return (ffelexHandler) ffelex_swallow_tokens (t,
					       (ffelexHandler) ffesta_zero);
    }
}

/* ffestb_R10018_ -- [[+/-] NUMBER] NAMES NUMBER PERIOD NUMBER

   return ffestb_R10018_;  // to lexer

   Expect a NAMES here, which must begin with "E" to be valid.	Maybe find a
   NUMBER to append to the current number, in which case return to this state.
   Anything else, pass token on to state _10_.	*/

static ffelexHandler
ffestb_R10018_ (ffelexToken t)
{
  ffeTokenLength i;
  const char *p;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNUMBER:
      assert (ffestb_local_.format.dot.present);
      ffesta_confirmed ();
      if (ffestb_local_.format.dot.rtexpr)
	{
	  ffebad_start (FFEBAD_FORMAT_SPURIOUS_NUMBER);
	  ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_finish ();
	  return (ffelexHandler) ffestb_R10018_;
	}
      for (i = ffelex_token_length (t) + 1; i > 0; --i)
	ffestb_local_.format.dot.u.unsigned_val *= 10;
      ffestb_local_.format.dot.u.unsigned_val += strtoul (ffelex_token_text (t),
							  NULL, 10);
      return (ffelexHandler) ffestb_R10018_;

    case FFELEX_typeNAMES:
      if (!ffesrc_char_match_init (*(p = ffelex_token_text (t)), 'E', 'e'))
	{
	  ffesta_ffebad_1t (FFEBAD_FORMAT_TEXT_IN_NUMBER, t);
	  return (ffelexHandler) ffestb_R10018_;
	}
      if (*++p == '\0')
	return (ffelexHandler) ffestb_R10019_;	/* Go get NUMBER. */
      i = 1;
      if (! ISDIGIT (*p))
	{
	  ffesta_ffebad_1p (FFEBAD_FORMAT_TEXT_IN_NUMBER, t, 1, NULL);
	  return (ffelexHandler) ffestb_R10018_;
	}
      ffestb_local_.format.exp.present = TRUE;
      ffestb_local_.format.exp.rtexpr = FALSE;
      ffestb_local_.format.exp.t = ffelex_token_number_from_names (t, i);
      ffestb_local_.format.exp.u.unsigned_val
	= strtoul (ffelex_token_text (ffestb_local_.format.exp.t), NULL, 10);
      p += ffelex_token_length (ffestb_local_.format.exp.t);
      i += ffelex_token_length (ffestb_local_.format.exp.t);
      if (*p == '\0')
	return (ffelexHandler) ffestb_R100110_;
      ffesta_ffebad_1p (FFEBAD_FORMAT_TEXT_IN_NUMBER, t, i, NULL);
      return (ffelexHandler) ffestb_R100110_;

    default:
      ffestb_local_.format.exp.present = FALSE;
      ffestb_local_.format.exp.rtexpr = FALSE;
      ffestb_local_.format.exp.t = NULL;
      ffestb_local_.format.exp.u.unsigned_val = 1;
      return (ffelexHandler) ffestb_R100110_ (t);
    }
}

/* ffestb_R10019_ -- [[+/-] NUMBER] NAMES NUMBER PERIOD NUMBER "E"

   return ffestb_R10019_;  // to lexer

   Here we've gotten the "E" following the edit descriptor.
   We expect either a NUMBER, for the exponent value, or something else.  */

static ffelexHandler
ffestb_R10019_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeOPEN_ANGLE:
      ffestb_local_.format.exp.t = ffelex_token_use (t);
      ffelex_set_names_pure (FALSE);
      if (!ffesta_seen_first_exec && !ffestb_local_.format.complained)
	{
	  ffestb_local_.format.complained = TRUE;
	  ffebad_start (FFEBAD_FORMAT_EXPR_SPEC);
	  ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_finish ();
	}
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		  FFEEXPR_contextFORMAT, (ffeexprCallback) ffestb_R100118_);

    case FFELEX_typeNUMBER:
      ffestb_local_.format.exp.present = TRUE;
      ffestb_local_.format.exp.rtexpr = FALSE;
      ffestb_local_.format.exp.t = ffelex_token_use (t);
      ffestb_local_.format.exp.u.unsigned_val
	= strtoul (ffelex_token_text (t), NULL, 10);
      return (ffelexHandler) ffestb_R100110_;

    default:
      ffelex_token_kill (ffestb_local_.format.t);
      if (ffestb_local_.format.pre.present)
	ffelex_token_kill (ffestb_local_.format.pre.t);
      if (ffestb_local_.format.post.present)
	ffelex_token_kill (ffestb_local_.format.post.t);
      if (ffestb_local_.format.dot.present)
	ffelex_token_kill (ffestb_local_.format.dot.t);
      ffesta_ffebad_1t (FFEBAD_FORMAT_MISSING_EXP, t);
      ffestt_formatlist_kill (ffestb_local_.format.f);
      return (ffelexHandler) ffelex_swallow_tokens (t,
					       (ffelexHandler) ffesta_zero);
    }
}

/* ffestb_R100110_ -- [[+/-] NUMBER] NAMES NUMBER [PERIOD NUMBER ["E" NUMBER]]

   return ffestb_R100110_;  // to lexer

   Maybe find a NUMBER to append to the current number, in which case return
   to this state.  Anything else, handle current descriptor, then pass token
   on to state _10_.  */

static ffelexHandler
ffestb_R100110_ (ffelexToken t)
{
  ffeTokenLength i;
  enum expect
    {
      required,
      optional,
      disallowed
    };
  ffebad err;
  enum expect pre;
  enum expect post;
  enum expect dot;
  enum expect exp;
  bool R1005;
  ffesttFormatList f;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNUMBER:
      assert (ffestb_local_.format.exp.present);
      ffesta_confirmed ();
      if (ffestb_local_.format.exp.rtexpr)
	{
	  ffebad_start (FFEBAD_FORMAT_SPURIOUS_NUMBER);
	  ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_finish ();
	  return (ffelexHandler) ffestb_R100110_;
	}
      for (i = ffelex_token_length (t) + 1; i > 0; --i)
	ffestb_local_.format.exp.u.unsigned_val *= 10;
      ffestb_local_.format.exp.u.unsigned_val += strtoul (ffelex_token_text (t),
							  NULL, 10);
      return (ffelexHandler) ffestb_R100110_;

    default:
      if (ffestb_local_.format.sign
	  && (ffestb_local_.format.current != FFESTP_formattypeP)
	  && (ffestb_local_.format.current != FFESTP_formattypeH))
	{
	  ffebad_start (FFEBAD_FORMAT_SPURIOUS_SIGN);
	  ffebad_here (0, ffelex_token_where_line (ffestb_local_.format.pre.t),
		    ffelex_token_where_column (ffestb_local_.format.pre.t));
	  ffebad_finish ();
	  ffestb_local_.format.pre.u.unsigned_val
	    = (ffestb_local_.format.pre.u.signed_val < 0)
	    ? -ffestb_local_.format.pre.u.signed_val
	    : ffestb_local_.format.pre.u.signed_val;
	}
      switch (ffestb_local_.format.current)
	{
	case FFESTP_formattypeI:
	  err = FFEBAD_FORMAT_BAD_I_SPEC;
	  pre = optional;
	  post = required;
	  dot = optional;
	  exp = disallowed;
	  R1005 = TRUE;
	  break;

	case FFESTP_formattypeB:
	  err = FFEBAD_FORMAT_BAD_B_SPEC;
	  pre = optional;
	  post = required;
	  dot = optional;
	  exp = disallowed;
	  R1005 = TRUE;
	  break;

	case FFESTP_formattypeO:
	  err = FFEBAD_FORMAT_BAD_O_SPEC;
	  pre = optional;
	  post = required;
	  dot = optional;
	  exp = disallowed;
	  R1005 = TRUE;
	  break;

	case FFESTP_formattypeZ:
	  err = FFEBAD_FORMAT_BAD_Z_SPEC;
	  pre = optional;
	  post = required;
	  dot = optional;
	  exp = disallowed;
	  R1005 = TRUE;
	  break;

	case FFESTP_formattypeF:
	  err = FFEBAD_FORMAT_BAD_F_SPEC;
	  pre = optional;
	  post = required;
	  dot = required;
	  exp = disallowed;
	  R1005 = TRUE;
	  break;

	case FFESTP_formattypeE:
	  err = FFEBAD_FORMAT_BAD_E_SPEC;
	  pre = optional;
	  post = required;
	  dot = required;
	  exp = optional;
	  R1005 = TRUE;
	  break;

	case FFESTP_formattypeEN:
	  err = FFEBAD_FORMAT_BAD_EN_SPEC;
	  pre = optional;
	  post = required;
	  dot = required;
	  exp = optional;
	  R1005 = TRUE;
	  break;

	case FFESTP_formattypeG:
	  err = FFEBAD_FORMAT_BAD_G_SPEC;
	  pre = optional;
	  post = required;
	  dot = required;
	  exp = optional;
	  R1005 = TRUE;
	  break;

	case FFESTP_formattypeL:
	  err = FFEBAD_FORMAT_BAD_L_SPEC;
	  pre = optional;
	  post = required;
	  dot = disallowed;
	  exp = disallowed;
	  R1005 = TRUE;
	  break;

	case FFESTP_formattypeA:
	  err = FFEBAD_FORMAT_BAD_A_SPEC;
	  pre = optional;
	  post = optional;
	  dot = disallowed;
	  exp = disallowed;
	  R1005 = TRUE;
	  break;

	case FFESTP_formattypeD:
	  err = FFEBAD_FORMAT_BAD_D_SPEC;
	  pre = optional;
	  post = required;
	  dot = required;
	  exp = disallowed;
	  R1005 = TRUE;
	  break;

	case FFESTP_formattypeQ:
	  err = FFEBAD_FORMAT_BAD_Q_SPEC;
	  pre = disallowed;
	  post = disallowed;
	  dot = disallowed;
	  exp = disallowed;
	  R1005 = FALSE;
	  break;

	case FFESTP_formattypeDOLLAR:
	  err = FFEBAD_FORMAT_BAD_DOLLAR_SPEC;
	  pre = disallowed;
	  post = disallowed;
	  dot = disallowed;
	  exp = disallowed;
	  R1005 = FALSE;
	  break;

	case FFESTP_formattypeP:
	  err = FFEBAD_FORMAT_BAD_P_SPEC;
	  pre = required;
	  post = disallowed;
	  dot = disallowed;
	  exp = disallowed;
	  R1005 = FALSE;
	  break;

	case FFESTP_formattypeT:
	  err = FFEBAD_FORMAT_BAD_T_SPEC;
	  pre = disallowed;
	  post = required;
	  dot = disallowed;
	  exp = disallowed;
	  R1005 = FALSE;
	  break;

	case FFESTP_formattypeTL:
	  err = FFEBAD_FORMAT_BAD_TL_SPEC;
	  pre = disallowed;
	  post = required;
	  dot = disallowed;
	  exp = disallowed;
	  R1005 = FALSE;
	  break;

	case FFESTP_formattypeTR:
	  err = FFEBAD_FORMAT_BAD_TR_SPEC;
	  pre = disallowed;
	  post = required;
	  dot = disallowed;
	  exp = disallowed;
	  R1005 = FALSE;
	  break;

	case FFESTP_formattypeX:
	  err = FFEBAD_FORMAT_BAD_X_SPEC;
	  pre = required;
	  post = disallowed;
	  dot = disallowed;
	  exp = disallowed;
	  R1005 = FALSE;
	  break;

	case FFESTP_formattypeS:
	  err = FFEBAD_FORMAT_BAD_S_SPEC;
	  pre = disallowed;
	  post = disallowed;
	  dot = disallowed;
	  exp = disallowed;
	  R1005 = FALSE;
	  break;

	case FFESTP_formattypeSP:
	  err = FFEBAD_FORMAT_BAD_SP_SPEC;
	  pre = disallowed;
	  post = disallowed;
	  dot = disallowed;
	  exp = disallowed;
	  R1005 = FALSE;
	  break;

	case FFESTP_formattypeSS:
	  err = FFEBAD_FORMAT_BAD_SS_SPEC;
	  pre = disallowed;
	  post = disallowed;
	  dot = disallowed;
	  exp = disallowed;
	  R1005 = FALSE;
	  break;

	case FFESTP_formattypeBN:
	  err = FFEBAD_FORMAT_BAD_BN_SPEC;
	  pre = disallowed;
	  post = disallowed;
	  dot = disallowed;
	  exp = disallowed;
	  R1005 = FALSE;
	  break;

	case FFESTP_formattypeBZ:
	  err = FFEBAD_FORMAT_BAD_BZ_SPEC;
	  pre = disallowed;
	  post = disallowed;
	  dot = disallowed;
	  exp = disallowed;
	  R1005 = FALSE;
	  break;

	case FFESTP_formattypeH:	/* Definitely an error, make sure of
					   it. */
	  err = FFEBAD_FORMAT_BAD_H_SPEC;
	  pre = ffestb_local_.format.pre.present ? disallowed : required;
	  post = disallowed;
	  dot = disallowed;
	  exp = disallowed;
	  R1005 = FALSE;
	  break;

	case FFESTP_formattypeNone:
	  ffesta_ffebad_1t (FFEBAD_FORMAT_BAD_SPEC,
			    ffestb_local_.format.t);

	clean_up_to_11_:	/* :::::::::::::::::::: */

	  ffelex_token_kill (ffestb_local_.format.t);
	  if (ffestb_local_.format.pre.present)
	    ffelex_token_kill (ffestb_local_.format.pre.t);
	  if (ffestb_local_.format.post.present)
	    ffelex_token_kill (ffestb_local_.format.post.t);
	  if (ffestb_local_.format.dot.present)
	    ffelex_token_kill (ffestb_local_.format.dot.t);
	  if (ffestb_local_.format.exp.present)
	    ffelex_token_kill (ffestb_local_.format.exp.t);
	  return (ffelexHandler) ffestb_R100111_ (t);

	default:
	  assert ("bad format item" == NULL);
	  err = FFEBAD_FORMAT_BAD_H_SPEC;
	  pre = disallowed;
	  post = disallowed;
	  dot = disallowed;
	  exp = disallowed;
	  R1005 = FALSE;
	  break;
	}
      if (((pre == disallowed) && ffestb_local_.format.pre.present)
	  || ((pre == required) && !ffestb_local_.format.pre.present))
	{
	  ffesta_ffebad_1t (err, (pre == required)
		     ? ffestb_local_.format.t : ffestb_local_.format.pre.t);
	  goto clean_up_to_11_;	/* :::::::::::::::::::: */
	}
      if (((post == disallowed) && ffestb_local_.format.post.present)
	  || ((post == required) && !ffestb_local_.format.post.present))
	{
	  ffesta_ffebad_1t (err, (post == required)
		    ? ffestb_local_.format.t : ffestb_local_.format.post.t);
	  goto clean_up_to_11_;	/* :::::::::::::::::::: */
	}
      if (((dot == disallowed) && ffestb_local_.format.dot.present)
	  || ((dot == required) && !ffestb_local_.format.dot.present))
	{
	  ffesta_ffebad_1t (err, (dot == required)
		     ? ffestb_local_.format.t : ffestb_local_.format.dot.t);
	  goto clean_up_to_11_;	/* :::::::::::::::::::: */
	}
      if (((exp == disallowed) && ffestb_local_.format.exp.present)
	  || ((exp == required) && !ffestb_local_.format.exp.present))
	{
	  ffesta_ffebad_1t (err, (exp == required)
		     ? ffestb_local_.format.t : ffestb_local_.format.exp.t);
	  goto clean_up_to_11_;	/* :::::::::::::::::::: */
	}
      f = ffestt_formatlist_append (ffestb_local_.format.f);
      f->type = ffestb_local_.format.current;
      f->t = ffestb_local_.format.t;
      if (R1005)
	{
	  f->u.R1005.R1004 = ffestb_local_.format.pre;
	  f->u.R1005.R1006 = ffestb_local_.format.post;
	  f->u.R1005.R1007_or_R1008 = ffestb_local_.format.dot;
	  f->u.R1005.R1009 = ffestb_local_.format.exp;
	}
      else
	/* Must be R1010. */
	{
	  if (pre == disallowed)
	    f->u.R1010.val = ffestb_local_.format.post;
	  else
	    f->u.R1010.val = ffestb_local_.format.pre;
	}
      return (ffelexHandler) ffestb_R100111_ (t);
    }
}

/* ffestb_R100111_ -- edit-descriptor

   return ffestb_R100111_;  // to lexer

   Expect a COMMA, CLOSE_PAREN, CLOSE_ARRAY, COLON, COLONCOLON, SLASH, or
   CONCAT, or complain about missing comma.  */

static ffelexHandler
ffestb_R100111_ (ffelexToken t)
{
  ffesttFormatList f;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      return (ffelexHandler) ffestb_R10012_;

    case FFELEX_typeCOLON:
    case FFELEX_typeCOLONCOLON:
    case FFELEX_typeSLASH:
    case FFELEX_typeCONCAT:
      return (ffelexHandler) ffestb_R10012_ (t);

    case FFELEX_typeCLOSE_PAREN:
      f = ffestb_local_.format.f->u.root.parent;
      if (f == NULL)
	return (ffelexHandler) ffestb_R100114_;
      ffestb_local_.format.f = f->next;
      return (ffelexHandler) ffestb_R100111_;

    case FFELEX_typeCLOSE_ARRAY:	/* "/)". */
      f = ffestt_formatlist_append (ffestb_local_.format.f);
      f->type = FFESTP_formattypeSLASH;
      f->t = ffelex_token_use (t);
      f->u.R1010.val.present = FALSE;
      f->u.R1010.val.rtexpr = FALSE;
      f->u.R1010.val.t = NULL;
      f->u.R1010.val.u.unsigned_val = 1;
      f = ffestb_local_.format.f->u.root.parent;
      if (f == NULL)
	return (ffelexHandler) ffestb_R100114_;
      ffestb_local_.format.f = f->next;
      return (ffelexHandler) ffestb_R100111_;

    case FFELEX_typeOPEN_ANGLE:
    case FFELEX_typeDOLLAR:
    case FFELEX_typeNUMBER:
    case FFELEX_typeOPEN_PAREN:
    case FFELEX_typeOPEN_ARRAY:
    case FFELEX_typeQUOTE:
    case FFELEX_typeAPOSTROPHE:
    case FFELEX_typeNAMES:
      ffesta_ffebad_1t (FFEBAD_FORMAT_MISSING_COMMA, t);
      return (ffelexHandler) ffestb_R10012_ (t);

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      ffesta_ffebad_1t (FFEBAD_FORMAT_MISSING_PAREN, t);
      for (f = ffestb_local_.format.f;
	   f->u.root.parent != NULL;
	   f = f->u.root.parent->next)
	;
      ffestb_local_.format.f = f;
      return (ffelexHandler) ffestb_R100114_ (t);

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FORMAT", t);
      ffestt_formatlist_kill (ffestb_local_.format.f);
      return (ffelexHandler) ffelex_swallow_tokens (t,
					       (ffelexHandler) ffesta_zero);
    }
}

/* ffestb_R100112_ -- COLON, COLONCOLON, SLASH, OPEN_ARRAY, or CONCAT

   return ffestb_R100112_;  // to lexer

   Like _11_ except the COMMA is optional.  */

static ffelexHandler
ffestb_R100112_ (ffelexToken t)
{
  ffesttFormatList f;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      return (ffelexHandler) ffestb_R10012_;

    case FFELEX_typeCOLON:
    case FFELEX_typeCOLONCOLON:
    case FFELEX_typeSLASH:
    case FFELEX_typeCONCAT:
    case FFELEX_typeOPEN_ANGLE:
    case FFELEX_typeNAMES:
    case FFELEX_typeDOLLAR:
    case FFELEX_typeNUMBER:
    case FFELEX_typeOPEN_PAREN:
    case FFELEX_typeOPEN_ARRAY:
    case FFELEX_typeQUOTE:
    case FFELEX_typeAPOSTROPHE:
    case FFELEX_typePLUS:
    case FFELEX_typeMINUS:
      return (ffelexHandler) ffestb_R10012_ (t);

    case FFELEX_typeCLOSE_PAREN:
      f = ffestb_local_.format.f->u.root.parent;
      if (f == NULL)
	return (ffelexHandler) ffestb_R100114_;
      ffestb_local_.format.f = f->next;
      return (ffelexHandler) ffestb_R100111_;

    case FFELEX_typeCLOSE_ARRAY:	/* "/)". */
      f = ffestt_formatlist_append (ffestb_local_.format.f);
      f->type = FFESTP_formattypeSLASH;
      f->t = ffelex_token_use (t);
      f->u.R1010.val.present = FALSE;
      f->u.R1010.val.rtexpr = FALSE;
      f->u.R1010.val.t = NULL;
      f->u.R1010.val.u.unsigned_val = 1;
      f = ffestb_local_.format.f->u.root.parent;
      if (f == NULL)
	return (ffelexHandler) ffestb_R100114_;
      ffestb_local_.format.f = f->next;
      return (ffelexHandler) ffestb_R100111_;

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      ffesta_ffebad_1t (FFEBAD_FORMAT_MISSING_PAREN, t);
      for (f = ffestb_local_.format.f;
	   f->u.root.parent != NULL;
	   f = f->u.root.parent->next)
	;
      ffestb_local_.format.f = f;
      return (ffelexHandler) ffestb_R100114_ (t);

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FORMAT", t);
      ffestt_formatlist_kill (ffestb_local_.format.f);
      return (ffelexHandler) ffelex_swallow_tokens (t,
					       (ffelexHandler) ffesta_zero);
    }
}

/* ffestb_R100113_ -- Handle CHARACTER token.

   return ffestb_R100113_;  // to lexer

   Append the format item to the list, go to _11_.  */

static ffelexHandler
ffestb_R100113_ (ffelexToken t)
{
  ffesttFormatList f;

  assert (ffelex_token_type (t) == FFELEX_typeCHARACTER);

  if (ffe_is_pedantic_not_90 () && (ffelex_token_length (t) == 0))
    {
      ffebad_start (FFEBAD_NULL_CHAR_CONST);
      ffebad_here (0, ffelex_token_where_line (t),
		   ffelex_token_where_column (t));
      ffebad_finish ();
    }

  f = ffestt_formatlist_append (ffestb_local_.format.f);
  f->type = FFESTP_formattypeR1016;
  f->t = ffelex_token_use (t);
  return (ffelexHandler) ffestb_R100111_;
}

/* ffestb_R100114_ -- "FORMAT" OPEN_PAREN format-item-list CLOSE_PAREN

   return ffestb_R100114_;  // to lexer

   Handle EOS/SEMICOLON or something else.  */

static ffelexHandler
ffestb_R100114_ (ffelexToken t)
{
  ffelex_set_names_pure (FALSE);

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited () && !ffestb_local_.format.complained)
	ffestc_R1001 (ffestb_local_.format.f);
      ffestt_formatlist_kill (ffestb_local_.format.f);
      return (ffelexHandler) ffesta_zero (t);

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FORMAT", t);
      ffestt_formatlist_kill (ffestb_local_.format.f);
      return (ffelexHandler) ffelex_swallow_tokens (t,
					       (ffelexHandler) ffesta_zero);
    }
}

/* ffestb_R100115_ -- OPEN_ANGLE expr

   (ffestb_R100115_)  // to expression handler

   Handle expression prior to the edit descriptor.  */

static ffelexHandler
ffestb_R100115_ (ffelexToken ft UNUSED, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCLOSE_ANGLE:
      ffestb_local_.format.pre.present = TRUE;
      ffestb_local_.format.pre.rtexpr = TRUE;
      ffestb_local_.format.pre.u.expr = expr;
      ffelex_set_names_pure (TRUE);
      return (ffelexHandler) ffestb_R10014_;

    default:
      ffelex_token_kill (ffestb_local_.format.pre.t);
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FORMAT", t);
      ffestt_formatlist_kill (ffestb_local_.format.f);
      return (ffelexHandler) ffelex_swallow_tokens (t,
					       (ffelexHandler) ffesta_zero);
    }
}

/* ffestb_R100116_ -- "[n]X" OPEN_ANGLE expr

   (ffestb_R100116_)  // to expression handler

   Handle expression after the edit descriptor.	 */

static ffelexHandler
ffestb_R100116_ (ffelexToken ft UNUSED, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCLOSE_ANGLE:
      ffestb_local_.format.post.present = TRUE;
      ffestb_local_.format.post.rtexpr = TRUE;
      ffestb_local_.format.post.u.expr = expr;
      ffelex_set_names_pure (TRUE);
      return (ffelexHandler) ffestb_R10016_;

    default:
      ffelex_token_kill (ffestb_local_.format.t);
      ffelex_token_kill (ffestb_local_.format.post.t);
      if (ffestb_local_.format.pre.present)
	ffelex_token_kill (ffestb_local_.format.pre.t);
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FORMAT", t);
      ffestt_formatlist_kill (ffestb_local_.format.f);
      return (ffelexHandler) ffelex_swallow_tokens (t,
					       (ffelexHandler) ffesta_zero);
    }
}

/* ffestb_R100117_ -- "[n]X[n]." OPEN_ANGLE expr

   (ffestb_R100117_)  // to expression handler

   Handle expression after the PERIOD.	*/

static ffelexHandler
ffestb_R100117_ (ffelexToken ft UNUSED, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCLOSE_ANGLE:
      ffestb_local_.format.dot.present = TRUE;
      ffestb_local_.format.dot.rtexpr = TRUE;
      ffestb_local_.format.dot.u.expr = expr;
      ffelex_set_names_pure (TRUE);
      return (ffelexHandler) ffestb_R10018_;

    default:
      ffelex_token_kill (ffestb_local_.format.t);
      ffelex_token_kill (ffestb_local_.format.dot.t);
      if (ffestb_local_.format.pre.present)
	ffelex_token_kill (ffestb_local_.format.pre.t);
      if (ffestb_local_.format.post.present)
	ffelex_token_kill (ffestb_local_.format.post.t);
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FORMAT", t);
      ffestt_formatlist_kill (ffestb_local_.format.f);
      return (ffelexHandler) ffelex_swallow_tokens (t,
					       (ffelexHandler) ffesta_zero);
    }
}

/* ffestb_R100118_ -- "[n]X[n].[n]E" OPEN_ANGLE expr

   (ffestb_R100118_)  // to expression handler

   Handle expression after the "E".  */

static ffelexHandler
ffestb_R100118_ (ffelexToken ft UNUSED, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCLOSE_ANGLE:
      ffestb_local_.format.exp.present = TRUE;
      ffestb_local_.format.exp.rtexpr = TRUE;
      ffestb_local_.format.exp.u.expr = expr;
      ffelex_set_names_pure (TRUE);
      return (ffelexHandler) ffestb_R100110_;

    default:
      ffelex_token_kill (ffestb_local_.format.t);
      ffelex_token_kill (ffestb_local_.format.exp.t);
      if (ffestb_local_.format.pre.present)
	ffelex_token_kill (ffestb_local_.format.pre.t);
      if (ffestb_local_.format.post.present)
	ffelex_token_kill (ffestb_local_.format.post.t);
      if (ffestb_local_.format.dot.present)
	ffelex_token_kill (ffestb_local_.format.dot.t);
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FORMAT", t);
      ffestt_formatlist_kill (ffestb_local_.format.f);
      return (ffelexHandler) ffelex_swallow_tokens (t,
					       (ffelexHandler) ffesta_zero);
    }
}

/* ffestb_R1107 -- Parse the USE statement

   return ffestb_R1107;	 // to lexer

   Make sure the statement has a valid form for the USE statement.
   If it does, implement the statement.	 */

#if FFESTR_F90
ffelexHandler
ffestb_R1107 (ffelexToken t)
{
  ffeTokenLength i;
  const char *p;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstUSE)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeNAME:
	  break;

	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_0;		/* :::::::::::::::::::: */
	}
      ffesta_confirmed ();
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_R11071_;

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstUSE)
	goto bad_0;		/* :::::::::::::::::::: */
      p = ffelex_token_text (ffesta_tokens[0]) + (i = FFESTR_firstlUSE);
      if (!ffesrc_is_name_init (*p))
	goto bad_i;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeCOMMA:
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  break;
	}
      ffesta_confirmed ();
      ffesta_tokens[1]
	= ffelex_token_name_from_names (ffesta_tokens[0], i, 0);
      return (ffelexHandler) ffestb_R11071_ (t);

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "USE", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "USE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */

bad_i:				/* :::::::::::::::::::: */
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, "USE", ffesta_tokens[0], i, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R11071_ -- "USE" NAME

   return ffestb_R11071_;  // to lexer

   Make sure the statement has a valid form for the USE statement.  If it
   does, implement the statement.  */

static ffelexHandler
ffestb_R11071_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (!ffesta_is_inhibited ())
	{
	  ffestc_R1107_start (ffesta_tokens[1], FALSE);
	  ffestc_R1107_finish ();
	}
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffesta_zero (t);

    case FFELEX_typeCOMMA:
      return (ffelexHandler) ffestb_R11072_;

    default:
      break;
    }

  ffelex_token_kill (ffesta_tokens[1]);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "USE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R11072_ -- "USE" NAME COMMA

   return ffestb_R11072_;  // to lexer

   Make sure the statement has a valid form for the USE statement.  If it
   does, implement the statement.  */

static ffelexHandler
ffestb_R11072_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_tokens[2] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_R11073_;

    default:
      break;
    }

  ffelex_token_kill (ffesta_tokens[1]);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "USE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R11073_ -- "USE" NAME COMMA NAME

   return ffestb_R11073_;  // to lexer

   Make sure the statement has a valid form for the USE statement.  If it
   does, implement the statement.  */

static ffelexHandler
ffestb_R11073_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOLON:
      if (ffestr_other (ffesta_tokens[2]) != FFESTR_otherONLY)
	break;
      if (!ffesta_is_inhibited ())
	ffestc_R1107_start (ffesta_tokens[1], TRUE);
      ffelex_token_kill (ffesta_tokens[1]);
      ffelex_token_kill (ffesta_tokens[2]);
      return (ffelexHandler) ffestb_R11074_;

    case FFELEX_typePOINTS:
      if (!ffesta_is_inhibited ())
	ffestc_R1107_start (ffesta_tokens[1], FALSE);
      ffelex_token_kill (ffesta_tokens[1]);
      ffesta_tokens[1] = ffesta_tokens[2];
      return (ffelexHandler) ffestb_R110711_;

    default:
      break;
    }

  ffelex_token_kill (ffesta_tokens[1]);
  ffelex_token_kill (ffesta_tokens[2]);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "USE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R11074_ -- "USE" NAME COMMA "ONLY" COLON

   return ffestb_R11074_;  // to lexer

   Make sure the statement has a valid form for the USE statement.  If it
   does, implement the statement.  */

static ffelexHandler
ffestb_R11074_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_R11075_;

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (!ffesta_is_inhibited ())
	ffestc_R1107_finish ();
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "USE", t);
  ffestc_R1107_finish ();
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R11075_ -- "USE" NAME COMMA "ONLY" COLON NAME

   return ffestb_R11075_;  // to lexer

   Make sure the statement has a valid form for the USE statement.  If it
   does, implement the statement.  */

static ffelexHandler
ffestb_R11075_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (!ffesta_is_inhibited ())
	{
	  ffestc_R1107_item (NULL, ffesta_tokens[1]);
	  ffestc_R1107_finish ();
	}
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffesta_zero (t);

    case FFELEX_typeCOMMA:
      if (!ffesta_is_inhibited ())
	ffestc_R1107_item (NULL, ffesta_tokens[1]);
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffestb_R11078_;

    case FFELEX_typePOINTS:
      return (ffelexHandler) ffestb_R11076_;

    default:
      break;
    }

  ffelex_token_kill (ffesta_tokens[1]);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "USE", t);
  ffestc_R1107_finish ();
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R11076_ -- "USE" NAME COMMA "ONLY" COLON NAME POINTS

   return ffestb_R11076_;  // to lexer

   Make sure the statement has a valid form for the USE statement.  If it
   does, implement the statement.  */

static ffelexHandler
ffestb_R11076_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      if (!ffesta_is_inhibited ())
	ffestc_R1107_item (ffesta_tokens[1], t);
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffestb_R11077_;

    default:
      break;
    }

  ffelex_token_kill (ffesta_tokens[1]);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "USE", t);
  ffestc_R1107_finish ();
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R11077_ -- "USE" NAME COMMA "ONLY" COLON NAME POINTS NAME

   return ffestb_R11077_;  // to lexer

   Make sure the statement has a valid form for the USE statement.  If it
   does, implement the statement.  */

static ffelexHandler
ffestb_R11077_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (!ffesta_is_inhibited ())
	ffestc_R1107_finish ();
      return (ffelexHandler) ffesta_zero (t);

    case FFELEX_typeCOMMA:
      return (ffelexHandler) ffestb_R11078_;

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "USE", t);
  ffestc_R1107_finish ();
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R11078_ -- "USE" NAME COMMA "ONLY" COLON NAME POINTS NAME COMMA

   return ffestb_R11078_;  // to lexer

   Make sure the statement has a valid form for the USE statement.  If it
   does, implement the statement.  */

static ffelexHandler
ffestb_R11078_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_R11075_;

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "USE", t);
  ffestc_R1107_finish ();
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R11079_ -- "USE" NAME COMMA

   return ffestb_R11079_;  // to lexer

   Make sure the statement has a valid form for the USE statement.  If it
   does, implement the statement.  */

static ffelexHandler
ffestb_R11079_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_R110710_;

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "USE", t);
  ffestc_R1107_finish ();
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R110710_ -- "USE" NAME COMMA NAME

   return ffestb_R110710_;  // to lexer

   Make sure the statement has a valid form for the USE statement.  If it
   does, implement the statement.  */

static ffelexHandler
ffestb_R110710_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typePOINTS:
      return (ffelexHandler) ffestb_R110711_;

    default:
      break;
    }

  ffelex_token_kill (ffesta_tokens[1]);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "USE", t);
  ffestc_R1107_finish ();
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R110711_ -- "USE" NAME COMMA NAME POINTS

   return ffestb_R110711_;  // to lexer

   Make sure the statement has a valid form for the USE statement.  If it
   does, implement the statement.  */

static ffelexHandler
ffestb_R110711_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      if (!ffesta_is_inhibited ())
	ffestc_R1107_item (ffesta_tokens[1], t);
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffestb_R110712_;

    default:
      break;
    }

  ffelex_token_kill (ffesta_tokens[1]);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "USE", t);
  ffestc_R1107_finish ();
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R110712_ -- "USE" NAME COMMA NAME POINTS NAME

   return ffestb_R110712_;  // to lexer

   Make sure the statement has a valid form for the USE statement.  If it
   does, implement the statement.  */

static ffelexHandler
ffestb_R110712_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (!ffesta_is_inhibited ())
	ffestc_R1107_finish ();
      return (ffelexHandler) ffesta_zero (t);

    case FFELEX_typeCOMMA:
      return (ffelexHandler) ffestb_R11079_;

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "USE", t);
  ffestc_R1107_finish ();
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

#endif
/* ffestb_R1202 -- Parse the INTERFACE statement

   return ffestb_R1202;	 // to lexer

   Make sure the statement has a valid form for the INTERFACE statement.
   If it does, implement the statement.

   15-May-90  JCB  1.1
      Allow INTERFACE by itself; missed this
      valid form when originally doing syntactic analysis code.	 */

#if FFESTR_F90
ffelexHandler
ffestb_R1202 (ffelexToken t)
{
  ffeTokenLength i;
  const char *p;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstINTERFACE)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeNAME:
	  break;

	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  ffesta_confirmed ();
	  if (!ffesta_is_inhibited ())
	    ffestc_R1202 (FFESTP_definedoperatorNone, NULL);
	  return (ffelexHandler) ffesta_zero (t);

	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */
	}

      ffesta_confirmed ();
      switch (ffesta_second_kw)
	{
	case FFESTR_secondOPERATOR:
	  ffestb_local_.interface.operator = FFESTP_definedoperatorOPERATOR;
	  break;

	case FFESTR_secondASSIGNMENT:
	  ffestb_local_.interface.operator = FFESTP_definedoperatorASSIGNMENT;
	  break;

	default:
	  ffestb_local_.interface.operator = FFESTP_definedoperatorNone;
	  break;
	}
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_R12021_;

    case FFELEX_typeNAMES:
      p = ffelex_token_text (ffesta_tokens[0]) + (i = FFESTR_firstlINTERFACE);
      switch (ffesta_first_kw)
	{
	case FFESTR_firstINTERFACEOPERATOR:
	  if (*(ffelex_token_text (ffesta_tokens[0])
		+ FFESTR_firstlINTERFACEOPERATOR) == '\0')
	    ffestb_local_.interface.operator
	      = FFESTP_definedoperatorOPERATOR;
	  break;

	case FFESTR_firstINTERFACEASSGNMNT:
	  if (*(ffelex_token_text (ffesta_tokens[0])
		+ FFESTR_firstlINTERFACEASSGNMNT) == '\0')
	    ffestb_local_.interface.operator
	      = FFESTP_definedoperatorASSIGNMENT;
	  break;

	case FFESTR_firstINTERFACE:
	  ffestb_local_.interface.operator = FFESTP_definedoperatorNone;
	  break;

	default:
	  goto bad_0;		/* :::::::::::::::::::: */
	}
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeOPEN_PAREN:
	case FFELEX_typeOPEN_ARRAY:	/* Sigh. */
	  break;

	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  ffesta_confirmed ();
	  if (*p == '\0')
	    {
	      if (!ffesta_is_inhibited ())
		ffestc_R1202 (FFESTP_definedoperatorNone, NULL);
	      return (ffelexHandler) ffesta_zero (t);
	    }
	  break;
	}
      if (!ffesrc_is_name_init (*p))
	goto bad_i;		/* :::::::::::::::::::: */
      ffesta_tokens[1] = ffelex_token_name_from_names (ffesta_tokens[0], i, 0);
      return (ffelexHandler) ffestb_R12021_ (t);

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "INTERFACE", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "INTERFACE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */

bad_i:				/* :::::::::::::::::::: */
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, "INTERFACE", ffesta_tokens[0], i, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R12021_ -- "INTERFACE" NAME

   return ffestb_R12021_;  // to lexer

   Make sure the statement has a valid form for the INTERFACE statement.  If
   it does, implement the statement.  */

static ffelexHandler
ffestb_R12021_ (ffelexToken t)
{
  ffestb_local_.interface.slash = TRUE;	/* Slash follows open paren. */

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (!ffesta_is_inhibited ())
	ffestc_R1202 (FFESTP_definedoperatorNone, ffesta_tokens[1]);
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffesta_zero (t);

    case FFELEX_typeOPEN_PAREN:
      ffestb_local_.interface.slash = FALSE;	/* Slash doesn't follow. */
      /* Fall through. */
    case FFELEX_typeOPEN_ARRAY:
      switch (ffestb_local_.interface.operator)
	{
	case FFESTP_definedoperatorNone:
	  break;

	case FFESTP_definedoperatorOPERATOR:
	  ffestb_local_.interface.assignment = FALSE;
	  return (ffelexHandler) ffestb_R12022_;

	case FFESTP_definedoperatorASSIGNMENT:
	  ffestb_local_.interface.assignment = TRUE;
	  return (ffelexHandler) ffestb_R12022_;

	default:
	  assert (FALSE);
	}
      break;

    case FFELEX_typeCOMMA:
    case FFELEX_typeCOLONCOLON:
      ffesta_confirmed ();	/* Error, but clearly intended. */
      break;

    default:
      break;
    }

  ffelex_token_kill (ffesta_tokens[1]);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "INTERFACE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R12022_ -- "INTERFACE" "OPERATOR/ASSIGNMENT" OPEN_PAREN

   return ffestb_R12022_;  // to lexer

   Make sure the statement has a valid form for the INTERFACE statement.  If
   it does, implement the statement.  */

static ffelexHandler
ffestb_R12022_ (ffelexToken t)
{
  ffesta_tokens[2] = ffelex_token_use (t);

  switch (ffelex_token_type (t))
    {
    case FFELEX_typePERIOD:
      if (ffestb_local_.interface.slash)
	break;
      return (ffelexHandler) ffestb_R12023_;

    case FFELEX_typePOWER:
      if (ffestb_local_.interface.slash)
	break;
      ffestb_local_.interface.operator = FFESTP_definedoperatorPOWER;
      return (ffelexHandler) ffestb_R12025_;

    case FFELEX_typeASTERISK:
      if (ffestb_local_.interface.slash)
	break;
      ffestb_local_.interface.operator = FFESTP_definedoperatorMULT;
      return (ffelexHandler) ffestb_R12025_;

    case FFELEX_typePLUS:
      if (ffestb_local_.interface.slash)
	break;
      ffestb_local_.interface.operator = FFESTP_definedoperatorADD;
      return (ffelexHandler) ffestb_R12025_;

    case FFELEX_typeCONCAT:
      if (ffestb_local_.interface.slash)
	break;
      ffestb_local_.interface.operator = FFESTP_definedoperatorCONCAT;
      return (ffelexHandler) ffestb_R12025_;

    case FFELEX_typeSLASH:
      if (ffestb_local_.interface.slash)
	{
	  ffestb_local_.interface.operator = FFESTP_definedoperatorCONCAT;
	  return (ffelexHandler) ffestb_R12025_;
	}
      ffestb_local_.interface.operator = FFESTP_definedoperatorDIVIDE;
      return (ffelexHandler) ffestb_R12025_;

    case FFELEX_typeMINUS:
      if (ffestb_local_.interface.slash)
	break;
      ffestb_local_.interface.operator = FFESTP_definedoperatorSUBTRACT;
      return (ffelexHandler) ffestb_R12025_;

    case FFELEX_typeREL_EQ:
      if (ffestb_local_.interface.slash)
	break;
      ffestb_local_.interface.operator = FFESTP_definedoperatorEQ;
      return (ffelexHandler) ffestb_R12025_;

    case FFELEX_typeREL_NE:
      if (ffestb_local_.interface.slash)
	break;
      ffestb_local_.interface.operator = FFESTP_definedoperatorNE;
      return (ffelexHandler) ffestb_R12025_;

    case FFELEX_typeOPEN_ANGLE:
      if (ffestb_local_.interface.slash)
	break;
      ffestb_local_.interface.operator = FFESTP_definedoperatorLT;
      return (ffelexHandler) ffestb_R12025_;

    case FFELEX_typeREL_LE:
      if (ffestb_local_.interface.slash)
	break;
      ffestb_local_.interface.operator = FFESTP_definedoperatorLE;
      return (ffelexHandler) ffestb_R12025_;

    case FFELEX_typeCLOSE_ANGLE:
      if (ffestb_local_.interface.slash)
	break;
      ffestb_local_.interface.operator = FFESTP_definedoperatorGT;
      return (ffelexHandler) ffestb_R12025_;

    case FFELEX_typeREL_GE:
      if (ffestb_local_.interface.slash)
	break;
      ffestb_local_.interface.operator = FFESTP_definedoperatorGE;
      return (ffelexHandler) ffestb_R12025_;

    case FFELEX_typeEQUALS:
      if (ffestb_local_.interface.slash)
	{
	  ffestb_local_.interface.operator = FFESTP_definedoperatorNE;
	  return (ffelexHandler) ffestb_R12025_;
	}
      ffestb_local_.interface.operator = FFESTP_definedoperatorASSIGNMENT;
      return (ffelexHandler) ffestb_R12025_;

    case FFELEX_typeCLOSE_ARRAY:
      if (!ffestb_local_.interface.slash)
	{
	  ffestb_local_.interface.operator = FFESTP_definedoperatorDIVIDE;
	  return (ffelexHandler) ffestb_R12026_;
	}
      ffestb_local_.interface.operator = FFESTP_definedoperatorCONCAT;
      return (ffelexHandler) ffestb_R12026_;

    case FFELEX_typeCLOSE_PAREN:
      if (!ffestb_local_.interface.slash)
	break;
      ffestb_local_.interface.operator = FFESTP_definedoperatorDIVIDE;
      return (ffelexHandler) ffestb_R12026_;

    default:
      break;
    }

  ffelex_token_kill (ffesta_tokens[1]);
  ffelex_token_kill (ffesta_tokens[2]);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "INTERFACE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R12023_ -- "INTERFACE" NAME OPEN_PAREN PERIOD

   return ffestb_R12023_;  // to lexer

   Make sure the statement has a valid form for the INTERFACE statement.  If
   it does, implement the statement.  */

static ffelexHandler
ffestb_R12023_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffelex_token_kill (ffesta_tokens[2]);
      ffesta_tokens[2] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_R12024_;

    default:
      break;
    }

  ffelex_token_kill (ffesta_tokens[1]);
  ffelex_token_kill (ffesta_tokens[2]);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "INTERFACE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R12024_ -- "INTERFACE" NAME OPEN_PAREN PERIOD NAME

   return ffestb_R12024_;  // to lexer

   Make sure the statement has a valid form for the INTERFACE statement.  If
   it does, implement the statement.  */

static ffelexHandler
ffestb_R12024_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typePERIOD:
      return (ffelexHandler) ffestb_R12025_;

    default:
      break;
    }

  ffelex_token_kill (ffesta_tokens[1]);
  ffelex_token_kill (ffesta_tokens[2]);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "INTERFACE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R12025_ -- "INTERFACE" NAME OPEN_PAREN operator

   return ffestb_R12025_;  // to lexer

   Make sure the statement has a valid form for the INTERFACE statement.  If
   it does, implement the statement.  */

static ffelexHandler
ffestb_R12025_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCLOSE_PAREN:
      return (ffelexHandler) ffestb_R12026_;

    default:
      break;
    }

  ffelex_token_kill (ffesta_tokens[1]);
  ffelex_token_kill (ffesta_tokens[2]);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "INTERFACE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R12026_ -- "INTERFACE" NAME OPEN_PAREN operator CLOSE_PAREN

   return ffestb_R12026_;  // to lexer

   Make sure the statement has a valid form for the INTERFACE statement.  If
   it does, implement the statement.  */

static ffelexHandler
ffestb_R12026_ (ffelexToken t)
{
  const char *p;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (ffestb_local_.interface.assignment
	  && (ffestb_local_.interface.operator
	      != FFESTP_definedoperatorASSIGNMENT))
	{
	  ffebad_start (FFEBAD_INTERFACE_ASSIGNMENT);
	  ffebad_here (0, ffelex_token_where_line (ffesta_tokens[1]),
		       ffelex_token_where_column (ffesta_tokens[1]));
	  ffebad_here (1, ffelex_token_where_line (ffesta_tokens[2]),
		       ffelex_token_where_column (ffesta_tokens[2]));
	  ffebad_finish ();
	}
      switch (ffelex_token_type (ffesta_tokens[2]))
	{
	case FFELEX_typeNAME:
	  switch (ffestr_other (ffesta_tokens[2]))
	    {
	    case FFESTR_otherNOT:
	      if (!ffesta_is_inhibited ())
		ffestc_R1202 (FFESTP_definedoperatorNOT, NULL);
	      break;

	    case FFESTR_otherAND:
	      if (!ffesta_is_inhibited ())
		ffestc_R1202 (FFESTP_definedoperatorAND, NULL);
	      break;

	    case FFESTR_otherOR:
	      if (!ffesta_is_inhibited ())
		ffestc_R1202 (FFESTP_definedoperatorOR, NULL);
	      break;

	    case FFESTR_otherEQV:
	      if (!ffesta_is_inhibited ())
		ffestc_R1202 (FFESTP_definedoperatorEQV, NULL);
	      break;

	    case FFESTR_otherNEQV:
	      if (!ffesta_is_inhibited ())
		ffestc_R1202 (FFESTP_definedoperatorNEQV, NULL);
	      break;

	    case FFESTR_otherEQ:
	      if (!ffesta_is_inhibited ())
		ffestc_R1202 (FFESTP_definedoperatorEQ, NULL);
	      break;

	    case FFESTR_otherNE:
	      if (!ffesta_is_inhibited ())
		ffestc_R1202 (FFESTP_definedoperatorNE, NULL);
	      break;

	    case FFESTR_otherLT:
	      if (!ffesta_is_inhibited ())
		ffestc_R1202 (FFESTP_definedoperatorLT, NULL);
	      break;

	    case FFESTR_otherLE:
	      if (!ffesta_is_inhibited ())
		ffestc_R1202 (FFESTP_definedoperatorLE, NULL);
	      break;

	    case FFESTR_otherGT:
	      if (!ffesta_is_inhibited ())
		ffestc_R1202 (FFESTP_definedoperatorGT, NULL);
	      break;

	    case FFESTR_otherGE:
	      if (!ffesta_is_inhibited ())
		ffestc_R1202 (FFESTP_definedoperatorGE, NULL);
	      break;

	    default:
	      for (p = ffelex_token_text (ffesta_tokens[2]); *p != '\0'; ++p)
		{
		  if (! ISALPHA (*p))
		    {
		      ffelex_token_kill (ffesta_tokens[1]);
		      ffelex_token_kill (ffesta_tokens[2]);
		      ffesta_ffebad_1t (FFEBAD_INTERFACE_NONLETTER,
					ffesta_tokens[2]);
		      return (ffelexHandler) ffelex_swallow_tokens (t,
					       (ffelexHandler) ffesta_zero);
		    }
		}
	      if (!ffesta_is_inhibited ())
		ffestc_R1202 (FFESTP_definedoperatorOPERATOR,
			      ffesta_tokens[2]);
	    }
	  break;

	case FFELEX_typeEQUALS:
	  if (!ffestb_local_.interface.assignment
	      && (ffestb_local_.interface.operator
		  == FFESTP_definedoperatorASSIGNMENT))
	    {
	      ffebad_start (FFEBAD_INTERFACE_OPERATOR);
	      ffebad_here (0, ffelex_token_where_line (ffesta_tokens[1]),
			   ffelex_token_where_column (ffesta_tokens[1]));
	      ffebad_here (1, ffelex_token_where_line (ffesta_tokens[2]),
			   ffelex_token_where_column (ffesta_tokens[2]));
	      ffebad_finish ();
	    }
	  if (!ffesta_is_inhibited ())
	    ffestc_R1202 (ffestb_local_.interface.operator, NULL);
	  break;

	default:
	  if (!ffesta_is_inhibited ())
	    ffestc_R1202 (ffestb_local_.interface.operator, NULL);
	}
      ffelex_token_kill (ffesta_tokens[1]);
      ffelex_token_kill (ffesta_tokens[2]);
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  ffelex_token_kill (ffesta_tokens[1]);
  ffelex_token_kill (ffesta_tokens[2]);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "INTERFACE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

#endif
/* ffestb_S3P4 -- Parse the INCLUDE line

   return ffestb_S3P4;	// to lexer

   Make sure the statement has a valid form for the INCLUDE line.  If it
   does, implement the statement.  */

ffelexHandler
ffestb_S3P4 (ffelexToken t)
{
  ffeTokenLength i;
  const char *p;
  ffelexHandler next;
  ffelexToken nt;
  ffelexToken ut;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstINCLUDE)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeNUMBER:
	case FFELEX_typeAPOSTROPHE:
	case FFELEX_typeQUOTE:
	  break;

	default:
	  goto bad_1;		/* :::::::::::::::::::: */
	}
      ffesta_confirmed ();
      return (ffelexHandler) (*((ffelexHandler)
		    ffeexpr_rhs (ffesta_output_pool, FFEEXPR_contextINCLUDE,
				 (ffeexprCallback) ffestb_S3P41_)))
	(t);

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstINCLUDE)
	goto bad_0;		/* :::::::::::::::::::: */
      p = ffelex_token_text (ffesta_tokens[0]) + (i = FFESTR_firstlINCLUDE);
      switch (ffelex_token_type (t))
	{
	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeAPOSTROPHE:
	case FFELEX_typeQUOTE:
	  break;
	}
      ffesta_confirmed ();
      if (*p == '\0')
	return (ffelexHandler) (*((ffelexHandler)
		    ffeexpr_rhs (ffesta_output_pool, FFEEXPR_contextINCLUDE,
				 (ffeexprCallback) ffestb_S3P41_)))
	  (t);
      if (! ISDIGIT (*p))
	goto bad_i;		/* :::::::::::::::::::: */
      nt = ffelex_token_number_from_names (ffesta_tokens[0], i);
      p += ffelex_token_length (nt);
      i += ffelex_token_length (nt);
      if ((*p != '_') || (++i, *++p != '\0'))
	{
	  ffelex_token_kill (nt);
	  goto bad_i;		/* :::::::::::::::::::: */
	}
      ut = ffelex_token_uscore_from_names (ffesta_tokens[0], i - 1);
      next = (ffelexHandler) (*((ffelexHandler) ffeexpr_rhs
				(ffesta_output_pool, FFEEXPR_contextINCLUDE,
				 (ffeexprCallback) ffestb_S3P41_)))
	(nt);
      ffelex_token_kill (nt);
      next = (ffelexHandler) (*next) (ut);
      ffelex_token_kill (ut);
      return (ffelexHandler) (*next) (t);

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "INCLUDE", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "INCLUDE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */

bad_i:				/* :::::::::::::::::::: */
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, "INCLUDE", ffesta_tokens[0], i, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_S3P41_ -- "INCLUDE" [NUMBER "_"] expr

   (ffestb_S3P41_)  // to expression handler

   Make sure the next token is an EOS, but not a SEMICOLON.  */

static ffelexHandler
ffestb_S3P41_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (expr == NULL)
	break;
      if (!ffesta_is_inhibited ())
	{
	  if (ffe_is_pedantic ()
	      && ((ffelex_token_type (t) == FFELEX_typeSEMICOLON)
		  || ffesta_line_has_semicolons))
	    {
	      ffebad_start_msg ("INCLUDE at %0 not the only statement on the source line", FFEBAD_severityWARNING);
	      ffebad_here (0, ffelex_token_where_line (ffesta_tokens[0]),
			   ffelex_token_where_column (ffesta_tokens[0]));
	      ffebad_finish ();
	    }
	  ffestc_S3P4 (expr, ft);
	}
      return (ffelexHandler) ffesta_zero (t);

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "INCLUDE", t);
      break;
    }

  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V012 -- Parse the MAP statement

   return ffestb_V012;	// to lexer

   Make sure the statement has a valid form for the MAP statement.  If
   it does, implement the statement.  */

#if FFESTR_VXT
ffelexHandler
ffestb_V012 (ffelexToken t)
{
  const char *p;
  ffeTokenLength i;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstMAP)
	goto bad_0;		/* :::::::::::::::::::: */
      break;

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstMAP)
	goto bad_0;		/* :::::::::::::::::::: */
      if (ffelex_token_length (ffesta_tokens[0]) != FFESTR_firstlMAP)
	{
	  p = ffelex_token_text (ffesta_tokens[0]) + (i = FFESTR_firstlMAP);
	  goto bad_i;		/* :::::::::::::::::::: */
	}
      break;

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	ffestc_V012 ();
      return (ffelexHandler) ffesta_zero (t);

    case FFELEX_typeCOMMA:
    case FFELEX_typeCOLONCOLON:
      ffesta_confirmed ();	/* Error, but clearly intended. */
      goto bad_1;		/* :::::::::::::::::::: */

    default:
      goto bad_1;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "MAP", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid first token. */

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "MAP", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */

bad_i:				/* :::::::::::::::::::: */
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, "MAP", ffesta_tokens[0], i, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

#endif
/* ffestb_V014 -- Parse the VOLATILE statement

   return ffestb_V014;	// to lexer

   Make sure the statement has a valid form for the VOLATILE statement.	 If it
   does, implement the statement.  */

ffelexHandler
ffestb_V014 (ffelexToken t)
{
  ffeTokenLength i;
  unsigned const char *p;
  ffelexToken nt;
  ffelexHandler next;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstVOLATILE)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	case FFELEX_typeCOMMA:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeNAME:
	case FFELEX_typeSLASH:
	  ffesta_confirmed ();
	  if (!ffesta_is_inhibited ())
	    ffestc_V014_start ();
	  return (ffelexHandler) ffestb_V0141_ (t);

	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();
	  if (!ffesta_is_inhibited ())
	    ffestc_V014_start ();
	  return (ffelexHandler) ffestb_V0141_;
	}

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstVOLATILE)
	goto bad_0;		/* :::::::::::::::::::: */
      p = ffelex_token_text (ffesta_tokens[0]) + (i = FFESTR_firstlVOLATILE);
      switch (ffelex_token_type (t))
	{
	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeCOMMA:
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  ffesta_confirmed ();
	  break;

	case FFELEX_typeSLASH:
	  ffesta_confirmed ();
	  if (*p != '\0')
	    goto bad_i;		/* :::::::::::::::::::: */
	  if (!ffesta_is_inhibited ())
	    ffestc_V014_start ();
	  return (ffelexHandler) ffestb_V0141_ (t);

	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();
	  if (*p != '\0')
	    goto bad_i;		/* :::::::::::::::::::: */
	  if (!ffesta_is_inhibited ())
	    ffestc_V014_start ();
	  return (ffelexHandler) ffestb_V0141_;
	}

      /* Here, we have at least one char after "VOLATILE" and t is COMMA or
	 EOS/SEMICOLON. */

      if (!ffesrc_is_name_init (*p))
	goto bad_i;		/* :::::::::::::::::::: */
      nt = ffelex_token_name_from_names (ffesta_tokens[0], i, 0);
      if (!ffesta_is_inhibited ())
	ffestc_V014_start ();
      next = (ffelexHandler) ffestb_V0141_ (nt);
      ffelex_token_kill (nt);
      return (ffelexHandler) (*next) (t);

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "VOLATILE", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "VOLATILE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */

bad_i:				/* :::::::::::::::::::: */
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, "VOLATILE", ffesta_tokens[0], i, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0141_ -- "VOLATILE" [COLONCOLON]

   return ffestb_V0141_;  // to lexer

   Handle NAME or SLASH.  */

static ffelexHandler
ffestb_V0141_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffestb_local_.V014.is_cblock = FALSE;
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_V0144_;

    case FFELEX_typeSLASH:
      ffestb_local_.V014.is_cblock = TRUE;
      return (ffelexHandler) ffestb_V0142_;

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "VOLATILE", t);
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_V014_finish ();
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0142_ -- "VOLATILE" [COLONCOLON] SLASH

   return ffestb_V0142_;  // to lexer

   Handle NAME.	 */

static ffelexHandler
ffestb_V0142_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_V0143_;

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "VOLATILE", t);
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_V014_finish ();
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0143_ -- "VOLATILE" [COLONCOLON] SLASH NAME

   return ffestb_V0143_;  // to lexer

   Handle SLASH.  */

static ffelexHandler
ffestb_V0143_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeSLASH:
      return (ffelexHandler) ffestb_V0144_;

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "VOLATILE", t);
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_V014_finish ();
  ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0144_ -- "VOLATILE" [COLONCOLON] R523

   return ffestb_V0144_;  // to lexer

   Handle COMMA or EOS/SEMICOLON.  */

static ffelexHandler
ffestb_V0144_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      if (!ffesta_is_inhibited ())
	{
	  if (ffestb_local_.V014.is_cblock)
	    ffestc_V014_item_cblock (ffesta_tokens[1]);
	  else
	    ffestc_V014_item_object (ffesta_tokens[1]);
	}
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffestb_V0141_;

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (!ffesta_is_inhibited ())
	{
	  if (ffestb_local_.V014.is_cblock)
	    ffestc_V014_item_cblock (ffesta_tokens[1]);
	  else
	    ffestc_V014_item_object (ffesta_tokens[1]);
	  ffestc_V014_finish ();
	}
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffesta_zero (t);

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "VOLATILE", t);
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_V014_finish ();
  ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V025 -- Parse the DEFINEFILE statement

   return ffestb_V025;	// to lexer

   Make sure the statement has a valid form for the DEFINEFILE statement.
   If it does, implement the statement.	 */

#if FFESTR_VXT
ffelexHandler
ffestb_V025 (ffelexToken t)
{
  ffeTokenLength i;
  const char *p;
  ffelexToken nt;
  ffelexHandler next;

  ffestb_local_.V025.started = FALSE;
  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      switch (ffesta_first_kw)
	{
	case FFESTR_firstDEFINE:
	  if ((ffelex_token_type (t) != FFELEX_typeNAME)
	      || (ffesta_second_kw != FFESTR_secondFILE))
	    goto bad_1;		/* :::::::::::::::::::: */
	  ffesta_confirmed ();
	  return (ffelexHandler) ffestb_V0251_;

	case FFESTR_firstDEFINEFILE:
	  return (ffelexHandler) ffestb_V0251_ (t);

	default:
	  goto bad_0;		/* :::::::::::::::::::: */
	}

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstDEFINEFILE)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeCOMMA:
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeOPEN_PAREN:
	  break;
	}
      p = ffelex_token_text (ffesta_tokens[0]) + (i = FFESTR_firstlDEFINEFILE);
      if (ISDIGIT (*p))
	nt = ffelex_token_number_from_names (ffesta_tokens[0], i);
      else if (ffesrc_is_name_init (*p))
	nt = ffelex_token_name_from_names (ffesta_tokens[0], i, 0);
      else
	goto bad_i;		/* :::::::::::::::::::: */
      next = (ffelexHandler) ffestb_V0251_ (nt);
      ffelex_token_kill (nt);
      return (ffelexHandler) (*next) (t);

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "DEFINE FILE", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "DEFINE FILE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */

bad_i:				/* :::::::::::::::::::: */
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, "DEFINE FILE", ffesta_tokens[0], i, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0251_ -- "DEFINEFILE" or "DEFINE" "FILE"

   return ffestb_V0251_;  // to lexer

   Make sure the statement has a valid form for the DEFINEFILE statement.  If it
   does, implement the statement.  */

static ffelexHandler
ffestb_V0251_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
    case FFELEX_typeNUMBER:
      if (ffelex_token_type (ffesta_tokens[0]) == FFELEX_typeNAME)
	ffesta_confirmed ();
      return (ffelexHandler) (*((ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
	      FFEEXPR_contextFILEUNIT_DF, (ffeexprCallback) ffestb_V0252_)))
	(t);

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
    case FFELEX_typeCOMMA:
    case FFELEX_typeCOLONCOLON:
      ffesta_confirmed ();	/* Error, but clearly intended. */
      break;

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "DEFINE FILE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0252_ -- "DEFINEFILE" expr

   (ffestb_V0252_)  // to expression handler

   Make sure the statement has a valid form for the DEFINEFILE statement.  If
   it does, implement the statement.  */

static ffelexHandler
ffestb_V0252_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeOPEN_PAREN:
      ffestb_local_.V025.u = expr;
      ffesta_tokens[1] = ffelex_token_use (ft);
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		   FFEEXPR_contextFILENUM, (ffeexprCallback) ffestb_V0253_);

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "DEFINE FILE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0253_ -- "DEFINEFILE" expr OPEN_PAREN expr

   (ffestb_V0253_)  // to expression handler

   Make sure the statement has a valid form for the DEFINEFILE statement.  If
   it does, implement the statement.  */

static ffelexHandler
ffestb_V0253_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      ffestb_local_.V025.m = expr;
      ffesta_tokens[2] = ffelex_token_use (ft);
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		   FFEEXPR_contextFILENUM, (ffeexprCallback) ffestb_V0254_);

    default:
      break;
    }

  ffelex_token_kill (ffesta_tokens[1]);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "DEFINE FILE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0254_ -- "DEFINEFILE" expr OPEN_PAREN expr COMMA expr

   (ffestb_V0254_)  // to expression handler

   Make sure the statement has a valid form for the DEFINEFILE statement.  If
   it does, implement the statement.  */

static ffelexHandler
ffestb_V0254_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      ffestb_local_.V025.n = expr;
      ffesta_tokens[3] = ffelex_token_use (ft);
      return (ffelexHandler) ffestb_V0255_;

    default:
      break;
    }

  ffelex_token_kill (ffesta_tokens[1]);
  ffelex_token_kill (ffesta_tokens[2]);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "DEFINE FILE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0255_ -- "DEFINEFILE" expr OPEN_PAREN expr COMMA expr COMMA

   return ffestb_V0255_;  // to lexer

   Make sure the statement has a valid form for the DEFINEFILE statement.  If
   it does, implement the statement.  */

static ffelexHandler
ffestb_V0255_ (ffelexToken t)
{
  const char *p;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      p = ffelex_token_text (t);
      if (!ffesrc_char_match_init (*p, 'U', 'u') || (*++p != '\0'))
	break;
      return (ffelexHandler) ffestb_V0256_;

    default:
      break;
    }

  ffelex_token_kill (ffesta_tokens[1]);
  ffelex_token_kill (ffesta_tokens[2]);
  ffelex_token_kill (ffesta_tokens[3]);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "DEFINE FILE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0256_ -- "DEFINEFILE" expr OPEN_PAREN expr COMMA expr COMMA "U"

   return ffestb_V0256_;  // to lexer

   Make sure the statement has a valid form for the DEFINEFILE statement.  If
   it does, implement the statement.  */

static ffelexHandler
ffestb_V0256_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      return (ffelexHandler) ffeexpr_lhs (ffesta_output_pool,
					  FFEEXPR_contextFILEASSOC,
					  (ffeexprCallback) ffestb_V0257_);

    default:
      break;
    }

  ffelex_token_kill (ffesta_tokens[1]);
  ffelex_token_kill (ffesta_tokens[2]);
  ffelex_token_kill (ffesta_tokens[3]);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "DEFINE FILE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0257_ -- "DEFINEFILE" expr OPEN_PAREN expr COMMA expr COMMA "U"
		    COMMA expr

   (ffestb_V0257_)  // to expression handler

   Make sure the statement has a valid form for the DEFINEFILE statement.  If
   it does, implement the statement.  */

static ffelexHandler
ffestb_V0257_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCLOSE_PAREN:
      ffestb_local_.V025.asv = expr;
      ffesta_tokens[4] = ffelex_token_use (ft);
      return (ffelexHandler) ffestb_V0258_;

    default:
      break;
    }

  ffelex_token_kill (ffesta_tokens[1]);
  ffelex_token_kill (ffesta_tokens[2]);
  ffelex_token_kill (ffesta_tokens[3]);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "DEFINE FILE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0258_ -- "DEFINEFILE" expr OPEN_PAREN expr COMMA expr COMMA "U"
		    COMMA expr CLOSE_PAREN

   return ffestb_V0258_;  // to lexer

   Make sure the statement has a valid form for the DEFINEFILE statement.  If
   it does, implement the statement.  */

static ffelexHandler
ffestb_V0258_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (!ffestb_local_.V025.started)
	{
	  ffesta_confirmed ();
	  if (!ffesta_is_inhibited ())
	    ffestc_V025_start ();
	  ffestb_local_.V025.started = TRUE;
	}
      if (!ffesta_is_inhibited ())
	ffestc_V025_item (ffestb_local_.V025.u, ffesta_tokens[1],
			  ffestb_local_.V025.m, ffesta_tokens[2],
			  ffestb_local_.V025.n, ffesta_tokens[3],
			  ffestb_local_.V025.asv, ffesta_tokens[4]);
      ffelex_token_kill (ffesta_tokens[1]);
      ffelex_token_kill (ffesta_tokens[2]);
      ffelex_token_kill (ffesta_tokens[3]);
      ffelex_token_kill (ffesta_tokens[4]);
      if (ffelex_token_type (t) == FFELEX_typeCOMMA)
	return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
	       FFEEXPR_contextFILEUNIT_DF, (ffeexprCallback) ffestb_V0252_);
      if (!ffesta_is_inhibited ())
	ffestc_V025_finish ();
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  ffelex_token_kill (ffesta_tokens[1]);
  ffelex_token_kill (ffesta_tokens[2]);
  ffelex_token_kill (ffesta_tokens[3]);
  ffelex_token_kill (ffesta_tokens[4]);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "DEFINE FILE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

#endif
/* ffestb_subr_kill_easy_ -- Kill I/O statement data structure

   ffestb_subr_kill_easy_();

   Kills all tokens in the I/O data structure.	Assumes that they are
   overlaid with each other (union) in ffest_private.h and the typing
   and structure references assume (though not necessarily dangerous if
   FALSE) that INQUIRE has the most file elements.  */

#if FFESTB_KILL_EASY_
static void
ffestb_subr_kill_easy_ (ffestpInquireIx max)
{
  ffestpInquireIx ix;

  for (ix = 0; ix < max; ++ix)
    {
      if (ffestp_file.inquire.inquire_spec[ix].kw_or_val_present)
	{
	  if (ffestp_file.inquire.inquire_spec[ix].kw_present)
	    ffelex_token_kill (ffestp_file.inquire.inquire_spec[ix].kw);
	  if (ffestp_file.inquire.inquire_spec[ix].value_present)
	    ffelex_token_kill (ffestp_file.inquire.inquire_spec[ix].value);
	}
    }
}

#endif
/* ffestb_subr_kill_accept_ -- Kill ACCEPT statement data structure

   ffestb_subr_kill_accept_();

   Kills all tokens in the ACCEPT data structure.  */

#if !FFESTB_KILL_EASY_
static void
ffestb_subr_kill_accept_ ()
{
  ffestpAcceptIx ix;

  for (ix = 0; ix < FFESTP_acceptix; ++ix)
    {
      if (ffestp_file.accept.accept_spec[ix].kw_or_val_present)
	{
	  if (ffestp_file.accept.accept_spec[ix].kw_present)
	    ffelex_token_kill (ffestp_file.accept.accept_spec[ix].kw);
	  if (ffestp_file.accept.accept_spec[ix].value_present)
	    ffelex_token_kill (ffestp_file.accept.accept_spec[ix].value);
	}
    }
}

#endif
/* ffestb_subr_kill_beru_ -- Kill BACKSPACE/ENDFILE/REWIND/UNLOCK statement
			    data structure

   ffestb_subr_kill_beru_();

   Kills all tokens in the BACKSPACE/ENDFILE/REWIND/UNLOCK data structure.  */

#if !FFESTB_KILL_EASY_
static void
ffestb_subr_kill_beru_ ()
{
  ffestpBeruIx ix;

  for (ix = 0; ix < FFESTP_beruix; ++ix)
    {
      if (ffestp_file.beru.beru_spec[ix].kw_or_val_present)
	{
	  if (ffestp_file.beru.beru_spec[ix].kw_present)
	    ffelex_token_kill (ffestp_file.beru.beru_spec[ix].kw);
	  if (ffestp_file.beru.beru_spec[ix].value_present)
	    ffelex_token_kill (ffestp_file.beru.beru_spec[ix].value);
	}
    }
}

#endif
/* ffestb_subr_kill_close_ -- Kill CLOSE statement data structure

   ffestb_subr_kill_close_();

   Kills all tokens in the CLOSE data structure.  */

#if !FFESTB_KILL_EASY_
static void
ffestb_subr_kill_close_ ()
{
  ffestpCloseIx ix;

  for (ix = 0; ix < FFESTP_closeix; ++ix)
    {
      if (ffestp_file.close.close_spec[ix].kw_or_val_present)
	{
	  if (ffestp_file.close.close_spec[ix].kw_present)
	    ffelex_token_kill (ffestp_file.close.close_spec[ix].kw);
	  if (ffestp_file.close.close_spec[ix].value_present)
	    ffelex_token_kill (ffestp_file.close.close_spec[ix].value);
	}
    }
}

#endif
/* ffestb_subr_kill_delete_ -- Kill DELETE statement data structure

   ffestb_subr_kill_delete_();

   Kills all tokens in the DELETE data structure.  */

#if !FFESTB_KILL_EASY_
static void
ffestb_subr_kill_delete_ ()
{
  ffestpDeleteIx ix;

  for (ix = 0; ix < FFESTP_deleteix; ++ix)
    {
      if (ffestp_file.delete.delete_spec[ix].kw_or_val_present)
	{
	  if (ffestp_file.delete.delete_spec[ix].kw_present)
	    ffelex_token_kill (ffestp_file.delete.delete_spec[ix].kw);
	  if (ffestp_file.delete.delete_spec[ix].value_present)
	    ffelex_token_kill (ffestp_file.delete.delete_spec[ix].value);
	}
    }
}

#endif
/* ffestb_subr_kill_inquire_ -- Kill INQUIRE statement data structure

   ffestb_subr_kill_inquire_();

   Kills all tokens in the INQUIRE data structure.  */

#if !FFESTB_KILL_EASY_
static void
ffestb_subr_kill_inquire_ ()
{
  ffestpInquireIx ix;

  for (ix = 0; ix < FFESTP_inquireix; ++ix)
    {
      if (ffestp_file.inquire.inquire_spec[ix].kw_or_val_present)
	{
	  if (ffestp_file.inquire.inquire_spec[ix].kw_present)
	    ffelex_token_kill (ffestp_file.inquire.inquire_spec[ix].kw);
	  if (ffestp_file.inquire.inquire_spec[ix].value_present)
	    ffelex_token_kill (ffestp_file.inquire.inquire_spec[ix].value);
	}
    }
}

#endif
/* ffestb_subr_kill_open_ -- Kill OPEN statement data structure

   ffestb_subr_kill_open_();

   Kills all tokens in the OPEN data structure.	 */

#if !FFESTB_KILL_EASY_
static void
ffestb_subr_kill_open_ ()
{
  ffestpOpenIx ix;

  for (ix = 0; ix < FFESTP_openix; ++ix)
    {
      if (ffestp_file.open.open_spec[ix].kw_or_val_present)
	{
	  if (ffestp_file.open.open_spec[ix].kw_present)
	    ffelex_token_kill (ffestp_file.open.open_spec[ix].kw);
	  if (ffestp_file.open.open_spec[ix].value_present)
	    ffelex_token_kill (ffestp_file.open.open_spec[ix].value);
	}
    }
}

#endif
/* ffestb_subr_kill_print_ -- Kill PRINT statement data structure

   ffestb_subr_kill_print_();

   Kills all tokens in the PRINT data structure.  */

#if !FFESTB_KILL_EASY_
static void
ffestb_subr_kill_print_ ()
{
  ffestpPrintIx ix;

  for (ix = 0; ix < FFESTP_printix; ++ix)
    {
      if (ffestp_file.print.print_spec[ix].kw_or_val_present)
	{
	  if (ffestp_file.print.print_spec[ix].kw_present)
	    ffelex_token_kill (ffestp_file.print.print_spec[ix].kw);
	  if (ffestp_file.print.print_spec[ix].value_present)
	    ffelex_token_kill (ffestp_file.print.print_spec[ix].value);
	}
    }
}

#endif
/* ffestb_subr_kill_read_ -- Kill READ statement data structure

   ffestb_subr_kill_read_();

   Kills all tokens in the READ data structure.	 */

#if !FFESTB_KILL_EASY_
static void
ffestb_subr_kill_read_ ()
{
  ffestpReadIx ix;

  for (ix = 0; ix < FFESTP_readix; ++ix)
    {
      if (ffestp_file.read.read_spec[ix].kw_or_val_present)
	{
	  if (ffestp_file.read.read_spec[ix].kw_present)
	    ffelex_token_kill (ffestp_file.read.read_spec[ix].kw);
	  if (ffestp_file.read.read_spec[ix].value_present)
	    ffelex_token_kill (ffestp_file.read.read_spec[ix].value);
	}
    }
}

#endif
/* ffestb_subr_kill_rewrite_ -- Kill REWRITE statement data structure

   ffestb_subr_kill_rewrite_();

   Kills all tokens in the REWRITE data structure.  */

#if !FFESTB_KILL_EASY_
static void
ffestb_subr_kill_rewrite_ ()
{
  ffestpRewriteIx ix;

  for (ix = 0; ix < FFESTP_rewriteix; ++ix)
    {
      if (ffestp_file.rewrite.rewrite_spec[ix].kw_or_val_present)
	{
	  if (ffestp_file.rewrite.rewrite_spec[ix].kw_present)
	    ffelex_token_kill (ffestp_file.rewrite.rewrite_spec[ix].kw);
	  if (ffestp_file.rewrite.rewrite_spec[ix].value_present)
	    ffelex_token_kill (ffestp_file.rewrite.rewrite_spec[ix].value);
	}
    }
}

#endif
/* ffestb_subr_kill_type_ -- Kill TYPE statement data structure

   ffestb_subr_kill_type_();

   Kills all tokens in the TYPE data structure.	 */

#if !FFESTB_KILL_EASY_
static void
ffestb_subr_kill_type_ ()
{
  ffestpTypeIx ix;

  for (ix = 0; ix < FFESTP_typeix; ++ix)
    {
      if (ffestp_file.type.type_spec[ix].kw_or_val_present)
	{
	  if (ffestp_file.type.type_spec[ix].kw_present)
	    ffelex_token_kill (ffestp_file.type.type_spec[ix].kw);
	  if (ffestp_file.type.type_spec[ix].value_present)
	    ffelex_token_kill (ffestp_file.type.type_spec[ix].value);
	}
    }
}

#endif
/* ffestb_subr_kill_write_ -- Kill WRITE statement data structure

   ffestb_subr_kill_write_();

   Kills all tokens in the WRITE data structure.  */

#if !FFESTB_KILL_EASY_
static void
ffestb_subr_kill_write_ ()
{
  ffestpWriteIx ix;

  for (ix = 0; ix < FFESTP_writeix; ++ix)
    {
      if (ffestp_file.write.write_spec[ix].kw_or_val_present)
	{
	  if (ffestp_file.write.write_spec[ix].kw_present)
	    ffelex_token_kill (ffestp_file.write.write_spec[ix].kw);
	  if (ffestp_file.write.write_spec[ix].value_present)
	    ffelex_token_kill (ffestp_file.write.write_spec[ix].value);
	}
    }
}

#endif
/* ffestb_beru -- Parse the BACKSPACE/ENDFILE/REWIND/UNLOCK statement

   return ffestb_beru;	// to lexer

   Make sure the statement has a valid form for the BACKSPACE/ENDFILE/REWIND/
   UNLOCK statement.  If it does, implement the statement.  */

ffelexHandler
ffestb_beru (ffelexToken t)
{
  ffelexHandler next;
  ffestpBeruIx ix;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeEQUALS:
	case FFELEX_typePOINTS:
	case FFELEX_typeCOLON:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeNAME:
	case FFELEX_typeNUMBER:
	  ffesta_confirmed ();
	  break;

	case FFELEX_typeOPEN_PAREN:
	  for (ix = 0; ix < FFESTP_beruix; ++ix)
	    ffestp_file.beru.beru_spec[ix].kw_or_val_present = FALSE;
	  ffesta_tokens[1] = ffelex_token_use (t);
	  return (ffelexHandler) ffestb_beru2_;

	default:
	  break;
	}

      for (ix = 0; ix < FFESTP_beruix; ++ix)
	ffestp_file.beru.beru_spec[ix].kw_or_val_present = FALSE;
      return (ffelexHandler) (*((ffelexHandler)
				ffeexpr_rhs (ffesta_output_pool,
					     FFEEXPR_contextFILENUM,
					  (ffeexprCallback) ffestb_beru1_)))
	(t);

    case FFELEX_typeNAMES:
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeEQUALS:
	case FFELEX_typePOINTS:
	case FFELEX_typeCOLON:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  ffesta_confirmed ();
	  break;

	case FFELEX_typeOPEN_PAREN:
	  if (ffelex_token_length (ffesta_tokens[0])
	      != ffestb_args.beru.len)
	    break;

	  for (ix = 0; ix < FFESTP_beruix; ++ix)
	    ffestp_file.beru.beru_spec[ix].kw_or_val_present = FALSE;
	  ffesta_tokens[1] = ffelex_token_use (t);
	  return (ffelexHandler) ffestb_beru2_;

	default:
	  break;
	}
      for (ix = 0; ix < FFESTP_beruix; ++ix)
	ffestp_file.beru.beru_spec[ix].kw_or_val_present = FALSE;
      next = (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		   FFEEXPR_contextFILENUM, (ffeexprCallback) ffestb_beru1_);
      next = (ffelexHandler) ffelex_splice_tokens (next, ffesta_tokens[0],
						   ffestb_args.beru.len);
      if (next == NULL)
	return (ffelexHandler) ffelex_swallow_tokens (t,
					       (ffelexHandler) ffesta_zero);
      return (ffelexHandler) (*next) (t);

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.beru.badname, ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.beru.badname, t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */
}

/* ffestb_beru1_ -- "BACKSPACE/ENDFILE/REWIND/UNLOCK" expr

   (ffestb_beru1_)  // to expression handler

   Make sure the next token is an EOS or SEMICOLON.  */

static ffelexHandler
ffestb_beru1_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (expr == NULL)
	break;
      ffesta_confirmed ();
      ffestp_file.beru.beru_spec[FFESTP_beruixUNIT].kw_or_val_present
	= TRUE;
      ffestp_file.beru.beru_spec[FFESTP_beruixUNIT].kw_present = FALSE;
      ffestp_file.beru.beru_spec[FFESTP_beruixUNIT].value_present = TRUE;
      ffestp_file.beru.beru_spec[FFESTP_beruixUNIT].value_is_label
	= FALSE;
      ffestp_file.beru.beru_spec[FFESTP_beruixUNIT].value
	= ffelex_token_use (ft);
      ffestp_file.beru.beru_spec[FFESTP_beruixUNIT].u.expr = expr;
      if (!ffesta_is_inhibited ())
	{
	  switch (ffesta_first_kw)
	    {
	    case FFESTR_firstBACKSPACE:
	      ffestc_R919 ();
	      break;

	    case FFESTR_firstENDFILE:
	    case FFESTR_firstEND:
	      ffestc_R920 ();
	      break;

	    case FFESTR_firstREWIND:
	      ffestc_R921 ();
	      break;

#if FFESTR_VXT
	    case FFESTR_firstUNLOCK:
	      ffestc_V022 ();
	      break;
#endif

	    default:
	      assert (FALSE);
	    }
	}
      ffestb_subr_kill_beru_ ();
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  ffestb_subr_kill_beru_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.beru.badname, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_beru2_ -- "BACKSPACE/ENDFILE/REWIND/UNLOCK" OPEN_PAREN

   return ffestb_beru2_;  // to lexer

   Handle expr construct (not NAME=expr construct) here.  */

static ffelexHandler
ffestb_beru2_ (ffelexToken t)
{
  ffelexToken nt;
  ffelexHandler next;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_tokens[2] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_beru3_;

    default:
      nt = ffesta_tokens[1];
      next = (ffelexHandler) (*((ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
	     FFEEXPR_contextFILENUMAMBIG, (ffeexprCallback) ffestb_beru4_)))
	(nt);
      ffelex_token_kill (nt);
      return (ffelexHandler) (*next) (t);
    }
}

/* ffestb_beru3_ -- "BACKSPACE/ENDFILE/REWIND/UNLOCK" OPEN_PAREN NAME

   return ffestb_beru3_;  // to lexer

   If EQUALS here, go to states that handle it.	 Else, send NAME and this
   token thru expression handler.  */

static ffelexHandler
ffestb_beru3_ (ffelexToken t)
{
  ffelexHandler next;
  ffelexToken nt;
  ffelexToken ot;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEQUALS:
      ffelex_token_kill (ffesta_tokens[1]);
      nt = ffesta_tokens[2];
      next = (ffelexHandler) ffestb_beru5_ (nt);
      ffelex_token_kill (nt);
      return (ffelexHandler) (*next) (t);

    default:
      nt = ffesta_tokens[1];
      ot = ffesta_tokens[2];
      next = (ffelexHandler) (*((ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
	     FFEEXPR_contextFILENUMAMBIG, (ffeexprCallback) ffestb_beru4_)))
	(nt);
      ffelex_token_kill (nt);
      next = (ffelexHandler) (*next) (ot);
      ffelex_token_kill (ot);
      return (ffelexHandler) (*next) (t);
    }
}

/* ffestb_beru4_ -- "BACKSPACE/ENDFILE/REWIND/UNLOCK" OPEN_PAREN expr [CLOSE_PAREN]

   (ffestb_beru4_)  // to expression handler

   Handle COMMA or EOS/SEMICOLON here.

   15-Feb-91  JCB  1.2
      Now using new mechanism whereby expr comes back as opITEM if the
      expr is considered part (or all) of an I/O control list (and should
      be stripped of its outer opITEM node) or not if it is considered
      a plain unit number that happens to have been enclosed in parens.
   26-Mar-90  JCB  1.1
      No longer expecting close-paren here because of constructs like
      BACKSPACE (5)+2, so now expecting either COMMA because it was a
      construct like BACKSPACE (5+2,... or EOS/SEMICOLON because it is like
      the former construct.  Ah, the vagaries of Fortran.  */

static ffelexHandler
ffestb_beru4_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  bool inlist;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
    case FFELEX_typeCLOSE_PAREN:
      if (expr == NULL)
	break;
      if (ffebld_op (expr) == FFEBLD_opITEM)
	{
	  inlist = TRUE;
	  expr = ffebld_head (expr);
	}
      else
	inlist = FALSE;
      ffestp_file.beru.beru_spec[FFESTP_beruixUNIT].kw_or_val_present
	= TRUE;
      ffestp_file.beru.beru_spec[FFESTP_beruixUNIT].kw_present = FALSE;
      ffestp_file.beru.beru_spec[FFESTP_beruixUNIT].value_present = TRUE;
      ffestp_file.beru.beru_spec[FFESTP_beruixUNIT].value_is_label
	= FALSE;
      ffestp_file.beru.beru_spec[FFESTP_beruixUNIT].value
	= ffelex_token_use (ft);
      ffestp_file.beru.beru_spec[FFESTP_beruixUNIT].u.expr = expr;
      if (inlist)
	return (ffelexHandler) ffestb_beru9_ (t);
      return (ffelexHandler) ffestb_beru10_ (t);

    default:
      break;
    }

  ffestb_subr_kill_beru_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.beru.badname, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_beru5_ -- "BACKSPACE/ENDFILE/REWIND/UNLOCK" OPEN_PAREN [external-file-unit
		    COMMA]

   return ffestb_beru5_;  // to lexer

   Handle expr construct (not NAME=expr construct) here.  */

static ffelexHandler
ffestb_beru5_ (ffelexToken t)
{
  ffestrGenio kw;

  ffestb_local_.beru.label = FALSE;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      kw = ffestr_genio (t);
      switch (kw)
	{
	case FFESTR_genioERR:
	  ffestb_local_.beru.ix = FFESTP_beruixERR;
	  ffestb_local_.beru.label = TRUE;
	  break;

	case FFESTR_genioIOSTAT:
	  ffestb_local_.beru.ix = FFESTP_beruixIOSTAT;
	  ffestb_local_.beru.left = TRUE;
	  ffestb_local_.beru.context = FFEEXPR_contextFILEINT;
	  break;

	case FFESTR_genioUNIT:
	  ffestb_local_.beru.ix = FFESTP_beruixUNIT;
	  ffestb_local_.beru.left = FALSE;
	  ffestb_local_.beru.context = FFEEXPR_contextFILENUM;
	  break;

	default:
	  goto bad;		/* :::::::::::::::::::: */
	}
      if (ffestp_file.beru.beru_spec[ffestb_local_.beru.ix]
	  .kw_or_val_present)
	break;			/* Can't specify a keyword twice! */
      ffestp_file.beru.beru_spec[ffestb_local_.beru.ix]
	.kw_or_val_present = TRUE;
      ffestp_file.beru.beru_spec[ffestb_local_.beru.ix]
	.kw_present = TRUE;
      ffestp_file.beru.beru_spec[ffestb_local_.beru.ix]
	.value_present = FALSE;
      ffestp_file.beru.beru_spec[ffestb_local_.beru.ix].value_is_label
	= ffestb_local_.beru.label;
      ffestp_file.beru.beru_spec[ffestb_local_.beru.ix].kw
	= ffelex_token_use (t);
      return (ffelexHandler) ffestb_beru6_;

    default:
      break;
    }

bad:				/* :::::::::::::::::::: */
  ffestb_subr_kill_beru_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.beru.badname, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_beru6_ -- "BACKSPACE/ENDFILE/REWIND/UNLOCK" OPEN_PAREN [external-file-unit
		    COMMA] NAME

   return ffestb_beru6_;  // to lexer

   Make sure EQUALS here, send next token to expression handler.  */

static ffelexHandler
ffestb_beru6_ (ffelexToken t)
{

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEQUALS:
      ffesta_confirmed ();
      if (ffestb_local_.beru.label)
	return (ffelexHandler) ffestb_beru8_;
      if (ffestb_local_.beru.left)
	return (ffelexHandler) ffeexpr_lhs (ffesta_output_pool,
					    ffestb_local_.beru.context,
					    (ffeexprCallback) ffestb_beru7_);
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
					  ffestb_local_.beru.context,
					  (ffeexprCallback) ffestb_beru7_);

    default:
      break;
    }

  ffestb_subr_kill_beru_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.beru.badname, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_beru7_ -- "BACKSPACE/ENDFILE/REWIND/UNLOCK" OPEN_PAREN ... NAME EQUALS expr

   (ffestb_beru7_)  // to expression handler

   Handle COMMA or CLOSE_PAREN here.  */

static ffelexHandler
ffestb_beru7_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
    case FFELEX_typeCLOSE_PAREN:
      if (expr == NULL)
	break;
      ffestp_file.beru.beru_spec[ffestb_local_.beru.ix].value_present
	= TRUE;
      ffestp_file.beru.beru_spec[ffestb_local_.beru.ix].value
	= ffelex_token_use (ft);
      ffestp_file.beru.beru_spec[ffestb_local_.beru.ix].u.expr = expr;
      if (ffelex_token_type (t) == FFELEX_typeCOMMA)
	return (ffelexHandler) ffestb_beru5_;
      return (ffelexHandler) ffestb_beru10_;

    default:
      break;
    }

  ffestb_subr_kill_beru_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.beru.badname, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_beru8_ -- "BACKSPACE/ENDFILE/REWIND/UNLOCK" OPEN_PAREN ... NAME EQUALS

   return ffestb_beru8_;  // to lexer

   Handle NUMBER for label here.  */

static ffelexHandler
ffestb_beru8_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNUMBER:
      ffestp_file.beru.beru_spec[ffestb_local_.beru.ix].value_present
	= TRUE;
      ffestp_file.beru.beru_spec[ffestb_local_.beru.ix].value
	= ffelex_token_use (t);
      return (ffelexHandler) ffestb_beru9_;

    default:
      break;
    }

  ffestb_subr_kill_beru_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.beru.badname, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_beru9_ -- "BACKSPACE/ENDFILE/REWIND/UNLOCK" OPEN_PAREN ... NAME EQUALS
		   NUMBER

   return ffestb_beru9_;  // to lexer

   Handle COMMA or CLOSE_PAREN here.  */

static ffelexHandler
ffestb_beru9_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      return (ffelexHandler) ffestb_beru5_;

    case FFELEX_typeCLOSE_PAREN:
      return (ffelexHandler) ffestb_beru10_;

    default:
      break;
    }

  ffestb_subr_kill_beru_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.beru.badname, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_beru10_ -- "BACKSPACE/ENDFILE/REWIND/UNLOCK" OPEN_PAREN ... CLOSE_PAREN

   return ffestb_beru10_;  // to lexer

   Handle EOS or SEMICOLON here.  */

static ffelexHandler
ffestb_beru10_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	{
	  switch (ffesta_first_kw)
	    {
	    case FFESTR_firstBACKSPACE:
	      ffestc_R919 ();
	      break;

	    case FFESTR_firstENDFILE:
	    case FFESTR_firstEND:
	      ffestc_R920 ();
	      break;

	    case FFESTR_firstREWIND:
	      ffestc_R921 ();
	      break;

#if FFESTR_VXT
	    case FFESTR_firstUNLOCK:
	      ffestc_V022 ();
	      break;
#endif

	    default:
	      assert (FALSE);
	    }
	}
      ffestb_subr_kill_beru_ ();
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  ffestb_subr_kill_beru_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.beru.badname, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_vxtcode -- Parse the VXT DECODE/ENCODE statement

   return ffestb_vxtcode;  // to lexer

   Make sure the statement has a valid form for the VXT DECODE/ENCODE
   statement.  If it does, implement the statement.  */

#if FFESTR_VXT
ffelexHandler
ffestb_vxtcode (ffelexToken t)
{
  ffestpVxtcodeIx ix;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	case FFELEX_typeNAME:
	case FFELEX_typeNUMBER:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeOPEN_PAREN:
	  for (ix = 0; ix < FFESTP_vxtcodeix; ++ix)
	    ffestp_file.vxtcode.vxtcode_spec[ix].kw_or_val_present = FALSE;
	  return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		FFEEXPR_contextFILENUM, (ffeexprCallback) ffestb_vxtcode1_);
	}

    case FFELEX_typeNAMES:
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeOPEN_PAREN:
	  if (ffelex_token_length (ffesta_tokens[0])
	      != ffestb_args.vxtcode.len)
	    goto bad_0;		/* :::::::::::::::::::: */

	  for (ix = 0; ix < FFESTP_vxtcodeix; ++ix)
	    ffestp_file.vxtcode.vxtcode_spec[ix].kw_or_val_present = FALSE;
	  return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		FFEEXPR_contextFILENUM, (ffeexprCallback) ffestb_vxtcode1_);
	}

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.vxtcode.badname, ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.vxtcode.badname, t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */
}

/* ffestb_vxtcode1_ -- "VXTCODE" OPEN_PAREN expr

   (ffestb_vxtcode1_)  // to expression handler

   Handle COMMA here.  */

static ffelexHandler
ffestb_vxtcode1_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      if (expr == NULL)
	break;
      ffestp_file.vxtcode.vxtcode_spec[FFESTP_vxtcodeixC].kw_or_val_present
	= TRUE;
      ffestp_file.vxtcode.vxtcode_spec[FFESTP_vxtcodeixC].kw_present = FALSE;
      ffestp_file.vxtcode.vxtcode_spec[FFESTP_vxtcodeixC].value_present = TRUE;
      ffestp_file.vxtcode.vxtcode_spec[FFESTP_vxtcodeixC].value_is_label
	= FALSE;
      ffestp_file.vxtcode.vxtcode_spec[FFESTP_vxtcodeixC].value
	= ffelex_token_use (ft);
      ffestp_file.vxtcode.vxtcode_spec[FFESTP_vxtcodeixC].u.expr = expr;
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
	     FFEEXPR_contextFILEFORMAT, (ffeexprCallback) ffestb_vxtcode2_);

    default:
      break;
    }

  ffestb_subr_kill_vxtcode_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.vxtcode.badname, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_vxtcode2_ -- "VXTCODE" OPEN_PAREN expr COMMA expr

   (ffestb_vxtcode2_)  // to expression handler

   Handle COMMA here.  */

static ffelexHandler
ffestb_vxtcode2_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      ffestp_file.vxtcode.vxtcode_spec[FFESTP_vxtcodeixF].kw_or_val_present
	= TRUE;
      ffestp_file.vxtcode.vxtcode_spec[FFESTP_vxtcodeixF].kw_present = FALSE;
      ffestp_file.vxtcode.vxtcode_spec[FFESTP_vxtcodeixF].value_present = TRUE;
      ffestp_file.vxtcode.vxtcode_spec[FFESTP_vxtcodeixF].value_is_label
	= (expr == NULL);
      ffestp_file.vxtcode.vxtcode_spec[FFESTP_vxtcodeixF].value
	= ffelex_token_use (ft);
      ffestp_file.vxtcode.vxtcode_spec[FFESTP_vxtcodeixF].u.expr = expr;
      if (ffesta_first_kw == FFESTR_firstENCODE)
	return (ffelexHandler) ffeexpr_lhs (ffesta_output_pool,
					    FFEEXPR_contextFILEVXTCODE,
					(ffeexprCallback) ffestb_vxtcode3_);
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
					  FFEEXPR_contextFILEVXTCODE,
					(ffeexprCallback) ffestb_vxtcode3_);

    default:
      break;
    }

  ffestb_subr_kill_vxtcode_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.vxtcode.badname, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_vxtcode3_ -- "VXTCODE" OPEN_PAREN expr COMMA expr COMMA expr

   (ffestb_vxtcode3_)  // to expression handler

   Handle COMMA or CLOSE_PAREN here.  */

static ffelexHandler
ffestb_vxtcode3_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
    case FFELEX_typeCLOSE_PAREN:
      if (expr == NULL)
	break;
      ffestp_file.vxtcode.vxtcode_spec[FFESTP_vxtcodeixB].kw_or_val_present
	= TRUE;
      ffestp_file.vxtcode.vxtcode_spec[FFESTP_vxtcodeixB].kw_present = FALSE;
      ffestp_file.vxtcode.vxtcode_spec[FFESTP_vxtcodeixB].value_present = TRUE;
      ffestp_file.vxtcode.vxtcode_spec[FFESTP_vxtcodeixB].value_is_label
	= FALSE;
      ffestp_file.vxtcode.vxtcode_spec[FFESTP_vxtcodeixB].value
	= ffelex_token_use (ft);
      ffestp_file.vxtcode.vxtcode_spec[FFESTP_vxtcodeixB].u.expr = expr;
      if (ffelex_token_type (t) == FFELEX_typeCOMMA)
	return (ffelexHandler) ffestb_vxtcode4_;
      return (ffelexHandler) ffestb_vxtcode9_;

    default:
      break;
    }

  ffestb_subr_kill_vxtcode_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.vxtcode.badname, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_vxtcode4_ -- "VXTCODE" OPEN_PAREN ...

   return ffestb_vxtcode4_;  // to lexer

   Handle NAME=expr construct here.  */

static ffelexHandler
ffestb_vxtcode4_ (ffelexToken t)
{
  ffestrGenio kw;

  ffestb_local_.vxtcode.label = FALSE;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      kw = ffestr_genio (t);
      switch (kw)
	{
	case FFESTR_genioERR:
	  ffestb_local_.vxtcode.ix = FFESTP_vxtcodeixERR;
	  ffestb_local_.vxtcode.label = TRUE;
	  break;

	case FFESTR_genioIOSTAT:
	  ffestb_local_.vxtcode.ix = FFESTP_vxtcodeixIOSTAT;
	  ffestb_local_.vxtcode.left = TRUE;
	  ffestb_local_.vxtcode.context = FFEEXPR_contextFILEINT;
	  break;

	default:
	  goto bad;		/* :::::::::::::::::::: */
	}
      if (ffestp_file.vxtcode.vxtcode_spec[ffestb_local_.vxtcode.ix]
	  .kw_or_val_present)
	break;			/* Can't specify a keyword twice! */
      ffestp_file.vxtcode.vxtcode_spec[ffestb_local_.vxtcode.ix]
	.kw_or_val_present = TRUE;
      ffestp_file.vxtcode.vxtcode_spec[ffestb_local_.vxtcode.ix]
	.kw_present = TRUE;
      ffestp_file.vxtcode.vxtcode_spec[ffestb_local_.vxtcode.ix]
	.value_present = FALSE;
      ffestp_file.vxtcode.vxtcode_spec[ffestb_local_.vxtcode.ix].value_is_label
	= ffestb_local_.vxtcode.label;
      ffestp_file.vxtcode.vxtcode_spec[ffestb_local_.vxtcode.ix].kw
	= ffelex_token_use (t);
      return (ffelexHandler) ffestb_vxtcode5_;

    default:
      break;
    }

bad:				/* :::::::::::::::::::: */
  ffestb_subr_kill_vxtcode_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.vxtcode.badname, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_vxtcode5_ -- "VXTCODE" OPEN_PAREN [external-file-unit COMMA [format
		   COMMA]] NAME

   return ffestb_vxtcode5_;  // to lexer

   Make sure EQUALS here, send next token to expression handler.  */

static ffelexHandler
ffestb_vxtcode5_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEQUALS:
      ffesta_confirmed ();
      if (ffestb_local_.vxtcode.label)
	return (ffelexHandler) ffestb_vxtcode7_;
      if (ffestb_local_.vxtcode.left)
	return (ffelexHandler) ffeexpr_lhs (ffesta_output_pool,
					    ffestb_local_.vxtcode.context,
					(ffeexprCallback) ffestb_vxtcode6_);
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
					  ffestb_local_.vxtcode.context,
					(ffeexprCallback) ffestb_vxtcode6_);

    default:
      break;
    }

  ffestb_subr_kill_vxtcode_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.vxtcode.badname, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_vxtcode6_ -- "VXTCODE" OPEN_PAREN ... NAME EQUALS expr

   (ffestb_vxtcode6_)  // to expression handler

   Handle COMMA or CLOSE_PAREN here.  */

static ffelexHandler
ffestb_vxtcode6_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
    case FFELEX_typeCLOSE_PAREN:
      if (expr == NULL)
	break;
      ffestp_file.vxtcode.vxtcode_spec[ffestb_local_.vxtcode.ix].value_present
	= TRUE;
      ffestp_file.vxtcode.vxtcode_spec[ffestb_local_.vxtcode.ix].value
	= ffelex_token_use (ft);
      ffestp_file.vxtcode.vxtcode_spec[ffestb_local_.vxtcode.ix].u.expr = expr;
      if (ffelex_token_type (t) == FFELEX_typeCOMMA)
	return (ffelexHandler) ffestb_vxtcode4_;
      return (ffelexHandler) ffestb_vxtcode9_;

    default:
      break;
    }

  ffestb_subr_kill_vxtcode_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.vxtcode.badname, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_vxtcode7_ -- "VXTCODE" OPEN_PAREN ... NAME EQUALS

   return ffestb_vxtcode7_;  // to lexer

   Handle NUMBER for label here.  */

static ffelexHandler
ffestb_vxtcode7_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNUMBER:
      ffestp_file.vxtcode.vxtcode_spec[ffestb_local_.vxtcode.ix].value_present
	= TRUE;
      ffestp_file.vxtcode.vxtcode_spec[ffestb_local_.vxtcode.ix].value
	= ffelex_token_use (t);
      return (ffelexHandler) ffestb_vxtcode8_;

    default:
      break;
    }

  ffestb_subr_kill_vxtcode_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.vxtcode.badname, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_vxtcode8_ -- "VXTCODE" OPEN_PAREN ... NAME EQUALS NUMBER

   return ffestb_vxtcode8_;  // to lexer

   Handle COMMA or CLOSE_PAREN here.  */

static ffelexHandler
ffestb_vxtcode8_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      return (ffelexHandler) ffestb_vxtcode4_;

    case FFELEX_typeCLOSE_PAREN:
      return (ffelexHandler) ffestb_vxtcode9_;

    default:
      break;
    }

  ffestb_subr_kill_vxtcode_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.vxtcode.badname, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_vxtcode9_ -- "VXTCODE" OPEN_PAREN ... CLOSE_PAREN

   return ffestb_vxtcode9_;  // to lexer

   Handle EOS or SEMICOLON here.

   07-Jun-90  JCB  1.1
      Context for ENCODE/DECODE expressions is now IOLISTDF instead of IOLIST
      since they apply to internal files.  */

static ffelexHandler
ffestb_vxtcode9_ (ffelexToken t)
{
  ffelexHandler next;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	{
	  if (ffesta_first_kw == FFESTR_firstENCODE)
	    {
	      ffestc_V023_start ();
	      ffestc_V023_finish ();
	    }
	  else
	    {
	      ffestc_V024_start ();
	      ffestc_V024_finish ();
	    }
	}
      ffestb_subr_kill_vxtcode_ ();
      return (ffelexHandler) ffesta_zero (t);

    case FFELEX_typeNAME:
    case FFELEX_typeOPEN_PAREN:
    case FFELEX_typeCOMMA:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	if (ffesta_first_kw == FFESTR_firstENCODE)
	  ffestc_V023_start ();
	else
	  ffestc_V024_start ();
      ffestb_subr_kill_vxtcode_ ();
      if (ffesta_first_kw == FFESTR_firstDECODE)
	next = (ffelexHandler) ffeexpr_lhs (ffesta_output_pool,
					    FFEEXPR_contextIOLISTDF,
				       (ffeexprCallback) ffestb_vxtcode10_);
      else
	next = (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
					    FFEEXPR_contextIOLISTDF,
				       (ffeexprCallback) ffestb_vxtcode10_);

      /* EXTENSION: Allow an optional preceding COMMA here if not pedantic.
	 (f2c provides this extension, as do other compilers, supposedly.) */

      if (!ffe_is_pedantic () && (ffelex_token_type (t) == FFELEX_typeCOMMA))
	return next;

      return (ffelexHandler) (*next) (t);

    default:
      break;
    }

  ffestb_subr_kill_vxtcode_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.vxtcode.badname, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_vxtcode10_ -- "VXTCODE(...)" expr

   (ffestb_vxtcode10_)	// to expression handler

   Handle COMMA or EOS/SEMICOLON here.

   07-Jun-90  JCB  1.1
      Context for ENCODE/DECODE expressions is now IOLISTDF instead of IOLIST
      since they apply to internal files.  */

static ffelexHandler
ffestb_vxtcode10_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      if (expr == NULL)
	break;
      if (!ffesta_is_inhibited ())
	if (ffesta_first_kw == FFESTR_firstENCODE)
	  ffestc_V023_item (expr, ft);
	else
	  ffestc_V024_item (expr, ft);
      if (ffesta_first_kw == FFESTR_firstDECODE)
	return (ffelexHandler) ffeexpr_lhs (ffesta_output_pool,
					    FFEEXPR_contextIOLISTDF,
				       (ffeexprCallback) ffestb_vxtcode10_);
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
					  FFEEXPR_contextIOLISTDF,
				       (ffeexprCallback) ffestb_vxtcode10_);

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (expr == NULL)
	break;
      if (!ffesta_is_inhibited ())
	{
	  if (ffesta_first_kw == FFESTR_firstENCODE)
	    {
	      ffestc_V023_item (expr, ft);
	      ffestc_V023_finish ();
	    }
	  else
	    {
	      ffestc_V024_item (expr, ft);
	      ffestc_V024_finish ();
	    }
	}
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  if (!ffesta_is_inhibited ())
    if (ffesta_first_kw == FFESTR_firstENCODE)
      ffestc_V023_finish ();
    else
      ffestc_V024_finish ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.vxtcode.badname, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

#endif
/* ffestb_R904 -- Parse an OPEN statement

   return ffestb_R904;	// to lexer

   Make sure the statement has a valid form for an OPEN statement.
   If it does, implement the statement.	 */

ffelexHandler
ffestb_R904 (ffelexToken t)
{
  ffestpOpenIx ix;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstOPEN)
	goto bad_0;		/* :::::::::::::::::::: */
      break;

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstOPEN)
	goto bad_0;		/* :::::::::::::::::::: */
      if (ffelex_token_length (ffesta_tokens[0]) != FFESTR_firstlOPEN)
	goto bad_0;		/* :::::::::::::::::::: */
      break;

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeOPEN_PAREN:
      break;

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
    case FFELEX_typeCOMMA:
    case FFELEX_typeCOLONCOLON:
      ffesta_confirmed ();	/* Error, but clearly intended. */
      goto bad_1;		/* :::::::::::::::::::: */

    default:
      goto bad_1;		/* :::::::::::::::::::: */
    }

  for (ix = 0; ix < FFESTP_openix; ++ix)
    ffestp_file.open.open_spec[ix].kw_or_val_present = FALSE;

  return (ffelexHandler) ffestb_R9041_;

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "OPEN", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "OPEN", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */
}

/* ffestb_R9041_ -- "OPEN" OPEN_PAREN

   return ffestb_R9041_;  // to lexer

   Handle expr construct (not NAME=expr construct) here.  */

static ffelexHandler
ffestb_R9041_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_R9042_;

    default:
      return (ffelexHandler) (*((ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		  FFEEXPR_contextFILENUM, (ffeexprCallback) ffestb_R9043_)))
	(t);
    }
}

/* ffestb_R9042_ -- "OPEN" OPEN_PAREN NAME

   return ffestb_R9042_;  // to lexer

   If EQUALS here, go to states that handle it.	 Else, send NAME and this
   token thru expression handler.  */

static ffelexHandler
ffestb_R9042_ (ffelexToken t)
{
  ffelexHandler next;
  ffelexToken nt;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEQUALS:
      nt = ffesta_tokens[1];
      next = (ffelexHandler) ffestb_R9044_ (nt);
      ffelex_token_kill (nt);
      return (ffelexHandler) (*next) (t);

    default:
      next = (ffelexHandler) (*((ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		  FFEEXPR_contextFILENUM, (ffeexprCallback) ffestb_R9043_)))
	(ffesta_tokens[1]);
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) (*next) (t);
    }
}

/* ffestb_R9043_ -- "OPEN" OPEN_PAREN expr

   (ffestb_R9043_)  // to expression handler

   Handle COMMA or CLOSE_PAREN here.  */

static ffelexHandler
ffestb_R9043_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
    case FFELEX_typeCLOSE_PAREN:
      if (expr == NULL)
	break;
      ffestp_file.open.open_spec[FFESTP_openixUNIT].kw_or_val_present
	= TRUE;
      ffestp_file.open.open_spec[FFESTP_openixUNIT].kw_present = FALSE;
      ffestp_file.open.open_spec[FFESTP_openixUNIT].value_present = TRUE;
      ffestp_file.open.open_spec[FFESTP_openixUNIT].value_is_label
	= FALSE;
      ffestp_file.open.open_spec[FFESTP_openixUNIT].value
	= ffelex_token_use (ft);
      ffestp_file.open.open_spec[FFESTP_openixUNIT].u.expr = expr;
      if (ffelex_token_type (t) == FFELEX_typeCOMMA)
	return (ffelexHandler) ffestb_R9044_;
      return (ffelexHandler) ffestb_R9049_;

    default:
      break;
    }

  ffestb_subr_kill_open_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "OPEN", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R9044_ -- "OPEN" OPEN_PAREN [external-file-unit COMMA]

   return ffestb_R9044_;  // to lexer

   Handle expr construct (not NAME=expr construct) here.  */

static ffelexHandler
ffestb_R9044_ (ffelexToken t)
{
  ffestrOpen kw;

  ffestb_local_.open.label = FALSE;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      kw = ffestr_open (t);
      switch (kw)
	{
	case FFESTR_openACCESS:
	  ffestb_local_.open.ix = FFESTP_openixACCESS;
	  ffestb_local_.open.left = FALSE;
	  ffestb_local_.open.context = FFEEXPR_contextFILEDFCHAR;
	  break;

	case FFESTR_openACTION:
	  ffestb_local_.open.ix = FFESTP_openixACTION;
	  ffestb_local_.open.left = FALSE;
	  ffestb_local_.open.context = FFEEXPR_contextFILEDFCHAR;
	  break;

	case FFESTR_openASSOCIATEVARIABLE:
	  ffestb_local_.open.ix = FFESTP_openixASSOCIATEVARIABLE;
	  ffestb_local_.open.left = TRUE;
	  ffestb_local_.open.context = FFEEXPR_contextFILEASSOC;
	  break;

	case FFESTR_openBLANK:
	  ffestb_local_.open.ix = FFESTP_openixBLANK;
	  ffestb_local_.open.left = FALSE;
	  ffestb_local_.open.context = FFEEXPR_contextFILEDFCHAR;
	  break;

	case FFESTR_openBLOCKSIZE:
	  ffestb_local_.open.ix = FFESTP_openixBLOCKSIZE;
	  ffestb_local_.open.left = FALSE;
	  ffestb_local_.open.context = FFEEXPR_contextFILENUM;
	  break;

	case FFESTR_openBUFFERCOUNT:
	  ffestb_local_.open.ix = FFESTP_openixBUFFERCOUNT;
	  ffestb_local_.open.left = FALSE;
	  ffestb_local_.open.context = FFEEXPR_contextFILENUM;
	  break;

	case FFESTR_openCARRIAGECONTROL:
	  ffestb_local_.open.ix = FFESTP_openixCARRIAGECONTROL;
	  ffestb_local_.open.left = FALSE;
	  ffestb_local_.open.context = FFEEXPR_contextFILECHAR;
	  break;

	case FFESTR_openDEFAULTFILE:
	  ffestb_local_.open.ix = FFESTP_openixDEFAULTFILE;
	  ffestb_local_.open.left = FALSE;
	  ffestb_local_.open.context = FFEEXPR_contextFILECHAR;
	  break;

	case FFESTR_openDELIM:
	  ffestb_local_.open.ix = FFESTP_openixDELIM;
	  ffestb_local_.open.left = FALSE;
	  ffestb_local_.open.context = FFEEXPR_contextFILEDFCHAR;
	  break;

	case FFESTR_openDISP:
	case FFESTR_openDISPOSE:
	  ffestb_local_.open.ix = FFESTP_openixDISPOSE;
	  ffestb_local_.open.left = FALSE;
	  ffestb_local_.open.context = FFEEXPR_contextFILECHAR;
	  break;

	case FFESTR_openERR:
	  ffestb_local_.open.ix = FFESTP_openixERR;
	  ffestb_local_.open.label = TRUE;
	  break;

	case FFESTR_openEXTENDSIZE:
	  ffestb_local_.open.ix = FFESTP_openixEXTENDSIZE;
	  ffestb_local_.open.left = FALSE;
	  ffestb_local_.open.context = FFEEXPR_contextFILENUM;
	  break;

	case FFESTR_openFILE:
	case FFESTR_openNAME:
	  ffestb_local_.open.ix = FFESTP_openixFILE;
	  ffestb_local_.open.left = FALSE;
	  ffestb_local_.open.context = FFEEXPR_contextFILECHAR;
	  break;

	case FFESTR_openFORM:
	  ffestb_local_.open.ix = FFESTP_openixFORM;
	  ffestb_local_.open.left = FALSE;
	  ffestb_local_.open.context = FFEEXPR_contextFILEDFCHAR;
	  break;

	case FFESTR_openINITIALSIZE:
	  ffestb_local_.open.ix = FFESTP_openixINITIALSIZE;
	  ffestb_local_.open.left = FALSE;
	  ffestb_local_.open.context = FFEEXPR_contextFILENUM;
	  break;

	case FFESTR_openIOSTAT:
	  ffestb_local_.open.ix = FFESTP_openixIOSTAT;
	  ffestb_local_.open.left = TRUE;
	  ffestb_local_.open.context = FFEEXPR_contextFILEINT;
	  break;

#if 0				/* Haven't added support for expression
				   context yet (though easy). */
	case FFESTR_openKEY:
	  ffestb_local_.open.ix = FFESTP_openixKEY;
	  ffestb_local_.open.left = FALSE;
	  ffestb_local_.open.context = FFEEXPR_contextFILEKEY;
	  break;
#endif

	case FFESTR_openMAXREC:
	  ffestb_local_.open.ix = FFESTP_openixMAXREC;
	  ffestb_local_.open.left = FALSE;
	  ffestb_local_.open.context = FFEEXPR_contextFILENUM;
	  break;

	case FFESTR_openNOSPANBLOCKS:
	  if (ffestp_file.open.open_spec[FFESTP_openixNOSPANBLOCKS]
	      .kw_or_val_present)
	    goto bad;		/* :::::::::::::::::::: */
	  ffestp_file.open.open_spec[FFESTP_openixNOSPANBLOCKS]
	    .kw_or_val_present = TRUE;
	  ffestp_file.open.open_spec[FFESTP_openixNOSPANBLOCKS]
	    .kw_present = TRUE;
	  ffestp_file.open.open_spec[FFESTP_openixNOSPANBLOCKS]
	    .value_present = FALSE;
	  ffestp_file.open.open_spec[FFESTP_openixNOSPANBLOCKS].kw
	    = ffelex_token_use (t);
	  return (ffelexHandler) ffestb_R9048_;

	case FFESTR_openORGANIZATION:
	  ffestb_local_.open.ix = FFESTP_openixORGANIZATION;
	  ffestb_local_.open.left = FALSE;
	  ffestb_local_.open.context = FFEEXPR_contextFILECHAR;
	  break;

	case FFESTR_openPAD:
	  ffestb_local_.open.ix = FFESTP_openixPAD;
	  ffestb_local_.open.left = FALSE;
	  ffestb_local_.open.context = FFEEXPR_contextFILEDFCHAR;
	  break;

	case FFESTR_openPOSITION:
	  ffestb_local_.open.ix = FFESTP_openixPOSITION;
	  ffestb_local_.open.left = FALSE;
	  ffestb_local_.open.context = FFEEXPR_contextFILEDFCHAR;
	  break;

	case FFESTR_openREADONLY:
	  if (ffestp_file.open.open_spec[FFESTP_openixREADONLY]
	      .kw_or_val_present)
	    goto bad;		/* :::::::::::::::::::: */
	  ffestp_file.open.open_spec[FFESTP_openixREADONLY]
	    .kw_or_val_present = TRUE;
	  ffestp_file.open.open_spec[FFESTP_openixREADONLY]
	    .kw_present = TRUE;
	  ffestp_file.open.open_spec[FFESTP_openixREADONLY]
	    .value_present = FALSE;
	  ffestp_file.open.open_spec[FFESTP_openixREADONLY].kw
	    = ffelex_token_use (t);
	  return (ffelexHandler) ffestb_R9048_;

	case FFESTR_openRECL:
	case FFESTR_openRECORDSIZE:
	  ffestb_local_.open.ix = FFESTP_openixRECL;
	  ffestb_local_.open.left = FALSE;
	  ffestb_local_.open.context = FFEEXPR_contextFILENUM;
	  break;

	case FFESTR_openRECORDTYPE:
	  ffestb_local_.open.ix = FFESTP_openixRECORDTYPE;
	  ffestb_local_.open.left = FALSE;
	  ffestb_local_.open.context = FFEEXPR_contextFILECHAR;
	  break;

	case FFESTR_openSHARED:
	  if (ffestp_file.open.open_spec[FFESTP_openixSHARED]
	      .kw_or_val_present)
	    goto bad;		/* :::::::::::::::::::: */
	  ffestp_file.open.open_spec[FFESTP_openixSHARED]
	    .kw_or_val_present = TRUE;
	  ffestp_file.open.open_spec[FFESTP_openixSHARED]
	    .kw_present = TRUE;
	  ffestp_file.open.open_spec[FFESTP_openixSHARED]
	    .value_present = FALSE;
	  ffestp_file.open.open_spec[FFESTP_openixSHARED].kw
	    = ffelex_token_use (t);
	  return (ffelexHandler) ffestb_R9048_;

	case FFESTR_openSTATUS:
	case FFESTR_openTYPE:
	  ffestb_local_.open.ix = FFESTP_openixSTATUS;
	  ffestb_local_.open.left = FALSE;
	  ffestb_local_.open.context = FFEEXPR_contextFILEDFCHAR;
	  break;

	case FFESTR_openUNIT:
	  ffestb_local_.open.ix = FFESTP_openixUNIT;
	  ffestb_local_.open.left = FALSE;
	  ffestb_local_.open.context = FFEEXPR_contextFILENUM;
	  break;

	case FFESTR_openUSEROPEN:
	  ffestb_local_.open.ix = FFESTP_openixUSEROPEN;
	  ffestb_local_.open.left = TRUE;
	  ffestb_local_.open.context = FFEEXPR_contextFILEEXTFUNC;
	  break;

	default:
	  goto bad;		/* :::::::::::::::::::: */
	}
      if (ffestp_file.open.open_spec[ffestb_local_.open.ix]
	  .kw_or_val_present)
	break;			/* Can't specify a keyword twice! */
      ffestp_file.open.open_spec[ffestb_local_.open.ix]
	.kw_or_val_present = TRUE;
      ffestp_file.open.open_spec[ffestb_local_.open.ix]
	.kw_present = TRUE;
      ffestp_file.open.open_spec[ffestb_local_.open.ix]
	.value_present = FALSE;
      ffestp_file.open.open_spec[ffestb_local_.open.ix].value_is_label
	= ffestb_local_.open.label;
      ffestp_file.open.open_spec[ffestb_local_.open.ix].kw
	= ffelex_token_use (t);
      return (ffelexHandler) ffestb_R9045_;

    default:
      break;
    }

bad:				/* :::::::::::::::::::: */
  ffestb_subr_kill_open_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "OPEN", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R9045_ -- "OPEN" OPEN_PAREN [external-file-unit COMMA] NAME

   return ffestb_R9045_;  // to lexer

   Make sure EQUALS here, send next token to expression handler.  */

static ffelexHandler
ffestb_R9045_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEQUALS:
      ffesta_confirmed ();
      if (ffestb_local_.open.label)
	return (ffelexHandler) ffestb_R9047_;
      if (ffestb_local_.open.left)
	return (ffelexHandler) ffeexpr_lhs (ffesta_output_pool,
					    ffestb_local_.open.context,
					    (ffeexprCallback) ffestb_R9046_);
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
					  ffestb_local_.open.context,
					  (ffeexprCallback) ffestb_R9046_);

    default:
      break;
    }

  ffestb_subr_kill_open_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "OPEN", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R9046_ -- "OPEN" OPEN_PAREN ... NAME EQUALS expr

   (ffestb_R9046_)  // to expression handler

   Handle COMMA or CLOSE_PAREN here.  */

static ffelexHandler
ffestb_R9046_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
    case FFELEX_typeCLOSE_PAREN:
      if (expr == NULL)
	break;
      ffestp_file.open.open_spec[ffestb_local_.open.ix].value_present
	= TRUE;
      ffestp_file.open.open_spec[ffestb_local_.open.ix].value
	= ffelex_token_use (ft);
      ffestp_file.open.open_spec[ffestb_local_.open.ix].u.expr = expr;
      if (ffelex_token_type (t) == FFELEX_typeCOMMA)
	return (ffelexHandler) ffestb_R9044_;
      return (ffelexHandler) ffestb_R9049_;

    default:
      break;
    }

  ffestb_subr_kill_open_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "OPEN", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R9047_ -- "OPEN" OPEN_PAREN ... NAME EQUALS

   return ffestb_R9047_;  // to lexer

   Handle NUMBER for label here.  */

static ffelexHandler
ffestb_R9047_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNUMBER:
      ffestp_file.open.open_spec[ffestb_local_.open.ix].value_present
	= TRUE;
      ffestp_file.open.open_spec[ffestb_local_.open.ix].value
	= ffelex_token_use (t);
      return (ffelexHandler) ffestb_R9048_;

    default:
      break;
    }

  ffestb_subr_kill_open_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "OPEN", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R9048_ -- "OPEN" OPEN_PAREN ... NAME EQUALS NUMBER

   return ffestb_R9048_;  // to lexer

   Handle COMMA or CLOSE_PAREN here.  */

static ffelexHandler
ffestb_R9048_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      return (ffelexHandler) ffestb_R9044_;

    case FFELEX_typeCLOSE_PAREN:
      return (ffelexHandler) ffestb_R9049_;

    default:
      break;
    }

  ffestb_subr_kill_open_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "OPEN", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R9049_ -- "OPEN" OPEN_PAREN ... CLOSE_PAREN

   return ffestb_R9049_;  // to lexer

   Handle EOS or SEMICOLON here.  */

static ffelexHandler
ffestb_R9049_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	ffestc_R904 ();
      ffestb_subr_kill_open_ ();
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  ffestb_subr_kill_open_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "OPEN", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R907 -- Parse a CLOSE statement

   return ffestb_R907;	// to lexer

   Make sure the statement has a valid form for a CLOSE statement.
   If it does, implement the statement.	 */

ffelexHandler
ffestb_R907 (ffelexToken t)
{
  ffestpCloseIx ix;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstCLOSE)
	goto bad_0;		/* :::::::::::::::::::: */
      break;

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstCLOSE)
	goto bad_0;		/* :::::::::::::::::::: */
      if (ffelex_token_length (ffesta_tokens[0]) != FFESTR_firstlCLOSE)
	goto bad_0;		/* :::::::::::::::::::: */
      break;

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeOPEN_PAREN:
      break;

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
    case FFELEX_typeCOMMA:
    case FFELEX_typeCOLONCOLON:
      ffesta_confirmed ();	/* Error, but clearly intended. */
      goto bad_1;		/* :::::::::::::::::::: */

    default:
      goto bad_1;		/* :::::::::::::::::::: */
    }

  for (ix = 0; ix < FFESTP_closeix; ++ix)
    ffestp_file.close.close_spec[ix].kw_or_val_present = FALSE;

  return (ffelexHandler) ffestb_R9071_;

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "CLOSE", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "CLOSE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */
}

/* ffestb_R9071_ -- "CLOSE" OPEN_PAREN

   return ffestb_R9071_;  // to lexer

   Handle expr construct (not NAME=expr construct) here.  */

static ffelexHandler
ffestb_R9071_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_R9072_;

    default:
      return (ffelexHandler) (*((ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		  FFEEXPR_contextFILENUM, (ffeexprCallback) ffestb_R9073_)))
	(t);
    }
}

/* ffestb_R9072_ -- "CLOSE" OPEN_PAREN NAME

   return ffestb_R9072_;  // to lexer

   If EQUALS here, go to states that handle it.	 Else, send NAME and this
   token thru expression handler.  */

static ffelexHandler
ffestb_R9072_ (ffelexToken t)
{
  ffelexHandler next;
  ffelexToken nt;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEQUALS:
      nt = ffesta_tokens[1];
      next = (ffelexHandler) ffestb_R9074_ (nt);
      ffelex_token_kill (nt);
      return (ffelexHandler) (*next) (t);

    default:
      next = (ffelexHandler) (*((ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		  FFEEXPR_contextFILENUM, (ffeexprCallback) ffestb_R9073_)))
	(ffesta_tokens[1]);
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) (*next) (t);
    }
}

/* ffestb_R9073_ -- "CLOSE" OPEN_PAREN expr

   (ffestb_R9073_)  // to expression handler

   Handle COMMA or CLOSE_PAREN here.  */

static ffelexHandler
ffestb_R9073_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
    case FFELEX_typeCLOSE_PAREN:
      if (expr == NULL)
	break;
      ffestp_file.close.close_spec[FFESTP_closeixUNIT].kw_or_val_present
	= TRUE;
      ffestp_file.close.close_spec[FFESTP_closeixUNIT].kw_present = FALSE;
      ffestp_file.close.close_spec[FFESTP_closeixUNIT].value_present = TRUE;
      ffestp_file.close.close_spec[FFESTP_closeixUNIT].value_is_label
	= FALSE;
      ffestp_file.close.close_spec[FFESTP_closeixUNIT].value
	= ffelex_token_use (ft);
      ffestp_file.close.close_spec[FFESTP_closeixUNIT].u.expr = expr;
      if (ffelex_token_type (t) == FFELEX_typeCOMMA)
	return (ffelexHandler) ffestb_R9074_;
      return (ffelexHandler) ffestb_R9079_;

    default:
      break;
    }

  ffestb_subr_kill_close_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "CLOSE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R9074_ -- "CLOSE" OPEN_PAREN [external-file-unit COMMA]

   return ffestb_R9074_;  // to lexer

   Handle expr construct (not NAME=expr construct) here.  */

static ffelexHandler
ffestb_R9074_ (ffelexToken t)
{
  ffestrGenio kw;

  ffestb_local_.close.label = FALSE;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      kw = ffestr_genio (t);
      switch (kw)
	{
	case FFESTR_genioERR:
	  ffestb_local_.close.ix = FFESTP_closeixERR;
	  ffestb_local_.close.label = TRUE;
	  break;

	case FFESTR_genioIOSTAT:
	  ffestb_local_.close.ix = FFESTP_closeixIOSTAT;
	  ffestb_local_.close.left = TRUE;
	  ffestb_local_.close.context = FFEEXPR_contextFILEINT;
	  break;

	case FFESTR_genioSTATUS:
	case FFESTR_genioDISP:
	case FFESTR_genioDISPOSE:
	  ffestb_local_.close.ix = FFESTP_closeixSTATUS;
	  ffestb_local_.close.left = FALSE;
	  ffestb_local_.close.context = FFEEXPR_contextFILEDFCHAR;
	  break;

	case FFESTR_genioUNIT:
	  ffestb_local_.close.ix = FFESTP_closeixUNIT;
	  ffestb_local_.close.left = FALSE;
	  ffestb_local_.close.context = FFEEXPR_contextFILENUM;
	  break;

	default:
	  goto bad;		/* :::::::::::::::::::: */
	}
      if (ffestp_file.close.close_spec[ffestb_local_.close.ix]
	  .kw_or_val_present)
	break;			/* Can't specify a keyword twice! */
      ffestp_file.close.close_spec[ffestb_local_.close.ix]
	.kw_or_val_present = TRUE;
      ffestp_file.close.close_spec[ffestb_local_.close.ix]
	.kw_present = TRUE;
      ffestp_file.close.close_spec[ffestb_local_.close.ix]
	.value_present = FALSE;
      ffestp_file.close.close_spec[ffestb_local_.close.ix].value_is_label
	= ffestb_local_.close.label;
      ffestp_file.close.close_spec[ffestb_local_.close.ix].kw
	= ffelex_token_use (t);
      return (ffelexHandler) ffestb_R9075_;

    default:
      break;
    }

bad:				/* :::::::::::::::::::: */
  ffestb_subr_kill_close_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "CLOSE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R9075_ -- "CLOSE" OPEN_PAREN [external-file-unit COMMA] NAME

   return ffestb_R9075_;  // to lexer

   Make sure EQUALS here, send next token to expression handler.  */

static ffelexHandler
ffestb_R9075_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEQUALS:
      ffesta_confirmed ();
      if (ffestb_local_.close.label)
	return (ffelexHandler) ffestb_R9077_;
      if (ffestb_local_.close.left)
	return (ffelexHandler) ffeexpr_lhs (ffesta_output_pool,
					    ffestb_local_.close.context,
					    (ffeexprCallback) ffestb_R9076_);
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
					  ffestb_local_.close.context,
					  (ffeexprCallback) ffestb_R9076_);

    default:
      break;
    }

  ffestb_subr_kill_close_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "CLOSE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R9076_ -- "CLOSE" OPEN_PAREN ... NAME EQUALS expr

   (ffestb_R9076_)  // to expression handler

   Handle COMMA or CLOSE_PAREN here.  */

static ffelexHandler
ffestb_R9076_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
    case FFELEX_typeCLOSE_PAREN:
      if (expr == NULL)
	break;
      ffestp_file.close.close_spec[ffestb_local_.close.ix].value_present
	= TRUE;
      ffestp_file.close.close_spec[ffestb_local_.close.ix].value
	= ffelex_token_use (ft);
      ffestp_file.close.close_spec[ffestb_local_.close.ix].u.expr = expr;
      if (ffelex_token_type (t) == FFELEX_typeCOMMA)
	return (ffelexHandler) ffestb_R9074_;
      return (ffelexHandler) ffestb_R9079_;

    default:
      break;
    }

  ffestb_subr_kill_close_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "CLOSE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R9077_ -- "CLOSE" OPEN_PAREN ... NAME EQUALS

   return ffestb_R9077_;  // to lexer

   Handle NUMBER for label here.  */

static ffelexHandler
ffestb_R9077_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNUMBER:
      ffestp_file.close.close_spec[ffestb_local_.close.ix].value_present
	= TRUE;
      ffestp_file.close.close_spec[ffestb_local_.close.ix].value
	= ffelex_token_use (t);
      return (ffelexHandler) ffestb_R9078_;

    default:
      break;
    }

  ffestb_subr_kill_close_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "CLOSE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R9078_ -- "CLOSE" OPEN_PAREN ... NAME EQUALS NUMBER

   return ffestb_R9078_;  // to lexer

   Handle COMMA or CLOSE_PAREN here.  */

static ffelexHandler
ffestb_R9078_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      return (ffelexHandler) ffestb_R9074_;

    case FFELEX_typeCLOSE_PAREN:
      return (ffelexHandler) ffestb_R9079_;

    default:
      break;
    }

  ffestb_subr_kill_close_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "CLOSE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R9079_ -- "CLOSE" OPEN_PAREN ... CLOSE_PAREN

   return ffestb_R9079_;  // to lexer

   Handle EOS or SEMICOLON here.  */

static ffelexHandler
ffestb_R9079_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	ffestc_R907 ();
      ffestb_subr_kill_close_ ();
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  ffestb_subr_kill_close_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "CLOSE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R909 -- Parse the READ statement

   return ffestb_R909;	// to lexer

   Make sure the statement has a valid form for the READ
   statement.  If it does, implement the statement.  */

ffelexHandler
ffestb_R909 (ffelexToken t)
{
  ffelexHandler next;
  ffestpReadIx ix;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstREAD)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeEQUALS:
	case FFELEX_typePOINTS:
	case FFELEX_typeCOLON:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeNAME:
	case FFELEX_typeNUMBER:
	  ffesta_confirmed ();
	  break;

	case FFELEX_typeOPEN_PAREN:
	  for (ix = 0; ix < FFESTP_readix; ++ix)
	    ffestp_file.read.read_spec[ix].kw_or_val_present = FALSE;
	  ffesta_tokens[1] = ffelex_token_use (t);
	  return (ffelexHandler) ffestb_R9092_;

	default:
	  break;
	}

      for (ix = 0; ix < FFESTP_readix; ++ix)
	ffestp_file.read.read_spec[ix].kw_or_val_present = FALSE;
      return (ffelexHandler) (*((ffelexHandler)
				ffeexpr_rhs (ffesta_output_pool,
	    FFEEXPR_contextFILEFORMATNML, (ffeexprCallback) ffestb_R9091_)))
	(t);

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstREAD)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	case FFELEX_typeCOMMA:
	  ffesta_confirmed ();
	  if (ffelex_token_length (ffesta_tokens[0]) != FFESTR_firstlREAD)
	    break;
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeEQUALS:
	case FFELEX_typePOINTS:
	case FFELEX_typeCOLON:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeOPEN_PAREN:
	  if (ffelex_token_length (ffesta_tokens[0]) != FFESTR_firstlREAD)
	    break;

	  for (ix = 0; ix < FFESTP_readix; ++ix)
	    ffestp_file.read.read_spec[ix].kw_or_val_present = FALSE;
	  ffesta_tokens[1] = ffelex_token_use (t);
	  return (ffelexHandler) ffestb_R9092_;

	default:
	  break;
	}
      for (ix = 0; ix < FFESTP_readix; ++ix)
	ffestp_file.read.read_spec[ix].kw_or_val_present = FALSE;
      next = (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
	     FFEEXPR_contextFILEFORMATNML, (ffeexprCallback) ffestb_R9091_);
      next = (ffelexHandler) ffelex_splice_tokens (next, ffesta_tokens[0],
						   FFESTR_firstlREAD);
      if (next == NULL)
	return (ffelexHandler) ffelex_swallow_tokens (t,
					       (ffelexHandler) ffesta_zero);
      return (ffelexHandler) (*next) (t);

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "READ", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "READ", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */
}

/* ffestb_R9091_ -- "READ" expr

   (ffestb_R9091_)  // to expression handler

   Make sure the next token is a COMMA or EOS/SEMICOLON.  */

static ffelexHandler
ffestb_R9091_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
    case FFELEX_typeCOMMA:
      ffesta_confirmed ();
      ffestp_file.read.read_spec[FFESTP_readixFORMAT].kw_or_val_present
	= TRUE;
      ffestp_file.read.read_spec[FFESTP_readixFORMAT].kw_present = FALSE;
      ffestp_file.read.read_spec[FFESTP_readixFORMAT].value_present = TRUE;
      ffestp_file.read.read_spec[FFESTP_readixFORMAT].value_is_label
	= (expr == NULL);
      ffestp_file.read.read_spec[FFESTP_readixFORMAT].value
	= ffelex_token_use (ft);
      ffestp_file.read.read_spec[FFESTP_readixFORMAT].u.expr = expr;
      if (!ffesta_is_inhibited ())
	ffestc_R909_start (TRUE);
      ffestb_subr_kill_read_ ();
      if (ffelex_token_type (t) == FFELEX_typeCOMMA)
	return (ffelexHandler) ffeexpr_lhs (ffesta_output_pool,
					    ffestc_context_iolist (),
					  (ffeexprCallback) ffestb_R90915_);
      if (!ffesta_is_inhibited ())
	ffestc_R909_finish ();
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  ffestb_subr_kill_read_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "READ", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R9092_ -- "READ" OPEN_PAREN

   return ffestb_R9092_;  // to lexer

   Handle expr construct (not NAME=expr construct) here.  */

static ffelexHandler
ffestb_R9092_ (ffelexToken t)
{
  ffelexToken nt;
  ffelexHandler next;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_tokens[2] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_R9093_;

    default:
      nt = ffesta_tokens[1];
      next = (ffelexHandler) (*((ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
	    FFEEXPR_contextFILEUNITAMBIG, (ffeexprCallback) ffestb_R9094_)))
	(nt);
      ffelex_token_kill (nt);
      return (ffelexHandler) (*next) (t);
    }
}

/* ffestb_R9093_ -- "READ" OPEN_PAREN NAME

   return ffestb_R9093_;  // to lexer

   If EQUALS here, go to states that handle it.	 Else, send NAME and this
   token thru expression handler.  */

static ffelexHandler
ffestb_R9093_ (ffelexToken t)
{
  ffelexHandler next;
  ffelexToken nt;
  ffelexToken ot;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEQUALS:
      ffelex_token_kill (ffesta_tokens[1]);
      nt = ffesta_tokens[2];
      next = (ffelexHandler) ffestb_R9098_ (nt);
      ffelex_token_kill (nt);
      return (ffelexHandler) (*next) (t);

    default:
      nt = ffesta_tokens[1];
      ot = ffesta_tokens[2];
      next = (ffelexHandler) (*((ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
	    FFEEXPR_contextFILEUNITAMBIG, (ffeexprCallback) ffestb_R9094_)))
	(nt);
      ffelex_token_kill (nt);
      next = (ffelexHandler) (*next) (ot);
      ffelex_token_kill (ot);
      return (ffelexHandler) (*next) (t);
    }
}

/* ffestb_R9094_ -- "READ" OPEN_PAREN expr [CLOSE_PAREN]

   (ffestb_R9094_)  // to expression handler

   Handle COMMA or EOS/SEMICOLON here.

   15-Feb-91  JCB  1.1
      Use new ffeexpr mechanism whereby the expr is encased in an opITEM if
      ffeexpr decided it was an item in a control list (hence a unit
      specifier), or a format specifier otherwise.  */

static ffelexHandler
ffestb_R9094_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  if (expr == NULL)
    goto bad;			/* :::::::::::::::::::: */

  if (ffebld_op (expr) != FFEBLD_opITEM)
    {
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeCOMMA:
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  ffesta_confirmed ();
	  ffestp_file.read.read_spec[FFESTP_readixFORMAT].kw_or_val_present
	    = TRUE;
	  ffestp_file.read.read_spec[FFESTP_readixFORMAT].kw_present = FALSE;
	  ffestp_file.read.read_spec[FFESTP_readixFORMAT].value_present = TRUE;
	  ffestp_file.read.read_spec[FFESTP_readixFORMAT].value_is_label
	    = FALSE;
	  ffestp_file.read.read_spec[FFESTP_readixFORMAT].value
	    = ffelex_token_use (ft);
	  ffestp_file.read.read_spec[FFESTP_readixFORMAT].u.expr = expr;
	  if (!ffesta_is_inhibited ())
	    ffestc_R909_start (TRUE);
	  ffestb_subr_kill_read_ ();
	  if (ffelex_token_type (t) == FFELEX_typeCOMMA)
	    return (ffelexHandler)
	      ffeexpr_lhs (ffesta_output_pool,
			   ffestc_context_iolist (),
			   (ffeexprCallback) ffestb_R90915_);
	  if (!ffesta_is_inhibited ())
	    ffestc_R909_finish ();
	  return (ffelexHandler) ffesta_zero (t);

	default:
	  goto bad;		/* :::::::::::::::::::: */
	}
    }

  expr = ffebld_head (expr);

  if (expr == NULL)
    goto bad;			/* :::::::::::::::::::: */

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
    case FFELEX_typeCLOSE_PAREN:
      ffestp_file.read.read_spec[FFESTP_readixUNIT].kw_or_val_present
	= TRUE;
      ffestp_file.read.read_spec[FFESTP_readixUNIT].kw_present = FALSE;
      ffestp_file.read.read_spec[FFESTP_readixUNIT].value_present = TRUE;
      ffestp_file.read.read_spec[FFESTP_readixUNIT].value_is_label
	= FALSE;
      ffestp_file.read.read_spec[FFESTP_readixUNIT].value
	= ffelex_token_use (ft);
      ffestp_file.read.read_spec[FFESTP_readixUNIT].u.expr = expr;
      if (ffelex_token_type (t) == FFELEX_typeCOMMA)
	return (ffelexHandler) ffestb_R9095_;
      return (ffelexHandler) ffestb_R90913_;

    default:
      break;
    }

bad:				/* :::::::::::::::::::: */
  ffestb_subr_kill_read_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "READ", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R9095_ -- "READ" OPEN_PAREN expr COMMA

   return ffestb_R9095_;  // to lexer

   Handle expr construct (not NAME=expr construct) here.  */

static ffelexHandler
ffestb_R9095_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_R9096_;

    default:
      return (ffelexHandler) (*((ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
	    FFEEXPR_contextFILEFORMATNML, (ffeexprCallback) ffestb_R9097_)))
	(t);
    }
}

/* ffestb_R9096_ -- "READ" OPEN_PAREN expr COMMA NAME

   return ffestb_R9096_;  // to lexer

   If EQUALS here, go to states that handle it.	 Else, send NAME and this
   token thru expression handler.  */

static ffelexHandler
ffestb_R9096_ (ffelexToken t)
{
  ffelexHandler next;
  ffelexToken nt;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEQUALS:
      nt = ffesta_tokens[1];
      next = (ffelexHandler) ffestb_R9098_ (nt);
      ffelex_token_kill (nt);
      return (ffelexHandler) (*next) (t);

    default:
      nt = ffesta_tokens[1];
      next = (ffelexHandler) (*((ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
	    FFEEXPR_contextFILEFORMATNML, (ffeexprCallback) ffestb_R9097_)))
	(nt);
      ffelex_token_kill (nt);
      return (ffelexHandler) (*next) (t);
    }
}

/* ffestb_R9097_ -- "READ" OPEN_PAREN expr COMMA expr

   (ffestb_R9097_)  // to expression handler

   Handle COMMA or CLOSE_PAREN here.  */

static ffelexHandler
ffestb_R9097_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
    case FFELEX_typeCLOSE_PAREN:
      ffestp_file.read.read_spec[FFESTP_readixFORMAT].kw_or_val_present
	= TRUE;
      ffestp_file.read.read_spec[FFESTP_readixFORMAT].kw_present = FALSE;
      ffestp_file.read.read_spec[FFESTP_readixFORMAT].value_present = TRUE;
      ffestp_file.read.read_spec[FFESTP_readixFORMAT].value_is_label
	= (expr == NULL);
      ffestp_file.read.read_spec[FFESTP_readixFORMAT].value
	= ffelex_token_use (ft);
      ffestp_file.read.read_spec[FFESTP_readixFORMAT].u.expr = expr;
      if (ffelex_token_type (t) == FFELEX_typeCOMMA)
	return (ffelexHandler) ffestb_R9098_;
      return (ffelexHandler) ffestb_R90913_;

    default:
      break;
    }

  ffestb_subr_kill_read_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "READ", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R9098_ -- "READ" OPEN_PAREN [external-file-unit COMMA [format
		   COMMA]]

   return ffestb_R9098_;  // to lexer

   Handle expr construct (not NAME=expr construct) here.  */

static ffelexHandler
ffestb_R9098_ (ffelexToken t)
{
  ffestrGenio kw;

  ffestb_local_.read.label = FALSE;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      kw = ffestr_genio (t);
      switch (kw)
	{
	case FFESTR_genioADVANCE:
	  ffestb_local_.read.ix = FFESTP_readixADVANCE;
	  ffestb_local_.read.left = FALSE;
	  ffestb_local_.read.context = FFEEXPR_contextFILEDFCHAR;
	  break;

	case FFESTR_genioEOR:
	  ffestb_local_.read.ix = FFESTP_readixEOR;
	  ffestb_local_.read.label = TRUE;
	  break;

	case FFESTR_genioERR:
	  ffestb_local_.read.ix = FFESTP_readixERR;
	  ffestb_local_.read.label = TRUE;
	  break;

	case FFESTR_genioEND:
	  ffestb_local_.read.ix = FFESTP_readixEND;
	  ffestb_local_.read.label = TRUE;
	  break;

	case FFESTR_genioFMT:
	  ffestb_local_.read.ix = FFESTP_readixFORMAT;
	  ffestb_local_.read.left = FALSE;
	  ffestb_local_.read.context = FFEEXPR_contextFILEFORMAT;
	  break;

	case FFESTR_genioIOSTAT:
	  ffestb_local_.read.ix = FFESTP_readixIOSTAT;
	  ffestb_local_.read.left = TRUE;
	  ffestb_local_.read.context = FFEEXPR_contextFILEINT;
	  break;

	case FFESTR_genioKEY:
	case FFESTR_genioKEYEQ:
	  ffestb_local_.read.ix = FFESTP_readixKEYEQ;
	  ffestb_local_.read.left = FALSE;
	  ffestb_local_.read.context = FFEEXPR_contextFILENUMCHAR;
	  break;

	case FFESTR_genioKEYGE:
	  ffestb_local_.read.ix = FFESTP_readixKEYGE;
	  ffestb_local_.read.left = FALSE;
	  ffestb_local_.read.context = FFEEXPR_contextFILENUMCHAR;
	  break;

	case FFESTR_genioKEYGT:
	  ffestb_local_.read.ix = FFESTP_readixKEYGT;
	  ffestb_local_.read.left = FALSE;
	  ffestb_local_.read.context = FFEEXPR_contextFILENUMCHAR;
	  break;

	case FFESTR_genioKEYID:
	  ffestb_local_.read.ix = FFESTP_readixKEYID;
	  ffestb_local_.read.left = FALSE;
	  ffestb_local_.read.context = FFEEXPR_contextFILENUM;
	  break;

	case FFESTR_genioNML:
	  ffestb_local_.read.ix = FFESTP_readixFORMAT;
	  ffestb_local_.read.left = TRUE;
	  ffestb_local_.read.context = FFEEXPR_contextFILENAMELIST;
	  break;

	case FFESTR_genioNULLS:
	  ffestb_local_.read.ix = FFESTP_readixNULLS;
	  ffestb_local_.read.left = TRUE;
	  ffestb_local_.read.context = FFEEXPR_contextFILEINT;
	  break;

	case FFESTR_genioREC:
	  ffestb_local_.read.ix = FFESTP_readixREC;
	  ffestb_local_.read.left = FALSE;
	  ffestb_local_.read.context = FFEEXPR_contextFILENUM;
	  break;

	case FFESTR_genioSIZE:
	  ffestb_local_.read.ix = FFESTP_readixSIZE;
	  ffestb_local_.read.left = TRUE;
	  ffestb_local_.read.context = FFEEXPR_contextFILEINT;
	  break;

	case FFESTR_genioUNIT:
	  ffestb_local_.read.ix = FFESTP_readixUNIT;
	  ffestb_local_.read.left = FALSE;
	  ffestb_local_.read.context = FFEEXPR_contextFILEUNIT;
	  break;

	default:
	  goto bad;		/* :::::::::::::::::::: */
	}
      if (ffestp_file.read.read_spec[ffestb_local_.read.ix]
	  .kw_or_val_present)
	break;			/* Can't specify a keyword twice! */
      ffestp_file.read.read_spec[ffestb_local_.read.ix]
	.kw_or_val_present = TRUE;
      ffestp_file.read.read_spec[ffestb_local_.read.ix]
	.kw_present = TRUE;
      ffestp_file.read.read_spec[ffestb_local_.read.ix]
	.value_present = FALSE;
      ffestp_file.read.read_spec[ffestb_local_.read.ix].value_is_label
	= ffestb_local_.read.label;
      ffestp_file.read.read_spec[ffestb_local_.read.ix].kw
	= ffelex_token_use (t);
      return (ffelexHandler) ffestb_R9099_;

    default:
      break;
    }

bad:				/* :::::::::::::::::::: */
  ffestb_subr_kill_read_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "READ", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R9099_ -- "READ" OPEN_PAREN [external-file-unit COMMA [format
		   COMMA]] NAME

   return ffestb_R9099_;  // to lexer

   Make sure EQUALS here, send next token to expression handler.  */

static ffelexHandler
ffestb_R9099_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEQUALS:
      ffesta_confirmed ();
      if (ffestb_local_.read.label)
	return (ffelexHandler) ffestb_R90911_;
      if (ffestb_local_.read.left)
	return (ffelexHandler)
	  ffeexpr_lhs (ffesta_output_pool,
		       ffestb_local_.read.context,
		       (ffeexprCallback) ffestb_R90910_);
      return (ffelexHandler)
	ffeexpr_rhs (ffesta_output_pool,
		     ffestb_local_.read.context,
		     (ffeexprCallback) ffestb_R90910_);

    default:
      break;
    }

  ffestb_subr_kill_read_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "READ", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R90910_ -- "READ" OPEN_PAREN ... NAME EQUALS expr

   (ffestb_R90910_)  // to expression handler

   Handle COMMA or CLOSE_PAREN here.  */

static ffelexHandler
ffestb_R90910_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
    case FFELEX_typeCLOSE_PAREN:
      if (expr == NULL)
	{
	  if (ffestb_local_.read.context == FFEEXPR_contextFILEFORMAT)
	    ffestp_file.read.read_spec[ffestb_local_.read.ix]
	      .value_is_label = TRUE;
	  else
	    break;
	}
      ffestp_file.read.read_spec[ffestb_local_.read.ix].value_present
	= TRUE;
      ffestp_file.read.read_spec[ffestb_local_.read.ix].value
	= ffelex_token_use (ft);
      ffestp_file.read.read_spec[ffestb_local_.read.ix].u.expr = expr;
      if (ffelex_token_type (t) == FFELEX_typeCOMMA)
	return (ffelexHandler) ffestb_R9098_;
      return (ffelexHandler) ffestb_R90913_;

    default:
      break;
    }

  ffestb_subr_kill_read_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "READ", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R90911_ -- "READ" OPEN_PAREN ... NAME EQUALS

   return ffestb_R90911_;  // to lexer

   Handle NUMBER for label here.  */

static ffelexHandler
ffestb_R90911_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNUMBER:
      ffestp_file.read.read_spec[ffestb_local_.read.ix].value_present
	= TRUE;
      ffestp_file.read.read_spec[ffestb_local_.read.ix].value
	= ffelex_token_use (t);
      return (ffelexHandler) ffestb_R90912_;

    default:
      break;
    }

  ffestb_subr_kill_read_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "READ", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R90912_ -- "READ" OPEN_PAREN ... NAME EQUALS NUMBER

   return ffestb_R90912_;  // to lexer

   Handle COMMA or CLOSE_PAREN here.  */

static ffelexHandler
ffestb_R90912_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      return (ffelexHandler) ffestb_R9098_;

    case FFELEX_typeCLOSE_PAREN:
      return (ffelexHandler) ffestb_R90913_;

    default:
      break;
    }

  ffestb_subr_kill_read_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "READ", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R90913_ -- "READ" OPEN_PAREN ... CLOSE_PAREN

   return ffestb_R90913_;  // to lexer

   Handle EOS or SEMICOLON here.

   15-Feb-91  JCB  1.1
      Fix to allow implied-DO construct here (OPEN_PAREN) -- actually,
      don't presume knowledge of what an initial token in an lhs context
      is going to be, let ffeexpr_lhs handle that as much as possible.	*/

static ffelexHandler
ffestb_R90913_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	{
	  ffestc_R909_start (FALSE);
	  ffestc_R909_finish ();
	}
      ffestb_subr_kill_read_ ();
      return (ffelexHandler) ffesta_zero (t);

    default:
      ffesta_confirmed ();
      /* Fall through. */
    case FFELEX_typeOPEN_PAREN:	/* Could still be assignment!! */
      break;
    }

  /* If token isn't NAME or OPEN_PAREN, ffeexpr_lhs will ultimately whine
     about it, so leave it up to that code. */

  /* EXTENSION: Allow an optional preceding COMMA here if not pedantic.	 (f2c
     provides this extension, as do other compilers, supposedly.) */

  if (!ffe_is_pedantic () && (ffelex_token_type (t) == FFELEX_typeCOMMA))
    return (ffelexHandler)
      ffeexpr_lhs (ffesta_output_pool,
		   ffestc_context_iolist (),
		   (ffeexprCallback) ffestb_R90914_);

  return (ffelexHandler) (*((ffelexHandler)
			    ffeexpr_lhs (ffesta_output_pool,
					 ffestc_context_iolist (),
					 (ffeexprCallback) ffestb_R90914_)))
    (t);
}

/* ffestb_R90914_ -- "READ(...)" expr

   (ffestb_R90914_)  // to expression handler

   Handle COMMA or EOS/SEMICOLON here.	*/

static ffelexHandler
ffestb_R90914_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      if (expr == NULL)
	break;

      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	ffestc_R909_start (FALSE);
      ffestb_subr_kill_read_ ();

      if (!ffesta_is_inhibited ())
	ffestc_R909_item (expr, ft);
      return (ffelexHandler) ffeexpr_lhs (ffesta_output_pool,
					  ffestc_context_iolist (),
					  (ffeexprCallback) ffestb_R90915_);

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (expr == NULL)
	break;

      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	ffestc_R909_start (FALSE);
      ffestb_subr_kill_read_ ();

      if (!ffesta_is_inhibited ())
	{
	  ffestc_R909_item (expr, ft);
	  ffestc_R909_finish ();
	}
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  ffestb_subr_kill_read_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "READ", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R90915_ -- "READ(...)" expr COMMA expr

   (ffestb_R90915_)  // to expression handler

   Handle COMMA or EOS/SEMICOLON here.	*/

static ffelexHandler
ffestb_R90915_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      if (expr == NULL)
	break;
      if (!ffesta_is_inhibited ())
	ffestc_R909_item (expr, ft);
      return (ffelexHandler) ffeexpr_lhs (ffesta_output_pool,
					  ffestc_context_iolist (),
					  (ffeexprCallback) ffestb_R90915_);

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (expr == NULL)
	break;
      if (!ffesta_is_inhibited ())
	{
	  ffestc_R909_item (expr, ft);
	  ffestc_R909_finish ();
	}
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_R909_finish ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "READ", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R910 -- Parse the WRITE statement

   return ffestb_R910;	// to lexer

   Make sure the statement has a valid form for the WRITE
   statement.  If it does, implement the statement.  */

ffelexHandler
ffestb_R910 (ffelexToken t)
{
  ffestpWriteIx ix;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstWRITE)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	case FFELEX_typeNAME:
	case FFELEX_typeNUMBER:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeOPEN_PAREN:
	  for (ix = 0; ix < FFESTP_writeix; ++ix)
	    ffestp_file.write.write_spec[ix].kw_or_val_present = FALSE;
	  return (ffelexHandler) ffestb_R9101_;
	}

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstWRITE)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeOPEN_PAREN:
	  if (ffelex_token_length (ffesta_tokens[0]) != FFESTR_firstlWRITE)
	    goto bad_0;		/* :::::::::::::::::::: */

	  for (ix = 0; ix < FFESTP_writeix; ++ix)
	    ffestp_file.write.write_spec[ix].kw_or_val_present = FALSE;
	  return (ffelexHandler) ffestb_R9101_;
	}

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "WRITE", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "WRITE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */
}

/* ffestb_R9101_ -- "WRITE" OPEN_PAREN

   return ffestb_R9101_;  // to lexer

   Handle expr construct (not NAME=expr construct) here.  */

static ffelexHandler
ffestb_R9101_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_R9102_;

    default:
      return (ffelexHandler) (*((ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		 FFEEXPR_contextFILEUNIT, (ffeexprCallback) ffestb_R9103_)))
	(t);
    }
}

/* ffestb_R9102_ -- "WRITE" OPEN_PAREN NAME

   return ffestb_R9102_;  // to lexer

   If EQUALS here, go to states that handle it.	 Else, send NAME and this
   token thru expression handler.  */

static ffelexHandler
ffestb_R9102_ (ffelexToken t)
{
  ffelexHandler next;
  ffelexToken nt;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEQUALS:
      nt = ffesta_tokens[1];
      next = (ffelexHandler) ffestb_R9107_ (nt);
      ffelex_token_kill (nt);
      return (ffelexHandler) (*next) (t);

    default:
      nt = ffesta_tokens[1];
      next = (ffelexHandler) (*((ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		 FFEEXPR_contextFILEUNIT, (ffeexprCallback) ffestb_R9103_)))
	(nt);
      ffelex_token_kill (nt);
      return (ffelexHandler) (*next) (t);
    }
}

/* ffestb_R9103_ -- "WRITE" OPEN_PAREN expr [CLOSE_PAREN]

   (ffestb_R9103_)  // to expression handler

   Handle COMMA or EOS/SEMICOLON here.	*/

static ffelexHandler
ffestb_R9103_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
    case FFELEX_typeCLOSE_PAREN:
      if (expr == NULL)
	break;
      ffestp_file.write.write_spec[FFESTP_writeixUNIT].kw_or_val_present
	= TRUE;
      ffestp_file.write.write_spec[FFESTP_writeixUNIT].kw_present = FALSE;
      ffestp_file.write.write_spec[FFESTP_writeixUNIT].value_present = TRUE;
      ffestp_file.write.write_spec[FFESTP_writeixUNIT].value_is_label
	= FALSE;
      ffestp_file.write.write_spec[FFESTP_writeixUNIT].value
	= ffelex_token_use (ft);
      ffestp_file.write.write_spec[FFESTP_writeixUNIT].u.expr = expr;
      if (ffelex_token_type (t) == FFELEX_typeCOMMA)
	return (ffelexHandler) ffestb_R9104_;
      return (ffelexHandler) ffestb_R91012_;

    default:
      break;
    }

  ffestb_subr_kill_write_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "WRITE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R9104_ -- "WRITE" OPEN_PAREN expr COMMA

   return ffestb_R9104_;  // to lexer

   Handle expr construct (not NAME=expr construct) here.  */

static ffelexHandler
ffestb_R9104_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_R9105_;

    default:
      return (ffelexHandler) (*((ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
	    FFEEXPR_contextFILEFORMATNML, (ffeexprCallback) ffestb_R9106_)))
	(t);
    }
}

/* ffestb_R9105_ -- "WRITE" OPEN_PAREN expr COMMA NAME

   return ffestb_R9105_;  // to lexer

   If EQUALS here, go to states that handle it.	 Else, send NAME and this
   token thru expression handler.  */

static ffelexHandler
ffestb_R9105_ (ffelexToken t)
{
  ffelexHandler next;
  ffelexToken nt;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEQUALS:
      nt = ffesta_tokens[1];
      next = (ffelexHandler) ffestb_R9107_ (nt);
      ffelex_token_kill (nt);
      return (ffelexHandler) (*next) (t);

    default:
      nt = ffesta_tokens[1];
      next = (ffelexHandler) (*((ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
	    FFEEXPR_contextFILEFORMATNML, (ffeexprCallback) ffestb_R9106_)))
	(nt);
      ffelex_token_kill (nt);
      return (ffelexHandler) (*next) (t);
    }
}

/* ffestb_R9106_ -- "WRITE" OPEN_PAREN expr COMMA expr

   (ffestb_R9106_)  // to expression handler

   Handle COMMA or CLOSE_PAREN here.  */

static ffelexHandler
ffestb_R9106_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
    case FFELEX_typeCLOSE_PAREN:
      ffestp_file.write.write_spec[FFESTP_writeixFORMAT].kw_or_val_present
	= TRUE;
      ffestp_file.write.write_spec[FFESTP_writeixFORMAT].kw_present = FALSE;
      ffestp_file.write.write_spec[FFESTP_writeixFORMAT].value_present = TRUE;
      ffestp_file.write.write_spec[FFESTP_writeixFORMAT].value_is_label
	= (expr == NULL);
      ffestp_file.write.write_spec[FFESTP_writeixFORMAT].value
	= ffelex_token_use (ft);
      ffestp_file.write.write_spec[FFESTP_writeixFORMAT].u.expr = expr;
      if (ffelex_token_type (t) == FFELEX_typeCOMMA)
	return (ffelexHandler) ffestb_R9107_;
      return (ffelexHandler) ffestb_R91012_;

    default:
      break;
    }

  ffestb_subr_kill_write_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "WRITE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R9107_ -- "WRITE" OPEN_PAREN [external-file-unit COMMA [format
		   COMMA]]

   return ffestb_R9107_;  // to lexer

   Handle expr construct (not NAME=expr construct) here.  */

static ffelexHandler
ffestb_R9107_ (ffelexToken t)
{
  ffestrGenio kw;

  ffestb_local_.write.label = FALSE;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      kw = ffestr_genio (t);
      switch (kw)
	{
	case FFESTR_genioADVANCE:
	  ffestb_local_.write.ix = FFESTP_writeixADVANCE;
	  ffestb_local_.write.left = FALSE;
	  ffestb_local_.write.context = FFEEXPR_contextFILEDFCHAR;
	  break;

	case FFESTR_genioEOR:
	  ffestb_local_.write.ix = FFESTP_writeixEOR;
	  ffestb_local_.write.label = TRUE;
	  break;

	case FFESTR_genioERR:
	  ffestb_local_.write.ix = FFESTP_writeixERR;
	  ffestb_local_.write.label = TRUE;
	  break;

	case FFESTR_genioFMT:
	  ffestb_local_.write.ix = FFESTP_writeixFORMAT;
	  ffestb_local_.write.left = FALSE;
	  ffestb_local_.write.context = FFEEXPR_contextFILEFORMAT;
	  break;

	case FFESTR_genioIOSTAT:
	  ffestb_local_.write.ix = FFESTP_writeixIOSTAT;
	  ffestb_local_.write.left = TRUE;
	  ffestb_local_.write.context = FFEEXPR_contextFILEINT;
	  break;

	case FFESTR_genioNML:
	  ffestb_local_.write.ix = FFESTP_writeixFORMAT;
	  ffestb_local_.write.left = TRUE;
	  ffestb_local_.write.context = FFEEXPR_contextFILENAMELIST;
	  break;

	case FFESTR_genioREC:
	  ffestb_local_.write.ix = FFESTP_writeixREC;
	  ffestb_local_.write.left = FALSE;
	  ffestb_local_.write.context = FFEEXPR_contextFILENUM;
	  break;

	case FFESTR_genioUNIT:
	  ffestb_local_.write.ix = FFESTP_writeixUNIT;
	  ffestb_local_.write.left = FALSE;
	  ffestb_local_.write.context = FFEEXPR_contextFILEUNIT;
	  break;

	default:
	  goto bad;		/* :::::::::::::::::::: */
	}
      if (ffestp_file.write.write_spec[ffestb_local_.write.ix]
	  .kw_or_val_present)
	break;			/* Can't specify a keyword twice! */
      ffestp_file.write.write_spec[ffestb_local_.write.ix]
	.kw_or_val_present = TRUE;
      ffestp_file.write.write_spec[ffestb_local_.write.ix]
	.kw_present = TRUE;
      ffestp_file.write.write_spec[ffestb_local_.write.ix]
	.value_present = FALSE;
      ffestp_file.write.write_spec[ffestb_local_.write.ix].value_is_label
	= ffestb_local_.write.label;
      ffestp_file.write.write_spec[ffestb_local_.write.ix].kw
	= ffelex_token_use (t);
      return (ffelexHandler) ffestb_R9108_;

    default:
      break;
    }

bad:				/* :::::::::::::::::::: */
  ffestb_subr_kill_write_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "WRITE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R9108_ -- "WRITE" OPEN_PAREN [external-file-unit COMMA [format
		   COMMA]] NAME

   return ffestb_R9108_;  // to lexer

   Make sure EQUALS here, send next token to expression handler.  */

static ffelexHandler
ffestb_R9108_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEQUALS:
      ffesta_confirmed ();
      if (ffestb_local_.write.label)
	return (ffelexHandler) ffestb_R91010_;
      if (ffestb_local_.write.left)
	return (ffelexHandler) ffeexpr_lhs (ffesta_output_pool,
					    ffestb_local_.write.context,
					    (ffeexprCallback) ffestb_R9109_);
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
					  ffestb_local_.write.context,
					  (ffeexprCallback) ffestb_R9109_);

    default:
      break;
    }

  ffestb_subr_kill_write_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "WRITE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R9109_ -- "WRITE" OPEN_PAREN ... NAME EQUALS expr

   (ffestb_R9109_)  // to expression handler

   Handle COMMA or CLOSE_PAREN here.  */

static ffelexHandler
ffestb_R9109_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
    case FFELEX_typeCLOSE_PAREN:
      if (expr == NULL)
	{
	  if (ffestb_local_.write.context == FFEEXPR_contextFILEFORMAT)
	    ffestp_file.write.write_spec[ffestb_local_.write.ix]
	      .value_is_label = TRUE;
	  else
	    break;
	}
      ffestp_file.write.write_spec[ffestb_local_.write.ix].value_present
	= TRUE;
      ffestp_file.write.write_spec[ffestb_local_.write.ix].value
	= ffelex_token_use (ft);
      ffestp_file.write.write_spec[ffestb_local_.write.ix].u.expr = expr;
      if (ffelex_token_type (t) == FFELEX_typeCOMMA)
	return (ffelexHandler) ffestb_R9107_;
      return (ffelexHandler) ffestb_R91012_;

    default:
      break;
    }

  ffestb_subr_kill_write_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "WRITE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R91010_ -- "WRITE" OPEN_PAREN ... NAME EQUALS

   return ffestb_R91010_;  // to lexer

   Handle NUMBER for label here.  */

static ffelexHandler
ffestb_R91010_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNUMBER:
      ffestp_file.write.write_spec[ffestb_local_.write.ix].value_present
	= TRUE;
      ffestp_file.write.write_spec[ffestb_local_.write.ix].value
	= ffelex_token_use (t);
      return (ffelexHandler) ffestb_R91011_;

    default:
      break;
    }

  ffestb_subr_kill_write_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "WRITE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R91011_ -- "WRITE" OPEN_PAREN ... NAME EQUALS NUMBER

   return ffestb_R91011_;  // to lexer

   Handle COMMA or CLOSE_PAREN here.  */

static ffelexHandler
ffestb_R91011_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      return (ffelexHandler) ffestb_R9107_;

    case FFELEX_typeCLOSE_PAREN:
      return (ffelexHandler) ffestb_R91012_;

    default:
      break;
    }

  ffestb_subr_kill_write_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "WRITE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R91012_ -- "WRITE" OPEN_PAREN ... CLOSE_PAREN

   return ffestb_R91012_;  // to lexer

   Handle EOS or SEMICOLON here.  */

static ffelexHandler
ffestb_R91012_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	{
	  ffestc_R910_start ();
	  ffestc_R910_finish ();
	}
      ffestb_subr_kill_write_ ();
      return (ffelexHandler) ffesta_zero (t);

    default:
      ffesta_confirmed ();
      /* Fall through. */
    case FFELEX_typeOPEN_PAREN:	/* Could still be assignment!! */

      /* EXTENSION: Allow an optional preceding COMMA here if not pedantic.
	 (f2c provides this extension, as do other compilers, supposedly.) */

      if (!ffe_is_pedantic () && (ffelex_token_type (t) == FFELEX_typeCOMMA))
	return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		ffestc_context_iolist (), (ffeexprCallback) ffestb_R91013_);

      return (ffelexHandler) (*((ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
	       ffestc_context_iolist (), (ffeexprCallback) ffestb_R91013_)))
	(t);

    case FFELEX_typeEQUALS:
    case FFELEX_typePOINTS:
      break;
    }

  ffestb_subr_kill_write_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "WRITE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R91013_ -- "WRITE(...)" expr

   (ffestb_R91013_)  // to expression handler

   Handle COMMA or EOS/SEMICOLON here.	*/

static ffelexHandler
ffestb_R91013_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      if (expr == NULL)
	break;

      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	ffestc_R910_start ();
      ffestb_subr_kill_write_ ();

      if (!ffesta_is_inhibited ())
	ffestc_R910_item (expr, ft);
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		ffestc_context_iolist (), (ffeexprCallback) ffestb_R91014_);

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (expr == NULL)
	break;

      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	ffestc_R910_start ();
      ffestb_subr_kill_write_ ();

      if (!ffesta_is_inhibited ())
	{
	  ffestc_R910_item (expr, ft);
	  ffestc_R910_finish ();
	}
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  ffestb_subr_kill_write_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "WRITE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R91014_ -- "WRITE(...)" expr COMMA expr

   (ffestb_R91014_)  // to expression handler

   Handle COMMA or EOS/SEMICOLON here.	*/

static ffelexHandler
ffestb_R91014_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      if (expr == NULL)
	break;
      if (!ffesta_is_inhibited ())
	ffestc_R910_item (expr, ft);
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		ffestc_context_iolist (), (ffeexprCallback) ffestb_R91014_);

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (expr == NULL)
	break;
      if (!ffesta_is_inhibited ())
	{
	  ffestc_R910_item (expr, ft);
	  ffestc_R910_finish ();
	}
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_R910_finish ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "WRITE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R911 -- Parse the PRINT statement

   return ffestb_R911;	// to lexer

   Make sure the statement has a valid form for the PRINT
   statement.  If it does, implement the statement.  */

ffelexHandler
ffestb_R911 (ffelexToken t)
{
  ffelexHandler next;
  ffestpPrintIx ix;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstPRINT)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeEQUALS:
	case FFELEX_typePOINTS:
	case FFELEX_typeCOLON:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeNAME:
	case FFELEX_typeNUMBER:
	  ffesta_confirmed ();
	  break;

	default:
	  break;
	}

      for (ix = 0; ix < FFESTP_printix; ++ix)
	ffestp_file.print.print_spec[ix].kw_or_val_present = FALSE;
      return (ffelexHandler) (*((ffelexHandler)
				ffeexpr_rhs (ffesta_output_pool,
	    FFEEXPR_contextFILEFORMATNML, (ffeexprCallback) ffestb_R9111_)))
	(t);

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstPRINT)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	case FFELEX_typeCOMMA:
	  ffesta_confirmed ();
	  if (ffelex_token_length (ffesta_tokens[0]) != FFESTR_firstlPRINT)
	    break;
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeEQUALS:
	case FFELEX_typePOINTS:
	case FFELEX_typeCOLON:
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  break;
	}
      for (ix = 0; ix < FFESTP_printix; ++ix)
	ffestp_file.print.print_spec[ix].kw_or_val_present = FALSE;
      next = (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
	     FFEEXPR_contextFILEFORMATNML, (ffeexprCallback) ffestb_R9111_);
      next = (ffelexHandler) ffelex_splice_tokens (next, ffesta_tokens[0],
						   FFESTR_firstlPRINT);
      if (next == NULL)
	return (ffelexHandler) ffelex_swallow_tokens (t,
					       (ffelexHandler) ffesta_zero);
      return (ffelexHandler) (*next) (t);

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "PRINT", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "PRINT", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */
}

/* ffestb_R9111_ -- "PRINT" expr

   (ffestb_R9111_)  // to expression handler

   Make sure the next token is a COMMA or EOS/SEMICOLON.  */

static ffelexHandler
ffestb_R9111_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
    case FFELEX_typeCOMMA:
      ffesta_confirmed ();
      ffestp_file.print.print_spec[FFESTP_printixFORMAT].kw_or_val_present
	= TRUE;
      ffestp_file.print.print_spec[FFESTP_printixFORMAT].kw_present = FALSE;
      ffestp_file.print.print_spec[FFESTP_printixFORMAT].value_present = TRUE;
      ffestp_file.print.print_spec[FFESTP_printixFORMAT].value_is_label
	= (expr == NULL);
      ffestp_file.print.print_spec[FFESTP_printixFORMAT].value
	= ffelex_token_use (ft);
      ffestp_file.print.print_spec[FFESTP_printixFORMAT].u.expr = expr;
      if (!ffesta_is_inhibited ())
	ffestc_R911_start ();
      ffestb_subr_kill_print_ ();
      if (ffelex_token_type (t) == FFELEX_typeCOMMA)
	return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		    FFEEXPR_contextIOLIST, (ffeexprCallback) ffestb_R9112_);
      if (!ffesta_is_inhibited ())
	ffestc_R911_finish ();
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  ffestb_subr_kill_print_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "PRINT", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R9112_ -- "PRINT" expr COMMA expr

   (ffestb_R9112_)  // to expression handler

   Handle COMMA or EOS/SEMICOLON here.	*/

static ffelexHandler
ffestb_R9112_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      if (expr == NULL)
	break;
      if (!ffesta_is_inhibited ())
	ffestc_R911_item (expr, ft);
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		    FFEEXPR_contextIOLIST, (ffeexprCallback) ffestb_R9112_);

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (expr == NULL)
	break;
      if (!ffesta_is_inhibited ())
	{
	  ffestc_R911_item (expr, ft);
	  ffestc_R911_finish ();
	}
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_R911_finish ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "PRINT", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R923 -- Parse an INQUIRE statement

   return ffestb_R923;	// to lexer

   Make sure the statement has a valid form for an INQUIRE statement.
   If it does, implement the statement.	 */

ffelexHandler
ffestb_R923 (ffelexToken t)
{
  ffestpInquireIx ix;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstINQUIRE)
	goto bad_0;		/* :::::::::::::::::::: */
      break;

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstINQUIRE)
	goto bad_0;		/* :::::::::::::::::::: */
      if (ffelex_token_length (ffesta_tokens[0]) != FFESTR_firstlINQUIRE)
	goto bad_0;		/* :::::::::::::::::::: */
      break;

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeOPEN_PAREN:
      break;

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
    case FFELEX_typeCOMMA:
    case FFELEX_typeCOLONCOLON:
      ffesta_confirmed ();	/* Error, but clearly intended. */
      goto bad_1;		/* :::::::::::::::::::: */

    default:
      goto bad_1;		/* :::::::::::::::::::: */
    }

  for (ix = 0; ix < FFESTP_inquireix; ++ix)
    ffestp_file.inquire.inquire_spec[ix].kw_or_val_present = FALSE;

  ffestb_local_.inquire.may_be_iolength = TRUE;
  return (ffelexHandler) ffestb_R9231_;

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "INQUIRE", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "INQUIRE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */
}

/* ffestb_R9231_ -- "INQUIRE" OPEN_PAREN

   return ffestb_R9231_;  // to lexer

   Handle expr construct (not NAME=expr construct) here.  */

static ffelexHandler
ffestb_R9231_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_R9232_;

    default:
      ffestb_local_.inquire.may_be_iolength = FALSE;
      return (ffelexHandler) (*((ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		  FFEEXPR_contextFILENUM, (ffeexprCallback) ffestb_R9233_)))
	(t);
    }
}

/* ffestb_R9232_ -- "INQUIRE" OPEN_PAREN NAME

   return ffestb_R9232_;  // to lexer

   If EQUALS here, go to states that handle it.	 Else, send NAME and this
   token thru expression handler.  */

static ffelexHandler
ffestb_R9232_ (ffelexToken t)
{
  ffelexHandler next;
  ffelexToken nt;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEQUALS:
      nt = ffesta_tokens[1];
      next = (ffelexHandler) ffestb_R9234_ (nt);
      ffelex_token_kill (nt);
      return (ffelexHandler) (*next) (t);

    default:
      ffestb_local_.inquire.may_be_iolength = FALSE;
      next = (ffelexHandler) (*((ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		  FFEEXPR_contextFILENUM, (ffeexprCallback) ffestb_R9233_)))
	(ffesta_tokens[1]);
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) (*next) (t);
    }
}

/* ffestb_R9233_ -- "INQUIRE" OPEN_PAREN expr

   (ffestb_R9233_)  // to expression handler

   Handle COMMA or CLOSE_PAREN here.  */

static ffelexHandler
ffestb_R9233_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
    case FFELEX_typeCLOSE_PAREN:
      if (expr == NULL)
	break;
      ffestp_file.inquire.inquire_spec[FFESTP_inquireixUNIT].kw_or_val_present
	= TRUE;
      ffestp_file.inquire.inquire_spec[FFESTP_inquireixUNIT].kw_present = FALSE;
      ffestp_file.inquire.inquire_spec[FFESTP_inquireixUNIT].value_present = TRUE;
      ffestp_file.inquire.inquire_spec[FFESTP_inquireixUNIT].value_is_label
	= FALSE;
      ffestp_file.inquire.inquire_spec[FFESTP_inquireixUNIT].value
	= ffelex_token_use (ft);
      ffestp_file.inquire.inquire_spec[FFESTP_inquireixUNIT].u.expr = expr;
      if (ffelex_token_type (t) == FFELEX_typeCOMMA)
	return (ffelexHandler) ffestb_R9234_;
      return (ffelexHandler) ffestb_R9239_;

    default:
      break;
    }

  ffestb_subr_kill_inquire_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "INQUIRE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R9234_ -- "INQUIRE" OPEN_PAREN [external-file-unit COMMA]

   return ffestb_R9234_;  // to lexer

   Handle expr construct (not NAME=expr construct) here.  */

static ffelexHandler
ffestb_R9234_ (ffelexToken t)
{
  ffestrInquire kw;

  ffestb_local_.inquire.label = FALSE;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      kw = ffestr_inquire (t);
      if (kw != FFESTR_inquireIOLENGTH)
	ffestb_local_.inquire.may_be_iolength = FALSE;
      switch (kw)
	{
	case FFESTR_inquireACCESS:
	  ffestb_local_.inquire.ix = FFESTP_inquireixACCESS;
	  ffestb_local_.inquire.left = TRUE;
	  ffestb_local_.inquire.context = FFEEXPR_contextFILEDFCHAR;
	  break;

	case FFESTR_inquireACTION:
	  ffestb_local_.inquire.ix = FFESTP_inquireixACTION;
	  ffestb_local_.inquire.left = TRUE;
	  ffestb_local_.inquire.context = FFEEXPR_contextFILEDFCHAR;
	  break;

	case FFESTR_inquireBLANK:
	  ffestb_local_.inquire.ix = FFESTP_inquireixBLANK;
	  ffestb_local_.inquire.left = TRUE;
	  ffestb_local_.inquire.context = FFEEXPR_contextFILEDFCHAR;
	  break;

	case FFESTR_inquireCARRIAGECONTROL:
	  ffestb_local_.inquire.ix = FFESTP_inquireixCARRIAGECONTROL;
	  ffestb_local_.inquire.left = TRUE;
	  ffestb_local_.inquire.context = FFEEXPR_contextFILECHAR;
	  break;

	case FFESTR_inquireDEFAULTFILE:
	  ffestb_local_.inquire.ix = FFESTP_inquireixDEFAULTFILE;
	  ffestb_local_.inquire.left = FALSE;
	  ffestb_local_.inquire.context = FFEEXPR_contextFILECHAR;
	  break;

	case FFESTR_inquireDELIM:
	  ffestb_local_.inquire.ix = FFESTP_inquireixDELIM;
	  ffestb_local_.inquire.left = TRUE;
	  ffestb_local_.inquire.context = FFEEXPR_contextFILEDFCHAR;
	  break;

	case FFESTR_inquireDIRECT:
	  ffestb_local_.inquire.ix = FFESTP_inquireixDIRECT;
	  ffestb_local_.inquire.left = TRUE;
	  ffestb_local_.inquire.context = FFEEXPR_contextFILEDFCHAR;
	  break;

	case FFESTR_inquireERR:
	  ffestb_local_.inquire.ix = FFESTP_inquireixERR;
	  ffestb_local_.inquire.label = TRUE;
	  break;

	case FFESTR_inquireEXIST:
	  ffestb_local_.inquire.ix = FFESTP_inquireixEXIST;
	  ffestb_local_.inquire.left = TRUE;
	  ffestb_local_.inquire.context = FFEEXPR_contextFILELOG;
	  break;

	case FFESTR_inquireFILE:
	  ffestb_local_.inquire.ix = FFESTP_inquireixFILE;
	  ffestb_local_.inquire.left = FALSE;
	  ffestb_local_.inquire.context = FFEEXPR_contextFILECHAR;
	  break;

	case FFESTR_inquireFORM:
	  ffestb_local_.inquire.ix = FFESTP_inquireixFORM;
	  ffestb_local_.inquire.left = TRUE;
	  ffestb_local_.inquire.context = FFEEXPR_contextFILEDFCHAR;
	  break;

	case FFESTR_inquireFORMATTED:
	  ffestb_local_.inquire.ix = FFESTP_inquireixFORMATTED;
	  ffestb_local_.inquire.left = TRUE;
	  ffestb_local_.inquire.context = FFEEXPR_contextFILEDFCHAR;
	  break;

	case FFESTR_inquireIOLENGTH:
	  if (!ffestb_local_.inquire.may_be_iolength)
	    goto bad;		/* :::::::::::::::::::: */
	  ffestb_local_.inquire.ix = FFESTP_inquireixIOLENGTH;
	  ffestb_local_.inquire.left = TRUE;
	  ffestb_local_.inquire.context = FFEEXPR_contextFILEINT;
	  break;

	case FFESTR_inquireIOSTAT:
	  ffestb_local_.inquire.ix = FFESTP_inquireixIOSTAT;
	  ffestb_local_.inquire.left = TRUE;
	  ffestb_local_.inquire.context = FFEEXPR_contextFILEINT;
	  break;

	case FFESTR_inquireKEYED:
	  ffestb_local_.inquire.ix = FFESTP_inquireixKEYED;
	  ffestb_local_.inquire.left = TRUE;
	  ffestb_local_.inquire.context = FFEEXPR_contextFILECHAR;
	  break;

	case FFESTR_inquireNAME:
	  ffestb_local_.inquire.ix = FFESTP_inquireixNAME;
	  ffestb_local_.inquire.left = TRUE;
	  ffestb_local_.inquire.context = FFEEXPR_contextFILECHAR;
	  break;

	case FFESTR_inquireNAMED:
	  ffestb_local_.inquire.ix = FFESTP_inquireixNAMED;
	  ffestb_local_.inquire.left = TRUE;
	  ffestb_local_.inquire.context = FFEEXPR_contextFILELOG;
	  break;

	case FFESTR_inquireNEXTREC:
	  ffestb_local_.inquire.ix = FFESTP_inquireixNEXTREC;
	  ffestb_local_.inquire.left = TRUE;
	  ffestb_local_.inquire.context = FFEEXPR_contextFILEDFINT;
	  break;

	case FFESTR_inquireNUMBER:
	  ffestb_local_.inquire.ix = FFESTP_inquireixNUMBER;
	  ffestb_local_.inquire.left = TRUE;
	  ffestb_local_.inquire.context = FFEEXPR_contextFILEINT;
	  break;

	case FFESTR_inquireOPENED:
	  ffestb_local_.inquire.ix = FFESTP_inquireixOPENED;
	  ffestb_local_.inquire.left = TRUE;
	  ffestb_local_.inquire.context = FFEEXPR_contextFILELOG;
	  break;

	case FFESTR_inquireORGANIZATION:
	  ffestb_local_.inquire.ix = FFESTP_inquireixORGANIZATION;
	  ffestb_local_.inquire.left = TRUE;
	  ffestb_local_.inquire.context = FFEEXPR_contextFILECHAR;
	  break;

	case FFESTR_inquirePAD:
	  ffestb_local_.inquire.ix = FFESTP_inquireixPAD;
	  ffestb_local_.inquire.left = TRUE;
	  ffestb_local_.inquire.context = FFEEXPR_contextFILEDFCHAR;
	  break;

	case FFESTR_inquirePOSITION:
	  ffestb_local_.inquire.ix = FFESTP_inquireixPOSITION;
	  ffestb_local_.inquire.left = TRUE;
	  ffestb_local_.inquire.context = FFEEXPR_contextFILEDFCHAR;
	  break;

	case FFESTR_inquireREAD:
	  ffestb_local_.inquire.ix = FFESTP_inquireixREAD;
	  ffestb_local_.inquire.left = TRUE;
	  ffestb_local_.inquire.context = FFEEXPR_contextFILEDFCHAR;
	  break;

	case FFESTR_inquireREADWRITE:
	  ffestb_local_.inquire.ix = FFESTP_inquireixREADWRITE;
	  ffestb_local_.inquire.left = TRUE;
	  ffestb_local_.inquire.context = FFEEXPR_contextFILEDFCHAR;
	  break;

	case FFESTR_inquireRECL:
	  ffestb_local_.inquire.ix = FFESTP_inquireixRECL;
	  ffestb_local_.inquire.left = TRUE;
	  ffestb_local_.inquire.context = FFEEXPR_contextFILEINT;
	  break;

	case FFESTR_inquireRECORDTYPE:
	  ffestb_local_.inquire.ix = FFESTP_inquireixRECORDTYPE;
	  ffestb_local_.inquire.left = TRUE;
	  ffestb_local_.inquire.context = FFEEXPR_contextFILECHAR;
	  break;

	case FFESTR_inquireSEQUENTIAL:
	  ffestb_local_.inquire.ix = FFESTP_inquireixSEQUENTIAL;
	  ffestb_local_.inquire.left = TRUE;
	  ffestb_local_.inquire.context = FFEEXPR_contextFILEDFCHAR;
	  break;

	case FFESTR_inquireUNFORMATTED:
	  ffestb_local_.inquire.ix = FFESTP_inquireixUNFORMATTED;
	  ffestb_local_.inquire.left = TRUE;
	  ffestb_local_.inquire.context = FFEEXPR_contextFILEDFCHAR;
	  break;

	case FFESTR_inquireUNIT:
	  ffestb_local_.inquire.ix = FFESTP_inquireixUNIT;
	  ffestb_local_.inquire.left = FALSE;
	  ffestb_local_.inquire.context = FFEEXPR_contextFILENUM;
	  break;

	default:
	  goto bad;		/* :::::::::::::::::::: */
	}
      if (ffestp_file.inquire.inquire_spec[ffestb_local_.inquire.ix]
	  .kw_or_val_present)
	break;			/* Can't specify a keyword twice! */
      ffestp_file.inquire.inquire_spec[ffestb_local_.inquire.ix]
	.kw_or_val_present = TRUE;
      ffestp_file.inquire.inquire_spec[ffestb_local_.inquire.ix]
	.kw_present = TRUE;
      ffestp_file.inquire.inquire_spec[ffestb_local_.inquire.ix]
	.value_present = FALSE;
      ffestp_file.inquire.inquire_spec[ffestb_local_.inquire.ix].value_is_label
	= ffestb_local_.inquire.label;
      ffestp_file.inquire.inquire_spec[ffestb_local_.inquire.ix].kw
	= ffelex_token_use (t);
      return (ffelexHandler) ffestb_R9235_;

    default:
      break;
    }

bad:				/* :::::::::::::::::::: */
  ffestb_subr_kill_inquire_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "INQUIRE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R9235_ -- "INQUIRE" OPEN_PAREN [external-file-unit COMMA] NAME

   return ffestb_R9235_;  // to lexer

   Make sure EQUALS here, send next token to expression handler.  */

static ffelexHandler
ffestb_R9235_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEQUALS:
      ffesta_confirmed ();
      if (ffestb_local_.inquire.label)
	return (ffelexHandler) ffestb_R9237_;
      if (ffestb_local_.inquire.left)
	return (ffelexHandler) ffeexpr_lhs (ffesta_output_pool,
					    ffestb_local_.inquire.context,
					    (ffeexprCallback) ffestb_R9236_);
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
					  ffestb_local_.inquire.context,
					  (ffeexprCallback) ffestb_R9236_);

    default:
      break;
    }

  ffestb_subr_kill_inquire_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "INQUIRE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R9236_ -- "INQUIRE" OPEN_PAREN ... NAME EQUALS expr

   (ffestb_R9236_)  // to expression handler

   Handle COMMA or CLOSE_PAREN here.  */

static ffelexHandler
ffestb_R9236_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      if (ffestb_local_.inquire.ix == FFESTP_inquireixIOLENGTH)
	break;			/* IOLENGTH=expr must be followed by
				   CLOSE_PAREN. */
      /* Fall through. */
    case FFELEX_typeCLOSE_PAREN:
      if (expr == NULL)
	break;
      ffestp_file.inquire.inquire_spec[ffestb_local_.inquire.ix].value_present
	= TRUE;
      ffestp_file.inquire.inquire_spec[ffestb_local_.inquire.ix].value
	= ffelex_token_use (ft);
      ffestp_file.inquire.inquire_spec[ffestb_local_.inquire.ix].u.expr = expr;
      if (ffelex_token_type (t) == FFELEX_typeCOMMA)
	return (ffelexHandler) ffestb_R9234_;
      if (ffestb_local_.inquire.ix == FFESTP_inquireixIOLENGTH)
	return (ffelexHandler) ffestb_R92310_;
      return (ffelexHandler) ffestb_R9239_;

    default:
      break;
    }

  ffestb_subr_kill_inquire_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "INQUIRE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R9237_ -- "INQUIRE" OPEN_PAREN ... NAME EQUALS

   return ffestb_R9237_;  // to lexer

   Handle NUMBER for label here.  */

static ffelexHandler
ffestb_R9237_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNUMBER:
      ffestp_file.inquire.inquire_spec[ffestb_local_.inquire.ix].value_present
	= TRUE;
      ffestp_file.inquire.inquire_spec[ffestb_local_.inquire.ix].value
	= ffelex_token_use (t);
      return (ffelexHandler) ffestb_R9238_;

    default:
      break;
    }

  ffestb_subr_kill_inquire_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "INQUIRE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R9238_ -- "INQUIRE" OPEN_PAREN ... NAME EQUALS NUMBER

   return ffestb_R9238_;  // to lexer

   Handle COMMA or CLOSE_PAREN here.  */

static ffelexHandler
ffestb_R9238_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      return (ffelexHandler) ffestb_R9234_;

    case FFELEX_typeCLOSE_PAREN:
      return (ffelexHandler) ffestb_R9239_;

    default:
      break;
    }

  ffestb_subr_kill_inquire_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "INQUIRE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R9239_ -- "INQUIRE" OPEN_PAREN ... CLOSE_PAREN

   return ffestb_R9239_;  // to lexer

   Handle EOS or SEMICOLON here.  */

static ffelexHandler
ffestb_R9239_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	ffestc_R923A ();
      ffestb_subr_kill_inquire_ ();
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  ffestb_subr_kill_inquire_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "INQUIRE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R92310_ -- "INQUIRE(IOLENGTH=expr)"

   return ffestb_R92310_;  // to lexer

   Make sure EOS or SEMICOLON not here; begin R923B processing and expect
   output IO list.  */

static ffelexHandler
ffestb_R92310_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      break;

    default:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	ffestc_R923B_start ();
      ffestb_subr_kill_inquire_ ();
      return (ffelexHandler) (*((ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		  FFEEXPR_contextIOLIST, (ffeexprCallback) ffestb_R92311_)))
	(t);
    }

  ffestb_subr_kill_inquire_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "INQUIRE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R92311_ -- "INQUIRE(IOLENGTH=expr)" expr

   (ffestb_R92311_)  // to expression handler

   Handle COMMA or EOS/SEMICOLON here.	*/

static ffelexHandler
ffestb_R92311_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      if (expr == NULL)
	break;
      if (!ffesta_is_inhibited ())
	ffestc_R923B_item (expr, ft);
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		   FFEEXPR_contextIOLIST, (ffeexprCallback) ffestb_R92311_);

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (expr == NULL)
	break;
      if (!ffesta_is_inhibited ())
	{
	  ffestc_R923B_item (expr, ft);
	  ffestc_R923B_finish ();
	}
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_R923B_finish ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "INQUIRE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V018 -- Parse the REWRITE statement

   return ffestb_V018;	// to lexer

   Make sure the statement has a valid form for the REWRITE
   statement.  If it does, implement the statement.  */

#if FFESTR_VXT
ffelexHandler
ffestb_V018 (ffelexToken t)
{
  ffestpRewriteIx ix;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstREWRITE)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	case FFELEX_typeNAME:
	case FFELEX_typeNUMBER:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeOPEN_PAREN:
	  for (ix = 0; ix < FFESTP_rewriteix; ++ix)
	    ffestp_file.rewrite.rewrite_spec[ix].kw_or_val_present = FALSE;
	  return (ffelexHandler) ffestb_V0181_;
	}

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstREWRITE)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeOPEN_PAREN:
	  if (ffelex_token_length (ffesta_tokens[0]) != FFESTR_firstlREWRITE)
	    goto bad_0;		/* :::::::::::::::::::: */

	  for (ix = 0; ix < FFESTP_rewriteix; ++ix)
	    ffestp_file.rewrite.rewrite_spec[ix].kw_or_val_present = FALSE;
	  return (ffelexHandler) ffestb_V0181_;
	}

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "REWRITE", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "REWRITE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */
}

/* ffestb_V0181_ -- "REWRITE" OPEN_PAREN

   return ffestb_V0181_;  // to lexer

   Handle expr construct (not NAME=expr construct) here.  */

static ffelexHandler
ffestb_V0181_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_V0182_;

    default:
      return (ffelexHandler) (*((ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		  FFEEXPR_contextFILENUM, (ffeexprCallback) ffestb_V0183_)))
	(t);
    }
}

/* ffestb_V0182_ -- "REWRITE" OPEN_PAREN NAME

   return ffestb_V0182_;  // to lexer

   If EQUALS here, go to states that handle it.	 Else, send NAME and this
   token thru expression handler.  */

static ffelexHandler
ffestb_V0182_ (ffelexToken t)
{
  ffelexHandler next;
  ffelexToken nt;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEQUALS:
      nt = ffesta_tokens[1];
      next = (ffelexHandler) ffestb_V0187_ (nt);
      ffelex_token_kill (nt);
      return (ffelexHandler) (*next) (t);

    default:
      nt = ffesta_tokens[1];
      next = (ffelexHandler) (*((ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		  FFEEXPR_contextFILENUM, (ffeexprCallback) ffestb_V0183_)))
	(nt);
      ffelex_token_kill (nt);
      return (ffelexHandler) (*next) (t);
    }
}

/* ffestb_V0183_ -- "REWRITE" OPEN_PAREN expr [CLOSE_PAREN]

   (ffestb_V0183_)  // to expression handler

   Handle COMMA or EOS/SEMICOLON here.	*/

static ffelexHandler
ffestb_V0183_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
    case FFELEX_typeCLOSE_PAREN:
      if (expr == NULL)
	break;
      ffestp_file.rewrite.rewrite_spec[FFESTP_rewriteixUNIT].kw_or_val_present
	= TRUE;
      ffestp_file.rewrite.rewrite_spec[FFESTP_rewriteixUNIT].kw_present = FALSE;
      ffestp_file.rewrite.rewrite_spec[FFESTP_rewriteixUNIT].value_present = TRUE;
      ffestp_file.rewrite.rewrite_spec[FFESTP_rewriteixUNIT].value_is_label
	= FALSE;
      ffestp_file.rewrite.rewrite_spec[FFESTP_rewriteixUNIT].value
	= ffelex_token_use (ft);
      ffestp_file.rewrite.rewrite_spec[FFESTP_rewriteixUNIT].u.expr = expr;
      if (ffelex_token_type (t) == FFELEX_typeCOMMA)
	return (ffelexHandler) ffestb_V0184_;
      return (ffelexHandler) ffestb_V01812_;

    default:
      break;
    }

  ffestb_subr_kill_rewrite_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "REWRITE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0184_ -- "REWRITE" OPEN_PAREN expr COMMA

   return ffestb_V0184_;  // to lexer

   Handle expr construct (not NAME=expr construct) here.  */

static ffelexHandler
ffestb_V0184_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_V0185_;

    default:
      return (ffelexHandler) (*((ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
	       FFEEXPR_contextFILEFORMAT, (ffeexprCallback) ffestb_V0186_)))
	(t);
    }
}

/* ffestb_V0185_ -- "REWRITE" OPEN_PAREN expr COMMA NAME

   return ffestb_V0185_;  // to lexer

   If EQUALS here, go to states that handle it.	 Else, send NAME and this
   token thru expression handler.  */

static ffelexHandler
ffestb_V0185_ (ffelexToken t)
{
  ffelexHandler next;
  ffelexToken nt;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEQUALS:
      nt = ffesta_tokens[1];
      next = (ffelexHandler) ffestb_V0187_ (nt);
      ffelex_token_kill (nt);
      return (ffelexHandler) (*next) (t);

    default:
      nt = ffesta_tokens[1];
      next = (ffelexHandler) (*((ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
	       FFEEXPR_contextFILEFORMAT, (ffeexprCallback) ffestb_V0186_)))
	(nt);
      ffelex_token_kill (nt);
      return (ffelexHandler) (*next) (t);
    }
}

/* ffestb_V0186_ -- "REWRITE" OPEN_PAREN expr COMMA expr

   (ffestb_V0186_)  // to expression handler

   Handle COMMA or CLOSE_PAREN here.  */

static ffelexHandler
ffestb_V0186_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
    case FFELEX_typeCLOSE_PAREN:
      ffestp_file.rewrite.rewrite_spec[FFESTP_rewriteixFMT].kw_or_val_present
	= TRUE;
      ffestp_file.rewrite.rewrite_spec[FFESTP_rewriteixFMT].kw_present = FALSE;
      ffestp_file.rewrite.rewrite_spec[FFESTP_rewriteixFMT].value_present = TRUE;
      ffestp_file.rewrite.rewrite_spec[FFESTP_rewriteixFMT].value_is_label
	= (expr == NULL);
      ffestp_file.rewrite.rewrite_spec[FFESTP_rewriteixFMT].value
	= ffelex_token_use (ft);
      ffestp_file.rewrite.rewrite_spec[FFESTP_rewriteixFMT].u.expr = expr;
      if (ffelex_token_type (t) == FFELEX_typeCOMMA)
	return (ffelexHandler) ffestb_V0187_;
      return (ffelexHandler) ffestb_V01812_;

    default:
      break;
    }

  ffestb_subr_kill_rewrite_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "REWRITE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0187_ -- "REWRITE" OPEN_PAREN [external-file-unit COMMA [format
		   COMMA]]

   return ffestb_V0187_;  // to lexer

   Handle expr construct (not NAME=expr construct) here.  */

static ffelexHandler
ffestb_V0187_ (ffelexToken t)
{
  ffestrGenio kw;

  ffestb_local_.rewrite.label = FALSE;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      kw = ffestr_genio (t);
      switch (kw)
	{
	case FFESTR_genioERR:
	  ffestb_local_.rewrite.ix = FFESTP_rewriteixERR;
	  ffestb_local_.rewrite.label = TRUE;
	  break;

	case FFESTR_genioFMT:
	  ffestb_local_.rewrite.ix = FFESTP_rewriteixFMT;
	  ffestb_local_.rewrite.left = FALSE;
	  ffestb_local_.rewrite.context = FFEEXPR_contextFILEFORMAT;
	  break;

	case FFESTR_genioIOSTAT:
	  ffestb_local_.rewrite.ix = FFESTP_rewriteixIOSTAT;
	  ffestb_local_.rewrite.left = TRUE;
	  ffestb_local_.rewrite.context = FFEEXPR_contextFILEINT;
	  break;

	case FFESTR_genioUNIT:
	  ffestb_local_.rewrite.ix = FFESTP_rewriteixUNIT;
	  ffestb_local_.rewrite.left = FALSE;
	  ffestb_local_.rewrite.context = FFEEXPR_contextFILENUM;
	  break;

	default:
	  goto bad;		/* :::::::::::::::::::: */
	}
      if (ffestp_file.rewrite.rewrite_spec[ffestb_local_.rewrite.ix]
	  .kw_or_val_present)
	break;			/* Can't specify a keyword twice! */
      ffestp_file.rewrite.rewrite_spec[ffestb_local_.rewrite.ix]
	.kw_or_val_present = TRUE;
      ffestp_file.rewrite.rewrite_spec[ffestb_local_.rewrite.ix]
	.kw_present = TRUE;
      ffestp_file.rewrite.rewrite_spec[ffestb_local_.rewrite.ix]
	.value_present = FALSE;
      ffestp_file.rewrite.rewrite_spec[ffestb_local_.rewrite.ix].value_is_label
	= ffestb_local_.rewrite.label;
      ffestp_file.rewrite.rewrite_spec[ffestb_local_.rewrite.ix].kw
	= ffelex_token_use (t);
      return (ffelexHandler) ffestb_V0188_;

    default:
      break;
    }

bad:				/* :::::::::::::::::::: */
  ffestb_subr_kill_rewrite_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "REWRITE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0188_ -- "REWRITE" OPEN_PAREN [external-file-unit COMMA [format
		   COMMA]] NAME

   return ffestb_V0188_;  // to lexer

   Make sure EQUALS here, send next token to expression handler.  */

static ffelexHandler
ffestb_V0188_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEQUALS:
      ffesta_confirmed ();
      if (ffestb_local_.rewrite.label)
	return (ffelexHandler) ffestb_V01810_;
      if (ffestb_local_.rewrite.left)
	return (ffelexHandler) ffeexpr_lhs (ffesta_output_pool,
					    ffestb_local_.rewrite.context,
					    (ffeexprCallback) ffestb_V0189_);
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
					  ffestb_local_.rewrite.context,
					  (ffeexprCallback) ffestb_V0189_);

    default:
      break;
    }

  ffestb_subr_kill_rewrite_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "REWRITE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0189_ -- "REWRITE" OPEN_PAREN ... NAME EQUALS expr

   (ffestb_V0189_)  // to expression handler

   Handle COMMA or CLOSE_PAREN here.  */

static ffelexHandler
ffestb_V0189_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
    case FFELEX_typeCLOSE_PAREN:
      if (expr == NULL)
	if (ffestb_local_.rewrite.context == FFEEXPR_contextFILEFORMAT)
	  ffestp_file.rewrite.rewrite_spec[ffestb_local_.rewrite.ix]
	    .value_is_label = TRUE;
	else
	  break;
      ffestp_file.rewrite.rewrite_spec[ffestb_local_.rewrite.ix].value_present
	= TRUE;
      ffestp_file.rewrite.rewrite_spec[ffestb_local_.rewrite.ix].value
	= ffelex_token_use (ft);
      ffestp_file.rewrite.rewrite_spec[ffestb_local_.rewrite.ix].u.expr = expr;
      if (ffelex_token_type (t) == FFELEX_typeCOMMA)
	return (ffelexHandler) ffestb_V0187_;
      return (ffelexHandler) ffestb_V01812_;

    default:
      break;
    }

  ffestb_subr_kill_rewrite_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "REWRITE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V01810_ -- "REWRITE" OPEN_PAREN ... NAME EQUALS

   return ffestb_V01810_;  // to lexer

   Handle NUMBER for label here.  */

static ffelexHandler
ffestb_V01810_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNUMBER:
      ffestp_file.rewrite.rewrite_spec[ffestb_local_.rewrite.ix].value_present
	= TRUE;
      ffestp_file.rewrite.rewrite_spec[ffestb_local_.rewrite.ix].value
	= ffelex_token_use (t);
      return (ffelexHandler) ffestb_V01811_;

    default:
      break;
    }

  ffestb_subr_kill_rewrite_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "REWRITE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V01811_ -- "REWRITE" OPEN_PAREN ... NAME EQUALS NUMBER

   return ffestb_V01811_;  // to lexer

   Handle COMMA or CLOSE_PAREN here.  */

static ffelexHandler
ffestb_V01811_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      return (ffelexHandler) ffestb_V0187_;

    case FFELEX_typeCLOSE_PAREN:
      return (ffelexHandler) ffestb_V01812_;

    default:
      break;
    }

  ffestb_subr_kill_rewrite_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "REWRITE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V01812_ -- "REWRITE" OPEN_PAREN ... CLOSE_PAREN

   return ffestb_V01812_;  // to lexer

   Handle EOS or SEMICOLON here.  */

static ffelexHandler
ffestb_V01812_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	{
	  ffestc_V018_start ();
	  ffestc_V018_finish ();
	}
      ffestb_subr_kill_rewrite_ ();
      return (ffelexHandler) ffesta_zero (t);

    case FFELEX_typeNAME:
    case FFELEX_typeOPEN_PAREN:
    case FFELEX_typeCOMMA:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	ffestc_V018_start ();
      ffestb_subr_kill_rewrite_ ();

      /* EXTENSION: Allow an optional preceding COMMA here if not pedantic.
	 (f2c provides this extension, as do other compilers, supposedly.) */

      if (!ffe_is_pedantic () && (ffelex_token_type (t) == FFELEX_typeCOMMA))
	return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		   FFEEXPR_contextIOLIST, (ffeexprCallback) ffestb_V01813_);

      return (ffelexHandler) (*((ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		  FFEEXPR_contextIOLIST, (ffeexprCallback) ffestb_V01813_)))
	(t);

    default:
      break;
    }

  ffestb_subr_kill_rewrite_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "REWRITE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V01813_ -- "REWRITE(...)" expr

   (ffestb_V01813_)  // to expression handler

   Handle COMMA or EOS/SEMICOLON here.	*/

static ffelexHandler
ffestb_V01813_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      if (expr == NULL)
	break;
      if (!ffesta_is_inhibited ())
	ffestc_V018_item (expr, ft);
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		   FFEEXPR_contextIOLIST, (ffeexprCallback) ffestb_V01813_);

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (expr == NULL)
	break;
      if (!ffesta_is_inhibited ())
	{
	  ffestc_V018_item (expr, ft);
	  ffestc_V018_finish ();
	}
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_V018_finish ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "REWRITE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V019 -- Parse the ACCEPT statement

   return ffestb_V019;	// to lexer

   Make sure the statement has a valid form for the ACCEPT
   statement.  If it does, implement the statement.  */

ffelexHandler
ffestb_V019 (ffelexToken t)
{
  ffelexHandler next;
  ffestpAcceptIx ix;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstACCEPT)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeEQUALS:
	case FFELEX_typePOINTS:
	case FFELEX_typeCOLON:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeNAME:
	case FFELEX_typeNUMBER:
	  ffesta_confirmed ();
	  break;

	default:
	  break;
	}

      for (ix = 0; ix < FFESTP_acceptix; ++ix)
	ffestp_file.accept.accept_spec[ix].kw_or_val_present = FALSE;
      return (ffelexHandler) (*((ffelexHandler)
				ffeexpr_rhs (ffesta_output_pool,
	    FFEEXPR_contextFILEFORMATNML, (ffeexprCallback) ffestb_V0191_)))
	(t);

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstACCEPT)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	case FFELEX_typeCOMMA:
	  ffesta_confirmed ();
	  if (ffelex_token_length (ffesta_tokens[0]) != FFESTR_firstlACCEPT)
	    break;
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeEQUALS:
	case FFELEX_typePOINTS:
	case FFELEX_typeCOLON:
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  break;
	}
      for (ix = 0; ix < FFESTP_acceptix; ++ix)
	ffestp_file.accept.accept_spec[ix].kw_or_val_present = FALSE;
      next = (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
	     FFEEXPR_contextFILEFORMATNML, (ffeexprCallback) ffestb_V0191_);
      next = (ffelexHandler) ffelex_splice_tokens (next, ffesta_tokens[0],
						   FFESTR_firstlACCEPT);
      if (next == NULL)
	return (ffelexHandler) ffelex_swallow_tokens (t,
					       (ffelexHandler) ffesta_zero);
      return (ffelexHandler) (*next) (t);

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "ACCEPT", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "ACCEPT", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */
}

/* ffestb_V0191_ -- "ACCEPT" expr

   (ffestb_V0191_)  // to expression handler

   Make sure the next token is a COMMA or EOS/SEMICOLON.  */

static ffelexHandler
ffestb_V0191_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
    case FFELEX_typeCOMMA:
      ffesta_confirmed ();
      ffestp_file.accept.accept_spec[FFESTP_acceptixFORMAT].kw_or_val_present
	= TRUE;
      ffestp_file.accept.accept_spec[FFESTP_acceptixFORMAT].kw_present = FALSE;
      ffestp_file.accept.accept_spec[FFESTP_acceptixFORMAT].value_present = TRUE;
      ffestp_file.accept.accept_spec[FFESTP_acceptixFORMAT].value_is_label
	= (expr == NULL);
      ffestp_file.accept.accept_spec[FFESTP_acceptixFORMAT].value
	= ffelex_token_use (ft);
      ffestp_file.accept.accept_spec[FFESTP_acceptixFORMAT].u.expr = expr;
      if (!ffesta_is_inhibited ())
	ffestc_V019_start ();
      ffestb_subr_kill_accept_ ();
      if (ffelex_token_type (t) == FFELEX_typeCOMMA)
	return (ffelexHandler) ffeexpr_lhs (ffesta_output_pool,
					    FFEEXPR_contextIOLIST,
					    (ffeexprCallback) ffestb_V0192_);
      if (!ffesta_is_inhibited ())
	ffestc_V019_finish ();
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  ffestb_subr_kill_accept_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "ACCEPT", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0192_ -- "ACCEPT" expr COMMA expr

   (ffestb_V0192_)  // to expression handler

   Handle COMMA or EOS/SEMICOLON here.	*/

static ffelexHandler
ffestb_V0192_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      if (expr == NULL)
	break;
      if (!ffesta_is_inhibited ())
	ffestc_V019_item (expr, ft);
      return (ffelexHandler) ffeexpr_lhs (ffesta_output_pool,
					  FFEEXPR_contextIOLIST,
					  (ffeexprCallback) ffestb_V0192_);

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (expr == NULL)
	break;
      if (!ffesta_is_inhibited ())
	{
	  ffestc_V019_item (expr, ft);
	  ffestc_V019_finish ();
	}
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_V019_finish ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "ACCEPT", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

#endif
/* ffestb_V020 -- Parse the TYPE statement

   return ffestb_V020;	// to lexer

   Make sure the statement has a valid form for the TYPE
   statement.  If it does, implement the statement.  */

ffelexHandler
ffestb_V020 (ffelexToken t)
{
  ffeTokenLength i;
  const char *p;
  ffelexHandler next;
  ffestpTypeIx ix;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstTYPE)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeCOLONCOLON:
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeEQUALS:
	case FFELEX_typePOINTS:
	case FFELEX_typeCOLON:
	case FFELEX_typeCOMMA:	/* Because "TYPE,PUBLIC::A" is ambiguous with
				   '90. */
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeNUMBER:
	  ffesta_confirmed ();
	  break;

	case FFELEX_typeNAME:	/* Because TYPE A is ambiguous with '90. */
	default:
	  break;
	}

      for (ix = 0; ix < FFESTP_typeix; ++ix)
	ffestp_file.type.type_spec[ix].kw_or_val_present = FALSE;
      return (ffelexHandler) (*((ffelexHandler)
				ffeexpr_rhs (ffesta_output_pool,
	    FFEEXPR_contextFILEFORMATNML, (ffeexprCallback) ffestb_V0201_)))
	(t);

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstTYPE)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	case FFELEX_typeCOMMA:
	  if (ffelex_token_length (ffesta_tokens[0]) != FFESTR_firstlTYPE)
	    break;
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeOPEN_PAREN:
	  if (ffelex_token_length (ffesta_tokens[0]) == FFESTR_firstlTYPE)
	    break;		/* Else might be assignment/stmtfuncdef. */
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeEQUALS:
	case FFELEX_typePOINTS:
	case FFELEX_typeCOLON:
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  break;
	}
      p = ffelex_token_text (ffesta_tokens[0]) + (i = FFESTR_firstlTYPE);
      if (ISDIGIT (*p))
	ffesta_confirmed ();	/* Else might be '90 TYPE statement. */
      for (ix = 0; ix < FFESTP_typeix; ++ix)
	ffestp_file.type.type_spec[ix].kw_or_val_present = FALSE;
      next = (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
	     FFEEXPR_contextFILEFORMATNML, (ffeexprCallback) ffestb_V0201_);
      next = (ffelexHandler) ffelex_splice_tokens (next, ffesta_tokens[0],
						   FFESTR_firstlTYPE);
      if (next == NULL)
	return (ffelexHandler) ffelex_swallow_tokens (t,
					       (ffelexHandler) ffesta_zero);
      return (ffelexHandler) (*next) (t);

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "TYPE I/O", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "TYPE I/O", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */
}

/* ffestb_V0201_ -- "TYPE" expr

   (ffestb_V0201_)  // to expression handler

   Make sure the next token is a COMMA or EOS/SEMICOLON.  */

static ffelexHandler
ffestb_V0201_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  bool comma = TRUE;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (!ffe_is_vxt () && (expr != NULL)
	  && (ffebld_op (expr) == FFEBLD_opSYMTER))
	break;
      comma = FALSE;
      /* Fall through. */
    case FFELEX_typeCOMMA:
      if (!ffe_is_vxt () && comma && (expr != NULL)
	  && (ffebld_op (expr) == FFEBLD_opPAREN)
	  && (ffebld_op (ffebld_left (expr)) == FFEBLD_opSYMTER))
	break;
      ffesta_confirmed ();
      ffestp_file.type.type_spec[FFESTP_typeixFORMAT].kw_or_val_present
	= TRUE;
      ffestp_file.type.type_spec[FFESTP_typeixFORMAT].kw_present = FALSE;
      ffestp_file.type.type_spec[FFESTP_typeixFORMAT].value_present = TRUE;
      ffestp_file.type.type_spec[FFESTP_typeixFORMAT].value_is_label
	= (expr == NULL);
      ffestp_file.type.type_spec[FFESTP_typeixFORMAT].value
	= ffelex_token_use (ft);
      ffestp_file.type.type_spec[FFESTP_typeixFORMAT].u.expr = expr;
      if (!ffesta_is_inhibited ())
	ffestc_V020_start ();
      ffestb_subr_kill_type_ ();
      if (ffelex_token_type (t) == FFELEX_typeCOMMA)
	return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		    FFEEXPR_contextIOLIST, (ffeexprCallback) ffestb_V0202_);
      if (!ffesta_is_inhibited ())
	ffestc_V020_finish ();
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  ffestb_subr_kill_type_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "TYPE I/O", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0202_ -- "TYPE" expr COMMA expr

   (ffestb_V0202_)  // to expression handler

   Handle COMMA or EOS/SEMICOLON here.	*/

static ffelexHandler
ffestb_V0202_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      if (expr == NULL)
	break;
      if (!ffesta_is_inhibited ())
	ffestc_V020_item (expr, ft);
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		    FFEEXPR_contextIOLIST, (ffeexprCallback) ffestb_V0202_);

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (expr == NULL)
	break;
      if (!ffesta_is_inhibited ())
	{
	  ffestc_V020_item (expr, ft);
	  ffestc_V020_finish ();
	}
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_V020_finish ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "TYPE I/O", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V021 -- Parse a DELETE statement

   return ffestb_V021;	// to lexer

   Make sure the statement has a valid form for a DELETE statement.
   If it does, implement the statement.	 */

#if FFESTR_VXT
ffelexHandler
ffestb_V021 (ffelexToken t)
{
  ffestpDeleteIx ix;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstDELETE)
	goto bad_0;		/* :::::::::::::::::::: */
      break;

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstDELETE)
	goto bad_0;		/* :::::::::::::::::::: */
      if (ffelex_token_length (ffesta_tokens[0]) != FFESTR_firstlDELETE)
	goto bad_0;		/* :::::::::::::::::::: */
      break;

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeOPEN_PAREN:
      break;

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
    case FFELEX_typeCOMMA:
    case FFELEX_typeCOLONCOLON:
      ffesta_confirmed ();	/* Error, but clearly intended. */
      goto bad_1;		/* :::::::::::::::::::: */

    default:
      goto bad_1;		/* :::::::::::::::::::: */
    }

  for (ix = 0; ix < FFESTP_deleteix; ++ix)
    ffestp_file.delete.delete_spec[ix].kw_or_val_present = FALSE;

  return (ffelexHandler) ffestb_V0211_;

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "DELETE", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "DELETE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */
}

/* ffestb_V0211_ -- "DELETE" OPEN_PAREN

   return ffestb_V0211_;  // to lexer

   Handle expr construct (not NAME=expr construct) here.  */

static ffelexHandler
ffestb_V0211_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_V0212_;

    default:
      return (ffelexHandler) (*((ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		  FFEEXPR_contextFILENUM, (ffeexprCallback) ffestb_V0213_)))
	(t);
    }
}

/* ffestb_V0212_ -- "DELETE" OPEN_PAREN NAME

   return ffestb_V0212_;  // to lexer

   If EQUALS here, go to states that handle it.	 Else, send NAME and this
   token thru expression handler.  */

static ffelexHandler
ffestb_V0212_ (ffelexToken t)
{
  ffelexHandler next;
  ffelexToken nt;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEQUALS:
      nt = ffesta_tokens[1];
      next = (ffelexHandler) ffestb_V0214_ (nt);
      ffelex_token_kill (nt);
      return (ffelexHandler) (*next) (t);

    default:
      next = (ffelexHandler) (*((ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		  FFEEXPR_contextFILENUM, (ffeexprCallback) ffestb_V0213_)))
	(ffesta_tokens[1]);
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) (*next) (t);
    }
}

/* ffestb_V0213_ -- "DELETE" OPEN_PAREN expr

   (ffestb_V0213_)  // to expression handler

   Handle COMMA or DELETE_PAREN here.  */

static ffelexHandler
ffestb_V0213_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
    case FFELEX_typeCLOSE_PAREN:
      if (expr == NULL)
	break;
      ffestp_file.delete.delete_spec[FFESTP_deleteixUNIT].kw_or_val_present
	= TRUE;
      ffestp_file.delete.delete_spec[FFESTP_deleteixUNIT].kw_present = FALSE;
      ffestp_file.delete.delete_spec[FFESTP_deleteixUNIT].value_present = TRUE;
      ffestp_file.delete.delete_spec[FFESTP_deleteixUNIT].value_is_label
	= FALSE;
      ffestp_file.delete.delete_spec[FFESTP_deleteixUNIT].value
	= ffelex_token_use (ft);
      ffestp_file.delete.delete_spec[FFESTP_deleteixUNIT].u.expr = expr;
      if (ffelex_token_type (t) == FFELEX_typeCOMMA)
	return (ffelexHandler) ffestb_V0214_;
      return (ffelexHandler) ffestb_V0219_;

    default:
      break;
    }

  ffestb_subr_kill_delete_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "DELETE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0214_ -- "DELETE" OPEN_PAREN [external-file-unit COMMA]

   return ffestb_V0214_;  // to lexer

   Handle expr construct (not NAME=expr construct) here.  */

static ffelexHandler
ffestb_V0214_ (ffelexToken t)
{
  ffestrGenio kw;

  ffestb_local_.delete.label = FALSE;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      kw = ffestr_genio (t);
      switch (kw)
	{
	case FFESTR_genioERR:
	  ffestb_local_.delete.ix = FFESTP_deleteixERR;
	  ffestb_local_.delete.label = TRUE;
	  break;

	case FFESTR_genioIOSTAT:
	  ffestb_local_.delete.ix = FFESTP_deleteixIOSTAT;
	  ffestb_local_.delete.left = TRUE;
	  ffestb_local_.delete.context = FFEEXPR_contextFILEINT;
	  break;

	case FFESTR_genioREC:
	  ffestb_local_.delete.ix = FFESTP_deleteixREC;
	  ffestb_local_.delete.left = FALSE;
	  ffestb_local_.delete.context = FFEEXPR_contextFILENUM;
	  break;

	case FFESTR_genioUNIT:
	  ffestb_local_.delete.ix = FFESTP_deleteixUNIT;
	  ffestb_local_.delete.left = FALSE;
	  ffestb_local_.delete.context = FFEEXPR_contextFILENUM;
	  break;

	default:
	  goto bad;		/* :::::::::::::::::::: */
	}
      if (ffestp_file.delete.delete_spec[ffestb_local_.delete.ix]
	  .kw_or_val_present)
	break;			/* Can't specify a keyword twice! */
      ffestp_file.delete.delete_spec[ffestb_local_.delete.ix]
	.kw_or_val_present = TRUE;
      ffestp_file.delete.delete_spec[ffestb_local_.delete.ix]
	.kw_present = TRUE;
      ffestp_file.delete.delete_spec[ffestb_local_.delete.ix]
	.value_present = FALSE;
      ffestp_file.delete.delete_spec[ffestb_local_.delete.ix].value_is_label
	= ffestb_local_.delete.label;
      ffestp_file.delete.delete_spec[ffestb_local_.delete.ix].kw
	= ffelex_token_use (t);
      return (ffelexHandler) ffestb_V0215_;

    default:
      break;
    }

bad:				/* :::::::::::::::::::: */
  ffestb_subr_kill_delete_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "DELETE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0215_ -- "DELETE" OPEN_PAREN [external-file-unit COMMA] NAME

   return ffestb_V0215_;  // to lexer

   Make sure EQUALS here, send next token to expression handler.  */

static ffelexHandler
ffestb_V0215_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEQUALS:
      ffesta_confirmed ();
      if (ffestb_local_.delete.label)
	return (ffelexHandler) ffestb_V0217_;
      if (ffestb_local_.delete.left)
	return (ffelexHandler) ffeexpr_lhs (ffesta_output_pool,
					    ffestb_local_.delete.context,
					    (ffeexprCallback) ffestb_V0216_);
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
	     ffestb_local_.delete.context, (ffeexprCallback) ffestb_V0216_);

    default:
      break;
    }

  ffestb_subr_kill_delete_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "DELETE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0216_ -- "DELETE" OPEN_PAREN ... NAME EQUALS expr

   (ffestb_V0216_)  // to expression handler

   Handle COMMA or CLOSE_PAREN here.  */

static ffelexHandler
ffestb_V0216_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
    case FFELEX_typeCLOSE_PAREN:
      if (expr == NULL)
	break;
      ffestp_file.delete.delete_spec[ffestb_local_.delete.ix].value_present
	= TRUE;
      ffestp_file.delete.delete_spec[ffestb_local_.delete.ix].value
	= ffelex_token_use (ft);
      ffestp_file.delete.delete_spec[ffestb_local_.delete.ix].u.expr = expr;
      if (ffelex_token_type (t) == FFELEX_typeCOMMA)
	return (ffelexHandler) ffestb_V0214_;
      return (ffelexHandler) ffestb_V0219_;

    default:
      break;
    }

  ffestb_subr_kill_delete_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "DELETE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0217_ -- "DELETE" OPEN_PAREN ... NAME EQUALS

   return ffestb_V0217_;  // to lexer

   Handle NUMBER for label here.  */

static ffelexHandler
ffestb_V0217_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNUMBER:
      ffestp_file.delete.delete_spec[ffestb_local_.delete.ix].value_present
	= TRUE;
      ffestp_file.delete.delete_spec[ffestb_local_.delete.ix].value
	= ffelex_token_use (t);
      return (ffelexHandler) ffestb_V0218_;

    default:
      break;
    }

  ffestb_subr_kill_delete_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "DELETE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0218_ -- "DELETE" OPEN_PAREN ... NAME EQUALS NUMBER

   return ffestb_V0218_;  // to lexer

   Handle COMMA or CLOSE_PAREN here.  */

static ffelexHandler
ffestb_V0218_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      return (ffelexHandler) ffestb_V0214_;

    case FFELEX_typeCLOSE_PAREN:
      return (ffelexHandler) ffestb_V0219_;

    default:
      break;
    }

  ffestb_subr_kill_delete_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "DELETE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0219_ -- "DELETE" OPEN_PAREN ... CLOSE_PAREN

   return ffestb_V0219_;  // to lexer

   Handle EOS or SEMICOLON here.  */

static ffelexHandler
ffestb_V0219_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	ffestc_V021 ();
      ffestb_subr_kill_delete_ ();
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  ffestb_subr_kill_delete_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "DELETE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V026 -- Parse a FIND statement

   return ffestb_V026;	// to lexer

   Make sure the statement has a valid form for a FIND statement.
   If it does, implement the statement.	 */

ffelexHandler
ffestb_V026 (ffelexToken t)
{
  ffestpFindIx ix;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstFIND)
	goto bad_0;		/* :::::::::::::::::::: */
      break;

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstFIND)
	goto bad_0;		/* :::::::::::::::::::: */
      if (ffelex_token_length (ffesta_tokens[0]) != FFESTR_firstlFIND)
	goto bad_0;		/* :::::::::::::::::::: */
      break;

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeOPEN_PAREN:
      break;

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
    case FFELEX_typeCOMMA:
    case FFELEX_typeCOLONCOLON:
      ffesta_confirmed ();	/* Error, but clearly intended. */
      goto bad_1;		/* :::::::::::::::::::: */

    default:
      goto bad_1;		/* :::::::::::::::::::: */
    }

  for (ix = 0; ix < FFESTP_findix; ++ix)
    ffestp_file.find.find_spec[ix].kw_or_val_present = FALSE;

  return (ffelexHandler) ffestb_V0261_;

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FIND", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FIND", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */
}

/* ffestb_V0261_ -- "FIND" OPEN_PAREN

   return ffestb_V0261_;  // to lexer

   Handle expr construct (not NAME=expr construct) here.  */

static ffelexHandler
ffestb_V0261_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_V0262_;

    default:
      return (ffelexHandler) (*((ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		  FFEEXPR_contextFILENUM, (ffeexprCallback) ffestb_V0263_)))
	(t);
    }
}

/* ffestb_V0262_ -- "FIND" OPEN_PAREN NAME

   return ffestb_V0262_;  // to lexer

   If EQUALS here, go to states that handle it.	 Else, send NAME and this
   token thru expression handler.  */

static ffelexHandler
ffestb_V0262_ (ffelexToken t)
{
  ffelexHandler next;
  ffelexToken nt;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEQUALS:
      nt = ffesta_tokens[1];
      next = (ffelexHandler) ffestb_V0264_ (nt);
      ffelex_token_kill (nt);
      return (ffelexHandler) (*next) (t);

    default:
      next = (ffelexHandler) (*((ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		  FFEEXPR_contextFILENUM, (ffeexprCallback) ffestb_V0263_)))
	(ffesta_tokens[1]);
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) (*next) (t);
    }
}

/* ffestb_V0263_ -- "FIND" OPEN_PAREN expr

   (ffestb_V0263_)  // to expression handler

   Handle COMMA or FIND_PAREN here.  */

static ffelexHandler
ffestb_V0263_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
    case FFELEX_typeCLOSE_PAREN:
      if (expr == NULL)
	break;
      ffestp_file.find.find_spec[FFESTP_findixUNIT].kw_or_val_present
	= TRUE;
      ffestp_file.find.find_spec[FFESTP_findixUNIT].kw_present = FALSE;
      ffestp_file.find.find_spec[FFESTP_findixUNIT].value_present = TRUE;
      ffestp_file.find.find_spec[FFESTP_findixUNIT].value_is_label
	= FALSE;
      ffestp_file.find.find_spec[FFESTP_findixUNIT].value
	= ffelex_token_use (ft);
      ffestp_file.find.find_spec[FFESTP_findixUNIT].u.expr = expr;
      if (ffelex_token_type (t) == FFELEX_typeCOMMA)
	return (ffelexHandler) ffestb_V0264_;
      return (ffelexHandler) ffestb_V0269_;

    default:
      break;
    }

  ffestb_subr_kill_find_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FIND", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0264_ -- "FIND" OPEN_PAREN [external-file-unit COMMA]

   return ffestb_V0264_;  // to lexer

   Handle expr construct (not NAME=expr construct) here.  */

static ffelexHandler
ffestb_V0264_ (ffelexToken t)
{
  ffestrGenio kw;

  ffestb_local_.find.label = FALSE;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      kw = ffestr_genio (t);
      switch (kw)
	{
	case FFESTR_genioERR:
	  ffestb_local_.find.ix = FFESTP_findixERR;
	  ffestb_local_.find.label = TRUE;
	  break;

	case FFESTR_genioIOSTAT:
	  ffestb_local_.find.ix = FFESTP_findixIOSTAT;
	  ffestb_local_.find.left = TRUE;
	  ffestb_local_.find.context = FFEEXPR_contextFILEINT;
	  break;

	case FFESTR_genioREC:
	  ffestb_local_.find.ix = FFESTP_findixREC;
	  ffestb_local_.find.left = FALSE;
	  ffestb_local_.find.context = FFEEXPR_contextFILENUM;
	  break;

	case FFESTR_genioUNIT:
	  ffestb_local_.find.ix = FFESTP_findixUNIT;
	  ffestb_local_.find.left = FALSE;
	  ffestb_local_.find.context = FFEEXPR_contextFILENUM;
	  break;

	default:
	  goto bad;		/* :::::::::::::::::::: */
	}
      if (ffestp_file.find.find_spec[ffestb_local_.find.ix]
	  .kw_or_val_present)
	break;			/* Can't specify a keyword twice! */
      ffestp_file.find.find_spec[ffestb_local_.find.ix]
	.kw_or_val_present = TRUE;
      ffestp_file.find.find_spec[ffestb_local_.find.ix]
	.kw_present = TRUE;
      ffestp_file.find.find_spec[ffestb_local_.find.ix]
	.value_present = FALSE;
      ffestp_file.find.find_spec[ffestb_local_.find.ix].value_is_label
	= ffestb_local_.find.label;
      ffestp_file.find.find_spec[ffestb_local_.find.ix].kw
	= ffelex_token_use (t);
      return (ffelexHandler) ffestb_V0265_;

    default:
      break;
    }

bad:				/* :::::::::::::::::::: */
  ffestb_subr_kill_find_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FIND", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0265_ -- "FIND" OPEN_PAREN [external-file-unit COMMA] NAME

   return ffestb_V0265_;  // to lexer

   Make sure EQUALS here, send next token to expression handler.  */

static ffelexHandler
ffestb_V0265_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEQUALS:
      ffesta_confirmed ();
      if (ffestb_local_.find.label)
	return (ffelexHandler) ffestb_V0267_;
      if (ffestb_local_.find.left)
	return (ffelexHandler) ffeexpr_lhs (ffesta_output_pool,
					    ffestb_local_.find.context,
					    (ffeexprCallback) ffestb_V0266_);
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
					  ffestb_local_.find.context,
					  (ffeexprCallback) ffestb_V0266_);

    default:
      break;
    }

  ffestb_subr_kill_find_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FIND", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0266_ -- "FIND" OPEN_PAREN ... NAME EQUALS expr

   (ffestb_V0266_)  // to expression handler

   Handle COMMA or CLOSE_PAREN here.  */

static ffelexHandler
ffestb_V0266_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
    case FFELEX_typeCLOSE_PAREN:
      if (expr == NULL)
	break;
      ffestp_file.find.find_spec[ffestb_local_.find.ix].value_present
	= TRUE;
      ffestp_file.find.find_spec[ffestb_local_.find.ix].value
	= ffelex_token_use (ft);
      ffestp_file.find.find_spec[ffestb_local_.find.ix].u.expr = expr;
      if (ffelex_token_type (t) == FFELEX_typeCOMMA)
	return (ffelexHandler) ffestb_V0264_;
      return (ffelexHandler) ffestb_V0269_;

    default:
      break;
    }

  ffestb_subr_kill_find_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FIND", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0267_ -- "FIND" OPEN_PAREN ... NAME EQUALS

   return ffestb_V0267_;  // to lexer

   Handle NUMBER for label here.  */

static ffelexHandler
ffestb_V0267_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNUMBER:
      ffestp_file.find.find_spec[ffestb_local_.find.ix].value_present
	= TRUE;
      ffestp_file.find.find_spec[ffestb_local_.find.ix].value
	= ffelex_token_use (t);
      return (ffelexHandler) ffestb_V0268_;

    default:
      break;
    }

  ffestb_subr_kill_find_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FIND", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0268_ -- "FIND" OPEN_PAREN ... NAME EQUALS NUMBER

   return ffestb_V0268_;  // to lexer

   Handle COMMA or CLOSE_PAREN here.  */

static ffelexHandler
ffestb_V0268_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      return (ffelexHandler) ffestb_V0264_;

    case FFELEX_typeCLOSE_PAREN:
      return (ffelexHandler) ffestb_V0269_;

    default:
      break;
    }

  ffestb_subr_kill_find_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FIND", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0269_ -- "FIND" OPEN_PAREN ... CLOSE_PAREN

   return ffestb_V0269_;  // to lexer

   Handle EOS or SEMICOLON here.  */

static ffelexHandler
ffestb_V0269_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	ffestc_V026 ();
      ffestb_subr_kill_find_ ();
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  ffestb_subr_kill_find_ ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FIND", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

#endif
/* ffestb_dimlist -- Parse the ALLOCATABLE/POINTER/TARGET statement

   return ffestb_dimlist;  // to lexer

   Make sure the statement has a valid form for the ALLOCATABLE/POINTER/
   TARGET statement.  If it does, implement the statement.  */

#if FFESTR_F90
ffelexHandler
ffestb_dimlist (ffelexToken t)
{
  ffeTokenLength i;
  const char *p;
  ffelexToken nt;
  ffelexHandler next;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeCOMMA:
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();
	  if (!ffesta_is_inhibited ())
	    {
	      switch (ffesta_first_kw)
		{
		case FFESTR_firstALLOCATABLE:
		  ffestc_R525_start ();
		  break;

		case FFESTR_firstPOINTER:
		  ffestc_R526_start ();
		  break;

		case FFESTR_firstTARGET:
		  ffestc_R527_start ();
		  break;

		default:
		  assert (FALSE);
		}
	    }
	  ffestb_local_.dimlist.started = TRUE;
	  return (ffelexHandler) ffestb_dimlist1_;

	case FFELEX_typeNAME:
	  ffesta_confirmed ();
	  if (!ffesta_is_inhibited ())
	    {
	      switch (ffesta_first_kw)
		{
		case FFESTR_firstALLOCATABLE:
		  ffestc_R525_start ();
		  break;

		case FFESTR_firstPOINTER:
		  ffestc_R526_start ();
		  break;

		case FFESTR_firstTARGET:
		  ffestc_R527_start ();
		  break;

		default:
		  assert (FALSE);
		}
	    }
	  ffestb_local_.dimlist.started = TRUE;
	  return (ffelexHandler) ffestb_dimlist1_ (t);
	}

    case FFELEX_typeNAMES:
      p = ffelex_token_text (ffesta_tokens[0]) + (i = ffestb_args.dimlist.len);
      switch (ffelex_token_type (t))
	{
	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	case FFELEX_typeCOMMA:
	  ffesta_confirmed ();
	  if (!ffesrc_is_name_init (*p))
	    goto bad_i;		/* :::::::::::::::::::: */
	  nt = ffelex_token_name_from_names (ffesta_tokens[0], i, 0);
	  if (!ffesta_is_inhibited ())
	    {
	      switch (ffesta_first_kw)
		{
		case FFESTR_firstALLOCATABLE:
		  ffestc_R525_start ();
		  break;

		case FFESTR_firstPOINTER:
		  ffestc_R526_start ();
		  break;

		case FFESTR_firstTARGET:
		  ffestc_R527_start ();
		  break;

		default:
		  assert (FALSE);
		}
	    }
	  ffestb_local_.dimlist.started = TRUE;
	  next = (ffelexHandler) ffestb_dimlist1_ (nt);
	  ffelex_token_kill (nt);
	  return (ffelexHandler) (*next) (t);

	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();
	  if (*p != '\0')
	    goto bad_i;		/* :::::::::::::::::::: */
	  if (!ffesta_is_inhibited ())
	    {
	      switch (ffesta_first_kw)
		{
		case FFESTR_firstALLOCATABLE:
		  ffestc_R525_start ();
		  break;

		case FFESTR_firstPOINTER:
		  ffestc_R526_start ();
		  break;

		case FFESTR_firstTARGET:
		  ffestc_R527_start ();
		  break;

		default:
		  assert (FALSE);
		}
	    }
	  ffestb_local_.dimlist.started = TRUE;
	  return (ffelexHandler) ffestb_dimlist1_;

	case FFELEX_typeOPEN_PAREN:
	  if (!ffesrc_is_name_init (*p))
	    goto bad_i;		/* :::::::::::::::::::: */
	  nt = ffelex_token_name_from_names (ffesta_tokens[0], i, 0);
	  ffestb_local_.dimlist.started = FALSE;
	  next = (ffelexHandler) ffestb_dimlist1_ (nt);
	  ffelex_token_kill (nt);
	  return (ffelexHandler) (*next) (t);
	}

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.dimlist.badname, ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.dimlist.badname, t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */

bad_i:				/* :::::::::::::::::::: */
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, ffestb_args.dimlist.badname, ffesta_tokens[0], i, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_dimlist1_ -- "ALLOCATABLE/POINTER/TARGET" [COLONCOLON]

   return ffestb_dimlist1_;  // to lexer

   Handle NAME.	 */

static ffelexHandler
ffestb_dimlist1_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_dimlist2_;

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.dimlist.badname, t);
      break;
    }

  if (!ffesta_is_inhibited ())
    {
      switch (ffesta_first_kw)
	{
	case FFESTR_firstALLOCATABLE:
	  ffestc_R525_finish ();
	  break;

	case FFESTR_firstPOINTER:
	  ffestc_R526_finish ();
	  break;

	case FFESTR_firstTARGET:
	  ffestc_R527_finish ();
	  break;

	default:
	  assert (FALSE);
	}
    }
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_dimlist2_ -- "ALLOCATABLE/POINTER/TARGET" ... NAME

   return ffestb_dimlist2_;  // to lexer

   Handle OPEN_PAREN.  */

static ffelexHandler
ffestb_dimlist2_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeOPEN_PAREN:
      ffestb_subrargs_.dim_list.dims = ffestt_dimlist_create ();
      ffestb_subrargs_.dim_list.handler = (ffelexHandler) ffestb_dimlist3_;
      ffestb_subrargs_.dim_list.pool = ffesta_output_pool;
      ffestb_subrargs_.dim_list.ctx = FFEEXPR_contextDIMLIST;
#ifdef FFECOM_dimensionsMAX
      ffestb_subrargs_.dim_list.ndims = 0;
#endif
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
	    FFEEXPR_contextDIMLIST, (ffeexprCallback) ffestb_subr_dimlist_);

    case FFELEX_typeCOMMA:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	{
	  if (!ffestb_local_.dimlist.started)
	    {
	      switch (ffesta_first_kw)
		{
		case FFESTR_firstALLOCATABLE:
		  ffestc_R525_start ();
		  break;

		case FFESTR_firstPOINTER:
		  ffestc_R526_start ();
		  break;

		case FFESTR_firstTARGET:
		  ffestc_R527_start ();
		  break;

		default:
		  assert (FALSE);
		}
	      ffestb_local_.dimlist.started = TRUE;
	    }
	  switch (ffesta_first_kw)
	    {
	    case FFESTR_firstALLOCATABLE:
	      ffestc_R525_item (ffesta_tokens[1], NULL);
	      break;

	    case FFESTR_firstPOINTER:
	      ffestc_R526_item (ffesta_tokens[1], NULL);
	      break;

	    case FFESTR_firstTARGET:
	      ffestc_R527_item (ffesta_tokens[1], NULL);
	      break;

	    default:
	      assert (FALSE);
	    }
	}
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffestb_dimlist4_;

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	{
	  if (!ffestb_local_.dimlist.started)
	    {
	      switch (ffesta_first_kw)
		{
		case FFESTR_firstALLOCATABLE:
		  ffestc_R525_start ();
		  break;

		case FFESTR_firstPOINTER:
		  ffestc_R526_start ();
		  break;

		case FFESTR_firstTARGET:
		  ffestc_R527_start ();
		  break;

		default:
		  assert (FALSE);
		}
	    }
	  switch (ffesta_first_kw)
	    {
	    case FFESTR_firstALLOCATABLE:
	      ffestc_R525_item (ffesta_tokens[1], NULL);
	      ffestc_R525_finish ();
	      break;

	    case FFESTR_firstPOINTER:
	      ffestc_R526_item (ffesta_tokens[1], NULL);
	      ffestc_R526_finish ();
	      break;

	    case FFESTR_firstTARGET:
	      ffestc_R527_item (ffesta_tokens[1], NULL);
	      ffestc_R527_finish ();
	      break;

	    default:
	      assert (FALSE);
	    }
	}
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffesta_zero (t);

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.dimlist.badname, t);
      break;
    }

  if (!ffesta_is_inhibited ())
    {
      switch (ffesta_first_kw)
	{
	case FFESTR_firstALLOCATABLE:
	  ffestc_R525_finish ();
	  break;

	case FFESTR_firstPOINTER:
	  ffestc_R526_finish ();
	  break;

	case FFESTR_firstTARGET:
	  ffestc_R527_finish ();
	  break;

	default:
	  assert (FALSE);
	}
    }
  ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_dimlist3_ -- "ALLOCATABLE/POINTER/TARGET" ... NAME OPEN_PAREN
		       dimlist CLOSE_PAREN

   return ffestb_dimlist3_;  // to lexer

   Handle COMMA or EOS/SEMICOLON.  */

static ffelexHandler
ffestb_dimlist3_ (ffelexToken t)
{
  if (!ffestb_subrargs_.dim_list.ok)
    goto bad;			/* :::::::::::::::::::: */

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	{
	  if (!ffestb_local_.dimlist.started)
	    {
	      switch (ffesta_first_kw)
		{
		case FFESTR_firstALLOCATABLE:
		  ffestc_R525_start ();
		  break;

		case FFESTR_firstPOINTER:
		  ffestc_R526_start ();
		  break;

		case FFESTR_firstTARGET:
		  ffestc_R527_start ();
		  break;

		default:
		  assert (FALSE);
		}
	      ffestb_local_.dimlist.started = TRUE;
	    }
	  switch (ffesta_first_kw)
	    {
	    case FFESTR_firstALLOCATABLE:
	      ffestc_R525_item (ffesta_tokens[1],
				ffestb_subrargs_.dim_list.dims);
	      break;

	    case FFESTR_firstPOINTER:
	      ffestc_R526_item (ffesta_tokens[1],
				ffestb_subrargs_.dim_list.dims);
	      break;

	    case FFESTR_firstTARGET:
	      ffestc_R527_item (ffesta_tokens[1],
				ffestb_subrargs_.dim_list.dims);
	      break;

	    default:
	      assert (FALSE);
	    }
	}
      ffelex_token_kill (ffesta_tokens[1]);
      ffestt_dimlist_kill (ffestb_subrargs_.dim_list.dims);
      return (ffelexHandler) ffestb_dimlist4_;

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	{
	  if (!ffestb_local_.dimlist.started)
	    {
	      switch (ffesta_first_kw)
		{
		case FFESTR_firstALLOCATABLE:
		  ffestc_R525_start ();
		  break;

		case FFESTR_firstPOINTER:
		  ffestc_R526_start ();
		  break;

		case FFESTR_firstTARGET:
		  ffestc_R527_start ();
		  break;

		default:
		  assert (FALSE);
		}
	    }
	  switch (ffesta_first_kw)
	    {
	    case FFESTR_firstALLOCATABLE:
	      ffestc_R525_item (ffesta_tokens[1],
				ffestb_subrargs_.dim_list.dims);
	      ffestc_R525_finish ();
	      break;

	    case FFESTR_firstPOINTER:
	      ffestc_R526_item (ffesta_tokens[1],
				ffestb_subrargs_.dim_list.dims);
	      ffestc_R526_finish ();
	      break;

	    case FFESTR_firstTARGET:
	      ffestc_R527_item (ffesta_tokens[1],
				ffestb_subrargs_.dim_list.dims);
	      ffestc_R527_finish ();
	      break;

	    default:
	      assert (FALSE);
	    }
	}
      ffelex_token_kill (ffesta_tokens[1]);
      ffestt_dimlist_kill (ffestb_subrargs_.dim_list.dims);
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

bad:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.dimlist.badname, t);
  if (ffestb_local_.dimlist.started && !ffesta_is_inhibited ())
    {
      switch (ffesta_first_kw)
	{
	case FFESTR_firstALLOCATABLE:
	  ffestc_R525_finish ();
	  break;

	case FFESTR_firstPOINTER:
	  ffestc_R526_finish ();
	  break;

	case FFESTR_firstTARGET:
	  ffestc_R527_finish ();
	  break;

	default:
	  assert (FALSE);
	}
    }
  ffestt_dimlist_kill (ffestb_subrargs_.dim_list.dims);
  ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_dimlist4_ -- "ALLOCATABLE/POINTER/TARGET" ... COMMA

   return ffestb_dimlist4_;  // to lexer

   Make sure we don't have EOS or SEMICOLON.  */

static ffelexHandler
ffestb_dimlist4_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (!ffesta_is_inhibited ())
	{
	  switch (ffesta_first_kw)
	    {
	    case FFESTR_firstALLOCATABLE:
	      ffestc_R525_finish ();
	      break;

	    case FFESTR_firstPOINTER:
	      ffestc_R526_finish ();
	      break;

	    case FFESTR_firstTARGET:
	      ffestc_R527_finish ();
	      break;

	    default:
	      assert (FALSE);
	    }
	}
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.dimlist.badname, t);
      return (ffelexHandler) ffesta_zero (t);

    default:
      return (ffelexHandler) ffestb_dimlist1_ (t);
    }
}

#endif
/* ffestb_dummy -- Parse an ENTRY/FUNCTION/SUBROUTINE statement

   return ffestb_dummy;	 // to lexer

   Make sure the statement has a valid form for an ENTRY/FUNCTION/SUBROUTINE
   statement.  If it does, implement the statement.  */

ffelexHandler
ffestb_dummy (ffelexToken t)
{
  ffeTokenLength i;
  unsigned const char *p;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeNAME:
	  break;
	}

      ffesta_confirmed ();
      ffesta_tokens[1] = ffelex_token_use (t);
      ffestb_local_.decl.recursive = NULL;
      ffestb_local_.dummy.badname = ffestb_args.dummy.badname;
      ffestb_local_.dummy.is_subr = ffestb_args.dummy.is_subr;
      ffestb_local_.dummy.first_kw = ffesta_first_kw;
      return (ffelexHandler) ffestb_dummy1_;

    case FFELEX_typeNAMES:
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  ffesta_confirmed ();
	  break;

	case FFELEX_typeOPEN_PAREN:
	  break;
	}
      p = ffelex_token_text (ffesta_tokens[0]) + (i = ffestb_args.dummy.len);
      if (!ffesrc_is_name_init (*p))
	goto bad_i;		/* :::::::::::::::::::: */
      ffesta_tokens[1]
	= ffelex_token_name_from_names (ffesta_tokens[0], i, 0);
      ffestb_local_.decl.recursive = NULL;
      ffestb_local_.dummy.badname = ffestb_args.dummy.badname;
      ffestb_local_.dummy.is_subr = ffestb_args.dummy.is_subr;
      ffestb_local_.dummy.first_kw = ffesta_first_kw;
      return (ffelexHandler) ffestb_dummy1_ (t);

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.dummy.badname, ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.dummy.badname, t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */

bad_i:				/* :::::::::::::::::::: */
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, ffestb_args.dummy.badname, ffesta_tokens[0], i, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_dummy1_ -- "ENTRY/FUNCTION/SUBROUTINE" NAME

   return ffestb_dummy1_;  // to lexer

   Make sure the next token is an EOS, SEMICOLON, or OPEN_PAREN.  In the
   former case, just implement a null arg list, else get the arg list and
   then implement.  */

static ffelexHandler
ffestb_dummy1_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (ffestb_local_.dummy.first_kw == FFESTR_firstFUNCTION)
	{
	  ffesta_confirmed ();	/* Later, not if typename w/o RECURSIVE. */
	  break;		/* Produce an error message, need that open
				   paren. */
	}
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	{			/* Pretend as though we got a truly NULL
				   list. */
	  ffestb_subrargs_.name_list.args = NULL;
	  ffestb_subrargs_.name_list.ok = TRUE;
	  ffestb_subrargs_.name_list.close_paren = ffelex_token_use (t);
	  return (ffelexHandler) ffestb_dummy2_ (t);
	}
      if (ffestb_local_.decl.recursive != NULL)
	ffelex_token_kill (ffestb_local_.decl.recursive);
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffesta_zero (t);

    case FFELEX_typeOPEN_PAREN:
      ffestb_subrargs_.name_list.args = ffestt_tokenlist_create ();
      ffestb_subrargs_.name_list.handler = (ffelexHandler) ffestb_dummy2_;
      ffestb_subrargs_.name_list.is_subr = ffestb_local_.dummy.is_subr;
      ffestb_subrargs_.name_list.names = FALSE;
      return (ffelexHandler) ffestb_subr_name_list_;

    default:
      break;
    }

  if (ffestb_local_.decl.recursive != NULL)
    ffelex_token_kill (ffestb_local_.decl.recursive);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_local_.dummy.badname, t);
  ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_dummy2_ -- <dummy-keyword> NAME OPEN_PAREN arg-list CLOSE_PAREN

   return ffestb_dummy2_;  // to lexer

   Make sure the statement has a valid form for a dummy-def statement.	If it
   does, implement the statement.  */

static ffelexHandler
ffestb_dummy2_ (ffelexToken t)
{
  if (!ffestb_subrargs_.name_list.ok)
    goto bad;			/* :::::::::::::::::::: */

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	{
	  switch (ffestb_local_.dummy.first_kw)
	    {
	    case FFESTR_firstFUNCTION:
	      ffestc_R1219 (ffesta_tokens[1], ffestb_subrargs_.name_list.args,
		    ffestb_subrargs_.name_list.close_paren, FFESTP_typeNone,
		NULL, NULL, NULL, NULL, ffestb_local_.decl.recursive, NULL);
	      break;

	    case FFESTR_firstSUBROUTINE:
	      ffestc_R1223 (ffesta_tokens[1], ffestb_subrargs_.name_list.args,
			    ffestb_subrargs_.name_list.close_paren,
			    ffestb_local_.decl.recursive);
	      break;

	    case FFESTR_firstENTRY:
	      ffestc_R1226 (ffesta_tokens[1], ffestb_subrargs_.name_list.args,
			    ffestb_subrargs_.name_list.close_paren);
	      break;

	    default:
	      assert (FALSE);
	    }
	}
      ffelex_token_kill (ffesta_tokens[1]);
      if (ffestb_local_.decl.recursive != NULL)
	ffelex_token_kill (ffestb_local_.decl.recursive);
      ffelex_token_kill (ffestb_subrargs_.name_list.close_paren);
      if (ffestb_subrargs_.name_list.args != NULL)
	ffestt_tokenlist_kill (ffestb_subrargs_.name_list.args);
      return (ffelexHandler) ffesta_zero (t);

    case FFELEX_typeNAME:
      ffesta_confirmed ();
      if ((ffestb_local_.dummy.first_kw != FFESTR_firstFUNCTION)
	  || (ffestr_other (t) != FFESTR_otherRESULT))
	break;
      ffestb_local_.decl.type = FFESTP_typeNone;
      ffestb_local_.decl.kind = NULL;
      ffestb_local_.decl.kindt = NULL;
      ffestb_local_.decl.len = NULL;
      ffestb_local_.decl.lent = NULL;
      return (ffelexHandler) ffestb_decl_funcname_6_;

    default:
      break;
    }

bad:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_local_.dummy.badname, t);
  ffelex_token_kill (ffesta_tokens[1]);
  if (ffestb_local_.decl.recursive != NULL)
    ffelex_token_kill (ffestb_local_.decl.recursive);
  ffelex_token_kill (ffestb_subrargs_.name_list.close_paren);
  if (ffestb_subrargs_.name_list.args != NULL)
    ffestt_tokenlist_kill (ffestb_subrargs_.name_list.args);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R524 -- Parse the DIMENSION statement

   return ffestb_R524;	// to lexer

   Make sure the statement has a valid form for the DIMENSION statement.  If
   it does, implement the statement.  */

ffelexHandler
ffestb_R524 (ffelexToken t)
{
  ffeTokenLength i;
  unsigned const char *p;
  ffelexToken nt;
  ffelexHandler next;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeNAME:
	  ffesta_confirmed ();
	  if (!ffesta_is_inhibited ())
	    ffestc_R524_start (ffesta_first_kw == FFESTR_firstVIRTUAL);
	  ffestb_local_.dimension.started = TRUE;
	  return (ffelexHandler) ffestb_R5241_ (t);
	}

    case FFELEX_typeNAMES:
      p = ffelex_token_text (ffesta_tokens[0]) + (i = ffestb_args.R524.len);
      switch (ffelex_token_type (t))
	{
	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeOPEN_PAREN:
	  break;
	}

      /* Here, we have at least one char after "DIMENSION" and t is
	 OPEN_PAREN. */

      if (!ffesrc_is_name_init (*p))
	goto bad_i;		/* :::::::::::::::::::: */
      nt = ffelex_token_name_from_names (ffesta_tokens[0], i, 0);
      ffestb_local_.dimension.started = FALSE;
      next = (ffelexHandler) ffestb_R5241_ (nt);
      ffelex_token_kill (nt);
      return (ffelexHandler) (*next) (t);

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.R524.badname, ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.R524.badname, t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */

bad_i:				/* :::::::::::::::::::: */
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, ffestb_args.R524.badname, ffesta_tokens[0], i, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R5241_ -- "DIMENSION"

   return ffestb_R5241_;  // to lexer

   Handle NAME.	 */

static ffelexHandler
ffestb_R5241_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_R5242_;

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.R524.badname, t);
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_R524_finish ();
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R5242_ -- "DIMENSION" ... NAME

   return ffestb_R5242_;  // to lexer

   Handle OPEN_PAREN.  */

static ffelexHandler
ffestb_R5242_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeOPEN_PAREN:
      ffestb_subrargs_.dim_list.dims = ffestt_dimlist_create ();
      ffestb_subrargs_.dim_list.handler = (ffelexHandler) ffestb_R5243_;
      ffestb_subrargs_.dim_list.pool = ffesta_output_pool;
      ffestb_subrargs_.dim_list.ctx = ffesta_is_entry_valid
	? FFEEXPR_contextDIMLIST : FFEEXPR_contextDIMLISTCOMMON;
#ifdef FFECOM_dimensionsMAX
      ffestb_subrargs_.dim_list.ndims = 0;
#endif
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
					  ffestb_subrargs_.dim_list.ctx,
				    (ffeexprCallback) ffestb_subr_dimlist_);

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.R524.badname, t);
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_R524_finish ();
  ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R5243_ -- "DIMENSION" ... NAME OPEN_PAREN dimlist CLOSE_PAREN

   return ffestb_R5243_;  // to lexer

   Handle COMMA or EOS/SEMICOLON.  */

static ffelexHandler
ffestb_R5243_ (ffelexToken t)
{
  if (!ffestb_subrargs_.dim_list.ok)
    goto bad;			/* :::::::::::::::::::: */

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	{
	  if (!ffestb_local_.dimension.started)
	    {
	      ffestc_R524_start (ffesta_first_kw == FFESTR_firstVIRTUAL);
	      ffestb_local_.dimension.started = TRUE;
	    }
	  ffestc_R524_item (ffesta_tokens[1],
			    ffestb_subrargs_.dim_list.dims);
	}
      ffelex_token_kill (ffesta_tokens[1]);
      ffestt_dimlist_kill (ffestb_subrargs_.dim_list.dims);
      return (ffelexHandler) ffestb_R5244_;

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	{
	  if (!ffestb_local_.dimension.started)
	    {
	      ffestc_R524_start (ffesta_first_kw == FFESTR_firstVIRTUAL);
	      ffestb_local_.dimension.started = TRUE;
	    }
	  ffestc_R524_item (ffesta_tokens[1],
			    ffestb_subrargs_.dim_list.dims);
	  ffestc_R524_finish ();
	}
      ffelex_token_kill (ffesta_tokens[1]);
      ffestt_dimlist_kill (ffestb_subrargs_.dim_list.dims);
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

bad:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.R524.badname, t);
  if (ffestb_local_.dimension.started && !ffesta_is_inhibited ())
    ffestc_R524_finish ();
  ffestt_dimlist_kill (ffestb_subrargs_.dim_list.dims);
  ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R5244_ -- "DIMENSION" ... COMMA

   return ffestb_R5244_;  // to lexer

   Make sure we don't have EOS or SEMICOLON.  */

static ffelexHandler
ffestb_R5244_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (!ffesta_is_inhibited ())
	ffestc_R524_finish ();
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, ffestb_args.R524.badname, t);
      return (ffelexHandler) ffesta_zero (t);

    default:
      return (ffelexHandler) ffestb_R5241_ (t);
    }
}

/* ffestb_R547 -- Parse the COMMON statement

   return ffestb_R547;	// to lexer

   Make sure the statement has a valid form for the COMMON statement.  If it
   does, implement the statement.  */

ffelexHandler
ffestb_R547 (ffelexToken t)
{
  ffeTokenLength i;
  unsigned const char *p;
  ffelexToken nt;
  ffelexHandler next;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstCOMMON)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeNAME:
	case FFELEX_typeSLASH:
	case FFELEX_typeCONCAT:
	  ffesta_confirmed ();
	  if (!ffesta_is_inhibited ())
	    ffestc_R547_start ();
	  ffestb_local_.common.started = TRUE;
	  return (ffelexHandler) ffestb_R5471_ (t);
	}

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstCOMMON)
	goto bad_0;		/* :::::::::::::::::::: */
      p = ffelex_token_text (ffesta_tokens[0]) + (i = FFESTR_firstlCOMMON);
      switch (ffelex_token_type (t))
	{
	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();
	  break;

	case FFELEX_typeSLASH:
	case FFELEX_typeCONCAT:
	  ffesta_confirmed ();
	  if (*p != '\0')
	    break;
	  if (!ffesta_is_inhibited ())
	    ffestc_R547_start ();
	  ffestb_local_.common.started = TRUE;
	  return (ffelexHandler) ffestb_R5471_ (t);

	case FFELEX_typeOPEN_PAREN:
	  break;
	}

      /* Here, we have at least one char after "COMMON" and t is COMMA,
	 EOS/SEMICOLON, OPEN_PAREN, SLASH, or CONCAT. */

      if (!ffesrc_is_name_init (*p))
	goto bad_i;		/* :::::::::::::::::::: */
      nt = ffelex_token_name_from_names (ffesta_tokens[0], i, 0);
      if (ffelex_token_type (t) == FFELEX_typeOPEN_PAREN)
	ffestb_local_.common.started = FALSE;
      else
	{
	  if (!ffesta_is_inhibited ())
	    ffestc_R547_start ();
	  ffestb_local_.common.started = TRUE;
	}
      next = (ffelexHandler) ffestb_R5471_ (nt);
      ffelex_token_kill (nt);
      return (ffelexHandler) (*next) (t);

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "COMMON", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "COMMON", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */

bad_i:				/* :::::::::::::::::::: */
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, "COMMON", ffesta_tokens[0], i, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R5471_ -- "COMMON"

   return ffestb_R5471_;  // to lexer

   Handle NAME, SLASH, or CONCAT.  */

static ffelexHandler
ffestb_R5471_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      return (ffelexHandler) ffestb_R5474_ (t);

    case FFELEX_typeSLASH:
      return (ffelexHandler) ffestb_R5472_;

    case FFELEX_typeCONCAT:
      if (!ffesta_is_inhibited ())
	ffestc_R547_item_cblock (NULL);
      return (ffelexHandler) ffestb_R5474_;

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "COMMON", t);
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_R547_finish ();
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R5472_ -- "COMMON" SLASH

   return ffestb_R5472_;  // to lexer

   Handle NAME.	 */

static ffelexHandler
ffestb_R5472_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_R5473_;

    case FFELEX_typeSLASH:
      if (!ffesta_is_inhibited ())
	ffestc_R547_item_cblock (NULL);
      return (ffelexHandler) ffestb_R5474_;

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "COMMON", t);
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_R547_finish ();
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R5473_ -- "COMMON" SLASH NAME

   return ffestb_R5473_;  // to lexer

   Handle SLASH.  */

static ffelexHandler
ffestb_R5473_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeSLASH:
      if (!ffesta_is_inhibited ())
	ffestc_R547_item_cblock (ffesta_tokens[1]);
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffestb_R5474_;

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "COMMON", t);
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_R547_finish ();
  ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R5474_ -- "COMMON" [SLASH NAME SLASH] or "COMMON" CONCAT

   return ffestb_R5474_;  // to lexer

   Handle NAME.	 */

static ffelexHandler
ffestb_R5474_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_R5475_;

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "COMMON", t);
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_R547_finish ();
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R5475_ -- "COMMON" ... NAME

   return ffestb_R5475_;  // to lexer

   Handle OPEN_PAREN.  */

static ffelexHandler
ffestb_R5475_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeOPEN_PAREN:
      ffestb_subrargs_.dim_list.dims = ffestt_dimlist_create ();
      ffestb_subrargs_.dim_list.handler = (ffelexHandler) ffestb_R5476_;
      ffestb_subrargs_.dim_list.pool = ffesta_output_pool;
      ffestb_subrargs_.dim_list.ctx = FFEEXPR_contextDIMLISTCOMMON;
#ifdef FFECOM_dimensionsMAX
      ffestb_subrargs_.dim_list.ndims = 0;
#endif
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
      FFEEXPR_contextDIMLISTCOMMON, (ffeexprCallback) ffestb_subr_dimlist_);

    case FFELEX_typeCOMMA:
      if (!ffesta_is_inhibited ())
	ffestc_R547_item_object (ffesta_tokens[1], NULL);
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffestb_R5477_;

    case FFELEX_typeSLASH:
    case FFELEX_typeCONCAT:
      if (!ffesta_is_inhibited ())
	ffestc_R547_item_object (ffesta_tokens[1], NULL);
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffestb_R5471_ (t);

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (!ffesta_is_inhibited ())
	{
	  ffestc_R547_item_object (ffesta_tokens[1], NULL);
	  ffestc_R547_finish ();
	}
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffesta_zero (t);

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "COMMON", t);
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_R547_finish ();
  ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R5476_ -- "COMMON" ... NAME OPEN_PAREN dimlist CLOSE_PAREN

   return ffestb_R5476_;  // to lexer

   Handle COMMA, SLASH, CONCAT, EOS/SEMICOLON.	*/

static ffelexHandler
ffestb_R5476_ (ffelexToken t)
{
  if (!ffestb_subrargs_.dim_list.ok)
    goto bad;			/* :::::::::::::::::::: */

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	{
	  if (!ffestb_local_.common.started)
	    {
	      ffestc_R547_start ();
	      ffestb_local_.common.started = TRUE;
	    }
	  ffestc_R547_item_object (ffesta_tokens[1],
				   ffestb_subrargs_.dim_list.dims);
	}
      ffelex_token_kill (ffesta_tokens[1]);
      ffestt_dimlist_kill (ffestb_subrargs_.dim_list.dims);
      return (ffelexHandler) ffestb_R5477_;

    case FFELEX_typeSLASH:
    case FFELEX_typeCONCAT:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	{
	  if (!ffestb_local_.common.started)
	    {
	      ffestc_R547_start ();
	      ffestb_local_.common.started = TRUE;
	    }
	  ffestc_R547_item_object (ffesta_tokens[1],
				   ffestb_subrargs_.dim_list.dims);
	}
      ffelex_token_kill (ffesta_tokens[1]);
      ffestt_dimlist_kill (ffestb_subrargs_.dim_list.dims);
      return (ffelexHandler) ffestb_R5471_ (t);

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	{
	  if (!ffestb_local_.common.started)
	    ffestc_R547_start ();
	  ffestc_R547_item_object (ffesta_tokens[1],
				   ffestb_subrargs_.dim_list.dims);
	  ffestc_R547_finish ();
	}
      ffelex_token_kill (ffesta_tokens[1]);
      ffestt_dimlist_kill (ffestb_subrargs_.dim_list.dims);
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

bad:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "COMMON", t);
  if (ffestb_local_.common.started && !ffesta_is_inhibited ())
    ffestc_R547_finish ();
  ffestt_dimlist_kill (ffestb_subrargs_.dim_list.dims);
  ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R5477_ -- "COMMON" ... COMMA

   return ffestb_R5477_;  // to lexer

   Make sure we don't have EOS or SEMICOLON.  */

static ffelexHandler
ffestb_R5477_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (!ffesta_is_inhibited ())
	ffestc_R547_finish ();
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "COMMON", t);
      return (ffelexHandler) ffesta_zero (t);

    default:
      return (ffelexHandler) ffestb_R5471_ (t);
    }
}

/* ffestb_R624 -- Parse a NULLIFY statement

   return ffestb_R624;	// to lexer

   Make sure the statement has a valid form for a NULLIFY
   statement.  If it does, implement the statement.

   31-May-90  JCB  2.0
      Rewrite to produce a list of expressions rather than just names; this
      eases semantic checking, putting it in expression handling where that
      kind of thing gets done anyway, and makes it easier to support more
      flexible extensions to Fortran 90 like NULLIFY(FOO%BAR).	*/

#if FFESTR_F90
ffelexHandler
ffestb_R624 (ffelexToken t)
{
  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstNULLIFY)
	goto bad_0;		/* :::::::::::::::::::: */
      break;

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstNULLIFY)
	goto bad_0;		/* :::::::::::::::::::: */
      if (ffelex_token_length (ffesta_tokens[0]) != FFESTR_firstlNULLIFY)
	goto bad_0;		/* :::::::::::::::::::: */
      break;

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeOPEN_PAREN:
      break;

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
    case FFELEX_typeCOMMA:
    case FFELEX_typeCOLONCOLON:
    case FFELEX_typeNAME:
      ffesta_confirmed ();	/* Error, but clearly intended. */
      goto bad_1;		/* :::::::::::::::::::: */

    default:
      goto bad_1;		/* :::::::::::::::::::: */
    }

  ffestb_local_.R624.exprs = ffestt_exprlist_create ();
  return (ffelexHandler) ffeexpr_lhs (ffesta_output_pool,
				      FFEEXPR_contextNULLIFY,
				      (ffeexprCallback) ffestb_R6241_);

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "NULLIFY", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "NULLIFY", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */
}

/* ffestb_R6241_ -- "NULLIFY" OPEN_PAREN expr

   return ffestb_R6241_;  // to lexer

   Make sure the statement has a valid form for a NULLIFY statement.  If it
   does, implement the statement.

   31-May-90  JCB  2.0
      Rewrite to produce a list of expressions rather than just names; this
      eases semantic checking, putting it in expression handling where that
      kind of thing gets done anyway, and makes it easier to support more
      flexible extensions to Fortran 90 like NULLIFY(FOO%BAR).	*/

static ffelexHandler
ffestb_R6241_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCLOSE_PAREN:
      if (expr == NULL)
	break;
      ffestt_exprlist_append (ffestb_local_.R624.exprs, expr,
			      ffelex_token_use (t));
      return (ffelexHandler) ffestb_R6242_;

    case FFELEX_typeCOMMA:
      if (expr == NULL)
	break;
      ffestt_exprlist_append (ffestb_local_.R624.exprs, expr,
			      ffelex_token_use (t));
      return (ffelexHandler) ffeexpr_lhs (ffesta_output_pool,
					  FFEEXPR_contextNULLIFY,
					  (ffeexprCallback) ffestb_R6241_);

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "NULLIFY", t);
  ffestt_exprlist_kill (ffestb_local_.R624.exprs);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R6242_ -- "NULLIFY" OPEN_PAREN expr-list CLOSE_PAREN

   return ffestb_R6242_;  // to lexer

   Make sure the statement has a valid form for a NULLIFY statement.  If it
   does, implement the statement.  */

static ffelexHandler
ffestb_R6242_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	ffestc_R624 (ffestb_local_.R624.exprs);
      ffestt_exprlist_kill (ffestb_local_.R624.exprs);
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "NULLIFY", t);
  ffestt_exprlist_kill (ffestb_local_.R624.exprs);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

#endif
/* ffestb_R1229 -- Parse a STMTFUNCTION statement

   return ffestb_R1229;	 // to lexer

   Make sure the statement has a valid form for a STMTFUNCTION
   statement.  If it does, implement the statement.  */

ffelexHandler
ffestb_R1229 (ffelexToken t)
{
  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
    case FFELEX_typeNAMES:
      break;

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeOPEN_PAREN:
      break;

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
    case FFELEX_typeCOMMA:
    case FFELEX_typeCOLONCOLON:
    case FFELEX_typeNAME:
      ffesta_confirmed ();	/* Error, but clearly intended. */
      goto bad_1;		/* :::::::::::::::::::: */

    default:
      goto bad_1;		/* :::::::::::::::::::: */
    }

  ffestb_subrargs_.name_list.args = ffestt_tokenlist_create ();
  ffestb_subrargs_.name_list.handler = (ffelexHandler) ffestb_R12291_;
  ffestb_subrargs_.name_list.is_subr = FALSE;	/* No "*" items in list! */
  ffestb_subrargs_.name_list.names = TRUE;	/* In case "IF(FOO)CALL
						   FOO...". */
  return (ffelexHandler) ffestb_subr_name_list_;

bad_0:				/* :::::::::::::::::::: */
bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_2t (FFEBAD_UNREC_STMT, ffesta_tokens[0], t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R12291_ -- "STMTFUNCTION" OPEN_PAREN dummy-name-list CLOSE_PAREN

   return ffestb_R12291_;  // to lexer

   Make sure the statement has a valid form for a STMTFUNCTION statement.  If
   it does, implement the statement.  */

static ffelexHandler
ffestb_R12291_ (ffelexToken t)
{
  ffelex_set_names (FALSE);

  if (!ffestb_subrargs_.name_list.ok)
    goto bad;			/* :::::::::::::::::::: */

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEQUALS:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	ffestc_R1229_start (ffesta_tokens[0],
			    ffestb_subrargs_.name_list.args,
			    ffestb_subrargs_.name_list.close_paren);
      ffelex_token_kill (ffestb_subrargs_.name_list.close_paren);
      ffestt_tokenlist_kill (ffestb_subrargs_.name_list.args);
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		 FFEEXPR_contextSFUNCDEF, (ffeexprCallback) ffestb_R12292_);

    default:
      break;
    }

bad:				/* :::::::::::::::::::: */
  ffesta_ffebad_2t (FFEBAD_UNREC_STMT, ffesta_tokens[0], t);
  ffelex_token_kill (ffestb_subrargs_.name_list.close_paren);
  ffestt_tokenlist_kill (ffestb_subrargs_.name_list.args);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_R12292_ -- "STMTFUNCTION" OPEN_PAREN dummy-name-list CLOSE_PAREN
		     EQUALS expr

   (ffestb_R12292_)  // to expression handler

   Make sure the statement has a valid form for a STMTFUNCTION statement.  If
   it does, implement the statement.  */

static ffelexHandler
ffestb_R12292_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  if (expr == NULL)
    goto bad;			/* :::::::::::::::::::: */

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (!ffesta_is_inhibited ())
	ffestc_R1229_finish (expr, ft);
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

bad:				/* :::::::::::::::::::: */
  ffestc_R1229_finish (NULL, NULL);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "statement-function-definition", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_chartype -- Parse the CHARACTER statement

   return ffestb_decl_chartype;	 // to lexer

   Make sure the statement has a valid form for the CHARACTER statement.  If
   it does, implement the statement.  */

ffelexHandler
ffestb_decl_chartype (ffelexToken t)
{
  ffeTokenLength i;
  unsigned const char *p;

  ffestb_local_.decl.type = FFESTP_typeCHARACTER;
  ffestb_local_.decl.recursive = NULL;
  ffestb_local_.decl.parameter = FALSE;	/* No PARAMETER attribute seen. */
  ffestb_local_.decl.coloncolon = FALSE;	/* No COLONCOLON seen. */

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstCHRCTR)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeCOMMA:
	  ffesta_confirmed ();
	  if (!ffesta_is_inhibited ())
	    ffestc_decl_start (ffestb_local_.decl.type, ffesta_tokens[0],
			       NULL, NULL, NULL, NULL);
	  return (ffelexHandler) ffestb_decl_attrs_;

	case FFELEX_typeCOLONCOLON:
	  ffestb_local_.decl.coloncolon = TRUE;
	  ffesta_confirmed ();
	  if (!ffesta_is_inhibited ())
	    ffestc_decl_start (ffestb_local_.decl.type, ffesta_tokens[0],
			       NULL, NULL, NULL, NULL);
	  return (ffelexHandler) ffestb_decl_ents_;

	case FFELEX_typeASTERISK:
	  ffesta_confirmed ();
	  ffestb_local_.decl.handler = (ffelexHandler) ffestb_decl_chartype1_;
	  ffestb_local_.decl.badname = "TYPEDECL";
	  return (ffelexHandler) ffestb_decl_starlen_;

	case FFELEX_typeOPEN_PAREN:
	  ffestb_local_.decl.kind = NULL;
	  ffestb_local_.decl.kindt = NULL;
	  ffestb_local_.decl.len = NULL;
	  ffestb_local_.decl.lent = NULL;
	  ffestb_local_.decl.handler = (ffelexHandler) ffestb_decl_attrsp_;
	  ffestb_local_.decl.badname = "_TYPEDECL";
	  return (ffelexHandler) ffestb_decl_typeparams_;

	case FFELEX_typeNAME:
	  ffesta_confirmed ();
	  ffestb_local_.decl.kind = NULL;
	  ffestb_local_.decl.kindt = NULL;
	  ffestb_local_.decl.len = NULL;
	  ffestb_local_.decl.lent = NULL;
	  return (ffelexHandler) ffestb_decl_entsp_ (t);
	}

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstCHRCTR)
	goto bad_0;		/* :::::::::::::::::::: */
      p = ffelex_token_text (ffesta_tokens[0]) + (i = FFESTR_firstlCHRCTR);
      switch (ffelex_token_type (t))
	{
	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  ffesta_confirmed ();
	  break;

	case FFELEX_typeCOMMA:
	  ffesta_confirmed ();
	  if (*p != '\0')
	    break;
	  if (!ffesta_is_inhibited ())
	    ffestc_decl_start (ffestb_local_.decl.type, ffesta_tokens[0],
			       NULL, NULL, NULL, NULL);
	  return (ffelexHandler) ffestb_decl_attrs_;

	case FFELEX_typeCOLONCOLON:
	  ffestb_local_.decl.coloncolon = TRUE;
	  ffesta_confirmed ();
	  if (*p != '\0')
	    goto bad_i;		/* :::::::::::::::::::: */
	  if (!ffesta_is_inhibited ())
	    ffestc_decl_start (ffestb_local_.decl.type, ffesta_tokens[0],
			       NULL, NULL, NULL, NULL);
	  return (ffelexHandler) ffestb_decl_ents_;

	case FFELEX_typeASTERISK:
	  ffesta_confirmed ();
	  if (*p != '\0')
	    break;
	  ffestb_local_.decl.handler = (ffelexHandler) ffestb_decl_chartype1_;
	  ffestb_local_.decl.badname = "TYPEDECL";
	  return (ffelexHandler) ffestb_decl_starlen_;

	case FFELEX_typeSLASH:
	  ffesta_confirmed ();
	  if (*p != '\0')
	    break;
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeOPEN_PAREN:
	  if (*p != '\0')
	    break;
	  ffestb_local_.decl.kind = NULL;
	  ffestb_local_.decl.kindt = NULL;
	  ffestb_local_.decl.len = NULL;
	  ffestb_local_.decl.lent = NULL;
	  ffestb_local_.decl.handler = (ffelexHandler) ffestb_decl_attrsp_;
	  ffestb_local_.decl.badname = "TYPEDECL";
	  return (ffelexHandler) ffestb_decl_typeparams_;
	}
      if (!ffesrc_is_name_init (*p))
	goto bad_i;		/* :::::::::::::::::::: */
      ffestb_local_.decl.kind = NULL;
      ffestb_local_.decl.kindt = NULL;
      ffestb_local_.decl.len = NULL;
      ffestb_local_.decl.lent = NULL;
      ffesta_tokens[1] = ffelex_token_names_from_names (ffesta_tokens[0], i, 0);
      return (ffelexHandler) ffestb_decl_entsp_2_ (t);

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "type-declaration", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "type-declaration", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */

bad_i:				/* :::::::::::::::::::: */
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, "type-declaration", ffesta_tokens[0], i, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_chartype1_ -- "CHARACTER" ASTERISK char-length

   return ffestb_decl_chartype1_;  // to lexer

   Handle COMMA, COLONCOLON, or anything else.	*/

static ffelexHandler
ffestb_decl_chartype1_ (ffelexToken t)
{
  ffelex_set_names (FALSE);

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOLONCOLON:
      ffestb_local_.decl.coloncolon = TRUE;
      /* Fall through. */
    case FFELEX_typeCOMMA:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	ffestc_decl_start (ffestb_local_.decl.type, ffesta_tokens[0],
	       NULL, NULL, ffestb_local_.decl.len, ffestb_local_.decl.lent);
      if (ffestb_local_.decl.lent != NULL)
	ffelex_token_kill (ffestb_local_.decl.lent);
      return (ffelexHandler) ffestb_decl_ents_;

    default:
      return (ffelexHandler) ffestb_decl_entsp_ (t);
    }
}

/* ffestb_decl_dbltype -- Parse the DOUBLEPRECISION/DOUBLECOMPLEX statement

   return ffestb_decl_dbltype;	// to lexer

   Make sure the statement has a valid form for the DOUBLEPRECISION/
   DOUBLECOMPLEX statement.  If it does, implement the statement.  */

ffelexHandler
ffestb_decl_dbltype (ffelexToken t)
{
  ffeTokenLength i;
  unsigned const char *p;

  ffestb_local_.decl.type = ffestb_args.decl.type;
  ffestb_local_.decl.recursive = NULL;
  ffestb_local_.decl.parameter = FALSE;	/* No PARAMETER attribute seen. */
  ffestb_local_.decl.coloncolon = FALSE;	/* No COLONCOLON seen. */

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeCOMMA:
	  ffesta_confirmed ();
	  if (!ffesta_is_inhibited ())
	    ffestc_decl_start (ffestb_local_.decl.type, ffesta_tokens[0],
			       NULL, NULL, NULL, NULL);
	  return (ffelexHandler) ffestb_decl_attrs_;

	case FFELEX_typeCOLONCOLON:
	  ffestb_local_.decl.coloncolon = TRUE;
	  ffesta_confirmed ();
	  if (!ffesta_is_inhibited ())
	    ffestc_decl_start (ffestb_local_.decl.type, ffesta_tokens[0],
			       NULL, NULL, NULL, NULL);
	  return (ffelexHandler) ffestb_decl_ents_;

	case FFELEX_typeNAME:
	  ffesta_confirmed ();
	  ffestb_local_.decl.kind = NULL;
	  ffestb_local_.decl.kindt = NULL;
	  ffestb_local_.decl.len = NULL;
	  ffestb_local_.decl.lent = NULL;
	  return (ffelexHandler) ffestb_decl_entsp_ (t);
	}

    case FFELEX_typeNAMES:
      p = ffelex_token_text (ffesta_tokens[0]) + (i = ffestb_args.decl.len);
      switch (ffelex_token_type (t))
	{
	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  ffesta_confirmed ();
	  break;

	case FFELEX_typeCOMMA:
	  ffesta_confirmed ();
	  if (*p != '\0')
	    break;
	  if (!ffesta_is_inhibited ())
	    ffestc_decl_start (ffestb_local_.decl.type, ffesta_tokens[0],
			       NULL, NULL, NULL, NULL);
	  return (ffelexHandler) ffestb_decl_attrs_;

	case FFELEX_typeCOLONCOLON:
	  ffestb_local_.decl.coloncolon = TRUE;
	  ffesta_confirmed ();
	  if (*p != '\0')
	    goto bad_i;		/* :::::::::::::::::::: */
	  if (!ffesta_is_inhibited ())
	    ffestc_decl_start (ffestb_local_.decl.type, ffesta_tokens[0],
			       NULL, NULL, NULL, NULL);
	  return (ffelexHandler) ffestb_decl_ents_;

	case FFELEX_typeSLASH:
	  ffesta_confirmed ();
	  if (*p != '\0')
	    break;
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeOPEN_PAREN:
	  if (*p != '\0')
	    break;
	  goto bad_1;		/* :::::::::::::::::::: */
	}
      if (!ffesrc_is_name_init (*p))
	goto bad_i;		/* :::::::::::::::::::: */
      ffestb_local_.decl.kind = NULL;
      ffestb_local_.decl.kindt = NULL;
      ffestb_local_.decl.len = NULL;
      ffestb_local_.decl.lent = NULL;
      ffesta_tokens[1] = ffelex_token_names_from_names (ffesta_tokens[0], i, 0);
      return (ffelexHandler) ffestb_decl_entsp_2_ (t);

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "type-declaration", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "type-declaration", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */

bad_i:				/* :::::::::::::::::::: */
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, "type-declaration", ffesta_tokens[0], i, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_double -- Parse the DOUBLE PRECISION/DOUBLE COMPLEX statement

   return ffestb_decl_double;  // to lexer

   Make sure the statement has a valid form for the DOUBLE PRECISION/
   DOUBLE COMPLEX statement.  If it does, implement the statement.  */

ffelexHandler
ffestb_decl_double (ffelexToken t)
{
  ffestb_local_.decl.recursive = NULL;
  ffestb_local_.decl.parameter = FALSE;	/* No PARAMETER attribute seen. */
  ffestb_local_.decl.coloncolon = FALSE;	/* No COLONCOLON seen. */

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstDBL)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeNAME:
	  ffesta_confirmed ();
	  switch (ffestr_second (t))
	    {
	    case FFESTR_secondCOMPLEX:
	      ffestb_local_.decl.type = FFESTP_typeDBLCMPLX;
	      break;

	    case FFESTR_secondPRECISION:
	      ffestb_local_.decl.type = FFESTP_typeDBLPRCSN;
	      break;

	    default:
	      goto bad_1;	/* :::::::::::::::::::: */
	    }
	  ffestb_local_.decl.kind = NULL;
	  ffestb_local_.decl.kindt = NULL;
	  ffestb_local_.decl.len = NULL;
	  ffestb_local_.decl.lent = NULL;
	  return (ffelexHandler) ffestb_decl_attrsp_;
	}

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "type-declaration", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "type-declaration", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */
}

/* ffestb_decl_gentype -- Parse the INTEGER/REAL/COMPLEX/LOGICAL statement

   return ffestb_decl_gentype;	// to lexer

   Make sure the statement has a valid form for the INTEGER/REAL/COMPLEX/
   LOGICAL statement.  If it does, implement the statement.  */

ffelexHandler
ffestb_decl_gentype (ffelexToken t)
{
  ffeTokenLength i;
  unsigned const char *p;

  ffestb_local_.decl.type = ffestb_args.decl.type;
  ffestb_local_.decl.recursive = NULL;
  ffestb_local_.decl.parameter = FALSE;	/* No PARAMETER attribute seen. */
  ffestb_local_.decl.coloncolon = FALSE;	/* No COLONCOLON seen. */

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeCOMMA:
	  ffesta_confirmed ();
	  if (!ffesta_is_inhibited ())
	    ffestc_decl_start (ffestb_local_.decl.type, ffesta_tokens[0],
			       NULL, NULL, NULL, NULL);
	  return (ffelexHandler) ffestb_decl_attrs_;

	case FFELEX_typeCOLONCOLON:
	  ffestb_local_.decl.coloncolon = TRUE;
	  ffesta_confirmed ();
	  if (!ffesta_is_inhibited ())
	    ffestc_decl_start (ffestb_local_.decl.type, ffesta_tokens[0],
			       NULL, NULL, NULL, NULL);
	  return (ffelexHandler) ffestb_decl_ents_;

	case FFELEX_typeASTERISK:
	  ffesta_confirmed ();
	  ffestb_local_.decl.handler = (ffelexHandler) ffestb_decl_attrsp_;
	  ffestb_local_.decl.badname = "TYPEDECL";
	  return (ffelexHandler) ffestb_decl_starkind_;

	case FFELEX_typeOPEN_PAREN:
	  ffestb_local_.decl.handler = (ffelexHandler) ffestb_decl_attrsp_;
	  ffestb_local_.decl.badname = "TYPEDECL";
	  return (ffelexHandler) ffestb_decl_kindparam_;

	case FFELEX_typeNAME:
	  ffesta_confirmed ();
	  ffestb_local_.decl.kind = NULL;
	  ffestb_local_.decl.kindt = NULL;
	  ffestb_local_.decl.len = NULL;
	  ffestb_local_.decl.lent = NULL;
	  return (ffelexHandler) ffestb_decl_entsp_ (t);
	}

    case FFELEX_typeNAMES:
      p = ffelex_token_text (ffesta_tokens[0]) + (i = ffestb_args.decl.len);
      switch (ffelex_token_type (t))
	{
	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  ffesta_confirmed ();
	  break;

	case FFELEX_typeCOMMA:
	  ffesta_confirmed ();
	  if (*p != '\0')
	    break;
	  if (!ffesta_is_inhibited ())
	    ffestc_decl_start (ffestb_local_.decl.type, ffesta_tokens[0],
			       NULL, NULL, NULL, NULL);
	  return (ffelexHandler) ffestb_decl_attrs_;

	case FFELEX_typeCOLONCOLON:
	  ffestb_local_.decl.coloncolon = TRUE;
	  ffesta_confirmed ();
	  if (*p != '\0')
	    goto bad_i;		/* :::::::::::::::::::: */
	  if (!ffesta_is_inhibited ())
	    ffestc_decl_start (ffestb_local_.decl.type, ffesta_tokens[0],
			       NULL, NULL, NULL, NULL);
	  return (ffelexHandler) ffestb_decl_ents_;

	case FFELEX_typeSLASH:
	  ffesta_confirmed ();
	  if (*p != '\0')
	    break;
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeASTERISK:
	  ffesta_confirmed ();
	  if (*p != '\0')
	    break;
	  ffestb_local_.decl.handler = (ffelexHandler) ffestb_decl_attrsp_;
	  ffestb_local_.decl.badname = "TYPEDECL";
	  return (ffelexHandler) ffestb_decl_starkind_;

	case FFELEX_typeOPEN_PAREN:
	  if (*p != '\0')
	    break;
	  ffestb_local_.decl.handler = (ffelexHandler) ffestb_decl_attrsp_;
	  ffestb_local_.decl.badname = "TYPEDECL";
	  return (ffelexHandler) ffestb_decl_kindparam_;
	}
      if (!ffesrc_is_name_init (*p))
	goto bad_i;		/* :::::::::::::::::::: */
      ffestb_local_.decl.kind = NULL;
      ffestb_local_.decl.kindt = NULL;
      ffestb_local_.decl.len = NULL;
      ffestb_local_.decl.lent = NULL;
      ffesta_tokens[1] = ffelex_token_names_from_names (ffesta_tokens[0], i, 0);
      return (ffelexHandler) ffestb_decl_entsp_2_ (t);

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "type-declaration", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "type-declaration", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */

bad_i:				/* :::::::::::::::::::: */
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, "type-declaration", ffesta_tokens[0], i, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_recursive -- Parse the RECURSIVE FUNCTION statement

   return ffestb_decl_recursive;  // to lexer

   Make sure the statement has a valid form for the RECURSIVE FUNCTION
   statement.  If it does, implement the statement.  */

#if FFESTR_F90
ffelexHandler
ffestb_decl_recursive (ffelexToken t)
{
  ffeTokenLength i;
  const char *p;
  ffelexToken nt;
  ffelexToken ot;
  ffelexHandler next;
  bool needfunc;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstRECURSIVE)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeNAME:
	  break;
	}
      ffesta_confirmed ();
      ffestb_local_.decl.recursive = ffelex_token_use (ffesta_tokens[0]);
      switch (ffesta_second_kw)
	{
	case FFESTR_secondINTEGER:
	  ffestb_local_.decl.type = FFESTP_typeINTEGER;
	  return (ffelexHandler) ffestb_decl_recursive1_;

	case FFESTR_secondBYTE:
	  ffestb_local_.decl.type = FFESTP_typeBYTE;
	  return (ffelexHandler) ffestb_decl_recursive1_;

	case FFESTR_secondWORD:
	  ffestb_local_.decl.type = FFESTP_typeWORD;
	  return (ffelexHandler) ffestb_decl_recursive1_;

	case FFESTR_secondREAL:
	  ffestb_local_.decl.type = FFESTP_typeREAL;
	  return (ffelexHandler) ffestb_decl_recursive1_;

	case FFESTR_secondCOMPLEX:
	  ffestb_local_.decl.type = FFESTP_typeCOMPLEX;
	  return (ffelexHandler) ffestb_decl_recursive1_;

	case FFESTR_secondLOGICAL:
	  ffestb_local_.decl.type = FFESTP_typeLOGICAL;
	  return (ffelexHandler) ffestb_decl_recursive1_;

	case FFESTR_secondCHARACTER:
	  ffestb_local_.decl.type = FFESTP_typeCHARACTER;
	  return (ffelexHandler) ffestb_decl_recursive1_;

	case FFESTR_secondDOUBLE:
	  return (ffelexHandler) ffestb_decl_recursive2_;

	case FFESTR_secondDOUBLEPRECISION:
	  ffestb_local_.decl.type = FFESTP_typeDBLPRCSN;
	  ffestb_local_.decl.kind = NULL;
	  ffestb_local_.decl.kindt = NULL;
	  ffestb_local_.decl.len = NULL;
	  ffestb_local_.decl.lent = NULL;
	  return (ffelexHandler) ffestb_decl_func_;

	case FFESTR_secondDOUBLECOMPLEX:
	  ffestb_local_.decl.type = FFESTP_typeDBLCMPLX;
	  ffestb_local_.decl.kind = NULL;
	  ffestb_local_.decl.kindt = NULL;
	  ffestb_local_.decl.len = NULL;
	  ffestb_local_.decl.lent = NULL;
	  return (ffelexHandler) ffestb_decl_func_;

	case FFESTR_secondTYPE:
	  ffestb_local_.decl.type = FFESTP_typeTYPE;
	  return (ffelexHandler) ffestb_decl_recursive3_;

	case FFESTR_secondFUNCTION:
	  ffestb_local_.dummy.first_kw = FFESTR_firstFUNCTION;
	  ffestb_local_.dummy.badname = "FUNCTION";
	  ffestb_local_.dummy.is_subr = FALSE;
	  return (ffelexHandler) ffestb_decl_recursive4_;

	case FFESTR_secondSUBROUTINE:
	  ffestb_local_.dummy.first_kw = FFESTR_firstSUBROUTINE;
	  ffestb_local_.dummy.badname = "SUBROUTINE";
	  ffestb_local_.dummy.is_subr = TRUE;
	  return (ffelexHandler) ffestb_decl_recursive4_;

	default:
	  ffelex_token_kill (ffestb_local_.decl.recursive);
	  goto bad_1;		/* :::::::::::::::::::: */
	}

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstRECURSIVE)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	case FFELEX_typeASTERISK:
	case FFELEX_typeSEMICOLON:
	case FFELEX_typeEOS:
	  ffesta_confirmed ();
	  break;

	default:
	  break;
	}
      p = ffelex_token_text (ffesta_tokens[0]) + (i = FFESTR_firstlRECURSIVE);
      if (!ffesrc_is_name_init (*p))
	goto bad_0;		/* :::::::::::::::::::: */
      ffestb_local_.decl.recursive
	= ffelex_token_name_from_names (ffesta_tokens[0], 0,
					FFESTR_firstlRECURSIVE);
      nt = ffelex_token_names_from_names (ffesta_tokens[0],
					  FFESTR_firstlRECURSIVE, 0);
      switch (ffestr_first (nt))
	{
	case FFESTR_firstINTGR:
	  p = ffelex_token_text (nt) + (i = FFESTR_firstlINTGR);
	  ffestb_local_.decl.type = FFESTP_typeINTEGER;
	  needfunc = FALSE;
	  goto typefunc;	/* :::::::::::::::::::: */

	case FFESTR_firstBYTE:
	  p = ffelex_token_text (nt) + (i = FFESTR_firstlBYTE);
	  ffestb_local_.decl.type = FFESTP_typeBYTE;
	  needfunc = FALSE;
	  goto typefunc;	/* :::::::::::::::::::: */

	case FFESTR_firstWORD:
	  p = ffelex_token_text (nt) + (i = FFESTR_firstlWORD);
	  ffestb_local_.decl.type = FFESTP_typeWORD;
	  needfunc = FALSE;
	  goto typefunc;	/* :::::::::::::::::::: */

	case FFESTR_firstREAL:
	  p = ffelex_token_text (nt) + (i = FFESTR_firstlREAL);
	  ffestb_local_.decl.type = FFESTP_typeREAL;
	  needfunc = FALSE;
	  goto typefunc;	/* :::::::::::::::::::: */

	case FFESTR_firstCMPLX:
	  p = ffelex_token_text (nt) + (i = FFESTR_firstlCMPLX);
	  ffestb_local_.decl.type = FFESTP_typeCOMPLEX;
	  needfunc = FALSE;
	  goto typefunc;	/* :::::::::::::::::::: */

	case FFESTR_firstLGCL:
	  p = ffelex_token_text (nt) + (i = FFESTR_firstlLGCL);
	  ffestb_local_.decl.type = FFESTP_typeLOGICAL;
	  needfunc = FALSE;
	  goto typefunc;	/* :::::::::::::::::::: */

	case FFESTR_firstCHRCTR:
	  p = ffelex_token_text (nt) + (i = FFESTR_firstlCHRCTR);
	  ffestb_local_.decl.type = FFESTP_typeCHARACTER;
	  needfunc = FALSE;
	  goto typefunc;	/* :::::::::::::::::::: */

	case FFESTR_firstDBLPRCSN:
	  p = ffelex_token_text (nt) + (i = FFESTR_firstlDBLPRCSN);
	  ffestb_local_.decl.type = FFESTP_typeDBLPRCSN;
	  needfunc = TRUE;
	  goto typefunc;	/* :::::::::::::::::::: */

	case FFESTR_firstDBLCMPLX:
	  p = ffelex_token_text (nt) + (i = FFESTR_firstlDBLCMPLX);
	  ffestb_local_.decl.type = FFESTP_typeDBLCMPLX;
	  needfunc = TRUE;
	  goto typefunc;	/* :::::::::::::::::::: */

	case FFESTR_firstTYPE:
	  p = ffelex_token_text (nt) + (i = FFESTR_firstlTYPE);
	  ffestb_local_.decl.type = FFESTP_typeTYPE;
	  next = (ffelexHandler) ffestb_decl_recursive3_;
	  break;

	case FFESTR_firstFUNCTION:
	  p = ffelex_token_text (nt) + (i = FFESTR_firstlFUNCTION);
	  ffestb_local_.dummy.first_kw = FFESTR_firstFUNCTION;
	  ffestb_local_.dummy.badname = "FUNCTION";
	  ffestb_local_.dummy.is_subr = FALSE;
	  next = (ffelexHandler) ffestb_decl_recursive4_;
	  break;

	case FFESTR_firstSUBROUTINE:
	  p = ffelex_token_text (nt) + (i = FFESTR_firstlSUBROUTINE);
	  ffestb_local_.dummy.first_kw = FFESTR_firstSUBROUTINE;
	  ffestb_local_.dummy.badname = "SUBROUTINE";
	  ffestb_local_.dummy.is_subr = TRUE;
	  next = (ffelexHandler) ffestb_decl_recursive4_;
	  break;

	default:
	  ffelex_token_kill (ffestb_local_.decl.recursive);
	  ffelex_token_kill (nt);
	  goto bad_1;		/* :::::::::::::::::::: */
	}
      if (*p == '\0')
	{
	  ffelex_token_kill (nt);
	  return (ffelexHandler) (*next) (t);
	}
      if (!ffesrc_is_name_init (*p))
	goto bad_i;		/* :::::::::::::::::::: */
      ot = ffelex_token_name_from_names (nt, i, 0);
      ffelex_token_kill (nt);
      next = (ffelexHandler) (*next) (ot);
      ffelex_token_kill (ot);
      return (ffelexHandler) (*next) (t);

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

typefunc:			/* :::::::::::::::::::: */
  if (*p == '\0')
    {
      ffelex_token_kill (nt);
      if (needfunc)		/* DOUBLE PRECISION or DOUBLE COMPLEX? */
	{
	  ffelex_token_kill (ffestb_local_.decl.recursive);
	  goto bad_1;		/* :::::::::::::::::::: */
	}
      return (ffelexHandler) ffestb_decl_recursive1_ (t);
    }
  if (!ffesrc_is_name_init (*p))
    goto bad_i;			/* :::::::::::::::::::: */
  ot = ffelex_token_names_from_names (nt, i, 0);
  ffelex_token_kill (nt);
  if (ffestr_first (ot) != FFESTR_firstFUNCTION)
    goto bad_o;			/* :::::::::::::::::::: */
  p = ffelex_token_text (ot) + (i = FFESTR_firstlFUNCTION);
  if (!ffesrc_is_name_init (*p))
    goto bad_i;			/* :::::::::::::::::::: */
  ffesta_tokens[1] = ffelex_token_name_from_names (ot, i, 0);
  ffelex_token_kill (ot);
  ffestb_local_.decl.kind = NULL;
  ffestb_local_.decl.kindt = NULL;
  ffestb_local_.decl.len = NULL;
  ffestb_local_.decl.lent = NULL;
  return (ffelexHandler) ffestb_decl_funcname_1_ (t);

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FUNCTION", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FUNCTION", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */

bad_i:				/* :::::::::::::::::::: */
  ffelex_token_kill (ffestb_local_.decl.recursive);
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, "type-declaration", nt, i, t);
  ffelex_token_kill (nt);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_o:				/* :::::::::::::::::::: */
  ffelex_token_kill (ffestb_local_.decl.recursive);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FUNCTION", ot);
  ffelex_token_kill (ot);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_recursive1_ -- "RECURSIVE" generic-type

   return ffestb_decl_recursive1_;  // to lexer

   Handle ASTERISK, OPEN_PAREN, or NAME.  */

static ffelexHandler
ffestb_decl_recursive1_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeASTERISK:
      ffesta_confirmed ();
      ffestb_local_.decl.handler = (ffelexHandler) ffestb_decl_func_;
      ffestb_local_.decl.badname = "TYPEFUNC";
      if (ffestb_local_.decl.type == FFESTP_typeCHARACTER)
	return (ffelexHandler) ffestb_decl_starlen_;
      return (ffelexHandler) ffestb_decl_starkind_;

    case FFELEX_typeOPEN_PAREN:
      ffestb_local_.decl.handler = (ffelexHandler) ffestb_decl_func_;
      ffestb_local_.decl.badname = "TYPEFUNC";
      if (ffestb_local_.decl.type == FFESTP_typeCHARACTER)
	{
	  ffestb_local_.decl.kind = NULL;
	  ffestb_local_.decl.kindt = NULL;
	  ffestb_local_.decl.len = NULL;
	  ffestb_local_.decl.lent = NULL;
	  return (ffelexHandler) ffestb_decl_typeparams_;
	}
      return (ffelexHandler) ffestb_decl_kindparam_;

    case FFELEX_typeNAME:
      ffestb_local_.decl.kind = NULL;
      ffestb_local_.decl.kindt = NULL;
      ffestb_local_.decl.len = NULL;
      ffestb_local_.decl.lent = NULL;
      return (ffelexHandler) ffestb_decl_func_ (t);

    default:
      break;
    }

  if (ffestb_local_.decl.recursive != NULL)
    ffelex_token_kill (ffestb_local_.decl.recursive);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FUNCTION", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_recursive2_ -- "RECURSIVE" "DOUBLE"

   return ffestb_decl_recursive2_;  // to lexer

   Handle NAME.	 */

static ffelexHandler
ffestb_decl_recursive2_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      switch (ffestr_second (t))
	{
	case FFESTR_secondPRECISION:
	  ffestb_local_.decl.type = FFESTP_typeDBLPRCSN;
	  break;

	case FFESTR_secondCOMPLEX:
	  ffestb_local_.decl.type = FFESTP_typeDBLCMPLX;
	  break;

	default:
	  goto bad;		/* :::::::::::::::::::: */
	}
      ffestb_local_.decl.kind = NULL;
      ffestb_local_.decl.kindt = NULL;
      ffestb_local_.decl.len = NULL;
      ffestb_local_.decl.lent = NULL;
      return (ffelexHandler) ffestb_decl_func_;

    default:
      break;
    }

bad:				/* :::::::::::::::::::: */
  if (ffestb_local_.decl.recursive != NULL)
    ffelex_token_kill (ffestb_local_.decl.recursive);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FUNCTION", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_recursive3_ -- "RECURSIVE" "TYPE"

   return ffestb_decl_recursive3_;  // to lexer

   Handle OPEN_PAREN.  */

static ffelexHandler
ffestb_decl_recursive3_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeOPEN_PAREN:
      ffestb_local_.decl.handler = (ffelexHandler) ffestb_decl_func_;
      ffestb_local_.decl.badname = "TYPEFUNC";
      return (ffelexHandler) ffestb_decl_typetype1_;

    default:
      break;
    }

  if (ffestb_local_.decl.recursive != NULL)
    ffelex_token_kill (ffestb_local_.decl.recursive);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FUNCTION", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_recursive4_ -- "RECURSIVE" "FUNCTION/SUBROUTINE"

   return ffestb_decl_recursive4_;  // to lexer

   Handle OPEN_PAREN.  */

static ffelexHandler
ffestb_decl_recursive4_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_dummy1_;

    default:
      break;
    }

  if (ffestb_local_.decl.recursive != NULL)
    ffelex_token_kill (ffestb_local_.decl.recursive);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FUNCTION", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

#endif
/* ffestb_decl_typetype -- Parse the R426/R501/R1219 TYPE statement

   return ffestb_decl_typetype;	 // to lexer

   Make sure the statement has a valid form for the TYPE statement.  If it
   does, implement the statement.  */

#if FFESTR_F90
ffelexHandler
ffestb_decl_typetype (ffelexToken t)
{
  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstTYPE)
	goto bad_0;		/* :::::::::::::::::::: */
      break;

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstTYPE)
	goto bad_0;		/* :::::::::::::::::::: */
      if (ffelex_token_length (ffesta_tokens[0]) != FFESTR_firstlTYPE)
	goto bad_0;		/* :::::::::::::::::::: */
      break;

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeOPEN_PAREN:
      break;

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
    case FFELEX_typeCOLONCOLON:/* Not COMMA: R424 "TYPE,PUBLIC::A". */
      ffesta_confirmed ();	/* Error, but clearly intended. */
      goto bad_1;		/* :::::::::::::::::::: */

    default:
      goto bad_1;		/* :::::::::::::::::::: */
    }

  ffestb_local_.decl.recursive = NULL;
  ffestb_local_.decl.parameter = FALSE;	/* No PARAMETER attribute seen. */
  ffestb_local_.decl.coloncolon = FALSE;	/* No COLONCOLON seen. */

  ffestb_local_.decl.handler = (ffelexHandler) ffestb_decl_attrsp_;
  ffestb_local_.decl.badname = "type-declaration";
  return (ffelexHandler) ffestb_decl_typetype1_;

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "type-declaration", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "type-declaration", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */
}

#endif
/* ffestb_decl_attrs_ -- "type" [type parameters] COMMA

   return ffestb_decl_attrs_;  // to lexer

   Handle NAME of an attribute.	 */

static ffelexHandler
ffestb_decl_attrs_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      switch (ffestr_first (t))
	{
#if FFESTR_F90
	case FFESTR_firstALLOCATABLE:
	  if (!ffesta_is_inhibited ())
	    ffestc_decl_attrib (FFESTP_attribALLOCATABLE, t,
				FFESTR_otherNone, NULL);
	  return (ffelexHandler) ffestb_decl_attrs_7_;
#endif

	case FFESTR_firstDIMENSION:
	  ffesta_tokens[1] = ffelex_token_use (t);
	  return (ffelexHandler) ffestb_decl_attrs_1_;

	case FFESTR_firstEXTERNAL:
	  if (!ffesta_is_inhibited ())
	    ffestc_decl_attrib (FFESTP_attribEXTERNAL, t,
				FFESTR_otherNone, NULL);
	  return (ffelexHandler) ffestb_decl_attrs_7_;

#if FFESTR_F90
	case FFESTR_firstINTENT:
	  ffesta_tokens[1] = ffelex_token_use (t);
	  return (ffelexHandler) ffestb_decl_attrs_3_;
#endif

	case FFESTR_firstINTRINSIC:
	  if (!ffesta_is_inhibited ())
	    ffestc_decl_attrib (FFESTP_attribINTRINSIC, t,
				FFESTR_otherNone, NULL);
	  return (ffelexHandler) ffestb_decl_attrs_7_;

#if FFESTR_F90
	case FFESTR_firstOPTIONAL:
	  if (!ffesta_is_inhibited ())
	    ffestc_decl_attrib (FFESTP_attribOPTIONAL, t,
				FFESTR_otherNone, NULL);
	  return (ffelexHandler) ffestb_decl_attrs_7_;
#endif

	case FFESTR_firstPARAMETER:
	  ffestb_local_.decl.parameter = TRUE;
	  if (!ffesta_is_inhibited ())
	    ffestc_decl_attrib (FFESTP_attribPARAMETER, t,
				FFESTR_otherNone, NULL);
	  return (ffelexHandler) ffestb_decl_attrs_7_;

#if FFESTR_F90
	case FFESTR_firstPOINTER:
	  if (!ffesta_is_inhibited ())
	    ffestc_decl_attrib (FFESTP_attribPOINTER, t,
				FFESTR_otherNone, NULL);
	  return (ffelexHandler) ffestb_decl_attrs_7_;
#endif

#if FFESTR_F90
	case FFESTR_firstPRIVATE:
	  if (!ffesta_is_inhibited ())
	    ffestc_decl_attrib (FFESTP_attribPRIVATE, t,
				FFESTR_otherNone, NULL);
	  return (ffelexHandler) ffestb_decl_attrs_7_;

	case FFESTR_firstPUBLIC:
	  if (!ffesta_is_inhibited ())
	    ffestc_decl_attrib (FFESTP_attribPUBLIC, t,
				FFESTR_otherNone, NULL);
	  return (ffelexHandler) ffestb_decl_attrs_7_;
#endif

	case FFESTR_firstSAVE:
	  if (!ffesta_is_inhibited ())
	    ffestc_decl_attrib (FFESTP_attribSAVE, t,
				FFESTR_otherNone, NULL);
	  return (ffelexHandler) ffestb_decl_attrs_7_;

#if FFESTR_F90
	case FFESTR_firstTARGET:
	  if (!ffesta_is_inhibited ())
	    ffestc_decl_attrib (FFESTP_attribTARGET, t,
				FFESTR_otherNone, NULL);
	  return (ffelexHandler) ffestb_decl_attrs_7_;
#endif

	default:
	  ffesta_ffebad_1t (FFEBAD_INVALID_TYPEDECL_ATTR, t);
	  return (ffelexHandler) ffestb_decl_attrs_7_;
	}
      break;

    default:
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_decl_finish ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "type-declaration", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_attrs_1_ -- "type" [type parameters] ",DIMENSION"

   return ffestb_decl_attrs_1_;	 // to lexer

   Handle OPEN_PAREN.  */

static ffelexHandler
ffestb_decl_attrs_1_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeOPEN_PAREN:
      ffestb_subrargs_.dim_list.dims = ffestt_dimlist_create ();
      ffestb_subrargs_.dim_list.handler = (ffelexHandler) ffestb_decl_attrs_2_;
      ffestb_subrargs_.dim_list.pool = ffesta_scratch_pool;
      ffestb_subrargs_.dim_list.ctx = ffesta_is_entry_valid
	? FFEEXPR_contextDIMLIST : FFEEXPR_contextDIMLISTCOMMON;
#ifdef FFECOM_dimensionsMAX
      ffestb_subrargs_.dim_list.ndims = 0;
#endif
      return (ffelexHandler) ffeexpr_rhs (ffesta_scratch_pool,
					  ffestb_subrargs_.dim_list.ctx,
				    (ffeexprCallback) ffestb_subr_dimlist_);

    case FFELEX_typeCOMMA:
    case FFELEX_typeCOLONCOLON:
      ffesta_ffebad_1t (FFEBAD_INVALID_TYPEDECL_ATTR, ffesta_tokens[1]);
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffestb_decl_attrs_7_ (t);

    default:
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_decl_finish ();
  ffelex_token_kill (ffesta_tokens[1]);
  ffesta_ffebad_1t (FFEBAD_INVALID_TYPEDECL_ATTR, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_attrs_2_ -- "type" [type parameters] ",DIMENSION" OPEN_PAREN
			  dimlist CLOSE_PAREN

   return ffestb_decl_attrs_2_;	 // to lexer

   Handle COMMA or COLONCOLON.	*/

static ffelexHandler
ffestb_decl_attrs_2_ (ffelexToken t)
{
  if (!ffestb_subrargs_.dim_list.ok)
    goto bad;			/* :::::::::::::::::::: */

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
    case FFELEX_typeCOLONCOLON:
      if (!ffesta_is_inhibited ())
	ffestc_decl_attrib (FFESTP_attribDIMENSION, ffesta_tokens[1],
			  FFESTR_otherNone, ffestb_subrargs_.dim_list.dims);
      ffelex_token_kill (ffesta_tokens[1]);
      ffestt_dimlist_kill (ffestb_subrargs_.dim_list.dims);
      return (ffelexHandler) ffestb_decl_attrs_7_ (t);

    default:
      break;
    }

bad:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "type-declaration", t);
  if (!ffesta_is_inhibited ())
    ffestc_decl_finish ();
  ffestt_dimlist_kill (ffestb_subrargs_.dim_list.dims);
  ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_attrs_3_ -- "type" [type parameters] ",INTENT"

   return ffestb_decl_attrs_3_;	 // to lexer

   Handle OPEN_PAREN.  */

#if FFESTR_F90
static ffelexHandler
ffestb_decl_attrs_3_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeOPEN_PAREN:
      return (ffelexHandler) ffestb_decl_attrs_4_;

    case FFELEX_typeCOMMA:
    case FFELEX_typeCOLONCOLON:
      ffesta_ffebad_1t (FFEBAD_INVALID_TYPEDECL_ATTR, ffesta_tokens[1]);
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffestb_decl_attrs_7_ (t);

    default:
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_decl_finish ();
  ffelex_token_kill (ffesta_tokens[1]);
  ffesta_ffebad_1t (FFEBAD_INVALID_TYPEDECL_ATTR, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_attrs_4_ -- "type" [type parameters] ",INTENT" OPEN_PAREN

   return ffestb_decl_attrs_4_;	 // to lexer

   Handle NAME.	 */

static ffelexHandler
ffestb_decl_attrs_4_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffestb_local_.decl.kw = ffestr_other (t);
      switch (ffestb_local_.decl.kw)
	{
	case FFESTR_otherIN:
	  return (ffelexHandler) ffestb_decl_attrs_5_;

	case FFESTR_otherINOUT:
	  return (ffelexHandler) ffestb_decl_attrs_6_;

	case FFESTR_otherOUT:
	  return (ffelexHandler) ffestb_decl_attrs_6_;

	default:
	  ffestb_local_.decl.kw = FFESTR_otherNone;
	  ffesta_ffebad_1t (FFEBAD_INVALID_TYPEDECL_ATTR, t);
	  return (ffelexHandler) ffestb_decl_attrs_5_;
	}
      break;

    default:
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_decl_finish ();
  ffelex_token_kill (ffesta_tokens[1]);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "type-declaration", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_attrs_5_ -- "type" [type parameters] ",INTENT" OPEN_PAREN "IN"

   return ffestb_decl_attrs_5_;	 // to lexer

   Handle NAME or CLOSE_PAREN.	*/

static ffelexHandler
ffestb_decl_attrs_5_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      switch (ffestr_other (t))
	{
	case FFESTR_otherOUT:
	  if (ffestb_local_.decl.kw != FFESTR_otherNone)
	    ffestb_local_.decl.kw = FFESTR_otherINOUT;
	  return (ffelexHandler) ffestb_decl_attrs_6_;

	default:
	  if (ffestb_local_.decl.kw != FFESTR_otherNone)
	    {
	      ffestb_local_.decl.kw = FFESTR_otherNone;
	      ffesta_ffebad_1t (FFEBAD_INVALID_TYPEDECL_ATTR, t);
	    }
	  return (ffelexHandler) ffestb_decl_attrs_5_;
	}
      break;

    case FFELEX_typeCLOSE_PAREN:
      return (ffelexHandler) ffestb_decl_attrs_6_ (t);

    default:
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_decl_finish ();
  ffelex_token_kill (ffesta_tokens[1]);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "type-declaration", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_attrs_6_ -- "type" [type parameters] ",INTENT" OPEN_PAREN "IN"
			  ["OUT"]

   return ffestb_decl_attrs_6_;	 // to lexer

   Handle CLOSE_PAREN.	*/

static ffelexHandler
ffestb_decl_attrs_6_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCLOSE_PAREN:
      if ((ffestb_local_.decl.kw != FFESTR_otherNone)
	  && !ffesta_is_inhibited ())
	ffestc_decl_attrib (FFESTP_attribINTENT, ffesta_tokens[1],
			    ffestb_local_.decl.kw, NULL);
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffestb_decl_attrs_7_;

    default:
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_decl_finish ();
  ffelex_token_kill (ffesta_tokens[1]);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "type-declaration", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

#endif
/* ffestb_decl_attrs_7_ -- "type" [type parameters] attribute

   return ffestb_decl_attrs_7_;	 // to lexer

   Handle COMMA (another attribute) or COLONCOLON (entities).  */

static ffelexHandler
ffestb_decl_attrs_7_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      return (ffelexHandler) ffestb_decl_attrs_;

    case FFELEX_typeCOLONCOLON:
      ffestb_local_.decl.coloncolon = TRUE;
      return (ffelexHandler) ffestb_decl_ents_;

    default:
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_decl_finish ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "type-declaration", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_attrsp_ -- "type" [type parameters]

   return ffestb_decl_attrsp_;	// to lexer

   Handle COMMA (meaning we have attributes), COLONCOLON (meaning we have
   no attributes but entities), or go to entsp to see about functions or
   entities.  */

static ffelexHandler
ffestb_decl_attrsp_ (ffelexToken t)
{
  ffelex_set_names (FALSE);

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	ffestc_decl_start (ffestb_local_.decl.type, ffesta_tokens[0],
			   ffestb_local_.decl.kind, ffestb_local_.decl.kindt,
			   ffestb_local_.decl.len, ffestb_local_.decl.lent);
      if (ffestb_local_.decl.kindt != NULL)
	ffelex_token_kill (ffestb_local_.decl.kindt);
      if (ffestb_local_.decl.lent != NULL)
	ffelex_token_kill (ffestb_local_.decl.lent);
      return (ffelexHandler) ffestb_decl_attrs_;

    case FFELEX_typeCOLONCOLON:
      ffestb_local_.decl.coloncolon = TRUE;
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	ffestc_decl_start (ffestb_local_.decl.type, ffesta_tokens[0],
			   ffestb_local_.decl.kind, ffestb_local_.decl.kindt,
			   ffestb_local_.decl.len, ffestb_local_.decl.lent);
      if (ffestb_local_.decl.kindt != NULL)
	ffelex_token_kill (ffestb_local_.decl.kindt);
      if (ffestb_local_.decl.lent != NULL)
	ffelex_token_kill (ffestb_local_.decl.lent);
      return (ffelexHandler) ffestb_decl_ents_;

    default:
      return (ffelexHandler) ffestb_decl_entsp_ (t);
    }
}

/* ffestb_decl_ents_ -- "type" [type parameters] [attributes "::"]

   return ffestb_decl_ents_;  // to lexer

   Handle NAME of an entity.  */

static ffelexHandler
ffestb_decl_ents_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_decl_ents_1_;

    default:
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_decl_finish ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "type-declaration", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_ents_1_ -- "type" [type parameters] [attributes "::"] NAME

   return ffestb_decl_ents_1_;	// to lexer

   Handle ASTERISK, OPEN_PAREN, EQUALS, SLASH, COMMA, or EOS/SEMICOLON.	 */

static ffelexHandler
ffestb_decl_ents_1_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      if (!ffesta_is_inhibited ())
	ffestc_decl_item (ffesta_tokens[1], NULL, NULL, NULL, NULL, NULL, NULL,
			  NULL, FALSE);
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffestb_decl_ents_;

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (!ffesta_is_inhibited ())
	{
	  ffestc_decl_item (ffesta_tokens[1], NULL, NULL, NULL, NULL, NULL, NULL,
			    NULL, FALSE);
	  ffestc_decl_finish ();
	}
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffesta_zero (t);

    case FFELEX_typeASTERISK:
      ffestb_local_.decl.len = NULL;
      ffestb_local_.decl.lent = NULL;
      return (ffelexHandler) ffestb_decl_ents_2_;

    case FFELEX_typeOPEN_PAREN:
      ffestb_local_.decl.kind = NULL;
      ffestb_local_.decl.kindt = NULL;
      ffestb_local_.decl.len = NULL;
      ffestb_local_.decl.lent = NULL;
      return (ffelexHandler) ffestb_decl_ents_3_ (t);

    case FFELEX_typeEQUALS:
    case FFELEX_typeSLASH:
      ffestb_local_.decl.kind = NULL;
      ffestb_local_.decl.kindt = NULL;
      ffestb_subrargs_.dim_list.dims = NULL;
      ffestb_local_.decl.len = NULL;
      ffestb_local_.decl.lent = NULL;
      return (ffelexHandler) ffestb_decl_ents_7_ (t);

    default:
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_decl_finish ();
  ffelex_token_kill (ffesta_tokens[1]);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "type-declaration", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_ents_2_ -- "type" [type parameters] [attributes "::"] NAME
			 ASTERISK

   return ffestb_decl_ents_2_;	// to lexer

   Handle NUMBER or OPEN_PAREN.	 */

static ffelexHandler
ffestb_decl_ents_2_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNUMBER:
      if (ffestb_local_.decl.type != FFESTP_typeCHARACTER)
	{
	  ffestb_local_.decl.kind = NULL;
	  ffestb_local_.decl.kindt = ffelex_token_use (t);
	  return (ffelexHandler) ffestb_decl_ents_3_;
	}
      /* Fall through. *//* (CHARACTER's *n is always a len spec. */
    case FFELEX_typeOPEN_PAREN:/* "*(" is after the (omitted)
				   "(array-spec)". */
      ffestb_local_.decl.kind = NULL;
      ffestb_local_.decl.kindt = NULL;
      ffestb_subrargs_.dim_list.dims = NULL;
      return (ffelexHandler) ffestb_decl_ents_5_ (t);

    default:
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_decl_finish ();
  ffelex_token_kill (ffesta_tokens[1]);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "type-declaration", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_ents_3_ -- "type" [type parameters] [attributes "::"] NAME
			 [ASTERISK NUMBER]

   return ffestb_decl_ents_3_;	// to lexer

   Handle ASTERISK, OPEN_PAREN, EQUALS, SLASH, COMMA, or EOS/SEMICOLON.	 */

static ffelexHandler
ffestb_decl_ents_3_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      if (!ffesta_is_inhibited ())
	ffestc_decl_item (ffesta_tokens[1], ffestb_local_.decl.kind,
	     ffestb_local_.decl.kindt, NULL, NULL, NULL, NULL, NULL, FALSE);
      ffelex_token_kill (ffesta_tokens[1]);
      if (ffestb_local_.decl.kindt != NULL)
	ffelex_token_kill (ffestb_local_.decl.kindt);
      return (ffelexHandler) ffestb_decl_ents_;

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (!ffesta_is_inhibited ())
	{
	  ffestc_decl_item (ffesta_tokens[1], ffestb_local_.decl.kind,
	     ffestb_local_.decl.kindt, NULL, NULL, NULL, NULL, NULL, FALSE);
	  ffestc_decl_finish ();
	}
      ffelex_token_kill (ffesta_tokens[1]);
      if (ffestb_local_.decl.kindt != NULL)
	ffelex_token_kill (ffestb_local_.decl.kindt);
      return (ffelexHandler) ffesta_zero (t);

    case FFELEX_typeASTERISK:
      ffestb_subrargs_.dim_list.dims = NULL;
      return (ffelexHandler) ffestb_decl_ents_5_;

    case FFELEX_typeOPEN_PAREN:
      ffestb_subrargs_.dim_list.dims = ffestt_dimlist_create ();
      ffestb_subrargs_.dim_list.handler = (ffelexHandler) ffestb_decl_ents_4_;
      ffestb_subrargs_.dim_list.pool = ffesta_output_pool;
      ffestb_subrargs_.dim_list.ctx = ffesta_is_entry_valid
	? FFEEXPR_contextDIMLIST : FFEEXPR_contextDIMLISTCOMMON;
#ifdef FFECOM_dimensionsMAX
      ffestb_subrargs_.dim_list.ndims = 0;
#endif
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
					  ffestb_subrargs_.dim_list.ctx,
				    (ffeexprCallback) ffestb_subr_dimlist_);

    case FFELEX_typeEQUALS:
    case FFELEX_typeSLASH:
      ffestb_local_.decl.kind = NULL;
      ffestb_local_.decl.kindt = NULL;
      ffestb_subrargs_.dim_list.dims = NULL;
      ffestb_local_.decl.len = NULL;
      ffestb_local_.decl.lent = NULL;
      return (ffelexHandler) ffestb_decl_ents_7_ (t);

    default:
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_decl_finish ();
  ffelex_token_kill (ffesta_tokens[1]);
  if (ffestb_local_.decl.kindt != NULL)
    ffelex_token_kill (ffestb_local_.decl.kindt);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "type-declaration", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_ents_4_ -- "type" [type parameters] [attributes "::"] NAME
			 [ASTERISK NUMBER] [OPEN_PAREN dimlist CLOSE_PAREN]

   return ffestb_decl_ents_4_;	// to lexer

   Handle ASTERISK, EQUALS, SLASH, COMMA, or EOS/SEMICOLON.  */

static ffelexHandler
ffestb_decl_ents_4_ (ffelexToken t)
{
  ffelexToken nt;

  if (!ffestb_subrargs_.dim_list.ok)
    goto bad;			/* :::::::::::::::::::: */

  if (ffelex_token_type (ffesta_tokens[1]) == FFELEX_typeNAMES)
    {
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeCOMMA:
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	case FFELEX_typeASTERISK:
	case FFELEX_typeSLASH:	/* But NOT FFELEX_typeEQUALS. */
	case FFELEX_typeCOLONCOLON:	/* Actually an error. */
	  break;		/* Confirm and handle. */

	default:		/* Perhaps EQUALS, as in
				   INTEGERFUNCTIONX(A)=B. */
	  goto bad;		/* :::::::::::::::::::: */
	}
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	{
	  nt = ffelex_token_name_from_names (ffesta_tokens[1], 0, 0);
	  ffelex_token_kill (ffesta_tokens[1]);
	  ffesta_tokens[1] = nt;
	  ffestc_decl_start (ffestb_local_.decl.type, ffesta_tokens[0],
			     NULL, NULL, NULL, NULL);
	}
    }

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      if (!ffesta_is_inhibited ())
	ffestc_decl_item (ffesta_tokens[1], ffestb_local_.decl.kind,
		   ffestb_local_.decl.kindt, ffestb_subrargs_.dim_list.dims,
		ffestb_local_.decl.len, ffestb_local_.decl.lent, NULL, NULL,
			  FALSE);
      ffelex_token_kill (ffesta_tokens[1]);
      if (ffestb_local_.decl.kindt != NULL)
	ffelex_token_kill (ffestb_local_.decl.kindt);
      if (ffestb_local_.decl.lent != NULL)
	ffelex_token_kill (ffestb_local_.decl.lent);
      ffestt_dimlist_kill (ffestb_subrargs_.dim_list.dims);
      return (ffelexHandler) ffestb_decl_ents_;

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (!ffesta_is_inhibited ())
	{
	  ffestc_decl_item (ffesta_tokens[1], ffestb_local_.decl.kind,
		   ffestb_local_.decl.kindt, ffestb_subrargs_.dim_list.dims,
		ffestb_local_.decl.len, ffestb_local_.decl.lent, NULL, NULL,
			    FALSE);
	  ffestc_decl_finish ();
	}
      ffelex_token_kill (ffesta_tokens[1]);
      if (ffestb_local_.decl.kindt != NULL)
	ffelex_token_kill (ffestb_local_.decl.kindt);
      if (ffestb_local_.decl.lent != NULL)
	ffelex_token_kill (ffestb_local_.decl.lent);
      ffestt_dimlist_kill (ffestb_subrargs_.dim_list.dims);
      return (ffelexHandler) ffesta_zero (t);

    case FFELEX_typeASTERISK:
      if (ffestb_local_.decl.lent != NULL)
	break;			/* Can't specify "*length" twice. */
      return (ffelexHandler) ffestb_decl_ents_5_;

    case FFELEX_typeEQUALS:
    case FFELEX_typeSLASH:
      return (ffelexHandler) ffestb_decl_ents_7_ (t);

    default:
      break;
    }

bad:				/* :::::::::::::::::::: */
  if ((ffelex_token_type (ffesta_tokens[1]) != FFELEX_typeNAMES)
      && !ffesta_is_inhibited ())
    ffestc_decl_finish ();
  ffelex_token_kill (ffesta_tokens[1]);
  if (ffestb_local_.decl.kindt != NULL)
    ffelex_token_kill (ffestb_local_.decl.kindt);
  if (ffestb_local_.decl.lent != NULL)
    ffelex_token_kill (ffestb_local_.decl.lent);
  ffestt_dimlist_kill (ffestb_subrargs_.dim_list.dims);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "type-declaration", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_ents_5_ -- "type" [type parameters] [attributes "::"] NAME
			 [ASTERISK NUMBER] [OPEN_PAREN dimlist CLOSE_PAREN]
			 ASTERISK

   return ffestb_decl_ents_5_;	// to lexer

   Handle NUMBER or OPEN_PAREN.	 */

static ffelexHandler
ffestb_decl_ents_5_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNUMBER:
      ffestb_local_.decl.len = NULL;
      ffestb_local_.decl.lent = ffelex_token_use (t);
      return (ffelexHandler) ffestb_decl_ents_7_;

    case FFELEX_typeOPEN_PAREN:
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
       FFEEXPR_contextCHARACTERSIZE, (ffeexprCallback) ffestb_decl_ents_6_);

    default:
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_decl_finish ();
  ffelex_token_kill (ffesta_tokens[1]);
  if (ffestb_local_.decl.kindt != NULL)
    ffelex_token_kill (ffestb_local_.decl.kindt);
  if (ffestb_subrargs_.dim_list.dims != NULL)
    ffestt_dimlist_kill (ffestb_subrargs_.dim_list.dims);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "type-declaration", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_ents_6_ -- "type" [type parameters] [attributes "::"] NAME
			 [ASTERISK NUMBER] [OPEN_PAREN dimlist CLOSE_PAREN]
			 ASTERISK OPEN_PAREN expr

   (ffestb_decl_ents_6_)  // to expression handler

   Handle CLOSE_PAREN.	*/

static ffelexHandler
ffestb_decl_ents_6_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCLOSE_PAREN:
      if (expr == NULL)
	break;
      ffestb_local_.decl.len = expr;
      ffestb_local_.decl.lent = ffelex_token_use (ft);
      return (ffelexHandler) ffestb_decl_ents_7_;

    default:
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_decl_finish ();
  ffelex_token_kill (ffesta_tokens[1]);
  if (ffestb_local_.decl.kindt != NULL)
    ffelex_token_kill (ffestb_local_.decl.kindt);
  if (ffestb_subrargs_.dim_list.dims != NULL)
    ffestt_dimlist_kill (ffestb_subrargs_.dim_list.dims);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "type-declaration", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_ents_7_ -- "type" [type parameters] [attributes "::"] NAME
			 [ASTERISK NUMBER] [OPEN_PAREN dimlist CLOSE_PAREN]
			 [ASTERISK charlength]

   return ffestb_decl_ents_7_;	// to lexer

   Handle EQUALS, SLASH, COMMA, or EOS/SEMICOLON.  */

static ffelexHandler
ffestb_decl_ents_7_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      if (!ffesta_is_inhibited ())
	ffestc_decl_item (ffesta_tokens[1], ffestb_local_.decl.kind,
		   ffestb_local_.decl.kindt, ffestb_subrargs_.dim_list.dims,
		ffestb_local_.decl.len, ffestb_local_.decl.lent, NULL, NULL,
			  FALSE);
      ffelex_token_kill (ffesta_tokens[1]);
      if (ffestb_local_.decl.kindt != NULL)
	ffelex_token_kill (ffestb_local_.decl.kindt);
      if (ffestb_subrargs_.dim_list.dims != NULL)
	ffestt_dimlist_kill (ffestb_subrargs_.dim_list.dims);
      if (ffestb_local_.decl.lent != NULL)
	ffelex_token_kill (ffestb_local_.decl.lent);
      return (ffelexHandler) ffestb_decl_ents_;

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (!ffesta_is_inhibited ())
	{
	  ffestc_decl_item (ffesta_tokens[1], ffestb_local_.decl.kind,
		   ffestb_local_.decl.kindt, ffestb_subrargs_.dim_list.dims,
		ffestb_local_.decl.len, ffestb_local_.decl.lent, NULL, NULL,
			    FALSE);
	  ffestc_decl_finish ();
	}
      ffelex_token_kill (ffesta_tokens[1]);
      if (ffestb_local_.decl.kindt != NULL)
	ffelex_token_kill (ffestb_local_.decl.kindt);
      if (ffestb_subrargs_.dim_list.dims != NULL)
	ffestt_dimlist_kill (ffestb_subrargs_.dim_list.dims);
      if (ffestb_local_.decl.lent != NULL)
	ffelex_token_kill (ffestb_local_.decl.lent);
      return (ffelexHandler) ffesta_zero (t);

    case FFELEX_typeEQUALS:
      if (!ffestb_local_.decl.coloncolon)
	ffesta_ffebad_1t (FFEBAD_INVALID_TYPEDECL_INIT, t);
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		     ffestb_local_.decl.parameter ? FFEEXPR_contextPARAMETER
	   : FFEEXPR_contextINITVAL, (ffeexprCallback) ffestb_decl_ents_8_);

    case FFELEX_typeSLASH:
      if (!ffesta_is_inhibited ())
	{
	  ffestc_decl_item (ffesta_tokens[1], ffestb_local_.decl.kind,
		   ffestb_local_.decl.kindt, ffestb_subrargs_.dim_list.dims,
		ffestb_local_.decl.len, ffestb_local_.decl.lent, NULL, NULL,
			    TRUE);
	  ffestc_decl_itemstartvals ();
	}
      ffelex_token_kill (ffesta_tokens[1]);
      if (ffestb_local_.decl.kindt != NULL)
	ffelex_token_kill (ffestb_local_.decl.kindt);
      if (ffestb_subrargs_.dim_list.dims != NULL)
	ffestt_dimlist_kill (ffestb_subrargs_.dim_list.dims);
      if (ffestb_local_.decl.lent != NULL)
	ffelex_token_kill (ffestb_local_.decl.lent);
      return (ffelexHandler) ffeexpr_rhs
	(ffesta_output_pool, FFEEXPR_contextDATA,
	 (ffeexprCallback) ffestb_decl_ents_9_);

    default:
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_decl_finish ();
  ffelex_token_kill (ffesta_tokens[1]);
  if (ffestb_local_.decl.kindt != NULL)
    ffelex_token_kill (ffestb_local_.decl.kindt);
  if (ffestb_subrargs_.dim_list.dims != NULL)
    ffestt_dimlist_kill (ffestb_subrargs_.dim_list.dims);
  if (ffestb_local_.decl.lent != NULL)
    ffelex_token_kill (ffestb_local_.decl.lent);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "type-declaration", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_ents_8_ -- "type" [type parameters] [attributes "::"] NAME
			 [ASTERISK NUMBER] [OPEN_PAREN dimlist CLOSE_PAREN]
			 [ASTERISK charlength] EQUALS expr

   (ffestb_decl_ents_8_)  // to expression handler

   Handle COMMA or EOS/SEMICOLON.  */

static ffelexHandler
ffestb_decl_ents_8_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      if (expr == NULL)
	break;
      if (!ffesta_is_inhibited ())
	ffestc_decl_item (ffesta_tokens[1], ffestb_local_.decl.kind,
		   ffestb_local_.decl.kindt, ffestb_subrargs_.dim_list.dims,
		  ffestb_local_.decl.len, ffestb_local_.decl.lent, expr, ft,
			  FALSE);
      ffelex_token_kill (ffesta_tokens[1]);
      if (ffestb_local_.decl.kindt != NULL)
	ffelex_token_kill (ffestb_local_.decl.kindt);
      if (ffestb_subrargs_.dim_list.dims != NULL)
	ffestt_dimlist_kill (ffestb_subrargs_.dim_list.dims);
      if (ffestb_local_.decl.lent != NULL)
	ffelex_token_kill (ffestb_local_.decl.lent);
      return (ffelexHandler) ffestb_decl_ents_;

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (!ffesta_is_inhibited ())
	{
	  ffestc_decl_item (ffesta_tokens[1], ffestb_local_.decl.kind,
		   ffestb_local_.decl.kindt, ffestb_subrargs_.dim_list.dims,
		  ffestb_local_.decl.len, ffestb_local_.decl.lent, expr, ft,
			    FALSE);
	  ffestc_decl_finish ();
	}
      ffelex_token_kill (ffesta_tokens[1]);
      if (ffestb_local_.decl.kindt != NULL)
	ffelex_token_kill (ffestb_local_.decl.kindt);
      if (ffestb_subrargs_.dim_list.dims != NULL)
	ffestt_dimlist_kill (ffestb_subrargs_.dim_list.dims);
      if (ffestb_local_.decl.lent != NULL)
	ffelex_token_kill (ffestb_local_.decl.lent);
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_decl_finish ();
  ffelex_token_kill (ffesta_tokens[1]);
  if (ffestb_local_.decl.kindt != NULL)
    ffelex_token_kill (ffestb_local_.decl.kindt);
  if (ffestb_subrargs_.dim_list.dims != NULL)
    ffestt_dimlist_kill (ffestb_subrargs_.dim_list.dims);
  if (ffestb_local_.decl.lent != NULL)
    ffelex_token_kill (ffestb_local_.decl.lent);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "type-declaration", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_ents_9_ -- "type" ... SLASH expr

   (ffestb_decl_ents_9_)  // to expression handler

   Handle ASTERISK, COMMA, or SLASH.  */

static ffelexHandler
ffestb_decl_ents_9_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      if (expr == NULL)
	break;
      if (!ffesta_is_inhibited ())
	ffestc_decl_itemvalue (NULL, NULL, expr, ft);
      return (ffelexHandler) ffeexpr_rhs
	(ffesta_output_pool, FFEEXPR_contextDATA,
	 (ffeexprCallback) ffestb_decl_ents_9_);

    case FFELEX_typeASTERISK:
      if (expr == NULL)
	break;
      ffestb_local_.decl.expr = expr;
      ffesta_tokens[1] = ffelex_token_use (ft);
      return (ffelexHandler) ffeexpr_rhs
	(ffesta_output_pool, FFEEXPR_contextDATA,
	 (ffeexprCallback) ffestb_decl_ents_10_);

    case FFELEX_typeSLASH:
      if (expr == NULL)
	break;
      if (!ffesta_is_inhibited ())
	{
	  ffestc_decl_itemvalue (NULL, NULL, expr, ft);
	  ffestc_decl_itemendvals (t);
	}
      return (ffelexHandler) ffestb_decl_ents_11_;

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "type-declaration", t);
      break;
    }

  if (!ffesta_is_inhibited ())
    {
      ffestc_decl_itemendvals (t);
      ffestc_decl_finish ();
    }
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_ents_10_ -- "type" ... SLASH expr ASTERISK expr

   (ffestb_decl_ents_10_)  // to expression handler

   Handle COMMA or SLASH.  */

static ffelexHandler
ffestb_decl_ents_10_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      if (expr == NULL)
	break;
      if (!ffesta_is_inhibited ())
	ffestc_decl_itemvalue (ffestb_local_.decl.expr, ffesta_tokens[1],
			       expr, ft);
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffeexpr_rhs
	(ffesta_output_pool, FFEEXPR_contextDATA,
	 (ffeexprCallback) ffestb_decl_ents_9_);

    case FFELEX_typeSLASH:
      if (expr == NULL)
	break;
      if (!ffesta_is_inhibited ())
	{
	  ffestc_decl_itemvalue (ffestb_local_.decl.expr, ffesta_tokens[1],
				 expr, ft);
	  ffestc_decl_itemendvals (t);
	}
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffestb_decl_ents_11_;

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "type-declaration", t);
      break;
    }

  if (!ffesta_is_inhibited ())
    {
      ffestc_decl_itemendvals (t);
      ffestc_decl_finish ();
    }
  ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_ents_11_ -- "type" [type parameters] [attributes "::"] NAME
			 [ASTERISK NUMBER] [OPEN_PAREN dimlist CLOSE_PAREN]
			 [ASTERISK charlength] SLASH initvals SLASH

   return ffestb_decl_ents_11_;	 // to lexer

   Handle COMMA or EOS/SEMICOLON.  */

static ffelexHandler
ffestb_decl_ents_11_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      return (ffelexHandler) ffestb_decl_ents_;

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (!ffesta_is_inhibited ())
	ffestc_decl_finish ();
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_decl_finish ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "type-declaration", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_entsp_ -- "type" [type parameters]

   return ffestb_decl_entsp_;  // to lexer

   Handle NAME or NAMES beginning either an entity (object) declaration or
   a function definition..  */

static ffelexHandler
ffestb_decl_entsp_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_confirmed ();
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_decl_entsp_1_;

    case FFELEX_typeNAMES:
      ffesta_confirmed ();
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_decl_entsp_2_;

    default:
      break;
    }

  if (ffestb_local_.decl.kindt != NULL)
    ffelex_token_kill (ffestb_local_.decl.kindt);
  if (ffestb_local_.decl.lent != NULL)
    ffelex_token_kill (ffestb_local_.decl.lent);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "type-declaration", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_entsp_1_ -- "type" [type parameters] NAME

   return ffestb_decl_entsp_1_;	 // to lexer

   If we get another NAME token here, then the previous one must be
   "RECURSIVE" or "FUNCTION" and we handle it accordingly.  Otherwise,
   we send the previous and current token through to _ents_.  */

static ffelexHandler
ffestb_decl_entsp_1_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      switch (ffestr_first (ffesta_tokens[1]))
	{
#if FFESTR_F90
	case FFESTR_firstRECURSIVE:
	  if (ffestr_first (t) != FFESTR_firstFUNCTION)
	    {
	      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FUNCTION", t);
	      break;
	    }
	  ffestb_local_.decl.recursive = ffesta_tokens[1];
	  return (ffelexHandler) ffestb_decl_funcname_;
#endif

	case FFESTR_firstFUNCTION:
	  ffelex_token_kill (ffesta_tokens[1]);
	  return (ffelexHandler) ffestb_decl_funcname_ (t);

	default:
	  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FUNCTION", ffesta_tokens[1]);
	  break;
	}
      break;

    default:
      if ((ffelex_token_type (ffesta_tokens[1]) != FFELEX_typeNAMES)
	  && !ffesta_is_inhibited ())
	ffestc_decl_start (ffestb_local_.decl.type, ffesta_tokens[0],
			   ffestb_local_.decl.kind, ffestb_local_.decl.kindt,
			   ffestb_local_.decl.len, ffestb_local_.decl.lent);
      if (ffestb_local_.decl.kindt != NULL)
	ffelex_token_kill (ffestb_local_.decl.kindt);
      if (ffestb_local_.decl.lent != NULL)
	ffelex_token_kill (ffestb_local_.decl.lent);
      /* NAME/NAMES token already in ffesta_tokens[1]. */
      return (ffelexHandler) ffestb_decl_ents_1_ (t);
    }

  if (ffestb_local_.decl.kindt != NULL)
    ffelex_token_kill (ffestb_local_.decl.kindt);
  if (ffestb_local_.decl.lent != NULL)
    ffelex_token_kill (ffestb_local_.decl.lent);
  ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_entsp_2_ -- "type" [type parameters] NAMES

   return ffestb_decl_entsp_2_;	 // to lexer

   If we get an ASTERISK or OPEN_PAREN here, then if the previous NAMES
   begins with "FUNCTION" or "RECURSIVEFUNCTION" and is followed by a
   first-name-char, we have a possible syntactically ambiguous situation.
   Otherwise, we have a straightforward situation just as if we went
   through _entsp_1_ instead of here.  */

static ffelexHandler
ffestb_decl_entsp_2_ (ffelexToken t)
{
  ffelexToken nt;
  bool asterisk_ok;
  unsigned const char *p;
  ffeTokenLength i;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeASTERISK:
      ffesta_confirmed ();
      switch (ffestb_local_.decl.type)
	{
	case FFESTP_typeINTEGER:
	case FFESTP_typeREAL:
	case FFESTP_typeCOMPLEX:
	case FFESTP_typeLOGICAL:
	  asterisk_ok = (ffestb_local_.decl.kindt == NULL);
	  break;

	case FFESTP_typeCHARACTER:
	  asterisk_ok = (ffestb_local_.decl.lent == NULL);
	  break;

	case FFESTP_typeBYTE:
	case FFESTP_typeWORD:
	default:
	  asterisk_ok = FALSE;
	  break;
	}
      switch (ffestr_first (ffesta_tokens[1]))
	{
#if FFESTR_F90
	case FFESTR_firstRECURSIVEFNCTN:
	  if (!asterisk_ok)
	    break;		/* For our own convenience, treat as non-FN
				   stmt. */
	  p = ffelex_token_text (ffesta_tokens[1])
	    + (i = FFESTR_firstlRECURSIVEFNCTN);
	  if (!ffesrc_is_name_init (*p))
	    break;
	  ffestb_local_.decl.recursive
	    = ffelex_token_name_from_names (ffesta_tokens[1], 0,
					    FFESTR_firstlRECURSIVEFNCTN);
	  ffesta_tokens[2] = ffelex_token_name_from_names (ffesta_tokens[1],
					    FFESTR_firstlRECURSIVEFNCTN, 0);
	  return (ffelexHandler) ffestb_decl_entsp_3_;
#endif

	case FFESTR_firstFUNCTION:
	  if (!asterisk_ok)
	    break;		/* For our own convenience, treat as non-FN
				   stmt. */
	  p = ffelex_token_text (ffesta_tokens[1])
	    + (i = FFESTR_firstlFUNCTION);
	  if (!ffesrc_is_name_init (*p))
	    break;
	  ffestb_local_.decl.recursive = NULL;
	  ffesta_tokens[2] = ffelex_token_name_from_names (ffesta_tokens[1],
						  FFESTR_firstlFUNCTION, 0);
	  return (ffelexHandler) ffestb_decl_entsp_3_;

	default:
	  break;
	}
      break;

    case FFELEX_typeOPEN_PAREN:
      ffestb_local_.decl.aster_after = FALSE;
      switch (ffestr_first (ffesta_tokens[1]))
	{
#if FFESTR_F90
	case FFESTR_firstRECURSIVEFNCTN:
	  p = ffelex_token_text (ffesta_tokens[1])
	    + (i = FFESTR_firstlRECURSIVEFNCTN);
	  if (!ffesrc_is_name_init (*p))
	    break;
	  ffestb_local_.decl.recursive
	    = ffelex_token_name_from_names (ffesta_tokens[1], 0,
					    FFESTR_firstlRECURSIVEFNCTN);
	  ffesta_tokens[2] = ffelex_token_name_from_names (ffesta_tokens[1],
					    FFESTR_firstlRECURSIVEFNCTN, 0);
	  return (ffelexHandler) ffestb_decl_entsp_5_ (t);
#endif

	case FFESTR_firstFUNCTION:
	  p = ffelex_token_text (ffesta_tokens[1])
	    + (i = FFESTR_firstlFUNCTION);
	  if (!ffesrc_is_name_init (*p))
	    break;
	  ffestb_local_.decl.recursive = NULL;
	  ffesta_tokens[2] = ffelex_token_name_from_names (ffesta_tokens[1],
						  FFESTR_firstlFUNCTION, 0);
	  return (ffelexHandler) ffestb_decl_entsp_5_ (t);

	default:
	  break;
	}
      if ((ffestb_local_.decl.kindt != NULL)
	  || (ffestb_local_.decl.lent != NULL))
	break;			/* Have kind/len type param, definitely not
				   assignment stmt. */
      return (ffelexHandler) ffestb_decl_entsp_1_ (t);

    default:
      break;
    }

  nt = ffelex_token_name_from_names (ffesta_tokens[1], 0, 0);
  ffelex_token_kill (ffesta_tokens[1]);
  ffesta_tokens[1] = nt;	/* Change NAMES to NAME. */
  return (ffelexHandler) ffestb_decl_entsp_1_ (t);
}

/* ffestb_decl_entsp_3_ -- "type" [type parameters] [RECURSIVE] FUNCTION
			     NAME ASTERISK

   return ffestb_decl_entsp_3_;	 // to lexer

   Handle NUMBER or OPEN_PAREN.	 */

static ffelexHandler
ffestb_decl_entsp_3_ (ffelexToken t)
{
  ffestb_local_.decl.aster_after = TRUE;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNUMBER:
      switch (ffestb_local_.decl.type)
	{
	case FFESTP_typeINTEGER:
	case FFESTP_typeREAL:
	case FFESTP_typeCOMPLEX:
	case FFESTP_typeLOGICAL:
	  ffestb_local_.decl.kindt = ffelex_token_use (t);
	  break;

	case FFESTP_typeCHARACTER:
	  ffestb_local_.decl.lent = ffelex_token_use (t);
	  break;

	case FFESTP_typeBYTE:
	case FFESTP_typeWORD:
	default:
	  assert (FALSE);
	}
      return (ffelexHandler) ffestb_decl_entsp_5_;

    case FFELEX_typeOPEN_PAREN:
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
					  FFEEXPR_contextCHARACTERSIZE,
				    (ffeexprCallback) ffestb_decl_entsp_4_);

    default:
      break;
    }

  if (ffestb_local_.decl.recursive != NULL)
    ffelex_token_kill (ffestb_local_.decl.recursive);
  if (ffestb_local_.decl.kindt != NULL)
    ffelex_token_kill (ffestb_local_.decl.kindt);
  if (ffestb_local_.decl.lent != NULL)
    ffelex_token_kill (ffestb_local_.decl.lent);
  ffelex_token_kill (ffesta_tokens[1]);
  ffelex_token_kill (ffesta_tokens[2]);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FUNCTION", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_entsp_4_ -- "type" [type parameters] [RECURSIVE] FUNCTION
			     NAME ASTERISK OPEN_PAREN expr

   (ffestb_decl_entsp_4_)  // to expression handler

   Allow only CLOSE_PAREN; and deal with character-length expression.  */

static ffelexHandler
ffestb_decl_entsp_4_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCLOSE_PAREN:
      if (expr == NULL)
	break;
      switch (ffestb_local_.decl.type)
	{
	case FFESTP_typeCHARACTER:
	  ffestb_local_.decl.len = expr;
	  ffestb_local_.decl.lent = ffelex_token_use (ft);
	  break;

	default:
	  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FUNCTION", t);
	  break;
	}
      return (ffelexHandler) ffestb_decl_entsp_5_;

    default:
      break;
    }

  if (ffestb_local_.decl.recursive != NULL)
    ffelex_token_kill (ffestb_local_.decl.recursive);
  if (ffestb_local_.decl.kindt != NULL)
    ffelex_token_kill (ffestb_local_.decl.kindt);
  if (ffestb_local_.decl.lent != NULL)
    ffelex_token_kill (ffestb_local_.decl.lent);
  ffelex_token_kill (ffesta_tokens[1]);
  ffelex_token_kill (ffesta_tokens[2]);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FUNCTION", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_entsp_5_ -- "type" [type parameters] [RECURSIVE] FUNCTION
			     NAME [type parameter]

   return ffestb_decl_entsp_5_;	 // to lexer

   Make sure the next token is an OPEN_PAREN.  Get the arg list or dimension
   list.  If it can't be an arg list, or if the CLOSE_PAREN is followed by
   something other than EOS/SEMICOLON or NAME, then treat as dimension list
   and handle statement as an R426/R501.  If it can't be a dimension list, or
   if the CLOSE_PAREN is followed by NAME, treat as an arg list and handle
   statement as an R1219.  If it can be either an arg list or a dimension
   list and if the CLOSE_PAREN is followed by EOS/SEMICOLON, ask FFESTC
   whether to treat the statement as an R426/R501 or an R1219 and act
   accordingly.	 */

static ffelexHandler
ffestb_decl_entsp_5_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeOPEN_PAREN:
      if (ffestb_local_.decl.aster_after && (ffestb_local_.decl.len != NULL))
	{			/* "CHARACTER[RECURSIVE]FUNCTIONxyz*(len-expr)
				   (..." must be a function-stmt, since the
				   (len-expr) cannot precede (array-spec) in
				   an object declaration but can precede
				   (name-list) in a function stmt. */
	  ffelex_token_kill (ffesta_tokens[1]);
	  ffesta_tokens[1] = ffesta_tokens[2];
	  return (ffelexHandler) ffestb_decl_funcname_4_ (t);
	}
      ffestb_local_.decl.toklist = ffestt_tokenlist_create ();
      ffestb_local_.decl.empty = TRUE;
      ffestt_tokenlist_append (ffestb_local_.decl.toklist, ffelex_token_use (t));
      return (ffelexHandler) ffestb_decl_entsp_6_;

    default:
      break;
    }

  assert (ffestb_local_.decl.aster_after);
  ffesta_confirmed ();		/* We've seen an ASTERISK, so even EQUALS
				   confirmed. */
  ffestb_subr_ambig_to_ents_ ();
  ffestb_subrargs_.dim_list.dims = NULL;
  return (ffelexHandler) ffestb_decl_ents_7_ (t);
}

/* ffestb_decl_entsp_6_ -- "type" [type parameters] [RECURSIVE] FUNCTION
			     NAME [type parameter] OPEN_PAREN

   return ffestb_decl_entsp_6_;	 // to lexer

   If CLOSE_PAREN, we definitely have an R1219 function-stmt, since
   the notation "name()" is invalid for a declaration.	*/

static ffelexHandler
ffestb_decl_entsp_6_ (ffelexToken t)
{
  ffelexHandler next;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCLOSE_PAREN:
      if (!ffestb_local_.decl.empty)
	{			/* Trailing comma, just a warning for
				   stmt func def, so allow ambiguity. */
	  ffestt_tokenlist_append (ffestb_local_.decl.toklist,
				   ffelex_token_use (t));
	  return (ffelexHandler) ffestb_decl_entsp_8_;
	}
      ffelex_token_kill (ffesta_tokens[1]);
      ffesta_tokens[1] = ffesta_tokens[2];
      next = (ffelexHandler) ffestt_tokenlist_handle
	(ffestb_local_.decl.toklist, (ffelexHandler) ffestb_decl_funcname_4_);
      ffestt_tokenlist_kill (ffestb_local_.decl.toklist);
      return (ffelexHandler) (*next) (t);

    case FFELEX_typeNAME:
      ffestb_local_.decl.empty = FALSE;
      ffestt_tokenlist_append (ffestb_local_.decl.toklist, ffelex_token_use (t));
      return (ffelexHandler) ffestb_decl_entsp_7_;

    case FFELEX_typeEQUALS:
    case FFELEX_typePOINTS:
    case FFELEX_typePERCENT:
    case FFELEX_typePERIOD:
    case FFELEX_typeOPEN_PAREN:
      if ((ffestb_local_.decl.kindt != NULL)
	  || (ffestb_local_.decl.lent != NULL))
	break;			/* type(params)name or type*val name, either
				   way confirmed. */
      return (ffelexHandler) ffestb_subr_ambig_nope_ (t);

    default:
      break;
    }

  ffesta_confirmed ();
  ffestb_subr_ambig_to_ents_ ();
  next = (ffelexHandler) ffestt_tokenlist_handle (ffestb_local_.decl.toklist,
				       (ffelexHandler) ffestb_decl_ents_3_);
  ffestt_tokenlist_kill (ffestb_local_.decl.toklist);
  return (ffelexHandler) (*next) (t);
}

/* ffestb_decl_entsp_7_ -- "type" [type parameters] [RECURSIVE] FUNCTION
			     NAME [type parameter] OPEN_PAREN NAME

   return ffestb_decl_entsp_7_;	 // to lexer

   Expect COMMA or CLOSE_PAREN to remain ambiguous, else not an R1219
   function-stmt.  */

static ffelexHandler
ffestb_decl_entsp_7_ (ffelexToken t)
{
  ffelexHandler next;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCLOSE_PAREN:
      ffestt_tokenlist_append (ffestb_local_.decl.toklist, ffelex_token_use (t));
      return (ffelexHandler) ffestb_decl_entsp_8_;

    case FFELEX_typeCOMMA:
      ffestt_tokenlist_append (ffestb_local_.decl.toklist, ffelex_token_use (t));
      return (ffelexHandler) ffestb_decl_entsp_6_;

    case FFELEX_typeEQUALS:
    case FFELEX_typePOINTS:
    case FFELEX_typePERCENT:
    case FFELEX_typePERIOD:
    case FFELEX_typeOPEN_PAREN:
      if ((ffestb_local_.decl.kindt != NULL)
	  || (ffestb_local_.decl.lent != NULL))
	break;			/* type(params)name or type*val name, either
				   way confirmed. */
      return (ffelexHandler) ffestb_subr_ambig_nope_ (t);

    default:
      break;
    }

  ffesta_confirmed ();
  ffestb_subr_ambig_to_ents_ ();
  next = (ffelexHandler) ffestt_tokenlist_handle (ffestb_local_.decl.toklist,
				       (ffelexHandler) ffestb_decl_ents_3_);
  ffestt_tokenlist_kill (ffestb_local_.decl.toklist);
  return (ffelexHandler) (*next) (t);
}

/* ffestb_decl_entsp_8_ -- "type" [type parameters] [RECURSIVE] FUNCTION
			     NAME [type parameter] OPEN_PAREN name-list
			     CLOSE_PAREN

   return ffestb_decl_entsp_8_;	 // to lexer

   If EOS/SEMICOLON, situation remains ambiguous, ask FFESTC to resolve
   it.	If NAME (must be "RESULT", but that is checked later on),
   definitely an R1219 function-stmt.  Anything else, handle as entity decl.  */

static ffelexHandler
ffestb_decl_entsp_8_ (ffelexToken t)
{
  ffelexHandler next;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (ffestc_is_decl_not_R1219 ())
	break;
      /* Fall through. */
    case FFELEX_typeNAME:
      ffesta_confirmed ();
      ffelex_token_kill (ffesta_tokens[1]);
      ffesta_tokens[1] = ffesta_tokens[2];
      next = (ffelexHandler) ffestt_tokenlist_handle
	(ffestb_local_.decl.toklist, (ffelexHandler) ffestb_decl_funcname_4_);
      ffestt_tokenlist_kill (ffestb_local_.decl.toklist);
      return (ffelexHandler) (*next) (t);

    case FFELEX_typeEQUALS:
    case FFELEX_typePOINTS:
    case FFELEX_typePERCENT:
    case FFELEX_typePERIOD:
    case FFELEX_typeOPEN_PAREN:
      if ((ffestb_local_.decl.kindt != NULL)
	  || (ffestb_local_.decl.lent != NULL))
	break;			/* type(params)name or type*val name, either
				   way confirmed. */
      return (ffelexHandler) ffestb_subr_ambig_nope_ (t);

    default:
      break;
    }

  ffesta_confirmed ();
  ffestb_subr_ambig_to_ents_ ();
  next = (ffelexHandler) ffestt_tokenlist_handle (ffestb_local_.decl.toklist,
				       (ffelexHandler) ffestb_decl_ents_3_);
  ffestt_tokenlist_kill (ffestb_local_.decl.toklist);
  return (ffelexHandler) (*next) (t);
}

/* ffestb_decl_func_ -- ["type" [type parameters]] RECURSIVE

   return ffestb_decl_func_;  // to lexer

   Handle "FUNCTION".  */

#if FFESTR_F90
static ffelexHandler
ffestb_decl_func_ (ffelexToken t)
{
  const char *p;
  ffeTokenLength i;

  ffelex_set_names (FALSE);

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      if (ffestr_first (t) != FFESTR_firstFUNCTION)
	break;
      return (ffelexHandler) ffestb_decl_funcname_;

    case FFELEX_typeNAMES:
      ffesta_confirmed ();
      if (ffestr_first (t) != FFESTR_firstFUNCTION)
	break;
      p = ffelex_token_text (t) + (i = FFESTR_firstlFUNCTION);
      if (*p == '\0')
	break;
      if (!ffesrc_is_name_init (*p))
	goto bad_i;		/* :::::::::::::::::::: */
      ffesta_tokens[1] = ffelex_token_name_from_names (t, i, 0);
      return (ffelexHandler) ffestb_decl_funcname_1_;

    default:
      break;
    }

  if (ffestb_local_.decl.recursive != NULL)
    ffelex_token_kill (ffestb_local_.decl.recursive);
  if (ffestb_local_.decl.kindt != NULL)
    ffelex_token_kill (ffestb_local_.decl.kindt);
  if (ffestb_local_.decl.lent != NULL)
    ffelex_token_kill (ffestb_local_.decl.lent);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FUNCTION", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_i:				/* :::::::::::::::::::: */
  if (ffestb_local_.decl.recursive != NULL)
    ffelex_token_kill (ffestb_local_.decl.recursive);
  if (ffestb_local_.decl.kindt != NULL)
    ffelex_token_kill (ffestb_local_.decl.kindt);
  if (ffestb_local_.decl.lent != NULL)
    ffelex_token_kill (ffestb_local_.decl.lent);
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, "FUNCTION", t, i, NULL);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

#endif
/* ffestb_decl_funcname_ -- "type" [type parameters] [RECURSIVE] FUNCTION

   return ffestb_decl_funcname_;  // to lexer

   Handle NAME of a function.  */

static ffelexHandler
ffestb_decl_funcname_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_decl_funcname_1_;

    default:
      break;
    }

  if (ffestb_local_.decl.recursive != NULL)
    ffelex_token_kill (ffestb_local_.decl.recursive);
  if (ffestb_local_.decl.kindt != NULL)
    ffelex_token_kill (ffestb_local_.decl.kindt);
  if (ffestb_local_.decl.lent != NULL)
    ffelex_token_kill (ffestb_local_.decl.lent);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FUNCTION", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_funcname_1_ -- "type" [type parameters] [RECURSIVE] FUNCTION
			     NAME

   return ffestb_decl_funcname_1_;  // to lexer

   Handle ASTERISK or OPEN_PAREN.  */

static ffelexHandler
ffestb_decl_funcname_1_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeASTERISK:
      return (ffelexHandler) ffestb_decl_funcname_2_;

    case FFELEX_typeOPEN_PAREN:
      return (ffelexHandler) ffestb_decl_funcname_4_ (t);

    default:
      break;
    }

  if (ffestb_local_.decl.recursive != NULL)
    ffelex_token_kill (ffestb_local_.decl.recursive);
  if (ffestb_local_.decl.kindt != NULL)
    ffelex_token_kill (ffestb_local_.decl.kindt);
  if (ffestb_local_.decl.lent != NULL)
    ffelex_token_kill (ffestb_local_.decl.lent);
  ffelex_token_kill (ffesta_tokens[1]);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FUNCTION", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_funcname_2_ -- "type" [type parameters] [RECURSIVE] FUNCTION
			     NAME ASTERISK

   return ffestb_decl_funcname_2_;  // to lexer

   Handle NUMBER or OPEN_PAREN.	 */

static ffelexHandler
ffestb_decl_funcname_2_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNUMBER:
      switch (ffestb_local_.decl.type)
	{
	case FFESTP_typeINTEGER:
	case FFESTP_typeREAL:
	case FFESTP_typeCOMPLEX:
	case FFESTP_typeLOGICAL:
	  if (ffestb_local_.decl.kindt == NULL)
	    ffestb_local_.decl.kindt = ffelex_token_use (t);
	  else
	    ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FUNCTION", t);
	  break;

	case FFESTP_typeCHARACTER:
	  if (ffestb_local_.decl.lent == NULL)
	    ffestb_local_.decl.lent = ffelex_token_use (t);
	  else
	    ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FUNCTION", t);
	  break;

	case FFESTP_typeBYTE:
	case FFESTP_typeWORD:
	default:
	  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FUNCTION", t);
	  break;
	}
      return (ffelexHandler) ffestb_decl_funcname_4_;

    case FFELEX_typeOPEN_PAREN:
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
					  FFEEXPR_contextCHARACTERSIZE,
				 (ffeexprCallback) ffestb_decl_funcname_3_);

    default:
      break;
    }

  if (ffestb_local_.decl.recursive != NULL)
    ffelex_token_kill (ffestb_local_.decl.recursive);
  if (ffestb_local_.decl.kindt != NULL)
    ffelex_token_kill (ffestb_local_.decl.kindt);
  if (ffestb_local_.decl.lent != NULL)
    ffelex_token_kill (ffestb_local_.decl.lent);
  ffelex_token_kill (ffesta_tokens[1]);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FUNCTION", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_funcname_3_ -- "type" [type parameters] [RECURSIVE] FUNCTION
			     NAME ASTERISK OPEN_PAREN expr

   (ffestb_decl_funcname_3_)  // to expression handler

   Allow only CLOSE_PAREN; and deal with character-length expression.  */

static ffelexHandler
ffestb_decl_funcname_3_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCLOSE_PAREN:
      if (expr == NULL)
	break;
      switch (ffestb_local_.decl.type)
	{
	case FFESTP_typeCHARACTER:
	  if (ffestb_local_.decl.lent == NULL)
	    {
	      ffestb_local_.decl.len = expr;
	      ffestb_local_.decl.lent = ffelex_token_use (ft);
	    }
	  else
	    ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FUNCTION", t);
	  break;

	default:
	  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FUNCTION", t);
	  break;
	}
      return (ffelexHandler) ffestb_decl_funcname_4_;

    default:
      break;
    }

  if (ffestb_local_.decl.recursive != NULL)
    ffelex_token_kill (ffestb_local_.decl.recursive);
  if (ffestb_local_.decl.kindt != NULL)
    ffelex_token_kill (ffestb_local_.decl.kindt);
  if (ffestb_local_.decl.lent != NULL)
    ffelex_token_kill (ffestb_local_.decl.lent);
  ffelex_token_kill (ffesta_tokens[1]);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FUNCTION", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_funcname_4_ -- "type" [type parameters] [RECURSIVE] FUNCTION
			     NAME [type parameter]

   return ffestb_decl_funcname_4_;  // to lexer

   Make sure the next token is an OPEN_PAREN.  Get the arg list and
   then implement.  */

static ffelexHandler
ffestb_decl_funcname_4_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeOPEN_PAREN:
      ffestb_subrargs_.name_list.args = ffestt_tokenlist_create ();
      ffestb_subrargs_.name_list.handler
	= (ffelexHandler) ffestb_decl_funcname_5_;
      ffestb_subrargs_.name_list.is_subr = FALSE;
      ffestb_subrargs_.name_list.names = FALSE;
      return (ffelexHandler) ffestb_subr_name_list_;

    default:
      break;
    }

  if (ffestb_local_.decl.recursive != NULL)
    ffelex_token_kill (ffestb_local_.decl.recursive);
  if (ffestb_local_.decl.kindt != NULL)
    ffelex_token_kill (ffestb_local_.decl.kindt);
  if (ffestb_local_.decl.lent != NULL)
    ffelex_token_kill (ffestb_local_.decl.lent);
  ffelex_token_kill (ffesta_tokens[1]);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FUNCTION", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_funcname_5_ -- "type" [type parameters] [RECURSIVE] FUNCTION
			     NAME [type parameter] OPEN_PAREN arg-list
			     CLOSE_PAREN

   return ffestb_decl_funcname_5_;  // to lexer

   Must have EOS/SEMICOLON or "RESULT" here.  */

static ffelexHandler
ffestb_decl_funcname_5_ (ffelexToken t)
{
  if (!ffestb_subrargs_.name_list.ok)
    goto bad;			/* :::::::::::::::::::: */

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	ffestc_R1219 (ffesta_tokens[1], ffestb_subrargs_.name_list.args,
	    ffestb_subrargs_.name_list.close_paren, ffestb_local_.decl.type,
		      ffestb_local_.decl.kind, ffestb_local_.decl.kindt,
		      ffestb_local_.decl.len, ffestb_local_.decl.lent,
		      ffestb_local_.decl.recursive, NULL);
      if (ffestb_local_.decl.recursive != NULL)
	ffelex_token_kill (ffestb_local_.decl.recursive);
      if (ffestb_local_.decl.kindt != NULL)
	ffelex_token_kill (ffestb_local_.decl.kindt);
      if (ffestb_local_.decl.lent != NULL)
	ffelex_token_kill (ffestb_local_.decl.lent);
      ffelex_token_kill (ffesta_tokens[1]);
      ffelex_token_kill (ffestb_subrargs_.name_list.close_paren);
      ffestt_tokenlist_kill (ffestb_subrargs_.name_list.args);
      return (ffelexHandler) ffesta_zero (t);

    case FFELEX_typeNAME:
      if (ffestr_other (t) != FFESTR_otherRESULT)
	break;
      return (ffelexHandler) ffestb_decl_funcname_6_;

    default:
      break;
    }

bad:				/* :::::::::::::::::::: */
  if (ffestb_local_.decl.recursive != NULL)
    ffelex_token_kill (ffestb_local_.decl.recursive);
  if (ffestb_local_.decl.kindt != NULL)
    ffelex_token_kill (ffestb_local_.decl.kindt);
  if (ffestb_local_.decl.lent != NULL)
    ffelex_token_kill (ffestb_local_.decl.lent);
  ffelex_token_kill (ffesta_tokens[1]);
  ffelex_token_kill (ffestb_subrargs_.name_list.close_paren);
  ffestt_tokenlist_kill (ffestb_subrargs_.name_list.args);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FUNCTION", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_funcname_6_ -- "type" [type parameters] [RECURSIVE] FUNCTION
			     NAME [type parameter] OPEN_PAREN arglist
			     CLOSE_PAREN "RESULT"

   return ffestb_decl_funcname_6_;  // to lexer

   Make sure the next token is an OPEN_PAREN.  */

static ffelexHandler
ffestb_decl_funcname_6_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeOPEN_PAREN:
      return (ffelexHandler) ffestb_decl_funcname_7_;

    default:
      break;
    }

  if (ffestb_local_.decl.recursive != NULL)
    ffelex_token_kill (ffestb_local_.decl.recursive);
  if (ffestb_local_.decl.kindt != NULL)
    ffelex_token_kill (ffestb_local_.decl.kindt);
  if (ffestb_local_.decl.lent != NULL)
    ffelex_token_kill (ffestb_local_.decl.lent);
  ffelex_token_kill (ffesta_tokens[1]);
  ffelex_token_kill (ffestb_subrargs_.name_list.close_paren);
  ffestt_tokenlist_kill (ffestb_subrargs_.name_list.args);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FUNCTION", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_funcname_7_ -- "type" [type parameters] [RECURSIVE] FUNCTION
			     NAME [type parameter] OPEN_PAREN arglist
			     CLOSE_PAREN "RESULT" OPEN_PAREN

   return ffestb_decl_funcname_7_;  // to lexer

   Make sure the next token is a NAME.	*/

static ffelexHandler
ffestb_decl_funcname_7_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_tokens[2] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_decl_funcname_8_;

    default:
      break;
    }

  if (ffestb_local_.decl.recursive != NULL)
    ffelex_token_kill (ffestb_local_.decl.recursive);
  if (ffestb_local_.decl.kindt != NULL)
    ffelex_token_kill (ffestb_local_.decl.kindt);
  if (ffestb_local_.decl.lent != NULL)
    ffelex_token_kill (ffestb_local_.decl.lent);
  ffelex_token_kill (ffesta_tokens[1]);
  ffelex_token_kill (ffestb_subrargs_.name_list.close_paren);
  ffestt_tokenlist_kill (ffestb_subrargs_.name_list.args);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FUNCTION", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_funcname_8_ -- "type" [type parameters] [RECURSIVE] FUNCTION
			     NAME [type parameter] OPEN_PAREN arglist
			     CLOSE_PAREN "RESULT" OPEN_PAREN NAME

   return ffestb_decl_funcname_8_;  // to lexer

   Make sure the next token is a CLOSE_PAREN.  */

static ffelexHandler
ffestb_decl_funcname_8_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCLOSE_PAREN:
      return (ffelexHandler) ffestb_decl_funcname_9_;

    default:
      break;
    }

  if (ffestb_local_.decl.recursive != NULL)
    ffelex_token_kill (ffestb_local_.decl.recursive);
  if (ffestb_local_.decl.kindt != NULL)
    ffelex_token_kill (ffestb_local_.decl.kindt);
  if (ffestb_local_.decl.lent != NULL)
    ffelex_token_kill (ffestb_local_.decl.lent);
  ffelex_token_kill (ffesta_tokens[1]);
  ffelex_token_kill (ffesta_tokens[2]);
  ffelex_token_kill (ffestb_subrargs_.name_list.close_paren);
  ffestt_tokenlist_kill (ffestb_subrargs_.name_list.args);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FUNCTION", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_funcname_9_ -- "type" [type parameters] [RECURSIVE] FUNCTION
			     NAME [type parameter] OPEN_PAREN arg-list
			     CLOSE_PAREN "RESULT" OPEN_PAREN NAME CLOSE_PAREN

   return ffestb_decl_funcname_9_;  // to lexer

   Must have EOS/SEMICOLON here.  */

static ffelexHandler
ffestb_decl_funcname_9_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (!ffesta_is_inhibited ())
	ffestc_R1219 (ffesta_tokens[1], ffestb_subrargs_.name_list.args,
	    ffestb_subrargs_.name_list.close_paren, ffestb_local_.decl.type,
		      ffestb_local_.decl.kind, ffestb_local_.decl.kindt,
		      ffestb_local_.decl.len, ffestb_local_.decl.lent,
		      ffestb_local_.decl.recursive, ffesta_tokens[2]);
      if (ffestb_local_.decl.recursive != NULL)
	ffelex_token_kill (ffestb_local_.decl.recursive);
      if (ffestb_local_.decl.kindt != NULL)
	ffelex_token_kill (ffestb_local_.decl.kindt);
      if (ffestb_local_.decl.lent != NULL)
	ffelex_token_kill (ffestb_local_.decl.lent);
      ffelex_token_kill (ffesta_tokens[1]);
      ffelex_token_kill (ffesta_tokens[2]);
      ffelex_token_kill (ffestb_subrargs_.name_list.close_paren);
      ffestt_tokenlist_kill (ffestb_subrargs_.name_list.args);
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  if (ffestb_local_.decl.recursive != NULL)
    ffelex_token_kill (ffestb_local_.decl.recursive);
  if (ffestb_local_.decl.kindt != NULL)
    ffelex_token_kill (ffestb_local_.decl.kindt);
  if (ffestb_local_.decl.lent != NULL)
    ffelex_token_kill (ffestb_local_.decl.lent);
  ffelex_token_kill (ffesta_tokens[1]);
  ffelex_token_kill (ffesta_tokens[2]);
  ffelex_token_kill (ffestb_subrargs_.name_list.close_paren);
  ffestt_tokenlist_kill (ffestb_subrargs_.name_list.args);
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "FUNCTION", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V003 -- Parse the STRUCTURE statement

   return ffestb_V003;	// to lexer

   Make sure the statement has a valid form for the STRUCTURE statement.
   If it does, implement the statement.	 */

#if FFESTR_VXT
ffelexHandler
ffestb_V003 (ffelexToken t)
{
  ffeTokenLength i;
  const char *p;
  ffelexToken nt;
  ffelexHandler next;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstSTRUCTURE)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeNAME:
	  ffesta_confirmed ();
	  if (!ffesta_is_inhibited ())
	    ffestc_V003_start (NULL);
	  ffestb_local_.structure.started = TRUE;
	  return (ffelexHandler) ffestb_V0034_ (t);

	case FFELEX_typeSLASH:
	  ffesta_confirmed ();
	  return (ffelexHandler) ffestb_V0031_;
	}

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstSTRUCTURE)
	goto bad_0;		/* :::::::::::::::::::: */
      p = ffelex_token_text (ffesta_tokens[0]) + (i = FFESTR_firstlSTRUCTURE);
      switch (ffelex_token_type (t))
	{
	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();
	  break;

	case FFELEX_typeSLASH:
	  ffesta_confirmed ();
	  if (*p != '\0')
	    goto bad_1;		/* :::::::::::::::::::: */
	  return (ffelexHandler) ffestb_V0031_;

	case FFELEX_typeOPEN_PAREN:
	  break;
	}

      /* Here, we have at least one char after "STRUCTURE" and t is COMMA,
	 EOS/SEMICOLON, or OPEN_PAREN. */

      if (!ffesrc_is_name_init (*p))
	goto bad_i;		/* :::::::::::::::::::: */
      nt = ffelex_token_name_from_names (ffesta_tokens[0], i, 0);
      if (ffelex_token_type (t) == FFELEX_typeOPEN_PAREN)
	ffestb_local_.structure.started = FALSE;
      else
	{
	  if (!ffesta_is_inhibited ())
	    ffestc_V003_start (NULL);
	  ffestb_local_.structure.started = TRUE;
	}
      next = (ffelexHandler) ffestb_V0034_ (nt);
      ffelex_token_kill (nt);
      return (ffelexHandler) (*next) (t);

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "STRUCTURE", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "STRUCTURE", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */

bad_i:				/* :::::::::::::::::::: */
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, "STRUCTURE", ffesta_tokens[0], i, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0031_ -- "STRUCTURE" SLASH

   return ffestb_V0031_;  // to lexer

   Handle NAME.	 */

static ffelexHandler
ffestb_V0031_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_V0032_;

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "STRUCTURE", t);
      break;
    }

  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0032_ -- "STRUCTURE" SLASH NAME

   return ffestb_V0032_;  // to lexer

   Handle SLASH.  */

static ffelexHandler
ffestb_V0032_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeSLASH:
      if (!ffesta_is_inhibited ())
	ffestc_V003_start (ffesta_tokens[1]);
      ffestb_local_.structure.started = TRUE;
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffestb_V0033_;

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "STRUCTURE", t);
      break;
    }

  ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0033_ -- "STRUCTURE" SLASH NAME SLASH

   return ffestb_V0033_;  // to lexer

   Handle NAME or EOS/SEMICOLON.  */

static ffelexHandler
ffestb_V0033_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      return (ffelexHandler) ffestb_V0034_ (t);

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (!ffesta_is_inhibited ())
	ffestc_V003_finish ();
      return (ffelexHandler) ffesta_zero (t);

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "STRUCTURE", t);
      break;
    }

  ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0034_ -- "STRUCTURE" [SLASH NAME SLASH]

   return ffestb_V0034_;  // to lexer

   Handle NAME.	 */

static ffelexHandler
ffestb_V0034_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_V0035_;

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "STRUCTURE", t);
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_V003_finish ();
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0035_ -- "STRUCTURE" ... NAME

   return ffestb_V0035_;  // to lexer

   Handle OPEN_PAREN.  */

static ffelexHandler
ffestb_V0035_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeOPEN_PAREN:
      ffestb_subrargs_.dim_list.dims = ffestt_dimlist_create ();
      ffestb_subrargs_.dim_list.handler = (ffelexHandler) ffestb_V0036_;
      ffestb_subrargs_.dim_list.pool = ffesta_output_pool;
      ffestb_subrargs_.dim_list.ctx = FFEEXPR_contextDIMLISTCOMMON;
#ifdef FFECOM_dimensionsMAX
      ffestb_subrargs_.dim_list.ndims = 0;
#endif
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
      FFEEXPR_contextDIMLISTCOMMON, (ffeexprCallback) ffestb_subr_dimlist_);

    case FFELEX_typeCOMMA:
      if (!ffesta_is_inhibited ())
	ffestc_V003_item (ffesta_tokens[1], NULL);
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffestb_V0034_;

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (!ffesta_is_inhibited ())
	{
	  ffestc_V003_item (ffesta_tokens[1], NULL);
	  ffestc_V003_finish ();
	}
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffesta_zero (t);

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "STRUCTURE", t);
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_V003_finish ();
  ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0036_ -- "STRUCTURE" ... NAME OPEN_PAREN dimlist CLOSE_PAREN

   return ffestb_V0036_;  // to lexer

   Handle COMMA or EOS/SEMICOLON.  */

static ffelexHandler
ffestb_V0036_ (ffelexToken t)
{
  if (!ffestb_subrargs_.dim_list.ok)
    goto bad;			/* :::::::::::::::::::: */

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	{
	  if (!ffestb_local_.structure.started)
	    {
	      ffestc_V003_start (NULL);
	      ffestb_local_.structure.started = TRUE;
	    }
	  ffestc_V003_item (ffesta_tokens[1],
			    ffestb_subrargs_.dim_list.dims);
	}
      ffelex_token_kill (ffesta_tokens[1]);
      ffestt_dimlist_kill (ffestb_subrargs_.dim_list.dims);
      return (ffelexHandler) ffestb_V0034_;

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	{
	  if (!ffestb_local_.structure.started)
	    ffestc_V003_start (NULL);
	  ffestc_V003_item (ffesta_tokens[1],
			    ffestb_subrargs_.dim_list.dims);
	  ffestc_V003_finish ();
	}
      ffelex_token_kill (ffesta_tokens[1]);
      ffestt_dimlist_kill (ffestb_subrargs_.dim_list.dims);
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

bad:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "STRUCTURE", t);
  if (ffestb_local_.structure.started && !ffesta_is_inhibited ())
    ffestc_V003_finish ();
  ffestt_dimlist_kill (ffestb_subrargs_.dim_list.dims);
  ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V016 -- Parse the RECORD statement

   return ffestb_V016;	// to lexer

   Make sure the statement has a valid form for the RECORD statement.  If it
   does, implement the statement.  */

ffelexHandler
ffestb_V016 (ffelexToken t)
{
  const char *p;
  ffeTokenLength i;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstRECORD)
	goto bad_0;		/* :::::::::::::::::::: */
      break;

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstRECORD)
	goto bad_0;		/* :::::::::::::::::::: */
      p = ffelex_token_text (ffesta_tokens[0]) + (i = FFESTR_firstlRECORD);
      if (*p != '\0')
	goto bad_i;		/* :::::::::::::::::::: */
      break;

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
    case FFELEX_typeCOLONCOLON:
      ffesta_confirmed ();	/* Error, but clearly intended. */
      goto bad_1;		/* :::::::::::::::::::: */

    default:
      goto bad_1;		/* :::::::::::::::::::: */

    case FFELEX_typeSLASH:
      break;
    }

  ffesta_confirmed ();
  if (!ffesta_is_inhibited ())
    ffestc_V016_start ();
  return (ffelexHandler) ffestb_V0161_;

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "RECORD", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "RECORD", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */

bad_i:				/* :::::::::::::::::::: */
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, "RECORD", ffesta_tokens[0], i, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0161_ -- "RECORD" SLASH

   return ffestb_V0161_;  // to lexer

   Handle NAME.	 */

static ffelexHandler
ffestb_V0161_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      if (!ffesta_is_inhibited ())
	ffestc_V016_item_structure (t);
      return (ffelexHandler) ffestb_V0162_;

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "RECORD", t);
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_V016_finish ();
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0162_ -- "RECORD" SLASH NAME

   return ffestb_V0162_;  // to lexer

   Handle SLASH.  */

static ffelexHandler
ffestb_V0162_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeSLASH:
      return (ffelexHandler) ffestb_V0163_;

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "RECORD", t);
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_V016_finish ();
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0163_ -- "RECORD" SLASH NAME SLASH

   return ffestb_V0163_;  // to lexer

   Handle NAME.	 */

static ffelexHandler
ffestb_V0163_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_V0164_;

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "RECORD", t);
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_V016_finish ();
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0164_ -- "RECORD" ... NAME

   return ffestb_V0164_;  // to lexer

   Handle OPEN_PAREN.  */

static ffelexHandler
ffestb_V0164_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeOPEN_PAREN:
      ffestb_subrargs_.dim_list.dims = ffestt_dimlist_create ();
      ffestb_subrargs_.dim_list.handler = (ffelexHandler) ffestb_V0165_;
      ffestb_subrargs_.dim_list.pool = ffesta_output_pool;
      ffestb_subrargs_.dim_list.ctx = FFEEXPR_contextDIMLISTCOMMON;
#ifdef FFECOM_dimensionsMAX
      ffestb_subrargs_.dim_list.ndims = 0;
#endif
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
      FFEEXPR_contextDIMLISTCOMMON, (ffeexprCallback) ffestb_subr_dimlist_);

    case FFELEX_typeCOMMA:
      if (!ffesta_is_inhibited ())
	ffestc_V016_item_object (ffesta_tokens[1], NULL);
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffestb_V0166_;

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (!ffesta_is_inhibited ())
	{
	  ffestc_V016_item_object (ffesta_tokens[1], NULL);
	  ffestc_V016_finish ();
	}
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffesta_zero (t);

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "RECORD", t);
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_V016_finish ();
  ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0165_ -- "RECORD" ... NAME OPEN_PAREN dimlist CLOSE_PAREN

   return ffestb_V0165_;  // to lexer

   Handle COMMA or EOS/SEMICOLON.  */

static ffelexHandler
ffestb_V0165_ (ffelexToken t)
{
  if (!ffestb_subrargs_.dim_list.ok)
    goto bad;			/* :::::::::::::::::::: */

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      if (!ffesta_is_inhibited ())
	ffestc_V016_item_object (ffesta_tokens[1],
				 ffestb_subrargs_.dim_list.dims);
      ffelex_token_kill (ffesta_tokens[1]);
      ffestt_dimlist_kill (ffestb_subrargs_.dim_list.dims);
      return (ffelexHandler) ffestb_V0166_;

    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (!ffesta_is_inhibited ())
	{
	  ffestc_V016_item_object (ffesta_tokens[1],
				   ffestb_subrargs_.dim_list.dims);
	  ffestc_V016_finish ();
	}
      ffelex_token_kill (ffesta_tokens[1]);
      ffestt_dimlist_kill (ffestb_subrargs_.dim_list.dims);
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

bad:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "RECORD", t);
  if (ffestb_local_.structure.started && !ffesta_is_inhibited ())
    ffestc_V016_finish ();
  ffestt_dimlist_kill (ffestb_subrargs_.dim_list.dims);
  ffelex_token_kill (ffesta_tokens[1]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0166_ -- "RECORD" SLASH NAME SLASH NAME [OPEN_PAREN dimlist
		    CLOSE_PAREN] COMMA

   return ffestb_V0166_;  // to lexer

   Handle NAME or SLASH.  */

static ffelexHandler
ffestb_V0166_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_V0164_;

    case FFELEX_typeSLASH:
      return (ffelexHandler) ffestb_V0161_;

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "RECORD", t);
      break;
    }

  if (!ffesta_is_inhibited ())
    ffestc_V016_finish ();
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

#endif
/* ffestb_V027 -- Parse the VXT PARAMETER statement

   return ffestb_V027;	// to lexer

   Make sure the statement has a valid form for the VXT PARAMETER statement.
   If it does, implement the statement.	 */

ffelexHandler
ffestb_V027 (ffelexToken t)
{
  unsigned const char *p;
  ffeTokenLength i;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstPARAMETER)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeNAME:
	  break;

	default:
	  goto bad_1;		/* :::::::::::::::::::: */
	}
      ffesta_confirmed ();
      ffestb_local_.vxtparam.started = TRUE;
      if (!ffesta_is_inhibited ())
	ffestc_V027_start ();
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_V0271_;

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstPARAMETER)
	goto bad_0;		/* :::::::::::::::::::: */
      p = ffelex_token_text (ffesta_tokens[0]) + (i = FFESTR_firstlPARAMETER);
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeEQUALS:
	  break;

	default:
	  goto bad_1;		/* :::::::::::::::::::: */
	}
      if (!ffesrc_is_name_init (*p))
	goto bad_i;		/* :::::::::::::::::::: */
      ffestb_local_.vxtparam.started = FALSE;
      ffesta_tokens[1] = ffelex_token_name_from_names (ffesta_tokens[0], i,
						       0);
      return (ffelexHandler) ffestb_V0271_ (t);

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "PARAMETER", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "PARAMETER", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */

bad_i:				/* :::::::::::::::::::: */
  ffesta_ffebad_1sp (FFEBAD_INVALID_STMT_FORM, "PARAMETER", ffesta_tokens[0], i, t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0271_ -- "PARAMETER" NAME

   return ffestb_V0271_;  // to lexer

   Handle EQUALS.  */

static ffelexHandler
ffestb_V0271_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEQUALS:
      return (ffelexHandler) ffeexpr_rhs (ffesta_output_pool,
		 FFEEXPR_contextPARAMETER, (ffeexprCallback) ffestb_V0272_);

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "PARAMETER", t);
      break;
    }

  ffelex_token_kill (ffesta_tokens[1]);
  if (ffestb_local_.vxtparam.started && !ffesta_is_inhibited ())
    ffestc_V027_finish ();
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0272_ -- "PARAMETER" NAME EQUALS expr

   (ffestb_V0272_)  // to expression handler

   Handle COMMA or EOS/SEMICOLON.  */

static ffelexHandler
ffestb_V0272_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (!ffestb_local_.vxtparam.started)
	{
	  if (ffestc_is_let_not_V027 ())
	    break;		/* Not a valid VXTPARAMETER stmt. */
	  ffesta_confirmed ();
	  if (!ffesta_is_inhibited ())
	    ffestc_V027_start ();
	  ffestb_local_.vxtparam.started = TRUE;
	}
      if (expr == NULL)
	break;
      if (!ffesta_is_inhibited ())
	{
	  ffestc_V027_item (ffesta_tokens[1], expr, ft);
	  ffestc_V027_finish ();
	}
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffesta_zero (t);

    case FFELEX_typeCOMMA:
      ffesta_confirmed ();
      if (!ffestb_local_.vxtparam.started)
	{
	  if (!ffesta_is_inhibited ())
	    ffestc_V027_start ();
	  ffestb_local_.vxtparam.started = TRUE;
	}
      if (expr == NULL)
	break;
      if (!ffesta_is_inhibited ())
	ffestc_V027_item (ffesta_tokens[1], expr, ft);
      ffelex_token_kill (ffesta_tokens[1]);
      return (ffelexHandler) ffestb_V0273_;

    default:
      break;
    }

  ffelex_token_kill (ffesta_tokens[1]);
  if (ffestb_local_.vxtparam.started && !ffesta_is_inhibited ())
    ffestc_V027_finish ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "PARAMETER", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_V0273_ -- "PARAMETER" NAME EQUALS expr COMMA

   return ffestb_V0273_;  // to lexer

   Handle NAME.	 */

static ffelexHandler
ffestb_V0273_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_V0271_;

    default:
      ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "PARAMETER", t);
      break;
    }

  if (ffestb_local_.vxtparam.started && !ffesta_is_inhibited ())
    ffestc_V027_finish ();
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_R539 -- Parse the IMPLICIT FUNCTION statement

   return ffestb_decl_R539;  // to lexer

   Make sure the statement has a valid form for the IMPLICIT
   statement.  If it does, implement the statement.  */

ffelexHandler
ffestb_decl_R539 (ffelexToken t)
{
  ffeTokenLength i;
  unsigned const char *p;
  ffelexToken nt;
  ffestrSecond kw;

  ffestb_local_.decl.recursive = NULL;

  switch (ffelex_token_type (ffesta_tokens[0]))
    {
    case FFELEX_typeNAME:
      if (ffesta_first_kw != FFESTR_firstIMPLICIT)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeEOS:
	case FFELEX_typeSEMICOLON:
	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	  ffesta_confirmed ();	/* Error, but clearly intended. */
	  goto bad_1;		/* :::::::::::::::::::: */

	default:
	  goto bad_1;		/* :::::::::::::::::::: */

	case FFELEX_typeNAME:
	  break;
	}
      ffesta_confirmed ();
      ffestb_local_.decl.imp_started = FALSE;
      switch (ffesta_second_kw)
	{
	case FFESTR_secondINTEGER:
	  ffestb_local_.decl.type = FFESTP_typeINTEGER;
	  return (ffelexHandler) ffestb_decl_R5391_;

	case FFESTR_secondBYTE:
	  ffestb_local_.decl.type = FFESTP_typeBYTE;
	  return (ffelexHandler) ffestb_decl_R5391_;

	case FFESTR_secondWORD:
	  ffestb_local_.decl.type = FFESTP_typeWORD;
	  return (ffelexHandler) ffestb_decl_R5391_;

	case FFESTR_secondREAL:
	  ffestb_local_.decl.type = FFESTP_typeREAL;
	  return (ffelexHandler) ffestb_decl_R5391_;

	case FFESTR_secondCOMPLEX:
	  ffestb_local_.decl.type = FFESTP_typeCOMPLEX;
	  return (ffelexHandler) ffestb_decl_R5391_;

	case FFESTR_secondLOGICAL:
	  ffestb_local_.decl.type = FFESTP_typeLOGICAL;
	  return (ffelexHandler) ffestb_decl_R5391_;

	case FFESTR_secondCHARACTER:
	  ffestb_local_.decl.type = FFESTP_typeCHARACTER;
	  return (ffelexHandler) ffestb_decl_R5391_;

	case FFESTR_secondDOUBLE:
	  return (ffelexHandler) ffestb_decl_R5392_;

	case FFESTR_secondDOUBLEPRECISION:
	  ffestb_local_.decl.type = FFESTP_typeDBLPRCSN;
	  ffestb_local_.decl.kind = NULL;
	  ffestb_local_.decl.kindt = NULL;
	  ffestb_local_.decl.len = NULL;
	  ffestb_local_.decl.lent = NULL;
	  return (ffelexHandler) ffestb_decl_R539letters_;

	case FFESTR_secondDOUBLECOMPLEX:
	  ffestb_local_.decl.type = FFESTP_typeDBLCMPLX;
	  ffestb_local_.decl.kind = NULL;
	  ffestb_local_.decl.kindt = NULL;
	  ffestb_local_.decl.len = NULL;
	  ffestb_local_.decl.lent = NULL;
	  return (ffelexHandler) ffestb_decl_R539letters_;

	case FFESTR_secondNONE:
	  return (ffelexHandler) ffestb_decl_R5394_;

#if FFESTR_F90
	case FFESTR_secondTYPE:
	  ffestb_local_.decl.type = FFESTP_typeTYPE;
	  return (ffelexHandler) ffestb_decl_R5393_;
#endif

	default:
	  goto bad_1;		/* :::::::::::::::::::: */
	}

    case FFELEX_typeNAMES:
      if (ffesta_first_kw != FFESTR_firstIMPLICIT)
	goto bad_0;		/* :::::::::::::::::::: */
      switch (ffelex_token_type (t))
	{
	case FFELEX_typeCOMMA:
	case FFELEX_typeCOLONCOLON:
	case FFELEX_typeASTERISK:
	case FFELEX_typeSEMICOLON:
	case FFELEX_typeEOS:
	  ffesta_confirmed ();
	  break;

	case FFELEX_typeOPEN_PAREN:
	  break;

	default:
	  goto bad_1;		/* :::::::::::::::::::: */
	}
      p = ffelex_token_text (ffesta_tokens[0]) + (i = FFESTR_firstlIMPLICIT);
      if (!ffesrc_is_name_init (*p))
	goto bad_0;		/* :::::::::::::::::::: */
      ffestb_local_.decl.imp_started = FALSE;
      nt = ffelex_token_name_from_names (ffesta_tokens[0],
					 FFESTR_firstlIMPLICIT, 0);
      kw = ffestr_second (nt);
      ffelex_token_kill (nt);
      switch (kw)
	{
	case FFESTR_secondINTEGER:
	  ffestb_local_.decl.type = FFESTP_typeINTEGER;
	  return (ffelexHandler) ffestb_decl_R5391_ (t);

	case FFESTR_secondBYTE:
	  ffestb_local_.decl.type = FFESTP_typeBYTE;
	  return (ffelexHandler) ffestb_decl_R5391_ (t);

	case FFESTR_secondWORD:
	  ffestb_local_.decl.type = FFESTP_typeWORD;
	  return (ffelexHandler) ffestb_decl_R5391_ (t);

	case FFESTR_secondREAL:
	  ffestb_local_.decl.type = FFESTP_typeREAL;
	  return (ffelexHandler) ffestb_decl_R5391_ (t);

	case FFESTR_secondCOMPLEX:
	  ffestb_local_.decl.type = FFESTP_typeCOMPLEX;
	  return (ffelexHandler) ffestb_decl_R5391_ (t);

	case FFESTR_secondLOGICAL:
	  ffestb_local_.decl.type = FFESTP_typeLOGICAL;
	  return (ffelexHandler) ffestb_decl_R5391_ (t);

	case FFESTR_secondCHARACTER:
	  ffestb_local_.decl.type = FFESTP_typeCHARACTER;
	  return (ffelexHandler) ffestb_decl_R5391_ (t);

	case FFESTR_secondDOUBLEPRECISION:
	  ffestb_local_.decl.type = FFESTP_typeDBLPRCSN;
	  ffestb_local_.decl.kind = NULL;
	  ffestb_local_.decl.kindt = NULL;
	  ffestb_local_.decl.len = NULL;
	  ffestb_local_.decl.lent = NULL;
	  return (ffelexHandler) ffestb_decl_R539letters_ (t);

	case FFESTR_secondDOUBLECOMPLEX:
	  ffestb_local_.decl.type = FFESTP_typeDBLCMPLX;
	  ffestb_local_.decl.kind = NULL;
	  ffestb_local_.decl.kindt = NULL;
	  ffestb_local_.decl.len = NULL;
	  ffestb_local_.decl.lent = NULL;
	  return (ffelexHandler) ffestb_decl_R539letters_ (t);

	case FFESTR_secondNONE:
	  return (ffelexHandler) ffestb_decl_R5394_ (t);

#if FFESTR_F90
	case FFESTR_secondTYPE:
	  ffestb_local_.decl.type = FFESTP_typeTYPE;
	  return (ffelexHandler) ffestb_decl_R5393_ (t);
#endif

	default:
	  goto bad_1;		/* :::::::::::::::::::: */
	}

    default:
      goto bad_0;		/* :::::::::::::::::::: */
    }

bad_0:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "IMPLICIT", ffesta_tokens[0]);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);

bad_1:				/* :::::::::::::::::::: */
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "IMPLICIT", t);
  return (ffelexHandler) ffelex_swallow_tokens (t,
						(ffelexHandler) ffesta_zero);	/* Invalid second token. */
}

/* ffestb_decl_R5391_ -- "IMPLICIT" generic-type

   return ffestb_decl_R5391_;  // to lexer

   Handle ASTERISK or OPEN_PAREN.  */

static ffelexHandler
ffestb_decl_R5391_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeASTERISK:
      ffesta_confirmed ();
      ffestb_local_.decl.handler = (ffelexHandler) ffestb_decl_R539letters_;
      ffestb_local_.decl.badname = "IMPLICIT";
      if (ffestb_local_.decl.type == FFESTP_typeCHARACTER)
	return (ffelexHandler) ffestb_decl_starlen_;
      return (ffelexHandler) ffestb_decl_starkind_;

    case FFELEX_typeOPEN_PAREN:
      ffestb_local_.decl.handler = (ffelexHandler) ffestb_decl_R539letters_;
      ffestb_local_.decl.badname = "IMPLICIT";
      ffestb_local_.decl.kind = NULL;
      ffestb_local_.decl.kindt = NULL;
      ffestb_local_.decl.len = NULL;
      ffestb_local_.decl.lent = NULL;
      if (ffestb_local_.decl.type == FFESTP_typeCHARACTER)
	ffestb_local_.decl.imp_handler
	  = (ffelexHandler) ffestb_decl_typeparams_;
      else
	ffestb_local_.decl.imp_handler
	  = (ffelexHandler) ffestb_decl_kindparam_;
      return (ffelexHandler) ffestb_decl_R539maybe_ (t);

    default:
      break;
    }

  if (ffestb_local_.decl.imp_started && !ffesta_is_inhibited ())
    ffestc_R539finish ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "IMPLICIT", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_R5392_ -- "IMPLICIT" "DOUBLE"

   return ffestb_decl_R5392_;  // to lexer

   Handle NAME.	 */

static ffelexHandler
ffestb_decl_R5392_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      switch (ffestr_second (t))
	{
	case FFESTR_secondPRECISION:
	  ffestb_local_.decl.type = FFESTP_typeDBLPRCSN;
	  break;

	case FFESTR_secondCOMPLEX:
	  ffestb_local_.decl.type = FFESTP_typeDBLCMPLX;
	  break;

	default:
	  goto bad;		/* :::::::::::::::::::: */
	}
      ffestb_local_.decl.kind = NULL;
      ffestb_local_.decl.kindt = NULL;
      ffestb_local_.decl.len = NULL;
      ffestb_local_.decl.lent = NULL;
      return (ffelexHandler) ffestb_decl_R539letters_;

    default:
      break;
    }

bad:				/* :::::::::::::::::::: */
  if (ffestb_local_.decl.imp_started && !ffesta_is_inhibited ())
    ffestc_R539finish ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "IMPLICIT", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_R5393_ -- "IMPLICIT" "TYPE"

   return ffestb_decl_R5393_;  // to lexer

   Handle OPEN_PAREN.  */

#if FFESTR_F90
static ffelexHandler
ffestb_decl_R5393_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeOPEN_PAREN:
      ffestb_local_.decl.handler = (ffelexHandler) ffestb_decl_R539letters_;
      ffestb_local_.decl.badname = "IMPLICIT";
      return (ffelexHandler) ffestb_decl_typetype1_;

    default:
      break;
    }

  if (ffestb_local_.decl.imp_started && !ffesta_is_inhibited ())
    ffestc_R539finish ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "IMPLICIT", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

#endif
/* ffestb_decl_R5394_ -- "IMPLICIT" "NONE"

   return ffestb_decl_R5394_;  // to lexer

   Handle EOS/SEMICOLON.  */

static ffelexHandler
ffestb_decl_R5394_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffesta_confirmed ();
      if (!ffesta_is_inhibited ())
	ffestc_R539 ();		/* IMPLICIT NONE. */
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "IMPLICIT", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_R5395_ -- "IMPLICIT" implicit-spec-list COMMA

   return ffestb_decl_R5395_;  // to lexer

   Handle NAME for next type-spec.  */

static ffelexHandler
ffestb_decl_R5395_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      switch (ffestr_second (t))
	{
	case FFESTR_secondINTEGER:
	  ffestb_local_.decl.type = FFESTP_typeINTEGER;
	  return (ffelexHandler) ffestb_decl_R5391_;

	case FFESTR_secondBYTE:
	  ffestb_local_.decl.type = FFESTP_typeBYTE;
	  return (ffelexHandler) ffestb_decl_R5391_;

	case FFESTR_secondWORD:
	  ffestb_local_.decl.type = FFESTP_typeWORD;
	  return (ffelexHandler) ffestb_decl_R5391_;

	case FFESTR_secondREAL:
	  ffestb_local_.decl.type = FFESTP_typeREAL;
	  return (ffelexHandler) ffestb_decl_R5391_;

	case FFESTR_secondCOMPLEX:
	  ffestb_local_.decl.type = FFESTP_typeCOMPLEX;
	  return (ffelexHandler) ffestb_decl_R5391_;

	case FFESTR_secondLOGICAL:
	  ffestb_local_.decl.type = FFESTP_typeLOGICAL;
	  return (ffelexHandler) ffestb_decl_R5391_;

	case FFESTR_secondCHARACTER:
	  ffestb_local_.decl.type = FFESTP_typeCHARACTER;
	  return (ffelexHandler) ffestb_decl_R5391_;

	case FFESTR_secondDOUBLE:
	  return (ffelexHandler) ffestb_decl_R5392_;

	case FFESTR_secondDOUBLEPRECISION:
	  ffestb_local_.decl.type = FFESTP_typeDBLPRCSN;
	  ffestb_local_.decl.kind = NULL;
	  ffestb_local_.decl.kindt = NULL;
	  ffestb_local_.decl.len = NULL;
	  ffestb_local_.decl.lent = NULL;
	  return (ffelexHandler) ffestb_decl_R539letters_;

	case FFESTR_secondDOUBLECOMPLEX:
	  ffestb_local_.decl.type = FFESTP_typeDBLCMPLX;
	  ffestb_local_.decl.kind = NULL;
	  ffestb_local_.decl.kindt = NULL;
	  ffestb_local_.decl.len = NULL;
	  ffestb_local_.decl.lent = NULL;
	  return (ffelexHandler) ffestb_decl_R539letters_;

#if FFESTR_F90
	case FFESTR_secondTYPE:
	  ffestb_local_.decl.type = FFESTP_typeTYPE;
	  return (ffelexHandler) ffestb_decl_R5393_;
#endif

	default:
	  break;
	}
      break;

    default:
      break;
    }

  if (ffestb_local_.decl.imp_started && !ffesta_is_inhibited ())
    ffestc_R539finish ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "IMPLICIT", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_R539letters_ -- "IMPLICIT" type-spec

   return ffestb_decl_R539letters_;  // to lexer

   Handle OPEN_PAREN.  */

static ffelexHandler
ffestb_decl_R539letters_ (ffelexToken t)
{
  ffelex_set_names (FALSE);

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeOPEN_PAREN:
      ffestb_local_.decl.imps = ffestt_implist_create ();
      return (ffelexHandler) ffestb_decl_R539letters_1_;

    default:
      break;
    }

  if (ffestb_local_.decl.kindt != NULL)
    ffelex_token_kill (ffestb_local_.decl.kindt);
  if (ffestb_local_.decl.lent != NULL)
    ffelex_token_kill (ffestb_local_.decl.lent);
  if (ffestb_local_.decl.imp_started && !ffesta_is_inhibited ())
    ffestc_R539finish ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "IMPLICIT", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_R539letters_1_ -- "IMPLICIT" type-spec OPEN_PAREN

   return ffestb_decl_R539letters_1_;  // to lexer

   Handle NAME.	 */

static ffelexHandler
ffestb_decl_R539letters_1_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      if (ffelex_token_length (t) != 1)
	break;
      ffesta_tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffestb_decl_R539letters_2_;

    default:
      break;
    }

  ffestt_implist_kill (ffestb_local_.decl.imps);
  if (ffestb_local_.decl.kindt != NULL)
    ffelex_token_kill (ffestb_local_.decl.kindt);
  if (ffestb_local_.decl.lent != NULL)
    ffelex_token_kill (ffestb_local_.decl.lent);
  if (ffestb_local_.decl.imp_started && !ffesta_is_inhibited ())
    ffestc_R539finish ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "IMPLICIT", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_R539letters_2_ -- "IMPLICIT" type-spec OPEN_PAREN NAME

   return ffestb_decl_R539letters_2_;  // to lexer

   Handle COMMA or MINUS.  */

static ffelexHandler
ffestb_decl_R539letters_2_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      ffestt_implist_append (ffestb_local_.decl.imps, ffesta_tokens[1], NULL);
      return (ffelexHandler) ffestb_decl_R539letters_1_;

    case FFELEX_typeCLOSE_PAREN:
      ffestt_implist_append (ffestb_local_.decl.imps, ffesta_tokens[1], NULL);
      return (ffelexHandler) ffestb_decl_R539letters_5_;

    case FFELEX_typeMINUS:
      return (ffelexHandler) ffestb_decl_R539letters_3_;

    default:
      break;
    }

  ffelex_token_kill (ffesta_tokens[1]);
  ffestt_implist_kill (ffestb_local_.decl.imps);
  if (ffestb_local_.decl.kindt != NULL)
    ffelex_token_kill (ffestb_local_.decl.kindt);
  if (ffestb_local_.decl.lent != NULL)
    ffelex_token_kill (ffestb_local_.decl.lent);
  if (ffestb_local_.decl.imp_started && !ffesta_is_inhibited ())
    ffestc_R539finish ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "IMPLICIT", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_R539letters_3_ -- "IMPLICIT" type-spec OPEN_PAREN NAME MINUS

   return ffestb_decl_R539letters_3_;  // to lexer

   Handle NAME.	 */

static ffelexHandler
ffestb_decl_R539letters_3_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      if (ffelex_token_length (t) != 1)
	break;
      ffestt_implist_append (ffestb_local_.decl.imps, ffesta_tokens[1],
			     ffelex_token_use (t));
      return (ffelexHandler) ffestb_decl_R539letters_4_;

    default:
      break;
    }

  ffelex_token_kill (ffesta_tokens[1]);
  ffestt_implist_kill (ffestb_local_.decl.imps);
  if (ffestb_local_.decl.kindt != NULL)
    ffelex_token_kill (ffestb_local_.decl.kindt);
  if (ffestb_local_.decl.lent != NULL)
    ffelex_token_kill (ffestb_local_.decl.lent);
  if (ffestb_local_.decl.imp_started && !ffesta_is_inhibited ())
    ffestc_R539finish ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "IMPLICIT", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_R539letters_4_ -- "IMPLICIT" type-spec OPEN_PAREN NAME MINUS
				 NAME

   return ffestb_decl_R539letters_4_;  // to lexer

   Handle COMMA or CLOSE_PAREN.	 */

static ffelexHandler
ffestb_decl_R539letters_4_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      return (ffelexHandler) ffestb_decl_R539letters_1_;

    case FFELEX_typeCLOSE_PAREN:
      return (ffelexHandler) ffestb_decl_R539letters_5_;

    default:
      break;
    }

  ffestt_implist_kill (ffestb_local_.decl.imps);
  if (ffestb_local_.decl.kindt != NULL)
    ffelex_token_kill (ffestb_local_.decl.kindt);
  if (ffestb_local_.decl.lent != NULL)
    ffelex_token_kill (ffestb_local_.decl.lent);
  if (ffestb_local_.decl.imp_started && !ffesta_is_inhibited ())
    ffestc_R539finish ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "IMPLICIT", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_R539letters_5_ -- "IMPLICIT" type-spec OPEN_PAREN
				 letter-spec-list CLOSE_PAREN

   return ffestb_decl_R539letters_5_;  // to lexer

   Handle COMMA or EOS/SEMICOLON.  */

static ffelexHandler
ffestb_decl_R539letters_5_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      if (!ffestb_local_.decl.imp_started)
	{
	  ffestb_local_.decl.imp_started = TRUE;
	  ffesta_confirmed ();
	  if (!ffesta_is_inhibited ())
	    ffestc_R539start ();
	}
      if (!ffesta_is_inhibited ())
	ffestc_R539item (ffestb_local_.decl.type, ffestb_local_.decl.kind,
			 ffestb_local_.decl.kindt, ffestb_local_.decl.len,
			 ffestb_local_.decl.lent, ffestb_local_.decl.imps);
      if (ffestb_local_.decl.kindt != NULL)
	ffelex_token_kill (ffestb_local_.decl.kindt);
      if (ffestb_local_.decl.lent != NULL)
	ffelex_token_kill (ffestb_local_.decl.lent);
      ffestt_implist_kill (ffestb_local_.decl.imps);
      if (ffelex_token_type (t) == FFELEX_typeCOMMA)
	return (ffelexHandler) ffestb_decl_R5395_;
      if (!ffesta_is_inhibited ())
	ffestc_R539finish ();
      return (ffelexHandler) ffesta_zero (t);

    default:
      break;
    }

  ffestt_implist_kill (ffestb_local_.decl.imps);
  if (ffestb_local_.decl.kindt != NULL)
    ffelex_token_kill (ffestb_local_.decl.kindt);
  if (ffestb_local_.decl.lent != NULL)
    ffelex_token_kill (ffestb_local_.decl.lent);
  if (ffestb_local_.decl.imp_started && !ffesta_is_inhibited ())
    ffestc_R539finish ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "IMPLICIT", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}

/* ffestb_decl_R539maybe_ -- "IMPLICIT" generic-type-spec

   return ffestb_decl_R539maybe_;  // to lexer

   Handle OPEN_PAREN.  */

static ffelexHandler
ffestb_decl_R539maybe_ (ffelexToken t)
{
  assert (ffelex_token_type (t) == FFELEX_typeOPEN_PAREN);
  ffestb_local_.decl.imps = ffestt_implist_create ();
  ffestb_local_.decl.toklist = ffestt_tokenlist_create ();
  ffestb_local_.decl.imp_seen_comma
    = (ffestb_local_.decl.type != FFESTP_typeCHARACTER);
  return (ffelexHandler) ffestb_decl_R539maybe_1_;
}

/* ffestb_decl_R539maybe_1_ -- "IMPLICIT" generic-type-spec OPEN_PAREN

   return ffestb_decl_R539maybe_1_;  // to lexer

   Handle NAME.	 */

static ffelexHandler
ffestb_decl_R539maybe_1_ (ffelexToken t)
{
  ffelexHandler next;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      if (ffelex_token_length (t) != 1)
	break;
      ffesta_tokens[1] = ffelex_token_use (t);
      ffestt_tokenlist_append (ffestb_local_.decl.toklist, ffelex_token_use (t));
      return (ffelexHandler) ffestb_decl_R539maybe_2_;

    default:
      break;
    }

  ffestt_implist_kill (ffestb_local_.decl.imps);
  next = (ffelexHandler) ffestt_tokenlist_handle (ffestb_local_.decl.toklist,
			    (ffelexHandler) ffestb_local_.decl.imp_handler);
  ffestt_tokenlist_kill (ffestb_local_.decl.toklist);
  return (ffelexHandler) (*next) (t);
}

/* ffestb_decl_R539maybe_2_ -- "IMPLICIT" generic-type-spec OPEN_PAREN NAME

   return ffestb_decl_R539maybe_2_;  // to lexer

   Handle COMMA or MINUS.  */

static ffelexHandler
ffestb_decl_R539maybe_2_ (ffelexToken t)
{
  ffelexHandler next;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      ffestt_implist_append (ffestb_local_.decl.imps, ffesta_tokens[1], NULL);
      if (ffestb_local_.decl.imp_seen_comma)
	{
	  ffestt_tokenlist_kill (ffestb_local_.decl.toklist);
	  return (ffelexHandler) ffestb_decl_R539letters_1_;
	}
      ffestb_local_.decl.imp_seen_comma = TRUE;
      ffestt_tokenlist_append (ffestb_local_.decl.toklist, ffelex_token_use (t));
      return (ffelexHandler) ffestb_decl_R539maybe_1_;

    case FFELEX_typeCLOSE_PAREN:
      ffestt_implist_append (ffestb_local_.decl.imps, ffesta_tokens[1], NULL);
      ffestt_tokenlist_append (ffestb_local_.decl.toklist, ffelex_token_use (t));
      return (ffelexHandler) ffestb_decl_R539maybe_5_;

    case FFELEX_typeMINUS:
      ffestt_tokenlist_append (ffestb_local_.decl.toklist, ffelex_token_use (t));
      return (ffelexHandler) ffestb_decl_R539maybe_3_;

    default:
      break;
    }

  ffelex_token_kill (ffesta_tokens[1]);
  ffestt_implist_kill (ffestb_local_.decl.imps);
  next = (ffelexHandler) ffestt_tokenlist_handle (ffestb_local_.decl.toklist,
			    (ffelexHandler) ffestb_local_.decl.imp_handler);
  ffestt_tokenlist_kill (ffestb_local_.decl.toklist);
  return (ffelexHandler) (*next) (t);
}

/* ffestb_decl_R539maybe_3_ -- "IMPLICIT" type-spec OPEN_PAREN NAME MINUS

   return ffestb_decl_R539maybe_3_;  // to lexer

   Handle NAME.	 */

static ffelexHandler
ffestb_decl_R539maybe_3_ (ffelexToken t)
{
  ffelexHandler next;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      if (ffelex_token_length (t) != 1)
	break;
      ffestt_implist_append (ffestb_local_.decl.imps, ffesta_tokens[1],
			     ffelex_token_use (t));
      ffestt_tokenlist_append (ffestb_local_.decl.toklist, ffelex_token_use (t));
      return (ffelexHandler) ffestb_decl_R539maybe_4_;

    default:
      break;
    }

  ffelex_token_kill (ffesta_tokens[1]);
  ffestt_implist_kill (ffestb_local_.decl.imps);
  next = (ffelexHandler) ffestt_tokenlist_handle (ffestb_local_.decl.toklist,
			    (ffelexHandler) ffestb_local_.decl.imp_handler);
  ffestt_tokenlist_kill (ffestb_local_.decl.toklist);
  return (ffelexHandler) (*next) (t);
}

/* ffestb_decl_R539maybe_4_ -- "IMPLICIT" type-spec OPEN_PAREN NAME MINUS
				 NAME

   return ffestb_decl_R539maybe_4_;  // to lexer

   Handle COMMA or CLOSE_PAREN.	 */

static ffelexHandler
ffestb_decl_R539maybe_4_ (ffelexToken t)
{
  ffelexHandler next;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      if (ffestb_local_.decl.imp_seen_comma)
	{
	  ffestt_tokenlist_kill (ffestb_local_.decl.toklist);
	  return (ffelexHandler) ffestb_decl_R539letters_1_;
	}
      ffestb_local_.decl.imp_seen_comma = TRUE;
      ffestt_tokenlist_append (ffestb_local_.decl.toklist, ffelex_token_use (t));
      return (ffelexHandler) ffestb_decl_R539maybe_1_;

    case FFELEX_typeCLOSE_PAREN:
      ffestt_tokenlist_append (ffestb_local_.decl.toklist, ffelex_token_use (t));
      return (ffelexHandler) ffestb_decl_R539maybe_5_;

    default:
      break;
    }

  ffestt_implist_kill (ffestb_local_.decl.imps);
  next = (ffelexHandler) ffestt_tokenlist_handle (ffestb_local_.decl.toklist,
			    (ffelexHandler) ffestb_local_.decl.imp_handler);
  ffestt_tokenlist_kill (ffestb_local_.decl.toklist);
  return (ffelexHandler) (*next) (t);
}

/* ffestb_decl_R539maybe_5_ -- "IMPLICIT" type-spec OPEN_PAREN
				 letter-spec-list CLOSE_PAREN

   return ffestb_decl_R539maybe_5_;  // to lexer

   Handle COMMA or EOS/SEMICOLON.  */

static ffelexHandler
ffestb_decl_R539maybe_5_ (ffelexToken t)
{
  ffelexHandler next;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      ffestt_tokenlist_kill (ffestb_local_.decl.toklist);
      if (!ffestb_local_.decl.imp_started)
	{
	  ffestb_local_.decl.imp_started = TRUE;
	  ffesta_confirmed ();
	  if (!ffesta_is_inhibited ())
	    ffestc_R539start ();
	}
      if (!ffesta_is_inhibited ())
	ffestc_R539item (ffestb_local_.decl.type, ffestb_local_.decl.kind,
			 ffestb_local_.decl.kindt, ffestb_local_.decl.len,
			 ffestb_local_.decl.lent, ffestb_local_.decl.imps);
      if (ffestb_local_.decl.kindt != NULL)
	ffelex_token_kill (ffestb_local_.decl.kindt);
      if (ffestb_local_.decl.lent != NULL)
	ffelex_token_kill (ffestb_local_.decl.lent);
      ffestt_implist_kill (ffestb_local_.decl.imps);
      if (ffelex_token_type (t) == FFELEX_typeCOMMA)
	return (ffelexHandler) ffestb_decl_R5395_;
      if (!ffesta_is_inhibited ())
	ffestc_R539finish ();
      return (ffelexHandler) ffesta_zero (t);

    case FFELEX_typeOPEN_PAREN:
      ffesta_confirmed ();
      ffestt_implist_kill (ffestb_local_.decl.imps);
      next = (ffelexHandler) ffestt_tokenlist_handle (ffestb_local_.decl.toklist,
			    (ffelexHandler) ffestb_local_.decl.imp_handler);
      ffestt_tokenlist_kill (ffestb_local_.decl.toklist);
      return (ffelexHandler) (*next) (t);

    default:
      break;
    }

  ffestt_implist_kill (ffestb_local_.decl.imps);
  ffestt_tokenlist_kill (ffestb_local_.decl.toklist);
  if (ffestb_local_.decl.kindt != NULL)
    ffelex_token_kill (ffestb_local_.decl.kindt);
  if (ffestb_local_.decl.lent != NULL)
    ffelex_token_kill (ffestb_local_.decl.lent);
  if (ffestb_local_.decl.imp_started && !ffesta_is_inhibited ())
    ffestc_R539finish ();
  ffesta_ffebad_1st (FFEBAD_INVALID_STMT_FORM, "IMPLICIT", t);
  return (ffelexHandler) ffelex_swallow_tokens (t, (ffelexHandler) ffesta_zero);
}
