/* std.c -- Implementation File (module.c template V1.0)
   Copyright (C) 1995, 1996, 2000, 2002 Free Software Foundation, Inc.
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
      Implements the various statements and such like.

   Modifications:
      21-Nov-91	 JCB  2.0
	 Split out actual code generation to ffeste.
*/

/* Include files. */

#include "proj.h"
#include "std.h"
#include "bld.h"
#include "com.h"
#include "lab.h"
#include "lex.h"
#include "malloc.h"
#include "sta.h"
#include "ste.h"
#include "stp.h"
#include "str.h"
#include "sts.h"
#include "stt.h"
#include "stv.h"
#include "stw.h"
#include "symbol.h"
#include "target.h"

/* Externals defined here. */


/* Simple definitions and enumerations. */

#define FFESTD_COPY_EASY_ 1	/* 1 for only one _subr_copy_xyz_ fn. */

#define FFESTD_IS_END_OPTIMIZED_ 1	/* 0=always gen STOP/RETURN before
					   END. */

typedef enum
  {
    FFESTD_stateletSIMPLE_,	/* Expecting simple/start. */
    FFESTD_stateletATTRIB_,	/* Expecting attrib/item/itemstart. */
    FFESTD_stateletITEM_,	/* Expecting item/itemstart/finish. */
    FFESTD_stateletITEMVALS_,	/* Expecting itemvalue/itemendvals. */
    FFESTD_
  } ffestdStatelet_;

typedef enum
  {
    FFESTD_stmtidENDDOLOOP_,
    FFESTD_stmtidENDLOGIF_,
    FFESTD_stmtidEXECLABEL_,
    FFESTD_stmtidFORMATLABEL_,
    FFESTD_stmtidR737A_,	/* let */
    FFESTD_stmtidR803_,		/* IF-block */
    FFESTD_stmtidR804_,		/* ELSE IF */
    FFESTD_stmtidR805_,		/* ELSE */
    FFESTD_stmtidR806_,		/* END IF */
    FFESTD_stmtidR807_,		/* IF-logical */
    FFESTD_stmtidR809_,		/* SELECT CASE */
    FFESTD_stmtidR810_,		/* CASE */
    FFESTD_stmtidR811_,		/* END SELECT */
    FFESTD_stmtidR819A_,	/* DO-iterative */
    FFESTD_stmtidR819B_,	/* DO WHILE */
    FFESTD_stmtidR825_,		/* END DO */
    FFESTD_stmtidR834_,		/* CYCLE */
    FFESTD_stmtidR835_,		/* EXIT */
    FFESTD_stmtidR836_,		/* GOTO */
    FFESTD_stmtidR837_,		/* GOTO-computed */
    FFESTD_stmtidR838_,		/* ASSIGN */
    FFESTD_stmtidR839_,		/* GOTO-assigned */
    FFESTD_stmtidR840_,		/* IF-arithmetic */
    FFESTD_stmtidR841_,		/* CONTINUE */
    FFESTD_stmtidR842_,		/* STOP */
    FFESTD_stmtidR843_,		/* PAUSE */
    FFESTD_stmtidR904_,		/* OPEN */
    FFESTD_stmtidR907_,		/* CLOSE */
    FFESTD_stmtidR909_,		/* READ */
    FFESTD_stmtidR910_,		/* WRITE */
    FFESTD_stmtidR911_,		/* PRINT */
    FFESTD_stmtidR919_,		/* BACKSPACE */
    FFESTD_stmtidR920_,		/* ENDFILE */
    FFESTD_stmtidR921_,		/* REWIND */
    FFESTD_stmtidR923A_,	/* INQUIRE */
    FFESTD_stmtidR923B_,	/* INQUIRE-iolength */
    FFESTD_stmtidR1001_,	/* FORMAT */
    FFESTD_stmtidR1103_,	/* END_PROGRAM */
    FFESTD_stmtidR1112_,	/* END_BLOCK_DATA */
    FFESTD_stmtidR1212_,	/* CALL */
    FFESTD_stmtidR1221_,	/* END_FUNCTION */
    FFESTD_stmtidR1225_,	/* END_SUBROUTINE */
    FFESTD_stmtidR1226_,	/* ENTRY */
    FFESTD_stmtidR1227_,	/* RETURN */
#if FFESTR_VXT
    FFESTD_stmtidV018_,		/* REWRITE */
    FFESTD_stmtidV019_,		/* ACCEPT */
#endif
    FFESTD_stmtidV020_,		/* TYPE */
#if FFESTR_VXT
    FFESTD_stmtidV021_,		/* DELETE */
    FFESTD_stmtidV022_,		/* UNLOCK */
    FFESTD_stmtidV023_,		/* ENCODE */
    FFESTD_stmtidV024_,		/* DECODE */
    FFESTD_stmtidV025start_,	/* DEFINEFILE (start) */
    FFESTD_stmtidV025item_,	/* (DEFINEFILE item) */
    FFESTD_stmtidV025finish_,	/* (DEFINEFILE finish) */
    FFESTD_stmtidV026_,		/* FIND */
#endif
    FFESTD_stmtid_,
  } ffestdStmtId_;

/* Internal typedefs. */

typedef struct _ffestd_expr_item_ *ffestdExprItem_;
typedef struct _ffestd_stmt_ *ffestdStmt_;

/* Private include files. */


/* Internal structure definitions. */

struct _ffestd_expr_item_
  {
    ffestdExprItem_ next;
    ffebld expr;
    ffelexToken token;
  };

struct _ffestd_stmt_
  {
    ffestdStmt_ next;
    ffestdStmt_ previous;
    ffestdStmtId_ id;
    char *filename;
    int filelinenum;
    union
      {
	struct
	  {
	    ffestw block;
	  }
	enddoloop;
	struct
	  {
	    ffelab label;
	  }
	execlabel;
	struct
	  {
	    ffelab label;
	  }
	formatlabel;
	struct
	  {
	    mallocPool pool;
	    ffebld dest;
	    ffebld source;
	  }
	R737A;
	struct
	  {
	    mallocPool pool;
	    ffestw block;
	    ffebld expr;
	  }
	R803;
	struct
	  {
	    mallocPool pool;
	    ffestw block;
	    ffebld expr;
	  }
	R804;
	struct
	  {
	    ffestw block;
	  }
	R805;
	struct
	  {
	    ffestw block;
	  }
	R806;
	struct
	  {
	    mallocPool pool;
	    ffebld expr;
	  }
	R807;
	struct
	  {
	    mallocPool pool;
	    ffestw block;
	    ffebld expr;
	  }
	R809;
	struct
	  {
	    mallocPool pool;
	    ffestw block;
	    unsigned long casenum;
	  }
	R810;
	struct
	  {
	    ffestw block;
	  }
	R811;
	struct
	  {
	    mallocPool pool;
	    ffestw block;
	    ffelab label;
	    ffebld var;
	    ffebld start;
	    ffelexToken start_token;
	    ffebld end;
	    ffelexToken end_token;
	    ffebld incr;
	    ffelexToken incr_token;
	  }
	R819A;
	struct
	  {
	    mallocPool pool;
	    ffestw block;
	    ffelab label;
	    ffebld expr;
	  }
	R819B;
	struct
	  {
	    ffestw block;
	  }
	R834;
	struct
	  {
	    ffestw block;
	  }
	R835;
	struct
	  {
	    ffelab label;
	  }
	R836;
	struct
	  {
	    mallocPool pool;
	    ffelab *labels;
	    int count;
	    ffebld expr;
	  }
	R837;
	struct
	  {
	    mallocPool pool;
	    ffelab label;
	    ffebld target;
	  }
	R838;
	struct
	  {
	    mallocPool pool;
	    ffebld target;
	  }
	R839;
	struct
	  {
	    mallocPool pool;
	    ffebld expr;
	    ffelab neg;
	    ffelab zero;
	    ffelab pos;
	  }
	R840;
	struct
	  {
	    mallocPool pool;
	    ffebld expr;
	  }
	R842;
	struct
	  {
	    mallocPool pool;
	    ffebld expr;
	  }
	R843;
	struct
	  {
	    mallocPool pool;
	    ffestpOpenStmt *params;
	  }
	R904;
	struct
	  {
	    mallocPool pool;
	    ffestpCloseStmt *params;
	  }
	R907;
	struct
	  {
	    mallocPool pool;
	    ffestpReadStmt *params;
	    bool only_format;
	    ffestvUnit unit;
	    ffestvFormat format;
	    bool rec;
	    bool key;
	    ffestdExprItem_ list;
	  }
	R909;
	struct
	  {
	    mallocPool pool;
	    ffestpWriteStmt *params;
	    ffestvUnit unit;
	    ffestvFormat format;
	    bool rec;
	    ffestdExprItem_ list;
	  }
	R910;
	struct
	  {
	    mallocPool pool;
	    ffestpPrintStmt *params;
	    ffestvFormat format;
	    ffestdExprItem_ list;
	  }
	R911;
	struct
	  {
	    mallocPool pool;
	    ffestpBeruStmt *params;
	  }
	R919;
	struct
	  {
	    mallocPool pool;
	    ffestpBeruStmt *params;
	  }
	R920;
	struct
	  {
	    mallocPool pool;
	    ffestpBeruStmt *params;
	  }
	R921;
	struct
	  {
	    mallocPool pool;
	    ffestpInquireStmt *params;
	    bool by_file;
	  }
	R923A;
	struct
	  {
	    mallocPool pool;
	    ffestpInquireStmt *params;
	    ffestdExprItem_ list;
	  }
	R923B;
	struct
	  {
	    ffestsHolder str;
	  }
	R1001;
	struct
	  {
	    mallocPool pool;
	    ffebld expr;
	  }
	R1212;
	struct
	  {
	    ffesymbol entry;
	    int entrynum;
	  }
	R1226;
	struct
	  {
	    mallocPool pool;
	    ffestw block;
	    ffebld expr;
	  }
	R1227;
#if FFESTR_VXT
	struct
	  {
	    mallocPool pool;
	    ffestpRewriteStmt *params;
	    ffestvFormat format;
	    ffestdExprItem_ list;
	  }
	V018;
	struct
	  {
	    mallocPool pool;
	    ffestpAcceptStmt *params;
	    ffestvFormat format;
	    ffestdExprItem_ list;
	  }
	V019;
#endif
	struct
	  {
	    mallocPool pool;
	    ffestpTypeStmt *params;
	    ffestvFormat format;
	    ffestdExprItem_ list;
	  }
	V020;
#if FFESTR_VXT
	struct
	  {
	    mallocPool pool;
	    ffestpDeleteStmt *params;
	  }
	V021;
	struct
	  {
	    mallocPool pool;
	    ffestpBeruStmt *params;
	  }
	V022;
	struct
	  {
	    mallocPool pool;
	    ffestpVxtcodeStmt *params;
	    ffestdExprItem_ list;
	  }
	V023;
	struct
	  {
	    mallocPool pool;
	    ffestpVxtcodeStmt *params;
	    ffestdExprItem_ list;
	  }
	V024;
	struct
	  {
	    ffebld u;
	    ffebld m;
	    ffebld n;
	    ffebld asv;
	  }
	V025item;
	struct
	  {
	    mallocPool pool;
	  } V025finish;
	struct
	  {
	    mallocPool pool;
	    ffestpFindStmt *params;
	  }
	V026;
#endif
      }
    u;
  };

/* Static objects accessed by functions in this module. */

static ffestdStatelet_ ffestd_statelet_ = FFESTD_stateletSIMPLE_;
static int ffestd_block_level_ = 0;	/* Block level for reachableness. */
static bool ffestd_is_reachable_;	/* Is the current stmt reachable?  */
static ffelab ffestd_label_formatdef_ = NULL;
static ffestdExprItem_ *ffestd_expr_list_;
static struct
  {
    ffestdStmt_ first;
    ffestdStmt_ last;
  }
ffestd_stmt_list_ =
{
  NULL, NULL
};


/* # ENTRY statements pending. */
static int ffestd_2pass_entrypoints_ = 0;

/* Static functions (internal). */

static void ffestd_stmt_append_ (ffestdStmt_ stmt);
static ffestdStmt_ ffestd_stmt_new_ (ffestdStmtId_ id);
static void ffestd_stmt_pass_ (void);
#if FFESTD_COPY_EASY_
static ffestpInquireStmt *ffestd_subr_copy_easy_ (ffestpInquireIx max);
#endif
static void ffestd_subr_vxt_ (void);
#if FFESTR_F90
static void ffestd_subr_f90_ (void);
#endif
static void ffestd_subr_labels_ (bool unexpected);
static void ffestd_R1001dump_ (ffests s, ffesttFormatList list);
static void ffestd_R1001dump_1005_1_ (ffests s, ffesttFormatList f,
				      const char *string);
static void ffestd_R1001dump_1005_2_ (ffests s, ffesttFormatList f,
				      const char *string);
static void ffestd_R1001dump_1005_3_ (ffests s, ffesttFormatList f,
				      const char *string);
static void ffestd_R1001dump_1005_4_ (ffests s, ffesttFormatList f,
				      const char *string);
static void ffestd_R1001dump_1005_5_ (ffests s, ffesttFormatList f,
				      const char *string);
static void ffestd_R1001dump_1010_1_ (ffests s, ffesttFormatList f,
				      const char *string);
static void ffestd_R1001dump_1010_2_ (ffests s, ffesttFormatList f,
				      const char *string);
static void ffestd_R1001dump_1010_3_ (ffests s, ffesttFormatList f,
				      const char *string);
static void ffestd_R1001dump_1010_4_ (ffests s, ffesttFormatList f,
				      const char *string);
static void ffestd_R1001dump_1010_5_ (ffests s, ffesttFormatList f,
				      const char *string);
static void ffestd_R1001error_ (ffesttFormatList f);
static void ffestd_R1001rtexpr_ (ffests s, ffesttFormatList f, ffebld expr);

/* Internal macros. */

#define ffestd_subr_line_now_()					       \
  ffeste_set_line (ffelex_token_where_filename (ffesta_tokens[0]), \
		   ffelex_token_where_filelinenum (ffesta_tokens[0]))
#define ffestd_subr_line_restore_(s) \
  ffeste_set_line ((s)->filename, (s)->filelinenum)
#define ffestd_subr_line_save_(s)					   \
  ((s)->filename = ffelex_token_where_filename (ffesta_tokens[0]),	   \
   (s)->filelinenum = ffelex_token_where_filelinenum (ffesta_tokens[0]))
#define ffestd_check_simple_() \
      assert(ffestd_statelet_ == FFESTD_stateletSIMPLE_)
#define ffestd_check_start_() \
      assert(ffestd_statelet_ == FFESTD_stateletSIMPLE_); \
      ffestd_statelet_ = FFESTD_stateletATTRIB_
#define ffestd_check_attrib_() \
      assert(ffestd_statelet_ == FFESTD_stateletATTRIB_)
#define ffestd_check_item_() \
      assert(ffestd_statelet_ == FFESTD_stateletATTRIB_	 \
	    || ffestd_statelet_ == FFESTD_stateletITEM_); \
      ffestd_statelet_ = FFESTD_stateletITEM_
#define ffestd_check_item_startvals_() \
      assert(ffestd_statelet_ == FFESTD_stateletATTRIB_	 \
	    || ffestd_statelet_ == FFESTD_stateletITEM_); \
      ffestd_statelet_ = FFESTD_stateletITEMVALS_
#define ffestd_check_item_value_() \
      assert(ffestd_statelet_ == FFESTD_stateletITEMVALS_)
#define ffestd_check_item_endvals_() \
      assert(ffestd_statelet_ == FFESTD_stateletITEMVALS_); \
      ffestd_statelet_ = FFESTD_stateletITEM_
#define ffestd_check_finish_() \
      assert(ffestd_statelet_ == FFESTD_stateletATTRIB_	 \
	    || ffestd_statelet_ == FFESTD_stateletITEM_); \
      ffestd_statelet_ = FFESTD_stateletSIMPLE_

#if FFESTD_COPY_EASY_
#define ffestd_subr_copy_accept_() (ffestpAcceptStmt *) \
      ffestd_subr_copy_easy_((ffestpInquireIx) FFESTP_acceptix)
#define ffestd_subr_copy_beru_() (ffestpBeruStmt *) \
      ffestd_subr_copy_easy_((ffestpInquireIx) FFESTP_beruix)
#define ffestd_subr_copy_close_() (ffestpCloseStmt *) \
      ffestd_subr_copy_easy_((ffestpInquireIx) FFESTP_closeix)
#define ffestd_subr_copy_delete_() (ffestpDeleteStmt *) \
      ffestd_subr_copy_easy_((ffestpInquireIx) FFESTP_deleteix)
#define ffestd_subr_copy_find_() (ffestpFindStmt *) \
      ffestd_subr_copy_easy_((ffestpInquireIx) FFESTP_findix)
#define ffestd_subr_copy_inquire_() (ffestpInquireStmt *) \
      ffestd_subr_copy_easy_((ffestpInquireIx) FFESTP_inquireix)
#define ffestd_subr_copy_open_() (ffestpOpenStmt *) \
      ffestd_subr_copy_easy_((ffestpInquireIx) FFESTP_openix)
#define ffestd_subr_copy_print_() (ffestpPrintStmt *) \
      ffestd_subr_copy_easy_((ffestpInquireIx) FFESTP_printix)
#define ffestd_subr_copy_read_() (ffestpReadStmt *) \
      ffestd_subr_copy_easy_((ffestpInquireIx) FFESTP_readix)
#define ffestd_subr_copy_rewrite_() (ffestpRewriteStmt *) \
      ffestd_subr_copy_easy_((ffestpInquireIx) FFESTP_rewriteix)
#define ffestd_subr_copy_type_() (ffestpTypeStmt *) \
      ffestd_subr_copy_easy_((ffestpInquireIx) FFESTP_typeix)
#define ffestd_subr_copy_vxtcode_() (ffestpVxtcodeStmt *) \
      ffestd_subr_copy_easy_((ffestpInquireIx) FFESTP_vxtcodeix)
#define ffestd_subr_copy_write_() (ffestpWriteStmt *) \
      ffestd_subr_copy_easy_((ffestpInquireIx) FFESTP_writeix)
#endif

/* ffestd_stmt_append_ -- Append statement to end of stmt list

   ffestd_stmt_append_(ffestd_stmt_new_(FFESTD_stmtidR737A_));	*/

static void
ffestd_stmt_append_ (ffestdStmt_ stmt)
{
  stmt->next = (ffestdStmt_) &ffestd_stmt_list_.first;
  stmt->previous = ffestd_stmt_list_.last;
  stmt->next->previous = stmt;
  stmt->previous->next = stmt;
}

/* ffestd_stmt_new_ -- Make new statement with given id

   ffestdStmt_ stmt;
   stmt = ffestd_stmt_new_(FFESTD_stmtidR737A_);  */

static ffestdStmt_
ffestd_stmt_new_ (ffestdStmtId_ id)
{
  ffestdStmt_ stmt;

  stmt = malloc_new_kp (ffe_pool_any_unit (), "ffestdStmt_", sizeof (*stmt));
  stmt->id = id;
  return stmt;
}

/* ffestd_stmt_pass_ -- Pass all statements on list to ffeste

   ffestd_stmt_pass_();	 */

static void
ffestd_stmt_pass_ ()
{
  ffestdStmt_ stmt;
  ffestdExprItem_ expr;		/* For traversing lists. */
  bool okay = (TREE_CODE (current_function_decl) != ERROR_MARK);

  if ((ffestd_2pass_entrypoints_ != 0) && okay)
    {
      tree which = ffecom_which_entrypoint_decl ();
      tree value;
      tree label;
      int pushok;
      int ents = ffestd_2pass_entrypoints_;
      tree duplicate;

      expand_start_case (0, which, TREE_TYPE (which), "entrypoint dispatch");

      stmt = ffestd_stmt_list_.first;
      do
	{
	  while (stmt->id != FFESTD_stmtidR1226_)
	    stmt = stmt->next;

	  if (stmt->u.R1226.entry != NULL)
	    {
	      value = build_int_2 (stmt->u.R1226.entrynum, 0);
	      /* Yes, we really want to build a null LABEL_DECL here and not
		 put it on any list.  That's what pushcase wants, so that's
		 what it gets!  */
	      label = build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);

	      pushok = pushcase (value, convert, label, &duplicate);
	      assert (pushok == 0);

	      label = ffecom_temp_label ();
	      TREE_USED (label) = 1;
	      expand_goto (label);

	      ffesymbol_hook (stmt->u.R1226.entry).length_tree = label;
	    }
	  stmt = stmt->next;
	}
      while (--ents != 0);

      expand_end_case (which);
    }

  for (stmt = ffestd_stmt_list_.first;
       stmt != (ffestdStmt_) &ffestd_stmt_list_.first;
       stmt = stmt->next)
    {
      switch (stmt->id)
	{
	case FFESTD_stmtidENDDOLOOP_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_do (stmt->u.enddoloop.block);
	  ffestw_kill (stmt->u.enddoloop.block);
	  break;

	case FFESTD_stmtidENDLOGIF_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_end_R807 ();
	  break;

	case FFESTD_stmtidEXECLABEL_:
	  if (okay)
	    ffeste_labeldef_branch (stmt->u.execlabel.label);
	  break;

	case FFESTD_stmtidFORMATLABEL_:
	  if (okay)
	    ffeste_labeldef_format (stmt->u.formatlabel.label);
	  break;

	case FFESTD_stmtidR737A_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_R737A (stmt->u.R737A.dest, stmt->u.R737A.source);
	  malloc_pool_kill (stmt->u.R737A.pool);
	  break;

	case FFESTD_stmtidR803_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_R803 (stmt->u.R803.block, stmt->u.R803.expr);
	  malloc_pool_kill (stmt->u.R803.pool);
	  break;

	case FFESTD_stmtidR804_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_R804 (stmt->u.R803.block, stmt->u.R804.expr);
	  malloc_pool_kill (stmt->u.R804.pool);
	  break;

	case FFESTD_stmtidR805_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_R805 (stmt->u.R803.block);
	  break;

	case FFESTD_stmtidR806_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_R806 (stmt->u.R806.block);
	  ffestw_kill (stmt->u.R806.block);
	  break;

	case FFESTD_stmtidR807_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_R807 (stmt->u.R807.expr);
	  malloc_pool_kill (stmt->u.R807.pool);
	  break;

	case FFESTD_stmtidR809_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_R809 (stmt->u.R809.block, stmt->u.R809.expr);
	  malloc_pool_kill (stmt->u.R809.pool);
	  break;

	case FFESTD_stmtidR810_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_R810 (stmt->u.R810.block, stmt->u.R810.casenum);
	  malloc_pool_kill (stmt->u.R810.pool);
	  break;

	case FFESTD_stmtidR811_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_R811 (stmt->u.R811.block);
	  malloc_pool_kill (ffestw_select (stmt->u.R811.block)->pool);
	  ffestw_kill (stmt->u.R811.block);
	  break;

	case FFESTD_stmtidR819A_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_R819A (stmt->u.R819A.block, stmt->u.R819A.label,
			  stmt->u.R819A.var,
			  stmt->u.R819A.start, stmt->u.R819A.start_token,
			  stmt->u.R819A.end, stmt->u.R819A.end_token,
			  stmt->u.R819A.incr, stmt->u.R819A.incr_token);
	  ffelex_token_kill (stmt->u.R819A.start_token);
	  ffelex_token_kill (stmt->u.R819A.end_token);
	  if (stmt->u.R819A.incr_token != NULL)
	    ffelex_token_kill (stmt->u.R819A.incr_token);
	  malloc_pool_kill (stmt->u.R819A.pool);
	  break;

	case FFESTD_stmtidR819B_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_R819B (stmt->u.R819B.block, stmt->u.R819B.label,
			  stmt->u.R819B.expr);
	  malloc_pool_kill (stmt->u.R819B.pool);
	  break;

	case FFESTD_stmtidR825_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_R825 ();
	  break;

	case FFESTD_stmtidR834_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_R834 (stmt->u.R834.block);
	  break;

	case FFESTD_stmtidR835_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_R835 (stmt->u.R835.block);
	  break;

	case FFESTD_stmtidR836_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_R836 (stmt->u.R836.label);
	  break;

	case FFESTD_stmtidR837_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_R837 (stmt->u.R837.labels, stmt->u.R837.count,
			 stmt->u.R837.expr);
	  malloc_pool_kill (stmt->u.R837.pool);
	  break;

	case FFESTD_stmtidR838_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_R838 (stmt->u.R838.label, stmt->u.R838.target);
	  malloc_pool_kill (stmt->u.R838.pool);
	  break;

	case FFESTD_stmtidR839_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_R839 (stmt->u.R839.target);
	  malloc_pool_kill (stmt->u.R839.pool);
	  break;

	case FFESTD_stmtidR840_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_R840 (stmt->u.R840.expr, stmt->u.R840.neg, stmt->u.R840.zero,
			 stmt->u.R840.pos);
	  malloc_pool_kill (stmt->u.R840.pool);
	  break;

	case FFESTD_stmtidR841_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_R841 ();
	  break;

	case FFESTD_stmtidR842_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_R842 (stmt->u.R842.expr);
	  if (stmt->u.R842.pool != NULL)
	    malloc_pool_kill (stmt->u.R842.pool);
	  break;

	case FFESTD_stmtidR843_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_R843 (stmt->u.R843.expr);
	  malloc_pool_kill (stmt->u.R843.pool);
	  break;

	case FFESTD_stmtidR904_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_R904 (stmt->u.R904.params);
	  malloc_pool_kill (stmt->u.R904.pool);
	  break;

	case FFESTD_stmtidR907_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_R907 (stmt->u.R907.params);
	  malloc_pool_kill (stmt->u.R907.pool);
	  break;

	case FFESTD_stmtidR909_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_R909_start (stmt->u.R909.params, stmt->u.R909.only_format,
			       stmt->u.R909.unit, stmt->u.R909.format,
			       stmt->u.R909.rec, stmt->u.R909.key);
	  for (expr = stmt->u.R909.list; expr != NULL; expr = expr->next)
	    {
	      if (okay)
		ffeste_R909_item (expr->expr, expr->token);
	      ffelex_token_kill (expr->token);
	    }
	  if (okay)
	    ffeste_R909_finish ();
	  malloc_pool_kill (stmt->u.R909.pool);
	  break;

	case FFESTD_stmtidR910_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_R910_start (stmt->u.R910.params, stmt->u.R910.unit,
			       stmt->u.R910.format, stmt->u.R910.rec);
	  for (expr = stmt->u.R910.list; expr != NULL; expr = expr->next)
	    {
	      if (okay)
		ffeste_R910_item (expr->expr, expr->token);
	      ffelex_token_kill (expr->token);
	    }
	  if (okay)
	    ffeste_R910_finish ();
	  malloc_pool_kill (stmt->u.R910.pool);
	  break;

	case FFESTD_stmtidR911_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_R911_start (stmt->u.R911.params, stmt->u.R911.format);
	  for (expr = stmt->u.R911.list; expr != NULL; expr = expr->next)
	    {
	      if (okay)
		ffeste_R911_item (expr->expr, expr->token);
	      ffelex_token_kill (expr->token);
	    }
	  if (okay)
	    ffeste_R911_finish ();
	  malloc_pool_kill (stmt->u.R911.pool);
	  break;

	case FFESTD_stmtidR919_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_R919 (stmt->u.R919.params);
	  malloc_pool_kill (stmt->u.R919.pool);
	  break;

	case FFESTD_stmtidR920_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_R920 (stmt->u.R920.params);
	  malloc_pool_kill (stmt->u.R920.pool);
	  break;

	case FFESTD_stmtidR921_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_R921 (stmt->u.R921.params);
	  malloc_pool_kill (stmt->u.R921.pool);
	  break;

	case FFESTD_stmtidR923A_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_R923A (stmt->u.R923A.params, stmt->u.R923A.by_file);
	  malloc_pool_kill (stmt->u.R923A.pool);
	  break;

	case FFESTD_stmtidR923B_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_R923B_start (stmt->u.R923B.params);
	  for (expr = stmt->u.R923B.list; expr != NULL; expr = expr->next)
	    {
	      if (okay)
		ffeste_R923B_item (expr->expr);
	    }
	  if (okay)
	    ffeste_R923B_finish ();
	  malloc_pool_kill (stmt->u.R923B.pool);
	  break;

	case FFESTD_stmtidR1001_:
	  if (okay)
	    ffeste_R1001 (&stmt->u.R1001.str);
	  ffests_kill (&stmt->u.R1001.str);
	  break;

	case FFESTD_stmtidR1103_:
	  if (okay)
	    ffeste_R1103 ();
	  break;

	case FFESTD_stmtidR1112_:
	  if (okay)
	    ffeste_R1112 ();
	  break;

	case FFESTD_stmtidR1212_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_R1212 (stmt->u.R1212.expr);
	  malloc_pool_kill (stmt->u.R1212.pool);
	  break;

	case FFESTD_stmtidR1221_:
	  if (okay)
	    ffeste_R1221 ();
	  break;

	case FFESTD_stmtidR1225_:
	  if (okay)
	    ffeste_R1225 ();
	  break;

	case FFESTD_stmtidR1226_:
	  ffestd_subr_line_restore_ (stmt);
	  if (stmt->u.R1226.entry != NULL)
	    {
	      if (okay)
		ffeste_R1226 (stmt->u.R1226.entry);
	    }
	  break;

	case FFESTD_stmtidR1227_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_R1227 (stmt->u.R1227.block, stmt->u.R1227.expr);
	  malloc_pool_kill (stmt->u.R1227.pool);
	  break;

#if FFESTR_VXT
	case FFESTD_stmtidV018_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_V018_start (stmt->u.V018.params, stmt->u.V018.format);
	  for (expr = stmt->u.V018.list; expr != NULL; expr = expr->next)
	    {
	      if (okay)
		ffeste_V018_item (expr->expr);
	    }
	  if (okay)
	    ffeste_V018_finish ();
	  malloc_pool_kill (stmt->u.V018.pool);
	  break;

	case FFESTD_stmtidV019_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_V019_start (stmt->u.V019.params, stmt->u.V019.format);
	  for (expr = stmt->u.V019.list; expr != NULL; expr = expr->next)
	    {
	      if (okay)
		ffeste_V019_item (expr->expr);
	    }
	  if (okay)
	    ffeste_V019_finish ();
	  malloc_pool_kill (stmt->u.V019.pool);
	  break;
#endif

	case FFESTD_stmtidV020_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_V020_start (stmt->u.V020.params, stmt->u.V020.format);
	  for (expr = stmt->u.V020.list; expr != NULL; expr = expr->next)
	    {
	      if (okay)
		ffeste_V020_item (expr->expr);
	    }
	  if (okay)
	    ffeste_V020_finish ();
	  malloc_pool_kill (stmt->u.V020.pool);
	  break;

#if FFESTR_VXT
	case FFESTD_stmtidV021_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_V021 (stmt->u.V021.params);
	  malloc_pool_kill (stmt->u.V021.pool);
	  break;

	case FFESTD_stmtidV023_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_V023_start (stmt->u.V023.params);
	  for (expr = stmt->u.V023.list; expr != NULL; expr = expr->next)
	    {
	      if (okay)
		ffeste_V023_item (expr->expr);
	    }
	  if (okay)
	    ffeste_V023_finish ();
	  malloc_pool_kill (stmt->u.V023.pool);
	  break;

	case FFESTD_stmtidV024_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_V024_start (stmt->u.V024.params);
	  for (expr = stmt->u.V024.list; expr != NULL; expr = expr->next)
	    {
	      if (okay)
		ffeste_V024_item (expr->expr);
	    }
	  if (okay)
	    ffeste_V024_finish ();
	  malloc_pool_kill (stmt->u.V024.pool);
	  break;

	case FFESTD_stmtidV025start_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_V025_start ();
	  break;

	case FFESTD_stmtidV025item_:
	  if (okay)
	    ffeste_V025_item (stmt->u.V025item.u, stmt->u.V025item.m,
			      stmt->u.V025item.n, stmt->u.V025item.asv);
	  break;

	case FFESTD_stmtidV025finish_:
	  if (okay)
	    ffeste_V025_finish ();
	  malloc_pool_kill (stmt->u.V025finish.pool);
	  break;

	case FFESTD_stmtidV026_:
	  ffestd_subr_line_restore_ (stmt);
	  if (okay)
	    ffeste_V026 (stmt->u.V026.params);
	  malloc_pool_kill (stmt->u.V026.pool);
	  break;
#endif

	default:
	  assert ("bad stmt->id" == NULL);
	  break;
	}
    }
}

/* ffestd_subr_copy_easy_ -- Copy I/O statement data structure

   ffestd_subr_copy_easy_();

   Copies all data except tokens in the I/O data structure into a new
   structure that lasts as long as the output pool for the current
   statement.  Assumes that they are
   overlaid with each other (union) in stp.h and the typing
   and structure references assume (though not necessarily dangerous if
   FALSE) that INQUIRE has the most file elements.  */

#if FFESTD_COPY_EASY_
static ffestpInquireStmt *
ffestd_subr_copy_easy_ (ffestpInquireIx max)
{
  ffestpInquireStmt *stmt;
  ffestpInquireIx ix;

  stmt = (ffestpInquireStmt *) malloc_new_kp (ffesta_output_pool,
				  "FFESTD easy", sizeof (ffestpFile) * max);

  for (ix = 0; ix < max; ++ix)
    {
      if ((stmt->inquire_spec[ix].kw_or_val_present
	   = ffestp_file.inquire.inquire_spec[ix].kw_or_val_present)
	  && (stmt->inquire_spec[ix].value_present
	      = ffestp_file.inquire.inquire_spec[ix].value_present))
	{
	  if ((stmt->inquire_spec[ix].value_is_label
	       = ffestp_file.inquire.inquire_spec[ix].value_is_label))
	    stmt->inquire_spec[ix].u.label
	      = ffestp_file.inquire.inquire_spec[ix].u.label;
	  else
	    stmt->inquire_spec[ix].u.expr
	      = ffestp_file.inquire.inquire_spec[ix].u.expr;
	}
    }

  return stmt;
}

#endif
/* ffestd_subr_labels_ -- Handle any undefined labels

   ffestd_subr_labels_(FALSE);

   For every undefined label, generate an error message and either define
   label as a FORMAT() statement (for FORMAT labels) or as a STOP statement
   (for all other labels).  */

static void
ffestd_subr_labels_ (bool unexpected)
{
  ffelab l;
  ffelabHandle h;
  ffelabNumber undef;
  ffesttFormatList f;

  undef = ffelab_number () - ffestv_num_label_defines_;

  for (h = ffelab_handle_first (); h != NULL; h = ffelab_handle_next (h))
    {
      l = ffelab_handle_target (h);
      if (ffewhere_line_is_unknown (ffelab_definition_line (l)))
	{			/* Undefined label. */
	  assert (!unexpected);
	  assert (undef > 0);
	  undef--;
	  ffebad_start (FFEBAD_UNDEF_LABEL);
	  if (ffelab_type (l) == FFELAB_typeLOOPEND)
	    ffebad_here (0, ffelab_doref_line (l), ffelab_doref_column (l));
	  else if (ffelab_type (l) != FFELAB_typeANY)
	    ffebad_here (0, ffelab_firstref_line (l), ffelab_firstref_column (l));
	  else if (!ffewhere_line_is_unknown (ffelab_firstref_line (l)))
	    ffebad_here (0, ffelab_firstref_line (l), ffelab_firstref_column (l));
	  else if (!ffewhere_line_is_unknown (ffelab_doref_line (l)))
	    ffebad_here (0, ffelab_doref_line (l), ffelab_doref_column (l));
	  else
	    ffebad_here (0, ffelab_definition_line (l), ffelab_definition_column (l));
	  ffebad_finish ();

	  switch (ffelab_type (l))
	    {
	    case FFELAB_typeFORMAT:
	      ffelab_set_definition_line (l,
			      ffewhere_line_use (ffelab_firstref_line (l)));
	      ffelab_set_definition_column (l,
			  ffewhere_column_use (ffelab_firstref_column (l)));
	      ffestv_num_label_defines_++;
	      f = ffestt_formatlist_create (NULL, NULL);
	      ffestd_labeldef_format (l);
	      ffestd_R1001 (f);
	      ffestt_formatlist_kill (f);
	      break;

	    case FFELAB_typeASSIGNABLE:
	      ffelab_set_definition_line (l,
			      ffewhere_line_use (ffelab_firstref_line (l)));
	      ffelab_set_definition_column (l,
			  ffewhere_column_use (ffelab_firstref_column (l)));
	      ffestv_num_label_defines_++;
	      ffelab_set_type (l, FFELAB_typeNOTLOOP);
	      ffelab_set_blocknum (l, ffestw_blocknum (ffestw_stack_top ()));
	      ffestd_labeldef_notloop (l);
	      ffestd_R842 (NULL);
	      break;

	    case FFELAB_typeNOTLOOP:
	      ffelab_set_definition_line (l,
			      ffewhere_line_use (ffelab_firstref_line (l)));
	      ffelab_set_definition_column (l,
			  ffewhere_column_use (ffelab_firstref_column (l)));
	      ffestv_num_label_defines_++;
	      ffelab_set_blocknum (l, ffestw_blocknum (ffestw_stack_top ()));
	      ffestd_labeldef_notloop (l);
	      ffestd_R842 (NULL);
	      break;

	    default:
	      assert ("bad label type" == NULL);
	      /* Fall through. */
	    case FFELAB_typeUNKNOWN:
	    case FFELAB_typeANY:
	      break;
	    }
	}
    }
  ffelab_handle_done (h);
  assert (undef == 0);
}

/* ffestd_subr_f90_ -- Report error about lack of full F90 support

   ffestd_subr_f90_();	*/

#if FFESTR_F90
static void
ffestd_subr_f90_ ()
{
  ffebad_start (FFEBAD_F90);
  ffebad_here (0, ffelex_token_where_line (ffesta_tokens[0]),
	       ffelex_token_where_column (ffesta_tokens[0]));
  ffebad_finish ();
}

#endif
/* ffestd_subr_vxt_ -- Report error about lack of full VXT support

   ffestd_subr_vxt_();	*/

static void
ffestd_subr_vxt_ ()
{
  ffebad_start (FFEBAD_VXT_UNSUPPORTED);
  ffebad_here (0, ffelex_token_where_line (ffesta_tokens[0]),
	       ffelex_token_where_column (ffesta_tokens[0]));
  ffebad_finish ();
}

/* ffestd_begin_uses -- Start a bunch of USE statements

   ffestd_begin_uses();

   Invoked before handling the first USE statement in a block of one or
   more USE statements.	 _end_uses_(bool ok) is invoked before handling
   the first statement after the block (there are no BEGIN USE and END USE
   statements, but the semantics of USE statements effectively requires
   handling them as a single block rather than one statement at a time).  */

void
ffestd_begin_uses ()
{
}

/* ffestd_do -- End of statement following DO-term-stmt etc

   ffestd_do(TRUE);

   Also invoked by _labeldef_branch_finish_ (or, in cases
   of errors, other _labeldef_ functions) when the label definition is
   for a DO-target (LOOPEND) label, once per matching/outstanding DO
   block on the stack.	These cases invoke this function with ok==TRUE, so
   only forced stack popping (via ffestd_eof_()) invokes it with ok==FALSE.  */

void
ffestd_do (bool ok UNUSED)
{
  ffestdStmt_ stmt;

  stmt = ffestd_stmt_new_ (FFESTD_stmtidENDDOLOOP_);
  ffestd_stmt_append_ (stmt);
  ffestd_subr_line_save_ (stmt);
  stmt->u.enddoloop.block = ffestw_stack_top ();

  --ffestd_block_level_;
  assert (ffestd_block_level_ >= 0);
}

/* ffestd_end_uses -- End a bunch of USE statements

   ffestd_end_uses(TRUE);

   ok==TRUE means simply not popping due to ffestd_eof_()
   being called, because there is no formal END USES statement in Fortran.  */

#if FFESTR_F90
void
ffestd_end_uses (bool ok)
{
}

/* ffestd_end_R740 -- End a WHERE(-THEN)

   ffestd_end_R740(TRUE);  */

void
ffestd_end_R740 (bool ok)
{
  return;			/* F90. */
}

#endif
/* ffestd_end_R807 -- End of statement following logical IF

   ffestd_end_R807(TRUE);

   Applies ONLY to logical IF, not to IF-THEN.	For example, does not
   ffelex_token_kill the construct name for an IF-THEN block (the name
   field is invalid for logical IF).  ok==TRUE iff statement following
   logical IF (substatement) is valid; else, statement is invalid or
   stack forcibly popped due to ffestd_eof_().	*/

void
ffestd_end_R807 (bool ok UNUSED)
{
  ffestdStmt_ stmt;

  stmt = ffestd_stmt_new_ (FFESTD_stmtidENDLOGIF_);
  ffestd_stmt_append_ (stmt);
  ffestd_subr_line_save_ (stmt);

  --ffestd_block_level_;
  assert (ffestd_block_level_ >= 0);
}

/* ffestd_exec_begin -- Executable statements can start coming in now

   ffestd_exec_begin();	 */

void
ffestd_exec_begin ()
{
  ffecom_exec_transition ();

  if (ffestd_2pass_entrypoints_ != 0)
    {				/* Process pending ENTRY statements now that
				   info filled in. */
      ffestdStmt_ stmt;
      int ents = ffestd_2pass_entrypoints_;

      stmt = ffestd_stmt_list_.first;
      do
	{
	  while (stmt->id != FFESTD_stmtidR1226_)
	    stmt = stmt->next;

	  if (!ffecom_2pass_advise_entrypoint (stmt->u.R1226.entry))
	    {
	      stmt->u.R1226.entry = NULL;
	      --ffestd_2pass_entrypoints_;
	    }
	  stmt = stmt->next;
	}
      while (--ents != 0);
    }
}

/* ffestd_exec_end -- Executable statements can no longer come in now

   ffestd_exec_end();  */

void
ffestd_exec_end ()
{
  int old_lineno = lineno;
  const char *old_input_filename = input_filename;

  ffecom_end_transition ();

  ffestd_stmt_pass_ ();

  ffecom_finish_progunit ();

  if (ffestd_2pass_entrypoints_ != 0)
    {
      int ents = ffestd_2pass_entrypoints_;
      ffestdStmt_ stmt = ffestd_stmt_list_.first;

      do
	{
	  while (stmt->id != FFESTD_stmtidR1226_)
	    stmt = stmt->next;

	  if (stmt->u.R1226.entry != NULL)
	    {
	      ffestd_subr_line_restore_ (stmt);
	      ffecom_2pass_do_entrypoint (stmt->u.R1226.entry);
	    }
	  stmt = stmt->next;
	}
      while (--ents != 0);
    }

  ffestd_stmt_list_.first = NULL;
  ffestd_stmt_list_.last = NULL;
  ffestd_2pass_entrypoints_ = 0;

  lineno = old_lineno;
  input_filename = old_input_filename;
}

/* ffestd_init_3 -- Initialize for any program unit

   ffestd_init_3();  */

void
ffestd_init_3 ()
{
  ffestd_stmt_list_.first = (ffestdStmt_) &ffestd_stmt_list_.first;
  ffestd_stmt_list_.last = (ffestdStmt_) &ffestd_stmt_list_.first;
}

/* Generate "code" for "any" label def.  */

void
ffestd_labeldef_any (ffelab label UNUSED)
{
}

/* ffestd_labeldef_branch -- Generate "code" for branch label def

   ffestd_labeldef_branch(label);  */

void
ffestd_labeldef_branch (ffelab label)
{
  ffestdStmt_ stmt;

  stmt = ffestd_stmt_new_ (FFESTD_stmtidEXECLABEL_);
  ffestd_stmt_append_ (stmt);
  stmt->u.execlabel.label = label;

  ffestd_is_reachable_ = TRUE;
}

/* ffestd_labeldef_format -- Generate "code" for FORMAT label def

   ffestd_labeldef_format(label);  */

void
ffestd_labeldef_format (ffelab label)
{
  ffestdStmt_ stmt;

  ffestd_label_formatdef_ = label;

  stmt = ffestd_stmt_new_ (FFESTD_stmtidFORMATLABEL_);
  ffestd_stmt_append_ (stmt);
  stmt->u.formatlabel.label = label;
}

/* ffestd_labeldef_useless -- Generate "code" for useless label def

   ffestd_labeldef_useless(label);  */

void
ffestd_labeldef_useless (ffelab label UNUSED)
{
}

/* ffestd_R423A -- PRIVATE statement (in R422 derived-type statement)

   ffestd_R423A();  */

#if FFESTR_F90
void
ffestd_R423A ()
{
  ffestd_check_simple_ ();
}

/* ffestd_R423B -- SEQUENCE statement (in R422 derived-type-stmt)

   ffestd_R423B();  */

void
ffestd_R423B ()
{
  ffestd_check_simple_ ();
}

/* ffestd_R424 -- derived-TYPE-def statement

   ffestd_R424(access_token,access_kw,name_token);

   Handle a derived-type definition.  */

void
ffestd_R424 (ffelexToken access, ffestrOther access_kw, ffelexToken name)
{
  ffestd_check_simple_ ();

  ffestd_subr_f90_ ();
  return;

#ifdef FFESTD_F90
  char *a;

  if (access == NULL)
    fprintf (dmpout, "* TYPE %s\n", ffelex_token_text (name));
  else
    {
      switch (access_kw)
	{
	case FFESTR_otherPUBLIC:
	  a = "PUBLIC";
	  break;

	case FFESTR_otherPRIVATE:
	  a = "PRIVATE";
	  break;

	default:
	  assert (FALSE);
	}
      fprintf (dmpout, "* TYPE,%s: %s\n", a, ffelex_token_text (name));
    }
#endif
}

/* ffestd_R425 -- End a TYPE

   ffestd_R425(TRUE);  */

void
ffestd_R425 (bool ok)
{
}

/* ffestd_R519_start -- INTENT statement list begin

   ffestd_R519_start();

   Verify that INTENT is valid here, and begin accepting items in the list.  */

void
ffestd_R519_start (ffestrOther intent_kw)
{
  ffestd_check_start_ ();

  ffestd_subr_f90_ ();
  return;

#ifdef FFESTD_F90
  char *a;

  switch (intent_kw)
    {
    case FFESTR_otherIN:
      a = "IN";
      break;

    case FFESTR_otherOUT:
      a = "OUT";
      break;

    case FFESTR_otherINOUT:
      a = "INOUT";
      break;

    default:
      assert (FALSE);
    }
  fprintf (dmpout, "* INTENT (%s) ", a);
#endif
}

/* ffestd_R519_item -- INTENT statement for name

   ffestd_R519_item(name_token);

   Make sure name_token identifies a valid object to be INTENTed.  */

void
ffestd_R519_item (ffelexToken name)
{
  ffestd_check_item_ ();

  return;			/* F90. */

#ifdef FFESTD_F90
  fprintf (dmpout, "%s,", ffelex_token_text (name));
#endif
}

/* ffestd_R519_finish -- INTENT statement list complete

   ffestd_R519_finish();

   Just wrap up any local activities.  */

void
ffestd_R519_finish ()
{
  ffestd_check_finish_ ();

  return;			/* F90. */

#ifdef FFESTD_F90
  fputc ('\n', dmpout);
#endif
}

/* ffestd_R520_start -- OPTIONAL statement list begin

   ffestd_R520_start();

   Verify that OPTIONAL is valid here, and begin accepting items in the list.  */

void
ffestd_R520_start ()
{
  ffestd_check_start_ ();

  ffestd_subr_f90_ ();
  return;

#ifdef FFESTD_F90
  fputs ("* OPTIONAL ", dmpout);
#endif
}

/* ffestd_R520_item -- OPTIONAL statement for name

   ffestd_R520_item(name_token);

   Make sure name_token identifies a valid object to be OPTIONALed.  */

void
ffestd_R520_item (ffelexToken name)
{
  ffestd_check_item_ ();

  return;			/* F90. */

#ifdef FFESTD_F90
  fprintf (dmpout, "%s,", ffelex_token_text (name));
#endif
}

/* ffestd_R520_finish -- OPTIONAL statement list complete

   ffestd_R520_finish();

   Just wrap up any local activities.  */

void
ffestd_R520_finish ()
{
  ffestd_check_finish_ ();

  return;			/* F90. */

#ifdef FFESTD_F90
  fputc ('\n', dmpout);
#endif
}

/* ffestd_R521A -- PUBLIC statement

   ffestd_R521A();

   Verify that PUBLIC is valid here.  */

void
ffestd_R521A ()
{
  ffestd_check_simple_ ();

  ffestd_subr_f90_ ();
  return;

#ifdef FFESTD_F90
  fputs ("* PUBLIC\n", dmpout);
#endif
}

/* ffestd_R521Astart -- PUBLIC statement list begin

   ffestd_R521Astart();

   Verify that PUBLIC is valid here, and begin accepting items in the list.  */

void
ffestd_R521Astart ()
{
  ffestd_check_start_ ();

  ffestd_subr_f90_ ();
  return;

#ifdef FFESTD_F90
  fputs ("* PUBLIC ", dmpout);
#endif
}

/* ffestd_R521Aitem -- PUBLIC statement for name

   ffestd_R521Aitem(name_token);

   Make sure name_token identifies a valid object to be PUBLICed.  */

void
ffestd_R521Aitem (ffelexToken name)
{
  ffestd_check_item_ ();

  return;			/* F90. */

#ifdef FFESTD_F90
  fprintf (dmpout, "%s,", ffelex_token_text (name));
#endif
}

/* ffestd_R521Afinish -- PUBLIC statement list complete

   ffestd_R521Afinish();

   Just wrap up any local activities.  */

void
ffestd_R521Afinish ()
{
  ffestd_check_finish_ ();

  return;			/* F90. */

#ifdef FFESTD_F90
  fputc ('\n', dmpout);
#endif
}

/* ffestd_R521B -- PRIVATE statement

   ffestd_R521B();

   Verify that PRIVATE is valid here (outside a derived-type statement).  */

void
ffestd_R521B ()
{
  ffestd_check_simple_ ();

  ffestd_subr_f90_ ();
  return;

#ifdef FFESTD_F90
  fputs ("* PRIVATE_outside_of_R422_derived_type_def\n", dmpout);
#endif
}

/* ffestd_R521Bstart -- PRIVATE statement list begin

   ffestd_R521Bstart();

   Verify that PRIVATE is valid here, and begin accepting items in the list.  */

void
ffestd_R521Bstart ()
{
  ffestd_check_start_ ();

  ffestd_subr_f90_ ();
  return;

#ifdef FFESTD_F90
  fputs ("* PRIVATE ", dmpout);
#endif
}

/* ffestd_R521Bitem -- PRIVATE statement for name

   ffestd_R521Bitem(name_token);

   Make sure name_token identifies a valid object to be PRIVATEed.  */

void
ffestd_R521Bitem (ffelexToken name)
{
  ffestd_check_item_ ();

  return;			/* F90. */

#ifdef FFESTD_F90
  fprintf (dmpout, "%s,", ffelex_token_text (name));
#endif
}

/* ffestd_R521Bfinish -- PRIVATE statement list complete

   ffestd_R521Bfinish();

   Just wrap up any local activities.  */

void
ffestd_R521Bfinish ()
{
  ffestd_check_finish_ ();

  return;			/* F90. */

#ifdef FFESTD_F90
  fputc ('\n', dmpout);
#endif
}

#endif
/* ffestd_R522 -- SAVE statement with no list

   ffestd_R522();

   Verify that SAVE is valid here, and flag everything as SAVEd.  */

void
ffestd_R522 ()
{
  ffestd_check_simple_ ();
}

/* ffestd_R522start -- SAVE statement list begin

   ffestd_R522start();

   Verify that SAVE is valid here, and begin accepting items in the list.  */

void
ffestd_R522start ()
{
  ffestd_check_start_ ();
}

/* ffestd_R522item_object -- SAVE statement for object-name

   ffestd_R522item_object(name_token);

   Make sure name_token identifies a valid object to be SAVEd.	*/

void
ffestd_R522item_object (ffelexToken name UNUSED)
{
  ffestd_check_item_ ();
}

/* ffestd_R522item_cblock -- SAVE statement for common-block-name

   ffestd_R522item_cblock(name_token);

   Make sure name_token identifies a valid common block to be SAVEd.  */

void
ffestd_R522item_cblock (ffelexToken name UNUSED)
{
  ffestd_check_item_ ();
}

/* ffestd_R522finish -- SAVE statement list complete

   ffestd_R522finish();

   Just wrap up any local activities.  */

void
ffestd_R522finish ()
{
  ffestd_check_finish_ ();
}

/* ffestd_R524_start -- DIMENSION statement list begin

   ffestd_R524_start(bool virtual);

   Verify that DIMENSION is valid here, and begin accepting items in the list.	*/

void
ffestd_R524_start (bool virtual UNUSED)
{
  ffestd_check_start_ ();
}

/* ffestd_R524_item -- DIMENSION statement for object-name

   ffestd_R524_item(name_token,dim_list);

   Make sure name_token identifies a valid object to be DIMENSIONd.  */

void
ffestd_R524_item (ffelexToken name UNUSED, ffesttDimList dims UNUSED)
{
  ffestd_check_item_ ();
}

/* ffestd_R524_finish -- DIMENSION statement list complete

   ffestd_R524_finish();

   Just wrap up any local activities.  */

void
ffestd_R524_finish ()
{
  ffestd_check_finish_ ();
}

/* ffestd_R525_start -- ALLOCATABLE statement list begin

   ffestd_R525_start();

   Verify that ALLOCATABLE is valid here, and begin accepting items in the
   list.  */

#if FFESTR_F90
void
ffestd_R525_start ()
{
  ffestd_check_start_ ();

  ffestd_subr_f90_ ();
  return;

#ifdef FFESTD_F90
  fputs ("* ALLOCATABLE ", dmpout);
#endif
}

/* ffestd_R525_item -- ALLOCATABLE statement for object-name

   ffestd_R525_item(name_token,dim_list);

   Make sure name_token identifies a valid object to be ALLOCATABLEd.  */

void
ffestd_R525_item (ffelexToken name, ffesttDimList dims)
{
  ffestd_check_item_ ();

  return;			/* F90. */

#ifdef FFESTD_F90
  fputs (ffelex_token_text (name), dmpout);
  if (dims != NULL)
    {
      fputc ('(', dmpout);
      ffestt_dimlist_dump (dims);
      fputc (')', dmpout);
    }
  fputc (',', dmpout);
#endif
}

/* ffestd_R525_finish -- ALLOCATABLE statement list complete

   ffestd_R525_finish();

   Just wrap up any local activities.  */

void
ffestd_R525_finish ()
{
  ffestd_check_finish_ ();

  return;			/* F90. */

#ifdef FFESTD_F90
  fputc ('\n', dmpout);
#endif
}

/* ffestd_R526_start -- POINTER statement list begin

   ffestd_R526_start();

   Verify that POINTER is valid here, and begin accepting items in the
   list.  */

void
ffestd_R526_start ()
{
  ffestd_check_start_ ();

  ffestd_subr_f90_ ();
  return;

#ifdef FFESTD_F90
  fputs ("* POINTER ", dmpout);
#endif
}

/* ffestd_R526_item -- POINTER statement for object-name

   ffestd_R526_item(name_token,dim_list);

   Make sure name_token identifies a valid object to be POINTERd.  */

void
ffestd_R526_item (ffelexToken name, ffesttDimList dims)
{
  ffestd_check_item_ ();

  return;			/* F90. */

#ifdef FFESTD_F90
  fputs (ffelex_token_text (name), dmpout);
  if (dims != NULL)
    {
      fputc ('(', dmpout);
      ffestt_dimlist_dump (dims);
      fputc (')', dmpout);
    }
  fputc (',', dmpout);
#endif
}

/* ffestd_R526_finish -- POINTER statement list complete

   ffestd_R526_finish();

   Just wrap up any local activities.  */

void
ffestd_R526_finish ()
{
  ffestd_check_finish_ ();

  return;			/* F90. */

#ifdef FFESTD_F90
  fputc ('\n', dmpout);
#endif
}

/* ffestd_R527_start -- TARGET statement list begin

   ffestd_R527_start();

   Verify that TARGET is valid here, and begin accepting items in the
   list.  */

void
ffestd_R527_start ()
{
  ffestd_check_start_ ();

  ffestd_subr_f90_ ();
  return;

#ifdef FFESTD_F90
  fputs ("* TARGET ", dmpout);
#endif
}

/* ffestd_R527_item -- TARGET statement for object-name

   ffestd_R527_item(name_token,dim_list);

   Make sure name_token identifies a valid object to be TARGETd.  */

void
ffestd_R527_item (ffelexToken name, ffesttDimList dims)
{
  ffestd_check_item_ ();

  return;			/* F90. */

#ifdef FFESTD_F90
  fputs (ffelex_token_text (name), dmpout);
  if (dims != NULL)
    {
      fputc ('(', dmpout);
      ffestt_dimlist_dump (dims);
      fputc (')', dmpout);
    }
  fputc (',', dmpout);
#endif
}

/* ffestd_R527_finish -- TARGET statement list complete

   ffestd_R527_finish();

   Just wrap up any local activities.  */

void
ffestd_R527_finish ()
{
  ffestd_check_finish_ ();

  return;			/* F90. */

#ifdef FFESTD_F90
  fputc ('\n', dmpout);
#endif
}

#endif
/* ffestd_R537_start -- PARAMETER statement list begin

   ffestd_R537_start();

   Verify that PARAMETER is valid here, and begin accepting items in the list.	*/

void
ffestd_R537_start ()
{
  ffestd_check_start_ ();
}

/* ffestd_R537_item -- PARAMETER statement assignment

   ffestd_R537_item(dest,dest_token,source,source_token);

   Make sure the source is a valid source for the destination; make the
   assignment.	*/

void
ffestd_R537_item (ffebld dest UNUSED, ffebld source UNUSED)
{
  ffestd_check_item_ ();
}

/* ffestd_R537_finish -- PARAMETER statement list complete

   ffestd_R537_finish();

   Just wrap up any local activities.  */

void
ffestd_R537_finish ()
{
  ffestd_check_finish_ ();
}

/* ffestd_R539 -- IMPLICIT NONE statement

   ffestd_R539();

   Verify that the IMPLICIT NONE statement is ok here and implement.  */

void
ffestd_R539 ()
{
  ffestd_check_simple_ ();
}

/* ffestd_R539start -- IMPLICIT statement

   ffestd_R539start();

   Verify that the IMPLICIT statement is ok here and implement.	 */

void
ffestd_R539start ()
{
  ffestd_check_start_ ();
}

/* ffestd_R539item -- IMPLICIT statement specification (R540)

   ffestd_R539item(...);

   Verify that the type and letter list are all ok and implement.  */

void
ffestd_R539item (ffestpType type UNUSED, ffebld kind UNUSED,
		 ffelexToken kindt UNUSED, ffebld len UNUSED,
		 ffelexToken lent UNUSED, ffesttImpList letters UNUSED)
{
  ffestd_check_item_ ();
}

/* ffestd_R539finish -- IMPLICIT statement

   ffestd_R539finish();

   Finish up any local activities.  */

void
ffestd_R539finish ()
{
  ffestd_check_finish_ ();
}

/* ffestd_R542_start -- NAMELIST statement list begin

   ffestd_R542_start();

   Verify that NAMELIST is valid here, and begin accepting items in the list.  */

void
ffestd_R542_start ()
{
  ffestd_check_start_ ();
}

/* ffestd_R542_item_nlist -- NAMELIST statement for group-name

   ffestd_R542_item_nlist(groupname_token);

   Make sure name_token identifies a valid object to be NAMELISTd.  */

void
ffestd_R542_item_nlist (ffelexToken name UNUSED)
{
  ffestd_check_item_ ();
}

/* ffestd_R542_item_nitem -- NAMELIST statement for variable-name

   ffestd_R542_item_nitem(name_token);

   Make sure name_token identifies a valid object to be NAMELISTd.  */

void
ffestd_R542_item_nitem (ffelexToken name UNUSED)
{
  ffestd_check_item_ ();
}

/* ffestd_R542_finish -- NAMELIST statement list complete

   ffestd_R542_finish();

   Just wrap up any local activities.  */

void
ffestd_R542_finish ()
{
  ffestd_check_finish_ ();
}

/* ffestd_R544_start -- EQUIVALENCE statement list begin

   ffestd_R544_start();

   Verify that EQUIVALENCE is valid here, and begin accepting items in the
   list.  */

#if 0
void
ffestd_R544_start ()
{
  ffestd_check_start_ ();
}

#endif
/* ffestd_R544_item -- EQUIVALENCE statement assignment

   ffestd_R544_item(exprlist);

   Make sure the equivalence is valid, then implement it.  */

#if 0
void
ffestd_R544_item (ffesttExprList exprlist)
{
  ffestd_check_item_ ();
}

#endif
/* ffestd_R544_finish -- EQUIVALENCE statement list complete

   ffestd_R544_finish();

   Just wrap up any local activities.  */

#if 0
void
ffestd_R544_finish ()
{
  ffestd_check_finish_ ();
}

#endif
/* ffestd_R547_start -- COMMON statement list begin

   ffestd_R547_start();

   Verify that COMMON is valid here, and begin accepting items in the list.  */

void
ffestd_R547_start ()
{
  ffestd_check_start_ ();
}

/* ffestd_R547_item_object -- COMMON statement for object-name

   ffestd_R547_item_object(name_token,dim_list);

   Make sure name_token identifies a valid object to be COMMONd.  */

void
ffestd_R547_item_object (ffelexToken name UNUSED,
			 ffesttDimList dims UNUSED)
{
  ffestd_check_item_ ();
}

/* ffestd_R547_item_cblock -- COMMON statement for common-block-name

   ffestd_R547_item_cblock(name_token);

   Make sure name_token identifies a valid common block to be COMMONd.	*/

void
ffestd_R547_item_cblock (ffelexToken name UNUSED)
{
  ffestd_check_item_ ();
}

/* ffestd_R547_finish -- COMMON statement list complete

   ffestd_R547_finish();

   Just wrap up any local activities.  */

void
ffestd_R547_finish ()
{
  ffestd_check_finish_ ();
}

/* ffestd_R620 -- ALLOCATE statement

   ffestd_R620(exprlist,stat,stat_token);

   Make sure the expression list is valid, then implement it.  */

#if FFESTR_F90
void
ffestd_R620 (ffesttExprList exprlist, ffebld stat)
{
  ffestd_check_simple_ ();

  ffestd_subr_f90_ ();
}

/* ffestd_R624 -- NULLIFY statement

   ffestd_R624(pointer_name_list);

   Make sure pointer_name_list identifies valid pointers for a NULLIFY.	 */

void
ffestd_R624 (ffesttExprList pointers)
{
  ffestd_check_simple_ ();

  ffestd_subr_f90_ ();
  return;

#ifdef FFESTD_F90
  fputs ("+ NULLIFY (", dmpout);
  assert (pointers != NULL);
  ffestt_exprlist_dump (pointers);
  fputs (")\n", dmpout);
#endif
}

/* ffestd_R625 -- DEALLOCATE statement

   ffestd_R625(exprlist,stat,stat_token);

   Make sure the equivalence is valid, then implement it.  */

void
ffestd_R625 (ffesttExprList exprlist, ffebld stat)
{
  ffestd_check_simple_ ();

  ffestd_subr_f90_ ();
}

#endif
/* ffestd_R737A -- Assignment statement outside of WHERE

   ffestd_R737A(dest_expr,source_expr);	 */

void
ffestd_R737A (ffebld dest, ffebld source)
{
  ffestdStmt_ stmt;

  ffestd_check_simple_ ();

  stmt = ffestd_stmt_new_ (FFESTD_stmtidR737A_);
  ffestd_stmt_append_ (stmt);
  ffestd_subr_line_save_ (stmt);
  stmt->u.R737A.pool = ffesta_output_pool;
  stmt->u.R737A.dest = dest;
  stmt->u.R737A.source = source;
  ffesta_set_outpooldisp (FFESTA_pooldispPRESERVE);
}

/* ffestd_R737B -- Assignment statement inside of WHERE

   ffestd_R737B(dest_expr,source_expr);	 */

#if FFESTR_F90
void
ffestd_R737B (ffebld dest, ffebld source)
{
  ffestd_check_simple_ ();
}

/* ffestd_R738 -- Pointer assignment statement

   ffestd_R738(dest_expr,source_expr,source_token);

   Make sure the assignment is valid.  */

void
ffestd_R738 (ffebld dest, ffebld source)
{
  ffestd_check_simple_ ();

  ffestd_subr_f90_ ();
}

/* ffestd_R740 -- WHERE statement

   ffestd_R740(expr,expr_token);

   Make sure statement is valid here; implement.  */

void
ffestd_R740 (ffebld expr)
{
  ffestd_check_simple_ ();

  ffestd_subr_f90_ ();
}

/* ffestd_R742 -- WHERE-construct statement

   ffestd_R742(expr,expr_token);

   Make sure statement is valid here; implement.  */

void
ffestd_R742 (ffebld expr)
{
  ffestd_check_simple_ ();

  ffestd_subr_f90_ ();
}

/* ffestd_R744 -- ELSE WHERE statement

   ffestd_R744();

   Make sure ffestd_kind_ identifies a WHERE block.
   Implement the ELSE of the current WHERE block.  */

void
ffestd_R744 ()
{
  ffestd_check_simple_ ();

  return;			/* F90. */

#ifdef FFESTD_F90
  fputs ("+ ELSE_WHERE\n", dmpout);
#endif
}

/* ffestd_R745 -- Implicit END WHERE statement.  */

void
ffestd_R745 (bool ok)
{
  return;			/* F90. */

#ifdef FFESTD_F90
  fputs ("+ END_WHERE\n", dmpout);	/* Also see ffestd_R745. */

  --ffestd_block_level_;
  assert (ffestd_block_level_ >= 0);
#endif
}

#endif

/* Block IF (IF-THEN) statement.  */

void
ffestd_R803 (ffelexToken construct_name UNUSED, ffebld expr)
{
  ffestdStmt_ stmt;

  ffestd_check_simple_ ();

  stmt = ffestd_stmt_new_ (FFESTD_stmtidR803_);
  ffestd_stmt_append_ (stmt);
  ffestd_subr_line_save_ (stmt);
  stmt->u.R803.pool = ffesta_output_pool;
  stmt->u.R803.block = ffestw_use (ffestw_stack_top ());
  stmt->u.R803.expr = expr;
  ffesta_set_outpooldisp (FFESTA_pooldispPRESERVE);

  ++ffestd_block_level_;
  assert (ffestd_block_level_ > 0);
}

/* ELSE IF statement.  */

void
ffestd_R804 (ffebld expr, ffelexToken name UNUSED)
{
  ffestdStmt_ stmt;

  ffestd_check_simple_ ();

  stmt = ffestd_stmt_new_ (FFESTD_stmtidR804_);
  ffestd_stmt_append_ (stmt);
  ffestd_subr_line_save_ (stmt);
  stmt->u.R804.pool = ffesta_output_pool;
  stmt->u.R804.block = ffestw_use (ffestw_stack_top ());
  stmt->u.R804.expr = expr;
  ffesta_set_outpooldisp (FFESTA_pooldispPRESERVE);
}

/* ELSE statement.  */

void
ffestd_R805 (ffelexToken name UNUSED)
{
  ffestdStmt_ stmt;

  ffestd_check_simple_ ();

  stmt = ffestd_stmt_new_ (FFESTD_stmtidR805_);
  ffestd_stmt_append_ (stmt);
  ffestd_subr_line_save_ (stmt);
  stmt->u.R805.block = ffestw_use (ffestw_stack_top ());
}

/* END IF statement.  */

void
ffestd_R806 (bool ok UNUSED)
{
  ffestdStmt_ stmt;

  stmt = ffestd_stmt_new_ (FFESTD_stmtidR806_);
  ffestd_stmt_append_ (stmt);
  ffestd_subr_line_save_ (stmt);
  stmt->u.R806.block = ffestw_use (ffestw_stack_top ());

  --ffestd_block_level_;
  assert (ffestd_block_level_ >= 0);
}

/* ffestd_R807 -- Logical IF statement

   ffestd_R807(expr,expr_token);

   Make sure statement is valid here; implement.  */

void
ffestd_R807 (ffebld expr)
{
  ffestdStmt_ stmt;

  ffestd_check_simple_ ();

  stmt = ffestd_stmt_new_ (FFESTD_stmtidR807_);
  ffestd_stmt_append_ (stmt);
  ffestd_subr_line_save_ (stmt);
  stmt->u.R807.pool = ffesta_output_pool;
  stmt->u.R807.expr = expr;
  ffesta_set_outpooldisp (FFESTA_pooldispPRESERVE);

  ++ffestd_block_level_;
  assert (ffestd_block_level_ > 0);
}

/* ffestd_R809 -- SELECT CASE statement

   ffestd_R809(construct_name,expr,expr_token);

   Make sure statement is valid here; implement.  */

void
ffestd_R809 (ffelexToken construct_name UNUSED, ffebld expr)
{
  ffestdStmt_ stmt;

  ffestd_check_simple_ ();

  stmt = ffestd_stmt_new_ (FFESTD_stmtidR809_);
  ffestd_stmt_append_ (stmt);
  ffestd_subr_line_save_ (stmt);
  stmt->u.R809.pool = ffesta_output_pool;
  stmt->u.R809.block = ffestw_use (ffestw_stack_top ());
  stmt->u.R809.expr = expr;
  ffesta_set_outpooldisp (FFESTA_pooldispPRESERVE);
  malloc_pool_use (ffestw_select (ffestw_stack_top ())->pool);

  ++ffestd_block_level_;
  assert (ffestd_block_level_ > 0);
}

/* ffestd_R810 -- CASE statement

   ffestd_R810(case_value_range_list,name);

   If casenum is 0, it's CASE DEFAULT.	Else it's the case ranges at
   the start of the first_stmt list in the select object at the top of
   the stack that match casenum.  */

void
ffestd_R810 (unsigned long casenum)
{
  ffestdStmt_ stmt;

  ffestd_check_simple_ ();

  stmt = ffestd_stmt_new_ (FFESTD_stmtidR810_);
  ffestd_stmt_append_ (stmt);
  ffestd_subr_line_save_ (stmt);
  stmt->u.R810.pool = ffesta_output_pool;
  stmt->u.R810.block = ffestw_stack_top ();
  stmt->u.R810.casenum = casenum;
  ffesta_set_outpooldisp (FFESTA_pooldispPRESERVE);
}

/* ffestd_R811 -- End a SELECT

   ffestd_R811(TRUE);  */

void
ffestd_R811 (bool ok UNUSED)
{
  ffestdStmt_ stmt;

  stmt = ffestd_stmt_new_ (FFESTD_stmtidR811_);
  ffestd_stmt_append_ (stmt);
  ffestd_subr_line_save_ (stmt);
  stmt->u.R811.block = ffestw_stack_top ();

  --ffestd_block_level_;
  assert (ffestd_block_level_ >= 0);
}

/* ffestd_R819A -- Iterative DO statement

   ffestd_R819A(construct_name,label_token,expr,expr_token);

   Make sure statement is valid here; implement.  */

void
ffestd_R819A (ffelexToken construct_name UNUSED, ffelab label,
	      ffebld var, ffebld start, ffelexToken start_token,
	      ffebld end, ffelexToken end_token,
	      ffebld incr, ffelexToken incr_token)
{
  ffestdStmt_ stmt;

  ffestd_check_simple_ ();

  stmt = ffestd_stmt_new_ (FFESTD_stmtidR819A_);
  ffestd_stmt_append_ (stmt);
  ffestd_subr_line_save_ (stmt);
  stmt->u.R819A.pool = ffesta_output_pool;
  stmt->u.R819A.block = ffestw_use (ffestw_stack_top ());
  stmt->u.R819A.label = label;
  stmt->u.R819A.var = var;
  stmt->u.R819A.start = start;
  stmt->u.R819A.start_token = ffelex_token_use (start_token);
  stmt->u.R819A.end = end;
  stmt->u.R819A.end_token = ffelex_token_use (end_token);
  stmt->u.R819A.incr = incr;
  stmt->u.R819A.incr_token = (incr_token == NULL) ? NULL
    : ffelex_token_use (incr_token);
  ffesta_set_outpooldisp (FFESTA_pooldispPRESERVE);

  ++ffestd_block_level_;
  assert (ffestd_block_level_ > 0);
}

/* ffestd_R819B -- DO WHILE statement

   ffestd_R819B(construct_name,label_token,expr,expr_token);

   Make sure statement is valid here; implement.  */

void
ffestd_R819B (ffelexToken construct_name UNUSED, ffelab label,
	      ffebld expr)
{
  ffestdStmt_ stmt;

  ffestd_check_simple_ ();

  stmt = ffestd_stmt_new_ (FFESTD_stmtidR819B_);
  ffestd_stmt_append_ (stmt);
  ffestd_subr_line_save_ (stmt);
  stmt->u.R819B.pool = ffesta_output_pool;
  stmt->u.R819B.block = ffestw_use (ffestw_stack_top ());
  stmt->u.R819B.label = label;
  stmt->u.R819B.expr = expr;
  ffesta_set_outpooldisp (FFESTA_pooldispPRESERVE);

  ++ffestd_block_level_;
  assert (ffestd_block_level_ > 0);
}

/* ffestd_R825 -- END DO statement

   ffestd_R825(name_token);

   Make sure ffestd_kind_ identifies a DO block.  If not
   NULL, make sure name_token gives the correct name.  Do whatever
   is specific to seeing END DO with a DO-target label definition on it,
   where the END DO is really treated as a CONTINUE (i.e. generate th
   same code you would for CONTINUE).  ffestd_do handles the actual
   generation of end-loop code.	 */

void
ffestd_R825 (ffelexToken name UNUSED)
{
  ffestdStmt_ stmt;

  ffestd_check_simple_ ();

  stmt = ffestd_stmt_new_ (FFESTD_stmtidR825_);
  ffestd_stmt_append_ (stmt);
  ffestd_subr_line_save_ (stmt);
}

/* ffestd_R834 -- CYCLE statement

   ffestd_R834(name_token);

   Handle a CYCLE within a loop.  */

void
ffestd_R834 (ffestw block)
{
  ffestdStmt_ stmt;

  ffestd_check_simple_ ();

  stmt = ffestd_stmt_new_ (FFESTD_stmtidR834_);
  ffestd_stmt_append_ (stmt);
  ffestd_subr_line_save_ (stmt);
  stmt->u.R834.block = block;
}

/* ffestd_R835 -- EXIT statement

   ffestd_R835(name_token);

   Handle a EXIT within a loop.	 */

void
ffestd_R835 (ffestw block)
{
  ffestdStmt_ stmt;

  ffestd_check_simple_ ();

  stmt = ffestd_stmt_new_ (FFESTD_stmtidR835_);
  ffestd_stmt_append_ (stmt);
  ffestd_subr_line_save_ (stmt);
  stmt->u.R835.block = block;
}

/* ffestd_R836 -- GOTO statement

   ffestd_R836(label);

   Make sure label_token identifies a valid label for a GOTO.  Update
   that label's info to indicate it is the target of a GOTO.  */

void
ffestd_R836 (ffelab label)
{
  ffestdStmt_ stmt;

  ffestd_check_simple_ ();

  stmt = ffestd_stmt_new_ (FFESTD_stmtidR836_);
  ffestd_stmt_append_ (stmt);
  ffestd_subr_line_save_ (stmt);
  stmt->u.R836.label = label;

  if (ffestd_block_level_ == 0)
    ffestd_is_reachable_ = FALSE;
}

/* ffestd_R837 -- Computed GOTO statement

   ffestd_R837(labels,expr);

   Make sure label_list identifies valid labels for a GOTO.  Update
   each label's info to indicate it is the target of a GOTO.  */

void
ffestd_R837 (ffelab *labels, int count, ffebld expr)
{
  ffestdStmt_ stmt;

  ffestd_check_simple_ ();

  stmt = ffestd_stmt_new_ (FFESTD_stmtidR837_);
  ffestd_stmt_append_ (stmt);
  ffestd_subr_line_save_ (stmt);
  stmt->u.R837.pool = ffesta_output_pool;
  stmt->u.R837.labels = labels;
  stmt->u.R837.count = count;
  stmt->u.R837.expr = expr;
  ffesta_set_outpooldisp (FFESTA_pooldispPRESERVE);
}

/* ffestd_R838 -- ASSIGN statement

   ffestd_R838(label_token,target_variable,target_token);

   Make sure label_token identifies a valid label for an assignment.  Update
   that label's info to indicate it is the source of an assignment.  Update
   target_variable's info to indicate it is the target the assignment of that
   label.  */

void
ffestd_R838 (ffelab label, ffebld target)
{
  ffestdStmt_ stmt;

  ffestd_check_simple_ ();

  stmt = ffestd_stmt_new_ (FFESTD_stmtidR838_);
  ffestd_stmt_append_ (stmt);
  ffestd_subr_line_save_ (stmt);
  stmt->u.R838.pool = ffesta_output_pool;
  stmt->u.R838.label = label;
  stmt->u.R838.target = target;
  ffesta_set_outpooldisp (FFESTA_pooldispPRESERVE);
}

/* ffestd_R839 -- Assigned GOTO statement

   ffestd_R839(target,labels);

   Make sure label_list identifies valid labels for a GOTO.  Update
   each label's info to indicate it is the target of a GOTO.  */

void
ffestd_R839 (ffebld target, ffelab *labels UNUSED, int count UNUSED)
{
  ffestdStmt_ stmt;

  ffestd_check_simple_ ();

  stmt = ffestd_stmt_new_ (FFESTD_stmtidR839_);
  ffestd_stmt_append_ (stmt);
  ffestd_subr_line_save_ (stmt);
  stmt->u.R839.pool = ffesta_output_pool;
  stmt->u.R839.target = target;
  ffesta_set_outpooldisp (FFESTA_pooldispPRESERVE);

  if (ffestd_block_level_ == 0)
    ffestd_is_reachable_ = FALSE;
}

/* ffestd_R840 -- Arithmetic IF statement

   ffestd_R840(expr,expr_token,neg,zero,pos);

   Make sure the labels are valid; implement.  */

void
ffestd_R840 (ffebld expr, ffelab neg, ffelab zero, ffelab pos)
{
  ffestdStmt_ stmt;

  ffestd_check_simple_ ();

  stmt = ffestd_stmt_new_ (FFESTD_stmtidR840_);
  ffestd_stmt_append_ (stmt);
  ffestd_subr_line_save_ (stmt);
  stmt->u.R840.pool = ffesta_output_pool;
  stmt->u.R840.expr = expr;
  stmt->u.R840.neg = neg;
  stmt->u.R840.zero = zero;
  stmt->u.R840.pos = pos;
  ffesta_set_outpooldisp (FFESTA_pooldispPRESERVE);

  if (ffestd_block_level_ == 0)
    ffestd_is_reachable_ = FALSE;
}

/* ffestd_R841 -- CONTINUE statement

   ffestd_R841();  */

void
ffestd_R841 (bool in_where UNUSED)
{
  ffestdStmt_ stmt;

  ffestd_check_simple_ ();

  stmt = ffestd_stmt_new_ (FFESTD_stmtidR841_);
  ffestd_stmt_append_ (stmt);
  ffestd_subr_line_save_ (stmt);
}

/* ffestd_R842 -- STOP statement

   ffestd_R842(expr);  */

void
ffestd_R842 (ffebld expr)
{
  ffestdStmt_ stmt;

  ffestd_check_simple_ ();

  stmt = ffestd_stmt_new_ (FFESTD_stmtidR842_);
  ffestd_stmt_append_ (stmt);
  ffestd_subr_line_save_ (stmt);
  if (ffesta_outpooldisp () == FFESTA_pooldispPRESERVE)
    {
      /* This is a "spurious" (automatically-generated) STOP
	 that follows a previous STOP or other statement.
	 Make sure we don't have an expression in the pool,
	 and then mark that the pool has already been killed.  */
      assert (expr == NULL);
      stmt->u.R842.pool = NULL;
      stmt->u.R842.expr = NULL;
    }
  else
    {
      stmt->u.R842.pool = ffesta_output_pool;
      stmt->u.R842.expr = expr;
      ffesta_set_outpooldisp (FFESTA_pooldispPRESERVE);
    }

  if (ffestd_block_level_ == 0)
    ffestd_is_reachable_ = FALSE;
}

/* ffestd_R843 -- PAUSE statement

   ffestd_R843(expr,expr_token);

   Make sure statement is valid here; implement.  expr and expr_token are
   both NULL if there was no expression.  */

void
ffestd_R843 (ffebld expr)
{
  ffestdStmt_ stmt;

  ffestd_check_simple_ ();

  stmt = ffestd_stmt_new_ (FFESTD_stmtidR843_);
  ffestd_stmt_append_ (stmt);
  ffestd_subr_line_save_ (stmt);
  stmt->u.R843.pool = ffesta_output_pool;
  stmt->u.R843.expr = expr;
  ffesta_set_outpooldisp (FFESTA_pooldispPRESERVE);
}

/* ffestd_R904 -- OPEN statement

   ffestd_R904();

   Make sure an OPEN is valid in the current context, and implement it.	 */

void
ffestd_R904 ()
{
  ffestdStmt_ stmt;

  ffestd_check_simple_ ();

#define specified(something) \
      (ffestp_file.open.open_spec[something].kw_or_val_present)

  /* Warn if there are any thing we don't handle via f2c libraries. */

  if (specified (FFESTP_openixACTION)
      || specified (FFESTP_openixASSOCIATEVARIABLE)
      || specified (FFESTP_openixBLOCKSIZE)
      || specified (FFESTP_openixBUFFERCOUNT)
      || specified (FFESTP_openixCARRIAGECONTROL)
      || specified (FFESTP_openixDEFAULTFILE)
      || specified (FFESTP_openixDELIM)
      || specified (FFESTP_openixDISPOSE)
      || specified (FFESTP_openixEXTENDSIZE)
      || specified (FFESTP_openixINITIALSIZE)
      || specified (FFESTP_openixKEY)
      || specified (FFESTP_openixMAXREC)
      || specified (FFESTP_openixNOSPANBLOCKS)
      || specified (FFESTP_openixORGANIZATION)
      || specified (FFESTP_openixPAD)
      || specified (FFESTP_openixPOSITION)
      || specified (FFESTP_openixREADONLY)
      || specified (FFESTP_openixRECORDTYPE)
      || specified (FFESTP_openixSHARED)
      || specified (FFESTP_openixUSEROPEN))
    {
      ffebad_start (FFEBAD_OPEN_UNSUPPORTED);
      ffebad_here (0, ffelex_token_where_line (ffesta_tokens[0]),
		   ffelex_token_where_column (ffesta_tokens[0]));
      ffebad_finish ();
    }

#undef specified

  stmt = ffestd_stmt_new_ (FFESTD_stmtidR904_);
  ffestd_stmt_append_ (stmt);
  ffestd_subr_line_save_ (stmt);
  stmt->u.R904.pool = ffesta_output_pool;
  stmt->u.R904.params = ffestd_subr_copy_open_ ();
  ffesta_set_outpooldisp (FFESTA_pooldispPRESERVE);
}

/* ffestd_R907 -- CLOSE statement

   ffestd_R907();

   Make sure a CLOSE is valid in the current context, and implement it.	 */

void
ffestd_R907 ()
{
  ffestdStmt_ stmt;

  ffestd_check_simple_ ();

  stmt = ffestd_stmt_new_ (FFESTD_stmtidR907_);
  ffestd_stmt_append_ (stmt);
  ffestd_subr_line_save_ (stmt);
  stmt->u.R907.pool = ffesta_output_pool;
  stmt->u.R907.params = ffestd_subr_copy_close_ ();
  ffesta_set_outpooldisp (FFESTA_pooldispPRESERVE);
}

/* ffestd_R909_start -- READ(...) statement list begin

   ffestd_R909_start(FALSE);

   Verify that READ is valid here, and begin accepting items in the
   list.  */

void
ffestd_R909_start (bool only_format, ffestvUnit unit,
		   ffestvFormat format, bool rec, bool key)
{
  ffestdStmt_ stmt;

  ffestd_check_start_ ();

#define specified(something) \
      (ffestp_file.read.read_spec[something].kw_or_val_present)

  /* Warn if there are any thing we don't handle via f2c libraries. */
  if (specified (FFESTP_readixADVANCE)
      || specified (FFESTP_readixEOR)
      || specified (FFESTP_readixKEYEQ)
      || specified (FFESTP_readixKEYGE)
      || specified (FFESTP_readixKEYGT)
      || specified (FFESTP_readixKEYID)
      || specified (FFESTP_readixNULLS)
      || specified (FFESTP_readixSIZE))
    {
      ffebad_start (FFEBAD_READ_UNSUPPORTED);
      ffebad_here (0, ffelex_token_where_line (ffesta_tokens[0]),
		   ffelex_token_where_column (ffesta_tokens[0]));
      ffebad_finish ();
    }

#undef specified

  stmt = ffestd_stmt_new_ (FFESTD_stmtidR909_);
  ffestd_stmt_append_ (stmt);
  ffestd_subr_line_save_ (stmt);
  stmt->u.R909.pool = ffesta_output_pool;
  stmt->u.R909.params = ffestd_subr_copy_read_ ();
  stmt->u.R909.only_format = only_format;
  stmt->u.R909.unit = unit;
  stmt->u.R909.format = format;
  stmt->u.R909.rec = rec;
  stmt->u.R909.key = key;
  stmt->u.R909.list = NULL;
  ffestd_expr_list_ = &stmt->u.R909.list;
  ffesta_set_outpooldisp (FFESTA_pooldispPRESERVE);
}

/* ffestd_R909_item -- READ statement i/o item

   ffestd_R909_item(expr,expr_token);

   Implement output-list expression.  */

void
ffestd_R909_item (ffebld expr, ffelexToken expr_token)
{
  ffestdExprItem_ item;

  ffestd_check_item_ ();

  item = (ffestdExprItem_) malloc_new_kp (ffesta_output_pool,
					  "ffestdExprItem_", sizeof (*item));

  item->next = NULL;
  item->expr = expr;
  item->token = ffelex_token_use (expr_token);
  *ffestd_expr_list_ = item;
  ffestd_expr_list_ = &item->next;
}

/* ffestd_R909_finish -- READ statement list complete

   ffestd_R909_finish();

   Just wrap up any local activities.  */

void
ffestd_R909_finish ()
{
  ffestd_check_finish_ ();
}

/* ffestd_R910_start -- WRITE(...) statement list begin

   ffestd_R910_start();

   Verify that WRITE is valid here, and begin accepting items in the
   list.  */

void
ffestd_R910_start (ffestvUnit unit, ffestvFormat format, bool rec)
{
  ffestdStmt_ stmt;

  ffestd_check_start_ ();

#define specified(something) \
      (ffestp_file.write.write_spec[something].kw_or_val_present)

  /* Warn if there are any thing we don't handle via f2c libraries. */
  if (specified (FFESTP_writeixADVANCE)
      || specified (FFESTP_writeixEOR))
    {
      ffebad_start (FFEBAD_WRITE_UNSUPPORTED);
      ffebad_here (0, ffelex_token_where_line (ffesta_tokens[0]),
		   ffelex_token_where_column (ffesta_tokens[0]));
      ffebad_finish ();
    }

#undef specified

  stmt = ffestd_stmt_new_ (FFESTD_stmtidR910_);
  ffestd_stmt_append_ (stmt);
  ffestd_subr_line_save_ (stmt);
  stmt->u.R910.pool = ffesta_output_pool;
  stmt->u.R910.params = ffestd_subr_copy_write_ ();
  stmt->u.R910.unit = unit;
  stmt->u.R910.format = format;
  stmt->u.R910.rec = rec;
  stmt->u.R910.list = NULL;
  ffestd_expr_list_ = &stmt->u.R910.list;
  ffesta_set_outpooldisp (FFESTA_pooldispPRESERVE);
}

/* ffestd_R910_item -- WRITE statement i/o item

   ffestd_R910_item(expr,expr_token);

   Implement output-list expression.  */

void
ffestd_R910_item (ffebld expr, ffelexToken expr_token)
{
  ffestdExprItem_ item;

  ffestd_check_item_ ();

  item = (ffestdExprItem_) malloc_new_kp (ffesta_output_pool,
					  "ffestdExprItem_", sizeof (*item));

  item->next = NULL;
  item->expr = expr;
  item->token = ffelex_token_use (expr_token);
  *ffestd_expr_list_ = item;
  ffestd_expr_list_ = &item->next;
}

/* ffestd_R910_finish -- WRITE statement list complete

   ffestd_R910_finish();

   Just wrap up any local activities.  */

void
ffestd_R910_finish ()
{
  ffestd_check_finish_ ();
}

/* ffestd_R911_start -- PRINT statement list begin

   ffestd_R911_start();

   Verify that PRINT is valid here, and begin accepting items in the
   list.  */

void
ffestd_R911_start (ffestvFormat format)
{
  ffestdStmt_ stmt;

  ffestd_check_start_ ();

  stmt = ffestd_stmt_new_ (FFESTD_stmtidR911_);
  ffestd_stmt_append_ (stmt);
  ffestd_subr_line_save_ (stmt);
  stmt->u.R911.pool = ffesta_output_pool;
  stmt->u.R911.params = ffestd_subr_copy_print_ ();
  stmt->u.R911.format = format;
  stmt->u.R911.list = NULL;
  ffestd_expr_list_ = &stmt->u.R911.list;
  ffesta_set_outpooldisp (FFESTA_pooldispPRESERVE);
}

/* ffestd_R911_item -- PRINT statement i/o item

   ffestd_R911_item(expr,expr_token);

   Implement output-list expression.  */

void
ffestd_R911_item (ffebld expr, ffelexToken expr_token)
{
  ffestdExprItem_ item;

  ffestd_check_item_ ();

  item = (ffestdExprItem_) malloc_new_kp (ffesta_output_pool,
					  "ffestdExprItem_", sizeof (*item));

  item->next = NULL;
  item->expr = expr;
  item->token = ffelex_token_use (expr_token);
  *ffestd_expr_list_ = item;
  ffestd_expr_list_ = &item->next;
}

/* ffestd_R911_finish -- PRINT statement list complete

   ffestd_R911_finish();

   Just wrap up any local activities.  */

void
ffestd_R911_finish ()
{
  ffestd_check_finish_ ();
}

/* ffestd_R919 -- BACKSPACE statement

   ffestd_R919();

   Make sure a BACKSPACE is valid in the current context, and implement it.  */

void
ffestd_R919 ()
{
  ffestdStmt_ stmt;

  ffestd_check_simple_ ();

  stmt = ffestd_stmt_new_ (FFESTD_stmtidR919_);
  ffestd_stmt_append_ (stmt);
  ffestd_subr_line_save_ (stmt);
  stmt->u.R919.pool = ffesta_output_pool;
  stmt->u.R919.params = ffestd_subr_copy_beru_ ();
  ffesta_set_outpooldisp (FFESTA_pooldispPRESERVE);
}

/* ffestd_R920 -- ENDFILE statement

   ffestd_R920();

   Make sure a ENDFILE is valid in the current context, and implement it.  */

void
ffestd_R920 ()
{
  ffestdStmt_ stmt;

  ffestd_check_simple_ ();

  stmt = ffestd_stmt_new_ (FFESTD_stmtidR920_);
  ffestd_stmt_append_ (stmt);
  ffestd_subr_line_save_ (stmt);
  stmt->u.R920.pool = ffesta_output_pool;
  stmt->u.R920.params = ffestd_subr_copy_beru_ ();
  ffesta_set_outpooldisp (FFESTA_pooldispPRESERVE);
}

/* ffestd_R921 -- REWIND statement

   ffestd_R921();

   Make sure a REWIND is valid in the current context, and implement it.  */

void
ffestd_R921 ()
{
  ffestdStmt_ stmt;

  ffestd_check_simple_ ();

  stmt = ffestd_stmt_new_ (FFESTD_stmtidR921_);
  ffestd_stmt_append_ (stmt);
  ffestd_subr_line_save_ (stmt);
  stmt->u.R921.pool = ffesta_output_pool;
  stmt->u.R921.params = ffestd_subr_copy_beru_ ();
  ffesta_set_outpooldisp (FFESTA_pooldispPRESERVE);
}

/* ffestd_R923A -- INQUIRE statement (non-IOLENGTH version)

   ffestd_R923A(bool by_file);

   Make sure an INQUIRE is valid in the current context, and implement it.  */

void
ffestd_R923A (bool by_file)
{
  ffestdStmt_ stmt;

  ffestd_check_simple_ ();

#define specified(something) \
      (ffestp_file.inquire.inquire_spec[something].kw_or_val_present)

  /* Warn if there are any thing we don't handle via f2c libraries. */
  if (specified (FFESTP_inquireixACTION)
      || specified (FFESTP_inquireixCARRIAGECONTROL)
      || specified (FFESTP_inquireixDEFAULTFILE)
      || specified (FFESTP_inquireixDELIM)
      || specified (FFESTP_inquireixKEYED)
      || specified (FFESTP_inquireixORGANIZATION)
      || specified (FFESTP_inquireixPAD)
      || specified (FFESTP_inquireixPOSITION)
      || specified (FFESTP_inquireixREAD)
      || specified (FFESTP_inquireixREADWRITE)
      || specified (FFESTP_inquireixRECORDTYPE)
      || specified (FFESTP_inquireixWRITE))
    {
      ffebad_start (FFEBAD_INQUIRE_UNSUPPORTED);
      ffebad_here (0, ffelex_token_where_line (ffesta_tokens[0]),
		   ffelex_token_where_column (ffesta_tokens[0]));
      ffebad_finish ();
    }

#undef specified

  stmt = ffestd_stmt_new_ (FFESTD_stmtidR923A_);
  ffestd_stmt_append_ (stmt);
  ffestd_subr_line_save_ (stmt);
  stmt->u.R923A.pool = ffesta_output_pool;
  stmt->u.R923A.params = ffestd_subr_copy_inquire_ ();
  stmt->u.R923A.by_file = by_file;
  ffesta_set_outpooldisp (FFESTA_pooldispPRESERVE);
}

/* ffestd_R923B_start -- INQUIRE(IOLENGTH=expr) statement list begin

   ffestd_R923B_start();

   Verify that INQUIRE is valid here, and begin accepting items in the
   list.  */

void
ffestd_R923B_start ()
{
  ffestdStmt_ stmt;

  ffestd_check_start_ ();

  stmt = ffestd_stmt_new_ (FFESTD_stmtidR923B_);
  ffestd_stmt_append_ (stmt);
  ffestd_subr_line_save_ (stmt);
  stmt->u.R923B.pool = ffesta_output_pool;
  stmt->u.R923B.params = ffestd_subr_copy_inquire_ ();
  stmt->u.R923B.list = NULL;
  ffestd_expr_list_ = &stmt->u.R923B.list;
  ffesta_set_outpooldisp (FFESTA_pooldispPRESERVE);
}

/* ffestd_R923B_item -- INQUIRE statement i/o item

   ffestd_R923B_item(expr,expr_token);

   Implement output-list expression.  */

void
ffestd_R923B_item (ffebld expr)
{
  ffestdExprItem_ item;

  ffestd_check_item_ ();

  item = (ffestdExprItem_) malloc_new_kp (ffesta_output_pool,
					  "ffestdExprItem_", sizeof (*item));

  item->next = NULL;
  item->expr = expr;
  *ffestd_expr_list_ = item;
  ffestd_expr_list_ = &item->next;
}

/* ffestd_R923B_finish -- INQUIRE statement list complete

   ffestd_R923B_finish();

   Just wrap up any local activities.  */

void
ffestd_R923B_finish ()
{
  ffestd_check_finish_ ();
}

/* ffestd_R1001 -- FORMAT statement

   ffestd_R1001(format_list);  */

void
ffestd_R1001 (ffesttFormatList f)
{
  ffestsHolder str;
  ffests s = &str;
  ffestdStmt_ stmt;

  ffestd_check_simple_ ();

  if (ffestd_label_formatdef_ == NULL)
    return;			/* Nothing to hook it up to (no label def). */

  ffests_new (s, malloc_pool_image (), 80);
  ffests_putc (s, '(');
  ffestd_R1001dump_ (s, f);	/* Build the string in s. */
  ffests_putc (s, ')');

  stmt = ffestd_stmt_new_ (FFESTD_stmtidR1001_);
  ffestd_stmt_append_ (stmt);
  stmt->u.R1001.str = str;

  ffestd_label_formatdef_ = NULL;
}

/* ffestd_R1001dump_ -- Dump list of formats

   ffesttFormatList list;
   ffestd_R1001dump_(list,0);

   The formats in the list are dumped.	*/

static void
ffestd_R1001dump_ (ffests s, ffesttFormatList list)
{
  ffesttFormatList next;

  for (next = list->next; next != list; next = next->next)
    {
      if (next != list->next)
	ffests_putc (s, ',');
      switch (next->type)
	{
	case FFESTP_formattypeI:
	  ffestd_R1001dump_1005_3_ (s, next, "I");
	  break;

	case FFESTP_formattypeB:
	  ffestd_R1001error_ (next);
	  break;

	case FFESTP_formattypeO:
	  ffestd_R1001dump_1005_3_ (s, next, "O");
	  break;

	case FFESTP_formattypeZ:
	  ffestd_R1001dump_1005_3_ (s, next, "Z");
	  break;

	case FFESTP_formattypeF:
	  ffestd_R1001dump_1005_4_ (s, next, "F");
	  break;

	case FFESTP_formattypeE:
	  ffestd_R1001dump_1005_5_ (s, next, "E");
	  break;

	case FFESTP_formattypeEN:
	  ffestd_R1001error_ (next);
	  break;

	case FFESTP_formattypeG:
	  ffestd_R1001dump_1005_5_ (s, next, "G");
	  break;

	case FFESTP_formattypeL:
	  ffestd_R1001dump_1005_2_ (s, next, "L");
	  break;

	case FFESTP_formattypeA:
	  ffestd_R1001dump_1005_1_ (s, next, "A");
	  break;

	case FFESTP_formattypeD:
	  ffestd_R1001dump_1005_4_ (s, next, "D");
	  break;

	case FFESTP_formattypeQ:
	  ffestd_R1001error_ (next);
	  break;

	case FFESTP_formattypeDOLLAR:
	  ffestd_R1001dump_1010_1_ (s, next, "$");
	  break;

	case FFESTP_formattypeP:
	  ffestd_R1001dump_1010_4_ (s, next, "P");
	  break;

	case FFESTP_formattypeT:
	  ffestd_R1001dump_1010_5_ (s, next, "T");
	  break;

	case FFESTP_formattypeTL:
	  ffestd_R1001dump_1010_5_ (s, next, "TL");
	  break;

	case FFESTP_formattypeTR:
	  ffestd_R1001dump_1010_5_ (s, next, "TR");
	  break;

	case FFESTP_formattypeX:
	  ffestd_R1001dump_1010_3_ (s, next, "X");
	  break;

	case FFESTP_formattypeS:
	  ffestd_R1001dump_1010_1_ (s, next, "S");
	  break;

	case FFESTP_formattypeSP:
	  ffestd_R1001dump_1010_1_ (s, next, "SP");
	  break;

	case FFESTP_formattypeSS:
	  ffestd_R1001dump_1010_1_ (s, next, "SS");
	  break;

	case FFESTP_formattypeBN:
	  ffestd_R1001dump_1010_1_ (s, next, "BN");
	  break;

	case FFESTP_formattypeBZ:
	  ffestd_R1001dump_1010_1_ (s, next, "BZ");
	  break;

	case FFESTP_formattypeSLASH:
	  ffestd_R1001dump_1010_2_ (s, next, "/");
	  break;

	case FFESTP_formattypeCOLON:
	  ffestd_R1001dump_1010_1_ (s, next, ":");
	  break;

	case FFESTP_formattypeR1016:
	  switch (ffelex_token_type (next->t))
	    {
	    case FFELEX_typeCHARACTER:
	      {
		char *p = ffelex_token_text (next->t);
		ffeTokenLength i = ffelex_token_length (next->t);

		ffests_putc (s, '\002');
		while (i-- != 0)
		  {
		    if (*p == '\002')
		      ffests_putc (s, '\002');
		    ffests_putc (s, *p);
		    ++p;
		  }
		ffests_putc (s, '\002');
	      }
	      break;

	    case FFELEX_typeHOLLERITH:
	      {
		char *p = ffelex_token_text (next->t);
		ffeTokenLength i = ffelex_token_length (next->t);

		ffests_printf (s, "%" ffeTokenLength_f "uH", i);
		while (i-- != 0)
		  {
		    ffests_putc (s, *p);
		    ++p;
		  }
	      }
	      break;

	    default:
	      assert (FALSE);
	    }
	  break;

	case FFESTP_formattypeFORMAT:
	  if (next->u.R1003D.R1004.present)
	    {
	      if (next->u.R1003D.R1004.rtexpr)
		ffestd_R1001rtexpr_ (s, next, next->u.R1003D.R1004.u.expr);
	      else
		ffests_printf (s, "%lu", next->u.R1003D.R1004.u.unsigned_val);
	    }

	  ffests_putc (s, '(');
	  ffestd_R1001dump_ (s, next->u.R1003D.format);
	  ffests_putc (s, ')');
	  break;

	default:
	  assert (FALSE);
	}
    }
}

/* ffestd_R1001dump_1005_1_ -- Dump a particular format

   ffesttFormatList f;
   ffestd_R1001dump_1005_1_(f,"I");

   The format is dumped with form [r]X[w].  */

static void
ffestd_R1001dump_1005_1_ (ffests s, ffesttFormatList f, const char *string)
{
  assert (!f->u.R1005.R1007_or_R1008.present);
  assert (!f->u.R1005.R1009.present);

  if (f->u.R1005.R1004.present)
    {
      if (f->u.R1005.R1004.rtexpr)
	ffestd_R1001rtexpr_ (s, f, f->u.R1005.R1004.u.expr);
      else
	ffests_printf (s, "%lu", f->u.R1005.R1004.u.unsigned_val);
    }

  ffests_puts (s, string);

  if (f->u.R1005.R1006.present)
    {
      if (f->u.R1005.R1006.rtexpr)
	ffestd_R1001rtexpr_ (s, f, f->u.R1005.R1006.u.expr);
      else
	ffests_printf (s, "%lu", f->u.R1005.R1006.u.unsigned_val);
    }
}

/* ffestd_R1001dump_1005_2_ -- Dump a particular format

   ffesttFormatList f;
   ffestd_R1001dump_1005_2_(f,"I");

   The format is dumped with form [r]Xw.  */

static void
ffestd_R1001dump_1005_2_ (ffests s, ffesttFormatList f, const char *string)
{
  assert (!f->u.R1005.R1007_or_R1008.present);
  assert (!f->u.R1005.R1009.present);
  assert (f->u.R1005.R1006.present);

  if (f->u.R1005.R1004.present)
    {
      if (f->u.R1005.R1004.rtexpr)
	ffestd_R1001rtexpr_ (s, f, f->u.R1005.R1004.u.expr);
      else
	ffests_printf (s, "%lu", f->u.R1005.R1004.u.unsigned_val);
    }

  ffests_puts (s, string);

  if (f->u.R1005.R1006.rtexpr)
    ffestd_R1001rtexpr_ (s, f, f->u.R1005.R1006.u.expr);
  else
    ffests_printf (s, "%lu", f->u.R1005.R1006.u.unsigned_val);
}

/* ffestd_R1001dump_1005_3_ -- Dump a particular format

   ffesttFormatList f;
   ffestd_R1001dump_1005_3_(f,"I");

   The format is dumped with form [r]Xw[.m].  */

static void
ffestd_R1001dump_1005_3_ (ffests s, ffesttFormatList f, const char *string)
{
  assert (!f->u.R1005.R1009.present);
  assert (f->u.R1005.R1006.present);

  if (f->u.R1005.R1004.present)
    {
      if (f->u.R1005.R1004.rtexpr)
	ffestd_R1001rtexpr_ (s, f, f->u.R1005.R1004.u.expr);
      else
	ffests_printf (s, "%lu", f->u.R1005.R1004.u.unsigned_val);
    }

  ffests_puts (s, string);

  if (f->u.R1005.R1006.rtexpr)
    ffestd_R1001rtexpr_ (s, f, f->u.R1005.R1006.u.expr);
  else
    ffests_printf (s, "%lu", f->u.R1005.R1006.u.unsigned_val);

  if (f->u.R1005.R1007_or_R1008.present)
    {
      ffests_putc (s, '.');
      if (f->u.R1005.R1007_or_R1008.rtexpr)
	ffestd_R1001rtexpr_ (s, f, f->u.R1005.R1007_or_R1008.u.expr);
      else
	ffests_printf (s, "%lu", f->u.R1005.R1007_or_R1008.u.unsigned_val);
    }
}

/* ffestd_R1001dump_1005_4_ -- Dump a particular format

   ffesttFormatList f;
   ffestd_R1001dump_1005_4_(f,"I");

   The format is dumped with form [r]Xw.d.  */

static void
ffestd_R1001dump_1005_4_ (ffests s, ffesttFormatList f, const char *string)
{
  assert (!f->u.R1005.R1009.present);
  assert (f->u.R1005.R1007_or_R1008.present);
  assert (f->u.R1005.R1006.present);

  if (f->u.R1005.R1004.present)
    {
      if (f->u.R1005.R1004.rtexpr)
	ffestd_R1001rtexpr_ (s, f, f->u.R1005.R1004.u.expr);
      else
	ffests_printf (s, "%lu", f->u.R1005.R1004.u.unsigned_val);
    }

  ffests_puts (s, string);

  if (f->u.R1005.R1006.rtexpr)
    ffestd_R1001rtexpr_ (s, f, f->u.R1005.R1006.u.expr);
  else
    ffests_printf (s, "%lu", f->u.R1005.R1006.u.unsigned_val);

  ffests_putc (s, '.');
  if (f->u.R1005.R1007_or_R1008.rtexpr)
    ffestd_R1001rtexpr_ (s, f, f->u.R1005.R1007_or_R1008.u.expr);
  else
    ffests_printf (s, "%lu", f->u.R1005.R1007_or_R1008.u.unsigned_val);
}

/* ffestd_R1001dump_1005_5_ -- Dump a particular format

   ffesttFormatList f;
   ffestd_R1001dump_1005_5_(f,"I");

   The format is dumped with form [r]Xw.d[Ee].	*/

static void
ffestd_R1001dump_1005_5_ (ffests s, ffesttFormatList f, const char *string)
{
  assert (f->u.R1005.R1007_or_R1008.present);
  assert (f->u.R1005.R1006.present);

  if (f->u.R1005.R1004.present)
    {
      if (f->u.R1005.R1004.rtexpr)
	ffestd_R1001rtexpr_ (s, f, f->u.R1005.R1004.u.expr);
      else
	ffests_printf (s, "%lu", f->u.R1005.R1004.u.unsigned_val);
    }

  ffests_puts (s, string);

  if (f->u.R1005.R1006.rtexpr)
    ffestd_R1001rtexpr_ (s, f, f->u.R1005.R1006.u.expr);
  else
    ffests_printf (s, "%lu", f->u.R1005.R1006.u.unsigned_val);

  ffests_putc (s, '.');
  if (f->u.R1005.R1007_or_R1008.rtexpr)
    ffestd_R1001rtexpr_ (s, f, f->u.R1005.R1007_or_R1008.u.expr);
  else
    ffests_printf (s, "%lu", f->u.R1005.R1007_or_R1008.u.unsigned_val);

  if (f->u.R1005.R1009.present)
    {
      ffests_putc (s, 'E');
      if (f->u.R1005.R1009.rtexpr)
	ffestd_R1001rtexpr_ (s, f, f->u.R1005.R1009.u.expr);
      else
	ffests_printf (s, "%lu", f->u.R1005.R1009.u.unsigned_val);
    }
}

/* ffestd_R1001dump_1010_1_ -- Dump a particular format

   ffesttFormatList f;
   ffestd_R1001dump_1010_1_(f,"I");

   The format is dumped with form X.  */

static void
ffestd_R1001dump_1010_1_ (ffests s, ffesttFormatList f, const char *string)
{
  assert (!f->u.R1010.val.present);

  ffests_puts (s, string);
}

/* ffestd_R1001dump_1010_2_ -- Dump a particular format

   ffesttFormatList f;
   ffestd_R1001dump_1010_2_(f,"I");

   The format is dumped with form [r]X.	 */

static void
ffestd_R1001dump_1010_2_ (ffests s, ffesttFormatList f, const char *string)
{
  if (f->u.R1010.val.present)
    {
      if (f->u.R1010.val.rtexpr)
	ffestd_R1001rtexpr_ (s, f, f->u.R1010.val.u.expr);
      else
	ffests_printf (s, "%lu", f->u.R1010.val.u.unsigned_val);
    }

  ffests_puts (s, string);
}

/* ffestd_R1001dump_1010_3_ -- Dump a particular format

   ffesttFormatList f;
   ffestd_R1001dump_1010_3_(f,"I");

   The format is dumped with form nX.  */

static void
ffestd_R1001dump_1010_3_ (ffests s, ffesttFormatList f, const char *string)
{
  assert (f->u.R1010.val.present);

  if (f->u.R1010.val.rtexpr)
    ffestd_R1001rtexpr_ (s, f, f->u.R1010.val.u.expr);
  else
    ffests_printf (s, "%lu", f->u.R1010.val.u.unsigned_val);

  ffests_puts (s, string);
}

/* ffestd_R1001dump_1010_4_ -- Dump a particular format

   ffesttFormatList f;
   ffestd_R1001dump_1010_4_(f,"I");

   The format is dumped with form kX.  Note that k is signed.  */

static void
ffestd_R1001dump_1010_4_ (ffests s, ffesttFormatList f, const char *string)
{
  assert (f->u.R1010.val.present);

  if (f->u.R1010.val.rtexpr)
    ffestd_R1001rtexpr_ (s, f, f->u.R1010.val.u.expr);
  else
    ffests_printf (s, "%ld", f->u.R1010.val.u.signed_val);

  ffests_puts (s, string);
}

/* ffestd_R1001dump_1010_5_ -- Dump a particular format

   ffesttFormatList f;
   ffestd_R1001dump_1010_5_(f,"I");

   The format is dumped with form Xn.  */

static void
ffestd_R1001dump_1010_5_ (ffests s, ffesttFormatList f, const char *string)
{
  assert (f->u.R1010.val.present);

  ffests_puts (s, string);

  if (f->u.R1010.val.rtexpr)
    ffestd_R1001rtexpr_ (s, f, f->u.R1010.val.u.expr);
  else
    ffests_printf (s, "%lu", f->u.R1010.val.u.unsigned_val);
}

/* ffestd_R1001error_ -- Complain about FORMAT specification not supported

   ffesttFormatList f;
   ffestd_R1001error_(f);

   An error message is produced.  */

static void
ffestd_R1001error_ (ffesttFormatList f)
{
  ffebad_start (FFEBAD_FORMAT_UNSUPPORTED);
  ffebad_here (0, ffelex_token_where_line (f->t), ffelex_token_where_column (f->t));
  ffebad_finish ();
}

static void
ffestd_R1001rtexpr_ (ffests s, ffesttFormatList f, ffebld expr)
{
  if ((expr == NULL)
      || (ffebld_op (expr) != FFEBLD_opCONTER)
      || (ffeinfo_basictype (ffebld_info (expr)) != FFEINFO_basictypeINTEGER)
      || (ffeinfo_kindtype (ffebld_info (expr)) == FFEINFO_kindtypeINTEGER4))
    {
      ffebad_start (FFEBAD_FORMAT_VARIABLE);
      ffebad_here (0, ffelex_token_where_line (f->t), ffelex_token_where_column (f->t));
      ffebad_finish ();
    }
  else
    {
      int val;

      switch (ffeinfo_kindtype (ffebld_info (expr)))
	{
#if FFETARGET_okINTEGER1
	case FFEINFO_kindtypeINTEGER1:
	  val = ffebld_constant_integer1 (ffebld_conter (expr));
	  break;
#endif

#if FFETARGET_okINTEGER2
	case FFEINFO_kindtypeINTEGER2:
	  val = ffebld_constant_integer2 (ffebld_conter (expr));
	  break;
#endif

#if FFETARGET_okINTEGER3
	case FFEINFO_kindtypeINTEGER3:
	  val = ffebld_constant_integer3 (ffebld_conter (expr));
	  break;
#endif

	default:
	  assert ("bad INTEGER constant kind type" == NULL);
	  /* Fall through. */
	case FFEINFO_kindtypeANY:
	  return;
	}
      ffests_printf (s, "%ld", (long) val);
    }
}

/* ffestd_R1102 -- PROGRAM statement

   ffestd_R1102(name_token);

   Make sure ffestd_kind_ identifies an empty block.  Make sure name_token
   gives a valid name.	Implement the beginning of a main program.  */

void
ffestd_R1102 (ffesymbol s, ffelexToken name UNUSED)
{
  ffestd_check_simple_ ();

  assert (ffestd_block_level_ == 0);
  ffestd_is_reachable_ = TRUE;

  ffecom_notify_primary_entry (s);
  ffe_set_is_mainprog (TRUE);	/* Is a main program. */
  ffe_set_is_saveall (TRUE);	/* Main program always has implicit SAVE. */

  ffestw_set_sym (ffestw_stack_top (), s);
}

/* ffestd_R1103 -- End a PROGRAM

   ffestd_R1103();  */

void
ffestd_R1103 (bool ok UNUSED)
{
  ffestdStmt_ stmt;

  assert (ffestd_block_level_ == 0);

  if (FFESTD_IS_END_OPTIMIZED_ && ffestd_is_reachable_)
    ffestd_R842 (NULL);		/* Generate STOP. */

  if (ffestw_state (ffestw_stack_top ()) != FFESTV_statePROGRAM5)
    ffestd_subr_labels_ (FALSE);/* Handle any undefined labels. */

  stmt = ffestd_stmt_new_ (FFESTD_stmtidR1103_);
  ffestd_stmt_append_ (stmt);
}

/* ffestd_R1105 -- MODULE statement

   ffestd_R1105(name_token);

   Make sure ffestd_kind_ identifies an empty block.  Make sure name_token
   gives a valid name.	Implement the beginning of a module.  */

#if FFESTR_F90
void
ffestd_R1105 (ffelexToken name)
{
  assert (ffestd_block_level_ == 0);

  ffestd_check_simple_ ();

  ffestd_subr_f90_ ();
  return;

#ifdef FFESTD_F90
  fprintf (dmpout, "* MODULE %s\n", ffelex_token_text (name));
#endif
}

/* ffestd_R1106 -- End a MODULE

   ffestd_R1106(TRUE);	*/

void
ffestd_R1106 (bool ok)
{
  assert (ffestd_block_level_ == 0);

  /* Generate any wrap-up code here (unlikely in MODULE!). */

  if (ffestw_state (ffestw_stack_top ()) != FFESTV_stateMODULE5)
    ffestd_subr_labels_ (TRUE);	/* Handle any undefined labels (unlikely). */

  return;			/* F90. */

#ifdef FFESTD_F90
  fprintf (dmpout, "< END_MODULE %s\n",
	   ffelex_token_text (ffestw_name (ffestw_stack_top ())));
#endif
}

/* ffestd_R1107_start -- USE statement list begin

   ffestd_R1107_start();

   Verify that USE is valid here, and begin accepting items in the list.  */

void
ffestd_R1107_start (ffelexToken name, bool only)
{
  ffestd_check_start_ ();

  ffestd_subr_f90_ ();
  return;

#ifdef FFESTD_F90
  fprintf (dmpout, "* USE %s,", ffelex_token_text (name));	/* NB
								   _shriek_begin_uses_. */
  if (only)
    fputs ("only: ", dmpout);
#endif
}

/* ffestd_R1107_item -- USE statement for name

   ffestd_R1107_item(local_token,use_token);

   Make sure name_token identifies a valid object to be USEed.	local_token
   may be NULL if _start_ was called with only==TRUE.  */

void
ffestd_R1107_item (ffelexToken local, ffelexToken use)
{
  ffestd_check_item_ ();
  assert (use != NULL);

  return;			/* F90. */

#ifdef FFESTD_F90
  if (local != NULL)
    fprintf (dmpout, "%s=>", ffelex_token_text (local));
  fprintf (dmpout, "%s,", ffelex_token_text (use));
#endif
}

/* ffestd_R1107_finish -- USE statement list complete

   ffestd_R1107_finish();

   Just wrap up any local activities.  */

void
ffestd_R1107_finish ()
{
  ffestd_check_finish_ ();

  return;			/* F90. */

#ifdef FFESTD_F90
  fputc ('\n', dmpout);
#endif
}

#endif
/* ffestd_R1111 -- BLOCK DATA statement

   ffestd_R1111(name_token);

   Make sure ffestd_kind_ identifies no current program unit.  If not
   NULL, make sure name_token gives a valid name.  Implement the beginning
   of a block data program unit.  */

void
ffestd_R1111 (ffesymbol s, ffelexToken name UNUSED)
{
  assert (ffestd_block_level_ == 0);
  ffestd_is_reachable_ = TRUE;

  ffestd_check_simple_ ();

  ffecom_notify_primary_entry (s);
  ffestw_set_sym (ffestw_stack_top (), s);
}

/* ffestd_R1112 -- End a BLOCK DATA

   ffestd_R1112(TRUE);	*/

void
ffestd_R1112 (bool ok UNUSED)
{
  ffestdStmt_ stmt;

  assert (ffestd_block_level_ == 0);

  /* Generate any return-like code here (not likely for BLOCK DATA!). */

  if (ffestw_state (ffestw_stack_top ()) != FFESTV_stateBLOCKDATA5)
    ffestd_subr_labels_ (TRUE);	/* Handle any undefined labels. */

  stmt = ffestd_stmt_new_ (FFESTD_stmtidR1112_);
  ffestd_stmt_append_ (stmt);
}

/* ffestd_R1202 -- INTERFACE statement

   ffestd_R1202(operator,defined_name);

   Make sure ffestd_kind_ identifies an INTERFACE block.
   Implement the end of the current interface.

   06-Jun-90  JCB  1.1
      Allow no operator or name to mean INTERFACE by itself; missed this
      valid form when originally doing syntactic analysis code.	 */

#if FFESTR_F90
void
ffestd_R1202 (ffestpDefinedOperator operator, ffelexToken name)
{
  ffestd_check_simple_ ();

  ffestd_subr_f90_ ();
  return;

#ifdef FFESTD_F90
  switch (operator)
    {
    case FFESTP_definedoperatorNone:
      if (name == NULL)
	fputs ("* INTERFACE_unnamed\n", dmpout);
      else
	fprintf (dmpout, "* INTERFACE %s\n", ffelex_token_text (name));
      break;

    case FFESTP_definedoperatorOPERATOR:
      fprintf (dmpout, "* INTERFACE_OPERATOR (.%s.)\n", ffelex_token_text (name));
      break;

    case FFESTP_definedoperatorASSIGNMENT:
      fputs ("* INTERFACE_ASSIGNMENT (=)\n", dmpout);
      break;

    case FFESTP_definedoperatorPOWER:
      fputs ("* INTERFACE_OPERATOR (**)\n", dmpout);
      break;

    case FFESTP_definedoperatorMULT:
      fputs ("* INTERFACE_OPERATOR (*)\n", dmpout);
      break;

    case FFESTP_definedoperatorADD:
      fputs ("* INTERFACE_OPERATOR (+)\n", dmpout);
      break;

    case FFESTP_definedoperatorCONCAT:
      fputs ("* INTERFACE_OPERATOR (//)\n", dmpout);
      break;

    case FFESTP_definedoperatorDIVIDE:
      fputs ("* INTERFACE_OPERATOR (/)\n", dmpout);
      break;

    case FFESTP_definedoperatorSUBTRACT:
      fputs ("* INTERFACE_OPERATOR (-)\n", dmpout);
      break;

    case FFESTP_definedoperatorNOT:
      fputs ("* INTERFACE_OPERATOR (.not.)\n", dmpout);
      break;

    case FFESTP_definedoperatorAND:
      fputs ("* INTERFACE_OPERATOR (.and.)\n", dmpout);
      break;

    case FFESTP_definedoperatorOR:
      fputs ("* INTERFACE_OPERATOR (.or.)\n", dmpout);
      break;

    case FFESTP_definedoperatorEQV:
      fputs ("* INTERFACE_OPERATOR (.eqv.)\n", dmpout);
      break;

    case FFESTP_definedoperatorNEQV:
      fputs ("* INTERFACE_OPERATOR (.neqv.)\n", dmpout);
      break;

    case FFESTP_definedoperatorEQ:
      fputs ("* INTERFACE_OPERATOR (==)\n", dmpout);
      break;

    case FFESTP_definedoperatorNE:
      fputs ("* INTERFACE_OPERATOR (/=)\n", dmpout);
      break;

    case FFESTP_definedoperatorLT:
      fputs ("* INTERFACE_OPERATOR (<)\n", dmpout);
      break;

    case FFESTP_definedoperatorLE:
      fputs ("* INTERFACE_OPERATOR (<=)\n", dmpout);
      break;

    case FFESTP_definedoperatorGT:
      fputs ("* INTERFACE_OPERATOR (>)\n", dmpout);
      break;

    case FFESTP_definedoperatorGE:
      fputs ("* INTERFACE_OPERATOR (>=)\n", dmpout);
      break;

    default:
      assert (FALSE);
      break;
    }
#endif
}

/* ffestd_R1203 -- End an INTERFACE

   ffestd_R1203(TRUE);	*/

void
ffestd_R1203 (bool ok)
{
  return;			/* F90. */

#ifdef FFESTD_F90
  fputs ("* END_INTERFACE\n", dmpout);
#endif
}

/* ffestd_R1205_start -- MODULE PROCEDURE statement list begin

   ffestd_R1205_start();

   Verify that MODULE PROCEDURE is valid here, and begin accepting items in
   the list.  */

void
ffestd_R1205_start ()
{
  ffestd_check_start_ ();

  return;			/* F90. */

#ifdef FFESTD_F90
  fputs ("* MODULE_PROCEDURE ", dmpout);
#endif
}

/* ffestd_R1205_item -- MODULE PROCEDURE statement for name

   ffestd_R1205_item(name_token);

   Make sure name_token identifies a valid object to be MODULE PROCEDUREed.  */

void
ffestd_R1205_item (ffelexToken name)
{
  ffestd_check_item_ ();
  assert (name != NULL);

  return;			/* F90. */

#ifdef FFESTD_F90
  fprintf (dmpout, "%s,", ffelex_token_text (name));
#endif
}

/* ffestd_R1205_finish -- MODULE PROCEDURE statement list complete

   ffestd_R1205_finish();

   Just wrap up any local activities.  */

void
ffestd_R1205_finish ()
{
  ffestd_check_finish_ ();

  return;			/* F90. */

#ifdef FFESTD_F90
  fputc ('\n', dmpout);
#endif
}

#endif
/* ffestd_R1207_start -- EXTERNAL statement list begin

   ffestd_R1207_start();

   Verify that EXTERNAL is valid here, and begin accepting items in the list.  */

void
ffestd_R1207_start ()
{
  ffestd_check_start_ ();
}

/* ffestd_R1207_item -- EXTERNAL statement for name

   ffestd_R1207_item(name_token);

   Make sure name_token identifies a valid object to be EXTERNALd.  */

void
ffestd_R1207_item (ffelexToken name)
{
  ffestd_check_item_ ();
  assert (name != NULL);
}

/* ffestd_R1207_finish -- EXTERNAL statement list complete

   ffestd_R1207_finish();

   Just wrap up any local activities.  */

void
ffestd_R1207_finish ()
{
  ffestd_check_finish_ ();
}

/* ffestd_R1208_start -- INTRINSIC statement list begin

   ffestd_R1208_start();

   Verify that INTRINSIC is valid here, and begin accepting items in the list.	*/

void
ffestd_R1208_start ()
{
  ffestd_check_start_ ();
}

/* ffestd_R1208_item -- INTRINSIC statement for name

   ffestd_R1208_item(name_token);

   Make sure name_token identifies a valid object to be INTRINSICd.  */

void
ffestd_R1208_item (ffelexToken name)
{
  ffestd_check_item_ ();
  assert (name != NULL);
}

/* ffestd_R1208_finish -- INTRINSIC statement list complete

   ffestd_R1208_finish();

   Just wrap up any local activities.  */

void
ffestd_R1208_finish ()
{
  ffestd_check_finish_ ();
}

/* ffestd_R1212 -- CALL statement

   ffestd_R1212(expr,expr_token);

   Make sure statement is valid here; implement.  */

void
ffestd_R1212 (ffebld expr)
{
  ffestdStmt_ stmt;

  ffestd_check_simple_ ();

  stmt = ffestd_stmt_new_ (FFESTD_stmtidR1212_);
  ffestd_stmt_append_ (stmt);
  ffestd_subr_line_save_ (stmt);
  stmt->u.R1212.pool = ffesta_output_pool;
  stmt->u.R1212.expr = expr;
  ffesta_set_outpooldisp (FFESTA_pooldispPRESERVE);
}

/* ffestd_R1213 -- Defined assignment statement

   ffestd_R1213(dest_expr,source_expr,source_token);

   Make sure the assignment is valid.  */

#if FFESTR_F90
void
ffestd_R1213 (ffebld dest, ffebld source)
{
  ffestd_check_simple_ ();

  ffestd_subr_f90_ ();
}

#endif
/* ffestd_R1219 -- FUNCTION statement

   ffestd_R1219(funcname,arglist,ending_token,kind,kindt,len,lent,
	 recursive);

   Make sure statement is valid here, register arguments for the
   function name, and so on.

   06-Jun-90  JCB  2.0
      Added the kind, len, and recursive arguments.  */

void
ffestd_R1219 (ffesymbol s, ffelexToken funcname UNUSED,
	      ffesttTokenList args UNUSED, ffestpType type UNUSED,
	      ffebld kind UNUSED, ffelexToken kindt UNUSED,
	      ffebld len UNUSED, ffelexToken lent UNUSED,
	      bool recursive UNUSED, ffelexToken result UNUSED,
	      bool separate_result UNUSED)
{
  assert (ffestd_block_level_ == 0);
  ffestd_is_reachable_ = TRUE;

  ffestd_check_simple_ ();

  ffecom_notify_primary_entry (s);
  ffestw_set_sym (ffestw_stack_top (), s);
}

/* ffestd_R1221 -- End a FUNCTION

   ffestd_R1221(TRUE);	*/

void
ffestd_R1221 (bool ok UNUSED)
{
  ffestdStmt_ stmt;

  assert (ffestd_block_level_ == 0);

  if (FFESTD_IS_END_OPTIMIZED_ && ffestd_is_reachable_)
    ffestd_R1227 (NULL);	/* Generate RETURN. */

  if (ffestw_state (ffestw_stack_top ()) != FFESTV_stateFUNCTION5)
    ffestd_subr_labels_ (FALSE);/* Handle any undefined labels. */

  stmt = ffestd_stmt_new_ (FFESTD_stmtidR1221_);
  ffestd_stmt_append_ (stmt);
}

/* ffestd_R1223 -- SUBROUTINE statement

   ffestd_R1223(subrname,arglist,ending_token,recursive_token);

   Make sure statement is valid here, register arguments for the
   subroutine name, and so on.

   06-Jun-90  JCB  2.0
      Added the recursive argument.  */

void
ffestd_R1223 (ffesymbol s, ffelexToken subrname UNUSED,
	      ffesttTokenList args UNUSED, ffelexToken final UNUSED,
	      bool recursive UNUSED)
{
  assert (ffestd_block_level_ == 0);
  ffestd_is_reachable_ = TRUE;

  ffestd_check_simple_ ();

  ffecom_notify_primary_entry (s);
  ffestw_set_sym (ffestw_stack_top (), s);
}

/* ffestd_R1225 -- End a SUBROUTINE

   ffestd_R1225(TRUE);	*/

void
ffestd_R1225 (bool ok UNUSED)
{
  ffestdStmt_ stmt;

  assert (ffestd_block_level_ == 0);

  if (FFESTD_IS_END_OPTIMIZED_ && ffestd_is_reachable_)
    ffestd_R1227 (NULL);	/* Generate RETURN. */

  if (ffestw_state (ffestw_stack_top ()) != FFESTV_stateSUBROUTINE5)
    ffestd_subr_labels_ (FALSE);/* Handle any undefined labels. */

  stmt = ffestd_stmt_new_ (FFESTD_stmtidR1225_);
  ffestd_stmt_append_ (stmt);
}

/* ffestd_R1226 -- ENTRY statement

   ffestd_R1226(entryname,arglist,ending_token);

   Make sure we're in a SUBROUTINE or FUNCTION, register arguments for the
   entry point name, and so on.	 */

void
ffestd_R1226 (ffesymbol entry)
{
  ffestd_check_simple_ ();

  if (!ffesta_seen_first_exec || ffecom_2pass_advise_entrypoint (entry))
    {
      ffestdStmt_ stmt;

      stmt = ffestd_stmt_new_ (FFESTD_stmtidR1226_);
      ffestd_stmt_append_ (stmt);
      ffestd_subr_line_save_ (stmt);
      stmt->u.R1226.entry = entry;
      stmt->u.R1226.entrynum = ++ffestd_2pass_entrypoints_;
    }

  ffestd_is_reachable_ = TRUE;
}

/* ffestd_R1227 -- RETURN statement

   ffestd_R1227(expr);

   Make sure statement is valid here; implement.  expr and expr_token are
   both NULL if there was no expression.  */

void
ffestd_R1227 (ffebld expr)
{
  ffestdStmt_ stmt;

  ffestd_check_simple_ ();

  stmt = ffestd_stmt_new_ (FFESTD_stmtidR1227_);
  ffestd_stmt_append_ (stmt);
  ffestd_subr_line_save_ (stmt);
  stmt->u.R1227.pool = ffesta_output_pool;
  stmt->u.R1227.block = ffestw_stack_top ();
  stmt->u.R1227.expr = expr;
  ffesta_set_outpooldisp (FFESTA_pooldispPRESERVE);

  if (ffestd_block_level_ == 0)
    ffestd_is_reachable_ = FALSE;
}

/* ffestd_R1228 -- CONTAINS statement

   ffestd_R1228();  */

#if FFESTR_F90
void
ffestd_R1228 ()
{
  assert (ffestd_block_level_ == 0);

  ffestd_check_simple_ ();

  /* Generate RETURN/STOP code here */

  ffestd_subr_labels_ (ffestw_state (ffestw_stack_top ())
		       == FFESTV_stateMODULE5);	/* Handle any undefined
						   labels. */

  ffestd_subr_f90_ ();
  return;

#ifdef FFESTD_F90
  fputs ("- CONTAINS\n", dmpout);
#endif
}

#endif
/* ffestd_R1229_start -- STMTFUNCTION statement begin

   ffestd_R1229_start(func_name,func_arg_list,close_paren);

   This function does not really need to do anything, since _finish_
   gets all the info needed, and ffestc_R1229_start has already
   done all the stuff that makes a two-phase operation (start and
   finish) for handling statement functions necessary.

   03-Jan-91  JCB  2.0
      Do nothing, now that _finish_ does everything.  */

void
ffestd_R1229_start (ffelexToken name UNUSED, ffesttTokenList args UNUSED)
{
  ffestd_check_start_ ();
}

/* ffestd_R1229_finish -- STMTFUNCTION statement list complete

   ffestd_R1229_finish(s);

   The statement function's symbol is passed.  Its list of dummy args is
   accessed via ffesymbol_dummyargs and its expansion expression (expr)
   is accessed via ffesymbol_sfexpr.

   If sfexpr is NULL, an error occurred parsing the expansion expression, so
   just cancel the effects of ffestd_R1229_start and pretend nothing
   happened.  Otherwise, install the expression as the expansion for the
   statement function, then clean up.

   03-Jan-91  JCB  2.0
      Takes sfunc sym instead of just the expansion expression as an
      argument, so this function can do all the work, and _start_ is just
      a nicety than can do nothing in a back end.  */

void
ffestd_R1229_finish (ffesymbol s)
{
  ffebld expr = ffesymbol_sfexpr (s);

  ffestd_check_finish_ ();

  if (expr == NULL)
    return;			/* Nothing to do, definition didn't work. */

  /* With gcc, cannot do anything here, because the backend hasn't even
     (necessarily) been notified that we're compiling a program unit! */
  ffesta_set_outpooldisp (FFESTA_pooldispPRESERVE);
}

/* ffestd_S3P4 -- INCLUDE line

   ffestd_S3P4(filename,filename_token);

   Make sure INCLUDE not preceded by any semicolons or a label def; implement.	*/

void
ffestd_S3P4 (ffebld filename)
{
  FILE *fi;
  ffetargetCharacterDefault buildname;
  ffewhereFile wf;

  ffestd_check_simple_ ();

  assert (filename != NULL);
  if (ffebld_op (filename) != FFEBLD_opANY)
    {
      assert (ffebld_op (filename) == FFEBLD_opCONTER);
      assert (ffeinfo_basictype (ffebld_info (filename))
	      == FFEINFO_basictypeCHARACTER);
      assert (ffeinfo_kindtype (ffebld_info (filename))
	      == FFEINFO_kindtypeCHARACTERDEFAULT);
      buildname = ffebld_constant_characterdefault (ffebld_conter (filename));
      wf = ffewhere_file_new (ffetarget_text_characterdefault (buildname),
			      ffetarget_length_characterdefault (buildname));
      fi = ffecom_open_include (ffewhere_file_name (wf),
				ffelex_token_where_line (ffesta_tokens[0]),
				ffelex_token_where_column (ffesta_tokens[0]));
      if (fi != NULL)
	ffelex_set_include (wf, (ffelex_token_type (ffesta_tokens[0])
				 == FFELEX_typeNAME), fi);
    }
}

/* ffestd_V003_start -- STRUCTURE statement list begin

   ffestd_V003_start(structure_name);

   Verify that STRUCTURE is valid here, and begin accepting items in the list.	*/

#if FFESTR_VXT
void
ffestd_V003_start (ffelexToken structure_name)
{
  ffestd_check_start_ ();
  ffestd_subr_vxt_ ();
}

/* ffestd_V003_item -- STRUCTURE statement for object-name

   ffestd_V003_item(name_token,dim_list);

   Make sure name_token identifies a valid object to be STRUCTUREd.  */

void
ffestd_V003_item (ffelexToken name, ffesttDimList dims)
{
  ffestd_check_item_ ();
}

/* ffestd_V003_finish -- STRUCTURE statement list complete

   ffestd_V003_finish();

   Just wrap up any local activities.  */

void
ffestd_V003_finish ()
{
  ffestd_check_finish_ ();
}

/* ffestd_V004 -- End a STRUCTURE

   ffestd_V004(TRUE);  */

void
ffestd_V004 (bool ok)
{
}

/* ffestd_V009 -- UNION statement

   ffestd_V009();  */

void
ffestd_V009 ()
{
  ffestd_check_simple_ ();
}

/* ffestd_V010 -- End a UNION

   ffestd_V010(TRUE);  */

void
ffestd_V010 (bool ok)
{
}

/* ffestd_V012 -- MAP statement

   ffestd_V012();  */

void
ffestd_V012 ()
{
  ffestd_check_simple_ ();
}

/* ffestd_V013 -- End a MAP

   ffestd_V013(TRUE);  */

void
ffestd_V013 (bool ok)
{
}

#endif
/* ffestd_V014_start -- VOLATILE statement list begin

   ffestd_V014_start();

   Verify that VOLATILE is valid here, and begin accepting items in the list.  */

void
ffestd_V014_start ()
{
  ffestd_check_start_ ();
}

/* ffestd_V014_item_object -- VOLATILE statement for object-name

   ffestd_V014_item_object(name_token);

   Make sure name_token identifies a valid object to be VOLATILEd.  */

void
ffestd_V014_item_object (ffelexToken name UNUSED)
{
  ffestd_check_item_ ();
}

/* ffestd_V014_item_cblock -- VOLATILE statement for common-block-name

   ffestd_V014_item_cblock(name_token);

   Make sure name_token identifies a valid common block to be VOLATILEd.  */

void
ffestd_V014_item_cblock (ffelexToken name UNUSED)
{
  ffestd_check_item_ ();
}

/* ffestd_V014_finish -- VOLATILE statement list complete

   ffestd_V014_finish();

   Just wrap up any local activities.  */

void
ffestd_V014_finish ()
{
  ffestd_check_finish_ ();
}

/* ffestd_V016_start -- RECORD statement list begin

   ffestd_V016_start();

   Verify that RECORD is valid here, and begin accepting items in the list.  */

#if FFESTR_VXT
void
ffestd_V016_start ()
{
  ffestd_check_start_ ();
}

/* ffestd_V016_item_structure -- RECORD statement for common-block-name

   ffestd_V016_item_structure(name_token);

   Make sure name_token identifies a valid structure to be RECORDed.  */

void
ffestd_V016_item_structure (ffelexToken name)
{
  ffestd_check_item_ ();
}

/* ffestd_V016_item_object -- RECORD statement for object-name

   ffestd_V016_item_object(name_token,dim_list);

   Make sure name_token identifies a valid object to be RECORDd.  */

void
ffestd_V016_item_object (ffelexToken name, ffesttDimList dims)
{
  ffestd_check_item_ ();
}

/* ffestd_V016_finish -- RECORD statement list complete

   ffestd_V016_finish();

   Just wrap up any local activities.  */

void
ffestd_V016_finish ()
{
  ffestd_check_finish_ ();
}

/* ffestd_V018_start -- REWRITE(...) statement list begin

   ffestd_V018_start();

   Verify that REWRITE is valid here, and begin accepting items in the
   list.  */

void
ffestd_V018_start (ffestvFormat format)
{
  ffestd_check_start_ ();
  ffestd_subr_vxt_ ();
}

/* ffestd_V018_item -- REWRITE statement i/o item

   ffestd_V018_item(expr,expr_token);

   Implement output-list expression.  */

void
ffestd_V018_item (ffebld expr)
{
  ffestd_check_item_ ();
}

/* ffestd_V018_finish -- REWRITE statement list complete

   ffestd_V018_finish();

   Just wrap up any local activities.  */

void
ffestd_V018_finish ()
{
  ffestd_check_finish_ ();
}

/* ffestd_V019_start -- ACCEPT statement list begin

   ffestd_V019_start();

   Verify that ACCEPT is valid here, and begin accepting items in the
   list.  */

void
ffestd_V019_start (ffestvFormat format)
{
  ffestd_check_start_ ();
  ffestd_subr_vxt_ ();
}

/* ffestd_V019_item -- ACCEPT statement i/o item

   ffestd_V019_item(expr,expr_token);

   Implement output-list expression.  */

void
ffestd_V019_item (ffebld expr)
{
  ffestd_check_item_ ();
}

/* ffestd_V019_finish -- ACCEPT statement list complete

   ffestd_V019_finish();

   Just wrap up any local activities.  */

void
ffestd_V019_finish ()
{
  ffestd_check_finish_ ();
}

#endif
/* ffestd_V020_start -- TYPE statement list begin

   ffestd_V020_start();

   Verify that TYPE is valid here, and begin accepting items in the
   list.  */

void
ffestd_V020_start (ffestvFormat format UNUSED)
{
  ffestd_check_start_ ();
  ffestd_subr_vxt_ ();
}

/* ffestd_V020_item -- TYPE statement i/o item

   ffestd_V020_item(expr,expr_token);

   Implement output-list expression.  */

void
ffestd_V020_item (ffebld expr UNUSED)
{
  ffestd_check_item_ ();
}

/* ffestd_V020_finish -- TYPE statement list complete

   ffestd_V020_finish();

   Just wrap up any local activities.  */

void
ffestd_V020_finish ()
{
  ffestd_check_finish_ ();
}

/* ffestd_V021 -- DELETE statement

   ffestd_V021();

   Make sure a DELETE is valid in the current context, and implement it.  */

#if FFESTR_VXT
void
ffestd_V021 ()
{
  ffestd_check_simple_ ();
  ffestd_subr_vxt_ ();
}

/* ffestd_V022 -- UNLOCK statement

   ffestd_V022();

   Make sure a UNLOCK is valid in the current context, and implement it.  */

void
ffestd_V022 ()
{
  ffestd_check_simple_ ();
  ffestd_subr_vxt_ ();
}

/* ffestd_V023_start -- ENCODE(...) statement list begin

   ffestd_V023_start();

   Verify that ENCODE is valid here, and begin accepting items in the
   list.  */

void
ffestd_V023_start ()
{
  ffestd_check_start_ ();
  ffestd_subr_vxt_ ();
}

/* ffestd_V023_item -- ENCODE statement i/o item

   ffestd_V023_item(expr,expr_token);

   Implement output-list expression.  */

void
ffestd_V023_item (ffebld expr)
{
  ffestd_check_item_ ();
}

/* ffestd_V023_finish -- ENCODE statement list complete

   ffestd_V023_finish();

   Just wrap up any local activities.  */

void
ffestd_V023_finish ()
{
  ffestd_check_finish_ ();
}

/* ffestd_V024_start -- DECODE(...) statement list begin

   ffestd_V024_start();

   Verify that DECODE is valid here, and begin accepting items in the
   list.  */

void
ffestd_V024_start ()
{
  ffestd_check_start_ ();
  ffestd_subr_vxt_ ();
}

/* ffestd_V024_item -- DECODE statement i/o item

   ffestd_V024_item(expr,expr_token);

   Implement output-list expression.  */

void
ffestd_V024_item (ffebld expr)
{
  ffestd_check_item_ ();
}

/* ffestd_V024_finish -- DECODE statement list complete

   ffestd_V024_finish();

   Just wrap up any local activities.  */

void
ffestd_V024_finish ()
{
  ffestd_check_finish_ ();
}

/* ffestd_V025_start -- DEFINEFILE statement list begin

   ffestd_V025_start();

   Verify that DEFINEFILE is valid here, and begin accepting items in the
   list.  */

void
ffestd_V025_start ()
{
  ffestd_check_start_ ();
  ffestd_subr_vxt_ ();
}

/* ffestd_V025_item -- DEFINE FILE statement item

   ffestd_V025_item(u,ut,m,mt,n,nt,asv,asvt);

   Implement item.  Treat each item kind of like a separate statement,
   since there's really no need to treat them as an aggregate.	*/

void
ffestd_V025_item (ffebld u, ffebld m, ffebld n, ffebld asv)
{
  ffestd_check_item_ ();
}

/* ffestd_V025_finish -- DEFINE FILE statement list complete

   ffestd_V025_finish();

   Just wrap up any local activities.  */

void
ffestd_V025_finish ()
{
  ffestd_check_finish_ ();
}

/* ffestd_V026 -- FIND statement

   ffestd_V026();

   Make sure a FIND is valid in the current context, and implement it.	*/

void
ffestd_V026 ()
{
  ffestd_check_simple_ ();
  ffestd_subr_vxt_ ();
}

#endif
/* ffestd_V027_start -- VXT PARAMETER statement list begin

   ffestd_V027_start();

   Verify that PARAMETER is valid here, and begin accepting items in the list.	*/

void
ffestd_V027_start ()
{
  ffestd_check_start_ ();
  ffestd_subr_vxt_ ();
}

/* ffestd_V027_item -- VXT PARAMETER statement assignment

   ffestd_V027_item(dest,dest_token,source,source_token);

   Make sure the source is a valid source for the destination; make the
   assignment.	*/

void
ffestd_V027_item (ffelexToken dest_token UNUSED, ffebld source UNUSED)
{
  ffestd_check_item_ ();
}

/* ffestd_V027_finish -- VXT PARAMETER statement list complete

   ffestd_V027_finish();

   Just wrap up any local activities.  */

void
ffestd_V027_finish ()
{
  ffestd_check_finish_ ();
}

/* Any executable statement.  */

void
ffestd_any ()
{
  ffestdStmt_ stmt;

  ffestd_check_simple_ ();

  stmt = ffestd_stmt_new_ (FFESTD_stmtidR841_);
  ffestd_stmt_append_ (stmt);
  ffestd_subr_line_save_ (stmt);
}
