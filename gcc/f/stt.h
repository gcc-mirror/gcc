/* stt.h -- Private #include File (module.h template V1.0)
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
      stt.c

   Modifications:
*/

/* Allow multiple inclusion to work. */

#ifndef GCC_F_STT_H
#define GCC_F_STT_H

/* Simple definitions and enumerations. */


/* Typedefs. */

typedef struct _ffest_case_list_ *ffesttCaseList;
typedef struct _ffest_dim_list_ *ffesttDimList;
typedef struct _ffest_expr_list_ *ffesttExprList;
typedef struct _ffest_format_value_ ffesttFormatValue;
typedef struct _ffest_format_list_ *ffesttFormatList;
typedef struct _ffest_imp_list_ *ffesttImpList;
typedef struct _ffest_token_item_ *ffesttTokenItem;
typedef struct _ffest_token_list_ *ffesttTokenList;

/* Include files needed by this one. */

#include "top.h"
#include "bld.h"
#include "info.h"
#include "lex.h"
#include "stp.h"

/* Structure definitions. */

struct _ffest_case_list_
  {
    ffesttCaseList next;
    ffesttCaseList previous;
    ffelexToken t;
    ffebld expr1;
    ffebld expr2;
    bool range;			/* TRUE if "[expr1]:[expr2]", FALSE if
				   "expr1". */
  };

struct _ffest_dim_list_
  {
    ffesttDimList next;
    ffesttDimList previous;
    ffelexToken t;
    ffebld lower;
    ffebld upper;
  };

struct _ffest_expr_list_
  {
    ffesttExprList next;
    ffesttExprList previous;
    ffelexToken t;
    ffebld expr;
  };

struct _ffest_token_item_
  {
    ffesttTokenItem next;
    ffesttTokenItem previous;
    ffelexToken t;
  };

struct _ffest_token_list_
  {
    ffesttTokenItem first;
    ffesttTokenItem last;
    int count;			/* Number of tokens in list. */
  };

struct _ffest_format_value_
  {
    bool present;		/* TRUE if value supplied (needed for
				   optional values only). */
    bool rtexpr;		/* FALSE if constant value here, TRUE if
				   run-time expr (VXT). */
    ffelexToken t;		/* The first token, or perhaps just prior if
				   can't get it. */
    union
      {
	ffeUnionLongPtr unused;	/* Make sure all the info gets copied. */
	long signed_val;	/* for R1011. */
	unsigned long unsigned_val;	/* For other constant values. */
	ffebld expr;		/* For run-time expression (VXT). */
      }
    u;
  };

struct _ffest_format_list_
  {
    ffesttFormatList next;
    ffesttFormatList previous;
    ffelexToken t;		/* The NAME, CHARACTER, or HOLLERITH token. */
    ffestpFormatType type;
    union ffest_format_
      {
	struct
	  {
	    ffesttFormatValue R1004;	/* r, the repeat count. */
	    ffesttFormatValue R1006;	/* w, the field width. */
	    ffesttFormatValue R1007_or_R1008;	/* m, the minimum number of
						   digits; d, the number of
						   decimal digits. */
	    ffesttFormatValue R1009;	/* e, the number of exponent digits. */
	  }
	R1005;			/* data-edit-desc. */
	struct
	  {
	    ffesttFormatValue val;	/* r, the repeat count; k, the
					   precision magnitude adjustment; n,
					   the column number (abs or rel). */
	  }
	R1010;			/* control-edit-desc. */
	struct
	  {
	    ffesttFormatValue R1004;	/* r, the repeat count. */
	    ffesttFormatList format;	/* the parenthesized
					   format-item-list. */
	  }
	R1003D;			/* format-item of for [r](format-item-list). */
	struct
	  {
	    ffesttFormatList parent;	/* NULL if outer list, else parent
					   item. */
	  }
	root;			/* FFESTP_formattypeNone case. */
      }
    u;
  };

/* Global objects accessed by users of this module. */


/* Declare functions with prototypes. */

void ffestt_caselist_append (ffesttCaseList list, bool range, ffebld case1,
			     ffebld case2, ffelexToken t);
ffesttCaseList ffestt_caselist_create (void);
void ffestt_caselist_kill (ffesttCaseList list);
void ffestt_dimlist_append (ffesttDimList list, ffebld lower, ffebld upper,
			    ffelexToken t);
ffebld ffestt_dimlist_as_expr (ffesttDimList list, ffeinfoRank *rank,
			       ffebld *array_size, ffebld *extents,
			       bool is_ugly_assumed);
ffesttDimList ffestt_dimlist_create (void);
void ffestt_dimlist_kill (ffesttDimList list);
ffestpDimtype ffestt_dimlist_type (ffesttDimList dims, bool is_ugly_assumed);
void ffestt_exprlist_append (ffesttExprList list, ffebld expr, ffelexToken t);
ffesttExprList ffestt_exprlist_create (void);
void ffestt_exprlist_drive (ffesttExprList list, void (*fn) (ffebld, ffelexToken));
void ffestt_exprlist_kill (ffesttExprList list);
ffesttFormatList ffestt_formatlist_append (ffesttFormatList list);
ffesttFormatList ffestt_formatlist_create (ffesttFormatList parent,
					   ffelexToken t);
void ffestt_formatlist_kill (ffesttFormatList list);
void ffestt_implist_append (ffesttImpList list, ffelexToken first,
			    ffelexToken last);
ffesttImpList ffestt_implist_create (void);
void ffestt_implist_drive (ffesttImpList list, void (*fn) (ffelexToken, ffelexToken));
void ffestt_implist_kill (ffesttImpList list);
void ffestt_tokenlist_append (ffesttTokenList list, ffelexToken t);
ffesttTokenList ffestt_tokenlist_create (void);
void ffestt_tokenlist_drive (ffesttTokenList list, void (*fn) (ffelexToken));
ffelexHandler ffestt_tokenlist_handle (ffesttTokenList list,
				       ffelexHandler handler);
void ffestt_tokenlist_kill (ffesttTokenList list);

/* Define macros. */

#define ffestt_init_0()
#define ffestt_init_1()
#define ffestt_init_2()
#define ffestt_init_3()
#define ffestt_init_4()
#define ffestt_terminate_0()
#define ffestt_terminate_1()
#define ffestt_terminate_2()
#define ffestt_terminate_3()
#define ffestt_terminate_4()
#define ffestt_tokenlist_count(tl) ((tl)->count)

/* End of #include file. */

#endif /* ! GCC_F_STT_H */
