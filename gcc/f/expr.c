/* expr.c -- Implementation File (module.c template V1.0)
   Copyright (C) 1995, 1996, 1997, 1998, 2001, 2002
   Free Software Foundation, Inc.
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
      None.

   Description:
      Handles syntactic and semantic analysis of Fortran expressions.

   Modifications:
*/

/* Include files. */

#include "proj.h"
#include "expr.h"
#include "bad.h"
#include "bld.h"
#include "com.h"
#include "global.h"
#include "implic.h"
#include "intrin.h"
#include "info.h"
#include "lex.h"
#include "malloc.h"
#include "src.h"
#include "st.h"
#include "symbol.h"
#include "str.h"
#include "target.h"
#include "where.h"
#include "real.h"

/* Externals defined here. */


/* Simple definitions and enumerations. */

typedef enum
  {
    FFEEXPR_exprtypeUNKNOWN_,
    FFEEXPR_exprtypeOPERAND_,
    FFEEXPR_exprtypeUNARY_,
    FFEEXPR_exprtypeBINARY_,
    FFEEXPR_exprtype_
  } ffeexprExprtype_;

typedef enum
  {
    FFEEXPR_operatorPOWER_,
    FFEEXPR_operatorMULTIPLY_,
    FFEEXPR_operatorDIVIDE_,
    FFEEXPR_operatorADD_,
    FFEEXPR_operatorSUBTRACT_,
    FFEEXPR_operatorCONCATENATE_,
    FFEEXPR_operatorLT_,
    FFEEXPR_operatorLE_,
    FFEEXPR_operatorEQ_,
    FFEEXPR_operatorNE_,
    FFEEXPR_operatorGT_,
    FFEEXPR_operatorGE_,
    FFEEXPR_operatorNOT_,
    FFEEXPR_operatorAND_,
    FFEEXPR_operatorOR_,
    FFEEXPR_operatorXOR_,
    FFEEXPR_operatorEQV_,
    FFEEXPR_operatorNEQV_,
    FFEEXPR_operator_
  } ffeexprOperator_;

typedef enum
  {
    FFEEXPR_operatorprecedenceHIGHEST_ = 1,
    FFEEXPR_operatorprecedencePOWER_ = 1,
    FFEEXPR_operatorprecedenceMULTIPLY_ = 2,
    FFEEXPR_operatorprecedenceDIVIDE_ = 2,
    FFEEXPR_operatorprecedenceADD_ = 3,
    FFEEXPR_operatorprecedenceSUBTRACT_ = 3,
    FFEEXPR_operatorprecedenceLOWARITH_ = 3,
    FFEEXPR_operatorprecedenceCONCATENATE_ = 3,
    FFEEXPR_operatorprecedenceLT_ = 4,
    FFEEXPR_operatorprecedenceLE_ = 4,
    FFEEXPR_operatorprecedenceEQ_ = 4,
    FFEEXPR_operatorprecedenceNE_ = 4,
    FFEEXPR_operatorprecedenceGT_ = 4,
    FFEEXPR_operatorprecedenceGE_ = 4,
    FFEEXPR_operatorprecedenceNOT_ = 5,
    FFEEXPR_operatorprecedenceAND_ = 6,
    FFEEXPR_operatorprecedenceOR_ = 7,
    FFEEXPR_operatorprecedenceXOR_ = 8,
    FFEEXPR_operatorprecedenceEQV_ = 8,
    FFEEXPR_operatorprecedenceNEQV_ = 8,
    FFEEXPR_operatorprecedenceLOWEST_ = 8,
    FFEEXPR_operatorprecedence_
  } ffeexprOperatorPrecedence_;

#define FFEEXPR_operatorassociativityL2R_ TRUE
#define FFEEXPR_operatorassociativityR2L_ FALSE
#define FFEEXPR_operatorassociativityPOWER_ FFEEXPR_operatorassociativityR2L_
#define FFEEXPR_operatorassociativityMULTIPLY_ FFEEXPR_operatorassociativityL2R_
#define FFEEXPR_operatorassociativityDIVIDE_ FFEEXPR_operatorassociativityL2R_
#define FFEEXPR_operatorassociativityADD_ FFEEXPR_operatorassociativityL2R_
#define FFEEXPR_operatorassociativitySUBTRACT_ FFEEXPR_operatorassociativityL2R_
#define FFEEXPR_operatorassociativityCONCATENATE_ FFEEXPR_operatorassociativityL2R_
#define FFEEXPR_operatorassociativityLT_ FFEEXPR_operatorassociativityL2R_
#define FFEEXPR_operatorassociativityLE_ FFEEXPR_operatorassociativityL2R_
#define FFEEXPR_operatorassociativityEQ_ FFEEXPR_operatorassociativityL2R_
#define FFEEXPR_operatorassociativityNE_ FFEEXPR_operatorassociativityL2R_
#define FFEEXPR_operatorassociativityGT_ FFEEXPR_operatorassociativityL2R_
#define FFEEXPR_operatorassociativityGE_ FFEEXPR_operatorassociativityL2R_
#define FFEEXPR_operatorassociativityNOT_ FFEEXPR_operatorassociativityL2R_
#define FFEEXPR_operatorassociativityAND_ FFEEXPR_operatorassociativityL2R_
#define FFEEXPR_operatorassociativityOR_ FFEEXPR_operatorassociativityL2R_
#define FFEEXPR_operatorassociativityXOR_ FFEEXPR_operatorassociativityL2R_
#define FFEEXPR_operatorassociativityEQV_ FFEEXPR_operatorassociativityL2R_
#define FFEEXPR_operatorassociativityNEQV_ FFEEXPR_operatorassociativityL2R_

typedef enum
  {
    FFEEXPR_parentypeFUNCTION_,
    FFEEXPR_parentypeSUBROUTINE_,
    FFEEXPR_parentypeARRAY_,
    FFEEXPR_parentypeSUBSTRING_,
    FFEEXPR_parentypeFUNSUBSTR_,/* Ambig: check for colon after first expr. */
    FFEEXPR_parentypeEQUIVALENCE_,	/* Ambig: ARRAY_ or SUBSTRING_. */
    FFEEXPR_parentypeANY_,	/* Allow basically anything. */
    FFEEXPR_parentype_
  } ffeexprParenType_;

typedef enum
  {
    FFEEXPR_percentNONE_,
    FFEEXPR_percentLOC_,
    FFEEXPR_percentVAL_,
    FFEEXPR_percentREF_,
    FFEEXPR_percentDESCR_,
    FFEEXPR_percent_
  } ffeexprPercent_;

/* Internal typedefs. */

typedef struct _ffeexpr_expr_ *ffeexprExpr_;
typedef bool ffeexprOperatorAssociativity_;
typedef struct _ffeexpr_stack_ *ffeexprStack_;

/* Private include files. */


/* Internal structure definitions. */

struct _ffeexpr_expr_
  {
    ffeexprExpr_ previous;
    ffelexToken token;
    ffeexprExprtype_ type;
    union
      {
	struct
	  {
	    ffeexprOperator_ op;
	    ffeexprOperatorPrecedence_ prec;
	    ffeexprOperatorAssociativity_ as;
	  }
	operator;
	ffebld operand;
      }
    u;
  };

struct _ffeexpr_stack_
  {
    ffeexprStack_ previous;
    mallocPool pool;
    ffeexprContext context;
    ffeexprCallback callback;
    ffelexToken first_token;
    ffeexprExpr_ exprstack;
    ffelexToken tokens[10];	/* Used in certain cases, like (unary)
				   open-paren. */
    ffebld expr;		/* For first of
				   complex/implied-do/substring/array-elements
				   / actual-args expression. */
    ffebld bound_list;		/* For tracking dimension bounds list of
				   array. */
    ffebldListBottom bottom;	/* For building lists. */
    ffeinfoRank rank;		/* For elements in an array reference. */
    bool constant;		/* TRUE while elements seen so far are
				   constants. */
    bool immediate;		/* TRUE while elements seen so far are
				   immediate/constants. */
    ffebld next_dummy;		/* Next SFUNC dummy arg in arg list. */
    ffebldListLength num_args;	/* Number of dummy args expected in arg list. */
    bool is_rhs;		/* TRUE if rhs context, FALSE otherwise. */
    ffeexprPercent_ percent;	/* Current %FOO keyword. */
  };

struct _ffeexpr_find_
  {
    ffelexToken t;
    ffelexHandler after;
    int level;
  };

/* Static objects accessed by functions in this module. */

static ffeexprStack_ ffeexpr_stack_;	/* Expression stack for semantic. */
static ffelexToken ffeexpr_tokens_[10];	/* Scratchpad tokens for syntactic. */
static ffestrOther ffeexpr_current_dotdot_;	/* Current .FOO. keyword. */
static long ffeexpr_hollerith_count_;	/* ffeexpr_token_number_ and caller. */
static int ffeexpr_level_;	/* Level of DATA implied-DO construct. */
static bool ffeexpr_is_substr_ok_;	/* If OPEN_PAREN as binary "op" ok. */
static struct _ffeexpr_find_ ffeexpr_find_;

/* Static functions (internal). */

static ffelexHandler ffeexpr_cb_close_paren_ (ffelexToken ft, ffebld expr,
					      ffelexToken t);
static ffelexHandler ffeexpr_cb_close_paren_ambig_ (ffelexToken ft,
						    ffebld expr,
						    ffelexToken t);
static ffelexHandler ffeexpr_cb_close_paren_ambig_1_ (ffelexToken t);
static ffelexHandler ffeexpr_cb_close_paren_c_ (ffelexToken ft,
						ffebld expr, ffelexToken t);
static ffelexHandler ffeexpr_cb_comma_c_ (ffelexToken ft, ffebld expr,
					  ffelexToken t);
static ffelexHandler ffeexpr_cb_close_paren_ci_ (ffelexToken ft,
						 ffebld expr, ffelexToken t);
static ffelexHandler ffeexpr_cb_comma_ci_ (ffelexToken ft, ffebld expr,
					   ffelexToken t);
static ffelexHandler ffeexpr_cb_comma_i_ (ffelexToken ft, ffebld expr,
					  ffelexToken t);
static ffelexHandler ffeexpr_cb_comma_i_1_ (ffelexToken ft, ffebld expr,
					    ffelexToken t);
static ffelexHandler ffeexpr_cb_comma_i_2_ (ffelexToken ft, ffebld expr,
					    ffelexToken t);
static ffelexHandler ffeexpr_cb_comma_i_3_ (ffelexToken ft, ffebld expr,
					    ffelexToken t);
static ffelexHandler ffeexpr_cb_comma_i_4_ (ffelexToken ft, ffebld expr,
					    ffelexToken t);
static ffelexHandler ffeexpr_cb_comma_i_5_ (ffelexToken t);
static ffelexHandler ffeexpr_cb_end_loc_ (ffelexToken ft, ffebld expr,
					  ffelexToken t);
static ffelexHandler ffeexpr_cb_end_notloc_ (ffelexToken ft, ffebld expr,
					     ffelexToken t);
static ffelexHandler ffeexpr_cb_end_notloc_1_ (ffelexToken t);
static ffesymbol ffeexpr_check_impctrl_ (ffesymbol s);
static void ffeexpr_check_impdo_ (ffebld list, ffelexToken list_t,
				  ffebld dovar, ffelexToken dovar_t);
static void ffeexpr_update_impdo_ (ffebld expr, ffebld dovar);
static void ffeexpr_update_impdo_sym_ (ffebld expr, ffesymbol dovar);
static ffeexprContext ffeexpr_context_outer_ (ffeexprStack_ s);
static ffeexprExpr_ ffeexpr_expr_new_ (void);
static void ffeexpr_fulfill_call_ (ffebld *expr, ffelexToken t);
static bool ffeexpr_isdigits_ (const char *p);
static ffelexHandler ffeexpr_token_first_lhs_ (ffelexToken t);
static ffelexHandler ffeexpr_token_first_lhs_1_ (ffelexToken t);
static ffelexHandler ffeexpr_token_first_rhs_ (ffelexToken t);
static ffelexHandler ffeexpr_token_first_rhs_1_ (ffelexToken t);
static ffelexHandler ffeexpr_token_first_rhs_2_ (ffelexToken t);
static ffelexHandler ffeexpr_token_first_rhs_3_ (ffelexToken t);
static ffelexHandler ffeexpr_token_first_rhs_4_ (ffelexToken t);
static ffelexHandler ffeexpr_token_first_rhs_5_ (ffelexToken t);
static ffelexHandler ffeexpr_token_first_rhs_6_ (ffelexToken t);
static ffelexHandler ffeexpr_token_namelist_ (ffelexToken t);
static void ffeexpr_expr_kill_ (ffeexprExpr_ e);
static void ffeexpr_exprstack_push_ (ffeexprExpr_ e);
static void ffeexpr_exprstack_push_binary_ (ffeexprExpr_ e);
static void ffeexpr_exprstack_push_operand_ (ffeexprExpr_ e);
static void ffeexpr_exprstack_push_unary_ (ffeexprExpr_ e);
static void ffeexpr_reduce_ (void);
static ffebld ffeexpr_reduced_bool1_ (ffebld reduced, ffeexprExpr_ op,
				      ffeexprExpr_ r);
static ffebld ffeexpr_reduced_bool2_ (ffebld reduced, ffeexprExpr_ l,
				      ffeexprExpr_ op, ffeexprExpr_ r);
static ffebld ffeexpr_reduced_concatenate_ (ffebld reduced, ffeexprExpr_ l,
					    ffeexprExpr_ op, ffeexprExpr_ r);
static ffebld ffeexpr_reduced_eqop2_ (ffebld reduced, ffeexprExpr_ l,
				      ffeexprExpr_ op, ffeexprExpr_ r);
static ffebld ffeexpr_reduced_math1_ (ffebld reduced, ffeexprExpr_ op,
				      ffeexprExpr_ r);
static ffebld ffeexpr_reduced_math2_ (ffebld reduced, ffeexprExpr_ l,
				      ffeexprExpr_ op, ffeexprExpr_ r);
static ffebld ffeexpr_reduced_power_ (ffebld reduced, ffeexprExpr_ l,
				      ffeexprExpr_ op, ffeexprExpr_ r);
static ffebld ffeexpr_reduced_relop2_ (ffebld reduced, ffeexprExpr_ l,
				       ffeexprExpr_ op, ffeexprExpr_ r);
static ffebld ffeexpr_reduced_ugly1_ (ffebld reduced, ffeexprExpr_ op, ffeexprExpr_ r);
static ffebld ffeexpr_reduced_ugly1log_ (ffebld reduced, ffeexprExpr_ op,
					 ffeexprExpr_ r);
static ffebld ffeexpr_reduced_ugly2_ (ffebld reduced, ffeexprExpr_ l,
				      ffeexprExpr_ op, ffeexprExpr_ r);
static ffebld ffeexpr_reduced_ugly2log_ (ffebld reduced, ffeexprExpr_ l,
					 ffeexprExpr_ op, ffeexprExpr_ r);
static ffelexHandler ffeexpr_find_close_paren_ (ffelexToken t,
						ffelexHandler after);
static ffelexHandler ffeexpr_nil_finished_ (ffelexToken t);
static ffelexHandler ffeexpr_nil_rhs_ (ffelexToken t);
static ffelexHandler ffeexpr_nil_period_ (ffelexToken t);
static ffelexHandler ffeexpr_nil_end_period_ (ffelexToken t);
static ffelexHandler ffeexpr_nil_swallow_period_ (ffelexToken t);
static ffelexHandler ffeexpr_nil_real_ (ffelexToken t);
static ffelexHandler ffeexpr_nil_real_exponent_ (ffelexToken t);
static ffelexHandler ffeexpr_nil_real_exp_sign_ (ffelexToken t);
static ffelexHandler ffeexpr_nil_number_ (ffelexToken t);
static ffelexHandler ffeexpr_nil_number_exponent_ (ffelexToken t);
static ffelexHandler ffeexpr_nil_number_exp_sign_ (ffelexToken t);
static ffelexHandler ffeexpr_nil_number_period_ (ffelexToken t);
static ffelexHandler ffeexpr_nil_number_per_exp_ (ffelexToken t);
static ffelexHandler ffeexpr_nil_number_real_ (ffelexToken t);
static ffelexHandler ffeexpr_nil_num_per_exp_sign_ (ffelexToken t);
static ffelexHandler ffeexpr_nil_number_real_exp_ (ffelexToken t);
static ffelexHandler ffeexpr_nil_num_real_exp_sn_ (ffelexToken t);
static ffelexHandler ffeexpr_nil_binary_ (ffelexToken t);
static ffelexHandler ffeexpr_nil_binary_period_ (ffelexToken t);
static ffelexHandler ffeexpr_nil_binary_end_per_ (ffelexToken t);
static ffelexHandler ffeexpr_nil_binary_sw_per_ (ffelexToken t);
static ffelexHandler ffeexpr_nil_quote_ (ffelexToken t);
static ffelexHandler ffeexpr_nil_apostrophe_ (ffelexToken t);
static ffelexHandler ffeexpr_nil_apos_char_ (ffelexToken t);
static ffelexHandler ffeexpr_nil_name_rhs_ (ffelexToken t);
static ffelexHandler ffeexpr_nil_name_apos_ (ffelexToken t);
static ffelexHandler ffeexpr_nil_name_apos_name_ (ffelexToken t);
static ffelexHandler ffeexpr_nil_percent_ (ffelexToken t);
static ffelexHandler ffeexpr_nil_percent_name_ (ffelexToken t);
static ffelexHandler ffeexpr_nil_substrp_ (ffelexToken t);
static ffelexHandler ffeexpr_finished_ (ffelexToken t);
static ffebld ffeexpr_finished_ambig_ (ffelexToken t, ffebld expr);
static ffelexHandler ffeexpr_token_lhs_ (ffelexToken t);
static ffelexHandler ffeexpr_token_rhs_ (ffelexToken t);
static ffelexHandler ffeexpr_token_binary_ (ffelexToken t);
static ffelexHandler ffeexpr_token_period_ (ffelexToken t);
static ffelexHandler ffeexpr_token_end_period_ (ffelexToken t);
static ffelexHandler ffeexpr_token_swallow_period_ (ffelexToken t);
static ffelexHandler ffeexpr_token_real_ (ffelexToken t);
static ffelexHandler ffeexpr_token_real_exponent_ (ffelexToken t);
static ffelexHandler ffeexpr_token_real_exp_sign_ (ffelexToken t);
static ffelexHandler ffeexpr_token_number_ (ffelexToken t);
static ffelexHandler ffeexpr_token_number_exponent_ (ffelexToken t);
static ffelexHandler ffeexpr_token_number_exp_sign_ (ffelexToken t);
static ffelexHandler ffeexpr_token_number_period_ (ffelexToken t);
static ffelexHandler ffeexpr_token_number_per_exp_ (ffelexToken t);
static ffelexHandler ffeexpr_token_number_real_ (ffelexToken t);
static ffelexHandler ffeexpr_token_num_per_exp_sign_ (ffelexToken t);
static ffelexHandler ffeexpr_token_number_real_exp_ (ffelexToken t);
static ffelexHandler ffeexpr_token_num_real_exp_sn_ (ffelexToken t);
static ffelexHandler ffeexpr_token_binary_period_ (ffelexToken t);
static ffelexHandler ffeexpr_token_binary_end_per_ (ffelexToken t);
static ffelexHandler ffeexpr_token_binary_sw_per_ (ffelexToken t);
static ffelexHandler ffeexpr_token_quote_ (ffelexToken t);
static ffelexHandler ffeexpr_token_apostrophe_ (ffelexToken t);
static ffelexHandler ffeexpr_token_apos_char_ (ffelexToken t);
static ffelexHandler ffeexpr_token_name_lhs_ (ffelexToken t);
static ffelexHandler ffeexpr_token_name_arg_ (ffelexToken t);
static ffelexHandler ffeexpr_token_name_rhs_ (ffelexToken t);
static ffelexHandler ffeexpr_token_name_apos_ (ffelexToken t);
static ffelexHandler ffeexpr_token_name_apos_name_ (ffelexToken t);
static ffelexHandler ffeexpr_token_percent_ (ffelexToken t);
static ffelexHandler ffeexpr_token_percent_name_ (ffelexToken t);
static ffelexHandler ffeexpr_token_arguments_ (ffelexToken ft, ffebld expr,
					       ffelexToken t);
static ffelexHandler ffeexpr_token_elements_ (ffelexToken ft, ffebld expr,
					      ffelexToken t);
static ffelexHandler ffeexpr_token_equivalence_ (ffelexToken ft, ffebld expr,
						 ffelexToken t);
static ffelexHandler ffeexpr_token_substring_ (ffelexToken ft, ffebld expr,
					       ffelexToken t);
static ffelexHandler ffeexpr_token_substring_1_ (ffelexToken ft, ffebld expr,
						 ffelexToken t);
static ffelexHandler ffeexpr_token_substrp_ (ffelexToken t);
static ffelexHandler ffeexpr_token_intrincheck_ (ffelexToken t);
static ffelexHandler ffeexpr_token_funsubstr_ (ffelexToken ft, ffebld expr,
					       ffelexToken t);
static ffelexHandler ffeexpr_token_anything_ (ffelexToken ft, ffebld expr,
					      ffelexToken t);
static void ffeexpr_make_float_const_ (char exp_letter, ffelexToken integer,
	    ffelexToken decimal, ffelexToken fraction, ffelexToken exponent,
		    ffelexToken exponent_sign, ffelexToken exponent_digits);
static ffesymbol ffeexpr_declare_unadorned_ (ffelexToken t, bool maybe_intrin);
static ffesymbol ffeexpr_sym_impdoitem_ (ffesymbol s, ffelexToken t);
static ffesymbol ffeexpr_sym_lhs_call_ (ffesymbol s, ffelexToken t);
static ffesymbol ffeexpr_sym_lhs_data_ (ffesymbol s, ffelexToken t);
static ffesymbol ffeexpr_sym_lhs_equivalence_ (ffesymbol s, ffelexToken t);
static ffesymbol ffeexpr_sym_lhs_extfunc_ (ffesymbol s, ffelexToken t);
static ffesymbol ffeexpr_sym_lhs_impdoctrl_ (ffesymbol s, ffelexToken t);
static ffesymbol ffeexpr_sym_lhs_parameter_ (ffesymbol s, ffelexToken t);
static ffesymbol ffeexpr_sym_rhs_actualarg_ (ffesymbol s, ffelexToken t);
static ffesymbol ffeexpr_sym_rhs_dimlist_ (ffesymbol s, ffelexToken t);
static ffesymbol ffeexpr_sym_rhs_let_ (ffesymbol s, ffelexToken t);
static ffesymbol ffeexpr_declare_parenthesized_ (ffelexToken t,
						 bool maybe_intrin,
					     ffeexprParenType_ *paren_type);
static ffesymbol ffeexpr_paren_rhs_let_ (ffesymbol s, ffelexToken t);

/* Internal macros. */

#define ffeexpr_paren_lhs_let_(s,t) ffeexpr_sym_rhs_let_(s,t)
#define ffeexpr_sym_lhs_let_(s,t) ffeexpr_sym_rhs_let_(s,t)

/* ffeexpr_collapse_convert -- Collapse convert expr

   ffebld expr;
   ffelexToken token;
   expr = ffeexpr_collapse_convert(expr,token);

   If the result of the expr is a constant, replaces the expr with the
   computed constant.  */

ffebld
ffeexpr_collapse_convert (ffebld expr, ffelexToken t)
{
  ffebad error = FFEBAD;
  ffebld l;
  ffebldConstantUnion u;
  ffeinfoBasictype bt;
  ffeinfoKindtype kt;
  ffetargetCharacterSize sz;
  ffetargetCharacterSize sz2;

  if (ffeinfo_where (ffebld_info (expr)) != FFEINFO_whereCONSTANT)
    return expr;

  l = ffebld_left (expr);

  if (ffebld_op (l) != FFEBLD_opCONTER)
    return expr;

  switch (bt = ffeinfo_basictype (ffebld_info (expr)))
    {
    case FFEINFO_basictypeANY:
      return expr;

    case FFEINFO_basictypeINTEGER:
      sz = FFETARGET_charactersizeNONE;
      switch (kt = ffeinfo_kindtype (ffebld_info (expr)))
	{
#if FFETARGET_okINTEGER1
	case FFEINFO_kindtypeINTEGER1:
	  switch (ffeinfo_basictype (ffebld_info (l)))
	    {
	    case FFEINFO_basictypeINTEGER:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okINTEGER2
		case FFEINFO_kindtypeINTEGER2:
		  error = ffetarget_convert_integer1_integer2
		    (ffebld_cu_ptr_integer1 (u),
		     ffebld_constant_integer2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER3
		case FFEINFO_kindtypeINTEGER3:
		  error = ffetarget_convert_integer1_integer3
		    (ffebld_cu_ptr_integer1 (u),
		     ffebld_constant_integer3 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER4
		case FFEINFO_kindtypeINTEGER4:
		  error = ffetarget_convert_integer1_integer4
		    (ffebld_cu_ptr_integer1 (u),
		     ffebld_constant_integer4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("INTEGER1/INTEGER bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeREAL:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okREAL1
		case FFEINFO_kindtypeREAL1:
		  error = ffetarget_convert_integer1_real1
		    (ffebld_cu_ptr_integer1 (u),
		     ffebld_constant_real1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okREAL2
		case FFEINFO_kindtypeREAL2:
		  error = ffetarget_convert_integer1_real2
		    (ffebld_cu_ptr_integer1 (u),
		     ffebld_constant_real2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okREAL3
		case FFEINFO_kindtypeREAL3:
		  error = ffetarget_convert_integer1_real3
		    (ffebld_cu_ptr_integer1 (u),
		     ffebld_constant_real3 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okREAL4
		case FFEINFO_kindtypeREAL4:
		  error = ffetarget_convert_integer1_real4
		    (ffebld_cu_ptr_integer1 (u),
		     ffebld_constant_real4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("INTEGER1/REAL bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeCOMPLEX:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okCOMPLEX1
		case FFEINFO_kindtypeREAL1:
		  error = ffetarget_convert_integer1_complex1
		    (ffebld_cu_ptr_integer1 (u),
		     ffebld_constant_complex1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okCOMPLEX2
		case FFEINFO_kindtypeREAL2:
		  error = ffetarget_convert_integer1_complex2
		    (ffebld_cu_ptr_integer1 (u),
		     ffebld_constant_complex2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okCOMPLEX3
		case FFEINFO_kindtypeREAL3:
		  error = ffetarget_convert_integer1_complex3
		    (ffebld_cu_ptr_integer1 (u),
		     ffebld_constant_complex3 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okCOMPLEX4
		case FFEINFO_kindtypeREAL4:
		  error = ffetarget_convert_integer1_complex4
		    (ffebld_cu_ptr_integer1 (u),
		     ffebld_constant_complex4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("INTEGER1/COMPLEX bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeLOGICAL:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okLOGICAL1
		case FFEINFO_kindtypeLOGICAL1:
		  error = ffetarget_convert_integer1_logical1
		    (ffebld_cu_ptr_integer1 (u),
		     ffebld_constant_logical1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okLOGICAL2
		case FFEINFO_kindtypeLOGICAL2:
		  error = ffetarget_convert_integer1_logical2
		    (ffebld_cu_ptr_integer1 (u),
		     ffebld_constant_logical2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okLOGICAL3
		case FFEINFO_kindtypeLOGICAL3:
		  error = ffetarget_convert_integer1_logical3
		    (ffebld_cu_ptr_integer1 (u),
		     ffebld_constant_logical3 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okLOGICAL4
		case FFEINFO_kindtypeLOGICAL4:
		  error = ffetarget_convert_integer1_logical4
		    (ffebld_cu_ptr_integer1 (u),
		     ffebld_constant_logical4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("INTEGER1/LOGICAL bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeCHARACTER:
	      error = ffetarget_convert_integer1_character1
		(ffebld_cu_ptr_integer1 (u),
		 ffebld_constant_character1 (ffebld_conter (l)));
	      break;

	    case FFEINFO_basictypeHOLLERITH:
	      error = ffetarget_convert_integer1_hollerith
		(ffebld_cu_ptr_integer1 (u),
		 ffebld_constant_hollerith (ffebld_conter (l)));
	      break;

	    case FFEINFO_basictypeTYPELESS:
	      error = ffetarget_convert_integer1_typeless
		(ffebld_cu_ptr_integer1 (u),
		 ffebld_constant_typeless (ffebld_conter (l)));
	      break;

	    default:
	      assert ("INTEGER1 bad type" == NULL);
	      break;
	    }

	  /* If conversion operation is not implemented, return original expr.  */
	  if (error == FFEBAD_NOCANDO)
	    return expr;

	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_integer1_val
	     (ffebld_cu_val_integer1 (u)), expr);
	  break;
#endif

#if FFETARGET_okINTEGER2
	case FFEINFO_kindtypeINTEGER2:
	  switch (ffeinfo_basictype (ffebld_info (l)))
	    {
	    case FFEINFO_basictypeINTEGER:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okINTEGER1
		case FFEINFO_kindtypeINTEGER1:
		  error = ffetarget_convert_integer2_integer1
		    (ffebld_cu_ptr_integer2 (u),
		     ffebld_constant_integer1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER3
		case FFEINFO_kindtypeINTEGER3:
		  error = ffetarget_convert_integer2_integer3
		    (ffebld_cu_ptr_integer2 (u),
		     ffebld_constant_integer3 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER4
		case FFEINFO_kindtypeINTEGER4:
		  error = ffetarget_convert_integer2_integer4
		    (ffebld_cu_ptr_integer2 (u),
		     ffebld_constant_integer4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("INTEGER2/INTEGER bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeREAL:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okREAL1
		case FFEINFO_kindtypeREAL1:
		  error = ffetarget_convert_integer2_real1
		    (ffebld_cu_ptr_integer2 (u),
		     ffebld_constant_real1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okREAL2
		case FFEINFO_kindtypeREAL2:
		  error = ffetarget_convert_integer2_real2
		    (ffebld_cu_ptr_integer2 (u),
		     ffebld_constant_real2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okREAL3
		case FFEINFO_kindtypeREAL3:
		  error = ffetarget_convert_integer2_real3
		    (ffebld_cu_ptr_integer2 (u),
		     ffebld_constant_real3 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okREAL4
		case FFEINFO_kindtypeREAL4:
		  error = ffetarget_convert_integer2_real4
		    (ffebld_cu_ptr_integer2 (u),
		     ffebld_constant_real4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("INTEGER2/REAL bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeCOMPLEX:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okCOMPLEX1
		case FFEINFO_kindtypeREAL1:
		  error = ffetarget_convert_integer2_complex1
		    (ffebld_cu_ptr_integer2 (u),
		     ffebld_constant_complex1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okCOMPLEX2
		case FFEINFO_kindtypeREAL2:
		  error = ffetarget_convert_integer2_complex2
		    (ffebld_cu_ptr_integer2 (u),
		     ffebld_constant_complex2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okCOMPLEX3
		case FFEINFO_kindtypeREAL3:
		  error = ffetarget_convert_integer2_complex3
		    (ffebld_cu_ptr_integer2 (u),
		     ffebld_constant_complex3 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okCOMPLEX4
		case FFEINFO_kindtypeREAL4:
		  error = ffetarget_convert_integer2_complex4
		    (ffebld_cu_ptr_integer2 (u),
		     ffebld_constant_complex4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("INTEGER2/COMPLEX bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeLOGICAL:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okLOGICAL1
		case FFEINFO_kindtypeLOGICAL1:
		  error = ffetarget_convert_integer2_logical1
		    (ffebld_cu_ptr_integer2 (u),
		     ffebld_constant_logical1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okLOGICAL2
		case FFEINFO_kindtypeLOGICAL2:
		  error = ffetarget_convert_integer2_logical2
		    (ffebld_cu_ptr_integer2 (u),
		     ffebld_constant_logical2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okLOGICAL3
		case FFEINFO_kindtypeLOGICAL3:
		  error = ffetarget_convert_integer2_logical3
		    (ffebld_cu_ptr_integer2 (u),
		     ffebld_constant_logical3 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okLOGICAL4
		case FFEINFO_kindtypeLOGICAL4:
		  error = ffetarget_convert_integer2_logical4
		    (ffebld_cu_ptr_integer2 (u),
		     ffebld_constant_logical4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("INTEGER2/LOGICAL bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeCHARACTER:
	      error = ffetarget_convert_integer2_character1
		(ffebld_cu_ptr_integer2 (u),
		 ffebld_constant_character1 (ffebld_conter (l)));
	      break;

	    case FFEINFO_basictypeHOLLERITH:
	      error = ffetarget_convert_integer2_hollerith
		(ffebld_cu_ptr_integer2 (u),
		 ffebld_constant_hollerith (ffebld_conter (l)));
	      break;

	    case FFEINFO_basictypeTYPELESS:
	      error = ffetarget_convert_integer2_typeless
		(ffebld_cu_ptr_integer2 (u),
		 ffebld_constant_typeless (ffebld_conter (l)));
	      break;

	    default:
	      assert ("INTEGER2 bad type" == NULL);
	      break;
	    }

	  /* If conversion operation is not implemented, return original expr.  */
	  if (error == FFEBAD_NOCANDO)
	    return expr;

	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_integer2_val
	     (ffebld_cu_val_integer2 (u)), expr);
	  break;
#endif

#if FFETARGET_okINTEGER3
	case FFEINFO_kindtypeINTEGER3:
	  switch (ffeinfo_basictype (ffebld_info (l)))
	    {
	    case FFEINFO_basictypeINTEGER:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okINTEGER1
		case FFEINFO_kindtypeINTEGER1:
		  error = ffetarget_convert_integer3_integer1
		    (ffebld_cu_ptr_integer3 (u),
		     ffebld_constant_integer1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER2
		case FFEINFO_kindtypeINTEGER2:
		  error = ffetarget_convert_integer3_integer2
		    (ffebld_cu_ptr_integer3 (u),
		     ffebld_constant_integer2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER4
		case FFEINFO_kindtypeINTEGER4:
		  error = ffetarget_convert_integer3_integer4
		    (ffebld_cu_ptr_integer3 (u),
		     ffebld_constant_integer4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("INTEGER3/INTEGER bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeREAL:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okREAL1
		case FFEINFO_kindtypeREAL1:
		  error = ffetarget_convert_integer3_real1
		    (ffebld_cu_ptr_integer3 (u),
		     ffebld_constant_real1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okREAL2
		case FFEINFO_kindtypeREAL2:
		  error = ffetarget_convert_integer3_real2
		    (ffebld_cu_ptr_integer3 (u),
		     ffebld_constant_real2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okREAL3
		case FFEINFO_kindtypeREAL3:
		  error = ffetarget_convert_integer3_real3
		    (ffebld_cu_ptr_integer3 (u),
		     ffebld_constant_real3 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okREAL4
		case FFEINFO_kindtypeREAL4:
		  error = ffetarget_convert_integer3_real4
		    (ffebld_cu_ptr_integer3 (u),
		     ffebld_constant_real4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("INTEGER3/REAL bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeCOMPLEX:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okCOMPLEX1
		case FFEINFO_kindtypeREAL1:
		  error = ffetarget_convert_integer3_complex1
		    (ffebld_cu_ptr_integer3 (u),
		     ffebld_constant_complex1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okCOMPLEX2
		case FFEINFO_kindtypeREAL2:
		  error = ffetarget_convert_integer3_complex2
		    (ffebld_cu_ptr_integer3 (u),
		     ffebld_constant_complex2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okCOMPLEX3
		case FFEINFO_kindtypeREAL3:
		  error = ffetarget_convert_integer3_complex3
		    (ffebld_cu_ptr_integer3 (u),
		     ffebld_constant_complex3 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okCOMPLEX4
		case FFEINFO_kindtypeREAL4:
		  error = ffetarget_convert_integer3_complex4
		    (ffebld_cu_ptr_integer3 (u),
		     ffebld_constant_complex4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("INTEGER3/COMPLEX bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeLOGICAL:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okLOGICAL1
		case FFEINFO_kindtypeLOGICAL1:
		  error = ffetarget_convert_integer3_logical1
		    (ffebld_cu_ptr_integer3 (u),
		     ffebld_constant_logical1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okLOGICAL2
		case FFEINFO_kindtypeLOGICAL2:
		  error = ffetarget_convert_integer3_logical2
		    (ffebld_cu_ptr_integer3 (u),
		     ffebld_constant_logical2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okLOGICAL3
		case FFEINFO_kindtypeLOGICAL3:
		  error = ffetarget_convert_integer3_logical3
		    (ffebld_cu_ptr_integer3 (u),
		     ffebld_constant_logical3 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okLOGICAL4
		case FFEINFO_kindtypeLOGICAL4:
		  error = ffetarget_convert_integer3_logical4
		    (ffebld_cu_ptr_integer3 (u),
		     ffebld_constant_logical4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("INTEGER3/LOGICAL bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeCHARACTER:
	      error = ffetarget_convert_integer3_character1
		(ffebld_cu_ptr_integer3 (u),
		 ffebld_constant_character1 (ffebld_conter (l)));
	      break;

	    case FFEINFO_basictypeHOLLERITH:
	      error = ffetarget_convert_integer3_hollerith
		(ffebld_cu_ptr_integer3 (u),
		 ffebld_constant_hollerith (ffebld_conter (l)));
	      break;

	    case FFEINFO_basictypeTYPELESS:
	      error = ffetarget_convert_integer3_typeless
		(ffebld_cu_ptr_integer3 (u),
		 ffebld_constant_typeless (ffebld_conter (l)));
	      break;

	    default:
	      assert ("INTEGER3 bad type" == NULL);
	      break;
	    }

	  /* If conversion operation is not implemented, return original expr.  */
	  if (error == FFEBAD_NOCANDO)
	    return expr;

	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_integer3_val
	     (ffebld_cu_val_integer3 (u)), expr);
	  break;
#endif

#if FFETARGET_okINTEGER4
	case FFEINFO_kindtypeINTEGER4:
	  switch (ffeinfo_basictype (ffebld_info (l)))
	    {
	    case FFEINFO_basictypeINTEGER:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okINTEGER1
		case FFEINFO_kindtypeINTEGER1:
		  error = ffetarget_convert_integer4_integer1
		    (ffebld_cu_ptr_integer4 (u),
		     ffebld_constant_integer1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER2
		case FFEINFO_kindtypeINTEGER2:
		  error = ffetarget_convert_integer4_integer2
		    (ffebld_cu_ptr_integer4 (u),
		     ffebld_constant_integer2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER3
		case FFEINFO_kindtypeINTEGER3:
		  error = ffetarget_convert_integer4_integer3
		    (ffebld_cu_ptr_integer4 (u),
		     ffebld_constant_integer3 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("INTEGER4/INTEGER bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeREAL:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okREAL1
		case FFEINFO_kindtypeREAL1:
		  error = ffetarget_convert_integer4_real1
		    (ffebld_cu_ptr_integer4 (u),
		     ffebld_constant_real1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okREAL2
		case FFEINFO_kindtypeREAL2:
		  error = ffetarget_convert_integer4_real2
		    (ffebld_cu_ptr_integer4 (u),
		     ffebld_constant_real2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okREAL3
		case FFEINFO_kindtypeREAL3:
		  error = ffetarget_convert_integer4_real3
		    (ffebld_cu_ptr_integer4 (u),
		     ffebld_constant_real3 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okREAL4
		case FFEINFO_kindtypeREAL4:
		  error = ffetarget_convert_integer4_real4
		    (ffebld_cu_ptr_integer4 (u),
		     ffebld_constant_real4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("INTEGER4/REAL bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeCOMPLEX:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okCOMPLEX1
		case FFEINFO_kindtypeREAL1:
		  error = ffetarget_convert_integer4_complex1
		    (ffebld_cu_ptr_integer4 (u),
		     ffebld_constant_complex1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okCOMPLEX2
		case FFEINFO_kindtypeREAL2:
		  error = ffetarget_convert_integer4_complex2
		    (ffebld_cu_ptr_integer4 (u),
		     ffebld_constant_complex2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okCOMPLEX3
		case FFEINFO_kindtypeREAL3:
		  error = ffetarget_convert_integer4_complex3
		    (ffebld_cu_ptr_integer4 (u),
		     ffebld_constant_complex3 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okCOMPLEX4
		case FFEINFO_kindtypeREAL4:
		  error = ffetarget_convert_integer4_complex4
		    (ffebld_cu_ptr_integer4 (u),
		     ffebld_constant_complex4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("INTEGER3/COMPLEX bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeLOGICAL:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okLOGICAL1
		case FFEINFO_kindtypeLOGICAL1:
		  error = ffetarget_convert_integer4_logical1
		    (ffebld_cu_ptr_integer4 (u),
		     ffebld_constant_logical1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okLOGICAL2
		case FFEINFO_kindtypeLOGICAL2:
		  error = ffetarget_convert_integer4_logical2
		    (ffebld_cu_ptr_integer4 (u),
		     ffebld_constant_logical2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okLOGICAL3
		case FFEINFO_kindtypeLOGICAL3:
		  error = ffetarget_convert_integer4_logical3
		    (ffebld_cu_ptr_integer4 (u),
		     ffebld_constant_logical3 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okLOGICAL4
		case FFEINFO_kindtypeLOGICAL4:
		  error = ffetarget_convert_integer4_logical4
		    (ffebld_cu_ptr_integer4 (u),
		     ffebld_constant_logical4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("INTEGER4/LOGICAL bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeCHARACTER:
	      error = ffetarget_convert_integer4_character1
		(ffebld_cu_ptr_integer4 (u),
		 ffebld_constant_character1 (ffebld_conter (l)));
	      break;

	    case FFEINFO_basictypeHOLLERITH:
	      error = ffetarget_convert_integer4_hollerith
		(ffebld_cu_ptr_integer4 (u),
		 ffebld_constant_hollerith (ffebld_conter (l)));
	      break;

	    case FFEINFO_basictypeTYPELESS:
	      error = ffetarget_convert_integer4_typeless
		(ffebld_cu_ptr_integer4 (u),
		 ffebld_constant_typeless (ffebld_conter (l)));
	      break;

	    default:
	      assert ("INTEGER4 bad type" == NULL);
	      break;
	    }

	  /* If conversion operation is not implemented, return original expr.  */
	  if (error == FFEBAD_NOCANDO)
	    return expr;

	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_integer4_val
	     (ffebld_cu_val_integer4 (u)), expr);
	  break;
#endif

	default:
	  assert ("bad integer kind type" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeLOGICAL:
      sz = FFETARGET_charactersizeNONE;
      switch (kt = ffeinfo_kindtype (ffebld_info (expr)))
	{
#if FFETARGET_okLOGICAL1
	case FFEINFO_kindtypeLOGICAL1:
	  switch (ffeinfo_basictype (ffebld_info (l)))
	    {
	    case FFEINFO_basictypeLOGICAL:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okLOGICAL2
		case FFEINFO_kindtypeLOGICAL2:
		  error = ffetarget_convert_logical1_logical2
		    (ffebld_cu_ptr_logical1 (u),
		     ffebld_constant_logical2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okLOGICAL3
		case FFEINFO_kindtypeLOGICAL3:
		  error = ffetarget_convert_logical1_logical3
		    (ffebld_cu_ptr_logical1 (u),
		     ffebld_constant_logical3 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okLOGICAL4
		case FFEINFO_kindtypeLOGICAL4:
		  error = ffetarget_convert_logical1_logical4
		    (ffebld_cu_ptr_logical1 (u),
		     ffebld_constant_logical4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("LOGICAL1/LOGICAL bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeINTEGER:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okINTEGER1
		case FFEINFO_kindtypeINTEGER1:
		  error = ffetarget_convert_logical1_integer1
		    (ffebld_cu_ptr_logical1 (u),
		     ffebld_constant_integer1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER2
		case FFEINFO_kindtypeINTEGER2:
		  error = ffetarget_convert_logical1_integer2
		    (ffebld_cu_ptr_logical1 (u),
		     ffebld_constant_integer2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER3
		case FFEINFO_kindtypeINTEGER3:
		  error = ffetarget_convert_logical1_integer3
		    (ffebld_cu_ptr_logical1 (u),
		     ffebld_constant_integer3 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER4
		case FFEINFO_kindtypeINTEGER4:
		  error = ffetarget_convert_logical1_integer4
		    (ffebld_cu_ptr_logical1 (u),
		     ffebld_constant_integer4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("LOGICAL1/INTEGER bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeCHARACTER:
	      error = ffetarget_convert_logical1_character1
		(ffebld_cu_ptr_logical1 (u),
		 ffebld_constant_character1 (ffebld_conter (l)));
	      break;

	    case FFEINFO_basictypeHOLLERITH:
	      error = ffetarget_convert_logical1_hollerith
		(ffebld_cu_ptr_logical1 (u),
		 ffebld_constant_hollerith (ffebld_conter (l)));
	      break;

	    case FFEINFO_basictypeTYPELESS:
	      error = ffetarget_convert_logical1_typeless
		(ffebld_cu_ptr_logical1 (u),
		 ffebld_constant_typeless (ffebld_conter (l)));
	      break;

	    default:
	      assert ("LOGICAL1 bad type" == NULL);
	      break;
	    }

	  /* If conversion operation is not implemented, return original expr.  */
	  if (error == FFEBAD_NOCANDO)
	    return expr;

	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logical1_val
	     (ffebld_cu_val_logical1 (u)), expr);
	  break;
#endif

#if FFETARGET_okLOGICAL2
	case FFEINFO_kindtypeLOGICAL2:
	  switch (ffeinfo_basictype (ffebld_info (l)))
	    {
	    case FFEINFO_basictypeLOGICAL:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okLOGICAL1
		case FFEINFO_kindtypeLOGICAL1:
		  error = ffetarget_convert_logical2_logical1
		    (ffebld_cu_ptr_logical2 (u),
		     ffebld_constant_logical1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okLOGICAL3
		case FFEINFO_kindtypeLOGICAL3:
		  error = ffetarget_convert_logical2_logical3
		    (ffebld_cu_ptr_logical2 (u),
		     ffebld_constant_logical3 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okLOGICAL4
		case FFEINFO_kindtypeLOGICAL4:
		  error = ffetarget_convert_logical2_logical4
		    (ffebld_cu_ptr_logical2 (u),
		     ffebld_constant_logical4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("LOGICAL2/LOGICAL bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeINTEGER:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okINTEGER1
		case FFEINFO_kindtypeINTEGER1:
		  error = ffetarget_convert_logical2_integer1
		    (ffebld_cu_ptr_logical2 (u),
		     ffebld_constant_integer1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER2
		case FFEINFO_kindtypeINTEGER2:
		  error = ffetarget_convert_logical2_integer2
		    (ffebld_cu_ptr_logical2 (u),
		     ffebld_constant_integer2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER3
		case FFEINFO_kindtypeINTEGER3:
		  error = ffetarget_convert_logical2_integer3
		    (ffebld_cu_ptr_logical2 (u),
		     ffebld_constant_integer3 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER4
		case FFEINFO_kindtypeINTEGER4:
		  error = ffetarget_convert_logical2_integer4
		    (ffebld_cu_ptr_logical2 (u),
		     ffebld_constant_integer4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("LOGICAL2/INTEGER bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeCHARACTER:
	      error = ffetarget_convert_logical2_character1
		(ffebld_cu_ptr_logical2 (u),
		 ffebld_constant_character1 (ffebld_conter (l)));
	      break;

	    case FFEINFO_basictypeHOLLERITH:
	      error = ffetarget_convert_logical2_hollerith
		(ffebld_cu_ptr_logical2 (u),
		 ffebld_constant_hollerith (ffebld_conter (l)));
	      break;

	    case FFEINFO_basictypeTYPELESS:
	      error = ffetarget_convert_logical2_typeless
		(ffebld_cu_ptr_logical2 (u),
		 ffebld_constant_typeless (ffebld_conter (l)));
	      break;

	    default:
	      assert ("LOGICAL2 bad type" == NULL);
	      break;
	    }

	  /* If conversion operation is not implemented, return original expr.  */
	  if (error == FFEBAD_NOCANDO)
	    return expr;

	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logical2_val
	     (ffebld_cu_val_logical2 (u)), expr);
	  break;
#endif

#if FFETARGET_okLOGICAL3
	case FFEINFO_kindtypeLOGICAL3:
	  switch (ffeinfo_basictype (ffebld_info (l)))
	    {
	    case FFEINFO_basictypeLOGICAL:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okLOGICAL1
		case FFEINFO_kindtypeLOGICAL1:
		  error = ffetarget_convert_logical3_logical1
		    (ffebld_cu_ptr_logical3 (u),
		     ffebld_constant_logical1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okLOGICAL2
		case FFEINFO_kindtypeLOGICAL2:
		  error = ffetarget_convert_logical3_logical2
		    (ffebld_cu_ptr_logical3 (u),
		     ffebld_constant_logical2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okLOGICAL4
		case FFEINFO_kindtypeLOGICAL4:
		  error = ffetarget_convert_logical3_logical4
		    (ffebld_cu_ptr_logical3 (u),
		     ffebld_constant_logical4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("LOGICAL3/LOGICAL bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeINTEGER:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okINTEGER1
		case FFEINFO_kindtypeINTEGER1:
		  error = ffetarget_convert_logical3_integer1
		    (ffebld_cu_ptr_logical3 (u),
		     ffebld_constant_integer1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER2
		case FFEINFO_kindtypeINTEGER2:
		  error = ffetarget_convert_logical3_integer2
		    (ffebld_cu_ptr_logical3 (u),
		     ffebld_constant_integer2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER3
		case FFEINFO_kindtypeINTEGER3:
		  error = ffetarget_convert_logical3_integer3
		    (ffebld_cu_ptr_logical3 (u),
		     ffebld_constant_integer3 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER4
		case FFEINFO_kindtypeINTEGER4:
		  error = ffetarget_convert_logical3_integer4
		    (ffebld_cu_ptr_logical3 (u),
		     ffebld_constant_integer4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("LOGICAL3/INTEGER bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeCHARACTER:
	      error = ffetarget_convert_logical3_character1
		(ffebld_cu_ptr_logical3 (u),
		 ffebld_constant_character1 (ffebld_conter (l)));
	      break;

	    case FFEINFO_basictypeHOLLERITH:
	      error = ffetarget_convert_logical3_hollerith
		(ffebld_cu_ptr_logical3 (u),
		 ffebld_constant_hollerith (ffebld_conter (l)));
	      break;

	    case FFEINFO_basictypeTYPELESS:
	      error = ffetarget_convert_logical3_typeless
		(ffebld_cu_ptr_logical3 (u),
		 ffebld_constant_typeless (ffebld_conter (l)));
	      break;

	    default:
	      assert ("LOGICAL3 bad type" == NULL);
	      break;
	    }

	  /* If conversion operation is not implemented, return original expr.  */
	  if (error == FFEBAD_NOCANDO)
	    return expr;

	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logical3_val
	     (ffebld_cu_val_logical3 (u)), expr);
	  break;
#endif

#if FFETARGET_okLOGICAL4
	case FFEINFO_kindtypeLOGICAL4:
	  switch (ffeinfo_basictype (ffebld_info (l)))
	    {
	    case FFEINFO_basictypeLOGICAL:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okLOGICAL1
		case FFEINFO_kindtypeLOGICAL1:
		  error = ffetarget_convert_logical4_logical1
		    (ffebld_cu_ptr_logical4 (u),
		     ffebld_constant_logical1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okLOGICAL2
		case FFEINFO_kindtypeLOGICAL2:
		  error = ffetarget_convert_logical4_logical2
		    (ffebld_cu_ptr_logical4 (u),
		     ffebld_constant_logical2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okLOGICAL3
		case FFEINFO_kindtypeLOGICAL3:
		  error = ffetarget_convert_logical4_logical3
		    (ffebld_cu_ptr_logical4 (u),
		     ffebld_constant_logical3 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("LOGICAL4/LOGICAL bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeINTEGER:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okINTEGER1
		case FFEINFO_kindtypeINTEGER1:
		  error = ffetarget_convert_logical4_integer1
		    (ffebld_cu_ptr_logical4 (u),
		     ffebld_constant_integer1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER2
		case FFEINFO_kindtypeINTEGER2:
		  error = ffetarget_convert_logical4_integer2
		    (ffebld_cu_ptr_logical4 (u),
		     ffebld_constant_integer2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER3
		case FFEINFO_kindtypeINTEGER3:
		  error = ffetarget_convert_logical4_integer3
		    (ffebld_cu_ptr_logical4 (u),
		     ffebld_constant_integer3 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER4
		case FFEINFO_kindtypeINTEGER4:
		  error = ffetarget_convert_logical4_integer4
		    (ffebld_cu_ptr_logical4 (u),
		     ffebld_constant_integer4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("LOGICAL4/INTEGER bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeCHARACTER:
	      error = ffetarget_convert_logical4_character1
		(ffebld_cu_ptr_logical4 (u),
		 ffebld_constant_character1 (ffebld_conter (l)));
	      break;

	    case FFEINFO_basictypeHOLLERITH:
	      error = ffetarget_convert_logical4_hollerith
		(ffebld_cu_ptr_logical4 (u),
		 ffebld_constant_hollerith (ffebld_conter (l)));
	      break;

	    case FFEINFO_basictypeTYPELESS:
	      error = ffetarget_convert_logical4_typeless
		(ffebld_cu_ptr_logical4 (u),
		 ffebld_constant_typeless (ffebld_conter (l)));
	      break;

	    default:
	      assert ("LOGICAL4 bad type" == NULL);
	      break;
	    }

	  /* If conversion operation is not implemented, return original expr.  */
	  if (error == FFEBAD_NOCANDO)
	    return expr;

	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logical4_val
	     (ffebld_cu_val_logical4 (u)), expr);
	  break;
#endif

	default:
	  assert ("bad logical kind type" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeREAL:
      sz = FFETARGET_charactersizeNONE;
      switch (kt = ffeinfo_kindtype (ffebld_info (expr)))
	{
#if FFETARGET_okREAL1
	case FFEINFO_kindtypeREAL1:
	  switch (ffeinfo_basictype (ffebld_info (l)))
	    {
	    case FFEINFO_basictypeINTEGER:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okINTEGER1
		case FFEINFO_kindtypeINTEGER1:
		  error = ffetarget_convert_real1_integer1
		    (ffebld_cu_ptr_real1 (u),
		     ffebld_constant_integer1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER2
		case FFEINFO_kindtypeINTEGER2:
		  error = ffetarget_convert_real1_integer2
		    (ffebld_cu_ptr_real1 (u),
		     ffebld_constant_integer2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER3
		case FFEINFO_kindtypeINTEGER3:
		  error = ffetarget_convert_real1_integer3
		    (ffebld_cu_ptr_real1 (u),
		     ffebld_constant_integer3 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER4
		case FFEINFO_kindtypeINTEGER4:
		  error = ffetarget_convert_real1_integer4
		    (ffebld_cu_ptr_real1 (u),
		     ffebld_constant_integer4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("REAL1/INTEGER bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeREAL:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okREAL2
		case FFEINFO_kindtypeREAL2:
		  error = ffetarget_convert_real1_real2
		    (ffebld_cu_ptr_real1 (u),
		     ffebld_constant_real2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okREAL3
		case FFEINFO_kindtypeREAL3:
		  error = ffetarget_convert_real1_real3
		    (ffebld_cu_ptr_real1 (u),
		     ffebld_constant_real3 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okREAL4
		case FFEINFO_kindtypeREAL4:
		  error = ffetarget_convert_real1_real4
		    (ffebld_cu_ptr_real1 (u),
		     ffebld_constant_real4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("REAL1/REAL bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeCOMPLEX:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okCOMPLEX1
		case FFEINFO_kindtypeREAL1:
		  error = ffetarget_convert_real1_complex1
		    (ffebld_cu_ptr_real1 (u),
		     ffebld_constant_complex1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okCOMPLEX2
		case FFEINFO_kindtypeREAL2:
		  error = ffetarget_convert_real1_complex2
		    (ffebld_cu_ptr_real1 (u),
		     ffebld_constant_complex2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okCOMPLEX3
		case FFEINFO_kindtypeREAL3:
		  error = ffetarget_convert_real1_complex3
		    (ffebld_cu_ptr_real1 (u),
		     ffebld_constant_complex3 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okCOMPLEX4
		case FFEINFO_kindtypeREAL4:
		  error = ffetarget_convert_real1_complex4
		    (ffebld_cu_ptr_real1 (u),
		     ffebld_constant_complex4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("REAL1/COMPLEX bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeCHARACTER:
	      error = ffetarget_convert_real1_character1
		(ffebld_cu_ptr_real1 (u),
		 ffebld_constant_character1 (ffebld_conter (l)));
	      break;

	    case FFEINFO_basictypeHOLLERITH:
	      error = ffetarget_convert_real1_hollerith
		(ffebld_cu_ptr_real1 (u),
		 ffebld_constant_hollerith (ffebld_conter (l)));
	      break;

	    case FFEINFO_basictypeTYPELESS:
	      error = ffetarget_convert_real1_typeless
		(ffebld_cu_ptr_real1 (u),
		 ffebld_constant_typeless (ffebld_conter (l)));
	      break;

	    default:
	      assert ("REAL1 bad type" == NULL);
	      break;
	    }

	  /* If conversion operation is not implemented, return original expr.  */
	  if (error == FFEBAD_NOCANDO)
	    return expr;

	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_real1_val
	     (ffebld_cu_val_real1 (u)), expr);
	  break;
#endif

#if FFETARGET_okREAL2
	case FFEINFO_kindtypeREAL2:
	  switch (ffeinfo_basictype (ffebld_info (l)))
	    {
	    case FFEINFO_basictypeINTEGER:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okINTEGER1
		case FFEINFO_kindtypeINTEGER1:
		  error = ffetarget_convert_real2_integer1
		    (ffebld_cu_ptr_real2 (u),
		     ffebld_constant_integer1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER2
		case FFEINFO_kindtypeINTEGER2:
		  error = ffetarget_convert_real2_integer2
		    (ffebld_cu_ptr_real2 (u),
		     ffebld_constant_integer2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER3
		case FFEINFO_kindtypeINTEGER3:
		  error = ffetarget_convert_real2_integer3
		    (ffebld_cu_ptr_real2 (u),
		     ffebld_constant_integer3 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER4
		case FFEINFO_kindtypeINTEGER4:
		  error = ffetarget_convert_real2_integer4
		    (ffebld_cu_ptr_real2 (u),
		     ffebld_constant_integer4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("REAL2/INTEGER bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeREAL:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okREAL1
		case FFEINFO_kindtypeREAL1:
		  error = ffetarget_convert_real2_real1
		    (ffebld_cu_ptr_real2 (u),
		     ffebld_constant_real1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okREAL3
		case FFEINFO_kindtypeREAL3:
		  error = ffetarget_convert_real2_real3
		    (ffebld_cu_ptr_real2 (u),
		     ffebld_constant_real3 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okREAL4
		case FFEINFO_kindtypeREAL4:
		  error = ffetarget_convert_real2_real4
		    (ffebld_cu_ptr_real2 (u),
		     ffebld_constant_real4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("REAL2/REAL bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeCOMPLEX:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okCOMPLEX1
		case FFEINFO_kindtypeREAL1:
		  error = ffetarget_convert_real2_complex1
		    (ffebld_cu_ptr_real2 (u),
		     ffebld_constant_complex1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okCOMPLEX2
		case FFEINFO_kindtypeREAL2:
		  error = ffetarget_convert_real2_complex2
		    (ffebld_cu_ptr_real2 (u),
		     ffebld_constant_complex2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okCOMPLEX3
		case FFEINFO_kindtypeREAL3:
		  error = ffetarget_convert_real2_complex3
		    (ffebld_cu_ptr_real2 (u),
		     ffebld_constant_complex3 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okCOMPLEX4
		case FFEINFO_kindtypeREAL4:
		  error = ffetarget_convert_real2_complex4
		    (ffebld_cu_ptr_real2 (u),
		     ffebld_constant_complex4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("REAL2/COMPLEX bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeCHARACTER:
	      error = ffetarget_convert_real2_character1
		(ffebld_cu_ptr_real2 (u),
		 ffebld_constant_character1 (ffebld_conter (l)));
	      break;

	    case FFEINFO_basictypeHOLLERITH:
	      error = ffetarget_convert_real2_hollerith
		(ffebld_cu_ptr_real2 (u),
		 ffebld_constant_hollerith (ffebld_conter (l)));
	      break;

	    case FFEINFO_basictypeTYPELESS:
	      error = ffetarget_convert_real2_typeless
		(ffebld_cu_ptr_real2 (u),
		 ffebld_constant_typeless (ffebld_conter (l)));
	      break;

	    default:
	      assert ("REAL2 bad type" == NULL);
	      break;
	    }

	  /* If conversion operation is not implemented, return original expr.  */
	  if (error == FFEBAD_NOCANDO)
	    return expr;

	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_real2_val
	     (ffebld_cu_val_real2 (u)), expr);
	  break;
#endif

#if FFETARGET_okREAL3
	case FFEINFO_kindtypeREAL3:
	  switch (ffeinfo_basictype (ffebld_info (l)))
	    {
	    case FFEINFO_basictypeINTEGER:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okINTEGER1
		case FFEINFO_kindtypeINTEGER1:
		  error = ffetarget_convert_real3_integer1
		    (ffebld_cu_ptr_real3 (u),
		     ffebld_constant_integer1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER2
		case FFEINFO_kindtypeINTEGER2:
		  error = ffetarget_convert_real3_integer2
		    (ffebld_cu_ptr_real3 (u),
		     ffebld_constant_integer2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER3
		case FFEINFO_kindtypeINTEGER3:
		  error = ffetarget_convert_real3_integer3
		    (ffebld_cu_ptr_real3 (u),
		     ffebld_constant_integer3 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER4
		case FFEINFO_kindtypeINTEGER4:
		  error = ffetarget_convert_real3_integer4
		    (ffebld_cu_ptr_real3 (u),
		     ffebld_constant_integer4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("REAL3/INTEGER bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeREAL:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okREAL1
		case FFEINFO_kindtypeREAL1:
		  error = ffetarget_convert_real3_real1
		    (ffebld_cu_ptr_real3 (u),
		     ffebld_constant_real1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okREAL2
		case FFEINFO_kindtypeREAL2:
		  error = ffetarget_convert_real3_real2
		    (ffebld_cu_ptr_real3 (u),
		     ffebld_constant_real2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okREAL4
		case FFEINFO_kindtypeREAL4:
		  error = ffetarget_convert_real3_real4
		    (ffebld_cu_ptr_real3 (u),
		     ffebld_constant_real4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("REAL3/REAL bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeCOMPLEX:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okCOMPLEX1
		case FFEINFO_kindtypeREAL1:
		  error = ffetarget_convert_real3_complex1
		    (ffebld_cu_ptr_real3 (u),
		     ffebld_constant_complex1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okCOMPLEX2
		case FFEINFO_kindtypeREAL2:
		  error = ffetarget_convert_real3_complex2
		    (ffebld_cu_ptr_real3 (u),
		     ffebld_constant_complex2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okCOMPLEX3
		case FFEINFO_kindtypeREAL3:
		  error = ffetarget_convert_real3_complex3
		    (ffebld_cu_ptr_real3 (u),
		     ffebld_constant_complex3 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okCOMPLEX4
		case FFEINFO_kindtypeREAL4:
		  error = ffetarget_convert_real3_complex4
		    (ffebld_cu_ptr_real3 (u),
		     ffebld_constant_complex4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("REAL3/COMPLEX bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeCHARACTER:
	      error = ffetarget_convert_real3_character1
		(ffebld_cu_ptr_real3 (u),
		 ffebld_constant_character1 (ffebld_conter (l)));
	      break;

	    case FFEINFO_basictypeHOLLERITH:
	      error = ffetarget_convert_real3_hollerith
		(ffebld_cu_ptr_real3 (u),
		 ffebld_constant_hollerith (ffebld_conter (l)));
	      break;

	    case FFEINFO_basictypeTYPELESS:
	      error = ffetarget_convert_real3_typeless
		(ffebld_cu_ptr_real3 (u),
		 ffebld_constant_typeless (ffebld_conter (l)));
	      break;

	    default:
	      assert ("REAL3 bad type" == NULL);
	      break;
	    }

	  /* If conversion operation is not implemented, return original expr.  */
	  if (error == FFEBAD_NOCANDO)
	    return expr;

	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_real3_val
	     (ffebld_cu_val_real3 (u)), expr);
	  break;
#endif

#if FFETARGET_okREAL4
	case FFEINFO_kindtypeREAL4:
	  switch (ffeinfo_basictype (ffebld_info (l)))
	    {
	    case FFEINFO_basictypeINTEGER:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okINTEGER1
		case FFEINFO_kindtypeINTEGER1:
		  error = ffetarget_convert_real4_integer1
		    (ffebld_cu_ptr_real4 (u),
		     ffebld_constant_integer1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER2
		case FFEINFO_kindtypeINTEGER2:
		  error = ffetarget_convert_real4_integer2
		    (ffebld_cu_ptr_real4 (u),
		     ffebld_constant_integer2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER3
		case FFEINFO_kindtypeINTEGER3:
		  error = ffetarget_convert_real4_integer3
		    (ffebld_cu_ptr_real4 (u),
		     ffebld_constant_integer3 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER4
		case FFEINFO_kindtypeINTEGER4:
		  error = ffetarget_convert_real4_integer4
		    (ffebld_cu_ptr_real4 (u),
		     ffebld_constant_integer4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("REAL4/INTEGER bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeREAL:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okREAL1
		case FFEINFO_kindtypeREAL1:
		  error = ffetarget_convert_real4_real1
		    (ffebld_cu_ptr_real4 (u),
		     ffebld_constant_real1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okREAL2
		case FFEINFO_kindtypeREAL2:
		  error = ffetarget_convert_real4_real2
		    (ffebld_cu_ptr_real4 (u),
		     ffebld_constant_real2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okREAL3
		case FFEINFO_kindtypeREAL3:
		  error = ffetarget_convert_real4_real3
		    (ffebld_cu_ptr_real4 (u),
		     ffebld_constant_real3 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("REAL4/REAL bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeCOMPLEX:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okCOMPLEX1
		case FFEINFO_kindtypeREAL1:
		  error = ffetarget_convert_real4_complex1
		    (ffebld_cu_ptr_real4 (u),
		     ffebld_constant_complex1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okCOMPLEX2
		case FFEINFO_kindtypeREAL2:
		  error = ffetarget_convert_real4_complex2
		    (ffebld_cu_ptr_real4 (u),
		     ffebld_constant_complex2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okCOMPLEX3
		case FFEINFO_kindtypeREAL3:
		  error = ffetarget_convert_real4_complex3
		    (ffebld_cu_ptr_real4 (u),
		     ffebld_constant_complex3 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okCOMPLEX4
		case FFEINFO_kindtypeREAL4:
		  error = ffetarget_convert_real4_complex4
		    (ffebld_cu_ptr_real4 (u),
		     ffebld_constant_complex4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("REAL4/COMPLEX bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeCHARACTER:
	      error = ffetarget_convert_real4_character1
		(ffebld_cu_ptr_real4 (u),
		 ffebld_constant_character1 (ffebld_conter (l)));
	      break;

	    case FFEINFO_basictypeHOLLERITH:
	      error = ffetarget_convert_real4_hollerith
		(ffebld_cu_ptr_real4 (u),
		 ffebld_constant_hollerith (ffebld_conter (l)));
	      break;

	    case FFEINFO_basictypeTYPELESS:
	      error = ffetarget_convert_real4_typeless
		(ffebld_cu_ptr_real4 (u),
		 ffebld_constant_typeless (ffebld_conter (l)));
	      break;

	    default:
	      assert ("REAL4 bad type" == NULL);
	      break;
	    }

	  /* If conversion operation is not implemented, return original expr.  */
	  if (error == FFEBAD_NOCANDO)
	    return expr;

	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_real4_val
	     (ffebld_cu_val_real4 (u)), expr);
	  break;
#endif

	default:
	  assert ("bad real kind type" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeCOMPLEX:
      sz = FFETARGET_charactersizeNONE;
      switch (kt = ffeinfo_kindtype (ffebld_info (expr)))
	{
#if FFETARGET_okCOMPLEX1
	case FFEINFO_kindtypeREAL1:
	  switch (ffeinfo_basictype (ffebld_info (l)))
	    {
	    case FFEINFO_basictypeINTEGER:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okINTEGER1
		case FFEINFO_kindtypeINTEGER1:
		  error = ffetarget_convert_complex1_integer1
		    (ffebld_cu_ptr_complex1 (u),
		     ffebld_constant_integer1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER2
		case FFEINFO_kindtypeINTEGER2:
		  error = ffetarget_convert_complex1_integer2
		    (ffebld_cu_ptr_complex1 (u),
		     ffebld_constant_integer2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER3
		case FFEINFO_kindtypeINTEGER3:
		  error = ffetarget_convert_complex1_integer3
		    (ffebld_cu_ptr_complex1 (u),
		     ffebld_constant_integer3 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER4
		case FFEINFO_kindtypeINTEGER4:
		  error = ffetarget_convert_complex1_integer4
		    (ffebld_cu_ptr_complex1 (u),
		     ffebld_constant_integer4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("COMPLEX1/INTEGER bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeREAL:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okREAL1
		case FFEINFO_kindtypeREAL1:
		  error = ffetarget_convert_complex1_real1
		    (ffebld_cu_ptr_complex1 (u),
		     ffebld_constant_real1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okREAL2
		case FFEINFO_kindtypeREAL2:
		  error = ffetarget_convert_complex1_real2
		    (ffebld_cu_ptr_complex1 (u),
		     ffebld_constant_real2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okREAL3
		case FFEINFO_kindtypeREAL3:
		  error = ffetarget_convert_complex1_real3
		    (ffebld_cu_ptr_complex1 (u),
		     ffebld_constant_real3 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okREAL4
		case FFEINFO_kindtypeREAL4:
		  error = ffetarget_convert_complex1_real4
		    (ffebld_cu_ptr_complex1 (u),
		     ffebld_constant_real4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("COMPLEX1/REAL bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeCOMPLEX:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okCOMPLEX2
		case FFEINFO_kindtypeREAL2:
		  error = ffetarget_convert_complex1_complex2
		    (ffebld_cu_ptr_complex1 (u),
		     ffebld_constant_complex2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okCOMPLEX3
		case FFEINFO_kindtypeREAL3:
		  error = ffetarget_convert_complex1_complex3
		    (ffebld_cu_ptr_complex1 (u),
		     ffebld_constant_complex3 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okCOMPLEX4
		case FFEINFO_kindtypeREAL4:
		  error = ffetarget_convert_complex1_complex4
		    (ffebld_cu_ptr_complex1 (u),
		     ffebld_constant_complex4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("COMPLEX1/COMPLEX bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeCHARACTER:
	      error = ffetarget_convert_complex1_character1
		(ffebld_cu_ptr_complex1 (u),
		 ffebld_constant_character1 (ffebld_conter (l)));
	      break;

	    case FFEINFO_basictypeHOLLERITH:
	      error = ffetarget_convert_complex1_hollerith
		(ffebld_cu_ptr_complex1 (u),
		 ffebld_constant_hollerith (ffebld_conter (l)));
	      break;

	    case FFEINFO_basictypeTYPELESS:
	      error = ffetarget_convert_complex1_typeless
		(ffebld_cu_ptr_complex1 (u),
		 ffebld_constant_typeless (ffebld_conter (l)));
	      break;

	    default:
	      assert ("COMPLEX1 bad type" == NULL);
	      break;
	    }

	  /* If conversion operation is not implemented, return original expr.  */
	  if (error == FFEBAD_NOCANDO)
	    return expr;

	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_complex1_val
	     (ffebld_cu_val_complex1 (u)), expr);
	  break;
#endif

#if FFETARGET_okCOMPLEX2
	case FFEINFO_kindtypeREAL2:
	  switch (ffeinfo_basictype (ffebld_info (l)))
	    {
	    case FFEINFO_basictypeINTEGER:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okINTEGER1
		case FFEINFO_kindtypeINTEGER1:
		  error = ffetarget_convert_complex2_integer1
		    (ffebld_cu_ptr_complex2 (u),
		     ffebld_constant_integer1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER2
		case FFEINFO_kindtypeINTEGER2:
		  error = ffetarget_convert_complex2_integer2
		    (ffebld_cu_ptr_complex2 (u),
		     ffebld_constant_integer2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER3
		case FFEINFO_kindtypeINTEGER3:
		  error = ffetarget_convert_complex2_integer3
		    (ffebld_cu_ptr_complex2 (u),
		     ffebld_constant_integer3 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER4
		case FFEINFO_kindtypeINTEGER4:
		  error = ffetarget_convert_complex2_integer4
		    (ffebld_cu_ptr_complex2 (u),
		     ffebld_constant_integer4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("COMPLEX2/INTEGER bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeREAL:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okREAL1
		case FFEINFO_kindtypeREAL1:
		  error = ffetarget_convert_complex2_real1
		    (ffebld_cu_ptr_complex2 (u),
		     ffebld_constant_real1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okREAL2
		case FFEINFO_kindtypeREAL2:
		  error = ffetarget_convert_complex2_real2
		    (ffebld_cu_ptr_complex2 (u),
		     ffebld_constant_real2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okREAL3
		case FFEINFO_kindtypeREAL3:
		  error = ffetarget_convert_complex2_real3
		    (ffebld_cu_ptr_complex2 (u),
		     ffebld_constant_real3 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okREAL4
		case FFEINFO_kindtypeREAL4:
		  error = ffetarget_convert_complex2_real4
		    (ffebld_cu_ptr_complex2 (u),
		     ffebld_constant_real4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("COMPLEX2/REAL bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeCOMPLEX:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okCOMPLEX1
		case FFEINFO_kindtypeREAL1:
		  error = ffetarget_convert_complex2_complex1
		    (ffebld_cu_ptr_complex2 (u),
		     ffebld_constant_complex1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okCOMPLEX3
		case FFEINFO_kindtypeREAL3:
		  error = ffetarget_convert_complex2_complex3
		    (ffebld_cu_ptr_complex2 (u),
		     ffebld_constant_complex3 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okCOMPLEX4
		case FFEINFO_kindtypeREAL4:
		  error = ffetarget_convert_complex2_complex4
		    (ffebld_cu_ptr_complex2 (u),
		     ffebld_constant_complex4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("COMPLEX2/COMPLEX bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeCHARACTER:
	      error = ffetarget_convert_complex2_character1
		(ffebld_cu_ptr_complex2 (u),
		 ffebld_constant_character1 (ffebld_conter (l)));
	      break;

	    case FFEINFO_basictypeHOLLERITH:
	      error = ffetarget_convert_complex2_hollerith
		(ffebld_cu_ptr_complex2 (u),
		 ffebld_constant_hollerith (ffebld_conter (l)));
	      break;

	    case FFEINFO_basictypeTYPELESS:
	      error = ffetarget_convert_complex2_typeless
		(ffebld_cu_ptr_complex2 (u),
		 ffebld_constant_typeless (ffebld_conter (l)));
	      break;

	    default:
	      assert ("COMPLEX2 bad type" == NULL);
	      break;
	    }

	  /* If conversion operation is not implemented, return original expr.  */
	  if (error == FFEBAD_NOCANDO)
	    return expr;

	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_complex2_val
	     (ffebld_cu_val_complex2 (u)), expr);
	  break;
#endif

#if FFETARGET_okCOMPLEX3
	case FFEINFO_kindtypeREAL3:
	  switch (ffeinfo_basictype (ffebld_info (l)))
	    {
	    case FFEINFO_basictypeINTEGER:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okINTEGER1
		case FFEINFO_kindtypeINTEGER1:
		  error = ffetarget_convert_complex3_integer1
		    (ffebld_cu_ptr_complex3 (u),
		     ffebld_constant_integer1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER2
		case FFEINFO_kindtypeINTEGER2:
		  error = ffetarget_convert_complex3_integer2
		    (ffebld_cu_ptr_complex3 (u),
		     ffebld_constant_integer2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER3
		case FFEINFO_kindtypeINTEGER3:
		  error = ffetarget_convert_complex3_integer3
		    (ffebld_cu_ptr_complex3 (u),
		     ffebld_constant_integer3 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER4
		case FFEINFO_kindtypeINTEGER4:
		  error = ffetarget_convert_complex3_integer4
		    (ffebld_cu_ptr_complex3 (u),
		     ffebld_constant_integer4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("COMPLEX3/INTEGER bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeREAL:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okREAL1
		case FFEINFO_kindtypeREAL1:
		  error = ffetarget_convert_complex3_real1
		    (ffebld_cu_ptr_complex3 (u),
		     ffebld_constant_real1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okREAL2
		case FFEINFO_kindtypeREAL2:
		  error = ffetarget_convert_complex3_real2
		    (ffebld_cu_ptr_complex3 (u),
		     ffebld_constant_real2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okREAL3
		case FFEINFO_kindtypeREAL3:
		  error = ffetarget_convert_complex3_real3
		    (ffebld_cu_ptr_complex3 (u),
		     ffebld_constant_real3 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okREAL4
		case FFEINFO_kindtypeREAL4:
		  error = ffetarget_convert_complex3_real4
		    (ffebld_cu_ptr_complex3 (u),
		     ffebld_constant_real4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("COMPLEX3/REAL bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeCOMPLEX:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okCOMPLEX1
		case FFEINFO_kindtypeREAL1:
		  error = ffetarget_convert_complex3_complex1
		    (ffebld_cu_ptr_complex3 (u),
		     ffebld_constant_complex1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okCOMPLEX2
		case FFEINFO_kindtypeREAL2:
		  error = ffetarget_convert_complex3_complex2
		    (ffebld_cu_ptr_complex3 (u),
		     ffebld_constant_complex2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okCOMPLEX4
		case FFEINFO_kindtypeREAL4:
		  error = ffetarget_convert_complex3_complex4
		    (ffebld_cu_ptr_complex3 (u),
		     ffebld_constant_complex4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("COMPLEX3/COMPLEX bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeCHARACTER:
	      error = ffetarget_convert_complex3_character1
		(ffebld_cu_ptr_complex3 (u),
		 ffebld_constant_character1 (ffebld_conter (l)));
	      break;

	    case FFEINFO_basictypeHOLLERITH:
	      error = ffetarget_convert_complex3_hollerith
		(ffebld_cu_ptr_complex3 (u),
		 ffebld_constant_hollerith (ffebld_conter (l)));
	      break;

	    case FFEINFO_basictypeTYPELESS:
	      error = ffetarget_convert_complex3_typeless
		(ffebld_cu_ptr_complex3 (u),
		 ffebld_constant_typeless (ffebld_conter (l)));
	      break;

	    default:
	      assert ("COMPLEX3 bad type" == NULL);
	      break;
	    }

	  /* If conversion operation is not implemented, return original expr.  */
	  if (error == FFEBAD_NOCANDO)
	    return expr;

	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_complex3_val
	     (ffebld_cu_val_complex3 (u)), expr);
	  break;
#endif

#if FFETARGET_okCOMPLEX4
	case FFEINFO_kindtypeREAL4:
	  switch (ffeinfo_basictype (ffebld_info (l)))
	    {
	    case FFEINFO_basictypeINTEGER:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okINTEGER1
		case FFEINFO_kindtypeINTEGER1:
		  error = ffetarget_convert_complex4_integer1
		    (ffebld_cu_ptr_complex4 (u),
		     ffebld_constant_integer1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER2
		case FFEINFO_kindtypeINTEGER2:
		  error = ffetarget_convert_complex4_integer2
		    (ffebld_cu_ptr_complex4 (u),
		     ffebld_constant_integer2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER3
		case FFEINFO_kindtypeINTEGER3:
		  error = ffetarget_convert_complex4_integer3
		    (ffebld_cu_ptr_complex4 (u),
		     ffebld_constant_integer3 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okINTEGER4
		case FFEINFO_kindtypeINTEGER4:
		  error = ffetarget_convert_complex4_integer4
		    (ffebld_cu_ptr_complex4 (u),
		     ffebld_constant_integer4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("COMPLEX4/INTEGER bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeREAL:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okREAL1
		case FFEINFO_kindtypeREAL1:
		  error = ffetarget_convert_complex4_real1
		    (ffebld_cu_ptr_complex4 (u),
		     ffebld_constant_real1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okREAL2
		case FFEINFO_kindtypeREAL2:
		  error = ffetarget_convert_complex4_real2
		    (ffebld_cu_ptr_complex4 (u),
		     ffebld_constant_real2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okREAL3
		case FFEINFO_kindtypeREAL3:
		  error = ffetarget_convert_complex4_real3
		    (ffebld_cu_ptr_complex4 (u),
		     ffebld_constant_real3 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okREAL4
		case FFEINFO_kindtypeREAL4:
		  error = ffetarget_convert_complex4_real4
		    (ffebld_cu_ptr_complex4 (u),
		     ffebld_constant_real4 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("COMPLEX4/REAL bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeCOMPLEX:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okCOMPLEX1
		case FFEINFO_kindtypeREAL1:
		  error = ffetarget_convert_complex4_complex1
		    (ffebld_cu_ptr_complex4 (u),
		     ffebld_constant_complex1 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okCOMPLEX2
		case FFEINFO_kindtypeREAL2:
		  error = ffetarget_convert_complex4_complex2
		    (ffebld_cu_ptr_complex4 (u),
		     ffebld_constant_complex2 (ffebld_conter (l)));
		  break;
#endif

#if FFETARGET_okCOMPLEX3
		case FFEINFO_kindtypeREAL3:
		  error = ffetarget_convert_complex4_complex3
		    (ffebld_cu_ptr_complex4 (u),
		     ffebld_constant_complex3 (ffebld_conter (l)));
		  break;
#endif

		default:
		  assert ("COMPLEX4/COMPLEX bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeCHARACTER:
	      error = ffetarget_convert_complex4_character1
		(ffebld_cu_ptr_complex4 (u),
		 ffebld_constant_character1 (ffebld_conter (l)));
	      break;

	    case FFEINFO_basictypeHOLLERITH:
	      error = ffetarget_convert_complex4_hollerith
		(ffebld_cu_ptr_complex4 (u),
		 ffebld_constant_hollerith (ffebld_conter (l)));
	      break;

	    case FFEINFO_basictypeTYPELESS:
	      error = ffetarget_convert_complex4_typeless
		(ffebld_cu_ptr_complex4 (u),
		 ffebld_constant_typeless (ffebld_conter (l)));
	      break;

	    default:
	      assert ("COMPLEX4 bad type" == NULL);
	      break;
	    }

	  /* If conversion operation is not implemented, return original expr.  */
	  if (error == FFEBAD_NOCANDO)
	    return expr;

	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_complex4_val
	     (ffebld_cu_val_complex4 (u)), expr);
	  break;
#endif

	default:
	  assert ("bad complex kind type" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeCHARACTER:
      if ((sz = ffebld_size (expr)) == FFETARGET_charactersizeNONE)
	return expr;
      kt = ffeinfo_kindtype (ffebld_info (expr));
      switch (kt)
	{
#if FFETARGET_okCHARACTER1
	case FFEINFO_kindtypeCHARACTER1:
	  switch (ffeinfo_basictype (ffebld_info (l)))
	    {
	    case FFEINFO_basictypeCHARACTER:
	      if ((sz2 = ffebld_size (l)) == FFETARGET_charactersizeNONE)
		return expr;
	      assert (kt == ffeinfo_kindtype (ffebld_info (l)));
	      assert (sz2 == ffetarget_length_character1
		      (ffebld_constant_character1
		       (ffebld_conter (l))));
	      error
		= ffetarget_convert_character1_character1
		(ffebld_cu_ptr_character1 (u), sz,
		 ffebld_constant_character1 (ffebld_conter (l)),
		 ffebld_constant_pool ());
	      break;

	    case FFEINFO_basictypeINTEGER:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okINTEGER1
		case FFEINFO_kindtypeINTEGER1:
		  error
		    = ffetarget_convert_character1_integer1
		      (ffebld_cu_ptr_character1 (u),
		       sz,
		       ffebld_constant_integer1 (ffebld_conter (l)),
		       ffebld_constant_pool ());
		  break;
#endif

#if FFETARGET_okINTEGER2
		case FFEINFO_kindtypeINTEGER2:
		  error
		    = ffetarget_convert_character1_integer2
		      (ffebld_cu_ptr_character1 (u),
		       sz,
		       ffebld_constant_integer2 (ffebld_conter (l)),
		       ffebld_constant_pool ());
		  break;
#endif

#if FFETARGET_okINTEGER3
		case FFEINFO_kindtypeINTEGER3:
		  error
		    = ffetarget_convert_character1_integer3
		      (ffebld_cu_ptr_character1 (u),
		       sz,
		       ffebld_constant_integer3 (ffebld_conter (l)),
		       ffebld_constant_pool ());
		  break;
#endif

#if FFETARGET_okINTEGER4
		case FFEINFO_kindtypeINTEGER4:
		  error
		    = ffetarget_convert_character1_integer4
		      (ffebld_cu_ptr_character1 (u),
		       sz,
		       ffebld_constant_integer4 (ffebld_conter (l)),
		       ffebld_constant_pool ());
		  break;
#endif

		default:
		  assert ("CHARACTER1/INTEGER bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeLOGICAL:
	      switch (ffeinfo_kindtype (ffebld_info (l)))
		{
#if FFETARGET_okLOGICAL1
		case FFEINFO_kindtypeLOGICAL1:
		  error
		    = ffetarget_convert_character1_logical1
		      (ffebld_cu_ptr_character1 (u),
		       sz,
		       ffebld_constant_logical1 (ffebld_conter (l)),
		       ffebld_constant_pool ());
		  break;
#endif

#if FFETARGET_okLOGICAL2
		case FFEINFO_kindtypeLOGICAL2:
		  error
		    = ffetarget_convert_character1_logical2
		      (ffebld_cu_ptr_character1 (u),
		       sz,
		       ffebld_constant_logical2 (ffebld_conter (l)),
		       ffebld_constant_pool ());
		  break;
#endif

#if FFETARGET_okLOGICAL3
		case FFEINFO_kindtypeLOGICAL3:
		  error
		    = ffetarget_convert_character1_logical3
		      (ffebld_cu_ptr_character1 (u),
		       sz,
		       ffebld_constant_logical3 (ffebld_conter (l)),
		       ffebld_constant_pool ());
		  break;
#endif

#if FFETARGET_okLOGICAL4
		case FFEINFO_kindtypeLOGICAL4:
		  error
		    = ffetarget_convert_character1_logical4
		      (ffebld_cu_ptr_character1 (u),
		       sz,
		       ffebld_constant_logical4 (ffebld_conter (l)),
		       ffebld_constant_pool ());
		  break;
#endif

		default:
		  assert ("CHARACTER1/LOGICAL bad source kind type" == NULL);
		  break;
		}
	      break;

	    case FFEINFO_basictypeHOLLERITH:
	      error
		= ffetarget_convert_character1_hollerith
		(ffebld_cu_ptr_character1 (u),
		 sz,
		 ffebld_constant_hollerith (ffebld_conter (l)),
		 ffebld_constant_pool ());
	      break;

	    case FFEINFO_basictypeTYPELESS:
	      error
		= ffetarget_convert_character1_typeless
		(ffebld_cu_ptr_character1 (u),
		 sz,
		 ffebld_constant_typeless (ffebld_conter (l)),
		 ffebld_constant_pool ());
	      break;

	    default:
	      assert ("CHARACTER1 bad type" == NULL);
	    }

	  expr
	    = ffebld_new_conter_with_orig
	    (ffebld_constant_new_character1_val
	     (ffebld_cu_val_character1 (u)),
	     expr);
	  break;
#endif

	default:
	  assert ("bad character kind type" == NULL);
	  break;
	}
      break;

    default:
      assert ("bad type" == NULL);
      return expr;
    }

  ffebld_set_info (expr, ffeinfo_new
		   (bt,
		    kt,
		    0,
		    FFEINFO_kindENTITY,
		    FFEINFO_whereCONSTANT,
		    sz));

  if ((error != FFEBAD)
      && ffebad_start (error))
    {
      assert (t != NULL);
      ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
      ffebad_finish ();
    }

  return expr;
}

/* ffeexpr_collapse_paren -- Collapse paren expr

   ffebld expr;
   ffelexToken token;
   expr = ffeexpr_collapse_paren(expr,token);

   If the result of the expr is a constant, replaces the expr with the
   computed constant.  */

ffebld
ffeexpr_collapse_paren (ffebld expr, ffelexToken t UNUSED)
{
  ffebld r;
  ffeinfoBasictype bt;
  ffeinfoKindtype kt;
  ffetargetCharacterSize len;

  if (ffeinfo_where (ffebld_info (expr)) != FFEINFO_whereCONSTANT)
    return expr;

  r = ffebld_left (expr);

  if (ffebld_op (r) != FFEBLD_opCONTER)
    return expr;

  bt = ffeinfo_basictype (ffebld_info (r));
  kt = ffeinfo_kindtype (ffebld_info (r));
  len = ffebld_size (r);

  expr = ffebld_new_conter_with_orig (ffebld_constant_copy (ffebld_conter (r)),
				      expr);

  ffebld_set_info (expr, ffeinfo_new
		   (bt,
		    kt,
		    0,
		    FFEINFO_kindENTITY,
		    FFEINFO_whereCONSTANT,
		    len));

  return expr;
}

/* ffeexpr_collapse_uplus -- Collapse uplus expr

   ffebld expr;
   ffelexToken token;
   expr = ffeexpr_collapse_uplus(expr,token);

   If the result of the expr is a constant, replaces the expr with the
   computed constant.  */

ffebld
ffeexpr_collapse_uplus (ffebld expr, ffelexToken t UNUSED)
{
  ffebld r;
  ffeinfoBasictype bt;
  ffeinfoKindtype kt;
  ffetargetCharacterSize len;

  if (ffeinfo_where (ffebld_info (expr)) != FFEINFO_whereCONSTANT)
    return expr;

  r = ffebld_left (expr);

  if (ffebld_op (r) != FFEBLD_opCONTER)
    return expr;

  bt = ffeinfo_basictype (ffebld_info (r));
  kt = ffeinfo_kindtype (ffebld_info (r));
  len = ffebld_size (r);

  expr = ffebld_new_conter_with_orig (ffebld_constant_copy (ffebld_conter (r)),
				      expr);

  ffebld_set_info (expr, ffeinfo_new
		   (bt,
		    kt,
		    0,
		    FFEINFO_kindENTITY,
		    FFEINFO_whereCONSTANT,
		    len));

  return expr;
}

/* ffeexpr_collapse_uminus -- Collapse uminus expr

   ffebld expr;
   ffelexToken token;
   expr = ffeexpr_collapse_uminus(expr,token);

   If the result of the expr is a constant, replaces the expr with the
   computed constant.  */

ffebld
ffeexpr_collapse_uminus (ffebld expr, ffelexToken t)
{
  ffebad error = FFEBAD;
  ffebld r;
  ffebldConstantUnion u;
  ffeinfoBasictype bt;
  ffeinfoKindtype kt;

  if (ffeinfo_where (ffebld_info (expr)) != FFEINFO_whereCONSTANT)
    return expr;

  r = ffebld_left (expr);

  if (ffebld_op (r) != FFEBLD_opCONTER)
    return expr;

  switch (bt = ffeinfo_basictype (ffebld_info (expr)))
    {
    case FFEINFO_basictypeANY:
      return expr;

    case FFEINFO_basictypeINTEGER:
      switch (kt = ffeinfo_kindtype (ffebld_info (expr)))
	{
#if FFETARGET_okINTEGER1
	case FFEINFO_kindtypeINTEGER1:
	  error = ffetarget_uminus_integer1 (ffebld_cu_ptr_integer1 (u),
			      ffebld_constant_integer1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer1_val
					(ffebld_cu_val_integer1 (u)), expr);
	  break;
#endif

#if FFETARGET_okINTEGER2
	case FFEINFO_kindtypeINTEGER2:
	  error = ffetarget_uminus_integer2 (ffebld_cu_ptr_integer2 (u),
			      ffebld_constant_integer2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer2_val
					(ffebld_cu_val_integer2 (u)), expr);
	  break;
#endif

#if FFETARGET_okINTEGER3
	case FFEINFO_kindtypeINTEGER3:
	  error = ffetarget_uminus_integer3 (ffebld_cu_ptr_integer3 (u),
			      ffebld_constant_integer3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer3_val
					(ffebld_cu_val_integer3 (u)), expr);
	  break;
#endif

#if FFETARGET_okINTEGER4
	case FFEINFO_kindtypeINTEGER4:
	  error = ffetarget_uminus_integer4 (ffebld_cu_ptr_integer4 (u),
			      ffebld_constant_integer4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer4_val
					(ffebld_cu_val_integer4 (u)), expr);
	  break;
#endif

	default:
	  assert ("bad integer kind type" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeREAL:
      switch (kt = ffeinfo_kindtype (ffebld_info (expr)))
	{
#if FFETARGET_okREAL1
	case FFEINFO_kindtypeREAL1:
	  error = ffetarget_uminus_real1 (ffebld_cu_ptr_real1 (u),
				 ffebld_constant_real1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_real1_val
					   (ffebld_cu_val_real1 (u)), expr);
	  break;
#endif

#if FFETARGET_okREAL2
	case FFEINFO_kindtypeREAL2:
	  error = ffetarget_uminus_real2 (ffebld_cu_ptr_real2 (u),
				 ffebld_constant_real2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_real2_val
					   (ffebld_cu_val_real2 (u)), expr);
	  break;
#endif

#if FFETARGET_okREAL3
	case FFEINFO_kindtypeREAL3:
	  error = ffetarget_uminus_real3 (ffebld_cu_ptr_real3 (u),
				 ffebld_constant_real3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_real3_val
					   (ffebld_cu_val_real3 (u)), expr);
	  break;
#endif

#if FFETARGET_okREAL4
	case FFEINFO_kindtypeREAL4:
	  error = ffetarget_uminus_real4 (ffebld_cu_ptr_real4 (u),
				 ffebld_constant_real4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_real4_val
					   (ffebld_cu_val_real4 (u)), expr);
	  break;
#endif

	default:
	  assert ("bad real kind type" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeCOMPLEX:
      switch (kt = ffeinfo_kindtype (ffebld_info (expr)))
	{
#if FFETARGET_okCOMPLEX1
	case FFEINFO_kindtypeREAL1:
	  error = ffetarget_uminus_complex1 (ffebld_cu_ptr_complex1 (u),
			      ffebld_constant_complex1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_complex1_val
					(ffebld_cu_val_complex1 (u)), expr);
	  break;
#endif

#if FFETARGET_okCOMPLEX2
	case FFEINFO_kindtypeREAL2:
	  error = ffetarget_uminus_complex2 (ffebld_cu_ptr_complex2 (u),
			      ffebld_constant_complex2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_complex2_val
					(ffebld_cu_val_complex2 (u)), expr);
	  break;
#endif

#if FFETARGET_okCOMPLEX3
	case FFEINFO_kindtypeREAL3:
	  error = ffetarget_uminus_complex3 (ffebld_cu_ptr_complex3 (u),
			      ffebld_constant_complex3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_complex3_val
					(ffebld_cu_val_complex3 (u)), expr);
	  break;
#endif

#if FFETARGET_okCOMPLEX4
	case FFEINFO_kindtypeREAL4:
	  error = ffetarget_uminus_complex4 (ffebld_cu_ptr_complex4 (u),
			      ffebld_constant_complex4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_complex4_val
					(ffebld_cu_val_complex4 (u)), expr);
	  break;
#endif

	default:
	  assert ("bad complex kind type" == NULL);
	  break;
	}
      break;

    default:
      assert ("bad type" == NULL);
      return expr;
    }

  ffebld_set_info (expr, ffeinfo_new
		   (bt,
		    kt,
		    0,
		    FFEINFO_kindENTITY,
		    FFEINFO_whereCONSTANT,
		    FFETARGET_charactersizeNONE));

  if ((error != FFEBAD)
      && ffebad_start (error))
    {
      ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
      ffebad_finish ();
    }

  return expr;
}

/* ffeexpr_collapse_not -- Collapse not expr

   ffebld expr;
   ffelexToken token;
   expr = ffeexpr_collapse_not(expr,token);

   If the result of the expr is a constant, replaces the expr with the
   computed constant.  */

ffebld
ffeexpr_collapse_not (ffebld expr, ffelexToken t)
{
  ffebad error = FFEBAD;
  ffebld r;
  ffebldConstantUnion u;
  ffeinfoBasictype bt;
  ffeinfoKindtype kt;

  if (ffeinfo_where (ffebld_info (expr)) != FFEINFO_whereCONSTANT)
    return expr;

  r = ffebld_left (expr);

  if (ffebld_op (r) != FFEBLD_opCONTER)
    return expr;

  switch (bt = ffeinfo_basictype (ffebld_info (expr)))
    {
    case FFEINFO_basictypeANY:
      return expr;

    case FFEINFO_basictypeINTEGER:
      switch (kt = ffeinfo_kindtype (ffebld_info (expr)))
	{
#if FFETARGET_okINTEGER1
	case FFEINFO_kindtypeINTEGER1:
	  error = ffetarget_not_integer1 (ffebld_cu_ptr_integer1 (u),
			      ffebld_constant_integer1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer1_val
					(ffebld_cu_val_integer1 (u)), expr);
	  break;
#endif

#if FFETARGET_okINTEGER2
	case FFEINFO_kindtypeINTEGER2:
	  error = ffetarget_not_integer2 (ffebld_cu_ptr_integer2 (u),
			      ffebld_constant_integer2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer2_val
					(ffebld_cu_val_integer2 (u)), expr);
	  break;
#endif

#if FFETARGET_okINTEGER3
	case FFEINFO_kindtypeINTEGER3:
	  error = ffetarget_not_integer3 (ffebld_cu_ptr_integer3 (u),
			      ffebld_constant_integer3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer3_val
					(ffebld_cu_val_integer3 (u)), expr);
	  break;
#endif

#if FFETARGET_okINTEGER4
	case FFEINFO_kindtypeINTEGER4:
	  error = ffetarget_not_integer4 (ffebld_cu_ptr_integer4 (u),
			      ffebld_constant_integer4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer4_val
					(ffebld_cu_val_integer4 (u)), expr);
	  break;
#endif

	default:
	  assert ("bad integer kind type" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeLOGICAL:
      switch (kt = ffeinfo_kindtype (ffebld_info (expr)))
	{
#if FFETARGET_okLOGICAL1
	case FFEINFO_kindtypeLOGICAL1:
	  error = ffetarget_not_logical1 (ffebld_cu_ptr_logical1 (u),
			      ffebld_constant_logical1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_logical1_val
					(ffebld_cu_val_logical1 (u)), expr);
	  break;
#endif

#if FFETARGET_okLOGICAL2
	case FFEINFO_kindtypeLOGICAL2:
	  error = ffetarget_not_logical2 (ffebld_cu_ptr_logical2 (u),
			      ffebld_constant_logical2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_logical2_val
					(ffebld_cu_val_logical2 (u)), expr);
	  break;
#endif

#if FFETARGET_okLOGICAL3
	case FFEINFO_kindtypeLOGICAL3:
	  error = ffetarget_not_logical3 (ffebld_cu_ptr_logical3 (u),
			      ffebld_constant_logical3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_logical3_val
					(ffebld_cu_val_logical3 (u)), expr);
	  break;
#endif

#if FFETARGET_okLOGICAL4
	case FFEINFO_kindtypeLOGICAL4:
	  error = ffetarget_not_logical4 (ffebld_cu_ptr_logical4 (u),
			      ffebld_constant_logical4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_logical4_val
					(ffebld_cu_val_logical4 (u)), expr);
	  break;
#endif

	default:
	  assert ("bad logical kind type" == NULL);
	  break;
	}
      break;

    default:
      assert ("bad type" == NULL);
      return expr;
    }

  ffebld_set_info (expr, ffeinfo_new
		   (bt,
		    kt,
		    0,
		    FFEINFO_kindENTITY,
		    FFEINFO_whereCONSTANT,
		    FFETARGET_charactersizeNONE));

  if ((error != FFEBAD)
      && ffebad_start (error))
    {
      ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
      ffebad_finish ();
    }

  return expr;
}

/* ffeexpr_collapse_add -- Collapse add expr

   ffebld expr;
   ffelexToken token;
   expr = ffeexpr_collapse_add(expr,token);

   If the result of the expr is a constant, replaces the expr with the
   computed constant.  */

ffebld
ffeexpr_collapse_add (ffebld expr, ffelexToken t)
{
  ffebad error = FFEBAD;
  ffebld l;
  ffebld r;
  ffebldConstantUnion u;
  ffeinfoBasictype bt;
  ffeinfoKindtype kt;

  if (ffeinfo_where (ffebld_info (expr)) != FFEINFO_whereCONSTANT)
    return expr;

  l = ffebld_left (expr);
  r = ffebld_right (expr);

  if (ffebld_op (l) != FFEBLD_opCONTER)
    return expr;
  if (ffebld_op (r) != FFEBLD_opCONTER)
    return expr;

  switch (bt = ffeinfo_basictype (ffebld_info (expr)))
    {
    case FFEINFO_basictypeANY:
      return expr;

    case FFEINFO_basictypeINTEGER:
      switch (kt = ffeinfo_kindtype (ffebld_info (expr)))
	{
#if FFETARGET_okINTEGER1
	case FFEINFO_kindtypeINTEGER1:
	  error = ffetarget_add_integer1 (ffebld_cu_ptr_integer1 (u),
			       ffebld_constant_integer1 (ffebld_conter (l)),
			      ffebld_constant_integer1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer1_val
					(ffebld_cu_val_integer1 (u)), expr);
	  break;
#endif

#if FFETARGET_okINTEGER2
	case FFEINFO_kindtypeINTEGER2:
	  error = ffetarget_add_integer2 (ffebld_cu_ptr_integer2 (u),
			       ffebld_constant_integer2 (ffebld_conter (l)),
			      ffebld_constant_integer2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer2_val
					(ffebld_cu_val_integer2 (u)), expr);
	  break;
#endif

#if FFETARGET_okINTEGER3
	case FFEINFO_kindtypeINTEGER3:
	  error = ffetarget_add_integer3 (ffebld_cu_ptr_integer3 (u),
			       ffebld_constant_integer3 (ffebld_conter (l)),
			      ffebld_constant_integer3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer3_val
					(ffebld_cu_val_integer3 (u)), expr);
	  break;
#endif

#if FFETARGET_okINTEGER4
	case FFEINFO_kindtypeINTEGER4:
	  error = ffetarget_add_integer4 (ffebld_cu_ptr_integer4 (u),
			       ffebld_constant_integer4 (ffebld_conter (l)),
			      ffebld_constant_integer4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer4_val
					(ffebld_cu_val_integer4 (u)), expr);
	  break;
#endif

	default:
	  assert ("bad integer kind type" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeREAL:
      switch (kt = ffeinfo_kindtype (ffebld_info (expr)))
	{
#if FFETARGET_okREAL1
	case FFEINFO_kindtypeREAL1:
	  error = ffetarget_add_real1 (ffebld_cu_ptr_real1 (u),
				  ffebld_constant_real1 (ffebld_conter (l)),
				 ffebld_constant_real1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_real1_val
					   (ffebld_cu_val_real1 (u)), expr);
	  break;
#endif

#if FFETARGET_okREAL2
	case FFEINFO_kindtypeREAL2:
	  error = ffetarget_add_real2 (ffebld_cu_ptr_real2 (u),
				  ffebld_constant_real2 (ffebld_conter (l)),
				 ffebld_constant_real2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_real2_val
					   (ffebld_cu_val_real2 (u)), expr);
	  break;
#endif

#if FFETARGET_okREAL3
	case FFEINFO_kindtypeREAL3:
	  error = ffetarget_add_real3 (ffebld_cu_ptr_real3 (u),
				  ffebld_constant_real3 (ffebld_conter (l)),
				 ffebld_constant_real3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_real3_val
					   (ffebld_cu_val_real3 (u)), expr);
	  break;
#endif

#if FFETARGET_okREAL4
	case FFEINFO_kindtypeREAL4:
	  error = ffetarget_add_real4 (ffebld_cu_ptr_real4 (u),
				  ffebld_constant_real4 (ffebld_conter (l)),
				 ffebld_constant_real4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_real4_val
					   (ffebld_cu_val_real4 (u)), expr);
	  break;
#endif

	default:
	  assert ("bad real kind type" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeCOMPLEX:
      switch (kt = ffeinfo_kindtype (ffebld_info (expr)))
	{
#if FFETARGET_okCOMPLEX1
	case FFEINFO_kindtypeREAL1:
	  error = ffetarget_add_complex1 (ffebld_cu_ptr_complex1 (u),
			       ffebld_constant_complex1 (ffebld_conter (l)),
			      ffebld_constant_complex1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_complex1_val
					(ffebld_cu_val_complex1 (u)), expr);
	  break;
#endif

#if FFETARGET_okCOMPLEX2
	case FFEINFO_kindtypeREAL2:
	  error = ffetarget_add_complex2 (ffebld_cu_ptr_complex2 (u),
			       ffebld_constant_complex2 (ffebld_conter (l)),
			      ffebld_constant_complex2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_complex2_val
					(ffebld_cu_val_complex2 (u)), expr);
	  break;
#endif

#if FFETARGET_okCOMPLEX3
	case FFEINFO_kindtypeREAL3:
	  error = ffetarget_add_complex3 (ffebld_cu_ptr_complex3 (u),
			       ffebld_constant_complex3 (ffebld_conter (l)),
			      ffebld_constant_complex3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_complex3_val
					(ffebld_cu_val_complex3 (u)), expr);
	  break;
#endif

#if FFETARGET_okCOMPLEX4
	case FFEINFO_kindtypeREAL4:
	  error = ffetarget_add_complex4 (ffebld_cu_ptr_complex4 (u),
			       ffebld_constant_complex4 (ffebld_conter (l)),
			      ffebld_constant_complex4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_complex4_val
					(ffebld_cu_val_complex4 (u)), expr);
	  break;
#endif

	default:
	  assert ("bad complex kind type" == NULL);
	  break;
	}
      break;

    default:
      assert ("bad type" == NULL);
      return expr;
    }

  ffebld_set_info (expr, ffeinfo_new
		   (bt,
		    kt,
		    0,
		    FFEINFO_kindENTITY,
		    FFEINFO_whereCONSTANT,
		    FFETARGET_charactersizeNONE));

  if ((error != FFEBAD)
      && ffebad_start (error))
    {
      ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
      ffebad_finish ();
    }

  return expr;
}

/* ffeexpr_collapse_subtract -- Collapse subtract expr

   ffebld expr;
   ffelexToken token;
   expr = ffeexpr_collapse_subtract(expr,token);

   If the result of the expr is a constant, replaces the expr with the
   computed constant.  */

ffebld
ffeexpr_collapse_subtract (ffebld expr, ffelexToken t)
{
  ffebad error = FFEBAD;
  ffebld l;
  ffebld r;
  ffebldConstantUnion u;
  ffeinfoBasictype bt;
  ffeinfoKindtype kt;

  if (ffeinfo_where (ffebld_info (expr)) != FFEINFO_whereCONSTANT)
    return expr;

  l = ffebld_left (expr);
  r = ffebld_right (expr);

  if (ffebld_op (l) != FFEBLD_opCONTER)
    return expr;
  if (ffebld_op (r) != FFEBLD_opCONTER)
    return expr;

  switch (bt = ffeinfo_basictype (ffebld_info (expr)))
    {
    case FFEINFO_basictypeANY:
      return expr;

    case FFEINFO_basictypeINTEGER:
      switch (kt = ffeinfo_kindtype (ffebld_info (expr)))
	{
#if FFETARGET_okINTEGER1
	case FFEINFO_kindtypeINTEGER1:
	  error = ffetarget_subtract_integer1 (ffebld_cu_ptr_integer1 (u),
			       ffebld_constant_integer1 (ffebld_conter (l)),
			      ffebld_constant_integer1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer1_val
					(ffebld_cu_val_integer1 (u)), expr);
	  break;
#endif

#if FFETARGET_okINTEGER2
	case FFEINFO_kindtypeINTEGER2:
	  error = ffetarget_subtract_integer2 (ffebld_cu_ptr_integer2 (u),
			       ffebld_constant_integer2 (ffebld_conter (l)),
			      ffebld_constant_integer2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer2_val
					(ffebld_cu_val_integer2 (u)), expr);
	  break;
#endif

#if FFETARGET_okINTEGER3
	case FFEINFO_kindtypeINTEGER3:
	  error = ffetarget_subtract_integer3 (ffebld_cu_ptr_integer3 (u),
			       ffebld_constant_integer3 (ffebld_conter (l)),
			      ffebld_constant_integer3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer3_val
					(ffebld_cu_val_integer3 (u)), expr);
	  break;
#endif

#if FFETARGET_okINTEGER4
	case FFEINFO_kindtypeINTEGER4:
	  error = ffetarget_subtract_integer4 (ffebld_cu_ptr_integer4 (u),
			       ffebld_constant_integer4 (ffebld_conter (l)),
			      ffebld_constant_integer4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer4_val
					(ffebld_cu_val_integer4 (u)), expr);
	  break;
#endif

	default:
	  assert ("bad integer kind type" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeREAL:
      switch (kt = ffeinfo_kindtype (ffebld_info (expr)))
	{
#if FFETARGET_okREAL1
	case FFEINFO_kindtypeREAL1:
	  error = ffetarget_subtract_real1 (ffebld_cu_ptr_real1 (u),
				  ffebld_constant_real1 (ffebld_conter (l)),
				 ffebld_constant_real1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_real1_val
					   (ffebld_cu_val_real1 (u)), expr);
	  break;
#endif

#if FFETARGET_okREAL2
	case FFEINFO_kindtypeREAL2:
	  error = ffetarget_subtract_real2 (ffebld_cu_ptr_real2 (u),
				  ffebld_constant_real2 (ffebld_conter (l)),
				 ffebld_constant_real2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_real2_val
					   (ffebld_cu_val_real2 (u)), expr);
	  break;
#endif

#if FFETARGET_okREAL3
	case FFEINFO_kindtypeREAL3:
	  error = ffetarget_subtract_real3 (ffebld_cu_ptr_real3 (u),
				  ffebld_constant_real3 (ffebld_conter (l)),
				 ffebld_constant_real3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_real3_val
					   (ffebld_cu_val_real3 (u)), expr);
	  break;
#endif

#if FFETARGET_okREAL4
	case FFEINFO_kindtypeREAL4:
	  error = ffetarget_subtract_real4 (ffebld_cu_ptr_real4 (u),
				  ffebld_constant_real4 (ffebld_conter (l)),
				 ffebld_constant_real4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_real4_val
					   (ffebld_cu_val_real4 (u)), expr);
	  break;
#endif

	default:
	  assert ("bad real kind type" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeCOMPLEX:
      switch (kt = ffeinfo_kindtype (ffebld_info (expr)))
	{
#if FFETARGET_okCOMPLEX1
	case FFEINFO_kindtypeREAL1:
	  error = ffetarget_subtract_complex1 (ffebld_cu_ptr_complex1 (u),
			       ffebld_constant_complex1 (ffebld_conter (l)),
			      ffebld_constant_complex1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_complex1_val
					(ffebld_cu_val_complex1 (u)), expr);
	  break;
#endif

#if FFETARGET_okCOMPLEX2
	case FFEINFO_kindtypeREAL2:
	  error = ffetarget_subtract_complex2 (ffebld_cu_ptr_complex2 (u),
			       ffebld_constant_complex2 (ffebld_conter (l)),
			      ffebld_constant_complex2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_complex2_val
					(ffebld_cu_val_complex2 (u)), expr);
	  break;
#endif

#if FFETARGET_okCOMPLEX3
	case FFEINFO_kindtypeREAL3:
	  error = ffetarget_subtract_complex3 (ffebld_cu_ptr_complex3 (u),
			       ffebld_constant_complex3 (ffebld_conter (l)),
			      ffebld_constant_complex3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_complex3_val
					(ffebld_cu_val_complex3 (u)), expr);
	  break;
#endif

#if FFETARGET_okCOMPLEX4
	case FFEINFO_kindtypeREAL4:
	  error = ffetarget_subtract_complex4 (ffebld_cu_ptr_complex4 (u),
			       ffebld_constant_complex4 (ffebld_conter (l)),
			      ffebld_constant_complex4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_complex4_val
					(ffebld_cu_val_complex4 (u)), expr);
	  break;
#endif

	default:
	  assert ("bad complex kind type" == NULL);
	  break;
	}
      break;

    default:
      assert ("bad type" == NULL);
      return expr;
    }

  ffebld_set_info (expr, ffeinfo_new
		   (bt,
		    kt,
		    0,
		    FFEINFO_kindENTITY,
		    FFEINFO_whereCONSTANT,
		    FFETARGET_charactersizeNONE));

  if ((error != FFEBAD)
      && ffebad_start (error))
    {
      ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
      ffebad_finish ();
    }

  return expr;
}

/* ffeexpr_collapse_multiply -- Collapse multiply expr

   ffebld expr;
   ffelexToken token;
   expr = ffeexpr_collapse_multiply(expr,token);

   If the result of the expr is a constant, replaces the expr with the
   computed constant.  */

ffebld
ffeexpr_collapse_multiply (ffebld expr, ffelexToken t)
{
  ffebad error = FFEBAD;
  ffebld l;
  ffebld r;
  ffebldConstantUnion u;
  ffeinfoBasictype bt;
  ffeinfoKindtype kt;

  if (ffeinfo_where (ffebld_info (expr)) != FFEINFO_whereCONSTANT)
    return expr;

  l = ffebld_left (expr);
  r = ffebld_right (expr);

  if (ffebld_op (l) != FFEBLD_opCONTER)
    return expr;
  if (ffebld_op (r) != FFEBLD_opCONTER)
    return expr;

  switch (bt = ffeinfo_basictype (ffebld_info (expr)))
    {
    case FFEINFO_basictypeANY:
      return expr;

    case FFEINFO_basictypeINTEGER:
      switch (kt = ffeinfo_kindtype (ffebld_info (expr)))
	{
#if FFETARGET_okINTEGER1
	case FFEINFO_kindtypeINTEGER1:
	  error = ffetarget_multiply_integer1 (ffebld_cu_ptr_integer1 (u),
			       ffebld_constant_integer1 (ffebld_conter (l)),
			      ffebld_constant_integer1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer1_val
					(ffebld_cu_val_integer1 (u)), expr);
	  break;
#endif

#if FFETARGET_okINTEGER2
	case FFEINFO_kindtypeINTEGER2:
	  error = ffetarget_multiply_integer2 (ffebld_cu_ptr_integer2 (u),
			       ffebld_constant_integer2 (ffebld_conter (l)),
			      ffebld_constant_integer2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer2_val
					(ffebld_cu_val_integer2 (u)), expr);
	  break;
#endif

#if FFETARGET_okINTEGER3
	case FFEINFO_kindtypeINTEGER3:
	  error = ffetarget_multiply_integer3 (ffebld_cu_ptr_integer3 (u),
			       ffebld_constant_integer3 (ffebld_conter (l)),
			      ffebld_constant_integer3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer3_val
					(ffebld_cu_val_integer3 (u)), expr);
	  break;
#endif

#if FFETARGET_okINTEGER4
	case FFEINFO_kindtypeINTEGER4:
	  error = ffetarget_multiply_integer4 (ffebld_cu_ptr_integer4 (u),
			       ffebld_constant_integer4 (ffebld_conter (l)),
			      ffebld_constant_integer4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer4_val
					(ffebld_cu_val_integer4 (u)), expr);
	  break;
#endif

	default:
	  assert ("bad integer kind type" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeREAL:
      switch (kt = ffeinfo_kindtype (ffebld_info (expr)))
	{
#if FFETARGET_okREAL1
	case FFEINFO_kindtypeREAL1:
	  error = ffetarget_multiply_real1 (ffebld_cu_ptr_real1 (u),
				  ffebld_constant_real1 (ffebld_conter (l)),
				 ffebld_constant_real1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_real1_val
					   (ffebld_cu_val_real1 (u)), expr);
	  break;
#endif

#if FFETARGET_okREAL2
	case FFEINFO_kindtypeREAL2:
	  error = ffetarget_multiply_real2 (ffebld_cu_ptr_real2 (u),
				  ffebld_constant_real2 (ffebld_conter (l)),
				 ffebld_constant_real2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_real2_val
					   (ffebld_cu_val_real2 (u)), expr);
	  break;
#endif

#if FFETARGET_okREAL3
	case FFEINFO_kindtypeREAL3:
	  error = ffetarget_multiply_real3 (ffebld_cu_ptr_real3 (u),
				  ffebld_constant_real3 (ffebld_conter (l)),
				 ffebld_constant_real3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_real3_val
					   (ffebld_cu_val_real3 (u)), expr);
	  break;
#endif

#if FFETARGET_okREAL4
	case FFEINFO_kindtypeREAL4:
	  error = ffetarget_multiply_real4 (ffebld_cu_ptr_real4 (u),
				  ffebld_constant_real4 (ffebld_conter (l)),
				 ffebld_constant_real4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_real4_val
					   (ffebld_cu_val_real4 (u)), expr);
	  break;
#endif

	default:
	  assert ("bad real kind type" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeCOMPLEX:
      switch (kt = ffeinfo_kindtype (ffebld_info (expr)))
	{
#if FFETARGET_okCOMPLEX1
	case FFEINFO_kindtypeREAL1:
	  error = ffetarget_multiply_complex1 (ffebld_cu_ptr_complex1 (u),
			       ffebld_constant_complex1 (ffebld_conter (l)),
			      ffebld_constant_complex1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_complex1_val
					(ffebld_cu_val_complex1 (u)), expr);
	  break;
#endif

#if FFETARGET_okCOMPLEX2
	case FFEINFO_kindtypeREAL2:
	  error = ffetarget_multiply_complex2 (ffebld_cu_ptr_complex2 (u),
			       ffebld_constant_complex2 (ffebld_conter (l)),
			      ffebld_constant_complex2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_complex2_val
					(ffebld_cu_val_complex2 (u)), expr);
	  break;
#endif

#if FFETARGET_okCOMPLEX3
	case FFEINFO_kindtypeREAL3:
	  error = ffetarget_multiply_complex3 (ffebld_cu_ptr_complex3 (u),
			       ffebld_constant_complex3 (ffebld_conter (l)),
			      ffebld_constant_complex3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_complex3_val
					(ffebld_cu_val_complex3 (u)), expr);
	  break;
#endif

#if FFETARGET_okCOMPLEX4
	case FFEINFO_kindtypeREAL4:
	  error = ffetarget_multiply_complex4 (ffebld_cu_ptr_complex4 (u),
			       ffebld_constant_complex4 (ffebld_conter (l)),
			      ffebld_constant_complex4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_complex4_val
					(ffebld_cu_val_complex4 (u)), expr);
	  break;
#endif

	default:
	  assert ("bad complex kind type" == NULL);
	  break;
	}
      break;

    default:
      assert ("bad type" == NULL);
      return expr;
    }

  ffebld_set_info (expr, ffeinfo_new
		   (bt,
		    kt,
		    0,
		    FFEINFO_kindENTITY,
		    FFEINFO_whereCONSTANT,
		    FFETARGET_charactersizeNONE));

  if ((error != FFEBAD)
      && ffebad_start (error))
    {
      ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
      ffebad_finish ();
    }

  return expr;
}

/* ffeexpr_collapse_divide -- Collapse divide expr

   ffebld expr;
   ffelexToken token;
   expr = ffeexpr_collapse_divide(expr,token);

   If the result of the expr is a constant, replaces the expr with the
   computed constant.  */

ffebld
ffeexpr_collapse_divide (ffebld expr, ffelexToken t)
{
  ffebad error = FFEBAD;
  ffebld l;
  ffebld r;
  ffebldConstantUnion u;
  ffeinfoBasictype bt;
  ffeinfoKindtype kt;

  if (ffeinfo_where (ffebld_info (expr)) != FFEINFO_whereCONSTANT)
    return expr;

  l = ffebld_left (expr);
  r = ffebld_right (expr);

  if (ffebld_op (l) != FFEBLD_opCONTER)
    return expr;
  if (ffebld_op (r) != FFEBLD_opCONTER)
    return expr;

  switch (bt = ffeinfo_basictype (ffebld_info (expr)))
    {
    case FFEINFO_basictypeANY:
      return expr;

    case FFEINFO_basictypeINTEGER:
      switch (kt = ffeinfo_kindtype (ffebld_info (expr)))
	{
#if FFETARGET_okINTEGER1
	case FFEINFO_kindtypeINTEGER1:
	  error = ffetarget_divide_integer1 (ffebld_cu_ptr_integer1 (u),
			       ffebld_constant_integer1 (ffebld_conter (l)),
			      ffebld_constant_integer1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer1_val
					(ffebld_cu_val_integer1 (u)), expr);
	  break;
#endif

#if FFETARGET_okINTEGER2
	case FFEINFO_kindtypeINTEGER2:
	  error = ffetarget_divide_integer2 (ffebld_cu_ptr_integer2 (u),
			       ffebld_constant_integer2 (ffebld_conter (l)),
			      ffebld_constant_integer2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer2_val
					(ffebld_cu_val_integer2 (u)), expr);
	  break;
#endif

#if FFETARGET_okINTEGER3
	case FFEINFO_kindtypeINTEGER3:
	  error = ffetarget_divide_integer3 (ffebld_cu_ptr_integer3 (u),
			       ffebld_constant_integer3 (ffebld_conter (l)),
			      ffebld_constant_integer3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer3_val
					(ffebld_cu_val_integer3 (u)), expr);
	  break;
#endif

#if FFETARGET_okINTEGER4
	case FFEINFO_kindtypeINTEGER4:
	  error = ffetarget_divide_integer4 (ffebld_cu_ptr_integer4 (u),
			       ffebld_constant_integer4 (ffebld_conter (l)),
			      ffebld_constant_integer4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer4_val
					(ffebld_cu_val_integer4 (u)), expr);
	  break;
#endif

	default:
	  assert ("bad integer kind type" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeREAL:
      switch (kt = ffeinfo_kindtype (ffebld_info (expr)))
	{
#if FFETARGET_okREAL1
	case FFEINFO_kindtypeREAL1:
	  error = ffetarget_divide_real1 (ffebld_cu_ptr_real1 (u),
				  ffebld_constant_real1 (ffebld_conter (l)),
				 ffebld_constant_real1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_real1_val
					   (ffebld_cu_val_real1 (u)), expr);
	  break;
#endif

#if FFETARGET_okREAL2
	case FFEINFO_kindtypeREAL2:
	  error = ffetarget_divide_real2 (ffebld_cu_ptr_real2 (u),
				  ffebld_constant_real2 (ffebld_conter (l)),
				 ffebld_constant_real2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_real2_val
					   (ffebld_cu_val_real2 (u)), expr);
	  break;
#endif

#if FFETARGET_okREAL3
	case FFEINFO_kindtypeREAL3:
	  error = ffetarget_divide_real3 (ffebld_cu_ptr_real3 (u),
				  ffebld_constant_real3 (ffebld_conter (l)),
				 ffebld_constant_real3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_real3_val
					   (ffebld_cu_val_real3 (u)), expr);
	  break;
#endif

#if FFETARGET_okREAL4
	case FFEINFO_kindtypeREAL4:
	  error = ffetarget_divide_real4 (ffebld_cu_ptr_real4 (u),
				  ffebld_constant_real4 (ffebld_conter (l)),
				 ffebld_constant_real4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_real4_val
					   (ffebld_cu_val_real4 (u)), expr);
	  break;
#endif

	default:
	  assert ("bad real kind type" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeCOMPLEX:
      switch (kt = ffeinfo_kindtype (ffebld_info (expr)))
	{
#if FFETARGET_okCOMPLEX1
	case FFEINFO_kindtypeREAL1:
	  error = ffetarget_divide_complex1 (ffebld_cu_ptr_complex1 (u),
			       ffebld_constant_complex1 (ffebld_conter (l)),
			      ffebld_constant_complex1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_complex1_val
					(ffebld_cu_val_complex1 (u)), expr);
	  break;
#endif

#if FFETARGET_okCOMPLEX2
	case FFEINFO_kindtypeREAL2:
	  error = ffetarget_divide_complex2 (ffebld_cu_ptr_complex2 (u),
			       ffebld_constant_complex2 (ffebld_conter (l)),
			      ffebld_constant_complex2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_complex2_val
					(ffebld_cu_val_complex2 (u)), expr);
	  break;
#endif

#if FFETARGET_okCOMPLEX3
	case FFEINFO_kindtypeREAL3:
	  error = ffetarget_divide_complex3 (ffebld_cu_ptr_complex3 (u),
			       ffebld_constant_complex3 (ffebld_conter (l)),
			      ffebld_constant_complex3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_complex3_val
					(ffebld_cu_val_complex3 (u)), expr);
	  break;
#endif

#if FFETARGET_okCOMPLEX4
	case FFEINFO_kindtypeREAL4:
	  error = ffetarget_divide_complex4 (ffebld_cu_ptr_complex4 (u),
			       ffebld_constant_complex4 (ffebld_conter (l)),
			      ffebld_constant_complex4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_complex4_val
					(ffebld_cu_val_complex4 (u)), expr);
	  break;
#endif

	default:
	  assert ("bad complex kind type" == NULL);
	  break;
	}
      break;

    default:
      assert ("bad type" == NULL);
      return expr;
    }

  ffebld_set_info (expr, ffeinfo_new
		   (bt,
		    kt,
		    0,
		    FFEINFO_kindENTITY,
		    FFEINFO_whereCONSTANT,
		    FFETARGET_charactersizeNONE));

  if ((error != FFEBAD)
      && ffebad_start (error))
    {
      ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
      ffebad_finish ();
    }

  return expr;
}

/* ffeexpr_collapse_power -- Collapse power expr

   ffebld expr;
   ffelexToken token;
   expr = ffeexpr_collapse_power(expr,token);

   If the result of the expr is a constant, replaces the expr with the
   computed constant.  */

ffebld
ffeexpr_collapse_power (ffebld expr, ffelexToken t)
{
  ffebad error = FFEBAD;
  ffebld l;
  ffebld r;
  ffebldConstantUnion u;
  ffeinfoBasictype bt;
  ffeinfoKindtype kt;

  if (ffeinfo_where (ffebld_info (expr)) != FFEINFO_whereCONSTANT)
    return expr;

  l = ffebld_left (expr);
  r = ffebld_right (expr);

  if (ffebld_op (l) != FFEBLD_opCONTER)
    return expr;
  if (ffebld_op (r) != FFEBLD_opCONTER)
    return expr;

  if ((ffeinfo_basictype (ffebld_info (r)) != FFEINFO_basictypeINTEGER)
  || (ffeinfo_kindtype (ffebld_info (r)) != FFEINFO_kindtypeINTEGERDEFAULT))
    return expr;

  switch (bt = ffeinfo_basictype (ffebld_info (expr)))
    {
    case FFEINFO_basictypeANY:
      return expr;

    case FFEINFO_basictypeINTEGER:
      switch (kt = ffeinfo_kindtype (ffebld_info (expr)))
	{
	case FFEINFO_kindtypeINTEGERDEFAULT:
	  error = ffetarget_power_integerdefault_integerdefault
	    (ffebld_cu_ptr_integerdefault (u),
	     ffebld_constant_integerdefault (ffebld_conter (l)),
	     ffebld_constant_integerdefault (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_integerdefault_val
	     (ffebld_cu_val_integerdefault (u)), expr);
	  break;

	default:
	  assert ("bad integer kind type" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeREAL:
      switch (kt = ffeinfo_kindtype (ffebld_info (expr)))
	{
	case FFEINFO_kindtypeREALDEFAULT:
	  error = ffetarget_power_realdefault_integerdefault
	    (ffebld_cu_ptr_realdefault (u),
	     ffebld_constant_realdefault (ffebld_conter (l)),
	     ffebld_constant_integerdefault (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_realdefault_val
	     (ffebld_cu_val_realdefault (u)), expr);
	  break;

	case FFEINFO_kindtypeREALDOUBLE:
	  error = ffetarget_power_realdouble_integerdefault
	    (ffebld_cu_ptr_realdouble (u),
	     ffebld_constant_realdouble (ffebld_conter (l)),
	     ffebld_constant_integerdefault (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_realdouble_val
	     (ffebld_cu_val_realdouble (u)), expr);
	  break;

#if FFETARGET_okREALQUAD
	case FFEINFO_kindtypeREALQUAD:
	  error = ffetarget_power_realquad_integerdefault
	    (ffebld_cu_ptr_realquad (u),
	     ffebld_constant_realquad (ffebld_conter (l)),
	     ffebld_constant_integerdefault (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_realquad_val
	     (ffebld_cu_val_realquad (u)), expr);
	  break;
#endif
	default:
	  assert ("bad real kind type" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeCOMPLEX:
      switch (kt = ffeinfo_kindtype (ffebld_info (expr)))
	{
	case FFEINFO_kindtypeREALDEFAULT:
	  error = ffetarget_power_complexdefault_integerdefault
	    (ffebld_cu_ptr_complexdefault (u),
	     ffebld_constant_complexdefault (ffebld_conter (l)),
	     ffebld_constant_integerdefault (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_complexdefault_val
	     (ffebld_cu_val_complexdefault (u)), expr);
	  break;

#if FFETARGET_okCOMPLEXDOUBLE
	case FFEINFO_kindtypeREALDOUBLE:
	  error = ffetarget_power_complexdouble_integerdefault
	    (ffebld_cu_ptr_complexdouble (u),
	     ffebld_constant_complexdouble (ffebld_conter (l)),
	     ffebld_constant_integerdefault (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_complexdouble_val
	     (ffebld_cu_val_complexdouble (u)), expr);
	  break;
#endif

#if FFETARGET_okCOMPLEXQUAD
	case FFEINFO_kindtypeREALQUAD:
	  error = ffetarget_power_complexquad_integerdefault
	    (ffebld_cu_ptr_complexquad (u),
	     ffebld_constant_complexquad (ffebld_conter (l)),
	     ffebld_constant_integerdefault (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_complexquad_val
	     (ffebld_cu_val_complexquad (u)), expr);
	  break;
#endif

	default:
	  assert ("bad complex kind type" == NULL);
	  break;
	}
      break;

    default:
      assert ("bad type" == NULL);
      return expr;
    }

  ffebld_set_info (expr, ffeinfo_new
		   (bt,
		    kt,
		    0,
		    FFEINFO_kindENTITY,
		    FFEINFO_whereCONSTANT,
		    FFETARGET_charactersizeNONE));

  if ((error != FFEBAD)
      && ffebad_start (error))
    {
      ffebad_here (0, ffelex_token_where_line (t),
		   ffelex_token_where_column (t));
      ffebad_finish ();
    }

  return expr;
}

/* ffeexpr_collapse_concatenate -- Collapse concatenate expr

   ffebld expr;
   ffelexToken token;
   expr = ffeexpr_collapse_concatenate(expr,token);

   If the result of the expr is a constant, replaces the expr with the
   computed constant.  */

ffebld
ffeexpr_collapse_concatenate (ffebld expr, ffelexToken t)
{
  ffebad error = FFEBAD;
  ffebld l;
  ffebld r;
  ffebldConstantUnion u;
  ffeinfoKindtype kt;
  ffetargetCharacterSize len;

  if (ffeinfo_where (ffebld_info (expr)) != FFEINFO_whereCONSTANT)
    return expr;

  l = ffebld_left (expr);
  r = ffebld_right (expr);

  if (ffebld_op (l) != FFEBLD_opCONTER)
    return expr;
  if (ffebld_op (r) != FFEBLD_opCONTER)
    return expr;

  switch (ffeinfo_basictype (ffebld_info (expr)))
    {
    case FFEINFO_basictypeANY:
      return expr;

    case FFEINFO_basictypeCHARACTER:
      switch (kt = ffeinfo_kindtype (ffebld_info (expr)))
	{
#if FFETARGET_okCHARACTER1
	case FFEINFO_kindtypeCHARACTER1:
	  error = ffetarget_concatenate_character1 (ffebld_cu_ptr_character1 (u),
			     ffebld_constant_character1 (ffebld_conter (l)),
			     ffebld_constant_character1 (ffebld_conter (r)),
				   ffebld_constant_pool (), &len);
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_character1_val
				      (ffebld_cu_val_character1 (u)), expr);
	  break;
#endif

#if FFETARGET_okCHARACTER2
	case FFEINFO_kindtypeCHARACTER2:
	  error = ffetarget_concatenate_character2 (ffebld_cu_ptr_character2 (u),
			     ffebld_constant_character2 (ffebld_conter (l)),
			     ffebld_constant_character2 (ffebld_conter (r)),
				   ffebld_constant_pool (), &len);
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_character2_val
				      (ffebld_cu_val_character2 (u)), expr);
	  break;
#endif

#if FFETARGET_okCHARACTER3
	case FFEINFO_kindtypeCHARACTER3:
	  error = ffetarget_concatenate_character3 (ffebld_cu_ptr_character3 (u),
			     ffebld_constant_character3 (ffebld_conter (l)),
			     ffebld_constant_character3 (ffebld_conter (r)),
				   ffebld_constant_pool (), &len);
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_character3_val
				      (ffebld_cu_val_character3 (u)), expr);
	  break;
#endif

#if FFETARGET_okCHARACTER4
	case FFEINFO_kindtypeCHARACTER4:
	  error = ffetarget_concatenate_character4 (ffebld_cu_ptr_character4 (u),
			     ffebld_constant_character4 (ffebld_conter (l)),
			     ffebld_constant_character4 (ffebld_conter (r)),
				   ffebld_constant_pool (), &len);
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_character4_val
				      (ffebld_cu_val_character4 (u)), expr);
	  break;
#endif

	default:
	  assert ("bad character kind type" == NULL);
	  break;
	}
      break;

    default:
      assert ("bad type" == NULL);
      return expr;
    }

  ffebld_set_info (expr, ffeinfo_new
		   (FFEINFO_basictypeCHARACTER,
		    kt,
		    0,
		    FFEINFO_kindENTITY,
		    FFEINFO_whereCONSTANT,
		    len));

  if ((error != FFEBAD)
      && ffebad_start (error))
    {
      ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
      ffebad_finish ();
    }

  return expr;
}

/* ffeexpr_collapse_eq -- Collapse eq expr

   ffebld expr;
   ffelexToken token;
   expr = ffeexpr_collapse_eq(expr,token);

   If the result of the expr is a constant, replaces the expr with the
   computed constant.  */

ffebld
ffeexpr_collapse_eq (ffebld expr, ffelexToken t)
{
  ffebad error = FFEBAD;
  ffebld l;
  ffebld r;
  bool val;

  if (ffeinfo_where (ffebld_info (expr)) != FFEINFO_whereCONSTANT)
    return expr;

  l = ffebld_left (expr);
  r = ffebld_right (expr);

  if (ffebld_op (l) != FFEBLD_opCONTER)
    return expr;
  if (ffebld_op (r) != FFEBLD_opCONTER)
    return expr;

  switch (ffeinfo_basictype (ffebld_info (ffebld_left (expr))))
    {
    case FFEINFO_basictypeANY:
      return expr;

    case FFEINFO_basictypeINTEGER:
      switch (ffeinfo_kindtype (ffebld_info (ffebld_left (expr))))
	{
#if FFETARGET_okINTEGER1
	case FFEINFO_kindtypeINTEGER1:
	  error = ffetarget_eq_integer1 (&val,
			       ffebld_constant_integer1 (ffebld_conter (l)),
			      ffebld_constant_integer1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okINTEGER2
	case FFEINFO_kindtypeINTEGER2:
	  error = ffetarget_eq_integer2 (&val,
			       ffebld_constant_integer2 (ffebld_conter (l)),
			      ffebld_constant_integer2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okINTEGER3
	case FFEINFO_kindtypeINTEGER3:
	  error = ffetarget_eq_integer3 (&val,
			       ffebld_constant_integer3 (ffebld_conter (l)),
			      ffebld_constant_integer3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okINTEGER4
	case FFEINFO_kindtypeINTEGER4:
	  error = ffetarget_eq_integer4 (&val,
			       ffebld_constant_integer4 (ffebld_conter (l)),
			      ffebld_constant_integer4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

	default:
	  assert ("bad integer kind type" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeREAL:
      switch (ffeinfo_kindtype (ffebld_info (ffebld_left (expr))))
	{
#if FFETARGET_okREAL1
	case FFEINFO_kindtypeREAL1:
	  error = ffetarget_eq_real1 (&val,
				  ffebld_constant_real1 (ffebld_conter (l)),
				 ffebld_constant_real1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okREAL2
	case FFEINFO_kindtypeREAL2:
	  error = ffetarget_eq_real2 (&val,
				  ffebld_constant_real2 (ffebld_conter (l)),
				 ffebld_constant_real2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okREAL3
	case FFEINFO_kindtypeREAL3:
	  error = ffetarget_eq_real3 (&val,
				  ffebld_constant_real3 (ffebld_conter (l)),
				 ffebld_constant_real3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okREAL4
	case FFEINFO_kindtypeREAL4:
	  error = ffetarget_eq_real4 (&val,
				  ffebld_constant_real4 (ffebld_conter (l)),
				 ffebld_constant_real4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

	default:
	  assert ("bad real kind type" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeCOMPLEX:
      switch (ffeinfo_kindtype (ffebld_info (ffebld_left (expr))))
	{
#if FFETARGET_okCOMPLEX1
	case FFEINFO_kindtypeREAL1:
	  error = ffetarget_eq_complex1 (&val,
			       ffebld_constant_complex1 (ffebld_conter (l)),
			      ffebld_constant_complex1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okCOMPLEX2
	case FFEINFO_kindtypeREAL2:
	  error = ffetarget_eq_complex2 (&val,
			       ffebld_constant_complex2 (ffebld_conter (l)),
			      ffebld_constant_complex2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okCOMPLEX3
	case FFEINFO_kindtypeREAL3:
	  error = ffetarget_eq_complex3 (&val,
			       ffebld_constant_complex3 (ffebld_conter (l)),
			      ffebld_constant_complex3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okCOMPLEX4
	case FFEINFO_kindtypeREAL4:
	  error = ffetarget_eq_complex4 (&val,
			       ffebld_constant_complex4 (ffebld_conter (l)),
			      ffebld_constant_complex4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

	default:
	  assert ("bad complex kind type" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeCHARACTER:
      switch (ffeinfo_kindtype (ffebld_info (ffebld_left (expr))))
	{
#if FFETARGET_okCHARACTER1
	case FFEINFO_kindtypeCHARACTER1:
	  error = ffetarget_eq_character1 (&val,
			     ffebld_constant_character1 (ffebld_conter (l)),
			    ffebld_constant_character1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okCHARACTER2
	case FFEINFO_kindtypeCHARACTER2:
	  error = ffetarget_eq_character2 (&val,
			     ffebld_constant_character2 (ffebld_conter (l)),
			    ffebld_constant_character2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okCHARACTER3
	case FFEINFO_kindtypeCHARACTER3:
	  error = ffetarget_eq_character3 (&val,
			     ffebld_constant_character3 (ffebld_conter (l)),
			    ffebld_constant_character3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okCHARACTER4
	case FFEINFO_kindtypeCHARACTER4:
	  error = ffetarget_eq_character4 (&val,
			     ffebld_constant_character4 (ffebld_conter (l)),
			    ffebld_constant_character4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

	default:
	  assert ("bad character kind type" == NULL);
	  break;
	}
      break;

    default:
      assert ("bad type" == NULL);
      return expr;
    }

  ffebld_set_info (expr, ffeinfo_new
		   (FFEINFO_basictypeLOGICAL,
		    FFEINFO_kindtypeLOGICALDEFAULT,
		    0,
		    FFEINFO_kindENTITY,
		    FFEINFO_whereCONSTANT,
		    FFETARGET_charactersizeNONE));

  if ((error != FFEBAD)
      && ffebad_start (error))
    {
      ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
      ffebad_finish ();
    }

  return expr;
}

/* ffeexpr_collapse_ne -- Collapse ne expr

   ffebld expr;
   ffelexToken token;
   expr = ffeexpr_collapse_ne(expr,token);

   If the result of the expr is a constant, replaces the expr with the
   computed constant.  */

ffebld
ffeexpr_collapse_ne (ffebld expr, ffelexToken t)
{
  ffebad error = FFEBAD;
  ffebld l;
  ffebld r;
  bool val;

  if (ffeinfo_where (ffebld_info (expr)) != FFEINFO_whereCONSTANT)
    return expr;

  l = ffebld_left (expr);
  r = ffebld_right (expr);

  if (ffebld_op (l) != FFEBLD_opCONTER)
    return expr;
  if (ffebld_op (r) != FFEBLD_opCONTER)
    return expr;

  switch (ffeinfo_basictype (ffebld_info (ffebld_left (expr))))
    {
    case FFEINFO_basictypeANY:
      return expr;

    case FFEINFO_basictypeINTEGER:
      switch (ffeinfo_kindtype (ffebld_info (ffebld_left (expr))))
	{
#if FFETARGET_okINTEGER1
	case FFEINFO_kindtypeINTEGER1:
	  error = ffetarget_ne_integer1 (&val,
			       ffebld_constant_integer1 (ffebld_conter (l)),
			      ffebld_constant_integer1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okINTEGER2
	case FFEINFO_kindtypeINTEGER2:
	  error = ffetarget_ne_integer2 (&val,
			       ffebld_constant_integer2 (ffebld_conter (l)),
			      ffebld_constant_integer2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okINTEGER3
	case FFEINFO_kindtypeINTEGER3:
	  error = ffetarget_ne_integer3 (&val,
			       ffebld_constant_integer3 (ffebld_conter (l)),
			      ffebld_constant_integer3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okINTEGER4
	case FFEINFO_kindtypeINTEGER4:
	  error = ffetarget_ne_integer4 (&val,
			       ffebld_constant_integer4 (ffebld_conter (l)),
			      ffebld_constant_integer4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

	default:
	  assert ("bad integer kind type" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeREAL:
      switch (ffeinfo_kindtype (ffebld_info (ffebld_left (expr))))
	{
#if FFETARGET_okREAL1
	case FFEINFO_kindtypeREAL1:
	  error = ffetarget_ne_real1 (&val,
				  ffebld_constant_real1 (ffebld_conter (l)),
				 ffebld_constant_real1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okREAL2
	case FFEINFO_kindtypeREAL2:
	  error = ffetarget_ne_real2 (&val,
				  ffebld_constant_real2 (ffebld_conter (l)),
				 ffebld_constant_real2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okREAL3
	case FFEINFO_kindtypeREAL3:
	  error = ffetarget_ne_real3 (&val,
				  ffebld_constant_real3 (ffebld_conter (l)),
				 ffebld_constant_real3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okREAL4
	case FFEINFO_kindtypeREAL4:
	  error = ffetarget_ne_real4 (&val,
				  ffebld_constant_real4 (ffebld_conter (l)),
				 ffebld_constant_real4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

	default:
	  assert ("bad real kind type" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeCOMPLEX:
      switch (ffeinfo_kindtype (ffebld_info (ffebld_left (expr))))
	{
#if FFETARGET_okCOMPLEX1
	case FFEINFO_kindtypeREAL1:
	  error = ffetarget_ne_complex1 (&val,
			       ffebld_constant_complex1 (ffebld_conter (l)),
			      ffebld_constant_complex1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okCOMPLEX2
	case FFEINFO_kindtypeREAL2:
	  error = ffetarget_ne_complex2 (&val,
			       ffebld_constant_complex2 (ffebld_conter (l)),
			      ffebld_constant_complex2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okCOMPLEX3
	case FFEINFO_kindtypeREAL3:
	  error = ffetarget_ne_complex3 (&val,
			       ffebld_constant_complex3 (ffebld_conter (l)),
			      ffebld_constant_complex3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okCOMPLEX4
	case FFEINFO_kindtypeREAL4:
	  error = ffetarget_ne_complex4 (&val,
			       ffebld_constant_complex4 (ffebld_conter (l)),
			      ffebld_constant_complex4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

	default:
	  assert ("bad complex kind type" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeCHARACTER:
      switch (ffeinfo_kindtype (ffebld_info (ffebld_left (expr))))
	{
#if FFETARGET_okCHARACTER1
	case FFEINFO_kindtypeCHARACTER1:
	  error = ffetarget_ne_character1 (&val,
			     ffebld_constant_character1 (ffebld_conter (l)),
			    ffebld_constant_character1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okCHARACTER2
	case FFEINFO_kindtypeCHARACTER2:
	  error = ffetarget_ne_character2 (&val,
			     ffebld_constant_character2 (ffebld_conter (l)),
			    ffebld_constant_character2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okCHARACTER3
	case FFEINFO_kindtypeCHARACTER3:
	  error = ffetarget_ne_character3 (&val,
			     ffebld_constant_character3 (ffebld_conter (l)),
			    ffebld_constant_character3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okCHARACTER4
	case FFEINFO_kindtypeCHARACTER4:
	  error = ffetarget_ne_character4 (&val,
			     ffebld_constant_character4 (ffebld_conter (l)),
			    ffebld_constant_character4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

	default:
	  assert ("bad character kind type" == NULL);
	  break;
	}
      break;

    default:
      assert ("bad type" == NULL);
      return expr;
    }

  ffebld_set_info (expr, ffeinfo_new
		   (FFEINFO_basictypeLOGICAL,
		    FFEINFO_kindtypeLOGICALDEFAULT,
		    0,
		    FFEINFO_kindENTITY,
		    FFEINFO_whereCONSTANT,
		    FFETARGET_charactersizeNONE));

  if ((error != FFEBAD)
      && ffebad_start (error))
    {
      ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
      ffebad_finish ();
    }

  return expr;
}

/* ffeexpr_collapse_ge -- Collapse ge expr

   ffebld expr;
   ffelexToken token;
   expr = ffeexpr_collapse_ge(expr,token);

   If the result of the expr is a constant, replaces the expr with the
   computed constant.  */

ffebld
ffeexpr_collapse_ge (ffebld expr, ffelexToken t)
{
  ffebad error = FFEBAD;
  ffebld l;
  ffebld r;
  bool val;

  if (ffeinfo_where (ffebld_info (expr)) != FFEINFO_whereCONSTANT)
    return expr;

  l = ffebld_left (expr);
  r = ffebld_right (expr);

  if (ffebld_op (l) != FFEBLD_opCONTER)
    return expr;
  if (ffebld_op (r) != FFEBLD_opCONTER)
    return expr;

  switch (ffeinfo_basictype (ffebld_info (ffebld_left (expr))))
    {
    case FFEINFO_basictypeANY:
      return expr;

    case FFEINFO_basictypeINTEGER:
      switch (ffeinfo_kindtype (ffebld_info (ffebld_left (expr))))
	{
#if FFETARGET_okINTEGER1
	case FFEINFO_kindtypeINTEGER1:
	  error = ffetarget_ge_integer1 (&val,
			       ffebld_constant_integer1 (ffebld_conter (l)),
			      ffebld_constant_integer1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okINTEGER2
	case FFEINFO_kindtypeINTEGER2:
	  error = ffetarget_ge_integer2 (&val,
			       ffebld_constant_integer2 (ffebld_conter (l)),
			      ffebld_constant_integer2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okINTEGER3
	case FFEINFO_kindtypeINTEGER3:
	  error = ffetarget_ge_integer3 (&val,
			       ffebld_constant_integer3 (ffebld_conter (l)),
			      ffebld_constant_integer3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okINTEGER4
	case FFEINFO_kindtypeINTEGER4:
	  error = ffetarget_ge_integer4 (&val,
			       ffebld_constant_integer4 (ffebld_conter (l)),
			      ffebld_constant_integer4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

	default:
	  assert ("bad integer kind type" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeREAL:
      switch (ffeinfo_kindtype (ffebld_info (ffebld_left (expr))))
	{
#if FFETARGET_okREAL1
	case FFEINFO_kindtypeREAL1:
	  error = ffetarget_ge_real1 (&val,
				  ffebld_constant_real1 (ffebld_conter (l)),
				 ffebld_constant_real1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okREAL2
	case FFEINFO_kindtypeREAL2:
	  error = ffetarget_ge_real2 (&val,
				  ffebld_constant_real2 (ffebld_conter (l)),
				 ffebld_constant_real2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okREAL3
	case FFEINFO_kindtypeREAL3:
	  error = ffetarget_ge_real3 (&val,
				  ffebld_constant_real3 (ffebld_conter (l)),
				 ffebld_constant_real3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okREAL4
	case FFEINFO_kindtypeREAL4:
	  error = ffetarget_ge_real4 (&val,
				  ffebld_constant_real4 (ffebld_conter (l)),
				 ffebld_constant_real4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

	default:
	  assert ("bad real kind type" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeCHARACTER:
      switch (ffeinfo_kindtype (ffebld_info (ffebld_left (expr))))
	{
#if FFETARGET_okCHARACTER1
	case FFEINFO_kindtypeCHARACTER1:
	  error = ffetarget_ge_character1 (&val,
			     ffebld_constant_character1 (ffebld_conter (l)),
			    ffebld_constant_character1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okCHARACTER2
	case FFEINFO_kindtypeCHARACTER2:
	  error = ffetarget_ge_character2 (&val,
			     ffebld_constant_character2 (ffebld_conter (l)),
			    ffebld_constant_character2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okCHARACTER3
	case FFEINFO_kindtypeCHARACTER3:
	  error = ffetarget_ge_character3 (&val,
			     ffebld_constant_character3 (ffebld_conter (l)),
			    ffebld_constant_character3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okCHARACTER4
	case FFEINFO_kindtypeCHARACTER4:
	  error = ffetarget_ge_character4 (&val,
			     ffebld_constant_character4 (ffebld_conter (l)),
			    ffebld_constant_character4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

	default:
	  assert ("bad character kind type" == NULL);
	  break;
	}
      break;

    default:
      assert ("bad type" == NULL);
      return expr;
    }

  ffebld_set_info (expr, ffeinfo_new
		   (FFEINFO_basictypeLOGICAL,
		    FFEINFO_kindtypeLOGICALDEFAULT,
		    0,
		    FFEINFO_kindENTITY,
		    FFEINFO_whereCONSTANT,
		    FFETARGET_charactersizeNONE));

  if ((error != FFEBAD)
      && ffebad_start (error))
    {
      ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
      ffebad_finish ();
    }

  return expr;
}

/* ffeexpr_collapse_gt -- Collapse gt expr

   ffebld expr;
   ffelexToken token;
   expr = ffeexpr_collapse_gt(expr,token);

   If the result of the expr is a constant, replaces the expr with the
   computed constant.  */

ffebld
ffeexpr_collapse_gt (ffebld expr, ffelexToken t)
{
  ffebad error = FFEBAD;
  ffebld l;
  ffebld r;
  bool val;

  if (ffeinfo_where (ffebld_info (expr)) != FFEINFO_whereCONSTANT)
    return expr;

  l = ffebld_left (expr);
  r = ffebld_right (expr);

  if (ffebld_op (l) != FFEBLD_opCONTER)
    return expr;
  if (ffebld_op (r) != FFEBLD_opCONTER)
    return expr;

  switch (ffeinfo_basictype (ffebld_info (ffebld_left (expr))))
    {
    case FFEINFO_basictypeANY:
      return expr;

    case FFEINFO_basictypeINTEGER:
      switch (ffeinfo_kindtype (ffebld_info (ffebld_left (expr))))
	{
#if FFETARGET_okINTEGER1
	case FFEINFO_kindtypeINTEGER1:
	  error = ffetarget_gt_integer1 (&val,
			       ffebld_constant_integer1 (ffebld_conter (l)),
			      ffebld_constant_integer1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okINTEGER2
	case FFEINFO_kindtypeINTEGER2:
	  error = ffetarget_gt_integer2 (&val,
			       ffebld_constant_integer2 (ffebld_conter (l)),
			      ffebld_constant_integer2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okINTEGER3
	case FFEINFO_kindtypeINTEGER3:
	  error = ffetarget_gt_integer3 (&val,
			       ffebld_constant_integer3 (ffebld_conter (l)),
			      ffebld_constant_integer3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okINTEGER4
	case FFEINFO_kindtypeINTEGER4:
	  error = ffetarget_gt_integer4 (&val,
			       ffebld_constant_integer4 (ffebld_conter (l)),
			      ffebld_constant_integer4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

	default:
	  assert ("bad integer kind type" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeREAL:
      switch (ffeinfo_kindtype (ffebld_info (ffebld_left (expr))))
	{
#if FFETARGET_okREAL1
	case FFEINFO_kindtypeREAL1:
	  error = ffetarget_gt_real1 (&val,
				  ffebld_constant_real1 (ffebld_conter (l)),
				 ffebld_constant_real1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okREAL2
	case FFEINFO_kindtypeREAL2:
	  error = ffetarget_gt_real2 (&val,
				  ffebld_constant_real2 (ffebld_conter (l)),
				 ffebld_constant_real2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okREAL3
	case FFEINFO_kindtypeREAL3:
	  error = ffetarget_gt_real3 (&val,
				  ffebld_constant_real3 (ffebld_conter (l)),
				 ffebld_constant_real3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okREAL4
	case FFEINFO_kindtypeREAL4:
	  error = ffetarget_gt_real4 (&val,
				  ffebld_constant_real4 (ffebld_conter (l)),
				 ffebld_constant_real4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

	default:
	  assert ("bad real kind type" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeCHARACTER:
      switch (ffeinfo_kindtype (ffebld_info (ffebld_left (expr))))
	{
#if FFETARGET_okCHARACTER1
	case FFEINFO_kindtypeCHARACTER1:
	  error = ffetarget_gt_character1 (&val,
			     ffebld_constant_character1 (ffebld_conter (l)),
			    ffebld_constant_character1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okCHARACTER2
	case FFEINFO_kindtypeCHARACTER2:
	  error = ffetarget_gt_character2 (&val,
			     ffebld_constant_character2 (ffebld_conter (l)),
			    ffebld_constant_character2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okCHARACTER3
	case FFEINFO_kindtypeCHARACTER3:
	  error = ffetarget_gt_character3 (&val,
			     ffebld_constant_character3 (ffebld_conter (l)),
			    ffebld_constant_character3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okCHARACTER4
	case FFEINFO_kindtypeCHARACTER4:
	  error = ffetarget_gt_character4 (&val,
			     ffebld_constant_character4 (ffebld_conter (l)),
			    ffebld_constant_character4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

	default:
	  assert ("bad character kind type" == NULL);
	  break;
	}
      break;

    default:
      assert ("bad type" == NULL);
      return expr;
    }

  ffebld_set_info (expr, ffeinfo_new
		   (FFEINFO_basictypeLOGICAL,
		    FFEINFO_kindtypeLOGICALDEFAULT,
		    0,
		    FFEINFO_kindENTITY,
		    FFEINFO_whereCONSTANT,
		    FFETARGET_charactersizeNONE));

  if ((error != FFEBAD)
      && ffebad_start (error))
    {
      ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
      ffebad_finish ();
    }

  return expr;
}

/* ffeexpr_collapse_le -- Collapse le expr

   ffebld expr;
   ffelexToken token;
   expr = ffeexpr_collapse_le(expr,token);

   If the result of the expr is a constant, replaces the expr with the
   computed constant.  */

ffebld
ffeexpr_collapse_le (ffebld expr, ffelexToken t)
{
  ffebad error = FFEBAD;
  ffebld l;
  ffebld r;
  bool val;

  if (ffeinfo_where (ffebld_info (expr)) != FFEINFO_whereCONSTANT)
    return expr;

  l = ffebld_left (expr);
  r = ffebld_right (expr);

  if (ffebld_op (l) != FFEBLD_opCONTER)
    return expr;
  if (ffebld_op (r) != FFEBLD_opCONTER)
    return expr;

  switch (ffeinfo_basictype (ffebld_info (ffebld_left (expr))))
    {
    case FFEINFO_basictypeANY:
      return expr;

    case FFEINFO_basictypeINTEGER:
      switch (ffeinfo_kindtype (ffebld_info (ffebld_left (expr))))
	{
#if FFETARGET_okINTEGER1
	case FFEINFO_kindtypeINTEGER1:
	  error = ffetarget_le_integer1 (&val,
			       ffebld_constant_integer1 (ffebld_conter (l)),
			      ffebld_constant_integer1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okINTEGER2
	case FFEINFO_kindtypeINTEGER2:
	  error = ffetarget_le_integer2 (&val,
			       ffebld_constant_integer2 (ffebld_conter (l)),
			      ffebld_constant_integer2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okINTEGER3
	case FFEINFO_kindtypeINTEGER3:
	  error = ffetarget_le_integer3 (&val,
			       ffebld_constant_integer3 (ffebld_conter (l)),
			      ffebld_constant_integer3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okINTEGER4
	case FFEINFO_kindtypeINTEGER4:
	  error = ffetarget_le_integer4 (&val,
			       ffebld_constant_integer4 (ffebld_conter (l)),
			      ffebld_constant_integer4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

	default:
	  assert ("bad integer kind type" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeREAL:
      switch (ffeinfo_kindtype (ffebld_info (ffebld_left (expr))))
	{
#if FFETARGET_okREAL1
	case FFEINFO_kindtypeREAL1:
	  error = ffetarget_le_real1 (&val,
				  ffebld_constant_real1 (ffebld_conter (l)),
				 ffebld_constant_real1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okREAL2
	case FFEINFO_kindtypeREAL2:
	  error = ffetarget_le_real2 (&val,
				  ffebld_constant_real2 (ffebld_conter (l)),
				 ffebld_constant_real2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okREAL3
	case FFEINFO_kindtypeREAL3:
	  error = ffetarget_le_real3 (&val,
				  ffebld_constant_real3 (ffebld_conter (l)),
				 ffebld_constant_real3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okREAL4
	case FFEINFO_kindtypeREAL4:
	  error = ffetarget_le_real4 (&val,
				  ffebld_constant_real4 (ffebld_conter (l)),
				 ffebld_constant_real4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

	default:
	  assert ("bad real kind type" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeCHARACTER:
      switch (ffeinfo_kindtype (ffebld_info (ffebld_left (expr))))
	{
#if FFETARGET_okCHARACTER1
	case FFEINFO_kindtypeCHARACTER1:
	  error = ffetarget_le_character1 (&val,
			     ffebld_constant_character1 (ffebld_conter (l)),
			    ffebld_constant_character1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okCHARACTER2
	case FFEINFO_kindtypeCHARACTER2:
	  error = ffetarget_le_character2 (&val,
			     ffebld_constant_character2 (ffebld_conter (l)),
			    ffebld_constant_character2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okCHARACTER3
	case FFEINFO_kindtypeCHARACTER3:
	  error = ffetarget_le_character3 (&val,
			     ffebld_constant_character3 (ffebld_conter (l)),
			    ffebld_constant_character3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okCHARACTER4
	case FFEINFO_kindtypeCHARACTER4:
	  error = ffetarget_le_character4 (&val,
			     ffebld_constant_character4 (ffebld_conter (l)),
			    ffebld_constant_character4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

	default:
	  assert ("bad character kind type" == NULL);
	  break;
	}
      break;

    default:
      assert ("bad type" == NULL);
      return expr;
    }

  ffebld_set_info (expr, ffeinfo_new
		   (FFEINFO_basictypeLOGICAL,
		    FFEINFO_kindtypeLOGICALDEFAULT,
		    0,
		    FFEINFO_kindENTITY,
		    FFEINFO_whereCONSTANT,
		    FFETARGET_charactersizeNONE));

  if ((error != FFEBAD)
      && ffebad_start (error))
    {
      ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
      ffebad_finish ();
    }

  return expr;
}

/* ffeexpr_collapse_lt -- Collapse lt expr

   ffebld expr;
   ffelexToken token;
   expr = ffeexpr_collapse_lt(expr,token);

   If the result of the expr is a constant, replaces the expr with the
   computed constant.  */

ffebld
ffeexpr_collapse_lt (ffebld expr, ffelexToken t)
{
  ffebad error = FFEBAD;
  ffebld l;
  ffebld r;
  bool val;

  if (ffeinfo_where (ffebld_info (expr)) != FFEINFO_whereCONSTANT)
    return expr;

  l = ffebld_left (expr);
  r = ffebld_right (expr);

  if (ffebld_op (l) != FFEBLD_opCONTER)
    return expr;
  if (ffebld_op (r) != FFEBLD_opCONTER)
    return expr;

  switch (ffeinfo_basictype (ffebld_info (ffebld_left (expr))))
    {
    case FFEINFO_basictypeANY:
      return expr;

    case FFEINFO_basictypeINTEGER:
      switch (ffeinfo_kindtype (ffebld_info (ffebld_left (expr))))
	{
#if FFETARGET_okINTEGER1
	case FFEINFO_kindtypeINTEGER1:
	  error = ffetarget_lt_integer1 (&val,
			       ffebld_constant_integer1 (ffebld_conter (l)),
			      ffebld_constant_integer1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okINTEGER2
	case FFEINFO_kindtypeINTEGER2:
	  error = ffetarget_lt_integer2 (&val,
			       ffebld_constant_integer2 (ffebld_conter (l)),
			      ffebld_constant_integer2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okINTEGER3
	case FFEINFO_kindtypeINTEGER3:
	  error = ffetarget_lt_integer3 (&val,
			       ffebld_constant_integer3 (ffebld_conter (l)),
			      ffebld_constant_integer3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okINTEGER4
	case FFEINFO_kindtypeINTEGER4:
	  error = ffetarget_lt_integer4 (&val,
			       ffebld_constant_integer4 (ffebld_conter (l)),
			      ffebld_constant_integer4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

	default:
	  assert ("bad integer kind type" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeREAL:
      switch (ffeinfo_kindtype (ffebld_info (ffebld_left (expr))))
	{
#if FFETARGET_okREAL1
	case FFEINFO_kindtypeREAL1:
	  error = ffetarget_lt_real1 (&val,
				  ffebld_constant_real1 (ffebld_conter (l)),
				 ffebld_constant_real1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okREAL2
	case FFEINFO_kindtypeREAL2:
	  error = ffetarget_lt_real2 (&val,
				  ffebld_constant_real2 (ffebld_conter (l)),
				 ffebld_constant_real2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okREAL3
	case FFEINFO_kindtypeREAL3:
	  error = ffetarget_lt_real3 (&val,
				  ffebld_constant_real3 (ffebld_conter (l)),
				 ffebld_constant_real3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okREAL4
	case FFEINFO_kindtypeREAL4:
	  error = ffetarget_lt_real4 (&val,
				  ffebld_constant_real4 (ffebld_conter (l)),
				 ffebld_constant_real4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

	default:
	  assert ("bad real kind type" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeCHARACTER:
      switch (ffeinfo_kindtype (ffebld_info (ffebld_left (expr))))
	{
#if FFETARGET_okCHARACTER1
	case FFEINFO_kindtypeCHARACTER1:
	  error = ffetarget_lt_character1 (&val,
			     ffebld_constant_character1 (ffebld_conter (l)),
			    ffebld_constant_character1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okCHARACTER2
	case FFEINFO_kindtypeCHARACTER2:
	  error = ffetarget_lt_character2 (&val,
			     ffebld_constant_character2 (ffebld_conter (l)),
			    ffebld_constant_character2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okCHARACTER3
	case FFEINFO_kindtypeCHARACTER3:
	  error = ffetarget_lt_character3 (&val,
			     ffebld_constant_character3 (ffebld_conter (l)),
			    ffebld_constant_character3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

#if FFETARGET_okCHARACTER4
	case FFEINFO_kindtypeCHARACTER4:
	  error = ffetarget_lt_character4 (&val,
			     ffebld_constant_character4 (ffebld_conter (l)),
			    ffebld_constant_character4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig
	    (ffebld_constant_new_logicaldefault (val), expr);
	  break;
#endif

	default:
	  assert ("bad character kind type" == NULL);
	  break;
	}
      break;

    default:
      assert ("bad type" == NULL);
      return expr;
    }

  ffebld_set_info (expr, ffeinfo_new
		   (FFEINFO_basictypeLOGICAL,
		    FFEINFO_kindtypeLOGICALDEFAULT,
		    0,
		    FFEINFO_kindENTITY,
		    FFEINFO_whereCONSTANT,
		    FFETARGET_charactersizeNONE));

  if ((error != FFEBAD)
      && ffebad_start (error))
    {
      ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
      ffebad_finish ();
    }

  return expr;
}

/* ffeexpr_collapse_and -- Collapse and expr

   ffebld expr;
   ffelexToken token;
   expr = ffeexpr_collapse_and(expr,token);

   If the result of the expr is a constant, replaces the expr with the
   computed constant.  */

ffebld
ffeexpr_collapse_and (ffebld expr, ffelexToken t)
{
  ffebad error = FFEBAD;
  ffebld l;
  ffebld r;
  ffebldConstantUnion u;
  ffeinfoBasictype bt;
  ffeinfoKindtype kt;

  if (ffeinfo_where (ffebld_info (expr)) != FFEINFO_whereCONSTANT)
    return expr;

  l = ffebld_left (expr);
  r = ffebld_right (expr);

  if (ffebld_op (l) != FFEBLD_opCONTER)
    return expr;
  if (ffebld_op (r) != FFEBLD_opCONTER)
    return expr;

  switch (bt = ffeinfo_basictype (ffebld_info (expr)))
    {
    case FFEINFO_basictypeANY:
      return expr;

    case FFEINFO_basictypeINTEGER:
      switch (kt = ffeinfo_kindtype (ffebld_info (expr)))
	{
#if FFETARGET_okINTEGER1
	case FFEINFO_kindtypeINTEGER1:
	  error = ffetarget_and_integer1 (ffebld_cu_ptr_integer1 (u),
			       ffebld_constant_integer1 (ffebld_conter (l)),
			      ffebld_constant_integer1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer1_val
					(ffebld_cu_val_integer1 (u)), expr);
	  break;
#endif

#if FFETARGET_okINTEGER2
	case FFEINFO_kindtypeINTEGER2:
	  error = ffetarget_and_integer2 (ffebld_cu_ptr_integer2 (u),
			       ffebld_constant_integer2 (ffebld_conter (l)),
			      ffebld_constant_integer2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer2_val
					(ffebld_cu_val_integer2 (u)), expr);
	  break;
#endif

#if FFETARGET_okINTEGER3
	case FFEINFO_kindtypeINTEGER3:
	  error = ffetarget_and_integer3 (ffebld_cu_ptr_integer3 (u),
			       ffebld_constant_integer3 (ffebld_conter (l)),
			      ffebld_constant_integer3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer3_val
					(ffebld_cu_val_integer3 (u)), expr);
	  break;
#endif

#if FFETARGET_okINTEGER4
	case FFEINFO_kindtypeINTEGER4:
	  error = ffetarget_and_integer4 (ffebld_cu_ptr_integer4 (u),
			       ffebld_constant_integer4 (ffebld_conter (l)),
			      ffebld_constant_integer4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer4_val
					(ffebld_cu_val_integer4 (u)), expr);
	  break;
#endif

	default:
	  assert ("bad integer kind type" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeLOGICAL:
      switch (kt = ffeinfo_kindtype (ffebld_info (expr)))
	{
#if FFETARGET_okLOGICAL1
	case FFEINFO_kindtypeLOGICAL1:
	  error = ffetarget_and_logical1 (ffebld_cu_ptr_logical1 (u),
			       ffebld_constant_logical1 (ffebld_conter (l)),
			      ffebld_constant_logical1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_logical1_val
					(ffebld_cu_val_logical1 (u)), expr);
	  break;
#endif

#if FFETARGET_okLOGICAL2
	case FFEINFO_kindtypeLOGICAL2:
	  error = ffetarget_and_logical2 (ffebld_cu_ptr_logical2 (u),
			       ffebld_constant_logical2 (ffebld_conter (l)),
			      ffebld_constant_logical2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_logical2_val
					(ffebld_cu_val_logical2 (u)), expr);
	  break;
#endif

#if FFETARGET_okLOGICAL3
	case FFEINFO_kindtypeLOGICAL3:
	  error = ffetarget_and_logical3 (ffebld_cu_ptr_logical3 (u),
			       ffebld_constant_logical3 (ffebld_conter (l)),
			      ffebld_constant_logical3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_logical3_val
					(ffebld_cu_val_logical3 (u)), expr);
	  break;
#endif

#if FFETARGET_okLOGICAL4
	case FFEINFO_kindtypeLOGICAL4:
	  error = ffetarget_and_logical4 (ffebld_cu_ptr_logical4 (u),
			       ffebld_constant_logical4 (ffebld_conter (l)),
			      ffebld_constant_logical4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_logical4_val
					(ffebld_cu_val_logical4 (u)), expr);
	  break;
#endif

	default:
	  assert ("bad logical kind type" == NULL);
	  break;
	}
      break;

    default:
      assert ("bad type" == NULL);
      return expr;
    }

  ffebld_set_info (expr, ffeinfo_new
		   (bt,
		    kt,
		    0,
		    FFEINFO_kindENTITY,
		    FFEINFO_whereCONSTANT,
		    FFETARGET_charactersizeNONE));

  if ((error != FFEBAD)
      && ffebad_start (error))
    {
      ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
      ffebad_finish ();
    }

  return expr;
}

/* ffeexpr_collapse_or -- Collapse or expr

   ffebld expr;
   ffelexToken token;
   expr = ffeexpr_collapse_or(expr,token);

   If the result of the expr is a constant, replaces the expr with the
   computed constant.  */

ffebld
ffeexpr_collapse_or (ffebld expr, ffelexToken t)
{
  ffebad error = FFEBAD;
  ffebld l;
  ffebld r;
  ffebldConstantUnion u;
  ffeinfoBasictype bt;
  ffeinfoKindtype kt;

  if (ffeinfo_where (ffebld_info (expr)) != FFEINFO_whereCONSTANT)
    return expr;

  l = ffebld_left (expr);
  r = ffebld_right (expr);

  if (ffebld_op (l) != FFEBLD_opCONTER)
    return expr;
  if (ffebld_op (r) != FFEBLD_opCONTER)
    return expr;

  switch (bt = ffeinfo_basictype (ffebld_info (expr)))
    {
    case FFEINFO_basictypeANY:
      return expr;

    case FFEINFO_basictypeINTEGER:
      switch (kt = ffeinfo_kindtype (ffebld_info (expr)))
	{
#if FFETARGET_okINTEGER1
	case FFEINFO_kindtypeINTEGER1:
	  error = ffetarget_or_integer1 (ffebld_cu_ptr_integer1 (u),
			       ffebld_constant_integer1 (ffebld_conter (l)),
			      ffebld_constant_integer1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer1_val
					(ffebld_cu_val_integer1 (u)), expr);
	  break;
#endif

#if FFETARGET_okINTEGER2
	case FFEINFO_kindtypeINTEGER2:
	  error = ffetarget_or_integer2 (ffebld_cu_ptr_integer2 (u),
			       ffebld_constant_integer2 (ffebld_conter (l)),
			      ffebld_constant_integer2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer2_val
					(ffebld_cu_val_integer2 (u)), expr);
	  break;
#endif

#if FFETARGET_okINTEGER3
	case FFEINFO_kindtypeINTEGER3:
	  error = ffetarget_or_integer3 (ffebld_cu_ptr_integer3 (u),
			       ffebld_constant_integer3 (ffebld_conter (l)),
			      ffebld_constant_integer3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer3_val
					(ffebld_cu_val_integer3 (u)), expr);
	  break;
#endif

#if FFETARGET_okINTEGER4
	case FFEINFO_kindtypeINTEGER4:
	  error = ffetarget_or_integer4 (ffebld_cu_ptr_integer4 (u),
			       ffebld_constant_integer4 (ffebld_conter (l)),
			      ffebld_constant_integer4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer4_val
					(ffebld_cu_val_integer4 (u)), expr);
	  break;
#endif

	default:
	  assert ("bad integer kind type" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeLOGICAL:
      switch (kt = ffeinfo_kindtype (ffebld_info (expr)))
	{
#if FFETARGET_okLOGICAL1
	case FFEINFO_kindtypeLOGICAL1:
	  error = ffetarget_or_logical1 (ffebld_cu_ptr_logical1 (u),
			       ffebld_constant_logical1 (ffebld_conter (l)),
			      ffebld_constant_logical1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_logical1_val
					(ffebld_cu_val_logical1 (u)), expr);
	  break;
#endif

#if FFETARGET_okLOGICAL2
	case FFEINFO_kindtypeLOGICAL2:
	  error = ffetarget_or_logical2 (ffebld_cu_ptr_logical2 (u),
			       ffebld_constant_logical2 (ffebld_conter (l)),
			      ffebld_constant_logical2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_logical2_val
					(ffebld_cu_val_logical2 (u)), expr);
	  break;
#endif

#if FFETARGET_okLOGICAL3
	case FFEINFO_kindtypeLOGICAL3:
	  error = ffetarget_or_logical3 (ffebld_cu_ptr_logical3 (u),
			       ffebld_constant_logical3 (ffebld_conter (l)),
			      ffebld_constant_logical3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_logical3_val
					(ffebld_cu_val_logical3 (u)), expr);
	  break;
#endif

#if FFETARGET_okLOGICAL4
	case FFEINFO_kindtypeLOGICAL4:
	  error = ffetarget_or_logical4 (ffebld_cu_ptr_logical4 (u),
			       ffebld_constant_logical4 (ffebld_conter (l)),
			      ffebld_constant_logical4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_logical4_val
					(ffebld_cu_val_logical4 (u)), expr);
	  break;
#endif

	default:
	  assert ("bad logical kind type" == NULL);
	  break;
	}
      break;

    default:
      assert ("bad type" == NULL);
      return expr;
    }

  ffebld_set_info (expr, ffeinfo_new
		   (bt,
		    kt,
		    0,
		    FFEINFO_kindENTITY,
		    FFEINFO_whereCONSTANT,
		    FFETARGET_charactersizeNONE));

  if ((error != FFEBAD)
      && ffebad_start (error))
    {
      ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
      ffebad_finish ();
    }

  return expr;
}

/* ffeexpr_collapse_xor -- Collapse xor expr

   ffebld expr;
   ffelexToken token;
   expr = ffeexpr_collapse_xor(expr,token);

   If the result of the expr is a constant, replaces the expr with the
   computed constant.  */

ffebld
ffeexpr_collapse_xor (ffebld expr, ffelexToken t)
{
  ffebad error = FFEBAD;
  ffebld l;
  ffebld r;
  ffebldConstantUnion u;
  ffeinfoBasictype bt;
  ffeinfoKindtype kt;

  if (ffeinfo_where (ffebld_info (expr)) != FFEINFO_whereCONSTANT)
    return expr;

  l = ffebld_left (expr);
  r = ffebld_right (expr);

  if (ffebld_op (l) != FFEBLD_opCONTER)
    return expr;
  if (ffebld_op (r) != FFEBLD_opCONTER)
    return expr;

  switch (bt = ffeinfo_basictype (ffebld_info (expr)))
    {
    case FFEINFO_basictypeANY:
      return expr;

    case FFEINFO_basictypeINTEGER:
      switch (kt = ffeinfo_kindtype (ffebld_info (expr)))
	{
#if FFETARGET_okINTEGER1
	case FFEINFO_kindtypeINTEGER1:
	  error = ffetarget_xor_integer1 (ffebld_cu_ptr_integer1 (u),
			       ffebld_constant_integer1 (ffebld_conter (l)),
			      ffebld_constant_integer1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer1_val
					(ffebld_cu_val_integer1 (u)), expr);
	  break;
#endif

#if FFETARGET_okINTEGER2
	case FFEINFO_kindtypeINTEGER2:
	  error = ffetarget_xor_integer2 (ffebld_cu_ptr_integer2 (u),
			       ffebld_constant_integer2 (ffebld_conter (l)),
			      ffebld_constant_integer2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer2_val
					(ffebld_cu_val_integer2 (u)), expr);
	  break;
#endif

#if FFETARGET_okINTEGER3
	case FFEINFO_kindtypeINTEGER3:
	  error = ffetarget_xor_integer3 (ffebld_cu_ptr_integer3 (u),
			       ffebld_constant_integer3 (ffebld_conter (l)),
			      ffebld_constant_integer3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer3_val
					(ffebld_cu_val_integer3 (u)), expr);
	  break;
#endif

#if FFETARGET_okINTEGER4
	case FFEINFO_kindtypeINTEGER4:
	  error = ffetarget_xor_integer4 (ffebld_cu_ptr_integer4 (u),
			       ffebld_constant_integer4 (ffebld_conter (l)),
			      ffebld_constant_integer4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer4_val
					(ffebld_cu_val_integer4 (u)), expr);
	  break;
#endif

	default:
	  assert ("bad integer kind type" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeLOGICAL:
      switch (kt = ffeinfo_kindtype (ffebld_info (expr)))
	{
#if FFETARGET_okLOGICAL1
	case FFEINFO_kindtypeLOGICAL1:
	  error = ffetarget_xor_logical1 (ffebld_cu_ptr_logical1 (u),
			       ffebld_constant_logical1 (ffebld_conter (l)),
			      ffebld_constant_logical1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_logical1_val
					(ffebld_cu_val_logical1 (u)), expr);
	  break;
#endif

#if FFETARGET_okLOGICAL2
	case FFEINFO_kindtypeLOGICAL2:
	  error = ffetarget_xor_logical2 (ffebld_cu_ptr_logical2 (u),
			       ffebld_constant_logical2 (ffebld_conter (l)),
			      ffebld_constant_logical2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_logical2_val
					(ffebld_cu_val_logical2 (u)), expr);
	  break;
#endif

#if FFETARGET_okLOGICAL3
	case FFEINFO_kindtypeLOGICAL3:
	  error = ffetarget_xor_logical3 (ffebld_cu_ptr_logical3 (u),
			       ffebld_constant_logical3 (ffebld_conter (l)),
			      ffebld_constant_logical3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_logical3_val
					(ffebld_cu_val_logical3 (u)), expr);
	  break;
#endif

#if FFETARGET_okLOGICAL4
	case FFEINFO_kindtypeLOGICAL4:
	  error = ffetarget_xor_logical4 (ffebld_cu_ptr_logical4 (u),
			       ffebld_constant_logical4 (ffebld_conter (l)),
			      ffebld_constant_logical4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_logical4_val
					(ffebld_cu_val_logical4 (u)), expr);
	  break;
#endif

	default:
	  assert ("bad logical kind type" == NULL);
	  break;
	}
      break;

    default:
      assert ("bad type" == NULL);
      return expr;
    }

  ffebld_set_info (expr, ffeinfo_new
		   (bt,
		    kt,
		    0,
		    FFEINFO_kindENTITY,
		    FFEINFO_whereCONSTANT,
		    FFETARGET_charactersizeNONE));

  if ((error != FFEBAD)
      && ffebad_start (error))
    {
      ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
      ffebad_finish ();
    }

  return expr;
}

/* ffeexpr_collapse_eqv -- Collapse eqv expr

   ffebld expr;
   ffelexToken token;
   expr = ffeexpr_collapse_eqv(expr,token);

   If the result of the expr is a constant, replaces the expr with the
   computed constant.  */

ffebld
ffeexpr_collapse_eqv (ffebld expr, ffelexToken t)
{
  ffebad error = FFEBAD;
  ffebld l;
  ffebld r;
  ffebldConstantUnion u;
  ffeinfoBasictype bt;
  ffeinfoKindtype kt;

  if (ffeinfo_where (ffebld_info (expr)) != FFEINFO_whereCONSTANT)
    return expr;

  l = ffebld_left (expr);
  r = ffebld_right (expr);

  if (ffebld_op (l) != FFEBLD_opCONTER)
    return expr;
  if (ffebld_op (r) != FFEBLD_opCONTER)
    return expr;

  switch (bt = ffeinfo_basictype (ffebld_info (expr)))
    {
    case FFEINFO_basictypeANY:
      return expr;

    case FFEINFO_basictypeINTEGER:
      switch (kt = ffeinfo_kindtype (ffebld_info (expr)))
	{
#if FFETARGET_okINTEGER1
	case FFEINFO_kindtypeINTEGER1:
	  error = ffetarget_eqv_integer1 (ffebld_cu_ptr_integer1 (u),
			       ffebld_constant_integer1 (ffebld_conter (l)),
			      ffebld_constant_integer1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer1_val
					(ffebld_cu_val_integer1 (u)), expr);
	  break;
#endif

#if FFETARGET_okINTEGER2
	case FFEINFO_kindtypeINTEGER2:
	  error = ffetarget_eqv_integer2 (ffebld_cu_ptr_integer2 (u),
			       ffebld_constant_integer2 (ffebld_conter (l)),
			      ffebld_constant_integer2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer2_val
					(ffebld_cu_val_integer2 (u)), expr);
	  break;
#endif

#if FFETARGET_okINTEGER3
	case FFEINFO_kindtypeINTEGER3:
	  error = ffetarget_eqv_integer3 (ffebld_cu_ptr_integer3 (u),
			       ffebld_constant_integer3 (ffebld_conter (l)),
			      ffebld_constant_integer3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer3_val
					(ffebld_cu_val_integer3 (u)), expr);
	  break;
#endif

#if FFETARGET_okINTEGER4
	case FFEINFO_kindtypeINTEGER4:
	  error = ffetarget_eqv_integer4 (ffebld_cu_ptr_integer4 (u),
			       ffebld_constant_integer4 (ffebld_conter (l)),
			      ffebld_constant_integer4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer4_val
					(ffebld_cu_val_integer4 (u)), expr);
	  break;
#endif

	default:
	  assert ("bad integer kind type" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeLOGICAL:
      switch (kt = ffeinfo_kindtype (ffebld_info (expr)))
	{
#if FFETARGET_okLOGICAL1
	case FFEINFO_kindtypeLOGICAL1:
	  error = ffetarget_eqv_logical1 (ffebld_cu_ptr_logical1 (u),
			       ffebld_constant_logical1 (ffebld_conter (l)),
			      ffebld_constant_logical1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_logical1_val
					(ffebld_cu_val_logical1 (u)), expr);
	  break;
#endif

#if FFETARGET_okLOGICAL2
	case FFEINFO_kindtypeLOGICAL2:
	  error = ffetarget_eqv_logical2 (ffebld_cu_ptr_logical2 (u),
			       ffebld_constant_logical2 (ffebld_conter (l)),
			      ffebld_constant_logical2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_logical2_val
					(ffebld_cu_val_logical2 (u)), expr);
	  break;
#endif

#if FFETARGET_okLOGICAL3
	case FFEINFO_kindtypeLOGICAL3:
	  error = ffetarget_eqv_logical3 (ffebld_cu_ptr_logical3 (u),
			       ffebld_constant_logical3 (ffebld_conter (l)),
			      ffebld_constant_logical3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_logical3_val
					(ffebld_cu_val_logical3 (u)), expr);
	  break;
#endif

#if FFETARGET_okLOGICAL4
	case FFEINFO_kindtypeLOGICAL4:
	  error = ffetarget_eqv_logical4 (ffebld_cu_ptr_logical4 (u),
			       ffebld_constant_logical4 (ffebld_conter (l)),
			      ffebld_constant_logical4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_logical4_val
					(ffebld_cu_val_logical4 (u)), expr);
	  break;
#endif

	default:
	  assert ("bad logical kind type" == NULL);
	  break;
	}
      break;

    default:
      assert ("bad type" == NULL);
      return expr;
    }

  ffebld_set_info (expr, ffeinfo_new
		   (bt,
		    kt,
		    0,
		    FFEINFO_kindENTITY,
		    FFEINFO_whereCONSTANT,
		    FFETARGET_charactersizeNONE));

  if ((error != FFEBAD)
      && ffebad_start (error))
    {
      ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
      ffebad_finish ();
    }

  return expr;
}

/* ffeexpr_collapse_neqv -- Collapse neqv expr

   ffebld expr;
   ffelexToken token;
   expr = ffeexpr_collapse_neqv(expr,token);

   If the result of the expr is a constant, replaces the expr with the
   computed constant.  */

ffebld
ffeexpr_collapse_neqv (ffebld expr, ffelexToken t)
{
  ffebad error = FFEBAD;
  ffebld l;
  ffebld r;
  ffebldConstantUnion u;
  ffeinfoBasictype bt;
  ffeinfoKindtype kt;

  if (ffeinfo_where (ffebld_info (expr)) != FFEINFO_whereCONSTANT)
    return expr;

  l = ffebld_left (expr);
  r = ffebld_right (expr);

  if (ffebld_op (l) != FFEBLD_opCONTER)
    return expr;
  if (ffebld_op (r) != FFEBLD_opCONTER)
    return expr;

  switch (bt = ffeinfo_basictype (ffebld_info (expr)))
    {
    case FFEINFO_basictypeANY:
      return expr;

    case FFEINFO_basictypeINTEGER:
      switch (kt = ffeinfo_kindtype (ffebld_info (expr)))
	{
#if FFETARGET_okINTEGER1
	case FFEINFO_kindtypeINTEGER1:
	  error = ffetarget_neqv_integer1 (ffebld_cu_ptr_integer1 (u),
			       ffebld_constant_integer1 (ffebld_conter (l)),
			      ffebld_constant_integer1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer1_val
					(ffebld_cu_val_integer1 (u)), expr);
	  break;
#endif

#if FFETARGET_okINTEGER2
	case FFEINFO_kindtypeINTEGER2:
	  error = ffetarget_neqv_integer2 (ffebld_cu_ptr_integer2 (u),
			       ffebld_constant_integer2 (ffebld_conter (l)),
			      ffebld_constant_integer2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer2_val
					(ffebld_cu_val_integer2 (u)), expr);
	  break;
#endif

#if FFETARGET_okINTEGER3
	case FFEINFO_kindtypeINTEGER3:
	  error = ffetarget_neqv_integer3 (ffebld_cu_ptr_integer3 (u),
			       ffebld_constant_integer3 (ffebld_conter (l)),
			      ffebld_constant_integer3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer3_val
					(ffebld_cu_val_integer3 (u)), expr);
	  break;
#endif

#if FFETARGET_okINTEGER4
	case FFEINFO_kindtypeINTEGER4:
	  error = ffetarget_neqv_integer4 (ffebld_cu_ptr_integer4 (u),
			       ffebld_constant_integer4 (ffebld_conter (l)),
			      ffebld_constant_integer4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_integer4_val
					(ffebld_cu_val_integer4 (u)), expr);
	  break;
#endif

	default:
	  assert ("bad integer kind type" == NULL);
	  break;
	}
      break;

    case FFEINFO_basictypeLOGICAL:
      switch (kt = ffeinfo_kindtype (ffebld_info (expr)))
	{
#if FFETARGET_okLOGICAL1
	case FFEINFO_kindtypeLOGICAL1:
	  error = ffetarget_neqv_logical1 (ffebld_cu_ptr_logical1 (u),
			       ffebld_constant_logical1 (ffebld_conter (l)),
			      ffebld_constant_logical1 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_logical1_val
					(ffebld_cu_val_logical1 (u)), expr);
	  break;
#endif

#if FFETARGET_okLOGICAL2
	case FFEINFO_kindtypeLOGICAL2:
	  error = ffetarget_neqv_logical2 (ffebld_cu_ptr_logical2 (u),
			       ffebld_constant_logical2 (ffebld_conter (l)),
			      ffebld_constant_logical2 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_logical2_val
					(ffebld_cu_val_logical2 (u)), expr);
	  break;
#endif

#if FFETARGET_okLOGICAL3
	case FFEINFO_kindtypeLOGICAL3:
	  error = ffetarget_neqv_logical3 (ffebld_cu_ptr_logical3 (u),
			       ffebld_constant_logical3 (ffebld_conter (l)),
			      ffebld_constant_logical3 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_logical3_val
					(ffebld_cu_val_logical3 (u)), expr);
	  break;
#endif

#if FFETARGET_okLOGICAL4
	case FFEINFO_kindtypeLOGICAL4:
	  error = ffetarget_neqv_logical4 (ffebld_cu_ptr_logical4 (u),
			       ffebld_constant_logical4 (ffebld_conter (l)),
			      ffebld_constant_logical4 (ffebld_conter (r)));
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_logical4_val
					(ffebld_cu_val_logical4 (u)), expr);
	  break;
#endif

	default:
	  assert ("bad logical kind type" == NULL);
	  break;
	}
      break;

    default:
      assert ("bad type" == NULL);
      return expr;
    }

  ffebld_set_info (expr, ffeinfo_new
		   (bt,
		    kt,
		    0,
		    FFEINFO_kindENTITY,
		    FFEINFO_whereCONSTANT,
		    FFETARGET_charactersizeNONE));

  if ((error != FFEBAD)
      && ffebad_start (error))
    {
      ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
      ffebad_finish ();
    }

  return expr;
}

/* ffeexpr_collapse_symter -- Collapse symter expr

   ffebld expr;
   ffelexToken token;
   expr = ffeexpr_collapse_symter(expr,token);

   If the result of the expr is a constant, replaces the expr with the
   computed constant.  */

ffebld
ffeexpr_collapse_symter (ffebld expr, ffelexToken t UNUSED)
{
  ffebld r;
  ffeinfoBasictype bt;
  ffeinfoKindtype kt;
  ffetargetCharacterSize len;

  if (ffeinfo_where (ffebld_info (expr)) != FFEINFO_whereCONSTANT)
    return expr;

  if ((r = ffesymbol_init (ffebld_symter (expr))) == NULL)
    return expr;		/* A PARAMETER lhs in progress. */

  switch (ffebld_op (r))
    {
    case FFEBLD_opCONTER:
      break;

    case FFEBLD_opANY:
      return r;

    default:
      return expr;
    }

  bt = ffeinfo_basictype (ffebld_info (r));
  kt = ffeinfo_kindtype (ffebld_info (r));
  len = ffebld_size (r);

  expr = ffebld_new_conter_with_orig (ffebld_constant_copy (ffebld_conter (r)),
				      expr);

  ffebld_set_info (expr, ffeinfo_new
		   (bt,
		    kt,
		    0,
		    FFEINFO_kindENTITY,
		    FFEINFO_whereCONSTANT,
		    len));

  return expr;
}

/* ffeexpr_collapse_funcref -- Collapse funcref expr

   ffebld expr;
   ffelexToken token;
   expr = ffeexpr_collapse_funcref(expr,token);

   If the result of the expr is a constant, replaces the expr with the
   computed constant.  */

ffebld
ffeexpr_collapse_funcref (ffebld expr, ffelexToken t UNUSED)
{
  return expr;			/* ~~someday go ahead and collapse these,
				   though not required */
}

/* ffeexpr_collapse_arrayref -- Collapse arrayref expr

   ffebld expr;
   ffelexToken token;
   expr = ffeexpr_collapse_arrayref(expr,token);

   If the result of the expr is a constant, replaces the expr with the
   computed constant.  */

ffebld
ffeexpr_collapse_arrayref (ffebld expr, ffelexToken t UNUSED)
{
  return expr;
}

/* ffeexpr_collapse_substr -- Collapse substr expr

   ffebld expr;
   ffelexToken token;
   expr = ffeexpr_collapse_substr(expr,token);

   If the result of the expr is a constant, replaces the expr with the
   computed constant.  */

ffebld
ffeexpr_collapse_substr (ffebld expr, ffelexToken t)
{
  ffebad error = FFEBAD;
  ffebld l;
  ffebld r;
  ffebld start;
  ffebld stop;
  ffebldConstantUnion u;
  ffeinfoKindtype kt;
  ffetargetCharacterSize len;
  ffetargetIntegerDefault first;
  ffetargetIntegerDefault last;

  if (ffeinfo_where (ffebld_info (expr)) != FFEINFO_whereCONSTANT)
    return expr;

  l = ffebld_left (expr);
  r = ffebld_right (expr);	/* opITEM. */

  if (ffebld_op (l) != FFEBLD_opCONTER)
    return expr;

  kt = ffeinfo_kindtype (ffebld_info (l));
  len = ffebld_size (l);

  start = ffebld_head (r);
  stop = ffebld_head (ffebld_trail (r));
  if (start == NULL)
    first = 1;
  else
    {
      if ((ffebld_op (start) != FFEBLD_opCONTER)
	  || (ffeinfo_basictype (ffebld_info (start)) != FFEINFO_basictypeINTEGER)
	  || (ffeinfo_kindtype (ffebld_info (start))
	      != FFEINFO_kindtypeINTEGERDEFAULT))
	return expr;
      first = ffebld_constant_integerdefault (ffebld_conter (start));
    }
  if (stop == NULL)
    last = len;
  else
    {
      if ((ffebld_op (stop) != FFEBLD_opCONTER)
      || (ffeinfo_basictype (ffebld_info (stop)) != FFEINFO_basictypeINTEGER)
	  || (ffeinfo_kindtype (ffebld_info (stop))
	      != FFEINFO_kindtypeINTEGERDEFAULT))
	return expr;
      last = ffebld_constant_integerdefault (ffebld_conter (stop));
    }

  /* Handle problems that should have already been diagnosed, but
     left in the expression tree.  */

  if (first <= 0)
    first = 1;
  if (last < first)
    last = first + len - 1;

  if ((first == 1) && (last == len))
    {				/* Same as original. */
      expr = ffebld_new_conter_with_orig (ffebld_constant_copy
					  (ffebld_conter (l)), expr);
      ffebld_set_info (expr, ffeinfo_new
		       (FFEINFO_basictypeCHARACTER,
			kt,
			0,
			FFEINFO_kindENTITY,
			FFEINFO_whereCONSTANT,
			len));

      return expr;
    }

  switch (ffeinfo_basictype (ffebld_info (expr)))
    {
    case FFEINFO_basictypeANY:
      return expr;

    case FFEINFO_basictypeCHARACTER:
      switch (kt = ffeinfo_kindtype (ffebld_info (expr)))
	{
#if FFETARGET_okCHARACTER1
	case FFEINFO_kindtypeCHARACTER1:
	  error = ffetarget_substr_character1 (ffebld_cu_ptr_character1 (u),
		ffebld_constant_character1 (ffebld_conter (l)), first, last,
				   ffebld_constant_pool (), &len);
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_character1_val
				      (ffebld_cu_val_character1 (u)), expr);
	  break;
#endif

#if FFETARGET_okCHARACTER2
	case FFEINFO_kindtypeCHARACTER2:
	  error = ffetarget_substr_character2 (ffebld_cu_ptr_character2 (u),
		ffebld_constant_character2 (ffebld_conter (l)), first, last,
				   ffebld_constant_pool (), &len);
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_character2_val
				      (ffebld_cu_val_character2 (u)), expr);
	  break;
#endif

#if FFETARGET_okCHARACTER3
	case FFEINFO_kindtypeCHARACTER3:
	  error = ffetarget_substr_character3 (ffebld_cu_ptr_character3 (u),
		ffebld_constant_character3 (ffebld_conter (l)), first, last,
				   ffebld_constant_pool (), &len);
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_character3_val
				      (ffebld_cu_val_character3 (u)), expr);
	  break;
#endif

#if FFETARGET_okCHARACTER4
	case FFEINFO_kindtypeCHARACTER4:
	  error = ffetarget_substr_character4 (ffebld_cu_ptr_character4 (u),
		ffebld_constant_character4 (ffebld_conter (l)), first, last,
				   ffebld_constant_pool (), &len);
	  expr = ffebld_new_conter_with_orig (ffebld_constant_new_character4_val
				      (ffebld_cu_val_character4 (u)), expr);
	  break;
#endif

	default:
	  assert ("bad character kind type" == NULL);
	  break;
	}
      break;

    default:
      assert ("bad type" == NULL);
      return expr;
    }

  ffebld_set_info (expr, ffeinfo_new
		   (FFEINFO_basictypeCHARACTER,
		    kt,
		    0,
		    FFEINFO_kindENTITY,
		    FFEINFO_whereCONSTANT,
		    len));

  if ((error != FFEBAD)
      && ffebad_start (error))
    {
      ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
      ffebad_finish ();
    }

  return expr;
}

/* ffeexpr_convert -- Convert source expression to given type

   ffebld source;
   ffelexToken source_token;
   ffelexToken dest_token;  // Any appropriate token for "destination".
   ffeinfoBasictype bt;
   ffeinfoKindtype kt;
   ffetargetCharactersize sz;
   ffeexprContext context;  // Mainly LET or DATA.
   source = ffeexpr_convert(source,source_token,dest_token,bt,kt,sz,context);

   If the expression conforms, returns the source expression.  Otherwise
   returns source wrapped in a convert node doing the conversion, or
   ANY wrapped in convert if there is a conversion error (and issues an
   error message).  Be sensitive to the context for certain aspects of
   the conversion.  */

ffebld
ffeexpr_convert (ffebld source, ffelexToken source_token, ffelexToken dest_token,
		 ffeinfoBasictype bt, ffeinfoKindtype kt, ffeinfoRank rk,
		 ffetargetCharacterSize sz, ffeexprContext context)
{
  bool bad;
  ffeinfo info;
  ffeinfoWhere wh;

  info = ffebld_info (source);
  if ((bt != ffeinfo_basictype (info))
      || (kt != ffeinfo_kindtype (info))
      || (rk != 0)		/* Can't convert from or to arrays yet. */
      || (ffeinfo_rank (info) != 0)
      || (sz != ffebld_size_known (source)))
#if 0	/* Nobody seems to need this spurious CONVERT node. */
      || ((context != FFEEXPR_contextLET)
	  && (bt == FFEINFO_basictypeCHARACTER)
	  && (sz == FFETARGET_charactersizeNONE)))
#endif
    {
      switch (ffeinfo_basictype (info))
	{
	case FFEINFO_basictypeLOGICAL:
	  switch (bt)
	    {
	    case FFEINFO_basictypeLOGICAL:
	      bad = FALSE;
	      break;

	    case FFEINFO_basictypeINTEGER:
	      bad = !ffe_is_ugly_logint ();
	      break;

	    case FFEINFO_basictypeCHARACTER:
	      bad = ffe_is_pedantic ()
		|| !(ffe_is_ugly_init ()
		     && (context == FFEEXPR_contextDATA));
	      break;

	    default:
	      bad = TRUE;
	      break;
	    }
	  break;

	case FFEINFO_basictypeINTEGER:
	  switch (bt)
	    {
	    case FFEINFO_basictypeINTEGER:
	    case FFEINFO_basictypeREAL:
	    case FFEINFO_basictypeCOMPLEX:
	      bad = FALSE;
	      break;

	    case FFEINFO_basictypeLOGICAL:
	      bad = !ffe_is_ugly_logint ();
	      break;

	    case FFEINFO_basictypeCHARACTER:
	      bad = ffe_is_pedantic ()
		|| !(ffe_is_ugly_init ()
		     && (context == FFEEXPR_contextDATA));
	      break;

	    default:
	      bad = TRUE;
	      break;
	    }
	  break;

	case FFEINFO_basictypeREAL:
	case FFEINFO_basictypeCOMPLEX:
	  switch (bt)
	    {
	    case FFEINFO_basictypeINTEGER:
	    case FFEINFO_basictypeREAL:
	    case FFEINFO_basictypeCOMPLEX:
	      bad = FALSE;
	      break;

	    case FFEINFO_basictypeCHARACTER:
	      bad = TRUE;
	      break;

	    default:
	      bad = TRUE;
	      break;
	    }
	  break;

	case FFEINFO_basictypeCHARACTER:
	  bad = (bt != FFEINFO_basictypeCHARACTER)
	    && (ffe_is_pedantic ()
		|| (bt != FFEINFO_basictypeINTEGER)
		|| !(ffe_is_ugly_init ()
		     && (context == FFEEXPR_contextDATA)));
	  break;

	case FFEINFO_basictypeTYPELESS:
	case FFEINFO_basictypeHOLLERITH:
	  bad = ffe_is_pedantic ()
	    || !(ffe_is_ugly_init ()
		 && ((context == FFEEXPR_contextDATA)
		     || (context == FFEEXPR_contextLET)));
	  break;

	default:
	  bad = TRUE;
	  break;
	}

      if (!bad && ((rk != 0) || (ffeinfo_rank (info) != 0)))
	bad = TRUE;

      if (bad && (bt != FFEINFO_basictypeANY) && (kt != FFEINFO_kindtypeANY)
	  && (ffeinfo_basictype (info) != FFEINFO_basictypeANY)
	  && (ffeinfo_kindtype (info) != FFEINFO_kindtypeANY)
	  && (ffeinfo_where (info) != FFEINFO_whereANY))
	{
	  if (ffebad_start (FFEBAD_BAD_TYPES))
	    {
	      if (dest_token == NULL)
		ffebad_here (0, ffewhere_line_unknown (),
			     ffewhere_column_unknown ());
	      else
		ffebad_here (0, ffelex_token_where_line (dest_token),
			     ffelex_token_where_column (dest_token));
	      assert (source_token != NULL);
	      ffebad_here (1, ffelex_token_where_line (source_token),
			   ffelex_token_where_column (source_token));
	      ffebad_finish ();
	    }

	  source = ffebld_new_any ();
	  ffebld_set_info (source, ffeinfo_new_any ());
	}
      else
	{
	  switch (ffeinfo_where (info))
	    {
	    case FFEINFO_whereCONSTANT:
	      wh = FFEINFO_whereCONSTANT;
	      break;

	    case FFEINFO_whereIMMEDIATE:
	      wh = FFEINFO_whereIMMEDIATE;
	      break;

	    default:
	      wh = FFEINFO_whereFLEETING;
	      break;
	    }
	  source = ffebld_new_convert (source);
	  ffebld_set_info (source, ffeinfo_new
			   (bt,
			    kt,
			    0,
			    FFEINFO_kindENTITY,
			    wh,
			    sz));
	  source = ffeexpr_collapse_convert (source, source_token);
	}
    }

  return source;
}

/* ffeexpr_convert_expr -- Convert source expr to conform to dest expr

   ffebld source;
   ffebld dest;
   ffelexToken source_token;
   ffelexToken dest_token;
   ffeexprContext context;
   source = ffeexpr_convert_expr(source,source_token,dest,dest_token,context);

   If the expressions conform, returns the source expression.  Otherwise
   returns source wrapped in a convert node doing the conversion, or
   ANY wrapped in convert if there is a conversion error (and issues an
   error message).  Be sensitive to the context, such as LET or DATA.  */

ffebld
ffeexpr_convert_expr (ffebld source, ffelexToken source_token, ffebld dest,
		      ffelexToken dest_token, ffeexprContext context)
{
  ffeinfo info;

  info = ffebld_info (dest);
  return ffeexpr_convert (source, source_token, dest_token,
			  ffeinfo_basictype (info),
			  ffeinfo_kindtype (info),
			  ffeinfo_rank (info),
			  ffebld_size_known (dest),
			  context);
}

/* ffeexpr_convert_to_sym -- Convert source expression to conform to symbol

   ffebld source;
   ffesymbol dest;
   ffelexToken source_token;
   ffelexToken dest_token;
   source = ffeexpr_convert_to_sym(source,source_token,dest,dest_token);

   If the expressions conform, returns the source expression.  Otherwise
   returns source wrapped in a convert node doing the conversion, or
   ANY wrapped in convert if there is a conversion error (and issues an
   error message).  */

ffebld
ffeexpr_convert_to_sym (ffebld source, ffelexToken source_token,
			ffesymbol dest, ffelexToken dest_token)
{
  return ffeexpr_convert (source, source_token, dest_token, ffesymbol_basictype (dest),
    ffesymbol_kindtype (dest), ffesymbol_rank (dest), ffesymbol_size (dest),
			  FFEEXPR_contextLET);
}

/* Initializes the module.  */

void
ffeexpr_init_2 ()
{
  ffeexpr_stack_ = NULL;
  ffeexpr_level_ = 0;
}

/* ffeexpr_lhs -- Begin processing left-hand-side-context expression

   Prepares cluster for delivery of lexer tokens representing an expression
   in a left-hand-side context (A in A=B, for example).	 ffebld is used
   to build expressions in the given pool.  The appropriate lexer-token
   handling routine within ffeexpr is returned.	 When the end of the
   expression is detected, mycallbackroutine is called with the resulting
   single ffebld object specifying the entire expression and the first
   lexer token that is not considered part of the expression.  This caller-
   supplied routine itself returns a lexer-token handling routine.  Thus,
   if necessary, ffeexpr can return several tokens as end-of-expression
   tokens if it needs to scan forward more than one in any instance.  */

ffelexHandler
ffeexpr_lhs (mallocPool pool, ffeexprContext context, ffeexprCallback callback)
{
  ffeexprStack_ s;

  ffebld_pool_push (pool);
  s = malloc_new_ks (ffe_pool_program_unit (), "FFEEXPR stack", sizeof (*s));
  s->previous = ffeexpr_stack_;
  s->pool = pool;
  s->context = context;
  s->callback = callback;
  s->first_token = NULL;
  s->exprstack = NULL;
  s->is_rhs = FALSE;
  ffeexpr_stack_ = s;
  return (ffelexHandler) ffeexpr_token_first_lhs_;
}

/* ffeexpr_rhs -- Begin processing right-hand-side-context expression

   return ffeexpr_rhs(malloc_pool_image(),mycallbackroutine);  // to lexer.

   Prepares cluster for delivery of lexer tokens representing an expression
   in a right-hand-side context (B in A=B, for example).  ffebld is used
   to build expressions in the given pool.  The appropriate lexer-token
   handling routine within ffeexpr is returned.	 When the end of the
   expression is detected, mycallbackroutine is called with the resulting
   single ffebld object specifying the entire expression and the first
   lexer token that is not considered part of the expression.  This caller-
   supplied routine itself returns a lexer-token handling routine.  Thus,
   if necessary, ffeexpr can return several tokens as end-of-expression
   tokens if it needs to scan forward more than one in any instance.  */

ffelexHandler
ffeexpr_rhs (mallocPool pool, ffeexprContext context, ffeexprCallback callback)
{
  ffeexprStack_ s;

  ffebld_pool_push (pool);
  s = malloc_new_ks (ffe_pool_program_unit (), "FFEEXPR stack", sizeof (*s));
  s->previous = ffeexpr_stack_;
  s->pool = pool;
  s->context = context;
  s->callback = callback;
  s->first_token = NULL;
  s->exprstack = NULL;
  s->is_rhs = TRUE;
  ffeexpr_stack_ = s;
  return (ffelexHandler) ffeexpr_token_first_rhs_;
}

/* ffeexpr_cb_close_paren_ -- OPEN_PAREN expr

   Pass it to ffeexpr_rhs as the callback routine.

   Makes sure the end token is close-paren and swallows it, else issues
   an error message and doesn't swallow the token (passing it along instead).
   In either case wraps up subexpression construction by enclosing the
   ffebld expression in a paren.  */

static ffelexHandler
ffeexpr_cb_close_paren_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  ffeexprExpr_ e;

  if (ffelex_token_type (t) != FFELEX_typeCLOSE_PAREN)
    {
      /* Oops, naughty user didn't specify the close paren! */

      if (ffest_ffebad_start (FFEBAD_MISSING_CLOSE_PAREN))
	{
	  ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_here (1, ffelex_token_where_line (ffeexpr_stack_->tokens[0]),
		       ffelex_token_where_column (ffeexpr_stack_->tokens[0]));
	  ffebad_finish ();
	}

      e = ffeexpr_expr_new_ ();
      e->type = FFEEXPR_exprtypeOPERAND_;
      e->u.operand = ffebld_new_any ();
      ffebld_set_info (e->u.operand, ffeinfo_new_any ());
      ffeexpr_exprstack_push_operand_ (e);

      return
	(ffelexHandler) ffeexpr_find_close_paren_ (t,
						   (ffelexHandler)
						   ffeexpr_token_binary_);
    }

  if (expr->op == FFEBLD_opIMPDO)
    {
      if (ffest_ffebad_start (FFEBAD_IMPDO_PAREN))
	{
	  ffebad_here (0, ffelex_token_where_line (ffeexpr_stack_->tokens[0]),
		       ffelex_token_where_column (ffeexpr_stack_->tokens[0]));
	  ffebad_finish ();
	}
    }
  else
    {
      expr = ffebld_new_paren (expr);
      ffebld_set_info (expr, ffeinfo_use (ffebld_info (ffebld_left (expr))));
    }

  /* Now push the (parenthesized) expression as an operand onto the
     expression stack. */

  e = ffeexpr_expr_new_ ();
  e->type = FFEEXPR_exprtypeOPERAND_;
  e->u.operand = expr;
  e->u.operand = ffeexpr_collapse_paren (e->u.operand, ft);
  e->token = ffeexpr_stack_->tokens[0];
  ffeexpr_exprstack_push_operand_ (e);

  return (ffelexHandler) ffeexpr_token_binary_;
}

/* ffeexpr_cb_close_paren_ambig_ -- OPEN_PAREN expr

   Pass it to ffeexpr_rhs as the callback routine.

   We get here in the READ/BACKEND/ENDFILE/REWIND case "READ(expr)"
   with the next token in t.  If the next token is possibly a binary
   operator, continue processing the outer expression.	If the next
   token is COMMA, then the expression is a unit specifier, and
   parentheses should not be added to it because it surrounds the
   I/O control list that starts with the unit specifier (and continues
   on from here -- we haven't seen the CLOSE_PAREN that matches the
   OPEN_PAREN, it is up to the callback function to expect to see it
   at some point).  In this case, we notify the callback function that
   the COMMA is inside, not outside, the parens by wrapping the expression
   in an opITEM (with a NULL trail) -- the callback function presumably
   unwraps it after seeing this kludgey indicator.

   If the next token is CLOSE_PAREN, then we go to the _1_ state to
   decide what to do with the token after that.

   15-Feb-91  JCB  1.1
      Use an extra state for the CLOSE_PAREN case to make READ &co really
      work right.  */

static ffelexHandler
ffeexpr_cb_close_paren_ambig_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  ffeexprCallback callback;
  ffeexprStack_ s;

  if (ffelex_token_type (t) == FFELEX_typeCLOSE_PAREN)
    {				/* Need to see the next token before we
				   decide anything. */
      ffeexpr_stack_->expr = expr;
      ffeexpr_tokens_[0] = ffelex_token_use (ft);
      ffeexpr_tokens_[1] = ffelex_token_use (t);
      return (ffelexHandler) ffeexpr_cb_close_paren_ambig_1_;
    }

  expr = ffeexpr_finished_ambig_ (ft, expr);

  /* Let the callback function handle the case where t isn't COMMA. */

  /* Here is a kludge whereby we tell the callback function the OPEN_PAREN
     that preceded the expression starts a list of expressions, and the expr
     hasn't been wrapped in a corresponding (and possibly collapsed) opPAREN
     node.  The callback function should extract the real expr from the head
     of this opITEM node after testing it. */

  expr = ffebld_new_item (expr, NULL);

  ffebld_pool_pop ();
  callback = ffeexpr_stack_->callback;
  ffelex_token_kill (ffeexpr_stack_->first_token);
  s = ffeexpr_stack_->previous;
  malloc_kill_ks (ffe_pool_program_unit (), ffeexpr_stack_, sizeof (*ffeexpr_stack_));
  ffeexpr_stack_ = s;
  return (ffelexHandler) (*callback) (ft, expr, t);
}

/* ffeexpr_cb_close_paren_ambig_1_ -- OPEN_PAREN expr CLOSE_PAREN

   See ffeexpr_cb_close_paren_ambig_.

   We get here in the READ/BACKEND/ENDFILE/REWIND case "READ(expr)"
   with the next token in t.  If the next token is possibly a binary
   operator, continue processing the outer expression.	If the next
   token is COMMA, the expression is a parenthesized format specifier.
   If the next token is not EOS or SEMICOLON, then because it is not a
   binary operator (it is NAME, OPEN_PAREN, &c), the expression is
   a unit specifier, and parentheses should not be added to it because
   they surround the I/O control list that consists of only the unit
   specifier.  If the next token is EOS or SEMICOLON, the statement
   must be disambiguated by looking at the type of the expression -- a
   character expression is a parenthesized format specifier, while a
   non-character expression is a unit specifier.

   Another issue is how to do the callback so the recipient of the
   next token knows how to handle it if it is a COMMA.	In all other
   cases, disambiguation is straightforward: the same approach as the
   above is used.

   EXTENSION: in COMMA case, if not pedantic, use same disambiguation
   as for EOS/SEMICOLON case; f2c allows "READ (cilist) [[,]iolist]"
   and apparently other compilers do, as well, and some code out there
   uses this "feature".

   19-Feb-91  JCB  1.1
      Extend to allow COMMA as nondisambiguating by itself.  Remember
      to not try and check info field for opSTAR, since that expr doesn't
      have a valid info field.	*/

static ffelexHandler
ffeexpr_cb_close_paren_ambig_1_ (ffelexToken t)
{
  ffeexprCallback callback;
  ffeexprStack_ s;
  ffelexHandler next;
  ffelexToken orig_ft = ffeexpr_tokens_[0];	/* In case callback clobbers
						   these. */
  ffelexToken orig_t = ffeexpr_tokens_[1];
  ffebld expr = ffeexpr_stack_->expr;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:	/* Subexpr is parenthesized format specifier. */
      if (ffe_is_pedantic ())
	goto pedantic_comma;	/* :::::::::::::::::::: */
      /* Fall through. */
    case FFELEX_typeEOS:	/* Ambiguous; use type of expr to
				   disambiguate. */
    case FFELEX_typeSEMICOLON:
      if ((expr == NULL) || (ffebld_op (expr) == FFEBLD_opANY)
	  || (ffebld_op (expr) == FFEBLD_opSTAR)
	  || (ffeinfo_basictype (ffebld_info (expr))
	      != FFEINFO_basictypeCHARACTER))
	break;			/* Not a valid CHARACTER entity, can't be a
				   format spec. */
      /* Fall through. */
    default:			/* Binary op (we assume; error otherwise);
				   format specifier. */

    pedantic_comma:		/* :::::::::::::::::::: */

      switch (ffeexpr_stack_->context)
	{
	case FFEEXPR_contextFILENUMAMBIG:
	  ffeexpr_stack_->context = FFEEXPR_contextFILENUM;
	  break;

	case FFEEXPR_contextFILEUNITAMBIG:
	  ffeexpr_stack_->context = FFEEXPR_contextFILEFORMAT;
	  break;

	default:
	  assert ("bad context" == NULL);
	  break;
	}

      ffeexpr_stack_->tokens[0] = ffelex_token_use (ffeexpr_stack_->first_token);
      next = (ffelexHandler) ffeexpr_cb_close_paren_ (orig_ft, expr, orig_t);
      ffelex_token_kill (orig_ft);
      ffelex_token_kill (orig_t);
      return (ffelexHandler) (*next) (t);

    case FFELEX_typeOPEN_PAREN:/* Non-binary op; beginning of I/O list. */
    case FFELEX_typeNAME:
      break;
    }

  expr = ffeexpr_finished_ambig_ (orig_ft, expr);

  /* Here is a kludge whereby we tell the callback function the OPEN_PAREN
     that preceded the expression starts a list of expressions, and the expr
     hasn't been wrapped in a corresponding (and possibly collapsed) opPAREN
     node.  The callback function should extract the real expr from the head
     of this opITEM node after testing it. */

  expr = ffebld_new_item (expr, NULL);

  ffebld_pool_pop ();
  callback = ffeexpr_stack_->callback;
  ffelex_token_kill (ffeexpr_stack_->first_token);
  s = ffeexpr_stack_->previous;
  malloc_kill_ks (ffe_pool_program_unit (), ffeexpr_stack_, sizeof (*ffeexpr_stack_));
  ffeexpr_stack_ = s;
  next = (ffelexHandler) (*callback) (orig_ft, expr, orig_t);
  ffelex_token_kill (orig_ft);
  ffelex_token_kill (orig_t);
  return (ffelexHandler) (*next) (t);
}

/* ffeexpr_cb_close_paren_c_ -- OPEN_PAREN expr (possible complex)

   Pass it to ffeexpr_rhs as the callback routine.

   Makes sure the end token is close-paren and swallows it, or a comma
   and handles complex/implied-do possibilities, else issues
   an error message and doesn't swallow the token (passing it along instead).  */

static ffelexHandler
ffeexpr_cb_close_paren_c_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  /* First check to see if this is a possible complex entity.  It is if the
     token is a comma. */

  if (ffelex_token_type (t) == FFELEX_typeCOMMA)
    {
      ffeexpr_stack_->tokens[1] = ffelex_token_use (ft);
      ffeexpr_stack_->expr = expr;
      return (ffelexHandler) ffeexpr_rhs (ffeexpr_stack_->pool,
				FFEEXPR_contextPAREN_, ffeexpr_cb_comma_c_);
    }

  return (ffelexHandler) ffeexpr_cb_close_paren_ (ft, expr, t);
}

/* ffeexpr_cb_comma_c_ -- OPEN_PAREN expr COMMA expr

   Pass it to ffeexpr_rhs as the callback routine.

   If this token is not a comma, we have a complex constant (or an attempt
   at one), so handle it accordingly, displaying error messages if the token
   is not a close-paren.  */

static ffelexHandler
ffeexpr_cb_comma_c_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  ffeexprExpr_ e;
  ffeinfoBasictype lty = (ffeexpr_stack_->expr == NULL)
    ? FFEINFO_basictypeNONE : ffeinfo_basictype (ffebld_info (ffeexpr_stack_->expr));
  ffeinfoBasictype rty = (expr == NULL)
    ? FFEINFO_basictypeNONE : ffeinfo_basictype (ffebld_info (expr));
  ffeinfoKindtype lkt;
  ffeinfoKindtype rkt;
  ffeinfoKindtype nkt;
  bool ok = TRUE;
  ffebld orig;

  if ((ffeexpr_stack_->expr == NULL)
      || (ffebld_op (ffeexpr_stack_->expr) != FFEBLD_opCONTER)
      || (((orig = ffebld_conter_orig (ffeexpr_stack_->expr)) != NULL)
	  && (((ffebld_op (orig) != FFEBLD_opUMINUS)
	       && (ffebld_op (orig) != FFEBLD_opUPLUS))
	      || (ffebld_conter_orig (ffebld_left (orig)) != NULL)))
      || ((lty != FFEINFO_basictypeINTEGER)
	  && (lty != FFEINFO_basictypeREAL)))
    {
      if ((lty != FFEINFO_basictypeANY)
	  && ffebad_start (FFEBAD_INVALID_COMPLEX_PART))
	{
	  ffebad_here (0, ffelex_token_where_line (ffeexpr_stack_->tokens[1]),
		     ffelex_token_where_column (ffeexpr_stack_->tokens[1]));
	  ffebad_string ("Real");
	  ffebad_finish ();
	}
      ok = FALSE;
    }
  if ((expr == NULL)
      || (ffebld_op (expr) != FFEBLD_opCONTER)
      || (((orig = ffebld_conter_orig (expr)) != NULL)
	  && (((ffebld_op (orig) != FFEBLD_opUMINUS)
	       && (ffebld_op (orig) != FFEBLD_opUPLUS))
	      || (ffebld_conter_orig (ffebld_left (orig)) != NULL)))
      || ((rty != FFEINFO_basictypeINTEGER)
	  && (rty != FFEINFO_basictypeREAL)))
    {
      if ((rty != FFEINFO_basictypeANY)
	  && ffebad_start (FFEBAD_INVALID_COMPLEX_PART))
	{
	  ffebad_here (0, ffelex_token_where_line (ft),
		       ffelex_token_where_column (ft));
	  ffebad_string ("Imaginary");
	  ffebad_finish ();
	}
      ok = FALSE;
    }

  ffelex_token_kill (ffeexpr_stack_->tokens[1]);

  /* Push the (parenthesized) expression as an operand onto the expression
     stack. */

  e = ffeexpr_expr_new_ ();
  e->type = FFEEXPR_exprtypeOPERAND_;
  e->token = ffeexpr_stack_->tokens[0];

  if (ok)
    {
      if (lty == FFEINFO_basictypeINTEGER)
	lkt = FFEINFO_kindtypeREALDEFAULT;
      else
	lkt = ffeinfo_kindtype (ffebld_info (ffeexpr_stack_->expr));
      if (rty == FFEINFO_basictypeINTEGER)
	rkt = FFEINFO_kindtypeREALDEFAULT;
      else
	rkt = ffeinfo_kindtype (ffebld_info (expr));

      nkt = ffeinfo_kindtype_max (FFEINFO_basictypeCOMPLEX, lkt, rkt);
      ffeexpr_stack_->expr = ffeexpr_convert (ffeexpr_stack_->expr,
		       ffeexpr_stack_->tokens[1], ffeexpr_stack_->tokens[0],
		 FFEINFO_basictypeREAL, nkt, 0, FFETARGET_charactersizeNONE,
					      FFEEXPR_contextLET);
      expr = ffeexpr_convert (expr,
		       ffeexpr_stack_->tokens[1], ffeexpr_stack_->tokens[0],
		 FFEINFO_basictypeREAL, nkt, 0, FFETARGET_charactersizeNONE,
			      FFEEXPR_contextLET);
    }
  else
    nkt = FFEINFO_kindtypeANY;

  switch (nkt)
    {
#if FFETARGET_okCOMPLEX1
    case FFEINFO_kindtypeREAL1:
      e->u.operand = ffebld_new_conter (ffebld_constant_new_complex1
	      (ffebld_conter (ffeexpr_stack_->expr), ffebld_conter (expr)));
      ffebld_set_info (e->u.operand,
		       ffeinfo_new (FFEINFO_basictypeCOMPLEX, nkt, 0,
				  FFEINFO_kindENTITY, FFEINFO_whereCONSTANT,
				    FFETARGET_charactersizeNONE));
      break;
#endif

#if FFETARGET_okCOMPLEX2
    case FFEINFO_kindtypeREAL2:
      e->u.operand = ffebld_new_conter (ffebld_constant_new_complex2
	      (ffebld_conter (ffeexpr_stack_->expr), ffebld_conter (expr)));
      ffebld_set_info (e->u.operand,
		       ffeinfo_new (FFEINFO_basictypeCOMPLEX, nkt, 0,
				  FFEINFO_kindENTITY, FFEINFO_whereCONSTANT,
				    FFETARGET_charactersizeNONE));
      break;
#endif

#if FFETARGET_okCOMPLEX3
    case FFEINFO_kindtypeREAL3:
      e->u.operand = ffebld_new_conter (ffebld_constant_new_complex3
	      (ffebld_conter (ffeexpr_stack_->expr), ffebld_conter (expr)));
      ffebld_set_info (e->u.operand,
		       ffeinfo_new (FFEINFO_basictypeCOMPLEX, nkt, 0,
				  FFEINFO_kindENTITY, FFEINFO_whereCONSTANT,
				    FFETARGET_charactersizeNONE));
      break;
#endif

#if FFETARGET_okCOMPLEX4
    case FFEINFO_kindtypeREAL4:
      e->u.operand = ffebld_new_conter (ffebld_constant_new_complex4
	      (ffebld_conter (ffeexpr_stack_->expr), ffebld_conter (expr)));
      ffebld_set_info (e->u.operand,
		       ffeinfo_new (FFEINFO_basictypeCOMPLEX, nkt, 0,
				  FFEINFO_kindENTITY, FFEINFO_whereCONSTANT,
				    FFETARGET_charactersizeNONE));
      break;
#endif

    default:
      if (ffebad_start ((nkt == FFEINFO_kindtypeREALDOUBLE)
			? FFEBAD_BAD_DBLCMPLX : FFEBAD_BAD_COMPLEX))
	{
	  ffebad_here (0, ffelex_token_where_line (ffeexpr_stack_->tokens[0]),
		     ffelex_token_where_column (ffeexpr_stack_->tokens[0]));
	  ffebad_finish ();
	}
      /* Fall through. */
    case FFEINFO_kindtypeANY:
      e->u.operand = ffebld_new_any ();
      ffebld_set_info (e->u.operand, ffeinfo_new_any ());
      break;
    }
  ffeexpr_exprstack_push_operand_ (e);

  /* Now, if the token is a close parenthese, we're in great shape so return
     the next handler. */

  if (ffelex_token_type (t) == FFELEX_typeCLOSE_PAREN)
    return (ffelexHandler) ffeexpr_token_binary_;

  /* Oops, naughty user didn't specify the close paren! */

  if (ffest_ffebad_start (FFEBAD_MISSING_CLOSE_PAREN))
    {
      ffebad_here (0, ffelex_token_where_line (t),
		   ffelex_token_where_column (t));
      ffebad_here (1, ffelex_token_where_line (ffeexpr_stack_->tokens[0]),
		   ffelex_token_where_column (ffeexpr_stack_->tokens[0]));
      ffebad_finish ();
    }

  return
    (ffelexHandler) ffeexpr_find_close_paren_ (t,
					       (ffelexHandler)
					       ffeexpr_token_binary_);
}

/* ffeexpr_cb_close_paren_ci_ -- OPEN_PAREN expr (possible complex or
				    implied-DO construct)

   Pass it to ffeexpr_rhs as the callback routine.

   Makes sure the end token is close-paren and swallows it, or a comma
   and handles complex/implied-do possibilities, else issues
   an error message and doesn't swallow the token (passing it along instead).  */

static ffelexHandler
ffeexpr_cb_close_paren_ci_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  ffeexprContext ctx;

  /* First check to see if this is a possible complex or implied-DO entity.
     It is if the token is a comma. */

  if (ffelex_token_type (t) == FFELEX_typeCOMMA)
    {
      switch (ffeexpr_stack_->context)
	{
	case FFEEXPR_contextIOLIST:
	case FFEEXPR_contextIMPDOITEM_:
	  ctx = FFEEXPR_contextIMPDOITEM_;
	  break;

	case FFEEXPR_contextIOLISTDF:
	case FFEEXPR_contextIMPDOITEMDF_:
	  ctx = FFEEXPR_contextIMPDOITEMDF_;
	  break;

	default:
	  assert ("bad context" == NULL);
	  ctx = FFEEXPR_contextIMPDOITEM_;
	  break;
	}

      ffeexpr_stack_->tokens[0] = ffelex_token_use (ft);
      ffeexpr_stack_->expr = expr;
      return (ffelexHandler) ffeexpr_rhs (ffeexpr_stack_->pool,
					  ctx, ffeexpr_cb_comma_ci_);
    }

  ffeexpr_stack_->tokens[0] = ffelex_token_use (ffeexpr_stack_->first_token);
  return (ffelexHandler) ffeexpr_cb_close_paren_ (ft, expr, t);
}

/* ffeexpr_cb_comma_ci_ -- OPEN_PAREN expr COMMA expr

   Pass it to ffeexpr_rhs as the callback routine.

   If this token is not a comma, we have a complex constant (or an attempt
   at one), so handle it accordingly, displaying error messages if the token
   is not a close-paren.  If we have a comma here, it is an attempt at an
   implied-DO, so start making a list accordingly.  Oh, it might be an
   equal sign also, meaning an implied-DO with only one item in its list.  */

static ffelexHandler
ffeexpr_cb_comma_ci_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  ffebld fexpr;

  /* First check to see if this is a possible complex constant.	 It is if the
     token is not a comma or an equals sign, in which case it should be a
     close-paren. */

  if ((ffelex_token_type (t) != FFELEX_typeCOMMA)
      && (ffelex_token_type (t) != FFELEX_typeEQUALS))
    {
      ffeexpr_stack_->tokens[1] = ffeexpr_stack_->tokens[0];
      ffeexpr_stack_->tokens[0] = ffelex_token_use (ffeexpr_stack_->first_token);
      return (ffelexHandler) ffeexpr_cb_comma_c_ (ft, expr, t);
    }

  /* Here we have either EQUALS or COMMA, meaning we are in an implied-DO
     construct.	 Make a list and handle accordingly. */

  ffelex_token_kill (ffeexpr_stack_->tokens[0]);
  fexpr = ffeexpr_stack_->expr;
  ffebld_init_list (&ffeexpr_stack_->expr, &ffeexpr_stack_->bottom);
  ffebld_append_item (&ffeexpr_stack_->bottom, fexpr);
  return (ffelexHandler) ffeexpr_cb_comma_i_1_ (ft, expr, t);
}

/* ffeexpr_cb_comma_i_ -- OPEN_PAREN expr

   Pass it to ffeexpr_rhs as the callback routine.

   Handle first item in an implied-DO construct.  */

static ffelexHandler
ffeexpr_cb_comma_i_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  if (ffelex_token_type (t) != FFELEX_typeCOMMA)
    {
      if (ffest_ffebad_start (FFEBAD_BAD_IMPDO))
	{
	  ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_here (1, ffelex_token_where_line (ffeexpr_stack_->first_token),
		   ffelex_token_where_column (ffeexpr_stack_->first_token));
	  ffebad_finish ();
	}
      ffebld_end_list (&ffeexpr_stack_->bottom);
      ffeexpr_stack_->expr = ffebld_new_any ();
      ffebld_set_info (ffeexpr_stack_->expr, ffeinfo_new_any ());
      if (ffelex_token_type (t) != FFELEX_typeCLOSE_PAREN)
	return (ffelexHandler) ffeexpr_cb_comma_i_5_ (t);
      return (ffelexHandler) ffeexpr_cb_comma_i_5_;
    }

  return (ffelexHandler) ffeexpr_cb_comma_i_1_ (ft, expr, t);
}

/* ffeexpr_cb_comma_i_1_ -- OPEN_PAREN expr

   Pass it to ffeexpr_rhs as the callback routine.

   Handle first item in an implied-DO construct.  */

static ffelexHandler
ffeexpr_cb_comma_i_1_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  ffeexprContext ctxi;
  ffeexprContext ctxc;

  switch (ffeexpr_stack_->context)
    {
    case FFEEXPR_contextDATA:
    case FFEEXPR_contextDATAIMPDOITEM_:
      ctxi = FFEEXPR_contextDATAIMPDOITEM_;
      ctxc = FFEEXPR_contextDATAIMPDOCTRL_;
      break;

    case FFEEXPR_contextIOLIST:
    case FFEEXPR_contextIMPDOITEM_:
      ctxi = FFEEXPR_contextIMPDOITEM_;
      ctxc = FFEEXPR_contextIMPDOCTRL_;
      break;

    case FFEEXPR_contextIOLISTDF:
    case FFEEXPR_contextIMPDOITEMDF_:
      ctxi = FFEEXPR_contextIMPDOITEMDF_;
      ctxc = FFEEXPR_contextIMPDOCTRL_;
      break;

    default:
      assert ("bad context" == NULL);
      ctxi = FFEEXPR_context;
      ctxc = FFEEXPR_context;
      break;
    }

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      ffebld_append_item (&ffeexpr_stack_->bottom, expr);
      if (ffeexpr_stack_->is_rhs)
	return (ffelexHandler) ffeexpr_rhs (ffeexpr_stack_->pool,
					    ctxi, ffeexpr_cb_comma_i_1_);
      return (ffelexHandler) ffeexpr_lhs (ffeexpr_stack_->pool,
					  ctxi, ffeexpr_cb_comma_i_1_);

    case FFELEX_typeEQUALS:
      ffebld_end_list (&ffeexpr_stack_->bottom);

      /* Complain if implied-DO variable in list of items to be read.  */

      if ((ctxc == FFEEXPR_contextIMPDOCTRL_) && !ffeexpr_stack_->is_rhs)
	ffeexpr_check_impdo_ (ffeexpr_stack_->expr,
			      ffeexpr_stack_->first_token, expr, ft);

      /* Set doiter flag for all appropriate SYMTERs.  */

      ffeexpr_update_impdo_ (ffeexpr_stack_->expr, expr);

      ffeexpr_stack_->expr = ffebld_new_impdo (ffeexpr_stack_->expr, NULL);
      ffebld_set_info (ffeexpr_stack_->expr,
		       ffeinfo_new (FFEINFO_basictypeNONE,
				    FFEINFO_kindtypeNONE,
				    0,
				    FFEINFO_kindNONE,
				    FFEINFO_whereNONE,
				    FFETARGET_charactersizeNONE));
      ffebld_init_list (&(ffebld_right (ffeexpr_stack_->expr)),
			&ffeexpr_stack_->bottom);
      ffebld_append_item (&ffeexpr_stack_->bottom, expr);
      return (ffelexHandler) ffeexpr_rhs (ffeexpr_stack_->pool,
					  ctxc, ffeexpr_cb_comma_i_2_);

    default:
      if (ffest_ffebad_start (FFEBAD_BAD_IMPDO))
	{
	  ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_here (1, ffelex_token_where_line (ffeexpr_stack_->first_token),
		   ffelex_token_where_column (ffeexpr_stack_->first_token));
	  ffebad_finish ();
	}
      ffebld_end_list (&ffeexpr_stack_->bottom);
      ffeexpr_stack_->expr = ffebld_new_any ();
      ffebld_set_info (ffeexpr_stack_->expr, ffeinfo_new_any ());
      if (ffelex_token_type (t) != FFELEX_typeCLOSE_PAREN)
	return (ffelexHandler) ffeexpr_cb_comma_i_5_ (t);
      return (ffelexHandler) ffeexpr_cb_comma_i_5_;
    }
}

/* ffeexpr_cb_comma_i_2_ -- OPEN_PAREN expr-list EQUALS expr

   Pass it to ffeexpr_rhs as the callback routine.

   Handle start-value in an implied-DO construct.  */

static ffelexHandler
ffeexpr_cb_comma_i_2_ (ffelexToken ft UNUSED, ffebld expr, ffelexToken t)
{
  ffeexprContext ctx;

  switch (ffeexpr_stack_->context)
    {
    case FFEEXPR_contextDATA:
    case FFEEXPR_contextDATAIMPDOITEM_:
      ctx = FFEEXPR_contextDATAIMPDOCTRL_;
      break;

    case FFEEXPR_contextIOLIST:
    case FFEEXPR_contextIOLISTDF:
    case FFEEXPR_contextIMPDOITEM_:
    case FFEEXPR_contextIMPDOITEMDF_:
      ctx = FFEEXPR_contextIMPDOCTRL_;
      break;

    default:
      assert ("bad context" == NULL);
      ctx = FFEEXPR_context;
      break;
    }

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      ffebld_append_item (&ffeexpr_stack_->bottom, expr);
      return (ffelexHandler) ffeexpr_rhs (ffeexpr_stack_->pool,
					  ctx, ffeexpr_cb_comma_i_3_);
      break;

    default:
      if (ffest_ffebad_start (FFEBAD_BAD_IMPDO))
	{
	  ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_here (1, ffelex_token_where_line (ffeexpr_stack_->first_token),
		   ffelex_token_where_column (ffeexpr_stack_->first_token));
	  ffebad_finish ();
	}
      ffebld_end_list (&ffeexpr_stack_->bottom);
      ffeexpr_stack_->expr = ffebld_new_any ();
      ffebld_set_info (ffeexpr_stack_->expr, ffeinfo_new_any ());
      if (ffelex_token_type (t) != FFELEX_typeCLOSE_PAREN)
	return (ffelexHandler) ffeexpr_cb_comma_i_5_ (t);
      return (ffelexHandler) ffeexpr_cb_comma_i_5_;
    }
}

/* ffeexpr_cb_comma_i_3_ -- OPEN_PAREN expr-list EQUALS expr COMMA expr

   Pass it to ffeexpr_rhs as the callback routine.

   Handle end-value in an implied-DO construct.	 */

static ffelexHandler
ffeexpr_cb_comma_i_3_ (ffelexToken ft UNUSED, ffebld expr, ffelexToken t)
{
  ffeexprContext ctx;

  switch (ffeexpr_stack_->context)
    {
    case FFEEXPR_contextDATA:
    case FFEEXPR_contextDATAIMPDOITEM_:
      ctx = FFEEXPR_contextDATAIMPDOCTRL_;
      break;

    case FFEEXPR_contextIOLIST:
    case FFEEXPR_contextIOLISTDF:
    case FFEEXPR_contextIMPDOITEM_:
    case FFEEXPR_contextIMPDOITEMDF_:
      ctx = FFEEXPR_contextIMPDOCTRL_;
      break;

    default:
      assert ("bad context" == NULL);
      ctx = FFEEXPR_context;
      break;
    }

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      ffebld_append_item (&ffeexpr_stack_->bottom, expr);
      return (ffelexHandler) ffeexpr_rhs (ffeexpr_stack_->pool,
					  ctx, ffeexpr_cb_comma_i_4_);
      break;

    case FFELEX_typeCLOSE_PAREN:
      ffebld_append_item (&ffeexpr_stack_->bottom, expr);
      return (ffelexHandler) ffeexpr_cb_comma_i_4_ (NULL, NULL, t);
      break;

    default:
      if (ffest_ffebad_start (FFEBAD_BAD_IMPDO))
	{
	  ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_here (1, ffelex_token_where_line (ffeexpr_stack_->first_token),
		   ffelex_token_where_column (ffeexpr_stack_->first_token));
	  ffebad_finish ();
	}
      ffebld_end_list (&ffeexpr_stack_->bottom);
      ffeexpr_stack_->expr = ffebld_new_any ();
      ffebld_set_info (ffeexpr_stack_->expr, ffeinfo_new_any ());
      if (ffelex_token_type (t) != FFELEX_typeCLOSE_PAREN)
	return (ffelexHandler) ffeexpr_cb_comma_i_5_ (t);
      return (ffelexHandler) ffeexpr_cb_comma_i_5_;
    }
}

/* ffeexpr_cb_comma_i_4_ -- OPEN_PAREN expr-list EQUALS expr COMMA expr
			       [COMMA expr]

   Pass it to ffeexpr_rhs as the callback routine.

   Handle incr-value in an implied-DO construct.  */

static ffelexHandler
ffeexpr_cb_comma_i_4_ (ffelexToken ft UNUSED, ffebld expr, ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCLOSE_PAREN:
      ffebld_append_item (&ffeexpr_stack_->bottom, expr);
      ffebld_end_list (&ffeexpr_stack_->bottom);
      {
	ffebld item;

	for (item = ffebld_left (ffeexpr_stack_->expr);
	     item != NULL;
	     item = ffebld_trail (item))
	  if (ffebld_op (ffebld_head (item)) == FFEBLD_opANY)
	    goto replace_with_any;	/* :::::::::::::::::::: */

	for (item = ffebld_right (ffeexpr_stack_->expr);
	     item != NULL;
	     item = ffebld_trail (item))
	  if ((ffebld_head (item) != NULL)	/* Increment may be NULL. */
	      && (ffebld_op (ffebld_head (item)) == FFEBLD_opANY))
	    goto replace_with_any;	/* :::::::::::::::::::: */
      }
      break;

    default:
      if (ffest_ffebad_start (FFEBAD_BAD_IMPDO))
	{
	  ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_here (1, ffelex_token_where_line (ffeexpr_stack_->first_token),
		   ffelex_token_where_column (ffeexpr_stack_->first_token));
	  ffebad_finish ();
	}
      ffebld_end_list (&ffeexpr_stack_->bottom);

    replace_with_any:		/* :::::::::::::::::::: */

      ffeexpr_stack_->expr = ffebld_new_any ();
      ffebld_set_info (ffeexpr_stack_->expr, ffeinfo_new_any ());
      break;
    }

  if (ffelex_token_type (t) == FFELEX_typeCLOSE_PAREN)
    return (ffelexHandler) ffeexpr_cb_comma_i_5_;
  return (ffelexHandler) ffeexpr_cb_comma_i_5_ (t);
}

/* ffeexpr_cb_comma_i_5_ -- OPEN_PAREN expr-list EQUALS expr COMMA expr
			       [COMMA expr] CLOSE_PAREN

   Pass it to ffeexpr_rhs as the callback routine.

   Collects token following implied-DO construct for callback function.	 */

static ffelexHandler
ffeexpr_cb_comma_i_5_ (ffelexToken t)
{
  ffeexprCallback callback;
  ffeexprStack_ s;
  ffelexHandler next;
  ffelexToken ft;
  ffebld expr;
  bool terminate;

  switch (ffeexpr_stack_->context)
    {
    case FFEEXPR_contextDATA:
    case FFEEXPR_contextDATAIMPDOITEM_:
      terminate = TRUE;
      break;

    case FFEEXPR_contextIOLIST:
    case FFEEXPR_contextIOLISTDF:
    case FFEEXPR_contextIMPDOITEM_:
    case FFEEXPR_contextIMPDOITEMDF_:
      terminate = FALSE;
      break;

    default:
      assert ("bad context" == NULL);
      terminate = FALSE;
      break;
    }

  ffebld_pool_pop ();
  callback = ffeexpr_stack_->callback;
  ft = ffeexpr_stack_->first_token;
  expr = ffeexpr_stack_->expr;
  s = ffeexpr_stack_->previous;
  malloc_kill_ks (ffe_pool_program_unit (), ffeexpr_stack_,
		  sizeof (*ffeexpr_stack_));
  ffeexpr_stack_ = s;
  next = (ffelexHandler) (*callback) (ft, expr, t);
  ffelex_token_kill (ft);
  if (terminate)
    {
      ffesymbol_drive_sfnames (ffeexpr_check_impctrl_);
      --ffeexpr_level_;
      if (ffeexpr_level_ == 0)
	ffe_terminate_4 ();
    }
  return (ffelexHandler) next;
}

/* ffeexpr_cb_end_loc_ -- Handle end of %LOC subexpression

   Makes sure the end token is close-paren and swallows it, else issues
   an error message and doesn't swallow the token (passing it along instead).
   In either case wraps up subexpression construction by enclosing the
   ffebld expression in a %LOC.	 */

static ffelexHandler
ffeexpr_cb_end_loc_ (ffelexToken ft UNUSED, ffebld expr, ffelexToken t)
{
  ffeexprExpr_ e;

  /* First push the (%LOC) expression as an operand onto the expression
     stack. */

  e = ffeexpr_expr_new_ ();
  e->type = FFEEXPR_exprtypeOPERAND_;
  e->token = ffeexpr_stack_->tokens[0];
  e->u.operand = ffebld_new_percent_loc (expr);
  ffebld_set_info (e->u.operand,
		   ffeinfo_new (FFEINFO_basictypeINTEGER,
				ffecom_pointer_kind (),
				0,
				FFEINFO_kindENTITY,
				FFEINFO_whereFLEETING,
				FFETARGET_charactersizeNONE));
#if 0				/* ~~ */
  e->u.operand = ffeexpr_collapse_percent_loc (e->u.operand, ft);
#endif
  ffeexpr_exprstack_push_operand_ (e);

  /* Now, if the token is a close parenthese, we're in great shape so return
     the next handler. */

  if (ffelex_token_type (t) == FFELEX_typeCLOSE_PAREN)
    {
      ffelex_token_kill (ffeexpr_stack_->tokens[1]);
      return (ffelexHandler) ffeexpr_token_binary_;
    }

  /* Oops, naughty user didn't specify the close paren! */

  if (ffest_ffebad_start (FFEBAD_MISSING_CLOSE_PAREN))
    {
      ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
      ffebad_here (1, ffelex_token_where_line (ffeexpr_stack_->tokens[1]),
		   ffelex_token_where_column (ffeexpr_stack_->tokens[1]));
      ffebad_finish ();
    }

  ffelex_token_kill (ffeexpr_stack_->tokens[1]);
  return
    (ffelexHandler) ffeexpr_find_close_paren_ (t,
					       (ffelexHandler)
					       ffeexpr_token_binary_);
}

/* ffeexpr_cb_end_notloc_ -- PERCENT NAME(VAL,REF,DESCR) OPEN_PAREN expr

   Should be CLOSE_PAREN, and make sure expr isn't a %(VAL,REF,DESCR).  */

static ffelexHandler
ffeexpr_cb_end_notloc_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  ffeexprExpr_ e;
  ffebldOp op;

  /* If expression is itself a %(VAL,REF,DESCR), complain and strip off all
     such things until the lowest-level expression is reached.  */

  op = ffebld_op (expr);
  if ((op == FFEBLD_opPERCENT_VAL) || (op == FFEBLD_opPERCENT_REF)
      || (op == FFEBLD_opPERCENT_DESCR))
    {
      if (ffebad_start (FFEBAD_NESTED_PERCENT))
	{
	  ffebad_here (0, ffelex_token_where_line (ft),
		       ffelex_token_where_column (ft));
	  ffebad_finish ();
	}

      do
	{
	  expr = ffebld_left (expr);
	  op = ffebld_op (expr);
	}
      while ((op == FFEBLD_opPERCENT_VAL) || (op == FFEBLD_opPERCENT_REF)
	     || (op == FFEBLD_opPERCENT_DESCR));
    }

  /* Push the expression as an operand onto the expression stack. */

  e = ffeexpr_expr_new_ ();
  e->type = FFEEXPR_exprtypeOPERAND_;
  e->token = ffeexpr_stack_->tokens[0];
  switch (ffeexpr_stack_->percent)
    {
    case FFEEXPR_percentVAL_:
      e->u.operand = ffebld_new_percent_val (expr);
      break;

    case FFEEXPR_percentREF_:
      e->u.operand = ffebld_new_percent_ref (expr);
      break;

    case FFEEXPR_percentDESCR_:
      e->u.operand = ffebld_new_percent_descr (expr);
      break;

    default:
      assert ("%lossage" == NULL);
      e->u.operand = expr;
      break;
    }
  ffebld_set_info (e->u.operand, ffebld_info (expr));
#if 0				/* ~~ */
  e->u.operand = ffeexpr_collapse_percent_ ? ? ? (e->u.operand, ft);
#endif
  ffeexpr_exprstack_push_operand_ (e);

  /* Now, if the token is a close parenthese, we're in great shape so return
     the next handler. */

  if (ffelex_token_type (t) == FFELEX_typeCLOSE_PAREN)
    return (ffelexHandler) ffeexpr_cb_end_notloc_1_;

  /* Oops, naughty user didn't specify the close paren! */

  if (ffest_ffebad_start (FFEBAD_MISSING_CLOSE_PAREN))
    {
      ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
      ffebad_here (1, ffelex_token_where_line (ffeexpr_stack_->tokens[1]),
		   ffelex_token_where_column (ffeexpr_stack_->tokens[1]));
      ffebad_finish ();
    }

  ffebld_set_op (e->u.operand, FFEBLD_opPERCENT_LOC);

  switch (ffeexpr_stack_->context)
    {
    case FFEEXPR_contextACTUALARG_:
      ffeexpr_stack_->context = FFEEXPR_contextACTUALARGEXPR_;
      break;

    case FFEEXPR_contextINDEXORACTUALARG_:
      ffeexpr_stack_->context = FFEEXPR_contextINDEXORACTUALARGEXPR_;
      break;

    case FFEEXPR_contextSFUNCDEFACTUALARG_:
      ffeexpr_stack_->context = FFEEXPR_contextSFUNCDEFACTUALARGEXPR_;
      break;

    case FFEEXPR_contextSFUNCDEFINDEXORACTUALARG_:
      ffeexpr_stack_->context = FFEEXPR_contextSFUNCDEFINDEXORACTUALARGEXPR_;
      break;

    default:
      assert ("bad context?!?!" == NULL);
      break;
    }

  ffelex_token_kill (ffeexpr_stack_->tokens[1]);
  return
    (ffelexHandler) ffeexpr_find_close_paren_ (t,
					       (ffelexHandler)
					       ffeexpr_cb_end_notloc_1_);
}

/* ffeexpr_cb_end_notloc_1_ -- PERCENT NAME(VAL,REF,DESCR) OPEN_PAREN expr
   CLOSE_PAREN

   Should be COMMA or CLOSE_PAREN, else change back to %LOC.  */

static ffelexHandler
ffeexpr_cb_end_notloc_1_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
    case FFELEX_typeCLOSE_PAREN:
      switch (ffeexpr_stack_->context)
	{
	case FFEEXPR_contextACTUALARG_:
	case FFEEXPR_contextSFUNCDEFACTUALARG_:
	  break;

	case FFEEXPR_contextINDEXORACTUALARG_:
	  ffeexpr_stack_->context = FFEEXPR_contextACTUALARG_;
	  break;

	case FFEEXPR_contextSFUNCDEFINDEXORACTUALARG_:
	  ffeexpr_stack_->context = FFEEXPR_contextSFUNCDEFACTUALARG_;
	  break;

	default:
	  assert ("bad context?!?!" == NULL);
	  break;
	}
      break;

    default:
      if (ffebad_start (FFEBAD_INVALID_PERCENT))
	{
	  ffebad_here (0,
		       ffelex_token_where_line (ffeexpr_stack_->first_token),
		   ffelex_token_where_column (ffeexpr_stack_->first_token));
	  ffebad_string (ffelex_token_text (ffeexpr_stack_->tokens[1]));
	  ffebad_finish ();
	}

      ffebld_set_op (ffeexpr_stack_->exprstack->u.operand,
		     FFEBLD_opPERCENT_LOC);

      switch (ffeexpr_stack_->context)
	{
	case FFEEXPR_contextACTUALARG_:
	  ffeexpr_stack_->context = FFEEXPR_contextACTUALARGEXPR_;
	  break;

	case FFEEXPR_contextINDEXORACTUALARG_:
	  ffeexpr_stack_->context = FFEEXPR_contextINDEXORACTUALARGEXPR_;
	  break;

	case FFEEXPR_contextSFUNCDEFACTUALARG_:
	  ffeexpr_stack_->context = FFEEXPR_contextSFUNCDEFACTUALARGEXPR_;
	  break;

	case FFEEXPR_contextSFUNCDEFINDEXORACTUALARG_:
	  ffeexpr_stack_->context = FFEEXPR_contextSFUNCDEFINDEXORACTUALARGEXPR_;
	  break;

	default:
	  assert ("bad context?!?!" == NULL);
	  break;
	}
    }

  ffelex_token_kill (ffeexpr_stack_->tokens[1]);
  return
    (ffelexHandler) ffeexpr_token_binary_ (t);
}

/* Process DATA implied-DO iterator variables as this implied-DO level
   terminates.  At this point, ffeexpr_level_ == 1 when we see the
   last right-paren in "DATA (A(I),I=1,10)/.../".  */

static ffesymbol
ffeexpr_check_impctrl_ (ffesymbol s)
{
  assert (s != NULL);
  assert (ffesymbol_sfdummyparent (s) != NULL);

  switch (ffesymbol_state (s))
    {
    case FFESYMBOL_stateNONE:	/* Used as iterator already. Now let symbol
				   be used as iterator at any level at or
				   innermore than the outermost of the
				   current level and the symbol's current
				   level. */
      if (ffeexpr_level_ < ffesymbol_maxentrynum (s))
	{
	  ffesymbol_signal_change (s);
	  ffesymbol_set_maxentrynum (s, ffeexpr_level_);
	  ffesymbol_signal_unreported (s);
	}
      break;

    case FFESYMBOL_stateSEEN:	/* Seen already in this or other implied-DO.
				   Error if at outermost level, else it can
				   still become an iterator. */
      if ((ffeexpr_level_ == 1)
	  && ffebad_start (FFEBAD_BAD_IMPDCL))
	{
	  ffebad_string (ffesymbol_text (s));
	  ffebad_here (0, ffesymbol_where_line (s), ffesymbol_where_column (s));
	  ffebad_finish ();
	}
      break;

    case FFESYMBOL_stateUNCERTAIN:	/* Iterator. */
      assert (ffeexpr_level_ <= ffesymbol_maxentrynum (s));
      ffesymbol_signal_change (s);
      ffesymbol_set_state (s, FFESYMBOL_stateNONE);
      ffesymbol_signal_unreported (s);
      break;

    case FFESYMBOL_stateUNDERSTOOD:
      break;			/* ANY. */

    default:
      assert ("Sasha Foo!!" == NULL);
      break;
    }

  return s;
}

/* Issue diagnostic if implied-DO variable appears in list of lhs
   expressions (as in "READ *, (I,I=1,10)").  */

static void
ffeexpr_check_impdo_ (ffebld list, ffelexToken list_t,
		      ffebld dovar, ffelexToken dovar_t)
{
  ffebld item;
  ffesymbol dovar_sym;
  int itemnum;

  if (ffebld_op (dovar) != FFEBLD_opSYMTER)
    return;			/* Presumably opANY. */

  dovar_sym = ffebld_symter (dovar);

  for (itemnum = 1; list != NULL; list = ffebld_trail (list), ++itemnum)
    {
      if (((item = ffebld_head (list)) != NULL)
	  && (ffebld_op (item) == FFEBLD_opSYMTER)
	  && (ffebld_symter (item) == dovar_sym))
	{
	  char itemno[20];

	  sprintf (&itemno[0], "%d", itemnum);
	  if (ffebad_start (FFEBAD_DOITER_IMPDO))
	    {
	      ffebad_here (0, ffelex_token_where_line (list_t),
			   ffelex_token_where_column (list_t));
	      ffebad_here (1, ffelex_token_where_line (dovar_t),
			   ffelex_token_where_column (dovar_t));
	      ffebad_string (ffesymbol_text (dovar_sym));
	      ffebad_string (itemno);
	      ffebad_finish ();
	    }
	}
    }
}

/* Decorate any SYMTERs referencing the DO variable with the "doiter"
   flag.  */

static void
ffeexpr_update_impdo_ (ffebld list, ffebld dovar)
{
  ffesymbol dovar_sym;

  if (ffebld_op (dovar) != FFEBLD_opSYMTER)
    return;			/* Presumably opANY. */

  dovar_sym = ffebld_symter (dovar);

  ffeexpr_update_impdo_sym_ (list, dovar_sym);	/* Recurse! */
}

/* Recursive function to update any expr so SYMTERs have "doiter" flag
   if they refer to the given variable.	 */

static void
ffeexpr_update_impdo_sym_ (ffebld expr, ffesymbol dovar)
{
  tail_recurse:			/* :::::::::::::::::::: */

  if (expr == NULL)
    return;

  switch (ffebld_op (expr))
    {
    case FFEBLD_opSYMTER:
      if (ffebld_symter (expr) == dovar)
	ffebld_symter_set_is_doiter (expr, TRUE);
      break;

    case FFEBLD_opITEM:
      ffeexpr_update_impdo_sym_ (ffebld_head (expr), dovar);
      expr = ffebld_trail (expr);
      goto tail_recurse;	/* :::::::::::::::::::: */

    default:
      break;
    }

  switch (ffebld_arity (expr))
    {
    case 2:
      ffeexpr_update_impdo_sym_ (ffebld_left (expr), dovar);
      expr = ffebld_right (expr);
      goto tail_recurse;	/* :::::::::::::::::::: */

    case 1:
      expr = ffebld_left (expr);
      goto tail_recurse;	/* :::::::::::::::::::: */

    default:
      break;
    }

  return;
}

/* ffeexpr_context_outer_ -- Determine context of stack entry, skipping PARENs

   if (ffeexpr_context_outer_(ffeexpr_stack_) == FFEEXPR_contextIF)
       // After zero or more PAREN_ contexts, an IF context exists  */

static ffeexprContext
ffeexpr_context_outer_ (ffeexprStack_ s)
{
  assert (s != NULL);

  for (;;)
    {
      switch (s->context)
	{
	case FFEEXPR_contextPAREN_:
	case FFEEXPR_contextPARENFILENUM_:
	case FFEEXPR_contextPARENFILEUNIT_:
	  break;

	default:
	  return s->context;
	}
      s = s->previous;
      assert (s != NULL);
    }
}

/* ffeexpr_percent_ -- Look up name in list of %FOO possibilities

   ffeexprPercent_ p;
   ffelexToken t;
   p = ffeexpr_percent_(t);

   Returns the identifier for the name, or the NONE identifier.	 */

static ffeexprPercent_
ffeexpr_percent_ (ffelexToken t)
{
  const char *p;

  switch (ffelex_token_length (t))
    {
    case 3:
      switch (*(p = ffelex_token_text (t)))
	{
	case FFESRC_CASE_MATCH_INIT ('L', 'l', match_3l, no_match_3):
	  if ((ffesrc_char_match_noninit (*++p, 'O', 'o'))
	      && (ffesrc_char_match_noninit (*++p, 'C', 'c')))
	    return FFEEXPR_percentLOC_;
	  return FFEEXPR_percentNONE_;

	case FFESRC_CASE_MATCH_INIT ('R', 'r', match_3r, no_match_3):
	  if ((ffesrc_char_match_noninit (*++p, 'E', 'e'))
	      && (ffesrc_char_match_noninit (*++p, 'F', 'f')))
	    return FFEEXPR_percentREF_;
	  return FFEEXPR_percentNONE_;

	case FFESRC_CASE_MATCH_INIT ('V', 'v', match_3v, no_match_3):
	  if ((ffesrc_char_match_noninit (*++p, 'A', 'a'))
	      && (ffesrc_char_match_noninit (*++p, 'L', 'l')))
	    return FFEEXPR_percentVAL_;
	  return FFEEXPR_percentNONE_;

	default:
	no_match_3:		/* :::::::::::::::::::: */
	  return FFEEXPR_percentNONE_;
	}

    case 5:
      if (ffesrc_strcmp_2c (ffe_case_match (), ffelex_token_text (t), "DESCR",
			    "descr", "Descr") == 0)
	return FFEEXPR_percentDESCR_;
      return FFEEXPR_percentNONE_;

    default:
      return FFEEXPR_percentNONE_;
    }
}

/* ffeexpr_type_combine -- Binop combine types, check for mythical new COMPLEX

   See prototype.

   If combining the two basictype/kindtype pairs produces a COMPLEX with an
   unsupported kind type, complain and use the default kind type for
   COMPLEX.  */

void
ffeexpr_type_combine (ffeinfoBasictype *xnbt, ffeinfoKindtype *xnkt,
		      ffeinfoBasictype lbt, ffeinfoKindtype lkt,
		      ffeinfoBasictype rbt, ffeinfoKindtype rkt,
		      ffelexToken t)
{
  ffeinfoBasictype nbt;
  ffeinfoKindtype nkt;

  nbt = ffeinfo_basictype_combine (lbt, rbt);
  if ((nbt == FFEINFO_basictypeCOMPLEX)
      && ((lbt == nbt) || (lbt == FFEINFO_basictypeREAL))
      && ((rbt == nbt) || (rbt == FFEINFO_basictypeREAL)))
    {
      nkt = ffeinfo_kindtype_max (nbt, lkt, rkt);
      if (ffe_is_pedantic_not_90 () && (nkt == FFEINFO_kindtypeREALDOUBLE))
	nkt = FFEINFO_kindtypeNONE;	/* Force error. */
      switch (nkt)
	{
#if FFETARGET_okCOMPLEX1
	case FFEINFO_kindtypeREAL1:
#endif
#if FFETARGET_okCOMPLEX2
	case FFEINFO_kindtypeREAL2:
#endif
#if FFETARGET_okCOMPLEX3
	case FFEINFO_kindtypeREAL3:
#endif
#if FFETARGET_okCOMPLEX4
	case FFEINFO_kindtypeREAL4:
#endif
	  break;		/* Fine and dandy. */

	default:
	  if (t != NULL)
	    {
	      ffebad_start ((nkt == FFEINFO_kindtypeREALDOUBLE)
			    ? FFEBAD_BAD_DBLCMPLX : FFEBAD_BAD_COMPLEX);
	      ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
	      ffebad_finish ();
	    }
	  nbt = FFEINFO_basictypeNONE;
	  nkt = FFEINFO_kindtypeNONE;
	  break;

	case FFEINFO_kindtypeANY:
	  nkt = FFEINFO_kindtypeREALDEFAULT;
	  break;
	}
    }
  else
    {				/* The normal stuff. */
      if (nbt == lbt)
	{
	  if (nbt == rbt)
	    nkt = ffeinfo_kindtype_max (nbt, lkt, rkt);
	  else
	    nkt = lkt;
	}
      else if (nbt == rbt)
	nkt = rkt;
      else
	{			/* Let the caller do the complaining. */
	  nbt = FFEINFO_basictypeNONE;
	  nkt = FFEINFO_kindtypeNONE;
	}
    }

  /* Always a good idea to avoid aliasing problems.  */

  *xnbt = nbt;
  *xnkt = nkt;
}

/* ffeexpr_token_first_lhs_ -- First state for lhs expression

   Return a pointer to this function to the lexer (ffelex), which will
   invoke it for the next token.

   Record line and column of first token in expression, then invoke the
   initial-state lhs handler.  */

static ffelexHandler
ffeexpr_token_first_lhs_ (ffelexToken t)
{
  ffeexpr_stack_->first_token = ffelex_token_use (t);

  /* When changing the list of valid initial lhs tokens, check whether to
     update a corresponding list in ffeexpr_cb_close_paren_ambig_1_ for the
     READ (expr) <token> case -- it assumes it knows which tokens <token> can
     be to indicate an lhs (or implied DO), which right now is the set
     {NAME,OPEN_PAREN}.

     This comment also appears in ffeexpr_token_lhs_. */

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeOPEN_PAREN:
      switch (ffeexpr_stack_->context)
	{
	case FFEEXPR_contextDATA:
	  ffe_init_4 ();
	  ffeexpr_level_ = 1;	/* Level of DATA implied-DO construct. */
	  ffebld_init_list (&ffeexpr_stack_->expr, &ffeexpr_stack_->bottom);
	  return (ffelexHandler) ffeexpr_lhs (ffeexpr_stack_->pool,
			FFEEXPR_contextDATAIMPDOITEM_, ffeexpr_cb_comma_i_);

	case FFEEXPR_contextDATAIMPDOITEM_:
	  ++ffeexpr_level_;	/* Level of DATA implied-DO construct. */
	  ffebld_init_list (&ffeexpr_stack_->expr, &ffeexpr_stack_->bottom);
	  return (ffelexHandler) ffeexpr_lhs (ffeexpr_stack_->pool,
			FFEEXPR_contextDATAIMPDOITEM_, ffeexpr_cb_comma_i_);

	case FFEEXPR_contextIOLIST:
	case FFEEXPR_contextIMPDOITEM_:
	  ffebld_init_list (&ffeexpr_stack_->expr, &ffeexpr_stack_->bottom);
	  return (ffelexHandler) ffeexpr_lhs (ffeexpr_stack_->pool,
			    FFEEXPR_contextIMPDOITEM_, ffeexpr_cb_comma_i_);

	case FFEEXPR_contextIOLISTDF:
	case FFEEXPR_contextIMPDOITEMDF_:
	  ffebld_init_list (&ffeexpr_stack_->expr, &ffeexpr_stack_->bottom);
	  return (ffelexHandler) ffeexpr_lhs (ffeexpr_stack_->pool,
			  FFEEXPR_contextIMPDOITEMDF_, ffeexpr_cb_comma_i_);

	case FFEEXPR_contextFILEEXTFUNC:
	  assert (ffeexpr_stack_->exprstack == NULL);
	  return (ffelexHandler) ffeexpr_token_first_lhs_1_;

	default:
	  break;
	}
      break;

    case FFELEX_typeNAME:
      switch (ffeexpr_stack_->context)
	{
	case FFEEXPR_contextFILENAMELIST:
	  assert (ffeexpr_stack_->exprstack == NULL);
	  return (ffelexHandler) ffeexpr_token_namelist_;

	case FFEEXPR_contextFILEEXTFUNC:
	  assert (ffeexpr_stack_->exprstack == NULL);
	  return (ffelexHandler) ffeexpr_token_first_lhs_1_;

	default:
	  break;
	}
      break;

    default:
      switch (ffeexpr_stack_->context)
	{
	case FFEEXPR_contextFILEEXTFUNC:
	  assert (ffeexpr_stack_->exprstack == NULL);
	  return (ffelexHandler) ffeexpr_token_first_lhs_1_;

	default:
	  break;
	}
      break;
    }

  return (ffelexHandler) ffeexpr_token_lhs_ (t);
}

/* ffeexpr_token_first_lhs_1_ -- NAME

   return ffeexpr_token_first_lhs_1_;  // to lexer

   Handle NAME as an external function (USEROPEN= VXT extension to OPEN
   statement).	*/

static ffelexHandler
ffeexpr_token_first_lhs_1_ (ffelexToken t)
{
  ffeexprCallback callback;
  ffeexprStack_ s;
  ffelexHandler next;
  ffelexToken ft;
  ffesymbol sy = NULL;
  ffebld expr;

  ffebld_pool_pop ();
  callback = ffeexpr_stack_->callback;
  ft = ffeexpr_stack_->first_token;
  s = ffeexpr_stack_->previous;

  if ((ffelex_token_type (ft) != FFELEX_typeNAME)
      || (ffesymbol_attrs (sy = ffeexpr_declare_unadorned_ (ft, FALSE))
	  & FFESYMBOL_attrANY))
    {
      if ((ffelex_token_type (ft) != FFELEX_typeNAME)
	  || !(ffesymbol_attrs (sy) & FFESYMBOL_attrsANY))
	{
	  ffebad_start (FFEBAD_EXPR_WRONG);
	  ffebad_here (0, ffelex_token_where_line (ft),
		       ffelex_token_where_column (ft));
	  ffebad_finish ();
	}
      expr = ffebld_new_any ();
      ffebld_set_info (expr, ffeinfo_new_any ());
    }
  else
    {
      expr = ffebld_new_symter (sy, FFEINTRIN_genNONE, FFEINTRIN_specNONE,
				FFEINTRIN_impNONE);
      ffebld_set_info (expr, ffesymbol_info (sy));
    }

  malloc_kill_ks (ffe_pool_program_unit (), ffeexpr_stack_,
		  sizeof (*ffeexpr_stack_));
  ffeexpr_stack_ = s;

  next = (ffelexHandler) (*callback) (ft, expr, t);
  ffelex_token_kill (ft);
  return (ffelexHandler) next;
}

/* ffeexpr_token_first_rhs_ -- First state for rhs expression

   Record line and column of first token in expression, then invoke the
   initial-state rhs handler.

   19-Feb-91  JCB  1.1
      Allow ASTERISK in PARENFILEUNIT_ case, but only on second level only
      (i.e. only as in READ(*), not READ((*))).	 */

static ffelexHandler
ffeexpr_token_first_rhs_ (ffelexToken t)
{
  ffesymbol s;

  ffeexpr_stack_->first_token = ffelex_token_use (t);

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeASTERISK:
      switch (ffeexpr_stack_->context)
	{
	case FFEEXPR_contextFILEFORMATNML:
	  ffeexpr_stack_->context = FFEEXPR_contextFILEFORMAT;
	  /* Fall through.  */
	case FFEEXPR_contextFILEUNIT:
	case FFEEXPR_contextDIMLIST:
	case FFEEXPR_contextFILEFORMAT:
	case FFEEXPR_contextCHARACTERSIZE:
	  if (ffeexpr_stack_->previous != NULL)
	    break;		/* Valid only on first level. */
	  assert (ffeexpr_stack_->exprstack == NULL);
	  return (ffelexHandler) ffeexpr_token_first_rhs_1_;

	case FFEEXPR_contextPARENFILEUNIT_:
	  if (ffeexpr_stack_->previous->previous != NULL)
	    break;		/* Valid only on second level. */
	  assert (ffeexpr_stack_->exprstack == NULL);
	  return (ffelexHandler) ffeexpr_token_first_rhs_1_;

	case FFEEXPR_contextACTUALARG_:
	  if (ffeexpr_stack_->previous->context
	      != FFEEXPR_contextSUBROUTINEREF)
	    {
	      ffeexpr_stack_->context = FFEEXPR_contextACTUALARGEXPR_;
	      break;
	    }
	  assert (ffeexpr_stack_->exprstack == NULL);
	  return (ffelexHandler) ffeexpr_token_first_rhs_3_;

	case FFEEXPR_contextINDEXORACTUALARG_:
	  ffeexpr_stack_->context = FFEEXPR_contextINDEXORACTUALARGEXPR_;
	  break;

	case FFEEXPR_contextSFUNCDEFACTUALARG_:
	  ffeexpr_stack_->context = FFEEXPR_contextSFUNCDEFACTUALARGEXPR_;
	  break;

	case FFEEXPR_contextSFUNCDEFINDEXORACTUALARG_:
	  ffeexpr_stack_->context = FFEEXPR_contextSFUNCDEFINDEXORACTUALARGEXPR_;
	  break;

	default:
	  break;
	}
      break;

    case FFELEX_typeOPEN_PAREN:
      switch (ffeexpr_stack_->context)
	{
	case FFEEXPR_contextFILENUMAMBIG:
	  return (ffelexHandler) ffeexpr_rhs (ffeexpr_stack_->pool,
					      FFEEXPR_contextPARENFILENUM_,
					      ffeexpr_cb_close_paren_ambig_);

	case FFEEXPR_contextFILEUNITAMBIG:
	  return (ffelexHandler) ffeexpr_rhs (ffeexpr_stack_->pool,
					      FFEEXPR_contextPARENFILEUNIT_,
					      ffeexpr_cb_close_paren_ambig_);

	case FFEEXPR_contextIOLIST:
	case FFEEXPR_contextIMPDOITEM_:
	  return (ffelexHandler) ffeexpr_rhs (ffeexpr_stack_->pool,
					      FFEEXPR_contextIMPDOITEM_,
					      ffeexpr_cb_close_paren_ci_);

	case FFEEXPR_contextIOLISTDF:
	case FFEEXPR_contextIMPDOITEMDF_:
	  return (ffelexHandler) ffeexpr_rhs (ffeexpr_stack_->pool,
					      FFEEXPR_contextIMPDOITEMDF_,
					      ffeexpr_cb_close_paren_ci_);

	case FFEEXPR_contextFILEFORMATNML:
	  ffeexpr_stack_->context = FFEEXPR_contextFILEFORMAT;
	  break;

	case FFEEXPR_contextACTUALARG_:
	  ffeexpr_stack_->context = FFEEXPR_contextACTUALARGEXPR_;
	  break;

	case FFEEXPR_contextINDEXORACTUALARG_:
	  ffeexpr_stack_->context = FFEEXPR_contextINDEXORACTUALARGEXPR_;
	  break;

	case FFEEXPR_contextSFUNCDEFACTUALARG_:
	  ffeexpr_stack_->context = FFEEXPR_contextSFUNCDEFACTUALARGEXPR_;
	  break;

	case FFEEXPR_contextSFUNCDEFINDEXORACTUALARG_:
	  ffeexpr_stack_->context = FFEEXPR_contextSFUNCDEFINDEXORACTUALARGEXPR_;
	  break;

	default:
	  break;
	}
      break;

    case FFELEX_typeNUMBER:
      switch (ffeexpr_stack_->context)
	{
	case FFEEXPR_contextFILEFORMATNML:
	  ffeexpr_stack_->context = FFEEXPR_contextFILEFORMAT;
	  /* Fall through.  */
	case FFEEXPR_contextFILEFORMAT:
	  if (ffeexpr_stack_->previous != NULL)
	    break;		/* Valid only on first level. */
	  assert (ffeexpr_stack_->exprstack == NULL);
	  return (ffelexHandler) ffeexpr_token_first_rhs_2_;

	case FFEEXPR_contextACTUALARG_:
	  ffeexpr_stack_->context = FFEEXPR_contextACTUALARGEXPR_;
	  break;

	case FFEEXPR_contextINDEXORACTUALARG_:
	  ffeexpr_stack_->context = FFEEXPR_contextINDEXORACTUALARGEXPR_;
	  break;

	case FFEEXPR_contextSFUNCDEFACTUALARG_:
	  ffeexpr_stack_->context = FFEEXPR_contextSFUNCDEFACTUALARGEXPR_;
	  break;

	case FFEEXPR_contextSFUNCDEFINDEXORACTUALARG_:
	  ffeexpr_stack_->context = FFEEXPR_contextSFUNCDEFINDEXORACTUALARGEXPR_;
	  break;

	default:
	  break;
	}
      break;

    case FFELEX_typeNAME:
      switch (ffeexpr_stack_->context)
	{
	case FFEEXPR_contextFILEFORMATNML:
	  assert (ffeexpr_stack_->exprstack == NULL);
	  s = ffesymbol_lookup_local (t);
	  if ((s != NULL) && (ffesymbol_kind (s) == FFEINFO_kindNAMELIST))
	    return (ffelexHandler) ffeexpr_token_namelist_;
	  ffeexpr_stack_->context = FFEEXPR_contextFILEFORMAT;
	  break;

	default:
	  break;
	}
      break;

    case FFELEX_typePERCENT:
      switch (ffeexpr_stack_->context)
	{
	case FFEEXPR_contextACTUALARG_:
	case FFEEXPR_contextINDEXORACTUALARG_:
	case FFEEXPR_contextSFUNCDEFACTUALARG_:
	case FFEEXPR_contextSFUNCDEFINDEXORACTUALARG_:
	  return (ffelexHandler) ffeexpr_token_first_rhs_5_;

	case FFEEXPR_contextFILEFORMATNML:
	  ffeexpr_stack_->context = FFEEXPR_contextFILEFORMAT;
	  break;

	default:
	  break;
	}

    default:
      switch (ffeexpr_stack_->context)
	{
	case FFEEXPR_contextACTUALARG_:
	  ffeexpr_stack_->context = FFEEXPR_contextACTUALARGEXPR_;
	  break;

	case FFEEXPR_contextINDEXORACTUALARG_:
	  ffeexpr_stack_->context = FFEEXPR_contextINDEXORACTUALARGEXPR_;
	  break;

	case FFEEXPR_contextSFUNCDEFACTUALARG_:
	  ffeexpr_stack_->context = FFEEXPR_contextSFUNCDEFACTUALARGEXPR_;
	  break;

	case FFEEXPR_contextSFUNCDEFINDEXORACTUALARG_:
	  ffeexpr_stack_->context = FFEEXPR_contextSFUNCDEFINDEXORACTUALARGEXPR_;
	  break;

	case FFEEXPR_contextFILEFORMATNML:
	  ffeexpr_stack_->context = FFEEXPR_contextFILEFORMAT;
	  break;

	default:
	  break;
	}
      break;
    }

  return (ffelexHandler) ffeexpr_token_rhs_ (t);
}

/* ffeexpr_token_first_rhs_1_ -- ASTERISK

   return ffeexpr_token_first_rhs_1_;  // to lexer

   Return STAR as expression.  */

static ffelexHandler
ffeexpr_token_first_rhs_1_ (ffelexToken t)
{
  ffebld expr;
  ffeexprCallback callback;
  ffeexprStack_ s;
  ffelexHandler next;
  ffelexToken ft;

  expr = ffebld_new_star ();
  ffebld_pool_pop ();
  callback = ffeexpr_stack_->callback;
  ft = ffeexpr_stack_->first_token;
  s = ffeexpr_stack_->previous;
  malloc_kill_ks (ffe_pool_program_unit (), ffeexpr_stack_, sizeof (*ffeexpr_stack_));
  ffeexpr_stack_ = s;
  next = (ffelexHandler) (*callback) (ft, expr, t);
  ffelex_token_kill (ft);
  return (ffelexHandler) next;
}

/* ffeexpr_token_first_rhs_2_ -- NUMBER

   return ffeexpr_token_first_rhs_2_;  // to lexer

   Return NULL as expression; NUMBER as first (and only) token, unless the
   current token is not a terminating token, in which case run normal
   expression handling.	 */

static ffelexHandler
ffeexpr_token_first_rhs_2_ (ffelexToken t)
{
  ffeexprCallback callback;
  ffeexprStack_ s;
  ffelexHandler next;
  ffelexToken ft;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCLOSE_PAREN:
    case FFELEX_typeCOMMA:
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
      break;

    default:
      next = (ffelexHandler) ffeexpr_token_rhs_ (ffeexpr_stack_->first_token);
      return (ffelexHandler) (*next) (t);
    }

  ffebld_pool_pop ();
  callback = ffeexpr_stack_->callback;
  ft = ffeexpr_stack_->first_token;
  s = ffeexpr_stack_->previous;
  malloc_kill_ks (ffe_pool_program_unit (), ffeexpr_stack_,
		  sizeof (*ffeexpr_stack_));
  ffeexpr_stack_ = s;
  next = (ffelexHandler) (*callback) (ft, NULL, t);
  ffelex_token_kill (ft);
  return (ffelexHandler) next;
}

/* ffeexpr_token_first_rhs_3_ -- ASTERISK

   return ffeexpr_token_first_rhs_3_;  // to lexer

   Expect NUMBER, make LABTOK (with copy of token if not inhibited after
   confirming, else NULL).  */

static ffelexHandler
ffeexpr_token_first_rhs_3_ (ffelexToken t)
{
  ffelexHandler next;

  if (ffelex_token_type (t) != FFELEX_typeNUMBER)
    {				/* An error, but let normal processing handle
				   it. */
      next = (ffelexHandler) ffeexpr_token_rhs_ (ffeexpr_stack_->first_token);
      return (ffelexHandler) (*next) (t);
    }

  /* Special case: when we see "*10" as an argument to a subroutine
     reference, we confirm the current statement and, if not inhibited at
     this point, put a copy of the token into a LABTOK node.  We do this
     instead of just resolving the label directly via ffelab and putting it
     into a LABTER simply to improve error reporting and consistency in
     ffestc.  We put NULL in the LABTOK if we're still inhibited, so ffestb
     doesn't have to worry about killing off any tokens when retracting. */

  ffest_confirmed ();
  if (ffest_is_inhibited ())
    ffeexpr_stack_->expr = ffebld_new_labtok (NULL);
  else
    ffeexpr_stack_->expr = ffebld_new_labtok (ffelex_token_use (t));
  ffebld_set_info (ffeexpr_stack_->expr,
		   ffeinfo_new (FFEINFO_basictypeNONE,
				FFEINFO_kindtypeNONE,
				0,
				FFEINFO_kindNONE,
				FFEINFO_whereNONE,
				FFETARGET_charactersizeNONE));

  return (ffelexHandler) ffeexpr_token_first_rhs_4_;
}

/* ffeexpr_token_first_rhs_4_ -- ASTERISK NUMBER

   return ffeexpr_token_first_rhs_4_;  // to lexer

   Collect/flush appropriate stuff, send token to callback function.  */

static ffelexHandler
ffeexpr_token_first_rhs_4_ (ffelexToken t)
{
  ffebld expr;
  ffeexprCallback callback;
  ffeexprStack_ s;
  ffelexHandler next;
  ffelexToken ft;

  expr = ffeexpr_stack_->expr;
  ffebld_pool_pop ();
  callback = ffeexpr_stack_->callback;
  ft = ffeexpr_stack_->first_token;
  s = ffeexpr_stack_->previous;
  malloc_kill_ks (ffe_pool_program_unit (), ffeexpr_stack_, sizeof (*ffeexpr_stack_));
  ffeexpr_stack_ = s;
  next = (ffelexHandler) (*callback) (ft, expr, t);
  ffelex_token_kill (ft);
  return (ffelexHandler) next;
}

/* ffeexpr_token_first_rhs_5_ -- PERCENT

   Should be NAME, or pass through original mechanism.  If NAME is LOC,
   pass through original mechanism, otherwise must be VAL, REF, or DESCR,
   in which case handle the argument (in parentheses), etc.  */

static ffelexHandler
ffeexpr_token_first_rhs_5_ (ffelexToken t)
{
  ffelexHandler next;

  if (ffelex_token_type (t) == FFELEX_typeNAME)
    {
      ffeexprPercent_ p = ffeexpr_percent_ (t);

      switch (p)
	{
	case FFEEXPR_percentNONE_:
	case FFEEXPR_percentLOC_:
	  break;		/* Treat %LOC as any other expression. */

	case FFEEXPR_percentVAL_:
	case FFEEXPR_percentREF_:
	case FFEEXPR_percentDESCR_:
	  ffeexpr_stack_->percent = p;
	  ffeexpr_stack_->tokens[0] = ffelex_token_use (t);
	  return (ffelexHandler) ffeexpr_token_first_rhs_6_;

	default:
	  assert ("bad percent?!?" == NULL);
	  break;
	}
    }

  switch (ffeexpr_stack_->context)
    {
    case FFEEXPR_contextACTUALARG_:
      ffeexpr_stack_->context = FFEEXPR_contextACTUALARGEXPR_;
      break;

    case FFEEXPR_contextINDEXORACTUALARG_:
      ffeexpr_stack_->context = FFEEXPR_contextINDEXORACTUALARGEXPR_;
      break;

    case FFEEXPR_contextSFUNCDEFACTUALARG_:
      ffeexpr_stack_->context = FFEEXPR_contextSFUNCDEFACTUALARGEXPR_;
      break;

    case FFEEXPR_contextSFUNCDEFINDEXORACTUALARG_:
      ffeexpr_stack_->context = FFEEXPR_contextSFUNCDEFINDEXORACTUALARGEXPR_;
      break;

    default:
      assert ("bad context?!?!" == NULL);
      break;
    }

  next = (ffelexHandler) ffeexpr_token_rhs_ (ffeexpr_stack_->first_token);
  return (ffelexHandler) (*next) (t);
}

/* ffeexpr_token_first_rhs_6_ -- PERCENT NAME(VAL,REF,DESCR)

   Should be OPEN_PAREN, or pass through original mechanism.  */

static ffelexHandler
ffeexpr_token_first_rhs_6_ (ffelexToken t)
{
  ffelexHandler next;
  ffelexToken ft;

  if (ffelex_token_type (t) == FFELEX_typeOPEN_PAREN)
    {
      ffeexpr_stack_->tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffeexpr_rhs (ffeexpr_stack_->pool,
					  ffeexpr_stack_->context,
					  ffeexpr_cb_end_notloc_);
    }

  switch (ffeexpr_stack_->context)
    {
    case FFEEXPR_contextACTUALARG_:
      ffeexpr_stack_->context = FFEEXPR_contextACTUALARGEXPR_;
      break;

    case FFEEXPR_contextINDEXORACTUALARG_:
      ffeexpr_stack_->context = FFEEXPR_contextINDEXORACTUALARGEXPR_;
      break;

    case FFEEXPR_contextSFUNCDEFACTUALARG_:
      ffeexpr_stack_->context = FFEEXPR_contextSFUNCDEFACTUALARGEXPR_;
      break;

    case FFEEXPR_contextSFUNCDEFINDEXORACTUALARG_:
      ffeexpr_stack_->context = FFEEXPR_contextSFUNCDEFINDEXORACTUALARGEXPR_;
      break;

    default:
      assert ("bad context?!?!" == NULL);
      break;
    }

  ft = ffeexpr_stack_->tokens[0];
  next = (ffelexHandler) ffeexpr_token_rhs_ (ffeexpr_stack_->first_token);
  next = (ffelexHandler) (*next) (ft);
  ffelex_token_kill (ft);
  return (ffelexHandler) (*next) (t);
}

/* ffeexpr_token_namelist_ -- NAME

   return ffeexpr_token_namelist_;  // to lexer

   Make sure NAME was a valid namelist object, wrap it in a SYMTER and
   return.  */

static ffelexHandler
ffeexpr_token_namelist_ (ffelexToken t)
{
  ffeexprCallback callback;
  ffeexprStack_ s;
  ffelexHandler next;
  ffelexToken ft;
  ffesymbol sy;
  ffebld expr;

  ffebld_pool_pop ();
  callback = ffeexpr_stack_->callback;
  ft = ffeexpr_stack_->first_token;
  s = ffeexpr_stack_->previous;
  malloc_kill_ks (ffe_pool_program_unit (), ffeexpr_stack_, sizeof (*ffeexpr_stack_));
  ffeexpr_stack_ = s;

  sy = ffesymbol_lookup_local (ft);
  if ((sy == NULL) || (ffesymbol_kind (sy) != FFEINFO_kindNAMELIST))
    {
      ffebad_start (FFEBAD_EXPR_WRONG);
      ffebad_here (0, ffelex_token_where_line (ft),
		   ffelex_token_where_column (ft));
      ffebad_finish ();
      expr = ffebld_new_any ();
      ffebld_set_info (expr, ffeinfo_new_any ());
    }
  else
    {
      expr = ffebld_new_symter (sy, FFEINTRIN_genNONE, FFEINTRIN_specNONE,
				FFEINTRIN_impNONE);
      ffebld_set_info (expr, ffesymbol_info (sy));
    }
  next = (ffelexHandler) (*callback) (ft, expr, t);
  ffelex_token_kill (ft);
  return (ffelexHandler) next;
}

/* ffeexpr_expr_kill_ -- Kill an existing internal expression object

   ffeexprExpr_ e;
   ffeexpr_expr_kill_(e);

   Kills the ffewhere info, if necessary, then kills the object.  */

static void
ffeexpr_expr_kill_ (ffeexprExpr_ e)
{
  if (e->token != NULL)
    ffelex_token_kill (e->token);
  malloc_kill_ks (ffe_pool_program_unit (), e, sizeof (*e));
}

/* ffeexpr_expr_new_ -- Make a new internal expression object

   ffeexprExpr_ e;
   e = ffeexpr_expr_new_();

   Allocates and initializes a new expression object, returns it.  */

static ffeexprExpr_
ffeexpr_expr_new_ ()
{
  ffeexprExpr_ e;

  e = (ffeexprExpr_) malloc_new_ks (ffe_pool_program_unit (), "FFEEXPR expr",
				    sizeof (*e));
  e->previous = NULL;
  e->type = FFEEXPR_exprtypeUNKNOWN_;
  e->token = NULL;
  return e;
}

/* Verify that call to global is valid, and register whatever
   new information about a global might be discoverable by looking
   at the call.  */

static void
ffeexpr_fulfill_call_ (ffebld *expr, ffelexToken t)
{
  int n_args;
  ffebld list;
  ffebld item;
  ffesymbol s;

  assert ((ffebld_op (*expr) == FFEBLD_opSUBRREF)
	  || (ffebld_op (*expr) == FFEBLD_opFUNCREF));

  if (ffebld_op (ffebld_left (*expr)) != FFEBLD_opSYMTER)
    return;

  if (ffesymbol_retractable ())
    return;

  s = ffebld_symter (ffebld_left (*expr));
  if (ffesymbol_global (s) == NULL)
    return;

  for (n_args = 0, list = ffebld_right (*expr);
       list != NULL;
       list = ffebld_trail (list), ++n_args)
    ;

  if (ffeglobal_proc_ref_nargs (s, n_args, t))
    {
      ffeglobalArgSummary as;
      ffeinfoBasictype bt;
      ffeinfoKindtype kt;
      bool array;
      bool fail = FALSE;

      for (n_args = 0, list = ffebld_right (*expr);
	   list != NULL;
	   list = ffebld_trail (list), ++n_args)
	{
	  item = ffebld_head (list);
	  if (item != NULL)
	    {
	      bt = ffeinfo_basictype (ffebld_info (item));
	      kt = ffeinfo_kindtype (ffebld_info (item));
	      array = (ffeinfo_rank (ffebld_info (item)) > 0);
	      switch (ffebld_op (item))
		{
		case FFEBLD_opLABTOK:
		case FFEBLD_opLABTER:
		  as = FFEGLOBAL_argsummaryALTRTN;
		  break;

#if 0
		  /* No, %LOC(foo) is just like any INTEGER(KIND=7)
		     expression, so don't treat it specially.  */
		case FFEBLD_opPERCENT_LOC:
		  as = FFEGLOBAL_argsummaryPTR;
		  break;
#endif

		case FFEBLD_opPERCENT_VAL:
		  as = FFEGLOBAL_argsummaryVAL;
		  break;

		case FFEBLD_opPERCENT_REF:
		  as = FFEGLOBAL_argsummaryREF;
		  break;

		case FFEBLD_opPERCENT_DESCR:
		  as = FFEGLOBAL_argsummaryDESCR;
		  break;

		case FFEBLD_opFUNCREF:
#if 0
		  /* No, LOC(foo) is just like any INTEGER(KIND=7)
		     expression, so don't treat it specially.  */
		  if ((ffebld_op (ffebld_left (item)) == FFEBLD_opSYMTER)
		      && (ffesymbol_specific (ffebld_symter (ffebld_left (item)))
			  == FFEINTRIN_specLOC))
		    {
		      as = FFEGLOBAL_argsummaryPTR;
		      break;
		    }
#endif
		  /* Fall through.  */
		default:
		  if (ffebld_op (item) == FFEBLD_opSYMTER)
		    {
		      as = FFEGLOBAL_argsummaryNONE;

		      switch (ffeinfo_kind (ffebld_info (item)))
			{
			case FFEINFO_kindFUNCTION:
			  as = FFEGLOBAL_argsummaryFUNC;
			  break;

			case FFEINFO_kindSUBROUTINE:
			  as = FFEGLOBAL_argsummarySUBR;
			  break;

			case FFEINFO_kindNONE:
			  as = FFEGLOBAL_argsummaryPROC;
			  break;

			default:
			  break;
			}

		      if (as != FFEGLOBAL_argsummaryNONE)
			break;
		    }

		  if (bt == FFEINFO_basictypeCHARACTER)
		    as = FFEGLOBAL_argsummaryDESCR;
		  else
		    as = FFEGLOBAL_argsummaryREF;
		  break;
		}
	    }
	  else
	    {
	      array = FALSE;
	      as = FFEGLOBAL_argsummaryNONE;
	      bt = FFEINFO_basictypeNONE;
	      kt = FFEINFO_kindtypeNONE;
	    }

	  if (! ffeglobal_proc_ref_arg (s, n_args, as, bt, kt, array, t))
	    fail = TRUE;
	}
      if (! fail)
	return;
    }

  *expr = ffebld_new_any ();
  ffebld_set_info (*expr, ffeinfo_new_any ());
}

/* Check whether rest of string is all decimal digits.  */

static bool
ffeexpr_isdigits_ (const char *p)
{
  for (; *p != '\0'; ++p)
    if (! ISDIGIT (*p))
      return FALSE;
  return TRUE;
}

/* ffeexpr_exprstack_push_ -- Push an arbitrary expression object onto the stack

   ffeexprExpr_ e;
   ffeexpr_exprstack_push_(e);

   Pushes the expression onto the stack without any analysis of the existing
   contents of the stack.  */

static void
ffeexpr_exprstack_push_ (ffeexprExpr_ e)
{
  e->previous = ffeexpr_stack_->exprstack;
  ffeexpr_stack_->exprstack = e;
}

/* ffeexpr_exprstack_push_operand_ -- Push an operand onto the stack, reduce?

   ffeexprExpr_ e;
   ffeexpr_exprstack_push_operand_(e);

   Pushes the expression already containing an operand (a constant, variable,
   or more complicated expression that has already been fully resolved) after
   analyzing the stack and checking for possible reduction (which will never
   happen here since the highest precedence operator is ** and it has right-
   to-left associativity).  */

static void
ffeexpr_exprstack_push_operand_ (ffeexprExpr_ e)
{
  ffeexpr_exprstack_push_ (e);
#ifdef WEIRD_NONFORTRAN_RULES
  if ((ffeexpr_stack_->exprstack != NULL)
      && (ffeexpr_stack_->exprstack->expr->type == FFEEXPR_exprtypeBINARY_)
      && (ffeexpr_stack_->exprstack->expr->u.operator.prec
	  == FFEEXPR_operatorprecedenceHIGHEST_)
      && (ffeexpr_stack_->exprstack->expr->u.operator.as
	  == FFEEXPR_operatorassociativityL2R_))
    ffeexpr_reduce_ ();
#endif
}

/* ffeexpr_exprstack_push_unary_ -- Push a unary operator onto the stack

   ffeexprExpr_ e;
   ffeexpr_exprstack_push_unary_(e);

   Pushes the expression already containing a unary operator.  Reduction can
   never happen since unary operators are themselves always R-L; that is, the
   top of the expression stack is not an operand, in that it is either empty,
   has a binary operator at the top, or a unary operator at the top.  In any
   of these cases, reduction is impossible.  */

static void
ffeexpr_exprstack_push_unary_ (ffeexprExpr_ e)
{
  if ((ffe_is_pedantic ()
       || ffe_is_warn_surprising ())
      && (ffeexpr_stack_->exprstack != NULL)
      && (ffeexpr_stack_->exprstack->type != FFEEXPR_exprtypeOPERAND_)
      && (ffeexpr_stack_->exprstack->u.operator.prec
	  <= FFEEXPR_operatorprecedenceLOWARITH_)
      && (e->u.operator.prec <= FFEEXPR_operatorprecedenceLOWARITH_))
    {
      /* xgettext:no-c-format */
      ffebad_start_msg ("Two arithmetic operators in a row at %0 and %1 -- use parentheses",
			ffe_is_pedantic ()
			? FFEBAD_severityPEDANTIC
			: FFEBAD_severityWARNING);
      ffebad_here (0,
		  ffelex_token_where_line (ffeexpr_stack_->exprstack->token),
	       ffelex_token_where_column (ffeexpr_stack_->exprstack->token));
      ffebad_here (1,
		   ffelex_token_where_line (e->token),
		   ffelex_token_where_column (e->token));
      ffebad_finish ();
    }

  ffeexpr_exprstack_push_ (e);
}

/* ffeexpr_exprstack_push_binary_ -- Push a binary operator onto the stack, reduce?

   ffeexprExpr_ e;
   ffeexpr_exprstack_push_binary_(e);

   Pushes the expression already containing a binary operator after checking
   whether reduction is possible.  If the stack is not empty, the top of the
   stack must be an operand or syntactic analysis has failed somehow.  If
   the operand is preceded by a unary operator of higher (or equal and L-R
   associativity) precedence than the new binary operator, then reduce that
   preceding operator and its operand(s) before pushing the new binary
   operator.  */

static void
ffeexpr_exprstack_push_binary_ (ffeexprExpr_ e)
{
  ffeexprExpr_ ce;

  if (ffe_is_warn_surprising ()
      /* These next two are always true (see assertions below).  */
      && (ffeexpr_stack_->exprstack != NULL)
      && (ffeexpr_stack_->exprstack->type == FFEEXPR_exprtypeOPERAND_)
      /* If the previous operator is a unary minus, and the binary op
	 is of higher precedence, might not do what user expects,
	 e.g. "-2**2" is "-(2**2)", i.e. "-4", not "(-2)**2", which would
	 yield "4".  */
      && (ffeexpr_stack_->exprstack->previous != NULL)
      && (ffeexpr_stack_->exprstack->previous->type == FFEEXPR_exprtypeUNARY_)
      && (ffeexpr_stack_->exprstack->previous->u.operator.op
	  == FFEEXPR_operatorSUBTRACT_)
      && (e->u.operator.prec
	  < ffeexpr_stack_->exprstack->previous->u.operator.prec))
    {
      /* xgettext:no-c-format */
      ffebad_start_msg ("Operator at %0 has lower precedence than that at %1 -- use parentheses", FFEBAD_severityWARNING);
      ffebad_here (0,
	 ffelex_token_where_line (ffeexpr_stack_->exprstack->previous->token),
      ffelex_token_where_column (ffeexpr_stack_->exprstack->previous->token));
      ffebad_here (1,
		   ffelex_token_where_line (e->token),
		   ffelex_token_where_column (e->token));
      ffebad_finish ();
    }

again:
  assert (ffeexpr_stack_->exprstack != NULL);
  assert (ffeexpr_stack_->exprstack->type == FFEEXPR_exprtypeOPERAND_);
  if ((ce = ffeexpr_stack_->exprstack->previous) != NULL)
    {
      assert (ce->type != FFEEXPR_exprtypeOPERAND_);
      if ((ce->u.operator.prec < e->u.operator.prec)
	  || ((ce->u.operator.prec == e->u.operator.prec)
	      && (e->u.operator.as == FFEEXPR_operatorassociativityL2R_)))
	{
	  ffeexpr_reduce_ ();
	  goto again;	/* :::::::::::::::::::: */
	}
    }

  ffeexpr_exprstack_push_ (e);
}

/* ffeexpr_reduce_ -- Reduce highest operator w/operands on stack

   ffeexpr_reduce_();

   Converts operand binop operand or unop operand at top of stack to a
   single operand having the appropriate ffebld expression, and makes
   sure that the expression is proper (like not trying to add two character
   variables, not trying to concatenate two numbers).  Also does the
   requisite type-assignment.  */

static void
ffeexpr_reduce_ ()
{
  ffeexprExpr_ operand;		/* This is B in -B or A+B. */
  ffeexprExpr_ left_operand;	/* When operator is binary, this is A in A+B. */
  ffeexprExpr_ operator;	/* This is + in A+B. */
  ffebld reduced;		/* This is +(A,B) in A+B or u-(B) in -B. */
  ffebldConstant constnode;	/* For checking magical numbers (where mag ==
				   -mag). */
  ffebld expr;
  ffebld left_expr;
  bool submag = FALSE;

  operand = ffeexpr_stack_->exprstack;
  assert (operand != NULL);
  assert (operand->type == FFEEXPR_exprtypeOPERAND_);
  operator = operand->previous;
  assert (operator != NULL);
  assert (operator->type != FFEEXPR_exprtypeOPERAND_);
  if (operator->type == FFEEXPR_exprtypeUNARY_)
    {
      expr = operand->u.operand;
      switch (operator->u.operator.op)
	{
	case FFEEXPR_operatorADD_:
	  reduced = ffebld_new_uplus (expr);
	  if (ffe_is_ugly_logint ())
	    reduced = ffeexpr_reduced_ugly1_ (reduced, operator, operand);
	  reduced = ffeexpr_reduced_math1_ (reduced, operator, operand);
	  reduced = ffeexpr_collapse_uplus (reduced, operator->token);
	  break;

	case FFEEXPR_operatorSUBTRACT_:
	  submag = TRUE;	/* Ok to negate a magic number. */
	  reduced = ffebld_new_uminus (expr);
	  if (ffe_is_ugly_logint ())
	    reduced = ffeexpr_reduced_ugly1_ (reduced, operator, operand);
	  reduced = ffeexpr_reduced_math1_ (reduced, operator, operand);
	  reduced = ffeexpr_collapse_uminus (reduced, operator->token);
	  break;

	case FFEEXPR_operatorNOT_:
	  reduced = ffebld_new_not (expr);
	  if (ffe_is_ugly_logint ())
	    reduced = ffeexpr_reduced_ugly1log_ (reduced, operator, operand);
	  reduced = ffeexpr_reduced_bool1_ (reduced, operator, operand);
	  reduced = ffeexpr_collapse_not (reduced, operator->token);
	  break;

	default:
	  assert ("unexpected unary op" != NULL);
	  reduced = NULL;
	  break;
	}
      if (!submag
	  && (ffebld_op (expr) == FFEBLD_opCONTER)
	  && (ffebld_conter_orig (expr) == NULL)
	  && ffebld_constant_is_magical (constnode = ffebld_conter (expr)))
	{
	  ffetarget_integer_bad_magical (operand->token);
	}
      ffeexpr_stack_->exprstack = operator->previous;	/* Pops unary-op operand
							   off stack. */
      ffeexpr_expr_kill_ (operand);
      operator->type = FFEEXPR_exprtypeOPERAND_;	/* Convert operator, but
							   save */
      operator->u.operand = reduced;	/* the line/column ffewhere info. */
      ffeexpr_exprstack_push_operand_ (operator);	/* Push it back on
							   stack. */
    }
  else
    {
      assert (operator->type == FFEEXPR_exprtypeBINARY_);
      left_operand = operator->previous;
      assert (left_operand != NULL);
      assert (left_operand->type == FFEEXPR_exprtypeOPERAND_);
      expr = operand->u.operand;
      left_expr = left_operand->u.operand;
      switch (operator->u.operator.op)
	{
	case FFEEXPR_operatorADD_:
	  reduced = ffebld_new_add (left_expr, expr);
	  if (ffe_is_ugly_logint ())
	    reduced = ffeexpr_reduced_ugly2_ (reduced, left_operand, operator,
					      operand);
	  reduced = ffeexpr_reduced_math2_ (reduced, left_operand, operator,
					    operand);
	  reduced = ffeexpr_collapse_add (reduced, operator->token);
	  break;

	case FFEEXPR_operatorSUBTRACT_:
	  submag = TRUE;	/* Just to pick the right error if magic
				   number. */
	  reduced = ffebld_new_subtract (left_expr, expr);
	  if (ffe_is_ugly_logint ())
	    reduced = ffeexpr_reduced_ugly2_ (reduced, left_operand, operator,
					      operand);
	  reduced = ffeexpr_reduced_math2_ (reduced, left_operand, operator,
					    operand);
	  reduced = ffeexpr_collapse_subtract (reduced, operator->token);
	  break;

	case FFEEXPR_operatorMULTIPLY_:
	  reduced = ffebld_new_multiply (left_expr, expr);
	  if (ffe_is_ugly_logint ())
	    reduced = ffeexpr_reduced_ugly2_ (reduced, left_operand, operator,
					      operand);
	  reduced = ffeexpr_reduced_math2_ (reduced, left_operand, operator,
					    operand);
	  reduced = ffeexpr_collapse_multiply (reduced, operator->token);
	  break;

	case FFEEXPR_operatorDIVIDE_:
	  reduced = ffebld_new_divide (left_expr, expr);
	  if (ffe_is_ugly_logint ())
	    reduced = ffeexpr_reduced_ugly2_ (reduced, left_operand, operator,
					      operand);
	  reduced = ffeexpr_reduced_math2_ (reduced, left_operand, operator,
					    operand);
	  reduced = ffeexpr_collapse_divide (reduced, operator->token);
	  break;

	case FFEEXPR_operatorPOWER_:
	  reduced = ffebld_new_power (left_expr, expr);
	  if (ffe_is_ugly_logint ())
	    reduced = ffeexpr_reduced_ugly2_ (reduced, left_operand, operator,
					      operand);
	  reduced = ffeexpr_reduced_power_ (reduced, left_operand, operator,
					    operand);
	  reduced = ffeexpr_collapse_power (reduced, operator->token);
	  break;

	case FFEEXPR_operatorCONCATENATE_:
	  reduced = ffebld_new_concatenate (left_expr, expr);
	  reduced = ffeexpr_reduced_concatenate_ (reduced, left_operand, operator,
						  operand);
	  reduced = ffeexpr_collapse_concatenate (reduced, operator->token);
	  break;

	case FFEEXPR_operatorLT_:
	  reduced = ffebld_new_lt (left_expr, expr);
	  if (ffe_is_ugly_logint ())
	    reduced = ffeexpr_reduced_ugly2_ (reduced, left_operand, operator,
					      operand);
	  reduced = ffeexpr_reduced_relop2_ (reduced, left_operand, operator,
					     operand);
	  reduced = ffeexpr_collapse_lt (reduced, operator->token);
	  break;

	case FFEEXPR_operatorLE_:
	  reduced = ffebld_new_le (left_expr, expr);
	  if (ffe_is_ugly_logint ())
	    reduced = ffeexpr_reduced_ugly2_ (reduced, left_operand, operator,
					      operand);
	  reduced = ffeexpr_reduced_relop2_ (reduced, left_operand, operator,
					     operand);
	  reduced = ffeexpr_collapse_le (reduced, operator->token);
	  break;

	case FFEEXPR_operatorEQ_:
	  reduced = ffebld_new_eq (left_expr, expr);
	  if (ffe_is_ugly_logint ())
	    reduced = ffeexpr_reduced_ugly2_ (reduced, left_operand, operator,
					      operand);
	  reduced = ffeexpr_reduced_eqop2_ (reduced, left_operand, operator,
					    operand);
	  reduced = ffeexpr_collapse_eq (reduced, operator->token);
	  break;

	case FFEEXPR_operatorNE_:
	  reduced = ffebld_new_ne (left_expr, expr);
	  if (ffe_is_ugly_logint ())
	    reduced = ffeexpr_reduced_ugly2_ (reduced, left_operand, operator,
					      operand);
	  reduced = ffeexpr_reduced_eqop2_ (reduced, left_operand, operator,
					    operand);
	  reduced = ffeexpr_collapse_ne (reduced, operator->token);
	  break;

	case FFEEXPR_operatorGT_:
	  reduced = ffebld_new_gt (left_expr, expr);
	  if (ffe_is_ugly_logint ())
	    reduced = ffeexpr_reduced_ugly2_ (reduced, left_operand, operator,
					      operand);
	  reduced = ffeexpr_reduced_relop2_ (reduced, left_operand, operator,
					     operand);
	  reduced = ffeexpr_collapse_gt (reduced, operator->token);
	  break;

	case FFEEXPR_operatorGE_:
	  reduced = ffebld_new_ge (left_expr, expr);
	  if (ffe_is_ugly_logint ())
	    reduced = ffeexpr_reduced_ugly2_ (reduced, left_operand, operator,
					      operand);
	  reduced = ffeexpr_reduced_relop2_ (reduced, left_operand, operator,
					     operand);
	  reduced = ffeexpr_collapse_ge (reduced, operator->token);
	  break;

	case FFEEXPR_operatorAND_:
	  reduced = ffebld_new_and (left_expr, expr);
	  if (ffe_is_ugly_logint ())
	    reduced = ffeexpr_reduced_ugly2log_ (reduced, left_operand, operator,
						 operand);
	  reduced = ffeexpr_reduced_bool2_ (reduced, left_operand, operator,
					    operand);
	  reduced = ffeexpr_collapse_and (reduced, operator->token);
	  break;

	case FFEEXPR_operatorOR_:
	  reduced = ffebld_new_or (left_expr, expr);
	  if (ffe_is_ugly_logint ())
	    reduced = ffeexpr_reduced_ugly2log_ (reduced, left_operand, operator,
						 operand);
	  reduced = ffeexpr_reduced_bool2_ (reduced, left_operand, operator,
					    operand);
	  reduced = ffeexpr_collapse_or (reduced, operator->token);
	  break;

	case FFEEXPR_operatorXOR_:
	  reduced = ffebld_new_xor (left_expr, expr);
	  if (ffe_is_ugly_logint ())
	    reduced = ffeexpr_reduced_ugly2log_ (reduced, left_operand, operator,
						 operand);
	  reduced = ffeexpr_reduced_bool2_ (reduced, left_operand, operator,
					    operand);
	  reduced = ffeexpr_collapse_xor (reduced, operator->token);
	  break;

	case FFEEXPR_operatorEQV_:
	  reduced = ffebld_new_eqv (left_expr, expr);
	  if (ffe_is_ugly_logint ())
	    reduced = ffeexpr_reduced_ugly2log_ (reduced, left_operand, operator,
						 operand);
	  reduced = ffeexpr_reduced_bool2_ (reduced, left_operand, operator,
					    operand);
	  reduced = ffeexpr_collapse_eqv (reduced, operator->token);
	  break;

	case FFEEXPR_operatorNEQV_:
	  reduced = ffebld_new_neqv (left_expr, expr);
	  if (ffe_is_ugly_logint ())
	    reduced = ffeexpr_reduced_ugly2log_ (reduced, left_operand, operator,
						 operand);
	  reduced = ffeexpr_reduced_bool2_ (reduced, left_operand, operator,
					    operand);
	  reduced = ffeexpr_collapse_neqv (reduced, operator->token);
	  break;

	default:
	  assert ("bad bin op" == NULL);
	  reduced = expr;
	  break;
	}
      if ((ffebld_op (left_expr) == FFEBLD_opCONTER)
	  && (ffebld_conter_orig (expr) == NULL)
      && ffebld_constant_is_magical (constnode = ffebld_conter (left_expr)))
	{
	  if ((left_operand->previous != NULL)
	      && (left_operand->previous->type != FFEEXPR_exprtypeOPERAND_)
	      && (left_operand->previous->u.operator.op
		  == FFEEXPR_operatorSUBTRACT_))
	    {
	      if (left_operand->previous->type == FFEEXPR_exprtypeUNARY_)
		ffetarget_integer_bad_magical_precedence (left_operand->token,
							  left_operand->previous->token,
							  operator->token);
	      else
		ffetarget_integer_bad_magical_precedence_binary
		  (left_operand->token,
		   left_operand->previous->token,
		   operator->token);
	    }
	  else
	    ffetarget_integer_bad_magical (left_operand->token);
	}
      if ((ffebld_op (expr) == FFEBLD_opCONTER)
	  && (ffebld_conter_orig (expr) == NULL)
	  && ffebld_constant_is_magical (constnode = ffebld_conter (expr)))
	{
	  if (submag)
	    ffetarget_integer_bad_magical_binary (operand->token,
						  operator->token);
	  else
	    ffetarget_integer_bad_magical (operand->token);
	}
      ffeexpr_stack_->exprstack = left_operand->previous;	/* Pops binary-op
								   operands off stack. */
      ffeexpr_expr_kill_ (left_operand);
      ffeexpr_expr_kill_ (operand);
      operator->type = FFEEXPR_exprtypeOPERAND_;	/* Convert operator, but
							   save */
      operator->u.operand = reduced;	/* the line/column ffewhere info. */
      ffeexpr_exprstack_push_operand_ (operator);	/* Push it back on
							   stack. */
    }
}

/* ffeexpr_reduced_bool1_ -- Wrap up reduction of NOT operator

   reduced = ffeexpr_reduced_bool1_(reduced,op,r);

   Makes sure the argument for reduced has basictype of
   LOGICAL or (ugly) INTEGER.  If
   argument has where of CONSTANT, assign where CONSTANT to
   reduced, else assign where FLEETING.

   If these requirements cannot be met, generate error message.	 */

static ffebld
ffeexpr_reduced_bool1_ (ffebld reduced, ffeexprExpr_ op, ffeexprExpr_ r)
{
  ffeinfo rinfo, ninfo;
  ffeinfoBasictype rbt;
  ffeinfoKindtype rkt;
  ffeinfoRank rrk;
  ffeinfoKind rkd;
  ffeinfoWhere rwh, nwh;

  rinfo = ffebld_info (ffebld_left (reduced));
  rbt = ffeinfo_basictype (rinfo);
  rkt = ffeinfo_kindtype (rinfo);
  rrk = ffeinfo_rank (rinfo);
  rkd = ffeinfo_kind (rinfo);
  rwh = ffeinfo_where (rinfo);

  if (((rbt == FFEINFO_basictypeLOGICAL)
       || (ffe_is_ugly_logint () && (rbt == FFEINFO_basictypeINTEGER)))
      && (rrk == 0))
    {
      switch (rwh)
	{
	case FFEINFO_whereCONSTANT:
	  nwh = FFEINFO_whereCONSTANT;
	  break;

	case FFEINFO_whereIMMEDIATE:
	  nwh = FFEINFO_whereIMMEDIATE;
	  break;

	default:
	  nwh = FFEINFO_whereFLEETING;
	  break;
	}

      ninfo = ffeinfo_new (rbt, rkt, 0, FFEINFO_kindENTITY, nwh,
			   FFETARGET_charactersizeNONE);
      ffebld_set_info (reduced, ninfo);
      return reduced;
    }

  if ((rbt != FFEINFO_basictypeLOGICAL)
      && (!ffe_is_ugly_logint () || (rbt != FFEINFO_basictypeINTEGER)))
    {
      if ((rbt != FFEINFO_basictypeANY)
	  && ffebad_start (FFEBAD_NOT_ARG_TYPE))
	{
	  ffebad_here (0, ffelex_token_where_line (op->token), ffelex_token_where_column (op->token));
	  ffebad_here (1, ffelex_token_where_line (r->token), ffelex_token_where_column (r->token));
	  ffebad_finish ();
	}
    }
  else
    {
      if ((rkd != FFEINFO_kindANY)
	  && ffebad_start (FFEBAD_NOT_ARG_KIND))
	{
	  ffebad_here (0, ffelex_token_where_line (op->token), ffelex_token_where_column (op->token));
	  ffebad_here (1, ffelex_token_where_line (r->token), ffelex_token_where_column (r->token));
	  ffebad_string ("an array");
	  ffebad_finish ();
	}
    }

  reduced = ffebld_new_any ();
  ffebld_set_info (reduced, ffeinfo_new_any ());
  return reduced;
}

/* ffeexpr_reduced_bool2_ -- Wrap up reduction of boolean operators

   reduced = ffeexpr_reduced_bool2_(reduced,l,op,r);

   Makes sure the left and right arguments for reduced have basictype of
   LOGICAL or (ugly) INTEGER.  Determine common basictype and
   size for reduction (flag expression for combined hollerith/typeless
   situations for later determination of effective basictype).	If both left
   and right arguments have where of CONSTANT, assign where CONSTANT to
   reduced, else assign where FLEETING.	 Create CONVERT ops for args where
   needed.  Convert typeless
   constants to the desired type/size explicitly.

   If these requirements cannot be met, generate error message.	 */

static ffebld
ffeexpr_reduced_bool2_ (ffebld reduced, ffeexprExpr_ l, ffeexprExpr_ op,
			ffeexprExpr_ r)
{
  ffeinfo linfo, rinfo, ninfo;
  ffeinfoBasictype lbt, rbt, nbt;
  ffeinfoKindtype lkt, rkt, nkt;
  ffeinfoRank lrk, rrk;
  ffeinfoKind lkd, rkd;
  ffeinfoWhere lwh, rwh, nwh;

  linfo = ffebld_info (ffebld_left (reduced));
  lbt = ffeinfo_basictype (linfo);
  lkt = ffeinfo_kindtype (linfo);
  lrk = ffeinfo_rank (linfo);
  lkd = ffeinfo_kind (linfo);
  lwh = ffeinfo_where (linfo);

  rinfo = ffebld_info (ffebld_right (reduced));
  rbt = ffeinfo_basictype (rinfo);
  rkt = ffeinfo_kindtype (rinfo);
  rrk = ffeinfo_rank (rinfo);
  rkd = ffeinfo_kind (rinfo);
  rwh = ffeinfo_where (rinfo);

  ffeexpr_type_combine (&nbt, &nkt, lbt, lkt, rbt, rkt, op->token);

  if (((nbt == FFEINFO_basictypeLOGICAL)
       || (ffe_is_ugly_logint () && (nbt == FFEINFO_basictypeINTEGER)))
      && (lrk == 0) && (rrk == 0))
    {
      switch (lwh)
	{
	case FFEINFO_whereCONSTANT:
	  switch (rwh)
	    {
	    case FFEINFO_whereCONSTANT:
	      nwh = FFEINFO_whereCONSTANT;
	      break;

	    case FFEINFO_whereIMMEDIATE:
	      nwh = FFEINFO_whereIMMEDIATE;
	      break;

	    default:
	      nwh = FFEINFO_whereFLEETING;
	      break;
	    }
	  break;

	case FFEINFO_whereIMMEDIATE:
	  switch (rwh)
	    {
	    case FFEINFO_whereCONSTANT:
	    case FFEINFO_whereIMMEDIATE:
	      nwh = FFEINFO_whereIMMEDIATE;
	      break;

	    default:
	      nwh = FFEINFO_whereFLEETING;
	      break;
	    }
	  break;

	default:
	  nwh = FFEINFO_whereFLEETING;
	  break;
	}

      ninfo = ffeinfo_new (nbt, nkt, 0, FFEINFO_kindENTITY, nwh,
			   FFETARGET_charactersizeNONE);
      ffebld_set_info (reduced, ninfo);
      ffebld_set_left (reduced, ffeexpr_convert (ffebld_left (reduced),
	      l->token, op->token, nbt, nkt, 0, FFETARGET_charactersizeNONE,
						 FFEEXPR_contextLET));
      ffebld_set_right (reduced, ffeexpr_convert (ffebld_right (reduced),
	      r->token, op->token, nbt, nkt, 0, FFETARGET_charactersizeNONE,
						  FFEEXPR_contextLET));
      return reduced;
    }

  if ((lbt != FFEINFO_basictypeLOGICAL)
      && (!ffe_is_ugly_logint () || (lbt != FFEINFO_basictypeINTEGER)))
    {
      if ((rbt != FFEINFO_basictypeLOGICAL)
	  && (!ffe_is_ugly_logint () || (rbt != FFEINFO_basictypeINTEGER)))
	{
	  if ((lbt != FFEINFO_basictypeANY) && (rbt != FFEINFO_basictypeANY)
	      && ffebad_start (FFEBAD_BOOL_ARGS_TYPE))
	    {
	      ffebad_here (0, ffelex_token_where_line (op->token), ffelex_token_where_column (op->token));
	      ffebad_here (1, ffelex_token_where_line (l->token), ffelex_token_where_column (l->token));
	      ffebad_here (2, ffelex_token_where_line (r->token), ffelex_token_where_column (r->token));
	      ffebad_finish ();
	    }
	}
      else
	{
	  if ((lbt != FFEINFO_basictypeANY)
	      && ffebad_start (FFEBAD_BOOL_ARG_TYPE))
	    {
	      ffebad_here (0, ffelex_token_where_line (op->token), ffelex_token_where_column (op->token));
	      ffebad_here (1, ffelex_token_where_line (l->token), ffelex_token_where_column (l->token));
	      ffebad_finish ();
	    }
	}
    }
  else if ((rbt != FFEINFO_basictypeLOGICAL)
	   && (!ffe_is_ugly_logint () || (rbt != FFEINFO_basictypeINTEGER)))
    {
      if ((rbt != FFEINFO_basictypeANY)
	  && ffebad_start (FFEBAD_BOOL_ARG_TYPE))
	{
	  ffebad_here (0, ffelex_token_where_line (op->token), ffelex_token_where_column (op->token));
	  ffebad_here (1, ffelex_token_where_line (r->token), ffelex_token_where_column (r->token));
	  ffebad_finish ();
	}
    }
  else if (lrk != 0)
    {
      if ((lkd != FFEINFO_kindANY)
	  && ffebad_start (FFEBAD_BOOL_ARG_KIND))
	{
	  ffebad_here (0, ffelex_token_where_line (op->token), ffelex_token_where_column (op->token));
	  ffebad_here (1, ffelex_token_where_line (l->token), ffelex_token_where_column (l->token));
	  ffebad_string ("an array");
	  ffebad_finish ();
	}
    }
  else
    {
      if ((rkd != FFEINFO_kindANY)
	  && ffebad_start (FFEBAD_BOOL_ARG_KIND))
	{
	  ffebad_here (0, ffelex_token_where_line (op->token), ffelex_token_where_column (op->token));
	  ffebad_here (1, ffelex_token_where_line (r->token), ffelex_token_where_column (r->token));
	  ffebad_string ("an array");
	  ffebad_finish ();
	}
    }

  reduced = ffebld_new_any ();
  ffebld_set_info (reduced, ffeinfo_new_any ());
  return reduced;
}

/* ffeexpr_reduced_concatenate_ -- Wrap up reduction of concatenate operator

   reduced = ffeexpr_reduced_concatenate_(reduced,l,op,r);

   Makes sure the left and right arguments for reduced have basictype of
   CHARACTER and kind of SCALAR, FUNCTION, or STATEMENT FUNCTION.  Assign
   basictype of CHARACTER and kind of SCALAR to reduced.  Calculate effective
   size of concatenation and assign that size to reduced.  If both left and
   right arguments have where of CONSTANT, assign where CONSTANT to reduced,
   else assign where FLEETING.

   If these requirements cannot be met, generate error message using the
   info in l, op, and r arguments and assign basictype, size, kind, and where
   of ANY.  */

static ffebld
ffeexpr_reduced_concatenate_ (ffebld reduced, ffeexprExpr_ l, ffeexprExpr_ op,
			      ffeexprExpr_ r)
{
  ffeinfo linfo, rinfo, ninfo;
  ffeinfoBasictype lbt, rbt, nbt;
  ffeinfoKindtype lkt, rkt, nkt;
  ffeinfoRank lrk, rrk;
  ffeinfoKind lkd, rkd, nkd;
  ffeinfoWhere lwh, rwh, nwh;
  ffetargetCharacterSize lszm, lszk, rszm, rszk, nszk;

  linfo = ffebld_info (ffebld_left (reduced));
  lbt = ffeinfo_basictype (linfo);
  lkt = ffeinfo_kindtype (linfo);
  lrk = ffeinfo_rank (linfo);
  lkd = ffeinfo_kind (linfo);
  lwh = ffeinfo_where (linfo);
  lszk = ffeinfo_size (linfo);	/* Known size. */
  lszm = ffebld_size_max (ffebld_left (reduced));

  rinfo = ffebld_info (ffebld_right (reduced));
  rbt = ffeinfo_basictype (rinfo);
  rkt = ffeinfo_kindtype (rinfo);
  rrk = ffeinfo_rank (rinfo);
  rkd = ffeinfo_kind (rinfo);
  rwh = ffeinfo_where (rinfo);
  rszk = ffeinfo_size (rinfo);	/* Known size. */
  rszm = ffebld_size_max (ffebld_right (reduced));

  if ((lbt == FFEINFO_basictypeCHARACTER) && (rbt == FFEINFO_basictypeCHARACTER)
      && (lkt == rkt) && (lrk == 0) && (rrk == 0)
      && (((lszm != FFETARGET_charactersizeNONE)
	   && (rszm != FFETARGET_charactersizeNONE))
	  || (ffeexpr_context_outer_ (ffeexpr_stack_)
	      == FFEEXPR_contextLET)
	  || (ffeexpr_context_outer_ (ffeexpr_stack_)
	      == FFEEXPR_contextSFUNCDEF)))
    {
      nbt = FFEINFO_basictypeCHARACTER;
      nkd = FFEINFO_kindENTITY;
      if ((lszk == FFETARGET_charactersizeNONE)
	  || (rszk == FFETARGET_charactersizeNONE))
	nszk = FFETARGET_charactersizeNONE;	/* Ok only in rhs of LET
						   stmt. */
      else
	nszk = lszk + rszk;

      switch (lwh)
	{
	case FFEINFO_whereCONSTANT:
	  switch (rwh)
	    {
	    case FFEINFO_whereCONSTANT:
	      nwh = FFEINFO_whereCONSTANT;
	      break;

	    case FFEINFO_whereIMMEDIATE:
	      nwh = FFEINFO_whereIMMEDIATE;
	      break;

	    default:
	      nwh = FFEINFO_whereFLEETING;
	      break;
	    }
	  break;

	case FFEINFO_whereIMMEDIATE:
	  switch (rwh)
	    {
	    case FFEINFO_whereCONSTANT:
	    case FFEINFO_whereIMMEDIATE:
	      nwh = FFEINFO_whereIMMEDIATE;
	      break;

	    default:
	      nwh = FFEINFO_whereFLEETING;
	      break;
	    }
	  break;

	default:
	  nwh = FFEINFO_whereFLEETING;
	  break;
	}

      nkt = lkt;
      ninfo = ffeinfo_new (nbt, nkt, 0, nkd, nwh, nszk);
      ffebld_set_info (reduced, ninfo);
      return reduced;
    }

  if ((lbt != FFEINFO_basictypeCHARACTER) && (rbt != FFEINFO_basictypeCHARACTER))
    {
      if ((lbt != FFEINFO_basictypeANY) && (rbt != FFEINFO_basictypeANY)
	  && ffebad_start (FFEBAD_CONCAT_ARGS_TYPE))
	{
	  ffebad_here (0, ffelex_token_where_line (op->token), ffelex_token_where_column (op->token));
	  ffebad_here (1, ffelex_token_where_line (l->token), ffelex_token_where_column (l->token));
	  ffebad_here (2, ffelex_token_where_line (r->token), ffelex_token_where_column (r->token));
	  ffebad_finish ();
	}
    }
  else if (lbt != FFEINFO_basictypeCHARACTER)
    {
      if ((lbt != FFEINFO_basictypeANY)
	  && ffebad_start (FFEBAD_CONCAT_ARG_TYPE))
	{
	  ffebad_here (0, ffelex_token_where_line (op->token), ffelex_token_where_column (op->token));
	  ffebad_here (1, ffelex_token_where_line (l->token), ffelex_token_where_column (l->token));
	  ffebad_finish ();
	}
    }
  else if (rbt != FFEINFO_basictypeCHARACTER)
    {
      if ((rbt != FFEINFO_basictypeANY)
	  && ffebad_start (FFEBAD_CONCAT_ARG_TYPE))
	{
	  ffebad_here (0, ffelex_token_where_line (op->token), ffelex_token_where_column (op->token));
	  ffebad_here (1, ffelex_token_where_line (r->token), ffelex_token_where_column (r->token));
	  ffebad_finish ();
	}
    }
  else if ((lrk != 0) || (lszm == FFETARGET_charactersizeNONE))
    {
      if ((lkd != FFEINFO_kindANY)
	  && ffebad_start (FFEBAD_CONCAT_ARG_KIND))
	{
	  const char *what;

	  if (lrk != 0)
	    what = "an array";
	  else
	    what = "of indeterminate length";
	  ffebad_here (0, ffelex_token_where_line (op->token), ffelex_token_where_column (op->token));
	  ffebad_here (1, ffelex_token_where_line (l->token), ffelex_token_where_column (l->token));
	  ffebad_string (what);
	  ffebad_finish ();
	}
    }
  else
    {
      if (ffebad_start (FFEBAD_CONCAT_ARG_KIND))
	{
	  const char *what;

	  if (rrk != 0)
	    what = "an array";
	  else
	    what = "of indeterminate length";
	  ffebad_here (0, ffelex_token_where_line (op->token), ffelex_token_where_column (op->token));
	  ffebad_here (1, ffelex_token_where_line (r->token), ffelex_token_where_column (r->token));
	  ffebad_string (what);
	  ffebad_finish ();
	}
    }

  reduced = ffebld_new_any ();
  ffebld_set_info (reduced, ffeinfo_new_any ());
  return reduced;
}

/* ffeexpr_reduced_eqop2_ -- Wrap up reduction of EQ and NE operators

   reduced = ffeexpr_reduced_eqop2_(reduced,l,op,r);

   Makes sure the left and right arguments for reduced have basictype of
   INTEGER, REAL, COMPLEX, or CHARACTER.  Determine common basictype and
   size for reduction.	If both left
   and right arguments have where of CONSTANT, assign where CONSTANT to
   reduced, else assign where FLEETING.	 Create CONVERT ops for args where
   needed.  Convert typeless
   constants to the desired type/size explicitly.

   If these requirements cannot be met, generate error message.	 */

static ffebld
ffeexpr_reduced_eqop2_ (ffebld reduced, ffeexprExpr_ l, ffeexprExpr_ op,
			ffeexprExpr_ r)
{
  ffeinfo linfo, rinfo, ninfo;
  ffeinfoBasictype lbt, rbt, nbt;
  ffeinfoKindtype lkt, rkt, nkt;
  ffeinfoRank lrk, rrk;
  ffeinfoKind lkd, rkd;
  ffeinfoWhere lwh, rwh, nwh;
  ffetargetCharacterSize lsz, rsz;

  linfo = ffebld_info (ffebld_left (reduced));
  lbt = ffeinfo_basictype (linfo);
  lkt = ffeinfo_kindtype (linfo);
  lrk = ffeinfo_rank (linfo);
  lkd = ffeinfo_kind (linfo);
  lwh = ffeinfo_where (linfo);
  lsz = ffebld_size_known (ffebld_left (reduced));

  rinfo = ffebld_info (ffebld_right (reduced));
  rbt = ffeinfo_basictype (rinfo);
  rkt = ffeinfo_kindtype (rinfo);
  rrk = ffeinfo_rank (rinfo);
  rkd = ffeinfo_kind (rinfo);
  rwh = ffeinfo_where (rinfo);
  rsz = ffebld_size_known (ffebld_right (reduced));

  ffeexpr_type_combine (&nbt, &nkt, lbt, lkt, rbt, rkt, op->token);

  if (((nbt == FFEINFO_basictypeINTEGER) || (nbt == FFEINFO_basictypeREAL)
       || (nbt == FFEINFO_basictypeCOMPLEX) || (nbt == FFEINFO_basictypeCHARACTER))
      && (lrk == 0) && (rrk == 0))
    {
      switch (lwh)
	{
	case FFEINFO_whereCONSTANT:
	  switch (rwh)
	    {
	    case FFEINFO_whereCONSTANT:
	      nwh = FFEINFO_whereCONSTANT;
	      break;

	    case FFEINFO_whereIMMEDIATE:
	      nwh = FFEINFO_whereIMMEDIATE;
	      break;

	    default:
	      nwh = FFEINFO_whereFLEETING;
	      break;
	    }
	  break;

	case FFEINFO_whereIMMEDIATE:
	  switch (rwh)
	    {
	    case FFEINFO_whereCONSTANT:
	    case FFEINFO_whereIMMEDIATE:
	      nwh = FFEINFO_whereIMMEDIATE;
	      break;

	    default:
	      nwh = FFEINFO_whereFLEETING;
	      break;
	    }
	  break;

	default:
	  nwh = FFEINFO_whereFLEETING;
	  break;
	}

      if ((lsz != FFETARGET_charactersizeNONE)
	  && (rsz != FFETARGET_charactersizeNONE))
	lsz = rsz = (lsz > rsz) ? lsz : rsz;

      ninfo = ffeinfo_new (FFEINFO_basictypeLOGICAL, FFEINFO_kindtypeLOGICALDEFAULT,
		   0, FFEINFO_kindENTITY, nwh, FFETARGET_charactersizeNONE);
      ffebld_set_info (reduced, ninfo);
      ffebld_set_left (reduced, ffeexpr_convert (ffebld_left (reduced),
				      l->token, op->token, nbt, nkt, 0, lsz,
						 FFEEXPR_contextLET));
      ffebld_set_right (reduced, ffeexpr_convert (ffebld_right (reduced),
				      r->token, op->token, nbt, nkt, 0, rsz,
						  FFEEXPR_contextLET));
      return reduced;
    }

  if ((lbt == FFEINFO_basictypeLOGICAL)
      && (rbt == FFEINFO_basictypeLOGICAL))
    {
      /* xgettext:no-c-format */
      if (ffebad_start_msg ("Use .EQV./.NEQV. instead of .EQ./.NE. at %0 for LOGICAL operands at %1 and %2",
			    FFEBAD_severityFATAL))
	{
	  ffebad_here (0, ffelex_token_where_line (op->token), ffelex_token_where_column (op->token));
	  ffebad_here (1, ffelex_token_where_line (l->token), ffelex_token_where_column (l->token));
	  ffebad_here (2, ffelex_token_where_line (r->token), ffelex_token_where_column (r->token));
	  ffebad_finish ();
	}
    }
  else if ((lbt != FFEINFO_basictypeINTEGER) && (lbt != FFEINFO_basictypeREAL)
      && (lbt != FFEINFO_basictypeCOMPLEX) && (lbt != FFEINFO_basictypeCHARACTER))
    {
      if ((rbt != FFEINFO_basictypeINTEGER) && (rbt != FFEINFO_basictypeREAL)
	  && (rbt != FFEINFO_basictypeCOMPLEX) && (rbt != FFEINFO_basictypeCHARACTER))
	{
	  if ((lbt != FFEINFO_basictypeANY) && (rbt != FFEINFO_basictypeANY)
	      && ffebad_start (FFEBAD_EQOP_ARGS_TYPE))
	    {
	      ffebad_here (0, ffelex_token_where_line (op->token), ffelex_token_where_column (op->token));
	      ffebad_here (1, ffelex_token_where_line (l->token), ffelex_token_where_column (l->token));
	      ffebad_here (2, ffelex_token_where_line (r->token), ffelex_token_where_column (r->token));
	      ffebad_finish ();
	    }
	}
      else
	{
	  if ((lbt != FFEINFO_basictypeANY)
	      && ffebad_start (FFEBAD_EQOP_ARG_TYPE))
	    {
	      ffebad_here (0, ffelex_token_where_line (op->token), ffelex_token_where_column (op->token));
	      ffebad_here (1, ffelex_token_where_line (l->token), ffelex_token_where_column (l->token));
	      ffebad_finish ();
	    }
	}
    }
  else if ((rbt != FFEINFO_basictypeINTEGER) && (rbt != FFEINFO_basictypeREAL)
	   && (rbt != FFEINFO_basictypeCOMPLEX) && (rbt != FFEINFO_basictypeCHARACTER))
    {
      if ((rbt != FFEINFO_basictypeANY)
	  && ffebad_start (FFEBAD_EQOP_ARG_TYPE))
	{
	  ffebad_here (0, ffelex_token_where_line (op->token), ffelex_token_where_column (op->token));
	  ffebad_here (1, ffelex_token_where_line (r->token), ffelex_token_where_column (r->token));
	  ffebad_finish ();
	}
    }
  else if (lrk != 0)
    {
      if ((lkd != FFEINFO_kindANY)
	  && ffebad_start (FFEBAD_EQOP_ARG_KIND))
	{
	  ffebad_here (0, ffelex_token_where_line (op->token), ffelex_token_where_column (op->token));
	  ffebad_here (1, ffelex_token_where_line (l->token), ffelex_token_where_column (l->token));
	  ffebad_string ("an array");
	  ffebad_finish ();
	}
    }
  else
    {
      if ((rkd != FFEINFO_kindANY)
	  && ffebad_start (FFEBAD_EQOP_ARG_KIND))
	{
	  ffebad_here (0, ffelex_token_where_line (op->token), ffelex_token_where_column (op->token));
	  ffebad_here (1, ffelex_token_where_line (r->token), ffelex_token_where_column (r->token));
	  ffebad_string ("an array");
	  ffebad_finish ();
	}
    }

  reduced = ffebld_new_any ();
  ffebld_set_info (reduced, ffeinfo_new_any ());
  return reduced;
}

/* ffeexpr_reduced_math1_ -- Wrap up reduction of + - unary operators

   reduced = ffeexpr_reduced_math1_(reduced,op,r);

   Makes sure the argument for reduced has basictype of
   INTEGER, REAL, or COMPLEX.  If the argument has where of CONSTANT,
   assign where CONSTANT to
   reduced, else assign where FLEETING.

   If these requirements cannot be met, generate error message.	 */

static ffebld
ffeexpr_reduced_math1_ (ffebld reduced, ffeexprExpr_ op, ffeexprExpr_ r)
{
  ffeinfo rinfo, ninfo;
  ffeinfoBasictype rbt;
  ffeinfoKindtype rkt;
  ffeinfoRank rrk;
  ffeinfoKind rkd;
  ffeinfoWhere rwh, nwh;

  rinfo = ffebld_info (ffebld_left (reduced));
  rbt = ffeinfo_basictype (rinfo);
  rkt = ffeinfo_kindtype (rinfo);
  rrk = ffeinfo_rank (rinfo);
  rkd = ffeinfo_kind (rinfo);
  rwh = ffeinfo_where (rinfo);

  if (((rbt == FFEINFO_basictypeINTEGER) || (rbt == FFEINFO_basictypeREAL)
       || (rbt == FFEINFO_basictypeCOMPLEX)) && (rrk == 0))
    {
      switch (rwh)
	{
	case FFEINFO_whereCONSTANT:
	  nwh = FFEINFO_whereCONSTANT;
	  break;

	case FFEINFO_whereIMMEDIATE:
	  nwh = FFEINFO_whereIMMEDIATE;
	  break;

	default:
	  nwh = FFEINFO_whereFLEETING;
	  break;
	}

      ninfo = ffeinfo_new (rbt, rkt, 0, FFEINFO_kindENTITY, nwh,
			   FFETARGET_charactersizeNONE);
      ffebld_set_info (reduced, ninfo);
      return reduced;
    }

  if ((rbt != FFEINFO_basictypeINTEGER) && (rbt != FFEINFO_basictypeREAL)
      && (rbt != FFEINFO_basictypeCOMPLEX))
    {
      if ((rbt != FFEINFO_basictypeANY)
	  && ffebad_start (FFEBAD_MATH_ARG_TYPE))
	{
	  ffebad_here (0, ffelex_token_where_line (op->token), ffelex_token_where_column (op->token));
	  ffebad_here (1, ffelex_token_where_line (r->token), ffelex_token_where_column (r->token));
	  ffebad_finish ();
	}
    }
  else
    {
      if ((rkd != FFEINFO_kindANY)
	  && ffebad_start (FFEBAD_MATH_ARG_KIND))
	{
	  ffebad_here (0, ffelex_token_where_line (op->token), ffelex_token_where_column (op->token));
	  ffebad_here (1, ffelex_token_where_line (r->token), ffelex_token_where_column (r->token));
	  ffebad_string ("an array");
	  ffebad_finish ();
	}
    }

  reduced = ffebld_new_any ();
  ffebld_set_info (reduced, ffeinfo_new_any ());
  return reduced;
}

/* ffeexpr_reduced_math2_ -- Wrap up reduction of + - * / operators

   reduced = ffeexpr_reduced_math2_(reduced,l,op,r);

   Makes sure the left and right arguments for reduced have basictype of
   INTEGER, REAL, or COMPLEX.  Determine common basictype and
   size for reduction (flag expression for combined hollerith/typeless
   situations for later determination of effective basictype).	If both left
   and right arguments have where of CONSTANT, assign where CONSTANT to
   reduced, else assign where FLEETING.	 Create CONVERT ops for args where
   needed.  Convert typeless
   constants to the desired type/size explicitly.

   If these requirements cannot be met, generate error message.	 */

static ffebld
ffeexpr_reduced_math2_ (ffebld reduced, ffeexprExpr_ l, ffeexprExpr_ op,
			ffeexprExpr_ r)
{
  ffeinfo linfo, rinfo, ninfo;
  ffeinfoBasictype lbt, rbt, nbt;
  ffeinfoKindtype lkt, rkt, nkt;
  ffeinfoRank lrk, rrk;
  ffeinfoKind lkd, rkd;
  ffeinfoWhere lwh, rwh, nwh;

  linfo = ffebld_info (ffebld_left (reduced));
  lbt = ffeinfo_basictype (linfo);
  lkt = ffeinfo_kindtype (linfo);
  lrk = ffeinfo_rank (linfo);
  lkd = ffeinfo_kind (linfo);
  lwh = ffeinfo_where (linfo);

  rinfo = ffebld_info (ffebld_right (reduced));
  rbt = ffeinfo_basictype (rinfo);
  rkt = ffeinfo_kindtype (rinfo);
  rrk = ffeinfo_rank (rinfo);
  rkd = ffeinfo_kind (rinfo);
  rwh = ffeinfo_where (rinfo);

  ffeexpr_type_combine (&nbt, &nkt, lbt, lkt, rbt, rkt, op->token);

  if (((nbt == FFEINFO_basictypeINTEGER) || (nbt == FFEINFO_basictypeREAL)
       || (nbt == FFEINFO_basictypeCOMPLEX)) && (lrk == 0) && (rrk == 0))
    {
      switch (lwh)
	{
	case FFEINFO_whereCONSTANT:
	  switch (rwh)
	    {
	    case FFEINFO_whereCONSTANT:
	      nwh = FFEINFO_whereCONSTANT;
	      break;

	    case FFEINFO_whereIMMEDIATE:
	      nwh = FFEINFO_whereIMMEDIATE;
	      break;

	    default:
	      nwh = FFEINFO_whereFLEETING;
	      break;
	    }
	  break;

	case FFEINFO_whereIMMEDIATE:
	  switch (rwh)
	    {
	    case FFEINFO_whereCONSTANT:
	    case FFEINFO_whereIMMEDIATE:
	      nwh = FFEINFO_whereIMMEDIATE;
	      break;

	    default:
	      nwh = FFEINFO_whereFLEETING;
	      break;
	    }
	  break;

	default:
	  nwh = FFEINFO_whereFLEETING;
	  break;
	}

      ninfo = ffeinfo_new (nbt, nkt, 0, FFEINFO_kindENTITY, nwh,
			   FFETARGET_charactersizeNONE);
      ffebld_set_info (reduced, ninfo);
      ffebld_set_left (reduced, ffeexpr_convert (ffebld_left (reduced),
	      l->token, op->token, nbt, nkt, 0, FFETARGET_charactersizeNONE,
						 FFEEXPR_contextLET));
      ffebld_set_right (reduced, ffeexpr_convert (ffebld_right (reduced),
	      r->token, op->token, nbt, nkt, 0, FFETARGET_charactersizeNONE,
						  FFEEXPR_contextLET));
      return reduced;
    }

  if ((lbt != FFEINFO_basictypeINTEGER) && (lbt != FFEINFO_basictypeREAL)
      && (lbt != FFEINFO_basictypeCOMPLEX))
    {
      if ((rbt != FFEINFO_basictypeINTEGER)
      && (rbt != FFEINFO_basictypeREAL) && (rbt != FFEINFO_basictypeCOMPLEX))
	{
	  if ((lbt != FFEINFO_basictypeANY) && (rbt != FFEINFO_basictypeANY)
	      && ffebad_start (FFEBAD_MATH_ARGS_TYPE))
	    {
	      ffebad_here (0, ffelex_token_where_line (op->token), ffelex_token_where_column (op->token));
	      ffebad_here (1, ffelex_token_where_line (l->token), ffelex_token_where_column (l->token));
	      ffebad_here (2, ffelex_token_where_line (r->token), ffelex_token_where_column (r->token));
	      ffebad_finish ();
	    }
	}
      else
	{
	  if ((lbt != FFEINFO_basictypeANY)
	      && ffebad_start (FFEBAD_MATH_ARG_TYPE))
	    {
	      ffebad_here (0, ffelex_token_where_line (op->token), ffelex_token_where_column (op->token));
	      ffebad_here (1, ffelex_token_where_line (l->token), ffelex_token_where_column (l->token));
	      ffebad_finish ();
	    }
	}
    }
  else if ((rbt != FFEINFO_basictypeINTEGER) && (rbt != FFEINFO_basictypeREAL)
	   && (rbt != FFEINFO_basictypeCOMPLEX))
    {
      if ((rbt != FFEINFO_basictypeANY)
	  && ffebad_start (FFEBAD_MATH_ARG_TYPE))
	{
	  ffebad_here (0, ffelex_token_where_line (op->token), ffelex_token_where_column (op->token));
	  ffebad_here (1, ffelex_token_where_line (r->token), ffelex_token_where_column (r->token));
	  ffebad_finish ();
	}
    }
  else if (lrk != 0)
    {
      if ((lkd != FFEINFO_kindANY)
	  && ffebad_start (FFEBAD_MATH_ARG_KIND))
	{
	  ffebad_here (0, ffelex_token_where_line (op->token), ffelex_token_where_column (op->token));
	  ffebad_here (1, ffelex_token_where_line (l->token), ffelex_token_where_column (l->token));
	  ffebad_string ("an array");
	  ffebad_finish ();
	}
    }
  else
    {
      if ((rkd != FFEINFO_kindANY)
	  && ffebad_start (FFEBAD_MATH_ARG_KIND))
	{
	  ffebad_here (0, ffelex_token_where_line (op->token), ffelex_token_where_column (op->token));
	  ffebad_here (1, ffelex_token_where_line (r->token), ffelex_token_where_column (r->token));
	  ffebad_string ("an array");
	  ffebad_finish ();
	}
    }

  reduced = ffebld_new_any ();
  ffebld_set_info (reduced, ffeinfo_new_any ());
  return reduced;
}

/* ffeexpr_reduced_power_ -- Wrap up reduction of ** operator

   reduced = ffeexpr_reduced_power_(reduced,l,op,r);

   Makes sure the left and right arguments for reduced have basictype of
   INTEGER, REAL, or COMPLEX.  Determine common basictype and
   size for reduction (flag expression for combined hollerith/typeless
   situations for later determination of effective basictype).	If both left
   and right arguments have where of CONSTANT, assign where CONSTANT to
   reduced, else assign where FLEETING.	 Create CONVERT ops for args where
   needed.  Note that real**int or complex**int
   comes out as int = real**int etc with no conversions.

   If these requirements cannot be met, generate error message using the
   info in l, op, and r arguments and assign basictype, size, kind, and where
   of ANY.  */

static ffebld
ffeexpr_reduced_power_ (ffebld reduced, ffeexprExpr_ l, ffeexprExpr_ op,
			ffeexprExpr_ r)
{
  ffeinfo linfo, rinfo, ninfo;
  ffeinfoBasictype lbt, rbt, nbt;
  ffeinfoKindtype lkt, rkt, nkt;
  ffeinfoRank lrk, rrk;
  ffeinfoKind lkd, rkd;
  ffeinfoWhere lwh, rwh, nwh;

  linfo = ffebld_info (ffebld_left (reduced));
  lbt = ffeinfo_basictype (linfo);
  lkt = ffeinfo_kindtype (linfo);
  lrk = ffeinfo_rank (linfo);
  lkd = ffeinfo_kind (linfo);
  lwh = ffeinfo_where (linfo);

  rinfo = ffebld_info (ffebld_right (reduced));
  rbt = ffeinfo_basictype (rinfo);
  rkt = ffeinfo_kindtype (rinfo);
  rrk = ffeinfo_rank (rinfo);
  rkd = ffeinfo_kind (rinfo);
  rwh = ffeinfo_where (rinfo);

  if ((rbt == FFEINFO_basictypeINTEGER)
      && ((lbt == FFEINFO_basictypeREAL)
	  || (lbt == FFEINFO_basictypeCOMPLEX)))
    {
      nbt = lbt;
      nkt = ffeinfo_kindtype_max (nbt, lkt, FFEINFO_kindtypeREALDEFAULT);
      if (nkt != FFEINFO_kindtypeREALDEFAULT)
	{
	  nkt = ffeinfo_kindtype_max (nbt, lkt, FFEINFO_kindtypeREALDOUBLE);
	  if (nkt != FFEINFO_kindtypeREALDOUBLE)
	    nkt = FFEINFO_kindtypeREALDOUBLE;	/* Highest kt we can power! */
	}
      if (rkt == FFEINFO_kindtypeINTEGER4)
	{
	  /* xgettext:no-c-format */
	  ffebad_start_msg ("Unsupported operand for ** at %1 -- converting to default INTEGER",
			    FFEBAD_severityWARNING);
	  ffebad_here (0, ffelex_token_where_line (r->token), ffelex_token_where_column (r->token));
	  ffebad_finish ();
	}
      if (rkt != FFEINFO_kindtypeINTEGERDEFAULT)
	{
	  ffebld_set_right (reduced, ffeexpr_convert (ffebld_right (reduced),
						      r->token, op->token,
		FFEINFO_basictypeINTEGER, FFEINFO_kindtypeINTEGERDEFAULT, 0,
						FFETARGET_charactersizeNONE,
						      FFEEXPR_contextLET));
	  rkt = FFEINFO_kindtypeINTEGERDEFAULT;
	}
    }
  else
    {
      ffeexpr_type_combine (&nbt, &nkt, lbt, lkt, rbt, rkt, op->token);

#if 0	/* INTEGER4**INTEGER4 works now. */
      if ((nbt == FFEINFO_basictypeINTEGER)
	  && (nkt != FFEINFO_kindtypeINTEGERDEFAULT))
	nkt = FFEINFO_kindtypeINTEGERDEFAULT;	/* Highest kt we can power! */
#endif
      if (((nbt == FFEINFO_basictypeREAL)
	   || (nbt == FFEINFO_basictypeCOMPLEX))
	  && (nkt != FFEINFO_kindtypeREALDEFAULT))
	{
	  nkt = ffeinfo_kindtype_max (nbt, nkt, FFEINFO_kindtypeREALDOUBLE);
	  if (nkt != FFEINFO_kindtypeREALDOUBLE)
	    nkt = FFEINFO_kindtypeREALDOUBLE;	/* Highest kt we can power! */
	}
      /* else Gonna turn into an error below. */
    }

  if (((nbt == FFEINFO_basictypeINTEGER) || (nbt == FFEINFO_basictypeREAL)
       || (nbt == FFEINFO_basictypeCOMPLEX)) && (lrk == 0) && (rrk == 0))
    {
      switch (lwh)
	{
	case FFEINFO_whereCONSTANT:
	  switch (rwh)
	    {
	    case FFEINFO_whereCONSTANT:
	      nwh = FFEINFO_whereCONSTANT;
	      break;

	    case FFEINFO_whereIMMEDIATE:
	      nwh = FFEINFO_whereIMMEDIATE;
	      break;

	    default:
	      nwh = FFEINFO_whereFLEETING;
	      break;
	    }
	  break;

	case FFEINFO_whereIMMEDIATE:
	  switch (rwh)
	    {
	    case FFEINFO_whereCONSTANT:
	    case FFEINFO_whereIMMEDIATE:
	      nwh = FFEINFO_whereIMMEDIATE;
	      break;

	    default:
	      nwh = FFEINFO_whereFLEETING;
	      break;
	    }
	  break;

	default:
	  nwh = FFEINFO_whereFLEETING;
	  break;
	}

      ninfo = ffeinfo_new (nbt, nkt, 0, FFEINFO_kindENTITY, nwh,
			   FFETARGET_charactersizeNONE);
      ffebld_set_info (reduced, ninfo);
      ffebld_set_left (reduced, ffeexpr_convert (ffebld_left (reduced),
	      l->token, op->token, nbt, nkt, 0, FFETARGET_charactersizeNONE,
						 FFEEXPR_contextLET));
      if (rbt != FFEINFO_basictypeINTEGER)
	ffebld_set_right (reduced, ffeexpr_convert (ffebld_right (reduced),
	      r->token, op->token, nbt, nkt, 0, FFETARGET_charactersizeNONE,
						    FFEEXPR_contextLET));
      return reduced;
    }

  if ((lbt != FFEINFO_basictypeINTEGER) && (lbt != FFEINFO_basictypeREAL)
      && (lbt != FFEINFO_basictypeCOMPLEX))
    {
      if ((rbt != FFEINFO_basictypeINTEGER)
      && (rbt != FFEINFO_basictypeREAL) && (rbt != FFEINFO_basictypeCOMPLEX))
	{
	  if ((lbt != FFEINFO_basictypeANY) && (rbt != FFEINFO_basictypeANY)
	      && ffebad_start (FFEBAD_MATH_ARGS_TYPE))
	    {
	      ffebad_here (0, ffelex_token_where_line (op->token), ffelex_token_where_column (op->token));
	      ffebad_here (1, ffelex_token_where_line (l->token), ffelex_token_where_column (l->token));
	      ffebad_here (2, ffelex_token_where_line (r->token), ffelex_token_where_column (r->token));
	      ffebad_finish ();
	    }
	}
      else
	{
	  if ((lbt != FFEINFO_basictypeANY)
	      && ffebad_start (FFEBAD_MATH_ARG_TYPE))
	    {
	      ffebad_here (0, ffelex_token_where_line (op->token), ffelex_token_where_column (op->token));
	      ffebad_here (1, ffelex_token_where_line (l->token), ffelex_token_where_column (l->token));
	      ffebad_finish ();
	    }
	}
    }
  else if ((rbt != FFEINFO_basictypeINTEGER) && (rbt != FFEINFO_basictypeREAL)
	   && (rbt != FFEINFO_basictypeCOMPLEX))
    {
      if ((rbt != FFEINFO_basictypeANY)
	  && ffebad_start (FFEBAD_MATH_ARG_TYPE))
	{
	  ffebad_here (0, ffelex_token_where_line (op->token), ffelex_token_where_column (op->token));
	  ffebad_here (1, ffelex_token_where_line (r->token), ffelex_token_where_column (r->token));
	  ffebad_finish ();
	}
    }
  else if (lrk != 0)
    {
      if ((lkd != FFEINFO_kindANY)
	  && ffebad_start (FFEBAD_MATH_ARG_KIND))
	{
	  ffebad_here (0, ffelex_token_where_line (op->token), ffelex_token_where_column (op->token));
	  ffebad_here (1, ffelex_token_where_line (l->token), ffelex_token_where_column (l->token));
	  ffebad_string ("an array");
	  ffebad_finish ();
	}
    }
  else
    {
      if ((rkd != FFEINFO_kindANY)
	  && ffebad_start (FFEBAD_MATH_ARG_KIND))
	{
	  ffebad_here (0, ffelex_token_where_line (op->token), ffelex_token_where_column (op->token));
	  ffebad_here (1, ffelex_token_where_line (r->token), ffelex_token_where_column (r->token));
	  ffebad_string ("an array");
	  ffebad_finish ();
	}
    }

  reduced = ffebld_new_any ();
  ffebld_set_info (reduced, ffeinfo_new_any ());
  return reduced;
}

/* ffeexpr_reduced_relop2_ -- Wrap up reduction of LT, LE, GE, and GT operators

   reduced = ffeexpr_reduced_relop2_(reduced,l,op,r);

   Makes sure the left and right arguments for reduced have basictype of
   INTEGER, REAL, or CHARACTER.	 Determine common basictype and
   size for reduction.	If both left
   and right arguments have where of CONSTANT, assign where CONSTANT to
   reduced, else assign where FLEETING.	 Create CONVERT ops for args where
   needed.  Convert typeless
   constants to the desired type/size explicitly.

   If these requirements cannot be met, generate error message.	 */

static ffebld
ffeexpr_reduced_relop2_ (ffebld reduced, ffeexprExpr_ l, ffeexprExpr_ op,
			 ffeexprExpr_ r)
{
  ffeinfo linfo, rinfo, ninfo;
  ffeinfoBasictype lbt, rbt, nbt;
  ffeinfoKindtype lkt, rkt, nkt;
  ffeinfoRank lrk, rrk;
  ffeinfoKind lkd, rkd;
  ffeinfoWhere lwh, rwh, nwh;
  ffetargetCharacterSize lsz, rsz;

  linfo = ffebld_info (ffebld_left (reduced));
  lbt = ffeinfo_basictype (linfo);
  lkt = ffeinfo_kindtype (linfo);
  lrk = ffeinfo_rank (linfo);
  lkd = ffeinfo_kind (linfo);
  lwh = ffeinfo_where (linfo);
  lsz = ffebld_size_known (ffebld_left (reduced));

  rinfo = ffebld_info (ffebld_right (reduced));
  rbt = ffeinfo_basictype (rinfo);
  rkt = ffeinfo_kindtype (rinfo);
  rrk = ffeinfo_rank (rinfo);
  rkd = ffeinfo_kind (rinfo);
  rwh = ffeinfo_where (rinfo);
  rsz = ffebld_size_known (ffebld_right (reduced));

  ffeexpr_type_combine (&nbt, &nkt, lbt, lkt, rbt, rkt, op->token);

  if (((nbt == FFEINFO_basictypeINTEGER) || (nbt == FFEINFO_basictypeREAL)
       || (nbt == FFEINFO_basictypeCHARACTER))
      && (lrk == 0) && (rrk == 0))
    {
      switch (lwh)
	{
	case FFEINFO_whereCONSTANT:
	  switch (rwh)
	    {
	    case FFEINFO_whereCONSTANT:
	      nwh = FFEINFO_whereCONSTANT;
	      break;

	    case FFEINFO_whereIMMEDIATE:
	      nwh = FFEINFO_whereIMMEDIATE;
	      break;

	    default:
	      nwh = FFEINFO_whereFLEETING;
	      break;
	    }
	  break;

	case FFEINFO_whereIMMEDIATE:
	  switch (rwh)
	    {
	    case FFEINFO_whereCONSTANT:
	    case FFEINFO_whereIMMEDIATE:
	      nwh = FFEINFO_whereIMMEDIATE;
	      break;

	    default:
	      nwh = FFEINFO_whereFLEETING;
	      break;
	    }
	  break;

	default:
	  nwh = FFEINFO_whereFLEETING;
	  break;
	}

      if ((lsz != FFETARGET_charactersizeNONE)
	  && (rsz != FFETARGET_charactersizeNONE))
	lsz = rsz = (lsz > rsz) ? lsz : rsz;

      ninfo = ffeinfo_new (FFEINFO_basictypeLOGICAL, FFEINFO_kindtypeLOGICALDEFAULT,
		   0, FFEINFO_kindENTITY, nwh, FFETARGET_charactersizeNONE);
      ffebld_set_info (reduced, ninfo);
      ffebld_set_left (reduced, ffeexpr_convert (ffebld_left (reduced),
				      l->token, op->token, nbt, nkt, 0, lsz,
						 FFEEXPR_contextLET));
      ffebld_set_right (reduced, ffeexpr_convert (ffebld_right (reduced),
				      r->token, op->token, nbt, nkt, 0, rsz,
						  FFEEXPR_contextLET));
      return reduced;
    }

  if ((lbt != FFEINFO_basictypeINTEGER) && (lbt != FFEINFO_basictypeREAL)
      && (lbt != FFEINFO_basictypeCHARACTER))
    {
      if ((rbt != FFEINFO_basictypeINTEGER) && (rbt != FFEINFO_basictypeREAL)
	  && (rbt != FFEINFO_basictypeCHARACTER))
	{
	  if ((lbt != FFEINFO_basictypeANY) && (rbt != FFEINFO_basictypeANY)
	      && ffebad_start (FFEBAD_RELOP_ARGS_TYPE))
	    {
	      ffebad_here (0, ffelex_token_where_line (op->token), ffelex_token_where_column (op->token));
	      ffebad_here (1, ffelex_token_where_line (l->token), ffelex_token_where_column (l->token));
	      ffebad_here (2, ffelex_token_where_line (r->token), ffelex_token_where_column (r->token));
	      ffebad_finish ();
	    }
	}
      else
	{
	  if ((lbt != FFEINFO_basictypeANY)
	      && ffebad_start (FFEBAD_RELOP_ARG_TYPE))
	    {
	      ffebad_here (0, ffelex_token_where_line (op->token), ffelex_token_where_column (op->token));
	      ffebad_here (1, ffelex_token_where_line (l->token), ffelex_token_where_column (l->token));
	      ffebad_finish ();
	    }
	}
    }
  else if ((rbt != FFEINFO_basictypeINTEGER) && (rbt != FFEINFO_basictypeREAL)
	   && (rbt != FFEINFO_basictypeCHARACTER))
    {
      if ((rbt != FFEINFO_basictypeANY)
	  && ffebad_start (FFEBAD_RELOP_ARG_TYPE))
	{
	  ffebad_here (0, ffelex_token_where_line (op->token), ffelex_token_where_column (op->token));
	  ffebad_here (1, ffelex_token_where_line (r->token), ffelex_token_where_column (r->token));
	  ffebad_finish ();
	}
    }
  else if (lrk != 0)
    {
      if ((lkd != FFEINFO_kindANY)
	  && ffebad_start (FFEBAD_RELOP_ARG_KIND))
	{
	  ffebad_here (0, ffelex_token_where_line (op->token), ffelex_token_where_column (op->token));
	  ffebad_here (1, ffelex_token_where_line (l->token), ffelex_token_where_column (l->token));
	  ffebad_string ("an array");
	  ffebad_finish ();
	}
    }
  else
    {
      if ((rkd != FFEINFO_kindANY)
	  && ffebad_start (FFEBAD_RELOP_ARG_KIND))
	{
	  ffebad_here (0, ffelex_token_where_line (op->token), ffelex_token_where_column (op->token));
	  ffebad_here (1, ffelex_token_where_line (r->token), ffelex_token_where_column (r->token));
	  ffebad_string ("an array");
	  ffebad_finish ();
	}
    }

  reduced = ffebld_new_any ();
  ffebld_set_info (reduced, ffeinfo_new_any ());
  return reduced;
}

/* ffeexpr_reduced_ugly1_ -- Deal with TYPELESS, HOLLERITH, and LOGICAL

   reduced = ffeexpr_reduced_ugly1_(reduced,op,r);

   Sigh.  */

static ffebld
ffeexpr_reduced_ugly1_ (ffebld reduced, ffeexprExpr_ op, ffeexprExpr_ r)
{
  ffeinfo rinfo;
  ffeinfoBasictype rbt;
  ffeinfoKindtype rkt;
  ffeinfoRank rrk;
  ffeinfoKind rkd;
  ffeinfoWhere rwh;

  rinfo = ffebld_info (ffebld_left (reduced));
  rbt = ffeinfo_basictype (rinfo);
  rkt = ffeinfo_kindtype (rinfo);
  rrk = ffeinfo_rank (rinfo);
  rkd = ffeinfo_kind (rinfo);
  rwh = ffeinfo_where (rinfo);

  if ((rbt == FFEINFO_basictypeTYPELESS)
      || (rbt == FFEINFO_basictypeHOLLERITH))
    {
      ffebld_set_left (reduced, ffeexpr_convert (ffebld_left (reduced),
			      r->token, op->token, FFEINFO_basictypeINTEGER,
					  FFEINFO_kindtypeINTEGERDEFAULT, 0,
						 FFETARGET_charactersizeNONE,
						 FFEEXPR_contextLET));
      rinfo = ffebld_info (ffebld_left (reduced));
      rbt = FFEINFO_basictypeINTEGER;
      rkt = FFEINFO_kindtypeINTEGERDEFAULT;
      rrk = 0;
      rkd = FFEINFO_kindENTITY;
      rwh = ffeinfo_where (rinfo);
    }

  if (rbt == FFEINFO_basictypeLOGICAL)
    {
      ffebld_set_left (reduced, ffeexpr_convert (ffebld_left (reduced),
			      r->token, op->token, FFEINFO_basictypeINTEGER,
					  FFEINFO_kindtypeINTEGERDEFAULT, 0,
						 FFETARGET_charactersizeNONE,
						 FFEEXPR_contextLET));
    }

  return reduced;
}

/* ffeexpr_reduced_ugly1log_ -- Deal with TYPELESS and HOLLERITH

   reduced = ffeexpr_reduced_ugly1log_(reduced,op,r);

   Sigh.  */

static ffebld
ffeexpr_reduced_ugly1log_ (ffebld reduced, ffeexprExpr_ op, ffeexprExpr_ r)
{
  ffeinfo rinfo;
  ffeinfoBasictype rbt;
  ffeinfoKindtype rkt;
  ffeinfoRank rrk;
  ffeinfoKind rkd;
  ffeinfoWhere rwh;

  rinfo = ffebld_info (ffebld_left (reduced));
  rbt = ffeinfo_basictype (rinfo);
  rkt = ffeinfo_kindtype (rinfo);
  rrk = ffeinfo_rank (rinfo);
  rkd = ffeinfo_kind (rinfo);
  rwh = ffeinfo_where (rinfo);

  if ((rbt == FFEINFO_basictypeTYPELESS)
      || (rbt == FFEINFO_basictypeHOLLERITH))
    {
      ffebld_set_left (reduced, ffeexpr_convert (ffebld_left (reduced),
			   r->token, op->token, FFEINFO_basictypeLOGICAL, 0,
					     FFEINFO_kindtypeLOGICALDEFAULT,
						 FFETARGET_charactersizeNONE,
						 FFEEXPR_contextLET));
      rinfo = ffebld_info (ffebld_left (reduced));
      rbt = FFEINFO_basictypeLOGICAL;
      rkt = FFEINFO_kindtypeLOGICALDEFAULT;
      rrk = 0;
      rkd = FFEINFO_kindENTITY;
      rwh = ffeinfo_where (rinfo);
    }

  return reduced;
}

/* ffeexpr_reduced_ugly2_ -- Deal with TYPELESS, HOLLERITH, and LOGICAL

   reduced = ffeexpr_reduced_ugly2_(reduced,l,op,r);

   Sigh.  */

static ffebld
ffeexpr_reduced_ugly2_ (ffebld reduced, ffeexprExpr_ l, ffeexprExpr_ op,
			ffeexprExpr_ r)
{
  ffeinfo linfo, rinfo;
  ffeinfoBasictype lbt, rbt;
  ffeinfoKindtype lkt, rkt;
  ffeinfoRank lrk, rrk;
  ffeinfoKind lkd, rkd;
  ffeinfoWhere lwh, rwh;

  linfo = ffebld_info (ffebld_left (reduced));
  lbt = ffeinfo_basictype (linfo);
  lkt = ffeinfo_kindtype (linfo);
  lrk = ffeinfo_rank (linfo);
  lkd = ffeinfo_kind (linfo);
  lwh = ffeinfo_where (linfo);

  rinfo = ffebld_info (ffebld_right (reduced));
  rbt = ffeinfo_basictype (rinfo);
  rkt = ffeinfo_kindtype (rinfo);
  rrk = ffeinfo_rank (rinfo);
  rkd = ffeinfo_kind (rinfo);
  rwh = ffeinfo_where (rinfo);

  if ((lbt == FFEINFO_basictypeTYPELESS)
      || (lbt == FFEINFO_basictypeHOLLERITH))
    {
      if ((rbt == FFEINFO_basictypeTYPELESS)
	  || (rbt == FFEINFO_basictypeHOLLERITH))
	{
	  ffebld_set_left (reduced, ffeexpr_convert (ffebld_left (reduced),
			      l->token, op->token, FFEINFO_basictypeINTEGER,
					  FFEINFO_kindtypeINTEGERDEFAULT, 0,
						FFETARGET_charactersizeNONE,
						     FFEEXPR_contextLET));
	  ffebld_set_right (reduced, ffeexpr_convert (ffebld_right (reduced),
			   r->token, op->token, FFEINFO_basictypeINTEGER, 0,
					     FFEINFO_kindtypeINTEGERDEFAULT,
						FFETARGET_charactersizeNONE,
						      FFEEXPR_contextLET));
	  linfo = ffebld_info (ffebld_left (reduced));
	  rinfo = ffebld_info (ffebld_right (reduced));
	  lbt = rbt = FFEINFO_basictypeINTEGER;
	  lkt = rkt = FFEINFO_kindtypeINTEGERDEFAULT;
	  lrk = rrk = 0;
	  lkd = rkd = FFEINFO_kindENTITY;
	  lwh = ffeinfo_where (linfo);
	  rwh = ffeinfo_where (rinfo);
	}
      else
	{
	  ffebld_set_left (reduced, ffeexpr_convert_expr (ffebld_left (reduced),
				 l->token, ffebld_right (reduced), r->token,
						       FFEEXPR_contextLET));
	  linfo = ffebld_info (ffebld_left (reduced));
	  lbt = ffeinfo_basictype (linfo);
	  lkt = ffeinfo_kindtype (linfo);
	  lrk = ffeinfo_rank (linfo);
	  lkd = ffeinfo_kind (linfo);
	  lwh = ffeinfo_where (linfo);
	}
    }
  else
    {
      if ((rbt == FFEINFO_basictypeTYPELESS)
	  || (rbt == FFEINFO_basictypeHOLLERITH))
	{
	  ffebld_set_right (reduced, ffeexpr_convert_expr (ffebld_right (reduced),
				  r->token, ffebld_left (reduced), l->token,
						       FFEEXPR_contextLET));
	  rinfo = ffebld_info (ffebld_right (reduced));
	  rbt = ffeinfo_basictype (rinfo);
	  rkt = ffeinfo_kindtype (rinfo);
	  rrk = ffeinfo_rank (rinfo);
	  rkd = ffeinfo_kind (rinfo);
	  rwh = ffeinfo_where (rinfo);
	}
      /* else Leave it alone. */
    }

  if (lbt == FFEINFO_basictypeLOGICAL)
    {
      ffebld_set_left (reduced, ffeexpr_convert (ffebld_left (reduced),
			      l->token, op->token, FFEINFO_basictypeINTEGER,
					  FFEINFO_kindtypeINTEGERDEFAULT, 0,
						 FFETARGET_charactersizeNONE,
						 FFEEXPR_contextLET));
    }

  if (rbt == FFEINFO_basictypeLOGICAL)
    {
      ffebld_set_right (reduced, ffeexpr_convert (ffebld_right (reduced),
			      r->token, op->token, FFEINFO_basictypeINTEGER,
					  FFEINFO_kindtypeINTEGERDEFAULT, 0,
						FFETARGET_charactersizeNONE,
						  FFEEXPR_contextLET));
    }

  return reduced;
}

/* ffeexpr_reduced_ugly2log_ -- Deal with TYPELESS and HOLLERITH

   reduced = ffeexpr_reduced_ugly2log_(reduced,l,op,r);

   Sigh.  */

static ffebld
ffeexpr_reduced_ugly2log_ (ffebld reduced, ffeexprExpr_ l, ffeexprExpr_ op,
			   ffeexprExpr_ r)
{
  ffeinfo linfo, rinfo;
  ffeinfoBasictype lbt, rbt;
  ffeinfoKindtype lkt, rkt;
  ffeinfoRank lrk, rrk;
  ffeinfoKind lkd, rkd;
  ffeinfoWhere lwh, rwh;

  linfo = ffebld_info (ffebld_left (reduced));
  lbt = ffeinfo_basictype (linfo);
  lkt = ffeinfo_kindtype (linfo);
  lrk = ffeinfo_rank (linfo);
  lkd = ffeinfo_kind (linfo);
  lwh = ffeinfo_where (linfo);

  rinfo = ffebld_info (ffebld_right (reduced));
  rbt = ffeinfo_basictype (rinfo);
  rkt = ffeinfo_kindtype (rinfo);
  rrk = ffeinfo_rank (rinfo);
  rkd = ffeinfo_kind (rinfo);
  rwh = ffeinfo_where (rinfo);

  if ((lbt == FFEINFO_basictypeTYPELESS)
      || (lbt == FFEINFO_basictypeHOLLERITH))
    {
      if ((rbt == FFEINFO_basictypeTYPELESS)
	  || (rbt == FFEINFO_basictypeHOLLERITH))
	{
	  ffebld_set_left (reduced, ffeexpr_convert (ffebld_left (reduced),
			      l->token, op->token, FFEINFO_basictypeLOGICAL,
					  FFEINFO_kindtypeLOGICALDEFAULT, 0,
						FFETARGET_charactersizeNONE,
						     FFEEXPR_contextLET));
	  ffebld_set_right (reduced, ffeexpr_convert (ffebld_right (reduced),
			      r->token, op->token, FFEINFO_basictypeLOGICAL,
					  FFEINFO_kindtypeLOGICALDEFAULT, 0,
						FFETARGET_charactersizeNONE,
						      FFEEXPR_contextLET));
	  linfo = ffebld_info (ffebld_left (reduced));
	  rinfo = ffebld_info (ffebld_right (reduced));
	  lbt = rbt = FFEINFO_basictypeLOGICAL;
	  lkt = rkt = FFEINFO_kindtypeLOGICALDEFAULT;
	  lrk = rrk = 0;
	  lkd = rkd = FFEINFO_kindENTITY;
	  lwh = ffeinfo_where (linfo);
	  rwh = ffeinfo_where (rinfo);
	}
      else
	{
	  ffebld_set_left (reduced, ffeexpr_convert_expr (ffebld_left (reduced),
				 l->token, ffebld_right (reduced), r->token,
						       FFEEXPR_contextLET));
	  linfo = ffebld_info (ffebld_left (reduced));
	  lbt = ffeinfo_basictype (linfo);
	  lkt = ffeinfo_kindtype (linfo);
	  lrk = ffeinfo_rank (linfo);
	  lkd = ffeinfo_kind (linfo);
	  lwh = ffeinfo_where (linfo);
	}
    }
  else
    {
      if ((rbt == FFEINFO_basictypeTYPELESS)
	  || (rbt == FFEINFO_basictypeHOLLERITH))
	{
	  ffebld_set_right (reduced, ffeexpr_convert_expr (ffebld_right (reduced),
				  r->token, ffebld_left (reduced), l->token,
						       FFEEXPR_contextLET));
	  rinfo = ffebld_info (ffebld_right (reduced));
	  rbt = ffeinfo_basictype (rinfo);
	  rkt = ffeinfo_kindtype (rinfo);
	  rrk = ffeinfo_rank (rinfo);
	  rkd = ffeinfo_kind (rinfo);
	  rwh = ffeinfo_where (rinfo);
	}
      /* else Leave it alone. */
    }

  if (lbt == FFEINFO_basictypeLOGICAL)
  {
	  ffebld_set_left (reduced, ffeexpr_convert (ffebld_left (reduced),
				  l->token, op->token, FFEINFO_basictypeINTEGER,
				  FFEINFO_kindtypeINTEGERDEFAULT, 0,
				  FFETARGET_charactersizeNONE,
				  FFEEXPR_contextLET));
  }

  if (rbt == FFEINFO_basictypeLOGICAL)
  {
	  ffebld_set_right (reduced, ffeexpr_convert (ffebld_right (reduced),
				  r->token, op->token, FFEINFO_basictypeINTEGER,
				  FFEINFO_kindtypeINTEGERDEFAULT, 0,
				  FFETARGET_charactersizeNONE,
				  FFEEXPR_contextLET));
  }
  
  return reduced;
}

/* Fumble through tokens until a nonmatching CLOSE_PAREN, EOS, or SEMICOLON
   is found.

   The idea is to process the tokens as they would be done by normal
   expression processing, with the key things being telling the lexer
   when hollerith/character constants are about to happen, until the
   true closing token is found.  */

static ffelexHandler
ffeexpr_find_close_paren_ (ffelexToken t,
			   ffelexHandler after)
{
  ffeexpr_find_.after = after;
  ffeexpr_find_.level = 1;
  return (ffelexHandler) ffeexpr_nil_rhs_ (t);
}

static ffelexHandler
ffeexpr_nil_finished_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCLOSE_PAREN:
      if (--ffeexpr_find_.level == 0)
	return (ffelexHandler) ffeexpr_find_.after;
      return (ffelexHandler) ffeexpr_nil_binary_;

    case FFELEX_typeCOMMA:
    case FFELEX_typeCOLON:
    case FFELEX_typeEQUALS:
    case FFELEX_typePOINTS:
      return (ffelexHandler) ffeexpr_nil_rhs_;

    default:
      if (--ffeexpr_find_.level == 0)
	return (ffelexHandler) ffeexpr_find_.after (t);
      return (ffelexHandler) ffeexpr_nil_rhs_ (t);
    }
}

static ffelexHandler
ffeexpr_nil_rhs_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeQUOTE:
      if (ffe_is_vxt ())
	return (ffelexHandler) ffeexpr_nil_quote_;
      ffelex_set_expecting_hollerith (-1, '\"',
				      ffelex_token_where_line (t),
				      ffelex_token_where_column (t));
      return (ffelexHandler) ffeexpr_nil_apostrophe_;

    case FFELEX_typeAPOSTROPHE:
      ffelex_set_expecting_hollerith (-1, '\'',
				      ffelex_token_where_line (t),
				      ffelex_token_where_column (t));
      return (ffelexHandler) ffeexpr_nil_apostrophe_;

    case FFELEX_typePERCENT:
      return (ffelexHandler) ffeexpr_nil_percent_;

    case FFELEX_typeOPEN_PAREN:
      ++ffeexpr_find_.level;
      return (ffelexHandler) ffeexpr_nil_rhs_;

    case FFELEX_typePLUS:
    case FFELEX_typeMINUS:
      return (ffelexHandler) ffeexpr_nil_rhs_;

    case FFELEX_typePERIOD:
      return (ffelexHandler) ffeexpr_nil_period_;

    case FFELEX_typeNUMBER:
      ffeexpr_hollerith_count_ = atol (ffelex_token_text (t));
      if (ffeexpr_hollerith_count_ > 0)
	ffelex_set_expecting_hollerith (ffeexpr_hollerith_count_,
					'\0',
					ffelex_token_where_line (t),
					ffelex_token_where_column (t));
      return (ffelexHandler) ffeexpr_nil_number_;

    case FFELEX_typeNAME:
    case FFELEX_typeNAMES:
      return (ffelexHandler) ffeexpr_nil_name_rhs_;

    case FFELEX_typeASTERISK:
    case FFELEX_typeSLASH:
    case FFELEX_typePOWER:
    case FFELEX_typeCONCAT:
    case FFELEX_typeREL_EQ:
    case FFELEX_typeREL_NE:
    case FFELEX_typeREL_LE:
    case FFELEX_typeREL_GE:
      return (ffelexHandler) ffeexpr_nil_rhs_;

    default:
      return (ffelexHandler) ffeexpr_nil_finished_ (t);
    }
}

static ffelexHandler
ffeexpr_nil_period_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
    case FFELEX_typeNAMES:
      ffeexpr_current_dotdot_ = ffestr_other (t);
      switch (ffeexpr_current_dotdot_)
	{
	case FFESTR_otherNone:
	  return (ffelexHandler) ffeexpr_nil_rhs_ (t);

	case FFESTR_otherTRUE:
	case FFESTR_otherFALSE:
	case FFESTR_otherNOT:
	  return (ffelexHandler) ffeexpr_nil_end_period_;

	default:
	  return (ffelexHandler) ffeexpr_nil_swallow_period_;
	}
      break;			/* Nothing really reaches here. */

    case FFELEX_typeNUMBER:
      return (ffelexHandler) ffeexpr_nil_real_;

    default:
      return (ffelexHandler) ffeexpr_nil_rhs_ (t);
    }
}

static ffelexHandler
ffeexpr_nil_end_period_ (ffelexToken t)
{
  switch (ffeexpr_current_dotdot_)
    {
    case FFESTR_otherNOT:
      if (ffelex_token_type (t) != FFELEX_typePERIOD)
	return (ffelexHandler) ffeexpr_nil_rhs_ (t);
      return (ffelexHandler) ffeexpr_nil_rhs_;

    case FFESTR_otherTRUE:
    case FFESTR_otherFALSE:
      if (ffelex_token_type (t) != FFELEX_typePERIOD)
	return (ffelexHandler) ffeexpr_nil_binary_ (t);
      return (ffelexHandler) ffeexpr_nil_binary_;

    default:
      assert ("Bad [nil] unary dotdot in ffeexpr_current_dotdot_" == NULL);
      exit (0);
      return NULL;
    }
}

static ffelexHandler
ffeexpr_nil_swallow_period_ (ffelexToken t)
{
  if (ffelex_token_type (t) != FFELEX_typePERIOD)
    return (ffelexHandler) ffeexpr_nil_rhs_ (t);
  return (ffelexHandler) ffeexpr_nil_rhs_;
}

static ffelexHandler
ffeexpr_nil_real_ (ffelexToken t)
{
  char d;
  const char *p;

  if (((ffelex_token_type (t) != FFELEX_typeNAME)
       && (ffelex_token_type (t) != FFELEX_typeNAMES))
      || !(((ffesrc_char_match_init ((d = *(p = ffelex_token_text (t))),
				     'D', 'd')
	     || ffesrc_char_match_init (d, 'E', 'e')
	     || ffesrc_char_match_init (d, 'Q', 'q')))
	   && ffeexpr_isdigits_ (++p)))
    return (ffelexHandler) ffeexpr_nil_binary_ (t);

  if (*p == '\0')
    return (ffelexHandler) ffeexpr_nil_real_exponent_;
  return (ffelexHandler) ffeexpr_nil_binary_;
}

static ffelexHandler
ffeexpr_nil_real_exponent_ (ffelexToken t)
{
  if ((ffelex_token_type (t) != FFELEX_typePLUS)
      && (ffelex_token_type (t) != FFELEX_typeMINUS))
    return (ffelexHandler) ffeexpr_nil_binary_ (t);

  return (ffelexHandler) ffeexpr_nil_real_exp_sign_;
}

static ffelexHandler
ffeexpr_nil_real_exp_sign_ (ffelexToken t)
{
  if (ffelex_token_type (t) != FFELEX_typeNUMBER)
    return (ffelexHandler) ffeexpr_nil_binary_ (t);
  return (ffelexHandler) ffeexpr_nil_binary_;
}

static ffelexHandler
ffeexpr_nil_number_ (ffelexToken t)
{
  char d;
  const char *p;

  if (ffeexpr_hollerith_count_ > 0)
    ffelex_set_expecting_hollerith (0, '\0',
				    ffewhere_line_unknown (),
				    ffewhere_column_unknown ());

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
    case FFELEX_typeNAMES:
      if ((ffesrc_char_match_init ((d = *(p = ffelex_token_text (t))),
				   'D', 'd')
	   || ffesrc_char_match_init (d, 'E', 'e')
	   || ffesrc_char_match_init (d, 'Q', 'q'))
	  && ffeexpr_isdigits_ (++p))
	{
	  if (*p == '\0')
	    {
	      ffeexpr_find_.t = ffelex_token_use (t);
	      return (ffelexHandler) ffeexpr_nil_number_exponent_;
	    }
	  return (ffelexHandler) ffeexpr_nil_binary_;
	}
      break;

    case FFELEX_typePERIOD:
      ffeexpr_find_.t = ffelex_token_use (t);
      return (ffelexHandler) ffeexpr_nil_number_period_;

    case FFELEX_typeHOLLERITH:
      return (ffelexHandler) ffeexpr_nil_binary_;

    default:
      break;
    }
  return (ffelexHandler) ffeexpr_nil_binary_ (t);
}

/* Expects ffeexpr_find_.t.  */

static ffelexHandler
ffeexpr_nil_number_exponent_ (ffelexToken t)
{
  ffelexHandler nexthandler;

  if ((ffelex_token_type (t) != FFELEX_typePLUS)
      && (ffelex_token_type (t) != FFELEX_typeMINUS))
    {
      nexthandler
	= (ffelexHandler) ffeexpr_nil_binary_ (ffeexpr_find_.t);
      ffelex_token_kill (ffeexpr_find_.t);
      return (ffelexHandler) (*nexthandler) (t);
    }

  ffelex_token_kill (ffeexpr_find_.t);
  return (ffelexHandler) ffeexpr_nil_number_exp_sign_;
}

static ffelexHandler
ffeexpr_nil_number_exp_sign_ (ffelexToken t)
{
  if (ffelex_token_type (t) != FFELEX_typeNUMBER)
    return (ffelexHandler) ffeexpr_nil_binary_ (t);

  return (ffelexHandler) ffeexpr_nil_binary_;
}

/* Expects ffeexpr_find_.t.  */

static ffelexHandler
ffeexpr_nil_number_period_ (ffelexToken t)
{
  ffelexHandler nexthandler;
  char d;
  const char *p;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
    case FFELEX_typeNAMES:
      if ((ffesrc_char_match_init ((d = *(p = ffelex_token_text (t))),
				   'D', 'd')
	   || ffesrc_char_match_init (d, 'E', 'e')
	   || ffesrc_char_match_init (d, 'Q', 'q'))
	  && ffeexpr_isdigits_ (++p))
	{
	  if (*p == '\0')
	    return (ffelexHandler) ffeexpr_nil_number_per_exp_;
	  ffelex_token_kill (ffeexpr_find_.t);
	  return (ffelexHandler) ffeexpr_nil_binary_;
	}
      nexthandler
	= (ffelexHandler) ffeexpr_nil_binary_ (ffeexpr_find_.t);
      ffelex_token_kill (ffeexpr_find_.t);
      return (ffelexHandler) (*nexthandler) (t);

    case FFELEX_typeNUMBER:
      ffelex_token_kill (ffeexpr_find_.t);
      return (ffelexHandler) ffeexpr_nil_number_real_;

    default:
      break;
    }
  ffelex_token_kill (ffeexpr_find_.t);
  return (ffelexHandler) ffeexpr_nil_binary_ (t);
}

/* Expects ffeexpr_find_.t.  */

static ffelexHandler
ffeexpr_nil_number_per_exp_ (ffelexToken t)
{
  if ((ffelex_token_type (t) != FFELEX_typePLUS)
      && (ffelex_token_type (t) != FFELEX_typeMINUS))
    {
      ffelexHandler nexthandler;

      nexthandler
	= (ffelexHandler) ffeexpr_nil_binary_ (ffeexpr_find_.t);
      ffelex_token_kill (ffeexpr_find_.t);
      return (ffelexHandler) (*nexthandler) (t);
    }

  ffelex_token_kill (ffeexpr_find_.t);
  return (ffelexHandler) ffeexpr_nil_num_per_exp_sign_;
}

static ffelexHandler
ffeexpr_nil_number_real_ (ffelexToken t)
{
  char d;
  const char *p;

  if (((ffelex_token_type (t) != FFELEX_typeNAME)
       && (ffelex_token_type (t) != FFELEX_typeNAMES))
      || !(((ffesrc_char_match_init ((d = *(p = ffelex_token_text (t))),
				     'D', 'd')
	     || ffesrc_char_match_init (d, 'E', 'e')
	     || ffesrc_char_match_init (d, 'Q', 'q')))
	   && ffeexpr_isdigits_ (++p)))
    return (ffelexHandler) ffeexpr_nil_binary_ (t);

  if (*p == '\0')
    return (ffelexHandler) ffeexpr_nil_number_real_exp_;

  return (ffelexHandler) ffeexpr_nil_binary_;
}

static ffelexHandler
ffeexpr_nil_num_per_exp_sign_ (ffelexToken t)
{
  if (ffelex_token_type (t) != FFELEX_typeNUMBER)
    return (ffelexHandler) ffeexpr_nil_binary_ (t);
  return (ffelexHandler) ffeexpr_nil_binary_;
}

static ffelexHandler
ffeexpr_nil_number_real_exp_ (ffelexToken t)
{
  if ((ffelex_token_type (t) != FFELEX_typePLUS)
      && (ffelex_token_type (t) != FFELEX_typeMINUS))
    return (ffelexHandler) ffeexpr_nil_binary_ (t);
  return (ffelexHandler) ffeexpr_nil_num_real_exp_sn_;
}

static ffelexHandler
ffeexpr_nil_num_real_exp_sn_ (ffelexToken t)
{
  if (ffelex_token_type (t) != FFELEX_typeNUMBER)
    return (ffelexHandler) ffeexpr_nil_binary_ (t);
  return (ffelexHandler) ffeexpr_nil_binary_;
}

static ffelexHandler
ffeexpr_nil_binary_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typePLUS:
    case FFELEX_typeMINUS:
    case FFELEX_typeASTERISK:
    case FFELEX_typeSLASH:
    case FFELEX_typePOWER:
    case FFELEX_typeCONCAT:
    case FFELEX_typeOPEN_ANGLE:
    case FFELEX_typeCLOSE_ANGLE:
    case FFELEX_typeREL_EQ:
    case FFELEX_typeREL_NE:
    case FFELEX_typeREL_GE:
    case FFELEX_typeREL_LE:
      return (ffelexHandler) ffeexpr_nil_rhs_;

    case FFELEX_typePERIOD:
      return (ffelexHandler) ffeexpr_nil_binary_period_;

    default:
      return (ffelexHandler) ffeexpr_nil_finished_ (t);
    }
}

static ffelexHandler
ffeexpr_nil_binary_period_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
    case FFELEX_typeNAMES:
      ffeexpr_current_dotdot_ = ffestr_other (t);
      switch (ffeexpr_current_dotdot_)
	{
	case FFESTR_otherTRUE:
	case FFESTR_otherFALSE:
	case FFESTR_otherNOT:
	  return (ffelexHandler) ffeexpr_nil_binary_sw_per_;

	default:
	  return (ffelexHandler) ffeexpr_nil_binary_end_per_;
	}
      break;			/* Nothing really reaches here. */

    default:
      return (ffelexHandler) ffeexpr_nil_binary_ (t);
    }
}

static ffelexHandler
ffeexpr_nil_binary_end_per_ (ffelexToken t)
{
  if (ffelex_token_type (t) != FFELEX_typePERIOD)
    return (ffelexHandler) ffeexpr_nil_rhs_ (t);
  return (ffelexHandler) ffeexpr_nil_rhs_;
}

static ffelexHandler
ffeexpr_nil_binary_sw_per_ (ffelexToken t)
{
  if (ffelex_token_type (t) != FFELEX_typePERIOD)
    return (ffelexHandler) ffeexpr_nil_binary_ (t);
  return (ffelexHandler) ffeexpr_nil_binary_;
}

static ffelexHandler
ffeexpr_nil_quote_ (ffelexToken t)
{
  if (ffelex_token_type (t) != FFELEX_typeNUMBER)
    return (ffelexHandler) ffeexpr_nil_rhs_ (t);
  return (ffelexHandler) ffeexpr_nil_binary_;
}

static ffelexHandler
ffeexpr_nil_apostrophe_ (ffelexToken t)
{
  assert (ffelex_token_type (t) == FFELEX_typeCHARACTER);
  return (ffelexHandler) ffeexpr_nil_apos_char_;
}

static ffelexHandler
ffeexpr_nil_apos_char_ (ffelexToken t)
{
  char c;

  if ((ffelex_token_type (t) == FFELEX_typeNAME)
      || (ffelex_token_type (t) == FFELEX_typeNAMES))
    {
      if ((ffelex_token_length (t) == 1)
	  && (ffesrc_char_match_init ((c = ffelex_token_text (t)[0]),
				      'B', 'b')
	      || ffesrc_char_match_init (c, 'O', 'o')
	      || ffesrc_char_match_init (c, 'X', 'x')
	      || ffesrc_char_match_init (c, 'Z', 'z')))
	return (ffelexHandler) ffeexpr_nil_binary_;
    }
  if ((ffelex_token_type (t) == FFELEX_typeNAME)
      || (ffelex_token_type (t) == FFELEX_typeNAMES))
    return (ffelexHandler) ffeexpr_nil_rhs_ (t);
  return (ffelexHandler) ffeexpr_nil_substrp_ (t);
}

static ffelexHandler
ffeexpr_nil_name_rhs_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeQUOTE:
    case FFELEX_typeAPOSTROPHE:
      ffelex_set_hexnum (TRUE);
      return (ffelexHandler) ffeexpr_nil_name_apos_;

    case FFELEX_typeOPEN_PAREN:
      ++ffeexpr_find_.level;
      return (ffelexHandler) ffeexpr_nil_rhs_;

    default:
      return (ffelexHandler) ffeexpr_nil_binary_ (t);
    }
}

static ffelexHandler
ffeexpr_nil_name_apos_ (ffelexToken t)
{
  if (ffelex_token_type (t) == FFELEX_typeNAME)
    return (ffelexHandler) ffeexpr_nil_name_apos_name_;
  return (ffelexHandler) ffeexpr_nil_binary_ (t);
}

static ffelexHandler
ffeexpr_nil_name_apos_name_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeAPOSTROPHE:
    case FFELEX_typeQUOTE:
      return (ffelexHandler) ffeexpr_nil_finished_;

    default:
      return (ffelexHandler) ffeexpr_nil_finished_ (t);
    }
}

static ffelexHandler
ffeexpr_nil_percent_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
    case FFELEX_typeNAMES:
      ffeexpr_stack_->percent = ffeexpr_percent_ (t);
      ffeexpr_find_.t = ffelex_token_use (t);
      return (ffelexHandler) ffeexpr_nil_percent_name_;

    default:
      return (ffelexHandler) ffeexpr_nil_rhs_ (t);
    }
}

/* Expects ffeexpr_find_.t.  */

static ffelexHandler
ffeexpr_nil_percent_name_ (ffelexToken t)
{
  ffelexHandler nexthandler;

  if (ffelex_token_type (t) != FFELEX_typeOPEN_PAREN)
    {
      nexthandler
	= (ffelexHandler) ffeexpr_nil_rhs_ (ffeexpr_find_.t);
      ffelex_token_kill (ffeexpr_find_.t);
      return (ffelexHandler) (*nexthandler) (t);
    }

  ffelex_token_kill (ffeexpr_find_.t);
  ++ffeexpr_find_.level;
  return (ffelexHandler) ffeexpr_nil_rhs_;
}

static ffelexHandler
ffeexpr_nil_substrp_ (ffelexToken t)
{
  if (ffelex_token_type (t) != FFELEX_typeOPEN_PAREN)
    return (ffelexHandler) ffeexpr_nil_binary_ (t);

  ++ffeexpr_find_.level;
  return (ffelexHandler) ffeexpr_nil_rhs_;
}

/* ffeexpr_finished_ -- Reduce expression stack to one expr, finish

   ffelexToken t;
   return ffeexpr_finished_(t);

   Reduces expression stack to one (or zero) elements by repeatedly reducing
   the top operator on the stack (or, if the top element on the stack is
   itself an operator, issuing an error message and discarding it).  Calls
   finishing routine with the expression, returning the ffelexHandler it
   returns to the caller.  */

static ffelexHandler
ffeexpr_finished_ (ffelexToken t)
{
  ffeexprExpr_ operand;		/* This is B in -B or A+B. */
  ffebld expr;
  ffeexprCallback callback;
  ffeexprStack_ s;
  ffebldConstant constnode;	/* For detecting magical number. */
  ffelexToken ft;		/* Temporary copy of first token in
				   expression. */
  ffelexHandler next;
  ffeinfo info;
  bool error = FALSE;

  while (((operand = ffeexpr_stack_->exprstack) != NULL)
	 && ((operand->previous != NULL) || (operand->type != FFEEXPR_exprtypeOPERAND_)))
    {
      if (operand->type == FFEEXPR_exprtypeOPERAND_)
	ffeexpr_reduce_ ();
      else
	{
	  if (ffest_ffebad_start (FFEBAD_MISSING_OPERAND_FOR_OPERATOR))
	    {
	      ffebad_here (0, ffelex_token_where_line (t),
			   ffelex_token_where_column (t));
	      ffebad_here (1, ffelex_token_where_line (ffeexpr_stack_->exprstack->token),
	      ffelex_token_where_column (ffeexpr_stack_->exprstack->token));
	      ffebad_finish ();
	    }
	  ffeexpr_stack_->exprstack = operand->previous;	/* Pop the useless
								   operator. */
	  ffeexpr_expr_kill_ (operand);
	}
    }

  assert ((operand == NULL) || (operand->previous == NULL));

  ffebld_pool_pop ();
  if (operand == NULL)
    expr = NULL;
  else
    {
      expr = operand->u.operand;
      info = ffebld_info (expr);
      if ((ffebld_op (expr) == FFEBLD_opCONTER)
	  && (ffebld_conter_orig (expr) == NULL)
	  && ffebld_constant_is_magical (constnode = ffebld_conter (expr)))
	{
	  ffetarget_integer_bad_magical (operand->token);
	}
      ffeexpr_expr_kill_ (operand);
      ffeexpr_stack_->exprstack = NULL;
    }

  ft = ffeexpr_stack_->first_token;

again:				/* :::::::::::::::::::: */
  switch (ffeexpr_stack_->context)
    {
    case FFEEXPR_contextLET:
    case FFEEXPR_contextSFUNCDEF:
      error = (expr == NULL)
	|| (ffeinfo_rank (info) != 0);
      break;

    case FFEEXPR_contextPAREN_:
      if ((error = (expr == NULL) || (ffeinfo_rank (info) != 0)))
	break;
      switch (ffeinfo_basictype (info))
	{
	case FFEINFO_basictypeHOLLERITH:
	case FFEINFO_basictypeTYPELESS:
	  expr = ffeexpr_convert (expr, ft, ft, FFEINFO_basictypeINTEGER,
	     FFEINFO_kindtypeINTEGERDEFAULT, 0, FFETARGET_charactersizeNONE,
				  FFEEXPR_contextLET);
	  break;

	default:
	  break;
	}
      break;

    case FFEEXPR_contextPARENFILENUM_:
      if (ffelex_token_type (t) != FFELEX_typeCOMMA)
	ffeexpr_stack_->context = FFEEXPR_contextPAREN_;
      else
	ffeexpr_stack_->context = FFEEXPR_contextFILENUM;
      goto again;		/* :::::::::::::::::::: */

    case FFEEXPR_contextPARENFILEUNIT_:
      if (ffelex_token_type (t) != FFELEX_typeCOMMA)
	ffeexpr_stack_->context = FFEEXPR_contextPAREN_;
      else
	ffeexpr_stack_->context = FFEEXPR_contextFILEUNIT;
      goto again;		/* :::::::::::::::::::: */

    case FFEEXPR_contextACTUALARGEXPR_:
    case FFEEXPR_contextSFUNCDEFACTUALARGEXPR_:
      switch ((expr == NULL) ? FFEINFO_basictypeNONE
	      : ffeinfo_basictype (info))
	{
	case FFEINFO_basictypeHOLLERITH:
	case FFEINFO_basictypeTYPELESS:
	  if (!ffe_is_ugly_args ()
	      && ffebad_start (FFEBAD_ACTUALARG))
	    {
	      ffebad_here (0, ffelex_token_where_line (ft),
			   ffelex_token_where_column (ft));
	      ffebad_finish ();
	    }
	  break;

	default:
	  break;
	}
      error = (expr != NULL) && (ffeinfo_rank (info) != 0);
      break;

    case FFEEXPR_contextACTUALARG_:
    case FFEEXPR_contextSFUNCDEFACTUALARG_:
      switch ((expr == NULL) ? FFEINFO_basictypeNONE
	      : ffeinfo_basictype (info))
	{
	case FFEINFO_basictypeHOLLERITH:
	case FFEINFO_basictypeTYPELESS:
#if 0				/* Should never get here. */
	  expr = ffeexpr_convert (expr, ft, ft,
				  FFEINFO_basictypeINTEGER,
				  FFEINFO_kindtypeINTEGERDEFAULT,
				  0,
				  FFETARGET_charactersizeNONE,
				  FFEEXPR_contextLET);
#else
	  assert ("why hollerith/typeless in actualarg_?" == NULL);
#endif
	  break;

	default:
	  break;
	}
      switch ((expr == NULL) ? FFEBLD_opANY : ffebld_op (expr))
	{
	case FFEBLD_opSYMTER:
	case FFEBLD_opPERCENT_LOC:
	case FFEBLD_opPERCENT_VAL:
	case FFEBLD_opPERCENT_REF:
	case FFEBLD_opPERCENT_DESCR:
	  error = FALSE;
	  break;

	default:
	  error = (expr != NULL) && (ffeinfo_rank (info) != 0);
	  break;
	}
      {
	ffesymbol s;
	ffeinfoWhere where;
	ffeinfoKind kind;

	if (!error
	    && (expr != NULL)
	    && (ffebld_op (expr) == FFEBLD_opSYMTER)
	    && ((s = ffebld_symter (expr)), (where = ffesymbol_where (s)),
		(where == FFEINFO_whereINTRINSIC)
		|| (where == FFEINFO_whereGLOBAL)
		|| ((where == FFEINFO_whereDUMMY)
		    && ((kind = ffesymbol_kind (s)),
			(kind == FFEINFO_kindFUNCTION)
			|| (kind == FFEINFO_kindSUBROUTINE))))
	    && !ffesymbol_explicitwhere (s))
	  {
	    ffebad_start (where == FFEINFO_whereINTRINSIC
			  ? FFEBAD_NEED_INTRINSIC : FFEBAD_NEED_EXTERNAL);
	    ffebad_here (0, ffelex_token_where_line (ft),
			 ffelex_token_where_column (ft));
	    ffebad_string (ffesymbol_text (s));
	    ffebad_finish ();
	    ffesymbol_signal_change (s);
	    ffesymbol_set_explicitwhere (s, TRUE);
	    ffesymbol_signal_unreported (s);
	  }
      }
      break;

    case FFEEXPR_contextINDEX_:
    case FFEEXPR_contextSFUNCDEFINDEX_:
      if ((error = (expr != NULL) && (ffeinfo_rank (info) != 0)))
	break;
      switch ((expr == NULL) ? FFEINFO_basictypeNONE
	      : ffeinfo_basictype (info))
	{
	case FFEINFO_basictypeNONE:
	  error = FALSE;
	  break;

	case FFEINFO_basictypeLOGICAL:
	  expr = ffeexpr_convert (expr, ft, ft, FFEINFO_basictypeLOGICAL,
	     FFEINFO_kindtypeLOGICALDEFAULT, 0, FFETARGET_charactersizeNONE,
				  FFEEXPR_contextLET);
	  /* Fall through. */
	case FFEINFO_basictypeREAL:
	case FFEINFO_basictypeCOMPLEX:
	  if (ffe_is_pedantic ())
	    {
	      error = TRUE;
	      break;
	    }
	  /* Fall through. */
	case FFEINFO_basictypeHOLLERITH:
	case FFEINFO_basictypeTYPELESS:
	  error = FALSE;
	  expr = ffeexpr_convert (expr, ft, ft, FFEINFO_basictypeINTEGER,
	     FFEINFO_kindtypeINTEGERDEFAULT, 0, FFETARGET_charactersizeNONE,
				  FFEEXPR_contextLET);
	  break;

	case FFEINFO_basictypeINTEGER:
	  /* Specifically, allow INTEGER(KIND=2), aka INTEGER*8, through
	     unmolested.  Leave it to downstream to handle kinds.  */
	  break;

	default:
	  error = TRUE;
	  break;
	}
      break;			/* expr==NULL ok for substring; element case
				   caught by callback. */

    case FFEEXPR_contextRETURN:
      if ((error = (expr != NULL) && (ffeinfo_rank (info) != 0)))
	break;
      switch ((expr == NULL) ? FFEINFO_basictypeNONE
	      : ffeinfo_basictype (info))
	{
	case FFEINFO_basictypeNONE:
	  error = FALSE;
	  break;

	case FFEINFO_basictypeLOGICAL:
	  expr = ffeexpr_convert (expr, ft, ft, FFEINFO_basictypeLOGICAL,
	     FFEINFO_kindtypeLOGICALDEFAULT, 0, FFETARGET_charactersizeNONE,
				  FFEEXPR_contextLET);
	  /* Fall through. */
	case FFEINFO_basictypeREAL:
	case FFEINFO_basictypeCOMPLEX:
	  if (ffe_is_pedantic ())
	    {
	      error = TRUE;
	      break;
	    }
	  /* Fall through. */
	case FFEINFO_basictypeINTEGER:
	case FFEINFO_basictypeHOLLERITH:
	case FFEINFO_basictypeTYPELESS:
	  error = FALSE;
	  expr = ffeexpr_convert (expr, ft, ft, FFEINFO_basictypeINTEGER,
	     FFEINFO_kindtypeINTEGERDEFAULT, 0, FFETARGET_charactersizeNONE,
				  FFEEXPR_contextLET);
	  break;

	default:
	  error = TRUE;
	  break;
	}
      break;

    case FFEEXPR_contextDO:
      if ((error = (expr == NULL) || (ffeinfo_rank (info) != 0)))
	break;
      switch (ffeinfo_basictype (info))
	{
	case FFEINFO_basictypeLOGICAL:
	  error = !ffe_is_ugly_logint ();
	  if (!ffeexpr_stack_->is_rhs)
	    break;		/* Don't convert lhs variable. */
	  expr = ffeexpr_convert (expr, ft, ft, FFEINFO_basictypeINTEGER,
				  ffeinfo_kindtype (ffebld_info (expr)), 0,
				  FFETARGET_charactersizeNONE,
				  FFEEXPR_contextLET);
	  break;

	case FFEINFO_basictypeHOLLERITH:
	case FFEINFO_basictypeTYPELESS:
	  if (!ffeexpr_stack_->is_rhs)
	    {
	      error = TRUE;
	      break;		/* Don't convert lhs variable. */
	    }
	  break;

	case FFEINFO_basictypeINTEGER:
	case FFEINFO_basictypeREAL:
	  break;

	default:
	  error = TRUE;
	  break;
	}
      if (!ffeexpr_stack_->is_rhs
	  && (ffebld_op (expr) != FFEBLD_opSYMTER))
	error = TRUE;
      break;

    case FFEEXPR_contextDOWHILE:
    case FFEEXPR_contextIF:
      if ((error = (expr == NULL) || (ffeinfo_rank (info) != 0)))
	break;
      switch (ffeinfo_basictype (info))
	{
	case FFEINFO_basictypeINTEGER:
	  error = FALSE;
	  expr = ffeexpr_convert (expr, ft, ft, FFEINFO_basictypeINTEGER,
	     FFEINFO_kindtypeINTEGERDEFAULT, 0, FFETARGET_charactersizeNONE,
				  FFEEXPR_contextLET);
	  /* Fall through. */
	case FFEINFO_basictypeLOGICAL:
	case FFEINFO_basictypeHOLLERITH:
	case FFEINFO_basictypeTYPELESS:
	  error = FALSE;
	  expr = ffeexpr_convert (expr, ft, ft, FFEINFO_basictypeLOGICAL,
	     FFEINFO_kindtypeLOGICALDEFAULT, 0, FFETARGET_charactersizeNONE,
				  FFEEXPR_contextLET);
	  break;

	default:
	  error = TRUE;
	  break;
	}
      break;

    case FFEEXPR_contextASSIGN:
    case FFEEXPR_contextAGOTO:
      switch ((expr == NULL) ? FFEINFO_basictypeNONE
	      : ffeinfo_basictype (info))
	{
	case FFEINFO_basictypeINTEGER:
	  error = (ffeinfo_kindtype (info) != ffecom_label_kind ());
	  break;

	case FFEINFO_basictypeLOGICAL:
	  error = !ffe_is_ugly_logint ()
	    || (ffeinfo_kindtype (info) != ffecom_label_kind ());
	  break;

	default:
	  error = TRUE;
	  break;
	}
      if ((expr == NULL) || (ffeinfo_rank (info) != 0)
	  || (ffebld_op (expr) != FFEBLD_opSYMTER))
	error = TRUE;
      break;

    case FFEEXPR_contextCGOTO:
    case FFEEXPR_contextFORMAT:
    case FFEEXPR_contextDIMLIST:
    case FFEEXPR_contextFILENUM:	/* See equiv code in _ambig_. */
      if ((error = (expr == NULL) || (ffeinfo_rank (info) != 0)))
	break;
      switch (ffeinfo_basictype (info))
	{
	case FFEINFO_basictypeLOGICAL:
	  expr = ffeexpr_convert (expr, ft, ft, FFEINFO_basictypeLOGICAL,
	     FFEINFO_kindtypeLOGICALDEFAULT, 0, FFETARGET_charactersizeNONE,
				  FFEEXPR_contextLET);
	  /* Fall through. */
	case FFEINFO_basictypeREAL:
	case FFEINFO_basictypeCOMPLEX:
	  if (ffe_is_pedantic ())
	    {
	      error = TRUE;
	      break;
	    }
	  /* Fall through. */
	case FFEINFO_basictypeINTEGER:
	case FFEINFO_basictypeHOLLERITH:
	case FFEINFO_basictypeTYPELESS:
	  error = FALSE;
	  expr = ffeexpr_convert (expr, ft, ft, FFEINFO_basictypeINTEGER,
	     FFEINFO_kindtypeINTEGERDEFAULT, 0, FFETARGET_charactersizeNONE,
				  FFEEXPR_contextLET);
	  break;

	default:
	  error = TRUE;
	  break;
	}
      break;

    case FFEEXPR_contextARITHIF:
      if ((error = (expr == NULL) || (ffeinfo_rank (info) != 0)))
	break;
      switch (ffeinfo_basictype (info))
	{
	case FFEINFO_basictypeLOGICAL:
	  expr = ffeexpr_convert (expr, ft, ft, FFEINFO_basictypeLOGICAL,
	     FFEINFO_kindtypeLOGICALDEFAULT, 0, FFETARGET_charactersizeNONE,
				  FFEEXPR_contextLET);
	  if (ffe_is_pedantic ())
	    {
	      error = TRUE;
	      break;
	    }
	  /* Fall through. */
	case FFEINFO_basictypeHOLLERITH:
	case FFEINFO_basictypeTYPELESS:
	  expr = ffeexpr_convert (expr, ft, ft, FFEINFO_basictypeINTEGER,
	     FFEINFO_kindtypeINTEGERDEFAULT, 0, FFETARGET_charactersizeNONE,
				  FFEEXPR_contextLET);
	  /* Fall through. */
	case FFEINFO_basictypeINTEGER:
	case FFEINFO_basictypeREAL:
	  error = FALSE;
	  break;

	default:
	  error = TRUE;
	  break;
	}
      break;

    case FFEEXPR_contextSTOP:
      if ((error = (expr != NULL) && (ffeinfo_rank (info) != 0)))
	break;
      switch ((expr == NULL) ? FFEINFO_basictypeNONE
	      : ffeinfo_basictype (info))
	{
	case FFEINFO_basictypeINTEGER:
	  error = (ffeinfo_kindtype (info) != FFEINFO_kindtypeINTEGERDEFAULT);
	  break;

	case FFEINFO_basictypeCHARACTER:
	  error = (ffeinfo_kindtype (info) != FFEINFO_kindtypeCHARACTERDEFAULT);
	  break;

	case FFEINFO_basictypeHOLLERITH:
	case FFEINFO_basictypeTYPELESS:
	  error = FALSE;
	  expr = ffeexpr_convert (expr, ft, ft, FFEINFO_basictypeINTEGER,
	     FFEINFO_kindtypeINTEGERDEFAULT, 0, FFETARGET_charactersizeNONE,
				  FFEEXPR_contextLET);
	  break;

	case FFEINFO_basictypeNONE:
	  error = FALSE;
	  break;

	default:
	  error = TRUE;
	  break;
	}
      if ((expr != NULL) && ((ffebld_op (expr) != FFEBLD_opCONTER)
			     || (ffebld_conter_orig (expr) != NULL)))
	error = TRUE;
      break;

    case FFEEXPR_contextINCLUDE:
      error = (expr == NULL) || (ffeinfo_rank (info) != 0)
	|| (ffeinfo_basictype (info) != FFEINFO_basictypeCHARACTER)
	|| (ffebld_op (expr) != FFEBLD_opCONTER)
	|| (ffebld_conter_orig (expr) != NULL);
      break;

    case FFEEXPR_contextSELECTCASE:
      if ((error = (expr == NULL) || (ffeinfo_rank (info) != 0)))
	break;
      switch (ffeinfo_basictype (info))
	{
	case FFEINFO_basictypeINTEGER:
	case FFEINFO_basictypeCHARACTER:
	case FFEINFO_basictypeLOGICAL:
	  error = FALSE;
	  break;

	case FFEINFO_basictypeHOLLERITH:
	case FFEINFO_basictypeTYPELESS:
	  error = FALSE;
	  expr = ffeexpr_convert (expr, ft, ft, FFEINFO_basictypeINTEGER,
	     FFEINFO_kindtypeINTEGERDEFAULT, 0, FFETARGET_charactersizeNONE,
				  FFEEXPR_contextLET);
	  break;

	default:
	  error = TRUE;
	  break;
	}
      break;

    case FFEEXPR_contextCASE:
      if ((error = (expr != NULL) && (ffeinfo_rank (info) != 0)))
	break;
      switch ((expr == NULL) ? FFEINFO_basictypeINTEGER
	      : ffeinfo_basictype (info))
	{
	case FFEINFO_basictypeINTEGER:
	case FFEINFO_basictypeCHARACTER:
	case FFEINFO_basictypeLOGICAL:
	  error = FALSE;
	  break;

	case FFEINFO_basictypeHOLLERITH:
	case FFEINFO_basictypeTYPELESS:
	  error = FALSE;
	  expr = ffeexpr_convert (expr, ft, ft, FFEINFO_basictypeINTEGER,
	     FFEINFO_kindtypeINTEGERDEFAULT, 0, FFETARGET_charactersizeNONE,
				  FFEEXPR_contextLET);
	  break;

	default:
	  error = TRUE;
	  break;
	}
      if ((expr != NULL) && (ffebld_op (expr) != FFEBLD_opCONTER))
	error = TRUE;
      break;

    case FFEEXPR_contextCHARACTERSIZE:
    case FFEEXPR_contextKINDTYPE:
    case FFEEXPR_contextDIMLISTCOMMON:
      if ((error = (expr != NULL) && (ffeinfo_rank (info) != 0)))
	break;
      switch ((expr == NULL) ? FFEINFO_basictypeNONE
	      : ffeinfo_basictype (info))
	{
	case FFEINFO_basictypeLOGICAL:
	  expr = ffeexpr_convert (expr, ft, ft, FFEINFO_basictypeLOGICAL,
	     FFEINFO_kindtypeLOGICALDEFAULT, 0, FFETARGET_charactersizeNONE,
				  FFEEXPR_contextLET);
	  /* Fall through. */
	case FFEINFO_basictypeREAL:
	case FFEINFO_basictypeCOMPLEX:
	  if (ffe_is_pedantic ())
	    {
	      error = TRUE;
	      break;
	    }
	  /* Fall through. */
	case FFEINFO_basictypeINTEGER:
	case FFEINFO_basictypeHOLLERITH:
	case FFEINFO_basictypeTYPELESS:
	  error = FALSE;
	  expr = ffeexpr_convert (expr, ft, ft, FFEINFO_basictypeINTEGER,
	     FFEINFO_kindtypeINTEGERDEFAULT, 0, FFETARGET_charactersizeNONE,
				  FFEEXPR_contextLET);
	  break;

	default:
	  error = TRUE;
	  break;
	}
      if ((expr != NULL) && (ffebld_op (expr) != FFEBLD_opCONTER))
	error = TRUE;
      break;

    case FFEEXPR_contextEQVINDEX_:
      if ((error = (expr != NULL) && (ffeinfo_rank (info) != 0)))
	break;
      switch ((expr == NULL) ? FFEINFO_basictypeNONE
	      : ffeinfo_basictype (info))
	{
	case FFEINFO_basictypeNONE:
	  error = FALSE;
	  break;

	case FFEINFO_basictypeLOGICAL:
	  expr = ffeexpr_convert (expr, ft, ft, FFEINFO_basictypeLOGICAL,
	     FFEINFO_kindtypeLOGICALDEFAULT, 0, FFETARGET_charactersizeNONE,
				  FFEEXPR_contextLET);
	  /* Fall through. */
	case FFEINFO_basictypeREAL:
	case FFEINFO_basictypeCOMPLEX:
	  if (ffe_is_pedantic ())
	    {
	      error = TRUE;
	      break;
	    }
	  /* Fall through. */
	case FFEINFO_basictypeINTEGER:
	case FFEINFO_basictypeHOLLERITH:
	case FFEINFO_basictypeTYPELESS:
	  error = FALSE;
	  expr = ffeexpr_convert (expr, ft, ft, FFEINFO_basictypeINTEGER,
	     FFEINFO_kindtypeINTEGERDEFAULT, 0, FFETARGET_charactersizeNONE,
				  FFEEXPR_contextLET);
	  break;

	default:
	  error = TRUE;
	  break;
	}
      if ((expr != NULL) && (ffebld_op (expr) != FFEBLD_opCONTER))
	error = TRUE;
      break;

    case FFEEXPR_contextPARAMETER:
      if (ffeexpr_stack_->is_rhs)
	error = (expr == NULL) || (ffeinfo_rank (info) != 0)
	  || (ffebld_op (expr) != FFEBLD_opCONTER);
      else
	error = (expr == NULL) || (ffeinfo_rank (info) != 0)
	  || (ffebld_op (expr) != FFEBLD_opSYMTER);
      break;

    case FFEEXPR_contextINDEXORACTUALARG_:
      if (ffelex_token_type (t) == FFELEX_typeCOLON)
	ffeexpr_stack_->context = FFEEXPR_contextINDEX_;
      else
	ffeexpr_stack_->context = FFEEXPR_contextACTUALARG_;
      goto again;		/* :::::::::::::::::::: */

    case FFEEXPR_contextINDEXORACTUALARGEXPR_:
      if (ffelex_token_type (t) == FFELEX_typeCOLON)
	ffeexpr_stack_->context = FFEEXPR_contextINDEX_;
      else
	ffeexpr_stack_->context = FFEEXPR_contextACTUALARGEXPR_;
      goto again;		/* :::::::::::::::::::: */

    case FFEEXPR_contextSFUNCDEFINDEXORACTUALARG_:
      if (ffelex_token_type (t) == FFELEX_typeCOLON)
	ffeexpr_stack_->context = FFEEXPR_contextSFUNCDEFINDEX_;
      else
	ffeexpr_stack_->context = FFEEXPR_contextSFUNCDEFACTUALARG_;
      goto again;		/* :::::::::::::::::::: */

    case FFEEXPR_contextSFUNCDEFINDEXORACTUALARGEXPR_:
      if (ffelex_token_type (t) == FFELEX_typeCOLON)
	ffeexpr_stack_->context = FFEEXPR_contextSFUNCDEFINDEX_;
      else
	ffeexpr_stack_->context = FFEEXPR_contextSFUNCDEFACTUALARGEXPR_;
      goto again;		/* :::::::::::::::::::: */

    case FFEEXPR_contextIMPDOCTRL_:
      if ((error = (expr == NULL) || (ffeinfo_rank (info) != 0)))
	break;
      if (!ffeexpr_stack_->is_rhs
	  && (ffebld_op (expr) != FFEBLD_opSYMTER))
	error = TRUE;
      switch (ffeinfo_basictype (info))
	{
	case FFEINFO_basictypeLOGICAL:
	  if (! ffe_is_ugly_logint ())
	    error = TRUE;
	  if (! ffeexpr_stack_->is_rhs)
	    break;
	  expr = ffeexpr_convert (expr, ft, ft, FFEINFO_basictypeINTEGER,
				  ffeinfo_kindtype (info), 0,
				  FFETARGET_charactersizeNONE,
				  FFEEXPR_contextLET);
	  break;

	case FFEINFO_basictypeINTEGER:
	case FFEINFO_basictypeHOLLERITH:
	case FFEINFO_basictypeTYPELESS:
	  break;

	case FFEINFO_basictypeREAL:
	  if (!ffeexpr_stack_->is_rhs
	      && ffe_is_warn_surprising ()
	      && !error)
	    {
	      ffebad_start (FFEBAD_DO_REAL);	/* See error message!!! */
	      ffebad_here (0, ffelex_token_where_line (ft),
			   ffelex_token_where_column (ft));
	      ffebad_string (ffelex_token_text (ft));
	      ffebad_finish ();
	    }
	  break;

	default:
	  error = TRUE;
	  break;
	}
      break;

    case FFEEXPR_contextDATAIMPDOCTRL_:
      if ((error = (expr == NULL) || (ffeinfo_rank (info) != 0)))
	break;
      if (ffeexpr_stack_->is_rhs)
	{
	  if ((ffebld_op (expr) != FFEBLD_opCONTER)
	      && (ffeinfo_where (info) != FFEINFO_whereIMMEDIATE))
	    error = TRUE;
	}
      else if ((ffebld_op (expr) != FFEBLD_opSYMTER)
	       || (ffeinfo_where (info) != FFEINFO_whereIMMEDIATE))
	error = TRUE;
      switch (ffeinfo_basictype (info))
	{
	case FFEINFO_basictypeLOGICAL:
	  if (! ffeexpr_stack_->is_rhs)
	    break;
	  expr = ffeexpr_convert (expr, ft, ft, FFEINFO_basictypeINTEGER,
				  ffeinfo_kindtype (info), 0,
				  FFETARGET_charactersizeNONE,
				  FFEEXPR_contextLET);
	  /* Fall through.  */
	case FFEINFO_basictypeINTEGER:
	  if (ffeexpr_stack_->is_rhs
	      && (ffeinfo_kindtype (ffebld_info (expr))
		  != FFEINFO_kindtypeINTEGERDEFAULT))
	    expr = ffeexpr_convert (expr, ft, ft, FFEINFO_basictypeINTEGER,
				    FFEINFO_kindtypeINTEGERDEFAULT, 0,
				    FFETARGET_charactersizeNONE,
				    FFEEXPR_contextLET);
	  break;

	case FFEINFO_basictypeHOLLERITH:
	case FFEINFO_basictypeTYPELESS:
	  expr = ffeexpr_convert (expr, ft, ft, FFEINFO_basictypeINTEGER,
	     FFEINFO_kindtypeINTEGERDEFAULT, 0, FFETARGET_charactersizeNONE,
				  FFEEXPR_contextLET);
	  break;

	case FFEINFO_basictypeREAL:
	  if (!ffeexpr_stack_->is_rhs
	      && ffe_is_warn_surprising ()
	      && !error)
	    {
	      ffebad_start (FFEBAD_DO_REAL);	/* See error message!!! */
	      ffebad_here (0, ffelex_token_where_line (ft),
			   ffelex_token_where_column (ft));
	      ffebad_string (ffelex_token_text (ft));
	      ffebad_finish ();
	    }
	  break;

	default:
	  error = TRUE;
	  break;
	}
      break;

    case FFEEXPR_contextIMPDOITEM_:
      if (ffelex_token_type (t) == FFELEX_typeEQUALS)
	{
	  ffeexpr_stack_->is_rhs = FALSE;
	  ffeexpr_stack_->context = FFEEXPR_contextIMPDOCTRL_;
	  goto again;		/* :::::::::::::::::::: */
	}
      /* Fall through. */
    case FFEEXPR_contextIOLIST:
    case FFEEXPR_contextFILEVXTCODE:
      switch ((expr == NULL) ? FFEINFO_basictypeNONE
	      : ffeinfo_basictype (info))
	{
	case FFEINFO_basictypeHOLLERITH:
	case FFEINFO_basictypeTYPELESS:
	  expr = ffeexpr_convert (expr, ft, ft, FFEINFO_basictypeINTEGER,
	     FFEINFO_kindtypeINTEGERDEFAULT, 0, FFETARGET_charactersizeNONE,
				  FFEEXPR_contextLET);
	  break;

	default:
	  break;
	}
      error = (expr == NULL)
	|| ((ffeinfo_rank (info) != 0)
	    && ((ffebld_op (expr) != FFEBLD_opSYMTER)
		|| (ffesymbol_arraysize (ffebld_symter (expr)) == NULL)
		|| (ffebld_op (ffesymbol_arraysize (ffebld_symter (expr)))
		    == FFEBLD_opSTAR)));	/* Bad if null expr, or if
						   array that is not a SYMTER
						   (can't happen yet, I
						   think) or has a NULL or
						   STAR (assumed) array
						   size. */
      break;

    case FFEEXPR_contextIMPDOITEMDF_:
      if (ffelex_token_type (t) == FFELEX_typeEQUALS)
	{
	  ffeexpr_stack_->is_rhs = FALSE;
	  ffeexpr_stack_->context = FFEEXPR_contextIMPDOCTRL_;
	  goto again;		/* :::::::::::::::::::: */
	}
      /* Fall through. */
    case FFEEXPR_contextIOLISTDF:
      switch ((expr == NULL) ? FFEINFO_basictypeNONE
	      : ffeinfo_basictype (info))
	{
	case FFEINFO_basictypeHOLLERITH:
	case FFEINFO_basictypeTYPELESS:
	  expr = ffeexpr_convert (expr, ft, ft, FFEINFO_basictypeINTEGER,
	     FFEINFO_kindtypeINTEGERDEFAULT, 0, FFETARGET_charactersizeNONE,
				  FFEEXPR_contextLET);
	  break;

	default:
	  break;
	}
      error
	= (expr == NULL)
	  || ((ffeinfo_basictype (info) == FFEINFO_basictypeCHARACTER)
	      && (ffeinfo_kindtype (info) != FFEINFO_kindtypeCHARACTERDEFAULT))
	    || ((ffeinfo_rank (info) != 0)
		&& ((ffebld_op (expr) != FFEBLD_opSYMTER)
		    || (ffesymbol_arraysize (ffebld_symter (expr)) == NULL)
		    || (ffebld_op (ffesymbol_arraysize (ffebld_symter (expr)))
			== FFEBLD_opSTAR)));	/* Bad if null expr,
						   non-default-kindtype
						   character expr, or if
						   array that is not a SYMTER
						   (can't happen yet, I
						   think) or has a NULL or
						   STAR (assumed) array
						   size. */
      break;

    case FFEEXPR_contextDATAIMPDOITEM_:
      error = (expr == NULL)
	|| (ffebld_op (expr) != FFEBLD_opARRAYREF)
	|| ((ffeinfo_where (info) != FFEINFO_whereFLEETING_CADDR)
	    && (ffeinfo_where (info) != FFEINFO_whereFLEETING_IADDR));
      break;

    case FFEEXPR_contextDATAIMPDOINDEX_:
      if ((error = (expr == NULL) || (ffeinfo_rank (info) != 0)))
	break;
      switch (ffeinfo_basictype (info))
	{
	case FFEINFO_basictypeLOGICAL:
	  expr = ffeexpr_convert (expr, ft, ft, FFEINFO_basictypeLOGICAL,
	     FFEINFO_kindtypeLOGICALDEFAULT, 0, FFETARGET_charactersizeNONE,
				  FFEEXPR_contextLET);
	  /* Fall through. */
	case FFEINFO_basictypeREAL:
	case FFEINFO_basictypeCOMPLEX:
	  if (ffe_is_pedantic ())
	    {
	      error = TRUE;
	      break;
	    }
	  /* Fall through. */
	case FFEINFO_basictypeINTEGER:
	case FFEINFO_basictypeHOLLERITH:
	case FFEINFO_basictypeTYPELESS:
	  error = FALSE;
	  expr = ffeexpr_convert (expr, ft, ft, FFEINFO_basictypeINTEGER,
	     FFEINFO_kindtypeINTEGERDEFAULT, 0, FFETARGET_charactersizeNONE,
				  FFEEXPR_contextLET);
	  break;

	default:
	  error = TRUE;
	  break;
	}
      if ((ffeinfo_where (info) != FFEINFO_whereCONSTANT)
	  && (ffeinfo_where (info) != FFEINFO_whereIMMEDIATE))
	error = TRUE;
      break;

    case FFEEXPR_contextDATA:
      if (expr == NULL)
	error = TRUE;
      else if (ffeexpr_stack_->is_rhs)
	error = (ffebld_op (expr) != FFEBLD_opCONTER);
      else if (ffebld_op (expr) == FFEBLD_opSYMTER)
	error = FALSE;
      else
	error = (ffeinfo_where (info) != FFEINFO_whereFLEETING_CADDR);
      break;

    case FFEEXPR_contextINITVAL:
      error = (expr == NULL) || (ffebld_op (expr) != FFEBLD_opCONTER);
      break;

    case FFEEXPR_contextEQUIVALENCE:
      if (expr == NULL)
	error = TRUE;
      else if (ffebld_op (expr) == FFEBLD_opSYMTER)
	error = FALSE;
      else
	error = (ffeinfo_where (info) != FFEINFO_whereFLEETING_CADDR);
      break;

    case FFEEXPR_contextFILEASSOC:
    case FFEEXPR_contextFILEINT:
      switch ((expr == NULL) ? FFEINFO_basictypeNONE
	      : ffeinfo_basictype (info))
	{
	case FFEINFO_basictypeINTEGER:
	  /* Maybe this should be supported someday, but, right now,
	     g77 can't generate a call to libf2c to write to an
	     integer other than the default size.  */
	  error = ((! ffeexpr_stack_->is_rhs)
		   && ffeinfo_kindtype (info) != FFEINFO_kindtypeINTEGERDEFAULT);
	  break;

	default:
	  error = TRUE;
	  break;
	}
      if ((expr == NULL) || (ffeinfo_rank (info) != 0))
	error = TRUE;
      break;

    case FFEEXPR_contextFILEDFINT:
      switch ((expr == NULL) ? FFEINFO_basictypeNONE
	      : ffeinfo_basictype (info))
	{
	case FFEINFO_basictypeINTEGER:
	  error = (ffeinfo_kindtype (info) != FFEINFO_kindtypeINTEGERDEFAULT);
	  break;

	default:
	  error = TRUE;
	  break;
	}
      if ((expr == NULL) || (ffeinfo_rank (info) != 0))
	error = TRUE;
      break;

    case FFEEXPR_contextFILELOG:
      switch ((expr == NULL) ? FFEINFO_basictypeNONE
	      : ffeinfo_basictype (info))
	{
	case FFEINFO_basictypeLOGICAL:
	  error = FALSE;
	  break;

	default:
	  error = TRUE;
	  break;
	}
      if ((expr == NULL) || (ffeinfo_rank (info) != 0))
	error = TRUE;
      break;

    case FFEEXPR_contextFILECHAR:
      switch ((expr == NULL) ? FFEINFO_basictypeNONE
	      : ffeinfo_basictype (info))
	{
	case FFEINFO_basictypeCHARACTER:
	  error = FALSE;
	  break;

	default:
	  error = TRUE;
	  break;
	}
      if ((expr == NULL) || (ffeinfo_rank (info) != 0))
	error = TRUE;
      break;

    case FFEEXPR_contextFILENUMCHAR:
      if ((error = (expr == NULL) || (ffeinfo_rank (info) != 0)))
	break;
      switch (ffeinfo_basictype (info))
	{
	case FFEINFO_basictypeLOGICAL:
	  expr = ffeexpr_convert (expr, ft, ft, FFEINFO_basictypeLOGICAL,
	     FFEINFO_kindtypeLOGICALDEFAULT, 0, FFETARGET_charactersizeNONE,
				  FFEEXPR_contextLET);
	  /* Fall through. */
	case FFEINFO_basictypeREAL:
	case FFEINFO_basictypeCOMPLEX:
	  if (ffe_is_pedantic ())
	    {
	      error = TRUE;
	      break;
	    }
	  expr = ffeexpr_convert (expr, ft, ft, FFEINFO_basictypeINTEGER,
	     FFEINFO_kindtypeINTEGERDEFAULT, 0, FFETARGET_charactersizeNONE,
				  FFEEXPR_contextLET);
	  break;

	case FFEINFO_basictypeINTEGER:
	case FFEINFO_basictypeCHARACTER:
	  error = FALSE;
	  break;

	default:
	  error = TRUE;
	  break;
	}
      break;

    case FFEEXPR_contextFILEDFCHAR:
      if ((error = (expr == NULL) || (ffeinfo_rank (info) != 0)))
	break;
      switch (ffeinfo_basictype (info))
	{
	case FFEINFO_basictypeCHARACTER:
	  error
	    = (ffeinfo_kindtype (info)
	       != FFEINFO_kindtypeCHARACTERDEFAULT);
	  break;

	default:
	  error = TRUE;
	  break;
	}
      if (!ffeexpr_stack_->is_rhs
	  && (ffebld_op (expr) == FFEBLD_opSUBSTR))
	error = TRUE;
      break;

    case FFEEXPR_contextFILEUNIT:	/* See equiv code in _ambig_. */
      switch ((expr == NULL) ? FFEINFO_basictypeNONE
	      : ffeinfo_basictype (info))
	{
	case FFEINFO_basictypeLOGICAL:
	  if ((error = (ffeinfo_rank (info) != 0)))
	    break;
	  expr = ffeexpr_convert (expr, ft, ft, FFEINFO_basictypeLOGICAL,
	     FFEINFO_kindtypeLOGICALDEFAULT, 0, FFETARGET_charactersizeNONE,
				  FFEEXPR_contextLET);
	  /* Fall through. */
	case FFEINFO_basictypeREAL:
	case FFEINFO_basictypeCOMPLEX:
	  if ((error = (ffeinfo_rank (info) != 0)))
	    break;
	  if (ffe_is_pedantic ())
	    {
	      error = TRUE;
	      break;
	    }
	  /* Fall through. */
	case FFEINFO_basictypeINTEGER:
	case FFEINFO_basictypeHOLLERITH:
	case FFEINFO_basictypeTYPELESS:
	  if ((error = (ffeinfo_rank (info) != 0)))
	    break;
	  expr = ffeexpr_convert (expr, ft, ft, FFEINFO_basictypeINTEGER,
	     FFEINFO_kindtypeINTEGERDEFAULT, 0, FFETARGET_charactersizeNONE,
				  FFEEXPR_contextLET);
	  break;

	case FFEINFO_basictypeCHARACTER:
	  switch (ffebld_op (expr))
	    {			/* As if _lhs had been called instead of
				   _rhs. */
	    case FFEBLD_opSYMTER:
	      error
		= (ffeinfo_where (ffebld_info (expr)) == FFEINFO_whereCONSTANT);
	      break;

	    case FFEBLD_opSUBSTR:
	      error = (ffeinfo_where (ffebld_info (expr))
		       == FFEINFO_whereCONSTANT_SUBOBJECT);
	      break;

	    case FFEBLD_opARRAYREF:
	      error = FALSE;
	      break;

	    default:
	      error = TRUE;
	      break;
	    }
	  if (!error
	   && ((ffeinfo_kindtype (info) != FFEINFO_kindtypeCHARACTERDEFAULT)
	       || ((ffeinfo_rank (info) != 0)
		   && ((ffebld_op (expr) != FFEBLD_opSYMTER)
		     || (ffesymbol_arraysize (ffebld_symter (expr)) == NULL)
		  || (ffebld_op (ffesymbol_arraysize (ffebld_symter (expr)))
		      == FFEBLD_opSTAR)))))	/* Bad if
						   non-default-kindtype
						   character expr, or if
						   array that is not a SYMTER
						   (can't happen yet, I
						   think), or has a NULL or
						   STAR (assumed) array
						   size. */
	    error = TRUE;
	  break;

	default:
	  error = TRUE;
	  break;
	}
      break;

    case FFEEXPR_contextFILEFORMAT:
      switch ((expr == NULL) ? FFEINFO_basictypeNONE
	      : ffeinfo_basictype (info))
	{
	case FFEINFO_basictypeINTEGER:
	  error = (expr == NULL)
	    || ((ffeinfo_rank (info) != 0) ?
		ffe_is_pedantic ()	/* F77 C5. */
		: (bool) (ffeinfo_kindtype (info) != ffecom_label_kind ()))
	    || (ffebld_op (expr) != FFEBLD_opSYMTER);
	  break;

	case FFEINFO_basictypeLOGICAL:
	case FFEINFO_basictypeREAL:
	case FFEINFO_basictypeCOMPLEX:
	  /* F77 C5 -- must be an array of hollerith.  */
	  error
	    = ffe_is_pedantic ()
	      || (ffeinfo_rank (info) == 0);
	  break;

	case FFEINFO_basictypeCHARACTER:
	  if ((ffeinfo_kindtype (info) != FFEINFO_kindtypeCHARACTERDEFAULT)
	      || ((ffeinfo_rank (info) != 0)
		  && ((ffebld_op (expr) != FFEBLD_opSYMTER)
		      || (ffesymbol_arraysize (ffebld_symter (expr)) == NULL)
		      || (ffebld_op (ffesymbol_arraysize (ffebld_symter (expr)))
			  == FFEBLD_opSTAR))))	/* Bad if
						   non-default-kindtype
						   character expr, or if
						   array that is not a SYMTER
						   (can't happen yet, I
						   think), or has a NULL or
						   STAR (assumed) array
						   size. */
	    error = TRUE;
	  else
	    error = FALSE;
	  break;

	default:
	  error = TRUE;
	  break;
	}
      break;

    case FFEEXPR_contextLOC_:
      /* See also ffeintrin_check_loc_.  */
      if ((expr == NULL)
	  || (ffeinfo_kind (info) != FFEINFO_kindENTITY)
	  || ((ffebld_op (expr) != FFEBLD_opSYMTER)
	      && (ffebld_op (expr) != FFEBLD_opSUBSTR)
	      && (ffebld_op (expr) != FFEBLD_opARRAYREF)))
	error = TRUE;
      break;

    default:
      error = FALSE;
      break;
    }

  if (error && ((expr == NULL) || (ffebld_op (expr) != FFEBLD_opANY)))
    {
      ffebad_start (FFEBAD_EXPR_WRONG);
      ffebad_here (0, ffelex_token_where_line (ft),
		   ffelex_token_where_column (ft));
      ffebad_finish ();
      expr = ffebld_new_any ();
      ffebld_set_info (expr, ffeinfo_new_any ());
    }

  callback = ffeexpr_stack_->callback;
  s = ffeexpr_stack_->previous;
  malloc_kill_ks (ffe_pool_program_unit (), ffeexpr_stack_,
		  sizeof (*ffeexpr_stack_));
  ffeexpr_stack_ = s;
  next = (ffelexHandler) (*callback) (ft, expr, t);
  ffelex_token_kill (ft);
  return (ffelexHandler) next;
}

/* ffeexpr_finished_ambig_ -- Check validity of ambiguous unit/form spec

   ffebld expr;
   expr = ffeexpr_finished_ambig_(expr);

   Replicates a bit of ffeexpr_finished_'s task when in a context
   of UNIT or FORMAT.  */

static ffebld
ffeexpr_finished_ambig_ (ffelexToken ft, ffebld expr)
{
  ffeinfo info = ffebld_info (expr);
  bool error;

  switch (ffeexpr_stack_->context)
    {
    case FFEEXPR_contextFILENUMAMBIG:	/* Same as FILENUM in _finished_. */
      switch ((expr == NULL) ? FFEINFO_basictypeNONE
	      : ffeinfo_basictype (info))
	{
	case FFEINFO_basictypeLOGICAL:
	  expr = ffeexpr_convert (expr, ft, ft, FFEINFO_basictypeLOGICAL,
	     FFEINFO_kindtypeLOGICALDEFAULT, 0, FFETARGET_charactersizeNONE,
				  FFEEXPR_contextLET);
	  /* Fall through. */
	case FFEINFO_basictypeREAL:
	case FFEINFO_basictypeCOMPLEX:
	  if (ffe_is_pedantic ())
	    {
	      error = TRUE;
	      break;
	    }
	  /* Fall through. */
	case FFEINFO_basictypeINTEGER:
	case FFEINFO_basictypeHOLLERITH:
	case FFEINFO_basictypeTYPELESS:
	  error = FALSE;
	  expr = ffeexpr_convert (expr, ft, ft, FFEINFO_basictypeINTEGER,
	     FFEINFO_kindtypeINTEGERDEFAULT, 0, FFETARGET_charactersizeNONE,
				  FFEEXPR_contextLET);
	  break;

	default:
	  error = TRUE;
	  break;
	}
      if ((expr == NULL) || (ffeinfo_rank (info) != 0))
	error = TRUE;
      break;

    case FFEEXPR_contextFILEUNITAMBIG:	/* Same as FILEUNIT in _finished_. */
      if ((expr != NULL) && (ffebld_op (expr) == FFEBLD_opSTAR))
	{
	  error = FALSE;
	  break;
	}
      switch ((expr == NULL) ? FFEINFO_basictypeNONE
	      : ffeinfo_basictype (info))
	{
	case FFEINFO_basictypeLOGICAL:
	  expr = ffeexpr_convert (expr, ft, ft, FFEINFO_basictypeLOGICAL,
	     FFEINFO_kindtypeLOGICALDEFAULT, 0, FFETARGET_charactersizeNONE,
				  FFEEXPR_contextLET);
	  /* Fall through. */
	case FFEINFO_basictypeREAL:
	case FFEINFO_basictypeCOMPLEX:
	  if (ffe_is_pedantic ())
	    {
	      error = TRUE;
	      break;
	    }
	  /* Fall through. */
	case FFEINFO_basictypeINTEGER:
	case FFEINFO_basictypeHOLLERITH:
	case FFEINFO_basictypeTYPELESS:
	  error = (ffeinfo_rank (info) != 0);
	  expr = ffeexpr_convert (expr, ft, ft, FFEINFO_basictypeINTEGER,
	     FFEINFO_kindtypeINTEGERDEFAULT, 0, FFETARGET_charactersizeNONE,
				  FFEEXPR_contextLET);
	  break;

	case FFEINFO_basictypeCHARACTER:
	  switch (ffebld_op (expr))
	    {			/* As if _lhs had been called instead of
				   _rhs. */
	    case FFEBLD_opSYMTER:
	      error
		= (ffeinfo_where (ffebld_info (expr)) == FFEINFO_whereCONSTANT);
	      break;

	    case FFEBLD_opSUBSTR:
	      error = (ffeinfo_where (ffebld_info (expr))
		       == FFEINFO_whereCONSTANT_SUBOBJECT);
	      break;

	    case FFEBLD_opARRAYREF:
	      error = FALSE;
	      break;

	    default:
	      error = TRUE;
	      break;
	    }
	  break;

	default:
	  error = TRUE;
	  break;
	}
      break;

    default:
      assert ("bad context" == NULL);
      error = TRUE;
      break;
    }

  if (error && ((expr == NULL) || (ffebld_op (expr) != FFEBLD_opANY)))
    {
      ffebad_start (FFEBAD_EXPR_WRONG);
      ffebad_here (0, ffelex_token_where_line (ft),
		   ffelex_token_where_column (ft));
      ffebad_finish ();
      expr = ffebld_new_any ();
      ffebld_set_info (expr, ffeinfo_new_any ());
    }

  return expr;
}

/* ffeexpr_token_lhs_ -- Initial state for lhs expression

   Return a pointer to this function to the lexer (ffelex), which will
   invoke it for the next token.

   Basically a smaller version of _rhs_; keep them both in sync, of course.  */

static ffelexHandler
ffeexpr_token_lhs_ (ffelexToken t)
{

  /* When changing the list of valid initial lhs tokens, check whether to
     update a corresponding list in ffeexpr_cb_close_paren_ambig_1_ for the
     READ (expr) <token> case -- it assumes it knows which tokens <token> can
     be to indicate an lhs (or implied DO), which right now is the set
     {NAME,OPEN_PAREN}.

     This comment also appears in ffeexpr_token_first_lhs_. */

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
    case FFELEX_typeNAMES:
      ffeexpr_tokens_[0] = ffelex_token_use (t);
      return (ffelexHandler) ffeexpr_token_name_lhs_;

    default:
      return (ffelexHandler) ffeexpr_finished_ (t);
    }
}

/* ffeexpr_token_rhs_ -- Initial state for rhs expression

   Return a pointer to this function to the lexer (ffelex), which will
   invoke it for the next token.

   The initial state and the post-binary-operator state are the same and
   both handled here, with the expression stack used to distinguish
   between them.  Binary operators are invalid here; unary operators,
   constants, subexpressions, and name references are valid.  */

static ffelexHandler
ffeexpr_token_rhs_ (ffelexToken t)
{
  ffeexprExpr_ e;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeQUOTE:
      if (ffe_is_vxt ())
	{
	  ffeexpr_tokens_[0] = ffelex_token_use (t);
	  return (ffelexHandler) ffeexpr_token_quote_;
	}
      ffeexpr_tokens_[0] = ffelex_token_use (t);
      ffelex_set_expecting_hollerith (-1, '\"',
				      ffelex_token_where_line (t),
				      ffelex_token_where_column (t));
      /* Don't have to unset this one. */
      return (ffelexHandler) ffeexpr_token_apostrophe_;

    case FFELEX_typeAPOSTROPHE:
      ffeexpr_tokens_[0] = ffelex_token_use (t);
      ffelex_set_expecting_hollerith (-1, '\'',
				      ffelex_token_where_line (t),
				      ffelex_token_where_column (t));
      /* Don't have to unset this one. */
      return (ffelexHandler) ffeexpr_token_apostrophe_;

    case FFELEX_typePERCENT:
      ffeexpr_tokens_[0] = ffelex_token_use (t);
      return (ffelexHandler) ffeexpr_token_percent_;

    case FFELEX_typeOPEN_PAREN:
      ffeexpr_stack_->tokens[0] = ffelex_token_use (t);
      return (ffelexHandler) ffeexpr_rhs (ffeexpr_stack_->pool,
					  FFEEXPR_contextPAREN_,
					  ffeexpr_cb_close_paren_c_);

    case FFELEX_typePLUS:
      e = ffeexpr_expr_new_ ();
      e->type = FFEEXPR_exprtypeUNARY_;
      e->token = ffelex_token_use (t);
      e->u.operator.op = FFEEXPR_operatorADD_;
      e->u.operator.prec = FFEEXPR_operatorprecedenceADD_;
      e->u.operator.as = FFEEXPR_operatorassociativityADD_;
      ffeexpr_exprstack_push_unary_ (e);
      return (ffelexHandler) ffeexpr_token_rhs_;

    case FFELEX_typeMINUS:
      e = ffeexpr_expr_new_ ();
      e->type = FFEEXPR_exprtypeUNARY_;
      e->token = ffelex_token_use (t);
      e->u.operator.op = FFEEXPR_operatorSUBTRACT_;
      e->u.operator.prec = FFEEXPR_operatorprecedenceSUBTRACT_;
      e->u.operator.as = FFEEXPR_operatorassociativitySUBTRACT_;
      ffeexpr_exprstack_push_unary_ (e);
      return (ffelexHandler) ffeexpr_token_rhs_;

    case FFELEX_typePERIOD:
      ffeexpr_tokens_[0] = ffelex_token_use (t);
      return (ffelexHandler) ffeexpr_token_period_;

    case FFELEX_typeNUMBER:
      ffeexpr_tokens_[0] = ffelex_token_use (t);
      ffeexpr_hollerith_count_ = atol (ffelex_token_text (t));
      if (ffeexpr_hollerith_count_ > 0)
	ffelex_set_expecting_hollerith (ffeexpr_hollerith_count_,
					'\0',
					ffelex_token_where_line (t),
					ffelex_token_where_column (t));
      return (ffelexHandler) ffeexpr_token_number_;

    case FFELEX_typeNAME:
    case FFELEX_typeNAMES:
      ffeexpr_tokens_[0] = ffelex_token_use (t);
      switch (ffeexpr_stack_->context)
	{
	case FFEEXPR_contextACTUALARG_:
	case FFEEXPR_contextINDEXORACTUALARG_:
	case FFEEXPR_contextSFUNCDEFACTUALARG_:
	case FFEEXPR_contextSFUNCDEFINDEXORACTUALARG_:
	  return (ffelexHandler) ffeexpr_token_name_arg_;

	default:
	  return (ffelexHandler) ffeexpr_token_name_rhs_;
	}

    case FFELEX_typeASTERISK:
    case FFELEX_typeSLASH:
    case FFELEX_typePOWER:
    case FFELEX_typeCONCAT:
    case FFELEX_typeREL_EQ:
    case FFELEX_typeREL_NE:
    case FFELEX_typeREL_LE:
    case FFELEX_typeREL_GE:
      if (ffest_ffebad_start (FFEBAD_MISSING_FIRST_BINARY_OPERAND))
	{
	  ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_finish ();
	}
      return (ffelexHandler) ffeexpr_token_rhs_;

#if 0
    case FFELEX_typeEQUALS:
    case FFELEX_typePOINTS:
    case FFELEX_typeCLOSE_ANGLE:
    case FFELEX_typeCLOSE_PAREN:
    case FFELEX_typeCOMMA:
    case FFELEX_typeCOLON:
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
#endif
    default:
      return (ffelexHandler) ffeexpr_finished_ (t);
    }
}

/* ffeexpr_token_period_ -- Rhs PERIOD

   Return a pointer to this function to the lexer (ffelex), which will
   invoke it for the next token.

   Handle a period detected at rhs (expecting unary op or operand) state.
   Must begin a floating-point value (as in .12) or a dot-dot name, of
   which only .NOT., .TRUE., and .FALSE. are truly valid.  Other sort-of-
   valid names represent binary operators, which are invalid here because
   there isn't an operand at the top of the stack.  */

static ffelexHandler
ffeexpr_token_period_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
    case FFELEX_typeNAMES:
      ffeexpr_current_dotdot_ = ffestr_other (t);
      switch (ffeexpr_current_dotdot_)
	{
	case FFESTR_otherNone:
	  if (ffest_ffebad_start (FFEBAD_IGNORING_PERIOD))
	    {
	      ffebad_here (0, ffelex_token_where_line (ffeexpr_tokens_[0]),
			   ffelex_token_where_column (ffeexpr_tokens_[0]));
	      ffebad_finish ();
	    }
	  ffelex_token_kill (ffeexpr_tokens_[0]);
	  return (ffelexHandler) ffeexpr_token_rhs_ (t);

	case FFESTR_otherTRUE:
	case FFESTR_otherFALSE:
	case FFESTR_otherNOT:
	  ffeexpr_tokens_[1] = ffelex_token_use (t);
	  return (ffelexHandler) ffeexpr_token_end_period_;

	default:
	  if (ffest_ffebad_start (FFEBAD_MISSING_FIRST_BINARY_OPERAND))
	    {
	      ffebad_here (0, ffelex_token_where_line (t),
			   ffelex_token_where_column (t));
	      ffebad_finish ();
	    }
	  ffelex_token_kill (ffeexpr_tokens_[0]);
	  return (ffelexHandler) ffeexpr_token_swallow_period_;
	}
      break;			/* Nothing really reaches here. */

    case FFELEX_typeNUMBER:
      ffeexpr_tokens_[1] = ffelex_token_use (t);
      return (ffelexHandler) ffeexpr_token_real_;

    default:
      if (ffest_ffebad_start (FFEBAD_IGNORING_PERIOD))
	{
	  ffebad_here (0, ffelex_token_where_line (ffeexpr_tokens_[0]),
		       ffelex_token_where_column (ffeexpr_tokens_[0]));
	  ffebad_finish ();
	}
      ffelex_token_kill (ffeexpr_tokens_[0]);
      return (ffelexHandler) ffeexpr_token_rhs_ (t);
    }
}

/* ffeexpr_token_end_period_ -- Rhs PERIOD NAME(NOT, TRUE, or FALSE)

   Return a pointer to this function to the lexer (ffelex), which will
   invoke it for the next token.

   Expecting a period to close a .NOT, .TRUE, or .FALSE at rhs (unary op
   or operator) state.	If period isn't found, issue a diagnostic but
   pretend we saw one.	ffeexpr_current_dotdot_ must already contained the
   dotdot representation of the name in between the two PERIOD tokens.	*/

static ffelexHandler
ffeexpr_token_end_period_ (ffelexToken t)
{
  ffeexprExpr_ e;

  if (ffelex_token_type (t) != FFELEX_typePERIOD)
    {
      if (ffest_ffebad_start (FFEBAD_INSERTING_PERIOD))
	{
	  ffebad_here (0, ffelex_token_where_line (ffeexpr_tokens_[0]),
		       ffelex_token_where_column (ffeexpr_tokens_[0]));
	  ffebad_here (1, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_string (ffelex_token_text (ffeexpr_tokens_[1]));
	  ffebad_finish ();
	}
    }

  ffelex_token_kill (ffeexpr_tokens_[1]);	/* Kill "NOT"/"TRUE"/"FALSE"
						   token. */

  e = ffeexpr_expr_new_ ();
  e->token = ffeexpr_tokens_[0];

  switch (ffeexpr_current_dotdot_)
    {
    case FFESTR_otherNOT:
      e->type = FFEEXPR_exprtypeUNARY_;
      e->u.operator.op = FFEEXPR_operatorNOT_;
      e->u.operator.prec = FFEEXPR_operatorprecedenceNOT_;
      e->u.operator.as = FFEEXPR_operatorassociativityNOT_;
      ffeexpr_exprstack_push_unary_ (e);
      if (ffelex_token_type (t) != FFELEX_typePERIOD)
	return (ffelexHandler) ffeexpr_token_rhs_ (t);
      return (ffelexHandler) ffeexpr_token_rhs_;

    case FFESTR_otherTRUE:
      e->type = FFEEXPR_exprtypeOPERAND_;
      e->u.operand
	= ffebld_new_conter (ffebld_constant_new_logicaldefault (TRUE));
      ffebld_set_info (e->u.operand,
      ffeinfo_new (FFEINFO_basictypeLOGICAL, FFEINFO_kindtypeLOGICALDEFAULT,
		   0, FFEINFO_kindENTITY, FFEINFO_whereCONSTANT, FFETARGET_charactersizeNONE));
      ffeexpr_exprstack_push_operand_ (e);
      if (ffelex_token_type (t) != FFELEX_typePERIOD)
	return (ffelexHandler) ffeexpr_token_binary_ (t);
      return (ffelexHandler) ffeexpr_token_binary_;

    case FFESTR_otherFALSE:
      e->type = FFEEXPR_exprtypeOPERAND_;
      e->u.operand
	= ffebld_new_conter (ffebld_constant_new_logicaldefault (FALSE));
      ffebld_set_info (e->u.operand,
      ffeinfo_new (FFEINFO_basictypeLOGICAL, FFEINFO_kindtypeLOGICALDEFAULT,
		   0, FFEINFO_kindENTITY, FFEINFO_whereCONSTANT, FFETARGET_charactersizeNONE));
      ffeexpr_exprstack_push_operand_ (e);
      if (ffelex_token_type (t) != FFELEX_typePERIOD)
	return (ffelexHandler) ffeexpr_token_binary_ (t);
      return (ffelexHandler) ffeexpr_token_binary_;

    default:
      assert ("Bad unary dotdot in ffeexpr_current_dotdot_" == NULL);
      exit (0);
      return NULL;
    }
}

/* ffeexpr_token_swallow_period_ -- Rhs PERIOD NAME(not NOT, TRUE, or FALSE)

   Return a pointer to this function to the lexer (ffelex), which will
   invoke it for the next token.

   A diagnostic has already been issued; just swallow a period if there is
   one, then continue with ffeexpr_token_rhs_.	*/

static ffelexHandler
ffeexpr_token_swallow_period_ (ffelexToken t)
{
  if (ffelex_token_type (t) != FFELEX_typePERIOD)
    return (ffelexHandler) ffeexpr_token_rhs_ (t);

  return (ffelexHandler) ffeexpr_token_rhs_;
}

/* ffeexpr_token_real_ -- Rhs PERIOD NUMBER

   Return a pointer to this function to the lexer (ffelex), which will
   invoke it for the next token.

   After a period and a string of digits, check next token for possible
   exponent designation (D, E, or Q as first/only character) and continue
   real-number handling accordingly.  Else form basic real constant, push
   onto expression stack, and enter binary state using current token (which,
   if it is a name not beginning with D, E, or Q, will certainly result
   in an error, but that's not for this routine to deal with).	*/

static ffelexHandler
ffeexpr_token_real_ (ffelexToken t)
{
  char d;
  const char *p;

  if (((ffelex_token_type (t) != FFELEX_typeNAME)
       && (ffelex_token_type (t) != FFELEX_typeNAMES))
      || !(((ffesrc_char_match_init ((d = *(p = ffelex_token_text (t))),
				     'D', 'd')
	     || ffesrc_char_match_init (d, 'E', 'e')
	     || ffesrc_char_match_init (d, 'Q', 'q')))
	   && ffeexpr_isdigits_ (++p)))
    {
#if 0
      /* This code has been removed because it seems inconsistent to
	 produce a diagnostic in this case, but not all of the other
	 ones that look for an exponent and cannot recognize one.  */
      if (((ffelex_token_type (t) == FFELEX_typeNAME)
	   || (ffelex_token_type (t) == FFELEX_typeNAMES))
	  && ffest_ffebad_start (FFEBAD_INVALID_EXPONENT))
	{
	  char bad[2];

	  ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_here (1, ffelex_token_where_line (ffeexpr_tokens_[0]),
		       ffelex_token_where_column (ffeexpr_tokens_[0]));
	  bad[0] = *(p - 1);
	  bad[1] = '\0';
	  ffebad_string (bad);
	  ffebad_finish ();
	}
#endif
      ffeexpr_make_float_const_ (ffesrc_char_internal_init ('E', 'e'), NULL,
				 ffeexpr_tokens_[0], ffeexpr_tokens_[1],
				 NULL, NULL, NULL);

      ffelex_token_kill (ffeexpr_tokens_[0]);
      ffelex_token_kill (ffeexpr_tokens_[1]);
      return (ffelexHandler) ffeexpr_token_binary_ (t);
    }

  /* Just exponent character by itself?	 In which case, PLUS or MINUS must
     surely be next, followed by a NUMBER token. */

  if (*p == '\0')
    {
      ffeexpr_tokens_[2] = ffelex_token_use (t);
      return (ffelexHandler) ffeexpr_token_real_exponent_;
    }

  ffeexpr_make_float_const_ (d, NULL, ffeexpr_tokens_[0], ffeexpr_tokens_[1],
			     t, NULL, NULL);

  ffelex_token_kill (ffeexpr_tokens_[0]);
  ffelex_token_kill (ffeexpr_tokens_[1]);
  return (ffelexHandler) ffeexpr_token_binary_;
}

/* ffeexpr_token_real_exponent_ -- Rhs PERIOD NUMBER NAME(D, E, or Q)

   Return a pointer to this function to the lexer (ffelex), which will
   invoke it for the next token.

   Ensures this token is PLUS or MINUS, preserves it, goes to final state
   for real number (exponent digits).  Else issues diagnostic, assumes a
   zero exponent field for number, passes token on to binary state as if
   previous token had been "E0" instead of "E", for example.  */

static ffelexHandler
ffeexpr_token_real_exponent_ (ffelexToken t)
{
  if ((ffelex_token_type (t) != FFELEX_typePLUS)
      && (ffelex_token_type (t) != FFELEX_typeMINUS))
    {
      if (ffest_ffebad_start (FFEBAD_MISSING_EXPONENT_VALUE))
	{
	  ffebad_here (0, ffelex_token_where_line (ffeexpr_tokens_[2]),
		       ffelex_token_where_column (ffeexpr_tokens_[2]));
	  ffebad_here (1, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_finish ();
	}

      ffeexpr_make_float_const_ (ffesrc_char_internal_init ('E', 'e'), NULL,
				 ffeexpr_tokens_[0], ffeexpr_tokens_[1],
				 NULL, NULL, NULL);

      ffelex_token_kill (ffeexpr_tokens_[0]);
      ffelex_token_kill (ffeexpr_tokens_[1]);
      ffelex_token_kill (ffeexpr_tokens_[2]);
      return (ffelexHandler) ffeexpr_token_binary_ (t);
    }

  ffeexpr_tokens_[3] = ffelex_token_use (t);
  return (ffelexHandler) ffeexpr_token_real_exp_sign_;
}

/* ffeexpr_token_real_exp_sign_ -- Rhs PERIOD NUMBER NAME(D,E,Q) PLUS/MINUS

   Return a pointer to this function to the lexer (ffelex), which will
   invoke it for the next token.

   Make sure token is a NUMBER, make a real constant out of all we have and
   push it onto the expression stack.  Else issue diagnostic and pretend
   exponent field was a zero.  */

static ffelexHandler
ffeexpr_token_real_exp_sign_ (ffelexToken t)
{
  if (ffelex_token_type (t) != FFELEX_typeNUMBER)
    {
      if (ffest_ffebad_start (FFEBAD_MISSING_EXPONENT_VALUE))
	{
	  ffebad_here (0, ffelex_token_where_line (ffeexpr_tokens_[2]),
		       ffelex_token_where_column (ffeexpr_tokens_[2]));
	  ffebad_here (1, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_finish ();
	}

      ffeexpr_make_float_const_ (ffesrc_char_internal_init ('E', 'e'), NULL,
				 ffeexpr_tokens_[0], ffeexpr_tokens_[1],
				 NULL, NULL, NULL);

      ffelex_token_kill (ffeexpr_tokens_[0]);
      ffelex_token_kill (ffeexpr_tokens_[1]);
      ffelex_token_kill (ffeexpr_tokens_[2]);
      ffelex_token_kill (ffeexpr_tokens_[3]);
      return (ffelexHandler) ffeexpr_token_binary_ (t);
    }

  ffeexpr_make_float_const_ (ffelex_token_text (ffeexpr_tokens_[2])[0], NULL,
		 ffeexpr_tokens_[0], ffeexpr_tokens_[1], ffeexpr_tokens_[2],
			     ffeexpr_tokens_[3], t);

  ffelex_token_kill (ffeexpr_tokens_[0]);
  ffelex_token_kill (ffeexpr_tokens_[1]);
  ffelex_token_kill (ffeexpr_tokens_[2]);
  ffelex_token_kill (ffeexpr_tokens_[3]);
  return (ffelexHandler) ffeexpr_token_binary_;
}

/* ffeexpr_token_number_ -- Rhs NUMBER

   Return a pointer to this function to the lexer (ffelex), which will
   invoke it for the next token.

   If the token is a period, we may have a floating-point number, or an
   integer followed by a dotdot binary operator.  If the token is a name
   beginning with D, E, or Q, we definitely have a floating-point number.
   If the token is a hollerith constant, that's what we've got, so push
   it onto the expression stack and continue with the binary state.

   Otherwise, we have an integer followed by something the binary state
   should be able to swallow.  */

static ffelexHandler
ffeexpr_token_number_ (ffelexToken t)
{
  ffeexprExpr_ e;
  ffeinfo ni;
  char d;
  const char *p;

  if (ffeexpr_hollerith_count_ > 0)
    ffelex_set_expecting_hollerith (0, '\0',
				    ffewhere_line_unknown (),
				    ffewhere_column_unknown ());

  /* See if we've got a floating-point number here. */

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
    case FFELEX_typeNAMES:
      if ((ffesrc_char_match_init ((d = *(p = ffelex_token_text (t))),
				   'D', 'd')
	   || ffesrc_char_match_init (d, 'E', 'e')
	   || ffesrc_char_match_init (d, 'Q', 'q'))
	  && ffeexpr_isdigits_ (++p))
	{

	  /* Just exponent character by itself?	 In which case, PLUS or MINUS
	     must surely be next, followed by a NUMBER token. */

	  if (*p == '\0')
	    {
	      ffeexpr_tokens_[1] = ffelex_token_use (t);
	      return (ffelexHandler) ffeexpr_token_number_exponent_;
	    }
	  ffeexpr_make_float_const_ (d, ffeexpr_tokens_[0], NULL, NULL, t,
				     NULL, NULL);

	  ffelex_token_kill (ffeexpr_tokens_[0]);
	  return (ffelexHandler) ffeexpr_token_binary_;
	}
      break;

    case FFELEX_typePERIOD:
      ffeexpr_tokens_[1] = ffelex_token_use (t);
      return (ffelexHandler) ffeexpr_token_number_period_;

    case FFELEX_typeHOLLERITH:
      e = ffeexpr_expr_new_ ();
      e->type = FFEEXPR_exprtypeOPERAND_;
      e->token = ffeexpr_tokens_[0];
      e->u.operand = ffebld_new_conter (ffebld_constant_new_hollerith (t));
      ni = ffeinfo_new (FFEINFO_basictypeHOLLERITH, FFEINFO_kindtypeNONE,
			0, FFEINFO_kindENTITY, FFEINFO_whereCONSTANT,
			ffelex_token_length (t));
      ffebld_set_info (e->u.operand, ni);
      ffeexpr_exprstack_push_operand_ (e);
      return (ffelexHandler) ffeexpr_token_binary_;

    default:
      break;
    }

  /* Nothing specific we were looking for, so make an integer and pass the
     current token to the binary state. */

  ffeexpr_make_float_const_ ('I', ffeexpr_tokens_[0], NULL, NULL,
			     NULL, NULL, NULL);
  return (ffelexHandler) ffeexpr_token_binary_ (t);
}

/* ffeexpr_token_number_exponent_ -- Rhs NUMBER NAME(D, E, or Q)

   Return a pointer to this function to the lexer (ffelex), which will
   invoke it for the next token.

   Ensures this token is PLUS or MINUS, preserves it, goes to final state
   for real number (exponent digits).  Else treats number as integer, passes
   name to binary, passes current token to subsequent handler.  */

static ffelexHandler
ffeexpr_token_number_exponent_ (ffelexToken t)
{
  if ((ffelex_token_type (t) != FFELEX_typePLUS)
      && (ffelex_token_type (t) != FFELEX_typeMINUS))
    {
      ffeexprExpr_ e;
      ffelexHandler nexthandler;

      e = ffeexpr_expr_new_ ();
      e->type = FFEEXPR_exprtypeOPERAND_;
      e->token = ffeexpr_tokens_[0];
      e->u.operand = ffebld_new_conter (ffebld_constant_new_integerdefault
					(ffeexpr_tokens_[0]));
      ffebld_set_info (e->u.operand,
      ffeinfo_new (FFEINFO_basictypeINTEGER, FFEINFO_kindtypeINTEGERDEFAULT,
		   0, FFEINFO_kindENTITY, FFEINFO_whereCONSTANT, FFETARGET_charactersizeNONE));
      ffeexpr_exprstack_push_operand_ (e);
      nexthandler = (ffelexHandler) ffeexpr_token_binary_ (ffeexpr_tokens_[1]);
      ffelex_token_kill (ffeexpr_tokens_[1]);
      return (ffelexHandler) (*nexthandler) (t);
    }

  ffeexpr_tokens_[2] = ffelex_token_use (t);
  return (ffelexHandler) ffeexpr_token_number_exp_sign_;
}

/* ffeexpr_token_number_exp_sign_ -- Rhs NUMBER NAME(D,E,Q) PLUS/MINUS

   Return a pointer to this function to the lexer (ffelex), which will
   invoke it for the next token.

   Make sure token is a NUMBER, make a real constant out of all we have and
   push it onto the expression stack.  Else issue diagnostic and pretend
   exponent field was a zero.  */

static ffelexHandler
ffeexpr_token_number_exp_sign_ (ffelexToken t)
{
  if (ffelex_token_type (t) != FFELEX_typeNUMBER)
    {
      if (ffest_ffebad_start (FFEBAD_MISSING_EXPONENT_VALUE))
	{
	  ffebad_here (0, ffelex_token_where_line (ffeexpr_tokens_[1]),
		       ffelex_token_where_column (ffeexpr_tokens_[1]));
	  ffebad_here (1, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_finish ();
	}

      ffeexpr_make_float_const_ (ffelex_token_text (ffeexpr_tokens_[1])[0],
				 ffeexpr_tokens_[0], NULL, NULL,
				 ffeexpr_tokens_[1], ffeexpr_tokens_[2],
				 NULL);

      ffelex_token_kill (ffeexpr_tokens_[0]);
      ffelex_token_kill (ffeexpr_tokens_[1]);
      ffelex_token_kill (ffeexpr_tokens_[2]);
      return (ffelexHandler) ffeexpr_token_binary_ (t);
    }

  ffeexpr_make_float_const_ (ffelex_token_text (ffeexpr_tokens_[1])[0],
			     ffeexpr_tokens_[0], NULL, NULL,
			     ffeexpr_tokens_[1], ffeexpr_tokens_[2], t);

  ffelex_token_kill (ffeexpr_tokens_[0]);
  ffelex_token_kill (ffeexpr_tokens_[1]);
  ffelex_token_kill (ffeexpr_tokens_[2]);
  return (ffelexHandler) ffeexpr_token_binary_;
}

/* ffeexpr_token_number_period_ -- Rhs NUMBER PERIOD

   Return a pointer to this function to the lexer (ffelex), which will
   invoke it for the next token.

   Handle a period detected following a number at rhs state.  Must begin a
   floating-point value (as in 1., 1.2, 1.E3, or 1.E+3) or a dot-dot name.  */

static ffelexHandler
ffeexpr_token_number_period_ (ffelexToken t)
{
  ffeexprExpr_ e;
  ffelexHandler nexthandler;
  const char *p;
  char d;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
    case FFELEX_typeNAMES:
      if ((ffesrc_char_match_init ((d = *(p = ffelex_token_text (t))),
				   'D', 'd')
	   || ffesrc_char_match_init (d, 'E', 'e')
	   || ffesrc_char_match_init (d, 'Q', 'q'))
	  && ffeexpr_isdigits_ (++p))
	{

	  /* Just exponent character by itself?	 In which case, PLUS or MINUS
	     must surely be next, followed by a NUMBER token. */

	  if (*p == '\0')
	    {
	      ffeexpr_tokens_[2] = ffelex_token_use (t);
	      return (ffelexHandler) ffeexpr_token_number_per_exp_;
	    }
	  ffeexpr_make_float_const_ (d, ffeexpr_tokens_[0],
				     ffeexpr_tokens_[1], NULL, t, NULL,
				     NULL);

	  ffelex_token_kill (ffeexpr_tokens_[0]);
	  ffelex_token_kill (ffeexpr_tokens_[1]);
	  return (ffelexHandler) ffeexpr_token_binary_;
	}
      /* A name not representing an exponent, so assume it will be something
	 like EQ, make an integer from the number, pass the period to binary
	 state and the current token to the resulting state. */

      e = ffeexpr_expr_new_ ();
      e->type = FFEEXPR_exprtypeOPERAND_;
      e->token = ffeexpr_tokens_[0];
      e->u.operand = ffebld_new_conter (ffebld_constant_new_integerdefault
					(ffeexpr_tokens_[0]));
      ffebld_set_info (e->u.operand,
		       ffeinfo_new (FFEINFO_basictypeINTEGER,
				    FFEINFO_kindtypeINTEGERDEFAULT, 0,
				  FFEINFO_kindENTITY, FFEINFO_whereCONSTANT,
				    FFETARGET_charactersizeNONE));
      ffeexpr_exprstack_push_operand_ (e);
      nexthandler = (ffelexHandler) ffeexpr_token_binary_
	(ffeexpr_tokens_[1]);
      ffelex_token_kill (ffeexpr_tokens_[1]);
      return (ffelexHandler) (*nexthandler) (t);

    case FFELEX_typeNUMBER:
      ffeexpr_tokens_[2] = ffelex_token_use (t);
      return (ffelexHandler) ffeexpr_token_number_real_;

    default:
      break;
    }

  /* Nothing specific we were looking for, so make a real number and pass the
     period and then the current token to the binary state. */

  ffeexpr_make_float_const_ (ffesrc_char_internal_init ('E', 'e'),
			     ffeexpr_tokens_[0], ffeexpr_tokens_[1],
			     NULL, NULL, NULL, NULL);

  ffelex_token_kill (ffeexpr_tokens_[0]);
  ffelex_token_kill (ffeexpr_tokens_[1]);
  return (ffelexHandler) ffeexpr_token_binary_ (t);
}

/* ffeexpr_token_number_per_exp_ -- Rhs NUMBER PERIOD NAME(D, E, or Q)

   Return a pointer to this function to the lexer (ffelex), which will
   invoke it for the next token.

   Ensures this token is PLUS or MINUS, preserves it, goes to final state
   for real number (exponent digits).  Else treats number as real, passes
   name to binary, passes current token to subsequent handler.	*/

static ffelexHandler
ffeexpr_token_number_per_exp_ (ffelexToken t)
{
  if ((ffelex_token_type (t) != FFELEX_typePLUS)
      && (ffelex_token_type (t) != FFELEX_typeMINUS))
    {
      ffelexHandler nexthandler;

      ffeexpr_make_float_const_ (ffesrc_char_internal_init ('E', 'e'),
				 ffeexpr_tokens_[0], ffeexpr_tokens_[1],
				 NULL, NULL, NULL, NULL);

      ffelex_token_kill (ffeexpr_tokens_[0]);
      ffelex_token_kill (ffeexpr_tokens_[1]);
      nexthandler = (ffelexHandler) ffeexpr_token_binary_ (ffeexpr_tokens_[2]);
      ffelex_token_kill (ffeexpr_tokens_[2]);
      return (ffelexHandler) (*nexthandler) (t);
    }

  ffeexpr_tokens_[3] = ffelex_token_use (t);
  return (ffelexHandler) ffeexpr_token_num_per_exp_sign_;
}

/* ffeexpr_token_number_real_ -- Rhs NUMBER PERIOD NUMBER

   Return a pointer to this function to the lexer (ffelex), which will
   invoke it for the next token.

   After a number, period, and number, check next token for possible
   exponent designation (D, E, or Q as first/only character) and continue
   real-number handling accordingly.  Else form basic real constant, push
   onto expression stack, and enter binary state using current token (which,
   if it is a name not beginning with D, E, or Q, will certainly result
   in an error, but that's not for this routine to deal with).	*/

static ffelexHandler
ffeexpr_token_number_real_ (ffelexToken t)
{
  char d;
  const char *p;

  if (((ffelex_token_type (t) != FFELEX_typeNAME)
       && (ffelex_token_type (t) != FFELEX_typeNAMES))
      || !(((ffesrc_char_match_init ((d = *(p = ffelex_token_text (t))),
				     'D', 'd')
	     || ffesrc_char_match_init (d, 'E', 'e')
	     || ffesrc_char_match_init (d, 'Q', 'q')))
	   && ffeexpr_isdigits_ (++p)))
    {
#if 0
      /* This code has been removed because it seems inconsistent to
	 produce a diagnostic in this case, but not all of the other
	 ones that look for an exponent and cannot recognize one.  */
      if (((ffelex_token_type (t) == FFELEX_typeNAME)
	   || (ffelex_token_type (t) == FFELEX_typeNAMES))
	  && ffest_ffebad_start (FFEBAD_INVALID_EXPONENT))
	{
	  char bad[2];

	  ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_here (1, ffelex_token_where_line (ffeexpr_tokens_[0]),
		       ffelex_token_where_column (ffeexpr_tokens_[0]));
	  bad[0] = *(p - 1);
	  bad[1] = '\0';
	  ffebad_string (bad);
	  ffebad_finish ();
	}
#endif
      ffeexpr_make_float_const_ (ffesrc_char_internal_init ('E', 'e'),
				 ffeexpr_tokens_[0], ffeexpr_tokens_[1],
				 ffeexpr_tokens_[2], NULL, NULL, NULL);

      ffelex_token_kill (ffeexpr_tokens_[0]);
      ffelex_token_kill (ffeexpr_tokens_[1]);
      ffelex_token_kill (ffeexpr_tokens_[2]);
      return (ffelexHandler) ffeexpr_token_binary_ (t);
    }

  /* Just exponent character by itself?	 In which case, PLUS or MINUS must
     surely be next, followed by a NUMBER token. */

  if (*p == '\0')
    {
      ffeexpr_tokens_[3] = ffelex_token_use (t);
      return (ffelexHandler) ffeexpr_token_number_real_exp_;
    }

  ffeexpr_make_float_const_ (d, ffeexpr_tokens_[0], ffeexpr_tokens_[1],
			     ffeexpr_tokens_[2], t, NULL, NULL);

  ffelex_token_kill (ffeexpr_tokens_[0]);
  ffelex_token_kill (ffeexpr_tokens_[1]);
  ffelex_token_kill (ffeexpr_tokens_[2]);
  return (ffelexHandler) ffeexpr_token_binary_;
}

/* ffeexpr_token_num_per_exp_sign_ -- Rhs NUMBER PERIOD NAME(D,E,Q) PLUS/MINUS

   Return a pointer to this function to the lexer (ffelex), which will
   invoke it for the next token.

   Make sure token is a NUMBER, make a real constant out of all we have and
   push it onto the expression stack.  Else issue diagnostic and pretend
   exponent field was a zero.  */

static ffelexHandler
ffeexpr_token_num_per_exp_sign_ (ffelexToken t)
{
  if (ffelex_token_type (t) != FFELEX_typeNUMBER)
    {
      if (ffest_ffebad_start (FFEBAD_MISSING_EXPONENT_VALUE))
	{
	  ffebad_here (0, ffelex_token_where_line (ffeexpr_tokens_[2]),
		       ffelex_token_where_column (ffeexpr_tokens_[2]));
	  ffebad_here (1, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_finish ();
	}

      ffeexpr_make_float_const_ (ffesrc_char_internal_init ('E', 'e'),
				 ffeexpr_tokens_[0], ffeexpr_tokens_[1],
				 NULL, NULL, NULL, NULL);

      ffelex_token_kill (ffeexpr_tokens_[0]);
      ffelex_token_kill (ffeexpr_tokens_[1]);
      ffelex_token_kill (ffeexpr_tokens_[2]);
      ffelex_token_kill (ffeexpr_tokens_[3]);
      return (ffelexHandler) ffeexpr_token_binary_ (t);
    }

  ffeexpr_make_float_const_ (ffelex_token_text (ffeexpr_tokens_[2])[0],
			     ffeexpr_tokens_[0], ffeexpr_tokens_[1], NULL,
			     ffeexpr_tokens_[2], ffeexpr_tokens_[3], t);

  ffelex_token_kill (ffeexpr_tokens_[0]);
  ffelex_token_kill (ffeexpr_tokens_[1]);
  ffelex_token_kill (ffeexpr_tokens_[2]);
  ffelex_token_kill (ffeexpr_tokens_[3]);
  return (ffelexHandler) ffeexpr_token_binary_;
}

/* ffeexpr_token_number_real_exp_ -- Rhs NUMBER PERIOD NUMBER NAME(D, E, or Q)

   Return a pointer to this function to the lexer (ffelex), which will
   invoke it for the next token.

   Ensures this token is PLUS or MINUS, preserves it, goes to final state
   for real number (exponent digits).  Else issues diagnostic, assumes a
   zero exponent field for number, passes token on to binary state as if
   previous token had been "E0" instead of "E", for example.  */

static ffelexHandler
ffeexpr_token_number_real_exp_ (ffelexToken t)
{
  if ((ffelex_token_type (t) != FFELEX_typePLUS)
      && (ffelex_token_type (t) != FFELEX_typeMINUS))
    {
      if (ffest_ffebad_start (FFEBAD_MISSING_EXPONENT_VALUE))
	{
	  ffebad_here (0, ffelex_token_where_line (ffeexpr_tokens_[3]),
		       ffelex_token_where_column (ffeexpr_tokens_[3]));
	  ffebad_here (1, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_finish ();
	}

      ffeexpr_make_float_const_ (ffesrc_char_internal_init ('E', 'e'),
				 ffeexpr_tokens_[0], ffeexpr_tokens_[1],
				 ffeexpr_tokens_[2], NULL, NULL, NULL);

      ffelex_token_kill (ffeexpr_tokens_[0]);
      ffelex_token_kill (ffeexpr_tokens_[1]);
      ffelex_token_kill (ffeexpr_tokens_[2]);
      ffelex_token_kill (ffeexpr_tokens_[3]);
      return (ffelexHandler) ffeexpr_token_binary_ (t);
    }

  ffeexpr_tokens_[4] = ffelex_token_use (t);
  return (ffelexHandler) ffeexpr_token_num_real_exp_sn_;
}

/* ffeexpr_token_num_real_exp_sn_ -- Rhs NUMBER PERIOD NUMBER NAME(D,E,Q)
				  PLUS/MINUS

   Return a pointer to this function to the lexer (ffelex), which will
   invoke it for the next token.

   Make sure token is a NUMBER, make a real constant out of all we have and
   push it onto the expression stack.  Else issue diagnostic and pretend
   exponent field was a zero.  */

static ffelexHandler
ffeexpr_token_num_real_exp_sn_ (ffelexToken t)
{
  if (ffelex_token_type (t) != FFELEX_typeNUMBER)
    {
      if (ffest_ffebad_start (FFEBAD_MISSING_EXPONENT_VALUE))
	{
	  ffebad_here (0, ffelex_token_where_line (ffeexpr_tokens_[3]),
		       ffelex_token_where_column (ffeexpr_tokens_[3]));
	  ffebad_here (1, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_finish ();
	}

      ffeexpr_make_float_const_ (ffesrc_char_internal_init ('E', 'e'),
				 ffeexpr_tokens_[0], ffeexpr_tokens_[1],
				 ffeexpr_tokens_[2], NULL, NULL, NULL);

      ffelex_token_kill (ffeexpr_tokens_[0]);
      ffelex_token_kill (ffeexpr_tokens_[1]);
      ffelex_token_kill (ffeexpr_tokens_[2]);
      ffelex_token_kill (ffeexpr_tokens_[3]);
      ffelex_token_kill (ffeexpr_tokens_[4]);
      return (ffelexHandler) ffeexpr_token_binary_ (t);
    }

  ffeexpr_make_float_const_ (ffelex_token_text (ffeexpr_tokens_[3])[0],
			     ffeexpr_tokens_[0], ffeexpr_tokens_[1],
			     ffeexpr_tokens_[2], ffeexpr_tokens_[3],
			     ffeexpr_tokens_[4], t);

  ffelex_token_kill (ffeexpr_tokens_[0]);
  ffelex_token_kill (ffeexpr_tokens_[1]);
  ffelex_token_kill (ffeexpr_tokens_[2]);
  ffelex_token_kill (ffeexpr_tokens_[3]);
  ffelex_token_kill (ffeexpr_tokens_[4]);
  return (ffelexHandler) ffeexpr_token_binary_;
}

/* ffeexpr_token_binary_ -- Handle binary operator possibility

   Return a pointer to this function to the lexer (ffelex), which will
   invoke it for the next token.

   The possibility of a binary operator is handled here, meaning the previous
   token was an operand.  */

static ffelexHandler
ffeexpr_token_binary_ (ffelexToken t)
{
  ffeexprExpr_ e;

  if (!ffeexpr_stack_->is_rhs)
    return (ffelexHandler) ffeexpr_finished_ (t);	/* For now. */

  switch (ffelex_token_type (t))
    {
    case FFELEX_typePLUS:
      e = ffeexpr_expr_new_ ();
      e->type = FFEEXPR_exprtypeBINARY_;
      e->token = ffelex_token_use (t);
      e->u.operator.op = FFEEXPR_operatorADD_;
      e->u.operator.prec = FFEEXPR_operatorprecedenceADD_;
      e->u.operator.as = FFEEXPR_operatorassociativityADD_;
      ffeexpr_exprstack_push_binary_ (e);
      return (ffelexHandler) ffeexpr_token_rhs_;

    case FFELEX_typeMINUS:
      e = ffeexpr_expr_new_ ();
      e->type = FFEEXPR_exprtypeBINARY_;
      e->token = ffelex_token_use (t);
      e->u.operator.op = FFEEXPR_operatorSUBTRACT_;
      e->u.operator.prec = FFEEXPR_operatorprecedenceSUBTRACT_;
      e->u.operator.as = FFEEXPR_operatorassociativitySUBTRACT_;
      ffeexpr_exprstack_push_binary_ (e);
      return (ffelexHandler) ffeexpr_token_rhs_;

    case FFELEX_typeASTERISK:
      switch (ffeexpr_stack_->context)
	{
	case FFEEXPR_contextDATA:
	  return (ffelexHandler) ffeexpr_finished_ (t);

	default:
	  break;
	}
      e = ffeexpr_expr_new_ ();
      e->type = FFEEXPR_exprtypeBINARY_;
      e->token = ffelex_token_use (t);
      e->u.operator.op = FFEEXPR_operatorMULTIPLY_;
      e->u.operator.prec = FFEEXPR_operatorprecedenceMULTIPLY_;
      e->u.operator.as = FFEEXPR_operatorassociativityMULTIPLY_;
      ffeexpr_exprstack_push_binary_ (e);
      return (ffelexHandler) ffeexpr_token_rhs_;

    case FFELEX_typeSLASH:
      switch (ffeexpr_stack_->context)
	{
	case FFEEXPR_contextDATA:
	  return (ffelexHandler) ffeexpr_finished_ (t);

	default:
	  break;
	}
      e = ffeexpr_expr_new_ ();
      e->type = FFEEXPR_exprtypeBINARY_;
      e->token = ffelex_token_use (t);
      e->u.operator.op = FFEEXPR_operatorDIVIDE_;
      e->u.operator.prec = FFEEXPR_operatorprecedenceDIVIDE_;
      e->u.operator.as = FFEEXPR_operatorassociativityDIVIDE_;
      ffeexpr_exprstack_push_binary_ (e);
      return (ffelexHandler) ffeexpr_token_rhs_;

    case FFELEX_typePOWER:
      e = ffeexpr_expr_new_ ();
      e->type = FFEEXPR_exprtypeBINARY_;
      e->token = ffelex_token_use (t);
      e->u.operator.op = FFEEXPR_operatorPOWER_;
      e->u.operator.prec = FFEEXPR_operatorprecedencePOWER_;
      e->u.operator.as = FFEEXPR_operatorassociativityPOWER_;
      ffeexpr_exprstack_push_binary_ (e);
      return (ffelexHandler) ffeexpr_token_rhs_;

    case FFELEX_typeCONCAT:
      e = ffeexpr_expr_new_ ();
      e->type = FFEEXPR_exprtypeBINARY_;
      e->token = ffelex_token_use (t);
      e->u.operator.op = FFEEXPR_operatorCONCATENATE_;
      e->u.operator.prec = FFEEXPR_operatorprecedenceCONCATENATE_;
      e->u.operator.as = FFEEXPR_operatorassociativityCONCATENATE_;
      ffeexpr_exprstack_push_binary_ (e);
      return (ffelexHandler) ffeexpr_token_rhs_;

    case FFELEX_typeOPEN_ANGLE:
      switch (ffeexpr_stack_->context)
	{
	case FFEEXPR_contextFORMAT:
	  ffebad_start (FFEBAD_FORMAT_EXPR_TOKEN);
	  ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_finish ();
	  break;

	default:
	  break;
	}
      e = ffeexpr_expr_new_ ();
      e->type = FFEEXPR_exprtypeBINARY_;
      e->token = ffelex_token_use (t);
      e->u.operator.op = FFEEXPR_operatorLT_;
      e->u.operator.prec = FFEEXPR_operatorprecedenceLT_;
      e->u.operator.as = FFEEXPR_operatorassociativityLT_;
      ffeexpr_exprstack_push_binary_ (e);
      return (ffelexHandler) ffeexpr_token_rhs_;

    case FFELEX_typeCLOSE_ANGLE:
      switch (ffeexpr_stack_->context)
	{
	case FFEEXPR_contextFORMAT:
	  return ffeexpr_finished_ (t);

	default:
	  break;
	}
      e = ffeexpr_expr_new_ ();
      e->type = FFEEXPR_exprtypeBINARY_;
      e->token = ffelex_token_use (t);
      e->u.operator.op = FFEEXPR_operatorGT_;
      e->u.operator.prec = FFEEXPR_operatorprecedenceGT_;
      e->u.operator.as = FFEEXPR_operatorassociativityGT_;
      ffeexpr_exprstack_push_binary_ (e);
      return (ffelexHandler) ffeexpr_token_rhs_;

    case FFELEX_typeREL_EQ:
      switch (ffeexpr_stack_->context)
	{
	case FFEEXPR_contextFORMAT:
	  ffebad_start (FFEBAD_FORMAT_EXPR_TOKEN);
	  ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_finish ();
	  break;

	default:
	  break;
	}
      e = ffeexpr_expr_new_ ();
      e->type = FFEEXPR_exprtypeBINARY_;
      e->token = ffelex_token_use (t);
      e->u.operator.op = FFEEXPR_operatorEQ_;
      e->u.operator.prec = FFEEXPR_operatorprecedenceEQ_;
      e->u.operator.as = FFEEXPR_operatorassociativityEQ_;
      ffeexpr_exprstack_push_binary_ (e);
      return (ffelexHandler) ffeexpr_token_rhs_;

    case FFELEX_typeREL_NE:
      switch (ffeexpr_stack_->context)
	{
	case FFEEXPR_contextFORMAT:
	  ffebad_start (FFEBAD_FORMAT_EXPR_TOKEN);
	  ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_finish ();
	  break;

	default:
	  break;
	}
      e = ffeexpr_expr_new_ ();
      e->type = FFEEXPR_exprtypeBINARY_;
      e->token = ffelex_token_use (t);
      e->u.operator.op = FFEEXPR_operatorNE_;
      e->u.operator.prec = FFEEXPR_operatorprecedenceNE_;
      e->u.operator.as = FFEEXPR_operatorassociativityNE_;
      ffeexpr_exprstack_push_binary_ (e);
      return (ffelexHandler) ffeexpr_token_rhs_;

    case FFELEX_typeREL_LE:
      switch (ffeexpr_stack_->context)
	{
	case FFEEXPR_contextFORMAT:
	  ffebad_start (FFEBAD_FORMAT_EXPR_TOKEN);
	  ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_finish ();
	  break;

	default:
	  break;
	}
      e = ffeexpr_expr_new_ ();
      e->type = FFEEXPR_exprtypeBINARY_;
      e->token = ffelex_token_use (t);
      e->u.operator.op = FFEEXPR_operatorLE_;
      e->u.operator.prec = FFEEXPR_operatorprecedenceLE_;
      e->u.operator.as = FFEEXPR_operatorassociativityLE_;
      ffeexpr_exprstack_push_binary_ (e);
      return (ffelexHandler) ffeexpr_token_rhs_;

    case FFELEX_typeREL_GE:
      switch (ffeexpr_stack_->context)
	{
	case FFEEXPR_contextFORMAT:
	  ffebad_start (FFEBAD_FORMAT_EXPR_TOKEN);
	  ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_finish ();
	  break;

	default:
	  break;
	}
      e = ffeexpr_expr_new_ ();
      e->type = FFEEXPR_exprtypeBINARY_;
      e->token = ffelex_token_use (t);
      e->u.operator.op = FFEEXPR_operatorGE_;
      e->u.operator.prec = FFEEXPR_operatorprecedenceGE_;
      e->u.operator.as = FFEEXPR_operatorassociativityGE_;
      ffeexpr_exprstack_push_binary_ (e);
      return (ffelexHandler) ffeexpr_token_rhs_;

    case FFELEX_typePERIOD:
      ffeexpr_tokens_[0] = ffelex_token_use (t);
      return (ffelexHandler) ffeexpr_token_binary_period_;

#if 0
    case FFELEX_typeOPEN_PAREN:
    case FFELEX_typeCLOSE_PAREN:
    case FFELEX_typeEQUALS:
    case FFELEX_typePOINTS:
    case FFELEX_typeCOMMA:
    case FFELEX_typeCOLON:
    case FFELEX_typeEOS:
    case FFELEX_typeSEMICOLON:
    case FFELEX_typeNAME:
    case FFELEX_typeNAMES:
#endif
    default:
      return (ffelexHandler) ffeexpr_finished_ (t);
    }
}

/* ffeexpr_token_binary_period_ -- Binary PERIOD

   Return a pointer to this function to the lexer (ffelex), which will
   invoke it for the next token.

   Handle a period detected at binary (expecting binary op or end) state.
   Must begin a dot-dot name, of which .NOT., .TRUE., and .FALSE. are not
   valid.  */

static ffelexHandler
ffeexpr_token_binary_period_ (ffelexToken t)
{
  ffeexprExpr_ operand;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
    case FFELEX_typeNAMES:
      ffeexpr_current_dotdot_ = ffestr_other (t);
      switch (ffeexpr_current_dotdot_)
	{
	case FFESTR_otherTRUE:
	case FFESTR_otherFALSE:
	case FFESTR_otherNOT:
	  if (ffest_ffebad_start (FFEBAD_MISSING_BINARY_OPERATOR))
	    {
	      operand = ffeexpr_stack_->exprstack;
	      assert (operand != NULL);
	      assert (operand->type == FFEEXPR_exprtypeOPERAND_);
	      ffebad_here (0, ffelex_token_where_line (operand->token), ffelex_token_where_column (operand->token));
	      ffebad_here (1, ffelex_token_where_line (t),
			   ffelex_token_where_column (t));
	      ffebad_finish ();
	    }
	  ffelex_token_kill (ffeexpr_tokens_[0]);
	  return (ffelexHandler) ffeexpr_token_binary_sw_per_;

	default:
	  ffeexpr_tokens_[1] = ffelex_token_use (t);
	  return (ffelexHandler) ffeexpr_token_binary_end_per_;
	}
      break;			/* Nothing really reaches here. */

    default:
      if (ffest_ffebad_start (FFEBAD_IGNORING_PERIOD))
	{
	  ffebad_here (0, ffelex_token_where_line (ffeexpr_tokens_[0]),
		       ffelex_token_where_column (ffeexpr_tokens_[0]));
	  ffebad_finish ();
	}
      ffelex_token_kill (ffeexpr_tokens_[0]);
      return (ffelexHandler) ffeexpr_token_binary_ (t);
    }
}

/* ffeexpr_token_binary_end_per_ -- Binary PERIOD NAME(not NOT, TRUE, or FALSE)

   Return a pointer to this function to the lexer (ffelex), which will
   invoke it for the next token.

   Expecting a period to close a dot-dot at binary (binary op
   or operator) state.	If period isn't found, issue a diagnostic but
   pretend we saw one.	ffeexpr_current_dotdot_ must already contained the
   dotdot representation of the name in between the two PERIOD tokens.	*/

static ffelexHandler
ffeexpr_token_binary_end_per_ (ffelexToken t)
{
  ffeexprExpr_ e;

  e = ffeexpr_expr_new_ ();
  e->type = FFEEXPR_exprtypeBINARY_;
  e->token = ffeexpr_tokens_[0];

  switch (ffeexpr_current_dotdot_)
    {
    case FFESTR_otherAND:
      e->u.operator.op = FFEEXPR_operatorAND_;
      e->u.operator.prec = FFEEXPR_operatorprecedenceAND_;
      e->u.operator.as = FFEEXPR_operatorassociativityAND_;
      break;

    case FFESTR_otherOR:
      e->u.operator.op = FFEEXPR_operatorOR_;
      e->u.operator.prec = FFEEXPR_operatorprecedenceOR_;
      e->u.operator.as = FFEEXPR_operatorassociativityOR_;
      break;

    case FFESTR_otherXOR:
      e->u.operator.op = FFEEXPR_operatorXOR_;
      e->u.operator.prec = FFEEXPR_operatorprecedenceXOR_;
      e->u.operator.as = FFEEXPR_operatorassociativityXOR_;
      break;

    case FFESTR_otherEQV:
      e->u.operator.op = FFEEXPR_operatorEQV_;
      e->u.operator.prec = FFEEXPR_operatorprecedenceEQV_;
      e->u.operator.as = FFEEXPR_operatorassociativityEQV_;
      break;

    case FFESTR_otherNEQV:
      e->u.operator.op = FFEEXPR_operatorNEQV_;
      e->u.operator.prec = FFEEXPR_operatorprecedenceNEQV_;
      e->u.operator.as = FFEEXPR_operatorassociativityNEQV_;
      break;

    case FFESTR_otherLT:
      e->u.operator.op = FFEEXPR_operatorLT_;
      e->u.operator.prec = FFEEXPR_operatorprecedenceLT_;
      e->u.operator.as = FFEEXPR_operatorassociativityLT_;
      break;

    case FFESTR_otherLE:
      e->u.operator.op = FFEEXPR_operatorLE_;
      e->u.operator.prec = FFEEXPR_operatorprecedenceLE_;
      e->u.operator.as = FFEEXPR_operatorassociativityLE_;
      break;

    case FFESTR_otherEQ:
      e->u.operator.op = FFEEXPR_operatorEQ_;
      e->u.operator.prec = FFEEXPR_operatorprecedenceEQ_;
      e->u.operator.as = FFEEXPR_operatorassociativityEQ_;
      break;

    case FFESTR_otherNE:
      e->u.operator.op = FFEEXPR_operatorNE_;
      e->u.operator.prec = FFEEXPR_operatorprecedenceNE_;
      e->u.operator.as = FFEEXPR_operatorassociativityNE_;
      break;

    case FFESTR_otherGT:
      e->u.operator.op = FFEEXPR_operatorGT_;
      e->u.operator.prec = FFEEXPR_operatorprecedenceGT_;
      e->u.operator.as = FFEEXPR_operatorassociativityGT_;
      break;

    case FFESTR_otherGE:
      e->u.operator.op = FFEEXPR_operatorGE_;
      e->u.operator.prec = FFEEXPR_operatorprecedenceGE_;
      e->u.operator.as = FFEEXPR_operatorassociativityGE_;
      break;

    default:
      if (ffest_ffebad_start (FFEBAD_INVALID_DOTDOT))
	{
	  ffebad_here (0, ffelex_token_where_line (ffeexpr_tokens_[0]),
		       ffelex_token_where_column (ffeexpr_tokens_[0]));
	  ffebad_string (ffelex_token_text (ffeexpr_tokens_[1]));
	  ffebad_finish ();
	}
      e->u.operator.op = FFEEXPR_operatorEQ_;
      e->u.operator.prec = FFEEXPR_operatorprecedenceEQ_;
      e->u.operator.as = FFEEXPR_operatorassociativityEQ_;
      break;
    }

  ffeexpr_exprstack_push_binary_ (e);

  if (ffelex_token_type (t) != FFELEX_typePERIOD)
    {
      if (ffest_ffebad_start (FFEBAD_INSERTING_PERIOD))
	{
	  ffebad_here (0, ffelex_token_where_line (ffeexpr_tokens_[0]),
		       ffelex_token_where_column (ffeexpr_tokens_[0]));
	  ffebad_here (1, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_string (ffelex_token_text (ffeexpr_tokens_[1]));
	  ffebad_finish ();
	}
      ffelex_token_kill (ffeexpr_tokens_[1]);	/* Kill dot-dot token. */
      return (ffelexHandler) ffeexpr_token_rhs_ (t);
    }

  ffelex_token_kill (ffeexpr_tokens_[1]);	/* Kill dot-dot token. */
  return (ffelexHandler) ffeexpr_token_rhs_;
}

/* ffeexpr_token_binary_sw_per_ -- Rhs PERIOD NAME(NOT, TRUE, or FALSE)

   Return a pointer to this function to the lexer (ffelex), which will
   invoke it for the next token.

   A diagnostic has already been issued; just swallow a period if there is
   one, then continue with ffeexpr_token_binary_.  */

static ffelexHandler
ffeexpr_token_binary_sw_per_ (ffelexToken t)
{
  if (ffelex_token_type (t) != FFELEX_typePERIOD)
    return (ffelexHandler) ffeexpr_token_binary_ (t);

  return (ffelexHandler) ffeexpr_token_binary_;
}

/* ffeexpr_token_quote_ -- Rhs QUOTE

   Return a pointer to this function to the lexer (ffelex), which will
   invoke it for the next token.

   Expecting a NUMBER that we'll treat as an octal integer.  */

static ffelexHandler
ffeexpr_token_quote_ (ffelexToken t)
{
  ffeexprExpr_ e;
  ffebld anyexpr;

  if (ffelex_token_type (t) != FFELEX_typeNUMBER)
    {
      if (ffest_ffebad_start (FFEBAD_QUOTE_MISSES_DIGITS))
	{
	  ffebad_here (0, ffelex_token_where_line (ffeexpr_tokens_[0]),
		       ffelex_token_where_column (ffeexpr_tokens_[0]));
	  ffebad_here (1, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_finish ();
	}
      ffelex_token_kill (ffeexpr_tokens_[0]);
      return (ffelexHandler) ffeexpr_token_rhs_ (t);
    }

  /* This is kind of a kludge to prevent any whining about magical numbers
     that start out as these octal integers, so "20000000000 (on a 32-bit
     2's-complement machine) by itself won't produce an error. */

  anyexpr = ffebld_new_any ();
  ffebld_set_info (anyexpr, ffeinfo_new_any ());

  e = ffeexpr_expr_new_ ();
  e->type = FFEEXPR_exprtypeOPERAND_;
  e->token = ffeexpr_tokens_[0];
  e->u.operand = ffebld_new_conter_with_orig
    (ffebld_constant_new_integeroctal (t), anyexpr);
  ffebld_set_info (e->u.operand, ffeinfo_new (FFEINFO_basictypeINTEGER,
		      FFEINFO_kindtypeINTEGERDEFAULT, 0, FFEINFO_kindENTITY,
		       FFEINFO_whereCONSTANT, FFETARGET_charactersizeNONE));
  ffeexpr_exprstack_push_operand_ (e);
  return (ffelexHandler) ffeexpr_token_binary_;
}

/* ffeexpr_token_apostrophe_ -- Rhs APOSTROPHE

   Return a pointer to this function to the lexer (ffelex), which will
   invoke it for the next token.

   Handle an open-apostrophe, which begins either a character ('char-const'),
   typeless octal ('octal-const'O), or typeless hexadecimal ('hex-const'Z or
   'hex-const'X) constant.  */

static ffelexHandler
ffeexpr_token_apostrophe_ (ffelexToken t)
{
  assert (ffelex_token_type (t) == FFELEX_typeCHARACTER);
  if (ffe_is_pedantic_not_90 () && (ffelex_token_length (t) == 0))
    {
      ffebad_start (FFEBAD_NULL_CHAR_CONST);
      ffebad_here (0, ffelex_token_where_line (t),
		   ffelex_token_where_column (t));
      ffebad_finish ();
    }
  ffeexpr_tokens_[1] = ffelex_token_use (t);
  return (ffelexHandler) ffeexpr_token_apos_char_;
}

/* ffeexpr_token_apos_char_ -- Rhs APOSTROPHE CHARACTER

   Return a pointer to this function to the lexer (ffelex), which will
   invoke it for the next token.

   Close-apostrophe is implicit; if this token is NAME, it is a possible
   typeless-constant radix specifier.  */

static ffelexHandler
ffeexpr_token_apos_char_ (ffelexToken t)
{
  ffeexprExpr_ e;
  ffeinfo ni;
  char c;
  ffetargetCharacterSize size;

  if ((ffelex_token_type (t) == FFELEX_typeNAME)
      || (ffelex_token_type (t) == FFELEX_typeNAMES))
    {
      if ((ffelex_token_length (t) == 1)
	  && (ffesrc_char_match_init ((c = ffelex_token_text (t)[0]), 'B',
				      'b')
	      || ffesrc_char_match_init (c, 'O', 'o')
	      || ffesrc_char_match_init (c, 'X', 'x')
	      || ffesrc_char_match_init (c, 'Z', 'z')))
	{
	  e = ffeexpr_expr_new_ ();
	  e->type = FFEEXPR_exprtypeOPERAND_;
	  e->token = ffeexpr_tokens_[0];
	  switch (c)
	    {
	    case FFESRC_CASE_MATCH_INIT ('B', 'b', match_b, no_match):
	      e->u.operand = ffebld_new_conter
		(ffebld_constant_new_typeless_bv (ffeexpr_tokens_[1]));
	      size = ffetarget_size_typeless_binary (ffeexpr_tokens_[1]);
	      break;

	    case FFESRC_CASE_MATCH_INIT ('O', 'o', match_o, no_match):
	      e->u.operand = ffebld_new_conter
		(ffebld_constant_new_typeless_ov (ffeexpr_tokens_[1]));
	      size = ffetarget_size_typeless_octal (ffeexpr_tokens_[1]);
	      break;

	    case FFESRC_CASE_MATCH_INIT ('X', 'x', match_x, no_match):
	      e->u.operand = ffebld_new_conter
		(ffebld_constant_new_typeless_hxv (ffeexpr_tokens_[1]));
	      size = ffetarget_size_typeless_hex (ffeexpr_tokens_[1]);
	      break;

	    case FFESRC_CASE_MATCH_INIT ('Z', 'z', match_z, no_match):
	      e->u.operand = ffebld_new_conter
		(ffebld_constant_new_typeless_hzv (ffeexpr_tokens_[1]));
	      size = ffetarget_size_typeless_hex (ffeexpr_tokens_[1]);
	      break;

	    default:
	    no_match:		/* :::::::::::::::::::: */
	      assert ("not BOXZ!" == NULL);
	      size = 0;
	      break;
	    }
	  ffebld_set_info (e->u.operand,
	       ffeinfo_new (FFEINFO_basictypeTYPELESS, FFEINFO_kindtypeNONE,
		       0, FFEINFO_kindENTITY, FFEINFO_whereCONSTANT, size));
	  ffeexpr_exprstack_push_operand_ (e);
	  ffelex_token_kill (ffeexpr_tokens_[1]);
	  return (ffelexHandler) ffeexpr_token_binary_;
	}
    }
  e = ffeexpr_expr_new_ ();
  e->type = FFEEXPR_exprtypeOPERAND_;
  e->token = ffeexpr_tokens_[0];
  e->u.operand = ffebld_new_conter (ffebld_constant_new_characterdefault
				    (ffeexpr_tokens_[1]));
  ni = ffeinfo_new (FFEINFO_basictypeCHARACTER, FFEINFO_kindtypeCHARACTERDEFAULT,
		    0, FFEINFO_kindENTITY, FFEINFO_whereCONSTANT,
		    ffelex_token_length (ffeexpr_tokens_[1]));
  ffebld_set_info (e->u.operand, ni);
  ffelex_token_kill (ffeexpr_tokens_[1]);
  ffeexpr_exprstack_push_operand_ (e);
  if ((ffelex_token_type (t) == FFELEX_typeNAME)
      || (ffelex_token_type (t) == FFELEX_typeNAMES))
    {
      if (ffest_ffebad_start (FFEBAD_INVALID_RADIX_SPECIFIER))
	{
	  ffebad_string (ffelex_token_text (t));
	  ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_here (1, ffelex_token_where_line (ffeexpr_tokens_[0]),
		       ffelex_token_where_column (ffeexpr_tokens_[0]));
	  ffebad_finish ();
	}
      e = ffeexpr_expr_new_ ();
      e->type = FFEEXPR_exprtypeBINARY_;
      e->token = ffelex_token_use (t);
      e->u.operator.op = FFEEXPR_operatorCONCATENATE_;
      e->u.operator.prec = FFEEXPR_operatorprecedenceCONCATENATE_;
      e->u.operator.as = FFEEXPR_operatorassociativityCONCATENATE_;
      ffeexpr_exprstack_push_binary_ (e);
      return (ffelexHandler) ffeexpr_token_rhs_ (t);
    }
  ffeexpr_is_substr_ok_ = !ffe_is_pedantic_not_90 ();	/* Allow "'hello'(3:5)". */
  return (ffelexHandler) ffeexpr_token_substrp_ (t);
}

/* ffeexpr_token_name_lhs_ -- Lhs NAME

   Return a pointer to this function to the lexer (ffelex), which will
   invoke it for the next token.

   Handle a name followed by open-paren, period (RECORD.MEMBER), percent
   (RECORD%MEMBER), or nothing at all.	*/

static ffelexHandler
ffeexpr_token_name_lhs_ (ffelexToken t)
{
  ffeexprExpr_ e;
  ffeexprParenType_ paren_type;
  ffesymbol s;
  ffebld expr;
  ffeinfo info;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeOPEN_PAREN:
      switch (ffeexpr_stack_->context)
	{
	case FFEEXPR_contextASSIGN:
	case FFEEXPR_contextAGOTO:
	case FFEEXPR_contextFILEUNIT_DF:
	  goto just_name;	/* :::::::::::::::::::: */

	default:
	  break;
	}
      e = ffeexpr_expr_new_ ();
      e->type = FFEEXPR_exprtypeOPERAND_;
      e->token = ffelex_token_use (ffeexpr_tokens_[0]);
      s = ffeexpr_declare_parenthesized_ (ffeexpr_tokens_[0], FALSE,
					  &paren_type);

      switch (ffesymbol_where (s))
	{
	case FFEINFO_whereLOCAL:
	  if (ffeexpr_stack_->context == FFEEXPR_contextSUBROUTINEREF)
	    ffesymbol_error (s, ffeexpr_tokens_[0]);	/* Recursion. */
	  break;

	case FFEINFO_whereINTRINSIC:
	case FFEINFO_whereGLOBAL:
	  if (ffeexpr_stack_->context != FFEEXPR_contextSUBROUTINEREF)
	    ffesymbol_error (s, ffeexpr_tokens_[0]);	/* Can call intrin. */
	  break;

	case FFEINFO_whereCOMMON:
	case FFEINFO_whereDUMMY:
	case FFEINFO_whereRESULT:
	  break;

	case FFEINFO_whereNONE:
	case FFEINFO_whereANY:
	  break;

	default:
	  ffesymbol_error (s, ffeexpr_tokens_[0]);
	  break;
	}

      if (ffesymbol_attrs (s) & FFESYMBOL_attrsANY)
	{
	  e->u.operand = ffebld_new_any ();
	  ffebld_set_info (e->u.operand, ffeinfo_new_any ());
	}
      else
	{
	  e->u.operand = ffebld_new_symter (s,
					    ffesymbol_generic (s),
					    ffesymbol_specific (s),
					    ffesymbol_implementation (s));
	  ffebld_set_info (e->u.operand, ffesymbol_info (s));
	}
      ffeexpr_exprstack_push_ (e);	/* Not a complete operand yet. */
      ffeexpr_stack_->tokens[0] = ffeexpr_tokens_[0];
      switch (paren_type)
	{
	case FFEEXPR_parentypeSUBROUTINE_:
	  ffebld_init_list (&ffeexpr_stack_->expr, &ffeexpr_stack_->bottom);
	  return
	    (ffelexHandler)
	    ffeexpr_rhs (ffeexpr_stack_->pool,
			 FFEEXPR_contextACTUALARG_,
			 ffeexpr_token_arguments_);

	case FFEEXPR_parentypeARRAY_:
	  ffebld_init_list (&ffeexpr_stack_->expr, &ffeexpr_stack_->bottom);
	  ffeexpr_stack_->bound_list = ffesymbol_dims (s);
	  ffeexpr_stack_->rank = 0;
	  ffeexpr_stack_->constant = TRUE;
	  ffeexpr_stack_->immediate = TRUE;
	  switch (ffeexpr_stack_->context)
	    {
	    case FFEEXPR_contextDATAIMPDOITEM_:
	      return
		(ffelexHandler)
		ffeexpr_rhs (ffeexpr_stack_->pool,
			     FFEEXPR_contextDATAIMPDOINDEX_,
			     ffeexpr_token_elements_);

	    case FFEEXPR_contextEQUIVALENCE:
	      return
		(ffelexHandler)
		ffeexpr_rhs (ffeexpr_stack_->pool,
			     FFEEXPR_contextEQVINDEX_,
			     ffeexpr_token_elements_);

	    default:
	      return
		(ffelexHandler)
		ffeexpr_rhs (ffeexpr_stack_->pool,
			     FFEEXPR_contextINDEX_,
			     ffeexpr_token_elements_);
	    }

	case FFEEXPR_parentypeSUBSTRING_:
	  e->u.operand = ffeexpr_collapse_symter (e->u.operand,
						  ffeexpr_tokens_[0]);
	  return
	    (ffelexHandler)
	    ffeexpr_rhs (ffeexpr_stack_->pool,
			 FFEEXPR_contextINDEX_,
			 ffeexpr_token_substring_);

	case FFEEXPR_parentypeEQUIVALENCE_:
	  ffebld_init_list (&ffeexpr_stack_->expr, &ffeexpr_stack_->bottom);
	  ffeexpr_stack_->bound_list = ffesymbol_dims (s);
	  ffeexpr_stack_->rank = 0;
	  ffeexpr_stack_->constant = TRUE;
	  ffeexpr_stack_->immediate = TRUE;
	  return
	    (ffelexHandler)
	    ffeexpr_rhs (ffeexpr_stack_->pool,
			 FFEEXPR_contextEQVINDEX_,
			 ffeexpr_token_equivalence_);

	case FFEEXPR_parentypeFUNCTION_:	/* Invalid case. */
	case FFEEXPR_parentypeFUNSUBSTR_:	/* Invalid case. */
	  ffesymbol_error (s, ffeexpr_tokens_[0]);
	  /* Fall through. */
	case FFEEXPR_parentypeANY_:
	  e->u.operand = ffebld_new_any ();
	  ffebld_set_info (e->u.operand, ffeinfo_new_any ());
	  return
	    (ffelexHandler)
	    ffeexpr_rhs (ffeexpr_stack_->pool,
			 FFEEXPR_contextACTUALARG_,
			 ffeexpr_token_anything_);

	default:
	  assert ("bad paren type" == NULL);
	  break;
	}

    case FFELEX_typeEQUALS:	/* As in "VAR=". */
      switch (ffeexpr_stack_->context)
	{
	case FFEEXPR_contextIMPDOITEM_:	/* within
						   "(,VAR=start,end[,incr])". */
	case FFEEXPR_contextIMPDOITEMDF_:
	  ffeexpr_stack_->context = FFEEXPR_contextIMPDOCTRL_;
	  break;

	case FFEEXPR_contextDATAIMPDOITEM_:
	  ffeexpr_stack_->context = FFEEXPR_contextDATAIMPDOCTRL_;
	  break;

	default:
	  break;
	}
      break;

#if 0
    case FFELEX_typePERIOD:
    case FFELEX_typePERCENT:
      assert ("FOO%, FOO. not yet supported!~~" == NULL);
      break;
#endif

    default:
      break;
    }

just_name:			/* :::::::::::::::::::: */
  e = ffeexpr_expr_new_ ();
  e->type = FFEEXPR_exprtypeOPERAND_;
  e->token = ffeexpr_tokens_[0];
  s = ffeexpr_declare_unadorned_ (ffeexpr_tokens_[0],
				  (ffeexpr_stack_->context
				   == FFEEXPR_contextSUBROUTINEREF));

  switch (ffesymbol_where (s))
    {
    case FFEINFO_whereCONSTANT:
      if ((ffeexpr_stack_->context != FFEEXPR_contextPARAMETER)
	  || (ffesymbol_kind (s) != FFEINFO_kindENTITY))
	ffesymbol_error (s, ffeexpr_tokens_[0]);
      break;

    case FFEINFO_whereIMMEDIATE:
      if ((ffeexpr_stack_->context != FFEEXPR_contextDATAIMPDOCTRL_)
	  && (ffeexpr_stack_->context != FFEEXPR_contextDATAIMPDOINDEX_))
	ffesymbol_error (s, ffeexpr_tokens_[0]);
      break;

    case FFEINFO_whereLOCAL:
      if (ffeexpr_stack_->context == FFEEXPR_contextSUBROUTINEREF)
	ffesymbol_error (s, ffeexpr_tokens_[0]);	/* Recurse!. */
      break;

    case FFEINFO_whereINTRINSIC:
      if (ffeexpr_stack_->context != FFEEXPR_contextSUBROUTINEREF)
	ffesymbol_error (s, ffeexpr_tokens_[0]);	/* Can call intrin. */
      break;

    default:
      break;
    }

  if (ffesymbol_attrs (s) & FFESYMBOL_attrsANY)
    {
      expr = ffebld_new_any ();
      info = ffeinfo_new_any ();
      ffebld_set_info (expr, info);
    }
  else
    {
      expr = ffebld_new_symter (s,
				ffesymbol_generic (s),
				ffesymbol_specific (s),
				ffesymbol_implementation (s));
      info = ffesymbol_info (s);
      ffebld_set_info (expr, info);
      if (ffesymbol_is_doiter (s))
	{
	  ffebad_start (FFEBAD_DOITER);
	  ffebad_here (0, ffelex_token_where_line (ffeexpr_tokens_[0]),
		       ffelex_token_where_column (ffeexpr_tokens_[0]));
	  ffest_ffebad_here_doiter (1, s);
	  ffebad_string (ffesymbol_text (s));
	  ffebad_finish ();
	}
      expr = ffeexpr_collapse_symter (expr, ffeexpr_tokens_[0]);
    }

  if (ffeexpr_stack_->context == FFEEXPR_contextSUBROUTINEREF)
    {
      if (ffebld_op (expr) == FFEBLD_opANY)
	{
	  expr = ffebld_new_any ();
	  ffebld_set_info (expr, ffeinfo_new_any ());
	}
      else
	{
	  expr = ffebld_new_subrref (expr, NULL);	/* No argument list. */
	  if (ffesymbol_generic (s) != FFEINTRIN_genNONE)
	    ffeintrin_fulfill_generic (&expr, &info, e->token);
	  else if (ffesymbol_specific (s) != FFEINTRIN_specNONE)
	    ffeintrin_fulfill_specific (&expr, &info, NULL, e->token);
	  else
	    ffeexpr_fulfill_call_ (&expr, e->token);

	  if (ffebld_op (expr) != FFEBLD_opANY)
	    ffebld_set_info (expr,
			     ffeinfo_new (ffeinfo_basictype (info),
					  ffeinfo_kindtype (info),
					  0,
					  FFEINFO_kindENTITY,
					  FFEINFO_whereFLEETING,
					  ffeinfo_size (info)));
	  else
	    ffebld_set_info (expr, ffeinfo_new_any ());
	}
    }

  e->u.operand = expr;
  ffeexpr_exprstack_push_operand_ (e);
  return (ffelexHandler) ffeexpr_finished_ (t);
}

/* ffeexpr_token_name_arg_ -- Rhs NAME

   Return a pointer to this function to the lexer (ffelex), which will
   invoke it for the next token.

   Handle first token in an actual-arg (or possible actual-arg) context
   being a NAME, and use second token to refine the context.  */

static ffelexHandler
ffeexpr_token_name_arg_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCLOSE_PAREN:
    case FFELEX_typeCOMMA:
      switch (ffeexpr_stack_->context)
	{
	case FFEEXPR_contextINDEXORACTUALARG_:
	  ffeexpr_stack_->context = FFEEXPR_contextACTUALARG_;
	  break;

	case FFEEXPR_contextSFUNCDEFINDEXORACTUALARG_:
	  ffeexpr_stack_->context = FFEEXPR_contextSFUNCDEFACTUALARG_;
	  break;

	default:
	  break;
	}
      break;

    default:
      switch (ffeexpr_stack_->context)
	{
	case FFEEXPR_contextACTUALARG_:
	  ffeexpr_stack_->context = FFEEXPR_contextACTUALARGEXPR_;
	  break;

	case FFEEXPR_contextINDEXORACTUALARG_:
	  ffeexpr_stack_->context = FFEEXPR_contextINDEXORACTUALARGEXPR_;
	  break;

	case FFEEXPR_contextSFUNCDEFACTUALARG_:
	  ffeexpr_stack_->context = FFEEXPR_contextSFUNCDEFACTUALARGEXPR_;
	  break;

	case FFEEXPR_contextSFUNCDEFINDEXORACTUALARG_:
	  ffeexpr_stack_->context
	    = FFEEXPR_contextSFUNCDEFINDEXORACTUALARGEXPR_;
	  break;

	default:
	  assert ("bad context in _name_arg_" == NULL);
	  break;
	}
      break;
    }

  return (ffelexHandler) ffeexpr_token_name_rhs_ (t);
}

/* ffeexpr_token_name_rhs_ -- Rhs NAME

   Return a pointer to this function to the lexer (ffelex), which will
   invoke it for the next token.

   Handle a name followed by open-paren, apostrophe (O'octal-const',
   Z'hex-const', or X'hex-const'), period (RECORD.MEMBER).

   26-Nov-91  JCB  1.2
      When followed by apostrophe or quote, set lex hexnum flag on so
      [0-9] as first char of next token seen as starting a potentially
      hex number (NAME).
   04-Oct-91  JCB  1.1
      In case of intrinsic, decorate its SYMTER with the type info for
      the specific intrinsic.  */

static ffelexHandler
ffeexpr_token_name_rhs_ (ffelexToken t)
{
  ffeexprExpr_ e;
  ffeexprParenType_ paren_type;
  ffesymbol s;
  bool sfdef;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeQUOTE:
    case FFELEX_typeAPOSTROPHE:
      ffeexpr_tokens_[1] = ffelex_token_use (t);
      ffelex_set_hexnum (TRUE);
      return (ffelexHandler) ffeexpr_token_name_apos_;

    case FFELEX_typeOPEN_PAREN:
      e = ffeexpr_expr_new_ ();
      e->type = FFEEXPR_exprtypeOPERAND_;
      e->token = ffelex_token_use (ffeexpr_tokens_[0]);
      s = ffeexpr_declare_parenthesized_ (ffeexpr_tokens_[0], TRUE,
					  &paren_type);
      if (ffesymbol_attrs (s) & FFESYMBOL_attrsANY)
	e->u.operand = ffebld_new_any ();
      else
	e->u.operand = ffebld_new_symter (s, ffesymbol_generic (s),
					  ffesymbol_specific (s),
					  ffesymbol_implementation (s));
      ffeexpr_exprstack_push_ (e);	/* Not a complete operand yet. */
      ffeexpr_stack_->tokens[0] = ffeexpr_tokens_[0];
      switch (ffeexpr_context_outer_ (ffeexpr_stack_))
	{
	case FFEEXPR_contextSFUNCDEF:
	case FFEEXPR_contextSFUNCDEFINDEX_:
	case FFEEXPR_contextSFUNCDEFACTUALARGEXPR_:
	case FFEEXPR_contextSFUNCDEFINDEXORACTUALARGEXPR_:
	  sfdef = TRUE;
	  break;

	case FFEEXPR_contextSFUNCDEFACTUALARG_:
	case FFEEXPR_contextSFUNCDEFINDEXORACTUALARG_:
	  assert ("weird context!" == NULL);
	  sfdef = FALSE;
	  break;

	default:
	  sfdef = FALSE;
	  break;
	}
      switch (paren_type)
	{
	case FFEEXPR_parentypeFUNCTION_:
	  ffebld_set_info (e->u.operand, ffesymbol_info (s));
	  ffebld_init_list (&ffeexpr_stack_->expr, &ffeexpr_stack_->bottom);
	  if (ffesymbol_where (s) == FFEINFO_whereCONSTANT)
	    {			/* A statement function. */
	      ffeexpr_stack_->num_args
		= ffebld_list_length
		  (ffeexpr_stack_->next_dummy
		   = ffesymbol_dummyargs (s));
	      ffeexpr_stack_->tokens[1] = NULL;	/* !=NULL when > num_args. */
	    }
	  else if ((ffesymbol_where (s) == FFEINFO_whereINTRINSIC)
		   && !ffe_is_pedantic_not_90 ()
		   && ((ffesymbol_implementation (s)
			== FFEINTRIN_impICHAR)
		       || (ffesymbol_implementation (s)
			   == FFEINTRIN_impIACHAR)
		       || (ffesymbol_implementation (s)
			   == FFEINTRIN_impLEN)))
	    {			/* Allow arbitrary concatenations. */
	      return
		(ffelexHandler)
		  ffeexpr_rhs (ffeexpr_stack_->pool,
			       sfdef
			       ? FFEEXPR_contextSFUNCDEF
			       : FFEEXPR_contextLET,
			       ffeexpr_token_arguments_);
	    }
	  return
	    (ffelexHandler)
	    ffeexpr_rhs (ffeexpr_stack_->pool,
			 sfdef
			 ? FFEEXPR_contextSFUNCDEFACTUALARG_
			 : FFEEXPR_contextACTUALARG_,
			 ffeexpr_token_arguments_);

	case FFEEXPR_parentypeARRAY_:
	  ffebld_set_info (e->u.operand,
			   ffesymbol_info (ffebld_symter (e->u.operand)));
	  ffebld_init_list (&ffeexpr_stack_->expr, &ffeexpr_stack_->bottom);
	  ffeexpr_stack_->bound_list = ffesymbol_dims (s);
	  ffeexpr_stack_->rank = 0;
	  ffeexpr_stack_->constant = TRUE;
	  ffeexpr_stack_->immediate = TRUE;
	  return (ffelexHandler) ffeexpr_rhs (ffeexpr_stack_->pool,
					      sfdef
					      ? FFEEXPR_contextSFUNCDEFINDEX_
					      : FFEEXPR_contextINDEX_,
					      ffeexpr_token_elements_);

	case FFEEXPR_parentypeSUBSTRING_:
	  ffebld_set_info (e->u.operand,
			   ffesymbol_info (ffebld_symter (e->u.operand)));
	  e->u.operand = ffeexpr_collapse_symter (e->u.operand,
						  ffeexpr_tokens_[0]);
	  return
	    (ffelexHandler)
	    ffeexpr_rhs (ffeexpr_stack_->pool,
			 sfdef
			 ? FFEEXPR_contextSFUNCDEFINDEX_
			 : FFEEXPR_contextINDEX_,
			 ffeexpr_token_substring_);

	case FFEEXPR_parentypeFUNSUBSTR_:
	  return
	    (ffelexHandler)
	    ffeexpr_rhs (ffeexpr_stack_->pool,
			 sfdef
			 ? FFEEXPR_contextSFUNCDEFINDEXORACTUALARG_
			 : FFEEXPR_contextINDEXORACTUALARG_,
			 ffeexpr_token_funsubstr_);

	case FFEEXPR_parentypeANY_:
	  ffebld_set_info (e->u.operand, ffesymbol_info (s));
	  return
	    (ffelexHandler)
	    ffeexpr_rhs (ffeexpr_stack_->pool,
			 sfdef
			 ? FFEEXPR_contextSFUNCDEFACTUALARG_
			 : FFEEXPR_contextACTUALARG_,
			 ffeexpr_token_anything_);

	default:
	  assert ("bad paren type" == NULL);
	  break;
	}

    case FFELEX_typeEQUALS:	/* As in "VAR=". */
      switch (ffeexpr_stack_->context)
	{
	case FFEEXPR_contextIMPDOITEM_:	/* "(,VAR=start,end[,incr])". */
	case FFEEXPR_contextIMPDOITEMDF_:
	  ffeexpr_stack_->is_rhs = FALSE;	/* Really an lhs construct. */
	  ffeexpr_stack_->context = FFEEXPR_contextIMPDOCTRL_;
	  break;

	default:
	  break;
	}
      break;

#if 0
    case FFELEX_typePERIOD:
    case FFELEX_typePERCENT:
      ~~Support these two someday, though not required
	assert ("FOO%, FOO. not yet supported!~~" == NULL);
      break;
#endif

    default:
      break;
    }

  switch (ffeexpr_stack_->context)
    {
    case FFEEXPR_contextINDEXORACTUALARG_:
    case FFEEXPR_contextSFUNCDEFINDEXORACTUALARG_:
      assert ("strange context" == NULL);
      break;

    default:
      break;
    }

  e = ffeexpr_expr_new_ ();
  e->type = FFEEXPR_exprtypeOPERAND_;
  e->token = ffeexpr_tokens_[0];
  s = ffeexpr_declare_unadorned_ (ffeexpr_tokens_[0], FALSE);
  if (ffesymbol_attrs (s) & FFESYMBOL_attrsANY)
    {
      e->u.operand = ffebld_new_any ();
      ffebld_set_info (e->u.operand, ffeinfo_new_any ());
    }
  else
    {
      e->u.operand = ffebld_new_symter (s, FFEINTRIN_genNONE,
					ffesymbol_specific (s),
					ffesymbol_implementation (s));
      if (ffesymbol_specific (s) == FFEINTRIN_specNONE)
	ffebld_set_info (e->u.operand, ffeinfo_use (ffesymbol_info (s)));
      else
	{			/* Decorate the SYMTER with the actual type
				   of the intrinsic. */
	  ffebld_set_info (e->u.operand, ffeinfo_new
			(ffeintrin_basictype (ffesymbol_specific (s)),
			 ffeintrin_kindtype (ffesymbol_specific (s)),
			 0,
			 ffesymbol_kind (s),
			 ffesymbol_where (s),
			 FFETARGET_charactersizeNONE));
	}
      if (ffesymbol_is_doiter (s))
	ffebld_symter_set_is_doiter (e->u.operand, TRUE);
      e->u.operand = ffeexpr_collapse_symter (e->u.operand,
					      ffeexpr_tokens_[0]);
    }
  ffeexpr_exprstack_push_operand_ (e);
  return (ffelexHandler) ffeexpr_token_binary_ (t);
}

/* ffeexpr_token_name_apos_ -- Rhs NAME APOSTROPHE

   Return a pointer to this function to the lexer (ffelex), which will
   invoke it for the next token.

   Expecting a NAME token, analyze the previous NAME token to see what kind,
   if any, typeless constant we've got.

   01-Sep-90  JCB  1.1
      Expect a NAME instead of CHARACTER in this situation.  */

static ffelexHandler
ffeexpr_token_name_apos_ (ffelexToken t)
{
  ffeexprExpr_ e;

  ffelex_set_hexnum (FALSE);

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
      ffeexpr_tokens_[2] = ffelex_token_use (t);
      return (ffelexHandler) ffeexpr_token_name_apos_name_;

    default:
      break;
    }

  if (ffest_ffebad_start (FFEBAD_INVALID_RADIX_SPECIFIER))
    {
      ffebad_string (ffelex_token_text (ffeexpr_tokens_[0]));
      ffebad_here (0, ffelex_token_where_line (ffeexpr_tokens_[0]),
		   ffelex_token_where_column (ffeexpr_tokens_[0]));
      ffebad_here (1, ffelex_token_where_line (t),
		   ffelex_token_where_column (t));
      ffebad_finish ();
    }

  ffelex_token_kill (ffeexpr_tokens_[1]);

  e = ffeexpr_expr_new_ ();
  e->type = FFEEXPR_exprtypeOPERAND_;
  e->u.operand = ffebld_new_any ();
  ffebld_set_info (e->u.operand, ffeinfo_new_any ());
  e->token = ffeexpr_tokens_[0];
  ffeexpr_exprstack_push_operand_ (e);

  return (ffelexHandler) ffeexpr_token_binary_ (t);
}

/* ffeexpr_token_name_apos_name_ -- Rhs NAME APOSTROPHE NAME

   Return a pointer to this function to the lexer (ffelex), which will
   invoke it for the next token.

   Expecting an APOSTROPHE token, analyze the previous NAME token to see
   what kind, if any, typeless constant we've got.  */

static ffelexHandler
ffeexpr_token_name_apos_name_ (ffelexToken t)
{
  ffeexprExpr_ e;
  char c;

  e = ffeexpr_expr_new_ ();
  e->type = FFEEXPR_exprtypeOPERAND_;
  e->token = ffeexpr_tokens_[0];

  if ((ffelex_token_type (t) == ffelex_token_type (ffeexpr_tokens_[1]))
      && (ffelex_token_length (ffeexpr_tokens_[0]) == 1)
      && (ffesrc_char_match_init ((c = ffelex_token_text (ffeexpr_tokens_[0])[0]),
				  'B', 'b')
	  || ffesrc_char_match_init (c, 'O', 'o')
	  || ffesrc_char_match_init (c, 'X', 'x')
	  || ffesrc_char_match_init (c, 'Z', 'z')))
    {
      ffetargetCharacterSize size;

      if (!ffe_is_typeless_boz ()) {

      switch (c)
	{
	case FFESRC_CASE_MATCH_INIT ('B', 'b', imatch_b, no_imatch):
	  e->u.operand = ffebld_new_conter (ffebld_constant_new_integerbinary
					    (ffeexpr_tokens_[2]));
	  break;

	case FFESRC_CASE_MATCH_INIT ('O', 'o', imatch_o, no_imatch):
	  e->u.operand = ffebld_new_conter (ffebld_constant_new_integeroctal
					    (ffeexpr_tokens_[2]));
	  break;

	case FFESRC_CASE_MATCH_INIT ('X', 'x', imatch_x, no_imatch):
	  e->u.operand = ffebld_new_conter (ffebld_constant_new_integerhex
					    (ffeexpr_tokens_[2]));
	  break;

	case FFESRC_CASE_MATCH_INIT ('Z', 'z', imatch_z, no_imatch):
	  e->u.operand = ffebld_new_conter (ffebld_constant_new_integerhex
					    (ffeexpr_tokens_[2]));
	  break;

	default:
	no_imatch:		/* :::::::::::::::::::: */
	  assert ("not BOXZ!" == NULL);
	  abort ();
	}

	ffebld_set_info (e->u.operand,
			 ffeinfo_new (FFEINFO_basictypeINTEGER,
				      FFEINFO_kindtypeINTEGERDEFAULT, 0,
				      FFEINFO_kindENTITY, FFEINFO_whereCONSTANT,
				      FFETARGET_charactersizeNONE));
	ffeexpr_exprstack_push_operand_ (e);
	ffelex_token_kill (ffeexpr_tokens_[1]);
	ffelex_token_kill (ffeexpr_tokens_[2]);
	return (ffelexHandler) ffeexpr_token_binary_;
      }

      switch (c)
	{
	case FFESRC_CASE_MATCH_INIT ('B', 'b', match_b, no_match):
	  e->u.operand = ffebld_new_conter (ffebld_constant_new_typeless_bm
					    (ffeexpr_tokens_[2]));
	  size = ffetarget_size_typeless_binary (ffeexpr_tokens_[2]);
	  break;

	case FFESRC_CASE_MATCH_INIT ('O', 'o', match_o, no_match):
	  e->u.operand = ffebld_new_conter (ffebld_constant_new_typeless_om
					    (ffeexpr_tokens_[2]));
	  size = ffetarget_size_typeless_octal (ffeexpr_tokens_[2]);
	  break;

	case FFESRC_CASE_MATCH_INIT ('X', 'x', match_x, no_match):
	  e->u.operand = ffebld_new_conter (ffebld_constant_new_typeless_hxm
					    (ffeexpr_tokens_[2]));
	  size = ffetarget_size_typeless_hex (ffeexpr_tokens_[2]);
	  break;

	case FFESRC_CASE_MATCH_INIT ('Z', 'z', match_z, no_match):
	  e->u.operand = ffebld_new_conter (ffebld_constant_new_typeless_hzm
					    (ffeexpr_tokens_[2]));
	  size = ffetarget_size_typeless_hex (ffeexpr_tokens_[2]);
	  break;

	default:
	no_match:		/* :::::::::::::::::::: */
	  assert ("not BOXZ!" == NULL);
	  e->u.operand = ffebld_new_conter (ffebld_constant_new_typeless_hzm
					    (ffeexpr_tokens_[2]));
	  size = ffetarget_size_typeless_hex (ffeexpr_tokens_[2]);
	  break;
	}
      ffebld_set_info (e->u.operand,
	       ffeinfo_new (FFEINFO_basictypeTYPELESS, FFEINFO_kindtypeNONE,
		       0, FFEINFO_kindENTITY, FFEINFO_whereCONSTANT, size));
      ffeexpr_exprstack_push_operand_ (e);
      ffelex_token_kill (ffeexpr_tokens_[1]);
      ffelex_token_kill (ffeexpr_tokens_[2]);
      return (ffelexHandler) ffeexpr_token_binary_;
    }

  if (ffest_ffebad_start (FFEBAD_INVALID_RADIX_SPECIFIER))
    {
      ffebad_string (ffelex_token_text (ffeexpr_tokens_[0]));
      ffebad_here (0, ffelex_token_where_line (ffeexpr_tokens_[0]),
		   ffelex_token_where_column (ffeexpr_tokens_[0]));
      ffebad_here (1, ffelex_token_where_line (t), ffelex_token_where_column (t));
      ffebad_finish ();
    }

  ffelex_token_kill (ffeexpr_tokens_[1]);
  ffelex_token_kill (ffeexpr_tokens_[2]);

  e->type = FFEEXPR_exprtypeOPERAND_;
  e->u.operand = ffebld_new_any ();
  ffebld_set_info (e->u.operand, ffeinfo_new_any ());
  e->token = ffeexpr_tokens_[0];
  ffeexpr_exprstack_push_operand_ (e);

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeAPOSTROPHE:
    case FFELEX_typeQUOTE:
      return (ffelexHandler) ffeexpr_token_binary_;

    default:
      return (ffelexHandler) ffeexpr_token_binary_ (t);
    }
}

/* ffeexpr_token_percent_ -- Rhs PERCENT

   Handle a percent sign possibly followed by "LOC".  If followed instead
   by "VAL", "REF", or "DESCR", issue an error message and substitute
   "LOC".  If followed by something else, treat the percent sign as a
   spurious incorrect token and reprocess the token via _rhs_.	*/

static ffelexHandler
ffeexpr_token_percent_ (ffelexToken t)
{
  switch (ffelex_token_type (t))
    {
    case FFELEX_typeNAME:
    case FFELEX_typeNAMES:
      ffeexpr_stack_->percent = ffeexpr_percent_ (t);
      ffeexpr_tokens_[1] = ffelex_token_use (t);
      return (ffelexHandler) ffeexpr_token_percent_name_;

    default:
      if (ffest_ffebad_start (FFEBAD_INVALID_TOKEN_IN_EXPRESSION))
	{
	  ffebad_here (0, ffelex_token_where_line (ffeexpr_tokens_[0]),
		       ffelex_token_where_column (ffeexpr_tokens_[0]));
	  ffebad_here (1, ffelex_token_where_line (ffeexpr_stack_->first_token),
		   ffelex_token_where_column (ffeexpr_stack_->first_token));
	  ffebad_finish ();
	}
      ffelex_token_kill (ffeexpr_tokens_[0]);
      return (ffelexHandler) ffeexpr_token_rhs_ (t);
    }
}

/* ffeexpr_token_percent_name_ -- Rhs PERCENT NAME

   Make sure the token is OPEN_PAREN and prepare for the one-item list of
   LHS expressions.  Else display an error message.  */

static ffelexHandler
ffeexpr_token_percent_name_ (ffelexToken t)
{
  ffelexHandler nexthandler;

  if (ffelex_token_type (t) != FFELEX_typeOPEN_PAREN)
    {
      if (ffest_ffebad_start (FFEBAD_INVALID_TOKEN_IN_EXPRESSION))
	{
	  ffebad_here (0, ffelex_token_where_line (ffeexpr_tokens_[0]),
		       ffelex_token_where_column (ffeexpr_tokens_[0]));
	  ffebad_here (1, ffelex_token_where_line (ffeexpr_stack_->first_token),
		   ffelex_token_where_column (ffeexpr_stack_->first_token));
	  ffebad_finish ();
	}
      ffelex_token_kill (ffeexpr_tokens_[0]);
      nexthandler = (ffelexHandler) ffeexpr_token_rhs_ (ffeexpr_tokens_[1]);
      ffelex_token_kill (ffeexpr_tokens_[1]);
      return (ffelexHandler) (*nexthandler) (t);
    }

  switch (ffeexpr_stack_->percent)
    {
    default:
      if (ffest_ffebad_start (FFEBAD_INVALID_PERCENT))
	{
	  ffebad_here (0, ffelex_token_where_line (ffeexpr_tokens_[0]),
		       ffelex_token_where_column (ffeexpr_tokens_[0]));
	  ffebad_string (ffelex_token_text (ffeexpr_tokens_[1]));
	  ffebad_finish ();
	}
      ffeexpr_stack_->percent = FFEEXPR_percentLOC_;
      /* Fall through. */
    case FFEEXPR_percentLOC_:
      ffeexpr_stack_->tokens[0] = ffeexpr_tokens_[0];
      ffelex_token_kill (ffeexpr_tokens_[1]);
      ffeexpr_stack_->tokens[1] = ffelex_token_use (t);
      return (ffelexHandler) ffeexpr_rhs (ffeexpr_stack_->pool,
					  FFEEXPR_contextLOC_,
					  ffeexpr_cb_end_loc_);
    }
}

/* ffeexpr_make_float_const_ -- Make a floating-point constant

   See prototype.

   Pass 'E', 'D', or 'Q' for exponent letter.  */

static void
ffeexpr_make_float_const_ (char exp_letter, ffelexToken integer,
			   ffelexToken decimal, ffelexToken fraction,
			   ffelexToken exponent, ffelexToken exponent_sign,
			   ffelexToken exponent_digits)
{
  ffeexprExpr_ e;

  e = ffeexpr_expr_new_ ();
  e->type = FFEEXPR_exprtypeOPERAND_;
  if (integer != NULL)
    e->token = ffelex_token_use (integer);
  else
    {
      assert (decimal != NULL);
      e->token = ffelex_token_use (decimal);
    }

  switch (exp_letter)
    {
#if !FFETARGET_okREALQUAD
    case FFESRC_CASE_MATCH_INIT ('Q', 'q', match_q, no_match):
      if (ffebad_start (FFEBAD_QUAD_UNSUPPORTED))
	{
	  ffebad_here (0, ffelex_token_where_line (e->token),
		       ffelex_token_where_column (e->token));
	  ffebad_finish ();
	}
      goto match_d;		/* The FFESRC_CASE_* macros don't
				   allow fall-through! */
#endif

    case FFESRC_CASE_MATCH_INIT ('D', 'd', match_d, no_match):
      e->u.operand = ffebld_new_conter (ffebld_constant_new_realdouble
					(integer, decimal, fraction, exponent, exponent_sign, exponent_digits));
      ffebld_set_info (e->u.operand,
	     ffeinfo_new (FFEINFO_basictypeREAL, FFEINFO_kindtypeREALDOUBLE,
			  0, FFEINFO_kindENTITY, FFEINFO_whereCONSTANT, FFETARGET_charactersizeNONE));
      break;

    case FFESRC_CASE_MATCH_INIT ('E', 'e', match_e, no_match):
      e->u.operand = ffebld_new_conter (ffebld_constant_new_realdefault
					(integer, decimal, fraction, exponent, exponent_sign, exponent_digits));
      ffebld_set_info (e->u.operand, ffeinfo_new (FFEINFO_basictypeREAL,
			 FFEINFO_kindtypeREALDEFAULT, 0, FFEINFO_kindENTITY,
		       FFEINFO_whereCONSTANT, FFETARGET_charactersizeNONE));
      break;

#if FFETARGET_okREALQUAD
    case FFESRC_CASE_MATCH_INIT ('Q', 'q', match_q, no_match):
      e->u.operand = ffebld_new_conter (ffebld_constant_new_realquad
					(integer, decimal, fraction, exponent, exponent_sign, exponent_digits));
      ffebld_set_info (e->u.operand,
	       ffeinfo_new (FFEINFO_basictypeREAL, FFEINFO_kindtypeREALQUAD,
			    0, FFEINFO_kindENTITY, FFEINFO_whereCONSTANT, FFETARGET_charactersizeNONE));
      break;
#endif

    case 'I':	/* Make an integer. */
      e->u.operand = ffebld_new_conter (ffebld_constant_new_integerdefault
					(ffeexpr_tokens_[0]));
      ffebld_set_info (e->u.operand,
		       ffeinfo_new (FFEINFO_basictypeINTEGER,
				    FFEINFO_kindtypeINTEGERDEFAULT, 0,
				    FFEINFO_kindENTITY, FFEINFO_whereCONSTANT,
				    FFETARGET_charactersizeNONE));
      break;

    default:
    no_match:			/* :::::::::::::::::::: */
      assert ("Lost the exponent letter!" == NULL);
    }

  ffeexpr_exprstack_push_operand_ (e);
}

/* Just like ffesymbol_declare_local, except performs any implicit info
   assignment necessary.  */

static ffesymbol
ffeexpr_declare_unadorned_ (ffelexToken t, bool maybe_intrin)
{
  ffesymbol s;
  ffeinfoKind k;
  bool bad;

  s = ffesymbol_declare_local (t, maybe_intrin);

  switch (ffeexpr_context_outer_ (ffeexpr_stack_))
    /* Special-case these since they can involve a different concept
       of "state" (in the stmtfunc name space).  */
    {
    case FFEEXPR_contextDATAIMPDOINDEX_:
    case FFEEXPR_contextDATAIMPDOCTRL_:
      if (ffeexpr_context_outer_ (ffeexpr_stack_)
	  == FFEEXPR_contextDATAIMPDOINDEX_)
	s = ffeexpr_sym_impdoitem_ (s, t);
      else
	if (ffeexpr_stack_->is_rhs)
	  s = ffeexpr_sym_impdoitem_ (s, t);
	else
	  s = ffeexpr_sym_lhs_impdoctrl_ (s, t);
      bad = (ffesymbol_kind (s) != FFEINFO_kindENTITY)
	|| ((ffesymbol_where (s) != FFEINFO_whereCONSTANT)
	    && (ffesymbol_where (s) != FFEINFO_whereIMMEDIATE));
      if (bad && (ffesymbol_kind (s) != FFEINFO_kindANY))
	ffesymbol_error (s, t);
      return s;

    default:
      break;
    }

  switch ((ffesymbol_sfdummyparent (s) == NULL)
	  ? ffesymbol_state (s)
	  : FFESYMBOL_stateUNDERSTOOD)
    {
    case FFESYMBOL_stateNONE:	/* Before first exec, not seen in expr
				   context. */
      if (!ffest_seen_first_exec ())
	goto seen;		/* :::::::::::::::::::: */
      /* Fall through. */
    case FFESYMBOL_stateUNCERTAIN:	/* Unseen since first exec. */
      switch (ffeexpr_context_outer_ (ffeexpr_stack_))
	{
	case FFEEXPR_contextSUBROUTINEREF:
	  s = ffeexpr_sym_lhs_call_ (s, t);
	  break;

	case FFEEXPR_contextFILEEXTFUNC:
	  s = ffeexpr_sym_lhs_extfunc_ (s, t);
	  break;

	case FFEEXPR_contextSFUNCDEFACTUALARG_:
	  s = ffecom_sym_exec_transition (s);
	  if (ffesymbol_state (s) == FFESYMBOL_stateUNDERSTOOD)
	    goto understood;	/* :::::::::::::::::::: */
	  /* Fall through. */
	case FFEEXPR_contextACTUALARG_:
	  s = ffeexpr_sym_rhs_actualarg_ (s, t);
	  break;

	case FFEEXPR_contextDATA:
	  if (ffeexpr_stack_->is_rhs)
	    s = ffeexpr_sym_rhs_let_ (s, t);
	  else
	    s = ffeexpr_sym_lhs_data_ (s, t);
	  break;

	case FFEEXPR_contextDATAIMPDOITEM_:
	  s = ffeexpr_sym_lhs_data_ (s, t);
	  break;

	case FFEEXPR_contextSFUNCDEF:
	case FFEEXPR_contextSFUNCDEFINDEX_:
	case FFEEXPR_contextSFUNCDEFACTUALARGEXPR_:
	case FFEEXPR_contextSFUNCDEFINDEXORACTUALARGEXPR_:
	  s = ffecom_sym_exec_transition (s);
	  if (ffesymbol_state (s) == FFESYMBOL_stateUNDERSTOOD)
	    goto understood;	/* :::::::::::::::::::: */
	  /* Fall through. */
	case FFEEXPR_contextLET:
	case FFEEXPR_contextPAREN_:
	case FFEEXPR_contextACTUALARGEXPR_:
	case FFEEXPR_contextINDEXORACTUALARGEXPR_:
	case FFEEXPR_contextASSIGN:
	case FFEEXPR_contextIOLIST:
	case FFEEXPR_contextIOLISTDF:
	case FFEEXPR_contextDO:
	case FFEEXPR_contextDOWHILE:
	case FFEEXPR_contextAGOTO:
	case FFEEXPR_contextCGOTO:
	case FFEEXPR_contextIF:
	case FFEEXPR_contextARITHIF:
	case FFEEXPR_contextFORMAT:
	case FFEEXPR_contextSTOP:
	case FFEEXPR_contextRETURN:
	case FFEEXPR_contextSELECTCASE:
	case FFEEXPR_contextCASE:
	case FFEEXPR_contextFILEASSOC:
	case FFEEXPR_contextFILEINT:
	case FFEEXPR_contextFILEDFINT:
	case FFEEXPR_contextFILELOG:
	case FFEEXPR_contextFILENUM:
	case FFEEXPR_contextFILENUMAMBIG:
	case FFEEXPR_contextFILECHAR:
	case FFEEXPR_contextFILENUMCHAR:
	case FFEEXPR_contextFILEDFCHAR:
	case FFEEXPR_contextFILEKEY:
	case FFEEXPR_contextFILEUNIT:
	case FFEEXPR_contextFILEUNIT_DF:
	case FFEEXPR_contextFILEUNITAMBIG:
	case FFEEXPR_contextFILEFORMAT:
	case FFEEXPR_contextFILENAMELIST:
	case FFEEXPR_contextFILEVXTCODE:
	case FFEEXPR_contextINDEX_:
	case FFEEXPR_contextIMPDOITEM_:
	case FFEEXPR_contextIMPDOITEMDF_:
	case FFEEXPR_contextIMPDOCTRL_:
	case FFEEXPR_contextLOC_:
	  if (ffeexpr_stack_->is_rhs)
	    s = ffeexpr_sym_rhs_let_ (s, t);
	  else
	    s = ffeexpr_sym_lhs_let_ (s, t);
	  break;

	case FFEEXPR_contextCHARACTERSIZE:
	case FFEEXPR_contextEQUIVALENCE:
	case FFEEXPR_contextINCLUDE:
	case FFEEXPR_contextPARAMETER:
	case FFEEXPR_contextDIMLIST:
	case FFEEXPR_contextDIMLISTCOMMON:
	case FFEEXPR_contextKINDTYPE:
	case FFEEXPR_contextINITVAL:
	case FFEEXPR_contextEQVINDEX_:
	  break;		/* Will turn into errors below. */

	default:
	  ffesymbol_error (s, t);
	  break;
	}
      /* Fall through. */
    case FFESYMBOL_stateUNDERSTOOD:	/* Nothing much more to learn. */
    understood:		/* :::::::::::::::::::: */
      k = ffesymbol_kind (s);
      switch (ffeexpr_context_outer_ (ffeexpr_stack_))
	{
	case FFEEXPR_contextSUBROUTINEREF:
	  bad = ((k != FFEINFO_kindSUBROUTINE)
		 && ((ffesymbol_where (s) != FFEINFO_whereINTRINSIC)
		     || (k != FFEINFO_kindNONE)));
	  break;

	case FFEEXPR_contextFILEEXTFUNC:
	  bad = (k != FFEINFO_kindFUNCTION)
	    || (ffesymbol_where (s) != FFEINFO_whereGLOBAL);
	  break;

	case FFEEXPR_contextSFUNCDEFACTUALARG_:
	case FFEEXPR_contextACTUALARG_:
	  switch (k)
	    {
	    case FFEINFO_kindENTITY:
	      bad = FALSE;
	      break;

	    case FFEINFO_kindFUNCTION:
	    case FFEINFO_kindSUBROUTINE:
	      bad
		= ((ffesymbol_where (s) != FFEINFO_whereGLOBAL)
		   && (ffesymbol_where (s) != FFEINFO_whereDUMMY)
		   && ((ffesymbol_where (s) != FFEINFO_whereINTRINSIC)
		       || !ffeintrin_is_actualarg (ffesymbol_specific (s))));
	      break;

	    case FFEINFO_kindNONE:
	      if (ffesymbol_where (s) == FFEINFO_whereINTRINSIC)
		{
		  bad = !(ffeintrin_is_actualarg (ffesymbol_specific (s)));
		  break;
		}

	      /* If state is UNDERSTOOD here, it's CHAR*(*) or attrsANY,
		 and in the former case, attrsTYPE is set, so we
		 see this as an error as we should, since CHAR*(*)
		 cannot be actually referenced in a main/block data
		 program unit.  */

	      if ((ffesymbol_attrs (s) & (FFESYMBOL_attrsANY
					  | FFESYMBOL_attrsEXTERNAL
					  | FFESYMBOL_attrsTYPE))
		  == FFESYMBOL_attrsEXTERNAL)
		bad = FALSE;
	      else
		bad = TRUE;
	      break;

	    default:
	      bad = TRUE;
	      break;
	    }
	  break;

	case FFEEXPR_contextDATA:
	  if (ffeexpr_stack_->is_rhs)
	    bad = (k != FFEINFO_kindENTITY)
	      || (ffesymbol_where (s) != FFEINFO_whereCONSTANT);
	  else
	    bad = (k != FFEINFO_kindENTITY)
	      || ((ffesymbol_where (s) != FFEINFO_whereNONE)
		  && (ffesymbol_where (s) != FFEINFO_whereLOCAL)
		  && (ffesymbol_where (s) != FFEINFO_whereCOMMON));
	  break;

	case FFEEXPR_contextDATAIMPDOITEM_:
	  bad = TRUE;		/* Unadorned item never valid. */
	  break;

	case FFEEXPR_contextSFUNCDEF:
	case FFEEXPR_contextSFUNCDEFINDEX_:
	case FFEEXPR_contextSFUNCDEFACTUALARGEXPR_:
	case FFEEXPR_contextSFUNCDEFINDEXORACTUALARGEXPR_:
	case FFEEXPR_contextLET:
	case FFEEXPR_contextPAREN_:
	case FFEEXPR_contextACTUALARGEXPR_:
	case FFEEXPR_contextINDEXORACTUALARGEXPR_:
	case FFEEXPR_contextASSIGN:
	case FFEEXPR_contextIOLIST:
	case FFEEXPR_contextIOLISTDF:
	case FFEEXPR_contextDO:
	case FFEEXPR_contextDOWHILE:
	case FFEEXPR_contextAGOTO:
	case FFEEXPR_contextCGOTO:
	case FFEEXPR_contextIF:
	case FFEEXPR_contextARITHIF:
	case FFEEXPR_contextFORMAT:
	case FFEEXPR_contextSTOP:
	case FFEEXPR_contextRETURN:
	case FFEEXPR_contextSELECTCASE:
	case FFEEXPR_contextCASE:
	case FFEEXPR_contextFILEASSOC:
	case FFEEXPR_contextFILEINT:
	case FFEEXPR_contextFILEDFINT:
	case FFEEXPR_contextFILELOG:
	case FFEEXPR_contextFILENUM:
	case FFEEXPR_contextFILENUMAMBIG:
	case FFEEXPR_contextFILECHAR:
	case FFEEXPR_contextFILENUMCHAR:
	case FFEEXPR_contextFILEDFCHAR:
	case FFEEXPR_contextFILEKEY:
	case FFEEXPR_contextFILEUNIT:
	case FFEEXPR_contextFILEUNIT_DF:
	case FFEEXPR_contextFILEUNITAMBIG:
	case FFEEXPR_contextFILEFORMAT:
	case FFEEXPR_contextFILENAMELIST:
	case FFEEXPR_contextFILEVXTCODE:
	case FFEEXPR_contextINDEX_:
	case FFEEXPR_contextIMPDOITEM_:
	case FFEEXPR_contextIMPDOITEMDF_:
	case FFEEXPR_contextIMPDOCTRL_:
	case FFEEXPR_contextLOC_:
	  bad = (k != FFEINFO_kindENTITY);	/* This catches "SUBROUTINE
						   X(A);EXTERNAL A;CALL
						   Y(A);B=A", for example. */
	  break;

	case FFEEXPR_contextCHARACTERSIZE:
	case FFEEXPR_contextEQUIVALENCE:
	case FFEEXPR_contextPARAMETER:
	case FFEEXPR_contextDIMLIST:
	case FFEEXPR_contextDIMLISTCOMMON:
	case FFEEXPR_contextKINDTYPE:
	case FFEEXPR_contextINITVAL:
	case FFEEXPR_contextEQVINDEX_:
	  bad = (k != FFEINFO_kindENTITY)
	    || (ffesymbol_where (s) != FFEINFO_whereCONSTANT);
	  break;

	case FFEEXPR_contextINCLUDE:
	  bad = TRUE;
	  break;

	default:
	  bad = TRUE;
	  break;
	}
      if (bad && (k != FFEINFO_kindANY))
	ffesymbol_error (s, t);
      return s;

    case FFESYMBOL_stateSEEN:	/* Seen but not yet in exec portion. */
    seen:			/* :::::::::::::::::::: */
      switch (ffeexpr_context_outer_ (ffeexpr_stack_))
	{
	case FFEEXPR_contextPARAMETER:
	  if (ffeexpr_stack_->is_rhs)
	    ffesymbol_error (s, t);
	  else
	    s = ffeexpr_sym_lhs_parameter_ (s, t);
	  break;

	case FFEEXPR_contextDATA:
	  s = ffecom_sym_exec_transition (s);
	  if (ffesymbol_state (s) == FFESYMBOL_stateUNDERSTOOD)
	    goto understood;	/* :::::::::::::::::::: */
	  if (ffeexpr_stack_->is_rhs)
	    ffesymbol_error (s, t);
	  else
	    s = ffeexpr_sym_lhs_data_ (s, t);
	  goto understood;	/* :::::::::::::::::::: */

	case FFEEXPR_contextDATAIMPDOITEM_:
	  s = ffecom_sym_exec_transition (s);
	  if (ffesymbol_state (s) == FFESYMBOL_stateUNDERSTOOD)
	    goto understood;	/* :::::::::::::::::::: */
	  s = ffeexpr_sym_lhs_data_ (s, t);
	  goto understood;	/* :::::::::::::::::::: */

	case FFEEXPR_contextEQUIVALENCE:
	  s = ffeexpr_sym_lhs_equivalence_ (s, t);
	  break;

	case FFEEXPR_contextDIMLIST:
	  s = ffeexpr_sym_rhs_dimlist_ (s, t);
	  break;

	case FFEEXPR_contextCHARACTERSIZE:
	case FFEEXPR_contextKINDTYPE:
	case FFEEXPR_contextDIMLISTCOMMON:
	case FFEEXPR_contextINITVAL:
	case FFEEXPR_contextEQVINDEX_:
	  ffesymbol_error (s, t);
	  break;

	case FFEEXPR_contextINCLUDE:
	  ffesymbol_error (s, t);
	  break;

	case FFEEXPR_contextACTUALARG_:	/* E.g. I in REAL A(Y(I)). */
	case FFEEXPR_contextSFUNCDEFACTUALARG_:
	  s = ffecom_sym_exec_transition (s);
	  if (ffesymbol_state (s) == FFESYMBOL_stateUNDERSTOOD)
	    goto understood;	/* :::::::::::::::::::: */
	  s = ffeexpr_sym_rhs_actualarg_ (s, t);
	  goto understood;	/* :::::::::::::::::::: */

	case FFEEXPR_contextINDEX_:
	case FFEEXPR_contextACTUALARGEXPR_:
	case FFEEXPR_contextINDEXORACTUALARGEXPR_:
	case FFEEXPR_contextSFUNCDEF:
	case FFEEXPR_contextSFUNCDEFINDEX_:
	case FFEEXPR_contextSFUNCDEFACTUALARGEXPR_:
	case FFEEXPR_contextSFUNCDEFINDEXORACTUALARGEXPR_:
	  assert (ffeexpr_stack_->is_rhs);
	  s = ffecom_sym_exec_transition (s);
	  if (ffesymbol_state (s) == FFESYMBOL_stateUNDERSTOOD)
	    goto understood;	/* :::::::::::::::::::: */
	  s = ffeexpr_sym_rhs_let_ (s, t);
	  goto understood;	/* :::::::::::::::::::: */

	default:
	  ffesymbol_error (s, t);
	  break;
	}
      return s;

    default:
      assert ("bad symbol state" == NULL);
      return NULL;
      break;
    }
}

/* Have FOO in DATA (XYZ(FOO),...)/.../ or DATA (...,XYZ=FOO,BAR,BLETCH).
   Could be found via the "statement-function" name space (in which case
   it should become an iterator) or the local name space (in which case
   it should be either a named constant, or a variable that will have an
   sfunc name space sibling that should become an iterator).  */

static ffesymbol
ffeexpr_sym_impdoitem_ (ffesymbol sp, ffelexToken t)
{
  ffesymbol s;
  ffesymbolAttrs sa;
  ffesymbolAttrs na;
  ffesymbolState ss;
  ffesymbolState ns;
  ffeinfoKind kind;
  ffeinfoWhere where;

  ss = ffesymbol_state (sp);

  if (ffesymbol_sfdummyparent (sp) != NULL)
    {				/* Have symbol in sfunc name space. */
      switch (ss)
	{
	case FFESYMBOL_stateNONE:	/* Used as iterator already. */
	  if (ffeexpr_level_ < ffesymbol_maxentrynum (sp))
	    ffesymbol_error (sp, t);	/* Can't use dead iterator. */
	  else
	    {			/* Can use dead iterator because we're at at
				   least an innermore (higher-numbered) level
				   than the iterator's outermost
				   (lowest-numbered) level. */
	      ffesymbol_signal_change (sp);
	      ffesymbol_set_state (sp, FFESYMBOL_stateSEEN);
	      ffesymbol_set_maxentrynum (sp, ffeexpr_level_);
	      ffesymbol_signal_unreported (sp);
	    }
	  break;

	case FFESYMBOL_stateSEEN:	/* Seen already in this or other
					   implied-DO.  Set symbol level
					   number to outermost value, as that
					   tells us we can see it as iterator
					   at that level at the innermost. */
	  if (ffeexpr_level_ < ffesymbol_maxentrynum (sp))
	    {
	      ffesymbol_signal_change (sp);
	      ffesymbol_set_maxentrynum (sp, ffeexpr_level_);
	      ffesymbol_signal_unreported (sp);
	    }
	  break;

	case FFESYMBOL_stateUNCERTAIN:	/* Iterator. */
	  assert (ffeexpr_level_ == ffesymbol_maxentrynum (sp));
	  ffesymbol_error (sp, t);	/* (,,,I=I,10). */
	  break;

	case FFESYMBOL_stateUNDERSTOOD:
	  break;		/* ANY. */

	default:
	  assert ("Foo Bar!!" == NULL);
	  break;
	}

      return sp;
    }

  /* Got symbol in local name space, so we haven't seen it in impdo yet.
     First, if it is brand-new and we're in executable statements, set the
     attributes and exec-transition it to set state UNCERTAIN or UNDERSTOOD.
     Second, if it is now a constant (PARAMETER), then just return it, it
     can't be an implied-do iterator.  If it is understood, complain if it is
     not a valid variable, but make the inner name space iterator anyway and
     return that.  If it is not understood, improve understanding of the
     symbol accordingly, complain accordingly, in either case make the inner
     name space iterator and return that.  */

  sa = ffesymbol_attrs (sp);

  if (ffesymbol_state_is_specable (ss)
      && ffest_seen_first_exec ())
    {
      assert (sa == FFESYMBOL_attrsetNONE);
      ffesymbol_signal_change (sp);
      ffesymbol_set_state (sp, FFESYMBOL_stateSEEN);
      ffesymbol_resolve_intrin (sp);
      if (ffeimplic_establish_symbol (sp))
	ffesymbol_set_attr (sp, FFESYMBOL_attrSFARG);
      else
	ffesymbol_error (sp, t);

      /* After the exec transition, the state will either be UNCERTAIN (could
	 be a dummy or local var) or UNDERSTOOD (local var, because this is a
	 PROGRAM/BLOCKDATA program unit).  */

      sp = ffecom_sym_exec_transition (sp);
      sa = ffesymbol_attrs (sp);
      ss = ffesymbol_state (sp);
    }

  ns = ss;
  kind = ffesymbol_kind (sp);
  where = ffesymbol_where (sp);

  if (ss == FFESYMBOL_stateUNDERSTOOD)
    {
      if (kind != FFEINFO_kindENTITY)
	ffesymbol_error (sp, t);
      if (where == FFEINFO_whereCONSTANT)
	return sp;
    }
  else
    {
      /* Enhance understanding of local symbol.  This used to imply exec
	 transition, but that doesn't seem necessary, since the local symbol
	 doesn't actually get put into an ffebld tree here -- we just learn
	 more about it, just like when we see a local symbol's name in the
	 dummy-arg list of a statement function.  */

      if (ss != FFESYMBOL_stateUNCERTAIN)
	{
	  /* Figure out what kind of object we've got based on previous
	     declarations of or references to the object. */

	  ns = FFESYMBOL_stateSEEN;

	  if (sa & FFESYMBOL_attrsANY)
	    na = sa;
	  else if (!(sa & ~(FFESYMBOL_attrsADJUSTS
			    | FFESYMBOL_attrsANY
			    | FFESYMBOL_attrsCOMMON
			    | FFESYMBOL_attrsDUMMY
			    | FFESYMBOL_attrsEQUIV
			    | FFESYMBOL_attrsINIT
			    | FFESYMBOL_attrsNAMELIST
			    | FFESYMBOL_attrsRESULT
			    | FFESYMBOL_attrsSAVE
			    | FFESYMBOL_attrsSFARG
			    | FFESYMBOL_attrsTYPE)))
	    na = sa | FFESYMBOL_attrsSFARG;
	  else
	    na = FFESYMBOL_attrsetNONE;
	}
      else
	{			/* stateUNCERTAIN. */
	  na = sa | FFESYMBOL_attrsSFARG;
	  ns = FFESYMBOL_stateUNDERSTOOD;

	  assert (!(sa & ~(FFESYMBOL_attrsACTUALARG
			   | FFESYMBOL_attrsADJUSTABLE
			   | FFESYMBOL_attrsANYLEN
			   | FFESYMBOL_attrsARRAY
			   | FFESYMBOL_attrsDUMMY
			   | FFESYMBOL_attrsEXTERNAL
			   | FFESYMBOL_attrsSFARG
			   | FFESYMBOL_attrsTYPE)));

	  if (sa & FFESYMBOL_attrsEXTERNAL)
	    {
	      assert (!(sa & ~(FFESYMBOL_attrsACTUALARG
			       | FFESYMBOL_attrsDUMMY
			       | FFESYMBOL_attrsEXTERNAL
			       | FFESYMBOL_attrsTYPE)));

	      na = FFESYMBOL_attrsetNONE;
	    }
	  else if (sa & FFESYMBOL_attrsDUMMY)
	    {
	      assert (!(sa & FFESYMBOL_attrsEXTERNAL));	/* Handled above. */
	      assert (!(sa & ~(FFESYMBOL_attrsDUMMY
			       | FFESYMBOL_attrsEXTERNAL
			       | FFESYMBOL_attrsTYPE)));

	      kind = FFEINFO_kindENTITY;
	    }
	  else if (sa & FFESYMBOL_attrsARRAY)
	    {
	      assert (!(sa & ~(FFESYMBOL_attrsARRAY
			       | FFESYMBOL_attrsADJUSTABLE
			       | FFESYMBOL_attrsTYPE)));

	      na = FFESYMBOL_attrsetNONE;
	    }
	  else if (sa & FFESYMBOL_attrsSFARG)
	    {
	      assert (!(sa & ~(FFESYMBOL_attrsSFARG
			       | FFESYMBOL_attrsTYPE)));

	      ns = FFESYMBOL_stateUNCERTAIN;
	    }
	  else if (sa & FFESYMBOL_attrsTYPE)
	    {
	      assert (!(sa & (FFESYMBOL_attrsARRAY
			      | FFESYMBOL_attrsDUMMY
			      | FFESYMBOL_attrsEXTERNAL
			      | FFESYMBOL_attrsSFARG)));	/* Handled above. */
	      assert (!(sa & ~(FFESYMBOL_attrsTYPE
			       | FFESYMBOL_attrsADJUSTABLE
			       | FFESYMBOL_attrsANYLEN
			       | FFESYMBOL_attrsARRAY
			       | FFESYMBOL_attrsDUMMY
			       | FFESYMBOL_attrsEXTERNAL
			       | FFESYMBOL_attrsSFARG)));

	      kind = FFEINFO_kindENTITY;

	      if (sa & (FFESYMBOL_attrsADJUSTABLE | FFESYMBOL_attrsANYLEN))
		na = FFESYMBOL_attrsetNONE;
	      else if (ffest_is_entry_valid ())
		ns = FFESYMBOL_stateUNCERTAIN;	/* Could be DUMMY or LOCAL. */
	      else
		where = FFEINFO_whereLOCAL;
	    }
	  else
	    na = FFESYMBOL_attrsetNONE;	/* Error. */
	}

      /* Now see what we've got for a new object: NONE means a new error
	 cropped up; ANY means an old error to be ignored; otherwise,
	 everything's ok, update the object (symbol) and continue on. */

      if (na == FFESYMBOL_attrsetNONE)
	ffesymbol_error (sp, t);
      else if (!(na & FFESYMBOL_attrsANY))
	{
	  ffesymbol_signal_change (sp);	/* May need to back up to previous
					   version. */
	  if (!ffeimplic_establish_symbol (sp))
	    ffesymbol_error (sp, t);
	  else
	    {
	      ffesymbol_set_info (sp,
				  ffeinfo_new (ffesymbol_basictype (sp),
					       ffesymbol_kindtype (sp),
					       ffesymbol_rank (sp),
					       kind,
					       where,
					       ffesymbol_size (sp)));
	      ffesymbol_set_attrs (sp, na);
	      ffesymbol_set_state (sp, ns);
	      ffesymbol_resolve_intrin (sp);
	      if (!ffesymbol_state_is_specable (ns))
		sp = ffecom_sym_learned (sp);
	      ffesymbol_signal_unreported (sp);	/* For debugging purposes. */
	    }
	}
    }

  /* Here we create the sfunc-name-space symbol representing what should
     become an iterator in this name space at this or an outermore (lower-
     numbered) expression level, else the implied-DO construct is in error.  */

  s = ffesymbol_declare_sfdummy (t);	/* Sets maxentrynum to 0 for new obj;
					   also sets sfa_dummy_parent to
					   parent symbol. */
  assert (sp == ffesymbol_sfdummyparent (s));

  ffesymbol_signal_change (s);
  ffesymbol_set_state (s, FFESYMBOL_stateSEEN);
  ffesymbol_set_maxentrynum (s, ffeexpr_level_);
  ffesymbol_set_info (s,
		      ffeinfo_new (FFEINFO_basictypeINTEGER,
				   FFEINFO_kindtypeINTEGERDEFAULT,
				   0,
				   FFEINFO_kindENTITY,
				   FFEINFO_whereIMMEDIATE,
				   FFETARGET_charactersizeNONE));
  ffesymbol_signal_unreported (s);

  if ((ffesymbol_basictype (sp) != FFEINFO_basictypeINTEGER)
       && (ffesymbol_basictype (sp) != FFEINFO_basictypeANY))
    ffesymbol_error (s, t);

  return s;
}

/* Have FOO in CALL FOO.  Local name space, executable context only.  */

static ffesymbol
ffeexpr_sym_lhs_call_ (ffesymbol s, ffelexToken t)
{
  ffesymbolAttrs sa;
  ffesymbolAttrs na;
  ffeinfoKind kind;
  ffeinfoWhere where;
  ffeintrinGen gen;
  ffeintrinSpec spec;
  ffeintrinImp imp;
  bool error = FALSE;

  assert ((ffesymbol_state (s) == FFESYMBOL_stateNONE)
	  || (ffesymbol_state (s) == FFESYMBOL_stateUNCERTAIN));

  na = sa = ffesymbol_attrs (s);

  assert (!(sa & ~(FFESYMBOL_attrsACTUALARG
		   | FFESYMBOL_attrsADJUSTABLE
		   | FFESYMBOL_attrsANYLEN
		   | FFESYMBOL_attrsARRAY
		   | FFESYMBOL_attrsDUMMY
		   | FFESYMBOL_attrsEXTERNAL
		   | FFESYMBOL_attrsSFARG
		   | FFESYMBOL_attrsTYPE)));

  kind = ffesymbol_kind (s);
  where = ffesymbol_where (s);

  /* Figure out what kind of object we've got based on previous declarations
     of or references to the object. */

  if (sa & FFESYMBOL_attrsEXTERNAL)
    {
      assert (!(sa & ~(FFESYMBOL_attrsACTUALARG
		       | FFESYMBOL_attrsDUMMY
		       | FFESYMBOL_attrsEXTERNAL
		       | FFESYMBOL_attrsTYPE)));

      if (sa & FFESYMBOL_attrsTYPE)
	error = TRUE;
      else
	/* Not TYPE. */
	{
	  kind = FFEINFO_kindSUBROUTINE;

	  if (sa & FFESYMBOL_attrsDUMMY)
	    ;			/* Not TYPE. */
	  else if (sa & FFESYMBOL_attrsACTUALARG)
	    ;			/* Not DUMMY or TYPE. */
	  else			/* Not ACTUALARG, DUMMY, or TYPE. */
	    where = FFEINFO_whereGLOBAL;
	}
    }
  else if (sa & FFESYMBOL_attrsDUMMY)
    {
      assert (!(sa & FFESYMBOL_attrsEXTERNAL));	/* Handled above. */
      assert (!(sa & ~(FFESYMBOL_attrsDUMMY
		       | FFESYMBOL_attrsEXTERNAL
		       | FFESYMBOL_attrsTYPE)));

      if (sa & FFESYMBOL_attrsTYPE)
	error = TRUE;
      else
	kind = FFEINFO_kindSUBROUTINE;
    }
  else if (sa & FFESYMBOL_attrsARRAY)
    {
      assert (!(sa & ~(FFESYMBOL_attrsARRAY
		       | FFESYMBOL_attrsADJUSTABLE
		       | FFESYMBOL_attrsTYPE)));

      error = TRUE;
    }
  else if (sa & FFESYMBOL_attrsSFARG)
    {
      assert (!(sa & ~(FFESYMBOL_attrsSFARG
		       | FFESYMBOL_attrsTYPE)));

      error = TRUE;
    }
  else if (sa & FFESYMBOL_attrsTYPE)
    {
      assert (!(sa & (FFESYMBOL_attrsARRAY
		      | FFESYMBOL_attrsDUMMY
		      | FFESYMBOL_attrsEXTERNAL
		      | FFESYMBOL_attrsSFARG)));	/* Handled above. */
      assert (!(sa & ~(FFESYMBOL_attrsTYPE
		       | FFESYMBOL_attrsADJUSTABLE
		       | FFESYMBOL_attrsANYLEN
		       | FFESYMBOL_attrsARRAY
		       | FFESYMBOL_attrsDUMMY
		       | FFESYMBOL_attrsEXTERNAL
		       | FFESYMBOL_attrsSFARG)));

      error = TRUE;
    }
  else if (sa == FFESYMBOL_attrsetNONE)
    {
      assert (ffesymbol_state (s) == FFESYMBOL_stateNONE);

      if (ffeintrin_is_intrinsic (ffesymbol_text (s), t, FALSE,
				  &gen, &spec, &imp))
	{
	  ffesymbol_signal_change (s);	/* May need to back up to previous
					   version. */
	  ffesymbol_set_generic (s, gen);
	  ffesymbol_set_specific (s, spec);
	  ffesymbol_set_implementation (s, imp);
	  ffesymbol_set_info (s,
			      ffeinfo_new (FFEINFO_basictypeNONE,
					   FFEINFO_kindtypeNONE,
					   0,
					   FFEINFO_kindSUBROUTINE,
					   FFEINFO_whereINTRINSIC,
					   FFETARGET_charactersizeNONE));
	  ffesymbol_set_state (s, FFESYMBOL_stateUNDERSTOOD);
	  ffesymbol_resolve_intrin (s);
	  ffesymbol_reference (s, t, FALSE);
	  s = ffecom_sym_learned (s);
	  ffesymbol_signal_unreported (s);	/* For debugging purposes. */

	  return s;
	}

      kind = FFEINFO_kindSUBROUTINE;
      where = FFEINFO_whereGLOBAL;
    }
  else
    error = TRUE;

  /* Now see what we've got for a new object: NONE means a new error cropped
     up; ANY means an old error to be ignored; otherwise, everything's ok,
     update the object (symbol) and continue on. */

  if (error)
    ffesymbol_error (s, t);
  else if (!(na & FFESYMBOL_attrsANY))
    {
      ffesymbol_signal_change (s);	/* May need to back up to previous
					   version. */
      ffesymbol_set_info (s,
			  ffeinfo_new (ffesymbol_basictype (s),
				       ffesymbol_kindtype (s),
				       ffesymbol_rank (s),
				       kind,	/* SUBROUTINE. */
				       where,	/* GLOBAL or DUMMY. */
				       ffesymbol_size (s)));
      ffesymbol_set_state (s, FFESYMBOL_stateUNDERSTOOD);
      ffesymbol_resolve_intrin (s);
      ffesymbol_reference (s, t, FALSE);
      s = ffecom_sym_learned (s);
      ffesymbol_signal_unreported (s);	/* For debugging purposes. */
    }

  return s;
}

/* Have FOO in DATA FOO/.../.  Local name space and executable context
   only.  (This will change in the future when DATA FOO may be followed
   by COMMON FOO or even INTEGER FOO(10), etc.)  */

static ffesymbol
ffeexpr_sym_lhs_data_ (ffesymbol s, ffelexToken t)
{
  ffesymbolAttrs sa;
  ffesymbolAttrs na;
  ffeinfoKind kind;
  ffeinfoWhere where;
  bool error = FALSE;

  assert ((ffesymbol_state (s) == FFESYMBOL_stateNONE)
	  || (ffesymbol_state (s) == FFESYMBOL_stateUNCERTAIN));

  na = sa = ffesymbol_attrs (s);

  assert (!(sa & ~(FFESYMBOL_attrsACTUALARG
		   | FFESYMBOL_attrsADJUSTABLE
		   | FFESYMBOL_attrsANYLEN
		   | FFESYMBOL_attrsARRAY
		   | FFESYMBOL_attrsDUMMY
		   | FFESYMBOL_attrsEXTERNAL
		   | FFESYMBOL_attrsSFARG
		   | FFESYMBOL_attrsTYPE)));

  kind = ffesymbol_kind (s);
  where = ffesymbol_where (s);

  /* Figure out what kind of object we've got based on previous declarations
     of or references to the object. */

  if (sa & FFESYMBOL_attrsEXTERNAL)
    {
      assert (!(sa & ~(FFESYMBOL_attrsACTUALARG
		       | FFESYMBOL_attrsDUMMY
		       | FFESYMBOL_attrsEXTERNAL
		       | FFESYMBOL_attrsTYPE)));

      error = TRUE;
    }
  else if (sa & FFESYMBOL_attrsDUMMY)
    {
      assert (!(sa & FFESYMBOL_attrsEXTERNAL));	/* Handled above. */
      assert (!(sa & ~(FFESYMBOL_attrsDUMMY
		       | FFESYMBOL_attrsEXTERNAL
		       | FFESYMBOL_attrsTYPE)));

      error = TRUE;
    }
  else if (sa & FFESYMBOL_attrsARRAY)
    {
      assert (!(sa & ~(FFESYMBOL_attrsARRAY
		       | FFESYMBOL_attrsADJUSTABLE
		       | FFESYMBOL_attrsTYPE)));

      if (sa & FFESYMBOL_attrsADJUSTABLE)
	error = TRUE;
      where = FFEINFO_whereLOCAL;
    }
  else if (sa & FFESYMBOL_attrsSFARG)
    {
      assert (!(sa & ~(FFESYMBOL_attrsSFARG
		       | FFESYMBOL_attrsTYPE)));

      where = FFEINFO_whereLOCAL;
    }
  else if (sa & FFESYMBOL_attrsTYPE)
    {
      assert (!(sa & (FFESYMBOL_attrsARRAY
		      | FFESYMBOL_attrsDUMMY
		      | FFESYMBOL_attrsEXTERNAL
		      | FFESYMBOL_attrsSFARG)));	/* Handled above. */
      assert (!(sa & ~(FFESYMBOL_attrsTYPE
		       | FFESYMBOL_attrsADJUSTABLE
		       | FFESYMBOL_attrsANYLEN
		       | FFESYMBOL_attrsARRAY
		       | FFESYMBOL_attrsDUMMY
		       | FFESYMBOL_attrsEXTERNAL
		       | FFESYMBOL_attrsSFARG)));

      if (sa & (FFESYMBOL_attrsADJUSTABLE | FFESYMBOL_attrsANYLEN))
	error = TRUE;
      else
	{
	  kind = FFEINFO_kindENTITY;
	  where = FFEINFO_whereLOCAL;
	}
    }
  else if (sa == FFESYMBOL_attrsetNONE)
    {
      assert (ffesymbol_state (s) == FFESYMBOL_stateNONE);
      kind = FFEINFO_kindENTITY;
      where = FFEINFO_whereLOCAL;
    }
  else
    error = TRUE;

  /* Now see what we've got for a new object: NONE means a new error cropped
     up; ANY means an old error to be ignored; otherwise, everything's ok,
     update the object (symbol) and continue on. */

  if (error)
    ffesymbol_error (s, t);
  else if (!(na & FFESYMBOL_attrsANY))
    {
      ffesymbol_signal_change (s);	/* May need to back up to previous
					   version. */
      if (!ffeimplic_establish_symbol (s))
	{
	  ffesymbol_error (s, t);
	  return s;
	}
      ffesymbol_set_info (s,
			  ffeinfo_new (ffesymbol_basictype (s),
				       ffesymbol_kindtype (s),
				       ffesymbol_rank (s),
				       kind,	/* ENTITY. */
				       where,	/* LOCAL. */
				       ffesymbol_size (s)));
      ffesymbol_set_state (s, FFESYMBOL_stateUNDERSTOOD);
      ffesymbol_resolve_intrin (s);
      s = ffecom_sym_learned (s);
      ffesymbol_signal_unreported (s);	/* For debugging purposes. */
    }

  return s;
}

/* Have FOO in EQUIVALENCE (...,FOO,...).  Does not include
   EQUIVALENCE (...,BAR(FOO),...).  */

static ffesymbol
ffeexpr_sym_lhs_equivalence_ (ffesymbol s, ffelexToken t)
{
  ffesymbolAttrs sa;
  ffesymbolAttrs na;
  ffeinfoKind kind;
  ffeinfoWhere where;

  na = sa = ffesymbol_attrs (s);
  kind = FFEINFO_kindENTITY;
  where = ffesymbol_where (s);

  /* Figure out what kind of object we've got based on previous declarations
     of or references to the object. */

  if (!(sa & ~(FFESYMBOL_attrsADJUSTS
	       | FFESYMBOL_attrsARRAY
	       | FFESYMBOL_attrsCOMMON
	       | FFESYMBOL_attrsEQUIV
	       | FFESYMBOL_attrsINIT
	       | FFESYMBOL_attrsNAMELIST
	       | FFESYMBOL_attrsSAVE
	       | FFESYMBOL_attrsSFARG
	       | FFESYMBOL_attrsTYPE)))
    na = sa | FFESYMBOL_attrsEQUIV;
  else
    na = FFESYMBOL_attrsetNONE;

  /* Don't know why we're bothering to set kind and where in this code, but
     added the following to make it complete, in case it's really important.
     Generally this is left up to symbol exec transition.  */

  if (where == FFEINFO_whereNONE)
    {
      if (na & (FFESYMBOL_attrsADJUSTS
		| FFESYMBOL_attrsCOMMON))
	where = FFEINFO_whereCOMMON;
      else if (na & FFESYMBOL_attrsSAVE)
	where = FFEINFO_whereLOCAL;
    }

  /* Now see what we've got for a new object: NONE means a new error cropped
     up; ANY means an old error to be ignored; otherwise, everything's ok,
     update the object (symbol) and continue on. */

  if (na == FFESYMBOL_attrsetNONE)
    ffesymbol_error (s, t);
  else if (!(na & FFESYMBOL_attrsANY))
    {
      ffesymbol_signal_change (s);	/* May need to back up to previous
					   version. */
      ffesymbol_set_info (s,
			  ffeinfo_new (ffesymbol_basictype (s),
				       ffesymbol_kindtype (s),
				       ffesymbol_rank (s),
				       kind,	/* Always ENTITY. */
				       where,	/* NONE, COMMON, or LOCAL. */
				       ffesymbol_size (s)));
      ffesymbol_set_attrs (s, na);
      ffesymbol_set_state (s, FFESYMBOL_stateSEEN);
      ffesymbol_resolve_intrin (s);
      ffesymbol_signal_unreported (s);	/* For debugging purposes. */
    }

  return s;
}

/* Have FOO in OPEN(...,USEROPEN=FOO,...).  Executable context only.

   Note that I think this should be considered semantically similar to
   doing CALL XYZ(FOO), in that it should be considered like an
   ACTUALARG context.  In particular, without EXTERNAL being specified,
   it should not be allowed.  */

static ffesymbol
ffeexpr_sym_lhs_extfunc_ (ffesymbol s, ffelexToken t)
{
  ffesymbolAttrs sa;
  ffesymbolAttrs na;
  ffeinfoKind kind;
  ffeinfoWhere where;
  bool needs_type = FALSE;
  bool error = FALSE;

  assert ((ffesymbol_state (s) == FFESYMBOL_stateNONE)
	  || (ffesymbol_state (s) == FFESYMBOL_stateUNCERTAIN));

  na = sa = ffesymbol_attrs (s);

  assert (!(sa & ~(FFESYMBOL_attrsACTUALARG
		   | FFESYMBOL_attrsADJUSTABLE
		   | FFESYMBOL_attrsANYLEN
		   | FFESYMBOL_attrsARRAY
		   | FFESYMBOL_attrsDUMMY
		   | FFESYMBOL_attrsEXTERNAL
		   | FFESYMBOL_attrsSFARG
		   | FFESYMBOL_attrsTYPE)));

  kind = ffesymbol_kind (s);
  where = ffesymbol_where (s);

  /* Figure out what kind of object we've got based on previous declarations
     of or references to the object. */

  if (sa & FFESYMBOL_attrsEXTERNAL)
    {
      assert (!(sa & ~(FFESYMBOL_attrsACTUALARG
		       | FFESYMBOL_attrsDUMMY
		       | FFESYMBOL_attrsEXTERNAL
		       | FFESYMBOL_attrsTYPE)));

      if (sa & FFESYMBOL_attrsTYPE)
	where = FFEINFO_whereGLOBAL;
      else
	/* Not TYPE. */
	{
	  kind = FFEINFO_kindFUNCTION;
	  needs_type = TRUE;

	  if (sa & FFESYMBOL_attrsDUMMY)
	    ;			/* Not TYPE. */
	  else if (sa & FFESYMBOL_attrsACTUALARG)
	    ;			/* Not DUMMY or TYPE. */
	  else			/* Not ACTUALARG, DUMMY, or TYPE. */
	    where = FFEINFO_whereGLOBAL;
	}
    }
  else if (sa & FFESYMBOL_attrsDUMMY)
    {
      assert (!(sa & FFESYMBOL_attrsEXTERNAL));	/* Handled above. */
      assert (!(sa & ~(FFESYMBOL_attrsDUMMY
		       | FFESYMBOL_attrsEXTERNAL
		       | FFESYMBOL_attrsTYPE)));

      kind = FFEINFO_kindFUNCTION;
      if (!(sa & FFESYMBOL_attrsTYPE))
	needs_type = TRUE;
    }
  else if (sa & FFESYMBOL_attrsARRAY)
    {
      assert (!(sa & ~(FFESYMBOL_attrsARRAY
		       | FFESYMBOL_attrsADJUSTABLE
		       | FFESYMBOL_attrsTYPE)));

      error = TRUE;
    }
  else if (sa & FFESYMBOL_attrsSFARG)
    {
      assert (!(sa & ~(FFESYMBOL_attrsSFARG
		       | FFESYMBOL_attrsTYPE)));

      error = TRUE;
    }
  else if (sa & FFESYMBOL_attrsTYPE)
    {
      assert (!(sa & (FFESYMBOL_attrsARRAY
		      | FFESYMBOL_attrsDUMMY
		      | FFESYMBOL_attrsEXTERNAL
		      | FFESYMBOL_attrsSFARG)));	/* Handled above. */
      assert (!(sa & ~(FFESYMBOL_attrsTYPE
		       | FFESYMBOL_attrsADJUSTABLE
		       | FFESYMBOL_attrsANYLEN
		       | FFESYMBOL_attrsARRAY
		       | FFESYMBOL_attrsDUMMY
		       | FFESYMBOL_attrsEXTERNAL
		       | FFESYMBOL_attrsSFARG)));

      if (sa & (FFESYMBOL_attrsADJUSTABLE | FFESYMBOL_attrsANYLEN))
	error = TRUE;
      else
	{
	  kind = FFEINFO_kindFUNCTION;
	  where = FFEINFO_whereGLOBAL;
	}
    }
  else if (sa == FFESYMBOL_attrsetNONE)
    {
      assert (ffesymbol_state (s) == FFESYMBOL_stateNONE);
      kind = FFEINFO_kindFUNCTION;
      where = FFEINFO_whereGLOBAL;
      needs_type = TRUE;
    }
  else
    error = TRUE;

  /* Now see what we've got for a new object: NONE means a new error cropped
     up; ANY means an old error to be ignored; otherwise, everything's ok,
     update the object (symbol) and continue on. */

  if (error)
    ffesymbol_error (s, t);
  else if (!(na & FFESYMBOL_attrsANY))
    {
      ffesymbol_signal_change (s);	/* May need to back up to previous
					   version. */
      if (needs_type && !ffeimplic_establish_symbol (s))
	{
	  ffesymbol_error (s, t);
	  return s;
	}
      if (!ffesymbol_explicitwhere (s))
	{
	  ffebad_start (FFEBAD_NEED_EXTERNAL);
	  ffebad_here (0, ffelex_token_where_line (t),
		       ffelex_token_where_column (t));
	  ffebad_string (ffesymbol_text (s));
	  ffebad_finish ();
	  ffesymbol_set_explicitwhere (s, TRUE);
	}
      ffesymbol_set_info (s,
			  ffeinfo_new (ffesymbol_basictype (s),
				       ffesymbol_kindtype (s),
				       ffesymbol_rank (s),
				       kind,	/* FUNCTION. */
				       where,	/* GLOBAL or DUMMY. */
				       ffesymbol_size (s)));
      ffesymbol_set_state (s, FFESYMBOL_stateUNDERSTOOD);
      ffesymbol_resolve_intrin (s);
      ffesymbol_reference (s, t, FALSE);
      s = ffecom_sym_learned (s);
      ffesymbol_signal_unreported (s);	/* For debugging purposes. */
    }

  return s;
}

/* Have FOO in DATA (stuff,FOO=1,10)/.../.  */

static ffesymbol
ffeexpr_sym_lhs_impdoctrl_ (ffesymbol s, ffelexToken t)
{
  ffesymbolState ss;

  /* If the symbol isn't in the sfunc name space, pretend as though we saw a
     reference to it already within the imp-DO construct at this level, so as
     to get a symbol that is in the sfunc name space. But this is an
     erroneous construct, and should be caught elsewhere.  */

  if (ffesymbol_sfdummyparent (s) == NULL)
    {
      s = ffeexpr_sym_impdoitem_ (s, t);
      if (ffesymbol_sfdummyparent (s) == NULL)
	{			/* PARAMETER FOO...DATA (A(I),FOO=...). */
	  ffesymbol_error (s, t);
	  return s;
	}
    }

  ss = ffesymbol_state (s);

  switch (ss)
    {
    case FFESYMBOL_stateNONE:	/* Used as iterator already. */
      if (ffeexpr_level_ < ffesymbol_maxentrynum (s))
	ffesymbol_error (s, t);	/* Can't reuse dead iterator.  F90 disallows
				   this; F77 allows it but it is a stupid
				   feature. */
      else
	{			/* Can use dead iterator because we're at at
				   least a innermore (higher-numbered) level
				   than the iterator's outermost
				   (lowest-numbered) level.  This should be
				   diagnosed later, because it means an item
				   in this list didn't reference this
				   iterator. */
#if 1
	  ffesymbol_error (s, t);	/* For now, complain. */
#else /* Someday will detect all cases where initializer doesn't reference
	 all applicable iterators, in which case reenable this code. */
	  ffesymbol_signal_change (s);
	  ffesymbol_set_state (s, FFESYMBOL_stateUNCERTAIN);
	  ffesymbol_set_maxentrynum (s, ffeexpr_level_);
	  ffesymbol_signal_unreported (s);
#endif
	}
      break;

    case FFESYMBOL_stateSEEN:	/* Seen already in this or other implied-DO.
				   If seen in outermore level, can't be an
				   iterator here, so complain.  If not seen
				   at current level, complain for now,
				   because that indicates something F90
				   rejects (though we currently don't detect
				   all such cases for now). */
      if (ffeexpr_level_ <= ffesymbol_maxentrynum (s))
	{
	  ffesymbol_signal_change (s);
	  ffesymbol_set_state (s, FFESYMBOL_stateUNCERTAIN);
	  ffesymbol_signal_unreported (s);
	}
      else
	ffesymbol_error (s, t);
      break;

    case FFESYMBOL_stateUNCERTAIN:	/* Already iterator! */
      assert ("DATA implied-DO control var seen twice!!" == NULL);
      ffesymbol_error (s, t);
      break;

    case FFESYMBOL_stateUNDERSTOOD:
      break;			/* ANY. */

    default:
      assert ("Foo Bletch!!" == NULL);
      break;
    }

  return s;
}

/* Have FOO in PARAMETER (FOO=...).  */

static ffesymbol
ffeexpr_sym_lhs_parameter_ (ffesymbol s, ffelexToken t)
{
  ffesymbolAttrs sa;

  sa = ffesymbol_attrs (s);

  /* Figure out what kind of object we've got based on previous declarations
     of or references to the object. */

  if (sa & ~(FFESYMBOL_attrsANYLEN
	     | FFESYMBOL_attrsTYPE))
    {
      if (!(sa & FFESYMBOL_attrsANY))
	ffesymbol_error (s, t);
    }
  else
    {
      ffesymbol_signal_change (s);	/* May need to back up to previous
					   version. */
      if (!ffeimplic_establish_symbol (s))
	{
	  ffesymbol_error (s, t);
	  return s;
	}
      ffesymbol_set_info (s,
			  ffeinfo_new (ffesymbol_basictype (s),
				       ffesymbol_kindtype (s),
				       ffesymbol_rank (s),
				       FFEINFO_kindENTITY,
				       FFEINFO_whereCONSTANT,
				       ffesymbol_size (s)));
      ffesymbol_set_state (s, FFESYMBOL_stateUNDERSTOOD);
      ffesymbol_resolve_intrin (s);
      s = ffecom_sym_learned (s);
      ffesymbol_signal_unreported (s);	/* For debugging purposes. */
    }

  return s;
}

/* Have FOO in CALL XYZ(...,FOO,...).  Does not include any other
   embedding of FOO, such as CALL XYZ((FOO)) or CALL XYZ(FOO+1).  */

static ffesymbol
ffeexpr_sym_rhs_actualarg_ (ffesymbol s, ffelexToken t)
{
  ffesymbolAttrs sa;
  ffesymbolAttrs na;
  ffeinfoKind kind;
  ffeinfoWhere where;
  ffesymbolState ns;
  bool needs_type = FALSE;

  assert ((ffesymbol_state (s) == FFESYMBOL_stateNONE)
	  || (ffesymbol_state (s) == FFESYMBOL_stateUNCERTAIN));

  na = sa = ffesymbol_attrs (s);

  assert (!(sa & ~(FFESYMBOL_attrsACTUALARG
		   | FFESYMBOL_attrsADJUSTABLE
		   | FFESYMBOL_attrsANYLEN
		   | FFESYMBOL_attrsARRAY
		   | FFESYMBOL_attrsDUMMY
		   | FFESYMBOL_attrsEXTERNAL
		   | FFESYMBOL_attrsSFARG
		   | FFESYMBOL_attrsTYPE)));

  kind = ffesymbol_kind (s);
  where = ffesymbol_where (s);

  /* Figure out what kind of object we've got based on previous declarations
     of or references to the object. */

  ns = FFESYMBOL_stateUNDERSTOOD;

  if (sa & FFESYMBOL_attrsEXTERNAL)
    {
      assert (!(sa & ~(FFESYMBOL_attrsACTUALARG
		       | FFESYMBOL_attrsDUMMY
		       | FFESYMBOL_attrsEXTERNAL
		       | FFESYMBOL_attrsTYPE)));

      if (sa & FFESYMBOL_attrsTYPE)
	where = FFEINFO_whereGLOBAL;
      else
	/* Not TYPE. */
	{
	  ns = FFESYMBOL_stateUNCERTAIN;

	  if (sa & FFESYMBOL_attrsDUMMY)
	    assert (kind == FFEINFO_kindNONE);	/* FUNCTION, SUBROUTINE. */
	  else if (sa & FFESYMBOL_attrsACTUALARG)
	    ;			/* Not DUMMY or TYPE. */
	  else
	    /* Not ACTUALARG, DUMMY, or TYPE. */
	    {
	      assert (kind == FFEINFO_kindNONE);	/* FUNCTION, SUBROUTINE. */
	      na |= FFESYMBOL_attrsACTUALARG;
	      where = FFEINFO_whereGLOBAL;
	    }
	}
    }
  else if (sa & FFESYMBOL_attrsDUMMY)
    {
      assert (!(sa & FFESYMBOL_attrsEXTERNAL));	/* Handled above. */
      assert (!(sa & ~(FFESYMBOL_attrsDUMMY
		       | FFESYMBOL_attrsEXTERNAL
		       | FFESYMBOL_attrsTYPE)));

      kind = FFEINFO_kindENTITY;
      if (!(sa & FFESYMBOL_attrsTYPE))
	needs_type = TRUE;
    }
  else if (sa & FFESYMBOL_attrsARRAY)
    {
      assert (!(sa & ~(FFESYMBOL_attrsARRAY
		       | FFESYMBOL_attrsADJUSTABLE
		       | FFESYMBOL_attrsTYPE)));

      where = FFEINFO_whereLOCAL;
    }
  else if (sa & FFESYMBOL_attrsSFARG)
    {
      assert (!(sa & ~(FFESYMBOL_attrsSFARG
		       | FFESYMBOL_attrsTYPE)));

      where = FFEINFO_whereLOCAL;
    }
  else if (sa & FFESYMBOL_attrsTYPE)
    {
      assert (!(sa & (FFESYMBOL_attrsARRAY
		      | FFESYMBOL_attrsDUMMY
		      | FFESYMBOL_attrsEXTERNAL
		      | FFESYMBOL_attrsSFARG)));	/* Handled above. */
      assert (!(sa & ~(FFESYMBOL_attrsTYPE
		       | FFESYMBOL_attrsADJUSTABLE
		       | FFESYMBOL_attrsANYLEN
		       | FFESYMBOL_attrsARRAY
		       | FFESYMBOL_attrsDUMMY
		       | FFESYMBOL_attrsEXTERNAL
		       | FFESYMBOL_attrsSFARG)));

      if (sa & FFESYMBOL_attrsANYLEN)
	ns = FFESYMBOL_stateNONE;
      else
	{
	  kind = FFEINFO_kindENTITY;
	  where = FFEINFO_whereLOCAL;
	}
    }
  else if (sa == FFESYMBOL_attrsetNONE)
    {
      /* New state is left empty because there isn't any state flag to
	 set for this case, and it's UNDERSTOOD after all.  */
      assert (ffesymbol_state (s) == FFESYMBOL_stateNONE);
      kind = FFEINFO_kindENTITY;
      where = FFEINFO_whereLOCAL;
      needs_type = TRUE;
    }
  else
    ns = FFESYMBOL_stateNONE;	/* Error. */

  /* Now see what we've got for a new object: NONE means a new error cropped
     up; ANY means an old error to be ignored; otherwise, everything's ok,
     update the object (symbol) and continue on. */

  if (ns == FFESYMBOL_stateNONE)
    ffesymbol_error (s, t);
  else if (!(na & FFESYMBOL_attrsANY))
    {
      ffesymbol_signal_change (s);	/* May need to back up to previous
					   version. */
      if (needs_type && !ffeimplic_establish_symbol (s))
	{
	  ffesymbol_error (s, t);
	  return s;
	}
      ffesymbol_set_info (s,
			  ffeinfo_new (ffesymbol_basictype (s),
				       ffesymbol_kindtype (s),
				       ffesymbol_rank (s),
				       kind,
				       where,
				       ffesymbol_size (s)));
      ffesymbol_set_attrs (s, na);
      ffesymbol_set_state (s, ns);
      s = ffecom_sym_learned (s);
      ffesymbol_reference (s, t, FALSE);
      ffesymbol_signal_unreported (s);	/* For debugging purposes. */
    }

  return s;
}

/* Have FOO in DIMENSION XYZ(FOO) or any array declarator containing
   a reference to FOO.  */

static ffesymbol
ffeexpr_sym_rhs_dimlist_ (ffesymbol s, ffelexToken t)
{
  ffesymbolAttrs sa;
  ffesymbolAttrs na;
  ffeinfoKind kind;
  ffeinfoWhere where;

  na = sa = ffesymbol_attrs (s);
  kind = FFEINFO_kindENTITY;
  where = ffesymbol_where (s);

  /* Figure out what kind of object we've got based on previous declarations
     of or references to the object. */

  if (!(sa & ~(FFESYMBOL_attrsADJUSTS
	       | FFESYMBOL_attrsCOMMON
	       | FFESYMBOL_attrsDUMMY
	       | FFESYMBOL_attrsEQUIV
	       | FFESYMBOL_attrsINIT
	       | FFESYMBOL_attrsNAMELIST
	       | FFESYMBOL_attrsSFARG
               | FFESYMBOL_attrsARRAY
	       | FFESYMBOL_attrsTYPE)))
    na = sa | FFESYMBOL_attrsADJUSTS;
  else
    na = FFESYMBOL_attrsetNONE;

  /* Since this symbol definitely is going into an expression (the
     dimension-list for some dummy array, presumably), figure out WHERE if
     possible.  */

  if (where == FFEINFO_whereNONE)
    {
      if (na & (FFESYMBOL_attrsCOMMON
		| FFESYMBOL_attrsEQUIV
		| FFESYMBOL_attrsINIT
		| FFESYMBOL_attrsNAMELIST))
	where = FFEINFO_whereCOMMON;
      else if (na & FFESYMBOL_attrsDUMMY)
	where = FFEINFO_whereDUMMY;
    }

  /* Now see what we've got for a new object: NONE means a new error cropped
     up; ANY means an old error to be ignored; otherwise, everything's ok,
     update the object (symbol) and continue on. */

  if (na == FFESYMBOL_attrsetNONE)
    ffesymbol_error (s, t);
  else if (!(na & FFESYMBOL_attrsANY))
    {
      ffesymbol_signal_change (s);	/* May need to back up to previous
					   version. */
      if (!ffeimplic_establish_symbol (s))
	{
	  ffesymbol_error (s, t);
	  return s;
	}
      ffesymbol_set_info (s,
			  ffeinfo_new (ffesymbol_basictype (s),
				       ffesymbol_kindtype (s),
				       ffesymbol_rank (s),
				       kind,	/* Always ENTITY. */
				       where,	/* NONE, COMMON, or DUMMY. */
				       ffesymbol_size (s)));
      ffesymbol_set_attrs (s, na);
      ffesymbol_set_state (s, FFESYMBOL_stateSEEN);
      ffesymbol_resolve_intrin (s);
      ffesymbol_signal_unreported (s);	/* For debugging purposes. */
    }

  return s;
}

/* Have FOO in XYZ = ...FOO....  Does not include cases like FOO in
   XYZ = BAR(FOO), as such cases are handled elsewhere.  */

static ffesymbol
ffeexpr_sym_rhs_let_ (ffesymbol s, ffelexToken t)
{
  ffesymbolAttrs sa;
  ffesymbolAttrs na;
  ffeinfoKind kind;
  ffeinfoWhere where;
  bool error = FALSE;

  assert ((ffesymbol_state (s) == FFESYMBOL_stateNONE)
	  || (ffesymbol_state (s) == FFESYMBOL_stateUNCERTAIN));

  na = sa = ffesymbol_attrs (s);

  assert (!(sa & ~(FFESYMBOL_attrsACTUALARG
		   | FFESYMBOL_attrsADJUSTABLE
		   | FFESYMBOL_attrsANYLEN
		   | FFESYMBOL_attrsARRAY
		   | FFESYMBOL_attrsDUMMY
		   | FFESYMBOL_attrsEXTERNAL
		   | FFESYMBOL_attrsSFARG
		   | FFESYMBOL_attrsTYPE)));

  kind = ffesymbol_kind (s);
  where = ffesymbol_where (s);

  /* Figure out what kind of object we've got based on previous declarations
     of or references to the object. */

  if (sa & FFESYMBOL_attrsEXTERNAL)
    {
      assert (!(sa & ~(FFESYMBOL_attrsACTUALARG
		       | FFESYMBOL_attrsDUMMY
		       | FFESYMBOL_attrsEXTERNAL
		       | FFESYMBOL_attrsTYPE)));

      error = TRUE;
    }
  else if (sa & FFESYMBOL_attrsDUMMY)
    {
      assert (!(sa & FFESYMBOL_attrsEXTERNAL));	/* Handled above. */
      assert (!(sa & ~(FFESYMBOL_attrsDUMMY
		       | FFESYMBOL_attrsEXTERNAL
		       | FFESYMBOL_attrsTYPE)));

      kind = FFEINFO_kindENTITY;
    }
  else if (sa & FFESYMBOL_attrsARRAY)
    {
      assert (!(sa & ~(FFESYMBOL_attrsARRAY
		       | FFESYMBOL_attrsADJUSTABLE
		       | FFESYMBOL_attrsTYPE)));

      where = FFEINFO_whereLOCAL;
    }
  else if (sa & FFESYMBOL_attrsSFARG)
    {
      assert (!(sa & ~(FFESYMBOL_attrsSFARG
		       | FFESYMBOL_attrsTYPE)));

      where = FFEINFO_whereLOCAL;
    }
  else if (sa & FFESYMBOL_attrsTYPE)
    {
      assert (!(sa & (FFESYMBOL_attrsARRAY
		      | FFESYMBOL_attrsDUMMY
		      | FFESYMBOL_attrsEXTERNAL
		      | FFESYMBOL_attrsSFARG)));	/* Handled above. */
      assert (!(sa & ~(FFESYMBOL_attrsTYPE
		       | FFESYMBOL_attrsADJUSTABLE
		       | FFESYMBOL_attrsANYLEN
		       | FFESYMBOL_attrsARRAY
		       | FFESYMBOL_attrsDUMMY
		       | FFESYMBOL_attrsEXTERNAL
		       | FFESYMBOL_attrsSFARG)));

      if (sa & FFESYMBOL_attrsANYLEN)
	error = TRUE;
      else
	{
	  kind = FFEINFO_kindENTITY;
	  where = FFEINFO_whereLOCAL;
	}
    }
  else if (sa == FFESYMBOL_attrsetNONE)
    {
      assert (ffesymbol_state (s) == FFESYMBOL_stateNONE);
      kind = FFEINFO_kindENTITY;
      where = FFEINFO_whereLOCAL;
    }
  else
    error = TRUE;

  /* Now see what we've got for a new object: NONE means a new error cropped
     up; ANY means an old error to be ignored; otherwise, everything's ok,
     update the object (symbol) and continue on. */

  if (error)
    ffesymbol_error (s, t);
  else if (!(na & FFESYMBOL_attrsANY))
    {
      ffesymbol_signal_change (s);	/* May need to back up to previous
					   version. */
      if (!ffeimplic_establish_symbol (s))
	{
	  ffesymbol_error (s, t);
	  return s;
	}
      ffesymbol_set_info (s,
			  ffeinfo_new (ffesymbol_basictype (s),
				       ffesymbol_kindtype (s),
				       ffesymbol_rank (s),
				       kind,	/* ENTITY. */
				       where,	/* LOCAL. */
				       ffesymbol_size (s)));
      ffesymbol_set_state (s, FFESYMBOL_stateUNDERSTOOD);
      ffesymbol_resolve_intrin (s);
      s = ffecom_sym_learned (s);
      ffesymbol_signal_unreported (s);	/* For debugging purposes. */
    }

  return s;
}

/* ffeexpr_declare_parenthesized_ -- ffesymbol wrapper for NAME(...) operand

   ffelexToken t;
   bool maybe_intrin;
   ffeexprParenType_ paren_type;
   ffesymbol s;
   s = ffeexpr_declare_parenthesized_ (t, maybe_intrin, &paren_type);

   Just like ffesymbol_declare_local, except performs any implicit info
   assignment necessary, and it returns the type of the parenthesized list
   (list of function args, list of array args, or substring spec).  */

static ffesymbol
ffeexpr_declare_parenthesized_ (ffelexToken t, bool maybe_intrin,
				ffeexprParenType_ *paren_type)
{
  ffesymbol s;
  ffesymbolState st;		/* Effective state. */
  ffeinfoKind k;
  bool bad;

  if (maybe_intrin && ffesrc_check_symbol ())
    {				/* Knock off some easy cases. */
      switch (ffeexpr_stack_->context)
	{
	case FFEEXPR_contextSUBROUTINEREF:
	case FFEEXPR_contextDATA:
	case FFEEXPR_contextDATAIMPDOINDEX_:
	case FFEEXPR_contextSFUNCDEF:
	case FFEEXPR_contextSFUNCDEFINDEX_:
	case FFEEXPR_contextSFUNCDEFACTUALARGEXPR_:
	case FFEEXPR_contextSFUNCDEFINDEXORACTUALARGEXPR_:
	case FFEEXPR_contextLET:
	case FFEEXPR_contextPAREN_:
	case FFEEXPR_contextACTUALARGEXPR_:
	case FFEEXPR_contextINDEXORACTUALARGEXPR_:
	case FFEEXPR_contextIOLIST:
	case FFEEXPR_contextIOLISTDF:
	case FFEEXPR_contextDO:
	case FFEEXPR_contextDOWHILE:
	case FFEEXPR_contextACTUALARG_:
	case FFEEXPR_contextCGOTO:
	case FFEEXPR_contextIF:
	case FFEEXPR_contextARITHIF:
	case FFEEXPR_contextFORMAT:
	case FFEEXPR_contextSTOP:
	case FFEEXPR_contextRETURN:
	case FFEEXPR_contextSELECTCASE:
	case FFEEXPR_contextCASE:
	case FFEEXPR_contextFILEASSOC:
	case FFEEXPR_contextFILEINT:
	case FFEEXPR_contextFILEDFINT:
	case FFEEXPR_contextFILELOG:
	case FFEEXPR_contextFILENUM:
	case FFEEXPR_contextFILENUMAMBIG:
	case FFEEXPR_contextFILECHAR:
	case FFEEXPR_contextFILENUMCHAR:
	case FFEEXPR_contextFILEDFCHAR:
	case FFEEXPR_contextFILEKEY:
	case FFEEXPR_contextFILEUNIT:
	case FFEEXPR_contextFILEUNIT_DF:
	case FFEEXPR_contextFILEUNITAMBIG:
	case FFEEXPR_contextFILEFORMAT:
	case FFEEXPR_contextFILENAMELIST:
	case FFEEXPR_contextFILEVXTCODE:
	case FFEEXPR_contextINDEX_:
	case FFEEXPR_contextIMPDOITEM_:
	case FFEEXPR_contextIMPDOITEMDF_:
	case FFEEXPR_contextIMPDOCTRL_:
	case FFEEXPR_contextDATAIMPDOCTRL_:
	case FFEEXPR_contextCHARACTERSIZE:
	case FFEEXPR_contextPARAMETER:
	case FFEEXPR_contextDIMLIST:
	case FFEEXPR_contextDIMLISTCOMMON:
	case FFEEXPR_contextKINDTYPE:
	case FFEEXPR_contextINITVAL:
	case FFEEXPR_contextEQVINDEX_:
	  break;		/* These could be intrinsic invocations. */

	case FFEEXPR_contextAGOTO:
	case FFEEXPR_contextFILEFORMATNML:
	case FFEEXPR_contextALLOCATE:
	case FFEEXPR_contextDEALLOCATE:
	case FFEEXPR_contextHEAPSTAT:
	case FFEEXPR_contextNULLIFY:
	case FFEEXPR_contextINCLUDE:
	case FFEEXPR_contextDATAIMPDOITEM_:
	case FFEEXPR_contextLOC_:
	case FFEEXPR_contextINDEXORACTUALARG_:
	case FFEEXPR_contextSFUNCDEFACTUALARG_:
	case FFEEXPR_contextSFUNCDEFINDEXORACTUALARG_:
	case FFEEXPR_contextPARENFILENUM_:
	case FFEEXPR_contextPARENFILEUNIT_:
	  maybe_intrin = FALSE;
	  break;		/* Can't be intrinsic invocation. */

	default:
	  assert ("blah! blah! waaauuggh!" == NULL);
	  break;
	}
    }

  s = ffesymbol_declare_local (t, maybe_intrin);

  switch (ffeexpr_context_outer_ (ffeexpr_stack_))
    /* Special-case these since they can involve a different concept
       of "state" (in the stmtfunc name space).  */
    {
    case FFEEXPR_contextDATAIMPDOINDEX_:
    case FFEEXPR_contextDATAIMPDOCTRL_:
      if (ffeexpr_context_outer_ (ffeexpr_stack_)
	  == FFEEXPR_contextDATAIMPDOINDEX_)
	s = ffeexpr_sym_impdoitem_ (s, t);
      else
	if (ffeexpr_stack_->is_rhs)
	  s = ffeexpr_sym_impdoitem_ (s, t);
	else
	  s = ffeexpr_sym_lhs_impdoctrl_ (s, t);
      if (ffesymbol_kind (s) != FFEINFO_kindANY)
	ffesymbol_error (s, t);
      return s;

    default:
      break;
    }

  switch ((ffesymbol_sfdummyparent (s) == NULL)
	  ? ffesymbol_state (s)
	  : FFESYMBOL_stateUNDERSTOOD)
    {
    case FFESYMBOL_stateNONE:	/* Before first exec, not seen in expr
				   context. */
      if (!ffest_seen_first_exec ())
	goto seen;		/* :::::::::::::::::::: */
      /* Fall through. */
    case FFESYMBOL_stateUNCERTAIN:	/* Unseen since first exec. */
      switch (ffeexpr_context_outer_ (ffeexpr_stack_))
	{
	case FFEEXPR_contextSUBROUTINEREF:
	  s = ffeexpr_sym_lhs_call_ (s, t);	/* "CALL FOO"=="CALL
						   FOO(...)". */
	  break;

	case FFEEXPR_contextDATA:
	  if (ffeexpr_stack_->is_rhs)
	    s = ffeexpr_sym_rhs_let_ (s, t);
	  else
	    s = ffeexpr_sym_lhs_data_ (s, t);
	  break;

	case FFEEXPR_contextDATAIMPDOITEM_:
	  s = ffeexpr_sym_lhs_data_ (s, t);
	  break;

	case FFEEXPR_contextSFUNCDEF:
	case FFEEXPR_contextSFUNCDEFINDEX_:
	case FFEEXPR_contextSFUNCDEFACTUALARGEXPR_:
	case FFEEXPR_contextSFUNCDEFINDEXORACTUALARGEXPR_:
	  s = ffecom_sym_exec_transition (s);
	  if (ffesymbol_state (s) == FFESYMBOL_stateUNDERSTOOD)
	    goto understood;	/* :::::::::::::::::::: */
	  /* Fall through. */
	case FFEEXPR_contextLET:
	case FFEEXPR_contextPAREN_:
	case FFEEXPR_contextACTUALARGEXPR_:
	case FFEEXPR_contextINDEXORACTUALARGEXPR_:
	case FFEEXPR_contextIOLIST:
	case FFEEXPR_contextIOLISTDF:
	case FFEEXPR_contextDO:
	case FFEEXPR_contextDOWHILE:
	case FFEEXPR_contextACTUALARG_:
	case FFEEXPR_contextCGOTO:
	case FFEEXPR_contextIF:
	case FFEEXPR_contextARITHIF:
	case FFEEXPR_contextFORMAT:
	case FFEEXPR_contextSTOP:
	case FFEEXPR_contextRETURN:
	case FFEEXPR_contextSELECTCASE:
	case FFEEXPR_contextCASE:
	case FFEEXPR_contextFILEASSOC:
	case FFEEXPR_contextFILEINT:
	case FFEEXPR_contextFILEDFINT:
	case FFEEXPR_contextFILELOG:
	case FFEEXPR_contextFILENUM:
	case FFEEXPR_contextFILENUMAMBIG:
	case FFEEXPR_contextFILECHAR:
	case FFEEXPR_contextFILENUMCHAR:
	case FFEEXPR_contextFILEDFCHAR:
	case FFEEXPR_contextFILEKEY:
	case FFEEXPR_contextFILEUNIT:
	case FFEEXPR_contextFILEUNIT_DF:
	case FFEEXPR_contextFILEUNITAMBIG:
	case FFEEXPR_contextFILEFORMAT:
	case FFEEXPR_contextFILENAMELIST:
	case FFEEXPR_contextFILEVXTCODE:
	case FFEEXPR_contextINDEX_:
	case FFEEXPR_contextIMPDOITEM_:
	case FFEEXPR_contextIMPDOITEMDF_:
	case FFEEXPR_contextIMPDOCTRL_:
	case FFEEXPR_contextLOC_:
	  if (ffeexpr_stack_->is_rhs)
	    s = ffeexpr_paren_rhs_let_ (s, t);
	  else
	    s = ffeexpr_paren_lhs_let_ (s, t);
	  break;

	case FFEEXPR_contextASSIGN:
	case FFEEXPR_contextAGOTO:
	case FFEEXPR_contextCHARACTERSIZE:
	case FFEEXPR_contextEQUIVALENCE:
	case FFEEXPR_contextINCLUDE:
	case FFEEXPR_contextPARAMETER:
	case FFEEXPR_contextDIMLIST:
	case FFEEXPR_contextDIMLISTCOMMON:
	case FFEEXPR_contextKINDTYPE:
	case FFEEXPR_contextINITVAL:
	case FFEEXPR_contextEQVINDEX_:
	  break;		/* Will turn into errors below. */

	default:
	  ffesymbol_error (s, t);
	  break;
	}
      /* Fall through. */
    case FFESYMBOL_stateUNDERSTOOD:	/* Nothing much more to learn. */
    understood:		/* :::::::::::::::::::: */

      /* State might have changed, update it.  */
      st = ((ffesymbol_sfdummyparent (s) == NULL)
	    ? ffesymbol_state (s)
	    : FFESYMBOL_stateUNDERSTOOD);

      k = ffesymbol_kind (s);
      switch (ffeexpr_context_outer_ (ffeexpr_stack_))
	{
	case FFEEXPR_contextSUBROUTINEREF:
	  bad = ((k != FFEINFO_kindSUBROUTINE)
		 && ((ffesymbol_where (s) != FFEINFO_whereINTRINSIC)
		     || (k != FFEINFO_kindNONE)));
	  break;

	case FFEEXPR_contextDATA:
	  if (ffeexpr_stack_->is_rhs)
	    bad = (k != FFEINFO_kindENTITY)
	      || (ffesymbol_where (s) != FFEINFO_whereCONSTANT);
	  else
	    bad = (k != FFEINFO_kindENTITY)
	      || ((ffesymbol_where (s) != FFEINFO_whereNONE)
		  && (ffesymbol_where (s) != FFEINFO_whereLOCAL)
		  && (ffesymbol_where (s) != FFEINFO_whereCOMMON));
	  break;

	case FFEEXPR_contextDATAIMPDOITEM_:
	  bad = (k != FFEINFO_kindENTITY) || (ffesymbol_rank (s) == 0)
	    || ((ffesymbol_where (s) != FFEINFO_whereNONE)
		&& (ffesymbol_where (s) != FFEINFO_whereLOCAL)
		&& (ffesymbol_where (s) != FFEINFO_whereCOMMON));
	  break;

	case FFEEXPR_contextSFUNCDEF:
	case FFEEXPR_contextSFUNCDEFINDEX_:
	case FFEEXPR_contextSFUNCDEFACTUALARGEXPR_:
	case FFEEXPR_contextSFUNCDEFINDEXORACTUALARGEXPR_:
	case FFEEXPR_contextLET:
	case FFEEXPR_contextPAREN_:
	case FFEEXPR_contextACTUALARGEXPR_:
	case FFEEXPR_contextINDEXORACTUALARGEXPR_:
	case FFEEXPR_contextIOLIST:
	case FFEEXPR_contextIOLISTDF:
	case FFEEXPR_contextDO:
	case FFEEXPR_contextDOWHILE:
	case FFEEXPR_contextACTUALARG_:
	case FFEEXPR_contextCGOTO:
	case FFEEXPR_contextIF:
	case FFEEXPR_contextARITHIF:
	case FFEEXPR_contextFORMAT:
	case FFEEXPR_contextSTOP:
	case FFEEXPR_contextRETURN:
	case FFEEXPR_contextSELECTCASE:
	case FFEEXPR_contextCASE:
	case FFEEXPR_contextFILEASSOC:
	case FFEEXPR_contextFILEINT:
	case FFEEXPR_contextFILEDFINT:
	case FFEEXPR_contextFILELOG:
	case FFEEXPR_contextFILENUM:
	case FFEEXPR_contextFILENUMAMBIG:
	case FFEEXPR_contextFILECHAR:
	case FFEEXPR_contextFILENUMCHAR:
	case FFEEXPR_contextFILEDFCHAR:
	case FFEEXPR_contextFILEKEY:
	case FFEEXPR_contextFILEUNIT:
	case FFEEXPR_contextFILEUNIT_DF:
	case FFEEXPR_contextFILEUNITAMBIG:
	case FFEEXPR_contextFILEFORMAT:
	case FFEEXPR_contextFILENAMELIST:
	case FFEEXPR_contextFILEVXTCODE:
	case FFEEXPR_contextINDEX_:
	case FFEEXPR_contextIMPDOITEM_:
	case FFEEXPR_contextIMPDOITEMDF_:
	case FFEEXPR_contextIMPDOCTRL_:
	case FFEEXPR_contextLOC_:
	  bad = FALSE;		/* Let paren-switch handle the cases. */
	  break;

	case FFEEXPR_contextASSIGN:
	case FFEEXPR_contextAGOTO:
	case FFEEXPR_contextCHARACTERSIZE:
	case FFEEXPR_contextEQUIVALENCE:
	case FFEEXPR_contextPARAMETER:
	case FFEEXPR_contextDIMLIST:
	case FFEEXPR_contextDIMLISTCOMMON:
	case FFEEXPR_contextKINDTYPE:
	case FFEEXPR_contextINITVAL:
	case FFEEXPR_contextEQVINDEX_:
	  bad = (k != FFEINFO_kindENTITY)
	    || (ffesymbol_where (s) != FFEINFO_whereCONSTANT);
	  break;

	case FFEEXPR_contextINCLUDE:
	  bad = TRUE;
	  break;

	default:
	  bad = TRUE;
	  break;
	}

      switch (bad ? FFEINFO_kindANY : k)
	{
	case FFEINFO_kindNONE:	/* Case "CHARACTER X,Y; Y=X(?". */
	  if (ffesymbol_where (s) == FFEINFO_whereINTRINSIC)
	    {
	      if (ffeexpr_context_outer_ (ffeexpr_stack_)
		  == FFEEXPR_contextSUBROUTINEREF)
		*paren_type = FFEEXPR_parentypeSUBROUTINE_;
	      else
		*paren_type = FFEEXPR_parentypeFUNCTION_;
	      break;
	    }
	  if (st == FFESYMBOL_stateUNDERSTOOD)
	    {
	      bad = TRUE;
	      *paren_type = FFEEXPR_parentypeANY_;
	    }
	  else
	    *paren_type = FFEEXPR_parentypeFUNSUBSTR_;
	  break;

	case FFEINFO_kindFUNCTION:
	  *paren_type = FFEEXPR_parentypeFUNCTION_;
	  switch (ffesymbol_where (s))
	    {
	    case FFEINFO_whereLOCAL:
	      bad = TRUE;	/* Attempt to recurse! */
	      break;

	    case FFEINFO_whereCONSTANT:
	      bad = ((ffesymbol_sfexpr (s) == NULL)
		     || (ffebld_op (ffesymbol_sfexpr (s))
			 == FFEBLD_opANY));	/* Attempt to recurse! */
	      break;

	    default:
	      break;
	    }
	  break;

	case FFEINFO_kindSUBROUTINE:
	  if ((ffeexpr_stack_->context != FFEEXPR_contextSUBROUTINEREF)
	      || (ffeexpr_stack_->previous != NULL))
	    {
	      bad = TRUE;
	      *paren_type = FFEEXPR_parentypeANY_;
	      break;
	    }

	  *paren_type = FFEEXPR_parentypeSUBROUTINE_;
	  switch (ffesymbol_where (s))
	    {
	    case FFEINFO_whereLOCAL:
	    case FFEINFO_whereCONSTANT:
	      bad = TRUE;	/* Attempt to recurse! */
	      break;

	    default:
	      break;
	    }
	  break;

	case FFEINFO_kindENTITY:
	  if (ffesymbol_rank (s) == 0)
	    {
	      if (ffesymbol_basictype (s) == FFEINFO_basictypeCHARACTER)
		*paren_type = FFEEXPR_parentypeSUBSTRING_;
	      else
		{
		  bad = TRUE;
		  *paren_type = FFEEXPR_parentypeANY_;
		}
	    }
	  else
	    *paren_type = FFEEXPR_parentypeARRAY_;
	  break;

	default:
	case FFEINFO_kindANY:
	  bad = TRUE;
	  *paren_type = FFEEXPR_parentypeANY_;
	  break;
	}

      if (bad)
	{
	  if (k == FFEINFO_kindANY)
	    ffest_shutdown ();
	  else
	    ffesymbol_error (s, t);
	}

      return s;

    case FFESYMBOL_stateSEEN:	/* Seen but not yet in exec portion. */
    seen:			/* :::::::::::::::::::: */
      bad = TRUE;
      switch (ffeexpr_context_outer_ (ffeexpr_stack_))
	{
	case FFEEXPR_contextPARAMETER:
	  if (ffeexpr_stack_->is_rhs)
	    ffesymbol_error (s, t);
	  else
	    s = ffeexpr_sym_lhs_parameter_ (s, t);
	  break;

	case FFEEXPR_contextDATA:
	  s = ffecom_sym_exec_transition (s);
	  if (ffesymbol_state (s) == FFESYMBOL_stateUNDERSTOOD)
	    goto understood;	/* :::::::::::::::::::: */
	  if (ffeexpr_stack_->is_rhs)
	    ffesymbol_error (s, t);
	  else
	    s = ffeexpr_sym_lhs_data_ (s, t);
	  goto understood;	/* :::::::::::::::::::: */

	case FFEEXPR_contextDATAIMPDOITEM_:
	  s = ffecom_sym_exec_transition (s);
	  if (ffesymbol_state (s) == FFESYMBOL_stateUNDERSTOOD)
	    goto understood;	/* :::::::::::::::::::: */
	  s = ffeexpr_sym_lhs_data_ (s, t);
	  goto understood;	/* :::::::::::::::::::: */

	case FFEEXPR_contextEQUIVALENCE:
	  s = ffeexpr_sym_lhs_equivalence_ (s, t);
	  bad = FALSE;
	  break;

	case FFEEXPR_contextDIMLIST:
	  s = ffeexpr_sym_rhs_dimlist_ (s, t);
          bad = FALSE;
	  break;

	case FFEEXPR_contextCHARACTERSIZE:
	case FFEEXPR_contextKINDTYPE:
	case FFEEXPR_contextDIMLISTCOMMON:
	case FFEEXPR_contextINITVAL:
	case FFEEXPR_contextEQVINDEX_:
	  break;

	case FFEEXPR_contextINCLUDE:
	  break;

	case FFEEXPR_contextINDEX_:
	case FFEEXPR_contextACTUALARGEXPR_:
	case FFEEXPR_contextINDEXORACTUALARGEXPR_:
	case FFEEXPR_contextSFUNCDEF:
	case FFEEXPR_contextSFUNCDEFINDEX_:
	case FFEEXPR_contextSFUNCDEFACTUALARGEXPR_:
	case FFEEXPR_contextSFUNCDEFINDEXORACTUALARGEXPR_:
	  assert (ffeexpr_stack_->is_rhs);
	  s = ffecom_sym_exec_transition (s);
	  if (ffesymbol_state (s) == FFESYMBOL_stateUNDERSTOOD)
	    goto understood;	/* :::::::::::::::::::: */
	  s = ffeexpr_paren_rhs_let_ (s, t);
	  goto understood;	/* :::::::::::::::::::: */

	default:
	  break;
	}
      k = ffesymbol_kind (s);
      switch (bad ? FFEINFO_kindANY : k)
	{
	case FFEINFO_kindNONE:	/* Case "CHARACTER X,Y; Y=X(?". */
	  *paren_type = FFEEXPR_parentypeFUNSUBSTR_;
	  break;

	case FFEINFO_kindFUNCTION:
	  *paren_type = FFEEXPR_parentypeFUNCTION_;
	  switch (ffesymbol_where (s))
	    {
	    case FFEINFO_whereLOCAL:
	      bad = TRUE;	/* Attempt to recurse! */
	      break;

	    case FFEINFO_whereCONSTANT:
	      bad = ((ffesymbol_sfexpr (s) == NULL)
		     || (ffebld_op (ffesymbol_sfexpr (s))
			 == FFEBLD_opANY));	/* Attempt to recurse! */
	      break;

	    default:
	      break;
	    }
	  break;

	case FFEINFO_kindSUBROUTINE:
	  *paren_type = FFEEXPR_parentypeANY_;
	  bad = TRUE;		/* Cannot possibly be in
				   contextSUBROUTINEREF. */
	  break;

	case FFEINFO_kindENTITY:
	  if (ffesymbol_rank (s) == 0)
	    {
	      if (ffeexpr_stack_->context == FFEEXPR_contextEQUIVALENCE)
		*paren_type = FFEEXPR_parentypeEQUIVALENCE_;
	      else if (ffesymbol_basictype (s) == FFEINFO_basictypeCHARACTER)
		*paren_type = FFEEXPR_parentypeSUBSTRING_;
	      else
		{
		  bad = TRUE;
		  *paren_type = FFEEXPR_parentypeANY_;
		}
	    }
	  else
	    *paren_type = FFEEXPR_parentypeARRAY_;
	  break;

	default:
	case FFEINFO_kindANY:
	  bad = TRUE;
	  *paren_type = FFEEXPR_parentypeANY_;
	  break;
	}

      if (bad)
	{
	  if (k == FFEINFO_kindANY)
	    ffest_shutdown ();
	  else
	    ffesymbol_error (s, t);
	}

      return s;

    default:
      assert ("bad symbol state" == NULL);
      return NULL;
    }
}

/* Have FOO in XYZ = ...FOO(...)....  Executable context only.  */

static ffesymbol
ffeexpr_paren_rhs_let_ (ffesymbol s, ffelexToken t)
{
  ffesymbolAttrs sa;
  ffesymbolAttrs na;
  ffeinfoKind kind;
  ffeinfoWhere where;
  ffeintrinGen gen;
  ffeintrinSpec spec;
  ffeintrinImp imp;
  bool maybe_ambig = FALSE;
  bool error = FALSE;

  assert ((ffesymbol_state (s) == FFESYMBOL_stateNONE)
	  || (ffesymbol_state (s) == FFESYMBOL_stateUNCERTAIN));

  na = sa = ffesymbol_attrs (s);

  assert (!(sa & ~(FFESYMBOL_attrsACTUALARG
		   | FFESYMBOL_attrsADJUSTABLE
		   | FFESYMBOL_attrsANYLEN
		   | FFESYMBOL_attrsARRAY
		   | FFESYMBOL_attrsDUMMY
		   | FFESYMBOL_attrsEXTERNAL
		   | FFESYMBOL_attrsSFARG
		   | FFESYMBOL_attrsTYPE)));

  kind = ffesymbol_kind (s);
  where = ffesymbol_where (s);

  /* Figure out what kind of object we've got based on previous declarations
     of or references to the object. */

  if (sa & FFESYMBOL_attrsEXTERNAL)
    {
      assert (!(sa & ~(FFESYMBOL_attrsACTUALARG
		       | FFESYMBOL_attrsDUMMY
		       | FFESYMBOL_attrsEXTERNAL
		       | FFESYMBOL_attrsTYPE)));

      if (sa & FFESYMBOL_attrsTYPE)
	where = FFEINFO_whereGLOBAL;
      else
	/* Not TYPE. */
	{
	  kind = FFEINFO_kindFUNCTION;

	  if (sa & FFESYMBOL_attrsDUMMY)
	    ;			/* Not TYPE. */
	  else if (sa & FFESYMBOL_attrsACTUALARG)
	    ;			/* Not DUMMY or TYPE. */
	  else			/* Not ACTUALARG, DUMMY, or TYPE. */
	    where = FFEINFO_whereGLOBAL;
	}
    }
  else if (sa & FFESYMBOL_attrsDUMMY)
    {
      assert (!(sa & FFESYMBOL_attrsEXTERNAL));	/* Handled above. */
      assert (!(sa & ~(FFESYMBOL_attrsDUMMY
		       | FFESYMBOL_attrsEXTERNAL
		       | FFESYMBOL_attrsTYPE)));

      kind = FFEINFO_kindFUNCTION;
      maybe_ambig = TRUE;	/* If basictypeCHARACTER, can't be sure; kind
				   could be ENTITY w/substring ref. */
    }
  else if (sa & FFESYMBOL_attrsARRAY)
    {
      assert (!(sa & ~(FFESYMBOL_attrsARRAY
		       | FFESYMBOL_attrsADJUSTABLE
		       | FFESYMBOL_attrsTYPE)));

      where = FFEINFO_whereLOCAL;
    }
  else if (sa & FFESYMBOL_attrsSFARG)
    {
      assert (!(sa & ~(FFESYMBOL_attrsSFARG
		       | FFESYMBOL_attrsTYPE)));

      where = FFEINFO_whereLOCAL;	/* Actually an error, but at least we
					   know it's a local var. */
    }
  else if (sa & FFESYMBOL_attrsTYPE)
    {
      assert (!(sa & (FFESYMBOL_attrsARRAY
		      | FFESYMBOL_attrsDUMMY
		      | FFESYMBOL_attrsEXTERNAL
		      | FFESYMBOL_attrsSFARG)));	/* Handled above. */
      assert (!(sa & ~(FFESYMBOL_attrsTYPE
		       | FFESYMBOL_attrsADJUSTABLE
		       | FFESYMBOL_attrsANYLEN
		       | FFESYMBOL_attrsARRAY
		       | FFESYMBOL_attrsDUMMY
		       | FFESYMBOL_attrsEXTERNAL
		       | FFESYMBOL_attrsSFARG)));

      if (ffeintrin_is_intrinsic (ffesymbol_text (s), t, FALSE,
				  &gen, &spec, &imp))
	{
	  if (!(sa & FFESYMBOL_attrsANYLEN)
	      && (ffeimplic_peek_symbol_type (s, NULL)
		  == FFEINFO_basictypeCHARACTER))
	    return s;		/* Haven't learned anything yet. */

	  ffesymbol_signal_change (s);	/* May need to back up to previous
					   version. */
	  ffesymbol_set_generic (s, gen);
	  ffesymbol_set_specific (s, spec);
	  ffesymbol_set_implementation (s, imp);
	  ffesymbol_set_info (s,
			      ffeinfo_new (ffesymbol_basictype (s),
					   ffesymbol_kindtype (s),
					   0,
					   FFEINFO_kindFUNCTION,
					   FFEINFO_whereINTRINSIC,
					   ffesymbol_size (s)));
	  ffesymbol_set_state (s, FFESYMBOL_stateUNDERSTOOD);
	  ffesymbol_resolve_intrin (s);
	  ffesymbol_reference (s, t, FALSE);
	  s = ffecom_sym_learned (s);
	  ffesymbol_signal_unreported (s);	/* For debugging purposes. */

	  return s;
	}
      if (sa & FFESYMBOL_attrsANYLEN)
	error = TRUE;		/* Error, since the only way we can,
				   given CHARACTER*(*) FOO, accept
				   FOO(...) is for FOO to be a dummy
				   arg or constant, but it can't
				   become either now. */
      else if (sa & FFESYMBOL_attrsADJUSTABLE)
	{
	  kind = FFEINFO_kindENTITY;
	  where = FFEINFO_whereLOCAL;
	}
      else
	{
	  kind = FFEINFO_kindFUNCTION;
	  where = FFEINFO_whereGLOBAL;
	  maybe_ambig = TRUE;	/* If basictypeCHARACTER, can't be sure;
				   could be ENTITY/LOCAL w/substring ref. */
	}
    }
  else if (sa == FFESYMBOL_attrsetNONE)
    {
      assert (ffesymbol_state (s) == FFESYMBOL_stateNONE);

      if (ffeintrin_is_intrinsic (ffesymbol_text (s), t, FALSE,
				  &gen, &spec, &imp))
	{
	  if (ffeimplic_peek_symbol_type (s, NULL)
	      == FFEINFO_basictypeCHARACTER)
	    return s;		/* Haven't learned anything yet. */

	  ffesymbol_signal_change (s);	/* May need to back up to previous
					   version. */
	  ffesymbol_set_generic (s, gen);
	  ffesymbol_set_specific (s, spec);
	  ffesymbol_set_implementation (s, imp);
	  ffesymbol_set_info (s,
			      ffeinfo_new (ffesymbol_basictype (s),
					   ffesymbol_kindtype (s),
					   0,
					   FFEINFO_kindFUNCTION,
					   FFEINFO_whereINTRINSIC,
					   ffesymbol_size (s)));
	  ffesymbol_set_state (s, FFESYMBOL_stateUNDERSTOOD);
	  ffesymbol_resolve_intrin (s);
	  s = ffecom_sym_learned (s);
	  ffesymbol_reference (s, t, FALSE);
	  ffesymbol_signal_unreported (s);	/* For debugging purposes. */
	  return s;
	}

      kind = FFEINFO_kindFUNCTION;
      where = FFEINFO_whereGLOBAL;
      maybe_ambig = TRUE;	/* If basictypeCHARACTER, can't be sure;
				   could be ENTITY/LOCAL w/substring ref. */
    }
  else
    error = TRUE;

  /* Now see what we've got for a new object: NONE means a new error cropped
     up; ANY means an old error to be ignored; otherwise, everything's ok,
     update the object (symbol) and continue on. */

  if (error)
    ffesymbol_error (s, t);
  else if (!(na & FFESYMBOL_attrsANY))
    {
      ffesymbol_signal_change (s);	/* May need to back up to previous
					   version. */
      if (!ffeimplic_establish_symbol (s))
	{
	  ffesymbol_error (s, t);
	  return s;
	}
      if (maybe_ambig
	  && (ffesymbol_basictype (s) == FFEINFO_basictypeCHARACTER))
	return s;		/* Still not sure, let caller deal with it
				   based on (...). */

      ffesymbol_set_info (s,
			  ffeinfo_new (ffesymbol_basictype (s),
				       ffesymbol_kindtype (s),
				       ffesymbol_rank (s),
				       kind,
				       where,
				       ffesymbol_size (s)));
      ffesymbol_set_state (s, FFESYMBOL_stateUNDERSTOOD);
      ffesymbol_resolve_intrin (s);
      s = ffecom_sym_learned (s);
      ffesymbol_reference (s, t, FALSE);
      ffesymbol_signal_unreported (s);	/* For debugging purposes. */
    }

  return s;
}

/* ffeexpr_token_arguments_ -- OPEN_PAREN [expr COMMA]...expr

   Return a pointer to this function to the lexer (ffelex), which will
   invoke it for the next token.

   Handle expression (which might be null) and COMMA or CLOSE_PAREN.  */

static ffelexHandler
ffeexpr_token_arguments_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  ffeexprExpr_ procedure;
  ffebld reduced;
  ffeinfo info;
  ffeexprContext ctx;
  bool check_intrin = FALSE;	/* Set TRUE if intrinsic is REAL(Z) or AIMAG(Z). */

  procedure = ffeexpr_stack_->exprstack;
  info = ffebld_info (procedure->u.operand);

  /* Is there an expression to add?  If the expression is nil,
     it might still be an argument.  It is if:

       -  The current token is comma, or

       -  The -fugly-comma flag was specified *and* the procedure
          being invoked is external.

     Otherwise, if neither of the above is the case, just
     ignore this (nil) expression.  */

  if ((expr != NULL)
      || (ffelex_token_type (t) == FFELEX_typeCOMMA)
      || (ffe_is_ugly_comma ()
	  && (ffeinfo_where (info) == FFEINFO_whereGLOBAL)))
    {
      /* This expression, even if nil, is apparently intended as an argument.  */

      /* Internal procedure (CONTAINS, or statement function)?  */

      if (ffeinfo_where (info) == FFEINFO_whereCONSTANT)
	{
	  if ((expr == NULL)
	      && ffebad_start (FFEBAD_NULL_ARGUMENT))
	    {
	      ffebad_here (0, ffelex_token_where_line (ffeexpr_stack_->tokens[0]),
			   ffelex_token_where_column (ffeexpr_stack_->tokens[0]));
	      ffebad_here (1, ffelex_token_where_line (t),
			   ffelex_token_where_column (t));
	      ffebad_finish ();
	    }

	  if (expr == NULL)
	    ;
	  else
	    {
	      if (ffeexpr_stack_->next_dummy == NULL)
		{			/* Report later which was the first extra argument. */
		  if (ffeexpr_stack_->tokens[1] == NULL)
		    {
		      ffeexpr_stack_->tokens[1] = ffelex_token_use (ft);
		      ffeexpr_stack_->num_args = 0;
		    }
		  ++ffeexpr_stack_->num_args;	/* Count # of extra arguments. */
		}
	      else
		{
		  if ((ffeinfo_rank (ffebld_info (expr)) != 0)
		      && ffebad_start (FFEBAD_ARRAY_AS_SFARG))
		    {
		      ffebad_here (0,
				   ffelex_token_where_line (ffeexpr_stack_->tokens[0]),
				   ffelex_token_where_column (ffeexpr_stack_->tokens[0]));
		      ffebad_here (1, ffelex_token_where_line (ft),
				   ffelex_token_where_column (ft));
		      ffebad_string (ffesymbol_text (ffesymbol_sfdummyparent
						     (ffebld_symter (ffebld_head
								     (ffeexpr_stack_->next_dummy)))));
		      ffebad_finish ();
		    }
		  else
		    {
		      expr = ffeexpr_convert_expr (expr, ft,
						   ffebld_head (ffeexpr_stack_->next_dummy),
						   ffeexpr_stack_->tokens[0],
						   FFEEXPR_contextLET);
		      ffebld_append_item (&ffeexpr_stack_->bottom, expr);
		    }
		  --ffeexpr_stack_->num_args;	/* Count down # of args. */
		  ffeexpr_stack_->next_dummy
		    = ffebld_trail (ffeexpr_stack_->next_dummy);
		}
	    }
	}
      else
	{
	  if ((expr == NULL)
	      && ffe_is_pedantic ()
	      && ffebad_start (FFEBAD_NULL_ARGUMENT_W))
	    {
	      ffebad_here (0, ffelex_token_where_line (ffeexpr_stack_->tokens[0]),
			   ffelex_token_where_column (ffeexpr_stack_->tokens[0]));
	      ffebad_here (1, ffelex_token_where_line (t),
			   ffelex_token_where_column (t));
	      ffebad_finish ();
	    }
	  ffebld_append_item (&ffeexpr_stack_->bottom, expr);
	}
    }

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      switch (ffeexpr_context_outer_ (ffeexpr_stack_))
	{
	case FFEEXPR_contextSFUNCDEF:
	case FFEEXPR_contextSFUNCDEFACTUALARGEXPR_:
	case FFEEXPR_contextSFUNCDEFINDEX_:
	case FFEEXPR_contextSFUNCDEFINDEXORACTUALARGEXPR_:
	  ctx = FFEEXPR_contextSFUNCDEFACTUALARG_;
	  break;

	case FFEEXPR_contextSFUNCDEFACTUALARG_:
	case FFEEXPR_contextSFUNCDEFINDEXORACTUALARG_:
	  assert ("bad context" == NULL);
	  ctx = FFEEXPR_context;
	  break;

	default:
	  ctx = FFEEXPR_contextACTUALARG_;
	  break;
	}
      return (ffelexHandler) ffeexpr_rhs (ffeexpr_stack_->pool, ctx,
					  ffeexpr_token_arguments_);

    default:
      break;
    }

  if ((ffeinfo_where (info) == FFEINFO_whereCONSTANT)
      && (ffeexpr_stack_->next_dummy != NULL))
    {				/* Too few arguments. */
      if (ffebad_start (FFEBAD_TOO_FEW_ARGUMENTS))
	{
	  char num[10];

	  sprintf (num, "%" ffebldListLength_f "u", ffeexpr_stack_->num_args);

	  ffebad_here (0, ffelex_token_where_line (t),
		       ffelex_token_where_column (t));
	  ffebad_here (1, ffelex_token_where_line (ffeexpr_stack_->tokens[0]),
		     ffelex_token_where_column (ffeexpr_stack_->tokens[0]));
	  ffebad_string (num);
	  ffebad_string (ffesymbol_text (ffesymbol_sfdummyparent (ffebld_symter
			      (ffebld_head (ffeexpr_stack_->next_dummy)))));
	  ffebad_finish ();
	}
      for (;
	   ffeexpr_stack_->next_dummy != NULL;
	   ffeexpr_stack_->next_dummy
	   = ffebld_trail (ffeexpr_stack_->next_dummy))
	{
	  expr = ffebld_new_conter (ffebld_constant_new_integerdefault_val (0));
	  ffebld_set_info (expr, ffeinfo_new_any ());
	  ffebld_append_item (&ffeexpr_stack_->bottom, expr);
	}
    }

  if ((ffeinfo_where (info) == FFEINFO_whereCONSTANT)
      && (ffeexpr_stack_->tokens[1] != NULL))
    {				/* Too many arguments to statement function. */
      if (ffebad_start (FFEBAD_TOO_MANY_ARGUMENTS))
	{
	  char num[10];

	  sprintf (num, "%" ffebldListLength_f "u", ffeexpr_stack_->num_args);

	  ffebad_here (0, ffelex_token_where_line (ffeexpr_stack_->tokens[1]),
		     ffelex_token_where_column (ffeexpr_stack_->tokens[1]));
	  ffebad_here (1, ffelex_token_where_line (ffeexpr_stack_->tokens[0]),
		     ffelex_token_where_column (ffeexpr_stack_->tokens[0]));
	  ffebad_string (num);
	  ffebad_finish ();
	}
      ffelex_token_kill (ffeexpr_stack_->tokens[1]);
    }
  ffebld_end_list (&ffeexpr_stack_->bottom);

  if (ffebld_op (procedure->u.operand) == FFEBLD_opANY)
    {
      reduced = ffebld_new_any ();
      ffebld_set_info (reduced, ffeinfo_new_any ());
    }
  else
    {
      if (ffeexpr_stack_->context != FFEEXPR_contextSUBROUTINEREF)
	reduced = ffebld_new_funcref (procedure->u.operand,
				      ffeexpr_stack_->expr);
      else
	reduced = ffebld_new_subrref (procedure->u.operand,
				      ffeexpr_stack_->expr);
      if (ffebld_symter_generic (procedure->u.operand) != FFEINTRIN_genNONE)
	ffeintrin_fulfill_generic (&reduced, &info, ffeexpr_stack_->tokens[0]);
      else if (ffebld_symter_specific (procedure->u.operand)
	       != FFEINTRIN_specNONE)
	ffeintrin_fulfill_specific (&reduced, &info, &check_intrin,
				    ffeexpr_stack_->tokens[0]);
      else
	ffeexpr_fulfill_call_ (&reduced, ffeexpr_stack_->tokens[0]);

      if (ffebld_op (reduced) != FFEBLD_opANY)
	ffebld_set_info (reduced,
			 ffeinfo_new (ffeinfo_basictype (info),
				      ffeinfo_kindtype (info),
				      0,
				      FFEINFO_kindENTITY,
				      FFEINFO_whereFLEETING,
				      ffeinfo_size (info)));
      else
	ffebld_set_info (reduced, ffeinfo_new_any ());
    }
  if (ffebld_op (reduced) == FFEBLD_opFUNCREF)
    reduced = ffeexpr_collapse_funcref (reduced, ffeexpr_stack_->tokens[0]);
  ffeexpr_stack_->exprstack = procedure->previous;	/* Pops
							   not-quite-operand off
							   stack. */
  procedure->u.operand = reduced;	/* Save the line/column ffewhere
					   info. */
  ffeexpr_exprstack_push_operand_ (procedure);	/* Push it back on stack. */
  if (ffelex_token_type (t) == FFELEX_typeCLOSE_PAREN)
    {
      ffelex_token_kill (ffeexpr_stack_->tokens[0]);
      ffeexpr_is_substr_ok_ = FALSE;	/* Nobody likes "FUNC(3)(1:1)".... */

      /* If the intrinsic needs checking (is REAL(Z) or AIMAG(Z), where
	 Z is DOUBLE COMPLEX), and a command-line option doesn't already
	 establish interpretation, probably complain.  */

      if (check_intrin
	  && !ffe_is_90 ()
	  && !ffe_is_ugly_complex ())
	{
	  /* If the outer expression is REAL(me...), issue diagnostic
	     only if next token isn't the close-paren for REAL(me).  */

	  if ((ffeexpr_stack_->previous != NULL)
	      && (ffeexpr_stack_->previous->exprstack != NULL)
	      && (ffeexpr_stack_->previous->exprstack->type == FFEEXPR_exprtypeOPERAND_)
	      && ((reduced = ffeexpr_stack_->previous->exprstack->u.operand) != NULL)
	      && (ffebld_op (reduced) == FFEBLD_opSYMTER)
	      && (ffebld_symter_implementation (reduced) == FFEINTRIN_impREAL))
	    return (ffelexHandler) ffeexpr_token_intrincheck_;

	  /* Diagnose the ambiguity now.  */

	  if (ffebad_start (FFEBAD_INTRINSIC_CMPAMBIG))
	    {
	      ffebad_string (ffeintrin_name_implementation
			     (ffebld_symter_implementation
			      (ffebld_left
			       (ffeexpr_stack_->exprstack->u.operand))));
	      ffebad_here (0, ffelex_token_where_line (ffeexpr_stack_->exprstack->token),
			   ffelex_token_where_column (ffeexpr_stack_->exprstack->token));
	      ffebad_finish ();
	    }
	}
      return (ffelexHandler) ffeexpr_token_substrp_;
    }

  if (ffest_ffebad_start (FFEBAD_INVALID_TOKEN_IN_EXPRESSION))
    {
      ffebad_here (0, ffelex_token_where_line (t),
		   ffelex_token_where_column (t));
      ffebad_here (1, ffelex_token_where_line (ffeexpr_stack_->tokens[0]),
		   ffelex_token_where_column (ffeexpr_stack_->tokens[0]));
      ffebad_finish ();
    }
  ffelex_token_kill (ffeexpr_stack_->tokens[0]);
  ffeexpr_is_substr_ok_ = FALSE;/* Nobody likes "FUNC(3)(1:1)".... */
  return
    (ffelexHandler) ffeexpr_find_close_paren_ (t,
					       (ffelexHandler)
					       ffeexpr_token_substrp_);
}

/* ffeexpr_token_elements_ -- OPEN_PAREN [expr COMMA]...expr

   Return a pointer to this array to the lexer (ffelex), which will
   invoke it for the next token.

   Handle expression and COMMA or CLOSE_PAREN.	*/

static ffelexHandler
ffeexpr_token_elements_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  ffeexprExpr_ array;
  ffebld reduced;
  ffeinfo info;
  ffeinfoWhere where;
  ffetargetIntegerDefault val;
  ffetargetIntegerDefault lval = 0;
  ffetargetIntegerDefault uval = 0;
  ffebld lbound;
  ffebld ubound;
  bool lcheck;
  bool ucheck;

  array = ffeexpr_stack_->exprstack;
  info = ffebld_info (array->u.operand);

  if ((expr == NULL)		/* && ((ffeexpr_stack_->rank != 0) ||
				   (ffelex_token_type(t) ==
	 FFELEX_typeCOMMA)) */ )
    {
      if (ffebad_start (FFEBAD_NULL_ELEMENT))
	{
	  ffebad_here (0, ffelex_token_where_line (ffeexpr_stack_->tokens[0]),
		     ffelex_token_where_column (ffeexpr_stack_->tokens[0]));
	  ffebad_here (1, ffelex_token_where_line (t),
		       ffelex_token_where_column (t));
	  ffebad_finish ();
	}
      if (ffeexpr_stack_->rank < ffeinfo_rank (info))
	{			/* Don't bother if we're going to complain
				   later! */
	  expr = ffebld_new_conter (ffebld_constant_new_integerdefault_val (1));
	  ffebld_set_info (expr, ffeinfo_new_any ());
	}
    }

  if (expr == NULL)
    ;
  else if (ffeinfo_rank (info) == 0)
    {				/* In EQUIVALENCE context, ffeinfo_rank(info)
				   may == 0. */
      ++ffeexpr_stack_->rank;	/* Track anyway, may need for new VXT
				   feature. */
      ffebld_append_item (&ffeexpr_stack_->bottom, expr);
    }
  else
    {
      ++ffeexpr_stack_->rank;
      if (ffeexpr_stack_->rank > ffeinfo_rank (info))
	{			/* Report later which was the first extra
				   element. */
	  if (ffeexpr_stack_->rank == ffeinfo_rank (info) + 1)
	    ffeexpr_stack_->tokens[1] = ffelex_token_use (ft);
	}
      else
	{
	  switch (ffeinfo_where (ffebld_info (expr)))
	    {
	    case FFEINFO_whereCONSTANT:
	      break;

	    case FFEINFO_whereIMMEDIATE:
	      ffeexpr_stack_->constant = FALSE;
	      break;

	    default:
	      ffeexpr_stack_->constant = FALSE;
	      ffeexpr_stack_->immediate = FALSE;
	      break;
	    }
	  if (ffebld_op (expr) == FFEBLD_opCONTER
	      && ffebld_kindtype (expr) == FFEINFO_kindtypeINTEGERDEFAULT)
	    {
	      val = ffebld_constant_integerdefault (ffebld_conter (expr));

	      lbound = ffebld_left (ffebld_head (ffeexpr_stack_->bound_list));
	      if (lbound == NULL)
		{
		  lcheck = TRUE;
		  lval = 1;
		}
	      else if (ffebld_op (lbound) == FFEBLD_opCONTER)
		{
		  lcheck = TRUE;
		  lval = ffebld_constant_integerdefault (ffebld_conter (lbound));
		}
	      else
		lcheck = FALSE;

	      ubound = ffebld_right (ffebld_head (ffeexpr_stack_->bound_list));
	      assert (ubound != NULL);
	      if (ffebld_op (ubound) == FFEBLD_opCONTER)
		{
		  ucheck = TRUE;
		  uval = ffebld_constant_integerdefault (ffebld_conter (ubound));
		}
	      else
		ucheck = FALSE;

	      if ((lcheck && (val < lval)) || (ucheck && (val > uval)))
		{
		  ffebad_start (FFEBAD_RANGE_ARRAY);
		  ffebad_here (0, ffelex_token_where_line (ft),
			       ffelex_token_where_column (ft));
		  ffebad_finish ();
		}
	    }
	  ffebld_append_item (&ffeexpr_stack_->bottom, expr);
	  ffeexpr_stack_->bound_list = ffebld_trail (ffeexpr_stack_->bound_list);
	}
    }

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
      switch (ffeexpr_context_outer_ (ffeexpr_stack_))
	{
	case FFEEXPR_contextDATAIMPDOITEM_:
	  return (ffelexHandler) ffeexpr_rhs (ffeexpr_stack_->pool,
					      FFEEXPR_contextDATAIMPDOINDEX_,
					      ffeexpr_token_elements_);

	case FFEEXPR_contextEQUIVALENCE:
	  return (ffelexHandler) ffeexpr_rhs (ffeexpr_stack_->pool,
					      FFEEXPR_contextEQVINDEX_,
					      ffeexpr_token_elements_);

	case FFEEXPR_contextSFUNCDEF:
	case FFEEXPR_contextSFUNCDEFINDEX_:
	  return (ffelexHandler) ffeexpr_rhs (ffeexpr_stack_->pool,
					      FFEEXPR_contextSFUNCDEFINDEX_,
					      ffeexpr_token_elements_);

	case FFEEXPR_contextSFUNCDEFACTUALARG_:
	case FFEEXPR_contextSFUNCDEFINDEXORACTUALARG_:
	  assert ("bad context" == NULL);
	  break;

	default:
	  return (ffelexHandler) ffeexpr_rhs (ffeexpr_stack_->pool,
					      FFEEXPR_contextINDEX_,
					      ffeexpr_token_elements_);
	}

    default:
      break;
    }

  if ((ffeexpr_stack_->rank != ffeinfo_rank (info))
      && (ffeinfo_rank (info) != 0))
    {
      char num[10];

      if (ffeexpr_stack_->rank < ffeinfo_rank (info))
	{
	  if (ffebad_start (FFEBAD_TOO_FEW_ELEMENTS))
	    {
	      sprintf (num, "%d",
		       (int) (ffeinfo_rank (info) - ffeexpr_stack_->rank));

	      ffebad_here (0, ffelex_token_where_line (t),
			   ffelex_token_where_column (t));
	      ffebad_here (1,
			ffelex_token_where_line (ffeexpr_stack_->tokens[0]),
		     ffelex_token_where_column (ffeexpr_stack_->tokens[0]));
	      ffebad_string (num);
	      ffebad_finish ();
	    }
	}
      else
	{
	  if (ffebad_start (FFEBAD_TOO_MANY_ELEMENTS))
	    {
	      sprintf (num, "%d",
		       (int) (ffeexpr_stack_->rank - ffeinfo_rank (info)));

	      ffebad_here (0,
			ffelex_token_where_line (ffeexpr_stack_->tokens[1]),
		     ffelex_token_where_column (ffeexpr_stack_->tokens[1]));
	      ffebad_here (1,
			ffelex_token_where_line (ffeexpr_stack_->tokens[0]),
		     ffelex_token_where_column (ffeexpr_stack_->tokens[0]));
	      ffebad_string (num);
	      ffebad_finish ();
	    }
	  ffelex_token_kill (ffeexpr_stack_->tokens[1]);
	}
      while (ffeexpr_stack_->rank++ < ffeinfo_rank (info))
	{
	  expr = ffebld_new_conter (ffebld_constant_new_integerdefault_val (1));
	  ffebld_set_info (expr, ffeinfo_new (FFEINFO_basictypeINTEGER,
					      FFEINFO_kindtypeINTEGERDEFAULT,
					      0, FFEINFO_kindENTITY,
					      FFEINFO_whereCONSTANT,
					      FFETARGET_charactersizeNONE));
	  ffebld_append_item (&ffeexpr_stack_->bottom, expr);
	}
    }
  ffebld_end_list (&ffeexpr_stack_->bottom);

  if (ffebld_op (array->u.operand) == FFEBLD_opANY)
    {
      reduced = ffebld_new_any ();
      ffebld_set_info (reduced, ffeinfo_new_any ());
    }
  else
    {
      reduced = ffebld_new_arrayref (array->u.operand, ffeexpr_stack_->expr);
      if (ffeexpr_stack_->constant)
	where = FFEINFO_whereFLEETING_CADDR;
      else if (ffeexpr_stack_->immediate)
	where = FFEINFO_whereFLEETING_IADDR;
      else
	where = FFEINFO_whereFLEETING;
      ffebld_set_info (reduced,
		       ffeinfo_new (ffeinfo_basictype (info),
				    ffeinfo_kindtype (info),
				    0,
				    FFEINFO_kindENTITY,
				    where,
				    ffeinfo_size (info)));
      reduced = ffeexpr_collapse_arrayref (reduced, ffeexpr_stack_->tokens[0]);
    }

  ffeexpr_stack_->exprstack = array->previous;	/* Pops not-quite-operand off
						   stack. */
  array->u.operand = reduced;	/* Save the line/column ffewhere info. */
  ffeexpr_exprstack_push_operand_ (array);	/* Push it back on stack. */

  switch (ffeinfo_basictype (info))
    {
    case FFEINFO_basictypeCHARACTER:
      ffeexpr_is_substr_ok_ = TRUE;	/* Everyone likes "FOO(3)(1:1)".... */
      break;

    case FFEINFO_basictypeNONE:
      ffeexpr_is_substr_ok_ = TRUE;
      assert (ffeexpr_stack_->context == FFEEXPR_contextEQUIVALENCE);
      break;

    default:
      ffeexpr_is_substr_ok_ = FALSE;
      break;
    }

  if (ffelex_token_type (t) == FFELEX_typeCLOSE_PAREN)
    {
      ffelex_token_kill (ffeexpr_stack_->tokens[0]);
      return (ffelexHandler) ffeexpr_token_substrp_;
    }

  if (ffest_ffebad_start (FFEBAD_INVALID_TOKEN_IN_EXPRESSION))
    {
      ffebad_here (0, ffelex_token_where_line (t),
		   ffelex_token_where_column (t));
      ffebad_here (1, ffelex_token_where_line (ffeexpr_stack_->tokens[0]),
		   ffelex_token_where_column (ffeexpr_stack_->tokens[0]));
      ffebad_finish ();
    }
  ffelex_token_kill (ffeexpr_stack_->tokens[0]);
  return
    (ffelexHandler) ffeexpr_find_close_paren_ (t,
					       (ffelexHandler)
					       ffeexpr_token_substrp_);
}

/* ffeexpr_token_equivalence_ -- OPEN_PAREN expr

   Return a pointer to this array to the lexer (ffelex), which will
   invoke it for the next token.

   If token is COLON, pass off to _substr_, else init list and pass off
   to _elements_.  This handles the case "EQUIVALENCE (FOO(expr?", where
   ? marks the token, and where FOO's rank/type has not yet been established,
   meaning we could be in a list of indices or in a substring
   specification.  */

static ffelexHandler
ffeexpr_token_equivalence_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  if (ffelex_token_type (t) == FFELEX_typeCOLON)
    return ffeexpr_token_substring_ (ft, expr, t);

  ffebld_init_list (&ffeexpr_stack_->expr, &ffeexpr_stack_->bottom);
  return ffeexpr_token_elements_ (ft, expr, t);
}

/* ffeexpr_token_substring_ -- NAME(of kindENTITY) OPEN_PAREN expr

   Return a pointer to this function to the lexer (ffelex), which will
   invoke it for the next token.

   Handle expression (which may be null) and COLON.  */

static ffelexHandler
ffeexpr_token_substring_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  ffeexprExpr_ string;
  ffeinfo info;
  ffetargetIntegerDefault i;
  ffeexprContext ctx;
  ffetargetCharacterSize size;

  string = ffeexpr_stack_->exprstack;
  info = ffebld_info (string->u.operand);
  size = ffebld_size_max (string->u.operand);

  if (ffelex_token_type (t) == FFELEX_typeCOLON)
    {
      if ((expr != NULL)
	  && (ffebld_op (expr) == FFEBLD_opCONTER)
	  && (((i = ffebld_constant_integerdefault (ffebld_conter (expr)))
	       < 1)
	      || ((size != FFETARGET_charactersizeNONE) && (i > size))))
	{
	  ffebad_start (FFEBAD_RANGE_SUBSTR);
	  ffebad_here (0, ffelex_token_where_line (ft),
		       ffelex_token_where_column (ft));
	  ffebad_finish ();
	}
      ffeexpr_stack_->expr = expr;

      switch (ffeexpr_stack_->context)
	{
	case FFEEXPR_contextSFUNCDEF:
	case FFEEXPR_contextSFUNCDEFINDEX_:
	  ctx = FFEEXPR_contextSFUNCDEFINDEX_;
	  break;

	case FFEEXPR_contextSFUNCDEFACTUALARG_:
	case FFEEXPR_contextSFUNCDEFINDEXORACTUALARG_:
	  assert ("bad context" == NULL);
	  ctx = FFEEXPR_context;
	  break;

	default:
	  ctx = FFEEXPR_contextINDEX_;
	  break;
	}

      return (ffelexHandler) ffeexpr_rhs (ffeexpr_stack_->pool, ctx,
					  ffeexpr_token_substring_1_);
    }

  if (ffest_ffebad_start (FFEBAD_MISSING_COLON_IN_SUBSTR))
    {
      ffebad_here (0, ffelex_token_where_line (t),
		   ffelex_token_where_column (t));
      ffebad_here (1, ffelex_token_where_line (ffeexpr_stack_->tokens[0]),
		   ffelex_token_where_column (ffeexpr_stack_->tokens[0]));
      ffebad_finish ();
    }

  ffeexpr_stack_->expr = NULL;
  return (ffelexHandler) ffeexpr_token_substring_1_ (ft, expr, t);
}

/* ffeexpr_token_substring_1_ -- NAME OPEN_PAREN [expr COMMA]...expr

   Return a pointer to this function to the lexer (ffelex), which will
   invoke it for the next token.

   Handle expression (which might be null) and CLOSE_PAREN.  */

static ffelexHandler
ffeexpr_token_substring_1_ (ffelexToken ft, ffebld last, ffelexToken t)
{
  ffeexprExpr_ string;
  ffebld reduced;
  ffebld substrlist;
  ffebld first = ffeexpr_stack_->expr;
  ffebld strop;
  ffeinfo info;
  ffeinfoWhere lwh;
  ffeinfoWhere rwh;
  ffeinfoWhere where;
  ffeinfoKindtype first_kt;
  ffeinfoKindtype last_kt;
  ffetargetIntegerDefault first_val;
  ffetargetIntegerDefault last_val;
  ffetargetCharacterSize size;
  ffetargetCharacterSize strop_size_max;
  bool first_known;

  string = ffeexpr_stack_->exprstack;
  strop = string->u.operand;
  info = ffebld_info (strop);

  if (first == NULL
      || (ffebld_op (first) == FFEBLD_opCONTER
	  && ffebld_kindtype (first) == FFEINFO_kindtypeINTEGERDEFAULT))
    {				/* The starting point is known. */
      first_val = (first == NULL) ? 1
	: ffebld_constant_integerdefault (ffebld_conter (first));
      first_known = TRUE;
    }
  else
    {				/* Assume start of the entity. */
      first_val = 1;
      first_known = FALSE;
    }

  if (last != NULL
      && (ffebld_op (last) == FFEBLD_opCONTER
	  && ffebld_kindtype (last) == FFEINFO_kindtypeINTEGERDEFAULT))
    {				/* The ending point is known. */
      last_val = ffebld_constant_integerdefault (ffebld_conter (last));

      if (first_known)
	{			/* The beginning point is a constant. */
	  if (first_val <= last_val)
	    size = last_val - first_val + 1;
	  else
	    {
	      if (0 && ffe_is_90 ())
		size = 0;
	      else
		{
		  size = 1;
		  ffebad_start (FFEBAD_ZERO_SIZE);
		  ffebad_here (0, ffelex_token_where_line (ft),
			       ffelex_token_where_column (ft));
		  ffebad_finish ();
		}
	    }
	}
      else
	size = FFETARGET_charactersizeNONE;

      strop_size_max = ffebld_size_max (strop);

      if ((strop_size_max != FFETARGET_charactersizeNONE)
	  && (last_val > strop_size_max))
	{			/* Beyond maximum possible end of string. */
	  ffebad_start (FFEBAD_RANGE_SUBSTR);
	  ffebad_here (0, ffelex_token_where_line (ft),
		       ffelex_token_where_column (ft));
	  ffebad_finish ();
	}
    }
  else
    size = FFETARGET_charactersizeNONE;	/* The size is not known. */

#if 0				/* Don't do this, or "is size of target
				   known?" would no longer be easily
				   answerable.	To see if there is a max
				   size, use ffebld_size_max; to get only the
				   known size, else NONE, use
				   ffebld_size_known; use ffebld_size if
				   values are sure to be the same (not
				   opSUBSTR or opCONCATENATE or known to have
				   known length). By getting rid of this
				   "useful info" stuff, we don't end up
				   blank-padding the constant in the
				   assignment "A(I:J)='XYZ'" to the known
				   length of A. */
  if (size == FFETARGET_charactersizeNONE)
    size = strop_size_max;	/* Assume we use the entire string. */
#endif

  substrlist
    = ffebld_new_item
    (first,
     ffebld_new_item
     (last,
      NULL
     )
    )
    ;

  if (first == NULL)
    lwh = FFEINFO_whereCONSTANT;
  else
    lwh = ffeinfo_where (ffebld_info (first));
  if (last == NULL)
    rwh = FFEINFO_whereCONSTANT;
  else
    rwh = ffeinfo_where (ffebld_info (last));

  switch (lwh)
    {
    case FFEINFO_whereCONSTANT:
      switch (rwh)
	{
	case FFEINFO_whereCONSTANT:
	  where = FFEINFO_whereCONSTANT;
	  break;

	case FFEINFO_whereIMMEDIATE:
	  where = FFEINFO_whereIMMEDIATE;
	  break;

	default:
	  where = FFEINFO_whereFLEETING;
	  break;
	}
      break;

    case FFEINFO_whereIMMEDIATE:
      switch (rwh)
	{
	case FFEINFO_whereCONSTANT:
	case FFEINFO_whereIMMEDIATE:
	  where = FFEINFO_whereIMMEDIATE;
	  break;

	default:
	  where = FFEINFO_whereFLEETING;
	  break;
	}
      break;

    default:
      where = FFEINFO_whereFLEETING;
      break;
    }

  if (first == NULL)
    first_kt = FFEINFO_kindtypeINTEGERDEFAULT;
  else
    first_kt = ffeinfo_kindtype (ffebld_info (first));
  if (last == NULL)
    last_kt = FFEINFO_kindtypeINTEGERDEFAULT;
  else
    last_kt = ffeinfo_kindtype (ffebld_info (last));

  switch (where)
    {
    case FFEINFO_whereCONSTANT:
      switch (ffeinfo_where (info))
	{
	case FFEINFO_whereCONSTANT:
	  break;

	case FFEINFO_whereIMMEDIATE:	/* Not possible, actually. */
	  where = FFEINFO_whereIMMEDIATE;
	  break;

	default:
	  where = FFEINFO_whereFLEETING_CADDR;
	  break;
	}
      break;

    case FFEINFO_whereIMMEDIATE:
      switch (ffeinfo_where (info))
	{
	case FFEINFO_whereCONSTANT:
	case FFEINFO_whereIMMEDIATE:	/* Not possible, actually. */
	  break;

	default:
	  where = FFEINFO_whereFLEETING_IADDR;
	  break;
	}
      break;

    default:
      switch (ffeinfo_where (info))
	{
	case FFEINFO_whereCONSTANT:
	  where = FFEINFO_whereCONSTANT_SUBOBJECT;	/* An F90 concept. */
	  break;

	case FFEINFO_whereIMMEDIATE:	/* Not possible, actually. */
	default:
	  where = FFEINFO_whereFLEETING;
	  break;
	}
      break;
    }

  if (ffebld_op (strop) == FFEBLD_opANY)
    {
      reduced = ffebld_new_any ();
      ffebld_set_info (reduced, ffeinfo_new_any ());
    }
  else
    {
      reduced = ffebld_new_substr (strop, substrlist);
      ffebld_set_info (reduced, ffeinfo_new
		       (FFEINFO_basictypeCHARACTER,
			ffeinfo_kindtype (info),
			0,
			FFEINFO_kindENTITY,
			where,
			size));
      reduced = ffeexpr_collapse_substr (reduced, ffeexpr_stack_->tokens[0]);
    }

  ffeexpr_stack_->exprstack = string->previous;	/* Pops not-quite-operand off
						   stack. */
  string->u.operand = reduced;	/* Save the line/column ffewhere info. */
  ffeexpr_exprstack_push_operand_ (string);	/* Push it back on stack. */

  if (ffelex_token_type (t) == FFELEX_typeCLOSE_PAREN)
    {
      ffelex_token_kill (ffeexpr_stack_->tokens[0]);
      ffeexpr_is_substr_ok_ = FALSE;	/* Nobody likes "FOO(3:5)(1:1)".... */
      return (ffelexHandler) ffeexpr_token_substrp_;
    }

  if (ffest_ffebad_start (FFEBAD_INVALID_TOKEN_IN_EXPRESSION))
    {
      ffebad_here (0, ffelex_token_where_line (t),
		   ffelex_token_where_column (t));
      ffebad_here (1, ffelex_token_where_line (ffeexpr_stack_->tokens[0]),
		   ffelex_token_where_column (ffeexpr_stack_->tokens[0]));
      ffebad_finish ();
    }

  ffelex_token_kill (ffeexpr_stack_->tokens[0]);
  ffeexpr_is_substr_ok_ = FALSE;/* Nobody likes "FOO(3:5)(1:1)".... */
  return
    (ffelexHandler) ffeexpr_find_close_paren_ (t,
					       (ffelexHandler)
					       ffeexpr_token_substrp_);
}

/* ffeexpr_token_substrp_ -- Rhs <character entity>

   Return a pointer to this function to the lexer (ffelex), which will
   invoke it for the next token.

   If OPEN_PAREN, treat as start of a substring ("(3:4)") construct, and
   issue error message if flag (serves as argument) is set.  Else, just
   forward token to binary_.  */

static ffelexHandler
ffeexpr_token_substrp_ (ffelexToken t)
{
  ffeexprContext ctx;

  if (ffelex_token_type (t) != FFELEX_typeOPEN_PAREN)
    return (ffelexHandler) ffeexpr_token_binary_ (t);

  ffeexpr_stack_->tokens[0] = ffelex_token_use (t);

  switch (ffeexpr_stack_->context)
    {
    case FFEEXPR_contextSFUNCDEF:
    case FFEEXPR_contextSFUNCDEFINDEX_:
      ctx = FFEEXPR_contextSFUNCDEFINDEX_;
      break;

    case FFEEXPR_contextSFUNCDEFACTUALARG_:
    case FFEEXPR_contextSFUNCDEFINDEXORACTUALARG_:
      assert ("bad context" == NULL);
      ctx = FFEEXPR_context;
      break;

    default:
      ctx = FFEEXPR_contextINDEX_;
      break;
    }

  if (!ffeexpr_is_substr_ok_)
    {
      if (ffebad_start (FFEBAD_BAD_SUBSTR))
	{
	  ffebad_here (0, ffelex_token_where_line (t),
		       ffelex_token_where_column (t));
	  ffebad_here (1, ffelex_token_where_line (ffeexpr_stack_->exprstack->token),
		       ffelex_token_where_column (ffeexpr_stack_->exprstack->token));
	  ffebad_finish ();
	}

      return (ffelexHandler) ffeexpr_rhs (ffeexpr_stack_->pool, ctx,
					  ffeexpr_token_anything_);
    }

  return (ffelexHandler) ffeexpr_rhs (ffeexpr_stack_->pool, ctx,
				      ffeexpr_token_substring_);
}

static ffelexHandler
ffeexpr_token_intrincheck_ (ffelexToken t)
{
  if ((ffelex_token_type (t) != FFELEX_typeCLOSE_PAREN)
      && ffebad_start (FFEBAD_INTRINSIC_CMPAMBIG))
    {
      ffebad_string (ffeintrin_name_implementation
		     (ffebld_symter_implementation
		      (ffebld_left
		       (ffeexpr_stack_->exprstack->u.operand))));
      ffebad_here (0, ffelex_token_where_line (ffeexpr_stack_->exprstack->token),
		   ffelex_token_where_column (ffeexpr_stack_->exprstack->token));
      ffebad_finish ();
    }

  return (ffelexHandler) ffeexpr_token_substrp_ (t);
}

/* ffeexpr_token_funsubstr_ -- NAME OPEN_PAREN expr

   Return a pointer to this function to the lexer (ffelex), which will
   invoke it for the next token.

   If COLON, do everything we would have done since _parenthesized_ if
   we had known NAME represented a kindENTITY instead of a kindFUNCTION.
   If not COLON, do likewise for kindFUNCTION instead.	*/

static ffelexHandler
ffeexpr_token_funsubstr_ (ffelexToken ft, ffebld expr, ffelexToken t)
{
  ffeinfoWhere where;
  ffesymbol s;
  ffesymbolAttrs sa;
  ffebld symter = ffeexpr_stack_->exprstack->u.operand;
  bool needs_type;
  ffeintrinGen gen;
  ffeintrinSpec spec;
  ffeintrinImp imp;

  s = ffebld_symter (symter);
  sa = ffesymbol_attrs (s);
  where = ffesymbol_where (s);

  /* We get here only if we don't already know enough about FOO when seeing a
     FOO(stuff) reference, and FOO might turn out to be a CHARACTER type.  If
     "stuff" is a substring reference, then FOO is a CHARACTER scalar type.
     Else FOO is a function, either intrinsic or external.  If intrinsic, it
     wouldn't necessarily be CHARACTER type, so unless it has already been
     declared DUMMY, it hasn't had its type established yet.  It can't be
     CHAR*(*) in any case, though it can have an explicit CHAR*n type.  */

  assert (!(sa & ~(FFESYMBOL_attrsDUMMY
		   | FFESYMBOL_attrsTYPE)));

  needs_type = !(ffesymbol_attrs (s) & FFESYMBOL_attrsDUMMY);

  ffesymbol_signal_change (s);	/* Probably already done, but in case.... */

  if (ffelex_token_type (t) == FFELEX_typeCOLON)
    {				/* Definitely an ENTITY (char substring). */
      if (needs_type && !ffeimplic_establish_symbol (s))
	{
	  ffesymbol_error (s, ffeexpr_stack_->tokens[0]);
	  return (ffelexHandler) ffeexpr_token_arguments_ (ft, expr, t);
	}

      ffesymbol_set_info (s,
			  ffeinfo_new (ffesymbol_basictype (s),
				       ffesymbol_kindtype (s),
				       ffesymbol_rank (s),
				       FFEINFO_kindENTITY,
				       (where == FFEINFO_whereNONE)
				       ? FFEINFO_whereLOCAL
				       : where,
				       ffesymbol_size (s)));
      ffebld_set_info (symter, ffeinfo_use (ffesymbol_info (s)));

      ffesymbol_set_state (s, FFESYMBOL_stateUNDERSTOOD);
      ffesymbol_resolve_intrin (s);
      s = ffecom_sym_learned (s);
      ffesymbol_signal_unreported (s);	/* For debugging purposes. */

      ffeexpr_stack_->exprstack->u.operand
	= ffeexpr_collapse_symter (symter, ffeexpr_tokens_[0]);

      return (ffelexHandler) ffeexpr_token_substring_ (ft, expr, t);
    }

  /* The "stuff" isn't a substring notation, so we now know the overall
     reference is to a function.  */

  if (ffeintrin_is_intrinsic (ffesymbol_text (s), ffeexpr_stack_->tokens[0],
			      FALSE, &gen, &spec, &imp))
    {
      ffebld_symter_set_generic (symter, gen);
      ffebld_symter_set_specific (symter, spec);
      ffebld_symter_set_implementation (symter, imp);
      ffesymbol_set_generic (s, gen);
      ffesymbol_set_specific (s, spec);
      ffesymbol_set_implementation (s, imp);
      ffesymbol_set_info (s,
			  ffeinfo_new (ffesymbol_basictype (s),
				       ffesymbol_kindtype (s),
				       0,
				       FFEINFO_kindFUNCTION,
				       FFEINFO_whereINTRINSIC,
				       ffesymbol_size (s)));
    }
  else
    {				/* Not intrinsic, now needs CHAR type. */
      if (!ffeimplic_establish_symbol (s))
	{
	  ffesymbol_error (s, ffeexpr_stack_->tokens[0]);
	  return (ffelexHandler) ffeexpr_token_arguments_ (ft, expr, t);
	}

      ffesymbol_set_info (s,
			  ffeinfo_new (ffesymbol_basictype (s),
				       ffesymbol_kindtype (s),
				       ffesymbol_rank (s),
				       FFEINFO_kindFUNCTION,
				       (where == FFEINFO_whereNONE)
				       ? FFEINFO_whereGLOBAL
				       : where,
				       ffesymbol_size (s)));
    }

  ffebld_set_info (symter, ffeinfo_use (ffesymbol_info (s)));

  ffesymbol_set_state (s, FFESYMBOL_stateUNDERSTOOD);
  ffesymbol_resolve_intrin (s);
  s = ffecom_sym_learned (s);
  ffesymbol_reference (s, ffeexpr_stack_->tokens[0], FALSE);
  ffesymbol_signal_unreported (s);	/* For debugging purposes. */
  ffebld_init_list (&ffeexpr_stack_->expr, &ffeexpr_stack_->bottom);
  return (ffelexHandler) ffeexpr_token_arguments_ (ft, expr, t);
}

/* ffeexpr_token_anything_ -- NAME OPEN_PAREN any-expr

   Handle basically any expression, looking for CLOSE_PAREN.  */

static ffelexHandler
ffeexpr_token_anything_ (ffelexToken ft UNUSED, ffebld expr UNUSED,
			 ffelexToken t)
{
  ffeexprExpr_ e = ffeexpr_stack_->exprstack;

  switch (ffelex_token_type (t))
    {
    case FFELEX_typeCOMMA:
    case FFELEX_typeCOLON:
      return (ffelexHandler) ffeexpr_rhs (ffeexpr_stack_->pool,
					  FFEEXPR_contextACTUALARG_,
					  ffeexpr_token_anything_);

    default:
      e->u.operand = ffebld_new_any ();
      ffebld_set_info (e->u.operand, ffeinfo_new_any ());
      ffelex_token_kill (ffeexpr_stack_->tokens[0]);
      ffeexpr_is_substr_ok_ = FALSE;
      if (ffelex_token_type (t) == FFELEX_typeCLOSE_PAREN)
	return (ffelexHandler) ffeexpr_token_substrp_;
      return (ffelexHandler) ffeexpr_token_substrp_ (t);
    }
}

/* Terminate module.  */

void
ffeexpr_terminate_2 ()
{
  assert (ffeexpr_stack_ == NULL);
  assert (ffeexpr_level_ == 0);
}
