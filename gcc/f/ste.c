/* ste.c -- Implementation File (module.c template V1.0)
   Copyright (C) 1995, 1996 Free Software Foundation, Inc.
   Contributed by James Craig Burley (burley@gnu.ai.mit.edu).

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
      ste.c

   Description:
      Implements the various statements and such like.

   Modifications:
*/

/* As of 0.5.4, any statement that calls on ffecom to transform an
   expression might need to be wrapped in ffecom_push_calltemps ()
   and ffecom_pop_calltemps () as are some other cases.  That is
   the case when the transformation might involve generation of
   a temporary that must be auto-popped, the specific case being
   when a COMPLEX operation requiring a call to libf2c being
   generated, whereby a temp is needed to hold the result since
   libf2c doesn't return COMPLEX results directly.  Cases where it
   is known that ffecom_expr () won't need to do this, such as
   the CALL statement (where it's the transformation of the
   call expr itself that does the wrapping), don't need to bother
   with this wrapping.  Forgetting to do the wrapping currently
   means a crash at an assertion when the wrapping would be helpful
   to keep temporaries from being wasted -- see ffecom_push_tempvar.  */

/* Include files. */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
#include "config.j"
#include "rtl.j"
#endif

#include "proj.h"
#include "ste.h"
#include "bld.h"
#include "com.h"
#include "expr.h"
#include "lab.h"
#include "lex.h"
#include "sta.h"
#include "stp.h"
#include "str.h"
#include "sts.h"
#include "stt.h"
#include "stv.h"
#include "stw.h"
#include "symbol.h"

/* Externals defined here. */


/* Simple definitions and enumerations. */

typedef enum
  {
    FFESTE_stateletSIMPLE_,	/* Expecting simple/start. */
    FFESTE_stateletATTRIB_,	/* Expecting attrib/item/itemstart. */
    FFESTE_stateletITEM_,	/* Expecting item/itemstart/finish. */
    FFESTE_stateletITEMVALS_,	/* Expecting itemvalue/itemendvals. */
    FFESTE_
  } ffesteStatelet_;

/* Internal typedefs. */


/* Private include files. */


/* Internal structure definitions. */


/* Static objects accessed by functions in this module. */

static ffesteStatelet_ ffeste_statelet_ = FFESTE_stateletSIMPLE_;
#if FFECOM_targetCURRENT == FFECOM_targetGCC
static ffelab ffeste_label_formatdef_ = NULL;
static tree (*ffeste_io_driver_) (ffebld expr);	/* do?io. */
static ffecomGfrt ffeste_io_endgfrt_;	/* end function to call. */
static tree ffeste_io_abort_;	/* abort-io label or NULL_TREE. */
static bool ffeste_io_abort_is_temp_;	/* abort-io label is a temp. */
static tree ffeste_io_end_;	/* END= label or NULL_TREE. */
static tree ffeste_io_err_;	/* ERR= label or NULL_TREE. */
static tree ffeste_io_iostat_;	/* IOSTAT= var or NULL_TREE. */
static bool ffeste_io_iostat_is_temp_;	/* IOSTAT= var is a temp. */
#endif

/* Static functions (internal). */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static void ffeste_begin_iterdo_ (ffestw block, tree *tvar, tree *tincr,
				  tree *xitersvar, ffebld var,
				  ffebld start, ffelexToken start_token,
				  ffebld end, ffelexToken end_token,
				  ffebld incr, ffelexToken incr_token,
				  char *msg);
static void ffeste_end_iterdo_ (tree tvar, tree tincr, tree itersvar);
static void ffeste_io_call_ (tree call, bool do_check);
static tree ffeste_io_dofio_ (ffebld expr);
static tree ffeste_io_dolio_ (ffebld expr);
static tree ffeste_io_douio_ (ffebld expr);
static tree ffeste_io_ialist_ (bool have_err, ffestvUnit unit,
			       ffebld unit_expr, int unit_dflt);
static tree ffeste_io_cilist_ (bool have_err, ffestvUnit unit,
			       ffebld unit_expr, int unit_dflt,
			       bool have_end, ffestvFormat format,
			       ffestpFile *format_spec, bool rec,
			       ffebld rec_expr);
static tree ffeste_io_cllist_ (bool have_err, ffebld unit_expr,
			       ffestpFile *stat_spec);
static tree ffeste_io_icilist_ (bool have_err, ffebld unit_expr,
				bool have_end, ffestvFormat format,
				ffestpFile *format_spec);
static void ffeste_io_impdo_ (ffebld impdo, ffelexToken impdo_token);
static tree ffeste_io_olist_ (bool have_err, ffebld unit_expr,
			      ffestpFile *file_spec,
			      ffestpFile *stat_spec,
			      ffestpFile *access_spec,
			      ffestpFile *form_spec,
			      ffestpFile *recl_spec,
			      ffestpFile *blank_spec);
static void ffeste_subr_beru_ (ffestpBeruStmt *info, ffecomGfrt rt);
#elif FFECOM_targetCURRENT == FFECOM_targetFFE
static void ffeste_subr_file_ (char *kw, ffestpFile *spec);
#else
#error
#endif

/* Internal macros. */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
#define ffeste_emit_line_note_() \
  emit_line_note (input_filename, lineno)
#endif
#define ffeste_check_simple_() \
  assert(ffeste_statelet_ == FFESTE_stateletSIMPLE_)
#define ffeste_check_start_() \
  assert(ffeste_statelet_ == FFESTE_stateletSIMPLE_); \
  ffeste_statelet_ = FFESTE_stateletATTRIB_
#define ffeste_check_attrib_() \
  assert(ffeste_statelet_ == FFESTE_stateletATTRIB_)
#define ffeste_check_item_() \
  assert(ffeste_statelet_ == FFESTE_stateletATTRIB_	 \
	 || ffeste_statelet_ == FFESTE_stateletITEM_); \
  ffeste_statelet_ = FFESTE_stateletITEM_
#define ffeste_check_item_startvals_() \
  assert(ffeste_statelet_ == FFESTE_stateletATTRIB_	 \
	 || ffeste_statelet_ == FFESTE_stateletITEM_); \
  ffeste_statelet_ = FFESTE_stateletITEMVALS_
#define ffeste_check_item_value_() \
  assert(ffeste_statelet_ == FFESTE_stateletITEMVALS_)
#define ffeste_check_item_endvals_() \
  assert(ffeste_statelet_ == FFESTE_stateletITEMVALS_); \
  ffeste_statelet_ = FFESTE_stateletITEM_
#define ffeste_check_finish_() \
  assert(ffeste_statelet_ == FFESTE_stateletATTRIB_	 \
	 || ffeste_statelet_ == FFESTE_stateletITEM_); \
  ffeste_statelet_ = FFESTE_stateletSIMPLE_

#define ffeste_f2c_charnolenspec_(Spec,Exp,Init)			    \
  do									      \
    {									      \
    if (Spec->kw_or_val_present)					      \
	Exp = ffecom_arg_ptr_to_expr(Spec->u.expr,&ignore);		 \
      else								      \
	Exp = null_pointer_node;					      \
    if (TREE_CONSTANT(Exp))						      \
	{								      \
	Init = Exp;							      \
	Exp = NULL_TREE;						      \
	}								      \
      else								      \
	{								      \
	Init = null_pointer_node;					      \
	constantp = FALSE;						      \
	}								      \
    } while(0)

#define ffeste_f2c_charspec_(Spec,Exp,Init,Lenexp,Leninit)		    \
  do									      \
    {									      \
    if (Spec->kw_or_val_present)					      \
	Exp = ffecom_arg_ptr_to_expr(Spec->u.expr,&Lenexp);		      \
      else								      \
	{								      \
	Exp = null_pointer_node;					      \
	Lenexp = ffecom_f2c_ftnlen_zero_node;				      \
	}								      \
    if (TREE_CONSTANT(Exp))						      \
	{								      \
	Init = Exp;							      \
	Exp = NULL_TREE;						      \
	}								      \
      else								      \
	{								      \
	Init = null_pointer_node;					      \
	constantp = FALSE;						      \
	}								      \
    if ((Lenexp != NULL_TREE) && TREE_CONSTANT(Lenexp))			      \
	{								      \
	Leninit = Lenexp;						      \
	Lenexp = NULL_TREE;						      \
	}								      \
      else								      \
	{								      \
	Leninit = ffecom_f2c_ftnlen_zero_node;				      \
	constantp = FALSE;						      \
	}								      \
    } while(0)

#define ffeste_f2c_exp_(Field,Exp)					      \
  do									      \
    {									      \
    if (Exp != NULL_TREE)						      \
	{								      \
	Exp = ffecom_modify(void_type_node,ffecom_2(COMPONENT_REF,	      \
	      TREE_TYPE(Field),t,Field),Exp);				      \
	expand_expr_stmt(Exp);						      \
	}								      \
    } while(0)

#define ffeste_f2c_init_(Init)						    \
  do									      \
    {									      \
    TREE_CHAIN(initn) = build_tree_list((field = TREE_CHAIN(field)),Init);    \
    initn = TREE_CHAIN(initn);						      \
    } while(0)

#define ffeste_f2c_flagspec_(Flag,Init)					      \
  do { Init = convert (ffecom_f2c_flag_type_node,			      \
		       Flag ? integer_one_node : integer_zero_node); }	      \
    while(0)

#define ffeste_f2c_intspec_(Spec,Exp,Init)				      \
  do									      \
    {									      \
    if (Spec->kw_or_val_present)					      \
	Exp = ffecom_expr(Spec->u.expr);				      \
      else								      \
	Exp = ffecom_integer_zero_node;					      \
    if (TREE_CONSTANT(Exp))						      \
	{								      \
	Init = Exp;							      \
	Exp = NULL_TREE;						      \
	}								      \
      else								      \
	{								      \
	Init = ffecom_integer_zero_node;				      \
	constantp = FALSE;						      \
	}								      \
    } while(0)

#define ffeste_f2c_ptrtointspec_(Spec,Exp,Init)				    \
  do									      \
    {									      \
    if (Spec->kw_or_val_present)					      \
	Exp = ffecom_ptr_to_expr(Spec->u.expr);				 \
      else								      \
	Exp = null_pointer_node;					      \
    if (TREE_CONSTANT(Exp))						      \
	{								      \
	Init = Exp;							      \
	Exp = NULL_TREE;						      \
	}								      \
      else								      \
	{								      \
	Init = null_pointer_node;					      \
	constantp = FALSE;						      \
	}								      \
    } while(0)


/* Begin an iterative DO loop.  Pass the block to start if applicable.

   NOTE: Does _two_ push_momentary () calls, which the caller must
   undo (by calling ffeste_end_iterdo_).  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static void
ffeste_begin_iterdo_ (ffestw block, tree *xtvar, tree *xtincr,
		      tree *xitersvar, ffebld var,
		      ffebld start, ffelexToken start_token,
		      ffebld end, ffelexToken end_token,
		      ffebld incr, ffelexToken incr_token,
		      char *msg)
{
  tree tvar;
  tree expr;
  tree tstart;
  tree tend;
  tree tincr;
  tree tincr_saved;
  tree niters;

  push_momentary ();		/* Want to save these throughout the loop. */

  tvar = ffecom_expr_rw (var);
  tincr = ffecom_expr (incr);

  /* Check whether incr is known to be zero, complain and fix.  */

  if (integer_zerop (tincr) || real_zerop (tincr))
    {
      ffebad_start (FFEBAD_DO_STEP_ZERO);
      ffebad_here (0, ffelex_token_where_line (incr_token),
		   ffelex_token_where_column (incr_token));
      ffebad_string (msg);
      ffebad_finish ();
      tincr = convert (TREE_TYPE (tvar), integer_one_node);
    }

  tincr_saved = ffecom_save_tree (tincr);

  push_momentary ();		/* Want to discard the rest after the loop. */

  tstart = ffecom_expr (start);
  tend = ffecom_expr (end);

  {				/* For warnings only, nothing else
				   happens here.  */
    tree try;

    if (!ffe_is_onetrip ())
      {
	try = ffecom_2 (MINUS_EXPR, TREE_TYPE (tvar),
			tend,
			tstart);

	try = ffecom_2 (PLUS_EXPR, TREE_TYPE (tvar),
			try,
			tincr);

	if (TREE_CODE (TREE_TYPE (tvar)) != REAL_TYPE)
	  try = ffecom_2 (TRUNC_DIV_EXPR, integer_type_node, try,
			  tincr);
	else
	  try = convert (integer_type_node,
			 ffecom_2 (RDIV_EXPR, TREE_TYPE (tvar),
				   try,
				   tincr));

	/* Warn if loop never executed, since we've done the evaluation
	   of the unofficial iteration count already.  */

	try = ffecom_truth_value (ffecom_2 (LE_EXPR, integer_type_node,
					    try,
					    convert (TREE_TYPE (tvar),
						     integer_zero_node)));

	if (integer_onep (try))
	  {
	    ffebad_start (FFEBAD_DO_NULL);
	    ffebad_here (0, ffelex_token_where_line (start_token),
			 ffelex_token_where_column (start_token));
	    ffebad_string (msg);
	    ffebad_finish ();
	  }
      }

    /* Warn if end plus incr would overflow.  */

    try = ffecom_2 (PLUS_EXPR, TREE_TYPE (tvar),
		    tend,
		    tincr);

    if ((TREE_CODE_CLASS (TREE_CODE (try)) == 'c')
	&& TREE_CONSTANT_OVERFLOW (try))
      {
	ffebad_start (FFEBAD_DO_END_OVERFLOW);
	ffebad_here (0, ffelex_token_where_line (end_token),
		     ffelex_token_where_column (end_token));
	ffebad_string (msg);
	ffebad_finish ();
      }
  }

  /* Do the initial assignment into the DO var.  */

  tstart = ffecom_save_tree (tstart);

  expr = ffecom_2 (MINUS_EXPR, TREE_TYPE (tvar),
		   tend,
		   tstart);

  if (!ffe_is_onetrip ())
    {
      expr = ffecom_2 (PLUS_EXPR, TREE_TYPE (expr),
		       expr,
		       convert (TREE_TYPE (expr), tincr_saved));
    }

  if (TREE_CODE (TREE_TYPE (tvar)) != REAL_TYPE)
    expr = ffecom_2 (TRUNC_DIV_EXPR, TREE_TYPE (expr),
		     expr,
		     tincr_saved);
  else
    expr = ffecom_2 (RDIV_EXPR, TREE_TYPE (expr),
		     expr,
		     tincr_saved);

#if 1	/* New, F90-approved approach: convert to default INTEGER. */
  if (TREE_TYPE (tvar) != error_mark_node)
    expr = convert (ffecom_integer_type_node, expr);
#else	/* Old approach; convert to INTEGER unless that's a narrowing. */
  if ((TREE_TYPE (tvar) != error_mark_node)
      && ((TREE_CODE (TREE_TYPE (tvar)) != INTEGER_TYPE)
	  || ((TYPE_SIZE (TREE_TYPE (tvar)) != NULL_TREE)
	      && ((TREE_CODE (TYPE_SIZE (TREE_TYPE (tvar)))
		   != INTEGER_CST)
		  || (TREE_INT_CST_LOW (TYPE_SIZE (TREE_TYPE (tvar)))
		      <= TREE_INT_CST_LOW (TYPE_SIZE (ffecom_integer_type_node)))))))
    /* Convert unless promoting INTEGER type of any kind downward to
       default INTEGER; else leave as, say, INTEGER*8 (long long int).  */
    expr = convert (ffecom_integer_type_node, expr);
#endif

  niters = ffecom_push_tempvar (TREE_TYPE (expr),
				FFETARGET_charactersizeNONE, -1, FALSE);
  expr = ffecom_modify (void_type_node, niters, expr);
  expand_expr_stmt (expr);

  expr = ffecom_modify (void_type_node, tvar, tstart);
  expand_expr_stmt (expr);

  if (block == NULL)
    expand_start_loop_continue_elsewhere (0);
  else
    ffestw_set_do_hook (block,
			expand_start_loop_continue_elsewhere (1));

  if (!ffe_is_onetrip ())
    {
      expr = ffecom_truth_value
	(ffecom_2 (GE_EXPR, integer_type_node,
		   ffecom_2 (PREDECREMENT_EXPR,
			     TREE_TYPE (niters),
			     niters,
			     convert (TREE_TYPE (niters),
				      ffecom_integer_one_node)),
		   convert (TREE_TYPE (niters),
			    ffecom_integer_zero_node)));

      expand_exit_loop_if_false (0, expr);
    }

  clear_momentary ();		/* Discard the above now that we're done with
				   DO stmt. */

  if (block == NULL)
    {
      *xtvar = tvar;
      *xtincr = tincr_saved;
      *xitersvar = niters;
    }
  else
    {
      ffestw_set_do_tvar (block, tvar);
      ffestw_set_do_incr_saved (block, tincr_saved);
      ffestw_set_do_count_var (block, niters);
    }
}

#endif

/* End an iterative DO loop.  Pass the same iteration variable and increment
   value trees that were generated in the paired _begin_ call.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static void
ffeste_end_iterdo_ (tree tvar, tree tincr, tree itersvar)
{
  tree expr;
  tree niters = itersvar;

  expand_loop_continue_here ();

  if (ffe_is_onetrip ())
    {
      expr = ffecom_truth_value
	(ffecom_2 (GE_EXPR, integer_type_node,
		   ffecom_2 (PREDECREMENT_EXPR,
			     TREE_TYPE (niters),
			     niters,
			     convert (TREE_TYPE (niters),
				      ffecom_integer_one_node)),
		   convert (TREE_TYPE (niters),
			    ffecom_integer_zero_node)));

      expand_exit_loop_if_false (0, expr);
    }

  expr = ffecom_modify (void_type_node, tvar,
			ffecom_2 (PLUS_EXPR, TREE_TYPE (tvar),
				  tvar,
				  tincr));
  expand_expr_stmt (expr);
  expand_end_loop ();

  ffecom_pop_tempvar (itersvar);	/* Free #iters var. */

  clear_momentary ();
  pop_momentary ();		/* Lose the stuff we just built. */

  clear_momentary ();
  pop_momentary ();		/* Lose the tvar and incr_saved trees. */
}

#endif
/* ffeste_io_call_ -- Generate call to run-time I/O routine

   tree callexpr = build(CALL_EXPR,...);
   ffeste_io_call_(callexpr,TRUE);

   Sets TREE_SIDE_EFFECTS(callexpr) = 1.  If ffeste_io_iostat_ is not
   NULL_TREE, replaces callexpr with "iostat = callexpr;".  Expands the
   result.  If ffeste_io_abort_ is not NULL_TREE and the second argument
   is TRUE, generates "if (iostat != 0) goto ffeste_io_abort_;".  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static void
ffeste_io_call_ (tree call, bool do_check)
{
  /* Generate the call and optional assignment into iostat var. */

  TREE_SIDE_EFFECTS (call) = 1;
  if (ffeste_io_iostat_ != NULL_TREE)
    {
      call = ffecom_modify (do_check ? NULL_TREE : void_type_node,
			    ffeste_io_iostat_, call);
    }
  expand_expr_stmt (call);

  if (!do_check
      || (ffeste_io_abort_ == NULL_TREE)
      || (TREE_CODE (ffeste_io_abort_) == ERROR_MARK))
    return;

  /* Generate optional test. */

  expand_start_cond (ffecom_truth_value (ffeste_io_iostat_), 0);
  expand_goto (ffeste_io_abort_);
  expand_end_cond ();
}

#endif
/* ffeste_io_dofio_ -- Generate call to do_fio for formatted I/O item

   ffebld expr;
   tree call;
   call = ffeste_io_dofio_(expr);

   Returns a tree for a CALL_EXPR to the do_fio function, which handles
   a formatted I/O list item, along with the appropriate arguments for
   the function.  It is up to the caller to set the TREE_SIDE_EFFECTS flag
   for the CALL_EXPR, expand (emit) the expression, emit any assignment
   of the result to an IOSTAT= variable, and emit any checking of the
   result for errors.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static tree
ffeste_io_dofio_ (ffebld expr)
{
  tree num_elements;
  tree variable;
  tree size;
  tree arglist;
  ffeinfoBasictype bt;
  ffeinfoKindtype kt;
  bool is_complex;

  bt = ffeinfo_basictype (ffebld_info (expr));
  kt = ffeinfo_kindtype (ffebld_info (expr));

  if ((bt == FFEINFO_basictypeANY)
      || (kt == FFEINFO_kindtypeANY))
    return error_mark_node;

  if (bt == FFEINFO_basictypeCOMPLEX)
    {
      is_complex = TRUE;
      bt = FFEINFO_basictypeREAL;
    }
  else
    is_complex = FALSE;

  ffecom_push_calltemps ();

  variable = ffecom_arg_ptr_to_expr (expr, &size);

  if ((variable == error_mark_node)
      || (size == error_mark_node))
    {
      ffecom_pop_calltemps ();
      return error_mark_node;
    }

  if (size == NULL_TREE)	/* Already filled in for CHARACTER type. */
    {				/* "(ftnlen) sizeof(type)" */
      size = size_binop (CEIL_DIV_EXPR,
			 TYPE_SIZE (ffecom_tree_type[bt][kt]),
			 size_int (TYPE_PRECISION (char_type_node)));
#if 0	/* Assume that while it is possible that char * is wider than
	   ftnlen, no object in Fortran space can get big enough for its
	   size to be wider than ftnlen.  I really hope nobody wastes
	   time debugging a case where it can!  */
      assert (TYPE_PRECISION (ffecom_f2c_ftnlen_type_node)
	      >= TYPE_PRECISION (TREE_TYPE (size)));
#endif
      size = convert (ffecom_f2c_ftnlen_type_node, size);
    }

  if ((ffeinfo_rank (ffebld_info (expr)) == 0)
      || (TREE_CODE (TREE_TYPE (TREE_TYPE (variable))) != ARRAY_TYPE))
    num_elements = is_complex ? ffecom_f2c_ftnlen_two_node
      : ffecom_f2c_ftnlen_one_node;
  else
    {
      num_elements = size_binop (CEIL_DIV_EXPR,
			TYPE_SIZE (TREE_TYPE (TREE_TYPE (variable))), size);
      num_elements = size_binop (CEIL_DIV_EXPR, num_elements,
				 size_int (TYPE_PRECISION
					   (char_type_node)));
      num_elements = convert (ffecom_f2c_ftnlen_type_node,
			      num_elements);
    }

  num_elements
    = ffecom_1 (ADDR_EXPR, ffecom_f2c_ptr_to_ftnlen_type_node,
		num_elements);

  variable = convert (string_type_node, variable);

  arglist = build_tree_list (NULL_TREE, num_elements);
  TREE_CHAIN (arglist) = build_tree_list (NULL_TREE, variable);
  TREE_CHAIN (TREE_CHAIN (arglist)) = build_tree_list (NULL_TREE, size);

  ffecom_pop_calltemps ();

  return ffecom_call_gfrt (FFECOM_gfrtDOFIO, arglist);
}

#endif
/* ffeste_io_dolio_ -- Generate call to do_lio for list-directed I/O item

   ffebld expr;
   tree call;
   call = ffeste_io_dolio_(expr);

   Returns a tree for a CALL_EXPR to the do_lio function, which handles
   a list-directed I/O list item, along with the appropriate arguments for
   the function.  It is up to the caller to set the TREE_SIDE_EFFECTS flag
   for the CALL_EXPR, expand (emit) the expression, emit any assignment
   of the result to an IOSTAT= variable, and emit any checking of the
   result for errors.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static tree
ffeste_io_dolio_ (ffebld expr)
{
  tree type_id;
  tree num_elements;
  tree variable;
  tree size;
  tree arglist;
  ffeinfoBasictype bt;
  ffeinfoKindtype kt;
  int tc;

  bt = ffeinfo_basictype (ffebld_info (expr));
  kt = ffeinfo_kindtype (ffebld_info (expr));

  if ((bt == FFEINFO_basictypeANY)
      || (kt == FFEINFO_kindtypeANY))
    return error_mark_node;

  ffecom_push_calltemps ();

  tc = ffecom_f2c_typecode (bt, kt);
  assert (tc != -1);
  type_id = build_int_2 (tc, 0);

  type_id
    = ffecom_1 (ADDR_EXPR, ffecom_f2c_ptr_to_ftnint_type_node,
		convert (ffecom_f2c_ftnint_type_node,
			 type_id));

  variable = ffecom_arg_ptr_to_expr (expr, &size);

  if ((type_id == error_mark_node)
      || (variable == error_mark_node)
      || (size == error_mark_node))
    {
      ffecom_pop_calltemps ();
      return error_mark_node;
    }

  if (size == NULL_TREE)	/* Already filled in for CHARACTER type. */
    {				/* "(ftnlen) sizeof(type)" */
      size = size_binop (CEIL_DIV_EXPR,
			 TYPE_SIZE (ffecom_tree_type[bt][kt]),
			 size_int (TYPE_PRECISION (char_type_node)));
#if 0	/* Assume that while it is possible that char * is wider than
	   ftnlen, no object in Fortran space can get big enough for its
	   size to be wider than ftnlen.  I really hope nobody wastes
	   time debugging a case where it can!  */
      assert (TYPE_PRECISION (ffecom_f2c_ftnlen_type_node)
	      >= TYPE_PRECISION (TREE_TYPE (size)));
#endif
      size = convert (ffecom_f2c_ftnlen_type_node, size);
    }

  if ((ffeinfo_rank (ffebld_info (expr)) == 0)
      || (TREE_CODE (TREE_TYPE (TREE_TYPE (variable))) != ARRAY_TYPE))
    num_elements = ffecom_integer_one_node;
  else
    {
      num_elements = size_binop (CEIL_DIV_EXPR,
			TYPE_SIZE (TREE_TYPE (TREE_TYPE (variable))), size);
      num_elements = size_binop (CEIL_DIV_EXPR,
				 num_elements, size_int (TYPE_PRECISION
							 (char_type_node)));
      num_elements = convert (ffecom_f2c_ftnlen_type_node,
			      num_elements);
    }

  num_elements
    = ffecom_1 (ADDR_EXPR, ffecom_f2c_ptr_to_ftnlen_type_node,
		num_elements);

  variable = convert (string_type_node, variable);

  arglist = build_tree_list (NULL_TREE, type_id);
  TREE_CHAIN (arglist) = build_tree_list (NULL_TREE, num_elements);
  TREE_CHAIN (TREE_CHAIN (arglist)) = build_tree_list (NULL_TREE, variable);
  TREE_CHAIN (TREE_CHAIN (TREE_CHAIN (arglist)))
    = build_tree_list (NULL_TREE, size);

  ffecom_pop_calltemps ();

  return ffecom_call_gfrt (FFECOM_gfrtDOLIO, arglist);
}

#endif
/* ffeste_io_douio_ -- Generate call to do_uio for unformatted I/O item

   ffebld expr;
   tree call;
   call = ffeste_io_douio_(expr);

   Returns a tree for a CALL_EXPR to the do_uio function, which handles
   an unformatted I/O list item, along with the appropriate arguments for
   the function.  It is up to the caller to set the TREE_SIDE_EFFECTS flag
   for the CALL_EXPR, expand (emit) the expression, emit any assignment
   of the result to an IOSTAT= variable, and emit any checking of the
   result for errors.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static tree
ffeste_io_douio_ (ffebld expr)
{
  tree num_elements;
  tree variable;
  tree size;
  tree arglist;
  ffeinfoBasictype bt;
  ffeinfoKindtype kt;
  bool is_complex;

  bt = ffeinfo_basictype (ffebld_info (expr));
  kt = ffeinfo_kindtype (ffebld_info (expr));

  if ((bt == FFEINFO_basictypeANY)
      || (kt == FFEINFO_kindtypeANY))
    return error_mark_node;

  if (bt == FFEINFO_basictypeCOMPLEX)
    {
      is_complex = TRUE;
      bt = FFEINFO_basictypeREAL;
    }
  else
    is_complex = FALSE;

  ffecom_push_calltemps ();

  variable = ffecom_arg_ptr_to_expr (expr, &size);

  if ((variable == error_mark_node)
      || (size == error_mark_node))
    {
      ffecom_pop_calltemps ();
      return error_mark_node;
    }

  if (size == NULL_TREE)	/* Already filled in for CHARACTER type. */
    {				/* "(ftnlen) sizeof(type)" */
      size = size_binop (CEIL_DIV_EXPR,
			 TYPE_SIZE (ffecom_tree_type[bt][kt]),
			 size_int (TYPE_PRECISION (char_type_node)));
#if 0	/* Assume that while it is possible that char * is wider than
	   ftnlen, no object in Fortran space can get big enough for its
	   size to be wider than ftnlen.  I really hope nobody wastes
	   time debugging a case where it can!  */
      assert (TYPE_PRECISION (ffecom_f2c_ftnlen_type_node)
	      >= TYPE_PRECISION (TREE_TYPE (size)));
#endif
      size = convert (ffecom_f2c_ftnlen_type_node, size);
    }

  if ((ffeinfo_rank (ffebld_info (expr)) == 0)
      || (TREE_CODE (TREE_TYPE (TREE_TYPE (variable))) != ARRAY_TYPE))
    num_elements = is_complex ? ffecom_f2c_ftnlen_two_node
      : ffecom_f2c_ftnlen_one_node;
  else
    {
      num_elements = size_binop (CEIL_DIV_EXPR,
			TYPE_SIZE (TREE_TYPE (TREE_TYPE (variable))), size);
      num_elements = size_binop (CEIL_DIV_EXPR, num_elements,
				 size_int (TYPE_PRECISION
					   (char_type_node)));
      num_elements = convert (ffecom_f2c_ftnlen_type_node,
			      num_elements);
    }

  num_elements
    = ffecom_1 (ADDR_EXPR, ffecom_f2c_ptr_to_ftnlen_type_node,
		num_elements);

  variable = convert (string_type_node, variable);

  arglist = build_tree_list (NULL_TREE, num_elements);
  TREE_CHAIN (arglist) = build_tree_list (NULL_TREE, variable);
  TREE_CHAIN (TREE_CHAIN (arglist)) = build_tree_list (NULL_TREE, size);

  ffecom_pop_calltemps ();

  return ffecom_call_gfrt (FFECOM_gfrtDOUIO, arglist);
}

#endif
/* ffeste_io_ialist_ -- Make arglist with ptr to B/E/R control list

   tree arglist;
   arglist = ffeste_io_ialist_(...);

   Returns a tree suitable as an argument list containing a pointer to
   a BACKSPACE/ENDFILE/REWIND control list.  First, generates that control
   list, if necessary, along with any static and run-time initializations
   that are needed as specified by the arguments to this function.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static tree
ffeste_io_ialist_ (bool have_err,
		   ffestvUnit unit,
		   ffebld unit_expr,
		   int unit_dflt)
{
  static tree f2c_alist_struct = NULL_TREE;
  tree t;
  tree ttype;
  int yes;
  tree field;
  tree inits, initn;
  bool constantp = TRUE;
  static tree errfield, unitfield;
  tree errinit, unitinit;
  tree unitexp;
  static int mynumber = 0;

  if (f2c_alist_struct == NULL_TREE)
    {
      tree ref;

      push_obstacks_nochange ();
      end_temporary_allocation ();

      ref = make_node (RECORD_TYPE);

      errfield = ffecom_decl_field (ref, NULL_TREE, "err",
				    ffecom_f2c_flag_type_node);
      unitfield = ffecom_decl_field (ref, errfield, "unit",
				     ffecom_f2c_ftnint_type_node);

      TYPE_FIELDS (ref) = errfield;
      layout_type (ref);

      resume_temporary_allocation ();
      pop_obstacks ();

      f2c_alist_struct = ref;
    }

  ffeste_f2c_flagspec_ (have_err, errinit);

  switch (unit)
    {
    case FFESTV_unitNONE:
    case FFESTV_unitASTERISK:
      unitinit = build_int_2 (unit_dflt, 0);
      unitexp = NULL_TREE;
      break;

    case FFESTV_unitINTEXPR:
      unitexp = ffecom_expr (unit_expr);
      if (TREE_CONSTANT (unitexp))
	{
	  unitinit = unitexp;
	  unitexp = NULL_TREE;
	}
      else
	{
	  unitinit = ffecom_integer_zero_node;
	  constantp = FALSE;
	}
      break;

    default:
      assert ("bad unit spec" == NULL);
      unitexp = NULL_TREE;
      unitinit = ffecom_integer_zero_node;
      break;
    }

  inits = build_tree_list ((field = TYPE_FIELDS (f2c_alist_struct)), errinit);
  initn = inits;
  ffeste_f2c_init_ (unitinit);

  inits = build (CONSTRUCTOR, f2c_alist_struct, NULL_TREE, inits);
  TREE_CONSTANT (inits) = constantp ? 1 : 0;
  TREE_STATIC (inits) = 1;

  yes = suspend_momentary ();

  t = build_decl (VAR_DECL,
		  ffecom_get_invented_identifier ("__g77_alist_%d", NULL,
						  mynumber++),
		  f2c_alist_struct);
  TREE_STATIC (t) = 1;
  t = ffecom_start_decl (t, 1);
  ffecom_finish_decl (t, inits, 0);

  resume_momentary (yes);

  ffeste_f2c_exp_ (unitfield, unitexp);

  ttype = build_pointer_type (TREE_TYPE (t));
  t = ffecom_1 (ADDR_EXPR, ttype, t);

  t = build_tree_list (NULL_TREE, t);

  return t;
}

#endif
/* ffeste_io_cilist_ -- Make arglist with ptr to external I/O control list

   tree arglist;
   arglist = ffeste_io_cilist_(...);

   Returns a tree suitable as an argument list containing a pointer to
   an external-file I/O control list.  First, generates that control
   list, if necessary, along with any static and run-time initializations
   that are needed as specified by the arguments to this function.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static tree
ffeste_io_cilist_ (bool have_err,
		   ffestvUnit unit,
		   ffebld unit_expr,
		   int unit_dflt,
		   bool have_end,
		   ffestvFormat format,
		   ffestpFile *format_spec,
		   bool rec,
		   ffebld rec_expr)
{
  static tree f2c_cilist_struct = NULL_TREE;
  tree t;
  tree ttype;
  int yes;
  tree field;
  tree inits, initn;
  tree ignore;			/* We ignore the length of format! */
  bool constantp = TRUE;
  static tree errfield, unitfield, endfield, formatfield, recfield;
  tree errinit, unitinit, endinit, formatinit, recinit;
  tree unitexp, formatexp, recexp;
  static int mynumber = 0;

  if (f2c_cilist_struct == NULL_TREE)
    {
      tree ref;

      push_obstacks_nochange ();
      end_temporary_allocation ();

      ref = make_node (RECORD_TYPE);

      errfield = ffecom_decl_field (ref, NULL_TREE, "err",
				    ffecom_f2c_flag_type_node);
      unitfield = ffecom_decl_field (ref, errfield, "unit",
				     ffecom_f2c_ftnint_type_node);
      endfield = ffecom_decl_field (ref, unitfield, "end",
				    ffecom_f2c_flag_type_node);
      formatfield = ffecom_decl_field (ref, endfield, "format",
				       string_type_node);
      recfield = ffecom_decl_field (ref, formatfield, "rec",
				    ffecom_f2c_ftnint_type_node);

      TYPE_FIELDS (ref) = errfield;
      layout_type (ref);

      resume_temporary_allocation ();
      pop_obstacks ();

      f2c_cilist_struct = ref;
    }

  ffeste_f2c_flagspec_ (have_err, errinit);

  switch (unit)
    {
    case FFESTV_unitNONE:
    case FFESTV_unitASTERISK:
      unitinit = build_int_2 (unit_dflt, 0);
      unitexp = NULL_TREE;
      break;

    case FFESTV_unitINTEXPR:
      unitexp = ffecom_expr (unit_expr);
      if (TREE_CONSTANT (unitexp))
	{
	  unitinit = unitexp;
	  unitexp = NULL_TREE;
	}
      else
	{
	  unitinit = ffecom_integer_zero_node;
	  constantp = FALSE;
	}
      break;

    default:
      assert ("bad unit spec" == NULL);
      unitexp = NULL_TREE;
      unitinit = ffecom_integer_zero_node;
      break;
    }

  switch (format)
    {
    case FFESTV_formatNONE:
      formatinit = null_pointer_node;
      formatexp = NULL_TREE;
      break;

    case FFESTV_formatLABEL:
      formatexp = NULL_TREE;
      formatinit = ffecom_lookup_label (format_spec->u.label);
      if ((formatinit == NULL_TREE)
	  || (TREE_CODE (formatinit) == ERROR_MARK))
	break;
      formatinit = ffecom_1 (ADDR_EXPR,
			     build_pointer_type (void_type_node),
			     formatinit);
      TREE_CONSTANT (formatinit) = 1;
      break;

    case FFESTV_formatCHAREXPR:
      formatexp = ffecom_arg_ptr_to_expr (format_spec->u.expr, &ignore);
      if (TREE_CONSTANT (formatexp))
	{
	  formatinit = formatexp;
	  formatexp = NULL_TREE;
	}
      else
	{
	  formatinit = null_pointer_node;
	  constantp = FALSE;
	}
      break;

    case FFESTV_formatASTERISK:
      formatinit = null_pointer_node;
      formatexp = NULL_TREE;
      break;

    case FFESTV_formatINTEXPR:
      formatinit = null_pointer_node;
      formatexp = ffecom_expr_assign (format_spec->u.expr);
      if (GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (formatexp)))
	  < GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (null_pointer_node))))
	error ("ASSIGNed FORMAT specifier is too small");
      formatexp = convert (string_type_node, formatexp);
      break;

    case FFESTV_formatNAMELIST:
      formatinit = ffecom_expr (format_spec->u.expr);
      formatexp = NULL_TREE;
      break;

    default:
      assert ("bad format spec" == NULL);
      formatexp = NULL_TREE;
      formatinit = integer_zero_node;
      break;
    }

  ffeste_f2c_flagspec_ (have_end, endinit);

  if (rec)
    recexp = ffecom_expr (rec_expr);
  else
    recexp = ffecom_integer_zero_node;
  if (TREE_CONSTANT (recexp))
    {
      recinit = recexp;
      recexp = NULL_TREE;
    }
  else
    {
      recinit = ffecom_integer_zero_node;
      constantp = FALSE;
    }

  inits = build_tree_list ((field = TYPE_FIELDS (f2c_cilist_struct)), errinit);
  initn = inits;
  ffeste_f2c_init_ (unitinit);
  ffeste_f2c_init_ (endinit);
  ffeste_f2c_init_ (formatinit);
  ffeste_f2c_init_ (recinit);

  inits = build (CONSTRUCTOR, f2c_cilist_struct, NULL_TREE, inits);
  TREE_CONSTANT (inits) = constantp ? 1 : 0;
  TREE_STATIC (inits) = 1;

  yes = suspend_momentary ();

  t = build_decl (VAR_DECL,
		  ffecom_get_invented_identifier ("__g77_cilist_%d", NULL,
						  mynumber++),
		  f2c_cilist_struct);
  TREE_STATIC (t) = 1;
  t = ffecom_start_decl (t, 1);
  ffecom_finish_decl (t, inits, 0);

  resume_momentary (yes);

  ffeste_f2c_exp_ (unitfield, unitexp);
  ffeste_f2c_exp_ (formatfield, formatexp);
  ffeste_f2c_exp_ (recfield, recexp);

  ttype = build_pointer_type (TREE_TYPE (t));
  t = ffecom_1 (ADDR_EXPR, ttype, t);

  t = build_tree_list (NULL_TREE, t);

  return t;
}

#endif
/* ffeste_io_cllist_ -- Make arglist with ptr to CLOSE control list

   tree arglist;
   arglist = ffeste_io_cllist_(...);

   Returns a tree suitable as an argument list containing a pointer to
   a CLOSE-statement control list.  First, generates that control
   list, if necessary, along with any static and run-time initializations
   that are needed as specified by the arguments to this function.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static tree
ffeste_io_cllist_ (bool have_err,
		   ffebld unit_expr,
		   ffestpFile *stat_spec)
{
  static tree f2c_close_struct = NULL_TREE;
  tree t;
  tree ttype;
  int yes;
  tree field;
  tree inits, initn;
  tree ignore;			/* Ignore length info for certain fields. */
  bool constantp = TRUE;
  static tree errfield, unitfield, statfield;
  tree errinit, unitinit, statinit;
  tree unitexp, statexp;
  static int mynumber = 0;

  if (f2c_close_struct == NULL_TREE)
    {
      tree ref;

      push_obstacks_nochange ();
      end_temporary_allocation ();

      ref = make_node (RECORD_TYPE);

      errfield = ffecom_decl_field (ref, NULL_TREE, "err",
				    ffecom_f2c_flag_type_node);
      unitfield = ffecom_decl_field (ref, errfield, "unit",
				     ffecom_f2c_ftnint_type_node);
      statfield = ffecom_decl_field (ref, unitfield, "stat",
				     string_type_node);

      TYPE_FIELDS (ref) = errfield;
      layout_type (ref);

      resume_temporary_allocation ();
      pop_obstacks ();

      f2c_close_struct = ref;
    }

  ffeste_f2c_flagspec_ (have_err, errinit);

  unitexp = ffecom_expr (unit_expr);
  if (TREE_CONSTANT (unitexp))
    {
      unitinit = unitexp;
      unitexp = NULL_TREE;
    }
  else
    {
      unitinit = ffecom_integer_zero_node;
      constantp = FALSE;
    }

  ffeste_f2c_charnolenspec_ (stat_spec, statexp, statinit);

  inits = build_tree_list ((field = TYPE_FIELDS (f2c_close_struct)), errinit);
  initn = inits;
  ffeste_f2c_init_ (unitinit);
  ffeste_f2c_init_ (statinit);

  inits = build (CONSTRUCTOR, f2c_close_struct, NULL_TREE, inits);
  TREE_CONSTANT (inits) = constantp ? 1 : 0;
  TREE_STATIC (inits) = 1;

  yes = suspend_momentary ();

  t = build_decl (VAR_DECL,
		  ffecom_get_invented_identifier ("__g77_cllist_%d", NULL,
						  mynumber++),
		  f2c_close_struct);
  TREE_STATIC (t) = 1;
  t = ffecom_start_decl (t, 1);
  ffecom_finish_decl (t, inits, 0);

  resume_momentary (yes);

  ffeste_f2c_exp_ (unitfield, unitexp);
  ffeste_f2c_exp_ (statfield, statexp);

  ttype = build_pointer_type (TREE_TYPE (t));
  t = ffecom_1 (ADDR_EXPR, ttype, t);

  t = build_tree_list (NULL_TREE, t);

  return t;
}

#endif
/* ffeste_io_icilist_ -- Make arglist with ptr to internal I/O control list

   tree arglist;
   arglist = ffeste_io_icilist_(...);

   Returns a tree suitable as an argument list containing a pointer to
   an internal-file I/O control list.  First, generates that control
   list, if necessary, along with any static and run-time initializations
   that are needed as specified by the arguments to this function.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static tree
ffeste_io_icilist_ (bool have_err,
		    ffebld unit_expr,
		    bool have_end,
		    ffestvFormat format,
		    ffestpFile *format_spec)
{
  static tree f2c_icilist_struct = NULL_TREE;
  tree t;
  tree ttype;
  int yes;
  tree field;
  tree inits, initn;
  tree ignore;			/* We ignore the length of format! */
  bool constantp = TRUE;
  static tree errfield, unitfield, endfield, formatfield, unitlenfield,
    unitnumfield;
  tree errinit, unitinit, endinit, formatinit, unitleninit, unitnuminit;
  tree unitexp, formatexp, unitlenexp, unitnumexp;
  static int mynumber = 0;

  if (f2c_icilist_struct == NULL_TREE)
    {
      tree ref;

      push_obstacks_nochange ();
      end_temporary_allocation ();

      ref = make_node (RECORD_TYPE);

      errfield = ffecom_decl_field (ref, NULL_TREE, "err",
				    ffecom_f2c_flag_type_node);
      unitfield = ffecom_decl_field (ref, errfield, "unit",
				     string_type_node);
      endfield = ffecom_decl_field (ref, unitfield, "end",
				    ffecom_f2c_flag_type_node);
      formatfield = ffecom_decl_field (ref, endfield, "format",
				       string_type_node);
      unitlenfield = ffecom_decl_field (ref, formatfield, "unitlen",
					ffecom_f2c_ftnint_type_node);
      unitnumfield = ffecom_decl_field (ref, unitlenfield, "unitnum",
					ffecom_f2c_ftnint_type_node);

      TYPE_FIELDS (ref) = errfield;
      layout_type (ref);

      resume_temporary_allocation ();
      pop_obstacks ();

      f2c_icilist_struct = ref;
    }

  ffeste_f2c_flagspec_ (have_err, errinit);

  unitexp = ffecom_arg_ptr_to_expr (unit_expr, &unitlenexp);
  if ((ffeinfo_rank (ffebld_info (unit_expr)) == 0)
      || (TREE_CODE (TREE_TYPE (TREE_TYPE (unitexp))) != ARRAY_TYPE))
    unitnumexp = ffecom_integer_one_node;
  else
    {
      unitnumexp = size_binop (CEIL_DIV_EXPR,
		   TYPE_SIZE (TREE_TYPE (TREE_TYPE (unitexp))), unitlenexp);
      unitnumexp = size_binop (CEIL_DIV_EXPR,
			       unitnumexp, size_int (TYPE_PRECISION
						     (char_type_node)));
    }
  if (TREE_CONSTANT (unitexp))
    {
      unitinit = unitexp;
      unitexp = NULL_TREE;
    }
  else
    {
      unitinit = null_pointer_node;
      constantp = FALSE;
    }
  if ((unitlenexp != NULL_TREE) && TREE_CONSTANT (unitlenexp))
    {
      unitleninit = unitlenexp;
      unitlenexp = NULL_TREE;
    }
  else
    {
      unitleninit = ffecom_integer_zero_node;
      constantp = FALSE;
    }
  if (TREE_CONSTANT (unitnumexp))
    {
      unitnuminit = unitnumexp;
      unitnumexp = NULL_TREE;
    }
  else
    {
      unitnuminit = ffecom_integer_zero_node;
      constantp = FALSE;
    }

  switch (format)
    {
    case FFESTV_formatNONE:
      formatinit = null_pointer_node;
      formatexp = NULL_TREE;
      break;

    case FFESTV_formatLABEL:
      formatexp = NULL_TREE;
      formatinit = ffecom_lookup_label (format_spec->u.label);
      if ((formatinit == NULL_TREE)
	  || (TREE_CODE (formatinit) == ERROR_MARK))
	break;
      formatinit = ffecom_1 (ADDR_EXPR,
			     build_pointer_type (void_type_node),
			     formatinit);
      TREE_CONSTANT (formatinit) = 1;
      break;

    case FFESTV_formatCHAREXPR:
      formatexp = ffecom_arg_ptr_to_expr (format_spec->u.expr, &ignore);
      if (TREE_CONSTANT (formatexp))
	{
	  formatinit = formatexp;
	  formatexp = NULL_TREE;
	}
      else
	{
	  formatinit = null_pointer_node;
	  constantp = FALSE;
	}
      break;

    case FFESTV_formatASTERISK:
      formatinit = null_pointer_node;
      formatexp = NULL_TREE;
      break;

    case FFESTV_formatINTEXPR:
      formatinit = null_pointer_node;
      formatexp = ffecom_expr_assign (format_spec->u.expr);
      if (GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (formatexp)))
	  < GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (null_pointer_node))))
	error ("ASSIGNed FORMAT specifier is too small");
      formatexp = convert (string_type_node, formatexp);
      break;

    default:
      assert ("bad format spec" == NULL);
      formatexp = NULL_TREE;
      formatinit = ffecom_integer_zero_node;
      break;
    }

  ffeste_f2c_flagspec_ (have_end, endinit);

  inits = build_tree_list ((field = TYPE_FIELDS (f2c_icilist_struct)),
			   errinit);
  initn = inits;
  ffeste_f2c_init_ (unitinit);
  ffeste_f2c_init_ (endinit);
  ffeste_f2c_init_ (formatinit);
  ffeste_f2c_init_ (unitleninit);
  ffeste_f2c_init_ (unitnuminit);

  inits = build (CONSTRUCTOR, f2c_icilist_struct, NULL_TREE, inits);
  TREE_CONSTANT (inits) = constantp ? 1 : 0;
  TREE_STATIC (inits) = 1;

  yes = suspend_momentary ();

  t = build_decl (VAR_DECL,
		  ffecom_get_invented_identifier ("__g77_icilist_%d", NULL,
						  mynumber++),
		  f2c_icilist_struct);
  TREE_STATIC (t) = 1;
  t = ffecom_start_decl (t, 1);
  ffecom_finish_decl (t, inits, 0);

  resume_momentary (yes);

  ffeste_f2c_exp_ (unitfield, unitexp);
  ffeste_f2c_exp_ (formatfield, formatexp);
  ffeste_f2c_exp_ (unitlenfield, unitlenexp);
  ffeste_f2c_exp_ (unitnumfield, unitnumexp);

  ttype = build_pointer_type (TREE_TYPE (t));
  t = ffecom_1 (ADDR_EXPR, ttype, t);

  t = build_tree_list (NULL_TREE, t);

  return t;
}

#endif
/* ffeste_io_impdo_ -- Handle implied-DO in I/O list

   ffebld expr;
   ffeste_io_impdo_(expr);

   Expands code to start up the DO loop.  Then for each item in the
   DO loop, handles appropriately (possibly including recursively calling
   itself).  Then expands code to end the DO loop.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static void
ffeste_io_impdo_ (ffebld impdo, ffelexToken impdo_token)
{
  ffebld var = ffebld_head (ffebld_right (impdo));
  ffebld start = ffebld_head (ffebld_trail (ffebld_right (impdo)));
  ffebld end = ffebld_head (ffebld_trail (ffebld_trail
					  (ffebld_right (impdo))));
  ffebld incr = ffebld_head (ffebld_trail (ffebld_trail
				    (ffebld_trail (ffebld_right (impdo)))));
  ffebld list;			/* Used for list of items in left part of
				   impdo. */
  ffebld item;			/* I/O item from head of given list. */
  tree tvar;
  tree tincr;
  tree titervar;

  if (incr == NULL)
    {
      incr = ffebld_new_conter (ffebld_constant_new_integerdefault_val (1));
      ffebld_set_info (incr, ffeinfo_new
		       (FFEINFO_basictypeINTEGER,
			FFEINFO_kindtypeINTEGERDEFAULT,
			0,
			FFEINFO_kindENTITY,
			FFEINFO_whereCONSTANT,
			FFETARGET_charactersizeNONE));
    }

  /* Start the DO loop.  */

  start = ffeexpr_convert_expr (start, impdo_token, var, impdo_token,
				FFEEXPR_contextLET);
  end = ffeexpr_convert_expr (end, impdo_token, var, impdo_token,
			      FFEEXPR_contextLET);
  incr = ffeexpr_convert_expr (incr, impdo_token, var, impdo_token,
			       FFEEXPR_contextLET);

  ffeste_begin_iterdo_ (NULL, &tvar, &tincr, &titervar, var,
			start, impdo_token,
			end, impdo_token,
			incr, impdo_token,
			"Implied DO loop");

  /* Handle the list of items.  */

  for (list = ffebld_left (impdo); list != NULL; list = ffebld_trail (list))
    {
      item = ffebld_head (list);
      if (item == NULL)
	continue;
      while (ffebld_op (item) == FFEBLD_opPAREN)
	item = ffebld_left (item);
      if (ffebld_op (item) == FFEBLD_opANY)
	continue;
      if (ffebld_op (item) == FFEBLD_opIMPDO)
	ffeste_io_impdo_ (item, impdo_token);
      else
	ffeste_io_call_ ((*ffeste_io_driver_) (item), TRUE);
      clear_momentary ();
    }

  /* Generate end of implied-do construct. */

  ffeste_end_iterdo_ (tvar, tincr, titervar);
}

#endif
/* ffeste_io_inlist_ -- Make arglist with ptr to INQUIRE control list

   tree arglist;
   arglist = ffeste_io_inlist_(...);

   Returns a tree suitable as an argument list containing a pointer to
   an INQUIRE-statement control list.  First, generates that control
   list, if necessary, along with any static and run-time initializations
   that are needed as specified by the arguments to this function.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static tree
ffeste_io_inlist_ (bool have_err,
		   ffestpFile *unit_spec,
		   ffestpFile *file_spec,
		   ffestpFile *exist_spec,
		   ffestpFile *open_spec,
		   ffestpFile *number_spec,
		   ffestpFile *named_spec,
		   ffestpFile *name_spec,
		   ffestpFile *access_spec,
		   ffestpFile *sequential_spec,
		   ffestpFile *direct_spec,
		   ffestpFile *form_spec,
		   ffestpFile *formatted_spec,
		   ffestpFile *unformatted_spec,
		   ffestpFile *recl_spec,
		   ffestpFile *nextrec_spec,
		   ffestpFile *blank_spec)
{
  static tree f2c_inquire_struct = NULL_TREE;
  tree t;
  tree ttype;
  int yes;
  tree field;
  tree inits, initn;
  bool constantp = TRUE;
  static tree errfield, unitfield, filefield, filelenfield, existfield,
    openfield, numberfield, namedfield, namefield, namelenfield, accessfield,
    accesslenfield, sequentialfield, sequentiallenfield, directfield, directlenfield,
    formfield, formlenfield, formattedfield, formattedlenfield, unformattedfield,
    unformattedlenfield, reclfield, nextrecfield, blankfield, blanklenfield;
  tree errinit, unitinit, fileinit, fileleninit, existinit, openinit, numberinit,
    namedinit, nameinit, nameleninit, accessinit, accessleninit, sequentialinit,
    sequentialleninit, directinit, directleninit, forminit, formleninit,
    formattedinit, formattedleninit, unformattedinit, unformattedleninit,
    reclinit, nextrecinit, blankinit, blankleninit;
  tree
    unitexp, fileexp, filelenexp, existexp, openexp, numberexp, namedexp,
    nameexp, namelenexp, accessexp, accesslenexp, sequentialexp, sequentiallenexp,
    directexp, directlenexp, formexp, formlenexp, formattedexp, formattedlenexp,
    unformattedexp, unformattedlenexp, reclexp, nextrecexp, blankexp, blanklenexp;
  static int mynumber = 0;

  if (f2c_inquire_struct == NULL_TREE)
    {
      tree ref;

      push_obstacks_nochange ();
      end_temporary_allocation ();

      ref = make_node (RECORD_TYPE);

      errfield = ffecom_decl_field (ref, NULL_TREE, "err",
				    ffecom_f2c_flag_type_node);
      unitfield = ffecom_decl_field (ref, errfield, "unit",
				     ffecom_f2c_ftnint_type_node);
      filefield = ffecom_decl_field (ref, unitfield, "file",
				     string_type_node);
      filelenfield = ffecom_decl_field (ref, filefield, "filelen",
					ffecom_f2c_ftnlen_type_node);
      existfield = ffecom_decl_field (ref, filelenfield, "exist",
				      ffecom_f2c_ptr_to_ftnint_type_node);
      openfield = ffecom_decl_field (ref, existfield, "open",
				     ffecom_f2c_ptr_to_ftnint_type_node);
      numberfield = ffecom_decl_field (ref, openfield, "number",
				       ffecom_f2c_ptr_to_ftnint_type_node);
      namedfield = ffecom_decl_field (ref, numberfield, "named",
				      ffecom_f2c_ptr_to_ftnint_type_node);
      namefield = ffecom_decl_field (ref, namedfield, "name",
				     string_type_node);
      namelenfield = ffecom_decl_field (ref, namefield, "namelen",
					ffecom_f2c_ftnlen_type_node);
      accessfield = ffecom_decl_field (ref, namelenfield, "access",
				       string_type_node);
      accesslenfield = ffecom_decl_field (ref, accessfield, "accesslen",
					  ffecom_f2c_ftnlen_type_node);
      sequentialfield = ffecom_decl_field (ref, accesslenfield, "sequential",
					   string_type_node);
      sequentiallenfield = ffecom_decl_field (ref, sequentialfield,
					      "sequentiallen",
					      ffecom_f2c_ftnlen_type_node);
      directfield = ffecom_decl_field (ref, sequentiallenfield, "direct",
				       string_type_node);
      directlenfield = ffecom_decl_field (ref, directfield, "directlen",
					  ffecom_f2c_ftnlen_type_node);
      formfield = ffecom_decl_field (ref, directlenfield, "form",
				     string_type_node);
      formlenfield = ffecom_decl_field (ref, formfield, "formlen",
					ffecom_f2c_ftnlen_type_node);
      formattedfield = ffecom_decl_field (ref, formlenfield, "formatted",
					  string_type_node);
      formattedlenfield = ffecom_decl_field (ref, formattedfield,
					     "formattedlen",
					     ffecom_f2c_ftnlen_type_node);
      unformattedfield = ffecom_decl_field (ref, formattedlenfield,
					    "unformatted",
					    string_type_node);
      unformattedlenfield = ffecom_decl_field (ref, unformattedfield,
					       "unformattedlen",
					       ffecom_f2c_ftnlen_type_node);
      reclfield = ffecom_decl_field (ref, unformattedlenfield, "recl",
				     ffecom_f2c_ptr_to_ftnint_type_node);
      nextrecfield = ffecom_decl_field (ref, reclfield, "nextrec",
					ffecom_f2c_ptr_to_ftnint_type_node);
      blankfield = ffecom_decl_field (ref, nextrecfield, "blank",
				      string_type_node);
      blanklenfield = ffecom_decl_field (ref, blankfield, "blanklen",
					 ffecom_f2c_ftnlen_type_node);

      TYPE_FIELDS (ref) = errfield;
      layout_type (ref);

      resume_temporary_allocation ();
      pop_obstacks ();

      f2c_inquire_struct = ref;
    }

  ffeste_f2c_flagspec_ (have_err, errinit);
  ffeste_f2c_intspec_ (unit_spec, unitexp, unitinit);
  ffeste_f2c_charspec_ (file_spec, fileexp, fileinit, filelenexp, fileleninit);
  ffeste_f2c_ptrtointspec_ (exist_spec, existexp, existinit);
  ffeste_f2c_ptrtointspec_ (open_spec, openexp, openinit);
  ffeste_f2c_ptrtointspec_ (number_spec, numberexp, numberinit);
  ffeste_f2c_ptrtointspec_ (named_spec, namedexp, namedinit);
  ffeste_f2c_charspec_ (name_spec, nameexp, nameinit, namelenexp, nameleninit);
  ffeste_f2c_charspec_ (access_spec, accessexp, accessinit, accesslenexp,
			accessleninit);
  ffeste_f2c_charspec_ (sequential_spec, sequentialexp, sequentialinit,
			sequentiallenexp, sequentialleninit);
  ffeste_f2c_charspec_ (direct_spec, directexp, directinit, directlenexp,
			directleninit);
  ffeste_f2c_charspec_ (form_spec, formexp, forminit, formlenexp, formleninit);
  ffeste_f2c_charspec_ (formatted_spec, formattedexp, formattedinit,
			formattedlenexp, formattedleninit);
  ffeste_f2c_charspec_ (unformatted_spec, unformattedexp, unformattedinit,
			unformattedlenexp, unformattedleninit);
  ffeste_f2c_ptrtointspec_ (recl_spec, reclexp, reclinit);
  ffeste_f2c_ptrtointspec_ (nextrec_spec, nextrecexp, nextrecinit);
  ffeste_f2c_charspec_ (blank_spec, blankexp, blankinit, blanklenexp,
			blankleninit);

  inits = build_tree_list ((field = TYPE_FIELDS (f2c_inquire_struct)),
			   errinit);
  initn = inits;
  ffeste_f2c_init_ (unitinit);
  ffeste_f2c_init_ (fileinit);
  ffeste_f2c_init_ (fileleninit);
  ffeste_f2c_init_ (existinit);
  ffeste_f2c_init_ (openinit);
  ffeste_f2c_init_ (numberinit);
  ffeste_f2c_init_ (namedinit);
  ffeste_f2c_init_ (nameinit);
  ffeste_f2c_init_ (nameleninit);
  ffeste_f2c_init_ (accessinit);
  ffeste_f2c_init_ (accessleninit);
  ffeste_f2c_init_ (sequentialinit);
  ffeste_f2c_init_ (sequentialleninit);
  ffeste_f2c_init_ (directinit);
  ffeste_f2c_init_ (directleninit);
  ffeste_f2c_init_ (forminit);
  ffeste_f2c_init_ (formleninit);
  ffeste_f2c_init_ (formattedinit);
  ffeste_f2c_init_ (formattedleninit);
  ffeste_f2c_init_ (unformattedinit);
  ffeste_f2c_init_ (unformattedleninit);
  ffeste_f2c_init_ (reclinit);
  ffeste_f2c_init_ (nextrecinit);
  ffeste_f2c_init_ (blankinit);
  ffeste_f2c_init_ (blankleninit);

  inits = build (CONSTRUCTOR, f2c_inquire_struct, NULL_TREE, inits);
  TREE_CONSTANT (inits) = constantp ? 1 : 0;
  TREE_STATIC (inits) = 1;

  yes = suspend_momentary ();

  t = build_decl (VAR_DECL,
		  ffecom_get_invented_identifier ("__g77_inlist_%d", NULL,
						  mynumber++),
		  f2c_inquire_struct);
  TREE_STATIC (t) = 1;
  t = ffecom_start_decl (t, 1);
  ffecom_finish_decl (t, inits, 0);

  resume_momentary (yes);

  ffeste_f2c_exp_ (unitfield, unitexp);
  ffeste_f2c_exp_ (filefield, fileexp);
  ffeste_f2c_exp_ (filelenfield, filelenexp);
  ffeste_f2c_exp_ (existfield, existexp);
  ffeste_f2c_exp_ (openfield, openexp);
  ffeste_f2c_exp_ (numberfield, numberexp);
  ffeste_f2c_exp_ (namedfield, namedexp);
  ffeste_f2c_exp_ (namefield, nameexp);
  ffeste_f2c_exp_ (namelenfield, namelenexp);
  ffeste_f2c_exp_ (accessfield, accessexp);
  ffeste_f2c_exp_ (accesslenfield, accesslenexp);
  ffeste_f2c_exp_ (sequentialfield, sequentialexp);
  ffeste_f2c_exp_ (sequentiallenfield, sequentiallenexp);
  ffeste_f2c_exp_ (directfield, directexp);
  ffeste_f2c_exp_ (directlenfield, directlenexp);
  ffeste_f2c_exp_ (formfield, formexp);
  ffeste_f2c_exp_ (formlenfield, formlenexp);
  ffeste_f2c_exp_ (formattedfield, formattedexp);
  ffeste_f2c_exp_ (formattedlenfield, formattedlenexp);
  ffeste_f2c_exp_ (unformattedfield, unformattedexp);
  ffeste_f2c_exp_ (unformattedlenfield, unformattedlenexp);
  ffeste_f2c_exp_ (reclfield, reclexp);
  ffeste_f2c_exp_ (nextrecfield, nextrecexp);
  ffeste_f2c_exp_ (blankfield, blankexp);
  ffeste_f2c_exp_ (blanklenfield, blanklenexp);

  ttype = build_pointer_type (TREE_TYPE (t));
  t = ffecom_1 (ADDR_EXPR, ttype, t);

  t = build_tree_list (NULL_TREE, t);

  return t;
}

#endif
/* ffeste_io_olist_ -- Make arglist with ptr to OPEN control list

   tree arglist;
   arglist = ffeste_io_olist_(...);

   Returns a tree suitable as an argument list containing a pointer to
   an OPEN-statement control list.  First, generates that control
   list, if necessary, along with any static and run-time initializations
   that are needed as specified by the arguments to this function.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static tree
ffeste_io_olist_ (bool have_err,
		  ffebld unit_expr,
		  ffestpFile *file_spec,
		  ffestpFile *stat_spec,
		  ffestpFile *access_spec,
		  ffestpFile *form_spec,
		  ffestpFile *recl_spec,
		  ffestpFile *blank_spec)
{
  static tree f2c_open_struct = NULL_TREE;
  tree t;
  tree ttype;
  int yes;
  tree field;
  tree inits, initn;
  tree ignore;			/* Ignore length info for certain fields. */
  bool constantp = TRUE;
  static tree errfield, unitfield, filefield, filelenfield, statfield,
    accessfield, formfield, reclfield, blankfield;
  tree errinit, unitinit, fileinit, fileleninit, statinit, accessinit,
    forminit, reclinit, blankinit;
  tree
    unitexp, fileexp, filelenexp, statexp, accessexp, formexp, reclexp,
    blankexp;
  static int mynumber = 0;

  if (f2c_open_struct == NULL_TREE)
    {
      tree ref;

      push_obstacks_nochange ();
      end_temporary_allocation ();

      ref = make_node (RECORD_TYPE);

      errfield = ffecom_decl_field (ref, NULL_TREE, "err",
				    ffecom_f2c_flag_type_node);
      unitfield = ffecom_decl_field (ref, errfield, "unit",
				     ffecom_f2c_ftnint_type_node);
      filefield = ffecom_decl_field (ref, unitfield, "file",
				     string_type_node);
      filelenfield = ffecom_decl_field (ref, filefield, "filelen",
					ffecom_f2c_ftnlen_type_node);
      statfield = ffecom_decl_field (ref, filelenfield, "stat",
				     string_type_node);
      accessfield = ffecom_decl_field (ref, statfield, "access",
				       string_type_node);
      formfield = ffecom_decl_field (ref, accessfield, "form",
				     string_type_node);
      reclfield = ffecom_decl_field (ref, formfield, "recl",
				     ffecom_f2c_ftnint_type_node);
      blankfield = ffecom_decl_field (ref, reclfield, "blank",
				      string_type_node);

      TYPE_FIELDS (ref) = errfield;
      layout_type (ref);

      resume_temporary_allocation ();
      pop_obstacks ();

      f2c_open_struct = ref;
    }

  ffeste_f2c_flagspec_ (have_err, errinit);

  unitexp = ffecom_expr (unit_expr);
  if (TREE_CONSTANT (unitexp))
    {
      unitinit = unitexp;
      unitexp = NULL_TREE;
    }
  else
    {
      unitinit = ffecom_integer_zero_node;
      constantp = FALSE;
    }

  ffeste_f2c_charspec_ (file_spec, fileexp, fileinit, filelenexp, fileleninit);
  ffeste_f2c_charnolenspec_ (stat_spec, statexp, statinit);
  ffeste_f2c_charnolenspec_ (access_spec, accessexp, accessinit);
  ffeste_f2c_charnolenspec_ (form_spec, formexp, forminit);
  ffeste_f2c_intspec_ (recl_spec, reclexp, reclinit);
  ffeste_f2c_charnolenspec_ (blank_spec, blankexp, blankinit);

  inits = build_tree_list ((field = TYPE_FIELDS (f2c_open_struct)), errinit);
  initn = inits;
  ffeste_f2c_init_ (unitinit);
  ffeste_f2c_init_ (fileinit);
  ffeste_f2c_init_ (fileleninit);
  ffeste_f2c_init_ (statinit);
  ffeste_f2c_init_ (accessinit);
  ffeste_f2c_init_ (forminit);
  ffeste_f2c_init_ (reclinit);
  ffeste_f2c_init_ (blankinit);

  inits = build (CONSTRUCTOR, f2c_open_struct, NULL_TREE, inits);
  TREE_CONSTANT (inits) = constantp ? 1 : 0;
  TREE_STATIC (inits) = 1;

  yes = suspend_momentary ();

  t = build_decl (VAR_DECL,
		  ffecom_get_invented_identifier ("__g77_olist_%d", NULL,
						  mynumber++),
		  f2c_open_struct);
  TREE_STATIC (t) = 1;
  t = ffecom_start_decl (t, 1);
  ffecom_finish_decl (t, inits, 0);

  resume_momentary (yes);

  ffeste_f2c_exp_ (unitfield, unitexp);
  ffeste_f2c_exp_ (filefield, fileexp);
  ffeste_f2c_exp_ (filelenfield, filelenexp);
  ffeste_f2c_exp_ (statfield, statexp);
  ffeste_f2c_exp_ (accessfield, accessexp);
  ffeste_f2c_exp_ (formfield, formexp);
  ffeste_f2c_exp_ (reclfield, reclexp);
  ffeste_f2c_exp_ (blankfield, blankexp);

  ttype = build_pointer_type (TREE_TYPE (t));
  t = ffecom_1 (ADDR_EXPR, ttype, t);

  t = build_tree_list (NULL_TREE, t);

  return t;
}

#endif
/* ffeste_subr_file_ -- Display file-statement specifier

   ffeste_subr_file_(&specifier);  */

#if FFECOM_targetCURRENT == FFECOM_targetFFE
static void
ffeste_subr_file_ (char *kw, ffestpFile *spec)
{
  if (!spec->kw_or_val_present)
    return;
  fputs (kw, dmpout);
  if (spec->value_present)
    {
      fputc ('=', dmpout);
      if (spec->value_is_label)
	{
	  assert (spec->value_is_label == 2);	/* Temporary checking only. */
	  fprintf (dmpout, "%" ffelabValue_f "u",
		   ffelab_value (spec->u.label));
	}
      else
	ffebld_dump (spec->u.expr);
    }
  fputc (',', dmpout);
}
#endif

/* ffeste_subr_beru_ -- Generate code for BACKSPACE/ENDFILE/REWIND

   ffeste_subr_beru_(FFECOM_gfrtFBACK);	 */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
static void
ffeste_subr_beru_ (ffestpBeruStmt *info, ffecomGfrt rt)
{
  tree alist;
  bool iostat;
  bool errl;

#define specified(something) (info->beru_spec[something].kw_or_val_present)

  ffeste_emit_line_note_ ();

  /* Do the real work. */

  iostat = specified (FFESTP_beruixIOSTAT);
  errl = specified (FFESTP_beruixERR);

  /* ~~For now, we assume the unit number is specified and is not ASTERISK,
     because the FFE doesn't support BACKSPACE(*) and rejects a BACKSPACE
     without any unit specifier.  f2c, however, supports the former
     construct.	 When it is time to add this feature to the FFE, which
     probably is fairly easy, ffestc_R919 and company will want to pass an
     ffestvUnit indicator of FFESTV_unitINTEXPR or _unitASTERISK to
     ffeste_R919 and company, and they will want to pass that same value to
     this function, and that argument will replace the constant _unitINTEXPR_
     in the call below.	 Right now, the default unit number, 6, is ignored. */

  ffecom_push_calltemps ();

  alist = ffeste_io_ialist_ (errl || iostat, FFESTV_unitINTEXPR,
			     info->beru_spec[FFESTP_beruixUNIT].u.expr, 6);

  if (errl)
    {				/* ERR= */
      ffeste_io_err_
	= ffeste_io_abort_
	= ffecom_lookup_label
	(info->beru_spec[FFESTP_beruixERR].u.label);
      ffeste_io_abort_is_temp_ = FALSE;
    }
  else
    {				/* no ERR= */
      ffeste_io_err_ = NULL_TREE;

      if ((ffeste_io_abort_is_temp_ = iostat))
	ffeste_io_abort_ = ffecom_temp_label ();
      else
	ffeste_io_abort_ = NULL_TREE;
    }

  if (iostat)
    {				/* IOSTAT= */
      ffeste_io_iostat_is_temp_ = FALSE;
      ffeste_io_iostat_ = ffecom_expr
	(info->beru_spec[FFESTP_beruixIOSTAT].u.expr);
    }
  else if (ffeste_io_abort_ != NULL_TREE)
    {				/* no IOSTAT= but ERR= */
      ffeste_io_iostat_is_temp_ = TRUE;
      ffeste_io_iostat_
	= ffecom_push_tempvar (ffecom_integer_type_node,
			       FFETARGET_charactersizeNONE, -1, FALSE);
    }
  else
    {				/* no IOSTAT=, or ERR= */
      ffeste_io_iostat_is_temp_ = FALSE;
      ffeste_io_iostat_ = NULL_TREE;
    }

  /* Don't generate "if (iostat != 0) goto label;" if label is temp abort
     label, since we're gonna fall through to there anyway. */

  ffeste_io_call_ (ffecom_call_gfrt (rt, alist),
		   !ffeste_io_abort_is_temp_);

  /* If we've got a temp label, generate its code here. */

  if (ffeste_io_abort_is_temp_)
    {
      DECL_INITIAL (ffeste_io_abort_) = error_mark_node;
      emit_nop ();
      expand_label (ffeste_io_abort_);

      assert (ffeste_io_err_ == NULL_TREE);
    }

  /* If we've got a temp iostat, pop the temp. */

  if (ffeste_io_iostat_is_temp_)
    ffecom_pop_tempvar (ffeste_io_iostat_);

  ffecom_pop_calltemps ();

#undef specified

  clear_momentary ();
}

#endif
/* ffeste_do -- End of statement following DO-term-stmt etc

   ffeste_do(TRUE);

   Also invoked by _labeldef_branch_finish_ (or, in cases
   of errors, other _labeldef_ functions) when the label definition is
   for a DO-target (LOOPEND) label, once per matching/outstanding DO
   block on the stack.	These cases invoke this function with ok==TRUE, so
   only forced stack popping (via ffeste_eof_()) invokes it with ok==FALSE.  */

void
ffeste_do (ffestw block)
{
#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fputs ("+ END_DO\n", dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  ffeste_emit_line_note_ ();
  if (ffestw_do_tvar (block) == 0)
    expand_end_loop ();		/* DO WHILE and just DO. */
  else
    ffeste_end_iterdo_ (ffestw_do_tvar (block),
			ffestw_do_incr_saved (block),
			ffestw_do_count_var (block));

  clear_momentary ();
#else
#error
#endif
}

/* ffeste_end_R807 -- End of statement following logical IF

   ffeste_end_R807(TRUE);

   Applies ONLY to logical IF, not to IF-THEN.	For example, does not
   ffelex_token_kill the construct name for an IF-THEN block (the name
   field is invalid for logical IF).  ok==TRUE iff statement following
   logical IF (substatement) is valid; else, statement is invalid or
   stack forcibly popped due to ffeste_eof_().	*/

void
ffeste_end_R807 ()
{
#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fputs ("+ END_IF\n", dmpout);	/* Also see ffeste_R806. */
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  ffeste_emit_line_note_ ();
  expand_end_cond ();
  clear_momentary ();
#else
#error
#endif
}

/* ffeste_labeldef_branch -- Generate "code" for branch label def

   ffeste_labeldef_branch(label);  */

void
ffeste_labeldef_branch (ffelab label)
{
#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fprintf (dmpout, "+ label %lu\n", ffelab_value (label));
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  {
    tree glabel;

    glabel = ffecom_lookup_label (label);
    assert (glabel != NULL_TREE);
    if (TREE_CODE (glabel) == ERROR_MARK)
      return;
    assert (DECL_INITIAL (glabel) == NULL_TREE);
    DECL_INITIAL (glabel) = error_mark_node;
    DECL_SOURCE_FILE (glabel) = ffelab_definition_filename (label);
    DECL_SOURCE_LINE (glabel) = ffelab_definition_filelinenum (label);
    emit_nop ();
    expand_label (glabel);
  }
#else
#error
#endif
}

/* ffeste_labeldef_format -- Generate "code" for FORMAT label def

   ffeste_labeldef_format(label);  */

void
ffeste_labeldef_format (ffelab label)
{
#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fprintf (dmpout, "$ label %lu\n", ffelab_value (label));
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  ffeste_label_formatdef_ = label;
#else
#error
#endif
}

/* ffeste_R737A -- Assignment statement outside of WHERE

   ffeste_R737A(dest_expr,source_expr);	 */

void
ffeste_R737A (ffebld dest, ffebld source)
{
  ffeste_check_simple_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fputs ("+ let ", dmpout);
  ffebld_dump (dest);
  fputs ("=", dmpout);
  ffebld_dump (source);
  fputc ('\n', dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  ffeste_emit_line_note_ ();
  ffecom_push_calltemps ();

  ffecom_expand_let_stmt (dest, source);

  ffecom_pop_calltemps ();
  clear_momentary ();
#else
#error
#endif
}

/* ffeste_R803 -- Block IF (IF-THEN) statement

   ffeste_R803(construct_name,expr,expr_token);

   Make sure statement is valid here; implement.  */

void
ffeste_R803 (ffebld expr)
{
  ffeste_check_simple_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fputs ("+ IF_block (", dmpout);
  ffebld_dump (expr);
  fputs (")\n", dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  ffeste_emit_line_note_ ();
  ffecom_push_calltemps ();

  expand_start_cond (ffecom_truth_value (ffecom_expr (expr)), 0);

  ffecom_pop_calltemps ();
  clear_momentary ();
#else
#error
#endif
}

/* ffeste_R804 -- ELSE IF statement

   ffeste_R804(expr,expr_token,name_token);

   Make sure ffeste_kind_ identifies an IF block.  If not
   NULL, make sure name_token gives the correct name.  Implement the else
   of the IF block.  */

void
ffeste_R804 (ffebld expr)
{
  ffeste_check_simple_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fputs ("+ ELSE_IF (", dmpout);
  ffebld_dump (expr);
  fputs (")\n", dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  ffeste_emit_line_note_ ();
  ffecom_push_calltemps ();

  expand_start_elseif (ffecom_truth_value (ffecom_expr (expr)));

  ffecom_pop_calltemps ();
  clear_momentary ();
#else
#error
#endif
}

/* ffeste_R805 -- ELSE statement

   ffeste_R805(name_token);

   Make sure ffeste_kind_ identifies an IF block.  If not
   NULL, make sure name_token gives the correct name.  Implement the ELSE
   of the IF block.  */

void
ffeste_R805 ()
{
  ffeste_check_simple_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fputs ("+ ELSE\n", dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  ffeste_emit_line_note_ ();
  expand_start_else ();
  clear_momentary ();
#else
#error
#endif
}

/* ffeste_R806 -- End an IF-THEN

   ffeste_R806(TRUE);  */

void
ffeste_R806 ()
{
#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fputs ("+ END_IF_then\n", dmpout);	/* Also see ffeste_shriek_if_. */
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  ffeste_emit_line_note_ ();
  expand_end_cond ();
  clear_momentary ();
#else
#error
#endif
}

/* ffeste_R807 -- Logical IF statement

   ffeste_R807(expr,expr_token);

   Make sure statement is valid here; implement.  */

void
ffeste_R807 (ffebld expr)
{
  ffeste_check_simple_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fputs ("+ IF_logical (", dmpout);
  ffebld_dump (expr);
  fputs (")\n", dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  ffeste_emit_line_note_ ();
  ffecom_push_calltemps ();

  expand_start_cond (ffecom_truth_value (ffecom_expr (expr)), 0);

  ffecom_pop_calltemps ();
  clear_momentary ();
#else
#error
#endif
}

/* ffeste_R809 -- SELECT CASE statement

   ffeste_R809(construct_name,expr,expr_token);

   Make sure statement is valid here; implement.  */

void
ffeste_R809 (ffestw block, ffebld expr)
{
  ffeste_check_simple_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fputs ("+ SELECT_CASE (", dmpout);
  ffebld_dump (expr);
  fputs (")\n", dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  ffecom_push_calltemps ();

  {
    tree texpr;

    ffeste_emit_line_note_ ();

    if ((expr == NULL)
	|| (ffeinfo_basictype (ffebld_info (expr))
	    == FFEINFO_basictypeANY))
      {
	ffestw_set_select_texpr (block, error_mark_node);
	clear_momentary ();
      }
    else
      {
	texpr = ffecom_expr (expr);
	if (ffeinfo_basictype (ffebld_info (expr))
	    != FFEINFO_basictypeCHARACTER)
	  {
	    expand_start_case (1, texpr, TREE_TYPE (texpr),
			       "SELECT CASE statement");
	    ffestw_set_select_texpr (block, texpr);
	    ffestw_set_select_break (block, FALSE);
	    push_momentary ();
	  }
	else
	  {
	    ffebad_start_msg ("SELECT CASE on CHARACTER type (at %0) not supported -- sorry",
			      FFEBAD_severityFATAL);
	    ffebad_here (0, ffestw_line (block), ffestw_col (block));
	    ffebad_finish ();
	    ffestw_set_select_texpr (block, error_mark_node);
	  }
      }
  }

  ffecom_pop_calltemps ();
#else
#error
#endif
}

/* ffeste_R810 -- CASE statement

   ffeste_R810(case_value_range_list,name);

   If casenum is 0, it's CASE DEFAULT.	Else it's the case ranges at
   the start of the first_stmt list in the select object at the top of
   the stack that match casenum.  */

void
ffeste_R810 (ffestw block, unsigned long casenum)
{
  ffestwSelect s = ffestw_select (block);
  ffestwCase c;

  ffeste_check_simple_ ();

  if (s->first_stmt == (ffestwCase) &s->first_rel)
    c = NULL;
  else
    c = s->first_stmt;

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  if ((c == NULL) || (casenum != c->casenum))
    {
      if (casenum == 0)		/* Intentional CASE DEFAULT. */
	fputs ("+ CASE_DEFAULT", dmpout);
    }
  else
    {
      bool comma = FALSE;

      fputs ("+ CASE (", dmpout);
      do
	{
	  if (comma)
	    fputc (',', dmpout);
	  else
	    comma = TRUE;
	  if (c->low != NULL)
	    ffebld_constant_dump (c->low);
	  if (c->low != c->high)
	    {
	      fputc (':', dmpout);
	      if (c->high != NULL)
		ffebld_constant_dump (c->high);
	    }
	  c = c->next_stmt;
	  /* Unlink prev.  */
	  c->previous_stmt->previous_stmt->next_stmt = c;
	  c->previous_stmt = c->previous_stmt->previous_stmt;
	}
      while ((c != (ffestwCase) &s->first_rel) && (casenum == c->casenum));
      fputc (')', dmpout);
    }

  fputc ('\n', dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  {
    tree texprlow;
    tree texprhigh;
    tree tlabel = build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);
    int pushok;
    tree duplicate;

    ffeste_emit_line_note_ ();

    if (TREE_CODE (ffestw_select_texpr (block)) == ERROR_MARK)
      {
	clear_momentary ();
	return;
      }

    if (ffestw_select_break (block))
      expand_exit_something ();
    else
      ffestw_set_select_break (block, TRUE);

    if ((c == NULL) || (casenum != c->casenum))
      {
	if (casenum == 0)	/* Intentional CASE DEFAULT. */
	  {
	    pushok = pushcase (NULL_TREE, 0, tlabel, &duplicate);
	    assert (pushok == 0);
	  }
      }
    else
      do
	{
	  texprlow = (c->low == NULL) ? NULL_TREE
	    : ffecom_constantunion (&ffebld_constant_union (c->low), s->type,
		       s->kindtype, ffecom_tree_type[s->type][s->kindtype]);
	  if (c->low != c->high)
	    {
	      texprhigh = (c->high == NULL) ? NULL_TREE
		: ffecom_constantunion (&ffebld_constant_union (c->high),
	      s->type, s->kindtype, ffecom_tree_type[s->type][s->kindtype]);
	      pushok = pushcase_range (texprlow, texprhigh, convert,
				       tlabel, &duplicate);
	    }
	  else
	    pushok = pushcase (texprlow, convert, tlabel, &duplicate);
	  assert (pushok == 0);
	  c = c->next_stmt;
	  /* Unlink prev.  */
	  c->previous_stmt->previous_stmt->next_stmt = c;
	  c->previous_stmt = c->previous_stmt->previous_stmt;
	}
      while ((c != (ffestwCase) &s->first_rel) && (casenum == c->casenum));

    clear_momentary ();
  }				/* ~~~handle character, character*1 */
#else
#error
#endif
}

/* ffeste_R811 -- End a SELECT

   ffeste_R811(TRUE);  */

void
ffeste_R811 (ffestw block)
{
#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fputs ("+ END_SELECT\n", dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  ffeste_emit_line_note_ ();

  if (TREE_CODE (ffestw_select_texpr (block)) == ERROR_MARK)
    {
      clear_momentary ();
      return;
    }

  expand_end_case (ffestw_select_texpr (block));
  pop_momentary ();
  clear_momentary ();		/* ~~~handle character and character*1 */
#else
#error
#endif
}

/* Iterative DO statement.  */

void
ffeste_R819A (ffestw block, ffelab label UNUSED, ffebld var,
	      ffebld start, ffelexToken start_token,
	      ffebld end, ffelexToken end_token,
	      ffebld incr, ffelexToken incr_token)
{
  ffeste_check_simple_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  if ((ffebld_op (incr) == FFEBLD_opCONTER)
      && (ffebld_constant_is_zero (ffebld_conter (incr))))
    {
      ffebad_start (FFEBAD_DO_STEP_ZERO);
      ffebad_here (0, ffelex_token_where_line (incr_token),
		   ffelex_token_where_column (incr_token));
      ffebad_string ("Iterative DO loop");
      ffebad_finish ();
      /* Don't bother replacing it with 1 yet.  */
    }

  if (label == NULL)
    fputs ("+ DO_iterative_nonlabeled (", dmpout);
  else
    fprintf (dmpout, "+ DO_iterative_labeled %lu (", ffelab_value (label));
  ffebld_dump (var);
  fputc ('=', dmpout);
  ffebld_dump (start);
  fputc (',', dmpout);
  ffebld_dump (end);
  fputc (',', dmpout);
  ffebld_dump (incr);
  fputs (")\n", dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  {
    ffeste_emit_line_note_ ();
    ffecom_push_calltemps ();

    /* Start the DO loop.  */

    ffeste_begin_iterdo_ (block, NULL, NULL, NULL,
			  var,
			  start, start_token,
			  end, end_token,
			  incr, incr_token,
			  "Iterative DO loop");

    ffecom_pop_calltemps ();
  }
#else
#error
#endif
}

/* ffeste_R819B -- DO WHILE statement

   ffeste_R819B(construct_name,label_token,expr,expr_token);

   Make sure statement is valid here; implement.  */

void
ffeste_R819B (ffestw block, ffelab label UNUSED, ffebld expr)
{
  ffeste_check_simple_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  if (label == NULL)
    fputs ("+ DO_WHILE_nonlabeled (", dmpout);
  else
    fprintf (dmpout, "+ DO_WHILE_labeled %lu (", ffelab_value (label));
  ffebld_dump (expr);
  fputs (")\n", dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  {
    ffeste_emit_line_note_ ();
    ffecom_push_calltemps ();

    ffestw_set_do_hook (block, expand_start_loop (1));
    ffestw_set_do_tvar (block, 0);	/* Means DO WHILE vs. iter DO. */
    if (expr != NULL)
      expand_exit_loop_if_false (0, ffecom_truth_value (ffecom_expr (expr)));

    ffecom_pop_calltemps ();
    clear_momentary ();
  }
#else
#error
#endif
}

/* ffeste_R825 -- END DO statement

   ffeste_R825(name_token);

   Make sure ffeste_kind_ identifies a DO block.  If not
   NULL, make sure name_token gives the correct name.  Do whatever
   is specific to seeing END DO with a DO-target label definition on it,
   where the END DO is really treated as a CONTINUE (i.e. generate th
   same code you would for CONTINUE).  ffeste_do handles the actual
   generation of end-loop code.	 */

void
ffeste_R825 ()
{
  ffeste_check_simple_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fputs ("+ END_DO_sugar\n", dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  ffeste_emit_line_note_ ();
  emit_nop ();
#else
#error
#endif
}

/* ffeste_R834 -- CYCLE statement

   ffeste_R834(name_token);

   Handle a CYCLE within a loop.  */

void
ffeste_R834 (ffestw block)
{
  ffeste_check_simple_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fprintf (dmpout, "+ CYCLE block #%lu\n", ffestw_blocknum (block));
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  ffeste_emit_line_note_ ();
  expand_continue_loop (ffestw_do_hook (block));
  clear_momentary ();
#else
#error
#endif
}

/* ffeste_R835 -- EXIT statement

   ffeste_R835(name_token);

   Handle a EXIT within a loop.	 */

void
ffeste_R835 (ffestw block)
{
  ffeste_check_simple_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fprintf (dmpout, "+ EXIT block #%lu\n", ffestw_blocknum (block));
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  ffeste_emit_line_note_ ();
  expand_exit_loop (ffestw_do_hook (block));
  clear_momentary ();
#else
#error
#endif
}

/* ffeste_R836 -- GOTO statement

   ffeste_R836(label);

   Make sure label_token identifies a valid label for a GOTO.  Update
   that label's info to indicate it is the target of a GOTO.  */

void
ffeste_R836 (ffelab label)
{
  ffeste_check_simple_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fprintf (dmpout, "+ GOTO %lu\n", ffelab_value (label));
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  {
    tree glabel;

    ffeste_emit_line_note_ ();
    glabel = ffecom_lookup_label (label);
    if ((glabel != NULL_TREE)
	&& (TREE_CODE (glabel) != ERROR_MARK))
      {
	TREE_USED (glabel) = 1;
	expand_goto (glabel);
	clear_momentary ();
      }
  }
#else
#error
#endif
}

/* ffeste_R837 -- Computed GOTO statement

   ffeste_R837(labels,count,expr);

   Make sure label_list identifies valid labels for a GOTO.  Update
   each label's info to indicate it is the target of a GOTO.  */

void
ffeste_R837 (ffelab *labels, int count, ffebld expr)
{
  int i;

  ffeste_check_simple_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fputs ("+ CGOTO (", dmpout);
  for (i = 0; i < count; ++i)
    {
      if (i != 0)
	fputc (',', dmpout);
      fprintf (dmpout, "%" ffelabValue_f "u", ffelab_value (labels[i]));
    }
  fputs ("),", dmpout);
  ffebld_dump (expr);
  fputc ('\n', dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  {
    tree texpr;
    tree value;
    tree tlabel;
    int pushok;
    tree duplicate;

    ffeste_emit_line_note_ ();
    ffecom_push_calltemps ();

    texpr = ffecom_expr (expr);
    expand_start_case (0, texpr, TREE_TYPE (texpr), "computed GOTO statement");
    push_momentary ();		/* In case of lots of labels, keep clearing
				   them out. */
    for (i = 0; i < count; ++i)
      {
	value = build_int_2 (i + 1, 0);
	tlabel = build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);

	pushok = pushcase (value, convert, tlabel, &duplicate);
	assert (pushok == 0);
	tlabel = ffecom_lookup_label (labels[i]);
	if ((tlabel == NULL_TREE)
	    || (TREE_CODE (tlabel) == ERROR_MARK))
	  continue;
	TREE_USED (tlabel) = 1;
	expand_goto (tlabel);
	clear_momentary ();
      }
    pop_momentary ();
    expand_end_case (texpr);

    ffecom_pop_calltemps ();
    clear_momentary ();
  }
#else
#error
#endif
}

/* ffeste_R838 -- ASSIGN statement

   ffeste_R838(label_token,target_variable,target_token);

   Make sure label_token identifies a valid label for an assignment.  Update
   that label's info to indicate it is the source of an assignment.  Update
   target_variable's info to indicate it is the target the assignment of that
   label.  */

void
ffeste_R838 (ffelab label, ffebld target)
{
  ffeste_check_simple_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fprintf (dmpout, "+ ASSIGN %lu TO ", ffelab_value (label));
  ffebld_dump (target);
  fputc ('\n', dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  {
    tree expr_tree;
    tree label_tree;
    tree target_tree;

    ffeste_emit_line_note_ ();
    ffecom_push_calltemps ();

    label_tree = ffecom_lookup_label (label);
    if ((label_tree != NULL_TREE)
	&& (TREE_CODE (label_tree) != ERROR_MARK))
      {
	label_tree = ffecom_1 (ADDR_EXPR,
			       build_pointer_type (void_type_node),
			       label_tree);
	TREE_CONSTANT (label_tree) = 1;
	target_tree = ffecom_expr_assign_w (target);
	if (GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (target_tree)))
	    < GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (label_tree))))
	  error ("ASSIGN to variable that is too small");
	label_tree = convert (TREE_TYPE (target_tree), label_tree);
	expr_tree = ffecom_modify (void_type_node,
				   target_tree,
				   label_tree);
	expand_expr_stmt (expr_tree);
	clear_momentary ();
      }

    ffecom_pop_calltemps ();
  }
#else
#error
#endif
}

/* ffeste_R839 -- Assigned GOTO statement

   ffeste_R839(target,target_token,label_list);

   Make sure label_list identifies valid labels for a GOTO.  Update
   each label's info to indicate it is the target of a GOTO.  */

void
ffeste_R839 (ffebld target)
{
  ffeste_check_simple_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fputs ("+ AGOTO ", dmpout);
  ffebld_dump (target);
  fputc ('\n', dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  {
    tree t;

    ffeste_emit_line_note_ ();
    ffecom_push_calltemps ();

    t = ffecom_expr_assign (target);
    if (GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (t)))
	< GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (null_pointer_node))))
      error ("ASSIGNed GOTO target variable is too small");
    expand_computed_goto (convert (TREE_TYPE (null_pointer_node), t));

    ffecom_pop_calltemps ();
    clear_momentary ();
  }
#else
#error
#endif
}

/* ffeste_R840 -- Arithmetic IF statement

   ffeste_R840(expr,expr_token,neg,zero,pos);

   Make sure the labels are valid; implement.  */

void
ffeste_R840 (ffebld expr, ffelab neg, ffelab zero, ffelab pos)
{
  ffeste_check_simple_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fputs ("+ IF_arithmetic (", dmpout);
  ffebld_dump (expr);
  fprintf (dmpout, ") %" ffelabValue_f "u,%" ffelabValue_f "u,%" ffelabValue_f "u\n",
	   ffelab_value (neg), ffelab_value (zero), ffelab_value (pos));
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  {
    tree gneg = ffecom_lookup_label (neg);
    tree gzero = ffecom_lookup_label (zero);
    tree gpos = ffecom_lookup_label (pos);
    tree texpr;

    if ((gneg == NULL_TREE) || (gzero == NULL_TREE) || (gpos == NULL_TREE))
      return;
    if ((TREE_CODE (gneg) == ERROR_MARK)
	|| (TREE_CODE (gzero) == ERROR_MARK)
	|| (TREE_CODE (gpos) == ERROR_MARK))
      return;

    ffecom_push_calltemps ();

    if (neg == zero)
      if (neg == pos)
	expand_goto (gzero);
      else
	{			/* IF (expr.LE.0) THEN GOTO neg/zero ELSE
				   GOTO pos. */
	  texpr = ffecom_expr (expr);
	  texpr = ffecom_2 (LE_EXPR, integer_type_node,
			    texpr,
			    convert (TREE_TYPE (texpr),
				     integer_zero_node));
	  expand_start_cond (ffecom_truth_value (texpr), 0);
	  expand_goto (gzero);
	  expand_start_else ();
	  expand_goto (gpos);
	  expand_end_cond ();
	}
    else if (neg == pos)
      {				/* IF (expr.NE.0) THEN GOTO neg/pos ELSE GOTO
				   zero. */
	texpr = ffecom_expr (expr);
	texpr = ffecom_2 (NE_EXPR, integer_type_node,
			  texpr,
			  convert (TREE_TYPE (texpr),
				   integer_zero_node));
	expand_start_cond (ffecom_truth_value (texpr), 0);
	expand_goto (gneg);
	expand_start_else ();
	expand_goto (gzero);
	expand_end_cond ();
      }
    else if (zero == pos)
      {				/* IF (expr.GE.0) THEN GOTO zero/pos ELSE
				   GOTO neg. */
	texpr = ffecom_expr (expr);
	texpr = ffecom_2 (GE_EXPR, integer_type_node,
			  texpr,
			  convert (TREE_TYPE (texpr),
				   integer_zero_node));
	expand_start_cond (ffecom_truth_value (texpr), 0);
	expand_goto (gzero);
	expand_start_else ();
	expand_goto (gneg);
	expand_end_cond ();
      }
    else
      {				/* Use a SAVE_EXPR in combo with:
				   IF (expr.LT.0) THEN GOTO neg
				   ELSEIF (expr.GT.0) THEN GOTO pos
				   ELSE GOTO zero. */
	tree expr_saved = ffecom_save_tree (ffecom_expr (expr));

	texpr = ffecom_2 (LT_EXPR, integer_type_node,
			  expr_saved,
			  convert (TREE_TYPE (expr_saved),
				   integer_zero_node));
	expand_start_cond (ffecom_truth_value (texpr), 0);
	expand_goto (gneg);
	texpr = ffecom_2 (GT_EXPR, integer_type_node,
			  expr_saved,
			  convert (TREE_TYPE (expr_saved),
				   integer_zero_node));
	expand_start_elseif (ffecom_truth_value (texpr));
	expand_goto (gpos);
	expand_start_else ();
	expand_goto (gzero);
	expand_end_cond ();
      }
    ffeste_emit_line_note_ ();

    ffecom_pop_calltemps ();
    clear_momentary ();
  }
#else
#error
#endif
}

/* ffeste_R841 -- CONTINUE statement

   ffeste_R841();  */

void
ffeste_R841 ()
{
  ffeste_check_simple_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fputs ("+ CONTINUE\n", dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  ffeste_emit_line_note_ ();
  emit_nop ();
#else
#error
#endif
}

/* ffeste_R842 -- STOP statement

   ffeste_R842(expr);  */

void
ffeste_R842 (ffebld expr)
{
  ffeste_check_simple_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  if (expr == NULL)
    {
      fputs ("+ STOP\n", dmpout);
    }
  else
    {
      fputs ("+ STOP_coded ", dmpout);
      ffebld_dump (expr);
      fputc ('\n', dmpout);
    }
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  {
    tree callit;
    ffelexToken msg;

    ffeste_emit_line_note_ ();
    if ((expr == NULL)
	|| (ffeinfo_basictype (ffebld_info (expr))
	    == FFEINFO_basictypeANY))
      {
	msg = ffelex_token_new_character ("", ffelex_token_where_line
			       (ffesta_tokens[0]), ffelex_token_where_column
					  (ffesta_tokens[0]));
	expr = ffebld_new_conter (ffebld_constant_new_characterdefault
				  (msg));
	ffelex_token_kill (msg);
	ffebld_set_info (expr, ffeinfo_new (FFEINFO_basictypeCHARACTER,
		    FFEINFO_kindtypeCHARACTERDEFAULT, 0, FFEINFO_kindENTITY,
					    FFEINFO_whereCONSTANT, 0));
      }
    else if (ffeinfo_basictype (ffebld_info (expr))
	     == FFEINFO_basictypeINTEGER)
      {
	char num[50];

	assert (ffebld_op (expr) == FFEBLD_opCONTER);
	assert (ffeinfo_kindtype (ffebld_info (expr))
		== FFEINFO_kindtypeINTEGERDEFAULT);
	sprintf (num, "%" ffetargetIntegerDefault_f "d",
		 ffebld_constant_integer1 (ffebld_conter (expr)));
	msg = ffelex_token_new_character (num, ffelex_token_where_line
			       (ffesta_tokens[0]), ffelex_token_where_column
					  (ffesta_tokens[0]));
	expr = ffebld_new_conter (ffebld_constant_new_characterdefault
				  (msg));
	ffelex_token_kill (msg);
	ffebld_set_info (expr, ffeinfo_new (FFEINFO_basictypeCHARACTER,
		    FFEINFO_kindtypeCHARACTERDEFAULT, 0, FFEINFO_kindENTITY,
					    FFEINFO_whereCONSTANT, 0));
      }
    else
      {
	assert (ffeinfo_basictype (ffebld_info (expr))
		== FFEINFO_basictypeCHARACTER);
	assert (ffebld_op (expr) == FFEBLD_opCONTER);
	assert (ffeinfo_kindtype (ffebld_info (expr))
		== FFEINFO_kindtypeCHARACTERDEFAULT);
      }

    ffecom_push_calltemps ();
    callit = ffecom_call_gfrt (FFECOM_gfrtSTOP,
		    ffecom_list_ptr_to_expr (ffebld_new_item (expr, NULL)));
    ffecom_pop_calltemps ();
    TREE_SIDE_EFFECTS (callit) = 1;
    expand_expr_stmt (callit);
    clear_momentary ();
  }
#else
#error
#endif
}

/* ffeste_R843 -- PAUSE statement

   ffeste_R843(expr,expr_token);

   Make sure statement is valid here; implement.  expr and expr_token are
   both NULL if there was no expression.  */

void
ffeste_R843 (ffebld expr)
{
  ffeste_check_simple_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  if (expr == NULL)
    {
      fputs ("+ PAUSE\n", dmpout);
    }
  else
    {
      fputs ("+ PAUSE_coded ", dmpout);
      ffebld_dump (expr);
      fputc ('\n', dmpout);
    }
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  {
    tree callit;
    ffelexToken msg;

    ffeste_emit_line_note_ ();
    if ((expr == NULL)
	|| (ffeinfo_basictype (ffebld_info (expr))
	    == FFEINFO_basictypeANY))
      {
	msg = ffelex_token_new_character ("", ffelex_token_where_line
			       (ffesta_tokens[0]), ffelex_token_where_column
					  (ffesta_tokens[0]));
	expr = ffebld_new_conter (ffebld_constant_new_characterdefault
				  (msg));
	ffelex_token_kill (msg);
	ffebld_set_info (expr, ffeinfo_new (FFEINFO_basictypeCHARACTER,
		    FFEINFO_kindtypeCHARACTERDEFAULT, 0, FFEINFO_kindENTITY,
					    FFEINFO_whereCONSTANT, 0));
      }
    else if (ffeinfo_basictype (ffebld_info (expr))
	     == FFEINFO_basictypeINTEGER)
      {
	char num[50];

	assert (ffebld_op (expr) == FFEBLD_opCONTER);
	assert (ffeinfo_kindtype (ffebld_info (expr))
		== FFEINFO_kindtypeINTEGERDEFAULT);
	sprintf (num, "%" ffetargetIntegerDefault_f "d",
		 ffebld_constant_integer1 (ffebld_conter (expr)));
	msg = ffelex_token_new_character (num, ffelex_token_where_line
			       (ffesta_tokens[0]), ffelex_token_where_column
					  (ffesta_tokens[0]));
	expr = ffebld_new_conter (ffebld_constant_new_characterdefault
				  (msg));
	ffelex_token_kill (msg);
	ffebld_set_info (expr, ffeinfo_new (FFEINFO_basictypeCHARACTER,
		    FFEINFO_kindtypeCHARACTERDEFAULT, 0, FFEINFO_kindENTITY,
					    FFEINFO_whereCONSTANT, 0));
      }
    else
      {
	assert (ffeinfo_basictype (ffebld_info (expr))
		== FFEINFO_basictypeCHARACTER);
	assert (ffebld_op (expr) == FFEBLD_opCONTER);
	assert (ffeinfo_kindtype (ffebld_info (expr))
		== FFEINFO_kindtypeCHARACTERDEFAULT);
      }

    ffecom_push_calltemps ();
    callit = ffecom_call_gfrt (FFECOM_gfrtPAUSE,
		    ffecom_list_ptr_to_expr (ffebld_new_item (expr, NULL)));
    ffecom_pop_calltemps ();
    TREE_SIDE_EFFECTS (callit) = 1;
    expand_expr_stmt (callit);
    clear_momentary ();
  }
#if 0				/* Old approach for phantom g77 run-time
				   library. */
  {
    tree callit;

    ffeste_emit_line_note_ ();
    if (expr == NULL)
      callit = ffecom_call_gfrt (FFECOM_gfrtPAUSENIL, NULL_TREE);
    else if (ffeinfo_basictype (ffebld_info (expr))
	     == FFEINFO_basictypeINTEGER)
      {
	ffecom_push_calltemps ();
	callit = ffecom_call_gfrt (FFECOM_gfrtPAUSEINT,
		    ffecom_list_ptr_to_expr (ffebld_new_item (expr, NULL)));
	ffecom_pop_calltemps ();
      }
    else
      {
	if (ffeinfo_basictype (ffebld_info (expr))
	    != FFEINFO_basictypeCHARACTER)
	  break;
	ffecom_push_calltemps ();
	callit = ffecom_call_gfrt (FFECOM_gfrtPAUSECHAR,
		    ffecom_list_ptr_to_expr (ffebld_new_item (expr, NULL)));
	ffecom_pop_calltemps ();
      }
    TREE_SIDE_EFFECTS (callit) = 1;
    expand_expr_stmt (callit);
    clear_momentary ();
  }
#endif
#else
#error
#endif
}

/* ffeste_R904 -- OPEN statement

   ffeste_R904();

   Make sure an OPEN is valid in the current context, and implement it.	 */

void
ffeste_R904 (ffestpOpenStmt *info)
{
  ffeste_check_simple_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fputs ("+ OPEN (", dmpout);
  ffeste_subr_file_ ("UNIT", &info->open_spec[FFESTP_openixUNIT]);
  ffeste_subr_file_ ("ACCESS", &info->open_spec[FFESTP_openixACCESS]);
  ffeste_subr_file_ ("ACTION", &info->open_spec[FFESTP_openixACTION]);
  ffeste_subr_file_ ("ASSOCIATEVARIABLE", &info->open_spec[FFESTP_openixASSOCIATEVARIABLE]);
  ffeste_subr_file_ ("BLANK", &info->open_spec[FFESTP_openixBLANK]);
  ffeste_subr_file_ ("BLOCKSIZE", &info->open_spec[FFESTP_openixBLOCKSIZE]);
  ffeste_subr_file_ ("BUFFERCOUNT", &info->open_spec[FFESTP_openixBUFFERCOUNT]);
  ffeste_subr_file_ ("CARRIAGECONTROL", &info->open_spec[FFESTP_openixCARRIAGECONTROL]);
  ffeste_subr_file_ ("DEFAULTFILE", &info->open_spec[FFESTP_openixDEFAULTFILE]);
  ffeste_subr_file_ ("DELIM", &info->open_spec[FFESTP_openixDELIM]);
  ffeste_subr_file_ ("DISPOSE", &info->open_spec[FFESTP_openixDISPOSE]);
  ffeste_subr_file_ ("ERR", &info->open_spec[FFESTP_openixERR]);
  ffeste_subr_file_ ("EXTENDSIZE", &info->open_spec[FFESTP_openixEXTENDSIZE]);
  ffeste_subr_file_ ("FILE", &info->open_spec[FFESTP_openixFILE]);
  ffeste_subr_file_ ("FORM", &info->open_spec[FFESTP_openixFORM]);
  ffeste_subr_file_ ("INITIALSIZE", &info->open_spec[FFESTP_openixINITIALSIZE]);
  ffeste_subr_file_ ("IOSTAT", &info->open_spec[FFESTP_openixIOSTAT]);
  ffeste_subr_file_ ("KEY", &info->open_spec[FFESTP_openixKEY]);
  ffeste_subr_file_ ("MAXREC", &info->open_spec[FFESTP_openixMAXREC]);
  ffeste_subr_file_ ("NOSPANBLOCKS", &info->open_spec[FFESTP_openixNOSPANBLOCKS]);
  ffeste_subr_file_ ("ORGANIZATION", &info->open_spec[FFESTP_openixORGANIZATION]);
  ffeste_subr_file_ ("PAD", &info->open_spec[FFESTP_openixPAD]);
  ffeste_subr_file_ ("POSITION", &info->open_spec[FFESTP_openixPOSITION]);
  ffeste_subr_file_ ("READONLY", &info->open_spec[FFESTP_openixREADONLY]);
  ffeste_subr_file_ ("RECL", &info->open_spec[FFESTP_openixRECL]);
  ffeste_subr_file_ ("RECORDTYPE", &info->open_spec[FFESTP_openixRECORDTYPE]);
  ffeste_subr_file_ ("SHARED", &info->open_spec[FFESTP_openixSHARED]);
  ffeste_subr_file_ ("STATUS", &info->open_spec[FFESTP_openixSTATUS]);
  ffeste_subr_file_ ("USEROPEN", &info->open_spec[FFESTP_openixUSEROPEN]);
  fputs (")\n", dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  {
    tree args;
    bool iostat;
    bool errl;

#define specified(something) (info->open_spec[something].kw_or_val_present)

    ffeste_emit_line_note_ ();

    iostat = specified (FFESTP_openixIOSTAT);
    errl = specified (FFESTP_openixERR);

    ffecom_push_calltemps ();

    args = ffeste_io_olist_ (errl || iostat,
			     info->open_spec[FFESTP_openixUNIT].u.expr,
			     &info->open_spec[FFESTP_openixFILE],
			     &info->open_spec[FFESTP_openixSTATUS],
			     &info->open_spec[FFESTP_openixACCESS],
			     &info->open_spec[FFESTP_openixFORM],
			     &info->open_spec[FFESTP_openixRECL],
			     &info->open_spec[FFESTP_openixBLANK]);

    if (errl)
      {
	ffeste_io_err_
	  = ffeste_io_abort_
	  = ffecom_lookup_label
	  (info->open_spec[FFESTP_openixERR].u.label);
	ffeste_io_abort_is_temp_ = FALSE;
      }
    else
      {
	ffeste_io_err_ = NULL_TREE;

	if ((ffeste_io_abort_is_temp_ = iostat))
	  ffeste_io_abort_ = ffecom_temp_label ();
	else
	  ffeste_io_abort_ = NULL_TREE;
      }

    if (iostat)
      {				/* IOSTAT= */
	ffeste_io_iostat_is_temp_ = FALSE;
	ffeste_io_iostat_ = ffecom_expr
	  (info->open_spec[FFESTP_openixIOSTAT].u.expr);
      }
    else if (ffeste_io_abort_ != NULL_TREE)
      {				/* no IOSTAT= but ERR= */
	ffeste_io_iostat_is_temp_ = TRUE;
	ffeste_io_iostat_
	  = ffecom_push_tempvar (ffecom_integer_type_node,
				 FFETARGET_charactersizeNONE, -1, FALSE);
      }
    else
      {				/* no IOSTAT=, or ERR= */
	ffeste_io_iostat_is_temp_ = FALSE;
	ffeste_io_iostat_ = NULL_TREE;
      }

    /* Don't generate "if (iostat != 0) goto label;" if label is temp abort
       label, since we're gonna fall through to there anyway. */

    ffeste_io_call_ (ffecom_call_gfrt (FFECOM_gfrtFOPEN, args),
		     !ffeste_io_abort_is_temp_);

    /* If we've got a temp label, generate its code here. */

    if (ffeste_io_abort_is_temp_)
      {
	DECL_INITIAL (ffeste_io_abort_) = error_mark_node;
	emit_nop ();
	expand_label (ffeste_io_abort_);

	assert (ffeste_io_err_ == NULL_TREE);
      }

    /* If we've got a temp iostat, pop the temp. */

    if (ffeste_io_iostat_is_temp_)
      ffecom_pop_tempvar (ffeste_io_iostat_);

    ffecom_pop_calltemps ();

#undef specified
  }

  clear_momentary ();
#else
#error
#endif
}

/* ffeste_R907 -- CLOSE statement

   ffeste_R907();

   Make sure a CLOSE is valid in the current context, and implement it.	 */

void
ffeste_R907 (ffestpCloseStmt *info)
{
  ffeste_check_simple_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fputs ("+ CLOSE (", dmpout);
  ffeste_subr_file_ ("UNIT", &info->close_spec[FFESTP_closeixUNIT]);
  ffeste_subr_file_ ("ERR", &info->close_spec[FFESTP_closeixERR]);
  ffeste_subr_file_ ("IOSTAT", &info->close_spec[FFESTP_closeixIOSTAT]);
  ffeste_subr_file_ ("STATUS", &info->close_spec[FFESTP_closeixSTATUS]);
  fputs (")\n", dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  {
    tree args;
    bool iostat;
    bool errl;

#define specified(something) (info->close_spec[something].kw_or_val_present)

    ffeste_emit_line_note_ ();

    iostat = specified (FFESTP_closeixIOSTAT);
    errl = specified (FFESTP_closeixERR);

    ffecom_push_calltemps ();

    args = ffeste_io_cllist_ (errl || iostat,
			      info->close_spec[FFESTP_closeixUNIT].u.expr,
			      &info->close_spec[FFESTP_closeixSTATUS]);

    if (errl)
      {
	ffeste_io_err_
	  = ffeste_io_abort_
	  = ffecom_lookup_label
	  (info->close_spec[FFESTP_closeixERR].u.label);
	ffeste_io_abort_is_temp_ = FALSE;
      }
    else
      {
	ffeste_io_err_ = NULL_TREE;

	if ((ffeste_io_abort_is_temp_ = iostat))
	  ffeste_io_abort_ = ffecom_temp_label ();
	else
	  ffeste_io_abort_ = NULL_TREE;
      }

    if (iostat)
      {				/* IOSTAT= */
	ffeste_io_iostat_is_temp_ = FALSE;
	ffeste_io_iostat_ = ffecom_expr
	  (info->close_spec[FFESTP_closeixIOSTAT].u.expr);
      }
    else if (ffeste_io_abort_ != NULL_TREE)
      {				/* no IOSTAT= but ERR= */
	ffeste_io_iostat_is_temp_ = TRUE;
	ffeste_io_iostat_
	  = ffecom_push_tempvar (ffecom_integer_type_node,
				 FFETARGET_charactersizeNONE, -1, FALSE);
      }
    else
      {				/* no IOSTAT=, or ERR= */
	ffeste_io_iostat_is_temp_ = FALSE;
	ffeste_io_iostat_ = NULL_TREE;
      }

    /* Don't generate "if (iostat != 0) goto label;" if label is temp abort
       label, since we're gonna fall through to there anyway. */

    ffeste_io_call_ (ffecom_call_gfrt (FFECOM_gfrtFCLOS, args),
		     !ffeste_io_abort_is_temp_);

    /* If we've got a temp label, generate its code here. */

    if (ffeste_io_abort_is_temp_)
      {
	DECL_INITIAL (ffeste_io_abort_) = error_mark_node;
	emit_nop ();
	expand_label (ffeste_io_abort_);

	assert (ffeste_io_err_ == NULL_TREE);
      }

    /* If we've got a temp iostat, pop the temp. */

    if (ffeste_io_iostat_is_temp_)
      ffecom_pop_tempvar (ffeste_io_iostat_);

    ffecom_pop_calltemps ();

#undef specified
  }

  clear_momentary ();
#else
#error
#endif
}

/* ffeste_R909_start -- READ(...) statement list begin

   ffeste_R909_start(FALSE);

   Verify that READ is valid here, and begin accepting items in the
   list.  */

void
ffeste_R909_start (ffestpReadStmt *info, bool only_format UNUSED,
		   ffestvUnit unit, ffestvFormat format, bool rec,
		   bool key UNUSED)
{
  ffeste_check_start_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  switch (format)
    {
    case FFESTV_formatNONE:
      if (rec)
	fputs ("+ READ_ufdac", dmpout);
      else if (key)
	fputs ("+ READ_ufidx", dmpout);
      else
	fputs ("+ READ_ufseq", dmpout);
      break;

    case FFESTV_formatLABEL:
    case FFESTV_formatCHAREXPR:
    case FFESTV_formatINTEXPR:
      if (rec)
	fputs ("+ READ_fmdac", dmpout);
      else if (key)
	fputs ("+ READ_fmidx", dmpout);
      else if (unit == FFESTV_unitCHAREXPR)
	fputs ("+ READ_fmint", dmpout);
      else
	fputs ("+ READ_fmseq", dmpout);
      break;

    case FFESTV_formatASTERISK:
      if (unit == FFESTV_unitCHAREXPR)
	fputs ("+ READ_lsint", dmpout);
      else
	fputs ("+ READ_lsseq", dmpout);
      break;

    case FFESTV_formatNAMELIST:
      fputs ("+ READ_nlseq", dmpout);
      break;

    default:
      assert ("Unexpected kind of format item in R909 READ" == NULL);
    }

  if (only_format)
    {
      fputc (' ', dmpout);
      ffeste_subr_file_ ("FORMAT", &info->read_spec[FFESTP_readixFORMAT]);
      fputc (' ', dmpout);

      return;
    }

  fputs (" (", dmpout);
  ffeste_subr_file_ ("UNIT", &info->read_spec[FFESTP_readixUNIT]);
  ffeste_subr_file_ ("FORMAT", &info->read_spec[FFESTP_readixFORMAT]);
  ffeste_subr_file_ ("ADVANCE", &info->read_spec[FFESTP_readixADVANCE]);
  ffeste_subr_file_ ("EOR", &info->read_spec[FFESTP_readixEOR]);
  ffeste_subr_file_ ("ERR", &info->read_spec[FFESTP_readixERR]);
  ffeste_subr_file_ ("END", &info->read_spec[FFESTP_readixEND]);
  ffeste_subr_file_ ("IOSTAT", &info->read_spec[FFESTP_readixIOSTAT]);
  ffeste_subr_file_ ("KEYEQ", &info->read_spec[FFESTP_readixKEYEQ]);
  ffeste_subr_file_ ("KEYGE", &info->read_spec[FFESTP_readixKEYGE]);
  ffeste_subr_file_ ("KEYGT", &info->read_spec[FFESTP_readixKEYGT]);
  ffeste_subr_file_ ("KEYID", &info->read_spec[FFESTP_readixKEYID]);
  ffeste_subr_file_ ("NULLS", &info->read_spec[FFESTP_readixNULLS]);
  ffeste_subr_file_ ("REC", &info->read_spec[FFESTP_readixREC]);
  ffeste_subr_file_ ("SIZE", &info->read_spec[FFESTP_readixSIZE]);
  fputs (") ", dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC

#define specified(something) (info->read_spec[something].kw_or_val_present)

  ffeste_emit_line_note_ ();

  /* Do the real work. */

  {
    ffecomGfrt start;
    ffecomGfrt end;
    tree cilist;
    bool iostat;
    bool errl;
    bool endl;

    /* First determine the start, per-item, and end run-time functions to
       call.  The per-item function is picked by choosing an ffeste functio
       to call to handle a given item; it knows how to generate a call to the
       appropriate run-time function, and is called an "io driver".  It
       handles the implied-DO construct, for example. */

    switch (format)
      {
      case FFESTV_formatNONE:	/* no FMT= */
	ffeste_io_driver_ = ffeste_io_douio_;
	if (rec)
	  start = FFECOM_gfrtSRDUE, end = FFECOM_gfrtERDUE;
#if 0
	else if (key)
	  start = FFECOM_gfrtSRIUE, end = FFECOM_gfrtERIUE;
#endif
	else
	  start = FFECOM_gfrtSRSUE, end = FFECOM_gfrtERSUE;
	break;

      case FFESTV_formatLABEL:	/* FMT=10 */
      case FFESTV_formatCHAREXPR:	/* FMT='(I10)' */
      case FFESTV_formatINTEXPR:	/* FMT=I [after ASSIGN 10 TO I] */
	ffeste_io_driver_ = ffeste_io_dofio_;
	if (rec)
	  start = FFECOM_gfrtSRDFE, end = FFECOM_gfrtERDFE;
#if 0
	else if (key)
	  start = FFECOM_gfrtSRIFE, end = FFECOM_gfrtERIFE;
#endif
	else if (unit == FFESTV_unitCHAREXPR)
	  start = FFECOM_gfrtSRSFI, end = FFECOM_gfrtERSFI;
	else
	  start = FFECOM_gfrtSRSFE, end = FFECOM_gfrtERSFE;
	break;

      case FFESTV_formatASTERISK:	/* FMT=* */
	ffeste_io_driver_ = ffeste_io_dolio_;
	if (unit == FFESTV_unitCHAREXPR)
	  start = FFECOM_gfrtSRSLI, end = FFECOM_gfrtERSLI;
	else
	  start = FFECOM_gfrtSRSLE, end = FFECOM_gfrtERSLE;
	break;

      case FFESTV_formatNAMELIST:	/* FMT=FOO or NML=FOO [NAMELIST
					   /FOO/] */
	ffeste_io_driver_ = NULL;	/* No start or driver function. */
	start = FFECOM_gfrtSRSNE, end = FFECOM_gfrt;
	break;

      default:
	assert ("Weird stuff" == NULL);
	start = FFECOM_gfrt, end = FFECOM_gfrt;
	break;
      }
    ffeste_io_endgfrt_ = end;

    iostat = specified (FFESTP_readixIOSTAT);
    errl = specified (FFESTP_readixERR);
    endl = specified (FFESTP_readixEND);

    ffecom_push_calltemps ();

    if (unit == FFESTV_unitCHAREXPR)
      {
	cilist = ffeste_io_icilist_ (errl || iostat,
				  info->read_spec[FFESTP_readixUNIT].u.expr,
				     endl || iostat, format,
				     &info->read_spec[FFESTP_readixFORMAT]);
      }
    else
      {
	cilist = ffeste_io_cilist_ (errl || iostat, unit,
				  info->read_spec[FFESTP_readixUNIT].u.expr,
				    5, endl || iostat, format,
				    &info->read_spec[FFESTP_readixFORMAT],
				    rec,
				  info->read_spec[FFESTP_readixREC].u.expr);
      }

    if (errl)
      {				/* ERR= */
	ffeste_io_err_
	  = ffecom_lookup_label
	  (info->read_spec[FFESTP_readixERR].u.label);

	if (endl)
	  {			/* ERR= END= */
	    ffeste_io_end_
	      = ffecom_lookup_label
	      (info->read_spec[FFESTP_readixEND].u.label);
	    ffeste_io_abort_is_temp_ = TRUE;
	    ffeste_io_abort_ = ffecom_temp_label ();
	  }
	else
	  {			/* ERR= but no END= */
	    ffeste_io_end_ = NULL_TREE;
	    if ((ffeste_io_abort_is_temp_ = iostat))
	      ffeste_io_abort_ = ffecom_temp_label ();
	    else
	      ffeste_io_abort_ = ffeste_io_err_;
	  }
      }
    else
      {				/* no ERR= */
	ffeste_io_err_ = NULL_TREE;
	if (endl)
	  {			/* END= but no ERR= */
	    ffeste_io_end_
	      = ffecom_lookup_label
	      (info->read_spec[FFESTP_readixEND].u.label);
	    if ((ffeste_io_abort_is_temp_ = iostat))
	      ffeste_io_abort_ = ffecom_temp_label ();
	    else
	      ffeste_io_abort_ = ffeste_io_end_;
	  }
	else
	  {			/* no ERR= or END= */
	    ffeste_io_end_ = NULL_TREE;
	    if ((ffeste_io_abort_is_temp_ = iostat))
	      ffeste_io_abort_ = ffecom_temp_label ();
	    else
	      ffeste_io_abort_ = NULL_TREE;
	  }
      }

    if (iostat)
      {				/* IOSTAT= */
	ffeste_io_iostat_is_temp_ = FALSE;
	ffeste_io_iostat_ = ffecom_expr
	  (info->read_spec[FFESTP_readixIOSTAT].u.expr);
      }
    else if (ffeste_io_abort_ != NULL_TREE)
      {				/* no IOSTAT= but ERR= or END= or both */
	ffeste_io_iostat_is_temp_ = TRUE;
	ffeste_io_iostat_
	  = ffecom_push_tempvar (ffecom_integer_type_node,
				 FFETARGET_charactersizeNONE, -1, FALSE);
      }
    else
      {				/* no IOSTAT=, ERR=, or END= */
	ffeste_io_iostat_is_temp_ = FALSE;
	ffeste_io_iostat_ = NULL_TREE;
      }

    /* If there is no end function, then there are no item functions (i.e.
       it's a NAMELIST), and vice versa by the way.  In this situation, don't
       generate the "if (iostat != 0) goto label;" if the label is temp abort
       label, since we're gonna fall through to there anyway.  */

    ffeste_io_call_ (ffecom_call_gfrt (start, cilist),
		     !ffeste_io_abort_is_temp_ || (end != FFECOM_gfrt));
  }

#undef specified

  push_momentary ();
#else
#error
#endif
}

/* ffeste_R909_item -- READ statement i/o item

   ffeste_R909_item(expr,expr_token);

   Implement output-list expression.  */

void
ffeste_R909_item (ffebld expr, ffelexToken expr_token)
{
  ffeste_check_item_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  ffebld_dump (expr);
  fputc (',', dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  if (expr == NULL)
    return;
  while (ffebld_op (expr) == FFEBLD_opPAREN)
    expr = ffebld_left (expr);	/* "READ *,(A)" -- really a bug in the user's
				   code, but I've been told lots of code does
				   this (blech)! */
  if (ffebld_op (expr) == FFEBLD_opANY)
    return;
  if (ffebld_op (expr) == FFEBLD_opIMPDO)
    ffeste_io_impdo_ (expr, expr_token);
  else
    ffeste_io_call_ ((*ffeste_io_driver_) (expr), TRUE);
  clear_momentary ();
#else
#error
#endif
}

/* ffeste_R909_finish -- READ statement list complete

   ffeste_R909_finish();

   Just wrap up any local activities.  */

void
ffeste_R909_finish ()
{
  ffeste_check_finish_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fputc ('\n', dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC

  /* Don't generate "if (iostat != 0) goto label;" if label is temp abort
     label, since we're gonna fall through to there anyway. */

  {
    if (ffeste_io_endgfrt_ != FFECOM_gfrt)
      ffeste_io_call_ (ffecom_call_gfrt (ffeste_io_endgfrt_, NULL_TREE),
		       !ffeste_io_abort_is_temp_);

    clear_momentary ();
    pop_momentary ();

    /* If we've got a temp label, generate its code here and have it fan out
       to the END= or ERR= label as appropriate. */

    if (ffeste_io_abort_is_temp_)
      {
	DECL_INITIAL (ffeste_io_abort_) = error_mark_node;
	emit_nop ();
	expand_label (ffeste_io_abort_);

	/* if (iostat<0) goto end_label; */

	if ((ffeste_io_end_ != NULL_TREE)
	    && (TREE_CODE (ffeste_io_end_) != ERROR_MARK))
	  {
	    expand_start_cond (ffecom_truth_value
			       (ffecom_2 (LT_EXPR, integer_type_node,
					  ffeste_io_iostat_,
					  ffecom_integer_zero_node)),
			       0);
	    expand_goto (ffeste_io_end_);
	    expand_end_cond ();
	  }

	/* if (iostat>0) goto err_label; */

	if ((ffeste_io_err_ != NULL_TREE)
	    && (TREE_CODE (ffeste_io_err_) != ERROR_MARK))
	  {
	    expand_start_cond (ffecom_truth_value
			       (ffecom_2 (GT_EXPR, integer_type_node,
					  ffeste_io_iostat_,
					  ffecom_integer_zero_node)),
			       0);
	    expand_goto (ffeste_io_err_);
	    expand_end_cond ();
	  }

      }

    /* If we've got a temp iostat, pop the temp. */

    if (ffeste_io_iostat_is_temp_)
      ffecom_pop_tempvar (ffeste_io_iostat_);

    ffecom_pop_calltemps ();

    clear_momentary ();
  }
#else
#error
#endif
}

/* ffeste_R910_start -- WRITE(...) statement list begin

   ffeste_R910_start();

   Verify that WRITE is valid here, and begin accepting items in the
   list.  */

void
ffeste_R910_start (ffestpWriteStmt *info, ffestvUnit unit,
		   ffestvFormat format, bool rec)
{
  ffeste_check_start_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  switch (format)
    {
    case FFESTV_formatNONE:
      if (rec)
	fputs ("+ WRITE_ufdac (", dmpout);
      else
	fputs ("+ WRITE_ufseq_or_idx (", dmpout);
      break;

    case FFESTV_formatLABEL:
    case FFESTV_formatCHAREXPR:
    case FFESTV_formatINTEXPR:
      if (rec)
	fputs ("+ WRITE_fmdac (", dmpout);
      else if (unit == FFESTV_unitCHAREXPR)
	fputs ("+ WRITE_fmint (", dmpout);
      else
	fputs ("+ WRITE_fmseq_or_idx (", dmpout);
      break;

    case FFESTV_formatASTERISK:
      if (unit == FFESTV_unitCHAREXPR)
	fputs ("+ WRITE_lsint (", dmpout);
      else
	fputs ("+ WRITE_lsseq (", dmpout);
      break;

    case FFESTV_formatNAMELIST:
      fputs ("+ WRITE_nlseq (", dmpout);
      break;

    default:
      assert ("Unexpected kind of format item in R910 WRITE" == NULL);
    }

  ffeste_subr_file_ ("UNIT", &info->write_spec[FFESTP_writeixUNIT]);
  ffeste_subr_file_ ("FORMAT", &info->write_spec[FFESTP_writeixFORMAT]);
  ffeste_subr_file_ ("ADVANCE", &info->write_spec[FFESTP_writeixADVANCE]);
  ffeste_subr_file_ ("EOR", &info->write_spec[FFESTP_writeixEOR]);
  ffeste_subr_file_ ("ERR", &info->write_spec[FFESTP_writeixERR]);
  ffeste_subr_file_ ("IOSTAT", &info->write_spec[FFESTP_writeixIOSTAT]);
  ffeste_subr_file_ ("REC", &info->write_spec[FFESTP_writeixREC]);
  fputs (") ", dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC

#define specified(something) (info->write_spec[something].kw_or_val_present)

  ffeste_emit_line_note_ ();

  /* Do the real work. */

  {
    ffecomGfrt start;
    ffecomGfrt end;
    tree cilist;
    bool iostat;
    bool errl;

    /* First determine the start, per-item, and end run-time functions to
       call.  The per-item function is picked by choosing an ffeste functio
       to call to handle a given item; it knows how to generate a call to the
       appropriate run-time function, and is called an "io driver".  It
       handles the implied-DO construct, for example. */

    switch (format)
      {
      case FFESTV_formatNONE:	/* no FMT= */
	ffeste_io_driver_ = ffeste_io_douio_;
	if (rec)
	  start = FFECOM_gfrtSWDUE, end = FFECOM_gfrtEWDUE;
	else
	  start = FFECOM_gfrtSWSUE, end = FFECOM_gfrtEWSUE;
	break;

      case FFESTV_formatLABEL:	/* FMT=10 */
      case FFESTV_formatCHAREXPR:	/* FMT='(I10)' */
      case FFESTV_formatINTEXPR:	/* FMT=I [after ASSIGN 10 TO I] */
	ffeste_io_driver_ = ffeste_io_dofio_;
	if (rec)
	  start = FFECOM_gfrtSWDFE, end = FFECOM_gfrtEWDFE;
	else if (unit == FFESTV_unitCHAREXPR)
	  start = FFECOM_gfrtSWSFI, end = FFECOM_gfrtEWSFI;
	else
	  start = FFECOM_gfrtSWSFE, end = FFECOM_gfrtEWSFE;
	break;

      case FFESTV_formatASTERISK:	/* FMT=* */
	ffeste_io_driver_ = ffeste_io_dolio_;
	if (unit == FFESTV_unitCHAREXPR)
	  start = FFECOM_gfrtSWSLI, end = FFECOM_gfrtEWSLI;
	else
	  start = FFECOM_gfrtSWSLE, end = FFECOM_gfrtEWSLE;
	break;

      case FFESTV_formatNAMELIST:	/* FMT=FOO or NML=FOO [NAMELIST
					   /FOO/] */
	ffeste_io_driver_ = NULL;	/* No start or driver function. */
	start = FFECOM_gfrtSWSNE, end = FFECOM_gfrt;
	break;

      default:
	assert ("Weird stuff" == NULL);
	start = FFECOM_gfrt, end = FFECOM_gfrt;
	break;
      }
    ffeste_io_endgfrt_ = end;

    iostat = specified (FFESTP_writeixIOSTAT);
    errl = specified (FFESTP_writeixERR);

    ffecom_push_calltemps ();

    if (unit == FFESTV_unitCHAREXPR)
      {
	cilist = ffeste_io_icilist_ (errl || iostat,
				info->write_spec[FFESTP_writeixUNIT].u.expr,
				     FALSE, format,
				   &info->write_spec[FFESTP_writeixFORMAT]);
      }
    else
      {
	cilist = ffeste_io_cilist_ (errl || iostat, unit,
				info->write_spec[FFESTP_writeixUNIT].u.expr,
				    6, FALSE, format,
				    &info->write_spec[FFESTP_writeixFORMAT],
				    rec,
				info->write_spec[FFESTP_writeixREC].u.expr);
      }

    ffeste_io_end_ = NULL_TREE;

    if (errl)
      {				/* ERR= */
	ffeste_io_err_
	  = ffeste_io_abort_
	  = ffecom_lookup_label
	  (info->write_spec[FFESTP_writeixERR].u.label);
	ffeste_io_abort_is_temp_ = FALSE;
      }
    else
      {				/* no ERR= */
	ffeste_io_err_ = NULL_TREE;

	if ((ffeste_io_abort_is_temp_ = iostat))
	  ffeste_io_abort_ = ffecom_temp_label ();
	else
	  ffeste_io_abort_ = NULL_TREE;
      }

    if (iostat)
      {				/* IOSTAT= */
	ffeste_io_iostat_is_temp_ = FALSE;
	ffeste_io_iostat_ = ffecom_expr
	  (info->write_spec[FFESTP_writeixIOSTAT].u.expr);
      }
    else if (ffeste_io_abort_ != NULL_TREE)
      {				/* no IOSTAT= but ERR= */
	ffeste_io_iostat_is_temp_ = TRUE;
	ffeste_io_iostat_
	  = ffecom_push_tempvar (ffecom_integer_type_node,
				 FFETARGET_charactersizeNONE, -1, FALSE);
      }
    else
      {				/* no IOSTAT=, or ERR= */
	ffeste_io_iostat_is_temp_ = FALSE;
	ffeste_io_iostat_ = NULL_TREE;
      }

    /* If there is no end function, then there are no item functions (i.e.
       it's a NAMELIST), and vice versa by the way.  In this situation, don't
       generate the "if (iostat != 0) goto label;" if the label is temp abort
       label, since we're gonna fall through to there anyway.  */

    ffeste_io_call_ (ffecom_call_gfrt (start, cilist),
		     !ffeste_io_abort_is_temp_ || (end != FFECOM_gfrt));
  }

#undef specified

  push_momentary ();
#else
#error
#endif
}

/* ffeste_R910_item -- WRITE statement i/o item

   ffeste_R910_item(expr,expr_token);

   Implement output-list expression.  */

void
ffeste_R910_item (ffebld expr, ffelexToken expr_token)
{
  ffeste_check_item_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  ffebld_dump (expr);
  fputc (',', dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  if (expr == NULL)
    return;
  if (ffebld_op (expr) == FFEBLD_opANY)
    return;
  if (ffebld_op (expr) == FFEBLD_opIMPDO)
    ffeste_io_impdo_ (expr, expr_token);
  else
    ffeste_io_call_ ((*ffeste_io_driver_) (expr), TRUE);
  clear_momentary ();
#else
#error
#endif
}

/* ffeste_R910_finish -- WRITE statement list complete

   ffeste_R910_finish();

   Just wrap up any local activities.  */

void
ffeste_R910_finish ()
{
  ffeste_check_finish_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fputc ('\n', dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC

  /* Don't generate "if (iostat != 0) goto label;" if label is temp abort
     label, since we're gonna fall through to there anyway. */

  {
    if (ffeste_io_endgfrt_ != FFECOM_gfrt)
      ffeste_io_call_ (ffecom_call_gfrt (ffeste_io_endgfrt_, NULL_TREE),
		       !ffeste_io_abort_is_temp_);

    clear_momentary ();
    pop_momentary ();

    /* If we've got a temp label, generate its code here. */

    if (ffeste_io_abort_is_temp_)
      {
	DECL_INITIAL (ffeste_io_abort_) = error_mark_node;
	emit_nop ();
	expand_label (ffeste_io_abort_);

	assert (ffeste_io_err_ == NULL_TREE);
      }

    /* If we've got a temp iostat, pop the temp. */

    if (ffeste_io_iostat_is_temp_)
      ffecom_pop_tempvar (ffeste_io_iostat_);

    ffecom_pop_calltemps ();

    clear_momentary ();
  }
#else
#error
#endif
}

/* ffeste_R911_start -- PRINT statement list begin

   ffeste_R911_start();

   Verify that PRINT is valid here, and begin accepting items in the
   list.  */

void
ffeste_R911_start (ffestpPrintStmt *info, ffestvFormat format)
{
  ffeste_check_start_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  switch (format)
    {
    case FFESTV_formatLABEL:
    case FFESTV_formatCHAREXPR:
    case FFESTV_formatINTEXPR:
      fputs ("+ PRINT_fm ", dmpout);
      break;

    case FFESTV_formatASTERISK:
      fputs ("+ PRINT_ls ", dmpout);
      break;

    case FFESTV_formatNAMELIST:
      fputs ("+ PRINT_nl ", dmpout);
      break;

    default:
      assert ("Unexpected kind of format item in R911 PRINT" == NULL);
    }
  ffeste_subr_file_ ("FORMAT", &info->print_spec[FFESTP_printixFORMAT]);
  fputc (' ', dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC

  ffeste_emit_line_note_ ();

  /* Do the real work. */

  {
    ffecomGfrt start;
    ffecomGfrt end;
    tree cilist;

    /* First determine the start, per-item, and end run-time functions to
       call.  The per-item function is picked by choosing an ffeste functio
       to call to handle a given item; it knows how to generate a call to the
       appropriate run-time function, and is called an "io driver".  It
       handles the implied-DO construct, for example. */

    switch (format)
      {
      case FFESTV_formatLABEL:	/* FMT=10 */
      case FFESTV_formatCHAREXPR:	/* FMT='(I10)' */
      case FFESTV_formatINTEXPR:	/* FMT=I [after ASSIGN 10 TO I] */
	ffeste_io_driver_ = ffeste_io_dofio_;
	start = FFECOM_gfrtSWSFE, end = FFECOM_gfrtEWSFE;
	break;

      case FFESTV_formatASTERISK:	/* FMT=* */
	ffeste_io_driver_ = ffeste_io_dolio_;
	start = FFECOM_gfrtSWSLE, end = FFECOM_gfrtEWSLE;
	break;

      case FFESTV_formatNAMELIST:	/* FMT=FOO or NML=FOO [NAMELIST
					   /FOO/] */
	ffeste_io_driver_ = NULL;	/* No start or driver function. */
	start = FFECOM_gfrtSWSNE, end = FFECOM_gfrt;
	break;

      default:
	assert ("Weird stuff" == NULL);
	start = FFECOM_gfrt, end = FFECOM_gfrt;
	break;
      }
    ffeste_io_endgfrt_ = end;

    ffecom_push_calltemps ();

    cilist = ffeste_io_cilist_ (FALSE, FFESTV_unitNONE, NULL, 6, FALSE, format,
		      &info->print_spec[FFESTP_printixFORMAT], FALSE, NULL);

    ffeste_io_end_ = NULL_TREE;
    ffeste_io_err_ = NULL_TREE;
    ffeste_io_abort_ = NULL_TREE;
    ffeste_io_abort_is_temp_ = FALSE;
    ffeste_io_iostat_is_temp_ = FALSE;
    ffeste_io_iostat_ = NULL_TREE;

    /* If there is no end function, then there are no item functions (i.e.
       it's a NAMELIST), and vice versa by the way.  In this situation, don't
       generate the "if (iostat != 0) goto label;" if the label is temp abort
       label, since we're gonna fall through to there anyway.  */

    ffeste_io_call_ (ffecom_call_gfrt (start, cilist),
		     !ffeste_io_abort_is_temp_ || (end != FFECOM_gfrt));
  }

  push_momentary ();
#else
#error
#endif
}

/* ffeste_R911_item -- PRINT statement i/o item

   ffeste_R911_item(expr,expr_token);

   Implement output-list expression.  */

void
ffeste_R911_item (ffebld expr, ffelexToken expr_token)
{
  ffeste_check_item_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  ffebld_dump (expr);
  fputc (',', dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  if (expr == NULL)
    return;
  if (ffebld_op (expr) == FFEBLD_opANY)
    return;
  if (ffebld_op (expr) == FFEBLD_opIMPDO)
    ffeste_io_impdo_ (expr, expr_token);
  else
    ffeste_io_call_ ((*ffeste_io_driver_) (expr), FALSE);
  clear_momentary ();
#else
#error
#endif
}

/* ffeste_R911_finish -- PRINT statement list complete

   ffeste_R911_finish();

   Just wrap up any local activities.  */

void
ffeste_R911_finish ()
{
  ffeste_check_finish_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fputc ('\n', dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  {
    if (ffeste_io_endgfrt_ != FFECOM_gfrt)
      ffeste_io_call_ (ffecom_call_gfrt (ffeste_io_endgfrt_, NULL_TREE),
		       FALSE);

    ffecom_pop_calltemps ();

    clear_momentary ();
    pop_momentary ();
    clear_momentary ();
  }
#else
#error
#endif
}

/* ffeste_R919 -- BACKSPACE statement

   ffeste_R919();

   Make sure a BACKSPACE is valid in the current context, and implement it.  */

void
ffeste_R919 (ffestpBeruStmt *info)
{
  ffeste_check_simple_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fputs ("+ BACKSPACE (", dmpout);
  ffeste_subr_file_ ("UNIT", &info->beru_spec[FFESTP_beruixUNIT]);
  ffeste_subr_file_ ("ERR", &info->beru_spec[FFESTP_beruixERR]);
  ffeste_subr_file_ ("IOSTAT", &info->beru_spec[FFESTP_beruixIOSTAT]);
  fputs (")\n", dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  ffeste_subr_beru_ (info, FFECOM_gfrtFBACK);
#else
#error
#endif
}

/* ffeste_R920 -- ENDFILE statement

   ffeste_R920();

   Make sure a ENDFILE is valid in the current context, and implement it.  */

void
ffeste_R920 (ffestpBeruStmt *info)
{
  ffeste_check_simple_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fputs ("+ ENDFILE (", dmpout);
  ffeste_subr_file_ ("UNIT", &info->beru_spec[FFESTP_beruixUNIT]);
  ffeste_subr_file_ ("ERR", &info->beru_spec[FFESTP_beruixERR]);
  ffeste_subr_file_ ("IOSTAT", &info->beru_spec[FFESTP_beruixIOSTAT]);
  fputs (")\n", dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  ffeste_subr_beru_ (info, FFECOM_gfrtFEND);
#else
#error
#endif
}

/* ffeste_R921 -- REWIND statement

   ffeste_R921();

   Make sure a REWIND is valid in the current context, and implement it.  */

void
ffeste_R921 (ffestpBeruStmt *info)
{
  ffeste_check_simple_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fputs ("+ REWIND (", dmpout);
  ffeste_subr_file_ ("UNIT", &info->beru_spec[FFESTP_beruixUNIT]);
  ffeste_subr_file_ ("ERR", &info->beru_spec[FFESTP_beruixERR]);
  ffeste_subr_file_ ("IOSTAT", &info->beru_spec[FFESTP_beruixIOSTAT]);
  fputs (")\n", dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  ffeste_subr_beru_ (info, FFECOM_gfrtFREW);
#else
#error
#endif
}

/* ffeste_R923A -- INQUIRE statement (non-IOLENGTH version)

   ffeste_R923A(bool by_file);

   Make sure an INQUIRE is valid in the current context, and implement it.  */

void
ffeste_R923A (ffestpInquireStmt *info, bool by_file UNUSED)
{
  ffeste_check_simple_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  if (by_file)
    {
      fputs ("+ INQUIRE_file (", dmpout);
      ffeste_subr_file_ ("FILE", &info->inquire_spec[FFESTP_inquireixFILE]);
    }
  else
    {
      fputs ("+ INQUIRE_unit (", dmpout);
      ffeste_subr_file_ ("UNIT", &info->inquire_spec[FFESTP_inquireixUNIT]);
    }
  ffeste_subr_file_ ("ACCESS", &info->inquire_spec[FFESTP_inquireixACCESS]);
  ffeste_subr_file_ ("ACTION", &info->inquire_spec[FFESTP_inquireixACTION]);
  ffeste_subr_file_ ("BLANK", &info->inquire_spec[FFESTP_inquireixBLANK]);
  ffeste_subr_file_ ("CARRIAGECONTROL", &info->inquire_spec[FFESTP_inquireixCARRIAGECONTROL]);
  ffeste_subr_file_ ("DEFAULTFILE", &info->inquire_spec[FFESTP_inquireixDEFAULTFILE]);
  ffeste_subr_file_ ("DELIM", &info->inquire_spec[FFESTP_inquireixDELIM]);
  ffeste_subr_file_ ("DIRECT", &info->inquire_spec[FFESTP_inquireixDIRECT]);
  ffeste_subr_file_ ("ERR", &info->inquire_spec[FFESTP_inquireixERR]);
  ffeste_subr_file_ ("EXIST", &info->inquire_spec[FFESTP_inquireixEXIST]);
  ffeste_subr_file_ ("FORM", &info->inquire_spec[FFESTP_inquireixFORM]);
  ffeste_subr_file_ ("FORMATTED", &info->inquire_spec[FFESTP_inquireixFORMATTED]);
  ffeste_subr_file_ ("IOSTAT", &info->inquire_spec[FFESTP_inquireixIOSTAT]);
  ffeste_subr_file_ ("KEYED", &info->inquire_spec[FFESTP_inquireixKEYED]);
  ffeste_subr_file_ ("NAME", &info->inquire_spec[FFESTP_inquireixNAME]);
  ffeste_subr_file_ ("NAMED", &info->inquire_spec[FFESTP_inquireixNAMED]);
  ffeste_subr_file_ ("NEXTREC", &info->inquire_spec[FFESTP_inquireixNEXTREC]);
  ffeste_subr_file_ ("NUMBER", &info->inquire_spec[FFESTP_inquireixNUMBER]);
  ffeste_subr_file_ ("OPENED", &info->inquire_spec[FFESTP_inquireixOPENED]);
  ffeste_subr_file_ ("ORGANIZATION", &info->inquire_spec[FFESTP_inquireixORGANIZATION]);
  ffeste_subr_file_ ("PAD", &info->inquire_spec[FFESTP_inquireixPAD]);
  ffeste_subr_file_ ("POSITION", &info->inquire_spec[FFESTP_inquireixPOSITION]);
  ffeste_subr_file_ ("READ", &info->inquire_spec[FFESTP_inquireixREAD]);
  ffeste_subr_file_ ("READWRITE", &info->inquire_spec[FFESTP_inquireixREADWRITE]);
  ffeste_subr_file_ ("RECL", &info->inquire_spec[FFESTP_inquireixRECL]);
  ffeste_subr_file_ ("RECORDTYPE", &info->inquire_spec[FFESTP_inquireixRECORDTYPE]);
  ffeste_subr_file_ ("SEQUENTIAL", &info->inquire_spec[FFESTP_inquireixSEQUENTIAL]);
  ffeste_subr_file_ ("UNFORMATTED", &info->inquire_spec[FFESTP_inquireixUNFORMATTED]);
  ffeste_subr_file_ ("WRITE", &info->inquire_spec[FFESTP_inquireixWRITE]);
  fputs (")\n", dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  {
    tree args;
    bool iostat;
    bool errl;

#define specified(something) (info->inquire_spec[something].kw_or_val_present)

    ffeste_emit_line_note_ ();

    iostat = specified (FFESTP_inquireixIOSTAT);
    errl = specified (FFESTP_inquireixERR);

    ffecom_push_calltemps ();

    args = ffeste_io_inlist_ (errl || iostat,
			      &info->inquire_spec[FFESTP_inquireixUNIT],
			      &info->inquire_spec[FFESTP_inquireixFILE],
			      &info->inquire_spec[FFESTP_inquireixEXIST],
			      &info->inquire_spec[FFESTP_inquireixOPENED],
			      &info->inquire_spec[FFESTP_inquireixNUMBER],
			      &info->inquire_spec[FFESTP_inquireixNAMED],
			      &info->inquire_spec[FFESTP_inquireixNAME],
			      &info->inquire_spec[FFESTP_inquireixACCESS],
			    &info->inquire_spec[FFESTP_inquireixSEQUENTIAL],
			      &info->inquire_spec[FFESTP_inquireixDIRECT],
			      &info->inquire_spec[FFESTP_inquireixFORM],
			      &info->inquire_spec[FFESTP_inquireixFORMATTED],
			   &info->inquire_spec[FFESTP_inquireixUNFORMATTED],
			      &info->inquire_spec[FFESTP_inquireixRECL],
			      &info->inquire_spec[FFESTP_inquireixNEXTREC],
			      &info->inquire_spec[FFESTP_inquireixBLANK]);

    if (errl)
      {
	ffeste_io_err_
	  = ffeste_io_abort_
	  = ffecom_lookup_label
	  (info->inquire_spec[FFESTP_inquireixERR].u.label);
	ffeste_io_abort_is_temp_ = FALSE;
      }
    else
      {
	ffeste_io_err_ = NULL_TREE;

	if ((ffeste_io_abort_is_temp_ = iostat))
	  ffeste_io_abort_ = ffecom_temp_label ();
	else
	  ffeste_io_abort_ = NULL_TREE;
      }

    if (iostat)
      {				/* IOSTAT= */
	ffeste_io_iostat_is_temp_ = FALSE;
	ffeste_io_iostat_ = ffecom_expr
	  (info->inquire_spec[FFESTP_inquireixIOSTAT].u.expr);
      }
    else if (ffeste_io_abort_ != NULL_TREE)
      {				/* no IOSTAT= but ERR= */
	ffeste_io_iostat_is_temp_ = TRUE;
	ffeste_io_iostat_
	  = ffecom_push_tempvar (ffecom_integer_type_node,
				 FFETARGET_charactersizeNONE, -1, FALSE);
      }
    else
      {				/* no IOSTAT=, or ERR= */
	ffeste_io_iostat_is_temp_ = FALSE;
	ffeste_io_iostat_ = NULL_TREE;
      }

    /* Don't generate "if (iostat != 0) goto label;" if label is temp abort
       label, since we're gonna fall through to there anyway. */

    ffeste_io_call_ (ffecom_call_gfrt (FFECOM_gfrtFINQU, args),
		     !ffeste_io_abort_is_temp_);

    /* If we've got a temp label, generate its code here. */

    if (ffeste_io_abort_is_temp_)
      {
	DECL_INITIAL (ffeste_io_abort_) = error_mark_node;
	emit_nop ();
	expand_label (ffeste_io_abort_);

	assert (ffeste_io_err_ == NULL_TREE);
      }

    /* If we've got a temp iostat, pop the temp. */

    if (ffeste_io_iostat_is_temp_)
      ffecom_pop_tempvar (ffeste_io_iostat_);

    ffecom_pop_calltemps ();

#undef specified
  }

  clear_momentary ();
#else
#error
#endif
}

/* ffeste_R923B_start -- INQUIRE(IOLENGTH=expr) statement list begin

   ffeste_R923B_start();

   Verify that INQUIRE is valid here, and begin accepting items in the
   list.  */

void
ffeste_R923B_start (ffestpInquireStmt *info UNUSED)
{
  ffeste_check_start_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fputs ("+ INQUIRE (", dmpout);
  ffeste_subr_file_ ("IOLENGTH", &info->inquire_spec[FFESTP_inquireixIOLENGTH]);
  fputs (") ", dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  assert ("INQUIRE(IOLENGTH=<var>) not implemented yet! ~~~" == NULL);
  ffeste_emit_line_note_ ();
  clear_momentary ();
#else
#error
#endif
}

/* ffeste_R923B_item -- INQUIRE statement i/o item

   ffeste_R923B_item(expr,expr_token);

   Implement output-list expression.  */

void
ffeste_R923B_item (ffebld expr UNUSED)
{
  ffeste_check_item_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  ffebld_dump (expr);
  fputc (',', dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  clear_momentary ();
#else
#error
#endif
}

/* ffeste_R923B_finish -- INQUIRE statement list complete

   ffeste_R923B_finish();

   Just wrap up any local activities.  */

void
ffeste_R923B_finish ()
{
  ffeste_check_finish_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fputc ('\n', dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  clear_momentary ();
#else
#error
#endif
}

/* ffeste_R1001 -- FORMAT statement

   ffeste_R1001(format_list);  */

void
ffeste_R1001 (ffests s)
{
  ffeste_check_simple_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fprintf (dmpout, "$ FORMAT %.*s\n", (int) ffests_length (s), ffests_text (s));
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  {
    tree t;
    tree ttype;
    tree maxindex;
    tree var;

    assert (ffeste_label_formatdef_ != NULL);

    ffeste_emit_line_note_ ();

    t = build_string (ffests_length (s), ffests_text (s));

    TREE_TYPE (t)
      = build_type_variant (build_array_type
			    (char_type_node,
			     build_range_type (integer_type_node,
					       integer_one_node,
					     build_int_2 (ffests_length (s),
							  0))),
			    1, 0);
    TREE_CONSTANT (t) = 1;
    TREE_STATIC (t) = 1;

    push_obstacks_nochange ();
    end_temporary_allocation ();

    var = ffecom_lookup_label (ffeste_label_formatdef_);
    if ((var != NULL_TREE)
	&& (TREE_CODE (var) == VAR_DECL))
      {
	DECL_INITIAL (var) = t;
	maxindex = build_int_2 (ffests_length (s) - 1, 0);
	ttype = TREE_TYPE (var);
	TYPE_DOMAIN (ttype) = build_range_type (integer_type_node,
						integer_zero_node,
						maxindex);
	if (!TREE_TYPE (maxindex))
	  TREE_TYPE (maxindex) = TYPE_DOMAIN (ttype);
	layout_type (ttype);
	rest_of_decl_compilation (var, NULL, 1, 0);
	expand_decl (var);
	expand_decl_init (var);
      }

    resume_temporary_allocation ();
    pop_obstacks ();

    ffeste_label_formatdef_ = NULL;
  }
#else
#error
#endif
}

/* ffeste_R1103 -- End a PROGRAM

   ffeste_R1103();  */

void
ffeste_R1103 ()
{
#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fputs ("+ END_PROGRAM\n", dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
#else
#error
#endif
}

/* ffeste_R1112 -- End a BLOCK DATA

   ffeste_R1112(TRUE);	*/

void
ffeste_R1112 ()
{
#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fputs ("* END_BLOCK_DATA\n", dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
#else
#error
#endif
}

/* ffeste_R1212 -- CALL statement

   ffeste_R1212(expr,expr_token);

   Make sure statement is valid here; implement.  */

void
ffeste_R1212 (ffebld expr)
{
  ffeste_check_simple_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fputs ("+ CALL ", dmpout);
  ffebld_dump (expr);
  fputc ('\n', dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  {
    ffebld args = ffebld_right (expr);
    ffebld arg;
    ffebld labels = NULL;	/* First in list of LABTERs. */
    ffebld prevlabels = NULL;
    ffebld prevargs = NULL;

    ffeste_emit_line_note_ ();

    /* Here we split the list at ffebld_right(expr) into two lists: one at
       ffebld_right(expr) consisting of all items that are not LABTERs, the
       other at labels consisting of all items that are LABTERs.  Then, if
       the latter list is NULL, we have an ordinary call, else we have a call
       with alternate returns. */

    for (args = ffebld_right (expr); args != NULL; args = ffebld_trail (args))
      {
	if (((arg = ffebld_head (args)) == NULL)
	    || (ffebld_op (arg) != FFEBLD_opLABTER))
	  {
	    if (prevargs == NULL)
	      {
		prevargs = args;
		ffebld_set_right (expr, args);
	      }
	    else
	      {
		ffebld_set_trail (prevargs, args);
		prevargs = args;
	      }
	  }
	else
	  {
	    if (prevlabels == NULL)
	      {
		prevlabels = labels = args;
	      }
	    else
	      {
		ffebld_set_trail (prevlabels, args);
		prevlabels = args;
	      }
	  }
      }
    if (prevlabels == NULL)
      labels = NULL;
    else
      ffebld_set_trail (prevlabels, NULL);
    if (prevargs == NULL)
      ffebld_set_right (expr, NULL);
    else
      ffebld_set_trail (prevargs, NULL);

    if (labels == NULL)
      expand_expr_stmt (ffecom_expr (expr));
    else
      {
	tree texpr;
	tree value;
	tree tlabel;
	int caseno;
	int pushok;
	tree duplicate;

	texpr = ffecom_expr (expr);
	expand_start_case (0, texpr, TREE_TYPE (texpr), "CALL statement");
	push_momentary ();	/* In case of many labels, keep 'em cleared
				   out. */
	for (caseno = 1;
	     labels != NULL;
	     ++caseno, labels = ffebld_trail (labels))
	  {
	    value = build_int_2 (caseno, 0);
	    tlabel = build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);

	    pushok = pushcase (value, convert, tlabel, &duplicate);
	    assert (pushok == 0);
	    tlabel
	      = ffecom_lookup_label (ffebld_labter (ffebld_head (labels)));
	    if ((tlabel == NULL_TREE)
		|| (TREE_CODE (tlabel) == ERROR_MARK))
	      continue;
	    TREE_USED (tlabel) = 1;
	    expand_goto (tlabel);
	    clear_momentary ();
	  }

	pop_momentary ();
	expand_end_case (texpr);
      }
    clear_momentary ();
  }
#else
#error
#endif
}

/* ffeste_R1221 -- End a FUNCTION

   ffeste_R1221(TRUE);	*/

void
ffeste_R1221 ()
{
#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fputs ("+ END_FUNCTION\n", dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
#else
#error
#endif
}

/* ffeste_R1225 -- End a SUBROUTINE

   ffeste_R1225(TRUE);	*/

void
ffeste_R1225 ()
{
#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fprintf (dmpout, "+ END_SUBROUTINE\n");
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
#else
#error
#endif
}

/* ffeste_R1226 -- ENTRY statement

   ffeste_R1226(entryname,arglist,ending_token);

   Make sure we're in a SUBROUTINE or FUNCTION, register arguments for the
   entry point name, and so on.	 */

void
ffeste_R1226 (ffesymbol entry)
{
  ffeste_check_simple_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fprintf (dmpout, "+ ENTRY %s", ffesymbol_text (entry));
  if (ffesymbol_dummyargs (entry) != NULL)
    {
      ffebld argh;

      fputc ('(', dmpout);
      for (argh = ffesymbol_dummyargs (entry);
	   argh != NULL;
	   argh = ffebld_trail (argh))
	{
	  assert (ffebld_head (argh) != NULL);
	  switch (ffebld_op (ffebld_head (argh)))
	    {
	    case FFEBLD_opSYMTER:
	      fputs (ffesymbol_text (ffebld_symter (ffebld_head (argh))),
		     dmpout);
	      break;

	    case FFEBLD_opSTAR:
	      fputc ('*', dmpout);
	      break;

	    default:
	      fputc ('?', dmpout);
	      ffebld_dump (ffebld_head (argh));
	      fputc ('?', dmpout);
	      break;
	    }
	  if (ffebld_trail (argh) != NULL)
	    fputc (',', dmpout);
	}
      fputc (')', dmpout);
    }
  fputc ('\n', dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  {
    tree label = ffesymbol_hook (entry).length_tree;

    ffeste_emit_line_note_ ();

    DECL_INITIAL (label) = error_mark_node;
    emit_nop ();
    expand_label (label);

    clear_momentary ();
  }
#else
#error
#endif
}

/* ffeste_R1227 -- RETURN statement

   ffeste_R1227(expr);

   Make sure statement is valid here; implement.  expr and expr_token are
   both NULL if there was no expression.  */

void
ffeste_R1227 (ffestw block UNUSED, ffebld expr)
{
  ffeste_check_simple_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  if (expr == NULL)
    {
      fputs ("+ RETURN\n", dmpout);
    }
  else
    {
      fputs ("+ RETURN_alternate ", dmpout);
      ffebld_dump (expr);
      fputc ('\n', dmpout);
    }
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
  {
    tree rtn;

    ffeste_emit_line_note_ ();
    ffecom_push_calltemps ();

    rtn = ffecom_return_expr (expr);

    if ((rtn == NULL_TREE)
	|| (rtn == error_mark_node))
      expand_null_return ();
    else
      {
	tree result = DECL_RESULT (current_function_decl);

	if ((result != error_mark_node)
	    && (TREE_TYPE (result) != error_mark_node))
	  expand_return (ffecom_modify (NULL_TREE,
					result,
					convert (TREE_TYPE (result),
						 rtn)));
	else
	  expand_null_return ();
      }

    ffecom_pop_calltemps ();
    clear_momentary ();
  }
#else
#error
#endif
}

/* ffeste_V018_start -- REWRITE(...) statement list begin

   ffeste_V018_start();

   Verify that REWRITE is valid here, and begin accepting items in the
   list.  */

#if FFESTR_VXT
void
ffeste_V018_start (ffestpRewriteStmt *info, ffestvFormat format)
{
  ffeste_check_start_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  switch (format)
    {
    case FFESTV_formatNONE:
      fputs ("+ REWRITE_uf (", dmpout);
      break;

    case FFESTV_formatLABEL:
    case FFESTV_formatCHAREXPR:
    case FFESTV_formatINTEXPR:
      fputs ("+ REWRITE_fm (", dmpout);
      break;

    default:
      assert ("Unexpected kind of format item in V018 REWRITE" == NULL);
    }
  ffeste_subr_file_ ("UNIT", &info->rewrite_spec[FFESTP_rewriteixUNIT]);
  ffeste_subr_file_ ("FMT", &info->rewrite_spec[FFESTP_rewriteixFMT]);
  ffeste_subr_file_ ("ERR", &info->rewrite_spec[FFESTP_rewriteixERR]);
  ffeste_subr_file_ ("IOSTAT", &info->rewrite_spec[FFESTP_rewriteixIOSTAT]);
  fputs (") ", dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
#else
#error
#endif
}

/* ffeste_V018_item -- REWRITE statement i/o item

   ffeste_V018_item(expr,expr_token);

   Implement output-list expression.  */

void
ffeste_V018_item (ffebld expr)
{
  ffeste_check_item_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  ffebld_dump (expr);
  fputc (',', dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
#else
#error
#endif
}

/* ffeste_V018_finish -- REWRITE statement list complete

   ffeste_V018_finish();

   Just wrap up any local activities.  */

void
ffeste_V018_finish ()
{
  ffeste_check_finish_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fputc ('\n', dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
#else
#error
#endif
}

/* ffeste_V019_start -- ACCEPT statement list begin

   ffeste_V019_start();

   Verify that ACCEPT is valid here, and begin accepting items in the
   list.  */

void
ffeste_V019_start (ffestpAcceptStmt *info, ffestvFormat format)
{
  ffeste_check_start_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  switch (format)
    {
    case FFESTV_formatLABEL:
    case FFESTV_formatCHAREXPR:
    case FFESTV_formatINTEXPR:
      fputs ("+ ACCEPT_fm ", dmpout);
      break;

    case FFESTV_formatASTERISK:
      fputs ("+ ACCEPT_ls ", dmpout);
      break;

    case FFESTV_formatNAMELIST:
      fputs ("+ ACCEPT_nl ", dmpout);
      break;

    default:
      assert ("Unexpected kind of format item in V019 ACCEPT" == NULL);
    }
  ffeste_subr_file_ ("FORMAT", &info->accept_spec[FFESTP_acceptixFORMAT]);
  fputc (' ', dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
#else
#error
#endif
}

/* ffeste_V019_item -- ACCEPT statement i/o item

   ffeste_V019_item(expr,expr_token);

   Implement output-list expression.  */

void
ffeste_V019_item (ffebld expr)
{
  ffeste_check_item_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  ffebld_dump (expr);
  fputc (',', dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
#else
#error
#endif
}

/* ffeste_V019_finish -- ACCEPT statement list complete

   ffeste_V019_finish();

   Just wrap up any local activities.  */

void
ffeste_V019_finish ()
{
  ffeste_check_finish_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fputc ('\n', dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
#else
#error
#endif
}

#endif
/* ffeste_V020_start -- TYPE statement list begin

   ffeste_V020_start();

   Verify that TYPE is valid here, and begin accepting items in the
   list.  */

void
ffeste_V020_start (ffestpTypeStmt *info UNUSED,
		   ffestvFormat format UNUSED)
{
  ffeste_check_start_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  switch (format)
    {
    case FFESTV_formatLABEL:
    case FFESTV_formatCHAREXPR:
    case FFESTV_formatINTEXPR:
      fputs ("+ TYPE_fm ", dmpout);
      break;

    case FFESTV_formatASTERISK:
      fputs ("+ TYPE_ls ", dmpout);
      break;

    case FFESTV_formatNAMELIST:
      fputs ("* TYPE_nl ", dmpout);
      break;

    default:
      assert ("Unexpected kind of format item in V020 TYPE" == NULL);
    }
  ffeste_subr_file_ ("FORMAT", &info->type_spec[FFESTP_typeixFORMAT]);
  fputc (' ', dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
#else
#error
#endif
}

/* ffeste_V020_item -- TYPE statement i/o item

   ffeste_V020_item(expr,expr_token);

   Implement output-list expression.  */

void
ffeste_V020_item (ffebld expr UNUSED)
{
  ffeste_check_item_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  ffebld_dump (expr);
  fputc (',', dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
#else
#error
#endif
}

/* ffeste_V020_finish -- TYPE statement list complete

   ffeste_V020_finish();

   Just wrap up any local activities.  */

void
ffeste_V020_finish ()
{
  ffeste_check_finish_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fputc ('\n', dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
#else
#error
#endif
}

/* ffeste_V021 -- DELETE statement

   ffeste_V021();

   Make sure a DELETE is valid in the current context, and implement it.  */

#if FFESTR_VXT
void
ffeste_V021 (ffestpDeleteStmt *info)
{
  ffeste_check_simple_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fputs ("+ DELETE (", dmpout);
  ffeste_subr_file_ ("UNIT", &info->delete_spec[FFESTP_deleteixUNIT]);
  ffeste_subr_file_ ("REC", &info->delete_spec[FFESTP_deleteixREC]);
  ffeste_subr_file_ ("ERR", &info->delete_spec[FFESTP_deleteixERR]);
  ffeste_subr_file_ ("IOSTAT", &info->delete_spec[FFESTP_deleteixIOSTAT]);
  fputs (")\n", dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
#else
#error
#endif
}

/* ffeste_V022 -- UNLOCK statement

   ffeste_V022();

   Make sure a UNLOCK is valid in the current context, and implement it.  */

void
ffeste_V022 (ffestpBeruStmt *info)
{
  ffeste_check_simple_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fputs ("+ UNLOCK (", dmpout);
  ffeste_subr_file_ ("UNIT", &info->beru_spec[FFESTP_beruixUNIT]);
  ffeste_subr_file_ ("ERR", &info->beru_spec[FFESTP_beruixERR]);
  ffeste_subr_file_ ("IOSTAT", &info->beru_spec[FFESTP_beruixIOSTAT]);
  fputs (")\n", dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
#else
#error
#endif
}

/* ffeste_V023_start -- ENCODE(...) statement list begin

   ffeste_V023_start();

   Verify that ENCODE is valid here, and begin accepting items in the
   list.  */

void
ffeste_V023_start (ffestpVxtcodeStmt *info)
{
  ffeste_check_start_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fputs ("+ ENCODE (", dmpout);
  ffeste_subr_file_ ("C", &info->vxtcode_spec[FFESTP_vxtcodeixC]);
  ffeste_subr_file_ ("F", &info->vxtcode_spec[FFESTP_vxtcodeixF]);
  ffeste_subr_file_ ("B", &info->vxtcode_spec[FFESTP_vxtcodeixB]);
  ffeste_subr_file_ ("ERR", &info->vxtcode_spec[FFESTP_vxtcodeixERR]);
  ffeste_subr_file_ ("IOSTAT", &info->vxtcode_spec[FFESTP_vxtcodeixIOSTAT]);
  fputs (") ", dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
#else
#error
#endif
}

/* ffeste_V023_item -- ENCODE statement i/o item

   ffeste_V023_item(expr,expr_token);

   Implement output-list expression.  */

void
ffeste_V023_item (ffebld expr)
{
  ffeste_check_item_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  ffebld_dump (expr);
  fputc (',', dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
#else
#error
#endif
}

/* ffeste_V023_finish -- ENCODE statement list complete

   ffeste_V023_finish();

   Just wrap up any local activities.  */

void
ffeste_V023_finish ()
{
  ffeste_check_finish_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fputc ('\n', dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
#else
#error
#endif
}

/* ffeste_V024_start -- DECODE(...) statement list begin

   ffeste_V024_start();

   Verify that DECODE is valid here, and begin accepting items in the
   list.  */

void
ffeste_V024_start (ffestpVxtcodeStmt *info)
{
  ffeste_check_start_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fputs ("+ DECODE (", dmpout);
  ffeste_subr_file_ ("C", &info->vxtcode_spec[FFESTP_vxtcodeixC]);
  ffeste_subr_file_ ("F", &info->vxtcode_spec[FFESTP_vxtcodeixF]);
  ffeste_subr_file_ ("B", &info->vxtcode_spec[FFESTP_vxtcodeixB]);
  ffeste_subr_file_ ("ERR", &info->vxtcode_spec[FFESTP_vxtcodeixERR]);
  ffeste_subr_file_ ("IOSTAT", &info->vxtcode_spec[FFESTP_vxtcodeixIOSTAT]);
  fputs (") ", dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
#else
#error
#endif
}

/* ffeste_V024_item -- DECODE statement i/o item

   ffeste_V024_item(expr,expr_token);

   Implement output-list expression.  */

void
ffeste_V024_item (ffebld expr)
{
  ffeste_check_item_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  ffebld_dump (expr);
  fputc (',', dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
#else
#error
#endif
}

/* ffeste_V024_finish -- DECODE statement list complete

   ffeste_V024_finish();

   Just wrap up any local activities.  */

void
ffeste_V024_finish ()
{
  ffeste_check_finish_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fputc ('\n', dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
#else
#error
#endif
}

/* ffeste_V025_start -- DEFINEFILE statement list begin

   ffeste_V025_start();

   Verify that DEFINEFILE is valid here, and begin accepting items in the
   list.  */

void
ffeste_V025_start ()
{
  ffeste_check_start_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fputs ("+ DEFINE_FILE ", dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
#else
#error
#endif
}

/* ffeste_V025_item -- DEFINE FILE statement item

   ffeste_V025_item(u,ut,m,mt,n,nt,asv,asvt);

   Implement item.  */

void
ffeste_V025_item (ffebld u, ffebld m, ffebld n, ffebld asv)
{
  ffeste_check_item_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  ffebld_dump (u);
  fputc ('(', dmpout);
  ffebld_dump (m);
  fputc (',', dmpout);
  ffebld_dump (n);
  fputs (",U,", dmpout);
  ffebld_dump (asv);
  fputs ("),", dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
#else
#error
#endif
}

/* ffeste_V025_finish -- DEFINE FILE statement list complete

   ffeste_V025_finish();

   Just wrap up any local activities.  */

void
ffeste_V025_finish ()
{
  ffeste_check_finish_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fputc ('\n', dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
#else
#error
#endif
}

/* ffeste_V026 -- FIND statement

   ffeste_V026();

   Make sure a FIND is valid in the current context, and implement it.	*/

void
ffeste_V026 (ffestpFindStmt *info)
{
  ffeste_check_simple_ ();

#if FFECOM_targetCURRENT == FFECOM_targetFFE
  fputs ("+ FIND (", dmpout);
  ffeste_subr_file_ ("UNIT", &info->find_spec[FFESTP_findixUNIT]);
  ffeste_subr_file_ ("REC", &info->find_spec[FFESTP_findixREC]);
  ffeste_subr_file_ ("ERR", &info->find_spec[FFESTP_findixERR]);
  ffeste_subr_file_ ("IOSTAT", &info->find_spec[FFESTP_findixIOSTAT]);
  fputs (")\n", dmpout);
#elif FFECOM_targetCURRENT == FFECOM_targetGCC
#else
#error
#endif
}

#endif
