/* stt.c -- Implementation File (module.c template V1.0)
   Copyright (C) 1995, 1997 Free Software Foundation, Inc.
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
      Manages lists of tokens and related info for parsing.

   Modifications:
*/

/* Include files. */

#include "proj.h"
#include "stt.h"
#include "bld.h"
#include "expr.h"
#include "info.h"
#include "lex.h"
#include "malloc.h"
#include "sta.h"
#include "stp.h"

/* Externals defined here. */


/* Simple definitions and enumerations. */


/* Internal typedefs. */


/* Private include files. */


/* Internal structure definitions. */


/* Static objects accessed by functions in this module. */


/* Static functions (internal). */


/* Internal macros. */


/* ffestt_caselist_append -- Append case to list of cases

   ffesttCaseList list;
   ffelexToken t;
   ffestt_caselist_append(list,range,case1,case2,t);

   list must have already been created by ffestt_caselist_create.  The
   list is allocated out of the scratch pool.  The token is consumed.  */

void
ffestt_caselist_append (ffesttCaseList list, bool range, ffebld case1,
			ffebld case2, ffelexToken t)
{
  ffesttCaseList new;

  new = (ffesttCaseList) malloc_new_kp (ffesta_scratch_pool,
					"FFEST case list", sizeof (*new));
  new->next = list->previous->next;
  new->previous = list->previous;
  new->next->previous = new;
  new->previous->next = new;
  new->expr1 = case1;
  new->expr2 = case2;
  new->range = range;
  new->t = t;
}

/* ffestt_caselist_create -- Create new list of cases

   ffesttCaseList list;
   list = ffestt_caselist_create();

   The list is allocated out of the scratch pool.  */

ffesttCaseList
ffestt_caselist_create ()
{
  ffesttCaseList new;

  new = (ffesttCaseList) malloc_new_kp (ffesta_scratch_pool,
					"FFEST case list root",
					sizeof (*new));
  new->next = new->previous = new;
  new->t = NULL;
  new->expr1 = NULL;
  new->expr2 = NULL;
  new->range = FALSE;
  return new;
}

/* ffestt_caselist_kill -- Kill list of cases

   ffesttCaseList list;
   ffestt_caselist_kill(list);

   The tokens on the list are killed.

   02-Mar-90  JCB  1.1
      Don't kill the list itself or change it, since it will be trashed when
      ffesta_scratch_pool is killed anyway, so kill only the lex tokens.  */

void
ffestt_caselist_kill (ffesttCaseList list)
{
  ffesttCaseList next;

  for (next = list->next; next != list; next = next->next)
    {
      ffelex_token_kill (next->t);
    }
}

/* ffestt_dimlist_append -- Append dim to list of dims

   ffesttDimList list;
   ffelexToken t;
   ffestt_dimlist_append(list,lower,upper,t);

   list must have already been created by ffestt_dimlist_create.  The
   list is allocated out of the scratch pool.  The token is consumed.  */

void
ffestt_dimlist_append (ffesttDimList list, ffebld lower, ffebld upper,
		       ffelexToken t)
{
  ffesttDimList new;

  new = (ffesttDimList) malloc_new_kp (ffesta_scratch_pool,
				       "FFEST dim list", sizeof (*new));
  new->next = list->previous->next;
  new->previous = list->previous;
  new->next->previous = new;
  new->previous->next = new;
  new->lower = lower;
  new->upper = upper;
  new->t = t;
}

/* Convert list of dims into ffebld format.

   ffesttDimList list;
   ffeinfoRank rank;
   ffebld array_size;
   ffebld extents;
   ffestt_dimlist_as_expr (list, &rank, &array_size, &extents);

   The dims in the list are converted to a list of ITEMs; the rank of the
   array, an expression representing the array size, a list of extent
   expressions, and the list of ITEMs are returned.

   If is_ugly_assumed, treat a final dimension with no lower bound
   and an upper bound of 1 as a * bound.  */

ffebld
ffestt_dimlist_as_expr (ffesttDimList list, ffeinfoRank *rank,
			ffebld *array_size, ffebld *extents,
			bool is_ugly_assumed)
{
  ffesttDimList next;
  ffebld expr;
  ffebld as;
  ffebld ex;			/* List of extents. */
  ffebld ext;			/* Extent of a given dimension. */
  ffebldListBottom bottom;
  ffeinfoRank r;
  ffeinfoKindtype nkt;
  ffetargetIntegerDefault low;
  ffetargetIntegerDefault high;
  bool zero = FALSE;		/* Zero-size array. */
  bool any = FALSE;
  bool star = FALSE;		/* Adjustable array. */

  assert (list != NULL);

  r = 0;
  ffebld_init_list (&expr, &bottom);
  for (next = list->next; next != list; next = next->next)
    {
      ++r;
      if (((next->lower == NULL)
	   || (ffebld_op (next->lower) == FFEBLD_opCONTER))
	  && (ffebld_op (next->upper) == FFEBLD_opCONTER))
	{
	  if (next->lower == NULL)
	    low = 1;
	  else
	    low = ffebld_constant_integerdefault (ffebld_conter (next->lower));
	  high = ffebld_constant_integerdefault (ffebld_conter (next->upper));
	  if (low
	      > high)
	    zero = TRUE;
	  if ((next->next == list)
	      && is_ugly_assumed
	      && (next->lower == NULL)
	      && (high == 1)
	      && (ffebld_conter_orig (next->upper) == NULL))
	    {
	      star = TRUE;
	      ffebld_append_item (&bottom,
				  ffebld_new_bounds (NULL, ffebld_new_star ()));
	      continue;
	    }
	}
      else if (((next->lower != NULL)
		&& (ffebld_op (next->lower) == FFEBLD_opANY))
	       || (ffebld_op (next->upper) == FFEBLD_opANY))
	any = TRUE;
      else if (ffebld_op (next->upper) == FFEBLD_opSTAR)
	star = TRUE;
      ffebld_append_item (&bottom,
			  ffebld_new_bounds (next->lower, next->upper));
    }
  ffebld_end_list (&bottom);

  if (zero)
    {
      as = ffebld_new_conter (ffebld_constant_new_integerdefault_val (0));
      ffebld_set_info (as, ffeinfo_new
		       (FFEINFO_basictypeINTEGER,
			FFEINFO_kindtypeINTEGERDEFAULT,
			0,
			FFEINFO_kindENTITY,
			FFEINFO_whereCONSTANT,
			FFETARGET_charactersizeNONE));
      ex = NULL;
    }
  else if (any)
    {
      as = ffebld_new_any ();
      ffebld_set_info (as, ffeinfo_new_any ());
      ex = ffebld_copy (as);
    }
  else if (star)
    {
      as = ffebld_new_star ();
      ex = ffebld_new_star ();	/* ~~Should really be list as below. */
    }
  else
    {
      as = NULL;
      ffebld_init_list (&ex, &bottom);
      for (next = list->next; next != list; next = next->next)
	{
	  if ((next->lower == NULL)
	      || ((ffebld_op (next->lower) == FFEBLD_opCONTER)
		  && (ffebld_constant_integerdefault (ffebld_conter
						      (next->lower)) == 1)))
	    ext = ffebld_copy (next->upper);
	  else
	    {
	      ext = ffebld_new_subtract (next->upper, next->lower);
	      nkt
		= ffeinfo_kindtype_max (FFEINFO_basictypeINTEGER,
					ffeinfo_kindtype (ffebld_info
							  (next->lower)),
					ffeinfo_kindtype (ffebld_info
							  (next->upper)));
	      ffebld_set_info (ext,
			       ffeinfo_new (FFEINFO_basictypeINTEGER,
					    nkt,
					    0,
					    FFEINFO_kindENTITY,
					    ((ffebld_op (ffebld_left (ext))
					      == FFEBLD_opCONTER)
					     && (ffebld_op (ffebld_right
							    (ext))
						 == FFEBLD_opCONTER))
					    ? FFEINFO_whereCONSTANT
					    : FFEINFO_whereFLEETING,
					    FFETARGET_charactersizeNONE));
	      ffebld_set_left (ext,
			       ffeexpr_convert_expr (ffebld_left (ext),
						     next->t, ext, next->t,
						     FFEEXPR_contextLET));
	      ffebld_set_right (ext,
				ffeexpr_convert_expr (ffebld_right (ext),
						      next->t, ext,
						      next->t,
						      FFEEXPR_contextLET));
	      ext = ffeexpr_collapse_subtract (ext, next->t);

	      nkt
		= ffeinfo_kindtype_max (FFEINFO_basictypeINTEGER,
					ffeinfo_kindtype (ffebld_info (ext)),
					FFEINFO_kindtypeINTEGERDEFAULT);
	      ext
		= ffebld_new_add (ext,
				  ffebld_new_conter
				  (ffebld_constant_new_integerdefault_val
				   (1)));
	      ffebld_set_info (ffebld_right (ext), ffeinfo_new
			       (FFEINFO_basictypeINTEGER,
				FFEINFO_kindtypeINTEGERDEFAULT,
				0,
				FFEINFO_kindENTITY,
				FFEINFO_whereCONSTANT,
				FFETARGET_charactersizeNONE));
	      ffebld_set_info (ext,
			       ffeinfo_new (FFEINFO_basictypeINTEGER,
					    nkt, 0, FFEINFO_kindENTITY,
					    (ffebld_op (ffebld_left (ext))
					     == FFEBLD_opCONTER)
					    ? FFEINFO_whereCONSTANT
					    : FFEINFO_whereFLEETING,
					    FFETARGET_charactersizeNONE));
	      ffebld_set_left (ext,
			       ffeexpr_convert_expr (ffebld_left (ext),
						     next->t, ext,
						     next->t,
						     FFEEXPR_contextLET));
	      ffebld_set_right (ext,
				ffeexpr_convert_expr (ffebld_right (ext),
						      next->t, ext,
						      next->t,
						      FFEEXPR_contextLET));
	      ext = ffeexpr_collapse_add (ext, next->t);
	    }
	  ffebld_append_item (&bottom, ext);
	  if (as == NULL)
	    as = ext;
	  else
	    {
	      nkt
		= ffeinfo_kindtype_max (FFEINFO_basictypeINTEGER,
					ffeinfo_kindtype (ffebld_info (as)),
				      ffeinfo_kindtype (ffebld_info (ext)));
	      as = ffebld_new_multiply (as, ext);
	      ffebld_set_info (as,
			       ffeinfo_new (FFEINFO_basictypeINTEGER,
					    nkt, 0, FFEINFO_kindENTITY,
					    ((ffebld_op (ffebld_left (as))
					      == FFEBLD_opCONTER)
					     && (ffebld_op (ffebld_right
							    (as))
						 == FFEBLD_opCONTER))
					    ? FFEINFO_whereCONSTANT
					    : FFEINFO_whereFLEETING,
					    FFETARGET_charactersizeNONE));
	      ffebld_set_left (as,
			       ffeexpr_convert_expr (ffebld_left (as),
						     next->t, as, next->t,
						     FFEEXPR_contextLET));
	      ffebld_set_right (as,
				ffeexpr_convert_expr (ffebld_right (as),
						      next->t, as,
						      next->t,
						      FFEEXPR_contextLET));
	      as = ffeexpr_collapse_multiply (as, next->t);
	    }
	}
      ffebld_end_list (&bottom);
      as = ffeexpr_convert (as, list->next->t, NULL,
			    FFEINFO_basictypeINTEGER,
			    FFEINFO_kindtypeINTEGERDEFAULT, 0,
			    FFETARGET_charactersizeNONE,
			    FFEEXPR_contextLET);
    }

  *rank = r;
  *array_size = as;
  *extents = ex;
  return expr;
}

/* ffestt_dimlist_create -- Create new list of dims

   ffesttDimList list;
   list = ffestt_dimlist_create();

   The list is allocated out of the scratch pool.  */

ffesttDimList
ffestt_dimlist_create ()
{
  ffesttDimList new;

  new = (ffesttDimList) malloc_new_kp (ffesta_scratch_pool,
				       "FFEST dim list root", sizeof (*new));
  new->next = new->previous = new;
  new->t = NULL;
  new->lower = NULL;
  new->upper = NULL;
  return new;
}

/* ffestt_dimlist_kill -- Kill list of dims

   ffesttDimList list;
   ffestt_dimlist_kill(list);

   The tokens on the list are killed.  */

void
ffestt_dimlist_kill (ffesttDimList list)
{
  ffesttDimList next;

  for (next = list->next; next != list; next = next->next)
    {
      ffelex_token_kill (next->t);
    }
}

/* Determine type of list of dimensions.

   Return KNOWN for all-constant bounds, ADJUSTABLE for constant
   and variable but no * bounds, ASSUMED for constant and * but
   not variable bounds, ADJUSTABLEASSUMED for constant and variable
   and * bounds.

   If is_ugly_assumed, treat a final dimension with no lower bound
   and an upper bound of 1 as a * bound.  */

ffestpDimtype
ffestt_dimlist_type (ffesttDimList list, bool is_ugly_assumed)
{
  ffesttDimList next;
  ffestpDimtype type;

  if (list == NULL)
    return FFESTP_dimtypeNONE;

  type = FFESTP_dimtypeKNOWN;
  for (next = list->next; next != list; next = next->next)
    {
      bool ugly_assumed = FALSE;

      if ((next->next == list)
	  && is_ugly_assumed
	  && (next->lower == NULL)
	  && (next->upper != NULL)
	  && (ffebld_op (next->upper) == FFEBLD_opCONTER)
	  && (ffebld_constant_integerdefault (ffebld_conter (next->upper))
	      == 1)
	  && (ffebld_conter_orig (next->upper) == NULL))
	ugly_assumed = TRUE;

      if (next->lower != NULL)
	{
	  if (ffebld_op (next->lower) != FFEBLD_opCONTER)
	    {
	      if (type == FFESTP_dimtypeASSUMED)
		type = FFESTP_dimtypeADJUSTABLEASSUMED;
	      else
		type = FFESTP_dimtypeADJUSTABLE;
	    }
	}
      if (next->upper != NULL)
	{
	  if (ugly_assumed
	      || (ffebld_op (next->upper) == FFEBLD_opSTAR))
	    {
	      if (type == FFESTP_dimtypeADJUSTABLE)
		type = FFESTP_dimtypeADJUSTABLEASSUMED;
	      else
		type = FFESTP_dimtypeASSUMED;
	    }
	  else if (ffebld_op (next->upper) != FFEBLD_opCONTER)
	    type = FFESTP_dimtypeADJUSTABLE;
	}
    }

  return type;
}

/* ffestt_exprlist_append -- Append expr to list of exprs

   ffesttExprList list;
   ffelexToken t;
   ffestt_exprlist_append(list,expr,t);

   list must have already been created by ffestt_exprlist_create.  The
   list is allocated out of the scratch pool.  The token is consumed.  */

void
ffestt_exprlist_append (ffesttExprList list, ffebld expr, ffelexToken t)
{
  ffesttExprList new;

  new = (ffesttExprList) malloc_new_kp (ffesta_scratch_pool,
					"FFEST expr list", sizeof (*new));
  new->next = list->previous->next;
  new->previous = list->previous;
  new->next->previous = new;
  new->previous->next = new;
  new->expr = expr;
  new->t = t;
}

/* ffestt_exprlist_create -- Create new list of exprs

   ffesttExprList list;
   list = ffestt_exprlist_create();

   The list is allocated out of the scratch pool.  */

ffesttExprList
ffestt_exprlist_create ()
{
  ffesttExprList new;

  new = (ffesttExprList) malloc_new_kp (ffesta_scratch_pool,
				     "FFEST expr list root", sizeof (*new));
  new->next = new->previous = new;
  new->expr = NULL;
  new->t = NULL;
  return new;
}

/* ffestt_exprlist_drive -- Drive list of token pairs into function

   ffesttExprList list;
   void fn(ffebld expr,ffelexToken t);
   ffestt_exprlist_drive(list,fn);

   The expr/token pairs in the list are passed to the function one pair
   at a time.  */

void
ffestt_exprlist_drive (ffesttExprList list, void (*fn) (ffebld, ffelexToken))
{
  ffesttExprList next;

  if (list == NULL)
    return;

  for (next = list->next; next != list; next = next->next)
    {
      (*fn) (next->expr, next->t);
    }
}

/* ffestt_exprlist_kill -- Kill list of exprs

   ffesttExprList list;
   ffestt_exprlist_kill(list);

   The tokens on the list are killed.

   02-Mar-90  JCB  1.1
      Don't kill the list itself or change it, since it will be trashed when
      ffesta_scratch_pool is killed anyway, so kill only the lex tokens.  */

void
ffestt_exprlist_kill (ffesttExprList list)
{
  ffesttExprList next;

  for (next = list->next; next != list; next = next->next)
    {
      ffelex_token_kill (next->t);
    }
}

/* ffestt_formatlist_append -- Append null format to list of formats

   ffesttFormatList list, new;
   new = ffestt_formatlist_append(list);

   list must have already been created by ffestt_formatlist_create.  The
   new item is allocated out of the scratch pool.  The caller must initialize
   it appropriately.  */

ffesttFormatList
ffestt_formatlist_append (ffesttFormatList list)
{
  ffesttFormatList new;

  new = (ffesttFormatList) malloc_new_kp (ffesta_scratch_pool,
					"FFEST format list", sizeof (*new));
  new->next = list->previous->next;
  new->previous = list->previous;
  new->next->previous = new;
  new->previous->next = new;
  return new;
}

/* ffestt_formatlist_create -- Create new list of formats

   ffesttFormatList list;
   list = ffestt_formatlist_create(NULL);

   The list is allocated out of the scratch pool.  */

ffesttFormatList
ffestt_formatlist_create (ffesttFormatList parent, ffelexToken t)
{
  ffesttFormatList new;

  new = (ffesttFormatList) malloc_new_kp (ffesta_scratch_pool,
				   "FFEST format list root", sizeof (*new));
  new->next = new->previous = new;
  new->type = FFESTP_formattypeNone;
  new->t = t;
  new->u.root.parent = parent;
  return new;
}

/* ffestt_formatlist_kill -- Kill tokens on list of formats

   ffesttFormatList list;
   ffestt_formatlist_kill(list);

   The tokens on the list are killed.  */

void
ffestt_formatlist_kill (ffesttFormatList list)
{
  ffesttFormatList next;

  /* Always kill from the very top on down. */

  while (list->u.root.parent != NULL)
    list = list->u.root.parent->next;

  /* Kill first token for this list. */

  if (list->t != NULL)
    ffelex_token_kill (list->t);

  /* Kill each item in this list. */

  for (next = list->next; next != list; next = next->next)
    {
      ffelex_token_kill (next->t);
      switch (next->type)
	{
	case FFESTP_formattypeI:
	case FFESTP_formattypeB:
	case FFESTP_formattypeO:
	case FFESTP_formattypeZ:
	case FFESTP_formattypeF:
	case FFESTP_formattypeE:
	case FFESTP_formattypeEN:
	case FFESTP_formattypeG:
	case FFESTP_formattypeL:
	case FFESTP_formattypeA:
	case FFESTP_formattypeD:
	  if (next->u.R1005.R1004.t != NULL)
	    ffelex_token_kill (next->u.R1005.R1004.t);
	  if (next->u.R1005.R1006.t != NULL)
	    ffelex_token_kill (next->u.R1005.R1006.t);
	  if (next->u.R1005.R1007_or_R1008.t != NULL)
	    ffelex_token_kill (next->u.R1005.R1007_or_R1008.t);
	  if (next->u.R1005.R1009.t != NULL)
	    ffelex_token_kill (next->u.R1005.R1009.t);
	  break;

	case FFESTP_formattypeQ:
	case FFESTP_formattypeDOLLAR:
	case FFESTP_formattypeP:
	case FFESTP_formattypeT:
	case FFESTP_formattypeTL:
	case FFESTP_formattypeTR:
	case FFESTP_formattypeX:
	case FFESTP_formattypeS:
	case FFESTP_formattypeSP:
	case FFESTP_formattypeSS:
	case FFESTP_formattypeBN:
	case FFESTP_formattypeBZ:
	case FFESTP_formattypeSLASH:
	case FFESTP_formattypeCOLON:
	  if (next->u.R1010.val.t != NULL)
	    ffelex_token_kill (next->u.R1010.val.t);
	  break;

	case FFESTP_formattypeR1016:
	  break;		/* Nothing more to do. */

	case FFESTP_formattypeFORMAT:
	  if (next->u.R1003D.R1004.t != NULL)
	    ffelex_token_kill (next->u.R1003D.R1004.t);
	  next->u.R1003D.format->u.root.parent = NULL;	/* Parent already dying. */
	  ffestt_formatlist_kill (next->u.R1003D.format);
	  break;

	default:
	  assert (FALSE);
	}
    }
}

/* ffestt_implist_append -- Append token pair to list of token pairs

   ffesttImpList list;
   ffelexToken t;
   ffestt_implist_append(list,start_token,end_token);

   list must have already been created by ffestt_implist_create.  The
   list is allocated out of the scratch pool.  The tokens are consumed.	 */

void
ffestt_implist_append (ffesttImpList list, ffelexToken first, ffelexToken last)
{
  ffesttImpList new;

  new = (ffesttImpList) malloc_new_kp (ffesta_scratch_pool,
				       "FFEST token list", sizeof (*new));
  new->next = list->previous->next;
  new->previous = list->previous;
  new->next->previous = new;
  new->previous->next = new;
  new->first = first;
  new->last = last;
}

/* ffestt_implist_create -- Create new list of token pairs

   ffesttImpList list;
   list = ffestt_implist_create();

   The list is allocated out of the scratch pool.  */

ffesttImpList
ffestt_implist_create ()
{
  ffesttImpList new;

  new = (ffesttImpList) malloc_new_kp (ffesta_scratch_pool,
				       "FFEST token list root",
				       sizeof (*new));
  new->next = new->previous = new;
  new->first = NULL;
  new->last = NULL;
  return new;
}

/* ffestt_implist_drive -- Drive list of token pairs into function

   ffesttImpList list;
   void fn(ffelexToken first,ffelexToken last);
   ffestt_implist_drive(list,fn);

   The token pairs in the list are passed to the function one pair at a time.  */

void
ffestt_implist_drive (ffesttImpList list, void (*fn) (ffelexToken, ffelexToken))
{
  ffesttImpList next;

  if (list == NULL)
    return;

  for (next = list->next; next != list; next = next->next)
    {
      (*fn) (next->first, next->last);
    }
}

/* ffestt_implist_kill -- Kill list of token pairs

   ffesttImpList list;
   ffestt_implist_kill(list);

   The tokens on the list are killed.  */

void
ffestt_implist_kill (ffesttImpList list)
{
  ffesttImpList next;

  for (next = list->next; next != list; next = next->next)
    {
      ffelex_token_kill (next->first);
      if (next->last != NULL)
	ffelex_token_kill (next->last);
    }
}

/* ffestt_tokenlist_append -- Append token to list of tokens

   ffesttTokenList tl;
   ffelexToken t;
   ffestt_tokenlist_append(tl,t);

   tl must have already been created by ffestt_tokenlist_create.  The
   list is allocated out of the scratch pool.  The token is consumed.  */

void
ffestt_tokenlist_append (ffesttTokenList tl, ffelexToken t)
{
  ffesttTokenItem ti;

  ti = (ffesttTokenItem) malloc_new_kp (ffesta_scratch_pool,
					"FFEST token item", sizeof (*ti));
  ti->next = (ffesttTokenItem) &tl->first;
  ti->previous = tl->last;
  ti->next->previous = ti;
  ti->previous->next = ti;
  ti->t = t;
  ++tl->count;
}

/* ffestt_tokenlist_create -- Create new list of tokens

   ffesttTokenList tl;
   tl = ffestt_tokenlist_create();

   The list is allocated out of the scratch pool.  */

ffesttTokenList
ffestt_tokenlist_create ()
{
  ffesttTokenList tl;

  tl = (ffesttTokenList) malloc_new_kp (ffesta_scratch_pool,
					"FFEST token list", sizeof (*tl));
  tl->first = tl->last = (ffesttTokenItem) &tl->first;
  tl->count = 0;
  return tl;
}

/* ffestt_tokenlist_drive -- Drive list of tokens

   ffesttTokenList tl;
   void fn(ffelexToken t);
   ffestt_tokenlist_drive(tl,fn);

   The tokens in the list are passed to the given function.  */

void
ffestt_tokenlist_drive (ffesttTokenList tl, void (*fn) (ffelexToken))
{
  ffesttTokenItem ti;

  if (tl == NULL)
    return;

  for (ti = tl->first; ti != (ffesttTokenItem) &tl->first; ti = ti->next)
    {
      (*fn) (ti->t);
    }
}

/* ffestt_tokenlist_handle -- Handle list of tokens

   ffesttTokenList tl;
   ffelexHandler handler;
   handler = ffestt_tokenlist_handle(tl,handler);

   The tokens in the list are passed to the handler(s).	 */

ffelexHandler
ffestt_tokenlist_handle (ffesttTokenList tl, ffelexHandler handler)
{
  ffesttTokenItem ti;

  for (ti = tl->first; ti != (ffesttTokenItem) &tl->first; ti = ti->next)
    handler = (ffelexHandler) (*handler) (ti->t);

  return (ffelexHandler) handler;
}

/* ffestt_tokenlist_kill -- Kill list of tokens

   ffesttTokenList tl;
   ffestt_tokenlist_kill(tl);

   The tokens on the list are killed.

   02-Mar-90  JCB  1.1
      Don't kill the list itself or change it, since it will be trashed when
      ffesta_scratch_pool is killed anyway, so kill only the lex tokens.  */

void
ffestt_tokenlist_kill (ffesttTokenList tl)
{
  ffesttTokenItem ti;

  for (ti = tl->first; ti != (ffesttTokenItem) &tl->first; ti = ti->next)
    {
      ffelex_token_kill (ti->t);
    }
}
