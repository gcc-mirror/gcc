/* data.c -- Implementation File (module.c template V1.0)
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

   Description:
      Do the tough things for DATA statement (and INTEGER FOO/.../-style
      initializations), like implied-DO and suchlike.

   Modifications:
*/

/* Include files. */

#include "proj.h"
#include "data.h"
#include "bit.h"
#include "bld.h"
#include "com.h"
#include "expr.h"
#include "global.h"
#include "malloc.h"
#include "st.h"
#include "storag.h"
#include "top.h"

/* Externals defined here. */


/* Simple definitions and enumerations. */

/* I picked this value as one that, when plugged into a couple of small
   but nearly identical test cases I have called BIG-0.f and BIG-1.f,
   causes BIG-1.f to take about 10 times as long (elapsed) to compile
   (in f771 only) as BIG-0.f.  These test cases differ in that BIG-0.f
   doesn't put the one initialized variable in a common area that has
   a large uninitialized array in it, while BIG-1.f does.  The size of
   the array is this many elements, as long as they all are INTEGER
   type.  Note that, as of 0.5.18, sparse cases are better handled,
   so BIG-2.f now is used; it provides nonzero initial
   values for all elements of the same array BIG-0 has.  */
#ifndef FFEDATA_sizeTOO_BIG_INIT_
#define FFEDATA_sizeTOO_BIG_INIT_ 75*1024
#endif

/* Internal typedefs. */

typedef struct _ffedata_convert_cache_ *ffedataConvertCache_;
typedef struct _ffedata_impdo_ *ffedataImpdo_;

/* Private include files. */


/* Internal structure definitions. */

struct _ffedata_convert_cache_
  {
    ffebld converted;		/* Results of converting expr to following
				   type. */
    ffeinfoBasictype basic_type;
    ffeinfoKindtype kind_type;
    ffetargetCharacterSize size;
    ffeinfoRank rank;
  };

struct _ffedata_impdo_
  {
    ffedataImpdo_ outer;	/* Enclosing IMPDO construct. */
    ffebld outer_list;		/* Item after my IMPDO on the outer list. */
    ffebld my_list;		/* Beginning of list in my IMPDO. */
    ffesymbol itervar;		/* Iteration variable. */
    ffetargetIntegerDefault increment;
    ffetargetIntegerDefault final;
  };

/* Static objects accessed by functions in this module. */

static ffedataImpdo_ ffedata_stack_ = NULL;
static ffebld ffedata_list_ = NULL;
static bool ffedata_reinit_;	/* value_ should report REINIT error. */
static bool ffedata_reported_error_;	/* Error has been reported. */
static ffesymbol ffedata_symbol_ = NULL;	/* Symbol being initialized. */
static ffeinfoBasictype ffedata_basictype_;	/* Info on symbol. */
static ffeinfoKindtype ffedata_kindtype_;
static ffestorag ffedata_storage_;	/* If non-NULL, inits go into this parent. */
static ffeinfoBasictype ffedata_storage_bt_;	/* Info on storage. */
static ffeinfoKindtype ffedata_storage_kt_;
static ffetargetOffset ffedata_storage_size_;	/* Size of entire storage. */
static ffetargetAlign ffedata_storage_units_;	/* #units per storage unit. */
static ffetargetOffset ffedata_arraysize_;	/* Size of array being
						   inited. */
static ffetargetOffset ffedata_expected_;	/* Number of elements to
						   init. */
static ffetargetOffset ffedata_number_;	/* #elements inited so far. */
static ffetargetOffset ffedata_offset_;	/* Offset of next element. */
static ffetargetOffset ffedata_symbolsize_;	/* Size of entire sym. */
static ffetargetCharacterSize ffedata_size_;	/* Size of an element. */
static ffetargetCharacterSize ffedata_charexpected_;	/* #char to init. */
static ffetargetCharacterSize ffedata_charnumber_;	/* #chars inited. */
static ffetargetCharacterSize ffedata_charoffset_;	/* Offset of next char. */
static ffedataConvertCache_ ffedata_convert_cache_;	/* Fewer conversions. */
static int ffedata_convert_cache_max_ = 0;	/* #entries available. */
static int ffedata_convert_cache_use_ = 0;	/* #entries in use. */

/* Static functions (internal). */

static bool ffedata_advance_ (void);
static ffebld ffedata_convert_ (ffebld source, ffelexToken source_token,
	    ffelexToken dest_token, ffeinfoBasictype bt, ffeinfoKindtype kt,
				ffeinfoRank rk, ffetargetCharacterSize sz);
static ffetargetInteger1 ffedata_eval_integer1_ (ffebld expr);
static ffetargetOffset ffedata_eval_offset_ (ffebld subscripts,
					     ffebld dims);
static ffetargetCharacterSize ffedata_eval_substr_begin_ (ffebld expr);
static ffetargetCharacterSize ffedata_eval_substr_end_ (ffebld expr,
		    ffetargetCharacterSize min, ffetargetCharacterSize max);
static void ffedata_gather_ (ffestorag mst, ffestorag st);
static void ffedata_pop_ (void);
static void ffedata_push_ (void);
static bool ffedata_value_ (ffebld value, ffelexToken token);

/* Internal macros. */


/* ffedata_begin -- Initialize with list of targets

   ffebld list;
   ffedata_begin(list);	 // ITEM... list of SYMTERs, ARRAYs, SUBSTRs, ...

   Remember the list.  After this call, 0...n calls to ffedata_value must
   follow, and then a single call to ffedata_end.  */

void
ffedata_begin (ffebld list)
{
  assert (ffedata_list_ == NULL);
  ffedata_list_ = list;
  ffedata_symbol_ = NULL;
  ffedata_reported_error_ = FALSE;
  ffedata_reinit_ = FALSE;
  ffedata_advance_ ();
}

/* ffedata_end -- End of initialization sequence

   if (ffedata_end(FALSE))
       // everything's ok

   Make sure the end of the list is valid here.	 */

bool
ffedata_end (bool reported_error, ffelexToken t)
{
  reported_error |= ffedata_reported_error_;

  /* If still targets to initialize, too few initializers, so complain. */

  if ((ffedata_symbol_ != NULL) && !reported_error)
    {
      reported_error = TRUE;
      ffebad_start (FFEBAD_DATA_TOOFEW);
      ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
      ffebad_string (ffesymbol_text (ffedata_symbol_));
      ffebad_finish ();
    }

  /* Pop off any impdo stacks (present only if ffedata_symbol_ != NULL). */

  while (ffedata_stack_ != NULL)
    ffedata_pop_ ();

  if (ffedata_list_ != NULL)
    {
      assert (reported_error);
      ffedata_list_ = NULL;
    }

  return TRUE;
}

/* ffedata_gather -- Gather previously disparate initializations into one place

   ffestorag st;  // A typeCBLOCK or typeLOCAL aggregate.
   ffedata_gather(st);

   Prior to this call, st has no init or accretion info, but (presumably
   at least one of) its subordinate storage areas has init or accretion
   info.  After this call, none of the subordinate storage areas has inits,
   because they've all been moved into the newly created init/accretion
   info for st.	 During this call, conflicting inits produce only one
   error message.  */

void
ffedata_gather (ffestorag st)
{
  ffesymbol s;
  ffebld b;

  /* Prepare info on the storage area we're putting init info into. */

  ffetarget_aggregate_info (&ffedata_storage_bt_, &ffedata_storage_kt_,
			    &ffedata_storage_units_, ffestorag_basictype (st),
			    ffestorag_kindtype (st));
  ffedata_storage_size_ = ffestorag_size (st) / ffedata_storage_units_;
  assert (ffestorag_size (st) % ffedata_storage_units_ == 0);

  /* If a CBLOCK, gather all the init info for its explicit members. */

  if ((ffestorag_type (st) == FFESTORAG_typeCBLOCK)
      && (ffestorag_symbol (st) != NULL))
    {
      s = ffestorag_symbol (st);
      for (b = ffesymbol_commonlist (s); b != NULL; b = ffebld_trail (b))
	ffedata_gather_ (st,
			 ffesymbol_storage (ffebld_symter (ffebld_head (b))));
    }

  /* For CBLOCK or LOCAL, gather all the init info for equivalenced members. */

  ffestorag_drive (ffestorag_list_equivs (st), ffedata_gather_, st);
}

/* ffedata_value -- Provide some number of initial values

   ffebld value;
   ffelexToken t;  // Points to the value.
   if (ffedata_value(1,value,t))
       // Everything's ok

   Makes sure the value is ok, then remembers it according to the list
   provided to ffedata_begin.  As many instances of the value may be
   supplied as desired, as indicated by the first argument.  */

bool
ffedata_value (ffetargetIntegerDefault rpt, ffebld value, ffelexToken token)
{
  ffetargetIntegerDefault i;

  /* Maybe ignore zero values, to speed up compiling, even though we lose
     checking for multiple initializations for now.  */

  if (!ffe_is_zeros ()
      && (value != NULL)
      && (ffebld_op (value) == FFEBLD_opCONTER)
      && ffebld_constant_is_zero (ffebld_conter (value)))
    value = NULL;
  else if ((value != NULL)
	   && (ffebld_op (value) == FFEBLD_opANY))
    value = NULL;
  else
    {
      /* Must be a constant. */
      assert (value != NULL);
      assert (ffebld_op (value) == FFEBLD_opCONTER);
    }

  /* Later we can optimize certain cases by seeing that the target array can
     take some number of values, and provide this number to _value_. */

  if (rpt == 1)
    ffedata_convert_cache_use_ = -1;	/* Don't bother caching. */
  else
    ffedata_convert_cache_use_ = 0;	/* Maybe use the cache. */

  for (i = 0; i < rpt; ++i)
    {
      if ((ffedata_symbol_ != NULL)
	  && !ffesymbol_is_init (ffedata_symbol_))
	{
	  ffesymbol_signal_change (ffedata_symbol_);
	  ffesymbol_update_init (ffedata_symbol_);
	  if (1 || ffe_is_90 ())
	    ffesymbol_update_save (ffedata_symbol_);
#if FFEGLOBAL_ENABLED
	  if (ffesymbol_common (ffedata_symbol_) != NULL)
	    ffeglobal_init_common (ffesymbol_common (ffedata_symbol_),
				   token);
#endif
	  ffesymbol_signal_unreported (ffedata_symbol_);
	}
      if (!ffedata_value_ (value, token))
	return FALSE;
    }

  return TRUE;
}

/* ffedata_advance_ -- Advance initialization target to next item in list

   if (ffedata_advance_())
       // everything's ok

   Sets common info to characterize the next item in the list.	Handles
   IMPDO constructs accordingly.  Does not handle advances within a single
   item, as in the common extension "DATA CHARTYPE/33,34,35/", where
   CHARTYPE is CHARACTER*3, for example.  */

static bool
ffedata_advance_ ()
{
  ffebld next;

  /* Come here after handling an IMPDO. */

tail_recurse:			/* :::::::::::::::::::: */

  /* Assume we're not going to find a new target for now. */

  ffedata_symbol_ = NULL;

  /* If at the end of the list, we're done. */

  if (ffedata_list_ == NULL)
    {
      ffetargetIntegerDefault newval;

      if (ffedata_stack_ == NULL)
	return TRUE;		/* No IMPDO in progress, we is done! */

      /* Iterate the IMPDO. */

      newval = ffesymbol_value (ffedata_stack_->itervar)
	+ ffedata_stack_->increment;

      /* See if we're still in the loop. */

      if (((ffedata_stack_->increment > 0)
	   ? newval > ffedata_stack_->final
	   : newval < ffedata_stack_->final)
	  || (((ffesymbol_value (ffedata_stack_->itervar) < 0)
	       == (ffedata_stack_->increment < 0))
	      && ((ffesymbol_value (ffedata_stack_->itervar) < 0)
		  != (newval < 0))))	/* Overflow/underflow? */
	{			/* Done with the loop. */
	  ffedata_list_ = ffedata_stack_->outer_list;	/* Restore list. */
	  ffedata_pop_ ();	/* Pop me off the impdo stack. */
	}
      else
	{			/* Still in the loop, reset the list and
				   update the iter var. */
	  ffedata_list_ = ffedata_stack_->my_list;	/* Reset list. */
	  ffesymbol_set_value (ffedata_stack_->itervar, newval);
	}
      goto tail_recurse;	/* :::::::::::::::::::: */
    }

  /* Move to the next item in the list. */

  next = ffebld_head (ffedata_list_);
  ffedata_list_ = ffebld_trail (ffedata_list_);

  /* Really shouldn't happen. */

  if (next == NULL)
    return TRUE;

  /* See what kind of target this is. */

  switch (ffebld_op (next))
    {
    case FFEBLD_opSYMTER:	/* Simple reference to scalar or array. */
      ffedata_symbol_ = ffebld_symter (next);
      ffedata_storage_ = (ffesymbol_storage (ffedata_symbol_) == NULL) ? NULL
	: ffestorag_parent (ffesymbol_storage (ffedata_symbol_));
      if (ffedata_storage_ != NULL)
	{
	  ffetarget_aggregate_info (&ffedata_storage_bt_, &ffedata_storage_kt_,
				    &ffedata_storage_units_,
				    ffestorag_basictype (ffedata_storage_),
				    ffestorag_kindtype (ffedata_storage_));
	  ffedata_storage_size_ = ffestorag_size (ffedata_storage_)
	    / ffedata_storage_units_;
	  assert (ffestorag_size (ffedata_storage_) % ffedata_storage_units_ == 0);
	}

      if ((ffesymbol_init (ffedata_symbol_) != NULL)
	  || (ffesymbol_accretion (ffedata_symbol_) != NULL)
	  || ((ffedata_storage_ != NULL)
	      && (ffestorag_init (ffedata_storage_) != NULL)))
	{
#if 0
	  ffebad_start (FFEBAD_DATA_REINIT);
	  ffest_ffebad_here_current_stmt (0);
	  ffebad_string (ffesymbol_text (ffedata_symbol_));
	  ffebad_finish ();
	  ffedata_reported_error_ = TRUE;
	  return FALSE;
#else
	  ffedata_reinit_ = TRUE;
	  return TRUE;
#endif
	}
      ffedata_basictype_ = ffesymbol_basictype (ffedata_symbol_);
      ffedata_kindtype_ = ffesymbol_kindtype (ffedata_symbol_);
      if (ffesymbol_rank (ffedata_symbol_) == 0)
	ffedata_arraysize_ = 1;
      else
	{
	  ffebld size = ffesymbol_arraysize (ffedata_symbol_);

	  assert (size != NULL);
	  assert (ffebld_op (size) == FFEBLD_opCONTER);
	  assert (ffeinfo_basictype (ffebld_info (size))
		  == FFEINFO_basictypeINTEGER);
	  assert (ffeinfo_kindtype (ffebld_info (size))
		  == FFEINFO_kindtypeINTEGERDEFAULT);
	  ffedata_arraysize_ = ffebld_constant_integerdefault (ffebld_conter
							       (size));
	}
      ffedata_expected_ = ffedata_arraysize_;
      ffedata_number_ = 0;
      ffedata_offset_ = 0;
      ffedata_size_ = (ffedata_basictype_ == FFEINFO_basictypeCHARACTER)
	? ffesymbol_size (ffedata_symbol_) : 1;
      ffedata_symbolsize_ = ffedata_size_ * ffedata_arraysize_;
      ffedata_charexpected_ = ffedata_size_;
      ffedata_charnumber_ = 0;
      ffedata_charoffset_ = 0;
      break;

    case FFEBLD_opARRAYREF:	/* Reference to element of array. */
      ffedata_symbol_ = ffebld_symter (ffebld_left (next));
      ffedata_storage_ = (ffesymbol_storage (ffedata_symbol_) == NULL) ? NULL
	: ffestorag_parent (ffesymbol_storage (ffedata_symbol_));
      if (ffedata_storage_ != NULL)
	{
	  ffetarget_aggregate_info (&ffedata_storage_bt_, &ffedata_storage_kt_,
				    &ffedata_storage_units_,
				    ffestorag_basictype (ffedata_storage_),
				    ffestorag_kindtype (ffedata_storage_));
	  ffedata_storage_size_ = ffestorag_size (ffedata_storage_)
	    / ffedata_storage_units_;
	  assert (ffestorag_size (ffedata_storage_) % ffedata_storage_units_ == 0);
	}

      if ((ffesymbol_init (ffedata_symbol_) != NULL)
	  || ((ffedata_storage_ != NULL)
	      && (ffestorag_init (ffedata_storage_) != NULL)))
	{
#if 0
	  ffebad_start (FFEBAD_DATA_REINIT);
	  ffest_ffebad_here_current_stmt (0);
	  ffebad_string (ffesymbol_text (ffedata_symbol_));
	  ffebad_finish ();
	  ffedata_reported_error_ = TRUE;
	  return FALSE;
#else
	  ffedata_reinit_ = TRUE;
	  return TRUE;
#endif
	}
      ffedata_basictype_ = ffesymbol_basictype (ffedata_symbol_);
      ffedata_kindtype_ = ffesymbol_kindtype (ffedata_symbol_);
      if (ffesymbol_rank (ffedata_symbol_) == 0)
	ffedata_arraysize_ = 1;	/* Shouldn't happen in this case... */
      else
	{
	  ffebld size = ffesymbol_arraysize (ffedata_symbol_);

	  assert (size != NULL);
	  assert (ffebld_op (size) == FFEBLD_opCONTER);
	  assert (ffeinfo_basictype (ffebld_info (size))
		  == FFEINFO_basictypeINTEGER);
	  assert (ffeinfo_kindtype (ffebld_info (size))
		  == FFEINFO_kindtypeINTEGERDEFAULT);
	  ffedata_arraysize_ = ffebld_constant_integerdefault (ffebld_conter
							       (size));
	}
      ffedata_expected_ = 1;
      ffedata_number_ = 0;
      ffedata_offset_ = ffedata_eval_offset_ (ffebld_right (next),
					  ffesymbol_dims (ffedata_symbol_));
      ffedata_size_ = (ffedata_basictype_ == FFEINFO_basictypeCHARACTER)
	? ffesymbol_size (ffedata_symbol_) : 1;
      ffedata_symbolsize_ = ffedata_size_ * ffedata_arraysize_;
      ffedata_charexpected_ = ffedata_size_;
      ffedata_charnumber_ = 0;
      ffedata_charoffset_ = 0;
      break;

    case FFEBLD_opSUBSTR:	/* Substring reference to scalar or array
				   element. */
      {
	bool arrayref = ffebld_op (ffebld_left (next)) == FFEBLD_opARRAYREF;
	ffebld colon = ffebld_right (next);

	assert (colon != NULL);

	ffedata_symbol_ = ffebld_symter (ffebld_left (arrayref
					      ? ffebld_left (next) : next));
	ffedata_storage_ = (ffesymbol_storage (ffedata_symbol_) == NULL) ? NULL
	  : ffestorag_parent (ffesymbol_storage (ffedata_symbol_));
	if (ffedata_storage_ != NULL)
	  {
	    ffetarget_aggregate_info (&ffedata_storage_bt_, &ffedata_storage_kt_,
				      &ffedata_storage_units_,
				      ffestorag_basictype (ffedata_storage_),
				      ffestorag_kindtype (ffedata_storage_));
	    ffedata_storage_size_ = ffestorag_size (ffedata_storage_)
	      / ffedata_storage_units_;
	    assert (ffestorag_size (ffedata_storage_) % ffedata_storage_units_ == 0);
	  }

	if ((ffesymbol_init (ffedata_symbol_) != NULL)
	    || ((ffedata_storage_ != NULL)
		&& (ffestorag_init (ffedata_storage_) != NULL)))
	  {
#if 0
	    ffebad_start (FFEBAD_DATA_REINIT);
	    ffest_ffebad_here_current_stmt (0);
	    ffebad_string (ffesymbol_text (ffedata_symbol_));
	    ffebad_finish ();
	    ffedata_reported_error_ = TRUE;
	    return FALSE;
#else
	    ffedata_reinit_ = TRUE;
	    return TRUE;
#endif
	  }
	ffedata_basictype_ = ffesymbol_basictype (ffedata_symbol_);
	ffedata_kindtype_ = ffesymbol_kindtype (ffedata_symbol_);
	if (ffesymbol_rank (ffedata_symbol_) == 0)
	  ffedata_arraysize_ = 1;
	else
	  {
	    ffebld size = ffesymbol_arraysize (ffedata_symbol_);

	    assert (size != NULL);
	    assert (ffebld_op (size) == FFEBLD_opCONTER);
	    assert (ffeinfo_basictype (ffebld_info (size))
		    == FFEINFO_basictypeINTEGER);
	    assert (ffeinfo_kindtype (ffebld_info (size))
		    == FFEINFO_kindtypeINTEGERDEFAULT);
	    ffedata_arraysize_ = ffebld_constant_integerdefault (ffebld_conter
								 (size));
	  }
	ffedata_expected_ = arrayref ? 1 : ffedata_arraysize_;
	ffedata_number_ = 0;
	ffedata_offset_ = arrayref ? ffedata_eval_offset_ (ffebld_right
		(ffebld_left (next)), ffesymbol_dims (ffedata_symbol_)) : 0;
	ffedata_size_ = ffesymbol_size (ffedata_symbol_);
	ffedata_symbolsize_ = ffedata_size_ * ffedata_arraysize_;
	ffedata_charnumber_ = 0;
	ffedata_charoffset_ = ffedata_eval_substr_begin_ (ffebld_head (colon));
	ffedata_charexpected_ = ffedata_eval_substr_end_ (ffebld_head
				(ffebld_trail (colon)), ffedata_charoffset_,
				   ffedata_size_) - ffedata_charoffset_ + 1;
      }
      break;

    case FFEBLD_opIMPDO:	/* Implied-DO construct. */
      {
	ffebld itervar;
	ffebld start;
	ffebld end;
	ffebld incr;
	ffebld item = ffebld_right (next);

	itervar = ffebld_head (item);
	item = ffebld_trail (item);
	start = ffebld_head (item);
	item = ffebld_trail (item);
	end = ffebld_head (item);
	item = ffebld_trail (item);
	incr = ffebld_head (item);

	ffedata_push_ ();
	ffedata_stack_->outer_list = ffedata_list_;
	ffedata_stack_->my_list = ffedata_list_ = ffebld_left (next);

	assert (ffeinfo_basictype (ffebld_info (itervar))
		== FFEINFO_basictypeINTEGER);
	assert (ffeinfo_kindtype (ffebld_info (itervar))
		== FFEINFO_kindtypeINTEGERDEFAULT);
	ffedata_stack_->itervar = ffebld_symter (itervar);

	assert (ffeinfo_basictype (ffebld_info (start))
		== FFEINFO_basictypeINTEGER);
	assert (ffeinfo_kindtype (ffebld_info (start))
		== FFEINFO_kindtypeINTEGERDEFAULT);
	ffesymbol_set_value (ffedata_stack_->itervar, ffedata_eval_integer1_ (start));

	assert (ffeinfo_basictype (ffebld_info (end))
		== FFEINFO_basictypeINTEGER);
	assert (ffeinfo_kindtype (ffebld_info (end))
		== FFEINFO_kindtypeINTEGERDEFAULT);
	ffedata_stack_->final = ffedata_eval_integer1_ (end);

	if (incr == NULL)
	  ffedata_stack_->increment = 1;
	else
	  {
	    assert (ffeinfo_basictype (ffebld_info (incr))
		    == FFEINFO_basictypeINTEGER);
	    assert (ffeinfo_kindtype (ffebld_info (incr))
		    == FFEINFO_kindtypeINTEGERDEFAULT);
	    ffedata_stack_->increment = ffedata_eval_integer1_ (incr);
	    if (ffedata_stack_->increment == 0)
	      {
		ffebad_start (FFEBAD_DATA_ZERO);
		ffest_ffebad_here_current_stmt (0);
		ffebad_string (ffesymbol_text (ffedata_stack_->itervar));
		ffebad_finish ();
		ffedata_pop_ ();
		ffedata_reported_error_ = TRUE;
		return FALSE;
	      }
	  }

	if ((ffedata_stack_->increment > 0)
	    ? ffesymbol_value (ffedata_stack_->itervar)
	    > ffedata_stack_->final
	    : ffesymbol_value (ffedata_stack_->itervar)
	    < ffedata_stack_->final)
	  {
	    ffedata_reported_error_ = TRUE;
	    ffebad_start (FFEBAD_DATA_EMPTY);
	    ffest_ffebad_here_current_stmt (0);
	    ffebad_string (ffesymbol_text (ffedata_stack_->itervar));
	    ffebad_finish ();
	    ffedata_pop_ ();
	    return FALSE;
	  }
      }
      goto tail_recurse;	/* :::::::::::::::::::: */

    case FFEBLD_opANY:
      ffedata_reported_error_ = TRUE;
      return FALSE;

    default:
      assert ("bad op" == NULL);
      break;
    }

  return TRUE;
}

/* ffedata_convert_ -- Convert source expression to given type using cache

   ffebld source;
   ffelexToken source_token;
   ffelexToken dest_token;  // Any appropriate token for "destination".
   ffeinfoBasictype bt;
   ffeinfoKindtype kt;
   ffetargetCharactersize sz;
   source = ffedata_convert_(source,source_token,dest_token,bt,kt,sz);

   Like ffeexpr_convert, but calls it only if necessary (if the converted
   expression doesn't already exist in the cache) and then puts the result
   in the cache.  */

static ffebld
ffedata_convert_ (ffebld source, ffelexToken source_token,
		  ffelexToken dest_token, ffeinfoBasictype bt,
		  ffeinfoKindtype kt, ffeinfoRank rk,
		  ffetargetCharacterSize sz)
{
  ffebld converted;
  int i;
  int max;
  ffedataConvertCache_ cache;

  for (i = 0; i < ffedata_convert_cache_use_; ++i)
    if ((bt == ffedata_convert_cache_[i].basic_type)
	&& (kt == ffedata_convert_cache_[i].kind_type)
	&& (sz == ffedata_convert_cache_[i].size)
	&& (rk == ffedata_convert_cache_[i].rank))
      return ffedata_convert_cache_[i].converted;

  converted = ffeexpr_convert (source, source_token, dest_token, bt, kt, rk,
			       sz, FFEEXPR_contextDATA);

  if (ffedata_convert_cache_use_ >= ffedata_convert_cache_max_)
    {
      if (ffedata_convert_cache_max_ == 0)
	max = 4;
      else
	max = ffedata_convert_cache_max_ << 1;

      if (max > ffedata_convert_cache_max_)
	{
	  cache = (ffedataConvertCache_) malloc_new_ks (malloc_pool_image (),
				    "FFEDATA cache", max * sizeof (*cache));
	  if (ffedata_convert_cache_max_ != 0)
	    {
	      memcpy (cache, ffedata_convert_cache_,
		      ffedata_convert_cache_max_ * sizeof (*cache));
	      malloc_kill_ks (malloc_pool_image (), ffedata_convert_cache_,
			      ffedata_convert_cache_max_ * sizeof (*cache));
	    }
	  ffedata_convert_cache_ = cache;
	  ffedata_convert_cache_max_ = max;
	}
      else
	return converted;	/* In case int overflows! */
    }

  i = ffedata_convert_cache_use_++;

  ffedata_convert_cache_[i].converted = converted;
  ffedata_convert_cache_[i].basic_type = bt;
  ffedata_convert_cache_[i].kind_type = kt;
  ffedata_convert_cache_[i].size = sz;
  ffedata_convert_cache_[i].rank = rk;

  return converted;
}

/* ffedata_eval_integer1_ -- Evaluate expression

   ffetargetIntegerDefault result;
   ffebld expr;	 // must be kindtypeINTEGER1.

   result = ffedata_eval_integer1_(expr);

   Evalues the expression (which yields a kindtypeINTEGER1 result) and
   returns the result.	*/

static ffetargetIntegerDefault
ffedata_eval_integer1_ (ffebld expr)
{
  ffetargetInteger1 result;
  ffebad error;

  assert (expr != NULL);

  switch (ffebld_op (expr))
    {
    case FFEBLD_opCONTER:
      return ffebld_constant_integer1 (ffebld_conter (expr));

    case FFEBLD_opSYMTER:
      return ffesymbol_value (ffebld_symter (expr));

    case FFEBLD_opUPLUS:
      return ffedata_eval_integer1_ (ffebld_left (expr));

    case FFEBLD_opUMINUS:
      error = ffetarget_uminus_integer1 (&result,
			       ffedata_eval_integer1_ (ffebld_left (expr)));
      break;

    case FFEBLD_opADD:
      error = ffetarget_add_integer1 (&result,
				ffedata_eval_integer1_ (ffebld_left (expr)),
			      ffedata_eval_integer1_ (ffebld_right (expr)));
      break;

    case FFEBLD_opSUBTRACT:
      error = ffetarget_subtract_integer1 (&result,
				ffedata_eval_integer1_ (ffebld_left (expr)),
			      ffedata_eval_integer1_ (ffebld_right (expr)));
      break;

    case FFEBLD_opMULTIPLY:
      error = ffetarget_multiply_integer1 (&result,
				ffedata_eval_integer1_ (ffebld_left (expr)),
			      ffedata_eval_integer1_ (ffebld_right (expr)));
      break;

    case FFEBLD_opDIVIDE:
      error = ffetarget_divide_integer1 (&result,
				ffedata_eval_integer1_ (ffebld_left (expr)),
			      ffedata_eval_integer1_ (ffebld_right (expr)));
      break;

    case FFEBLD_opPOWER:
      {
	ffebld r = ffebld_right (expr);

	if ((ffeinfo_basictype (ffebld_info (r)) != FFEINFO_basictypeINTEGER)
	    || (ffeinfo_kindtype (ffebld_info (r)) != FFEINFO_kindtypeINTEGERDEFAULT))
	  error = FFEBAD_DATA_EVAL;
	else
	  error = ffetarget_power_integerdefault_integerdefault (&result,
				ffedata_eval_integer1_ (ffebld_left (expr)),
						ffedata_eval_integer1_ (r));
      }
      break;

#if 0				/* Only for character basictype. */
    case FFEBLD_opCONCATENATE:
      error =;
      break;
#endif

    case FFEBLD_opNOT:
      error = ffetarget_not_integer1 (&result,
			       ffedata_eval_integer1_ (ffebld_left (expr)));
      break;

#if 0				/* Only for logical basictype. */
    case FFEBLD_opLT:
      error =;
      break;

    case FFEBLD_opLE:
      error =;
      break;

    case FFEBLD_opEQ:
      error =;
      break;

    case FFEBLD_opNE:
      error =;
      break;

    case FFEBLD_opGT:
      error =;
      break;

    case FFEBLD_opGE:
      error =;
      break;
#endif

    case FFEBLD_opAND:
      error = ffetarget_and_integer1 (&result,
				ffedata_eval_integer1_ (ffebld_left (expr)),
			      ffedata_eval_integer1_ (ffebld_right (expr)));
      break;

    case FFEBLD_opOR:
      error = ffetarget_or_integer1 (&result,
				ffedata_eval_integer1_ (ffebld_left (expr)),
			      ffedata_eval_integer1_ (ffebld_right (expr)));
      break;

    case FFEBLD_opXOR:
      error = ffetarget_xor_integer1 (&result,
				ffedata_eval_integer1_ (ffebld_left (expr)),
			      ffedata_eval_integer1_ (ffebld_right (expr)));
      break;

    case FFEBLD_opEQV:
      error = ffetarget_eqv_integer1 (&result,
				ffedata_eval_integer1_ (ffebld_left (expr)),
			      ffedata_eval_integer1_ (ffebld_right (expr)));
      break;

    case FFEBLD_opNEQV:
      error = ffetarget_neqv_integer1 (&result,
				ffedata_eval_integer1_ (ffebld_left (expr)),
			      ffedata_eval_integer1_ (ffebld_right (expr)));
      break;

    case FFEBLD_opPAREN:
      return ffedata_eval_integer1_ (ffebld_left (expr));

#if 0				/* ~~ no idea how to do this */
    case FFEBLD_opPERCENT_LOC:
      error =;
      break;
#endif

#if 0				/* not allowed by ANSI, but perhaps as an
				   extension someday? */
    case FFEBLD_opCONVERT:
      switch (ffeinfo_basictype (ffebld_info (ffebld_left (expr))))
	{
	case FFEINFO_basictypeINTEGER:
	  switch (ffeinfo_kindtype (ffebld_info (ffebld_left (expr))))
	    {
	    default:
	      error = FFEBAD_DATA_EVAL;
	      break;
	    }
	  break;

	case FFEINFO_basictypeREAL:
	  switch (ffeinfo_kindtype (ffebld_info (ffebld_left (expr))))
	    {
	    default:
	      error = FFEBAD_DATA_EVAL;
	      break;
	    }
	  break;
	}
      break;
#endif

#if 0				/* not valid ops */
    case FFEBLD_opREPEAT:
      error =;
      break;

    case FFEBLD_opBOUNDS:
      error =;
      break;
#endif

#if 0				/* not allowed by ANSI, but perhaps as an
				   extension someday? */
    case FFEBLD_opFUNCREF:
      error =;
      break;
#endif

#if 0				/* not valid ops */
    case FFEBLD_opSUBRREF:
      error =;
      break;

    case FFEBLD_opARRAYREF:
      error =;
      break;
#endif

#if 0				/* not valid for integer1 */
    case FFEBLD_opSUBSTR:
      error =;
      break;
#endif

    default:
      error = FFEBAD_DATA_EVAL;
      break;
    }

  if (error != FFEBAD)
    {
      ffebad_start (error);
      ffest_ffebad_here_current_stmt (0);
      ffebad_finish ();
      result = 0;
    }

  return result;
}

/* ffedata_eval_offset_ -- Evaluate offset info array

   ffetargetOffset offset;  // 0...max-1.
   ffebld subscripts;  // an opITEM list of subscript exprs.
   ffebld dims;	 // an opITEM list of opBOUNDS exprs.

   result = ffedata_eval_offset_(expr);

   Evalues the expression (which yields a kindtypeINTEGER1 result) and
   returns the result.	*/

static ffetargetOffset
ffedata_eval_offset_ (ffebld subscripts, ffebld dims)
{
  ffetargetIntegerDefault offset = 0;
  ffetargetIntegerDefault width = 1;
  ffetargetIntegerDefault value;
  ffetargetIntegerDefault lowbound;
  ffetargetIntegerDefault highbound;
  ffetargetOffset final;
  ffebld subscript;
  ffebld dim;
  ffebld low;
  ffebld high;
  int rank = 0;
  bool ok;

  while (subscripts != NULL)
    {
      ++rank;
      assert (dims != NULL);

      subscript = ffebld_head (subscripts);
      dim = ffebld_head (dims);

      assert (ffeinfo_basictype (ffebld_info (subscript)) == FFEINFO_basictypeINTEGER);
      assert (ffeinfo_kindtype (ffebld_info (subscript)) == FFEINFO_kindtypeINTEGER1);
      value = ffedata_eval_integer1_ (subscript);

      assert (ffebld_op (dim) == FFEBLD_opBOUNDS);
      low = ffebld_left (dim);
      high = ffebld_right (dim);

      if (low == NULL)
	lowbound = 1;
      else
	{
	  assert (ffeinfo_basictype (ffebld_info (low)) == FFEINFO_basictypeINTEGER);
	  assert (ffeinfo_kindtype (ffebld_info (low)) == FFEINFO_kindtypeINTEGERDEFAULT);
	  lowbound = ffedata_eval_integer1_ (low);
	}

      assert (ffeinfo_basictype (ffebld_info (high)) == FFEINFO_basictypeINTEGER);
      assert (ffeinfo_kindtype (ffebld_info (high)) == FFEINFO_kindtypeINTEGERDEFAULT);
      highbound = ffedata_eval_integer1_ (high);

      if ((value < lowbound) || (value > highbound))
	{
	  char rankstr[10];

	  sprintf (rankstr, "%d", rank);
	  value = lowbound;
	  ffebad_start (FFEBAD_DATA_SUBSCRIPT);
	  ffebad_string (ffesymbol_text (ffedata_symbol_));
	  ffebad_string (rankstr);
	  ffebad_finish ();
	}

      subscripts = ffebld_trail (subscripts);
      dims = ffebld_trail (dims);

      offset += width * (value - lowbound);
      if (subscripts != NULL)
	width *= highbound - lowbound + 1;
    }

  assert (dims == NULL);

  ok = ffetarget_offset (&final, offset);
  assert (ok);

  return final;
}

/* ffedata_eval_substr_begin_ -- Evaluate begin-point of substr reference

   ffetargetCharacterSize beginpoint;
   ffebld endval;  // head(colon).

   beginpoint = ffedata_eval_substr_end_(endval);

   If beginval is NULL, returns 0.  Otherwise makes sure beginval is
   kindtypeINTEGERDEFAULT, makes sure its value is > 0,
   and returns its value minus one, or issues an error message.	 */

static ffetargetCharacterSize
ffedata_eval_substr_begin_ (ffebld expr)
{
  ffetargetIntegerDefault val;

  if (expr == NULL)
    return 0;

  assert (ffeinfo_basictype (ffebld_info (expr)) == FFEINFO_basictypeINTEGER);
  assert (ffeinfo_kindtype (ffebld_info (expr)) == FFEINFO_kindtypeINTEGERDEFAULT);

  val = ffedata_eval_integer1_ (expr);

  if (val < 1)
    {
      val = 1;
      ffebad_start (FFEBAD_DATA_RANGE);
      ffest_ffebad_here_current_stmt (0);
      ffebad_string (ffesymbol_text (ffedata_symbol_));
      ffebad_finish ();
      ffedata_reported_error_ = TRUE;
    }

  return val - 1;
}

/* ffedata_eval_substr_end_ -- Evaluate end-point of substr reference

   ffetargetCharacterSize endpoint;
   ffebld endval;  // head(trail(colon)).
   ffetargetCharacterSize min;	// beginpoint of substr reference.
   ffetargetCharacterSize max;	// size of entity.

   endpoint = ffedata_eval_substr_end_(endval,dflt);

   If endval is NULL, returns max.  Otherwise makes sure endval is
   kindtypeINTEGERDEFAULT, makes sure its value is > min and <= max,
   and returns its value minus one, or issues an error message.	 */

static ffetargetCharacterSize
ffedata_eval_substr_end_ (ffebld expr, ffetargetCharacterSize min,
			  ffetargetCharacterSize max)
{
  ffetargetIntegerDefault val;

  if (expr == NULL)
    return max - 1;

  assert (ffeinfo_basictype (ffebld_info (expr)) == FFEINFO_basictypeINTEGER);
  assert (ffeinfo_kindtype (ffebld_info (expr)) == FFEINFO_kindtypeINTEGER1);

  val = ffedata_eval_integer1_ (expr);

  if ((val < (ffetargetIntegerDefault) min)
      || (val > (ffetargetIntegerDefault) max))
    {
      val = 1;
      ffebad_start (FFEBAD_DATA_RANGE);
      ffest_ffebad_here_current_stmt (0);
      ffebad_string (ffesymbol_text (ffedata_symbol_));
      ffebad_finish ();
      ffedata_reported_error_ = TRUE;
    }

  return val - 1;
}

/* ffedata_gather_ -- Gather initial values for sym into master sym inits

   ffestorag mst;  // A typeCBLOCK or typeLOCAL aggregate.
   ffestorag st;  // A typeCOMMON or typeEQUIV member.
   ffedata_gather_(mst,st);

   If st has any initialization info, transfer that info into mst and
   clear st's info.  */

static void
ffedata_gather_ (ffestorag mst, ffestorag st)
{
  ffesymbol s;
  ffesymbol s_whine;		/* Symbol to complain about in diagnostics. */
  ffebld b;
  ffetargetOffset offset;
  ffetargetOffset units_expected;
  ffebitCount actual;
  ffebldConstantArray array;
  ffebld accter;
  ffetargetCopyfunc fn;
  void *ptr1;
  void *ptr2;
  size_t size;
  ffeinfoBasictype bt;
  ffeinfoKindtype kt;
  ffeinfoBasictype ign_bt;
  ffeinfoKindtype ign_kt;
  ffetargetAlign units;
  ffebit bits;
  ffetargetOffset source_offset;
  bool whine = FALSE;

  if (st == NULL)
    return;			/* Nothing to do. */

  s = ffestorag_symbol (st);

  assert (s != NULL);		/* Must have a corresponding symbol (else how
				   inited?). */
  assert (ffestorag_init (st) == NULL);	/* No init info on storage itself. */
  assert (ffestorag_accretion (st) == NULL);

  if ((((b = ffesymbol_init (s)) == NULL)
       && ((b = ffesymbol_accretion (s)) == NULL))
      || (ffebld_op (b) == FFEBLD_opANY)
      || ((ffebld_op (b) == FFEBLD_opCONVERT)
	  && (ffebld_op (ffebld_left (b)) == FFEBLD_opANY)))
    return;			/* Nothing to do. */

  /* b now holds the init/accretion expr. */

  ffesymbol_set_init (s, NULL);
  ffesymbol_set_accretion (s, NULL);
  ffesymbol_set_accretes (s, 0);

  s_whine = ffestorag_symbol (mst);
  if (s_whine == NULL)
    s_whine = s;

  /* Make sure we haven't fully accreted during an array init. */

  if (ffestorag_init (mst) != NULL)
    {
      ffebad_start (FFEBAD_DATA_MULTIPLE);
      ffebad_here (0, ffewhere_line_unknown (), ffewhere_column_unknown ());
      ffebad_string (ffesymbol_text (s_whine));
      ffebad_finish ();
      return;
    }

  bt = ffeinfo_basictype (ffebld_info (b));
  kt = ffeinfo_kindtype (ffebld_info (b));

  /* Calculate offset for aggregate area. */

  ffedata_charexpected_ = (bt == FFEINFO_basictypeCHARACTER)
    ? ffebld_size (b) : 1;
  ffetarget_aggregate_info (&ign_bt, &ign_kt, &units, bt,
			    kt);/* Find out unit size of source datum. */
  assert (units % ffedata_storage_units_ == 0);
  units_expected = ffedata_charexpected_ * units / ffedata_storage_units_;
  offset = (ffestorag_offset (st) - ffestorag_offset (mst))
    / ffedata_storage_units_;

  /* Does an accretion array exist?  If not, create it. */

  if (ffestorag_accretion (mst) == NULL)
    {
#if FFEDATA_sizeTOO_BIG_INIT_ != 0
      if (ffedata_storage_size_ >= FFEDATA_sizeTOO_BIG_INIT_)
	{
	  char bignum[40];

	  sprintf (&bignum[0], "%ld", (long) ffedata_storage_size_);
	  ffebad_start (FFEBAD_TOO_BIG_INIT);
	  ffebad_here (0, ffesymbol_where_line (s_whine),
		       ffesymbol_where_column (s_whine));
	  ffebad_string (ffesymbol_text (s_whine));
	  ffebad_string (bignum);
	  ffebad_finish ();
	}
#endif
      array = ffebld_constantarray_new (ffedata_storage_bt_,
				ffedata_storage_kt_, ffedata_storage_size_);
      accter = ffebld_new_accter (array, ffebit_new (ffe_pool_program_unit (),
						     ffedata_storage_size_));
      ffebld_set_info (accter, ffeinfo_new
		       (ffedata_storage_bt_,
			ffedata_storage_kt_,
			1,
			FFEINFO_kindENTITY,
			FFEINFO_whereCONSTANT,
			(ffedata_basictype_ == FFEINFO_basictypeCHARACTER)
			? 1 : FFETARGET_charactersizeNONE));
      ffestorag_set_accretion (mst, accter);
      ffestorag_set_accretes (mst, ffedata_storage_size_);
    }
  else
    {
      accter = ffestorag_accretion (mst);
      assert (ffedata_storage_size_ == (ffetargetOffset) ffebld_accter_size (accter));
      array = ffebld_accter (accter);
    }

  /* Put value in accretion array at desired offset. */

  fn = ffetarget_aggregate_ptr_memcpy (ffedata_storage_bt_, ffedata_storage_kt_,
				       bt, kt);

  switch (ffebld_op (b))
    {
    case FFEBLD_opCONTER:
      ffebld_constantarray_prepare (&ptr1, &ptr2, &size, array, ffedata_storage_bt_,
				    ffedata_storage_kt_, offset,
			   ffebld_constant_ptr_to_union (ffebld_conter (b)),
				    bt, kt);
      (*fn) (ptr1, ptr2, size);	/* Does the appropriate memcpy-like
				   operation. */
      ffebit_count (ffebld_accter_bits (accter),
		    offset, FALSE, units_expected, &actual);	/* How many FALSE? */
      if (units_expected != (ffetargetOffset) actual)
	{
	  ffebad_start (FFEBAD_DATA_MULTIPLE);
	  ffebad_here (0, ffewhere_line_unknown (), ffewhere_column_unknown ());
	  ffebad_string (ffesymbol_text (s));
	  ffebad_finish ();
	}
      ffestorag_set_accretes (mst,
			      ffestorag_accretes (mst)
			      - actual);	/* Decrement # of values
						   actually accreted. */
      ffebit_set (ffebld_accter_bits (accter), offset, 1, units_expected);

      /* If done accreting for this storage area, establish as initialized. */

      if (ffestorag_accretes (mst) == 0)
	{
	  ffestorag_set_init (mst, accter);
	  ffestorag_set_accretion (mst, NULL);
	  ffebit_kill (ffebld_accter_bits (ffestorag_init (mst)));
	  ffebld_set_op (ffestorag_init (mst), FFEBLD_opARRTER);
	  ffebld_set_arrter (ffestorag_init (mst),
			     ffebld_accter (ffestorag_init (mst)));
	  ffebld_arrter_set_size (ffestorag_init (mst),
				  ffedata_storage_size_);
	  ffebld_arrter_set_pad (ffestorag_init (mst), 0);
	  ffecom_notify_init_storage (mst);
	}

      return;

    case FFEBLD_opARRTER:
      ffebld_constantarray_preparray (&ptr1, &ptr2, &size, array, ffedata_storage_bt_,
			     ffedata_storage_kt_, offset, ffebld_arrter (b),
				      bt, kt);
      size *= ffebld_arrter_size (b);
      units_expected *= ffebld_arrter_size (b);
      (*fn) (ptr1, ptr2, size);	/* Does the appropriate memcpy-like
				   operation. */
      ffebit_count (ffebld_accter_bits (accter),
		    offset, FALSE, units_expected, &actual);	/* How many FALSE? */
      if (units_expected != (ffetargetOffset) actual)
	{
	  ffebad_start (FFEBAD_DATA_MULTIPLE);
	  ffebad_here (0, ffewhere_line_unknown (), ffewhere_column_unknown ());
	  ffebad_string (ffesymbol_text (s));
	  ffebad_finish ();
	}
      ffestorag_set_accretes (mst,
			      ffestorag_accretes (mst)
			      - actual);	/* Decrement # of values
						   actually accreted. */
      ffebit_set (ffebld_accter_bits (accter), offset, 1, units_expected);

      /* If done accreting for this storage area, establish as initialized. */

      if (ffestorag_accretes (mst) == 0)
	{
	  ffestorag_set_init (mst, accter);
	  ffestorag_set_accretion (mst, NULL);
	  ffebit_kill (ffebld_accter_bits (ffestorag_init (mst)));
	  ffebld_set_op (ffestorag_init (mst), FFEBLD_opARRTER);
	  ffebld_set_arrter (ffestorag_init (mst),
			     ffebld_accter (ffestorag_init (mst)));
	  ffebld_arrter_set_size (ffestorag_init (mst),
				  ffedata_storage_size_);
	  ffebld_arrter_set_pad (ffestorag_init (mst), 0);
	  ffecom_notify_init_storage (mst);
	}

      return;

    case FFEBLD_opACCTER:
      ffebld_constantarray_preparray (&ptr1, &ptr2, &size, array, ffedata_storage_bt_,
			     ffedata_storage_kt_, offset, ffebld_accter (b),
				      bt, kt);
      bits = ffebld_accter_bits (b);
      source_offset = 0;

      for (;;)
	{
	  ffetargetOffset unexp;
	  ffetargetOffset siz;
	  ffebitCount length;
	  bool value;

	  ffebit_test (bits, source_offset, &value, &length);
	  if (length == 0)
	    break;		/* Exit the loop early. */
	  siz = size * length;
	  unexp = units_expected * length;
	  if (value)
	    {
	      (*fn) (ptr1, ptr2, siz);	/* Does memcpy-like operation. */
	      ffebit_count (ffebld_accter_bits (accter),	/* How many FALSE? */
			    offset, FALSE, unexp, &actual);
	      if (!whine && (unexp != (ffetargetOffset) actual))
		{
		  whine = TRUE;	/* Don't whine more than once for one gather. */
		  ffebad_start (FFEBAD_DATA_MULTIPLE);
		  ffebad_here (0, ffewhere_line_unknown (), ffewhere_column_unknown ());
		  ffebad_string (ffesymbol_text (s));
		  ffebad_finish ();
		}
	      ffestorag_set_accretes (mst,
				      ffestorag_accretes (mst)
				      - actual);	/* Decrement # of values
							   actually accreted. */
	      ffebit_set (ffebld_accter_bits (accter), offset, 1, unexp);
	    }
	  source_offset += length;
	  offset += unexp;
	  ptr1 = ((char *) ptr1) + siz;
	  ptr2 = ((char *) ptr2) + siz;
	}

      /* If done accreting for this storage area, establish as initialized. */

      if (ffestorag_accretes (mst) == 0)
	{
	  ffestorag_set_init (mst, accter);
	  ffestorag_set_accretion (mst, NULL);
	  ffebit_kill (ffebld_accter_bits (ffestorag_init (mst)));
	  ffebld_set_op (ffestorag_init (mst), FFEBLD_opARRTER);
	  ffebld_set_arrter (ffestorag_init (mst),
			     ffebld_accter (ffestorag_init (mst)));
	  ffebld_arrter_set_size (ffestorag_init (mst),
				  ffedata_storage_size_);
	  ffebld_arrter_set_pad (ffestorag_init (mst), 0);
	  ffecom_notify_init_storage (mst);
	}

      return;

    default:
      assert ("bad init op in gather_" == NULL);
      return;
    }
}

/* ffedata_pop_ -- Pop an impdo stack entry

   ffedata_pop_();  */

static void
ffedata_pop_ ()
{
  ffedataImpdo_ victim = ffedata_stack_;

  assert (victim != NULL);

  ffedata_stack_ = ffedata_stack_->outer;

  malloc_kill_ks (ffe_pool_program_unit (), victim, sizeof (*victim));
}

/* ffedata_push_ -- Push an impdo stack entry

   ffedata_push_();  */

static void
ffedata_push_ ()
{
  ffedataImpdo_ baby;

  baby = malloc_new_ks (ffe_pool_program_unit (), "ffedataImpdo_", sizeof (*baby));

  baby->outer = ffedata_stack_;
  ffedata_stack_ = baby;
}

/* ffedata_value_ -- Provide an initial value

   ffebld value;
   ffelexToken t;  // Points to the value.
   if (ffedata_value(value,t))
       // Everything's ok

   Makes sure the value is ok, then remembers it according to the list
   provided to ffedata_begin.  */

static bool
ffedata_value_ (ffebld value, ffelexToken token)
{

  /* If already reported an error, don't do anything. */

  if (ffedata_reported_error_)
    return FALSE;

  /* If the value is an error marker, remember we've seen one and do nothing
     else. */

  if ((value != NULL)
      && (ffebld_op (value) == FFEBLD_opANY))
    {
      ffedata_reported_error_ = TRUE;
      return FALSE;
    }

  /* If too many values (no more targets), complain. */

  if (ffedata_symbol_ == NULL)
    {
      ffebad_start (FFEBAD_DATA_TOOMANY);
      ffebad_here (0, ffelex_token_where_line (token),
		   ffelex_token_where_column (token));
      ffebad_finish ();
      ffedata_reported_error_ = TRUE;
      return FALSE;
    }

  /* If ffedata_advance_ wanted to register a complaint, do it now
     that we have the token to point at instead of just the start
     of the whole statement.  */

  if (ffedata_reinit_)
    {
      ffebad_start (FFEBAD_DATA_REINIT);
      ffebad_here (0, ffelex_token_where_line (token),
		   ffelex_token_where_column (token));
      ffebad_string (ffesymbol_text (ffedata_symbol_));
      ffebad_finish ();
      ffedata_reported_error_ = TRUE;
      return FALSE;
    }

#if FFEGLOBAL_ENABLED
  if (ffesymbol_common (ffedata_symbol_) != NULL)
    ffeglobal_init_common (ffesymbol_common (ffedata_symbol_), token);
#endif

  /* Convert value to desired type. */

  if (value != NULL)
    {
      if (ffedata_convert_cache_use_ == -1)
	value = ffeexpr_convert
	  (value, token, NULL, ffedata_basictype_,
	   ffedata_kindtype_, 0,
	   (ffedata_basictype_ == FFEINFO_basictypeCHARACTER)
	   ? ffedata_charexpected_ : FFETARGET_charactersizeNONE,
	   FFEEXPR_contextDATA);
      else				/* Use the cache. */
	value = ffedata_convert_
	  (value, token, NULL, ffedata_basictype_,
	   ffedata_kindtype_, 0,
	   (ffedata_basictype_ == FFEINFO_basictypeCHARACTER)
	   ? ffedata_charexpected_ : FFETARGET_charactersizeNONE);
    }

  /* If we couldn't, bug out. */

  if ((value != NULL) && (ffebld_op (value) == FFEBLD_opANY))
    {
      ffedata_reported_error_ = TRUE;
      return FALSE;
    }

  /* Handle the case where initializes go to a parent's storage area. */

  if (ffedata_storage_ != NULL)
    {
      ffetargetOffset offset;
      ffetargetOffset units_expected;
      ffebitCount actual;
      ffebldConstantArray array;
      ffebld accter;
      ffetargetCopyfunc fn;
      void *ptr1;
      void *ptr2;
      size_t size;
      ffeinfoBasictype ign_bt;
      ffeinfoKindtype ign_kt;
      ffetargetAlign units;

      /* Make sure we haven't fully accreted during an array init. */

      if (ffestorag_init (ffedata_storage_) != NULL)
	{
	  ffebad_start (FFEBAD_DATA_MULTIPLE);
	  ffebad_here (0, ffelex_token_where_line (token),
		       ffelex_token_where_column (token));
	  ffebad_string (ffesymbol_text (ffedata_symbol_));
	  ffebad_finish ();
	  ffedata_reported_error_ = TRUE;
	  return FALSE;
	}

      /* Calculate offset. */

      offset = ffedata_offset_ * ffedata_size_ + ffedata_charoffset_;

      /* Is offset within range?  If not, whine, but don't do anything else. */

      if (offset + ffedata_charexpected_ - 1 > ffedata_symbolsize_)
	{
	  ffebad_start (FFEBAD_DATA_RANGE);
	  ffest_ffebad_here_current_stmt (0);
	  ffebad_string (ffesymbol_text (ffedata_symbol_));
	  ffebad_finish ();
	  ffedata_reported_error_ = TRUE;
	  return FALSE;
	}

      /* Now calculate offset for aggregate area. */

      ffetarget_aggregate_info (&ign_bt, &ign_kt, &units, ffedata_basictype_,
				ffedata_kindtype_);	/* Find out unit size of
							   source datum. */
      assert (units % ffedata_storage_units_ == 0);
      units_expected = ffedata_charexpected_ * units / ffedata_storage_units_;
      offset *= units / ffedata_storage_units_;
      offset += (ffestorag_offset (ffesymbol_storage (ffedata_symbol_))
		 - ffestorag_offset (ffedata_storage_))
	/ ffedata_storage_units_;

      assert (offset + units_expected - 1 <= ffedata_storage_size_);

      /* Does an accretion array exist?	 If not, create it. */

      if (value != NULL)
	{
	  if (ffestorag_accretion (ffedata_storage_) == NULL)
	    {
#if FFEDATA_sizeTOO_BIG_INIT_ != 0
	      if (ffedata_storage_size_ >= FFEDATA_sizeTOO_BIG_INIT_)
		{
		  char bignum[40];

		  sprintf (&bignum[0], "%ld", (long) ffedata_storage_size_);
		  ffebad_start (FFEBAD_TOO_BIG_INIT);
		  ffebad_here (0, ffelex_token_where_line (token),
			       ffelex_token_where_column (token));
		  ffebad_string (ffesymbol_text (ffedata_symbol_));
		  ffebad_string (bignum);
		  ffebad_finish ();
		}
#endif
	      array = ffebld_constantarray_new
		(ffedata_storage_bt_, ffedata_storage_kt_,
		 ffedata_storage_size_);
	      accter = ffebld_new_accter (array,
					  ffebit_new (ffe_pool_program_unit (),
						      ffedata_storage_size_));
	      ffebld_set_info (accter, ffeinfo_new
			       (ffedata_storage_bt_,
				ffedata_storage_kt_,
				1,
				FFEINFO_kindENTITY,
				FFEINFO_whereCONSTANT,
				(ffedata_basictype_
				 == FFEINFO_basictypeCHARACTER)
				? 1 : FFETARGET_charactersizeNONE));
	      ffestorag_set_accretion (ffedata_storage_, accter);
	      ffestorag_set_accretes (ffedata_storage_, ffedata_storage_size_);
	    }
	  else
	    {
	      accter = ffestorag_accretion (ffedata_storage_);
	      assert (ffedata_storage_size_ == (ffetargetOffset) ffebld_accter_size (accter));
	      array = ffebld_accter (accter);
	    }

	  /* Put value in accretion array at desired offset. */

	  fn = ffetarget_aggregate_ptr_memcpy
	    (ffedata_storage_bt_, ffedata_storage_kt_,
	     ffedata_basictype_, ffedata_kindtype_);
	  ffebld_constantarray_prepare
	    (&ptr1, &ptr2, &size, array, ffedata_storage_bt_,
	     ffedata_storage_kt_, offset,
	     ffebld_constant_ptr_to_union (ffebld_conter (value)),
	     ffedata_basictype_, ffedata_kindtype_);
	  (*fn) (ptr1, ptr2, size);	/* Does the appropriate memcpy-like
					   operation. */
	  ffebit_count (ffebld_accter_bits (accter),
			offset, FALSE, units_expected,
			&actual);	/* How many FALSE? */
	  if (units_expected != (ffetargetOffset) actual)
	    {
	      ffebad_start (FFEBAD_DATA_MULTIPLE);
	      ffebad_here (0, ffelex_token_where_line (token),
			   ffelex_token_where_column (token));
	      ffebad_string (ffesymbol_text (ffedata_symbol_));
	      ffebad_finish ();
	    }
	  ffestorag_set_accretes (ffedata_storage_,
				  ffestorag_accretes (ffedata_storage_)
				  - actual);	/* Decrement # of values
						   actually accreted. */
	  ffebit_set (ffebld_accter_bits (accter), offset,
		      1, units_expected);

	  /* If done accreting for this storage area, establish as
	     initialized. */

	  if (ffestorag_accretes (ffedata_storage_) == 0)
	    {
	      ffestorag_set_init (ffedata_storage_, accter);
	      ffestorag_set_accretion (ffedata_storage_, NULL);
	      ffebit_kill (ffebld_accter_bits
			   (ffestorag_init (ffedata_storage_)));
	      ffebld_set_op (ffestorag_init (ffedata_storage_),
			     FFEBLD_opARRTER);
	      ffebld_set_arrter
		(ffestorag_init (ffedata_storage_),
		 ffebld_accter (ffestorag_init (ffedata_storage_)));
	      ffebld_arrter_set_size (ffestorag_init (ffedata_storage_),
				      ffedata_storage_size_);
	      ffebld_arrter_set_pad (ffestorag_init (ffedata_storage_),
				     0);
	      ffecom_notify_init_storage (ffedata_storage_);
	    }
	}

      /* If still accreting, adjust specs accordingly and return. */

      if (++ffedata_number_ < ffedata_expected_)
	{
	  ++ffedata_offset_;
	  return TRUE;
	}

      return ffedata_advance_ ();
    }

  /* Figure out where the value goes -- in an accretion array or directly
     into the final initial-value slot for the symbol. */

  if ((ffedata_number_ != 0)
      || (ffedata_arraysize_ > 1)
      || (ffedata_charnumber_ != 0)
      || (ffedata_size_ > ffedata_charexpected_))
    {				/* Accrete this value. */
      ffetargetOffset offset;
      ffebitCount actual;
      ffebldConstantArray array;
      ffebld accter = NULL;

      /* Calculate offset. */

      offset = ffedata_offset_ * ffedata_size_ + ffedata_charoffset_;

      /* Is offset within range?  If not, whine, but don't do anything else. */

      if (offset + ffedata_charexpected_ - 1 > ffedata_symbolsize_)
	{
	  ffebad_start (FFEBAD_DATA_RANGE);
	  ffest_ffebad_here_current_stmt (0);
	  ffebad_string (ffesymbol_text (ffedata_symbol_));
	  ffebad_finish ();
	  ffedata_reported_error_ = TRUE;
	  return FALSE;
	}

      /* Does an accretion array exist?	 If not, create it. */

      if (value != NULL)
	{
	  if (ffesymbol_accretion (ffedata_symbol_) == NULL)
	    {
#if FFEDATA_sizeTOO_BIG_INIT_ != 0
	      if (ffedata_symbolsize_ >= FFEDATA_sizeTOO_BIG_INIT_ )
		{
		  char bignum[40];

		  sprintf (&bignum[0], "%ld", (long) ffedata_symbolsize_);
		  ffebad_start (FFEBAD_TOO_BIG_INIT);
		  ffebad_here (0, ffelex_token_where_line (token),
			       ffelex_token_where_column (token));
		  ffebad_string (ffesymbol_text (ffedata_symbol_));
		  ffebad_string (bignum);
		  ffebad_finish ();
		}
#endif
	      array = ffebld_constantarray_new
		(ffedata_basictype_, ffedata_kindtype_,
		 ffedata_symbolsize_);
	      accter = ffebld_new_accter (array,
					  ffebit_new (ffe_pool_program_unit (),
						      ffedata_symbolsize_));
	      ffebld_set_info (accter, ffeinfo_new
			       (ffedata_basictype_,
				ffedata_kindtype_,
				1,
				FFEINFO_kindENTITY,
				FFEINFO_whereCONSTANT,
				(ffedata_basictype_
				 == FFEINFO_basictypeCHARACTER)
				? 1 : FFETARGET_charactersizeNONE));
	      ffesymbol_set_accretion (ffedata_symbol_, accter);
	      ffesymbol_set_accretes (ffedata_symbol_, ffedata_symbolsize_);
	    }
	  else
	    {
	      accter = ffesymbol_accretion (ffedata_symbol_);
	      assert (ffedata_symbolsize_
		      == (ffetargetOffset) ffebld_accter_size (accter));
	      array = ffebld_accter (accter);
	    }

	  /* Put value in accretion array at desired offset. */

	  ffebld_constantarray_put
	    (array, ffedata_basictype_, ffedata_kindtype_,
	     offset, ffebld_constant_union (ffebld_conter (value)));
	  ffebit_count (ffebld_accter_bits (accter), offset, FALSE,
			ffedata_charexpected_,
			&actual);	/* How many FALSE? */
	  if (actual != (unsigned long int) ffedata_charexpected_)
	    {
	      ffebad_start (FFEBAD_DATA_MULTIPLE);
	      ffebad_here (0, ffelex_token_where_line (token),
			   ffelex_token_where_column (token));
	      ffebad_string (ffesymbol_text (ffedata_symbol_));
	      ffebad_finish ();
	    }
	  ffesymbol_set_accretes (ffedata_symbol_,
				  ffesymbol_accretes (ffedata_symbol_)
				  - actual);	/* Decrement # of values
						   actually accreted. */
	  ffebit_set (ffebld_accter_bits (accter), offset,
		      1, ffedata_charexpected_);
	  ffesymbol_signal_unreported (ffedata_symbol_);
	}

      /* If still accreting, adjust specs accordingly and return. */

      if (++ffedata_number_ < ffedata_expected_)
	{
	  ++ffedata_offset_;
	  return TRUE;
	}

      /* Else, if done accreting for this symbol, establish as initialized. */

      if ((value != NULL)
	  && (ffesymbol_accretes (ffedata_symbol_) == 0))
	{
	  ffesymbol_set_init (ffedata_symbol_, accter);
	  ffesymbol_set_accretion (ffedata_symbol_, NULL);
	  ffebit_kill (ffebld_accter_bits (ffesymbol_init (ffedata_symbol_)));
	  ffebld_set_op (ffesymbol_init (ffedata_symbol_), FFEBLD_opARRTER);
	  ffebld_set_arrter (ffesymbol_init (ffedata_symbol_),
			  ffebld_accter (ffesymbol_init (ffedata_symbol_)));
	  ffebld_arrter_set_size (ffesymbol_init (ffedata_symbol_),
				  ffedata_symbolsize_);
	  ffebld_arrter_set_pad (ffestorag_init (ffedata_symbol_), 0);
	  ffecom_notify_init_symbol (ffedata_symbol_);
	}
    }
  else if (value != NULL)
    {
      /* Simple, direct, one-shot assignment. */
      ffesymbol_set_init (ffedata_symbol_, value);
      ffecom_notify_init_symbol (ffedata_symbol_);
    }

  /* Call on advance function to get next target in list. */

  return ffedata_advance_ ();
}
