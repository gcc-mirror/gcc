/* equiv.c -- Implementation File (module.c template V1.0)
   Copyright (C) 1995, 1996, 1997, 1998 Free Software Foundation, Inc.
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
      Handles the EQUIVALENCE relationships in a program unit.

   Modifications:
*/

#define FFEEQUIV_DEBUG 0

/* Include files. */

#include "proj.h"
#include "equiv.h"
#include "bad.h"
#include "bld.h"
#include "com.h"
#include "data.h"
#include "global.h"
#include "lex.h"
#include "malloc.h"
#include "symbol.h"

/* Externals defined here. */


/* Simple definitions and enumerations. */


/* Internal typedefs. */


/* Private include files. */


/* Internal structure definitions. */

struct _ffeequiv_list_
  {
    ffeequiv first;
    ffeequiv last;
  };

/* Static objects accessed by functions in this module. */

static struct _ffeequiv_list_ ffeequiv_list_;

/* Static functions (internal). */

static void ffeequiv_destroy_ (ffeequiv eq);
static void ffeequiv_layout_local_ (ffeequiv eq);
static bool ffeequiv_offset_ (ffetargetOffset *offset, ffesymbol s,
			      ffebld expr, bool subtract,
			      ffetargetOffset adjust, bool no_precede);

/* Internal macros. */


static void
ffeequiv_destroy_ (ffeequiv victim)
{
  ffebld list;
  ffebld item;
  ffebld expr;

  for (list = victim->list; list != NULL; list = ffebld_trail (list))
    {
      for (item = ffebld_head (list); item != NULL; item = ffebld_trail (item))
	{
	  ffesymbol sym;

	  expr = ffebld_head (item);
	  sym = ffeequiv_symbol (expr);
	  if (sym == NULL)
	    continue;
	  if (ffesymbol_equiv (sym) != NULL)
	    ffesymbol_set_equiv (sym, NULL);
	}
    }
  ffeequiv_kill (victim);
}

/* ffeequiv_layout_local_ -- Lay out storage for local equivalenced vars

   ffeequiv eq;
   ffeequiv_layout_local_(eq);

   Makes a single master ffestorag object that contains all the vars
   in the equivalence, and makes subordinate ffestorag objects for the
   vars with the correct offsets.

   The resulting var offsets are relative not necessarily to 0 -- the
   are relative to the offset of the master area, which might be 0 or
   negative, but should never be positive.  */

static void
ffeequiv_layout_local_ (ffeequiv eq)
{
  ffestorag st;			/* Equivalence storage area. */
  ffebld list;			/* List of list of equivalences. */
  ffebld item;			/* List of equivalences. */
  ffebld root_exp;		/* Expression for root sym. */
  ffestorag root_st;		/* Storage for root. */
  ffesymbol root_sym;		/* Root itself. */
  ffebld rooted_exp;		/* Expression for rooted sym in an eqlist. */
  ffestorag rooted_st;		/* Storage for rooted. */
  ffesymbol rooted_sym;		/* Rooted symbol itself. */
  ffetargetOffset eqlist_offset;/* Offset for eqlist from rooted sym. */
  ffetargetAlign alignment;
  ffetargetAlign modulo;
  ffetargetAlign pad;
  ffetargetOffset size;
  ffetargetOffset num_elements;
  bool new_storage;		/* Established new storage info. */
  bool need_storage;		/* Have need for more storage info. */
  bool init;

  assert (eq != NULL);

  if (ffeequiv_common (eq) != NULL)
    {				/* Put in common due to programmer error. */
      ffeequiv_destroy_ (eq);
      return;
    }

  /* Find the symbol for the first valid item in the list of lists, use that
     as the root symbol.  Doesn't matter if it won't end up at the beginning
     of the list, though.  */

#if FFEEQUIV_DEBUG
  fprintf (stderr, "Equiv1:\n");
#endif

  root_sym = NULL;
  root_exp = NULL;

  for (list = ffeequiv_list (eq);
       list != NULL;
       list = ffebld_trail (list))
    {				/* For every equivalence list in the list of
				   equivs */
      for (item = ffebld_head (list);
	   item != NULL;
	   item = ffebld_trail (item))
	{			/* For every equivalence item in the list */
	  ffetargetOffset ign;	/* Ignored. */

	  root_exp = ffebld_head (item);
	  root_sym = ffeequiv_symbol (root_exp);
	  if (root_sym == NULL)
	    continue;		/* Ignore me. */

	  assert (ffesymbol_storage (root_sym) == NULL);	/* No storage yet. */

	  if (!ffeequiv_offset_ (&ign, root_sym, root_exp, FALSE, 0, FALSE))
	    {
	      /* We can't just eliminate this one symbol from the list
		 of candidates, because it might be the only one that
		 ties all these equivs together.  So just destroy the
		 whole list.  */

	      ffeequiv_destroy_ (eq);
	      return;
	    }

	  break;	/* Use first valid eqv expr for root exp/sym. */
	}
      if (root_sym != NULL)
	break;
    }

  if (root_sym == NULL)
    {
      ffeequiv_destroy_ (eq);
      return;
    }


#if FFEEQUIV_DEBUG
  fprintf (stderr, "  Root: `%s'\n", ffesymbol_text (root_sym));
#endif

  /* We've got work to do, so make the LOCAL storage object that'll hold all
     the equivalenced vars inside it. */

  st = ffestorag_new (ffestorag_list_master ());
  ffestorag_set_parent (st, NULL);	/* Initializations happen here. */
  ffestorag_set_init (st, NULL);
  ffestorag_set_accretion (st, NULL);
  ffestorag_set_offset (st, 0);		/* Assume equiv will be at root offset 0 for now. */
  ffestorag_set_alignment (st, 1);
  ffestorag_set_modulo (st, 0);
  ffestorag_set_type (st, FFESTORAG_typeLOCAL);
  ffestorag_set_basictype (st, ffesymbol_basictype (root_sym));
  ffestorag_set_kindtype (st, ffesymbol_kindtype (root_sym));
  ffestorag_set_typesymbol (st, root_sym);
  ffestorag_set_is_save (st, ffeequiv_is_save (eq));
  if (ffesymbol_is_save (root_sym))
    ffestorag_update_save (st);
  ffestorag_set_is_init (st, ffeequiv_is_init (eq));
  if (ffesymbol_is_init (root_sym))
    ffestorag_update_init (st);
  ffestorag_set_symbol (st, root_sym);	/* Assume this will be the root until
					   we know better (used only to generate
					   the internal name for the aggregate area,
					   e.g. for debugging). */

  /* Make the EQUIV storage object for the root symbol. */

  if (ffesymbol_rank (root_sym) == 0)
    num_elements = 1;
  else
    num_elements = ffebld_constant_integerdefault (ffebld_conter
						(ffesymbol_arraysize (root_sym)));
  ffetarget_layout (ffesymbol_text (root_sym), &alignment, &modulo, &size,
		    ffesymbol_basictype (root_sym), ffesymbol_kindtype (root_sym),
		    ffesymbol_size (root_sym), num_elements);
  ffestorag_set_size (st, size);	/* Set initial size of aggregate area. */

  pad = ffetarget_align (ffestorag_ptr_to_alignment (st),
			 ffestorag_ptr_to_modulo (st), 0, alignment,
			 modulo);
  assert (pad == 0);

  root_st = ffestorag_new (ffestorag_list_equivs (st));
  ffestorag_set_parent (root_st, st);	/* Initializations happen there. */
  ffestorag_set_init (root_st, NULL);
  ffestorag_set_accretion (root_st, NULL);
  ffestorag_set_symbol (root_st, root_sym);
  ffestorag_set_size (root_st, size);
  ffestorag_set_offset (root_st, 0);	/* Will not change; always 0 relative to itself! */
  ffestorag_set_alignment (root_st, alignment);
  ffestorag_set_modulo (root_st, modulo);
  ffestorag_set_type (root_st, FFESTORAG_typeEQUIV);
  ffestorag_set_basictype (root_st, ffesymbol_basictype (root_sym));
  ffestorag_set_kindtype (root_st, ffesymbol_kindtype (root_sym));
  ffestorag_set_typesymbol (root_st, root_sym);
  ffestorag_set_is_save (root_st, FALSE);	/* Assume FALSE, then... */
  if (ffestorag_is_save (st))	/* ...update to TRUE if needed. */
    ffestorag_update_save (root_st);
  ffestorag_set_is_init (root_st, FALSE);	/* Assume FALSE, then... */
  if (ffestorag_is_init (st))	/* ...update to TRUE if needed. */
    ffestorag_update_init (root_st);
  ffesymbol_set_storage (root_sym, root_st);
  ffesymbol_signal_unreported (root_sym);
  init = ffesymbol_is_init (root_sym);

  /* Now that we know the root (offset=0) symbol, revisit all the lists and
     do the actual storage allocation.	Keep doing this until we've gone
     through them all without making any new storage objects. */

  do
    {
      new_storage = FALSE;
      need_storage = FALSE;
      for (list = ffeequiv_list (eq);
	   list != NULL;
	   list = ffebld_trail (list))
	{			/* For every equivalence list in the list of
				   equivs */
	  /* Now find a "rooted" symbol in this list.  That is, find the
	     first item we can that is valid and whose symbol already
	     has a storage area, because that means we know where it
	     belongs in the equivalence area and can then allocate the
	     rest of the items in the list accordingly.  */

	  rooted_sym = NULL;
	  rooted_exp = NULL;
	  eqlist_offset = 0;

	  for (item = ffebld_head (list);
	       item != NULL;
	       item = ffebld_trail (item))
	    {			/* For every equivalence item in the list */
	      rooted_exp = ffebld_head (item);
	      rooted_sym = ffeequiv_symbol (rooted_exp);
	      if ((rooted_sym == NULL)
		  || ((rooted_st = ffesymbol_storage (rooted_sym)) == NULL))
		{
		  rooted_sym = NULL;
		  continue;	/* Ignore me. */
		}

	      need_storage = TRUE;	/* Somebody is likely to need
					   storage. */

#if FFEEQUIV_DEBUG
	      fprintf (stderr, "  Rooted: `%s' at %" ffetargetOffset_f "d\n",
		       ffesymbol_text (rooted_sym),
		       ffestorag_offset (rooted_st));
#endif

	      /* The offset of this symbol from the equiv's root symbol
		 is already known, and the size of this symbol is already
		 incorporated in the size of the equiv's aggregate area.
		 What we now determine is the offset of this equivalence
		 _list_ from the equiv's root symbol.

		 For example, if we know that A is at offset 16 from the
		 root symbol, given EQUIVALENCE (B(24),A(2)), we're looking
		 at A(2), meaning that the offset for this equivalence list
		 is 20 (4 bytes beyond the beginning of A, assuming typical
		 array types, dimensions, and type info).  */

	      if (!ffeequiv_offset_ (&eqlist_offset, rooted_sym, rooted_exp, FALSE,
				     ffestorag_offset (rooted_st), FALSE))

		{	/* Can't use this one. */
		  ffesymbol_set_equiv (rooted_sym, NULL);/* Equiv area slated for
							    death. */
		  rooted_sym = NULL;
		  continue;		/* Something's wrong with eqv expr, try another. */
		}

#if FFEEQUIV_DEBUG
	      fprintf (stderr, "  Eqlist offset: %" ffetargetOffset_f "d\n",
		       eqlist_offset);
#endif

	      break;
	    }

	  /* If no rooted symbol, it means this list has no roots -- yet.
	     So, forget this list this time around, but we'll get back
	     to it after the outer loop iterates at least one more time,
	     and, ultimately, it will have a root.  */

	  if (rooted_sym == NULL)
	    {
#if FFEEQUIV_DEBUG
	      fprintf (stderr, "No roots.\n");
#endif
	      continue;
	    }

	  /* We now have a rooted symbol/expr and the offset of this equivalence
	     list from the root symbol.  The other expressions in this
	     list all identify an initial storage unit that must have the
	     same offset. */

	  for (item = ffebld_head (list);
	       item != NULL;
	       item = ffebld_trail (item))
	    {			/* For every equivalence item in the list */
	      ffebld item_exp;			/* Expression for equivalence. */
	      ffestorag item_st;		/* Storage for var. */
	      ffesymbol item_sym;		/* Var itself. */
	      ffetargetOffset item_offset;	/* Offset for var from root. */
	      ffetargetOffset new_size;

	      item_exp = ffebld_head (item);
	      item_sym = ffeequiv_symbol (item_exp);
	      if ((item_sym == NULL)
		  || (ffesymbol_equiv (item_sym) == NULL))
		continue;	/* Ignore me. */

	      if (item_sym == rooted_sym)
		continue;	/* Rooted sym already set up. */

	      if (!ffeequiv_offset_ (&item_offset, item_sym, item_exp, TRUE,
				     eqlist_offset, FALSE))
		{
		  ffesymbol_set_equiv (item_sym, NULL);	/* Don't bother with me anymore. */
		  continue;
		}

#if FFEEQUIV_DEBUG
	      fprintf (stderr, "  Item `%s' at %" ffetargetOffset_f "d",
		       ffesymbol_text (item_sym), item_offset);
#endif

	      if (ffesymbol_rank (item_sym) == 0)
		num_elements = 1;
	      else
		num_elements = ffebld_constant_integerdefault (ffebld_conter
						(ffesymbol_arraysize (item_sym)));
	      ffetarget_layout (ffesymbol_text (item_sym), &alignment, &modulo,
				&size, ffesymbol_basictype (item_sym),
				ffesymbol_kindtype (item_sym), ffesymbol_size (item_sym),
				num_elements);
	      pad = ffetarget_align (ffestorag_ptr_to_alignment (st),
				     ffestorag_ptr_to_modulo (st),
				     item_offset, alignment, modulo);
	      if (pad != 0)
		{
		  ffebad_start (FFEBAD_EQUIV_ALIGN);
		  ffebad_string (ffesymbol_text (item_sym));
		  ffebad_finish ();
		  ffesymbol_set_equiv (item_sym, NULL);	/* Don't bother with me anymore. */
		  continue;
		}

	      /* If the variable's offset is less than the offset for the
		 aggregate storage area, it means it has to expand backwards
		 -- i.e. the new known starting point of the area precedes the
		 old one.  This can't happen with COMMON areas (the standard,
		 and common sense, disallow it), but it is normal for local
		 EQUIVALENCE areas.

		 Also handle choosing the "documented" rooted symbol for this
		 area here.  It's the symbol at the bottom (lowest offset)
		 of the aggregate area, with ties going to the name that would
		 sort to the top of the list of ties.  */

	      if (item_offset == ffestorag_offset (st))
		{
		  if ((item_sym != ffestorag_symbol (st))
		      && (strcmp (ffesymbol_text (item_sym),
				  ffesymbol_text (ffestorag_symbol (st)))
			  < 0))
		    ffestorag_set_symbol (st, item_sym);
		}
	      else if (item_offset < ffestorag_offset (st))
		{
		  /* Increase size of equiv area to start for lower offset
		     relative to root symbol.  */
		  if (! ffetarget_offset_add (&new_size,
					      ffestorag_offset (st)
					      - item_offset,
					      ffestorag_size (st)))
		    ffetarget_offset_overflow (ffesymbol_text (s));
		  else
		    ffestorag_set_size (st, new_size);

		  ffestorag_set_symbol (st, item_sym);
		  ffestorag_set_offset (st, item_offset);

#if FFEEQUIV_DEBUG
		  fprintf (stderr, " [eq offset=%" ffetargetOffset_f
			   "d, size=%" ffetargetOffset_f "d]",
			   item_offset, new_size);
#endif
		}

	      if ((item_st = ffesymbol_storage (item_sym)) == NULL)
		{		/* Create new ffestorag object, extend equiv
				   area. */
#if FFEEQUIV_DEBUG
		  fprintf (stderr, ".\n");
#endif
		  new_storage = TRUE;
		  item_st = ffestorag_new (ffestorag_list_equivs (st));
		  ffestorag_set_parent (item_st, st);	/* Initializations
							   happen there. */
		  ffestorag_set_init (item_st, NULL);
		  ffestorag_set_accretion (item_st, NULL);
		  ffestorag_set_symbol (item_st, item_sym);
		  ffestorag_set_size (item_st, size);
		  ffestorag_set_offset (item_st, item_offset);
		  ffestorag_set_alignment (item_st, alignment);
		  ffestorag_set_modulo (item_st, modulo);
		  ffestorag_set_type (item_st, FFESTORAG_typeEQUIV);
		  ffestorag_set_basictype (item_st, ffesymbol_basictype (item_sym));
		  ffestorag_set_kindtype (item_st, ffesymbol_kindtype (item_sym));
		  ffestorag_set_typesymbol (item_st, item_sym);
		  ffestorag_set_is_save (item_st, FALSE);	/* Assume FALSE... */
		  if (ffestorag_is_save (st))	/* ...update TRUE */
		    ffestorag_update_save (item_st);	/* if needed. */
		  ffestorag_set_is_init (item_st, FALSE);	/* Assume FALSE... */
		  if (ffestorag_is_init (st))	/* ...update TRUE */
		    ffestorag_update_init (item_st);	/* if needed. */
		  ffesymbol_set_storage (item_sym, item_st);
		  ffesymbol_signal_unreported (item_sym);
		  if (ffesymbol_is_init (item_sym))
		    init = TRUE;

		  /* Determine new size of equiv area, complain if overflow.  */

		  if (!ffetarget_offset_add (&size, item_offset, size)
		      || !ffetarget_offset_add (&size, -ffestorag_offset (st), size))
		    ffetarget_offset_overflow (ffesymbol_text (s));
		  else if (size > ffestorag_size (st))
		    ffestorag_set_size (st, size);
		  ffestorag_update (st, item_sym, ffesymbol_basictype (item_sym),
				    ffesymbol_kindtype (item_sym));
		}
	      else
		{
#if FFEEQUIV_DEBUG
		  fprintf (stderr, " (was %" ffetargetOffset_f "d).\n",
			   ffestorag_offset (item_st));
#endif
		  /* Make sure offset agrees with known offset. */
		  if (item_offset != ffestorag_offset (item_st))
		    {
		      char io1[40];
		      char io2[40];

		      sprintf (&io1[0], "%" ffetargetOffset_f "d", item_offset);
		      sprintf (&io2[0], "%" ffetargetOffset_f "d", ffestorag_offset (item_st));
		      ffebad_start (FFEBAD_EQUIV_MISMATCH);
		      ffebad_string (ffesymbol_text (item_sym));
		      ffebad_string (ffesymbol_text (root_sym));
		      ffebad_string (io1);
		      ffebad_string (io2);
		      ffebad_finish ();
		    }
		}
	      ffesymbol_set_equiv (item_sym, NULL);	/* Don't bother with me anymore. */
	    }			/* (For every equivalence item in the list) */
	  ffebld_set_head (list, NULL);	/* Don't do this list again. */
	}			/* (For every equivalence list in the list of
				   equivs) */
    } while (new_storage && need_storage);

  ffesymbol_set_equiv (root_sym, NULL);	/* This one has storage now. */

  ffeequiv_kill (eq);		/* Fully processed, no longer needed. */

  /* If the offset for this storage area is zero (it cannot be positive),
     that means the alignment/modulo info is already correct.  Otherwise,
     the alignment info is correct, but the modulo info reflects a
     zero offset, so fix it.  */

  if (ffestorag_offset (st) < 0)
    {
      /* Calculate the initial padding necessary to preserve
	 the alignment/modulo requirements for the storage area.
	 These requirements are themselves kept track of in the
	 record for the storage area as a whole, but really pertain
	 to offset 0 of that area, which is where the root symbol
	 was originally placed.

	 The goal here is to have the offset and size for the area
	 faithfully reflect the area itself, not extra requirements
	 like alignment.  So to meet the alignment requirements,
	 the modulo for the area should be set as if the area had an
	 alignment requirement of alignment/0 and was aligned/padded
	 downward to meet the alignment requirements of the area at
	 offset zero, the amount of padding needed being the desired
	 value for the modulo of the area.  */

      alignment = ffestorag_alignment (st);
      modulo = ffestorag_modulo (st);

      /* Since we want to move the whole area *down* (lower memory
	 addresses) as required by the alignment/modulo paid, negate
	 the offset to ffetarget_align, which assumes aligning *up*
	 is desired.  */
      pad = ffetarget_align (&alignment, &modulo,
			     - ffestorag_offset (st),
			     alignment, 0);
      ffestorag_set_modulo (st, pad);
    }

  if (init)
    ffedata_gather (st);	/* Gather subordinate inits into one init. */
}

/* ffeequiv_offset_ -- Determine offset from start of symbol

   ffetargetOffset offset;
   ffesymbol s;	 // Symbol for error reporting.
   ffebld expr;	 // opSUBSTR, opARRAYREF, opSYMTER, opANY.
   bool subtract;  // FALSE means add to adjust, TRUE means subtract from it.
   ffetargetOffset adjust;  // Helps keep answer in pos range (unsigned).
   if (!ffeequiv_offset_(&offset,s,expr,subtract,adjust))
       // error doing the calculation, message already printed

   Returns the offset represented by the SUBSTR, ARRAYREF, or SUBSTR/ARRAYREF
   combination added-to/subtracted-from the adjustment specified.  If there
   is an error of some kind, returns FALSE, else returns TRUE.	Note that
   only the first storage unit specified is considered; A(1:1) and A(1:2000)
   have the same first storage unit and so return the same offset.  */

static bool
ffeequiv_offset_ (ffetargetOffset *offset, ffesymbol s UNUSED,
		  ffebld expr, bool subtract, ffetargetOffset adjust,
		  bool no_precede)
{
  ffetargetIntegerDefault value = 0;
  ffetargetOffset cval;		/* Converted value. */
  ffesymbol sym;

  if (expr == NULL)
    return FALSE;

again:				/* :::::::::::::::::::: */

  switch (ffebld_op (expr))
    {
    case FFEBLD_opANY:
      return FALSE;

    case FFEBLD_opSYMTER:
      {
	ffetargetOffset size;	/* Size of a single unit. */
	ffetargetAlign a;	/* Ignored. */
	ffetargetAlign m;	/* Ignored. */

	sym = ffebld_symter (expr);
	if (ffesymbol_basictype (sym) == FFEINFO_basictypeANY)
	  return FALSE;

	ffetarget_layout (ffesymbol_text (sym), &a, &m, &size,
			  ffesymbol_basictype (sym),
			  ffesymbol_kindtype (sym), 1, 1);

	if (value < 0)
	  {			/* Really invalid, as in A(-2:5), but in case
				   it's wanted.... */
	    if (!ffetarget_offset (&cval, -value))
	      return FALSE;

	    if (!ffetarget_offset_multiply (&cval, cval, size))
	      return FALSE;

	    if (subtract)
	      return ffetarget_offset_add (offset, cval, adjust);

	    if (no_precede && (cval > adjust))
	      {
	      neg:		/* :::::::::::::::::::: */
		ffebad_start (FFEBAD_COMMON_NEG);
		ffebad_string (ffesymbol_text (sym));
		ffebad_finish ();
		return FALSE;
	      }
	    return ffetarget_offset_add (offset, -cval, adjust);
	  }

	if (!ffetarget_offset (&cval, value))
	  return FALSE;

	if (!ffetarget_offset_multiply (&cval, cval, size))
	  return FALSE;

	if (!subtract)
	  return ffetarget_offset_add (offset, cval, adjust);

	if (no_precede && (cval > adjust))
	  goto neg;		/* :::::::::::::::::::: */

	return ffetarget_offset_add (offset, -cval, adjust);
      }

    case FFEBLD_opARRAYREF:
      {
	ffebld symexp = ffebld_left (expr);
	ffebld subscripts = ffebld_right (expr);
	ffebld dims;
	ffetargetIntegerDefault width;
	ffetargetIntegerDefault arrayval;
	ffetargetIntegerDefault lowbound;
	ffetargetIntegerDefault highbound;
	ffebld subscript;
	ffebld dim;
	ffebld low;
	ffebld high;
	int rank = 0;

	if (ffebld_op (symexp) != FFEBLD_opSYMTER)
	  return FALSE;

	sym = ffebld_symter (symexp);
	if (ffesymbol_basictype (sym) == FFEINFO_basictypeANY)
	  return FALSE;

	if (ffesymbol_size (sym) == FFETARGET_charactersizeNONE)
	  width = 1;
	else
	  width = ffesymbol_size (sym);
	dims = ffesymbol_dims (sym);

	while (subscripts != NULL)
	  {
	    ++rank;
	    if (dims == NULL)
	      {
		ffebad_start (FFEBAD_EQUIV_MANY);
		ffebad_string (ffesymbol_text (sym));
		ffebad_finish ();
		return FALSE;
	      }

	    subscript = ffebld_head (subscripts);
	    dim = ffebld_head (dims);

	    if (ffebld_op (subscript) == FFEBLD_opANY)
	      return FALSE;

	    assert (ffebld_op (subscript) == FFEBLD_opCONTER);
	    assert (ffeinfo_basictype (ffebld_info (subscript))
		    == FFEINFO_basictypeINTEGER);
	    assert (ffeinfo_kindtype (ffebld_info (subscript))
		    == FFEINFO_kindtypeINTEGERDEFAULT);
	    arrayval = ffebld_constant_integerdefault (ffebld_conter
						       (subscript));

	    if (ffebld_op (dim) == FFEBLD_opANY)
	      return FALSE;

	    assert (ffebld_op (dim) == FFEBLD_opBOUNDS);
	    low = ffebld_left (dim);
	    high = ffebld_right (dim);

	    if (low == NULL)
	      lowbound = 1;
	    else
	      {
		if (ffebld_op (low) == FFEBLD_opANY)
		  return FALSE;

		assert (ffebld_op (low) == FFEBLD_opCONTER);
		assert (ffeinfo_basictype (ffebld_info (low))
			== FFEINFO_basictypeINTEGER);
		assert (ffeinfo_kindtype (ffebld_info (low))
			== FFEINFO_kindtypeINTEGERDEFAULT);
		lowbound
		  = ffebld_constant_integerdefault (ffebld_conter (low));
	      }

	    if (ffebld_op (high) == FFEBLD_opANY)
	      return FALSE;

	    assert (ffebld_op (high) == FFEBLD_opCONTER);
	    assert (ffeinfo_basictype (ffebld_info (high))
		    == FFEINFO_basictypeINTEGER);
	    assert (ffeinfo_kindtype (ffebld_info (high))
		    == FFEINFO_kindtypeINTEGER1);
	    highbound
	      = ffebld_constant_integerdefault (ffebld_conter (high));

	    if ((arrayval < lowbound) || (arrayval > highbound))
	      {
		char rankstr[10];

		sprintf (rankstr, "%d", rank);
		ffebad_start (FFEBAD_EQUIV_SUBSCRIPT);
		ffebad_string (ffesymbol_text (sym));
		ffebad_string (rankstr);
		ffebad_finish ();
	      }

	    subscripts = ffebld_trail (subscripts);
	    dims = ffebld_trail (dims);

	    value += width * (arrayval - lowbound);
	    if (subscripts != NULL)
	      width *= highbound - lowbound + 1;
	  }

	if (dims != NULL)
	  {
	    ffebad_start (FFEBAD_EQUIV_FEW);
	    ffebad_string (ffesymbol_text (sym));
	    ffebad_finish ();
	    return FALSE;
	  }

	expr = symexp;
      }
      goto again;		/* :::::::::::::::::::: */

    case FFEBLD_opSUBSTR:
      {
	ffebld begin = ffebld_head (ffebld_right (expr));

	expr = ffebld_left (expr);
	if (ffebld_op (expr) == FFEBLD_opANY)
	  return FALSE;
	if (ffebld_op (expr) == FFEBLD_opARRAYREF)
	  sym = ffebld_symter (ffebld_left (expr));
	else if (ffebld_op (expr) == FFEBLD_opSYMTER)
	  sym = ffebld_symter (expr);
	else
	  sym = NULL;

	if ((sym != NULL)
	    && (ffesymbol_basictype (sym) == FFEINFO_basictypeANY))
	  return FALSE;

	if (begin == NULL)
	  value = 0;
	else
	  {
	    if (ffebld_op (begin) == FFEBLD_opANY)
	      return FALSE;
	    assert (ffebld_op (begin) == FFEBLD_opCONTER);
	    assert (ffeinfo_basictype (ffebld_info (begin))
		    == FFEINFO_basictypeINTEGER);
	    assert (ffeinfo_kindtype (ffebld_info (begin))
		    == FFEINFO_kindtypeINTEGERDEFAULT);

	    value = ffebld_constant_integerdefault (ffebld_conter (begin));

	    if ((value < 1)
		|| ((sym != NULL)
		    && (value > ffesymbol_size (sym))))
	      {
		ffebad_start (FFEBAD_EQUIV_RANGE);
		ffebad_string (ffesymbol_text (sym));
		ffebad_finish ();
	      }

	    --value;
	  }
	if ((sym != NULL)
	    && (ffesymbol_basictype (sym) != FFEINFO_basictypeCHARACTER))
	  {
	    ffebad_start (FFEBAD_EQUIV_SUBSTR);
	    ffebad_string (ffesymbol_text (sym));
	    ffebad_finish ();
	    value = 0;
	  }
      }
      goto again;		/* :::::::::::::::::::: */

    default:
      assert ("bad op" == NULL);
      return FALSE;
    }

}

/* ffeequiv_add -- Add list of equivalences to list of lists for eq object

   ffeequiv eq;
   ffebld list;
   ffelexToken t;  // points to first item in equivalence list
   ffeequiv_add(eq,list,t);

   Check the list to make sure only one common symbol is involved (even
   if multiple times) and agrees with the common symbol for the equivalence
   object (or it has no common symbol until now).  Prepend (or append, it
   doesn't matter) the list to the list of lists for the equivalence object.
   Otherwise report an error and return.  */

void
ffeequiv_add (ffeequiv eq, ffebld list, ffelexToken t)
{
  ffebld item;
  ffesymbol symbol;
  ffesymbol common = ffeequiv_common (eq);

  for (item = list; item != NULL; item = ffebld_trail (item))
    {
      symbol = ffeequiv_symbol (ffebld_head (item));

      if (ffesymbol_common (symbol) != NULL)	/* Is symbol known in COMMON yet? */
	{
	  if (common == NULL)
	    common = ffesymbol_common (symbol);
	  else if (common != ffesymbol_common (symbol))
	    {
	      /* Yes, and symbol disagrees with others on the COMMON area. */
	      ffebad_start (FFEBAD_EQUIV_COMMON);
	      ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
	      ffebad_string (ffesymbol_text (common));
	      ffebad_string (ffesymbol_text (ffesymbol_common (symbol)));
	      ffebad_finish ();
	      return;
	    }
	}
    }

  if ((common != NULL)
      && (ffeequiv_common (eq) == NULL))	/* Is COMMON involved already? */
    ffeequiv_set_common (eq, common);	/* No, but it is now. */

  for (item = list; item != NULL; item = ffebld_trail (item))
    {
      symbol = ffeequiv_symbol (ffebld_head (item));

      if (ffesymbol_equiv (symbol) == NULL)
	ffesymbol_set_equiv (symbol, eq);
      else
	assert (ffesymbol_equiv (symbol) == eq);

      if (ffesymbol_common (symbol) == NULL)	/* Is symbol in a COMMON
						   area? */
	{			/* No (at least not yet). */
	  if (ffesymbol_is_save (symbol))
	    ffeequiv_update_save (eq);	/* EQUIVALENCE has >=1 SAVEd entity. */
	  if (ffesymbol_is_init (symbol))
	    ffeequiv_update_init (eq);	/* EQUIVALENCE has >=1 init'd entity. */
	  continue;		/* Nothing more to do here. */
	}

#if FFEGLOBAL_ENABLED
      if (ffesymbol_is_init (symbol))
	ffeglobal_init_common (ffesymbol_common (symbol), t);
#endif

      if (ffesymbol_is_save (ffesymbol_common (symbol)))
	ffeequiv_update_save (eq);	/* EQUIVALENCE is in a SAVEd COMMON block. */
      if (ffesymbol_is_init (ffesymbol_common (symbol)))
	ffeequiv_update_init (eq);	/* EQUIVALENCE is in a init'd COMMON block. */
    }

  ffeequiv_set_list (eq, ffebld_new_item (list, ffeequiv_list (eq)));
}

/* ffeequiv_dump -- Dump info on equivalence object

   ffeequiv eq;
   ffeequiv_dump(eq);  */

#if FFECOM_targetCURRENT == FFECOM_targetFFE
void
ffeequiv_dump (ffeequiv eq)
{
  if (ffeequiv_common (eq) != NULL)
    fprintf (dmpout, "(common %s) ", ffesymbol_text (ffeequiv_common (eq)));
  ffebld_dump (ffeequiv_list (eq));
}
#endif

/* ffeequiv_exec_transition -- Do the hard work on all the equivalence objects

   ffeequiv_exec_transition();	*/

void
ffeequiv_exec_transition ()
{
  while (ffeequiv_list_.first != (ffeequiv) &ffeequiv_list_.first)
    ffeequiv_layout_local_ (ffeequiv_list_.first);
}

/* ffeequiv_init_2 -- Initialize for new program unit

   ffeequiv_init_2();

   Initializes the list of equivalences.  */

void
ffeequiv_init_2 ()
{
  ffeequiv_list_.first = (ffeequiv) &ffeequiv_list_.first;
  ffeequiv_list_.last = (ffeequiv) &ffeequiv_list_.first;
}

/* ffeequiv_kill -- Kill equivalence object after removing from list

   ffeequiv eq;
   ffeequiv_kill(eq);

   Removes equivalence object from master list, then kills it.	*/

void
ffeequiv_kill (ffeequiv victim)
{
  victim->next->previous = victim->previous;
  victim->previous->next = victim->next;
  if (ffe_is_do_internal_checks ())
    {
      ffebld list;
      ffebld item;
      ffebld expr;

      /* Assert that nobody our victim points to still points to it.  */

      assert ((victim->common == NULL)
	      || (ffesymbol_equiv (victim->common) == NULL));

      for (list = victim->list; list != NULL; list = ffebld_trail (list))
	{
	  for (item = ffebld_head (list); item != NULL; item = ffebld_trail (item))
	    {
	      ffesymbol sym;

	      expr = ffebld_head (item);
	      sym = ffeequiv_symbol (expr);
	      if (sym == NULL)
		continue;
	      assert (ffesymbol_equiv (sym) != victim);
	    }
	}
    }
  malloc_kill_ks (ffe_pool_program_unit (), victim, sizeof (*victim));
}

/* ffeequiv_layout_cblock -- Lay out storage for common area

   ffestorag st;
   if (ffeequiv_layout_cblock(st))
       // at least one equiv'd symbol has init/accretion expr.

   Now that the explicitly COMMONed variables in the common area (whose
   ffestorag object is passed) have been laid out, lay out the storage
   for all variables equivalenced into the area by making subordinate
   ffestorag objects for them.	*/

bool
ffeequiv_layout_cblock (ffestorag st)
{
  ffesymbol s = ffestorag_symbol (st);	/* CBLOCK symbol. */
  ffebld list;			/* List of explicit common vars, in order, in
				   s. */
  ffebld item;			/* List of list of equivalences in a given
				   explicit common var. */
  ffebld root;			/* Expression for (1st) explicit common var
				   in list of eqs. */
  ffestorag rst;		/* Storage for root. */
  ffetargetOffset root_offset;	/* Offset for root into common area. */
  ffesymbol sr;			/* Root itself. */
  ffeequiv seq;			/* Its equivalence object, if any. */
  ffebld var;			/* Expression for equivalence. */
  ffestorag vst;		/* Storage for var. */
  ffetargetOffset var_offset;	/* Offset for var into common area. */
  ffesymbol sv;			/* Var itself. */
  ffebld altroot;		/* Alternate root. */
  ffesymbol altrootsym;		/* Alternate root symbol. */
  ffetargetAlign alignment;
  ffetargetAlign modulo;
  ffetargetAlign pad;
  ffetargetOffset size;
  ffetargetOffset num_elements;
  bool new_storage;		/* Established new storage info. */
  bool need_storage;		/* Have need for more storage info. */
  bool ok;
  bool init = FALSE;

  assert (st != NULL);
  assert (ffestorag_type (st) == FFESTORAG_typeCBLOCK);
  assert (ffesymbol_kind (ffestorag_symbol (st)) == FFEINFO_kindCOMMON);

  for (list = ffesymbol_commonlist (ffestorag_symbol (st));
       list != NULL;
       list = ffebld_trail (list))
    {				/* For every variable in the common area */
      assert (ffebld_op (ffebld_head (list)) == FFEBLD_opSYMTER);
      sr = ffebld_symter (ffebld_head (list));
      if ((seq = ffesymbol_equiv (sr)) == NULL)
	continue;		/* No equivalences to process. */
      rst = ffesymbol_storage (sr);
      if (rst == NULL)
	{
	  assert (ffesymbol_kind (sr) == FFEINFO_kindANY);
	  continue;
	}
      ffesymbol_set_equiv (sr, NULL);	/* Cancel ref to equiv obj. */
      do
	{
	  new_storage = FALSE;
	  need_storage = FALSE;
	  for (item = ffeequiv_list (seq);	/* Get list of equivs. */
	       item != NULL;
	       item = ffebld_trail (item))
	    {			/* For every eqv list in the list of equivs
				   for the variable */
	      altroot = NULL;
	      altrootsym = NULL;
	      for (root = ffebld_head (item);
		   root != NULL;
		   root = ffebld_trail (root))
		{		/* For every equivalence item in the list */
		  sv = ffeequiv_symbol (ffebld_head (root));
		  if (sv == sr)
		    break;	/* Found first mention of "rooted" symbol. */
		  if (ffesymbol_storage (sv) != NULL)
		    {
		      altroot = root;	/* If no mention, use this guy
					   instead. */
		      altrootsym = sv;
		    }
		}
	      if (root != NULL)
		{
		  root = ffebld_head (root);	/* Lose its opITEM. */
		  ok = ffeequiv_offset_ (&root_offset, sr, root, FALSE,
					 ffestorag_offset (rst), TRUE);
		  /* Equiv point prior to start of common area? */
		}
	      else if (altroot != NULL)
		{
		  /* Equiv point prior to start of common area? */
		  root = ffebld_head (altroot);
		  ok = ffeequiv_offset_ (&root_offset, altrootsym, root,
					 FALSE,
			 ffestorag_offset (ffesymbol_storage (altrootsym)),
					 TRUE);
		  ffesymbol_set_equiv (altrootsym, NULL);
		}
	      else
		/* No rooted symbol in list of equivalences! */
		{		/* Assume this was due to opANY and ignore
				   this list for now. */
		  need_storage = TRUE;
		  continue;
		}

	      /* We now know the root symbol and the operating offset of that
		 root into the common area.  The other expressions in the
		 list all identify an initial storage unit that must have the
		 same offset. */

	      for (var = ffebld_head (item);
		   var != NULL;
		   var = ffebld_trail (var))
		{		/* For every equivalence item in the list */
		  if (ffebld_head (var) == root)
		    continue;	/* Except root, of course. */
		  sv = ffeequiv_symbol (ffebld_head (var));
		  if (sv == NULL)
		    continue;	/* Except erroneous stuff (opANY). */
		  ffesymbol_set_equiv (sv, NULL);	/* Don't need this ref
							   anymore. */
		  if (!ok
		      || !ffeequiv_offset_ (&var_offset, sv,
					    ffebld_head (var), TRUE,
					    root_offset, TRUE))
		    continue;	/* Can't do negative offset wrt COMMON. */

		  if (ffesymbol_rank (sv) == 0)
		    num_elements = 1;
		  else
		    num_elements = ffebld_constant_integerdefault
		      (ffebld_conter (ffesymbol_arraysize (sv)));
		  ffetarget_layout (ffesymbol_text (sv), &alignment,
				    &modulo, &size,
				    ffesymbol_basictype (sv),
				    ffesymbol_kindtype (sv),
				    ffesymbol_size (sv), num_elements);
		  pad = ffetarget_align (ffestorag_ptr_to_alignment (st),
					 ffestorag_ptr_to_modulo (st),
					 var_offset, alignment, modulo);
		  if (pad != 0)
		    {
		      ffebad_start (FFEBAD_EQUIV_ALIGN);
		      ffebad_string (ffesymbol_text (sv));
		      ffebad_finish ();
		      continue;
		    }

		  if ((vst = ffesymbol_storage (sv)) == NULL)
		    {		/* Create new ffestorag object, extend
				   cblock. */
		      new_storage = TRUE;
		      vst = ffestorag_new (ffestorag_list_equivs (st));
		      ffestorag_set_parent (vst, st);	/* Initializations
							   happen there. */
		      ffestorag_set_init (vst, NULL);
		      ffestorag_set_accretion (vst, NULL);
		      ffestorag_set_symbol (vst, sv);
		      ffestorag_set_size (vst, size);
		      ffestorag_set_offset (vst, var_offset);
		      ffestorag_set_alignment (vst, alignment);
		      ffestorag_set_modulo (vst, modulo);
		      ffestorag_set_type (vst, FFESTORAG_typeEQUIV);
		      ffestorag_set_basictype (vst, ffesymbol_basictype (sv));
		      ffestorag_set_kindtype (vst, ffesymbol_kindtype (sv));
		      ffestorag_set_typesymbol (vst, sv);
		      ffestorag_set_is_save (vst, FALSE);	/* Assume FALSE... */
		      if (ffestorag_is_save (st))	/* ...update TRUE */
			ffestorag_update_save (vst);	/* if needed. */
		      ffestorag_set_is_init (vst, FALSE);	/* Assume FALSE... */
		      if (ffestorag_is_init (st))	/* ...update TRUE */
			ffestorag_update_init (vst);	/* if needed. */
		      if (!ffetarget_offset_add (&size, var_offset, size))
			/* Find one size of common block, complain if
			   overflow. */
			ffetarget_offset_overflow (ffesymbol_text (s));
		      else if (size > ffestorag_size (st))
			/* Extend common. */
			ffestorag_set_size (st, size);
		      ffesymbol_set_storage (sv, vst);
		      ffesymbol_set_common (sv, s);
		      ffesymbol_signal_unreported (sv);
		      ffestorag_update (st, sv, ffesymbol_basictype (sv),
					ffesymbol_kindtype (sv));
		      if (ffesymbol_is_init (sv))
			init = TRUE;
		    }
		  else
		    {
		      /* Make sure offset agrees with known offset. */
		      if (var_offset != ffestorag_offset (vst))
			{
			  char io1[40];
			  char io2[40];

			  sprintf (&io1[0], "%" ffetargetOffset_f "d", var_offset);
			  sprintf (&io2[0], "%" ffetargetOffset_f "d", ffestorag_offset (vst));
			  ffebad_start (FFEBAD_EQUIV_MISMATCH);
			  ffebad_string (ffesymbol_text (sv));
			  ffebad_string (ffesymbol_text (s));
			  ffebad_string (io1);
			  ffebad_string (io2);
			  ffebad_finish ();
			}
		    }
		}		/* (For every equivalence item in the list) */
	    }			/* (For every eqv list in the list of equivs
				   for the variable) */
	}
      while (new_storage && need_storage);

      ffeequiv_kill (seq);	/* Kill equiv obj. */
    }				/* (For every variable in the common area) */

  return init;
}

/* ffeequiv_merge -- Merge two equivalence objects, return the merged result

   ffeequiv eq1;
   ffeequiv eq2;
   ffelexToken t;  // points to current equivalence item forcing the merge.
   eq1 = ffeequiv_merge(eq1,eq2,t);

   If the two equivalence objects can be merged, they are, all the
   ffesymbols in their lists of lists are adjusted to point to the merged
   equivalence object, and the merged object is returned.

   Otherwise, the two equivalence objects have different non-NULL common
   symbols, so the merge cannot take place.  An error message is issued and
   NULL is returned.  */

ffeequiv
ffeequiv_merge (ffeequiv eq1, ffeequiv eq2, ffelexToken t)
{
  ffebld list;
  ffebld eqs;
  ffesymbol symbol;
  ffebld last = NULL;

  /* If both equivalence objects point to different common-based symbols,
     complain.	Of course, one or both might have NULL common symbols now,
     and get COMMONed later, but the COMMON statement handler checks for
     this. */

  if ((ffeequiv_common (eq1) != NULL) && (ffeequiv_common (eq2) != NULL)
      && (ffeequiv_common (eq1) != ffeequiv_common (eq2)))
    {
      ffebad_start (FFEBAD_EQUIV_COMMON);
      ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
      ffebad_string (ffesymbol_text (ffeequiv_common (eq1)));
      ffebad_string (ffesymbol_text (ffeequiv_common (eq2)));
      ffebad_finish ();
      return NULL;
    }

  /* Make eq1 the new, merged object (arbitrarily). */

  if (ffeequiv_common (eq1) == NULL)
    ffeequiv_set_common (eq1, ffeequiv_common (eq2));

  /* If the victim object has any init'ed entities, so does the new object. */

  if (eq2->is_init)
    eq1->is_init = TRUE;

#if FFEGLOBAL_ENABLED
  if (eq1->is_init && (ffeequiv_common (eq1) != NULL))
    ffeglobal_init_common (ffeequiv_common (eq1), t);
#endif

  /* If the victim object has any SAVEd entities, then the new object has
     some. */

  if (ffeequiv_is_save (eq2))
    ffeequiv_update_save (eq1);

  /* If the victim object has any init'd entities, then the new object has
     some. */

  if (ffeequiv_is_init (eq2))
    ffeequiv_update_init (eq1);

  /* Adjust all the symbols in the list of lists of equivalences for the
     victim equivalence object so they point to the new merged object
     instead. */

  for (list = ffeequiv_list (eq2); list != NULL; list = ffebld_trail (list))
    {
      for (eqs = ffebld_head (list); eqs != NULL; eqs = ffebld_trail (eqs))
	{
	  symbol = ffeequiv_symbol (ffebld_head (eqs));
	  if (ffesymbol_equiv (symbol) == eq2)
	    ffesymbol_set_equiv (symbol, eq1);
	  else
	    assert (ffesymbol_equiv (symbol) == eq1);	/* Can see a sym > once. */
	}

      /* For convenience, remember where the last ITEM in the outer list is. */

      if (ffebld_trail (list) == NULL)
	{
	  last = list;
	  break;
	}
    }

  /* Append the list of lists in the new, merged object to the list of lists
     in the victim object, then use the new combined list in the new merged
     object. */

  ffebld_set_trail (last, ffeequiv_list (eq1));
  ffeequiv_set_list (eq1, ffeequiv_list (eq2));

  /* Unlink and kill the victim object. */

  ffeequiv_kill (eq2);

  return eq1;			/* Return the new merged object. */
}

/* ffeequiv_new -- Create new equivalence object, put in list

   ffeequiv eq;
   eq = ffeequiv_new();

   Creates a new equivalence object and adds it to the list of equivalence
   objects.  */

ffeequiv
ffeequiv_new ()
{
  ffeequiv eq;

  eq = malloc_new_ks (ffe_pool_program_unit (), "ffeequiv", sizeof (*eq));
  eq->next = (ffeequiv) &ffeequiv_list_.first;
  eq->previous = ffeequiv_list_.last;
  ffeequiv_set_common (eq, NULL);	/* No COMMON area yet. */
  ffeequiv_set_list (eq, NULL);	/* No list of lists of equivalences yet. */
  ffeequiv_set_is_save (eq, FALSE);
  ffeequiv_set_is_init (eq, FALSE);
  eq->next->previous = eq;
  eq->previous->next = eq;

  return eq;
}

/* ffeequiv_symbol -- Return symbol for equivalence expression

   ffesymbol symbol;
   ffebld expr;
   symbol = ffeequiv_symbol(expr);

   Finds the terminal SYMTER in an equivalence expression and returns the
   ffesymbol for it.  */

ffesymbol
ffeequiv_symbol (ffebld expr)
{
  assert (expr != NULL);

again:				/* :::::::::::::::::::: */

  switch (ffebld_op (expr))
    {
    case FFEBLD_opARRAYREF:
    case FFEBLD_opSUBSTR:
      expr = ffebld_left (expr);
      goto again;		/* :::::::::::::::::::: */

    case FFEBLD_opSYMTER:
      return ffebld_symter (expr);

    case FFEBLD_opANY:
      return NULL;

    default:
      assert ("bad eq expr" == NULL);
      return NULL;
    }
}

/* ffeequiv_update_init -- Update the INIT flag for the area to TRUE

   ffeequiv eq;
   ffeequiv_update_init(eq);

   If the INIT flag for the <eq> object is already set, return.	 Else,
   set it TRUE and call ffe*_update_init for all objects contained in
   this one.  */

void
ffeequiv_update_init (ffeequiv eq)
{
  ffebld list;			/* Current list in list of lists. */
  ffebld item;			/* Current item in current list. */
  ffebld expr;			/* Expression in head of current item. */

  if (eq->is_init)
    return;

  eq->is_init = TRUE;

  if ((eq->common != NULL)
      && !ffesymbol_is_init (eq->common))
    ffesymbol_update_init (eq->common);	/* Shouldn't be needed. */

  for (list = eq->list; list != NULL; list = ffebld_trail (list))
    {
      for (item = ffebld_head (list); item != NULL; item = ffebld_trail (item))
	{
	  expr = ffebld_head (item);

	again:			/* :::::::::::::::::::: */

	  switch (ffebld_op (expr))
	    {
	    case FFEBLD_opANY:
	      break;

	    case FFEBLD_opSYMTER:
	      if (!ffesymbol_is_init (ffebld_symter (expr)))
		ffesymbol_update_init (ffebld_symter (expr));
	      break;

	    case FFEBLD_opARRAYREF:
	      expr = ffebld_left (expr);
	      goto again;	/* :::::::::::::::::::: */

	    case FFEBLD_opSUBSTR:
	      expr = ffebld_left (expr);
	      goto again;	/* :::::::::::::::::::: */

	    default:
	      assert ("bad op for ffeequiv_update_init" == NULL);
	      break;
	    }
	}
    }
}

/* ffeequiv_update_save -- Update the SAVE flag for the area to TRUE

   ffeequiv eq;
   ffeequiv_update_save(eq);

   If the SAVE flag for the <eq> object is already set, return.	 Else,
   set it TRUE and call ffe*_update_save for all objects contained in
   this one.  */

void
ffeequiv_update_save (ffeequiv eq)
{
  ffebld list;			/* Current list in list of lists. */
  ffebld item;			/* Current item in current list. */
  ffebld expr;			/* Expression in head of current item. */

  if (eq->is_save)
    return;

  eq->is_save = TRUE;

  if ((eq->common != NULL)
      && !ffesymbol_is_save (eq->common))
    ffesymbol_update_save (eq->common);	/* Shouldn't be needed. */

  for (list = eq->list; list != NULL; list = ffebld_trail (list))
    {
      for (item = ffebld_head (list); item != NULL; item = ffebld_trail (item))
	{
	  expr = ffebld_head (item);

	again:			/* :::::::::::::::::::: */

	  switch (ffebld_op (expr))
	    {
	    case FFEBLD_opANY:
	      break;

	    case FFEBLD_opSYMTER:
	      if (!ffesymbol_is_save (ffebld_symter (expr)))
		ffesymbol_update_save (ffebld_symter (expr));
	      break;

	    case FFEBLD_opARRAYREF:
	      expr = ffebld_left (expr);
	      goto again;	/* :::::::::::::::::::: */

	    case FFEBLD_opSUBSTR:
	      expr = ffebld_left (expr);
	      goto again;	/* :::::::::::::::::::: */

	    default:
	      assert ("bad op for ffeequiv_update_save" == NULL);
	      break;
	    }
	}
    }
}
