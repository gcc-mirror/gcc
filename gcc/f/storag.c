/* storag.c -- Implementation File (module.c template V1.0)
   Copyright (C) 1995, 1996, 2003 Free Software Foundation, Inc.
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
      Maintains information on storage (memory) relationships between
      COMMON, dummy, and local variables, plus their equivalences (dummies
      don't have equivalences, however).

   Modifications:
*/

/* Include files. */

#include "proj.h"
#include "storag.h"
#include "data.h"
#include "malloc.h"
#include "symbol.h"
#include "target.h"

/* Externals defined here. */

ffestoragList_ ffestorag_list_;

/* Simple definitions and enumerations. */


/* Internal typedefs. */


/* Private include files. */


/* Internal structure definitions. */


/* Static objects accessed by functions in this module. */

static ffetargetOffset ffestorag_local_size_;	/* #units allocated so far. */
static bool ffestorag_reported_;/* Reports happen only once. */

/* Static functions (internal). */


/* Internal macros. */

#define ffestorag_next_(s) ((s)->next)
#define ffestorag_previous_(s) ((s)->previous)

/* ffestorag_drive -- Drive fn from list of storage objects

   ffestoragList sl;
   void (*fn)(ffestorag mst,ffestorag st);
   ffestorag mst;  // the master ffestorag object (or whatever)
   ffestorag_drive(sl,fn,mst);

   Calls (*fn)(mst,st) for every st in the list sl.  */

void
ffestorag_drive (ffestoragList sl, void (*fn) (ffestorag mst, ffestorag st),
		 ffestorag mst)
{
  ffestorag st;

  for (st = sl->first;
       st != (ffestorag) &sl->first;
       st = st->next)
    (*fn) (mst, st);
}

/* ffestorag_dump -- Dump information on storage object

   ffestorag s;	 // the ffestorag object
   ffestorag_dump(s);

   Dumps information in the storage object.  */

void
ffestorag_dump (ffestorag s)
{
  if (s == NULL)
    {
      fprintf (dmpout, "(no storage object)");
      return;
    }

  switch (s->type)
    {
    case FFESTORAG_typeCBLOCK:
      fprintf (dmpout, "CBLOCK ");
      break;

    case FFESTORAG_typeCOMMON:
      fprintf (dmpout, "COMMON ");
      break;

    case FFESTORAG_typeLOCAL:
      fprintf (dmpout, "LOCAL ");
      break;

    case FFESTORAG_typeEQUIV:
      fprintf (dmpout, "EQUIV ");
      break;

    default:
      fprintf (dmpout, "?%d? ", s->type);
      break;
    }

  if (s->symbol != NULL)
    fprintf (dmpout, "\"%s\" ", ffesymbol_text (s->symbol));

  fprintf (dmpout, "at %" ffetargetOffset_f "d size %" ffetargetOffset_f
	   "d, align loc%%%"
	   ffetargetAlign_f "u=%" ffetargetAlign_f "u, bt=%s, kt=%s",
	   s->offset,
	   s->size, (unsigned int) s->alignment, (unsigned int) s->modulo,
	   ffeinfo_basictype_string (s->basic_type),
	   ffeinfo_kindtype_string (s->kind_type));

  if (s->equivs_.first != (ffestorag) &s->equivs_.first)
    {
      ffestorag sq;

      fprintf (dmpout, " with equivs");
      for (sq = s->equivs_.first;
	   sq != (ffestorag) &s->equivs_.first;
	   sq = ffestorag_next_ (sq))
	{
	  if (ffestorag_previous_ (sq) == (ffestorag) &s->equivs_.first)
	    fputc (' ', dmpout);
	  else
	    fputc (',', dmpout);
	  fprintf (dmpout, "%s", ffesymbol_text (ffestorag_symbol (sq)));
	}
    }
}

/* ffestorag_init_2 -- Initialize for new program unit

   ffestorag_init_2();	*/

void
ffestorag_init_2 (void)
{
  ffestorag_list_.first = ffestorag_list_.last
  = (ffestorag) &ffestorag_list_.first;
  ffestorag_local_size_ = 0;
  ffestorag_reported_ = FALSE;
}

/* ffestorag_end_layout -- Do final layout for symbol

   ffesymbol s;
   ffestorag_end_layout(s);  */

void
ffestorag_end_layout (ffesymbol s)
{
  if (ffesymbol_storage (s) != NULL)
    return;			/* Already laid out. */

  ffestorag_exec_layout (s);	/* Do what we have in common. */
#if 0
  assert (ffesymbol_storage (s) == NULL);	/* I'd like to know what
						   cases miss going through
						   ffecom_sym_learned, and
						   why; I don't think we
						   should have to do the
						   exec_layout thing at all
						   here. */
  /* Now I think I know: we have to do exec_layout here, because equivalence
     handling could encounter an error that takes a variable off of its
     equivalence object (and vice versa), and we should then layout the var
     as a local entity. */
#endif
}

/* ffestorag_exec_layout -- Do initial layout for symbol

   ffesymbol s;
   ffestorag_exec_layout(s);  */

void
ffestorag_exec_layout (ffesymbol s)
{
  ffetargetAlign alignment;
  ffetargetAlign modulo;
  ffetargetOffset size;
  ffetargetOffset num_elements;
  ffetargetAlign pad;
  ffestorag st;
  ffestorag stv;
  ffebld list;
  ffebld item;
  ffesymbol var;
  bool init;

  if (ffesymbol_storage (s) != NULL)
    return;			/* Already laid out. */

  switch (ffesymbol_kind (s))
    {
    default:
      return;			/* Do nothing. */

    case FFEINFO_kindENTITY:
      switch (ffesymbol_where (s))
	{
	case FFEINFO_whereLOCAL:
	  if (ffesymbol_equiv (s) != NULL)
	    return;		/* Let ffeequiv handle this guy. */
	  if (ffesymbol_rank (s) == 0)
	    num_elements = 1;
	  else
	    {
	      if (ffebld_op (ffesymbol_arraysize (s))
		  != FFEBLD_opCONTER)
		return;	/* An adjustable local array, just like a dummy. */
	      num_elements
		= ffebld_constant_integerdefault (ffebld_conter
						  (ffesymbol_arraysize (s)));
	    }
	  ffetarget_layout (ffesymbol_text (s), &alignment, &modulo,
			    &size, ffesymbol_basictype (s),
			    ffesymbol_kindtype (s), ffesymbol_size (s),
			    num_elements);
	  st = ffestorag_new (ffestorag_list_master ());
	  st->parent = NULL;	/* Initializations happen at sym level. */
	  st->init = NULL;
	  st->accretion = NULL;
	  st->symbol = s;
	  st->size = size;
	  st->offset = 0;
	  st->alignment = alignment;
	  st->modulo = modulo;
	  st->type = FFESTORAG_typeLOCAL;
	  st->basic_type = ffesymbol_basictype (s);
	  st->kind_type = ffesymbol_kindtype (s);
	  st->type_symbol = s;
	  st->is_save = ffesymbol_is_save (s);
	  st->is_init = ffesymbol_is_init (s);
	  ffesymbol_set_storage (s, st);
	  if (ffesymbol_is_init (s))
	    ffecom_notify_init_symbol (s);	/* Init completed before, but
						   we didn't have a storage
						   object for it; maybe back
						   end wants to see the sym
						   again now. */
	  ffesymbol_signal_unreported (s);
	  return;

	case FFEINFO_whereCOMMON:
	  return;		/* Allocate storage for entire common block
				   at once. */

	case FFEINFO_whereDUMMY:
	  return;		/* Don't do anything about dummies for now. */

	case FFEINFO_whereRESULT:
	case FFEINFO_whereIMMEDIATE:
	case FFEINFO_whereCONSTANT:
	case FFEINFO_whereNONE:
	  return;		/* These don't get storage (esp. NONE, which
				   is UNCERTAIN). */

	default:
	  assert ("bad ENTITY where" == NULL);
	  return;
	}
      break;

    case FFEINFO_kindCOMMON:
      assert (ffesymbol_where (s) == FFEINFO_whereLOCAL);
      st = ffestorag_new (ffestorag_list_master ());
      st->parent = NULL;	/* Initializations happen here. */
      st->init = NULL;
      st->accretion = NULL;
      st->symbol = s;
      st->size = 0;
      st->offset = 0;
      st->alignment = 1;
      st->modulo = 0;
      st->type = FFESTORAG_typeCBLOCK;
      if (ffesymbol_commonlist (s) != NULL)
	{
	  var = ffebld_symter (ffebld_head (ffesymbol_commonlist (s)));
	  st->basic_type = ffesymbol_basictype (var);
	  st->kind_type = ffesymbol_kindtype (var);
	  st->type_symbol = var;
	}
      else
	{			/* Special case for empty common area:
				   NONE/NONE means nothing. */
	  st->basic_type = FFEINFO_basictypeNONE;
	  st->kind_type = FFEINFO_kindtypeNONE;
	  st->type_symbol = NULL;
	}
      st->is_save = ffesymbol_is_save (s);
      st->is_init = ffesymbol_is_init (s);
      if (!ffe_is_mainprog ())
	ffeglobal_save_common (s,
			       st->is_save || ffe_is_saveall (),
			       ffesymbol_where_line (s),
			       ffesymbol_where_column (s));
      ffesymbol_set_storage (s, st);

      init = FALSE;
      for (list = ffesymbol_commonlist (s);
	   list != NULL;
	   list = ffebld_trail (list))
	{
	  item = ffebld_head (list);
	  assert (ffebld_op (item) == FFEBLD_opSYMTER);
	  var = ffebld_symter (item);
	  if (ffesymbol_basictype (var) == FFEINFO_basictypeANY)
	    continue;		/* Ignore any symbols that have errors. */
	  if (ffesymbol_rank (var) == 0)
	    num_elements = 1;
	  else
	    num_elements = ffebld_constant_integerdefault (ffebld_conter
					       (ffesymbol_arraysize (var)));
	  ffetarget_layout (ffesymbol_text (var), &alignment, &modulo,
			    &size, ffesymbol_basictype (var),
			    ffesymbol_kindtype (var), ffesymbol_size (var),
			    num_elements);
	  pad = ffetarget_align (&st->alignment, &st->modulo, st->size,
				 alignment, modulo);
	  if (pad != 0)
	    {			/* Warn about padding in the midst of a
				   common area. */
	      char padding[20];

	      sprintf (&padding[0], "%" ffetargetAlign_f "u", pad);
	      ffebad_start (FFEBAD_COMMON_PAD);
	      ffebad_string (padding);
	      ffebad_string (ffesymbol_text (var));
	      ffebad_string (ffesymbol_text (s));
	      ffebad_string ((pad == 1)
			     ? FFECOM_SIZE_UNIT : FFECOM_SIZE_UNITS);
	      ffebad_here (0, ffesymbol_where_line (s), ffesymbol_where_column (s));
	      ffebad_finish ();
	    }
	  stv = ffestorag_new (ffestorag_list_master ());
	  stv->parent = st;	/* Initializations happen in COMMON block. */
	  stv->init = NULL;
	  stv->accretion = NULL;
	  stv->symbol = var;
	  stv->size = size;
	  if (!ffetarget_offset_add (&stv->offset, st->size, pad))
	    {			/* Common block size plus pad, complain if
				   overflow. */
	      ffetarget_offset_overflow (ffesymbol_text (s));
	    }
	  if (!ffetarget_offset_add (&st->size, stv->offset, stv->size))
	    {			/* Adjust size of common block, complain if
				   overflow. */
	      ffetarget_offset_overflow (ffesymbol_text (s));
	    }
	  stv->alignment = alignment;
	  stv->modulo = modulo;
	  stv->type = FFESTORAG_typeCOMMON;
	  stv->basic_type = ffesymbol_basictype (var);
	  stv->kind_type = ffesymbol_kindtype (var);
	  stv->type_symbol = var;
	  stv->is_save = st->is_save;
	  stv->is_init = st->is_init;
	  ffesymbol_set_storage (var, stv);
	  ffesymbol_signal_unreported (var);
	  ffestorag_update (st, var, ffesymbol_basictype (var),
			    ffesymbol_kindtype (var));
	  if (ffesymbol_is_init (var))
	    init = TRUE;	/* Must move inits over to COMMON's
				   ffestorag. */
	}
      if (ffeequiv_layout_cblock (st))
	init = TRUE;
      ffeglobal_pad_common (s, st->modulo, ffesymbol_where_line (s),
			    ffesymbol_where_column (s));
      if (init)
	ffedata_gather (st);	/* Gather subordinate inits into one init. */
      ffesymbol_signal_unreported (s);
      return;
    }
}

/* ffestorag_new -- Create new ffestorag object, append to list

   ffestorag s;
   ffestoragList sl;
   s = ffestorag_new(sl);  */

ffestorag
ffestorag_new (ffestoragList sl)
{
  ffestorag s;

  s = malloc_new_kp (ffe_pool_program_unit (), "ffestorag", sizeof (*s));
  s->next = (ffestorag) &sl->first;
  s->previous = sl->last;
  s->hook = FFECOM_storageNULL;
  s->previous->next = s;
  sl->last = s;
  s->equivs_.first = s->equivs_.last = (ffestorag) &s->equivs_.first;

  return s;
}

/* Report info on LOCAL non-sym-assoc'ed entities if needed.  */

void
ffestorag_report (void)
{
  ffestorag s;

  if (ffestorag_reported_)
    return;

  for (s = ffestorag_list_.first;
       s != (ffestorag) &ffestorag_list_.first;
       s = s->next)
    {
      if (s->symbol == NULL)
	{
	  ffestorag_reported_ = TRUE;
	  fputs ("Storage area: ", dmpout);
	  ffestorag_dump (s);
	  fputc ('\n', dmpout);
	}
    }
}

/* ffestorag_update -- Update type info for ffestorag object

   ffestorag s;	 // existing object
   ffeinfoBasictype bt;	 // basic type for newly added member of object
   ffeinfoKindtype kt;	// kind type for it
   ffestorag_update(s,bt,kt);

   If the existing type for the storage object agrees with the new type
   info, just returns.	If the basic types agree but not the kind types,
   sets the kind type for the object to NONE.  If the basic types
   disagree, sets the kind type to NONE, and the basic type to NONE if the
   basic types both are not CHARACTER, otherwise to ANY.  If the basic
   type for the object already is NONE, it is set to ANY if the new basic
   type is CHARACTER.  Any time a transition is made to ANY and pedantic
   mode is on, a message is issued that mixing CHARACTER and non-CHARACTER
   stuff in the same COMMON/EQUIVALENCE is invalid.  */

void
ffestorag_update (ffestorag s, ffesymbol sym, ffeinfoBasictype bt,
		  ffeinfoKindtype kt)
{
  if (s->basic_type == bt)
    {
      if (s->kind_type == kt)
	return;
      s->kind_type = FFEINFO_kindtypeNONE;
      return;
    }

  switch (s->basic_type)
    {
    case FFEINFO_basictypeANY:
      return;			/* No need to do anything further. */

    case FFEINFO_basictypeCHARACTER:
    any:			/* :::::::::::::::::::: */
      s->basic_type = FFEINFO_basictypeANY;
      s->kind_type = FFEINFO_kindtypeANY;
      if (ffe_is_pedantic ())
	{
	  ffebad_start (FFEBAD_MIXED_TYPES);
	  ffebad_string (ffesymbol_text (s->type_symbol));
	  ffebad_string (ffesymbol_text (sym));
	  ffebad_finish ();
	}
      return;

    default:
      if (bt == FFEINFO_basictypeCHARACTER)
	goto any;		/* :::::::::::::::::::: */
      s->basic_type = FFEINFO_basictypeNONE;
      s->kind_type = FFEINFO_kindtypeNONE;
      return;
    }
}

/* Update INIT flag for storage object.

   If the INIT flag for the <s> object is already TRUE, return.	 Else,
   set it to TRUE and call ffe*_update_init for all contained objects.	*/

void
ffestorag_update_init (ffestorag s)
{
  ffestorag sq;

  if (s->is_init)
    return;

  s->is_init = TRUE;

  if ((s->symbol != NULL)
      && !ffesymbol_is_init (s->symbol))
    ffesymbol_update_init (s->symbol);

  if (s->parent != NULL)
    ffestorag_update_init (s->parent);

  for (sq = s->equivs_.first;
       sq != (ffestorag) &s->equivs_.first;
       sq = ffestorag_next_ (sq))
    {
      if (!sq->is_init)
	ffestorag_update_init (sq);
    }
}

/* Update SAVE flag for storage object.

   If the SAVE flag for the <s> object is already TRUE, return.	 Else,
   set it to TRUE and call ffe*_update_save for all contained objects.	*/

void
ffestorag_update_save (ffestorag s)
{
  ffestorag sq;

  if (s->is_save)
    return;

  s->is_save = TRUE;

  if ((s->symbol != NULL)
      && !ffesymbol_is_save (s->symbol))
    ffesymbol_update_save (s->symbol);

  if (s->parent != NULL)
    ffestorag_update_save (s->parent);

  for (sq = s->equivs_.first;
       sq != (ffestorag) &s->equivs_.first;
       sq = ffestorag_next_ (sq))
    {
      if (!sq->is_save)
	ffestorag_update_save (sq);
    }
}
