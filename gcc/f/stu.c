/* stu.c -- Implementation File (module.c template V1.0)
   Copyright (C) 1995, 1996, 1997 Free Software Foundation, Inc.
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

*/

/* Include files. */

#include "proj.h"
#include "bld.h"
#include "com.h"
#include "equiv.h"
#include "global.h"
#include "info.h"
#include "implic.h"
#include "intrin.h"
#include "stu.h"
#include "storag.h"
#include "sta.h"
#include "symbol.h"
#include "target.h"

/* Externals defined here. */


/* Simple definitions and enumerations. */


/* Internal typedefs. */


/* Private include files. */


/* Internal structure definitions. */


/* Static objects accessed by functions in this module. */


/* Static functions (internal). */

static void ffestu_list_exec_transition_ (ffebld list);
static bool ffestu_symter_end_transition_ (ffebld expr);
static bool ffestu_symter_exec_transition_ (ffebld expr);
static bool ffestu_dummies_transition_ (ffesymbol (*symfunc) (ffesymbol),
					ffebld list);

/* Internal macros. */

#define ffestu_equiv_(s) (((ffesymbol_equiv (s) == NULL)		      \
  || (ffeequiv_common (ffesymbol_equiv (s)) == NULL)) ? FFEINFO_whereLOCAL    \
  : FFEINFO_whereCOMMON)

/* Update symbol info just before end of unit.  */

ffesymbol
ffestu_sym_end_transition (ffesymbol s)
{
  ffeinfoKind skd;
  ffeinfoWhere swh;
  ffeinfoKind nkd;
  ffeinfoWhere nwh;
  ffesymbolAttrs sa;
  ffesymbolAttrs na;
  ffesymbolState ss;
  ffesymbolState ns;
  bool needs_type = TRUE;	/* Implicit type assignment might be
				   necessary. */

  assert (s != NULL);
  ss = ffesymbol_state (s);
  sa = ffesymbol_attrs (s);
  skd = ffesymbol_kind (s);
  swh = ffesymbol_where (s);

  switch (ss)
    {
    case FFESYMBOL_stateUNCERTAIN:
      if ((swh == FFEINFO_whereDUMMY)
	  && (ffesymbol_numentries (s) == 0))
	{			/* Not actually in any dummy list! */
	  ffesymbol_error (s, ffesta_tokens[0]);
	  return s;
	}
      else if (((swh == FFEINFO_whereLOCAL)
		|| (swh == FFEINFO_whereNONE))
	       && (skd == FFEINFO_kindENTITY)
	       && ffestu_symter_end_transition_ (ffesymbol_dims (s)))
	{			/* Bad dimension expressions. */
	  ffesymbol_error (s, NULL);
	  return s;
	}
      break;

    case FFESYMBOL_stateUNDERSTOOD:
      if ((swh == FFEINFO_whereLOCAL)
	  && ((skd == FFEINFO_kindFUNCTION)
	      || (skd == FFEINFO_kindSUBROUTINE)))
	{
	  int n_args;
	  ffebld list;
	  ffebld item;
	  ffeglobalArgSummary as;
	  ffeinfoBasictype bt;
	  ffeinfoKindtype kt;
	  bool array;
	  const char *name = NULL;

	  ffestu_dummies_transition_ (ffecom_sym_end_transition,
				      ffesymbol_dummyargs (s));

	  n_args = ffebld_list_length (ffesymbol_dummyargs (s));
	  ffeglobal_proc_def_nargs (s, n_args);
	  for (list = ffesymbol_dummyargs (s), n_args = 0;
	       list != NULL;
	       list = ffebld_trail (list), ++n_args)
	    {
	      item = ffebld_head (list);
	      array = FALSE;
	      if (item != NULL)
		{
		  bt = ffeinfo_basictype (ffebld_info (item));
		  kt = ffeinfo_kindtype (ffebld_info (item));
		  array = (ffeinfo_rank (ffebld_info (item)) > 0);
		  switch (ffebld_op (item))
		    {
		    case FFEBLD_opSTAR:
		      as = FFEGLOBAL_argsummaryALTRTN;
		      break;

		    case FFEBLD_opSYMTER:
		      name = ffesymbol_text (ffebld_symter (item));
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

		      /* Fall through.  */
		    default:
		      if (bt == FFEINFO_basictypeCHARACTER)
			as = FFEGLOBAL_argsummaryDESCR;
		      else
			as = FFEGLOBAL_argsummaryREF;
		      break;
		    }
		}
	      else
		{
		  as = FFEGLOBAL_argsummaryNONE;
		  bt = FFEINFO_basictypeNONE;
		  kt = FFEINFO_kindtypeNONE;
		}
	      ffeglobal_proc_def_arg (s, n_args, name, as, bt, kt, array);
	    }
	}
      else if (swh == FFEINFO_whereDUMMY)
	{
	  if (ffesymbol_numentries (s) == 0)
	    {			/* Not actually in any dummy list! */
	      ffesymbol_error (s, ffesta_tokens[0]);
	      return s;
	    }
	  if (ffestu_symter_end_transition_ (ffesymbol_dims (s)))
	    {			/* Bad dimension expressions. */
	      ffesymbol_error (s, NULL);
	      return s;
	    }
	}
      else if ((swh == FFEINFO_whereLOCAL)
	       && ffestu_symter_end_transition_ (ffesymbol_dims (s)))
	{			/* Bad dimension expressions. */
	  ffesymbol_error (s, NULL);
	  return s;
	}

      ffestorag_end_layout (s);
      ffesymbol_signal_unreported (s);	/* For debugging purposes. */
      return s;

    default:
      assert ("bad status" == NULL);
      return s;
    }

  ns = FFESYMBOL_stateUNDERSTOOD;
  na = sa = ffesymbol_attrs (s);

  assert (!(sa & ~(FFESYMBOL_attrsACTUALARG
		   | FFESYMBOL_attrsADJUSTABLE
		   | FFESYMBOL_attrsANYLEN
		   | FFESYMBOL_attrsARRAY
		   | FFESYMBOL_attrsDUMMY
		   | FFESYMBOL_attrsEXTERNAL
		   | FFESYMBOL_attrsSFARG
		   | FFESYMBOL_attrsTYPE)));

  nkd = skd;
  nwh = swh;

  /* Figure out what kind of object we've got based on previous declarations
     of or references to the object. */

  if (sa & FFESYMBOL_attrsEXTERNAL)
    {
      assert (!(sa & ~(FFESYMBOL_attrsACTUALARG
		       | FFESYMBOL_attrsDUMMY
		       | FFESYMBOL_attrsEXTERNAL
		       | FFESYMBOL_attrsTYPE)));

      if (sa & FFESYMBOL_attrsTYPE)
	nwh = FFEINFO_whereGLOBAL;
      else
	/* Not TYPE. */
	{
	  if (sa & FFESYMBOL_attrsDUMMY)
	    {			/* Not TYPE. */
	      ns = FFESYMBOL_stateUNCERTAIN;	/* FUNCTION/SUBROUTINE. */
	      needs_type = FALSE;	/* Don't assign type to SUBROUTINE! */
	    }
	  else if (sa & FFESYMBOL_attrsACTUALARG)
	    {			/* Not DUMMY or TYPE. */
	      ns = FFESYMBOL_stateUNCERTAIN;	/* FUNCTION/SUBROUTINE. */
	      needs_type = FALSE;	/* Don't assign type to SUBROUTINE! */
	    }
	  else
	    /* Not ACTUALARG, DUMMY, or TYPE. */
	    {			/* This is an assumption, essentially. */
	      nkd = FFEINFO_kindBLOCKDATA;
	      nwh = FFEINFO_whereGLOBAL;
	      needs_type = FALSE;
	    }
	}
    }
  else if (sa & FFESYMBOL_attrsDUMMY)
    {
      assert (!(sa & FFESYMBOL_attrsEXTERNAL));	/* Handled above. */
      assert (!(sa & ~(FFESYMBOL_attrsDUMMY
		       | FFESYMBOL_attrsEXTERNAL
		       | FFESYMBOL_attrsTYPE)));

      /* Honestly, this appears to be a guess.  I can't find anyplace in the
	 standard that makes clear whether this unreferenced dummy argument
	 is an ENTITY or a FUNCTION.  And yet, for the f2c interface, picking
	 one is critical for CHARACTER entities because it determines whether
	 to expect an additional argument specifying the length of an ENTITY
	 that is not expected (or needed) for a FUNCTION.  HOWEVER, F90 makes
	 this guess a correct one, and it does seem that the Section 18 Notes
	 in Appendix B of F77 make it clear the F77 standard at least
	 intended to make this guess correct as well, so this seems ok.  */

      nkd = FFEINFO_kindENTITY;
    }
  else if (sa & FFESYMBOL_attrsARRAY)
    {
      assert (!(sa & ~(FFESYMBOL_attrsARRAY
		       | FFESYMBOL_attrsADJUSTABLE
		       | FFESYMBOL_attrsTYPE)));

      if (ffestu_symter_end_transition_ (ffesymbol_dims (s)))
	{
	  ffesymbol_error (s, NULL);
	  return s;
	}

      if (sa & FFESYMBOL_attrsADJUSTABLE)
	{			/* Not actually in any dummy list! */
	  if (ffe_is_pedantic ()
	      && ffebad_start_msg ("Local adjustable symbol `%A' at %0",
				   FFEBAD_severityPEDANTIC))
	    {
	      ffebad_string (ffesymbol_text (s));
	      ffebad_here (0, ffesymbol_where_line (s),
			   ffesymbol_where_column (s));
	      ffebad_finish ();
	    }
	}
      nwh = FFEINFO_whereLOCAL;
    }
  else if (sa & FFESYMBOL_attrsSFARG)
    {
      assert (!(sa & ~(FFESYMBOL_attrsSFARG
		       | FFESYMBOL_attrsTYPE)));

      nwh = FFEINFO_whereLOCAL;
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
	{			/* Can't touch this. */
	  ffesymbol_signal_change (s);
	  ffesymbol_set_state (s, FFESYMBOL_stateUNDERSTOOD);
	  ffesymbol_resolve_intrin (s);
	  s = ffecom_sym_learned (s);
	  ffesymbol_reference (s, NULL, FALSE);
	  ffestorag_end_layout (s);
	  ffesymbol_signal_unreported (s);	/* For debugging purposes. */
	  return s;
	}

      nkd = FFEINFO_kindENTITY;
      nwh = FFEINFO_whereLOCAL;
    }
  else
    assert ("unexpected attribute set" == NULL);

  /* Now see what we've got for a new object: NONE means a new error cropped
     up; ANY means an old error to be ignored; otherwise, everything's ok,
     update the object (symbol) and continue on. */

  if (na == FFESYMBOL_attrsetNONE)
    ffesymbol_error (s, ffesta_tokens[0]);
  else if (!(na & FFESYMBOL_attrsANY))
    {
      ffesymbol_signal_change (s);
      ffesymbol_set_attrs (s, na);	/* Establish new info. */
      ffesymbol_set_state (s, ns);
      ffesymbol_set_info (s,
			  ffeinfo_new (ffesymbol_basictype (s),
				       ffesymbol_kindtype (s),
				       ffesymbol_rank (s),
				       nkd,
				       nwh,
				       ffesymbol_size (s)));
      if (needs_type && !ffeimplic_establish_symbol (s))
	ffesymbol_error (s, ffesta_tokens[0]);
      else
	ffesymbol_resolve_intrin (s);
      s = ffecom_sym_learned (s);
      ffesymbol_reference (s, NULL, FALSE);
      ffestorag_end_layout (s);
      ffesymbol_signal_unreported (s);	/* For debugging purposes. */
    }

  return s;
}

/* ffestu_sym_exec_transition -- Update symbol just before first exec stmt

   ffesymbol s;
   ffestu_sym_exec_transition(s);  */

ffesymbol
ffestu_sym_exec_transition (ffesymbol s)
{
  ffeinfoKind skd;
  ffeinfoWhere swh;
  ffeinfoKind nkd;
  ffeinfoWhere nwh;
  ffesymbolAttrs sa;
  ffesymbolAttrs na;
  ffesymbolState ss;
  ffesymbolState ns;
  ffeintrinGen gen;
  ffeintrinSpec spec;
  ffeintrinImp imp;
  bool needs_type = TRUE;	/* Implicit type assignment might be
				   necessary. */
  bool resolve_intrin = TRUE;	/* Might need to resolve intrinsic. */

  assert (s != NULL);

  sa = ffesymbol_attrs (s);
  skd = ffesymbol_kind (s);
  swh = ffesymbol_where (s);
  ss = ffesymbol_state (s);

  switch (ss)
    {
    case FFESYMBOL_stateNONE:
      return s;			/* Assume caller will handle it. */

    case FFESYMBOL_stateSEEN:
      break;

    case FFESYMBOL_stateUNCERTAIN:
      ffestorag_exec_layout (s);
      return s;			/* Already processed this one, or not
				   necessary. */

    case FFESYMBOL_stateUNDERSTOOD:
      if (skd == FFEINFO_kindNAMELIST)
	{
	  ffebld_end_list (ffesymbol_ptr_to_listbottom (s));
	  ffestu_list_exec_transition_ (ffesymbol_namelist (s));
	}
      else if ((swh == FFEINFO_whereLOCAL)
	       && ((skd == FFEINFO_kindFUNCTION)
		   || (skd == FFEINFO_kindSUBROUTINE)))
	{
	  ffestu_dummies_transition_ (ffecom_sym_exec_transition,
				      ffesymbol_dummyargs (s));
	  if ((skd == FFEINFO_kindFUNCTION)
	      && !ffeimplic_establish_symbol (s))
	    ffesymbol_error (s, ffesta_tokens[0]);
	}

      ffesymbol_reference (s, NULL, FALSE);
      ffestorag_exec_layout (s);
      ffesymbol_signal_unreported (s);	/* For debugging purposes. */
      return s;

    default:
      assert ("bad status" == NULL);
      return s;
    }

  ns = FFESYMBOL_stateUNDERSTOOD;	/* Only a few UNCERTAIN exceptions. */

  na = sa;
  nkd = skd;
  nwh = swh;

  assert (!(sa & FFESYMBOL_attrsANY));

  if (sa & FFESYMBOL_attrsCOMMON)
    {
      assert (!(sa & ~(FFESYMBOL_attrsADJUSTS
		       | FFESYMBOL_attrsARRAY
		       | FFESYMBOL_attrsCOMMON
		       | FFESYMBOL_attrsEQUIV
		       | FFESYMBOL_attrsINIT
		       | FFESYMBOL_attrsNAMELIST
		       | FFESYMBOL_attrsSFARG
		       | FFESYMBOL_attrsTYPE)));

      nkd = FFEINFO_kindENTITY;
      nwh = FFEINFO_whereCOMMON;
    }
  else if (sa & FFESYMBOL_attrsRESULT)
    {				/* Result variable for function. */
      assert (!(sa & ~(FFESYMBOL_attrsANYLEN
		       | FFESYMBOL_attrsRESULT
		       | FFESYMBOL_attrsSFARG
		       | FFESYMBOL_attrsTYPE)));

      nkd = FFEINFO_kindENTITY;
      nwh = FFEINFO_whereRESULT;
    }
  else if (sa & FFESYMBOL_attrsSFUNC)
    {				/* Statement function. */
      assert (!(sa & ~(FFESYMBOL_attrsSFUNC
		       | FFESYMBOL_attrsTYPE)));

      nkd = FFEINFO_kindFUNCTION;
      nwh = FFEINFO_whereCONSTANT;
    }
  else if (sa & FFESYMBOL_attrsEXTERNAL)
    {
      assert (!(sa & ~(FFESYMBOL_attrsDUMMY
		       | FFESYMBOL_attrsEXTERNAL
		       | FFESYMBOL_attrsTYPE)));

      if (sa & FFESYMBOL_attrsTYPE)
	{
	  nkd = FFEINFO_kindFUNCTION;

	  if (sa & FFESYMBOL_attrsDUMMY)
	    nwh = FFEINFO_whereDUMMY;
	  else
	    {
	      if (ffesta_is_entry_valid)
		{
		  nwh = FFEINFO_whereNONE;	/* DUMMY, GLOBAL. */
		  ns = FFESYMBOL_stateUNCERTAIN;
		}
	      else
		nwh = FFEINFO_whereGLOBAL;
	    }
	}
      else
	/* No TYPE. */
	{
	  nkd = FFEINFO_kindNONE;	/* FUNCTION, SUBROUTINE, BLOCKDATA. */
	  needs_type = FALSE;	/* Only gets type if FUNCTION. */
	  ns = FFESYMBOL_stateUNCERTAIN;

	  if (sa & FFESYMBOL_attrsDUMMY)
	    nwh = FFEINFO_whereDUMMY;	/* Not BLOCKDATA. */
	  else
	    {
	      if (ffesta_is_entry_valid)
		nwh = FFEINFO_whereNONE;	/* DUMMY, GLOBAL. */
	      else
		nwh = FFEINFO_whereGLOBAL;
	    }
	}
    }
  else if (sa & FFESYMBOL_attrsDUMMY)
    {
      assert (!(sa & FFESYMBOL_attrsEXTERNAL));	/* Handled above. */
      assert (!(sa & ~(FFESYMBOL_attrsADJUSTABLE	/* Possible. */
		       | FFESYMBOL_attrsADJUSTS	/* Possible. */
		       | FFESYMBOL_attrsANYLEN	/* Possible. */
		       | FFESYMBOL_attrsANYSIZE	/* Possible. */
		       | FFESYMBOL_attrsARRAY	/* Possible. */
		       | FFESYMBOL_attrsDUMMY	/* Have it. */
		       | FFESYMBOL_attrsEXTERNAL
		       | FFESYMBOL_attrsSFARG	/* Possible. */
		       | FFESYMBOL_attrsTYPE)));	/* Possible. */

      nwh = FFEINFO_whereDUMMY;

      if (ffestu_symter_exec_transition_ (ffesymbol_dims (s)))
	na = FFESYMBOL_attrsetNONE;

      if (sa & (FFESYMBOL_attrsADJUSTS
		| FFESYMBOL_attrsARRAY
		| FFESYMBOL_attrsANYLEN
		| FFESYMBOL_attrsNAMELIST
		| FFESYMBOL_attrsSFARG))
	nkd = FFEINFO_kindENTITY;
      else if (sa & FFESYMBOL_attrsDUMMY)	/* Still okay. */
	{
	  if (!(sa & FFESYMBOL_attrsTYPE))
	    needs_type = FALSE;	/* Don't assign type to SUBROUTINE! */
	  nkd = FFEINFO_kindNONE;	/* ENTITY, FUNCTION, SUBROUTINE. */
	  ns = FFESYMBOL_stateUNCERTAIN;
	}
    }
  else if (sa & FFESYMBOL_attrsADJUSTS)
    {				/* Must be DUMMY or COMMON at some point. */
      assert (!(sa & (FFESYMBOL_attrsCOMMON
		      | FFESYMBOL_attrsDUMMY)));	/* Handled above. */
      assert (!(sa & ~(FFESYMBOL_attrsADJUSTS	/* Have it. */
		       | FFESYMBOL_attrsCOMMON
		       | FFESYMBOL_attrsDUMMY
		       | FFESYMBOL_attrsEQUIV	/* Possible. */
		       | FFESYMBOL_attrsINIT	/* Possible. */
		       | FFESYMBOL_attrsNAMELIST	/* Possible. */
		       | FFESYMBOL_attrsSFARG	/* Possible. */
		       | FFESYMBOL_attrsTYPE)));	/* Possible. */

      nkd = FFEINFO_kindENTITY;

      if (sa & FFESYMBOL_attrsEQUIV)
	{
	  if ((ffesymbol_equiv (s) == NULL)
	      || (ffeequiv_common (ffesymbol_equiv (s)) == NULL))
	    na = FFESYMBOL_attrsetNONE;	/* Not equiv'd into COMMON. */
	  else
	    nwh = FFEINFO_whereCOMMON;
	}
      else if (!ffesta_is_entry_valid
	       || (sa & (FFESYMBOL_attrsINIT
			 | FFESYMBOL_attrsNAMELIST)))
	na = FFESYMBOL_attrsetNONE;
      else
	nwh = FFEINFO_whereDUMMY;
    }
  else if (sa & FFESYMBOL_attrsSAVE)
    {
      assert (!(sa & ~(FFESYMBOL_attrsARRAY
		       | FFESYMBOL_attrsEQUIV
		       | FFESYMBOL_attrsINIT
		       | FFESYMBOL_attrsNAMELIST
		       | FFESYMBOL_attrsSAVE
		       | FFESYMBOL_attrsSFARG
		       | FFESYMBOL_attrsTYPE)));

      nkd = FFEINFO_kindENTITY;
      nwh = FFEINFO_whereLOCAL;
    }
  else if (sa & FFESYMBOL_attrsEQUIV)
    {
      assert (!(sa & FFESYMBOL_attrsCOMMON));	/* Handled above. */
      assert (!(sa & ~(FFESYMBOL_attrsADJUSTS	/* Possible. */
		       | FFESYMBOL_attrsARRAY	/* Possible. */
		       | FFESYMBOL_attrsCOMMON
		       | FFESYMBOL_attrsEQUIV	/* Have it. */
		       | FFESYMBOL_attrsINIT	/* Possible. */
		       | FFESYMBOL_attrsNAMELIST	/* Possible. */
		       | FFESYMBOL_attrsSAVE	/* Possible. */
		       | FFESYMBOL_attrsSFARG	/* Possible. */
		       | FFESYMBOL_attrsTYPE)));	/* Possible. */

      nkd = FFEINFO_kindENTITY;
      nwh = ffestu_equiv_ (s);
    }
  else if (sa & FFESYMBOL_attrsNAMELIST)
    {
      assert (!(sa & (FFESYMBOL_attrsADJUSTS
		      | FFESYMBOL_attrsCOMMON
		      | FFESYMBOL_attrsEQUIV
		      | FFESYMBOL_attrsSAVE)));	/* Handled above. */
      assert (!(sa & ~(FFESYMBOL_attrsADJUSTS
		       | FFESYMBOL_attrsARRAY	/* Possible. */
		       | FFESYMBOL_attrsCOMMON
		       | FFESYMBOL_attrsEQUIV
		       | FFESYMBOL_attrsINIT	/* Possible. */
		       | FFESYMBOL_attrsNAMELIST	/* Have it. */
		       | FFESYMBOL_attrsSAVE
		       | FFESYMBOL_attrsSFARG	/* Possible. */
		       | FFESYMBOL_attrsTYPE)));	/* Possible. */

      nkd = FFEINFO_kindENTITY;
      nwh = FFEINFO_whereLOCAL;
    }
  else if (sa & FFESYMBOL_attrsINIT)
    {
      assert (!(sa & (FFESYMBOL_attrsADJUSTS
		      | FFESYMBOL_attrsCOMMON
		      | FFESYMBOL_attrsEQUIV
		      | FFESYMBOL_attrsNAMELIST
		      | FFESYMBOL_attrsSAVE)));	/* Handled above. */
      assert (!(sa & ~(FFESYMBOL_attrsADJUSTS
		       | FFESYMBOL_attrsARRAY	/* Possible. */
		       | FFESYMBOL_attrsCOMMON
		       | FFESYMBOL_attrsEQUIV
		       | FFESYMBOL_attrsINIT	/* Have it. */
		       | FFESYMBOL_attrsNAMELIST
		       | FFESYMBOL_attrsSAVE
		       | FFESYMBOL_attrsSFARG	/* Possible. */
		       | FFESYMBOL_attrsTYPE)));	/* Possible. */

      nkd = FFEINFO_kindENTITY;
      nwh = FFEINFO_whereLOCAL;
    }
  else if (sa & FFESYMBOL_attrsSFARG)
    {
      assert (!(sa & (FFESYMBOL_attrsADJUSTS
		      | FFESYMBOL_attrsCOMMON
		      | FFESYMBOL_attrsDUMMY
		      | FFESYMBOL_attrsEQUIV
		      | FFESYMBOL_attrsINIT
		      | FFESYMBOL_attrsNAMELIST
		      | FFESYMBOL_attrsRESULT
		      | FFESYMBOL_attrsSAVE)));	/* Handled above. */
      assert (!(sa & ~(FFESYMBOL_attrsADJUSTS
		       | FFESYMBOL_attrsCOMMON
		       | FFESYMBOL_attrsDUMMY
		       | FFESYMBOL_attrsEQUIV
		       | FFESYMBOL_attrsINIT
		       | FFESYMBOL_attrsNAMELIST
		       | FFESYMBOL_attrsRESULT
		       | FFESYMBOL_attrsSAVE
		       | FFESYMBOL_attrsSFARG	/* Have it. */
		       | FFESYMBOL_attrsTYPE)));	/* Possible. */

      nkd = FFEINFO_kindENTITY;

      if (ffesta_is_entry_valid)
	{
	  nwh = FFEINFO_whereNONE;	/* DUMMY, LOCAL. */
	  ns = FFESYMBOL_stateUNCERTAIN;
	}
      else
	nwh = FFEINFO_whereLOCAL;
    }
  else if (sa & (FFESYMBOL_attrsADJUSTABLE | FFESYMBOL_attrsANYSIZE))
    {
      assert (!(sa & ~(FFESYMBOL_attrsADJUSTABLE
		       | FFESYMBOL_attrsANYLEN
		       | FFESYMBOL_attrsANYSIZE
		       | FFESYMBOL_attrsARRAY
		       | FFESYMBOL_attrsTYPE)));

      nkd = FFEINFO_kindENTITY;

      if (ffestu_symter_exec_transition_ (ffesymbol_dims (s)))
	na = FFESYMBOL_attrsetNONE;

      if (sa & (FFESYMBOL_attrsANYLEN | FFESYMBOL_attrsANYSIZE))
	nwh = FFEINFO_whereDUMMY;
      else if (sa & (FFESYMBOL_attrsADJUSTABLE | FFESYMBOL_attrsANYSIZE))
	/* Still okay.  */
	{
	  nwh = FFEINFO_whereNONE;	/* DUMMY, LOCAL. */
	  ns = FFESYMBOL_stateUNCERTAIN;
	}
    }
  else if (sa & FFESYMBOL_attrsARRAY)
    {
      assert (!(sa & (FFESYMBOL_attrsADJUSTABLE
		      | FFESYMBOL_attrsANYSIZE
		      | FFESYMBOL_attrsCOMMON
		      | FFESYMBOL_attrsDUMMY
		      | FFESYMBOL_attrsEQUIV
		      | FFESYMBOL_attrsINIT
		      | FFESYMBOL_attrsNAMELIST
		      | FFESYMBOL_attrsSAVE)));	/* Handled above. */
      assert (!(sa & ~(FFESYMBOL_attrsADJUSTABLE
		       | FFESYMBOL_attrsANYLEN	/* Possible. */
		       | FFESYMBOL_attrsANYSIZE
		       | FFESYMBOL_attrsARRAY	/* Have it. */
		       | FFESYMBOL_attrsCOMMON
		       | FFESYMBOL_attrsDUMMY
		       | FFESYMBOL_attrsEQUIV
		       | FFESYMBOL_attrsINIT
		       | FFESYMBOL_attrsNAMELIST
		       | FFESYMBOL_attrsSAVE
		       | FFESYMBOL_attrsTYPE)));	/* Possible. */

      nkd = FFEINFO_kindENTITY;

      if (sa & FFESYMBOL_attrsANYLEN)
	{
	  assert (ffesta_is_entry_valid);	/* Already diagnosed. */
	  nwh = FFEINFO_whereDUMMY;
	}
      else
	{
	  if (ffesta_is_entry_valid)
	    {
	      nwh = FFEINFO_whereNONE;	/* DUMMY, LOCAL. */
	      ns = FFESYMBOL_stateUNCERTAIN;
	    }
	  else
	    nwh = FFEINFO_whereLOCAL;
	}
    }
  else if (sa & FFESYMBOL_attrsANYLEN)
    {
      assert (!(sa & (FFESYMBOL_attrsADJUSTABLE
		      | FFESYMBOL_attrsANYSIZE
		      | FFESYMBOL_attrsARRAY
		      | FFESYMBOL_attrsDUMMY
		      | FFESYMBOL_attrsRESULT)));	/* Handled above. */
      assert (!(sa & ~(FFESYMBOL_attrsADJUSTABLE
		       | FFESYMBOL_attrsANYLEN	/* Have it. */
		       | FFESYMBOL_attrsANYSIZE
		       | FFESYMBOL_attrsARRAY
		       | FFESYMBOL_attrsDUMMY
		       | FFESYMBOL_attrsRESULT
		       | FFESYMBOL_attrsTYPE)));	/* Have it too. */

      if (ffesta_is_entry_valid)
	{
	  nkd = FFEINFO_kindNONE;	/* ENTITY, FUNCTION. */
	  nwh = FFEINFO_whereNONE;	/* DUMMY, INTRINSIC, RESULT. */
	  ns = FFESYMBOL_stateUNCERTAIN;
	  resolve_intrin = FALSE;
	}
      else if (ffeintrin_is_intrinsic (ffesymbol_text (s), NULL, FALSE,
				       &gen, &spec, &imp))
	{
	  ffesymbol_signal_change (s);
	  ffesymbol_set_state (s, FFESYMBOL_stateUNDERSTOOD);
	  ffesymbol_set_generic (s, gen);
	  ffesymbol_set_specific (s, spec);
	  ffesymbol_set_implementation (s, imp);
	  ffesymbol_set_info (s,
			      ffeinfo_new (FFEINFO_basictypeNONE,
					   FFEINFO_kindtypeNONE,
					   0,
					   FFEINFO_kindNONE,
					   FFEINFO_whereINTRINSIC,
					   FFETARGET_charactersizeNONE));
	  ffesymbol_resolve_intrin (s);
	  ffesymbol_reference (s, NULL, FALSE);
	  ffestorag_exec_layout (s);
	  ffesymbol_signal_unreported (s);	/* For debugging purposes. */
	  return s;
	}
      else
	{			/* SPECIAL: can't have CHAR*(*) var in
				   PROGRAM/BLOCKDATA, unless it isn't
				   referenced anywhere in the code. */
	  ffesymbol_signal_change (s);	/* Can't touch this. */
	  ffesymbol_set_state (s, FFESYMBOL_stateUNDERSTOOD);
	  ffesymbol_resolve_intrin (s);
	  ffesymbol_reference (s, NULL, FALSE);
	  ffestorag_exec_layout (s);
	  ffesymbol_signal_unreported (s);	/* For debugging purposes. */
	  return s;
	}
    }
  else if (sa & FFESYMBOL_attrsTYPE)
    {
      assert (!(sa & (FFESYMBOL_attrsADJUSTABLE
		      | FFESYMBOL_attrsADJUSTS
		      | FFESYMBOL_attrsANYLEN
		      | FFESYMBOL_attrsANYSIZE
		      | FFESYMBOL_attrsARRAY
		      | FFESYMBOL_attrsCOMMON
		      | FFESYMBOL_attrsDUMMY
		      | FFESYMBOL_attrsEQUIV
		      | FFESYMBOL_attrsEXTERNAL
		      | FFESYMBOL_attrsINIT
		      | FFESYMBOL_attrsNAMELIST
		      | FFESYMBOL_attrsRESULT
		      | FFESYMBOL_attrsSAVE
		      | FFESYMBOL_attrsSFARG
		      | FFESYMBOL_attrsSFUNC)));
      assert (!(sa & ~(FFESYMBOL_attrsADJUSTABLE
		       | FFESYMBOL_attrsADJUSTS
		       | FFESYMBOL_attrsANYLEN
		       | FFESYMBOL_attrsANYSIZE
		       | FFESYMBOL_attrsARRAY
		       | FFESYMBOL_attrsCOMMON
		       | FFESYMBOL_attrsDUMMY
		       | FFESYMBOL_attrsEQUIV
		       | FFESYMBOL_attrsEXTERNAL
		       | FFESYMBOL_attrsINIT
		       | FFESYMBOL_attrsINTRINSIC	/* UNDERSTOOD. */
		       | FFESYMBOL_attrsNAMELIST
		       | FFESYMBOL_attrsRESULT
		       | FFESYMBOL_attrsSAVE
		       | FFESYMBOL_attrsSFARG
		       | FFESYMBOL_attrsSFUNC
		       | FFESYMBOL_attrsTYPE)));	/* Have it. */

      nkd = FFEINFO_kindNONE;	/* ENTITY, FUNCTION. */
      nwh = FFEINFO_whereNONE;	/* DUMMY, GLOBAL, INTRINSIC, LOCAL, RESULT. */
      ns = FFESYMBOL_stateUNCERTAIN;
      resolve_intrin = FALSE;
    }
  else if (sa & (FFESYMBOL_attrsCBLOCK | FFESYMBOL_attrsSAVECBLOCK))
    {				/* COMMON block. */
      assert (!(sa & ~(FFESYMBOL_attrsCBLOCK
		       | FFESYMBOL_attrsSAVECBLOCK)));

      if (sa & FFESYMBOL_attrsCBLOCK)
	ffebld_end_list (ffesymbol_ptr_to_listbottom (s));
      else
	ffesymbol_set_commonlist (s, NULL);
      ffestu_list_exec_transition_ (ffesymbol_commonlist (s));
      nkd = FFEINFO_kindCOMMON;
      nwh = FFEINFO_whereLOCAL;
      needs_type = FALSE;
    }
  else
    {				/* First seen in stmt func definition. */
      assert (sa == FFESYMBOL_attrsetNONE);
      assert ("Why are we here again?" == NULL);	/* ~~~~~ */

      nkd = FFEINFO_kindNONE;	/* ENTITY, FUNCTION. */
      nwh = FFEINFO_whereNONE;	/* DUMMY, GLOBAL, LOCAL. */
      ns = FFESYMBOL_stateUNCERTAIN;	/* Will get repromoted by caller. */
      needs_type = FALSE;
    }

  if (na == FFESYMBOL_attrsetNONE)
    ffesymbol_error (s, ffesta_tokens[0]);
  else if (!(na & FFESYMBOL_attrsANY)
	   && (needs_type || (nkd != skd) || (nwh != swh)
	       || (na != sa) || (ns != ss)))
    {
      ffesymbol_signal_change (s);
      ffesymbol_set_attrs (s, na);	/* Establish new info. */
      ffesymbol_set_state (s, ns);
      if ((ffesymbol_common (s) == NULL)
	  && (ffesymbol_equiv (s) != NULL))
	ffesymbol_set_common (s, ffeequiv_common (ffesymbol_equiv (s)));
      ffesymbol_set_info (s,
			  ffeinfo_new (ffesymbol_basictype (s),
				       ffesymbol_kindtype (s),
				       ffesymbol_rank (s),
				       nkd,
				       nwh,
				       ffesymbol_size (s)));
      if (needs_type && !ffeimplic_establish_symbol (s))
	ffesymbol_error (s, ffesta_tokens[0]);
      else if (resolve_intrin)
	ffesymbol_resolve_intrin (s);
      ffesymbol_reference (s, NULL, FALSE);
      ffestorag_exec_layout (s);
      ffesymbol_signal_unreported (s);	/* For debugging purposes. */
    }

  return s;
}

/* ffestu_list_exec_transition_ -- Update SYMTERs in ITEM list w/in symbol

   ffebld list;
   ffestu_list_exec_transition_(list);

   list contains an FFEBLD_opITEM list of SYMTERs (possibly STARs and
   other things, too, but we'll ignore the known ones).	 For each SYMTER,
   we run sym_exec_transition_ on the corresponding ffesymbol (a recursive
   call, since that's the function that's calling us) to update it's
   information.	 Then we copy that information into the SYMTER.

   Make sure we don't get called recursively ourselves!	 */

static void
ffestu_list_exec_transition_ (ffebld list)
{
  static bool in_progress = FALSE;
  ffebld item;
  ffesymbol symbol;

  assert (!in_progress);
  in_progress = TRUE;

  for (; list != NULL; list = ffebld_trail (list))
    {
      if ((item = ffebld_head (list)) == NULL)
	continue;		/* Try next item. */

      switch (ffebld_op (item))
	{
	case FFEBLD_opSTAR:
	  break;

	case FFEBLD_opSYMTER:
	  symbol = ffebld_symter (item);
	  if (symbol == NULL)
	    break;		/* Detached from stmt func dummy list. */
	  symbol = ffecom_sym_exec_transition (symbol);
	  assert (ffesymbol_kind (symbol) != FFEINFO_kindNONE);
	  assert (ffesymbol_where (symbol) != FFEINFO_whereNONE);
	  ffebld_set_info (item, ffesymbol_info (symbol));
	  break;

	default:
	  assert ("Unexpected item on list" == NULL);
	  break;
	}
    }

  in_progress = FALSE;
}

/* ffestu_symter_end_transition_ -- Update SYMTERs in expr w/in symbol

   ffebld expr;
   ffestu_symter_end_transition_(expr);

   Any SYMTER in expr's tree with whereNONE gets updated to the
   (recursively transitioned) sym it identifies (DUMMY or COMMON).  */

static bool
ffestu_symter_end_transition_ (ffebld expr)
{
  ffesymbol symbol;
  bool any = FALSE;

  /* Label used for tail recursion (reset expr and go here instead of calling
     self). */

tail:				/* :::::::::::::::::::: */

  if (expr == NULL)
    return any;

  switch (ffebld_op (expr))
    {
    case FFEBLD_opITEM:
      while (ffebld_trail (expr) != NULL)
	{
	  if (ffestu_symter_end_transition_ (ffebld_head (expr)))
	    any = TRUE;
	  expr = ffebld_trail (expr);
	}
      expr = ffebld_head (expr);
      goto tail;		/* :::::::::::::::::::: */

    case FFEBLD_opSYMTER:
      symbol = ffecom_sym_end_transition (ffebld_symter (expr));
      if ((symbol != NULL)
	  && ffesymbol_attr (symbol, FFESYMBOL_attrANY))
	any = TRUE;
      ffebld_set_info (expr, ffesymbol_info (symbol));
      break;

    case FFEBLD_opANY:
      return TRUE;

    default:
      break;
    }

  switch (ffebld_arity (expr))
    {
    case 2:
      if (ffestu_symter_end_transition_ (ffebld_left (expr)))
	any = TRUE;
      expr = ffebld_right (expr);
      goto tail;		/* :::::::::::::::::::: */

    case 1:
      expr = ffebld_left (expr);
      goto tail;		/* :::::::::::::::::::: */

    default:
      break;
    }

  return any;
}

/* ffestu_symter_exec_transition_ -- Update SYMTERs in expr w/in symbol

   ffebld expr;
   ffestu_symter_exec_transition_(expr);

   Any SYMTER in expr's tree with whereNONE gets updated to the
   (recursively transitioned) sym it identifies (DUMMY or COMMON).  */

static bool
ffestu_symter_exec_transition_ (ffebld expr)
{
  ffesymbol symbol;
  bool any = FALSE;

  /* Label used for tail recursion (reset expr and go here instead of calling
     self). */

tail:				/* :::::::::::::::::::: */

  if (expr == NULL)
    return any;

  switch (ffebld_op (expr))
    {
    case FFEBLD_opITEM:
      while (ffebld_trail (expr) != NULL)
	{
	  if (ffestu_symter_exec_transition_ (ffebld_head (expr)))
	    any = TRUE;
	  expr = ffebld_trail (expr);
	}
      expr = ffebld_head (expr);
      goto tail;		/* :::::::::::::::::::: */

    case FFEBLD_opSYMTER:
      symbol = ffecom_sym_exec_transition (ffebld_symter (expr));
      if ((symbol != NULL)
	  && ffesymbol_attr (symbol, FFESYMBOL_attrANY))
	any = TRUE;
      ffebld_set_info (expr, ffesymbol_info (symbol));
      break;

    case FFEBLD_opANY:
      return TRUE;

    default:
      break;
    }

  switch (ffebld_arity (expr))
    {
    case 2:
      if (ffestu_symter_exec_transition_ (ffebld_left (expr)))
	any = TRUE;
      expr = ffebld_right (expr);
      goto tail;		/* :::::::::::::::::::: */

    case 1:
      expr = ffebld_left (expr);
      goto tail;		/* :::::::::::::::::::: */

    default:
      break;
    }

  return any;
}

/* ffestu_dummies_transition_ -- Update SYMTERs in ITEM list w/in entry

   ffebld list;
   ffesymbol symfunc(ffesymbol s);
   if (ffestu_dummies_transition_(symfunc,list))
       // One or more items are still UNCERTAIN.

   list contains an FFEBLD_opITEM list of SYMTERs (possibly STARs and
   other things, too, but we'll ignore the known ones).	 For each SYMTER,
   we run symfunc on the corresponding ffesymbol (a recursive
   call, since that's the function that's calling us) to update it's
   information.	 Then we copy that information into the SYMTER.

   Return TRUE if any of the SYMTER's has incomplete information.

   Make sure we don't get called recursively ourselves!	 */

static bool
ffestu_dummies_transition_ (ffesymbol (*symfunc) (ffesymbol), ffebld list)
{
  static bool in_progress = FALSE;
  ffebld item;
  ffesymbol symbol;
  bool uncertain = FALSE;

  assert (!in_progress);
  in_progress = TRUE;

  for (; list != NULL; list = ffebld_trail (list))
    {
      if ((item = ffebld_head (list)) == NULL)
	continue;		/* Try next item. */

      switch (ffebld_op (item))
	{
	case FFEBLD_opSTAR:
	  break;

	case FFEBLD_opSYMTER:
	  symbol = ffebld_symter (item);
	  if (symbol == NULL)
	    break;		/* Detached from stmt func dummy list. */
	  symbol = (*symfunc) (symbol);
	  if (ffesymbol_state (symbol) == FFESYMBOL_stateUNCERTAIN)
	    uncertain = TRUE;
	  else
	    {
	      assert (ffesymbol_kind (symbol) != FFEINFO_kindNONE);
	      assert (ffesymbol_where (symbol) != FFEINFO_whereNONE);
	    }
	  ffebld_set_info (item, ffesymbol_info (symbol));
	  break;

	default:
	  assert ("Unexpected item on list" == NULL);
	  break;
	}
    }

  in_progress = FALSE;

  return uncertain;
}
