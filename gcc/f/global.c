/* global.c -- Implementation File (module.c template V1.0)
   Copyright (C) 1995, 1997, 2003 Free Software Foundation, Inc.
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
      Manages information kept across individual program units within a single
      source file.  This includes reporting errors when a name is defined
      multiple times (for example, two program units named FOO) and when a
      COMMON block is given initial data in more than one program unit.

   Modifications:
*/

/* Include files. */

#include "proj.h"
#include "global.h"
#include "info.h"
#include "lex.h"
#include "malloc.h"
#include "name.h"
#include "symbol.h"
#include "top.h"

/* Externals defined here. */


/* Simple definitions and enumerations. */


/* Internal typedefs. */


/* Private include files. */


/* Internal structure definitions. */


/* Static objects accessed by functions in this module. */

#if FFEGLOBAL_ENABLED
static ffenameSpace ffeglobal_filewide_ = NULL;
static const char *const ffeglobal_type_string_[] =
{
  [FFEGLOBAL_typeNONE] = "??",
  [FFEGLOBAL_typeMAIN] = "main program",
  [FFEGLOBAL_typeEXT] = "external",
  [FFEGLOBAL_typeSUBR] = "subroutine",
  [FFEGLOBAL_typeFUNC] = "function",
  [FFEGLOBAL_typeBDATA] = "block data",
  [FFEGLOBAL_typeCOMMON] = "common block",
  [FFEGLOBAL_typeANY] = "?any?"
};
#endif

/* Static functions (internal). */


/* Internal macros. */


/* Call given fn with all globals

   ffeglobal (*fn)(ffeglobal g);
   ffeglobal_drive(fn);	 */

#if FFEGLOBAL_ENABLED
void
ffeglobal_drive (ffeglobal (*fn) (ffeglobal))
{
  if (ffeglobal_filewide_ != NULL)
    ffename_space_drive_global (ffeglobal_filewide_, fn);
}

#endif
/* ffeglobal_new_ -- Make new global

   ffename n;
   ffeglobal g;
   g = ffeglobal_new_(n);  */

#if FFEGLOBAL_ENABLED
static ffeglobal
ffeglobal_new_ (ffename n)
{
  ffeglobal g;

  assert (n != NULL);

  g = malloc_new_ks (malloc_pool_image (), "FFEGLOBAL", sizeof (*g));
  g->n = n;
  g->hook = FFECOM_globalNULL;
  g->tick = 0;

  ffename_set_global (n, g);

  return g;
}

#endif
/* ffeglobal_init_1 -- Initialize per file

   ffeglobal_init_1();	*/

void
ffeglobal_init_1 (void)
{
#if FFEGLOBAL_ENABLED
  if (ffeglobal_filewide_ != NULL)
    ffename_space_kill (ffeglobal_filewide_);
  ffeglobal_filewide_ = ffename_space_new (malloc_pool_image ());
#endif
}

/* ffeglobal_init_common -- Initial value specified for common block

   ffesymbol s;	 // the ffesymbol for the common block
   ffelexToken t;  // the token with the point of initialization
   ffeglobal_init_common(s,t);

   For back ends where file-wide global symbols are not maintained, does
   nothing.  Otherwise, makes sure this common block hasn't already been
   initialized in a previous program unit, and flag that it's been
   initialized in this one.  */

void
ffeglobal_init_common (ffesymbol s, ffelexToken t)
{
#if FFEGLOBAL_ENABLED
  ffeglobal g;

  g = ffesymbol_global (s);

  if ((g == NULL) || (g->type != FFEGLOBAL_typeCOMMON))
    return;
  if (g->type == FFEGLOBAL_typeANY)
    return;

  if (g->tick == ffe_count_2)
    return;

  if (g->tick != 0)
    {
      if (g->u.common.initt != NULL)
	{
	  ffebad_start (FFEBAD_COMMON_ALREADY_INIT);
	  ffebad_string (ffesymbol_text (s));
	  ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_here (1, ffelex_token_where_line (g->u.common.initt),
		       ffelex_token_where_column (g->u.common.initt));
	  ffebad_finish ();
	}

      /* Complain about just one attempt to reinit per program unit, but
	 continue referring back to the first such successful attempt.  */
    }
  else
    {
      if (g->u.common.blank)
	{
	  /* Not supposed to initialize blank common, though it works.  */
	  ffebad_start (FFEBAD_COMMON_BLANK_INIT);
	  ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_finish ();
	}

      g->u.common.initt = ffelex_token_use (t);
    }

  g->tick = ffe_count_2;
#endif
}

/* ffeglobal_new_common -- New common block

   ffesymbol s;	 // the ffesymbol for the new common block
   ffelexToken t;  // the token with the name of the common block
   bool blank;	// TRUE if blank common
   ffeglobal_new_common(s,t,blank);

   For back ends where file-wide global symbols are not maintained, does
   nothing.  Otherwise, makes sure this symbol hasn't been seen before or
   is known as a common block.	*/

void
ffeglobal_new_common (ffesymbol s, ffelexToken t, bool blank)
{
#if FFEGLOBAL_ENABLED
  ffename n;
  ffeglobal g;

  if (ffesymbol_global (s) == NULL)
    {
      n = ffename_find (ffeglobal_filewide_, t);
      g = ffename_global (n);
    }
  else
    {
      g = ffesymbol_global (s);
      n = NULL;
    }

  if ((g != NULL) && (g->type == FFEGLOBAL_typeANY))
    return;

  if ((g != NULL) && (g->type != FFEGLOBAL_typeNONE))
    {
      if (g->type == FFEGLOBAL_typeCOMMON)
	{
	  /* The names match, so the "blankness" should match too!  */
	  assert (g->u.common.blank == blank);
	}
      else
	{
	  /* This global name has already been established,
	     but as something other than a common block.  */
	  if (ffe_is_globals () || ffe_is_warn_globals ())
	    {
	      ffebad_start (ffe_is_globals ()
			    ? FFEBAD_FILEWIDE_ALREADY_SEEN
			    : FFEBAD_FILEWIDE_ALREADY_SEEN_W);
	      ffebad_string (ffelex_token_text (t));
	      ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
	      ffebad_here (1, ffelex_token_where_line (g->t),
			   ffelex_token_where_column (g->t));
	      ffebad_finish ();
	    }
	  g->type = FFEGLOBAL_typeANY;
	}
    }
  else
    {
      if (g == NULL)
	{
	  g = ffeglobal_new_ (n);
	  g->intrinsic = FALSE;
	}
      else if (g->intrinsic
	       && !g->explicit_intrinsic
	       && ffe_is_warn_globals ())
	{
	  /* Common name previously used as intrinsic.  Though it works,
	     warn, because the intrinsic reference might have been intended
	     as a ref to an external procedure, but g77's vast list of
	     intrinsics happened to snarf the name.  */
	  ffebad_start (FFEBAD_INTRINSIC_GLOBAL);
	  ffebad_string (ffelex_token_text (t));
	  ffebad_string ("common block");
	  ffebad_string ("intrinsic");
	  ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_here (1, ffelex_token_where_line (g->t),
		       ffelex_token_where_column (g->t));
	  ffebad_finish ();
	}
      g->t = ffelex_token_use (t);
      g->type = FFEGLOBAL_typeCOMMON;
      g->u.common.have_pad = FALSE;
      g->u.common.have_save = FALSE;
      g->u.common.have_size = FALSE;
      g->u.common.blank = blank;
    }

  ffesymbol_set_global (s, g);
#endif
}

/* ffeglobal_new_progunit_ -- New program unit

   ffesymbol s;	 // the ffesymbol for the new unit
   ffelexToken t;  // the token with the name of the unit
   ffeglobalType type;	// the type of the new unit
   ffeglobal_new_progunit_(s,t,type);

   For back ends where file-wide global symbols are not maintained, does
   nothing.  Otherwise, makes sure this symbol hasn't been seen before.	 */

void
ffeglobal_new_progunit_ (ffesymbol s, ffelexToken t, ffeglobalType type)
{
#if FFEGLOBAL_ENABLED
  ffename n;
  ffeglobal g;

  n = ffename_find (ffeglobal_filewide_, t);
  g = ffename_global (n);
  if ((g != NULL) && (g->type == FFEGLOBAL_typeANY))
    return;

  if ((g != NULL)
      && ((g->type == FFEGLOBAL_typeMAIN)
	  || (g->type == FFEGLOBAL_typeSUBR)
	  || (g->type == FFEGLOBAL_typeFUNC)
	  || (g->type == FFEGLOBAL_typeBDATA))
      && g->u.proc.defined)
    {
      /* This program unit has already been defined.  */
      if (ffe_is_globals () || ffe_is_warn_globals ())
	{
	  ffebad_start (ffe_is_globals ()
			? FFEBAD_FILEWIDE_ALREADY_SEEN
			: FFEBAD_FILEWIDE_ALREADY_SEEN_W);
	  ffebad_string (ffelex_token_text (t));
	  ffebad_here (0, ffelex_token_where_line (t),
		       ffelex_token_where_column (t));
	  ffebad_here (1, ffelex_token_where_line (g->t),
		       ffelex_token_where_column (g->t));
	  ffebad_finish ();
	}
      g->type = FFEGLOBAL_typeANY;
    }
  else if ((g != NULL)
	   && (g->type != FFEGLOBAL_typeNONE)
	   && (g->type != FFEGLOBAL_typeEXT)
	   && (g->type != type))
    {
      /* A reference to this program unit has been seen, but its
	 context disagrees about the new definition regarding
	 what kind of program unit it is.  (E.g. `call foo' followed
	 by `function foo'.)  But `external foo' alone doesn't mean
	 disagreement with either a function or subroutine, though
	 g77 normally interprets it as a request to force-load
	 a block data program unit by that name (to cope with libs).  */
      if (ffe_is_globals () || ffe_is_warn_globals ())
	{
	  ffebad_start (ffe_is_globals ()
			? FFEBAD_FILEWIDE_DISAGREEMENT
			: FFEBAD_FILEWIDE_DISAGREEMENT_W);
	  ffebad_string (ffelex_token_text (t));
	  ffebad_string (ffeglobal_type_string_[type]);
	  ffebad_string (ffeglobal_type_string_[g->type]);
	  ffebad_here (0, ffelex_token_where_line (t),
		       ffelex_token_where_column (t));
	  ffebad_here (1, ffelex_token_where_line (g->t),
		       ffelex_token_where_column (g->t));
	  ffebad_finish ();
	}
      g->type = FFEGLOBAL_typeANY;
    }
  else
    {
      if (g == NULL)
	{
	  g = ffeglobal_new_ (n);
	  g->intrinsic = FALSE;
	  g->u.proc.n_args = -1;
	  g->u.proc.other_t = NULL;
	}
      else if ((ffesymbol_basictype (s) != FFEINFO_basictypeNONE)
	       && (g->type == FFEGLOBAL_typeFUNC)
	       && ((ffesymbol_basictype (s) != g->u.proc.bt)
		   || (ffesymbol_kindtype (s) != g->u.proc.kt)
		   || ((ffesymbol_size (s) != FFETARGET_charactersizeNONE)
		       && (ffesymbol_size (s) != g->u.proc.sz))))
	{
	  /* The previous reference and this new function definition
	     disagree about the type of the function.  I (Burley) think
	     this rarely occurs, because when this code is reached,
	     the type info doesn't appear to be filled in yet.  */
	  if (ffe_is_globals () || ffe_is_warn_globals ())
	    {
	      ffebad_start (ffe_is_globals ()
			    ? FFEBAD_FILEWIDE_TYPE_MISMATCH
			    : FFEBAD_FILEWIDE_TYPE_MISMATCH_W);
	      ffebad_string (ffelex_token_text (t));
	      ffebad_here (0, ffelex_token_where_line (t),
			   ffelex_token_where_column (t));
	      ffebad_here (1, ffelex_token_where_line (g->t),
			   ffelex_token_where_column (g->t));
	      ffebad_finish ();
	    }
	  g->type = FFEGLOBAL_typeANY;
	  return;
	}
      if (g->intrinsic
	  && !g->explicit_intrinsic
	  && ffe_is_warn_globals ())
	{
	  /* This name, previously used as an intrinsic, now is known
	     to also be a global procedure name.  Warn, since the previous
	     use as an intrinsic might have been intended to refer to
	     this procedure.  */
	  ffebad_start (FFEBAD_INTRINSIC_GLOBAL);
	  ffebad_string (ffelex_token_text (t));
	  ffebad_string ("global");
	  ffebad_string ("intrinsic");
	  ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_here (1, ffelex_token_where_line (g->t),
		       ffelex_token_where_column (g->t));
	  ffebad_finish ();
	}
      g->t = ffelex_token_use (t);
      if ((g->tick == 0)
	  || (g->u.proc.bt == FFEINFO_basictypeNONE)
	  || (g->u.proc.kt == FFEINFO_kindtypeNONE))
	{
	  g->u.proc.bt = ffesymbol_basictype (s);
	  g->u.proc.kt = ffesymbol_kindtype (s);
	  g->u.proc.sz = ffesymbol_size (s);
	}
      /* If there's a known disagreement about the kind of program
	 unit, then don't even bother tracking arglist argreement.  */
      if ((g->tick != 0)
	  && (g->type != type))
	g->u.proc.n_args = -1;
      g->tick = ffe_count_2;
      g->type = type;
      g->u.proc.defined = TRUE;
    }

  ffesymbol_set_global (s, g);
#endif
}

/* ffeglobal_pad_common -- Check initial padding of common area

   ffesymbol s;	 // the common area
   ffetargetAlign pad;	// the initial padding
   ffeglobal_pad_common(s,pad,ffesymbol_where_line(s),
	 ffesymbol_where_column(s));

   In global-enabled mode, make sure the padding agrees with any existing
   padding established for the common area, otherwise complain.
   In global-disabled mode, warn about nonzero padding.	 */

void
ffeglobal_pad_common (ffesymbol s, ffetargetAlign pad, ffewhereLine wl,
		      ffewhereColumn wc)
{
#if FFEGLOBAL_ENABLED
  ffeglobal g;

  g = ffesymbol_global (s);
  if ((g == NULL) || (g->type != FFEGLOBAL_typeCOMMON))
    return;			/* Let someone else catch this! */
  if (g->type == FFEGLOBAL_typeANY)
    return;

  if (!g->u.common.have_pad)
    {
      g->u.common.have_pad = TRUE;
      g->u.common.pad = pad;
      g->u.common.pad_where_line = ffewhere_line_use (wl);
      g->u.common.pad_where_col = ffewhere_column_use (wc);

      if (pad != 0)
	{
	  char padding[20];

	  sprintf (&padding[0], "%" ffetargetAlign_f "u", pad);
	  ffebad_start (FFEBAD_COMMON_INIT_PAD);
	  ffebad_string (ffesymbol_text (s));
	  ffebad_string (padding);
	  ffebad_string ((pad == 1)
			 ? FFECOM_SIZE_UNIT : FFECOM_SIZE_UNITS);
	  ffebad_here (0, wl, wc);
	  ffebad_finish ();
	}
    }
  else
    {
      if (g->u.common.pad != pad)
	{
	  char padding_1[20];
	  char padding_2[20];

	  sprintf (&padding_1[0], "%" ffetargetAlign_f "u", pad);
	  sprintf (&padding_2[0], "%" ffetargetAlign_f "u", g->u.common.pad);
	  ffebad_start (FFEBAD_COMMON_DIFF_PAD);
	  ffebad_string (ffesymbol_text (s));
	  ffebad_string (padding_1);
	  ffebad_here (0, wl, wc);
	  ffebad_string (padding_2);
	  ffebad_string ((pad == 1)
			 ? FFECOM_SIZE_UNIT : FFECOM_SIZE_UNITS);
	  ffebad_string ((g->u.common.pad == 1)
			 ? FFECOM_SIZE_UNIT : FFECOM_SIZE_UNITS);
	  ffebad_here (1, g->u.common.pad_where_line, g->u.common.pad_where_col);
	  ffebad_finish ();
	}

      if (g->u.common.pad < pad)
	{
	  g->u.common.pad = pad;
	  g->u.common.pad_where_line = ffewhere_line_use (wl);
	  g->u.common.pad_where_col = ffewhere_column_use (wc);
	}
    }
#endif
}

/* Collect info for a global's argument.  */

void
ffeglobal_proc_def_arg (ffesymbol s, int argno, const char *name, ffeglobalArgSummary as,
			ffeinfoBasictype bt, ffeinfoKindtype kt,
			bool array)
{
  ffeglobal g = ffesymbol_global (s);
  ffeglobalArgInfo_ ai;

  assert (g != NULL);

  if (g->type == FFEGLOBAL_typeANY)
    return;

  assert (g->u.proc.n_args >= 0);

  if (argno >= g->u.proc.n_args)
    return;	/* Already complained about this discrepancy. */

  ai = &g->u.proc.arg_info[argno];

  /* Maybe warn about previous references.  */

  if ((ai->t != NULL)
      && ffe_is_warn_globals ())
    {
      const char *refwhy = NULL;
      const char *defwhy = NULL;
      bool warn = FALSE;

      switch (as)
	{
	case FFEGLOBAL_argsummaryREF:
	  if ((ai->as != FFEGLOBAL_argsummaryREF)
	      && (ai->as != FFEGLOBAL_argsummaryNONE)
	      && ((ai->as != FFEGLOBAL_argsummaryDESCR)	/* Choose better message. */
		  || (ai->bt != FFEINFO_basictypeCHARACTER)
		  || (ai->bt == bt)))
	    {
	      warn = TRUE;
	      refwhy = "passed by reference";
	    }
	  break;

	case FFEGLOBAL_argsummaryDESCR:
	  if ((ai->as != FFEGLOBAL_argsummaryDESCR)
	      && (ai->as != FFEGLOBAL_argsummaryNONE)
	      && ((ai->as != FFEGLOBAL_argsummaryREF)	/* Choose better message. */
		  || (bt != FFEINFO_basictypeCHARACTER)
		  || (ai->bt == bt)))
	    {
	      warn = TRUE;
	      refwhy = "passed by descriptor";
	    }
	  break;

	case FFEGLOBAL_argsummaryPROC:
	  if ((ai->as != FFEGLOBAL_argsummaryPROC)
	      && (ai->as != FFEGLOBAL_argsummarySUBR)
	      && (ai->as != FFEGLOBAL_argsummaryFUNC)
	      && (ai->as != FFEGLOBAL_argsummaryNONE))
	    {
	      warn = TRUE;
	      refwhy = "a procedure";
	    }
	  break;

	case FFEGLOBAL_argsummarySUBR:
	  if ((ai->as != FFEGLOBAL_argsummaryPROC)
	      && (ai->as != FFEGLOBAL_argsummarySUBR)
	      && (ai->as != FFEGLOBAL_argsummaryNONE))
	    {
	      warn = TRUE;
	      refwhy = "a subroutine";
	    }
	  break;

	case FFEGLOBAL_argsummaryFUNC:
	  if ((ai->as != FFEGLOBAL_argsummaryPROC)
	      && (ai->as != FFEGLOBAL_argsummaryFUNC)
	      && (ai->as != FFEGLOBAL_argsummaryNONE))
	    {
	      warn = TRUE;
	      refwhy = "a function";
	    }
	  break;

	case FFEGLOBAL_argsummaryALTRTN:
	  if ((ai->as != FFEGLOBAL_argsummaryALTRTN)
	      && (ai->as != FFEGLOBAL_argsummaryNONE))
	    {
	      warn = TRUE;
	      refwhy = "an alternate-return label";
	    }
	  break;

	default:
	  break;
	}

      if ((refwhy != NULL) && (defwhy == NULL))
	{
	  /* Fill in the def info.  */

	  switch (ai->as)
	    {
	    case FFEGLOBAL_argsummaryNONE:
	      defwhy = "omitted";
	      break;

	    case FFEGLOBAL_argsummaryVAL:
	      defwhy = "passed by value";
	      break;

	    case FFEGLOBAL_argsummaryREF:
	      defwhy = "passed by reference";
	      break;

	    case FFEGLOBAL_argsummaryDESCR:
	      defwhy = "passed by descriptor";
	      break;

	    case FFEGLOBAL_argsummaryPROC:
	      defwhy = "a procedure";
	      break;

	    case FFEGLOBAL_argsummarySUBR:
	      defwhy = "a subroutine";
	      break;

	    case FFEGLOBAL_argsummaryFUNC:
	      defwhy = "a function";
	      break;

	    case FFEGLOBAL_argsummaryALTRTN:
	      defwhy = "an alternate-return label";
	      break;

#if 0
	    case FFEGLOBAL_argsummaryPTR:
	      defwhy = "a pointer";
	      break;
#endif

	    default:
	      defwhy = "???";
	      break;
	    }
	}

      if (!warn
	  && (bt != FFEINFO_basictypeHOLLERITH)
	  && (bt != FFEINFO_basictypeTYPELESS)
	  && (bt != FFEINFO_basictypeNONE)
	  && (ai->bt != FFEINFO_basictypeHOLLERITH)
	  && (ai->bt != FFEINFO_basictypeTYPELESS)
	  && (ai->bt != FFEINFO_basictypeNONE))
	{
	  /* Check types.  */

	  if ((bt != ai->bt)
	      && ((bt != FFEINFO_basictypeREAL)
		  || (ai->bt != FFEINFO_basictypeCOMPLEX))
	      && ((bt != FFEINFO_basictypeCOMPLEX)
		  || (ai->bt != FFEINFO_basictypeREAL)))
	    {
	      warn = TRUE;	/* We can cope with these differences. */
	      refwhy = "one type";
	      defwhy = "some other type";
	    }

	  if (!warn && (kt != ai->kt))
	    {
	      warn = TRUE;
	      refwhy = "one precision";
	      defwhy = "some other precision";
	    }
	}

      if (warn)
	{
	  char num[60];

	  if (name == NULL)
	    sprintf (&num[0], "%d", argno + 1);
	  else
	    {
	      if (strlen (name) < 30)
		sprintf (&num[0], "%d (named `%s')", argno + 1, name);
	      else
		sprintf (&num[0], "%d (named `%.*s...')", argno + 1, 30, name);
	    }
	  ffebad_start (FFEBAD_FILEWIDE_ARG_W);
	  ffebad_string (ffesymbol_text (s));
	  ffebad_string (num);
	  ffebad_string (refwhy);
	  ffebad_string (defwhy);
	  ffebad_here (0, ffelex_token_where_line (g->t), ffelex_token_where_column (g->t));
	  ffebad_here (1, ffelex_token_where_line (ai->t), ffelex_token_where_column (ai->t));
	  ffebad_finish ();
	}
    }

  /* Define this argument.  */

  if (ai->t != NULL)
    ffelex_token_kill (ai->t);
  if ((as != FFEGLOBAL_argsummaryPROC)
      || (ai->t == NULL))
    ai->as = as;	/* Otherwise leave SUBR/FUNC info intact. */
  ai->t = ffelex_token_use (g->t);
  if (name == NULL)
    ai->name = NULL;
  else
    {
      ai->name = malloc_new_ks (malloc_pool_image (),
				"ffeglobalArgInfo_ name",
				strlen (name) + 1);
      strcpy (ai->name, name);
    }
  ai->bt = bt;
  ai->kt = kt;
  ai->array = array;
}

/* Collect info on #args a global accepts.  */

void
ffeglobal_proc_def_nargs (ffesymbol s, int n_args)
{
  ffeglobal g = ffesymbol_global (s);

  assert (g != NULL);

  if (g->type == FFEGLOBAL_typeANY)
    return;

  if (g->u.proc.n_args >= 0)
    {
      if (g->u.proc.n_args == n_args)
	return;

      if (ffe_is_warn_globals ())
	{
	  ffebad_start (FFEBAD_FILEWIDE_NARGS_W);
	  ffebad_string (ffesymbol_text (s));
	  if (g->u.proc.n_args > n_args)
	    ffebad_string ("few");
	  else
	    ffebad_string ("many");
	  ffebad_here (0, ffelex_token_where_line (g->u.proc.other_t),
		       ffelex_token_where_column (g->u.proc.other_t));
	  ffebad_here (1, ffelex_token_where_line (g->t),
		       ffelex_token_where_column (g->t));
	  ffebad_finish ();
	}
    }

  /* This is new info we can use in cross-checking future references
     and a possible future definition.  */

  g->u.proc.n_args = n_args;
  g->u.proc.other_t = NULL;	/* No other reference yet. */

  if (n_args == 0)
    {
      g->u.proc.arg_info = NULL;
      return;
    }

  g->u.proc.arg_info = malloc_new_ks (malloc_pool_image (),
				      "ffeglobalArgInfo_",
				      n_args * sizeof (g->u.proc.arg_info[0]));
  while (n_args-- > 0)
    g->u.proc.arg_info[n_args].t = NULL;
}

/* Verify that the info for a global's argument is valid.  */

bool
ffeglobal_proc_ref_arg (ffesymbol s, int argno, ffeglobalArgSummary as,
			ffeinfoBasictype bt, ffeinfoKindtype kt,
			bool array, ffelexToken t)
{
  ffeglobal g = ffesymbol_global (s);
  ffeglobalArgInfo_ ai;

  assert (g != NULL);

  if (g->type == FFEGLOBAL_typeANY)
    return FALSE;

  assert (g->u.proc.n_args >= 0);

  if (argno >= g->u.proc.n_args)
    return TRUE;	/* Already complained about this discrepancy. */

  ai = &g->u.proc.arg_info[argno];

  /* Warn about previous references.  */

  if (ai->t != NULL)
    {
      const char *refwhy = NULL;
      const char *defwhy = NULL;
      bool fail = FALSE;
      bool warn = FALSE;

      switch (as)
	{
	case FFEGLOBAL_argsummaryNONE:
	  if (g->u.proc.defined)
	    {
	      fail = TRUE;
	      refwhy = "omitted";
	      defwhy = "not optional";
	    }
	  break;

	case FFEGLOBAL_argsummaryVAL:
	  if (ai->as != FFEGLOBAL_argsummaryVAL)
	    {
	      fail = TRUE;
	      refwhy = "passed by value";
	    }
	  break;

	case FFEGLOBAL_argsummaryREF:
	  if ((ai->as != FFEGLOBAL_argsummaryREF)
	      && (ai->as != FFEGLOBAL_argsummaryNONE)
	      && ((ai->as != FFEGLOBAL_argsummaryDESCR)	/* Choose better message. */
		  || (ai->bt != FFEINFO_basictypeCHARACTER)
		  || (ai->bt == bt)))
	    {
	      fail = TRUE;
	      refwhy = "passed by reference";
	    }
	  break;

	case FFEGLOBAL_argsummaryDESCR:
	  if ((ai->as != FFEGLOBAL_argsummaryDESCR)
	      && (ai->as != FFEGLOBAL_argsummaryNONE)
	      && ((ai->as != FFEGLOBAL_argsummaryREF)	/* Choose better message. */
		  || (bt != FFEINFO_basictypeCHARACTER)
		  || (ai->bt == bt)))
	    {
	      fail = TRUE;
	      refwhy = "passed by descriptor";
	    }
	  break;

	case FFEGLOBAL_argsummaryPROC:
	  if ((ai->as != FFEGLOBAL_argsummaryPROC)
	      && (ai->as != FFEGLOBAL_argsummarySUBR)
	      && (ai->as != FFEGLOBAL_argsummaryFUNC)
	      && (ai->as != FFEGLOBAL_argsummaryNONE))
	    {
	      fail = TRUE;
	      refwhy = "a procedure";
	    }
	  break;

	case FFEGLOBAL_argsummarySUBR:
	  if ((ai->as != FFEGLOBAL_argsummaryPROC)
	      && (ai->as != FFEGLOBAL_argsummarySUBR)
	      && (ai->as != FFEGLOBAL_argsummaryNONE))
	    {
	      fail = TRUE;
	      refwhy = "a subroutine";
	    }
	  break;

	case FFEGLOBAL_argsummaryFUNC:
	  if ((ai->as != FFEGLOBAL_argsummaryPROC)
	      && (ai->as != FFEGLOBAL_argsummaryFUNC)
	      && (ai->as != FFEGLOBAL_argsummaryNONE))
	    {
	      fail = TRUE;
	      refwhy = "a function";
	    }
	  break;

	case FFEGLOBAL_argsummaryALTRTN:
	  if ((ai->as != FFEGLOBAL_argsummaryALTRTN)
	      && (ai->as != FFEGLOBAL_argsummaryNONE))
	    {
	      fail = TRUE;
	      refwhy = "an alternate-return label";
	    }
	  break;

#if 0
	case FFEGLOBAL_argsummaryPTR:
	  if ((ai->as != FFEGLOBAL_argsummaryPTR)
	      && (ai->as != FFEGLOBAL_argsummaryNONE))
	    {
	      fail = TRUE;
	      refwhy = "a pointer";
	    }
	  break;
#endif

	default:
	  break;
	}

      if ((refwhy != NULL) && (defwhy == NULL))
	{
	  /* Fill in the def info.  */

	  switch (ai->as)
	    {
	    case FFEGLOBAL_argsummaryNONE:
	      defwhy = "omitted";
	      break;

	    case FFEGLOBAL_argsummaryVAL:
	      defwhy = "passed by value";
	      break;

	    case FFEGLOBAL_argsummaryREF:
	      defwhy = "passed by reference";
	      break;

	    case FFEGLOBAL_argsummaryDESCR:
	      defwhy = "passed by descriptor";
	      break;

	    case FFEGLOBAL_argsummaryPROC:
	      defwhy = "a procedure";
	      break;

	    case FFEGLOBAL_argsummarySUBR:
	      defwhy = "a subroutine";
	      break;

	    case FFEGLOBAL_argsummaryFUNC:
	      defwhy = "a function";
	      break;

	    case FFEGLOBAL_argsummaryALTRTN:
	      defwhy = "an alternate-return label";
	      break;

#if 0
	    case FFEGLOBAL_argsummaryPTR:
	      defwhy = "a pointer";
	      break;
#endif

	    default:
	      defwhy = "???";
	      break;
	    }
	}

      if (!fail && !warn
	  && (bt != FFEINFO_basictypeHOLLERITH)
	  && (bt != FFEINFO_basictypeTYPELESS)
	  && (bt != FFEINFO_basictypeNONE)
	  && (ai->bt != FFEINFO_basictypeHOLLERITH)
	  && (ai->bt != FFEINFO_basictypeNONE)
	  && (ai->bt != FFEINFO_basictypeTYPELESS))
	{
	  /* Check types.  */

	  if ((bt != ai->bt)
	      && ((bt != FFEINFO_basictypeREAL)
		  || (ai->bt != FFEINFO_basictypeCOMPLEX))
	      && ((bt != FFEINFO_basictypeCOMPLEX)
		  || (ai->bt != FFEINFO_basictypeREAL)))
	    {
	      if (((bt == FFEINFO_basictypeINTEGER)
		   && (ai->bt == FFEINFO_basictypeLOGICAL))
		  || ((bt == FFEINFO_basictypeLOGICAL)
		   && (ai->bt == FFEINFO_basictypeINTEGER)))
		warn = TRUE;	/* We can cope with these differences. */
	      else
		fail = TRUE;
	      refwhy = "one type";
	      defwhy = "some other type";
	    }

	  if (!fail && !warn && (kt != ai->kt))
	    {
	      fail = TRUE;
	      refwhy = "one precision";
	      defwhy = "some other precision";
	    }
	}

      if (fail && ! g->u.proc.defined)
	{
	  /* No point failing if we're worried only about invocations.  */
	  fail = FALSE;
	  warn = TRUE;
	}

      if (fail && ! ffe_is_globals ())
	{
	  warn = TRUE;
	  fail = FALSE;
	}

      if (fail || (warn && ffe_is_warn_globals ()))
	{
	  char num[60];

	  if (ai->name == NULL)
	    sprintf (&num[0], "%d", argno + 1);
	  else
	    {
	      if (strlen (ai->name) < 30)
		sprintf (&num[0], "%d (named `%s')", argno + 1, ai->name);
	      else
		sprintf (&num[0], "%d (named `%.*s...')", argno + 1, 30, ai->name);
	    }
	  ffebad_start (fail ? FFEBAD_FILEWIDE_ARG : FFEBAD_FILEWIDE_ARG_W);
	  ffebad_string (ffesymbol_text (s));
	  ffebad_string (num);
	  ffebad_string (refwhy);
	  ffebad_string (defwhy);
	  ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_here (1, ffelex_token_where_line (ai->t), ffelex_token_where_column (ai->t));
	  ffebad_finish ();
	  return (fail ? FALSE : TRUE);
	}

      if (warn)
	return TRUE;
    }

  /* Define this argument.  */

  if (ai->t != NULL)
    ffelex_token_kill (ai->t);
  if ((as != FFEGLOBAL_argsummaryPROC)
      || (ai->t == NULL))
    ai->as = as;
  ai->t = ffelex_token_use (g->t);
  ai->name = NULL;
  ai->bt = bt;
  ai->kt = kt;
  ai->array = array;
  return TRUE;
}

bool
ffeglobal_proc_ref_nargs (ffesymbol s, int n_args, ffelexToken t)
{
  ffeglobal g = ffesymbol_global (s);

  assert (g != NULL);

  if (g->type == FFEGLOBAL_typeANY)
    return FALSE;

  if (g->u.proc.n_args >= 0)
    {
      if (g->u.proc.n_args == n_args)
	return TRUE;

      if (g->u.proc.defined && ffe_is_globals ())
	{
	  ffebad_start (FFEBAD_FILEWIDE_NARGS);
	  ffebad_string (ffesymbol_text (s));
	  if (g->u.proc.n_args > n_args)
	    ffebad_string ("few");
	  else
	    ffebad_string ("many");
	  ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_here (1, ffelex_token_where_line (g->t),
		       ffelex_token_where_column (g->t));
	  ffebad_finish ();
	  return FALSE;
	}

      if (ffe_is_warn_globals ())
	{
	  ffebad_start (FFEBAD_FILEWIDE_NARGS_W);
	  ffebad_string (ffesymbol_text (s));
	  if (g->u.proc.n_args > n_args)
	    ffebad_string ("few");
	  else
	    ffebad_string ("many");
	  ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_here (1, ffelex_token_where_line (g->t),
		       ffelex_token_where_column (g->t));
	  ffebad_finish ();
	}

      return TRUE;		/* Don't replace the info we already have. */
    }

  /* This is new info we can use in cross-checking future references
     and a possible future definition.  */

  g->u.proc.n_args = n_args;
  g->u.proc.other_t = ffelex_token_use (t);

  /* Make this "the" place we found the global, since it has the most info.  */

  if (g->t != NULL)
    ffelex_token_kill (g->t);
  g->t = ffelex_token_use (t);

  if (n_args == 0)
    {
      g->u.proc.arg_info = NULL;
      return TRUE;
    }

  g->u.proc.arg_info = malloc_new_ks (malloc_pool_image (),
				      "ffeglobalArgInfo_",
				      n_args * sizeof (g->u.proc.arg_info[0]));
  while (n_args-- > 0)
    g->u.proc.arg_info[n_args].t = NULL;

  return TRUE;
}

/* Return a global for a promoted symbol (one that has heretofore
   been assumed to be local, but since discovered to be global).  */

ffeglobal
ffeglobal_promoted (ffesymbol s)
{
#if FFEGLOBAL_ENABLED
  ffename n;
  ffeglobal g;

  assert (ffesymbol_global (s) == NULL);

  n = ffename_find (ffeglobal_filewide_, ffename_token (ffesymbol_name (s)));
  g = ffename_global (n);

  return g;
#else
  return NULL;
#endif
}

/* Register a reference to an intrinsic.  Such a reference is always
   valid, though a warning might be in order if the same name has
   already been used for a global.  */

void
ffeglobal_ref_intrinsic (ffesymbol s, ffelexToken t, bool explicit)
{
#if FFEGLOBAL_ENABLED
  ffename n;
  ffeglobal g;

  if (ffesymbol_global (s) == NULL)
    {
      n = ffename_find (ffeglobal_filewide_, t);
      g = ffename_global (n);
    }
  else
    {
      g = ffesymbol_global (s);
      n = NULL;
    }

  if ((g != NULL) && (g->type == FFEGLOBAL_typeANY))
    return;

  if ((g != NULL) && (g->type != FFEGLOBAL_typeNONE))
    {
      if (! explicit
	  && ! g->intrinsic
	  && ffe_is_warn_globals ())
	{
	  /* This name, previously used as a global, now is used
	     for an intrinsic.  Warn, since this new use as an
	     intrinsic might have been intended to refer to
	     the global procedure.  */
	  ffebad_start (FFEBAD_INTRINSIC_GLOBAL);
	  ffebad_string (ffelex_token_text (t));
	  ffebad_string ("intrinsic");
	  ffebad_string ("global");
	  ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_here (1, ffelex_token_where_line (g->t),
		       ffelex_token_where_column (g->t));
	  ffebad_finish ();
	}
    }
  else
    {
      if (g == NULL)
	{
	  g = ffeglobal_new_ (n);
	  g->tick = ffe_count_2;
	  g->type = FFEGLOBAL_typeNONE;
	  g->intrinsic = TRUE;
	  g->explicit_intrinsic = explicit;
	  g->t = ffelex_token_use (t);
	}
      else if (g->intrinsic
	       && (explicit != g->explicit_intrinsic)
	       && (g->tick != ffe_count_2)
	       && ffe_is_warn_globals ())
	{
	  /* An earlier reference to this intrinsic disagrees with
	     this reference vis-a-vis explicit `intrinsic foo',
	     which suggests that the one relying on implicit
	     intrinsicacity might have actually intended to refer
	     to a global of the same name.  */
	  ffebad_start (FFEBAD_INTRINSIC_EXPIMP);
	  ffebad_string (ffelex_token_text (t));
	  ffebad_string (explicit ? "explicit" : "implicit");
	  ffebad_string (explicit ? "implicit" : "explicit");
	  ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_here (1, ffelex_token_where_line (g->t),
		       ffelex_token_where_column (g->t));
	  ffebad_finish ();
	}
    }

  g->intrinsic = TRUE;
  if (explicit)
    g->explicit_intrinsic = TRUE;

  ffesymbol_set_global (s, g);
#endif
}

/* Register a reference to a global.  Returns TRUE if the reference
   is valid.  */

bool
ffeglobal_ref_progunit_ (ffesymbol s, ffelexToken t, ffeglobalType type)
{
#if FFEGLOBAL_ENABLED
  ffename n = NULL;
  ffeglobal g;

  /* It is never really _known_ that an EXTERNAL statement
     names a BLOCK DATA by just looking at the program unit,
     so override a different notion here.  */
  if (type == FFEGLOBAL_typeBDATA)
    type = FFEGLOBAL_typeEXT;

  g = ffesymbol_global (s);
  if (g == NULL)
    {
      n = ffename_find (ffeglobal_filewide_, t);
      g = ffename_global (n);
      if (g != NULL)
	ffesymbol_set_global (s, g);
    }

  if ((g != NULL) && (g->type == FFEGLOBAL_typeANY))
    return TRUE;

  if ((g != NULL)
      && (g->type != FFEGLOBAL_typeNONE)
      && (g->type != FFEGLOBAL_typeEXT)
      && (g->type != type)
      && (type != FFEGLOBAL_typeEXT))
    {
      /* Disagreement about (fully refined) class of program unit
	 (main, subroutine, function, block data).  Treat EXTERNAL/
	 COMMON disagreements distinctly.  */
      if ((((type == FFEGLOBAL_typeBDATA)
	    && (g->type != FFEGLOBAL_typeCOMMON))
	   || ((g->type == FFEGLOBAL_typeBDATA)
	       && (type != FFEGLOBAL_typeCOMMON)
	       && ! g->u.proc.defined)))
	{
#if 0	/* This is likely to just annoy people. */
	  if (ffe_is_warn_globals ())
	    {
	      /* Warn about EXTERNAL of a COMMON name, though it works.  */
	      ffebad_start (FFEBAD_FILEWIDE_TIFF);
	      ffebad_string (ffelex_token_text (t));
	      ffebad_string (ffeglobal_type_string_[type]);
	      ffebad_string (ffeglobal_type_string_[g->type]);
	      ffebad_here (0, ffelex_token_where_line (t),
			   ffelex_token_where_column (t));
	      ffebad_here (1, ffelex_token_where_line (g->t),
			   ffelex_token_where_column (g->t));
	      ffebad_finish ();
	    }
#endif
	}
      else if (ffe_is_globals () || ffe_is_warn_globals ())
	{
	  ffebad_start (ffe_is_globals ()
			? FFEBAD_FILEWIDE_DISAGREEMENT
			: FFEBAD_FILEWIDE_DISAGREEMENT_W);
	  ffebad_string (ffelex_token_text (t));
	  ffebad_string (ffeglobal_type_string_[type]);
	  ffebad_string (ffeglobal_type_string_[g->type]);
	  ffebad_here (0, ffelex_token_where_line (t),
		       ffelex_token_where_column (t));
	  ffebad_here (1, ffelex_token_where_line (g->t),
		       ffelex_token_where_column (g->t));
	  ffebad_finish ();
	  g->type = FFEGLOBAL_typeANY;
	  return (! ffe_is_globals ());
	}
    }

  if ((g != NULL)
      && (type == FFEGLOBAL_typeFUNC))
    {
      /* If just filling in this function's type, do so.  */
      if ((g->tick == ffe_count_2)
	  && (ffesymbol_basictype (s) != FFEINFO_basictypeNONE)
	  && (ffesymbol_kindtype (s) != FFEINFO_kindtypeNONE))
	{
	  g->u.proc.bt = ffesymbol_basictype (s);
	  g->u.proc.kt = ffesymbol_kindtype (s);
	  g->u.proc.sz = ffesymbol_size (s);
	}
      /* Make sure there is type agreement.  */
      if (g->type == FFEGLOBAL_typeFUNC
	  && g->u.proc.bt != FFEINFO_basictypeNONE
	  && ffesymbol_basictype (s) != FFEINFO_basictypeNONE
	  && (ffesymbol_basictype (s) != g->u.proc.bt
	      || ffesymbol_kindtype (s) != g->u.proc.kt
	      /* CHARACTER*n disagreements matter only once a
		 definition is involved, since the definition might
		 be CHARACTER*(*), which accepts all references.  */
	      || (g->u.proc.defined
		  && ffesymbol_size (s) != g->u.proc.sz
		  && ffesymbol_size (s) != FFETARGET_charactersizeNONE
		  && g->u.proc.sz != FFETARGET_charactersizeNONE)))
	{
	  int error;

	  /* Type mismatch between function reference/definition and
	     this subsequent reference (which might just be the filling-in
	     of type info for the definition, but we can't reach here
	     if that's the case and there was a previous definition).

	     It's an error given a previous definition, since that
	     implies inlining can crash the compiler, unless the user
	     asked for no such inlining.  */
	  error = (g->tick != ffe_count_2
		   && g->u.proc.defined
		   && ffe_is_globals ());
	  if (error || ffe_is_warn_globals ())
	    {
	      ffebad_start (error
			    ? FFEBAD_FILEWIDE_TYPE_MISMATCH
			    : FFEBAD_FILEWIDE_TYPE_MISMATCH_W);
	      ffebad_string (ffelex_token_text (t));
	      if (g->tick == ffe_count_2)
		{
		  /* Current reference fills in type info for definition.
		     The current token doesn't necessarily point to the actual
		     definition of the function, so use the definition pointer
		     and the pointer to the pre-definition type info.  */
		  ffebad_here (0, ffelex_token_where_line (g->t),
			       ffelex_token_where_column (g->t));
		  ffebad_here (1, ffelex_token_where_line (g->u.proc.other_t),
			       ffelex_token_where_column (g->u.proc.other_t));
		}
	      else
		{
		  /* Current reference is not a filling-in of a current
		     definition.  The current token is fine, as is
		     the previous-mention token.  */
		  ffebad_here (0, ffelex_token_where_line (t),
			       ffelex_token_where_column (t));
		  ffebad_here (1, ffelex_token_where_line (g->t),
			       ffelex_token_where_column (g->t));
		}
	      ffebad_finish ();
	      if (error)
		g->type = FFEGLOBAL_typeANY;
	      return FALSE;
	    }
	}
    }

  if (g == NULL)
    {
      g = ffeglobal_new_ (n);
      g->t = ffelex_token_use (t);
      g->tick = ffe_count_2;
      g->intrinsic = FALSE;
      g->type = type;
      g->u.proc.defined = FALSE;
      g->u.proc.bt = ffesymbol_basictype (s);
      g->u.proc.kt = ffesymbol_kindtype (s);
      g->u.proc.sz = ffesymbol_size (s);
      g->u.proc.n_args = -1;
      ffesymbol_set_global (s, g);
    }
  else if (g->intrinsic
	   && !g->explicit_intrinsic
	   && (g->tick != ffe_count_2)
	   && ffe_is_warn_globals ())
    {
      /* Now known as a global, this name previously was seen as an
	 intrinsic.  Warn, in case the previous reference was intended
	 for the same global.  */
      ffebad_start (FFEBAD_INTRINSIC_GLOBAL);
      ffebad_string (ffelex_token_text (t));
      ffebad_string ("global");
      ffebad_string ("intrinsic");
      ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
      ffebad_here (1, ffelex_token_where_line (g->t),
		   ffelex_token_where_column (g->t));
      ffebad_finish ();
    }

  if ((g->type != type)
      && (type != FFEGLOBAL_typeEXT))
    {
      /* We've learned more, so point to where we learned it.  */
      g->t = ffelex_token_use (t);
      g->type = type;
      g->hook = FFECOM_globalNULL;	/* Discard previous _DECL. */
      g->u.proc.n_args = -1;
    }

  return TRUE;
#endif
}

/* ffeglobal_save_common -- Check SAVE status of common area

   ffesymbol s;	 // the common area
   bool save;  // TRUE if SAVEd, FALSE otherwise
   ffeglobal_save_common(s,save,ffesymbol_where_line(s),
	 ffesymbol_where_column(s));

   In global-enabled mode, make sure the save info agrees with any existing
   info established for the common area, otherwise complain.
   In global-disabled mode, do nothing.	 */

void
ffeglobal_save_common (ffesymbol s, bool save, ffewhereLine wl,
		       ffewhereColumn wc)
{
#if FFEGLOBAL_ENABLED
  ffeglobal g;

  g = ffesymbol_global (s);
  if ((g == NULL) || (g->type != FFEGLOBAL_typeCOMMON))
    return;			/* Let someone else catch this! */
  if (g->type == FFEGLOBAL_typeANY)
    return;

  if (!g->u.common.have_save)
    {
      g->u.common.have_save = TRUE;
      g->u.common.save = save;
      g->u.common.save_where_line = ffewhere_line_use (wl);
      g->u.common.save_where_col = ffewhere_column_use (wc);
    }
  else
    {
      if ((g->u.common.save != save) && ffe_is_pedantic ())
	{
	  ffebad_start (FFEBAD_COMMON_DIFF_SAVE);
	  ffebad_string (ffesymbol_text (s));
	  ffebad_here (save ? 0 : 1, wl, wc);
	  ffebad_here (save ? 1 : 0, g->u.common.pad_where_line, g->u.common.pad_where_col);
	  ffebad_finish ();
	}
    }
#endif
}

/* ffeglobal_size_common -- Establish size of COMMON area

   ffesymbol s;	 // the common area
   ffetargetOffset size;  // size in units
   if (ffeglobal_size_common(s,size))  // new size is largest seen

   In global-enabled mode, set the size if it current size isn't known or is
   smaller than new size, and for non-blank common, complain if old size
   is different from new.  Return TRUE if the new size is the largest seen
   for this COMMON area (or if no size was known for it previously).
   In global-disabled mode, do nothing.	 */

#if FFEGLOBAL_ENABLED
bool
ffeglobal_size_common (ffesymbol s, ffetargetOffset size)
{
  ffeglobal g;

  g = ffesymbol_global (s);
  if ((g == NULL) || (g->type != FFEGLOBAL_typeCOMMON))
    return FALSE;
  if (g->type == FFEGLOBAL_typeANY)
    return FALSE;

  if (!g->u.common.have_size)
    {
      g->u.common.have_size = TRUE;
      g->u.common.size = size;
      return TRUE;
    }

  if ((g->tick > 0) && (g->tick < ffe_count_2)
      && (g->u.common.size < size))
    {
      char oldsize[40];
      char newsize[40];

      /* Common block initialized in a previous program unit, which
	 effectively freezes its size, but now the program is trying
	 to enlarge it.  */

      sprintf (&oldsize[0], "%" ffetargetOffset_f "d", g->u.common.size);
      sprintf (&newsize[0], "%" ffetargetOffset_f "d", size);

      ffebad_start (FFEBAD_COMMON_ENLARGED);
      ffebad_string (ffesymbol_text (s));
      ffebad_string (oldsize);
      ffebad_string (newsize);
      ffebad_string ((g->u.common.size == 1)
		     ? FFECOM_SIZE_UNIT : FFECOM_SIZE_UNITS);
      ffebad_string ((size == 1)
		     ? FFECOM_SIZE_UNIT : FFECOM_SIZE_UNITS);
      ffebad_here (0, ffelex_token_where_line (g->u.common.initt),
		   ffelex_token_where_column (g->u.common.initt));
      ffebad_here (1, ffesymbol_where_line (s),
		   ffesymbol_where_column (s));
      ffebad_finish ();
    }
  else if ((g->u.common.size != size) && !g->u.common.blank)
    {
      char oldsize[40];
      char newsize[40];

      /* Warn about this even if not -pedantic, because putting all
	 program units in a single source file is the only way to
	 detect this.  Apparently UNIX-model linkers neither handle
	 nor report when they make a common unit smaller than
	 requested, such as when the smaller-declared version is
	 initialized and the larger-declared version is not.  So
	 if people complain about strange overwriting, we can tell
	 them to put all their code in a single file and compile
	 that way.  Warnings about differing sizes must therefore
	 always be issued.  */

      sprintf (&oldsize[0], "%" ffetargetOffset_f "d", g->u.common.size);
      sprintf (&newsize[0], "%" ffetargetOffset_f "d", size);

      ffebad_start (FFEBAD_COMMON_DIFF_SIZE);
      ffebad_string (ffesymbol_text (s));
      ffebad_string (oldsize);
      ffebad_string (newsize);
      ffebad_string ((g->u.common.size == 1)
		     ? FFECOM_SIZE_UNIT : FFECOM_SIZE_UNITS);
      ffebad_string ((size == 1)
		     ? FFECOM_SIZE_UNIT : FFECOM_SIZE_UNITS);
      ffebad_here (0, ffelex_token_where_line (g->t),
		   ffelex_token_where_column (g->t));
      ffebad_here (1, ffesymbol_where_line (s),
		   ffesymbol_where_column (s));
      ffebad_finish ();
    }

  if (size > g->u.common.size)
    {
      g->u.common.size = size;
      return TRUE;
    }

  return FALSE;
}

#endif
void
ffeglobal_terminate_1 (void)
{
}
