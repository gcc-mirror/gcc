/* intrin.c -- Recognize references to intrinsics
   Copyright (C) 1995-1998 Free Software Foundation, Inc.
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

#include "proj.h"
#include "intrin.h"
#include "expr.h"
#include "info.h"
#include "src.h"
#include "symbol.h"
#include "target.h"
#include "top.h"

struct _ffeintrin_name_
  {
    const char *name_uc;
    const char *name_lc;
    const char *name_ic;
    ffeintrinGen generic;
    ffeintrinSpec specific;
  };

struct _ffeintrin_gen_
  {
    const char *name;			/* Name as seen in program. */
    ffeintrinSpec specs[2];
  };

struct _ffeintrin_spec_
  {
    const char *name;		/* Uppercase name as seen in source code,
				   lowercase if no source name, "none" if no
				   name at all (NONE case). */
    bool is_actualarg;		/* Ok to pass as actual arg if -pedantic. */
    ffeintrinFamily family;
    ffeintrinImp implementation;
  };

struct _ffeintrin_imp_
  {
    const char *name;		/* Name of implementation. */
#if FFECOM_targetCURRENT == FFECOM_targetGCC
    ffecomGfrt gfrt_direct;	/* library routine, direct-callable form. */
    ffecomGfrt gfrt_f2c;	/* library routine, f2c-callable form. */
    ffecomGfrt gfrt_gnu;	/* library routine, gnu-callable form. */
#endif	/* FFECOM_targetCURRENT == FFECOM_targetGCC */
    const char *control;
    char y2kbad;
  };

static ffebad ffeintrin_check_ (ffeintrinImp imp, ffebldOp op,
				ffebld args, ffeinfoBasictype *xbt,
				ffeinfoKindtype *xkt,
				ffetargetCharacterSize *xsz,
				bool *check_intrin,
				ffelexToken t,
				bool commit);
static bool ffeintrin_check_any_ (ffebld arglist);
static int ffeintrin_cmp_name_ (const void *name, const void *intrinsic);

static struct _ffeintrin_name_ ffeintrin_names_[]
=
{				/* Alpha order. */
#define DEFNAME(UPPER,LOWER,MIXED,GEN,SPEC) \
  { UPPER, LOWER, MIXED, FFEINTRIN_ ## GEN, FFEINTRIN_ ## SPEC },
#define DEFGEN(CODE,NAME,SPEC1,SPEC2)
#define DEFSPEC(CODE,NAME,CALLABLE,FAMILY,IMP)
#define DEFIMP(CODE,NAME,GFRTDIRECT,GFRTF2C,GFRTGNU,CONTROL)
#define DEFIMPY(CODE,NAME,GFRTDIRECT,GFRTF2C,GFRTGNU,CONTROL,Y2KBAD)
#include "intrin.def"
#undef DEFNAME
#undef DEFGEN
#undef DEFSPEC
#undef DEFIMP
#undef DEFIMPY
};

static struct _ffeintrin_gen_ ffeintrin_gens_[]
=
{
#define DEFNAME(UPPER,LOWER,MIXED,GEN,SPEC)
#define DEFGEN(CODE,NAME,SPEC1,SPEC2) \
  { NAME, { SPEC1, SPEC2, }, },
#define DEFSPEC(CODE,NAME,CALLABLE,FAMILY,IMP)
#define DEFIMP(CODE,NAME,GFRTDIRECT,GFRTF2C,GFRTGNU,CONTROL)
#define DEFIMPY(CODE,NAME,GFRTDIRECT,GFRTF2C,GFRTGNU,CONTROL,Y2KBAD)
#include "intrin.def"
#undef DEFNAME
#undef DEFGEN
#undef DEFSPEC
#undef DEFIMP
#undef DEFIMPY
};

static struct _ffeintrin_imp_ ffeintrin_imps_[]
=
{
#define DEFNAME(UPPER,LOWER,MIXED,GEN,SPEC)
#define DEFGEN(CODE,NAME,SPEC1,SPEC2)
#define DEFSPEC(CODE,NAME,CALLABLE,FAMILY,IMP)
#if FFECOM_targetCURRENT == FFECOM_targetGCC
#define DEFIMP(CODE,NAME,GFRTDIRECT,GFRTF2C,GFRTGNU,CONTROL) \
      { NAME, FFECOM_gfrt ## GFRTDIRECT, FFECOM_gfrt ## GFRTF2C, \
	FFECOM_gfrt ## GFRTGNU, CONTROL, FALSE },
#define DEFIMPY(CODE,NAME,GFRTDIRECT,GFRTF2C,GFRTGNU,CONTROL,Y2KBAD) \
      { NAME, FFECOM_gfrt ## GFRTDIRECT, FFECOM_gfrt ## GFRTF2C, \
	FFECOM_gfrt ## GFRTGNU, CONTROL, Y2KBAD },
#elif FFECOM_targetCURRENT == FFECOM_targetFFE
#define DEFIMP(CODE,NAME,GFRTDIRECT,GFRTF2C,GFRTGNU,CONTROL) \
      { NAME, CONTROL, FALSE },
#define DEFIMPY(CODE,NAME,GFRTDIRECT,GFRTF2C,GFRTGNU,CONTROL,Y2KBAD) \
      { NAME, CONTROL, Y2KBAD },
#else
#error
#endif
#include "intrin.def"
#undef DEFNAME
#undef DEFGEN
#undef DEFSPEC
#undef DEFIMP
#undef DEFIMPY
};

static struct _ffeintrin_spec_ ffeintrin_specs_[]
=
{
#define DEFNAME(UPPER,LOWER,MIXED,GEN,SPEC)
#define DEFGEN(CODE,NAME,SPEC1,SPEC2)
#define DEFSPEC(CODE,NAME,CALLABLE,FAMILY,IMP) \
  { NAME, CALLABLE, FAMILY, IMP, },
#define DEFIMP(CODE,NAME,GFRTDIRECT,GFRTF2C,GFRTGNU,CONTROL)
#define DEFIMPY(CODE,NAME,GFRTDIRECT,GFRTF2C,GFRTGNU,CONTROL,Y2KBAD)
#include "intrin.def"
#undef DEFGEN
#undef DEFSPEC
#undef DEFIMP
#undef DEFIMPY
};


static ffebad
ffeintrin_check_ (ffeintrinImp imp, ffebldOp op,
		  ffebld args, ffeinfoBasictype *xbt,
		  ffeinfoKindtype *xkt,
		  ffetargetCharacterSize *xsz,
		  bool *check_intrin,
		  ffelexToken t,
		  bool commit)
{
  const char *c = ffeintrin_imps_[imp].control;
  bool subr = (c[0] == '-');
  const char *argc;
  ffebld arg;
  ffeinfoBasictype bt;
  ffeinfoKindtype kt;
  ffetargetCharacterSize sz = FFETARGET_charactersizeNONE;
  ffeinfoKindtype firstarg_kt;
  bool need_col;
  ffeinfoBasictype col_bt = FFEINFO_basictypeNONE;
  ffeinfoKindtype col_kt = FFEINFO_kindtypeNONE;
  int colon = (c[2] == ':') ? 2 : 3;
  int argno;

  /* Check procedure type (function vs. subroutine) against
     invocation.  */

  if (op == FFEBLD_opSUBRREF)
    {
      if (!subr)
	return FFEBAD_INTRINSIC_IS_FUNC;
    }
  else if (op == FFEBLD_opFUNCREF)
    {
      if (subr)
	return FFEBAD_INTRINSIC_IS_SUBR;
    }
  else
    return FFEBAD_INTRINSIC_REF;

  /* Check the arglist for validity.  */

  if ((args != NULL)
      && (ffebld_head (args) != NULL))
    firstarg_kt = ffeinfo_kindtype (ffebld_info (ffebld_head (args)));
  else
    firstarg_kt = FFEINFO_kindtype;

  for (argc = &c[colon + 3],
	 arg = args;
       *argc != '\0';
       )
    {
      char optional = '\0';
      char required = '\0';
      char extra = '\0';
      char basic;
      char kind;
      int length;
      int elements;
      bool lastarg_complex = FALSE;

      /* We don't do anything with keywords yet.  */
      do
	{
	} while (*(++argc) != '=');

      ++argc;
      if ((*argc == '?')
	  || (*argc == '!')
	  || (*argc == '*'))
	optional = *(argc++);
      if ((*argc == '+')
	  || (*argc == 'n')
	  || (*argc == 'p'))
	required = *(argc++);
      basic = *(argc++);
      kind = *(argc++);
      if (*argc == '[')
	{
	  length = *++argc - '0';
	  if (*++argc != ']')
	    length = 10 * length + (*(argc++) - '0');
	  ++argc;
	}
      else
	length = -1;
      if (*argc == '(')
	{
	  elements = *++argc - '0';
	  if (*++argc != ')')
	    elements = 10 * elements + (*(argc++) - '0');
	  ++argc;
	}
      else if (*argc == '&')
	{
	  elements = -1;
	  ++argc;
	}
      else
	elements = 0;
      if ((*argc == '&')
	  || (*argc == 'i')
	  || (*argc == 'w')
	  || (*argc == 'x'))
	extra = *(argc++);
      if (*argc == ',')
	++argc;

      /* Break out of this loop only when current arg spec completely
	 processed.  */

      do
	{
	  bool okay;
	  ffebld a;
	  ffeinfo i;
	  bool anynum;
	  ffeinfoBasictype abt = FFEINFO_basictypeNONE;
	  ffeinfoKindtype akt = FFEINFO_kindtypeNONE;

	  if ((arg == NULL)
	      || (ffebld_head (arg) == NULL))
	    {
	      if (required != '\0')
		return FFEBAD_INTRINSIC_TOOFEW;
	      if (optional == '\0')
		return FFEBAD_INTRINSIC_TOOFEW;
	      if (arg != NULL)
		arg = ffebld_trail (arg);
	      break;	/* Try next argspec. */
	    }

	  a = ffebld_head (arg);
	  i = ffebld_info (a);
	  anynum = (ffeinfo_basictype (i) == FFEINFO_basictypeHOLLERITH)
	    || (ffeinfo_basictype (i) == FFEINFO_basictypeTYPELESS);

	  /* See how well the arg matches up to the spec.  */

	  switch (basic)
	    {
	    case 'A':
	      okay = (ffeinfo_basictype (i) == FFEINFO_basictypeCHARACTER)
		&& ((length == -1)
		    || (ffeinfo_size (i) == (ffetargetCharacterSize) length));
	      break;

	    case 'C':
	      okay = anynum
		|| (ffeinfo_basictype (i) == FFEINFO_basictypeCOMPLEX);
	      abt = FFEINFO_basictypeCOMPLEX;
	      break;

	    case 'I':
	      okay = anynum
		|| (ffeinfo_basictype (i) == FFEINFO_basictypeINTEGER);
	      abt = FFEINFO_basictypeINTEGER;
	      break;

	    case 'L':
	      okay = anynum
		|| (ffeinfo_basictype (i) == FFEINFO_basictypeLOGICAL);
	      abt = FFEINFO_basictypeLOGICAL;
	      break;

	    case 'R':
	      okay = anynum
		|| (ffeinfo_basictype (i) == FFEINFO_basictypeREAL);
	      abt = FFEINFO_basictypeREAL;
	      break;

	    case 'B':
	      okay = anynum
		|| (ffeinfo_basictype (i) == FFEINFO_basictypeINTEGER)
		|| (ffeinfo_basictype (i) == FFEINFO_basictypeLOGICAL);
	      break;

	    case 'F':
	      okay = anynum
		|| (ffeinfo_basictype (i) == FFEINFO_basictypeCOMPLEX)
		|| (ffeinfo_basictype (i) == FFEINFO_basictypeREAL);
	      break;

	    case 'N':
	      okay = anynum
		|| (ffeinfo_basictype (i) == FFEINFO_basictypeCOMPLEX)
		|| (ffeinfo_basictype (i) == FFEINFO_basictypeINTEGER)
		|| (ffeinfo_basictype (i) == FFEINFO_basictypeREAL);
	      break;

	    case 'S':
	      okay = anynum
		|| (ffeinfo_basictype (i) == FFEINFO_basictypeINTEGER)
		|| (ffeinfo_basictype (i) == FFEINFO_basictypeREAL);
	      break;

	    case 'g':
	      okay = ((ffebld_op (a) == FFEBLD_opLABTER)
		      || (ffebld_op (a) == FFEBLD_opLABTOK));
	      elements = -1;
	      extra = '-';
	      break;

	    case 's':
	      okay = (((((ffeinfo_basictype (i) == FFEINFO_basictypeNONE)
			 && (ffeinfo_kindtype (i) == FFEINFO_kindtypeNONE)
			 && (ffeinfo_kind (i) == FFEINFO_kindSUBROUTINE))
			|| ((ffeinfo_basictype (i) == FFEINFO_basictypeINTEGER)
			    && (ffeinfo_kindtype (i) == FFEINFO_kindtypeINTEGERDEFAULT)
			    && (ffeinfo_kind (i) == FFEINFO_kindFUNCTION))
			|| (ffeinfo_kind (i) == FFEINFO_kindNONE))
		       && ((ffeinfo_where (i) == FFEINFO_whereDUMMY)
			   || (ffeinfo_where (i) == FFEINFO_whereGLOBAL)))
		      || ((ffeinfo_basictype (i) == FFEINFO_basictypeINTEGER)
			  && (ffeinfo_kind (i) == FFEINFO_kindENTITY)));
	      elements = -1;
	      extra = '-';
	      break;

	    case '-':
	    default:
	      okay = TRUE;
	      break;
	    }

	  switch (kind)
	    {
	    case '1': case '2': case '3': case '4': case '5':
	    case '6': case '7': case '8': case '9':
	      akt = (kind - '0');
	      if ((ffeinfo_basictype (i) == FFEINFO_basictypeINTEGER)
		  || (ffeinfo_basictype (i) == FFEINFO_basictypeLOGICAL))
		{
		  switch (akt)
		    {	/* Translate to internal kinds for now! */
		    default:
		      break;

		    case 2:
		      akt = 4;
		      break;

		    case 3:
		      akt = 2;
		      break;

		    case 4:
		      akt = 5;
		      break;

		    case 6:
		      akt = 3;
		      break;

		    case 7:
		      akt = ffecom_pointer_kind ();
		      break;
		    }
		}
	      okay &= anynum || (ffeinfo_kindtype (i) == akt);
	      break;

	    case 'A':
	      okay &= anynum || (ffeinfo_kindtype (i) == firstarg_kt);
	      akt = (firstarg_kt == FFEINFO_kindtype) ? FFEINFO_kindtypeNONE
		: firstarg_kt;
	      break;

	    case '*':
	    default:
	      break;
	    }

	  switch (elements)
	    {
	      ffebld b;

	    case -1:
	      break;

	    case 0:
	      if (ffeinfo_rank (i) != 0)
		okay = FALSE;
	      break;

	    default:
	      if ((ffeinfo_rank (i) != 1)
		  || (ffebld_op (a) != FFEBLD_opSYMTER)
		  || ((b = ffesymbol_arraysize (ffebld_symter (a))) == NULL)
		  || (ffebld_op (b) != FFEBLD_opCONTER)
		  || (ffeinfo_basictype (ffebld_info (b)) != FFEINFO_basictypeINTEGER)
		  || (ffeinfo_kindtype (ffebld_info (b)) != FFEINFO_kindtypeINTEGERDEFAULT)
		  || (ffebld_constant_integer1 (ffebld_conter (b)) != elements))
		okay = FALSE;
	      break;
	    }

	  switch (extra)
	    {
	    case '&':
	      if ((ffeinfo_kind (i) != FFEINFO_kindENTITY)
		  || ((ffebld_op (a) != FFEBLD_opSYMTER)
		      && (ffebld_op (a) != FFEBLD_opSUBSTR)
		      && (ffebld_op (a) != FFEBLD_opARRAYREF)))
		okay = FALSE;
	      break;

	    case 'w':
	    case 'x':
	      if ((ffeinfo_kind (i) != FFEINFO_kindENTITY)
		  || ((ffebld_op (a) != FFEBLD_opSYMTER)
		      && (ffebld_op (a) != FFEBLD_opARRAYREF)
		      && (ffebld_op (a) != FFEBLD_opSUBSTR)))
		okay = FALSE;
	      break;

	    case '-':
	    case 'i':
	      break;

	    default:
	      if (ffeinfo_kind (i) != FFEINFO_kindENTITY)
		okay = FALSE;
	      break;
	    }

	  if ((optional == '!')
	      && lastarg_complex)
	    okay = FALSE;

	  if (!okay)
	    {
	      /* If it wasn't optional, it's an error,
		 else maybe it could match a later argspec.  */
	      if (optional == '\0')
		return FFEBAD_INTRINSIC_REF;
	      break;	/* Try next argspec. */
	    }

	  lastarg_complex
	    = (ffeinfo_basictype (i) == FFEINFO_basictypeCOMPLEX);

	  if (anynum)
	    {
	      /* If we know dummy arg type, convert to that now.  */

	      if ((abt != FFEINFO_basictypeNONE)
		  && (akt != FFEINFO_kindtypeNONE)
		  && commit)
		{
		  /* We have a known type, convert hollerith/typeless
		     to it.  */

		  a = ffeexpr_convert (a, t, NULL,
				       abt, akt, 0,
				       FFETARGET_charactersizeNONE,
				       FFEEXPR_contextLET);
		  ffebld_set_head (arg, a);
		}
	    }

	  arg = ffebld_trail (arg);	/* Arg accepted, now move on. */

	  if (optional == '*')
	    continue;	/* Go ahead and try another arg. */
	  if (required == '\0')
	    break;
	  if ((required == 'n')
	      || (required == '+'))
	    {
	      optional = '*';
	      required = '\0';
	    }
	  else if (required == 'p')
	    required = 'n';
	} while (TRUE);
    }

  if (arg != NULL)
    return FFEBAD_INTRINSIC_TOOMANY;

  /* Set up the initial type for the return value of the function.  */

  need_col = FALSE;
  switch (c[0])
    {
    case 'A':
      bt = FFEINFO_basictypeCHARACTER;
      sz = (c[2] == '*') ? FFETARGET_charactersizeNONE : 1;
      break;

    case 'C':
      bt = FFEINFO_basictypeCOMPLEX;
      break;

    case 'I':
      bt = FFEINFO_basictypeINTEGER;
      break;

    case 'L':
      bt = FFEINFO_basictypeLOGICAL;
      break;

    case 'R':
      bt = FFEINFO_basictypeREAL;
      break;

    case 'B':
    case 'F':
    case 'N':
    case 'S':
      need_col = TRUE;
      /* Fall through.  */
    case '-':
    default:
      bt = FFEINFO_basictypeNONE;
      break;
    }

  switch (c[1])
    {
    case '1': case '2': case '3': case '4': case '5':
    case '6': case '7': case '8': case '9':
      kt = (c[1] - '0');
      if ((bt == FFEINFO_basictypeINTEGER)
	  || (bt == FFEINFO_basictypeLOGICAL))
	{
	  switch (kt)
	    {	/* Translate to internal kinds for now! */
	    default:
	      break;

	    case 2:
	      kt = 4;
	      break;

	    case 3:
	      kt = 2;
	      break;

	    case 4:
	      kt = 5;
	      break;

	    case 6:
	      kt = 3;
	      break;

	    case 7:
	      kt = ffecom_pointer_kind ();
	      break;
	    }
	}
      break;

    case 'C':
      if (ffe_is_90 ())
	need_col = TRUE;
      kt = 1;
      break;

    case '=':
      need_col = TRUE;
      /* Fall through.  */
    case '-':
    default:
      kt = FFEINFO_kindtypeNONE;
      break;
    }

  /* Determine collective type of COL, if there is one.  */

  if (need_col || c[colon + 1] != '-')
    {
      bool okay = TRUE;
      bool have_anynum = FALSE;

      for (arg = args;
	   arg != NULL;
	   arg = (c[colon + 1] == '*') ? ffebld_trail (arg) : NULL)
	{
	  ffebld a = ffebld_head (arg);
	  ffeinfo i;
	  bool anynum;

	  if (a == NULL)
	    continue;
	  i = ffebld_info (a);

	  anynum = (ffeinfo_basictype (i) == FFEINFO_basictypeHOLLERITH)
	    || (ffeinfo_basictype (i) == FFEINFO_basictypeTYPELESS);
	  if (anynum)
	    {
	      have_anynum = TRUE;
	      continue;
	    }

	  if ((col_bt == FFEINFO_basictypeNONE)
	      && (col_kt == FFEINFO_kindtypeNONE))
	    {
	      col_bt = ffeinfo_basictype (i);
	      col_kt = ffeinfo_kindtype (i);
	    }
	  else
	    {
	      ffeexpr_type_combine (&col_bt, &col_kt,
				    col_bt, col_kt,
				    ffeinfo_basictype (i),
				    ffeinfo_kindtype (i),
				    NULL);
	      if ((col_bt == FFEINFO_basictypeNONE)
		  || (col_kt == FFEINFO_kindtypeNONE))
		return FFEBAD_INTRINSIC_REF;
	    }
	}

      if (have_anynum
	  && ((col_bt == FFEINFO_basictypeNONE)
	      || (col_kt == FFEINFO_kindtypeNONE)))
	{
	  /* No type, but have hollerith/typeless.  Use type of return
	     value to determine type of COL.  */

	  switch (c[0])
	    {
	    case 'A':
	      return FFEBAD_INTRINSIC_REF;

	    case 'B':
	    case 'I':
	    case 'L':
	      if ((col_bt != FFEINFO_basictypeNONE)
		  && (col_bt != FFEINFO_basictypeINTEGER))
		return FFEBAD_INTRINSIC_REF;
	      /* Fall through.  */
	    case 'N':
	    case 'S':
	    case '-':
	    default:
	      col_bt = FFEINFO_basictypeINTEGER;
	      col_kt = FFEINFO_kindtypeINTEGER1;
	      break;

	    case 'C':
	      if ((col_bt != FFEINFO_basictypeNONE)
		  && (col_bt != FFEINFO_basictypeCOMPLEX))
		return FFEBAD_INTRINSIC_REF;
	      col_bt = FFEINFO_basictypeCOMPLEX;
	      col_kt = FFEINFO_kindtypeREAL1;
	      break;

	    case 'R':
	      if ((col_bt != FFEINFO_basictypeNONE)
		  && (col_bt != FFEINFO_basictypeREAL))
		return FFEBAD_INTRINSIC_REF;
	      /* Fall through.  */
	    case 'F':
	      col_bt = FFEINFO_basictypeREAL;
	      col_kt = FFEINFO_kindtypeREAL1;
	      break;
	    }
	}

      switch (c[0])
	{
	case 'B':
	  okay = (col_bt == FFEINFO_basictypeINTEGER)
	    || (col_bt == FFEINFO_basictypeLOGICAL);
	  if (need_col)
	    bt = col_bt;
	  break;

	case 'F':
	  okay = (col_bt == FFEINFO_basictypeCOMPLEX)
	    || (col_bt == FFEINFO_basictypeREAL);
	  if (need_col)
	    bt = col_bt;
	  break;

	case 'N':
	  okay = (col_bt == FFEINFO_basictypeCOMPLEX)
	    || (col_bt == FFEINFO_basictypeINTEGER)
	    || (col_bt == FFEINFO_basictypeREAL);
	  if (need_col)
	    bt = col_bt;
	  break;

	case 'S':
	  okay = (col_bt == FFEINFO_basictypeINTEGER)
	    || (col_bt == FFEINFO_basictypeREAL)
	    || (col_bt == FFEINFO_basictypeCOMPLEX);
	  if (need_col)
	    bt = ((col_bt != FFEINFO_basictypeCOMPLEX) ? col_bt
		  : FFEINFO_basictypeREAL);
	  break;
	}

      switch (c[1])
	{
	case '=':
	  if (need_col)
	    kt = col_kt;
	  break;

	case 'C':
	  if (col_bt == FFEINFO_basictypeCOMPLEX)
	    {
	      if (col_kt != FFEINFO_kindtypeREALDEFAULT)
		*check_intrin = TRUE;
	      if (need_col)
		kt = col_kt;
	    }
	  break;
	}

      if (!okay)
	return FFEBAD_INTRINSIC_REF;
    }

  /* Now, convert args in the arglist to the final type of the COL.  */

  for (argno = 0, argc = &c[colon + 3],
	 arg = args;
       *argc != '\0';
       ++argno)
    {
      char optional = '\0';
      char required = '\0';
      char extra = '\0';
      char basic;
      char kind;
      int length;
      int elements;
      bool lastarg_complex = FALSE;

      /* We don't do anything with keywords yet.  */
      do
	{
	} while (*(++argc) != '=');

      ++argc;
      if ((*argc == '?')
	  || (*argc == '!')
	  || (*argc == '*'))
	optional = *(argc++);
      if ((*argc == '+')
	  || (*argc == 'n')
	  || (*argc == 'p'))
	required = *(argc++);
      basic = *(argc++);
      kind = *(argc++);
      if (*argc == '[')
	{
	  length = *++argc - '0';
	  if (*++argc != ']')
	    length = 10 * length + (*(argc++) - '0');
	  ++argc;
	}
      else
	length = -1;
      if (*argc == '(')
	{
	  elements = *++argc - '0';
	  if (*++argc != ')')
	    elements = 10 * elements + (*(argc++) - '0');
	  ++argc;
	}
      else if (*argc == '&')
	{
	  elements = -1;
	  ++argc;
	}
      else
	elements = 0;
      if ((*argc == '&')
	  || (*argc == 'i')
	  || (*argc == 'w')
	  || (*argc == 'x'))
	extra = *(argc++);
      if (*argc == ',')
	++argc;

      /* Break out of this loop only when current arg spec completely
	 processed.  */

      do
	{
	  bool okay;
	  ffebld a;
	  ffeinfo i;
	  bool anynum;
	  ffeinfoBasictype abt = FFEINFO_basictypeNONE;
	  ffeinfoKindtype akt = FFEINFO_kindtypeNONE;

	  if ((arg == NULL)
	      || (ffebld_head (arg) == NULL))
	    {
	      if (arg != NULL)
		arg = ffebld_trail (arg);
	      break;	/* Try next argspec. */
	    }

	  a = ffebld_head (arg);
	  i = ffebld_info (a);
	  anynum = (ffeinfo_basictype (i) == FFEINFO_basictypeHOLLERITH)
	    || (ffeinfo_basictype (i) == FFEINFO_basictypeTYPELESS);

	  /* Determine what the default type for anynum would be.  */

	  if (anynum)
	    {
	      switch (c[colon + 1])
		{
		case '-':
		  break;
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
		  if (argno != (c[colon + 1] - '0'))
		    break;
		case '*':
		  abt = col_bt;
		  akt = col_kt;
		  break;
		}
	    }

	  /* Again, match arg up to the spec.  We go through all of
	     this again to properly follow the contour of optional
	     arguments.  Probably this level of flexibility is not
	     needed, perhaps it's even downright naughty.  */

	  switch (basic)
	    {
	    case 'A':
	      okay = (ffeinfo_basictype (i) == FFEINFO_basictypeCHARACTER)
		&& ((length == -1)
		    || (ffeinfo_size (i) == (ffetargetCharacterSize) length));
	      break;

	    case 'C':
	      okay = anynum
		|| (ffeinfo_basictype (i) == FFEINFO_basictypeCOMPLEX);
	      abt = FFEINFO_basictypeCOMPLEX;
	      break;

	    case 'I':
	      okay = anynum
		|| (ffeinfo_basictype (i) == FFEINFO_basictypeINTEGER);
	      abt = FFEINFO_basictypeINTEGER;
	      break;

	    case 'L':
	      okay = anynum
		|| (ffeinfo_basictype (i) == FFEINFO_basictypeLOGICAL);
	      abt = FFEINFO_basictypeLOGICAL;
	      break;

	    case 'R':
	      okay = anynum
		|| (ffeinfo_basictype (i) == FFEINFO_basictypeREAL);
	      abt = FFEINFO_basictypeREAL;
	      break;

	    case 'B':
	      okay = anynum
		|| (ffeinfo_basictype (i) == FFEINFO_basictypeINTEGER)
		|| (ffeinfo_basictype (i) == FFEINFO_basictypeLOGICAL);
	      break;

	    case 'F':
	      okay = anynum
		|| (ffeinfo_basictype (i) == FFEINFO_basictypeCOMPLEX)
		|| (ffeinfo_basictype (i) == FFEINFO_basictypeREAL);
	      break;

	    case 'N':
	      okay = anynum
		|| (ffeinfo_basictype (i) == FFEINFO_basictypeCOMPLEX)
		|| (ffeinfo_basictype (i) == FFEINFO_basictypeINTEGER)
		|| (ffeinfo_basictype (i) == FFEINFO_basictypeREAL);
	      break;

	    case 'S':
	      okay = anynum
		|| (ffeinfo_basictype (i) == FFEINFO_basictypeINTEGER)
		|| (ffeinfo_basictype (i) == FFEINFO_basictypeREAL);
	      break;

	    case 'g':
	      okay = ((ffebld_op (a) == FFEBLD_opLABTER)
		      || (ffebld_op (a) == FFEBLD_opLABTOK));
	      elements = -1;
	      extra = '-';
	      break;

	    case 's':
	      okay = (((((ffeinfo_basictype (i) == FFEINFO_basictypeNONE)
			 && (ffeinfo_kindtype (i) == FFEINFO_kindtypeNONE)
			 && (ffeinfo_kind (i) == FFEINFO_kindSUBROUTINE))
			|| ((ffeinfo_basictype (i) == FFEINFO_basictypeINTEGER)
			    && (ffeinfo_kindtype (i) == FFEINFO_kindtypeINTEGERDEFAULT)
			    && (ffeinfo_kind (i) == FFEINFO_kindFUNCTION))
			|| (ffeinfo_kind (i) == FFEINFO_kindNONE))
		       && ((ffeinfo_where (i) == FFEINFO_whereDUMMY)
			   || (ffeinfo_where (i) == FFEINFO_whereGLOBAL)))
		      || ((ffeinfo_basictype (i) == FFEINFO_basictypeINTEGER)
			  && (ffeinfo_kind (i) == FFEINFO_kindENTITY)));
	      elements = -1;
	      extra = '-';
	      break;

	    case '-':
	    default:
	      okay = TRUE;
	      break;
	    }

	  switch (kind)
	    {
	    case '1': case '2': case '3': case '4': case '5':
	    case '6': case '7': case '8': case '9':
	      akt = (kind - '0');
	      if ((ffeinfo_basictype (i) == FFEINFO_basictypeINTEGER)
		  || (ffeinfo_basictype (i) == FFEINFO_basictypeLOGICAL))
		{
		  switch (akt)
		    {	/* Translate to internal kinds for now! */
		    default:
		      break;

		    case 2:
		      akt = 4;
		      break;

		    case 3:
		      akt = 2;
		      break;

		    case 4:
		      akt = 5;
		      break;

		    case 6:
		      akt = 3;
		      break;

		    case 7:
		      akt = ffecom_pointer_kind ();
		      break;
		    }
		}
	      okay &= anynum || (ffeinfo_kindtype (i) == akt);
	      break;

	    case 'A':
	      okay &= anynum || (ffeinfo_kindtype (i) == firstarg_kt);
	      akt = (firstarg_kt == FFEINFO_kindtype) ? FFEINFO_kindtypeNONE
		: firstarg_kt;
	      break;

	    case '*':
	    default:
	      break;
	    }

	  switch (elements)
	    {
	      ffebld b;

	    case -1:
	      break;

	    case 0:
	      if (ffeinfo_rank (i) != 0)
		okay = FALSE;
	      break;

	    default:
	      if ((ffeinfo_rank (i) != 1)
		  || (ffebld_op (a) != FFEBLD_opSYMTER)
		  || ((b = ffesymbol_arraysize (ffebld_symter (a))) == NULL)
		  || (ffebld_op (b) != FFEBLD_opCONTER)
		  || (ffeinfo_basictype (ffebld_info (b)) != FFEINFO_basictypeINTEGER)
		  || (ffeinfo_kindtype (ffebld_info (b)) != FFEINFO_kindtypeINTEGERDEFAULT)
		  || (ffebld_constant_integer1 (ffebld_conter (b)) != elements))
		okay = FALSE;
	      break;
	    }

	  switch (extra)
	    {
	    case '&':
	      if ((ffeinfo_kind (i) != FFEINFO_kindENTITY)
		  || ((ffebld_op (a) != FFEBLD_opSYMTER)
		      && (ffebld_op (a) != FFEBLD_opSUBSTR)
		      && (ffebld_op (a) != FFEBLD_opARRAYREF)))
		okay = FALSE;
	      break;

	    case 'w':
	    case 'x':
	      if ((ffeinfo_kind (i) != FFEINFO_kindENTITY)
		  || ((ffebld_op (a) != FFEBLD_opSYMTER)
		      && (ffebld_op (a) != FFEBLD_opARRAYREF)
		      && (ffebld_op (a) != FFEBLD_opSUBSTR)))
		okay = FALSE;
	      break;

	    case '-':
	    case 'i':
	      break;

	    default:
	      if (ffeinfo_kind (i) != FFEINFO_kindENTITY)
		okay = FALSE;
	      break;
	    }

	  if ((optional == '!')
	      && lastarg_complex)
	    okay = FALSE;

	  if (!okay)
	    {
	      /* If it wasn't optional, it's an error,
		 else maybe it could match a later argspec.  */
	      if (optional == '\0')
		return FFEBAD_INTRINSIC_REF;
	      break;	/* Try next argspec. */
	    }

	  lastarg_complex
	    = (ffeinfo_basictype (i) == FFEINFO_basictypeCOMPLEX);

	  if (anynum && commit)
	    {
	      /* If we know dummy arg type, convert to that now.  */

	      if (abt == FFEINFO_basictypeNONE)
		abt = FFEINFO_basictypeINTEGER;
	      if (akt == FFEINFO_kindtypeNONE)
		akt = FFEINFO_kindtypeINTEGER1;

	      /* We have a known type, convert hollerith/typeless to it.  */

	      a = ffeexpr_convert (a, t, NULL,
				   abt, akt, 0,
				   FFETARGET_charactersizeNONE,
				   FFEEXPR_contextLET);
	      ffebld_set_head (arg, a);
	    }
	  else if ((c[colon + 1] == '*') && commit)
	    {
	      /* This is where we promote types to the consensus
		 type for the COL.  Maybe this is where -fpedantic
		 should issue a warning as well.  */

	      a = ffeexpr_convert (a, t, NULL,
				   col_bt, col_kt, 0,
				   ffeinfo_size (i),
				   FFEEXPR_contextLET);
	      ffebld_set_head (arg, a);
	    }

	  arg = ffebld_trail (arg);	/* Arg accepted, now move on. */

	  if (optional == '*')
	    continue;	/* Go ahead and try another arg. */
	  if (required == '\0')
	    break;
	  if ((required == 'n')
	      || (required == '+'))
	    {
	      optional = '*';
	      required = '\0';
	    }
	  else if (required == 'p')
	    required = 'n';
	} while (TRUE);
    }

  *xbt = bt;
  *xkt = kt;
  *xsz = sz;
  return FFEBAD;
}

static bool
ffeintrin_check_any_ (ffebld arglist)
{
  ffebld item;

  for (; arglist != NULL; arglist = ffebld_trail (arglist))
    {
      item = ffebld_head (arglist);
      if ((item != NULL)
	  && (ffebld_op (item) == FFEBLD_opANY))
	return TRUE;
    }

  return FALSE;
}

/* Compare name to intrinsic's name.  Uses strcmp on arguments' names.	*/

static int
ffeintrin_cmp_name_ (const void *name, const void *intrinsic)
{
  const char *uc = ((struct _ffeintrin_name_ *) intrinsic)->name_uc;
  const char *lc = ((struct _ffeintrin_name_ *) intrinsic)->name_lc;
  const char *ic = ((struct _ffeintrin_name_ *) intrinsic)->name_ic;

  return ffesrc_strcmp_2c (ffe_case_intrin (), name, uc, lc, ic);
}

/* Return basic type of intrinsic implementation, based on its
   run-time implementation *only*.  (This is used only when
   the type of an intrinsic name is needed without having a
   list of arguments, i.e. an interface signature, such as when
   passing the intrinsic itself, or really the run-time-library
   function, as an argument.)

   If there's no eligible intrinsic implementation, there must be
   a bug somewhere else; no such reference should have been permitted
   to go this far.  (Well, this might be wrong.)  */

ffeinfoBasictype
ffeintrin_basictype (ffeintrinSpec spec)
{
  ffeintrinImp imp;
  ffecomGfrt gfrt;

  assert (spec < FFEINTRIN_spec);
  imp = ffeintrin_specs_[spec].implementation;
  assert (imp < FFEINTRIN_imp);

  if (ffe_is_f2c ())
    gfrt = ffeintrin_imps_[imp].gfrt_f2c;
  else 
    gfrt = ffeintrin_imps_[imp].gfrt_gnu;

  assert (gfrt != FFECOM_gfrt);

  return ffecom_gfrt_basictype (gfrt);
}

/* Return family to which specific intrinsic belongs.  */

ffeintrinFamily
ffeintrin_family (ffeintrinSpec spec)
{
  if (spec >= FFEINTRIN_spec)
    return FALSE;
  return ffeintrin_specs_[spec].family;
}

/* Check and fill in info on func/subr ref node.

   ffebld expr;			// FUNCREF or SUBRREF with no info (caller
				// gets it from the modified info structure).
   ffeinfo info;		// Already filled in, will be overwritten.
   ffelexToken token;		// Used for error message.
   ffeintrin_fulfill_generic (&expr, &info, token);

   Based on the generic id, figure out which specific procedure is meant and
   pick that one.  Else return an error, a la _specific.  */

void
ffeintrin_fulfill_generic (ffebld *expr, ffeinfo *info, ffelexToken t)
{
  ffebld symter;
  ffebldOp op;
  ffeintrinGen gen;
  ffeintrinSpec spec = FFEINTRIN_specNONE;
  ffeinfoBasictype bt = FFEINFO_basictypeNONE;
  ffeinfoKindtype kt = FFEINFO_kindtypeNONE;
  ffetargetCharacterSize sz = FFETARGET_charactersizeNONE;
  ffeintrinImp imp;
  ffeintrinSpec tspec;
  ffeintrinImp nimp = FFEINTRIN_impNONE;
  ffebad error;
  bool any = FALSE;
  bool highly_specific = FALSE;
  int i;

  op = ffebld_op (*expr);
  assert ((op == FFEBLD_opFUNCREF) || (op == FFEBLD_opSUBRREF));
  assert (ffebld_op (ffebld_left (*expr)) == FFEBLD_opSYMTER);

  gen = ffebld_symter_generic (ffebld_left (*expr));
  assert (gen != FFEINTRIN_genNONE);

  imp = FFEINTRIN_impNONE;
  error = FFEBAD;

  any = ffeintrin_check_any_ (ffebld_right (*expr));

  for (i = 0;
       (((size_t) i) < ARRAY_SIZE (ffeintrin_gens_[gen].specs))
	 && ((tspec = ffeintrin_gens_[gen].specs[i]) != FFEINTRIN_specNONE)
	 && !any;
       ++i)
    {
      ffeintrinImp timp = ffeintrin_specs_[tspec].implementation;
      ffeinfoBasictype tbt;
      ffeinfoKindtype tkt;
      ffetargetCharacterSize tsz;
      ffeIntrinsicState state
      = ffeintrin_state_family (ffeintrin_specs_[tspec].family);
      ffebad terror;

      if (state == FFE_intrinsicstateDELETED)
	continue;

      if (timp != FFEINTRIN_impNONE)
	{
	  if (!(ffeintrin_imps_[timp].control[0] == '-')
	      != !(ffebld_op (*expr) == FFEBLD_opSUBRREF))
	    continue;		/* Form of reference must match form of specific. */
	}

      if (state == FFE_intrinsicstateDISABLED)
	terror = FFEBAD_INTRINSIC_DISABLED;
      else if (timp == FFEINTRIN_impNONE)
	terror = FFEBAD_INTRINSIC_UNIMPL;
      else
	{
	  terror = ffeintrin_check_ (timp, ffebld_op (*expr),
				     ffebld_right (*expr),
				     &tbt, &tkt, &tsz, NULL, t, FALSE);
	  if (terror == FFEBAD)
	    {
	      if (imp != FFEINTRIN_impNONE)
		{
		  ffebad_start (FFEBAD_INTRINSIC_AMBIG);
		  ffebad_here (0, ffelex_token_where_line (t),
			       ffelex_token_where_column (t));
		  ffebad_string (ffeintrin_gens_[gen].name);
		  ffebad_string (ffeintrin_specs_[spec].name);
		  ffebad_string (ffeintrin_specs_[tspec].name);
		  ffebad_finish ();
		}
	      else
		{
		  if (ffebld_symter_specific (ffebld_left (*expr))
		      == tspec)
		    highly_specific = TRUE;
		  imp = timp;
		  spec = tspec;
		  bt = tbt;
		  kt = tkt;
		  sz = tkt;
		  error = terror;
		}
	    }
	  else if (terror != FFEBAD)
	    {			/* This error has precedence over others. */
	      if ((error == FFEBAD_INTRINSIC_DISABLED)
		  || (error == FFEBAD_INTRINSIC_UNIMPL))
		error = FFEBAD;
	    }
	}

      if (error == FFEBAD)
	error = terror;
    }

  if (any || (imp == FFEINTRIN_impNONE))
    {
      if (!any)
	{
	  if (error == FFEBAD)
	    error = FFEBAD_INTRINSIC_REF;
	  ffebad_start (error);
	  ffebad_here (0, ffelex_token_where_line (t),
		       ffelex_token_where_column (t));
	  ffebad_string (ffeintrin_gens_[gen].name);
	  ffebad_finish ();
	}

      *expr = ffebld_new_any ();
      *info = ffeinfo_new_any ();
    }
  else
    {
      if (!highly_specific && (nimp != FFEINTRIN_impNONE))
	{
	  fprintf (stderr, "lineno=%ld, gen=%s, imp=%s, timp=%s\n",
		   (long) lineno,
		   ffeintrin_gens_[gen].name,
		   ffeintrin_imps_[imp].name,
		   ffeintrin_imps_[nimp].name);
	  assert ("Ambiguous generic reference" == NULL);
	  abort ();
	}
      error = ffeintrin_check_ (imp, ffebld_op (*expr),
				ffebld_right (*expr),
				&bt, &kt, &sz, NULL, t, TRUE);
      assert (error == FFEBAD);
      *info = ffeinfo_new (bt,
			   kt,
			   0,
			   FFEINFO_kindENTITY,
			   FFEINFO_whereFLEETING,
			   sz);
      symter = ffebld_left (*expr);
      ffebld_symter_set_specific (symter, spec);
      ffebld_symter_set_implementation (symter, imp);
      ffebld_set_info (symter,
		       ffeinfo_new (bt,
				    kt,
				    0,
				    (bt == FFEINFO_basictypeNONE)
				    ? FFEINFO_kindSUBROUTINE
				    : FFEINFO_kindFUNCTION,
				    FFEINFO_whereINTRINSIC,
				    sz));

      if ((ffesymbol_attrs (ffebld_symter (symter)) & FFESYMBOL_attrsTYPE)
	  && (((bt != ffesymbol_basictype (ffebld_symter (symter)))
	       || (kt != ffesymbol_kindtype (ffebld_symter (symter)))
	       || ((sz != FFETARGET_charactersizeNONE)
		   && (sz != ffesymbol_size (ffebld_symter (symter)))))))
	{
	  ffebad_start (FFEBAD_INTRINSIC_TYPE);
	  ffebad_here (0, ffelex_token_where_line (t),
		       ffelex_token_where_column (t));
	  ffebad_string (ffeintrin_gens_[gen].name);
	  ffebad_finish ();
	}
      if (ffeintrin_imps_[imp].y2kbad)
	{
	  ffebad_start (FFEBAD_INTRINSIC_Y2KBAD);
	  ffebad_here (0, ffelex_token_where_line (t),
		       ffelex_token_where_column (t));
	  ffebad_string (ffeintrin_gens_[gen].name);
	  ffebad_finish ();
	}
    }
}

/* Check and fill in info on func/subr ref node.

   ffebld expr;			// FUNCREF or SUBRREF with no info (caller
				// gets it from the modified info structure).
   ffeinfo info;		// Already filled in, will be overwritten.
   bool check_intrin;           // May be omitted, else set TRUE if intrinsic needs checking.
   ffelexToken token;		// Used for error message.
   ffeintrin_fulfill_specific (&expr, &info, &check_intrin, token);

   Based on the specific id, determine whether the arg list is valid
   (number, type, rank, and kind of args) and fill in the info structure
   accordingly.	 Currently don't rewrite the expression, but perhaps
   someday do so for constant collapsing, except when an error occurs,
   in which case it is overwritten with ANY and info is also overwritten
   accordingly.	 */

void
ffeintrin_fulfill_specific (ffebld *expr, ffeinfo *info,
			    bool *check_intrin, ffelexToken t)
{
  ffebld symter;
  ffebldOp op;
  ffeintrinGen gen;
  ffeintrinSpec spec;
  ffeintrinImp imp;
  ffeinfoBasictype bt = FFEINFO_basictypeNONE;
  ffeinfoKindtype kt = FFEINFO_kindtypeNONE;
  ffetargetCharacterSize sz = FFETARGET_charactersizeNONE;
  ffeIntrinsicState state;
  ffebad error;
  bool any = FALSE;
  const char *name;

  op = ffebld_op (*expr);
  assert ((op == FFEBLD_opFUNCREF) || (op == FFEBLD_opSUBRREF));
  assert (ffebld_op (ffebld_left (*expr)) == FFEBLD_opSYMTER);

  gen = ffebld_symter_generic (ffebld_left (*expr));
  spec = ffebld_symter_specific (ffebld_left (*expr));
  assert (spec != FFEINTRIN_specNONE);

  if (gen != FFEINTRIN_genNONE)
    name = ffeintrin_gens_[gen].name;
  else
    name = ffeintrin_specs_[spec].name;

  state = ffeintrin_state_family (ffeintrin_specs_[spec].family);

  imp = ffeintrin_specs_[spec].implementation;
  if (check_intrin != NULL)
    *check_intrin = FALSE;

  any = ffeintrin_check_any_ (ffebld_right (*expr));

  if (state == FFE_intrinsicstateDISABLED)
    error = FFEBAD_INTRINSIC_DISABLED;
  else if (imp == FFEINTRIN_impNONE)
    error = FFEBAD_INTRINSIC_UNIMPL;
  else if (!any)
    {
      error = ffeintrin_check_ (imp, ffebld_op (*expr),
				ffebld_right (*expr),
				&bt, &kt, &sz, check_intrin, t, TRUE);
    }
  else
    error = FFEBAD;	/* Not really needed, but quiet -Wuninitialized. */

  if (any || (error != FFEBAD))
    {
      if (!any)
	{

	  ffebad_start (error);
	  ffebad_here (0, ffelex_token_where_line (t),
		       ffelex_token_where_column (t));
	  ffebad_string (name);
	  ffebad_finish ();
	}

      *expr = ffebld_new_any ();
      *info = ffeinfo_new_any ();
    }
  else
    {
      *info = ffeinfo_new (bt,
			   kt,
			   0,
			   FFEINFO_kindENTITY,
			   FFEINFO_whereFLEETING,
			   sz);
      symter = ffebld_left (*expr);
      ffebld_set_info (symter,
		       ffeinfo_new (bt,
				    kt,
				    0,
				    (bt == FFEINFO_basictypeNONE)
				    ? FFEINFO_kindSUBROUTINE
				    : FFEINFO_kindFUNCTION,
				    FFEINFO_whereINTRINSIC,
				    sz));

      if ((ffesymbol_attrs (ffebld_symter (symter)) & FFESYMBOL_attrsTYPE)
	  && (((bt != ffesymbol_basictype (ffebld_symter (symter)))
	       || (kt != ffesymbol_kindtype (ffebld_symter (symter)))
	       || (sz != ffesymbol_size (ffebld_symter (symter))))))
	{
	  ffebad_start (FFEBAD_INTRINSIC_TYPE);
	  ffebad_here (0, ffelex_token_where_line (t),
		       ffelex_token_where_column (t));
	  ffebad_string (name);
	  ffebad_finish ();
	}
      if (ffeintrin_imps_[imp].y2kbad)
	{
	  ffebad_start (FFEBAD_INTRINSIC_Y2KBAD);
	  ffebad_here (0, ffelex_token_where_line (t),
		       ffelex_token_where_column (t));
	  ffebad_string (name);
	  ffebad_finish ();
	}
    }
}

/* Return run-time index of intrinsic implementation as direct call.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
ffecomGfrt
ffeintrin_gfrt_direct (ffeintrinImp imp)
{
  assert (imp < FFEINTRIN_imp);

  return ffeintrin_imps_[imp].gfrt_direct;
}
#endif

/* Return run-time index of intrinsic implementation as actual argument.  */

#if FFECOM_targetCURRENT == FFECOM_targetGCC
ffecomGfrt
ffeintrin_gfrt_indirect (ffeintrinImp imp)
{
  assert (imp < FFEINTRIN_imp);

  if (! ffe_is_f2c ())
    return ffeintrin_imps_[imp].gfrt_gnu;
  return ffeintrin_imps_[imp].gfrt_f2c;
}
#endif

void
ffeintrin_init_0 ()
{
  int i;
  const char *p1;
  const char *p2;
  const char *p3;
  int colon;

  if (!ffe_is_do_internal_checks ())
    return;

  assert (FFEINTRIN_gen == ARRAY_SIZE (ffeintrin_gens_));
  assert (FFEINTRIN_imp == ARRAY_SIZE (ffeintrin_imps_));
  assert (FFEINTRIN_spec == ARRAY_SIZE (ffeintrin_specs_));

  for (i = 1; ((size_t) i) < ARRAY_SIZE (ffeintrin_names_); ++i)
    {				/* Make sure binary-searched list is in alpha
				   order. */
      if (strcmp (ffeintrin_names_[i - 1].name_uc,
		  ffeintrin_names_[i].name_uc) >= 0)
	assert ("name list out of order" == NULL);
    }

  for (i = 0; ((size_t) i) < ARRAY_SIZE (ffeintrin_names_); ++i)
    {
      assert ((ffeintrin_names_[i].generic == FFEINTRIN_genNONE)
	      || (ffeintrin_names_[i].specific == FFEINTRIN_specNONE));

      p1 = ffeintrin_names_[i].name_uc;
      p2 = ffeintrin_names_[i].name_lc;
      p3 = ffeintrin_names_[i].name_ic;
      for (; *p1 != '\0' && *p2 != '\0' && *p3 != '\0'; ++p1, ++p2, ++p3)
	{
	  if (! IN_CTYPE_DOMAIN (*p1)
	      || ! IN_CTYPE_DOMAIN (*p2)
	      || ! IN_CTYPE_DOMAIN (*p3))
	    break;
	  if ((ISDIGIT (*p1) || (*p1 == '_')) && (*p1 == *p2) && (*p1 == *p3))
	    continue;
	  if (! ISUPPER ((unsigned char)*p1) || ! ISLOWER ((unsigned char)*p2)
	      || (*p1 != toupper ((unsigned char)*p2))
	      || ((*p3 != *p1) && (*p3 != *p2)))
	    break;
	}
      assert ((*p1 == *p2) && (*p1 == *p3) && (*p1 == '\0'));
    }

  for (i = 0; ((size_t) i) < ARRAY_SIZE (ffeintrin_imps_); ++i)
    {
      const char *c = ffeintrin_imps_[i].control;

      if (c[0] == '\0')
	continue;

      if ((c[0] != '-')
	  && (c[0] != 'A')
	  && (c[0] != 'C')
	  && (c[0] != 'I')
	  && (c[0] != 'L')
	  && (c[0] != 'R')
	  && (c[0] != 'B')
	  && (c[0] != 'F')
	  && (c[0] != 'N')
	  && (c[0] != 'S'))
	{
	  fprintf (stderr, "%s: bad return-base-type\n",
		   ffeintrin_imps_[i].name);
	  continue;
	}
      if ((c[1] != '-')
	  && (c[1] != '=')
	  && ((c[1] < '1')
	      || (c[1] > '9'))
	  && (c[1] != 'C'))
	{
	  fprintf (stderr, "%s: bad return-kind-type\n",
		   ffeintrin_imps_[i].name);
	  continue;
	}
      if (c[2] == ':')
	colon = 2;
      else
	{
	  if (c[2] != '*')
	    {
	      fprintf (stderr, "%s: bad return-modifier\n",
		       ffeintrin_imps_[i].name);
	      continue;
	    }
	  colon = 3;
	}
      if ((c[colon] != ':') || (c[colon + 2] != ':'))
	{
	  fprintf (stderr, "%s: bad control\n",
		   ffeintrin_imps_[i].name);
	  continue;
	}
      if ((c[colon + 1] != '-')
	  && (c[colon + 1] != '*')
	  && ((c[colon + 1] < '0')
	      || (c[colon + 1] > '9')))
	{
	  fprintf (stderr, "%s: bad COL-spec\n",
		   ffeintrin_imps_[i].name);
	  continue;
	}
      c += (colon + 3);
      while (c[0] != '\0')
	{
	  while ((c[0] != '=')
		 && (c[0] != ',')
		 && (c[0] != '\0'))
	    ++c;
	  if (c[0] != '=')
	    {
	      fprintf (stderr, "%s: bad keyword\n",
		       ffeintrin_imps_[i].name);
	      break;
	    }
	  if ((c[1] == '?')
	      || (c[1] == '!')
	      || (c[1] == '+')
	      || (c[1] == '*')
	      || (c[1] == 'n')
	      || (c[1] == 'p'))
	    ++c;
	  if ((c[1] != '-')
	      && (c[1] != 'A')
	      && (c[1] != 'C')
	      && (c[1] != 'I')
	      && (c[1] != 'L')
	      && (c[1] != 'R')
	      && (c[1] != 'B')
	      && (c[1] != 'F')
	      && (c[1] != 'N')
	      && (c[1] != 'S')
	      && (c[1] != 'g')
	      && (c[1] != 's'))
	    {
	      fprintf (stderr, "%s: bad arg-base-type\n",
		       ffeintrin_imps_[i].name);
	      break;
	    }
	  if ((c[2] != '*')
	      && ((c[2] < '1')
		  || (c[2] > '9'))
	      && (c[2] != 'A'))
	    {
	      fprintf (stderr, "%s: bad arg-kind-type\n",
		       ffeintrin_imps_[i].name);
	      break;
	    }
	  if (c[3] == '[')
	    {
	      if (((c[4] < '0') || (c[4] > '9'))
		  || ((c[5] != ']')
		      && (++c, (c[4] < '0') || (c[4] > '9')
			  || (c[5] != ']'))))
		{
		  fprintf (stderr, "%s: bad arg-len\n",
			   ffeintrin_imps_[i].name);
		  break;
		}
	      c += 3;
	    }
	  if (c[3] == '(')
	    {
	      if (((c[4] < '0') || (c[4] > '9'))
		  || ((c[5] != ')')
		      && (++c, (c[4] < '0') || (c[4] > '9')
			  || (c[5] != ')'))))
		{
		  fprintf (stderr, "%s: bad arg-rank\n",
			   ffeintrin_imps_[i].name);
		  break;
		}
	      c += 3;
	    }
	  else if ((c[3] == '&')
		   && (c[4] == '&'))
	    ++c;
	  if ((c[3] == '&')
	      || (c[3] == 'i')
	      || (c[3] == 'w')
	      || (c[3] == 'x'))
	    ++c;
	  if (c[3] == ',')
	    {
	      c += 4;
	      continue;
	    }
	  if (c[3] != '\0')
	    {
	      fprintf (stderr, "%s: bad arg-list\n",
		       ffeintrin_imps_[i].name);
	    }
	  break;
	}
    }
}

/* Determine whether intrinsic is okay as an actual argument.  */

bool
ffeintrin_is_actualarg (ffeintrinSpec spec)
{
  ffeIntrinsicState state;

  if (spec >= FFEINTRIN_spec)
    return FALSE;

  state = ffeintrin_state_family (ffeintrin_specs_[spec].family);

  return (!ffe_is_pedantic () || ffeintrin_specs_[spec].is_actualarg)
#if FFECOM_targetCURRENT == FFECOM_targetGCC
    && (ffe_is_f2c ()
	? (ffeintrin_imps_[ffeintrin_specs_[spec].implementation].gfrt_f2c
	   != FFECOM_gfrt)
	: (ffeintrin_imps_[ffeintrin_specs_[spec].implementation].gfrt_gnu
	   != FFECOM_gfrt))
#endif
    && ((state == FFE_intrinsicstateENABLED)
	|| (state == FFE_intrinsicstateHIDDEN));
}

/* Determine if name is intrinsic, return info.

   const char *name;		// C-string name of possible intrinsic.
   ffelexToken t;		// NULL if no diagnostic to be given.
   bool explicit;		// TRUE if INTRINSIC name.
   ffeintrinGen gen;		// (TRUE only) Generic id of intrinsic.
   ffeintrinSpec spec;		// (TRUE only) Specific id of intrinsic.
   ffeintrinImp imp;		// (TRUE only) Implementation id of intrinsic.
   if (ffeintrin_is_intrinsic (name, t, explicit,
			       &gen, &spec, &imp))
				// is an intrinsic, use gen, spec, imp, and
				// kind accordingly.  */

bool
ffeintrin_is_intrinsic (const char *name, ffelexToken t, bool explicit,
			ffeintrinGen *xgen, ffeintrinSpec *xspec,
			ffeintrinImp *ximp)
{
  struct _ffeintrin_name_ *intrinsic;
  ffeintrinGen gen;
  ffeintrinSpec spec;
  ffeintrinImp imp;
  ffeIntrinsicState state;
  bool disabled = FALSE;
  bool unimpl = FALSE;

  intrinsic = bsearch (name, &ffeintrin_names_[0],
		       ARRAY_SIZE (ffeintrin_names_),
		       sizeof (struct _ffeintrin_name_),
		         (void *) ffeintrin_cmp_name_);

  if (intrinsic == NULL)
    return FALSE;

  gen = intrinsic->generic;
  spec = intrinsic->specific;
  imp = ffeintrin_specs_[spec].implementation;

  /* Generic is okay only if at least one of its specifics is okay.  */

  if (gen != FFEINTRIN_genNONE)
    {
      int i;
      ffeintrinSpec tspec;
      bool ok = FALSE;

      name = ffeintrin_gens_[gen].name;

      for (i = 0;
	   (((size_t) i) < ARRAY_SIZE (ffeintrin_gens_[gen].specs))
	   && ((tspec
		= ffeintrin_gens_[gen].specs[i]) != FFEINTRIN_specNONE);
	   ++i)
	{
	  state = ffeintrin_state_family (ffeintrin_specs_[tspec].family);

	  if (state == FFE_intrinsicstateDELETED)
	    continue;

	  if (state == FFE_intrinsicstateDISABLED)
	    {
	      disabled = TRUE;
	      continue;
	    }

	  if (ffeintrin_specs_[tspec].implementation == FFEINTRIN_impNONE)
	    {
	      unimpl = TRUE;
	      continue;
	    }

	  if ((state == FFE_intrinsicstateENABLED)
	      || (explicit
		  && (state == FFE_intrinsicstateHIDDEN)))
	    {
	      ok = TRUE;
	      break;
	    }
	}
      if (!ok)
	gen = FFEINTRIN_genNONE;
    }

  /* Specific is okay only if not: unimplemented, disabled, deleted, or
     hidden and not explicit.  */

  if (spec != FFEINTRIN_specNONE)
    {
      if (gen != FFEINTRIN_genNONE)
	name = ffeintrin_gens_[gen].name;
      else
	name = ffeintrin_specs_[spec].name;

      if (((state = ffeintrin_state_family (ffeintrin_specs_[spec].family))
	   == FFE_intrinsicstateDELETED)
	  || (!explicit
	      && (state == FFE_intrinsicstateHIDDEN)))
	spec = FFEINTRIN_specNONE;
      else if (state == FFE_intrinsicstateDISABLED)
	{
	  disabled = TRUE;
	  spec = FFEINTRIN_specNONE;
	}
      else if (imp == FFEINTRIN_impNONE)
	{
	  unimpl = TRUE;
	  spec = FFEINTRIN_specNONE;
	}
    }

  /* If neither is okay, not an intrinsic.  */

  if ((gen == FFEINTRIN_genNONE) && (spec == FFEINTRIN_specNONE))
    {
      /* Here is where we produce a diagnostic about a reference to a
	 disabled or unimplemented intrinsic, if the diagnostic is desired.  */

      if ((disabled || unimpl)
	  && (t != NULL))
	{
	  ffebad_start (disabled
			? FFEBAD_INTRINSIC_DISABLED
			: FFEBAD_INTRINSIC_UNIMPLW);
	  ffebad_here (0, ffelex_token_where_line (t), ffelex_token_where_column (t));
	  ffebad_string (name);
	  ffebad_finish ();
	}

      return FALSE;
    }

  /* Determine whether intrinsic is function or subroutine.  If no specific
     id, scan list of possible specifics for generic to get consensus.  If
     not unanimous, or clear from the context, return NONE.  */

  if (spec == FFEINTRIN_specNONE)
    {
      int i;
      ffeintrinSpec tspec;
      ffeintrinImp timp;
      bool at_least_one_ok = FALSE;

      for (i = 0;
	   (((size_t) i) < ARRAY_SIZE (ffeintrin_gens_[gen].specs))
	   && ((tspec
		= ffeintrin_gens_[gen].specs[i]) != FFEINTRIN_specNONE);
	   ++i)
	{
	  if (((state = ffeintrin_state_family (ffeintrin_specs_[tspec].family))
	       == FFE_intrinsicstateDELETED)
	      || (state == FFE_intrinsicstateDISABLED))
	    continue;

	  if ((timp = ffeintrin_specs_[tspec].implementation)
	      == FFEINTRIN_impNONE)
	    continue;

	  at_least_one_ok = TRUE;
	  break;
	}

      if (!at_least_one_ok)
	{
	  *xgen = FFEINTRIN_genNONE;
	  *xspec = FFEINTRIN_specNONE;
	  *ximp = FFEINTRIN_impNONE;
	  return FALSE;
	}
    }

  *xgen = gen;
  *xspec = spec;
  *ximp = imp;
  return TRUE;
}

/* Return TRUE if intrinsic is standard F77 (or, if -ff90, F90).  */

bool
ffeintrin_is_standard (ffeintrinGen gen, ffeintrinSpec spec)
{
  if (spec == FFEINTRIN_specNONE)
    {
      if (gen == FFEINTRIN_genNONE)
	return FALSE;

      spec = ffeintrin_gens_[gen].specs[0];
      if (spec == FFEINTRIN_specNONE)
	return FALSE;
    }

  if ((ffeintrin_specs_[spec].family == FFEINTRIN_familyF77)
      || (ffe_is_90 ()
	  && ((ffeintrin_specs_[spec].family == FFEINTRIN_familyF90)
	      || (ffeintrin_specs_[spec].family == FFEINTRIN_familyMIL)
	      || (ffeintrin_specs_[spec].family == FFEINTRIN_familyASC))))
    return TRUE;
  return FALSE;
}

/* Return kind type of intrinsic implementation.  See ffeintrin_basictype,
   its sibling.  */

ffeinfoKindtype
ffeintrin_kindtype (ffeintrinSpec spec)
{
  ffeintrinImp imp;
  ffecomGfrt gfrt;

  assert (spec < FFEINTRIN_spec);
  imp = ffeintrin_specs_[spec].implementation;
  assert (imp < FFEINTRIN_imp);

  if (ffe_is_f2c ())
    gfrt = ffeintrin_imps_[imp].gfrt_f2c;
  else
    gfrt = ffeintrin_imps_[imp].gfrt_gnu;

  assert (gfrt != FFECOM_gfrt);

  return ffecom_gfrt_kindtype (gfrt);
}

/* Return name of generic intrinsic.  */

const char *
ffeintrin_name_generic (ffeintrinGen gen)
{
  assert (gen < FFEINTRIN_gen);
  return ffeintrin_gens_[gen].name;
}

/* Return name of intrinsic implementation.  */

const char *
ffeintrin_name_implementation (ffeintrinImp imp)
{
  assert (imp < FFEINTRIN_imp);
  return ffeintrin_imps_[imp].name;
}

/* Return external/internal name of specific intrinsic.	 */

const char *
ffeintrin_name_specific (ffeintrinSpec spec)
{
  assert (spec < FFEINTRIN_spec);
  return ffeintrin_specs_[spec].name;
}

/* Return state of family.  */

ffeIntrinsicState
ffeintrin_state_family (ffeintrinFamily family)
{
  ffeIntrinsicState state;

  switch (family)
    {
    case FFEINTRIN_familyNONE:
      return FFE_intrinsicstateDELETED;

    case FFEINTRIN_familyF77:
      return FFE_intrinsicstateENABLED;

    case FFEINTRIN_familyASC:
      state = ffe_intrinsic_state_f2c ();
      state = ffe_state_max (state, ffe_intrinsic_state_f90 ());
      return state;

    case FFEINTRIN_familyMIL:
      state = ffe_intrinsic_state_vxt ();
      state = ffe_state_max (state, ffe_intrinsic_state_f90 ());
      state = ffe_state_max (state, ffe_intrinsic_state_mil ());
      return state;

    case FFEINTRIN_familyGNU:
      state = ffe_intrinsic_state_gnu ();
      return state;

    case FFEINTRIN_familyF90:
      state = ffe_intrinsic_state_f90 ();
      return state;

    case FFEINTRIN_familyVXT:
      state = ffe_intrinsic_state_vxt ();
      return state;

    case FFEINTRIN_familyFVZ:
      state = ffe_intrinsic_state_f2c ();
      state = ffe_state_max (state, ffe_intrinsic_state_vxt ());
      return state;

    case FFEINTRIN_familyF2C:
      state = ffe_intrinsic_state_f2c ();
      return state;

    case FFEINTRIN_familyF2U:
      state = ffe_intrinsic_state_unix ();
      return state;

    case FFEINTRIN_familyBADU77:
      state = ffe_intrinsic_state_badu77 ();
      return state;

    default:
      assert ("bad family" == NULL);
      return FFE_intrinsicstateDELETED;
    }
}
