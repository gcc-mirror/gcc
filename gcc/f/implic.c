/* implic.c -- Implementation File (module.c template V1.0)
   Copyright (C) 1995, 2002, 2003 Free Software Foundation, Inc.
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
      The GNU Fortran Front End.

   Modifications:
*/

/* Include files. */

#include "proj.h"
#include "implic.h"
#include "info.h"
#include "src.h"
#include "symbol.h"
#include "target.h"

/* Externals defined here. */


/* Simple definitions and enumerations. */

typedef enum
  {
    FFEIMPLIC_stateINITIAL_,
    FFEIMPLIC_stateASSUMED_,
    FFEIMPLIC_stateESTABLISHED_,
    FFEIMPLIC_state
  } ffeimplicState_;

/* Internal typedefs. */

typedef struct _ffeimplic_ *ffeimplic_;

/* Private include files. */


/* Internal structure definitions. */

struct _ffeimplic_
  {
    ffeimplicState_ state;
    ffeinfo info;
  };

/* Static objects accessed by functions in this module. */

/* NOTE: This is definitely ASCII-specific!!  */

static struct _ffeimplic_ ffeimplic_table_['z' - 'A' + 1];

/* Static functions (internal). */

static ffeimplic_ ffeimplic_lookup_ (unsigned char c);

/* Internal macros. */


/* ffeimplic_lookup_ -- Look up implicit descriptor for initial character

   ffeimplic_ imp;
   if ((imp = ffeimplic_lookup_('A')) == NULL)
       // error

   Returns a pointer to an implicit descriptor block based on the character
   passed, or NULL if it is not a valid initial character for an implicit
   data type.  */

static ffeimplic_
ffeimplic_lookup_ (unsigned char c)
{
  /* NOTE: This is definitely ASCII-specific!!  */
  if (ISIDST (c))
    return &ffeimplic_table_[c - 'A'];
  return NULL;
}

/* ffeimplic_establish_initial -- Establish type of implicit initial letter

   ffesymbol s;
   if (!ffeimplic_establish_initial(s))
       // error

   Assigns implicit type information to the symbol based on the first
   character of the symbol's name.  */

bool
ffeimplic_establish_initial (char c, ffeinfoBasictype basic_type,
		     ffeinfoKindtype kind_type, ffetargetCharacterSize size)
{
  ffeimplic_ imp;

  imp = ffeimplic_lookup_ (c);
  if (imp == NULL)
    return FALSE;		/* Character not A-Z or some such thing. */
  if (ffeinfo_basictype (imp->info) == FFEINFO_basictypeNONE)
    return FALSE;		/* IMPLICIT NONE in effect here. */

  switch (imp->state)
    {
    case FFEIMPLIC_stateINITIAL_:
      imp->info = ffeinfo_new (basic_type,
			       kind_type,
			       0,
			       FFEINFO_kindNONE,
			       FFEINFO_whereNONE,
			       size);
      imp->state = FFEIMPLIC_stateESTABLISHED_;
      return TRUE;

    case FFEIMPLIC_stateASSUMED_:
      if ((ffeinfo_basictype (imp->info) != basic_type)
	  || (ffeinfo_kindtype (imp->info) != kind_type)
	  || (ffeinfo_size (imp->info) != size))
	return FALSE;
      imp->state = FFEIMPLIC_stateESTABLISHED_;
      return TRUE;

    case FFEIMPLIC_stateESTABLISHED_:
      return FALSE;

    default:
      assert ("Weird state for implicit object" == NULL);
      return FALSE;
    }
}

/* ffeimplic_establish_symbol -- Establish implicit type of a symbol

   ffesymbol s;
   if (!ffeimplic_establish_symbol(s))
       // error

   Assigns implicit type information to the symbol based on the first
   character of the symbol's name.

   If symbol already has a type, return TRUE.
   Get first character of symbol's name.
   Get ffeimplic_ object for it (return FALSE if NULL returned).
   Return FALSE if object has no assigned type (IMPLICIT NONE).
   Copy the type information from the object to the symbol.
   If the object is state "INITIAL", set to state "ASSUMED" so no
       subsequent IMPLICIT statement may change the state.
   Return TRUE.	 */

bool
ffeimplic_establish_symbol (ffesymbol s)
{
  char c;
  ffeimplic_ imp;

  if (ffesymbol_basictype (s) != FFEINFO_basictypeNONE)
    return TRUE;

  c = *(ffesymbol_text (s));
  imp = ffeimplic_lookup_ (c);
  if (imp == NULL)
    return FALSE;		/* First character not A-Z or some such
				   thing. */
  if (ffeinfo_basictype (imp->info) == FFEINFO_basictypeNONE)
    return FALSE;		/* IMPLICIT NONE in effect here. */

  ffesymbol_signal_change (s);	/* Gonna change, save existing? */

  /* Establish basictype, kindtype, size; preserve rank, kind, where. */

  ffesymbol_set_info (s,
		      ffeinfo_new (ffeinfo_basictype (imp->info),
				   ffeinfo_kindtype (imp->info),
				   ffesymbol_rank (s),
				   ffesymbol_kind (s),
				   ffesymbol_where (s),
				   ffeinfo_size (imp->info)));

  if (imp->state == FFEIMPLIC_stateINITIAL_)
    imp->state = FFEIMPLIC_stateASSUMED_;

  if (ffe_is_warn_implicit ())
    {
      /* xgettext:no-c-format */
      ffebad_start_msg ("Implicit declaration of `%A' at %0",
			FFEBAD_severityWARNING);
      ffebad_here (0, ffesymbol_where_line (s),
		   ffesymbol_where_column (s));
      ffebad_string (ffesymbol_text (s));
      ffebad_finish ();
    }

  return TRUE;
}

/* ffeimplic_init_2 -- Initialize table

   ffeimplic_init_2();

   Assigns initial type information to all initial letters.

   Allows for holes in the sequence of letters (i.e. EBCDIC).  */

void
ffeimplic_init_2 (void)
{
  ffeimplic_ imp;
  char c;

  for (c = 'A'; c <= 'z'; ++c)
    {
      imp = &ffeimplic_table_[c - 'A'];
      imp->state = FFEIMPLIC_stateINITIAL_;
      switch (c)
	{
	case 'A':
	case 'B':
	case 'C':
	case 'D':
	case 'E':
	case 'F':
	case 'G':
	case 'H':
	case 'O':
	case 'P':
	case 'Q':
	case 'R':
	case 'S':
	case 'T':
	case 'U':
	case 'V':
	case 'W':
	case 'X':
	case 'Y':
	case 'Z':
	case '_':
	case 'a':
	case 'b':
	case 'c':
	case 'd':
	case 'e':
	case 'f':
	case 'g':
	case 'h':
	case 'o':
	case 'p':
	case 'q':
	case 'r':
	case 's':
	case 't':
	case 'u':
	case 'v':
	case 'w':
	case 'x':
	case 'y':
	case 'z':
	  imp->info = ffeinfo_new (FFEINFO_basictypeREAL,
				   FFEINFO_kindtypeREALDEFAULT,
				   0,
				   FFEINFO_kindNONE,
				   FFEINFO_whereNONE,
				   FFETARGET_charactersizeNONE);
	  break;

	case 'I':
	case 'J':
	case 'K':
	case 'L':
	case 'M':
	case 'N':
	case 'i':
	case 'j':
	case 'k':
	case 'l':
	case 'm':
	case 'n':
	  imp->info = ffeinfo_new (FFEINFO_basictypeINTEGER,
				   FFEINFO_kindtypeINTEGERDEFAULT, 0, FFEINFO_kindNONE, FFEINFO_whereNONE,
				   FFETARGET_charactersizeNONE);
	  break;

	default:
	  imp->info = ffeinfo_new (FFEINFO_basictypeNONE, FFEINFO_kindtypeNONE, 0,
	  FFEINFO_kindNONE, FFEINFO_whereNONE, FFETARGET_charactersizeNONE);
	  break;
	}
    }
}

/* ffeimplic_none -- Implement IMPLICIT NONE statement

   ffeimplic_none();

   Assigns null type information to all initial letters.  */

void
ffeimplic_none (void)
{
  ffeimplic_ imp;

  for (imp = &ffeimplic_table_[0];
       imp != &ffeimplic_table_[ARRAY_SIZE (ffeimplic_table_)];
       imp++)
    {
      imp->info = ffeinfo_new (FFEINFO_basictypeNONE,
			       FFEINFO_kindtypeNONE,
			       0,
			       FFEINFO_kindNONE,
			       FFEINFO_whereNONE,
			       FFETARGET_charactersizeNONE);
    }
}

/* ffeimplic_peek_symbol_type -- Determine implicit type of a symbol

   ffesymbol s;
   const char *name; // name for s in case it is NULL, or NULL if s never NULL
   if (ffeimplic_peek_symbol_type(s,name) == FFEINFO_basictypeCHARACTER)
       // is or will be a CHARACTER-typed name

   Like establish_symbol, but doesn't change anything.

   If symbol is non-NULL and already has a type, return it.
   Get first character of symbol's name or from name arg if symbol is NULL.
   Get ffeimplic_ object for it (return FALSE if NULL returned).
   Return NONE if object has no assigned type (IMPLICIT NONE).
   Return the data type indicated in the object.

   24-Oct-91  JCB  2.0
      Take a char * instead of ffelexToken, since the latter isn't always
      needed anyway (as when ffecom calls it).	*/

ffeinfoBasictype
ffeimplic_peek_symbol_type (ffesymbol s, const char *name)
{
  char c;
  ffeimplic_ imp;

  if (s == NULL)
    c = *name;
  else
    {
      if (ffesymbol_basictype (s) != FFEINFO_basictypeNONE)
	return ffesymbol_basictype (s);

      c = *(ffesymbol_text (s));
    }

  imp = ffeimplic_lookup_ (c);
  if (imp == NULL)
    return FFEINFO_basictypeNONE;	/* First character not A-Z or
					   something. */
  return ffeinfo_basictype (imp->info);
}

/* ffeimplic_terminate_2 -- Terminate table

   ffeimplic_terminate_2();

   Kills info object for each entry in table.  */

void
ffeimplic_terminate_2 (void)
{
}
