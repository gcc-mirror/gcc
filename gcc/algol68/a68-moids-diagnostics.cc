/* MOID diagnostics routines.
   Copyright (C) 2001-2023 J. Marcel van der Veer.
   Copyright (C) 2025 Jose E. Marchesi.

   Original implementation by J. Marcel van der Veer.
   Adapted for GCC by Jose E. Marchesi.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "options.h"

#include "a68.h"

/* Give accurate error message.  */

const char *
a68_mode_error_text (NODE_T *n, MOID_T *p, MOID_T *q, int context, int deflex, int depth)
{
#define TAIL(z) (&(z)[strlen (z)])
#define ACTUAL_SNPRINTF_SIZE ((SNPRINTF_SIZE - len))
  static BUFFER txt;
  size_t len;
  if (depth == 1)
    txt[0] = '\0';
  if (IS (p, SERIES_MODE))
    {
      len = strlen (txt);
      PACK_T *u = PACK (p);

      int N = 0;
      if (u == NO_PACK)
	{
	  if (snprintf (txt, ACTUAL_SNPRINTF_SIZE, "empty mode-list") < 0)
	    gcc_unreachable ();
	  N++;
	}
      else
	{
	  for (; u != NO_PACK; FORWARD (u))
	    {
	      if (MOID (u) != NO_MOID)
		{
		  if (IS (MOID (u), SERIES_MODE))
		    (void) a68_mode_error_text (n, MOID (u), q, context, deflex, depth + 1);
		  else if (!a68_is_coercible (MOID (u), q, context, deflex))
		    {
		      len = strlen (txt);
		      if (len > BUFFER_SIZE / 2)
			{
			  if (snprintf (TAIL (txt), ACTUAL_SNPRINTF_SIZE, " etcetera") < 0)
			    gcc_unreachable ();
			  N++;
			}
		      else
			{
			  if (len > 0)
			    {
			      if (snprintf (TAIL (txt), ACTUAL_SNPRINTF_SIZE, " and ") < 0)
				gcc_unreachable ();
			      N++;
			      len = strlen (txt);
			    }
			  if (snprintf (TAIL (txt), ACTUAL_SNPRINTF_SIZE, "%%<%s%%>",
					a68_moid_to_string (MOID (u), MOID_ERROR_WIDTH, n)) < 0)
			    gcc_unreachable ();
			  N++;
			}
		    }
		}
	    }
	}
      if (depth == 1)
	{
	  len = strlen (txt);
	  if (N == 0)
	    {
	      if (snprintf (TAIL (txt), ACTUAL_SNPRINTF_SIZE, "mode") < 0)
		gcc_unreachable ();
	      len = strlen (txt);
	    }
	  if (snprintf (TAIL (txt), ACTUAL_SNPRINTF_SIZE, " cannot be coerced to %%<%s%%>",
			a68_moid_to_string (q, MOID_ERROR_WIDTH, n)) < 0)
	    gcc_unreachable ();
	}
    }
  else if (IS (p, STOWED_MODE) && IS_FLEX (q))
    {
      PACK_T *u = PACK (p);
      len = strlen (txt);
      if (u == NO_PACK)
	{
	  if (snprintf (txt, ACTUAL_SNPRINTF_SIZE, "empty mode-list") < 0)
	    gcc_unreachable ();
	}
      else
	{
	  for (; u != NO_PACK; FORWARD (u))
	    {
	      if (!a68_is_coercible (MOID (u), SLICE (SUB (q)), context, deflex))
		{
		  len = strlen (txt);
		  if (len > BUFFER_SIZE / 2)
		    {
		      if (snprintf (TAIL (txt), ACTUAL_SNPRINTF_SIZE, " etcetera") < 0)
			gcc_unreachable ();
		    }
		  else
		    {
		      if (len > 0)
			{
			  if (snprintf (TAIL (txt), ACTUAL_SNPRINTF_SIZE, " and ") < 0)
			    gcc_unreachable ();
			  len = strlen (txt);
			}
		      if (snprintf (TAIL (txt), ACTUAL_SNPRINTF_SIZE, "%s",
				    a68_moid_to_string (MOID (u), MOID_ERROR_WIDTH, n)) < 0)
			gcc_unreachable ();
		    }
		}
	    }
	  len = strlen (txt);
	  if (snprintf (TAIL (txt), ACTUAL_SNPRINTF_SIZE, " cannot be coerced to %%<%s%%>",
			a68_moid_to_string (SLICE (SUB (q)), MOID_ERROR_WIDTH, n)) < 0)
	    gcc_unreachable ();
	}
    }
  else if (IS (p, STOWED_MODE) && IS (q, ROW_SYMBOL))
    {
      PACK_T *u = PACK (p);
      len = strlen (txt);
      if (u == NO_PACK)
	{
	  if (snprintf (txt, ACTUAL_SNPRINTF_SIZE, "empty mode-list") < 0)
	    gcc_unreachable ();
	}
      else
	{
	  for (; u != NO_PACK; FORWARD (u))
	    {
	      if (!a68_is_coercible (MOID (u), SLICE (q), context, deflex))
		{
		  len = strlen (txt);
		  if (len > BUFFER_SIZE / 2)
		    {
		      if (snprintf (TAIL (txt), ACTUAL_SNPRINTF_SIZE, " etcetera") < 0)
			gcc_unreachable ();
		    }
		  else
		    {
		      if (len > 0)
			{
			  if (snprintf (TAIL (txt), ACTUAL_SNPRINTF_SIZE, " and ") < 0)
			    gcc_unreachable ();
			  len = strlen (txt);
			}
		      if (snprintf (TAIL (txt), ACTUAL_SNPRINTF_SIZE, "%%<%s%%>",
				    a68_moid_to_string (MOID (u), MOID_ERROR_WIDTH, n)) < 0)
			gcc_unreachable ();
		    }
		}
	    }
	  len = strlen (txt);
	  if (snprintf (TAIL (txt), ACTUAL_SNPRINTF_SIZE, " cannot be coerced to %s",
			a68_moid_to_string (SLICE (q), MOID_ERROR_WIDTH, n)) < 0)
	    gcc_unreachable ();
	}
    }
  else if (IS (p, STOWED_MODE) && (IS (q, PROC_SYMBOL) || IS (q, STRUCT_SYMBOL)))
    {
      PACK_T *u = PACK (p), *v = PACK (q);
      len = strlen (txt);
      if (u == NO_PACK)
	{
	  if (snprintf (txt, ACTUAL_SNPRINTF_SIZE, "empty mode-list") < 0)
	    gcc_unreachable ();
	}
      else
	{
	  for (; u != NO_PACK && v != NO_PACK; FORWARD (u), FORWARD (v))
	    {
	      if (!a68_is_coercible (MOID (u), MOID (v), context, deflex))
		{
		  len = strlen (txt);
		  if (len > BUFFER_SIZE / 2)
		    {
		      if (snprintf (TAIL (txt), ACTUAL_SNPRINTF_SIZE, " etcetera") < 0)
			gcc_unreachable ();
		    }
		  else
		    {
		      if (len > 0)
			{
			  if (snprintf (TAIL (txt), ACTUAL_SNPRINTF_SIZE, " and ") < 0)
			    gcc_unreachable ();
			  len = strlen (txt);
			}
		      if (snprintf (TAIL (txt), ACTUAL_SNPRINTF_SIZE, "%%<%s%%> cannot be coerced to %%<%s%%>",
				    a68_moid_to_string (MOID (u), MOID_ERROR_WIDTH, n),
				    a68_moid_to_string (MOID (v), MOID_ERROR_WIDTH, n)) < 0)
			gcc_unreachable ();
		    }
		}
	    }
	}
    }
  return txt;
#undef TAIL
#undef ACTUAL_SNPRINTF_SIZE
}

/* Cannot coerce error.  */

void
a68_cannot_coerce (NODE_T *p, MOID_T *from, MOID_T *to, int context, int deflex, int att)
{
  const char *txt = a68_mode_error_text (p, from, to, context, deflex, 1);

  if (att == STOP)
    {
      if (strlen (txt) == 0)
	a68_error (p, "M cannot be coerced to M in C context", from, to, context);
      else
	a68_error (p, "Y in C context", txt, context);
    }
  else
    {
      if (strlen (txt) == 0)
	a68_error (p, "M cannot be coerced to M in C-A", from, to, context, att);
      else
	a68_error (p, "Y in C-A", txt, context, att);
    }
}

/* Give a warning when a value is silently discarded.  */

void
a68_warn_for_voiding (NODE_T *p, SOID_T *x, SOID_T *y, int c)
{
  (void) c;

  if (CAST (x) == false)
    {
      if (MOID (x) == M_VOID && MOID (y) != M_ERROR && !(MOID (y) == M_VOID || !a68_is_nonproc (MOID (y))))
	{
	  if (IS (p, FORMULA))
	    a68_warning (p, OPT_Wvoiding, "value of M @ will be voided", MOID (y));
	  else
	    a68_warning (p, OPT_Wvoiding, "value of M @ will be voided", MOID (y));
	}
    }
}

/* Warn for things that are likely unintended.  */

void
a68_semantic_pitfall (NODE_T *p, MOID_T *m, int c, int u)
{
  /* semantic_pitfall: warn for things that are likely unintended, for instance
                       REF INT i := LOC INT := 0, which should probably be
                       REF INT i = LOC INT := 0.  */
  if (IS (p, u))
    a68_warning (p, 0, "possibly unintended M A in M A",
		 MOID (p), u, m, c);
  else if (a68_is_one_of (p, UNIT, TERTIARY, SECONDARY, PRIMARY, STOP))
    a68_semantic_pitfall (SUB (p), m, c, u);
}
