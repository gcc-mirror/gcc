/* Recursive-descent parenthesis checker.
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

#include "a68.h"

/* After this checker, we know that at least brackets are matched.  This
   stabilises later parser phases.

   Note that this checker operates on a linear list of nodes.

   Error diagnostics are placed near offending lines.  */

/* Intelligible diagnostics for the bracket checker.  */

static void
bracket_check_error (char *txt, int n, const char *bra, const char *ket)
{
  BUFFER buf;

  if (n == 0)
    return;

  const char *strop_ket = a68_strop_keyword (ket);
  const char *strop_bra = a68_strop_keyword (bra);

  BUFCLR (buf);
  if (snprintf (buf, SNPRINTF_SIZE, "missing matching %%<%s%%>",
		(n > 0 ? strop_ket : strop_bra)) < 0)
    gcc_unreachable ();

  if (strlen (txt) > 0)
    a68_bufcat (txt, " or ", BUFFER_SIZE);
  a68_bufcat (txt, buf, BUFFER_SIZE);
}

/* Diagnose brackets in local branch of the tree.  */

static char *
bracket_check_diagnose (NODE_T *p)
{
  int begins = 0, opens = 0, format_delims = 0, format_opens = 0;
  int subs = 0, ifs = 0, cases = 0, dos = 0;

  for (; p != NO_NODE; FORWARD (p))
    {
      switch (ATTRIBUTE (p))
	{
	case BEGIN_SYMBOL:
	  begins++;
	  break;
	case END_SYMBOL:
	  begins--;
	  break;
	case OPEN_SYMBOL:
	  opens++;
	  break;
	case CLOSE_SYMBOL:
	  opens--;
	  break;
	case FORMAT_DELIMITER_SYMBOL:
	  if (format_delims == 0)
	    format_delims = 1;
	  else
	    format_delims = 0;
	  break;
	case FORMAT_OPEN_SYMBOL:
	  format_opens++;
	  break;
	case FORMAT_CLOSE_SYMBOL:
	  format_opens--;
	  break;
	case SUB_SYMBOL:
	  subs++;
	  break;
	case BUS_SYMBOL:
	  subs--;
	  break;
	case IF_SYMBOL:
	  ifs++;
	  break;
	case FI_SYMBOL:
	  ifs--;
	  break;
	case CASE_SYMBOL:
	  cases++;
	  break;
	case ESAC_SYMBOL:
	  cases--;
	  break;
	case DO_SYMBOL:
	  dos++;
	  break;
	case OD_SYMBOL:
	  dos--;
	  break;
	default:
	  break;
	}
    }

  A68 (edit_line)[0] = '\0';
  bracket_check_error (A68 (edit_line), begins, "BEGIN", "END");
  bracket_check_error (A68 (edit_line), opens, "(", ")");
  bracket_check_error (A68 (edit_line), format_opens, "(", ")");
  bracket_check_error (A68 (edit_line), format_delims, "$", "$");
  bracket_check_error (A68 (edit_line), subs, "[", "]");
  bracket_check_error (A68 (edit_line), ifs, "IF", "FI");
  bracket_check_error (A68 (edit_line), cases, "CASE", "ESAC");
  bracket_check_error (A68 (edit_line), dos, "DO", "OD");
  return A68 (edit_line);
}

/* Driver for locally diagnosing non-matching tokens.  */

static NODE_T *
bracket_check_parse (NODE_T *top, NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      int ket = STOP;
      NODE_T *q = NO_NODE;
      bool ignore_token = false;

      switch (ATTRIBUTE (p))
	{
	case BEGIN_SYMBOL:
	  ket = END_SYMBOL;
	  q = bracket_check_parse (top, NEXT (p));
	  break;
	case OPEN_SYMBOL:
	  ket = CLOSE_SYMBOL;
	  q = bracket_check_parse (top, NEXT (p));
	  break;
	case FORMAT_OPEN_SYMBOL:
	  ket = FORMAT_CLOSE_SYMBOL;
	  q = bracket_check_parse (top, NEXT (p));
	  break;
	case SUB_SYMBOL:
	  ket = BUS_SYMBOL;
	  q = bracket_check_parse (top, NEXT (p));
	  break;
	case IF_SYMBOL:
	  ket = FI_SYMBOL;
	  q = bracket_check_parse (top, NEXT (p));
	  break;
	case CASE_SYMBOL:
	  ket = ESAC_SYMBOL;
	  q = bracket_check_parse (top, NEXT (p));
	  break;
	case DO_SYMBOL:
	  ket = OD_SYMBOL;
	  q = bracket_check_parse (top, NEXT (p));
	  break;
	case END_SYMBOL:
	case CLOSE_SYMBOL:
	case FORMAT_CLOSE_SYMBOL:
	case BUS_SYMBOL:
	case FI_SYMBOL:
	case ESAC_SYMBOL:
	case OD_SYMBOL:
	  return p;
	default:
	  ignore_token = true;
	}

      if (ignore_token)
	;
      else if (q != NO_NODE && IS (q, ket))
	p = q;
      else if (q == NO_NODE)
	{
	  char *diag = bracket_check_diagnose (top);
	  a68_error (p, "incorrect nesting, check for Y",
		     (strlen (diag) > 0 ? diag : "missing or unmatched keyword"));
	  longjmp (A68_PARSER (top_down_crash_exit), 1);
	}
      else
	{
	  char *diag = bracket_check_diagnose (top);
	  a68_error (q, "unexpected X, check for Y",
		     ATTRIBUTE (q),
		     (strlen (diag) > 0 ? diag : "missing or unmatched keyword"));
	  longjmp (A68_PARSER (top_down_crash_exit), 1);
	}
    }
  return NO_NODE;
}

/* Driver for globally diagnosing non-matching tokens.  */

void
a68_check_parenthesis (NODE_T *top)
{
  if (!setjmp (A68_PARSER (top_down_crash_exit)))
    {
      if (bracket_check_parse (top, top) != NO_NODE)
	a68_error (top, "incorrect nesting, check for Y",
		   "missing or unmatched keyword");
    }
}
