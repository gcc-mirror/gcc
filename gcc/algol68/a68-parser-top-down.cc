/* Top-down parser for control structure.
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

/* A few forward prototypes of functions defined below.  */

static NODE_T *top_down_loop (NODE_T *p);
static NODE_T *top_down_skip_unit (NODE_T *p);
static NODE_T *top_down_def (NODE_T *def_p);

/* Substitute brackets.

   Traditional ALGOL 68 syntax allows ( .. ) to replace [ .. ] in bounds and
   slices.  This top-down pass substitutes [ .. ] occurrences into ( .. ).  */

void
a68_substitute_brackets (NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      a68_substitute_brackets (SUB (p));

      switch (ATTRIBUTE (p))
	{
	case SUB_SYMBOL:
	  ATTRIBUTE (p) = OPEN_SYMBOL;
	  break;
	case BUS_SYMBOL:
	  ATTRIBUTE (p) = CLOSE_SYMBOL;
	  break;
	default:
	  break;
	}
    }
}

/* Intelligible diagnostic from syntax tree branch.  */

const char *
a68_phrase_to_text (NODE_T * p, NODE_T ** w)
{
#define MAX_TERMINALS 8
  int count = 0, line = -1;
  static BUFFER buffer;

  for (buffer[0] = '\0'; p != NO_NODE && count < MAX_TERMINALS; FORWARD (p))
    {
      if (LINE_NUMBER (p) == 0)
	continue;

      enum a68_attribute gatt = a68_get_good_attribute (p);
      const char *z = a68_attribute_name (gatt);

      /* Where to put the error message? Bob Uzgalis noted that actual
	 content of a diagnostic is not as important as accurately
	 indicating *were* the problem is!  */
      if (w != NO_VAR)
	{
	  if (count == 0 || (*w) == NO_NODE)
	    *w = p;
	  else if (a68_dont_mark_here (*w))
	    *w = p;
	}

      /* Add initiation.  */
      if (count == 0)
	{
	  if (w != NO_VAR)
	    a68_bufcat (buffer, "construct beginning with", BUFFER_SIZE);
	}
      else if (count == 1)
	a68_bufcat (buffer, " followed by", BUFFER_SIZE);
      else if (count == 2)
	a68_bufcat (buffer, " and then", BUFFER_SIZE);
      else if (count >= 3)
	a68_bufcat (buffer, " and", BUFFER_SIZE);

      /* Attribute or symbol.  */
      if (z != NO_TEXT && SUB (p) != NO_NODE)
	{
	  if (gatt == IDENTIFIER || gatt == OPERATOR || gatt == DENOTATION)
	    {
	      const char *strop_symbol = a68_strop_keyword (NSYMBOL (p));
	      if (snprintf (A68 (edit_line), SNPRINTF_SIZE, " %%<%s%%>", strop_symbol) < 0)
		gcc_unreachable ();
	      a68_bufcat (buffer, A68 (edit_line), BUFFER_SIZE);
	    }
	  else
	    {
	      if (strchr ("aeio", z[0]) != NO_TEXT)
		a68_bufcat (buffer, " an", BUFFER_SIZE);
	      else
		a68_bufcat (buffer, " a", BUFFER_SIZE);

	      if (snprintf (A68 (edit_line), SNPRINTF_SIZE, " %s", z) < 0)
		gcc_unreachable ();
	      a68_bufcat (buffer, A68 (edit_line), BUFFER_SIZE);
	    }
	}
      else if (z != NO_TEXT && SUB (p) == NO_NODE)
	{
	  const char *strop_symbol = a68_strop_keyword (NSYMBOL (p));
	  if (snprintf (A68 (edit_line), SNPRINTF_SIZE, " %%<%s%%>", strop_symbol) < 0)
	    gcc_unreachable ();
	  a68_bufcat (buffer, A68 (edit_line), BUFFER_SIZE);
	}
      else if (NSYMBOL (p) != NO_TEXT)
	{
	  const char *strop_symbol = a68_strop_keyword (NSYMBOL (p));
	  if (snprintf (A68 (edit_line), SNPRINTF_SIZE, " %%<%s%%>", strop_symbol) < 0)
	    gcc_unreachable ();
	  a68_bufcat (buffer, A68 (edit_line), BUFFER_SIZE);
	}
      /* Add "starting in line nn".  */
      if (z != NO_TEXT && line != LINE_NUMBER (p))
	{
	  line = LINE_NUMBER (p);
	  if (gatt == SERIAL_CLAUSE || gatt == ENQUIRY_CLAUSE || gatt == INITIALISER_SERIES)
	    a68_bufcat (buffer, " starting", BUFFER_SIZE);
	  if (snprintf (A68 (edit_line), SNPRINTF_SIZE, " in line %d", line) < 0)
	    gcc_unreachable ();
	  a68_bufcat (buffer, A68 (edit_line), BUFFER_SIZE);
	}
      count++;
    }

  if (p != NO_NODE && count == MAX_TERMINALS)
    a68_bufcat (buffer, " etcetera", BUFFER_SIZE);
  return buffer;
}

/* Next is a top-down parser that branches out the basic blocks.
   After this we can assign symbol tables to basic blocks.
   This renders the two-level grammar LALR.  */

/* Give diagnose from top-down parser.  */

static void
top_down_diagnose (NODE_T *start, NODE_T *p, int clause, int expected)
{
  NODE_T *issue = (p != NO_NODE ? p : start);
  const char *strop_keyword = a68_strop_keyword (NSYMBOL (start));

  if (expected != 0)
    a68_error (issue, "B expected in A, near Z L",
	       expected, clause, strop_keyword, LINE (INFO (start)));
  else
    a68_error (issue, "missing or unbalanced keyword in A, near Z L",
	       clause, strop_keyword, LINE (INFO (start)));
}

/* Check for premature exhaustion of tokens.  */

static void
tokens_exhausted (NODE_T *p, NODE_T *q)
{
  if (p == NO_NODE)
    {
      a68_error (q, "check for missing or unmatched keyword in clause starting at S");
      longjmp (A68_PARSER (top_down_crash_exit), 1);
    }
}

/*
 * This part specifically branches out loop clauses.
 */

/* Whether in cast or formula with loop clause.  */

static int
is_loop_cast_formula (NODE_T *p)
{
  /* Accept declarers that can appear in such casts but not much more.  */
  if (IS (p, VOID_SYMBOL))
    return 1;
  else if (IS (p, INT_SYMBOL))
    return 1;
  else if (IS_REF (p))
    return 1;
  else if (a68_is_one_of (p, OPERATOR, BOLD_TAG, STOP))
    return 1;
  else if (a68_whether (p, UNION_SYMBOL, OPEN_SYMBOL, STOP))
    return 2;
  else if (a68_is_one_of (p, OPEN_SYMBOL, SUB_SYMBOL, STOP))
    {
      int k = 0;
      for (; p != NO_NODE && (a68_is_one_of (p, OPEN_SYMBOL, SUB_SYMBOL, STOP)); FORWARD (p), k++)
	;
      return p != NO_NODE && (a68_whether (p, UNION_SYMBOL, OPEN_SYMBOL, STOP) ? k : 0);
    }
  return 0;
}

/* Skip a unit in a loop clause (FROM u BY u TO u).  */

static NODE_T *
top_down_skip_loop_unit (NODE_T *p)
{
  /* Unit may start with, or consist of, a loop.  */
  if (a68_is_loop_keyword (p))
    p = top_down_loop (p);

  /* Skip rest of unit.  */
  while (p != NO_NODE)
    {
      int k = is_loop_cast_formula (p);

      if (k != 0)
	{
	  /* operator-cast series ...  */
	  while (p != NO_NODE && k != 0)
	    {
	      while (k != 0)
		{
		  FORWARD (p);
		  k--;
		}
	      k = is_loop_cast_formula (p);
	    }

	  /* ... may be followed by a loop clause.  */
	  if (a68_is_loop_keyword (p))
	    p = top_down_loop (p);
	}
      else if (a68_is_loop_keyword (p) || IS (p, OD_SYMBOL))
	/* new loop or end-of-loop.  */
	return p;
      else if (IS (p, COLON_SYMBOL))
	{
	  FORWARD (p);
	  /* skip routine header: loop clause.  */
	  if (p != NO_NODE && a68_is_loop_keyword (p))
	    p = top_down_loop (p);
	}
      else if (a68_is_one_of (p, SEMI_SYMBOL, COMMA_SYMBOL, STOP) || IS (p, EXIT_SYMBOL))
	/* Statement separators.  */
	return p;
      else
	FORWARD (p);
    }
  return NO_NODE;
}

/* Skip a loop clause.  */

static NODE_T *
top_down_skip_loop_series (NODE_T *p)
{
  bool siga;

  do
    {
      p = top_down_skip_loop_unit (p);
      siga = (p != NO_NODE && (a68_is_one_of (p, SEMI_SYMBOL, EXIT_SYMBOL,
					      COMMA_SYMBOL, COLON_SYMBOL,
					      STOP)));
    if (siga)
      FORWARD (p);
    }
  while (!(p == NO_NODE || !siga));

  return p;
}

/* Make branch of loop parts.  */

static NODE_T *
top_down_loop (NODE_T *p)
{
  NODE_T *start = p, *q = p;

  if (IS (q, FOR_SYMBOL))
    {
      tokens_exhausted (FORWARD (q), start);

      if (IS (q, IDENTIFIER))
	ATTRIBUTE (q) = DEFINING_IDENTIFIER;
      else
	{
	  top_down_diagnose (start, q, LOOP_CLAUSE, IDENTIFIER);
	  longjmp (A68_PARSER (top_down_crash_exit), 1);
	}

      tokens_exhausted (FORWARD (q), start);

      if (a68_is_one_of (q, FROM_SYMBOL, BY_SYMBOL, TO_SYMBOL,
			 WHILE_SYMBOL, STOP))
	;
      else if (IS (q, DO_SYMBOL))
	ATTRIBUTE (q) = ALT_DO_SYMBOL;
      else
	{
	  top_down_diagnose (start, q, LOOP_CLAUSE, STOP);
	  longjmp (A68_PARSER (top_down_crash_exit), 1);
	}
    }

  if (IS (q, FROM_SYMBOL))
    {
      start = q;
      q = top_down_skip_loop_unit (NEXT (q));
      tokens_exhausted (q, start);
      if (a68_is_one_of (q, BY_SYMBOL, TO_SYMBOL, WHILE_SYMBOL, STOP))
	;
      else if (IS (q, DO_SYMBOL))
	ATTRIBUTE (q) = ALT_DO_SYMBOL;
      else
	{
	  top_down_diagnose (start, q, LOOP_CLAUSE, STOP);
	  longjmp (A68_PARSER (top_down_crash_exit), 1);
	}

      a68_make_sub (start, PREVIOUS (q), FROM_SYMBOL);
    }

  if (IS (q, BY_SYMBOL))
    {
      start = q;
      q = top_down_skip_loop_series (NEXT (q));
      tokens_exhausted (q, start);

      if (a68_is_one_of (q, TO_SYMBOL, WHILE_SYMBOL, STOP))
	;
      else if (IS (q, DO_SYMBOL))
	ATTRIBUTE (q) = ALT_DO_SYMBOL;
      else
	{
	  top_down_diagnose (start, q, LOOP_CLAUSE, STOP);
	  longjmp (A68_PARSER (top_down_crash_exit), 1);
	}

      a68_make_sub (start, PREVIOUS (q), BY_SYMBOL);
    }

  if (a68_is_one_of (q, TO_SYMBOL, STOP))
    {
      start = q;
      q = top_down_skip_loop_series (NEXT (q));
      tokens_exhausted (q, start);

      if (IS (q, WHILE_SYMBOL))
	;
      else if (IS (q, DO_SYMBOL))
	ATTRIBUTE (q) = ALT_DO_SYMBOL;
      else
	{
	  top_down_diagnose (start, q, LOOP_CLAUSE, STOP);
	  longjmp (A68_PARSER (top_down_crash_exit), 1);
	}

      a68_make_sub (start, PREVIOUS (q), TO_SYMBOL);
    }

  if (IS (q, WHILE_SYMBOL))
    {
      start = q;
      q = top_down_skip_loop_series (NEXT (q));
      tokens_exhausted (q, start);

      if (IS (q, DO_SYMBOL))
	ATTRIBUTE (q) = ALT_DO_SYMBOL;
      else
	{
	  top_down_diagnose (start, q, LOOP_CLAUSE, DO_SYMBOL);
	  longjmp (A68_PARSER (top_down_crash_exit), 1);
	}

      a68_make_sub (start, PREVIOUS (q), WHILE_SYMBOL);
    }

  if (a68_is_one_of (q, DO_SYMBOL, ALT_DO_SYMBOL, STOP))
    {
      enum a68_attribute k = ATTRIBUTE (q);

      start = q;
      q = top_down_skip_loop_series (NEXT (q));
      tokens_exhausted (q, start);

      if (!IS (q, OD_SYMBOL))
	{
	  top_down_diagnose (start, q, LOOP_CLAUSE, OD_SYMBOL);
	  longjmp (A68_PARSER (top_down_crash_exit), 1);
	}

      a68_make_sub (start, q, k);
    }

  NODE_T *save = NEXT (start);
  a68_make_sub (p, start, LOOP_CLAUSE);
  return save;
}

/* Driver for making branches of loop parts.  */

static void
top_down_loops (NODE_T *p)
{
  NODE_T *q = p;

  for (; q != NO_NODE; FORWARD (q))
    {
      if (SUB (q) != NO_NODE)
	top_down_loops (SUB (q));
    }

  q = p;
  while (q != NO_NODE)
    {
      if (a68_is_loop_keyword (q) != STOP)
	q = top_down_loop (q);
      else
	FORWARD (q);
    }
}

/*
 * Branch anything except parts of a loop.
 */

/* Skip serial/enquiry clause (unit series).  */

static NODE_T *
top_down_series (NODE_T *p)
{
  bool siga = true;
  while (siga)
    {
      siga = false;
      p = top_down_skip_unit (p);
      if (p != NO_NODE)
	{
	  if (a68_is_one_of (p, SEMI_SYMBOL, EXIT_SYMBOL, COMMA_SYMBOL, STOP))
	    {
	      siga = true;
	      FORWARD (p);
	    }
	}
    }
  return p;
}

/* Make branch of DEF .. POSTLUDE .. FED.  */

static NODE_T *
top_down_def (NODE_T *def_p)
{
  NODE_T *fed_p = top_down_series (NEXT (def_p));

  if (fed_p == NO_NODE || !(IS (fed_p, FED_SYMBOL) || IS (fed_p, POSTLUDE_SYMBOL)))
    {
      top_down_diagnose (def_p, fed_p, MODULE_TEXT, FED_SYMBOL);
      longjmp (A68_PARSER (top_down_crash_exit), 1);
    }

  a68_make_sub (def_p, PREVIOUS (fed_p), DEF_SYMBOL);

  if (IS (fed_p, POSTLUDE_SYMBOL))
    {
      NODE_T *postlude_p = top_down_series (NEXT (fed_p));

      if (postlude_p == NO_NODE || !IS (postlude_p, FED_SYMBOL))
	{
	  top_down_diagnose (def_p, fed_p, MODULE_TEXT, FED_SYMBOL);
	  longjmp (A68_PARSER (top_down_crash_exit), 1);
	}

      a68_make_sub (fed_p, PREVIOUS (postlude_p), POSTLUDE_SYMBOL);
    }

  return NEXT (def_p);
}

/* Make branch of

   ACCESS REVELATIONS [DEF_SYMBOL]
   or
   ACCESS REVELATIONS ENCLOSED_CLAUSE.   */

static void
top_down_access (NODE_T *p)
{
  for (NODE_T *q = p; q != NO_NODE; FORWARD (q))
    {
      if (SUB (q) != NO_NODE)
	top_down_access (SUB (q));
    }

  for (NODE_T *q = p; q != NO_NODE; FORWARD (q))
    {
      if (IS (q, ACCESS_SYMBOL))
	{
	  NODE_T *end_p = NEXT (q);

	  /* Skip joined list of revelations and make branch until the enclosed
	     clause, DEF_SYMBOL or POSTLUDE_SYMBOL.  */
	  while (a68_is_one_of (end_p, BOLD_TAG, PUBLIC_SYMBOL, COMMA_SYMBOL, STOP))
	    FORWARD (end_p);

	  if (IS (end_p, DEF_SYMBOL))
	    {
	      FORWARD (end_p);
	      if (IS (end_p, POSTLUDE_SYMBOL))
		FORWARD (end_p);
	      if (IS (end_p, FED_SYMBOL))
		{
		  ATTRIBUTE (q) = ALT_ACCESS_SYMBOL;
		  a68_make_sub (q, end_p, ALT_ACCESS_SYMBOL);
		}
	    }
	  else if (IS (end_p, ACCESS_SYMBOL))
	    {
	      top_down_access (end_p);
	      a68_make_sub (q, end_p, ACCESS_SYMBOL);
	    }
	  else
	    a68_make_sub (q, end_p, ACCESS_SYMBOL);
	}
    }
}

/* Make branch of BEGIN .. END.  */

static NODE_T *
top_down_begin (NODE_T *begin_p)
{
  NODE_T *end_p = top_down_series (NEXT (begin_p));

  if (end_p == NO_NODE || !IS (end_p, END_SYMBOL))
    {
      top_down_diagnose (begin_p, end_p, ENCLOSED_CLAUSE, END_SYMBOL);
      longjmp (A68_PARSER (top_down_crash_exit), 1);
      return NO_NODE;
    }
  else
    {
      a68_make_sub (begin_p, end_p, BEGIN_SYMBOL);
      return NEXT (begin_p);
    }
}

/* Make branch of ( .. ).  */

static NODE_T *
top_down_open (NODE_T *open_p)
{
  NODE_T *then_bar_p = top_down_series (NEXT (open_p)), *elif_bar_p;

  if (then_bar_p != NO_NODE && IS (then_bar_p, CLOSE_SYMBOL))
    {
      a68_make_sub (open_p, then_bar_p, OPEN_SYMBOL);
      return NEXT (open_p);
    }

  if (then_bar_p == NO_NODE || !IS (then_bar_p, THEN_BAR_SYMBOL))
    {
      top_down_diagnose (open_p, then_bar_p, ENCLOSED_CLAUSE, STOP);
      longjmp (A68_PARSER (top_down_crash_exit), 1);
    }

  a68_make_sub (open_p, PREVIOUS (then_bar_p), OPEN_SYMBOL);
  elif_bar_p = top_down_series (NEXT (then_bar_p));
  if (elif_bar_p != NO_NODE && IS (elif_bar_p, CLOSE_SYMBOL))
    {
      a68_make_sub (then_bar_p, PREVIOUS (elif_bar_p), THEN_BAR_SYMBOL);
      a68_make_sub (open_p, elif_bar_p, OPEN_SYMBOL);
      return NEXT (open_p);
    }

  if (elif_bar_p != NO_NODE && IS (elif_bar_p, THEN_BAR_SYMBOL))
    {
      NODE_T *close_p = top_down_series (NEXT (elif_bar_p));

      if (close_p == NO_NODE || !IS (close_p, CLOSE_SYMBOL))
	{
	  top_down_diagnose (open_p, elif_bar_p, ENCLOSED_CLAUSE, CLOSE_SYMBOL);
	  longjmp (A68_PARSER (top_down_crash_exit), 1);
	}

      a68_make_sub (then_bar_p, PREVIOUS (elif_bar_p), THEN_BAR_SYMBOL);
      a68_make_sub (elif_bar_p, PREVIOUS (close_p), THEN_BAR_SYMBOL);
      a68_make_sub (open_p, close_p, OPEN_SYMBOL);
      return NEXT (open_p);
    }

  if (elif_bar_p != NO_NODE && IS (elif_bar_p, ELSE_BAR_SYMBOL))
    {
      NODE_T *close_p = top_down_open (elif_bar_p);
      a68_make_sub (then_bar_p, PREVIOUS (elif_bar_p), THEN_BAR_SYMBOL);
      a68_make_sub (open_p, elif_bar_p, OPEN_SYMBOL);
      return close_p;
    }
  else
    {
      top_down_diagnose (open_p, elif_bar_p, ENCLOSED_CLAUSE, CLOSE_SYMBOL);
      longjmp (A68_PARSER (top_down_crash_exit), 1);
      return NO_NODE;
    }
}

/* Make branch of [ .. ].  */

static NODE_T *
top_down_sub (NODE_T *sub_p)
{
  NODE_T *bus_p = top_down_series (NEXT (sub_p));

  if (bus_p != NO_NODE && IS (bus_p, BUS_SYMBOL))
    {
      a68_make_sub (sub_p, bus_p, SUB_SYMBOL);
      return NEXT (sub_p);
    }
  else
    {
      top_down_diagnose (sub_p, bus_p, 0, BUS_SYMBOL);
      longjmp (A68_PARSER (top_down_crash_exit), 1);
      return NO_NODE;
    }
}

/* Make branch of IF .. THEN .. ELSE .. FI.  */

static NODE_T *
top_down_if (NODE_T * if_p)
{
  NODE_T *then_p = top_down_series (NEXT (if_p)), *elif_p;

  if (then_p == NO_NODE || !IS (then_p, THEN_SYMBOL))
    {
      top_down_diagnose (if_p, then_p, CONDITIONAL_CLAUSE, THEN_SYMBOL);
      longjmp (A68_PARSER (top_down_crash_exit), 1);
    }

  a68_make_sub (if_p, PREVIOUS (then_p), IF_SYMBOL);

  elif_p = top_down_series (NEXT (then_p));
  if (elif_p != NO_NODE && IS (elif_p, FI_SYMBOL))
    {
      a68_make_sub (then_p, PREVIOUS (elif_p), THEN_SYMBOL);
      a68_make_sub (if_p, elif_p, IF_SYMBOL);
      return NEXT (if_p);
    }

  if (elif_p != NO_NODE && IS (elif_p, ELSE_SYMBOL))
    {
      NODE_T *fi_p = top_down_series (NEXT (elif_p));

      if (fi_p == NO_NODE || !IS (fi_p, FI_SYMBOL))
	{
	  top_down_diagnose (if_p, fi_p, CONDITIONAL_CLAUSE, FI_SYMBOL);
	  longjmp (A68_PARSER (top_down_crash_exit), 1);
	}
      else
	{
	  a68_make_sub (then_p, PREVIOUS (elif_p), THEN_SYMBOL);
	  a68_make_sub (elif_p, PREVIOUS (fi_p), ELSE_SYMBOL);
	  a68_make_sub (if_p, fi_p, IF_SYMBOL);
	  return NEXT (if_p);
	}
    }

  if (elif_p != NO_NODE && IS (elif_p, ELIF_SYMBOL))
    {
      NODE_T *fi_p = top_down_if (elif_p);

      a68_make_sub (then_p, PREVIOUS (elif_p), THEN_SYMBOL);
      a68_make_sub (if_p, elif_p, IF_SYMBOL);
      return fi_p;
    }
  else
    {
      top_down_diagnose (if_p, elif_p, CONDITIONAL_CLAUSE, FI_SYMBOL);
      longjmp (A68_PARSER (top_down_crash_exit), 1);
      return NO_NODE;
    }
}

/* Make branch of CASE .. IN .. OUT .. ESAC.  */

static NODE_T *
top_down_case (NODE_T *case_p)
{
  NODE_T *in_p = top_down_series (NEXT (case_p)), *ouse_p;

  if (in_p == NO_NODE || !IS (in_p, IN_SYMBOL))
    {
      top_down_diagnose (case_p, in_p, ENCLOSED_CLAUSE, IN_SYMBOL);
      longjmp (A68_PARSER (top_down_crash_exit), 1);
    }

  a68_make_sub (case_p, PREVIOUS (in_p), CASE_SYMBOL);

  ouse_p = top_down_series (NEXT (in_p));
  if (ouse_p != NO_NODE && IS (ouse_p, ESAC_SYMBOL))
    {
      a68_make_sub (in_p, PREVIOUS (ouse_p), IN_SYMBOL);
      a68_make_sub (case_p, ouse_p, CASE_SYMBOL);
      return NEXT (case_p);
    }

  if (ouse_p != NO_NODE && IS (ouse_p, OUT_SYMBOL))
    {
      NODE_T *esac_p = top_down_series (NEXT (ouse_p));

      if (esac_p == NO_NODE || !IS (esac_p, ESAC_SYMBOL))
	{
	  top_down_diagnose (case_p, esac_p, ENCLOSED_CLAUSE, ESAC_SYMBOL);
	  longjmp (A68_PARSER (top_down_crash_exit), 1);
	}
      else
	{
	  a68_make_sub (in_p, PREVIOUS (ouse_p), IN_SYMBOL);
	  a68_make_sub (ouse_p, PREVIOUS (esac_p), OUT_SYMBOL);
	  a68_make_sub (case_p, esac_p, CASE_SYMBOL);
	  return NEXT (case_p);
	}
    }

  if (ouse_p != NO_NODE && IS (ouse_p, OUSE_SYMBOL))
    {
      NODE_T *esac_p = top_down_case (ouse_p);

      a68_make_sub (in_p, PREVIOUS (ouse_p), IN_SYMBOL);
      a68_make_sub (case_p, ouse_p, CASE_SYMBOL);
      return esac_p;
    }
  else
    {
      top_down_diagnose (case_p, ouse_p, ENCLOSED_CLAUSE, ESAC_SYMBOL);
      longjmp (A68_PARSER (top_down_crash_exit), 1);
      return NO_NODE;
    }
}

/* Skip a unit.  */

static NODE_T *
top_down_skip_unit (NODE_T *p)
{
  while (p != NO_NODE && !a68_is_unit_terminator (p))
    {
      if (IS (p, BEGIN_SYMBOL))
	p = top_down_begin (p);
      else if (IS (p, SUB_SYMBOL))
	p = top_down_sub (p);
      else if (IS (p, OPEN_SYMBOL))
	p = top_down_open (p);
      else if (IS (p, IF_SYMBOL))
	p = top_down_if (p);
      else if (IS (p, CASE_SYMBOL))
	p = top_down_case (p);
      else
	FORWARD (p);
    }
  return p;
}

static NODE_T *top_down_skip_format (NODE_T *);

/* Make branch of ( .. ) in a format.  */

static NODE_T *
top_down_format_open (NODE_T *open_p)
{
  NODE_T *close_p = top_down_skip_format (NEXT (open_p));

  if (close_p != NO_NODE && IS (close_p, FORMAT_CLOSE_SYMBOL))
    {
      a68_make_sub (open_p, close_p, FORMAT_OPEN_SYMBOL);
      return NEXT (open_p);
    }
  else
    {
      top_down_diagnose (open_p, close_p, 0, FORMAT_CLOSE_SYMBOL);
      longjmp (A68_PARSER (top_down_crash_exit), 1);
      return NO_NODE;
    }
}

/* Skip a format text.  */

static NODE_T *
top_down_skip_format (NODE_T *p)
{
  while (p != NO_NODE)
    {
      if (IS (p, FORMAT_OPEN_SYMBOL))
	p = top_down_format_open (p);
      else if (a68_is_one_of (p, FORMAT_CLOSE_SYMBOL, FORMAT_DELIMITER_SYMBOL, STOP))
	return p;
      else
	FORWARD (p);
  }
  return NO_NODE;
}

/* Make branch of $ .. $.  */

static void
top_down_formats (NODE_T * p)
{
  for (NODE_T *q = p; q != NO_NODE; FORWARD (q))
    {
      if (SUB (q) != NO_NODE)
	top_down_formats (SUB (q));
    }

  for (NODE_T *q = p; q != NO_NODE; FORWARD (q))
    {
      if (IS (q, FORMAT_DELIMITER_SYMBOL))
	{
	  NODE_T *f = NEXT (q);

	  while (f != NO_NODE && !IS (f, FORMAT_DELIMITER_SYMBOL))
	    {
	      if (IS (f, FORMAT_OPEN_SYMBOL))
		f = top_down_format_open (f);
	      else
		f = NEXT (f);
	    }

	  if (f == NO_NODE)
	    {
	      top_down_diagnose (p, f, FORMAT_TEXT, FORMAT_DELIMITER_SYMBOL);
	      longjmp (A68_PARSER (top_down_crash_exit), 1);
	    }
	  else
	    a68_make_sub (q, f, FORMAT_DELIMITER_SYMBOL);
	}
    }
}

/* Skip prelude packet.  */

static NODE_T *
top_down_prelude_packet (NODE_T *p)
{
  while (p != NO_NODE)
    {
      if (IS (p, DEF_SYMBOL))
	p = top_down_def (p);
      else
	FORWARD (p);
    }

  return p;
}

/* Skip particular program.  */

static NODE_T *
top_down_particular_program (NODE_T *p)
{
  (void) top_down_series (p);
  return p;
}

/* Make branches of phrases for the bottom-up parser.  */

void
a68_top_down_parser (NODE_T *p)
{
  if (p == NO_NODE)
    return;

  if (!setjmp (A68_PARSER (top_down_crash_exit)))
    {
      if (IS (p, MODULE_SYMBOL))
	(void) top_down_prelude_packet (p);
      else
	(void) top_down_particular_program (p);

      top_down_loops (p);
      top_down_formats (p);
      top_down_access (p);
    }
}
