/* Mode checker routines.
   Copyright (C) 2001-2023 J. Marcel van der Veer.
   Copyright (C) 2025 Jose E. Marchesi.

   Original implementation by J. Marcel van der Veer.
   Adapted for GCC and fixes by Jose E. Marchesi.

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

/* ALGOL 68 contexts are SOFT, WEAK, MEEK, FIRM and STRONG.
   These contexts are increasing in strength:

   SOFT: Deproceduring

   WEAK: Dereferencing to REF [] or REF STRUCT

   MEEK: Deproceduring and dereferencing

   FIRM: MEEK followed by uniting

   STRONG: FIRM followed by rowing, widening or voiding

   Furthermore you will see in this file next switches:

   (1) FORCE_DEFLEXING allows assignment compatibility between FLEX and non FLEX
   rows. This can only be the case when there is no danger of altering bounds of a
   non FLEX row.

   (2) ALIAS_DEFLEXING prohibits aliasing a FLEX row to a non FLEX row (vice versa
   is no problem) so that one cannot alter the bounds of a non FLEX row by
   aliasing it to a FLEX row. This is particularly the case when passing names as
   parameters to procedures:

      PROC x = (REF STRING s) VOID: ..., PROC y = (REF [] CHAR c) VOID: ...;

      x (LOC STRING);    # OK #

      x (LOC [10] CHAR); # Not OK, suppose x changes bounds of s! #

      y (LOC STRING);    # OK #

      y (LOC [10] CHAR); # OK #

   (3) SAFE_DEFLEXING sets FLEX row apart from non FLEX row. This holds for names,
   not for values, so common things are not rejected, for instance

      STRING x = read string;

      [] CHAR y = read string

   (4) NO_DEFLEXING sets FLEX row apart from non FLEX row.  */

/*
  In the RR grammar:

     SORT: strong; firm; weak; meek; soft.
     SORT MOID serial clause;
       strong void unit, go on token, SORT MOID serial clause;
       declaration, go on token, SORT MOID serial clause;
       SORT MOID unit

  And it is the SORT MOID sequence of metanotions, which shall evaluate the
  same in the complete rule, that control the balancing! o_O

  Also, it denotes how the SORT MOID of the serial clause gets "passed" to the
  last unit in the serial clause.  Other units have SOID `strong void'.

  It is used to pass down the required mode on whatever context.  Like,
  PARTICULAR_PROGRAM evaluates in strong context and requires VOID.

  The ATTRIBUTE in the soid is used to pass down the kind of construct that
  introduces the context+required mode.  This is used in
  a68_determine_unique_mode in order to know whether balancing shall be
  performed or not.
*/

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "options.h"

#include "a68.h"

/* Forward declarations of some of the functions defined below.  */

static void mode_check_enclosed (NODE_T *p, SOID_T *x, SOID_T *y);
static void mode_check_unit (NODE_T *p, SOID_T *x, SOID_T *y);
static void mode_check_formula (NODE_T *p, SOID_T *x, SOID_T *y);
static void mode_check_module_declaration (NODE_T *p);
static void mode_check_module_text (NODE_T *p);
static void mode_check_module_declaration (NODE_T *p);

/* Driver for mode checker.  */

void
a68_mode_checker (NODE_T *p)
{
  if (IS (p, PACKET))
    {
      p = SUB (p);

      if (IS (p, PARTICULAR_PROGRAM))
	{
	  A68 (top_soid_list) = NO_SOID;
	  SOID_T x, y;
	  a68_make_soid (&x, STRONG, M_VOID, 0);
	  mode_check_enclosed (SUB (p), &x, &y);
	  MOID (p) = MOID (&y);
	}
      else if (IS (p, PRELUDE_PACKET))
	mode_check_module_declaration (SUB (p));
    }
}

/* Mode check on bounds.  */

static void
mode_check_bounds (NODE_T *p)
{
  if (p == NO_NODE)
    return;
  else if (IS (p, UNIT))
    {
      SOID_T x, y;
      a68_make_soid (&x, STRONG, M_INT, 0);
      mode_check_unit (p, &x, &y);
      if (!a68_is_coercible_in_context (&y, &x, SAFE_DEFLEXING))
	a68_cannot_coerce (p, MOID (&y), M_INT, MEEK, SAFE_DEFLEXING, UNIT);
      mode_check_bounds (NEXT (p));
    }
  else
    {
      mode_check_bounds (SUB (p));
      mode_check_bounds (NEXT (p));
    }
}

/* Mode check declarer.  */

static void
mode_check_declarer (NODE_T *p)
{
  if (p == NO_NODE)
    return;
  else if (IS (p, BOUNDS))
    {
      mode_check_bounds (SUB (p));
      mode_check_declarer (NEXT (p));
    }
  else
    {
      mode_check_declarer (SUB (p));
      mode_check_declarer (NEXT (p));
    }
}

/* Mode check identity declaration.  */

static void
mode_check_identity_declaration (NODE_T *p)
{
  if (p != NO_NODE)
    {
      switch (ATTRIBUTE (p))
	{
	case DECLARER:
	  mode_check_declarer (SUB (p));
	  mode_check_identity_declaration (NEXT (p));
	  break;
	case DEFINING_IDENTIFIER:
	  {
	    SOID_T x, y;
	    a68_make_soid (&x, STRONG, MOID (p), 0);
	    mode_check_unit (NEXT_NEXT (p), &x, &y);
	    if (!a68_is_coercible_in_context (&y, &x, SAFE_DEFLEXING))
	      a68_cannot_coerce (NEXT_NEXT (p), MOID (&y), MOID (&x), STRONG, SAFE_DEFLEXING, UNIT);
	    else if (MOID (&x) != MOID (&y))
	      /* Check for instance, REF INT i = LOC REF INT.  */
	      a68_semantic_pitfall (NEXT_NEXT (p), MOID (&x), IDENTITY_DECLARATION, GENERATOR);
	    break;
	  }
	default:
	  mode_check_identity_declaration (SUB (p));
	  mode_check_identity_declaration (NEXT (p));
	  break;
	}
    }
}

/* Mode check variable declaration.  */

static void
mode_check_variable_declaration (NODE_T *p)
{
  if (p != NO_NODE)
    {
      switch (ATTRIBUTE (p))
	{
	case DECLARER:
	  mode_check_declarer (SUB (p));
	  mode_check_variable_declaration (NEXT (p));
	  break;
	case DEFINING_IDENTIFIER:
	  if (a68_whether (p, DEFINING_IDENTIFIER, ASSIGN_SYMBOL, UNIT, STOP))
	    {
	      SOID_T x, y;
	      a68_make_soid (&x, STRONG, SUB_MOID (p), 0);
	      mode_check_unit (NEXT_NEXT (p), &x, &y);
	      if (!a68_is_coercible_in_context (&y, &x, FORCE_DEFLEXING))
		a68_cannot_coerce (p, MOID (&y), MOID (&x), STRONG, FORCE_DEFLEXING, UNIT);
	      else if (SUB_MOID (&x) != MOID (&y))
		/* Check for instance, REF INT i = LOC REF INT.  */
		a68_semantic_pitfall (NEXT_NEXT (p), MOID (&x), VARIABLE_DECLARATION, GENERATOR);
	    }
	  break;
	default:
	  mode_check_variable_declaration (SUB (p));
	  mode_check_variable_declaration (NEXT (p));
	  break;
	}
    }
}

/* Mode check routine text.  */

static void
mode_check_routine_text (NODE_T *p, SOID_T *y)
{
  SOID_T w;

  if (IS (p, PARAMETER_PACK))
    {
      mode_check_declarer (SUB (p));
      FORWARD (p);
    }

  mode_check_declarer (SUB (p));
  a68_make_soid (&w, STRONG, MOID (p), 0);
  mode_check_unit (NEXT_NEXT (p), &w, y);
  if (!a68_is_coercible_in_context (y, &w, FORCE_DEFLEXING))
    a68_cannot_coerce (NEXT_NEXT (p), MOID (y), MOID (&w), STRONG, FORCE_DEFLEXING, UNIT);
}

/* Mode check proc declaration.  */

static void
mode_check_proc_declaration (NODE_T *p)
{
  if (p == NO_NODE)
    return;
  else if (IS (p, ROUTINE_TEXT))
    {
      SOID_T x, y;
      a68_make_soid (&x, STRONG, NO_MOID, 0);
      mode_check_routine_text (SUB (p), &y);
    }
  else
    {
      mode_check_proc_declaration (SUB (p));
      mode_check_proc_declaration (NEXT (p));
    }
}

/* Mode check brief op declaration.  */

static void
mode_check_brief_op_declaration (NODE_T *p)
{
  if (p == NO_NODE)
    return;
  else if (IS (p, DEFINING_OPERATOR))
    {
      SOID_T y;

      if (MOID (p) != MOID (NEXT_NEXT (p)))
	{
	  SOID_T y2, x;
	  a68_make_soid (&y2, NO_SORT, MOID (NEXT_NEXT (p)), 0);
	  a68_make_soid (&x, NO_SORT, MOID (p), 0);
	  a68_cannot_coerce (NEXT_NEXT (p), MOID (&y2), MOID (&x), STRONG, SKIP_DEFLEXING, ROUTINE_TEXT);
	}
      mode_check_routine_text (SUB (NEXT_NEXT (p)), &y);
    }
  else
    {
      mode_check_brief_op_declaration (SUB (p));
      mode_check_brief_op_declaration (NEXT (p));
    }
}

/* Mode check op declaration.  */

static void
mode_check_op_declaration (NODE_T *p)
{
  if (p == NO_NODE)
    return;
  else if (IS (p, DEFINING_OPERATOR))
    {
      SOID_T y, x;
      a68_make_soid (&x, STRONG, MOID (p), 0);
      mode_check_unit (NEXT_NEXT (p), &x, &y);
      if (!a68_is_coercible_in_context (&y, &x, SAFE_DEFLEXING))
	a68_cannot_coerce (NEXT_NEXT (p), MOID (&y), MOID (&x), STRONG, SAFE_DEFLEXING, UNIT);
    }
  else
    {
      mode_check_op_declaration (SUB (p));
      mode_check_op_declaration (NEXT (p));
    }
}

/* Mode check declaration list.  */

static void
mode_check_declaration_list (NODE_T * p)
{
  if (p != NO_NODE)
    {
      switch (ATTRIBUTE (p))
	{
	case IDENTITY_DECLARATION:
	  mode_check_identity_declaration (SUB (p));
	  break;
	case VARIABLE_DECLARATION:
	  mode_check_variable_declaration (SUB (p));
	  break;
	case MODE_DECLARATION:
	  mode_check_declarer (SUB (p));
	  break;
	case PROCEDURE_DECLARATION:
	case PROCEDURE_VARIABLE_DECLARATION:
	  mode_check_proc_declaration (SUB (p));
	  break;
	case BRIEF_OPERATOR_DECLARATION:
	  mode_check_brief_op_declaration (SUB (p));
	  break;
	case OPERATOR_DECLARATION:
	  mode_check_op_declaration (SUB (p));
	  break;
	default:
	  mode_check_declaration_list (SUB (p));
	  mode_check_declaration_list (NEXT (p));
	  break;
	}
    }
}

/* Mode check serial clause.  */

static void
mode_check_serial (SOID_T **r, NODE_T *p, SOID_T *x, bool k)
{
  if (p == NO_NODE)
    return;
  else if (IS (p, INITIALISER_SERIES))
    {
      mode_check_serial (r, SUB (p), x, false);
      mode_check_serial (r, NEXT (p), x, k);
    }
  else if (IS (p, DECLARATION_LIST))
    mode_check_declaration_list (SUB (p));
  else if (a68_is_one_of (p, LABEL, SEMI_SYMBOL, EXIT_SYMBOL, STOP))
    mode_check_serial (r, NEXT (p), x, k);
  else if (a68_is_one_of (p, SERIAL_CLAUSE, ENQUIRY_CLAUSE, STOP))
    {
      if (NEXT (p) != NO_NODE)
	{
	  if (IS (NEXT (p), EXIT_SYMBOL) || IS (NEXT (p), END_SYMBOL) || IS (NEXT (p), CLOSE_SYMBOL))
	    mode_check_serial (r, SUB (p), x, true);
	  else
	    mode_check_serial (r, SUB (p), x, false);
	  mode_check_serial (r, NEXT (p), x, k);
	}
      else
	mode_check_serial (r, SUB (p), x, true);
    }
  else if (IS (p, LABELED_UNIT))
    mode_check_serial (r, SUB (p), x, k);
  else if (IS (p, UNIT))
    {
      SOID_T y;

      if (k)
	mode_check_unit (p, x, &y);
      else
	{
	  SOID_T w;
	  a68_make_soid (&w, STRONG, M_VOID, 0);
	  mode_check_unit (p, &w, &y);
	}
      if (NEXT (p) != NO_NODE)
	mode_check_serial (r, NEXT (p), x, k);
      else
	{
	  if (k)
	    a68_add_to_soid_list (r, p, &y);
	}
    }
}

/* Mode check serial clause units.  */

static void
mode_check_serial_units (NODE_T *p, SOID_T *x, SOID_T *y,
			 int att __attribute__((unused)))
{
  SOID_T *top_sl = NO_SOID;

  mode_check_serial (&top_sl, SUB (p), x, true);
  if (a68_is_balanced (p, top_sl, SORT (x)))
    {
      MOID_T *result = a68_pack_soids_in_moid (top_sl, SERIES_MODE);
      a68_make_soid (y, SORT (x), result, SERIAL_CLAUSE);
    }
  else
    a68_make_soid (y, SORT (x), (MOID (x) != NO_MOID ? MOID (x) : M_ERROR), 0);

  a68_free_soid_list (top_sl);
}

/* Mode check unit list.  */

static void
mode_check_unit_list (SOID_T **r, NODE_T *p, SOID_T *x)
{
  if (p == NO_NODE)
    return;
  else if (IS (p, UNIT_LIST))
    {
      mode_check_unit_list (r, SUB (p), x);
      mode_check_unit_list (r, NEXT (p), x);
    }
  else if (IS (p, COMMA_SYMBOL))
    mode_check_unit_list (r, NEXT (p), x);
  else if (IS (p, UNIT))
    {
      SOID_T y;
      mode_check_unit (p, x, &y);
      a68_add_to_soid_list (r, p, &y);
      mode_check_unit_list (r, NEXT (p), x);
    }
}

/* Mode check struct display.  */

static void
mode_check_struct_display (SOID_T **r, NODE_T *p, PACK_T **fields)
{
  if (p == NO_NODE)
    return;
  else if (IS (p, UNIT_LIST))
    {
      mode_check_struct_display (r, SUB (p), fields);
      mode_check_struct_display (r, NEXT (p), fields);
    }
  else if (IS (p, COMMA_SYMBOL))
    mode_check_struct_display (r, NEXT (p), fields);
  else if (IS (p, UNIT))
    {
      SOID_T x, y;

      if (*fields != NO_PACK)
	{
	  a68_make_soid (&x, STRONG, MOID (*fields), 0);
	  FORWARD (*fields);
	}
      else
	a68_make_soid (&x, STRONG, NO_MOID, 0);
      mode_check_unit (p, &x, &y);
      a68_add_to_soid_list (r, p, &y);
      mode_check_struct_display (r, NEXT (p), fields);
    }
}

/* Mode check get specified moids.  */

static void
mode_check_get_specified_moids (NODE_T *p, MOID_T *u)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (a68_is_one_of (p, SPECIFIED_UNIT_LIST, SPECIFIED_UNIT, STOP))
	mode_check_get_specified_moids (SUB (p), u);
      else if (IS (p, SPECIFIER))
	{
	  MOID_T *m = MOID (NEXT_SUB (p));
	  a68_add_mode_to_pack (&(PACK (u)), m, NO_TEXT, NODE (m));
	}
    }
}

/* Mode check specified unit list.  */

void
mode_check_specified_unit_list (SOID_T **r, NODE_T *p, SOID_T *x, MOID_T *u)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (a68_is_one_of (p, SPECIFIED_UNIT_LIST, SPECIFIED_UNIT, STOP))
	mode_check_specified_unit_list (r, SUB (p), x, u);
      else if (IS (p, SPECIFIER))
	{
	  MOID_T *m = MOID (NEXT_SUB (p));
	  if (u != NO_MOID && !a68_is_unitable (m, u, SAFE_DEFLEXING))
	    a68_error (p, "M is neither component nor subset of M", m, u);

	}
      else if (IS (p, UNIT))
	{
	  SOID_T y;
	  mode_check_unit (p, x, &y);
	  a68_add_to_soid_list (r, p, &y);
	}
    }
}

/* Mode check united case parts.  */

static void
mode_check_united_case_parts (SOID_T **ry, NODE_T *p, SOID_T *x)
{
  SOID_T enq_expct, enq_yield;
  MOID_T *u = NO_MOID, *v = NO_MOID, *w = NO_MOID;
  /* Check the CASE part and deduce the united mode.  */
  a68_make_soid (&enq_expct, MEEK, NO_MOID, 0);
  mode_check_serial_units (NEXT_SUB (p), &enq_expct, &enq_yield, ENQUIRY_CLAUSE);
  /* Deduce the united mode from the enquiry clause.
     This requires balancing.  */
  u = MOID (&enq_yield);
  a68_absorb_series_pack (&u);
  DIM (u) = a68_count_pack_members (PACK (u));
  if (DIM (u) == 1)
    u = MOID (PACK (u));
  else
    {
      MOID_T *united, *balanced;
      united = a68_make_united_mode (u);
      balanced = a68_get_balanced_mode_or_no_mode (united,
						   STRONG, A68_NO_DEPREF,
						   SAFE_DEFLEXING);
      if (balanced != NO_MOID)
	u = balanced;
    }
  u = a68_depref_completely (u);
  /* Also deduce the united mode from the specifiers.  */
  v = a68_new_moid ();
  ATTRIBUTE (v) = SERIES_MODE;
  mode_check_get_specified_moids (NEXT_SUB (NEXT (p)), v);
  v = a68_make_united_mode (v);
  /* Determine a resulting union.  */
  if (u == M_HIP)
    w = v;
  else
    {
      if (IS (u, UNION_SYMBOL))
	{
	  bool uv, vu, some;
	  a68_investigate_firm_relations (PACK (u), PACK (v), &uv, &some);
	  a68_investigate_firm_relations (PACK (v), PACK (u), &vu, &some);
	  if (uv && vu)
	    {
	      /* Every component has a specifier.  */
	      w = u;
	    }
	  else if (!uv && !vu)
	    {
	      /* Hmmmm ... let the coercer sort it out.  */
	      w = u;
	    }
	  else
	    {
	      /* This is all the balancing we allow here for the moment. Firmly
		 related subsets are not valid so we absorb them. If this
		 doesn't solve it then we get a coercion-error later. */
	      w = a68_absorb_related_subsets (u);
	    }
	}
      else
	{
	  a68_error (NEXT_SUB (p), "M is not a united mode", u);
	  return;
	}
    }
  MOID (SUB (p)) = w;
  FORWARD (p);
  /* Check the IN part.  */
  mode_check_specified_unit_list (ry, NEXT_SUB (p), x, w);
  /* OUSE, OUT, ESAC.  */
  if ((FORWARD (p)) != NO_NODE)
    {
      if (a68_is_one_of (p, OUT_PART, CHOICE, STOP))
	mode_check_serial (ry, NEXT_SUB (p), x, true);
      else if (a68_is_one_of (p, CONFORMITY_OUSE_PART, BRIEF_CONFORMITY_OUSE_PART, STOP))
	mode_check_united_case_parts (ry, SUB (p), x);
    }
}

/* Mode check united case.  */

static void
mode_check_united_case (NODE_T *p, SOID_T *x, SOID_T *y)
{
  SOID_T *top_sl = NO_SOID;

  mode_check_united_case_parts (&top_sl, p, x);
  if (!a68_is_balanced (p, top_sl, SORT (x)))
    {
      if (MOID (x) != NO_MOID)
	a68_make_soid (y, SORT (x), MOID (x), CONFORMITY_CLAUSE);
      else
	a68_make_soid (y, SORT (x), M_ERROR, 0);
    }
  else
    {
      MOID_T *z = a68_pack_soids_in_moid (top_sl, SERIES_MODE);
      a68_make_soid (y, SORT (x), z, CONFORMITY_CLAUSE);
    }
  a68_free_soid_list (top_sl);
}

/* Mode check unit list 2.  */

static void
mode_check_unit_list_2 (NODE_T *p, SOID_T *x, SOID_T *y)
{
  SOID_T *top_sl = NO_SOID;

  if (MOID (x) != NO_MOID)
    {
      if (IS_FLEX (MOID (x)))
	{
	  SOID_T y2;
	  a68_make_soid (&y2, SORT (x), SLICE (SUB_MOID (x)), 0);
	  mode_check_unit_list (&top_sl, SUB (p), &y2);
	}
      else if (IS_ROW (MOID (x)))
	{
	  SOID_T y2;
	  a68_make_soid (&y2, SORT (x), SLICE (MOID (x)), 0);
	  mode_check_unit_list (&top_sl, SUB (p), &y2);
	}
      else if (IS (MOID (x), STRUCT_SYMBOL))
	{
	  PACK_T *y2 = PACK (MOID (x));
	  mode_check_struct_display (&top_sl, SUB (p), &y2);
	}
      else
	mode_check_unit_list (&top_sl, SUB (p), x);
    }
  else
    mode_check_unit_list (&top_sl, SUB (p), x);

  a68_make_soid (y, STRONG, a68_pack_soids_in_moid (top_sl, STOWED_MODE), 0);
  a68_free_soid_list (top_sl);
}

/* Mode check access.  */

static void
mode_check_access (NODE_T *p, SOID_T *x, SOID_T *y)
{
  for (NODE_T *q = p; q != NO_NODE; FORWARD (q))
    {
      if (IS (q, ENCLOSED_CLAUSE))
	{
	  mode_check_enclosed (q, x, y);
	  MOID (p) = MOID (y);
	}
    }
}

/* Mode check closed.  */

static void
mode_check_closed (NODE_T *p, SOID_T *x, SOID_T *y)
{
  if (p == NO_NODE)
    return;
  else if (IS (p, SERIAL_CLAUSE))
    mode_check_serial_units (p, x, y, SERIAL_CLAUSE);
  else if (a68_is_one_of (p, OPEN_SYMBOL, BEGIN_SYMBOL, STOP))
    mode_check_closed (NEXT (p), x, y);
  MOID (p) = MOID (y);
}

/* Mode check collateral.  */

void
mode_check_collateral (NODE_T *p, SOID_T *x, SOID_T *y)
{
  if (p == NO_NODE)
    return;
  else if (a68_whether (p, BEGIN_SYMBOL, END_SYMBOL, STOP)
	   || a68_whether (p, OPEN_SYMBOL, CLOSE_SYMBOL, STOP))
    {
      if (SORT (x) == STRONG)
	{
	  if (MOID (x) == NO_MOID)
	    a68_error (p, "vacuum cannot have row elements (use a Y generator)",
		       "REF MODE");
	  else if (IS_FLEXETY_ROW (MOID (x)))
	    a68_make_soid (y, STRONG, M_VACUUM, 0);
	  else
	    {
	      /* The syntax only allows vacuums in strong contexts with rowed
		 modes.  See rule 33d.  */
	      a68_error (p, "a vacuum is not a valid M", MOID (x));
	      a68_make_soid (y, STRONG, M_ERROR, 0);
	    }
	}
      else
	a68_make_soid (y, STRONG, M_UNDEFINED, 0);
    }
  else
    {
      if (IS (p, UNIT_LIST))
	mode_check_unit_list_2 (p, x, y);
      else if (a68_is_one_of (p, OPEN_SYMBOL, BEGIN_SYMBOL, STOP))
	mode_check_collateral (NEXT (p), x, y);
      MOID (p) = MOID (y);
    }
}

/* Mode check conditional 2.  */

static void
mode_check_conditional_2 (SOID_T **ry, NODE_T *p, SOID_T *x)
{
  SOID_T enq_expct, enq_yield;

  a68_make_soid (&enq_expct, MEEK, M_BOOL, 0);
  mode_check_serial_units (NEXT_SUB (p), &enq_expct, &enq_yield, ENQUIRY_CLAUSE);
  if (!a68_is_coercible_in_context (&enq_yield, &enq_expct, SAFE_DEFLEXING))
    a68_cannot_coerce (p, MOID (&enq_yield), MOID (&enq_expct), MEEK, SAFE_DEFLEXING, ENQUIRY_CLAUSE);
  FORWARD (p);
  mode_check_serial (ry, NEXT_SUB (p), x, true);
  if ((FORWARD (p)) != NO_NODE)
    {
      if (a68_is_one_of (p, ELSE_PART, CHOICE, STOP))
	mode_check_serial (ry, NEXT_SUB (p), x, true);
      else if (a68_is_one_of (p, ELIF_PART, BRIEF_ELIF_PART, STOP))
	mode_check_conditional_2 (ry, SUB (p), x);
    }
}

/* Mode check conditional.  */

static void
mode_check_conditional (NODE_T *p, SOID_T *x, SOID_T *y)
{
  SOID_T *top_sl = NO_SOID;
  mode_check_conditional_2 (&top_sl, p, x);
  if (!a68_is_balanced (p, top_sl, SORT (x)))
    {
      if (MOID (x) != NO_MOID)
	a68_make_soid (y, SORT (x), MOID (x), CONDITIONAL_CLAUSE);
      else
	a68_make_soid (y, SORT (x), M_ERROR, 0);
    }
  else
    {
      MOID_T *z = a68_pack_soids_in_moid (top_sl, SERIES_MODE);
      a68_make_soid (y, SORT (x), z, CONDITIONAL_CLAUSE);
    }
  a68_free_soid_list (top_sl);
}

/* Mode check int case 2.  */

static void
mode_check_int_case_2 (SOID_T **ry, NODE_T *p, SOID_T *x)
{
  SOID_T enq_expct, enq_yield;
  a68_make_soid (&enq_expct, MEEK, M_INT, 0);
  mode_check_serial_units (NEXT_SUB (p), &enq_expct, &enq_yield, ENQUIRY_CLAUSE);
  if (!a68_is_coercible_in_context (&enq_yield, &enq_expct, SAFE_DEFLEXING))
    a68_cannot_coerce (p, MOID (&enq_yield), MOID (&enq_expct), MEEK, SAFE_DEFLEXING, ENQUIRY_CLAUSE);
  FORWARD (p);
  mode_check_unit_list (ry, NEXT_SUB (p), x);
  if ((FORWARD (p)) != NO_NODE)
    {
      if (a68_is_one_of (p, OUT_PART, CHOICE, STOP))
	mode_check_serial (ry, NEXT_SUB (p), x, true);
      else if (a68_is_one_of (p, CASE_OUSE_PART, BRIEF_OUSE_PART, STOP))
	mode_check_int_case_2 (ry, SUB (p), x);
    }
}

/* Mode check int case.  */

static void
mode_check_int_case (NODE_T *p, SOID_T *x, SOID_T *y)
{
  SOID_T *top_sl = NO_SOID;
  mode_check_int_case_2 (&top_sl, p, x);
  if (!a68_is_balanced (p, top_sl, SORT (x)))
    {
      if (MOID (x) != NO_MOID)
	a68_make_soid (y, SORT (x), MOID (x), CASE_CLAUSE);
      else
	a68_make_soid (y, SORT (x), M_ERROR, 0);
    }
  else
    {
      MOID_T *z = a68_pack_soids_in_moid (top_sl, SERIES_MODE);
      a68_make_soid (y, SORT (x), z, CASE_CLAUSE);
    }
  a68_free_soid_list (top_sl);
}

/* Mode check loop 2.  */

static void
mode_check_loop_2 (NODE_T *p, SOID_T *y)
{
  if (p == NO_NODE)
    return;
  else if (IS (p, FOR_PART))
    mode_check_loop_2 (NEXT (p), y);
  else if (a68_is_one_of (p, FROM_PART, BY_PART, TO_PART, STOP))
    {
      SOID_T ix, iy;
      a68_make_soid (&ix, STRONG, M_INT, 0);
      mode_check_unit (NEXT_SUB (p), &ix, &iy);
      if (!a68_is_coercible_in_context (&iy, &ix, SAFE_DEFLEXING))
	a68_cannot_coerce (NEXT_SUB (p), MOID (&iy), M_INT, MEEK, SAFE_DEFLEXING, ENQUIRY_CLAUSE);
      mode_check_loop_2 (NEXT (p), y);
    }
  else if (IS (p, WHILE_PART))
    {
      SOID_T enq_expct, enq_yield;
      a68_make_soid (&enq_expct, MEEK, M_BOOL, 0);
      mode_check_serial_units (NEXT_SUB (p), &enq_expct, &enq_yield, ENQUIRY_CLAUSE);
      if (!a68_is_coercible_in_context (&enq_yield, &enq_expct, SAFE_DEFLEXING))
	a68_cannot_coerce (p, MOID (&enq_yield), MOID (&enq_expct), MEEK, SAFE_DEFLEXING, ENQUIRY_CLAUSE);
      mode_check_loop_2 (NEXT (p), y);
    }
  else if (a68_is_one_of (p, DO_PART, ALT_DO_PART, STOP))
    {
      SOID_T *z = NO_SOID;
      NODE_T *do_p = NEXT_SUB (p);
      SOID_T ix;
      a68_make_soid (&ix, STRONG, M_VOID, 0);
      if (IS (do_p, SERIAL_CLAUSE))
	mode_check_serial (&z, do_p, &ix, true);
      a68_free_soid_list (z);
    }
}

/* Mode check loop.  */

static void
mode_check_loop (NODE_T *p, SOID_T *y)
{
  SOID_T *z = NO_SOID;
  mode_check_loop_2 (p, z);
  a68_make_soid (y, STRONG, M_VOID, 0);
}

/* Mode check enclosed.  */

static void
mode_check_enclosed (NODE_T *p, SOID_T *x, SOID_T *y)
{
  if (p == NO_NODE)
    return;
  else if (IS (p, ENCLOSED_CLAUSE))
    mode_check_enclosed (SUB (p), x, y);
  else if (IS (p, CLOSED_CLAUSE))
    mode_check_closed (SUB (p), x, y);
  else if (IS (p, ACCESS_CLAUSE))
    mode_check_access (SUB (p), x, y);
  else if (IS (p, PARALLEL_CLAUSE))
    {
      mode_check_collateral (SUB (NEXT_SUB (p)), x, y);
      a68_make_soid (y, STRONG, M_VOID, 0);
      MOID (NEXT_SUB (p)) = M_VOID;
    }
  else if (IS (p, COLLATERAL_CLAUSE))
    mode_check_collateral (SUB (p), x, y);
  else if (IS (p, CONDITIONAL_CLAUSE))
    mode_check_conditional (SUB (p), x, y);
  else if (IS (p, CASE_CLAUSE))
    mode_check_int_case (SUB (p), x, y);
  else if (IS (p, CONFORMITY_CLAUSE))
    mode_check_united_case (SUB (p), x, y);
  else if (IS (p, LOOP_CLAUSE))
    mode_check_loop (SUB (p), y);

  MOID (p) = MOID (y);
}

/* Search table for operator.  */

static TAG_T *
search_table_for_operator (TAG_T *t, const char *n, MOID_T *x, MOID_T *y)
{
  if (a68_is_mode_isnt_well (x))
    return A68_PARSER (error_tag);
  else if (y != NO_MOID && a68_is_mode_isnt_well (y))
    return A68_PARSER (error_tag);

  for (; t != NO_TAG; FORWARD (t))
    {
      if (NSYMBOL (NODE (t)) == n || strcmp (NSYMBOL (NODE (t)), n) == 0)
	{
	  PACK_T *p = PACK (MOID (t));
	  if (a68_is_coercible (x, MOID (p), FIRM, ALIAS_DEFLEXING))
	    {
	      FORWARD (p);
	      if (p == NO_PACK && y == NO_MOID)
		/* Matched in case of a monadic.  */
		return t;
	      else if (p != NO_PACK && y != NO_MOID
		       && a68_is_coercible (y, MOID (p), FIRM, ALIAS_DEFLEXING))
		/* Matched in case of a dyadic.  */
		return t;
	    }
	}
    }
  return NO_TAG;
}

/* Search chain of symbol tables and return matching operator "x n y" or
   "n x".  */

static TAG_T *
search_table_chain_for_operator (TABLE_T *s, const char *n, MOID_T *x, MOID_T *y)
{
  if (a68_is_mode_isnt_well (x))
    return A68_PARSER (error_tag);
  else if (y != NO_MOID && a68_is_mode_isnt_well (y))
    return A68_PARSER (error_tag);

  while (s != NO_TABLE)
    {
      TAG_T *z = search_table_for_operator (OPERATORS (s), n, x, y);
      if (z != NO_TAG)
	return z;
      BACKWARD (s);
    }
  return NO_TAG;
}

/* Return a matching operator "x n y".  */

static TAG_T *
find_operator (TABLE_T *s, const char *n, MOID_T *x, MOID_T *y)
{
  /* Coercions to operand modes are FIRM.  */
  MOID_T *u, *v; TAG_T *z;
  /* (A) Catch exceptions first.  */
  if (x == NO_MOID && y == NO_MOID)
    return NO_TAG;
  else if (a68_is_mode_isnt_well (x))
    return A68_PARSER (error_tag);
  else if (y != NO_MOID && a68_is_mode_isnt_well (y))
    return A68_PARSER (error_tag);

  /* (B) MONADs.  */
  if (x != NO_MOID && y == NO_MOID)
    {
      z = search_table_chain_for_operator (s, n, x, NO_MOID);
      if (z != NO_TAG)
	return z;
      else
	{
	  /* (B.2) A little trick to allow - (0, 1) or ABS (1, long pi).  */
	  if (a68_is_coercible (x, M_COMPLEX, STRONG, SAFE_DEFLEXING))
	    {
	      z = search_table_for_operator (OPERATORS (A68_STANDENV), n, M_COMPLEX, NO_MOID);
	      if (z != NO_TAG)
		return z;
	    }
	  if (a68_is_coercible (x, M_LONG_COMPLEX, STRONG, SAFE_DEFLEXING))
	    {
	      z = search_table_for_operator (OPERATORS (A68_STANDENV), n, M_LONG_COMPLEX, NO_MOID);
	      if (z != NO_TAG)
		return z;
	    }
	  if (a68_is_coercible (x, M_LONG_LONG_COMPLEX, STRONG, SAFE_DEFLEXING))
	    z = search_table_for_operator (OPERATORS (A68_STANDENV), n, M_LONG_LONG_COMPLEX, NO_MOID);
	}
      return NO_TAG;
    }
  /* (C) DYADs.  */
  z = search_table_chain_for_operator (s, n, x, y);
  if (z != NO_TAG)
    return z;
  /* (C.2) Vector and matrix "strong coercions" in standard environ.  */
  u = DEFLEX (a68_depref_completely (x));
  v = DEFLEX (a68_depref_completely (y));
  if ((u == M_ROW_REAL || u == M_ROW_ROW_REAL)
      || (v == M_ROW_REAL || v == M_ROW_ROW_REAL)
      || (u == M_ROW_COMPLEX || u == M_ROW_ROW_COMPLEX)
      || (v == M_ROW_COMPLEX || v == M_ROW_ROW_COMPLEX))
    {
      if (u == M_INT)
	{
	  z = search_table_for_operator (OPERATORS (A68_STANDENV), n, M_REAL, y);
	  if (z != NO_TAG)
	    return z;
	  z = search_table_for_operator (OPERATORS (A68_STANDENV), n, M_COMPLEX, y);
	  if (z != NO_TAG)
	    return z;
	}
      else if (v == M_INT)
	{
	  z = search_table_for_operator (OPERATORS (A68_STANDENV), n, x, M_REAL);
	  if (z != NO_TAG)
	    return z;
	  z = search_table_for_operator (OPERATORS (A68_STANDENV), n, x, M_COMPLEX);
	  if (z != NO_TAG)
	    return z;
	}
      else if (u == M_REAL)
	{
	  z = search_table_for_operator (OPERATORS (A68_STANDENV), n, M_COMPLEX, y);
	  if (z != NO_TAG)
	    return z;
	}
      else if (v == M_REAL)
	{
	  z = search_table_for_operator (OPERATORS (A68_STANDENV), n, x, M_COMPLEX);
	  if (z != NO_TAG)
	    return z;
	}
    }
  /* (C.3) Look in standenv for an appropriate cross-term.  */
  u = a68_make_series_from_moids (x, y);
  u = a68_make_united_mode (u);
  v = a68_get_balanced_mode (u, STRONG, A68_NO_DEPREF, SAFE_DEFLEXING);
  z = search_table_for_operator (OPERATORS (A68_STANDENV), n, v, v);
  if (z != NO_TAG)
    return z;
  if (a68_is_coercible_series (u, M_REAL, STRONG, SAFE_DEFLEXING))
    {
      z = search_table_for_operator (OPERATORS (A68_STANDENV), n, M_REAL, M_REAL);
      if (z != NO_TAG)
	return z;
    }
  if (a68_is_coercible_series (u, M_LONG_REAL, STRONG, SAFE_DEFLEXING))
    {
      z = search_table_for_operator (OPERATORS (A68_STANDENV), n, M_LONG_REAL, M_LONG_REAL);
      if (z != NO_TAG)
	return z;
    }
  if (a68_is_coercible_series (u, M_LONG_LONG_REAL, STRONG, SAFE_DEFLEXING))
    {
      z = search_table_for_operator (OPERATORS (A68_STANDENV), n, M_LONG_LONG_REAL, M_LONG_LONG_REAL);
      if (z != NO_TAG)
	return z;
    }
  if (a68_is_coercible_series (u, M_COMPLEX, STRONG, SAFE_DEFLEXING))
    {
      z = search_table_for_operator (OPERATORS (A68_STANDENV), n, M_COMPLEX, M_COMPLEX);
      if (z != NO_TAG)
	return z;
    }
  if (a68_is_coercible_series (u, M_LONG_COMPLEX, STRONG, SAFE_DEFLEXING))
    {
      z = search_table_for_operator (OPERATORS (A68_STANDENV), n, M_LONG_COMPLEX, M_LONG_COMPLEX);
      if (z != NO_TAG)
	return z;
    }
  if (a68_is_coercible_series (u, M_LONG_LONG_COMPLEX, STRONG, SAFE_DEFLEXING))
    {
      z = search_table_for_operator (OPERATORS (A68_STANDENV), n, M_LONG_LONG_COMPLEX, M_LONG_LONG_COMPLEX);
      if (z != NO_TAG)
	return z;
    }
  /* (C.4) Now allow for depreffing for REF REAL +:= INT and alike.  */
  v = a68_get_balanced_mode (u, STRONG, A68_DEPREF, SAFE_DEFLEXING);
  z = search_table_for_operator (OPERATORS (A68_STANDENV), n, v, v);
  if (z != NO_TAG)
    return z;
  return NO_TAG;
}

/* Mode check monadic operator.  */

static void
mode_check_monadic_operator (NODE_T *p, SOID_T *x, SOID_T *y)
{
  if (p != NO_NODE)
    {
      TAG_T *t;
      MOID_T *u = a68_determine_unique_mode (y, SAFE_DEFLEXING);
      if (a68_is_mode_isnt_well (u))
	a68_make_soid (y, SORT (x), M_ERROR, 0);
      else if (u == M_HIP)
	{
	  a68_error (NEXT (p), "M construct is an invalid operand", u);
	  a68_make_soid (y, SORT (x), M_ERROR, 0);
	}
      else
	{
	  if (strchr (NOMADS, *(NSYMBOL (p))) != NO_TEXT)
	    {
	      t = NO_TAG;
	      a68_error (p, "monadic S cannot start with a character from Z", NOMADS);
	      a68_make_soid (y, SORT (x), M_ERROR, 0);
	    }
	  else
	    {
	      t = find_operator (TABLE (p), NSYMBOL (p), u, NO_MOID);
	      if (t == NO_TAG)
		{
		  a68_error (p, "monadic operator S O has not been declared", u);
		  a68_make_soid (y, SORT (x), M_ERROR, 0);
		}
	    }
	  if (t != NO_TAG)
	    MOID (p) = MOID (t);
	  TAX (p) = t;
	  if (t != NO_TAG && t != A68_PARSER (error_tag))
	    {
	      MOID (p) = MOID (t);
	      a68_make_soid (y, SORT (x), SUB_MOID (t), 0);
	    }
	  else
	    {
	      MOID (p) = M_ERROR;
	      a68_make_soid (y, SORT (x), M_ERROR, 0);
	    }
	}
    }
}

/* Mode check monadic formula.  */

static void
mode_check_monadic_formula (NODE_T *p, SOID_T *x, SOID_T *y)
{
  SOID_T e;
  a68_make_soid (&e, FIRM, NO_MOID, 0);
  mode_check_formula (NEXT (p), &e, y);
  mode_check_monadic_operator (p, &e, y);
  a68_make_soid (y, SORT (x), MOID (y), 0);
}

/* Mode check formula.  */

static void
mode_check_formula (NODE_T *p, SOID_T *x, SOID_T *y)
{
  SOID_T ls;
  if (IS (p, MONADIC_FORMULA))
    mode_check_monadic_formula (SUB (p), x, &ls);
  else if (IS (p, FORMULA))
    mode_check_formula (SUB (p), x, &ls);
  else if (IS (p, SECONDARY))
    {
      SOID_T e;
      a68_make_soid (&e, FIRM, NO_MOID, 0);
      mode_check_unit (SUB (p), &e, &ls);
    }
  MOID_T *u = a68_determine_unique_mode (&ls, SAFE_DEFLEXING);
  MOID (p) = u;
  SOID_T rs;
  if (NEXT (p) == NO_NODE)
    a68_make_soid (y, SORT (x), u, 0);
  else
    {
      NODE_T *q = NEXT_NEXT (p);
      if (IS (q, MONADIC_FORMULA))
	mode_check_monadic_formula (SUB (NEXT_NEXT (p)), x, &rs);
      else if (IS (q, FORMULA))
	mode_check_formula (SUB (NEXT_NEXT (p)), x, &rs);
      else if (IS (q, SECONDARY))
	{
	  SOID_T e;
	  a68_make_soid (&e, FIRM, NO_MOID, 0);
	  mode_check_unit (SUB (q), &e, &rs);
	}
      MOID_T *v = a68_determine_unique_mode (&rs, SAFE_DEFLEXING);
      MOID (q) = v;
      if (a68_is_mode_isnt_well (u) || a68_is_mode_isnt_well (v))
	a68_make_soid (y, SORT (x), M_ERROR, 0);
      else if (u == M_HIP)
	{
	  a68_error (p, "M construct is an invalid operand", u);
	  a68_make_soid (y, SORT (x), M_ERROR, 0);
	}
      else if (v == M_HIP)
	{
	  a68_error (q, "M construct is an invalid operand", u);
	  a68_make_soid (y, SORT (x), M_ERROR, 0);
	}
      else
	{
	  TAG_T *op = find_operator (TABLE (NEXT (p)), NSYMBOL (NEXT (p)), u, v);
	  if (op == NO_TAG)
	    {
	      a68_error (NEXT (p), "dyadic operator O S O has not been declared", u, v);
	      a68_make_soid (y, SORT (x), M_ERROR, 0);
	    }
	  if (op != NO_TAG)
	    MOID (NEXT (p)) = MOID (op);
	  TAX (NEXT (p)) = op;
	  if (op != NO_TAG && op != A68_PARSER (error_tag))
	    a68_make_soid (y, SORT (x), SUB_MOID (op), 0);
	  else
	    a68_make_soid (y, SORT (x), M_ERROR, 0);
	}
    }
}

/* Mode check assignation.  */

static void
mode_check_assignation (NODE_T *p, SOID_T *x, SOID_T *y)
{
  /* Get destination mode.  */
  SOID_T name, tmp, value;
  a68_make_soid (&name, SOFT, NO_MOID, 0);
  mode_check_unit (SUB (p), &name, &tmp);
  /* SOFT coercion.  */
  MOID_T *ori = a68_determine_unique_mode (&tmp, SAFE_DEFLEXING);
  MOID_T *name_moid = a68_deproc_completely (ori);
  if (ATTRIBUTE (name_moid) != REF_SYMBOL)
    {
      if (A68_IF_MODE_IS_WELL (name_moid))
	a68_error (p, "M A does not yield a name", ori, ATTRIBUTE (SUB (p)));
      a68_make_soid (y, SORT (x), M_ERROR, 0);
      return;
    }
  MOID (p) = name_moid;
  /* Get source mode.  */
  a68_make_soid (&name, STRONG, SUB (name_moid), 0);
  mode_check_unit (NEXT_NEXT (p), &name, &value);
  if (!a68_is_coercible_in_context (&value, &name, FORCE_DEFLEXING))
    {
      a68_cannot_coerce (p, MOID (&value), MOID (&name), STRONG, FORCE_DEFLEXING, UNIT);
      a68_make_soid (y, SORT (x), M_ERROR, 0);
    }
  else
    a68_make_soid (y, SORT (x), name_moid, 0);
}

/* Mode check identity relation.  */

static void
mode_check_identity_relation (NODE_T *p, SOID_T *x, SOID_T *y)
{
  NODE_T *ln = p, *rn = NEXT_NEXT (p);
  SOID_T e, l, r;
  a68_make_soid (&e, SOFT, NO_MOID, 0);
  mode_check_unit (SUB (ln), &e, &l);
  mode_check_unit (SUB (rn), &e, &r);
  /* SOFT coercion.  */
  MOID_T *oril = a68_determine_unique_mode (&l, SAFE_DEFLEXING);
  MOID_T *orir = a68_determine_unique_mode (&r, SAFE_DEFLEXING);
  MOID_T *lhs = a68_deproc_completely (oril);
  MOID_T *rhs = a68_deproc_completely (orir);
  if (A68_IF_MODE_IS_WELL (lhs) && lhs != M_HIP && ATTRIBUTE (lhs) != REF_SYMBOL)
    {
      a68_error (ln, "M A does not yield a name", oril, ATTRIBUTE (SUB (ln)));
      lhs = M_ERROR;
    }
  if (A68_IF_MODE_IS_WELL (rhs) && rhs != M_HIP && ATTRIBUTE (rhs) != REF_SYMBOL)
    {
      a68_error (rn, "M A does not yield a name", orir, ATTRIBUTE (SUB (rn)));
      rhs = M_ERROR;
    }
  if (lhs == M_HIP && rhs == M_HIP)
    a68_error (p, "construct has no unique mode");

  if (a68_is_coercible (lhs, rhs, STRONG, SAFE_DEFLEXING))
    lhs = rhs;
  else if (a68_is_coercible (rhs, lhs, STRONG, SAFE_DEFLEXING))
    rhs = lhs;
  else
    {
      a68_cannot_coerce (NEXT (p), rhs, lhs, SOFT, SKIP_DEFLEXING, TERTIARY);
      lhs = rhs = M_ERROR;
    }
  MOID (ln) = lhs;
  MOID (rn) = rhs;
  a68_make_soid (y, SORT (x), M_BOOL, 0);
}

/* Mode check bool functions ANDF and ORF.  */

static void
mode_check_bool_function (NODE_T *p, SOID_T *x, SOID_T *y)
{
  SOID_T e, l, r;
  NODE_T *ln = p, *rn = NEXT_NEXT (p);
  a68_make_soid (&e, STRONG, M_BOOL, 0);
  mode_check_unit (SUB (ln), &e, &l);
  if (!a68_is_coercible_in_context (&l, &e, SAFE_DEFLEXING))
    a68_cannot_coerce (ln, MOID (&l), MOID (&e), MEEK, SAFE_DEFLEXING, TERTIARY);
  mode_check_unit (SUB (rn), &e, &r);
  if (!a68_is_coercible_in_context (&r, &e, SAFE_DEFLEXING))
    a68_cannot_coerce (rn, MOID (&r), MOID (&e), MEEK, SAFE_DEFLEXING, TERTIARY);
  MOID (ln) = M_BOOL;
  MOID (rn) = M_BOOL;
  a68_make_soid (y, SORT (x), M_BOOL, 0);
}

/* Mode check cast.  */

static void
mode_check_cast (NODE_T *p, SOID_T *x, SOID_T *y)
{
  SOID_T w;
  mode_check_declarer (p);
  a68_make_soid (&w, STRONG, MOID (p), 0);
  CAST (&w) = true;
  mode_check_enclosed (SUB_NEXT (p), &w, y);
  if (!a68_is_coercible_in_context (y, &w, SAFE_DEFLEXING))
    a68_cannot_coerce (NEXT (p), MOID (y), MOID (&w), STRONG, SAFE_DEFLEXING, ENCLOSED_CLAUSE);
  a68_make_soid (y, SORT (x), MOID (p), 0);
}

/* Mode check assertion.  */

static void
mode_check_assertion (NODE_T *p)
{
  SOID_T w, y;
  a68_make_soid (&w, STRONG, M_BOOL, 0);
  mode_check_enclosed (SUB_NEXT (p), &w, &y);
  SORT (&y) = SORT (&w);
  if (!a68_is_coercible_in_context (&y, &w, NO_DEFLEXING))
    a68_cannot_coerce (NEXT (p), MOID (&y), MOID (&w), MEEK, NO_DEFLEXING, ENCLOSED_CLAUSE);
}

/* Mode check argument list.  */

static void
mode_check_argument_list (SOID_T **r, NODE_T *p, PACK_T **x, PACK_T **v, PACK_T **w)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (IS (p, GENERIC_ARGUMENT_LIST))
	ATTRIBUTE (p) = ARGUMENT_LIST;

      if (IS (p, ARGUMENT_LIST))
	mode_check_argument_list (r, SUB (p), x, v, w);
      else if (IS (p, UNIT))
	{
	  SOID_T y, z;
	  if (*x != NO_PACK)
	    {
	      a68_make_soid (&z, STRONG, MOID (*x), 0);
	      a68_add_mode_to_pack_end (v, MOID (*x), NO_TEXT, p);
	      FORWARD (*x);
	    }
	  else
	    a68_make_soid (&z, STRONG, NO_MOID, 0);
	  mode_check_unit (p, &z, &y);
	  a68_add_to_soid_list (r, p, &y);
	}
      else if (IS (p, TRIMMER))
	{
	  SOID_T z;
	  if (SUB (p) != NO_NODE)
	    {
	      a68_error (p, "syntax error detected in A", ARGUMENT);
	      a68_make_soid (&z, STRONG, M_ERROR, 0);
	      a68_add_mode_to_pack_end (v, M_VOID, NO_TEXT, p);
	      a68_add_mode_to_pack_end (w, MOID (*x), NO_TEXT, p);
	      FORWARD (*x);
	    }
	  else if (*x != NO_PACK)
	    {
	      a68_make_soid (&z, STRONG, MOID (*x), 0);
	      a68_add_mode_to_pack_end (v, M_VOID, NO_TEXT, p);
	      a68_add_mode_to_pack_end (w, MOID (*x), NO_TEXT, p);
	      FORWARD (*x);
	    }
	  else
	    a68_make_soid (&z, STRONG, NO_MOID, 0);
	  a68_add_to_soid_list (r, p, &z);
	}
      else if (IS (p, SUB_SYMBOL) && !OPTION_BRACKETS (&A68_JOB))
	a68_error (p, "syntax error detected in A", CALL);
    }
}

/* Mode check argument list 2.  */

static void
mode_check_argument_list_2 (NODE_T *p, PACK_T *x, SOID_T *y, PACK_T **v, PACK_T **w)
{
  SOID_T *top_sl = NO_SOID;
  mode_check_argument_list (&top_sl, SUB (p), &x, v, w);
  a68_make_soid (y, STRONG, a68_pack_soids_in_moid (top_sl, STOWED_MODE), 0);
  a68_free_soid_list (top_sl);
}

/* Mode check meek int.  */

static void
mode_check_meek_int (NODE_T *p)
{
  SOID_T x, y;
  a68_make_soid (&x, MEEK, M_INT, 0);
  mode_check_unit (p, &x, &y);
  if (!a68_is_coercible_in_context (&y, &x, SAFE_DEFLEXING))
    a68_cannot_coerce (p, MOID (&y), MOID (&x), MEEK, SAFE_DEFLEXING, 0);
}

/* Mode check trimmer.  */

static void
mode_check_trimmer (NODE_T *p)
{
  if (p == NO_NODE)
    return;
  else if (IS (p, TRIMMER))
    mode_check_trimmer (SUB (p));
  else if (IS (p, UNIT))
    {
      mode_check_meek_int (p);
      mode_check_trimmer (NEXT (p));
    }
  else
    mode_check_trimmer (NEXT (p));
}

/* Mode check indexer.  */

static void
mode_check_indexer (NODE_T *p, int *subs, int *trims)
{
  if (p == NO_NODE)
    return;
  else if (IS (p, TRIMMER))
    {
      (*trims)++;
      mode_check_trimmer (SUB (p));
    }
  else if (IS (p, UNIT))
    {
      (*subs)++;
      mode_check_meek_int (p);
    }
  else
    {
      mode_check_indexer (SUB (p), subs, trims);
      mode_check_indexer (NEXT (p), subs, trims);
    }
}

/* Mode check call.  */

static void
mode_check_call (NODE_T *p, MOID_T *n, SOID_T *x, SOID_T *y)
{
  MOID (p) = n;
  /* "partial_locale" is the mode of the locale.  */
  PARTIAL_LOCALE (GINFO (p)) = a68_new_moid ();
  ATTRIBUTE (PARTIAL_LOCALE (GINFO (p))) = PROC_SYMBOL;
  PACK (PARTIAL_LOCALE (GINFO (p))) = NO_PACK;
  SUB (PARTIAL_LOCALE (GINFO (p))) = SUB (n);
  /* "partial_proc" is the mode of the resulting proc.  */
  PARTIAL_PROC (GINFO (p)) = a68_new_moid ();
  ATTRIBUTE (PARTIAL_PROC (GINFO (p))) = PROC_SYMBOL;
  PACK (PARTIAL_PROC (GINFO (p))) = NO_PACK;
  SUB (PARTIAL_PROC (GINFO (p))) = SUB (n);
  /* Check arguments and construct modes.  */
  SOID_T d;
  mode_check_argument_list_2 (NEXT (p), PACK (n), &d, &PACK (PARTIAL_LOCALE (GINFO (p))),
			      &PACK (PARTIAL_PROC (GINFO (p))));
  DIM (PARTIAL_PROC (GINFO (p))) = a68_count_pack_members (PACK (PARTIAL_PROC (GINFO (p))));
  DIM (PARTIAL_LOCALE (GINFO (p))) = a68_count_pack_members (PACK (PARTIAL_LOCALE (GINFO (p))));
  PARTIAL_PROC (GINFO (p)) = a68_register_extra_mode (&TOP_MOID (&A68_JOB), PARTIAL_PROC (GINFO (p)));
  PARTIAL_LOCALE (GINFO (p)) = a68_register_extra_mode (&TOP_MOID (&A68_JOB), PARTIAL_LOCALE (GINFO (p)));
  if (DIM (MOID (&d)) != DIM (n))
    {
      a68_error (p, "incorrect number of arguments for M", n);
      a68_make_soid (y, SORT (x), SUB (n), 0);
      /*  a68_make_soid (y, SORT (x), M_ERROR, 0);.  */
    }
  else
    {
      if (!a68_is_coercible (MOID (&d), n, STRONG, ALIAS_DEFLEXING))
	a68_cannot_coerce (p, MOID (&d), n, STRONG, ALIAS_DEFLEXING, ARGUMENT);
      if (DIM (PARTIAL_PROC (GINFO (p))) == 0)
	a68_make_soid (y, SORT (x), SUB (n), 0);
      else
	{
	  a68_warning (NEXT (p), OPT_Wextensions, "@ is an extension");
	  a68_make_soid (y, SORT (x), PARTIAL_PROC (GINFO (p)), 0);
	}
    }
}

/* Mode check slice.  */

static void
mode_check_slice (NODE_T *p, MOID_T *ori, SOID_T *x, SOID_T *y)
{
  MOID_T *m = a68_depref_completely (ori), *n = ori;
  /* WEAK coercion.  */
  while ((IS_REF (n) && !a68_is_ref_row (n)) || (IS (n, PROC_SYMBOL) && PACK (n) == NO_PACK))
    n = a68_depref_once (n);

  if (n == NO_MOID || !(SLICE (DEFLEX (n)) != NO_MOID || a68_is_ref_row (n)))
    {
      if (A68_IF_MODE_IS_WELL (n))
	a68_error (p, "M A does not yield a row or procedure",
		   n, ATTRIBUTE (SUB (p)));
      a68_make_soid (y, SORT (x), M_ERROR, 0);
    }

  MOID (p) = n;
  int dim = 0, subs = 0, trims = 0;
  mode_check_indexer (SUB_NEXT (p), &subs, &trims);
  bool is_ref;
  if ((is_ref = a68_is_ref_row (n)) != 0)
    dim = DIM (DEFLEX (SUB (n)));
  else
    dim = DIM (DEFLEX (n));

  if ((subs + trims) != dim)
    {
      a68_error (p, "incorrect number of indexers for M", n);
      a68_make_soid (y, SORT (x), M_ERROR, 0);
    }
  else
    {
      if (subs > 0 && trims == 0)
	{
	  ANNOTATION (NEXT (p)) = SLICE;
	  m = n;
	}
      else
	{
	  ANNOTATION (NEXT (p)) = TRIMMER;
	  m = n;
	}
      while (subs > 0)
	{
	  if (is_ref)
	    m = NAME (m);
	  else
	    {
	      if (IS_FLEX (m))
		m = SUB (m);
	      m = SLICE (m);
	    }
	  gcc_assert (m != NO_MOID);
	  subs--;
	}
      /* A trim cannot be but deflexed.  */
      if (ANNOTATION (NEXT (p)) == TRIMMER && TRIM (m) != NO_MOID)
	{
	  gcc_assert (TRIM (m) != NO_MOID);
	  a68_make_soid (y, SORT (x), TRIM (m), 0);
	}
      else
	a68_make_soid (y, SORT (x), m, 0);
    }
}

/* Mode check specification.  */

static enum a68_attribute
mode_check_specification (NODE_T *p, SOID_T *x, SOID_T *y)
{
  SOID_T w, d;
  a68_make_soid (&w, WEAK, NO_MOID, 0);
  mode_check_unit (SUB (p), &w, &d);
  MOID_T *ori = a68_determine_unique_mode (&d, SAFE_DEFLEXING);
  MOID_T *m = a68_depref_completely (ori);
  if (IS (m, PROC_SYMBOL))
    {
      /* Assume CALL.  */
      mode_check_call (p, m, x, y);
      return CALL;
    }
  else if (IS_ROW (m) || IS_FLEX (m))
    {
      /* Assume SLICE.  */
      mode_check_slice (p, ori, x, y);
      return SLICE;
    }
  else
    {
      if (m != M_ERROR)
	a68_error (p, "M construct must yield a routine or a row value", m);
      a68_make_soid (y, SORT (x), M_ERROR, 0);
      return PRIMARY;
    }
}

/* Mode check selection.  */

static void
mode_check_selection (NODE_T *p, SOID_T *x, SOID_T *y)
{
  bool deflex = false;
  NODE_T *secondary = SUB_NEXT (p);
  SOID_T w, d;
  a68_make_soid (&w, WEAK, NO_MOID, 0);
  mode_check_unit (secondary, &w, &d);
  MOID_T *n, *ori;
  n = ori = a68_determine_unique_mode (&d, SAFE_DEFLEXING);
  PACK_T *t = NO_PACK, *t_2 = NO_PACK;
  bool coerce = true;
  while (coerce)
    {
      if (IS (n, STRUCT_SYMBOL))
	{
	  coerce = false;
	  t = PACK (n);
	}
      else if (IS_REF (n) && (IS_ROW (SUB (n)) || IS_FLEX (SUB (n))) && MULTIPLE (n) != NO_MOID)
	{
	  coerce = false;
	  deflex = true;
	  t = PACK (MULTIPLE (n));
	}
      else if ((IS_ROW (n) || IS_FLEX (n)) && MULTIPLE (n) != NO_MOID)
	{
	  coerce = false;
	  deflex = true;
	  t = PACK (MULTIPLE (n));
	}
      else if (IS_REF (n) && a68_is_name_struct (n))
	{
	  coerce = false;
	  t = PACK (NAME (n));
	}
      else if (a68_is_deprefable (n))
	{
	  coerce = true;
	  n = SUB (n);
	  t = NO_PACK;
	}
      else
	{
	  coerce = false;
	  t = NO_PACK;
	}
    }
  if (t == NO_PACK)
    {
      if (A68_IF_MODE_IS_WELL (MOID (&d)))
	a68_error (secondary, "M A does not yield a structured value", ori, ATTRIBUTE (secondary));
      a68_make_soid (y, SORT (x), M_ERROR, 0);
      return;
    }

  MOID (NEXT (p)) = n;
  const char *fs = NSYMBOL (SUB (p));
  MOID_T *str = n;
  while (IS_REF (str))
    str = SUB (str);
  if (IS_FLEX (str))
    str = SUB (str);
  if (IS_ROW (str))
    str = SUB (str);
  t_2 = PACK (str);
  while (t != NO_PACK && t_2 != NO_PACK)
    {
      if (TEXT (t) == fs || strcmp (TEXT (t), fs) == 0)
	{
	  MOID_T *ret = MOID (t);
	  if (deflex && TRIM (ret) != NO_MOID)
	    ret = TRIM (ret);
	  a68_make_soid (y, SORT (x), ret, 0);
	  MOID (p) = ret;
	  NODE_PACK (SUB (p)) = t_2;
	  return;
	}
      FORWARD (t);
      FORWARD (t_2);
    }
  a68_make_soid (&d, NO_SORT, n, 0);
  a68_error (p, "M has no field Z", str, fs);
  a68_make_soid (y, SORT (x), M_ERROR, 0);
}

/* Mode check format text.  */

static void
mode_check_format_text (NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      mode_check_format_text (SUB (p));
      if (IS (p, FORMAT_PATTERN))
	{
	  SOID_T x, y;
	  a68_make_soid (&x, STRONG, M_FORMAT, 0);
	  mode_check_enclosed (SUB (NEXT_SUB (p)), &x, &y);
	  if (!a68_is_coercible_in_context (&y, &x, SAFE_DEFLEXING))
	    a68_cannot_coerce (p, MOID (&y), MOID (&x), STRONG, SAFE_DEFLEXING, ENCLOSED_CLAUSE);
	}
      else if (IS (p, GENERAL_PATTERN) && NEXT_SUB (p) != NO_NODE)
	{
	  SOID_T x, y;
	  a68_make_soid (&x, STRONG, M_ROW_INT, 0);
	  mode_check_enclosed (SUB (NEXT_SUB (p)), &x, &y);
	  if (!a68_is_coercible_in_context (&y, &x, SAFE_DEFLEXING))
	    a68_cannot_coerce (p, MOID (&y), MOID (&x), STRONG, SAFE_DEFLEXING, ENCLOSED_CLAUSE);
	}
      else if (IS (p, DYNAMIC_REPLICATOR))
	{
	  SOID_T x, y;
	  a68_make_soid (&x, STRONG, M_INT, 0);
	  mode_check_enclosed (SUB (NEXT_SUB (p)), &x, &y);
	  if (!a68_is_coercible_in_context (&y, &x, SAFE_DEFLEXING))
	    a68_cannot_coerce (p, MOID (&y), MOID (&x), STRONG, SAFE_DEFLEXING, ENCLOSED_CLAUSE);
	}
    }
}

/* Mode check unit.  */

static void
mode_check_unit (NODE_T *p, SOID_T *x, SOID_T *y)
{
  if (p == NO_NODE)
    return;
  else if (a68_is_one_of (p, UNIT, TERTIARY, SECONDARY, PRIMARY, STOP))
    mode_check_unit (SUB (p), x, y);
  /* Ex primary.  */
  else if (IS (p, SPECIFICATION))
    {
      ATTRIBUTE (p) = mode_check_specification (SUB (p), x, y);
      a68_warn_for_voiding (p, x, y, ATTRIBUTE (p));
    }
  else if (IS (p, CAST))
    {
      mode_check_cast (SUB (p), x, y);
      a68_warn_for_voiding (p, x, y, CAST);
    }
  else if (IS (p, DENOTATION))
    {
      a68_make_soid (y, SORT (x), MOID (SUB (p)), 0);
      a68_warn_for_voiding (p, x, y, DENOTATION);
    }
  else if (IS (p, IDENTIFIER))
    {
      if ((TAX (p) == NO_TAG) && (MOID (p) == NO_MOID))
	{
	  int att = a68_first_tag_global (TABLE (p), NSYMBOL (p));
	  if (att == STOP)
	    {
	      (void) a68_add_tag (TABLE (p), IDENTIFIER, p, M_ERROR, NORMAL_IDENTIFIER);
	      a68_error (p, "tag S has not been declared properly");
	      MOID (p) = M_ERROR;
	    }
	  else
	    {
	      TAG_T *z = a68_find_tag_global (TABLE (p), att, NSYMBOL (p));
	      if (att == IDENTIFIER && z != NO_TAG)
		MOID (p) = MOID (z);
	      else
		{
		  (void) a68_add_tag (TABLE (p), IDENTIFIER, p, M_ERROR, NORMAL_IDENTIFIER);
		  a68_error (p, "tag S has not been declared properly");
		  MOID (p) = M_ERROR;
		}
	    }
	}
      a68_make_soid (y, SORT (x), MOID (p), 0);
      a68_warn_for_voiding (p, x, y, IDENTIFIER);
    }
  else if (IS (p, ENCLOSED_CLAUSE))
    mode_check_enclosed (SUB (p), x, y);
  else if (IS (p, FORMAT_TEXT))
    {
      mode_check_format_text (p);
      a68_make_soid (y, SORT (x), M_FORMAT, 0);
      a68_warn_for_voiding (p, x, y, FORMAT_TEXT);
      /* Ex secondary.  */
    }
  else if (IS (p, GENERATOR))
    {
      mode_check_declarer (SUB (p));
      a68_make_soid (y, SORT (x), MOID (SUB (p)), 0);
      a68_warn_for_voiding (p, x, y, GENERATOR);
    }
  else if (IS (p, SELECTION))
    {
      mode_check_selection (SUB (p), x, y);
      a68_warn_for_voiding (p, x, y, SELECTION);
      /* Ex tertiary.  */
    }
  else if (IS (p, NIHIL))
    a68_make_soid (y, STRONG, M_HIP, 0);
  else if (IS (p, FORMULA))
    {
      mode_check_formula (p, x, y);
      if (!IS_REF (MOID (y)))
	a68_warn_for_voiding (p, x, y, FORMULA);
    }
  else if (a68_is_one_of (p, JUMP, SKIP, STOP))
    {
      if (SORT (x) != STRONG)
	a68_warning (p, 0, "@ should not be in C context", SORT (x));
      /*  a68_make_soid (y, STRONG, M_HIP, 0);  */
      a68_make_soid (y, SORT (x), M_HIP, 0);
    }
  else if (IS (p, ASSIGNATION))
    mode_check_assignation (SUB (p), x, y);
  else if (IS (p, IDENTITY_RELATION))
    {
      mode_check_identity_relation (SUB (p), x, y);
      a68_warn_for_voiding (p, x, y, IDENTITY_RELATION);
    }
  else if (IS (p, ROUTINE_TEXT))
    {
      mode_check_routine_text (SUB (p), y);
      a68_make_soid (y, SORT (x), MOID (p), 0);
      a68_warn_for_voiding (p, x, y, ROUTINE_TEXT);
    }
  else if (IS (p, ASSERTION))
    {
      mode_check_assertion (SUB (p));
      a68_make_soid (y, STRONG, M_VOID, 0);
    }
  else if (IS (p, AND_FUNCTION))
    {
      mode_check_bool_function (SUB (p), x, y);
      a68_warn_for_voiding (p, x, y, AND_FUNCTION);
    }
  else if (IS (p, OR_FUNCTION))
    {
      mode_check_bool_function (SUB (p), x, y);
      a68_warn_for_voiding (p, x, y, OR_FUNCTION);
    }
  else if (IS (p, FORMAL_HOLE))
    {
      NODE_T *tertiary = NO_NODE;

      for (NODE_T *q = SUB (p); q != NO_NODE; FORWARD (q))
	{
	  if (IS (q, TERTIARY))
	    {
	      tertiary = q;
	      break;
	    }
	}

      NODE_T *str = tertiary;
      while (str != NO_NODE && !IS (str, ROW_CHAR_DENOTATION))
	str = SUB (str);
      gcc_assert (IS (str, ROW_CHAR_DENOTATION));

      if (SORT (x) != STRONG)
	{
	  /* A formal hole should appear in a strong context, and its mode is
	     the goal mode of the context.  */
	  a68_error (p, "formal hole should be in a strong context");
	  a68_make_soid (y, STRONG, M_ERROR, 0);
	}
      else if (!a68_is_c_mode (MOID (x)))
	{
	  /* Additionally, the mode of the formal hole should be amenable to be
	     somehow "translated" to C semantics. */
	  a68_error (p, "formal hole cannot be of mode M", MOID (x));
	  a68_make_soid (y, STRONG, M_ERROR, 0);
	}
      else if (NSYMBOL (str)[0] == '&' && !IS_REF (MOID (x)))
	{
	  /* A C formal whole whose string starts with & requires
	     a ref mode.  */
	  a68_error (p, "formal hole should be a name (ref to a mode)");
	  a68_make_soid (y, STRONG, M_ERROR, 0);
	}
      else
	{
	  SOID_T z;
	  mode_check_unit (tertiary, x, &z);
	  a68_make_soid (y, SORT (x), MOID (x), 0);
	  a68_warn_for_voiding (p, x, y, FORMAL_HOLE);
	}
    }

  MOID (p) = MOID (y);
}

/* Mode check a module text.  */

static void
mode_check_module_text (NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (IS (p, DEF_PART) || IS (p, POSTLUDE_PART))
	{
	  /* XXX unde def is an enquiry clause  */
	  SOID_T *z = NO_SOID;
	  SOID_T ix;
	  a68_make_soid (&ix, STRONG, M_VOID, 0);
	  mode_check_serial (&z, NEXT_SUB (p), &ix, true);
	  a68_free_soid_list (z);
	}
    }
}

/* Mode check a module declaration.  */

static void
mode_check_module_declaration (NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (IS (p, MODULE_TEXT))
	mode_check_module_text (SUB (p));
      else
	mode_check_module_declaration (SUB (p));
    }
}
