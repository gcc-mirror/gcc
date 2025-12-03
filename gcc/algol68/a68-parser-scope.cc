/* Static scope checker.
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

/* A static scope checker inspects the source. Note that ALGOL 68 also needs
   dynamic scope checking. This phase concludes the parser.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "options.h"

#include "a68.h"

struct TUPLE_T
{
  int level;
  bool transient;
};

struct SCOPE_T
{
  NODE_T *where;
  TUPLE_T tuple;
  SCOPE_T *next;
};

constexpr TUPLE_T *NO_TUPLE = nullptr;
constexpr SCOPE_T *NO_SCOPE = nullptr;

#define TUPLE(p) ((p)->tuple)

enum { NOT_TRANSIENT = 0, TRANSIENT };

static void gather_scopes_for_youngest (NODE_T *, SCOPE_T **);
static void scope_statement (NODE_T *, SCOPE_T **);
static void scope_enclosed_clause (NODE_T *, SCOPE_T **);
static void scope_formula (NODE_T *, SCOPE_T **);
static void scope_routine_text (NODE_T *, SCOPE_T **);

/*
 * Static scope checker.
 */

/* Scope_make_tuple.  */

static TUPLE_T
scope_make_tuple (int e, int t)
{
  static TUPLE_T z;
  LEVEL (&z) = e;
  TRANSIENT (&z) = t;
  return z;
}

/* Link scope information into the list.  */

static void
scope_add (SCOPE_T **sl, NODE_T *p, TUPLE_T tup)
{
  if (sl != NO_VAR)
    {
      SCOPE_T *ns = (SCOPE_T *) xmalloc (sizeof (SCOPE_T));
      WHERE (ns) = p;
      TUPLE (ns) = tup;
      NEXT (ns) = *sl;
      *sl = ns;
    }
}

/* Scope_check.  */

static bool
scope_check (SCOPE_T *top, int mask, int dest)
{
  int errors = 0;

  /* Transient names cannot be stored.  */
  if (mask & TRANSIENT)
    {
      for (SCOPE_T *s = top; s != NO_SCOPE; FORWARD (s))
	{
	  if (TRANSIENT (&TUPLE (s)) & TRANSIENT)
	    {
	      a68_error (WHERE (s), "attempt at storing a transient name");
	      STATUS_SET (WHERE (s), SCOPE_ERROR_MASK);
	      errors++;
	    }
	}
    }

  /* Potential scope violations.  */
  for (SCOPE_T *s = top; s != NO_SCOPE; FORWARD (s))
    {
      if (dest < LEVEL (&TUPLE (s)) && !STATUS_TEST (WHERE (s), SCOPE_ERROR_MASK))
	{
	  MOID_T *ws = MOID (WHERE (s));

	  if (ws != NO_MOID)
	    {
	      if (IS_REF (ws) || IS (ws, PROC_SYMBOL) || IS (ws, FORMAT_SYMBOL) || IS (ws, UNION_SYMBOL))
		a68_warning (WHERE (s), OPT_Wscope, "M A is a potential scope violation",
			     MOID (WHERE (s)), ATTRIBUTE (WHERE (s)));
	    }
	  STATUS_SET (WHERE (s), SCOPE_ERROR_MASK);
	  errors++;
	}
    }
  return (errors == 0);
}

/* Scope_check_multiple.  */

static bool
scope_check_multiple (SCOPE_T *top, int mask, SCOPE_T *dest)
{
  bool no_err = true;

  for (; dest != NO_SCOPE; FORWARD (dest))
    no_err = no_err && scope_check (top, mask, LEVEL (&TUPLE (dest)));
  return no_err;
}

/* Check_identifier_usage.  */

static void
check_identifier_usage (TAG_T *t, NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (IS (p, IDENTIFIER) && TAX (p) == t && ATTRIBUTE (MOID (t)) != PROC_SYMBOL)
	a68_warning (p, OPT_Wuninitialized, "identifier S might be used uninitialised");
      check_identifier_usage (t, SUB (p));
    }
}

/* Scope_find_youngest_outside.  */

static TUPLE_T
scope_find_youngest_outside (SCOPE_T *s, int treshold)
{
  TUPLE_T z = scope_make_tuple (PRIMAL_SCOPE, NOT_TRANSIENT);

  for (; s != NO_SCOPE; FORWARD (s))
    {
      if (LEVEL (&TUPLE (s)) > LEVEL (&z) && LEVEL (&TUPLE (s)) <= treshold)
	z = TUPLE (s);
    }
  return z;
}

/* Scope_find_youngest.  */

static TUPLE_T
scope_find_youngest (SCOPE_T *s)
{
  return scope_find_youngest_outside (s, INT_MAX);
}

/*
 * Routines for determining scope of ROUTINE TEXT or FORMAT TEXT.
 */

/* Get_declarer_elements.  */

static void
get_declarer_elements (NODE_T *p, SCOPE_T **r, bool no_ref)
{
  if (p != NO_NODE)
    {
      if (IS (p, BOUNDS))
	gather_scopes_for_youngest (SUB (p), r);
      else if (IS (p, INDICANT))
	{
	  if (MOID (p) != NO_MOID && TAX (p) != NO_TAG && HAS_ROWS (MOID (p)) && no_ref)
	    scope_add (r, p, scope_make_tuple (TAG_LEX_LEVEL (TAX (p)), NOT_TRANSIENT));
	}
      else if (IS_REF (p))
	get_declarer_elements (NEXT (p), r, false);
      else if (a68_is_one_of (p, PROC_SYMBOL, UNION_SYMBOL, STOP))
	;
      else
	{
	  get_declarer_elements (SUB (p), r, no_ref);
	  get_declarer_elements (NEXT (p), r, no_ref);
	}
    }
}

/* Gather_scopes_for_youngest.  */

static void
gather_scopes_for_youngest (NODE_T *p, SCOPE_T **s)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if ((a68_is_one_of (p, ROUTINE_TEXT, FORMAT_TEXT, STOP))
	  && (YOUNGEST_ENVIRON (TAX (p)) == PRIMAL_SCOPE))
	{
	  SCOPE_T *t = NO_SCOPE;
	  TUPLE_T tup;

	  gather_scopes_for_youngest (SUB (p), &t);
	  tup = scope_find_youngest_outside (t, LEX_LEVEL (p));
	  YOUNGEST_ENVIRON (TAX (p)) = LEVEL (&tup);
	  /* Direct link into list iso "gather_scopes_for_youngest (SUB (p),
	     s);".  */
	  if (t != NO_SCOPE)
	    {
	      SCOPE_T *u = t;
	      while (NEXT (u) != NO_SCOPE) {
		FORWARD (u);
	      }
	      NEXT (u) = *s;
	      (*s) = t;
	    }
	}
      else if (a68_is_one_of (p, IDENTIFIER, OPERATOR, STOP))
	{
	  if (TAX (p) != NO_TAG && TAG_LEX_LEVEL (TAX (p)) != PRIMAL_SCOPE)
	    scope_add (s, p, scope_make_tuple (TAG_LEX_LEVEL (TAX (p)), NOT_TRANSIENT));
	}
      else if (IS (p, DECLARER))
	get_declarer_elements (p, s, true);
      else
	gather_scopes_for_youngest (SUB (p), s);
  }
}

/* Get_youngest_environs.  */

static void
get_youngest_environs (NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (a68_is_one_of (p, ROUTINE_TEXT, FORMAT_TEXT, STOP))
	{
	  SCOPE_T *s = NO_SCOPE;
	  TUPLE_T tup;
	  gather_scopes_for_youngest (SUB (p), &s);
	  tup = scope_find_youngest_outside (s, LEX_LEVEL (p));
	  YOUNGEST_ENVIRON (TAX (p)) = LEVEL (&tup);
	}
      else
	get_youngest_environs (SUB (p));
  }
}

/* Bind_scope_to_tag.  */

static void
bind_scope_to_tag (NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (IS (p, DEFINING_IDENTIFIER) && MOID (p) == M_FORMAT)
	{
	  if (IS (NEXT_NEXT (p), FORMAT_TEXT))
	    {
	      SCOPE (TAX (p)) = YOUNGEST_ENVIRON (TAX (NEXT_NEXT (p)));
	      SCOPE_ASSIGNED (TAX (p)) = true;
	    }
	  return;
	}
      else if (IS (p, DEFINING_IDENTIFIER))
	{
	  if (IS (NEXT_NEXT (p), ROUTINE_TEXT))
	    {
	      SCOPE (TAX (p)) = YOUNGEST_ENVIRON (TAX (NEXT_NEXT (p)));
	      SCOPE_ASSIGNED (TAX (p)) = true;
	    }
	  return;
	}
      else
	bind_scope_to_tag (SUB (p));
  }
}

/* Bind_scope_to_tags.  */

static void
bind_scope_to_tags (NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (a68_is_one_of (p, PROCEDURE_DECLARATION, IDENTITY_DECLARATION, STOP))
	bind_scope_to_tag (SUB (p));
      else
	bind_scope_to_tags (SUB (p));
    }
}

/* Scope_bounds.  */

static void
scope_bounds (NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (IS (p, UNIT))
	scope_statement (p, NO_VAR);
      else
	scope_bounds (SUB (p));
    }
}

/* Scope_declarer.  */

static void
scope_declarer (NODE_T *p)
{
  if (p != NO_NODE)
    {
      if (IS (p, BOUNDS))
	scope_bounds (SUB (p));
      else if (IS (p, INDICANT))
	;
      else if (IS_REF (p))
	scope_declarer (NEXT (p));
      else if (a68_is_one_of (p, PROC_SYMBOL, UNION_SYMBOL, STOP))
	;
      else
	{
	  scope_declarer (SUB (p));
	  scope_declarer (NEXT (p));
	}
    }
}

/* Scope_identity_declaration.  */

static void
scope_identity_declaration (NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      scope_identity_declaration (SUB (p));

      if (IS (p, DEFINING_IDENTIFIER))
	{
	  NODE_T *unit = NEXT_NEXT (p);
	  SCOPE_T *s = NO_SCOPE;
	  TUPLE_T tup;
	  int z = PRIMAL_SCOPE;

	  if (ATTRIBUTE (MOID (TAX (p))) != PROC_SYMBOL)
	    check_identifier_usage (TAX (p), unit);
	  scope_statement (unit, &s);
	  (void) scope_check (s, TRANSIENT, LEX_LEVEL (p));
	  tup = scope_find_youngest (s);
	  z = LEVEL (&tup);
	  if (z < LEX_LEVEL (p))
	    {
	      SCOPE (TAX (p)) = z;
	      SCOPE_ASSIGNED (TAX (p)) = true;
	    }
	  return;
	}
    }
}

/* Scope_variable_declaration.  */

static void
scope_variable_declaration (NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      scope_variable_declaration (SUB (p));
      if (IS (p, DECLARER))
	scope_declarer (SUB (p));
      else if (IS (p, DEFINING_IDENTIFIER))
	{
	  if (a68_whether (p, DEFINING_IDENTIFIER, ASSIGN_SYMBOL, UNIT, STOP))
	    {
	      NODE_T *unit = NEXT_NEXT (p);
	      SCOPE_T *s = NO_SCOPE;
	      check_identifier_usage (TAX (p), unit);
	      scope_statement (unit, &s);
	      (void) scope_check (s, TRANSIENT, LEX_LEVEL (p));
	      return;
	    }
	}
    }
}

/* Scope_procedure_declaration.  */

static void
scope_procedure_declaration (NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      scope_procedure_declaration (SUB (p));

      if (a68_is_one_of (p, DEFINING_IDENTIFIER, DEFINING_OPERATOR, STOP))
	{
	  NODE_T *unit = NEXT_NEXT (p);
	  SCOPE_T *s = NO_SCOPE;

	  scope_statement (unit, &s);
	  (void) scope_check (s, NOT_TRANSIENT, LEX_LEVEL (p));
	  return;
	}
    }
}

/* Scope_declaration_list.  */

static void
scope_declaration_list (NODE_T *p)
{
  if (p != NO_NODE)
    {
      if (IS (p, IDENTITY_DECLARATION))
	scope_identity_declaration (SUB (p));
      else if (IS (p, VARIABLE_DECLARATION))
	scope_variable_declaration (SUB (p));
      else if (IS (p, MODE_DECLARATION))
	scope_declarer (SUB (p));
      else if (IS (p, PRIORITY_DECLARATION))
	;
      else if (IS (p, PROCEDURE_DECLARATION))
       scope_procedure_declaration (SUB (p));
      else if (IS (p, PROCEDURE_VARIABLE_DECLARATION))
	scope_procedure_declaration (SUB (p));
      else if (a68_is_one_of (p, BRIEF_OPERATOR_DECLARATION, OPERATOR_DECLARATION, STOP))
	scope_procedure_declaration (SUB (p));
      else
	{
	  scope_declaration_list (SUB (p));
	  scope_declaration_list (NEXT (p));
	}
    }
}

/* Scope_arguments.  */

static void
scope_arguments (NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (IS (p, UNIT))
	{
	  SCOPE_T *s = NO_SCOPE;
	  scope_statement (p, &s);
	  (void) scope_check (s, TRANSIENT, LEX_LEVEL (p));
	}
      else
	scope_arguments (SUB (p));
  }
}

/* Is_coercion.  */

static bool
is_coercion (NODE_T *p)
{
  if (p != NO_NODE)
    {
      switch (ATTRIBUTE (p))
	{
	case DEPROCEDURING:
	case DEREFERENCING:
	case UNITING:
	case ROWING:
	case WIDENING:
	case VOIDING:
	case PROCEDURING:
	  return true;
	default:
	  return false;
	}
    }
  else
    return false;
}

/* Scope_coercion.  */

static void
scope_coercion (NODE_T *p, SCOPE_T **s)
{
  if (is_coercion (p))
    {
      if (IS (p, VOIDING))
	scope_coercion (SUB (p), NO_VAR);
      else if (IS (p, DEREFERENCING))
	/* Leave this to the dynamic scope checker.  */
	scope_coercion (SUB (p), NO_VAR);
      else if (IS (p, DEPROCEDURING))
	scope_coercion (SUB (p), NO_VAR);
      else if (IS (p, ROWING))
	{
	  SCOPE_T *z = NO_SCOPE;

	  scope_coercion (SUB (p), &z);
	  (void) scope_check (z, TRANSIENT, LEX_LEVEL (p));
	  if (IS_REF_FLEX (MOID (SUB (p))))
	    scope_add (s, p, scope_make_tuple (LEX_LEVEL (p), TRANSIENT));
	  else
	    scope_add (s, p, scope_make_tuple (LEX_LEVEL (p), NOT_TRANSIENT));
	}
      else if (IS (p, PROCEDURING))
	{
	  /* Can only be a JUMP.  */
	  NODE_T *q = SUB_SUB (p);
	  if (IS (q, GOTO_SYMBOL))
	    FORWARD (q);

	  scope_add (s, q, scope_make_tuple (TAG_LEX_LEVEL (TAX (q)), NOT_TRANSIENT));
	}
      else if (IS (p, UNITING))
	{
	  SCOPE_T *z = NO_SCOPE;

	  scope_coercion (SUB (p), &z);
	  if (z != NO_SCOPE)
	    {
	      (void) scope_check (z, TRANSIENT, LEX_LEVEL (p));
	      scope_add (s, p, scope_find_youngest (z));
	    }
	}
      else
	scope_coercion (SUB (p), s);
    }
  else
    scope_statement (p, s);
}

/* Scope_format_text.  */

static void
scope_format_text (NODE_T *p, SCOPE_T **s)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (IS (p, FORMAT_PATTERN))
	scope_enclosed_clause (SUB (NEXT_SUB (p)), s);
      else if (IS (p, FORMAT_ITEM_G) && NEXT (p) != NO_NODE)
	scope_enclosed_clause (SUB_NEXT (p), s);
      else if (IS (p, DYNAMIC_REPLICATOR))
	scope_enclosed_clause (SUB (NEXT_SUB (p)), s);
      else
	scope_format_text (SUB (p), s);
    }
}

/* Scope_operand.  */

static void
scope_operand (NODE_T *p, SCOPE_T **s)
{
  if (IS (p, MONADIC_FORMULA))
    scope_operand (NEXT_SUB (p), s);
  else if (IS (p, FORMULA))
    scope_formula (p, s);
  else if (IS (p, SECONDARY))
    scope_statement (SUB (p), s);
}

/* Scope_formula.  */

static void
scope_formula (NODE_T *p, SCOPE_T **s)
{
  NODE_T *q = SUB (p);
  SCOPE_T *s2 = NO_SCOPE;

  scope_operand (q, &s2);
  (void) scope_check (s2, TRANSIENT, LEX_LEVEL (p));
  if (NEXT (q) != NO_NODE)
    {
      SCOPE_T *s3 = NO_SCOPE;
      scope_operand (NEXT_NEXT (q), &s3);
      (void) scope_check (s3, TRANSIENT, LEX_LEVEL (p));
    }
  (void) s;
}

/* Scope_routine_text.  */

static void
scope_routine_text (NODE_T *p, SCOPE_T **s)
{
  NODE_T *q = SUB (p);
  NODE_T *routine = (IS (q, PARAMETER_PACK) ? NEXT (q) : q);
  SCOPE_T *x = NO_SCOPE;

  scope_statement (NEXT_NEXT (routine), &x);
  (void) scope_check (x, TRANSIENT, LEX_LEVEL (p));
  TUPLE_T routine_tuple = scope_make_tuple (YOUNGEST_ENVIRON (TAX (p)), NOT_TRANSIENT);
  scope_add (s, p, routine_tuple);
}

/* Scope_statement.  */

static void
scope_statement (NODE_T *p, SCOPE_T **s)
{
  if (is_coercion (p))
    scope_coercion (p, s);
  else if (a68_is_one_of (p, PRIMARY, SECONDARY, TERTIARY, UNIT, STOP))
    scope_statement (SUB (p), s);
  else if (a68_is_one_of (p, NIHIL, STOP))
    scope_add (s, p, scope_make_tuple (PRIMAL_SCOPE, NOT_TRANSIENT));
  else if (IS (p, DENOTATION))
    ;
  else if (IS (p, IDENTIFIER))
    {
      if (IS_REF (MOID (p)))
	{
	  if (PRIO (TAX (p)) == PARAMETER_IDENTIFIER)
	    scope_add (s, p, scope_make_tuple (TAG_LEX_LEVEL (TAX (p)) - 1, NOT_TRANSIENT));
	  else
	    {
	      if (HEAP (TAX (p)) == HEAP_SYMBOL)
		scope_add (s, p, scope_make_tuple (PRIMAL_SCOPE, NOT_TRANSIENT));
	      else if (SCOPE_ASSIGNED (TAX (p)))
		scope_add (s, p, scope_make_tuple (SCOPE (TAX (p)), NOT_TRANSIENT));
	      else
		scope_add (s, p, scope_make_tuple (TAG_LEX_LEVEL (TAX (p)), NOT_TRANSIENT));
	    }
	}
      else if (ATTRIBUTE (MOID (p)) == PROC_SYMBOL && SCOPE_ASSIGNED (TAX (p)) == true)
	scope_add (s, p, scope_make_tuple (SCOPE (TAX (p)), NOT_TRANSIENT));
      else if (MOID (p) == M_FORMAT && SCOPE_ASSIGNED (TAX (p)) == true)
	scope_add (s, p, scope_make_tuple (SCOPE (TAX (p)), NOT_TRANSIENT));
    }
  else if (IS (p, ENCLOSED_CLAUSE))
    scope_enclosed_clause (SUB (p), s);
  else if (IS (p, CALL))
    {
      SCOPE_T *x = NO_SCOPE;

      scope_statement (SUB (p), &x);
      (void) scope_check (x, NOT_TRANSIENT, LEX_LEVEL (p));
      scope_arguments (NEXT_SUB (p));
    }
  else if (IS (p, SLICE))
    {
      SCOPE_T *x = NO_SCOPE;
      MOID_T *m = MOID (SUB (p));

      if (IS_REF (m))
	{
	  if (ATTRIBUTE (SUB (p)) == PRIMARY && ATTRIBUTE (SUB_SUB (p)) == SLICE)
	    scope_statement (SUB (p), s);
	  else
	    {
	      scope_statement (SUB (p), &x);
	      (void) scope_check (x, NOT_TRANSIENT, LEX_LEVEL (p));
	    }
	  if (IS_FLEX (SUB (m)))
	    scope_add (s, SUB (p), scope_make_tuple (LEX_LEVEL (p), TRANSIENT));
	  scope_bounds (SUB (NEXT_SUB (p)));
	}
      if (IS_REF (MOID (p)))
	scope_add (s, p, scope_find_youngest (x));
    }
  else if (IS (p, FORMAT_TEXT))
    {
      SCOPE_T *x = NO_SCOPE;
      scope_format_text (SUB (p), &x);
      scope_add (s, p, scope_find_youngest (x));
    }
  else if (IS (p, CAST))
    {
      SCOPE_T *x = NO_SCOPE;
      scope_enclosed_clause (SUB (NEXT_SUB (p)), &x);
      (void) scope_check (x, NOT_TRANSIENT, LEX_LEVEL (p));
      scope_add (s, p, scope_find_youngest (x));
    }
  else if (IS (p, SELECTION))
    {
      SCOPE_T *ns = NO_SCOPE;
      scope_statement (NEXT_SUB (p), &ns);
      (void) scope_check (ns, NOT_TRANSIENT, LEX_LEVEL (p));
      if (a68_is_ref_refety_flex (MOID (NEXT_SUB (p))))
	scope_add (s, p, scope_make_tuple (LEX_LEVEL (p), TRANSIENT));
      scope_add (s, p, scope_find_youngest (ns));
  }
  else if (IS (p, GENERATOR))
    {
      if (IS (SUB (p), LOC_SYMBOL))
	{
	  if (NON_LOCAL (p) != NO_TABLE)
	    scope_add (s, p, scope_make_tuple (LEVEL (NON_LOCAL (p)), NOT_TRANSIENT));
	  else
	    scope_add (s, p, scope_make_tuple (LEX_LEVEL (p), NOT_TRANSIENT));
	}
      else
	scope_add (s, p, scope_make_tuple (PRIMAL_SCOPE, NOT_TRANSIENT));
      scope_declarer (SUB (NEXT_SUB (p)));
    }
  else if (IS (p, FORMULA))
    scope_formula (p, s);
  else if (IS (p, ASSIGNATION))
    {
      NODE_T *unit = NEXT (NEXT_SUB (p));
      SCOPE_T *ns = NO_SCOPE, *nd = NO_SCOPE;
      TUPLE_T tup;
      scope_statement (SUB_SUB (p), &nd);
      scope_statement (unit, &ns);
      (void) scope_check_multiple (ns, TRANSIENT, nd);
      tup = scope_find_youngest (nd);
      scope_add (s, p, scope_make_tuple (LEVEL (&tup), NOT_TRANSIENT));
    }
  else if (IS (p, ROUTINE_TEXT))
    scope_routine_text (p, s);
  else if (a68_is_one_of (p, IDENTITY_RELATION, AND_FUNCTION, OR_FUNCTION, STOP))
    {
      SCOPE_T *n = NO_SCOPE;
      scope_statement (SUB (p), &n);
      scope_statement (NEXT (NEXT_SUB (p)), &n);
      (void) scope_check (n, NOT_TRANSIENT, LEX_LEVEL (p));
    }
  else if (IS (p, ASSERTION))
    {
      SCOPE_T *n = NO_SCOPE;
      scope_enclosed_clause (SUB (NEXT_SUB (p)), &n);
      (void) scope_check (n, NOT_TRANSIENT, LEX_LEVEL (p));
    }
  else if (a68_is_one_of (p, JUMP, SKIP, STOP))
    {
      ;
    }
}

/* Scope_statement_list.  */

static void
scope_statement_list (NODE_T *p, SCOPE_T **s)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (IS (p, UNIT))
	scope_statement (p, s);
      else
	scope_statement_list (SUB (p), s);
    }
}

/* Scope_serial_clause.  */

static void
scope_serial_clause (NODE_T *p, SCOPE_T **s, bool terminator)
{
  if (p != NO_NODE)
    {
      if (IS (p, INITIALISER_SERIES))
	{
	  scope_serial_clause (SUB (p), s, false);
	  scope_serial_clause (NEXT (p), s, terminator);
	}
      else if (IS (p, DECLARATION_LIST))
	scope_declaration_list (SUB (p));
      else if (a68_is_one_of (p, LABEL, SEMI_SYMBOL, EXIT_SYMBOL, STOP))
	scope_serial_clause (NEXT (p), s, terminator);
      else if (a68_is_one_of (p, SERIAL_CLAUSE, ENQUIRY_CLAUSE, STOP))
	{
	  if (NEXT (p) != NO_NODE)
	    {
	      int j = ATTRIBUTE (NEXT (p));
	      if (j == EXIT_SYMBOL || j == END_SYMBOL || j == CLOSE_SYMBOL)
		scope_serial_clause (SUB (p), s, true);
	      else
		scope_serial_clause (SUB (p), s, false);
	    }
	  else
	    scope_serial_clause (SUB (p), s, true);
	  scope_serial_clause (NEXT (p), s, terminator);
	}
      else if (IS (p, LABELED_UNIT))
	scope_serial_clause (SUB (p), s, terminator);
      else if (IS (p, UNIT))
	{
	  if (terminator)
	    scope_statement (p, s);
	  else
	    scope_statement (p, NO_VAR);
	}
    }
}

/* Scope_closed_clause.  */

static void
scope_closed_clause (NODE_T *p, SCOPE_T **s)
{
  if (p != NO_NODE)
    {
      if (IS (p, SERIAL_CLAUSE))
	scope_serial_clause (p, s, true);
      else if (a68_is_one_of (p, OPEN_SYMBOL, BEGIN_SYMBOL, STOP))
	scope_closed_clause (NEXT (p), s);
    }
}

/* Scope_collateral_clause.  */

static void
scope_collateral_clause (NODE_T *p, SCOPE_T **s)
{
  if (p != NO_NODE)
    {
      if (!(a68_whether (p, BEGIN_SYMBOL, END_SYMBOL, STOP)
	    || a68_whether (p, OPEN_SYMBOL, CLOSE_SYMBOL, STOP)))
	{
	  scope_statement_list (p, s);
	}
    }
}

/* Scope_conditional_clause.  */

static void
scope_conditional_clause (NODE_T *p, SCOPE_T **s)
{
  scope_serial_clause (NEXT_SUB (p), NO_VAR, true);
  FORWARD (p);
  scope_serial_clause (NEXT_SUB (p), s, true);
  if ((FORWARD (p)) != NO_NODE)
    {
      if (a68_is_one_of (p, ELSE_PART, CHOICE, STOP))
	scope_serial_clause (NEXT_SUB (p), s, true);
      else if (a68_is_one_of (p, ELIF_PART, BRIEF_ELIF_PART, STOP))
	scope_conditional_clause (SUB (p), s);
    }
}

/* Scope_case_clause.  */

static void
scope_case_clause (NODE_T *p, SCOPE_T **s)
{
  SCOPE_T *n = NO_SCOPE;
  scope_serial_clause (NEXT_SUB (p), &n, true);
  (void) scope_check (n, NOT_TRANSIENT, LEX_LEVEL (p));
  FORWARD (p);
  scope_statement_list (NEXT_SUB (p), s);
  if ((FORWARD (p)) != NO_NODE)
    {
      if (a68_is_one_of (p, OUT_PART, CHOICE, STOP))
	scope_serial_clause (NEXT_SUB (p), s, true);
      else if (a68_is_one_of (p, CASE_OUSE_PART, BRIEF_OUSE_PART, STOP))
	scope_case_clause (SUB (p), s);
      else if (a68_is_one_of (p, CONFORMITY_OUSE_PART, BRIEF_CONFORMITY_OUSE_PART, STOP))
	scope_case_clause (SUB (p), s);
    }
}

/* Scope_loop_clause.  */

static void
scope_loop_clause (NODE_T *p)
{
  if (p != NO_NODE)
    {
      if (IS (p, FOR_PART))
	scope_loop_clause (NEXT (p));
      else if (a68_is_one_of (p, FROM_PART, BY_PART, TO_PART, STOP))
	{
	  scope_statement (NEXT_SUB (p), NO_VAR);
	  scope_loop_clause (NEXT (p));
	}
      else if (IS (p, WHILE_PART))
	{
	  scope_serial_clause (NEXT_SUB (p), NO_VAR, true);
	  scope_loop_clause (NEXT (p));
	}
      else if (a68_is_one_of (p, DO_PART, ALT_DO_PART, STOP))
	{
	  NODE_T *do_p = NEXT_SUB (p);

	  if (IS (do_p, SERIAL_CLAUSE))
	    scope_serial_clause (do_p, NO_VAR, true);
	}
    }
}

/* Scope_enclosed_clause.  */

static void
scope_enclosed_clause (NODE_T *p, SCOPE_T **s)
{
  if (IS (p, ENCLOSED_CLAUSE))
    scope_enclosed_clause (SUB (p), s);
  else if (IS (p, CLOSED_CLAUSE))
    scope_closed_clause (SUB (p), s);
  else if (a68_is_one_of (p, COLLATERAL_CLAUSE, PARALLEL_CLAUSE, STOP))
    scope_collateral_clause (SUB (p), s);
  else if (IS (p, CONDITIONAL_CLAUSE))
    scope_conditional_clause (SUB (p), s);
  else if (a68_is_one_of (p, CASE_CLAUSE, CONFORMITY_CLAUSE, STOP))
    scope_case_clause (SUB (p), s);
  else if (IS (p, LOOP_CLAUSE))
    scope_loop_clause (SUB (p));
}

/* Whether a symbol table contains no (anonymous) definition.  */

static bool
empty_table (TABLE_T * t)
{
  if (IDENTIFIERS (t) == NO_TAG)
    return (OPERATORS (t) == NO_TAG && INDICANTS (t) == NO_TAG);
  else if (PRIO (IDENTIFIERS (t)) == LOOP_IDENTIFIER && NEXT (IDENTIFIERS (t)) == NO_TAG)
    return (OPERATORS (t) == NO_TAG && INDICANTS (t) == NO_TAG);
  else if (PRIO (IDENTIFIERS (t)) == SPECIFIER_IDENTIFIER && NEXT (IDENTIFIERS (t)) == NO_TAG)
    return (OPERATORS (t) == NO_TAG && INDICANTS (t) == NO_TAG);
  else
    return false;
}

/* Indicate non-local environs.  */

static void
get_non_local_environs (NODE_T *p, int max)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (IS (p, ROUTINE_TEXT))
	get_non_local_environs (SUB (p), LEX_LEVEL (SUB (p)));
      else if (IS (p, FORMAT_TEXT))
	get_non_local_environs (SUB (p), LEX_LEVEL (SUB (p)));
      else
	{
	  get_non_local_environs (SUB (p), max);
	  NON_LOCAL (p) = NO_TABLE;
	  if (TABLE (p) != NO_TABLE)
	    {
	      TABLE_T *q = TABLE (p);
	      while (q != NO_TABLE && empty_table (q)
		     && PREVIOUS (q) != NO_TABLE && LEVEL (PREVIOUS (q)) >= max)
		{
		  NON_LOCAL (p) = PREVIOUS (q);
		  q = PREVIOUS (q);
		}
	    }
	}
    }
}

/* The static scope checker.  */

void
a68_scope_checker (NODE_T *p)
{
  if (SUB (p))
    {
      if (IS (SUB (p), PARTICULAR_PROGRAM))
	p = SUB (p);
      else if (IS (SUB (p), PRELUDE_PACKET))
	{
	  /* XXX writeme.  */
	  return;
	}
    }

  /* Establish scopes of routine texts and format texts.  */
  get_youngest_environs (p);
  /* Find non-local environs.  */
  get_non_local_environs (p, PRIMAL_SCOPE);
  /* PROC and FORMAT identities can now be assigned a scope.  */
  bind_scope_to_tags (p);
  /* Now check evertyhing else.  */
  scope_enclosed_clause (SUB (p), NO_VAR);
}
