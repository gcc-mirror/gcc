/* Mode coercion driver.
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

#define A68_INSERT_COERCIONS(n, p, q) a68_make_strong ((n), (p), MOID (q))

/* A few forward references of functions defined below.  */

static void coerce_unit (NODE_T *p, SOID_T *q);
static void coerce_formula (NODE_T *p, SOID_T *q __attribute__ ((unused)));
static void coerce_operand (NODE_T *p, SOID_T *q);
static void coerce_enclosed (NODE_T *p, SOID_T *q);

/* Coerce bounds.  */

static void
coerce_bounds (NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (IS (p, UNIT))
	{
	  SOID_T q;
	  a68_make_soid (&q, MEEK, M_INT, 0);
	  coerce_unit (p, &q);
	}
      else
	coerce_bounds (SUB (p));
    }
}

/* Coerce declarer.  */

static void
coerce_declarer (NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (IS (p, BOUNDS))
	coerce_bounds (SUB (p));
      else
	coerce_declarer (SUB (p));
    }
}

/* Coerce identity declaration.  */

static void
coerce_identity_declaration (NODE_T *p)
{
  if (p != NO_NODE)
    {
      switch (ATTRIBUTE (p))
	{
	case DECLARER:
	  coerce_declarer (SUB (p));
	  coerce_identity_declaration (NEXT (p));
        break;
	case DEFINING_IDENTIFIER:
	  {
	    SOID_T q;
	    a68_make_soid (&q, STRONG, MOID (p), 0);
	    coerce_unit (NEXT_NEXT (p), &q);
	    break;
	  }
	default:
	  coerce_identity_declaration (SUB (p));
	  coerce_identity_declaration (NEXT (p));
	  break;
	}
    }
}

/* Coerce variable declaration.  */

static void
coerce_variable_declaration (NODE_T *p)
{
  if (p != NO_NODE)
    {
      switch (ATTRIBUTE (p))
	{
	case DECLARER:
	  coerce_declarer (SUB (p));
	  coerce_variable_declaration (NEXT (p));
	  break;
	case DEFINING_IDENTIFIER:
	  if (a68_whether (p, DEFINING_IDENTIFIER, ASSIGN_SYMBOL, UNIT, STOP))
	    {
	      SOID_T q;
	      a68_make_soid (&q, STRONG, SUB_MOID (p), 0);
	      coerce_unit (NEXT_NEXT (p), &q);
	      break;
	    }
	  /* Fallthrough.  */
	default:
	  coerce_variable_declaration (SUB (p));
	  coerce_variable_declaration (NEXT (p));
	  break;
	}
    }
}

/* Coerce routine text.  */

static void
coerce_routine_text (NODE_T *p)
{
  if (IS (p, PARAMETER_PACK))
    FORWARD (p);
  SOID_T w;
  a68_make_soid (&w, STRONG, MOID (p), 0);
  coerce_unit (NEXT_NEXT (p), &w);
}

/* Coerce proc declaration.  */

static void
coerce_proc_declaration (NODE_T *p)
{
  if (p == NO_NODE)
    return;
  else if (IS (p, ROUTINE_TEXT))
    coerce_routine_text (SUB (p));
  else
    {
      coerce_proc_declaration (SUB (p));
      coerce_proc_declaration (NEXT (p));
    }
}

/* Coerce_op_declaration.  */

static void
coerce_op_declaration (NODE_T *p)
{
  if (p == NO_NODE)
    return;
  else if (IS (p, DEFINING_OPERATOR))
    {
      SOID_T q;
      a68_make_soid (&q, STRONG, MOID (p), 0);
      coerce_unit (NEXT_NEXT (p), &q);
    }
  else
    {
      coerce_op_declaration (SUB (p));
      coerce_op_declaration (NEXT (p));
    }
}

/* Coerce brief op declaration.  */

static void
coerce_brief_op_declaration (NODE_T *p)
{
  if (p == NO_NODE)
    return;
  else if (IS (p, DEFINING_OPERATOR))
    coerce_routine_text (SUB (NEXT_NEXT (p)));
  else
    {
      coerce_brief_op_declaration (SUB (p));
      coerce_brief_op_declaration (NEXT (p));
    }
}

/* Coerce declaration list.  */

static void
coerce_declaration_list (NODE_T *p)
{
  if (p != NO_NODE)
    {
      switch (ATTRIBUTE (p))
	{
	case IDENTITY_DECLARATION:
	  coerce_identity_declaration (SUB (p));
	  break;
	case VARIABLE_DECLARATION:
	  coerce_variable_declaration (SUB (p));
	  break;
	case MODE_DECLARATION:
	  coerce_declarer (SUB (p));
	  break;
	case PROCEDURE_DECLARATION:
	case PROCEDURE_VARIABLE_DECLARATION:
	  coerce_proc_declaration (SUB (p));
	  break;
	case BRIEF_OPERATOR_DECLARATION:
	  coerce_brief_op_declaration (SUB (p));
	  break;
	case OPERATOR_DECLARATION:
	  coerce_op_declaration (SUB (p));
	  break;
	default:
	  coerce_declaration_list (SUB (p));
	  coerce_declaration_list (NEXT (p));
	  break;
	}
    }
}

/* Coerce serial.  */

static void
coerce_serial (NODE_T *p, SOID_T *q, bool k)
{
  if (p == NO_NODE)
    return;
  else if (IS (p, INITIALISER_SERIES))
    {
      coerce_serial (SUB (p), q, false);
      coerce_serial (NEXT (p), q, k);
    }
  else if (IS (p, DECLARATION_LIST))
    coerce_declaration_list (SUB (p));
  else if (a68_is_one_of (p, LABEL, SEMI_SYMBOL, EXIT_SYMBOL, STOP))
    coerce_serial (NEXT (p), q, k);
  else if (a68_is_one_of (p, SERIAL_CLAUSE, ENQUIRY_CLAUSE, STOP))
    {
      NODE_T *z = NEXT (p);
      if (z != NO_NODE)
	{
	  if (IS (z, EXIT_SYMBOL) || IS (z, END_SYMBOL) || IS (z, CLOSE_SYMBOL))
	    coerce_serial (SUB (p), q, true);
	  else
	    coerce_serial (SUB (p), q, false);
	}
      else
	coerce_serial (SUB (p), q, true);
      coerce_serial (NEXT (p), q, k);
    }
  else if (IS (p, LABELED_UNIT))
    coerce_serial (SUB (p), q, k);
  else if (IS (p, UNIT))
    {
      if (k)
	coerce_unit (p, q);
      else
	{
	  SOID_T strongvoid;
	  a68_make_soid (&strongvoid, STRONG, M_VOID, 0);
	  coerce_unit (p, &strongvoid);
	}
    }
}

/* Coerce closed.  */

static void
coerce_closed (NODE_T *p, SOID_T *q)
{
  if (IS (p, SERIAL_CLAUSE))
    coerce_serial (p, q, true);
  else if (a68_is_one_of (p, OPEN_SYMBOL, BEGIN_SYMBOL, STOP))
    coerce_closed (NEXT (p), q);
}

/* Coerce access clause.  */

static void
coerce_access (NODE_T *p, SOID_T *q)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (IS (p, ENCLOSED_CLAUSE))
	coerce_enclosed (p, q);
    }
}

/* Coerce conditional.  */

static void
coerce_conditional (NODE_T *p, SOID_T *q)
{
  SOID_T w;
  a68_make_soid (&w, MEEK, M_BOOL, 0);
  coerce_serial (NEXT_SUB (p), &w, true);
  FORWARD (p);
  coerce_serial (NEXT_SUB (p), q, true);
  if ((FORWARD (p)) != NO_NODE)
    {
      if (a68_is_one_of (p, ELSE_PART, CHOICE, STOP))
	coerce_serial (NEXT_SUB (p), q, true);
      else if (a68_is_one_of (p, ELIF_PART, BRIEF_ELIF_PART, STOP))
	coerce_conditional (SUB (p), q);
    }
}

/* Coerce unit list.  */

static void
coerce_unit_list (NODE_T *p, SOID_T *q)
{
  if (p == NO_NODE)
    return;
  else if (IS (p, UNIT_LIST))
    {
      coerce_unit_list (SUB (p), q);
      coerce_unit_list (NEXT (p), q);
    }
  else if (a68_is_one_of (p, OPEN_SYMBOL, BEGIN_SYMBOL, COMMA_SYMBOL, STOP))
    coerce_unit_list (NEXT (p), q);
  else if (IS (p, UNIT))
    {
      coerce_unit (p, q);
      coerce_unit_list (NEXT (p), q);
    }
}

/* Coerce int case.  */

static void
coerce_int_case (NODE_T *p, SOID_T *q)
{
  SOID_T w;
  a68_make_soid (&w, MEEK, M_INT, 0);
  coerce_serial (NEXT_SUB (p), &w, true);
  FORWARD (p);
  coerce_unit_list (NEXT_SUB (p), q);
  if ((FORWARD (p)) != NO_NODE)
    {
      if (a68_is_one_of (p, OUT_PART, CHOICE, STOP))
	coerce_serial (NEXT_SUB (p), q, true);
      else if (a68_is_one_of (p, CASE_OUSE_PART, BRIEF_OUSE_PART, STOP))
	coerce_int_case (SUB (p), q);
    }
}

/* Coerce spec unit list.  */

static void
coerce_spec_unit_list (NODE_T *p, SOID_T *q)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (a68_is_one_of (p, SPECIFIED_UNIT_LIST, SPECIFIED_UNIT, STOP))
	coerce_spec_unit_list (SUB (p), q);
      else if (IS (p, UNIT))
	coerce_unit (p, q);
    }
}

/* Coerce united case.  */

static void
coerce_united_case (NODE_T *p, SOID_T *q)
{
  SOID_T w;
  a68_make_soid (&w, MEEK, MOID (SUB (p)), 0);
  coerce_serial (NEXT_SUB (p), &w, true);
  FORWARD (p);
  coerce_spec_unit_list (NEXT_SUB (p), q);
  if ((FORWARD (p)) != NO_NODE)
    {
      if (a68_is_one_of (p, OUT_PART, CHOICE, STOP))
	coerce_serial (NEXT_SUB (p), q, true);
      else if (a68_is_one_of (p, CONFORMITY_OUSE_PART, BRIEF_CONFORMITY_OUSE_PART, STOP))
	coerce_united_case (SUB (p), q);
    }
}

/* Coerce loop.  */

static void
coerce_loop (NODE_T *p)
{
  if (IS (p, FOR_PART))
    coerce_loop (NEXT (p));
  else if (a68_is_one_of (p, FROM_PART, BY_PART, TO_PART, STOP))
    {
      SOID_T w;
      a68_make_soid (&w, MEEK, M_INT, 0);
      coerce_unit (NEXT_SUB (p), &w);
      coerce_loop (NEXT (p));
    }
  else if (IS (p, WHILE_PART))
    {
      SOID_T w;
      a68_make_soid (&w, MEEK, M_BOOL, 0);
      coerce_serial (NEXT_SUB (p), &w, true);
      coerce_loop (NEXT (p));
    }
  else if (a68_is_one_of (p, DO_PART, ALT_DO_PART, STOP))
    {
      SOID_T w;
      NODE_T *do_p = NEXT_SUB (p);
      a68_make_soid (&w, STRONG, M_VOID, 0);
      coerce_serial (do_p, &w, true);
    }
}

/* Coerce struct display.  */

static void
coerce_struct_display (PACK_T **r, NODE_T *p)
{
  if (p == NO_NODE)
    return;
  else if (IS (p, UNIT_LIST))
    {
      coerce_struct_display (r, SUB (p));
      coerce_struct_display (r, NEXT (p));
    }
  else if (a68_is_one_of (p, OPEN_SYMBOL, BEGIN_SYMBOL, COMMA_SYMBOL, STOP))
    coerce_struct_display (r, NEXT (p));
  else if (IS (p, UNIT))
    {
      SOID_T s;
      a68_make_soid (&s, STRONG, MOID (*r), 0);
      coerce_unit (p, &s);
      FORWARD (*r);
      coerce_struct_display (r, NEXT (p));
    }
}

/* Coerce collateral.  */

static void
coerce_collateral (NODE_T *p, SOID_T *q)
{
  if (!(a68_whether (p, BEGIN_SYMBOL, END_SYMBOL, STOP)
	|| a68_whether (p, OPEN_SYMBOL, CLOSE_SYMBOL, STOP)))
    {
      if (IS (MOID (q), STRUCT_SYMBOL))
	{
	  PACK_T *t = PACK (MOID (q));
	  coerce_struct_display (&t, p);
	}
      else if (IS_FLEX (MOID (q)))
	{
	  SOID_T w;
	  a68_make_soid (&w, STRONG, SLICE (SUB_MOID (q)), 0);
	  coerce_unit_list (p, &w);
	}
      else if (IS_ROW (MOID (q)))
	{
	  SOID_T w;
	  a68_make_soid (&w, STRONG, SLICE (MOID (q)), 0);
	  coerce_unit_list (p, &w);
	}
      else
	{
	  /* if (MOID (q) != M_VOID).  */
	  coerce_unit_list (p, q);
	}
    }
}

/* Coerce_enclosed.  */

static void
coerce_enclosed (NODE_T *p, SOID_T *q)
{
  if (IS (p, ENCLOSED_CLAUSE))
    coerce_enclosed (SUB (p), q);
  else if (IS (p, CLOSED_CLAUSE))
    coerce_closed (SUB (p), q);
  else if (IS (p, COLLATERAL_CLAUSE))
    coerce_collateral (SUB (p), q);
  else if (IS (p, ACCESS_CLAUSE))
    coerce_access (SUB (p), q);
  else if (IS (p, PARALLEL_CLAUSE))
    coerce_collateral (SUB (NEXT_SUB (p)), q);
  else if (IS (p, CONDITIONAL_CLAUSE))
    coerce_conditional (SUB (p), q);
  else if (IS (p, CASE_CLAUSE))
    coerce_int_case (SUB (p), q);
  else if (IS (p, CONFORMITY_CLAUSE))
    coerce_united_case (SUB (p), q);
  else if (IS (p, LOOP_CLAUSE))
    coerce_loop (SUB (p));

  MOID (p) = a68_depref_rows (MOID (p), MOID (q));
}

/* Get monad moid.  */

static MOID_T *
get_monad_moid (NODE_T *p)
{
  if (TAX (p) != NO_TAG && TAX (p) != A68_PARSER (error_tag))
    {
      MOID (p) = MOID (TAX (p));
      return MOID (PACK (MOID (p)));
    }
  else
    return M_ERROR;
}

/* Coerce monad oper.  */

static void
coerce_monad_oper (NODE_T *p, SOID_T *q)
{
  if (p != NO_NODE)
    {
      SOID_T z;
      a68_make_soid (&z, FIRM, MOID (PACK (MOID (TAX (p)))), 0);
      A68_INSERT_COERCIONS (NEXT (p), MOID (q), &z);
    }
}

/* Coerce monad formula.  */

static void
coerce_monad_formula (NODE_T *p)
{
  SOID_T e;
  a68_make_soid (&e, STRONG, get_monad_moid (p), 0);
  coerce_operand (NEXT (p), &e);
  coerce_monad_oper (p, &e);
}

/* Coerce operand.  */

static void
coerce_operand (NODE_T *p, SOID_T *q)
{
  if (IS (p, MONADIC_FORMULA))
    {
      coerce_monad_formula (SUB (p));
      if (MOID (p) != MOID (q))
	{
	  a68_make_sub (p, p, FORMULA);
	  A68_INSERT_COERCIONS (p, MOID (p), q);
	  a68_make_sub (p, p, TERTIARY);
	}
      MOID (p) = a68_depref_rows (MOID (p), MOID (q));
    }
  else if (IS (p, FORMULA))
    {
      coerce_formula (SUB (p), q);
      A68_INSERT_COERCIONS (p, MOID (p), q);
      MOID (p) = a68_depref_rows (MOID (p), MOID (q));
    }
  else if (IS (p, SECONDARY))
    {
      coerce_unit (SUB (p), q);
      MOID (p) = MOID (SUB (p));
    }
}

/* Coerce formula.  */

static void
coerce_formula (NODE_T *p, SOID_T *q __attribute__ ((unused)))
{
  if (IS (p, MONADIC_FORMULA) && NEXT (p) == NO_NODE)
    coerce_monad_formula (SUB (p));
  else
    {
      if (TAX (NEXT (p)) != NO_TAG && TAX (NEXT (p)) != A68_PARSER (error_tag))
	{
	  SOID_T s;
	  NODE_T *op = NEXT (p), *nq = NEXT_NEXT (p);
	  MOID_T *w = MOID (op);
	  MOID_T *u = MOID (PACK (w)), *v = MOID (NEXT (PACK (w)));
	  a68_make_soid (&s, STRONG, u, 0);
	  coerce_operand (p, &s);
	  a68_make_soid (&s, STRONG, v, 0);
	  coerce_operand (nq, &s);
	}
    }
}

/* Coerce assignation.  */

static void
coerce_assignation (NODE_T *p)
{
  SOID_T w;
  a68_make_soid (&w, SOFT, MOID (p), 0);
  coerce_unit (SUB (p), &w);
  a68_make_soid (&w, STRONG, SUB_MOID (p), 0);
  coerce_unit (NEXT_NEXT (p), &w);
}

/* Coerce relation.  */

static void
coerce_relation (NODE_T *p)
{
  SOID_T w;
  a68_make_soid (&w, STRONG, MOID (p), 0);
  coerce_unit (SUB (p), &w);
  a68_make_soid (&w, STRONG, MOID (NEXT_NEXT (p)), 0);
  coerce_unit (SUB (NEXT_NEXT (p)), &w);
}

/* Coerce bool function.  */

static void
coerce_bool_function (NODE_T *p)
{
  SOID_T w;
  a68_make_soid (&w, STRONG, M_BOOL, 0);
  coerce_unit (SUB (p), &w);
  coerce_unit (SUB (NEXT_NEXT (p)), &w);
}

/* Coerce assertion.  */

static void
coerce_assertion (NODE_T *p)
{
  SOID_T w;
  a68_make_soid (&w, MEEK, M_BOOL, 0);
  coerce_enclosed (SUB_NEXT (p), &w);
}

/* Coerce selection.  */

static void
coerce_selection (NODE_T * p)
{
  SOID_T w;
  a68_make_soid (&w, STRONG, MOID (NEXT (p)), 0);
  coerce_unit (SUB_NEXT (p), &w);
}

/* Coerce cast.  */

static void
coerce_cast (NODE_T * p)
{
  coerce_declarer (p);
  SOID_T w;
  a68_make_soid (&w, STRONG, MOID (p), 0);
  coerce_enclosed (NEXT (p), &w);
}

/* Coerce argument list.  */

static void
coerce_argument_list (PACK_T **r, NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (IS (p, ARGUMENT_LIST))
	coerce_argument_list (r, SUB (p));
      else if (IS (p, UNIT))
	{
	  SOID_T s;
	  a68_make_soid (&s, STRONG, MOID (*r), 0);
	  coerce_unit (p, &s);
	  FORWARD (*r);
	}
      else if (IS (p, TRIMMER))
	FORWARD (*r);
    }
}

/* Coerce call.  */

static void
coerce_call (NODE_T *p)
{
  MOID_T *proc = MOID (p);
  SOID_T w;
  a68_make_soid (&w, MEEK, proc, 0);
  coerce_unit (SUB (p), &w);
  FORWARD (p);
  PACK_T *t = PACK (proc);
  coerce_argument_list (&t, SUB (p));
}

/* Coerce meek int.  */

static void
coerce_meek_int (NODE_T *p)
{
  SOID_T x;
  a68_make_soid (&x, MEEK, M_INT, 0);
  coerce_unit (p, &x);
}

/* Coerce trimmer.  */

static void
coerce_trimmer (NODE_T *p)
{
  if (p != NO_NODE)
    {
      if (IS (p, UNIT))
	{
	  coerce_meek_int (p);
	  coerce_trimmer (NEXT (p));
	}
      else
	coerce_trimmer (NEXT (p));
    }
}

/* Coerce indexer.  */

static void
coerce_indexer (NODE_T *p)
{
  if (p != NO_NODE)
    {
      if (IS (p, TRIMMER))
	coerce_trimmer (SUB (p));
      else if (IS (p, UNIT))
	coerce_meek_int (p);
      else
	{
	  coerce_indexer (SUB (p));
	  coerce_indexer (NEXT (p));
	}
    }
}

/* Coerce_slice.  */

static void
coerce_slice (NODE_T *p)
{
  SOID_T w;
  MOID_T *row = MOID (p);
  a68_make_soid (&w, STRONG, row, 0);
  coerce_unit (SUB (p), &w);
  coerce_indexer (SUB_NEXT (p));
}

/* Coerce format text.  */

static void
coerce_format_text (NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      coerce_format_text (SUB (p));
      if (IS (p, FORMAT_PATTERN))
	{
	  SOID_T x;
	  a68_make_soid (&x, STRONG, M_FORMAT, 0);
	  coerce_enclosed (SUB (NEXT_SUB (p)), &x);
	}
      else if (IS (p, GENERAL_PATTERN) && NEXT_SUB (p) != NO_NODE)
	{
	  SOID_T x;
	  a68_make_soid (&x, STRONG, M_ROW_INT, 0);
	  coerce_enclosed (SUB (NEXT_SUB (p)), &x);
	}
      else if (IS (p, DYNAMIC_REPLICATOR))
	{
	  SOID_T x;
	  a68_make_soid (&x, STRONG, M_INT, 0);
	  coerce_enclosed (SUB (NEXT_SUB (p)), &x);
	}
    }
}

/* Coerce unit.  */

static void
coerce_unit (NODE_T *p, SOID_T *q)
{
  if (p == NO_NODE)
    return;
  else if (a68_is_one_of (p, UNIT, TERTIARY, SECONDARY, PRIMARY, STOP))
    {
      coerce_unit (SUB (p), q);
      MOID (p) = MOID (SUB (p));
      /* Ex primary.  */
    }
  else if (IS (p, CALL))
    {
      coerce_call (SUB (p));
      A68_INSERT_COERCIONS (p, MOID (p), q);
    }
  else if (IS (p, SLICE))
    {
      coerce_slice (SUB (p));
      A68_INSERT_COERCIONS (p, MOID (p), q);
    }
  else if (IS (p, CAST))
    {
      coerce_cast (SUB (p));
      A68_INSERT_COERCIONS (p, MOID (p), q);
    }
  else if (a68_is_one_of (p, DENOTATION, IDENTIFIER, STOP))
    A68_INSERT_COERCIONS (p, MOID (p), q);
  else if (IS (p, FORMAT_TEXT))
    {
      coerce_format_text (SUB (p));
      A68_INSERT_COERCIONS (p, MOID (p), q);
    }
  else if (IS (p, ENCLOSED_CLAUSE))
    {
      coerce_enclosed (p, q);
      /* Ex secondary.  */
    }
  else if (IS (p, SELECTION))
    {
      coerce_selection (SUB (p));
      A68_INSERT_COERCIONS (p, MOID (p), q);
    }
  else if (IS (p, GENERATOR))
    {
      coerce_declarer (SUB (p));
      A68_INSERT_COERCIONS (p, MOID (p), q);
      /* Ex tertiary.  */
    }
  else if (IS (p, NIHIL))
    {
      if (ATTRIBUTE (MOID (q)) != REF_SYMBOL && MOID (q) != M_VOID)
	a68_error (p, "context does not require a name");
      MOID (p) = a68_depref_rows (MOID (p), MOID (q));
    }
  else if (IS (p, FORMULA))
    {
      coerce_formula (SUB (p), q);
      A68_INSERT_COERCIONS (p, MOID (p), q);
    }
  else if (IS (p, JUMP))
    {
      if (MOID (q) == M_PROC_VOID)
	a68_make_sub (p, p, PROCEDURING);
      MOID (p) = a68_depref_rows (MOID (p), MOID (q));
    }
  else if (IS (p, SKIP))
    MOID (p) = a68_depref_rows (MOID (p), MOID (q));
  else if (IS (p, ASSIGNATION))
    {
      coerce_assignation (SUB (p));
      A68_INSERT_COERCIONS (p, MOID (p), q);
      MOID (p) = a68_depref_rows (MOID (p), MOID (q));
    }
  else if (IS (p, IDENTITY_RELATION))
    {
      coerce_relation (SUB (p));
      A68_INSERT_COERCIONS (p, MOID (p), q);
    }
  else if (IS (p, ROUTINE_TEXT))
    {
      coerce_routine_text (SUB (p));
      A68_INSERT_COERCIONS (p, MOID (p), q);
    }
  else if (a68_is_one_of (p, AND_FUNCTION, OR_FUNCTION, STOP))
    {
      coerce_bool_function (SUB (p));
      A68_INSERT_COERCIONS (p, MOID (p), q);
    }
  else if (IS (p, ASSERTION))
    {
      coerce_assertion (SUB (p));
      A68_INSERT_COERCIONS (p, MOID (p), q);
    }
}

/* Coerce module text.  */

static void
coerce_module_text (NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (IS (p, DEF_PART) || IS (p, POSTLUDE_PART))
	{
	  SOID_T w;
	  a68_make_soid (&w, STRONG, M_VOID, 0);
	  coerce_serial (NEXT_SUB (p), &w, true);
	}
    }
}

/* Coerce module declaration.  */

static void
coerce_module_declaration (NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (IS (p, MODULE_TEXT))
	coerce_module_text (SUB (p));
      else
	coerce_module_declaration (SUB (p));
    }
}	  

/* Driver for coercion insertions.  */

void
a68_coercion_inserter (NODE_T *p)
{
  if (IS (p, PACKET))
    {
      p = SUB (p);
      if (IS (p, PARTICULAR_PROGRAM))
	{
	  SOID_T q;
	  a68_make_soid (&q, STRONG, M_VOID, 0);
	  coerce_enclosed (SUB (p), &q);
	}
      else if (IS (p, PRELUDE_PACKET))
	coerce_module_declaration (SUB (p));
    }
}
