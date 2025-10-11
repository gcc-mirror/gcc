/* Symbol table management.
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

/*
 * Symbol table handling, managing TAGS.
 */

/* Forward declarations for several functions defined below.  */

static TAG_T *find_tag_local (TABLE_T *table, int a, const char *name);

/* Set level for procedures.  */

void
a68_set_proc_level (NODE_T *p, int n)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      PROCEDURE_LEVEL (INFO (p)) = n;
      if (IS (p, ROUTINE_TEXT))
	a68_set_proc_level (SUB (p), n + 1);
      else
	a68_set_proc_level (SUB (p), n);
    }
}

/* Set nests for diagnostics.  */

void
a68_set_nest (NODE_T *p, NODE_T *s)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      NEST (p) = s;
      if (IS (p, PARTICULAR_PROGRAM))
	a68_set_nest (SUB (p), p);
      else if (IS (p, CLOSED_CLAUSE) && LINE_NUMBER (p) != 0)
	a68_set_nest (SUB (p), p);
      else if (IS (p, COLLATERAL_CLAUSE) && LINE_NUMBER (p) != 0)
	a68_set_nest (SUB (p), p);
      else if (IS (p, CONDITIONAL_CLAUSE) && LINE_NUMBER (p) != 0)
	a68_set_nest (SUB (p), p);
      else if (IS (p, CASE_CLAUSE) && LINE_NUMBER (p) != 0)
	a68_set_nest (SUB (p), p);
      else if (IS (p, CONFORMITY_CLAUSE) && LINE_NUMBER (p) != 0)
	a68_set_nest (SUB (p), p);
      else if (IS (p, LOOP_CLAUSE) && LINE_NUMBER (p) != 0)
	a68_set_nest (SUB (p), p);
      else
	a68_set_nest (SUB (p), s);
    }
}

/*
 * Routines that work with tags and symbol tables.
 */

static void tax_tags (NODE_T *);
static void tax_specifier_list (NODE_T *);
static void tax_parameter_list (NODE_T *);
static void tax_format_texts (NODE_T *);

/* Find a tag, searching symbol tables towards the root.  */

int
a68_first_tag_global (TABLE_T * table, const char *name)
{
  if (table != NO_TABLE)
    {
      for (TAG_T *s = IDENTIFIERS (table); s != NO_TAG; FORWARD (s))
	{
	  if (NSYMBOL (NODE (s)) == name || strcmp (NSYMBOL (NODE (s)), name) == 0)
	    return IDENTIFIER;
	}
      for (TAG_T *s = INDICANTS (table); s != NO_TAG; FORWARD (s))
	{
	  if (NSYMBOL (NODE (s)) == name || strcmp (NSYMBOL (NODE (s)), name) == 0)
	    return INDICANT;
	}
      for (TAG_T *s = LABELS (table); s != NO_TAG; FORWARD (s))
	{
	  if (NSYMBOL (NODE (s)) == name || strcmp (NSYMBOL (NODE (s)), name) == 0)
	    return LABEL;
	}
      for (TAG_T *s = OPERATORS (table); s != NO_TAG; FORWARD (s))
	{
	  if (NSYMBOL (NODE (s)) == name || strcmp (NSYMBOL (NODE (s)), name) == 0)
	    return OP_SYMBOL;
	}
      for (TAG_T *s = MODULES (table); s != NO_TAG; FORWARD (s))
	{
	  if (NSYMBOL (NODE (s)) == name || strcmp (NSYMBOL (NODE (s)), name) == 0)
	    return MODULE_SYMBOL;
	}
      for (TAG_T *s = PRIO (table); s != NO_TAG; FORWARD (s))
	{
	  if (NSYMBOL (NODE (s)) == name || strcmp (NSYMBOL (NODE (s)), name) == 0)
	    return PRIO_SYMBOL;
	}
      return a68_first_tag_global (PREVIOUS (table), name);
    }
  else
    return STOP;
}

/* Whether routine can be "lengthety-mapped".  */

static bool
is_mappable_routine (const char *z)
{
#define ACCEPT(u, v) {\
  if (strlen (u) >= strlen (v)) {\
    if (strcmp (&u[strlen (u) - strlen (v)], v) == 0) {\
      return true;\
  }}}

  /* Math routines.  */
  ACCEPT (z, "arccos");
  ACCEPT (z, "arccosdg");
  ACCEPT (z, "arccot");
  ACCEPT (z, "arccotdg");
  ACCEPT (z, "arcsin");
  ACCEPT (z, "arcsindg");
  ACCEPT (z, "arctan");
  ACCEPT (z, "arctandg");
  ACCEPT (z, "beta");
  ACCEPT (z, "betainc");
  ACCEPT (z, "cbrt");
  ACCEPT (z, "cos");
  ACCEPT (z, "cosdg");
  ACCEPT (z, "cospi");
  ACCEPT (z, "cot");
  ACCEPT (z, "cot");
  ACCEPT (z, "cotdg");
  ACCEPT (z, "cotpi");
  ACCEPT (z, "curt");
  ACCEPT (z, "erf");
  ACCEPT (z, "erfc");
  ACCEPT (z, "exp");
  ACCEPT (z, "gamma");
  ACCEPT (z, "gammainc");
  ACCEPT (z, "gammaincg");
  ACCEPT (z, "gammaincgf");
  ACCEPT (z, "ln");
  ACCEPT (z, "log");
  ACCEPT (z, "pi");
  ACCEPT (z, "sin");
  ACCEPT (z, "sindg");
  ACCEPT (z, "sinpi");
  ACCEPT (z, "sqrt");
  ACCEPT (z, "tan");
  ACCEPT (z, "tandg");
  ACCEPT (z, "tanpi");
  /* Random generator.  */
  ACCEPT (z, "nextrandom");
  ACCEPT (z, "random");
  /* BITS.  */
  ACCEPT (z, "bitspack");
  /* Enquiries.  */
  ACCEPT (z, "maxint");
  ACCEPT (z, "intwidth");
  ACCEPT (z, "maxreal");
  ACCEPT (z, "realwidth");
  ACCEPT (z, "expwidth");
  ACCEPT (z, "maxbits");
  ACCEPT (z, "bitswidth");
  ACCEPT (z, "byteswidth");
  ACCEPT (z, "smallreal");
  return false;
#undef ACCEPT
}

/* Map "short sqrt" onto "sqrt" etcetera.  */

static TAG_T *
bind_lengthety_identifier (const char *u)
{
#define CAR(u, v) (strncmp (u, v, strlen(v)) == 0)
  /* We can only map routines blessed by "is_mappable_routine", so there is no
     "short print" or "long char in string". */
  if (CAR (u, "short"))
    {
      do
	{
	  u = &u[strlen ("short")];
	  const char *v = TEXT (a68_add_token (&A68 (top_token), u));
	  TAG_T *w = find_tag_local (A68_STANDENV, IDENTIFIER, v);
	  if (w != NO_TAG && is_mappable_routine (v))
	    return w;
	}
      while (CAR (u, "short"));
    }
  else if (CAR (u, "long"))
    {
      do
	{
	  u = &u[strlen ("long")];
	  const char *v = TEXT (a68_add_token (&A68 (top_token), u));
	  TAG_T *w = find_tag_local (A68_STANDENV, IDENTIFIER, v);
	  if (w != NO_TAG && is_mappable_routine (v))
	    return w;
	}
      while (CAR (u, "long"));
    }

  return NO_TAG;
#undef CAR
}

/* Bind identifier tags to the symbol table.  */

static void
bind_identifier_tag_to_symbol_table (NODE_T * p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      bind_identifier_tag_to_symbol_table (SUB (p));

      if (a68_is_one_of (p, IDENTIFIER, DEFINING_IDENTIFIER, STOP))
	{
	  int att = a68_first_tag_global (TABLE (p), NSYMBOL (p));

	  if (att == STOP)
	    {
	      TAG_T *z = bind_lengthety_identifier (NSYMBOL (p));

	      if (z != NO_TAG)
		MOID (p) = MOID (z);
	      TAX (p) = z;
	    }
	  else
	    {
	      TAG_T *z = a68_find_tag_global (TABLE (p), att, NSYMBOL (p));

	      if (att == IDENTIFIER && z != NO_TAG)
		MOID (p) = MOID (z);
	      else if (att == LABEL && z != NO_TAG)
		;
	      else if ((z = bind_lengthety_identifier (NSYMBOL (p))) != NO_TAG)
		MOID (p) = MOID (z);
	      else
		{
		  a68_error (p, "tag S has not been declared properly");
		  z = a68_add_tag (TABLE (p), IDENTIFIER, p, M_ERROR, NORMAL_IDENTIFIER);
		  MOID (p) = M_ERROR;
		}
	      TAX (p) = z;
	      if (IS (p, DEFINING_IDENTIFIER))
		NODE (z) = p;
	    }
	}
    }
}

/* Tell whether the given tree refers to the applied indicant INDICANT in an
   actual declarer.  */

static bool
declarer_contains_indicant (NODE_T *p, NODE_T *indicant)
{
  for (NODE_T *q = p; q != NO_NODE; FORWARD (q))
    {
      if (IS (q, DECLARER)
	  && IS (SUB (q), INDICANT)
	  && ((TAX (SUB (q)) && IS_RECURSIVE (TAX (SUB (q))))
	      || IS_LITERALLY (SUB (q), NSYMBOL (indicant))))
	{
	  return true;
	}

      if (declarer_contains_indicant (SUB (q), indicant))
	return true;
    }

  return false;
}

/* Bind indicant tags to the symbol table.  */

static void
bind_indicant_tag_to_symbol_table (NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      bind_indicant_tag_to_symbol_table (SUB (p));

      if (a68_is_one_of (p, INDICANT, DEFINING_INDICANT, STOP))
	{
	  TAG_T *z = a68_find_tag_global (TABLE (p), INDICANT, NSYMBOL (p));

	  if (z != NO_TAG)
	    {
	      MOID (p) = MOID (z);
	      TAX (p) = z;
	      if (IS (p, DEFINING_INDICANT))
		{
		  NODE (z) = p;
		  IS_RECURSIVE (z) = declarer_contains_indicant (NEXT_NEXT (p), p);
		}
	    }
	}
    }
}

/* Enter specifier identifiers in the symbol table.  */

static void
tax_specifiers (NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      tax_specifiers (SUB (p));

      if (SUB (p) != NO_NODE && IS (p, SPECIFIER))
	tax_specifier_list (SUB (p));
    }
}

/* Enter specifier identifiers in the symbol table.  */

static void
tax_specifier_list (NODE_T *p)
{
  if (p != NO_NODE)
    {
      if (IS (p, OPEN_SYMBOL))
	tax_specifier_list (NEXT (p));
      else if (a68_is_one_of (p, CLOSE_SYMBOL, VOID_SYMBOL, STOP))
	;
      else if (IS (p, IDENTIFIER))
	{
	  TAG_T *z = a68_add_tag (TABLE (p), IDENTIFIER, p, NO_MOID, SPECIFIER_IDENTIFIER);
	  HEAP (z) = LOC_SYMBOL;
	}
      else if (IS (p, DECLARER))
	{
	  tax_specifiers (SUB (p));
	  tax_specifier_list (NEXT (p));
	  /* last identifier entry is identifier with this declarer.  */
	  if (IDENTIFIERS (TABLE (p)) != NO_TAG
	      && PRIO (IDENTIFIERS (TABLE (p))) == SPECIFIER_IDENTIFIER)
	    MOID (IDENTIFIERS (TABLE (p))) = MOID (p);
	}
    }
}

/* Enter parameter identifiers in the symbol table.  */

static void
tax_parameters (NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (SUB (p) != NO_NODE)
	{
	  tax_parameters (SUB (p));
	  if (IS (p, PARAMETER_PACK))
	    tax_parameter_list (SUB (p));
	}
    }
}

/* Enter parameter identifiers in the symbol table.  */

static void
tax_parameter_list (NODE_T *p)
{
  if (p != NO_NODE)
    {
      if (a68_is_one_of (p, OPEN_SYMBOL, COMMA_SYMBOL, STOP))
      tax_parameter_list (NEXT (p));
      else if (IS (p, CLOSE_SYMBOL))
	;
      else if (a68_is_one_of (p, PARAMETER_LIST, PARAMETER, STOP))
	{
	  tax_parameter_list (NEXT (p));
	  tax_parameter_list (SUB (p));
	}
      else if (IS (p, IDENTIFIER))
	{
	  /* parameters are always local.  */
	  HEAP (a68_add_tag (TABLE (p), IDENTIFIER, p, NO_MOID, PARAMETER_IDENTIFIER)) = LOC_SYMBOL;
	}
      else if (IS (p, DECLARER))
	{
	  tax_parameter_list (NEXT (p));
	  /* last identifier entries are identifiers with this declarer.  */
	  for (TAG_T *s = IDENTIFIERS (TABLE (p)); s != NO_TAG && MOID (s) == NO_MOID; FORWARD (s))
	    MOID (s) = MOID (p);
	  tax_parameters (SUB (p));
	}
    }
}

/* Enter FOR identifiers in the symbol table.  */

static void
tax_for_identifiers (NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      tax_for_identifiers (SUB (p));

      if (IS (p, FOR_SYMBOL))
	{
	  if ((FORWARD (p)) != NO_NODE)
	    (void) a68_add_tag (TABLE (p), IDENTIFIER, p, M_INT, LOOP_IDENTIFIER);
	}
    }
}

/* Enter routine texts in the symbol table.  */

static void
tax_routine_texts (NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      tax_routine_texts (SUB (p));

      if (IS (p, ROUTINE_TEXT))
	{
	  TAG_T *z = a68_add_tag (TABLE (p), ANONYMOUS, p, MOID (p), ROUTINE_TEXT);
	  TAX (p) = z;
	  HEAP (z) = LOC_SYMBOL;
	  USE (z) = true;
	}
    }
}

/* Enter format texts in the symbol table.  */

static void
tax_format_texts (NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      tax_format_texts (SUB (p));

      if (IS (p, FORMAT_TEXT))
	{
	  TAG_T *z = a68_add_tag (TABLE (p), ANONYMOUS, p, M_FORMAT, FORMAT_TEXT);
	  TAX (p) = z;
	  USE (z) = true;
	}
      else if (IS (p, FORMAT_DELIMITER_SYMBOL) && NEXT (p) != NO_NODE)
	{
	  TAG_T *z = a68_add_tag (TABLE (p), ANONYMOUS, p, M_FORMAT, FORMAT_IDENTIFIER);
	  TAX (p) = z;
	  USE (z) = true;
	}
    }
}

/* Enter FORMAT pictures in the symbol table.  */

static void
tax_pictures (NODE_T * p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      tax_pictures (SUB (p));

      if (IS (p, PICTURE))
	TAX (p) = a68_add_tag (TABLE (p), ANONYMOUS, p, M_COLLITEM, FORMAT_IDENTIFIER);
    }
}

/* Enter generators in the symbol table.  */

static void
tax_generators (NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      tax_generators (SUB (p));

      if (IS (p, GENERATOR))
	{
	  if (IS (SUB (p), LOC_SYMBOL))
	    {
	      TAG_T *z = a68_add_tag (TABLE (p), ANONYMOUS, p, SUB_MOID (SUB (p)), GENERATOR);
	      HEAP (z) = LOC_SYMBOL;
	      USE (z) = true;
	      TAX (p) = z;
	    }
	}
    }
}

/* Find a firmly related operator for operands.  */

static TAG_T *
find_firmly_related_op (TABLE_T *c, const char *n, MOID_T *l, MOID_T *r, TAG_T *self)
{
  if (c != NO_TABLE)
    {
      TAG_T *s = OPERATORS (c);

      for (; s != NO_TAG; FORWARD (s))
	{
	  if (s != self && NSYMBOL (NODE (s)) == n)
	    {
	      PACK_T *t = PACK (MOID (s));
	      if (t != NO_PACK && a68_is_firm (MOID (t), l))
		{
		  /* catch monadic operator.  */
		  if ((FORWARD (t)) == NO_PACK)
		    {
		      if (r == NO_MOID)
			return s;
		    }
		  else
		    {
		      /* catch dyadic operator.  */
		      if (r != NO_MOID && a68_is_firm (MOID (t), r))
			return s;
		    }
		}
	    }
	}
    }
  return NO_TAG;
}

/* Check for firmly related operators in this range.  */

static void
test_firmly_related_ops_local (NODE_T *p, TAG_T *s)
{
  if (s != NO_TAG)
    {
      PACK_T *u = PACK (MOID (s));

      if (u != NO_PACK)
	{
	  MOID_T *l = MOID (u);
	  MOID_T *r = (NEXT (u) != NO_PACK ? MOID (NEXT (u)) : NO_MOID);
	  TAG_T *t = find_firmly_related_op (TAG_TABLE (s), NSYMBOL (NODE (s)), l, r, s);

	  if (t != NO_TAG)
	    {
	      a68_error (p, "M Z is firmly related to M Z",
			 MOID (s), NSYMBOL (NODE (s)), MOID (t),
			 NSYMBOL (NODE (t)));
	    }
	  else
	    {
	      /* Warn for hidden firmly related operators defined in outer
		 ranges, if requested.  */
	      for (TABLE_T *prev = PREVIOUS (TAG_TABLE (s));
		   prev != NO_TABLE;
		   prev = PREVIOUS (prev))
		{
		  TAG_T *t = find_firmly_related_op (prev, NSYMBOL (NODE (s)), l, r,
						     NO_TAG /* self */);
		  if (t != NO_TAG)
		    {
		      if (TAG_TABLE (t) == A68_STANDENV
			  && warn_algol68_hidden_declarations > 0)
			{
			  if (a68_warning (p, OPT_Whidden_declarations_,
					   "Z hides a firmly related operator in a larger reach",
					   NSYMBOL (NODE (s))))
			    {
			      a68_inform (NO_NODE,
					  "operator M Z defined in the standard prelude",
					  MOID (t), NSYMBOL (NODE (t)));
			    }
			}
		      else if (warn_algol68_hidden_declarations > 1)
			{
			  if (a68_warning (p, OPT_Whidden_declarations_,
					   "Z hides a firmly related operator in a larger reach",
					   NSYMBOL (NODE (s))))
			    {
			      a68_inform (NODE (t),
					  "previous hidden declaration of S declared here",
					  NSYMBOL (NODE (s)));
			    }
			}

		      /* Report only one level of hidding or it gets messy.  */
		      break;
		    }
		}
	    }
	}
      if (NEXT (s) != NO_TAG)
	test_firmly_related_ops_local ((p == NO_NODE ? NO_NODE : NODE (NEXT (s))), NEXT (s));
    }
}

/* Find firmly related operators in this program.  */

static void
test_firmly_related_ops (NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (SUB (p) != NO_NODE && a68_is_new_lexical_level (p))
	{
	  TAG_T *oops = OPERATORS (TABLE (SUB (p)));

	  if (oops != NO_TAG)
	    test_firmly_related_ops_local (NODE (oops), oops);
	}
      test_firmly_related_ops (SUB (p));
    }
}

/* Driver for the processing of TAXes.  */

void
a68_collect_taxes (NODE_T *p)
{
  tax_tags (p);
  tax_specifiers (p);
  tax_parameters (p);
  tax_for_identifiers (p);
  tax_routine_texts (p);
  tax_pictures (p);
  tax_format_texts (p);
  tax_generators (p);
  bind_identifier_tag_to_symbol_table (p);
  bind_indicant_tag_to_symbol_table (p);
  test_firmly_related_ops (p);
  test_firmly_related_ops_local (NO_NODE, OPERATORS (A68_STANDENV));
}

/* Whether tag has already been declared in this range.  */

static void
already_declared (NODE_T *n, int a)
{
  if (find_tag_local (TABLE (n), a, NSYMBOL (n)) != NO_TAG)
    a68_error (n, "multiple declaration of tag S");
}

/* Whether tag has already been declared in this range.  */

static void
already_declared_hidden (NODE_T *n, int a)
{
  if (find_tag_local (TABLE (n), a, NSYMBOL (n)) != NO_TAG)
    a68_error (n, "multiple declaration of tag S");

  TAG_T *s = a68_find_tag_global (PREVIOUS (TABLE (n)), a, NSYMBOL (n));

  if (s != NO_TAG
      && ((TAG_TABLE (s) == A68_STANDENV && warn_algol68_hidden_declarations > 0)
	  || (TAG_TABLE (s) != A68_STANDENV && warn_algol68_hidden_declarations > 1)))
    {
      if (a68_warning (n, OPT_Whidden_declarations_,
		       "Z hides a declaration with larger reach",
		       NSYMBOL (n)))
	{
	  if (TAG_TABLE (s) == A68_STANDENV)
	    a68_inform (NO_NODE,
			"M Z defined in the standard prelude",
			MOID (s), NSYMBOL (NODE (s)));
	  else
	    a68_inform (NODE (s),
			"previous hidden declaration of S declared here",
			NSYMBOL (n));
	}
    }
}

/* Add tag to local symbol table.  */

TAG_T *
a68_add_tag (TABLE_T *s, int a, NODE_T *n, MOID_T *m, int p)
{
#define INSERT_TAG(l, n) {NEXT (n) = *(l); *(l) = (n);}
  if (s != NO_TABLE)
    {
      TAG_T *z = a68_new_tag ();

      TAG_TABLE (z) = s;
      PRIO (z) = p;
      MOID (z) = m;
      NODE (z) = n;
      /* TAX(n) = z;.  */
      switch (a)
	{
	case IDENTIFIER:
	  already_declared_hidden (n, IDENTIFIER);
	  already_declared_hidden (n, LABEL);
	  INSERT_TAG (&IDENTIFIERS (s), z);
	  break;
	case INDICANT:
	  already_declared_hidden (n, INDICANT);
	  already_declared (n, OP_SYMBOL);
	  already_declared (n, PRIO_SYMBOL);
	  INSERT_TAG (&INDICANTS (s), z);
	  break;
	case LABEL:
	  already_declared_hidden (n, LABEL);
	  already_declared_hidden (n, IDENTIFIER);
	  INSERT_TAG (&LABELS (s), z);
	  break;
	case OP_SYMBOL:
	  already_declared (n, INDICANT);
	  INSERT_TAG (&OPERATORS (s), z);
	  break;
	case MODULE_SYMBOL:
	  already_declared (n, INDICANT);
	  INSERT_TAG (&MODULES (s), z);
	  break;
	case PRIO_SYMBOL:
	  already_declared (n, PRIO_SYMBOL);
	  already_declared (n, INDICANT);
	  INSERT_TAG (&PRIO (s), z);
	  break;
	case ANONYMOUS:
	  INSERT_TAG (&ANONYMOUS (s), z);
	  break;
	default:
	  gcc_unreachable ();
	}
      return z;
    }
  else
    return NO_TAG;
}

/* Find a tag, searching symbol tables towards the root.  */

TAG_T *
a68_find_tag_global (TABLE_T *table, int a, const char *name)
{
  if (table != NO_TABLE)
    {
      TAG_T *s = NO_TAG;
      switch (a)
	{
	case IDENTIFIER:
	  s = IDENTIFIERS (table);
	  break;
	case INDICANT:
	  s = INDICANTS (table);
	  break;
	case LABEL:
	  s = LABELS (table);
	  break;
	case OP_SYMBOL:
	  s = OPERATORS (table);
	  break;
	case MODULE_SYMBOL:
	  s = MODULES (table);
	  break;
	case PRIO_SYMBOL:
	  s = PRIO (table);
	  break;
	default:
	  gcc_unreachable ();
	  break;
	}

      for (; s != NO_TAG; FORWARD (s))
	{
	  if (NSYMBOL (NODE (s)) == name || strcmp (NSYMBOL (NODE (s)), name) == 0)
	    return s;
	}
      return a68_find_tag_global (PREVIOUS (table), a, name);
    }
  else
    return NO_TAG;
}

/* Whether identifier or label global.  */

int
a68_is_identifier_or_label_global (TABLE_T *table, const char *name)
{
  if (table != NO_TABLE)
    {
      for (TAG_T *s = IDENTIFIERS (table); s != NO_TAG; FORWARD (s))
	{
	  if (NSYMBOL (NODE (s)) == name || strcmp (NSYMBOL (NODE (s)), name) == 0)
	    return IDENTIFIER;
	}
      for (TAG_T *s = LABELS (table); s != NO_TAG; FORWARD (s))
	{
	  if (NSYMBOL (NODE (s)) == name || strcmp (NSYMBOL (NODE (s)), name) == 0)
	    return LABEL;
	}
      return a68_is_identifier_or_label_global (PREVIOUS (table), name);
    }
  else
    return 0;
}

/* Find a tag, searching only local symbol table.  */

static TAG_T *
find_tag_local (TABLE_T *table, int a, const char *name)
{
  if (table != NO_TABLE)
    {
      TAG_T *s = NO_TAG;

      if (a == OP_SYMBOL)
	s = OPERATORS (table);
      else if (a == MODULE_SYMBOL)
	s = MODULES (table);
      else if (a == PRIO_SYMBOL)
	s = PRIO (table);
      else if (a == IDENTIFIER)
	s = IDENTIFIERS (table);
      else if (a == INDICANT)
	s = INDICANTS (table);
      else if (a == LABEL)
	s = LABELS (table);
      else
	gcc_unreachable ();

    for (; s != NO_TAG; FORWARD (s))
      {
	if (NSYMBOL (NODE (s)) == name || strcmp (NSYMBOL (NODE (s)), name) == 0)
	  return s;
      }
    }
  return NO_TAG;
}

/* Whether context specifies HEAP or LOC for an identifier.

   The boolean *E is set to true if an explicit qualifier was found, false
   otherwise.  */

static int
tab_qualifier (NODE_T *p, bool *e)
{
  *e = false;
  if (p != NO_NODE)
    {
      if (a68_is_one_of (p, UNIT, ASSIGNATION, TERTIARY, SECONDARY, GENERATOR, STOP))
	return tab_qualifier (SUB (p), e);
      else if (a68_is_one_of (p, LOC_SYMBOL, HEAP_SYMBOL, STOP))
	{
	  *e = true;
	  return ATTRIBUTE (p) == LOC_SYMBOL ? LOC_SYMBOL : HEAP_SYMBOL;
	}
      else
	return LOC_SYMBOL;
    }
  else
    return LOC_SYMBOL;
}

/* Enter identity declarations in the symbol table.

   E is true if HEAP or LOC got specified explicitly in the identity
   declaration's formal parameter.  */

static void
tax_identity_dec (NODE_T *p, MOID_T **m)
{
  if (p != NO_NODE)
    {
      if (IS (p, IDENTITY_DECLARATION))
	{
	  tax_identity_dec (SUB (p), m);
	  tax_identity_dec (NEXT (p), m);
	}
      else if (IS (p, DECLARER))
	{
	  tax_tags (SUB (p));
	  *m = MOID (p);
	  tax_identity_dec (NEXT (p), m);
	}
      else if (IS (p, COMMA_SYMBOL))
	{
	  tax_identity_dec (NEXT (p), m);
	}
      else if (IS (p, DEFINING_IDENTIFIER))
	{
	  TAG_T *entry = find_tag_local (TABLE (p), IDENTIFIER, NSYMBOL (p));
	  gcc_assert (entry != NO_TAG);

	  MOID (p) = *m;
	  HEAP (entry) = LOC_SYMBOL;
	  TAX (p) = entry;
	  MOID (entry) = *m;
	  PUBLICIZED (entry) = PUBLICIZED (p);
	  bool e;
	  if (ATTRIBUTE (*m) == REF_SYMBOL)
	    {
	      HEAP (entry) = tab_qualifier (NEXT_NEXT (p), &e);
	      if (e && HEAP (entry) == LOC_SYMBOL && PUBLICIZED (entry))
		a68_warning (p, 0, "value of local generator will be out of scope");
	    }
	  tax_identity_dec (NEXT_NEXT (p), m);
	}
      else
	tax_tags (p);
    }
}

/* Enter variable declarations in the symbol table.

   E is true if an explicit sample generator was specified in the variable
   declaration.  */

static void
tax_variable_dec (NODE_T *p, int *q, MOID_T **m, bool e)
{
  if (p != NO_NODE)
    {
      if (IS (p, VARIABLE_DECLARATION))
	{
	  tax_variable_dec (SUB (p), q, m, e);
	  tax_variable_dec (NEXT (p), q, m, e);
	}
      else if (IS (p, DECLARER))
	{
	  tax_tags (SUB (p));
	  *m = MOID (p);
	  tax_variable_dec (NEXT (p), q, m, e);
	}
      else if (IS (p, QUALIFIER))
	{
	  *q = ATTRIBUTE (SUB (p));
	  tax_variable_dec (NEXT (p), q, m, true);
	}
      else if (IS (p, COMMA_SYMBOL))
	{
	  tax_variable_dec (NEXT (p), q, m, e);
	}
      else if (IS (p, DEFINING_IDENTIFIER))
	{
	  TAG_T *entry = find_tag_local (TABLE (p), IDENTIFIER, NSYMBOL (p));

	  MOID (p) = *m;
	  TAX (p) = entry;
	  PUBLICIZED (entry) = PUBLICIZED (p);

	  if (PUBLICIZED (p) && e && *q == LOC_SYMBOL)
	    a68_error (p, "publicized variable should not be allocated on the stack");

	  if (PUBLICIZED (p))
	    HEAP (entry) = STATIC_SYMBOL;
	  else
	    HEAP (entry) = *q;
	  
	  if (HEAP (entry) == LOC_SYMBOL)
	    {
	      TAG_T *z = a68_add_tag (TABLE (p), ANONYMOUS, p, SUB (*m), GENERATOR);
	      HEAP (z) = LOC_SYMBOL;
	      USE (z) = true;
	      BODY (entry) = z;
	    }
	  else
	    {
	      BODY (entry) = NO_TAG;
	    }
	  MOID (entry) = *m;
	  tax_variable_dec (NEXT (p), q, m, e);
	}
      else
	tax_tags (p);
    }
}

/* Enter procedure variable declarations in the symbol table.

   E is true if an explicit sample generator was specified in the procedure
   variable declaration.  */

static void
tax_proc_variable_dec (NODE_T *p, int *q, bool e)
{
  if (p != NO_NODE)
    {
      if (IS (p, PROCEDURE_VARIABLE_DECLARATION))
	{
	  tax_proc_variable_dec (SUB (p), q, e);
	  tax_proc_variable_dec (NEXT (p), q, e);
	}
      else if (IS (p, QUALIFIER))
	{
	  *q = ATTRIBUTE (SUB (p));
	  tax_proc_variable_dec (NEXT (p), q, true);
	}
      else if (a68_is_one_of (p, PROC_SYMBOL, COMMA_SYMBOL, STOP))
	{
	  tax_proc_variable_dec (NEXT (p), q, e);
	}
      else if (IS (p, DEFINING_IDENTIFIER))
	{
	  TAG_T *entry = find_tag_local (TABLE (p), IDENTIFIER, NSYMBOL (p));

	  TAX (p) = entry;
	  MOID (entry) = MOID (p);
	  PUBLICIZED (entry) = PUBLICIZED (p);

	  if (PUBLICIZED (p) && e && *q == LOC_SYMBOL)
	    a68_error (p, "publicized variable should not be allocated on the stack");

	  if (PUBLICIZED (p))
	    HEAP (entry) = STATIC_SYMBOL;
	  else
	    HEAP (entry) = *q;

	  if (HEAP (entry) == LOC_SYMBOL)
	    {
	      TAG_T *z = a68_add_tag (TABLE (p), ANONYMOUS, p, SUB_MOID (p), GENERATOR);
	      HEAP (z) = LOC_SYMBOL;
	      USE (z) = true;
	      BODY (entry) = z;
	    }
	  else
	    {
	      BODY (entry) = NO_TAG;
	    }
	  tax_proc_variable_dec (NEXT (p), q, e);
	}
      else
	tax_tags (p);
    }
}

/* Enter procedure declarations in the symbol table.  */

static void
tax_proc_dec (NODE_T *p)
{
  if (p != NO_NODE)
    {
      if (IS (p, PROCEDURE_DECLARATION))
	{
	  tax_proc_dec (SUB (p));
	  tax_proc_dec (NEXT (p));
	}
      else if (a68_is_one_of (p, PROC_SYMBOL, COMMA_SYMBOL, STOP))
	{
	  tax_proc_dec (NEXT (p));
	}
      else if (IS (p, DEFINING_IDENTIFIER))
	{
	  TAG_T *entry = find_tag_local (TABLE (p), IDENTIFIER, NSYMBOL (p));

	  MOID_T *m = MOID (NEXT_NEXT (p));
	  MOID (p) = m;
	  TAX (p) = entry;
	  HEAP (entry) = LOC_SYMBOL;
	  MOID (entry) = m;
	  PUBLICIZED (entry) = PUBLICIZED (p);
	  tax_proc_dec (NEXT (p));
	}
      else
	tax_tags (p);
    }
}

/* Check validity of operator declaration.  */

static void
check_operator_dec (NODE_T *p, MOID_T *u)
{
  int k = 0;

  if (u == NO_MOID)
    {
      NODE_T *pack = SUB_SUB (NEXT_NEXT (p)); /* Where the parameter pack
						 is.  */
      if (ATTRIBUTE (NEXT_NEXT (p)) != ROUTINE_TEXT)
	pack = SUB (pack);
      k = 1 + a68_count_operands (pack);
    }
  else
    k = a68_count_pack_members (PACK (u));

  if (k < 1 || k > 2)
    {
      a68_error (p, "incorrect number of operands for S");
      k = 0;
    }

  if (k == 1 && strchr (NOMADS, NSYMBOL (p)[0]) != NO_TEXT)
    {
      a68_error (p, "monadic S cannot start with a character from Z", NOMADS);
    }
  else if (k == 2 && !a68_find_tag_global (TABLE (p), PRIO_SYMBOL, NSYMBOL (p)))
    {
      a68_error (p, "dyadic S has no priority declaration");
    }
}

/* Enter operator declarations in the symbol table.  */

static void
tax_op_dec (NODE_T *p, MOID_T **m)
{
  if (p != NO_NODE)
    {
      if (IS (p, OPERATOR_DECLARATION))
	{
	  tax_op_dec (SUB (p), m);
	  tax_op_dec (NEXT (p), m);
	}
      else if (IS (p, OPERATOR_PLAN))
	{
	  tax_tags (SUB (p));
	  *m = MOID (p);
	  tax_op_dec (NEXT (p), m);
	}
      else if (IS (p, OP_SYMBOL))
	{
	  tax_op_dec (NEXT (p), m);
	}
      else if (IS (p, COMMA_SYMBOL))
	{
	  tax_op_dec (NEXT (p), m);
	}
      else if (IS (p, DEFINING_OPERATOR))
	{
	  TAG_T *entry = OPERATORS (TABLE (p));
	  check_operator_dec (p, *m);
	  while (entry != NO_TAG && NODE (entry) != p)
	    FORWARD (entry);
	  MOID (p) = *m;
	  TAX (p) = entry;
	  HEAP (entry) = LOC_SYMBOL;
	  MOID (entry) = *m;
	  PUBLICIZED (entry) = PUBLICIZED (p);
	  tax_op_dec (NEXT (p), m);
	}
      else
	{
	  tax_tags (p);
	}
    }
}

/* Enter module declarations in the symbol table.  */

static void
tax_module_dec (NODE_T *p)
{
  if (p != NO_NODE)
    {
      if (IS (p, MODULE_DECLARATION))
	{
	  tax_module_dec (SUB (p));
	  tax_module_dec (NEXT (p));
	}
      else if (IS (p, MODULE_SYMBOL))
	{
	  tax_module_dec (NEXT (p));
	}
      else if (IS (p, COMMA_SYMBOL))
	{
	  tax_module_dec (NEXT (p));
	}
      else if (IS (p, DEFINING_MODULE_INDICANT))
	{
	  TAG_T *entry = MODULES (TABLE (p));
	  while (entry != NO_TAG && NODE (entry) != p)
	    FORWARD (entry);
	  MOID (p) = NO_MOID;
	  TAX (p) = entry;
	  HEAP (entry) = LOC_SYMBOL;
	  MOID (entry) = NO_MOID;
	  PUBLICIZED (entry) = PUBLICIZED (p);
	  tax_module_dec (NEXT (p));
	}
      else
	{
	  tax_tags (p);
	}
    }
}

/* Enter brief operator declarations in the symbol table.  */

static void
tax_brief_op_dec (NODE_T *p)
{
  if (p != NO_NODE)
    {
      if (IS (p, BRIEF_OPERATOR_DECLARATION))
	{
	  tax_brief_op_dec (SUB (p));
	  tax_brief_op_dec (NEXT (p));
	}
      else if (a68_is_one_of (p, OP_SYMBOL, COMMA_SYMBOL, STOP))
	{
	  tax_brief_op_dec (NEXT (p));
	}
      else if (IS (p, DEFINING_OPERATOR))
	{
	  TAG_T *entry = OPERATORS (TABLE (p));
	  MOID_T *m = MOID (NEXT_NEXT (p));
	  check_operator_dec (p, NO_MOID);
	  while (entry != NO_TAG && NODE (entry) != p)
	    FORWARD (entry);
	  MOID (p) = m;
	  TAX (p) = entry;
	  HEAP (entry) = LOC_SYMBOL;
	  MOID (entry) = m;
	  PUBLICIZED (entry) = PUBLICIZED (p);
	  tax_brief_op_dec (NEXT (p));
	}
      else
	{
	  tax_tags (p);
	}
    }
}

/* Enter priority declarations in the symbol table.  */

static void tax_prio_dec (NODE_T *p)
{
  if (p != NO_NODE)
    {
      if (IS (p, PRIORITY_DECLARATION))
	{
	  tax_prio_dec (SUB (p));
	  tax_prio_dec (NEXT (p));
	}
      else if (a68_is_one_of (p, PRIO_SYMBOL, COMMA_SYMBOL, STOP))
	{
	  tax_prio_dec (NEXT (p));
	}
      else if (IS (p, DEFINING_OPERATOR))
	{
	  TAG_T *entry = PRIO (TABLE (p));
	  while (entry != NO_TAG && NODE (entry) != p)
	    FORWARD (entry);
	  MOID (p) = NO_MOID;
	  TAX (p) = entry;
	  HEAP (entry) = LOC_SYMBOL;
	  PUBLICIZED (entry) = PUBLICIZED (p);
	  tax_prio_dec (NEXT (p));
	}
      else
	{
	  tax_tags (p);
	}
    }
}

/* Enter TAXes in the symbol table.  */

static void
tax_tags (NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      int heap = LOC_SYMBOL;
      MOID_T *m = NO_MOID;

      if (IS (p, IDENTITY_DECLARATION))
	tax_identity_dec (p, &m);
      else if (IS (p, VARIABLE_DECLARATION))
	tax_variable_dec (p, &heap, &m, false);
      else if (IS (p, PROCEDURE_DECLARATION))
	tax_proc_dec (p);
      else if (IS (p, PROCEDURE_VARIABLE_DECLARATION))
	tax_proc_variable_dec (p, &heap, false);
      else if (IS (p, OPERATOR_DECLARATION))
	tax_op_dec (p, &m);
      else if (IS (p, BRIEF_OPERATOR_DECLARATION))
	tax_brief_op_dec (p);
      else if (IS (p, PRIORITY_DECLARATION))
	tax_prio_dec (p);
      else if (IS (p, MODULE_DECLARATION))
	tax_module_dec (p);
      else
	tax_tags (SUB (p));
    }
}

/* Reset symbol table nest count.  */

void
a68_reset_symbol_table_nest_count (NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (SUB (p) != NO_NODE && a68_is_new_lexical_level (p))
	NEST (TABLE (SUB (p))) = A68 (symbol_table_count)++;
      a68_reset_symbol_table_nest_count (SUB (p));
    }
}

//! @brief Bind routines in symbol table to the tree.

void
a68_bind_routine_tags_to_tree (NODE_T *p)
{
  /* By inserting coercions etc. some may have shifted.  */
  for (; p != NO_NODE; FORWARD (p))
    {
      if (IS (p, ROUTINE_TEXT) && TAX (p) != NO_TAG)
	NODE (TAX (p)) = p;
      a68_bind_routine_tags_to_tree (SUB (p));
    }
}

/* Bind formats in symbol table to tree.  */

static void
bind_format_tags_to_tree (NODE_T *p)
{
  /* By inserting coercions etc. some may have shifted.  */
  for (; p != NO_NODE; FORWARD (p))
    {
      if (IS (p, FORMAT_TEXT) && TAX (p) != NO_TAG)
	NODE (TAX (p)) = p;
      else if (IS (p, FORMAT_DELIMITER_SYMBOL) && NEXT (p) != NO_NODE && TAX (p) != NO_TAG)
	NODE (TAX (p)) = p;

      bind_format_tags_to_tree (SUB (p));
    }
}

/* Fill outer level of symbol table.  */

void
a68_fill_symbol_table_outer (NODE_T *p, TABLE_T *s)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (TABLE (p) != NO_TABLE)
	OUTER (TABLE (p)) = s;

      if (SUB (p) != NO_NODE && IS (p, ROUTINE_TEXT))
	a68_fill_symbol_table_outer (SUB (p), TABLE (SUB (p)));
      else if (SUB (p) != NO_NODE && IS (p, FORMAT_TEXT))
	a68_fill_symbol_table_outer (SUB (p), TABLE (SUB (p)));
      else if (SUB (p) != NO_NODE && IS (p, MODULE_TEXT))
	a68_fill_symbol_table_outer (SUB (p), TABLE (SUB (p)));
      else
	a68_fill_symbol_table_outer (SUB (p), s);
    }
}

/* Flood branch in tree with local symbol table S.  */

static void
flood_with_symbol_table_restricted (NODE_T *p, TABLE_T *s)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      TABLE (p) = s;
      if (ATTRIBUTE (p) != ROUTINE_TEXT && ATTRIBUTE (p) != SPECIFIED_UNIT)
	{
	  if (a68_is_new_lexical_level (p))
	    PREVIOUS (TABLE (SUB (p))) = s;
	  else
	    flood_with_symbol_table_restricted (SUB (p), s);
	}
    }
}

/* Final structure of symbol table after parsing.  */

void
a68_finalise_symbol_table_setup (NODE_T *p, int l)
{
  TABLE_T *s = TABLE (p);
  NODE_T *q = p;

  while (q != NO_NODE)
    {
      /* routine texts are ranges.  */
      if (IS (q, ROUTINE_TEXT))
	flood_with_symbol_table_restricted (SUB (q), a68_new_symbol_table (s));

      /* specifiers are ranges.  */
      else if (IS (q, SPECIFIED_UNIT))
	flood_with_symbol_table_restricted (SUB (q), a68_new_symbol_table (s));

      /* level count and recursion.  */
      if (SUB (q) != NO_NODE)
	{
	  if (a68_is_new_lexical_level (q))
	    {
	      LEX_LEVEL (SUB (q)) = l + 1;
	      gcc_assert (TABLE (SUB (q)) != s);
	      PREVIOUS (TABLE (SUB (q))) = s;
	      a68_finalise_symbol_table_setup (SUB (q), l + 1);
	      if (IS (q, WHILE_PART))
		{
		  /* This was a bug that went unnoticed for 15 years!.  */
		  TABLE_T *s2 = TABLE (SUB (q));
		  if ((FORWARD (q)) == NO_NODE)
		    return;
		  if (IS (q, ALT_DO_PART))
		    {
		      PREVIOUS (TABLE (SUB (q))) = s2;
		      LEX_LEVEL (SUB (q)) = l + 2;
		      a68_finalise_symbol_table_setup (SUB (q), l + 2);
		    }
		}
	      if (IS (q, DEF_PART))
		{
		  TABLE_T *s2 = TABLE (SUB (q));
		  if ((FORWARD (q)) == NO_NODE)
		    return;
		  if (IS (q, POSTLUDE_PART))
		    {
		      PREVIOUS (TABLE (SUB (q))) = s2;
		      LEX_LEVEL (SUB (q)) = l + 2;
		      a68_finalise_symbol_table_setup (SUB (q), l + 2);
		    }
		}
	    }
	  else
	    {
	      TABLE (SUB (q)) = s;
	      a68_finalise_symbol_table_setup (SUB (q), l);
	    }
	}
      TABLE (q) = s;

      if (IS (q, FOR_SYMBOL))
	FORWARD (q);
      FORWARD (q);
    }

  /* FOR identifiers are in the DO ... OD range.  */
  for (q = p; q != NO_NODE; FORWARD (q))
    {
      if (IS (q, FOR_SYMBOL))
	TABLE (NEXT (q)) = TABLE (SEQUENCE (NEXT (q)));
    }
}

/* First structure of symbol table for parsing.  */

void
a68_preliminary_symbol_table_setup (NODE_T *p)
{
  TABLE_T *s = TABLE (p);
  bool not_a_for_range = false;

  /* Let the tree point to the current symbol table.  */
  for (NODE_T *q = p; q != NO_NODE; FORWARD (q))
    TABLE (q) = s;

  /* insert new tables when required.  */
  for (NODE_T *q = p; q != NO_NODE && !not_a_for_range; FORWARD (q))
    {
      if (SUB (q) != NO_NODE)
	{
	  /* BEGIN ... END, CODE ... EDOC, DO ... OD, $ ... $, { ... } are
	     ranges.  */
	  if (a68_is_one_of (q, BEGIN_SYMBOL, DO_SYMBOL, ALT_DO_SYMBOL,
			     FORMAT_DELIMITER_SYMBOL, STOP))
	    {
	      TABLE (SUB (q)) = a68_new_symbol_table (s);
	      a68_preliminary_symbol_table_setup (SUB (q));
	    }
	  /* ( ... ) is a range.   */
	  else if (IS (q, OPEN_SYMBOL))
	    {
	      if (a68_whether (q, OPEN_SYMBOL, THEN_BAR_SYMBOL, STOP))
		{
		  TABLE (SUB (q)) = s;
		  a68_preliminary_symbol_table_setup (SUB (q));
		  FORWARD (q);
		  TABLE (SUB (q)) = a68_new_symbol_table (s);
		  a68_preliminary_symbol_table_setup (SUB (q));
		  if ((FORWARD (q)) == NO_NODE)
		    not_a_for_range = true;
		  else
		    {
		      if (IS (q, THEN_BAR_SYMBOL))
			{
			  TABLE (SUB (q)) = a68_new_symbol_table (s);
			  a68_preliminary_symbol_table_setup (SUB (q));
			}
		      if (IS (q, OPEN_SYMBOL))
			{
			  TABLE (SUB (q)) = a68_new_symbol_table (s);
			  a68_preliminary_symbol_table_setup (SUB (q));
			}
		    }
		}
	      else
		{
		  /* Don't worry about STRUCT (...), UNION (...), PROC (...)
		     yet.  */
		  TABLE (SUB (q)) = a68_new_symbol_table (s);
		  a68_preliminary_symbol_table_setup (SUB (q));
		}
	    }
	  /* IF ... THEN ... ELSE ... FI are ranges.  */
	  else if (IS (q, IF_SYMBOL))
	    {
	      if (a68_whether (q, IF_SYMBOL, THEN_SYMBOL, STOP))
		{
		  TABLE (SUB (q)) = s;
		  a68_preliminary_symbol_table_setup (SUB (q));
		  FORWARD (q);
		  TABLE (SUB (q)) = a68_new_symbol_table (s);
		  a68_preliminary_symbol_table_setup (SUB (q));
		  if ((FORWARD (q)) == NO_NODE)
		    not_a_for_range = true;
		  else
		    if (IS (q, ELSE_SYMBOL))
		      {
			TABLE (SUB (q)) = a68_new_symbol_table (s);
			a68_preliminary_symbol_table_setup (SUB (q));
		      }
		  if (IS (q, IF_SYMBOL))
		    {
		      TABLE (SUB (q)) = a68_new_symbol_table (s);
		      a68_preliminary_symbol_table_setup (SUB (q));
		    }
		}
	      else
		{
		  TABLE (SUB (q)) = a68_new_symbol_table (s);
		  a68_preliminary_symbol_table_setup (SUB (q));
		}
	    }
	  /* CASE ... IN ... OUT ... ESAC are ranges.  */
	  else if (IS (q, CASE_SYMBOL))
	    {
	      if (a68_whether (q, CASE_SYMBOL, IN_SYMBOL, STOP))
		{
		  TABLE (SUB (q)) = s;
		  a68_preliminary_symbol_table_setup (SUB (q));
		  FORWARD (q);
		  TABLE (SUB (q)) = a68_new_symbol_table (s);
		  a68_preliminary_symbol_table_setup (SUB (q));
		  if ((FORWARD (q)) == NO_NODE)
		    not_a_for_range = true;
		  else
		    {
		      if (IS (q, OUT_SYMBOL))
			{
			  TABLE (SUB (q)) = a68_new_symbol_table (s);
			  a68_preliminary_symbol_table_setup (SUB (q));
			}
		      if (IS (q, CASE_SYMBOL))
			{
			  TABLE (SUB (q)) = a68_new_symbol_table (s);
			  a68_preliminary_symbol_table_setup (SUB (q));
			}
		    }
		}
	      else
		{
		  TABLE (SUB (q)) = a68_new_symbol_table (s);
		  a68_preliminary_symbol_table_setup (SUB (q));
		}
	    }
	  /* WHILE ... DO ... OD are ranges.  */
	  else if (IS (q, WHILE_SYMBOL))
	    {
	      TABLE_T *u = a68_new_symbol_table (s);
	      TABLE (SUB (q)) = u;
	      a68_preliminary_symbol_table_setup (SUB (q));
	      if ((FORWARD (q)) == NO_NODE)
		not_a_for_range = true;
	      else if (IS (q, ALT_DO_SYMBOL))
		{
		  TABLE (SUB (q)) = a68_new_symbol_table (u);
		  a68_preliminary_symbol_table_setup (SUB (q));
		}
	    }
	  /* ACCESS ... DEF ...POSTLUDE ...FED are ranges.  */
	  else if (IS (q, ALT_ACCESS_SYMBOL))
	    {
	      TABLE_T *u = a68_new_symbol_table (s);
	      TABLE (SUB (q)) = u;
	      a68_preliminary_symbol_table_setup (SUB (q));
	      if (NEXT (q) == NO_NODE)
		not_a_for_range = true;
	      else if (IS ((FORWARD (q)), DEF_SYMBOL))
		{
		  TABLE_T *v = a68_new_symbol_table (u);
		  TABLE (SUB (q)) = v;
		  a68_preliminary_symbol_table_setup (SUB (q));
		}
	    }
	  /* DEF ... POSTLUDE ... FED are ranges.  */
	  else if (IS (q, DEF_SYMBOL))
	    {
	      TABLE_T *u = a68_new_symbol_table (s);
	      PUBLIC_RANGE (u) = true;
	      TABLE (SUB (q)) = u;
	      a68_preliminary_symbol_table_setup (SUB (q));
	      if (NEXT (q) == NO_NODE)
		  not_a_for_range = true;
	      else if (IS ((FORWARD (q)), POSTLUDE_SYMBOL))
		{
		  TABLE_T *v = a68_new_symbol_table (u);
		  TABLE (SUB (q)) = v;
		  a68_preliminary_symbol_table_setup (SUB (q));
		}
	    }
	  /* ACCESS ... CONTROLLED_CLAUSE  are ranges.  */
	  else if (IS (q, ACCESS_SYMBOL))
	    {
	      TABLE_T *u = a68_new_symbol_table (s);
	      TABLE (SUB (q)) = u;
	      a68_preliminary_symbol_table_setup (SUB (q));
	      if (NEXT (q) == NO_NODE)
		not_a_for_range = true;
	    }
	  else
	    {
	      TABLE (SUB (q)) = s;
	      a68_preliminary_symbol_table_setup (SUB (q));
	    }
	}
    }
  /* FOR identifiers will go to the DO ... OD range.  */
  if (!not_a_for_range)
    {
      for (NODE_T *q = p; q != NO_NODE; FORWARD (q))
	{
	  if (IS (q, FOR_SYMBOL))
	    {
	      NODE_T *r = q;
	      TABLE (NEXT (q)) = NO_TABLE;
	      for (; r != NO_NODE && TABLE (NEXT (q)) == NO_TABLE; FORWARD (r))
		{
		  if ((a68_is_one_of (r, WHILE_SYMBOL, ALT_DO_SYMBOL, STOP))
		      && (NEXT (q) != NO_NODE && SUB (r) != NO_NODE))
		    {
		      TABLE (NEXT (q)) = TABLE (SUB (r));
		      SEQUENCE (NEXT (q)) = SUB (r);
		    }
		}
	    }
	}
    }
}

/* Mark a mode as in use.  */

static void
mark_mode (MOID_T *m)
{
  if (m != NO_MOID && USE (m) == false)
    {
      PACK_T *p = PACK (m);
      USE (m) = true;
      for (; p != NO_PACK; FORWARD (p))
	{
	  mark_mode (MOID (p));
	  mark_mode (SUB (m));
	  mark_mode (SLICE (m));
	}
    }
}

//! @brief Traverse tree and mark modes as used.

void
a68_mark_moids (NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      a68_mark_moids (SUB (p));
      if (MOID (p) != NO_MOID)
	mark_mode (MOID (p));
    }
}

/* Mark various tags as used.  */

void
a68_mark_auxilliary (NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (SUB (p) != NO_NODE)
	{
	  /* You get no warnings on unused PROC parameters. That is ok since
	     A68 has some parameters that you may not use at all - think of
	     PROC (REF FILE) BOOL event routines in transput.  */
	  a68_mark_auxilliary (SUB (p));
	}
      else if (IS (p, OPERATOR))
	{
	  TAG_T *z;

	  if (TAX (p) != NO_TAG)
	    USE (TAX (p)) = true;
	  
	  if ((z = a68_find_tag_global (TABLE (p), PRIO_SYMBOL, NSYMBOL (p))) != NO_TAG)
	    USE (z) = true;
	}
      else if (IS (p, INDICANT))
	{
	  TAG_T *z = a68_find_tag_global (TABLE (p), INDICANT, NSYMBOL (p));

	  if (z != NO_TAG)
	    {
	      TAX (p) = z;
	      USE (z) = true;
	    }
	}
      else if (IS (p, IDENTIFIER))
	{
	  if (TAX (p) != NO_TAG)
	    USE (TAX (p)) = true;
	}
    }
}

/* Check a single tag.  */

static void
unused (TAG_T *s)
{
  for (; s != NO_TAG; FORWARD (s))
    {
      if (LINE_NUMBER (NODE (s)) > 0 && !USE (s))
	a68_warning (NODE (s), OPT_Wunused, "tag S is not used", NODE (s));
    }
}

/* Driver for traversing tree and warn for unused tags.  */

void
a68_warn_for_unused_tags (NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (SUB (p) != NO_NODE)
	{
	  if (a68_is_new_lexical_level (p))
	    {
	      unused (MODULES (TABLE (SUB (p))));
	      unused (OPERATORS (TABLE (SUB (p))));
	      unused (PRIO (TABLE (SUB (p))));
	      unused (IDENTIFIERS (TABLE (SUB (p))));
	      unused (LABELS (TABLE (SUB (p))));
	      unused (INDICANTS (TABLE (SUB (p))));
	    }
	}
      a68_warn_for_unused_tags (SUB (p));
    }
}

/* Mark jumps and procedured jumps.  */

void
a68_jumps_from_procs (NODE_T *p)
{
  for (; p != NO_NODE; FORWARD (p))
    {
      if (IS (p, PROCEDURING))
	{
	  NODE_T *u = SUB_SUB (p);

	  if (IS (u, GOTO_SYMBOL))
	    FORWARD (u);
	  USE (TAX (u)) = true;
	}
      else if (IS (p, JUMP))
	{
	  NODE_T *u = SUB (p);

	  if (IS (u, GOTO_SYMBOL))
	    FORWARD (u);
	  if ((TAX (u) == NO_TAG) && (MOID (u) == NO_MOID)
	      && (a68_find_tag_global (TABLE (u), LABEL, NSYMBOL (u)) == NO_TAG))
	    {
	      (void) a68_add_tag (TABLE (u), LABEL, u, NO_MOID, LOCAL_LABEL);
	      a68_error (u, "tag S has not been declared properly");
	    }
	  else
	    USE (TAX (u)) = true;
	}
      else
	a68_jumps_from_procs (SUB (p));
    }
}
