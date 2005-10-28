/* Deal with interfaces.
   Copyright (C) 2000, 2001, 2002, 2004, 2005 Free Software Foundation, Inc.
   Contributed by Andy Vaught

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */


/* Deal with interfaces.  An explicit interface is represented as a
   singly linked list of formal argument structures attached to the
   relevant symbols.  For an implicit interface, the arguments don't
   point to symbols.  Explicit interfaces point to namespaces that
   contain the symbols within that interface.

   Implicit interfaces are linked together in a singly linked list
   along the next_if member of symbol nodes.  Since a particular
   symbol can only have a single explicit interface, the symbol cannot
   be part of multiple lists and a single next-member suffices.

   This is not the case for general classes, though.  An operator
   definition is independent of just about all other uses and has it's
   own head pointer.

   Nameless interfaces:
     Nameless interfaces create symbols with explicit interfaces within
     the current namespace.  They are otherwise unlinked.

   Generic interfaces:
     The generic name points to a linked list of symbols.  Each symbol
     has an explicit interface.  Each explicit interface has it's own
     namespace containing the arguments.  Module procedures are symbols in
     which the interface is added later when the module procedure is parsed.

   User operators:
     User-defined operators are stored in a their own set of symtrees
     separate from regular symbols.  The symtrees point to gfc_user_op
     structures which in turn head up a list of relevant interfaces.

   Extended intrinsics and assignment:
     The head of these interface lists are stored in the containing namespace.

   Implicit interfaces:
     An implicit interface is represented as a singly linked list of
     formal argument list structures that don't point to any symbol
     nodes -- they just contain types.


   When a subprogram is defined, the program unit's name points to an
   interface as usual, but the link to the namespace is NULL and the
   formal argument list points to symbols within the same namespace as
   the program unit name.  */

#include "config.h"
#include "system.h"
#include "gfortran.h"
#include "match.h"


/* The current_interface structure holds information about the
   interface currently being parsed.  This structure is saved and
   restored during recursive interfaces.  */

gfc_interface_info current_interface;


/* Free a singly linked list of gfc_interface structures.  */

void
gfc_free_interface (gfc_interface * intr)
{
  gfc_interface *next;

  for (; intr; intr = next)
    {
      next = intr->next;
      gfc_free (intr);
    }
}


/* Change the operators unary plus and minus into binary plus and
   minus respectively, leaving the rest unchanged.  */

static gfc_intrinsic_op
fold_unary (gfc_intrinsic_op operator)
{

  switch (operator)
    {
    case INTRINSIC_UPLUS:
      operator = INTRINSIC_PLUS;
      break;
    case INTRINSIC_UMINUS:
      operator = INTRINSIC_MINUS;
      break;
    default:
      break;
    }

  return operator;
}


/* Match a generic specification.  Depending on which type of
   interface is found, the 'name' or 'operator' pointers may be set.
   This subroutine doesn't return MATCH_NO.  */

match
gfc_match_generic_spec (interface_type * type,
			char *name,
			gfc_intrinsic_op *operator)
{
  char buffer[GFC_MAX_SYMBOL_LEN + 1];
  match m;
  gfc_intrinsic_op i;

  if (gfc_match (" assignment ( = )") == MATCH_YES)
    {
      *type = INTERFACE_INTRINSIC_OP;
      *operator = INTRINSIC_ASSIGN;
      return MATCH_YES;
    }

  if (gfc_match (" operator ( %o )", &i) == MATCH_YES)
    {				/* Operator i/f */
      *type = INTERFACE_INTRINSIC_OP;
      *operator = fold_unary (i);
      return MATCH_YES;
    }

  if (gfc_match (" operator ( ") == MATCH_YES)
    {
      m = gfc_match_defined_op_name (buffer, 1);
      if (m == MATCH_NO)
	goto syntax;
      if (m != MATCH_YES)
	return MATCH_ERROR;

      m = gfc_match_char (')');
      if (m == MATCH_NO)
	goto syntax;
      if (m != MATCH_YES)
	return MATCH_ERROR;

      strcpy (name, buffer);
      *type = INTERFACE_USER_OP;
      return MATCH_YES;
    }

  if (gfc_match_name (buffer) == MATCH_YES)
    {
      strcpy (name, buffer);
      *type = INTERFACE_GENERIC;
      return MATCH_YES;
    }

  *type = INTERFACE_NAMELESS;
  return MATCH_YES;

syntax:
  gfc_error ("Syntax error in generic specification at %C");
  return MATCH_ERROR;
}


/* Match one of the five forms of an interface statement.  */

match
gfc_match_interface (void)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  interface_type type;
  gfc_symbol *sym;
  gfc_intrinsic_op operator;
  match m;

  m = gfc_match_space ();

  if (gfc_match_generic_spec (&type, name, &operator) == MATCH_ERROR)
    return MATCH_ERROR;


  /* If we're not looking at the end of the statement now, or if this
     is not a nameless interface but we did not see a space, punt.  */
  if (gfc_match_eos () != MATCH_YES
      || (type != INTERFACE_NAMELESS
	  && m != MATCH_YES))
    {
      gfc_error
	("Syntax error: Trailing garbage in INTERFACE statement at %C");
      return MATCH_ERROR;
    }

  current_interface.type = type;

  switch (type)
    {
    case INTERFACE_GENERIC:
      if (gfc_get_symbol (name, NULL, &sym))
	return MATCH_ERROR;

      if (!sym->attr.generic 
	  && gfc_add_generic (&sym->attr, sym->name, NULL) == FAILURE)
	return MATCH_ERROR;

      current_interface.sym = gfc_new_block = sym;
      break;

    case INTERFACE_USER_OP:
      current_interface.uop = gfc_get_uop (name);
      break;

    case INTERFACE_INTRINSIC_OP:
      current_interface.op = operator;
      break;

    case INTERFACE_NAMELESS:
      break;
    }

  return MATCH_YES;
}


/* Match the different sort of generic-specs that can be present after
   the END INTERFACE itself.  */

match
gfc_match_end_interface (void)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  interface_type type;
  gfc_intrinsic_op operator;
  match m;

  m = gfc_match_space ();

  if (gfc_match_generic_spec (&type, name, &operator) == MATCH_ERROR)
    return MATCH_ERROR;

  /* If we're not looking at the end of the statement now, or if this
     is not a nameless interface but we did not see a space, punt.  */
  if (gfc_match_eos () != MATCH_YES
      || (type != INTERFACE_NAMELESS
	  && m != MATCH_YES))
    {
      gfc_error
	("Syntax error: Trailing garbage in END INTERFACE statement at %C");
      return MATCH_ERROR;
    }

  m = MATCH_YES;

  switch (current_interface.type)
    {
    case INTERFACE_NAMELESS:
      if (type != current_interface.type)
	{
	  gfc_error ("Expected a nameless interface at %C");
	  m = MATCH_ERROR;
	}

      break;

    case INTERFACE_INTRINSIC_OP:
      if (type != current_interface.type || operator != current_interface.op)
	{

	  if (current_interface.op == INTRINSIC_ASSIGN)
	    gfc_error ("Expected 'END INTERFACE ASSIGNMENT (=)' at %C");
	  else
	    gfc_error ("Expecting 'END INTERFACE OPERATOR (%s)' at %C",
		       gfc_op2string (current_interface.op));

	  m = MATCH_ERROR;
	}

      break;

    case INTERFACE_USER_OP:
      /* Comparing the symbol node names is OK because only use-associated
         symbols can be renamed.  */
      if (type != current_interface.type
	  || strcmp (current_interface.uop->name, name) != 0)
	{
	  gfc_error ("Expecting 'END INTERFACE OPERATOR (.%s.)' at %C",
		     current_interface.sym->name);
	  m = MATCH_ERROR;
	}

      break;

    case INTERFACE_GENERIC:
      if (type != current_interface.type
	  || strcmp (current_interface.sym->name, name) != 0)
	{
	  gfc_error ("Expecting 'END INTERFACE %s' at %C",
		     current_interface.sym->name);
	  m = MATCH_ERROR;
	}

      break;
    }

  return m;
}


/* Compare two typespecs, recursively if necessary.  */

int
gfc_compare_types (gfc_typespec * ts1, gfc_typespec * ts2)
{
  gfc_component *dt1, *dt2;

  if (ts1->type != ts2->type)
    return 0;
  if (ts1->type != BT_DERIVED)
    return (ts1->kind == ts2->kind);

  /* Compare derived types.  */
  if (ts1->derived == ts2->derived)
    return 1;

  /* Special case for comparing derived types across namespaces.  If the
     true names and module names are the same and the module name is
     nonnull, then they are equal.  */
  if (strcmp (ts1->derived->name, ts2->derived->name) == 0
      && ((ts1->derived->module == NULL && ts2->derived->module == NULL)
	  || (ts1->derived != NULL && ts2->derived != NULL
	      && strcmp (ts1->derived->module, ts2->derived->module) == 0)))
    return 1;

  /* Compare type via the rules of the standard.  Both types must have
     the SEQUENCE attribute to be equal.  */

  if (strcmp (ts1->derived->name, ts2->derived->name))
    return 0;

  dt1 = ts1->derived->components;
  dt2 = ts2->derived->components;

  if (ts1->derived->attr.sequence == 0 || ts2->derived->attr.sequence == 0)
    return 0;

  /* Since subtypes of SEQUENCE types must be SEQUENCE types as well, a
     simple test can speed things up.  Otherwise, lots of things have to
     match.  */
  for (;;)
    {
      if (strcmp (dt1->name, dt2->name) != 0)
	return 0;

      if (dt1->pointer != dt2->pointer)
	return 0;

      if (dt1->dimension != dt2->dimension)
	return 0;

      if (dt1->dimension && gfc_compare_array_spec (dt1->as, dt2->as) == 0)
	return 0;

      if (gfc_compare_types (&dt1->ts, &dt2->ts) == 0)
	return 0;

      dt1 = dt1->next;
      dt2 = dt2->next;

      if (dt1 == NULL && dt2 == NULL)
	break;
      if (dt1 == NULL || dt2 == NULL)
	return 0;
    }

  return 1;
}


/* Given two symbols that are formal arguments, compare their ranks
   and types.  Returns nonzero if they have the same rank and type,
   zero otherwise.  */

static int
compare_type_rank (gfc_symbol * s1, gfc_symbol * s2)
{
  int r1, r2;

  r1 = (s1->as != NULL) ? s1->as->rank : 0;
  r2 = (s2->as != NULL) ? s2->as->rank : 0;

  if (r1 != r2)
    return 0;			/* Ranks differ */

  return gfc_compare_types (&s1->ts, &s2->ts);
}


static int compare_interfaces (gfc_symbol *, gfc_symbol *, int);

/* Given two symbols that are formal arguments, compare their types
   and rank and their formal interfaces if they are both dummy
   procedures.  Returns nonzero if the same, zero if different.  */

static int
compare_type_rank_if (gfc_symbol * s1, gfc_symbol * s2)
{

  if (s1->attr.flavor != FL_PROCEDURE && s2->attr.flavor != FL_PROCEDURE)
    return compare_type_rank (s1, s2);

  if (s1->attr.flavor != FL_PROCEDURE || s2->attr.flavor != FL_PROCEDURE)
    return 0;

  /* At this point, both symbols are procedures.  */
  if ((s1->attr.function == 0 && s1->attr.subroutine == 0)
      || (s2->attr.function == 0 && s2->attr.subroutine == 0))
    return 0;

  if (s1->attr.function != s2->attr.function
      || s1->attr.subroutine != s2->attr.subroutine)
    return 0;

  if (s1->attr.function && compare_type_rank (s1, s2) == 0)
    return 0;

  return compare_interfaces (s1, s2, 0);	/* Recurse! */
}


/* Given a formal argument list and a keyword name, search the list
   for that keyword.  Returns the correct symbol node if found, NULL
   if not found.  */

static gfc_symbol *
find_keyword_arg (const char *name, gfc_formal_arglist * f)
{

  for (; f; f = f->next)
    if (strcmp (f->sym->name, name) == 0)
      return f->sym;

  return NULL;
}


/******** Interface checking subroutines **********/


/* Given an operator interface and the operator, make sure that all
   interfaces for that operator are legal.  */

static void
check_operator_interface (gfc_interface * intr, gfc_intrinsic_op operator)
{
  gfc_formal_arglist *formal;
  sym_intent i1, i2;
  gfc_symbol *sym;
  bt t1, t2;
  int args;

  if (intr == NULL)
    return;

  args = 0;
  t1 = t2 = BT_UNKNOWN;
  i1 = i2 = INTENT_UNKNOWN;

  for (formal = intr->sym->formal; formal; formal = formal->next)
    {
      sym = formal->sym;

      if (args == 0)
	{
	  t1 = sym->ts.type;
	  i1 = sym->attr.intent;
	}
      if (args == 1)
	{
	  t2 = sym->ts.type;
	  i2 = sym->attr.intent;
	}
      args++;
    }

  if (args == 0 || args > 2)
    goto num_args;

  sym = intr->sym;

  if (operator == INTRINSIC_ASSIGN)
    {
      if (!sym->attr.subroutine)
	{
	  gfc_error
	    ("Assignment operator interface at %L must be a SUBROUTINE",
	     &intr->where);
	  return;
	}
    }
  else
    {
      if (!sym->attr.function)
	{
	  gfc_error ("Intrinsic operator interface at %L must be a FUNCTION",
		     &intr->where);
	  return;
	}
    }

  switch (operator)
    {
    case INTRINSIC_PLUS:	/* Numeric unary or binary */
    case INTRINSIC_MINUS:
      if ((args == 1)
	  && (t1 == BT_INTEGER
	      || t1 == BT_REAL
	      || t1 == BT_COMPLEX))
	goto bad_repl;

      if ((args == 2)
	  && (t1 == BT_INTEGER || t1 == BT_REAL || t1 == BT_COMPLEX)
	  && (t2 == BT_INTEGER || t2 == BT_REAL || t2 == BT_COMPLEX))
	goto bad_repl;

      break;

    case INTRINSIC_POWER:	/* Binary numeric */
    case INTRINSIC_TIMES:
    case INTRINSIC_DIVIDE:

    case INTRINSIC_EQ:
    case INTRINSIC_NE:
      if (args == 1)
	goto num_args;

      if ((t1 == BT_INTEGER || t1 == BT_REAL || t1 == BT_COMPLEX)
	  && (t2 == BT_INTEGER || t2 == BT_REAL || t2 == BT_COMPLEX))
	goto bad_repl;

      break;

    case INTRINSIC_GE:		/* Binary numeric operators that do not support */
    case INTRINSIC_LE:		/* complex numbers */
    case INTRINSIC_LT:
    case INTRINSIC_GT:
      if (args == 1)
	goto num_args;

      if ((t1 == BT_INTEGER || t1 == BT_REAL)
	  && (t2 == BT_INTEGER || t2 == BT_REAL))
	goto bad_repl;

      break;

    case INTRINSIC_OR:		/* Binary logical */
    case INTRINSIC_AND:
    case INTRINSIC_EQV:
    case INTRINSIC_NEQV:
      if (args == 1)
	goto num_args;
      if (t1 == BT_LOGICAL && t2 == BT_LOGICAL)
	goto bad_repl;
      break;

    case INTRINSIC_NOT:	/* Unary logical */
      if (args != 1)
	goto num_args;
      if (t1 == BT_LOGICAL)
	goto bad_repl;
      break;

    case INTRINSIC_CONCAT:	/* Binary string */
      if (args != 2)
	goto num_args;
      if (t1 == BT_CHARACTER && t2 == BT_CHARACTER)
	goto bad_repl;
      break;

    case INTRINSIC_ASSIGN:	/* Class by itself */
      if (args != 2)
	goto num_args;
      break;
    default:
      gfc_internal_error ("check_operator_interface(): Bad operator");
    }

  /* Check intents on operator interfaces.  */
  if (operator == INTRINSIC_ASSIGN)
    {
      if (i1 != INTENT_OUT && i1 != INTENT_INOUT)
	gfc_error ("First argument of defined assignment at %L must be "
		   "INTENT(IN) or INTENT(INOUT)", &intr->where);

      if (i2 != INTENT_IN)
	gfc_error ("Second argument of defined assignment at %L must be "
		   "INTENT(IN)", &intr->where);
    }
  else
    {
      if (i1 != INTENT_IN)
	gfc_error ("First argument of operator interface at %L must be "
		   "INTENT(IN)", &intr->where);

      if (args == 2 && i2 != INTENT_IN)
	gfc_error ("Second argument of operator interface at %L must be "
		   "INTENT(IN)", &intr->where);
    }

  return;

bad_repl:
  gfc_error ("Operator interface at %L conflicts with intrinsic interface",
	     &intr->where);
  return;

num_args:
  gfc_error ("Operator interface at %L has the wrong number of arguments",
	     &intr->where);
  return;
}


/* Given a pair of formal argument lists, we see if the two lists can
   be distinguished by counting the number of nonoptional arguments of
   a given type/rank in f1 and seeing if there are less then that
   number of those arguments in f2 (including optional arguments).
   Since this test is asymmetric, it has to be called twice to make it
   symmetric.  Returns nonzero if the argument lists are incompatible
   by this test.  This subroutine implements rule 1 of section
   14.1.2.3.  */

static int
count_types_test (gfc_formal_arglist * f1, gfc_formal_arglist * f2)
{
  int rc, ac1, ac2, i, j, k, n1;
  gfc_formal_arglist *f;

  typedef struct
  {
    int flag;
    gfc_symbol *sym;
  }
  arginfo;

  arginfo *arg;

  n1 = 0;

  for (f = f1; f; f = f->next)
    n1++;

  /* Build an array of integers that gives the same integer to
     arguments of the same type/rank.  */
  arg = gfc_getmem (n1 * sizeof (arginfo));

  f = f1;
  for (i = 0; i < n1; i++, f = f->next)
    {
      arg[i].flag = -1;
      arg[i].sym = f->sym;
    }

  k = 0;

  for (i = 0; i < n1; i++)
    {
      if (arg[i].flag != -1)
	continue;

      if (arg[i].sym->attr.optional)
	continue;		/* Skip optional arguments */

      arg[i].flag = k;

      /* Find other nonoptional arguments of the same type/rank.  */
      for (j = i + 1; j < n1; j++)
	if (!arg[j].sym->attr.optional
	    && compare_type_rank_if (arg[i].sym, arg[j].sym))
	  arg[j].flag = k;

      k++;
    }

  /* Now loop over each distinct type found in f1.  */
  k = 0;
  rc = 0;

  for (i = 0; i < n1; i++)
    {
      if (arg[i].flag != k)
	continue;

      ac1 = 1;
      for (j = i + 1; j < n1; j++)
	if (arg[j].flag == k)
	  ac1++;

      /* Count the number of arguments in f2 with that type, including
         those that are optional.  */
      ac2 = 0;

      for (f = f2; f; f = f->next)
	if (compare_type_rank_if (arg[i].sym, f->sym))
	  ac2++;

      if (ac1 > ac2)
	{
	  rc = 1;
	  break;
	}

      k++;
    }

  gfc_free (arg);

  return rc;
}


/* Perform the abbreviated correspondence test for operators.  The
   arguments cannot be optional and are always ordered correctly,
   which makes this test much easier than that for generic tests.

   This subroutine is also used when comparing a formal and actual
   argument list when an actual parameter is a dummy procedure.  At
   that point, two formal interfaces must be compared for equality
   which is what happens here.  */

static int
operator_correspondence (gfc_formal_arglist * f1, gfc_formal_arglist * f2)
{
  for (;;)
    {
      if (f1 == NULL && f2 == NULL)
	break;
      if (f1 == NULL || f2 == NULL)
	return 1;

      if (!compare_type_rank (f1->sym, f2->sym))
	return 1;

      f1 = f1->next;
      f2 = f2->next;
    }

  return 0;
}


/* Perform the correspondence test in rule 2 of section 14.1.2.3.
   Returns zero if no argument is found that satisifes rule 2, nonzero
   otherwise.

   This test is also not symmetric in f1 and f2 and must be called
   twice.  This test finds problems caused by sorting the actual
   argument list with keywords.  For example:

   INTERFACE FOO
       SUBROUTINE F1(A, B)
           INTEGER :: A ; REAL :: B
       END SUBROUTINE F1

       SUBROUTINE F2(B, A)
           INTEGER :: A ; REAL :: B
       END SUBROUTINE F1
   END INTERFACE FOO

   At this point, 'CALL FOO(A=1, B=1.0)' is ambiguous.  */

static int
generic_correspondence (gfc_formal_arglist * f1, gfc_formal_arglist * f2)
{

  gfc_formal_arglist *f2_save, *g;
  gfc_symbol *sym;

  f2_save = f2;

  while (f1)
    {
      if (f1->sym->attr.optional)
	goto next;

      if (f2 != NULL && compare_type_rank (f1->sym, f2->sym))
	goto next;

      /* Now search for a disambiguating keyword argument starting at
         the current non-match.  */
      for (g = f1; g; g = g->next)
	{
	  if (g->sym->attr.optional)
	    continue;

	  sym = find_keyword_arg (g->sym->name, f2_save);
	  if (sym == NULL || !compare_type_rank (g->sym, sym))
	    return 1;
	}

    next:
      f1 = f1->next;
      if (f2 != NULL)
	f2 = f2->next;
    }

  return 0;
}


/* 'Compare' two formal interfaces associated with a pair of symbols.
   We return nonzero if there exists an actual argument list that
   would be ambiguous between the two interfaces, zero otherwise.  */

static int
compare_interfaces (gfc_symbol * s1, gfc_symbol * s2, int generic_flag)
{
  gfc_formal_arglist *f1, *f2;

  if (s1->attr.function != s2->attr.function
      && s1->attr.subroutine != s2->attr.subroutine)
    return 0;			/* disagreement between function/subroutine */

  f1 = s1->formal;
  f2 = s2->formal;

  if (f1 == NULL && f2 == NULL)
    return 1;			/* Special case */

  if (count_types_test (f1, f2))
    return 0;
  if (count_types_test (f2, f1))
    return 0;

  if (generic_flag)
    {
      if (generic_correspondence (f1, f2))
	return 0;
      if (generic_correspondence (f2, f1))
	return 0;
    }
  else
    {
      if (operator_correspondence (f1, f2))
	return 0;
    }

  return 1;
}


/* Given a pointer to an interface pointer, remove duplicate
   interfaces and make sure that all symbols are either functions or
   subroutines.  Returns nonzero if something goes wrong.  */

static int
check_interface0 (gfc_interface * p, const char *interface_name)
{
  gfc_interface *psave, *q, *qlast;

  psave = p;
  /* Make sure all symbols in the interface have been defined as
     functions or subroutines.  */
  for (; p; p = p->next)
    if (!p->sym->attr.function && !p->sym->attr.subroutine)
      {
	gfc_error ("Procedure '%s' in %s at %L is neither function nor "
		   "subroutine", p->sym->name, interface_name,
		   &p->sym->declared_at);
	return 1;
      }
  p = psave;

  /* Remove duplicate interfaces in this interface list.  */
  for (; p; p = p->next)
    {
      qlast = p;

      for (q = p->next; q;)
	{
	  if (p->sym != q->sym)
	    {
	      qlast = q;
	      q = q->next;

	    }
	  else
	    {
	      /* Duplicate interface */
	      qlast->next = q->next;
	      gfc_free (q);
	      q = qlast->next;
	    }
	}
    }

  return 0;
}


/* Check lists of interfaces to make sure that no two interfaces are
   ambiguous.  Duplicate interfaces (from the same symbol) are OK
   here.  */

static int
check_interface1 (gfc_interface * p, gfc_interface * q,
		  int generic_flag, const char *interface_name)
{

  for (; p; p = p->next)
    for (; q; q = q->next)
      {
	if (p->sym == q->sym)
	  continue;		/* Duplicates OK here */

	if (p->sym->name == q->sym->name && p->sym->module == q->sym->module)
	  continue;

	if (compare_interfaces (p->sym, q->sym, generic_flag))
	  {
	    gfc_error ("Ambiguous interfaces '%s' and '%s' in %s at %L",
		       p->sym->name, q->sym->name, interface_name, &p->where);
	    return 1;
	  }
      }

  return 0;
}


/* Check the generic and operator interfaces of symbols to make sure
   that none of the interfaces conflict.  The check has to be done
   after all of the symbols are actually loaded.  */

static void
check_sym_interfaces (gfc_symbol * sym)
{
  char interface_name[100];
  gfc_symbol *s2;

  if (sym->ns != gfc_current_ns)
    return;

  if (sym->generic != NULL)
    {
      sprintf (interface_name, "generic interface '%s'", sym->name);
      if (check_interface0 (sym->generic, interface_name))
	return;

      s2 = sym;
      while (s2 != NULL)
	{
	  if (check_interface1 (sym->generic, s2->generic, 1, interface_name))
	    return;

	  if (s2->ns->parent == NULL)
	    break;
	  if (gfc_find_symbol (sym->name, s2->ns->parent, 1, &s2))
	    break;
	}
    }
}


static void
check_uop_interfaces (gfc_user_op * uop)
{
  char interface_name[100];
  gfc_user_op *uop2;
  gfc_namespace *ns;

  sprintf (interface_name, "operator interface '%s'", uop->name);
  if (check_interface0 (uop->operator, interface_name))
    return;

  for (ns = gfc_current_ns; ns; ns = ns->parent)
    {
      uop2 = gfc_find_uop (uop->name, ns);
      if (uop2 == NULL)
	continue;

      check_interface1 (uop->operator, uop2->operator, 0, interface_name);
    }
}


/* For the namespace, check generic, user operator and intrinsic
   operator interfaces for consistency and to remove duplicate
   interfaces.  We traverse the whole namespace, counting on the fact
   that most symbols will not have generic or operator interfaces.  */

void
gfc_check_interfaces (gfc_namespace * ns)
{
  gfc_namespace *old_ns, *ns2;
  char interface_name[100];
  gfc_intrinsic_op i;

  old_ns = gfc_current_ns;
  gfc_current_ns = ns;

  gfc_traverse_ns (ns, check_sym_interfaces);

  gfc_traverse_user_op (ns, check_uop_interfaces);

  for (i = GFC_INTRINSIC_BEGIN; i != GFC_INTRINSIC_END; i++)
    {
      if (i == INTRINSIC_USER)
	continue;

      if (i == INTRINSIC_ASSIGN)
	strcpy (interface_name, "intrinsic assignment operator");
      else
	sprintf (interface_name, "intrinsic '%s' operator",
		 gfc_op2string (i));

      if (check_interface0 (ns->operator[i], interface_name))
	continue;

      check_operator_interface (ns->operator[i], i);

      for (ns2 = ns->parent; ns2; ns2 = ns2->parent)
	if (check_interface1 (ns->operator[i], ns2->operator[i], 0,
			      interface_name))
	  break;
    }

  gfc_current_ns = old_ns;
}


static int
symbol_rank (gfc_symbol * sym)
{

  return (sym->as == NULL) ? 0 : sym->as->rank;
}


/* Given a symbol of a formal argument list and an expression, if the
   formal argument is a pointer, see if the actual argument is a
   pointer. Returns nonzero if compatible, zero if not compatible.  */

static int
compare_pointer (gfc_symbol * formal, gfc_expr * actual)
{
  symbol_attribute attr;

  if (formal->attr.pointer)
    {
      attr = gfc_expr_attr (actual);
      if (!attr.pointer)
	return 0;
    }

  return 1;
}


/* Given a symbol of a formal argument list and an expression, see if
   the two are compatible as arguments.  Returns nonzero if
   compatible, zero if not compatible.  */

static int
compare_parameter (gfc_symbol * formal, gfc_expr * actual,
		   int ranks_must_agree, int is_elemental)
{
  gfc_ref *ref;

  if (actual->ts.type == BT_PROCEDURE)
    {
      if (formal->attr.flavor != FL_PROCEDURE)
	return 0;

      if (formal->attr.function
	  && !compare_type_rank (formal, actual->symtree->n.sym))
	return 0;

      if (formal->attr.if_source == IFSRC_UNKNOWN)
	return 1;		/* Assume match */

      return compare_interfaces (formal, actual->symtree->n.sym, 0);
    }

  if ((actual->expr_type != EXPR_NULL || actual->ts.type != BT_UNKNOWN)
      && !gfc_compare_types (&formal->ts, &actual->ts))
    return 0;

  if (symbol_rank (formal) == actual->rank)
    return 1;

  /* At this point the ranks didn't agree.  */
  if (ranks_must_agree || formal->attr.pointer)
    return 0;

  if (actual->rank != 0)
    return is_elemental || formal->attr.dimension;

  /* At this point, we are considering a scalar passed to an array.
     This is legal if the scalar is an array element of the right sort.  */
  if (formal->as->type == AS_ASSUMED_SHAPE)
    return 0;

  for (ref = actual->ref; ref; ref = ref->next)
    if (ref->type == REF_SUBSTRING)
      return 0;

  for (ref = actual->ref; ref; ref = ref->next)
    if (ref->type == REF_ARRAY && ref->u.ar.type == AR_ELEMENT)
      break;

  if (ref == NULL)
    return 0;			/* Not an array element */

  return 1;
}


/* Given formal and actual argument lists, see if they are compatible.
   If they are compatible, the actual argument list is sorted to
   correspond with the formal list, and elements for missing optional
   arguments are inserted. If WHERE pointer is nonnull, then we issue
   errors when things don't match instead of just returning the status
   code.  */

static int
compare_actual_formal (gfc_actual_arglist ** ap,
		       gfc_formal_arglist * formal,
		       int ranks_must_agree, int is_elemental, locus * where)
{
  gfc_actual_arglist **new, *a, *actual, temp;
  gfc_formal_arglist *f;
  int i, n, na;

  actual = *ap;

  if (actual == NULL && formal == NULL)
    return 1;

  n = 0;
  for (f = formal; f; f = f->next)
    n++;

  new = (gfc_actual_arglist **) alloca (n * sizeof (gfc_actual_arglist *));

  for (i = 0; i < n; i++)
    new[i] = NULL;

  na = 0;
  f = formal;
  i = 0;

  for (a = actual; a; a = a->next, f = f->next)
    {
      if (a->name != NULL)
	{
	  i = 0;
	  for (f = formal; f; f = f->next, i++)
	    {
	      if (f->sym == NULL)
		continue;
	      if (strcmp (f->sym->name, a->name) == 0)
		break;
	    }

	  if (f == NULL)
	    {
	      if (where)
		gfc_error
		  ("Keyword argument '%s' at %L is not in the procedure",
		   a->name, &a->expr->where);
	      return 0;
	    }

	  if (new[i] != NULL)
	    {
	      if (where)
		gfc_error
		  ("Keyword argument '%s' at %L is already associated "
		   "with another actual argument", a->name, &a->expr->where);
	      return 0;
	    }
	}

      if (f == NULL)
	{
	  if (where)
	    gfc_error
	      ("More actual than formal arguments in procedure call at %L",
	       where);

	  return 0;
	}

      if (f->sym == NULL && a->expr == NULL)
	goto match;

      if (f->sym == NULL)
	{
	  if (where)
	    gfc_error
	      ("Missing alternate return spec in subroutine call at %L",
	       where);
	  return 0;
	}

      if (a->expr == NULL)
	{
	  if (where)
	    gfc_error
	      ("Unexpected alternate return spec in subroutine call at %L",
	       where);
	  return 0;
	}

      if (!compare_parameter
	  (f->sym, a->expr, ranks_must_agree, is_elemental))
	{
	  if (where)
	    gfc_error ("Type/rank mismatch in argument '%s' at %L",
		       f->sym->name, &a->expr->where);
	  return 0;
	}

      if (a->expr->expr_type != EXPR_NULL
	  && compare_pointer (f->sym, a->expr) == 0)
	{
	  if (where)
	    gfc_error ("Actual argument for '%s' must be a pointer at %L",
		       f->sym->name, &a->expr->where);
	  return 0;
	}

    match:
      if (a == actual)
	na = i;

      new[i++] = a;
    }

  /* Make sure missing actual arguments are optional.  */
  i = 0;
  for (f = formal; f; f = f->next, i++)
    {
      if (new[i] != NULL)
	continue;
      if (!f->sym->attr.optional)
	{
	  if (where)
	    gfc_error ("Missing actual argument for argument '%s' at %L",
		       f->sym->name, where);
	  return 0;
	}
    }

  /* The argument lists are compatible.  We now relink a new actual
     argument list with null arguments in the right places.  The head
     of the list remains the head.  */
  for (i = 0; i < n; i++)
    if (new[i] == NULL)
      new[i] = gfc_get_actual_arglist ();

  if (na != 0)
    {
      temp = *new[0];
      *new[0] = *actual;
      *actual = temp;

      a = new[0];
      new[0] = new[na];
      new[na] = a;
    }

  for (i = 0; i < n - 1; i++)
    new[i]->next = new[i + 1];

  new[i]->next = NULL;

  if (*ap == NULL && n > 0)
    *ap = new[0];

  /* Note the types of omitted optional arguments.  */
  for (a = actual, f = formal; a; a = a->next, f = f->next)
    if (a->expr == NULL && a->label == NULL)
      a->missing_arg_type = f->sym->ts.type;

  return 1;
}


typedef struct
{
  gfc_formal_arglist *f;
  gfc_actual_arglist *a;
}
argpair;

/* qsort comparison function for argument pairs, with the following
   order:
    - p->a->expr == NULL
    - p->a->expr->expr_type != EXPR_VARIABLE
    - growing p->a->expr->symbol.  */

static int
pair_cmp (const void *p1, const void *p2)
{
  const gfc_actual_arglist *a1, *a2;

  /* *p1 and *p2 are elements of the to-be-sorted array.  */
  a1 = ((const argpair *) p1)->a;
  a2 = ((const argpair *) p2)->a;
  if (!a1->expr)
    {
      if (!a2->expr)
	return 0;
      return -1;
    }
  if (!a2->expr)
    return 1;
  if (a1->expr->expr_type != EXPR_VARIABLE)
    {
      if (a2->expr->expr_type != EXPR_VARIABLE)
	return 0;
      return -1;
    }
  if (a2->expr->expr_type != EXPR_VARIABLE)
    return 1;
  return a1->expr->symtree->n.sym < a2->expr->symtree->n.sym;
}


/* Given two expressions from some actual arguments, test whether they
   refer to the same expression. The analysis is conservative.
   Returning FAILURE will produce no warning.  */

static try
compare_actual_expr (gfc_expr * e1, gfc_expr * e2)
{
  const gfc_ref *r1, *r2;

  if (!e1 || !e2
      || e1->expr_type != EXPR_VARIABLE
      || e2->expr_type != EXPR_VARIABLE
      || e1->symtree->n.sym != e2->symtree->n.sym)
    return FAILURE;

  /* TODO: improve comparison, see expr.c:show_ref().  */
  for (r1 = e1->ref, r2 = e2->ref; r1 && r2; r1 = r1->next, r2 = r2->next)
    {
      if (r1->type != r2->type)
	return FAILURE;
      switch (r1->type)
	{
	case REF_ARRAY:
	  if (r1->u.ar.type != r2->u.ar.type)
	    return FAILURE;
	  /* TODO: At the moment, consider only full arrays;
	     we could do better.  */
	  if (r1->u.ar.type != AR_FULL || r2->u.ar.type != AR_FULL)
	    return FAILURE;
	  break;

	case REF_COMPONENT:
	  if (r1->u.c.component != r2->u.c.component)
	    return FAILURE;
	  break;

	case REF_SUBSTRING:
	  return FAILURE;

	default:
	  gfc_internal_error ("compare_actual_expr(): Bad component code");
	}
    }
  if (!r1 && !r2)
    return SUCCESS;
  return FAILURE;
}

/* Given formal and actual argument lists that correspond to one
   another, check that identical actual arguments aren't not
   associated with some incompatible INTENTs.  */

static try
check_some_aliasing (gfc_formal_arglist * f, gfc_actual_arglist * a)
{
  sym_intent f1_intent, f2_intent;
  gfc_formal_arglist *f1;
  gfc_actual_arglist *a1;
  size_t n, i, j;
  argpair *p;
  try t = SUCCESS;

  n = 0;
  for (f1 = f, a1 = a;; f1 = f1->next, a1 = a1->next)
    {
      if (f1 == NULL && a1 == NULL)
	break;
      if (f1 == NULL || a1 == NULL)
	gfc_internal_error ("check_some_aliasing(): List mismatch");
      n++;
    }
  if (n == 0)
    return t;
  p = (argpair *) alloca (n * sizeof (argpair));

  for (i = 0, f1 = f, a1 = a; i < n; i++, f1 = f1->next, a1 = a1->next)
    {
      p[i].f = f1;
      p[i].a = a1;
    }

  qsort (p, n, sizeof (argpair), pair_cmp);

  for (i = 0; i < n; i++)
    {
      if (!p[i].a->expr
	  || p[i].a->expr->expr_type != EXPR_VARIABLE
	  || p[i].a->expr->ts.type == BT_PROCEDURE)
	continue;
      f1_intent = p[i].f->sym->attr.intent;
      for (j = i + 1; j < n; j++)
	{
	  /* Expected order after the sort.  */
	  if (!p[j].a->expr || p[j].a->expr->expr_type != EXPR_VARIABLE)
	    gfc_internal_error ("check_some_aliasing(): corrupted data");

	  /* Are the expression the same?  */
	  if (compare_actual_expr (p[i].a->expr, p[j].a->expr) == FAILURE)
	    break;
	  f2_intent = p[j].f->sym->attr.intent;
	  if ((f1_intent == INTENT_IN && f2_intent == INTENT_OUT)
	      || (f1_intent == INTENT_OUT && f2_intent == INTENT_IN))
	    {
	      gfc_warning ("Same actual argument associated with INTENT(%s) "
			   "argument '%s' and INTENT(%s) argument '%s' at %L",
			   gfc_intent_string (f1_intent), p[i].f->sym->name,
			   gfc_intent_string (f2_intent), p[j].f->sym->name,
			   &p[i].a->expr->where);
	      t = FAILURE;
	    }
	}
    }

  return t;
}


/* Given formal and actual argument lists that correspond to one
   another, check that they are compatible in the sense that intents
   are not mismatched.  */

static try
check_intents (gfc_formal_arglist * f, gfc_actual_arglist * a)
{
  sym_intent a_intent, f_intent;

  for (;; f = f->next, a = a->next)
    {
      if (f == NULL && a == NULL)
	break;
      if (f == NULL || a == NULL)
	gfc_internal_error ("check_intents(): List mismatch");

      if (a->expr == NULL || a->expr->expr_type != EXPR_VARIABLE)
	continue;

      a_intent = a->expr->symtree->n.sym->attr.intent;
      f_intent = f->sym->attr.intent;

      if (a_intent == INTENT_IN
	  && (f_intent == INTENT_INOUT
	      || f_intent == INTENT_OUT))
	{

	  gfc_error ("Procedure argument at %L is INTENT(IN) while interface "
		     "specifies INTENT(%s)", &a->expr->where,
		     gfc_intent_string (f_intent));
	  return FAILURE;
	}

      if (gfc_pure (NULL) && gfc_impure_variable (a->expr->symtree->n.sym))
	{
	  if (f_intent == INTENT_INOUT || f_intent == INTENT_OUT)
	    {
	      gfc_error
		("Procedure argument at %L is local to a PURE procedure and "
		 "is passed to an INTENT(%s) argument", &a->expr->where,
		 gfc_intent_string (f_intent));
	      return FAILURE;
	    }

	  if (a->expr->symtree->n.sym->attr.pointer)
	    {
	      gfc_error
		("Procedure argument at %L is local to a PURE procedure and "
		 "has the POINTER attribute", &a->expr->where);
	      return FAILURE;
	    }
	}
    }

  return SUCCESS;
}


/* Check how a procedure is used against its interface.  If all goes
   well, the actual argument list will also end up being properly
   sorted.  */

void
gfc_procedure_use (gfc_symbol * sym, gfc_actual_arglist ** ap, locus * where)
{
  /* Warn about calls with an implicit interface.  */
  if (gfc_option.warn_implicit_interface
      && sym->attr.if_source == IFSRC_UNKNOWN)
    gfc_warning ("Procedure '%s' called with an implicit interface at %L",
                 sym->name, where);

  if (sym->attr.if_source == IFSRC_UNKNOWN
      || !compare_actual_formal (ap, sym->formal, 0,
			         sym->attr.elemental, where))
    return;

  check_intents (sym->formal, *ap);
  if (gfc_option.warn_aliasing)
    check_some_aliasing (sym->formal, *ap);
}


/* Given an interface pointer and an actual argument list, search for
   a formal argument list that matches the actual.  If found, returns
   a pointer to the symbol of the correct interface.  Returns NULL if
   not found.  */

gfc_symbol *
gfc_search_interface (gfc_interface * intr, int sub_flag,
		      gfc_actual_arglist ** ap)
{
  int r;

  for (; intr; intr = intr->next)
    {
      if (sub_flag && intr->sym->attr.function)
	continue;
      if (!sub_flag && intr->sym->attr.subroutine)
	continue;

      r = !intr->sym->attr.elemental;

      if (compare_actual_formal (ap, intr->sym->formal, r, !r, NULL))
	{
	  check_intents (intr->sym->formal, *ap);
	  if (gfc_option.warn_aliasing)
	    check_some_aliasing (intr->sym->formal, *ap);
	  return intr->sym;
	}
    }

  return NULL;
}


/* Do a brute force recursive search for a symbol.  */

static gfc_symtree *
find_symtree0 (gfc_symtree * root, gfc_symbol * sym)
{
  gfc_symtree * st;

  if (root->n.sym == sym)
    return root;

  st = NULL;
  if (root->left)
    st = find_symtree0 (root->left, sym);
  if (root->right && ! st)
    st = find_symtree0 (root->right, sym);
  return st;
}


/* Find a symtree for a symbol.  */

static gfc_symtree *
find_sym_in_symtree (gfc_symbol * sym)
{
  gfc_symtree *st;
  gfc_namespace *ns;

  /* First try to find it by name.  */
  gfc_find_sym_tree (sym->name, gfc_current_ns, 1, &st);
  if (st && st->n.sym == sym)
    return st;

  /* if it's been renamed, resort to a brute-force search.  */
  /* TODO: avoid having to do this search.  If the symbol doesn't exist
     in the symtree for the current namespace, it should probably be added.  */
  for (ns = gfc_current_ns; ns; ns = ns->parent)
    {
      st = find_symtree0 (ns->sym_root, sym);
      if (st)
        return st;
    }
  gfc_internal_error ("Unable to find symbol %s", sym->name);
  /* Not reached */
}


/* This subroutine is called when an expression is being resolved.
   The expression node in question is either a user defined operator
   or an intrinsic operator with arguments that aren't compatible
   with the operator.  This subroutine builds an actual argument list
   corresponding to the operands, then searches for a compatible
   interface.  If one is found, the expression node is replaced with
   the appropriate function call.  */

try
gfc_extend_expr (gfc_expr * e)
{
  gfc_actual_arglist *actual;
  gfc_symbol *sym;
  gfc_namespace *ns;
  gfc_user_op *uop;
  gfc_intrinsic_op i;

  sym = NULL;

  actual = gfc_get_actual_arglist ();
  actual->expr = e->value.op.op1;

  if (e->value.op.op2 != NULL)
    {
      actual->next = gfc_get_actual_arglist ();
      actual->next->expr = e->value.op.op2;
    }

  i = fold_unary (e->value.op.operator);

  if (i == INTRINSIC_USER)
    {
      for (ns = gfc_current_ns; ns; ns = ns->parent)
	{
	  uop = gfc_find_uop (e->value.op.uop->name, ns);
	  if (uop == NULL)
	    continue;

	  sym = gfc_search_interface (uop->operator, 0, &actual);
	  if (sym != NULL)
	    break;
	}
    }
  else
    {
      for (ns = gfc_current_ns; ns; ns = ns->parent)
	{
	  sym = gfc_search_interface (ns->operator[i], 0, &actual);
	  if (sym != NULL)
	    break;
	}
    }

  if (sym == NULL)
    {
      /* Don't use gfc_free_actual_arglist() */
      if (actual->next != NULL)
	gfc_free (actual->next);
      gfc_free (actual);

      return FAILURE;
    }

  /* Change the expression node to a function call.  */
  e->expr_type = EXPR_FUNCTION;
  e->symtree = find_sym_in_symtree (sym);
  e->value.function.actual = actual;
  e->value.function.esym = NULL;
  e->value.function.isym = NULL;

  if (gfc_pure (NULL) && !gfc_pure (sym))
    {
      gfc_error
	("Function '%s' called in lieu of an operator at %L must be PURE",
	 sym->name, &e->where);
      return FAILURE;
    }

  if (gfc_resolve_expr (e) == FAILURE)
    return FAILURE;

  return SUCCESS;
}


/* Tries to replace an assignment code node with a subroutine call to
   the subroutine associated with the assignment operator.  Return
   SUCCESS if the node was replaced.  On FAILURE, no error is
   generated.  */

try
gfc_extend_assign (gfc_code * c, gfc_namespace * ns)
{
  gfc_actual_arglist *actual;
  gfc_expr *lhs, *rhs;
  gfc_symbol *sym;

  lhs = c->expr;
  rhs = c->expr2;

  /* Don't allow an intrinsic assignment to be replaced.  */
  if (lhs->ts.type != BT_DERIVED && rhs->ts.type != BT_DERIVED
      && (lhs->ts.type == rhs->ts.type
          || (gfc_numeric_ts (&lhs->ts)
	      && gfc_numeric_ts (&rhs->ts))))
    return FAILURE;

  actual = gfc_get_actual_arglist ();
  actual->expr = lhs;

  actual->next = gfc_get_actual_arglist ();
  actual->next->expr = rhs;

  sym = NULL;

  for (; ns; ns = ns->parent)
    {
      sym = gfc_search_interface (ns->operator[INTRINSIC_ASSIGN], 1, &actual);
      if (sym != NULL)
	break;
    }

  if (sym == NULL)
    {
      gfc_free (actual->next);
      gfc_free (actual);
      return FAILURE;
    }

  /* Replace the assignment with the call.  */
  c->op = EXEC_CALL;
  c->symtree = find_sym_in_symtree (sym);
  c->expr = NULL;
  c->expr2 = NULL;
  c->ext.actual = actual;

  if (gfc_pure (NULL) && !gfc_pure (sym))
    {
      gfc_error ("Subroutine '%s' called in lieu of assignment at %L must be "
		 "PURE", sym->name, &c->loc);
      return FAILURE;
    }

  return SUCCESS;
}


/* Make sure that the interface just parsed is not already present in
   the given interface list.  Ambiguity isn't checked yet since module
   procedures can be present without interfaces.  */

static try
check_new_interface (gfc_interface * base, gfc_symbol * new)
{
  gfc_interface *ip;

  for (ip = base; ip; ip = ip->next)
    {
      if (ip->sym == new)
	{
	  gfc_error ("Entity '%s' at %C is already present in the interface",
		     new->name);
	  return FAILURE;
	}
    }

  return SUCCESS;
}


/* Add a symbol to the current interface.  */

try
gfc_add_interface (gfc_symbol * new)
{
  gfc_interface **head, *intr;
  gfc_namespace *ns;
  gfc_symbol *sym;

  switch (current_interface.type)
    {
    case INTERFACE_NAMELESS:
      return SUCCESS;

    case INTERFACE_INTRINSIC_OP:
      for (ns = current_interface.ns; ns; ns = ns->parent)
	if (check_new_interface (ns->operator[current_interface.op], new)
	    == FAILURE)
	  return FAILURE;

      head = &current_interface.ns->operator[current_interface.op];
      break;

    case INTERFACE_GENERIC:
      for (ns = current_interface.ns; ns; ns = ns->parent)
	{
	  gfc_find_symbol (current_interface.sym->name, ns, 0, &sym);
	  if (sym == NULL)
	    continue;

	  if (check_new_interface (sym->generic, new) == FAILURE)
	    return FAILURE;
	}

      head = &current_interface.sym->generic;
      break;

    case INTERFACE_USER_OP:
      if (check_new_interface (current_interface.uop->operator, new) ==
	  FAILURE)
	return FAILURE;

      head = &current_interface.uop->operator;
      break;

    default:
      gfc_internal_error ("gfc_add_interface(): Bad interface type");
    }

  intr = gfc_get_interface ();
  intr->sym = new;
  intr->where = gfc_current_locus;

  intr->next = *head;
  *head = intr;

  return SUCCESS;
}


/* Gets rid of a formal argument list.  We do not free symbols.
   Symbols are freed when a namespace is freed.  */

void
gfc_free_formal_arglist (gfc_formal_arglist * p)
{
  gfc_formal_arglist *q;

  for (; p; p = q)
    {
      q = p->next;
      gfc_free (p);
    }
}
