/* Deal with interfaces.
   Copyright (C) 2000, 2001, 2002, 2004, 2005, 2006, 2007, 2008, 2009
   Free Software Foundation, Inc.
   Contributed by Andy Vaught

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */


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
     has an explicit interface.  Each explicit interface has its own
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
gfc_free_interface (gfc_interface *intr)
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
fold_unary_intrinsic (gfc_intrinsic_op op)
{
  switch (op)
    {
    case INTRINSIC_UPLUS:
      op = INTRINSIC_PLUS;
      break;
    case INTRINSIC_UMINUS:
      op = INTRINSIC_MINUS;
      break;
    default:
      break;
    }

  return op;
}


/* Match a generic specification.  Depending on which type of
   interface is found, the 'name' or 'op' pointers may be set.
   This subroutine doesn't return MATCH_NO.  */

match
gfc_match_generic_spec (interface_type *type,
			char *name,
			gfc_intrinsic_op *op)
{
  char buffer[GFC_MAX_SYMBOL_LEN + 1];
  match m;
  gfc_intrinsic_op i;

  if (gfc_match (" assignment ( = )") == MATCH_YES)
    {
      *type = INTERFACE_INTRINSIC_OP;
      *op = INTRINSIC_ASSIGN;
      return MATCH_YES;
    }

  if (gfc_match (" operator ( %o )", &i) == MATCH_YES)
    {				/* Operator i/f */
      *type = INTERFACE_INTRINSIC_OP;
      *op = fold_unary_intrinsic (i);
      return MATCH_YES;
    }

  *op = INTRINSIC_NONE;
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


/* Match one of the five F95 forms of an interface statement.  The
   matcher for the abstract interface follows.  */

match
gfc_match_interface (void)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  interface_type type;
  gfc_symbol *sym;
  gfc_intrinsic_op op;
  match m;

  m = gfc_match_space ();

  if (gfc_match_generic_spec (&type, name, &op) == MATCH_ERROR)
    return MATCH_ERROR;

  /* If we're not looking at the end of the statement now, or if this
     is not a nameless interface but we did not see a space, punt.  */
  if (gfc_match_eos () != MATCH_YES
      || (type != INTERFACE_NAMELESS && m != MATCH_YES))
    {
      gfc_error ("Syntax error: Trailing garbage in INTERFACE statement "
		 "at %C");
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

      if (sym->attr.dummy)
	{
	  gfc_error ("Dummy procedure '%s' at %C cannot have a "
		     "generic interface", sym->name);
	  return MATCH_ERROR;
	}

      current_interface.sym = gfc_new_block = sym;
      break;

    case INTERFACE_USER_OP:
      current_interface.uop = gfc_get_uop (name);
      break;

    case INTERFACE_INTRINSIC_OP:
      current_interface.op = op;
      break;

    case INTERFACE_NAMELESS:
    case INTERFACE_ABSTRACT:
      break;
    }

  return MATCH_YES;
}



/* Match a F2003 abstract interface.  */

match
gfc_match_abstract_interface (void)
{
  match m;

  if (gfc_notify_std (GFC_STD_F2003, "Fortran 2003: ABSTRACT INTERFACE at %C")
		      == FAILURE)
    return MATCH_ERROR;

  m = gfc_match_eos ();

  if (m != MATCH_YES)
    {
      gfc_error ("Syntax error in ABSTRACT INTERFACE statement at %C");
      return MATCH_ERROR;
    }

  current_interface.type = INTERFACE_ABSTRACT;

  return m;
}


/* Match the different sort of generic-specs that can be present after
   the END INTERFACE itself.  */

match
gfc_match_end_interface (void)
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  interface_type type;
  gfc_intrinsic_op op;
  match m;

  m = gfc_match_space ();

  if (gfc_match_generic_spec (&type, name, &op) == MATCH_ERROR)
    return MATCH_ERROR;

  /* If we're not looking at the end of the statement now, or if this
     is not a nameless interface but we did not see a space, punt.  */
  if (gfc_match_eos () != MATCH_YES
      || (type != INTERFACE_NAMELESS && m != MATCH_YES))
    {
      gfc_error ("Syntax error: Trailing garbage in END INTERFACE "
		 "statement at %C");
      return MATCH_ERROR;
    }

  m = MATCH_YES;

  switch (current_interface.type)
    {
    case INTERFACE_NAMELESS:
    case INTERFACE_ABSTRACT:
      if (type != INTERFACE_NAMELESS)
	{
	  gfc_error ("Expected a nameless interface at %C");
	  m = MATCH_ERROR;
	}

      break;

    case INTERFACE_INTRINSIC_OP:
      if (type != current_interface.type || op != current_interface.op)
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
		     current_interface.uop->name);
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


/* Compare two derived types using the criteria in 4.4.2 of the standard,
   recursing through gfc_compare_types for the components.  */

int
gfc_compare_derived_types (gfc_symbol *derived1, gfc_symbol *derived2)
{
  gfc_component *dt1, *dt2;

  /* Special case for comparing derived types across namespaces.  If the
     true names and module names are the same and the module name is
     nonnull, then they are equal.  */
  if (derived1 != NULL && derived2 != NULL
      && strcmp (derived1->name, derived2->name) == 0
      && derived1->module != NULL && derived2->module != NULL
      && strcmp (derived1->module, derived2->module) == 0)
    return 1;

  /* Compare type via the rules of the standard.  Both types must have
     the SEQUENCE attribute to be equal.  */

  if (strcmp (derived1->name, derived2->name))
    return 0;

  if (derived1->component_access == ACCESS_PRIVATE
      || derived2->component_access == ACCESS_PRIVATE)
    return 0;

  if (derived1->attr.sequence == 0 || derived2->attr.sequence == 0)
    return 0;

  dt1 = derived1->components;
  dt2 = derived2->components;

  /* Since subtypes of SEQUENCE types must be SEQUENCE types as well, a
     simple test can speed things up.  Otherwise, lots of things have to
     match.  */
  for (;;)
    {
      if (strcmp (dt1->name, dt2->name) != 0)
	return 0;

      if (dt1->attr.access != dt2->attr.access)
	return 0;

      if (dt1->attr.pointer != dt2->attr.pointer)
	return 0;

      if (dt1->attr.dimension != dt2->attr.dimension)
	return 0;

     if (dt1->attr.allocatable != dt2->attr.allocatable)
	return 0;

      if (dt1->attr.dimension && gfc_compare_array_spec (dt1->as, dt2->as) == 0)
	return 0;

      /* Make sure that link lists do not put this function into an 
	 endless recursive loop!  */
      if (!(dt1->ts.type == BT_DERIVED && derived1 == dt1->ts.derived)
	    && !(dt1->ts.type == BT_DERIVED && derived1 == dt1->ts.derived)
	    && gfc_compare_types (&dt1->ts, &dt2->ts) == 0)
	return 0;

      else if ((dt1->ts.type == BT_DERIVED && derived1 == dt1->ts.derived)
		&& !(dt1->ts.type == BT_DERIVED && derived1 == dt1->ts.derived))
	return 0;

      else if (!(dt1->ts.type == BT_DERIVED && derived1 == dt1->ts.derived)
		&& (dt1->ts.type == BT_DERIVED && derived1 == dt1->ts.derived))
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


/* Compare two typespecs, recursively if necessary.  */

int
gfc_compare_types (gfc_typespec *ts1, gfc_typespec *ts2)
{
  /* See if one of the typespecs is a BT_VOID, which is what is being used
     to allow the funcs like c_f_pointer to accept any pointer type.
     TODO: Possibly should narrow this to just the one typespec coming in
     that is for the formal arg, but oh well.  */
  if (ts1->type == BT_VOID || ts2->type == BT_VOID)
    return 1;
   
  if (ts1->type != ts2->type)
    return 0;
  if (ts1->type != BT_DERIVED)
    return (ts1->kind == ts2->kind);

  /* Compare derived types.  */
  if (ts1->derived == ts2->derived)
    return 1;

  return gfc_compare_derived_types (ts1->derived ,ts2->derived);
}


/* Given two symbols that are formal arguments, compare their ranks
   and types.  Returns nonzero if they have the same rank and type,
   zero otherwise.  */

static int
compare_type_rank (gfc_symbol *s1, gfc_symbol *s2)
{
  int r1, r2;

  r1 = (s1->as != NULL) ? s1->as->rank : 0;
  r2 = (s2->as != NULL) ? s2->as->rank : 0;

  if (r1 != r2)
    return 0;			/* Ranks differ.  */

  return gfc_compare_types (&s1->ts, &s2->ts);
}


/* Given two symbols that are formal arguments, compare their types
   and rank and their formal interfaces if they are both dummy
   procedures.  Returns nonzero if the same, zero if different.  */

static int
compare_type_rank_if (gfc_symbol *s1, gfc_symbol *s2)
{
  if (s1 == NULL || s2 == NULL)
    return s1 == s2 ? 1 : 0;

  if (s1 == s2)
    return 1;

  if (s1->attr.flavor != FL_PROCEDURE && s2->attr.flavor != FL_PROCEDURE)
    return compare_type_rank (s1, s2);

  if (s1->attr.flavor != FL_PROCEDURE || s2->attr.flavor != FL_PROCEDURE)
    return 0;

  /* At this point, both symbols are procedures.  It can happen that
     external procedures are compared, where one is identified by usage
     to be a function or subroutine but the other is not.  Check TKR
     nonetheless for these cases.  */
  if (s1->attr.function == 0 && s1->attr.subroutine == 0)
    return s1->attr.external == 1 ? compare_type_rank (s1, s2) : 0;

  if (s2->attr.function == 0 && s2->attr.subroutine == 0)
    return s2->attr.external == 1 ? compare_type_rank (s1, s2) : 0;

  /* Now the type of procedure has been identified.  */
  if (s1->attr.function != s2->attr.function
      || s1->attr.subroutine != s2->attr.subroutine)
    return 0;

  if (s1->attr.function && compare_type_rank (s1, s2) == 0)
    return 0;

  /* Originally, gfortran recursed here to check the interfaces of passed
     procedures.  This is explicitly not required by the standard.  */
  return 1;
}


/* Given a formal argument list and a keyword name, search the list
   for that keyword.  Returns the correct symbol node if found, NULL
   if not found.  */

static gfc_symbol *
find_keyword_arg (const char *name, gfc_formal_arglist *f)
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
check_operator_interface (gfc_interface *intr, gfc_intrinsic_op op)
{
  gfc_formal_arglist *formal;
  sym_intent i1, i2;
  gfc_symbol *sym;
  bt t1, t2;
  int args, r1, r2, k1, k2;

  if (intr == NULL)
    return;

  args = 0;
  t1 = t2 = BT_UNKNOWN;
  i1 = i2 = INTENT_UNKNOWN;
  r1 = r2 = -1;
  k1 = k2 = -1;

  for (formal = intr->sym->formal; formal; formal = formal->next)
    {
      sym = formal->sym;
      if (sym == NULL)
	{
	  gfc_error ("Alternate return cannot appear in operator "
		     "interface at %L", &intr->sym->declared_at);
	  return;
	}
      if (args == 0)
	{
	  t1 = sym->ts.type;
	  i1 = sym->attr.intent;
	  r1 = (sym->as != NULL) ? sym->as->rank : 0;
	  k1 = sym->ts.kind;
	}
      if (args == 1)
	{
	  t2 = sym->ts.type;
	  i2 = sym->attr.intent;
	  r2 = (sym->as != NULL) ? sym->as->rank : 0;
	  k2 = sym->ts.kind;
	}
      args++;
    }

  sym = intr->sym;

  /* Only +, - and .not. can be unary operators.
     .not. cannot be a binary operator.  */
  if (args == 0 || args > 2 || (args == 1 && op != INTRINSIC_PLUS
				&& op != INTRINSIC_MINUS
				&& op != INTRINSIC_NOT)
      || (args == 2 && op == INTRINSIC_NOT))
    {
      gfc_error ("Operator interface at %L has the wrong number of arguments",
		 &intr->sym->declared_at);
      return;
    }

  /* Check that intrinsics are mapped to functions, except
     INTRINSIC_ASSIGN which should map to a subroutine.  */
  if (op == INTRINSIC_ASSIGN)
    {
      if (!sym->attr.subroutine)
	{
	  gfc_error ("Assignment operator interface at %L must be "
		     "a SUBROUTINE", &intr->sym->declared_at);
	  return;
	}
      if (args != 2)
	{
	  gfc_error ("Assignment operator interface at %L must have "
		     "two arguments", &intr->sym->declared_at);
	  return;
	}

      /* Allowed are (per F2003, 12.3.2.1.2 Defined assignments):
         - First argument an array with different rank than second,
         - Types and kinds do not conform, and
         - First argument is of derived type.  */
      if (sym->formal->sym->ts.type != BT_DERIVED
	  && (r1 == 0 || r1 == r2)
	  && (sym->formal->sym->ts.type == sym->formal->next->sym->ts.type
	      || (gfc_numeric_ts (&sym->formal->sym->ts)
		  && gfc_numeric_ts (&sym->formal->next->sym->ts))))
	{
	  gfc_error ("Assignment operator interface at %L must not redefine "
		     "an INTRINSIC type assignment", &intr->sym->declared_at);
	  return;
	}
    }
  else
    {
      if (!sym->attr.function)
	{
	  gfc_error ("Intrinsic operator interface at %L must be a FUNCTION",
		     &intr->sym->declared_at);
	  return;
	}
    }

  /* Check intents on operator interfaces.  */
  if (op == INTRINSIC_ASSIGN)
    {
      if (i1 != INTENT_OUT && i1 != INTENT_INOUT)
	gfc_error ("First argument of defined assignment at %L must be "
		   "INTENT(OUT) or INTENT(INOUT)", &intr->sym->declared_at);

      if (i2 != INTENT_IN)
	gfc_error ("Second argument of defined assignment at %L must be "
		   "INTENT(IN)", &intr->sym->declared_at);
    }
  else
    {
      if (i1 != INTENT_IN)
	gfc_error ("First argument of operator interface at %L must be "
		   "INTENT(IN)", &intr->sym->declared_at);

      if (args == 2 && i2 != INTENT_IN)
	gfc_error ("Second argument of operator interface at %L must be "
		   "INTENT(IN)", &intr->sym->declared_at);
    }

  /* From now on, all we have to do is check that the operator definition
     doesn't conflict with an intrinsic operator. The rules for this
     game are defined in 7.1.2 and 7.1.3 of both F95 and F2003 standards,
     as well as 12.3.2.1.1 of Fortran 2003:

     "If the operator is an intrinsic-operator (R310), the number of
     function arguments shall be consistent with the intrinsic uses of
     that operator, and the types, kind type parameters, or ranks of the
     dummy arguments shall differ from those required for the intrinsic
     operation (7.1.2)."  */

#define IS_NUMERIC_TYPE(t) \
  ((t) == BT_INTEGER || (t) == BT_REAL || (t) == BT_COMPLEX)

  /* Unary ops are easy, do them first.  */
  if (op == INTRINSIC_NOT)
    {
      if (t1 == BT_LOGICAL)
	goto bad_repl;
      else
	return;
    }

  if (args == 1 && (op == INTRINSIC_PLUS || op == INTRINSIC_MINUS))
    {
      if (IS_NUMERIC_TYPE (t1))
	goto bad_repl;
      else
	return;
    }

  /* Character intrinsic operators have same character kind, thus
     operator definitions with operands of different character kinds
     are always safe.  */
  if (t1 == BT_CHARACTER && t2 == BT_CHARACTER && k1 != k2)
    return;

  /* Intrinsic operators always perform on arguments of same rank,
     so different ranks is also always safe.  (rank == 0) is an exception
     to that, because all intrinsic operators are elemental.  */
  if (r1 != r2 && r1 != 0 && r2 != 0)
    return;

  switch (op)
  {
    case INTRINSIC_EQ:
    case INTRINSIC_EQ_OS:
    case INTRINSIC_NE:
    case INTRINSIC_NE_OS:
      if (t1 == BT_CHARACTER && t2 == BT_CHARACTER)
	goto bad_repl;
      /* Fall through.  */

    case INTRINSIC_PLUS:
    case INTRINSIC_MINUS:
    case INTRINSIC_TIMES:
    case INTRINSIC_DIVIDE:
    case INTRINSIC_POWER:
      if (IS_NUMERIC_TYPE (t1) && IS_NUMERIC_TYPE (t2))
	goto bad_repl;
      break;

    case INTRINSIC_GT:
    case INTRINSIC_GT_OS:
    case INTRINSIC_GE:
    case INTRINSIC_GE_OS:
    case INTRINSIC_LT:
    case INTRINSIC_LT_OS:
    case INTRINSIC_LE:
    case INTRINSIC_LE_OS:
      if (t1 == BT_CHARACTER && t2 == BT_CHARACTER)
	goto bad_repl;
      if ((t1 == BT_INTEGER || t1 == BT_REAL)
	  && (t2 == BT_INTEGER || t2 == BT_REAL))
	goto bad_repl;
      break;

    case INTRINSIC_CONCAT:
      if (t1 == BT_CHARACTER && t2 == BT_CHARACTER)
	goto bad_repl;
      break;

    case INTRINSIC_AND:
    case INTRINSIC_OR:
    case INTRINSIC_EQV:
    case INTRINSIC_NEQV:
      if (t1 == BT_LOGICAL && t2 == BT_LOGICAL)
	goto bad_repl;
      break;

    default:
      break;
  }

  return;

#undef IS_NUMERIC_TYPE

bad_repl:
  gfc_error ("Operator interface at %L conflicts with intrinsic interface",
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
   14.1.2.3 in the Fortran 95 standard.  */

static int
count_types_test (gfc_formal_arglist *f1, gfc_formal_arglist *f2)
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
  arg = XCNEWVEC (arginfo, n1);

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

      if (arg[i].sym && arg[i].sym->attr.optional)
	continue;		/* Skip optional arguments.  */

      arg[i].flag = k;

      /* Find other nonoptional arguments of the same type/rank.  */
      for (j = i + 1; j < n1; j++)
	if ((arg[j].sym == NULL || !arg[j].sym->attr.optional)
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


/* Perform the correspondence test in rule 2 of section 14.1.2.3.
   Returns zero if no argument is found that satisfies rule 2, nonzero
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
generic_correspondence (gfc_formal_arglist *f1, gfc_formal_arglist *f2)
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
   would be ambiguous between the two interfaces, zero otherwise.
   'intent_flag' specifies whether INTENT and OPTIONAL of the arguments are
   required to match, which is not the case for ambiguity checks.*/

int
gfc_compare_interfaces (gfc_symbol *s1, gfc_symbol *s2, int generic_flag,
			int intent_flag, char *errmsg, int err_len)
{
  gfc_formal_arglist *f1, *f2;

  if (s1->attr.function && (s2->attr.subroutine
      || (!s2->attr.function && s2->ts.type == BT_UNKNOWN
	  && gfc_get_default_type (s2->name, s2->ns)->type == BT_UNKNOWN)))
    {
      if (errmsg != NULL)
	snprintf (errmsg, err_len, "'%s' is not a function", s2->name);
      return 0;
    }

  if (s1->attr.subroutine && s2->attr.function)
    {
      if (errmsg != NULL)
	snprintf (errmsg, err_len, "'%s' is not a subroutine", s2->name);
      return 0;
    }

  /* If the arguments are functions, check type and kind
     (only for dummy procedures and procedure pointer assignments).  */
  if ((s1->attr.dummy || s1->attr.proc_pointer)
      && s1->attr.function && s2->attr.function)
    {
      if (s1->ts.type == BT_UNKNOWN)
	return 1;
      if ((s1->ts.type != s2->ts.type) || (s1->ts.kind != s2->ts.kind))
	{
	  if (errmsg != NULL)
	    snprintf (errmsg, err_len, "Type/kind mismatch in return value "
		      "of '%s'", s2->name);
	  return 0;
	}
    }

  if (s1->attr.if_source == IFSRC_UNKNOWN
      || s2->attr.if_source == IFSRC_UNKNOWN)
    return 1;

  f1 = s1->formal;
  f2 = s2->formal;

  if (f1 == NULL && f2 == NULL)
    return 1;			/* Special case: No arguments.  */

  if (generic_flag)
    {
      if (count_types_test (f1, f2) || count_types_test (f2, f1))
	return 0;
      if (generic_correspondence (f1, f2) || generic_correspondence (f2, f1))
	return 0;
    }
  else
    /* Perform the abbreviated correspondence test for operators (the
       arguments cannot be optional and are always ordered correctly).
       This is also done when comparing interfaces for dummy procedures and in
       procedure pointer assignments.  */

    for (;;)
      {
	/* Check existence.  */
	if (f1 == NULL && f2 == NULL)
	  break;
	if (f1 == NULL || f2 == NULL)
	  {
	    if (errmsg != NULL)
	      snprintf (errmsg, err_len, "'%s' has the wrong number of "
			"arguments", s2->name);
	    return 0;
	  }

	/* Check type and rank.  */
	if (!compare_type_rank (f1->sym, f2->sym))
	  {
	    if (errmsg != NULL)
	      snprintf (errmsg, err_len, "Type/rank mismatch in argument '%s'",
			f1->sym->name);
	    return 0;
	  }

	/* Check INTENT.  */
	if (intent_flag && (f1->sym->attr.intent != f2->sym->attr.intent))
	  {
	    snprintf (errmsg, err_len, "INTENT mismatch in argument '%s'",
		      f1->sym->name);
	    return 0;
	  }

	/* Check OPTIONAL.  */
	if (intent_flag && (f1->sym->attr.optional != f2->sym->attr.optional))
	  {
	    snprintf (errmsg, err_len, "OPTIONAL mismatch in argument '%s'",
		      f1->sym->name);
	    return 0;
	  }

	f1 = f1->next;
	f2 = f2->next;
      }

  return 1;
}


/* Given a pointer to an interface pointer, remove duplicate
   interfaces and make sure that all symbols are either functions or
   subroutines.  Returns nonzero if something goes wrong.  */

static int
check_interface0 (gfc_interface *p, const char *interface_name)
{
  gfc_interface *psave, *q, *qlast;

  psave = p;
  /* Make sure all symbols in the interface have been defined as
     functions or subroutines.  */
  for (; p; p = p->next)
    if ((!p->sym->attr.function && !p->sym->attr.subroutine)
	|| !p->sym->attr.if_source)
      {
	if (p->sym->attr.external)
	  gfc_error ("Procedure '%s' in %s at %L has no explicit interface",
		     p->sym->name, interface_name, &p->sym->declared_at);
	else
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
	      /* Duplicate interface.  */
	      qlast->next = q->next;
	      gfc_free (q);
	      q = qlast->next;
	    }
	}
    }

  return 0;
}


/* Check lists of interfaces to make sure that no two interfaces are
   ambiguous.  Duplicate interfaces (from the same symbol) are OK here.  */

static int
check_interface1 (gfc_interface *p, gfc_interface *q0,
		  int generic_flag, const char *interface_name,
		  bool referenced)
{
  gfc_interface *q;
  for (; p; p = p->next)
    for (q = q0; q; q = q->next)
      {
	if (p->sym == q->sym)
	  continue;		/* Duplicates OK here.  */

	if (p->sym->name == q->sym->name && p->sym->module == q->sym->module)
	  continue;

	if (gfc_compare_interfaces (p->sym, q->sym, generic_flag, 0, NULL, 0))
	  {
	    if (referenced)
	      {
		gfc_error ("Ambiguous interfaces '%s' and '%s' in %s at %L",
			   p->sym->name, q->sym->name, interface_name,
			   &p->where);
	      }

	    if (!p->sym->attr.use_assoc && q->sym->attr.use_assoc)
	      gfc_warning ("Ambiguous interfaces '%s' and '%s' in %s at %L",
			   p->sym->name, q->sym->name, interface_name,
			   &p->where);
	    return 1;
	  }
      }
  return 0;
}


/* Check the generic and operator interfaces of symbols to make sure
   that none of the interfaces conflict.  The check has to be done
   after all of the symbols are actually loaded.  */

static void
check_sym_interfaces (gfc_symbol *sym)
{
  char interface_name[100];
  bool k;
  gfc_interface *p;

  if (sym->ns != gfc_current_ns)
    return;

  if (sym->generic != NULL)
    {
      sprintf (interface_name, "generic interface '%s'", sym->name);
      if (check_interface0 (sym->generic, interface_name))
	return;

      for (p = sym->generic; p; p = p->next)
	{
	  if (p->sym->attr.mod_proc
	      && (p->sym->attr.if_source != IFSRC_DECL
		  || p->sym->attr.procedure))
	    {
	      gfc_error ("'%s' at %L is not a module procedure",
			 p->sym->name, &p->where);
	      return;
	    }
	}

      /* Originally, this test was applied to host interfaces too;
	 this is incorrect since host associated symbols, from any
	 source, cannot be ambiguous with local symbols.  */
      k = sym->attr.referenced || !sym->attr.use_assoc;
      if (check_interface1 (sym->generic, sym->generic, 1, interface_name, k))
	sym->attr.ambiguous_interfaces = 1;
    }
}


static void
check_uop_interfaces (gfc_user_op *uop)
{
  char interface_name[100];
  gfc_user_op *uop2;
  gfc_namespace *ns;

  sprintf (interface_name, "operator interface '%s'", uop->name);
  if (check_interface0 (uop->op, interface_name))
    return;

  for (ns = gfc_current_ns; ns; ns = ns->parent)
    {
      uop2 = gfc_find_uop (uop->name, ns);
      if (uop2 == NULL)
	continue;

      check_interface1 (uop->op, uop2->op, 0,
			interface_name, true);
    }
}


/* For the namespace, check generic, user operator and intrinsic
   operator interfaces for consistency and to remove duplicate
   interfaces.  We traverse the whole namespace, counting on the fact
   that most symbols will not have generic or operator interfaces.  */

void
gfc_check_interfaces (gfc_namespace *ns)
{
  gfc_namespace *old_ns, *ns2;
  char interface_name[100];
  int i;

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
		 gfc_op2string ((gfc_intrinsic_op) i));

      if (check_interface0 (ns->op[i], interface_name))
	continue;

      check_operator_interface (ns->op[i], (gfc_intrinsic_op) i);

      for (ns2 = ns; ns2; ns2 = ns2->parent)
	{
	  if (check_interface1 (ns->op[i], ns2->op[i], 0,
				interface_name, true))
	    goto done;

	  switch (i)
	    {
	      case INTRINSIC_EQ:
		if (check_interface1 (ns->op[i], ns2->op[INTRINSIC_EQ_OS],
				      0, interface_name, true)) goto done;
		break;

	      case INTRINSIC_EQ_OS:
		if (check_interface1 (ns->op[i], ns2->op[INTRINSIC_EQ],
				      0, interface_name, true)) goto done;
		break;

	      case INTRINSIC_NE:
		if (check_interface1 (ns->op[i], ns2->op[INTRINSIC_NE_OS],
				      0, interface_name, true)) goto done;
		break;

	      case INTRINSIC_NE_OS:
		if (check_interface1 (ns->op[i], ns2->op[INTRINSIC_NE],
				      0, interface_name, true)) goto done;
		break;

	      case INTRINSIC_GT:
		if (check_interface1 (ns->op[i], ns2->op[INTRINSIC_GT_OS],
				      0, interface_name, true)) goto done;
		break;

	      case INTRINSIC_GT_OS:
		if (check_interface1 (ns->op[i], ns2->op[INTRINSIC_GT],
				      0, interface_name, true)) goto done;
		break;

	      case INTRINSIC_GE:
		if (check_interface1 (ns->op[i], ns2->op[INTRINSIC_GE_OS],
				      0, interface_name, true)) goto done;
		break;

	      case INTRINSIC_GE_OS:
		if (check_interface1 (ns->op[i], ns2->op[INTRINSIC_GE],
				      0, interface_name, true)) goto done;
		break;

	      case INTRINSIC_LT:
		if (check_interface1 (ns->op[i], ns2->op[INTRINSIC_LT_OS],
				      0, interface_name, true)) goto done;
		break;

	      case INTRINSIC_LT_OS:
		if (check_interface1 (ns->op[i], ns2->op[INTRINSIC_LT],
				      0, interface_name, true)) goto done;
		break;

	      case INTRINSIC_LE:
		if (check_interface1 (ns->op[i], ns2->op[INTRINSIC_LE_OS],
				      0, interface_name, true)) goto done;
		break;

	      case INTRINSIC_LE_OS:
		if (check_interface1 (ns->op[i], ns2->op[INTRINSIC_LE],
				      0, interface_name, true)) goto done;
		break;

	      default:
		break;
            }
	}
    }

done:
  gfc_current_ns = old_ns;
}


static int
symbol_rank (gfc_symbol *sym)
{
  return (sym->as == NULL) ? 0 : sym->as->rank;
}


/* Given a symbol of a formal argument list and an expression, if the
   formal argument is allocatable, check that the actual argument is
   allocatable. Returns nonzero if compatible, zero if not compatible.  */

static int
compare_allocatable (gfc_symbol *formal, gfc_expr *actual)
{
  symbol_attribute attr;

  if (formal->attr.allocatable)
    {
      attr = gfc_expr_attr (actual);
      if (!attr.allocatable)
	return 0;
    }

  return 1;
}


/* Given a symbol of a formal argument list and an expression, if the
   formal argument is a pointer, see if the actual argument is a
   pointer. Returns nonzero if compatible, zero if not compatible.  */

static int
compare_pointer (gfc_symbol *formal, gfc_expr *actual)
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
compare_parameter (gfc_symbol *formal, gfc_expr *actual,
		   int ranks_must_agree, int is_elemental, locus *where)
{
  gfc_ref *ref;
  bool rank_check;

  /* If the formal arg has type BT_VOID, it's to one of the iso_c_binding
     procs c_f_pointer or c_f_procpointer, and we need to accept most
     pointers the user could give us.  This should allow that.  */
  if (formal->ts.type == BT_VOID)
    return 1;

  if (formal->ts.type == BT_DERIVED
      && formal->ts.derived && formal->ts.derived->ts.is_iso_c
      && actual->ts.type == BT_DERIVED
      && actual->ts.derived && actual->ts.derived->ts.is_iso_c)
    return 1;

  if (actual->ts.type == BT_PROCEDURE)
    {
      char err[200];
      gfc_symbol *act_sym = actual->symtree->n.sym;

      if (formal->attr.flavor != FL_PROCEDURE)
	{
	  if (where)
	    gfc_error ("Invalid procedure argument at %L", &actual->where);
	  return 0;
	}

      if (!gfc_compare_interfaces (formal, act_sym, 0, 1, err,
				   sizeof(err)))
	{
	  if (where)
	    gfc_error ("Interface mismatch in dummy procedure '%s' at %L: %s",
		       formal->name, &actual->where, err);
	  return 0;
	}

      if (formal->attr.function && !act_sym->attr.function)
	{
	  gfc_add_function (&act_sym->attr, act_sym->name,
	  &act_sym->declared_at);
	  if (act_sym->ts.type == BT_UNKNOWN
	      && gfc_set_default_type (act_sym, 1, act_sym->ns) == FAILURE)
	    return 0;
	}
      else if (formal->attr.subroutine && !act_sym->attr.subroutine)
	gfc_add_subroutine (&act_sym->attr, act_sym->name,
			    &act_sym->declared_at);

      return 1;
    }

  if ((actual->expr_type != EXPR_NULL || actual->ts.type != BT_UNKNOWN)
      && !gfc_compare_types (&formal->ts, &actual->ts))
    {
      if (where)
	gfc_error ("Type mismatch in argument '%s' at %L; passed %s to %s",
		   formal->name, &actual->where, gfc_typename (&actual->ts),
		   gfc_typename (&formal->ts));
      return 0;
    }

  if (symbol_rank (formal) == actual->rank)
    return 1;

  rank_check = where != NULL && !is_elemental && formal->as
	       && (formal->as->type == AS_ASSUMED_SHAPE
		   || formal->as->type == AS_DEFERRED);

  if (rank_check || ranks_must_agree || formal->attr.pointer
      || (actual->rank != 0 && !(is_elemental || formal->attr.dimension))
      || (actual->rank == 0 && formal->as->type == AS_ASSUMED_SHAPE))
    {
      if (where)
	gfc_error ("Rank mismatch in argument '%s' at %L (%d and %d)",
		   formal->name, &actual->where, symbol_rank (formal),
		   actual->rank);
      return 0;
    }
  else if (actual->rank != 0 && (is_elemental || formal->attr.dimension))
    return 1;

  /* At this point, we are considering a scalar passed to an array.   This
     is valid (cf. F95 12.4.1.1; F2003 12.4.1.2),
     - if the actual argument is (a substring of) an element of a
       non-assumed-shape/non-pointer array;
     - (F2003) if the actual argument is of type character.  */

  for (ref = actual->ref; ref; ref = ref->next)
    if (ref->type == REF_ARRAY && ref->u.ar.type == AR_ELEMENT)
      break;

  /* Not an array element.  */
  if (formal->ts.type == BT_CHARACTER
      && (ref == NULL
          || (actual->expr_type == EXPR_VARIABLE
	      && (actual->symtree->n.sym->as->type == AS_ASSUMED_SHAPE
		  || actual->symtree->n.sym->attr.pointer))))
    {
      if (where && (gfc_option.allow_std & GFC_STD_F2003) == 0)
	{
	  gfc_error ("Fortran 2003: Scalar CHARACTER actual argument with "
		     "array dummy argument '%s' at %L",
		     formal->name, &actual->where);
	  return 0;
	}
      else if ((gfc_option.allow_std & GFC_STD_F2003) == 0)
	return 0;
      else
	return 1;
    }
  else if (ref == NULL)
    {
      if (where)
	gfc_error ("Rank mismatch in argument '%s' at %L (%d and %d)",
		   formal->name, &actual->where, symbol_rank (formal),
		   actual->rank);
      return 0;
    }

  if (actual->expr_type == EXPR_VARIABLE
      && actual->symtree->n.sym->as
      && (actual->symtree->n.sym->as->type == AS_ASSUMED_SHAPE
	  || actual->symtree->n.sym->attr.pointer))
    {
      if (where)
	gfc_error ("Element of assumed-shaped array passed to dummy "
		   "argument '%s' at %L", formal->name, &actual->where);
      return 0;
    }

  return 1;
}


/* Given a symbol of a formal argument list and an expression, see if
   the two are compatible as arguments.  Returns nonzero if
   compatible, zero if not compatible.  */

static int
compare_parameter_protected (gfc_symbol *formal, gfc_expr *actual)
{
  if (actual->expr_type != EXPR_VARIABLE)
    return 1;

  if (!actual->symtree->n.sym->attr.is_protected)
    return 1;

  if (!actual->symtree->n.sym->attr.use_assoc)
    return 1;

  if (formal->attr.intent == INTENT_IN
      || formal->attr.intent == INTENT_UNKNOWN)
    return 1;

  if (!actual->symtree->n.sym->attr.pointer)
    return 0;

  if (actual->symtree->n.sym->attr.pointer && formal->attr.pointer)
    return 0;

  return 1;
}


/* Returns the storage size of a symbol (formal argument) or
   zero if it cannot be determined.  */

static unsigned long
get_sym_storage_size (gfc_symbol *sym)
{
  int i;
  unsigned long strlen, elements;

  if (sym->ts.type == BT_CHARACTER)
    {
      if (sym->ts.cl && sym->ts.cl->length
          && sym->ts.cl->length->expr_type == EXPR_CONSTANT)
	strlen = mpz_get_ui (sym->ts.cl->length->value.integer);
      else
	return 0;
    }
  else
    strlen = 1; 

  if (symbol_rank (sym) == 0)
    return strlen;

  elements = 1;
  if (sym->as->type != AS_EXPLICIT)
    return 0;
  for (i = 0; i < sym->as->rank; i++)
    {
      if (!sym->as || sym->as->upper[i]->expr_type != EXPR_CONSTANT
	  || sym->as->lower[i]->expr_type != EXPR_CONSTANT)
	return 0;

      elements *= mpz_get_ui (sym->as->upper[i]->value.integer)
		  - mpz_get_ui (sym->as->lower[i]->value.integer) + 1L;
    }

  return strlen*elements;
}


/* Returns the storage size of an expression (actual argument) or
   zero if it cannot be determined. For an array element, it returns
   the remaining size as the element sequence consists of all storage
   units of the actual argument up to the end of the array.  */

static unsigned long
get_expr_storage_size (gfc_expr *e)
{
  int i;
  long int strlen, elements;
  long int substrlen = 0;
  bool is_str_storage = false;
  gfc_ref *ref;

  if (e == NULL)
    return 0;
  
  if (e->ts.type == BT_CHARACTER)
    {
      if (e->ts.cl && e->ts.cl->length
          && e->ts.cl->length->expr_type == EXPR_CONSTANT)
	strlen = mpz_get_si (e->ts.cl->length->value.integer);
      else if (e->expr_type == EXPR_CONSTANT
	       && (e->ts.cl == NULL || e->ts.cl->length == NULL))
	strlen = e->value.character.length;
      else
	return 0;
    }
  else
    strlen = 1; /* Length per element.  */

  if (e->rank == 0 && !e->ref)
    return strlen;

  elements = 1;
  if (!e->ref)
    {
      if (!e->shape)
	return 0;
      for (i = 0; i < e->rank; i++)
	elements *= mpz_get_si (e->shape[i]);
      return elements*strlen;
    }

  for (ref = e->ref; ref; ref = ref->next)
    {
      if (ref->type == REF_SUBSTRING && ref->u.ss.start
	  && ref->u.ss.start->expr_type == EXPR_CONSTANT)
	{
	  if (is_str_storage)
	    {
	      /* The string length is the substring length.
		 Set now to full string length.  */
	      if (ref->u.ss.length == NULL
		  || ref->u.ss.length->length->expr_type != EXPR_CONSTANT)
		return 0;

	      strlen = mpz_get_ui (ref->u.ss.length->length->value.integer);
	    }
	  substrlen = strlen - mpz_get_ui (ref->u.ss.start->value.integer) + 1;
	  continue;
	}

      if (ref->type == REF_ARRAY && ref->u.ar.type == AR_SECTION
	  && ref->u.ar.start && ref->u.ar.end && ref->u.ar.stride
	  && ref->u.ar.as->upper)
	for (i = 0; i < ref->u.ar.dimen; i++)
	  {
	    long int start, end, stride;
	    stride = 1;

	    if (ref->u.ar.stride[i])
	      {
		if (ref->u.ar.stride[i]->expr_type == EXPR_CONSTANT)
		  stride = mpz_get_si (ref->u.ar.stride[i]->value.integer);
		else
		  return 0;
	      }

	    if (ref->u.ar.start[i])
	      {
		if (ref->u.ar.start[i]->expr_type == EXPR_CONSTANT)
		  start = mpz_get_si (ref->u.ar.start[i]->value.integer);
		else
		  return 0;
	      }
	    else if (ref->u.ar.as->lower[i]
		     && ref->u.ar.as->lower[i]->expr_type == EXPR_CONSTANT)
	      start = mpz_get_si (ref->u.ar.as->lower[i]->value.integer);
	    else
	      return 0;

	    if (ref->u.ar.end[i])
	      {
		if (ref->u.ar.end[i]->expr_type == EXPR_CONSTANT)
		  end = mpz_get_si (ref->u.ar.end[i]->value.integer);
		else
		  return 0;
	      }
	    else if (ref->u.ar.as->upper[i]
		     && ref->u.ar.as->upper[i]->expr_type == EXPR_CONSTANT)
	      end = mpz_get_si (ref->u.ar.as->upper[i]->value.integer);
	    else
	      return 0;

	    elements *= (end - start)/stride + 1L;
	  }
      else if (ref->type == REF_ARRAY && ref->u.ar.type == AR_FULL
	       && ref->u.ar.as->lower && ref->u.ar.as->upper)
	for (i = 0; i < ref->u.ar.as->rank; i++)
	  {
	    if (ref->u.ar.as->lower[i] && ref->u.ar.as->upper[i]
		&& ref->u.ar.as->lower[i]->expr_type == EXPR_CONSTANT
		&& ref->u.ar.as->upper[i]->expr_type == EXPR_CONSTANT)
	      elements *= mpz_get_si (ref->u.ar.as->upper[i]->value.integer)
			  - mpz_get_si (ref->u.ar.as->lower[i]->value.integer)
			  + 1L;
	    else
	      return 0;
	  }
      else if (ref->type == REF_ARRAY && ref->u.ar.type == AR_ELEMENT
	       && e->expr_type == EXPR_VARIABLE)
	{
	  if (e->symtree->n.sym->as->type == AS_ASSUMED_SHAPE
	      || e->symtree->n.sym->attr.pointer)
	    {
	      elements = 1;
	      continue;
	    }

	  /* Determine the number of remaining elements in the element
	     sequence for array element designators.  */
	  is_str_storage = true;
	  for (i = ref->u.ar.dimen - 1; i >= 0; i--)
	    {
	      if (ref->u.ar.start[i] == NULL
		  || ref->u.ar.start[i]->expr_type != EXPR_CONSTANT
		  || ref->u.ar.as->upper[i] == NULL
		  || ref->u.ar.as->lower[i] == NULL
		  || ref->u.ar.as->upper[i]->expr_type != EXPR_CONSTANT
		  || ref->u.ar.as->lower[i]->expr_type != EXPR_CONSTANT)
		return 0;

	      elements
		   = elements
		     * (mpz_get_si (ref->u.ar.as->upper[i]->value.integer)
			- mpz_get_si (ref->u.ar.as->lower[i]->value.integer)
			+ 1L)
		     - (mpz_get_si (ref->u.ar.start[i]->value.integer)
			- mpz_get_si (ref->u.ar.as->lower[i]->value.integer));
	    }
        }
      else
	return 0;
    }

  if (substrlen)
    return (is_str_storage) ? substrlen + (elements-1)*strlen
			    : elements*strlen;
  else
    return elements*strlen;
}


/* Given an expression, check whether it is an array section
   which has a vector subscript. If it has, one is returned,
   otherwise zero.  */

static int
has_vector_subscript (gfc_expr *e)
{
  int i;
  gfc_ref *ref;

  if (e == NULL || e->rank == 0 || e->expr_type != EXPR_VARIABLE)
    return 0;

  for (ref = e->ref; ref; ref = ref->next)
    if (ref->type == REF_ARRAY && ref->u.ar.type == AR_SECTION)
      for (i = 0; i < ref->u.ar.dimen; i++)
	if (ref->u.ar.dimen_type[i] == DIMEN_VECTOR)
	  return 1;

  return 0;
}


/* Given formal and actual argument lists, see if they are compatible.
   If they are compatible, the actual argument list is sorted to
   correspond with the formal list, and elements for missing optional
   arguments are inserted. If WHERE pointer is nonnull, then we issue
   errors when things don't match instead of just returning the status
   code.  */

static int
compare_actual_formal (gfc_actual_arglist **ap, gfc_formal_arglist *formal,
	 	       int ranks_must_agree, int is_elemental, locus *where)
{
  gfc_actual_arglist **new_arg, *a, *actual, temp;
  gfc_formal_arglist *f;
  int i, n, na;
  unsigned long actual_size, formal_size;

  actual = *ap;

  if (actual == NULL && formal == NULL)
    return 1;

  n = 0;
  for (f = formal; f; f = f->next)
    n++;

  new_arg = (gfc_actual_arglist **) alloca (n * sizeof (gfc_actual_arglist *));

  for (i = 0; i < n; i++)
    new_arg[i] = NULL;

  na = 0;
  f = formal;
  i = 0;

  for (a = actual; a; a = a->next, f = f->next)
    {
      /* Look for keywords but ignore g77 extensions like %VAL.  */
      if (a->name != NULL && a->name[0] != '%')
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
		gfc_error ("Keyword argument '%s' at %L is not in "
			   "the procedure", a->name, &a->expr->where);
	      return 0;
	    }

	  if (new_arg[i] != NULL)
	    {
	      if (where)
		gfc_error ("Keyword argument '%s' at %L is already associated "
			   "with another actual argument", a->name,
			   &a->expr->where);
	      return 0;
	    }
	}

      if (f == NULL)
	{
	  if (where)
	    gfc_error ("More actual than formal arguments in procedure "
		       "call at %L", where);

	  return 0;
	}

      if (f->sym == NULL && a->expr == NULL)
	goto match;

      if (f->sym == NULL)
	{
	  if (where)
	    gfc_error ("Missing alternate return spec in subroutine call "
		       "at %L", where);
	  return 0;
	}

      if (a->expr == NULL)
	{
	  if (where)
	    gfc_error ("Unexpected alternate return spec in subroutine "
		       "call at %L", where);
	  return 0;
	}
      
      if (!compare_parameter (f->sym, a->expr, ranks_must_agree,
			      is_elemental, where))
	return 0;

      /* Special case for character arguments.  For allocatable, pointer
	 and assumed-shape dummies, the string length needs to match
	 exactly.  */
      if (a->expr->ts.type == BT_CHARACTER
	   && a->expr->ts.cl && a->expr->ts.cl->length
	   && a->expr->ts.cl->length->expr_type == EXPR_CONSTANT
	   && f->sym->ts.cl && f->sym->ts.cl && f->sym->ts.cl->length
	   && f->sym->ts.cl->length->expr_type == EXPR_CONSTANT
	   && (f->sym->attr.pointer || f->sym->attr.allocatable
	       || (f->sym->as && f->sym->as->type == AS_ASSUMED_SHAPE))
	   && (mpz_cmp (a->expr->ts.cl->length->value.integer,
			f->sym->ts.cl->length->value.integer) != 0))
	 {
	   if (where && (f->sym->attr.pointer || f->sym->attr.allocatable))
	     gfc_warning ("Character length mismatch (%ld/%ld) between actual "
			  "argument and pointer or allocatable dummy argument "
			  "'%s' at %L",
			  mpz_get_si (a->expr->ts.cl->length->value.integer),
			  mpz_get_si (f->sym->ts.cl->length->value.integer),
			  f->sym->name, &a->expr->where);
	   else if (where)
	     gfc_warning ("Character length mismatch (%ld/%ld) between actual "
			  "argument and assumed-shape dummy argument '%s' "
			  "at %L",
			  mpz_get_si (a->expr->ts.cl->length->value.integer),
			  mpz_get_si (f->sym->ts.cl->length->value.integer),
			  f->sym->name, &a->expr->where);
	   return 0;
	 }

      actual_size = get_expr_storage_size (a->expr);
      formal_size = get_sym_storage_size (f->sym);
      if (actual_size != 0
	    && actual_size < formal_size
	    && a->expr->ts.type != BT_PROCEDURE)
	{
	  if (a->expr->ts.type == BT_CHARACTER && !f->sym->as && where)
	    gfc_warning ("Character length of actual argument shorter "
			"than of dummy argument '%s' (%lu/%lu) at %L",
			f->sym->name, actual_size, formal_size,
			&a->expr->where);
          else if (where)
	    gfc_warning ("Actual argument contains too few "
			"elements for dummy argument '%s' (%lu/%lu) at %L",
			f->sym->name, actual_size, formal_size,
			&a->expr->where);
	  return  0;
	}

      /* Satisfy 12.4.1.3 by ensuring that a procedure pointer actual argument
	 is provided for a procedure pointer formal argument.  */
      if (f->sym->attr.proc_pointer
	  && !((a->expr->expr_type == EXPR_VARIABLE
		&& a->expr->symtree->n.sym->attr.proc_pointer)
	       || (a->expr->expr_type == EXPR_FUNCTION
		   && a->expr->symtree->n.sym->result->attr.proc_pointer)
	       || gfc_is_proc_ptr_comp (a->expr, NULL)))
	{
	  if (where)
	    gfc_error ("Expected a procedure pointer for argument '%s' at %L",
		       f->sym->name, &a->expr->where);
	  return 0;
	}

      /* Satisfy 12.4.1.2 by ensuring that a procedure actual argument is
	 provided for a procedure formal argument.  */
      if (a->expr->ts.type != BT_PROCEDURE && !gfc_is_proc_ptr_comp (a->expr, NULL)
	  && a->expr->expr_type == EXPR_VARIABLE
	  && f->sym->attr.flavor == FL_PROCEDURE)
	{
	  if (where)
	    gfc_error ("Expected a procedure for argument '%s' at %L",
		       f->sym->name, &a->expr->where);
	  return 0;
	}

      if (f->sym->attr.flavor == FL_PROCEDURE && f->sym->attr.pure
	  && a->expr->ts.type == BT_PROCEDURE
	  && !a->expr->symtree->n.sym->attr.pure)
	{
	  if (where)
	    gfc_error ("Expected a PURE procedure for argument '%s' at %L",
		       f->sym->name, &a->expr->where);
	  return 0;
	}

      if (f->sym->as && f->sym->as->type == AS_ASSUMED_SHAPE
	  && a->expr->expr_type == EXPR_VARIABLE
	  && a->expr->symtree->n.sym->as
	  && a->expr->symtree->n.sym->as->type == AS_ASSUMED_SIZE
	  && (a->expr->ref == NULL
	      || (a->expr->ref->type == REF_ARRAY
		  && a->expr->ref->u.ar.type == AR_FULL)))
	{
	  if (where)
	    gfc_error ("Actual argument for '%s' cannot be an assumed-size"
		       " array at %L", f->sym->name, where);
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

      if (a->expr->expr_type != EXPR_NULL
	  && compare_allocatable (f->sym, a->expr) == 0)
	{
	  if (where)
	    gfc_error ("Actual argument for '%s' must be ALLOCATABLE at %L",
		       f->sym->name, &a->expr->where);
	  return 0;
	}

      /* Check intent = OUT/INOUT for definable actual argument.  */
      if ((a->expr->expr_type != EXPR_VARIABLE
	   || (a->expr->symtree->n.sym->attr.flavor != FL_VARIABLE
	       && a->expr->symtree->n.sym->attr.flavor != FL_PROCEDURE))
	  && (f->sym->attr.intent == INTENT_OUT
	      || f->sym->attr.intent == INTENT_INOUT))
	{
	  if (where)
	    gfc_error ("Actual argument at %L must be definable as "
		       "the dummy argument '%s' is INTENT = OUT/INOUT",
		       &a->expr->where, f->sym->name);
	  return 0;
	}

      if (!compare_parameter_protected(f->sym, a->expr))
	{
	  if (where)
	    gfc_error ("Actual argument at %L is use-associated with "
		       "PROTECTED attribute and dummy argument '%s' is "
		       "INTENT = OUT/INOUT",
		       &a->expr->where,f->sym->name);
	  return 0;
	}

      if ((f->sym->attr.intent == INTENT_OUT
	   || f->sym->attr.intent == INTENT_INOUT
	   || f->sym->attr.volatile_)
          && has_vector_subscript (a->expr))
	{
	  if (where)
	    gfc_error ("Array-section actual argument with vector subscripts "
		       "at %L is incompatible with INTENT(OUT), INTENT(INOUT) "
		       "or VOLATILE attribute of the dummy argument '%s'",
		       &a->expr->where, f->sym->name);
	  return 0;
	}

      /* C1232 (R1221) For an actual argument which is an array section or
	 an assumed-shape array, the dummy argument shall be an assumed-
	 shape array, if the dummy argument has the VOLATILE attribute.  */

      if (f->sym->attr.volatile_
	  && a->expr->symtree->n.sym->as
	  && a->expr->symtree->n.sym->as->type == AS_ASSUMED_SHAPE
	  && !(f->sym->as && f->sym->as->type == AS_ASSUMED_SHAPE))
	{
	  if (where)
	    gfc_error ("Assumed-shape actual argument at %L is "
		       "incompatible with the non-assumed-shape "
		       "dummy argument '%s' due to VOLATILE attribute",
		       &a->expr->where,f->sym->name);
	  return 0;
	}

      if (f->sym->attr.volatile_
	  && a->expr->ref && a->expr->ref->u.ar.type == AR_SECTION
	  && !(f->sym->as && f->sym->as->type == AS_ASSUMED_SHAPE))
	{
	  if (where)
	    gfc_error ("Array-section actual argument at %L is "
		       "incompatible with the non-assumed-shape "
		       "dummy argument '%s' due to VOLATILE attribute",
		       &a->expr->where,f->sym->name);
	  return 0;
	}

      /* C1233 (R1221) For an actual argument which is a pointer array, the
	 dummy argument shall be an assumed-shape or pointer array, if the
	 dummy argument has the VOLATILE attribute.  */

      if (f->sym->attr.volatile_
	  && a->expr->symtree->n.sym->attr.pointer
	  && a->expr->symtree->n.sym->as
	  && !(f->sym->as
	       && (f->sym->as->type == AS_ASSUMED_SHAPE
		   || f->sym->attr.pointer)))
	{
	  if (where)
	    gfc_error ("Pointer-array actual argument at %L requires "
		       "an assumed-shape or pointer-array dummy "
		       "argument '%s' due to VOLATILE attribute",
		       &a->expr->where,f->sym->name);
	  return 0;
	}

    match:
      if (a == actual)
	na = i;

      new_arg[i++] = a;
    }

  /* Make sure missing actual arguments are optional.  */
  i = 0;
  for (f = formal; f; f = f->next, i++)
    {
      if (new_arg[i] != NULL)
	continue;
      if (f->sym == NULL)
	{
	  if (where)
	    gfc_error ("Missing alternate return spec in subroutine call "
		       "at %L", where);
	  return 0;
	}
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
    if (new_arg[i] == NULL)
      new_arg[i] = gfc_get_actual_arglist ();

  if (na != 0)
    {
      temp = *new_arg[0];
      *new_arg[0] = *actual;
      *actual = temp;

      a = new_arg[0];
      new_arg[0] = new_arg[na];
      new_arg[na] = a;
    }

  for (i = 0; i < n - 1; i++)
    new_arg[i]->next = new_arg[i + 1];

  new_arg[i]->next = NULL;

  if (*ap == NULL && n > 0)
    *ap = new_arg[0];

  /* Note the types of omitted optional arguments.  */
  for (a = *ap, f = formal; a; a = a->next, f = f->next)
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

static gfc_try
compare_actual_expr (gfc_expr *e1, gfc_expr *e2)
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

static gfc_try
check_some_aliasing (gfc_formal_arglist *f, gfc_actual_arglist *a)
{
  sym_intent f1_intent, f2_intent;
  gfc_formal_arglist *f1;
  gfc_actual_arglist *a1;
  size_t n, i, j;
  argpair *p;
  gfc_try t = SUCCESS;

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


/* Given a symbol of a formal argument list and an expression,
   return nonzero if their intents are compatible, zero otherwise.  */

static int
compare_parameter_intent (gfc_symbol *formal, gfc_expr *actual)
{
  if (actual->symtree->n.sym->attr.pointer && !formal->attr.pointer)
    return 1;

  if (actual->symtree->n.sym->attr.intent != INTENT_IN)
    return 1;

  if (formal->attr.intent == INTENT_INOUT || formal->attr.intent == INTENT_OUT)
    return 0;

  return 1;
}


/* Given formal and actual argument lists that correspond to one
   another, check that they are compatible in the sense that intents
   are not mismatched.  */

static gfc_try
check_intents (gfc_formal_arglist *f, gfc_actual_arglist *a)
{
  sym_intent f_intent;

  for (;; f = f->next, a = a->next)
    {
      if (f == NULL && a == NULL)
	break;
      if (f == NULL || a == NULL)
	gfc_internal_error ("check_intents(): List mismatch");

      if (a->expr == NULL || a->expr->expr_type != EXPR_VARIABLE)
	continue;

      f_intent = f->sym->attr.intent;

      if (!compare_parameter_intent(f->sym, a->expr))
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
	      gfc_error ("Procedure argument at %L is local to a PURE "
			 "procedure and is passed to an INTENT(%s) argument",
			 &a->expr->where, gfc_intent_string (f_intent));
	      return FAILURE;
	    }

	  if (f->sym->attr.pointer)
	    {
	      gfc_error ("Procedure argument at %L is local to a PURE "
			 "procedure and has the POINTER attribute",
			 &a->expr->where);
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
gfc_procedure_use (gfc_symbol *sym, gfc_actual_arglist **ap, locus *where)
{

  /* Warn about calls with an implicit interface.  Special case
     for calling a ISO_C_BINDING becase c_loc and c_funloc
     are pseudo-unknown.  */
  if (gfc_option.warn_implicit_interface
      && sym->attr.if_source == IFSRC_UNKNOWN
      && ! sym->attr.is_iso_c)
    gfc_warning ("Procedure '%s' called with an implicit interface at %L",
		 sym->name, where);

  if (sym->attr.if_source == IFSRC_UNKNOWN)
    {
      gfc_actual_arglist *a;
      for (a = *ap; a; a = a->next)
	{
	  /* Skip g77 keyword extensions like %VAL, %REF, %LOC.  */
	  if (a->name != NULL && a->name[0] != '%')
	    {
	      gfc_error("Keyword argument requires explicit interface "
			"for procedure '%s' at %L", sym->name, &a->expr->where);
	      break;
	    }
	}

      return;
    }

  if (!compare_actual_formal (ap, sym->formal, 0, sym->attr.elemental, where))
    return;

  check_intents (sym->formal, *ap);
  if (gfc_option.warn_aliasing)
    check_some_aliasing (sym->formal, *ap);
}


/* Check how a procedure pointer component is used against its interface.
   If all goes well, the actual argument list will also end up being properly
   sorted. Completely analogous to gfc_procedure_use.  */

void
gfc_ppc_use (gfc_component *comp, gfc_actual_arglist **ap, locus *where)
{

  /* Warn about calls with an implicit interface.  Special case
     for calling a ISO_C_BINDING becase c_loc and c_funloc
     are pseudo-unknown.  */
  if (gfc_option.warn_implicit_interface
      && comp->attr.if_source == IFSRC_UNKNOWN
      && !comp->attr.is_iso_c)
    gfc_warning ("Procedure pointer component '%s' called with an implicit "
		 "interface at %L", comp->name, where);

  if (comp->attr.if_source == IFSRC_UNKNOWN)
    {
      gfc_actual_arglist *a;
      for (a = *ap; a; a = a->next)
	{
	  /* Skip g77 keyword extensions like %VAL, %REF, %LOC.  */
	  if (a->name != NULL && a->name[0] != '%')
	    {
	      gfc_error("Keyword argument requires explicit interface "
			"for procedure pointer component '%s' at %L",
			comp->name, &a->expr->where);
	      break;
	    }
	}

      return;
    }

  if (!compare_actual_formal (ap, comp->formal, 0, comp->attr.elemental, where))
    return;

  check_intents (comp->formal, *ap);
  if (gfc_option.warn_aliasing)
    check_some_aliasing (comp->formal, *ap);
}


/* Try if an actual argument list matches the formal list of a symbol,
   respecting the symbol's attributes like ELEMENTAL.  This is used for
   GENERIC resolution.  */

bool
gfc_arglist_matches_symbol (gfc_actual_arglist** args, gfc_symbol* sym)
{
  bool r;

  gcc_assert (sym->attr.flavor == FL_PROCEDURE);

  r = !sym->attr.elemental;
  if (compare_actual_formal (args, sym->formal, r, !r, NULL))
    {
      check_intents (sym->formal, *args);
      if (gfc_option.warn_aliasing)
	check_some_aliasing (sym->formal, *args);
      return true;
    }

  return false;
}


/* Given an interface pointer and an actual argument list, search for
   a formal argument list that matches the actual.  If found, returns
   a pointer to the symbol of the correct interface.  Returns NULL if
   not found.  */

gfc_symbol *
gfc_search_interface (gfc_interface *intr, int sub_flag,
		      gfc_actual_arglist **ap)
{
  gfc_symbol *elem_sym = NULL;
  for (; intr; intr = intr->next)
    {
      if (sub_flag && intr->sym->attr.function)
	continue;
      if (!sub_flag && intr->sym->attr.subroutine)
	continue;

      if (gfc_arglist_matches_symbol (ap, intr->sym))
	{
	  /* Satisfy 12.4.4.1 such that an elemental match has lower
	     weight than a non-elemental match.  */ 
	  if (intr->sym->attr.elemental)
	    {
	      elem_sym = intr->sym;
	      continue;
	    }
	  return intr->sym;
	}
    }

  return elem_sym ? elem_sym : NULL;
}


/* Do a brute force recursive search for a symbol.  */

static gfc_symtree *
find_symtree0 (gfc_symtree *root, gfc_symbol *sym)
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

gfc_symtree *
gfc_find_sym_in_symtree (gfc_symbol *sym)
{
  gfc_symtree *st;
  gfc_namespace *ns;

  /* First try to find it by name.  */
  gfc_find_sym_tree (sym->name, gfc_current_ns, 1, &st);
  if (st && st->n.sym == sym)
    return st;

  /* If it's been renamed, resort to a brute-force search.  */
  /* TODO: avoid having to do this search.  If the symbol doesn't exist
     in the symtree for the current namespace, it should probably be added.  */
  for (ns = gfc_current_ns; ns; ns = ns->parent)
    {
      st = find_symtree0 (ns->sym_root, sym);
      if (st)
	return st;
    }
  gfc_internal_error ("Unable to find symbol %s", sym->name);
  /* Not reached.  */
}


/* This subroutine is called when an expression is being resolved.
   The expression node in question is either a user defined operator
   or an intrinsic operator with arguments that aren't compatible
   with the operator.  This subroutine builds an actual argument list
   corresponding to the operands, then searches for a compatible
   interface.  If one is found, the expression node is replaced with
   the appropriate function call.  */

gfc_try
gfc_extend_expr (gfc_expr *e)
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

  i = fold_unary_intrinsic (e->value.op.op);

  if (i == INTRINSIC_USER)
    {
      for (ns = gfc_current_ns; ns; ns = ns->parent)
	{
	  uop = gfc_find_uop (e->value.op.uop->name, ns);
	  if (uop == NULL)
	    continue;

	  sym = gfc_search_interface (uop->op, 0, &actual);
	  if (sym != NULL)
	    break;
	}
    }
  else
    {
      for (ns = gfc_current_ns; ns; ns = ns->parent)
	{
	  /* Due to the distinction between '==' and '.eq.' and friends, one has
	     to check if either is defined.  */
	  switch (i)
	    {
	      case INTRINSIC_EQ:
	      case INTRINSIC_EQ_OS:
		sym = gfc_search_interface (ns->op[INTRINSIC_EQ], 0, &actual);
		if (sym == NULL)
		  sym = gfc_search_interface (ns->op[INTRINSIC_EQ_OS], 0, &actual);
		break;

	      case INTRINSIC_NE:
	      case INTRINSIC_NE_OS:
		sym = gfc_search_interface (ns->op[INTRINSIC_NE], 0, &actual);
		if (sym == NULL)
		  sym = gfc_search_interface (ns->op[INTRINSIC_NE_OS], 0, &actual);
		break;

	      case INTRINSIC_GT:
	      case INTRINSIC_GT_OS:
		sym = gfc_search_interface (ns->op[INTRINSIC_GT], 0, &actual);
		if (sym == NULL)
		  sym = gfc_search_interface (ns->op[INTRINSIC_GT_OS], 0, &actual);
		break;

	      case INTRINSIC_GE:
	      case INTRINSIC_GE_OS:
		sym = gfc_search_interface (ns->op[INTRINSIC_GE], 0, &actual);
		if (sym == NULL)
		  sym = gfc_search_interface (ns->op[INTRINSIC_GE_OS], 0, &actual);
		break;

	      case INTRINSIC_LT:
	      case INTRINSIC_LT_OS:
		sym = gfc_search_interface (ns->op[INTRINSIC_LT], 0, &actual);
		if (sym == NULL)
		  sym = gfc_search_interface (ns->op[INTRINSIC_LT_OS], 0, &actual);
		break;

	      case INTRINSIC_LE:
	      case INTRINSIC_LE_OS:
		sym = gfc_search_interface (ns->op[INTRINSIC_LE], 0, &actual);
		if (sym == NULL)
		  sym = gfc_search_interface (ns->op[INTRINSIC_LE_OS], 0, &actual);
		break;

	      default:
		sym = gfc_search_interface (ns->op[i], 0, &actual);
	    }

	  if (sym != NULL)
	    break;
	}
    }

  if (sym == NULL)
    {
      /* Don't use gfc_free_actual_arglist().  */
      if (actual->next != NULL)
	gfc_free (actual->next);
      gfc_free (actual);

      return FAILURE;
    }

  /* Change the expression node to a function call.  */
  e->expr_type = EXPR_FUNCTION;
  e->symtree = gfc_find_sym_in_symtree (sym);
  e->value.function.actual = actual;
  e->value.function.esym = NULL;
  e->value.function.isym = NULL;
  e->value.function.name = NULL;
  e->user_operator = 1;

  if (gfc_pure (NULL) && !gfc_pure (sym))
    {
      gfc_error ("Function '%s' called in lieu of an operator at %L must "
		 "be PURE", sym->name, &e->where);
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

gfc_try
gfc_extend_assign (gfc_code *c, gfc_namespace *ns)
{
  gfc_actual_arglist *actual;
  gfc_expr *lhs, *rhs;
  gfc_symbol *sym;

  lhs = c->expr1;
  rhs = c->expr2;

  /* Don't allow an intrinsic assignment to be replaced.  */
  if (lhs->ts.type != BT_DERIVED
      && (rhs->rank == 0 || rhs->rank == lhs->rank)
      && (lhs->ts.type == rhs->ts.type
	  || (gfc_numeric_ts (&lhs->ts) && gfc_numeric_ts (&rhs->ts))))
    return FAILURE;

  actual = gfc_get_actual_arglist ();
  actual->expr = lhs;

  actual->next = gfc_get_actual_arglist ();
  actual->next->expr = rhs;

  sym = NULL;

  for (; ns; ns = ns->parent)
    {
      sym = gfc_search_interface (ns->op[INTRINSIC_ASSIGN], 1, &actual);
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
  c->op = EXEC_ASSIGN_CALL;
  c->symtree = gfc_find_sym_in_symtree (sym);
  c->expr1 = NULL;
  c->expr2 = NULL;
  c->ext.actual = actual;

  return SUCCESS;
}


/* Make sure that the interface just parsed is not already present in
   the given interface list.  Ambiguity isn't checked yet since module
   procedures can be present without interfaces.  */

static gfc_try
check_new_interface (gfc_interface *base, gfc_symbol *new_sym)
{
  gfc_interface *ip;

  for (ip = base; ip; ip = ip->next)
    {
      if (ip->sym == new_sym)
	{
	  gfc_error ("Entity '%s' at %C is already present in the interface",
		     new_sym->name);
	  return FAILURE;
	}
    }

  return SUCCESS;
}


/* Add a symbol to the current interface.  */

gfc_try
gfc_add_interface (gfc_symbol *new_sym)
{
  gfc_interface **head, *intr;
  gfc_namespace *ns;
  gfc_symbol *sym;

  switch (current_interface.type)
    {
    case INTERFACE_NAMELESS:
    case INTERFACE_ABSTRACT:
      return SUCCESS;

    case INTERFACE_INTRINSIC_OP:
      for (ns = current_interface.ns; ns; ns = ns->parent)
	switch (current_interface.op)
	  {
	    case INTRINSIC_EQ:
	    case INTRINSIC_EQ_OS:
	      if (check_new_interface (ns->op[INTRINSIC_EQ], new_sym) == FAILURE ||
	          check_new_interface (ns->op[INTRINSIC_EQ_OS], new_sym) == FAILURE)
		return FAILURE;
	      break;

	    case INTRINSIC_NE:
	    case INTRINSIC_NE_OS:
	      if (check_new_interface (ns->op[INTRINSIC_NE], new_sym) == FAILURE ||
	          check_new_interface (ns->op[INTRINSIC_NE_OS], new_sym) == FAILURE)
		return FAILURE;
	      break;

	    case INTRINSIC_GT:
	    case INTRINSIC_GT_OS:
	      if (check_new_interface (ns->op[INTRINSIC_GT], new_sym) == FAILURE ||
	          check_new_interface (ns->op[INTRINSIC_GT_OS], new_sym) == FAILURE)
		return FAILURE;
	      break;

	    case INTRINSIC_GE:
	    case INTRINSIC_GE_OS:
	      if (check_new_interface (ns->op[INTRINSIC_GE], new_sym) == FAILURE ||
	          check_new_interface (ns->op[INTRINSIC_GE_OS], new_sym) == FAILURE)
		return FAILURE;
	      break;

	    case INTRINSIC_LT:
	    case INTRINSIC_LT_OS:
	      if (check_new_interface (ns->op[INTRINSIC_LT], new_sym) == FAILURE ||
	          check_new_interface (ns->op[INTRINSIC_LT_OS], new_sym) == FAILURE)
		return FAILURE;
	      break;

	    case INTRINSIC_LE:
	    case INTRINSIC_LE_OS:
	      if (check_new_interface (ns->op[INTRINSIC_LE], new_sym) == FAILURE ||
	          check_new_interface (ns->op[INTRINSIC_LE_OS], new_sym) == FAILURE)
		return FAILURE;
	      break;

	    default:
	      if (check_new_interface (ns->op[current_interface.op], new_sym) == FAILURE)
		return FAILURE;
	  }

      head = &current_interface.ns->op[current_interface.op];
      break;

    case INTERFACE_GENERIC:
      for (ns = current_interface.ns; ns; ns = ns->parent)
	{
	  gfc_find_symbol (current_interface.sym->name, ns, 0, &sym);
	  if (sym == NULL)
	    continue;

	  if (check_new_interface (sym->generic, new_sym) == FAILURE)
	    return FAILURE;
	}

      head = &current_interface.sym->generic;
      break;

    case INTERFACE_USER_OP:
      if (check_new_interface (current_interface.uop->op, new_sym)
	  == FAILURE)
	return FAILURE;

      head = &current_interface.uop->op;
      break;

    default:
      gfc_internal_error ("gfc_add_interface(): Bad interface type");
    }

  intr = gfc_get_interface ();
  intr->sym = new_sym;
  intr->where = gfc_current_locus;

  intr->next = *head;
  *head = intr;

  return SUCCESS;
}


gfc_interface *
gfc_current_interface_head (void)
{
  switch (current_interface.type)
    {
      case INTERFACE_INTRINSIC_OP:
	return current_interface.ns->op[current_interface.op];
	break;

      case INTERFACE_GENERIC:
	return current_interface.sym->generic;
	break;

      case INTERFACE_USER_OP:
	return current_interface.uop->op;
	break;

      default:
	gcc_unreachable ();
    }
}


void
gfc_set_current_interface_head (gfc_interface *i)
{
  switch (current_interface.type)
    {
      case INTERFACE_INTRINSIC_OP:
	current_interface.ns->op[current_interface.op] = i;
	break;

      case INTERFACE_GENERIC:
	current_interface.sym->generic = i;
	break;

      case INTERFACE_USER_OP:
	current_interface.uop->op = i;
	break;

      default:
	gcc_unreachable ();
    }
}


/* Gets rid of a formal argument list.  We do not free symbols.
   Symbols are freed when a namespace is freed.  */

void
gfc_free_formal_arglist (gfc_formal_arglist *p)
{
  gfc_formal_arglist *q;

  for (; p; p = q)
    {
      q = p->next;
      gfc_free (p);
    }
}
