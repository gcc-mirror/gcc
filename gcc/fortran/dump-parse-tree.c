/* Parse tree dumper
   Copyright (C) 2003, 2004, 2005, 2006 Free Software Foundation, Inc.
   Contributed by Steven Bosscher

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
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */


/* Actually this is just a collection of routines that used to be
   scattered around the sources.  Now that they are all in a single
   file, almost all of them can be static, and the other files don't
   have this mess in them.

   As a nice side-effect, this file can act as documentation of the
   gfc_code and gfc_expr structures and all their friends and
   relatives.

   TODO: Dump DATA.  */

#include "config.h"
#include "gfortran.h"

/* Keep track of indentation for symbol tree dumps.  */
static int show_level = 0;

/* Do indentation for a specific level.  */

static inline void
code_indent (int level, gfc_st_label * label)
{
  int i;

  if (label != NULL)
    gfc_status ("%-5d ", label->value);
  else
    gfc_status ("      ");

  for (i = 0; i < 2 * level; i++)
    gfc_status_char (' ');
}


/* Simple indentation at the current level.  This one
   is used to show symbols.  */

static inline void
show_indent (void)
{
  gfc_status ("\n");
  code_indent (show_level, NULL);
}


/* Show type-specific information.  */

void
gfc_show_typespec (gfc_typespec * ts)
{

  gfc_status ("(%s ", gfc_basic_typename (ts->type));

  switch (ts->type)
    {
    case BT_DERIVED:
      gfc_status ("%s", ts->derived->name);
      break;

    case BT_CHARACTER:
      gfc_show_expr (ts->cl->length);
      break;

    default:
      gfc_status ("%d", ts->kind);
      break;
    }

  gfc_status (")");
}


/* Show an actual argument list.  */

void
gfc_show_actual_arglist (gfc_actual_arglist * a)
{

  gfc_status ("(");

  for (; a; a = a->next)
    {
      gfc_status_char ('(');
      if (a->name != NULL)
	gfc_status ("%s = ", a->name);
      if (a->expr != NULL)
	gfc_show_expr (a->expr);
      else
	gfc_status ("(arg not-present)");

      gfc_status_char (')');
      if (a->next != NULL)
	gfc_status (" ");
    }

  gfc_status (")");
}


/* Show a gfc_array_spec array specification structure.  */

void
gfc_show_array_spec (gfc_array_spec * as)
{
  const char *c;
  int i;

  if (as == NULL)
    {
      gfc_status ("()");
      return;
    }

  gfc_status ("(%d", as->rank);

  if (as->rank != 0)
    {
      switch (as->type)
      {
	case AS_EXPLICIT:      c = "AS_EXPLICIT";      break;
	case AS_DEFERRED:      c = "AS_DEFERRED";      break;
	case AS_ASSUMED_SIZE:  c = "AS_ASSUMED_SIZE";  break;
	case AS_ASSUMED_SHAPE: c = "AS_ASSUMED_SHAPE"; break;
	default:
	  gfc_internal_error
		("gfc_show_array_spec(): Unhandled array shape type.");
      }
      gfc_status (" %s ", c);

      for (i = 0; i < as->rank; i++)
	{
	  gfc_show_expr (as->lower[i]);
	  gfc_status_char (' ');
	  gfc_show_expr (as->upper[i]);
	  gfc_status_char (' ');
	}
    }

  gfc_status (")");
}


/* Show a gfc_array_ref array reference structure.  */

void
gfc_show_array_ref (gfc_array_ref * ar)
{
  int i;

  gfc_status_char ('(');

  switch (ar->type)
    {
    case AR_FULL:
      gfc_status ("FULL");
      break;

    case AR_SECTION:
      for (i = 0; i < ar->dimen; i++)
	{
	  /* There are two types of array sections: either the
	     elements are identified by an integer array ('vector'),
	     or by an index range. In the former case we only have to
	     print the start expression which contains the vector, in
	     the latter case we have to print any of lower and upper
	     bound and the stride, if they're present.  */
  
	  if (ar->start[i] != NULL)
	    gfc_show_expr (ar->start[i]);

	  if (ar->dimen_type[i] == DIMEN_RANGE)
	    {
	      gfc_status_char (':');

	      if (ar->end[i] != NULL)
		gfc_show_expr (ar->end[i]);

	      if (ar->stride[i] != NULL)
		{
		  gfc_status_char (':');
		  gfc_show_expr (ar->stride[i]);
		}
	    }

	  if (i != ar->dimen - 1)
	    gfc_status (" , ");
	}
      break;

    case AR_ELEMENT:
      for (i = 0; i < ar->dimen; i++)
	{
	  gfc_show_expr (ar->start[i]);
	  if (i != ar->dimen - 1)
	    gfc_status (" , ");
	}
      break;

    case AR_UNKNOWN:
      gfc_status ("UNKNOWN");
      break;

    default:
      gfc_internal_error ("gfc_show_array_ref(): Unknown array reference");
    }

  gfc_status_char (')');
}


/* Show a list of gfc_ref structures.  */

void
gfc_show_ref (gfc_ref * p)
{

  for (; p; p = p->next)
    switch (p->type)
      {
      case REF_ARRAY:
	gfc_show_array_ref (&p->u.ar);
	break;

      case REF_COMPONENT:
	gfc_status (" %% %s", p->u.c.component->name);
	break;

      case REF_SUBSTRING:
	gfc_status_char ('(');
	gfc_show_expr (p->u.ss.start);
	gfc_status_char (':');
	gfc_show_expr (p->u.ss.end);
	gfc_status_char (')');
	break;

      default:
	gfc_internal_error ("gfc_show_ref(): Bad component code");
      }
}


/* Display a constructor.  Works recursively for array constructors.  */

void
gfc_show_constructor (gfc_constructor * c)
{

  for (; c; c = c->next)
    {
      if (c->iterator == NULL)
	gfc_show_expr (c->expr);
      else
	{
	  gfc_status_char ('(');
	  gfc_show_expr (c->expr);

	  gfc_status_char (' ');
	  gfc_show_expr (c->iterator->var);
	  gfc_status_char ('=');
	  gfc_show_expr (c->iterator->start);
	  gfc_status_char (',');
	  gfc_show_expr (c->iterator->end);
	  gfc_status_char (',');
	  gfc_show_expr (c->iterator->step);

	  gfc_status_char (')');
	}

      if (c->next != NULL)
	gfc_status (" , ");
    }
}


/* Show an expression.  */

void
gfc_show_expr (gfc_expr * p)
{
  const char *c;
  int i;

  if (p == NULL)
    {
      gfc_status ("()");
      return;
    }

  switch (p->expr_type)
    {
    case EXPR_SUBSTRING:
      c = p->value.character.string;

      for (i = 0; i < p->value.character.length; i++, c++)
	{
	  if (*c == '\'')
	    gfc_status ("''");
	  else
	    gfc_status ("%c", *c);
	}

      gfc_show_ref (p->ref);
      break;

    case EXPR_STRUCTURE:
      gfc_status ("%s(", p->ts.derived->name);
      gfc_show_constructor (p->value.constructor);
      gfc_status_char (')');
      break;

    case EXPR_ARRAY:
      gfc_status ("(/ ");
      gfc_show_constructor (p->value.constructor);
      gfc_status (" /)");

      gfc_show_ref (p->ref);
      break;

    case EXPR_NULL:
      gfc_status ("NULL()");
      break;

    case EXPR_CONSTANT:
      if (p->from_H || p->ts.type == BT_HOLLERITH)
	{
	  gfc_status ("%dH", p->value.character.length);
	  c = p->value.character.string;
	  for (i = 0; i < p->value.character.length; i++, c++)
	    {
	      gfc_status_char (*c);
	    }
	  break;
	}
      switch (p->ts.type)
	{
	case BT_INTEGER:
	  mpz_out_str (stdout, 10, p->value.integer);

	  if (p->ts.kind != gfc_default_integer_kind)
	    gfc_status ("_%d", p->ts.kind);
	  break;

	case BT_LOGICAL:
	  if (p->value.logical)
	    gfc_status (".true.");
	  else
	    gfc_status (".false.");
	  break;

	case BT_REAL:
	  mpfr_out_str (stdout, 10, 0, p->value.real, GFC_RND_MODE);
	  if (p->ts.kind != gfc_default_real_kind)
	    gfc_status ("_%d", p->ts.kind);
	  break;

	case BT_CHARACTER:
	  c = p->value.character.string;

	  gfc_status_char ('\'');

	  for (i = 0; i < p->value.character.length; i++, c++)
	    {
	      if (*c == '\'')
		gfc_status ("''");
	      else
		gfc_status_char (*c);
	    }

	  gfc_status_char ('\'');

	  break;

	case BT_COMPLEX:
	  gfc_status ("(complex ");

	  mpfr_out_str (stdout, 10, 0, p->value.complex.r, GFC_RND_MODE);
	  if (p->ts.kind != gfc_default_complex_kind)
	    gfc_status ("_%d", p->ts.kind);

	  gfc_status (" ");

	  mpfr_out_str (stdout, 10, 0, p->value.complex.i, GFC_RND_MODE);
	  if (p->ts.kind != gfc_default_complex_kind)
	    gfc_status ("_%d", p->ts.kind);

	  gfc_status (")");
	  break;

	default:
	  gfc_status ("???");
	  break;
	}

      break;

    case EXPR_VARIABLE:
      if (p->symtree->n.sym->ns && p->symtree->n.sym->ns->proc_name)
	gfc_status ("%s:", p->symtree->n.sym->ns->proc_name->name);
      gfc_status ("%s", p->symtree->n.sym->name);
      gfc_show_ref (p->ref);
      break;

    case EXPR_OP:
      gfc_status ("(");
      switch (p->value.op.operator)
	{
	case INTRINSIC_UPLUS:
	  gfc_status ("U+ ");
	  break;
	case INTRINSIC_UMINUS:
	  gfc_status ("U- ");
	  break;
	case INTRINSIC_PLUS:
	  gfc_status ("+ ");
	  break;
	case INTRINSIC_MINUS:
	  gfc_status ("- ");
	  break;
	case INTRINSIC_TIMES:
	  gfc_status ("* ");
	  break;
	case INTRINSIC_DIVIDE:
	  gfc_status ("/ ");
	  break;
	case INTRINSIC_POWER:
	  gfc_status ("** ");
	  break;
	case INTRINSIC_CONCAT:
	  gfc_status ("// ");
	  break;
	case INTRINSIC_AND:
	  gfc_status ("AND ");
	  break;
	case INTRINSIC_OR:
	  gfc_status ("OR ");
	  break;
	case INTRINSIC_EQV:
	  gfc_status ("EQV ");
	  break;
	case INTRINSIC_NEQV:
	  gfc_status ("NEQV ");
	  break;
	case INTRINSIC_EQ:
	  gfc_status ("= ");
	  break;
	case INTRINSIC_NE:
	  gfc_status ("<> ");
	  break;
	case INTRINSIC_GT:
	  gfc_status ("> ");
	  break;
	case INTRINSIC_GE:
	  gfc_status (">= ");
	  break;
	case INTRINSIC_LT:
	  gfc_status ("< ");
	  break;
	case INTRINSIC_LE:
	  gfc_status ("<= ");
	  break;
	case INTRINSIC_NOT:
	  gfc_status ("NOT ");
	  break;
	case INTRINSIC_PARENTHESES:
	  gfc_status ("parens");
	  break;

	default:
	  gfc_internal_error
	    ("gfc_show_expr(): Bad intrinsic in expression!");
	}

      gfc_show_expr (p->value.op.op1);

      if (p->value.op.op2)
	{
	  gfc_status (" ");
	  gfc_show_expr (p->value.op.op2);
	}

      gfc_status (")");
      break;

    case EXPR_FUNCTION:
      if (p->value.function.name == NULL)
	{
	  gfc_status ("%s[", p->symtree->n.sym->name);
	  gfc_show_actual_arglist (p->value.function.actual);
	  gfc_status_char (']');
	}
      else
	{
	  gfc_status ("%s[[", p->value.function.name);
	  gfc_show_actual_arglist (p->value.function.actual);
	  gfc_status_char (']');
	  gfc_status_char (']');
	}

      break;

    default:
      gfc_internal_error ("gfc_show_expr(): Don't know how to show expr");
    }
}


/* Show symbol attributes.  The flavor and intent are followed by
   whatever single bit attributes are present.  */

void
gfc_show_attr (symbol_attribute * attr)
{

  gfc_status ("(%s %s %s %s", gfc_code2string (flavors, attr->flavor),
	      gfc_intent_string (attr->intent),
	      gfc_code2string (access_types, attr->access),
	      gfc_code2string (procedures, attr->proc));

  if (attr->allocatable)
    gfc_status (" ALLOCATABLE");
  if (attr->dimension)
    gfc_status (" DIMENSION");
  if (attr->external)
    gfc_status (" EXTERNAL");
  if (attr->intrinsic)
    gfc_status (" INTRINSIC");
  if (attr->optional)
    gfc_status (" OPTIONAL");
  if (attr->pointer)
    gfc_status (" POINTER");
  if (attr->save)
    gfc_status (" SAVE");
  if (attr->threadprivate)
    gfc_status (" THREADPRIVATE");
  if (attr->target)
    gfc_status (" TARGET");
  if (attr->dummy)
    gfc_status (" DUMMY");
  if (attr->result)
    gfc_status (" RESULT");
  if (attr->entry)
    gfc_status (" ENTRY");

  if (attr->data)
    gfc_status (" DATA");
  if (attr->use_assoc)
    gfc_status (" USE-ASSOC");
  if (attr->in_namelist)
    gfc_status (" IN-NAMELIST");
  if (attr->in_common)
    gfc_status (" IN-COMMON");

  if (attr->function)
    gfc_status (" FUNCTION");
  if (attr->subroutine)
    gfc_status (" SUBROUTINE");
  if (attr->implicit_type)
    gfc_status (" IMPLICIT-TYPE");

  if (attr->sequence)
    gfc_status (" SEQUENCE");
  if (attr->elemental)
    gfc_status (" ELEMENTAL");
  if (attr->pure)
    gfc_status (" PURE");
  if (attr->recursive)
    gfc_status (" RECURSIVE");

  gfc_status (")");
}


/* Show components of a derived type.  */

void
gfc_show_components (gfc_symbol * sym)
{
  gfc_component *c;

  for (c = sym->components; c; c = c->next)
    {
      gfc_status ("(%s ", c->name);
      gfc_show_typespec (&c->ts);
      if (c->pointer)
	gfc_status (" POINTER");
      if (c->dimension)
	gfc_status (" DIMENSION");
      gfc_status_char (' ');
      gfc_show_array_spec (c->as);
      gfc_status (")");
      if (c->next != NULL)
	gfc_status_char (' ');
    }
}


/* Show a symbol.  If a symbol is an ENTRY, SUBROUTINE or FUNCTION, we
   show the interface.  Information needed to reconstruct the list of
   specific interfaces associated with a generic symbol is done within
   that symbol.  */

void
gfc_show_symbol (gfc_symbol * sym)
{
  gfc_formal_arglist *formal;
  gfc_interface *intr;

  if (sym == NULL)
    return;

  show_indent ();

  gfc_status ("symbol %s ", sym->name);
  gfc_show_typespec (&sym->ts);
  gfc_show_attr (&sym->attr);

  if (sym->value)
    {
      show_indent ();
      gfc_status ("value: ");
      gfc_show_expr (sym->value);
    }

  if (sym->as)
    {
      show_indent ();
      gfc_status ("Array spec:");
      gfc_show_array_spec (sym->as);
    }

  if (sym->generic)
    {
      show_indent ();
      gfc_status ("Generic interfaces:");
      for (intr = sym->generic; intr; intr = intr->next)
	gfc_status (" %s", intr->sym->name);
    }

  if (sym->result)
    {
      show_indent ();
      gfc_status ("result: %s", sym->result->name);
    }

  if (sym->components)
    {
      show_indent ();
      gfc_status ("components: ");
      gfc_show_components (sym);
    }

  if (sym->formal)
    {
      show_indent ();
      gfc_status ("Formal arglist:");

      for (formal = sym->formal; formal; formal = formal->next)
        {
          if (formal->sym != NULL)
            gfc_status (" %s", formal->sym->name);
          else
            gfc_status (" [Alt Return]");
        }
    }

  if (sym->formal_ns)
    {
      show_indent ();
      gfc_status ("Formal namespace");
      gfc_show_namespace (sym->formal_ns);
    }

  gfc_status_char ('\n');
}


/* Show a user-defined operator.  Just prints an operator
   and the name of the associated subroutine, really.  */

static void
show_uop (gfc_user_op * uop)
{
  gfc_interface *intr;

  show_indent ();
  gfc_status ("%s:", uop->name);

  for (intr = uop->operator; intr; intr = intr->next)
    gfc_status (" %s", intr->sym->name);
}


/* Workhorse function for traversing the user operator symtree.  */

static void
traverse_uop (gfc_symtree * st, void (*func) (gfc_user_op *))
{

  if (st == NULL)
    return;

  (*func) (st->n.uop);

  traverse_uop (st->left, func);
  traverse_uop (st->right, func);
}


/* Traverse the tree of user operator nodes.  */

void
gfc_traverse_user_op (gfc_namespace * ns, void (*func) (gfc_user_op *))
{

  traverse_uop (ns->uop_root, func);
}


/* Function to display a common block.  */

static void
show_common (gfc_symtree * st)
{
  gfc_symbol *s;

  show_indent ();
  gfc_status ("common: /%s/ ", st->name);

  s = st->n.common->head;
  while (s)
    {
      gfc_status ("%s", s->name);
      s = s->common_next;
      if (s)
	gfc_status (", ");
    }
  gfc_status_char ('\n');
}    


/* Worker function to display the symbol tree.  */

static void
show_symtree (gfc_symtree * st)
{

  show_indent ();
  gfc_status ("symtree: %s  Ambig %d", st->name, st->ambiguous);

  if (st->n.sym->ns != gfc_current_ns)
    gfc_status (" from namespace %s", st->n.sym->ns->proc_name->name);
  else
    gfc_show_symbol (st->n.sym);
}


/******************* Show gfc_code structures **************/



static void gfc_show_code_node (int level, gfc_code * c);

/* Show a list of code structures.  Mutually recursive with
   gfc_show_code_node().  */

void
gfc_show_code (int level, gfc_code * c)
{

  for (; c; c = c->next)
    gfc_show_code_node (level, c);
}

void
gfc_show_namelist (gfc_namelist *n)
{
  for (; n->next; n = n->next)
    gfc_status ("%s,", n->sym->name);
  gfc_status ("%s", n->sym->name);
}

/* Show a single OpenMP directive node and everything underneath it
   if necessary.  */

static void
gfc_show_omp_node (int level, gfc_code * c)
{
  gfc_omp_clauses *omp_clauses = NULL;
  const char *name = NULL;

  switch (c->op)
    {
    case EXEC_OMP_ATOMIC: name = "ATOMIC"; break;
    case EXEC_OMP_BARRIER: name = "BARRIER"; break;
    case EXEC_OMP_CRITICAL: name = "CRITICAL"; break;
    case EXEC_OMP_FLUSH: name = "FLUSH"; break;
    case EXEC_OMP_DO: name = "DO"; break;
    case EXEC_OMP_MASTER: name = "MASTER"; break;
    case EXEC_OMP_ORDERED: name = "ORDERED"; break;
    case EXEC_OMP_PARALLEL: name = "PARALLEL"; break;
    case EXEC_OMP_PARALLEL_DO: name = "PARALLEL DO"; break;
    case EXEC_OMP_PARALLEL_SECTIONS: name = "PARALLEL SECTIONS"; break;
    case EXEC_OMP_PARALLEL_WORKSHARE: name = "PARALLEL WORKSHARE"; break;
    case EXEC_OMP_SECTIONS: name = "SECTIONS"; break;
    case EXEC_OMP_SINGLE: name = "SINGLE"; break;
    case EXEC_OMP_WORKSHARE: name = "WORKSHARE"; break;
    default:
      gcc_unreachable ();
    }
  gfc_status ("!$OMP %s", name);
  switch (c->op)
    {
    case EXEC_OMP_DO:
    case EXEC_OMP_PARALLEL:
    case EXEC_OMP_PARALLEL_DO:
    case EXEC_OMP_PARALLEL_SECTIONS:
    case EXEC_OMP_SECTIONS:
    case EXEC_OMP_SINGLE:
    case EXEC_OMP_WORKSHARE:
    case EXEC_OMP_PARALLEL_WORKSHARE:
      omp_clauses = c->ext.omp_clauses;
      break;
    case EXEC_OMP_CRITICAL:
      if (c->ext.omp_name)
	gfc_status (" (%s)", c->ext.omp_name);
      break;
    case EXEC_OMP_FLUSH:
      if (c->ext.omp_namelist)
	{
	  gfc_status (" (");
	  gfc_show_namelist (c->ext.omp_namelist);
	  gfc_status_char (')');
	}
      return;
    case EXEC_OMP_BARRIER:
      return;
    default:
      break;
    }
  if (omp_clauses)
    {
      int list_type;

      if (omp_clauses->if_expr)
	{
	  gfc_status (" IF(");
	  gfc_show_expr (omp_clauses->if_expr);
	  gfc_status_char (')');
	}
      if (omp_clauses->num_threads)
	{
	  gfc_status (" NUM_THREADS(");
	  gfc_show_expr (omp_clauses->num_threads);
	  gfc_status_char (')');
	}
      if (omp_clauses->sched_kind != OMP_SCHED_NONE)
	{
	  const char *type;
	  switch (omp_clauses->sched_kind)
	    {
	    case OMP_SCHED_STATIC: type = "STATIC"; break;
	    case OMP_SCHED_DYNAMIC: type = "DYNAMIC"; break;
	    case OMP_SCHED_GUIDED: type = "GUIDED"; break;
	    case OMP_SCHED_RUNTIME: type = "RUNTIME"; break;
	    default:
	      gcc_unreachable ();
	    }
	  gfc_status (" SCHEDULE (%s", type);
	  if (omp_clauses->chunk_size)
	    {
	      gfc_status_char (',');
	      gfc_show_expr (omp_clauses->chunk_size);
	    }
	  gfc_status_char (')');
	}
      if (omp_clauses->default_sharing != OMP_DEFAULT_UNKNOWN)
	{
	  const char *type;
	  switch (omp_clauses->default_sharing)
	    {
	    case OMP_DEFAULT_NONE: type = "NONE"; break;
	    case OMP_DEFAULT_PRIVATE: type = "PRIVATE"; break;
	    case OMP_DEFAULT_SHARED: type = "SHARED"; break;
	    case OMP_SCHED_RUNTIME: type = "RUNTIME"; break;
	    default:
	      gcc_unreachable ();
	    }
	  gfc_status (" DEFAULT(%s)", type);
	}
      if (omp_clauses->ordered)
	gfc_status (" ORDERED");
      for (list_type = 0; list_type < OMP_LIST_NUM; list_type++)
	if (omp_clauses->lists[list_type] != NULL
	    && list_type != OMP_LIST_COPYPRIVATE)
	  {
	    const char *type;
	    if (list_type >= OMP_LIST_REDUCTION_FIRST)
	      {
		switch (list_type)
		  {
		  case OMP_LIST_PLUS: type = "+"; break;
		  case OMP_LIST_MULT: type = "*"; break;
		  case OMP_LIST_SUB: type = "-"; break;
		  case OMP_LIST_AND: type = ".AND."; break;
		  case OMP_LIST_OR: type = ".OR."; break;
		  case OMP_LIST_EQV: type = ".EQV."; break;
		  case OMP_LIST_NEQV: type = ".NEQV."; break;
		  case OMP_LIST_MAX: type = "MAX"; break;
		  case OMP_LIST_MIN: type = "MIN"; break;
		  case OMP_LIST_IAND: type = "IAND"; break;
		  case OMP_LIST_IOR: type = "IOR"; break;
		  case OMP_LIST_IEOR: type = "IEOR"; break;
		  default:
		    gcc_unreachable ();
		  }
		gfc_status (" REDUCTION(%s:", type);
	      }
	    else
	      {
		switch (list_type)
		  {
		  case OMP_LIST_PRIVATE: type = "PRIVATE"; break;
		  case OMP_LIST_FIRSTPRIVATE: type = "FIRSTPRIVATE"; break;
		  case OMP_LIST_LASTPRIVATE: type = "LASTPRIVATE"; break;
		  case OMP_LIST_SHARED: type = "SHARED"; break;
		  case OMP_LIST_COPYIN: type = "COPYIN"; break;
		  default:
		    gcc_unreachable ();
		  }
		gfc_status (" %s(", type);
	      }
	    gfc_show_namelist (omp_clauses->lists[list_type]);
	    gfc_status_char (')');
	  }
    }
  gfc_status_char ('\n');
  if (c->op == EXEC_OMP_SECTIONS || c->op == EXEC_OMP_PARALLEL_SECTIONS)
    {
      gfc_code *d = c->block;
      while (d != NULL)
	{
	  gfc_show_code (level + 1, d->next);
	  if (d->block == NULL)
	    break;
	  code_indent (level, 0);
	  gfc_status ("!$OMP SECTION\n");
	  d = d->block;
	}
    }
  else
    gfc_show_code (level + 1, c->block->next);
  if (c->op == EXEC_OMP_ATOMIC)
    return;
  code_indent (level, 0);
  gfc_status ("!$OMP END %s", name);
  if (omp_clauses != NULL)
    {
      if (omp_clauses->lists[OMP_LIST_COPYPRIVATE])
	{
	  gfc_status (" COPYPRIVATE(");
	  gfc_show_namelist (omp_clauses->lists[OMP_LIST_COPYPRIVATE]);
	  gfc_status_char (')');
	}
      else if (omp_clauses->nowait)
	gfc_status (" NOWAIT");
    }
  else if (c->op == EXEC_OMP_CRITICAL && c->ext.omp_name)
    gfc_status (" (%s)", c->ext.omp_name);
}

/* Show a single code node and everything underneath it if necessary.  */

static void
gfc_show_code_node (int level, gfc_code * c)
{
  gfc_forall_iterator *fa;
  gfc_open *open;
  gfc_case *cp;
  gfc_alloc *a;
  gfc_code *d;
  gfc_close *close;
  gfc_filepos *fp;
  gfc_inquire *i;
  gfc_dt *dt;

  code_indent (level, c->here);

  switch (c->op)
    {
    case EXEC_NOP:
      gfc_status ("NOP");
      break;

    case EXEC_CONTINUE:
      gfc_status ("CONTINUE");
      break;

    case EXEC_ENTRY:
      gfc_status ("ENTRY %s", c->ext.entry->sym->name);
      break;

    case EXEC_INIT_ASSIGN:
    case EXEC_ASSIGN:
      gfc_status ("ASSIGN ");
      gfc_show_expr (c->expr);
      gfc_status_char (' ');
      gfc_show_expr (c->expr2);
      break;

    case EXEC_LABEL_ASSIGN:
      gfc_status ("LABEL ASSIGN ");
      gfc_show_expr (c->expr);
      gfc_status (" %d", c->label->value);
      break;

    case EXEC_POINTER_ASSIGN:
      gfc_status ("POINTER ASSIGN ");
      gfc_show_expr (c->expr);
      gfc_status_char (' ');
      gfc_show_expr (c->expr2);
      break;

    case EXEC_GOTO:
      gfc_status ("GOTO ");
      if (c->label)
        gfc_status ("%d", c->label->value);
      else
        {
          gfc_show_expr (c->expr);
          d = c->block;
          if (d != NULL)
            {
              gfc_status (", (");
              for (; d; d = d ->block)
                {
                  code_indent (level, d->label);
                  if (d->block != NULL)
                    gfc_status_char (',');
                  else
                    gfc_status_char (')');
                }
            }
        }
      break;

    case EXEC_CALL:
      if (c->resolved_sym)
	gfc_status ("CALL %s ", c->resolved_sym->name);
      else if (c->symtree)
	gfc_status ("CALL %s ", c->symtree->name);
      else
	gfc_status ("CALL ?? ");

      gfc_show_actual_arglist (c->ext.actual);
      break;

    case EXEC_RETURN:
      gfc_status ("RETURN ");
      if (c->expr)
	gfc_show_expr (c->expr);
      break;

    case EXEC_PAUSE:
      gfc_status ("PAUSE ");

      if (c->expr != NULL)
        gfc_show_expr (c->expr);
      else
        gfc_status ("%d", c->ext.stop_code);

      break;

    case EXEC_STOP:
      gfc_status ("STOP ");

      if (c->expr != NULL)
        gfc_show_expr (c->expr);
      else
        gfc_status ("%d", c->ext.stop_code);

      break;

    case EXEC_ARITHMETIC_IF:
      gfc_status ("IF ");
      gfc_show_expr (c->expr);
      gfc_status (" %d, %d, %d",
		  c->label->value, c->label2->value, c->label3->value);
      break;

    case EXEC_IF:
      d = c->block;
      gfc_status ("IF ");
      gfc_show_expr (d->expr);
      gfc_status_char ('\n');
      gfc_show_code (level + 1, d->next);

      d = d->block;
      for (; d; d = d->block)
	{
	  code_indent (level, 0);

	  if (d->expr == NULL)
	    gfc_status ("ELSE\n");
	  else
	    {
	      gfc_status ("ELSE IF ");
	      gfc_show_expr (d->expr);
	      gfc_status_char ('\n');
	    }

	  gfc_show_code (level + 1, d->next);
	}

      code_indent (level, c->label);

      gfc_status ("ENDIF");
      break;

    case EXEC_SELECT:
      d = c->block;
      gfc_status ("SELECT CASE ");
      gfc_show_expr (c->expr);
      gfc_status_char ('\n');

      for (; d; d = d->block)
	{
	  code_indent (level, 0);

	  gfc_status ("CASE ");
	  for (cp = d->ext.case_list; cp; cp = cp->next)
	    {
	      gfc_status_char ('(');
	      gfc_show_expr (cp->low);
	      gfc_status_char (' ');
	      gfc_show_expr (cp->high);
	      gfc_status_char (')');
	      gfc_status_char (' ');
	    }
	  gfc_status_char ('\n');

	  gfc_show_code (level + 1, d->next);
	}

      code_indent (level, c->label);
      gfc_status ("END SELECT");
      break;

    case EXEC_WHERE:
      gfc_status ("WHERE ");

      d = c->block;
      gfc_show_expr (d->expr);
      gfc_status_char ('\n');

      gfc_show_code (level + 1, d->next);

      for (d = d->block; d; d = d->block)
	{
	  code_indent (level, 0);
	  gfc_status ("ELSE WHERE ");
	  gfc_show_expr (d->expr);
	  gfc_status_char ('\n');
	  gfc_show_code (level + 1, d->next);
	}

      code_indent (level, 0);
      gfc_status ("END WHERE");
      break;


    case EXEC_FORALL:
      gfc_status ("FORALL ");
      for (fa = c->ext.forall_iterator; fa; fa = fa->next)
	{
	  gfc_show_expr (fa->var);
	  gfc_status_char (' ');
	  gfc_show_expr (fa->start);
	  gfc_status_char (':');
	  gfc_show_expr (fa->end);
	  gfc_status_char (':');
	  gfc_show_expr (fa->stride);

	  if (fa->next != NULL)
	    gfc_status_char (',');
	}

      if (c->expr != NULL)
	{
	  gfc_status_char (',');
	  gfc_show_expr (c->expr);
	}
      gfc_status_char ('\n');

      gfc_show_code (level + 1, c->block->next);

      code_indent (level, 0);
      gfc_status ("END FORALL");
      break;

    case EXEC_DO:
      gfc_status ("DO ");

      gfc_show_expr (c->ext.iterator->var);
      gfc_status_char ('=');
      gfc_show_expr (c->ext.iterator->start);
      gfc_status_char (' ');
      gfc_show_expr (c->ext.iterator->end);
      gfc_status_char (' ');
      gfc_show_expr (c->ext.iterator->step);
      gfc_status_char ('\n');

      gfc_show_code (level + 1, c->block->next);

      code_indent (level, 0);
      gfc_status ("END DO");
      break;

    case EXEC_DO_WHILE:
      gfc_status ("DO WHILE ");
      gfc_show_expr (c->expr);
      gfc_status_char ('\n');

      gfc_show_code (level + 1, c->block->next);

      code_indent (level, c->label);
      gfc_status ("END DO");
      break;

    case EXEC_CYCLE:
      gfc_status ("CYCLE");
      if (c->symtree)
	gfc_status (" %s", c->symtree->n.sym->name);
      break;

    case EXEC_EXIT:
      gfc_status ("EXIT");
      if (c->symtree)
	gfc_status (" %s", c->symtree->n.sym->name);
      break;

    case EXEC_ALLOCATE:
      gfc_status ("ALLOCATE ");
      if (c->expr)
	{
	  gfc_status (" STAT=");
	  gfc_show_expr (c->expr);
	}

      for (a = c->ext.alloc_list; a; a = a->next)
	{
	  gfc_status_char (' ');
	  gfc_show_expr (a->expr);
	}

      break;

    case EXEC_DEALLOCATE:
      gfc_status ("DEALLOCATE ");
      if (c->expr)
	{
	  gfc_status (" STAT=");
	  gfc_show_expr (c->expr);
	}

      for (a = c->ext.alloc_list; a; a = a->next)
	{
	  gfc_status_char (' ');
	  gfc_show_expr (a->expr);
	}

      break;

    case EXEC_OPEN:
      gfc_status ("OPEN");
      open = c->ext.open;

      if (open->unit)
	{
	  gfc_status (" UNIT=");
	  gfc_show_expr (open->unit);
	}
      if (open->iomsg)
	{
	  gfc_status (" IOMSG=");
	  gfc_show_expr (open->iomsg);
	}
      if (open->iostat)
	{
	  gfc_status (" IOSTAT=");
	  gfc_show_expr (open->iostat);
	}
      if (open->file)
	{
	  gfc_status (" FILE=");
	  gfc_show_expr (open->file);
	}
      if (open->status)
	{
	  gfc_status (" STATUS=");
	  gfc_show_expr (open->status);
	}
      if (open->access)
	{
	  gfc_status (" ACCESS=");
	  gfc_show_expr (open->access);
	}
      if (open->form)
	{
	  gfc_status (" FORM=");
	  gfc_show_expr (open->form);
	}
      if (open->recl)
	{
	  gfc_status (" RECL=");
	  gfc_show_expr (open->recl);
	}
      if (open->blank)
	{
	  gfc_status (" BLANK=");
	  gfc_show_expr (open->blank);
	}
      if (open->position)
	{
	  gfc_status (" POSITION=");
	  gfc_show_expr (open->position);
	}
      if (open->action)
	{
	  gfc_status (" ACTION=");
	  gfc_show_expr (open->action);
	}
      if (open->delim)
	{
	  gfc_status (" DELIM=");
	  gfc_show_expr (open->delim);
	}
      if (open->pad)
	{
	  gfc_status (" PAD=");
	  gfc_show_expr (open->pad);
	}
      if (open->convert)
	{
	  gfc_status (" CONVERT=");
	  gfc_show_expr (open->convert);
	}
      if (open->err != NULL)
	gfc_status (" ERR=%d", open->err->value);

      break;

    case EXEC_CLOSE:
      gfc_status ("CLOSE");
      close = c->ext.close;

      if (close->unit)
	{
	  gfc_status (" UNIT=");
	  gfc_show_expr (close->unit);
	}
      if (close->iomsg)
	{
	  gfc_status (" IOMSG=");
	  gfc_show_expr (close->iomsg);
	}
      if (close->iostat)
	{
	  gfc_status (" IOSTAT=");
	  gfc_show_expr (close->iostat);
	}
      if (close->status)
	{
	  gfc_status (" STATUS=");
	  gfc_show_expr (close->status);
	}
      if (close->err != NULL)
	gfc_status (" ERR=%d", close->err->value);
      break;

    case EXEC_BACKSPACE:
      gfc_status ("BACKSPACE");
      goto show_filepos;

    case EXEC_ENDFILE:
      gfc_status ("ENDFILE");
      goto show_filepos;

    case EXEC_REWIND:
      gfc_status ("REWIND");
      goto show_filepos;

    case EXEC_FLUSH:
      gfc_status ("FLUSH");

    show_filepos:
      fp = c->ext.filepos;

      if (fp->unit)
	{
	  gfc_status (" UNIT=");
	  gfc_show_expr (fp->unit);
	}
      if (fp->iomsg)
	{
	  gfc_status (" IOMSG=");
	  gfc_show_expr (fp->iomsg);
	}
      if (fp->iostat)
	{
	  gfc_status (" IOSTAT=");
	  gfc_show_expr (fp->iostat);
	}
      if (fp->err != NULL)
	gfc_status (" ERR=%d", fp->err->value);
      break;

    case EXEC_INQUIRE:
      gfc_status ("INQUIRE");
      i = c->ext.inquire;

      if (i->unit)
	{
	  gfc_status (" UNIT=");
	  gfc_show_expr (i->unit);
	}
      if (i->file)
	{
	  gfc_status (" FILE=");
	  gfc_show_expr (i->file);
	}

      if (i->iomsg)
	{
	  gfc_status (" IOMSG=");
	  gfc_show_expr (i->iomsg);
	}
      if (i->iostat)
	{
	  gfc_status (" IOSTAT=");
	  gfc_show_expr (i->iostat);
	}
      if (i->exist)
	{
	  gfc_status (" EXIST=");
	  gfc_show_expr (i->exist);
	}
      if (i->opened)
	{
	  gfc_status (" OPENED=");
	  gfc_show_expr (i->opened);
	}
      if (i->number)
	{
	  gfc_status (" NUMBER=");
	  gfc_show_expr (i->number);
	}
      if (i->named)
	{
	  gfc_status (" NAMED=");
	  gfc_show_expr (i->named);
	}
      if (i->name)
	{
	  gfc_status (" NAME=");
	  gfc_show_expr (i->name);
	}
      if (i->access)
	{
	  gfc_status (" ACCESS=");
	  gfc_show_expr (i->access);
	}
      if (i->sequential)
	{
	  gfc_status (" SEQUENTIAL=");
	  gfc_show_expr (i->sequential);
	}

      if (i->direct)
	{
	  gfc_status (" DIRECT=");
	  gfc_show_expr (i->direct);
	}
      if (i->form)
	{
	  gfc_status (" FORM=");
	  gfc_show_expr (i->form);
	}
      if (i->formatted)
	{
	  gfc_status (" FORMATTED");
	  gfc_show_expr (i->formatted);
	}
      if (i->unformatted)
	{
	  gfc_status (" UNFORMATTED=");
	  gfc_show_expr (i->unformatted);
	}
      if (i->recl)
	{
	  gfc_status (" RECL=");
	  gfc_show_expr (i->recl);
	}
      if (i->nextrec)
	{
	  gfc_status (" NEXTREC=");
	  gfc_show_expr (i->nextrec);
	}
      if (i->blank)
	{
	  gfc_status (" BLANK=");
	  gfc_show_expr (i->blank);
	}
      if (i->position)
	{
	  gfc_status (" POSITION=");
	  gfc_show_expr (i->position);
	}
      if (i->action)
	{
	  gfc_status (" ACTION=");
	  gfc_show_expr (i->action);
	}
      if (i->read)
	{
	  gfc_status (" READ=");
	  gfc_show_expr (i->read);
	}
      if (i->write)
	{
	  gfc_status (" WRITE=");
	  gfc_show_expr (i->write);
	}
      if (i->readwrite)
	{
	  gfc_status (" READWRITE=");
	  gfc_show_expr (i->readwrite);
	}
      if (i->delim)
	{
	  gfc_status (" DELIM=");
	  gfc_show_expr (i->delim);
	}
      if (i->pad)
	{
	  gfc_status (" PAD=");
	  gfc_show_expr (i->pad);
	}
      if (i->convert)
	{
	  gfc_status (" CONVERT=");
	  gfc_show_expr (i->convert);
	}

      if (i->err != NULL)
	gfc_status (" ERR=%d", i->err->value);
      break;

    case EXEC_IOLENGTH:
      gfc_status ("IOLENGTH ");
      gfc_show_expr (c->expr);
      goto show_dt_code;
      break;

    case EXEC_READ:
      gfc_status ("READ");
      goto show_dt;

    case EXEC_WRITE:
      gfc_status ("WRITE");

    show_dt:
      dt = c->ext.dt;
      if (dt->io_unit)
	{
	  gfc_status (" UNIT=");
	  gfc_show_expr (dt->io_unit);
	}

      if (dt->format_expr)
	{
	  gfc_status (" FMT=");
	  gfc_show_expr (dt->format_expr);
	}

      if (dt->format_label != NULL)
	gfc_status (" FMT=%d", dt->format_label->value);
      if (dt->namelist)
	gfc_status (" NML=%s", dt->namelist->name);

      if (dt->iomsg)
	{
	  gfc_status (" IOMSG=");
	  gfc_show_expr (dt->iomsg);
	}
      if (dt->iostat)
	{
	  gfc_status (" IOSTAT=");
	  gfc_show_expr (dt->iostat);
	}
      if (dt->size)
	{
	  gfc_status (" SIZE=");
	  gfc_show_expr (dt->size);
	}
      if (dt->rec)
	{
	  gfc_status (" REC=");
	  gfc_show_expr (dt->rec);
	}
      if (dt->advance)
	{
	  gfc_status (" ADVANCE=");
	  gfc_show_expr (dt->advance);
	}

    show_dt_code:
      gfc_status_char ('\n');
      for (c = c->block->next; c; c = c->next)
	gfc_show_code_node (level + (c->next != NULL), c);
      return;

    case EXEC_TRANSFER:
      gfc_status ("TRANSFER ");
      gfc_show_expr (c->expr);
      break;

    case EXEC_DT_END:
      gfc_status ("DT_END");
      dt = c->ext.dt;

      if (dt->err != NULL)
	gfc_status (" ERR=%d", dt->err->value);
      if (dt->end != NULL)
	gfc_status (" END=%d", dt->end->value);
      if (dt->eor != NULL)
	gfc_status (" EOR=%d", dt->eor->value);
      break;

    case EXEC_OMP_ATOMIC:
    case EXEC_OMP_BARRIER:
    case EXEC_OMP_CRITICAL:
    case EXEC_OMP_FLUSH:
    case EXEC_OMP_DO:
    case EXEC_OMP_MASTER:
    case EXEC_OMP_ORDERED:
    case EXEC_OMP_PARALLEL:
    case EXEC_OMP_PARALLEL_DO:
    case EXEC_OMP_PARALLEL_SECTIONS:
    case EXEC_OMP_PARALLEL_WORKSHARE:
    case EXEC_OMP_SECTIONS:
    case EXEC_OMP_SINGLE:
    case EXEC_OMP_WORKSHARE:
      gfc_show_omp_node (level, c);
      break;

    default:
      gfc_internal_error ("gfc_show_code_node(): Bad statement code");
    }

  gfc_status_char ('\n');
}


/* Show an equivalence chain.  */

void
gfc_show_equiv (gfc_equiv *eq)
{
  show_indent ();
  gfc_status ("Equivalence: ");
  while (eq)
    {
      gfc_show_expr (eq->expr);
      eq = eq->eq;
      if (eq)
	gfc_status (", ");
    }
}

    
/* Show a freakin' whole namespace.  */

void
gfc_show_namespace (gfc_namespace * ns)
{
  gfc_interface *intr;
  gfc_namespace *save;
  gfc_intrinsic_op op;
  gfc_equiv *eq;
  int i;

  save = gfc_current_ns;
  show_level++;

  show_indent ();
  gfc_status ("Namespace:");

  if (ns != NULL)
    {
      i = 0;
      do
	{
	  int l = i;
	  while (i < GFC_LETTERS - 1
		 && gfc_compare_types(&ns->default_type[i+1],
				      &ns->default_type[l]))
	    i++;

	  if (i > l)
	    gfc_status(" %c-%c: ", l+'A', i+'A');
	  else
	    gfc_status(" %c: ", l+'A');

	  gfc_show_typespec(&ns->default_type[l]);
	  i++;
      } while (i < GFC_LETTERS);

      if (ns->proc_name != NULL)
	{
	  show_indent ();
	  gfc_status ("procedure name = %s", ns->proc_name->name);
	}

      gfc_current_ns = ns;
      gfc_traverse_symtree (ns->common_root, show_common);

      gfc_traverse_symtree (ns->sym_root, show_symtree);

      for (op = GFC_INTRINSIC_BEGIN; op != GFC_INTRINSIC_END; op++)
	{
	  /* User operator interfaces */
	  intr = ns->operator[op];
	  if (intr == NULL)
	    continue;

	  show_indent ();
	  gfc_status ("Operator interfaces for %s:", gfc_op2string (op));

	  for (; intr; intr = intr->next)
	    gfc_status (" %s", intr->sym->name);
	}

      if (ns->uop_root != NULL)
	{
	  show_indent ();
	  gfc_status ("User operators:\n");
	  gfc_traverse_user_op (ns, show_uop);
	}
    }
  
  for (eq = ns->equiv; eq; eq = eq->next)
    gfc_show_equiv (eq);

  gfc_status_char ('\n');
  gfc_status_char ('\n');

  gfc_show_code (0, ns->code);

  for (ns = ns->contained; ns; ns = ns->sibling)
    {
      show_indent ();
      gfc_status ("CONTAINS\n");
      gfc_show_namespace (ns);
    }

  show_level--;
  gfc_status_char ('\n');
  gfc_current_ns = save;
}
