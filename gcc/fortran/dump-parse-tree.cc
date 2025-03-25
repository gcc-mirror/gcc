/* Parse tree dumper
   Copyright (C) 2003-2025 Free Software Foundation, Inc.
   Contributed by Steven Bosscher

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


/* Actually this is just a collection of routines that used to be
   scattered around the sources.  Now that they are all in a single
   file, almost all of them can be static, and the other files don't
   have this mess in them.

   As a nice side-effect, this file can act as documentation of the
   gfc_code and gfc_expr structures and all their friends and
   relatives.

   TODO: Dump DATA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "gfortran.h"
#include "constructor.h"
#include "version.h"
#include "parse.h"  /* For gfc_ascii_statement.  */
#include "omp-api.h"  /* For omp_get_name_from_fr_id.  */
#include "gomp-constants.h"  /* For GOMP_INTEROP_IFR_SEPARATOR.  */

/* Keep track of indentation for symbol tree dumps.  */
static int show_level = 0;

/* The file handle we're dumping to is kept in a static variable.  This
   is not too cool, but it avoids a lot of passing it around.  */
static FILE *dumpfile;

/* Forward declaration of some of the functions.  */
static void show_expr (gfc_expr *p);
static void show_code_node (int, gfc_code *);
static void show_namespace (gfc_namespace *ns);
static void show_code (int, gfc_code *);
static void show_symbol (gfc_symbol *);
static void show_typespec (gfc_typespec *);
static void show_ref (gfc_ref *);
static void show_attr (symbol_attribute *, const char *);

DEBUG_FUNCTION void
debug (symbol_attribute *attr)
{
  FILE *tmp = dumpfile;
  dumpfile = stderr;
  show_attr (attr, NULL);
  fputc ('\n', dumpfile);
  dumpfile = tmp;
}

DEBUG_FUNCTION void
debug (gfc_formal_arglist *formal)
{
  FILE *tmp = dumpfile;
  dumpfile = stderr;
  for (; formal; formal = formal->next)
    {
      fputc ('\n', dumpfile);
      show_symbol (formal->sym);
    }
  fputc ('\n', dumpfile);
  dumpfile = tmp;
}

DEBUG_FUNCTION void
debug (symbol_attribute attr)
{
  debug (&attr);
}

DEBUG_FUNCTION void
debug (gfc_expr *e)
{
  FILE *tmp = dumpfile;
  dumpfile = stderr;
  if (e != NULL)
    {
      show_expr (e);
      fputc (' ', dumpfile);
      show_typespec (&e->ts);
    }
  else
    fputs ("() ", dumpfile);

  fputc ('\n', dumpfile);
  dumpfile = tmp;
}

DEBUG_FUNCTION void
debug (gfc_typespec *ts)
{
  FILE *tmp = dumpfile;
  dumpfile = stderr;
  show_typespec (ts);
  fputc ('\n', dumpfile);
  dumpfile = tmp;
}

DEBUG_FUNCTION void
debug (gfc_typespec ts)
{
  debug (&ts);
}

DEBUG_FUNCTION void
debug (gfc_ref *p)
{
  FILE *tmp = dumpfile;
  dumpfile = stderr;
  show_ref (p);
  fputc ('\n', dumpfile);
  dumpfile = tmp;
}

DEBUG_FUNCTION void
debug (gfc_namespace *ns)
{
  FILE *tmp = dumpfile;
  dumpfile = stderr;
  show_namespace (ns);
  fputc ('\n', dumpfile);
  dumpfile = tmp;
}

DEBUG_FUNCTION void
gfc_debug_expr (gfc_expr *e)
{
  FILE *tmp = dumpfile;
  dumpfile = stderr;
  show_expr (e);
  fputc ('\n', dumpfile);
  dumpfile = tmp;
}

/* Allow for dumping of a piece of code in the debugger.  */

DEBUG_FUNCTION void
gfc_debug_code (gfc_code *c)
{
  FILE *tmp = dumpfile;
  dumpfile = stderr;
  show_code (1, c);
  fputc ('\n', dumpfile);
  dumpfile = tmp;
}

DEBUG_FUNCTION void
debug (gfc_symbol *sym)
{
  FILE *tmp = dumpfile;
  dumpfile = stderr;
  show_symbol (sym);
  fputc ('\n', dumpfile);
  dumpfile = tmp;
}

/* Do indentation for a specific level.  */

static inline void
code_indent (int level, gfc_st_label *label)
{
  int i;

  if (label != NULL)
    fprintf (dumpfile, "%-5d ", label->value);

  for (i = 0; i < (2 * level - (label ? 6 : 0)); i++)
    fputc (' ', dumpfile);
}


/* Simple indentation at the current level.  This one
   is used to show symbols.  */

static inline void
show_indent (void)
{
  fputc ('\n', dumpfile);
  code_indent (show_level, NULL);
}


/* Show type-specific information.  */

static void
show_typespec (gfc_typespec *ts)
{
  if (ts->type == BT_ASSUMED)
    {
      fputs ("(TYPE(*))", dumpfile);
      return;
    }

  fprintf (dumpfile, "(%s ", gfc_basic_typename (ts->type));

  switch (ts->type)
    {
    case BT_DERIVED:
    case BT_CLASS:
    case BT_UNION:
      fprintf (dumpfile, "%s", ts->u.derived->name);
      break;

    case BT_CHARACTER:
      if (ts->u.cl)
	show_expr (ts->u.cl->length);
      fprintf(dumpfile, " %d", ts->kind);
      break;

    default:
      fprintf (dumpfile, "%d", ts->kind);
      break;
    }
  if (ts->is_c_interop)
    fputs (" C_INTEROP", dumpfile);

  if (ts->is_iso_c)
    fputs (" ISO_C", dumpfile);

  if (ts->deferred)
    fputs (" DEFERRED", dumpfile);

  fputc (')', dumpfile);
}


/* Show an actual argument list.  */

static void
show_actual_arglist (gfc_actual_arglist *a)
{
  fputc ('(', dumpfile);

  for (; a; a = a->next)
    {
      fputc ('(', dumpfile);
      if (a->name != NULL)
	fprintf (dumpfile, "%s = ", a->name);
      if (a->expr != NULL)
	show_expr (a->expr);
      else
	fputs ("(arg not-present)", dumpfile);

      fputc (')', dumpfile);
      if (a->next != NULL)
	fputc (' ', dumpfile);
    }

  fputc (')', dumpfile);
}


/* Show a gfc_array_spec array specification structure.  */

static void
show_array_spec (gfc_array_spec *as)
{
  const char *c;
  int i;

  if (as == NULL)
    {
      fputs ("()", dumpfile);
      return;
    }

  fprintf (dumpfile, "(%d [%d]", as->rank, as->corank);

  if (as->rank + as->corank > 0 || as->rank == -1)
    {
      switch (as->type)
      {
	case AS_EXPLICIT:      c = "AS_EXPLICIT";      break;
	case AS_DEFERRED:      c = "AS_DEFERRED";      break;
	case AS_ASSUMED_SIZE:  c = "AS_ASSUMED_SIZE";  break;
	case AS_ASSUMED_SHAPE: c = "AS_ASSUMED_SHAPE"; break;
	case AS_ASSUMED_RANK:  c = "AS_ASSUMED_RANK";  break;
	default:
	  gfc_internal_error ("show_array_spec(): Unhandled array shape "
			      "type.");
      }
      fprintf (dumpfile, " %s ", c);

      for (i = 0; i < as->rank + as->corank; i++)
	{
	  show_expr (as->lower[i]);
	  fputc (' ', dumpfile);
	  show_expr (as->upper[i]);
	  fputc (' ', dumpfile);
	}
    }

  fputc (')', dumpfile);
}


/* Show a gfc_array_ref array reference structure.  */

static void
show_array_ref (gfc_array_ref * ar)
{
  int i;

  fputc ('(', dumpfile);

  switch (ar->type)
    {
    case AR_FULL:
      fputs ("FULL", dumpfile);
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
	    show_expr (ar->start[i]);

	  if (ar->dimen_type[i] == DIMEN_RANGE)
	    {
	      fputc (':', dumpfile);

	      if (ar->end[i] != NULL)
		show_expr (ar->end[i]);

	      if (ar->stride[i] != NULL)
		{
		  fputc (':', dumpfile);
		  show_expr (ar->stride[i]);
		}
	    }

	  if (i != ar->dimen - 1)
	    fputs (" , ", dumpfile);
	}
      break;

    case AR_ELEMENT:
      for (i = 0; i < ar->dimen; i++)
	{
	  show_expr (ar->start[i]);
	  if (i != ar->dimen - 1)
	    fputs (" , ", dumpfile);
	}
      break;

    case AR_UNKNOWN:
      fputs ("UNKNOWN", dumpfile);
      break;

    default:
      gfc_internal_error ("show_array_ref(): Unknown array reference");
    }

  fputc (')', dumpfile);
  if (ar->codimen == 0)
    return;

  /* Show coarray part of the reference, if any.  */
  fputc ('[',dumpfile);
  for (i = ar->dimen; i < ar->dimen + ar->codimen; i++)
    {
      if (ar->dimen_type[i] == DIMEN_STAR)
	fputc('*',dumpfile);
      else if (ar->dimen_type[i] == DIMEN_THIS_IMAGE)
	fputs("THIS_IMAGE", dumpfile);
      else
	{
	  show_expr (ar->start[i]);
	  if (ar->end[i])
	    {
	      fputc(':', dumpfile);
	      show_expr (ar->end[i]);
	    }
	}
      if (i != ar->dimen + ar->codimen - 1)
	fputs (" , ", dumpfile);

    }
  fputc (']',dumpfile);
}


/* Show a list of gfc_ref structures.  */

static void
show_ref (gfc_ref *p)
{
  for (; p; p = p->next)
    switch (p->type)
      {
      case REF_ARRAY:
	show_array_ref (&p->u.ar);
	break;

      case REF_COMPONENT:
	fprintf (dumpfile, " %% %s", p->u.c.component->name);
	break;

      case REF_SUBSTRING:
	fputc ('(', dumpfile);
	show_expr (p->u.ss.start);
	fputc (':', dumpfile);
	show_expr (p->u.ss.end);
	fputc (')', dumpfile);
	break;

      case REF_INQUIRY:
	switch (p->u.i)
	{
	  case INQUIRY_KIND:
	    fprintf (dumpfile, " INQUIRY_KIND ");
	    break;
	  case INQUIRY_LEN:
	    fprintf (dumpfile, " INQUIRY_LEN ");
	    break;
	  case INQUIRY_RE:
	    fprintf (dumpfile, " INQUIRY_RE ");
	    break;
	  case INQUIRY_IM:
	    fprintf (dumpfile, " INQUIRY_IM ");
	}
	break;

      default:
	gfc_internal_error ("show_ref(): Bad component code");
      }
}


/* Display a constructor.  Works recursively for array constructors.  */

static void
show_constructor (gfc_constructor_base base)
{
  gfc_constructor *c;
  for (c = gfc_constructor_first (base); c; c = gfc_constructor_next (c))
    {
      if (c->iterator == NULL)
	show_expr (c->expr);
      else
	{
	  fputc ('(', dumpfile);
	  show_expr (c->expr);

	  fputc (' ', dumpfile);
	  show_expr (c->iterator->var);
	  fputc ('=', dumpfile);
	  show_expr (c->iterator->start);
	  fputc (',', dumpfile);
	  show_expr (c->iterator->end);
	  fputc (',', dumpfile);
	  show_expr (c->iterator->step);

	  fputc (')', dumpfile);
	}

      if (gfc_constructor_next (c) != NULL)
	fputs (" , ", dumpfile);
    }
}


static void
show_char_const (const gfc_char_t *c, gfc_charlen_t length)
{
  fputc ('\'', dumpfile);
  for (size_t i = 0; i < (size_t) length; i++)
    {
      if (c[i] == '\'')
	fputs ("''", dumpfile);
      else
	fputs (gfc_print_wide_char (c[i]), dumpfile);
    }
  fputc ('\'', dumpfile);
}


/* Show a component-call expression.  */

static void
show_compcall (gfc_expr* p)
{
  gcc_assert (p->expr_type == EXPR_COMPCALL);

  fprintf (dumpfile, "%s", p->symtree->n.sym->name);
  show_ref (p->ref);
  fprintf (dumpfile, "%s", p->value.compcall.name);

  show_actual_arglist (p->value.compcall.actual);
}


/* Show an expression.  */

static void
show_expr (gfc_expr *p)
{
  const char *c;
  int i;

  if (p == NULL)
    {
      fputs ("()", dumpfile);
      return;
    }

  switch (p->expr_type)
    {
    case EXPR_SUBSTRING:
      show_char_const (p->value.character.string, p->value.character.length);
      show_ref (p->ref);
      break;

    case EXPR_STRUCTURE:
      fprintf (dumpfile, "%s(", p->ts.u.derived->name);
      show_constructor (p->value.constructor);
      fputc (')', dumpfile);
      break;

    case EXPR_ARRAY:
      fputs ("(/ ", dumpfile);
      show_constructor (p->value.constructor);
      fputs (" /)", dumpfile);

      show_ref (p->ref);
      break;

    case EXPR_NULL:
      fputs ("NULL()", dumpfile);
      break;

    case EXPR_CONSTANT:
      switch (p->ts.type)
	{
	case BT_INTEGER:
	  mpz_out_str (dumpfile, 10, p->value.integer);

	  if (p->ts.kind != gfc_default_integer_kind)
	    fprintf (dumpfile, "_%d", p->ts.kind);
	  break;

	case BT_UNSIGNED:
	  mpz_out_str (dumpfile, 10, p->value.integer);
	  fputc('u', dumpfile);

	  if (p->ts.kind != gfc_default_integer_kind)
	    fprintf (dumpfile, "_%d", p->ts.kind);
	  break;

	case BT_LOGICAL:
	  if (p->value.logical)
	    fputs (".true.", dumpfile);
	  else
	    fputs (".false.", dumpfile);
	  break;

	case BT_REAL:
	  mpfr_out_str (dumpfile, 10, 0, p->value.real, GFC_RND_MODE);
	  if (p->ts.kind != gfc_default_real_kind)
	    fprintf (dumpfile, "_%d", p->ts.kind);
	  break;

	case BT_CHARACTER:
	  show_char_const (p->value.character.string,
			   p->value.character.length);
	  break;

	case BT_COMPLEX:
	  fputs ("(complex ", dumpfile);

	  mpfr_out_str (dumpfile, 10, 0, mpc_realref (p->value.complex),
			GFC_RND_MODE);
	  if (p->ts.kind != gfc_default_complex_kind)
	    fprintf (dumpfile, "_%d", p->ts.kind);

	  fputc (' ', dumpfile);

	  mpfr_out_str (dumpfile, 10, 0, mpc_imagref (p->value.complex),
			GFC_RND_MODE);
	  if (p->ts.kind != gfc_default_complex_kind)
	    fprintf (dumpfile, "_%d", p->ts.kind);

	  fputc (')', dumpfile);
	  break;

	case BT_BOZ:
	  if (p->boz.rdx == 2)
	    fputs ("b'", dumpfile);
	  else if (p->boz.rdx == 8)
	    fputs ("o'", dumpfile);
	  else
	    fputs ("z'", dumpfile);
	  fprintf (dumpfile, "%s'", p->boz.str);
	  break;

	case BT_HOLLERITH:
	  fprintf (dumpfile, HOST_WIDE_INT_PRINT_DEC "H",
		   p->representation.length);
	  c = p->representation.string;
	  for (i = 0; i < p->representation.length; i++, c++)
	    {
	      fputc (*c, dumpfile);
	    }
	  break;

	default:
	  fputs ("???", dumpfile);
	  break;
	}

      if (p->representation.string)
	{
	  fputs (" {", dumpfile);
	  c = p->representation.string;
	  for (i = 0; i < p->representation.length; i++, c++)
	    {
	      fprintf (dumpfile, "%.2x", (unsigned int) *c);
	      if (i < p->representation.length - 1)
		fputc (',', dumpfile);
	    }
	  fputc ('}', dumpfile);
	}

      break;

    case EXPR_VARIABLE:
      if (p->symtree->n.sym->ns && p->symtree->n.sym->ns->proc_name)
	fprintf (dumpfile, "%s:", p->symtree->n.sym->ns->proc_name->name);
      fprintf (dumpfile, "%s", p->symtree->n.sym->name);
      show_ref (p->ref);
      break;

    case EXPR_OP:
      fputc ('(', dumpfile);
      switch (p->value.op.op)
	{
	case INTRINSIC_UPLUS:
	  fputs ("U+ ", dumpfile);
	  break;
	case INTRINSIC_UMINUS:
	  fputs ("U- ", dumpfile);
	  break;
	case INTRINSIC_PLUS:
	  fputs ("+ ", dumpfile);
	  break;
	case INTRINSIC_MINUS:
	  fputs ("- ", dumpfile);
	  break;
	case INTRINSIC_TIMES:
	  fputs ("* ", dumpfile);
	  break;
	case INTRINSIC_DIVIDE:
	  fputs ("/ ", dumpfile);
	  break;
	case INTRINSIC_POWER:
	  fputs ("** ", dumpfile);
	  break;
	case INTRINSIC_CONCAT:
	  fputs ("// ", dumpfile);
	  break;
	case INTRINSIC_AND:
	  fputs ("AND ", dumpfile);
	  break;
	case INTRINSIC_OR:
	  fputs ("OR ", dumpfile);
	  break;
	case INTRINSIC_EQV:
	  fputs ("EQV ", dumpfile);
	  break;
	case INTRINSIC_NEQV:
	  fputs ("NEQV ", dumpfile);
	  break;
	case INTRINSIC_EQ:
	case INTRINSIC_EQ_OS:
	  fputs ("== ", dumpfile);
	  break;
	case INTRINSIC_NE:
	case INTRINSIC_NE_OS:
	  fputs ("/= ", dumpfile);
	  break;
	case INTRINSIC_GT:
	case INTRINSIC_GT_OS:
	  fputs ("> ", dumpfile);
	  break;
	case INTRINSIC_GE:
	case INTRINSIC_GE_OS:
	  fputs (">= ", dumpfile);
	  break;
	case INTRINSIC_LT:
	case INTRINSIC_LT_OS:
	  fputs ("< ", dumpfile);
	  break;
	case INTRINSIC_LE:
	case INTRINSIC_LE_OS:
	  fputs ("<= ", dumpfile);
	  break;
	case INTRINSIC_NOT:
	  fputs ("NOT ", dumpfile);
	  break;
	case INTRINSIC_PARENTHESES:
	  fputs ("parens ", dumpfile);
	  break;

	default:
	  gfc_internal_error
	    ("show_expr(): Bad intrinsic in expression");
	}

      show_expr (p->value.op.op1);

      if (p->value.op.op2)
	{
	  fputc (' ', dumpfile);
	  show_expr (p->value.op.op2);
	}

      fputc (')', dumpfile);
      break;

    case EXPR_FUNCTION:
      if (p->value.function.name == NULL)
	{
	  fprintf (dumpfile, "%s", p->symtree->n.sym->name);
	  if (gfc_is_proc_ptr_comp (p))
	    show_ref (p->ref);
	  fputc ('[', dumpfile);
	  show_actual_arglist (p->value.function.actual);
	  fputc (']', dumpfile);
	}
      else
	{
	  fprintf (dumpfile, "%s", p->value.function.name);
	  if (gfc_is_proc_ptr_comp (p))
	    show_ref (p->ref);
	  fputc ('[', dumpfile);
	  fputc ('[', dumpfile);
	  show_actual_arglist (p->value.function.actual);
	  fputc (']', dumpfile);
	  fputc (']', dumpfile);
	}

      break;

    case EXPR_COMPCALL:
      show_compcall (p);
      break;

    default:
      gfc_internal_error ("show_expr(): Don't know how to show expr");
    }
}

/* Show symbol attributes.  The flavor and intent are followed by
   whatever single bit attributes are present.  */

static void
show_attr (symbol_attribute *attr, const char * module)
{
  fputc ('(', dumpfile);
  if (attr->flavor != FL_UNKNOWN)
    {
      if (attr->flavor == FL_DERIVED && attr->pdt_template)
	fputs ("PDT-TEMPLATE ", dumpfile);
      else
	fprintf (dumpfile, "%s ", gfc_code2string (flavors, attr->flavor));
    }
  if (attr->access != ACCESS_UNKNOWN)
    fprintf (dumpfile, "%s ", gfc_code2string (access_types, attr->access));
  if (attr->proc != PROC_UNKNOWN)
    fprintf (dumpfile, "%s ", gfc_code2string (procedures, attr->proc));
  if (attr->save != SAVE_NONE)
    fprintf (dumpfile, "%s", gfc_code2string (save_status, attr->save));

  if (attr->artificial)
    fputs (" ARTIFICIAL", dumpfile);
  if (attr->allocatable)
    fputs (" ALLOCATABLE", dumpfile);
  if (attr->asynchronous)
    fputs (" ASYNCHRONOUS", dumpfile);
  if (attr->codimension)
    fputs (" CODIMENSION", dumpfile);
  if (attr->dimension)
    fputs (" DIMENSION", dumpfile);
  if (attr->contiguous)
    fputs (" CONTIGUOUS", dumpfile);
  if (attr->external)
    fputs (" EXTERNAL", dumpfile);
  if (attr->intrinsic)
    fputs (" INTRINSIC", dumpfile);
  if (attr->optional)
    fputs (" OPTIONAL", dumpfile);
  if (attr->pdt_kind)
    fputs (" KIND", dumpfile);
  if (attr->pdt_len)
    fputs (" LEN", dumpfile);
  if (attr->pointer)
    fputs (" POINTER", dumpfile);
  if (attr->subref_array_pointer)
    fputs (" SUBREF-ARRAY-POINTER", dumpfile);
  if (attr->cray_pointer)
    fputs (" CRAY-POINTER", dumpfile);
  if (attr->cray_pointee)
    fputs (" CRAY-POINTEE", dumpfile);
  if (attr->is_protected)
    fputs (" PROTECTED", dumpfile);
  if (attr->value)
    fputs (" VALUE", dumpfile);
  if (attr->volatile_)
    fputs (" VOLATILE", dumpfile);
  if (attr->threadprivate)
    fputs (" THREADPRIVATE", dumpfile);
  if (attr->temporary)
    fputs (" TEMPORARY", dumpfile);
  if (attr->target)
    fputs (" TARGET", dumpfile);
  if (attr->dummy)
    {
      fputs (" DUMMY", dumpfile);
      if (attr->intent != INTENT_UNKNOWN)
	fprintf (dumpfile, "(%s)", gfc_intent_string (attr->intent));
    }

  if (attr->result)
    fputs (" RESULT", dumpfile);
  if (attr->entry)
    fputs (" ENTRY", dumpfile);
  if (attr->entry_master)
    fputs (" ENTRY-MASTER", dumpfile);
  if (attr->mixed_entry_master)
    fputs (" MIXED-ENTRY-MASTER", dumpfile);
  if (attr->is_bind_c)
    fputs (" BIND(C)", dumpfile);

  if (attr->data)
    fputs (" DATA", dumpfile);
  if (attr->use_assoc)
    {
      fputs (" USE-ASSOC", dumpfile);
      if (module != NULL)
	fprintf (dumpfile, "(%s)", module);
    }

  if (attr->in_namelist)
    fputs (" IN-NAMELIST", dumpfile);
  if (attr->in_common)
    fputs (" IN-COMMON", dumpfile);
  if (attr->in_equivalence)
    fputs (" IN-EQUIVALENCE", dumpfile);

  if (attr->abstract)
    fputs (" ABSTRACT", dumpfile);
  if (attr->function)
    fputs (" FUNCTION", dumpfile);
  if (attr->subroutine)
    fputs (" SUBROUTINE", dumpfile);
  if (attr->implicit_type)
    fputs (" IMPLICIT-TYPE", dumpfile);

  if (attr->sequence)
    fputs (" SEQUENCE", dumpfile);
  if (attr->alloc_comp)
    fputs (" ALLOC-COMP", dumpfile);
  if (attr->pointer_comp)
    fputs (" POINTER-COMP", dumpfile);
  if (attr->proc_pointer_comp)
    fputs (" PROC-POINTER-COMP", dumpfile);
  if (attr->private_comp)
    fputs (" PRIVATE-COMP", dumpfile);
  if (attr->zero_comp)
    fputs (" ZERO-COMP", dumpfile);
  if (attr->coarray_comp)
    fputs (" COARRAY-COMP", dumpfile);
  if (attr->lock_comp)
    fputs (" LOCK-COMP", dumpfile);
  if (attr->event_comp)
    fputs (" EVENT-COMP", dumpfile);
  if (attr->defined_assign_comp)
    fputs (" DEFINED-ASSIGNED-COMP", dumpfile);
  if (attr->unlimited_polymorphic)
    fputs (" UNLIMITED-POLYMORPHIC", dumpfile);
  if (attr->has_dtio_procs)
    fputs (" HAS-DTIO-PROCS", dumpfile);
  if (attr->caf_token)
    fputs (" CAF-TOKEN", dumpfile);
  if (attr->select_type_temporary)
    fputs (" SELECT-TYPE-TEMPORARY", dumpfile);
  if (attr->associate_var)
    fputs (" ASSOCIATE-VAR", dumpfile);
  if (attr->pdt_kind)
    fputs (" PDT-KIND", dumpfile);
  if (attr->pdt_len)
    fputs (" PDT-LEN", dumpfile);
  if (attr->pdt_type)
    fputs (" PDT-TYPE", dumpfile);
  if (attr->pdt_array)
    fputs (" PDT-ARRAY", dumpfile);
  if (attr->pdt_string)
    fputs (" PDT-STRING", dumpfile);
  if (attr->omp_udr_artificial_var)
    fputs (" OMP-UDR-ARTIFICIAL-VAR", dumpfile);
  if (attr->omp_declare_target)
    fputs (" OMP-DECLARE-TARGET", dumpfile);
  if (attr->omp_declare_target_link)
    fputs (" OMP-DECLARE-TARGET-LINK", dumpfile);
  if (attr->omp_declare_target_indirect)
    fputs (" OMP-DECLARE-TARGET-INDIRECT", dumpfile);
  if (attr->omp_device_type == OMP_DEVICE_TYPE_HOST)
    fputs (" OMP-DEVICE-TYPE-HOST", dumpfile);
  if (attr->omp_device_type == OMP_DEVICE_TYPE_NOHOST)
    fputs (" OMP-DEVICE-TYPE-NOHOST", dumpfile);
  if (attr->omp_device_type == OMP_DEVICE_TYPE_ANY)
    fputs (" OMP-DEVICE-TYPE-ANY", dumpfile);
  if (attr->omp_allocate)
    fputs (" OMP-ALLOCATE", dumpfile);

  if (attr->oacc_declare_create)
    fputs (" OACC-DECLARE-CREATE", dumpfile);
  if (attr->oacc_declare_copyin)
    fputs (" OACC-DECLARE-COPYIN", dumpfile);
  if (attr->oacc_declare_deviceptr)
    fputs (" OACC-DECLARE-DEVICEPTR", dumpfile);
  if (attr->oacc_declare_device_resident)
    fputs (" OACC-DECLARE-DEVICE-RESIDENT", dumpfile);

  switch (attr->oacc_routine_lop)
    {
    case OACC_ROUTINE_LOP_NONE:
    case OACC_ROUTINE_LOP_ERROR:
      break;

    case OACC_ROUTINE_LOP_GANG:
      fputs (" OACC-ROUTINE-LOP-GANG", dumpfile);
      break;

    case OACC_ROUTINE_LOP_WORKER:
      fputs (" OACC-ROUTINE-LOP-WORKER", dumpfile);
      break;

    case  OACC_ROUTINE_LOP_VECTOR:
      fputs (" OACC-ROUTINE-LOP-VECTOR", dumpfile);
      break;

    case OACC_ROUTINE_LOP_SEQ:
      fputs (" OACC-ROUTINE-LOP-SEQ", dumpfile);
      break;
      }

  if (attr->elemental)
    fputs (" ELEMENTAL", dumpfile);
  if (attr->pure)
    fputs (" PURE", dumpfile);
  if (attr->implicit_pure)
    fputs (" IMPLICIT-PURE", dumpfile);
  if (attr->recursive)
    fputs (" RECURSIVE", dumpfile);
  if (attr->unmaskable)
    fputs (" UNMASKABKE", dumpfile);
  if (attr->masked)
    fputs (" MASKED", dumpfile);
  if (attr->contained)
    fputs (" CONTAINED", dumpfile);
  if (attr->mod_proc)
    fputs (" MOD-PROC", dumpfile);
  if (attr->module_procedure)
    fputs (" MODULE-PROCEDURE", dumpfile);
  if (attr->public_used)
    fputs (" PUBLIC_USED", dumpfile);
  if (attr->array_outer_dependency)
    fputs (" ARRAY-OUTER-DEPENDENCY", dumpfile);
  if (attr->noreturn)
    fputs (" NORETURN", dumpfile);
  if (attr->always_explicit)
    fputs (" ALWAYS-EXPLICIT", dumpfile);
  if (attr->is_main_program)
    fputs (" IS-MAIN-PROGRAM", dumpfile);
  if (attr->oacc_routine_nohost)
    fputs (" OACC-ROUTINE-NOHOST", dumpfile);
  if (attr->temporary)
    fputs (" TEMPORARY", dumpfile);
  if (attr->assign)
    fputs (" ASSIGN", dumpfile);
  if (attr->not_always_present)
    fputs (" NOT-ALWAYS-PRESENT", dumpfile);
  if (attr->implied_index)
    fputs (" IMPLIED-INDEX", dumpfile);
  if (attr->proc_pointer)
    fputs (" PROC-POINTER", dumpfile);
  if (attr->fe_temp)
    fputs (" FE-TEMP", dumpfile);
  if (attr->automatic)
    fputs (" AUTOMATIC", dumpfile);
  if (attr->class_pointer)
    fputs (" CLASS-POINTER", dumpfile);
  if (attr->used_in_submodule)
    fputs (" USED-IN-SUBMODULE", dumpfile);
  if (attr->use_only)
    fputs (" USE-ONLY", dumpfile);
  if (attr->use_rename)
    fputs (" USE-RENAME", dumpfile);
  if (attr->imported)
    fputs (" IMPORTED", dumpfile);
  if (attr->host_assoc)
    fputs (" HOST-ASSOC", dumpfile);
  if (attr->generic)
    fputs (" GENERIC", dumpfile);
  if (attr->generic_copy)
    fputs (" GENERIC-COPY", dumpfile);
  if (attr->untyped)
    fputs (" UNTYPED", dumpfile);
  if (attr->extension)
    fprintf (dumpfile, " EXTENSION(%u)", attr->extension);
  if (attr->is_class)
    fputs (" IS-CLASS", dumpfile);
  if (attr->class_ok)
    fputs (" CLASS-OK", dumpfile);
  if (attr->vtab)
    fputs (" VTAB", dumpfile);
  if (attr->vtype)
    fputs (" VTYPE", dumpfile);
  if (attr->module_procedure)
    fputs (" MODULE-PROCEDURE", dumpfile);
  if (attr->if_source == IFSRC_DECL)
    fputs (" IFSRC-DECL", dumpfile);
  if (attr->if_source == IFSRC_IFBODY)
    fputs (" IFSRC-IFBODY", dumpfile);

  for (int i = 0; i < EXT_ATTR_LAST; i++)
    {
      if (attr->ext_attr & (1 << i))
	{
	  fputs (" ATTRIBUTE-", dumpfile);
	  for (const char *p = ext_attr_list[i].name; p && *p; p++)
	    putc (TOUPPER (*p), dumpfile);
	}
    }

  fputc (')', dumpfile);
}


/* Show components of a derived type.  */

static void
show_components (gfc_symbol *sym)
{
  gfc_component *c;

  for (c = sym->components; c; c = c->next)
    {
      show_indent ();
      fprintf (dumpfile, "(%s ", c->name);
      show_typespec (&c->ts);
      if (c->kind_expr)
	{
	  fputs (" kind_expr: ", dumpfile);
	  show_expr (c->kind_expr);
	}
      if (c->param_list)
	{
	  fputs ("PDT parameters", dumpfile);
	  show_actual_arglist (c->param_list);
	}

      if (c->attr.allocatable)
	fputs (" ALLOCATABLE", dumpfile);
      if (c->attr.pdt_kind)
	fputs (" KIND", dumpfile);
      if (c->attr.pdt_len)
	fputs (" LEN", dumpfile);
      if (c->attr.pointer)
	fputs (" POINTER", dumpfile);
      if (c->attr.proc_pointer)
	fputs (" PPC", dumpfile);
      if (c->attr.dimension)
	fputs (" DIMENSION", dumpfile);
      fputc (' ', dumpfile);
      show_array_spec (c->as);
      if (c->attr.access)
	fprintf (dumpfile, " %s", gfc_code2string (access_types, c->attr.access));
      fputc (')', dumpfile);
      if (c->next != NULL)
	fputc (' ', dumpfile);
    }
}


/* Show the f2k_derived namespace with procedure bindings.  */

static void
show_typebound_proc (gfc_typebound_proc* tb, const char* name)
{
  show_indent ();

  if (tb->is_generic)
    fputs ("GENERIC", dumpfile);
  else
    {
      fputs ("PROCEDURE, ", dumpfile);
      if (tb->nopass)
	fputs ("NOPASS", dumpfile);
      else
	{
	  if (tb->pass_arg)
	    fprintf (dumpfile, "PASS(%s)", tb->pass_arg);
	  else
	    fputs ("PASS", dumpfile);
	}
      if (tb->non_overridable)
	fputs (", NON_OVERRIDABLE", dumpfile);
    }

  if (tb->access == ACCESS_PUBLIC)
    fputs (", PUBLIC", dumpfile);
  else
    fputs (", PRIVATE", dumpfile);

  fprintf (dumpfile, " :: %s => ", name);

  if (tb->is_generic)
    {
      gfc_tbp_generic* g;
      for (g = tb->u.generic; g; g = g->next)
	{
	  fputs (g->specific_st->name, dumpfile);
	  if (g->next)
	    fputs (", ", dumpfile);
	}
    }
  else
    fputs (tb->u.specific->n.sym->name, dumpfile);
}

static void
show_typebound_symtree (gfc_symtree* st)
{
  gcc_assert (st->n.tb);
  show_typebound_proc (st->n.tb, st->name);
}

static void
show_f2k_derived (gfc_namespace* f2k)
{
  gfc_finalizer* f;
  int op;

  show_indent ();
  fputs ("Procedure bindings:", dumpfile);
  ++show_level;

  /* Finalizer bindings.  */
  for (f = f2k->finalizers; f; f = f->next)
    {
      show_indent ();
      fprintf (dumpfile, "FINAL %s", f->proc_tree->n.sym->name);
    }

  /* Type-bound procedures.  */
  gfc_traverse_symtree (f2k->tb_sym_root, &show_typebound_symtree);

  --show_level;

  show_indent ();
  fputs ("Operator bindings:", dumpfile);
  ++show_level;

  /* User-defined operators.  */
  gfc_traverse_symtree (f2k->tb_uop_root, &show_typebound_symtree);

  /* Intrinsic operators.  */
  for (op = GFC_INTRINSIC_BEGIN; op != GFC_INTRINSIC_END; ++op)
    if (f2k->tb_op[op])
      show_typebound_proc (f2k->tb_op[op],
			   gfc_op2string ((gfc_intrinsic_op) op));

  --show_level;
}


/* Show a symbol.  If a symbol is an ENTRY, SUBROUTINE or FUNCTION, we
   show the interface.  Information needed to reconstruct the list of
   specific interfaces associated with a generic symbol is done within
   that symbol.  */

static void
show_symbol (gfc_symbol *sym)
{
  gfc_formal_arglist *formal;
  gfc_interface *intr;
  int i,len;

  if (sym == NULL)
    return;

  fprintf (dumpfile, "|| symbol: '%s' ", sym->name);
  len = strlen (sym->name);
  for (i=len; i<12; i++)
    fputc(' ', dumpfile);

  if (sym->binding_label)
      fprintf (dumpfile,"|| binding_label: '%s' ", sym->binding_label);

  ++show_level;

  show_indent ();
  fputs ("type spec : ", dumpfile);
  show_typespec (&sym->ts);

  show_indent ();
  fputs ("attributes: ", dumpfile);
  show_attr (&sym->attr, sym->module);

  if (sym->value)
    {
      show_indent ();
      fputs ("value: ", dumpfile);
      show_expr (sym->value);
    }

  if (sym->ts.type != BT_CLASS && sym->as)
    {
      show_indent ();
      fputs ("Array spec:", dumpfile);
      show_array_spec (sym->as);
    }
  else if (sym->ts.type == BT_CLASS && CLASS_DATA (sym)->as)
    {
      show_indent ();
      fputs ("Array spec:", dumpfile);
      show_array_spec (CLASS_DATA (sym)->as);
    }

  if (sym->generic)
    {
      show_indent ();
      fputs ("Generic interfaces:", dumpfile);
      for (intr = sym->generic; intr; intr = intr->next)
	fprintf (dumpfile, " %s", intr->sym->name);
    }

  if (sym->result)
    {
      show_indent ();
      fprintf (dumpfile, "result: %s", sym->result->name);
    }

  if (sym->components)
    {
      show_indent ();
      fputs ("components: ", dumpfile);
      show_components (sym);
    }

  if (sym->f2k_derived)
    {
      show_indent ();
      if (sym->hash_value)
	fprintf (dumpfile, "hash: %d", sym->hash_value);
      show_f2k_derived (sym->f2k_derived);
    }

  if (sym->formal)
    {
      show_indent ();
      fputs ("Formal arglist:", dumpfile);

      for (formal = sym->formal; formal; formal = formal->next)
	{
	  if (formal->sym != NULL)
	    fprintf (dumpfile, " %s", formal->sym->name);
	  else
	    fputs (" [Alt Return]", dumpfile);
	}
    }

  if (sym->formal_ns && (sym->formal_ns->proc_name != sym)
      && sym->attr.proc != PROC_ST_FUNCTION
      && !sym->attr.entry)
    {
      show_indent ();
      fputs ("Formal namespace", dumpfile);
      show_namespace (sym->formal_ns);
    }

  if (sym->attr.flavor == FL_VARIABLE
      && sym->param_list)
    {
      show_indent ();
      fputs ("PDT parameters", dumpfile);
      show_actual_arglist (sym->param_list);
    }

  if (sym->attr.flavor == FL_NAMELIST)
    {
      gfc_namelist *nl;
      show_indent ();
      fputs ("variables : ", dumpfile);
      for (nl = sym->namelist; nl; nl = nl->next)
	fprintf (dumpfile, " %s",nl->sym->name);
    }

  --show_level;
}


/* Show a user-defined operator.  Just prints an operator
   and the name of the associated subroutine, really.  */

static void
show_uop (gfc_user_op *uop)
{
  gfc_interface *intr;

  show_indent ();
  fprintf (dumpfile, "%s:", uop->name);

  for (intr = uop->op; intr; intr = intr->next)
    fprintf (dumpfile, " %s", intr->sym->name);
}


/* Workhorse function for traversing the user operator symtree.  */

static void
traverse_uop (gfc_symtree *st, void (*func) (gfc_user_op *))
{
  if (st == NULL)
    return;

  (*func) (st->n.uop);

  traverse_uop (st->left, func);
  traverse_uop (st->right, func);
}


/* Traverse the tree of user operator nodes.  */

void
gfc_traverse_user_op (gfc_namespace *ns, void (*func) (gfc_user_op *))
{
  traverse_uop (ns->uop_root, func);
}


/* Function to display a common block.  */

static void
show_common (gfc_symtree *st)
{
  gfc_symbol *s;

  show_indent ();
  fprintf (dumpfile, "common: /%s/ ", st->name);

  s = st->n.common->head;
  while (s)
    {
      fprintf (dumpfile, "%s", s->name);
      s = s->common_next;
      if (s)
	fputs (", ", dumpfile);
    }
  fputc ('\n', dumpfile);
}


/* Worker function to display the symbol tree.  */

static void
show_symtree (gfc_symtree *st)
{
  int len, i;

  show_indent ();

  len = strlen(st->name);
  fprintf (dumpfile, "symtree: '%s'", st->name);

  for (i=len; i<12; i++)
    fputc(' ', dumpfile);

  if (st->ambiguous)
    fputs( " Ambiguous", dumpfile);

  if (st->n.sym->ns != gfc_current_ns)
    fprintf (dumpfile, "|| symbol: '%s' from namespace '%s'", st->n.sym->name,
	     st->n.sym->ns->proc_name->name);
  else
    show_symbol (st->n.sym);
}


/******************* Show gfc_code structures **************/


/* Show a list of code structures.  Mutually recursive with
   show_code_node().  */

static void
show_code (int level, gfc_code *c)
{
  for (; c; c = c->next)
    show_code_node (level, c);
}

static void
show_iterator (gfc_namespace *ns)
{
  for (gfc_symbol *sym = ns->omp_affinity_iterators; sym; sym = sym->tlink)
    {
      gfc_constructor *c;
      if (sym != ns->omp_affinity_iterators)
	fputc (',', dumpfile);
      fputs (sym->name, dumpfile);
      fputc ('=', dumpfile);
      c = gfc_constructor_first (sym->value->value.constructor);
      show_expr (c->expr);
      fputc (':', dumpfile);
      c = gfc_constructor_next (c);
      show_expr (c->expr);
      c = gfc_constructor_next (c);
      if (c)
	{
	  fputc (':', dumpfile);
	  show_expr (c->expr);
	}
    }
}

static void
show_omp_namelist (int list_type, gfc_omp_namelist *n)
{
  gfc_namespace *ns_iter = NULL, *ns_curr = gfc_current_ns;
  gfc_omp_namelist *n2 = n;
  for (; n; n = n->next)
    {
      gfc_current_ns = ns_curr;
      if (list_type == OMP_LIST_AFFINITY || list_type == OMP_LIST_DEPEND)
	{
	  gfc_current_ns = n->u2.ns ? n->u2.ns : ns_curr;
	  if (n->u2.ns != ns_iter)
	    {
	      if (n != n2)
		{
		  fputs (") ", dumpfile);
		  if (list_type == OMP_LIST_AFFINITY)
		    fputs ("AFFINITY (", dumpfile);
		  else if (n->u.depend_doacross_op == OMP_DOACROSS_SINK_FIRST)
		    fputs ("DOACROSS (", dumpfile);
		  else
		    fputs ("DEPEND (", dumpfile);
		}
	      if (n->u2.ns)
		{
		  fputs ("ITERATOR(", dumpfile);
		  show_iterator (n->u2.ns);
		  fputc (')', dumpfile);
		  fputc (list_type == OMP_LIST_AFFINITY ? ':' : ',', dumpfile);
		}
	    }
	  ns_iter = n->u2.ns;
	}
      else if (list_type == OMP_LIST_INIT && n != n2)
	fputs (") INIT(", dumpfile);
      if (list_type == OMP_LIST_ALLOCATE)
	{
	  if (n->u2.allocator)
	    {
	      fputs ("allocator(", dumpfile);
	      show_expr (n->u2.allocator);
	      fputc (')', dumpfile);
	    }
	  if (n->expr && n->u.align)
	    fputc (',', dumpfile);
	  if (n->u.align)
	    {
	      fputs ("align(", dumpfile);
	      show_expr (n->u.align);
	      fputc (')', dumpfile);
	    }
	  if (n->u2.allocator || n->u.align)
	    fputc (':', dumpfile);
	  if (n->expr)
	    show_expr (n->expr);
	  else
	    fputs (n->sym->name, dumpfile);
	  if (n->next)
	    fputs (") ALLOCATE(", dumpfile);
	  continue;
	}
      if ((list_type == OMP_LIST_MAP || list_type == OMP_LIST_CACHE)
	  && n->u.map.readonly)
	fputs ("readonly,", dumpfile);
      if (list_type == OMP_LIST_REDUCTION)
	switch (n->u.reduction_op)
	  {
	  case OMP_REDUCTION_PLUS:
	  case OMP_REDUCTION_TIMES:
	  case OMP_REDUCTION_MINUS:
	  case OMP_REDUCTION_AND:
	  case OMP_REDUCTION_OR:
	  case OMP_REDUCTION_EQV:
	  case OMP_REDUCTION_NEQV:
	    fprintf (dumpfile, "%s:",
		     gfc_op2string ((gfc_intrinsic_op) n->u.reduction_op));
	    break;
	  case OMP_REDUCTION_MAX: fputs ("max:", dumpfile); break;
	  case OMP_REDUCTION_MIN: fputs ("min:", dumpfile); break;
	  case OMP_REDUCTION_IAND: fputs ("iand:", dumpfile); break;
	  case OMP_REDUCTION_IOR: fputs ("ior:", dumpfile); break;
	  case OMP_REDUCTION_IEOR: fputs ("ieor:", dumpfile); break;
	  case OMP_REDUCTION_USER:
	    if (n->u2.udr)
	      fprintf (dumpfile, "%s:", n->u2.udr->udr->name);
	    break;
	  default: break;
	  }
      else if (list_type == OMP_LIST_DEPEND)
	switch (n->u.depend_doacross_op)
	  {
	  case OMP_DEPEND_IN: fputs ("in:", dumpfile); break;
	  case OMP_DEPEND_OUT: fputs ("out:", dumpfile); break;
	  case OMP_DEPEND_INOUT: fputs ("inout:", dumpfile); break;
	  case OMP_DEPEND_INOUTSET: fputs ("inoutset:", dumpfile); break;
	  case OMP_DEPEND_DEPOBJ: fputs ("depobj:", dumpfile); break;
	  case OMP_DEPEND_MUTEXINOUTSET:
	    fputs ("mutexinoutset:", dumpfile);
	    break;
	  case OMP_DEPEND_SINK_FIRST:
	  case OMP_DOACROSS_SINK_FIRST:
	    fputs ("sink:", dumpfile);
	    while (1)
	      {
		if (!n->sym)
		  fputs ("omp_cur_iteration", dumpfile);
		else
		  fprintf (dumpfile, "%s", n->sym->name);
		if (n->expr)
		  {
		    fputc ('+', dumpfile);
		    show_expr (n->expr);
		  }
		if (n->next == NULL)
		  break;
		else if (n->next->u.depend_doacross_op != OMP_DOACROSS_SINK)
		  {
		    if (n->next->u.depend_doacross_op
			== OMP_DOACROSS_SINK_FIRST)
		      fputs (") DOACROSS(", dumpfile);
		    else
		      fputs (") DEPEND(", dumpfile);
		    break;
		  }
		fputc (',', dumpfile);
		n = n->next;
	      }
	    continue;
	  default: break;
	  }
      else if (list_type == OMP_LIST_MAP)
	switch (n->u.map.op)
	  {
	  case OMP_MAP_ALLOC: fputs ("alloc:", dumpfile); break;
	  case OMP_MAP_TO: fputs ("to:", dumpfile); break;
	  case OMP_MAP_FROM: fputs ("from:", dumpfile); break;
	  case OMP_MAP_TOFROM: fputs ("tofrom:", dumpfile); break;
	  case OMP_MAP_PRESENT_ALLOC: fputs ("present,alloc:", dumpfile); break;
	  case OMP_MAP_PRESENT_TO: fputs ("present,to:", dumpfile); break;
	  case OMP_MAP_PRESENT_FROM: fputs ("present,from:", dumpfile); break;
	  case OMP_MAP_PRESENT_TOFROM:
	    fputs ("present,tofrom:", dumpfile); break;
	  case OMP_MAP_ALWAYS_TO: fputs ("always,to:", dumpfile); break;
	  case OMP_MAP_ALWAYS_FROM: fputs ("always,from:", dumpfile); break;
	  case OMP_MAP_ALWAYS_TOFROM: fputs ("always,tofrom:", dumpfile); break;
	  case OMP_MAP_ALWAYS_PRESENT_TO:
	    fputs ("always,present,to:", dumpfile); break;
	  case OMP_MAP_ALWAYS_PRESENT_FROM:
	    fputs ("always,present,from:", dumpfile); break;
	  case OMP_MAP_ALWAYS_PRESENT_TOFROM:
	    fputs ("always,present,tofrom:", dumpfile); break;
	  case OMP_MAP_DELETE: fputs ("delete:", dumpfile); break;
	  case OMP_MAP_RELEASE: fputs ("release:", dumpfile); break;
	  default: break;
	  }
      else if (list_type == OMP_LIST_LINEAR && n->u.linear.old_modifier)
	switch (n->u.linear.op)
	  {
	  case OMP_LINEAR_REF: fputs ("ref(", dumpfile); break;
	  case OMP_LINEAR_VAL: fputs ("val(", dumpfile); break;
	  case OMP_LINEAR_UVAL: fputs ("uval(", dumpfile); break;
	  default: break;
	  }
      else if (list_type == OMP_LIST_USES_ALLOCATORS)
	{
	  if (n->u.memspace_sym)
	    {
	      fputs ("memspace(", dumpfile);
	      fputs (n->sym->name, dumpfile);
	      fputc (')', dumpfile);
	    }
	  if (n->u.memspace_sym && n->u2.traits_sym)
	    fputc (',', dumpfile);
	  if (n->u2.traits_sym)
	    {
	      fputs ("traits(", dumpfile);
	      fputs (n->u2.traits_sym->name, dumpfile);
	      fputc (')', dumpfile);
	    }
	  if (n->u.memspace_sym || n->u2.traits_sym)
	    fputc (':', dumpfile);
	  fputs (n->sym->name, dumpfile);
	  if (n->next)
	    fputs (", ", dumpfile);
	  continue;
	}
      else if (list_type == OMP_LIST_INIT)
	{
	  if (n->u.init.target)
	    fputs ("target,", dumpfile);
	  if (n->u.init.targetsync)
	    fputs ("targetsync,", dumpfile);
	  if (n->u2.init_interop)
	    {
	      char *str = n->u2.init_interop;
	      fputs ("prefer_type(", dumpfile);
	      while (str[0] == (char) GOMP_INTEROP_IFR_SEPARATOR)
		{
		  bool has_fr = false;
		  fputc ('{', dumpfile);
		  str++;
		  while (str[0] != (char) GOMP_INTEROP_IFR_SEPARATOR)
		    {
		      if (has_fr)
			fputc (',', dumpfile);
		      has_fr = true;
		      fputs ("fr(\"", dumpfile);
		      fputs (omp_get_name_from_fr_id (str[0]), dumpfile);
		      fputs ("\")", dumpfile);
		      str++;
		    }
		  str++;
		  if (has_fr && str[0] != '\0')
		    fputc (',', dumpfile);
		  while (str[0] != '\0')
		    {
		      fputs ("attr(\"", dumpfile);
		      fputs (str, dumpfile);
		      fputs ("\")", dumpfile);
		      str += strlen (str) + 1;
		      if (str[0] != '\0')
			fputc (',', dumpfile);
		    }
		  str++;
		  fputc ('}', dumpfile);
		  if (str[0] != '\0')
		    fputs (", ", dumpfile);
		}
	      fputc (')', dumpfile);
	    }
	  fputc (':', dumpfile);
	}
      fprintf (dumpfile, "%s", n->sym ? n->sym->name : "omp_all_memory");
      if (list_type == OMP_LIST_LINEAR && n->u.linear.op != OMP_LINEAR_DEFAULT)
	fputc (')', dumpfile);
      if (n->expr)
	{
	  fputc (':', dumpfile);
	  show_expr (n->expr);
	}
      if (n->next)
	fputc (',', dumpfile);
    }
  gfc_current_ns = ns_curr;
}

static void
show_omp_assumes (gfc_omp_assumptions *assume)
{
  for (int i = 0; i < assume->n_absent; i++)
    {
      fputs (" ABSENT (", dumpfile);
      fputs (gfc_ascii_statement (assume->absent[i], true), dumpfile);
      fputc (')', dumpfile);
    }
  for (int i = 0; i < assume->n_contains; i++)
    {
      fputs (" CONTAINS (", dumpfile);
      fputs (gfc_ascii_statement (assume->contains[i], true), dumpfile);
      fputc (')', dumpfile);
    }
  for (gfc_expr_list *el = assume->holds; el; el = el->next)
    {
      fputs (" HOLDS (", dumpfile);
      show_expr (el->expr);
      fputc (')', dumpfile);
    }
  if (assume->no_openmp)
    fputs (" NO_OPENMP", dumpfile);
  if (assume->no_openmp_routines)
    fputs (" NO_OPENMP_ROUTINES", dumpfile);
  if (assume->no_parallelism)
    fputs (" NO_PARALLELISM", dumpfile);
}

/* Show OpenMP or OpenACC clauses.  */

static void
show_omp_clauses (gfc_omp_clauses *omp_clauses)
{
  int list_type, i;

  switch (omp_clauses->cancel)
    {
    case OMP_CANCEL_UNKNOWN:
      break;
    case OMP_CANCEL_PARALLEL:
      fputs (" PARALLEL", dumpfile);
      break;
    case OMP_CANCEL_SECTIONS:
      fputs (" SECTIONS", dumpfile);
      break;
    case OMP_CANCEL_DO:
      fputs (" DO", dumpfile);
      break;
    case OMP_CANCEL_TASKGROUP:
      fputs (" TASKGROUP", dumpfile);
      break;
    }
  if (omp_clauses->if_expr)
    {
      fputs (" IF(", dumpfile);
      show_expr (omp_clauses->if_expr);
      fputc (')', dumpfile);
    }
  for (i = 0; i < OMP_IF_LAST; i++)
    if (omp_clauses->if_exprs[i])
      {
	static const char *ifs[] = {
	  "CANCEL",
	  "PARALLEL",
	  "SIMD",
	  "TASK",
	  "TASKLOOP",
	  "TARGET",
	  "TARGET DATA",
	  "TARGET UPDATE",
	  "TARGET ENTER DATA",
	  "TARGET EXIT DATA"
	};
      fputs (" IF(", dumpfile);
      fputs (ifs[i], dumpfile);
      fputs (": ", dumpfile);
      show_expr (omp_clauses->if_exprs[i]);
      fputc (')', dumpfile);
    }
  if (omp_clauses->self_expr)
    {
      fputs (" SELF(", dumpfile);
      show_expr (omp_clauses->self_expr);
      fputc (')', dumpfile);
    }
  if (omp_clauses->final_expr)
    {
      fputs (" FINAL(", dumpfile);
      show_expr (omp_clauses->final_expr);
      fputc (')', dumpfile);
    }
  if (omp_clauses->num_threads)
    {
      fputs (" NUM_THREADS(", dumpfile);
      show_expr (omp_clauses->num_threads);
      fputc (')', dumpfile);
    }
  if (omp_clauses->async)
    {
      fputs (" ASYNC", dumpfile);
      if (omp_clauses->async_expr)
	{
	  fputc ('(', dumpfile);
	  show_expr (omp_clauses->async_expr);
	  fputc (')', dumpfile);
	}
    }
  if (omp_clauses->num_gangs_expr)
    {
      fputs (" NUM_GANGS(", dumpfile);
      show_expr (omp_clauses->num_gangs_expr);
      fputc (')', dumpfile);
    }
  if (omp_clauses->num_workers_expr)
    {
      fputs (" NUM_WORKERS(", dumpfile);
      show_expr (omp_clauses->num_workers_expr);
      fputc (')', dumpfile);
    }
  if (omp_clauses->vector_length_expr)
    {
      fputs (" VECTOR_LENGTH(", dumpfile);
      show_expr (omp_clauses->vector_length_expr);
      fputc (')', dumpfile);
    }
  if (omp_clauses->gang)
    {
      fputs (" GANG", dumpfile);
      if (omp_clauses->gang_num_expr || omp_clauses->gang_static_expr)
	{
	  fputc ('(', dumpfile);
	  if (omp_clauses->gang_num_expr)
	    {
	      fprintf (dumpfile, "num:");
	      show_expr (omp_clauses->gang_num_expr);
	    }
	  if (omp_clauses->gang_num_expr && omp_clauses->gang_static)
	    fputc (',', dumpfile);
	  if (omp_clauses->gang_static)
	    {
	      fprintf (dumpfile, "static:");
	      if (omp_clauses->gang_static_expr)
		show_expr (omp_clauses->gang_static_expr);
	      else
		fputc ('*', dumpfile);
	    }
	  fputc (')', dumpfile);
	}
    }
  if (omp_clauses->worker)
    {
      fputs (" WORKER", dumpfile);
      if (omp_clauses->worker_expr)
	{
	  fputc ('(', dumpfile);
	  show_expr (omp_clauses->worker_expr);
	  fputc (')', dumpfile);
	}
    }
  if (omp_clauses->vector)
    {
      fputs (" VECTOR", dumpfile);
      if (omp_clauses->vector_expr)
	{
	  fputc ('(', dumpfile);
	  show_expr (omp_clauses->vector_expr);
	  fputc (')', dumpfile);
	}
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
	case OMP_SCHED_AUTO: type = "AUTO"; break;
	default:
	  gcc_unreachable ();
	}
      fputs (" SCHEDULE (", dumpfile);
      if (omp_clauses->sched_simd)
	{
	  if (omp_clauses->sched_monotonic
	      || omp_clauses->sched_nonmonotonic)
	    fputs ("SIMD, ", dumpfile);
	  else
	    fputs ("SIMD: ", dumpfile);
	}
      if (omp_clauses->sched_monotonic)
	fputs ("MONOTONIC: ", dumpfile);
      else if (omp_clauses->sched_nonmonotonic)
	fputs ("NONMONOTONIC: ", dumpfile);
      fputs (type, dumpfile);
      if (omp_clauses->chunk_size)
	{
	  fputc (',', dumpfile);
	  show_expr (omp_clauses->chunk_size);
	}
      fputc (')', dumpfile);
    }
  if (omp_clauses->default_sharing != OMP_DEFAULT_UNKNOWN)
    {
      const char *type;
      switch (omp_clauses->default_sharing)
	{
	case OMP_DEFAULT_NONE: type = "NONE"; break;
	case OMP_DEFAULT_PRIVATE: type = "PRIVATE"; break;
	case OMP_DEFAULT_SHARED: type = "SHARED"; break;
	case OMP_DEFAULT_FIRSTPRIVATE: type = "FIRSTPRIVATE"; break;
	case OMP_DEFAULT_PRESENT: type = "PRESENT"; break;
	default:
	  gcc_unreachable ();
	}
      fprintf (dumpfile, " DEFAULT(%s)", type);
    }
  if (omp_clauses->tile_list)
    {
      gfc_expr_list *list;
      fputs (" TILE(", dumpfile);
      for (list = omp_clauses->tile_list; list; list = list->next)
	{
	  show_expr (list->expr);
	  if (list->next)
	    fputs (", ", dumpfile);
	}
      fputc (')', dumpfile);
    }
  if (omp_clauses->wait_list)
    {
      gfc_expr_list *list;
      fputs (" WAIT(", dumpfile);
      for (list = omp_clauses->wait_list; list; list = list->next)
	{
	  show_expr (list->expr);
	  if (list->next)
	    fputs (", ", dumpfile);
	}
      fputc (')', dumpfile);
    }
  if (omp_clauses->seq)
    fputs (" SEQ", dumpfile);
  if (omp_clauses->independent)
    fputs (" INDEPENDENT", dumpfile);
  if (omp_clauses->order_concurrent)
    {
      fputs (" ORDER(", dumpfile);
      if (omp_clauses->order_unconstrained)
	fputs ("UNCONSTRAINED:", dumpfile);
      else if (omp_clauses->order_reproducible)
	fputs ("REPRODUCIBLE:", dumpfile);
      fputs ("CONCURRENT)", dumpfile);
    }
  if (omp_clauses->ordered)
    {
      if (omp_clauses->orderedc)
	fprintf (dumpfile, " ORDERED(%d)", omp_clauses->orderedc);
      else
	fputs (" ORDERED", dumpfile);
    }
  if (omp_clauses->untied)
    fputs (" UNTIED", dumpfile);
  if (omp_clauses->mergeable)
    fputs (" MERGEABLE", dumpfile);
  if (omp_clauses->nowait)
    fputs (" NOWAIT", dumpfile);
  if (omp_clauses->collapse)
    fprintf (dumpfile, " COLLAPSE(%d)", omp_clauses->collapse);
  for (list_type = 0; list_type < OMP_LIST_NUM; list_type++)
    if (omp_clauses->lists[list_type] != NULL)
      {
	const char *type = NULL;
	switch (list_type)
	  {
	  case OMP_LIST_PRIVATE: type = "PRIVATE"; break;
	  case OMP_LIST_FIRSTPRIVATE: type = "FIRSTPRIVATE"; break;
	  case OMP_LIST_LASTPRIVATE: type = "LASTPRIVATE"; break;
	  case OMP_LIST_COPYPRIVATE: type = "COPYPRIVATE"; break;
	  case OMP_LIST_SHARED: type = "SHARED"; break;
	  case OMP_LIST_COPYIN: type = "COPYIN"; break;
	  case OMP_LIST_UNIFORM: type = "UNIFORM"; break;
	  case OMP_LIST_AFFINITY: type = "AFFINITY"; break;
	  case OMP_LIST_ALIGNED: type = "ALIGNED"; break;
	  case OMP_LIST_LINEAR: type = "LINEAR"; break;
	  case OMP_LIST_DEPEND:
	    if (omp_clauses->lists[list_type]
		&& (omp_clauses->lists[list_type]->u.depend_doacross_op
		    == OMP_DOACROSS_SINK_FIRST))
	      type = "DOACROSS";
	    else
	      type = "DEPEND";
	    break;
	  case OMP_LIST_MAP: type = "MAP"; break;
	  case OMP_LIST_TO: type = "TO"; break;
	  case OMP_LIST_FROM: type = "FROM"; break;
	  case OMP_LIST_REDUCTION:
	  case OMP_LIST_REDUCTION_INSCAN:
	  case OMP_LIST_REDUCTION_TASK: type = "REDUCTION"; break;
	  case OMP_LIST_IN_REDUCTION: type = "IN_REDUCTION"; break;
	  case OMP_LIST_TASK_REDUCTION: type = "TASK_REDUCTION"; break;
	  case OMP_LIST_DEVICE_RESIDENT: type = "DEVICE_RESIDENT"; break;
	  case OMP_LIST_ENTER: type = "ENTER"; break;
	  case OMP_LIST_LINK: type = "LINK"; break;
	  case OMP_LIST_USE_DEVICE: type = "USE_DEVICE"; break;
	  case OMP_LIST_CACHE: type = "CACHE"; break;
	  case OMP_LIST_IS_DEVICE_PTR: type = "IS_DEVICE_PTR"; break;
	  case OMP_LIST_USE_DEVICE_PTR: type = "USE_DEVICE_PTR"; break;
	  case OMP_LIST_HAS_DEVICE_ADDR: type = "HAS_DEVICE_ADDR"; break;
	  case OMP_LIST_USE_DEVICE_ADDR: type = "USE_DEVICE_ADDR"; break;
	  case OMP_LIST_NONTEMPORAL: type = "NONTEMPORAL"; break;
	  case OMP_LIST_ALLOCATE: type = "ALLOCATE"; break;
	  case OMP_LIST_SCAN_IN: type = "INCLUSIVE"; break;
	  case OMP_LIST_SCAN_EX: type = "EXCLUSIVE"; break;
	  case OMP_LIST_USES_ALLOCATORS: type = "USES_ALLOCATORS"; break;
	  case OMP_LIST_INIT: type = "INIT"; break;
	  case OMP_LIST_USE: type = "USE"; break;
	  case OMP_LIST_DESTROY: type = "DESTROY"; break;
	  default:
	    gcc_unreachable ();
	  }
	fprintf (dumpfile, " %s(", type);
	if (list_type == OMP_LIST_REDUCTION_INSCAN)
	  fputs ("inscan, ", dumpfile);
	if (list_type == OMP_LIST_REDUCTION_TASK)
	  fputs ("task, ", dumpfile);
	if ((list_type == OMP_LIST_TO || list_type == OMP_LIST_FROM)
	    && omp_clauses->lists[list_type]->u.present_modifier)
	  fputs ("present:", dumpfile);
	show_omp_namelist (list_type, omp_clauses->lists[list_type]);
	fputc (')', dumpfile);
      }
  if (omp_clauses->safelen_expr)
    {
      fputs (" SAFELEN(", dumpfile);
      show_expr (omp_clauses->safelen_expr);
      fputc (')', dumpfile);
    }
  if (omp_clauses->simdlen_expr)
    {
      fputs (" SIMDLEN(", dumpfile);
      show_expr (omp_clauses->simdlen_expr);
      fputc (')', dumpfile);
    }
  if (omp_clauses->inbranch)
    fputs (" INBRANCH", dumpfile);
  if (omp_clauses->notinbranch)
    fputs (" NOTINBRANCH", dumpfile);
  if (omp_clauses->proc_bind != OMP_PROC_BIND_UNKNOWN)
    {
      const char *type;
      switch (omp_clauses->proc_bind)
	{
	case OMP_PROC_BIND_PRIMARY: type = "PRIMARY"; break;
	case OMP_PROC_BIND_MASTER: type = "MASTER"; break;
	case OMP_PROC_BIND_SPREAD: type = "SPREAD"; break;
	case OMP_PROC_BIND_CLOSE: type = "CLOSE"; break;
	default:
	  gcc_unreachable ();
	}
      fprintf (dumpfile, " PROC_BIND(%s)", type);
    }
  if (omp_clauses->bind != OMP_BIND_UNSET)
    {
      const char *type;
      switch (omp_clauses->bind)
	{
	case OMP_BIND_TEAMS: type = "TEAMS"; break;
	case OMP_BIND_PARALLEL: type = "PARALLEL"; break;
	case OMP_BIND_THREAD: type = "THREAD"; break;
	default:
	  gcc_unreachable ();
	}
      fprintf (dumpfile, " BIND(%s)", type);
    }
  if (omp_clauses->num_teams_upper)
    {
      fputs (" NUM_TEAMS(", dumpfile);
      if (omp_clauses->num_teams_lower)
	{
	  show_expr (omp_clauses->num_teams_lower);
	  fputc (':', dumpfile);
	}
      show_expr (omp_clauses->num_teams_upper);
      fputc (')', dumpfile);
    }
  if (omp_clauses->device)
    {
      fputs (" DEVICE(", dumpfile);
      if (omp_clauses->ancestor)
	fputs ("ANCESTOR:", dumpfile);
      show_expr (omp_clauses->device);
      fputc (')', dumpfile);
    }
  if (omp_clauses->thread_limit)
    {
      fputs (" THREAD_LIMIT(", dumpfile);
      show_expr (omp_clauses->thread_limit);
      fputc (')', dumpfile);
    }
  if (omp_clauses->dist_sched_kind != OMP_SCHED_NONE)
    {
      fputs (" DIST_SCHEDULE (STATIC", dumpfile);
      if (omp_clauses->dist_chunk_size)
	{
	  fputc (',', dumpfile);
	  show_expr (omp_clauses->dist_chunk_size);
	}
      fputc (')', dumpfile);
    }
  for (int i = 0; i < OMP_DEFAULTMAP_CAT_NUM; i++)
    {
      const char *dfltmap;
      if (omp_clauses->defaultmap[i] == OMP_DEFAULTMAP_UNSET)
	continue;
      fputs (" DEFAULTMAP (", dumpfile);
      switch (omp_clauses->defaultmap[i])
	{
	case OMP_DEFAULTMAP_ALLOC: dfltmap = "ALLOC"; break;
	case OMP_DEFAULTMAP_TO: dfltmap = "TO"; break;
	case OMP_DEFAULTMAP_FROM: dfltmap = "FROM"; break;
	case OMP_DEFAULTMAP_TOFROM: dfltmap = "TOFROM"; break;
	case OMP_DEFAULTMAP_FIRSTPRIVATE: dfltmap = "FIRSTPRIVATE"; break;
	case OMP_DEFAULTMAP_NONE: dfltmap = "NONE"; break;
	case OMP_DEFAULTMAP_DEFAULT: dfltmap = "DEFAULT"; break;
	case OMP_DEFAULTMAP_PRESENT: dfltmap = "PRESENT"; break;
	default: gcc_unreachable ();
	}
      fputs (dfltmap, dumpfile);
      if (i != OMP_DEFAULTMAP_CAT_UNCATEGORIZED)
	{
	  fputc (':', dumpfile);
	  switch ((enum gfc_omp_defaultmap_category) i)
	    {
	    case OMP_DEFAULTMAP_CAT_SCALAR: dfltmap = "SCALAR"; break;
	    case OMP_DEFAULTMAP_CAT_AGGREGATE: dfltmap = "AGGREGATE"; break;
	    case OMP_DEFAULTMAP_CAT_ALLOCATABLE: dfltmap = "ALLOCATABLE"; break;
	    case OMP_DEFAULTMAP_CAT_POINTER: dfltmap = "POINTER"; break;
	    default: gcc_unreachable ();
	    }
	  fputs (dfltmap, dumpfile);
	}
      fputc (')', dumpfile);
    }
  if (omp_clauses->weak)
    fputs (" WEAK", dumpfile);
  if (omp_clauses->compare)
    fputs (" COMPARE", dumpfile);
  if (omp_clauses->nogroup)
    fputs (" NOGROUP", dumpfile);
  if (omp_clauses->simd)
    fputs (" SIMD", dumpfile);
  if (omp_clauses->threads)
    fputs (" THREADS", dumpfile);
  if (omp_clauses->grainsize)
    {
      fputs (" GRAINSIZE(", dumpfile);
      if (omp_clauses->grainsize_strict)
	fputs ("strict: ", dumpfile);
      show_expr (omp_clauses->grainsize);
      fputc (')', dumpfile);
    }
  if (omp_clauses->filter)
    {
      fputs (" FILTER(", dumpfile);
      show_expr (omp_clauses->filter);
      fputc (')', dumpfile);
    }
  if (omp_clauses->hint)
    {
      fputs (" HINT(", dumpfile);
      show_expr (omp_clauses->hint);
      fputc (')', dumpfile);
    }
  if (omp_clauses->num_tasks)
    {
      fputs (" NUM_TASKS(", dumpfile);
      if (omp_clauses->num_tasks_strict)
	fputs ("strict: ", dumpfile);
      show_expr (omp_clauses->num_tasks);
      fputc (')', dumpfile);
    }
  if (omp_clauses->priority)
    {
      fputs (" PRIORITY(", dumpfile);
      show_expr (omp_clauses->priority);
      fputc (')', dumpfile);
    }
  if (omp_clauses->detach)
    {
      fputs (" DETACH(", dumpfile);
      show_expr (omp_clauses->detach);
      fputc (')', dumpfile);
    }
  if (omp_clauses->destroy)
    fputs (" DESTROY", dumpfile);
  if (omp_clauses->depend_source)
    fputs (" DEPEND(source)", dumpfile);
  if (omp_clauses->doacross_source)
    fputs (" DOACROSS(source:)", dumpfile);
  if (omp_clauses->capture)
    fputs (" CAPTURE", dumpfile);
  if (omp_clauses->depobj_update != OMP_DEPEND_UNSET)
    {
      const char *deptype;
      fputs (" UPDATE(", dumpfile);
      switch (omp_clauses->depobj_update)
	{
	case OMP_DEPEND_IN: deptype = "IN"; break;
	case OMP_DEPEND_OUT: deptype = "OUT"; break;
	case OMP_DEPEND_INOUT: deptype = "INOUT"; break;
	case OMP_DEPEND_INOUTSET: deptype = "INOUTSET"; break;
	case OMP_DEPEND_MUTEXINOUTSET: deptype = "MUTEXINOUTSET"; break;
	default: gcc_unreachable ();
	}
      fputs (deptype, dumpfile);
      fputc (')', dumpfile);
    }
  if (omp_clauses->atomic_op != GFC_OMP_ATOMIC_UNSET)
    {
      const char *atomic_op;
      switch (omp_clauses->atomic_op & GFC_OMP_ATOMIC_MASK)
	{
	case GFC_OMP_ATOMIC_READ: atomic_op = "READ"; break;
	case GFC_OMP_ATOMIC_WRITE: atomic_op = "WRITE"; break;
	case GFC_OMP_ATOMIC_UPDATE: atomic_op = "UPDATE"; break;
	default: gcc_unreachable ();
	}
      fputc (' ', dumpfile);
      fputs (atomic_op, dumpfile);
    }
  if (omp_clauses->memorder != OMP_MEMORDER_UNSET)
    {
      const char *memorder;
      switch (omp_clauses->memorder)
	{
	case OMP_MEMORDER_ACQ_REL: memorder = "ACQ_REL"; break;
	case OMP_MEMORDER_ACQUIRE: memorder = "AQUIRE"; break;
	case OMP_MEMORDER_RELAXED: memorder = "RELAXED"; break;
	case OMP_MEMORDER_RELEASE: memorder = "RELEASE"; break;
	case OMP_MEMORDER_SEQ_CST: memorder = "SEQ_CST"; break;
	default: gcc_unreachable ();
	}
      fputc (' ', dumpfile);
      fputs (memorder, dumpfile);
    }
  if (omp_clauses->fail != OMP_MEMORDER_UNSET)
    {
      const char *memorder;
      switch (omp_clauses->fail)
	{
	case OMP_MEMORDER_ACQUIRE: memorder = "AQUIRE"; break;
	case OMP_MEMORDER_RELAXED: memorder = "RELAXED"; break;
	case OMP_MEMORDER_SEQ_CST: memorder = "SEQ_CST"; break;
	default: gcc_unreachable ();
	}
      fputs (" FAIL(", dumpfile);
      fputs (memorder, dumpfile);
      putc (')', dumpfile);
    }
  if (omp_clauses->at != OMP_AT_UNSET)
    {
      if (omp_clauses->at != OMP_AT_COMPILATION)
	fputs (" AT (COMPILATION)", dumpfile);
      else
	fputs (" AT (EXECUTION)", dumpfile);
    }
  if (omp_clauses->severity != OMP_SEVERITY_UNSET)
    {
      if (omp_clauses->severity != OMP_SEVERITY_FATAL)
	fputs (" SEVERITY (FATAL)", dumpfile);
      else
	fputs (" SEVERITY (WARNING)", dumpfile);
    }
  if (omp_clauses->message)
    {
      fputs (" ERROR (", dumpfile);
      show_expr (omp_clauses->message);
      fputc (')', dumpfile);
    }
  if (omp_clauses->assume)
    show_omp_assumes (omp_clauses->assume);
  if (omp_clauses->full)
    fputs (" FULL", dumpfile);
  if (omp_clauses->partial)
    {
      fputs (" PARTIAL", dumpfile);
      if (omp_clauses->partial > 0)
	fprintf (dumpfile, "(%d)", omp_clauses->partial);
    }
  if (omp_clauses->sizes_list)
    {
      gfc_expr_list *sizes;
      fputs (" SIZES(", dumpfile);
      for (sizes = omp_clauses->sizes_list; sizes; sizes = sizes->next)
	{
	  show_expr (sizes->expr);
	  if (sizes->next)
	    fputs (", ", dumpfile);
	}
      fputc (')', dumpfile);
    }
  if (omp_clauses->novariants)
    {
      fputs (" NOVARIANTS(", dumpfile);
      show_expr (omp_clauses->novariants);
      fputc (')', dumpfile);
    }
  if (omp_clauses->nocontext)
    {
      fputs (" NOCONTEXT(", dumpfile);
      show_expr (omp_clauses->nocontext);
      fputc (')', dumpfile);
    }
}

/* Show a single OpenMP or OpenACC directive node and everything underneath it
   if necessary.  */

static void
show_omp_node (int level, gfc_code *c)
{
  gfc_omp_clauses *omp_clauses = NULL;
  const char *name = NULL;
  bool is_oacc = false;

  switch (c->op)
    {
    case EXEC_OACC_PARALLEL_LOOP:
      name = "PARALLEL LOOP"; is_oacc = true; break;
    case EXEC_OACC_PARALLEL: name = "PARALLEL"; is_oacc = true; break;
    case EXEC_OACC_KERNELS_LOOP: name = "KERNELS LOOP"; is_oacc = true; break;
    case EXEC_OACC_KERNELS: name = "KERNELS"; is_oacc = true; break;
    case EXEC_OACC_SERIAL_LOOP: name = "SERIAL LOOP"; is_oacc = true; break;
    case EXEC_OACC_SERIAL: name = "SERIAL"; is_oacc = true; break;
    case EXEC_OACC_DATA: name = "DATA"; is_oacc = true; break;
    case EXEC_OACC_HOST_DATA: name = "HOST_DATA"; is_oacc = true; break;
    case EXEC_OACC_LOOP: name = "LOOP"; is_oacc = true; break;
    case EXEC_OACC_UPDATE: name = "UPDATE"; is_oacc = true; break;
    case EXEC_OACC_WAIT: name = "WAIT"; is_oacc = true; break;
    case EXEC_OACC_CACHE: name = "CACHE"; is_oacc = true; break;
    case EXEC_OACC_ENTER_DATA: name = "ENTER DATA"; is_oacc = true; break;
    case EXEC_OACC_EXIT_DATA: name = "EXIT DATA"; is_oacc = true; break;
    case EXEC_OMP_ALLOCATE: name = "ALLOCATE"; break;
    case EXEC_OMP_ALLOCATORS: name = "ALLOCATORS"; break;
    case EXEC_OMP_ASSUME: name = "ASSUME"; break;
    case EXEC_OMP_ATOMIC: name = "ATOMIC"; break;
    case EXEC_OMP_BARRIER: name = "BARRIER"; break;
    case EXEC_OMP_CANCEL: name = "CANCEL"; break;
    case EXEC_OMP_CANCELLATION_POINT: name = "CANCELLATION POINT"; break;
    case EXEC_OMP_CRITICAL: name = "CRITICAL"; break;
    case EXEC_OMP_DISPATCH:
      name = "DISPATCH";
      break;
    case EXEC_OMP_DISTRIBUTE: name = "DISTRIBUTE"; break;
    case EXEC_OMP_DISTRIBUTE_PARALLEL_DO:
      name = "DISTRIBUTE PARALLEL DO"; break;
    case EXEC_OMP_DISTRIBUTE_PARALLEL_DO_SIMD:
      name = "DISTRIBUTE PARALLEL DO SIMD"; break;
    case EXEC_OMP_DISTRIBUTE_SIMD: name = "DISTRIBUTE SIMD"; break;
    case EXEC_OMP_DO: name = "DO"; break;
    case EXEC_OMP_DO_SIMD: name = "DO SIMD"; break;
    case EXEC_OMP_ERROR: name = "ERROR"; break;
    case EXEC_OMP_FLUSH: name = "FLUSH"; break;
    case EXEC_OMP_INTEROP: name = "INTEROP"; break;
    case EXEC_OMP_LOOP: name = "LOOP"; break;
    case EXEC_OMP_MASKED: name = "MASKED"; break;
    case EXEC_OMP_MASKED_TASKLOOP: name = "MASKED TASKLOOP"; break;
    case EXEC_OMP_MASKED_TASKLOOP_SIMD: name = "MASKED TASKLOOP SIMD"; break;
    case EXEC_OMP_MASTER: name = "MASTER"; break;
    case EXEC_OMP_MASTER_TASKLOOP: name = "MASTER TASKLOOP"; break;
    case EXEC_OMP_MASTER_TASKLOOP_SIMD: name = "MASTER TASKLOOP SIMD"; break;
    case EXEC_OMP_METADIRECTIVE: name = "METADIRECTIVE"; break;
    case EXEC_OMP_ORDERED: name = "ORDERED"; break;
    case EXEC_OMP_DEPOBJ: name = "DEPOBJ"; break;
    case EXEC_OMP_PARALLEL: name = "PARALLEL"; break;
    case EXEC_OMP_PARALLEL_DO: name = "PARALLEL DO"; break;
    case EXEC_OMP_PARALLEL_DO_SIMD: name = "PARALLEL DO SIMD"; break;
    case EXEC_OMP_PARALLEL_LOOP: name = "PARALLEL LOOP"; break;
    case EXEC_OMP_PARALLEL_MASTER: name = "PARALLEL MASTER"; break;
    case EXEC_OMP_PARALLEL_MASKED: name = "PARALLEL MASK"; break;
    case EXEC_OMP_PARALLEL_MASKED_TASKLOOP:
      name = "PARALLEL MASK TASKLOOP"; break;
    case EXEC_OMP_PARALLEL_MASKED_TASKLOOP_SIMD:
      name = "PARALLEL MASK TASKLOOP SIMD"; break;
    case EXEC_OMP_PARALLEL_MASTER_TASKLOOP:
      name = "PARALLEL MASTER TASKLOOP"; break;
    case EXEC_OMP_PARALLEL_MASTER_TASKLOOP_SIMD:
      name = "PARALLEL MASTER TASKLOOP SIMD"; break;
    case EXEC_OMP_PARALLEL_SECTIONS: name = "PARALLEL SECTIONS"; break;
    case EXEC_OMP_PARALLEL_WORKSHARE: name = "PARALLEL WORKSHARE"; break;
    case EXEC_OMP_SCAN: name = "SCAN"; break;
    case EXEC_OMP_SCOPE: name = "SCOPE"; break;
    case EXEC_OMP_SECTIONS: name = "SECTIONS"; break;
    case EXEC_OMP_SIMD: name = "SIMD"; break;
    case EXEC_OMP_SINGLE: name = "SINGLE"; break;
    case EXEC_OMP_TARGET: name = "TARGET"; break;
    case EXEC_OMP_TARGET_DATA: name = "TARGET DATA"; break;
    case EXEC_OMP_TARGET_ENTER_DATA: name = "TARGET ENTER DATA"; break;
    case EXEC_OMP_TARGET_EXIT_DATA: name = "TARGET EXIT DATA"; break;
    case EXEC_OMP_TARGET_PARALLEL: name = "TARGET PARALLEL"; break;
    case EXEC_OMP_TARGET_PARALLEL_DO: name = "TARGET PARALLEL DO"; break;
    case EXEC_OMP_TARGET_PARALLEL_DO_SIMD:
      name = "TARGET_PARALLEL_DO_SIMD"; break;
    case EXEC_OMP_TARGET_PARALLEL_LOOP: name = "TARGET PARALLEL LOOP"; break;
    case EXEC_OMP_TARGET_SIMD: name = "TARGET SIMD"; break;
    case EXEC_OMP_TARGET_TEAMS: name = "TARGET TEAMS"; break;
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE:
      name = "TARGET TEAMS DISTRIBUTE"; break;
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO:
      name = "TARGET TEAMS DISTRIBUTE PARALLEL DO"; break;
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
      name = "TARGET TEAMS DISTRIBUTE PARALLEL DO SIMD"; break;
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_SIMD:
      name = "TARGET TEAMS DISTRIBUTE SIMD"; break;
    case EXEC_OMP_TARGET_TEAMS_LOOP: name = "TARGET TEAMS LOOP"; break;
    case EXEC_OMP_TARGET_UPDATE: name = "TARGET UPDATE"; break;
    case EXEC_OMP_TASK: name = "TASK"; break;
    case EXEC_OMP_TASKGROUP: name = "TASKGROUP"; break;
    case EXEC_OMP_TASKLOOP: name = "TASKLOOP"; break;
    case EXEC_OMP_TASKLOOP_SIMD: name = "TASKLOOP SIMD"; break;
    case EXEC_OMP_TASKWAIT: name = "TASKWAIT"; break;
    case EXEC_OMP_TASKYIELD: name = "TASKYIELD"; break;
    case EXEC_OMP_TEAMS: name = "TEAMS"; break;
    case EXEC_OMP_TEAMS_DISTRIBUTE: name = "TEAMS DISTRIBUTE"; break;
    case EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO:
      name = "TEAMS DISTRIBUTE PARALLEL DO"; break;
    case EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
      name = "TEAMS DISTRIBUTE PARALLEL DO SIMD"; break;
    case EXEC_OMP_TEAMS_DISTRIBUTE_SIMD: name = "TEAMS DISTRIBUTE SIMD"; break;
    case EXEC_OMP_TEAMS_LOOP: name = "TEAMS LOOP"; break;
    case EXEC_OMP_TILE: name = "TILE"; break;
    case EXEC_OMP_UNROLL: name = "UNROLL"; break;
    case EXEC_OMP_WORKSHARE: name = "WORKSHARE"; break;
    default:
      gcc_unreachable ();
    }
  fprintf (dumpfile, "!$%s %s", is_oacc ? "ACC" : "OMP", name);
  switch (c->op)
    {
    case EXEC_OACC_PARALLEL_LOOP:
    case EXEC_OACC_PARALLEL:
    case EXEC_OACC_KERNELS_LOOP:
    case EXEC_OACC_KERNELS:
    case EXEC_OACC_SERIAL_LOOP:
    case EXEC_OACC_SERIAL:
    case EXEC_OACC_DATA:
    case EXEC_OACC_HOST_DATA:
    case EXEC_OACC_LOOP:
    case EXEC_OACC_UPDATE:
    case EXEC_OACC_WAIT:
    case EXEC_OACC_CACHE:
    case EXEC_OACC_ENTER_DATA:
    case EXEC_OACC_EXIT_DATA:
    case EXEC_OMP_ALLOCATE:
    case EXEC_OMP_ALLOCATORS:
    case EXEC_OMP_ASSUME:
    case EXEC_OMP_CANCEL:
    case EXEC_OMP_CANCELLATION_POINT:
    case EXEC_OMP_DISPATCH:
    case EXEC_OMP_DISTRIBUTE:
    case EXEC_OMP_DISTRIBUTE_PARALLEL_DO:
    case EXEC_OMP_DISTRIBUTE_PARALLEL_DO_SIMD:
    case EXEC_OMP_DISTRIBUTE_SIMD:
    case EXEC_OMP_DO:
    case EXEC_OMP_DO_SIMD:
    case EXEC_OMP_ERROR:
    case EXEC_OMP_INTEROP:
    case EXEC_OMP_LOOP:
    case EXEC_OMP_ORDERED:
    case EXEC_OMP_MASKED:
    case EXEC_OMP_PARALLEL:
    case EXEC_OMP_PARALLEL_DO:
    case EXEC_OMP_PARALLEL_DO_SIMD:
    case EXEC_OMP_PARALLEL_LOOP:
    case EXEC_OMP_PARALLEL_MASKED:
    case EXEC_OMP_PARALLEL_MASKED_TASKLOOP:
    case EXEC_OMP_PARALLEL_MASKED_TASKLOOP_SIMD:
    case EXEC_OMP_PARALLEL_MASTER:
    case EXEC_OMP_PARALLEL_MASTER_TASKLOOP:
    case EXEC_OMP_PARALLEL_MASTER_TASKLOOP_SIMD:
    case EXEC_OMP_PARALLEL_SECTIONS:
    case EXEC_OMP_PARALLEL_WORKSHARE:
    case EXEC_OMP_SCAN:
    case EXEC_OMP_SCOPE:
    case EXEC_OMP_SECTIONS:
    case EXEC_OMP_SIMD:
    case EXEC_OMP_SINGLE:
    case EXEC_OMP_TARGET:
    case EXEC_OMP_TARGET_DATA:
    case EXEC_OMP_TARGET_ENTER_DATA:
    case EXEC_OMP_TARGET_EXIT_DATA:
    case EXEC_OMP_TARGET_PARALLEL:
    case EXEC_OMP_TARGET_PARALLEL_DO:
    case EXEC_OMP_TARGET_PARALLEL_DO_SIMD:
    case EXEC_OMP_TARGET_PARALLEL_LOOP:
    case EXEC_OMP_TARGET_SIMD:
    case EXEC_OMP_TARGET_TEAMS:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_SIMD:
    case EXEC_OMP_TARGET_TEAMS_LOOP:
    case EXEC_OMP_TARGET_UPDATE:
    case EXEC_OMP_TASK:
    case EXEC_OMP_TASKLOOP:
    case EXEC_OMP_TASKLOOP_SIMD:
    case EXEC_OMP_TEAMS:
    case EXEC_OMP_TEAMS_DISTRIBUTE:
    case EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO:
    case EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
    case EXEC_OMP_TEAMS_DISTRIBUTE_SIMD:
    case EXEC_OMP_TEAMS_LOOP:
    case EXEC_OMP_TILE:
    case EXEC_OMP_UNROLL:
    case EXEC_OMP_WORKSHARE:
      omp_clauses = c->ext.omp_clauses;
      break;
    case EXEC_OMP_CRITICAL:
      omp_clauses = c->ext.omp_clauses;
      if (omp_clauses)
	fprintf (dumpfile, " (%s)", c->ext.omp_clauses->critical_name);
      break;
    case EXEC_OMP_DEPOBJ:
      omp_clauses = c->ext.omp_clauses;
      if (omp_clauses)
	{
	  fputc ('(', dumpfile);
	  show_expr (c->ext.omp_clauses->depobj);
	  fputc (')', dumpfile);
	}
      break;
    case EXEC_OMP_FLUSH:
      if (c->ext.omp_namelist)
	{
	  fputs (" (", dumpfile);
	  show_omp_namelist (OMP_LIST_NUM, c->ext.omp_namelist);
	  fputc (')', dumpfile);
	}
      return;
    case EXEC_OMP_BARRIER:
    case EXEC_OMP_TASKWAIT:
    case EXEC_OMP_TASKYIELD:
      return;
    case EXEC_OACC_ATOMIC:
    case EXEC_OMP_ATOMIC:
      omp_clauses = c->block ? c->block->ext.omp_clauses : NULL;
      break;
    default:
      break;
    }
  if (omp_clauses)
    show_omp_clauses (omp_clauses);
  fputc ('\n', dumpfile);

  /* OpenMP and OpenACC executable directives don't have associated blocks.  */
  if (c->op == EXEC_OACC_CACHE || c->op == EXEC_OACC_UPDATE
      || c->op == EXEC_OACC_ENTER_DATA || c->op == EXEC_OACC_EXIT_DATA
      || c->op == EXEC_OMP_TARGET_UPDATE || c->op == EXEC_OMP_TARGET_ENTER_DATA
      || c->op == EXEC_OMP_TARGET_EXIT_DATA || c->op == EXEC_OMP_SCAN
      || c->op == EXEC_OMP_DEPOBJ || c->op == EXEC_OMP_ERROR
      || c->op == EXEC_OMP_INTEROP
      || (c->op == EXEC_OMP_ORDERED && c->block == NULL))
    return;
  if (c->op == EXEC_OMP_SECTIONS || c->op == EXEC_OMP_PARALLEL_SECTIONS)
    {
      gfc_code *d = c->block;
      while (d != NULL)
	{
	  show_code (level + 1, d->next);
	  if (d->block == NULL)
	    break;
	  code_indent (level, 0);
	  fputs ("!$OMP SECTION\n", dumpfile);
	  d = d->block;
	}
    }
  else if (c->op == EXEC_OMP_METADIRECTIVE)
    {
      gfc_omp_variant *variant = c->ext.omp_variants;

      while (variant)
	{
	  code_indent (level + 1, 0);
	  if (variant->selectors)
	    fputs ("WHEN ()\n", dumpfile);
	  else
	    fputs ("DEFAULT ()\n", dumpfile);
	  /* TODO: Print selector.  */
	  show_code (level + 2, variant->code);
	  if (variant->next)
	    fputs ("\n", dumpfile);
	  variant = variant->next;
	}
    }
  else
    show_code (level + 1, c->block->next);
  if (c->op == EXEC_OMP_ATOMIC)
    return;
  fputc ('\n', dumpfile);
  code_indent (level, 0);
  fprintf (dumpfile, "!$%s END %s", is_oacc ? "ACC" : "OMP", name);
  if (c->op == EXEC_OMP_CRITICAL && c->ext.omp_clauses)
    fprintf (dumpfile, " (%s)", c->ext.omp_clauses->critical_name);
}


/* Show a single code node and everything underneath it if necessary.  */

static void
show_code_node (int level, gfc_code *c)
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
  gfc_namespace *ns;

  if (c->here)
    {
      fputc ('\n', dumpfile);
      code_indent (level, c->here);
    }
  else
    show_indent ();

  switch (c->op)
    {
    case EXEC_END_PROCEDURE:
      break;

    case EXEC_NOP:
      fputs ("NOP", dumpfile);
      break;

    case EXEC_CONTINUE:
      fputs ("CONTINUE", dumpfile);
      break;

    case EXEC_ENTRY:
      fprintf (dumpfile, "ENTRY %s", c->ext.entry->sym->name);
      break;

    case EXEC_INIT_ASSIGN:
    case EXEC_ASSIGN:
      fputs ("ASSIGN ", dumpfile);
      show_expr (c->expr1);
      fputc (' ', dumpfile);
      show_expr (c->expr2);
      break;

    case EXEC_LABEL_ASSIGN:
      fputs ("LABEL ASSIGN ", dumpfile);
      show_expr (c->expr1);
      fprintf (dumpfile, " %d", c->label1->value);
      break;

    case EXEC_POINTER_ASSIGN:
      fputs ("POINTER ASSIGN ", dumpfile);
      show_expr (c->expr1);
      fputc (' ', dumpfile);
      show_expr (c->expr2);
      break;

    case EXEC_GOTO:
      fputs ("GOTO ", dumpfile);
      if (c->label1)
	fprintf (dumpfile, "%d", c->label1->value);
      else
	{
	  show_expr (c->expr1);
	  d = c->block;
	  if (d != NULL)
	    {
	      fputs (", (", dumpfile);
	      for (; d; d = d ->block)
		{
		  code_indent (level, d->label1);
		  if (d->block != NULL)
		    fputc (',', dumpfile);
		  else
		    fputc (')', dumpfile);
		}
	    }
	}
      break;

    case EXEC_CALL:
    case EXEC_ASSIGN_CALL:
      if (c->resolved_sym)
	fprintf (dumpfile, "CALL %s ", c->resolved_sym->name);
      else if (c->symtree)
	fprintf (dumpfile, "CALL %s ", c->symtree->name);
      else
	fputs ("CALL ?? ", dumpfile);

      show_actual_arglist (c->ext.actual);
      break;

    case EXEC_COMPCALL:
      fputs ("CALL ", dumpfile);
      show_compcall (c->expr1);
      break;

    case EXEC_CALL_PPC:
      fputs ("CALL ", dumpfile);
      show_expr (c->expr1);
      show_actual_arglist (c->ext.actual);
      break;

    case EXEC_RETURN:
      fputs ("RETURN ", dumpfile);
      if (c->expr1)
	show_expr (c->expr1);
      break;

    case EXEC_PAUSE:
      fputs ("PAUSE ", dumpfile);

      if (c->expr1 != NULL)
	show_expr (c->expr1);
      else
	fprintf (dumpfile, "%d", c->ext.stop_code);

      break;

    case EXEC_ERROR_STOP:
      fputs ("ERROR ", dumpfile);
      /* Fall through.  */

    case EXEC_STOP:
      fputs ("STOP ", dumpfile);

      if (c->expr1 != NULL)
	show_expr (c->expr1);
      else
	fprintf (dumpfile, "%d", c->ext.stop_code);
      if (c->expr2 != NULL)
	{
	  fputs (" QUIET=", dumpfile);
	  show_expr (c->expr2);
	}

      break;

    case EXEC_FAIL_IMAGE:
      fputs ("FAIL IMAGE ", dumpfile);
      break;

    case EXEC_CHANGE_TEAM:
      fputs ("CHANGE TEAM", dumpfile);
      break;

    case EXEC_END_TEAM:
      fputs ("END TEAM", dumpfile);
      break;

    case EXEC_FORM_TEAM:
      fputs ("FORM TEAM", dumpfile);
      break;

    case EXEC_SYNC_TEAM:
      fputs ("SYNC TEAM", dumpfile);
      break;

    case EXEC_SYNC_ALL:
      fputs ("SYNC ALL ", dumpfile);
      if (c->expr2 != NULL)
	{
	  fputs (" stat=", dumpfile);
	  show_expr (c->expr2);
	}
      if (c->expr3 != NULL)
	{
	  fputs (" errmsg=", dumpfile);
	  show_expr (c->expr3);
	}
      break;

    case EXEC_SYNC_MEMORY:
      fputs ("SYNC MEMORY ", dumpfile);
      if (c->expr2 != NULL)
	{
	  fputs (" stat=", dumpfile);
	  show_expr (c->expr2);
	}
      if (c->expr3 != NULL)
	{
	  fputs (" errmsg=", dumpfile);
	  show_expr (c->expr3);
	}
      break;

    case EXEC_SYNC_IMAGES:
      fputs ("SYNC IMAGES  image-set=", dumpfile);
      if (c->expr1 != NULL)
	show_expr (c->expr1);
      else
	fputs ("* ", dumpfile);
      if (c->expr2 != NULL)
	{
	  fputs (" stat=", dumpfile);
	  show_expr (c->expr2);
	}
      if (c->expr3 != NULL)
	{
	  fputs (" errmsg=", dumpfile);
	  show_expr (c->expr3);
	}
      break;

    case EXEC_EVENT_POST:
    case EXEC_EVENT_WAIT:
      if (c->op == EXEC_EVENT_POST)
	fputs ("EVENT POST ", dumpfile);
      else
	fputs ("EVENT WAIT ", dumpfile);

      fputs ("event-variable=", dumpfile);
      if (c->expr1 != NULL)
	show_expr (c->expr1);
      if (c->expr4 != NULL)
	{
	  fputs (" until_count=", dumpfile);
	  show_expr (c->expr4);
	}
      if (c->expr2 != NULL)
	{
	  fputs (" stat=", dumpfile);
	  show_expr (c->expr2);
	}
      if (c->expr3 != NULL)
	{
	  fputs (" errmsg=", dumpfile);
	  show_expr (c->expr3);
	}
      break;

    case EXEC_LOCK:
    case EXEC_UNLOCK:
      if (c->op == EXEC_LOCK)
	fputs ("LOCK ", dumpfile);
      else
	fputs ("UNLOCK ", dumpfile);

      fputs ("lock-variable=", dumpfile);
      if (c->expr1 != NULL)
	show_expr (c->expr1);
      if (c->expr4 != NULL)
	{
	  fputs (" acquired_lock=", dumpfile);
	  show_expr (c->expr4);
	}
      if (c->expr2 != NULL)
	{
	  fputs (" stat=", dumpfile);
	  show_expr (c->expr2);
	}
      if (c->expr3 != NULL)
	{
	  fputs (" errmsg=", dumpfile);
	  show_expr (c->expr3);
	}
      break;

    case EXEC_ARITHMETIC_IF:
      fputs ("IF ", dumpfile);
      show_expr (c->expr1);
      fprintf (dumpfile, " %d, %d, %d",
		  c->label1->value, c->label2->value, c->label3->value);
      break;

    case EXEC_IF:
      d = c->block;
      fputs ("IF ", dumpfile);
      show_expr (d->expr1);

      ++show_level;
      show_code (level + 1, d->next);
      --show_level;

      d = d->block;
      for (; d; d = d->block)
	{
	  fputs("\n", dumpfile);
	  code_indent (level, 0);
	  if (d->expr1 == NULL)
	    fputs ("ELSE", dumpfile);
	  else
	    {
	      fputs ("ELSE IF ", dumpfile);
	      show_expr (d->expr1);
	    }

	  ++show_level;
	  show_code (level + 1, d->next);
	  --show_level;
	}

      if (c->label1)
	code_indent (level, c->label1);
      else
	show_indent ();

      fputs ("ENDIF", dumpfile);
      break;

    case EXEC_BLOCK:
      {
	const char *blocktype, *sname = NULL;
	gfc_namespace *saved_ns;
	gfc_association_list *alist;

	if (c->ext.block.ns && c->ext.block.ns->code
	    && c->ext.block.ns->code->op == EXEC_SELECT_TYPE)
	  {
	    gfc_expr *fcn = c->ext.block.ns->code->expr1;
	    blocktype = "SELECT TYPE";
	    /* expr1 is _loc(assoc_name->vptr)  */
	    if (fcn && fcn->expr_type == EXPR_FUNCTION)
	      sname = fcn->value.function.actual->expr->symtree->n.sym->name;
	  }
	else if (c->ext.block.assoc)
	  blocktype = "ASSOCIATE";
	else
	  blocktype = "BLOCK";
	show_indent ();
	fprintf (dumpfile, "%s ", blocktype);
	for (alist = c->ext.block.assoc; alist; alist = alist->next)
	  {
	    fprintf (dumpfile, " %s = ", sname ? sname : alist->name);
	    show_expr (alist->target);
	  }

	++show_level;
	ns = c->ext.block.ns;
	saved_ns = gfc_current_ns;
	gfc_current_ns = ns;
	gfc_traverse_symtree (ns->sym_root, show_symtree);
	gfc_current_ns = saved_ns;
	show_code (show_level, ns->code);
	--show_level;
	show_indent ();
	fprintf (dumpfile, "END %s ", blocktype);
	break;
      }

    case EXEC_END_BLOCK:
      /* Only come here when there is a label on an
	 END ASSOCIATE construct.  */
      break;

    case EXEC_SELECT:
    case EXEC_SELECT_TYPE:
    case EXEC_SELECT_RANK:
      d = c->block;
      fputc ('\n', dumpfile);
      code_indent (level, 0);
      if (c->op == EXEC_SELECT_RANK)
	fputs ("SELECT RANK ", dumpfile);
      else if (c->op == EXEC_SELECT_TYPE)
	fputs ("SELECT CASE ", dumpfile); // Preceded by SELECT TYPE construct
      else
	fputs ("SELECT CASE ", dumpfile);
      show_expr (c->expr1);

      for (; d; d = d->block)
	{
	  fputc ('\n', dumpfile);
	  code_indent (level, 0);
	  fputs ("CASE ", dumpfile);
	  for (cp = d->ext.block.case_list; cp; cp = cp->next)
	    {
	      fputc ('(', dumpfile);
	      show_expr (cp->low);
	      fputc (' ', dumpfile);
	      show_expr (cp->high);
	      fputc (')', dumpfile);
	      fputc (' ', dumpfile);
	    }

	  show_code (level + 1, d->next);
	  fputc ('\n', dumpfile);
	}

      code_indent (level, c->label1);
      fputs ("END SELECT", dumpfile);
      break;

    case EXEC_WHERE:
      fputs ("WHERE ", dumpfile);

      d = c->block;
      show_expr (d->expr1);
      fputc ('\n', dumpfile);

      show_code (level + 1, d->next);

      for (d = d->block; d; d = d->block)
	{
	  code_indent (level, 0);
	  fputs ("ELSE WHERE ", dumpfile);
	  show_expr (d->expr1);
	  fputc ('\n', dumpfile);
	  show_code (level + 1, d->next);
	}

      code_indent (level, 0);
      fputs ("END WHERE", dumpfile);
      break;


    case EXEC_FORALL:
      fputs ("FORALL ", dumpfile);
      for (fa = c->ext.concur.forall_iterator; fa; fa = fa->next)
	{
	  show_expr (fa->var);
	  fputc (' ', dumpfile);
	  show_expr (fa->start);
	  fputc (':', dumpfile);
	  show_expr (fa->end);
	  fputc (':', dumpfile);
	  show_expr (fa->stride);

	  if (fa->next != NULL)
	    fputc (',', dumpfile);
	}

      if (c->expr1 != NULL)
	{
	  fputc (',', dumpfile);
	  show_expr (c->expr1);
	}
      fputc ('\n', dumpfile);

      show_code (level + 1, c->block->next);

      code_indent (level, 0);
      fputs ("END FORALL", dumpfile);
      break;

    case EXEC_CRITICAL:
      fputs ("CRITICAL\n", dumpfile);
      show_code (level + 1, c->block->next);
      code_indent (level, 0);
      fputs ("END CRITICAL", dumpfile);
      break;

    case EXEC_DO:
      fputs ("DO ", dumpfile);
      if (c->label1)
	fprintf (dumpfile, " %-5d ", c->label1->value);

      show_expr (c->ext.iterator->var);
      fputc ('=', dumpfile);
      show_expr (c->ext.iterator->start);
      fputc (' ', dumpfile);
      show_expr (c->ext.iterator->end);
      fputc (' ', dumpfile);
      show_expr (c->ext.iterator->step);

      ++show_level;
      show_code (level + 1, c->block->next);
      --show_level;

      if (c->label1)
	break;

      show_indent ();
      fputs ("END DO", dumpfile);
      break;

    case EXEC_DO_CONCURRENT:
      fputs ("DO CONCURRENT ", dumpfile);
      for (fa = c->ext.concur.forall_iterator; fa; fa = fa->next)
        {
          show_expr (fa->var);
          fputc (' ', dumpfile);
          show_expr (fa->start);
          fputc (':', dumpfile);
          show_expr (fa->end);
          fputc (':', dumpfile);
          show_expr (fa->stride);

          if (fa->next != NULL)
            fputc (',', dumpfile);
        }

      if (c->expr1 != NULL)
	{
	  fputc (',', dumpfile);
	  show_expr (c->expr1);
	}

      if (c->ext.concur.locality[LOCALITY_LOCAL])
	{
	  fputs (" LOCAL (", dumpfile);

	  for (gfc_expr_list *el = c->ext.concur.locality[LOCALITY_LOCAL];
	       el; el = el->next)
	    {
	      show_expr (el->expr);
	      if (el->next)
		fputc (',', dumpfile);
	    }
	  fputc (')', dumpfile);
	}

      if (c->ext.concur.locality[LOCALITY_LOCAL_INIT])
	{
	  fputs (" LOCAL_INIT (", dumpfile);
	  for (gfc_expr_list *el = c->ext.concur.locality[LOCALITY_LOCAL_INIT];
	       el; el = el->next)
	  {
	    show_expr (el->expr);
	    if (el->next)
	      fputc (',', dumpfile);
	  }
	  fputc (')', dumpfile);
	}

      if (c->ext.concur.locality[LOCALITY_SHARED])
	{
	  fputs (" SHARED (", dumpfile);
	  for (gfc_expr_list *el = c->ext.concur.locality[LOCALITY_SHARED];
	       el; el = el->next)
	    {
	      show_expr (el->expr);
	      if (el->next)
		fputc (',', dumpfile);
	    }
	  fputc (')', dumpfile);
	}

      if (c->ext.concur.default_none)
	{
	  fputs (" DEFAULT (NONE)", dumpfile);
	}

      if (c->ext.concur.locality[LOCALITY_REDUCE])
	{
	  gfc_expr_list *el = c->ext.concur.locality[LOCALITY_REDUCE];
	  while (el)
	    {
	      fputs (" REDUCE (", dumpfile);
	      if (el->expr)
		{
		  if (el->expr->expr_type == EXPR_FUNCTION)
		    {
		      const char *name;
		      switch (el->expr->value.function.isym->id)
			{
			  case GFC_ISYM_MIN:
			    name = "MIN";
			    break;
			  case GFC_ISYM_MAX:
			    name = "MAX";
			    break;
			  case GFC_ISYM_IAND:
			    name = "IAND";
			    break;
			  case GFC_ISYM_IOR:
			    name = "IOR";
			    break;
			  case GFC_ISYM_IEOR:
			    name = "IEOR";
			    break;
			  default:
			    gcc_unreachable ();
			}
		      fputs (name, dumpfile);
		    }
		  else
		    show_expr (el->expr);
		}
	      else
		{
		  fputs ("(NULL)", dumpfile);
		}

	      fputc (':', dumpfile);
	      el = el->next;

	      while (el && el->expr && el->expr->expr_type == EXPR_VARIABLE)
		{
		  show_expr (el->expr);
		  el = el->next;
		  if (el && el->expr && el->expr->expr_type == EXPR_VARIABLE)
		    fputc (',', dumpfile);
		}

	      fputc (')', dumpfile);
	    }
	}

      ++show_level;

      show_code (level + 1, c->block->next);
      --show_level;
      code_indent (level, c->label1);
      show_indent ();
      fputs ("END DO", dumpfile);
      break;

    case EXEC_DO_WHILE:
      fputs ("DO WHILE ", dumpfile);
      show_expr (c->expr1);
      fputc ('\n', dumpfile);

      show_code (level + 1, c->block->next);

      code_indent (level, c->label1);
      fputs ("END DO", dumpfile);
      break;

    case EXEC_CYCLE:
      fputs ("CYCLE", dumpfile);
      if (c->symtree)
	fprintf (dumpfile, " %s", c->symtree->n.sym->name);
      break;

    case EXEC_EXIT:
      fputs ("EXIT", dumpfile);
      if (c->symtree)
	fprintf (dumpfile, " %s", c->symtree->n.sym->name);
      break;

    case EXEC_ALLOCATE:
      fputs ("ALLOCATE ", dumpfile);
      if (c->expr1)
	{
	  fputs (" STAT=", dumpfile);
	  show_expr (c->expr1);
	}

      if (c->expr2)
	{
	  fputs (" ERRMSG=", dumpfile);
	  show_expr (c->expr2);
	}

      if (c->expr3)
	{
	  if (c->expr3->mold)
	    fputs (" MOLD=", dumpfile);
	  else
	    fputs (" SOURCE=", dumpfile);
	  show_expr (c->expr3);
	}

      for (a = c->ext.alloc.list; a; a = a->next)
	{
	  fputc (' ', dumpfile);
	  show_expr (a->expr);
	}

      break;

    case EXEC_DEALLOCATE:
      fputs ("DEALLOCATE ", dumpfile);
      if (c->expr1)
	{
	  fputs (" STAT=", dumpfile);
	  show_expr (c->expr1);
	}

      if (c->expr2)
	{
	  fputs (" ERRMSG=", dumpfile);
	  show_expr (c->expr2);
	}

      for (a = c->ext.alloc.list; a; a = a->next)
	{
	  fputc (' ', dumpfile);
	  show_expr (a->expr);
	}

      break;

    case EXEC_OPEN:
      fputs ("OPEN", dumpfile);
      open = c->ext.open;

      if (open->unit)
	{
	  fputs (" UNIT=", dumpfile);
	  show_expr (open->unit);
	}
      if (open->iomsg)
	{
	  fputs (" IOMSG=", dumpfile);
	  show_expr (open->iomsg);
	}
      if (open->iostat)
	{
	  fputs (" IOSTAT=", dumpfile);
	  show_expr (open->iostat);
	}
      if (open->file)
	{
	  fputs (" FILE=", dumpfile);
	  show_expr (open->file);
	}
      if (open->status)
	{
	  fputs (" STATUS=", dumpfile);
	  show_expr (open->status);
	}
      if (open->access)
	{
	  fputs (" ACCESS=", dumpfile);
	  show_expr (open->access);
	}
      if (open->form)
	{
	  fputs (" FORM=", dumpfile);
	  show_expr (open->form);
	}
      if (open->recl)
	{
	  fputs (" RECL=", dumpfile);
	  show_expr (open->recl);
	}
      if (open->blank)
	{
	  fputs (" BLANK=", dumpfile);
	  show_expr (open->blank);
	}
      if (open->position)
	{
	  fputs (" POSITION=", dumpfile);
	  show_expr (open->position);
	}
      if (open->action)
	{
	  fputs (" ACTION=", dumpfile);
	  show_expr (open->action);
	}
      if (open->delim)
	{
	  fputs (" DELIM=", dumpfile);
	  show_expr (open->delim);
	}
      if (open->pad)
	{
	  fputs (" PAD=", dumpfile);
	  show_expr (open->pad);
	}
      if (open->decimal)
	{
	  fputs (" DECIMAL=", dumpfile);
	  show_expr (open->decimal);
	}
      if (open->encoding)
	{
	  fputs (" ENCODING=", dumpfile);
	  show_expr (open->encoding);
	}
      if (open->round)
	{
	  fputs (" ROUND=", dumpfile);
	  show_expr (open->round);
	}
      if (open->sign)
	{
	  fputs (" SIGN=", dumpfile);
	  show_expr (open->sign);
	}
      if (open->convert)
	{
	  fputs (" CONVERT=", dumpfile);
	  show_expr (open->convert);
	}
      if (open->asynchronous)
	{
	  fputs (" ASYNCHRONOUS=", dumpfile);
	  show_expr (open->asynchronous);
	}
      if (open->err != NULL)
	fprintf (dumpfile, " ERR=%d", open->err->value);

      break;

    case EXEC_CLOSE:
      fputs ("CLOSE", dumpfile);
      close = c->ext.close;

      if (close->unit)
	{
	  fputs (" UNIT=", dumpfile);
	  show_expr (close->unit);
	}
      if (close->iomsg)
	{
	  fputs (" IOMSG=", dumpfile);
	  show_expr (close->iomsg);
	}
      if (close->iostat)
	{
	  fputs (" IOSTAT=", dumpfile);
	  show_expr (close->iostat);
	}
      if (close->status)
	{
	  fputs (" STATUS=", dumpfile);
	  show_expr (close->status);
	}
      if (close->err != NULL)
	fprintf (dumpfile, " ERR=%d", close->err->value);
      break;

    case EXEC_BACKSPACE:
      fputs ("BACKSPACE", dumpfile);
      goto show_filepos;

    case EXEC_ENDFILE:
      fputs ("ENDFILE", dumpfile);
      goto show_filepos;

    case EXEC_REWIND:
      fputs ("REWIND", dumpfile);
      goto show_filepos;

    case EXEC_FLUSH:
      fputs ("FLUSH", dumpfile);

    show_filepos:
      fp = c->ext.filepos;

      if (fp->unit)
	{
	  fputs (" UNIT=", dumpfile);
	  show_expr (fp->unit);
	}
      if (fp->iomsg)
	{
	  fputs (" IOMSG=", dumpfile);
	  show_expr (fp->iomsg);
	}
      if (fp->iostat)
	{
	  fputs (" IOSTAT=", dumpfile);
	  show_expr (fp->iostat);
	}
      if (fp->err != NULL)
	fprintf (dumpfile, " ERR=%d", fp->err->value);
      break;

    case EXEC_INQUIRE:
      fputs ("INQUIRE", dumpfile);
      i = c->ext.inquire;

      if (i->unit)
	{
	  fputs (" UNIT=", dumpfile);
	  show_expr (i->unit);
	}
      if (i->file)
	{
	  fputs (" FILE=", dumpfile);
	  show_expr (i->file);
	}

      if (i->iomsg)
	{
	  fputs (" IOMSG=", dumpfile);
	  show_expr (i->iomsg);
	}
      if (i->iostat)
	{
	  fputs (" IOSTAT=", dumpfile);
	  show_expr (i->iostat);
	}
      if (i->exist)
	{
	  fputs (" EXIST=", dumpfile);
	  show_expr (i->exist);
	}
      if (i->opened)
	{
	  fputs (" OPENED=", dumpfile);
	  show_expr (i->opened);
	}
      if (i->number)
	{
	  fputs (" NUMBER=", dumpfile);
	  show_expr (i->number);
	}
      if (i->named)
	{
	  fputs (" NAMED=", dumpfile);
	  show_expr (i->named);
	}
      if (i->name)
	{
	  fputs (" NAME=", dumpfile);
	  show_expr (i->name);
	}
      if (i->access)
	{
	  fputs (" ACCESS=", dumpfile);
	  show_expr (i->access);
	}
      if (i->sequential)
	{
	  fputs (" SEQUENTIAL=", dumpfile);
	  show_expr (i->sequential);
	}

      if (i->direct)
	{
	  fputs (" DIRECT=", dumpfile);
	  show_expr (i->direct);
	}
      if (i->form)
	{
	  fputs (" FORM=", dumpfile);
	  show_expr (i->form);
	}
      if (i->formatted)
	{
	  fputs (" FORMATTED", dumpfile);
	  show_expr (i->formatted);
	}
      if (i->unformatted)
	{
	  fputs (" UNFORMATTED=", dumpfile);
	  show_expr (i->unformatted);
	}
      if (i->recl)
	{
	  fputs (" RECL=", dumpfile);
	  show_expr (i->recl);
	}
      if (i->nextrec)
	{
	  fputs (" NEXTREC=", dumpfile);
	  show_expr (i->nextrec);
	}
      if (i->blank)
	{
	  fputs (" BLANK=", dumpfile);
	  show_expr (i->blank);
	}
      if (i->position)
	{
	  fputs (" POSITION=", dumpfile);
	  show_expr (i->position);
	}
      if (i->action)
	{
	  fputs (" ACTION=", dumpfile);
	  show_expr (i->action);
	}
      if (i->read)
	{
	  fputs (" READ=", dumpfile);
	  show_expr (i->read);
	}
      if (i->write)
	{
	  fputs (" WRITE=", dumpfile);
	  show_expr (i->write);
	}
      if (i->readwrite)
	{
	  fputs (" READWRITE=", dumpfile);
	  show_expr (i->readwrite);
	}
      if (i->delim)
	{
	  fputs (" DELIM=", dumpfile);
	  show_expr (i->delim);
	}
      if (i->pad)
	{
	  fputs (" PAD=", dumpfile);
	  show_expr (i->pad);
	}
      if (i->convert)
	{
	  fputs (" CONVERT=", dumpfile);
	  show_expr (i->convert);
	}
      if (i->asynchronous)
	{
	  fputs (" ASYNCHRONOUS=", dumpfile);
	  show_expr (i->asynchronous);
	}
      if (i->decimal)
	{
	  fputs (" DECIMAL=", dumpfile);
	  show_expr (i->decimal);
	}
      if (i->encoding)
	{
	  fputs (" ENCODING=", dumpfile);
	  show_expr (i->encoding);
	}
      if (i->pending)
	{
	  fputs (" PENDING=", dumpfile);
	  show_expr (i->pending);
	}
      if (i->round)
	{
	  fputs (" ROUND=", dumpfile);
	  show_expr (i->round);
	}
      if (i->sign)
	{
	  fputs (" SIGN=", dumpfile);
	  show_expr (i->sign);
	}
      if (i->size)
	{
	  fputs (" SIZE=", dumpfile);
	  show_expr (i->size);
	}
      if (i->id)
	{
	  fputs (" ID=", dumpfile);
	  show_expr (i->id);
	}

      if (i->err != NULL)
	fprintf (dumpfile, " ERR=%d", i->err->value);
      break;

    case EXEC_IOLENGTH:
      fputs ("IOLENGTH ", dumpfile);
      show_expr (c->expr1);
      goto show_dt_code;
      break;

    case EXEC_READ:
      fputs ("READ", dumpfile);
      goto show_dt;

    case EXEC_WRITE:
      fputs ("WRITE", dumpfile);

    show_dt:
      dt = c->ext.dt;
      if (dt->io_unit)
	{
	  fputs (" UNIT=", dumpfile);
	  show_expr (dt->io_unit);
	}

      if (dt->format_expr)
	{
	  fputs (" FMT=", dumpfile);
	  show_expr (dt->format_expr);
	}

      if (dt->format_label != NULL)
	fprintf (dumpfile, " FMT=%d", dt->format_label->value);
      if (dt->namelist)
	fprintf (dumpfile, " NML=%s", dt->namelist->name);

      if (dt->iomsg)
	{
	  fputs (" IOMSG=", dumpfile);
	  show_expr (dt->iomsg);
	}
      if (dt->iostat)
	{
	  fputs (" IOSTAT=", dumpfile);
	  show_expr (dt->iostat);
	}
      if (dt->size)
	{
	  fputs (" SIZE=", dumpfile);
	  show_expr (dt->size);
	}
      if (dt->rec)
	{
	  fputs (" REC=", dumpfile);
	  show_expr (dt->rec);
	}
      if (dt->advance)
	{
	  fputs (" ADVANCE=", dumpfile);
	  show_expr (dt->advance);
	}
      if (dt->id)
	{
	  fputs (" ID=", dumpfile);
	  show_expr (dt->id);
	}
      if (dt->pos)
	{
	  fputs (" POS=", dumpfile);
	  show_expr (dt->pos);
	}
      if (dt->asynchronous)
	{
	  fputs (" ASYNCHRONOUS=", dumpfile);
	  show_expr (dt->asynchronous);
	}
      if (dt->blank)
	{
	  fputs (" BLANK=", dumpfile);
	  show_expr (dt->blank);
	}
      if (dt->decimal)
	{
	  fputs (" DECIMAL=", dumpfile);
	  show_expr (dt->decimal);
	}
      if (dt->delim)
	{
	  fputs (" DELIM=", dumpfile);
	  show_expr (dt->delim);
	}
      if (dt->pad)
	{
	  fputs (" PAD=", dumpfile);
	  show_expr (dt->pad);
	}
      if (dt->round)
	{
	  fputs (" ROUND=", dumpfile);
	  show_expr (dt->round);
	}
      if (dt->sign)
	{
	  fputs (" SIGN=", dumpfile);
	  show_expr (dt->sign);
	}

    show_dt_code:
      for (c = c->block->next; c; c = c->next)
	show_code_node (level + (c->next != NULL), c);
      return;

    case EXEC_TRANSFER:
      fputs ("TRANSFER ", dumpfile);
      show_expr (c->expr1);
      break;

    case EXEC_DT_END:
      fputs ("DT_END", dumpfile);
      dt = c->ext.dt;

      if (dt->err != NULL)
	fprintf (dumpfile, " ERR=%d", dt->err->value);
      if (dt->end != NULL)
	fprintf (dumpfile, " END=%d", dt->end->value);
      if (dt->eor != NULL)
	fprintf (dumpfile, " EOR=%d", dt->eor->value);
      break;

    case EXEC_WAIT:
      fputs ("WAIT", dumpfile);

      if (c->ext.wait != NULL)
	{
	  gfc_wait *wait = c->ext.wait;
	  if (wait->unit)
	    {
	      fputs (" UNIT=", dumpfile);
	      show_expr (wait->unit);
	    }
	  if (wait->iostat)
	    {
	      fputs (" IOSTAT=", dumpfile);
	      show_expr (wait->iostat);
	    }
	  if (wait->iomsg)
	    {
	      fputs (" IOMSG=", dumpfile);
	      show_expr (wait->iomsg);
	    }
	  if (wait->id)
	    {
	      fputs (" ID=", dumpfile);
	      show_expr (wait->id);
	    }
	  if (wait->err)
	    fprintf (dumpfile, " ERR=%d", wait->err->value);
	  if (wait->end)
	    fprintf (dumpfile, " END=%d", wait->end->value);
	  if (wait->eor)
	    fprintf (dumpfile, " EOR=%d", wait->eor->value);
	}
      break;

    case EXEC_OACC_PARALLEL_LOOP:
    case EXEC_OACC_PARALLEL:
    case EXEC_OACC_KERNELS_LOOP:
    case EXEC_OACC_KERNELS:
    case EXEC_OACC_SERIAL_LOOP:
    case EXEC_OACC_SERIAL:
    case EXEC_OACC_DATA:
    case EXEC_OACC_HOST_DATA:
    case EXEC_OACC_LOOP:
    case EXEC_OACC_UPDATE:
    case EXEC_OACC_WAIT:
    case EXEC_OACC_CACHE:
    case EXEC_OACC_ENTER_DATA:
    case EXEC_OACC_EXIT_DATA:
    case EXEC_OMP_ALLOCATE:
    case EXEC_OMP_ALLOCATORS:
    case EXEC_OMP_ASSUME:
    case EXEC_OMP_ATOMIC:
    case EXEC_OMP_CANCEL:
    case EXEC_OMP_CANCELLATION_POINT:
    case EXEC_OMP_BARRIER:
    case EXEC_OMP_CRITICAL:
    case EXEC_OMP_DEPOBJ:
    case EXEC_OMP_DISPATCH:
    case EXEC_OMP_DISTRIBUTE:
    case EXEC_OMP_DISTRIBUTE_PARALLEL_DO:
    case EXEC_OMP_DISTRIBUTE_PARALLEL_DO_SIMD:
    case EXEC_OMP_DISTRIBUTE_SIMD:
    case EXEC_OMP_DO:
    case EXEC_OMP_DO_SIMD:
    case EXEC_OMP_ERROR:
    case EXEC_OMP_INTEROP:
    case EXEC_OMP_FLUSH:
    case EXEC_OMP_LOOP:
    case EXEC_OMP_MASKED:
    case EXEC_OMP_MASKED_TASKLOOP:
    case EXEC_OMP_MASKED_TASKLOOP_SIMD:
    case EXEC_OMP_MASTER:
    case EXEC_OMP_MASTER_TASKLOOP:
    case EXEC_OMP_MASTER_TASKLOOP_SIMD:
    case EXEC_OMP_METADIRECTIVE:
    case EXEC_OMP_ORDERED:
    case EXEC_OMP_PARALLEL:
    case EXEC_OMP_PARALLEL_DO:
    case EXEC_OMP_PARALLEL_DO_SIMD:
    case EXEC_OMP_PARALLEL_LOOP:
    case EXEC_OMP_PARALLEL_MASKED:
    case EXEC_OMP_PARALLEL_MASKED_TASKLOOP:
    case EXEC_OMP_PARALLEL_MASKED_TASKLOOP_SIMD:
    case EXEC_OMP_PARALLEL_MASTER:
    case EXEC_OMP_PARALLEL_MASTER_TASKLOOP:
    case EXEC_OMP_PARALLEL_MASTER_TASKLOOP_SIMD:
    case EXEC_OMP_PARALLEL_SECTIONS:
    case EXEC_OMP_PARALLEL_WORKSHARE:
    case EXEC_OMP_SCAN:
    case EXEC_OMP_SCOPE:
    case EXEC_OMP_SECTIONS:
    case EXEC_OMP_SIMD:
    case EXEC_OMP_SINGLE:
    case EXEC_OMP_TARGET:
    case EXEC_OMP_TARGET_DATA:
    case EXEC_OMP_TARGET_ENTER_DATA:
    case EXEC_OMP_TARGET_EXIT_DATA:
    case EXEC_OMP_TARGET_PARALLEL:
    case EXEC_OMP_TARGET_PARALLEL_DO:
    case EXEC_OMP_TARGET_PARALLEL_DO_SIMD:
    case EXEC_OMP_TARGET_PARALLEL_LOOP:
    case EXEC_OMP_TARGET_SIMD:
    case EXEC_OMP_TARGET_TEAMS:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_SIMD:
    case EXEC_OMP_TARGET_TEAMS_LOOP:
    case EXEC_OMP_TARGET_UPDATE:
    case EXEC_OMP_TASK:
    case EXEC_OMP_TASKGROUP:
    case EXEC_OMP_TASKLOOP:
    case EXEC_OMP_TASKLOOP_SIMD:
    case EXEC_OMP_TASKWAIT:
    case EXEC_OMP_TASKYIELD:
    case EXEC_OMP_TEAMS:
    case EXEC_OMP_TEAMS_DISTRIBUTE:
    case EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO:
    case EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
    case EXEC_OMP_TEAMS_DISTRIBUTE_SIMD:
    case EXEC_OMP_TEAMS_LOOP:
    case EXEC_OMP_TILE:
    case EXEC_OMP_UNROLL:
    case EXEC_OMP_WORKSHARE:
      show_omp_node (level, c);
      break;

    default:
      gfc_internal_error ("show_code_node(): Bad statement code");
    }
}


/* Show an equivalence chain.  */

static void
show_equiv (gfc_equiv *eq)
{
  show_indent ();
  fputs ("Equivalence: ", dumpfile);
  while (eq)
    {
      show_expr (eq->expr);
      eq = eq->eq;
      if (eq)
	fputs (", ", dumpfile);
    }
}


/* Show a freakin' whole namespace.  */

static void
show_namespace (gfc_namespace *ns)
{
  gfc_interface *intr;
  gfc_namespace *save;
  int op;
  gfc_equiv *eq;
  int i;

  gcc_assert (ns);
  save = gfc_current_ns;

  show_indent ();
  fputs ("Namespace:", dumpfile);

  i = 0;
  do
    {
      int l = i;
      while (i < GFC_LETTERS - 1
	     && gfc_compare_types (&ns->default_type[i+1],
				   &ns->default_type[l]))
	i++;

      if (i > l)
	fprintf (dumpfile, " %c-%c: ", l+'A', i+'A');
      else
	fprintf (dumpfile, " %c: ", l+'A');

      show_typespec(&ns->default_type[l]);
      i++;
    } while (i < GFC_LETTERS);

  if (ns->proc_name != NULL)
    {
      show_indent ();
      fprintf (dumpfile, "procedure name = %s", ns->proc_name->name);
    }

  ++show_level;
  gfc_current_ns = ns;
  gfc_traverse_symtree (ns->common_root, show_common);

  gfc_traverse_symtree (ns->sym_root, show_symtree);

  for (op = GFC_INTRINSIC_BEGIN; op != GFC_INTRINSIC_END; op++)
    {
      /* User operator interfaces */
      intr = ns->op[op];
      if (intr == NULL)
	continue;

      show_indent ();
      fprintf (dumpfile, "Operator interfaces for %s:",
	       gfc_op2string ((gfc_intrinsic_op) op));

      for (; intr; intr = intr->next)
	fprintf (dumpfile, " %s", intr->sym->name);
    }

  if (ns->uop_root != NULL)
    {
      show_indent ();
      fputs ("User operators:\n", dumpfile);
      gfc_traverse_user_op (ns, show_uop);
    }

  for (eq = ns->equiv; eq; eq = eq->next)
    show_equiv (eq);

  if (ns->oacc_declare)
    {
      struct gfc_oacc_declare *decl;
      /* Dump !$ACC DECLARE clauses.  */
      for (decl = ns->oacc_declare; decl; decl = decl->next)
	{
	  show_indent ();
	  fprintf (dumpfile, "!$ACC DECLARE");
	  show_omp_clauses (decl->clauses);
	}
    }

  if (ns->omp_assumes)
    {
      show_indent ();
      fprintf (dumpfile, "!$OMP ASSUMES");
      show_omp_assumes (ns->omp_assumes);
    }

  fputc ('\n', dumpfile);
  show_indent ();
  fputs ("code:", dumpfile);
  show_code (show_level, ns->code);
  --show_level;

  for (ns = ns->contained; ns; ns = ns->sibling)
    {
      fputs ("\nCONTAINS\n", dumpfile);
      ++show_level;
      show_namespace (ns);
      --show_level;
    }

  fputc ('\n', dumpfile);
  gfc_current_ns = save;
}


/* Main function for dumping a parse tree.  */

void
gfc_dump_parse_tree (gfc_namespace *ns, FILE *file)
{
  dumpfile = file;
  show_namespace (ns);
}

/* This part writes BIND(C) prototypes and declatations, and prototypes
   for EXTERNAL preocedures, for use in a C programs.  */

static void write_interop_decl (gfc_symbol *);
static void write_proc (gfc_symbol *, bool);
static void show_external_symbol (gfc_gsymbol *, void *);
static void write_type (gfc_symbol *sym);
static void write_funptr_fcn (gfc_symbol *);

/* Do we need to write out an #include <ISO_Fortran_binding.h> or not?  */

static void
has_cfi_cdesc (gfc_gsymbol *gsym, void *p)
{
  bool *data_p = (bool *) p;
  gfc_formal_arglist *f;
  gfc_symbol *sym;

  if (*data_p)
    return;

  if (gsym->ns == NULL || gsym->sym_name == NULL )
    return;

  gfc_find_symbol (gsym->sym_name, gsym->ns, 0, &sym);

  if (sym == NULL || sym->attr.flavor != FL_PROCEDURE || !sym->attr.is_bind_c)
    return;

  for (f = sym->formal; f; f = f->next)
    {
      gfc_symbol *s;
      s = f->sym;
      if (s->as && (s->as->type == AS_ASSUMED_RANK || s->as->type == AS_ASSUMED_SHAPE))
	{
	  *data_p = true;
	  return;
	}
    }
}

static bool
need_iso_fortran_binding ()
{
  bool needs_include = false;

  if (gfc_gsym_root == NULL)
    return false;

  gfc_traverse_gsymbol (gfc_gsym_root, has_cfi_cdesc, (void *) &needs_include);
  return needs_include;
}

void
gfc_dump_c_prototypes (FILE *file)
{
  bool bind_c = true;
  int error_count;
  gfc_namespace *ns;
  gfc_get_errors (NULL, &error_count);
  if (error_count != 0)
    return;

  if (gfc_gsym_root == NULL)
    return;

  dumpfile = file;
  if (need_iso_fortran_binding ())
    fputs ("#include <ISO_Fortran_binding.h>\n\n", dumpfile);

  for (ns = gfc_global_ns_list; ns; ns = ns->sibling)
    gfc_traverse_ns (ns, write_type);

  gfc_traverse_gsymbol (gfc_gsym_root, show_external_symbol, (void *) &bind_c);
}

/* Loop over all external symbols, writing out their declarations.  */

static bool seen_conflict;

void
gfc_dump_external_c_prototypes (FILE * file)
{
  bool bind_c = false;
  int error_count;

  gfc_get_errors (NULL, &error_count);
  if (error_count != 0)
    return;

  dumpfile = file;
  seen_conflict = false;
  fprintf (dumpfile,
	   _("/* Prototypes for external procedures generated from %s\n"
	     "   by GNU Fortran %s%s.\n\n"
	     "   Use of this interface is discouraged, consider using the\n"
	     "   BIND(C) feature of standard Fortran instead.  */\n\n"),
	   gfc_source_file, pkgversion_string, version_string);

  if (gfc_gsym_root == NULL)
    return;

  gfc_traverse_gsymbol (gfc_gsym_root, show_external_symbol, (void *) &bind_c);
  if (seen_conflict)
    fprintf (dumpfile,
	     _("\n\n/* WARNING: Because of differing arguments to an external\n"
	       "   procedure, this header file is not compatible with -std=c23."
	       "\n\n   Use another -std option to compile.  */\n"));
}

/* Callback function for dumping external symbols, be they BIND(C) or
 external.  */

static void
show_external_symbol (gfc_gsymbol *gsym, void *data)
{
  bool bind_c, *data_p;
  gfc_symbol *sym;
  const char *name;

  if (gsym->ns == NULL)
    return;

  name = gsym->sym_name ? gsym->sym_name : gsym->name;

  gfc_find_symbol (name, gsym->ns, 0, &sym);
  if (sym == NULL)
    return;

  data_p = (bool *) data;
  bind_c = *data_p;

  if (bind_c)
    {
      if (!sym->attr.is_bind_c)
	return;

      write_interop_decl (sym);
    }
  else
    {
      if (sym->attr.flavor != FL_PROCEDURE || sym->attr.is_bind_c)
	return;
      write_proc (sym, false);
    }
}

enum type_return { T_OK=0, T_WARN, T_ERROR };

/* Return the name of the type for later output.  Both function pointers and
   void pointers will be mapped to void *.  */

static enum type_return
get_c_type_name (gfc_typespec *ts, gfc_array_spec *as, const char **pre,
		 const char **type_name, bool *asterisk, const char **post,
		 bool func_ret)
{
  static char post_buffer[40];
  enum type_return ret;
  ret = T_ERROR;

  *pre = " ";
  *asterisk = false;
  *post = "";
  *type_name = "<error>";

  if (as && (as->type == AS_ASSUMED_RANK || as->type == AS_ASSUMED_SHAPE))
    {
      *asterisk = true;
      *post = "";
      *type_name = "CFI_cdesc_t";
      return T_OK;
    }

  if (ts->type == BT_REAL || ts->type == BT_INTEGER || ts->type == BT_COMPLEX
      || ts->type == BT_UNSIGNED)
    {
      if (ts->is_c_interop && ts->interop_kind)
	ret = T_OK;
      else
	ret = T_WARN;

      for (int i = 0; i < ISOCBINDING_NUMBER; i++)
	{
	  if (c_interop_kinds_table[i].f90_type == ts->type
	      && c_interop_kinds_table[i].value == ts->kind)
	    {
	      /* Skip over 'c_'. */
	      *type_name = c_interop_kinds_table[i].name + 2;
	      if (strcmp (*type_name, "long_long") == 0)
		*type_name = "long long";
	      if (strcmp (*type_name, "long_double") == 0)
		*type_name = "long double";
	      if (strcmp (*type_name, "signed_char") == 0)
		*type_name = "signed char";
	      else if (strcmp (*type_name, "size_t") == 0)
		*type_name = "ssize_t";
	      else if (strcmp (*type_name, "float_complex") == 0)
		*type_name = "__GFORTRAN_FLOAT_COMPLEX";
	      else if (strcmp (*type_name, "double_complex") == 0)
		*type_name = "__GFORTRAN_DOUBLE_COMPLEX";
	      else if (strcmp (*type_name, "long_double_complex") == 0)
		*type_name = "__GFORTRAN_LONG_DOUBLE_COMPLEX";
	      else if (strcmp (*type_name, "unsigned") == 0)
		*type_name = "unsigned int";
	      else if (strcmp (*type_name, "unsigned_char") == 0)
		*type_name = "unsigned char";
	      else if (strcmp (*type_name, "unsigned_short") == 0)
		*type_name = "unsigned short int";
	      else if (strcmp (*type_name, "unsigned_long") == 0)
		*type_name = "unsigned long int";
	      else if (strcmp (*type_name, "unsigned_long long") == 0)
		*type_name = "unsigned long long int";
	      break;
	    }
	}
    }
  else if (ts->type == BT_LOGICAL)
    {
      if (ts->is_c_interop && ts->interop_kind)
	{
	  *type_name = "_Bool";
	  ret = T_OK;
	}
      else
	{
	  /* Let's select an appropriate int, with a warning. */
	  for (int i = 0; i < ISOCBINDING_NUMBER; i++)
	    {
	      if (c_interop_kinds_table[i].f90_type == BT_INTEGER
		  && c_interop_kinds_table[i].value == ts->kind)
		{
		  *type_name = c_interop_kinds_table[i].name + 2;
		  ret = T_WARN;
		}
	    }
	}
    }
  else if (ts->type == BT_CHARACTER)
    {
      if (ts->is_c_interop)
	{
	  *type_name = "char";
	  ret = T_OK;
	}
      else
	{
	  if (ts->kind == gfc_default_character_kind)
	    *type_name = "char";
	  else
	    /* Let's select an appropriate int. */
	    for (int i = 0; i < ISOCBINDING_NUMBER; i++)
	      {
		if (c_interop_kinds_table[i].f90_type == BT_INTEGER
		    && c_interop_kinds_table[i].value == ts->kind)
		  {
		    *type_name = c_interop_kinds_table[i].name + 2;
		    break;
		  }
	    }
	  ret = T_WARN;

	}
    }
  else if (ts->type == BT_DERIVED)
    {
      if (ts->u.derived->from_intmod == INTMOD_ISO_C_BINDING)
	{
	  if (strcmp (ts->u.derived->name, "c_ptr") == 0)
	    *type_name = "void";
	  else if (strcmp (ts->u.derived->name, "c_funptr") == 0)
	    {
	      *type_name = "int ";
	      if (func_ret)
		{
		  *pre = "(";
		  *post = "())";
		}
	      else
		{
		  *pre = "(";
		  *post = ")()";
		}
	    }
	  *asterisk = true;
	  ret = T_OK;
	}
      else
	*type_name = ts->u.derived->name;

      ret = T_OK;
    }

  if (ret != T_ERROR && as && as->type == AS_EXPLICIT)
    {
      mpz_t sz;
      bool size_ok;
      size_ok = spec_size (as, &sz);
      if (size_ok)
	{
	  gmp_snprintf (post_buffer, sizeof(post_buffer), "[%Zd]", sz);
	  *post = post_buffer;
	  mpz_clear (sz);
	  *asterisk = false;
	}
    }
  return ret;
}

/* Write out a declaration.  */

static void
write_decl (gfc_typespec *ts, gfc_array_spec *as, const char *sym_name,
	    bool func_ret, locus *where, bool bind_c)
{
  const char *pre, *type_name, *post;
  bool asterisk;
  enum type_return rok;

  rok = get_c_type_name (ts, as, &pre, &type_name, &asterisk, &post, func_ret);
  if (rok == T_ERROR)
    {
      gfc_error_now ("Cannot convert %qs to interoperable type at %L",
		     gfc_typename (ts), where);
      fprintf (dumpfile, "/* Cannot convert '%s' to interoperable type */",
	       gfc_typename (ts));
      return;
    }
  fputs (type_name, dumpfile);
  fputs (pre, dumpfile);
  if (asterisk)
    fputs ("*", dumpfile);

  fputs (sym_name, dumpfile);
  fputs (post, dumpfile);

  if (rok == T_WARN && bind_c)
    fprintf (dumpfile," /* WARNING: Converting '%s' to interoperable type */",
	     gfc_typename (ts));
}

/* Write out an interoperable type.  It will be written as a typedef
   for a struct.  */

static void
write_type (gfc_symbol *sym)
{
  gfc_component *c;

  /* Don't dump our iso c module, nor vtypes.  */

  if (sym->from_intmod == INTMOD_ISO_C_BINDING || sym->attr.flavor != FL_DERIVED
      || sym->attr.vtype)
    return;

  fprintf (dumpfile, "typedef struct %s {\n", sym->name);
  for (c = sym->components; c; c = c->next)
    {
      fputs ("    ", dumpfile);
      write_decl (&(c->ts), c->as, c->name, false, &sym->declared_at, true);
      fputs (";\n", dumpfile);
    }

  fprintf (dumpfile, "} %s;\n\n", sym->name);
}

/* Write out a variable.  */

static void
write_variable (gfc_symbol *sym)
{
  const char *sym_name;

  gcc_assert (sym->attr.flavor == FL_VARIABLE);

  if (sym->binding_label)
    sym_name = sym->binding_label;
  else
    sym_name = sym->name;

  fputs ("extern ", dumpfile);
  write_decl (&(sym->ts), sym->as, sym_name, false, &sym->declared_at, true);
  fputs (";\n", dumpfile);
}

static void
write_formal_arglist (gfc_symbol *sym, bool bind_c)
{
  gfc_formal_arglist *f;

  for (f = sym->formal; f != NULL; f = f->next)
    {
      enum type_return rok;
      const char *intent_in;
      gfc_symbol *s;
      const char *pre, *type_name, *post;
      bool asterisk;

      s = f->sym;
      rok = get_c_type_name (&(s->ts), s->as, &pre, &type_name, &asterisk,
			     &post, false);
      /* Procedure arguments have to be converted to function pointers.  */
      if (s->attr.subroutine)
	{
	  fprintf (dumpfile, "void (*%s) (", s->name);
	  if (s->ext_dummy_arglist_mismatch)
	    seen_conflict = true;
	  else
	    write_formal_arglist (s, bind_c);

	  fputc (')', dumpfile);
	  goto next;
	}

      if (rok == T_ERROR)
	{
	  gfc_error_now ("Cannot convert %qs to interoperable type at %L",
			 gfc_typename (&s->ts), &s->declared_at);
	  fprintf (dumpfile, "/* Cannot convert '%s' to interoperable type */",
		   gfc_typename (&s->ts));
	  return;
	}

      if (s->attr.function)
	{
	  fprintf (dumpfile, "%s (*%s) (", type_name, s->name);
	  if (s->ext_dummy_arglist_mismatch)
	    seen_conflict = true;
	  else
	    write_formal_arglist (s, bind_c);

	  fputc (')',dumpfile);
	  goto next;
	}

      /* For explicit arrays, we already set the asterisk above.  */
      if (!s->attr.value && !(s->as && s->as->type == AS_EXPLICIT))
	asterisk = true;

      if (s->attr.intent == INTENT_IN && !s->attr.value)
	intent_in = "const ";
      else
	intent_in = "";

      fputs (intent_in, dumpfile);
      fputs (type_name, dumpfile);
      fputs (pre, dumpfile);
      if (asterisk)
	fputs ("*", dumpfile);

      fputs (s->name, dumpfile);
      fputs (post, dumpfile);
      if (bind_c && rok == T_WARN)
	fputs(" /* WARNING: non-interoperable KIND */ ", dumpfile);

    next:
      if (f->next)
	fputs(", ", dumpfile);
    }
  if (!bind_c)
    for (f = sym->formal; f; f = f->next)
      if (f->sym->ts.type == BT_CHARACTER)
	fprintf (dumpfile, ", size_t %s_len", f->sym->name);

}

/* Write out an interoperable function returning a function pointer.  Better
   handled separately.  As we know nothing about the type, assume void.
   Function ponters can be freely converted in C anyway.  */

static void
write_funptr_fcn (gfc_symbol *sym)
{
  fprintf (dumpfile, "void (*%s (", sym->binding_label);
  write_formal_arglist (sym, 1);
  fputs (")) ();\n", dumpfile);
}

/* Write out a procedure, including its arguments.  */
static void
write_proc (gfc_symbol *sym, bool bind_c)
{
  const char *sym_name;
  bool external_character;

  external_character =  sym->ts.type == BT_CHARACTER && !bind_c;

  if (sym->binding_label)
    sym_name = sym->binding_label;
  else
    sym_name = sym->name;

  if (sym->ts.type == BT_UNKNOWN || external_character)
    {
      fprintf (dumpfile, "void ");
      fputs (sym_name, dumpfile);
    }
  else
    write_decl (&(sym->ts), sym->as, sym_name, true, &sym->declared_at, bind_c);

  if (!bind_c)
    fputs ("_", dumpfile);

  fputs (" (", dumpfile);
  if (external_character)
    {
      fprintf (dumpfile, "char *result_%s, size_t result_%s_len",
	       sym_name, sym_name);
      if (sym->formal)
	fputs (", ", dumpfile);
    }
  write_formal_arglist (sym, bind_c);
  fputs (");\n", dumpfile);
}


/* Write a C-interoperable declaration as a C prototype or extern
   declaration.  */

static void
write_interop_decl (gfc_symbol *sym)
{
  /* Only dump bind(c) entities.  */
  if (!sym->attr.is_bind_c)
    return;

  /* Don't dump our iso c module.  */
  if (sym->from_intmod == INTMOD_ISO_C_BINDING)
    return;

  if (sym->attr.flavor == FL_VARIABLE)
    write_variable (sym);
  else if (sym->attr.flavor == FL_DERIVED)
    write_type (sym);
  else if (sym->attr.flavor == FL_PROCEDURE)
    {
      if (sym->ts.type == BT_DERIVED
	  && sym->ts.u.derived->intmod_sym_id == ISOCBINDING_FUNPTR)
	write_funptr_fcn (sym);
      else
	write_proc (sym, true);
    }
}

/* This section deals with dumping the global symbol tree.  */

/* Callback function for printing out the contents of the tree.  */

static void
show_global_symbol (gfc_gsymbol *gsym, void *f_data)
{
  FILE *out;
  out = (FILE *) f_data;

  if (gsym->name)
    fprintf (out, "name=%s", gsym->name);

  if (gsym->sym_name)
    fprintf (out, ", sym_name=%s", gsym->sym_name);

  if (gsym->mod_name)
    fprintf (out, ", mod_name=%s", gsym->mod_name);

  if (gsym->binding_label)
    fprintf (out, ", binding_label=%s", gsym->binding_label);

  fputc ('\n', out);
}

/* Show all global symbols.  */

void
gfc_dump_global_symbols (FILE *f)
{
  if (gfc_gsym_root == NULL)
    fprintf (f, "empty\n");
  else
    gfc_traverse_gsymbol (gfc_gsym_root, show_global_symbol, (void *) f);
}

/* Show an array ref.  */

DEBUG_FUNCTION void
debug (gfc_array_ref *ar)
{
  FILE *tmp = dumpfile;
  dumpfile = stderr;
  show_array_ref (ar);
  fputc ('\n', dumpfile);
  dumpfile = tmp;
}
