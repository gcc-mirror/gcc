/* Parse tree dumper
   Copyright (C) 2003-2018 Free Software Foundation, Inc.
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


/* Allow dumping of an expression in the debugger.  */
void gfc_debug_expr (gfc_expr *);

void
gfc_debug_expr (gfc_expr *e)
{
  FILE *tmp = dumpfile;
  dumpfile = stderr;
  show_expr (e);
  fputc ('\n', dumpfile);
  dumpfile = tmp;
}

/* Allow for dumping of a piece of code in the debugger.  */
void gfc_debug_code (gfc_code *c);

void
gfc_debug_code (gfc_code *c)
{
  FILE *tmp = dumpfile;
  dumpfile = stderr;
  show_code (1, c);
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
	  fputs ("= ", dumpfile);
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
  if (attr->flavor != FL_UNKNOWN)
    {
      if (attr->flavor == FL_DERIVED && attr->pdt_template)
	fputs (" (PDT template", dumpfile);
      else
    fprintf (dumpfile, "(%s ", gfc_code2string (flavors, attr->flavor));
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
  if (attr->is_protected)
    fputs (" PROTECTED", dumpfile);
  if (attr->value)
    fputs (" VALUE", dumpfile);
  if (attr->volatile_)
    fputs (" VOLATILE", dumpfile);
  if (attr->threadprivate)
    fputs (" THREADPRIVATE", dumpfile);
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
  if (attr->elemental)
    fputs (" ELEMENTAL", dumpfile);
  if (attr->pure)
    fputs (" PURE", dumpfile);
  if (attr->recursive)
    fputs (" RECURSIVE", dumpfile);

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

  if (sym->as)
    {
      show_indent ();
      fputs ("Array spec:", dumpfile);
      show_array_spec (sym->as);
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
show_omp_namelist (int list_type, gfc_omp_namelist *n)
{
  for (; n; n = n->next)
    {
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
	    if (n->udr)
	      fprintf (dumpfile, "%s:", n->udr->udr->name);
	    break;
	  default: break;
	  }
      else if (list_type == OMP_LIST_DEPEND)
	switch (n->u.depend_op)
	  {
	  case OMP_DEPEND_IN: fputs ("in:", dumpfile); break;
	  case OMP_DEPEND_OUT: fputs ("out:", dumpfile); break;
	  case OMP_DEPEND_INOUT: fputs ("inout:", dumpfile); break;
	  case OMP_DEPEND_SINK_FIRST:
	    fputs ("sink:", dumpfile);
	    while (1)
	      {
		fprintf (dumpfile, "%s", n->sym->name);
		if (n->expr)
		  {
		    fputc ('+', dumpfile);
		    show_expr (n->expr);
		  }
		if (n->next == NULL)
		  break;
		else if (n->next->u.depend_op != OMP_DEPEND_SINK)
		  {
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
	switch (n->u.map_op)
	  {
	  case OMP_MAP_ALLOC: fputs ("alloc:", dumpfile); break;
	  case OMP_MAP_TO: fputs ("to:", dumpfile); break;
	  case OMP_MAP_FROM: fputs ("from:", dumpfile); break;
	  case OMP_MAP_TOFROM: fputs ("tofrom:", dumpfile); break;
	  default: break;
	  }
      else if (list_type == OMP_LIST_LINEAR)
	switch (n->u.linear_op)
	  {
	  case OMP_LINEAR_REF: fputs ("ref(", dumpfile); break;
	  case OMP_LINEAR_VAL: fputs ("val(", dumpfile); break;
	  case OMP_LINEAR_UVAL: fputs ("uval(", dumpfile); break;
	  default: break;
	  }
      fprintf (dumpfile, "%s", n->sym->name);
      if (list_type == OMP_LIST_LINEAR && n->u.linear_op != OMP_LINEAR_DEFAULT)
	fputc (')', dumpfile);
      if (n->expr)
	{
	  fputc (':', dumpfile);
	  show_expr (n->expr);
	}
      if (n->next)
	fputc (',', dumpfile);
    }
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
  if (omp_clauses->collapse)
    fprintf (dumpfile, " COLLAPSE(%d)", omp_clauses->collapse);
  for (list_type = 0; list_type < OMP_LIST_NUM; list_type++)
    if (omp_clauses->lists[list_type] != NULL
	&& list_type != OMP_LIST_COPYPRIVATE)
      {
	const char *type = NULL;
	switch (list_type)
	  {
	  case OMP_LIST_USE_DEVICE: type = "USE_DEVICE"; break;
	  case OMP_LIST_DEVICE_RESIDENT: type = "USE_DEVICE"; break;
	  case OMP_LIST_CACHE: type = ""; break;
	  case OMP_LIST_PRIVATE: type = "PRIVATE"; break;
	  case OMP_LIST_FIRSTPRIVATE: type = "FIRSTPRIVATE"; break;
	  case OMP_LIST_LASTPRIVATE: type = "LASTPRIVATE"; break;
	  case OMP_LIST_SHARED: type = "SHARED"; break;
	  case OMP_LIST_COPYIN: type = "COPYIN"; break;
	  case OMP_LIST_UNIFORM: type = "UNIFORM"; break;
	  case OMP_LIST_ALIGNED: type = "ALIGNED"; break;
	  case OMP_LIST_LINEAR: type = "LINEAR"; break;
	  case OMP_LIST_REDUCTION: type = "REDUCTION"; break;
	  case OMP_LIST_IS_DEVICE_PTR: type = "IS_DEVICE_PTR"; break;
	  case OMP_LIST_USE_DEVICE_PTR: type = "USE_DEVICE_PTR"; break;
	  case OMP_LIST_DEPEND: type = "DEPEND"; break;
	  default:
	    gcc_unreachable ();
	  }
	fprintf (dumpfile, " %s(", type);
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
	case OMP_PROC_BIND_MASTER: type = "MASTER"; break;
	case OMP_PROC_BIND_SPREAD: type = "SPREAD"; break;
	case OMP_PROC_BIND_CLOSE: type = "CLOSE"; break;
	default:
	  gcc_unreachable ();
	}
      fprintf (dumpfile, " PROC_BIND(%s)", type);
    }
  if (omp_clauses->num_teams)
    {
      fputs (" NUM_TEAMS(", dumpfile);
      show_expr (omp_clauses->num_teams);
      fputc (')', dumpfile);
    }
  if (omp_clauses->device)
    {
      fputs (" DEVICE(", dumpfile);
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
      fprintf (dumpfile, " DIST_SCHEDULE (STATIC");
      if (omp_clauses->dist_chunk_size)
	{
	  fputc (',', dumpfile);
	  show_expr (omp_clauses->dist_chunk_size);
	}
      fputc (')', dumpfile);
    }
  if (omp_clauses->defaultmap)
    fputs (" DEFALTMAP (TOFROM: SCALAR)", dumpfile);
  if (omp_clauses->nogroup)
    fputs (" NOGROUP", dumpfile);
  if (omp_clauses->simd)
    fputs (" SIMD", dumpfile);
  if (omp_clauses->threads)
    fputs (" THREADS", dumpfile);
  if (omp_clauses->grainsize)
    {
      fputs (" GRAINSIZE(", dumpfile);
      show_expr (omp_clauses->grainsize);
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
      show_expr (omp_clauses->num_tasks);
      fputc (')', dumpfile);
    }
  if (omp_clauses->priority)
    {
      fputs (" PRIORITY(", dumpfile);
      show_expr (omp_clauses->priority);
      fputc (')', dumpfile);
    }
  for (i = 0; i < OMP_IF_LAST; i++)
    if (omp_clauses->if_exprs[i])
      {
	static const char *ifs[] = {
	  "PARALLEL",
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
  if (omp_clauses->depend_source)
    fputs (" DEPEND(source)", dumpfile);
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
    case EXEC_OACC_DATA: name = "DATA"; is_oacc = true; break;
    case EXEC_OACC_HOST_DATA: name = "HOST_DATA"; is_oacc = true; break;
    case EXEC_OACC_LOOP: name = "LOOP"; is_oacc = true; break;
    case EXEC_OACC_UPDATE: name = "UPDATE"; is_oacc = true; break;
    case EXEC_OACC_WAIT: name = "WAIT"; is_oacc = true; break;
    case EXEC_OACC_CACHE: name = "CACHE"; is_oacc = true; break;
    case EXEC_OACC_ENTER_DATA: name = "ENTER DATA"; is_oacc = true; break;
    case EXEC_OACC_EXIT_DATA: name = "EXIT DATA"; is_oacc = true; break;
    case EXEC_OMP_ATOMIC: name = "ATOMIC"; break;
    case EXEC_OMP_BARRIER: name = "BARRIER"; break;
    case EXEC_OMP_CANCEL: name = "CANCEL"; break;
    case EXEC_OMP_CANCELLATION_POINT: name = "CANCELLATION POINT"; break;
    case EXEC_OMP_CRITICAL: name = "CRITICAL"; break;
    case EXEC_OMP_DISTRIBUTE: name = "DISTRIBUTE"; break;
    case EXEC_OMP_DISTRIBUTE_PARALLEL_DO:
      name = "DISTRIBUTE PARALLEL DO"; break;
    case EXEC_OMP_DISTRIBUTE_PARALLEL_DO_SIMD:
      name = "DISTRIBUTE PARALLEL DO SIMD"; break;
    case EXEC_OMP_DISTRIBUTE_SIMD: name = "DISTRIBUTE SIMD"; break;
    case EXEC_OMP_DO: name = "DO"; break;
    case EXEC_OMP_DO_SIMD: name = "DO SIMD"; break;
    case EXEC_OMP_FLUSH: name = "FLUSH"; break;
    case EXEC_OMP_MASTER: name = "MASTER"; break;
    case EXEC_OMP_ORDERED: name = "ORDERED"; break;
    case EXEC_OMP_PARALLEL: name = "PARALLEL"; break;
    case EXEC_OMP_PARALLEL_DO: name = "PARALLEL DO"; break;
    case EXEC_OMP_PARALLEL_DO_SIMD: name = "PARALLEL DO SIMD"; break;
    case EXEC_OMP_PARALLEL_SECTIONS: name = "PARALLEL SECTIONS"; break;
    case EXEC_OMP_PARALLEL_WORKSHARE: name = "PARALLEL WORKSHARE"; break;
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
    case EXEC_OACC_DATA:
    case EXEC_OACC_HOST_DATA:
    case EXEC_OACC_LOOP:
    case EXEC_OACC_UPDATE:
    case EXEC_OACC_WAIT:
    case EXEC_OACC_CACHE:
    case EXEC_OACC_ENTER_DATA:
    case EXEC_OACC_EXIT_DATA:
    case EXEC_OMP_CANCEL:
    case EXEC_OMP_CANCELLATION_POINT:
    case EXEC_OMP_DISTRIBUTE:
    case EXEC_OMP_DISTRIBUTE_PARALLEL_DO:
    case EXEC_OMP_DISTRIBUTE_PARALLEL_DO_SIMD:
    case EXEC_OMP_DISTRIBUTE_SIMD:
    case EXEC_OMP_DO:
    case EXEC_OMP_DO_SIMD:
    case EXEC_OMP_ORDERED:
    case EXEC_OMP_PARALLEL:
    case EXEC_OMP_PARALLEL_DO:
    case EXEC_OMP_PARALLEL_DO_SIMD:
    case EXEC_OMP_PARALLEL_SECTIONS:
    case EXEC_OMP_PARALLEL_WORKSHARE:
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
    case EXEC_OMP_TARGET_SIMD:
    case EXEC_OMP_TARGET_TEAMS:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_SIMD:
    case EXEC_OMP_TARGET_UPDATE:
    case EXEC_OMP_TASK:
    case EXEC_OMP_TASKLOOP:
    case EXEC_OMP_TASKLOOP_SIMD:
    case EXEC_OMP_TEAMS:
    case EXEC_OMP_TEAMS_DISTRIBUTE:
    case EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO:
    case EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
    case EXEC_OMP_TEAMS_DISTRIBUTE_SIMD:
    case EXEC_OMP_WORKSHARE:
      omp_clauses = c->ext.omp_clauses;
      break;
    case EXEC_OMP_CRITICAL:
      omp_clauses = c->ext.omp_clauses;
      if (omp_clauses)
	fprintf (dumpfile, " (%s)", c->ext.omp_clauses->critical_name);
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
      || c->op == EXEC_OMP_TARGET_EXIT_DATA
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
  else
    show_code (level + 1, c->block->next);
  if (c->op == EXEC_OMP_ATOMIC)
    return;
  fputc ('\n', dumpfile);
  code_indent (level, 0);
  fprintf (dumpfile, "!$%s END %s", is_oacc ? "ACC" : "OMP", name);
  if (omp_clauses != NULL)
    {
      if (omp_clauses->lists[OMP_LIST_COPYPRIVATE])
	{
	  fputs (" COPYPRIVATE(", dumpfile);
	  show_omp_namelist (OMP_LIST_COPYPRIVATE,
			     omp_clauses->lists[OMP_LIST_COPYPRIVATE]);
	  fputc (')', dumpfile);
	}
      else if (omp_clauses->nowait)
	fputs (" NOWAIT", dumpfile);
    }
  else if (c->op == EXEC_OMP_CRITICAL && c->ext.omp_clauses)
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
	const char* blocktype;
	gfc_namespace *saved_ns;
	gfc_association_list *alist;

	if (c->ext.block.assoc)
	  blocktype = "ASSOCIATE";
	else
	  blocktype = "BLOCK";
	show_indent ();
	fprintf (dumpfile, "%s ", blocktype);
	for (alist = c->ext.block.assoc; alist; alist = alist->next)
	  {
	    fprintf (dumpfile, " %s = ", alist->name);
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
      d = c->block;
      if (c->op == EXEC_SELECT_TYPE)
	fputs ("SELECT TYPE ", dumpfile);
      else
	fputs ("SELECT CASE ", dumpfile);
      show_expr (c->expr1);
      fputc ('\n', dumpfile);

      for (; d; d = d->block)
	{
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
	  fputc ('\n', dumpfile);

	  show_code (level + 1, d->next);
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
      for (fa = c->ext.forall_iterator; fa; fa = fa->next)
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
      for (fa = c->ext.forall_iterator; fa; fa = fa->next)
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
      show_expr (c->expr1);
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
    case EXEC_OACC_DATA:
    case EXEC_OACC_HOST_DATA:
    case EXEC_OACC_LOOP:
    case EXEC_OACC_UPDATE:
    case EXEC_OACC_WAIT:
    case EXEC_OACC_CACHE:
    case EXEC_OACC_ENTER_DATA:
    case EXEC_OACC_EXIT_DATA:
    case EXEC_OMP_ATOMIC:
    case EXEC_OMP_CANCEL:
    case EXEC_OMP_CANCELLATION_POINT:
    case EXEC_OMP_BARRIER:
    case EXEC_OMP_CRITICAL:
    case EXEC_OMP_DISTRIBUTE:
    case EXEC_OMP_DISTRIBUTE_PARALLEL_DO:
    case EXEC_OMP_DISTRIBUTE_PARALLEL_DO_SIMD:
    case EXEC_OMP_DISTRIBUTE_SIMD:
    case EXEC_OMP_DO:
    case EXEC_OMP_DO_SIMD:
    case EXEC_OMP_FLUSH:
    case EXEC_OMP_MASTER:
    case EXEC_OMP_ORDERED:
    case EXEC_OMP_PARALLEL:
    case EXEC_OMP_PARALLEL_DO:
    case EXEC_OMP_PARALLEL_DO_SIMD:
    case EXEC_OMP_PARALLEL_SECTIONS:
    case EXEC_OMP_PARALLEL_WORKSHARE:
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
    case EXEC_OMP_TARGET_SIMD:
    case EXEC_OMP_TARGET_TEAMS:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_SIMD:
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

/* This part writes BIND(C) definition for use in external C programs.  */

static void write_interop_decl (gfc_symbol *);

void
gfc_dump_c_prototypes (gfc_namespace *ns, FILE *file)
{
  int error_count;
  gfc_get_errors (NULL, &error_count);
  if (error_count != 0)
    return;
  dumpfile = file;
  gfc_traverse_ns (ns, write_interop_decl);
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
  if (ts->type == BT_REAL || ts->type == BT_INTEGER)
    {
      if (ts->is_c_interop && ts->interop_kind)
	{
	  *type_name = ts->interop_kind->name + 2;
	  if (strcmp (*type_name, "signed_char") == 0)
	    *type_name = "signed char";
	  else if (strcmp (*type_name, "size_t") == 0)
	    *type_name = "ssize_t";

	  ret = T_OK;
	}
      else
	{
	  /* The user did not specify a C interop type.  Let's look through
	     the available table and use the first one, but warn.  */
	  for (int i = 0; i < ISOCBINDING_NUMBER; i++)
	    {
	      if (c_interop_kinds_table[i].f90_type == ts->type
		  && c_interop_kinds_table[i].value == ts->kind)
		{
		  *type_name = c_interop_kinds_table[i].name + 2;
		  if (strcmp (*type_name, "signed_char") == 0)
		    *type_name = "signed char";
		  else if (strcmp (*type_name, "size_t") == 0)
		    *type_name = "ssize_t";

		  ret = T_WARN;
		  break;
		}
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
	}
      else
	*type_name = ts->u.derived->name;

      ret = T_OK;
    }
  if (ret != T_ERROR && as)
    {
      mpz_t sz;
      bool size_ok;
      size_ok = spec_size (as, &sz);
      gcc_assert (size_ok == true);
      gmp_snprintf (post_buffer, sizeof(post_buffer), "[%Zd]", sz);
      *post = post_buffer;
      mpz_clear (sz);
    }
  return ret;
}

/* Write out a declaration.  */
static void
write_decl (gfc_typespec *ts, gfc_array_spec *as, const char *sym_name,
	    bool func_ret, locus *where)
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
    
  if (rok == T_WARN)
    fprintf (dumpfile," /* WARNING: Converting '%s' to interoperable type */",
	     gfc_typename (ts));
}

/* Write out an interoperable type.  It will be written as a typedef
   for a struct.  */

static void
write_type (gfc_symbol *sym)
{
  gfc_component *c;

  fprintf (dumpfile, "typedef struct %s {\n", sym->name);
  for (c = sym->components; c; c = c->next)
    {
      fputs ("    ", dumpfile);
      write_decl (&(c->ts), c->as, c->name, false, &sym->declared_at);
      fputs (";\n", dumpfile);
    }

  fprintf (dumpfile, "} %s;\n", sym->name);
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
  write_decl (&(sym->ts), sym->as, sym_name, false, &sym->declared_at);
  fputs (";\n", dumpfile);
}


/* Write out a procedure, including its arguments.  */
static void
write_proc (gfc_symbol *sym)
{
  const char *pre, *type_name, *post;
  bool asterisk;
  enum type_return rok;
  gfc_formal_arglist *f;
  const char *sym_name;
  const char *intent_in;

  if (sym->binding_label)
    sym_name = sym->binding_label;
  else
    sym_name = sym->name;

  if (sym->ts.type == BT_UNKNOWN)
    {
      fprintf (dumpfile, "void ");
      fputs (sym_name, dumpfile);
    }
  else
    write_decl (&(sym->ts), sym->as, sym->name, true, &sym->declared_at);

  fputs (" (", dumpfile);

  for (f = sym->formal; f; f = f->next)
    {
      gfc_symbol *s;
      s = f->sym;
      rok = get_c_type_name (&(s->ts), NULL, &pre, &type_name, &asterisk,
			     &post, false);
      if (rok == T_ERROR)
	{
	  gfc_error_now ("Cannot convert %qs to interoperable type at %L",
			 gfc_typename (&s->ts), &s->declared_at);
	  fprintf (stderr, "/* Cannot convert '%s' to interoperable type */",
		   gfc_typename (&s->ts));
	  return;
	}

      if (!s->attr.value)
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
      if (rok == T_WARN)
	fputs(" /* WARNING: non-interoperable KIND */ ", dumpfile);

      if (f->next)
	fputs(", ", dumpfile);
    }
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
    write_proc (sym);
}
