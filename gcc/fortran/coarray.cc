/* Rewrite the expression tree for coarrays.
   Copyright (C) 2010-2025 Free Software Foundation, Inc.
   Contributed by Andre Vehreschild.

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

/* Rewrite the expression for coarrays where needed:
   - coarray indexing operations need the indexing expression put into a
     routine callable on the remote image

   This rewriter is meant to used for non-optimisational expression tree
   rewrites.  When implementing early optimisation it is recommended to
   do this in frontend-passes.cc.
*/

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "options.h"
#include "bitmap.h"
#include "gfortran.h"

static gfc_code **current_code;

static bool caf_on_lhs = false;

static gfc_array_spec *
get_arrayspec_from_expr (gfc_expr *expr)
{
  gfc_array_spec *src_as, *dst_as = NULL;
  gfc_ref *ref;
  gfc_array_ref mod_src_ar;
  int dst_rank = 0;

  if (expr->rank == 0)
    return NULL;

  /* Follow any component references.  */
  if (expr->expr_type == EXPR_VARIABLE || expr->expr_type == EXPR_CONSTANT)
    {
      if (expr->symtree)
	src_as = expr->symtree->n.sym->as;
      else
	src_as = NULL;

      for (ref = expr->ref; ref; ref = ref->next)
	{
	  switch (ref->type)
	    {
	    case REF_COMPONENT:
	      src_as = ref->u.c.component->as;
	      continue;

	    case REF_SUBSTRING:
	    case REF_INQUIRY:
	      continue;

	    case REF_ARRAY:
	      switch (ref->u.ar.type)
		{
		case AR_ELEMENT:
		  src_as = NULL;
		  break;
		case AR_SECTION:
		  {
		    if (!dst_as)
		      dst_as = gfc_get_array_spec ();
		    memset (&mod_src_ar, 0, sizeof (gfc_array_ref));
		    mod_src_ar = ref->u.ar;
		    for (int dim = 0; dim < src_as->rank; ++dim)
		      {
			switch (ref->u.ar.dimen_type[dim])
			  {
			  case DIMEN_ELEMENT:
			    gfc_free_expr (mod_src_ar.start[dim]);
			    mod_src_ar.start[dim] = NULL;
			    break;
			  case DIMEN_RANGE:
			    dst_as->lower[dst_rank]
			      = gfc_copy_expr (ref->u.ar.start[dim]);
			    mod_src_ar.start[dst_rank]
			      = gfc_copy_expr (ref->u.ar.start[dim]);
			    if (ref->u.ar.end[dim])
			      {
				dst_as->upper[dst_rank]
				  = gfc_copy_expr (ref->u.ar.end[dim]);
				mod_src_ar.end[dst_rank] = ref->u.ar.end[dim];
				mod_src_ar.stride[dst_rank]
				  = ref->u.ar.stride[dim];
			      }
			    else
			      dst_as->upper[dst_rank]
				= gfc_copy_expr (ref->u.ar.as->upper[dim]);
			    ++dst_rank;
			    break;
			  case DIMEN_STAR:
			    dst_as->lower[dst_rank]
			      = gfc_copy_expr (ref->u.ar.as->lower[dim]);
			    mod_src_ar.start[dst_rank]
			      = gfc_copy_expr (ref->u.ar.start[dim]);
			    if (ref->u.ar.as->upper[dim])
			      {
				dst_as->upper[dst_rank]
				  = gfc_copy_expr (ref->u.ar.as->upper[dim]);
				mod_src_ar.end[dst_rank] = ref->u.ar.end[dim];
				mod_src_ar.stride[dst_rank]
				  = ref->u.ar.stride[dim];
			      }
			    ++dst_rank;
			    break;
			  case DIMEN_VECTOR:
			    dst_as->lower[dst_rank]
			      = gfc_get_constant_expr (BT_INTEGER,
						       gfc_index_integer_kind,
						       &expr->where);
			    mpz_set_ui (dst_as->lower[dst_rank]->value.integer,
					1);
			    mod_src_ar.start[dst_rank]
			      = gfc_copy_expr (ref->u.ar.start[dim]);
			    dst_as->upper[dst_rank]
			      = gfc_get_constant_expr (BT_INTEGER,
						       gfc_index_integer_kind,
						       &expr->where);
			    mpz_set (dst_as->upper[dst_rank]->value.integer,
				     ref->u.ar.start[dim]->shape[0]);
			    ++dst_rank;
			    break;
			  case DIMEN_THIS_IMAGE:
			  case DIMEN_UNKNOWN:
			    gcc_unreachable ();
			  }
			if (ref->u.ar.dimen_type[dim] != DIMEN_ELEMENT)
			  mod_src_ar.dimen_type[dst_rank]
			    = ref->u.ar.dimen_type[dim];
		      }
		    dst_as->rank = dst_rank;
		    dst_as->type = AS_EXPLICIT;
		    ref->u.ar = mod_src_ar;
		    ref->u.ar.dimen = dst_rank;
		    break;

		  case AR_UNKNOWN:
		    src_as = NULL;
		    break;

		  case AR_FULL:
		    dst_as = gfc_copy_array_spec (src_as);
		    break;
		  }
		  break;
		}
	    }
	}
    }
  else
    src_as = NULL;

  return dst_as;
}

static void
remove_coarray_from_derived_type (gfc_symbol *base, gfc_namespace *ns,
				  gfc_array_spec *src_as = NULL)
{
  gfc_symbol *derived;
  gfc_symbol *src_derived = base->ts.u.derived;

  if (!src_as)
    src_as = src_derived->as;
  gfc_get_symbol (src_derived->name, ns, &derived);
  derived->attr.flavor = FL_DERIVED;
  derived->attr.alloc_comp = src_derived->attr.alloc_comp;
  if (src_as && src_as->rank != 0)
    {
      base->attr.dimension = 1;
      base->as = gfc_copy_array_spec (src_as);
      base->as->corank = 0;
    }
  for (gfc_component *p = NULL, *c = src_derived->components; c; c = c->next)
    {
      gfc_component *n = gfc_get_component ();
      *n = *c;
      if (n->as)
	n->as = gfc_copy_array_spec (c->as);
      n->backend_decl = NULL;
      n->initializer = NULL;
      n->param_list = NULL;
      if (p)
	p->next = n;
      else
	derived->components = n;

      p = n;
    }
  gfc_set_sym_referenced (derived);
  gfc_commit_symbol (derived);
  base->ts.u.derived = derived;
  gfc_commit_symbol (base);
}

static void
convert_coarray_class_to_derived_type (gfc_symbol *base, gfc_namespace *ns)
{
  gfc_symbol *src_derived = CLASS_DATA (base)->ts.u.derived;
  gfc_array_spec *src_as = CLASS_DATA (base)->as;
  const bool attr_allocatable
    = src_as && src_as->rank && src_as->type == AS_DEFERRED;

  base->ts.type = BT_DERIVED;
  base->ts.u.derived = src_derived;

  remove_coarray_from_derived_type (base, ns, src_as);

  base->attr.allocatable = attr_allocatable;
  base->attr.pointer = 0; // Ensure, that it is no pointer.
}

static void
split_expr_at_caf_ref (gfc_expr *expr, gfc_namespace *ns,
		       gfc_expr **post_caf_ref_expr)
{
  gfc_ref *caf_ref = NULL;
  gfc_symtree *st;
  gfc_symbol *base;

  gcc_assert (expr->expr_type == EXPR_VARIABLE);
  if (!expr->symtree->n.sym->attr.codimension)
    {
      /* The coarray is in some component.  Find it.  */
      caf_ref = expr->ref;
      while (caf_ref)
	{
	  if (caf_ref->type == REF_COMPONENT
	      && caf_ref->u.c.component->attr.codimension)
	    break;
	  caf_ref = caf_ref->next;
	}
    }

  gcc_assert (!gfc_get_sym_tree (!caf_ref ? expr->symtree->name : "base", ns,
				 &st, false));
  st->n.sym->attr.flavor = FL_PARAMETER;
  st->n.sym->attr.dummy = 1;
  st->n.sym->attr.intent = INTENT_IN;
  st->n.sym->ts = caf_ref ? caf_ref->u.c.sym->ts : expr->symtree->n.sym->ts;

  *post_caf_ref_expr = gfc_get_variable_expr (st);
  (*post_caf_ref_expr)->where = expr->where;
  base = (*post_caf_ref_expr)->symtree->n.sym;

  if (!caf_ref)
    {
      (*post_caf_ref_expr)->ref = gfc_copy_ref (expr->ref);
      if (expr->symtree->n.sym->attr.dimension)
	{
	  base->as = gfc_copy_array_spec (expr->symtree->n.sym->as);
	  base->as->corank = 0;
	  base->attr.dimension = 1;
	  base->attr.allocatable = expr->symtree->n.sym->attr.allocatable;
	  base->attr.pointer = expr->symtree->n.sym->attr.pointer
			       || expr->symtree->n.sym->attr.associate_var;
	}
    }
  else
    {
      (*post_caf_ref_expr)->ref = gfc_copy_ref (caf_ref->next);
      if (caf_ref->u.c.component->attr.dimension)
	{
	  base->as = gfc_copy_array_spec (caf_ref->u.c.component->as);
	  base->as->corank = 0;
	  base->attr.dimension = 1;
	  base->attr.allocatable = caf_ref->u.c.component->attr.allocatable;
	  base->attr.pointer = caf_ref->u.c.component->attr.pointer;
	}
      base->ts = caf_ref->u.c.component->ts;
    }
  (*post_caf_ref_expr)->ts = expr->ts;
  if (base->ts.type == BT_CHARACTER)
    {
      base->ts.u.cl = gfc_get_charlen ();
      *base->ts.u.cl = *(caf_ref ? caf_ref->u.c.component->ts.u.cl
				 : expr->symtree->n.sym->ts.u.cl);
      base->ts.deferred = 1;
      base->ts.u.cl->length = nullptr;
    }

  if (base->ts.type == BT_DERIVED)
    remove_coarray_from_derived_type (base, ns);
  else if (base->ts.type == BT_CLASS)
    convert_coarray_class_to_derived_type (base, ns);

  gfc_expression_rank (expr);
  gfc_expression_rank (*post_caf_ref_expr);
}

static void
check_add_new_component (gfc_symbol *type, gfc_expr *e, gfc_symbol *get_data)
{
  if (e)
    {
      switch (e->expr_type)
	{
	case EXPR_CONSTANT:
	case EXPR_NULL:
	  break;
	case EXPR_OP:
	  check_add_new_component (type, e->value.op.op1, get_data);
	  if (e->value.op.op2)
	    check_add_new_component (type, e->value.op.op2, get_data);
	  break;
	case EXPR_COMPCALL:
	  for (gfc_actual_arglist *actual = e->value.compcall.actual; actual;
	       actual = actual->next)
	    check_add_new_component (type, actual->expr, get_data);
	  break;
	case EXPR_FUNCTION:
	  if (!e->symtree->n.sym->attr.pure
	      && !e->symtree->n.sym->attr.elemental)
	    {
	      // Treat non-pure functions.
	      gfc_error ("Sorry, not yet able to call a non-pure/non-elemental"
			 " function %s in a coarray reference;  use a temporary"
			 " for the function's result instead",
			 e->symtree->n.sym->name);
	    }
	  for (gfc_actual_arglist *actual = e->value.function.actual; actual;
	       actual = actual->next)
	    check_add_new_component (type, actual->expr, get_data);
	  break;
	case EXPR_VARIABLE:
	  {
	    gfc_component *comp;
	    gfc_ref *ref;
	    int old_rank = e->rank;

	    /* Can't use gfc_find_component here, because type is not yet
	       complete.  */
	    comp = type->components;
	    while (comp)
	      {
		if (strcmp (comp->name, e->symtree->name) == 0)
		  break;
		comp = comp->next;
	      }
	    if (!comp)
	      {
		gcc_assert (gfc_add_component (type, e->symtree->name, &comp));
		/* Take a copy of e, before modifying it.  */
		gfc_expr *init = gfc_copy_expr (e);
		if (e->ref)
		  {
		    switch (e->ref->type)
		      {
		      case REF_ARRAY:
			comp->as = get_arrayspec_from_expr (e);
			comp->attr.dimension = e->ref->u.ar.dimen != 0;
			comp->ts = e->ts;
			break;
		      case REF_COMPONENT:
			comp->ts = e->ref->u.c.sym->ts;
			break;
		      default:
			gcc_unreachable ();
			break;
		      }
		  }
		else
		  comp->ts = e->ts;
		comp->attr.access = ACCESS_PRIVATE;
		comp->initializer = init;
	      }
	    else
	      gcc_assert (comp->ts.type == e->ts.type
			  && comp->ts.u.derived == e->ts.u.derived);

	    ref = e->ref;
	    e->ref = NULL;
	    gcc_assert (gfc_find_component (get_data->ts.u.derived,
					    e->symtree->name, false, true,
					    &e->ref));
	    e->symtree
	      = gfc_find_symtree (get_data->ns->sym_root, get_data->name);
	    e->ref->next = ref;
	    gfc_free_shape (&e->shape, old_rank);
	    gfc_expression_rank (e);
	    break;
	  }
	case EXPR_ARRAY:
	case EXPR_PPC:
	case EXPR_STRUCTURE:
	case EXPR_SUBSTRING:
	  gcc_unreachable ();
	default:;
	}
    }
}

static gfc_symbol *
create_get_parameter_type (gfc_expr *expr, gfc_namespace *ns,
			   gfc_symbol *get_data)
{
  static int type_cnt = 0;
  char tname[GFC_MAX_SYMBOL_LEN + 1];
  char *name;
  gfc_symbol *type;

  gcc_assert (expr->expr_type == EXPR_VARIABLE);

  strcpy (tname, expr->symtree->name);
  name = xasprintf ("@_rget_data_t_%s_%d", tname, ++type_cnt);
  gfc_get_symbol (name, ns, &type);

  type->attr.flavor = FL_DERIVED;
  get_data->ts.u.derived = type;

  for (gfc_ref *ref = expr->ref; ref; ref = ref->next)
    {
      if (ref->type == REF_ARRAY)
	{
	  gfc_array_ref *ar = &ref->u.ar;
	  for (int i = 0; i < ar->dimen; ++i)
	    {
	      check_add_new_component (type, ar->start[i], get_data);
	      check_add_new_component (type, ar->end[i], get_data);
	      check_add_new_component (type, ar->stride[i], get_data);
	    }
	}
    }

  gfc_set_sym_referenced (type);
  gfc_commit_symbol (type);
  return type;
}

static gfc_expr *
create_get_callback (gfc_expr *expr)
{
  static int cnt = 0;
  gfc_namespace *ns;
  gfc_symbol *extproc, *proc, *buffer, *free_buffer, *base, *get_data,
    *old_buffer_data;
  char tname[GFC_MAX_SYMBOL_LEN + 1];
  char *name;
  const char *mname;
  gfc_expr *cb, *post_caf_ref_expr;
  gfc_code *code;
  int expr_rank = expr->rank;

  /* Find the top-level namespace.  */
  for (ns = gfc_current_ns; ns->parent; ns = ns->parent)
    ;

  if (expr->expr_type == EXPR_VARIABLE)
    strcpy (tname, expr->symtree->name);
  else
    strcpy (tname, "dummy");
  if (expr->symtree->n.sym->module)
    mname = expr->symtree->n.sym->module;
  else
    mname = "main";
  name = xasprintf ("_caf_rget_%s_%s_%d", mname, tname, ++cnt);
  gfc_get_symbol (name, ns, &extproc);
  gfc_set_sym_referenced (extproc);
  ++extproc->refs;
  gfc_commit_symbol (extproc);

  /* Set up namespace.  */
  gfc_namespace *sub_ns = gfc_get_namespace (ns, 0);
  sub_ns->sibling = ns->contained;
  ns->contained = sub_ns;
  sub_ns->resolved = 1;
  /* Set up procedure symbol.  */
  gfc_find_symbol (name, sub_ns, 1, &proc);
  sub_ns->proc_name = proc;
  proc->attr.if_source = IFSRC_DECL;
  proc->attr.access = ACCESS_PUBLIC;
  gfc_add_subroutine (&proc->attr, name, NULL);
  proc->attr.host_assoc = 1;
  proc->attr.always_explicit = 1;
  ++proc->refs;
  gfc_commit_symbol (proc);
  free (name);

  split_expr_at_caf_ref (expr, sub_ns, &post_caf_ref_expr);

  if (ns->proc_name->attr.flavor == FL_MODULE)
    proc->module = ns->proc_name->name;
  gfc_set_sym_referenced (proc);
  /* Set up formal arguments.  */
  gfc_formal_arglist **argptr = &proc->formal;
#define ADD_ARG(name, nsym, stype, sintent)                                    \
  gfc_get_symbol (name, sub_ns, &nsym);                                        \
  nsym->ts.type = stype;                                                       \
  nsym->attr.flavor = FL_PARAMETER;                                            \
  nsym->attr.dummy = 1;                                                        \
  nsym->attr.intent = sintent;                                                 \
  gfc_set_sym_referenced (nsym);                                               \
  *argptr = gfc_get_formal_arglist ();                                         \
  (*argptr)->sym = nsym;                                                       \
  argptr = &(*argptr)->next

  ADD_ARG ("buffer", buffer, expr->ts.type, INTENT_INOUT);
  buffer->ts = expr->ts;
  if (expr_rank)
    {
      buffer->as = gfc_get_array_spec ();
      buffer->as->rank = expr_rank;
      if (expr->shape)
	{
	  buffer->as->type = AS_EXPLICIT;
	  for (int d = 0; d < expr_rank; ++d)
	    {
	      buffer->as->lower[d]
		= gfc_get_constant_expr (BT_INTEGER, gfc_index_integer_kind,
					 &gfc_current_locus);
	      gfc_mpz_set_hwi (buffer->as->lower[d]->value.integer, 1);
	      buffer->as->upper[d]
		= gfc_get_constant_expr (BT_INTEGER, gfc_index_integer_kind,
					 &gfc_current_locus);
	      gfc_mpz_set_hwi (buffer->as->upper[d]->value.integer,
			       gfc_mpz_get_hwi (expr->shape[d]));
	    }
	  buffer->attr.allocatable = 1;
	}
      else
	{
	  buffer->as->type = AS_DEFERRED;
	  buffer->attr.allocatable = 1;
	}
      buffer->attr.dimension = 1;
    }
  else
    buffer->attr.pointer = 1;
  if (buffer->ts.type == BT_CHARACTER)
    {
      buffer->ts.u.cl = gfc_get_charlen ();
      *buffer->ts.u.cl = *expr->ts.u.cl;
      buffer->ts.deferred = 1;
      buffer->ts.u.cl->length = nullptr;
    }
  gfc_commit_symbol (buffer);
  ADD_ARG ("free_buffer", free_buffer, BT_LOGICAL, INTENT_OUT);
  free_buffer->ts.kind = gfc_default_logical_kind;
  gfc_commit_symbol (free_buffer);

  // ADD_ARG (expr->symtree->name, base, BT_VOID, INTENT_IN);
  base = post_caf_ref_expr->symtree->n.sym;
  gfc_set_sym_referenced (base);
  gfc_commit_symbol (base);
  *argptr = gfc_get_formal_arglist ();
  (*argptr)->sym = base;
  argptr = &(*argptr)->next;

  gfc_commit_symbol (base);
  ADD_ARG ("get_data", get_data, BT_DERIVED, INTENT_IN);
  gfc_commit_symbol (get_data);
#undef ADD_ARG

  /* Set up code.  */
  if (expr->rank != 0)
    {
      /* Code: old_buffer_ptr = C_LOC (buffer);  */
      code = sub_ns->code = gfc_get_code (EXEC_ASSIGN);
      gfc_get_symbol ("old_buffer_data", sub_ns, &old_buffer_data);
      old_buffer_data->ts.type = BT_VOID;
      old_buffer_data->attr.flavor = FL_VARIABLE;
      gfc_set_sym_referenced (old_buffer_data);
      gfc_commit_symbol (old_buffer_data);
      code->expr1 = gfc_lval_expr_from_sym (old_buffer_data);
      code->expr2 = gfc_build_intrinsic_call (ns, GFC_ISYM_C_LOC, "C_LOC",
					      gfc_current_locus, 1,
					      gfc_lval_expr_from_sym (buffer));
      code->next = gfc_get_code (EXEC_ASSIGN);
      code = code->next;
    }
  else
    code = sub_ns->code = gfc_get_code (EXEC_POINTER_ASSIGN);

  /* Code: buffer = expr;  */
  code->expr1 = gfc_lval_expr_from_sym (buffer);
  code->expr2 = post_caf_ref_expr;
  gfc_ref *ref = code->expr2->ref, **pref = &code->expr2->ref;
  if (ref && ref->type == REF_ARRAY && ref->u.ar.codimen != 0)
    {
      if (ref->u.ar.dimen != 0)
	{
	  ref->u.ar.codimen = 0;
	  pref = &ref->next;
	  ref = ref->next;
	}
      else
	{
	  code->expr2->ref = ref->next;
	  ref->next = NULL;
	  gfc_free_ref_list (ref);
	  ref = code->expr2->ref;
	  pref = &code->expr2->ref;
	}
    }
  if (ref && ref->type == REF_COMPONENT)
    {
      gfc_find_component (code->expr2->symtree->n.sym->ts.u.derived,
			  ref->u.c.component->name, false, false, pref);
      if (*pref != ref)
	{
	  (*pref)->next = ref->next;
	  ref->next = NULL;
	  gfc_free_ref_list (ref);
	}
    }
  get_data->ts.u.derived
    = create_get_parameter_type (code->expr2, ns, get_data);
  if (code->expr2->rank == 0)
    code->expr2 = gfc_build_intrinsic_call (ns, GFC_ISYM_C_LOC, "C_LOC",
					    gfc_current_locus, 1, code->expr2);

  /* Code: *free_buffer = old_buffer_ptr /= C_LOC (buffer); for rank != 0 or
   *       *free_buffer = 0; for rank == 0.  */
  code->next = gfc_get_code (EXEC_ASSIGN);
  code = code->next;
  code->expr1 = gfc_lval_expr_from_sym (free_buffer);
  if (expr->rank != 0)
    {
      code->expr2 = gfc_get_operator_expr (
	&gfc_current_locus, INTRINSIC_NE_OS,
	gfc_lval_expr_from_sym (old_buffer_data),
	gfc_build_intrinsic_call (ns, GFC_ISYM_C_LOC, "C_LOC",
				  gfc_current_locus, 1,
				  gfc_lval_expr_from_sym (buffer)));
      code->expr2->ts.type = BT_LOGICAL;
      code->expr2->ts.kind = gfc_default_logical_kind;
    }
  else
    {
      code->expr2 = gfc_get_logical_expr (gfc_default_logical_kind,
					  &gfc_current_locus, false);
    }

  cb = gfc_lval_expr_from_sym (extproc);
  cb->ts.interface = extproc;

  return cb;
}

static void
add_caf_get_intrinsic (gfc_expr *e)
{
  gfc_expr *wrapper, *tmp_expr, *rget_expr, *rget_hash_expr;
  gfc_ref *ref;
  int n;

  for (ref = e->ref; ref; ref = ref->next)
    if (ref->type == REF_ARRAY && ref->u.ar.codimen > 0)
      break;
  if (ref == NULL)
    return;

  for (n = ref->u.ar.dimen; n < ref->u.ar.dimen + ref->u.ar.codimen; n++)
    if (ref->u.ar.dimen_type[n] != DIMEN_ELEMENT)
      return;

  tmp_expr = XCNEW (gfc_expr);
  *tmp_expr = *e;
  rget_expr = create_get_callback (tmp_expr);
  rget_hash_expr = gfc_get_expr ();
  rget_hash_expr->expr_type = EXPR_CONSTANT;
  rget_hash_expr->ts.type = BT_INTEGER;
  rget_hash_expr->ts.kind = gfc_default_integer_kind;
  rget_hash_expr->where = tmp_expr->where;
  mpz_init_set_ui (rget_hash_expr->value.integer,
		   gfc_hash_value (rget_expr->symtree->n.sym));
  wrapper = gfc_build_intrinsic_call (gfc_current_ns, GFC_ISYM_CAF_GET,
				      "caf_get", tmp_expr->where, 3, tmp_expr,
				      rget_hash_expr, rget_expr);
  gfc_add_caf_accessor (rget_hash_expr, rget_expr);
  wrapper->ts = e->ts;
  wrapper->rank = e->rank;
  wrapper->corank = e->corank;
  if (e->rank)
    wrapper->shape = gfc_copy_shape (e->shape, e->rank);
  *e = *wrapper;
  free (wrapper);
}

static int
coindexed_expr_callback (gfc_expr **e, int *walk_subtrees,
			 void *data ATTRIBUTE_UNUSED)
{
  if ((*e)->expr_type == EXPR_VARIABLE)
    {
      if (!caf_on_lhs && gfc_is_coindexed (*e))
	{
	  add_caf_get_intrinsic (*e);
	  *walk_subtrees = 0;
	  return 0;
	}
      /* Clear the flag to rewrite caf_gets in sub expressions of the lhs.  */
      caf_on_lhs = false;
    }

  *walk_subtrees = 1;
  return 0;
}

static int
coindexed_code_callback (gfc_code **c, int *walk_subtrees,
			 void *data ATTRIBUTE_UNUSED)
{
  int ws = 1;
  current_code = c;

  switch ((*c)->op)
    {
    case EXEC_ASSIGN:
    case EXEC_POINTER_ASSIGN:
      caf_on_lhs = true;
      coindexed_expr_callback (&((*c)->expr1), &ws, NULL);
      caf_on_lhs = false;
      ws = 1;
      coindexed_expr_callback (&((*c)->expr2), &ws, NULL);
      *walk_subtrees = ws;
      break;
    case EXEC_LOCK:
    case EXEC_UNLOCK:
    case EXEC_EVENT_POST:
    case EXEC_EVENT_WAIT:
      *walk_subtrees = 0;
      break;
    default:
      *walk_subtrees = 1;
      break;
    }
  return 0;
}

void
gfc_coarray_rewrite (gfc_namespace *ns)
{
  gfc_namespace *saved_ns = gfc_current_ns;
  gfc_current_ns = ns;

  if (flag_coarray == GFC_FCOARRAY_LIB)
    gfc_code_walker (&ns->code, coindexed_code_callback,
		     coindexed_expr_callback, NULL);

  gfc_current_ns = saved_ns;
}
