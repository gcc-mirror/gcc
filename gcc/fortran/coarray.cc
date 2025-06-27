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

/* The code tree element that is currently processed.  */
static gfc_code **current_code;

/* Code that is inserted into the current caf_accessor at the beginning.  */
static gfc_code *caf_accessor_prepend = nullptr;

static bool caf_on_lhs = false;

static int caf_sym_cnt = 0;

static gfc_array_spec *
get_arrayspec_from_expr (gfc_expr *expr)
{
  gfc_array_spec *src_as, *dst_as = NULL;
  gfc_ref *ref;
  gfc_array_ref mod_src_ar;
  int dst_rank = 0;

  if (expr->rank == 0)
    return NULL;

  if (expr->expr_type == EXPR_FUNCTION)
    return gfc_copy_array_spec (expr->symtree->n.sym->as);

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
		    if (dst_as)
		      /* Prevent memory loss.  */
		      gfc_free_array_spec (dst_as);
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
  derived->declared_at = base->declared_at;
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
move_coarray_ref (gfc_ref **from, gfc_expr *expr)
{
  int i;
  gfc_ref *to = expr->ref;
  for (; to && to->next; to = to->next)
    ;

  if (!to)
    {
      expr->ref = gfc_get_ref ();
      to = expr->ref;
      to->type = REF_ARRAY;
    }
  gcc_assert (to->type == REF_ARRAY);
  to->u.ar.as = gfc_copy_array_spec ((*from)->u.ar.as);
  to->u.ar.codimen = (*from)->u.ar.codimen;
  to->u.ar.dimen = (*from)->u.ar.dimen;
  to->u.ar.type = AR_FULL;
  to->u.ar.stat = (*from)->u.ar.stat;
  (*from)->u.ar.stat = nullptr;
  to->u.ar.team = (*from)->u.ar.team;
  (*from)->u.ar.team = nullptr;
  to->u.ar.team_type = (*from)->u.ar.team_type;
  (*from)->u.ar.team_type = TEAM_UNSET;
  for (i = 0; i < to->u.ar.dimen; ++i)
    {
      to->u.ar.start[i] = nullptr;
      to->u.ar.end[i] = nullptr;
      to->u.ar.stride[i] = nullptr;
    }
  for (i = (*from)->u.ar.dimen; i < (*from)->u.ar.dimen + (*from)->u.ar.codimen;
       ++i)
    {
      to->u.ar.dimen_type[i] = (*from)->u.ar.dimen_type[i];
      to->u.ar.start[i] = (*from)->u.ar.start[i];
      (*from)->u.ar.start[i] = nullptr;
      to->u.ar.end[i] = (*from)->u.ar.end[i];
      (*from)->u.ar.end[i] = nullptr;
      to->u.ar.stride[i] = (*from)->u.ar.stride[i];
      (*from)->u.ar.stride[i] = nullptr;
    }
  (*from)->u.ar.codimen = 0;
  if ((*from)->u.ar.dimen == 0)
    {
      gfc_ref *nref = (*from)->next;
      (*from)->next = nullptr;
      gfc_free_ref_list (*from);
      *from = nref;
    }
}

static void
fixup_comp_refs (gfc_expr *expr)
{
  bool class_ref = expr->symtree->n.sym->ts.type == BT_CLASS;
  gfc_symbol *type
    = expr->symtree->n.sym->ts.type == BT_DERIVED
	? expr->symtree->n.sym->ts.u.derived
	: (class_ref ? CLASS_DATA (expr->symtree->n.sym)->ts.u.derived
		     : nullptr);
  if (!type)
    return;
  gfc_ref **pref = &(expr->ref);
  for (gfc_ref *ref = expr->ref; ref && type;)
    {
      switch (ref->type)
	{
	case REF_COMPONENT:
	  gfc_find_component (type, ref->u.c.component->name, false, true,
			      pref);
	  if (!*pref)
	    {
	      /* This happens when there were errors previously.  Just don't
		 crash.  */
	      ref = nullptr;
	      break;
	    }
	  if (class_ref)
	    /* Link to the class type to allow for derived type resolution.  */
	    (*pref)->u.c.sym = ref->u.c.sym;
	  (*pref)->next = ref->next;
	  ref->next = NULL;
	  gfc_free_ref_list (ref);
	  ref = (*pref)->next;
	  type = (*pref)->u.c.component->ts.type == BT_DERIVED
		   ? (*pref)->u.c.component->ts.u.derived
		   : ((*pref)->u.c.component->ts.type == BT_CLASS
			? CLASS_DATA ((*pref)->u.c.component)->ts.u.derived
			: nullptr);
	  pref = &(*pref)->next;
	  break;
	case REF_ARRAY:
	  pref = &ref->next;
	  ref = ref->next;
	  break;
	default:
	  gcc_unreachable ();
	  break;
	}
    }
}

static void
split_expr_at_caf_ref (gfc_expr *expr, gfc_namespace *ns,
		       gfc_expr **post_caf_ref_expr, bool for_send)
{
  gfc_ref *caf_ref = NULL;
  gfc_symtree *st;
  gfc_symbol *base;
  gfc_typespec *caf_ts;
  bool created;

  gcc_assert (expr->expr_type == EXPR_VARIABLE);
  caf_ts = &expr->symtree->n.sym->ts;
  if (!expr->symtree->n.sym->attr.codimension)
    {
      /* The coarray is in some component.  Find it.  */
      caf_ref = expr->ref;
      while (caf_ref)
	{
	  if (caf_ref->type == REF_ARRAY && caf_ref->u.ar.codimen != 0)
	    break;
	  if (caf_ref->type == REF_COMPONENT)
	    caf_ts = &caf_ref->u.c.component->ts;
	  caf_ref = caf_ref->next;
	}
    }

  created = !gfc_get_sym_tree (!caf_ref ? expr->symtree->name : "base", ns, &st,
			       false);
  gcc_assert (created);
  st->n.sym->attr.flavor = FL_PARAMETER;
  st->n.sym->attr.dummy = 1;
  st->n.sym->attr.intent = INTENT_IN;
  st->n.sym->ts = *caf_ts;
  st->n.sym->declared_at = expr->where;

  *post_caf_ref_expr = gfc_get_variable_expr (st);
  (*post_caf_ref_expr)->where = expr->where;
  base = (*post_caf_ref_expr)->symtree->n.sym;

  if (!caf_ref)
    {
      (*post_caf_ref_expr)->ref = gfc_get_ref ();
      *(*post_caf_ref_expr)->ref = *expr->ref;
      expr->ref = nullptr;
      move_coarray_ref (&(*post_caf_ref_expr)->ref, expr);
      fixup_comp_refs (expr);

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
      (*post_caf_ref_expr)->ref = gfc_get_ref ();
      *(*post_caf_ref_expr)->ref = *caf_ref;
      caf_ref->next = nullptr;
      move_coarray_ref (&(*post_caf_ref_expr)->ref, expr);
      fixup_comp_refs (expr);

      if (caf_ref && caf_ref->u.ar.dimen)
	{
	  base->as = gfc_copy_array_spec (caf_ref->u.ar.as);
	  base->as->corank = 0;
	  base->attr.dimension = 1;
	  base->attr.allocatable = caf_ref->u.ar.as->type != AS_EXPLICIT;
	}
      base->ts = *caf_ts;
    }
  (*post_caf_ref_expr)->ts = expr->ts;
  if (base->ts.type == BT_CHARACTER)
    {
      base->ts.u.cl = gfc_get_charlen ();
      *base->ts.u.cl = *(caf_ts->u.cl);
      base->ts.deferred = 1;
      base->ts.u.cl->length = nullptr;
    }
  else if (base->ts.type == BT_DERIVED)
    remove_coarray_from_derived_type (base, ns);
  else if (base->ts.type == BT_CLASS)
    convert_coarray_class_to_derived_type (base, ns);

  gfc_expression_rank (*post_caf_ref_expr);
  if (for_send)
    gfc_expression_rank (expr);
  else
    expr->rank = (*post_caf_ref_expr)->rank;
}

static void add_caf_get_from_remote (gfc_expr *e);

static gfc_component *
find_comp (gfc_symbol *type, gfc_expr *e, int *cnt, const bool is_var)
{
  char *temp_name = nullptr;
  gfc_component *comp = type->components;

  /* For variables:
     - look up same name or create new
     all else:
     - create unique new
  */
  if (is_var)
    {
      ++(*cnt);
      free (temp_name);
      temp_name = xasprintf ("caf_temp_%s_%d", e->symtree->name, *cnt);
      while (comp && strcmp (comp->name, temp_name) != 0)
	comp = comp->next;
      if (!comp)
	{
	  const bool added = gfc_add_component (type, temp_name, &comp);
	  gcc_assert (added);
	}
    }
  else
    {
      int r = -1;
      /* Components are always appended, i.e., when searching to add a unique
	 one just iterating forward is sufficient.  */
      do
	{
	  ++(*cnt);
	  free (temp_name);
	  temp_name = xasprintf ("caf_temp_%s_%d", e->symtree->name, *cnt);

	  while (comp && (r = strcmp (comp->name, temp_name)) <= 0)
	    comp = comp->next;
	}
      while (comp && r <= 0);
      {
	const bool added = gfc_add_component (type, temp_name, &comp);
	gcc_assert (added);
      }
    }

  comp->loc = e->where;
  comp->ts = e->ts;
  free (temp_name);

  return comp;
}

static void
check_add_new_comp_handle_array (gfc_expr *e, gfc_symbol *type,
				 gfc_symbol *add_data)
{
  gfc_component *comp;
  static int cnt = -1;
  gfc_symtree *caller_image;
  gfc_code *pre_code = caf_accessor_prepend;
  bool static_array_or_scalar = true;
  symbol_attribute e_attr = gfc_expr_attr (e);

  gfc_free_shape (&e->shape, e->rank);

  /* When already code to prepend into the accessor exists, go to
     the end of the chain.  */
  for (; pre_code && pre_code->next; pre_code = pre_code->next)
    ;

  comp = find_comp (type, e, &cnt,
		    e->symtree->n.sym->attr.flavor == FL_VARIABLE
		      || e->symtree->n.sym->attr.flavor == FL_PARAMETER);

  if (e->expr_type == EXPR_FUNCTION
      || (e->expr_type == EXPR_VARIABLE && e_attr.dimension
	  && e_attr.allocatable))
    {
      gfc_code *code;
      gfc_symtree *st;
      const bool created
	= !gfc_get_sym_tree (comp->name, gfc_current_ns, &st, false, &e->where);
      gcc_assert (created);

      st->n.sym->ts = e->ts;
      gfc_set_sym_referenced (st->n.sym);
      code = gfc_get_code (EXEC_ASSIGN);
      code->loc = e->where;
      code->expr1 = gfc_get_variable_expr (st);
      code->expr2 = XCNEW (gfc_expr);
      *(code->expr2) = *e;
      code->next = *current_code;
      *current_code = code;

      if (e_attr.dimension)
	{
	  gfc_array_spec *as = get_arrayspec_from_expr (e);
	  static_array_or_scalar = gfc_is_compile_time_shape (as);

	  comp->attr.dimension = 1;
	  st->n.sym->attr.dimension = 1;
	  st->n.sym->as = as;

	  if (!static_array_or_scalar)
	    {
	      comp->attr.allocatable = 1;
	      st->n.sym->attr.allocatable = 1;
	    }
	  code->expr1->rank = as->rank;
	  gfc_add_full_array_ref (code->expr1, gfc_copy_array_spec (as));
	  comp->as = gfc_copy_array_spec (as);
	}

      gfc_expression_rank (code->expr1);
      comp->initializer = gfc_get_variable_expr (st);
      gfc_commit_symbol (st->n.sym);
    }
  else
    {
      comp->initializer = gfc_copy_expr (e);
      if (e_attr.dimension && e->rank)
	{
	  comp->attr.dimension = 1;
	  comp->as = get_arrayspec_from_expr (e);
	}
    }
  comp->initializer->where = e->where;
  comp->attr.access = ACCESS_PRIVATE;
  memset (e, 0, sizeof (gfc_expr));
  e->ts = comp->initializer->ts;
  e->expr_type = EXPR_VARIABLE;
  e->where = comp->initializer->where;

  if (comp->as && comp->as->rank)
    {
      if (static_array_or_scalar)
	{
	  e->ref = gfc_get_ref ();
	  e->ref->type = REF_ARRAY;
	  e->ref->u.ar.as = gfc_copy_array_spec (add_data->as);
	  e->ref->u.ar.codimen = 1;
	  e->ref->u.ar.dimen_type[0] = DIMEN_THIS_IMAGE;
	}
      else
	{
	  gfc_code *c;
	  gfc_symtree *lv, *ad;
	  bool created = !gfc_get_sym_tree (comp->name, add_data->ns, &lv,
					    false, &e->where);
	  gcc_assert (created);

	  lv->n.sym->ts = e->ts;
	  lv->n.sym->attr.dimension = 1;
	  lv->n.sym->attr.allocatable = 1;
	  lv->n.sym->attr.flavor = FL_VARIABLE;
	  lv->n.sym->as = gfc_copy_array_spec (comp->as);
	  gfc_set_sym_referenced (lv->n.sym);
	  gfc_commit_symbol (lv->n.sym);
	  c = gfc_get_code (EXEC_ASSIGN);
	  c->loc = e->where;
	  c->expr1 = gfc_get_variable_expr (lv);
	  c->expr1->where = e->where;

	  created = !gfc_find_sym_tree (add_data->name, add_data->ns, 0, &ad);
	  gcc_assert (created);
	  c->expr2 = gfc_get_variable_expr (ad);
	  c->expr2->where = e->where;
	  c->expr2->ts = comp->initializer->ts;
	  c->expr2->ref = gfc_get_ref ();
	  c->expr2->ref->type = REF_ARRAY;
	  c->expr2->ref->u.ar.as = gfc_copy_array_spec (add_data->as);
	  c->expr2->ref->u.ar.codimen = 1;
	  c->expr2->ref->u.ar.dimen_type[0] = DIMEN_ELEMENT;
	  caller_image
	    = gfc_find_symtree_in_proc ("caller_image", add_data->ns);
	  gcc_assert (caller_image);
	  c->expr2->ref->u.ar.start[0] = gfc_get_variable_expr (caller_image);
	  c->expr2->ref->u.ar.start[0]->where = e->where;
	  created = gfc_find_component (ad->n.sym->ts.u.derived, comp->name,
					false, true, &c->expr2->ref->next)
		    != nullptr;
	  gcc_assert (created);
	  c->expr2->rank = comp->as->rank;
	  gfc_add_full_array_ref (c->expr2, gfc_copy_array_spec (comp->as));
	  gfc_set_sym_referenced (ad->n.sym);
	  gfc_commit_symbol (ad->n.sym);
	  if (pre_code)
	    pre_code->next = c;
	  else
	    caf_accessor_prepend = c;
	  add_caf_get_from_remote (c->expr2);

	  e->symtree = lv;
	  gfc_expression_rank (e);
	  gfc_add_full_array_ref (e, gfc_copy_array_spec (comp->as));
	}
    }
  else
    {
      e->ref = gfc_get_ref ();
      e->ref->type = REF_ARRAY;
      e->ref->u.ar.as = gfc_copy_array_spec (add_data->as);
      e->ref->u.ar.codimen = 1;
      e->ref->u.ar.dimen_type[0] = DIMEN_THIS_IMAGE;
    }

  if (static_array_or_scalar)
    {
      const bool created
	= gfc_find_component (add_data->ts.u.derived, comp->name, false, true,
			      &e->ref);
      gcc_assert (created);
      e->symtree = gfc_find_symtree (add_data->ns->sym_root, add_data->name);
      gcc_assert (e->symtree);
      if (IS_CLASS_ARRAY (e->ref->u.c.component)
	  || e->ref->u.c.component->attr.dimension)
	{
	  gfc_add_full_array_ref (e, e->ref->u.c.component->ts.type == BT_CLASS
				       ? CLASS_DATA (e->ref->u.c.component)->as
				       : e->ref->u.c.component->as);
	  e->ref->next->u.ar.dimen
	    = e->ref->u.c.component->ts.type == BT_CLASS
		? CLASS_DATA (e->ref->u.c.component)->as->rank
		: e->ref->u.c.component->as->rank;
	}
      gfc_expression_rank (e);
    }
}

static void
check_add_new_component (gfc_symbol *type, gfc_expr *e, gfc_symbol *add_data)
{
  if (e)
    {
      switch (e->expr_type)
	{
	case EXPR_CONSTANT:
	case EXPR_NULL:
	  break;
	case EXPR_OP:
	  check_add_new_component (type, e->value.op.op1, add_data);
	  if (e->value.op.op2)
	    check_add_new_component (type, e->value.op.op2, add_data);
	  break;
	case EXPR_COMPCALL:
	  for (gfc_actual_arglist *actual = e->value.compcall.actual; actual;
	       actual = actual->next)
	    check_add_new_component (type, actual->expr, add_data);
	  break;
	case EXPR_FUNCTION:
	  if (!e->symtree->n.sym->attr.pure
	      && !e->symtree->n.sym->attr.elemental)
	    /* Treat non-pure/non-elemental functions.  */
	    check_add_new_comp_handle_array (e, type, add_data);
	  else
	    for (gfc_actual_arglist *actual = e->value.function.actual; actual;
		 actual = actual->next)
	      check_add_new_component (type, actual->expr, add_data);
	  break;
	case EXPR_VARIABLE:
	    check_add_new_comp_handle_array (e, type, add_data);
	    break;
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
create_caf_add_data_parameter_type (gfc_expr *expr, gfc_namespace *ns,
				    gfc_symbol *add_data)
{
  static int type_cnt = 0;
  char tname[GFC_MAX_SYMBOL_LEN + 1];
  char *name;
  gfc_symbol *type;

  gcc_assert (expr->expr_type == EXPR_VARIABLE);

  strcpy (tname, expr->symtree->name);
  name = xasprintf ("@_caf_add_data_t_%s_%d", tname, ++type_cnt);
  gfc_get_symbol (name, ns, &type);

  type->attr.flavor = FL_DERIVED;
  add_data->ts.u.derived = type;
  add_data->attr.codimension = 1;
  add_data->as = gfc_get_array_spec ();
  add_data->as->corank = 1;
  add_data->as->type = AS_EXPLICIT;
  add_data->as->cotype = AS_DEFERRED;
  add_data->as->lower[0]
    = gfc_get_constant_expr (BT_INTEGER, gfc_default_integer_kind,
			     &expr->where);
  mpz_init (add_data->as->lower[0]->value.integer);
  mpz_set_si (add_data->as->lower[0]->value.integer, 1);

  for (gfc_ref *ref = expr->ref; ref; ref = ref->next)
    {
      if (ref->type == REF_ARRAY)
	{
	  gfc_array_ref *ar = &ref->u.ar;
	  for (int i = 0; i < ar->dimen; ++i)
	    {
	      check_add_new_component (type, ar->start[i], add_data);
	      check_add_new_component (type, ar->end[i], add_data);
	      check_add_new_component (type, ar->stride[i], add_data);
	    }
	}
    }

  type->declared_at = expr->where;
  gfc_set_sym_referenced (type);
  gfc_commit_symbol (type);
  return type;
}

static void
remove_caf_ref (gfc_expr *expr, const bool conv_to_this_image_cafref = false)
{
  gfc_ref *ref = expr->ref;
  while (ref && (ref->type != REF_ARRAY || ref->u.ar.codimen == 0))
    {
      ref = ref->next;
    }
  if (ref && ref->type == REF_ARRAY && ref->u.ar.codimen != 0)
    {
      if (ref->u.ar.dimen != 0)
	{
	  ref->u.ar.codimen = 0;
	  ref = ref->next;
	}
      else
	{
	  if (conv_to_this_image_cafref)
	    {
	      for (int i = ref->u.ar.dimen;
		   i < ref->u.ar.dimen + ref->u.ar.codimen; ++i)
		ref->u.ar.dimen_type[i] = DIMEN_THIS_IMAGE;
	    }
	  else
	    {
	      expr->ref = ref->next;
	      ref->next = NULL;
	      gfc_free_ref_list (ref);
	      ref = expr->ref;
	    }
	}
    }
  fixup_comp_refs (expr);
}

static gfc_expr *
create_get_callback (gfc_expr *expr)
{
  gfc_namespace *ns;
  gfc_symbol *extproc, *proc, *buffer, *free_buffer, *base, *get_data,
    *old_buffer_data, *caller_image;
  char tname[GFC_MAX_SYMBOL_LEN + 1];
  char *name;
  const char *mname;
  gfc_expr *cb, *post_caf_ref_expr;
  gfc_code *code;
  int expr_rank = expr->rank;
  gfc_code *backup_caf_accessor_prepend = caf_accessor_prepend;
  caf_accessor_prepend = nullptr;

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
  name = xasprintf ("_caf_accessor_%s_%s_%d", mname, tname, ++caf_sym_cnt);
  gfc_get_symbol (name, ns, &extproc);
  extproc->declared_at = expr->where;
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
  proc->declared_at = expr->where;
  gfc_commit_symbol (proc);
  free (name);

  split_expr_at_caf_ref (expr, sub_ns, &post_caf_ref_expr, false);

  if (ns->proc_name->attr.flavor == FL_MODULE)
    proc->module = ns->proc_name->name;
  gfc_set_sym_referenced (proc);
  /* Set up formal arguments.  */
  gfc_formal_arglist **argptr = &proc->formal;
#define ADD_ARG(name, nsym, stype, skind, sintent)                             \
  gfc_get_symbol (name, sub_ns, &nsym);                                        \
  nsym->ts.type = stype;                                                       \
  nsym->ts.kind = skind;                                                       \
  nsym->attr.flavor = FL_PARAMETER;                                            \
  nsym->attr.dummy = 1;                                                        \
  nsym->attr.intent = sintent;                                                 \
  nsym->declared_at = expr->where;                                             \
  gfc_set_sym_referenced (nsym);                                               \
  *argptr = gfc_get_formal_arglist ();                                         \
  (*argptr)->sym = nsym;                                                       \
  argptr = &(*argptr)->next

  name = xasprintf ("add_data_%s_%s_%d", mname, tname, caf_sym_cnt);
  ADD_ARG (name, get_data, BT_DERIVED, 0, INTENT_IN);
  gfc_commit_symbol (get_data);
  free (name);

  ADD_ARG ("caller_image", caller_image, BT_INTEGER, gfc_default_integer_kind,
	   INTENT_IN);
  gfc_commit_symbol (caller_image);

  ADD_ARG ("buffer", buffer, expr->ts.type, expr->ts.kind, INTENT_INOUT);
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
      buffer->ts.u.cl->length = gfc_copy_expr (expr->ts.u.cl->length);
    }
  gfc_commit_symbol (buffer);

  ADD_ARG ("free_buffer", free_buffer, BT_LOGICAL, gfc_default_logical_kind,
	   INTENT_OUT);
  gfc_commit_symbol (free_buffer);

  // ADD_ARG (expr->symtree->name, base, BT_VOID, INTENT_IN);
  base = post_caf_ref_expr->symtree->n.sym;
  gfc_set_sym_referenced (base);
  gfc_commit_symbol (base);
  *argptr = gfc_get_formal_arglist ();
  (*argptr)->sym = base;
  argptr = &(*argptr)->next;
  gfc_commit_symbol (base);
#undef ADD_ARG

  /* Set up code.  */
  if (expr->rank != 0)
    {
      /* Code: old_buffer_ptr = C_LOC (buffer);  */
      code = sub_ns->code = gfc_get_code (EXEC_ASSIGN);
      gfc_get_symbol ("old_buffer_data", sub_ns, &old_buffer_data);
      old_buffer_data->ts.type = BT_VOID;
      old_buffer_data->attr.flavor = FL_VARIABLE;
      old_buffer_data->declared_at = expr->where;
      gfc_set_sym_referenced (old_buffer_data);
      gfc_commit_symbol (old_buffer_data);
      code->loc = expr->where;
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
  code->loc = expr->where;
  code->expr1 = gfc_lval_expr_from_sym (buffer);
  code->expr2 = post_caf_ref_expr;
  remove_caf_ref (post_caf_ref_expr);
  get_data->ts.u.derived
    = create_caf_add_data_parameter_type (code->expr2, ns, get_data);
  if (code->expr2->rank == 0 && code->expr2->ts.type != BT_CHARACTER)
    code->expr2 = gfc_build_intrinsic_call (ns, GFC_ISYM_C_LOC, "C_LOC",
					    gfc_current_locus, 1, code->expr2);

  /* Code: *free_buffer = old_buffer_ptr /= C_LOC (buffer); for rank != 0 or
   *       *free_buffer = 0; for rank == 0.  */
  code->next = gfc_get_code (EXEC_ASSIGN);
  code = code->next;
  code->loc = expr->where;
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

  if (caf_accessor_prepend)
    {
      gfc_code *c = caf_accessor_prepend;
      /* Find last in chain.  */
      for (; c->next; c = c->next)
	;
      c->next = sub_ns->code;
      sub_ns->code = caf_accessor_prepend;
    }
  caf_accessor_prepend = backup_caf_accessor_prepend;
  return cb;
}

void
add_caf_get_from_remote (gfc_expr *e)
{
  gfc_expr *wrapper, *tmp_expr, *get_from_remote_expr,
    *get_from_remote_hash_expr;
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
  get_from_remote_expr = create_get_callback (tmp_expr);
  get_from_remote_hash_expr = gfc_get_expr ();
  get_from_remote_hash_expr->expr_type = EXPR_CONSTANT;
  get_from_remote_hash_expr->ts.type = BT_INTEGER;
  get_from_remote_hash_expr->ts.kind = gfc_default_integer_kind;
  get_from_remote_hash_expr->where = tmp_expr->where;
  mpz_init_set_ui (get_from_remote_hash_expr->value.integer,
		   gfc_hash_value (get_from_remote_expr->symtree->n.sym));
  wrapper = gfc_build_intrinsic_call (gfc_current_ns, GFC_ISYM_CAF_GET,
				      "caf_get", tmp_expr->where, 3, tmp_expr,
				      get_from_remote_hash_expr,
				      get_from_remote_expr);
  gfc_add_caf_accessor (get_from_remote_hash_expr, get_from_remote_expr);
  wrapper->ts = e->ts;
  wrapper->rank = e->rank;
  wrapper->corank = e->corank;
  if (e->rank)
    wrapper->shape = gfc_copy_shape (e->shape, e->rank);
  *e = *wrapper;
  free (wrapper);
}

static gfc_expr *
create_allocated_callback (gfc_expr *expr)
{
  gfc_namespace *ns;
  gfc_symbol *extproc, *proc, *result, *base, *add_data, *caller_image;
  char tname[GFC_MAX_SYMBOL_LEN + 1];
  char *name;
  const char *mname;
  gfc_expr *cb, *post_caf_ref_expr;
  gfc_code *code;
  gfc_code *backup_caf_accessor_prepend = caf_accessor_prepend;
  caf_accessor_prepend = nullptr;
  gfc_expr swp;

  /* Find the top-level namespace.  */
  for (ns = gfc_current_ns; ns->parent; ns = ns->parent)
    ;

  if (expr->value.function.actual->expr->expr_type == EXPR_VARIABLE)
    strcpy (tname, expr->value.function.actual->expr->symtree->name);
  else
    strcpy (tname, "dummy");
  if (expr->value.function.actual->expr->symtree->n.sym->module)
    mname = expr->value.function.actual->expr->symtree->n.sym->module;
  else
    mname = "main";
  name = xasprintf ("_caf_present_%s_%s_%d", mname, tname, ++caf_sym_cnt);
  gfc_get_symbol (name, ns, &extproc);
  extproc->declared_at = expr->where;
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
  proc->declared_at = expr->where;
  ++proc->refs;
  gfc_commit_symbol (proc);
  free (name);

  split_expr_at_caf_ref (expr->value.function.actual->expr, sub_ns,
			 &post_caf_ref_expr, true);

  if (ns->proc_name->attr.flavor == FL_MODULE)
    proc->module = ns->proc_name->name;
  gfc_set_sym_referenced (proc);
  /* Set up formal arguments.  */
  gfc_formal_arglist **argptr = &proc->formal;
#define ADD_ARG(name, nsym, stype, skind, sintent)                             \
  gfc_get_symbol (name, sub_ns, &nsym);                                        \
  nsym->ts.type = stype;                                                       \
  nsym->ts.kind = skind;                                                       \
  nsym->attr.flavor = FL_PARAMETER;                                            \
  nsym->attr.dummy = 1;                                                        \
  nsym->attr.intent = sintent;                                                 \
  nsym->declared_at = expr->where;                                             \
  gfc_set_sym_referenced (nsym);                                               \
  *argptr = gfc_get_formal_arglist ();                                         \
  (*argptr)->sym = nsym;                                                       \
  argptr = &(*argptr)->next

  name = xasprintf ("add_data_%s_%s_%d", mname, tname, ++caf_sym_cnt);
  ADD_ARG (name, add_data, BT_DERIVED, 0, INTENT_IN);
  gfc_commit_symbol (add_data);
  free (name);
  ADD_ARG ("caller_image", caller_image, BT_INTEGER, gfc_default_integer_kind,
	   INTENT_IN);
  gfc_commit_symbol (caller_image);

  ADD_ARG ("result", result, BT_LOGICAL, gfc_default_logical_kind, INTENT_OUT);
  gfc_commit_symbol (result);

  // ADD_ARG (expr->symtree->name, base, BT_VOID, INTENT_IN);
  base = post_caf_ref_expr->symtree->n.sym;
  gfc_set_sym_referenced (base);
  gfc_commit_symbol (base);
  *argptr = gfc_get_formal_arglist ();
  (*argptr)->sym = base;
  argptr = &(*argptr)->next;
  gfc_commit_symbol (base);
#undef ADD_ARG

  /* Set up code.  */
  /* Code: result = post_caf_ref_expr;  */
  code = sub_ns->code = gfc_get_code (EXEC_ASSIGN);
  code->loc = expr->where;
  code->expr1 = gfc_lval_expr_from_sym (result);
  swp = *expr;
  *expr = *swp.value.function.actual->expr;
  swp.value.function.actual->expr = nullptr;
  code->expr2 = gfc_copy_expr (&swp);
  code->expr2->value.function.actual->expr = post_caf_ref_expr;

  remove_caf_ref (code->expr2->value.function.actual->expr, true);
  add_data->ts.u.derived
    = create_caf_add_data_parameter_type (post_caf_ref_expr, ns, add_data);

  cb = gfc_lval_expr_from_sym (extproc);
  cb->ts.interface = extproc;

  if (caf_accessor_prepend)
    {
      gfc_code *c = caf_accessor_prepend;
      /* Find last in chain.  */
      for (; c->next; c = c->next)
	;
      c->next = sub_ns->code;
      sub_ns->code = caf_accessor_prepend;
    }
  caf_accessor_prepend = backup_caf_accessor_prepend;
  return cb;
}

static void
rewrite_caf_allocated (gfc_expr **e)
{
  gfc_expr *present_fn_expr, *present_hash_expr, *wrapper;

  present_fn_expr = create_allocated_callback (*e);

  present_hash_expr = gfc_get_expr ();
  present_hash_expr->expr_type = EXPR_CONSTANT;
  present_hash_expr->ts.type = BT_INTEGER;
  present_hash_expr->ts.kind = gfc_default_integer_kind;
  present_hash_expr->where = (*e)->where;
  mpz_init_set_ui (present_hash_expr->value.integer,
		   gfc_hash_value (present_fn_expr->symtree->n.sym));
  wrapper
    = gfc_build_intrinsic_call (gfc_current_ns,
				GFC_ISYM_CAF_IS_PRESENT_ON_REMOTE,
				"caf_is_present_on_remote", (*e)->where, 3, *e,
				present_hash_expr, present_fn_expr);
  gfc_add_caf_accessor (present_hash_expr, present_fn_expr);
  *e = wrapper;
}

static gfc_expr *
create_send_callback (gfc_expr *expr, gfc_expr *rhs)
{
  gfc_namespace *ns;
  gfc_symbol *extproc, *proc, *buffer, *base, *send_data, *caller_image;
  char tname[GFC_MAX_SYMBOL_LEN + 1];
  char *name;
  const char *mname;
  gfc_expr *cb, *post_caf_ref_expr;
  gfc_code *code;
  gfc_code *backup_caf_accessor_prepend = caf_accessor_prepend;
  caf_accessor_prepend = nullptr;

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
  name = xasprintf ("_caf_accessor_%s_%s_%d", mname, tname, ++caf_sym_cnt);
  gfc_get_symbol (name, ns, &extproc);
  extproc->declared_at = expr->where;
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
  proc->declared_at = expr->where;
  gfc_commit_symbol (proc);
  free (name);

  split_expr_at_caf_ref (expr, sub_ns, &post_caf_ref_expr, true);

  if (ns->proc_name->attr.flavor == FL_MODULE)
    proc->module = ns->proc_name->name;
  gfc_set_sym_referenced (proc);
  /* Set up formal arguments.  */
  gfc_formal_arglist **argptr = &proc->formal;
#define ADD_ARG(name, nsym, stype, skind, sintent)                             \
  gfc_get_symbol (name, sub_ns, &nsym);                                        \
  nsym->ts.type = stype;                                                       \
  nsym->ts.kind = skind;                                                       \
  nsym->attr.flavor = FL_PARAMETER;                                            \
  nsym->attr.dummy = 1;                                                        \
  nsym->attr.intent = sintent;                                                 \
  nsym->declared_at = expr->where;                                             \
  gfc_set_sym_referenced (nsym);                                               \
  *argptr = gfc_get_formal_arglist ();                                         \
  (*argptr)->sym = nsym;                                                       \
  argptr = &(*argptr)->next

  name = xasprintf ("add_send_data_%s_%s_%d", mname, tname, caf_sym_cnt);
  ADD_ARG (name, send_data, BT_DERIVED, 0, INTENT_IN);
  gfc_commit_symbol (send_data);
  free (name);

  ADD_ARG ("caller_image", caller_image, BT_INTEGER, gfc_default_integer_kind,
	   INTENT_IN);
  gfc_commit_symbol (caller_image);

  // ADD_ARG (expr->symtree->name, base, BT_VOID, INTENT_IN);
  base = post_caf_ref_expr->symtree->n.sym;
  base->attr.intent = INTENT_INOUT;
  gfc_set_sym_referenced (base);
  gfc_commit_symbol (base);
  *argptr = gfc_get_formal_arglist ();
  (*argptr)->sym = base;
  argptr = &(*argptr)->next;
  gfc_commit_symbol (base);

  ADD_ARG ("buffer", buffer, rhs->ts.type, rhs->ts.kind, INTENT_IN);
  buffer->ts = rhs->ts;
  if (rhs->rank)
    {
      buffer->as = gfc_get_array_spec ();
      buffer->as->rank = rhs->rank;
      buffer->as->type = AS_DEFERRED;
      buffer->attr.allocatable = 1;
      buffer->attr.dimension = 1;
    }
  if (buffer->ts.type == BT_CHARACTER)
    {
      buffer->ts.u.cl = gfc_get_charlen ();
      *buffer->ts.u.cl = *rhs->ts.u.cl;
      buffer->ts.deferred = 1;
      buffer->ts.u.cl->length = gfc_copy_expr (rhs->ts.u.cl->length);
    }
  gfc_commit_symbol (buffer);
#undef ADD_ARG

  /* Set up code.  */
  /* Code: base = buffer;  */
  code = sub_ns->code = gfc_get_code (EXEC_ASSIGN);
  code->loc = expr->where;
  code->expr1 = post_caf_ref_expr;
  if (code->expr1->ts.type == BT_CHARACTER
      && code->expr1->ts.kind != buffer->ts.kind)
    {
      bool converted;
      code->expr2 = gfc_lval_expr_from_sym (buffer);
      converted = gfc_convert_chartype (code->expr2, &code->expr1->ts);
      gcc_assert (converted);
    }
  else if (code->expr1->ts.type != buffer->ts.type)
    {
      bool converted;
      code->expr2 = gfc_lval_expr_from_sym (buffer);
      converted = gfc_convert_type_warn (code->expr2, &code->expr1->ts, 0, 0,
					 buffer->attr.dimension);
      gcc_assert (converted);
    }
  else
    code->expr2 = gfc_lval_expr_from_sym (buffer);
  remove_caf_ref (post_caf_ref_expr);
  send_data->ts.u.derived
    = create_caf_add_data_parameter_type (code->expr1, ns, send_data);

  cb = gfc_lval_expr_from_sym (extproc);
  cb->ts.interface = extproc;

  if (caf_accessor_prepend)
    {
      gfc_code *c = caf_accessor_prepend;
      /* Find last in chain.  */
      for (; c->next; c = c->next)
	;
      c->next = sub_ns->code;
      sub_ns->code = caf_accessor_prepend;
    }
  caf_accessor_prepend = backup_caf_accessor_prepend;
  return cb;
}

static void
rewrite_caf_send (gfc_code *c)
{
  gfc_expr *send_to_remote_expr, *send_to_remote_hash_expr, *lhs, *rhs;
  gfc_actual_arglist *arg = c->ext.actual;

  lhs = arg->expr;
  arg = arg->next;
  rhs = arg->expr;
  /* Detect an already rewritten caf_send.  */
  if (arg->next && arg->next->expr->expr_type == EXPR_CONSTANT
      && arg->next->next && arg->next->next->expr->expr_type == EXPR_VARIABLE)
    return;

  send_to_remote_expr = create_send_callback (lhs, rhs);
  send_to_remote_hash_expr = gfc_get_expr ();
  send_to_remote_hash_expr->expr_type = EXPR_CONSTANT;
  send_to_remote_hash_expr->ts.type = BT_INTEGER;
  send_to_remote_hash_expr->ts.kind = gfc_default_integer_kind;
  send_to_remote_hash_expr->where = lhs->where;
  mpz_init_set_ui (send_to_remote_hash_expr->value.integer,
		   gfc_hash_value (send_to_remote_expr->symtree->n.sym));
  arg->next = gfc_get_actual_arglist ();
  arg = arg->next;
  arg->expr = send_to_remote_hash_expr;
  arg->next = gfc_get_actual_arglist ();
  arg = arg->next;
  arg->expr = send_to_remote_expr;
  gfc_add_caf_accessor (send_to_remote_hash_expr, send_to_remote_expr);

  if (gfc_is_coindexed (rhs))
    {
      gfc_expr *get_from_remote_expr, *get_from_remote_hash_expr;

      c->resolved_isym = gfc_intrinsic_subroutine_by_id (GFC_ISYM_CAF_SENDGET);
      get_from_remote_expr = create_get_callback (rhs);
      get_from_remote_hash_expr = gfc_get_expr ();
      get_from_remote_hash_expr->expr_type = EXPR_CONSTANT;
      get_from_remote_hash_expr->ts.type = BT_INTEGER;
      get_from_remote_hash_expr->ts.kind = gfc_default_integer_kind;
      get_from_remote_hash_expr->where = rhs->where;
      mpz_init_set_ui (get_from_remote_hash_expr->value.integer,
		       gfc_hash_value (get_from_remote_expr->symtree->n.sym));
      arg->next = gfc_get_actual_arglist ();
      arg = arg->next;
      arg->expr = get_from_remote_hash_expr;
      arg->next = gfc_get_actual_arglist ();
      arg = arg->next;
      arg->expr = get_from_remote_expr;
      gfc_add_caf_accessor (get_from_remote_hash_expr, get_from_remote_expr);
    }
}

static int
coindexed_expr_callback (gfc_expr **e, int *walk_subtrees,
			 void *data ATTRIBUTE_UNUSED)
{
  *walk_subtrees = 1;

  switch ((*e)->expr_type)
    {
    case EXPR_VARIABLE:
      if (!caf_on_lhs && gfc_is_coindexed (*e))
	{
	  add_caf_get_from_remote (*e);
	  *walk_subtrees = 0;
	}
      /* Clear the flag to rewrite caf_gets in sub expressions of the lhs.  */
      caf_on_lhs = false;
      break;
    case EXPR_FUNCTION:
      if ((*e)->value.function.isym)
	switch ((*e)->value.function.isym->id)
	  {
	  case GFC_ISYM_ALLOCATED:
	    if ((*e)->value.function.actual->expr
		&& gfc_is_coindexed ((*e)->value.function.actual->expr))
	      {
		rewrite_caf_allocated (e);
		*walk_subtrees = 0;
	      }
	    break;
	  case GFC_ISYM_CAF_GET:
	  case GFC_ISYM_CAF_IS_PRESENT_ON_REMOTE:
	    *walk_subtrees = 0;
	    break;
	  default:
	    break;
	  }
    default:
      break;
    }

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
    case EXEC_CALL:
      *walk_subtrees = 1;
      if ((*c)->resolved_isym)
	switch ((*c)->resolved_isym->id)
	  {
	  case GFC_ISYM_CAF_SEND:
	    rewrite_caf_send (*c);
	    *walk_subtrees = 0;
	    break;
	  case GFC_ISYM_CAF_SENDGET:
	    /* Seldomly this routine is called again with the symbol already
	       changed to CAF_SENDGET.  Do not process the subtree again.  The
	       rewrite has already been done by rewrite_caf_send ().  */
	    *walk_subtrees = 0;
	    break;
	  case GFC_ISYM_ATOMIC_ADD:
	  case GFC_ISYM_ATOMIC_AND:
	  case GFC_ISYM_ATOMIC_CAS:
	  case GFC_ISYM_ATOMIC_DEF:
	  case GFC_ISYM_ATOMIC_FETCH_ADD:
	  case GFC_ISYM_ATOMIC_FETCH_AND:
	  case GFC_ISYM_ATOMIC_FETCH_OR:
	  case GFC_ISYM_ATOMIC_FETCH_XOR:
	  case GFC_ISYM_ATOMIC_OR:
	  case GFC_ISYM_ATOMIC_REF:
	  case GFC_ISYM_ATOMIC_XOR:
	    *walk_subtrees = 0;
	    break;
	  default:
	    break;
	  }
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
    {
      gfc_code_walker (&ns->code, coindexed_code_callback,
		       coindexed_expr_callback, NULL);

      for (gfc_namespace *cns = ns->contained; cns; cns = cns->sibling)
	gfc_coarray_rewrite (cns);
    }

  gfc_current_ns = saved_ns;
}
