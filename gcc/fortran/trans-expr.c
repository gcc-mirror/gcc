/* Expression translation
   Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010,
   2011, 2012
   Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>
   and Steven Bosscher <s.bosscher@student.tudelft.nl>

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

/* trans-expr.c-- generate GENERIC trees for gfc_expr.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "diagnostic-core.h"	/* For fatal_error.  */
#include "langhooks.h"
#include "flags.h"
#include "gfortran.h"
#include "arith.h"
#include "constructor.h"
#include "trans.h"
#include "trans-const.h"
#include "trans-types.h"
#include "trans-array.h"
/* Only for gfc_trans_assign and gfc_trans_pointer_assign.  */
#include "trans-stmt.h"
#include "dependency.h"


/* This is the seed for an eventual trans-class.c

   The following parameters should not be used directly since they might
   in future implementations.  Use the corresponding APIs.  */
#define CLASS_DATA_FIELD 0
#define CLASS_VPTR_FIELD 1
#define VTABLE_HASH_FIELD 0
#define VTABLE_SIZE_FIELD 1
#define VTABLE_EXTENDS_FIELD 2
#define VTABLE_DEF_INIT_FIELD 3
#define VTABLE_COPY_FIELD 4


tree
gfc_class_data_get (tree decl)
{
  tree data;
  if (POINTER_TYPE_P (TREE_TYPE (decl)))
    decl = build_fold_indirect_ref_loc (input_location, decl);
  data = gfc_advance_chain (TYPE_FIELDS (TREE_TYPE (decl)),
			    CLASS_DATA_FIELD);
  return fold_build3_loc (input_location, COMPONENT_REF,
			  TREE_TYPE (data), decl, data,
			  NULL_TREE);
}


tree
gfc_class_vptr_get (tree decl)
{
  tree vptr;
  if (POINTER_TYPE_P (TREE_TYPE (decl)))
    decl = build_fold_indirect_ref_loc (input_location, decl);
  vptr = gfc_advance_chain (TYPE_FIELDS (TREE_TYPE (decl)),
			    CLASS_VPTR_FIELD);
  return fold_build3_loc (input_location, COMPONENT_REF,
			  TREE_TYPE (vptr), decl, vptr,
			  NULL_TREE);
}


static tree
gfc_vtable_field_get (tree decl, int field)
{
  tree size;
  tree vptr;
  vptr = gfc_class_vptr_get (decl);
  vptr = build_fold_indirect_ref_loc (input_location, vptr);
  size = gfc_advance_chain (TYPE_FIELDS (TREE_TYPE (vptr)),
			    field);
  size = fold_build3_loc (input_location, COMPONENT_REF,
			  TREE_TYPE (size), vptr, size,
			  NULL_TREE);
  /* Always return size as an array index type.  */
  if (field == VTABLE_SIZE_FIELD)
    size = fold_convert (gfc_array_index_type, size);
  gcc_assert (size);
  return size;
}


tree
gfc_vtable_hash_get (tree decl)
{
  return gfc_vtable_field_get (decl, VTABLE_HASH_FIELD);
}


tree
gfc_vtable_size_get (tree decl)
{
  return gfc_vtable_field_get (decl, VTABLE_SIZE_FIELD);
}


tree
gfc_vtable_extends_get (tree decl)
{
  return gfc_vtable_field_get (decl, VTABLE_EXTENDS_FIELD);
}


tree
gfc_vtable_def_init_get (tree decl)
{
  return gfc_vtable_field_get (decl, VTABLE_DEF_INIT_FIELD);
}


tree
gfc_vtable_copy_get (tree decl)
{
  return gfc_vtable_field_get (decl, VTABLE_COPY_FIELD);
}


#undef CLASS_DATA_FIELD
#undef CLASS_VPTR_FIELD
#undef VTABLE_HASH_FIELD
#undef VTABLE_SIZE_FIELD
#undef VTABLE_EXTENDS_FIELD
#undef VTABLE_DEF_INIT_FIELD
#undef VTABLE_COPY_FIELD


/* Takes a derived type expression and returns the address of a temporary
   class object of the 'declared' type.  */ 
static void
gfc_conv_derived_to_class (gfc_se *parmse, gfc_expr *e,
			   gfc_typespec class_ts)
{
  gfc_symbol *vtab;
  gfc_ss *ss;
  tree ctree;
  tree var;
  tree tmp;

  /* The derived type needs to be converted to a temporary
     CLASS object.  */
  tmp = gfc_typenode_for_spec (&class_ts);
  var = gfc_create_var (tmp, "class");

  /* Set the vptr.  */
  ctree =  gfc_class_vptr_get (var);

  /* Remember the vtab corresponds to the derived type
     not to the class declared type.  */
  vtab = gfc_find_derived_vtab (e->ts.u.derived);
  gcc_assert (vtab);
  tmp = gfc_build_addr_expr (NULL_TREE, gfc_get_symbol_decl (vtab));
  gfc_add_modify (&parmse->pre, ctree,
		  fold_convert (TREE_TYPE (ctree), tmp));

  /* Now set the data field.  */
  ctree =  gfc_class_data_get (var);

  if (parmse->ss && parmse->ss->info->useflags)
    {
      /* For an array reference in an elemental procedure call we need
	 to retain the ss to provide the scalarized array reference.  */
      gfc_conv_expr_reference (parmse, e);
      tmp = fold_convert (TREE_TYPE (ctree), parmse->expr);
      gfc_add_modify (&parmse->pre, ctree, tmp);
    }
  else
    {
      ss = gfc_walk_expr (e);
      if (ss == gfc_ss_terminator)
	{
	  parmse->ss = NULL;
	  gfc_conv_expr_reference (parmse, e);
	  tmp = fold_convert (TREE_TYPE (ctree), parmse->expr);
	  gfc_add_modify (&parmse->pre, ctree, tmp);
	}
      else
	{
	  parmse->ss = ss;
	  gfc_conv_expr_descriptor (parmse, e, ss);
	  gfc_add_modify (&parmse->pre, ctree, parmse->expr);
	}
    }

  /* Pass the address of the class object.  */
  parmse->expr = gfc_build_addr_expr (NULL_TREE, var);
}


/* Takes a scalarized class array expression and returns the
   address of a temporary scalar class object of the 'declared'
   type.  
   OOP-TODO: This could be improved by adding code that branched on
   the dynamic type being the same as the declared type. In this case
   the original class expression can be passed directly.  */ 
static void
gfc_conv_class_to_class (gfc_se *parmse, gfc_expr *e,
			 gfc_typespec class_ts, bool elemental)
{
  tree ctree;
  tree var;
  tree tmp;
  tree vptr;
  gfc_ref *ref;
  gfc_ref *class_ref;
  bool full_array = false;

  class_ref = NULL;
  for (ref = e->ref; ref; ref = ref->next)
    {
      if (ref->type == REF_COMPONENT
	    && ref->u.c.component->ts.type == BT_CLASS)
	class_ref = ref;

      if (ref->next == NULL)
	break;
    }

  if (ref == NULL || class_ref == ref)
    return;

  /* Test for FULL_ARRAY.  */
  gfc_is_class_array_ref (e, &full_array);

  /* The derived type needs to be converted to a temporary
     CLASS object.  */
  tmp = gfc_typenode_for_spec (&class_ts);
  var = gfc_create_var (tmp, "class");

  /* Set the data.  */
  ctree = gfc_class_data_get (var);
  gfc_add_modify (&parmse->pre, ctree, parmse->expr);

  /* Return the data component, except in the case of scalarized array
     references, where nullification of the cannot occur and so there
     is no need.  */
  if (!elemental && full_array)
    gfc_add_modify (&parmse->post, parmse->expr, ctree);

  /* Set the vptr.  */
  ctree = gfc_class_vptr_get (var);

  /* The vptr is the second field of the actual argument.
     First we have to find the corresponding class reference. */

  tmp = NULL_TREE;
  if (class_ref == NULL
	&& e->symtree && e->symtree->n.sym->ts.type == BT_CLASS) 
    tmp = e->symtree->n.sym->backend_decl;
  else
    {
      /* Remove everything after the last class reference, convert the
	 expression and then recover its tailend once more.  */
      gfc_se tmpse;
      ref = class_ref->next;
      class_ref->next = NULL;
      gfc_init_se (&tmpse, NULL);
      gfc_conv_expr (&tmpse, e);
      class_ref->next = ref;
      tmp = tmpse.expr;
    }

  gcc_assert (tmp != NULL_TREE);

  /* Dereference if needs be.  */
  if (TREE_CODE (TREE_TYPE (tmp)) == REFERENCE_TYPE)
    tmp = build_fold_indirect_ref_loc (input_location, tmp);

  vptr = gfc_class_vptr_get (tmp);
  gfc_add_modify (&parmse->pre, ctree,
		  fold_convert (TREE_TYPE (ctree), vptr));

  /* Return the vptr component, except in the case of scalarized array
     references, where the dynamic type cannot change.  */
  if (!elemental && full_array)
    gfc_add_modify (&parmse->post, vptr,
		    fold_convert (TREE_TYPE (vptr), ctree));

  /* Pass the address of the class object.  */
  parmse->expr = gfc_build_addr_expr (NULL_TREE, var);
}


static tree
gfc_trans_class_array_init_assign (gfc_expr *rhs, gfc_expr *lhs, gfc_expr *obj)
{
  gfc_actual_arglist *actual;
  gfc_expr *ppc;
  gfc_code *ppc_code;
  tree res;

  actual = gfc_get_actual_arglist ();
  actual->expr = gfc_copy_expr (rhs);
  actual->next = gfc_get_actual_arglist ();
  actual->next->expr = gfc_copy_expr (lhs);
  ppc = gfc_copy_expr (obj);
  gfc_add_vptr_component (ppc);
  gfc_add_component_ref (ppc, "_copy");
  ppc_code = gfc_get_code ();
  ppc_code->resolved_sym = ppc->symtree->n.sym;
  /* Although '_copy' is set to be elemental in class.c, it is
     not staying that way.  Find out why, sometime....  */
  ppc_code->resolved_sym->attr.elemental = 1;
  ppc_code->ext.actual = actual;
  ppc_code->expr1 = ppc;
  ppc_code->op = EXEC_CALL;
  /* Since '_copy' is elemental, the scalarizer will take care
     of arrays in gfc_trans_call.  */
  res = gfc_trans_call (ppc_code, false, NULL, NULL, false);
  gfc_free_statements (ppc_code);
  return res;
}

/* Special case for initializing a polymorphic dummy with INTENT(OUT).
   A MEMCPY is needed to copy the full data from the default initializer
   of the dynamic type.  */

tree
gfc_trans_class_init_assign (gfc_code *code)
{
  stmtblock_t block;
  tree tmp;
  gfc_se dst,src,memsz;
  gfc_expr *lhs, *rhs, *sz;

  gfc_start_block (&block);

  lhs = gfc_copy_expr (code->expr1);
  gfc_add_data_component (lhs);

  rhs = gfc_copy_expr (code->expr1);
  gfc_add_vptr_component (rhs);

  /* Make sure that the component backend_decls have been built, which
     will not have happened if the derived types concerned have not
     been referenced.  */
  gfc_get_derived_type (rhs->ts.u.derived);
  gfc_add_def_init_component (rhs);

  if (code->expr1->ts.type == BT_CLASS
	&& CLASS_DATA (code->expr1)->attr.dimension)
    tmp = gfc_trans_class_array_init_assign (rhs, lhs, code->expr1);
  else
    {
      sz = gfc_copy_expr (code->expr1);
      gfc_add_vptr_component (sz);
      gfc_add_size_component (sz);

      gfc_init_se (&dst, NULL);
      gfc_init_se (&src, NULL);
      gfc_init_se (&memsz, NULL);
      gfc_conv_expr (&dst, lhs);
      gfc_conv_expr (&src, rhs);
      gfc_conv_expr (&memsz, sz);
      gfc_add_block_to_block (&block, &src.pre);
      tmp = gfc_build_memcpy_call (dst.expr, src.expr, memsz.expr);
    }
  gfc_add_expr_to_block (&block, tmp);
  
  return gfc_finish_block (&block);
}


/* Translate an assignment to a CLASS object
   (pointer or ordinary assignment).  */

tree
gfc_trans_class_assign (gfc_expr *expr1, gfc_expr *expr2, gfc_exec_op op)
{
  stmtblock_t block;
  tree tmp;
  gfc_expr *lhs;
  gfc_expr *rhs;
  gfc_ref *ref;

  gfc_start_block (&block);

  ref = expr1->ref;
  while (ref && ref->next)
     ref = ref->next;

  /* Class valued proc_pointer assignments do not need any further
     preparation.  */
  if (ref && ref->type == REF_COMPONENT
	&& ref->u.c.component->attr.proc_pointer
	&& expr2->expr_type == EXPR_VARIABLE
	&& expr2->symtree->n.sym->attr.flavor == FL_PROCEDURE
	&& op == EXEC_POINTER_ASSIGN)
    goto assign;

  if (expr2->ts.type != BT_CLASS)
    {
      /* Insert an additional assignment which sets the '_vptr' field.  */
      gfc_symbol *vtab = NULL;
      gfc_symtree *st;

      lhs = gfc_copy_expr (expr1);
      gfc_add_vptr_component (lhs);

      if (expr2->ts.type == BT_DERIVED)
	vtab = gfc_find_derived_vtab (expr2->ts.u.derived);
      else if (expr2->expr_type == EXPR_NULL)
	vtab = gfc_find_derived_vtab (expr1->ts.u.derived);
      gcc_assert (vtab);

      rhs = gfc_get_expr ();
      rhs->expr_type = EXPR_VARIABLE;
      gfc_find_sym_tree (vtab->name, vtab->ns, 1, &st);
      rhs->symtree = st;
      rhs->ts = vtab->ts;

      tmp = gfc_trans_pointer_assignment (lhs, rhs);
      gfc_add_expr_to_block (&block, tmp);

      gfc_free_expr (lhs);
      gfc_free_expr (rhs);
    }
  else if (CLASS_DATA (expr2)->attr.dimension)
    {
      /* Insert an additional assignment which sets the '_vptr' field.  */
      lhs = gfc_copy_expr (expr1);
      gfc_add_vptr_component (lhs);

      rhs = gfc_copy_expr (expr2);
      gfc_add_vptr_component (rhs);

      tmp = gfc_trans_pointer_assignment (lhs, rhs);
      gfc_add_expr_to_block (&block, tmp);

      gfc_free_expr (lhs);
      gfc_free_expr (rhs);
    }

  /* Do the actual CLASS assignment.  */
  if (expr2->ts.type == BT_CLASS
	&& !CLASS_DATA (expr2)->attr.dimension)
    op = EXEC_ASSIGN;
  else
    gfc_add_data_component (expr1);

assign:

  if (op == EXEC_ASSIGN)
    tmp = gfc_trans_assignment (expr1, expr2, false, true);
  else if (op == EXEC_POINTER_ASSIGN)
    tmp = gfc_trans_pointer_assignment (expr1, expr2);
  else
    gcc_unreachable();

  gfc_add_expr_to_block (&block, tmp);

  return gfc_finish_block (&block);
}


/* End of prototype trans-class.c  */


static tree gfc_trans_structure_assign (tree dest, gfc_expr * expr);
static void gfc_apply_interface_mapping_to_expr (gfc_interface_mapping *,
						 gfc_expr *);

/* Copy the scalarization loop variables.  */

static void
gfc_copy_se_loopvars (gfc_se * dest, gfc_se * src)
{
  dest->ss = src->ss;
  dest->loop = src->loop;
}


/* Initialize a simple expression holder.

   Care must be taken when multiple se are created with the same parent.
   The child se must be kept in sync.  The easiest way is to delay creation
   of a child se until after after the previous se has been translated.  */

void
gfc_init_se (gfc_se * se, gfc_se * parent)
{
  memset (se, 0, sizeof (gfc_se));
  gfc_init_block (&se->pre);
  gfc_init_block (&se->post);

  se->parent = parent;

  if (parent)
    gfc_copy_se_loopvars (se, parent);
}


/* Advances to the next SS in the chain.  Use this rather than setting
   se->ss = se->ss->next because all the parents needs to be kept in sync.
   See gfc_init_se.  */

void
gfc_advance_se_ss_chain (gfc_se * se)
{
  gfc_se *p;
  gfc_ss *ss;

  gcc_assert (se != NULL && se->ss != NULL && se->ss != gfc_ss_terminator);

  p = se;
  /* Walk down the parent chain.  */
  while (p != NULL)
    {
      /* Simple consistency check.  */
      gcc_assert (p->parent == NULL || p->parent->ss == p->ss
		  || p->parent->ss->nested_ss == p->ss);

      /* If we were in a nested loop, the next scalarized expression can be
	 on the parent ss' next pointer.  Thus we should not take the next
	 pointer blindly, but rather go up one nest level as long as next
	 is the end of chain.  */
      ss = p->ss;
      while (ss->next == gfc_ss_terminator && ss->parent != NULL)
	ss = ss->parent;

      p->ss = ss->next;

      p = p->parent;
    }
}


/* Ensures the result of the expression as either a temporary variable
   or a constant so that it can be used repeatedly.  */

void
gfc_make_safe_expr (gfc_se * se)
{
  tree var;

  if (CONSTANT_CLASS_P (se->expr))
    return;

  /* We need a temporary for this result.  */
  var = gfc_create_var (TREE_TYPE (se->expr), NULL);
  gfc_add_modify (&se->pre, var, se->expr);
  se->expr = var;
}


/* Return an expression which determines if a dummy parameter is present.
   Also used for arguments to procedures with multiple entry points.  */

tree
gfc_conv_expr_present (gfc_symbol * sym)
{
  tree decl, cond;

  gcc_assert (sym->attr.dummy);

  decl = gfc_get_symbol_decl (sym);
  if (TREE_CODE (decl) != PARM_DECL)
    {
      /* Array parameters use a temporary descriptor, we want the real
         parameter.  */
      gcc_assert (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (decl))
             || GFC_ARRAY_TYPE_P (TREE_TYPE (decl)));
      decl = GFC_DECL_SAVED_DESCRIPTOR (decl);
    }

  cond = fold_build2_loc (input_location, NE_EXPR, boolean_type_node, decl,
			  fold_convert (TREE_TYPE (decl), null_pointer_node));

  /* Fortran 2008 allows to pass null pointers and non-associated pointers
     as actual argument to denote absent dummies. For array descriptors,
     we thus also need to check the array descriptor.  */
  if (!sym->attr.pointer && !sym->attr.allocatable
      && sym->as && sym->as->type == AS_ASSUMED_SHAPE
      && (gfc_option.allow_std & GFC_STD_F2008) != 0)
    {
      tree tmp;
      tmp = build_fold_indirect_ref_loc (input_location, decl);
      tmp = gfc_conv_array_data (tmp);
      tmp = fold_build2_loc (input_location, NE_EXPR, boolean_type_node, tmp,
			     fold_convert (TREE_TYPE (tmp), null_pointer_node));
      cond = fold_build2_loc (input_location, TRUTH_ANDIF_EXPR,
			      boolean_type_node, cond, tmp);
    }

  return cond;
}


/* Converts a missing, dummy argument into a null or zero.  */

void
gfc_conv_missing_dummy (gfc_se * se, gfc_expr * arg, gfc_typespec ts, int kind)
{
  tree present;
  tree tmp;

  present = gfc_conv_expr_present (arg->symtree->n.sym);

  if (kind > 0)
    {
      /* Create a temporary and convert it to the correct type.  */
      tmp = gfc_get_int_type (kind);
      tmp = fold_convert (tmp, build_fold_indirect_ref_loc (input_location,
							se->expr));
    
      /* Test for a NULL value.  */
      tmp = build3_loc (input_location, COND_EXPR, TREE_TYPE (tmp), present,
			tmp, fold_convert (TREE_TYPE (tmp), integer_one_node));
      tmp = gfc_evaluate_now (tmp, &se->pre);
      se->expr = gfc_build_addr_expr (NULL_TREE, tmp);
    }
  else
    {
      tmp = build3_loc (input_location, COND_EXPR, TREE_TYPE (se->expr),
			present, se->expr,
			build_zero_cst (TREE_TYPE (se->expr)));
      tmp = gfc_evaluate_now (tmp, &se->pre);
      se->expr = tmp;
    }

  if (ts.type == BT_CHARACTER)
    {
      tmp = build_int_cst (gfc_charlen_type_node, 0);
      tmp = fold_build3_loc (input_location, COND_EXPR, gfc_charlen_type_node,
			     present, se->string_length, tmp);
      tmp = gfc_evaluate_now (tmp, &se->pre);
      se->string_length = tmp;
    }
  return;
}


/* Get the character length of an expression, looking through gfc_refs
   if necessary.  */

tree
gfc_get_expr_charlen (gfc_expr *e)
{
  gfc_ref *r;
  tree length;

  gcc_assert (e->expr_type == EXPR_VARIABLE 
	      && e->ts.type == BT_CHARACTER);
  
  length = NULL; /* To silence compiler warning.  */

  if (is_subref_array (e) && e->ts.u.cl->length)
    {
      gfc_se tmpse;
      gfc_init_se (&tmpse, NULL);
      gfc_conv_expr_type (&tmpse, e->ts.u.cl->length, gfc_charlen_type_node);
      e->ts.u.cl->backend_decl = tmpse.expr;
      return tmpse.expr;
    }

  /* First candidate: if the variable is of type CHARACTER, the
     expression's length could be the length of the character
     variable.  */
  if (e->symtree->n.sym->ts.type == BT_CHARACTER)
    length = e->symtree->n.sym->ts.u.cl->backend_decl;

  /* Look through the reference chain for component references.  */
  for (r = e->ref; r; r = r->next)
    {
      switch (r->type)
	{
	case REF_COMPONENT:
	  if (r->u.c.component->ts.type == BT_CHARACTER)
	    length = r->u.c.component->ts.u.cl->backend_decl;
	  break;

	case REF_ARRAY:
	  /* Do nothing.  */
	  break;

	default:
	  /* We should never got substring references here.  These will be
	     broken down by the scalarizer.  */
	  gcc_unreachable ();
	  break;
	}
    }

  gcc_assert (length != NULL);
  return length;
}


/* Return for an expression the backend decl of the coarray.  */

static tree
get_tree_for_caf_expr (gfc_expr *expr)
{
   tree caf_decl = NULL_TREE;
   gfc_ref *ref;

   gcc_assert (expr && expr->expr_type == EXPR_VARIABLE);
   if (expr->symtree->n.sym->attr.codimension)
     caf_decl = expr->symtree->n.sym->backend_decl;

   for (ref = expr->ref; ref; ref = ref->next)
     if (ref->type == REF_COMPONENT)
       {
	gfc_component *comp = ref->u.c.component;
        if (comp->attr.pointer || comp->attr.allocatable)
	  caf_decl = NULL_TREE;
	if (comp->attr.codimension)
	  caf_decl = comp->backend_decl;
       }

   gcc_assert (caf_decl != NULL_TREE);
   return caf_decl;
}


/* For each character array constructor subexpression without a ts.u.cl->length,
   replace it by its first element (if there aren't any elements, the length
   should already be set to zero).  */

static void
flatten_array_ctors_without_strlen (gfc_expr* e)
{
  gfc_actual_arglist* arg;
  gfc_constructor* c;

  if (!e)
    return;

  switch (e->expr_type)
    {

    case EXPR_OP:
      flatten_array_ctors_without_strlen (e->value.op.op1); 
      flatten_array_ctors_without_strlen (e->value.op.op2); 
      break;

    case EXPR_COMPCALL:
      /* TODO: Implement as with EXPR_FUNCTION when needed.  */
      gcc_unreachable ();

    case EXPR_FUNCTION:
      for (arg = e->value.function.actual; arg; arg = arg->next)
	flatten_array_ctors_without_strlen (arg->expr);
      break;

    case EXPR_ARRAY:

      /* We've found what we're looking for.  */
      if (e->ts.type == BT_CHARACTER && !e->ts.u.cl->length)
	{
	  gfc_constructor *c;
	  gfc_expr* new_expr;

	  gcc_assert (e->value.constructor);

	  c = gfc_constructor_first (e->value.constructor);
	  new_expr = c->expr;
	  c->expr = NULL;

	  flatten_array_ctors_without_strlen (new_expr);
	  gfc_replace_expr (e, new_expr);
	  break;
	}

      /* Otherwise, fall through to handle constructor elements.  */
    case EXPR_STRUCTURE:
      for (c = gfc_constructor_first (e->value.constructor);
	   c; c = gfc_constructor_next (c))
	flatten_array_ctors_without_strlen (c->expr);
      break;

    default:
      break;

    }
}


/* Generate code to initialize a string length variable. Returns the
   value.  For array constructors, cl->length might be NULL and in this case,
   the first element of the constructor is needed.  expr is the original
   expression so we can access it but can be NULL if this is not needed.  */

void
gfc_conv_string_length (gfc_charlen * cl, gfc_expr * expr, stmtblock_t * pblock)
{
  gfc_se se;

  gfc_init_se (&se, NULL);

  if (!cl->length
	&& cl->backend_decl
	&& TREE_CODE (cl->backend_decl) == VAR_DECL)
    return;

  /* If cl->length is NULL, use gfc_conv_expr to obtain the string length but
     "flatten" array constructors by taking their first element; all elements
     should be the same length or a cl->length should be present.  */
  if (!cl->length)
    {
      gfc_expr* expr_flat;
      gcc_assert (expr);
      expr_flat = gfc_copy_expr (expr);
      flatten_array_ctors_without_strlen (expr_flat);
      gfc_resolve_expr (expr_flat);

      gfc_conv_expr (&se, expr_flat);
      gfc_add_block_to_block (pblock, &se.pre);
      cl->backend_decl = convert (gfc_charlen_type_node, se.string_length);

      gfc_free_expr (expr_flat);
      return;
    }

  /* Convert cl->length.  */

  gcc_assert (cl->length);

  gfc_conv_expr_type (&se, cl->length, gfc_charlen_type_node);
  se.expr = fold_build2_loc (input_location, MAX_EXPR, gfc_charlen_type_node,
			     se.expr, build_int_cst (gfc_charlen_type_node, 0));
  gfc_add_block_to_block (pblock, &se.pre);

  if (cl->backend_decl)
    gfc_add_modify (pblock, cl->backend_decl, se.expr);
  else
    cl->backend_decl = gfc_evaluate_now (se.expr, pblock);
}


static void
gfc_conv_substring (gfc_se * se, gfc_ref * ref, int kind,
		    const char *name, locus *where)
{
  tree tmp;
  tree type;
  tree fault;
  gfc_se start;
  gfc_se end;
  char *msg;

  type = gfc_get_character_type (kind, ref->u.ss.length);
  type = build_pointer_type (type);

  gfc_init_se (&start, se);
  gfc_conv_expr_type (&start, ref->u.ss.start, gfc_charlen_type_node);
  gfc_add_block_to_block (&se->pre, &start.pre);

  if (integer_onep (start.expr))
    gfc_conv_string_parameter (se);
  else
    {
      tmp = start.expr;
      STRIP_NOPS (tmp);
      /* Avoid multiple evaluation of substring start.  */
      if (!CONSTANT_CLASS_P (tmp) && !DECL_P (tmp))
	start.expr = gfc_evaluate_now (start.expr, &se->pre);

      /* Change the start of the string.  */
      if (TYPE_STRING_FLAG (TREE_TYPE (se->expr)))
	tmp = se->expr;
      else
	tmp = build_fold_indirect_ref_loc (input_location,
				       se->expr);
      tmp = gfc_build_array_ref (tmp, start.expr, NULL);
      se->expr = gfc_build_addr_expr (type, tmp);
    }

  /* Length = end + 1 - start.  */
  gfc_init_se (&end, se);
  if (ref->u.ss.end == NULL)
    end.expr = se->string_length;
  else
    {
      gfc_conv_expr_type (&end, ref->u.ss.end, gfc_charlen_type_node);
      gfc_add_block_to_block (&se->pre, &end.pre);
    }
  tmp = end.expr;
  STRIP_NOPS (tmp);
  if (!CONSTANT_CLASS_P (tmp) && !DECL_P (tmp))
    end.expr = gfc_evaluate_now (end.expr, &se->pre);

  if (gfc_option.rtcheck & GFC_RTCHECK_BOUNDS)
    {
      tree nonempty = fold_build2_loc (input_location, LE_EXPR,
				       boolean_type_node, start.expr,
				       end.expr);

      /* Check lower bound.  */
      fault = fold_build2_loc (input_location, LT_EXPR, boolean_type_node,
			       start.expr,
			       build_int_cst (gfc_charlen_type_node, 1));
      fault = fold_build2_loc (input_location, TRUTH_ANDIF_EXPR,
			       boolean_type_node, nonempty, fault);
      if (name)
	asprintf (&msg, "Substring out of bounds: lower bound (%%ld) of '%s' "
		  "is less than one", name);
      else
	asprintf (&msg, "Substring out of bounds: lower bound (%%ld)"
		  "is less than one");
      gfc_trans_runtime_check (true, false, fault, &se->pre, where, msg,
			       fold_convert (long_integer_type_node,
					     start.expr));
      free (msg);

      /* Check upper bound.  */
      fault = fold_build2_loc (input_location, GT_EXPR, boolean_type_node,
			       end.expr, se->string_length);
      fault = fold_build2_loc (input_location, TRUTH_ANDIF_EXPR,
			       boolean_type_node, nonempty, fault);
      if (name)
	asprintf (&msg, "Substring out of bounds: upper bound (%%ld) of '%s' "
		  "exceeds string length (%%ld)", name);
      else
	asprintf (&msg, "Substring out of bounds: upper bound (%%ld) "
		  "exceeds string length (%%ld)");
      gfc_trans_runtime_check (true, false, fault, &se->pre, where, msg,
			       fold_convert (long_integer_type_node, end.expr),
			       fold_convert (long_integer_type_node,
					     se->string_length));
      free (msg);
    }

  /* If the start and end expressions are equal, the length is one.  */
  if (ref->u.ss.end
      && gfc_dep_compare_expr (ref->u.ss.start, ref->u.ss.end) == 0)
    tmp = build_int_cst (gfc_charlen_type_node, 1);
  else
    {
      tmp = fold_build2_loc (input_location, MINUS_EXPR, gfc_charlen_type_node,
			     end.expr, start.expr);
      tmp = fold_build2_loc (input_location, PLUS_EXPR, gfc_charlen_type_node,
			     build_int_cst (gfc_charlen_type_node, 1), tmp);
      tmp = fold_build2_loc (input_location, MAX_EXPR, gfc_charlen_type_node,
			     tmp, build_int_cst (gfc_charlen_type_node, 0));
    }

  se->string_length = tmp;
}


/* Convert a derived type component reference.  */

static void
gfc_conv_component_ref (gfc_se * se, gfc_ref * ref)
{
  gfc_component *c;
  tree tmp;
  tree decl;
  tree field;

  c = ref->u.c.component;

  gcc_assert (c->backend_decl);

  field = c->backend_decl;
  gcc_assert (TREE_CODE (field) == FIELD_DECL);
  decl = se->expr;

  /* Components can correspond to fields of different containing
     types, as components are created without context, whereas
     a concrete use of a component has the type of decl as context.
     So, if the type doesn't match, we search the corresponding
     FIELD_DECL in the parent type.  To not waste too much time
     we cache this result in norestrict_decl.  */

  if (DECL_FIELD_CONTEXT (field) != TREE_TYPE (decl))
    {
      tree f2 = c->norestrict_decl;
      if (!f2 || DECL_FIELD_CONTEXT (f2) != TREE_TYPE (decl))
	for (f2 = TYPE_FIELDS (TREE_TYPE (decl)); f2; f2 = DECL_CHAIN (f2))
	  if (TREE_CODE (f2) == FIELD_DECL
	      && DECL_NAME (f2) == DECL_NAME (field))
	    break;
      gcc_assert (f2);
      c->norestrict_decl = f2;
      field = f2;
    }
  tmp = fold_build3_loc (input_location, COMPONENT_REF, TREE_TYPE (field),
			 decl, field, NULL_TREE);

  se->expr = tmp;

  if (c->ts.type == BT_CHARACTER && !c->attr.proc_pointer)
    {
      tmp = c->ts.u.cl->backend_decl;
      /* Components must always be constant length.  */
      gcc_assert (tmp && INTEGER_CST_P (tmp));
      se->string_length = tmp;
    }

  if (((c->attr.pointer || c->attr.allocatable)
       && (!c->attr.dimension && !c->attr.codimension)
       && c->ts.type != BT_CHARACTER)
      || c->attr.proc_pointer)
    se->expr = build_fold_indirect_ref_loc (input_location,
					se->expr);
}


/* This function deals with component references to components of the
   parent type for derived type extensons.  */
static void
conv_parent_component_references (gfc_se * se, gfc_ref * ref)
{
  gfc_component *c;
  gfc_component *cmp;
  gfc_symbol *dt;
  gfc_ref parent;

  dt = ref->u.c.sym;
  c = ref->u.c.component;

  /* Return if the component is not in the parent type.  */
  for (cmp = dt->components; cmp; cmp = cmp->next)
    if (strcmp (c->name, cmp->name) == 0)
      return;

  /* Build a gfc_ref to recursively call gfc_conv_component_ref.  */
  parent.type = REF_COMPONENT;
  parent.next = NULL;
  parent.u.c.sym = dt;
  parent.u.c.component = dt->components;

  if (dt->backend_decl == NULL)
    gfc_get_derived_type (dt);

  /* Build the reference and call self.  */
  gfc_conv_component_ref (se, &parent);
  parent.u.c.sym = dt->components->ts.u.derived;
  parent.u.c.component = c;
  conv_parent_component_references (se, &parent);
}

/* Return the contents of a variable. Also handles reference/pointer
   variables (all Fortran pointer references are implicit).  */

static void
gfc_conv_variable (gfc_se * se, gfc_expr * expr)
{
  gfc_ss *ss;
  gfc_ref *ref;
  gfc_symbol *sym;
  tree parent_decl = NULL_TREE;
  int parent_flag;
  bool return_value;
  bool alternate_entry;
  bool entry_master;

  sym = expr->symtree->n.sym;
  ss = se->ss;
  if (ss != NULL)
    {
      gfc_ss_info *ss_info = ss->info;

      /* Check that something hasn't gone horribly wrong.  */
      gcc_assert (ss != gfc_ss_terminator);
      gcc_assert (ss_info->expr == expr);

      /* A scalarized term.  We already know the descriptor.  */
      se->expr = ss_info->data.array.descriptor;
      se->string_length = ss_info->string_length;
      for (ref = ss_info->data.array.ref; ref; ref = ref->next)
	if (ref->type == REF_ARRAY && ref->u.ar.type != AR_ELEMENT)
	  break;
    }
  else
    {
      tree se_expr = NULL_TREE;

      se->expr = gfc_get_symbol_decl (sym);

      /* Deal with references to a parent results or entries by storing
	 the current_function_decl and moving to the parent_decl.  */
      return_value = sym->attr.function && sym->result == sym;
      alternate_entry = sym->attr.function && sym->attr.entry
			&& sym->result == sym;
      entry_master = sym->attr.result
		     && sym->ns->proc_name->attr.entry_master
		     && !gfc_return_by_reference (sym->ns->proc_name);
      if (current_function_decl)
	parent_decl = DECL_CONTEXT (current_function_decl);

      if ((se->expr == parent_decl && return_value)
	   || (sym->ns && sym->ns->proc_name
	       && parent_decl
	       && sym->ns->proc_name->backend_decl == parent_decl
	       && (alternate_entry || entry_master)))
	parent_flag = 1;
      else
	parent_flag = 0;

      /* Special case for assigning the return value of a function.
	 Self recursive functions must have an explicit return value.  */
      if (return_value && (se->expr == current_function_decl || parent_flag))
	se_expr = gfc_get_fake_result_decl (sym, parent_flag);

      /* Similarly for alternate entry points.  */
      else if (alternate_entry 
	       && (sym->ns->proc_name->backend_decl == current_function_decl
		   || parent_flag))
	{
	  gfc_entry_list *el = NULL;

	  for (el = sym->ns->entries; el; el = el->next)
	    if (sym == el->sym)
	      {
		se_expr = gfc_get_fake_result_decl (sym, parent_flag);
		break;
	      }
	}

      else if (entry_master
	       && (sym->ns->proc_name->backend_decl == current_function_decl
		   || parent_flag))
	se_expr = gfc_get_fake_result_decl (sym, parent_flag);

      if (se_expr)
	se->expr = se_expr;

      /* Procedure actual arguments.  */
      else if (sym->attr.flavor == FL_PROCEDURE
	       && se->expr != current_function_decl)
	{
	  if (!sym->attr.dummy && !sym->attr.proc_pointer)
	    {
	      gcc_assert (TREE_CODE (se->expr) == FUNCTION_DECL);
	      se->expr = gfc_build_addr_expr (NULL_TREE, se->expr);
	    }
	  return;
	}


      /* Dereference the expression, where needed. Since characters
	 are entirely different from other types, they are treated 
	 separately.  */
      if (sym->ts.type == BT_CHARACTER)
	{
	  /* Dereference character pointer dummy arguments
	     or results.  */
	  if ((sym->attr.pointer || sym->attr.allocatable)
	      && (sym->attr.dummy
		  || sym->attr.function
		  || sym->attr.result))
	    se->expr = build_fold_indirect_ref_loc (input_location,
						se->expr);

	}
      else if (!sym->attr.value)
	{
	  /* Dereference non-character scalar dummy arguments.  */
	  if (sym->attr.dummy && !sym->attr.dimension
	      && !(sym->attr.codimension && sym->attr.allocatable))
	    se->expr = build_fold_indirect_ref_loc (input_location,
						se->expr);

          /* Dereference scalar hidden result.  */
	  if (gfc_option.flag_f2c && sym->ts.type == BT_COMPLEX
	      && (sym->attr.function || sym->attr.result)
	      && !sym->attr.dimension && !sym->attr.pointer
	      && !sym->attr.always_explicit)
	    se->expr = build_fold_indirect_ref_loc (input_location,
						se->expr);

	  /* Dereference non-character pointer variables. 
	     These must be dummies, results, or scalars.  */
	  if ((sym->attr.pointer || sym->attr.allocatable
	       || gfc_is_associate_pointer (sym))
	      && (sym->attr.dummy
		  || sym->attr.function
		  || sym->attr.result
		  || (!sym->attr.dimension
		      && (!sym->attr.codimension || !sym->attr.allocatable))))
	    se->expr = build_fold_indirect_ref_loc (input_location,
						se->expr);
	}

      ref = expr->ref;
    }

  /* For character variables, also get the length.  */
  if (sym->ts.type == BT_CHARACTER)
    {
      /* If the character length of an entry isn't set, get the length from
         the master function instead.  */
      if (sym->attr.entry && !sym->ts.u.cl->backend_decl)
        se->string_length = sym->ns->proc_name->ts.u.cl->backend_decl;
      else
        se->string_length = sym->ts.u.cl->backend_decl;
      gcc_assert (se->string_length);
    }

  while (ref)
    {
      switch (ref->type)
	{
	case REF_ARRAY:
	  /* Return the descriptor if that's what we want and this is an array
	     section reference.  */
	  if (se->descriptor_only && ref->u.ar.type != AR_ELEMENT)
	    return;
/* TODO: Pointers to single elements of array sections, eg elemental subs.  */
	  /* Return the descriptor for array pointers and allocations.  */
	  if (se->want_pointer
	      && ref->next == NULL && (se->descriptor_only))
	    return;

	  gfc_conv_array_ref (se, &ref->u.ar, sym, &expr->where);
	  /* Return a pointer to an element.  */
	  break;

	case REF_COMPONENT:
	  if (ref->u.c.sym->attr.extension)
	    conv_parent_component_references (se, ref);

	  gfc_conv_component_ref (se, ref);

	  break;

	case REF_SUBSTRING:
	  gfc_conv_substring (se, ref, expr->ts.kind,
			      expr->symtree->name, &expr->where);
	  break;

	default:
	  gcc_unreachable ();
	  break;
	}
      ref = ref->next;
    }
  /* Pointer assignment, allocation or pass by reference.  Arrays are handled
     separately.  */
  if (se->want_pointer)
    {
      if (expr->ts.type == BT_CHARACTER && !gfc_is_proc_ptr_comp (expr, NULL))
	gfc_conv_string_parameter (se);
      else 
	se->expr = gfc_build_addr_expr (NULL_TREE, se->expr);
    }
}


/* Unary ops are easy... Or they would be if ! was a valid op.  */

static void
gfc_conv_unary_op (enum tree_code code, gfc_se * se, gfc_expr * expr)
{
  gfc_se operand;
  tree type;

  gcc_assert (expr->ts.type != BT_CHARACTER);
  /* Initialize the operand.  */
  gfc_init_se (&operand, se);
  gfc_conv_expr_val (&operand, expr->value.op.op1);
  gfc_add_block_to_block (&se->pre, &operand.pre);

  type = gfc_typenode_for_spec (&expr->ts);

  /* TRUTH_NOT_EXPR is not a "true" unary operator in GCC.
     We must convert it to a compare to 0 (e.g. EQ_EXPR (op1, 0)).
     All other unary operators have an equivalent GIMPLE unary operator.  */
  if (code == TRUTH_NOT_EXPR)
    se->expr = fold_build2_loc (input_location, EQ_EXPR, type, operand.expr,
				build_int_cst (type, 0));
  else
    se->expr = fold_build1_loc (input_location, code, type, operand.expr);

}

/* Expand power operator to optimal multiplications when a value is raised
   to a constant integer n. See section 4.6.3, "Evaluation of Powers" of
   Donald E. Knuth, "Seminumerical Algorithms", Vol. 2, "The Art of Computer
   Programming", 3rd Edition, 1998.  */

/* This code is mostly duplicated from expand_powi in the backend.
   We establish the "optimal power tree" lookup table with the defined size.
   The items in the table are the exponents used to calculate the index
   exponents. Any integer n less than the value can get an "addition chain",
   with the first node being one.  */
#define POWI_TABLE_SIZE 256

/* The table is from builtins.c.  */
static const unsigned char powi_table[POWI_TABLE_SIZE] =
  {
      0,   1,   1,   2,   2,   3,   3,   4,  /*   0 -   7 */
      4,   6,   5,   6,   6,  10,   7,   9,  /*   8 -  15 */
      8,  16,   9,  16,  10,  12,  11,  13,  /*  16 -  23 */
     12,  17,  13,  18,  14,  24,  15,  26,  /*  24 -  31 */
     16,  17,  17,  19,  18,  33,  19,  26,  /*  32 -  39 */
     20,  25,  21,  40,  22,  27,  23,  44,  /*  40 -  47 */
     24,  32,  25,  34,  26,  29,  27,  44,  /*  48 -  55 */
     28,  31,  29,  34,  30,  60,  31,  36,  /*  56 -  63 */
     32,  64,  33,  34,  34,  46,  35,  37,  /*  64 -  71 */
     36,  65,  37,  50,  38,  48,  39,  69,  /*  72 -  79 */
     40,  49,  41,  43,  42,  51,  43,  58,  /*  80 -  87 */
     44,  64,  45,  47,  46,  59,  47,  76,  /*  88 -  95 */
     48,  65,  49,  66,  50,  67,  51,  66,  /*  96 - 103 */
     52,  70,  53,  74,  54, 104,  55,  74,  /* 104 - 111 */
     56,  64,  57,  69,  58,  78,  59,  68,  /* 112 - 119 */
     60,  61,  61,  80,  62,  75,  63,  68,  /* 120 - 127 */
     64,  65,  65, 128,  66, 129,  67,  90,  /* 128 - 135 */
     68,  73,  69, 131,  70,  94,  71,  88,  /* 136 - 143 */
     72, 128,  73,  98,  74, 132,  75, 121,  /* 144 - 151 */
     76, 102,  77, 124,  78, 132,  79, 106,  /* 152 - 159 */
     80,  97,  81, 160,  82,  99,  83, 134,  /* 160 - 167 */
     84,  86,  85,  95,  86, 160,  87, 100,  /* 168 - 175 */
     88, 113,  89,  98,  90, 107,  91, 122,  /* 176 - 183 */
     92, 111,  93, 102,  94, 126,  95, 150,  /* 184 - 191 */
     96, 128,  97, 130,  98, 133,  99, 195,  /* 192 - 199 */
    100, 128, 101, 123, 102, 164, 103, 138,  /* 200 - 207 */
    104, 145, 105, 146, 106, 109, 107, 149,  /* 208 - 215 */
    108, 200, 109, 146, 110, 170, 111, 157,  /* 216 - 223 */
    112, 128, 113, 130, 114, 182, 115, 132,  /* 224 - 231 */
    116, 200, 117, 132, 118, 158, 119, 206,  /* 232 - 239 */
    120, 240, 121, 162, 122, 147, 123, 152,  /* 240 - 247 */
    124, 166, 125, 214, 126, 138, 127, 153,  /* 248 - 255 */
  };

/* If n is larger than lookup table's max index, we use the "window 
   method".  */
#define POWI_WINDOW_SIZE 3

/* Recursive function to expand the power operator. The temporary 
   values are put in tmpvar. The function returns tmpvar[1] ** n.  */
static tree
gfc_conv_powi (gfc_se * se, unsigned HOST_WIDE_INT n, tree * tmpvar)
{
  tree op0;
  tree op1;
  tree tmp;
  int digit;

  if (n < POWI_TABLE_SIZE)
    {
      if (tmpvar[n])
        return tmpvar[n];

      op0 = gfc_conv_powi (se, n - powi_table[n], tmpvar);
      op1 = gfc_conv_powi (se, powi_table[n], tmpvar);
    }
  else if (n & 1)
    {
      digit = n & ((1 << POWI_WINDOW_SIZE) - 1);
      op0 = gfc_conv_powi (se, n - digit, tmpvar);
      op1 = gfc_conv_powi (se, digit, tmpvar);
    }
  else
    {
      op0 = gfc_conv_powi (se, n >> 1, tmpvar);
      op1 = op0;
    }

  tmp = fold_build2_loc (input_location, MULT_EXPR, TREE_TYPE (op0), op0, op1);
  tmp = gfc_evaluate_now (tmp, &se->pre);

  if (n < POWI_TABLE_SIZE)
    tmpvar[n] = tmp;

  return tmp;
}


/* Expand lhs ** rhs. rhs is a constant integer. If it expands successfully,
   return 1. Else return 0 and a call to runtime library functions
   will have to be built.  */
static int
gfc_conv_cst_int_power (gfc_se * se, tree lhs, tree rhs)
{
  tree cond;
  tree tmp;
  tree type;
  tree vartmp[POWI_TABLE_SIZE];
  HOST_WIDE_INT m;
  unsigned HOST_WIDE_INT n;
  int sgn;

  /* If exponent is too large, we won't expand it anyway, so don't bother
     with large integer values.  */
  if (!double_int_fits_in_shwi_p (TREE_INT_CST (rhs)))
    return 0;

  m = double_int_to_shwi (TREE_INT_CST (rhs));
  /* There's no ABS for HOST_WIDE_INT, so here we go. It also takes care
     of the asymmetric range of the integer type.  */
  n = (unsigned HOST_WIDE_INT) (m < 0 ? -m : m);
  
  type = TREE_TYPE (lhs);
  sgn = tree_int_cst_sgn (rhs);

  if (((FLOAT_TYPE_P (type) && !flag_unsafe_math_optimizations)
       || optimize_size) && (m > 2 || m < -1))
    return 0;

  /* rhs == 0  */
  if (sgn == 0)
    {
      se->expr = gfc_build_const (type, integer_one_node);
      return 1;
    }

  /* If rhs < 0 and lhs is an integer, the result is -1, 0 or 1.  */
  if ((sgn == -1) && (TREE_CODE (type) == INTEGER_TYPE))
    {
      tmp = fold_build2_loc (input_location, EQ_EXPR, boolean_type_node,
			     lhs, build_int_cst (TREE_TYPE (lhs), -1));
      cond = fold_build2_loc (input_location, EQ_EXPR, boolean_type_node,
			      lhs, build_int_cst (TREE_TYPE (lhs), 1));

      /* If rhs is even,
	 result = (lhs == 1 || lhs == -1) ? 1 : 0.  */
      if ((n & 1) == 0)
        {
	  tmp = fold_build2_loc (input_location, TRUTH_OR_EXPR,
				 boolean_type_node, tmp, cond);
	  se->expr = fold_build3_loc (input_location, COND_EXPR, type,
				      tmp, build_int_cst (type, 1),
				      build_int_cst (type, 0));
	  return 1;
	}
      /* If rhs is odd,
	 result = (lhs == 1) ? 1 : (lhs == -1) ? -1 : 0.  */
      tmp = fold_build3_loc (input_location, COND_EXPR, type, tmp,
			     build_int_cst (type, -1),
			     build_int_cst (type, 0));
      se->expr = fold_build3_loc (input_location, COND_EXPR, type,
				  cond, build_int_cst (type, 1), tmp);
      return 1;
    }

  memset (vartmp, 0, sizeof (vartmp));
  vartmp[1] = lhs;
  if (sgn == -1)
    {
      tmp = gfc_build_const (type, integer_one_node);
      vartmp[1] = fold_build2_loc (input_location, RDIV_EXPR, type, tmp,
				   vartmp[1]);
    }

  se->expr = gfc_conv_powi (se, n, vartmp);

  return 1;
}


/* Power op (**).  Constant integer exponent has special handling.  */

static void
gfc_conv_power_op (gfc_se * se, gfc_expr * expr)
{
  tree gfc_int4_type_node;
  int kind;
  int ikind;
  int res_ikind_1, res_ikind_2;
  gfc_se lse;
  gfc_se rse;
  tree fndecl = NULL;

  gfc_init_se (&lse, se);
  gfc_conv_expr_val (&lse, expr->value.op.op1);
  lse.expr = gfc_evaluate_now (lse.expr, &lse.pre);
  gfc_add_block_to_block (&se->pre, &lse.pre);

  gfc_init_se (&rse, se);
  gfc_conv_expr_val (&rse, expr->value.op.op2);
  gfc_add_block_to_block (&se->pre, &rse.pre);

  if (expr->value.op.op2->ts.type == BT_INTEGER
      && expr->value.op.op2->expr_type == EXPR_CONSTANT)
    if (gfc_conv_cst_int_power (se, lse.expr, rse.expr))
      return;

  gfc_int4_type_node = gfc_get_int_type (4);

  /* In case of integer operands with kinds 1 or 2, we call the integer kind 4
     library routine.  But in the end, we have to convert the result back
     if this case applies -- with res_ikind_K, we keep track whether operand K
     falls into this case.  */
  res_ikind_1 = -1;
  res_ikind_2 = -1;

  kind = expr->value.op.op1->ts.kind;
  switch (expr->value.op.op2->ts.type)
    {
    case BT_INTEGER:
      ikind = expr->value.op.op2->ts.kind;
      switch (ikind)
	{
	case 1:
	case 2:
	  rse.expr = convert (gfc_int4_type_node, rse.expr);
	  res_ikind_2 = ikind;
	  /* Fall through.  */

	case 4:
	  ikind = 0;
	  break;
	  
	case 8:
	  ikind = 1;
	  break;

	case 16:
	  ikind = 2;
	  break;

	default:
	  gcc_unreachable ();
	}
      switch (kind)
	{
	case 1:
	case 2:
	  if (expr->value.op.op1->ts.type == BT_INTEGER)
	    {
	      lse.expr = convert (gfc_int4_type_node, lse.expr);
	      res_ikind_1 = kind;
	    }
	  else
	    gcc_unreachable ();
	  /* Fall through.  */

	case 4:
	  kind = 0;
	  break;
	  
	case 8:
	  kind = 1;
	  break;

	case 10:
	  kind = 2;
	  break;

	case 16:
	  kind = 3;
	  break;

	default:
	  gcc_unreachable ();
	}
      
      switch (expr->value.op.op1->ts.type)
	{
	case BT_INTEGER:
	  if (kind == 3) /* Case 16 was not handled properly above.  */
	    kind = 2;
	  fndecl = gfor_fndecl_math_powi[kind][ikind].integer;
	  break;

	case BT_REAL:
	  /* Use builtins for real ** int4.  */
	  if (ikind == 0)
	    {
	      switch (kind)
		{
		case 0:
		  fndecl = builtin_decl_explicit (BUILT_IN_POWIF);
		  break;
		
		case 1:
		  fndecl = builtin_decl_explicit (BUILT_IN_POWI);
		  break;

		case 2:
		  fndecl = builtin_decl_explicit (BUILT_IN_POWIL);
		  break;

		case 3:
		  /* Use the __builtin_powil() only if real(kind=16) is 
		     actually the C long double type.  */
		  if (!gfc_real16_is_float128)
		    fndecl = builtin_decl_explicit (BUILT_IN_POWIL);
		  break;

		default:
		  gcc_unreachable ();
		}
	    }

	  /* If we don't have a good builtin for this, go for the 
	     library function.  */
	  if (!fndecl)
	    fndecl = gfor_fndecl_math_powi[kind][ikind].real;
	  break;

	case BT_COMPLEX:
	  fndecl = gfor_fndecl_math_powi[kind][ikind].cmplx;
	  break;

	default:
	  gcc_unreachable ();
 	}
      break;

    case BT_REAL:
      fndecl = gfc_builtin_decl_for_float_kind (BUILT_IN_POW, kind);
      break;

    case BT_COMPLEX:
      fndecl = gfc_builtin_decl_for_float_kind (BUILT_IN_CPOW, kind);
      break;

    default:
      gcc_unreachable ();
      break;
    }

  se->expr = build_call_expr_loc (input_location,
			      fndecl, 2, lse.expr, rse.expr);

  /* Convert the result back if it is of wrong integer kind.  */
  if (res_ikind_1 != -1 && res_ikind_2 != -1)
    {
      /* We want the maximum of both operand kinds as result.  */
      if (res_ikind_1 < res_ikind_2)
	res_ikind_1 = res_ikind_2;
      se->expr = convert (gfc_get_int_type (res_ikind_1), se->expr);
    }
}


/* Generate code to allocate a string temporary.  */

tree
gfc_conv_string_tmp (gfc_se * se, tree type, tree len)
{
  tree var;
  tree tmp;

  if (gfc_can_put_var_on_stack (len))
    {
      /* Create a temporary variable to hold the result.  */
      tmp = fold_build2_loc (input_location, MINUS_EXPR,
			     gfc_charlen_type_node, len,
			     build_int_cst (gfc_charlen_type_node, 1));
      tmp = build_range_type (gfc_array_index_type, gfc_index_zero_node, tmp);

      if (TREE_CODE (TREE_TYPE (type)) == ARRAY_TYPE)
	tmp = build_array_type (TREE_TYPE (TREE_TYPE (type)), tmp);
      else
	tmp = build_array_type (TREE_TYPE (type), tmp);

      var = gfc_create_var (tmp, "str");
      var = gfc_build_addr_expr (type, var);
    }
  else
    {
      /* Allocate a temporary to hold the result.  */
      var = gfc_create_var (type, "pstr");
      tmp = gfc_call_malloc (&se->pre, type,
			     fold_build2_loc (input_location, MULT_EXPR,
					      TREE_TYPE (len), len,
					      fold_convert (TREE_TYPE (len),
							    TYPE_SIZE (type))));
      gfc_add_modify (&se->pre, var, tmp);

      /* Free the temporary afterwards.  */
      tmp = gfc_call_free (convert (pvoid_type_node, var));
      gfc_add_expr_to_block (&se->post, tmp);
    }

  return var;
}


/* Handle a string concatenation operation.  A temporary will be allocated to
   hold the result.  */

static void
gfc_conv_concat_op (gfc_se * se, gfc_expr * expr)
{
  gfc_se lse, rse;
  tree len, type, var, tmp, fndecl;

  gcc_assert (expr->value.op.op1->ts.type == BT_CHARACTER
	      && expr->value.op.op2->ts.type == BT_CHARACTER);
  gcc_assert (expr->value.op.op1->ts.kind == expr->value.op.op2->ts.kind);

  gfc_init_se (&lse, se);
  gfc_conv_expr (&lse, expr->value.op.op1);
  gfc_conv_string_parameter (&lse);
  gfc_init_se (&rse, se);
  gfc_conv_expr (&rse, expr->value.op.op2);
  gfc_conv_string_parameter (&rse);

  gfc_add_block_to_block (&se->pre, &lse.pre);
  gfc_add_block_to_block (&se->pre, &rse.pre);

  type = gfc_get_character_type (expr->ts.kind, expr->ts.u.cl);
  len = TYPE_MAX_VALUE (TYPE_DOMAIN (type));
  if (len == NULL_TREE)
    {
      len = fold_build2_loc (input_location, PLUS_EXPR,
			     TREE_TYPE (lse.string_length),
			     lse.string_length, rse.string_length);
    }

  type = build_pointer_type (type);

  var = gfc_conv_string_tmp (se, type, len);

  /* Do the actual concatenation.  */
  if (expr->ts.kind == 1)
    fndecl = gfor_fndecl_concat_string;
  else if (expr->ts.kind == 4)
    fndecl = gfor_fndecl_concat_string_char4;
  else
    gcc_unreachable ();

  tmp = build_call_expr_loc (input_location,
			 fndecl, 6, len, var, lse.string_length, lse.expr,
			 rse.string_length, rse.expr);
  gfc_add_expr_to_block (&se->pre, tmp);

  /* Add the cleanup for the operands.  */
  gfc_add_block_to_block (&se->pre, &rse.post);
  gfc_add_block_to_block (&se->pre, &lse.post);

  se->expr = var;
  se->string_length = len;
}

/* Translates an op expression. Common (binary) cases are handled by this
   function, others are passed on. Recursion is used in either case.
   We use the fact that (op1.ts == op2.ts) (except for the power
   operator **).
   Operators need no special handling for scalarized expressions as long as
   they call gfc_conv_simple_val to get their operands.
   Character strings get special handling.  */

static void
gfc_conv_expr_op (gfc_se * se, gfc_expr * expr)
{
  enum tree_code code;
  gfc_se lse;
  gfc_se rse;
  tree tmp, type;
  int lop;
  int checkstring;

  checkstring = 0;
  lop = 0;
  switch (expr->value.op.op)
    {
    case INTRINSIC_PARENTHESES:
      if ((expr->ts.type == BT_REAL
	   || expr->ts.type == BT_COMPLEX)
	  && gfc_option.flag_protect_parens)
	{
	  gfc_conv_unary_op (PAREN_EXPR, se, expr);
	  gcc_assert (FLOAT_TYPE_P (TREE_TYPE (se->expr)));
	  return;
	}

      /* Fallthrough.  */
    case INTRINSIC_UPLUS:
      gfc_conv_expr (se, expr->value.op.op1);
      return;

    case INTRINSIC_UMINUS:
      gfc_conv_unary_op (NEGATE_EXPR, se, expr);
      return;

    case INTRINSIC_NOT:
      gfc_conv_unary_op (TRUTH_NOT_EXPR, se, expr);
      return;

    case INTRINSIC_PLUS:
      code = PLUS_EXPR;
      break;

    case INTRINSIC_MINUS:
      code = MINUS_EXPR;
      break;

    case INTRINSIC_TIMES:
      code = MULT_EXPR;
      break;

    case INTRINSIC_DIVIDE:
      /* If expr is a real or complex expr, use an RDIV_EXPR. If op1 is
         an integer, we must round towards zero, so we use a
         TRUNC_DIV_EXPR.  */
      if (expr->ts.type == BT_INTEGER)
	code = TRUNC_DIV_EXPR;
      else
	code = RDIV_EXPR;
      break;

    case INTRINSIC_POWER:
      gfc_conv_power_op (se, expr);
      return;

    case INTRINSIC_CONCAT:
      gfc_conv_concat_op (se, expr);
      return;

    case INTRINSIC_AND:
      code = TRUTH_ANDIF_EXPR;
      lop = 1;
      break;

    case INTRINSIC_OR:
      code = TRUTH_ORIF_EXPR;
      lop = 1;
      break;

      /* EQV and NEQV only work on logicals, but since we represent them
         as integers, we can use EQ_EXPR and NE_EXPR for them in GIMPLE.  */
    case INTRINSIC_EQ:
    case INTRINSIC_EQ_OS:
    case INTRINSIC_EQV:
      code = EQ_EXPR;
      checkstring = 1;
      lop = 1;
      break;

    case INTRINSIC_NE:
    case INTRINSIC_NE_OS:
    case INTRINSIC_NEQV:
      code = NE_EXPR;
      checkstring = 1;
      lop = 1;
      break;

    case INTRINSIC_GT:
    case INTRINSIC_GT_OS:
      code = GT_EXPR;
      checkstring = 1;
      lop = 1;
      break;

    case INTRINSIC_GE:
    case INTRINSIC_GE_OS:
      code = GE_EXPR;
      checkstring = 1;
      lop = 1;
      break;

    case INTRINSIC_LT:
    case INTRINSIC_LT_OS:
      code = LT_EXPR;
      checkstring = 1;
      lop = 1;
      break;

    case INTRINSIC_LE:
    case INTRINSIC_LE_OS:
      code = LE_EXPR;
      checkstring = 1;
      lop = 1;
      break;

    case INTRINSIC_USER:
    case INTRINSIC_ASSIGN:
      /* These should be converted into function calls by the frontend.  */
      gcc_unreachable ();

    default:
      fatal_error ("Unknown intrinsic op");
      return;
    }

  /* The only exception to this is **, which is handled separately anyway.  */
  gcc_assert (expr->value.op.op1->ts.type == expr->value.op.op2->ts.type);

  if (checkstring && expr->value.op.op1->ts.type != BT_CHARACTER)
    checkstring = 0;

  /* lhs */
  gfc_init_se (&lse, se);
  gfc_conv_expr (&lse, expr->value.op.op1);
  gfc_add_block_to_block (&se->pre, &lse.pre);

  /* rhs */
  gfc_init_se (&rse, se);
  gfc_conv_expr (&rse, expr->value.op.op2);
  gfc_add_block_to_block (&se->pre, &rse.pre);

  if (checkstring)
    {
      gfc_conv_string_parameter (&lse);
      gfc_conv_string_parameter (&rse);

      lse.expr = gfc_build_compare_string (lse.string_length, lse.expr,
					   rse.string_length, rse.expr,
					   expr->value.op.op1->ts.kind,
					   code);
      rse.expr = build_int_cst (TREE_TYPE (lse.expr), 0);
      gfc_add_block_to_block (&lse.post, &rse.post);
    }

  type = gfc_typenode_for_spec (&expr->ts);

  if (lop)
    {
      /* The result of logical ops is always boolean_type_node.  */
      tmp = fold_build2_loc (input_location, code, boolean_type_node,
			     lse.expr, rse.expr);
      se->expr = convert (type, tmp);
    }
  else
    se->expr = fold_build2_loc (input_location, code, type, lse.expr, rse.expr);

  /* Add the post blocks.  */
  gfc_add_block_to_block (&se->post, &rse.post);
  gfc_add_block_to_block (&se->post, &lse.post);
}

/* If a string's length is one, we convert it to a single character.  */

tree
gfc_string_to_single_character (tree len, tree str, int kind)
{

  if (!INTEGER_CST_P (len) || TREE_INT_CST_HIGH (len) != 0
      || !POINTER_TYPE_P (TREE_TYPE (str)))
    return NULL_TREE;

  if (TREE_INT_CST_LOW (len) == 1)
    {
      str = fold_convert (gfc_get_pchar_type (kind), str);
      return build_fold_indirect_ref_loc (input_location, str);
    }

  if (kind == 1
      && TREE_CODE (str) == ADDR_EXPR
      && TREE_CODE (TREE_OPERAND (str, 0)) == ARRAY_REF
      && TREE_CODE (TREE_OPERAND (TREE_OPERAND (str, 0), 0)) == STRING_CST
      && array_ref_low_bound (TREE_OPERAND (str, 0))
	 == TREE_OPERAND (TREE_OPERAND (str, 0), 1)
      && TREE_INT_CST_LOW (len) > 1
      && TREE_INT_CST_LOW (len)
	 == (unsigned HOST_WIDE_INT)
	    TREE_STRING_LENGTH (TREE_OPERAND (TREE_OPERAND (str, 0), 0)))
    {
      tree ret = fold_convert (gfc_get_pchar_type (kind), str);
      ret = build_fold_indirect_ref_loc (input_location, ret);
      if (TREE_CODE (ret) == INTEGER_CST)
	{
	  tree string_cst = TREE_OPERAND (TREE_OPERAND (str, 0), 0);
	  int i, length = TREE_STRING_LENGTH (string_cst);
	  const char *ptr = TREE_STRING_POINTER (string_cst);

	  for (i = 1; i < length; i++)
	    if (ptr[i] != ' ')
	      return NULL_TREE;

	  return ret;
	}
    }

  return NULL_TREE;
}


void
gfc_conv_scalar_char_value (gfc_symbol *sym, gfc_se *se, gfc_expr **expr)
{

  if (sym->backend_decl)
    {
      /* This becomes the nominal_type in
	 function.c:assign_parm_find_data_types.  */
      TREE_TYPE (sym->backend_decl) = unsigned_char_type_node;
      /* This becomes the passed_type in
	 function.c:assign_parm_find_data_types.  C promotes char to
	 integer for argument passing.  */
      DECL_ARG_TYPE (sym->backend_decl) = unsigned_type_node;

      DECL_BY_REFERENCE (sym->backend_decl) = 0;
    }

  if (expr != NULL)
    {
      /* If we have a constant character expression, make it into an
	 integer.  */
      if ((*expr)->expr_type == EXPR_CONSTANT)
        {
	  gfc_typespec ts;
          gfc_clear_ts (&ts);

	  *expr = gfc_get_int_expr (gfc_default_integer_kind, NULL,
				    (int)(*expr)->value.character.string[0]);
	  if ((*expr)->ts.kind != gfc_c_int_kind)
	    {
  	      /* The expr needs to be compatible with a C int.  If the 
		 conversion fails, then the 2 causes an ICE.  */
	      ts.type = BT_INTEGER;
	      ts.kind = gfc_c_int_kind;
	      gfc_convert_type (*expr, &ts, 2);
	    }
	}
      else if (se != NULL && (*expr)->expr_type == EXPR_VARIABLE)
        {
	  if ((*expr)->ref == NULL)
	    {
	      se->expr = gfc_string_to_single_character
		(build_int_cst (integer_type_node, 1),
		 gfc_build_addr_expr (gfc_get_pchar_type ((*expr)->ts.kind),
				      gfc_get_symbol_decl
				      ((*expr)->symtree->n.sym)),
		 (*expr)->ts.kind);
	    }
	  else
	    {
	      gfc_conv_variable (se, *expr);
	      se->expr = gfc_string_to_single_character
		(build_int_cst (integer_type_node, 1),
		 gfc_build_addr_expr (gfc_get_pchar_type ((*expr)->ts.kind),
				      se->expr),
		 (*expr)->ts.kind);
	    }
	}
    }
}

/* Helper function for gfc_build_compare_string.  Return LEN_TRIM value
   if STR is a string literal, otherwise return -1.  */

static int
gfc_optimize_len_trim (tree len, tree str, int kind)
{
  if (kind == 1
      && TREE_CODE (str) == ADDR_EXPR
      && TREE_CODE (TREE_OPERAND (str, 0)) == ARRAY_REF
      && TREE_CODE (TREE_OPERAND (TREE_OPERAND (str, 0), 0)) == STRING_CST
      && array_ref_low_bound (TREE_OPERAND (str, 0))
	 == TREE_OPERAND (TREE_OPERAND (str, 0), 1)
      && TREE_INT_CST_LOW (len) >= 1
      && TREE_INT_CST_LOW (len)
	 == (unsigned HOST_WIDE_INT)
	    TREE_STRING_LENGTH (TREE_OPERAND (TREE_OPERAND (str, 0), 0)))
    {
      tree folded = fold_convert (gfc_get_pchar_type (kind), str);
      folded = build_fold_indirect_ref_loc (input_location, folded);
      if (TREE_CODE (folded) == INTEGER_CST)
	{
	  tree string_cst = TREE_OPERAND (TREE_OPERAND (str, 0), 0);
	  int length = TREE_STRING_LENGTH (string_cst);
	  const char *ptr = TREE_STRING_POINTER (string_cst);

	  for (; length > 0; length--)
	    if (ptr[length - 1] != ' ')
	      break;

	  return length;
	}
    }
  return -1;
}

/* Compare two strings. If they are all single characters, the result is the
   subtraction of them. Otherwise, we build a library call.  */

tree
gfc_build_compare_string (tree len1, tree str1, tree len2, tree str2, int kind,
			  enum tree_code code)
{
  tree sc1;
  tree sc2;
  tree fndecl;

  gcc_assert (POINTER_TYPE_P (TREE_TYPE (str1)));
  gcc_assert (POINTER_TYPE_P (TREE_TYPE (str2)));

  sc1 = gfc_string_to_single_character (len1, str1, kind);
  sc2 = gfc_string_to_single_character (len2, str2, kind);

  if (sc1 != NULL_TREE && sc2 != NULL_TREE)
    {
      /* Deal with single character specially.  */
      sc1 = fold_convert (integer_type_node, sc1);
      sc2 = fold_convert (integer_type_node, sc2);
      return fold_build2_loc (input_location, MINUS_EXPR, integer_type_node,
			      sc1, sc2);
    }

  if ((code == EQ_EXPR || code == NE_EXPR)
      && optimize
      && INTEGER_CST_P (len1) && INTEGER_CST_P (len2))
    {
      /* If one string is a string literal with LEN_TRIM longer
	 than the length of the second string, the strings
	 compare unequal.  */
      int len = gfc_optimize_len_trim (len1, str1, kind);
      if (len > 0 && compare_tree_int (len2, len) < 0)
	return integer_one_node;
      len = gfc_optimize_len_trim (len2, str2, kind);
      if (len > 0 && compare_tree_int (len1, len) < 0)
	return integer_one_node;
    }

  /* Build a call for the comparison.  */
  if (kind == 1)
    fndecl = gfor_fndecl_compare_string;
  else if (kind == 4)
    fndecl = gfor_fndecl_compare_string_char4;
  else
    gcc_unreachable ();

  return build_call_expr_loc (input_location, fndecl, 4,
			      len1, str1, len2, str2);
}


/* Return the backend_decl for a procedure pointer component.  */

static tree
get_proc_ptr_comp (gfc_expr *e)
{
  gfc_se comp_se;
  gfc_expr *e2;
  expr_t old_type;

  gfc_init_se (&comp_se, NULL);
  e2 = gfc_copy_expr (e);
  /* We have to restore the expr type later so that gfc_free_expr frees
     the exact same thing that was allocated.
     TODO: This is ugly.  */
  old_type = e2->expr_type;
  e2->expr_type = EXPR_VARIABLE;
  gfc_conv_expr (&comp_se, e2);
  e2->expr_type = old_type;
  gfc_free_expr (e2);
  return build_fold_addr_expr_loc (input_location, comp_se.expr);
}


/* Convert a typebound function reference from a class object.  */
static void
conv_base_obj_fcn_val (gfc_se * se, tree base_object, gfc_expr * expr)
{
  gfc_ref *ref;
  tree var;

  if (TREE_CODE (base_object) != VAR_DECL)
    {
      var = gfc_create_var (TREE_TYPE (base_object), NULL);
      gfc_add_modify (&se->pre, var, base_object);
    }
  se->expr = gfc_class_vptr_get (base_object);
  se->expr = build_fold_indirect_ref_loc (input_location, se->expr);
  ref = expr->ref;
  while (ref && ref->next)
    ref = ref->next;
  gcc_assert (ref && ref->type == REF_COMPONENT);
  if (ref->u.c.sym->attr.extension)
    conv_parent_component_references (se, ref);
  gfc_conv_component_ref (se, ref);
  se->expr = build_fold_addr_expr_loc (input_location, se->expr);
}


static void
conv_function_val (gfc_se * se, gfc_symbol * sym, gfc_expr * expr)
{
  tree tmp;

  if (gfc_is_proc_ptr_comp (expr, NULL))
    tmp = get_proc_ptr_comp (expr);
  else if (sym->attr.dummy)
    {
      tmp = gfc_get_symbol_decl (sym);
      if (sym->attr.proc_pointer)
        tmp = build_fold_indirect_ref_loc (input_location,
				       tmp);
      gcc_assert (TREE_CODE (TREE_TYPE (tmp)) == POINTER_TYPE
	      && TREE_CODE (TREE_TYPE (TREE_TYPE (tmp))) == FUNCTION_TYPE);
    }
  else
    {
      if (!sym->backend_decl)
	sym->backend_decl = gfc_get_extern_function_decl (sym);

      tmp = sym->backend_decl;

      if (sym->attr.cray_pointee)
	{
	  /* TODO - make the cray pointee a pointer to a procedure,
	     assign the pointer to it and use it for the call.  This
	     will do for now!  */
	  tmp = convert (build_pointer_type (TREE_TYPE (tmp)),
			 gfc_get_symbol_decl (sym->cp_pointer));
	  tmp = gfc_evaluate_now (tmp, &se->pre);
	}

      if (!POINTER_TYPE_P (TREE_TYPE (tmp)))
	{
	  gcc_assert (TREE_CODE (tmp) == FUNCTION_DECL);
	  tmp = gfc_build_addr_expr (NULL_TREE, tmp);
	}
    }
  se->expr = tmp;
}


/* Initialize MAPPING.  */

void
gfc_init_interface_mapping (gfc_interface_mapping * mapping)
{
  mapping->syms = NULL;
  mapping->charlens = NULL;
}


/* Free all memory held by MAPPING (but not MAPPING itself).  */

void
gfc_free_interface_mapping (gfc_interface_mapping * mapping)
{
  gfc_interface_sym_mapping *sym;
  gfc_interface_sym_mapping *nextsym;
  gfc_charlen *cl;
  gfc_charlen *nextcl;

  for (sym = mapping->syms; sym; sym = nextsym)
    {
      nextsym = sym->next;
      sym->new_sym->n.sym->formal = NULL;
      gfc_free_symbol (sym->new_sym->n.sym);
      gfc_free_expr (sym->expr);
      free (sym->new_sym);
      free (sym);
    }
  for (cl = mapping->charlens; cl; cl = nextcl)
    {
      nextcl = cl->next;
      gfc_free_expr (cl->length);
      free (cl);
    }
}


/* Return a copy of gfc_charlen CL.  Add the returned structure to
   MAPPING so that it will be freed by gfc_free_interface_mapping.  */

static gfc_charlen *
gfc_get_interface_mapping_charlen (gfc_interface_mapping * mapping,
				   gfc_charlen * cl)
{
  gfc_charlen *new_charlen;

  new_charlen = gfc_get_charlen ();
  new_charlen->next = mapping->charlens;
  new_charlen->length = gfc_copy_expr (cl->length);

  mapping->charlens = new_charlen;
  return new_charlen;
}


/* A subroutine of gfc_add_interface_mapping.  Return a descriptorless
   array variable that can be used as the actual argument for dummy
   argument SYM.  Add any initialization code to BLOCK.  PACKED is as
   for gfc_get_nodesc_array_type and DATA points to the first element
   in the passed array.  */

static tree
gfc_get_interface_mapping_array (stmtblock_t * block, gfc_symbol * sym,
				 gfc_packed packed, tree data)
{
  tree type;
  tree var;

  type = gfc_typenode_for_spec (&sym->ts);
  type = gfc_get_nodesc_array_type (type, sym->as, packed,
				    !sym->attr.target && !sym->attr.pointer
				    && !sym->attr.proc_pointer);

  var = gfc_create_var (type, "ifm");
  gfc_add_modify (block, var, fold_convert (type, data));

  return var;
}


/* A subroutine of gfc_add_interface_mapping.  Set the stride, upper bounds
   and offset of descriptorless array type TYPE given that it has the same
   size as DESC.  Add any set-up code to BLOCK.  */

static void
gfc_set_interface_mapping_bounds (stmtblock_t * block, tree type, tree desc)
{
  int n;
  tree dim;
  tree offset;
  tree tmp;

  offset = gfc_index_zero_node;
  for (n = 0; n < GFC_TYPE_ARRAY_RANK (type); n++)
    {
      dim = gfc_rank_cst[n];
      GFC_TYPE_ARRAY_STRIDE (type, n) = gfc_conv_array_stride (desc, n);
      if (GFC_TYPE_ARRAY_LBOUND (type, n) == NULL_TREE)
	{
	  GFC_TYPE_ARRAY_LBOUND (type, n)
		= gfc_conv_descriptor_lbound_get (desc, dim);
	  GFC_TYPE_ARRAY_UBOUND (type, n)
		= gfc_conv_descriptor_ubound_get (desc, dim);
	}
      else if (GFC_TYPE_ARRAY_UBOUND (type, n) == NULL_TREE)
	{
	  tmp = fold_build2_loc (input_location, MINUS_EXPR,
				 gfc_array_index_type,
				 gfc_conv_descriptor_ubound_get (desc, dim),
				 gfc_conv_descriptor_lbound_get (desc, dim));
	  tmp = fold_build2_loc (input_location, PLUS_EXPR,
				 gfc_array_index_type,
				 GFC_TYPE_ARRAY_LBOUND (type, n), tmp);
	  tmp = gfc_evaluate_now (tmp, block);
	  GFC_TYPE_ARRAY_UBOUND (type, n) = tmp;
	}
      tmp = fold_build2_loc (input_location, MULT_EXPR, gfc_array_index_type,
			     GFC_TYPE_ARRAY_LBOUND (type, n),
			     GFC_TYPE_ARRAY_STRIDE (type, n));
      offset = fold_build2_loc (input_location, MINUS_EXPR,
				gfc_array_index_type, offset, tmp);
    }
  offset = gfc_evaluate_now (offset, block);
  GFC_TYPE_ARRAY_OFFSET (type) = offset;
}


/* Extend MAPPING so that it maps dummy argument SYM to the value stored
   in SE.  The caller may still use se->expr and se->string_length after
   calling this function.  */

void
gfc_add_interface_mapping (gfc_interface_mapping * mapping,
			   gfc_symbol * sym, gfc_se * se,
			   gfc_expr *expr)
{
  gfc_interface_sym_mapping *sm;
  tree desc;
  tree tmp;
  tree value;
  gfc_symbol *new_sym;
  gfc_symtree *root;
  gfc_symtree *new_symtree;

  /* Create a new symbol to represent the actual argument.  */
  new_sym = gfc_new_symbol (sym->name, NULL);
  new_sym->ts = sym->ts;
  new_sym->as = gfc_copy_array_spec (sym->as);
  new_sym->attr.referenced = 1;
  new_sym->attr.dimension = sym->attr.dimension;
  new_sym->attr.contiguous = sym->attr.contiguous;
  new_sym->attr.codimension = sym->attr.codimension;
  new_sym->attr.pointer = sym->attr.pointer;
  new_sym->attr.allocatable = sym->attr.allocatable;
  new_sym->attr.flavor = sym->attr.flavor;
  new_sym->attr.function = sym->attr.function;

  /* Ensure that the interface is available and that
     descriptors are passed for array actual arguments.  */
  if (sym->attr.flavor == FL_PROCEDURE)
    {
      new_sym->formal = expr->symtree->n.sym->formal;
      new_sym->attr.always_explicit
	    = expr->symtree->n.sym->attr.always_explicit;
    }

  /* Create a fake symtree for it.  */
  root = NULL;
  new_symtree = gfc_new_symtree (&root, sym->name);
  new_symtree->n.sym = new_sym;
  gcc_assert (new_symtree == root);

  /* Create a dummy->actual mapping.  */
  sm = XCNEW (gfc_interface_sym_mapping);
  sm->next = mapping->syms;
  sm->old = sym;
  sm->new_sym = new_symtree;
  sm->expr = gfc_copy_expr (expr);
  mapping->syms = sm;

  /* Stabilize the argument's value.  */
  if (!sym->attr.function && se)
    se->expr = gfc_evaluate_now (se->expr, &se->pre);

  if (sym->ts.type == BT_CHARACTER)
    {
      /* Create a copy of the dummy argument's length.  */
      new_sym->ts.u.cl = gfc_get_interface_mapping_charlen (mapping, sym->ts.u.cl);
      sm->expr->ts.u.cl = new_sym->ts.u.cl;

      /* If the length is specified as "*", record the length that
	 the caller is passing.  We should use the callee's length
	 in all other cases.  */
      if (!new_sym->ts.u.cl->length && se)
	{
	  se->string_length = gfc_evaluate_now (se->string_length, &se->pre);
	  new_sym->ts.u.cl->backend_decl = se->string_length;
	}
    }

  if (!se)
    return;

  /* Use the passed value as-is if the argument is a function.  */
  if (sym->attr.flavor == FL_PROCEDURE)
    value = se->expr;

  /* If the argument is either a string or a pointer to a string,
     convert it to a boundless character type.  */
  else if (!sym->attr.dimension && sym->ts.type == BT_CHARACTER)
    {
      tmp = gfc_get_character_type_len (sym->ts.kind, NULL);
      tmp = build_pointer_type (tmp);
      if (sym->attr.pointer)
        value = build_fold_indirect_ref_loc (input_location,
					 se->expr);
      else
        value = se->expr;
      value = fold_convert (tmp, value);
    }

  /* If the argument is a scalar, a pointer to an array or an allocatable,
     dereference it.  */
  else if (!sym->attr.dimension || sym->attr.pointer || sym->attr.allocatable)
    value = build_fold_indirect_ref_loc (input_location,
				     se->expr);
  
  /* For character(*), use the actual argument's descriptor.  */  
  else if (sym->ts.type == BT_CHARACTER && !new_sym->ts.u.cl->length)
    value = build_fold_indirect_ref_loc (input_location,
				     se->expr);

  /* If the argument is an array descriptor, use it to determine
     information about the actual argument's shape.  */
  else if (POINTER_TYPE_P (TREE_TYPE (se->expr))
	   && GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (TREE_TYPE (se->expr))))
    {
      /* Get the actual argument's descriptor.  */
      desc = build_fold_indirect_ref_loc (input_location,
				      se->expr);

      /* Create the replacement variable.  */
      tmp = gfc_conv_descriptor_data_get (desc);
      value = gfc_get_interface_mapping_array (&se->pre, sym,
					       PACKED_NO, tmp);

      /* Use DESC to work out the upper bounds, strides and offset.  */
      gfc_set_interface_mapping_bounds (&se->pre, TREE_TYPE (value), desc);
    }
  else
    /* Otherwise we have a packed array.  */
    value = gfc_get_interface_mapping_array (&se->pre, sym,
					     PACKED_FULL, se->expr);

  new_sym->backend_decl = value;
}


/* Called once all dummy argument mappings have been added to MAPPING,
   but before the mapping is used to evaluate expressions.  Pre-evaluate
   the length of each argument, adding any initialization code to PRE and
   any finalization code to POST.  */

void
gfc_finish_interface_mapping (gfc_interface_mapping * mapping,
			      stmtblock_t * pre, stmtblock_t * post)
{
  gfc_interface_sym_mapping *sym;
  gfc_expr *expr;
  gfc_se se;

  for (sym = mapping->syms; sym; sym = sym->next)
    if (sym->new_sym->n.sym->ts.type == BT_CHARACTER
	&& !sym->new_sym->n.sym->ts.u.cl->backend_decl)
      {
	expr = sym->new_sym->n.sym->ts.u.cl->length;
	gfc_apply_interface_mapping_to_expr (mapping, expr);
	gfc_init_se (&se, NULL);
	gfc_conv_expr (&se, expr);
	se.expr = fold_convert (gfc_charlen_type_node, se.expr);
	se.expr = gfc_evaluate_now (se.expr, &se.pre);
	gfc_add_block_to_block (pre, &se.pre);
	gfc_add_block_to_block (post, &se.post);

	sym->new_sym->n.sym->ts.u.cl->backend_decl = se.expr;
      }
}


/* Like gfc_apply_interface_mapping_to_expr, but applied to
   constructor C.  */

static void
gfc_apply_interface_mapping_to_cons (gfc_interface_mapping * mapping,
				     gfc_constructor_base base)
{
  gfc_constructor *c;
  for (c = gfc_constructor_first (base); c; c = gfc_constructor_next (c))
    {
      gfc_apply_interface_mapping_to_expr (mapping, c->expr);
      if (c->iterator)
	{
	  gfc_apply_interface_mapping_to_expr (mapping, c->iterator->start);
	  gfc_apply_interface_mapping_to_expr (mapping, c->iterator->end);
	  gfc_apply_interface_mapping_to_expr (mapping, c->iterator->step);
	}
    }
}


/* Like gfc_apply_interface_mapping_to_expr, but applied to
   reference REF.  */

static void
gfc_apply_interface_mapping_to_ref (gfc_interface_mapping * mapping,
				    gfc_ref * ref)
{
  int n;

  for (; ref; ref = ref->next)
    switch (ref->type)
      {
      case REF_ARRAY:
	for (n = 0; n < ref->u.ar.dimen; n++)
	  {
	    gfc_apply_interface_mapping_to_expr (mapping, ref->u.ar.start[n]);
	    gfc_apply_interface_mapping_to_expr (mapping, ref->u.ar.end[n]);
	    gfc_apply_interface_mapping_to_expr (mapping, ref->u.ar.stride[n]);
	  }
	gfc_apply_interface_mapping_to_expr (mapping, ref->u.ar.offset);
	break;

      case REF_COMPONENT:
	break;

      case REF_SUBSTRING:
	gfc_apply_interface_mapping_to_expr (mapping, ref->u.ss.start);
	gfc_apply_interface_mapping_to_expr (mapping, ref->u.ss.end);
	break;
      }
}


/* Convert intrinsic function calls into result expressions.  */

static bool
gfc_map_intrinsic_function (gfc_expr *expr, gfc_interface_mapping *mapping)
{
  gfc_symbol *sym;
  gfc_expr *new_expr;
  gfc_expr *arg1;
  gfc_expr *arg2;
  int d, dup;

  arg1 = expr->value.function.actual->expr;
  if (expr->value.function.actual->next)
    arg2 = expr->value.function.actual->next->expr;
  else
    arg2 = NULL;

  sym = arg1->symtree->n.sym;

  if (sym->attr.dummy)
    return false;

  new_expr = NULL;

  switch (expr->value.function.isym->id)
    {
    case GFC_ISYM_LEN:
      /* TODO figure out why this condition is necessary.  */
      if (sym->attr.function
	  && (arg1->ts.u.cl->length == NULL
	      || (arg1->ts.u.cl->length->expr_type != EXPR_CONSTANT
		  && arg1->ts.u.cl->length->expr_type != EXPR_VARIABLE)))
	return false;

      new_expr = gfc_copy_expr (arg1->ts.u.cl->length);
      break;

    case GFC_ISYM_SIZE:
      if (!sym->as || sym->as->rank == 0)
	return false;

      if (arg2 && arg2->expr_type == EXPR_CONSTANT)
	{
	  dup = mpz_get_si (arg2->value.integer);
	  d = dup - 1;
	}
      else
	{
	  dup = sym->as->rank;
	  d = 0;
	}

      for (; d < dup; d++)
	{
	  gfc_expr *tmp;

	  if (!sym->as->upper[d] || !sym->as->lower[d])
	    {
	      gfc_free_expr (new_expr);
	      return false;
	    }

	  tmp = gfc_add (gfc_copy_expr (sym->as->upper[d]),
					gfc_get_int_expr (gfc_default_integer_kind,
							  NULL, 1));
	  tmp = gfc_subtract (tmp, gfc_copy_expr (sym->as->lower[d]));
	  if (new_expr)
	    new_expr = gfc_multiply (new_expr, tmp);
	  else
	    new_expr = tmp;
	}
      break;

    case GFC_ISYM_LBOUND:
    case GFC_ISYM_UBOUND:
	/* TODO These implementations of lbound and ubound do not limit if
	   the size < 0, according to F95's 13.14.53 and 13.14.113.  */

      if (!sym->as || sym->as->rank == 0)
	return false;

      if (arg2 && arg2->expr_type == EXPR_CONSTANT)
	d = mpz_get_si (arg2->value.integer) - 1;
      else
	/* TODO: If the need arises, this could produce an array of
	   ubound/lbounds.  */
	gcc_unreachable ();

      if (expr->value.function.isym->id == GFC_ISYM_LBOUND)
	{
	  if (sym->as->lower[d])
	    new_expr = gfc_copy_expr (sym->as->lower[d]);
	}
      else
	{
	  if (sym->as->upper[d])
	    new_expr = gfc_copy_expr (sym->as->upper[d]);
	}
      break;

    default:
      break;
    }

  gfc_apply_interface_mapping_to_expr (mapping, new_expr);
  if (!new_expr)
    return false;

  gfc_replace_expr (expr, new_expr);
  return true;
}


static void
gfc_map_fcn_formal_to_actual (gfc_expr *expr, gfc_expr *map_expr,
			      gfc_interface_mapping * mapping)
{
  gfc_formal_arglist *f;
  gfc_actual_arglist *actual;

  actual = expr->value.function.actual;
  f = map_expr->symtree->n.sym->formal;

  for (; f && actual; f = f->next, actual = actual->next)
    {
      if (!actual->expr)
	continue;

      gfc_add_interface_mapping (mapping, f->sym, NULL, actual->expr);
    }

  if (map_expr->symtree->n.sym->attr.dimension)
    {
      int d;
      gfc_array_spec *as;

      as = gfc_copy_array_spec (map_expr->symtree->n.sym->as);

      for (d = 0; d < as->rank; d++)
	{
	  gfc_apply_interface_mapping_to_expr (mapping, as->lower[d]);
	  gfc_apply_interface_mapping_to_expr (mapping, as->upper[d]);
	}

      expr->value.function.esym->as = as;
    }

  if (map_expr->symtree->n.sym->ts.type == BT_CHARACTER)
    {
      expr->value.function.esym->ts.u.cl->length
	= gfc_copy_expr (map_expr->symtree->n.sym->ts.u.cl->length);

      gfc_apply_interface_mapping_to_expr (mapping,
			expr->value.function.esym->ts.u.cl->length);
    }
}


/* EXPR is a copy of an expression that appeared in the interface
   associated with MAPPING.  Walk it recursively looking for references to
   dummy arguments that MAPPING maps to actual arguments.  Replace each such
   reference with a reference to the associated actual argument.  */

static void
gfc_apply_interface_mapping_to_expr (gfc_interface_mapping * mapping,
				     gfc_expr * expr)
{
  gfc_interface_sym_mapping *sym;
  gfc_actual_arglist *actual;

  if (!expr)
    return;

  /* Copying an expression does not copy its length, so do that here.  */
  if (expr->ts.type == BT_CHARACTER && expr->ts.u.cl)
    {
      expr->ts.u.cl = gfc_get_interface_mapping_charlen (mapping, expr->ts.u.cl);
      gfc_apply_interface_mapping_to_expr (mapping, expr->ts.u.cl->length);
    }

  /* Apply the mapping to any references.  */
  gfc_apply_interface_mapping_to_ref (mapping, expr->ref);

  /* ...and to the expression's symbol, if it has one.  */
  /* TODO Find out why the condition on expr->symtree had to be moved into
     the loop rather than being outside it, as originally.  */
  for (sym = mapping->syms; sym; sym = sym->next)
    if (expr->symtree && sym->old == expr->symtree->n.sym)
      {
	if (sym->new_sym->n.sym->backend_decl)
	  expr->symtree = sym->new_sym;
	else if (sym->expr)
	  gfc_replace_expr (expr, gfc_copy_expr (sym->expr));
	/* Replace base type for polymorphic arguments.  */
	if (expr->ref && expr->ref->type == REF_COMPONENT
	    && sym->expr && sym->expr->ts.type == BT_CLASS)
	  expr->ref->u.c.sym = sym->expr->ts.u.derived;
      }

      /* ...and to subexpressions in expr->value.  */
  switch (expr->expr_type)
    {
    case EXPR_VARIABLE:
    case EXPR_CONSTANT:
    case EXPR_NULL:
    case EXPR_SUBSTRING:
      break;

    case EXPR_OP:
      gfc_apply_interface_mapping_to_expr (mapping, expr->value.op.op1);
      gfc_apply_interface_mapping_to_expr (mapping, expr->value.op.op2);
      break;

    case EXPR_FUNCTION:
      for (actual = expr->value.function.actual; actual; actual = actual->next)
	gfc_apply_interface_mapping_to_expr (mapping, actual->expr);

      if (expr->value.function.esym == NULL
	    && expr->value.function.isym != NULL
	    && expr->value.function.actual->expr->symtree
	    && gfc_map_intrinsic_function (expr, mapping))
	break;

      for (sym = mapping->syms; sym; sym = sym->next)
	if (sym->old == expr->value.function.esym)
	  {
	    expr->value.function.esym = sym->new_sym->n.sym;
	    gfc_map_fcn_formal_to_actual (expr, sym->expr, mapping);
	    expr->value.function.esym->result = sym->new_sym->n.sym;
	  }
      break;

    case EXPR_ARRAY:
    case EXPR_STRUCTURE:
      gfc_apply_interface_mapping_to_cons (mapping, expr->value.constructor);
      break;

    case EXPR_COMPCALL:
    case EXPR_PPC:
      gcc_unreachable ();
      break;
    }

  return;
}


/* Evaluate interface expression EXPR using MAPPING.  Store the result
   in SE.  */

void
gfc_apply_interface_mapping (gfc_interface_mapping * mapping,
			     gfc_se * se, gfc_expr * expr)
{
  expr = gfc_copy_expr (expr);
  gfc_apply_interface_mapping_to_expr (mapping, expr);
  gfc_conv_expr (se, expr);
  se->expr = gfc_evaluate_now (se->expr, &se->pre);
  gfc_free_expr (expr);
}


/* Returns a reference to a temporary array into which a component of
   an actual argument derived type array is copied and then returned
   after the function call.  */
void
gfc_conv_subref_array_arg (gfc_se * parmse, gfc_expr * expr, int g77,
			   sym_intent intent, bool formal_ptr)
{
  gfc_se lse;
  gfc_se rse;
  gfc_ss *lss;
  gfc_ss *rss;
  gfc_loopinfo loop;
  gfc_loopinfo loop2;
  gfc_array_info *info;
  tree offset;
  tree tmp_index;
  tree tmp;
  tree base_type;
  tree size;
  stmtblock_t body;
  int n;
  int dimen;

  gcc_assert (expr->expr_type == EXPR_VARIABLE);

  gfc_init_se (&lse, NULL);
  gfc_init_se (&rse, NULL);

  /* Walk the argument expression.  */
  rss = gfc_walk_expr (expr);

  gcc_assert (rss != gfc_ss_terminator);
 
  /* Initialize the scalarizer.  */
  gfc_init_loopinfo (&loop);
  gfc_add_ss_to_loop (&loop, rss);

  /* Calculate the bounds of the scalarization.  */
  gfc_conv_ss_startstride (&loop);

  /* Build an ss for the temporary.  */
  if (expr->ts.type == BT_CHARACTER && !expr->ts.u.cl->backend_decl)
    gfc_conv_string_length (expr->ts.u.cl, expr, &parmse->pre);

  base_type = gfc_typenode_for_spec (&expr->ts);
  if (GFC_ARRAY_TYPE_P (base_type)
		|| GFC_DESCRIPTOR_TYPE_P (base_type))
    base_type = gfc_get_element_type (base_type);

  if (expr->ts.type == BT_CLASS)
    base_type = gfc_typenode_for_spec (&CLASS_DATA (expr)->ts);

  loop.temp_ss = gfc_get_temp_ss (base_type, ((expr->ts.type == BT_CHARACTER)
					      ? expr->ts.u.cl->backend_decl
					      : NULL),
				  loop.dimen);

  parmse->string_length = loop.temp_ss->info->string_length;

  /* Associate the SS with the loop.  */
  gfc_add_ss_to_loop (&loop, loop.temp_ss);

  /* Setup the scalarizing loops.  */
  gfc_conv_loop_setup (&loop, &expr->where);

  /* Pass the temporary descriptor back to the caller.  */
  info = &loop.temp_ss->info->data.array;
  parmse->expr = info->descriptor;

  /* Setup the gfc_se structures.  */
  gfc_copy_loopinfo_to_se (&lse, &loop);
  gfc_copy_loopinfo_to_se (&rse, &loop);

  rse.ss = rss;
  lse.ss = loop.temp_ss;
  gfc_mark_ss_chain_used (rss, 1);
  gfc_mark_ss_chain_used (loop.temp_ss, 1);

  /* Start the scalarized loop body.  */
  gfc_start_scalarized_body (&loop, &body);

  /* Translate the expression.  */
  gfc_conv_expr (&rse, expr);

  gfc_conv_tmp_array_ref (&lse);

  if (intent != INTENT_OUT)
    {
      tmp = gfc_trans_scalar_assign (&lse, &rse, expr->ts, true, false, true);
      gfc_add_expr_to_block (&body, tmp);
      gcc_assert (rse.ss == gfc_ss_terminator);
      gfc_trans_scalarizing_loops (&loop, &body);
    }
  else
    {
      /* Make sure that the temporary declaration survives by merging
       all the loop declarations into the current context.  */
      for (n = 0; n < loop.dimen; n++)
	{
	  gfc_merge_block_scope (&body);
	  body = loop.code[loop.order[n]];
	}
      gfc_merge_block_scope (&body);
    }

  /* Add the post block after the second loop, so that any
     freeing of allocated memory is done at the right time.  */
  gfc_add_block_to_block (&parmse->pre, &loop.pre);

  /**********Copy the temporary back again.*********/

  gfc_init_se (&lse, NULL);
  gfc_init_se (&rse, NULL);

  /* Walk the argument expression.  */
  lss = gfc_walk_expr (expr);
  rse.ss = loop.temp_ss;
  lse.ss = lss;

  /* Initialize the scalarizer.  */
  gfc_init_loopinfo (&loop2);
  gfc_add_ss_to_loop (&loop2, lss);

  /* Calculate the bounds of the scalarization.  */
  gfc_conv_ss_startstride (&loop2);

  /* Setup the scalarizing loops.  */
  gfc_conv_loop_setup (&loop2, &expr->where);

  gfc_copy_loopinfo_to_se (&lse, &loop2);
  gfc_copy_loopinfo_to_se (&rse, &loop2);

  gfc_mark_ss_chain_used (lss, 1);
  gfc_mark_ss_chain_used (loop.temp_ss, 1);

  /* Declare the variable to hold the temporary offset and start the
     scalarized loop body.  */
  offset = gfc_create_var (gfc_array_index_type, NULL);
  gfc_start_scalarized_body (&loop2, &body);

  /* Build the offsets for the temporary from the loop variables.  The
     temporary array has lbounds of zero and strides of one in all
     dimensions, so this is very simple.  The offset is only computed
     outside the innermost loop, so the overall transfer could be
     optimized further.  */
  info = &rse.ss->info->data.array;
  dimen = rse.ss->dimen;

  tmp_index = gfc_index_zero_node;
  for (n = dimen - 1; n > 0; n--)
    {
      tree tmp_str;
      tmp = rse.loop->loopvar[n];
      tmp = fold_build2_loc (input_location, MINUS_EXPR, gfc_array_index_type,
			     tmp, rse.loop->from[n]);
      tmp = fold_build2_loc (input_location, PLUS_EXPR, gfc_array_index_type,
			     tmp, tmp_index);

      tmp_str = fold_build2_loc (input_location, MINUS_EXPR,
				 gfc_array_index_type,
				 rse.loop->to[n-1], rse.loop->from[n-1]);
      tmp_str = fold_build2_loc (input_location, PLUS_EXPR,
				 gfc_array_index_type,
				 tmp_str, gfc_index_one_node);

      tmp_index = fold_build2_loc (input_location, MULT_EXPR,
				   gfc_array_index_type, tmp, tmp_str);
    }

  tmp_index = fold_build2_loc (input_location, MINUS_EXPR,
			       gfc_array_index_type,
			       tmp_index, rse.loop->from[0]);
  gfc_add_modify (&rse.loop->code[0], offset, tmp_index);

  tmp_index = fold_build2_loc (input_location, PLUS_EXPR,
			       gfc_array_index_type,
			       rse.loop->loopvar[0], offset);

  /* Now use the offset for the reference.  */
  tmp = build_fold_indirect_ref_loc (input_location,
				 info->data);
  rse.expr = gfc_build_array_ref (tmp, tmp_index, NULL);

  if (expr->ts.type == BT_CHARACTER)
    rse.string_length = expr->ts.u.cl->backend_decl;

  gfc_conv_expr (&lse, expr);

  gcc_assert (lse.ss == gfc_ss_terminator);

  tmp = gfc_trans_scalar_assign (&lse, &rse, expr->ts, false, false, true);
  gfc_add_expr_to_block (&body, tmp);
  
  /* Generate the copying loops.  */
  gfc_trans_scalarizing_loops (&loop2, &body);

  /* Wrap the whole thing up by adding the second loop to the post-block
     and following it by the post-block of the first loop.  In this way,
     if the temporary needs freeing, it is done after use!  */
  if (intent != INTENT_IN)
    {
      gfc_add_block_to_block (&parmse->post, &loop2.pre);
      gfc_add_block_to_block (&parmse->post, &loop2.post);
    }

  gfc_add_block_to_block (&parmse->post, &loop.post);

  gfc_cleanup_loop (&loop);
  gfc_cleanup_loop (&loop2);

  /* Pass the string length to the argument expression.  */
  if (expr->ts.type == BT_CHARACTER)
    parmse->string_length = expr->ts.u.cl->backend_decl;

  /* Determine the offset for pointer formal arguments and set the
     lbounds to one.  */
  if (formal_ptr)
    {
      size = gfc_index_one_node;
      offset = gfc_index_zero_node;  
      for (n = 0; n < dimen; n++)
	{
	  tmp = gfc_conv_descriptor_ubound_get (parmse->expr,
						gfc_rank_cst[n]);
	  tmp = fold_build2_loc (input_location, PLUS_EXPR,
				 gfc_array_index_type, tmp,
				 gfc_index_one_node);
	  gfc_conv_descriptor_ubound_set (&parmse->pre,
					  parmse->expr,
					  gfc_rank_cst[n],
					  tmp);
	  gfc_conv_descriptor_lbound_set (&parmse->pre,
					  parmse->expr,
					  gfc_rank_cst[n],
					  gfc_index_one_node);
	  size = gfc_evaluate_now (size, &parmse->pre);
	  offset = fold_build2_loc (input_location, MINUS_EXPR,
				    gfc_array_index_type,
				    offset, size);
	  offset = gfc_evaluate_now (offset, &parmse->pre);
	  tmp = fold_build2_loc (input_location, MINUS_EXPR,
				 gfc_array_index_type,
				 rse.loop->to[n], rse.loop->from[n]);
	  tmp = fold_build2_loc (input_location, PLUS_EXPR,
				 gfc_array_index_type,
				 tmp, gfc_index_one_node);
	  size = fold_build2_loc (input_location, MULT_EXPR,
				  gfc_array_index_type, size, tmp);
	}

      gfc_conv_descriptor_offset_set (&parmse->pre, parmse->expr,
				      offset);
    }

  /* We want either the address for the data or the address of the descriptor,
     depending on the mode of passing array arguments.  */
  if (g77)
    parmse->expr = gfc_conv_descriptor_data_get (parmse->expr);
  else
    parmse->expr = gfc_build_addr_expr (NULL_TREE, parmse->expr);

  return;
}


/* Generate the code for argument list functions.  */

static void
conv_arglist_function (gfc_se *se, gfc_expr *expr, const char *name)
{
  /* Pass by value for g77 %VAL(arg), pass the address
     indirectly for %LOC, else by reference.  Thus %REF
     is a "do-nothing" and %LOC is the same as an F95
     pointer.  */
  if (strncmp (name, "%VAL", 4) == 0)
    gfc_conv_expr (se, expr);
  else if (strncmp (name, "%LOC", 4) == 0)
    {
      gfc_conv_expr_reference (se, expr);
      se->expr = gfc_build_addr_expr (NULL, se->expr);
    }
  else if (strncmp (name, "%REF", 4) == 0)
    gfc_conv_expr_reference (se, expr);
  else
    gfc_error ("Unknown argument list function at %L", &expr->where);
}


/* The following routine generates code for the intrinsic
   procedures from the ISO_C_BINDING module:
    * C_LOC           (function)
    * C_FUNLOC        (function)
    * C_F_POINTER     (subroutine)
    * C_F_PROCPOINTER (subroutine)
    * C_ASSOCIATED    (function)
   One exception which is not handled here is C_F_POINTER with non-scalar
   arguments. Returns 1 if the call was replaced by inline code (else: 0).  */

static int
conv_isocbinding_procedure (gfc_se * se, gfc_symbol * sym,
			    gfc_actual_arglist * arg)
{
  gfc_symbol *fsym;
  gfc_ss *argss;
    
  if (sym->intmod_sym_id == ISOCBINDING_LOC)
    {
      if (arg->expr->rank == 0)
	gfc_conv_expr_reference (se, arg->expr);
      else
	{
	  int f;
	  /* This is really the actual arg because no formal arglist is
	     created for C_LOC.	 */
	  fsym = arg->expr->symtree->n.sym;

	  /* We should want it to do g77 calling convention.  */
	  f = (fsym != NULL)
	    && !(fsym->attr.pointer || fsym->attr.allocatable)
	    && fsym->as->type != AS_ASSUMED_SHAPE;
	  f = f || !sym->attr.always_explicit;
      
	  argss = gfc_walk_expr (arg->expr);
	  gfc_conv_array_parameter (se, arg->expr, argss, f,
				    NULL, NULL, NULL);
	}

      /* TODO -- the following two lines shouldn't be necessary, but if
	 they're removed, a bug is exposed later in the code path.
	 This workaround was thus introduced, but will have to be
	 removed; please see PR 35150 for details about the issue.  */
      se->expr = convert (pvoid_type_node, se->expr);
      se->expr = gfc_evaluate_now (se->expr, &se->pre);

      return 1;
    }
  else if (sym->intmod_sym_id == ISOCBINDING_FUNLOC)
    {
      arg->expr->ts.type = sym->ts.u.derived->ts.type;
      arg->expr->ts.f90_type = sym->ts.u.derived->ts.f90_type;
      arg->expr->ts.kind = sym->ts.u.derived->ts.kind;
      gfc_conv_expr_reference (se, arg->expr);
  
      return 1;
    }
  else if ((sym->intmod_sym_id == ISOCBINDING_F_POINTER
	    && arg->next->expr->rank == 0)
	   || sym->intmod_sym_id == ISOCBINDING_F_PROCPOINTER)
    {
      /* Convert c_f_pointer if fptr is a scalar
	 and convert c_f_procpointer.  */
      gfc_se cptrse;
      gfc_se fptrse;

      gfc_init_se (&cptrse, NULL);
      gfc_conv_expr (&cptrse, arg->expr);
      gfc_add_block_to_block (&se->pre, &cptrse.pre);
      gfc_add_block_to_block (&se->post, &cptrse.post);

      gfc_init_se (&fptrse, NULL);
      if (sym->intmod_sym_id == ISOCBINDING_F_POINTER
	  || gfc_is_proc_ptr_comp (arg->next->expr, NULL))
	fptrse.want_pointer = 1;

      gfc_conv_expr (&fptrse, arg->next->expr);
      gfc_add_block_to_block (&se->pre, &fptrse.pre);
      gfc_add_block_to_block (&se->post, &fptrse.post);
      
      if (arg->next->expr->symtree->n.sym->attr.proc_pointer
	  && arg->next->expr->symtree->n.sym->attr.dummy)
	fptrse.expr = build_fold_indirect_ref_loc (input_location,
						   fptrse.expr);
      
      se->expr = fold_build2_loc (input_location, MODIFY_EXPR,
				  TREE_TYPE (fptrse.expr),
				  fptrse.expr,
				  fold_convert (TREE_TYPE (fptrse.expr),
						cptrse.expr));

      return 1;
    }
  else if (sym->intmod_sym_id == ISOCBINDING_ASSOCIATED)
    {
      gfc_se arg1se;
      gfc_se arg2se;

      /* Build the addr_expr for the first argument.  The argument is
	 already an *address* so we don't need to set want_pointer in
	 the gfc_se.  */
      gfc_init_se (&arg1se, NULL);
      gfc_conv_expr (&arg1se, arg->expr);
      gfc_add_block_to_block (&se->pre, &arg1se.pre);
      gfc_add_block_to_block (&se->post, &arg1se.post);

      /* See if we were given two arguments.  */
      if (arg->next == NULL)
	/* Only given one arg so generate a null and do a
	   not-equal comparison against the first arg.  */
	se->expr = fold_build2_loc (input_location, NE_EXPR, boolean_type_node,
				    arg1se.expr,
				    fold_convert (TREE_TYPE (arg1se.expr),
						  null_pointer_node));
      else
	{
	  tree eq_expr;
	  tree not_null_expr;
	  
	  /* Given two arguments so build the arg2se from second arg.  */
	  gfc_init_se (&arg2se, NULL);
	  gfc_conv_expr (&arg2se, arg->next->expr);
	  gfc_add_block_to_block (&se->pre, &arg2se.pre);
	  gfc_add_block_to_block (&se->post, &arg2se.post);

	  /* Generate test to compare that the two args are equal.  */
	  eq_expr = fold_build2_loc (input_location, EQ_EXPR, boolean_type_node,
				     arg1se.expr, arg2se.expr);
	  /* Generate test to ensure that the first arg is not null.  */
	  not_null_expr = fold_build2_loc (input_location, NE_EXPR,
					   boolean_type_node,
					   arg1se.expr, null_pointer_node);

	  /* Finally, the generated test must check that both arg1 is not
	     NULL and that it is equal to the second arg.  */
	  se->expr = fold_build2_loc (input_location, TRUTH_AND_EXPR,
				      boolean_type_node,
				      not_null_expr, eq_expr);
	}

      return 1;
    }
    
  /* Nothing was done.  */
  return 0;
}


/* Generate code for a procedure call.  Note can return se->post != NULL.
   If se->direct_byref is set then se->expr contains the return parameter.
   Return nonzero, if the call has alternate specifiers.
   'expr' is only needed for procedure pointer components.  */

int
gfc_conv_procedure_call (gfc_se * se, gfc_symbol * sym,
			 gfc_actual_arglist * args, gfc_expr * expr,
			 VEC(tree,gc) *append_args)
{
  gfc_interface_mapping mapping;
  VEC(tree,gc) *arglist;
  VEC(tree,gc) *retargs;
  tree tmp;
  tree fntype;
  gfc_se parmse;
  gfc_ss *argss;
  gfc_array_info *info;
  int byref;
  int parm_kind;
  tree type;
  tree var;
  tree len;
  tree base_object;
  VEC(tree,gc) *stringargs;
  tree result = NULL;
  gfc_formal_arglist *formal;
  gfc_actual_arglist *arg;
  int has_alternate_specifier = 0;
  bool need_interface_mapping;
  bool callee_alloc;
  gfc_typespec ts;
  gfc_charlen cl;
  gfc_expr *e;
  gfc_symbol *fsym;
  stmtblock_t post;
  enum {MISSING = 0, ELEMENTAL, SCALAR, SCALAR_POINTER, ARRAY};
  gfc_component *comp = NULL;
  int arglen;

  arglist = NULL;
  retargs = NULL;
  stringargs = NULL;
  var = NULL_TREE;
  len = NULL_TREE;
  gfc_clear_ts (&ts);

  if (sym->from_intmod == INTMOD_ISO_C_BINDING
      && conv_isocbinding_procedure (se, sym, args))
    return 0;

  gfc_is_proc_ptr_comp (expr, &comp);

  if (se->ss != NULL)
    {
      if (!sym->attr.elemental && !(comp && comp->attr.elemental))
	{
	  gcc_assert (se->ss->info->type == GFC_SS_FUNCTION);
	  if (se->ss->info->useflags)
	    {
	      gcc_assert ((!comp && gfc_return_by_reference (sym)
			   && sym->result->attr.dimension)
			  || (comp && comp->attr.dimension));
	      gcc_assert (se->loop != NULL);

	      /* Access the previously obtained result.  */
	      gfc_conv_tmp_array_ref (se);
	      return 0;
	    }
	}
      info = &se->ss->info->data.array;
    }
  else
    info = NULL;

  gfc_init_block (&post);
  gfc_init_interface_mapping (&mapping);
  if (!comp)
    {
      formal = sym->formal;
      need_interface_mapping = sym->attr.dimension ||
			       (sym->ts.type == BT_CHARACTER
				&& sym->ts.u.cl->length
				&& sym->ts.u.cl->length->expr_type
				   != EXPR_CONSTANT);
    }
  else
    {
      formal = comp->formal;
      need_interface_mapping = comp->attr.dimension ||
			       (comp->ts.type == BT_CHARACTER
				&& comp->ts.u.cl->length
				&& comp->ts.u.cl->length->expr_type
				   != EXPR_CONSTANT);
    }

  base_object = NULL_TREE;

  /* Evaluate the arguments.  */
  for (arg = args; arg != NULL;
       arg = arg->next, formal = formal ? formal->next : NULL)
    {
      e = arg->expr;
      fsym = formal ? formal->sym : NULL;
      parm_kind = MISSING;

      /* Class array expressions are sometimes coming completely unadorned
	 with either arrayspec or _data component.  Correct that here.
	 OOP-TODO: Move this to the frontend.  */
      if (e && e->expr_type == EXPR_VARIABLE
	    && !e->ref
	    && e->ts.type == BT_CLASS
	    && CLASS_DATA (e)->attr.dimension)
	{
	  gfc_typespec temp_ts = e->ts;
	  gfc_add_class_array_ref (e);
	  e->ts = temp_ts;
	}

      if (e == NULL)
	{
	  if (se->ignore_optional)
	    {
	      /* Some intrinsics have already been resolved to the correct
	         parameters.  */
	      continue;
	    }
	  else if (arg->label)
	    {
	      has_alternate_specifier = 1;
	      continue;
	    }
	  else
	    {
	      /* Pass a NULL pointer for an absent arg.  */
	      gfc_init_se (&parmse, NULL);
	      parmse.expr = null_pointer_node;
	      if (arg->missing_arg_type == BT_CHARACTER)
		parmse.string_length = build_int_cst (gfc_charlen_type_node, 0);
	    }
	}
      else if (arg->expr->expr_type == EXPR_NULL && fsym && !fsym->attr.pointer)
	{
	  /* Pass a NULL pointer to denote an absent arg.  */
	  gcc_assert (fsym->attr.optional && !fsym->attr.allocatable);
	  gfc_init_se (&parmse, NULL);
	  parmse.expr = null_pointer_node;
	  if (arg->missing_arg_type == BT_CHARACTER)
	    parmse.string_length = build_int_cst (gfc_charlen_type_node, 0);
	}
      else if (fsym && fsym->ts.type == BT_CLASS
		 && e->ts.type == BT_DERIVED)
	{
	  /* The derived type needs to be converted to a temporary
	     CLASS object.  */
	  gfc_init_se (&parmse, se);
	  gfc_conv_derived_to_class (&parmse, e, fsym->ts);
	}
      else if (se->ss && se->ss->info->useflags)
	{
	  /* An elemental function inside a scalarized loop.  */
	  gfc_init_se (&parmse, se);
	  parm_kind = ELEMENTAL;

	  if (se->ss->dimen > 0
	      && se->ss->info->data.array.ref == NULL)
	    {
	      gfc_conv_tmp_array_ref (&parmse);
	      if (e->ts.type == BT_CHARACTER)
		gfc_conv_string_parameter (&parmse);
	      else
		parmse.expr = gfc_build_addr_expr (NULL_TREE, parmse.expr);
	    }
	  else
	    gfc_conv_expr_reference (&parmse, e);

	  /* The scalarizer does not repackage the reference to a class
	     array - instead it returns a pointer to the data element.  */
	  if (fsym && fsym->ts.type == BT_CLASS && e->ts.type == BT_CLASS)
	    gfc_conv_class_to_class (&parmse, e, fsym->ts, true);
	}
      else
	{
	  /* A scalar or transformational function.  */
	  gfc_init_se (&parmse, NULL);
	  argss = gfc_walk_expr (e);

	  if (argss == gfc_ss_terminator)
	    {
	      if (e->expr_type == EXPR_VARIABLE
		    && e->symtree->n.sym->attr.cray_pointee
		    && fsym && fsym->attr.flavor == FL_PROCEDURE)
		{
		    /* The Cray pointer needs to be converted to a pointer to
		       a type given by the expression.  */
		    gfc_conv_expr (&parmse, e);
		    type = build_pointer_type (TREE_TYPE (parmse.expr));
		    tmp = gfc_get_symbol_decl (e->symtree->n.sym->cp_pointer);
		    parmse.expr = convert (type, tmp);
		}
 	      else if (fsym && fsym->attr.value)
		{
		  if (fsym->ts.type == BT_CHARACTER
		      && fsym->ts.is_c_interop
		      && fsym->ns->proc_name != NULL
		      && fsym->ns->proc_name->attr.is_bind_c)
		    {
		      parmse.expr = NULL;
		      gfc_conv_scalar_char_value (fsym, &parmse, &e);
		      if (parmse.expr == NULL)
			gfc_conv_expr (&parmse, e);
		    }
		  else
		    gfc_conv_expr (&parmse, e);
		}
	      else if (arg->name && arg->name[0] == '%')
		/* Argument list functions %VAL, %LOC and %REF are signalled
		   through arg->name.  */
		conv_arglist_function (&parmse, arg->expr, arg->name);
	      else if ((e->expr_type == EXPR_FUNCTION)
			&& ((e->value.function.esym
			     && e->value.function.esym->result->attr.pointer)
			    || (!e->value.function.esym
				&& e->symtree->n.sym->attr.pointer))
			&& fsym && fsym->attr.target)
		{
		  gfc_conv_expr (&parmse, e);
		  parmse.expr = gfc_build_addr_expr (NULL_TREE, parmse.expr);
		}
	      else if (e->expr_type == EXPR_FUNCTION
		       && e->symtree->n.sym->result
		       && e->symtree->n.sym->result != e->symtree->n.sym
		       && e->symtree->n.sym->result->attr.proc_pointer)
		{
		  /* Functions returning procedure pointers.  */
		  gfc_conv_expr (&parmse, e);
		  if (fsym && fsym->attr.proc_pointer)
		    parmse.expr = gfc_build_addr_expr (NULL_TREE, parmse.expr);
		}
	      else
		{
		  gfc_conv_expr_reference (&parmse, e);

		  /* Catch base objects that are not variables.  */
		  if (e->ts.type == BT_CLASS
			&& e->expr_type != EXPR_VARIABLE
			&& expr && e == expr->base_expr)
		    base_object = build_fold_indirect_ref_loc (input_location,
							       parmse.expr);

		  /* A class array element needs converting back to be a
		     class object, if the formal argument is a class object.  */
		  if (fsym && fsym->ts.type == BT_CLASS
			&& e->ts.type == BT_CLASS
			&& CLASS_DATA (e)->attr.dimension)
		    gfc_conv_class_to_class (&parmse, e, fsym->ts, false);

		  /* If an ALLOCATABLE dummy argument has INTENT(OUT) and is 
		     allocated on entry, it must be deallocated.  */
		  if (fsym && fsym->attr.allocatable
		      && fsym->attr.intent == INTENT_OUT)
		    {
		      stmtblock_t block;

		      gfc_init_block  (&block);
		      tmp = gfc_deallocate_with_status (parmse.expr, NULL_TREE,
							true, NULL);
		      gfc_add_expr_to_block (&block, tmp);
		      tmp = fold_build2_loc (input_location, MODIFY_EXPR,
					     void_type_node, parmse.expr,
					     null_pointer_node);
		      gfc_add_expr_to_block (&block, tmp);

		      if (fsym->attr.optional
			  && e->expr_type == EXPR_VARIABLE
			  && e->symtree->n.sym->attr.optional)
			{
			  tmp = fold_build3_loc (input_location, COND_EXPR,
				     void_type_node,
				     gfc_conv_expr_present (e->symtree->n.sym),
					    gfc_finish_block (&block),
					    build_empty_stmt (input_location));
			}
		      else
			tmp = gfc_finish_block (&block);

		      gfc_add_expr_to_block (&se->pre, tmp);
		    }

		  if (fsym && e->expr_type != EXPR_NULL
		      && ((fsym->attr.pointer
			   && fsym->attr.flavor != FL_PROCEDURE)
			  || (fsym->attr.proc_pointer
			      && !(e->expr_type == EXPR_VARIABLE
				   && e->symtree->n.sym->attr.dummy))
			  || (fsym->attr.proc_pointer
			      && e->expr_type == EXPR_VARIABLE
			      && gfc_is_proc_ptr_comp (e, NULL))
			  || fsym->attr.allocatable))
		    {
		      /* Scalar pointer dummy args require an extra level of
			 indirection. The null pointer already contains
			 this level of indirection.  */
		      parm_kind = SCALAR_POINTER;
		      parmse.expr = gfc_build_addr_expr (NULL_TREE, parmse.expr);
		    }
		}
	    }
	  else if (e->ts.type == BT_CLASS
		    && fsym && fsym->ts.type == BT_CLASS
		    && CLASS_DATA (fsym)->attr.dimension)
	    {
	      /* Pass a class array.  */
	      gfc_init_se (&parmse, se);
	      gfc_conv_expr_descriptor (&parmse, e, argss);
	      /* The conversion does not repackage the reference to a class
	         array - _data descriptor.  */
	      gfc_conv_class_to_class (&parmse, e, fsym->ts, false);
	    }
	  else
	    {
              /* If the procedure requires an explicit interface, the actual
                 argument is passed according to the corresponding formal
                 argument.  If the corresponding formal argument is a POINTER,
                 ALLOCATABLE or assumed shape, we do not use g77's calling
                 convention, and pass the address of the array descriptor
                 instead. Otherwise we use g77's calling convention.  */
	      bool f;
	      f = (fsym != NULL)
		  && !(fsym->attr.pointer || fsym->attr.allocatable)
		  && fsym->as && fsym->as->type != AS_ASSUMED_SHAPE;
	      if (comp)
		f = f || !comp->attr.always_explicit;
	      else
		f = f || !sym->attr.always_explicit;

	      /* If the argument is a function call that may not create
		 a temporary for the result, we have to check that we
		 can do it, i.e. that there is no alias between this 
		 argument and another one.  */
	      if (gfc_get_noncopying_intrinsic_argument (e) != NULL)
		{
		  gfc_expr *iarg;
		  sym_intent intent;

		  if (fsym != NULL)
		    intent = fsym->attr.intent;
		  else
		    intent = INTENT_UNKNOWN;

		  if (gfc_check_fncall_dependency (e, intent, sym, args,
						   NOT_ELEMENTAL))
		    parmse.force_tmp = 1;

		  iarg = e->value.function.actual->expr;

		  /* Temporary needed if aliasing due to host association.  */
		  if (sym->attr.contained
			&& !sym->attr.pure
			&& !sym->attr.implicit_pure
			&& !sym->attr.use_assoc
			&& iarg->expr_type == EXPR_VARIABLE
			&& sym->ns == iarg->symtree->n.sym->ns)
		    parmse.force_tmp = 1;

		  /* Ditto within module.  */
		  if (sym->attr.use_assoc
			&& !sym->attr.pure
			&& !sym->attr.implicit_pure
			&& iarg->expr_type == EXPR_VARIABLE
			&& sym->module == iarg->symtree->n.sym->module)
		    parmse.force_tmp = 1;
		}

	      if (e->expr_type == EXPR_VARIABLE
		    && is_subref_array (e))
		/* The actual argument is a component reference to an
		   array of derived types.  In this case, the argument
		   is converted to a temporary, which is passed and then
		   written back after the procedure call.  */
		gfc_conv_subref_array_arg (&parmse, e, f,
				fsym ? fsym->attr.intent : INTENT_INOUT,
				fsym && fsym->attr.pointer);
	      else if (gfc_is_class_array_ref (e, NULL)
			 && fsym && fsym->ts.type == BT_DERIVED)
		/* The actual argument is a component reference to an
		   array of derived types.  In this case, the argument
		   is converted to a temporary, which is passed and then
		   written back after the procedure call.
		   OOP-TODO: Insert code so that if the dynamic type is
		   the same as the declared type, copy-in/copy-out does
		   not occur.  */
		gfc_conv_subref_array_arg (&parmse, e, f,
				fsym ? fsym->attr.intent : INTENT_INOUT,
				fsym && fsym->attr.pointer);
	      else
	        gfc_conv_array_parameter (&parmse, e, argss, f, fsym,
					  sym->name, NULL);

	      /* If an ALLOCATABLE dummy argument has INTENT(OUT) and is 
		 allocated on entry, it must be deallocated.  */
	      if (fsym && fsym->attr.allocatable
		  && fsym->attr.intent == INTENT_OUT)
		{
		  tmp = build_fold_indirect_ref_loc (input_location,
						     parmse.expr);
		  tmp = gfc_trans_dealloc_allocated (tmp);
		  if (fsym->attr.optional
		      && e->expr_type == EXPR_VARIABLE
		      && e->symtree->n.sym->attr.optional)
		    tmp = fold_build3_loc (input_location, COND_EXPR,
				     void_type_node,
				     gfc_conv_expr_present (e->symtree->n.sym),
				       tmp, build_empty_stmt (input_location));
		  gfc_add_expr_to_block (&se->pre, tmp);
		}
	    } 
	}

      /* The case with fsym->attr.optional is that of a user subroutine
	 with an interface indicating an optional argument.  When we call
	 an intrinsic subroutine, however, fsym is NULL, but we might still
	 have an optional argument, so we proceed to the substitution
	 just in case.  */
      if (e && (fsym == NULL || fsym->attr.optional))
	{
	  /* If an optional argument is itself an optional dummy argument,
	     check its presence and substitute a null if absent.  This is
	     only needed when passing an array to an elemental procedure
	     as then array elements are accessed - or no NULL pointer is
	     allowed and a "1" or "0" should be passed if not present.
	     When passing a non-array-descriptor full array to a
	     non-array-descriptor dummy, no check is needed. For
	     array-descriptor actual to array-descriptor dummy, see
	     PR 41911 for why a check has to be inserted.
	     fsym == NULL is checked as intrinsics required the descriptor
	     but do not always set fsym.  */
	  if (e->expr_type == EXPR_VARIABLE
	      && e->symtree->n.sym->attr.optional
	      && ((e->rank > 0 && sym->attr.elemental)
		  || e->representation.length || e->ts.type == BT_CHARACTER
		  || (e->rank > 0
		      && (fsym == NULL 
			  || (fsym-> as
			      && (fsym->as->type == AS_ASSUMED_SHAPE
			      	  || fsym->as->type == AS_DEFERRED))))))
	    gfc_conv_missing_dummy (&parmse, e, fsym ? fsym->ts : e->ts,
				    e->representation.length);
	}

      if (fsym && e)
	{
	  /* Obtain the character length of an assumed character length
	     length procedure from the typespec.  */
	  if (fsym->ts.type == BT_CHARACTER
	      && parmse.string_length == NULL_TREE
	      && e->ts.type == BT_PROCEDURE
	      && e->symtree->n.sym->ts.type == BT_CHARACTER
	      && e->symtree->n.sym->ts.u.cl->length != NULL
	      && e->symtree->n.sym->ts.u.cl->length->expr_type == EXPR_CONSTANT)
	    {
	      gfc_conv_const_charlen (e->symtree->n.sym->ts.u.cl);
	      parmse.string_length = e->symtree->n.sym->ts.u.cl->backend_decl;
	    }
	}

      if (fsym && need_interface_mapping && e)
	gfc_add_interface_mapping (&mapping, fsym, &parmse, e);

      gfc_add_block_to_block (&se->pre, &parmse.pre);
      gfc_add_block_to_block (&post, &parmse.post);

      /* Allocated allocatable components of derived types must be
	 deallocated for non-variable scalars.  Non-variable arrays are
	 dealt with in trans-array.c(gfc_conv_array_parameter).  */
      if (e && e->ts.type == BT_DERIVED
	    && e->ts.u.derived->attr.alloc_comp
	    && !(e->symtree && e->symtree->n.sym->attr.pointer)
	    && (e->expr_type != EXPR_VARIABLE && !e->rank))
        {
	  int parm_rank;
	  tmp = build_fold_indirect_ref_loc (input_location,
					 parmse.expr);
	  parm_rank = e->rank;
	  switch (parm_kind)
	    {
	    case (ELEMENTAL):
	    case (SCALAR):
	      parm_rank = 0;
	      break;

	    case (SCALAR_POINTER):
              tmp = build_fold_indirect_ref_loc (input_location,
					     tmp);
	      break;
	    }

	  if (e->expr_type == EXPR_OP
		&& e->value.op.op == INTRINSIC_PARENTHESES
		&& e->value.op.op1->expr_type == EXPR_VARIABLE)
	    {
	      tree local_tmp;
	      local_tmp = gfc_evaluate_now (tmp, &se->pre);
	      local_tmp = gfc_copy_alloc_comp (e->ts.u.derived, local_tmp, tmp, parm_rank);
	      gfc_add_expr_to_block (&se->post, local_tmp);
	    }

	  tmp = gfc_deallocate_alloc_comp (e->ts.u.derived, tmp, parm_rank);

	  gfc_add_expr_to_block (&se->post, tmp);
        }

      /* Add argument checking of passing an unallocated/NULL actual to
         a nonallocatable/nonpointer dummy.  */

      if (gfc_option.rtcheck & GFC_RTCHECK_POINTER && e != NULL)
        {
	  symbol_attribute attr;
	  char *msg;
	  tree cond;

	  if (e->expr_type == EXPR_VARIABLE || e->expr_type == EXPR_FUNCTION)
	    attr = gfc_expr_attr (e);
	  else
	    goto end_pointer_check;

	  /*  In Fortran 2008 it's allowed to pass a NULL pointer/nonallocated
	      allocatable to an optional dummy, cf. 12.5.2.12.  */
	  if (fsym != NULL && fsym->attr.optional && !attr.proc_pointer
	      && (gfc_option.allow_std & GFC_STD_F2008) != 0)
	    goto end_pointer_check;

          if (attr.optional)
	    {
              /* If the actual argument is an optional pointer/allocatable and
		 the formal argument takes an nonpointer optional value,
		 it is invalid to pass a non-present argument on, even
		 though there is no technical reason for this in gfortran.
		 See Fortran 2003, Section 12.4.1.6 item (7)+(8).  */
	      tree present, null_ptr, type;

	      if (attr.allocatable
		  && (fsym == NULL || !fsym->attr.allocatable))
		asprintf (&msg, "Allocatable actual argument '%s' is not "
			  "allocated or not present", e->symtree->n.sym->name);
	      else if (attr.pointer
		       && (fsym == NULL || !fsym->attr.pointer))
		asprintf (&msg, "Pointer actual argument '%s' is not "
			  "associated or not present",
			  e->symtree->n.sym->name);
	      else if (attr.proc_pointer
		       && (fsym == NULL || !fsym->attr.proc_pointer))
		asprintf (&msg, "Proc-pointer actual argument '%s' is not "
			  "associated or not present",
			  e->symtree->n.sym->name);
	      else
		goto end_pointer_check;

	      present = gfc_conv_expr_present (e->symtree->n.sym);
	      type = TREE_TYPE (present);
	      present = fold_build2_loc (input_location, EQ_EXPR,
					 boolean_type_node, present,
					 fold_convert (type,
						       null_pointer_node));
	      type = TREE_TYPE (parmse.expr);
	      null_ptr = fold_build2_loc (input_location, EQ_EXPR,
					  boolean_type_node, parmse.expr,
					  fold_convert (type,
							null_pointer_node));
	      cond = fold_build2_loc (input_location, TRUTH_ORIF_EXPR,
				      boolean_type_node, present, null_ptr);
	    }
          else
	    {
	      if (attr.allocatable
		  && (fsym == NULL || !fsym->attr.allocatable))
		asprintf (&msg, "Allocatable actual argument '%s' is not "
		      "allocated", e->symtree->n.sym->name);
	      else if (attr.pointer
		       && (fsym == NULL || !fsym->attr.pointer))
		asprintf (&msg, "Pointer actual argument '%s' is not "
		      "associated", e->symtree->n.sym->name);
	      else if (attr.proc_pointer
		       && (fsym == NULL || !fsym->attr.proc_pointer))
		asprintf (&msg, "Proc-pointer actual argument '%s' is not "
		      "associated", e->symtree->n.sym->name);
	      else
		goto end_pointer_check;

	      tmp = parmse.expr;

	      /* If the argument is passed by value, we need to strip the
		 INDIRECT_REF.  */
	      if (!POINTER_TYPE_P (TREE_TYPE (parmse.expr)))
		tmp = gfc_build_addr_expr (NULL_TREE, tmp);

	      cond = fold_build2_loc (input_location, EQ_EXPR,
				      boolean_type_node, tmp,
				      fold_convert (TREE_TYPE (tmp),
						    null_pointer_node));
	    }
 
	  gfc_trans_runtime_check (true, false, cond, &se->pre, &e->where,
				   msg);
	  free (msg);
        }
      end_pointer_check:

      /* Deferred length dummies pass the character length by reference
	 so that the value can be returned.  */
      if (parmse.string_length && fsym && fsym->ts.deferred)
	{
	  tmp = parmse.string_length;
	  if (TREE_CODE (tmp) != VAR_DECL)
	    tmp = gfc_evaluate_now (parmse.string_length, &se->pre);
	  parmse.string_length = gfc_build_addr_expr (NULL_TREE, tmp);
	}

      /* Character strings are passed as two parameters, a length and a
         pointer - except for Bind(c) which only passes the pointer.  */
      if (parmse.string_length != NULL_TREE && !sym->attr.is_bind_c)
	VEC_safe_push (tree, gc, stringargs, parmse.string_length);

      /* For descriptorless coarrays and assumed-shape coarray dummies, we
	 pass the token and the offset as additional arguments.  */
      if (fsym && fsym->attr.codimension
	  && gfc_option.coarray == GFC_FCOARRAY_LIB
	  && !fsym->attr.allocatable
	  && e == NULL)
	{
	  /* Token and offset. */
	  VEC_safe_push (tree, gc, stringargs, null_pointer_node);
	  VEC_safe_push (tree, gc, stringargs,
			 build_int_cst (gfc_array_index_type, 0));
	  gcc_assert (fsym->attr.optional);
	}
      else if (fsym && fsym->attr.codimension
	       && !fsym->attr.allocatable
	       && gfc_option.coarray == GFC_FCOARRAY_LIB)
	{
	  tree caf_decl, caf_type;
	  tree offset, tmp2;

	  caf_decl = get_tree_for_caf_expr (e);
	  caf_type = TREE_TYPE (caf_decl);

	  if (GFC_DESCRIPTOR_TYPE_P (caf_type)
	      && GFC_TYPE_ARRAY_AKIND (caf_type) == GFC_ARRAY_ALLOCATABLE)
	    tmp = gfc_conv_descriptor_token (caf_decl);
	  else if (DECL_LANG_SPECIFIC (caf_decl)
		   && GFC_DECL_TOKEN (caf_decl) != NULL_TREE)
	    tmp = GFC_DECL_TOKEN (caf_decl);
	  else
	    {
	      gcc_assert (GFC_ARRAY_TYPE_P (caf_type)
			  && GFC_TYPE_ARRAY_CAF_TOKEN (caf_type) != NULL_TREE);
	      tmp = GFC_TYPE_ARRAY_CAF_TOKEN (caf_type);
	    }
	  
	  VEC_safe_push (tree, gc, stringargs, tmp);

	  if (GFC_DESCRIPTOR_TYPE_P (caf_type)
	      && GFC_TYPE_ARRAY_AKIND (caf_type) == GFC_ARRAY_ALLOCATABLE)
	    offset = build_int_cst (gfc_array_index_type, 0);
	  else if (DECL_LANG_SPECIFIC (caf_decl)
		   && GFC_DECL_CAF_OFFSET (caf_decl) != NULL_TREE)
	    offset = GFC_DECL_CAF_OFFSET (caf_decl);
	  else if (GFC_TYPE_ARRAY_CAF_OFFSET (caf_type) != NULL_TREE)
	    offset = GFC_TYPE_ARRAY_CAF_OFFSET (caf_type);
	  else
	    offset = build_int_cst (gfc_array_index_type, 0);

	  if (GFC_DESCRIPTOR_TYPE_P (caf_type))
	    tmp = gfc_conv_descriptor_data_get (caf_decl);
	  else
	    {
	      gcc_assert (POINTER_TYPE_P (caf_type));
	      tmp = caf_decl;
	    }

          if (fsym->as->type == AS_ASSUMED_SHAPE)
	    {
	      gcc_assert (POINTER_TYPE_P (TREE_TYPE (parmse.expr)));
	      gcc_assert (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE
						   (TREE_TYPE (parmse.expr))));
	      tmp2 = build_fold_indirect_ref_loc (input_location, parmse.expr);
	      tmp2 = gfc_conv_descriptor_data_get (tmp2);
	    }
	  else if (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (parmse.expr)))
	    tmp2 = gfc_conv_descriptor_data_get (parmse.expr);
	  else
	    {
	      gcc_assert (POINTER_TYPE_P (TREE_TYPE (parmse.expr)));
	      tmp2 = parmse.expr;
	    }

	  tmp = fold_build2_loc (input_location, MINUS_EXPR,
                                 gfc_array_index_type,
                                 fold_convert (gfc_array_index_type, tmp2),
                                 fold_convert (gfc_array_index_type, tmp));
	  offset = fold_build2_loc (input_location, PLUS_EXPR,
				    gfc_array_index_type, offset, tmp);

	  VEC_safe_push (tree, gc, stringargs, offset);
	}

      VEC_safe_push (tree, gc, arglist, parmse.expr);
    }
  gfc_finish_interface_mapping (&mapping, &se->pre, &se->post);

  if (comp)
    ts = comp->ts;
  else
   ts = sym->ts;

  if (ts.type == BT_CHARACTER && sym->attr.is_bind_c)
    se->string_length = build_int_cst (gfc_charlen_type_node, 1);
  else if (ts.type == BT_CHARACTER)
    {
      if (ts.u.cl->length == NULL)
	{
	  /* Assumed character length results are not allowed by 5.1.1.5 of the
	     standard and are trapped in resolve.c; except in the case of SPREAD
	     (and other intrinsics?) and dummy functions.  In the case of SPREAD,
	     we take the character length of the first argument for the result.
	     For dummies, we have to look through the formal argument list for
	     this function and use the character length found there.*/
	  if (ts.deferred && (sym->attr.allocatable || sym->attr.pointer))
	    cl.backend_decl = gfc_create_var (gfc_charlen_type_node, "slen");
	  else if (!sym->attr.dummy)
	    cl.backend_decl = VEC_index (tree, stringargs, 0);
	  else
	    {
	      formal = sym->ns->proc_name->formal;
	      for (; formal; formal = formal->next)
		if (strcmp (formal->sym->name, sym->name) == 0)
		  cl.backend_decl = formal->sym->ts.u.cl->backend_decl;
	    }
        }
      else
        {
	  tree tmp;

	  /* Calculate the length of the returned string.  */
	  gfc_init_se (&parmse, NULL);
	  if (need_interface_mapping)
	    gfc_apply_interface_mapping (&mapping, &parmse, ts.u.cl->length);
	  else
	    gfc_conv_expr (&parmse, ts.u.cl->length);
	  gfc_add_block_to_block (&se->pre, &parmse.pre);
	  gfc_add_block_to_block (&se->post, &parmse.post);
	  
	  tmp = fold_convert (gfc_charlen_type_node, parmse.expr);
	  tmp = fold_build2_loc (input_location, MAX_EXPR,
				 gfc_charlen_type_node, tmp,
				 build_int_cst (gfc_charlen_type_node, 0));
	  cl.backend_decl = tmp;
	}

      /* Set up a charlen structure for it.  */
      cl.next = NULL;
      cl.length = NULL;
      ts.u.cl = &cl;

      len = cl.backend_decl;
    }

  byref = (comp && (comp->attr.dimension || comp->ts.type == BT_CHARACTER))
	  || (!comp && gfc_return_by_reference (sym));
  if (byref)
    {
      if (se->direct_byref)
	{
	  /* Sometimes, too much indirection can be applied; e.g. for
	     function_result = array_valued_recursive_function.  */
	  if (TREE_TYPE (TREE_TYPE (se->expr))
		&& TREE_TYPE (TREE_TYPE (TREE_TYPE (se->expr)))
		&& GFC_DESCRIPTOR_TYPE_P
			(TREE_TYPE (TREE_TYPE (TREE_TYPE (se->expr)))))
	    se->expr = build_fold_indirect_ref_loc (input_location,
						se->expr);

	  /* If the lhs of an assignment x = f(..) is allocatable and
	     f2003 is allowed, we must do the automatic reallocation.
	     TODO - deal with intrinsics, without using a temporary.  */
	  if (gfc_option.flag_realloc_lhs
		&& se->ss && se->ss->loop_chain
		&& se->ss->loop_chain->is_alloc_lhs
		&& !expr->value.function.isym
		&& sym->result->as != NULL)
	    {
	      /* Evaluate the bounds of the result, if known.  */
	      gfc_set_loop_bounds_from_array_spec (&mapping, se,
						   sym->result->as);

	      /* Perform the automatic reallocation.  */
	      tmp = gfc_alloc_allocatable_for_assignment (se->loop,
							  expr, NULL);
	      gfc_add_expr_to_block (&se->pre, tmp);

	      /* Pass the temporary as the first argument.  */
	      result = info->descriptor;
	    }
	  else
	    result = build_fold_indirect_ref_loc (input_location,
						  se->expr);
	  VEC_safe_push (tree, gc, retargs, se->expr);
	}
      else if (comp && comp->attr.dimension)
	{
	  gcc_assert (se->loop && info);

	  /* Set the type of the array.  */
	  tmp = gfc_typenode_for_spec (&comp->ts);
	  gcc_assert (se->ss->dimen == se->loop->dimen);

	  /* Evaluate the bounds of the result, if known.  */
	  gfc_set_loop_bounds_from_array_spec (&mapping, se, comp->as);

	  /* If the lhs of an assignment x = f(..) is allocatable and
	     f2003 is allowed, we must not generate the function call
	     here but should just send back the results of the mapping.
	     This is signalled by the function ss being flagged.  */
	  if (gfc_option.flag_realloc_lhs
		&& se->ss && se->ss->is_alloc_lhs)
	    {
	      gfc_free_interface_mapping (&mapping);
	      return has_alternate_specifier;
	    }

	  /* Create a temporary to store the result.  In case the function
	     returns a pointer, the temporary will be a shallow copy and
	     mustn't be deallocated.  */
	  callee_alloc = comp->attr.allocatable || comp->attr.pointer;
	  gfc_trans_create_temp_array (&se->pre, &se->post, se->ss,
				       tmp, NULL_TREE, false,
				       !comp->attr.pointer, callee_alloc,
				       &se->ss->info->expr->where);

	  /* Pass the temporary as the first argument.  */
	  result = info->descriptor;
	  tmp = gfc_build_addr_expr (NULL_TREE, result);
	  VEC_safe_push (tree, gc, retargs, tmp);
	}
      else if (!comp && sym->result->attr.dimension)
	{
	  gcc_assert (se->loop && info);

	  /* Set the type of the array.  */
	  tmp = gfc_typenode_for_spec (&ts);
	  gcc_assert (se->ss->dimen == se->loop->dimen);

	  /* Evaluate the bounds of the result, if known.  */
	  gfc_set_loop_bounds_from_array_spec (&mapping, se, sym->result->as);

	  /* If the lhs of an assignment x = f(..) is allocatable and
	     f2003 is allowed, we must not generate the function call
	     here but should just send back the results of the mapping.
	     This is signalled by the function ss being flagged.  */
	  if (gfc_option.flag_realloc_lhs
		&& se->ss && se->ss->is_alloc_lhs)
	    {
	      gfc_free_interface_mapping (&mapping);
	      return has_alternate_specifier;
	    }

	  /* Create a temporary to store the result.  In case the function
	     returns a pointer, the temporary will be a shallow copy and
	     mustn't be deallocated.  */
	  callee_alloc = sym->attr.allocatable || sym->attr.pointer;
	  gfc_trans_create_temp_array (&se->pre, &se->post, se->ss,
				       tmp, NULL_TREE, false,
				       !sym->attr.pointer, callee_alloc,
				       &se->ss->info->expr->where);

	  /* Pass the temporary as the first argument.  */
	  result = info->descriptor;
	  tmp = gfc_build_addr_expr (NULL_TREE, result);
	  VEC_safe_push (tree, gc, retargs, tmp);
	}
      else if (ts.type == BT_CHARACTER)
	{
	  /* Pass the string length.  */
	  type = gfc_get_character_type (ts.kind, ts.u.cl);
	  type = build_pointer_type (type);

	  /* Return an address to a char[0:len-1]* temporary for
	     character pointers.  */
	  if ((!comp && (sym->attr.pointer || sym->attr.allocatable))
	       || (comp && (comp->attr.pointer || comp->attr.allocatable)))
	    {
	      var = gfc_create_var (type, "pstr");

	      if ((!comp && sym->attr.allocatable)
		  || (comp && comp->attr.allocatable))
		gfc_add_modify (&se->pre, var,
				fold_convert (TREE_TYPE (var),
					      null_pointer_node));

	      /* Provide an address expression for the function arguments.  */
	      var = gfc_build_addr_expr (NULL_TREE, var);
	    }
	  else
	    var = gfc_conv_string_tmp (se, type, len);

	  VEC_safe_push (tree, gc, retargs, var);
	}
      else
	{
	  gcc_assert (gfc_option.flag_f2c && ts.type == BT_COMPLEX);

	  type = gfc_get_complex_type (ts.kind);
	  var = gfc_build_addr_expr (NULL_TREE, gfc_create_var (type, "cmplx"));
	  VEC_safe_push (tree, gc, retargs, var);
	}

      if (ts.type == BT_CHARACTER && ts.deferred
	    && (sym->attr.allocatable || sym->attr.pointer))
	{
	  tmp = len;
	  if (TREE_CODE (tmp) != VAR_DECL)
	    tmp = gfc_evaluate_now (len, &se->pre);
	  len = gfc_build_addr_expr (NULL_TREE, tmp);
	}

      /* Add the string length to the argument list.  */
      if (ts.type == BT_CHARACTER)
	VEC_safe_push (tree, gc, retargs, len);
    }
  gfc_free_interface_mapping (&mapping);

  /* We need to glom RETARGS + ARGLIST + STRINGARGS + APPEND_ARGS.  */
  arglen = (VEC_length (tree, arglist)
	    + VEC_length (tree, stringargs) + VEC_length (tree, append_args));
  VEC_reserve_exact (tree, gc, retargs, arglen);

  /* Add the return arguments.  */
  VEC_splice (tree, retargs, arglist);

  /* Add the hidden string length parameters to the arguments.  */
  VEC_splice (tree, retargs, stringargs);

  /* We may want to append extra arguments here.  This is used e.g. for
     calls to libgfortran_matmul_??, which need extra information.  */
  if (!VEC_empty (tree, append_args))
    VEC_splice (tree, retargs, append_args);
  arglist = retargs;

  /* Generate the actual call.  */
  if (base_object == NULL_TREE)
    conv_function_val (se, sym, expr);
  else
    conv_base_obj_fcn_val (se, base_object, expr);

  /* If there are alternate return labels, function type should be
     integer.  Can't modify the type in place though, since it can be shared
     with other functions.  For dummy arguments, the typing is done to
     this result, even if it has to be repeated for each call.  */
  if (has_alternate_specifier
      && TREE_TYPE (TREE_TYPE (TREE_TYPE (se->expr))) != integer_type_node)
    {
      if (!sym->attr.dummy)
	{
	  TREE_TYPE (sym->backend_decl)
		= build_function_type (integer_type_node,
		      TYPE_ARG_TYPES (TREE_TYPE (sym->backend_decl)));
	  se->expr = gfc_build_addr_expr (NULL_TREE, sym->backend_decl);
	}
      else
	TREE_TYPE (TREE_TYPE (TREE_TYPE (se->expr))) = integer_type_node;
    }

  fntype = TREE_TYPE (TREE_TYPE (se->expr));
  se->expr = build_call_vec (TREE_TYPE (fntype), se->expr, arglist);

  /* If we have a pointer function, but we don't want a pointer, e.g.
     something like
        x = f()
     where f is pointer valued, we have to dereference the result.  */
  if (!se->want_pointer && !byref
      && ((!comp && (sym->attr.pointer || sym->attr.allocatable))
	  || (comp && (comp->attr.pointer || comp->attr.allocatable))))
    se->expr = build_fold_indirect_ref_loc (input_location, se->expr);

  /* f2c calling conventions require a scalar default real function to
     return a double precision result.  Convert this back to default
     real.  We only care about the cases that can happen in Fortran 77.
  */
  if (gfc_option.flag_f2c && sym->ts.type == BT_REAL
      && sym->ts.kind == gfc_default_real_kind
      && !sym->attr.always_explicit)
    se->expr = fold_convert (gfc_get_real_type (sym->ts.kind), se->expr);

  /* A pure function may still have side-effects - it may modify its
     parameters.  */
  TREE_SIDE_EFFECTS (se->expr) = 1;
#if 0
  if (!sym->attr.pure)
    TREE_SIDE_EFFECTS (se->expr) = 1;
#endif

  if (byref)
    {
      /* Add the function call to the pre chain.  There is no expression.  */
      gfc_add_expr_to_block (&se->pre, se->expr);
      se->expr = NULL_TREE;

      if (!se->direct_byref)
	{
	  if ((sym->attr.dimension && !comp) || (comp && comp->attr.dimension))
	    {
	      if (gfc_option.rtcheck & GFC_RTCHECK_BOUNDS)
		{
		  /* Check the data pointer hasn't been modified.  This would
		     happen in a function returning a pointer.  */
		  tmp = gfc_conv_descriptor_data_get (info->descriptor);
		  tmp = fold_build2_loc (input_location, NE_EXPR,
					 boolean_type_node,
					 tmp, info->data);
		  gfc_trans_runtime_check (true, false, tmp, &se->pre, NULL,
					   gfc_msg_fault);
		}
	      se->expr = info->descriptor;
	      /* Bundle in the string length.  */
	      se->string_length = len;
	    }
	  else if (ts.type == BT_CHARACTER)
	    {
	      /* Dereference for character pointer results.  */
	      if ((!comp && (sym->attr.pointer || sym->attr.allocatable))
		  || (comp && (comp->attr.pointer || comp->attr.allocatable)))
		se->expr = build_fold_indirect_ref_loc (input_location, var);
	      else
	        se->expr = var;

	      if (!ts.deferred)
		se->string_length = len;
	      else if (sym->attr.allocatable || sym->attr.pointer)
		se->string_length = cl.backend_decl;
	    }
	  else
	    {
	      gcc_assert (ts.type == BT_COMPLEX && gfc_option.flag_f2c);
	      se->expr = build_fold_indirect_ref_loc (input_location, var);
	    }
	}
    }

  /* Follow the function call with the argument post block.  */
  if (byref)
    {
      gfc_add_block_to_block (&se->pre, &post);

      /* Transformational functions of derived types with allocatable
         components must have the result allocatable components copied.  */
      arg = expr->value.function.actual;
      if (result && arg && expr->rank
	    && expr->value.function.isym
	    && expr->value.function.isym->transformational
	    && arg->expr->ts.type == BT_DERIVED
	    && arg->expr->ts.u.derived->attr.alloc_comp)
	{
	  tree tmp2;
	  /* Copy the allocatable components.  We have to use a
	     temporary here to prevent source allocatable components
	     from being corrupted.  */
	  tmp2 = gfc_evaluate_now (result, &se->pre);
	  tmp = gfc_copy_alloc_comp (arg->expr->ts.u.derived,
				     result, tmp2, expr->rank);
	  gfc_add_expr_to_block (&se->pre, tmp);
	  tmp = gfc_copy_allocatable_data (result, tmp2, TREE_TYPE(tmp2),
				           expr->rank);
	  gfc_add_expr_to_block (&se->pre, tmp);

	  /* Finally free the temporary's data field.  */
	  tmp = gfc_conv_descriptor_data_get (tmp2);
	  tmp = gfc_deallocate_with_status (tmp, NULL_TREE, true, NULL);
	  gfc_add_expr_to_block (&se->pre, tmp);
	}
    }
  else
    gfc_add_block_to_block (&se->post, &post);

  return has_alternate_specifier;
}


/* Fill a character string with spaces.  */

static tree
fill_with_spaces (tree start, tree type, tree size)
{
  stmtblock_t block, loop;
  tree i, el, exit_label, cond, tmp;

  /* For a simple char type, we can call memset().  */
  if (compare_tree_int (TYPE_SIZE_UNIT (type), 1) == 0)
    return build_call_expr_loc (input_location,
			    builtin_decl_explicit (BUILT_IN_MEMSET),
			    3, start,
			    build_int_cst (gfc_get_int_type (gfc_c_int_kind),
					   lang_hooks.to_target_charset (' ')),
			    size);

  /* Otherwise, we use a loop:
	for (el = start, i = size; i > 0; el--, i+= TYPE_SIZE_UNIT (type))
	  *el = (type) ' ';
   */

  /* Initialize variables.  */
  gfc_init_block (&block);
  i = gfc_create_var (sizetype, "i");
  gfc_add_modify (&block, i, fold_convert (sizetype, size));
  el = gfc_create_var (build_pointer_type (type), "el");
  gfc_add_modify (&block, el, fold_convert (TREE_TYPE (el), start));
  exit_label = gfc_build_label_decl (NULL_TREE);
  TREE_USED (exit_label) = 1;


  /* Loop body.  */
  gfc_init_block (&loop);

  /* Exit condition.  */
  cond = fold_build2_loc (input_location, LE_EXPR, boolean_type_node, i,
			  build_zero_cst (sizetype));
  tmp = build1_v (GOTO_EXPR, exit_label);
  tmp = fold_build3_loc (input_location, COND_EXPR, void_type_node, cond, tmp,
			 build_empty_stmt (input_location));
  gfc_add_expr_to_block (&loop, tmp);

  /* Assignment.  */
  gfc_add_modify (&loop,
		  fold_build1_loc (input_location, INDIRECT_REF, type, el),
		  build_int_cst (type, lang_hooks.to_target_charset (' ')));

  /* Increment loop variables.  */
  gfc_add_modify (&loop, i,
		  fold_build2_loc (input_location, MINUS_EXPR, sizetype, i,
				   TYPE_SIZE_UNIT (type)));
  gfc_add_modify (&loop, el,
		  fold_build_pointer_plus_loc (input_location,
					       el, TYPE_SIZE_UNIT (type)));

  /* Making the loop... actually loop!  */
  tmp = gfc_finish_block (&loop);
  tmp = build1_v (LOOP_EXPR, tmp);
  gfc_add_expr_to_block (&block, tmp);

  /* The exit label.  */
  tmp = build1_v (LABEL_EXPR, exit_label);
  gfc_add_expr_to_block (&block, tmp);


  return gfc_finish_block (&block);
}


/* Generate code to copy a string.  */

void
gfc_trans_string_copy (stmtblock_t * block, tree dlength, tree dest,
		       int dkind, tree slength, tree src, int skind)
{
  tree tmp, dlen, slen;
  tree dsc;
  tree ssc;
  tree cond;
  tree cond2;
  tree tmp2;
  tree tmp3;
  tree tmp4;
  tree chartype;
  stmtblock_t tempblock;

  gcc_assert (dkind == skind);

  if (slength != NULL_TREE)
    {
      slen = fold_convert (size_type_node, gfc_evaluate_now (slength, block));
      ssc = gfc_string_to_single_character (slen, src, skind);
    }
  else
    {
      slen = build_int_cst (size_type_node, 1);
      ssc =  src;
    }

  if (dlength != NULL_TREE)
    {
      dlen = fold_convert (size_type_node, gfc_evaluate_now (dlength, block));
      dsc = gfc_string_to_single_character (dlen, dest, dkind);
    }
  else
    {
      dlen = build_int_cst (size_type_node, 1);
      dsc =  dest;
    }

  /* Assign directly if the types are compatible.  */
  if (dsc != NULL_TREE && ssc != NULL_TREE
      && TREE_TYPE (dsc) == TREE_TYPE (ssc))
    {
      gfc_add_modify (block, dsc, ssc);
      return;
    }

  /* Do nothing if the destination length is zero.  */
  cond = fold_build2_loc (input_location, GT_EXPR, boolean_type_node, dlen,
			  build_int_cst (size_type_node, 0));

  /* The following code was previously in _gfortran_copy_string:

       // The two strings may overlap so we use memmove.
       void
       copy_string (GFC_INTEGER_4 destlen, char * dest,
                    GFC_INTEGER_4 srclen, const char * src)
       {
         if (srclen >= destlen)
           {
             // This will truncate if too long.
             memmove (dest, src, destlen);
           }
         else
           {
             memmove (dest, src, srclen);
             // Pad with spaces.
             memset (&dest[srclen], ' ', destlen - srclen);
           }
       }

     We're now doing it here for better optimization, but the logic
     is the same.  */

  /* For non-default character kinds, we have to multiply the string
     length by the base type size.  */
  chartype = gfc_get_char_type (dkind);
  slen = fold_build2_loc (input_location, MULT_EXPR, size_type_node,
			  fold_convert (size_type_node, slen),
			  fold_convert (size_type_node,
					TYPE_SIZE_UNIT (chartype)));
  dlen = fold_build2_loc (input_location, MULT_EXPR, size_type_node,
			  fold_convert (size_type_node, dlen),
			  fold_convert (size_type_node,
					TYPE_SIZE_UNIT (chartype)));

  if (dlength && POINTER_TYPE_P (TREE_TYPE (dest)))
    dest = fold_convert (pvoid_type_node, dest);
  else
    dest = gfc_build_addr_expr (pvoid_type_node, dest);

  if (slength && POINTER_TYPE_P (TREE_TYPE (src)))
    src = fold_convert (pvoid_type_node, src);
  else
    src = gfc_build_addr_expr (pvoid_type_node, src);

  /* Truncate string if source is too long.  */
  cond2 = fold_build2_loc (input_location, GE_EXPR, boolean_type_node, slen,
			   dlen);
  tmp2 = build_call_expr_loc (input_location,
			      builtin_decl_explicit (BUILT_IN_MEMMOVE),
			      3, dest, src, dlen);

  /* Else copy and pad with spaces.  */
  tmp3 = build_call_expr_loc (input_location,
			      builtin_decl_explicit (BUILT_IN_MEMMOVE),
			      3, dest, src, slen);

  tmp4 = fold_build_pointer_plus_loc (input_location, dest, slen);
  tmp4 = fill_with_spaces (tmp4, chartype,
			   fold_build2_loc (input_location, MINUS_EXPR,
					    TREE_TYPE(dlen), dlen, slen));

  gfc_init_block (&tempblock);
  gfc_add_expr_to_block (&tempblock, tmp3);
  gfc_add_expr_to_block (&tempblock, tmp4);
  tmp3 = gfc_finish_block (&tempblock);

  /* The whole copy_string function is there.  */
  tmp = fold_build3_loc (input_location, COND_EXPR, void_type_node, cond2,
			 tmp2, tmp3);
  tmp = fold_build3_loc (input_location, COND_EXPR, void_type_node, cond, tmp,
			 build_empty_stmt (input_location));
  gfc_add_expr_to_block (block, tmp);
}


/* Translate a statement function.
   The value of a statement function reference is obtained by evaluating the
   expression using the values of the actual arguments for the values of the
   corresponding dummy arguments.  */

static void
gfc_conv_statement_function (gfc_se * se, gfc_expr * expr)
{
  gfc_symbol *sym;
  gfc_symbol *fsym;
  gfc_formal_arglist *fargs;
  gfc_actual_arglist *args;
  gfc_se lse;
  gfc_se rse;
  gfc_saved_var *saved_vars;
  tree *temp_vars;
  tree type;
  tree tmp;
  int n;

  sym = expr->symtree->n.sym;
  args = expr->value.function.actual;
  gfc_init_se (&lse, NULL);
  gfc_init_se (&rse, NULL);

  n = 0;
  for (fargs = sym->formal; fargs; fargs = fargs->next)
    n++;
  saved_vars = XCNEWVEC (gfc_saved_var, n);
  temp_vars = XCNEWVEC (tree, n);

  for (fargs = sym->formal, n = 0; fargs; fargs = fargs->next, n++)
    {
      /* Each dummy shall be specified, explicitly or implicitly, to be
         scalar.  */
      gcc_assert (fargs->sym->attr.dimension == 0);
      fsym = fargs->sym;

      if (fsym->ts.type == BT_CHARACTER)
        {
	  /* Copy string arguments.  */
	  tree arglen;

	  gcc_assert (fsym->ts.u.cl && fsym->ts.u.cl->length
		      && fsym->ts.u.cl->length->expr_type == EXPR_CONSTANT);

	  /* Create a temporary to hold the value.  */
          if (fsym->ts.u.cl->backend_decl == NULL_TREE)
	     fsym->ts.u.cl->backend_decl
		= gfc_conv_constant_to_tree (fsym->ts.u.cl->length);

	  type = gfc_get_character_type (fsym->ts.kind, fsym->ts.u.cl);
	  temp_vars[n] = gfc_create_var (type, fsym->name);

	  arglen = TYPE_MAX_VALUE (TYPE_DOMAIN (type));

	  gfc_conv_expr (&rse, args->expr);
	  gfc_conv_string_parameter (&rse);
	  gfc_add_block_to_block (&se->pre, &lse.pre);
	  gfc_add_block_to_block (&se->pre, &rse.pre);

	  gfc_trans_string_copy (&se->pre, arglen, temp_vars[n], fsym->ts.kind,
				 rse.string_length, rse.expr, fsym->ts.kind);
	  gfc_add_block_to_block (&se->pre, &lse.post);
	  gfc_add_block_to_block (&se->pre, &rse.post);
        }
      else
        {
          /* For everything else, just evaluate the expression.  */

	  /* Create a temporary to hold the value.  */
	  type = gfc_typenode_for_spec (&fsym->ts);
	  temp_vars[n] = gfc_create_var (type, fsym->name);

          gfc_conv_expr (&lse, args->expr);

          gfc_add_block_to_block (&se->pre, &lse.pre);
          gfc_add_modify (&se->pre, temp_vars[n], lse.expr);
          gfc_add_block_to_block (&se->pre, &lse.post);
        }

      args = args->next;
    }

  /* Use the temporary variables in place of the real ones.  */
  for (fargs = sym->formal, n = 0; fargs; fargs = fargs->next, n++)
    gfc_shadow_sym (fargs->sym, temp_vars[n], &saved_vars[n]);

  gfc_conv_expr (se, sym->value);

  if (sym->ts.type == BT_CHARACTER)
    {
      gfc_conv_const_charlen (sym->ts.u.cl);

      /* Force the expression to the correct length.  */
      if (!INTEGER_CST_P (se->string_length)
	  || tree_int_cst_lt (se->string_length,
			      sym->ts.u.cl->backend_decl))
	{
	  type = gfc_get_character_type (sym->ts.kind, sym->ts.u.cl);
	  tmp = gfc_create_var (type, sym->name);
	  tmp = gfc_build_addr_expr (build_pointer_type (type), tmp);
	  gfc_trans_string_copy (&se->pre, sym->ts.u.cl->backend_decl, tmp,
				 sym->ts.kind, se->string_length, se->expr,
				 sym->ts.kind);
	  se->expr = tmp;
	}
      se->string_length = sym->ts.u.cl->backend_decl;
    }

  /* Restore the original variables.  */
  for (fargs = sym->formal, n = 0; fargs; fargs = fargs->next, n++)
    gfc_restore_sym (fargs->sym, &saved_vars[n]);
  free (saved_vars);
}


/* Translate a function expression.  */

static void
gfc_conv_function_expr (gfc_se * se, gfc_expr * expr)
{
  gfc_symbol *sym;

  if (expr->value.function.isym)
    {
      gfc_conv_intrinsic_function (se, expr);
      return;
    }

  /* We distinguish statement functions from general functions to improve
     runtime performance.  */
  if (expr->symtree->n.sym->attr.proc == PROC_ST_FUNCTION)
    {
      gfc_conv_statement_function (se, expr);
      return;
    }

  /* expr.value.function.esym is the resolved (specific) function symbol for
     most functions.  However this isn't set for dummy procedures.  */
  sym = expr->value.function.esym;
  if (!sym)
    sym = expr->symtree->n.sym;

  gfc_conv_procedure_call (se, sym, expr->value.function.actual, expr, NULL);
}


/* Determine whether the given EXPR_CONSTANT is a zero initializer.  */

static bool
is_zero_initializer_p (gfc_expr * expr)
{
  if (expr->expr_type != EXPR_CONSTANT)
    return false;

  /* We ignore constants with prescribed memory representations for now.  */
  if (expr->representation.string)
    return false;

  switch (expr->ts.type)
    {
    case BT_INTEGER:
      return mpz_cmp_si (expr->value.integer, 0) == 0;

    case BT_REAL:
      return mpfr_zero_p (expr->value.real)
	     && MPFR_SIGN (expr->value.real) >= 0;

    case BT_LOGICAL:
      return expr->value.logical == 0;

    case BT_COMPLEX:
      return mpfr_zero_p (mpc_realref (expr->value.complex))
	     && MPFR_SIGN (mpc_realref (expr->value.complex)) >= 0
             && mpfr_zero_p (mpc_imagref (expr->value.complex))
	     && MPFR_SIGN (mpc_imagref (expr->value.complex)) >= 0;

    default:
      break;
    }
  return false;
}


static void
gfc_conv_array_constructor_expr (gfc_se * se, gfc_expr * expr)
{
  gfc_ss *ss;

  ss = se->ss;
  gcc_assert (ss != NULL && ss != gfc_ss_terminator);
  gcc_assert (ss->info->expr == expr && ss->info->type == GFC_SS_CONSTRUCTOR);

  gfc_conv_tmp_array_ref (se);
}


/* Build a static initializer.  EXPR is the expression for the initial value.
   The other parameters describe the variable of the component being 
   initialized. EXPR may be null.  */

tree
gfc_conv_initializer (gfc_expr * expr, gfc_typespec * ts, tree type,
		      bool array, bool pointer, bool procptr)
{
  gfc_se se;

  if (!(expr || pointer || procptr))
    return NULL_TREE;

  /* Check if we have ISOCBINDING_NULL_PTR or ISOCBINDING_NULL_FUNPTR
     (these are the only two iso_c_binding derived types that can be
     used as initialization expressions).  If so, we need to modify
     the 'expr' to be that for a (void *).  */
  if (expr != NULL && expr->ts.type == BT_DERIVED
      && expr->ts.is_iso_c && expr->ts.u.derived)
    {
      gfc_symbol *derived = expr->ts.u.derived;

      /* The derived symbol has already been converted to a (void *).  Use
	 its kind.  */
      expr = gfc_get_int_expr (derived->ts.kind, NULL, 0);
      expr->ts.f90_type = derived->ts.f90_type;

      gfc_init_se (&se, NULL);
      gfc_conv_constant (&se, expr);
      gcc_assert (TREE_CODE (se.expr) != CONSTRUCTOR);
      return se.expr;
    }
  
  if (array && !procptr)
    {
      tree ctor;
      /* Arrays need special handling.  */
      if (pointer)
	ctor = gfc_build_null_descriptor (type);
      /* Special case assigning an array to zero.  */
      else if (is_zero_initializer_p (expr))
        ctor = build_constructor (type, NULL);
      else
	ctor = gfc_conv_array_initializer (type, expr);
      TREE_STATIC (ctor) = 1;
      return ctor;
    }
  else if (pointer || procptr)
    {
      if (!expr || expr->expr_type == EXPR_NULL)
	return fold_convert (type, null_pointer_node);
      else
	{
	  gfc_init_se (&se, NULL);
	  se.want_pointer = 1;
	  gfc_conv_expr (&se, expr);
          gcc_assert (TREE_CODE (se.expr) != CONSTRUCTOR);
	  return se.expr;
	}
    }
  else
    {
      switch (ts->type)
	{
	case BT_DERIVED:
	case BT_CLASS:
	  gfc_init_se (&se, NULL);
	  if (ts->type == BT_CLASS && expr->expr_type == EXPR_NULL)
	    gfc_conv_structure (&se, gfc_class_null_initializer(ts), 1);
	  else
	    gfc_conv_structure (&se, expr, 1);
	  gcc_assert (TREE_CODE (se.expr) == CONSTRUCTOR);
	  TREE_STATIC (se.expr) = 1;
	  return se.expr;

	case BT_CHARACTER:
	  {
	    tree ctor = gfc_conv_string_init (ts->u.cl->backend_decl,expr);
	    TREE_STATIC (ctor) = 1;
	    return ctor;
	  }

	default:
	  gfc_init_se (&se, NULL);
	  gfc_conv_constant (&se, expr);
	  gcc_assert (TREE_CODE (se.expr) != CONSTRUCTOR);
	  return se.expr;
	}
    }
}
  
static tree
gfc_trans_subarray_assign (tree dest, gfc_component * cm, gfc_expr * expr)
{
  gfc_se rse;
  gfc_se lse;
  gfc_ss *rss;
  gfc_ss *lss;
  gfc_array_info *lss_array;
  stmtblock_t body;
  stmtblock_t block;
  gfc_loopinfo loop;
  int n;
  tree tmp;

  gfc_start_block (&block);

  /* Initialize the scalarizer.  */
  gfc_init_loopinfo (&loop);

  gfc_init_se (&lse, NULL);
  gfc_init_se (&rse, NULL);

  /* Walk the rhs.  */
  rss = gfc_walk_expr (expr);
  if (rss == gfc_ss_terminator)
    /* The rhs is scalar.  Add a ss for the expression.  */
    rss = gfc_get_scalar_ss (gfc_ss_terminator, expr);

  /* Create a SS for the destination.  */
  lss = gfc_get_array_ss (gfc_ss_terminator, NULL, cm->as->rank,
			  GFC_SS_COMPONENT);
  lss_array = &lss->info->data.array;
  lss_array->shape = gfc_get_shape (cm->as->rank);
  lss_array->descriptor = dest;
  lss_array->data = gfc_conv_array_data (dest);
  lss_array->offset = gfc_conv_array_offset (dest);
  for (n = 0; n < cm->as->rank; n++)
    {
      lss_array->start[n] = gfc_conv_array_lbound (dest, n);
      lss_array->stride[n] = gfc_index_one_node;

      mpz_init (lss_array->shape[n]);
      mpz_sub (lss_array->shape[n], cm->as->upper[n]->value.integer,
	       cm->as->lower[n]->value.integer);
      mpz_add_ui (lss_array->shape[n], lss_array->shape[n], 1);
    }
  
  /* Associate the SS with the loop.  */
  gfc_add_ss_to_loop (&loop, lss);
  gfc_add_ss_to_loop (&loop, rss);

  /* Calculate the bounds of the scalarization.  */
  gfc_conv_ss_startstride (&loop);

  /* Setup the scalarizing loops.  */
  gfc_conv_loop_setup (&loop, &expr->where);

  /* Setup the gfc_se structures.  */
  gfc_copy_loopinfo_to_se (&lse, &loop);
  gfc_copy_loopinfo_to_se (&rse, &loop);

  rse.ss = rss;
  gfc_mark_ss_chain_used (rss, 1);
  lse.ss = lss;
  gfc_mark_ss_chain_used (lss, 1);

  /* Start the scalarized loop body.  */
  gfc_start_scalarized_body (&loop, &body);

  gfc_conv_tmp_array_ref (&lse);
  if (cm->ts.type == BT_CHARACTER)
    lse.string_length = cm->ts.u.cl->backend_decl;

  gfc_conv_expr (&rse, expr);

  tmp = gfc_trans_scalar_assign (&lse, &rse, cm->ts, true, false, true);
  gfc_add_expr_to_block (&body, tmp);

  gcc_assert (rse.ss == gfc_ss_terminator);

  /* Generate the copying loops.  */
  gfc_trans_scalarizing_loops (&loop, &body);

  /* Wrap the whole thing up.  */
  gfc_add_block_to_block (&block, &loop.pre);
  gfc_add_block_to_block (&block, &loop.post);

  gcc_assert (lss_array->shape != NULL);
  gfc_free_shape (&lss_array->shape, cm->as->rank);
  gfc_cleanup_loop (&loop);

  return gfc_finish_block (&block);
}


static tree
gfc_trans_alloc_subarray_assign (tree dest, gfc_component * cm,
				 gfc_expr * expr)
{
  gfc_se se;
  gfc_ss *rss;
  stmtblock_t block;
  tree offset;
  int n;
  tree tmp;
  tree tmp2;
  gfc_array_spec *as;
  gfc_expr *arg = NULL;

  gfc_start_block (&block);
  gfc_init_se (&se, NULL);

  /* Get the descriptor for the expressions.  */ 
  rss = gfc_walk_expr (expr);
  se.want_pointer = 0;
  gfc_conv_expr_descriptor (&se, expr, rss);
  gfc_add_block_to_block (&block, &se.pre);
  gfc_add_modify (&block, dest, se.expr);

  /* Deal with arrays of derived types with allocatable components.  */
  if (cm->ts.type == BT_DERIVED
	&& cm->ts.u.derived->attr.alloc_comp)
    tmp = gfc_copy_alloc_comp (cm->ts.u.derived,
			       se.expr, dest,
			       cm->as->rank);
  else
    tmp = gfc_duplicate_allocatable (dest, se.expr,
				     TREE_TYPE(cm->backend_decl),
				     cm->as->rank);

  gfc_add_expr_to_block (&block, tmp);
  gfc_add_block_to_block (&block, &se.post);

  if (expr->expr_type != EXPR_VARIABLE)
    gfc_conv_descriptor_data_set (&block, se.expr,
				  null_pointer_node);

  /* We need to know if the argument of a conversion function is a
     variable, so that the correct lower bound can be used.  */
  if (expr->expr_type == EXPR_FUNCTION
	&& expr->value.function.isym
	&& expr->value.function.isym->conversion
	&& expr->value.function.actual->expr
	&& expr->value.function.actual->expr->expr_type == EXPR_VARIABLE)
    arg = expr->value.function.actual->expr;

  /* Obtain the array spec of full array references.  */
  if (arg)
    as = gfc_get_full_arrayspec_from_expr (arg);
  else
    as = gfc_get_full_arrayspec_from_expr (expr);

  /* Shift the lbound and ubound of temporaries to being unity,
     rather than zero, based. Always calculate the offset.  */
  offset = gfc_conv_descriptor_offset_get (dest);
  gfc_add_modify (&block, offset, gfc_index_zero_node);
  tmp2 =gfc_create_var (gfc_array_index_type, NULL);

  for (n = 0; n < expr->rank; n++)
    {
      tree span;
      tree lbound;

      /* Obtain the correct lbound - ISO/IEC TR 15581:2001 page 9.
	 TODO It looks as if gfc_conv_expr_descriptor should return
	 the correct bounds and that the following should not be
	 necessary.  This would simplify gfc_conv_intrinsic_bound
	 as well.  */
      if (as && as->lower[n])
	{
	  gfc_se lbse;
	  gfc_init_se (&lbse, NULL);
	  gfc_conv_expr (&lbse, as->lower[n]);
	  gfc_add_block_to_block (&block, &lbse.pre);
	  lbound = gfc_evaluate_now (lbse.expr, &block);
	}
      else if (as && arg)
	{
	  tmp = gfc_get_symbol_decl (arg->symtree->n.sym);
	  lbound = gfc_conv_descriptor_lbound_get (tmp,
					gfc_rank_cst[n]);
	}
      else if (as)
	lbound = gfc_conv_descriptor_lbound_get (dest,
						gfc_rank_cst[n]);
      else
	lbound = gfc_index_one_node;

      lbound = fold_convert (gfc_array_index_type, lbound);

      /* Shift the bounds and set the offset accordingly.  */
      tmp = gfc_conv_descriptor_ubound_get (dest, gfc_rank_cst[n]);
      span = fold_build2_loc (input_location, MINUS_EXPR, gfc_array_index_type,
		tmp, gfc_conv_descriptor_lbound_get (dest, gfc_rank_cst[n]));
      tmp = fold_build2_loc (input_location, PLUS_EXPR, gfc_array_index_type,
			     span, lbound);
      gfc_conv_descriptor_ubound_set (&block, dest,
				      gfc_rank_cst[n], tmp);
      gfc_conv_descriptor_lbound_set (&block, dest,
				      gfc_rank_cst[n], lbound);

      tmp = fold_build2_loc (input_location, MULT_EXPR, gfc_array_index_type,
			 gfc_conv_descriptor_lbound_get (dest,
							 gfc_rank_cst[n]),
			 gfc_conv_descriptor_stride_get (dest,
							 gfc_rank_cst[n]));
      gfc_add_modify (&block, tmp2, tmp);
      tmp = fold_build2_loc (input_location, MINUS_EXPR, gfc_array_index_type,
			     offset, tmp2);
      gfc_conv_descriptor_offset_set (&block, dest, tmp);
    }

  if (arg)
    {
      /* If a conversion expression has a null data pointer
	 argument, nullify the allocatable component.  */
      tree non_null_expr;
      tree null_expr;

      if (arg->symtree->n.sym->attr.allocatable
	    || arg->symtree->n.sym->attr.pointer)
	{
	  non_null_expr = gfc_finish_block (&block);
	  gfc_start_block (&block);
	  gfc_conv_descriptor_data_set (&block, dest,
					null_pointer_node);
	  null_expr = gfc_finish_block (&block);
	  tmp = gfc_conv_descriptor_data_get (arg->symtree->n.sym->backend_decl);
	  tmp = build2_loc (input_location, EQ_EXPR, boolean_type_node, tmp,
			    fold_convert (TREE_TYPE (tmp), null_pointer_node));
	  return build3_v (COND_EXPR, tmp,
			   null_expr, non_null_expr);
	}
    }

  return gfc_finish_block (&block);
}


/* Assign a single component of a derived type constructor.  */

static tree
gfc_trans_subcomponent_assign (tree dest, gfc_component * cm, gfc_expr * expr)
{
  gfc_se se;
  gfc_se lse;
  gfc_ss *rss;
  stmtblock_t block;
  tree tmp;

  gfc_start_block (&block);

  if (cm->attr.pointer)
    {
      gfc_init_se (&se, NULL);
      /* Pointer component.  */
      if (cm->attr.dimension)
	{
	  /* Array pointer.  */
	  if (expr->expr_type == EXPR_NULL)
	    gfc_conv_descriptor_data_set (&block, dest, null_pointer_node);
	  else
	    {
	      rss = gfc_walk_expr (expr);
	      se.direct_byref = 1;
	      se.expr = dest;
	      gfc_conv_expr_descriptor (&se, expr, rss);
	      gfc_add_block_to_block (&block, &se.pre);
	      gfc_add_block_to_block (&block, &se.post);
	    }
	}
      else
	{
	  /* Scalar pointers.  */
	  se.want_pointer = 1;
	  gfc_conv_expr (&se, expr);
	  gfc_add_block_to_block (&block, &se.pre);
	  gfc_add_modify (&block, dest,
			       fold_convert (TREE_TYPE (dest), se.expr));
	  gfc_add_block_to_block (&block, &se.post);
	}
    }
  else if (cm->ts.type == BT_CLASS && expr->expr_type == EXPR_NULL)
    {
      /* NULL initialization for CLASS components.  */
      tmp = gfc_trans_structure_assign (dest,
					gfc_class_null_initializer (&cm->ts));
      gfc_add_expr_to_block (&block, tmp);
    }
  else if (cm->attr.dimension && !cm->attr.proc_pointer)
    {
      if (cm->attr.allocatable && expr->expr_type == EXPR_NULL)
 	gfc_conv_descriptor_data_set (&block, dest, null_pointer_node);
      else if (cm->attr.allocatable)
	{
	  tmp = gfc_trans_alloc_subarray_assign (dest, cm, expr);
	  gfc_add_expr_to_block (&block, tmp);
	}
      else
	{
	  tmp = gfc_trans_subarray_assign (dest, cm, expr);
	  gfc_add_expr_to_block (&block, tmp);
	}
    }
  else if (expr->ts.type == BT_DERIVED)
    {
      if (expr->expr_type != EXPR_STRUCTURE)
	{
	  gfc_init_se (&se, NULL);
	  gfc_conv_expr (&se, expr);
	  gfc_add_block_to_block (&block, &se.pre);
	  gfc_add_modify (&block, dest,
			       fold_convert (TREE_TYPE (dest), se.expr));
	  gfc_add_block_to_block (&block, &se.post);
	}
      else
	{
	  /* Nested constructors.  */
	  tmp = gfc_trans_structure_assign (dest, expr);
	  gfc_add_expr_to_block (&block, tmp);
	}
    }
  else
    {
      /* Scalar component.  */
      gfc_init_se (&se, NULL);
      gfc_init_se (&lse, NULL);

      gfc_conv_expr (&se, expr);
      if (cm->ts.type == BT_CHARACTER)
	lse.string_length = cm->ts.u.cl->backend_decl;
      lse.expr = dest;
      tmp = gfc_trans_scalar_assign (&lse, &se, cm->ts, true, false, true);
      gfc_add_expr_to_block (&block, tmp);
    }
  return gfc_finish_block (&block);
}

/* Assign a derived type constructor to a variable.  */

static tree
gfc_trans_structure_assign (tree dest, gfc_expr * expr)
{
  gfc_constructor *c;
  gfc_component *cm;
  stmtblock_t block;
  tree field;
  tree tmp;

  gfc_start_block (&block);
  cm = expr->ts.u.derived->components;

  if (expr->ts.u.derived->from_intmod == INTMOD_ISO_C_BINDING
      && (expr->ts.u.derived->intmod_sym_id == ISOCBINDING_PTR
          || expr->ts.u.derived->intmod_sym_id == ISOCBINDING_FUNPTR))
    {
      gfc_se se, lse;

      gcc_assert (cm->backend_decl == NULL);
      gfc_init_se (&se, NULL);
      gfc_init_se (&lse, NULL);
      gfc_conv_expr (&se, gfc_constructor_first (expr->value.constructor)->expr);
      lse.expr = dest;
      gfc_add_modify (&block, lse.expr,
		      fold_convert (TREE_TYPE (lse.expr), se.expr));

      return gfc_finish_block (&block);
    } 

  for (c = gfc_constructor_first (expr->value.constructor);
       c; c = gfc_constructor_next (c), cm = cm->next)
    {
      /* Skip absent members in default initializers.  */
      if (!c->expr)
	continue;

      field = cm->backend_decl;
      tmp = fold_build3_loc (input_location, COMPONENT_REF, TREE_TYPE (field),
			     dest, field, NULL_TREE);
      tmp = gfc_trans_subcomponent_assign (tmp, cm, c->expr);
      gfc_add_expr_to_block (&block, tmp);
    }
  return gfc_finish_block (&block);
}

/* Build an expression for a constructor. If init is nonzero then
   this is part of a static variable initializer.  */

void
gfc_conv_structure (gfc_se * se, gfc_expr * expr, int init)
{
  gfc_constructor *c;
  gfc_component *cm;
  tree val;
  tree type;
  tree tmp;
  VEC(constructor_elt,gc) *v = NULL;

  gcc_assert (se->ss == NULL);
  gcc_assert (expr->expr_type == EXPR_STRUCTURE);
  type = gfc_typenode_for_spec (&expr->ts);

  if (!init)
    {
      /* Create a temporary variable and fill it in.  */
      se->expr = gfc_create_var (type, expr->ts.u.derived->name);
      tmp = gfc_trans_structure_assign (se->expr, expr);
      gfc_add_expr_to_block (&se->pre, tmp);
      return;
    }

  cm = expr->ts.u.derived->components;

  for (c = gfc_constructor_first (expr->value.constructor);
       c; c = gfc_constructor_next (c), cm = cm->next)
    {
      /* Skip absent members in default initializers and allocatable
	 components.  Although the latter have a default initializer
	 of EXPR_NULL,... by default, the static nullify is not needed
	 since this is done every time we come into scope.  */
      if (!c->expr || (cm->attr.allocatable && cm->attr.flavor != FL_PROCEDURE))
        continue;

      if (strcmp (cm->name, "_size") == 0)
	{
	  val = TYPE_SIZE_UNIT (gfc_get_derived_type (cm->ts.u.derived));
	  CONSTRUCTOR_APPEND_ELT (v, cm->backend_decl, val);
	}
      else if (cm->initializer && cm->initializer->expr_type != EXPR_NULL
	       && strcmp (cm->name, "_extends") == 0)
	{
	  tree vtab;
	  gfc_symbol *vtabs;
	  vtabs = cm->initializer->symtree->n.sym;
	  vtab = gfc_build_addr_expr (NULL_TREE, gfc_get_symbol_decl (vtabs));
	  CONSTRUCTOR_APPEND_ELT (v, cm->backend_decl, vtab);
	}
      else
	{
	  val = gfc_conv_initializer (c->expr, &cm->ts,
				      TREE_TYPE (cm->backend_decl),
				      cm->attr.dimension, cm->attr.pointer,
				      cm->attr.proc_pointer);

	  /* Append it to the constructor list.  */
	  CONSTRUCTOR_APPEND_ELT (v, cm->backend_decl, val);
	}
    }
  se->expr = build_constructor (type, v);
  if (init) 
    TREE_CONSTANT (se->expr) = 1;
}


/* Translate a substring expression.  */

static void
gfc_conv_substring_expr (gfc_se * se, gfc_expr * expr)
{
  gfc_ref *ref;

  ref = expr->ref;

  gcc_assert (ref == NULL || ref->type == REF_SUBSTRING);

  se->expr = gfc_build_wide_string_const (expr->ts.kind,
					  expr->value.character.length,
					  expr->value.character.string);

  se->string_length = TYPE_MAX_VALUE (TYPE_DOMAIN (TREE_TYPE (se->expr)));
  TYPE_STRING_FLAG (TREE_TYPE (se->expr)) = 1;

  if (ref)
    gfc_conv_substring (se, ref, expr->ts.kind, NULL, &expr->where);
}


/* Entry point for expression translation.  Evaluates a scalar quantity.
   EXPR is the expression to be translated, and SE is the state structure if
   called from within the scalarized.  */

void
gfc_conv_expr (gfc_se * se, gfc_expr * expr)
{
  gfc_ss *ss;

  ss = se->ss;
  if (ss && ss->info->expr == expr
      && (ss->info->type == GFC_SS_SCALAR
	  || ss->info->type == GFC_SS_REFERENCE))
    {
      gfc_ss_info *ss_info;

      ss_info = ss->info;
      /* Substitute a scalar expression evaluated outside the scalarization
         loop.  */
      se->expr = ss_info->data.scalar.value;
      /* If the reference can be NULL, the value field contains the reference,
	 not the value the reference points to (see gfc_add_loop_ss_code).  */
      if (ss_info->data.scalar.can_be_null_ref)
	se->expr = build_fold_indirect_ref_loc (input_location, se->expr);

      se->string_length = ss_info->string_length;
      gfc_advance_se_ss_chain (se);
      return;
    }

  /* We need to convert the expressions for the iso_c_binding derived types.
     C_NULL_PTR and C_NULL_FUNPTR will be made EXPR_NULL, which evaluates to
     null_pointer_node.  C_PTR and C_FUNPTR are converted to match the
     typespec for the C_PTR and C_FUNPTR symbols, which has already been
     updated to be an integer with a kind equal to the size of a (void *).  */
  if (expr->ts.type == BT_DERIVED && expr->ts.u.derived
      && expr->ts.u.derived->attr.is_iso_c)
    {
      if (expr->expr_type == EXPR_VARIABLE
	  && (expr->symtree->n.sym->intmod_sym_id == ISOCBINDING_NULL_PTR
	      || expr->symtree->n.sym->intmod_sym_id
		 == ISOCBINDING_NULL_FUNPTR))
        {
	  /* Set expr_type to EXPR_NULL, which will result in
	     null_pointer_node being used below.  */
          expr->expr_type = EXPR_NULL;
        }
      else
        {
          /* Update the type/kind of the expression to be what the new
             type/kind are for the updated symbols of C_PTR/C_FUNPTR.  */
          expr->ts.type = expr->ts.u.derived->ts.type;
          expr->ts.f90_type = expr->ts.u.derived->ts.f90_type;
          expr->ts.kind = expr->ts.u.derived->ts.kind;
        }
    }

  /* TODO: make this work for general class array expressions.  */
  if (expr->ts.type == BT_CLASS
	&& expr->ref && expr->ref->type == REF_ARRAY)
    gfc_add_component_ref (expr, "_data");

  switch (expr->expr_type)
    {
    case EXPR_OP:
      gfc_conv_expr_op (se, expr);
      break;

    case EXPR_FUNCTION:
      gfc_conv_function_expr (se, expr);
      break;

    case EXPR_CONSTANT:
      gfc_conv_constant (se, expr);
      break;

    case EXPR_VARIABLE:
      gfc_conv_variable (se, expr);
      break;

    case EXPR_NULL:
      se->expr = null_pointer_node;
      break;

    case EXPR_SUBSTRING:
      gfc_conv_substring_expr (se, expr);
      break;

    case EXPR_STRUCTURE:
      gfc_conv_structure (se, expr, 0);
      break;

    case EXPR_ARRAY:
      gfc_conv_array_constructor_expr (se, expr);
      break;

    default:
      gcc_unreachable ();
      break;
    }
}

/* Like gfc_conv_expr_val, but the value is also suitable for use in the lhs
   of an assignment.  */
void
gfc_conv_expr_lhs (gfc_se * se, gfc_expr * expr)
{
  gfc_conv_expr (se, expr);
  /* All numeric lvalues should have empty post chains.  If not we need to
     figure out a way of rewriting an lvalue so that it has no post chain.  */
  gcc_assert (expr->ts.type == BT_CHARACTER || !se->post.head);
}

/* Like gfc_conv_expr, but the POST block is guaranteed to be empty for
   numeric expressions.  Used for scalar values where inserting cleanup code
   is inconvenient.  */
void
gfc_conv_expr_val (gfc_se * se, gfc_expr * expr)
{
  tree val;

  gcc_assert (expr->ts.type != BT_CHARACTER);
  gfc_conv_expr (se, expr);
  if (se->post.head)
    {
      val = gfc_create_var (TREE_TYPE (se->expr), NULL);
      gfc_add_modify (&se->pre, val, se->expr);
      se->expr = val;
      gfc_add_block_to_block (&se->pre, &se->post);
    }
}

/* Helper to translate an expression and convert it to a particular type.  */
void
gfc_conv_expr_type (gfc_se * se, gfc_expr * expr, tree type)
{
  gfc_conv_expr_val (se, expr);
  se->expr = convert (type, se->expr);
}


/* Converts an expression so that it can be passed by reference.  Scalar
   values only.  */

void
gfc_conv_expr_reference (gfc_se * se, gfc_expr * expr)
{
  gfc_ss *ss;
  tree var;

  ss = se->ss;
  if (ss && ss->info->expr == expr
      && ss->info->type == GFC_SS_REFERENCE)
    {
      /* Returns a reference to the scalar evaluated outside the loop
	 for this case.  */
      gfc_conv_expr (se, expr);
      se->expr = gfc_build_addr_expr (NULL_TREE, se->expr);
      return;
    }

  if (expr->ts.type == BT_CHARACTER)
    {
      gfc_conv_expr (se, expr);
      gfc_conv_string_parameter (se);
      return;
    }

  if (expr->expr_type == EXPR_VARIABLE)
    {
      se->want_pointer = 1;
      gfc_conv_expr (se, expr);
      if (se->post.head)
	{
	  var = gfc_create_var (TREE_TYPE (se->expr), NULL);
	  gfc_add_modify (&se->pre, var, se->expr);
	  gfc_add_block_to_block (&se->pre, &se->post);
	  se->expr = var;
	}
      return;
    }

  if (expr->expr_type == EXPR_FUNCTION
      && ((expr->value.function.esym
	   && expr->value.function.esym->result->attr.pointer
	   && !expr->value.function.esym->result->attr.dimension)
	  || (!expr->value.function.esym
	      && expr->symtree->n.sym->attr.pointer
	      && !expr->symtree->n.sym->attr.dimension)))
    {
      se->want_pointer = 1;
      gfc_conv_expr (se, expr);
      var = gfc_create_var (TREE_TYPE (se->expr), NULL);
      gfc_add_modify (&se->pre, var, se->expr);
      se->expr = var;
      return;
    }

  gfc_conv_expr (se, expr);

  /* Create a temporary var to hold the value.  */
  if (TREE_CONSTANT (se->expr))
    {
      tree tmp = se->expr;
      STRIP_TYPE_NOPS (tmp);
      var = build_decl (input_location,
			CONST_DECL, NULL, TREE_TYPE (tmp));
      DECL_INITIAL (var) = tmp;
      TREE_STATIC (var) = 1;
      pushdecl (var);
    }
  else
    {
      var = gfc_create_var (TREE_TYPE (se->expr), NULL);
      gfc_add_modify (&se->pre, var, se->expr);
    }
  gfc_add_block_to_block (&se->pre, &se->post);

  /* Take the address of that value.  */
  se->expr = gfc_build_addr_expr (NULL_TREE, var);
}


tree
gfc_trans_pointer_assign (gfc_code * code)
{
  return gfc_trans_pointer_assignment (code->expr1, code->expr2);
}


/* Generate code for a pointer assignment.  */

tree
gfc_trans_pointer_assignment (gfc_expr * expr1, gfc_expr * expr2)
{
  gfc_se lse;
  gfc_se rse;
  gfc_ss *lss;
  gfc_ss *rss;
  stmtblock_t block;
  tree desc;
  tree tmp;
  tree decl;

  gfc_start_block (&block);

  gfc_init_se (&lse, NULL);

  lss = gfc_walk_expr (expr1);
  rss = gfc_walk_expr (expr2);
  if (lss == gfc_ss_terminator)
    {
      /* Scalar pointers.  */
      lse.want_pointer = 1;
      gfc_conv_expr (&lse, expr1);
      gcc_assert (rss == gfc_ss_terminator);
      gfc_init_se (&rse, NULL);
      rse.want_pointer = 1;
      gfc_conv_expr (&rse, expr2);

      if (expr1->symtree->n.sym->attr.proc_pointer
	  && expr1->symtree->n.sym->attr.dummy)
	lse.expr = build_fold_indirect_ref_loc (input_location,
					    lse.expr);

      if (expr2->symtree && expr2->symtree->n.sym->attr.proc_pointer
	  && expr2->symtree->n.sym->attr.dummy)
	rse.expr = build_fold_indirect_ref_loc (input_location,
					    rse.expr);

      gfc_add_block_to_block (&block, &lse.pre);
      gfc_add_block_to_block (&block, &rse.pre);

      /* Check character lengths if character expression.  The test is only
	 really added if -fbounds-check is enabled.  Exclude deferred
	 character length lefthand sides.  */
      if (expr1->ts.type == BT_CHARACTER && expr2->expr_type != EXPR_NULL
	  && !(expr1->ts.deferred
			&& (TREE_CODE (lse.string_length) == VAR_DECL))
	  && !expr1->symtree->n.sym->attr.proc_pointer
	  && !gfc_is_proc_ptr_comp (expr1, NULL))
	{
	  gcc_assert (expr2->ts.type == BT_CHARACTER);
	  gcc_assert (lse.string_length && rse.string_length);
	  gfc_trans_same_strlen_check ("pointer assignment", &expr1->where,
				       lse.string_length, rse.string_length,
				       &block);
	}

      /* The assignment to an deferred character length sets the string
	 length to that of the rhs.  */
      if (expr1->ts.deferred && (TREE_CODE (lse.string_length) == VAR_DECL))
	{
	  if (expr2->expr_type != EXPR_NULL)
	    gfc_add_modify (&block, lse.string_length, rse.string_length);
	  else
	    gfc_add_modify (&block, lse.string_length,
			    build_int_cst (gfc_charlen_type_node, 0));
	}

      gfc_add_modify (&block, lse.expr,
			   fold_convert (TREE_TYPE (lse.expr), rse.expr));

      gfc_add_block_to_block (&block, &rse.post);
      gfc_add_block_to_block (&block, &lse.post);
    }
  else
    {
      gfc_ref* remap;
      bool rank_remap;
      tree strlen_lhs;
      tree strlen_rhs = NULL_TREE;

      /* Array pointer.  Find the last reference on the LHS and if it is an
	 array section ref, we're dealing with bounds remapping.  In this case,
	 set it to AR_FULL so that gfc_conv_expr_descriptor does
	 not see it and process the bounds remapping afterwards explicitely.  */
      for (remap = expr1->ref; remap; remap = remap->next)
	if (!remap->next && remap->type == REF_ARRAY
	    && remap->u.ar.type == AR_SECTION)
	  {  
	    remap->u.ar.type = AR_FULL;
	    break;
	  }
      rank_remap = (remap && remap->u.ar.end[0]);

      gfc_conv_expr_descriptor (&lse, expr1, lss);
      strlen_lhs = lse.string_length;
      desc = lse.expr;

      if (expr2->expr_type == EXPR_NULL)
	{
	  /* Just set the data pointer to null.  */
	  gfc_conv_descriptor_data_set (&lse.pre, lse.expr, null_pointer_node);
	}
      else if (rank_remap)
	{
	  /* If we are rank-remapping, just get the RHS's descriptor and
	     process this later on.  */
	  gfc_init_se (&rse, NULL);
	  rse.direct_byref = 1;
	  rse.byref_noassign = 1;
	  gfc_conv_expr_descriptor (&rse, expr2, rss);
	  strlen_rhs = rse.string_length;
	}
      else if (expr2->expr_type == EXPR_VARIABLE)
	{
	  /* Assign directly to the LHS's descriptor.  */
	  lse.direct_byref = 1;
	  gfc_conv_expr_descriptor (&lse, expr2, rss);
	  strlen_rhs = lse.string_length;

	  /* If this is a subreference array pointer assignment, use the rhs
	     descriptor element size for the lhs span.  */
	  if (expr1->symtree->n.sym->attr.subref_array_pointer)
	    {
	      decl = expr1->symtree->n.sym->backend_decl;
	      gfc_init_se (&rse, NULL);
	      rse.descriptor_only = 1;
	      gfc_conv_expr (&rse, expr2);
	      tmp = gfc_get_element_type (TREE_TYPE (rse.expr));
	      tmp = fold_convert (gfc_array_index_type, size_in_bytes (tmp));
	      if (!INTEGER_CST_P (tmp))
		gfc_add_block_to_block (&lse.post, &rse.pre);
	      gfc_add_modify (&lse.post, GFC_DECL_SPAN(decl), tmp);
	    }
	}
      else
	{
	  /* Assign to a temporary descriptor and then copy that
	     temporary to the pointer.  */
	  tmp = gfc_create_var (TREE_TYPE (desc), "ptrtemp");

	  lse.expr = tmp;
	  lse.direct_byref = 1;
	  gfc_conv_expr_descriptor (&lse, expr2, rss);
	  strlen_rhs = lse.string_length;
	  gfc_add_modify (&lse.pre, desc, tmp);
	}

      gfc_add_block_to_block (&block, &lse.pre);
      if (rank_remap)
	gfc_add_block_to_block (&block, &rse.pre);

      /* If we do bounds remapping, update LHS descriptor accordingly.  */
      if (remap)
	{
	  int dim;
	  gcc_assert (remap->u.ar.dimen == expr1->rank);

	  if (rank_remap)
	    {
	      /* Do rank remapping.  We already have the RHS's descriptor
		 converted in rse and now have to build the correct LHS
		 descriptor for it.  */

	      tree dtype, data;
	      tree offs, stride;
	      tree lbound, ubound;

	      /* Set dtype.  */
	      dtype = gfc_conv_descriptor_dtype (desc);
	      tmp = gfc_get_dtype (TREE_TYPE (desc));
	      gfc_add_modify (&block, dtype, tmp);

	      /* Copy data pointer.  */
	      data = gfc_conv_descriptor_data_get (rse.expr);
	      gfc_conv_descriptor_data_set (&block, desc, data);

	      /* Copy offset but adjust it such that it would correspond
		 to a lbound of zero.  */
	      offs = gfc_conv_descriptor_offset_get (rse.expr);
	      for (dim = 0; dim < expr2->rank; ++dim)
		{
		  stride = gfc_conv_descriptor_stride_get (rse.expr,
							   gfc_rank_cst[dim]);
		  lbound = gfc_conv_descriptor_lbound_get (rse.expr,
							   gfc_rank_cst[dim]);
		  tmp = fold_build2_loc (input_location, MULT_EXPR,
					 gfc_array_index_type, stride, lbound);
		  offs = fold_build2_loc (input_location, PLUS_EXPR,
					  gfc_array_index_type, offs, tmp);
		}
	      gfc_conv_descriptor_offset_set (&block, desc, offs);

	      /* Set the bounds as declared for the LHS and calculate strides as
		 well as another offset update accordingly.  */
	      stride = gfc_conv_descriptor_stride_get (rse.expr,
						       gfc_rank_cst[0]);
	      for (dim = 0; dim < expr1->rank; ++dim)
		{
		  gfc_se lower_se;
		  gfc_se upper_se;

		  gcc_assert (remap->u.ar.start[dim] && remap->u.ar.end[dim]);

		  /* Convert declared bounds.  */
		  gfc_init_se (&lower_se, NULL);
		  gfc_init_se (&upper_se, NULL);
		  gfc_conv_expr (&lower_se, remap->u.ar.start[dim]);
		  gfc_conv_expr (&upper_se, remap->u.ar.end[dim]);

		  gfc_add_block_to_block (&block, &lower_se.pre);
		  gfc_add_block_to_block (&block, &upper_se.pre);

		  lbound = fold_convert (gfc_array_index_type, lower_se.expr);
		  ubound = fold_convert (gfc_array_index_type, upper_se.expr);

		  lbound = gfc_evaluate_now (lbound, &block);
		  ubound = gfc_evaluate_now (ubound, &block);

		  gfc_add_block_to_block (&block, &lower_se.post);
		  gfc_add_block_to_block (&block, &upper_se.post);

		  /* Set bounds in descriptor.  */
		  gfc_conv_descriptor_lbound_set (&block, desc,
						  gfc_rank_cst[dim], lbound);
		  gfc_conv_descriptor_ubound_set (&block, desc,
						  gfc_rank_cst[dim], ubound);

		  /* Set stride.  */
		  stride = gfc_evaluate_now (stride, &block);
		  gfc_conv_descriptor_stride_set (&block, desc,
						  gfc_rank_cst[dim], stride);

		  /* Update offset.  */
		  offs = gfc_conv_descriptor_offset_get (desc);
		  tmp = fold_build2_loc (input_location, MULT_EXPR,
					 gfc_array_index_type, lbound, stride);
		  offs = fold_build2_loc (input_location, MINUS_EXPR,
					  gfc_array_index_type, offs, tmp);
		  offs = gfc_evaluate_now (offs, &block);
		  gfc_conv_descriptor_offset_set (&block, desc, offs);

		  /* Update stride.  */
		  tmp = gfc_conv_array_extent_dim (lbound, ubound, NULL);
		  stride = fold_build2_loc (input_location, MULT_EXPR,
					    gfc_array_index_type, stride, tmp);
		}
	    }
	  else
	    {
	      /* Bounds remapping.  Just shift the lower bounds.  */

	      gcc_assert (expr1->rank == expr2->rank);

	      for (dim = 0; dim < remap->u.ar.dimen; ++dim)
		{
		  gfc_se lbound_se;

		  gcc_assert (remap->u.ar.start[dim]);
		  gcc_assert (!remap->u.ar.end[dim]);
		  gfc_init_se (&lbound_se, NULL);
		  gfc_conv_expr (&lbound_se, remap->u.ar.start[dim]);

		  gfc_add_block_to_block (&block, &lbound_se.pre);
		  gfc_conv_shift_descriptor_lbound (&block, desc,
						    dim, lbound_se.expr);
		  gfc_add_block_to_block (&block, &lbound_se.post);
		}
	    }
	}

      /* Check string lengths if applicable.  The check is only really added
	 to the output code if -fbounds-check is enabled.  */
      if (expr1->ts.type == BT_CHARACTER && expr2->expr_type != EXPR_NULL)
	{
	  gcc_assert (expr2->ts.type == BT_CHARACTER);
	  gcc_assert (strlen_lhs && strlen_rhs);
	  gfc_trans_same_strlen_check ("pointer assignment", &expr1->where,
				       strlen_lhs, strlen_rhs, &block);
	}

      /* If rank remapping was done, check with -fcheck=bounds that
	 the target is at least as large as the pointer.  */
      if (rank_remap && (gfc_option.rtcheck & GFC_RTCHECK_BOUNDS))
	{
	  tree lsize, rsize;
	  tree fault;
	  const char* msg;

	  lsize = gfc_conv_descriptor_size (lse.expr, expr1->rank);
	  rsize = gfc_conv_descriptor_size (rse.expr, expr2->rank);

	  lsize = gfc_evaluate_now (lsize, &block);
	  rsize = gfc_evaluate_now (rsize, &block);
	  fault = fold_build2_loc (input_location, LT_EXPR, boolean_type_node,
				   rsize, lsize);

	  msg = _("Target of rank remapping is too small (%ld < %ld)");
	  gfc_trans_runtime_check (true, false, fault, &block, &expr2->where,
				   msg, rsize, lsize);
	}

      gfc_add_block_to_block (&block, &lse.post);
      if (rank_remap)
	gfc_add_block_to_block (&block, &rse.post);
    }

  return gfc_finish_block (&block);
}


/* Makes sure se is suitable for passing as a function string parameter.  */
/* TODO: Need to check all callers of this function.  It may be abused.  */

void
gfc_conv_string_parameter (gfc_se * se)
{
  tree type;

  if (TREE_CODE (se->expr) == STRING_CST)
    {
      type = TREE_TYPE (TREE_TYPE (se->expr));
      se->expr = gfc_build_addr_expr (build_pointer_type (type), se->expr);
      return;
    }

  if (TYPE_STRING_FLAG (TREE_TYPE (se->expr)))
    {
      if (TREE_CODE (se->expr) != INDIRECT_REF)
	{
	  type = TREE_TYPE (se->expr);
          se->expr = gfc_build_addr_expr (build_pointer_type (type), se->expr);
	}
      else
	{
	  type = gfc_get_character_type_len (gfc_default_character_kind,
					     se->string_length);
	  type = build_pointer_type (type);
	  se->expr = gfc_build_addr_expr (type, se->expr);
	}
    }

  gcc_assert (POINTER_TYPE_P (TREE_TYPE (se->expr)));
}


/* Generate code for assignment of scalar variables.  Includes character
   strings and derived types with allocatable components.
   If you know that the LHS has no allocations, set dealloc to false.  */

tree
gfc_trans_scalar_assign (gfc_se * lse, gfc_se * rse, gfc_typespec ts,
			 bool l_is_temp, bool r_is_var, bool dealloc)
{
  stmtblock_t block;
  tree tmp;
  tree cond;

  gfc_init_block (&block);

  if (ts.type == BT_CHARACTER)
    {
      tree rlen = NULL;
      tree llen = NULL;

      if (lse->string_length != NULL_TREE)
	{
	  gfc_conv_string_parameter (lse);
	  gfc_add_block_to_block (&block, &lse->pre);
	  llen = lse->string_length;
	}

      if (rse->string_length != NULL_TREE)
	{
	  gcc_assert (rse->string_length != NULL_TREE);
	  gfc_conv_string_parameter (rse);
	  gfc_add_block_to_block (&block, &rse->pre);
	  rlen = rse->string_length;
	}

      gfc_trans_string_copy (&block, llen, lse->expr, ts.kind, rlen,
			     rse->expr, ts.kind);
    }
  else if (ts.type == BT_DERIVED && ts.u.derived->attr.alloc_comp)
    {
      cond = NULL_TREE;
	
      /* Are the rhs and the lhs the same?  */
      if (r_is_var)
	{
	  cond = fold_build2_loc (input_location, EQ_EXPR, boolean_type_node,
				  gfc_build_addr_expr (NULL_TREE, lse->expr),
				  gfc_build_addr_expr (NULL_TREE, rse->expr));
	  cond = gfc_evaluate_now (cond, &lse->pre);
	}

      /* Deallocate the lhs allocated components as long as it is not
	 the same as the rhs.  This must be done following the assignment
	 to prevent deallocating data that could be used in the rhs
	 expression.  */
      if (!l_is_temp && dealloc)
	{
	  tmp = gfc_evaluate_now (lse->expr, &lse->pre);
	  tmp = gfc_deallocate_alloc_comp (ts.u.derived, tmp, 0);
	  if (r_is_var)
	    tmp = build3_v (COND_EXPR, cond, build_empty_stmt (input_location),
			    tmp);
	  gfc_add_expr_to_block (&lse->post, tmp);
	}

      gfc_add_block_to_block (&block, &rse->pre);
      gfc_add_block_to_block (&block, &lse->pre);

      gfc_add_modify (&block, lse->expr,
			   fold_convert (TREE_TYPE (lse->expr), rse->expr));

      /* Do a deep copy if the rhs is a variable, if it is not the
	 same as the lhs.  */
      if (r_is_var)
	{
	  tmp = gfc_copy_alloc_comp (ts.u.derived, rse->expr, lse->expr, 0);
	  tmp = build3_v (COND_EXPR, cond, build_empty_stmt (input_location),
			  tmp);
	  gfc_add_expr_to_block (&block, tmp);
	}
    }
  else if (ts.type == BT_DERIVED || ts.type == BT_CLASS)
    {
      gfc_add_block_to_block (&block, &lse->pre);
      gfc_add_block_to_block (&block, &rse->pre);
      tmp = fold_build1_loc (input_location, VIEW_CONVERT_EXPR,
			     TREE_TYPE (lse->expr), rse->expr);
      gfc_add_modify (&block, lse->expr, tmp);
    }
  else
    {
      gfc_add_block_to_block (&block, &lse->pre);
      gfc_add_block_to_block (&block, &rse->pre);

      gfc_add_modify (&block, lse->expr,
		      fold_convert (TREE_TYPE (lse->expr), rse->expr));
    }

  gfc_add_block_to_block (&block, &lse->post);
  gfc_add_block_to_block (&block, &rse->post);

  return gfc_finish_block (&block);
}


/* There are quite a lot of restrictions on the optimisation in using an
   array function assign without a temporary.  */

static bool
arrayfunc_assign_needs_temporary (gfc_expr * expr1, gfc_expr * expr2)
{
  gfc_ref * ref;
  bool seen_array_ref;
  bool c = false;
  gfc_symbol *sym = expr1->symtree->n.sym;

  /* The caller has already checked rank>0 and expr_type == EXPR_FUNCTION.  */
  if (expr2->value.function.isym && !gfc_is_intrinsic_libcall (expr2))
    return true;

  /* Elemental functions are scalarized so that they don't need a
     temporary in gfc_trans_assignment_1, so return a true.  Otherwise,
     they would need special treatment in gfc_trans_arrayfunc_assign.  */
  if (expr2->value.function.esym != NULL
      && expr2->value.function.esym->attr.elemental)
    return true;

  /* Need a temporary if rhs is not FULL or a contiguous section.  */
  if (expr1->ref && !(gfc_full_array_ref_p (expr1->ref, &c) || c))
    return true;

  /* Need a temporary if EXPR1 can't be expressed as a descriptor.  */
  if (gfc_ref_needs_temporary_p (expr1->ref))
    return true;

  /* Functions returning pointers or allocatables need temporaries.  */
  c = expr2->value.function.esym
      ? (expr2->value.function.esym->attr.pointer 
	 || expr2->value.function.esym->attr.allocatable)
      : (expr2->symtree->n.sym->attr.pointer
	 || expr2->symtree->n.sym->attr.allocatable);
  if (c)
    return true;

  /* Character array functions need temporaries unless the
     character lengths are the same.  */
  if (expr2->ts.type == BT_CHARACTER && expr2->rank > 0)
    {
      if (expr1->ts.u.cl->length == NULL
	    || expr1->ts.u.cl->length->expr_type != EXPR_CONSTANT)
	return true;

      if (expr2->ts.u.cl->length == NULL
	    || expr2->ts.u.cl->length->expr_type != EXPR_CONSTANT)
	return true;

      if (mpz_cmp (expr1->ts.u.cl->length->value.integer,
		     expr2->ts.u.cl->length->value.integer) != 0)
	return true;
    }

  /* Check that no LHS component references appear during an array
     reference. This is needed because we do not have the means to
     span any arbitrary stride with an array descriptor. This check
     is not needed for the rhs because the function result has to be
     a complete type.  */
  seen_array_ref = false;
  for (ref = expr1->ref; ref; ref = ref->next)
    {
      if (ref->type == REF_ARRAY)
	seen_array_ref= true;
      else if (ref->type == REF_COMPONENT && seen_array_ref)
	return true;
    }

  /* Check for a dependency.  */
  if (gfc_check_fncall_dependency (expr1, INTENT_OUT,
				   expr2->value.function.esym,
				   expr2->value.function.actual,
				   NOT_ELEMENTAL))
    return true;

  /* If we have reached here with an intrinsic function, we do not
     need a temporary except in the particular case that reallocation
     on assignment is active and the lhs is allocatable and a target.  */
  if (expr2->value.function.isym)
    return (gfc_option.flag_realloc_lhs
	      && sym->attr.allocatable
	      && sym->attr.target);

  /* If the LHS is a dummy, we need a temporary if it is not
     INTENT(OUT).  */
  if (sym->attr.dummy && sym->attr.intent != INTENT_OUT)
    return true;

  /* If the lhs has been host_associated, is in common, a pointer or is
     a target and the function is not using a RESULT variable, aliasing
     can occur and a temporary is needed.  */
  if ((sym->attr.host_assoc
	   || sym->attr.in_common
	   || sym->attr.pointer
	   || sym->attr.cray_pointee
	   || sym->attr.target)
	&& expr2->symtree != NULL
	&& expr2->symtree->n.sym == expr2->symtree->n.sym->result)
    return true;

  /* A PURE function can unconditionally be called without a temporary.  */
  if (expr2->value.function.esym != NULL
      && expr2->value.function.esym->attr.pure)
    return false;

  /* Implicit_pure functions are those which could legally be declared
     to be PURE.  */
  if (expr2->value.function.esym != NULL
      && expr2->value.function.esym->attr.implicit_pure)
    return false;

  if (!sym->attr.use_assoc
	&& !sym->attr.in_common
	&& !sym->attr.pointer
	&& !sym->attr.target
	&& !sym->attr.cray_pointee
	&& expr2->value.function.esym)
    {
      /* A temporary is not needed if the function is not contained and
	 the variable is local or host associated and not a pointer or
	 a target. */
      if (!expr2->value.function.esym->attr.contained)
	return false;

      /* A temporary is not needed if the lhs has never been host
	 associated and the procedure is contained.  */
      else if (!sym->attr.host_assoc)
	return false;

      /* A temporary is not needed if the variable is local and not
	 a pointer, a target or a result.  */
      if (sym->ns->parent
	    && expr2->value.function.esym->ns == sym->ns->parent)
	return false;
    }

  /* Default to temporary use.  */
  return true;
}


/* Provide the loop info so that the lhs descriptor can be built for
   reallocatable assignments from extrinsic function calls.  */

static void
realloc_lhs_loop_for_fcn_call (gfc_se *se, locus *where, gfc_ss **ss,
			       gfc_loopinfo *loop)
{
  /* Signal that the function call should not be made by
     gfc_conv_loop_setup. */
  se->ss->is_alloc_lhs = 1;
  gfc_init_loopinfo (loop);
  gfc_add_ss_to_loop (loop, *ss);
  gfc_add_ss_to_loop (loop, se->ss);
  gfc_conv_ss_startstride (loop);
  gfc_conv_loop_setup (loop, where);
  gfc_copy_loopinfo_to_se (se, loop);
  gfc_add_block_to_block (&se->pre, &loop->pre);
  gfc_add_block_to_block (&se->pre, &loop->post);
  se->ss->is_alloc_lhs = 0;
}


/* For Assignment to a reallocatable lhs from intrinsic functions,
   replace the se.expr (ie. the result) with a temporary descriptor.
   Null the data field so that the library allocates space for the
   result. Free the data of the original descriptor after the function,
   in case it appears in an argument expression and transfer the
   result to the original descriptor.  */

static void
fcncall_realloc_result (gfc_se *se, int rank)
{
  tree desc;
  tree res_desc;
  tree tmp;
  tree offset;
  int n;

  /* Use the allocation done by the library.  Substitute the lhs
     descriptor with a copy, whose data field is nulled.*/
  desc = build_fold_indirect_ref_loc (input_location, se->expr);
  /* Unallocated, the descriptor does not have a dtype.  */
  tmp = gfc_conv_descriptor_dtype (desc);
  gfc_add_modify (&se->pre, tmp, gfc_get_dtype (TREE_TYPE (desc)));
  res_desc = gfc_evaluate_now (desc, &se->pre);
  gfc_conv_descriptor_data_set (&se->pre, res_desc, null_pointer_node);
  se->expr = gfc_build_addr_expr (TREE_TYPE (se->expr), res_desc);

  /* Free the lhs after the function call and copy the result to
     the lhs descriptor.  */
  tmp = gfc_conv_descriptor_data_get (desc);
  tmp = gfc_call_free (fold_convert (pvoid_type_node, tmp));
  gfc_add_expr_to_block (&se->post, tmp);
  gfc_add_modify (&se->post, desc, res_desc);

  offset = gfc_index_zero_node;
  tmp = gfc_index_one_node;
  /* Now reset the bounds from zero based to unity based.  */
  for (n = 0 ; n < rank; n++)
    {
      /* Accumulate the offset.  */
      offset = fold_build2_loc (input_location, MINUS_EXPR,
				gfc_array_index_type,
				offset, tmp);
      /* Now do the bounds.  */
      gfc_conv_descriptor_offset_set (&se->post, desc, tmp);
      tmp = gfc_conv_descriptor_ubound_get (desc, gfc_rank_cst[n]);
      tmp = fold_build2_loc (input_location, PLUS_EXPR,
			     gfc_array_index_type,
			     tmp, gfc_index_one_node);
      gfc_conv_descriptor_lbound_set (&se->post, desc,
				      gfc_rank_cst[n],
				      gfc_index_one_node);
      gfc_conv_descriptor_ubound_set (&se->post, desc,
				      gfc_rank_cst[n], tmp);

      /* The extent for the next contribution to offset.  */
      tmp = fold_build2_loc (input_location, MINUS_EXPR,
			     gfc_array_index_type,
			     gfc_conv_descriptor_ubound_get (desc, gfc_rank_cst[n]),
			     gfc_conv_descriptor_lbound_get (desc, gfc_rank_cst[n]));
      tmp = fold_build2_loc (input_location, PLUS_EXPR,
			     gfc_array_index_type,
			     tmp, gfc_index_one_node);
    }
  gfc_conv_descriptor_offset_set (&se->post, desc, offset);
}



/* Try to translate array(:) = func (...), where func is a transformational
   array function, without using a temporary.  Returns NULL if this isn't the
   case.  */

static tree
gfc_trans_arrayfunc_assign (gfc_expr * expr1, gfc_expr * expr2)
{
  gfc_se se;
  gfc_ss *ss;
  gfc_component *comp = NULL;
  gfc_loopinfo loop;

  if (arrayfunc_assign_needs_temporary (expr1, expr2))
    return NULL;

  /* The frontend doesn't seem to bother filling in expr->symtree for intrinsic
     functions.  */
  gcc_assert (expr2->value.function.isym
	      || (gfc_is_proc_ptr_comp (expr2, &comp)
		  && comp && comp->attr.dimension)
	      || (!comp && gfc_return_by_reference (expr2->value.function.esym)
		  && expr2->value.function.esym->result->attr.dimension));

  ss = gfc_walk_expr (expr1);
  gcc_assert (ss != gfc_ss_terminator);
  gfc_init_se (&se, NULL);
  gfc_start_block (&se.pre);
  se.want_pointer = 1;

  gfc_conv_array_parameter (&se, expr1, ss, false, NULL, NULL, NULL);

  if (expr1->ts.type == BT_DERIVED
	&& expr1->ts.u.derived->attr.alloc_comp)
    {
      tree tmp;
      tmp = gfc_deallocate_alloc_comp (expr1->ts.u.derived, se.expr,
				       expr1->rank);
      gfc_add_expr_to_block (&se.pre, tmp);
    }

  se.direct_byref = 1;
  se.ss = gfc_walk_expr (expr2);
  gcc_assert (se.ss != gfc_ss_terminator);

  /* Reallocate on assignment needs the loopinfo for extrinsic functions.
     This is signalled to gfc_conv_procedure_call by setting is_alloc_lhs.
     Clearly, this cannot be done for an allocatable function result, since
     the shape of the result is unknown and, in any case, the function must
     correctly take care of the reallocation internally. For intrinsic
     calls, the array data is freed and the library takes care of allocation.
     TODO: Add logic of trans-array.c: gfc_alloc_allocatable_for_assignment
     to the library.  */    
  if (gfc_option.flag_realloc_lhs
	&& gfc_is_reallocatable_lhs (expr1)
	&& !gfc_expr_attr (expr1).codimension
	&& !gfc_is_coindexed (expr1)
	&& !(expr2->value.function.esym
	    && expr2->value.function.esym->result->attr.allocatable))
    {
      if (!expr2->value.function.isym)
	{
	  realloc_lhs_loop_for_fcn_call (&se, &expr1->where, &ss, &loop);
	  ss->is_alloc_lhs = 1;
	}
      else
	fcncall_realloc_result (&se, expr1->rank);
    }

  gfc_conv_function_expr (&se, expr2);
  gfc_add_block_to_block (&se.pre, &se.post);

  return gfc_finish_block (&se.pre);
}


/* Try to efficiently translate array(:) = 0.  Return NULL if this
   can't be done.  */

static tree
gfc_trans_zero_assign (gfc_expr * expr)
{
  tree dest, len, type;
  tree tmp;
  gfc_symbol *sym;

  sym = expr->symtree->n.sym;
  dest = gfc_get_symbol_decl (sym);

  type = TREE_TYPE (dest);
  if (POINTER_TYPE_P (type))
    type = TREE_TYPE (type);
  if (!GFC_ARRAY_TYPE_P (type))
    return NULL_TREE;

  /* Determine the length of the array.  */
  len = GFC_TYPE_ARRAY_SIZE (type);
  if (!len || TREE_CODE (len) != INTEGER_CST)
    return NULL_TREE;

  tmp = TYPE_SIZE_UNIT (gfc_get_element_type (type));
  len = fold_build2_loc (input_location, MULT_EXPR, gfc_array_index_type, len,
			 fold_convert (gfc_array_index_type, tmp));

  /* If we are zeroing a local array avoid taking its address by emitting
     a = {} instead.  */
  if (!POINTER_TYPE_P (TREE_TYPE (dest)))
    return build2_loc (input_location, MODIFY_EXPR, void_type_node,
		       dest, build_constructor (TREE_TYPE (dest), NULL));

  /* Convert arguments to the correct types.  */
  dest = fold_convert (pvoid_type_node, dest);
  len = fold_convert (size_type_node, len);

  /* Construct call to __builtin_memset.  */
  tmp = build_call_expr_loc (input_location,
			     builtin_decl_explicit (BUILT_IN_MEMSET),
			     3, dest, integer_zero_node, len);
  return fold_convert (void_type_node, tmp);
}


/* Helper for gfc_trans_array_copy and gfc_trans_array_constructor_copy
   that constructs the call to __builtin_memcpy.  */

tree
gfc_build_memcpy_call (tree dst, tree src, tree len)
{
  tree tmp;

  /* Convert arguments to the correct types.  */
  if (!POINTER_TYPE_P (TREE_TYPE (dst)))
    dst = gfc_build_addr_expr (pvoid_type_node, dst);
  else
    dst = fold_convert (pvoid_type_node, dst);

  if (!POINTER_TYPE_P (TREE_TYPE (src)))
    src = gfc_build_addr_expr (pvoid_type_node, src);
  else
    src = fold_convert (pvoid_type_node, src);

  len = fold_convert (size_type_node, len);

  /* Construct call to __builtin_memcpy.  */
  tmp = build_call_expr_loc (input_location,
			     builtin_decl_explicit (BUILT_IN_MEMCPY),
			     3, dst, src, len);
  return fold_convert (void_type_node, tmp);
}


/* Try to efficiently translate dst(:) = src(:).  Return NULL if this
   can't be done.  EXPR1 is the destination/lhs and EXPR2 is the
   source/rhs, both are gfc_full_array_ref_p which have been checked for
   dependencies.  */

static tree
gfc_trans_array_copy (gfc_expr * expr1, gfc_expr * expr2)
{
  tree dst, dlen, dtype;
  tree src, slen, stype;
  tree tmp;

  dst = gfc_get_symbol_decl (expr1->symtree->n.sym);
  src = gfc_get_symbol_decl (expr2->symtree->n.sym);

  dtype = TREE_TYPE (dst);
  if (POINTER_TYPE_P (dtype))
    dtype = TREE_TYPE (dtype);
  stype = TREE_TYPE (src);
  if (POINTER_TYPE_P (stype))
    stype = TREE_TYPE (stype);

  if (!GFC_ARRAY_TYPE_P (dtype) || !GFC_ARRAY_TYPE_P (stype))
    return NULL_TREE;

  /* Determine the lengths of the arrays.  */
  dlen = GFC_TYPE_ARRAY_SIZE (dtype);
  if (!dlen || TREE_CODE (dlen) != INTEGER_CST)
    return NULL_TREE;
  tmp = TYPE_SIZE_UNIT (gfc_get_element_type (dtype));
  dlen = fold_build2_loc (input_location, MULT_EXPR, gfc_array_index_type,
			  dlen, fold_convert (gfc_array_index_type, tmp));

  slen = GFC_TYPE_ARRAY_SIZE (stype);
  if (!slen || TREE_CODE (slen) != INTEGER_CST)
    return NULL_TREE;
  tmp = TYPE_SIZE_UNIT (gfc_get_element_type (stype));
  slen = fold_build2_loc (input_location, MULT_EXPR, gfc_array_index_type,
			  slen, fold_convert (gfc_array_index_type, tmp));

  /* Sanity check that they are the same.  This should always be
     the case, as we should already have checked for conformance.  */
  if (!tree_int_cst_equal (slen, dlen))
    return NULL_TREE;

  return gfc_build_memcpy_call (dst, src, dlen);
}


/* Try to efficiently translate array(:) = (/ ... /).  Return NULL if
   this can't be done.  EXPR1 is the destination/lhs for which
   gfc_full_array_ref_p is true, and EXPR2 is the source/rhs.  */

static tree
gfc_trans_array_constructor_copy (gfc_expr * expr1, gfc_expr * expr2)
{
  unsigned HOST_WIDE_INT nelem;
  tree dst, dtype;
  tree src, stype;
  tree len;
  tree tmp;

  nelem = gfc_constant_array_constructor_p (expr2->value.constructor);
  if (nelem == 0)
    return NULL_TREE;

  dst = gfc_get_symbol_decl (expr1->symtree->n.sym);
  dtype = TREE_TYPE (dst);
  if (POINTER_TYPE_P (dtype))
    dtype = TREE_TYPE (dtype);
  if (!GFC_ARRAY_TYPE_P (dtype))
    return NULL_TREE;

  /* Determine the lengths of the array.  */
  len = GFC_TYPE_ARRAY_SIZE (dtype);
  if (!len || TREE_CODE (len) != INTEGER_CST)
    return NULL_TREE;

  /* Confirm that the constructor is the same size.  */
  if (compare_tree_int (len, nelem) != 0)
    return NULL_TREE;

  tmp = TYPE_SIZE_UNIT (gfc_get_element_type (dtype));
  len = fold_build2_loc (input_location, MULT_EXPR, gfc_array_index_type, len,
			 fold_convert (gfc_array_index_type, tmp));

  stype = gfc_typenode_for_spec (&expr2->ts);
  src = gfc_build_constant_array_constructor (expr2, stype);

  stype = TREE_TYPE (src);
  if (POINTER_TYPE_P (stype))
    stype = TREE_TYPE (stype);

  return gfc_build_memcpy_call (dst, src, len);
}


/* Tells whether the expression is to be treated as a variable reference.  */

static bool
expr_is_variable (gfc_expr *expr)
{
  gfc_expr *arg;

  if (expr->expr_type == EXPR_VARIABLE)
    return true;

  arg = gfc_get_noncopying_intrinsic_argument (expr);
  if (arg)
    {
      gcc_assert (expr->value.function.isym->id == GFC_ISYM_TRANSPOSE);
      return expr_is_variable (arg);
    }

  return false;
}


/* Is the lhs OK for automatic reallocation?  */

static bool
is_scalar_reallocatable_lhs (gfc_expr *expr)
{
  gfc_ref * ref;

  /* An allocatable variable with no reference.  */
  if (expr->symtree->n.sym->attr.allocatable
	&& !expr->ref)
    return true;

  /* All that can be left are allocatable components.  */
  if ((expr->symtree->n.sym->ts.type != BT_DERIVED
	&& expr->symtree->n.sym->ts.type != BT_CLASS)
	|| !expr->symtree->n.sym->ts.u.derived->attr.alloc_comp)
    return false;

  /* Find an allocatable component ref last.  */
  for (ref = expr->ref; ref; ref = ref->next)
    if (ref->type == REF_COMPONENT
	  && !ref->next
	  && ref->u.c.component->attr.allocatable)
      return true;

  return false;
}


/* Allocate or reallocate scalar lhs, as necessary.  */

static void
alloc_scalar_allocatable_for_assignment (stmtblock_t *block,
					 tree string_length,
					 gfc_expr *expr1,
					 gfc_expr *expr2)

{
  tree cond;
  tree tmp;
  tree size;
  tree size_in_bytes;
  tree jump_label1;
  tree jump_label2;
  gfc_se lse;

  if (!expr1 || expr1->rank)
    return;

  if (!expr2 || expr2->rank)
    return;

  /* Since this is a scalar lhs, we can afford to do this.  That is,
     there is no risk of side effects being repeated.  */
  gfc_init_se (&lse, NULL);
  lse.want_pointer = 1;
  gfc_conv_expr (&lse, expr1);
  
  jump_label1 = gfc_build_label_decl (NULL_TREE);
  jump_label2 = gfc_build_label_decl (NULL_TREE);

  /* Do the allocation if the lhs is NULL. Otherwise go to label 1.  */
  tmp = build_int_cst (TREE_TYPE (lse.expr), 0);
  cond = fold_build2_loc (input_location, NE_EXPR, boolean_type_node,
			  lse.expr, tmp);
  tmp = build3_v (COND_EXPR, cond,
		  build1_v (GOTO_EXPR, jump_label1),
		  build_empty_stmt (input_location));
  gfc_add_expr_to_block (block, tmp);

  if (expr1->ts.type == BT_CHARACTER && expr1->ts.deferred)
    {
      /* Use the rhs string length and the lhs element size.  */
      size = string_length;
      tmp = TREE_TYPE (gfc_typenode_for_spec (&expr1->ts));
      tmp = TYPE_SIZE_UNIT (tmp);
      size_in_bytes = fold_build2_loc (input_location, MULT_EXPR,
				       TREE_TYPE (tmp), tmp,
				       fold_convert (TREE_TYPE (tmp), size));
    }
  else
    {
      /* Otherwise use the length in bytes of the rhs.  */
      size = TYPE_SIZE_UNIT (gfc_typenode_for_spec (&expr1->ts));
      size_in_bytes = size;
    }

  tmp = build_call_expr_loc (input_location,
			     builtin_decl_explicit (BUILT_IN_MALLOC),
			     1, size_in_bytes);
  tmp = fold_convert (TREE_TYPE (lse.expr), tmp);
  gfc_add_modify (block, lse.expr, tmp);
  if (expr1->ts.type == BT_CHARACTER && expr1->ts.deferred)
    {
      /* Deferred characters need checking for lhs and rhs string
	 length.  Other deferred parameter variables will have to
	 come here too.  */
      tmp = build1_v (GOTO_EXPR, jump_label2);
      gfc_add_expr_to_block (block, tmp);
    }
  tmp = build1_v (LABEL_EXPR, jump_label1);
  gfc_add_expr_to_block (block, tmp);

  /* For a deferred length character, reallocate if lengths of lhs and
     rhs are different.  */
  if (expr1->ts.type == BT_CHARACTER && expr1->ts.deferred)
    {
      cond = fold_build2_loc (input_location, EQ_EXPR, boolean_type_node,
			      expr1->ts.u.cl->backend_decl, size);
      /* Jump past the realloc if the lengths are the same.  */
      tmp = build3_v (COND_EXPR, cond,
		      build1_v (GOTO_EXPR, jump_label2),
		      build_empty_stmt (input_location));
      gfc_add_expr_to_block (block, tmp);
      tmp = build_call_expr_loc (input_location,
				 builtin_decl_explicit (BUILT_IN_REALLOC),
				 2, fold_convert (pvoid_type_node, lse.expr),
				 size_in_bytes);
      tmp = fold_convert (TREE_TYPE (lse.expr), tmp);
      gfc_add_modify (block, lse.expr, tmp);
      tmp = build1_v (LABEL_EXPR, jump_label2);
      gfc_add_expr_to_block (block, tmp);

      /* Update the lhs character length.  */
      size = string_length;
      gfc_add_modify (block, expr1->ts.u.cl->backend_decl, size);
    }
}


/* Subroutine of gfc_trans_assignment that actually scalarizes the
   assignment.  EXPR1 is the destination/LHS and EXPR2 is the source/RHS.
   init_flag indicates initialization expressions and dealloc that no
   deallocate prior assignment is needed (if in doubt, set true).  */

static tree
gfc_trans_assignment_1 (gfc_expr * expr1, gfc_expr * expr2, bool init_flag,
			bool dealloc)
{
  gfc_se lse;
  gfc_se rse;
  gfc_ss *lss;
  gfc_ss *lss_section;
  gfc_ss *rss;
  gfc_loopinfo loop;
  tree tmp;
  stmtblock_t block;
  stmtblock_t body;
  bool l_is_temp;
  bool scalar_to_array;
  bool def_clen_func;
  tree string_length;
  int n;

  /* Assignment of the form lhs = rhs.  */
  gfc_start_block (&block);

  gfc_init_se (&lse, NULL);
  gfc_init_se (&rse, NULL);

  /* Walk the lhs.  */
  lss = gfc_walk_expr (expr1);
  if (gfc_is_reallocatable_lhs (expr1)
	&& !(expr2->expr_type == EXPR_FUNCTION
	     && expr2->value.function.isym != NULL))
    lss->is_alloc_lhs = 1;
  rss = NULL;
  if (lss != gfc_ss_terminator)
    {
      /* The assignment needs scalarization.  */
      lss_section = lss;

      /* Find a non-scalar SS from the lhs.  */
      while (lss_section != gfc_ss_terminator
	     && lss_section->info->type != GFC_SS_SECTION)
	lss_section = lss_section->next;

      gcc_assert (lss_section != gfc_ss_terminator);

      /* Initialize the scalarizer.  */
      gfc_init_loopinfo (&loop);

      /* Walk the rhs.  */
      rss = gfc_walk_expr (expr2);
      if (rss == gfc_ss_terminator)
	/* The rhs is scalar.  Add a ss for the expression.  */
	rss = gfc_get_scalar_ss (gfc_ss_terminator, expr2);

      /* Associate the SS with the loop.  */
      gfc_add_ss_to_loop (&loop, lss);
      gfc_add_ss_to_loop (&loop, rss);

      /* Calculate the bounds of the scalarization.  */
      gfc_conv_ss_startstride (&loop);
      /* Enable loop reversal.  */
      for (n = 0; n < GFC_MAX_DIMENSIONS; n++)
	loop.reverse[n] = GFC_ENABLE_REVERSE;
      /* Resolve any data dependencies in the statement.  */
      gfc_conv_resolve_dependencies (&loop, lss, rss);
      /* Setup the scalarizing loops.  */
      gfc_conv_loop_setup (&loop, &expr2->where);

      /* Setup the gfc_se structures.  */
      gfc_copy_loopinfo_to_se (&lse, &loop);
      gfc_copy_loopinfo_to_se (&rse, &loop);

      rse.ss = rss;
      gfc_mark_ss_chain_used (rss, 1);
      if (loop.temp_ss == NULL)
	{
	  lse.ss = lss;
	  gfc_mark_ss_chain_used (lss, 1);
	}
      else
	{
	  lse.ss = loop.temp_ss;
	  gfc_mark_ss_chain_used (lss, 3);
	  gfc_mark_ss_chain_used (loop.temp_ss, 3);
	}

      /* Allow the scalarizer to workshare array assignments.  */
      if ((ompws_flags & OMPWS_WORKSHARE_FLAG) && loop.temp_ss == NULL)
	ompws_flags |= OMPWS_SCALARIZER_WS;

      /* Start the scalarized loop body.  */
      gfc_start_scalarized_body (&loop, &body);
    }
  else
    gfc_init_block (&body);

  l_is_temp = (lss != gfc_ss_terminator && loop.temp_ss != NULL);

  /* Translate the expression.  */
  gfc_conv_expr (&rse, expr2);

  /* Stabilize a string length for temporaries.  */
  if (expr2->ts.type == BT_CHARACTER)
    string_length = gfc_evaluate_now (rse.string_length, &rse.pre);
  else
    string_length = NULL_TREE;

  if (l_is_temp)
    {
      gfc_conv_tmp_array_ref (&lse);
      if (expr2->ts.type == BT_CHARACTER)
	lse.string_length = string_length;
    }
  else
    gfc_conv_expr (&lse, expr1);

  /* Assignments of scalar derived types with allocatable components
     to arrays must be done with a deep copy and the rhs temporary
     must have its components deallocated afterwards.  */
  scalar_to_array = (expr2->ts.type == BT_DERIVED
		       && expr2->ts.u.derived->attr.alloc_comp
		       && !expr_is_variable (expr2)
		       && !gfc_is_constant_expr (expr2)
		       && expr1->rank && !expr2->rank);
  if (scalar_to_array && dealloc)
    {
      tmp = gfc_deallocate_alloc_comp (expr2->ts.u.derived, rse.expr, 0);
      gfc_add_expr_to_block (&loop.post, tmp);
    }

  /* For a deferred character length function, the function call must
     happen before the (re)allocation of the lhs, otherwise the character
     length of the result is not known.  */
  def_clen_func = (((expr2->expr_type == EXPR_FUNCTION)
			   || (expr2->expr_type == EXPR_COMPCALL)
			   || (expr2->expr_type == EXPR_PPC))
		       && expr2->ts.deferred);
  if (gfc_option.flag_realloc_lhs
	&& expr2->ts.type == BT_CHARACTER
	&& (def_clen_func || expr2->expr_type == EXPR_OP)
	&& expr1->ts.deferred)
    gfc_add_block_to_block (&block, &rse.pre);

  tmp = gfc_trans_scalar_assign (&lse, &rse, expr1->ts,
				 l_is_temp || init_flag,
				 expr_is_variable (expr2) || scalar_to_array
				 || expr2->expr_type == EXPR_ARRAY, dealloc);
  gfc_add_expr_to_block (&body, tmp);

  if (lss == gfc_ss_terminator)
    {
      /* F2003: Add the code for reallocation on assignment.  */
      if (gfc_option.flag_realloc_lhs
	    && is_scalar_reallocatable_lhs (expr1))
	alloc_scalar_allocatable_for_assignment (&block, rse.string_length,
						 expr1, expr2);

      /* Use the scalar assignment as is.  */
      gfc_add_block_to_block (&block, &body);
    }
  else
    {
      gcc_assert (lse.ss == gfc_ss_terminator
		  && rse.ss == gfc_ss_terminator);

      if (l_is_temp)
	{
	  gfc_trans_scalarized_loop_boundary (&loop, &body);

	  /* We need to copy the temporary to the actual lhs.  */
	  gfc_init_se (&lse, NULL);
	  gfc_init_se (&rse, NULL);
	  gfc_copy_loopinfo_to_se (&lse, &loop);
	  gfc_copy_loopinfo_to_se (&rse, &loop);

	  rse.ss = loop.temp_ss;
	  lse.ss = lss;

	  gfc_conv_tmp_array_ref (&rse);
	  gfc_conv_expr (&lse, expr1);

	  gcc_assert (lse.ss == gfc_ss_terminator
		      && rse.ss == gfc_ss_terminator);

	  if (expr2->ts.type == BT_CHARACTER)
	    rse.string_length = string_length;

	  tmp = gfc_trans_scalar_assign (&lse, &rse, expr1->ts,
					 false, false, dealloc);
	  gfc_add_expr_to_block (&body, tmp);
	}

      /* F2003: Allocate or reallocate lhs of allocatable array.  */
      if (gfc_option.flag_realloc_lhs
	    && gfc_is_reallocatable_lhs (expr1)
	    && !gfc_expr_attr (expr1).codimension
	    && !gfc_is_coindexed (expr1))
	{
	  ompws_flags &= ~OMPWS_SCALARIZER_WS;
	  tmp = gfc_alloc_allocatable_for_assignment (&loop, expr1, expr2);
	  if (tmp != NULL_TREE)
	    gfc_add_expr_to_block (&loop.code[expr1->rank - 1], tmp);
	}

      /* Generate the copying loops.  */
      gfc_trans_scalarizing_loops (&loop, &body);

      /* Wrap the whole thing up.  */
      gfc_add_block_to_block (&block, &loop.pre);
      gfc_add_block_to_block (&block, &loop.post);

      gfc_cleanup_loop (&loop);
    }

  return gfc_finish_block (&block);
}


/* Check whether EXPR is a copyable array.  */

static bool
copyable_array_p (gfc_expr * expr)
{
  if (expr->expr_type != EXPR_VARIABLE)
    return false;

  /* First check it's an array.  */
  if (expr->rank < 1 || !expr->ref || expr->ref->next)
    return false;

  if (!gfc_full_array_ref_p (expr->ref, NULL))
    return false;

  /* Next check that it's of a simple enough type.  */
  switch (expr->ts.type)
    {
    case BT_INTEGER:
    case BT_REAL:
    case BT_COMPLEX:
    case BT_LOGICAL:
      return true;

    case BT_CHARACTER:
      return false;

    case BT_DERIVED:
      return !expr->ts.u.derived->attr.alloc_comp;

    default:
      break;
    }

  return false;
}

/* Translate an assignment.  */

tree
gfc_trans_assignment (gfc_expr * expr1, gfc_expr * expr2, bool init_flag,
		      bool dealloc)
{
  tree tmp;

  /* Special case a single function returning an array.  */
  if (expr2->expr_type == EXPR_FUNCTION && expr2->rank > 0)
    {
      tmp = gfc_trans_arrayfunc_assign (expr1, expr2);
      if (tmp)
	return tmp;
    }

  /* Special case assigning an array to zero.  */
  if (copyable_array_p (expr1)
      && is_zero_initializer_p (expr2))
    {
      tmp = gfc_trans_zero_assign (expr1);
      if (tmp)
        return tmp;
    }

  /* Special case copying one array to another.  */
  if (copyable_array_p (expr1)
      && copyable_array_p (expr2)
      && gfc_compare_types (&expr1->ts, &expr2->ts)
      && !gfc_check_dependency (expr1, expr2, 0))
    {
      tmp = gfc_trans_array_copy (expr1, expr2);
      if (tmp)
        return tmp;
    }

  /* Special case initializing an array from a constant array constructor.  */
  if (copyable_array_p (expr1)
      && expr2->expr_type == EXPR_ARRAY
      && gfc_compare_types (&expr1->ts, &expr2->ts))
    {
      tmp = gfc_trans_array_constructor_copy (expr1, expr2);
      if (tmp)
	return tmp;
    }

  /* Fallback to the scalarizer to generate explicit loops.  */
  return gfc_trans_assignment_1 (expr1, expr2, init_flag, dealloc);
}

tree
gfc_trans_init_assign (gfc_code * code)
{
  return gfc_trans_assignment (code->expr1, code->expr2, true, false);
}

tree
gfc_trans_assign (gfc_code * code)
{
  return gfc_trans_assignment (code->expr1, code->expr2, false, true);
}
