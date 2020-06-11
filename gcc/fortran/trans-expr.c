/* Expression translation
   Copyright (C) 2002-2020 Free Software Foundation, Inc.
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
#include "options.h"
#include "tree.h"
#include "gfortran.h"
#include "trans.h"
#include "stringpool.h"
#include "diagnostic-core.h"	/* For fatal_error.  */
#include "fold-const.h"
#include "langhooks.h"
#include "arith.h"
#include "constructor.h"
#include "trans-const.h"
#include "trans-types.h"
#include "trans-array.h"
/* Only for gfc_trans_assign and gfc_trans_pointer_assign.  */
#include "trans-stmt.h"
#include "dependency.h"
#include "gimplify.h"

/* Convert a scalar to an array descriptor. To be used for assumed-rank
   arrays.  */

static tree
get_scalar_to_descriptor_type (tree scalar, symbol_attribute attr)
{
  enum gfc_array_kind akind;

  if (attr.pointer)
    akind = GFC_ARRAY_POINTER_CONT;
  else if (attr.allocatable)
    akind = GFC_ARRAY_ALLOCATABLE;
  else
    akind = GFC_ARRAY_ASSUMED_SHAPE_CONT;

  if (POINTER_TYPE_P (TREE_TYPE (scalar)))
    scalar = TREE_TYPE (scalar);
  return gfc_get_array_type_bounds (TREE_TYPE (scalar), 0, 0, NULL, NULL, 1,
				    akind, !(attr.pointer || attr.target));
}

tree
gfc_conv_scalar_to_descriptor (gfc_se *se, tree scalar, symbol_attribute attr)
{
  tree desc, type, etype;

  type = get_scalar_to_descriptor_type (scalar, attr);
  etype = TREE_TYPE (scalar);
  desc = gfc_create_var (type, "desc");
  DECL_ARTIFICIAL (desc) = 1;

  if (CONSTANT_CLASS_P (scalar))
    {
      tree tmp;
      tmp = gfc_create_var (TREE_TYPE (scalar), "scalar");
      gfc_add_modify (&se->pre, tmp, scalar);
      scalar = tmp;
    }
  if (!POINTER_TYPE_P (TREE_TYPE (scalar)))
    scalar = gfc_build_addr_expr (NULL_TREE, scalar);
  else if (TREE_TYPE (etype) && TREE_CODE (TREE_TYPE (etype)) == ARRAY_TYPE)
    etype = TREE_TYPE (etype);
  gfc_add_modify (&se->pre, gfc_conv_descriptor_dtype (desc),
		  gfc_get_dtype_rank_type (0, etype));
  gfc_conv_descriptor_data_set (&se->pre, desc, scalar);

  /* Copy pointer address back - but only if it could have changed and
     if the actual argument is a pointer and not, e.g., NULL().  */
  if ((attr.pointer || attr.allocatable) && attr.intent != INTENT_IN)
    gfc_add_modify (&se->post, scalar,
		    fold_convert (TREE_TYPE (scalar),
				  gfc_conv_descriptor_data_get (desc)));
  return desc;
}


/* Get the coarray token from the ultimate array or component ref.
   Returns a NULL_TREE, when the ref object is not allocatable or pointer.  */

tree
gfc_get_ultimate_alloc_ptr_comps_caf_token (gfc_se *outerse, gfc_expr *expr)
{
  gfc_symbol *sym = expr->symtree->n.sym;
  bool is_coarray = sym->attr.codimension;
  gfc_expr *caf_expr = gfc_copy_expr (expr);
  gfc_ref *ref = caf_expr->ref, *last_caf_ref = NULL;

  while (ref)
    {
      if (ref->type == REF_COMPONENT
	  && (ref->u.c.component->attr.allocatable
	      || ref->u.c.component->attr.pointer)
	  && (is_coarray || ref->u.c.component->attr.codimension))
	  last_caf_ref = ref;
      ref = ref->next;
    }

  if (last_caf_ref == NULL)
    return NULL_TREE;

  tree comp = last_caf_ref->u.c.component->caf_token, caf;
  gfc_se se;
  bool comp_ref = !last_caf_ref->u.c.component->attr.dimension;
  if (comp == NULL_TREE && comp_ref)
    return NULL_TREE;
  gfc_init_se (&se, outerse);
  gfc_free_ref_list (last_caf_ref->next);
  last_caf_ref->next = NULL;
  caf_expr->rank = comp_ref ? 0 : last_caf_ref->u.c.component->as->rank;
  se.want_pointer = comp_ref;
  gfc_conv_expr (&se, caf_expr);
  gfc_add_block_to_block (&outerse->pre, &se.pre);

  if (TREE_CODE (se.expr) == COMPONENT_REF && comp_ref)
    se.expr = TREE_OPERAND (se.expr, 0);
  gfc_free_expr (caf_expr);

  if (comp_ref)
    caf = fold_build3_loc (input_location, COMPONENT_REF,
			   TREE_TYPE (comp), se.expr, comp, NULL_TREE);
  else
    caf = gfc_conv_descriptor_token (se.expr);
  return gfc_build_addr_expr (NULL_TREE, caf);
}


/* This is the seed for an eventual trans-class.c

   The following parameters should not be used directly since they might
   in future implementations.  Use the corresponding APIs.  */
#define CLASS_DATA_FIELD 0
#define CLASS_VPTR_FIELD 1
#define CLASS_LEN_FIELD 2
#define VTABLE_HASH_FIELD 0
#define VTABLE_SIZE_FIELD 1
#define VTABLE_EXTENDS_FIELD 2
#define VTABLE_DEF_INIT_FIELD 3
#define VTABLE_COPY_FIELD 4
#define VTABLE_FINAL_FIELD 5
#define VTABLE_DEALLOCATE_FIELD 6


tree
gfc_class_set_static_fields (tree decl, tree vptr, tree data)
{
  tree tmp;
  tree field;
  vec<constructor_elt, va_gc> *init = NULL;

  field = TYPE_FIELDS (TREE_TYPE (decl));
  tmp = gfc_advance_chain (field, CLASS_DATA_FIELD);
  CONSTRUCTOR_APPEND_ELT (init, tmp, data);

  tmp = gfc_advance_chain (field, CLASS_VPTR_FIELD);
  CONSTRUCTOR_APPEND_ELT (init, tmp, vptr);

  return build_constructor (TREE_TYPE (decl), init);
}


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
  /* For class arrays decl may be a temporary descriptor handle, the vptr is
     then available through the saved descriptor.  */
  if (VAR_P (decl) && DECL_LANG_SPECIFIC (decl)
      && GFC_DECL_SAVED_DESCRIPTOR (decl))
    decl = GFC_DECL_SAVED_DESCRIPTOR (decl);
  if (POINTER_TYPE_P (TREE_TYPE (decl)))
    decl = build_fold_indirect_ref_loc (input_location, decl);
  vptr = gfc_advance_chain (TYPE_FIELDS (TREE_TYPE (decl)),
			    CLASS_VPTR_FIELD);
  return fold_build3_loc (input_location, COMPONENT_REF,
			  TREE_TYPE (vptr), decl, vptr,
			  NULL_TREE);
}


tree
gfc_class_len_get (tree decl)
{
  tree len;
  /* For class arrays decl may be a temporary descriptor handle, the len is
     then available through the saved descriptor.  */
  if (VAR_P (decl) && DECL_LANG_SPECIFIC (decl)
      && GFC_DECL_SAVED_DESCRIPTOR (decl))
    decl = GFC_DECL_SAVED_DESCRIPTOR (decl);
  if (POINTER_TYPE_P (TREE_TYPE (decl)))
    decl = build_fold_indirect_ref_loc (input_location, decl);
  len = gfc_advance_chain (TYPE_FIELDS (TREE_TYPE (decl)),
			   CLASS_LEN_FIELD);
  return fold_build3_loc (input_location, COMPONENT_REF,
			  TREE_TYPE (len), decl, len,
			  NULL_TREE);
}


/* Try to get the _len component of a class.  When the class is not unlimited
   poly, i.e. no _len field exists, then return a zero node.  */

tree
gfc_class_len_or_zero_get (tree decl)
{
  tree len;
  /* For class arrays decl may be a temporary descriptor handle, the vptr is
     then available through the saved descriptor.  */
  if (VAR_P (decl) && DECL_LANG_SPECIFIC (decl)
      && GFC_DECL_SAVED_DESCRIPTOR (decl))
    decl = GFC_DECL_SAVED_DESCRIPTOR (decl);
  if (POINTER_TYPE_P (TREE_TYPE (decl)))
    decl = build_fold_indirect_ref_loc (input_location, decl);
  len = gfc_advance_chain (TYPE_FIELDS (TREE_TYPE (decl)),
			   CLASS_LEN_FIELD);
  return len != NULL_TREE ? fold_build3_loc (input_location, COMPONENT_REF,
					     TREE_TYPE (len), decl, len,
					     NULL_TREE)
    : build_zero_cst (gfc_charlen_type_node);
}


/* Get the specified FIELD from the VPTR.  */

static tree
vptr_field_get (tree vptr, int fieldno)
{
  tree field;
  vptr = build_fold_indirect_ref_loc (input_location, vptr);
  field = gfc_advance_chain (TYPE_FIELDS (TREE_TYPE (vptr)),
			     fieldno);
  field = fold_build3_loc (input_location, COMPONENT_REF,
			   TREE_TYPE (field), vptr, field,
			   NULL_TREE);
  gcc_assert (field);
  return field;
}


/* Get the field from the class' vptr.  */

static tree
class_vtab_field_get (tree decl, int fieldno)
{
  tree vptr;
  vptr = gfc_class_vptr_get (decl);
  return vptr_field_get (vptr, fieldno);
}


/* Define a macro for creating the class_vtab_* and vptr_* accessors in
   unison.  */
#define VTAB_GET_FIELD_GEN(name, field) tree \
gfc_class_vtab_## name ##_get (tree cl) \
{ \
  return class_vtab_field_get (cl, field); \
} \
 \
tree \
gfc_vptr_## name ##_get (tree vptr) \
{ \
  return vptr_field_get (vptr, field); \
}

VTAB_GET_FIELD_GEN (hash, VTABLE_HASH_FIELD)
VTAB_GET_FIELD_GEN (extends, VTABLE_EXTENDS_FIELD)
VTAB_GET_FIELD_GEN (def_init, VTABLE_DEF_INIT_FIELD)
VTAB_GET_FIELD_GEN (copy, VTABLE_COPY_FIELD)
VTAB_GET_FIELD_GEN (final, VTABLE_FINAL_FIELD)
VTAB_GET_FIELD_GEN (deallocate, VTABLE_DEALLOCATE_FIELD)


/* The size field is returned as an array index type.  Therefore treat
   it and only it specially.  */

tree
gfc_class_vtab_size_get (tree cl)
{
  tree size;
  size = class_vtab_field_get (cl, VTABLE_SIZE_FIELD);
  /* Always return size as an array index type.  */
  size = fold_convert (gfc_array_index_type, size);
  gcc_assert (size);
  return size;
}

tree
gfc_vptr_size_get (tree vptr)
{
  tree size;
  size = vptr_field_get (vptr, VTABLE_SIZE_FIELD);
  /* Always return size as an array index type.  */
  size = fold_convert (gfc_array_index_type, size);
  gcc_assert (size);
  return size;
}


#undef CLASS_DATA_FIELD
#undef CLASS_VPTR_FIELD
#undef CLASS_LEN_FIELD
#undef VTABLE_HASH_FIELD
#undef VTABLE_SIZE_FIELD
#undef VTABLE_EXTENDS_FIELD
#undef VTABLE_DEF_INIT_FIELD
#undef VTABLE_COPY_FIELD
#undef VTABLE_FINAL_FIELD


/* Search for the last _class ref in the chain of references of this
   expression and cut the chain there.  Albeit this routine is similiar
   to class.c::gfc_add_component_ref (), is there a significant
   difference: gfc_add_component_ref () concentrates on an array ref to
   be the last ref in the chain.  This routine is oblivious to the kind
   of refs following.  */

gfc_expr *
gfc_find_and_cut_at_last_class_ref (gfc_expr *e, bool is_mold)
{
  gfc_expr *base_expr;
  gfc_ref *ref, *class_ref, *tail = NULL, *array_ref;

  /* Find the last class reference.  */
  class_ref = NULL;
  array_ref = NULL;
  for (ref = e->ref; ref; ref = ref->next)
    {
      if (ref->type == REF_ARRAY && ref->u.ar.type != AR_ELEMENT)
	array_ref = ref;

      if (ref->type == REF_COMPONENT
	  && ref->u.c.component->ts.type == BT_CLASS)
	{
	  /* Component to the right of a part reference with nonzero rank
	     must not have the ALLOCATABLE attribute.  If attempts are
	     made to reference such a component reference, an error results
	     followed by an ICE.  */
	  if (array_ref && CLASS_DATA (ref->u.c.component)->attr.allocatable)
	    return NULL;
	  class_ref = ref;
	}

      if (ref->next == NULL)
	break;
    }

  /* Remove and store all subsequent references after the
     CLASS reference.  */
  if (class_ref)
    {
      tail = class_ref->next;
      class_ref->next = NULL;
    }
  else if (e->symtree && e->symtree->n.sym->ts.type == BT_CLASS)
    {
      tail = e->ref;
      e->ref = NULL;
    }

  if (is_mold)
    base_expr = gfc_expr_to_initialize (e);
  else
    base_expr = gfc_copy_expr (e);

  /* Restore the original tail expression.  */
  if (class_ref)
    {
      gfc_free_ref_list (class_ref->next);
      class_ref->next = tail;
    }
  else if (e->symtree && e->symtree->n.sym->ts.type == BT_CLASS)
    {
      gfc_free_ref_list (e->ref);
      e->ref = tail;
    }
  return base_expr;
}


/* Reset the vptr to the declared type, e.g. after deallocation.  */

void
gfc_reset_vptr (stmtblock_t *block, gfc_expr *e)
{
  gfc_symbol *vtab;
  tree vptr;
  tree vtable;
  gfc_se se;

  /* Evaluate the expression and obtain the vptr from it.  */
  gfc_init_se (&se, NULL);
  if (e->rank)
    gfc_conv_expr_descriptor (&se, e);
  else
    gfc_conv_expr (&se, e);
  gfc_add_block_to_block (block, &se.pre);
  vptr = gfc_get_vptr_from_expr (se.expr);

  /* If a vptr is not found, we can do nothing more.  */
  if (vptr == NULL_TREE)
    return;

  if (UNLIMITED_POLY (e))
    gfc_add_modify (block, vptr, build_int_cst (TREE_TYPE (vptr), 0));
  else
    {
      /* Return the vptr to the address of the declared type.  */
      vtab = gfc_find_derived_vtab (e->ts.u.derived);
      vtable = vtab->backend_decl;
      if (vtable == NULL_TREE)
	vtable = gfc_get_symbol_decl (vtab);
      vtable = gfc_build_addr_expr (NULL, vtable);
      vtable = fold_convert (TREE_TYPE (vptr), vtable);
      gfc_add_modify (block, vptr, vtable);
    }
}


/* Reset the len for unlimited polymorphic objects.  */

void
gfc_reset_len (stmtblock_t *block, gfc_expr *expr)
{
  gfc_expr *e;
  gfc_se se_len;
  e = gfc_find_and_cut_at_last_class_ref (expr);
  if (e == NULL)
    return;
  gfc_add_len_component (e);
  gfc_init_se (&se_len, NULL);
  gfc_conv_expr (&se_len, e);
  gfc_add_modify (block, se_len.expr,
		  fold_convert (TREE_TYPE (se_len.expr), integer_zero_node));
  gfc_free_expr (e);
}


/* Obtain the last class reference in an expression.
   Return NULL_TREE if no class reference is found.  */

tree
gfc_get_class_from_expr (tree expr)
{
  tree tmp;
  tree type;

  for (tmp = expr; tmp; tmp = TREE_OPERAND (tmp, 0))
    {
      type = TREE_TYPE (tmp);
      while (type)
	{
	  if (GFC_CLASS_TYPE_P (type))
	    return tmp;
	  if (type != TYPE_CANONICAL (type))
	    type = TYPE_CANONICAL (type);
	  else
	    type = NULL_TREE;
	}
      if (VAR_P (tmp) || TREE_CODE (tmp) == PARM_DECL)
	break;
    }

  if (POINTER_TYPE_P (TREE_TYPE (tmp)))
    tmp = build_fold_indirect_ref_loc (input_location, tmp);

  if (GFC_CLASS_TYPE_P (TREE_TYPE (tmp)))
    return tmp;

  return NULL_TREE;
}


/* Obtain the vptr of the last class reference in an expression.
   Return NULL_TREE if no class reference is found.  */

tree
gfc_get_vptr_from_expr (tree expr)
{
  tree tmp;

  tmp = gfc_get_class_from_expr (expr);

  if (tmp != NULL_TREE)
    return gfc_class_vptr_get (tmp);

  return NULL_TREE;
}


static void
class_array_data_assign (stmtblock_t *block, tree lhs_desc, tree rhs_desc,
			 bool lhs_type)
{
  tree tmp, tmp2, type;

  gfc_conv_descriptor_data_set (block, lhs_desc,
				gfc_conv_descriptor_data_get (rhs_desc));
  gfc_conv_descriptor_offset_set (block, lhs_desc,
				  gfc_conv_descriptor_offset_get (rhs_desc));

  gfc_add_modify (block, gfc_conv_descriptor_dtype (lhs_desc),
		  gfc_conv_descriptor_dtype (rhs_desc));

  /* Assign the dimension as range-ref.  */
  tmp = gfc_get_descriptor_dimension (lhs_desc);
  tmp2 = gfc_get_descriptor_dimension (rhs_desc);

  type = lhs_type ? TREE_TYPE (tmp) : TREE_TYPE (tmp2);
  tmp = build4_loc (input_location, ARRAY_RANGE_REF, type, tmp,
		    gfc_index_zero_node, NULL_TREE, NULL_TREE);
  tmp2 = build4_loc (input_location, ARRAY_RANGE_REF, type, tmp2,
		     gfc_index_zero_node, NULL_TREE, NULL_TREE);
  gfc_add_modify (block, tmp, tmp2);
}


/* Takes a derived type expression and returns the address of a temporary
   class object of the 'declared' type.  If vptr is not NULL, this is
   used for the temporary class object.
   optional_alloc_ptr is false when the dummy is neither allocatable
   nor a pointer; that's only relevant for the optional handling.  */
void
gfc_conv_derived_to_class (gfc_se *parmse, gfc_expr *e,
			   gfc_typespec class_ts, tree vptr, bool optional,
			   bool optional_alloc_ptr)
{
  gfc_symbol *vtab;
  tree cond_optional = NULL_TREE;
  gfc_ss *ss;
  tree ctree;
  tree var;
  tree tmp;
  int dim;

  /* The derived type needs to be converted to a temporary
     CLASS object.  */
  tmp = gfc_typenode_for_spec (&class_ts);
  var = gfc_create_var (tmp, "class");

  /* Set the vptr.  */
  ctree =  gfc_class_vptr_get (var);

  if (vptr != NULL_TREE)
    {
      /* Use the dynamic vptr.  */
      tmp = vptr;
    }
  else
    {
      /* In this case the vtab corresponds to the derived type and the
	 vptr must point to it.  */
      vtab = gfc_find_derived_vtab (e->ts.u.derived);
      gcc_assert (vtab);
      tmp = gfc_build_addr_expr (NULL_TREE, gfc_get_symbol_decl (vtab));
    }
  gfc_add_modify (&parmse->pre, ctree,
		  fold_convert (TREE_TYPE (ctree), tmp));

  /* Now set the data field.  */
  ctree =  gfc_class_data_get (var);

  if (optional)
    cond_optional = gfc_conv_expr_present (e->symtree->n.sym);

  if (parmse->expr && POINTER_TYPE_P (TREE_TYPE (parmse->expr)))
    {
      /* If there is a ready made pointer to a derived type, use it
	 rather than evaluating the expression again.  */
      tmp = fold_convert (TREE_TYPE (ctree), parmse->expr);
      gfc_add_modify (&parmse->pre, ctree, tmp);
    }
  else if (parmse->ss && parmse->ss->info && parmse->ss->info->useflags)
    {
      /* For an array reference in an elemental procedure call we need
	 to retain the ss to provide the scalarized array reference.  */
      gfc_conv_expr_reference (parmse, e);
      tmp = fold_convert (TREE_TYPE (ctree), parmse->expr);
      if (optional)
	tmp = build3_loc (input_location, COND_EXPR, TREE_TYPE (tmp),
			  cond_optional, tmp,
			  fold_convert (TREE_TYPE (tmp), null_pointer_node));
      gfc_add_modify (&parmse->pre, ctree, tmp);
    }
  else
    {
      ss = gfc_walk_expr (e);
      if (ss == gfc_ss_terminator)
	{
	  parmse->ss = NULL;
	  gfc_conv_expr_reference (parmse, e);

	  /* Scalar to an assumed-rank array.  */
	  if (class_ts.u.derived->components->as)
	    {
	      tree type;
	      type = get_scalar_to_descriptor_type (parmse->expr,
						    gfc_expr_attr (e));
	      gfc_add_modify (&parmse->pre, gfc_conv_descriptor_dtype (ctree),
			      gfc_get_dtype (type));
	      if (optional)
		parmse->expr = build3_loc (input_location, COND_EXPR,
					   TREE_TYPE (parmse->expr),
					   cond_optional, parmse->expr,
					   fold_convert (TREE_TYPE (parmse->expr),
							 null_pointer_node));
	      gfc_conv_descriptor_data_set (&parmse->pre, ctree, parmse->expr);
	    }
          else
	    {
	      tmp = fold_convert (TREE_TYPE (ctree), parmse->expr);
	      if (optional)
		tmp = build3_loc (input_location, COND_EXPR, TREE_TYPE (tmp),
				  cond_optional, tmp,
				  fold_convert (TREE_TYPE (tmp),
						null_pointer_node));
	      gfc_add_modify (&parmse->pre, ctree, tmp);
	    }
	}
      else
	{
	  stmtblock_t block;
	  gfc_init_block (&block);
	  gfc_ref *ref;

	  parmse->ss = ss;
	  parmse->use_offset = 1;
	  gfc_conv_expr_descriptor (parmse, e);

	  /* Detect any array references with vector subscripts.  */
	  for (ref = e->ref; ref; ref = ref->next)
	    if (ref->type == REF_ARRAY
		&& ref->u.ar.type != AR_ELEMENT
		&& ref->u.ar.type != AR_FULL)
	      {
		for (dim = 0; dim < ref->u.ar.dimen; dim++)
		  if (ref->u.ar.dimen_type[dim] == DIMEN_VECTOR)
		    break;
		if (dim < ref->u.ar.dimen)
		  break;
	      }

	  /* Array references with vector subscripts and non-variable expressions
	     need be converted to a one-based descriptor.  */
	  if (ref || e->expr_type != EXPR_VARIABLE)
	    {
	      for (dim = 0; dim < e->rank; ++dim)
		gfc_conv_shift_descriptor_lbound (&block, parmse->expr, dim,
						  gfc_index_one_node);
	    }

	  if (e->rank != class_ts.u.derived->components->as->rank)
	    {
	      gcc_assert (class_ts.u.derived->components->as->type
			  == AS_ASSUMED_RANK);
	      class_array_data_assign (&block, ctree, parmse->expr, false);
	    }
	  else
	    {
	      if (gfc_expr_attr (e).codimension)
		parmse->expr = fold_build1_loc (input_location,
						VIEW_CONVERT_EXPR,
						TREE_TYPE (ctree),
						parmse->expr);
	      gfc_add_modify (&block, ctree, parmse->expr);
	    }

	  if (optional)
	    {
	      tmp = gfc_finish_block (&block);

	      gfc_init_block (&block);
	      gfc_conv_descriptor_data_set (&block, ctree, null_pointer_node);

	      tmp = build3_v (COND_EXPR, cond_optional, tmp,
			      gfc_finish_block (&block));
	      gfc_add_expr_to_block (&parmse->pre, tmp);
	    }
	  else
	    gfc_add_block_to_block (&parmse->pre, &block);
	}
    }

  if (class_ts.u.derived->components->ts.type == BT_DERIVED
      && class_ts.u.derived->components->ts.u.derived
		 ->attr.unlimited_polymorphic)
    {
      /* Take care about initializing the _len component correctly.  */
      ctree = gfc_class_len_get (var);
      if (UNLIMITED_POLY (e))
	{
	  gfc_expr *len;
	  gfc_se se;

	  len = gfc_copy_expr (e);
	  gfc_add_len_component (len);
	  gfc_init_se (&se, NULL);
	  gfc_conv_expr (&se, len);
	  if (optional)
	    tmp = build3_loc (input_location, COND_EXPR, TREE_TYPE (se.expr),
			      cond_optional, se.expr,
			      fold_convert (TREE_TYPE (se.expr),
					    integer_zero_node));
	  else
	    tmp = se.expr;
	}
      else
	tmp = integer_zero_node;
      gfc_add_modify (&parmse->pre, ctree, fold_convert (TREE_TYPE (ctree),
							  tmp));
    }
  /* Pass the address of the class object.  */
  parmse->expr = gfc_build_addr_expr (NULL_TREE, var);

  if (optional && optional_alloc_ptr)
    parmse->expr = build3_loc (input_location, COND_EXPR,
			       TREE_TYPE (parmse->expr),
			       cond_optional, parmse->expr,
			       fold_convert (TREE_TYPE (parmse->expr),
					     null_pointer_node));
}


/* Create a new class container, which is required as scalar coarrays
   have an array descriptor while normal scalars haven't. Optionally,
   NULL pointer checks are added if the argument is OPTIONAL.  */

static void
class_scalar_coarray_to_class (gfc_se *parmse, gfc_expr *e,
			       gfc_typespec class_ts, bool optional)
{
  tree var, ctree, tmp;
  stmtblock_t block;
  gfc_ref *ref;
  gfc_ref *class_ref;

  gfc_init_block (&block);

  class_ref = NULL;
  for (ref = e->ref; ref; ref = ref->next)
    {
      if (ref->type == REF_COMPONENT
	    && ref->u.c.component->ts.type == BT_CLASS)
	class_ref = ref;
    }

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

  var = gfc_typenode_for_spec (&class_ts);
  var = gfc_create_var (var, "class");

  ctree = gfc_class_vptr_get (var);
  gfc_add_modify (&block, ctree,
		  fold_convert (TREE_TYPE (ctree), gfc_class_vptr_get (tmp)));

  ctree = gfc_class_data_get (var);
  tmp = gfc_conv_descriptor_data_get (gfc_class_data_get (tmp));
  gfc_add_modify (&block, ctree, fold_convert (TREE_TYPE (ctree), tmp));

  /* Pass the address of the class object.  */
  parmse->expr = gfc_build_addr_expr (NULL_TREE, var);

  if (optional)
    {
      tree cond = gfc_conv_expr_present (e->symtree->n.sym);
      tree tmp2;

      tmp = gfc_finish_block (&block);

      gfc_init_block (&block);
      tmp2 = gfc_class_data_get (var);
      gfc_add_modify (&block, tmp2, fold_convert (TREE_TYPE (tmp2),
						  null_pointer_node));
      tmp2 = gfc_finish_block (&block);

      tmp = build3_loc (input_location, COND_EXPR, void_type_node,
			cond, tmp, tmp2);
      gfc_add_expr_to_block (&parmse->pre, tmp);
    }
  else
    gfc_add_block_to_block (&parmse->pre, &block);
}


/* Takes an intrinsic type expression and returns the address of a temporary
   class object of the 'declared' type.  */
void
gfc_conv_intrinsic_to_class (gfc_se *parmse, gfc_expr *e,
			     gfc_typespec class_ts)
{
  gfc_symbol *vtab;
  gfc_ss *ss;
  tree ctree;
  tree var;
  tree tmp;
  int dim;

  /* The intrinsic type needs to be converted to a temporary
     CLASS object.  */
  tmp = gfc_typenode_for_spec (&class_ts);
  var = gfc_create_var (tmp, "class");

  /* Set the vptr.  */
  ctree = gfc_class_vptr_get (var);

  vtab = gfc_find_vtab (&e->ts);
  gcc_assert (vtab);
  tmp = gfc_build_addr_expr (NULL_TREE, gfc_get_symbol_decl (vtab));
  gfc_add_modify (&parmse->pre, ctree,
		  fold_convert (TREE_TYPE (ctree), tmp));

  /* Now set the data field.  */
  ctree = gfc_class_data_get (var);
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
	  if (class_ts.u.derived->components->as
	      && class_ts.u.derived->components->as->type == AS_ASSUMED_RANK)
	    {
	      tmp = gfc_conv_scalar_to_descriptor (parmse, parmse->expr,
						   gfc_expr_attr (e));
	      tmp = fold_build1_loc (input_location, VIEW_CONVERT_EXPR,
				     TREE_TYPE (ctree), tmp);
	    }
	  else
	      tmp = fold_convert (TREE_TYPE (ctree), parmse->expr);
	  gfc_add_modify (&parmse->pre, ctree, tmp);
	}
      else
	{
	  parmse->ss = ss;
	  parmse->use_offset = 1;
	  gfc_conv_expr_descriptor (parmse, e);

	  /* Array references with vector subscripts and non-variable expressions
	     need be converted to a one-based descriptor.  */
	  if (e->expr_type != EXPR_VARIABLE)
	    {
	      for (dim = 0; dim < e->rank; ++dim)
		gfc_conv_shift_descriptor_lbound (&parmse->pre, parmse->expr,
						  dim, gfc_index_one_node);
	    }

	  if (class_ts.u.derived->components->as->rank != e->rank)
	    {
	      tmp = fold_build1_loc (input_location, VIEW_CONVERT_EXPR,
				     TREE_TYPE (ctree), parmse->expr);
	      gfc_add_modify (&parmse->pre, ctree, tmp);
	    }
	  else
	    gfc_add_modify (&parmse->pre, ctree, parmse->expr);
	}
    }

  gcc_assert (class_ts.type == BT_CLASS);
  if (class_ts.u.derived->components->ts.type == BT_DERIVED
      && class_ts.u.derived->components->ts.u.derived
		 ->attr.unlimited_polymorphic)
    {
      ctree = gfc_class_len_get (var);
      /* When the actual arg is a char array, then set the _len component of the
	 unlimited polymorphic entity to the length of the string.  */
      if (e->ts.type == BT_CHARACTER)
	{
	  /* Start with parmse->string_length because this seems to be set to a
	   correct value more often.  */
	  if (parmse->string_length)
	    tmp = parmse->string_length;
	  /* When the string_length is not yet set, then try the backend_decl of
	   the cl.  */
	  else if (e->ts.u.cl->backend_decl)
	    tmp = e->ts.u.cl->backend_decl;
	  /* If both of the above approaches fail, then try to generate an
	   expression from the input, which is only feasible currently, when the
	   expression can be evaluated to a constant one.  */
	  else
	    {
	      /* Try to simplify the expression.  */
	      gfc_simplify_expr (e, 0);
	      if (e->expr_type == EXPR_CONSTANT && !e->ts.u.cl->resolved)
		{
		  /* Amazingly all data is present to compute the length of a
		   constant string, but the expression is not yet there.  */
		  e->ts.u.cl->length = gfc_get_constant_expr (BT_INTEGER,
							      gfc_charlen_int_kind,
							      &e->where);
		  mpz_set_ui (e->ts.u.cl->length->value.integer,
			      e->value.character.length);
		  gfc_conv_const_charlen (e->ts.u.cl);
		  e->ts.u.cl->resolved = 1;
		  tmp = e->ts.u.cl->backend_decl;
		}
	      else
		{
		  gfc_error ("Cannot compute the length of the char array "
			     "at %L.", &e->where);
		}
	    }
	}
      else
	tmp = integer_zero_node;

      gfc_add_modify (&parmse->pre, ctree, fold_convert (TREE_TYPE (ctree), tmp));
    }
  else if (class_ts.type == BT_CLASS
	   && class_ts.u.derived->components
	   && class_ts.u.derived->components->ts.u
		.derived->attr.unlimited_polymorphic)
    {
      ctree = gfc_class_len_get (var);
      gfc_add_modify (&parmse->pre, ctree,
		      fold_convert (TREE_TYPE (ctree),
				    integer_zero_node));
    }
  /* Pass the address of the class object.  */
  parmse->expr = gfc_build_addr_expr (NULL_TREE, var);
}


/* Takes a scalarized class array expression and returns the
   address of a temporary scalar class object of the 'declared'
   type.
   OOP-TODO: This could be improved by adding code that branched on
   the dynamic type being the same as the declared type. In this case
   the original class expression can be passed directly.
   optional_alloc_ptr is false when the dummy is neither allocatable
   nor a pointer; that's relevant for the optional handling.
   Set copyback to true if class container's _data and _vtab pointers
   might get modified.  */

void
gfc_conv_class_to_class (gfc_se *parmse, gfc_expr *e, gfc_typespec class_ts,
			 bool elemental, bool copyback, bool optional,
		         bool optional_alloc_ptr)
{
  tree ctree;
  tree var;
  tree tmp;
  tree vptr;
  tree cond = NULL_TREE;
  tree slen = NULL_TREE;
  gfc_ref *ref;
  gfc_ref *class_ref;
  stmtblock_t block;
  bool full_array = false;

  gfc_init_block (&block);

  class_ref = NULL;
  for (ref = e->ref; ref; ref = ref->next)
    {
      if (ref->type == REF_COMPONENT
	    && ref->u.c.component->ts.type == BT_CLASS)
	class_ref = ref;

      if (ref->next == NULL)
	break;
    }

  if ((ref == NULL || class_ref == ref)
      && !(gfc_is_class_array_function (e) && parmse->class_vptr != NULL_TREE)
      && (!class_ts.u.derived->components->as
	  || class_ts.u.derived->components->as->rank != -1))
    return;

  /* Test for FULL_ARRAY.  */
  if (e->rank == 0 && gfc_expr_attr (e).codimension
      && gfc_expr_attr (e).dimension)
    full_array = true;
  else
    gfc_is_class_array_ref (e, &full_array);

  /* The derived type needs to be converted to a temporary
     CLASS object.  */
  tmp = gfc_typenode_for_spec (&class_ts);
  var = gfc_create_var (tmp, "class");

  /* Set the data.  */
  ctree = gfc_class_data_get (var);
  if (class_ts.u.derived->components->as
      && e->rank != class_ts.u.derived->components->as->rank)
    {
      if (e->rank == 0)
	{
	  tree type = get_scalar_to_descriptor_type (parmse->expr,
						     gfc_expr_attr (e));
	  gfc_add_modify (&block, gfc_conv_descriptor_dtype (ctree),
			  gfc_get_dtype (type));

	  tmp = gfc_class_data_get (parmse->expr);
	  if (!POINTER_TYPE_P (TREE_TYPE (tmp)))
	    tmp = gfc_build_addr_expr (NULL_TREE, tmp);

	  gfc_conv_descriptor_data_set (&block, ctree, tmp);
	}
      else
	class_array_data_assign (&block, ctree, parmse->expr, false);
    }
  else
    {
      if (TREE_TYPE (parmse->expr) != TREE_TYPE (ctree))
	parmse->expr = fold_build1_loc (input_location, VIEW_CONVERT_EXPR,
					TREE_TYPE (ctree), parmse->expr);
      gfc_add_modify (&block, ctree, parmse->expr);
    }

  /* Return the data component, except in the case of scalarized array
     references, where nullification of the cannot occur and so there
     is no need.  */
  if (!elemental && full_array && copyback)
    {
      if (class_ts.u.derived->components->as
	  && e->rank != class_ts.u.derived->components->as->rank)
	{
	  if (e->rank == 0)
	    gfc_add_modify (&parmse->post, gfc_class_data_get (parmse->expr),
			    gfc_conv_descriptor_data_get (ctree));
	  else
	    class_array_data_assign (&parmse->post, parmse->expr, ctree, true);
	}
      else
	gfc_add_modify (&parmse->post, parmse->expr, ctree);
    }

  /* Set the vptr.  */
  ctree = gfc_class_vptr_get (var);

  /* The vptr is the second field of the actual argument.
     First we have to find the corresponding class reference.  */

  tmp = NULL_TREE;
  if (gfc_is_class_array_function (e)
      && parmse->class_vptr != NULL_TREE)
    tmp = parmse->class_vptr;
  else if (class_ref == NULL
	   && e->symtree && e->symtree->n.sym->ts.type == BT_CLASS)
    {
      tmp = e->symtree->n.sym->backend_decl;

      if (TREE_CODE (tmp) == FUNCTION_DECL)
	tmp = gfc_get_fake_result_decl (e->symtree->n.sym, 0);

      if (DECL_LANG_SPECIFIC (tmp) && GFC_DECL_SAVED_DESCRIPTOR (tmp))
	tmp = GFC_DECL_SAVED_DESCRIPTOR (tmp);

      slen = build_zero_cst (size_type_node);
    }
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
      slen = tmpse.string_length;
    }

  gcc_assert (tmp != NULL_TREE);

  /* Dereference if needs be.  */
  if (TREE_CODE (TREE_TYPE (tmp)) == REFERENCE_TYPE)
    tmp = build_fold_indirect_ref_loc (input_location, tmp);

  if (!(gfc_is_class_array_function (e) && parmse->class_vptr))
    vptr = gfc_class_vptr_get (tmp);
  else
    vptr = tmp;

  gfc_add_modify (&block, ctree,
		  fold_convert (TREE_TYPE (ctree), vptr));

  /* Return the vptr component, except in the case of scalarized array
     references, where the dynamic type cannot change.  */
  if (!elemental && full_array && copyback)
    gfc_add_modify (&parmse->post, vptr,
		    fold_convert (TREE_TYPE (vptr), ctree));

  /* For unlimited polymorphic objects also set the _len component.  */
  if (class_ts.type == BT_CLASS
      && class_ts.u.derived->components
      && class_ts.u.derived->components->ts.u
		      .derived->attr.unlimited_polymorphic)
    {
      ctree = gfc_class_len_get (var);
      if (UNLIMITED_POLY (e))
	tmp = gfc_class_len_get (tmp);
      else if (e->ts.type == BT_CHARACTER)
	{
	  gcc_assert (slen != NULL_TREE);
	  tmp = slen;
	}
      else
	tmp = build_zero_cst (size_type_node);
      gfc_add_modify (&parmse->pre, ctree,
		      fold_convert (TREE_TYPE (ctree), tmp));

      /* Return the len component, except in the case of scalarized array
	references, where the dynamic type cannot change.  */
      if (!elemental && full_array && copyback
	  && (UNLIMITED_POLY (e) || VAR_P (tmp)))
	  gfc_add_modify (&parmse->post, tmp,
			  fold_convert (TREE_TYPE (tmp), ctree));
    }

  if (optional)
    {
      tree tmp2;

      cond = gfc_conv_expr_present (e->symtree->n.sym);
      /* parmse->pre may contain some preparatory instructions for the
 	 temporary array descriptor.  Those may only be executed when the
	 optional argument is set, therefore add parmse->pre's instructions
	 to block, which is later guarded by an if (optional_arg_given).  */
      gfc_add_block_to_block (&parmse->pre, &block);
      block.head = parmse->pre.head;
      parmse->pre.head = NULL_TREE;
      tmp = gfc_finish_block (&block);

      if (optional_alloc_ptr)
	tmp2 = build_empty_stmt (input_location);
      else
	{
	  gfc_init_block (&block);

	  tmp2 = gfc_conv_descriptor_data_get (gfc_class_data_get (var));
	  gfc_add_modify (&block, tmp2, fold_convert (TREE_TYPE (tmp2),
						      null_pointer_node));
	  tmp2 = gfc_finish_block (&block);
	}

      tmp = build3_loc (input_location, COND_EXPR, void_type_node,
			cond, tmp, tmp2);
      gfc_add_expr_to_block (&parmse->pre, tmp);
    }
  else
    gfc_add_block_to_block (&parmse->pre, &block);

  /* Pass the address of the class object.  */
  parmse->expr = gfc_build_addr_expr (NULL_TREE, var);

  if (optional && optional_alloc_ptr)
    parmse->expr = build3_loc (input_location, COND_EXPR,
			       TREE_TYPE (parmse->expr),
			       cond, parmse->expr,
			       fold_convert (TREE_TYPE (parmse->expr),
					     null_pointer_node));
}


/* Given a class array declaration and an index, returns the address
   of the referenced element.  */

tree
gfc_get_class_array_ref (tree index, tree class_decl, tree data_comp,
			 bool unlimited)
{
  tree data, size, tmp, ctmp, offset, ptr;

  data = data_comp != NULL_TREE ? data_comp :
				  gfc_class_data_get (class_decl);
  size = gfc_class_vtab_size_get (class_decl);

  if (unlimited)
    {
      tmp = fold_convert (gfc_array_index_type,
			  gfc_class_len_get (class_decl));
      ctmp = fold_build2_loc (input_location, MULT_EXPR,
			      gfc_array_index_type, size, tmp);
      tmp = fold_build2_loc (input_location, GT_EXPR,
			     logical_type_node, tmp,
			     build_zero_cst (TREE_TYPE (tmp)));
      size = fold_build3_loc (input_location, COND_EXPR,
			      gfc_array_index_type, tmp, ctmp, size);
    }

  offset = fold_build2_loc (input_location, MULT_EXPR,
			    gfc_array_index_type,
			    index, size);

  data = gfc_conv_descriptor_data_get (data);
  ptr = fold_convert (pvoid_type_node, data);
  ptr = fold_build_pointer_plus_loc (input_location, ptr, offset);
  return fold_convert (TREE_TYPE (data), ptr);
}


/* Copies one class expression to another, assuming that if either
   'to' or 'from' are arrays they are packed.  Should 'from' be
   NULL_TREE, the initialization expression for 'to' is used, assuming
   that the _vptr is set.  */

tree
gfc_copy_class_to_class (tree from, tree to, tree nelems, bool unlimited)
{
  tree fcn;
  tree fcn_type;
  tree from_data;
  tree from_len;
  tree to_data;
  tree to_len;
  tree to_ref;
  tree from_ref;
  vec<tree, va_gc> *args;
  tree tmp;
  tree stdcopy;
  tree extcopy;
  tree index;
  bool is_from_desc = false, is_to_class = false;

  args = NULL;
  /* To prevent warnings on uninitialized variables.  */
  from_len = to_len = NULL_TREE;

  if (from != NULL_TREE)
    fcn = gfc_class_vtab_copy_get (from);
  else
    fcn = gfc_class_vtab_copy_get (to);

  fcn_type = TREE_TYPE (TREE_TYPE (fcn));

  if (from != NULL_TREE)
    {
      is_from_desc = GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (from));
      if (is_from_desc)
	{
	  from_data = from;
	  from = GFC_DECL_SAVED_DESCRIPTOR (from);
	}
      else
	{
	  /* Check that from is a class.  When the class is part of a coarray,
	     then from is a common pointer and is to be used as is.  */
	  tmp = POINTER_TYPE_P (TREE_TYPE (from))
	      ? build_fold_indirect_ref (from) : from;
	  from_data =
	      (GFC_CLASS_TYPE_P (TREE_TYPE (tmp))
	       || (DECL_P (tmp) && GFC_DECL_CLASS (tmp)))
	      ? gfc_class_data_get (from) : from;
	  is_from_desc = GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (from_data));
	}
     }
  else
    from_data = gfc_class_vtab_def_init_get (to);

  if (unlimited)
    {
      if (from != NULL_TREE && unlimited)
	from_len = gfc_class_len_or_zero_get (from);
      else
	from_len = build_zero_cst (size_type_node);
    }

  if (GFC_CLASS_TYPE_P (TREE_TYPE (to)))
    {
      is_to_class = true;
      to_data = gfc_class_data_get (to);
      if (unlimited)
	to_len = gfc_class_len_get (to);
    }
  else
    /* When to is a BT_DERIVED and not a BT_CLASS, then to_data == to.  */
    to_data = to;

  if (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (to_data)))
    {
      stmtblock_t loopbody;
      stmtblock_t body;
      stmtblock_t ifbody;
      gfc_loopinfo loop;
      tree orig_nelems = nelems; /* Needed for bounds check.  */

      gfc_init_block (&body);
      tmp = fold_build2_loc (input_location, MINUS_EXPR,
			     gfc_array_index_type, nelems,
			     gfc_index_one_node);
      nelems = gfc_evaluate_now (tmp, &body);
      index = gfc_create_var (gfc_array_index_type, "S");

      if (is_from_desc)
	{
	  from_ref = gfc_get_class_array_ref (index, from, from_data,
					      unlimited);
	  vec_safe_push (args, from_ref);
	}
      else
        vec_safe_push (args, from_data);

      if (is_to_class)
	to_ref = gfc_get_class_array_ref (index, to, to_data, unlimited);
      else
	{
	  tmp = gfc_conv_array_data (to);
	  tmp = build_fold_indirect_ref_loc (input_location, tmp);
	  to_ref = gfc_build_addr_expr (NULL_TREE,
					gfc_build_array_ref (tmp, index, to));
	}
      vec_safe_push (args, to_ref);

      /* Add bounds check.  */
      if ((gfc_option.rtcheck & GFC_RTCHECK_BOUNDS) > 0 && is_from_desc)
	{
	  char *msg;
	  const char *name = "<<unknown>>";
	  tree from_len;

	  if (DECL_P (to))
	    name = (const char *)(DECL_NAME (to)->identifier.id.str);

	  from_len = gfc_conv_descriptor_size (from_data, 1);
	  tmp = fold_build2_loc (input_location, NE_EXPR,
				  logical_type_node, from_len, orig_nelems);
	  msg = xasprintf ("Array bound mismatch for dimension %d "
			   "of array '%s' (%%ld/%%ld)",
			   1, name);

	  gfc_trans_runtime_check (true, false, tmp, &body,
				   &gfc_current_locus, msg,
			     fold_convert (long_integer_type_node, orig_nelems),
			       fold_convert (long_integer_type_node, from_len));

	  free (msg);
	}

      tmp = build_call_vec (fcn_type, fcn, args);

      /* Build the body of the loop.  */
      gfc_init_block (&loopbody);
      gfc_add_expr_to_block (&loopbody, tmp);

      /* Build the loop and return.  */
      gfc_init_loopinfo (&loop);
      loop.dimen = 1;
      loop.from[0] = gfc_index_zero_node;
      loop.loopvar[0] = index;
      loop.to[0] = nelems;
      gfc_trans_scalarizing_loops (&loop, &loopbody);
      gfc_init_block (&ifbody);
      gfc_add_block_to_block (&ifbody, &loop.pre);
      stdcopy = gfc_finish_block (&ifbody);
      /* In initialization mode from_len is a constant zero.  */
      if (unlimited && !integer_zerop (from_len))
	{
	  vec_safe_push (args, from_len);
	  vec_safe_push (args, to_len);
	  tmp = build_call_vec (fcn_type, fcn, args);
	  /* Build the body of the loop.  */
	  gfc_init_block (&loopbody);
	  gfc_add_expr_to_block (&loopbody, tmp);

	  /* Build the loop and return.  */
	  gfc_init_loopinfo (&loop);
	  loop.dimen = 1;
	  loop.from[0] = gfc_index_zero_node;
	  loop.loopvar[0] = index;
	  loop.to[0] = nelems;
	  gfc_trans_scalarizing_loops (&loop, &loopbody);
	  gfc_init_block (&ifbody);
	  gfc_add_block_to_block (&ifbody, &loop.pre);
	  extcopy = gfc_finish_block (&ifbody);

	  tmp = fold_build2_loc (input_location, GT_EXPR,
				 logical_type_node, from_len,
				 build_zero_cst (TREE_TYPE (from_len)));
	  tmp = fold_build3_loc (input_location, COND_EXPR,
				 void_type_node, tmp, extcopy, stdcopy);
	  gfc_add_expr_to_block (&body, tmp);
	  tmp = gfc_finish_block (&body);
	}
      else
	{
	  gfc_add_expr_to_block (&body, stdcopy);
	  tmp = gfc_finish_block (&body);
	}
      gfc_cleanup_loop (&loop);
    }
  else
    {
      gcc_assert (!is_from_desc);
      vec_safe_push (args, from_data);
      vec_safe_push (args, to_data);
      stdcopy = build_call_vec (fcn_type, fcn, args);

      /* In initialization mode from_len is a constant zero.  */
      if (unlimited && !integer_zerop (from_len))
	{
	  vec_safe_push (args, from_len);
	  vec_safe_push (args, to_len);
	  extcopy = build_call_vec (fcn_type, fcn, args);
	  tmp = fold_build2_loc (input_location, GT_EXPR,
				 logical_type_node, from_len,
				 build_zero_cst (TREE_TYPE (from_len)));
	  tmp = fold_build3_loc (input_location, COND_EXPR,
				 void_type_node, tmp, extcopy, stdcopy);
	}
      else
	tmp = stdcopy;
    }

  /* Only copy _def_init to to_data, when it is not a NULL-pointer.  */
  if (from == NULL_TREE)
    {
      tree cond;
      cond = fold_build2_loc (input_location, NE_EXPR,
			      logical_type_node,
			      from_data, null_pointer_node);
      tmp = fold_build3_loc (input_location, COND_EXPR,
			     void_type_node, cond,
			     tmp, build_empty_stmt (input_location));
    }

  return tmp;
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
  ppc_code = gfc_get_code (EXEC_CALL);
  ppc_code->resolved_sym = ppc->symtree->n.sym;
  /* Although '_copy' is set to be elemental in class.c, it is
     not staying that way.  Find out why, sometime....  */
  ppc_code->resolved_sym->attr.elemental = 1;
  ppc_code->ext.actual = actual;
  ppc_code->expr1 = ppc;
  /* Since '_copy' is elemental, the scalarizer will take care
     of arrays in gfc_trans_call.  */
  res = gfc_trans_call (ppc_code, false, NULL, NULL, false);
  gfc_free_statements (ppc_code);

  if (UNLIMITED_POLY(obj))
    {
      /* Check if rhs is non-NULL. */
      gfc_se src;
      gfc_init_se (&src, NULL);
      gfc_conv_expr (&src, rhs);
      src.expr = gfc_build_addr_expr (NULL_TREE, src.expr);
      tree cond = fold_build2_loc (input_location, NE_EXPR, logical_type_node,
				   src.expr, fold_convert (TREE_TYPE (src.expr),
							   null_pointer_node));
      res = build3_loc (input_location, COND_EXPR, TREE_TYPE (res), cond, res,
			build_empty_stmt (input_location));
    }

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

  rhs = gfc_copy_expr (code->expr1);
  gfc_add_vptr_component (rhs);

  /* Make sure that the component backend_decls have been built, which
     will not have happened if the derived types concerned have not
     been referenced.  */
  gfc_get_derived_type (rhs->ts.u.derived);
  gfc_add_def_init_component (rhs);
  /* The _def_init is always scalar.  */
  rhs->rank = 0;

  if (code->expr1->ts.type == BT_CLASS
      && CLASS_DATA (code->expr1)->attr.dimension)
    {
      gfc_array_spec *tmparr = gfc_get_array_spec ();
      *tmparr = *CLASS_DATA (code->expr1)->as;
      /* Adding the array ref to the class expression results in correct
	 indexing to the dynamic type.  */
      gfc_add_full_array_ref (lhs, tmparr);
      tmp = gfc_trans_class_array_init_assign (rhs, lhs, code->expr1);
    }
  else
    {
      /* Scalar initialization needs the _data component.  */
      gfc_add_data_component (lhs);
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
      src.expr = gfc_build_addr_expr (NULL_TREE, src.expr);

      tmp = gfc_build_memcpy_call (dst.expr, src.expr, memsz.expr);

      if (UNLIMITED_POLY(code->expr1))
	{
	  /* Check if _def_init is non-NULL. */
	  tree cond = fold_build2_loc (input_location, NE_EXPR,
				       logical_type_node, src.expr,
				       fold_convert (TREE_TYPE (src.expr),
						     null_pointer_node));
	  tmp = build3_loc (input_location, COND_EXPR, TREE_TYPE (tmp), cond,
			    tmp, build_empty_stmt (input_location));
	}
    }

  if (code->expr1->symtree->n.sym->attr.optional
      || code->expr1->symtree->n.sym->ns->proc_name->attr.entry_master)
    {
      tree present = gfc_conv_expr_present (code->expr1->symtree->n.sym);
      tmp = build3_loc (input_location, COND_EXPR, TREE_TYPE (tmp),
			present, tmp,
			build_empty_stmt (input_location));
    }

  gfc_add_expr_to_block (&block, tmp);

  return gfc_finish_block (&block);
}


/* End of prototype trans-class.c  */


static void
realloc_lhs_warning (bt type, bool array, locus *where)
{
  if (array && type != BT_CLASS && type != BT_DERIVED && warn_realloc_lhs)
    gfc_warning (OPT_Wrealloc_lhs,
		 "Code for reallocating the allocatable array at %L will "
		 "be added", where);
  else if (warn_realloc_lhs_all)
    gfc_warning (OPT_Wrealloc_lhs_all,
		 "Code for reallocating the allocatable variable at %L "
		 "will be added", where);
}


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
   of a child se until after the previous se has been translated.  */

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
gfc_conv_expr_present (gfc_symbol * sym, bool use_saved_desc)
{
  tree decl, orig_decl, cond;

  gcc_assert (sym->attr.dummy);
  orig_decl = decl = gfc_get_symbol_decl (sym);

  /* Intrinsic scalars with VALUE attribute which are passed by value
     use a hidden argument to denote the present status.  */
  if (sym->attr.value && sym->ts.type != BT_CHARACTER
      && sym->ts.type != BT_CLASS && sym->ts.type != BT_DERIVED
      && !sym->attr.dimension)
    {
      char name[GFC_MAX_SYMBOL_LEN + 2];
      tree tree_name;

      gcc_assert (TREE_CODE (decl) == PARM_DECL);
      name[0] = '_';
      strcpy (&name[1], sym->name);
      tree_name = get_identifier (name);

      /* Walk function argument list to find hidden arg.  */
      cond = DECL_ARGUMENTS (DECL_CONTEXT (decl));
      for ( ; cond != NULL_TREE; cond = TREE_CHAIN (cond))
	if (DECL_NAME (cond) == tree_name
	    && DECL_ARTIFICIAL (cond))
	  break;

      gcc_assert (cond);
      return cond;
    }

  /* Assumed-shape arrays use a local variable for the array data;
     the actual PARAM_DECL is in a saved decl.  As the local variable
     is NULL, it can be checked instead, unless use_saved_desc is
     requested.  */

  if (use_saved_desc && TREE_CODE (decl) != PARM_DECL)
    {
      gcc_assert (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (decl))
             || GFC_ARRAY_TYPE_P (TREE_TYPE (decl)));
      decl = GFC_DECL_SAVED_DESCRIPTOR (decl);
    }

  cond = fold_build2_loc (input_location, NE_EXPR, logical_type_node, decl,
			  fold_convert (TREE_TYPE (decl), null_pointer_node));

  /* Fortran 2008 allows to pass null pointers and non-associated pointers
     as actual argument to denote absent dummies. For array descriptors,
     we thus also need to check the array descriptor.  For BT_CLASS, it
     can also occur for scalars and F2003 due to type->class wrapping and
     class->class wrapping.  Note further that BT_CLASS always uses an
     array descriptor for arrays, also for explicit-shape/assumed-size.
     For assumed-rank arrays, no local variable is generated, hence,
     the following also applies with !use_saved_desc.  */

  if ((use_saved_desc || TREE_CODE (orig_decl) == PARM_DECL)
      && !sym->attr.allocatable
      && ((sym->ts.type != BT_CLASS && !sym->attr.pointer)
	  || (sym->ts.type == BT_CLASS
	      && !CLASS_DATA (sym)->attr.allocatable
	      && !CLASS_DATA (sym)->attr.class_pointer))
      && ((gfc_option.allow_std & GFC_STD_F2008) != 0
	  || sym->ts.type == BT_CLASS))
    {
      tree tmp;

      if ((sym->as && (sym->as->type == AS_ASSUMED_SHAPE
		       || sym->as->type == AS_ASSUMED_RANK
		       || sym->attr.codimension))
	  || (sym->ts.type == BT_CLASS && CLASS_DATA (sym)->as))
	{
	  tmp = build_fold_indirect_ref_loc (input_location, decl);
	  if (sym->ts.type == BT_CLASS)
	    tmp = gfc_class_data_get (tmp);
	  tmp = gfc_conv_array_data (tmp);
	}
      else if (sym->ts.type == BT_CLASS)
	tmp = gfc_class_data_get (decl);
      else
	tmp = NULL_TREE;

      if (tmp != NULL_TREE)
	{
	  tmp = fold_build2_loc (input_location, NE_EXPR, logical_type_node, tmp,
				 fold_convert (TREE_TYPE (tmp), null_pointer_node));
	  cond = fold_build2_loc (input_location, TRUTH_ANDIF_EXPR,
				  logical_type_node, cond, tmp);
	}
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
  gfc_se se;

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

	case REF_SUBSTRING:
	  gfc_init_se (&se, NULL);
	  gfc_conv_expr_type (&se, r->u.ss.start, gfc_charlen_type_node);
	  length = se.expr;
	  gfc_conv_expr_type (&se, r->u.ss.end, gfc_charlen_type_node);
	  length = fold_build2_loc (input_location, MINUS_EXPR,
				    gfc_charlen_type_node,
				    se.expr, length);
	  length = fold_build2_loc (input_location, PLUS_EXPR,
				    gfc_charlen_type_node, length,
				    gfc_index_one_node);
	  break;

	default:
	  gcc_unreachable ();
	  break;
	}
    }

  gcc_assert (length != NULL);
  return length;
}


/* Return for an expression the backend decl of the coarray.  */

tree
gfc_get_tree_for_caf_expr (gfc_expr *expr)
{
  tree caf_decl;
  bool found = false;
  gfc_ref *ref;

  gcc_assert (expr && expr->expr_type == EXPR_VARIABLE);

  /* Not-implemented diagnostic.  */
  if (expr->symtree->n.sym->ts.type == BT_CLASS
      && UNLIMITED_POLY (expr->symtree->n.sym)
      && CLASS_DATA (expr->symtree->n.sym)->attr.codimension)
    gfc_error ("Sorry, coindexed access to an unlimited polymorphic object at "
	       "%L is not supported", &expr->where);

  for (ref = expr->ref; ref; ref = ref->next)
    if (ref->type == REF_COMPONENT)
      {
	if (ref->u.c.component->ts.type == BT_CLASS
	    && UNLIMITED_POLY (ref->u.c.component)
	    && CLASS_DATA (ref->u.c.component)->attr.codimension)
	  gfc_error ("Sorry, coindexed access to an unlimited polymorphic "
		     "component at %L is not supported", &expr->where);
      }

  /* Make sure the backend_decl is present before accessing it.  */
  caf_decl = expr->symtree->n.sym->backend_decl == NULL_TREE
      ? gfc_get_symbol_decl (expr->symtree->n.sym)
      : expr->symtree->n.sym->backend_decl;

  if (expr->symtree->n.sym->ts.type == BT_CLASS)
    {
      if (expr->ref && expr->ref->type == REF_ARRAY)
	{
	  caf_decl = gfc_class_data_get (caf_decl);
	  if (CLASS_DATA (expr->symtree->n.sym)->attr.codimension)
	    return caf_decl;
	}
      for (ref = expr->ref; ref; ref = ref->next)
	{
	  if (ref->type == REF_COMPONENT
	      && strcmp (ref->u.c.component->name, "_data") != 0)
	    {
	      caf_decl = gfc_class_data_get (caf_decl);
	      if (CLASS_DATA (expr->symtree->n.sym)->attr.codimension)
		return caf_decl;
	      break;
	    }
	  else if (ref->type == REF_ARRAY && ref->u.ar.dimen)
	    break;
	}
    }
  if (expr->symtree->n.sym->attr.codimension)
    return caf_decl;

  /* The following code assumes that the coarray is a component reachable via
     only scalar components/variables; the Fortran standard guarantees this.  */

  for (ref = expr->ref; ref; ref = ref->next)
    if (ref->type == REF_COMPONENT)
      {
	gfc_component *comp = ref->u.c.component;

	if (POINTER_TYPE_P (TREE_TYPE (caf_decl)))
	  caf_decl = build_fold_indirect_ref_loc (input_location, caf_decl);
	caf_decl = fold_build3_loc (input_location, COMPONENT_REF,
				    TREE_TYPE (comp->backend_decl), caf_decl,
				    comp->backend_decl, NULL_TREE);
	if (comp->ts.type == BT_CLASS)
	  {
	    caf_decl = gfc_class_data_get (caf_decl);
	    if (CLASS_DATA (comp)->attr.codimension)
	      {
		found = true;
		break;
	      }
	  }
	if (comp->attr.codimension)
	  {
	    found = true;
	    break;
	  }
      }
  gcc_assert (found && caf_decl);
  return caf_decl;
}


/* Obtain the Coarray token - and optionally also the offset.  */

void
gfc_get_caf_token_offset (gfc_se *se, tree *token, tree *offset, tree caf_decl,
			  tree se_expr, gfc_expr *expr)
{
  tree tmp;

  /* Coarray token.  */
  if (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (caf_decl)))
    {
      gcc_assert (GFC_TYPE_ARRAY_AKIND (TREE_TYPE (caf_decl))
		    == GFC_ARRAY_ALLOCATABLE
		  || expr->symtree->n.sym->attr.select_type_temporary);
      *token = gfc_conv_descriptor_token (caf_decl);
    }
  else if (DECL_LANG_SPECIFIC (caf_decl)
	   && GFC_DECL_TOKEN (caf_decl) != NULL_TREE)
    *token = GFC_DECL_TOKEN (caf_decl);
  else
    {
      gcc_assert (GFC_ARRAY_TYPE_P (TREE_TYPE (caf_decl))
		  && GFC_TYPE_ARRAY_CAF_TOKEN (TREE_TYPE (caf_decl)) != NULL_TREE);
      *token = GFC_TYPE_ARRAY_CAF_TOKEN (TREE_TYPE (caf_decl));
    }

  if (offset == NULL)
    return;

  /* Offset between the coarray base address and the address wanted.  */
  if (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (caf_decl))
      && (GFC_TYPE_ARRAY_AKIND (TREE_TYPE (caf_decl)) == GFC_ARRAY_ALLOCATABLE
	  || GFC_TYPE_ARRAY_AKIND (TREE_TYPE (caf_decl)) == GFC_ARRAY_POINTER))
    *offset = build_int_cst (gfc_array_index_type, 0);
  else if (DECL_LANG_SPECIFIC (caf_decl)
	   && GFC_DECL_CAF_OFFSET (caf_decl) != NULL_TREE)
    *offset = GFC_DECL_CAF_OFFSET (caf_decl);
  else if (GFC_TYPE_ARRAY_CAF_OFFSET (TREE_TYPE (caf_decl)) != NULL_TREE)
    *offset = GFC_TYPE_ARRAY_CAF_OFFSET (TREE_TYPE (caf_decl));
  else
    *offset = build_int_cst (gfc_array_index_type, 0);

  if (POINTER_TYPE_P (TREE_TYPE (se_expr))
      && GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (TREE_TYPE (se_expr))))
    {
      tmp = build_fold_indirect_ref_loc (input_location, se_expr);
      tmp = gfc_conv_descriptor_data_get (tmp);
    }
  else if (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (se_expr)))
    tmp = gfc_conv_descriptor_data_get (se_expr);
  else
    {
      gcc_assert (POINTER_TYPE_P (TREE_TYPE (se_expr)));
      tmp = se_expr;
    }

  *offset = fold_build2_loc (input_location, PLUS_EXPR, gfc_array_index_type,
			     *offset, fold_convert (gfc_array_index_type, tmp));

  if (expr->symtree->n.sym->ts.type == BT_DERIVED
      && expr->symtree->n.sym->attr.codimension
      && expr->symtree->n.sym->ts.u.derived->attr.alloc_comp)
    {
      gfc_expr *base_expr = gfc_copy_expr (expr);
      gfc_ref *ref = base_expr->ref;
      gfc_se base_se;

      // Iterate through the refs until the last one.
      while (ref->next)
	  ref = ref->next;

      if (ref->type == REF_ARRAY
	  && ref->u.ar.type != AR_FULL)
	{
	  const int ranksum = ref->u.ar.dimen + ref->u.ar.codimen;
	  int i;
	  for (i = 0; i < ranksum; ++i)
	    {
	      ref->u.ar.start[i] = NULL;
	      ref->u.ar.end[i] = NULL;
	    }
	  ref->u.ar.type = AR_FULL;
	}
      gfc_init_se (&base_se, NULL);
      if (gfc_caf_attr (base_expr).dimension)
	{
	  gfc_conv_expr_descriptor (&base_se, base_expr);
	  tmp = gfc_conv_descriptor_data_get (base_se.expr);
	}
      else
	{
	  gfc_conv_expr (&base_se, base_expr);
	  tmp = base_se.expr;
	}

      gfc_free_expr (base_expr);
      gfc_add_block_to_block (&se->pre, &base_se.pre);
      gfc_add_block_to_block (&se->post, &base_se.post);
    }
  else if (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (caf_decl)))
    tmp = gfc_conv_descriptor_data_get (caf_decl);
  else
   {
     gcc_assert (POINTER_TYPE_P (TREE_TYPE (caf_decl)));
     tmp = caf_decl;
   }

  *offset = fold_build2_loc (input_location, MINUS_EXPR, gfc_array_index_type,
			    fold_convert (gfc_array_index_type, *offset),
			    fold_convert (gfc_array_index_type, tmp));
}


/* Convert the coindex of a coarray into an image index; the result is
   image_num =  (idx(1)-lcobound(1)+1) + (idx(2)-lcobound(2))*extent(1)
              + (idx(3)-lcobound(3))*extend(1)*extent(2) + ...  */

tree
gfc_caf_get_image_index (stmtblock_t *block, gfc_expr *e, tree desc)
{
  gfc_ref *ref;
  tree lbound, ubound, extent, tmp, img_idx;
  gfc_se se;
  int i;

  for (ref = e->ref; ref; ref = ref->next)
    if (ref->type == REF_ARRAY && ref->u.ar.codimen > 0)
      break;
  gcc_assert (ref != NULL);

  if (ref->u.ar.dimen_type[ref->u.ar.dimen] == DIMEN_THIS_IMAGE)
    {
      return build_call_expr_loc (input_location, gfor_fndecl_caf_this_image, 1,
				  integer_zero_node);
    }

  img_idx = build_zero_cst (gfc_array_index_type);
  extent = build_one_cst (gfc_array_index_type);
  if (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (desc)))
    for (i = ref->u.ar.dimen; i < ref->u.ar.dimen + ref->u.ar.codimen; i++)
      {
	gfc_init_se (&se, NULL);
	gfc_conv_expr_type (&se, ref->u.ar.start[i], gfc_array_index_type);
	gfc_add_block_to_block (block, &se.pre);
	lbound = gfc_conv_descriptor_lbound_get (desc, gfc_rank_cst[i]);
	tmp = fold_build2_loc (input_location, MINUS_EXPR,
			       TREE_TYPE (lbound), se.expr, lbound);
	tmp = fold_build2_loc (input_location, MULT_EXPR, TREE_TYPE (tmp),
			       extent, tmp);
	img_idx = fold_build2_loc (input_location, PLUS_EXPR,
				   TREE_TYPE (tmp), img_idx, tmp);
	if (i < ref->u.ar.dimen + ref->u.ar.codimen - 1)
	  {
	    ubound = gfc_conv_descriptor_ubound_get (desc, gfc_rank_cst[i]);
	    tmp = gfc_conv_array_extent_dim (lbound, ubound, NULL);
	    extent = fold_build2_loc (input_location, MULT_EXPR,
				      TREE_TYPE (tmp), extent, tmp);
	  }
      }
  else
    for (i = ref->u.ar.dimen; i < ref->u.ar.dimen + ref->u.ar.codimen; i++)
      {
	gfc_init_se (&se, NULL);
	gfc_conv_expr_type (&se, ref->u.ar.start[i], gfc_array_index_type);
	gfc_add_block_to_block (block, &se.pre);
	lbound = GFC_TYPE_ARRAY_LBOUND (TREE_TYPE (desc), i);
	tmp = fold_build2_loc (input_location, MINUS_EXPR,
			       TREE_TYPE (lbound), se.expr, lbound);
	tmp = fold_build2_loc (input_location, MULT_EXPR, TREE_TYPE (tmp),
			       extent, tmp);
	img_idx = fold_build2_loc (input_location, PLUS_EXPR, TREE_TYPE (tmp),
				   img_idx, tmp);
	if (i < ref->u.ar.dimen + ref->u.ar.codimen - 1)
	  {
	    ubound = GFC_TYPE_ARRAY_UBOUND (TREE_TYPE (desc), i);
	    tmp = fold_build2_loc (input_location, MINUS_EXPR,
				   TREE_TYPE (ubound), ubound, lbound);
	    tmp = fold_build2_loc (input_location, PLUS_EXPR, TREE_TYPE (tmp),
				   tmp, build_one_cst (TREE_TYPE (tmp)));
	    extent = fold_build2_loc (input_location, MULT_EXPR,
				      TREE_TYPE (tmp), extent, tmp);
	  }
      }
  img_idx = fold_build2_loc (input_location, PLUS_EXPR, TREE_TYPE (img_idx),
			     img_idx, build_one_cst (TREE_TYPE (img_idx)));
  return fold_convert (integer_type_node, img_idx);
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
      gcc_fallthrough ();
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

  if (!cl->length && cl->backend_decl && VAR_P (cl->backend_decl))
    return;

  /* If cl->length is NULL, use gfc_conv_expr to obtain the string length but
     "flatten" array constructors by taking their first element; all elements
     should be the same length or a cl->length should be present.  */
  if (!cl->length)
    {
      gfc_expr* expr_flat;
      if (!expr)
	return;
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
			     se.expr, build_zero_cst (TREE_TYPE (se.expr)));
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
  mpz_t length;

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
      if ((TREE_CODE (TREE_TYPE (se->expr)) == ARRAY_TYPE
	   || TREE_CODE (TREE_TYPE (se->expr)) == INTEGER_TYPE)
	  && TYPE_STRING_FLAG (TREE_TYPE (se->expr)))
	tmp = se->expr;
      else
	tmp = build_fold_indirect_ref_loc (input_location,
				       se->expr);
      /* For BIND(C), a BT_CHARACTER is not an ARRAY_TYPE.  */
      if (TREE_CODE (TREE_TYPE (tmp)) == ARRAY_TYPE)
	{
	  tmp = gfc_build_array_ref (tmp, start.expr, NULL);
	  se->expr = gfc_build_addr_expr (type, tmp);
	}
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
				       logical_type_node, start.expr,
				       end.expr);

      /* Check lower bound.  */
      fault = fold_build2_loc (input_location, LT_EXPR, logical_type_node,
			       start.expr,
			       build_one_cst (TREE_TYPE (start.expr)));
      fault = fold_build2_loc (input_location, TRUTH_ANDIF_EXPR,
			       logical_type_node, nonempty, fault);
      if (name)
	msg = xasprintf ("Substring out of bounds: lower bound (%%ld) of '%s' "
			 "is less than one", name);
      else
	msg = xasprintf ("Substring out of bounds: lower bound (%%ld) "
			 "is less than one");
      gfc_trans_runtime_check (true, false, fault, &se->pre, where, msg,
			       fold_convert (long_integer_type_node,
					     start.expr));
      free (msg);

      /* Check upper bound.  */
      fault = fold_build2_loc (input_location, GT_EXPR, logical_type_node,
			       end.expr, se->string_length);
      fault = fold_build2_loc (input_location, TRUTH_ANDIF_EXPR,
			       logical_type_node, nonempty, fault);
      if (name)
	msg = xasprintf ("Substring out of bounds: upper bound (%%ld) of '%s' "
			 "exceeds string length (%%ld)", name);
      else
	msg = xasprintf ("Substring out of bounds: upper bound (%%ld) "
			 "exceeds string length (%%ld)");
      gfc_trans_runtime_check (true, false, fault, &se->pre, where, msg,
			       fold_convert (long_integer_type_node, end.expr),
			       fold_convert (long_integer_type_node,
					     se->string_length));
      free (msg);
    }

  /* Try to calculate the length from the start and end expressions.  */
  if (ref->u.ss.end
      && gfc_dep_difference (ref->u.ss.end, ref->u.ss.start, &length))
    {
      HOST_WIDE_INT i_len;

      i_len = gfc_mpz_get_hwi (length) + 1;
      if (i_len < 0)
	i_len = 0;

      tmp = build_int_cst (gfc_charlen_type_node, i_len);
      mpz_clear (length);  /* Was initialized by gfc_dep_difference.  */
    }
  else
    {
      tmp = fold_build2_loc (input_location, MINUS_EXPR, gfc_charlen_type_node,
			     fold_convert (gfc_charlen_type_node, end.expr),
			     fold_convert (gfc_charlen_type_node, start.expr));
      tmp = fold_build2_loc (input_location, PLUS_EXPR, gfc_charlen_type_node,
			     build_int_cst (gfc_charlen_type_node, 1), tmp);
      tmp = fold_build2_loc (input_location, MAX_EXPR, gfc_charlen_type_node,
			     tmp, build_int_cst (gfc_charlen_type_node, 0));
    }

  se->string_length = tmp;
}


/* Convert a derived type component reference.  */

void
gfc_conv_component_ref (gfc_se * se, gfc_ref * ref)
{
  gfc_component *c;
  tree tmp;
  tree decl;
  tree field;
  tree context;

  c = ref->u.c.component;

  if (c->backend_decl == NULL_TREE
      && ref->u.c.sym != NULL)
    gfc_get_derived_type (ref->u.c.sym);

  field = c->backend_decl;
  gcc_assert (field && TREE_CODE (field) == FIELD_DECL);
  decl = se->expr;
  context = DECL_FIELD_CONTEXT (field);

  /* Components can correspond to fields of different containing
     types, as components are created without context, whereas
     a concrete use of a component has the type of decl as context.
     So, if the type doesn't match, we search the corresponding
     FIELD_DECL in the parent type.  To not waste too much time
     we cache this result in norestrict_decl.
     On the other hand, if the context is a UNION or a MAP (a
     RECORD_TYPE within a UNION_TYPE) always use the given FIELD_DECL.  */

  if (context != TREE_TYPE (decl)
      && !(   TREE_CODE (TREE_TYPE (field)) == UNION_TYPE /* Field is union */
           || TREE_CODE (context) == UNION_TYPE))         /* Field is map */
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

  if (ref->u.c.sym && ref->u.c.sym->ts.type == BT_CLASS
      && strcmp ("_data", c->name) == 0)
    {
      /* Found a ref to the _data component.  Store the associated ref to
	 the vptr in se->class_vptr.  */
      se->class_vptr = gfc_class_vptr_get (decl);
    }
  else
    se->class_vptr = NULL_TREE;

  tmp = fold_build3_loc (input_location, COMPONENT_REF, TREE_TYPE (field),
			 decl, field, NULL_TREE);

  se->expr = tmp;

  /* Allocatable deferred char arrays are to be handled by the gfc_deferred_
     strlen () conditional below.  */
  if (c->ts.type == BT_CHARACTER && !c->attr.proc_pointer
      && !(c->attr.allocatable && c->ts.deferred)
      && !c->attr.pdt_string)
    {
      tmp = c->ts.u.cl->backend_decl;
      /* Components must always be constant length.  */
      gcc_assert (tmp && INTEGER_CST_P (tmp));
      se->string_length = tmp;
    }

  if (gfc_deferred_strlen (c, &field))
    {
      tmp = fold_build3_loc (input_location, COMPONENT_REF,
			     TREE_TYPE (field),
			     decl, field, NULL_TREE);
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
   parent type for derived type extensions.  */
void
conv_parent_component_references (gfc_se * se, gfc_ref * ref)
{
  gfc_component *c;
  gfc_component *cmp;
  gfc_symbol *dt;
  gfc_ref parent;

  dt = ref->u.c.sym;
  c = ref->u.c.component;

  /* Return if the component is in the parent type.  */
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


static void
conv_inquiry (gfc_se * se, gfc_ref * ref, gfc_expr *expr, gfc_typespec *ts)
{
  tree res = se->expr;

  switch (ref->u.i)
    {
    case INQUIRY_RE:
      res = fold_build1_loc (input_location, REALPART_EXPR,
			     TREE_TYPE (TREE_TYPE (res)), res);
      break;

    case INQUIRY_IM:
      res = fold_build1_loc (input_location, IMAGPART_EXPR,
			     TREE_TYPE (TREE_TYPE (res)), res);
      break;

    case INQUIRY_KIND:
      res = build_int_cst (gfc_typenode_for_spec (&expr->ts),
			   ts->kind);
      break;

    case INQUIRY_LEN:
      res = fold_convert (gfc_typenode_for_spec (&expr->ts),
			  se->string_length);
      break;

    default:
      gcc_unreachable ();
    }
  se->expr = res;
}

/* Dereference VAR where needed if it is a pointer, reference, etc.
   according to Fortran semantics.  */

tree
gfc_maybe_dereference_var (gfc_symbol *sym, tree var, bool descriptor_only_p,
			   bool is_classarray)
{
  /* Characters are entirely different from other types, they are treated
     separately.  */
  if (sym->ts.type == BT_CHARACTER)
    {
      /* Dereference character pointer dummy arguments
	 or results.  */
      if ((sym->attr.pointer || sym->attr.allocatable
	   || (sym->as && sym->as->type == AS_ASSUMED_RANK))
	  && (sym->attr.dummy
	      || sym->attr.function
	      || sym->attr.result))
	var = build_fold_indirect_ref_loc (input_location, var);
    }
  else if (!sym->attr.value)
    {
      /* Dereference temporaries for class array dummy arguments.  */
      if (sym->attr.dummy && is_classarray
	  && GFC_ARRAY_TYPE_P (TREE_TYPE (var)))
	{
	  if (!descriptor_only_p)
	    var = GFC_DECL_SAVED_DESCRIPTOR (var);

	  var = build_fold_indirect_ref_loc (input_location, var);
	}

      /* Dereference non-character scalar dummy arguments.  */
      if (sym->attr.dummy && !sym->attr.dimension
	  && !(sym->attr.codimension && sym->attr.allocatable)
	  && (sym->ts.type != BT_CLASS
	      || (!CLASS_DATA (sym)->attr.dimension
		  && !(CLASS_DATA (sym)->attr.codimension
		       && CLASS_DATA (sym)->attr.allocatable))))
	var = build_fold_indirect_ref_loc (input_location, var);

      /* Dereference scalar hidden result.  */
      if (flag_f2c && sym->ts.type == BT_COMPLEX
	  && (sym->attr.function || sym->attr.result)
	  && !sym->attr.dimension && !sym->attr.pointer
	  && !sym->attr.always_explicit)
	var = build_fold_indirect_ref_loc (input_location, var);

      /* Dereference non-character, non-class pointer variables.
	 These must be dummies, results, or scalars.  */
      if (!is_classarray
	  && (sym->attr.pointer || sym->attr.allocatable
	      || gfc_is_associate_pointer (sym)
	      || (sym->as && sym->as->type == AS_ASSUMED_RANK))
	  && (sym->attr.dummy
	      || sym->attr.function
	      || sym->attr.result
	      || (!sym->attr.dimension
		  && (!sym->attr.codimension || !sym->attr.allocatable))))
	var = build_fold_indirect_ref_loc (input_location, var);
      /* Now treat the class array pointer variables accordingly.  */
      else if (sym->ts.type == BT_CLASS
	       && sym->attr.dummy
	       && (CLASS_DATA (sym)->attr.dimension
		   || CLASS_DATA (sym)->attr.codimension)
	       && ((CLASS_DATA (sym)->as
		    && CLASS_DATA (sym)->as->type == AS_ASSUMED_RANK)
		   || CLASS_DATA (sym)->attr.allocatable
		   || CLASS_DATA (sym)->attr.class_pointer))
	var = build_fold_indirect_ref_loc (input_location, var);
      /* And the case where a non-dummy, non-result, non-function,
	 non-allotable and non-pointer classarray is present.  This case was
	 previously covered by the first if, but with introducing the
	 condition !is_classarray there, that case has to be covered
	 explicitly.  */
      else if (sym->ts.type == BT_CLASS
	       && !sym->attr.dummy
	       && !sym->attr.function
	       && !sym->attr.result
	       && (CLASS_DATA (sym)->attr.dimension
		   || CLASS_DATA (sym)->attr.codimension)
	       && (sym->assoc
		   || !CLASS_DATA (sym)->attr.allocatable)
	       && !CLASS_DATA (sym)->attr.class_pointer)
	var = build_fold_indirect_ref_loc (input_location, var);
    }

  return var;
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
  bool is_classarray;
  bool first_time = true;

  sym = expr->symtree->n.sym;
  is_classarray = IS_CLASS_ARRAY (sym);
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
      ref = ss_info->data.array.ref;
      if (ref)
	gcc_assert (ref->type == REF_ARRAY
		    && ref->u.ar.type != AR_ELEMENT);
      else
	gfc_conv_tmp_array_ref (se);
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

      /* Procedure actual arguments.  Look out for temporary variables
	 with the same attributes as function values.  */
      else if (!sym->attr.temporary
	       && sym->attr.flavor == FL_PROCEDURE
	       && se->expr != current_function_decl)
	{
	  if (!sym->attr.dummy && !sym->attr.proc_pointer)
	    {
	      gcc_assert (TREE_CODE (se->expr) == FUNCTION_DECL);
	      se->expr = gfc_build_addr_expr (NULL_TREE, se->expr);
	    }
	  return;
	}

      /* Dereference the expression, where needed.  */
      se->expr = gfc_maybe_dereference_var (sym, se->expr, se->descriptor_only,
					    is_classarray);

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

  gfc_typespec *ts = &sym->ts;
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

	  gfc_conv_array_ref (se, &ref->u.ar, expr, &expr->where);
	  /* Return a pointer to an element.  */
	  break;

	case REF_COMPONENT:
	  ts = &ref->u.c.component->ts;
	  if (first_time && is_classarray && sym->attr.dummy
	      && se->descriptor_only
	      && !CLASS_DATA (sym)->attr.allocatable
	      && !CLASS_DATA (sym)->attr.class_pointer
	      && CLASS_DATA (sym)->as
	      && CLASS_DATA (sym)->as->type != AS_ASSUMED_RANK
	      && strcmp ("_data", ref->u.c.component->name) == 0)
	    /* Skip the first ref of a _data component, because for class
	       arrays that one is already done by introducing a temporary
	       array descriptor.  */
	    break;

	  if (ref->u.c.sym->attr.extension)
	    conv_parent_component_references (se, ref);

	  gfc_conv_component_ref (se, ref);
	  if (!ref->next && ref->u.c.sym->attr.codimension
	      && se->want_pointer && se->descriptor_only)
	    return;

	  break;

	case REF_SUBSTRING:
	  gfc_conv_substring (se, ref, expr->ts.kind,
			      expr->symtree->name, &expr->where);
	  break;

	case REF_INQUIRY:
	  conv_inquiry (se, ref, expr, ts);
	  break;

	default:
	  gcc_unreachable ();
	  break;
	}
      first_time = false;
      ref = ref->next;
    }
  /* Pointer assignment, allocation or pass by reference.  Arrays are handled
     separately.  */
  if (se->want_pointer)
    {
      if (expr->ts.type == BT_CHARACTER && !gfc_is_proc_ptr_comp (expr))
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
  wi::tree_to_wide_ref wrhs = wi::to_wide (rhs);

  /* If exponent is too large, we won't expand it anyway, so don't bother
     with large integer values.  */
  if (!wi::fits_shwi_p (wrhs))
    return 0;

  m = wrhs.to_shwi ();
  /* Use the wide_int's routine to reliably get the absolute value on all
     platforms.  Then convert it to a HOST_WIDE_INT like above.  */
  n = wi::abs (wrhs).to_shwi ();

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
      tmp = fold_build2_loc (input_location, EQ_EXPR, logical_type_node,
			     lhs, build_int_cst (TREE_TYPE (lhs), -1));
      cond = fold_build2_loc (input_location, EQ_EXPR, logical_type_node,
			      lhs, build_int_cst (TREE_TYPE (lhs), 1));

      /* If rhs is even,
	 result = (lhs == 1 || lhs == -1) ? 1 : 0.  */
      if ((n & 1) == 0)
        {
	  tmp = fold_build2_loc (input_location, TRUTH_OR_EXPR,
				 logical_type_node, tmp, cond);
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

  if (INTEGER_CST_P (lse.expr)
      && TREE_CODE (TREE_TYPE (rse.expr)) == INTEGER_TYPE)
    {
      wi::tree_to_wide_ref wlhs = wi::to_wide (lse.expr);
      HOST_WIDE_INT v, w;
      int kind, ikind, bit_size;

      v = wlhs.to_shwi ();
      w = abs (v);

      kind = expr->value.op.op1->ts.kind;
      ikind = gfc_validate_kind (BT_INTEGER, kind, false);
      bit_size = gfc_integer_kinds[ikind].bit_size;

      if (v == 1)
	{
	  /* 1**something is always 1.  */
	  se->expr = build_int_cst (TREE_TYPE (lse.expr), 1);
	  return;
	}
      else if (v == -1)
	{
	  /* (-1)**n is 1 - ((n & 1) << 1) */
	  tree type;
	  tree tmp;

	  type = TREE_TYPE (lse.expr);
	  tmp = fold_build2_loc (input_location, BIT_AND_EXPR, type,
				 rse.expr, build_int_cst (type, 1));
	  tmp = fold_build2_loc (input_location, LSHIFT_EXPR, type,
				 tmp, build_int_cst (type, 1));
	  tmp = fold_build2_loc (input_location, MINUS_EXPR, type,
				 build_int_cst (type, 1), tmp);
	  se->expr = tmp;
	  return;
	}
      else if (w > 0 && ((w & (w-1)) == 0) && ((w >> (bit_size-1)) == 0))
	{
	  /* Here v is +/- 2**e.  The further simplification uses
	     2**n = 1<<n, 4**n = 1<<(n+n), 8**n = 1 <<(3*n), 16**n =
	     1<<(4*n), etc., but we have to make sure to return zero
	     if the number of bits is too large. */
	  tree lshift;
	  tree type;
	  tree shift;
	  tree ge;
	  tree cond;
	  tree num_bits;
	  tree cond2;
	  tree tmp1;

	  type = TREE_TYPE (lse.expr);

	  if (w == 2)
	    shift = rse.expr;
	  else if (w == 4)
	    shift = fold_build2_loc (input_location, PLUS_EXPR,
				     TREE_TYPE (rse.expr),
				       rse.expr, rse.expr);
	  else
	    {
	      /* use popcount for fast log2(w) */
	      int e = wi::popcount (w-1);
	      shift = fold_build2_loc (input_location, MULT_EXPR,
				       TREE_TYPE (rse.expr),
				       build_int_cst (TREE_TYPE (rse.expr), e),
				       rse.expr);
	    }

	  lshift = fold_build2_loc (input_location, LSHIFT_EXPR, type,
				    build_int_cst (type, 1), shift);
	  ge = fold_build2_loc (input_location, GE_EXPR, logical_type_node,
				rse.expr, build_int_cst (type, 0));
	  cond = fold_build3_loc (input_location, COND_EXPR, type, ge, lshift,
				 build_int_cst (type, 0));
	  num_bits = build_int_cst (TREE_TYPE (rse.expr), TYPE_PRECISION (type));
	  cond2 = fold_build2_loc (input_location, GE_EXPR, logical_type_node,
				   rse.expr, num_bits);
	  tmp1 = fold_build3_loc (input_location, COND_EXPR, type, cond2,
				  build_int_cst (type, 0), cond);
	  if (v > 0)
	    {
	      se->expr = tmp1;
	    }
	  else
	    {
	      /* for v < 0, calculate v**n = |v|**n * (-1)**n */
	      tree tmp2;
	      tmp2 = fold_build2_loc (input_location, BIT_AND_EXPR, type,
				      rse.expr, build_int_cst (type, 1));
	      tmp2 = fold_build2_loc (input_location, LSHIFT_EXPR, type,
				      tmp2, build_int_cst (type, 1));
	      tmp2 = fold_build2_loc (input_location, MINUS_EXPR, type,
				      build_int_cst (type, 1), tmp2);
	      se->expr = fold_build2_loc (input_location, MULT_EXPR, type,
					  tmp1, tmp2);
	    }
	  return;
	}
    }

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
			     TREE_TYPE (len), len,
			     build_int_cst (TREE_TYPE (len), 1));
      tmp = build_range_type (gfc_charlen_type_node, size_zero_node, tmp);

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
      gcc_assert (POINTER_TYPE_P (type));
      tmp = TREE_TYPE (type);
      if (TREE_CODE (tmp) == ARRAY_TYPE)
        tmp = TREE_TYPE (tmp);
      tmp = TYPE_SIZE_UNIT (tmp);
      tmp = fold_build2_loc (input_location, MULT_EXPR, size_type_node,
			    fold_convert (size_type_node, len),
			    fold_convert (size_type_node, tmp));
      tmp = gfc_call_malloc (&se->pre, type, tmp);
      gfc_add_modify (&se->pre, var, tmp);

      /* Free the temporary afterwards.  */
      tmp = gfc_call_free (var);
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
			     gfc_charlen_type_node,
			     fold_convert (gfc_charlen_type_node,
					   lse.string_length),
			     fold_convert (gfc_charlen_type_node,
					   rse.string_length));
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
      if ((expr->ts.type == BT_REAL || expr->ts.type == BT_COMPLEX)
	  && flag_protect_parens)
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
      code = flag_frontend_optimize ? TRUTH_ANDIF_EXPR : TRUTH_AND_EXPR;
      lop = 1;
      break;

    case INTRINSIC_OR:
      code = flag_frontend_optimize ? TRUTH_ORIF_EXPR : TRUTH_OR_EXPR;
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
      fatal_error (input_location, "Unknown intrinsic op");
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
      /* The result of logical ops is always logical_type_node.  */
      tmp = fold_build2_loc (input_location, code, logical_type_node,
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

  if (len == NULL
      || !tree_fits_uhwi_p (len)
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
      && tree_fits_uhwi_p (len)
      && tree_to_uhwi (len) >= 1
      && tree_to_uhwi (len)
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

/* Helper to build a call to memcmp.  */

static tree
build_memcmp_call (tree s1, tree s2, tree n)
{
  tree tmp;

  if (!POINTER_TYPE_P (TREE_TYPE (s1)))
    s1 = gfc_build_addr_expr (pvoid_type_node, s1);
  else
    s1 = fold_convert (pvoid_type_node, s1);

  if (!POINTER_TYPE_P (TREE_TYPE (s2)))
    s2 = gfc_build_addr_expr (pvoid_type_node, s2);
  else
    s2 = fold_convert (pvoid_type_node, s2);

  n = fold_convert (size_type_node, n);

  tmp = build_call_expr_loc (input_location,
			     builtin_decl_explicit (BUILT_IN_MEMCMP),
			     3, s1, s2, n);

  return fold_convert (integer_type_node, tmp);
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

  /* We can compare via memcpy if the strings are known to be equal
     in length and they are
     - kind=1
     - kind=4 and the comparison is for (in)equality.  */

  if (INTEGER_CST_P (len1) && INTEGER_CST_P (len2)
      && tree_int_cst_equal (len1, len2)
      && (kind == 1 || code == EQ_EXPR || code == NE_EXPR))
    {
      tree tmp;
      tree chartype;

      chartype = gfc_get_char_type (kind);
      tmp = fold_build2_loc (input_location, MULT_EXPR, TREE_TYPE(len1),
			     fold_convert (TREE_TYPE(len1),
					   TYPE_SIZE_UNIT(chartype)),
			     len1);
      return build_memcmp_call (str1, str2, tmp);
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

  if (!VAR_P (base_object))
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
conv_function_val (gfc_se * se, gfc_symbol * sym, gfc_expr * expr,
		   gfc_actual_arglist *actual_args)
{
  tree tmp;

  if (gfc_is_proc_ptr_comp (expr))
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
	sym->backend_decl = gfc_get_extern_function_decl (sym, actual_args);

      TREE_USED (sym->backend_decl) = 1;

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

  /* If the argument is a pass-by-value scalar, use the value as is.  */
  else if (!sym->attr.dimension && sym->attr.value)
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
	break;

      case REF_COMPONENT:
      case REF_INQUIRY:
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

    case GFC_ISYM_LEN_TRIM:
      new_expr = gfc_copy_expr (arg1);
      gfc_apply_interface_mapping_to_expr (mapping, new_expr);

      if (!new_expr)
	return false;

      gfc_replace_expr (arg1, new_expr);
      return true;

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
	return false;

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
  f = gfc_sym_get_dummy_args (map_expr->symtree->n.sym);

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
	    && expr->value.function.actual
	    && expr->value.function.actual->expr
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
    case EXPR_UNKNOWN:
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
gfc_conv_subref_array_arg (gfc_se *se, gfc_expr * expr, int g77,
			   sym_intent intent, bool formal_ptr,
			   const gfc_symbol *fsym, const char *proc_name,
			   gfc_symbol *sym, bool check_contiguous)
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
  gfc_se work_se;
  gfc_se *parmse;
  bool pass_optional;

  pass_optional = fsym && fsym->attr.optional && sym && sym->attr.optional;

  if (pass_optional || check_contiguous)
    {
      gfc_init_se (&work_se, NULL);
      parmse = &work_se;
    }
  else
    parmse = se;

  if (gfc_option.rtcheck & GFC_RTCHECK_ARRAY_TEMPS)
    {
      /* We will create a temporary array, so let us warn.  */
      char * msg;

      if (fsym && proc_name)
	msg = xasprintf ("An array temporary was created for argument "
			     "'%s' of procedure '%s'", fsym->name, proc_name);
      else
	msg = xasprintf ("An array temporary was created");

      tmp = build_int_cst (logical_type_node, 1);
      gfc_trans_runtime_check (false, true, tmp, &parmse->pre,
			       &expr->where, msg);
      free (msg);
    }

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

  /* Reset the offset for the function call since the loop
     is zero based on the data pointer.  Note that the temp
     comes first in the loop chain since it is added second.  */
  if (gfc_is_class_array_function (expr))
    {
      tmp = loop.ss->loop_chain->info->data.array.descriptor;
      gfc_conv_descriptor_offset_set (&loop.pre, tmp,
				      gfc_index_zero_node);
    }

  gfc_conv_tmp_array_ref (&lse);

  if (intent != INTENT_OUT)
    {
      tmp = gfc_trans_scalar_assign (&lse, &rse, expr->ts, false, false);
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

  dimen = rse.ss->dimen;

  /* Skip the write-out loop for this case.  */
  if (gfc_is_class_array_function (expr))
    goto class_array_fcn;

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

  tmp = gfc_trans_scalar_assign (&lse, &rse, expr->ts, false, true);
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

class_array_fcn:

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

  /* Basically make this into

     if (present)
       {
	 if (contiguous)
	   {
	     pointer = a;
	   }
	 else
	   {
	     parmse->pre();
	     pointer = parmse->expr;
	   }
       }
     else
       pointer = NULL;

     foo (pointer);
     if (present && !contiguous)
	   se->post();

     */

  if (pass_optional || check_contiguous)
    {
      tree type;
      stmtblock_t else_block;
      tree pre_stmts, post_stmts;
      tree pointer;
      tree else_stmt;
      tree present_var = NULL_TREE;
      tree cont_var = NULL_TREE;
      tree post_cond;

      type = TREE_TYPE (parmse->expr);
      pointer = gfc_create_var (type, "arg_ptr");

      if (check_contiguous)
	{
	  gfc_se cont_se, array_se;
	  stmtblock_t if_block, else_block;
	  tree if_stmt, else_stmt;
	  mpz_t size;
	  bool size_set;

	  cont_var = gfc_create_var (boolean_type_node, "contiguous");

	  /* If the size is known to be one at compile-time, set
	     cont_var to true unconditionally.  This may look
	     inelegant, but we're only doing this during
	     optimization, so the statements will be optimized away,
	     and this saves complexity here.  */

	  size_set = gfc_array_size (expr, &size);
	  if (size_set && mpz_cmp_ui (size, 1) == 0)
	    {
	      gfc_add_modify (&se->pre, cont_var,
			      build_one_cst (boolean_type_node));
	    }
	  else
	    {
	      /* cont_var = is_contiguous (expr); .  */
	      gfc_init_se (&cont_se, parmse);
	      gfc_conv_is_contiguous_expr (&cont_se, expr);
	      gfc_add_block_to_block (&se->pre, &(&cont_se)->pre);
	      gfc_add_modify (&se->pre, cont_var, cont_se.expr);
	      gfc_add_block_to_block (&se->pre, &(&cont_se)->post);
	    }

	  if (size_set)
	    mpz_clear (size);

	  /* arrayse->expr = descriptor of a.  */
	  gfc_init_se (&array_se, se);
	  gfc_conv_expr_descriptor (&array_se, expr);
	  gfc_add_block_to_block (&se->pre, &(&array_se)->pre);
	  gfc_add_block_to_block (&se->pre, &(&array_se)->post);

	  /* if_stmt = { pointer = &a[0]; } .  */
	  gfc_init_block (&if_block);
	  tmp = gfc_conv_array_data (array_se.expr);
	  tmp = fold_convert (type, tmp);
	  gfc_add_modify (&if_block, pointer, tmp);
	  if_stmt = gfc_finish_block (&if_block);

	  /* else_stmt = { parmse->pre(); pointer = parmse->expr; } .  */
	  gfc_init_block (&else_block);
	  gfc_add_block_to_block (&else_block, &parmse->pre);
	  gfc_add_modify (&else_block, pointer, parmse->expr);
	  else_stmt = gfc_finish_block (&else_block);

	  /* And put the above into an if statement.  */
	  pre_stmts = fold_build3_loc (input_location, COND_EXPR, void_type_node,
				       gfc_likely (cont_var,
						   PRED_FORTRAN_CONTIGUOUS),
				       if_stmt, else_stmt);
	}
      else
	{
	  /* pointer = pramse->expr;  .  */
	  gfc_add_modify (&parmse->pre, pointer, parmse->expr);
	  pre_stmts = gfc_finish_block (&parmse->pre);
	}

      if (pass_optional)
	{
	  present_var = gfc_create_var (boolean_type_node, "present");

	  /* present_var = present(sym); .  */
	  tmp = gfc_conv_expr_present (sym);
	  tmp = fold_convert (boolean_type_node, tmp);
	  gfc_add_modify (&se->pre, present_var, tmp);

	  /* else_stmt = { pointer = NULL; } .  */
	  gfc_init_block (&else_block);
	  gfc_add_modify (&else_block, pointer, build_int_cst (type, 0));
	  else_stmt = gfc_finish_block (&else_block);

	  tmp = fold_build3_loc (input_location, COND_EXPR, void_type_node,
				 gfc_likely (present_var,
					     PRED_FORTRAN_ABSENT_DUMMY),
				 pre_stmts, else_stmt);
	  gfc_add_expr_to_block (&se->pre, tmp);
	}
      else
	gfc_add_expr_to_block (&se->pre, pre_stmts);

      post_stmts = gfc_finish_block (&parmse->post);

      /* Put together the post stuff, plus the optional
	 deallocation.  */
      if (check_contiguous)
	{
	  /* !cont_var.  */
	  tmp = fold_build2_loc (input_location, EQ_EXPR, boolean_type_node,
				 cont_var,
				 build_zero_cst (boolean_type_node));
	  tmp = gfc_unlikely (tmp, PRED_FORTRAN_CONTIGUOUS);

	  if (pass_optional)
	    {
	      tree present_likely = gfc_likely (present_var,
						PRED_FORTRAN_ABSENT_DUMMY);
	      post_cond = fold_build2_loc (input_location, TRUTH_ANDIF_EXPR,
					   boolean_type_node, present_likely,
					   tmp);
	    }
	  else
	    post_cond = tmp;
	}
      else
	{
	  gcc_assert (pass_optional);
	  post_cond = present_var;
	}

      tmp = fold_build3_loc (input_location, COND_EXPR, void_type_node, post_cond,
			     post_stmts, build_empty_stmt (input_location));
      gfc_add_expr_to_block (&se->post, tmp);
      se->expr = pointer;
    }

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
  if (strcmp (name, "%VAL") == 0)
    gfc_conv_expr (se, expr);
  else if (strcmp (name, "%LOC") == 0)
    {
      gfc_conv_expr_reference (se, expr);
      se->expr = gfc_build_addr_expr (NULL, se->expr);
    }
  else if (strcmp (name, "%REF") == 0)
    gfc_conv_expr_reference (se, expr);
  else
    gfc_error ("Unknown argument list function at %L", &expr->where);
}


/* This function tells whether the middle-end representation of the expression
   E given as input may point to data otherwise accessible through a variable
   (sub-)reference.
   It is assumed that the only expressions that may alias are variables,
   and array constructors if ARRAY_MAY_ALIAS is true and some of its elements
   may alias.
   This function is used to decide whether freeing an expression's allocatable
   components is safe or should be avoided.

   If ARRAY_MAY_ALIAS is true, an array constructor may alias if some of
   its elements are copied from a variable.  This ARRAY_MAY_ALIAS trick
   is necessary because for array constructors, aliasing depends on how
   the array is used:
    - If E is an array constructor used as argument to an elemental procedure,
      the array, which is generated through shallow copy by the scalarizer,
      is used directly and can alias the expressions it was copied from.
    - If E is an array constructor used as argument to a non-elemental
      procedure,the scalarizer is used in gfc_conv_expr_descriptor to generate
      the array as in the previous case, but then that array is used
      to initialize a new descriptor through deep copy.  There is no alias
      possible in that case.
   Thus, the ARRAY_MAY_ALIAS flag is necessary to distinguish the two cases
   above.  */

static bool
expr_may_alias_variables (gfc_expr *e, bool array_may_alias)
{
  gfc_constructor *c;

  if (e->expr_type == EXPR_VARIABLE)
    return true;
  else if (e->expr_type == EXPR_FUNCTION)
    {
      gfc_symbol *proc_ifc = gfc_get_proc_ifc_for_expr (e);

      if (proc_ifc->result != NULL
	  && ((proc_ifc->result->ts.type == BT_CLASS
	       && proc_ifc->result->ts.u.derived->attr.is_class
	       && CLASS_DATA (proc_ifc->result)->attr.class_pointer)
	      || proc_ifc->result->attr.pointer))
	return true;
      else
	return false;
    }
  else if (e->expr_type != EXPR_ARRAY || !array_may_alias)
    return false;

  for (c = gfc_constructor_first (e->value.constructor);
       c; c = gfc_constructor_next (c))
    if (c->expr
	&& expr_may_alias_variables (c->expr, array_may_alias))
      return true;

  return false;
}


/* A helper function to set the dtype for unallocated or unassociated
   entities.  */

static void
set_dtype_for_unallocated (gfc_se *parmse, gfc_expr *e)
{
  tree tmp;
  tree desc;
  tree cond;
  tree type;
  stmtblock_t block;

  /* TODO Figure out how to handle optional dummies.  */
  if (e && e->expr_type == EXPR_VARIABLE
      && e->symtree->n.sym->attr.optional)
    return;

  desc = parmse->expr;
  if (desc == NULL_TREE)
    return;

  if (POINTER_TYPE_P (TREE_TYPE (desc)))
    desc = build_fold_indirect_ref_loc (input_location, desc);

  if (!GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (desc)))
    return;

  gfc_init_block (&block);
  tmp = gfc_conv_descriptor_data_get (desc);
  cond = fold_build2_loc (input_location, EQ_EXPR,
			  logical_type_node, tmp,
			  build_int_cst (TREE_TYPE (tmp), 0));
  tmp = gfc_conv_descriptor_dtype (desc);
  type = gfc_get_element_type (TREE_TYPE (desc));
  tmp = fold_build2_loc (input_location, MODIFY_EXPR,
			 TREE_TYPE (tmp), tmp,
			 gfc_get_dtype_rank_type (e->rank, type));
  gfc_add_expr_to_block (&block, tmp);
  cond = build3_v (COND_EXPR, cond,
		   gfc_finish_block (&block),
		   build_empty_stmt (input_location));
  gfc_add_expr_to_block (&parmse->pre, cond);
}



/* Provide an interface between gfortran array descriptors and the F2018:18.4
   ISO_Fortran_binding array descriptors. */

static void
gfc_conv_gfc_desc_to_cfi_desc (gfc_se *parmse, gfc_expr *e, gfc_symbol *fsym)
{
  tree tmp;
  tree cfi_desc_ptr;
  tree gfc_desc_ptr;
  tree type;
  tree cond;
  tree desc_attr;
  int attribute;
  int cfi_attribute;
  symbol_attribute attr = gfc_expr_attr (e);

  /* If this is a full array or a scalar, the allocatable and pointer
     attributes can be passed. Otherwise it is 'CFI_attribute_other'*/
  attribute = 2;
  if (!e->rank || gfc_get_full_arrayspec_from_expr (e))
    {
      if (attr.pointer)
	attribute = 0;
      else if (attr.allocatable)
	attribute = 1;
    }

  /* If the formal argument is assumed shape and neither a pointer nor
     allocatable, it is unconditionally CFI_attribute_other.  */
  if (fsym->as->type == AS_ASSUMED_SHAPE
      && !fsym->attr.pointer && !fsym->attr.allocatable)
   cfi_attribute = 2;
  else
   cfi_attribute = attribute;

  if (e->rank != 0)
    {
      parmse->force_no_tmp = 1;
      if (fsym->attr.contiguous
	  && !gfc_is_simply_contiguous (e, false, true))
	gfc_conv_subref_array_arg (parmse, e, false, fsym->attr.intent,
				   fsym->attr.pointer);
      else
	gfc_conv_expr_descriptor (parmse, e);

      if (POINTER_TYPE_P (TREE_TYPE (parmse->expr)))
	parmse->expr = build_fold_indirect_ref_loc (input_location,
						    parmse->expr);
      bool is_artificial = (INDIRECT_REF_P (parmse->expr)
			    ? DECL_ARTIFICIAL (TREE_OPERAND (parmse->expr, 0))
			    : DECL_ARTIFICIAL (parmse->expr));

      /* Unallocated allocatable arrays and unassociated pointer arrays
	 need their dtype setting if they are argument associated with
	 assumed rank dummies.  */
      if (fsym && fsym->as
	  && (gfc_expr_attr (e).pointer
	      || gfc_expr_attr (e).allocatable))
	set_dtype_for_unallocated (parmse, e);

      /* All the temporary descriptors are marked as DECL_ARTIFICIAL. If
	 the expression type is different from the descriptor type, then
	 the offset must be found (eg. to a component ref or substring)
	 and the dtype updated.  Assumed type entities are only allowed
	 to be dummies in Fortran. They therefore lack the decl specific
	 appendiges and so must be treated differently from other fortran
	 entities passed to CFI descriptors in the interface decl.  */
      type = e->ts.type != BT_ASSUMED ? gfc_typenode_for_spec (&e->ts) :
					NULL_TREE;

      if (type && is_artificial
	  && type != gfc_get_element_type (TREE_TYPE (parmse->expr)))
	{
	  /* Obtain the offset to the data.  */
	  gfc_get_dataptr_offset (&parmse->pre, parmse->expr, parmse->expr,
				  gfc_index_zero_node, true, e);

	  /* Update the dtype.  */
	  gfc_add_modify (&parmse->pre,
			  gfc_conv_descriptor_dtype (parmse->expr),
			  gfc_get_dtype_rank_type (e->rank, type));
	}
      else if (type == NULL_TREE
	       || (!is_subref_array (e) && !is_artificial))
	{
	  /* Make sure that the span is set for expressions where it
	     might not have been done already.  */
	  tmp = gfc_conv_descriptor_elem_len (parmse->expr);
	  tmp = fold_convert (gfc_array_index_type, tmp);
	  gfc_conv_descriptor_span_set (&parmse->pre, parmse->expr, tmp);
	}
    }
  else
    {
      gfc_conv_expr (parmse, e);

      if (POINTER_TYPE_P (TREE_TYPE (parmse->expr)))
	parmse->expr = build_fold_indirect_ref_loc (input_location,
						    parmse->expr);

      parmse->expr = gfc_conv_scalar_to_descriptor (parmse,
						    parmse->expr, attr);
    }

  /* Set the CFI attribute field through a temporary value for the
     gfc attribute.  */
  desc_attr = gfc_conv_descriptor_attribute (parmse->expr);
  tmp = fold_build2_loc (input_location, MODIFY_EXPR,
			 void_type_node, desc_attr,
			 build_int_cst (TREE_TYPE (desc_attr), cfi_attribute));
  gfc_add_expr_to_block (&parmse->pre, tmp);

  /* Now pass the gfc_descriptor by reference.  */
  parmse->expr = gfc_build_addr_expr (NULL_TREE, parmse->expr);

  /* Variables to point to the gfc and CFI descriptors; cfi = NULL implies
     that the CFI descriptor is allocated by the gfor_fndecl_gfc_to_cfi call.  */
  gfc_desc_ptr = parmse->expr;
  cfi_desc_ptr = gfc_create_var (pvoid_type_node, "cfi");
  gfc_add_modify (&parmse->pre, cfi_desc_ptr, null_pointer_node);

  /* Allocate the CFI descriptor itself and fill the fields.  */
  tmp = gfc_build_addr_expr (NULL_TREE, cfi_desc_ptr);
  tmp = build_call_expr_loc (input_location,
			     gfor_fndecl_gfc_to_cfi, 2, tmp, gfc_desc_ptr);
  gfc_add_expr_to_block (&parmse->pre, tmp);

  /* Now set the gfc descriptor attribute.  */
  tmp = fold_build2_loc (input_location, MODIFY_EXPR,
			 void_type_node, desc_attr,
			 build_int_cst (TREE_TYPE (desc_attr), attribute));
  gfc_add_expr_to_block (&parmse->pre, tmp);

  /* The CFI descriptor is passed to the bind_C procedure.  */
  parmse->expr = cfi_desc_ptr;

  /* Free the CFI descriptor.  */
  tmp = gfc_call_free (cfi_desc_ptr);
  gfc_prepend_expr_to_block (&parmse->post, tmp);

  /* Transfer values back to gfc descriptor.  */
  tmp = gfc_build_addr_expr (NULL_TREE, parmse->expr);
  tmp = build_call_expr_loc (input_location,
			     gfor_fndecl_cfi_to_gfc, 2, gfc_desc_ptr, tmp);
  gfc_prepend_expr_to_block (&parmse->post, tmp);

  /* Deal with an optional dummy being passed to an optional formal arg
     by finishing the pre and post blocks and making their execution
     conditional on the dummy being present.  */
  if (fsym->attr.optional && e->expr_type == EXPR_VARIABLE
      && e->symtree->n.sym->attr.optional)
    {
      cond = gfc_conv_expr_present (e->symtree->n.sym);
      tmp = fold_build2 (MODIFY_EXPR, void_type_node,
			 cfi_desc_ptr,
			 build_int_cst (pvoid_type_node, 0));
      tmp = build3_v (COND_EXPR, cond,
		      gfc_finish_block (&parmse->pre), tmp);
      gfc_add_expr_to_block (&parmse->pre, tmp);
      tmp = build3_v (COND_EXPR, cond,
		      gfc_finish_block (&parmse->post),
		      build_empty_stmt (input_location));
      gfc_add_expr_to_block (&parmse->post, tmp);
    }
}


/* Generate code for a procedure call.  Note can return se->post != NULL.
   If se->direct_byref is set then se->expr contains the return parameter.
   Return nonzero, if the call has alternate specifiers.
   'expr' is only needed for procedure pointer components.  */

int
gfc_conv_procedure_call (gfc_se * se, gfc_symbol * sym,
			 gfc_actual_arglist * args, gfc_expr * expr,
			 vec<tree, va_gc> *append_args)
{
  gfc_interface_mapping mapping;
  vec<tree, va_gc> *arglist;
  vec<tree, va_gc> *retargs;
  tree tmp;
  tree fntype;
  gfc_se parmse;
  gfc_array_info *info;
  int byref;
  int parm_kind;
  tree type;
  tree var;
  tree len;
  tree base_object;
  vec<tree, va_gc> *stringargs;
  vec<tree, va_gc> *optionalargs;
  tree result = NULL;
  gfc_formal_arglist *formal;
  gfc_actual_arglist *arg;
  int has_alternate_specifier = 0;
  bool need_interface_mapping;
  bool callee_alloc;
  bool ulim_copy;
  gfc_typespec ts;
  gfc_charlen cl;
  gfc_expr *e;
  gfc_symbol *fsym;
  stmtblock_t post;
  enum {MISSING = 0, ELEMENTAL, SCALAR, SCALAR_POINTER, ARRAY};
  gfc_component *comp = NULL;
  int arglen;
  unsigned int argc;

  arglist = NULL;
  retargs = NULL;
  stringargs = NULL;
  optionalargs = NULL;
  var = NULL_TREE;
  len = NULL_TREE;
  gfc_clear_ts (&ts);

  comp = gfc_get_proc_ptr_comp (expr);

  bool elemental_proc = (comp
			 && comp->ts.interface
			 && comp->ts.interface->attr.elemental)
			|| (comp && comp->attr.elemental)
			|| sym->attr.elemental;

  if (se->ss != NULL)
    {
      if (!elemental_proc)
	{
	  gcc_assert (se->ss->info->type == GFC_SS_FUNCTION);
	  if (se->ss->info->useflags)
	    {
	      gcc_assert ((!comp && gfc_return_by_reference (sym)
			   && sym->result->attr.dimension)
			  || (comp && comp->attr.dimension)
			  || gfc_is_class_array_function (expr));
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
      formal = gfc_sym_get_dummy_args (sym);
      need_interface_mapping = sym->attr.dimension ||
			       (sym->ts.type == BT_CHARACTER
				&& sym->ts.u.cl->length
				&& sym->ts.u.cl->length->expr_type
				   != EXPR_CONSTANT);
    }
  else
    {
      formal = comp->ts.interface ? comp->ts.interface->formal : NULL;
      need_interface_mapping = comp->attr.dimension ||
			       (comp->ts.type == BT_CHARACTER
				&& comp->ts.u.cl->length
				&& comp->ts.u.cl->length->expr_type
				   != EXPR_CONSTANT);
    }

  base_object = NULL_TREE;
  /* For _vprt->_copy () routines no formal symbol is present.  Nevertheless
     is the third and fourth argument to such a function call a value
     denoting the number of elements to copy (i.e., most of the time the
     length of a deferred length string).  */
  ulim_copy = (formal == NULL)
	       && UNLIMITED_POLY (sym)
	       && comp && (strcmp ("_copy", comp->name) == 0);

  /* Evaluate the arguments.  */
  for (arg = args, argc = 0; arg != NULL;
       arg = arg->next, formal = formal ? formal->next : NULL, ++argc)
    {
      bool finalized = false;
      bool non_unity_length_string = false;

      e = arg->expr;
      fsym = formal ? formal->sym : NULL;
      parm_kind = MISSING;

      if (fsym && fsym->ts.type == BT_CHARACTER && fsym->ts.u.cl
	  && (!fsym->ts.u.cl->length
	      || fsym->ts.u.cl->length->expr_type != EXPR_CONSTANT
	      || mpz_cmp_si (fsym->ts.u.cl->length->value.integer, 1) != 0))
	non_unity_length_string = true;

      /* If the procedure requires an explicit interface, the actual
	 argument is passed according to the corresponding formal
	 argument.  If the corresponding formal argument is a POINTER,
	 ALLOCATABLE or assumed shape, we do not use g77's calling
	 convention, and pass the address of the array descriptor
	 instead.  Otherwise we use g77's calling convention, in other words
	 pass the array data pointer without descriptor.  */
      bool nodesc_arg = fsym != NULL
			&& !(fsym->attr.pointer || fsym->attr.allocatable)
			&& fsym->as
			&& fsym->as->type != AS_ASSUMED_SHAPE
			&& fsym->as->type != AS_ASSUMED_RANK;
      if (comp)
	nodesc_arg = nodesc_arg || !comp->attr.always_explicit;
      else
	nodesc_arg = nodesc_arg || !sym->attr.always_explicit;

      /* Class array expressions are sometimes coming completely unadorned
	 with either arrayspec or _data component.  Correct that here.
	 OOP-TODO: Move this to the frontend.  */
      if (e && e->expr_type == EXPR_VARIABLE
	    && !e->ref
	    && e->ts.type == BT_CLASS
	    && (CLASS_DATA (e)->attr.codimension
		|| CLASS_DATA (e)->attr.dimension))
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
	      gfc_init_se (&parmse, NULL);

	      /* For scalar arguments with VALUE attribute which are passed by
		 value, pass "0" and a hidden argument gives the optional
		 status.  */
	      if (fsym && fsym->attr.optional && fsym->attr.value
		  && !fsym->attr.dimension && fsym->ts.type != BT_CHARACTER
		  && fsym->ts.type != BT_CLASS && fsym->ts.type != BT_DERIVED)
		{
		  parmse.expr = fold_convert (gfc_sym_type (fsym),
					      integer_zero_node);
		  vec_safe_push (optionalargs, boolean_false_node);
		}
	      else
		{
		  /* Pass a NULL pointer for an absent arg.  */
		  parmse.expr = null_pointer_node;
		  if (arg->missing_arg_type == BT_CHARACTER)
		    parmse.string_length = build_int_cst (gfc_charlen_type_node,
							  0);
		}
	    }
	}
      else if (arg->expr->expr_type == EXPR_NULL
	       && fsym && !fsym->attr.pointer
	       && (fsym->ts.type != BT_CLASS
		   || !CLASS_DATA (fsym)->attr.class_pointer))
	{
	  /* Pass a NULL pointer to denote an absent arg.  */
	  gcc_assert (fsym->attr.optional && !fsym->attr.allocatable
		      && (fsym->ts.type != BT_CLASS
			  || !CLASS_DATA (fsym)->attr.allocatable));
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
	  gfc_conv_derived_to_class (&parmse, e, fsym->ts, NULL,
				     fsym->attr.optional
				     && e->expr_type == EXPR_VARIABLE
				     && e->symtree->n.sym->attr.optional,
				     CLASS_DATA (fsym)->attr.class_pointer
				     || CLASS_DATA (fsym)->attr.allocatable);
	}
      else if (UNLIMITED_POLY (fsym) && e->ts.type != BT_CLASS)
	{
	  /* The intrinsic type needs to be converted to a temporary
	     CLASS object for the unlimited polymorphic formal.  */
	  gfc_init_se (&parmse, se);
	  gfc_conv_intrinsic_to_class (&parmse, e, fsym->ts);
	}
      else if (se->ss && se->ss->info->useflags)
	{
	  gfc_ss *ss;

	  ss = se->ss;

	  /* An elemental function inside a scalarized loop.  */
	  gfc_init_se (&parmse, se);
	  parm_kind = ELEMENTAL;

	  /* When no fsym is present, ulim_copy is set and this is a third or
	     fourth argument, use call-by-value instead of by reference to
	     hand the length properties to the copy routine (i.e., most of the
	     time this will be a call to a __copy_character_* routine where the
	     third and fourth arguments are the lengths of a deferred length
	     char array).  */
	  if ((fsym && fsym->attr.value)
	      || (ulim_copy && (argc == 2 || argc == 3)))
	    gfc_conv_expr (&parmse, e);
	  else
	    gfc_conv_expr_reference (&parmse, e);

	  if (e->ts.type == BT_CHARACTER && !e->rank
	      && e->expr_type == EXPR_FUNCTION)
	    parmse.expr = build_fold_indirect_ref_loc (input_location,
						       parmse.expr);

	  if (fsym && fsym->ts.type == BT_DERIVED
	      && gfc_is_class_container_ref (e))
	    {
	      parmse.expr = gfc_class_data_get (parmse.expr);

	      if (fsym->attr.optional && e->expr_type == EXPR_VARIABLE
		  && e->symtree->n.sym->attr.optional)
		{
		  tree cond = gfc_conv_expr_present (e->symtree->n.sym);
		  parmse.expr = build3_loc (input_location, COND_EXPR,
					TREE_TYPE (parmse.expr),
					cond, parmse.expr,
					fold_convert (TREE_TYPE (parmse.expr),
						      null_pointer_node));
		}
	    }

	  /* If we are passing an absent array as optional dummy to an
	     elemental procedure, make sure that we pass NULL when the data
	     pointer is NULL.  We need this extra conditional because of
	     scalarization which passes arrays elements to the procedure,
	     ignoring the fact that the array can be absent/unallocated/...  */
	  if (ss->info->can_be_null_ref && ss->info->type != GFC_SS_REFERENCE)
	    {
	      tree descriptor_data;

	      descriptor_data = ss->info->data.array.data;
	      tmp = fold_build2_loc (input_location, EQ_EXPR, logical_type_node,
				     descriptor_data,
				     fold_convert (TREE_TYPE (descriptor_data),
						   null_pointer_node));
	      parmse.expr
		= fold_build3_loc (input_location, COND_EXPR,
				   TREE_TYPE (parmse.expr),
				   gfc_unlikely (tmp, PRED_FORTRAN_ABSENT_DUMMY),
				   fold_convert (TREE_TYPE (parmse.expr),
						 null_pointer_node),
				   parmse.expr);
	    }

	  /* The scalarizer does not repackage the reference to a class
	     array - instead it returns a pointer to the data element.  */
	  if (fsym && fsym->ts.type == BT_CLASS && e->ts.type == BT_CLASS)
	    gfc_conv_class_to_class (&parmse, e, fsym->ts, true,
				     fsym->attr.intent != INTENT_IN
				     && (CLASS_DATA (fsym)->attr.class_pointer
					 || CLASS_DATA (fsym)->attr.allocatable),
				     fsym->attr.optional
				     && e->expr_type == EXPR_VARIABLE
				     && e->symtree->n.sym->attr.optional,
				     CLASS_DATA (fsym)->attr.class_pointer
				     || CLASS_DATA (fsym)->attr.allocatable);
	}
      else
	{
	  bool scalar;
	  gfc_ss *argss;

	  gfc_init_se (&parmse, NULL);

	  /* Check whether the expression is a scalar or not; we cannot use
	     e->rank as it can be nonzero for functions arguments.  */
	  argss = gfc_walk_expr (e);
	  scalar = argss == gfc_ss_terminator;
	  if (!scalar)
	    gfc_free_ss_chain (argss);

	  /* Special handling for passing scalar polymorphic coarrays;
	     otherwise one passes "class->_data.data" instead of "&class".  */
	  if (e->rank == 0 && e->ts.type == BT_CLASS
	      && fsym && fsym->ts.type == BT_CLASS
	      && CLASS_DATA (fsym)->attr.codimension
	      && !CLASS_DATA (fsym)->attr.dimension)
	    {
	      gfc_add_class_array_ref (e);
              parmse.want_coarray = 1;
	      scalar = false;
	    }

	  /* A scalar or transformational function.  */
	  if (scalar)
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

	      else if (sym->attr.is_bind_c && e
		       && (is_CFI_desc (fsym, NULL)
			   || non_unity_length_string))
		/* Implement F2018, C.12.6.1: paragraph (2).  */
		gfc_conv_gfc_desc_to_cfi_desc (&parmse, e, fsym);

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
		    {
		    gfc_conv_expr (&parmse, e);
		    if (fsym->attr.optional
			&& fsym->ts.type != BT_CLASS
			&& fsym->ts.type != BT_DERIVED)
		      {
			if (e->expr_type != EXPR_VARIABLE
			    || !e->symtree->n.sym->attr.optional
			    || e->ref != NULL)
			  vec_safe_push (optionalargs, boolean_true_node);
			else
			  {
			    tmp = gfc_conv_expr_present (e->symtree->n.sym);
			    if (!e->symtree->n.sym->attr.value)
			      parmse.expr
				= fold_build3_loc (input_location, COND_EXPR,
					TREE_TYPE (parmse.expr),
					tmp, parmse.expr,
					fold_convert (TREE_TYPE (parmse.expr),
						      integer_zero_node));

			    vec_safe_push (optionalargs,
					   fold_convert (boolean_type_node,
							 tmp));
			  }
		      }
		    }
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
		  if (e->ts.type == BT_CLASS && fsym
		      && fsym->ts.type == BT_CLASS
		      && (!CLASS_DATA (fsym)->as
			  || CLASS_DATA (fsym)->as->type != AS_ASSUMED_RANK)
		      && CLASS_DATA (e)->attr.codimension)
		    {
		      gcc_assert (!CLASS_DATA (fsym)->attr.codimension);
		      gcc_assert (!CLASS_DATA (fsym)->as);
		      gfc_add_class_array_ref (e);
		      parmse.want_coarray = 1;
		      gfc_conv_expr_reference (&parmse, e);
		      class_scalar_coarray_to_class (&parmse, e, fsym->ts,
				     fsym->attr.optional
				     && e->expr_type == EXPR_VARIABLE);
		    }
		  else if (e->ts.type == BT_CLASS && fsym
			   && fsym->ts.type == BT_CLASS
			   && !CLASS_DATA (fsym)->as
			   && !CLASS_DATA (e)->as
			   && strcmp (fsym->ts.u.derived->name,
				      e->ts.u.derived->name))
		    {
		      type = gfc_typenode_for_spec (&fsym->ts);
		      var = gfc_create_var (type, fsym->name);
		      gfc_conv_expr (&parmse, e);
		      if (fsym->attr.optional
			  && e->expr_type == EXPR_VARIABLE
			  && e->symtree->n.sym->attr.optional)
			{
			  stmtblock_t block;
			  tree cond;
			  tmp = gfc_build_addr_expr (NULL_TREE, parmse.expr);
			  cond = fold_build2_loc (input_location, NE_EXPR,
						  logical_type_node, tmp,
						  fold_convert (TREE_TYPE (tmp),
							    null_pointer_node));
			  gfc_start_block (&block);
			  gfc_add_modify (&block, var,
					  fold_build1_loc (input_location,
							   VIEW_CONVERT_EXPR,
							   type, parmse.expr));
			  gfc_add_expr_to_block (&parmse.pre,
				 fold_build3_loc (input_location,
					 COND_EXPR, void_type_node,
					 cond, gfc_finish_block (&block),
					 build_empty_stmt (input_location)));
			  parmse.expr = gfc_build_addr_expr (NULL_TREE, var);
			  parmse.expr = build3_loc (input_location, COND_EXPR,
					 TREE_TYPE (parmse.expr),
					 cond, parmse.expr,
					 fold_convert (TREE_TYPE (parmse.expr),
						       null_pointer_node));
			}
		      else
			{
			  /* Since the internal representation of unlimited
			     polymorphic expressions includes an extra field
			     that other class objects do not, a cast to the
			     formal type does not work.  */
			  if (!UNLIMITED_POLY (e) && UNLIMITED_POLY (fsym))
			    {
			      tree efield;

			      /* Set the _data field.  */
			      tmp = gfc_class_data_get (var);
			      efield = fold_convert (TREE_TYPE (tmp),
					gfc_class_data_get (parmse.expr));
			      gfc_add_modify (&parmse.pre, tmp, efield);

			      /* Set the _vptr field.  */
			      tmp = gfc_class_vptr_get (var);
			      efield = fold_convert (TREE_TYPE (tmp),
					gfc_class_vptr_get (parmse.expr));
			      gfc_add_modify (&parmse.pre, tmp, efield);

			      /* Set the _len field.  */
			      tmp = gfc_class_len_get (var);
			      gfc_add_modify (&parmse.pre, tmp,
					      build_int_cst (TREE_TYPE (tmp), 0));
			    }
			  else
			    {
			      tmp = fold_build1_loc (input_location,
						     VIEW_CONVERT_EXPR,
						     type, parmse.expr);
			      gfc_add_modify (&parmse.pre, var, tmp);
					      ;
			    }
			  parmse.expr = gfc_build_addr_expr (NULL_TREE, var);
			}
		    }
		  else
		    {
		      bool add_clobber;
		      add_clobber = fsym && fsym->attr.intent == INTENT_OUT
			&& !fsym->attr.allocatable && !fsym->attr.pointer
			&& !e->symtree->n.sym->attr.dimension
			&& !e->symtree->n.sym->attr.pointer
			/* See PR 41453.  */
			&& !e->symtree->n.sym->attr.dummy
			/* FIXME - PR 87395 and PR 41453  */
			&& e->symtree->n.sym->attr.save == SAVE_NONE
			&& !e->symtree->n.sym->attr.associate_var
			&& e->ts.type != BT_CHARACTER && e->ts.type != BT_DERIVED
			&& e->ts.type != BT_CLASS && !sym->attr.elemental;

		      gfc_conv_expr_reference (&parmse, e, add_clobber);
		    }
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
			&& ((CLASS_DATA (fsym)->as
			     && CLASS_DATA (fsym)->as->type == AS_ASSUMED_RANK)
			    || CLASS_DATA (e)->attr.dimension))
		    gfc_conv_class_to_class (&parmse, e, fsym->ts, false,
				     fsym->attr.intent != INTENT_IN
				     && (CLASS_DATA (fsym)->attr.class_pointer
					 || CLASS_DATA (fsym)->attr.allocatable),
				     fsym->attr.optional
				     && e->expr_type == EXPR_VARIABLE
				     && e->symtree->n.sym->attr.optional,
				     CLASS_DATA (fsym)->attr.class_pointer
				     || CLASS_DATA (fsym)->attr.allocatable);

		  /* If an ALLOCATABLE dummy argument has INTENT(OUT) and is
		     allocated on entry, it must be deallocated.  */
		  if (fsym && fsym->attr.intent == INTENT_OUT
		      && (fsym->attr.allocatable
			  || (fsym->ts.type == BT_CLASS
			      && CLASS_DATA (fsym)->attr.allocatable)))
		    {
		      stmtblock_t block;
		      tree ptr;

		      gfc_init_block  (&block);
		      ptr = parmse.expr;
		      if (e->ts.type == BT_CLASS)
			ptr = gfc_class_data_get (ptr);

		      tmp = gfc_deallocate_scalar_with_status (ptr, NULL_TREE,
							       NULL_TREE, true,
							       e, e->ts);
		      gfc_add_expr_to_block (&block, tmp);
		      tmp = fold_build2_loc (input_location, MODIFY_EXPR,
					     void_type_node, ptr,
					     null_pointer_node);
		      gfc_add_expr_to_block (&block, tmp);

		      if (fsym->ts.type == BT_CLASS && UNLIMITED_POLY (fsym))
			{
			  gfc_add_modify (&block, ptr,
					  fold_convert (TREE_TYPE (ptr),
							null_pointer_node));
			  gfc_add_expr_to_block (&block, tmp);
			}
		      else if (fsym->ts.type == BT_CLASS)
			{
			  gfc_symbol *vtab;
			  vtab = gfc_find_derived_vtab (fsym->ts.u.derived);
			  tmp = gfc_get_symbol_decl (vtab);
			  tmp = gfc_build_addr_expr (NULL_TREE, tmp);
			  ptr = gfc_class_vptr_get (parmse.expr);
			  gfc_add_modify (&block, ptr,
					  fold_convert (TREE_TYPE (ptr), tmp));
			  gfc_add_expr_to_block (&block, tmp);
			}

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

		  if (fsym && (fsym->ts.type == BT_DERIVED
			       || fsym->ts.type == BT_ASSUMED)
		      && e->ts.type == BT_CLASS
		      && !CLASS_DATA (e)->attr.dimension
		      && !CLASS_DATA (e)->attr.codimension)
		    {
		      parmse.expr = gfc_class_data_get (parmse.expr);
		      /* The result is a class temporary, whose _data component
			 must be freed to avoid a memory leak.  */
		      if (e->expr_type == EXPR_FUNCTION
			  && CLASS_DATA (e)->attr.allocatable)
			{
			  tree zero;

			  gfc_expr *var;

			  /* Borrow the function symbol to make a call to
			     gfc_add_finalizer_call and then restore it.  */
			  tmp = e->symtree->n.sym->backend_decl;
			  e->symtree->n.sym->backend_decl
					= TREE_OPERAND (parmse.expr, 0);
			  e->symtree->n.sym->attr.flavor = FL_VARIABLE;
			  var = gfc_lval_expr_from_sym (e->symtree->n.sym);
			  finalized = gfc_add_finalizer_call (&parmse.post,
							      var);
			  gfc_free_expr (var);
			  e->symtree->n.sym->backend_decl = tmp;
			  e->symtree->n.sym->attr.flavor = FL_PROCEDURE;

			  /* Then free the class _data.  */
			  zero = build_int_cst (TREE_TYPE (parmse.expr), 0);
			  tmp = fold_build2_loc (input_location, NE_EXPR,
						 logical_type_node,
						 parmse.expr, zero);
			  tmp = build3_v (COND_EXPR, tmp,
					  gfc_call_free (parmse.expr),
					  build_empty_stmt (input_location));
			  gfc_add_expr_to_block (&parmse.post, tmp);
			  gfc_add_modify (&parmse.post, parmse.expr, zero);
			}
		    }

		  /* Wrap scalar variable in a descriptor. We need to convert
		     the address of a pointer back to the pointer itself before,
		     we can assign it to the data field.  */

		  if (fsym && fsym->as && fsym->as->type == AS_ASSUMED_RANK
		      && fsym->ts.type != BT_CLASS && e->expr_type != EXPR_NULL)
		    {
		      tmp = parmse.expr;
		      if (TREE_CODE (tmp) == ADDR_EXPR)
			tmp = build_fold_indirect_ref_loc (input_location, tmp);
		      parmse.expr = gfc_conv_scalar_to_descriptor (&parmse, tmp,
								   fsym->attr);
		      parmse.expr = gfc_build_addr_expr (NULL_TREE,
							 parmse.expr);
		    }
		  else if (fsym && e->expr_type != EXPR_NULL
		      && ((fsym->attr.pointer
			   && fsym->attr.flavor != FL_PROCEDURE)
			  || (fsym->attr.proc_pointer
			      && !(e->expr_type == EXPR_VARIABLE
				   && e->symtree->n.sym->attr.dummy))
			  || (fsym->attr.proc_pointer
			      && e->expr_type == EXPR_VARIABLE
			      && gfc_is_proc_ptr_comp (e))
			  || (fsym->attr.allocatable
			      && fsym->attr.flavor != FL_PROCEDURE)))
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
		    && (CLASS_DATA (fsym)->attr.dimension
			|| CLASS_DATA (fsym)->attr.codimension))
	    {
	      /* Pass a class array.  */
	      parmse.use_offset = 1;
	      gfc_conv_expr_descriptor (&parmse, e);

	      /* If an ALLOCATABLE dummy argument has INTENT(OUT) and is
		 allocated on entry, it must be deallocated.  */
	      if (fsym->attr.intent == INTENT_OUT
		  && CLASS_DATA (fsym)->attr.allocatable)
		{
		  stmtblock_t block;
		  tree ptr;

		  gfc_init_block  (&block);
		  ptr = parmse.expr;
		  ptr = gfc_class_data_get (ptr);

		  tmp = gfc_deallocate_with_status (ptr, NULL_TREE,
						    NULL_TREE, NULL_TREE,
						    NULL_TREE, true, e,
						    GFC_CAF_COARRAY_NOCOARRAY);
		  gfc_add_expr_to_block (&block, tmp);
		  tmp = fold_build2_loc (input_location, MODIFY_EXPR,
					 void_type_node, ptr,
					 null_pointer_node);
		  gfc_add_expr_to_block (&block, tmp);
		  gfc_reset_vptr (&block, e);

		  if (fsym->attr.optional
		      && e->expr_type == EXPR_VARIABLE
		      && (!e->ref
			  || (e->ref->type == REF_ARRAY
			      && e->ref->u.ar.type != AR_FULL))
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

	      /* The conversion does not repackage the reference to a class
	         array - _data descriptor.  */
	      gfc_conv_class_to_class (&parmse, e, fsym->ts, false,
				     fsym->attr.intent != INTENT_IN
				     && (CLASS_DATA (fsym)->attr.class_pointer
					 || CLASS_DATA (fsym)->attr.allocatable),
				     fsym->attr.optional
				     && e->expr_type == EXPR_VARIABLE
				     && e->symtree->n.sym->attr.optional,
				     CLASS_DATA (fsym)->attr.class_pointer
				     || CLASS_DATA (fsym)->attr.allocatable);
	    }
	  else
	    {
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

	      if (sym->attr.is_bind_c && e
		  && (is_CFI_desc (fsym, NULL) || non_unity_length_string))
		/* Implement F2018, C.12.6.1: paragraph (2).  */
		gfc_conv_gfc_desc_to_cfi_desc (&parmse, e, fsym);

	      else if (e->expr_type == EXPR_VARIABLE
		    && is_subref_array (e)
		    && !(fsym && fsym->attr.pointer))
		/* The actual argument is a component reference to an
		   array of derived types.  In this case, the argument
		   is converted to a temporary, which is passed and then
		   written back after the procedure call.  */
		gfc_conv_subref_array_arg (&parmse, e, nodesc_arg,
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
		gfc_conv_subref_array_arg (&parmse, e, nodesc_arg,
					   fsym->attr.intent,
					   fsym->attr.pointer);

	      else if (gfc_is_class_array_function (e)
		       && fsym && fsym->ts.type == BT_DERIVED)
		/* See previous comment.  For function actual argument,
		   the write out is not needed so the intent is set as
		   intent in.  */
		{
		  e->must_finalize = 1;
		  gfc_conv_subref_array_arg (&parmse, e, nodesc_arg,
					     INTENT_IN, fsym->attr.pointer);
		}
	      else if (fsym && fsym->attr.contiguous
		       && !gfc_is_simply_contiguous (e, false, true)
		       && gfc_expr_is_variable (e))
		{
		  gfc_conv_subref_array_arg (&parmse, e, nodesc_arg,
					     fsym->attr.intent,
					     fsym->attr.pointer);
		}
	      else
		gfc_conv_array_parameter (&parmse, e, nodesc_arg, fsym,
					  sym->name, NULL);

	      /* Unallocated allocatable arrays and unassociated pointer arrays
		 need their dtype setting if they are argument associated with
		 assumed rank dummies.  */
	      if (!sym->attr.is_bind_c && e && fsym && fsym->as
		  && fsym->as->type == AS_ASSUMED_RANK)
		{
		  if (gfc_expr_attr (e).pointer
		      || gfc_expr_attr (e).allocatable)
		    set_dtype_for_unallocated (&parmse, e);
		  else if (e->expr_type == EXPR_VARIABLE
			   && e->ref
			   && e->ref->u.ar.type == AR_FULL
			   && e->symtree->n.sym->attr.dummy
			   && e->symtree->n.sym->as
			   && e->symtree->n.sym->as->type == AS_ASSUMED_SIZE)
		    {
		      tree minus_one;
		      tmp = build_fold_indirect_ref_loc (input_location,
							 parmse.expr);
		      minus_one = build_int_cst (gfc_array_index_type, -1);
		      gfc_conv_descriptor_ubound_set (&parmse.pre, tmp,
						      gfc_rank_cst[e->rank - 1],
						      minus_one);
 		    }
		}

	      /* If an ALLOCATABLE dummy argument has INTENT(OUT) and is
		 allocated on entry, it must be deallocated.  */
	      if (fsym && fsym->attr.allocatable
		  && fsym->attr.intent == INTENT_OUT)
		{
		  if (fsym->ts.type == BT_DERIVED
		      && fsym->ts.u.derived->attr.alloc_comp)
		  {
		    // deallocate the components first
		    tmp = gfc_deallocate_alloc_comp (fsym->ts.u.derived,
						     parmse.expr, e->rank);
		    if (tmp != NULL_TREE)
		      gfc_add_expr_to_block (&se->pre, tmp);
		  }

		  tmp = parmse.expr;
		  /* With bind(C), the actual argument is replaced by a bind-C
		     descriptor; in this case, the data component arrives here,
		     which shall not be dereferenced, but still freed and
		     nullified.  */
		  if  (TREE_TYPE(tmp) != pvoid_type_node)
		    tmp = build_fold_indirect_ref_loc (input_location,
						       parmse.expr);
		  if (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (tmp)))
		    tmp = gfc_conv_descriptor_data_get (tmp);
		  tmp = gfc_deallocate_with_status (tmp, NULL_TREE, NULL_TREE,
						    NULL_TREE, NULL_TREE, true,
						    e,
						    GFC_CAF_COARRAY_NOCOARRAY);
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
	     but do not always set fsym.
	     Also, it is necessary to pass a NULL pointer to library routines
	     which usually ignore optional arguments, so they can handle
	     these themselves.  */
	  if (e->expr_type == EXPR_VARIABLE
	      && e->symtree->n.sym->attr.optional
	      && (((e->rank != 0 && elemental_proc)
		   || e->representation.length || e->ts.type == BT_CHARACTER
		   || (e->rank != 0
		       && (fsym == NULL
			   || (fsym->as
			       && (fsym->as->type == AS_ASSUMED_SHAPE
				   || fsym->as->type == AS_ASSUMED_RANK
				   || fsym->as->type == AS_DEFERRED)))))
		  || se->ignore_optional))
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
	 deallocated for non-variable scalars, array arguments to elemental
	 procedures, and array arguments with descriptor to non-elemental
	 procedures.  As bounds information for descriptorless arrays is no
	 longer available here, they are dealt with in trans-array.c
	 (gfc_conv_array_parameter).  */
      if (e && (e->ts.type == BT_DERIVED || e->ts.type == BT_CLASS)
	    && e->ts.u.derived->attr.alloc_comp
	    && (e->rank == 0 || elemental_proc || !nodesc_arg)
	    && !expr_may_alias_variables (e, elemental_proc))
	{
	  int parm_rank;
	  /* It is known the e returns a structure type with at least one
	     allocatable component.  When e is a function, ensure that the
	     function is called once only by using a temporary variable.  */
	  if (!DECL_P (parmse.expr))
	    parmse.expr = gfc_evaluate_now_loc (input_location,
						parmse.expr, &se->pre);

	  if (fsym && fsym->attr.value)
	    tmp = parmse.expr;
	  else
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

	  if (e->ts.type == BT_DERIVED && fsym && fsym->ts.type == BT_CLASS)
	    {
	      /* The derived type is passed to gfc_deallocate_alloc_comp.
		 Therefore, class actuals can be handled correctly but derived
		 types passed to class formals need the _data component.  */
	      tmp = gfc_class_data_get (tmp);
	      if (!CLASS_DATA (fsym)->attr.dimension)
		tmp = build_fold_indirect_ref_loc (input_location, tmp);
	    }

	  if (e->expr_type == EXPR_OP
		&& e->value.op.op == INTRINSIC_PARENTHESES
		&& e->value.op.op1->expr_type == EXPR_VARIABLE)
	    {
	      tree local_tmp;
	      local_tmp = gfc_evaluate_now (tmp, &se->pre);
	      local_tmp = gfc_copy_alloc_comp (e->ts.u.derived, local_tmp, tmp,
					       parm_rank, 0);
	      gfc_add_expr_to_block (&se->post, local_tmp);
	    }

	  if (!finalized && !e->must_finalize)
	    {
	      if ((e->ts.type == BT_CLASS
		   && GFC_CLASS_TYPE_P (TREE_TYPE (tmp)))
		  || e->ts.type == BT_DERIVED)
		tmp = gfc_deallocate_alloc_comp (e->ts.u.derived, tmp,
						 parm_rank);
	      else if (e->ts.type == BT_CLASS)
		tmp = gfc_deallocate_alloc_comp (CLASS_DATA (e)->ts.u.derived,
						 tmp, parm_rank);
	      gfc_prepend_expr_to_block (&post, tmp);
	    }
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
		msg = xasprintf ("Allocatable actual argument '%s' is not "
				 "allocated or not present",
				 e->symtree->n.sym->name);
	      else if (attr.pointer
		       && (fsym == NULL || !fsym->attr.pointer))
		msg = xasprintf ("Pointer actual argument '%s' is not "
				 "associated or not present",
				 e->symtree->n.sym->name);
	      else if (attr.proc_pointer
		       && (fsym == NULL || !fsym->attr.proc_pointer))
		msg = xasprintf ("Proc-pointer actual argument '%s' is not "
				 "associated or not present",
				 e->symtree->n.sym->name);
	      else
		goto end_pointer_check;

	      present = gfc_conv_expr_present (e->symtree->n.sym);
	      type = TREE_TYPE (present);
	      present = fold_build2_loc (input_location, EQ_EXPR,
					 logical_type_node, present,
					 fold_convert (type,
						       null_pointer_node));
	      type = TREE_TYPE (parmse.expr);
	      null_ptr = fold_build2_loc (input_location, EQ_EXPR,
					  logical_type_node, parmse.expr,
					  fold_convert (type,
							null_pointer_node));
	      cond = fold_build2_loc (input_location, TRUTH_ORIF_EXPR,
				      logical_type_node, present, null_ptr);
	    }
          else
	    {
	      if (attr.allocatable
		  && (fsym == NULL || !fsym->attr.allocatable))
		msg = xasprintf ("Allocatable actual argument '%s' is not "
				 "allocated", e->symtree->n.sym->name);
	      else if (attr.pointer
		       && (fsym == NULL || !fsym->attr.pointer))
		msg = xasprintf ("Pointer actual argument '%s' is not "
				 "associated", e->symtree->n.sym->name);
	      else if (attr.proc_pointer
		       && (fsym == NULL || !fsym->attr.proc_pointer))
		msg = xasprintf ("Proc-pointer actual argument '%s' is not "
				 "associated", e->symtree->n.sym->name);
	      else
		goto end_pointer_check;

	      tmp = parmse.expr;

	      /* If the argument is passed by value, we need to strip the
		 INDIRECT_REF.  */
	      if (!POINTER_TYPE_P (TREE_TYPE (parmse.expr)))
		tmp = gfc_build_addr_expr (NULL_TREE, tmp);

	      cond = fold_build2_loc (input_location, EQ_EXPR,
				      logical_type_node, tmp,
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
	  if (INDIRECT_REF_P (parmse.string_length))
	    /* In chains of functions/procedure calls the string_length already
	       is a pointer to the variable holding the length.  Therefore
	       remove the deref on call.  */
	    parmse.string_length = TREE_OPERAND (parmse.string_length, 0);
	  else
	    {
	      tmp = parmse.string_length;
	      if (!VAR_P (tmp) && TREE_CODE (tmp) != COMPONENT_REF)
		tmp = gfc_evaluate_now (parmse.string_length, &se->pre);
	      parmse.string_length = gfc_build_addr_expr (NULL_TREE, tmp);
	    }
	}

      /* Character strings are passed as two parameters, a length and a
	 pointer - except for Bind(c) which only passes the pointer.
	 An unlimited polymorphic formal argument likewise does not
	 need the length.  */
      if (parmse.string_length != NULL_TREE
	  && !sym->attr.is_bind_c
	  && !(fsym && UNLIMITED_POLY (fsym)))
	vec_safe_push (stringargs, parmse.string_length);

      /* When calling __copy for character expressions to unlimited
	 polymorphic entities, the dst argument needs a string length.  */
      if (sym->name[0] == '_' && e && e->ts.type == BT_CHARACTER
	  && gfc_str_startswith (sym->name, "__vtab_CHARACTER")
	  && arg->next && arg->next->expr
	  && (arg->next->expr->ts.type == BT_DERIVED
	      || arg->next->expr->ts.type == BT_CLASS)
	  && arg->next->expr->ts.u.derived->attr.unlimited_polymorphic)
	vec_safe_push (stringargs, parmse.string_length);

      /* For descriptorless coarrays and assumed-shape coarray dummies, we
	 pass the token and the offset as additional arguments.  */
      if (fsym && e == NULL && flag_coarray == GFC_FCOARRAY_LIB
	  && ((fsym->ts.type != BT_CLASS && fsym->attr.codimension
	       && !fsym->attr.allocatable)
	      || (fsym->ts.type == BT_CLASS
		  && CLASS_DATA (fsym)->attr.codimension
		  && !CLASS_DATA (fsym)->attr.allocatable)))
	{
	  /* Token and offset.  */
	  vec_safe_push (stringargs, null_pointer_node);
	  vec_safe_push (stringargs, build_int_cst (gfc_array_index_type, 0));
	  gcc_assert (fsym->attr.optional);
	}
      else if (fsym && flag_coarray == GFC_FCOARRAY_LIB
	       && ((fsym->ts.type != BT_CLASS && fsym->attr.codimension
		    && !fsym->attr.allocatable)
		   || (fsym->ts.type == BT_CLASS
		       && CLASS_DATA (fsym)->attr.codimension
		       && !CLASS_DATA (fsym)->attr.allocatable)))
	{
	  tree caf_decl, caf_type;
	  tree offset, tmp2;

	  caf_decl = gfc_get_tree_for_caf_expr (e);
	  caf_type = TREE_TYPE (caf_decl);

	  if (GFC_DESCRIPTOR_TYPE_P (caf_type)
	      && (GFC_TYPE_ARRAY_AKIND (caf_type) == GFC_ARRAY_ALLOCATABLE
		  || GFC_TYPE_ARRAY_AKIND (caf_type) == GFC_ARRAY_POINTER))
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

	  vec_safe_push (stringargs, tmp);

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

          tmp2 = fsym->ts.type == BT_CLASS
		 ? gfc_class_data_get (parmse.expr) : parmse.expr;
          if ((fsym->ts.type != BT_CLASS
	       && (fsym->as->type == AS_ASSUMED_SHAPE
		   || fsym->as->type == AS_ASSUMED_RANK))
	      || (fsym->ts.type == BT_CLASS
		  && (CLASS_DATA (fsym)->as->type == AS_ASSUMED_SHAPE
		      || CLASS_DATA (fsym)->as->type == AS_ASSUMED_RANK)))
	    {
	      if (fsym->ts.type == BT_CLASS)
		gcc_assert (!POINTER_TYPE_P (TREE_TYPE (tmp2)));
	      else
		{
		  gcc_assert (POINTER_TYPE_P (TREE_TYPE (tmp2)));
		  tmp2 = build_fold_indirect_ref_loc (input_location, tmp2);
		}
	      gcc_assert (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (tmp2)));
	      tmp2 = gfc_conv_descriptor_data_get (tmp2);
	    }
	  else if (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (tmp2)))
	    tmp2 = gfc_conv_descriptor_data_get (tmp2);
	  else
	    {
	      gcc_assert (POINTER_TYPE_P (TREE_TYPE (tmp2)));
	    }

	  tmp = fold_build2_loc (input_location, MINUS_EXPR,
                                 gfc_array_index_type,
                                 fold_convert (gfc_array_index_type, tmp2),
                                 fold_convert (gfc_array_index_type, tmp));
	  offset = fold_build2_loc (input_location, PLUS_EXPR,
				    gfc_array_index_type, offset, tmp);

	  vec_safe_push (stringargs, offset);
	}

      vec_safe_push (arglist, parmse.expr);
    }
  gfc_finish_interface_mapping (&mapping, &se->pre, &se->post);

  if (comp)
    ts = comp->ts;
  else if (sym->ts.type == BT_CLASS)
    ts = CLASS_DATA (sym)->ts;
  else
    ts = sym->ts;

  if (ts.type == BT_CHARACTER && sym->attr.is_bind_c)
    se->string_length = build_int_cst (gfc_charlen_type_node, 1);
  else if (ts.type == BT_CHARACTER)
    {
      if (ts.u.cl->length == NULL)
	{
	  /* Assumed character length results are not allowed by C418 of the 2003
	     standard and are trapped in resolve.c; except in the case of SPREAD
	     (and other intrinsics?) and dummy functions.  In the case of SPREAD,
	     we take the character length of the first argument for the result.
	     For dummies, we have to look through the formal argument list for
	     this function and use the character length found there.*/
	  if (ts.deferred)
	    cl.backend_decl = gfc_create_var (gfc_charlen_type_node, "slen");
	  else if (!sym->attr.dummy)
	    cl.backend_decl = (*stringargs)[0];
	  else
	    {
	      formal = gfc_sym_get_dummy_args (sym->ns->proc_name);
	      for (; formal; formal = formal->next)
		if (strcmp (formal->sym->name, sym->name) == 0)
		  cl.backend_decl = formal->sym->ts.u.cl->backend_decl;
	    }
	  len = cl.backend_decl;
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
	  tmp = parmse.expr;
	  /* TODO: It would be better to have the charlens as
	     gfc_charlen_type_node already when the interface is
	     created instead of converting it here (see PR 84615).  */
	  tmp = fold_build2_loc (input_location, MAX_EXPR,
				 gfc_charlen_type_node,
				 fold_convert (gfc_charlen_type_node, tmp),
				 build_zero_cst (gfc_charlen_type_node));
	  cl.backend_decl = tmp;
	}

      /* Set up a charlen structure for it.  */
      cl.next = NULL;
      cl.length = NULL;
      ts.u.cl = &cl;

      len = cl.backend_decl;
    }

  byref = (comp && (comp->attr.dimension
	   || (comp->ts.type == BT_CHARACTER && !sym->attr.is_bind_c)))
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
	  if (flag_realloc_lhs
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
	  vec_safe_push (retargs, se->expr);
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
	  if (flag_realloc_lhs && se->ss && se->ss->is_alloc_lhs)
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
	  vec_safe_push (retargs, tmp);
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
	  if (flag_realloc_lhs && se->ss && se->ss->is_alloc_lhs)
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
	  vec_safe_push (retargs, tmp);
	}
      else if (ts.type == BT_CHARACTER)
	{
	  /* Pass the string length.  */
	  type = gfc_get_character_type (ts.kind, ts.u.cl);
	  type = build_pointer_type (type);

	  /* Emit a DECL_EXPR for the VLA type.  */
	  tmp = TREE_TYPE (type);
	  if (TYPE_SIZE (tmp)
	      && TREE_CODE (TYPE_SIZE (tmp)) != INTEGER_CST)
	    {
	      tmp = build_decl (input_location, TYPE_DECL, NULL_TREE, tmp);
	      DECL_ARTIFICIAL (tmp) = 1;
	      DECL_IGNORED_P (tmp) = 1;
	      tmp = fold_build1_loc (input_location, DECL_EXPR,
				     TREE_TYPE (tmp), tmp);
	      gfc_add_expr_to_block (&se->pre, tmp);
	    }

	  /* Return an address to a char[0:len-1]* temporary for
	     character pointers.  */
	  if ((!comp && (sym->attr.pointer || sym->attr.allocatable))
	       || (comp && (comp->attr.pointer || comp->attr.allocatable)))
	    {
	      var = gfc_create_var (type, "pstr");

	      if ((!comp && sym->attr.allocatable)
		  || (comp && comp->attr.allocatable))
		{
		  gfc_add_modify (&se->pre, var,
				  fold_convert (TREE_TYPE (var),
						null_pointer_node));
		  tmp = gfc_call_free (var);
		  gfc_add_expr_to_block (&se->post, tmp);
		}

	      /* Provide an address expression for the function arguments.  */
	      var = gfc_build_addr_expr (NULL_TREE, var);
	    }
	  else
	    var = gfc_conv_string_tmp (se, type, len);

	  vec_safe_push (retargs, var);
	}
      else
	{
	  gcc_assert (flag_f2c && ts.type == BT_COMPLEX);

	  type = gfc_get_complex_type (ts.kind);
	  var = gfc_build_addr_expr (NULL_TREE, gfc_create_var (type, "cmplx"));
	  vec_safe_push (retargs, var);
	}

      /* Add the string length to the argument list.  */
      if (ts.type == BT_CHARACTER && ts.deferred)
	{
	  tmp = len;
	  if (!VAR_P (tmp))
	    tmp = gfc_evaluate_now (len, &se->pre);
	  TREE_STATIC (tmp) = 1;
	  gfc_add_modify (&se->pre, tmp,
			  build_int_cst (TREE_TYPE (tmp), 0));
	  tmp = gfc_build_addr_expr (NULL_TREE, tmp);
	  vec_safe_push (retargs, tmp);
	}
      else if (ts.type == BT_CHARACTER)
	vec_safe_push (retargs, len);
    }
  gfc_free_interface_mapping (&mapping);

  /* We need to glom RETARGS + ARGLIST + STRINGARGS + APPEND_ARGS.  */
  arglen = (vec_safe_length (arglist) + vec_safe_length (optionalargs)
	    + vec_safe_length (stringargs) + vec_safe_length (append_args));
  vec_safe_reserve (retargs, arglen);

  /* Add the return arguments.  */
  vec_safe_splice (retargs, arglist);

  /* Add the hidden present status for optional+value to the arguments.  */
  vec_safe_splice (retargs, optionalargs);

  /* Add the hidden string length parameters to the arguments.  */
  vec_safe_splice (retargs, stringargs);

  /* We may want to append extra arguments here.  This is used e.g. for
     calls to libgfortran_matmul_??, which need extra information.  */
  vec_safe_splice (retargs, append_args);

  arglist = retargs;

  /* Generate the actual call.  */
  if (base_object == NULL_TREE)
    conv_function_val (se, sym, expr, args);
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

  /* Allocatable scalar function results must be freed and nullified
     after use. This necessitates the creation of a temporary to
     hold the result to prevent duplicate calls.  */
  if (!byref && sym->ts.type != BT_CHARACTER
      && ((sym->attr.allocatable && !sym->attr.dimension && !comp)
	  || (comp && comp->attr.allocatable && !comp->attr.dimension)))
    {
      tmp = gfc_create_var (TREE_TYPE (se->expr), NULL);
      gfc_add_modify (&se->pre, tmp, se->expr);
      se->expr = tmp;
      tmp = gfc_call_free (tmp);
      gfc_add_expr_to_block (&post, tmp);
      gfc_add_modify (&post, se->expr, build_int_cst (TREE_TYPE (se->expr), 0));
    }

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
  if (flag_f2c && sym->ts.type == BT_REAL
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
					 logical_type_node,
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

	      se->string_length = len;
	    }
	  else
	    {
	      gcc_assert (ts.type == BT_COMPLEX && flag_f2c);
	      se->expr = build_fold_indirect_ref_loc (input_location, var);
	    }
	}
    }

  /* Associate the rhs class object's meta-data with the result, when the
     result is a temporary.  */
  if (args && args->expr && args->expr->ts.type == BT_CLASS
      && sym->ts.type == BT_CLASS && result != NULL_TREE && DECL_P (result)
      && !GFC_CLASS_TYPE_P (TREE_TYPE (result)))
    {
      gfc_se parmse;
      gfc_expr *class_expr = gfc_find_and_cut_at_last_class_ref (args->expr);

      gfc_init_se (&parmse, NULL);
      parmse.data_not_needed = 1;
      gfc_conv_expr (&parmse, class_expr);
      if (!DECL_LANG_SPECIFIC (result))
	gfc_allocate_lang_decl (result);
      GFC_DECL_SAVED_DESCRIPTOR (result) = parmse.expr;
      gfc_free_expr (class_expr);
      /* -fcheck= can add diagnostic code, which has to be placed before
	 the call. */
      if (parmse.pre.head != NULL)
	  gfc_add_expr_to_block (&se->pre, parmse.pre.head);
      gcc_assert (parmse.post.head == NULL_TREE);
    }

  /* Follow the function call with the argument post block.  */
  if (byref)
    {
      gfc_add_block_to_block (&se->pre, &post);

      /* Transformational functions of derived types with allocatable
	 components must have the result allocatable components copied when the
	 argument is actually given.  */
      arg = expr->value.function.actual;
      if (result && arg && expr->rank
	  && expr->value.function.isym
	  && expr->value.function.isym->transformational
	  && arg->expr
	  && arg->expr->ts.type == BT_DERIVED
	  && arg->expr->ts.u.derived->attr.alloc_comp)
	{
	  tree tmp2;
	  /* Copy the allocatable components.  We have to use a
	     temporary here to prevent source allocatable components
	     from being corrupted.  */
	  tmp2 = gfc_evaluate_now (result, &se->pre);
	  tmp = gfc_copy_alloc_comp (arg->expr->ts.u.derived,
				     result, tmp2, expr->rank, 0);
	  gfc_add_expr_to_block (&se->pre, tmp);
	  tmp = gfc_copy_allocatable_data (result, tmp2, TREE_TYPE(tmp2),
				           expr->rank);
	  gfc_add_expr_to_block (&se->pre, tmp);

	  /* Finally free the temporary's data field.  */
	  tmp = gfc_conv_descriptor_data_get (tmp2);
	  tmp = gfc_deallocate_with_status (tmp, NULL_TREE, NULL_TREE,
					    NULL_TREE, NULL_TREE, true,
					    NULL, GFC_CAF_COARRAY_NOCOARRAY);
	  gfc_add_expr_to_block (&se->pre, tmp);
	}
    }
  else
    {
      /* For a function with a class array result, save the result as
	 a temporary, set the info fields needed by the scalarizer and
	 call the finalization function of the temporary. Note that the
	 nullification of allocatable components needed by the result
	 is done in gfc_trans_assignment_1.  */
      if (expr && ((gfc_is_class_array_function (expr)
		    && se->ss && se->ss->loop)
		   || gfc_is_alloc_class_scalar_function (expr))
	  && se->expr && GFC_CLASS_TYPE_P (TREE_TYPE (se->expr))
	  && expr->must_finalize)
	{
	  tree final_fndecl;
	  tree is_final;
	  int n;
	  if (se->ss && se->ss->loop)
	    {
	      gfc_add_block_to_block (&se->ss->loop->pre, &se->pre);
	      se->expr = gfc_evaluate_now (se->expr, &se->ss->loop->pre);
	      tmp = gfc_class_data_get (se->expr);
	      info->descriptor = tmp;
	      info->data = gfc_conv_descriptor_data_get (tmp);
	      info->offset = gfc_conv_descriptor_offset_get (tmp);
	      for (n = 0; n < se->ss->loop->dimen; n++)
		{
		  tree dim = gfc_rank_cst[n];
		  se->ss->loop->to[n] = gfc_conv_descriptor_ubound_get (tmp, dim);
		  se->ss->loop->from[n] = gfc_conv_descriptor_lbound_get (tmp, dim);
		}
	    }
	  else
	    {
	      /* TODO Eliminate the doubling of temporaries. This
		 one is necessary to ensure no memory leakage.  */
	      se->expr = gfc_evaluate_now (se->expr, &se->pre);
	      tmp = gfc_class_data_get (se->expr);
	      tmp = gfc_conv_scalar_to_descriptor (se, tmp,
			CLASS_DATA (expr->value.function.esym->result)->attr);
	    }

	  if ((gfc_is_class_array_function (expr)
	       || gfc_is_alloc_class_scalar_function (expr))
	      && CLASS_DATA (expr->value.function.esym->result)->attr.pointer)
	    goto no_finalization;

	  final_fndecl = gfc_class_vtab_final_get (se->expr);
	  is_final = fold_build2_loc (input_location, NE_EXPR,
				      logical_type_node,
				      final_fndecl,
				      fold_convert (TREE_TYPE (final_fndecl),
					   	    null_pointer_node));
	  final_fndecl = build_fold_indirect_ref_loc (input_location,
						      final_fndecl);
 	  tmp = build_call_expr_loc (input_location,
				     final_fndecl, 3,
				     gfc_build_addr_expr (NULL, tmp),
				     gfc_class_vtab_size_get (se->expr),
				     boolean_false_node);
	  tmp = fold_build3_loc (input_location, COND_EXPR,
				 void_type_node, is_final, tmp,
				 build_empty_stmt (input_location));

	  if (se->ss && se->ss->loop)
	    {
	      gfc_prepend_expr_to_block (&se->ss->loop->post, tmp);
	      tmp = fold_build2_loc (input_location, NE_EXPR,
				     logical_type_node,
				     info->data,
				     fold_convert (TREE_TYPE (info->data),
					   	    null_pointer_node));
	      tmp = fold_build3_loc (input_location, COND_EXPR,
				     void_type_node, tmp,
				     gfc_call_free (info->data),
				     build_empty_stmt (input_location));
	      gfc_add_expr_to_block (&se->ss->loop->post, tmp);
	    }
	  else
	    {
	      tree classdata;
	      gfc_prepend_expr_to_block (&se->post, tmp);
	      classdata = gfc_class_data_get (se->expr);
	      tmp = fold_build2_loc (input_location, NE_EXPR,
				     logical_type_node,
				     classdata,
				     fold_convert (TREE_TYPE (classdata),
					   	    null_pointer_node));
	      tmp = fold_build3_loc (input_location, COND_EXPR,
				     void_type_node, tmp,
				     gfc_call_free (classdata),
				     build_empty_stmt (input_location));
	      gfc_add_expr_to_block (&se->post, tmp);
	    }
	}

no_finalization:
      gfc_add_block_to_block (&se->post, &post);
    }

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
				fold_convert (size_type_node, size));

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
  cond = fold_build2_loc (input_location, LE_EXPR, logical_type_node, i,
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
      slen = gfc_evaluate_now (fold_convert (gfc_charlen_type_node, slength), block);
      ssc = gfc_string_to_single_character (slen, src, skind);
    }
  else
    {
      slen = build_one_cst (gfc_charlen_type_node);
      ssc =  src;
    }

  if (dlength != NULL_TREE)
    {
      dlen = gfc_evaluate_now (fold_convert (gfc_charlen_type_node, dlength), block);
      dsc = gfc_string_to_single_character (dlen, dest, dkind);
    }
  else
    {
      dlen = build_one_cst (gfc_charlen_type_node);
      dsc =  dest;
    }

  /* Assign directly if the types are compatible.  */
  if (dsc != NULL_TREE && ssc != NULL_TREE
      && TREE_TYPE (dsc) == TREE_TYPE (ssc))
    {
      gfc_add_modify (block, dsc, ssc);
      return;
    }

  /* The string copy algorithm below generates code like

     if (destlen > 0)
       {
         if (srclen < destlen)
           {
             memmove (dest, src, srclen);
             // Pad with spaces.
             memset (&dest[srclen], ' ', destlen - srclen);
           }
         else
           {
             // Truncate if too long.
             memmove (dest, src, destlen);
           }
       }
  */

  /* Do nothing if the destination length is zero.  */
  cond = fold_build2_loc (input_location, GT_EXPR, logical_type_node, dlen,
			  build_zero_cst (TREE_TYPE (dlen)));

  /* For non-default character kinds, we have to multiply the string
     length by the base type size.  */
  chartype = gfc_get_char_type (dkind);
  slen = fold_build2_loc (input_location, MULT_EXPR, TREE_TYPE (slen),
			  slen,
			  fold_convert (TREE_TYPE (slen),
					TYPE_SIZE_UNIT (chartype)));
  dlen = fold_build2_loc (input_location, MULT_EXPR, TREE_TYPE (dlen),
			  dlen,
			  fold_convert (TREE_TYPE (dlen),
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
  cond2 = fold_build2_loc (input_location, LT_EXPR, logical_type_node, slen,
			   dlen);

  /* Copy and pad with spaces.  */
  tmp3 = build_call_expr_loc (input_location,
			      builtin_decl_explicit (BUILT_IN_MEMMOVE),
			      3, dest, src,
			      fold_convert (size_type_node, slen));

  /* Wstringop-overflow appears at -O3 even though this warning is not
     explicitly available in fortran nor can it be switched off. If the
     source length is a constant, its negative appears as a very large
     postive number and triggers the warning in BUILTIN_MEMSET. Fixing
     the result of the MINUS_EXPR suppresses this spurious warning.  */
  tmp = fold_build2_loc (input_location, MINUS_EXPR,
			 TREE_TYPE(dlen), dlen, slen);
  if (slength && TREE_CONSTANT (slength))
    tmp = gfc_evaluate_now (tmp, block);

  tmp4 = fold_build_pointer_plus_loc (input_location, dest, slen);
  tmp4 = fill_with_spaces (tmp4, chartype, tmp);

  gfc_init_block (&tempblock);
  gfc_add_expr_to_block (&tempblock, tmp3);
  gfc_add_expr_to_block (&tempblock, tmp4);
  tmp3 = gfc_finish_block (&tempblock);

  /* The truncated memmove if the slen >= dlen.  */
  tmp2 = build_call_expr_loc (input_location,
			      builtin_decl_explicit (BUILT_IN_MEMMOVE),
			      3, dest, src,
			      fold_convert (size_type_node, dlen));

  /* The whole copy_string function is there.  */
  tmp = fold_build3_loc (input_location, COND_EXPR, void_type_node, cond2,
			 tmp3, tmp2);
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
  for (fargs = gfc_sym_get_dummy_args (sym); fargs; fargs = fargs->next)
    n++;
  saved_vars = XCNEWVEC (gfc_saved_var, n);
  temp_vars = XCNEWVEC (tree, n);

  for (fargs = gfc_sym_get_dummy_args (sym), n = 0; fargs;
       fargs = fargs->next, n++)
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
  for (fargs = gfc_sym_get_dummy_args (sym), n = 0; fargs;
       fargs = fargs->next, n++)
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
  for (fargs = gfc_sym_get_dummy_args (sym), n = 0; fargs;
       fargs = fargs->next, n++)
    gfc_restore_sym (fargs->sym, &saved_vars[n]);
  free (temp_vars);
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

  /* expr.value.function.esym is the resolved (specific) function symbol for
     most functions.  However this isn't set for dummy procedures.  */
  sym = expr->value.function.esym;
  if (!sym)
    sym = expr->symtree->n.sym;

  /* The IEEE_ARITHMETIC functions are caught here. */
  if (sym->from_intmod == INTMOD_IEEE_ARITHMETIC)
    if (gfc_conv_ieee_arithmetic_function (se, expr))
      return;

  /* We distinguish statement functions from general functions to improve
     runtime performance.  */
  if (sym->attr.proc == PROC_ST_FUNCTION)
    {
      gfc_conv_statement_function (se, expr);
      return;
    }

  gfc_conv_procedure_call (se, sym, expr->value.function.actual, expr,
			   NULL);
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

  if (flag_coarray != GFC_FCOARRAY_LIB && ts->type == BT_DERIVED
      && ts->u.derived->from_intmod == INTMOD_ISO_FORTRAN_ENV
      && ts->u.derived->intmod_sym_id == ISOFORTRAN_EVENT_TYPE)
    return build_constructor (type, NULL);

  if (!(expr || pointer || procptr))
    return NULL_TREE;

  /* Check if we have ISOCBINDING_NULL_PTR or ISOCBINDING_NULL_FUNPTR
     (these are the only two iso_c_binding derived types that can be
     used as initialization expressions).  If so, we need to modify
     the 'expr' to be that for a (void *).  */
  if (expr != NULL && expr->ts.type == BT_DERIVED
      && expr->ts.is_iso_c && expr->ts.u.derived)
    {
      if (TREE_CODE (type) == ARRAY_TYPE)
	return build_constructor (type, NULL);
      else if (POINTER_TYPE_P (type))
	return build_int_cst (type, 0);
      else
	gcc_unreachable ();
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
      if (ts->type == BT_CLASS && !procptr)
	{
	  gfc_init_se (&se, NULL);
	  gfc_conv_structure (&se, gfc_class_initializer (ts, expr), 1);
	  gcc_assert (TREE_CODE (se.expr) == CONSTRUCTOR);
	  TREE_STATIC (se.expr) = 1;
	  return se.expr;
	}
      else if (!expr || expr->expr_type == EXPR_NULL)
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
	case_bt_struct:
	case BT_CLASS:
	  gfc_init_se (&se, NULL);
	  if (ts->type == BT_CLASS && expr->expr_type == EXPR_NULL)
	    gfc_conv_structure (&se, gfc_class_initializer (ts, expr), 1);
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

  tmp = gfc_trans_scalar_assign (&lse, &rse, cm->ts, true, false);
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
  se.want_pointer = 0;
  gfc_conv_expr_descriptor (&se, expr);
  gfc_add_block_to_block (&block, &se.pre);
  gfc_add_modify (&block, dest, se.expr);

  /* Deal with arrays of derived types with allocatable components.  */
  if (gfc_bt_struct (cm->ts.type)
	&& cm->ts.u.derived->attr.alloc_comp)
    // TODO: Fix caf_mode
    tmp = gfc_copy_alloc_comp (cm->ts.u.derived,
			       se.expr, dest,
			       cm->as->rank, 0);
  else if (cm->ts.type == BT_CLASS && expr->ts.type == BT_DERIVED
	   && CLASS_DATA(cm)->attr.allocatable)
    {
      if (cm->ts.u.derived->attr.alloc_comp)
	// TODO: Fix caf_mode
	tmp = gfc_copy_alloc_comp (expr->ts.u.derived,
				   se.expr, dest,
				   expr->rank, 0);
      else
	{
	  tmp = TREE_TYPE (dest);
	  tmp = gfc_duplicate_allocatable (dest, se.expr,
					   tmp, expr->rank, NULL_TREE);
	}
    }
  else
    tmp = gfc_duplicate_allocatable (dest, se.expr,
				     TREE_TYPE(cm->backend_decl),
				     cm->as->rank, NULL_TREE);

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
	  tmp = build2_loc (input_location, EQ_EXPR, logical_type_node, tmp,
			    fold_convert (TREE_TYPE (tmp), null_pointer_node));
	  return build3_v (COND_EXPR, tmp,
			   null_expr, non_null_expr);
	}
    }

  return gfc_finish_block (&block);
}


/* Allocate or reallocate scalar component, as necessary.  */

static void
alloc_scalar_allocatable_for_subcomponent_assignment (stmtblock_t *block,
						      tree comp,
						      gfc_component *cm,
						      gfc_expr *expr2,
						      gfc_symbol *sym)
{
  tree tmp;
  tree ptr;
  tree size;
  tree size_in_bytes;
  tree lhs_cl_size = NULL_TREE;

  if (!comp)
    return;

  if (!expr2 || expr2->rank)
    return;

  realloc_lhs_warning (expr2->ts.type, false, &expr2->where);

  if (cm->ts.type == BT_CHARACTER && cm->ts.deferred)
    {
      char name[GFC_MAX_SYMBOL_LEN+9];
      gfc_component *strlen;
      /* Use the rhs string length and the lhs element size.  */
      gcc_assert (expr2->ts.type == BT_CHARACTER);
      if (!expr2->ts.u.cl->backend_decl)
	{
	  gfc_conv_string_length (expr2->ts.u.cl, expr2, block);
	  gcc_assert (expr2->ts.u.cl->backend_decl);
	}

      size = expr2->ts.u.cl->backend_decl;

      /* Ensure that cm->ts.u.cl->backend_decl is a componentref to _%s_length
	 component.  */
      sprintf (name, "_%s_length", cm->name);
      strlen = gfc_find_component (sym, name, true, true, NULL);
      lhs_cl_size = fold_build3_loc (input_location, COMPONENT_REF,
				     gfc_charlen_type_node,
				     TREE_OPERAND (comp, 0),
				     strlen->backend_decl, NULL_TREE);

      tmp = TREE_TYPE (gfc_typenode_for_spec (&cm->ts));
      tmp = TYPE_SIZE_UNIT (tmp);
      size_in_bytes = fold_build2_loc (input_location, MULT_EXPR,
				       TREE_TYPE (tmp), tmp,
				       fold_convert (TREE_TYPE (tmp), size));
    }
  else if (cm->ts.type == BT_CLASS)
    {
      gcc_assert (expr2->ts.type == BT_CLASS || expr2->ts.type == BT_DERIVED);
      if (expr2->ts.type == BT_DERIVED)
	{
	  tmp = gfc_get_symbol_decl (expr2->ts.u.derived);
	  size = TYPE_SIZE_UNIT (tmp);
	}
      else
	{
	  gfc_expr *e2vtab;
	  gfc_se se;
	  e2vtab = gfc_find_and_cut_at_last_class_ref (expr2);
	  gfc_add_vptr_component (e2vtab);
	  gfc_add_size_component (e2vtab);
	  gfc_init_se (&se, NULL);
	  gfc_conv_expr (&se, e2vtab);
	  gfc_add_block_to_block (block, &se.pre);
	  size = fold_convert (size_type_node, se.expr);
	  gfc_free_expr (e2vtab);
	}
      size_in_bytes = size;
    }
  else
    {
      /* Otherwise use the length in bytes of the rhs.  */
      size = TYPE_SIZE_UNIT (gfc_typenode_for_spec (&cm->ts));
      size_in_bytes = size;
    }

  size_in_bytes = fold_build2_loc (input_location, MAX_EXPR, size_type_node,
				   size_in_bytes, size_one_node);

  if (cm->ts.type == BT_DERIVED && cm->ts.u.derived->attr.alloc_comp)
    {
      tmp = build_call_expr_loc (input_location,
				 builtin_decl_explicit (BUILT_IN_CALLOC),
				 2, build_one_cst (size_type_node),
				 size_in_bytes);
      tmp = fold_convert (TREE_TYPE (comp), tmp);
      gfc_add_modify (block, comp, tmp);
    }
  else
    {
      tmp = build_call_expr_loc (input_location,
				 builtin_decl_explicit (BUILT_IN_MALLOC),
				 1, size_in_bytes);
      if (GFC_CLASS_TYPE_P (TREE_TYPE (comp)))
	ptr = gfc_class_data_get (comp);
      else
	ptr = comp;
      tmp = fold_convert (TREE_TYPE (ptr), tmp);
      gfc_add_modify (block, ptr, tmp);
    }

  if (cm->ts.type == BT_CHARACTER && cm->ts.deferred)
    /* Update the lhs character length.  */
    gfc_add_modify (block, lhs_cl_size,
		    fold_convert (TREE_TYPE (lhs_cl_size), size));
}


/* Assign a single component of a derived type constructor.  */

static tree
gfc_trans_subcomponent_assign (tree dest, gfc_component * cm, gfc_expr * expr,
			       gfc_symbol *sym, bool init)
{
  gfc_se se;
  gfc_se lse;
  stmtblock_t block;
  tree tmp;
  tree vtab;

  gfc_start_block (&block);

  if (cm->attr.pointer || cm->attr.proc_pointer)
    {
      /* Only care about pointers here, not about allocatables.  */
      gfc_init_se (&se, NULL);
      /* Pointer component.  */
      if ((cm->attr.dimension || cm->attr.codimension)
	  && !cm->attr.proc_pointer)
	{
	  /* Array pointer.  */
	  if (expr->expr_type == EXPR_NULL)
	    gfc_conv_descriptor_data_set (&block, dest, null_pointer_node);
	  else
	    {
	      se.direct_byref = 1;
	      se.expr = dest;
	      gfc_conv_expr_descriptor (&se, expr);
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

	  if (expr->symtree && expr->symtree->n.sym->attr.proc_pointer
	      && expr->symtree->n.sym->attr.dummy)
	    se.expr = build_fold_indirect_ref_loc (input_location, se.expr);

	  gfc_add_modify (&block, dest,
			       fold_convert (TREE_TYPE (dest), se.expr));
	  gfc_add_block_to_block (&block, &se.post);
	}
    }
  else if (cm->ts.type == BT_CLASS && expr->expr_type == EXPR_NULL)
    {
      /* NULL initialization for CLASS components.  */
      tmp = gfc_trans_structure_assign (dest,
					gfc_class_initializer (&cm->ts, expr),
					false);
      gfc_add_expr_to_block (&block, tmp);
    }
  else if ((cm->attr.dimension || cm->attr.codimension)
	   && !cm->attr.proc_pointer)
    {
      if (cm->attr.allocatable && expr->expr_type == EXPR_NULL)
 	gfc_conv_descriptor_data_set (&block, dest, null_pointer_node);
      else if (cm->attr.allocatable || cm->attr.pdt_array)
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
  else if (cm->ts.type == BT_CLASS
	   && CLASS_DATA (cm)->attr.dimension
	   && CLASS_DATA (cm)->attr.allocatable
	   && expr->ts.type == BT_DERIVED)
    {
      vtab = gfc_get_symbol_decl (gfc_find_vtab (&expr->ts));
      vtab = gfc_build_addr_expr (NULL_TREE, vtab);
      tmp = gfc_class_vptr_get (dest);
      gfc_add_modify (&block, tmp,
		      fold_convert (TREE_TYPE (tmp), vtab));
      tmp = gfc_class_data_get (dest);
      tmp = gfc_trans_alloc_subarray_assign (tmp, cm, expr);
      gfc_add_expr_to_block (&block, tmp);
    }
  else if (init && cm->attr.allocatable && expr->expr_type == EXPR_NULL)
    {
      /* NULL initialization for allocatable components.  */
      gfc_add_modify (&block, dest, fold_convert (TREE_TYPE (dest),
						  null_pointer_node));
    }
  else if (init && (cm->attr.allocatable
	   || (cm->ts.type == BT_CLASS && CLASS_DATA (cm)->attr.allocatable
	       && expr->ts.type != BT_CLASS)))
    {
      /* Take care about non-array allocatable components here.  The alloc_*
	 routine below is motivated by the alloc_scalar_allocatable_for_
	 assignment() routine, but with the realloc portions removed and
	 different input.  */
      alloc_scalar_allocatable_for_subcomponent_assignment (&block,
							    dest,
							    cm,
							    expr,
							    sym);
      /* The remainder of these instructions follow the if (cm->attr.pointer)
	 if (!cm->attr.dimension) part above.  */
      gfc_init_se (&se, NULL);
      gfc_conv_expr (&se, expr);
      gfc_add_block_to_block (&block, &se.pre);

      if (expr->symtree && expr->symtree->n.sym->attr.proc_pointer
	  && expr->symtree->n.sym->attr.dummy)
	se.expr = build_fold_indirect_ref_loc (input_location, se.expr);

      if (cm->ts.type == BT_CLASS && expr->ts.type == BT_DERIVED)
	{
	  tmp = gfc_class_data_get (dest);
	  tmp = build_fold_indirect_ref_loc (input_location, tmp);
	  vtab = gfc_get_symbol_decl (gfc_find_vtab (&expr->ts));
	  vtab = gfc_build_addr_expr (NULL_TREE, vtab);
	  gfc_add_modify (&block, gfc_class_vptr_get (dest),
		 fold_convert (TREE_TYPE (gfc_class_vptr_get (dest)), vtab));
	}
      else
	tmp = build_fold_indirect_ref_loc (input_location, dest);

      /* For deferred strings insert a memcpy.  */
      if (cm->ts.type == BT_CHARACTER && cm->ts.deferred)
	{
	  tree size;
	  gcc_assert (se.string_length || expr->ts.u.cl->backend_decl);
	  size = size_of_string_in_bytes (cm->ts.kind, se.string_length
						? se.string_length
						: expr->ts.u.cl->backend_decl);
	  tmp = gfc_build_memcpy_call (tmp, se.expr, size);
	  gfc_add_expr_to_block (&block, tmp);
	}
      else
	gfc_add_modify (&block, tmp,
			fold_convert (TREE_TYPE (tmp), se.expr));
      gfc_add_block_to_block (&block, &se.post);
    }
  else if (expr->ts.type == BT_UNION)
    {
      tree tmp;
      gfc_constructor *c = gfc_constructor_first (expr->value.constructor);
      /* We mark that the entire union should be initialized with a contrived
         EXPR_NULL expression at the beginning.  */
      if (c != NULL && c->n.component == NULL
	  && c->expr != NULL && c->expr->expr_type == EXPR_NULL)
        {
          tmp = build2_loc (input_location, MODIFY_EXPR, void_type_node,
		            dest, build_constructor (TREE_TYPE (dest), NULL));
	  gfc_add_expr_to_block (&block, tmp);
          c = gfc_constructor_next (c);
        }
      /* The following constructor expression, if any, represents a specific
         map intializer, as given by the user.  */
      if (c != NULL && c->expr != NULL)
        {
          gcc_assert (expr->expr_type == EXPR_STRUCTURE);
	  tmp = gfc_trans_structure_assign (dest, expr, expr->symtree != NULL);
	  gfc_add_expr_to_block (&block, tmp);
        }
    }
  else if (expr->ts.type == BT_DERIVED && expr->ts.f90_type != BT_VOID)
    {
      if (expr->expr_type != EXPR_STRUCTURE)
	{
	  tree dealloc = NULL_TREE;
	  gfc_init_se (&se, NULL);
	  gfc_conv_expr (&se, expr);
	  gfc_add_block_to_block (&block, &se.pre);
	  /* Prevent repeat evaluations in gfc_copy_alloc_comp by fixing the
	     expression in  a temporary variable and deallocate the allocatable
	     components. Then we can the copy the expression to the result.  */
	  if (cm->ts.u.derived->attr.alloc_comp
	      && expr->expr_type != EXPR_VARIABLE)
	    {
	      se.expr = gfc_evaluate_now (se.expr, &block);
	      dealloc = gfc_deallocate_alloc_comp (cm->ts.u.derived, se.expr,
						   expr->rank);
	    }
	  gfc_add_modify (&block, dest,
			  fold_convert (TREE_TYPE (dest), se.expr));
	  if (cm->ts.u.derived->attr.alloc_comp
	      && expr->expr_type != EXPR_NULL)
	    {
	      // TODO: Fix caf_mode
	      tmp = gfc_copy_alloc_comp (cm->ts.u.derived, se.expr,
					 dest, expr->rank, 0);
	      gfc_add_expr_to_block (&block, tmp);
	      if (dealloc != NULL_TREE)
		gfc_add_expr_to_block (&block, dealloc);
	    }
	  gfc_add_block_to_block (&block, &se.post);
	}
      else
	{
	  /* Nested constructors.  */
	  tmp = gfc_trans_structure_assign (dest, expr, expr->symtree != NULL);
	  gfc_add_expr_to_block (&block, tmp);
	}
    }
  else if (gfc_deferred_strlen (cm, &tmp))
    {
      tree strlen;
      strlen = tmp;
      gcc_assert (strlen);
      strlen = fold_build3_loc (input_location, COMPONENT_REF,
				TREE_TYPE (strlen),
				TREE_OPERAND (dest, 0),
				strlen, NULL_TREE);

      if (expr->expr_type == EXPR_NULL)
	{
	  tmp = build_int_cst (TREE_TYPE (cm->backend_decl), 0);
	  gfc_add_modify (&block, dest, tmp);
	  tmp = build_int_cst (TREE_TYPE (strlen), 0);
	  gfc_add_modify (&block, strlen, tmp);
	}
      else
	{
	  tree size;
	  gfc_init_se (&se, NULL);
	  gfc_conv_expr (&se, expr);
	  size = size_of_string_in_bytes (cm->ts.kind, se.string_length);
	  tmp = build_call_expr_loc (input_location,
				     builtin_decl_explicit (BUILT_IN_MALLOC),
				     1, size);
	  gfc_add_modify (&block, dest,
			  fold_convert (TREE_TYPE (dest), tmp));
	  gfc_add_modify (&block, strlen,
			  fold_convert (TREE_TYPE (strlen), se.string_length));
	  tmp = gfc_build_memcpy_call (dest, se.expr, size);
	  gfc_add_expr_to_block (&block, tmp);
	}
    }
  else if (!cm->attr.artificial)
    {
      /* Scalar component (excluding deferred parameters).  */
      gfc_init_se (&se, NULL);
      gfc_init_se (&lse, NULL);

      gfc_conv_expr (&se, expr);
      if (cm->ts.type == BT_CHARACTER)
	lse.string_length = cm->ts.u.cl->backend_decl;
      lse.expr = dest;
      tmp = gfc_trans_scalar_assign (&lse, &se, cm->ts, false, false);
      gfc_add_expr_to_block (&block, tmp);
    }
  return gfc_finish_block (&block);
}

/* Assign a derived type constructor to a variable.  */

tree
gfc_trans_structure_assign (tree dest, gfc_expr * expr, bool init, bool coarray)
{
  gfc_constructor *c;
  gfc_component *cm;
  stmtblock_t block;
  tree field;
  tree tmp;
  gfc_se se;

  gfc_start_block (&block);

  if (expr->ts.u.derived->from_intmod == INTMOD_ISO_C_BINDING
      && (expr->ts.u.derived->intmod_sym_id == ISOCBINDING_PTR
          || expr->ts.u.derived->intmod_sym_id == ISOCBINDING_FUNPTR))
    {
      gfc_se lse;

      gfc_init_se (&se, NULL);
      gfc_init_se (&lse, NULL);
      gfc_conv_expr (&se, gfc_constructor_first (expr->value.constructor)->expr);
      lse.expr = dest;
      gfc_add_modify (&block, lse.expr,
		      fold_convert (TREE_TYPE (lse.expr), se.expr));

      return gfc_finish_block (&block);
    }

  /* Make sure that the derived type has been completely built.  */
  if (!expr->ts.u.derived->backend_decl
      || !TYPE_FIELDS (expr->ts.u.derived->backend_decl))
    {
      tmp = gfc_typenode_for_spec (&expr->ts);
      gcc_assert (tmp);
    }

  cm = expr->ts.u.derived->components;


  if (coarray)
    gfc_init_se (&se, NULL);

  for (c = gfc_constructor_first (expr->value.constructor);
       c; c = gfc_constructor_next (c), cm = cm->next)
    {
      /* Skip absent members in default initializers.  */
      if (!c->expr && !cm->attr.allocatable)
	continue;

      /* Register the component with the caf-lib before it is initialized.
	 Register only allocatable components, that are not coarray'ed
	 components (%comp[*]).  Only register when the constructor is not the
	 null-expression.  */
      if (coarray && !cm->attr.codimension
	  && (cm->attr.allocatable || cm->attr.pointer)
	  && (!c->expr || c->expr->expr_type == EXPR_NULL))
	{
	  tree token, desc, size;
	  bool is_array = cm->ts.type == BT_CLASS
	      ? CLASS_DATA (cm)->attr.dimension : cm->attr.dimension;

	  field = cm->backend_decl;
	  field = fold_build3_loc (input_location, COMPONENT_REF,
				   TREE_TYPE (field), dest, field, NULL_TREE);
	  if (cm->ts.type == BT_CLASS)
	    field = gfc_class_data_get (field);

	  token = is_array ? gfc_conv_descriptor_token (field)
			   : fold_build3_loc (input_location, COMPONENT_REF,
					      TREE_TYPE (cm->caf_token), dest,
					      cm->caf_token, NULL_TREE);

	  if (is_array)
	    {
	      /* The _caf_register routine looks at the rank of the array
		 descriptor to decide whether the data registered is an array
		 or not.  */
	      int rank = cm->ts.type == BT_CLASS ? CLASS_DATA (cm)->as->rank
						 : cm->as->rank;
	      /* When the rank is not known just set a positive rank, which
		 suffices to recognize the data as array.  */
	      if (rank < 0)
		rank = 1;
	      size = build_zero_cst (size_type_node);
	      desc = field;
	      gfc_add_modify (&block, gfc_conv_descriptor_rank (desc),
			      build_int_cst (signed_char_type_node, rank));
	    }
	  else
	    {
	      desc = gfc_conv_scalar_to_descriptor (&se, field,
						    cm->ts.type == BT_CLASS
						    ? CLASS_DATA (cm)->attr
						    : cm->attr);
	      size = TYPE_SIZE_UNIT (TREE_TYPE (field));
	    }
	  gfc_add_block_to_block (&block, &se.pre);
	  tmp =  build_call_expr_loc (input_location, gfor_fndecl_caf_register,
				      7, size, build_int_cst (
					integer_type_node,
					GFC_CAF_COARRAY_ALLOC_REGISTER_ONLY),
				      gfc_build_addr_expr (pvoid_type_node,
							   token),
				      gfc_build_addr_expr (NULL_TREE, desc),
				      null_pointer_node, null_pointer_node,
				      integer_zero_node);
	  gfc_add_expr_to_block (&block, tmp);
	}
      field = cm->backend_decl;
      gcc_assert(field);
      tmp = fold_build3_loc (input_location, COMPONENT_REF, TREE_TYPE (field),
			     dest, field, NULL_TREE);
      if (!c->expr)
	{
	  gfc_expr *e = gfc_get_null_expr (NULL);
	  tmp = gfc_trans_subcomponent_assign (tmp, cm, e, expr->ts.u.derived,
					       init);
	  gfc_free_expr (e);
	}
      else
        tmp = gfc_trans_subcomponent_assign (tmp, cm, c->expr,
                                             expr->ts.u.derived, init);
      gfc_add_expr_to_block (&block, tmp);
    }
  return gfc_finish_block (&block);
}

void
gfc_conv_union_initializer (vec<constructor_elt, va_gc> *v,
                            gfc_component *un, gfc_expr *init)
{
  gfc_constructor *ctor;

  if (un->ts.type != BT_UNION || un == NULL || init == NULL)
    return;

  ctor = gfc_constructor_first (init->value.constructor);

  if (ctor == NULL || ctor->expr == NULL)
    return;

  gcc_assert (init->expr_type == EXPR_STRUCTURE);

  /* If we have an 'initialize all' constructor, do it first.  */
  if (ctor->expr->expr_type == EXPR_NULL)
    {
      tree union_type = TREE_TYPE (un->backend_decl);
      tree val = build_constructor (union_type, NULL);
      CONSTRUCTOR_APPEND_ELT (v, un->backend_decl, val);
      ctor = gfc_constructor_next (ctor);
    }

  /* Add the map initializer on top.  */
  if (ctor != NULL && ctor->expr != NULL)
    {
      gcc_assert (ctor->expr->expr_type == EXPR_STRUCTURE);
      tree val = gfc_conv_initializer (ctor->expr, &un->ts,
                                       TREE_TYPE (un->backend_decl),
                                       un->attr.dimension, un->attr.pointer,
                                       un->attr.proc_pointer);
      CONSTRUCTOR_APPEND_ELT (v, un->backend_decl, val);
    }
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
  vec<constructor_elt, va_gc> *v = NULL;

  gcc_assert (se->ss == NULL);
  gcc_assert (expr->expr_type == EXPR_STRUCTURE);
  type = gfc_typenode_for_spec (&expr->ts);

  if (!init)
    {
      /* Create a temporary variable and fill it in.  */
      se->expr = gfc_create_var (type, expr->ts.u.derived->name);
      /* The symtree in expr is NULL, if the code to generate is for
	 initializing the static members only.  */
      tmp = gfc_trans_structure_assign (se->expr, expr, expr->symtree != NULL,
					se->want_coarray);
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

      if (cm->initializer && cm->initializer->expr_type != EXPR_NULL
	  && strcmp (cm->name, "_extends") == 0
	  && cm->initializer->symtree)
	{
	  tree vtab;
	  gfc_symbol *vtabs;
	  vtabs = cm->initializer->symtree->n.sym;
	  vtab = gfc_build_addr_expr (NULL_TREE, gfc_get_symbol_decl (vtabs));
	  vtab = unshare_expr_without_location (vtab);
	  CONSTRUCTOR_APPEND_ELT (v, cm->backend_decl, vtab);
	}
      else if (cm->ts.u.derived && strcmp (cm->name, "_size") == 0)
	{
	  val = TYPE_SIZE_UNIT (gfc_get_derived_type (cm->ts.u.derived));
	  CONSTRUCTOR_APPEND_ELT (v, cm->backend_decl,
				  fold_convert (TREE_TYPE (cm->backend_decl),
						val));
	}
      else if (cm->ts.type == BT_INTEGER && strcmp (cm->name, "_len") == 0)
	CONSTRUCTOR_APPEND_ELT (v, cm->backend_decl,
				fold_convert (TREE_TYPE (cm->backend_decl),
					      integer_zero_node));
      else if (cm->ts.type == BT_UNION)
        gfc_conv_union_initializer (v, cm, c->expr);
      else
	{
	  val = gfc_conv_initializer (c->expr, &cm->ts,
				      TREE_TYPE (cm->backend_decl),
				      cm->attr.dimension, cm->attr.pointer,
				      cm->attr.proc_pointer);
	  val = unshare_expr_without_location (val);

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
      if (gfc_scalar_elemental_arg_saved_as_reference (ss_info))
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
  if (expr->ts.type == BT_DERIVED && expr->ts.u.derived->ts.f90_type == BT_VOID
      && expr->ts.u.derived->attr.is_bind_c)
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
          expr->ts.type = BT_INTEGER;
          expr->ts.f90_type = BT_VOID;
          expr->ts.kind = gfc_index_integer_kind;
        }
    }

  gfc_fix_class_refs (expr);

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
gfc_conv_expr_reference (gfc_se * se, gfc_expr * expr, bool add_clobber)
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

      if (expr->ts.type == BT_CHARACTER
	  && expr->expr_type != EXPR_FUNCTION)
	gfc_conv_string_parameter (se);
     else
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
      else if (add_clobber && expr->ref == NULL)
	{
	  tree clobber;
	  tree var;
	  /* FIXME: This fails if var is passed by reference, see PR
	     41453.  */
	  var = expr->symtree->n.sym->backend_decl;
	  clobber = build_clobber (TREE_TYPE (var));
	  gfc_add_modify (&se->pre, var, clobber);
	}
      return;
    }

  if (expr->expr_type == EXPR_FUNCTION
      && ((expr->value.function.esym
	   && expr->value.function.esym->result
	   && expr->value.function.esym->result->attr.pointer
	   && !expr->value.function.esym->result->attr.dimension)
	  || (!expr->value.function.esym && !expr->ref
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

  if (!expr->must_finalize)
    gfc_add_block_to_block (&se->pre, &se->post);

  /* Take the address of that value.  */
  se->expr = gfc_build_addr_expr (NULL_TREE, var);
}


/* Get the _len component for an unlimited polymorphic expression.  */

static tree
trans_get_upoly_len (stmtblock_t *block, gfc_expr *expr)
{
  gfc_se se;
  gfc_ref *ref = expr->ref;

  gfc_init_se (&se, NULL);
  while (ref && ref->next)
    ref = ref->next;
  gfc_add_len_component (expr);
  gfc_conv_expr (&se, expr);
  gfc_add_block_to_block (block, &se.pre);
  gcc_assert (se.post.head == NULL_TREE);
  if (ref)
    {
      gfc_free_ref_list (ref->next);
      ref->next = NULL;
    }
  else
    {
      gfc_free_ref_list (expr->ref);
      expr->ref = NULL;
    }
  return se.expr;
}


/* Assign _vptr and _len components as appropriate.  BLOCK should be a
   statement-list outside of the scalarizer-loop.  When code is generated, that
   depends on the scalarized expression, it is added to RSE.PRE.
   Returns le's _vptr tree and when set the len expressions in to_lenp and
   from_lenp to form a le%_vptr%_copy (re, le, [from_lenp, to_lenp])
   expression.  */

static tree
trans_class_vptr_len_assignment (stmtblock_t *block, gfc_expr * le,
				 gfc_expr * re, gfc_se *rse,
				 tree * to_lenp, tree * from_lenp)
{
  gfc_se se;
  gfc_expr * vptr_expr;
  tree tmp, to_len = NULL_TREE, from_len = NULL_TREE, lhs_vptr;
  bool set_vptr = false, temp_rhs = false;
  stmtblock_t *pre = block;

  /* Create a temporary for complicated expressions.  */
  if (re->expr_type != EXPR_VARIABLE && re->expr_type != EXPR_NULL
      && rse->expr != NULL_TREE && !DECL_P (rse->expr))
    {
      tmp = gfc_create_var (TREE_TYPE (rse->expr), "rhs");
      pre = &rse->pre;
      gfc_add_modify (&rse->pre, tmp, rse->expr);
      rse->expr = tmp;
      temp_rhs = true;
    }

  /* Get the _vptr for the left-hand side expression.  */
  gfc_init_se (&se, NULL);
  vptr_expr = gfc_find_and_cut_at_last_class_ref (le);
  if (vptr_expr != NULL && gfc_expr_attr (vptr_expr).class_ok)
    {
      /* Care about _len for unlimited polymorphic entities.  */
      if (UNLIMITED_POLY (vptr_expr)
	  || (vptr_expr->ts.type == BT_DERIVED
	      && vptr_expr->ts.u.derived->attr.unlimited_polymorphic))
	to_len = trans_get_upoly_len (block, vptr_expr);
      gfc_add_vptr_component (vptr_expr);
      set_vptr = true;
    }
  else
    vptr_expr = gfc_lval_expr_from_sym (gfc_find_vtab (&le->ts));
  se.want_pointer = 1;
  gfc_conv_expr (&se, vptr_expr);
  gfc_free_expr (vptr_expr);
  gfc_add_block_to_block (block, &se.pre);
  gcc_assert (se.post.head == NULL_TREE);
  lhs_vptr = se.expr;
  STRIP_NOPS (lhs_vptr);

  /* Set the _vptr only when the left-hand side of the assignment is a
     class-object.  */
  if (set_vptr)
    {
      /* Get the vptr from the rhs expression only, when it is variable.
	 Functions are expected to be assigned to a temporary beforehand.  */
      vptr_expr = (re->expr_type == EXPR_VARIABLE && re->ts.type == BT_CLASS)
	  ? gfc_find_and_cut_at_last_class_ref (re)
	  : NULL;
      if (vptr_expr != NULL && vptr_expr->ts.type == BT_CLASS)
	{
	  if (to_len != NULL_TREE)
	    {
	      /* Get the _len information from the rhs.  */
	      if (UNLIMITED_POLY (vptr_expr)
		  || (vptr_expr->ts.type == BT_DERIVED
		      && vptr_expr->ts.u.derived->attr.unlimited_polymorphic))
		from_len = trans_get_upoly_len (block, vptr_expr);
	    }
	  gfc_add_vptr_component (vptr_expr);
	}
      else
	{
	  if (re->expr_type == EXPR_VARIABLE
	      && DECL_P (re->symtree->n.sym->backend_decl)
	      && DECL_LANG_SPECIFIC (re->symtree->n.sym->backend_decl)
	      && GFC_DECL_SAVED_DESCRIPTOR (re->symtree->n.sym->backend_decl)
	      && GFC_CLASS_TYPE_P (TREE_TYPE (GFC_DECL_SAVED_DESCRIPTOR (
					   re->symtree->n.sym->backend_decl))))
	    {
	      vptr_expr = NULL;
	      se.expr = gfc_class_vptr_get (GFC_DECL_SAVED_DESCRIPTOR (
					     re->symtree->n.sym->backend_decl));
	      if (to_len)
		from_len = gfc_class_len_get (GFC_DECL_SAVED_DESCRIPTOR (
					     re->symtree->n.sym->backend_decl));
	    }
	  else if (temp_rhs && re->ts.type == BT_CLASS)
	    {
	      vptr_expr = NULL;
	      se.expr = gfc_class_vptr_get (rse->expr);
	      if (UNLIMITED_POLY (re))
		from_len = gfc_class_len_get (rse->expr);
	    }
	  else if (re->expr_type != EXPR_NULL)
	    /* Only when rhs is non-NULL use its declared type for vptr
	       initialisation.  */
	    vptr_expr = gfc_lval_expr_from_sym (gfc_find_vtab (&re->ts));
	  else
	    /* When the rhs is NULL use the vtab of lhs' declared type.  */
	    vptr_expr = gfc_lval_expr_from_sym (gfc_find_vtab (&le->ts));
	}

      if (vptr_expr)
	{
	  gfc_init_se (&se, NULL);
	  se.want_pointer = 1;
	  gfc_conv_expr (&se, vptr_expr);
	  gfc_free_expr (vptr_expr);
	  gfc_add_block_to_block (block, &se.pre);
	  gcc_assert (se.post.head == NULL_TREE);
	}
      gfc_add_modify (pre, lhs_vptr, fold_convert (TREE_TYPE (lhs_vptr),
						se.expr));

      if (to_len != NULL_TREE)
	{
	  /* The _len component needs to be set.  Figure how to get the
	     value of the right-hand side.  */
	  if (from_len == NULL_TREE)
	    {
	      if (rse->string_length != NULL_TREE)
		from_len = rse->string_length;
	      else if (re->ts.type == BT_CHARACTER && re->ts.u.cl->length)
		{
		  gfc_init_se (&se, NULL);
		  gfc_conv_expr (&se, re->ts.u.cl->length);
		  gfc_add_block_to_block (block, &se.pre);
		  gcc_assert (se.post.head == NULL_TREE);
		  from_len = gfc_evaluate_now (se.expr, block);
		}
	      else
		from_len = build_zero_cst (gfc_charlen_type_node);
	    }
	  gfc_add_modify (pre, to_len, fold_convert (TREE_TYPE (to_len),
						     from_len));
	}
    }

  /* Return the _len trees only, when requested.  */
  if (to_lenp)
    *to_lenp = to_len;
  if (from_lenp)
    *from_lenp = from_len;
  return lhs_vptr;
}


/* Assign tokens for pointer components.  */

static void
trans_caf_token_assign (gfc_se *lse, gfc_se *rse, gfc_expr *expr1,
			gfc_expr *expr2)
{
  symbol_attribute lhs_attr, rhs_attr;
  tree tmp, lhs_tok, rhs_tok;
  /* Flag to indicated component refs on the rhs.  */
  bool rhs_cr;

  lhs_attr = gfc_caf_attr (expr1);
  if (expr2->expr_type != EXPR_NULL)
    {
      rhs_attr = gfc_caf_attr (expr2, false, &rhs_cr);
      if (lhs_attr.codimension && rhs_attr.codimension)
	{
	  lhs_tok = gfc_get_ultimate_alloc_ptr_comps_caf_token (lse, expr1);
	  lhs_tok = build_fold_indirect_ref (lhs_tok);

	  if (rhs_cr)
	    rhs_tok = gfc_get_ultimate_alloc_ptr_comps_caf_token (rse, expr2);
	  else
	    {
	      tree caf_decl;
	      caf_decl = gfc_get_tree_for_caf_expr (expr2);
	      gfc_get_caf_token_offset (rse, &rhs_tok, NULL, caf_decl,
					NULL_TREE, NULL);
	    }
	  tmp = build2_loc (input_location, MODIFY_EXPR, void_type_node,
			    lhs_tok,
			    fold_convert (TREE_TYPE (lhs_tok), rhs_tok));
	  gfc_prepend_expr_to_block (&lse->post, tmp);
	}
    }
  else if (lhs_attr.codimension)
    {
      lhs_tok = gfc_get_ultimate_alloc_ptr_comps_caf_token (lse, expr1);
      lhs_tok = build_fold_indirect_ref (lhs_tok);
      tmp = build2_loc (input_location, MODIFY_EXPR, void_type_node,
			lhs_tok, null_pointer_node);
      gfc_prepend_expr_to_block (&lse->post, tmp);
    }
}


/* Do everything that is needed for a CLASS function expr2.  */

static tree
trans_class_pointer_fcn (stmtblock_t *block, gfc_se *lse, gfc_se *rse,
			 gfc_expr *expr1, gfc_expr *expr2)
{
  tree expr1_vptr = NULL_TREE;
  tree tmp;

  gfc_conv_function_expr (rse, expr2);
  rse->expr = gfc_evaluate_now (rse->expr, &rse->pre);

  if (expr1->ts.type != BT_CLASS)
      rse->expr = gfc_class_data_get (rse->expr);
  else
    {
      expr1_vptr = trans_class_vptr_len_assignment (block, expr1,
						    expr2, rse,
						    NULL, NULL);
      gfc_add_block_to_block (block, &rse->pre);
      tmp = gfc_create_var (TREE_TYPE (rse->expr), "ptrtemp");
      gfc_add_modify (&lse->pre, tmp, rse->expr);

      gfc_add_modify (&lse->pre, expr1_vptr,
		      fold_convert (TREE_TYPE (expr1_vptr),
		      gfc_class_vptr_get (tmp)));
      rse->expr = gfc_class_data_get (tmp);
    }

  return expr1_vptr;
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
  stmtblock_t block;
  tree desc;
  tree tmp;
  tree expr1_vptr = NULL_TREE;
  bool scalar, non_proc_ptr_assign;
  gfc_ss *ss;

  gfc_start_block (&block);

  gfc_init_se (&lse, NULL);

  /* Usually testing whether this is not a proc pointer assignment.  */
  non_proc_ptr_assign = !(gfc_expr_attr (expr1).proc_pointer
			&& expr2->expr_type == EXPR_VARIABLE
			&& expr2->symtree->n.sym->attr.flavor == FL_PROCEDURE);

  /* Check whether the expression is a scalar or not; we cannot use
     expr1->rank as it can be nonzero for proc pointers.  */
  ss = gfc_walk_expr (expr1);
  scalar = ss == gfc_ss_terminator;
  if (!scalar)
    gfc_free_ss_chain (ss);

  if (expr1->ts.type == BT_DERIVED && expr2->ts.type == BT_CLASS
      && expr2->expr_type != EXPR_FUNCTION && non_proc_ptr_assign)
    {
      gfc_add_data_component (expr2);
      /* The following is required as gfc_add_data_component doesn't
	 update ts.type if there is a tailing REF_ARRAY.  */
      expr2->ts.type = BT_DERIVED;
    }

  if (scalar)
    {
      /* Scalar pointers.  */
      lse.want_pointer = 1;
      gfc_conv_expr (&lse, expr1);
      gfc_init_se (&rse, NULL);
      rse.want_pointer = 1;
      if (expr2->expr_type == EXPR_FUNCTION && expr2->ts.type == BT_CLASS)
	trans_class_pointer_fcn (&block, &lse, &rse, expr1, expr2);
      else
	gfc_conv_expr (&rse, expr2);

      if (non_proc_ptr_assign && expr1->ts.type == BT_CLASS)
	{
	  trans_class_vptr_len_assignment (&block, expr1, expr2, &rse, NULL,
					   NULL);
	  lse.expr = gfc_class_data_get (lse.expr);
	}

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
	  && !expr1->ts.deferred
	  && !expr1->symtree->n.sym->attr.proc_pointer
	  && !gfc_is_proc_ptr_comp (expr1))
	{
	  gcc_assert (expr2->ts.type == BT_CHARACTER);
	  gcc_assert (lse.string_length && rse.string_length);
	  gfc_trans_same_strlen_check ("pointer assignment", &expr1->where,
				       lse.string_length, rse.string_length,
				       &block);
	}

      /* The assignment to an deferred character length sets the string
	 length to that of the rhs.  */
      if (expr1->ts.deferred)
	{
	  if (expr2->expr_type != EXPR_NULL && lse.string_length != NULL)
	    gfc_add_modify (&block, lse.string_length,
			    fold_convert (TREE_TYPE (lse.string_length),
					  rse.string_length));
	  else if (lse.string_length != NULL)
	    gfc_add_modify (&block, lse.string_length,
			    build_zero_cst (TREE_TYPE (lse.string_length)));
	}

      gfc_add_modify (&block, lse.expr,
		      fold_convert (TREE_TYPE (lse.expr), rse.expr));

      /* Also set the tokens for pointer components in derived typed
	 coarrays.  */
      if (flag_coarray == GFC_FCOARRAY_LIB)
	trans_caf_token_assign (&lse, &rse, expr1, expr2);

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
	 not see it and process the bounds remapping afterwards explicitly.  */
      for (remap = expr1->ref; remap; remap = remap->next)
	if (!remap->next && remap->type == REF_ARRAY
	    && remap->u.ar.type == AR_SECTION)
	  break;
      rank_remap = (remap && remap->u.ar.end[0]);

      if (remap && expr2->expr_type == EXPR_NULL)
	{
	  gfc_error ("If bounds remapping is specified at %L, "
		     "the pointer target shall not be NULL", &expr1->where);
	  return NULL_TREE;
	}

      gfc_init_se (&lse, NULL);
      if (remap)
	lse.descriptor_only = 1;
      gfc_conv_expr_descriptor (&lse, expr1);
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

	  if (expr2->expr_type == EXPR_FUNCTION && expr2->ts.type == BT_CLASS)
	    expr1_vptr = trans_class_pointer_fcn (&block, &lse, &rse,
						  expr1, expr2);
	  else if (expr2->expr_type == EXPR_FUNCTION)
	    {
	      tree bound[GFC_MAX_DIMENSIONS];
	      int i;

	      for (i = 0; i < expr2->rank; i++)
		bound[i] = NULL_TREE;
	      tmp = gfc_typenode_for_spec (&expr2->ts);
	      tmp = gfc_get_array_type_bounds (tmp, expr2->rank, 0,
					       bound, bound, 0,
					       GFC_ARRAY_POINTER_CONT, false);
	      tmp = gfc_create_var (tmp, "ptrtemp");
	      rse.descriptor_only = 0;
	      rse.expr = tmp;
	      rse.direct_byref = 1;
	      gfc_conv_expr_descriptor (&rse, expr2);
	      strlen_rhs = rse.string_length;
	      rse.expr = tmp;
	    }
	  else
	    {
	      gfc_conv_expr_descriptor (&rse, expr2);
	      strlen_rhs = rse.string_length;
	      if (expr1->ts.type == BT_CLASS)
		expr1_vptr = trans_class_vptr_len_assignment (&block, expr1,
							      expr2, &rse,
							      NULL, NULL);
	    }
	}
      else if (expr2->expr_type == EXPR_VARIABLE)
	{
	  /* Assign directly to the LHS's descriptor.  */
	  lse.descriptor_only = 0;
	  lse.direct_byref = 1;
	  gfc_conv_expr_descriptor (&lse, expr2);
	  strlen_rhs = lse.string_length;

	  if (expr1->ts.type == BT_CLASS)
	    {
	      rse.expr = NULL_TREE;
	      rse.string_length = NULL_TREE;
	      trans_class_vptr_len_assignment (&block, expr1, expr2, &rse,
					       NULL, NULL);
	    }

	  if (remap == NULL)
	    {
	      /* If the target is not a whole array, use the target array
		 reference for remap.  */
	      for (remap = expr2->ref; remap; remap = remap->next)
		if (remap->type == REF_ARRAY
		    && remap->u.ar.type == AR_FULL
		    && remap->next)
		  break;
	    }
	}
      else if (expr2->expr_type == EXPR_FUNCTION && expr2->ts.type == BT_CLASS)
	{
	  gfc_init_se (&rse, NULL);
	  rse.want_pointer = 1;
	  gfc_conv_function_expr (&rse, expr2);
	  if (expr1->ts.type != BT_CLASS)
	    {
	      rse.expr = gfc_class_data_get (rse.expr);
	      gfc_add_modify (&lse.pre, desc, rse.expr);
	      /* Set the lhs span.  */
	      tmp = TREE_TYPE (rse.expr);
	      tmp = TYPE_SIZE_UNIT (gfc_get_element_type (tmp));
	      tmp = fold_convert (gfc_array_index_type, tmp);
	      gfc_conv_descriptor_span_set (&lse.pre, desc, tmp);
 	    }
	  else
	    {
	      expr1_vptr = trans_class_vptr_len_assignment (&block, expr1,
							    expr2, &rse, NULL,
							    NULL);
	      gfc_add_block_to_block (&block, &rse.pre);
	      tmp = gfc_create_var (TREE_TYPE (rse.expr), "ptrtemp");
	      gfc_add_modify (&lse.pre, tmp, rse.expr);

	      gfc_add_modify (&lse.pre, expr1_vptr,
			      fold_convert (TREE_TYPE (expr1_vptr),
					gfc_class_vptr_get (tmp)));
	      rse.expr = gfc_class_data_get (tmp);
	      gfc_add_modify (&lse.pre, desc, rse.expr);
	    }
	}
      else
	{
	  /* Assign to a temporary descriptor and then copy that
	     temporary to the pointer.  */
	  tmp = gfc_create_var (TREE_TYPE (desc), "ptrtemp");
	  lse.descriptor_only = 0;
	  lse.expr = tmp;
	  lse.direct_byref = 1;
	  gfc_conv_expr_descriptor (&lse, expr2);
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

	      tree dtype, data, span;
	      tree offs, stride;
	      tree lbound, ubound;

	      /* Set dtype.  */
	      dtype = gfc_conv_descriptor_dtype (desc);
	      tmp = gfc_get_dtype (TREE_TYPE (desc));
	      gfc_add_modify (&block, dtype, tmp);

	      /* Copy data pointer.  */
	      data = gfc_conv_descriptor_data_get (rse.expr);
	      gfc_conv_descriptor_data_set (&block, desc, data);

	      /* Copy the span.  */
	      if (TREE_CODE (rse.expr) == VAR_DECL
		  && GFC_DECL_PTR_ARRAY_P (rse.expr))
		span = gfc_conv_descriptor_span_get (rse.expr);
	      else
		{
		  tmp = TREE_TYPE (rse.expr);
		  tmp = TYPE_SIZE_UNIT (gfc_get_element_type (tmp));
		  span = fold_convert (gfc_array_index_type, tmp);
		}
	      gfc_conv_descriptor_span_set (&block, desc, span);

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

		  gcc_assert (!remap->u.ar.end[dim]);
		  gfc_init_se (&lbound_se, NULL);
		  if (remap->u.ar.start[dim])
		    {
		      gfc_conv_expr (&lbound_se, remap->u.ar.start[dim]);
		      gfc_add_block_to_block (&block, &lbound_se.pre);
		    }
		  else
		    /* This remap arises from a target that is not a whole
		       array. The start expressions will be NULL but we need
		       the lbounds to be one.  */
		    lbound_se.expr = gfc_index_one_node;
		  gfc_conv_shift_descriptor_lbound (&block, desc,
						    dim, lbound_se.expr);
		  gfc_add_block_to_block (&block, &lbound_se.post);
		}
	    }
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
	  fault = fold_build2_loc (input_location, LT_EXPR, logical_type_node,
				   rsize, lsize);

	  msg = _("Target of rank remapping is too small (%ld < %ld)");
	  gfc_trans_runtime_check (true, false, fault, &block, &expr2->where,
				   msg, rsize, lsize);
	}

      if (expr1->ts.type == BT_CHARACTER
	  && expr1->symtree->n.sym->ts.deferred
	  && expr1->symtree->n.sym->ts.u.cl->backend_decl
	  && VAR_P (expr1->symtree->n.sym->ts.u.cl->backend_decl))
	{
	  tmp = expr1->symtree->n.sym->ts.u.cl->backend_decl;
	  if (expr2->expr_type != EXPR_NULL)
	    gfc_add_modify (&block, tmp,
			    fold_convert (TREE_TYPE (tmp), strlen_rhs));
	  else
	    gfc_add_modify (&block, tmp, build_zero_cst (TREE_TYPE (tmp)));
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

  if ((TREE_CODE (TREE_TYPE (se->expr)) == ARRAY_TYPE
       || TREE_CODE (TREE_TYPE (se->expr)) == INTEGER_TYPE)
      && TYPE_STRING_FLAG (TREE_TYPE (se->expr)))
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
   If you know that the LHS has no allocations, set dealloc to false.

   DEEP_COPY has no effect if the typespec TS is not a derived type with
   allocatable components.  Otherwise, if it is set, an explicit copy of each
   allocatable component is made.  This is necessary as a simple copy of the
   whole object would copy array descriptors as is, so that the lhs's
   allocatable components would point to the rhs's after the assignment.
   Typically, setting DEEP_COPY is necessary if the rhs is a variable, and not
   necessary if the rhs is a non-pointer function, as the allocatable components
   are not accessible by other means than the function's result after the
   function has returned.  It is even more subtle when temporaries are involved,
   as the two following examples show:
    1.  When we evaluate an array constructor, a temporary is created.  Thus
      there is theoretically no alias possible.  However, no deep copy is
      made for this temporary, so that if the constructor is made of one or
      more variable with allocatable components, those components still point
      to the variable's: DEEP_COPY should be set for the assignment from the
      temporary to the lhs in that case.
    2.  When assigning a scalar to an array, we evaluate the scalar value out
      of the loop, store it into a temporary variable, and assign from that.
      In that case, deep copying when assigning to the temporary would be a
      waste of resources; however deep copies should happen when assigning from
      the temporary to each array element: again DEEP_COPY should be set for
      the assignment from the temporary to the lhs.  */

tree
gfc_trans_scalar_assign (gfc_se * lse, gfc_se * rse, gfc_typespec ts,
			 bool deep_copy, bool dealloc, bool in_coarray)
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
	  gfc_conv_string_parameter (rse);
	  gfc_add_block_to_block (&block, &rse->pre);
	  rlen = rse->string_length;
	}

      gfc_trans_string_copy (&block, llen, lse->expr, ts.kind, rlen,
			     rse->expr, ts.kind);
    }
  else if (gfc_bt_struct (ts.type)
	   && (ts.u.derived->attr.alloc_comp
		|| (deep_copy && ts.u.derived->attr.pdt_type)))
    {
      tree tmp_var = NULL_TREE;
      cond = NULL_TREE;

      /* Are the rhs and the lhs the same?  */
      if (deep_copy)
	{
	  cond = fold_build2_loc (input_location, EQ_EXPR, logical_type_node,
				  gfc_build_addr_expr (NULL_TREE, lse->expr),
				  gfc_build_addr_expr (NULL_TREE, rse->expr));
	  cond = gfc_evaluate_now (cond, &lse->pre);
	}

      /* Deallocate the lhs allocated components as long as it is not
	 the same as the rhs.  This must be done following the assignment
	 to prevent deallocating data that could be used in the rhs
	 expression.  */
      if (dealloc)
	{
	  tmp_var = gfc_evaluate_now (lse->expr, &lse->pre);
	  tmp = gfc_deallocate_alloc_comp_no_caf (ts.u.derived, tmp_var, 0);
	  if (deep_copy)
	    tmp = build3_v (COND_EXPR, cond, build_empty_stmt (input_location),
			    tmp);
	  gfc_add_expr_to_block (&lse->post, tmp);
	}

      gfc_add_block_to_block (&block, &rse->pre);
      gfc_add_block_to_block (&block, &lse->pre);

      gfc_add_modify (&block, lse->expr,
			   fold_convert (TREE_TYPE (lse->expr), rse->expr));

      /* Restore pointer address of coarray components.  */
      if (ts.u.derived->attr.coarray_comp && deep_copy && tmp_var != NULL_TREE)
	{
	  tmp = gfc_reassign_alloc_comp_caf (ts.u.derived, tmp_var, lse->expr);
	  tmp = build3_v (COND_EXPR, cond, build_empty_stmt (input_location),
			  tmp);
	  gfc_add_expr_to_block (&block, tmp);
	}

      /* Do a deep copy if the rhs is a variable, if it is not the
	 same as the lhs.  */
      if (deep_copy)
	{
	  int caf_mode = in_coarray ? (GFC_STRUCTURE_CAF_MODE_ENABLE_COARRAY
				       | GFC_STRUCTURE_CAF_MODE_IN_COARRAY) : 0;
	  tmp = gfc_copy_alloc_comp (ts.u.derived, rse->expr, lse->expr, 0,
				     caf_mode);
	  tmp = build3_v (COND_EXPR, cond, build_empty_stmt (input_location),
			  tmp);
	  gfc_add_expr_to_block (&block, tmp);
	}
    }
  else if (gfc_bt_struct (ts.type) || ts.type == BT_CLASS)
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

  /* Play it safe with class functions assigned to a derived type.  */
  if (gfc_is_class_array_function (expr2)
      && expr1->ts.type == BT_DERIVED)
    return true;

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
     on assignment is active and the lhs is allocatable and a target,
     or a pointer which may be a subref pointer.  FIXME: The last
     condition can go away when we use span in the intrinsics
     directly.*/
  if (expr2->value.function.isym)
    return (flag_realloc_lhs && sym->attr.allocatable && sym->attr.target)
      || (sym->attr.pointer && sym->attr.subref_array_pointer);

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
	 a target.  */
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
     gfc_conv_loop_setup.  */
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


/* For assignment to a reallocatable lhs from intrinsic functions,
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
  tree zero_cond;
  int n;

  /* Use the allocation done by the library.  Substitute the lhs
     descriptor with a copy, whose data field is nulled.*/
  desc = build_fold_indirect_ref_loc (input_location, se->expr);
  if (POINTER_TYPE_P (TREE_TYPE (desc)))
    desc = build_fold_indirect_ref_loc (input_location, desc);

  /* Unallocated, the descriptor does not have a dtype.  */
  tmp = gfc_conv_descriptor_dtype (desc);
  gfc_add_modify (&se->pre, tmp, gfc_get_dtype (TREE_TYPE (desc)));

  res_desc = gfc_evaluate_now (desc, &se->pre);
  gfc_conv_descriptor_data_set (&se->pre, res_desc, null_pointer_node);
  se->expr = gfc_build_addr_expr (NULL_TREE, res_desc);

  /* Free the lhs after the function call and copy the result data to
     the lhs descriptor.  */
  tmp = gfc_conv_descriptor_data_get (desc);
  zero_cond = fold_build2_loc (input_location, EQ_EXPR,
			       logical_type_node, tmp,
			       build_int_cst (TREE_TYPE (tmp), 0));
  zero_cond = gfc_evaluate_now (zero_cond, &se->post);
  tmp = gfc_call_free (tmp);
  gfc_add_expr_to_block (&se->post, tmp);

  tmp = gfc_conv_descriptor_data_get (res_desc);
  gfc_conv_descriptor_data_set (&se->post, desc, tmp);

  /* Check that the shapes are the same between lhs and expression.  */
  for (n = 0 ; n < rank; n++)
    {
      tree tmp1;
      tmp = gfc_conv_descriptor_lbound_get (desc, gfc_rank_cst[n]);
      tmp1 = gfc_conv_descriptor_lbound_get (res_desc, gfc_rank_cst[n]);
      tmp = fold_build2_loc (input_location, MINUS_EXPR,
			     gfc_array_index_type, tmp, tmp1);
      tmp1 = gfc_conv_descriptor_ubound_get (desc, gfc_rank_cst[n]);
      tmp = fold_build2_loc (input_location, MINUS_EXPR,
			     gfc_array_index_type, tmp, tmp1);
      tmp1 = gfc_conv_descriptor_ubound_get (res_desc, gfc_rank_cst[n]);
      tmp = fold_build2_loc (input_location, PLUS_EXPR,
			     gfc_array_index_type, tmp, tmp1);
      tmp = fold_build2_loc (input_location, NE_EXPR,
			     logical_type_node, tmp,
			     gfc_index_zero_node);
      tmp = gfc_evaluate_now (tmp, &se->post);
      zero_cond = fold_build2_loc (input_location, TRUTH_OR_EXPR,
				   logical_type_node, tmp,
				   zero_cond);
    }

  /* 'zero_cond' being true is equal to lhs not being allocated or the
     shapes being different.  */
  zero_cond = gfc_evaluate_now (zero_cond, &se->post);

  /* Now reset the bounds returned from the function call to bounds based
     on the lhs lbounds, except where the lhs is not allocated or the shapes
     of 'variable and 'expr' are different. Set the offset accordingly.  */
  offset = gfc_index_zero_node;
  for (n = 0 ; n < rank; n++)
    {
      tree lbound;

      lbound = gfc_conv_descriptor_lbound_get (desc, gfc_rank_cst[n]);
      lbound = fold_build3_loc (input_location, COND_EXPR,
				gfc_array_index_type, zero_cond,
				gfc_index_one_node, lbound);
      lbound = gfc_evaluate_now (lbound, &se->post);

      tmp = gfc_conv_descriptor_ubound_get (res_desc, gfc_rank_cst[n]);
      tmp = fold_build2_loc (input_location, PLUS_EXPR,
			     gfc_array_index_type, tmp, lbound);
      gfc_conv_descriptor_lbound_set (&se->post, desc,
				      gfc_rank_cst[n], lbound);
      gfc_conv_descriptor_ubound_set (&se->post, desc,
				      gfc_rank_cst[n], tmp);

      /* Set stride and accumulate the offset.  */
      tmp = gfc_conv_descriptor_stride_get (res_desc, gfc_rank_cst[n]);
      gfc_conv_descriptor_stride_set (&se->post, desc,
				      gfc_rank_cst[n], tmp);
      tmp = fold_build2_loc (input_location, MULT_EXPR,
			     gfc_array_index_type, lbound, tmp);
      offset = fold_build2_loc (input_location, MINUS_EXPR,
				gfc_array_index_type, offset, tmp);
      offset = gfc_evaluate_now (offset, &se->post);
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
  gfc_ss *ss = NULL;
  gfc_component *comp = NULL;
  gfc_loopinfo loop;

  if (arrayfunc_assign_needs_temporary (expr1, expr2))
    return NULL;

  /* The frontend doesn't seem to bother filling in expr->symtree for intrinsic
     functions.  */
  comp = gfc_get_proc_ptr_comp (expr2);

  if (!(expr2->value.function.isym
	      || (comp && comp->attr.dimension)
	      || (!comp && gfc_return_by_reference (expr2->value.function.esym)
		  && expr2->value.function.esym->result->attr.dimension)))
    return NULL;

  gfc_init_se (&se, NULL);
  gfc_start_block (&se.pre);
  se.want_pointer = 1;

  gfc_conv_array_parameter (&se, expr1, false, NULL, NULL, NULL);

  if (expr1->ts.type == BT_DERIVED
	&& expr1->ts.u.derived->attr.alloc_comp)
    {
      tree tmp;
      tmp = gfc_deallocate_alloc_comp_no_caf (expr1->ts.u.derived, se.expr,
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
  if (flag_realloc_lhs
	&& gfc_is_reallocatable_lhs (expr1)
	&& !gfc_expr_attr (expr1).codimension
	&& !gfc_is_coindexed (expr1)
	&& !(expr2->value.function.esym
	    && expr2->value.function.esym->result->attr.allocatable))
    {
      realloc_lhs_warning (expr1->ts.type, true, &expr1->where);

      if (!expr2->value.function.isym)
	{
	  ss = gfc_walk_expr (expr1);
	  gcc_assert (ss != gfc_ss_terminator);

	  realloc_lhs_loop_for_fcn_call (&se, &expr1->where, &ss, &loop);
	  ss->is_alloc_lhs = 1;
	}
      else
	fcncall_realloc_result (&se, expr1->rank);
    }

  gfc_conv_function_expr (&se, expr2);
  gfc_add_block_to_block (&se.pre, &se.post);

  if (ss)
    gfc_cleanup_loop (&loop);
  else
    gfc_free_ss_chain (se.ss);

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
		       dest, build_constructor (TREE_TYPE (dest),
					      NULL));

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

  return gfc_build_memcpy_call (dst, src, len);
}


/* Tells whether the expression is to be treated as a variable reference.  */

bool
gfc_expr_is_variable (gfc_expr *expr)
{
  gfc_expr *arg;
  gfc_component *comp;
  gfc_symbol *func_ifc;

  if (expr->expr_type == EXPR_VARIABLE)
    return true;

  arg = gfc_get_noncopying_intrinsic_argument (expr);
  if (arg)
    {
      gcc_assert (expr->value.function.isym->id == GFC_ISYM_TRANSPOSE);
      return gfc_expr_is_variable (arg);
    }

  /* A data-pointer-returning function should be considered as a variable
     too.  */
  if (expr->expr_type == EXPR_FUNCTION
      && expr->ref == NULL)
    {
      if (expr->value.function.isym != NULL)
	return false;

      if (expr->value.function.esym != NULL)
	{
	  func_ifc = expr->value.function.esym;
	  goto found_ifc;
	}
      else
	{
	  gcc_assert (expr->symtree);
	  func_ifc = expr->symtree->n.sym;
	  goto found_ifc;
	}

      gcc_unreachable ();
    }

  comp = gfc_get_proc_ptr_comp (expr);
  if ((expr->expr_type == EXPR_PPC || expr->expr_type == EXPR_FUNCTION)
      && comp)
    {
      func_ifc = comp->ts.interface;
      goto found_ifc;
    }

  if (expr->expr_type == EXPR_COMPCALL)
    {
      gcc_assert (!expr->value.compcall.tbp->is_generic);
      func_ifc = expr->value.compcall.tbp->u.specific->n.sym;
      goto found_ifc;
    }

  return false;

found_ifc:
  gcc_assert (func_ifc->attr.function
	      && func_ifc->result != NULL);
  return func_ifc->result->attr.pointer;
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

  /* All that can be left are allocatable components.  However, we do
     not check for allocatable components here because the expression
     could be an allocatable component of a pointer component.  */
  if (expr->symtree->n.sym->ts.type != BT_DERIVED
	&& expr->symtree->n.sym->ts.type != BT_CLASS)
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
  gfc_ref *ref;

  if (!expr1 || expr1->rank)
    return;

  if (!expr2 || expr2->rank)
    return;

  for (ref = expr1->ref; ref; ref = ref->next)
    if (ref->type == REF_SUBSTRING)
      return;

  realloc_lhs_warning (expr2->ts.type, false, &expr2->where);

  /* Since this is a scalar lhs, we can afford to do this.  That is,
     there is no risk of side effects being repeated.  */
  gfc_init_se (&lse, NULL);
  lse.want_pointer = 1;
  gfc_conv_expr (&lse, expr1);

  jump_label1 = gfc_build_label_decl (NULL_TREE);
  jump_label2 = gfc_build_label_decl (NULL_TREE);

  /* Do the allocation if the lhs is NULL. Otherwise go to label 1.  */
  tmp = build_int_cst (TREE_TYPE (lse.expr), 0);
  cond = fold_build2_loc (input_location, NE_EXPR, logical_type_node,
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

  size_in_bytes = fold_build2_loc (input_location, MAX_EXPR, size_type_node,
				   size_in_bytes, size_one_node);

  if (gfc_caf_attr (expr1).codimension && flag_coarray == GFC_FCOARRAY_LIB)
    {
      tree caf_decl, token;
      gfc_se caf_se;
      symbol_attribute attr;

      gfc_clear_attr (&attr);
      gfc_init_se (&caf_se, NULL);

      caf_decl = gfc_get_tree_for_caf_expr (expr1);
      gfc_get_caf_token_offset (&caf_se, &token, NULL, caf_decl, NULL_TREE,
				NULL);
      gfc_add_block_to_block (block, &caf_se.pre);
      gfc_allocate_allocatable (block, lse.expr, size_in_bytes,
				gfc_build_addr_expr (NULL_TREE, token),
				NULL_TREE, NULL_TREE, NULL_TREE, jump_label1,
				expr1, 1);
    }
  else if (expr1->ts.type == BT_DERIVED && expr1->ts.u.derived->attr.alloc_comp)
    {
      tmp = build_call_expr_loc (input_location,
				 builtin_decl_explicit (BUILT_IN_CALLOC),
				 2, build_one_cst (size_type_node),
				 size_in_bytes);
      tmp = fold_convert (TREE_TYPE (lse.expr), tmp);
      gfc_add_modify (block, lse.expr, tmp);
    }
  else
    {
      tmp = build_call_expr_loc (input_location,
				 builtin_decl_explicit (BUILT_IN_MALLOC),
				 1, size_in_bytes);
      tmp = fold_convert (TREE_TYPE (lse.expr), tmp);
      gfc_add_modify (block, lse.expr, tmp);
    }

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
      cond = fold_build2_loc (input_location, EQ_EXPR, logical_type_node,
			      lse.string_length,
			      fold_convert (TREE_TYPE (lse.string_length),
					    size));
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
      gfc_add_modify (block, lse.string_length,
		      fold_convert (TREE_TYPE (lse.string_length), size));
    }
}

/* Check for assignments of the type

   a = a + 4

   to make sure we do not check for reallocation unneccessarily.  */


static bool
is_runtime_conformable (gfc_expr *expr1, gfc_expr *expr2)
{
  gfc_actual_arglist *a;
  gfc_expr *e1, *e2;

  switch (expr2->expr_type)
    {
    case EXPR_VARIABLE:
      return gfc_dep_compare_expr (expr1, expr2) == 0;

    case EXPR_FUNCTION:
      if (expr2->value.function.esym
	  && expr2->value.function.esym->attr.elemental)
	{
	  for (a = expr2->value.function.actual; a != NULL; a = a->next)
	    {
	      e1 = a->expr;
	      if (e1 && e1->rank > 0 && !is_runtime_conformable (expr1, e1))
		return false;
	    }
	  return true;
	}
      else if (expr2->value.function.isym
	       && expr2->value.function.isym->elemental)
	{
	  for (a = expr2->value.function.actual; a != NULL; a = a->next)
	    {
	      e1 = a->expr;
	      if (e1 && e1->rank > 0 && !is_runtime_conformable (expr1, e1))
		return false;
	    }
	  return true;
	}

      break;

    case EXPR_OP:
      switch (expr2->value.op.op)
	{
	case INTRINSIC_NOT:
	case INTRINSIC_UPLUS:
	case INTRINSIC_UMINUS:
	case INTRINSIC_PARENTHESES:
	  return is_runtime_conformable (expr1, expr2->value.op.op1);

	case INTRINSIC_PLUS:
	case INTRINSIC_MINUS:
	case INTRINSIC_TIMES:
	case INTRINSIC_DIVIDE:
	case INTRINSIC_POWER:
	case INTRINSIC_AND:
	case INTRINSIC_OR:
	case INTRINSIC_EQV:
	case INTRINSIC_NEQV:
	case INTRINSIC_EQ:
	case INTRINSIC_NE:
	case INTRINSIC_GT:
	case INTRINSIC_GE:
	case INTRINSIC_LT:
	case INTRINSIC_LE:
	case INTRINSIC_EQ_OS:
	case INTRINSIC_NE_OS:
	case INTRINSIC_GT_OS:
	case INTRINSIC_GE_OS:
	case INTRINSIC_LT_OS:
	case INTRINSIC_LE_OS:

	  e1 = expr2->value.op.op1;
	  e2 = expr2->value.op.op2;

	  if (e1->rank == 0 && e2->rank > 0)
	    return is_runtime_conformable (expr1, e2);
	  else if (e1->rank > 0 && e2->rank == 0)
	    return is_runtime_conformable (expr1, e1);
	  else if (e1->rank > 0 && e2->rank > 0)
	    return is_runtime_conformable (expr1, e1)
	      && is_runtime_conformable (expr1, e2);
	  break;

	default:
	  break;

	}

      break;

    default:
      break;
    }
  return false;
}


static tree
trans_class_assignment (stmtblock_t *block, gfc_expr *lhs, gfc_expr *rhs,
			gfc_se *lse, gfc_se *rse, bool use_vptr_copy,
			bool class_realloc)
{
  tree tmp, fcn, stdcopy, to_len, from_len, vptr;
  vec<tree, va_gc> *args = NULL;

  vptr = trans_class_vptr_len_assignment (block, lhs, rhs, rse, &to_len,
					 &from_len);

  /* Generate allocation of the lhs.  */
  if (class_realloc)
    {
      stmtblock_t alloc;
      tree class_han;

      tmp = gfc_vptr_size_get (vptr);
      class_han = GFC_CLASS_TYPE_P (TREE_TYPE (lse->expr))
	  ? gfc_class_data_get (lse->expr) : lse->expr;
      gfc_init_block (&alloc);
      gfc_allocate_using_malloc (&alloc, class_han, tmp, NULL_TREE);
      tmp = fold_build2_loc (input_location, EQ_EXPR,
			     logical_type_node, class_han,
			     build_int_cst (prvoid_type_node, 0));
      tmp = fold_build3_loc (input_location, COND_EXPR, void_type_node,
			     gfc_unlikely (tmp,
					   PRED_FORTRAN_FAIL_ALLOC),
			     gfc_finish_block (&alloc),
			     build_empty_stmt (input_location));
      gfc_add_expr_to_block (&lse->pre, tmp);
    }

  fcn = gfc_vptr_copy_get (vptr);

  tmp = GFC_CLASS_TYPE_P (TREE_TYPE (rse->expr))
      ? gfc_class_data_get (rse->expr) : rse->expr;
  if (use_vptr_copy)
    {
      if (!POINTER_TYPE_P (TREE_TYPE (tmp))
	  || INDIRECT_REF_P (tmp)
	  || (rhs->ts.type == BT_DERIVED
	      && rhs->ts.u.derived->attr.unlimited_polymorphic
	      && !rhs->ts.u.derived->attr.pointer
	      && !rhs->ts.u.derived->attr.allocatable)
	  || (UNLIMITED_POLY (rhs)
	      && !CLASS_DATA (rhs)->attr.pointer
	      && !CLASS_DATA (rhs)->attr.allocatable))
	vec_safe_push (args, gfc_build_addr_expr (NULL_TREE, tmp));
      else
	vec_safe_push (args, tmp);
      tmp = GFC_CLASS_TYPE_P (TREE_TYPE (lse->expr))
	  ? gfc_class_data_get (lse->expr) : lse->expr;
      if (!POINTER_TYPE_P (TREE_TYPE (tmp))
	  || INDIRECT_REF_P (tmp)
	  || (lhs->ts.type == BT_DERIVED
	      && lhs->ts.u.derived->attr.unlimited_polymorphic
	      && !lhs->ts.u.derived->attr.pointer
	      && !lhs->ts.u.derived->attr.allocatable)
	  || (UNLIMITED_POLY (lhs)
	      && !CLASS_DATA (lhs)->attr.pointer
	      && !CLASS_DATA (lhs)->attr.allocatable))
	vec_safe_push (args, gfc_build_addr_expr (NULL_TREE, tmp));
      else
	vec_safe_push (args, tmp);

      stdcopy = build_call_vec (TREE_TYPE (TREE_TYPE (fcn)), fcn, args);

      if (to_len != NULL_TREE && !integer_zerop (from_len))
	{
	  tree extcopy;
	  vec_safe_push (args, from_len);
	  vec_safe_push (args, to_len);
	  extcopy = build_call_vec (TREE_TYPE (TREE_TYPE (fcn)), fcn, args);

	  tmp = fold_build2_loc (input_location, GT_EXPR,
				 logical_type_node, from_len,
				 build_zero_cst (TREE_TYPE (from_len)));
	  return fold_build3_loc (input_location, COND_EXPR,
				  void_type_node, tmp,
				  extcopy, stdcopy);
	}
      else
	return stdcopy;
    }
  else
    {
      tree rhst = GFC_CLASS_TYPE_P (TREE_TYPE (lse->expr))
	  ? gfc_class_data_get (lse->expr) : lse->expr;
      stmtblock_t tblock;
      gfc_init_block (&tblock);
      if (!POINTER_TYPE_P (TREE_TYPE (tmp)))
	tmp = gfc_build_addr_expr (NULL_TREE, tmp);
      if (!POINTER_TYPE_P (TREE_TYPE (rhst)))
	rhst = gfc_build_addr_expr (NULL_TREE, rhst);
      /* When coming from a ptr_copy lhs and rhs are swapped.  */
      gfc_add_modify_loc (input_location, &tblock, rhst,
			  fold_convert (TREE_TYPE (rhst), tmp));
      return gfc_finish_block (&tblock);
    }
}

/* Subroutine of gfc_trans_assignment that actually scalarizes the
   assignment.  EXPR1 is the destination/LHS and EXPR2 is the source/RHS.
   init_flag indicates initialization expressions and dealloc that no
   deallocate prior assignment is needed (if in doubt, set true).
   When PTR_COPY is set and expr1 is a class type, then use the _vptr-copy
   routine instead of a pointer assignment.  Alias resolution is only done,
   when MAY_ALIAS is set (the default).  This flag is used by ALLOCATE()
   where it is known, that newly allocated memory on the lhs can never be
   an alias of the rhs.  */

static tree
gfc_trans_assignment_1 (gfc_expr * expr1, gfc_expr * expr2, bool init_flag,
			bool dealloc, bool use_vptr_copy, bool may_alias)
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
  tree string_length;
  int n;
  bool maybe_workshare = false, lhs_refs_comp = false, rhs_refs_comp = false;
  symbol_attribute lhs_caf_attr, rhs_caf_attr, lhs_attr;
  bool is_poly_assign;

  /* Assignment of the form lhs = rhs.  */
  gfc_start_block (&block);

  gfc_init_se (&lse, NULL);
  gfc_init_se (&rse, NULL);

  /* Walk the lhs.  */
  lss = gfc_walk_expr (expr1);
  if (gfc_is_reallocatable_lhs (expr1))
    {
      lss->no_bounds_check = 1;
      if (!(expr2->expr_type == EXPR_FUNCTION
	    && expr2->value.function.isym != NULL
	    && !(expr2->value.function.isym->elemental
		 || expr2->value.function.isym->conversion)))
	lss->is_alloc_lhs = 1;
    }
  else
    lss->no_bounds_check = expr1->no_bounds_check;

  rss = NULL;

  if ((expr1->ts.type == BT_DERIVED)
      && (gfc_is_class_array_function (expr2)
	  || gfc_is_alloc_class_scalar_function (expr2)))
    expr2->must_finalize = 1;

  /* Checking whether a class assignment is desired is quite complicated and
     needed at two locations, so do it once only before the information is
     needed.  */
  lhs_attr = gfc_expr_attr (expr1);
  is_poly_assign = (use_vptr_copy || lhs_attr.pointer
		    || (lhs_attr.allocatable && !lhs_attr.dimension))
		   && (expr1->ts.type == BT_CLASS
		       || gfc_is_class_array_ref (expr1, NULL)
		       || gfc_is_class_scalar_expr (expr1)
		       || gfc_is_class_array_ref (expr2, NULL)
		       || gfc_is_class_scalar_expr (expr2));


  /* Only analyze the expressions for coarray properties, when in coarray-lib
     mode.  */
  if (flag_coarray == GFC_FCOARRAY_LIB)
    {
      lhs_caf_attr = gfc_caf_attr (expr1, false, &lhs_refs_comp);
      rhs_caf_attr = gfc_caf_attr (expr2, false, &rhs_refs_comp);
    }

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
      /* When doing a class assign, then the handle to the rhs needs to be a
	 pointer to allow for polymorphism.  */
      if (is_poly_assign && expr2->rank == 0 && !UNLIMITED_POLY (expr2))
	rss->info->type = GFC_SS_REFERENCE;

      rss->no_bounds_check = expr2->no_bounds_check;
      /* Associate the SS with the loop.  */
      gfc_add_ss_to_loop (&loop, lss);
      gfc_add_ss_to_loop (&loop, rss);

      /* Calculate the bounds of the scalarization.  */
      gfc_conv_ss_startstride (&loop);
      /* Enable loop reversal.  */
      for (n = 0; n < GFC_MAX_DIMENSIONS; n++)
	loop.reverse[n] = GFC_ENABLE_REVERSE;
      /* Resolve any data dependencies in the statement.  */
      if (may_alias)
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
      if ((ompws_flags & (OMPWS_WORKSHARE_FLAG | OMPWS_SCALARIZER_BODY))
	  == OMPWS_WORKSHARE_FLAG
	  && loop.temp_ss == NULL)
	{
	  maybe_workshare = true;
	  ompws_flags |= OMPWS_SCALARIZER_WS | OMPWS_SCALARIZER_BODY;
	}

      /* Start the scalarized loop body.  */
      gfc_start_scalarized_body (&loop, &body);
    }
  else
    gfc_init_block (&body);

  l_is_temp = (lss != gfc_ss_terminator && loop.temp_ss != NULL);

  /* Translate the expression.  */
  rse.want_coarray = flag_coarray == GFC_FCOARRAY_LIB && init_flag
      && lhs_caf_attr.codimension;
  gfc_conv_expr (&rse, expr2);

  /* Deal with the case of a scalar class function assigned to a derived type.  */
  if (gfc_is_alloc_class_scalar_function (expr2)
      && expr1->ts.type == BT_DERIVED)
    {
      rse.expr = gfc_class_data_get (rse.expr);
      rse.expr = build_fold_indirect_ref_loc (input_location, rse.expr);
    }

  /* Stabilize a string length for temporaries.  */
  if (expr2->ts.type == BT_CHARACTER && !expr1->ts.deferred
      && !(VAR_P (rse.string_length)
	   || TREE_CODE (rse.string_length) == PARM_DECL
	   || TREE_CODE (rse.string_length) == INDIRECT_REF))
    string_length = gfc_evaluate_now (rse.string_length, &rse.pre);
  else if (expr2->ts.type == BT_CHARACTER)
    {
      if (expr1->ts.deferred
	  && gfc_expr_attr (expr1).allocatable
	  && gfc_check_dependency (expr1, expr2, true))
	rse.string_length =
	  gfc_evaluate_now_function_scope (rse.string_length, &rse.pre);
      string_length = rse.string_length;
    }
  else
    string_length = NULL_TREE;

  if (l_is_temp)
    {
      gfc_conv_tmp_array_ref (&lse);
      if (expr2->ts.type == BT_CHARACTER)
	lse.string_length = string_length;
    }
  else
    {
      gfc_conv_expr (&lse, expr1);
      if (gfc_option.rtcheck & GFC_RTCHECK_MEM
	  && !init_flag
	  && gfc_expr_attr (expr1).allocatable
	  && expr1->rank
	  && !expr2->rank)
	{
	  tree cond;
	  const char* msg;

	  tmp = INDIRECT_REF_P (lse.expr)
	      ? gfc_build_addr_expr (NULL_TREE, lse.expr) : lse.expr;

	  /* We should only get array references here.  */
	  gcc_assert (TREE_CODE (tmp) == POINTER_PLUS_EXPR
		      || TREE_CODE (tmp) == ARRAY_REF);

	  /* 'tmp' is either the pointer to the array(POINTER_PLUS_EXPR)
	     or the array itself(ARRAY_REF).  */
	  tmp = TREE_OPERAND (tmp, 0);

	  /* Provide the address of the array.  */
	  if (TREE_CODE (lse.expr) == ARRAY_REF)
	    tmp = gfc_build_addr_expr (NULL_TREE, tmp);

	  cond = fold_build2_loc (input_location, EQ_EXPR, logical_type_node,
				  tmp, build_int_cst (TREE_TYPE (tmp), 0));
	  msg = _("Assignment of scalar to unallocated array");
	  gfc_trans_runtime_check (true, false, cond, &loop.pre,
				   &expr1->where, msg);
	}

      /* Deallocate the lhs parameterized components if required.  */
      if (dealloc && expr2->expr_type == EXPR_FUNCTION
	  && !expr1->symtree->n.sym->attr.associate_var)
	{
	  if (expr1->ts.type == BT_DERIVED
	      && expr1->ts.u.derived
	      && expr1->ts.u.derived->attr.pdt_type)
	    {
	      tmp = gfc_deallocate_pdt_comp (expr1->ts.u.derived, lse.expr,
					     expr1->rank);
	      gfc_add_expr_to_block (&lse.pre, tmp);
	    }
	  else if (expr1->ts.type == BT_CLASS
		   && CLASS_DATA (expr1)->ts.u.derived
		   && CLASS_DATA (expr1)->ts.u.derived->attr.pdt_type)
	    {
	      tmp = gfc_class_data_get (lse.expr);
	      tmp = gfc_deallocate_pdt_comp (CLASS_DATA (expr1)->ts.u.derived,
					     tmp, expr1->rank);
	      gfc_add_expr_to_block (&lse.pre, tmp);
	    }
	}
    }

  /* Assignments of scalar derived types with allocatable components
     to arrays must be done with a deep copy and the rhs temporary
     must have its components deallocated afterwards.  */
  scalar_to_array = (expr2->ts.type == BT_DERIVED
		       && expr2->ts.u.derived->attr.alloc_comp
		       && !gfc_expr_is_variable (expr2)
		       && expr1->rank && !expr2->rank);
  scalar_to_array |= (expr1->ts.type == BT_DERIVED
				    && expr1->rank
				    && expr1->ts.u.derived->attr.alloc_comp
				    && gfc_is_alloc_class_scalar_function (expr2));
  if (scalar_to_array && dealloc)
    {
      tmp = gfc_deallocate_alloc_comp_no_caf (expr2->ts.u.derived, rse.expr, 0);
      gfc_prepend_expr_to_block (&loop.post, tmp);
    }

  /* When assigning a character function result to a deferred-length variable,
     the function call must happen before the (re)allocation of the lhs -
     otherwise the character length of the result is not known.
     NOTE 1: This relies on having the exact dependence of the length type
     parameter available to the caller; gfortran saves it in the .mod files.
     NOTE 2: Vector array references generate an index temporary that must
     not go outside the loop. Otherwise, variables should not generate
     a pre block.
     NOTE 3: The concatenation operation generates a temporary pointer,
     whose allocation must go to the innermost loop.
     NOTE 4: Elemental functions may generate a temporary, too.  */
  if (flag_realloc_lhs
      && expr2->ts.type == BT_CHARACTER && expr1->ts.deferred
      && !(lss != gfc_ss_terminator
	   && rss != gfc_ss_terminator
	   && ((expr2->expr_type == EXPR_VARIABLE && expr2->rank)
	       || (expr2->expr_type == EXPR_FUNCTION
		   && expr2->value.function.esym != NULL
		   && expr2->value.function.esym->attr.elemental)
	       || (expr2->expr_type == EXPR_FUNCTION
		   && expr2->value.function.isym != NULL
		   && expr2->value.function.isym->elemental)
	       || (expr2->expr_type == EXPR_OP
		   && expr2->value.op.op == INTRINSIC_CONCAT))))
    gfc_add_block_to_block (&block, &rse.pre);

  /* Nullify the allocatable components corresponding to those of the lhs
     derived type, so that the finalization of the function result does not
     affect the lhs of the assignment. Prepend is used to ensure that the
     nullification occurs before the call to the finalizer. In the case of
     a scalar to array assignment, this is done in gfc_trans_scalar_assign
     as part of the deep copy.  */
  if (!scalar_to_array && expr1->ts.type == BT_DERIVED
		       && (gfc_is_class_array_function (expr2)
			   || gfc_is_alloc_class_scalar_function (expr2)))
    {
      tmp = gfc_nullify_alloc_comp (expr1->ts.u.derived, rse.expr, 0);
      gfc_prepend_expr_to_block (&rse.post, tmp);
      if (lss != gfc_ss_terminator && rss == gfc_ss_terminator)
	gfc_add_block_to_block (&loop.post, &rse.post);
    }

  tmp = NULL_TREE;

  if (is_poly_assign)
    tmp = trans_class_assignment (&body, expr1, expr2, &lse, &rse,
				  use_vptr_copy || (lhs_attr.allocatable
						    && !lhs_attr.dimension),
				  flag_realloc_lhs && !lhs_attr.pointer);
  else if (flag_coarray == GFC_FCOARRAY_LIB
	   && lhs_caf_attr.codimension && rhs_caf_attr.codimension
	   && ((lhs_caf_attr.allocatable && lhs_refs_comp)
	       || (rhs_caf_attr.allocatable && rhs_refs_comp)))
    {
      /* Only detour to caf_send[get][_by_ref] () when the lhs or rhs is an
	 allocatable component, because those need to be accessed via the
	 caf-runtime.  No need to check for coindexes here, because resolve
	 has rewritten those already.  */
      gfc_code code;
      gfc_actual_arglist a1, a2;
      /* Clear the structures to prevent accessing garbage.  */
      memset (&code, '\0', sizeof (gfc_code));
      memset (&a1, '\0', sizeof (gfc_actual_arglist));
      memset (&a2, '\0', sizeof (gfc_actual_arglist));
      a1.expr = expr1;
      a1.next = &a2;
      a2.expr = expr2;
      a2.next = NULL;
      code.ext.actual = &a1;
      code.resolved_isym = gfc_intrinsic_subroutine_by_id (GFC_ISYM_CAF_SEND);
      tmp = gfc_conv_intrinsic_subroutine (&code);
    }
  else if (!is_poly_assign && expr2->must_finalize
	   && expr1->ts.type == BT_CLASS
	   && expr2->ts.type == BT_CLASS)
    {
      /* This case comes about when the scalarizer provides array element
	 references. Use the vptr copy function, since this does a deep
	 copy of allocatable components, without which the finalizer call */
      tmp = gfc_get_vptr_from_expr (rse.expr);
      if (tmp != NULL_TREE)
	{
	  tree fcn = gfc_vptr_copy_get (tmp);
	  if (POINTER_TYPE_P (TREE_TYPE (fcn)))
	    fcn = build_fold_indirect_ref_loc (input_location, fcn);
	  tmp = build_call_expr_loc (input_location,
				     fcn, 2,
				     gfc_build_addr_expr (NULL, rse.expr),
				     gfc_build_addr_expr (NULL, lse.expr));
	}
    }

  /* If nothing else works, do it the old fashioned way!  */
  if (tmp == NULL_TREE)
    tmp = gfc_trans_scalar_assign (&lse, &rse, expr1->ts,
				   gfc_expr_is_variable (expr2)
				   || scalar_to_array
				   || expr2->expr_type == EXPR_ARRAY,
				   !(l_is_temp || init_flag) && dealloc,
				   expr1->symtree->n.sym->attr.codimension);

  /* Add the pre blocks to the body.  */
  gfc_add_block_to_block (&body, &rse.pre);
  gfc_add_block_to_block (&body, &lse.pre);
  gfc_add_expr_to_block (&body, tmp);
  /* Add the post blocks to the body.  */
  gfc_add_block_to_block (&body, &rse.post);
  gfc_add_block_to_block (&body, &lse.post);

  if (lss == gfc_ss_terminator)
    {
      /* F2003: Add the code for reallocation on assignment.  */
      if (flag_realloc_lhs && is_scalar_reallocatable_lhs (expr1)
	  && !is_poly_assign)
	alloc_scalar_allocatable_for_assignment (&block, string_length,
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
					 false, dealloc);
	  gfc_add_expr_to_block (&body, tmp);
	}

      /* F2003: Allocate or reallocate lhs of allocatable array.  */
      if (flag_realloc_lhs
	  && gfc_is_reallocatable_lhs (expr1)
	  && expr2->rank
	  && !is_runtime_conformable (expr1, expr2))
	{
	  realloc_lhs_warning (expr1->ts.type, true, &expr1->where);
	  ompws_flags &= ~OMPWS_SCALARIZER_WS;
	  tmp = gfc_alloc_allocatable_for_assignment (&loop, expr1, expr2);
	  if (tmp != NULL_TREE)
	    gfc_add_expr_to_block (&loop.code[expr1->rank - 1], tmp);
	}

      if (maybe_workshare)
	ompws_flags &= ~OMPWS_SCALARIZER_BODY;

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

    case_bt_struct:
      return !expr->ts.u.derived->attr.alloc_comp;

    default:
      break;
    }

  return false;
}

/* Translate an assignment.  */

tree
gfc_trans_assignment (gfc_expr * expr1, gfc_expr * expr2, bool init_flag,
		      bool dealloc, bool use_vptr_copy, bool may_alias)
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

  if (UNLIMITED_POLY (expr1) && expr1->rank
      && expr2->ts.type != BT_CLASS)
    use_vptr_copy = true;

  /* Fallback to the scalarizer to generate explicit loops.  */
  return gfc_trans_assignment_1 (expr1, expr2, init_flag, dealloc,
				 use_vptr_copy, may_alias);
}

tree
gfc_trans_init_assign (gfc_code * code)
{
  return gfc_trans_assignment (code->expr1, code->expr2, true, false, true);
}

tree
gfc_trans_assign (gfc_code * code)
{
  return gfc_trans_assignment (code->expr1, code->expr2, false, true);
}
