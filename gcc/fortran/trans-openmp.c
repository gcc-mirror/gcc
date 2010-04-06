/* OpenMP directive translation -- generate GCC trees from gfc_code.
   Copyright (C) 2005, 2006, 2007, 2008, 2009 Free Software Foundation, Inc.
   Contributed by Jakub Jelinek <jakub@redhat.com>

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


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "gimple.h"
#include "ggc.h"
#include "toplev.h"
#include "real.h"
#include "gfortran.h"
#include "trans.h"
#include "trans-stmt.h"
#include "trans-types.h"
#include "trans-array.h"
#include "trans-const.h"
#include "arith.h"

int ompws_flags;

/* True if OpenMP should privatize what this DECL points to rather
   than the DECL itself.  */

bool
gfc_omp_privatize_by_reference (const_tree decl)
{
  tree type = TREE_TYPE (decl);

  if (TREE_CODE (type) == REFERENCE_TYPE
      && (!DECL_ARTIFICIAL (decl) || TREE_CODE (decl) == PARM_DECL))
    return true;

  if (TREE_CODE (type) == POINTER_TYPE)
    {
      /* Array POINTER/ALLOCATABLE have aggregate types, all user variables
	 that have POINTER_TYPE type and don't have GFC_POINTER_TYPE_P
	 set are supposed to be privatized by reference.  */
      if (GFC_POINTER_TYPE_P (type))
	return false;

      if (!DECL_ARTIFICIAL (decl))
	return true;

      /* Some arrays are expanded as DECL_ARTIFICIAL pointers
	 by the frontend.  */
      if (DECL_LANG_SPECIFIC (decl)
	  && GFC_DECL_SAVED_DESCRIPTOR (decl))
	return true;
    }

  return false;
}

/* True if OpenMP sharing attribute of DECL is predetermined.  */

enum omp_clause_default_kind
gfc_omp_predetermined_sharing (tree decl)
{
  if (DECL_ARTIFICIAL (decl) && ! GFC_DECL_RESULT (decl))
    return OMP_CLAUSE_DEFAULT_SHARED;

  /* Cray pointees shouldn't be listed in any clauses and should be
     gimplified to dereference of the corresponding Cray pointer.
     Make them all private, so that they are emitted in the debug
     information.  */
  if (GFC_DECL_CRAY_POINTEE (decl))
    return OMP_CLAUSE_DEFAULT_PRIVATE;

  /* Assumed-size arrays are predetermined to inherit sharing
     attributes of the associated actual argument, which is shared
     for all we care.  */
  if (TREE_CODE (decl) == PARM_DECL
      && GFC_ARRAY_TYPE_P (TREE_TYPE (decl))
      && GFC_TYPE_ARRAY_AKIND (TREE_TYPE (decl)) == GFC_ARRAY_UNKNOWN
      && GFC_TYPE_ARRAY_UBOUND (TREE_TYPE (decl),
				GFC_TYPE_ARRAY_RANK (TREE_TYPE (decl)) - 1)
	 == NULL)
    return OMP_CLAUSE_DEFAULT_SHARED;

  /* COMMON and EQUIVALENCE decls are shared.  They
     are only referenced through DECL_VALUE_EXPR of the variables
     contained in them.  If those are privatized, they will not be
     gimplified to the COMMON or EQUIVALENCE decls.  */
  if (GFC_DECL_COMMON_OR_EQUIV (decl) && ! DECL_HAS_VALUE_EXPR_P (decl))
    return OMP_CLAUSE_DEFAULT_SHARED;

  if (GFC_DECL_RESULT (decl) && ! DECL_HAS_VALUE_EXPR_P (decl))
    return OMP_CLAUSE_DEFAULT_SHARED;

  return OMP_CLAUSE_DEFAULT_UNSPECIFIED;
}


/* Return true if DECL in private clause needs
   OMP_CLAUSE_PRIVATE_OUTER_REF on the private clause.  */
bool
gfc_omp_private_outer_ref (tree decl)
{
  tree type = TREE_TYPE (decl);

  if (GFC_DESCRIPTOR_TYPE_P (type)
      && GFC_TYPE_ARRAY_AKIND (type) == GFC_ARRAY_ALLOCATABLE)
    return true;

  return false;
}

/* Return code to initialize DECL with its default constructor, or
   NULL if there's nothing to do.  */

tree
gfc_omp_clause_default_ctor (tree clause, tree decl, tree outer)
{
  tree type = TREE_TYPE (decl), rank, size, esize, ptr, cond, then_b, else_b;
  stmtblock_t block, cond_block;

  if (! GFC_DESCRIPTOR_TYPE_P (type)
      || GFC_TYPE_ARRAY_AKIND (type) != GFC_ARRAY_ALLOCATABLE)
    return NULL;

  gcc_assert (outer != NULL);
  gcc_assert (OMP_CLAUSE_CODE (clause) == OMP_CLAUSE_PRIVATE
	      || OMP_CLAUSE_CODE (clause) == OMP_CLAUSE_LASTPRIVATE);

  /* Allocatable arrays in PRIVATE clauses need to be set to
     "not currently allocated" allocation status if outer
     array is "not currently allocated", otherwise should be allocated.  */
  gfc_start_block (&block);

  gfc_init_block (&cond_block);

  gfc_add_modify (&cond_block, decl, outer);
  rank = gfc_rank_cst[GFC_TYPE_ARRAY_RANK (type) - 1];
  size = gfc_conv_descriptor_ubound_get (decl, rank);
  size = fold_build2 (MINUS_EXPR, gfc_array_index_type, size,
		      gfc_conv_descriptor_lbound_get (decl, rank));
  size = fold_build2 (PLUS_EXPR, gfc_array_index_type, size,
		      gfc_index_one_node);
  if (GFC_TYPE_ARRAY_RANK (type) > 1)
    size = fold_build2 (MULT_EXPR, gfc_array_index_type, size,
			gfc_conv_descriptor_stride_get (decl, rank));
  esize = fold_convert (gfc_array_index_type,
			TYPE_SIZE_UNIT (gfc_get_element_type (type)));
  size = fold_build2 (MULT_EXPR, gfc_array_index_type, size, esize);
  size = gfc_evaluate_now (fold_convert (size_type_node, size), &cond_block);
  ptr = gfc_allocate_array_with_status (&cond_block,
					build_int_cst (pvoid_type_node, 0),
					size, NULL, NULL);
  gfc_conv_descriptor_data_set (&cond_block, decl, ptr);
  then_b = gfc_finish_block (&cond_block);

  gfc_init_block (&cond_block);
  gfc_conv_descriptor_data_set (&cond_block, decl, null_pointer_node);
  else_b = gfc_finish_block (&cond_block);

  cond = fold_build2 (NE_EXPR, boolean_type_node,
		      fold_convert (pvoid_type_node,
				    gfc_conv_descriptor_data_get (outer)),
		      null_pointer_node);
  gfc_add_expr_to_block (&block, build3 (COND_EXPR, void_type_node,
			 cond, then_b, else_b));

  return gfc_finish_block (&block);
}

/* Build and return code for a copy constructor from SRC to DEST.  */

tree
gfc_omp_clause_copy_ctor (tree clause, tree dest, tree src)
{
  tree type = TREE_TYPE (dest), ptr, size, esize, rank, call;
  stmtblock_t block;

  if (! GFC_DESCRIPTOR_TYPE_P (type)
      || GFC_TYPE_ARRAY_AKIND (type) != GFC_ARRAY_ALLOCATABLE)
    return build2_v (MODIFY_EXPR, dest, src);

  gcc_assert (OMP_CLAUSE_CODE (clause) == OMP_CLAUSE_FIRSTPRIVATE);

  /* Allocatable arrays in FIRSTPRIVATE clauses need to be allocated
     and copied from SRC.  */
  gfc_start_block (&block);

  gfc_add_modify (&block, dest, src);
  rank = gfc_rank_cst[GFC_TYPE_ARRAY_RANK (type) - 1];
  size = gfc_conv_descriptor_ubound_get (dest, rank);
  size = fold_build2 (MINUS_EXPR, gfc_array_index_type, size,
		      gfc_conv_descriptor_lbound_get (dest, rank));
  size = fold_build2 (PLUS_EXPR, gfc_array_index_type, size,
		      gfc_index_one_node);
  if (GFC_TYPE_ARRAY_RANK (type) > 1)
    size = fold_build2 (MULT_EXPR, gfc_array_index_type, size,
			gfc_conv_descriptor_stride_get (dest, rank));
  esize = fold_convert (gfc_array_index_type,
			TYPE_SIZE_UNIT (gfc_get_element_type (type)));
  size = fold_build2 (MULT_EXPR, gfc_array_index_type, size, esize);
  size = gfc_evaluate_now (fold_convert (size_type_node, size), &block);
  ptr = gfc_allocate_array_with_status (&block,
					build_int_cst (pvoid_type_node, 0),
					size, NULL, NULL);
  gfc_conv_descriptor_data_set (&block, dest, ptr);
  call = build_call_expr_loc (input_location,
			  built_in_decls[BUILT_IN_MEMCPY], 3, ptr,
			  fold_convert (pvoid_type_node,
					gfc_conv_descriptor_data_get (src)),
			  size);
  gfc_add_expr_to_block (&block, fold_convert (void_type_node, call));

  return gfc_finish_block (&block);
}

/* Similarly, except use an assignment operator instead.  */

tree
gfc_omp_clause_assign_op (tree clause ATTRIBUTE_UNUSED, tree dest, tree src)
{
  tree type = TREE_TYPE (dest), rank, size, esize, call;
  stmtblock_t block;

  if (! GFC_DESCRIPTOR_TYPE_P (type)
      || GFC_TYPE_ARRAY_AKIND (type) != GFC_ARRAY_ALLOCATABLE)
    return build2_v (MODIFY_EXPR, dest, src);

  /* Handle copying allocatable arrays.  */
  gfc_start_block (&block);

  rank = gfc_rank_cst[GFC_TYPE_ARRAY_RANK (type) - 1];
  size = gfc_conv_descriptor_ubound_get (dest, rank);
  size = fold_build2 (MINUS_EXPR, gfc_array_index_type, size,
		      gfc_conv_descriptor_lbound_get (dest, rank));
  size = fold_build2 (PLUS_EXPR, gfc_array_index_type, size,
		      gfc_index_one_node);
  if (GFC_TYPE_ARRAY_RANK (type) > 1)
    size = fold_build2 (MULT_EXPR, gfc_array_index_type, size,
			gfc_conv_descriptor_stride_get (dest, rank));
  esize = fold_convert (gfc_array_index_type,
			TYPE_SIZE_UNIT (gfc_get_element_type (type)));
  size = fold_build2 (MULT_EXPR, gfc_array_index_type, size, esize);
  size = gfc_evaluate_now (fold_convert (size_type_node, size), &block);
  call = build_call_expr_loc (input_location,
			  built_in_decls[BUILT_IN_MEMCPY], 3,
			  fold_convert (pvoid_type_node,
					gfc_conv_descriptor_data_get (dest)),
			  fold_convert (pvoid_type_node,
					gfc_conv_descriptor_data_get (src)),
			  size);
  gfc_add_expr_to_block (&block, fold_convert (void_type_node, call));

  return gfc_finish_block (&block);
}

/* Build and return code destructing DECL.  Return NULL if nothing
   to be done.  */

tree
gfc_omp_clause_dtor (tree clause ATTRIBUTE_UNUSED, tree decl)
{
  tree type = TREE_TYPE (decl);

  if (! GFC_DESCRIPTOR_TYPE_P (type)
      || GFC_TYPE_ARRAY_AKIND (type) != GFC_ARRAY_ALLOCATABLE)
    return NULL;

  /* Allocatable arrays in FIRSTPRIVATE/LASTPRIVATE etc. clauses need
     to be deallocated if they were allocated.  */
  return gfc_trans_dealloc_allocated (decl);
}


/* Return true if DECL's DECL_VALUE_EXPR (if any) should be
   disregarded in OpenMP construct, because it is going to be
   remapped during OpenMP lowering.  SHARED is true if DECL
   is going to be shared, false if it is going to be privatized.  */

bool
gfc_omp_disregard_value_expr (tree decl, bool shared)
{
  if (GFC_DECL_COMMON_OR_EQUIV (decl)
      && DECL_HAS_VALUE_EXPR_P (decl))
    {
      tree value = DECL_VALUE_EXPR (decl);

      if (TREE_CODE (value) == COMPONENT_REF
	  && TREE_CODE (TREE_OPERAND (value, 0)) == VAR_DECL
	  && GFC_DECL_COMMON_OR_EQUIV (TREE_OPERAND (value, 0)))
	{
	  /* If variable in COMMON or EQUIVALENCE is privatized, return
	     true, as just that variable is supposed to be privatized,
	     not the whole COMMON or whole EQUIVALENCE.
	     For shared variables in COMMON or EQUIVALENCE, let them be
	     gimplified to DECL_VALUE_EXPR, so that for multiple shared vars
	     from the same COMMON or EQUIVALENCE just one sharing of the
	     whole COMMON or EQUIVALENCE is enough.  */
	  return ! shared;
	}
    }

  if (GFC_DECL_RESULT (decl) && DECL_HAS_VALUE_EXPR_P (decl))
    return ! shared;

  return false;
}

/* Return true if DECL that is shared iff SHARED is true should
   be put into OMP_CLAUSE_PRIVATE with OMP_CLAUSE_PRIVATE_DEBUG
   flag set.  */

bool
gfc_omp_private_debug_clause (tree decl, bool shared)
{
  if (GFC_DECL_CRAY_POINTEE (decl))
    return true;

  if (GFC_DECL_COMMON_OR_EQUIV (decl)
      && DECL_HAS_VALUE_EXPR_P (decl))
    {
      tree value = DECL_VALUE_EXPR (decl);

      if (TREE_CODE (value) == COMPONENT_REF
	  && TREE_CODE (TREE_OPERAND (value, 0)) == VAR_DECL
	  && GFC_DECL_COMMON_OR_EQUIV (TREE_OPERAND (value, 0)))
	return shared;
    }

  return false;
}

/* Register language specific type size variables as potentially OpenMP
   firstprivate variables.  */

void
gfc_omp_firstprivatize_type_sizes (struct gimplify_omp_ctx *ctx, tree type)
{
  if (GFC_ARRAY_TYPE_P (type) || GFC_DESCRIPTOR_TYPE_P (type))
    {
      int r;

      gcc_assert (TYPE_LANG_SPECIFIC (type) != NULL);
      for (r = 0; r < GFC_TYPE_ARRAY_RANK (type); r++)
	{
	  omp_firstprivatize_variable (ctx, GFC_TYPE_ARRAY_LBOUND (type, r));
	  omp_firstprivatize_variable (ctx, GFC_TYPE_ARRAY_UBOUND (type, r));
	  omp_firstprivatize_variable (ctx, GFC_TYPE_ARRAY_STRIDE (type, r));
	}
      omp_firstprivatize_variable (ctx, GFC_TYPE_ARRAY_SIZE (type));
      omp_firstprivatize_variable (ctx, GFC_TYPE_ARRAY_OFFSET (type));
    }
}


static inline tree
gfc_trans_add_clause (tree node, tree tail)
{
  OMP_CLAUSE_CHAIN (node) = tail;
  return node;
}

static tree
gfc_trans_omp_variable (gfc_symbol *sym)
{
  tree t = gfc_get_symbol_decl (sym);
  tree parent_decl;
  int parent_flag;
  bool return_value;
  bool alternate_entry;
  bool entry_master;

  return_value = sym->attr.function && sym->result == sym;
  alternate_entry = sym->attr.function && sym->attr.entry
		    && sym->result == sym;
  entry_master = sym->attr.result
		 && sym->ns->proc_name->attr.entry_master
		 && !gfc_return_by_reference (sym->ns->proc_name);
  parent_decl = DECL_CONTEXT (current_function_decl);

  if ((t == parent_decl && return_value)
       || (sym->ns && sym->ns->proc_name
	   && sym->ns->proc_name->backend_decl == parent_decl
	   && (alternate_entry || entry_master)))
    parent_flag = 1;
  else
    parent_flag = 0;

  /* Special case for assigning the return value of a function.
     Self recursive functions must have an explicit return value.  */
  if (return_value && (t == current_function_decl || parent_flag))
    t = gfc_get_fake_result_decl (sym, parent_flag);

  /* Similarly for alternate entry points.  */
  else if (alternate_entry
	   && (sym->ns->proc_name->backend_decl == current_function_decl
	       || parent_flag))
    {
      gfc_entry_list *el = NULL;

      for (el = sym->ns->entries; el; el = el->next)
	if (sym == el->sym)
	  {
	    t = gfc_get_fake_result_decl (sym, parent_flag);
	    break;
	  }
    }

  else if (entry_master
	   && (sym->ns->proc_name->backend_decl == current_function_decl
	       || parent_flag))
    t = gfc_get_fake_result_decl (sym, parent_flag);

  return t;
}

static tree
gfc_trans_omp_variable_list (enum omp_clause_code code, gfc_namelist *namelist,
			     tree list)
{
  for (; namelist != NULL; namelist = namelist->next)
    if (namelist->sym->attr.referenced)
      {
	tree t = gfc_trans_omp_variable (namelist->sym);
	if (t != error_mark_node)
	  {
	    tree node = build_omp_clause (input_location, code);
	    OMP_CLAUSE_DECL (node) = t;
	    list = gfc_trans_add_clause (node, list);
	  }
      }
  return list;
}

static void
gfc_trans_omp_array_reduction (tree c, gfc_symbol *sym, locus where)
{
  gfc_symtree *root1 = NULL, *root2 = NULL, *root3 = NULL, *root4 = NULL;
  gfc_symtree *symtree1, *symtree2, *symtree3, *symtree4 = NULL;
  gfc_symbol init_val_sym, outer_sym, intrinsic_sym;
  gfc_expr *e1, *e2, *e3, *e4;
  gfc_ref *ref;
  tree decl, backend_decl, stmt;
  locus old_loc = gfc_current_locus;
  const char *iname;
  gfc_try t;

  decl = OMP_CLAUSE_DECL (c);
  gfc_current_locus = where;

  /* Create a fake symbol for init value.  */
  memset (&init_val_sym, 0, sizeof (init_val_sym));
  init_val_sym.ns = sym->ns;
  init_val_sym.name = sym->name;
  init_val_sym.ts = sym->ts;
  init_val_sym.attr.referenced = 1;
  init_val_sym.declared_at = where;
  init_val_sym.attr.flavor = FL_VARIABLE;
  backend_decl = omp_reduction_init (c, gfc_sym_type (&init_val_sym));
  init_val_sym.backend_decl = backend_decl;

  /* Create a fake symbol for the outer array reference.  */
  outer_sym = *sym;
  outer_sym.as = gfc_copy_array_spec (sym->as);
  outer_sym.attr.dummy = 0;
  outer_sym.attr.result = 0;
  outer_sym.attr.flavor = FL_VARIABLE;
  outer_sym.backend_decl = create_tmp_var_raw (TREE_TYPE (decl), NULL);

  /* Create fake symtrees for it.  */
  symtree1 = gfc_new_symtree (&root1, sym->name);
  symtree1->n.sym = sym;
  gcc_assert (symtree1 == root1);

  symtree2 = gfc_new_symtree (&root2, sym->name);
  symtree2->n.sym = &init_val_sym;
  gcc_assert (symtree2 == root2);

  symtree3 = gfc_new_symtree (&root3, sym->name);
  symtree3->n.sym = &outer_sym;
  gcc_assert (symtree3 == root3);

  /* Create expressions.  */
  e1 = gfc_get_expr ();
  e1->expr_type = EXPR_VARIABLE;
  e1->where = where;
  e1->symtree = symtree1;
  e1->ts = sym->ts;
  e1->ref = ref = gfc_get_ref ();
  ref->type = REF_ARRAY;
  ref->u.ar.where = where;
  ref->u.ar.as = sym->as;
  ref->u.ar.type = AR_FULL;
  ref->u.ar.dimen = 0;
  t = gfc_resolve_expr (e1);
  gcc_assert (t == SUCCESS);

  e2 = gfc_get_expr ();
  e2->expr_type = EXPR_VARIABLE;
  e2->where = where;
  e2->symtree = symtree2;
  e2->ts = sym->ts;
  t = gfc_resolve_expr (e2);
  gcc_assert (t == SUCCESS);

  e3 = gfc_copy_expr (e1);
  e3->symtree = symtree3;
  t = gfc_resolve_expr (e3);
  gcc_assert (t == SUCCESS);

  iname = NULL;
  switch (OMP_CLAUSE_REDUCTION_CODE (c))
    {
    case PLUS_EXPR:
    case MINUS_EXPR:
      e4 = gfc_add (e3, e1);
      break;
    case MULT_EXPR:
      e4 = gfc_multiply (e3, e1);
      break;
    case TRUTH_ANDIF_EXPR:
      e4 = gfc_and (e3, e1);
      break;
    case TRUTH_ORIF_EXPR:
      e4 = gfc_or (e3, e1);
      break;
    case EQ_EXPR:
      e4 = gfc_eqv (e3, e1);
      break;
    case NE_EXPR:
      e4 = gfc_neqv (e3, e1);
      break;
    case MIN_EXPR:
      iname = "min";
      break;
    case MAX_EXPR:
      iname = "max";
      break;
    case BIT_AND_EXPR:
      iname = "iand";
      break;
    case BIT_IOR_EXPR:
      iname = "ior";
      break;
    case BIT_XOR_EXPR:
      iname = "ieor";
      break;
    default:
      gcc_unreachable ();
    }
  if (iname != NULL)
    {
      memset (&intrinsic_sym, 0, sizeof (intrinsic_sym));
      intrinsic_sym.ns = sym->ns;
      intrinsic_sym.name = iname;
      intrinsic_sym.ts = sym->ts;
      intrinsic_sym.attr.referenced = 1;
      intrinsic_sym.attr.intrinsic = 1;
      intrinsic_sym.attr.function = 1;
      intrinsic_sym.result = &intrinsic_sym;
      intrinsic_sym.declared_at = where;

      symtree4 = gfc_new_symtree (&root4, iname);
      symtree4->n.sym = &intrinsic_sym;
      gcc_assert (symtree4 == root4);

      e4 = gfc_get_expr ();
      e4->expr_type = EXPR_FUNCTION;
      e4->where = where;
      e4->symtree = symtree4;
      e4->value.function.isym = gfc_find_function (iname);
      e4->value.function.actual = gfc_get_actual_arglist ();
      e4->value.function.actual->expr = e3;
      e4->value.function.actual->next = gfc_get_actual_arglist ();
      e4->value.function.actual->next->expr = e1;
    }
  /* e1 and e3 have been stored as arguments of e4, avoid sharing.  */
  e1 = gfc_copy_expr (e1);
  e3 = gfc_copy_expr (e3);
  t = gfc_resolve_expr (e4);
  gcc_assert (t == SUCCESS);

  /* Create the init statement list.  */
  pushlevel (0);
  if (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (decl))
      && GFC_TYPE_ARRAY_AKIND (TREE_TYPE (decl)) == GFC_ARRAY_ALLOCATABLE)
    {
      /* If decl is an allocatable array, it needs to be allocated
	 with the same bounds as the outer var.  */
      tree type = TREE_TYPE (decl), rank, size, esize, ptr;
      stmtblock_t block;

      gfc_start_block (&block);

      gfc_add_modify (&block, decl, outer_sym.backend_decl);
      rank = gfc_rank_cst[GFC_TYPE_ARRAY_RANK (type) - 1];
      size = gfc_conv_descriptor_ubound_get (decl, rank);
      size = fold_build2 (MINUS_EXPR, gfc_array_index_type, size,
			  gfc_conv_descriptor_lbound_get (decl, rank));
      size = fold_build2 (PLUS_EXPR, gfc_array_index_type, size,
			  gfc_index_one_node);
      if (GFC_TYPE_ARRAY_RANK (type) > 1)
	size = fold_build2 (MULT_EXPR, gfc_array_index_type, size,
			    gfc_conv_descriptor_stride_get (decl, rank));
      esize = fold_convert (gfc_array_index_type,
			    TYPE_SIZE_UNIT (gfc_get_element_type (type)));
      size = fold_build2 (MULT_EXPR, gfc_array_index_type, size, esize);
      size = gfc_evaluate_now (fold_convert (size_type_node, size), &block);
      ptr = gfc_allocate_array_with_status (&block,
					    build_int_cst (pvoid_type_node, 0),
					    size, NULL, NULL);
      gfc_conv_descriptor_data_set (&block, decl, ptr);
      gfc_add_expr_to_block (&block, gfc_trans_assignment (e1, e2, false,
			     false));
      stmt = gfc_finish_block (&block);
    }
  else
    stmt = gfc_trans_assignment (e1, e2, false, false);
  if (TREE_CODE (stmt) != BIND_EXPR)
    stmt = build3_v (BIND_EXPR, NULL, stmt, poplevel (1, 0, 0));
  else
    poplevel (0, 0, 0);
  OMP_CLAUSE_REDUCTION_INIT (c) = stmt;

  /* Create the merge statement list.  */
  pushlevel (0);
  if (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (decl))
      && GFC_TYPE_ARRAY_AKIND (TREE_TYPE (decl)) == GFC_ARRAY_ALLOCATABLE)
    {
      /* If decl is an allocatable array, it needs to be deallocated
	 afterwards.  */
      stmtblock_t block;

      gfc_start_block (&block);
      gfc_add_expr_to_block (&block, gfc_trans_assignment (e3, e4, false,
			     true));
      gfc_add_expr_to_block (&block, gfc_trans_dealloc_allocated (decl));
      stmt = gfc_finish_block (&block);
    }
  else
    stmt = gfc_trans_assignment (e3, e4, false, true);
  if (TREE_CODE (stmt) != BIND_EXPR)
    stmt = build3_v (BIND_EXPR, NULL, stmt, poplevel (1, 0, 0));
  else
    poplevel (0, 0, 0);
  OMP_CLAUSE_REDUCTION_MERGE (c) = stmt;

  /* And stick the placeholder VAR_DECL into the clause as well.  */
  OMP_CLAUSE_REDUCTION_PLACEHOLDER (c) = outer_sym.backend_decl;

  gfc_current_locus = old_loc;

  gfc_free_expr (e1);
  gfc_free_expr (e2);
  gfc_free_expr (e3);
  gfc_free_expr (e4);
  gfc_free (symtree1);
  gfc_free (symtree2);
  gfc_free (symtree3);
  if (symtree4)
    gfc_free (symtree4);
  gfc_free_array_spec (outer_sym.as);
}

static tree
gfc_trans_omp_reduction_list (gfc_namelist *namelist, tree list, 
			      enum tree_code reduction_code, locus where)
{
  for (; namelist != NULL; namelist = namelist->next)
    if (namelist->sym->attr.referenced)
      {
	tree t = gfc_trans_omp_variable (namelist->sym);
	if (t != error_mark_node)
	  {
	    tree node = build_omp_clause (where.lb->location,
					  OMP_CLAUSE_REDUCTION);
	    OMP_CLAUSE_DECL (node) = t;
	    OMP_CLAUSE_REDUCTION_CODE (node) = reduction_code;
	    if (namelist->sym->attr.dimension)
	      gfc_trans_omp_array_reduction (node, namelist->sym, where);
	    list = gfc_trans_add_clause (node, list);
	  }
      }
  return list;
}

static tree
gfc_trans_omp_clauses (stmtblock_t *block, gfc_omp_clauses *clauses,
		       locus where)
{
  tree omp_clauses = NULL_TREE, chunk_size, c;
  int list;
  enum omp_clause_code clause_code;
  gfc_se se;

  if (clauses == NULL)
    return NULL_TREE;

  for (list = 0; list < OMP_LIST_NUM; list++)
    {
      gfc_namelist *n = clauses->lists[list];

      if (n == NULL)
	continue;
      if (list >= OMP_LIST_REDUCTION_FIRST
	  && list <= OMP_LIST_REDUCTION_LAST)
	{
	  enum tree_code reduction_code;
	  switch (list)
	    {
	    case OMP_LIST_PLUS:
	      reduction_code = PLUS_EXPR;
	      break;
	    case OMP_LIST_MULT:
	      reduction_code = MULT_EXPR;
	      break;
	    case OMP_LIST_SUB:
	      reduction_code = MINUS_EXPR;
	      break;
	    case OMP_LIST_AND:
	      reduction_code = TRUTH_ANDIF_EXPR;
	      break;
	    case OMP_LIST_OR:
	      reduction_code = TRUTH_ORIF_EXPR;
	      break;
	    case OMP_LIST_EQV:
	      reduction_code = EQ_EXPR;
	      break;
	    case OMP_LIST_NEQV:
	      reduction_code = NE_EXPR;
	      break;
	    case OMP_LIST_MAX:
	      reduction_code = MAX_EXPR;
	      break;
	    case OMP_LIST_MIN:
	      reduction_code = MIN_EXPR;
	      break;
	    case OMP_LIST_IAND:
	      reduction_code = BIT_AND_EXPR;
	      break;
	    case OMP_LIST_IOR:
	      reduction_code = BIT_IOR_EXPR;
	      break;
	    case OMP_LIST_IEOR:
	      reduction_code = BIT_XOR_EXPR;
	      break;
	    default:
	      gcc_unreachable ();
	    }
	  omp_clauses
	    = gfc_trans_omp_reduction_list (n, omp_clauses, reduction_code,
					    where);
	  continue;
	}
      switch (list)
	{
	case OMP_LIST_PRIVATE:
	  clause_code = OMP_CLAUSE_PRIVATE;
	  goto add_clause;
	case OMP_LIST_SHARED:
	  clause_code = OMP_CLAUSE_SHARED;
	  goto add_clause;
	case OMP_LIST_FIRSTPRIVATE:
	  clause_code = OMP_CLAUSE_FIRSTPRIVATE;
	  goto add_clause;
	case OMP_LIST_LASTPRIVATE:
	  clause_code = OMP_CLAUSE_LASTPRIVATE;
	  goto add_clause;
	case OMP_LIST_COPYIN:
	  clause_code = OMP_CLAUSE_COPYIN;
	  goto add_clause;
	case OMP_LIST_COPYPRIVATE:
	  clause_code = OMP_CLAUSE_COPYPRIVATE;
	  /* FALLTHROUGH */
	add_clause:
	  omp_clauses
	    = gfc_trans_omp_variable_list (clause_code, n, omp_clauses);
	  break;
	default:
	  break;
	}
    }

  if (clauses->if_expr)
    {
      tree if_var;

      gfc_init_se (&se, NULL);
      gfc_conv_expr (&se, clauses->if_expr);
      gfc_add_block_to_block (block, &se.pre);
      if_var = gfc_evaluate_now (se.expr, block);
      gfc_add_block_to_block (block, &se.post);

      c = build_omp_clause (where.lb->location, OMP_CLAUSE_IF);
      OMP_CLAUSE_IF_EXPR (c) = if_var;
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }

  if (clauses->num_threads)
    {
      tree num_threads;

      gfc_init_se (&se, NULL);
      gfc_conv_expr (&se, clauses->num_threads);
      gfc_add_block_to_block (block, &se.pre);
      num_threads = gfc_evaluate_now (se.expr, block);
      gfc_add_block_to_block (block, &se.post);

      c = build_omp_clause (where.lb->location, OMP_CLAUSE_NUM_THREADS);
      OMP_CLAUSE_NUM_THREADS_EXPR (c) = num_threads;
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }

  chunk_size = NULL_TREE;
  if (clauses->chunk_size)
    {
      gfc_init_se (&se, NULL);
      gfc_conv_expr (&se, clauses->chunk_size);
      gfc_add_block_to_block (block, &se.pre);
      chunk_size = gfc_evaluate_now (se.expr, block);
      gfc_add_block_to_block (block, &se.post);
    }

  if (clauses->sched_kind != OMP_SCHED_NONE)
    {
      c = build_omp_clause (where.lb->location, OMP_CLAUSE_SCHEDULE);
      OMP_CLAUSE_SCHEDULE_CHUNK_EXPR (c) = chunk_size;
      switch (clauses->sched_kind)
	{
	case OMP_SCHED_STATIC:
	  OMP_CLAUSE_SCHEDULE_KIND (c) = OMP_CLAUSE_SCHEDULE_STATIC;
	  break;
	case OMP_SCHED_DYNAMIC:
	  OMP_CLAUSE_SCHEDULE_KIND (c) = OMP_CLAUSE_SCHEDULE_DYNAMIC;
	  break;
	case OMP_SCHED_GUIDED:
	  OMP_CLAUSE_SCHEDULE_KIND (c) = OMP_CLAUSE_SCHEDULE_GUIDED;
	  break;
	case OMP_SCHED_RUNTIME:
	  OMP_CLAUSE_SCHEDULE_KIND (c) = OMP_CLAUSE_SCHEDULE_RUNTIME;
	  break;
	case OMP_SCHED_AUTO:
	  OMP_CLAUSE_SCHEDULE_KIND (c) = OMP_CLAUSE_SCHEDULE_AUTO;
	  break;
	default:
	  gcc_unreachable ();
	}
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }

  if (clauses->default_sharing != OMP_DEFAULT_UNKNOWN)
    {
      c = build_omp_clause (where.lb->location, OMP_CLAUSE_DEFAULT);
      switch (clauses->default_sharing)
	{
	case OMP_DEFAULT_NONE:
	  OMP_CLAUSE_DEFAULT_KIND (c) = OMP_CLAUSE_DEFAULT_NONE;
	  break;
	case OMP_DEFAULT_SHARED:
	  OMP_CLAUSE_DEFAULT_KIND (c) = OMP_CLAUSE_DEFAULT_SHARED;
	  break;
	case OMP_DEFAULT_PRIVATE:
	  OMP_CLAUSE_DEFAULT_KIND (c) = OMP_CLAUSE_DEFAULT_PRIVATE;
	  break;
	case OMP_DEFAULT_FIRSTPRIVATE:
	  OMP_CLAUSE_DEFAULT_KIND (c) = OMP_CLAUSE_DEFAULT_FIRSTPRIVATE;
	  break;
	default:
	  gcc_unreachable ();
	}
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }

  if (clauses->nowait)
    {
      c = build_omp_clause (where.lb->location, OMP_CLAUSE_NOWAIT);
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }

  if (clauses->ordered)
    {
      c = build_omp_clause (where.lb->location, OMP_CLAUSE_ORDERED);
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }

  if (clauses->untied)
    {
      c = build_omp_clause (where.lb->location, OMP_CLAUSE_UNTIED);
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }

  if (clauses->collapse)
    {
      c = build_omp_clause (where.lb->location, OMP_CLAUSE_COLLAPSE);
      OMP_CLAUSE_COLLAPSE_EXPR (c) = build_int_cst (NULL, clauses->collapse);
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }

  return omp_clauses;
}

/* Like gfc_trans_code, but force creation of a BIND_EXPR around it.  */

static tree
gfc_trans_omp_code (gfc_code *code, bool force_empty)
{
  tree stmt;

  pushlevel (0);
  stmt = gfc_trans_code (code);
  if (TREE_CODE (stmt) != BIND_EXPR)
    {
      if (!IS_EMPTY_STMT (stmt) || force_empty)
	{
	  tree block = poplevel (1, 0, 0);
	  stmt = build3_v (BIND_EXPR, NULL, stmt, block);
	}
      else
	poplevel (0, 0, 0);
    }
  else
    poplevel (0, 0, 0);
  return stmt;
}


static tree gfc_trans_omp_sections (gfc_code *, gfc_omp_clauses *);
static tree gfc_trans_omp_workshare (gfc_code *, gfc_omp_clauses *);

static tree
gfc_trans_omp_atomic (gfc_code *code)
{
  gfc_se lse;
  gfc_se rse;
  gfc_expr *expr2, *e;
  gfc_symbol *var;
  stmtblock_t block;
  tree lhsaddr, type, rhs, x;
  enum tree_code op = ERROR_MARK;
  bool var_on_left = false;

  code = code->block->next;
  gcc_assert (code->op == EXEC_ASSIGN);
  gcc_assert (code->next == NULL);
  var = code->expr1->symtree->n.sym;

  gfc_init_se (&lse, NULL);
  gfc_init_se (&rse, NULL);
  gfc_start_block (&block);

  gfc_conv_expr (&lse, code->expr1);
  gfc_add_block_to_block (&block, &lse.pre);
  type = TREE_TYPE (lse.expr);
  lhsaddr = gfc_build_addr_expr (NULL, lse.expr);

  expr2 = code->expr2;
  if (expr2->expr_type == EXPR_FUNCTION
      && expr2->value.function.isym->id == GFC_ISYM_CONVERSION)
    expr2 = expr2->value.function.actual->expr;

  if (expr2->expr_type == EXPR_OP)
    {
      gfc_expr *e;
      switch (expr2->value.op.op)
	{
	case INTRINSIC_PLUS:
	  op = PLUS_EXPR;
	  break;
	case INTRINSIC_TIMES:
	  op = MULT_EXPR;
	  break;
	case INTRINSIC_MINUS:
	  op = MINUS_EXPR;
	  break;
	case INTRINSIC_DIVIDE:
	  if (expr2->ts.type == BT_INTEGER)
	    op = TRUNC_DIV_EXPR;
	  else
	    op = RDIV_EXPR;
	  break;
	case INTRINSIC_AND:
	  op = TRUTH_ANDIF_EXPR;
	  break;
	case INTRINSIC_OR:
	  op = TRUTH_ORIF_EXPR;
	  break;
	case INTRINSIC_EQV:
	  op = EQ_EXPR;
	  break;
	case INTRINSIC_NEQV:
	  op = NE_EXPR;
	  break;
	default:
	  gcc_unreachable ();
	}
      e = expr2->value.op.op1;
      if (e->expr_type == EXPR_FUNCTION
	  && e->value.function.isym->id == GFC_ISYM_CONVERSION)
	e = e->value.function.actual->expr;
      if (e->expr_type == EXPR_VARIABLE
	  && e->symtree != NULL
	  && e->symtree->n.sym == var)
	{
	  expr2 = expr2->value.op.op2;
	  var_on_left = true;
	}
      else
	{
	  e = expr2->value.op.op2;
	  if (e->expr_type == EXPR_FUNCTION
	      && e->value.function.isym->id == GFC_ISYM_CONVERSION)
	    e = e->value.function.actual->expr;
	  gcc_assert (e->expr_type == EXPR_VARIABLE
		      && e->symtree != NULL
		      && e->symtree->n.sym == var);
	  expr2 = expr2->value.op.op1;
	  var_on_left = false;
	}
      gfc_conv_expr (&rse, expr2);
      gfc_add_block_to_block (&block, &rse.pre);
    }
  else
    {
      gcc_assert (expr2->expr_type == EXPR_FUNCTION);
      switch (expr2->value.function.isym->id)
	{
	case GFC_ISYM_MIN:
	  op = MIN_EXPR;
	  break;
	case GFC_ISYM_MAX:
	  op = MAX_EXPR;
	  break;
	case GFC_ISYM_IAND:
	  op = BIT_AND_EXPR;
	  break;
	case GFC_ISYM_IOR:
	  op = BIT_IOR_EXPR;
	  break;
	case GFC_ISYM_IEOR:
	  op = BIT_XOR_EXPR;
	  break;
	default:
	  gcc_unreachable ();
	}
      e = expr2->value.function.actual->expr;
      gcc_assert (e->expr_type == EXPR_VARIABLE
		  && e->symtree != NULL
		  && e->symtree->n.sym == var);

      gfc_conv_expr (&rse, expr2->value.function.actual->next->expr);
      gfc_add_block_to_block (&block, &rse.pre);
      if (expr2->value.function.actual->next->next != NULL)
	{
	  tree accum = gfc_create_var (TREE_TYPE (rse.expr), NULL);
	  gfc_actual_arglist *arg;

	  gfc_add_modify (&block, accum, rse.expr);
	  for (arg = expr2->value.function.actual->next->next; arg;
	       arg = arg->next)
	    {
	      gfc_init_block (&rse.pre);
	      gfc_conv_expr (&rse, arg->expr);
	      gfc_add_block_to_block (&block, &rse.pre);
	      x = fold_build2 (op, TREE_TYPE (accum), accum, rse.expr);
	      gfc_add_modify (&block, accum, x);
	    }

	  rse.expr = accum;
	}

      expr2 = expr2->value.function.actual->next->expr;
    }

  lhsaddr = save_expr (lhsaddr);
  rhs = gfc_evaluate_now (rse.expr, &block);
  x = convert (TREE_TYPE (rhs), build_fold_indirect_ref_loc (input_location,
							 lhsaddr));

  if (var_on_left)
    x = fold_build2 (op, TREE_TYPE (rhs), x, rhs);
  else
    x = fold_build2 (op, TREE_TYPE (rhs), rhs, x);

  if (TREE_CODE (TREE_TYPE (rhs)) == COMPLEX_TYPE
      && TREE_CODE (type) != COMPLEX_TYPE)
    x = fold_build1 (REALPART_EXPR, TREE_TYPE (TREE_TYPE (rhs)), x);

  x = build2_v (OMP_ATOMIC, lhsaddr, convert (type, x));
  gfc_add_expr_to_block (&block, x);

  gfc_add_block_to_block (&block, &lse.pre);
  gfc_add_block_to_block (&block, &rse.pre);

  return gfc_finish_block (&block);
}

static tree
gfc_trans_omp_barrier (void)
{
  tree decl = built_in_decls [BUILT_IN_GOMP_BARRIER];
  return build_call_expr_loc (input_location, decl, 0);
}

static tree
gfc_trans_omp_critical (gfc_code *code)
{
  tree name = NULL_TREE, stmt;
  if (code->ext.omp_name != NULL)
    name = get_identifier (code->ext.omp_name);
  stmt = gfc_trans_code (code->block->next);
  return build2 (OMP_CRITICAL, void_type_node, stmt, name);
}

static tree
gfc_trans_omp_do (gfc_code *code, stmtblock_t *pblock,
		  gfc_omp_clauses *do_clauses, tree par_clauses)
{
  gfc_se se;
  tree dovar, stmt, from, to, step, type, init, cond, incr;
  tree count = NULL_TREE, cycle_label, tmp, omp_clauses;
  stmtblock_t block;
  stmtblock_t body;
  gfc_omp_clauses *clauses = code->ext.omp_clauses;
  int i, collapse = clauses->collapse;
  tree dovar_init = NULL_TREE;

  if (collapse <= 0)
    collapse = 1;

  code = code->block->next;
  gcc_assert (code->op == EXEC_DO);

  init = make_tree_vec (collapse);
  cond = make_tree_vec (collapse);
  incr = make_tree_vec (collapse);

  if (pblock == NULL)
    {
      gfc_start_block (&block);
      pblock = &block;
    }

  omp_clauses = gfc_trans_omp_clauses (pblock, do_clauses, code->loc);

  for (i = 0; i < collapse; i++)
    {
      int simple = 0;
      int dovar_found = 0;
      tree dovar_decl;

      if (clauses)
	{
	  gfc_namelist *n;
	  for (n = clauses->lists[OMP_LIST_LASTPRIVATE]; n != NULL;
	       n = n->next)
	    if (code->ext.iterator->var->symtree->n.sym == n->sym)
	      break;
	  if (n != NULL)
	    dovar_found = 1;
	  else if (n == NULL)
	    for (n = clauses->lists[OMP_LIST_PRIVATE]; n != NULL; n = n->next)
	      if (code->ext.iterator->var->symtree->n.sym == n->sym)
		break;
	  if (n != NULL)
	    dovar_found++;
	}

      /* Evaluate all the expressions in the iterator.  */
      gfc_init_se (&se, NULL);
      gfc_conv_expr_lhs (&se, code->ext.iterator->var);
      gfc_add_block_to_block (pblock, &se.pre);
      dovar = se.expr;
      type = TREE_TYPE (dovar);
      gcc_assert (TREE_CODE (type) == INTEGER_TYPE);

      gfc_init_se (&se, NULL);
      gfc_conv_expr_val (&se, code->ext.iterator->start);
      gfc_add_block_to_block (pblock, &se.pre);
      from = gfc_evaluate_now (se.expr, pblock);

      gfc_init_se (&se, NULL);
      gfc_conv_expr_val (&se, code->ext.iterator->end);
      gfc_add_block_to_block (pblock, &se.pre);
      to = gfc_evaluate_now (se.expr, pblock);

      gfc_init_se (&se, NULL);
      gfc_conv_expr_val (&se, code->ext.iterator->step);
      gfc_add_block_to_block (pblock, &se.pre);
      step = gfc_evaluate_now (se.expr, pblock);
      dovar_decl = dovar;

      /* Special case simple loops.  */
      if (TREE_CODE (dovar) == VAR_DECL)
	{
	  if (integer_onep (step))
	    simple = 1;
	  else if (tree_int_cst_equal (step, integer_minus_one_node))
	    simple = -1;
	}
      else
	dovar_decl
	  = gfc_trans_omp_variable (code->ext.iterator->var->symtree->n.sym);

      /* Loop body.  */
      if (simple)
	{
	  TREE_VEC_ELT (init, i) = build2_v (MODIFY_EXPR, dovar, from);
	  TREE_VEC_ELT (cond, i) = fold_build2 (simple > 0 ? LE_EXPR : GE_EXPR,
						boolean_type_node, dovar, to);
	  TREE_VEC_ELT (incr, i) = fold_build2 (PLUS_EXPR, type, dovar, step);
	  TREE_VEC_ELT (incr, i) = fold_build2 (MODIFY_EXPR, type, dovar,
						TREE_VEC_ELT (incr, i));
	}
      else
	{
	  /* STEP is not 1 or -1.  Use:
	     for (count = 0; count < (to + step - from) / step; count++)
	       {
		 dovar = from + count * step;
		 body;
	       cycle_label:;
	       }  */
	  tmp = fold_build2 (MINUS_EXPR, type, step, from);
	  tmp = fold_build2 (PLUS_EXPR, type, to, tmp);
	  tmp = fold_build2 (TRUNC_DIV_EXPR, type, tmp, step);
	  tmp = gfc_evaluate_now (tmp, pblock);
	  count = gfc_create_var (type, "count");
	  TREE_VEC_ELT (init, i) = build2_v (MODIFY_EXPR, count,
					     build_int_cst (type, 0));
	  TREE_VEC_ELT (cond, i) = fold_build2 (LT_EXPR, boolean_type_node,
						count, tmp);
	  TREE_VEC_ELT (incr, i) = fold_build2 (PLUS_EXPR, type, count,
						build_int_cst (type, 1));
	  TREE_VEC_ELT (incr, i) = fold_build2 (MODIFY_EXPR, type,
						count, TREE_VEC_ELT (incr, i));

	  /* Initialize DOVAR.  */
	  tmp = fold_build2 (MULT_EXPR, type, count, step);
	  tmp = fold_build2 (PLUS_EXPR, type, from, tmp);
	  dovar_init = tree_cons (dovar, tmp, dovar_init);
	}

      if (!dovar_found)
	{
	  tmp = build_omp_clause (input_location, OMP_CLAUSE_PRIVATE);
	  OMP_CLAUSE_DECL (tmp) = dovar_decl;
	  omp_clauses = gfc_trans_add_clause (tmp, omp_clauses);
	}
      else if (dovar_found == 2)
	{
	  tree c = NULL;

	  tmp = NULL;
	  if (!simple)
	    {
	      /* If dovar is lastprivate, but different counter is used,
		 dovar += step needs to be added to
		 OMP_CLAUSE_LASTPRIVATE_STMT, otherwise the copied dovar
		 will have the value on entry of the last loop, rather
		 than value after iterator increment.  */
	      tmp = gfc_evaluate_now (step, pblock);
	      tmp = fold_build2 (PLUS_EXPR, type, dovar, tmp);
	      tmp = fold_build2 (MODIFY_EXPR, type, dovar, tmp);
	      for (c = omp_clauses; c ; c = OMP_CLAUSE_CHAIN (c))
		if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LASTPRIVATE
		    && OMP_CLAUSE_DECL (c) == dovar_decl)
		  {
		    OMP_CLAUSE_LASTPRIVATE_STMT (c) = tmp;
		    break;
		  }
	    }
	  if (c == NULL && par_clauses != NULL)
	    {
	      for (c = par_clauses; c ; c = OMP_CLAUSE_CHAIN (c))
		if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LASTPRIVATE
		    && OMP_CLAUSE_DECL (c) == dovar_decl)
		  {
		    tree l = build_omp_clause (input_location,
					       OMP_CLAUSE_LASTPRIVATE);
		    OMP_CLAUSE_DECL (l) = dovar_decl;
		    OMP_CLAUSE_CHAIN (l) = omp_clauses;
		    OMP_CLAUSE_LASTPRIVATE_STMT (l) = tmp;
		    omp_clauses = l;
		    OMP_CLAUSE_SET_CODE (c, OMP_CLAUSE_SHARED);
		    break;
		  }
	    }
	  gcc_assert (simple || c != NULL);
	}
      if (!simple)
	{
	  tmp = build_omp_clause (input_location, OMP_CLAUSE_PRIVATE);
	  OMP_CLAUSE_DECL (tmp) = count;
	  omp_clauses = gfc_trans_add_clause (tmp, omp_clauses);
	}

      if (i + 1 < collapse)
	code = code->block->next;
    }

  if (pblock != &block)
    {
      pushlevel (0);
      gfc_start_block (&block);
    }

  gfc_start_block (&body);

  dovar_init = nreverse (dovar_init);
  while (dovar_init)
    {
      gfc_add_modify (&body, TREE_PURPOSE (dovar_init),
			   TREE_VALUE (dovar_init));
      dovar_init = TREE_CHAIN (dovar_init);
    }

  /* Cycle statement is implemented with a goto.  Exit statement must not be
     present for this loop.  */
  cycle_label = gfc_build_label_decl (NULL_TREE);

  /* Put these labels where they can be found later. We put the
     labels in a TREE_LIST node (because TREE_CHAIN is already
     used). cycle_label goes in TREE_PURPOSE (backend_decl), exit
     label in TREE_VALUE (backend_decl).  */

  code->block->backend_decl = tree_cons (cycle_label, NULL, NULL);

  /* Main loop body.  */
  tmp = gfc_trans_omp_code (code->block->next, true);
  gfc_add_expr_to_block (&body, tmp);

  /* Label for cycle statements (if needed).  */
  if (TREE_USED (cycle_label))
    {
      tmp = build1_v (LABEL_EXPR, cycle_label);
      gfc_add_expr_to_block (&body, tmp);
    }

  /* End of loop body.  */
  stmt = make_node (OMP_FOR);

  TREE_TYPE (stmt) = void_type_node;
  OMP_FOR_BODY (stmt) = gfc_finish_block (&body);
  OMP_FOR_CLAUSES (stmt) = omp_clauses;
  OMP_FOR_INIT (stmt) = init;
  OMP_FOR_COND (stmt) = cond;
  OMP_FOR_INCR (stmt) = incr;
  gfc_add_expr_to_block (&block, stmt);

  return gfc_finish_block (&block);
}

static tree
gfc_trans_omp_flush (void)
{
  tree decl = built_in_decls [BUILT_IN_SYNCHRONIZE];
  return build_call_expr_loc (input_location, decl, 0);
}

static tree
gfc_trans_omp_master (gfc_code *code)
{
  tree stmt = gfc_trans_code (code->block->next);
  if (IS_EMPTY_STMT (stmt))
    return stmt;
  return build1_v (OMP_MASTER, stmt);
}

static tree
gfc_trans_omp_ordered (gfc_code *code)
{
  return build1_v (OMP_ORDERED, gfc_trans_code (code->block->next));
}

static tree
gfc_trans_omp_parallel (gfc_code *code)
{
  stmtblock_t block;
  tree stmt, omp_clauses;

  gfc_start_block (&block);
  omp_clauses = gfc_trans_omp_clauses (&block, code->ext.omp_clauses,
				       code->loc);
  stmt = gfc_trans_omp_code (code->block->next, true);
  stmt = build2 (OMP_PARALLEL, void_type_node, stmt, omp_clauses);
  gfc_add_expr_to_block (&block, stmt);
  return gfc_finish_block (&block);
}

static tree
gfc_trans_omp_parallel_do (gfc_code *code)
{
  stmtblock_t block, *pblock = NULL;
  gfc_omp_clauses parallel_clauses, do_clauses;
  tree stmt, omp_clauses = NULL_TREE;

  gfc_start_block (&block);

  memset (&do_clauses, 0, sizeof (do_clauses));
  if (code->ext.omp_clauses != NULL)
    {
      memcpy (&parallel_clauses, code->ext.omp_clauses,
	      sizeof (parallel_clauses));
      do_clauses.sched_kind = parallel_clauses.sched_kind;
      do_clauses.chunk_size = parallel_clauses.chunk_size;
      do_clauses.ordered = parallel_clauses.ordered;
      do_clauses.collapse = parallel_clauses.collapse;
      parallel_clauses.sched_kind = OMP_SCHED_NONE;
      parallel_clauses.chunk_size = NULL;
      parallel_clauses.ordered = false;
      parallel_clauses.collapse = 0;
      omp_clauses = gfc_trans_omp_clauses (&block, &parallel_clauses,
					   code->loc);
    }
  do_clauses.nowait = true;
  if (!do_clauses.ordered && do_clauses.sched_kind != OMP_SCHED_STATIC)
    pblock = &block;
  else
    pushlevel (0);
  stmt = gfc_trans_omp_do (code, pblock, &do_clauses, omp_clauses);
  if (TREE_CODE (stmt) != BIND_EXPR)
    stmt = build3_v (BIND_EXPR, NULL, stmt, poplevel (1, 0, 0));
  else
    poplevel (0, 0, 0);
  stmt = build2 (OMP_PARALLEL, void_type_node, stmt, omp_clauses);
  OMP_PARALLEL_COMBINED (stmt) = 1;
  gfc_add_expr_to_block (&block, stmt);
  return gfc_finish_block (&block);
}

static tree
gfc_trans_omp_parallel_sections (gfc_code *code)
{
  stmtblock_t block;
  gfc_omp_clauses section_clauses;
  tree stmt, omp_clauses;

  memset (&section_clauses, 0, sizeof (section_clauses));
  section_clauses.nowait = true;

  gfc_start_block (&block);
  omp_clauses = gfc_trans_omp_clauses (&block, code->ext.omp_clauses,
				       code->loc);
  pushlevel (0);
  stmt = gfc_trans_omp_sections (code, &section_clauses);
  if (TREE_CODE (stmt) != BIND_EXPR)
    stmt = build3_v (BIND_EXPR, NULL, stmt, poplevel (1, 0, 0));
  else
    poplevel (0, 0, 0);
  stmt = build2 (OMP_PARALLEL, void_type_node, stmt, omp_clauses);
  OMP_PARALLEL_COMBINED (stmt) = 1;
  gfc_add_expr_to_block (&block, stmt);
  return gfc_finish_block (&block);
}

static tree
gfc_trans_omp_parallel_workshare (gfc_code *code)
{
  stmtblock_t block;
  gfc_omp_clauses workshare_clauses;
  tree stmt, omp_clauses;

  memset (&workshare_clauses, 0, sizeof (workshare_clauses));
  workshare_clauses.nowait = true;

  gfc_start_block (&block);
  omp_clauses = gfc_trans_omp_clauses (&block, code->ext.omp_clauses,
				       code->loc);
  pushlevel (0);
  stmt = gfc_trans_omp_workshare (code, &workshare_clauses);
  if (TREE_CODE (stmt) != BIND_EXPR)
    stmt = build3_v (BIND_EXPR, NULL, stmt, poplevel (1, 0, 0));
  else
    poplevel (0, 0, 0);
  stmt = build2 (OMP_PARALLEL, void_type_node, stmt, omp_clauses);
  OMP_PARALLEL_COMBINED (stmt) = 1;
  gfc_add_expr_to_block (&block, stmt);
  return gfc_finish_block (&block);
}

static tree
gfc_trans_omp_sections (gfc_code *code, gfc_omp_clauses *clauses)
{
  stmtblock_t block, body;
  tree omp_clauses, stmt;
  bool has_lastprivate = clauses->lists[OMP_LIST_LASTPRIVATE] != NULL;

  gfc_start_block (&block);

  omp_clauses = gfc_trans_omp_clauses (&block, clauses, code->loc);

  gfc_init_block (&body);
  for (code = code->block; code; code = code->block)
    {
      /* Last section is special because of lastprivate, so even if it
	 is empty, chain it in.  */
      stmt = gfc_trans_omp_code (code->next,
				 has_lastprivate && code->block == NULL);
      if (! IS_EMPTY_STMT (stmt))
	{
	  stmt = build1_v (OMP_SECTION, stmt);
	  gfc_add_expr_to_block (&body, stmt);
	}
    }
  stmt = gfc_finish_block (&body);

  stmt = build2 (OMP_SECTIONS, void_type_node, stmt, omp_clauses);
  gfc_add_expr_to_block (&block, stmt);

  return gfc_finish_block (&block);
}

static tree
gfc_trans_omp_single (gfc_code *code, gfc_omp_clauses *clauses)
{
  tree omp_clauses = gfc_trans_omp_clauses (NULL, clauses, code->loc);
  tree stmt = gfc_trans_omp_code (code->block->next, true);
  stmt = build2 (OMP_SINGLE, void_type_node, stmt, omp_clauses);
  return stmt;
}

static tree
gfc_trans_omp_task (gfc_code *code)
{
  stmtblock_t block;
  tree stmt, omp_clauses;

  gfc_start_block (&block);
  omp_clauses = gfc_trans_omp_clauses (&block, code->ext.omp_clauses,
				       code->loc);
  stmt = gfc_trans_omp_code (code->block->next, true);
  stmt = build2 (OMP_TASK, void_type_node, stmt, omp_clauses);
  gfc_add_expr_to_block (&block, stmt);
  return gfc_finish_block (&block);
}

static tree
gfc_trans_omp_taskwait (void)
{
  tree decl = built_in_decls [BUILT_IN_GOMP_TASKWAIT];
  return build_call_expr_loc (input_location, decl, 0);
}

static tree
gfc_trans_omp_workshare (gfc_code *code, gfc_omp_clauses *clauses)
{
  tree res, tmp, stmt;
  stmtblock_t block, *pblock = NULL;
  stmtblock_t singleblock;
  int saved_ompws_flags;
  bool singleblock_in_progress = false;
  /* True if previous gfc_code in workshare construct is not workshared.  */
  bool prev_singleunit;

  code = code->block->next;

  pushlevel (0);

  if (!code)
    return build_empty_stmt (input_location);

  gfc_start_block (&block);
  pblock = &block;

  ompws_flags = OMPWS_WORKSHARE_FLAG;
  prev_singleunit = false;

  /* Translate statements one by one to trees until we reach
     the end of the workshare construct.  Adjacent gfc_codes that
     are a single unit of work are clustered and encapsulated in a
     single OMP_SINGLE construct.  */
  for (; code; code = code->next)
    {
      if (code->here != 0)
	{
	  res = gfc_trans_label_here (code);
	  gfc_add_expr_to_block (pblock, res);
	}

      /* No dependence analysis, use for clauses with wait.
	 If this is the last gfc_code, use default omp_clauses.  */
      if (code->next == NULL && clauses->nowait)
	ompws_flags |= OMPWS_NOWAIT;

      /* By default, every gfc_code is a single unit of work.  */
      ompws_flags |= OMPWS_CURR_SINGLEUNIT;
      ompws_flags &= ~OMPWS_SCALARIZER_WS;

      switch (code->op)
	{
	case EXEC_NOP:
	  res = NULL_TREE;
	  break;

	case EXEC_ASSIGN:
	  res = gfc_trans_assign (code);
	  break;

	case EXEC_POINTER_ASSIGN:
	  res = gfc_trans_pointer_assign (code);
	  break;

	case EXEC_INIT_ASSIGN:
	  res = gfc_trans_init_assign (code);
	  break;

	case EXEC_FORALL:
	  res = gfc_trans_forall (code);
	  break;

	case EXEC_WHERE:
	  res = gfc_trans_where (code);
	  break;

	case EXEC_OMP_ATOMIC:
	  res = gfc_trans_omp_directive (code);
	  break;

	case EXEC_OMP_PARALLEL:
	case EXEC_OMP_PARALLEL_DO:
	case EXEC_OMP_PARALLEL_SECTIONS:
	case EXEC_OMP_PARALLEL_WORKSHARE:
	case EXEC_OMP_CRITICAL:
	  saved_ompws_flags = ompws_flags;
	  ompws_flags = 0;
	  res = gfc_trans_omp_directive (code);
	  ompws_flags = saved_ompws_flags;
	  break;
	
	default:
	  internal_error ("gfc_trans_omp_workshare(): Bad statement code");
	}

      gfc_set_backend_locus (&code->loc);

      if (res != NULL_TREE && ! IS_EMPTY_STMT (res))
	{
	  if (prev_singleunit)
	    {
	      if (ompws_flags & OMPWS_CURR_SINGLEUNIT)
		/* Add current gfc_code to single block.  */
		gfc_add_expr_to_block (&singleblock, res);
	      else
		{
		  /* Finish single block and add it to pblock.  */
		  tmp = gfc_finish_block (&singleblock);
		  tmp = build2 (OMP_SINGLE, void_type_node, tmp, NULL_TREE);
		  gfc_add_expr_to_block (pblock, tmp);
		  /* Add current gfc_code to pblock.  */
		  gfc_add_expr_to_block (pblock, res);
		  singleblock_in_progress = false;
		}
	    }
	  else
	    {
	      if (ompws_flags & OMPWS_CURR_SINGLEUNIT)
		{
		  /* Start single block.  */
		  gfc_init_block (&singleblock);
		  gfc_add_expr_to_block (&singleblock, res);
		  singleblock_in_progress = true;
		}
	      else
		/* Add the new statement to the block.  */
		gfc_add_expr_to_block (pblock, res);
	    }
	  prev_singleunit = (ompws_flags & OMPWS_CURR_SINGLEUNIT) != 0;
	}
    }

  /* Finish remaining SINGLE block, if we were in the middle of one.  */
  if (singleblock_in_progress)
    {
      /* Finish single block and add it to pblock.  */
      tmp = gfc_finish_block (&singleblock);
      tmp = build2 (OMP_SINGLE, void_type_node, tmp,
		    clauses->nowait
		    ? build_omp_clause (input_location, OMP_CLAUSE_NOWAIT)
		    : NULL_TREE);
      gfc_add_expr_to_block (pblock, tmp);
    }

  stmt = gfc_finish_block (pblock);
  if (TREE_CODE (stmt) != BIND_EXPR)
    {
      if (!IS_EMPTY_STMT (stmt))
	{
	  tree bindblock = poplevel (1, 0, 0);
	  stmt = build3_v (BIND_EXPR, NULL, stmt, bindblock);
	}
      else
	poplevel (0, 0, 0);
    }
  else
    poplevel (0, 0, 0);

  ompws_flags = 0;
  return stmt;
}

tree
gfc_trans_omp_directive (gfc_code *code)
{
  switch (code->op)
    {
    case EXEC_OMP_ATOMIC:
      return gfc_trans_omp_atomic (code);
    case EXEC_OMP_BARRIER:
      return gfc_trans_omp_barrier ();
    case EXEC_OMP_CRITICAL:
      return gfc_trans_omp_critical (code);
    case EXEC_OMP_DO:
      return gfc_trans_omp_do (code, NULL, code->ext.omp_clauses, NULL);
    case EXEC_OMP_FLUSH:
      return gfc_trans_omp_flush ();
    case EXEC_OMP_MASTER:
      return gfc_trans_omp_master (code);
    case EXEC_OMP_ORDERED:
      return gfc_trans_omp_ordered (code);
    case EXEC_OMP_PARALLEL:
      return gfc_trans_omp_parallel (code);
    case EXEC_OMP_PARALLEL_DO:
      return gfc_trans_omp_parallel_do (code);
    case EXEC_OMP_PARALLEL_SECTIONS:
      return gfc_trans_omp_parallel_sections (code);
    case EXEC_OMP_PARALLEL_WORKSHARE:
      return gfc_trans_omp_parallel_workshare (code);
    case EXEC_OMP_SECTIONS:
      return gfc_trans_omp_sections (code, code->ext.omp_clauses);
    case EXEC_OMP_SINGLE:
      return gfc_trans_omp_single (code, code->ext.omp_clauses);
    case EXEC_OMP_TASK:
      return gfc_trans_omp_task (code);
    case EXEC_OMP_TASKWAIT:
      return gfc_trans_omp_taskwait ();
    case EXEC_OMP_WORKSHARE:
      return gfc_trans_omp_workshare (code, code->ext.omp_clauses);
    default:
      gcc_unreachable ();
    }
}
