/* OpenMP directive translation -- generate GCC trees from gfc_code.
   Copyright (C) 2005-2025 Free Software Foundation, Inc.
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
#include "options.h"
#include "tree.h"
#include "gfortran.h"
#include "gimple-expr.h"
#include "trans.h"
#include "stringpool.h"
#include "fold-const.h"
#include "gimplify.h"	/* For create_tmp_var_raw.  */
#include "trans-stmt.h"
#include "trans-types.h"
#include "trans-array.h"
#include "trans-const.h"
#include "arith.h"
#include "constructor.h"
#include "gomp-constants.h"
#include "omp-general.h"
#include "omp-low.h"
#include "memmodel.h"  /* For MEMMODEL_ enums.  */
#include "dependency.h"

#undef GCC_DIAG_STYLE
#define GCC_DIAG_STYLE __gcc_tdiag__
#include "diagnostic-core.h"
#undef GCC_DIAG_STYLE
#define GCC_DIAG_STYLE __gcc_gfc__
#include "attribs.h"
#include "function.h"

int ompws_flags;

/* True if OpenMP should regard this DECL as being a scalar which has Fortran's
   allocatable or pointer attribute.  */

bool
gfc_omp_is_allocatable_or_ptr (const_tree decl)
{
  return (DECL_P (decl)
	  && (GFC_DECL_GET_SCALAR_POINTER (decl)
	      || GFC_DECL_GET_SCALAR_ALLOCATABLE (decl)));
}

/* True if the argument is an optional argument; except that false is also
   returned for arguments with the value attribute (nonpointers) and for
   assumed-shape variables (decl is a local variable containing arg->data).
   Note that for 'procedure(), optional' the value false is used as that's
   always a pointer and no additional indirection is used.
   Note that pvoid_type_node is for 'type(c_ptr), value' (and c_funloc).  */

static bool
gfc_omp_is_optional_argument (const_tree decl)
{
  /* Note: VAR_DECL can occur with BIND(C) and array descriptors.  */
  return ((TREE_CODE (decl) == PARM_DECL || VAR_P (decl))
	  && DECL_LANG_SPECIFIC (decl)
	  && TREE_CODE (TREE_TYPE (decl)) == POINTER_TYPE
	  && !VOID_TYPE_P (TREE_TYPE (TREE_TYPE (decl)))
	  && TREE_CODE (TREE_TYPE (TREE_TYPE (decl))) != FUNCTION_TYPE
	  && GFC_DECL_OPTIONAL_ARGUMENT (decl));
}

/* Check whether this DECL belongs to a Fortran optional argument.
   With 'for_present_check' set to false, decls which are optional parameters
   themselves are returned as tree - or a NULL_TREE otherwise. Those decls are
   always pointers.  With 'for_present_check' set to true, the decl for checking
   whether an argument is present is returned; for arguments with value
   attribute this is the hidden argument and of BOOLEAN_TYPE.  If the decl is
   unrelated to optional arguments, NULL_TREE is returned.  */

tree
gfc_omp_check_optional_argument (tree decl, bool for_present_check)
{
  if (!for_present_check)
    return gfc_omp_is_optional_argument (decl) ? decl : NULL_TREE;

  if (!DECL_LANG_SPECIFIC (decl))
    return NULL_TREE;

  tree orig_decl = decl;

  /* For assumed-shape arrays, a local decl with arg->data is used.  */
  if (TREE_CODE (decl) != PARM_DECL
      && (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (decl))
	  || GFC_ARRAY_TYPE_P (TREE_TYPE (decl))))
    decl = GFC_DECL_SAVED_DESCRIPTOR (decl);

  /* Note: With BIND(C), array descriptors are converted to a VAR_DECL.  */
  if (decl == NULL_TREE
      || (TREE_CODE (decl) != PARM_DECL && TREE_CODE (decl) != VAR_DECL)
      || !DECL_LANG_SPECIFIC (decl)
      || !GFC_DECL_OPTIONAL_ARGUMENT (decl))
    return NULL_TREE;

   /* Scalars with VALUE attribute which are passed by value use a hidden
      argument to denote the present status.  They are passed as nonpointer type
      with one exception: 'type(c_ptr), value' as 'void*'.  */
   /* Cf. trans-expr.cc's gfc_conv_expr_present.  */
   if (TREE_CODE (TREE_TYPE (decl)) != POINTER_TYPE
       || VOID_TYPE_P (TREE_TYPE (TREE_TYPE (decl))))
    {
      char name[GFC_MAX_SYMBOL_LEN + 2];
      tree tree_name;

      name[0] = '.';
      strcpy (&name[1], IDENTIFIER_POINTER (DECL_NAME (decl)));
      tree_name = get_identifier (name);

      /* Walk function argument list to find the hidden arg.  */
      decl = DECL_ARGUMENTS (DECL_CONTEXT (decl));
      for ( ; decl != NULL_TREE; decl = TREE_CHAIN (decl))
	if (DECL_NAME (decl) == tree_name
	    && DECL_ARTIFICIAL (decl))
	  break;

      gcc_assert (decl);
      return decl;
    }

  return fold_build2_loc (input_location, NE_EXPR, boolean_type_node,
			  orig_decl, null_pointer_node);
}


/* Returns tree with NULL if it is not an array descriptor and with the tree to
   access the 'data' component otherwise.  With type_only = true, it returns the
   TREE_TYPE without creating a new tree.  */

tree
gfc_omp_array_data (tree decl, bool type_only)
{
  tree type = TREE_TYPE (decl);

  if (POINTER_TYPE_P (type))
    type = TREE_TYPE (type);

  if (!GFC_DESCRIPTOR_TYPE_P (type))
    return NULL_TREE;

  if (type_only)
    return GFC_TYPE_ARRAY_DATAPTR_TYPE (type);

  if (POINTER_TYPE_P (TREE_TYPE (decl)))
    decl = build_fold_indirect_ref (decl);

  decl = gfc_conv_descriptor_data_get (decl);
  STRIP_NOPS (decl);
  return decl;
}

/* Return the byte-size of the passed array descriptor. */

tree
gfc_omp_array_size (tree decl, gimple_seq *pre_p)
{
  stmtblock_t block;
  if (POINTER_TYPE_P (TREE_TYPE (decl)))
    decl = build_fold_indirect_ref (decl);
  tree type = TREE_TYPE (decl);
  gcc_assert (GFC_DESCRIPTOR_TYPE_P (type));
  bool allocatable = (GFC_TYPE_ARRAY_AKIND (type) == GFC_ARRAY_ALLOCATABLE
		      || GFC_TYPE_ARRAY_AKIND (type) == GFC_ARRAY_POINTER
		      || GFC_TYPE_ARRAY_AKIND (type) == GFC_ARRAY_POINTER_CONT);
  gfc_init_block (&block);
  tree size = gfc_full_array_size (&block, decl,
				   GFC_TYPE_ARRAY_RANK (TREE_TYPE (decl)));
  size = fold_convert (size_type_node, size);
  tree elemsz = gfc_get_element_type (TREE_TYPE (decl));
  if (TREE_CODE (elemsz) == ARRAY_TYPE && TYPE_STRING_FLAG (elemsz))
    elemsz = gfc_conv_descriptor_elem_len (decl);
  else
    elemsz = TYPE_SIZE_UNIT (elemsz);
  size = fold_build2 (MULT_EXPR, size_type_node, size, elemsz);
  if (!allocatable)
    gimplify_and_add (gfc_finish_block (&block), pre_p);
  else
    {
      tree var = create_tmp_var (size_type_node);
      gfc_add_expr_to_block (&block, build2 (MODIFY_EXPR, sizetype, var, size));
      tree tmp = fold_build2_loc (input_location, NE_EXPR, boolean_type_node,
				  gfc_conv_descriptor_data_get (decl),
				  null_pointer_node);
      tmp = build3_loc (input_location, COND_EXPR, void_type_node, tmp,
			gfc_finish_block (&block),
			build2 (MODIFY_EXPR, sizetype, var, size_zero_node));
      gimplify_and_add (tmp, pre_p);
      size = var;
    }
  return size;
}


/* True if OpenMP should privatize what this DECL points to rather
   than the DECL itself.  */

bool
gfc_omp_privatize_by_reference (const_tree decl)
{
  tree type = TREE_TYPE (decl);

  if (TREE_CODE (type) == REFERENCE_TYPE
      && (!DECL_ARTIFICIAL (decl) || TREE_CODE (decl) == PARM_DECL))
    return true;

  if (TREE_CODE (type) == POINTER_TYPE
      && gfc_omp_is_optional_argument (decl))
    return true;

  if (TREE_CODE (type) == POINTER_TYPE)
    {
      while (TREE_CODE (decl) == COMPONENT_REF)
	decl = TREE_OPERAND (decl, 1);

      /* Array POINTER/ALLOCATABLE have aggregate types, all user variables
	 that have POINTER_TYPE type and aren't scalar pointers, scalar
	 allocatables, Cray pointees or C pointers are supposed to be
	 privatized by reference.  */
      if (GFC_DECL_GET_SCALAR_POINTER (decl)
	  || GFC_DECL_GET_SCALAR_ALLOCATABLE (decl)
	  || GFC_DECL_CRAY_POINTEE (decl)
	  || GFC_DECL_ASSOCIATE_VAR_P (decl)
	  || VOID_TYPE_P (TREE_TYPE (TREE_TYPE (decl))))
	return false;

      if (!DECL_ARTIFICIAL (decl)
	  && TREE_CODE (TREE_TYPE (type)) != FUNCTION_TYPE)
	return true;

      /* Some arrays are expanded as DECL_ARTIFICIAL pointers
	 by the frontend.  */
      if (DECL_LANG_SPECIFIC (decl)
	  && GFC_DECL_SAVED_DESCRIPTOR (decl))
	return true;
    }

  return false;
}

/* OMP_CLAUSE_DEFAULT_UNSPECIFIED unless OpenMP sharing attribute
   of DECL is predetermined.  */

enum omp_clause_default_kind
gfc_omp_predetermined_sharing (tree decl)
{
  /* Associate names preserve the association established during ASSOCIATE.
     As they are implemented either as pointers to the selector or array
     descriptor and shouldn't really change in the ASSOCIATE region,
     this decl can be either shared or firstprivate.  If it is a pointer,
     use firstprivate, as it is cheaper that way, otherwise make it shared.  */
  if (GFC_DECL_ASSOCIATE_VAR_P (decl))
    {
      if (TREE_CODE (TREE_TYPE (decl)) == POINTER_TYPE)
	return OMP_CLAUSE_DEFAULT_FIRSTPRIVATE;
      else
	return OMP_CLAUSE_DEFAULT_SHARED;
    }

  if (DECL_ARTIFICIAL (decl)
      && ! GFC_DECL_RESULT (decl)
      && ! (DECL_LANG_SPECIFIC (decl)
	    && GFC_DECL_SAVED_DESCRIPTOR (decl)))
    return OMP_CLAUSE_DEFAULT_SHARED;

  /* Cray pointees shouldn't be listed in any clauses and should be
     gimplified to dereference of the corresponding Cray pointer.
     Make them all private, so that they are emitted in the debug
     information.  */
  if (GFC_DECL_CRAY_POINTEE (decl))
    return OMP_CLAUSE_DEFAULT_PRIVATE;

  /* Assumed-size arrays are predetermined shared.  */
  if (TREE_CODE (decl) == PARM_DECL
      && GFC_ARRAY_TYPE_P (TREE_TYPE (decl))
      && GFC_TYPE_ARRAY_AKIND (TREE_TYPE (decl)) == GFC_ARRAY_UNKNOWN
      && GFC_TYPE_ARRAY_UBOUND (TREE_TYPE (decl),
				GFC_TYPE_ARRAY_RANK (TREE_TYPE (decl)) - 1)
	 == NULL)
    return OMP_CLAUSE_DEFAULT_SHARED;

  /* Dummy procedures aren't considered variables by OpenMP, thus are
     disallowed in OpenMP clauses.  They are represented as PARM_DECLs
     in the middle-end, so return OMP_CLAUSE_DEFAULT_FIRSTPRIVATE here
     to avoid complaining about their uses with default(none).  */
  if (TREE_CODE (decl) == PARM_DECL
      && TREE_CODE (TREE_TYPE (decl)) == POINTER_TYPE
      && TREE_CODE (TREE_TYPE (TREE_TYPE (decl))) == FUNCTION_TYPE)
    return OMP_CLAUSE_DEFAULT_FIRSTPRIVATE;

  /* COMMON and EQUIVALENCE decls are shared.  They
     are only referenced through DECL_VALUE_EXPR of the variables
     contained in them.  If those are privatized, they will not be
     gimplified to the COMMON or EQUIVALENCE decls.  */
  if (GFC_DECL_COMMON_OR_EQUIV (decl) && ! DECL_HAS_VALUE_EXPR_P (decl))
    return OMP_CLAUSE_DEFAULT_SHARED;

  if (GFC_DECL_RESULT (decl) && ! DECL_HAS_VALUE_EXPR_P (decl))
    return OMP_CLAUSE_DEFAULT_SHARED;

  /* These are either array or derived parameters, or vtables.
     In the former cases, the OpenMP standard doesn't consider them to be
     variables at all (they can't be redefined), but they can nevertheless appear
     in parallel/task regions and for default(none) purposes treat them as shared.
     For vtables likely the same handling is desirable.  */
  if (VAR_P (decl) && TREE_READONLY (decl)
      && (TREE_STATIC (decl) || DECL_EXTERNAL (decl)))
    return OMP_CLAUSE_DEFAULT_SHARED;

  return OMP_CLAUSE_DEFAULT_UNSPECIFIED;
}


/* OMP_CLAUSE_DEFAULTMAP_CATEGORY_UNSPECIFIED unless OpenMP mapping attribute
   of DECL is predetermined.  */

enum omp_clause_defaultmap_kind
gfc_omp_predetermined_mapping (tree decl)
{
  if (DECL_ARTIFICIAL (decl)
      && ! GFC_DECL_RESULT (decl)
      && ! (DECL_LANG_SPECIFIC (decl)
	    && GFC_DECL_SAVED_DESCRIPTOR (decl)))
    return OMP_CLAUSE_DEFAULTMAP_TO;

  /* Dummy procedures aren't considered variables by OpenMP, thus are
     disallowed in OpenMP clauses.  They are represented as PARM_DECLs
     in the middle-end, so return OMP_CLAUSE_DEFAULTMAP_FIRSTPRIVATE here
     to avoid complaining about their uses with defaultmap(none).  */
  if (TREE_CODE (decl) == PARM_DECL
      && TREE_CODE (TREE_TYPE (decl)) == POINTER_TYPE
      && TREE_CODE (TREE_TYPE (TREE_TYPE (decl))) == FUNCTION_TYPE)
    return OMP_CLAUSE_DEFAULTMAP_FIRSTPRIVATE;

  /* These are either array or derived parameters, or vtables.  */
  if (VAR_P (decl) && TREE_READONLY (decl)
      && (TREE_STATIC (decl) || DECL_EXTERNAL (decl)))
    return OMP_CLAUSE_DEFAULTMAP_TO;

  return OMP_CLAUSE_DEFAULTMAP_CATEGORY_UNSPECIFIED;
}


/* Return decl that should be used when reporting DEFAULT(NONE)
   diagnostics.  */

tree
gfc_omp_report_decl (tree decl)
{
  if (DECL_ARTIFICIAL (decl)
      && DECL_LANG_SPECIFIC (decl)
      && GFC_DECL_SAVED_DESCRIPTOR (decl))
    return GFC_DECL_SAVED_DESCRIPTOR (decl);

  return decl;
}

/* Return true if TYPE has any allocatable components.  */

static bool
gfc_has_alloc_comps (tree type, tree decl)
{
  tree field, ftype;

  if (POINTER_TYPE_P (type))
    {
      if (GFC_DECL_GET_SCALAR_ALLOCATABLE (decl))
	type = TREE_TYPE (type);
      else if (GFC_DECL_GET_SCALAR_POINTER (decl))
	return false;
    }

  if (GFC_DESCRIPTOR_TYPE_P (type)
      && (GFC_TYPE_ARRAY_AKIND (type) == GFC_ARRAY_POINTER
	  || GFC_TYPE_ARRAY_AKIND (type) == GFC_ARRAY_POINTER_CONT))
    return false;

  if (GFC_DESCRIPTOR_TYPE_P (type) || GFC_ARRAY_TYPE_P (type))
    type = gfc_get_element_type (type);

  if (TREE_CODE (type) != RECORD_TYPE)
    return false;

  for (field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
    {
      ftype = TREE_TYPE (field);
      if (GFC_DECL_GET_SCALAR_ALLOCATABLE (field))
	return true;
      if (GFC_DESCRIPTOR_TYPE_P (ftype)
	  && GFC_TYPE_ARRAY_AKIND (ftype) == GFC_ARRAY_ALLOCATABLE)
	return true;
      if (gfc_has_alloc_comps (ftype, field))
	return true;
    }
  return false;
}

/* Return true if TYPE is polymorphic but not with pointer attribute.  */

static bool
gfc_is_polymorphic_nonptr (tree type)
{
  if (POINTER_TYPE_P (type))
    type = TREE_TYPE (type);
  return GFC_CLASS_TYPE_P (type);
}

/* Return true if TYPE is unlimited polymorphic but not with pointer attribute;
   unlimited means also intrinsic types are handled and _len is used.  */

static bool
gfc_is_unlimited_polymorphic_nonptr (tree type)
{
  if (POINTER_TYPE_P (type))
    type = TREE_TYPE (type);
  if (!GFC_CLASS_TYPE_P (type))
    return false;

  tree field = TYPE_FIELDS (type); /* _data */
  gcc_assert (field);
  field = DECL_CHAIN (field); /* _vptr */
  gcc_assert (field);
  field = DECL_CHAIN (field);
  if (!field)
    return false;
  gcc_assert (strcmp ("_len", IDENTIFIER_POINTER (DECL_NAME (field))) == 0);
  return true;
}

/* Return true if the DECL is for an allocatable array or scalar.  */

bool
gfc_omp_allocatable_p (tree decl)
{
  if (!DECL_P (decl))
    return false;

  if (GFC_DECL_GET_SCALAR_ALLOCATABLE (decl))
    return true;

  tree type = TREE_TYPE (decl);
  if (gfc_omp_privatize_by_reference (decl))
    type = TREE_TYPE (type);

  if (GFC_DESCRIPTOR_TYPE_P (type)
      && GFC_TYPE_ARRAY_AKIND (type) == GFC_ARRAY_ALLOCATABLE)
    return true;

  return false;
}


/* Return true if DECL in private clause needs
   OMP_CLAUSE_PRIVATE_OUTER_REF on the private clause.  */
bool
gfc_omp_private_outer_ref (tree decl)
{
  tree type = TREE_TYPE (decl);

  if (gfc_omp_privatize_by_reference (decl))
    type = TREE_TYPE (type);

  if (GFC_DESCRIPTOR_TYPE_P (type)
      && GFC_TYPE_ARRAY_AKIND (type) == GFC_ARRAY_ALLOCATABLE)
    return true;

  if (GFC_DECL_GET_SCALAR_ALLOCATABLE (decl))
    return true;

  if (gfc_has_alloc_comps (type, decl))
    return true;

  return false;
}

/* Callback for gfc_omp_unshare_expr.  */

static tree
gfc_omp_unshare_expr_r (tree *tp, int *walk_subtrees, void *)
{
  tree t = *tp;
  enum tree_code code = TREE_CODE (t);

  /* Stop at types, decls, constants like copy_tree_r.  */
  if (TREE_CODE_CLASS (code) == tcc_type
      || TREE_CODE_CLASS (code) == tcc_declaration
      || TREE_CODE_CLASS (code) == tcc_constant
      || code == BLOCK)
    *walk_subtrees = 0;
  else if (handled_component_p (t)
	   || TREE_CODE (t) == MEM_REF)
    {
      *tp = unshare_expr (t);
      *walk_subtrees = 0;
    }

  return NULL_TREE;
}

/* Unshare in expr anything that the FE which normally doesn't
   care much about tree sharing (because during gimplification
   everything is unshared) could cause problems with tree sharing
   at omp-low.cc time.  */

static tree
gfc_omp_unshare_expr (tree expr)
{
  walk_tree (&expr, gfc_omp_unshare_expr_r, NULL, NULL);
  return expr;
}

enum walk_alloc_comps
{
  WALK_ALLOC_COMPS_DTOR,
  WALK_ALLOC_COMPS_DEFAULT_CTOR,
  WALK_ALLOC_COMPS_COPY_CTOR
};

/* Handle allocatable components in OpenMP clauses.  */

static tree
gfc_walk_alloc_comps (tree decl, tree dest, tree var,
		      enum walk_alloc_comps kind)
{
  stmtblock_t block, tmpblock;
  tree type = TREE_TYPE (decl), then_b, tem, field;
  gfc_init_block (&block);

  if (GFC_ARRAY_TYPE_P (type) || GFC_DESCRIPTOR_TYPE_P (type))
    {
      if (GFC_DESCRIPTOR_TYPE_P (type))
	{
	  gfc_init_block (&tmpblock);
	  tem = gfc_full_array_size (&tmpblock, decl,
				     GFC_TYPE_ARRAY_RANK (type));
	  then_b = gfc_finish_block (&tmpblock);
	  gfc_add_expr_to_block (&block, gfc_omp_unshare_expr (then_b));
	  tem = gfc_omp_unshare_expr (tem);
	  tem = fold_build2_loc (input_location, MINUS_EXPR,
				 gfc_array_index_type, tem,
				 gfc_index_one_node);
	}
      else
	{
	  bool compute_nelts = false;
	  if (!TYPE_DOMAIN (type)
	      || TYPE_MAX_VALUE (TYPE_DOMAIN (type)) == NULL_TREE
	      || TYPE_MIN_VALUE (TYPE_DOMAIN (type)) == error_mark_node
	      || TYPE_MAX_VALUE (TYPE_DOMAIN (type)) == error_mark_node)
	    compute_nelts = true;
	  else if (VAR_P (TYPE_MAX_VALUE (TYPE_DOMAIN (type))))
	    {
	      tree a = DECL_ATTRIBUTES (TYPE_MAX_VALUE (TYPE_DOMAIN (type)));
	      if (lookup_attribute ("omp dummy var", a))
		compute_nelts = true;
	    }
	  if (compute_nelts)
	    {
	      tem = fold_build2 (EXACT_DIV_EXPR, sizetype,
				 TYPE_SIZE_UNIT (type),
				 TYPE_SIZE_UNIT (TREE_TYPE (type)));
	      tem = size_binop (MINUS_EXPR, tem, size_one_node);
	    }
	  else
	    tem = array_type_nelts_minus_one (type);
	  tem = fold_convert (gfc_array_index_type, tem);
	}

      tree nelems = gfc_evaluate_now (tem, &block);
      tree index = gfc_create_var (gfc_array_index_type, "S");

      gfc_init_block (&tmpblock);
      tem = gfc_conv_array_data (decl);
      tree declvar = build_fold_indirect_ref_loc (input_location, tem);
      tree declvref = gfc_build_array_ref (declvar, index, NULL);
      tree destvar, destvref = NULL_TREE;
      if (dest)
	{
	  tem = gfc_conv_array_data (dest);
	  destvar = build_fold_indirect_ref_loc (input_location, tem);
	  destvref = gfc_build_array_ref (destvar, index, NULL);
	}
      gfc_add_expr_to_block (&tmpblock,
			     gfc_walk_alloc_comps (declvref, destvref,
						   var, kind));

      gfc_loopinfo loop;
      gfc_init_loopinfo (&loop);
      loop.dimen = 1;
      loop.from[0] = gfc_index_zero_node;
      loop.loopvar[0] = index;
      loop.to[0] = nelems;
      gfc_trans_scalarizing_loops (&loop, &tmpblock);
      gfc_add_block_to_block (&block, &loop.pre);
      return gfc_finish_block (&block);
    }
  else if (GFC_DECL_GET_SCALAR_ALLOCATABLE (var))
    {
      decl = build_fold_indirect_ref_loc (input_location, decl);
      if (dest)
	dest = build_fold_indirect_ref_loc (input_location, dest);
      type = TREE_TYPE (decl);
    }

  gcc_assert (TREE_CODE (type) == RECORD_TYPE);
  for (field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
    {
      tree ftype = TREE_TYPE (field);
      tree declf, destf = NULL_TREE;
      bool has_alloc_comps = gfc_has_alloc_comps (ftype, field);
      if ((!GFC_DESCRIPTOR_TYPE_P (ftype)
	   || GFC_TYPE_ARRAY_AKIND (ftype) != GFC_ARRAY_ALLOCATABLE)
	  && !GFC_DECL_GET_SCALAR_ALLOCATABLE (field)
	  && !has_alloc_comps)
	continue;
      declf = fold_build3_loc (input_location, COMPONENT_REF, ftype,
			       decl, field, NULL_TREE);
      if (dest)
	destf = fold_build3_loc (input_location, COMPONENT_REF, ftype,
				 dest, field, NULL_TREE);

      tem = NULL_TREE;
      switch (kind)
	{
	case WALK_ALLOC_COMPS_DTOR:
	  break;
	case WALK_ALLOC_COMPS_DEFAULT_CTOR:
	  if (GFC_DESCRIPTOR_TYPE_P (ftype)
	      && GFC_TYPE_ARRAY_AKIND (ftype) == GFC_ARRAY_ALLOCATABLE)
	    {
	      gfc_add_modify (&block, unshare_expr (destf),
			      unshare_expr (declf));
	      tem = gfc_duplicate_allocatable_nocopy
					(destf, declf, ftype,
					 GFC_TYPE_ARRAY_RANK (ftype));
	    }
	  else if (GFC_DECL_GET_SCALAR_ALLOCATABLE (field))
	    tem = gfc_duplicate_allocatable_nocopy (destf, declf, ftype, 0);
	  break;
	case WALK_ALLOC_COMPS_COPY_CTOR:
	  if (GFC_DESCRIPTOR_TYPE_P (ftype)
	      && GFC_TYPE_ARRAY_AKIND (ftype) == GFC_ARRAY_ALLOCATABLE)
	    tem = gfc_duplicate_allocatable (destf, declf, ftype,
					     GFC_TYPE_ARRAY_RANK (ftype),
					     NULL_TREE);
	  else if (GFC_DECL_GET_SCALAR_ALLOCATABLE (field))
	    tem = gfc_duplicate_allocatable (destf, declf, ftype, 0,
					     NULL_TREE);
	  break;
	}
      if (tem)
	gfc_add_expr_to_block (&block, gfc_omp_unshare_expr (tem));
      if (has_alloc_comps)
	{
	  gfc_init_block (&tmpblock);
	  gfc_add_expr_to_block (&tmpblock,
				 gfc_walk_alloc_comps (declf, destf,
						       field, kind));
	  then_b = gfc_finish_block (&tmpblock);
	  if (GFC_DESCRIPTOR_TYPE_P (ftype)
	      && GFC_TYPE_ARRAY_AKIND (ftype) == GFC_ARRAY_ALLOCATABLE)
	    tem = gfc_conv_descriptor_data_get (unshare_expr (declf));
	  else if (GFC_DECL_GET_SCALAR_ALLOCATABLE (field))
	    tem = unshare_expr (declf);
	  else
	    tem = NULL_TREE;
	  if (tem)
	    {
	      tem = fold_convert (pvoid_type_node, tem);
	      tem = fold_build2_loc (input_location, NE_EXPR,
				     logical_type_node, tem,
				     null_pointer_node);
	      then_b = build3_loc (input_location, COND_EXPR, void_type_node,
				   tem, then_b,
				   build_empty_stmt (input_location));
	    }
	  gfc_add_expr_to_block (&block, then_b);
	}
      if (kind == WALK_ALLOC_COMPS_DTOR)
	{
	  if (GFC_DESCRIPTOR_TYPE_P (ftype)
	      && GFC_TYPE_ARRAY_AKIND (ftype) == GFC_ARRAY_ALLOCATABLE)
	    {
	      tem = gfc_conv_descriptor_data_get (unshare_expr (declf));
	      tem = gfc_deallocate_with_status (tem, NULL_TREE, NULL_TREE,
						NULL_TREE, NULL_TREE, true,
						NULL,
						GFC_CAF_COARRAY_NOCOARRAY);
	      gfc_add_expr_to_block (&block, gfc_omp_unshare_expr (tem));
	    }
	  else if (GFC_DECL_GET_SCALAR_ALLOCATABLE (field))
	    {
	      tem = gfc_call_free (unshare_expr (declf));
	      gfc_add_expr_to_block (&block, gfc_omp_unshare_expr (tem));
	    }
	}
    }

  return gfc_finish_block (&block);
}

/* Return code to initialize DECL with its default constructor, or
   NULL if there's nothing to do.  */

tree
gfc_omp_clause_default_ctor (tree clause, tree decl, tree outer)
{
  tree type = TREE_TYPE (decl), size, ptr, cond, then_b, else_b;
  stmtblock_t block, cond_block;

  switch (OMP_CLAUSE_CODE (clause))
    {
    case OMP_CLAUSE__LOOPTEMP_:
    case OMP_CLAUSE__REDUCTEMP_:
    case OMP_CLAUSE__CONDTEMP_:
    case OMP_CLAUSE__SCANTEMP_:
      return NULL;
    case OMP_CLAUSE_PRIVATE:
    case OMP_CLAUSE_LASTPRIVATE:
    case OMP_CLAUSE_LINEAR:
    case OMP_CLAUSE_REDUCTION:
    case OMP_CLAUSE_IN_REDUCTION:
    case OMP_CLAUSE_TASK_REDUCTION:
      break;
    default:
      gcc_unreachable ();
    }

  if ((! GFC_DESCRIPTOR_TYPE_P (type)
       || GFC_TYPE_ARRAY_AKIND (type) != GFC_ARRAY_ALLOCATABLE)
      && (!GFC_DECL_GET_SCALAR_ALLOCATABLE (OMP_CLAUSE_DECL (clause))
	  || !POINTER_TYPE_P (type)))
    {
      if (gfc_has_alloc_comps (type, OMP_CLAUSE_DECL (clause)))
	{
	  gcc_assert (outer);
	  gfc_start_block (&block);
	  tree tem = gfc_walk_alloc_comps (outer, decl,
					   OMP_CLAUSE_DECL (clause),
					   WALK_ALLOC_COMPS_DEFAULT_CTOR);
	  gfc_add_expr_to_block (&block, tem);
	  return gfc_finish_block (&block);
	}
      return NULL_TREE;
    }

  gcc_assert (outer != NULL_TREE);

  /* Allocatable arrays and scalars in PRIVATE clauses need to be set to
     "not currently allocated" allocation status if outer
     array is "not currently allocated", otherwise should be allocated.  */
  gfc_start_block (&block);

  gfc_init_block (&cond_block);

  if (GFC_DESCRIPTOR_TYPE_P (type))
    {
      gfc_add_modify (&cond_block, decl, outer);
      tree rank = gfc_rank_cst[GFC_TYPE_ARRAY_RANK (type) - 1];
      size = gfc_conv_descriptor_ubound_get (decl, rank);
      size = fold_build2_loc (input_location, MINUS_EXPR, gfc_array_index_type,
			      size,
			      gfc_conv_descriptor_lbound_get (decl, rank));
      size = fold_build2_loc (input_location, PLUS_EXPR, gfc_array_index_type,
			      size, gfc_index_one_node);
      if (GFC_TYPE_ARRAY_RANK (type) > 1)
	size = fold_build2_loc (input_location, MULT_EXPR,
				gfc_array_index_type, size,
				gfc_conv_descriptor_stride_get (decl, rank));
      tree esize = fold_convert (gfc_array_index_type,
				 TYPE_SIZE_UNIT (gfc_get_element_type (type)));
      size = fold_build2_loc (input_location, MULT_EXPR, gfc_array_index_type,
			      size, esize);
      size = unshare_expr (size);
      size = gfc_evaluate_now (fold_convert (size_type_node, size),
			       &cond_block);
    }
  else
    size = fold_convert (size_type_node, TYPE_SIZE_UNIT (TREE_TYPE (type)));
  ptr = gfc_create_var (pvoid_type_node, NULL);
  gfc_allocate_using_malloc (&cond_block, ptr, size, NULL_TREE);
  if (GFC_DESCRIPTOR_TYPE_P (type))
    gfc_conv_descriptor_data_set (&cond_block, unshare_expr (decl), ptr);
  else
    gfc_add_modify (&cond_block, unshare_expr (decl),
		    fold_convert (TREE_TYPE (decl), ptr));
  if (gfc_has_alloc_comps (type, OMP_CLAUSE_DECL (clause)))
    {
      tree tem = gfc_walk_alloc_comps (outer, decl,
				       OMP_CLAUSE_DECL (clause),
				       WALK_ALLOC_COMPS_DEFAULT_CTOR);
      gfc_add_expr_to_block (&cond_block, tem);
    }
  then_b = gfc_finish_block (&cond_block);

  /* Reduction clause requires allocated ALLOCATABLE.  */
  if (OMP_CLAUSE_CODE (clause) != OMP_CLAUSE_REDUCTION
      && OMP_CLAUSE_CODE (clause) != OMP_CLAUSE_IN_REDUCTION
      && OMP_CLAUSE_CODE (clause) != OMP_CLAUSE_TASK_REDUCTION)
    {
      gfc_init_block (&cond_block);
      if (GFC_DESCRIPTOR_TYPE_P (type))
	gfc_conv_descriptor_data_set (&cond_block, unshare_expr (decl),
				      null_pointer_node);
      else
	gfc_add_modify (&cond_block, unshare_expr (decl),
			build_zero_cst (TREE_TYPE (decl)));
      else_b = gfc_finish_block (&cond_block);

      tree tem = fold_convert (pvoid_type_node,
			       GFC_DESCRIPTOR_TYPE_P (type)
			       ? gfc_conv_descriptor_data_get (outer) : outer);
      tem = unshare_expr (tem);
      cond = fold_build2_loc (input_location, NE_EXPR, logical_type_node,
			      tem, null_pointer_node);
      gfc_add_expr_to_block (&block,
			     build3_loc (input_location, COND_EXPR,
					 void_type_node, cond, then_b,
					 else_b));
      /* Avoid -W*uninitialized warnings.  */
      if (DECL_P (decl))
	suppress_warning (decl, OPT_Wuninitialized);
    }
  else
    gfc_add_expr_to_block (&block, then_b);

  return gfc_finish_block (&block);
}

/* Build and return code for a copy constructor from SRC to DEST.  */

tree
gfc_omp_clause_copy_ctor (tree clause, tree dest, tree src)
{
  tree type = TREE_TYPE (dest), ptr, size, call;
  tree decl_type = TREE_TYPE (OMP_CLAUSE_DECL (clause));
  tree cond, then_b, else_b;
  stmtblock_t block, cond_block;

  gcc_assert (OMP_CLAUSE_CODE (clause) == OMP_CLAUSE_FIRSTPRIVATE
	      || OMP_CLAUSE_CODE (clause) == OMP_CLAUSE_LINEAR);

  /* Privatize pointer, only; cf. gfc_omp_predetermined_sharing. */
  if (DECL_P (OMP_CLAUSE_DECL (clause))
      && GFC_DECL_ASSOCIATE_VAR_P (OMP_CLAUSE_DECL (clause)))
    return build2 (MODIFY_EXPR, TREE_TYPE (dest), dest, src);

  if (DECL_ARTIFICIAL (OMP_CLAUSE_DECL (clause))
      && DECL_LANG_SPECIFIC (OMP_CLAUSE_DECL (clause))
      && GFC_DECL_SAVED_DESCRIPTOR (OMP_CLAUSE_DECL (clause)))
    decl_type
      = TREE_TYPE (GFC_DECL_SAVED_DESCRIPTOR (OMP_CLAUSE_DECL (clause)));

  if (gfc_is_polymorphic_nonptr (decl_type))
    {
      if (POINTER_TYPE_P (decl_type))
	decl_type = TREE_TYPE (decl_type);
      decl_type = TREE_TYPE (TYPE_FIELDS (decl_type));
      if (GFC_DESCRIPTOR_TYPE_P (decl_type) || GFC_ARRAY_TYPE_P (decl_type))
	fatal_error (input_location,
		     "Sorry, polymorphic arrays not yet supported for "
		     "firstprivate");
      tree src_len;
      tree nelems = build_int_cst (size_type_node, 1);  /* Scalar.  */
      tree src_data = gfc_class_data_get (unshare_expr (src));
      tree dest_data = gfc_class_data_get (unshare_expr (dest));
      bool unlimited = gfc_is_unlimited_polymorphic_nonptr (type);

      gfc_start_block (&block);
      gfc_add_modify (&block, gfc_class_vptr_get (dest),
		      gfc_class_vptr_get (src));
      gfc_init_block (&cond_block);

      if (unlimited)
	{
	  src_len = gfc_class_len_get (src);
	  gfc_add_modify (&cond_block, gfc_class_len_get (unshare_expr (dest)), src_len);
	}

      /* Use: size = class._vtab._size * (class._len > 0 ? class._len : 1).  */
      size = fold_convert (size_type_node, gfc_class_vtab_size_get (src));
      if (unlimited)
	{
	  cond = fold_build2_loc (input_location, GT_EXPR, boolean_type_node,
				  unshare_expr (src_len),
				  build_zero_cst (TREE_TYPE (src_len)));
	  cond = build3_loc (input_location, COND_EXPR, size_type_node, cond,
			     fold_convert (size_type_node,
					   unshare_expr (src_len)),
			     build_int_cst (size_type_node, 1));
	  size = fold_build2_loc (input_location, MULT_EXPR, size_type_node,
				  size, cond);
	}

      /* Malloc memory + call class->_vpt->_copy.  */
      call = builtin_decl_explicit (BUILT_IN_MALLOC);
      call = build_call_expr_loc (input_location, call, 1, size);
      gfc_add_modify (&cond_block, dest_data,
		      fold_convert (TREE_TYPE (dest_data), call));
      gfc_add_expr_to_block (&cond_block,
			     gfc_copy_class_to_class (src, dest, nelems,
						      unlimited));

      gcc_assert (TREE_CODE (dest_data) == COMPONENT_REF);
      if (!GFC_DECL_GET_SCALAR_ALLOCATABLE (TREE_OPERAND (dest_data, 1)))
	{
	  gfc_add_block_to_block (&block, &cond_block);
	}
      else
	{
	  /* Create: if (class._data != 0) <cond_block> else class._data = NULL; */
	  cond = fold_build2_loc (input_location, NE_EXPR, boolean_type_node,
				  src_data, null_pointer_node);
	  gfc_add_expr_to_block (&block, build3_loc (input_location, COND_EXPR,
				 void_type_node, cond,
				 gfc_finish_block (&cond_block),
				 fold_build2_loc (input_location, MODIFY_EXPR, void_type_node,
				 unshare_expr (dest_data), null_pointer_node)));
	}
      return gfc_finish_block (&block);
    }

  if ((! GFC_DESCRIPTOR_TYPE_P (type)
       || GFC_TYPE_ARRAY_AKIND (type) != GFC_ARRAY_ALLOCATABLE)
      && (!GFC_DECL_GET_SCALAR_ALLOCATABLE (OMP_CLAUSE_DECL (clause))
	  || !POINTER_TYPE_P (type)))
    {
      if (gfc_has_alloc_comps (type, OMP_CLAUSE_DECL (clause)))
	{
	  gfc_start_block (&block);
	  gfc_add_modify (&block, dest, src);
	  tree tem = gfc_walk_alloc_comps (src, dest, OMP_CLAUSE_DECL (clause),
					   WALK_ALLOC_COMPS_COPY_CTOR);
	  gfc_add_expr_to_block (&block, tem);
	  return gfc_finish_block (&block);
	}
      else
	return build2_v (MODIFY_EXPR, dest, src);
    }

  /* Allocatable arrays in FIRSTPRIVATE clauses need to be allocated
     and copied from SRC.  */
  gfc_start_block (&block);

  gfc_init_block (&cond_block);

  gfc_add_modify (&cond_block, dest, fold_convert (TREE_TYPE (dest), src));
  if (GFC_DESCRIPTOR_TYPE_P (type))
    {
      tree rank = gfc_rank_cst[GFC_TYPE_ARRAY_RANK (type) - 1];
      size = gfc_conv_descriptor_ubound_get (dest, rank);
      size = fold_build2_loc (input_location, MINUS_EXPR, gfc_array_index_type,
			      size,
			      gfc_conv_descriptor_lbound_get (dest, rank));
      size = fold_build2_loc (input_location, PLUS_EXPR, gfc_array_index_type,
			      size, gfc_index_one_node);
      if (GFC_TYPE_ARRAY_RANK (type) > 1)
	size = fold_build2_loc (input_location, MULT_EXPR,
				gfc_array_index_type, size,
				gfc_conv_descriptor_stride_get (dest, rank));
      tree esize = fold_convert (gfc_array_index_type,
				 TYPE_SIZE_UNIT (gfc_get_element_type (type)));
      size = fold_build2_loc (input_location, MULT_EXPR, gfc_array_index_type,
			      size, esize);
      size = unshare_expr (size);
      size = gfc_evaluate_now (fold_convert (size_type_node, size),
			       &cond_block);
    }
  else
    size = fold_convert (size_type_node, TYPE_SIZE_UNIT (TREE_TYPE (type)));
  ptr = gfc_create_var (pvoid_type_node, NULL);
  gfc_allocate_using_malloc (&cond_block, ptr, size, NULL_TREE);
  if (GFC_DESCRIPTOR_TYPE_P (type))
    gfc_conv_descriptor_data_set (&cond_block, unshare_expr (dest), ptr);
  else
    gfc_add_modify (&cond_block, unshare_expr (dest),
		    fold_convert (TREE_TYPE (dest), ptr));

  tree srcptr = GFC_DESCRIPTOR_TYPE_P (type)
		? gfc_conv_descriptor_data_get (src) : src;
  srcptr = unshare_expr (srcptr);
  srcptr = fold_convert (pvoid_type_node, srcptr);
  call = build_call_expr_loc (input_location,
			      builtin_decl_explicit (BUILT_IN_MEMCPY), 3, ptr,
			      srcptr, size);
  gfc_add_expr_to_block (&cond_block, fold_convert (void_type_node, call));
  if (gfc_has_alloc_comps (type, OMP_CLAUSE_DECL (clause)))
    {
      tree tem = gfc_walk_alloc_comps (src, dest,
				       OMP_CLAUSE_DECL (clause),
				       WALK_ALLOC_COMPS_COPY_CTOR);
      gfc_add_expr_to_block (&cond_block, tem);
    }
  then_b = gfc_finish_block (&cond_block);

  gfc_init_block (&cond_block);
  if (GFC_DESCRIPTOR_TYPE_P (type))
    gfc_conv_descriptor_data_set (&cond_block, unshare_expr (dest),
				  null_pointer_node);
  else
    gfc_add_modify (&cond_block, unshare_expr (dest),
		    build_zero_cst (TREE_TYPE (dest)));
  else_b = gfc_finish_block (&cond_block);

  cond = fold_build2_loc (input_location, NE_EXPR, logical_type_node,
			  unshare_expr (srcptr), null_pointer_node);
  gfc_add_expr_to_block (&block,
			 build3_loc (input_location, COND_EXPR,
				     void_type_node, cond, then_b, else_b));
  /* Avoid -W*uninitialized warnings.  */
  if (DECL_P (dest))
    suppress_warning (dest, OPT_Wuninitialized);

  return gfc_finish_block (&block);
}

/* Similarly, except use an intrinsic or pointer assignment operator
   instead.  */

tree
gfc_omp_clause_assign_op (tree clause, tree dest, tree src)
{
  tree type = TREE_TYPE (dest), ptr, size, call, nonalloc;
  tree cond, then_b, else_b;
  stmtblock_t block, cond_block, cond_block2, inner_block;

  if ((! GFC_DESCRIPTOR_TYPE_P (type)
       || GFC_TYPE_ARRAY_AKIND (type) != GFC_ARRAY_ALLOCATABLE)
      && (!GFC_DECL_GET_SCALAR_ALLOCATABLE (OMP_CLAUSE_DECL (clause))
	  || !POINTER_TYPE_P (type)))
    {
      if (gfc_has_alloc_comps (type, OMP_CLAUSE_DECL (clause)))
	{
	  gfc_start_block (&block);
	  /* First dealloc any allocatable components in DEST.  */
	  tree tem = gfc_walk_alloc_comps (dest, NULL_TREE,
					   OMP_CLAUSE_DECL (clause),
					   WALK_ALLOC_COMPS_DTOR);
	  gfc_add_expr_to_block (&block, tem);
	  /* Then copy over toplevel data.  */
	  gfc_add_modify (&block, dest, src);
	  /* Finally allocate any allocatable components and copy.  */
	  tem = gfc_walk_alloc_comps (src, dest, OMP_CLAUSE_DECL (clause),
					   WALK_ALLOC_COMPS_COPY_CTOR);
	  gfc_add_expr_to_block (&block, tem);
	  return gfc_finish_block (&block);
	}
      else
	return build2_v (MODIFY_EXPR, dest, src);
    }

  gfc_start_block (&block);

  if (gfc_has_alloc_comps (type, OMP_CLAUSE_DECL (clause)))
    {
      then_b = gfc_walk_alloc_comps (dest, NULL_TREE, OMP_CLAUSE_DECL (clause),
				     WALK_ALLOC_COMPS_DTOR);
      tree tem = fold_convert (pvoid_type_node,
			       GFC_DESCRIPTOR_TYPE_P (type)
			       ? gfc_conv_descriptor_data_get (dest) : dest);
      tem = unshare_expr (tem);
      cond = fold_build2_loc (input_location, NE_EXPR, logical_type_node,
			      tem, null_pointer_node);
      tem = build3_loc (input_location, COND_EXPR, void_type_node, cond,
			then_b, build_empty_stmt (input_location));
      gfc_add_expr_to_block (&block, tem);
    }

  gfc_init_block (&cond_block);

  if (GFC_DESCRIPTOR_TYPE_P (type))
    {
      tree rank = gfc_rank_cst[GFC_TYPE_ARRAY_RANK (type) - 1];
      size = gfc_conv_descriptor_ubound_get (src, rank);
      size = fold_build2_loc (input_location, MINUS_EXPR, gfc_array_index_type,
			      size,
			      gfc_conv_descriptor_lbound_get (src, rank));
      size = fold_build2_loc (input_location, PLUS_EXPR, gfc_array_index_type,
			      size, gfc_index_one_node);
      if (GFC_TYPE_ARRAY_RANK (type) > 1)
	size = fold_build2_loc (input_location, MULT_EXPR,
				gfc_array_index_type, size,
				gfc_conv_descriptor_stride_get (src, rank));
      tree esize = fold_convert (gfc_array_index_type,
				 TYPE_SIZE_UNIT (gfc_get_element_type (type)));
      size = fold_build2_loc (input_location, MULT_EXPR, gfc_array_index_type,
			      size, esize);
      size = unshare_expr (size);
      size = gfc_evaluate_now (fold_convert (size_type_node, size),
			       &cond_block);
    }
  else
    size = fold_convert (size_type_node, TYPE_SIZE_UNIT (TREE_TYPE (type)));
  ptr = gfc_create_var (pvoid_type_node, NULL);

  tree destptr = GFC_DESCRIPTOR_TYPE_P (type)
		 ? gfc_conv_descriptor_data_get (dest) : dest;
  destptr = unshare_expr (destptr);
  destptr = fold_convert (pvoid_type_node, destptr);
  gfc_add_modify (&cond_block, ptr, destptr);

  nonalloc = fold_build2_loc (input_location, EQ_EXPR, logical_type_node,
			      destptr, null_pointer_node);
  cond = nonalloc;
  if (GFC_DESCRIPTOR_TYPE_P (type))
    {
      int i;
      for (i = 0; i < GFC_TYPE_ARRAY_RANK (type); i++)
	{
	  tree rank = gfc_rank_cst[i];
	  tree tem = gfc_conv_descriptor_ubound_get (src, rank);
	  tem = fold_build2_loc (input_location, MINUS_EXPR,
				 gfc_array_index_type, tem,
				 gfc_conv_descriptor_lbound_get (src, rank));
	  tem = fold_build2_loc (input_location, PLUS_EXPR,
				 gfc_array_index_type, tem,
				 gfc_conv_descriptor_lbound_get (dest, rank));
	  tem = fold_build2_loc (input_location, NE_EXPR, logical_type_node,
				 tem, gfc_conv_descriptor_ubound_get (dest,
								      rank));
	  cond = fold_build2_loc (input_location, TRUTH_ORIF_EXPR,
				  logical_type_node, cond, tem);
	}
    }

  gfc_init_block (&cond_block2);

  if (GFC_DESCRIPTOR_TYPE_P (type))
    {
      gfc_init_block (&inner_block);
      gfc_allocate_using_malloc (&inner_block, ptr, size, NULL_TREE);
      then_b = gfc_finish_block (&inner_block);

      gfc_init_block (&inner_block);
      gfc_add_modify (&inner_block, ptr,
		      gfc_call_realloc (&inner_block, ptr, size));
      else_b = gfc_finish_block (&inner_block);

      gfc_add_expr_to_block (&cond_block2,
			     build3_loc (input_location, COND_EXPR,
					 void_type_node,
					 unshare_expr (nonalloc),
					 then_b, else_b));
      gfc_add_modify (&cond_block2, dest, src);
      gfc_conv_descriptor_data_set (&cond_block2, unshare_expr (dest), ptr);
    }
  else
    {
      gfc_allocate_using_malloc (&cond_block2, ptr, size, NULL_TREE);
      gfc_add_modify (&cond_block2, unshare_expr (dest),
		      fold_convert (type, ptr));
    }
  then_b = gfc_finish_block (&cond_block2);
  else_b = build_empty_stmt (input_location);

  gfc_add_expr_to_block (&cond_block,
			 build3_loc (input_location, COND_EXPR,
				     void_type_node, unshare_expr (cond),
				     then_b, else_b));

  tree srcptr = GFC_DESCRIPTOR_TYPE_P (type)
		? gfc_conv_descriptor_data_get (src) : src;
  srcptr = unshare_expr (srcptr);
  srcptr = fold_convert (pvoid_type_node, srcptr);
  call = build_call_expr_loc (input_location,
			      builtin_decl_explicit (BUILT_IN_MEMCPY), 3, ptr,
			      srcptr, size);
  gfc_add_expr_to_block (&cond_block, fold_convert (void_type_node, call));
  if (gfc_has_alloc_comps (type, OMP_CLAUSE_DECL (clause)))
    {
      tree tem = gfc_walk_alloc_comps (src, dest,
				       OMP_CLAUSE_DECL (clause),
				       WALK_ALLOC_COMPS_COPY_CTOR);
      gfc_add_expr_to_block (&cond_block, tem);
    }
  then_b = gfc_finish_block (&cond_block);

  if (OMP_CLAUSE_CODE (clause) == OMP_CLAUSE_COPYIN)
    {
      gfc_init_block (&cond_block);
      if (GFC_DESCRIPTOR_TYPE_P (type))
	{
	  tree tmp = gfc_conv_descriptor_data_get (unshare_expr (dest));
	  tmp = gfc_deallocate_with_status (tmp, NULL_TREE, NULL_TREE,
					    NULL_TREE, NULL_TREE, true, NULL,
					    GFC_CAF_COARRAY_NOCOARRAY);
	  gfc_add_expr_to_block (&cond_block, tmp);
	}
      else
	{
	  destptr = gfc_evaluate_now (destptr, &cond_block);
	  gfc_add_expr_to_block (&cond_block, gfc_call_free (destptr));
	  gfc_add_modify (&cond_block, unshare_expr (dest),
			  build_zero_cst (TREE_TYPE (dest)));
	}
      else_b = gfc_finish_block (&cond_block);

      cond = fold_build2_loc (input_location, NE_EXPR, logical_type_node,
			      unshare_expr (srcptr), null_pointer_node);
      gfc_add_expr_to_block (&block,
			     build3_loc (input_location, COND_EXPR,
					 void_type_node, cond,
					 then_b, else_b));
    }
  else
    gfc_add_expr_to_block (&block, then_b);

  return gfc_finish_block (&block);
}

static void
gfc_omp_linear_clause_add_loop (stmtblock_t *block, tree dest, tree src,
				tree add, tree nelems)
{
  stmtblock_t tmpblock;
  tree desta, srca, index = gfc_create_var (gfc_array_index_type, "S");
  nelems = gfc_evaluate_now (nelems, block);

  gfc_init_block (&tmpblock);
  if (TREE_CODE (TREE_TYPE (dest)) == ARRAY_TYPE)
    {
      desta = gfc_build_array_ref (dest, index, NULL);
      srca = gfc_build_array_ref (src, index, NULL);
    }
  else
    {
      gcc_assert (POINTER_TYPE_P (TREE_TYPE (dest)));
      tree idx = fold_build2 (MULT_EXPR, sizetype,
			      fold_convert (sizetype, index),
			      TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (dest))));
      desta = build_fold_indirect_ref (fold_build2 (POINTER_PLUS_EXPR,
						    TREE_TYPE (dest), dest,
						    idx));
      srca = build_fold_indirect_ref (fold_build2 (POINTER_PLUS_EXPR,
						   TREE_TYPE (src), src,
						    idx));
    }
  gfc_add_modify (&tmpblock, desta,
		  fold_build2 (PLUS_EXPR, TREE_TYPE (desta),
			       srca, add));

  gfc_loopinfo loop;
  gfc_init_loopinfo (&loop);
  loop.dimen = 1;
  loop.from[0] = gfc_index_zero_node;
  loop.loopvar[0] = index;
  loop.to[0] = nelems;
  gfc_trans_scalarizing_loops (&loop, &tmpblock);
  gfc_add_block_to_block (block, &loop.pre);
}

/* Build and return code for a constructor of DEST that initializes
   it to SRC plus ADD (ADD is scalar integer).  */

tree
gfc_omp_clause_linear_ctor (tree clause, tree dest, tree src, tree add)
{
  tree type = TREE_TYPE (dest), ptr, size, nelems = NULL_TREE;
  stmtblock_t block;

  gcc_assert (OMP_CLAUSE_CODE (clause) == OMP_CLAUSE_LINEAR);

  gfc_start_block (&block);
  add = gfc_evaluate_now (add, &block);

  if ((! GFC_DESCRIPTOR_TYPE_P (type)
       || GFC_TYPE_ARRAY_AKIND (type) != GFC_ARRAY_ALLOCATABLE)
      && (!GFC_DECL_GET_SCALAR_ALLOCATABLE (OMP_CLAUSE_DECL (clause))
	  || !POINTER_TYPE_P (type)))
    {
      bool compute_nelts = false;
      gcc_assert (TREE_CODE (type) == ARRAY_TYPE);
      if (!TYPE_DOMAIN (type)
	  || TYPE_MAX_VALUE (TYPE_DOMAIN (type)) == NULL_TREE
	  || TYPE_MIN_VALUE (TYPE_DOMAIN (type)) == error_mark_node
	  || TYPE_MAX_VALUE (TYPE_DOMAIN (type)) == error_mark_node)
	compute_nelts = true;
      else if (VAR_P (TYPE_MAX_VALUE (TYPE_DOMAIN (type))))
	{
	  tree a = DECL_ATTRIBUTES (TYPE_MAX_VALUE (TYPE_DOMAIN (type)));
	  if (lookup_attribute ("omp dummy var", a))
	    compute_nelts = true;
	}
      if (compute_nelts)
	{
	  nelems = fold_build2 (EXACT_DIV_EXPR, sizetype,
				TYPE_SIZE_UNIT (type),
				TYPE_SIZE_UNIT (TREE_TYPE (type)));
	  nelems = size_binop (MINUS_EXPR, nelems, size_one_node);
	}
      else
	nelems = array_type_nelts_minus_one (type);
      nelems = fold_convert (gfc_array_index_type, nelems);

      gfc_omp_linear_clause_add_loop (&block, dest, src, add, nelems);
      return gfc_finish_block (&block);
    }

  /* Allocatable arrays in LINEAR clauses need to be allocated
     and copied from SRC.  */
  gfc_add_modify (&block, dest, src);
  if (GFC_DESCRIPTOR_TYPE_P (type))
    {
      tree rank = gfc_rank_cst[GFC_TYPE_ARRAY_RANK (type) - 1];
      size = gfc_conv_descriptor_ubound_get (dest, rank);
      size = fold_build2_loc (input_location, MINUS_EXPR, gfc_array_index_type,
			      size,
			      gfc_conv_descriptor_lbound_get (dest, rank));
      size = fold_build2_loc (input_location, PLUS_EXPR, gfc_array_index_type,
			      size, gfc_index_one_node);
      if (GFC_TYPE_ARRAY_RANK (type) > 1)
	size = fold_build2_loc (input_location, MULT_EXPR,
				gfc_array_index_type, size,
				gfc_conv_descriptor_stride_get (dest, rank));
      tree esize = fold_convert (gfc_array_index_type,
				 TYPE_SIZE_UNIT (gfc_get_element_type (type)));
      nelems = gfc_evaluate_now (unshare_expr (size), &block);
      size = fold_build2_loc (input_location, MULT_EXPR, gfc_array_index_type,
			      nelems, unshare_expr (esize));
      size = gfc_evaluate_now (fold_convert (size_type_node, size),
			       &block);
      nelems = fold_build2_loc (input_location, MINUS_EXPR,
				gfc_array_index_type, nelems,
				gfc_index_one_node);
    }
  else
    size = fold_convert (size_type_node, TYPE_SIZE_UNIT (TREE_TYPE (type)));
  ptr = gfc_create_var (pvoid_type_node, NULL);
  gfc_allocate_using_malloc (&block, ptr, size, NULL_TREE);
  if (GFC_DESCRIPTOR_TYPE_P (type))
    {
      gfc_conv_descriptor_data_set (&block, unshare_expr (dest), ptr);
      tree etype = gfc_get_element_type (type);
      ptr = fold_convert (build_pointer_type (etype), ptr);
      tree srcptr = gfc_conv_descriptor_data_get (unshare_expr (src));
      srcptr = fold_convert (build_pointer_type (etype), srcptr);
      gfc_omp_linear_clause_add_loop (&block, ptr, srcptr, add, nelems);
    }
  else
    {
      gfc_add_modify (&block, unshare_expr (dest),
		      fold_convert (TREE_TYPE (dest), ptr));
      ptr = fold_convert (TREE_TYPE (dest), ptr);
      tree dstm = build_fold_indirect_ref (ptr);
      tree srcm = build_fold_indirect_ref (unshare_expr (src));
      gfc_add_modify (&block, dstm,
		      fold_build2 (PLUS_EXPR, TREE_TYPE (add), srcm, add));
    }
  return gfc_finish_block (&block);
}

/* Build and return code destructing DECL.  Return NULL if nothing
   to be done.  */

tree
gfc_omp_clause_dtor (tree clause, tree decl)
{
  tree type = TREE_TYPE (decl), tem;
  tree decl_type = TREE_TYPE (OMP_CLAUSE_DECL (clause));

  /* Only pointer was privatized; cf. gfc_omp_clause_copy_ctor. */
  if (DECL_P (OMP_CLAUSE_DECL (clause))
      && GFC_DECL_ASSOCIATE_VAR_P (OMP_CLAUSE_DECL (clause)))
    return NULL_TREE;

  if (DECL_ARTIFICIAL (OMP_CLAUSE_DECL (clause))
      && DECL_LANG_SPECIFIC (OMP_CLAUSE_DECL (clause))
      && GFC_DECL_SAVED_DESCRIPTOR (OMP_CLAUSE_DECL (clause)))
    decl_type
	= TREE_TYPE (GFC_DECL_SAVED_DESCRIPTOR (OMP_CLAUSE_DECL (clause)));
  if (gfc_is_polymorphic_nonptr (decl_type))
    {
      if (POINTER_TYPE_P (decl_type))
	decl_type = TREE_TYPE (decl_type);
      decl_type = TREE_TYPE (TYPE_FIELDS (decl_type));
      if (GFC_DESCRIPTOR_TYPE_P (decl_type) || GFC_ARRAY_TYPE_P (decl_type))
	fatal_error (input_location,
		     "Sorry, polymorphic arrays not yet supported for "
		     "firstprivate");
      stmtblock_t block, cond_block;
      gfc_start_block (&block);
      gfc_init_block (&cond_block);
      tree final = gfc_class_vtab_final_get (decl);
      tree size = fold_convert (size_type_node, gfc_class_vtab_size_get (decl));
      gfc_se se;
      gfc_init_se (&se, NULL);
      symbol_attribute attr = {};
      tree data = gfc_class_data_get (decl);
      tree desc = gfc_conv_scalar_to_descriptor (&se, data, attr);

      /* Call class->_vpt->_finalize + free.  */
      tree call = build_fold_indirect_ref (final);
      call = build_call_expr_loc (input_location, call, 3,
				  gfc_build_addr_expr (NULL, desc),
				  size, boolean_false_node);
      gfc_add_block_to_block (&cond_block, &se.pre);
      gfc_add_expr_to_block (&cond_block, fold_convert (void_type_node, call));
      gfc_add_block_to_block (&cond_block, &se.post);
      /* Create: if (_vtab && _final) <cond_block>  */
      tree cond = fold_build2_loc (input_location, NE_EXPR, boolean_type_node,
				   gfc_class_vptr_get (decl),
				   null_pointer_node);
      tree cond2 = fold_build2_loc (input_location, NE_EXPR, boolean_type_node,
				   final, null_pointer_node);
      cond = fold_build2_loc (input_location, TRUTH_ANDIF_EXPR,
			      boolean_type_node, cond, cond2);
      gfc_add_expr_to_block (&block, build3_loc (input_location, COND_EXPR,
				 void_type_node, cond,
				 gfc_finish_block (&cond_block), NULL_TREE));
      call = builtin_decl_explicit (BUILT_IN_FREE);
      call = build_call_expr_loc (input_location, call, 1, data);
      gfc_add_expr_to_block (&block, fold_convert (void_type_node, call));
      return gfc_finish_block (&block);
    }

  if ((! GFC_DESCRIPTOR_TYPE_P (type)
       || GFC_TYPE_ARRAY_AKIND (type) != GFC_ARRAY_ALLOCATABLE)
      && (!GFC_DECL_GET_SCALAR_ALLOCATABLE (OMP_CLAUSE_DECL (clause))
	  || !POINTER_TYPE_P (type)))
    {
      if (gfc_has_alloc_comps (type, OMP_CLAUSE_DECL (clause)))
	return gfc_walk_alloc_comps (decl, NULL_TREE,
				     OMP_CLAUSE_DECL (clause),
				     WALK_ALLOC_COMPS_DTOR);
      return NULL_TREE;
    }

  if (GFC_DESCRIPTOR_TYPE_P (type))
    {
      /* Allocatable arrays in FIRSTPRIVATE/LASTPRIVATE etc. clauses need
	 to be deallocated if they were allocated.  */
      tem = gfc_conv_descriptor_data_get (decl);
      tem = gfc_deallocate_with_status (tem, NULL_TREE, NULL_TREE, NULL_TREE,
					NULL_TREE, true, NULL,
					GFC_CAF_COARRAY_NOCOARRAY);
    }
  else
    tem = gfc_call_free (decl);
  tem = gfc_omp_unshare_expr (tem);

  if (gfc_has_alloc_comps (type, OMP_CLAUSE_DECL (clause)))
    {
      stmtblock_t block;
      tree then_b;

      gfc_init_block (&block);
      gfc_add_expr_to_block (&block,
			     gfc_walk_alloc_comps (decl, NULL_TREE,
						   OMP_CLAUSE_DECL (clause),
						   WALK_ALLOC_COMPS_DTOR));
      gfc_add_expr_to_block (&block, tem);
      then_b = gfc_finish_block (&block);

      tem = fold_convert (pvoid_type_node,
			  GFC_DESCRIPTOR_TYPE_P (type)
			  ? gfc_conv_descriptor_data_get (decl) : decl);
      tem = unshare_expr (tem);
      tree cond = fold_build2_loc (input_location, NE_EXPR, logical_type_node,
				   tem, null_pointer_node);
      tem = build3_loc (input_location, COND_EXPR, void_type_node, cond,
			then_b, build_empty_stmt (input_location));
    }
  return tem;
}

/* Build a conditional expression in BLOCK.  If COND_VAL is not
   null, then the block THEN_B is executed, otherwise ELSE_VAL
   is assigned to VAL.  */

static void
gfc_build_cond_assign (stmtblock_t *block, tree val, tree cond_val,
		       tree then_b, tree else_val)
{
  stmtblock_t cond_block;
  tree else_b = NULL_TREE;
  tree val_ty = TREE_TYPE (val);

  if (else_val)
    {
      gfc_init_block (&cond_block);
      gfc_add_modify (&cond_block, val, fold_convert (val_ty, else_val));
      else_b = gfc_finish_block (&cond_block);
    }
  gfc_add_expr_to_block (block,
			 build3_loc (input_location, COND_EXPR, void_type_node,
				     cond_val, then_b, else_b));
}

/* Build a conditional expression in BLOCK, returning a temporary
   variable containing the result.  If COND_VAL is not null, then
   THEN_VAL will be assigned to the variable, otherwise ELSE_VAL
   is assigned.
 */

static tree
gfc_build_cond_assign_expr (stmtblock_t *block, tree cond_val,
			    tree then_val, tree else_val)
{
  tree val;
  tree val_ty = TREE_TYPE (then_val);
  stmtblock_t cond_block;

  val = create_tmp_var (val_ty);

  gfc_init_block (&cond_block);
  gfc_add_modify (&cond_block, val, then_val);
  tree then_b = gfc_finish_block (&cond_block);

  gfc_build_cond_assign (block, val, cond_val, then_b, else_val);

  return val;
}

void
gfc_omp_finish_clause (tree c, gimple_seq *pre_p, bool openacc)
{
  if (OMP_CLAUSE_CODE (c) != OMP_CLAUSE_MAP)
    return;

  tree decl = OMP_CLAUSE_DECL (c);

  /* Assumed-size arrays can't be mapped implicitly, they have to be
     mapped explicitly using array sections.  */
  if (TREE_CODE (decl) == PARM_DECL
      && GFC_ARRAY_TYPE_P (TREE_TYPE (decl))
      && GFC_TYPE_ARRAY_AKIND (TREE_TYPE (decl)) == GFC_ARRAY_UNKNOWN
      && GFC_TYPE_ARRAY_UBOUND (TREE_TYPE (decl),
				GFC_TYPE_ARRAY_RANK (TREE_TYPE (decl)) - 1)
	 == NULL)
    {
      error_at (OMP_CLAUSE_LOCATION (c),
		"implicit mapping of assumed size array %qD", decl);
      return;
    }

  if (!openacc && GFC_CLASS_TYPE_P (TREE_TYPE (decl)))
    warning_at (OMP_CLAUSE_LOCATION (c), OPT_Wopenmp,
		"Implicit mapping of polymorphic variable %qD is "
		"unspecified behavior", decl);

  tree c2 = NULL_TREE, c3 = NULL_TREE, c4 = NULL_TREE;
  tree present = gfc_omp_check_optional_argument (decl, true);
  if (POINTER_TYPE_P (TREE_TYPE (decl)))
    {
      if (!gfc_omp_privatize_by_reference (decl)
	  && !GFC_DECL_GET_SCALAR_POINTER (decl)
	  && !GFC_DECL_GET_SCALAR_ALLOCATABLE (decl)
	  && !GFC_DECL_CRAY_POINTEE (decl)
	  && !GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (TREE_TYPE (decl))))
	return;
      tree orig_decl = decl;

      c4 = build_omp_clause (OMP_CLAUSE_LOCATION (c), OMP_CLAUSE_MAP);
      OMP_CLAUSE_SET_MAP_KIND (c4, GOMP_MAP_POINTER);
      OMP_CLAUSE_DECL (c4) = decl;
      OMP_CLAUSE_SIZE (c4) = size_int (0);
      decl = build_fold_indirect_ref (decl);
      if (present
	  && (GFC_DECL_GET_SCALAR_POINTER (orig_decl)
	      || GFC_DECL_GET_SCALAR_ALLOCATABLE (orig_decl)))
	{
	  c2 = build_omp_clause (input_location, OMP_CLAUSE_MAP);
	  OMP_CLAUSE_SET_MAP_KIND (c2, GOMP_MAP_POINTER);
	  OMP_CLAUSE_DECL (c2) = decl;
	  OMP_CLAUSE_SIZE (c2) = size_int (0);

	  stmtblock_t block;
	  gfc_start_block (&block);
	  tree ptr = decl;
	  ptr = gfc_build_cond_assign_expr (&block, present, decl,
					    null_pointer_node);
	  gimplify_and_add (gfc_finish_block (&block), pre_p);
	  ptr = build_fold_indirect_ref (ptr);
	  OMP_CLAUSE_DECL (c) = ptr;
	  OMP_CLAUSE_SIZE (c) = TYPE_SIZE_UNIT (TREE_TYPE (ptr));
	}
      else
	{
	  OMP_CLAUSE_DECL (c) = decl;
	  OMP_CLAUSE_SIZE (c) = NULL_TREE;
	}
      if (TREE_CODE (TREE_TYPE (orig_decl)) == REFERENCE_TYPE
	  && (GFC_DECL_GET_SCALAR_POINTER (orig_decl)
	      || GFC_DECL_GET_SCALAR_ALLOCATABLE (orig_decl)))
	{
	  c3 = build_omp_clause (OMP_CLAUSE_LOCATION (c), OMP_CLAUSE_MAP);
	  OMP_CLAUSE_SET_MAP_KIND (c3, GOMP_MAP_POINTER);
	  OMP_CLAUSE_DECL (c3) = unshare_expr (decl);
	  OMP_CLAUSE_SIZE (c3) = size_int (0);
	  decl = build_fold_indirect_ref (decl);
	  OMP_CLAUSE_DECL (c) = decl;
	}
    }
  if (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (decl)))
    {
      stmtblock_t block;
      gfc_start_block (&block);
      tree type = TREE_TYPE (decl);
      tree ptr = gfc_conv_descriptor_data_get (decl);

      /* OpenMP: automatically map pointer targets with the pointer;
	 hence, always update the descriptor/pointer itself.
	 NOTE: This also remaps the pointer for allocatable arrays with
	 'target' attribute which also don't have the 'restrict' qualifier.  */
      bool always_modifier = false;

      if (!openacc
	  && !(TYPE_QUALS (TREE_TYPE (ptr)) & TYPE_QUAL_RESTRICT))
	always_modifier = true;

      if (present)
	ptr = gfc_build_cond_assign_expr (&block, present, ptr,
					  null_pointer_node);
      gcc_assert (POINTER_TYPE_P (TREE_TYPE (ptr)));
      ptr = build_fold_indirect_ref (ptr);
      OMP_CLAUSE_DECL (c) = ptr;
      c2 = build_omp_clause (input_location, OMP_CLAUSE_MAP);
      OMP_CLAUSE_SET_MAP_KIND (c2, GOMP_MAP_TO_PSET);
      if (present)
	{
	  ptr = create_tmp_var (TREE_TYPE (TREE_OPERAND (decl, 0)));
	  gfc_add_modify (&block, ptr, TREE_OPERAND (decl, 0));

	  OMP_CLAUSE_DECL (c2) = build_fold_indirect_ref (ptr);
	}
      else
	OMP_CLAUSE_DECL (c2) = decl;
      OMP_CLAUSE_SIZE (c2) = TYPE_SIZE_UNIT (type);
      c3 = build_omp_clause (OMP_CLAUSE_LOCATION (c), OMP_CLAUSE_MAP);
      OMP_CLAUSE_SET_MAP_KIND (c3, always_modifier ? GOMP_MAP_ALWAYS_POINTER
						   : GOMP_MAP_POINTER);
      if (present)
	{
	  ptr = gfc_conv_descriptor_data_get (decl);
	  ptr = gfc_build_addr_expr (NULL, ptr);
	  ptr = gfc_build_cond_assign_expr (&block, present,
					    ptr, null_pointer_node);
	  ptr = build_fold_indirect_ref (ptr);
	  OMP_CLAUSE_DECL (c3) = ptr;
	}
      else
	OMP_CLAUSE_DECL (c3) = gfc_conv_descriptor_data_get (decl);
      OMP_CLAUSE_SIZE (c3) = size_int (0);
      tree size = create_tmp_var (gfc_array_index_type);
      tree elemsz = TYPE_SIZE_UNIT (gfc_get_element_type (type));
      elemsz = fold_convert (gfc_array_index_type, elemsz);
      enum gfc_array_kind akind = GFC_TYPE_ARRAY_AKIND (type);
      if (akind == GFC_ARRAY_ALLOCATABLE
	  || akind == GFC_ARRAY_POINTER
	  || akind == GFC_ARRAY_POINTER_CONT
	  || akind == GFC_ARRAY_ASSUMED_RANK_ALLOCATABLE
	  || akind == GFC_ARRAY_ASSUMED_RANK_POINTER
	  || akind == GFC_ARRAY_ASSUMED_RANK_POINTER_CONT)
	{
	  stmtblock_t cond_block;
	  tree tem, then_b, else_b, zero, cond;

	  int rank = ((akind == GFC_ARRAY_ASSUMED_RANK_ALLOCATABLE
		       || akind == GFC_ARRAY_ASSUMED_RANK_POINTER
		       || akind == GFC_ARRAY_ASSUMED_RANK_POINTER_CONT)
		      ? -1 : GFC_TYPE_ARRAY_RANK (type));
	  gfc_init_block (&cond_block);
	  tem = gfc_full_array_size (&cond_block, unshare_expr (decl), rank);
	  gfc_add_modify (&cond_block, size, tem);
	  gfc_add_modify (&cond_block, size,
			  fold_build2 (MULT_EXPR, gfc_array_index_type,
				       size, elemsz));
	  then_b = gfc_finish_block (&cond_block);
	  gfc_init_block (&cond_block);
	  zero = build_int_cst (gfc_array_index_type, 0);
	  gfc_add_modify (&cond_block, size, zero);
	  else_b = gfc_finish_block (&cond_block);
	  tem = gfc_conv_descriptor_data_get (unshare_expr (decl));
	  tem = fold_convert (pvoid_type_node, tem);
	  cond = fold_build2_loc (input_location, NE_EXPR,
				  boolean_type_node, tem, null_pointer_node);
	  if (present)
	    {
	      cond = fold_build2_loc (input_location, TRUTH_ANDIF_EXPR,
				      boolean_type_node, present, cond);
	    }
	  gfc_add_expr_to_block (&block, build3_loc (input_location, COND_EXPR,
						     void_type_node, cond,
						     then_b, else_b));
	}
      else if (present)
	{
	  stmtblock_t cond_block;
	  tree then_b;

	  int rank = ((akind == GFC_ARRAY_ASSUMED_RANK
		       || akind == GFC_ARRAY_ASSUMED_RANK_CONT)
		      ? -1 : GFC_TYPE_ARRAY_RANK (type));
	  gfc_init_block (&cond_block);
	  gfc_add_modify (&cond_block, size,
			  gfc_full_array_size (&cond_block, unshare_expr (decl),
					       rank));
	  gfc_add_modify (&cond_block, size,
			  fold_build2 (MULT_EXPR, gfc_array_index_type,
				       size, elemsz));
	  then_b = gfc_finish_block (&cond_block);

	  gfc_build_cond_assign (&block, size, present, then_b,
				 build_int_cst (gfc_array_index_type, 0));
	}
      else
	{
	  int rank = ((akind == GFC_ARRAY_ASSUMED_RANK
		       || akind == GFC_ARRAY_ASSUMED_RANK_CONT)
		      ? -1 : GFC_TYPE_ARRAY_RANK (type));
	  gfc_add_modify (&block, size,
			  gfc_full_array_size (&block, unshare_expr (decl),
					       rank));
	  gfc_add_modify (&block, size,
			  fold_build2 (MULT_EXPR, gfc_array_index_type,
				       size, elemsz));
	}
      OMP_CLAUSE_SIZE (c) = size;
      tree stmt = gfc_finish_block (&block);
      gimplify_and_add (stmt, pre_p);
    }
  tree last = c;
  if (OMP_CLAUSE_SIZE (c) == NULL_TREE)
    OMP_CLAUSE_SIZE (c)
      = DECL_P (decl) ? DECL_SIZE_UNIT (decl)
		      : TYPE_SIZE_UNIT (TREE_TYPE (decl));
  if (gimplify_expr (&OMP_CLAUSE_SIZE (c), pre_p,
		     NULL, is_gimple_val, fb_rvalue) == GS_ERROR)
    OMP_CLAUSE_SIZE (c) = size_int (0);
  if (c2)
    {
      OMP_CLAUSE_CHAIN (c2) = OMP_CLAUSE_CHAIN (last);
      OMP_CLAUSE_CHAIN (last) = c2;
      last = c2;
    }
  if (c3)
    {
      OMP_CLAUSE_CHAIN (c3) = OMP_CLAUSE_CHAIN (last);
      OMP_CLAUSE_CHAIN (last) = c3;
      last = c3;
    }
  if (c4)
    {
      OMP_CLAUSE_CHAIN (c4) = OMP_CLAUSE_CHAIN (last);
      OMP_CLAUSE_CHAIN (last) = c4;
    }
}


/* Return true if DECL is a scalar variable (for the purpose of
   implicit firstprivatization/mapping). Only if 'ptr_alloc_ok.'
   is true, allocatables and pointers are permitted. */

bool
gfc_omp_scalar_p (tree decl, bool ptr_alloc_ok)
{
  tree type = TREE_TYPE (decl);
  if (TREE_CODE (type) == REFERENCE_TYPE)
    type = TREE_TYPE (type);
  if (TREE_CODE (type) == POINTER_TYPE)
    {
      if (GFC_DECL_GET_SCALAR_ALLOCATABLE (decl)
	  || GFC_DECL_GET_SCALAR_POINTER (decl))
	{
	  if (!ptr_alloc_ok)
	    return false;
	  type = TREE_TYPE (type);
	}
      if (GFC_ARRAY_TYPE_P (type)
	  || GFC_CLASS_TYPE_P (type))
	return false;
    }
  if ((TREE_CODE (type) == ARRAY_TYPE || TREE_CODE (type) == INTEGER_TYPE)
      && TYPE_STRING_FLAG (type))
    return false;
  if (INTEGRAL_TYPE_P (type)
      || SCALAR_FLOAT_TYPE_P (type)
      || COMPLEX_FLOAT_TYPE_P (type))
    return true;
  return false;
}


/* Return true if DECL is a scalar with target attribute but does not have the
   allocatable (or pointer) attribute (for the purpose of implicit mapping).  */

bool
gfc_omp_scalar_target_p (tree decl)
{
  return (DECL_P (decl) && GFC_DECL_GET_SCALAR_TARGET (decl)
	  && gfc_omp_scalar_p (decl, false));
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
	  && VAR_P (TREE_OPERAND (value, 0))
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
	  && VAR_P (TREE_OPERAND (value, 0))
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
gfc_trans_omp_variable (gfc_symbol *sym, bool declare_simd)
{
  if (declare_simd)
    {
      int cnt = 0;
      gfc_symbol *proc_sym;
      gfc_formal_arglist *f;

      gcc_assert (sym->attr.dummy);
      proc_sym = sym->ns->proc_name;
      if (proc_sym->attr.entry_master)
	++cnt;
      if (gfc_return_by_reference (proc_sym))
	{
	  ++cnt;
	  if (proc_sym->ts.type == BT_CHARACTER)
	    ++cnt;
	}
      for (f = gfc_sym_get_dummy_args (proc_sym); f; f = f->next)
	if (f->sym == sym)
	  break;
	else if (f->sym)
	  ++cnt;
      gcc_assert (f);
      return build_int_cst (integer_type_node, cnt);
    }

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
  parent_decl = current_function_decl
		? DECL_CONTEXT (current_function_decl) : NULL_TREE;

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
gfc_trans_omp_variable_list (enum omp_clause_code code,
			     gfc_omp_namelist *namelist, tree list,
			     bool declare_simd)
{
  for (; namelist != NULL; namelist = namelist->next)
    if (namelist->sym->attr.referenced || declare_simd)
      {
	tree t = gfc_trans_omp_variable (namelist->sym, declare_simd);
	if (t != error_mark_node)
	  {
	    tree node;
	    node = build_omp_clause (input_location, code);
	    OMP_CLAUSE_DECL (node) = t;
	    list = gfc_trans_add_clause (node, list);

	    if (code == OMP_CLAUSE_LASTPRIVATE
		&& namelist->u.lastprivate_conditional)
	      OMP_CLAUSE_LASTPRIVATE_CONDITIONAL (node) = 1;
	  }
      }
  return list;
}

struct omp_udr_find_orig_data
{
  gfc_omp_udr *omp_udr;
  bool omp_orig_seen;
};

static int
omp_udr_find_orig (gfc_expr **e, int *walk_subtrees ATTRIBUTE_UNUSED,
		   void *data)
{
  struct omp_udr_find_orig_data *cd = (struct omp_udr_find_orig_data *) data;
  if ((*e)->expr_type == EXPR_VARIABLE
      && (*e)->symtree->n.sym == cd->omp_udr->omp_orig)
    cd->omp_orig_seen = true;

  return 0;
}

static void
gfc_trans_omp_array_reduction_or_udr (tree c, gfc_omp_namelist *n, locus where)
{
  gfc_symbol *sym = n->sym;
  gfc_symtree *root1 = NULL, *root2 = NULL, *root3 = NULL, *root4 = NULL;
  gfc_symtree *symtree1, *symtree2, *symtree3, *symtree4 = NULL;
  gfc_symbol init_val_sym, outer_sym, intrinsic_sym;
  gfc_symbol omp_var_copy[4];
  gfc_expr *e1, *e2, *e3, *e4;
  gfc_ref *ref;
  tree decl, backend_decl, stmt, type, outer_decl;
  locus old_loc = gfc_current_locus;
  const char *iname;
  bool t;
  gfc_omp_udr *udr = n->u2.udr ? n->u2.udr->udr : NULL;

  decl = OMP_CLAUSE_DECL (c);
  gfc_current_locus = where;
  type = TREE_TYPE (decl);
  outer_decl = create_tmp_var_raw (type);
  if (TREE_CODE (decl) == PARM_DECL
      && TREE_CODE (type) == REFERENCE_TYPE
      && GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (type))
      && GFC_TYPE_ARRAY_AKIND (TREE_TYPE (type)) == GFC_ARRAY_ALLOCATABLE)
    {
      decl = build_fold_indirect_ref (decl);
      type = TREE_TYPE (type);
    }

  /* Create a fake symbol for init value.  */
  memset (&init_val_sym, 0, sizeof (init_val_sym));
  init_val_sym.ns = sym->ns;
  init_val_sym.name = sym->name;
  init_val_sym.ts = sym->ts;
  init_val_sym.attr.referenced = 1;
  init_val_sym.declared_at = where;
  init_val_sym.attr.flavor = FL_VARIABLE;
  if (OMP_CLAUSE_REDUCTION_CODE (c) != ERROR_MARK)
    backend_decl = omp_reduction_init (c, gfc_sym_type (&init_val_sym));
  else if (udr->initializer_ns)
    backend_decl = NULL;
  else
    switch (sym->ts.type)
      {
      case BT_LOGICAL:
      case BT_INTEGER:
      case BT_REAL:
      case BT_COMPLEX:
	backend_decl = build_zero_cst (gfc_sym_type (&init_val_sym));
	break;
      default:
	backend_decl = NULL_TREE;
	break;
      }
  init_val_sym.backend_decl = backend_decl;

  /* Create a fake symbol for the outer array reference.  */
  outer_sym = *sym;
  if (sym->as)
    outer_sym.as = gfc_copy_array_spec (sym->as);
  outer_sym.attr.dummy = 0;
  outer_sym.attr.result = 0;
  outer_sym.attr.flavor = FL_VARIABLE;
  outer_sym.backend_decl = outer_decl;
  if (decl != OMP_CLAUSE_DECL (c))
    outer_sym.backend_decl = build_fold_indirect_ref (outer_decl);

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

  memset (omp_var_copy, 0, sizeof omp_var_copy);
  if (udr)
    {
      omp_var_copy[0] = *udr->omp_out;
      omp_var_copy[1] = *udr->omp_in;
      *udr->omp_out = outer_sym;
      *udr->omp_in = *sym;
      if (udr->initializer_ns)
	{
	  omp_var_copy[2] = *udr->omp_priv;
	  omp_var_copy[3] = *udr->omp_orig;
	  *udr->omp_priv = *sym;
	  *udr->omp_orig = outer_sym;
	}
    }

  /* Create expressions.  */
  e1 = gfc_get_expr ();
  e1->expr_type = EXPR_VARIABLE;
  e1->where = where;
  e1->symtree = symtree1;
  e1->ts = sym->ts;
  if (sym->attr.dimension)
    {
      e1->ref = ref = gfc_get_ref ();
      ref->type = REF_ARRAY;
      ref->u.ar.where = where;
      ref->u.ar.as = sym->as;
      ref->u.ar.type = AR_FULL;
      ref->u.ar.dimen = 0;
    }
  t = gfc_resolve_expr (e1);
  gcc_assert (t);

  e2 = NULL;
  if (backend_decl != NULL_TREE)
    {
      e2 = gfc_get_expr ();
      e2->expr_type = EXPR_VARIABLE;
      e2->where = where;
      e2->symtree = symtree2;
      e2->ts = sym->ts;
      t = gfc_resolve_expr (e2);
      gcc_assert (t);
    }
  else if (udr->initializer_ns == NULL)
    {
      gcc_assert (sym->ts.type == BT_DERIVED);
      e2 = gfc_default_initializer (&sym->ts);
      gcc_assert (e2);
      t = gfc_resolve_expr (e2);
      gcc_assert (t);
    }
  else if (n->u2.udr->initializer->op == EXEC_ASSIGN)
    {
      e2 = gfc_copy_expr (n->u2.udr->initializer->expr2);
      t = gfc_resolve_expr (e2);
      gcc_assert (t);
    }
  if (udr && udr->initializer_ns)
    {
      struct omp_udr_find_orig_data cd;
      cd.omp_udr = udr;
      cd.omp_orig_seen = false;
      gfc_code_walker (&n->u2.udr->initializer,
		       gfc_dummy_code_callback, omp_udr_find_orig, &cd);
      if (cd.omp_orig_seen)
	OMP_CLAUSE_REDUCTION_OMP_ORIG_REF (c) = 1;
    }

  e3 = gfc_copy_expr (e1);
  e3->symtree = symtree3;
  t = gfc_resolve_expr (e3);
  gcc_assert (t);

  iname = NULL;
  e4 = NULL;
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
    case ERROR_MARK:
      if (n->u2.udr->combiner->op == EXEC_ASSIGN)
	{
	  gfc_free_expr (e3);
	  e3 = gfc_copy_expr (n->u2.udr->combiner->expr1);
	  e4 = gfc_copy_expr (n->u2.udr->combiner->expr2);
	  t = gfc_resolve_expr (e3);
	  gcc_assert (t);
	  t = gfc_resolve_expr (e4);
	  gcc_assert (t);
	}
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
      intrinsic_sym.attr.implicit_type = 1;
      intrinsic_sym.result = &intrinsic_sym;
      intrinsic_sym.declared_at = where;

      symtree4 = gfc_new_symtree (&root4, iname);
      symtree4->n.sym = &intrinsic_sym;
      gcc_assert (symtree4 == root4);

      e4 = gfc_get_expr ();
      e4->expr_type = EXPR_FUNCTION;
      e4->where = where;
      e4->symtree = symtree4;
      e4->value.function.actual = gfc_get_actual_arglist ();
      e4->value.function.actual->expr = e3;
      e4->value.function.actual->next = gfc_get_actual_arglist ();
      e4->value.function.actual->next->expr = e1;
    }
  if (OMP_CLAUSE_REDUCTION_CODE (c) != ERROR_MARK)
    {
      /* e1 and e3 have been stored as arguments of e4, avoid sharing.  */
      e1 = gfc_copy_expr (e1);
      e3 = gfc_copy_expr (e3);
      t = gfc_resolve_expr (e4);
      gcc_assert (t);
    }

  /* Create the init statement list.  */
  pushlevel ();
  if (e2)
    stmt = gfc_trans_assignment (e1, e2, false, false);
  else
    stmt = gfc_trans_call (n->u2.udr->initializer, false,
			   NULL_TREE, NULL_TREE, false);
  if (TREE_CODE (stmt) != BIND_EXPR)
    stmt = build3_v (BIND_EXPR, NULL, stmt, poplevel (1, 0));
  else
    poplevel (0, 0);
  OMP_CLAUSE_REDUCTION_INIT (c) = stmt;

  /* Create the merge statement list.  */
  pushlevel ();
  if (e4)
    stmt = gfc_trans_assignment (e3, e4, false, true);
  else
    stmt = gfc_trans_call (n->u2.udr->combiner, false,
			   NULL_TREE, NULL_TREE, false);
  if (TREE_CODE (stmt) != BIND_EXPR)
    stmt = build3_v (BIND_EXPR, NULL, stmt, poplevel (1, 0));
  else
    poplevel (0, 0);
  OMP_CLAUSE_REDUCTION_MERGE (c) = stmt;

  /* And stick the placeholder VAR_DECL into the clause as well.  */
  OMP_CLAUSE_REDUCTION_PLACEHOLDER (c) = outer_decl;

  gfc_current_locus = old_loc;

  gfc_free_expr (e1);
  if (e2)
    gfc_free_expr (e2);
  gfc_free_expr (e3);
  if (e4)
    gfc_free_expr (e4);
  free (symtree1);
  free (symtree2);
  free (symtree3);
  free (symtree4);
  if (outer_sym.as)
    gfc_free_array_spec (outer_sym.as);

  if (udr)
    {
      *udr->omp_out = omp_var_copy[0];
      *udr->omp_in = omp_var_copy[1];
      if (udr->initializer_ns)
	{
	  *udr->omp_priv = omp_var_copy[2];
	  *udr->omp_orig = omp_var_copy[3];
	}
    }
}

static tree
gfc_trans_omp_reduction_list (int kind, gfc_omp_namelist *namelist, tree list,
			      locus where, bool mark_addressable)
{
  omp_clause_code clause = OMP_CLAUSE_REDUCTION;
  switch (kind)
    {
    case OMP_LIST_REDUCTION:
    case OMP_LIST_REDUCTION_INSCAN:
    case OMP_LIST_REDUCTION_TASK:
      break;
    case OMP_LIST_IN_REDUCTION:
      clause = OMP_CLAUSE_IN_REDUCTION;
      break;
    case OMP_LIST_TASK_REDUCTION:
      clause = OMP_CLAUSE_TASK_REDUCTION;
      break;
    default:
      gcc_unreachable ();
    }
  for (; namelist != NULL; namelist = namelist->next)
    if (namelist->sym->attr.referenced)
      {
	tree t = gfc_trans_omp_variable (namelist->sym, false);
	if (t != error_mark_node)
	  {
	    tree node = build_omp_clause (gfc_get_location (&namelist->where),
					  clause);
	    OMP_CLAUSE_DECL (node) = t;
	    if (mark_addressable)
	      TREE_ADDRESSABLE (t) = 1;
	    if (kind == OMP_LIST_REDUCTION_INSCAN)
	      OMP_CLAUSE_REDUCTION_INSCAN (node) = 1;
	    if (kind == OMP_LIST_REDUCTION_TASK)
	      OMP_CLAUSE_REDUCTION_TASK (node) = 1;
	    switch (namelist->u.reduction_op)
	      {
	      case OMP_REDUCTION_PLUS:
		OMP_CLAUSE_REDUCTION_CODE (node) = PLUS_EXPR;
		break;
	      case OMP_REDUCTION_MINUS:
		OMP_CLAUSE_REDUCTION_CODE (node) = MINUS_EXPR;
		break;
	      case OMP_REDUCTION_TIMES:
		OMP_CLAUSE_REDUCTION_CODE (node) = MULT_EXPR;
		break;
	      case OMP_REDUCTION_AND:
		OMP_CLAUSE_REDUCTION_CODE (node) = TRUTH_ANDIF_EXPR;
		break;
	      case OMP_REDUCTION_OR:
		OMP_CLAUSE_REDUCTION_CODE (node) = TRUTH_ORIF_EXPR;
		break;
	      case OMP_REDUCTION_EQV:
		OMP_CLAUSE_REDUCTION_CODE (node) = EQ_EXPR;
		break;
	      case OMP_REDUCTION_NEQV:
		OMP_CLAUSE_REDUCTION_CODE (node) = NE_EXPR;
		break;
	      case OMP_REDUCTION_MAX:
		OMP_CLAUSE_REDUCTION_CODE (node) = MAX_EXPR;
		break;
	      case OMP_REDUCTION_MIN:
		OMP_CLAUSE_REDUCTION_CODE (node) = MIN_EXPR;
		break;
 	      case OMP_REDUCTION_IAND:
		OMP_CLAUSE_REDUCTION_CODE (node) = BIT_AND_EXPR;
		break;
 	      case OMP_REDUCTION_IOR:
		OMP_CLAUSE_REDUCTION_CODE (node) = BIT_IOR_EXPR;
		break;
 	      case OMP_REDUCTION_IEOR:
		OMP_CLAUSE_REDUCTION_CODE (node) = BIT_XOR_EXPR;
		break;
	      case OMP_REDUCTION_USER:
		OMP_CLAUSE_REDUCTION_CODE (node) = ERROR_MARK;
		break;
	      default:
		gcc_unreachable ();
	      }
	    if (namelist->sym->attr.dimension
		|| namelist->u.reduction_op == OMP_REDUCTION_USER
		|| namelist->sym->attr.allocatable)
	      gfc_trans_omp_array_reduction_or_udr (node, namelist, where);
	    list = gfc_trans_add_clause (node, list);
	  }
      }
  return list;
}

static inline tree
gfc_convert_expr_to_tree (stmtblock_t *block, gfc_expr *expr)
{
  gfc_se se;
  tree result;

  gfc_init_se (&se, NULL );
  gfc_conv_expr (&se, expr);
  gfc_add_block_to_block (block, &se.pre);
  result = gfc_evaluate_now (se.expr, block);
  gfc_add_block_to_block (block, &se.post);

  return result;
}

static vec<tree, va_heap, vl_embed> *doacross_steps;


/* Translate an array section or array element.  */

static void
gfc_trans_omp_array_section (stmtblock_t *block, gfc_exec_op op,
			     gfc_omp_namelist *n, tree decl, bool element,
			     bool openmp, gomp_map_kind ptr_kind, tree &node,
			     tree &node2, tree &node3, tree &node4)
{
  gfc_se se;
  tree ptr, ptr2;
  tree elemsz = NULL_TREE;

  gfc_init_se (&se, NULL);
  if (element)
    {
      gfc_conv_expr_reference (&se, n->expr);
      gfc_add_block_to_block (block, &se.pre);
      ptr = se.expr;
    }
  else
    {
      gfc_conv_expr_descriptor (&se, n->expr);
      ptr = gfc_conv_array_data (se.expr);
    }
  if (n->expr->ts.type == BT_CHARACTER && n->expr->ts.deferred)
    {
      gcc_assert (se.string_length);
      tree len = gfc_evaluate_now (se.string_length, block);
      elemsz = gfc_get_char_type (n->expr->ts.kind);
      elemsz = TYPE_SIZE_UNIT (elemsz);
      elemsz = fold_build2 (MULT_EXPR, size_type_node,
			    fold_convert (size_type_node, len), elemsz);
    }
  if (element)
    {
      if (!elemsz)
	elemsz = TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (ptr)));
      OMP_CLAUSE_SIZE (node) = elemsz;
    }
  else
    {
      tree type = TREE_TYPE (se.expr);
      gfc_add_block_to_block (block, &se.pre);
      OMP_CLAUSE_SIZE (node) = gfc_full_array_size (block, se.expr,
						    GFC_TYPE_ARRAY_RANK (type));
      if (!elemsz)
	elemsz = TYPE_SIZE_UNIT (gfc_get_element_type (type));
      elemsz = fold_convert (gfc_array_index_type, elemsz);
      OMP_CLAUSE_SIZE (node) = fold_build2 (MULT_EXPR, gfc_array_index_type,
					    OMP_CLAUSE_SIZE (node), elemsz);
    }
  gcc_assert (se.post.head == NULL_TREE);
  gcc_assert (POINTER_TYPE_P (TREE_TYPE (ptr)));
  OMP_CLAUSE_DECL (node) = build_fold_indirect_ref (ptr);
  ptr = fold_convert (ptrdiff_type_node, ptr);

  if (POINTER_TYPE_P (TREE_TYPE (decl))
      && GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (TREE_TYPE (decl)))
      && ptr_kind == GOMP_MAP_POINTER
      && op != EXEC_OMP_TARGET_EXIT_DATA
      && OMP_CLAUSE_MAP_KIND (node) != GOMP_MAP_RELEASE
      && OMP_CLAUSE_MAP_KIND (node) != GOMP_MAP_DELETE)

    {
      node4 = build_omp_clause (input_location,
				OMP_CLAUSE_MAP);
      OMP_CLAUSE_SET_MAP_KIND (node4, GOMP_MAP_POINTER);
      OMP_CLAUSE_DECL (node4) = decl;
      OMP_CLAUSE_SIZE (node4) = size_int (0);
      decl = build_fold_indirect_ref (decl);
    }
  else if (ptr_kind == GOMP_MAP_ALWAYS_POINTER
	   && n->expr->ts.type == BT_CHARACTER
	   && n->expr->ts.deferred)
    {
      gomp_map_kind map_kind;
      if (OMP_CLAUSE_MAP_KIND (node) == GOMP_MAP_DELETE)
	map_kind = OMP_CLAUSE_MAP_KIND (node);
      else if (op == EXEC_OMP_TARGET_EXIT_DATA
	       || OMP_CLAUSE_MAP_KIND (node) == GOMP_MAP_RELEASE)
	map_kind = GOMP_MAP_RELEASE;
      else
	map_kind = GOMP_MAP_TO;
      gcc_assert (se.string_length);
      node4 = build_omp_clause (input_location, OMP_CLAUSE_MAP);
      OMP_CLAUSE_SET_MAP_KIND (node4, map_kind);
      OMP_CLAUSE_DECL (node4) = se.string_length;
      OMP_CLAUSE_SIZE (node4) = TYPE_SIZE_UNIT (gfc_charlen_type_node);
    }
  if (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (decl)))
    {
      tree type = TREE_TYPE (decl);
      ptr2 = gfc_conv_descriptor_data_get (decl);
      node2 = build_omp_clause (input_location, OMP_CLAUSE_MAP);
      OMP_CLAUSE_DECL (node2) = decl;
      OMP_CLAUSE_SIZE (node2) = TYPE_SIZE_UNIT (type);
      if (OMP_CLAUSE_MAP_KIND (node) == GOMP_MAP_DELETE
	  || OMP_CLAUSE_MAP_KIND (node) == GOMP_MAP_RELEASE
	  || op == EXEC_OMP_TARGET_EXIT_DATA
	  || op == EXEC_OACC_EXIT_DATA)
	{
	  gomp_map_kind map_kind
	    = OMP_CLAUSE_MAP_KIND (node) == GOMP_MAP_DELETE ? GOMP_MAP_DELETE
							    : GOMP_MAP_RELEASE;
	  OMP_CLAUSE_SET_MAP_KIND (node2, map_kind);
	  OMP_CLAUSE_RELEASE_DESCRIPTOR (node2) = 1;
	}
      else
	OMP_CLAUSE_SET_MAP_KIND (node2, GOMP_MAP_TO_PSET);
      node3 = build_omp_clause (input_location, OMP_CLAUSE_MAP);
      OMP_CLAUSE_SET_MAP_KIND (node3, ptr_kind);
      OMP_CLAUSE_DECL (node3) = gfc_conv_descriptor_data_get (decl);
      /* This purposely does not include GOMP_MAP_ALWAYS_POINTER.  The extra
	 cast prevents gimplify.cc from recognising it as being part of the
	 struct - and adding an 'alloc: for the 'desc.data' pointer, which
	 would break as the 'desc' (the descriptor) is also mapped
	 (see node4 above).  */
      if (ptr_kind == GOMP_MAP_ATTACH_DETACH && !openmp)
	STRIP_NOPS (OMP_CLAUSE_DECL (node3));
    }
  else
    {
      if (TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE)
	{
	  tree offset;
	  ptr2 = build_fold_addr_expr (decl);
	  offset = fold_build2 (MINUS_EXPR, ptrdiff_type_node, ptr,
				fold_convert (ptrdiff_type_node, ptr2));
	  offset = build2 (TRUNC_DIV_EXPR, ptrdiff_type_node,
			   offset, fold_convert (ptrdiff_type_node, elemsz));
	  offset = build4_loc (input_location, ARRAY_REF,
			       TREE_TYPE (TREE_TYPE (decl)),
			       decl, offset, NULL_TREE, NULL_TREE);
	  OMP_CLAUSE_DECL (node) = offset;

	  if (ptr_kind == GOMP_MAP_ATTACH_DETACH && openmp)
	    return;
	}
      else
	{
	  gcc_assert (POINTER_TYPE_P (TREE_TYPE (decl)));
	  ptr2 = decl;
	}
      node3 = build_omp_clause (input_location,
				OMP_CLAUSE_MAP);
      OMP_CLAUSE_SET_MAP_KIND (node3, ptr_kind);
      OMP_CLAUSE_DECL (node3) = decl;
    }
  ptr2 = fold_convert (ptrdiff_type_node, ptr2);
  OMP_CLAUSE_SIZE (node3) = fold_build2 (MINUS_EXPR, ptrdiff_type_node,
					 ptr, ptr2);
}

static tree
handle_iterator (gfc_namespace *ns, stmtblock_t *iter_block, tree block)
{
  tree list = NULL_TREE;
  for (gfc_symbol *sym = ns->omp_affinity_iterators; sym; sym = sym->tlink)
    {
      gfc_constructor *c;
      gfc_se se;

      tree last = make_tree_vec (6);
      tree iter_var = gfc_get_symbol_decl (sym);
      tree type = TREE_TYPE (iter_var);
      TREE_VEC_ELT (last, 0) = iter_var;
      DECL_CHAIN (iter_var) = BLOCK_VARS (block);
      BLOCK_VARS (block) = iter_var;

      /* begin */
      c = gfc_constructor_first (sym->value->value.constructor);
      gfc_init_se (&se, NULL);
      gfc_conv_expr (&se, c->expr);
      gfc_add_block_to_block (iter_block, &se.pre);
      gfc_add_block_to_block (iter_block, &se.post);
      TREE_VEC_ELT (last, 1) = fold_convert (type,
					     gfc_evaluate_now (se.expr,
							       iter_block));
      /* end */
      c = gfc_constructor_next (c);
      gfc_init_se (&se, NULL);
      gfc_conv_expr (&se, c->expr);
      gfc_add_block_to_block (iter_block, &se.pre);
      gfc_add_block_to_block (iter_block, &se.post);
      TREE_VEC_ELT (last, 2) = fold_convert (type,
					     gfc_evaluate_now (se.expr,
							       iter_block));
      /* step */
      c = gfc_constructor_next (c);
      tree step;
      if (c)
	{
	  gfc_init_se (&se, NULL);
	  gfc_conv_expr (&se, c->expr);
	  gfc_add_block_to_block (iter_block, &se.pre);
	  gfc_add_block_to_block (iter_block, &se.post);
	  gfc_conv_expr (&se, c->expr);
	  step = fold_convert (type,
			       gfc_evaluate_now (se.expr,
						 iter_block));
	}
      else
	step = build_int_cst (type, 1);
      TREE_VEC_ELT (last, 3) = step;
      /* orig_step */
      TREE_VEC_ELT (last, 4) = save_expr (step);
      TREE_CHAIN (last) = list;
      list = last;
    }
  return list;
}

/* To alleviate quadratic behaviour in checking each entry of a
   gfc_omp_namelist against every other entry, we build a hashtable indexed by
   gfc_symbol pointer, which we can use in the usual case that a map
   expression has a symbol as its root term.  Return a namelist based on the
   root symbol used by N, building a new table in SYM_ROOTED_NL using the
   gfc_omp_namelist N2 (all clauses) if we haven't done so already.  */

static gfc_omp_namelist *
get_symbol_rooted_namelist (hash_map<gfc_symbol *,
				     gfc_omp_namelist *> *&sym_rooted_nl,
			    gfc_omp_namelist *n,
			    gfc_omp_namelist *n2, bool *sym_based)
{
  /* Early-out if we have a NULL clause list (e.g. for OpenACC).  */
  if (!n2)
    return NULL;

  gfc_symbol *use_sym = NULL;

  /* We're only interested in cases where we have an expression, e.g. a
     component access.  */
  if (n->expr && n->expr->expr_type == EXPR_VARIABLE && n->expr->symtree)
    use_sym = n->expr->symtree->n.sym;

  *sym_based = false;

  if (!use_sym)
    return n2;

  if (!sym_rooted_nl)
    {
      sym_rooted_nl = new hash_map<gfc_symbol *, gfc_omp_namelist *> ();

      for (; n2 != NULL; n2 = n2->next)
	{
	  if (!n2->expr
	      || n2->expr->expr_type != EXPR_VARIABLE
	      || !n2->expr->symtree)
	    continue;

	  gfc_omp_namelist *nl_copy = gfc_get_omp_namelist ();
	  memcpy (nl_copy, n2, sizeof *nl_copy);
	  nl_copy->u2.duplicate_of = n2;
	  nl_copy->next = NULL;

	  gfc_symbol *idx_sym = n2->expr->symtree->n.sym;

	  bool existed;
	  gfc_omp_namelist *&entry
	    = sym_rooted_nl->get_or_insert (idx_sym, &existed);
	  if (existed)
	    nl_copy->next = entry;
	  entry = nl_copy;
	}
    }

  gfc_omp_namelist **n2_sym = sym_rooted_nl->get (use_sym);

  if (n2_sym)
    {
      *sym_based = true;
      return *n2_sym;
    }

  return NULL;
}

static tree
gfc_trans_omp_clauses (stmtblock_t *block, gfc_omp_clauses *clauses,
		       locus where, bool declare_simd = false,
		       bool openacc = false, gfc_exec_op op = EXEC_NOP)
{
  tree omp_clauses = NULL_TREE, prev_clauses, chunk_size, c;
  tree iterator = NULL_TREE;
  tree tree_block = NULL_TREE;
  stmtblock_t iter_block;
  int list, ifc;
  enum omp_clause_code clause_code;
  gfc_omp_namelist *prev = NULL;
  gfc_se se;

  if (clauses == NULL)
    return NULL_TREE;

  hash_map<gfc_symbol *, gfc_omp_namelist *> *sym_rooted_nl = NULL;

  for (list = 0; list < OMP_LIST_NUM; list++)
    {
      gfc_omp_namelist *n = clauses->lists[list];

      if (n == NULL)
	continue;
      switch (list)
	{
	case OMP_LIST_REDUCTION:
	case OMP_LIST_REDUCTION_INSCAN:
	case OMP_LIST_REDUCTION_TASK:
	case OMP_LIST_IN_REDUCTION:
	case OMP_LIST_TASK_REDUCTION:
	  /* An OpenACC async clause indicates the need to set reduction
	     arguments addressable, to allow asynchronous copy-out.  */
	  omp_clauses = gfc_trans_omp_reduction_list (list, n, omp_clauses,
						      where, clauses->async);
	  break;
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
	  goto add_clause;
	case OMP_LIST_UNIFORM:
	  clause_code = OMP_CLAUSE_UNIFORM;
	  goto add_clause;
	case OMP_LIST_USE_DEVICE:
	case OMP_LIST_USE_DEVICE_PTR:
	  clause_code = OMP_CLAUSE_USE_DEVICE_PTR;
	  goto add_clause;
	case OMP_LIST_USE_DEVICE_ADDR:
	  clause_code = OMP_CLAUSE_USE_DEVICE_ADDR;
	  goto add_clause;
	case OMP_LIST_IS_DEVICE_PTR:
	  clause_code = OMP_CLAUSE_IS_DEVICE_PTR;
	  goto add_clause;
	case OMP_LIST_HAS_DEVICE_ADDR:
	  clause_code = OMP_CLAUSE_HAS_DEVICE_ADDR;
	  goto add_clause;
	case OMP_LIST_NONTEMPORAL:
	  clause_code = OMP_CLAUSE_NONTEMPORAL;
	  goto add_clause;
	case OMP_LIST_SCAN_IN:
	  clause_code = OMP_CLAUSE_INCLUSIVE;
	  goto add_clause;
	case OMP_LIST_SCAN_EX:
	  clause_code = OMP_CLAUSE_EXCLUSIVE;
	  goto add_clause;
	case OMP_LIST_USE:
	  clause_code = OMP_CLAUSE_USE;
	  goto add_clause;
	case OMP_LIST_DESTROY:
	  clause_code = OMP_CLAUSE_DESTROY;
	  goto add_clause;
	case OMP_LIST_INTEROP:
	  clause_code = OMP_CLAUSE_INTEROP;
	  goto add_clause;

	add_clause:
	  omp_clauses
	    = gfc_trans_omp_variable_list (clause_code, n, omp_clauses,
					   declare_simd);
	  break;

	case OMP_LIST_INIT:
	  {
	    tree pref_type = NULL_TREE;
	    const char *last = NULL;
	    for (; n != NULL; n = n->next)
	      if (n->sym->attr.referenced)
		{
		  tree t = gfc_trans_omp_variable (n->sym, false);
		  if (t == error_mark_node)
		    continue;
		  tree node = build_omp_clause (input_location,
						OMP_CLAUSE_INIT);
		  OMP_CLAUSE_DECL (node) = t;
		  if (n->u.init.target)
		    OMP_CLAUSE_INIT_TARGET (node) = 1;
		  if (n->u.init.targetsync)
		    OMP_CLAUSE_INIT_TARGETSYNC (node) = 1;
		  if (last != n->u2.init_interop)
		    {
		      last = n->u2.init_interop;
		      if (n->u2.init_interop == NULL)
			pref_type = NULL_TREE;
		      else
			{
			  pref_type = build_string (n->u.init.len,
						    n->u2.init_interop);
			  TREE_TYPE (pref_type)
			    = build_array_type_nelts (unsigned_char_type_node,
						      n->u.init.len);
			}
		    }
		  OMP_CLAUSE_INIT_PREFER_TYPE (node) = pref_type;
		  omp_clauses = gfc_trans_add_clause (node, omp_clauses);
		}
	    break;
	  }

	case OMP_LIST_ALIGNED:
	  for (; n != NULL; n = n->next)
	    if (n->sym->attr.referenced || declare_simd)
	      {
		tree t = gfc_trans_omp_variable (n->sym, declare_simd);
		if (t != error_mark_node)
		  {
		    tree node = build_omp_clause (input_location,
						  OMP_CLAUSE_ALIGNED);
		    OMP_CLAUSE_DECL (node) = t;
		    if (n->expr)
		      {
			tree alignment_var;

			if (declare_simd)
			  alignment_var = gfc_conv_constant_to_tree (n->expr);
			else
			  {
			    gfc_init_se (&se, NULL);
			    gfc_conv_expr (&se, n->expr);
			    gfc_add_block_to_block (block, &se.pre);
			    alignment_var = gfc_evaluate_now (se.expr, block);
			    gfc_add_block_to_block (block, &se.post);
			  }
			OMP_CLAUSE_ALIGNED_ALIGNMENT (node) = alignment_var;
		      }
		    omp_clauses = gfc_trans_add_clause (node, omp_clauses);
		  }
	      }
	  break;
	case OMP_LIST_ALLOCATE:
	  {
	    tree allocator_ = NULL_TREE;
	    gfc_expr *alloc_expr = NULL;
	    for (; n != NULL; n = n->next)
	      if (n->sym->attr.referenced)
		{
		  tree t = gfc_trans_omp_variable (n->sym, false);
		  if (t != error_mark_node)
		    {
		      tree node = build_omp_clause (input_location,
						    OMP_CLAUSE_ALLOCATE);
		      OMP_CLAUSE_DECL (node) = t;
		      if (n->u2.allocator)
			{
			  if (alloc_expr != n->u2.allocator)
			    {
			      gfc_init_se (&se, NULL);
			      gfc_conv_expr (&se, n->u2.allocator);
			      gfc_add_block_to_block (block, &se.pre);
			      allocator_ = gfc_evaluate_now (se.expr, block);
			      gfc_add_block_to_block (block, &se.post);
			    }
			  OMP_CLAUSE_ALLOCATE_ALLOCATOR (node) = allocator_;
			}
		      alloc_expr = n->u2.allocator;
		      if (n->u.align)
			{
			  tree align_;
			  gfc_init_se (&se, NULL);
			  gfc_conv_expr (&se, n->u.align);
			  gcc_assert (CONSTANT_CLASS_P (se.expr)
				      && se.pre.head == NULL
				      && se.post.head == NULL);
			  align_ = se.expr;
			  OMP_CLAUSE_ALLOCATE_ALIGN (node) = align_;
			}
		      omp_clauses = gfc_trans_add_clause (node, omp_clauses);
		    }
		}
	      else
		alloc_expr = n->u2.allocator;
	    }
	  break;
	case OMP_LIST_LINEAR:
	  {
	    gfc_expr *last_step_expr = NULL;
	    tree last_step = NULL_TREE;
	    bool last_step_parm = false;

	    for (; n != NULL; n = n->next)
	      {
		if (n->expr)
		  {
		    last_step_expr = n->expr;
		    last_step = NULL_TREE;
		    last_step_parm = false;
		  }
		if (n->sym->attr.referenced || declare_simd)
		  {
		    tree t = gfc_trans_omp_variable (n->sym, declare_simd);
		    if (t != error_mark_node)
		      {
			tree node = build_omp_clause (input_location,
						      OMP_CLAUSE_LINEAR);
			OMP_CLAUSE_DECL (node) = t;
			omp_clause_linear_kind kind;
			switch (n->u.linear.op)
			  {
			  case OMP_LINEAR_DEFAULT:
			    kind = OMP_CLAUSE_LINEAR_DEFAULT;
			    break;
			  case OMP_LINEAR_REF:
			    kind = OMP_CLAUSE_LINEAR_REF;
			    break;
			  case OMP_LINEAR_VAL:
			    kind = OMP_CLAUSE_LINEAR_VAL;
			    break;
			  case OMP_LINEAR_UVAL:
			    kind = OMP_CLAUSE_LINEAR_UVAL;
			    break;
			  default:
			    gcc_unreachable ();
			  }
			OMP_CLAUSE_LINEAR_KIND (node) = kind;
			OMP_CLAUSE_LINEAR_OLD_LINEAR_MODIFIER (node)
			  = n->u.linear.old_modifier;
			if (last_step_expr && last_step == NULL_TREE)
			  {
			    if (!declare_simd)
			      {
				gfc_init_se (&se, NULL);
				gfc_conv_expr (&se, last_step_expr);
				gfc_add_block_to_block (block, &se.pre);
				last_step = gfc_evaluate_now (se.expr, block);
				gfc_add_block_to_block (block, &se.post);
			      }
			    else if (last_step_expr->expr_type == EXPR_VARIABLE)
			      {
				gfc_symbol *s = last_step_expr->symtree->n.sym;
				last_step = gfc_trans_omp_variable (s, true);
				last_step_parm = true;
			      }
			    else
			      last_step
				= gfc_conv_constant_to_tree (last_step_expr);
			  }
			if (last_step_parm)
			  {
			    OMP_CLAUSE_LINEAR_VARIABLE_STRIDE (node) = 1;
			    OMP_CLAUSE_LINEAR_STEP (node) = last_step;
			  }
			else
			  {
			    if (kind == OMP_CLAUSE_LINEAR_REF)
			      {
				tree type;
				if (n->sym->attr.flavor == FL_PROCEDURE)
				  {
				    type = gfc_get_function_type (n->sym);
				    type = build_pointer_type (type);
				  }
				else
				  type = gfc_sym_type (n->sym);
				if (POINTER_TYPE_P (type))
				  type = TREE_TYPE (type);
				/* Otherwise to be determined what exactly
				   should be done.  */
				tree t = fold_convert (sizetype, last_step);
				t = size_binop (MULT_EXPR, t,
						TYPE_SIZE_UNIT (type));
				OMP_CLAUSE_LINEAR_STEP (node) = t;
			      }
			    else
			      {
				tree type
				  = gfc_typenode_for_spec (&n->sym->ts);
				OMP_CLAUSE_LINEAR_STEP (node)
				  = fold_convert (type, last_step);
			      }
			  }
			if (n->sym->attr.dimension || n->sym->attr.allocatable)
			  OMP_CLAUSE_LINEAR_ARRAY (node) = 1;
			omp_clauses = gfc_trans_add_clause (node, omp_clauses);
		      }
		  }
	      }
	  }
	  break;
	case OMP_LIST_AFFINITY:
	case OMP_LIST_DEPEND:
	  iterator = NULL_TREE;
	  prev = NULL;
	  prev_clauses = omp_clauses;
	  for (; n != NULL; n = n->next)
	    {
	      if (iterator && prev->u2.ns != n->u2.ns)
		{
		  BLOCK_SUBBLOCKS (tree_block) = gfc_finish_block (&iter_block);
		  TREE_VEC_ELT (iterator, 5) = tree_block;
		  for (tree c = omp_clauses; c != prev_clauses;
		       c = OMP_CLAUSE_CHAIN (c))
		    OMP_CLAUSE_DECL (c) = build_tree_list (iterator,
							   OMP_CLAUSE_DECL (c));
		  prev_clauses = omp_clauses;
		  iterator = NULL_TREE;
		}
	      if (n->u2.ns && (!prev || prev->u2.ns != n->u2.ns))
		{
		  gfc_init_block (&iter_block);
		  tree_block = make_node (BLOCK);
		  TREE_USED (tree_block) = 1;
		  BLOCK_VARS (tree_block) = NULL_TREE;
		  iterator = handle_iterator (n->u2.ns, block,
					      tree_block);
		}
	      if (!iterator)
		gfc_init_block (&iter_block);
	      prev = n;
	      if (list == OMP_LIST_DEPEND
		  && (n->u.depend_doacross_op == OMP_DOACROSS_SINK_FIRST
		      || n->u.depend_doacross_op == OMP_DEPEND_SINK_FIRST))
		{
		  tree vec = NULL_TREE;
		  unsigned int i;
		  bool is_depend
		    = n->u.depend_doacross_op == OMP_DEPEND_SINK_FIRST;
		  for (i = 0; ; i++)
		    {
		      tree addend = integer_zero_node, t;
		      bool neg = false;
		      if (n->sym && n->expr)
			{
			  addend = gfc_conv_constant_to_tree (n->expr);
			  if (TREE_CODE (addend) == INTEGER_CST
			      && tree_int_cst_sgn (addend) == -1)
			    {
			      neg = true;
			      addend = const_unop (NEGATE_EXPR,
						   TREE_TYPE (addend), addend);
			    }
			}

		      if (n->sym == NULL)
			t = null_pointer_node;  /* "omp_cur_iteration - 1".  */
		      else
			t = gfc_trans_omp_variable (n->sym, false);
		      if (t != error_mark_node)
			{
			  if (i < vec_safe_length (doacross_steps)
			      && !integer_zerop (addend)
			      && (*doacross_steps)[i])
			    {
			      tree step = (*doacross_steps)[i];
			      addend = fold_convert (TREE_TYPE (step), addend);
			      addend = build2 (TRUNC_DIV_EXPR,
					       TREE_TYPE (step), addend, step);
			    }
			  vec = tree_cons (addend, t, vec);
			  if (neg)
			    OMP_CLAUSE_DOACROSS_SINK_NEGATIVE (vec) = 1;
			}
		      if (n->next == NULL
			  || n->next->u.depend_doacross_op != OMP_DOACROSS_SINK)
			break;
		      n = n->next;
		    }
		  if (vec == NULL_TREE)
		    continue;

		  tree node = build_omp_clause (input_location,
						OMP_CLAUSE_DOACROSS);
		  OMP_CLAUSE_DOACROSS_KIND (node) = OMP_CLAUSE_DOACROSS_SINK;
		  OMP_CLAUSE_DOACROSS_DEPEND (node) = is_depend;
		  OMP_CLAUSE_DECL (node) = nreverse (vec);
		  omp_clauses = gfc_trans_add_clause (node, omp_clauses);
		  continue;
		}

	      if (n->sym && !n->sym->attr.referenced)
		continue;

	      tree node = build_omp_clause (input_location,
					    list == OMP_LIST_DEPEND
					    ? OMP_CLAUSE_DEPEND
					    : OMP_CLAUSE_AFFINITY);
	      if (n->sym == NULL)  /* omp_all_memory  */
		OMP_CLAUSE_DECL (node) = null_pointer_node;
	      else if (n->expr == NULL || n->expr->ref->u.ar.type == AR_FULL)
		{
		  tree decl = gfc_trans_omp_variable (n->sym, false);
		  if (gfc_omp_privatize_by_reference (decl))
		    decl = build_fold_indirect_ref (decl);
		  if (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (decl)))
		    {
		      decl = gfc_conv_descriptor_data_get (decl);
		      gcc_assert (POINTER_TYPE_P (TREE_TYPE (decl)));
		      decl = build_fold_indirect_ref (decl);
		    }
		  else if (n->sym->attr.allocatable || n->sym->attr.pointer)
		    decl = build_fold_indirect_ref (decl);
		  else if (DECL_P (decl))
		    TREE_ADDRESSABLE (decl) = 1;
		  OMP_CLAUSE_DECL (node) = decl;
		}
	      else
		{
		  tree ptr;
		  gfc_init_se (&se, NULL);
		  if (n->expr->ref->u.ar.type == AR_ELEMENT)
		    {
		      gfc_conv_expr_reference (&se, n->expr);
		      ptr = se.expr;
		    }
		  else
		    {
		      gfc_conv_expr_descriptor (&se, n->expr);
		      ptr = gfc_conv_array_data (se.expr);
		    }
		  gfc_add_block_to_block (&iter_block, &se.pre);
		  gfc_add_block_to_block (&iter_block, &se.post);
		  gcc_assert (POINTER_TYPE_P (TREE_TYPE (ptr)));
		  OMP_CLAUSE_DECL (node) = build_fold_indirect_ref (ptr);
		}
	      if (list == OMP_LIST_DEPEND)
		switch (n->u.depend_doacross_op)
		  {
		  case OMP_DEPEND_IN:
		    OMP_CLAUSE_DEPEND_KIND (node) = OMP_CLAUSE_DEPEND_IN;
		    break;
		  case OMP_DEPEND_OUT:
		    OMP_CLAUSE_DEPEND_KIND (node) = OMP_CLAUSE_DEPEND_OUT;
		    break;
		  case OMP_DEPEND_INOUT:
		    OMP_CLAUSE_DEPEND_KIND (node) = OMP_CLAUSE_DEPEND_INOUT;
		    break;
		  case OMP_DEPEND_INOUTSET:
		    OMP_CLAUSE_DEPEND_KIND (node) = OMP_CLAUSE_DEPEND_INOUTSET;
		    break;
		  case OMP_DEPEND_MUTEXINOUTSET:
		    OMP_CLAUSE_DEPEND_KIND (node)
		      = OMP_CLAUSE_DEPEND_MUTEXINOUTSET;
		    break;
		  case OMP_DEPEND_DEPOBJ:
		    OMP_CLAUSE_DEPEND_KIND (node) = OMP_CLAUSE_DEPEND_DEPOBJ;
		    break;
		  default:
		    gcc_unreachable ();
		  }
	      if (!iterator)
		gfc_add_block_to_block (block, &iter_block);
	      omp_clauses = gfc_trans_add_clause (node, omp_clauses);
	    }
	  if (iterator)
	    {
	      BLOCK_SUBBLOCKS (tree_block) = gfc_finish_block (&iter_block);
	      TREE_VEC_ELT (iterator, 5) = tree_block;
	      for (tree c = omp_clauses; c != prev_clauses;
		   c = OMP_CLAUSE_CHAIN (c))
		OMP_CLAUSE_DECL (c) = build_tree_list (iterator,
						       OMP_CLAUSE_DECL (c));
	    }
	  break;
	case OMP_LIST_MAP:
	  for (; n != NULL; n = n->next)
	    {
	      if (!n->sym->attr.referenced)
		continue;

	      bool always_modifier = false;
	      tree node = build_omp_clause (input_location, OMP_CLAUSE_MAP);
	      tree node2 = NULL_TREE;
	      tree node3 = NULL_TREE;
	      tree node4 = NULL_TREE;
	      tree node5 = NULL_TREE;

	      /* OpenMP: automatically map pointer targets with the pointer;
		 hence, always update the descriptor/pointer itself.  */
	      if (!openacc
		  && ((n->expr == NULL && n->sym->attr.pointer)
		      || (n->expr && gfc_expr_attr (n->expr).pointer)))
		always_modifier = true;

	      if (n->u.map.readonly)
		OMP_CLAUSE_MAP_READONLY (node) = 1;

	      switch (n->u.map.op)
		{
		case OMP_MAP_ALLOC:
		  OMP_CLAUSE_SET_MAP_KIND (node, GOMP_MAP_ALLOC);
		  break;
		case OMP_MAP_IF_PRESENT:
		  OMP_CLAUSE_SET_MAP_KIND (node, GOMP_MAP_IF_PRESENT);
		  break;
		case OMP_MAP_ATTACH:
		  OMP_CLAUSE_SET_MAP_KIND (node, GOMP_MAP_ATTACH);
		  break;
		case OMP_MAP_TO:
		  OMP_CLAUSE_SET_MAP_KIND (node, GOMP_MAP_TO);
		  break;
		case OMP_MAP_FROM:
		  OMP_CLAUSE_SET_MAP_KIND (node, GOMP_MAP_FROM);
		  break;
		case OMP_MAP_TOFROM:
		  OMP_CLAUSE_SET_MAP_KIND (node, GOMP_MAP_TOFROM);
		  break;
		case OMP_MAP_ALWAYS_TO:
		  always_modifier = true;
		  OMP_CLAUSE_SET_MAP_KIND (node, GOMP_MAP_ALWAYS_TO);
		  break;
		case OMP_MAP_ALWAYS_FROM:
		  always_modifier = true;
		  OMP_CLAUSE_SET_MAP_KIND (node, GOMP_MAP_ALWAYS_FROM);
		  break;
		case OMP_MAP_ALWAYS_TOFROM:
		  always_modifier = true;
		  OMP_CLAUSE_SET_MAP_KIND (node, GOMP_MAP_ALWAYS_TOFROM);
		  break;
		case OMP_MAP_PRESENT_ALLOC:
		  OMP_CLAUSE_SET_MAP_KIND (node, GOMP_MAP_PRESENT_ALLOC);
		  break;
		case OMP_MAP_PRESENT_TO:
		  OMP_CLAUSE_SET_MAP_KIND (node, GOMP_MAP_PRESENT_TO);
		  break;
		case OMP_MAP_PRESENT_FROM:
		  OMP_CLAUSE_SET_MAP_KIND (node, GOMP_MAP_PRESENT_FROM);
		  break;
		case OMP_MAP_PRESENT_TOFROM:
		  OMP_CLAUSE_SET_MAP_KIND (node, GOMP_MAP_PRESENT_TOFROM);
		  break;
		case OMP_MAP_ALWAYS_PRESENT_TO:
		  always_modifier = true;
		  OMP_CLAUSE_SET_MAP_KIND (node, GOMP_MAP_ALWAYS_PRESENT_TO);
		  break;
		case OMP_MAP_ALWAYS_PRESENT_FROM:
		  always_modifier = true;
		  OMP_CLAUSE_SET_MAP_KIND (node, GOMP_MAP_ALWAYS_PRESENT_FROM);
		  break;
		case OMP_MAP_ALWAYS_PRESENT_TOFROM:
		  always_modifier = true;
		  OMP_CLAUSE_SET_MAP_KIND (node, GOMP_MAP_ALWAYS_PRESENT_TOFROM);
		  break;
		case OMP_MAP_RELEASE:
		  OMP_CLAUSE_SET_MAP_KIND (node, GOMP_MAP_RELEASE);
		  break;
		case OMP_MAP_DELETE:
		  OMP_CLAUSE_SET_MAP_KIND (node, GOMP_MAP_DELETE);
		  break;
		case OMP_MAP_DETACH:
		  OMP_CLAUSE_SET_MAP_KIND (node, GOMP_MAP_DETACH);
		  break;
		case OMP_MAP_FORCE_ALLOC:
		  OMP_CLAUSE_SET_MAP_KIND (node, GOMP_MAP_FORCE_ALLOC);
		  break;
		case OMP_MAP_FORCE_TO:
		  OMP_CLAUSE_SET_MAP_KIND (node, GOMP_MAP_FORCE_TO);
		  break;
		case OMP_MAP_FORCE_FROM:
		  OMP_CLAUSE_SET_MAP_KIND (node, GOMP_MAP_FORCE_FROM);
		  break;
		case OMP_MAP_FORCE_TOFROM:
		  OMP_CLAUSE_SET_MAP_KIND (node, GOMP_MAP_FORCE_TOFROM);
		  break;
		case OMP_MAP_FORCE_PRESENT:
		  OMP_CLAUSE_SET_MAP_KIND (node, GOMP_MAP_FORCE_PRESENT);
		  break;
		case OMP_MAP_FORCE_DEVICEPTR:
		  OMP_CLAUSE_SET_MAP_KIND (node, GOMP_MAP_FORCE_DEVICEPTR);
		  break;
		default:
		  gcc_unreachable ();
		}

	      tree decl = gfc_trans_omp_variable (n->sym, false);
	      if (DECL_P (decl))
		TREE_ADDRESSABLE (decl) = 1;

	      gfc_ref *lastref = NULL;

	      if (n->expr)
		for (gfc_ref *ref = n->expr->ref; ref; ref = ref->next)
		  if (ref->type == REF_COMPONENT || ref->type == REF_ARRAY)
		    lastref = ref;

	      bool allocatable = false, pointer = false;

	      if (lastref && lastref->type == REF_COMPONENT)
		{
		  gfc_component *c = lastref->u.c.component;

		  if (c->ts.type == BT_CLASS)
		    {
		      pointer = CLASS_DATA (c)->attr.class_pointer;
		      allocatable = CLASS_DATA (c)->attr.allocatable;
		    }
		  else
		    {
		      pointer = c->attr.pointer;
		      allocatable = c->attr.allocatable;
		    }
		}

	      if (n->expr == NULL
		  || (n->expr->ref->type == REF_ARRAY
		      && n->expr->ref->u.ar.type == AR_FULL))
		{
		  gomp_map_kind map_kind;
		  tree type = TREE_TYPE (decl);
		  if (n->sym->ts.type == BT_CHARACTER
		      && n->sym->ts.deferred
		      && n->sym->attr.omp_declare_target
		      && (always_modifier || n->sym->attr.pointer)
		      && op != EXEC_OMP_TARGET_EXIT_DATA
		      && n->u.map.op != OMP_MAP_DELETE
		      && n->u.map.op != OMP_MAP_RELEASE)
		    {
		      gcc_assert (n->sym->ts.u.cl->backend_decl);
		      node5 = build_omp_clause (input_location, OMP_CLAUSE_MAP);
		      OMP_CLAUSE_SET_MAP_KIND (node5, GOMP_MAP_ALWAYS_TO);
		      OMP_CLAUSE_DECL (node5) = n->sym->ts.u.cl->backend_decl;
		      OMP_CLAUSE_SIZE (node5)
			= TYPE_SIZE_UNIT (gfc_charlen_type_node);
		    }

		  tree present = gfc_omp_check_optional_argument (decl, true);
		  if (openacc && n->sym->ts.type == BT_CLASS)
		    {
		      if (n->sym->attr.optional)
			sorry_at (gfc_get_location (&n->where),
				  "optional class parameter");
		      tree ptr = gfc_class_data_get (decl);
		      ptr = build_fold_indirect_ref (ptr);
		      OMP_CLAUSE_DECL (node) = ptr;
		      OMP_CLAUSE_SIZE (node) = gfc_class_vtab_size_get (decl);
		      node2 = build_omp_clause (input_location, OMP_CLAUSE_MAP);
		      OMP_CLAUSE_SET_MAP_KIND (node2, GOMP_MAP_ATTACH_DETACH);
		      OMP_CLAUSE_DECL (node2) = gfc_class_data_get (decl);
		      OMP_CLAUSE_SIZE (node2) = size_int (0);
		      goto finalize_map_clause;
		    }
		  else if (POINTER_TYPE_P (type)
			   && (gfc_omp_privatize_by_reference (decl)
			       || GFC_DECL_GET_SCALAR_POINTER (decl)
			       || GFC_DECL_GET_SCALAR_ALLOCATABLE (decl)
			       || GFC_DECL_CRAY_POINTEE (decl)
			       || GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (type))
			       || (n->sym->ts.type == BT_DERIVED
				   && (n->sym->ts.u.derived->ts.f90_type
				       != BT_VOID))))
		    {
		      tree orig_decl = decl;

		      /* For nonallocatable, nonpointer arrays, a temporary
			 variable is generated, but this one is only defined if
			 the variable is present; hence, we now set it to NULL
			 to avoid accessing undefined variables.  We cannot use
			 a temporary variable here as otherwise the replacement
			 of the variables in omp-low.cc will not work.  */
		      if (present && GFC_ARRAY_TYPE_P (type))
			{
			  tree tmp = fold_build2_loc (input_location,
						      MODIFY_EXPR,
						      void_type_node, decl,
						      null_pointer_node);
			  tree cond = fold_build1_loc (input_location,
						       TRUTH_NOT_EXPR,
						       boolean_type_node,
						       present);
			  gfc_add_expr_to_block (block,
						 build3_loc (input_location,
							     COND_EXPR,
							     void_type_node,
							     cond, tmp,
							     NULL_TREE));
			}
		      /* For descriptor types, the unmapping happens below.  */
		      if (op != EXEC_OMP_TARGET_EXIT_DATA
			  || !GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (decl)))
			{
			  enum gomp_map_kind gmk = GOMP_MAP_POINTER;
			  if (op == EXEC_OMP_TARGET_EXIT_DATA
			      && n->u.map.op == OMP_MAP_DELETE)
			    gmk = GOMP_MAP_DELETE;
			  else if (op == EXEC_OMP_TARGET_EXIT_DATA)
			    gmk = GOMP_MAP_RELEASE;
			  tree size;
			  if (gmk == GOMP_MAP_RELEASE || gmk == GOMP_MAP_DELETE)
			    size = TYPE_SIZE_UNIT (TREE_TYPE (decl));
			  else
			    size = size_int (0);
			  node4 = build_omp_clause (input_location,
						    OMP_CLAUSE_MAP);
			  OMP_CLAUSE_SET_MAP_KIND (node4, gmk);
			  OMP_CLAUSE_DECL (node4) = decl;
			  OMP_CLAUSE_SIZE (node4) = size;
			}
		      decl = build_fold_indirect_ref (decl);
		      if ((TREE_CODE (TREE_TYPE (orig_decl)) == REFERENCE_TYPE
			   || gfc_omp_is_optional_argument (orig_decl))
			  && (GFC_DECL_GET_SCALAR_POINTER (orig_decl)
			      || GFC_DECL_GET_SCALAR_ALLOCATABLE (orig_decl)))
			{
			  enum gomp_map_kind gmk;
			  if (op == EXEC_OMP_TARGET_EXIT_DATA
			      && n->u.map.op == OMP_MAP_DELETE)
			    gmk = GOMP_MAP_DELETE;
			  else if (op == EXEC_OMP_TARGET_EXIT_DATA)
			    gmk = GOMP_MAP_RELEASE;
			  else
			    gmk = GOMP_MAP_POINTER;
			  tree size;
			  if (gmk == GOMP_MAP_RELEASE || gmk == GOMP_MAP_DELETE)
			    size = TYPE_SIZE_UNIT (TREE_TYPE (decl));
			  else
			    size = size_int (0);
			  node3 = build_omp_clause (input_location,
						    OMP_CLAUSE_MAP);
			  OMP_CLAUSE_SET_MAP_KIND (node3, gmk);
			  OMP_CLAUSE_DECL (node3) = decl;
			  OMP_CLAUSE_SIZE (node3) = size;
			  decl = build_fold_indirect_ref (decl);
			}
		    }
		  if (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (decl)))
		    {
		      tree type = TREE_TYPE (decl);
		      tree ptr = gfc_conv_descriptor_data_get (decl);
		      if (present)
			ptr = gfc_build_cond_assign_expr (block, present, ptr,
							  null_pointer_node);
		      gcc_assert (POINTER_TYPE_P (TREE_TYPE (ptr)));
		      ptr = build_fold_indirect_ref (ptr);
		      OMP_CLAUSE_DECL (node) = ptr;
		      node2 = build_omp_clause (input_location, OMP_CLAUSE_MAP);
		      OMP_CLAUSE_DECL (node2) = decl;
		      OMP_CLAUSE_SIZE (node2) = TYPE_SIZE_UNIT (type);
		      if (n->u.map.op == OMP_MAP_DELETE)
			map_kind = GOMP_MAP_DELETE;
		      else if (op == EXEC_OMP_TARGET_EXIT_DATA
			       || n->u.map.op == OMP_MAP_RELEASE)
			map_kind = GOMP_MAP_RELEASE;
		      else
			map_kind = GOMP_MAP_TO_PSET;
		      OMP_CLAUSE_SET_MAP_KIND (node2, map_kind);

		      if (op != EXEC_OMP_TARGET_EXIT_DATA
			  && n->u.map.op != OMP_MAP_DELETE
			  && n->u.map.op != OMP_MAP_RELEASE)
			{
			  node3 = build_omp_clause (input_location,
						    OMP_CLAUSE_MAP);
			  if (present)
			    {
			      ptr = gfc_conv_descriptor_data_get (decl);
			      ptr = gfc_build_addr_expr (NULL, ptr);
			      ptr = gfc_build_cond_assign_expr (
				      block, present, ptr, null_pointer_node);
			      ptr = build_fold_indirect_ref (ptr);
			      OMP_CLAUSE_DECL (node3) = ptr;
			    }
			  else
			    OMP_CLAUSE_DECL (node3)
			      = gfc_conv_descriptor_data_get (decl);
			  OMP_CLAUSE_SIZE (node3) = size_int (0);

			  if (n->u.map.op == OMP_MAP_ATTACH)
			    {
			      /* Standalone attach clauses used with arrays with
				 descriptors must copy the descriptor to the
				 target, else they won't have anything to
				 perform the attachment onto (see OpenACC 2.6,
				 "2.6.3. Data Structures with Pointers").  */
			      OMP_CLAUSE_SET_MAP_KIND (node3, GOMP_MAP_ATTACH);
			      /* We don't want to map PTR at all in this case,
				 so delete its node and shuffle the others
				 down.  */
			      node = node2;
			      node2 = node3;
			      node3 = NULL;
			      goto finalize_map_clause;
			    }
			  else if (n->u.map.op == OMP_MAP_DETACH)
			    {
			      OMP_CLAUSE_SET_MAP_KIND (node3, GOMP_MAP_DETACH);
			      /* Similarly to above, we don't want to unmap PTR
				 here.  */
			      node = node2;
			      node2 = node3;
			      node3 = NULL;
			      goto finalize_map_clause;
			    }
			  else
			    OMP_CLAUSE_SET_MAP_KIND (node3,
						     always_modifier
						     ? GOMP_MAP_ALWAYS_POINTER
						     : GOMP_MAP_POINTER);
			}

		      /* We have to check for n->sym->attr.dimension because
			 of scalar coarrays.  */
		      if ((n->sym->attr.pointer || n->sym->attr.allocatable)
			  && n->sym->attr.dimension)
			{
			  stmtblock_t cond_block;
			  tree size
			    = gfc_create_var (gfc_array_index_type, NULL);
			  tree tem, then_b, else_b, zero, cond;

			  gfc_init_block (&cond_block);
			  tem
			    = gfc_full_array_size (&cond_block, decl,
						   GFC_TYPE_ARRAY_RANK (type));
			  tree elemsz;
			  if (n->sym->ts.type == BT_CHARACTER
			      && n->sym->ts.deferred)
			    {
			      tree len = n->sym->ts.u.cl->backend_decl;
			      len = fold_convert (size_type_node, len);
			      elemsz = gfc_get_char_type (n->sym->ts.kind);
			      elemsz = TYPE_SIZE_UNIT (elemsz);
			      elemsz = fold_build2 (MULT_EXPR, size_type_node,
						    len, elemsz);
			    }
			  else
			    elemsz
			      = TYPE_SIZE_UNIT (gfc_get_element_type (type));
			  elemsz = fold_convert (gfc_array_index_type, elemsz);
			  tem = fold_build2 (MULT_EXPR, gfc_array_index_type,
					     tem, elemsz);
			  gfc_add_modify (&cond_block, size, tem);
			  then_b = gfc_finish_block (&cond_block);
			  gfc_init_block (&cond_block);
			  zero = build_int_cst (gfc_array_index_type, 0);
			  gfc_add_modify (&cond_block, size, zero);
			  else_b = gfc_finish_block (&cond_block);
			  tem = gfc_conv_descriptor_data_get (decl);
			  tem = fold_convert (pvoid_type_node, tem);
			  cond = fold_build2_loc (input_location, NE_EXPR,
						  boolean_type_node,
						  tem, null_pointer_node);
			  if (present)
			    cond = fold_build2_loc (input_location,
						    TRUTH_ANDIF_EXPR,
						    boolean_type_node,
						    present, cond);
			  gfc_add_expr_to_block (block,
						 build3_loc (input_location,
							     COND_EXPR,
							     void_type_node,
							     cond, then_b,
							     else_b));
			  OMP_CLAUSE_SIZE (node) = size;
			}
		      else if (n->sym->attr.dimension)
			{
			  stmtblock_t cond_block;
			  gfc_init_block (&cond_block);
			  tree size = gfc_full_array_size (&cond_block, decl,
					GFC_TYPE_ARRAY_RANK (type));
			  tree elemsz
			    = TYPE_SIZE_UNIT (gfc_get_element_type (type));
			  elemsz = fold_convert (gfc_array_index_type, elemsz);
			  size = fold_build2 (MULT_EXPR, gfc_array_index_type,
					      size, elemsz);
			  size = gfc_evaluate_now (size, &cond_block);
			  if (present)
			    {
			      tree var = gfc_create_var (gfc_array_index_type,
							 NULL);
			      gfc_add_modify (&cond_block, var, size);
			      tree cond_body = gfc_finish_block (&cond_block);
			      tree cond = build3_loc (input_location, COND_EXPR,
						      void_type_node, present,
						      cond_body, NULL_TREE);
			      gfc_add_expr_to_block (block, cond);
			      OMP_CLAUSE_SIZE (node) = var;
			    }
			  else
			    {
			      gfc_add_block_to_block (block, &cond_block);
			      OMP_CLAUSE_SIZE (node) = size;
			    }
			}
		    }
		  else if (present
			   && INDIRECT_REF_P (decl)
			   && INDIRECT_REF_P (TREE_OPERAND (decl, 0)))
		    {
		      /* A single indirectref is handled by the middle end.  */
		      gcc_assert (!POINTER_TYPE_P (TREE_TYPE (decl)));
		      decl = TREE_OPERAND (decl, 0);
		      decl = gfc_build_cond_assign_expr (block, present, decl,
							 null_pointer_node);
		      OMP_CLAUSE_DECL (node) = build_fold_indirect_ref (decl);
		    }
		  else
		    OMP_CLAUSE_DECL (node) = decl;

		  if (!n->sym->attr.dimension
		      && n->sym->ts.type == BT_CHARACTER
		      && n->sym->ts.deferred)
		    {
		      if (!DECL_P (decl))
			{
			  gcc_assert (TREE_CODE (decl) == INDIRECT_REF);
			  decl = TREE_OPERAND (decl, 0);
			}
		      tree cond = fold_build2_loc (input_location, NE_EXPR,
						   boolean_type_node,
						   decl, null_pointer_node);
		      if (present)
			cond = fold_build2_loc (input_location,
						TRUTH_ANDIF_EXPR,
						boolean_type_node,
						present, cond);
		      tree len = n->sym->ts.u.cl->backend_decl;
		      len = fold_convert (size_type_node, len);
		      tree size = gfc_get_char_type (n->sym->ts.kind);
		      size = TYPE_SIZE_UNIT (size);
		      size = fold_build2 (MULT_EXPR, size_type_node, len, size);
		      size = build3_loc (input_location,
							 COND_EXPR,
							 size_type_node,
							 cond, size,
							 size_zero_node);
		      size = gfc_evaluate_now (size, block);
		      OMP_CLAUSE_SIZE (node) = size;
		    }
		}
	      else if (n->expr
		       && n->expr->expr_type == EXPR_VARIABLE
		       && n->expr->ref->type == REF_ARRAY
		       && !n->expr->ref->next)
		{
		  /* An array element or array section which is not part of a
		     derived type, etc.  */
		  bool element = n->expr->ref->u.ar.type == AR_ELEMENT;
		  tree type = TREE_TYPE (decl);
		  gomp_map_kind k = GOMP_MAP_POINTER;
		  if (!openacc
		      && !GFC_DESCRIPTOR_TYPE_P (type)
		      && !(POINTER_TYPE_P (type)
			   && GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (type))))
		    k = GOMP_MAP_FIRSTPRIVATE_POINTER;
		  gfc_trans_omp_array_section (block, op, n, decl, element,
					       !openacc, k, node, node2,
					       node3, node4);
		}
	      else if (n->expr
		       && n->expr->expr_type == EXPR_VARIABLE
		       && (n->expr->ref->type == REF_COMPONENT
			   || n->expr->ref->type == REF_ARRAY)
		       && lastref
		       && lastref->type == REF_COMPONENT
		       && lastref->u.c.component->ts.type != BT_CLASS
		       && lastref->u.c.component->ts.type != BT_DERIVED
		       && !lastref->u.c.component->attr.dimension)
		{
		  /* Derived type access with last component being a scalar.  */
		  gfc_init_se (&se, NULL);

		  gfc_conv_expr (&se, n->expr);
		  gfc_add_block_to_block (block, &se.pre);
		  /* For BT_CHARACTER a pointer is returned.  */
		  OMP_CLAUSE_DECL (node)
		    = POINTER_TYPE_P (TREE_TYPE (se.expr))
		      ? build_fold_indirect_ref (se.expr) : se.expr;
		  gfc_add_block_to_block (block, &se.post);
		  if (pointer || allocatable)
		    {
		      /* If it's a bare attach/detach clause, we just want
			 to perform a single attach/detach operation, of the
			 pointer itself, not of the pointed-to object.  */
		      if (openacc
			  && (n->u.map.op == OMP_MAP_ATTACH
			      || n->u.map.op == OMP_MAP_DETACH))
			{
			  OMP_CLAUSE_DECL (node)
			    = build_fold_addr_expr (OMP_CLAUSE_DECL (node));
			  OMP_CLAUSE_SIZE (node) = size_zero_node;
			  goto finalize_map_clause;
			}

		      node2 = build_omp_clause (input_location,
						OMP_CLAUSE_MAP);
		      OMP_CLAUSE_SET_MAP_KIND (node2, GOMP_MAP_ATTACH_DETACH);
		      OMP_CLAUSE_DECL (node2)
			= POINTER_TYPE_P (TREE_TYPE (se.expr))
			  ? se.expr
			  : gfc_build_addr_expr (NULL, se.expr);
		      OMP_CLAUSE_SIZE (node2) = size_int (0);
		      if (!openacc
			  && n->expr->ts.type == BT_CHARACTER
			  && n->expr->ts.deferred)
			{
			  gcc_assert (se.string_length);
			  tree tmp
			    = gfc_get_char_type (n->expr->ts.kind);
			  OMP_CLAUSE_SIZE (node)
			    = fold_build2 (MULT_EXPR, size_type_node,
					   fold_convert (size_type_node,
					       se.string_length),
					   TYPE_SIZE_UNIT (tmp));
			  gomp_map_kind kind;
			  if (n->u.map.op == OMP_MAP_DELETE)
			    kind = GOMP_MAP_DELETE;
			  else if (op == EXEC_OMP_TARGET_EXIT_DATA)
			    kind = GOMP_MAP_RELEASE;
			  else
			    kind = GOMP_MAP_TO;
			  node3 = build_omp_clause (input_location,
						    OMP_CLAUSE_MAP);
			  OMP_CLAUSE_SET_MAP_KIND (node3, kind);
			  OMP_CLAUSE_DECL (node3) = se.string_length;
			  OMP_CLAUSE_SIZE (node3)
			    = TYPE_SIZE_UNIT (gfc_charlen_type_node);
			}
		    }
		}
	      else if (n->expr
		       && n->expr->expr_type == EXPR_VARIABLE
		       && (n->expr->ref->type == REF_COMPONENT
			   || n->expr->ref->type == REF_ARRAY))
		{
		  gfc_init_se (&se, NULL);
		  se.expr = gfc_maybe_dereference_var (n->sym, decl);

		  for (gfc_ref *ref = n->expr->ref; ref; ref = ref->next)
		    {
		      if (ref->type == REF_COMPONENT)
			{
			  if (ref->u.c.sym->attr.extension)
			    conv_parent_component_references (&se, ref);

			  gfc_conv_component_ref (&se, ref);
			}
		      else if (ref->type == REF_ARRAY)
			{
			  if (ref->u.ar.type == AR_ELEMENT && ref->next)
			    gfc_conv_array_ref (&se, &ref->u.ar, n->expr,
						&n->expr->where);
			  else
			    gcc_assert (!ref->next);
			}
		      else
			sorry_at (gfc_get_location (&n->where),
				  "unhandled expression type");
		    }

		  tree inner = se.expr;

		  /* Last component is a derived type or class pointer.  */
		  if (lastref->type == REF_COMPONENT
		      && (lastref->u.c.component->ts.type == BT_DERIVED
			  || lastref->u.c.component->ts.type == BT_CLASS))
		    {
		      if (pointer || (openacc && allocatable))
			{
			  /* If it's a bare attach/detach clause, we just want
			     to perform a single attach/detach operation, of the
			     pointer itself, not of the pointed-to object.  */
			  if (openacc
			      && (n->u.map.op == OMP_MAP_ATTACH
				  || n->u.map.op == OMP_MAP_DETACH))
			    {
			      OMP_CLAUSE_DECL (node)
				= build_fold_addr_expr (inner);
			      OMP_CLAUSE_SIZE (node) = size_zero_node;
			      goto finalize_map_clause;
			    }

			  gfc_omp_namelist *n2
			    = openacc ? NULL : clauses->lists[OMP_LIST_MAP];

			  bool sym_based;
			  n2 = get_symbol_rooted_namelist (sym_rooted_nl, n,
							   n2, &sym_based);

			  /* If the last reference is a pointer to a derived
			     type ("foo%dt_ptr"), check if any subcomponents
			     of the same derived type member are being mapped
			     elsewhere in the clause list ("foo%dt_ptr%x",
			     etc.).  If we have such subcomponent mappings,
			     we only create an ALLOC node for the pointer
			     itself, and inhibit mapping the whole derived
			     type.  */

			  for (; n2 != NULL; n2 = n2->next)
			    {
			      if ((!sym_based && n == n2)
				  || (sym_based && n == n2->u2.duplicate_of)
				  || !n2->expr)
				continue;

			      if (!gfc_omp_expr_prefix_same (n->expr,
							     n2->expr))
				continue;

			      gfc_ref *ref1 = n->expr->ref;
			      gfc_ref *ref2 = n2->expr->ref;

			      while (ref1->next && ref2->next)
				{
				  ref1 = ref1->next;
				  ref2 = ref2->next;
				}

			      if (ref2->next)
				{
				  inner = build_fold_addr_expr (inner);
				  OMP_CLAUSE_SET_MAP_KIND (node,
							   GOMP_MAP_ALLOC);
				  OMP_CLAUSE_DECL (node) = inner;
				  OMP_CLAUSE_SIZE (node)
				    = TYPE_SIZE_UNIT (TREE_TYPE (inner));
				  goto finalize_map_clause;
				}
			    }

			  tree data, size;

			  if (lastref->u.c.component->ts.type == BT_CLASS)
			    {
			      data = gfc_class_data_get (inner);
			      gcc_assert (POINTER_TYPE_P (TREE_TYPE (data)));
			      data = build_fold_indirect_ref (data);
			      size = gfc_class_vtab_size_get (inner);
			    }
			  else  /* BT_DERIVED.  */
			    {
			      data = inner;
			      size = TYPE_SIZE_UNIT (TREE_TYPE (inner));
			    }

			  OMP_CLAUSE_DECL (node) = data;
			  OMP_CLAUSE_SIZE (node) = size;
			  node2 = build_omp_clause (input_location,
						    OMP_CLAUSE_MAP);
			  OMP_CLAUSE_SET_MAP_KIND (node2,
						   GOMP_MAP_ATTACH_DETACH);
			  OMP_CLAUSE_DECL (node2) = build_fold_addr_expr (data);
			  OMP_CLAUSE_SIZE (node2) = size_int (0);
			}
		      else
			{
			  OMP_CLAUSE_DECL (node) = inner;
			  OMP_CLAUSE_SIZE (node)
			    = TYPE_SIZE_UNIT (TREE_TYPE (inner));
			}
		    }
		  else if (lastref->type == REF_ARRAY
			   && lastref->u.ar.type == AR_FULL)
		    {
		      /* Bare attach and detach clauses don't want any
			 additional nodes.  */
		      if ((n->u.map.op == OMP_MAP_ATTACH
			   || n->u.map.op == OMP_MAP_DETACH)
			  && (POINTER_TYPE_P (TREE_TYPE (inner))
			      || GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (inner))))
			{
			  if (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (inner)))
			    {
			      tree ptr = gfc_conv_descriptor_data_get (inner);
			      OMP_CLAUSE_DECL (node) = ptr;
			    }
			  else
			    OMP_CLAUSE_DECL (node) = inner;
			  OMP_CLAUSE_SIZE (node) = size_zero_node;
			  goto finalize_map_clause;
			}

		      if (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (inner)))
			{
			  gomp_map_kind map_kind;
			  tree type = TREE_TYPE (inner);
			  tree ptr = gfc_conv_descriptor_data_get (inner);
			  ptr = build_fold_indirect_ref (ptr);
			  OMP_CLAUSE_DECL (node) = ptr;
			  int rank = GFC_TYPE_ARRAY_RANK (type);
			  OMP_CLAUSE_SIZE (node)
			    = gfc_full_array_size (block, inner, rank);
			  tree elemsz
			    = TYPE_SIZE_UNIT (gfc_get_element_type (type));
			  map_kind = OMP_CLAUSE_MAP_KIND (node);
			  if (GOMP_MAP_COPY_TO_P (map_kind)
			      || map_kind == GOMP_MAP_ALLOC)
			    map_kind = ((GOMP_MAP_ALWAYS_P (map_kind)
					 || gfc_expr_attr (n->expr).pointer)
					? GOMP_MAP_ALWAYS_TO : GOMP_MAP_TO);
			  else if (n->u.map.op == OMP_MAP_RELEASE
				   || n->u.map.op == OMP_MAP_DELETE)
			    ;
			  else if (op == EXEC_OMP_TARGET_EXIT_DATA
				   || op == EXEC_OACC_EXIT_DATA)
			    map_kind = GOMP_MAP_RELEASE;
			  else
			    map_kind = GOMP_MAP_ALLOC;
			  if (!openacc
			      && n->expr->ts.type == BT_CHARACTER
			      && n->expr->ts.deferred)
			    {
			      gcc_assert (se.string_length);
			      tree len = fold_convert (size_type_node,
						       se.string_length);
			      elemsz = gfc_get_char_type (n->expr->ts.kind);
			      elemsz = TYPE_SIZE_UNIT (elemsz);
			      elemsz = fold_build2 (MULT_EXPR, size_type_node,
						    len, elemsz);
			      node4 = build_omp_clause (input_location,
							OMP_CLAUSE_MAP);
			      OMP_CLAUSE_SET_MAP_KIND (node4, map_kind);
			      OMP_CLAUSE_DECL (node4) = se.string_length;
			      OMP_CLAUSE_SIZE (node4)
				= TYPE_SIZE_UNIT (gfc_charlen_type_node);
			    }
			  elemsz = fold_convert (gfc_array_index_type, elemsz);
			  OMP_CLAUSE_SIZE (node)
			    = fold_build2 (MULT_EXPR, gfc_array_index_type,
					   OMP_CLAUSE_SIZE (node), elemsz);
			  node2 = build_omp_clause (input_location,
						    OMP_CLAUSE_MAP);
			  if (map_kind == GOMP_MAP_RELEASE
			      || map_kind == GOMP_MAP_DELETE)
			    {
			      OMP_CLAUSE_SET_MAP_KIND (node2, map_kind);
			      OMP_CLAUSE_RELEASE_DESCRIPTOR (node2) = 1;
			    }
			  else
			    OMP_CLAUSE_SET_MAP_KIND (node2,
						     GOMP_MAP_TO_PSET);
			  OMP_CLAUSE_DECL (node2) = inner;
			  OMP_CLAUSE_SIZE (node2) = TYPE_SIZE_UNIT (type);
			  if (!openacc)
			    {
			      gfc_omp_namelist *n2
				= clauses->lists[OMP_LIST_MAP];

			      /* If we don't have a mapping of a smaller part
				 of the array -- or we can't prove that we do
				 statically -- set this flag.  If there is a
				 mapping of a smaller part of the array after
				 all, this will turn into a no-op at
				 runtime.  */
			      OMP_CLAUSE_MAP_RUNTIME_IMPLICIT_P (node) = 1;

			      bool sym_based;
			      n2 = get_symbol_rooted_namelist (sym_rooted_nl,
							       n, n2,
							       &sym_based);

			      bool drop_mapping = false;

			      for (; n2 != NULL; n2 = n2->next)
				{
				  if ((!sym_based && n == n2)
				      || (sym_based && n == n2->u2.duplicate_of)
				      || !n2->expr)
				    continue;

				  if (!gfc_omp_expr_prefix_same (n->expr,
								 n2->expr))
				    continue;

				  gfc_ref *ref1 = n->expr->ref;
				  gfc_ref *ref2 = n2->expr->ref;

				  /* We know ref1 and ref2 overlap.  We're
				     interested in whether ref2 describes a
				     smaller part of the array than ref1, which
				     we already know refers to the full
				     array.  */

				  while (ref1->next && ref2->next)
				    {
				      ref1 = ref1->next;
				      ref2 = ref2->next;
				    }

				  if (ref2->next
				      || (ref2->type == REF_ARRAY
					  && (ref2->u.ar.type == AR_ELEMENT
					      || (ref2->u.ar.type
						  == AR_SECTION))))
				    {
				      drop_mapping = true;
				      break;
				    }
				}
			      if (drop_mapping)
				continue;
			    }
			  node3 = build_omp_clause (input_location,
						    OMP_CLAUSE_MAP);
			  OMP_CLAUSE_SET_MAP_KIND (node3,
						   GOMP_MAP_ATTACH_DETACH);
			  OMP_CLAUSE_DECL (node3)
			    = gfc_conv_descriptor_data_get (inner);
			  /* Similar to gfc_trans_omp_array_section (details
			     there), we add/keep the cast for OpenMP to prevent
			     that an 'alloc:' gets added for node3 ('desc.data')
			     as that is part of the whole descriptor (node3).
			     TODO: Remove once the ME handles this properly.  */
			  if (!openacc)
			    OMP_CLAUSE_DECL (node3)
				= fold_convert (TREE_TYPE (TREE_OPERAND(ptr, 0)),
						OMP_CLAUSE_DECL (node3));
			  else
			    STRIP_NOPS (OMP_CLAUSE_DECL (node3));
			  OMP_CLAUSE_SIZE (node3) = size_int (0);
			}
		      else
			OMP_CLAUSE_DECL (node) = inner;
		    }
		  else if (lastref->type == REF_ARRAY)
		    {
		      /* An array element or section.  */
		      bool element = lastref->u.ar.type == AR_ELEMENT;
		      gomp_map_kind kind = GOMP_MAP_ATTACH_DETACH;
		      gfc_trans_omp_array_section (block, op, n, inner, element,
						   !openacc, kind, node, node2,
						   node3, node4);
		    }
		  else
		    gcc_unreachable ();
		}
	      else
		sorry_at (gfc_get_location (&n->where), "unhandled expression");

	      finalize_map_clause:

	      omp_clauses = gfc_trans_add_clause (node, omp_clauses);
	      if (node2)
		omp_clauses = gfc_trans_add_clause (node2, omp_clauses);
	      if (node3)
		omp_clauses = gfc_trans_add_clause (node3, omp_clauses);
	      if (node4)
		omp_clauses = gfc_trans_add_clause (node4, omp_clauses);
	      if (node5)
		omp_clauses = gfc_trans_add_clause (node5, omp_clauses);
	    }
	  break;
	case OMP_LIST_TO:
	case OMP_LIST_FROM:
	case OMP_LIST_CACHE:
	  for (; n != NULL; n = n->next)
	    {
	      if (!n->sym->attr.referenced)
		continue;

	      switch (list)
		{
		case OMP_LIST_TO:
		  clause_code = OMP_CLAUSE_TO;
		  break;
		case OMP_LIST_FROM:
		  clause_code = OMP_CLAUSE_FROM;
		  break;
		case OMP_LIST_CACHE:
		  clause_code = OMP_CLAUSE__CACHE_;
		  break;
		default:
		  gcc_unreachable ();
		}
	      tree node = build_omp_clause (input_location, clause_code);
	      if (n->expr == NULL
		  || (n->expr->ref->type == REF_ARRAY
		      && n->expr->ref->u.ar.type == AR_FULL
		      && n->expr->ref->next == NULL))
		{
		  tree decl = gfc_trans_omp_variable (n->sym, false);
		  if (gfc_omp_privatize_by_reference (decl))
		    {
		      if (gfc_omp_is_allocatable_or_ptr (decl))
			decl = build_fold_indirect_ref (decl);
		      decl = build_fold_indirect_ref (decl);
		    }
		  else if (DECL_P (decl))
		    TREE_ADDRESSABLE (decl) = 1;
		  if (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (decl)))
		    {
		      tree type = TREE_TYPE (decl);
		      tree ptr = gfc_conv_descriptor_data_get (decl);
		      gcc_assert (POINTER_TYPE_P (TREE_TYPE (ptr)));
		      ptr = build_fold_indirect_ref (ptr);
		      OMP_CLAUSE_DECL (node) = ptr;
		      OMP_CLAUSE_SIZE (node)
			= gfc_full_array_size (block, decl,
					       GFC_TYPE_ARRAY_RANK (type));
		      tree elemsz
			= TYPE_SIZE_UNIT (gfc_get_element_type (type));
		      elemsz = fold_convert (gfc_array_index_type, elemsz);
		      OMP_CLAUSE_SIZE (node)
			= fold_build2 (MULT_EXPR, gfc_array_index_type,
				       OMP_CLAUSE_SIZE (node), elemsz);
		    }
		  else
		    {
		      OMP_CLAUSE_DECL (node) = decl;
		      if (gfc_omp_is_allocatable_or_ptr (decl))
			OMP_CLAUSE_SIZE (node)
				= TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (decl)));
		    }
		}
	      else
		{
		  tree ptr;
		  gfc_init_se (&se, NULL);
		  if (n->expr->rank == 0)
		    {
		      gfc_conv_expr_reference (&se, n->expr);
		      ptr = se.expr;
		      gfc_add_block_to_block (block, &se.pre);
		      OMP_CLAUSE_SIZE (node)
			= TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (ptr)));
		    }
		  else
		    {
		      gfc_conv_expr_descriptor (&se, n->expr);
		      ptr = gfc_conv_array_data (se.expr);
		      tree type = TREE_TYPE (se.expr);
		      gfc_add_block_to_block (block, &se.pre);
		      OMP_CLAUSE_SIZE (node)
			= gfc_full_array_size (block, se.expr,
					       GFC_TYPE_ARRAY_RANK (type));
		      tree elemsz
			= TYPE_SIZE_UNIT (gfc_get_element_type (type));
		      elemsz = fold_convert (gfc_array_index_type, elemsz);
		      OMP_CLAUSE_SIZE (node)
			= fold_build2 (MULT_EXPR, gfc_array_index_type,
				       OMP_CLAUSE_SIZE (node), elemsz);
		    }
		  gfc_add_block_to_block (block, &se.post);
		  gcc_assert (POINTER_TYPE_P (TREE_TYPE (ptr)));
		  OMP_CLAUSE_DECL (node) = build_fold_indirect_ref (ptr);
		}
	      if (n->u.present_modifier)
		OMP_CLAUSE_MOTION_PRESENT (node) = 1;
	      if (list == OMP_LIST_CACHE && n->u.map.readonly)
		OMP_CLAUSE__CACHE__READONLY (node) = 1;
	      omp_clauses = gfc_trans_add_clause (node, omp_clauses);
	    }
	  break;
	case OMP_LIST_USES_ALLOCATORS:
	  /* Ignore pre-defined allocators as no special treatment is needed. */
	  for (; n != NULL; n = n->next)
	    if (n->sym->attr.flavor == FL_VARIABLE)
	      break;
	  if (n != NULL)
	    sorry_at (input_location, "%<uses_allocators%> clause with traits "
				      "and memory spaces");
	  break;
	default:
	  break;
	}
    }

  /* Free hashmap if we built it.  */
  if (sym_rooted_nl)
    {
      typedef hash_map<gfc_symbol *, gfc_omp_namelist *>::iterator hti;
      for (hti it = sym_rooted_nl->begin (); it != sym_rooted_nl->end (); ++it)
	{
	  gfc_omp_namelist *&nl = (*it).second;
	  while (nl)
	    {
	      gfc_omp_namelist *next = nl->next;
	      free (nl);
	      nl = next;
	    }
	}
      delete sym_rooted_nl;
    }

  if (clauses->if_expr)
    {
      tree if_var;

      gfc_init_se (&se, NULL);
      gfc_conv_expr (&se, clauses->if_expr);
      gfc_add_block_to_block (block, &se.pre);
      if_var = gfc_evaluate_now (se.expr, block);
      gfc_add_block_to_block (block, &se.post);

      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_IF);
      OMP_CLAUSE_IF_MODIFIER (c) = ERROR_MARK;
      OMP_CLAUSE_IF_EXPR (c) = if_var;
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }

  for (ifc = 0; ifc < OMP_IF_LAST; ifc++)
    if (clauses->if_exprs[ifc])
      {
	tree if_var;

	gfc_init_se (&se, NULL);
	gfc_conv_expr (&se, clauses->if_exprs[ifc]);
	gfc_add_block_to_block (block, &se.pre);
	if_var = gfc_evaluate_now (se.expr, block);
	gfc_add_block_to_block (block, &se.post);

	c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_IF);
	switch (ifc)
	  {
	  case OMP_IF_CANCEL:
	    OMP_CLAUSE_IF_MODIFIER (c) = VOID_CST;
	    break;
	  case OMP_IF_PARALLEL:
	    OMP_CLAUSE_IF_MODIFIER (c) = OMP_PARALLEL;
	    break;
	  case OMP_IF_SIMD:
	    OMP_CLAUSE_IF_MODIFIER (c) = OMP_SIMD;
	    break;
	  case OMP_IF_TASK:
	    OMP_CLAUSE_IF_MODIFIER (c) = OMP_TASK;
	    break;
	  case OMP_IF_TASKLOOP:
	    OMP_CLAUSE_IF_MODIFIER (c) = OMP_TASKLOOP;
	    break;
	  case OMP_IF_TARGET:
	    OMP_CLAUSE_IF_MODIFIER (c) = OMP_TARGET;
	    break;
	  case OMP_IF_TARGET_DATA:
	    OMP_CLAUSE_IF_MODIFIER (c) = OMP_TARGET_DATA;
	    break;
	  case OMP_IF_TARGET_UPDATE:
	    OMP_CLAUSE_IF_MODIFIER (c) = OMP_TARGET_UPDATE;
	    break;
	  case OMP_IF_TARGET_ENTER_DATA:
	    OMP_CLAUSE_IF_MODIFIER (c) = OMP_TARGET_ENTER_DATA;
	    break;
	  case OMP_IF_TARGET_EXIT_DATA:
	    OMP_CLAUSE_IF_MODIFIER (c) = OMP_TARGET_EXIT_DATA;
	    break;
	  default:
	    gcc_unreachable ();
	  }
	OMP_CLAUSE_IF_EXPR (c) = if_var;
	omp_clauses = gfc_trans_add_clause (c, omp_clauses);
      }

  if (clauses->self_expr)
    {
      tree self_var;

      gfc_init_se (&se, NULL);
      gfc_conv_expr (&se, clauses->self_expr);
      gfc_add_block_to_block (block, &se.pre);
      self_var = gfc_evaluate_now (se.expr, block);
      gfc_add_block_to_block (block, &se.post);

      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_SELF);
      OMP_CLAUSE_SELF_EXPR (c) = self_var;
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }

  if (clauses->final_expr)
    {
      tree final_var;

      gfc_init_se (&se, NULL);
      gfc_conv_expr (&se, clauses->final_expr);
      gfc_add_block_to_block (block, &se.pre);
      final_var = gfc_evaluate_now (se.expr, block);
      gfc_add_block_to_block (block, &se.post);

      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_FINAL);
      OMP_CLAUSE_FINAL_EXPR (c) = final_var;
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }

  if (clauses->novariants)
    {
      tree novariants_var;

      gfc_init_se (&se, NULL);
      gfc_conv_expr (&se, clauses->novariants);
      gfc_add_block_to_block (block, &se.pre);
      novariants_var = gfc_evaluate_now (se.expr, block);
      gfc_add_block_to_block (block, &se.post);

      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_NOVARIANTS);
      OMP_CLAUSE_NOVARIANTS_EXPR (c) = novariants_var;
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }

  if (clauses->nocontext)
    {
      tree nocontext_var;

      gfc_init_se (&se, NULL);
      gfc_conv_expr (&se, clauses->nocontext);
      gfc_add_block_to_block (block, &se.pre);
      nocontext_var = gfc_evaluate_now (se.expr, block);
      gfc_add_block_to_block (block, &se.post);

      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_NOCONTEXT);
      OMP_CLAUSE_NOCONTEXT_EXPR (c) = nocontext_var;
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

      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_NUM_THREADS);
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
      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_SCHEDULE);
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
      if (clauses->sched_monotonic)
	OMP_CLAUSE_SCHEDULE_KIND (c)
	  = (omp_clause_schedule_kind) (OMP_CLAUSE_SCHEDULE_KIND (c)
					| OMP_CLAUSE_SCHEDULE_MONOTONIC);
      else if (clauses->sched_nonmonotonic)
	OMP_CLAUSE_SCHEDULE_KIND (c)
	  = (omp_clause_schedule_kind) (OMP_CLAUSE_SCHEDULE_KIND (c)
					| OMP_CLAUSE_SCHEDULE_NONMONOTONIC);
      if (clauses->sched_simd)
	OMP_CLAUSE_SCHEDULE_SIMD (c) = 1;
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }

  if (clauses->default_sharing != OMP_DEFAULT_UNKNOWN)
    {
      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_DEFAULT);
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
	case OMP_DEFAULT_PRESENT:
	  OMP_CLAUSE_DEFAULT_KIND (c) = OMP_CLAUSE_DEFAULT_PRESENT;
	  break;
	default:
	  gcc_unreachable ();
	}
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }

  if (clauses->nowait)
    {
      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_NOWAIT);
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }

  if (clauses->full)
    {
      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_FULL);
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }

  if (clauses->partial)
    {
      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_PARTIAL);
      OMP_CLAUSE_PARTIAL_EXPR (c)
	= (clauses->partial > 0
	   ? build_int_cst (integer_type_node, clauses->partial)
	   : NULL_TREE);
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }

  if (clauses->sizes_list)
    {
      tree list = NULL_TREE;
      for (gfc_expr_list *el = clauses->sizes_list; el; el = el->next)
	list = tree_cons (NULL_TREE, gfc_convert_expr_to_tree (block, el->expr),
			  list);

      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_SIZES);
      OMP_CLAUSE_SIZES_LIST (c) = nreverse (list);
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }

  if (clauses->ordered)
    {
      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_ORDERED);
      OMP_CLAUSE_ORDERED_EXPR (c)
	= clauses->orderedc ? build_int_cst (integer_type_node,
					     clauses->orderedc) : NULL_TREE;
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }

  if (clauses->order_concurrent)
    {
      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_ORDER);
      OMP_CLAUSE_ORDER_UNCONSTRAINED (c) = clauses->order_unconstrained;
      OMP_CLAUSE_ORDER_REPRODUCIBLE (c) = clauses->order_reproducible;
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }

  if (clauses->untied)
    {
      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_UNTIED);
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }

  if (clauses->mergeable)
    {
      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_MERGEABLE);
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }

  if (clauses->collapse)
    {
      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_COLLAPSE);
      OMP_CLAUSE_COLLAPSE_EXPR (c)
	= build_int_cst (integer_type_node, clauses->collapse);
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }

  if (clauses->inbranch)
    {
      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_INBRANCH);
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }

  if (clauses->notinbranch)
    {
      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_NOTINBRANCH);
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }

  switch (clauses->cancel)
    {
    case OMP_CANCEL_UNKNOWN:
      break;
    case OMP_CANCEL_PARALLEL:
      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_PARALLEL);
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
      break;
    case OMP_CANCEL_SECTIONS:
      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_SECTIONS);
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
      break;
    case OMP_CANCEL_DO:
      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_FOR);
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
      break;
    case OMP_CANCEL_TASKGROUP:
      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_TASKGROUP);
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
      break;
    }

  if (clauses->proc_bind != OMP_PROC_BIND_UNKNOWN)
    {
      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_PROC_BIND);
      switch (clauses->proc_bind)
	{
	case OMP_PROC_BIND_PRIMARY:
	  OMP_CLAUSE_PROC_BIND_KIND (c) = OMP_CLAUSE_PROC_BIND_PRIMARY;
	  break;
	case OMP_PROC_BIND_MASTER:
	  OMP_CLAUSE_PROC_BIND_KIND (c) = OMP_CLAUSE_PROC_BIND_MASTER;
	  break;
	case OMP_PROC_BIND_SPREAD:
	  OMP_CLAUSE_PROC_BIND_KIND (c) = OMP_CLAUSE_PROC_BIND_SPREAD;
	  break;
	case OMP_PROC_BIND_CLOSE:
	  OMP_CLAUSE_PROC_BIND_KIND (c) = OMP_CLAUSE_PROC_BIND_CLOSE;
	  break;
	default:
	  gcc_unreachable ();
	}
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }

  if (clauses->safelen_expr)
    {
      tree safelen_var;

      gfc_init_se (&se, NULL);
      gfc_conv_expr (&se, clauses->safelen_expr);
      gfc_add_block_to_block (block, &se.pre);
      safelen_var = gfc_evaluate_now (se.expr, block);
      gfc_add_block_to_block (block, &se.post);

      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_SAFELEN);
      OMP_CLAUSE_SAFELEN_EXPR (c) = safelen_var;
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }

  if (clauses->simdlen_expr)
    {
      if (declare_simd)
	{
	  c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_SIMDLEN);
	  OMP_CLAUSE_SIMDLEN_EXPR (c)
	    = gfc_conv_constant_to_tree (clauses->simdlen_expr);
	  omp_clauses = gfc_trans_add_clause (c, omp_clauses);
	}
      else
	{
	  tree simdlen_var;

	  gfc_init_se (&se, NULL);
	  gfc_conv_expr (&se, clauses->simdlen_expr);
	  gfc_add_block_to_block (block, &se.pre);
	  simdlen_var = gfc_evaluate_now (se.expr, block);
	  gfc_add_block_to_block (block, &se.post);

	  c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_SIMDLEN);
	  OMP_CLAUSE_SIMDLEN_EXPR (c) = simdlen_var;
	  omp_clauses = gfc_trans_add_clause (c, omp_clauses);
	}
    }

  if (clauses->num_teams_upper)
    {
      tree num_teams_lower = NULL_TREE, num_teams_upper;

      gfc_init_se (&se, NULL);
      gfc_conv_expr (&se, clauses->num_teams_upper);
      gfc_add_block_to_block (block, &se.pre);
      num_teams_upper = gfc_evaluate_now (se.expr, block);
      gfc_add_block_to_block (block, &se.post);

      if (clauses->num_teams_lower)
	{
	  gfc_init_se (&se, NULL);
	  gfc_conv_expr (&se, clauses->num_teams_lower);
	  gfc_add_block_to_block (block, &se.pre);
	  num_teams_lower = gfc_evaluate_now (se.expr, block);
	  gfc_add_block_to_block (block, &se.post);
	}
      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_NUM_TEAMS);
      OMP_CLAUSE_NUM_TEAMS_LOWER_EXPR (c) = num_teams_lower;
      OMP_CLAUSE_NUM_TEAMS_UPPER_EXPR (c) = num_teams_upper;
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }

  if (clauses->device)
    {
      tree device;

      gfc_init_se (&se, NULL);
      gfc_conv_expr (&se, clauses->device);
      gfc_add_block_to_block (block, &se.pre);
      device = gfc_evaluate_now (se.expr, block);
      gfc_add_block_to_block (block, &se.post);

      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_DEVICE);
      OMP_CLAUSE_DEVICE_ID (c) = device;

      if (clauses->ancestor)
	OMP_CLAUSE_DEVICE_ANCESTOR (c) = 1;

      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }

  if (clauses->thread_limit)
    {
      tree thread_limit;

      gfc_init_se (&se, NULL);
      gfc_conv_expr (&se, clauses->thread_limit);
      gfc_add_block_to_block (block, &se.pre);
      thread_limit = gfc_evaluate_now (se.expr, block);
      gfc_add_block_to_block (block, &se.post);

      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_THREAD_LIMIT);
      OMP_CLAUSE_THREAD_LIMIT_EXPR (c) = thread_limit;
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }

  chunk_size = NULL_TREE;
  if (clauses->dist_chunk_size)
    {
      gfc_init_se (&se, NULL);
      gfc_conv_expr (&se, clauses->dist_chunk_size);
      gfc_add_block_to_block (block, &se.pre);
      chunk_size = gfc_evaluate_now (se.expr, block);
      gfc_add_block_to_block (block, &se.post);
    }

  if (clauses->dist_sched_kind != OMP_SCHED_NONE)
    {
      c = build_omp_clause (gfc_get_location (&where),
			    OMP_CLAUSE_DIST_SCHEDULE);
      OMP_CLAUSE_DIST_SCHEDULE_CHUNK_EXPR (c) = chunk_size;
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }

  if (clauses->grainsize)
    {
      tree grainsize;

      gfc_init_se (&se, NULL);
      gfc_conv_expr (&se, clauses->grainsize);
      gfc_add_block_to_block (block, &se.pre);
      grainsize = gfc_evaluate_now (se.expr, block);
      gfc_add_block_to_block (block, &se.post);

      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_GRAINSIZE);
      OMP_CLAUSE_GRAINSIZE_EXPR (c) = grainsize;
      if (clauses->grainsize_strict)
	OMP_CLAUSE_GRAINSIZE_STRICT (c) = 1;
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }

  if (clauses->num_tasks)
    {
      tree num_tasks;

      gfc_init_se (&se, NULL);
      gfc_conv_expr (&se, clauses->num_tasks);
      gfc_add_block_to_block (block, &se.pre);
      num_tasks = gfc_evaluate_now (se.expr, block);
      gfc_add_block_to_block (block, &se.post);

      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_NUM_TASKS);
      OMP_CLAUSE_NUM_TASKS_EXPR (c) = num_tasks;
      if (clauses->num_tasks_strict)
	OMP_CLAUSE_NUM_TASKS_STRICT (c) = 1;
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }

  if (clauses->priority)
    {
      tree priority;

      gfc_init_se (&se, NULL);
      gfc_conv_expr (&se, clauses->priority);
      gfc_add_block_to_block (block, &se.pre);
      priority = gfc_evaluate_now (se.expr, block);
      gfc_add_block_to_block (block, &se.post);

      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_PRIORITY);
      OMP_CLAUSE_PRIORITY_EXPR (c) = priority;
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }

  if (clauses->detach)
    {
      tree detach;

      gfc_init_se (&se, NULL);
      gfc_conv_expr (&se, clauses->detach);
      gfc_add_block_to_block (block, &se.pre);
      detach = se.expr;
      gfc_add_block_to_block (block, &se.post);

      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_DETACH);
      TREE_ADDRESSABLE (detach) = 1;
      OMP_CLAUSE_DECL (c) = detach;
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }

  if (clauses->filter)
    {
      tree filter;

      gfc_init_se (&se, NULL);
      gfc_conv_expr (&se, clauses->filter);
      gfc_add_block_to_block (block, &se.pre);
      filter = gfc_evaluate_now (se.expr, block);
      gfc_add_block_to_block (block, &se.post);

      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_FILTER);
      OMP_CLAUSE_FILTER_EXPR (c) = filter;
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }

  if (clauses->hint)
    {
      tree hint;

      gfc_init_se (&se, NULL);
      gfc_conv_expr (&se, clauses->hint);
      gfc_add_block_to_block (block, &se.pre);
      hint = gfc_evaluate_now (se.expr, block);
      gfc_add_block_to_block (block, &se.post);

      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_HINT);
      OMP_CLAUSE_HINT_EXPR (c) = hint;
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }

  if (clauses->simd)
    {
      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_SIMD);
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }
  if (clauses->threads)
    {
      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_THREADS);
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }
  if (clauses->nogroup)
    {
      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_NOGROUP);
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }

  for (int i = 0; i < OMP_DEFAULTMAP_CAT_NUM; i++)
    {
      if (clauses->defaultmap[i] == OMP_DEFAULTMAP_UNSET)
       continue;
      enum omp_clause_defaultmap_kind behavior, category;
      switch ((gfc_omp_defaultmap_category) i)
	{
	case OMP_DEFAULTMAP_CAT_UNCATEGORIZED:
	  category = OMP_CLAUSE_DEFAULTMAP_CATEGORY_UNSPECIFIED;
	  break;
	case OMP_DEFAULTMAP_CAT_ALL:
	  category = OMP_CLAUSE_DEFAULTMAP_CATEGORY_ALL;
	  break;
	case OMP_DEFAULTMAP_CAT_SCALAR:
	  category = OMP_CLAUSE_DEFAULTMAP_CATEGORY_SCALAR;
	  break;
	case OMP_DEFAULTMAP_CAT_AGGREGATE:
	  category = OMP_CLAUSE_DEFAULTMAP_CATEGORY_AGGREGATE;
	  break;
	case OMP_DEFAULTMAP_CAT_ALLOCATABLE:
	  category = OMP_CLAUSE_DEFAULTMAP_CATEGORY_ALLOCATABLE;
	  break;
	case OMP_DEFAULTMAP_CAT_POINTER:
	  category = OMP_CLAUSE_DEFAULTMAP_CATEGORY_POINTER;
	  break;
	default: gcc_unreachable ();
	}
      switch (clauses->defaultmap[i])
	{
	case OMP_DEFAULTMAP_ALLOC:
	  behavior = OMP_CLAUSE_DEFAULTMAP_ALLOC;
	  break;
	case OMP_DEFAULTMAP_TO: behavior = OMP_CLAUSE_DEFAULTMAP_TO; break;
	case OMP_DEFAULTMAP_FROM: behavior = OMP_CLAUSE_DEFAULTMAP_FROM; break;
	case OMP_DEFAULTMAP_TOFROM:
	  behavior = OMP_CLAUSE_DEFAULTMAP_TOFROM;
	  break;
	case OMP_DEFAULTMAP_FIRSTPRIVATE:
	  behavior = OMP_CLAUSE_DEFAULTMAP_FIRSTPRIVATE;
	  break;
	case OMP_DEFAULTMAP_PRESENT:
	  behavior = OMP_CLAUSE_DEFAULTMAP_PRESENT;
	  break;
	case OMP_DEFAULTMAP_NONE: behavior = OMP_CLAUSE_DEFAULTMAP_NONE; break;
	case OMP_DEFAULTMAP_DEFAULT:
	  behavior = OMP_CLAUSE_DEFAULTMAP_DEFAULT;
	  break;
	default: gcc_unreachable ();
	}
      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_DEFAULTMAP);
      OMP_CLAUSE_DEFAULTMAP_SET_KIND (c, behavior, category);
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }

  if (clauses->doacross_source)
    {
      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_DOACROSS);
      OMP_CLAUSE_DOACROSS_KIND (c) = OMP_CLAUSE_DOACROSS_SOURCE;
      OMP_CLAUSE_DOACROSS_DEPEND (c) = clauses->depend_source;
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }

  if (clauses->async)
    {
      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_ASYNC);
      if (clauses->async_expr)
	OMP_CLAUSE_ASYNC_EXPR (c)
	  = gfc_convert_expr_to_tree (block, clauses->async_expr);
      else
	OMP_CLAUSE_ASYNC_EXPR (c) = NULL;
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }
  if (clauses->seq)
    {
      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_SEQ);
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }
  if (clauses->par_auto)
    {
      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_AUTO);
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }
  if (clauses->if_present)
    {
      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_IF_PRESENT);
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }
  if (clauses->finalize)
    {
      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_FINALIZE);
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }
  if (clauses->independent)
    {
      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_INDEPENDENT);
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }
  if (clauses->wait_list)
    {
      gfc_expr_list *el;

      for (el = clauses->wait_list; el; el = el->next)
	{
	  c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_WAIT);
	  OMP_CLAUSE_DECL (c) = gfc_convert_expr_to_tree (block, el->expr);
	  OMP_CLAUSE_CHAIN (c) = omp_clauses;
	  omp_clauses = c;
	}
    }
  if (clauses->num_gangs_expr)
    {
      tree num_gangs_var
	= gfc_convert_expr_to_tree (block, clauses->num_gangs_expr);
      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_NUM_GANGS);
      OMP_CLAUSE_NUM_GANGS_EXPR (c) = num_gangs_var;
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }
  if (clauses->num_workers_expr)
    {
      tree num_workers_var
	= gfc_convert_expr_to_tree (block, clauses->num_workers_expr);
      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_NUM_WORKERS);
      OMP_CLAUSE_NUM_WORKERS_EXPR (c) = num_workers_var;
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }
  if (clauses->vector_length_expr)
    {
      tree vector_length_var
	= gfc_convert_expr_to_tree (block, clauses->vector_length_expr);
      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_VECTOR_LENGTH);
      OMP_CLAUSE_VECTOR_LENGTH_EXPR (c) = vector_length_var;
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }
  if (clauses->tile_list)
    {
      tree list = NULL_TREE;
      for (gfc_expr_list *el = clauses->tile_list; el; el = el->next)
	list = tree_cons (NULL_TREE, gfc_convert_expr_to_tree (block, el->expr),
			  list);

      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_TILE);
      OMP_CLAUSE_TILE_LIST (c) = nreverse (list);
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
    }
  if (clauses->vector)
    {
      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_VECTOR);
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);

      if (clauses->vector_expr)
	{
	  tree vector_var
	    = gfc_convert_expr_to_tree (block, clauses->vector_expr);
	  OMP_CLAUSE_VECTOR_EXPR (c) = vector_var;

	  /* TODO: We're not capturing location information for individual
	     clauses.  However, if we have an expression attached to the
	     clause, that one provides better location information.  */
	  OMP_CLAUSE_LOCATION (c)
	    = gfc_get_location (&clauses->vector_expr->where);
	}
    }
  if (clauses->worker)
    {
      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_WORKER);
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);

      if (clauses->worker_expr)
	{
	  tree worker_var
	    = gfc_convert_expr_to_tree (block, clauses->worker_expr);
	  OMP_CLAUSE_WORKER_EXPR (c) = worker_var;

	  /* TODO: We're not capturing location information for individual
	     clauses.  However, if we have an expression attached to the
	     clause, that one provides better location information.  */
	  OMP_CLAUSE_LOCATION (c)
	    = gfc_get_location (&clauses->worker_expr->where);
	}
    }
  if (clauses->gang)
    {
      tree arg;
      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_GANG);
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);

      if (clauses->gang_num_expr)
	{
	  arg = gfc_convert_expr_to_tree (block, clauses->gang_num_expr);
	  OMP_CLAUSE_GANG_EXPR (c) = arg;

	  /* TODO: We're not capturing location information for individual
	     clauses.  However, if we have an expression attached to the
	     clause, that one provides better location information.  */
	  OMP_CLAUSE_LOCATION (c)
	    = gfc_get_location (&clauses->gang_num_expr->where);
	}

      if (clauses->gang_static)
	{
	  arg = clauses->gang_static_expr
	    ? gfc_convert_expr_to_tree (block, clauses->gang_static_expr)
	    : integer_minus_one_node;
	  OMP_CLAUSE_GANG_STATIC_EXPR (c) = arg;
	}
    }
  if (clauses->bind != OMP_BIND_UNSET)
    {
      c = build_omp_clause (gfc_get_location (&where), OMP_CLAUSE_BIND);
      omp_clauses = gfc_trans_add_clause (c, omp_clauses);
      switch (clauses->bind)
	{
	case OMP_BIND_TEAMS:
	  OMP_CLAUSE_BIND_KIND (c) = OMP_CLAUSE_BIND_TEAMS;
	  break;
	case OMP_BIND_PARALLEL:
	  OMP_CLAUSE_BIND_KIND (c) = OMP_CLAUSE_BIND_PARALLEL;
	  break;
	case OMP_BIND_THREAD:
	  OMP_CLAUSE_BIND_KIND (c) = OMP_CLAUSE_BIND_THREAD;
	  break;
	default:
	  gcc_unreachable ();
	}
    }
  /* OpenACC 'nohost' clauses cannot appear here.  */
  gcc_checking_assert (!clauses->nohost);

  return nreverse (omp_clauses);
}

/* Like gfc_trans_code, but force creation of a BIND_EXPR around it.  */

static tree
gfc_trans_omp_code (gfc_code *code, bool force_empty)
{
  tree stmt;

  pushlevel ();
  stmt = gfc_trans_code (code);
  if (TREE_CODE (stmt) != BIND_EXPR)
    {
      if (!IS_EMPTY_STMT (stmt) || force_empty)
	{
	  tree block = poplevel (1, 0);
	  stmt = build3_v (BIND_EXPR, NULL, stmt, block);
	}
      else
	poplevel (0, 0);
    }
  else
    poplevel (0, 0);
  return stmt;
}

/* Translate OpenACC 'parallel', 'kernels', 'serial', 'data', 'host_data'
   construct. */

static tree
gfc_trans_oacc_construct (gfc_code *code)
{
  stmtblock_t block;
  tree stmt, oacc_clauses;
  enum tree_code construct_code;

  switch (code->op)
    {
      case EXEC_OACC_PARALLEL:
	construct_code = OACC_PARALLEL;
	break;
      case EXEC_OACC_KERNELS:
	construct_code = OACC_KERNELS;
	break;
      case EXEC_OACC_SERIAL:
	construct_code = OACC_SERIAL;
	break;
      case EXEC_OACC_DATA:
	construct_code = OACC_DATA;
	break;
      case EXEC_OACC_HOST_DATA:
	construct_code = OACC_HOST_DATA;
	break;
      default:
	gcc_unreachable ();
    }

  gfc_start_block (&block);
  oacc_clauses = gfc_trans_omp_clauses (&block, code->ext.omp_clauses,
					code->loc, false, true);
  pushlevel ();
  stmt = gfc_trans_omp_code (code->block->next, true);
  stmt = build3_v (BIND_EXPR, NULL, stmt, poplevel (1, 0));
  stmt = build2_loc (gfc_get_location (&code->loc), construct_code,
		     void_type_node, stmt, oacc_clauses);
  gfc_add_expr_to_block (&block, stmt);
  return gfc_finish_block (&block);
}

/* update, enter_data, exit_data, cache. */
static tree
gfc_trans_oacc_executable_directive (gfc_code *code)
{
  stmtblock_t block;
  tree stmt, oacc_clauses;
  enum tree_code construct_code;

  switch (code->op)
    {
      case EXEC_OACC_UPDATE:
	construct_code = OACC_UPDATE;
	break;
      case EXEC_OACC_ENTER_DATA:
	construct_code = OACC_ENTER_DATA;
	break;
      case EXEC_OACC_EXIT_DATA:
	construct_code = OACC_EXIT_DATA;
	break;
      case EXEC_OACC_CACHE:
	construct_code = OACC_CACHE;
	break;
      default:
	gcc_unreachable ();
    }

  gfc_start_block (&block);
  oacc_clauses = gfc_trans_omp_clauses (&block, code->ext.omp_clauses,
					code->loc, false, true, code->op);
  stmt = build1_loc (input_location, construct_code, void_type_node,
		     oacc_clauses);
  gfc_add_expr_to_block (&block, stmt);
  return gfc_finish_block (&block);
}

static tree
gfc_trans_oacc_wait_directive (gfc_code *code)
{
  stmtblock_t block;
  tree stmt, t;
  vec<tree, va_gc> *args;
  int nparms = 0;
  gfc_expr_list *el;
  gfc_omp_clauses *clauses = code->ext.omp_clauses;
  location_t loc = input_location;

  for (el = clauses->wait_list; el; el = el->next)
    nparms++;

  vec_alloc (args, nparms + 2);
  stmt = builtin_decl_explicit (BUILT_IN_GOACC_WAIT);

  gfc_start_block (&block);

  if (clauses->async_expr)
    t = gfc_convert_expr_to_tree (&block, clauses->async_expr);
  else
    t = build_int_cst (integer_type_node, -2);

  args->quick_push (t);
  args->quick_push (build_int_cst (integer_type_node, nparms));

  for (el = clauses->wait_list; el; el = el->next)
    args->quick_push (gfc_convert_expr_to_tree (&block, el->expr));

  stmt = build_call_expr_loc_vec (loc, stmt, args);
  gfc_add_expr_to_block (&block, stmt);

  vec_free (args);

  return gfc_finish_block (&block);
}

static tree gfc_trans_omp_sections (gfc_code *, gfc_omp_clauses *);
static tree gfc_trans_omp_workshare (gfc_code *, gfc_omp_clauses *);

static tree
gfc_trans_omp_allocators (gfc_code *code)
{
  static bool warned = false;
  gfc_omp_namelist *omp_allocate
    = code->ext.omp_clauses->lists[OMP_LIST_ALLOCATE];
  if (!flag_openmp_allocators && !warned)
    {
      omp_allocate = NULL;
      gfc_error ("%<!$OMP %s%> at %L requires %<-fopenmp-allocators%>",
		 code->op == EXEC_OMP_ALLOCATE ? "ALLOCATE" : "ALLOCATORS",
		 &code->loc);
      warning (0, "All files that might deallocate such a variable must be "
		  "compiled with %<-fopenmp-allocators%>");
      inform (UNKNOWN_LOCATION,
	      "This includes explicit DEALLOCATE, reallocation on intrinsic "
	      "assignment, INTENT(OUT) for allocatable dummy arguments, and "
	      "reallocation of allocatable components allocated with an "
	      "OpenMP allocator");
      warned = true;
    }
  return gfc_trans_allocate (code->block->next, omp_allocate);
}

static tree
gfc_trans_omp_assume (gfc_code *code)
{
  stmtblock_t block;
  gfc_init_block (&block);
  gfc_omp_assumptions *assume = code->ext.omp_clauses->assume;
  if (assume)
    for (gfc_expr_list *el = assume->holds; el; el = el->next)
      {
	location_t loc = gfc_get_location (&el->expr->where);
	gfc_se se;
	gfc_init_se (&se, NULL);
	gfc_conv_expr (&se, el->expr);
	tree t;
	if (se.pre.head == NULL_TREE && se.post.head == NULL_TREE)
	  t = se.expr;
	else
	  {
	    tree var = create_tmp_var_raw (boolean_type_node);
	    DECL_CONTEXT (var) = current_function_decl;
	    stmtblock_t block2;
	    gfc_init_block (&block2);
	    gfc_add_block_to_block (&block2, &se.pre);
	    gfc_add_modify_loc (loc, &block2, var,
	    			fold_convert_loc (loc, boolean_type_node,
						  se.expr));
	    gfc_add_block_to_block (&block2, &se.post);
	    t = gfc_finish_block (&block2);
	    t = build4 (TARGET_EXPR, boolean_type_node, var, t, NULL, NULL);
	  }
	t = build_call_expr_internal_loc (loc, IFN_ASSUME,
					  void_type_node, 1, t);
	gfc_add_expr_to_block (&block, t);
      }
  gfc_add_expr_to_block (&block, gfc_trans_omp_code (code->block->next, true));
  return gfc_finish_block (&block);
}

static tree
gfc_trans_omp_atomic (gfc_code *code)
{
  gfc_code *atomic_code = code->block;
  gfc_se lse;
  gfc_se rse;
  gfc_se vse;
  gfc_expr *expr1, *expr2, *e, *capture_expr1 = NULL, *capture_expr2 = NULL;
  gfc_symbol *var;
  stmtblock_t block;
  tree lhsaddr, type, rhs, x, compare = NULL_TREE, comp_tgt = NULL_TREE;
  enum tree_code op = ERROR_MARK;
  enum tree_code aop = OMP_ATOMIC;
  bool var_on_left = false, else_branch = false;
  enum omp_memory_order mo, fail_mo;
  switch (atomic_code->ext.omp_clauses->memorder)
    {
    case OMP_MEMORDER_UNSET: mo = OMP_MEMORY_ORDER_UNSPECIFIED; break;
    case OMP_MEMORDER_ACQ_REL: mo = OMP_MEMORY_ORDER_ACQ_REL; break;
    case OMP_MEMORDER_ACQUIRE: mo = OMP_MEMORY_ORDER_ACQUIRE; break;
    case OMP_MEMORDER_RELAXED: mo = OMP_MEMORY_ORDER_RELAXED; break;
    case OMP_MEMORDER_RELEASE: mo = OMP_MEMORY_ORDER_RELEASE; break;
    case OMP_MEMORDER_SEQ_CST: mo = OMP_MEMORY_ORDER_SEQ_CST; break;
    default: gcc_unreachable ();
    }
  switch (atomic_code->ext.omp_clauses->fail)
    {
    case OMP_MEMORDER_UNSET: fail_mo = OMP_FAIL_MEMORY_ORDER_UNSPECIFIED; break;
    case OMP_MEMORDER_ACQUIRE: fail_mo = OMP_FAIL_MEMORY_ORDER_ACQUIRE; break;
    case OMP_MEMORDER_RELAXED: fail_mo = OMP_FAIL_MEMORY_ORDER_RELAXED; break;
    case OMP_MEMORDER_SEQ_CST: fail_mo = OMP_FAIL_MEMORY_ORDER_SEQ_CST; break;
    default: gcc_unreachable ();
    }
  mo = (omp_memory_order) (mo | fail_mo);

  code = code->block->next;
  if (atomic_code->ext.omp_clauses->compare)
    {
      gfc_expr *comp_expr;
      if (code->op == EXEC_IF)
	{
	  comp_expr = code->block->expr1;
	  gcc_assert (code->block->next->op == EXEC_ASSIGN);
	  expr1 = code->block->next->expr1;
	  expr2 = code->block->next->expr2;
	  if (code->block->block)
	    {
	      gcc_assert (atomic_code->ext.omp_clauses->capture
			  && code->block->block->next->op == EXEC_ASSIGN);
	      else_branch = true;
	      aop = OMP_ATOMIC_CAPTURE_OLD;
	      capture_expr1 = code->block->block->next->expr1;
	      capture_expr2 = code->block->block->next->expr2;
	    }
	  else if (atomic_code->ext.omp_clauses->capture)
	    {
	      gcc_assert (code->next->op == EXEC_ASSIGN);
	      aop = OMP_ATOMIC_CAPTURE_NEW;
	      capture_expr1 = code->next->expr1;
	      capture_expr2 = code->next->expr2;
	    }
	}
      else
	{
	  gcc_assert (atomic_code->ext.omp_clauses->capture
		      && code->op == EXEC_ASSIGN
		      && code->next->op == EXEC_IF);
	  aop = OMP_ATOMIC_CAPTURE_OLD;
	  capture_expr1 = code->expr1;
	  capture_expr2 = code->expr2;
	  expr1 = code->next->block->next->expr1;
	  expr2 = code->next->block->next->expr2;
	  comp_expr = code->next->block->expr1;
	}
      gfc_init_se (&lse, NULL);
      gfc_conv_expr (&lse, comp_expr->value.op.op2);
      gfc_add_block_to_block (&block, &lse.pre);
      compare = lse.expr;
      var = expr1->symtree->n.sym;
    }
  else
    {
      gcc_assert (code->op == EXEC_ASSIGN);
      expr1 = code->expr1;
      expr2 = code->expr2;
      if (atomic_code->ext.omp_clauses->capture
	  && (expr2->expr_type == EXPR_VARIABLE
	      || (expr2->expr_type == EXPR_FUNCTION
		  && expr2->value.function.isym
		  && expr2->value.function.isym->id == GFC_ISYM_CONVERSION
		  && (expr2->value.function.actual->expr->expr_type
		      == EXPR_VARIABLE))))
	{
	  capture_expr1 = expr1;
	  capture_expr2 = expr2;
	  expr1 = code->next->expr1;
	  expr2 = code->next->expr2;
	  aop = OMP_ATOMIC_CAPTURE_OLD;
	}
      else if (atomic_code->ext.omp_clauses->capture)
	{
	  aop = OMP_ATOMIC_CAPTURE_NEW;
	  capture_expr1 = code->next->expr1;
	  capture_expr2 = code->next->expr2;
	}
      var = expr1->symtree->n.sym;
    }

  gfc_init_se (&lse, NULL);
  gfc_init_se (&rse, NULL);
  gfc_init_se (&vse, NULL);
  gfc_start_block (&block);

  if (((atomic_code->ext.omp_clauses->atomic_op & GFC_OMP_ATOMIC_MASK)
       != GFC_OMP_ATOMIC_WRITE)
      && expr2->expr_type == EXPR_FUNCTION
      && expr2->value.function.isym
      && expr2->value.function.isym->id == GFC_ISYM_CONVERSION)
    expr2 = expr2->value.function.actual->expr;

  if ((atomic_code->ext.omp_clauses->atomic_op & GFC_OMP_ATOMIC_MASK)
      == GFC_OMP_ATOMIC_READ)
    {
      gfc_conv_expr (&vse, expr1);
      gfc_add_block_to_block (&block, &vse.pre);

      gfc_conv_expr (&lse, expr2);
      gfc_add_block_to_block (&block, &lse.pre);
      type = TREE_TYPE (lse.expr);
      lhsaddr = gfc_build_addr_expr (NULL, lse.expr);

      x = build1 (OMP_ATOMIC_READ, type, lhsaddr);
      OMP_ATOMIC_MEMORY_ORDER (x) = mo;
      x = convert (TREE_TYPE (vse.expr), x);
      gfc_add_modify (&block, vse.expr, x);

      gfc_add_block_to_block (&block, &lse.pre);
      gfc_add_block_to_block (&block, &rse.pre);

      return gfc_finish_block (&block);
    }

  if (capture_expr2
      && capture_expr2->expr_type == EXPR_FUNCTION
      && capture_expr2->value.function.isym
      && capture_expr2->value.function.isym->id == GFC_ISYM_CONVERSION)
    capture_expr2 = capture_expr2->value.function.actual->expr;
  gcc_assert (!capture_expr2 || capture_expr2->expr_type == EXPR_VARIABLE);

  if (aop == OMP_ATOMIC_CAPTURE_OLD)
    {
      gfc_conv_expr (&vse, capture_expr1);
      gfc_add_block_to_block (&block, &vse.pre);
      gfc_conv_expr (&lse, capture_expr2);
      gfc_add_block_to_block (&block, &lse.pre);
      gfc_init_se (&lse, NULL);
    }

  gfc_conv_expr (&lse, expr1);
  gfc_add_block_to_block (&block, &lse.pre);
  type = TREE_TYPE (lse.expr);
  lhsaddr = gfc_build_addr_expr (NULL, lse.expr);

  if (((atomic_code->ext.omp_clauses->atomic_op & GFC_OMP_ATOMIC_MASK)
       == GFC_OMP_ATOMIC_WRITE)
      || (atomic_code->ext.omp_clauses->atomic_op & GFC_OMP_ATOMIC_SWAP)
      || compare)
    {
      gfc_conv_expr (&rse, expr2);
      gfc_add_block_to_block (&block, &rse.pre);
    }
  else if (expr2->expr_type == EXPR_OP)
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
	  && e->value.function.isym
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
	      && e->value.function.isym
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
      if (e->expr_type == EXPR_FUNCTION
	  && e->value.function.isym
	  && e->value.function.isym->id == GFC_ISYM_CONVERSION)
	e = e->value.function.actual->expr;
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
	      x = fold_build2_loc (input_location, op, TREE_TYPE (accum),
				   accum, rse.expr);
	      gfc_add_modify (&block, accum, x);
	    }

	  rse.expr = accum;
	}

      expr2 = expr2->value.function.actual->next->expr;
    }

  lhsaddr = save_expr (lhsaddr);
  if (TREE_CODE (lhsaddr) != SAVE_EXPR
      && (TREE_CODE (lhsaddr) != ADDR_EXPR
	  || !VAR_P (TREE_OPERAND (lhsaddr, 0))))
    {
      /* Make sure LHS is simple enough so that goa_lhs_expr_p can recognize
	 it even after unsharing function body.  */
      tree var = create_tmp_var_raw (TREE_TYPE (lhsaddr));
      DECL_CONTEXT (var) = current_function_decl;
      lhsaddr = build4 (TARGET_EXPR, TREE_TYPE (lhsaddr), var, lhsaddr,
			NULL_TREE, NULL_TREE);
    }

  if (compare)
    {
      tree var = create_tmp_var_raw (TREE_TYPE (lhsaddr));
      DECL_CONTEXT (var) = current_function_decl;
      lhsaddr = build4 (TARGET_EXPR, TREE_TYPE (lhsaddr), var, lhsaddr, NULL,
			NULL);
      lse.expr = build_fold_indirect_ref_loc (input_location, lhsaddr);
      compare = convert (TREE_TYPE (lse.expr), compare);
      compare = fold_build2_loc (input_location, EQ_EXPR, boolean_type_node,
				 lse.expr, compare);
    }

  if (expr2->expr_type == EXPR_VARIABLE || compare)
    rhs = rse.expr;
  else
    rhs = gfc_evaluate_now (rse.expr, &block);

  if (((atomic_code->ext.omp_clauses->atomic_op & GFC_OMP_ATOMIC_MASK)
       == GFC_OMP_ATOMIC_WRITE)
      || (atomic_code->ext.omp_clauses->atomic_op & GFC_OMP_ATOMIC_SWAP)
      || compare)
    x = rhs;
  else
    {
      x = convert (TREE_TYPE (rhs),
		   build_fold_indirect_ref_loc (input_location, lhsaddr));
      if (var_on_left)
	x = fold_build2_loc (input_location, op, TREE_TYPE (rhs), x, rhs);
      else
	x = fold_build2_loc (input_location, op, TREE_TYPE (rhs), rhs, x);
    }

  if (TREE_CODE (TREE_TYPE (rhs)) == COMPLEX_TYPE
      && TREE_CODE (type) != COMPLEX_TYPE)
    x = fold_build1_loc (input_location, REALPART_EXPR,
			 TREE_TYPE (TREE_TYPE (rhs)), x);

  gfc_add_block_to_block (&block, &lse.pre);
  gfc_add_block_to_block (&block, &rse.pre);

  if (aop == OMP_ATOMIC_CAPTURE_NEW)
    {
      gfc_conv_expr (&vse, capture_expr1);
      gfc_add_block_to_block (&block, &vse.pre);
      gfc_add_block_to_block (&block, &lse.pre);
    }

  if (compare && else_branch)
    {
      tree var2 = create_tmp_var_raw (boolean_type_node);
      DECL_CONTEXT (var2) = current_function_decl;
      comp_tgt = build4 (TARGET_EXPR, boolean_type_node, var2,
			 boolean_false_node, NULL, NULL);
      compare = fold_build2_loc (input_location, MODIFY_EXPR, TREE_TYPE (var2),
				 var2, compare);
      TREE_OPERAND (compare, 0) = comp_tgt;
      compare = omit_one_operand_loc (input_location, boolean_type_node,
				      compare, comp_tgt);
    }

  if (compare)
    x = build3_loc (input_location, COND_EXPR, type, compare,
		    convert (type, x), lse.expr);

  if (aop == OMP_ATOMIC)
    {
      x = build2_v (OMP_ATOMIC, lhsaddr, convert (type, x));
      OMP_ATOMIC_MEMORY_ORDER (x) = mo;
      OMP_ATOMIC_WEAK (x) = atomic_code->ext.omp_clauses->weak;
      gfc_add_expr_to_block (&block, x);
    }
  else
    {
      x = build2 (aop, type, lhsaddr, convert (type, x));
      OMP_ATOMIC_MEMORY_ORDER (x) = mo;
      OMP_ATOMIC_WEAK (x) = atomic_code->ext.omp_clauses->weak;
      if (compare && else_branch)
	{
	  tree vtmp = create_tmp_var_raw (TREE_TYPE (x));
	  DECL_CONTEXT (vtmp) = current_function_decl;
	  x = fold_build2_loc (input_location, MODIFY_EXPR,
			       TREE_TYPE (vtmp), vtmp, x);
	  vtmp = build4 (TARGET_EXPR, TREE_TYPE (vtmp), vtmp,
			 build_zero_cst (TREE_TYPE (vtmp)), NULL, NULL);
	  TREE_OPERAND (x, 0) = vtmp;
	  tree x2 = convert (TREE_TYPE (vse.expr), vtmp);
	  x2 = fold_build2_loc (input_location, MODIFY_EXPR,
			       TREE_TYPE (vse.expr), vse.expr, x2);
	  x2 = build3_loc (input_location, COND_EXPR, void_type_node, comp_tgt,
			   void_node, x2);
	  x = omit_one_operand_loc (input_location, TREE_TYPE (x2), x2, x);
	  gfc_add_expr_to_block (&block, x);
	}
      else
	{
	  x = convert (TREE_TYPE (vse.expr), x);
	  gfc_add_modify (&block, vse.expr, x);
	}
    }

  return gfc_finish_block (&block);
}

static tree
gfc_trans_omp_barrier (void)
{
  tree decl = builtin_decl_explicit (BUILT_IN_GOMP_BARRIER);
  return build_call_expr_loc (input_location, decl, 0);
}

static tree
gfc_trans_omp_cancel (gfc_code *code)
{
  int mask = 0;
  tree ifc = boolean_true_node;
  stmtblock_t block;
  switch (code->ext.omp_clauses->cancel)
    {
    case OMP_CANCEL_PARALLEL: mask = 1; break;
    case OMP_CANCEL_DO: mask = 2; break;
    case OMP_CANCEL_SECTIONS: mask = 4; break;
    case OMP_CANCEL_TASKGROUP: mask = 8; break;
    default: gcc_unreachable ();
    }
  gfc_start_block (&block);
  if (code->ext.omp_clauses->if_expr
      || code->ext.omp_clauses->if_exprs[OMP_IF_CANCEL])
    {
      gfc_se se;
      tree if_var;

      gcc_assert ((code->ext.omp_clauses->if_expr == NULL)
		  ^ (code->ext.omp_clauses->if_exprs[OMP_IF_CANCEL] == NULL));
      gfc_init_se (&se, NULL);
      gfc_conv_expr (&se, code->ext.omp_clauses->if_expr != NULL
			  ? code->ext.omp_clauses->if_expr
			  : code->ext.omp_clauses->if_exprs[OMP_IF_CANCEL]);
      gfc_add_block_to_block (&block, &se.pre);
      if_var = gfc_evaluate_now (se.expr, &block);
      gfc_add_block_to_block (&block, &se.post);
      tree type = TREE_TYPE (if_var);
      ifc = fold_build2_loc (input_location, NE_EXPR,
			     boolean_type_node, if_var,
			     build_zero_cst (type));
    }
  tree decl = builtin_decl_explicit (BUILT_IN_GOMP_CANCEL);
  tree c_bool_type = TREE_TYPE (TREE_TYPE (decl));
  ifc = fold_convert (c_bool_type, ifc);
  gfc_add_expr_to_block (&block,
			 build_call_expr_loc (input_location, decl, 2,
					      build_int_cst (integer_type_node,
							     mask), ifc));
  return gfc_finish_block (&block);
}

static tree
gfc_trans_omp_cancellation_point (gfc_code *code)
{
  int mask = 0;
  switch (code->ext.omp_clauses->cancel)
    {
    case OMP_CANCEL_PARALLEL: mask = 1; break;
    case OMP_CANCEL_DO: mask = 2; break;
    case OMP_CANCEL_SECTIONS: mask = 4; break;
    case OMP_CANCEL_TASKGROUP: mask = 8; break;
    default: gcc_unreachable ();
    }
  tree decl = builtin_decl_explicit (BUILT_IN_GOMP_CANCELLATION_POINT);
  return build_call_expr_loc (input_location, decl, 1,
			      build_int_cst (integer_type_node, mask));
}

static tree
gfc_trans_omp_critical (gfc_code *code)
{
  stmtblock_t block;
  tree stmt, name = NULL_TREE;
  if (code->ext.omp_clauses->critical_name != NULL)
    name = get_identifier (code->ext.omp_clauses->critical_name);
  gfc_start_block (&block);
  stmt = make_node (OMP_CRITICAL);
  SET_EXPR_LOCATION (stmt, gfc_get_location (&code->loc));
  TREE_TYPE (stmt) = void_type_node;
  OMP_CRITICAL_BODY (stmt) = gfc_trans_code (code->block->next);
  OMP_CRITICAL_NAME (stmt) = name;
  OMP_CRITICAL_CLAUSES (stmt) = gfc_trans_omp_clauses (&block,
						       code->ext.omp_clauses,
						       code->loc);
  gfc_add_expr_to_block (&block, stmt);
  return gfc_finish_block (&block);
}

typedef struct dovar_init_d {
  gfc_symbol *sym;
  tree var;
  tree init;
  bool non_unit_iter;
} dovar_init;

static bool
gfc_nonrect_loop_expr (stmtblock_t *pblock, gfc_se *sep, int loop_n,
		       gfc_code *code, gfc_expr *expr, vec<dovar_init> *inits,
		       int simple, gfc_expr *curr_loop_var)
{
  int i;
  for (i = 0; i < loop_n; i++)
    {
      gcc_assert (code->ext.iterator->var->expr_type == EXPR_VARIABLE);
      if (gfc_find_sym_in_expr (code->ext.iterator->var->symtree->n.sym, expr))
	break;
      code = code->block->next;
    }
  if (i >= loop_n)
    return false;

  /* Canonical format: TREE_VEC with [var, multiplier, offset].  */
  gfc_symbol *var = code->ext.iterator->var->symtree->n.sym;

  tree tree_var = NULL_TREE;
  tree a1 = integer_one_node;
  tree a2 = integer_zero_node;

  if (!simple)
    {
      /* FIXME: Handle non-const iter steps, cf. PR fortran/110735.  */
      sorry_at (gfc_get_location (&curr_loop_var->where),
		"non-rectangular loop nest with non-constant step for %qs",
		curr_loop_var->symtree->n.sym->name);
      return false;
    }

  dovar_init *di;
  unsigned ix;
  FOR_EACH_VEC_ELT (*inits, ix, di)
    if (di->sym == var)
      {
	if (!di->non_unit_iter)
	  {
	    tree_var = di->init;
	    gcc_assert (DECL_P (tree_var));
	    break;
	  }
	else
	  {
	    /* FIXME: Handle non-const iter steps, cf. PR fortran/110735.  */
	    sorry_at (gfc_get_location (&code->loc),
		      "non-rectangular loop nest with non-constant step "
		      "for %qs", var->name);
	    inform (gfc_get_location (&expr->where), "Used here");
	    return false;
	  }
      }
  if (tree_var == NULL_TREE)
    tree_var = var->backend_decl;

  if (expr->expr_type == EXPR_VARIABLE)
    gcc_assert (expr->symtree->n.sym == var);
  else if (expr->expr_type != EXPR_OP
	   || (expr->value.op.op != INTRINSIC_TIMES
	       && expr->value.op.op != INTRINSIC_PLUS
	       && expr->value.op.op != INTRINSIC_MINUS))
    gcc_unreachable ();
  else
    {
      gfc_se se;
      gfc_expr *et = NULL, *eo = NULL, *e = expr;
      if (expr->value.op.op != INTRINSIC_TIMES)
	{
	  if (gfc_find_sym_in_expr (var, expr->value.op.op1))
	    {
	      e = expr->value.op.op1;
	      eo = expr->value.op.op2;
	    }
	  else
	    {
	      eo = expr->value.op.op1;
	      e = expr->value.op.op2;
	    }
	}
      if (e->value.op.op == INTRINSIC_TIMES)
	{
	  if (e->value.op.op1->expr_type == EXPR_VARIABLE
	      && e->value.op.op1->symtree->n.sym == var)
	    et = e->value.op.op2;
	  else
	    {
	      et = e->value.op.op1;
	      gcc_assert (e->value.op.op2->expr_type == EXPR_VARIABLE
			  && e->value.op.op2->symtree->n.sym == var);
	    }
	}
      else
	gcc_assert (e->expr_type == EXPR_VARIABLE && e->symtree->n.sym == var);
      if (et != NULL)
	{
	  gfc_init_se (&se, NULL);
	  gfc_conv_expr_val (&se, et);
	  gfc_add_block_to_block (pblock, &se.pre);
	  a1 = se.expr;
	}
      if (eo != NULL)
	{
	  gfc_init_se (&se, NULL);
	  gfc_conv_expr_val (&se, eo);
	  gfc_add_block_to_block (pblock, &se.pre);
	  a2 = se.expr;
	  if (expr->value.op.op == INTRINSIC_MINUS && expr->value.op.op2 == eo)
	    /* outer-var - a2.  */
	    a2 = fold_build1 (NEGATE_EXPR, TREE_TYPE (a2), a2);
	  else if (expr->value.op.op == INTRINSIC_MINUS)
	    /* a2 - outer-var.  */
	    a1 = fold_build1 (NEGATE_EXPR, TREE_TYPE (a1), a1);
	}
      a1 = DECL_P (a1) ? a1 : gfc_evaluate_now (a1, pblock);
      a2 = DECL_P (a2) ? a2 : gfc_evaluate_now (a2, pblock);
    }

  gfc_init_se (sep, NULL);
  sep->expr = make_tree_vec (3);
  TREE_VEC_ELT (sep->expr, 0) = tree_var;
  TREE_VEC_ELT (sep->expr, 1) = fold_convert (TREE_TYPE (tree_var), a1);
  TREE_VEC_ELT (sep->expr, 2) = fold_convert (TREE_TYPE (tree_var), a2);

  return true;
}

int
gfc_expr_list_len (gfc_expr_list *list)
{
  unsigned len = 0;
  for (; list; list = list->next)
    len++;

  return len;
}

static tree
gfc_trans_omp_do (gfc_code *code, gfc_exec_op op, stmtblock_t *pblock,
		  gfc_omp_clauses *do_clauses, tree par_clauses)
{
  gfc_se se;
  tree dovar, stmt, from, to, step, type, init, cond, incr, orig_decls;
  tree local_dovar = NULL_TREE, cycle_label, tmp, omp_clauses;
  stmtblock_t block;
  stmtblock_t body;
  gfc_omp_clauses *clauses = code->ext.omp_clauses;
  int i, collapse = clauses->collapse;
  vec<dovar_init> inits = vNULL;
  dovar_init *di;
  unsigned ix;
  vec<tree, va_heap, vl_embed> *saved_doacross_steps = doacross_steps;
  gfc_expr_list *oacc_tile
    = do_clauses ? do_clauses->tile_list : clauses->tile_list;
  gfc_expr_list *sizes
    = do_clauses ? do_clauses->sizes_list : clauses->sizes_list;
  gfc_code *orig_code = code;

  /* Both collapsed and tiled loops are lowered the same way.  In
     OpenACC, those clauses are not compatible, so prioritize the tile
     clause, if present.  */
  if (oacc_tile)
    collapse = gfc_expr_list_len (oacc_tile);
  else if (sizes)
    collapse = gfc_expr_list_len (sizes);

  doacross_steps = NULL;
  if (clauses->orderedc)
    collapse = clauses->orderedc;
  if (collapse <= 0)
    collapse = 1;

  code = code->block->next;

  init = make_tree_vec (collapse);
  cond = make_tree_vec (collapse);
  incr = make_tree_vec (collapse);
  orig_decls = clauses->ordered ? make_tree_vec (collapse) : NULL_TREE;

  if (pblock == NULL)
    {
      gfc_start_block (&block);
      pblock = &block;
    }

  /* simd schedule modifier is only useful for composite do simd and other
     constructs including that, where gfc_trans_omp_do is only called
     on the simd construct and DO's clauses are translated elsewhere.  */
  do_clauses->sched_simd = false;

  omp_clauses = gfc_trans_omp_clauses (pblock, do_clauses, code->loc);

  for (i = 0; i < collapse; i++)
    {
      int simple = 0;
      int dovar_found = 0;
      tree dovar_decl;

      if (code->op == EXEC_OMP_TILE || code->op == EXEC_OMP_UNROLL)
	{
	  TREE_VEC_ELT (init, i) = NULL_TREE;
	  TREE_VEC_ELT (cond, i) = NULL_TREE;
	  TREE_VEC_ELT (incr, i) = NULL_TREE;
	  TREE_VEC_ELT (incr, i) = NULL_TREE;
	  if (orig_decls)
	    TREE_VEC_ELT (orig_decls, i) = NULL_TREE;
	  continue;
	}
      gcc_assert (code->op == EXEC_DO);
      if (clauses)
	{
	  gfc_omp_namelist *n = NULL;
	  if (op == EXEC_OMP_SIMD && collapse == 1)
	    for (n = clauses->lists[OMP_LIST_LINEAR];
		 n != NULL; n = n->next)
	      if (code->ext.iterator->var->symtree->n.sym == n->sym)
		{
		  dovar_found = 3;
		  break;
		}
	  if (n == NULL && op != EXEC_OMP_DISTRIBUTE)
	    for (n = clauses->lists[OMP_LIST_LASTPRIVATE];
		 n != NULL; n = n->next)
	      if (code->ext.iterator->var->symtree->n.sym == n->sym)
		{
		  dovar_found = 2;
		  break;
		}
	  if (n == NULL)
	    for (n = clauses->lists[OMP_LIST_PRIVATE]; n != NULL; n = n->next)
	      if (code->ext.iterator->var->symtree->n.sym == n->sym)
		{
		  dovar_found = 1;
		  break;
		}
	}

      /* Evaluate all the expressions in the iterator.  */
      gfc_init_se (&se, NULL);
      gfc_conv_expr_lhs (&se, code->ext.iterator->var);
      gfc_add_block_to_block (pblock, &se.pre);
      local_dovar = dovar_decl = dovar = se.expr;
      type = TREE_TYPE (dovar);
      gcc_assert (TREE_CODE (type) == INTEGER_TYPE);

      gfc_init_se (&se, NULL);
      gfc_conv_expr_val (&se, code->ext.iterator->step);
      gfc_add_block_to_block (pblock, &se.pre);
      step = gfc_evaluate_now (se.expr, pblock);

      if (TREE_CODE (step) == INTEGER_CST)
	simple = tree_int_cst_sgn (step);

      gfc_init_se (&se, NULL);
      if (!clauses->non_rectangular
	  || !gfc_nonrect_loop_expr (pblock, &se, i, orig_code->block->next,
				     code->ext.iterator->start, &inits, simple,
				     code->ext.iterator->var))
	{
	  gfc_conv_expr_val (&se, code->ext.iterator->start);
	  gfc_add_block_to_block (pblock, &se.pre);
	  if (!DECL_P (se.expr))
	    se.expr = gfc_evaluate_now (se.expr, pblock);
	}
      from = se.expr;

      gfc_init_se (&se, NULL);
      if (!clauses->non_rectangular
	  || !gfc_nonrect_loop_expr (pblock, &se, i, orig_code->block->next,
				     code->ext.iterator->end, &inits, simple,
				     code->ext.iterator->var))
	{
	  gfc_conv_expr_val (&se, code->ext.iterator->end);
	  gfc_add_block_to_block (pblock, &se.pre);
	  if (!DECL_P (se.expr))
	    se.expr = gfc_evaluate_now (se.expr, pblock);
	}
      to = se.expr;

      if (!DECL_P (dovar))
	dovar_decl
	  = gfc_trans_omp_variable (code->ext.iterator->var->symtree->n.sym,
				    false);
      if (simple && !DECL_P (dovar))
	{
	  const char *name = code->ext.iterator->var->symtree->n.sym->name;
	  local_dovar = gfc_create_var (type, name);
	  dovar_init e = {code->ext.iterator->var->symtree->n.sym,
			  dovar, local_dovar, false};
	  inits.safe_push (e);
	}
      /* Loop body.  */
      if (simple)
	{
	  TREE_VEC_ELT (init, i) = build2_v (MODIFY_EXPR, local_dovar, from);
	  /* The condition should not be folded.  */
	  TREE_VEC_ELT (cond, i) = build2_loc (input_location, simple > 0
					       ? LE_EXPR : GE_EXPR,
					       logical_type_node, local_dovar,
					       to);
	  TREE_VEC_ELT (incr, i) = fold_build2_loc (input_location, PLUS_EXPR,
						    type, local_dovar, step);
	  TREE_VEC_ELT (incr, i) = fold_build2_loc (input_location,
						    MODIFY_EXPR,
						    type, local_dovar,
						    TREE_VEC_ELT (incr, i));
	  if (orig_decls && !clauses->orderedc)
	    orig_decls = NULL;
	  else if (orig_decls)
	    TREE_VEC_ELT (orig_decls, i) = dovar_decl;
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
	  tmp = fold_build2_loc (input_location, MINUS_EXPR, type, step, from);
	  tmp = fold_build2_loc (input_location, PLUS_EXPR, type, to, tmp);
	  tmp = fold_build2_loc (input_location, TRUNC_DIV_EXPR, type, tmp,
				 step);
	  tmp = gfc_evaluate_now (tmp, pblock);
	  local_dovar = gfc_create_var (type, "count");
	  TREE_VEC_ELT (init, i) = build2_v (MODIFY_EXPR, local_dovar,
					     build_int_cst (type, 0));
	  /* The condition should not be folded.  */
	  TREE_VEC_ELT (cond, i) = build2_loc (input_location, LT_EXPR,
					       logical_type_node,
					       local_dovar, tmp);
	  TREE_VEC_ELT (incr, i) = fold_build2_loc (input_location, PLUS_EXPR,
						    type, local_dovar,
						    build_int_cst (type, 1));
	  TREE_VEC_ELT (incr, i) = fold_build2_loc (input_location,
						    MODIFY_EXPR, type,
						    local_dovar,
						    TREE_VEC_ELT (incr, i));

	  /* Initialize DOVAR.  */
	  tmp = fold_build2_loc (input_location, MULT_EXPR, type, local_dovar,
				 step);
	  tmp = fold_build2_loc (input_location, PLUS_EXPR, type, from, tmp);
	  dovar_init e = {code->ext.iterator->var->symtree->n.sym,
			  dovar, tmp, true};
	  inits.safe_push (e);
	  if (clauses->orderedc)
	    {
	      if (doacross_steps == NULL)
		vec_safe_grow_cleared (doacross_steps, clauses->orderedc, true);
	      (*doacross_steps)[i] = step;
	    }
	  if (orig_decls)
	    TREE_VEC_ELT (orig_decls, i) = dovar_decl;
	}

      if (dovar_found == 3
	  && op == EXEC_OMP_SIMD
	  && collapse == 1
	  && local_dovar != dovar)
	{
	  for (tmp = omp_clauses; tmp; tmp = OMP_CLAUSE_CHAIN (tmp))
	    if (OMP_CLAUSE_CODE (tmp) == OMP_CLAUSE_LINEAR
		&& OMP_CLAUSE_DECL (tmp) == dovar)
	      {
		OMP_CLAUSE_LINEAR_NO_COPYIN (tmp) = 1;
		break;
	      }
	}
      if (!dovar_found && op == EXEC_OMP_SIMD)
	{
	  if (collapse == 1)
	    {
	      tmp = build_omp_clause (input_location, OMP_CLAUSE_LINEAR);
	      OMP_CLAUSE_LINEAR_STEP (tmp) = step;
	      OMP_CLAUSE_LINEAR_NO_COPYIN (tmp) = 1;
	      OMP_CLAUSE_DECL (tmp) = dovar_decl;
	      omp_clauses = gfc_trans_add_clause (tmp, omp_clauses);
	      if (local_dovar != dovar)
		dovar_found = 3;
	    }
	}
      else if (!dovar_found && local_dovar != dovar)
	{
	  tmp = build_omp_clause (input_location, OMP_CLAUSE_PRIVATE);
	  OMP_CLAUSE_DECL (tmp) = dovar_decl;
	  omp_clauses = gfc_trans_add_clause (tmp, omp_clauses);
	}
      if (dovar_found > 1)
	{
	  tree c = NULL;

	  tmp = NULL;
	  if (local_dovar != dovar)
	    {
	      /* If dovar is lastprivate, but different counter is used,
		 dovar += step needs to be added to
		 OMP_CLAUSE_LASTPRIVATE_STMT, otherwise the copied dovar
		 will have the value on entry of the last loop, rather
		 than value after iterator increment.  */
	      if (clauses->orderedc)
		{
		  if (clauses->collapse <= 1 || i >= clauses->collapse)
		    tmp = local_dovar;
		  else
		    tmp = fold_build2_loc (input_location, PLUS_EXPR,
					   type, local_dovar,
					   build_one_cst (type));
		  tmp = fold_build2_loc (input_location, MULT_EXPR, type,
					 tmp, step);
		  tmp = fold_build2_loc (input_location, PLUS_EXPR, type,
					 from, tmp);
		}
	      else
		tmp = fold_build2_loc (input_location, PLUS_EXPR, type,
				       dovar, step);
	      tmp = fold_build2_loc (input_location, MODIFY_EXPR, type,
				     dovar, tmp);
	      for (c = omp_clauses; c ; c = OMP_CLAUSE_CHAIN (c))
		if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LASTPRIVATE
		    && OMP_CLAUSE_DECL (c) == dovar_decl)
		  {
		    OMP_CLAUSE_LASTPRIVATE_STMT (c) = tmp;
		    break;
		  }
		else if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LINEAR
			 && OMP_CLAUSE_DECL (c) == dovar_decl)
		  {
		    OMP_CLAUSE_LINEAR_STMT (c) = tmp;
		    break;
		  }
	    }
	  if (c == NULL && op == EXEC_OMP_DO && par_clauses != NULL)
	    {
	      for (c = par_clauses; c ; c = OMP_CLAUSE_CHAIN (c))
		if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_LASTPRIVATE
		    && OMP_CLAUSE_DECL (c) == dovar_decl)
		  {
		    tree l = build_omp_clause (input_location,
					       OMP_CLAUSE_LASTPRIVATE);
		    if (OMP_CLAUSE_LASTPRIVATE_CONDITIONAL (c))
		      OMP_CLAUSE_LASTPRIVATE_CONDITIONAL (l) = 1;
		    OMP_CLAUSE_DECL (l) = dovar_decl;
		    OMP_CLAUSE_CHAIN (l) = omp_clauses;
		    OMP_CLAUSE_LASTPRIVATE_STMT (l) = tmp;
		    omp_clauses = l;
		    OMP_CLAUSE_SET_CODE (c, OMP_CLAUSE_SHARED);
		    break;
		  }
	    }
	  gcc_assert (local_dovar == dovar || c != NULL);
	}
      if (local_dovar != dovar)
	{
	  if (op != EXEC_OMP_SIMD || dovar_found == 1)
	    tmp = build_omp_clause (input_location, OMP_CLAUSE_PRIVATE);
	  else if (collapse == 1)
	    {
	      tmp = build_omp_clause (input_location, OMP_CLAUSE_LINEAR);
	      OMP_CLAUSE_LINEAR_STEP (tmp) = build_int_cst (type, 1);
	      OMP_CLAUSE_LINEAR_NO_COPYIN (tmp) = 1;
	      OMP_CLAUSE_LINEAR_NO_COPYOUT (tmp) = 1;
	    }
	  else
	    tmp = build_omp_clause (input_location, OMP_CLAUSE_LASTPRIVATE);
	  OMP_CLAUSE_DECL (tmp) = local_dovar;
	  omp_clauses = gfc_trans_add_clause (tmp, omp_clauses);
	}

      if (i + 1 < collapse)
	code = code->block->next;
    }

  if (pblock != &block)
    {
      pushlevel ();
      gfc_start_block (&block);
    }

  gfc_start_block (&body);

  FOR_EACH_VEC_ELT (inits, ix, di)
    gfc_add_modify (&body, di->var, di->init);
  inits.release ();

  /* Cycle statement is implemented with a goto.  Exit statement must not be
     present for this loop.  */
  cycle_label = gfc_build_label_decl (NULL_TREE);

  /* Put these labels where they can be found later.  */

  code->cycle_label = cycle_label;
  code->exit_label = NULL_TREE;

  /* Main loop body.  */
  if (clauses->lists[OMP_LIST_REDUCTION_INSCAN])
    {
      gfc_code *code1, *scan, *code2, *tmpcode;
      code1 = tmpcode = code->block->next;
      if (tmpcode && tmpcode->op != EXEC_OMP_SCAN)
	while (tmpcode && tmpcode->next && tmpcode->next->op != EXEC_OMP_SCAN)
	  tmpcode = tmpcode->next;
      scan = tmpcode->op == EXEC_OMP_SCAN ? tmpcode : tmpcode->next;
      if (code1 != scan)
	tmpcode->next = NULL;
      code2 = scan->next;
      gcc_assert (scan->op == EXEC_OMP_SCAN);
      location_t loc = gfc_get_location (&scan->loc);

      tmp = code1 != scan ? gfc_trans_code (code1) : build_empty_stmt (loc);
      tmp = build2 (OMP_SCAN, void_type_node, tmp, NULL_TREE);
      SET_EXPR_LOCATION (tmp, loc);
      gfc_add_expr_to_block (&body, tmp);
      input_location = loc;
      tree c = gfc_trans_omp_clauses (&body, scan->ext.omp_clauses, scan->loc);
      tmp = code2 ? gfc_trans_code (code2) : build_empty_stmt (loc);
      tmp = build2 (OMP_SCAN, void_type_node, tmp, c);
      SET_EXPR_LOCATION (tmp, loc);
      if (code1 != scan)
	tmpcode->next = scan;
    }
  else if (code->op == EXEC_OMP_TILE || code->op == EXEC_OMP_UNROLL)
    tmp = gfc_trans_omp_code (code, true);
  else
    tmp = gfc_trans_omp_code (code->block->next, true);
  gfc_add_expr_to_block (&body, tmp);

  /* Label for cycle statements (if needed).  */
  if (TREE_USED (cycle_label))
    {
      tmp = build1_v (LABEL_EXPR, cycle_label);
      gfc_add_expr_to_block (&body, tmp);
    }

  /* End of loop body.  */
  switch (op)
    {
    case EXEC_OMP_SIMD: stmt = make_node (OMP_SIMD); break;
    case EXEC_OMP_DO: stmt = make_node (OMP_FOR); break;
    case EXEC_OMP_DISTRIBUTE: stmt = make_node (OMP_DISTRIBUTE); break;
    case EXEC_OMP_LOOP: stmt = make_node (OMP_LOOP); break;
    case EXEC_OMP_TASKLOOP: stmt = make_node (OMP_TASKLOOP); break;
    case EXEC_OACC_LOOP: stmt = make_node (OACC_LOOP); break;
    case EXEC_OMP_TILE: stmt = make_node (OMP_TILE); break;
    case EXEC_OMP_UNROLL: stmt = make_node (OMP_UNROLL); break;
    default: gcc_unreachable ();
    }

  SET_EXPR_LOCATION (stmt, gfc_get_location (&orig_code->loc));
  TREE_TYPE (stmt) = void_type_node;
  OMP_FOR_BODY (stmt) = gfc_finish_block (&body);
  OMP_FOR_CLAUSES (stmt) = omp_clauses;
  OMP_FOR_INIT (stmt) = init;
  OMP_FOR_COND (stmt) = cond;
  OMP_FOR_INCR (stmt) = incr;
  if (orig_decls)
    OMP_FOR_ORIG_DECLS (stmt) = orig_decls;
  OMP_FOR_NON_RECTANGULAR (stmt) = clauses->non_rectangular;
  gfc_add_expr_to_block (&block, stmt);

  vec_free (doacross_steps);
  doacross_steps = saved_doacross_steps;

  return gfc_finish_block (&block);
}

/* Translate combined OpenACC 'parallel loop', 'kernels loop', 'serial loop'
   construct. */

static tree
gfc_trans_oacc_combined_directive (gfc_code *code)
{
  stmtblock_t block, *pblock = NULL;
  gfc_omp_clauses construct_clauses, loop_clauses;
  tree stmt, oacc_clauses = NULL_TREE;
  enum tree_code construct_code;
  location_t loc = input_location;

  switch (code->op)
    {
      case EXEC_OACC_PARALLEL_LOOP:
	construct_code = OACC_PARALLEL;
	break;
      case EXEC_OACC_KERNELS_LOOP:
	construct_code = OACC_KERNELS;
	break;
      case EXEC_OACC_SERIAL_LOOP:
	construct_code = OACC_SERIAL;
	break;
      default:
	gcc_unreachable ();
    }

  gfc_start_block (&block);

  memset (&loop_clauses, 0, sizeof (loop_clauses));
  if (code->ext.omp_clauses != NULL)
    {
      memcpy (&construct_clauses, code->ext.omp_clauses,
	      sizeof (construct_clauses));
      loop_clauses.collapse = construct_clauses.collapse;
      loop_clauses.gang = construct_clauses.gang;
      loop_clauses.gang_static = construct_clauses.gang_static;
      loop_clauses.gang_num_expr = construct_clauses.gang_num_expr;
      loop_clauses.gang_static_expr = construct_clauses.gang_static_expr;
      loop_clauses.vector = construct_clauses.vector;
      loop_clauses.vector_expr = construct_clauses.vector_expr;
      loop_clauses.worker = construct_clauses.worker;
      loop_clauses.worker_expr = construct_clauses.worker_expr;
      loop_clauses.seq = construct_clauses.seq;
      loop_clauses.par_auto = construct_clauses.par_auto;
      loop_clauses.independent = construct_clauses.independent;
      loop_clauses.tile_list = construct_clauses.tile_list;
      loop_clauses.lists[OMP_LIST_PRIVATE]
	= construct_clauses.lists[OMP_LIST_PRIVATE];
      loop_clauses.lists[OMP_LIST_REDUCTION]
	= construct_clauses.lists[OMP_LIST_REDUCTION];
      construct_clauses.gang = false;
      construct_clauses.gang_static = false;
      construct_clauses.gang_num_expr = NULL;
      construct_clauses.gang_static_expr = NULL;
      construct_clauses.vector = false;
      construct_clauses.vector_expr = NULL;
      construct_clauses.worker = false;
      construct_clauses.worker_expr = NULL;
      construct_clauses.seq = false;
      construct_clauses.par_auto = false;
      construct_clauses.independent = false;
      construct_clauses.independent = false;
      construct_clauses.tile_list = NULL;
      construct_clauses.lists[OMP_LIST_PRIVATE] = NULL;
      if (construct_code == OACC_KERNELS)
	construct_clauses.lists[OMP_LIST_REDUCTION] = NULL;
      oacc_clauses = gfc_trans_omp_clauses (&block, &construct_clauses,
					    code->loc, false, true);
    }
  if (!loop_clauses.seq)
    pblock = &block;
  else
    pushlevel ();
  stmt = gfc_trans_omp_do (code, EXEC_OACC_LOOP, pblock, &loop_clauses, NULL);
  protected_set_expr_location (stmt, loc);
  if (TREE_CODE (stmt) != BIND_EXPR)
    stmt = build3_v (BIND_EXPR, NULL, stmt, poplevel (1, 0));
  else
    poplevel (0, 0);
  stmt = build2_loc (loc, construct_code, void_type_node, stmt, oacc_clauses);
  gfc_add_expr_to_block (&block, stmt);
  return gfc_finish_block (&block);
}

static tree
gfc_trans_omp_depobj (gfc_code *code)
{
  stmtblock_t block;
  gfc_se se;
  gfc_init_se (&se, NULL);
  gfc_init_block (&block);
  gfc_conv_expr (&se, code->ext.omp_clauses->depobj);
  gcc_assert (se.pre.head == NULL && se.post.head == NULL);
  tree depobj = se.expr;
  location_t loc = EXPR_LOCATION (depobj);
  if (!POINTER_TYPE_P (TREE_TYPE (depobj)))
    depobj = gfc_build_addr_expr (NULL, depobj);
  depobj = fold_convert (build_pointer_type_for_mode (ptr_type_node,
						      TYPE_MODE (ptr_type_node),
						      true), depobj);
  gfc_omp_namelist *n = code->ext.omp_clauses->lists[OMP_LIST_DEPEND];
  if (n)
    {
      tree var;
      if (!n->sym)  /* omp_all_memory.  */
	var = null_pointer_node;
      else if (n->expr && n->expr->ref->u.ar.type != AR_FULL)
	{
	  gfc_init_se (&se, NULL);
	  if (n->expr->ref->u.ar.type == AR_ELEMENT)
	    {
	      gfc_conv_expr_reference (&se, n->expr);
	      var = se.expr;
	    }
	  else
	    {
	      gfc_conv_expr_descriptor (&se, n->expr);
	      var = gfc_conv_array_data (se.expr);
	    }
	  gfc_add_block_to_block (&block, &se.pre);
	  gfc_add_block_to_block (&block, &se.post);
	  gcc_assert (POINTER_TYPE_P (TREE_TYPE (var)));
	}
      else
	{
	  var = gfc_get_symbol_decl (n->sym);
	  if (POINTER_TYPE_P (TREE_TYPE (var))
	      && GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (TREE_TYPE (var))))
	    var = build_fold_indirect_ref (var);
	  if (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (var)))
	    {
	      var = gfc_conv_descriptor_data_get (var);
	      gcc_assert (POINTER_TYPE_P (TREE_TYPE (var)));
	    }
	  else if ((n->sym->attr.allocatable || n->sym->attr.pointer)
		   && n->sym->attr.dummy)
	    var = build_fold_indirect_ref (var);
	  else if (!POINTER_TYPE_P (TREE_TYPE (var))
		   || (n->sym->ts.f90_type == BT_VOID
		       && !POINTER_TYPE_P (TREE_TYPE (TREE_TYPE (var)))
		       && !GFC_ARRAY_TYPE_P (TREE_TYPE (TREE_TYPE (var)))))
	    {
	      TREE_ADDRESSABLE (var) = 1;
	      var = gfc_build_addr_expr (NULL, var);
	    }
	}
      depobj = save_expr (depobj);
      tree r = build_fold_indirect_ref_loc (loc, depobj);
      gfc_add_expr_to_block (&block,
			     build2 (MODIFY_EXPR, void_type_node, r, var));
    }

  /* Only one may be set. */
  gcc_assert (((int)(n != NULL) + (int)(code->ext.omp_clauses->destroy)
	      + (int)(code->ext.omp_clauses->depobj_update != OMP_DEPEND_UNSET))
	      == 1);
  int k = -1; /* omp_clauses->destroy */
  if (!code->ext.omp_clauses->destroy)
    switch (code->ext.omp_clauses->depobj_update != OMP_DEPEND_UNSET
	    ? code->ext.omp_clauses->depobj_update : n->u.depend_doacross_op)
      {
      case OMP_DEPEND_IN: k = GOMP_DEPEND_IN; break;
      case OMP_DEPEND_OUT: k = GOMP_DEPEND_OUT; break;
      case OMP_DEPEND_INOUT: k = GOMP_DEPEND_INOUT; break;
      case OMP_DEPEND_INOUTSET: k = GOMP_DEPEND_INOUTSET; break;
      case OMP_DEPEND_MUTEXINOUTSET: k = GOMP_DEPEND_MUTEXINOUTSET; break;
      default: gcc_unreachable ();
      }
  tree t = build_int_cst (ptr_type_node, k);
  depobj = build2_loc (loc, POINTER_PLUS_EXPR, TREE_TYPE (depobj), depobj,
                       TYPE_SIZE_UNIT (ptr_type_node));
  depobj = build_fold_indirect_ref_loc (loc, depobj);
  gfc_add_expr_to_block (&block, build2 (MODIFY_EXPR, void_type_node, depobj, t));

  return gfc_finish_block (&block);
}

/* Callback for walk_tree to find an OMP dispatch call and wrap it into an
 * IFN_GOMP_DISPATCH.  */

static tree
replace_omp_dispatch_call (tree *tp, int *, void *decls_p)
{
  tree t = *tp;
  tree decls = (tree) decls_p;
  tree orig_fn_decl = TREE_PURPOSE (decls);
  tree dup_fn_decl = TREE_VALUE (decls);
  if (TREE_CODE (t) == CALL_EXPR)
    {
      if (CALL_EXPR_FN (t) == dup_fn_decl)
	CALL_EXPR_FN (t) = orig_fn_decl;
      else if (TREE_CODE (CALL_EXPR_FN (t)) == ADDR_EXPR
	       && TREE_OPERAND (CALL_EXPR_FN (t), 0) == dup_fn_decl)
	TREE_OPERAND (CALL_EXPR_FN (t), 0) = dup_fn_decl;
      else
	return NULL_TREE;
      *tp = build_call_expr_internal_loc (input_location, IFN_GOMP_DISPATCH,
					  TREE_TYPE (t), 1, t);
      return *tp;
    }

  return NULL_TREE;
}

static tree
gfc_trans_omp_dispatch (gfc_code *code)
{
  stmtblock_t block;
  gfc_code *next = code->block->next;
  // assume ill-formed "function dispatch structured
  // block" have already been rejected by resolve_omp_dispatch
  gcc_assert (next->op == EXEC_CALL || next->op == EXEC_ASSIGN);

  // Make duplicate decl for dispatch function call to make it easy to spot
  // after translation
  gfc_symbol *orig_fn_sym;
  gfc_expr *call_expr = next->op == EXEC_CALL ? next->expr1 : next->expr2;
  if (call_expr != NULL) // function
    {
      if (call_expr->value.function.isym != NULL) // dig into convert intrinsics
	call_expr = call_expr->value.function.actual->expr;
      gcc_assert (call_expr->expr_type == EXPR_FUNCTION);
      orig_fn_sym = call_expr->value.function.esym
		      ? call_expr->value.function.esym
		      : call_expr->symtree->n.sym;
    }
  else // subroutine
    {
      orig_fn_sym = next->resolved_sym;
    }
  if (!orig_fn_sym->backend_decl)
    gfc_get_symbol_decl (orig_fn_sym);
  gfc_symbol dup_fn_sym = *orig_fn_sym;
  dup_fn_sym.backend_decl = copy_node (orig_fn_sym->backend_decl);
  if (call_expr != NULL)
    call_expr->value.function.esym = &dup_fn_sym;
  else
    next->resolved_sym = &dup_fn_sym;

  tree body = gfc_trans_code (next);

  // Walk the tree to find the duplicate decl, wrap IFN call and replace
  // dup decl with original
  tree fn_decls
    = build_tree_list (orig_fn_sym->backend_decl, dup_fn_sym.backend_decl);
  tree dispatch_call
    = walk_tree (&body, replace_omp_dispatch_call, fn_decls, NULL);
  gcc_assert (dispatch_call != NULL_TREE);

  gfc_start_block (&block);
  tree omp_clauses
    = gfc_trans_omp_clauses (&block, code->ext.omp_clauses, code->loc);

  // Extract depend clauses and create taskwait
  tree depend_clauses = NULL_TREE;
  tree *depend_clauses_ptr = &depend_clauses;
  for (tree c = omp_clauses; c; c = OMP_CLAUSE_CHAIN (c))
    {
      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_DEPEND)
	{
	  *depend_clauses_ptr = c;
	  depend_clauses_ptr = &OMP_CLAUSE_CHAIN (c);
	}
    }
  if (depend_clauses != NULL_TREE)
    {
      tree stmt = make_node (OMP_TASK);
      TREE_TYPE (stmt) = void_node;
      OMP_TASK_CLAUSES (stmt) = depend_clauses;
      OMP_TASK_BODY (stmt) = NULL_TREE;
      SET_EXPR_LOCATION (stmt, gfc_get_location (&code->loc));
      gfc_add_expr_to_block (&block, stmt);
    }

  tree stmt = make_node (OMP_DISPATCH);
  SET_EXPR_LOCATION (stmt, gfc_get_location (&code->loc));
  TREE_TYPE (stmt) = void_type_node;
  OMP_DISPATCH_BODY (stmt) = body;
  OMP_DISPATCH_CLAUSES (stmt) = omp_clauses;

  gfc_add_expr_to_block (&block, stmt);
  return gfc_finish_block (&block);
}

static tree
gfc_trans_omp_error (gfc_code *code)
{
  stmtblock_t block;
  gfc_se se;
  tree len, message;
  bool fatal = code->ext.omp_clauses->severity == OMP_SEVERITY_FATAL;
  tree fndecl = builtin_decl_explicit (fatal ? BUILT_IN_GOMP_ERROR
					     : BUILT_IN_GOMP_WARNING);
  gfc_start_block (&block);
  gfc_init_se (&se, NULL );
  if (!code->ext.omp_clauses->message)
    {
      message = null_pointer_node;
      len = build_int_cst (size_type_node, 0);
    }
  else
    {
      gfc_conv_expr (&se, code->ext.omp_clauses->message);
      message = se.expr;
      if (!POINTER_TYPE_P (TREE_TYPE (message)))
	/* To ensure an ARRAY_TYPE is not passed as such.  */
	message = gfc_build_addr_expr (NULL, message);
      len = se.string_length;
    }
  gfc_add_block_to_block (&block, &se.pre);
  gfc_add_expr_to_block (&block, build_call_expr_loc (input_location, fndecl,
						      2, message, len));
  gfc_add_block_to_block (&block, &se.post);
  return gfc_finish_block (&block);
}

static tree
gfc_trans_omp_flush (gfc_code *code)
{
  tree call;
  if (!code->ext.omp_clauses
      || code->ext.omp_clauses->memorder == OMP_MEMORDER_UNSET
      || code->ext.omp_clauses->memorder == OMP_MEMORDER_SEQ_CST)
    {
      call = builtin_decl_explicit (BUILT_IN_SYNC_SYNCHRONIZE);
      call = build_call_expr_loc (input_location, call, 0);
    }
  else
    {
      enum memmodel mo = MEMMODEL_LAST;
      switch (code->ext.omp_clauses->memorder)
	{
	case OMP_MEMORDER_ACQ_REL: mo = MEMMODEL_ACQ_REL; break;
	case OMP_MEMORDER_RELEASE: mo = MEMMODEL_RELEASE; break;
	case OMP_MEMORDER_ACQUIRE: mo = MEMMODEL_ACQUIRE; break;
	default: gcc_unreachable (); break;
	}
      call = builtin_decl_explicit (BUILT_IN_ATOMIC_THREAD_FENCE);
      call = build_call_expr_loc (input_location, call, 1,
				  build_int_cst (integer_type_node, mo));
    }
  return call;
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
gfc_trans_omp_masked (gfc_code *code, gfc_omp_clauses *clauses)
{
  stmtblock_t block;
  tree body = gfc_trans_code (code->block->next);
  if (IS_EMPTY_STMT (body))
    return body;
  if (!clauses)
    clauses = code->ext.omp_clauses;
  gfc_start_block (&block);
  tree omp_clauses = gfc_trans_omp_clauses (&block, clauses, code->loc);
  tree stmt = make_node (OMP_MASKED);
  SET_EXPR_LOCATION (stmt, gfc_get_location (&code->loc));
  TREE_TYPE (stmt) = void_type_node;
  OMP_MASKED_BODY (stmt) = body;
  OMP_MASKED_CLAUSES (stmt) = omp_clauses;
  gfc_add_expr_to_block (&block, stmt);
  return gfc_finish_block (&block);
}


static tree
gfc_trans_omp_ordered (gfc_code *code)
{
  if (!flag_openmp)
    {
      if (!code->ext.omp_clauses->simd)
	return gfc_trans_code (code->block ? code->block->next : NULL);
      code->ext.omp_clauses->threads = 0;
    }
  tree omp_clauses = gfc_trans_omp_clauses (NULL, code->ext.omp_clauses,
					    code->loc);
  return build2_loc (input_location, OMP_ORDERED, void_type_node,
		     code->block ? gfc_trans_code (code->block->next)
		     : NULL_TREE, omp_clauses);
}

static tree
gfc_trans_omp_parallel (gfc_code *code)
{
  stmtblock_t block;
  tree stmt, omp_clauses;

  gfc_start_block (&block);
  omp_clauses = gfc_trans_omp_clauses (&block, code->ext.omp_clauses,
				       code->loc);
  pushlevel ();
  stmt = gfc_trans_omp_code (code->block->next, true);
  stmt = build3_v (BIND_EXPR, NULL, stmt, poplevel (1, 0));
  stmt = build2_loc (input_location, OMP_PARALLEL, void_type_node, stmt,
		     omp_clauses);
  gfc_add_expr_to_block (&block, stmt);
  return gfc_finish_block (&block);
}

enum
{
  GFC_OMP_SPLIT_SIMD,
  GFC_OMP_SPLIT_DO,
  GFC_OMP_SPLIT_PARALLEL,
  GFC_OMP_SPLIT_DISTRIBUTE,
  GFC_OMP_SPLIT_TEAMS,
  GFC_OMP_SPLIT_TARGET,
  GFC_OMP_SPLIT_TASKLOOP,
  GFC_OMP_SPLIT_MASKED,
  GFC_OMP_SPLIT_NUM
};

enum
{
  GFC_OMP_MASK_SIMD = (1 << GFC_OMP_SPLIT_SIMD),
  GFC_OMP_MASK_DO = (1 << GFC_OMP_SPLIT_DO),
  GFC_OMP_MASK_PARALLEL = (1 << GFC_OMP_SPLIT_PARALLEL),
  GFC_OMP_MASK_DISTRIBUTE = (1 << GFC_OMP_SPLIT_DISTRIBUTE),
  GFC_OMP_MASK_TEAMS = (1 << GFC_OMP_SPLIT_TEAMS),
  GFC_OMP_MASK_TARGET = (1 << GFC_OMP_SPLIT_TARGET),
  GFC_OMP_MASK_TASKLOOP = (1 << GFC_OMP_SPLIT_TASKLOOP),
  GFC_OMP_MASK_MASKED = (1 << GFC_OMP_SPLIT_MASKED)
};

/* If a var is in lastprivate/firstprivate/reduction but not in a
   data mapping/sharing clause, add it to 'map(tofrom:' if is_target
   and to 'shared' otherwise.  */
static void
gfc_add_clause_implicitly (gfc_omp_clauses *clauses_out,
			   gfc_omp_clauses *clauses_in,
			   bool is_target, bool is_parallel_do)
{
  int clauselist_to_add = is_target ? OMP_LIST_MAP : OMP_LIST_SHARED;
  gfc_omp_namelist *tail = NULL;
  for (int i = 0; i < 5; ++i)
    {
      gfc_omp_namelist *n;
      switch (i)
	{
	case 0: n = clauses_in->lists[OMP_LIST_FIRSTPRIVATE]; break;
	case 1: n = clauses_in->lists[OMP_LIST_LASTPRIVATE]; break;
	case 2: n = clauses_in->lists[OMP_LIST_REDUCTION]; break;
	case 3: n = clauses_in->lists[OMP_LIST_REDUCTION_INSCAN]; break;
	case 4: n = clauses_in->lists[OMP_LIST_REDUCTION_TASK]; break;
	default: gcc_unreachable ();
	}
      for (; n != NULL; n = n->next)
	{
	  gfc_omp_namelist *n2, **n_firstp = NULL, **n_lastp = NULL;
	  for (int j = 0; j < 6; ++j)
	    {
	      gfc_omp_namelist **n2ref = NULL, *prev2 = NULL;
	      switch (j)
		{
		case 0:
		  n2ref = &clauses_out->lists[clauselist_to_add];
		  break;
		case 1:
		  n2ref = &clauses_out->lists[OMP_LIST_FIRSTPRIVATE];
		  break;
		case 2:
		  if (is_target)
		    n2ref = &clauses_in->lists[OMP_LIST_LASTPRIVATE];
		  else
		    n2ref = &clauses_out->lists[OMP_LIST_LASTPRIVATE];
		  break;
		case 3: n2ref = &clauses_out->lists[OMP_LIST_REDUCTION]; break;
		case 4:
		  n2ref = &clauses_out->lists[OMP_LIST_REDUCTION_INSCAN];
		  break;
		case 5:
		  n2ref = &clauses_out->lists[OMP_LIST_REDUCTION_TASK];
		  break;
		default: gcc_unreachable ();
		}
	      for (n2 = *n2ref; n2 != NULL; prev2 = n2, n2 = n2->next)
		if (n2->sym == n->sym)
		  break;
	      if (n2)
		{
		  if (j == 0 /* clauselist_to_add */)
		    break;  /* Already present.  */
		  if (j == 1 /* OMP_LIST_FIRSTPRIVATE */)
		    {
		      n_firstp = prev2 ? &prev2->next : n2ref;
		      continue;
		    }
		  if (j == 2 /* OMP_LIST_LASTPRIVATE */)
		    {
		      n_lastp = prev2 ? &prev2->next : n2ref;
		      continue;
		    }
		  break;
		}
	    }
	  if (n_firstp && n_lastp)
	    {
	      /* For parallel do, GCC puts firstprivate/lastprivate
		 on the parallel.  */
	      if (is_parallel_do)
		continue;
	      *n_firstp = (*n_firstp)->next;
	      if (!is_target)
		*n_lastp = (*n_lastp)->next;
	    }
	  else if (is_target && n_lastp)
	    ;
	  else if (n2 || n_firstp || n_lastp)
	    continue;
	  if (clauses_out->lists[clauselist_to_add]
	      && (clauses_out->lists[clauselist_to_add]
		  == clauses_in->lists[clauselist_to_add]))
	    {
	      gfc_omp_namelist *p = NULL;
	      for (n2 = clauses_in->lists[clauselist_to_add]; n2; n2 = n2->next)
		{
		  if (p)
		    {
		      p->next = gfc_get_omp_namelist ();
		      p = p->next;
		    }
		  else
		    {
		      p = gfc_get_omp_namelist ();
		      clauses_out->lists[clauselist_to_add] = p;
		    }
		  *p = *n2;
		}
	    }
	  if (!tail)
	    {
	      tail = clauses_out->lists[clauselist_to_add];
	      for (; tail && tail->next; tail = tail->next)
		;
	    }
	  n2 = gfc_get_omp_namelist ();
	  n2->where = n->where;
	  n2->sym = n->sym;
	  if (is_target)
	    n2->u.map.op = OMP_MAP_TOFROM;
	  if (tail)
	    {
	      tail->next = n2;
	      tail = n2;
	    }
	  else
	    clauses_out->lists[clauselist_to_add] = n2;
	}
    }
}

/* Kind of opposite to above, add firstprivate to CLAUSES_OUT if it is mapped
   in CLAUSES_IN's FIRSTPRIVATE list but not its MAP list.  */

static void
gfc_add_firstprivate_if_unmapped (gfc_omp_clauses *clauses_out,
				  gfc_omp_clauses *clauses_in)
{
  gfc_omp_namelist *n = clauses_in->lists[OMP_LIST_FIRSTPRIVATE];
  gfc_omp_namelist **tail = NULL;

  for (; n != NULL; n = n->next)
    {
      gfc_omp_namelist *n2 = clauses_out->lists[OMP_LIST_MAP];
      for (; n2 != NULL; n2 = n2->next)
	if (n->sym == n2->sym)
	  break;
      if (n2 == NULL)
	{
	  gfc_omp_namelist *dup = gfc_get_omp_namelist ();
	  *dup = *n;
	  dup->next = NULL;
	  if (!tail)
	    {
	      tail = &clauses_out->lists[OMP_LIST_FIRSTPRIVATE];
	      while (*tail && (*tail)->next)
		tail = &(*tail)->next;
	    }
	  *tail = dup;
	  tail = &(*tail)->next;
	}
    }
}

static void
gfc_free_split_omp_clauses (gfc_code *code, gfc_omp_clauses *clausesa)
{
  for (int i = 0; i < GFC_OMP_SPLIT_NUM; ++i)
    for (int j = 0; j < OMP_LIST_NUM; ++j)
      if (clausesa[i].lists[j] && clausesa[i].lists[j] != code->ext.omp_clauses->lists[j])
	for (gfc_omp_namelist *n = clausesa[i].lists[j]; n;)
	  {
	    gfc_omp_namelist *p = n;
	    n = n->next;
	    free (p);
	  }
}

static void
gfc_split_omp_clauses (gfc_code *code,
		       gfc_omp_clauses clausesa[GFC_OMP_SPLIT_NUM])
{
  int mask = 0, innermost = 0;
  bool is_loop = false;
  memset (clausesa, 0, GFC_OMP_SPLIT_NUM * sizeof (gfc_omp_clauses));
  switch (code->op)
    {
    case EXEC_OMP_DISTRIBUTE:
      innermost = GFC_OMP_SPLIT_DISTRIBUTE;
      break;
    case EXEC_OMP_DISTRIBUTE_PARALLEL_DO:
      mask = GFC_OMP_MASK_DISTRIBUTE | GFC_OMP_MASK_PARALLEL | GFC_OMP_MASK_DO;
      innermost = GFC_OMP_SPLIT_DO;
      break;
    case EXEC_OMP_DISTRIBUTE_PARALLEL_DO_SIMD:
      mask = GFC_OMP_MASK_DISTRIBUTE | GFC_OMP_MASK_PARALLEL
	     | GFC_OMP_MASK_DO | GFC_OMP_MASK_SIMD;
      innermost = GFC_OMP_SPLIT_SIMD;
      break;
    case EXEC_OMP_DISTRIBUTE_SIMD:
      mask = GFC_OMP_MASK_DISTRIBUTE | GFC_OMP_MASK_SIMD;
      innermost = GFC_OMP_SPLIT_SIMD;
      break;
    case EXEC_OMP_DO:
    case EXEC_OMP_LOOP:
      innermost = GFC_OMP_SPLIT_DO;
      break;
    case EXEC_OMP_DO_SIMD:
      mask = GFC_OMP_MASK_DO | GFC_OMP_MASK_SIMD;
      innermost = GFC_OMP_SPLIT_SIMD;
      break;
    case EXEC_OMP_PARALLEL:
      innermost = GFC_OMP_SPLIT_PARALLEL;
      break;
    case EXEC_OMP_PARALLEL_DO:
    case EXEC_OMP_PARALLEL_LOOP:
      mask = GFC_OMP_MASK_PARALLEL | GFC_OMP_MASK_DO;
      innermost = GFC_OMP_SPLIT_DO;
      break;
    case EXEC_OMP_PARALLEL_DO_SIMD:
      mask = GFC_OMP_MASK_PARALLEL | GFC_OMP_MASK_DO | GFC_OMP_MASK_SIMD;
      innermost = GFC_OMP_SPLIT_SIMD;
      break;
    case EXEC_OMP_PARALLEL_MASKED:
      mask = GFC_OMP_MASK_PARALLEL | GFC_OMP_MASK_MASKED;
      innermost = GFC_OMP_SPLIT_MASKED;
      break;
    case EXEC_OMP_PARALLEL_MASKED_TASKLOOP:
      mask = (GFC_OMP_MASK_PARALLEL | GFC_OMP_MASK_MASKED
	      | GFC_OMP_MASK_TASKLOOP | GFC_OMP_MASK_SIMD);
      innermost = GFC_OMP_SPLIT_TASKLOOP;
      break;
    case EXEC_OMP_PARALLEL_MASTER_TASKLOOP:
      mask = GFC_OMP_MASK_PARALLEL | GFC_OMP_MASK_TASKLOOP | GFC_OMP_MASK_SIMD;
      innermost = GFC_OMP_SPLIT_TASKLOOP;
      break;
    case EXEC_OMP_PARALLEL_MASKED_TASKLOOP_SIMD:
      mask = (GFC_OMP_MASK_PARALLEL | GFC_OMP_MASK_MASKED
	      | GFC_OMP_MASK_TASKLOOP | GFC_OMP_MASK_SIMD);
      innermost = GFC_OMP_SPLIT_SIMD;
      break;
    case EXEC_OMP_PARALLEL_MASTER_TASKLOOP_SIMD:
      mask = GFC_OMP_MASK_PARALLEL | GFC_OMP_MASK_TASKLOOP | GFC_OMP_MASK_SIMD;
      innermost = GFC_OMP_SPLIT_SIMD;
      break;
    case EXEC_OMP_SIMD:
      innermost = GFC_OMP_SPLIT_SIMD;
      break;
    case EXEC_OMP_TARGET:
      innermost = GFC_OMP_SPLIT_TARGET;
      break;
    case EXEC_OMP_TARGET_PARALLEL:
      mask = GFC_OMP_MASK_TARGET | GFC_OMP_MASK_PARALLEL;
      innermost = GFC_OMP_SPLIT_PARALLEL;
      break;
    case EXEC_OMP_TARGET_PARALLEL_DO:
    case EXEC_OMP_TARGET_PARALLEL_LOOP:
      mask = GFC_OMP_MASK_TARGET | GFC_OMP_MASK_PARALLEL | GFC_OMP_MASK_DO;
      innermost = GFC_OMP_SPLIT_DO;
      break;
    case EXEC_OMP_TARGET_PARALLEL_DO_SIMD:
      mask = GFC_OMP_MASK_TARGET | GFC_OMP_MASK_PARALLEL | GFC_OMP_MASK_DO
	     | GFC_OMP_MASK_SIMD;
      innermost = GFC_OMP_SPLIT_SIMD;
      break;
    case EXEC_OMP_TARGET_SIMD:
      mask = GFC_OMP_MASK_TARGET | GFC_OMP_MASK_SIMD;
      innermost = GFC_OMP_SPLIT_SIMD;
      break;
    case EXEC_OMP_TARGET_TEAMS:
      mask = GFC_OMP_MASK_TARGET | GFC_OMP_MASK_TEAMS;
      innermost = GFC_OMP_SPLIT_TEAMS;
      break;
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE:
      mask = GFC_OMP_MASK_TARGET | GFC_OMP_MASK_TEAMS
	     | GFC_OMP_MASK_DISTRIBUTE;
      innermost = GFC_OMP_SPLIT_DISTRIBUTE;
      break;
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO:
      mask = GFC_OMP_MASK_TARGET | GFC_OMP_MASK_TEAMS | GFC_OMP_MASK_DISTRIBUTE
	     | GFC_OMP_MASK_PARALLEL | GFC_OMP_MASK_DO;
      innermost = GFC_OMP_SPLIT_DO;
      break;
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
      mask = GFC_OMP_MASK_TARGET | GFC_OMP_MASK_TEAMS | GFC_OMP_MASK_DISTRIBUTE
	     | GFC_OMP_MASK_PARALLEL | GFC_OMP_MASK_DO | GFC_OMP_MASK_SIMD;
      innermost = GFC_OMP_SPLIT_SIMD;
      break;
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_SIMD:
      mask = GFC_OMP_MASK_TARGET | GFC_OMP_MASK_TEAMS
	     | GFC_OMP_MASK_DISTRIBUTE | GFC_OMP_MASK_SIMD;
      innermost = GFC_OMP_SPLIT_SIMD;
      break;
    case EXEC_OMP_TARGET_TEAMS_LOOP:
      mask = GFC_OMP_MASK_TARGET | GFC_OMP_MASK_TEAMS | GFC_OMP_MASK_DO;
      innermost = GFC_OMP_SPLIT_DO;
      break;
    case EXEC_OMP_MASKED_TASKLOOP:
      mask = GFC_OMP_MASK_MASKED | GFC_OMP_MASK_TASKLOOP;
      innermost = GFC_OMP_SPLIT_TASKLOOP;
      break;
    case EXEC_OMP_MASTER_TASKLOOP:
    case EXEC_OMP_TASKLOOP:
      innermost = GFC_OMP_SPLIT_TASKLOOP;
      break;
    case EXEC_OMP_MASKED_TASKLOOP_SIMD:
      mask = GFC_OMP_MASK_MASKED | GFC_OMP_MASK_TASKLOOP | GFC_OMP_MASK_SIMD;
      innermost = GFC_OMP_SPLIT_SIMD;
      break;
    case EXEC_OMP_MASTER_TASKLOOP_SIMD:
    case EXEC_OMP_TASKLOOP_SIMD:
      mask = GFC_OMP_MASK_TASKLOOP | GFC_OMP_MASK_SIMD;
      innermost = GFC_OMP_SPLIT_SIMD;
      break;
    case EXEC_OMP_TEAMS:
      innermost = GFC_OMP_SPLIT_TEAMS;
      break;
    case EXEC_OMP_TEAMS_DISTRIBUTE:
      mask = GFC_OMP_MASK_TEAMS | GFC_OMP_MASK_DISTRIBUTE;
      innermost = GFC_OMP_SPLIT_DISTRIBUTE;
      break;
    case EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO:
      mask = GFC_OMP_MASK_TEAMS | GFC_OMP_MASK_DISTRIBUTE
	     | GFC_OMP_MASK_PARALLEL | GFC_OMP_MASK_DO;
      innermost = GFC_OMP_SPLIT_DO;
      break;
    case EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
      mask = GFC_OMP_MASK_TEAMS | GFC_OMP_MASK_DISTRIBUTE
	     | GFC_OMP_MASK_PARALLEL | GFC_OMP_MASK_DO | GFC_OMP_MASK_SIMD;
      innermost = GFC_OMP_SPLIT_SIMD;
      break;
    case EXEC_OMP_TEAMS_DISTRIBUTE_SIMD:
      mask = GFC_OMP_MASK_TEAMS | GFC_OMP_MASK_DISTRIBUTE | GFC_OMP_MASK_SIMD;
      innermost = GFC_OMP_SPLIT_SIMD;
      break;
    case EXEC_OMP_TEAMS_LOOP:
      mask = GFC_OMP_MASK_TEAMS | GFC_OMP_MASK_DO;
      innermost = GFC_OMP_SPLIT_DO;
      break;
    default:
      gcc_unreachable ();
    }
  if (mask == 0)
    {
      clausesa[innermost] = *code->ext.omp_clauses;
      return;
    }
  /* Loops are similar to DO but still a bit different.  */
  switch (code->op)
    {
    case EXEC_OMP_LOOP:
    case EXEC_OMP_PARALLEL_LOOP:
    case EXEC_OMP_TEAMS_LOOP:
    case EXEC_OMP_TARGET_PARALLEL_LOOP:
    case EXEC_OMP_TARGET_TEAMS_LOOP:
      is_loop = true;
    default:
      break;
    }
  if (code->ext.omp_clauses != NULL)
    {
      if (mask & GFC_OMP_MASK_TARGET)
	{
	  /* First the clauses that are unique to some constructs.  */
	  clausesa[GFC_OMP_SPLIT_TARGET].lists[OMP_LIST_MAP]
	    = code->ext.omp_clauses->lists[OMP_LIST_MAP];
	  clausesa[GFC_OMP_SPLIT_TARGET].lists[OMP_LIST_IS_DEVICE_PTR]
	    = code->ext.omp_clauses->lists[OMP_LIST_IS_DEVICE_PTR];
	  clausesa[GFC_OMP_SPLIT_TARGET].lists[OMP_LIST_HAS_DEVICE_ADDR]
	    = code->ext.omp_clauses->lists[OMP_LIST_HAS_DEVICE_ADDR];
	  clausesa[GFC_OMP_SPLIT_TARGET].device
	    = code->ext.omp_clauses->device;
	  clausesa[GFC_OMP_SPLIT_TARGET].thread_limit
	    = code->ext.omp_clauses->thread_limit;
	  clausesa[GFC_OMP_SPLIT_TARGET].lists[OMP_LIST_USES_ALLOCATORS]
	    = code->ext.omp_clauses->lists[OMP_LIST_USES_ALLOCATORS];
	  for (int i = 0; i < OMP_DEFAULTMAP_CAT_NUM; i++)
	    clausesa[GFC_OMP_SPLIT_TARGET].defaultmap[i]
	      = code->ext.omp_clauses->defaultmap[i];
	  clausesa[GFC_OMP_SPLIT_TARGET].if_exprs[OMP_IF_TARGET]
	    = code->ext.omp_clauses->if_exprs[OMP_IF_TARGET];
	  /* And this is copied to all.  */
	  clausesa[GFC_OMP_SPLIT_TARGET].if_expr
	    = code->ext.omp_clauses->if_expr;
	  clausesa[GFC_OMP_SPLIT_TARGET].nowait
	    = code->ext.omp_clauses->nowait;
	}
      if (mask & GFC_OMP_MASK_TEAMS)
	{
	  /* First the clauses that are unique to some constructs.  */
	  clausesa[GFC_OMP_SPLIT_TEAMS].num_teams_lower
	    = code->ext.omp_clauses->num_teams_lower;
	  clausesa[GFC_OMP_SPLIT_TEAMS].num_teams_upper
	    = code->ext.omp_clauses->num_teams_upper;
	  clausesa[GFC_OMP_SPLIT_TEAMS].thread_limit
	    = code->ext.omp_clauses->thread_limit;
	  /* Shared and default clauses are allowed on parallel, teams
	     and taskloop.  */
	  clausesa[GFC_OMP_SPLIT_TEAMS].lists[OMP_LIST_SHARED]
	    = code->ext.omp_clauses->lists[OMP_LIST_SHARED];
	  clausesa[GFC_OMP_SPLIT_TEAMS].default_sharing
	    = code->ext.omp_clauses->default_sharing;
	}
      if (mask & GFC_OMP_MASK_DISTRIBUTE)
	{
	  /* First the clauses that are unique to some constructs.  */
	  clausesa[GFC_OMP_SPLIT_DISTRIBUTE].dist_sched_kind
	    = code->ext.omp_clauses->dist_sched_kind;
	  clausesa[GFC_OMP_SPLIT_DISTRIBUTE].dist_chunk_size
	    = code->ext.omp_clauses->dist_chunk_size;
	  /* Duplicate collapse.  */
	  clausesa[GFC_OMP_SPLIT_DISTRIBUTE].collapse
	    = code->ext.omp_clauses->collapse;
	  clausesa[GFC_OMP_SPLIT_DISTRIBUTE].order_concurrent
	    = code->ext.omp_clauses->order_concurrent;
	  clausesa[GFC_OMP_SPLIT_DISTRIBUTE].order_unconstrained
	    = code->ext.omp_clauses->order_unconstrained;
	  clausesa[GFC_OMP_SPLIT_DISTRIBUTE].order_reproducible
	    = code->ext.omp_clauses->order_reproducible;
	}
      if (mask & GFC_OMP_MASK_PARALLEL)
	{
	  /* First the clauses that are unique to some constructs.  */
	  clausesa[GFC_OMP_SPLIT_PARALLEL].lists[OMP_LIST_COPYIN]
	    = code->ext.omp_clauses->lists[OMP_LIST_COPYIN];
	  clausesa[GFC_OMP_SPLIT_PARALLEL].num_threads
	    = code->ext.omp_clauses->num_threads;
	  clausesa[GFC_OMP_SPLIT_PARALLEL].proc_bind
	    = code->ext.omp_clauses->proc_bind;
	  /* Shared and default clauses are allowed on parallel, teams
	     and taskloop.  */
	  clausesa[GFC_OMP_SPLIT_PARALLEL].lists[OMP_LIST_SHARED]
	    = code->ext.omp_clauses->lists[OMP_LIST_SHARED];
	  clausesa[GFC_OMP_SPLIT_PARALLEL].default_sharing
	    = code->ext.omp_clauses->default_sharing;
	  clausesa[GFC_OMP_SPLIT_PARALLEL].if_exprs[OMP_IF_PARALLEL]
	    = code->ext.omp_clauses->if_exprs[OMP_IF_PARALLEL];
	  /* And this is copied to all.  */
	  clausesa[GFC_OMP_SPLIT_PARALLEL].if_expr
	    = code->ext.omp_clauses->if_expr;
	}
      if (mask & GFC_OMP_MASK_MASKED)
	clausesa[GFC_OMP_SPLIT_MASKED].filter = code->ext.omp_clauses->filter;
      if ((mask & GFC_OMP_MASK_DO) && !is_loop)
	{
	  /* First the clauses that are unique to some constructs.  */
	  clausesa[GFC_OMP_SPLIT_DO].ordered
	    = code->ext.omp_clauses->ordered;
	  clausesa[GFC_OMP_SPLIT_DO].orderedc
	    = code->ext.omp_clauses->orderedc;
	  clausesa[GFC_OMP_SPLIT_DO].sched_kind
	    = code->ext.omp_clauses->sched_kind;
	  if (innermost == GFC_OMP_SPLIT_SIMD)
	    clausesa[GFC_OMP_SPLIT_DO].sched_simd
	      = code->ext.omp_clauses->sched_simd;
	  clausesa[GFC_OMP_SPLIT_DO].sched_monotonic
	    = code->ext.omp_clauses->sched_monotonic;
	  clausesa[GFC_OMP_SPLIT_DO].sched_nonmonotonic
	    = code->ext.omp_clauses->sched_nonmonotonic;
	  clausesa[GFC_OMP_SPLIT_DO].chunk_size
	    = code->ext.omp_clauses->chunk_size;
	  clausesa[GFC_OMP_SPLIT_DO].nowait
	    = code->ext.omp_clauses->nowait;
	}
      if (mask & GFC_OMP_MASK_DO)
	{
	  clausesa[GFC_OMP_SPLIT_DO].bind
	    = code->ext.omp_clauses->bind;
	  /* Duplicate collapse.  */
	  clausesa[GFC_OMP_SPLIT_DO].collapse
	    = code->ext.omp_clauses->collapse;
	  clausesa[GFC_OMP_SPLIT_DO].order_concurrent
	    = code->ext.omp_clauses->order_concurrent;
	  clausesa[GFC_OMP_SPLIT_DO].order_unconstrained
	    = code->ext.omp_clauses->order_unconstrained;
	  clausesa[GFC_OMP_SPLIT_DO].order_reproducible
	    = code->ext.omp_clauses->order_reproducible;
	}
      if (mask & GFC_OMP_MASK_SIMD)
	{
	  clausesa[GFC_OMP_SPLIT_SIMD].safelen_expr
	    = code->ext.omp_clauses->safelen_expr;
	  clausesa[GFC_OMP_SPLIT_SIMD].simdlen_expr
	    = code->ext.omp_clauses->simdlen_expr;
	  clausesa[GFC_OMP_SPLIT_SIMD].lists[OMP_LIST_ALIGNED]
	    = code->ext.omp_clauses->lists[OMP_LIST_ALIGNED];
	  /* Duplicate collapse.  */
	  clausesa[GFC_OMP_SPLIT_SIMD].collapse
	    = code->ext.omp_clauses->collapse;
	  clausesa[GFC_OMP_SPLIT_SIMD].if_exprs[OMP_IF_SIMD]
	    = code->ext.omp_clauses->if_exprs[OMP_IF_SIMD];
	  clausesa[GFC_OMP_SPLIT_SIMD].order_concurrent
	    = code->ext.omp_clauses->order_concurrent;
	  clausesa[GFC_OMP_SPLIT_SIMD].order_unconstrained
	    = code->ext.omp_clauses->order_unconstrained;
	  clausesa[GFC_OMP_SPLIT_SIMD].order_reproducible
	    = code->ext.omp_clauses->order_reproducible;
	  /* And this is copied to all.  */
	  clausesa[GFC_OMP_SPLIT_SIMD].if_expr
	    = code->ext.omp_clauses->if_expr;
	}
      if (mask & GFC_OMP_MASK_TASKLOOP)
	{
	  /* First the clauses that are unique to some constructs.  */
	  clausesa[GFC_OMP_SPLIT_TASKLOOP].nogroup
	    = code->ext.omp_clauses->nogroup;
	  clausesa[GFC_OMP_SPLIT_TASKLOOP].grainsize
	    = code->ext.omp_clauses->grainsize;
	  clausesa[GFC_OMP_SPLIT_TASKLOOP].grainsize_strict
	    = code->ext.omp_clauses->grainsize_strict;
	  clausesa[GFC_OMP_SPLIT_TASKLOOP].num_tasks
	    = code->ext.omp_clauses->num_tasks;
	  clausesa[GFC_OMP_SPLIT_TASKLOOP].num_tasks_strict
	    = code->ext.omp_clauses->num_tasks_strict;
	  clausesa[GFC_OMP_SPLIT_TASKLOOP].priority
	    = code->ext.omp_clauses->priority;
	  clausesa[GFC_OMP_SPLIT_TASKLOOP].final_expr
	    = code->ext.omp_clauses->final_expr;
	  clausesa[GFC_OMP_SPLIT_TASKLOOP].untied
	    = code->ext.omp_clauses->untied;
	  clausesa[GFC_OMP_SPLIT_TASKLOOP].mergeable
	    = code->ext.omp_clauses->mergeable;
	  clausesa[GFC_OMP_SPLIT_TASKLOOP].if_exprs[OMP_IF_TASKLOOP]
	    = code->ext.omp_clauses->if_exprs[OMP_IF_TASKLOOP];
	  /* And this is copied to all.  */
	  clausesa[GFC_OMP_SPLIT_TASKLOOP].if_expr
	    = code->ext.omp_clauses->if_expr;
	  /* Shared and default clauses are allowed on parallel, teams
	     and taskloop.  */
	  clausesa[GFC_OMP_SPLIT_TASKLOOP].lists[OMP_LIST_SHARED]
	    = code->ext.omp_clauses->lists[OMP_LIST_SHARED];
	  clausesa[GFC_OMP_SPLIT_TASKLOOP].default_sharing
	    = code->ext.omp_clauses->default_sharing;
	  /* Duplicate collapse.  */
	  clausesa[GFC_OMP_SPLIT_TASKLOOP].collapse
	    = code->ext.omp_clauses->collapse;
	}
      /* Private clause is supported on all constructs but master/masked,
	 it is enough to put it on the innermost one except for master/masked.  For
	 !$ omp parallel do put it on parallel though,
	 as that's what we did for OpenMP 3.1.  */
      clausesa[((innermost == GFC_OMP_SPLIT_DO && !is_loop)
		|| code->op == EXEC_OMP_PARALLEL_MASTER
		|| code->op == EXEC_OMP_PARALLEL_MASKED)
	       ? (int) GFC_OMP_SPLIT_PARALLEL
	       : innermost].lists[OMP_LIST_PRIVATE]
	= code->ext.omp_clauses->lists[OMP_LIST_PRIVATE];
      /* Firstprivate clause is supported on all constructs but
	 simd and masked/master.  Put it on the outermost of those and duplicate
	 on parallel and teams.  */
      if (mask & GFC_OMP_MASK_TARGET)
	gfc_add_firstprivate_if_unmapped (&clausesa[GFC_OMP_SPLIT_TARGET],
					  code->ext.omp_clauses);
      if (mask & GFC_OMP_MASK_TEAMS)
	clausesa[GFC_OMP_SPLIT_TEAMS].lists[OMP_LIST_FIRSTPRIVATE]
	  = code->ext.omp_clauses->lists[OMP_LIST_FIRSTPRIVATE];
      else if (mask & GFC_OMP_MASK_DISTRIBUTE)
	clausesa[GFC_OMP_SPLIT_DISTRIBUTE].lists[OMP_LIST_FIRSTPRIVATE]
	  = code->ext.omp_clauses->lists[OMP_LIST_FIRSTPRIVATE];
      if (mask & GFC_OMP_MASK_TASKLOOP)
	clausesa[GFC_OMP_SPLIT_TASKLOOP].lists[OMP_LIST_FIRSTPRIVATE]
	  = code->ext.omp_clauses->lists[OMP_LIST_FIRSTPRIVATE];
      if ((mask & GFC_OMP_MASK_PARALLEL)
	  && !(mask & GFC_OMP_MASK_TASKLOOP))
	clausesa[GFC_OMP_SPLIT_PARALLEL].lists[OMP_LIST_FIRSTPRIVATE]
	  = code->ext.omp_clauses->lists[OMP_LIST_FIRSTPRIVATE];
      else if ((mask & GFC_OMP_MASK_DO) && !is_loop)
	clausesa[GFC_OMP_SPLIT_DO].lists[OMP_LIST_FIRSTPRIVATE]
	  = code->ext.omp_clauses->lists[OMP_LIST_FIRSTPRIVATE];
      /* Lastprivate is allowed on distribute, do, simd, taskloop and loop.
         In parallel do{, simd} we actually want to put it on
	 parallel rather than do.  */
      if (mask & GFC_OMP_MASK_DISTRIBUTE)
	clausesa[GFC_OMP_SPLIT_DISTRIBUTE].lists[OMP_LIST_LASTPRIVATE]
	  = code->ext.omp_clauses->lists[OMP_LIST_LASTPRIVATE];
      if (mask & GFC_OMP_MASK_TASKLOOP)
	clausesa[GFC_OMP_SPLIT_TASKLOOP].lists[OMP_LIST_LASTPRIVATE]
	  = code->ext.omp_clauses->lists[OMP_LIST_LASTPRIVATE];
      if ((mask & GFC_OMP_MASK_PARALLEL) && !is_loop
	  && !(mask & GFC_OMP_MASK_TASKLOOP))
	clausesa[GFC_OMP_SPLIT_PARALLEL].lists[OMP_LIST_LASTPRIVATE]
	  = code->ext.omp_clauses->lists[OMP_LIST_LASTPRIVATE];
      else if (mask & GFC_OMP_MASK_DO)
	clausesa[GFC_OMP_SPLIT_DO].lists[OMP_LIST_LASTPRIVATE]
	  = code->ext.omp_clauses->lists[OMP_LIST_LASTPRIVATE];
      if (mask & GFC_OMP_MASK_SIMD)
	clausesa[GFC_OMP_SPLIT_SIMD].lists[OMP_LIST_LASTPRIVATE]
	  = code->ext.omp_clauses->lists[OMP_LIST_LASTPRIVATE];
      /* Reduction is allowed on simd, do, parallel, teams, taskloop, and loop.
	 Duplicate it on all of them, but
	 - omit on do if parallel is present;
	 - omit on task and parallel if loop is present;
	 additionally, inscan applies to do/simd only.  */
      for (int i = OMP_LIST_REDUCTION; i <= OMP_LIST_REDUCTION_TASK; i++)
	{
	  if (mask & GFC_OMP_MASK_TASKLOOP
	      && i != OMP_LIST_REDUCTION_INSCAN)
	    clausesa[GFC_OMP_SPLIT_TASKLOOP].lists[i]
	      = code->ext.omp_clauses->lists[i];
	  if (mask & GFC_OMP_MASK_TEAMS
	      && i != OMP_LIST_REDUCTION_INSCAN
	      && !is_loop)
	    clausesa[GFC_OMP_SPLIT_TEAMS].lists[i]
	      = code->ext.omp_clauses->lists[i];
	  if (mask & GFC_OMP_MASK_PARALLEL
	      && i != OMP_LIST_REDUCTION_INSCAN
	      && !(mask & GFC_OMP_MASK_TASKLOOP)
	      && !is_loop)
	    clausesa[GFC_OMP_SPLIT_PARALLEL].lists[i]
	      = code->ext.omp_clauses->lists[i];
	  else if (mask & GFC_OMP_MASK_DO)
	    clausesa[GFC_OMP_SPLIT_DO].lists[i]
	      = code->ext.omp_clauses->lists[i];
	  if (mask & GFC_OMP_MASK_SIMD)
	    clausesa[GFC_OMP_SPLIT_SIMD].lists[i]
	      = code->ext.omp_clauses->lists[i];
	}
      if (mask & GFC_OMP_MASK_TARGET)
	clausesa[GFC_OMP_SPLIT_TARGET].lists[OMP_LIST_IN_REDUCTION]
	  = code->ext.omp_clauses->lists[OMP_LIST_IN_REDUCTION];
      if (mask & GFC_OMP_MASK_TASKLOOP)
	clausesa[GFC_OMP_SPLIT_TASKLOOP].lists[OMP_LIST_IN_REDUCTION]
	  = code->ext.omp_clauses->lists[OMP_LIST_IN_REDUCTION];
      /* Linear clause is supported on do and simd,
	 put it on the innermost one.  */
      clausesa[innermost].lists[OMP_LIST_LINEAR]
	= code->ext.omp_clauses->lists[OMP_LIST_LINEAR];
    }
   /* Propagate firstprivate/lastprivate/reduction vars to
      shared (parallel, teams) and map-tofrom (target).  */
   if (mask & GFC_OMP_MASK_TARGET)
     gfc_add_clause_implicitly (&clausesa[GFC_OMP_SPLIT_TARGET],
				code->ext.omp_clauses, true, false);
   if ((mask & GFC_OMP_MASK_PARALLEL) && innermost != GFC_OMP_MASK_PARALLEL)
     gfc_add_clause_implicitly (&clausesa[GFC_OMP_SPLIT_PARALLEL],
				code->ext.omp_clauses, false,
				mask & GFC_OMP_MASK_DO);
   if (mask & GFC_OMP_MASK_TEAMS && innermost != GFC_OMP_MASK_TEAMS)
     gfc_add_clause_implicitly (&clausesa[GFC_OMP_SPLIT_TEAMS],
				code->ext.omp_clauses, false, false);
   if (((mask & (GFC_OMP_MASK_PARALLEL | GFC_OMP_MASK_DO))
	== (GFC_OMP_MASK_PARALLEL | GFC_OMP_MASK_DO))
       && !is_loop)
    clausesa[GFC_OMP_SPLIT_DO].nowait = true;

   /* Distribute allocate clause to do, parallel, distribute, teams, target
      and taskloop.  The code below iterates over variables in the
      allocate list and checks if that available is also in any
      privatization clause on those construct.  If yes, then we add it
      to the list of 'allocate'ed variables for that construct.  If a
      variable is found in none of them then we issue an error.  */

   if (code->ext.omp_clauses->lists[OMP_LIST_ALLOCATE])
     {
       gfc_omp_namelist *alloc_nl, *priv_nl;
       gfc_omp_namelist *tails[GFC_OMP_SPLIT_NUM];
       for (alloc_nl = code->ext.omp_clauses->lists[OMP_LIST_ALLOCATE];
	   alloc_nl; alloc_nl = alloc_nl->next)
	 {
	   bool found = false;
	   for (int i = GFC_OMP_SPLIT_DO; i <= GFC_OMP_SPLIT_TASKLOOP; i++)
	     {
	       gfc_omp_namelist *p;
	       int list;
	       for (list = 0; list < OMP_LIST_NUM; list++)
		 {
		   switch (list)
		   {
		     case OMP_LIST_PRIVATE:
		     case OMP_LIST_FIRSTPRIVATE:
		     case OMP_LIST_LASTPRIVATE:
		     case OMP_LIST_REDUCTION:
		     case OMP_LIST_REDUCTION_INSCAN:
		     case OMP_LIST_REDUCTION_TASK:
		     case OMP_LIST_IN_REDUCTION:
		     case OMP_LIST_TASK_REDUCTION:
		     case OMP_LIST_LINEAR:
		       for (priv_nl = clausesa[i].lists[list]; priv_nl;
			    priv_nl = priv_nl->next)
			 if (alloc_nl->sym == priv_nl->sym)
			   {
			     found = true;
			     p = gfc_get_omp_namelist ();
			     p->sym = alloc_nl->sym;
			     p->expr = alloc_nl->expr;
			     p->u.align = alloc_nl->u.align;
			     p->u2.allocator = alloc_nl->u2.allocator;
			     p->where = alloc_nl->where;
			     if (clausesa[i].lists[OMP_LIST_ALLOCATE] == NULL)
			       {
				 clausesa[i].lists[OMP_LIST_ALLOCATE] = p;
				 tails[i] = p;
			       }
			     else
			       {
				 tails[i]->next = p;
				 tails[i] = tails[i]->next;
			       }
			   }
		       break;
		     default:
		       break;
		   }
		 }
	     }
	   if (!found)
	     gfc_error ("%qs specified in %<allocate%> clause at %L but not "
			"in an explicit privatization clause",
			alloc_nl->sym->name, &alloc_nl->where);
	 }
     }
}

static tree
gfc_trans_omp_do_simd (gfc_code *code, stmtblock_t *pblock,
		       gfc_omp_clauses *clausesa, tree omp_clauses)
{
  stmtblock_t block;
  gfc_omp_clauses clausesa_buf[GFC_OMP_SPLIT_NUM];
  tree stmt, body, omp_do_clauses = NULL_TREE;
  bool free_clausesa = false;

  if (pblock == NULL)
    gfc_start_block (&block);
  else
    gfc_init_block (&block);

  if (clausesa == NULL)
    {
      clausesa = clausesa_buf;
      gfc_split_omp_clauses (code, clausesa);
      free_clausesa = true;
    }
  if (flag_openmp)
    omp_do_clauses
      = gfc_trans_omp_clauses (&block, &clausesa[GFC_OMP_SPLIT_DO], code->loc);
  body = gfc_trans_omp_do (code, EXEC_OMP_SIMD, pblock ? pblock : &block,
			   &clausesa[GFC_OMP_SPLIT_SIMD], omp_clauses);
  if (pblock == NULL)
    {
      if (TREE_CODE (body) != BIND_EXPR)
	body = build3_v (BIND_EXPR, NULL, body, poplevel (1, 0));
      else
	poplevel (0, 0);
    }
  else if (TREE_CODE (body) != BIND_EXPR)
    body = build3_v (BIND_EXPR, NULL, body, NULL_TREE);
  if (flag_openmp)
    {
      stmt = make_node (OMP_FOR);
      SET_EXPR_LOCATION (stmt, gfc_get_location (&code->loc));
      TREE_TYPE (stmt) = void_type_node;
      OMP_FOR_BODY (stmt) = body;
      OMP_FOR_CLAUSES (stmt) = omp_do_clauses;
    }
  else
    stmt = body;
  gfc_add_expr_to_block (&block, stmt);
  if (free_clausesa)
    gfc_free_split_omp_clauses (code, clausesa);
  return gfc_finish_block (&block);
}

static tree
gfc_trans_omp_parallel_do (gfc_code *code, bool is_loop, stmtblock_t *pblock,
			   gfc_omp_clauses *clausesa)
{
  stmtblock_t block, *new_pblock = pblock;
  gfc_omp_clauses clausesa_buf[GFC_OMP_SPLIT_NUM];
  tree stmt, omp_clauses = NULL_TREE;
  bool free_clausesa = false;

  if (pblock == NULL)
    gfc_start_block (&block);
  else
    gfc_init_block (&block);

  if (clausesa == NULL)
    {
      clausesa = clausesa_buf;
      gfc_split_omp_clauses (code, clausesa);
      free_clausesa = true;
    }
  omp_clauses
    = gfc_trans_omp_clauses (&block, &clausesa[GFC_OMP_SPLIT_PARALLEL],
			     code->loc);
  if (pblock == NULL)
    {
      if (!clausesa[GFC_OMP_SPLIT_DO].ordered
	  && clausesa[GFC_OMP_SPLIT_DO].sched_kind != OMP_SCHED_STATIC)
	new_pblock = &block;
      else
	pushlevel ();
    }
  stmt = gfc_trans_omp_do (code, is_loop ? EXEC_OMP_LOOP : EXEC_OMP_DO,
			   new_pblock, &clausesa[GFC_OMP_SPLIT_DO],
			   omp_clauses);
  if (pblock == NULL)
    {
      if (TREE_CODE (stmt) != BIND_EXPR)
	stmt = build3_v (BIND_EXPR, NULL, stmt, poplevel (1, 0));
      else
	poplevel (0, 0);
    }
  else if (TREE_CODE (stmt) != BIND_EXPR)
    stmt = build3_v (BIND_EXPR, NULL, stmt, NULL_TREE);
  stmt = build2_loc (gfc_get_location (&code->loc), OMP_PARALLEL,
		     void_type_node, stmt, omp_clauses);
  OMP_PARALLEL_COMBINED (stmt) = 1;
  gfc_add_expr_to_block (&block, stmt);
  if (free_clausesa)
    gfc_free_split_omp_clauses (code, clausesa);
  return gfc_finish_block (&block);
}

static tree
gfc_trans_omp_parallel_do_simd (gfc_code *code, stmtblock_t *pblock,
				gfc_omp_clauses *clausesa)
{
  stmtblock_t block;
  gfc_omp_clauses clausesa_buf[GFC_OMP_SPLIT_NUM];
  tree stmt, omp_clauses = NULL_TREE;
  bool free_clausesa = false;

  if (pblock == NULL)
    gfc_start_block (&block);
  else
    gfc_init_block (&block);

  if (clausesa == NULL)
    {
      clausesa = clausesa_buf;
      gfc_split_omp_clauses (code, clausesa);
      free_clausesa = true;
    }
  if (flag_openmp)
    omp_clauses
      = gfc_trans_omp_clauses (&block, &clausesa[GFC_OMP_SPLIT_PARALLEL],
			       code->loc);
  if (pblock == NULL)
    pushlevel ();
  stmt = gfc_trans_omp_do_simd (code, pblock, clausesa, omp_clauses);
  if (pblock == NULL)
    {
      if (TREE_CODE (stmt) != BIND_EXPR)
	stmt = build3_v (BIND_EXPR, NULL, stmt, poplevel (1, 0));
      else
	poplevel (0, 0);
    }
  else if (TREE_CODE (stmt) != BIND_EXPR)
    stmt = build3_v (BIND_EXPR, NULL, stmt, NULL_TREE);
  if (flag_openmp)
    {
      stmt = build2_loc (gfc_get_location (&code->loc), OMP_PARALLEL,
			 void_type_node, stmt, omp_clauses);
      OMP_PARALLEL_COMBINED (stmt) = 1;
    }
  gfc_add_expr_to_block (&block, stmt);
  if (free_clausesa)
    gfc_free_split_omp_clauses (code, clausesa);
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
  pushlevel ();
  stmt = gfc_trans_omp_sections (code, &section_clauses);
  if (TREE_CODE (stmt) != BIND_EXPR)
    stmt = build3_v (BIND_EXPR, NULL, stmt, poplevel (1, 0));
  else
    poplevel (0, 0);
  stmt = build2_loc (gfc_get_location (&code->loc), OMP_PARALLEL,
		     void_type_node, stmt, omp_clauses);
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
  pushlevel ();
  stmt = gfc_trans_omp_workshare (code, &workshare_clauses);
  stmt = build3_v (BIND_EXPR, NULL, stmt, poplevel (1, 0));
  stmt = build2_loc (gfc_get_location (&code->loc), OMP_PARALLEL,
		     void_type_node, stmt, omp_clauses);
  OMP_PARALLEL_COMBINED (stmt) = 1;
  gfc_add_expr_to_block (&block, stmt);
  return gfc_finish_block (&block);
}

static tree
gfc_trans_omp_scope (gfc_code *code)
{
  stmtblock_t block;
  tree body = gfc_trans_code (code->block->next);
  if (IS_EMPTY_STMT (body))
    return body;
  gfc_start_block (&block);
  tree omp_clauses = gfc_trans_omp_clauses (&block, code->ext.omp_clauses,
					    code->loc);
  tree stmt = make_node (OMP_SCOPE);
  SET_EXPR_LOCATION (stmt, gfc_get_location (&code->loc));
  TREE_TYPE (stmt) = void_type_node;
  OMP_SCOPE_BODY (stmt) = body;
  OMP_SCOPE_CLAUSES (stmt) = omp_clauses;
  gfc_add_expr_to_block (&block, stmt);
  return gfc_finish_block (&block);
}

static tree
gfc_trans_omp_sections (gfc_code *code, gfc_omp_clauses *clauses)
{
  stmtblock_t block, body;
  tree omp_clauses, stmt;
  bool has_lastprivate = clauses->lists[OMP_LIST_LASTPRIVATE] != NULL;
  location_t loc = gfc_get_location (&code->loc);

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

  stmt = build2_loc (loc, OMP_SECTIONS, void_type_node, stmt, omp_clauses);
  gfc_add_expr_to_block (&block, stmt);

  return gfc_finish_block (&block);
}

static tree
gfc_trans_omp_single (gfc_code *code, gfc_omp_clauses *clauses)
{
  stmtblock_t block;
  gfc_start_block (&block);
  tree omp_clauses = gfc_trans_omp_clauses (&block, clauses, code->loc);
  tree stmt = gfc_trans_omp_code (code->block->next, true);
  stmt = build2_loc (gfc_get_location (&code->loc), OMP_SINGLE, void_type_node,
		     stmt, omp_clauses);
  gfc_add_expr_to_block (&block, stmt);
  return gfc_finish_block (&block);
}

static tree
gfc_trans_omp_task (gfc_code *code)
{
  stmtblock_t block;
  tree stmt, omp_clauses;

  gfc_start_block (&block);
  omp_clauses = gfc_trans_omp_clauses (&block, code->ext.omp_clauses,
				       code->loc);
  pushlevel ();
  stmt = gfc_trans_omp_code (code->block->next, true);
  stmt = build3_v (BIND_EXPR, NULL, stmt, poplevel (1, 0));
  stmt = build2_loc (gfc_get_location (&code->loc), OMP_TASK, void_type_node,
		     stmt, omp_clauses);
  gfc_add_expr_to_block (&block, stmt);
  return gfc_finish_block (&block);
}

static tree
gfc_trans_omp_taskgroup (gfc_code *code)
{
  stmtblock_t block;
  gfc_start_block (&block);
  tree body = gfc_trans_code (code->block->next);
  tree stmt = make_node (OMP_TASKGROUP);
  SET_EXPR_LOCATION (stmt, gfc_get_location (&code->loc));
  TREE_TYPE (stmt) = void_type_node;
  OMP_TASKGROUP_BODY (stmt) = body;
  OMP_TASKGROUP_CLAUSES (stmt) = gfc_trans_omp_clauses (&block,
							code->ext.omp_clauses,
							code->loc);
  gfc_add_expr_to_block (&block, stmt);
  return gfc_finish_block (&block);
}

static tree
gfc_trans_omp_taskwait (gfc_code *code)
{
  if (!code->ext.omp_clauses)
    {
      tree decl = builtin_decl_explicit (BUILT_IN_GOMP_TASKWAIT);
      return build_call_expr_loc (input_location, decl, 0);
    }
  stmtblock_t block;
  gfc_start_block (&block);
  tree stmt = make_node (OMP_TASK);
  SET_EXPR_LOCATION (stmt, gfc_get_location (&code->loc));
  TREE_TYPE (stmt) = void_type_node;
  OMP_TASK_BODY (stmt) = NULL_TREE;
  OMP_TASK_CLAUSES (stmt) = gfc_trans_omp_clauses (&block,
						   code->ext.omp_clauses,
						   code->loc);
  gfc_add_expr_to_block (&block, stmt);
  return gfc_finish_block (&block);
}

static tree
gfc_trans_omp_taskyield (void)
{
  tree decl = builtin_decl_explicit (BUILT_IN_GOMP_TASKYIELD);
  return build_call_expr_loc (input_location, decl, 0);
}

static tree
gfc_trans_omp_distribute (gfc_code *code, gfc_omp_clauses *clausesa)
{
  stmtblock_t block;
  gfc_omp_clauses clausesa_buf[GFC_OMP_SPLIT_NUM];
  tree stmt, omp_clauses = NULL_TREE;
  bool free_clausesa = false;

  gfc_start_block (&block);
  if (clausesa == NULL)
    {
      clausesa = clausesa_buf;
      gfc_split_omp_clauses (code, clausesa);
      free_clausesa = true;
    }
  if (flag_openmp)
    omp_clauses
      = gfc_trans_omp_clauses (&block, &clausesa[GFC_OMP_SPLIT_DISTRIBUTE],
			       code->loc);
  switch (code->op)
    {
    case EXEC_OMP_DISTRIBUTE:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE:
    case EXEC_OMP_TEAMS_DISTRIBUTE:
      /* This is handled in gfc_trans_omp_do.  */
      gcc_unreachable ();
      break;
    case EXEC_OMP_DISTRIBUTE_PARALLEL_DO:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO:
    case EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO:
      stmt = gfc_trans_omp_parallel_do (code, false, &block, clausesa);
      if (TREE_CODE (stmt) != BIND_EXPR)
	stmt = build3_v (BIND_EXPR, NULL, stmt, poplevel (1, 0));
      else
	poplevel (0, 0);
      break;
    case EXEC_OMP_DISTRIBUTE_PARALLEL_DO_SIMD:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
    case EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
      stmt = gfc_trans_omp_parallel_do_simd (code, &block, clausesa);
      if (TREE_CODE (stmt) != BIND_EXPR)
	stmt = build3_v (BIND_EXPR, NULL, stmt, poplevel (1, 0));
      else
	poplevel (0, 0);
      break;
    case EXEC_OMP_DISTRIBUTE_SIMD:
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE_SIMD:
    case EXEC_OMP_TEAMS_DISTRIBUTE_SIMD:
      stmt = gfc_trans_omp_do (code, EXEC_OMP_SIMD, &block,
			       &clausesa[GFC_OMP_SPLIT_SIMD], NULL_TREE);
      if (TREE_CODE (stmt) != BIND_EXPR)
	stmt = build3_v (BIND_EXPR, NULL, stmt, poplevel (1, 0));
      else
	poplevel (0, 0);
      break;
    default:
      gcc_unreachable ();
    }
  if (flag_openmp)
    {
      tree distribute = make_node (OMP_DISTRIBUTE);
      SET_EXPR_LOCATION (distribute, gfc_get_location (&code->loc));
      TREE_TYPE (distribute) = void_type_node;
      OMP_FOR_BODY (distribute) = stmt;
      OMP_FOR_CLAUSES (distribute) = omp_clauses;
      stmt = distribute;
    }
  gfc_add_expr_to_block (&block, stmt);
  if (free_clausesa)
    gfc_free_split_omp_clauses (code, clausesa);
  return gfc_finish_block (&block);
}

static tree
gfc_trans_omp_teams (gfc_code *code, gfc_omp_clauses *clausesa,
		     tree omp_clauses)
{
  stmtblock_t block;
  gfc_omp_clauses clausesa_buf[GFC_OMP_SPLIT_NUM];
  tree stmt;
  bool combined = true, free_clausesa = false;

  gfc_start_block (&block);
  if (clausesa == NULL)
    {
      clausesa = clausesa_buf;
      gfc_split_omp_clauses (code, clausesa);
      free_clausesa = true;
    }
  if (flag_openmp)
    {
      omp_clauses
	= chainon (omp_clauses,
		   gfc_trans_omp_clauses (&block,
					  &clausesa[GFC_OMP_SPLIT_TEAMS],
					  code->loc));
      pushlevel ();
    }
  switch (code->op)
    {
    case EXEC_OMP_TARGET_TEAMS:
    case EXEC_OMP_TEAMS:
      stmt = gfc_trans_omp_code (code->block->next, true);
      combined = false;
      break;
    case EXEC_OMP_TARGET_TEAMS_DISTRIBUTE:
    case EXEC_OMP_TEAMS_DISTRIBUTE:
      stmt = gfc_trans_omp_do (code, EXEC_OMP_DISTRIBUTE, NULL,
			       &clausesa[GFC_OMP_SPLIT_DISTRIBUTE],
			       NULL);
      break;
    case EXEC_OMP_TARGET_TEAMS_LOOP:
    case EXEC_OMP_TEAMS_LOOP:
      stmt = gfc_trans_omp_do (code, EXEC_OMP_LOOP, NULL,
			       &clausesa[GFC_OMP_SPLIT_DO],
			       NULL);
      break;
    default:
      stmt = gfc_trans_omp_distribute (code, clausesa);
      break;
    }
  if (flag_openmp)
    {
      stmt = build3_v (BIND_EXPR, NULL, stmt, poplevel (1, 0));
      stmt = build2_loc (gfc_get_location (&code->loc), OMP_TEAMS,
			 void_type_node, stmt, omp_clauses);
      if (combined)
	OMP_TEAMS_COMBINED (stmt) = 1;
    }
  gfc_add_expr_to_block (&block, stmt);
  if (free_clausesa)
    gfc_free_split_omp_clauses (code, clausesa);
  return gfc_finish_block (&block);
}

static tree
gfc_trans_omp_target (gfc_code *code)
{
  stmtblock_t block;
  gfc_omp_clauses clausesa[GFC_OMP_SPLIT_NUM];
  tree stmt, omp_clauses = NULL_TREE;

  gfc_start_block (&block);
  gfc_split_omp_clauses (code, clausesa);
  if (flag_openmp)
    omp_clauses
      = gfc_trans_omp_clauses (&block, &clausesa[GFC_OMP_SPLIT_TARGET],
			       code->loc);
  switch (code->op)
    {
    case EXEC_OMP_TARGET:
      pushlevel ();
      stmt = gfc_trans_omp_code (code->block->next, true);
      stmt = build3_v (BIND_EXPR, NULL, stmt, poplevel (1, 0));
      break;
    case EXEC_OMP_TARGET_PARALLEL:
      {
	stmtblock_t iblock;

	pushlevel ();
	gfc_start_block (&iblock);
	tree inner_clauses
	  = gfc_trans_omp_clauses (&iblock, &clausesa[GFC_OMP_SPLIT_PARALLEL],
				   code->loc);
	stmt = gfc_trans_omp_code (code->block->next, true);
	stmt = build2_loc (input_location, OMP_PARALLEL, void_type_node, stmt,
			   inner_clauses);
	gfc_add_expr_to_block (&iblock, stmt);
	stmt = gfc_finish_block (&iblock);
	if (TREE_CODE (stmt) != BIND_EXPR)
	  stmt = build3_v (BIND_EXPR, NULL, stmt, poplevel (1, 0));
	else
	  poplevel (0, 0);
      }
      break;
    case EXEC_OMP_TARGET_PARALLEL_DO:
    case EXEC_OMP_TARGET_PARALLEL_LOOP:
      stmt = gfc_trans_omp_parallel_do (code,
					(code->op
					 == EXEC_OMP_TARGET_PARALLEL_LOOP),
					&block, clausesa);
      if (TREE_CODE (stmt) != BIND_EXPR)
	stmt = build3_v (BIND_EXPR, NULL, stmt, poplevel (1, 0));
      else
	poplevel (0, 0);
      break;
    case EXEC_OMP_TARGET_PARALLEL_DO_SIMD:
      stmt = gfc_trans_omp_parallel_do_simd (code, &block, clausesa);
      if (TREE_CODE (stmt) != BIND_EXPR)
	stmt = build3_v (BIND_EXPR, NULL, stmt, poplevel (1, 0));
      else
	poplevel (0, 0);
      break;
    case EXEC_OMP_TARGET_SIMD:
      stmt = gfc_trans_omp_do (code, EXEC_OMP_SIMD, &block,
			       &clausesa[GFC_OMP_SPLIT_SIMD], NULL_TREE);
      if (TREE_CODE (stmt) != BIND_EXPR)
	stmt = build3_v (BIND_EXPR, NULL, stmt, poplevel (1, 0));
      else
	poplevel (0, 0);
      break;
    default:
      if (flag_openmp
	  && (clausesa[GFC_OMP_SPLIT_TEAMS].num_teams_upper
	      || clausesa[GFC_OMP_SPLIT_TEAMS].thread_limit))
	{
	  gfc_omp_clauses clausesb;
	  tree teams_clauses;
	  /* For combined !$omp target teams, the num_teams and
	     thread_limit clauses are evaluated before entering the
	     target construct.  */
	  memset (&clausesb, '\0', sizeof (clausesb));
	  clausesb.num_teams_lower
	    = clausesa[GFC_OMP_SPLIT_TEAMS].num_teams_lower;
	  clausesb.num_teams_upper
	    = clausesa[GFC_OMP_SPLIT_TEAMS].num_teams_upper;
	  clausesb.thread_limit = clausesa[GFC_OMP_SPLIT_TEAMS].thread_limit;
	  clausesa[GFC_OMP_SPLIT_TEAMS].num_teams_lower = NULL;
	  clausesa[GFC_OMP_SPLIT_TEAMS].num_teams_upper = NULL;
	  clausesa[GFC_OMP_SPLIT_TEAMS].thread_limit = NULL;
	  teams_clauses
	    = gfc_trans_omp_clauses (&block, &clausesb, code->loc);
	  pushlevel ();
	  stmt = gfc_trans_omp_teams (code, clausesa, teams_clauses);
	}
      else
	{
	  pushlevel ();
	  stmt = gfc_trans_omp_teams (code, clausesa, NULL_TREE);
	}
      if (TREE_CODE (stmt) != BIND_EXPR)
	stmt = build3_v (BIND_EXPR, NULL, stmt, poplevel (1, 0));
      else
	poplevel (0, 0);
      break;
    }
  if (flag_openmp)
    {
      stmt = build2_loc (gfc_get_location (&code->loc), OMP_TARGET,
			 void_type_node, stmt, omp_clauses);
      if (code->op != EXEC_OMP_TARGET)
	OMP_TARGET_COMBINED (stmt) = 1;
      cfun->has_omp_target = true;
    }
  gfc_add_expr_to_block (&block, stmt);
  gfc_free_split_omp_clauses (code, clausesa);
  return gfc_finish_block (&block);
}

static tree
gfc_trans_omp_taskloop (gfc_code *code, gfc_exec_op op)
{
  stmtblock_t block;
  gfc_omp_clauses clausesa[GFC_OMP_SPLIT_NUM];
  tree stmt, omp_clauses = NULL_TREE;

  gfc_start_block (&block);
  gfc_split_omp_clauses (code, clausesa);
  if (flag_openmp)
    omp_clauses
      = gfc_trans_omp_clauses (&block, &clausesa[GFC_OMP_SPLIT_TASKLOOP],
			       code->loc);
  switch (op)
    {
    case EXEC_OMP_TASKLOOP:
      /* This is handled in gfc_trans_omp_do.  */
      gcc_unreachable ();
      break;
    case EXEC_OMP_TASKLOOP_SIMD:
      stmt = gfc_trans_omp_do (code, EXEC_OMP_SIMD, &block,
			       &clausesa[GFC_OMP_SPLIT_SIMD], NULL_TREE);
      if (TREE_CODE (stmt) != BIND_EXPR)
	stmt = build3_v (BIND_EXPR, NULL, stmt, poplevel (1, 0));
      else
	poplevel (0, 0);
      break;
    default:
      gcc_unreachable ();
    }
  if (flag_openmp)
    {
      tree taskloop = make_node (OMP_TASKLOOP);
      SET_EXPR_LOCATION (taskloop, gfc_get_location (&code->loc));
      TREE_TYPE (taskloop) = void_type_node;
      OMP_FOR_BODY (taskloop) = stmt;
      OMP_FOR_CLAUSES (taskloop) = omp_clauses;
      stmt = taskloop;
    }
  gfc_add_expr_to_block (&block, stmt);
  gfc_free_split_omp_clauses (code, clausesa);
  return gfc_finish_block (&block);
}

static tree
gfc_trans_omp_master_masked_taskloop (gfc_code *code, gfc_exec_op op)
{
  gfc_omp_clauses clausesa[GFC_OMP_SPLIT_NUM];
  stmtblock_t block;
  tree stmt;

  if (op != EXEC_OMP_MASTER_TASKLOOP_SIMD
      && code->op != EXEC_OMP_MASTER_TASKLOOP)
    gfc_split_omp_clauses (code, clausesa);

  pushlevel ();
  if (op == EXEC_OMP_MASKED_TASKLOOP_SIMD
      || op == EXEC_OMP_MASTER_TASKLOOP_SIMD)
    stmt = gfc_trans_omp_taskloop (code, EXEC_OMP_TASKLOOP_SIMD);
  else
    {
      gcc_assert (op == EXEC_OMP_MASKED_TASKLOOP
		  || op == EXEC_OMP_MASTER_TASKLOOP);
      stmt = gfc_trans_omp_do (code, EXEC_OMP_TASKLOOP, NULL,
			       code->op != EXEC_OMP_MASTER_TASKLOOP
			       ? &clausesa[GFC_OMP_SPLIT_TASKLOOP]
			       : code->ext.omp_clauses, NULL);
    }
  if (TREE_CODE (stmt) != BIND_EXPR)
    stmt = build3_v (BIND_EXPR, NULL, stmt, poplevel (1, 0));
  else
    poplevel (0, 0);
  gfc_start_block (&block);
  if (op == EXEC_OMP_MASKED_TASKLOOP || op == EXEC_OMP_MASKED_TASKLOOP_SIMD)
    {
      tree clauses = gfc_trans_omp_clauses (&block,
					    &clausesa[GFC_OMP_SPLIT_MASKED],
					    code->loc);
      tree msk = make_node (OMP_MASKED);
      SET_EXPR_LOCATION (msk, gfc_get_location (&code->loc));
      TREE_TYPE (msk) = void_type_node;
      OMP_MASKED_BODY (msk) = stmt;
      OMP_MASKED_CLAUSES (msk) = clauses;
      OMP_MASKED_COMBINED (msk) = 1;
      gfc_add_expr_to_block (&block, msk);
    }
  else
    {
      gcc_assert (op == EXEC_OMP_MASTER_TASKLOOP
		  || op == EXEC_OMP_MASTER_TASKLOOP_SIMD);
      stmt = build1_v (OMP_MASTER, stmt);
      gfc_add_expr_to_block (&block, stmt);
    }
  if (op != EXEC_OMP_MASTER_TASKLOOP_SIMD
      && code->op != EXEC_OMP_MASTER_TASKLOOP)
    gfc_free_split_omp_clauses (code, clausesa);
  return gfc_finish_block (&block);
}

static tree
gfc_trans_omp_parallel_master_masked (gfc_code *code)
{
  stmtblock_t block;
  tree stmt, omp_clauses;
  gfc_omp_clauses clausesa[GFC_OMP_SPLIT_NUM];
  bool parallel_combined = false;

  if (code->op != EXEC_OMP_PARALLEL_MASTER)
    gfc_split_omp_clauses (code, clausesa);

  gfc_start_block (&block);
  omp_clauses = gfc_trans_omp_clauses (&block,
				       code->op == EXEC_OMP_PARALLEL_MASTER
				       ? code->ext.omp_clauses
				       : &clausesa[GFC_OMP_SPLIT_PARALLEL],
				       code->loc);
  pushlevel ();
  if (code->op == EXEC_OMP_PARALLEL_MASTER)
    stmt = gfc_trans_omp_master (code);
  else if (code->op == EXEC_OMP_PARALLEL_MASKED)
    stmt = gfc_trans_omp_masked (code, &clausesa[GFC_OMP_SPLIT_MASKED]);
  else
    {
      gfc_exec_op op;
      switch (code->op)
	{
	case EXEC_OMP_PARALLEL_MASKED_TASKLOOP:
	  op = EXEC_OMP_MASKED_TASKLOOP;
	  break;
	case EXEC_OMP_PARALLEL_MASKED_TASKLOOP_SIMD:
	  op = EXEC_OMP_MASKED_TASKLOOP_SIMD;
	  break;
	case EXEC_OMP_PARALLEL_MASTER_TASKLOOP:
	  op = EXEC_OMP_MASTER_TASKLOOP;
	  break;
	case EXEC_OMP_PARALLEL_MASTER_TASKLOOP_SIMD:
	  op = EXEC_OMP_MASTER_TASKLOOP_SIMD;
	  break;
	default:
	  gcc_unreachable ();
	}
      stmt = gfc_trans_omp_master_masked_taskloop (code, op);
      parallel_combined = true;
    }
  if (TREE_CODE (stmt) != BIND_EXPR)
    stmt = build3_v (BIND_EXPR, NULL, stmt, poplevel (1, 0));
  else
    poplevel (0, 0);
  stmt = build2_loc (gfc_get_location (&code->loc), OMP_PARALLEL,
		     void_type_node, stmt, omp_clauses);
  /* masked does have just filter clause, but during gimplification
     isn't represented by a gimplification omp context, so for
       !$omp parallel masked don't set OMP_PARALLEL_COMBINED,
     so that
       !$omp parallel masked
       !$omp taskloop simd lastprivate (x)
     isn't confused with
       !$omp parallel masked taskloop simd lastprivate (x)  */
  if (parallel_combined)
    OMP_PARALLEL_COMBINED (stmt) = 1;
  gfc_add_expr_to_block (&block, stmt);
  if (code->op != EXEC_OMP_PARALLEL_MASTER)
    gfc_free_split_omp_clauses (code, clausesa);
  return gfc_finish_block (&block);
}

static tree
gfc_trans_omp_target_data (gfc_code *code)
{
  stmtblock_t block;
  tree stmt, omp_clauses;

  gfc_start_block (&block);
  omp_clauses = gfc_trans_omp_clauses (&block, code->ext.omp_clauses,
				       code->loc);
  stmt = gfc_trans_omp_code (code->block->next, true);
  stmt = build2_loc (gfc_get_location (&code->loc), OMP_TARGET_DATA,
		     void_type_node, stmt, omp_clauses);
  gfc_add_expr_to_block (&block, stmt);
  return gfc_finish_block (&block);
}

static tree
gfc_trans_omp_target_enter_data (gfc_code *code)
{
  stmtblock_t block;
  tree stmt, omp_clauses;

  gfc_start_block (&block);
  omp_clauses = gfc_trans_omp_clauses (&block, code->ext.omp_clauses,
				       code->loc);
  stmt = build1_loc (input_location, OMP_TARGET_ENTER_DATA, void_type_node,
		     omp_clauses);
  gfc_add_expr_to_block (&block, stmt);
  return gfc_finish_block (&block);
}

static tree
gfc_trans_omp_target_exit_data (gfc_code *code)
{
  stmtblock_t block;
  tree stmt, omp_clauses;

  gfc_start_block (&block);
  omp_clauses = gfc_trans_omp_clauses (&block, code->ext.omp_clauses,
				       code->loc, false, false, code->op);
  stmt = build1_loc (input_location, OMP_TARGET_EXIT_DATA, void_type_node,
		     omp_clauses);
  gfc_add_expr_to_block (&block, stmt);
  return gfc_finish_block (&block);
}

static tree
gfc_trans_omp_target_update (gfc_code *code)
{
  stmtblock_t block;
  tree stmt, omp_clauses;

  gfc_start_block (&block);
  omp_clauses = gfc_trans_omp_clauses (&block, code->ext.omp_clauses,
				       code->loc);
  stmt = build1_loc (input_location, OMP_TARGET_UPDATE, void_type_node,
		     omp_clauses);
  gfc_add_expr_to_block (&block, stmt);
  return gfc_finish_block (&block);
}

static tree
gfc_trans_openmp_interop (gfc_code *code, gfc_omp_clauses *clauses)
{
  stmtblock_t block;
  gfc_start_block (&block);
  tree omp_clauses = gfc_trans_omp_clauses (&block, clauses, code->loc);
  tree stmt = build1_loc (input_location, OMP_INTEROP, void_type_node,
			  omp_clauses);
  gfc_add_expr_to_block (&block, stmt);
  return gfc_finish_block (&block);
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
  location_t loc = gfc_get_location (&code->loc);

  code = code->block->next;

  pushlevel ();

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
      ompws_flags &= ~(OMPWS_SCALARIZER_WS | OMPWS_SCALARIZER_BODY);

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
	case EXEC_OMP_PARALLEL_MASTER:
	case EXEC_OMP_PARALLEL_SECTIONS:
	case EXEC_OMP_PARALLEL_WORKSHARE:
	case EXEC_OMP_CRITICAL:
	  saved_ompws_flags = ompws_flags;
	  ompws_flags = 0;
	  res = gfc_trans_omp_directive (code);
	  ompws_flags = saved_ompws_flags;
	  break;

	case EXEC_BLOCK:
	  res = gfc_trans_block_construct (code);
	  break;

	default:
	  gfc_internal_error ("gfc_trans_omp_workshare(): Bad statement code");
	}

      input_location = gfc_get_location (&code->loc);

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
		  tmp = build2_loc (loc, OMP_SINGLE,
				    void_type_node, tmp, NULL_TREE);
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
		  loc = gfc_get_location (&code->loc);
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
      tmp = build2_loc (loc, OMP_SINGLE, void_type_node, tmp,
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
	  tree bindblock = poplevel (1, 0);
	  stmt = build3_v (BIND_EXPR, NULL, stmt, bindblock);
	}
      else
	poplevel (0, 0);
    }
  else
    poplevel (0, 0);

  if (IS_EMPTY_STMT (stmt) && !clauses->nowait)
    stmt = gfc_trans_omp_barrier ();

  ompws_flags = 0;
  return stmt;
}

tree
gfc_trans_oacc_declare (gfc_code *code)
{
  stmtblock_t block;
  tree stmt, oacc_clauses;
  enum tree_code construct_code;

  construct_code = OACC_DATA;

  gfc_start_block (&block);

  oacc_clauses = gfc_trans_omp_clauses (&block, code->ext.oacc_declare->clauses,
					code->loc, false, true);
  stmt = gfc_trans_omp_code (code->block->next, true);
  stmt = build2_loc (input_location, construct_code, void_type_node, stmt,
		     oacc_clauses);
  gfc_add_expr_to_block (&block, stmt);

  return gfc_finish_block (&block);
}

tree
gfc_trans_oacc_directive (gfc_code *code)
{
  switch (code->op)
    {
    case EXEC_OACC_PARALLEL_LOOP:
    case EXEC_OACC_KERNELS_LOOP:
    case EXEC_OACC_SERIAL_LOOP:
      return gfc_trans_oacc_combined_directive (code);
    case EXEC_OACC_PARALLEL:
    case EXEC_OACC_KERNELS:
    case EXEC_OACC_SERIAL:
    case EXEC_OACC_DATA:
    case EXEC_OACC_HOST_DATA:
      return gfc_trans_oacc_construct (code);
    case EXEC_OACC_LOOP:
      return gfc_trans_omp_do (code, code->op, NULL, code->ext.omp_clauses,
			       NULL);
    case EXEC_OACC_UPDATE:
    case EXEC_OACC_CACHE:
    case EXEC_OACC_ENTER_DATA:
    case EXEC_OACC_EXIT_DATA:
      return gfc_trans_oacc_executable_directive (code);
    case EXEC_OACC_WAIT:
      return gfc_trans_oacc_wait_directive (code);
    case EXEC_OACC_ATOMIC:
      return gfc_trans_omp_atomic (code);
    case EXEC_OACC_DECLARE:
      return gfc_trans_oacc_declare (code);
    default:
      gcc_unreachable ();
    }
}

tree
gfc_trans_omp_directive (gfc_code *code)
{
  switch (code->op)
    {
    case EXEC_OMP_ALLOCATE:
    case EXEC_OMP_ALLOCATORS:
      return gfc_trans_omp_allocators (code);
    case EXEC_OMP_ASSUME:
      return gfc_trans_omp_assume (code);
    case EXEC_OMP_ATOMIC:
      return gfc_trans_omp_atomic (code);
    case EXEC_OMP_BARRIER:
      return gfc_trans_omp_barrier ();
    case EXEC_OMP_CANCEL:
      return gfc_trans_omp_cancel (code);
    case EXEC_OMP_CANCELLATION_POINT:
      return gfc_trans_omp_cancellation_point (code);
    case EXEC_OMP_CRITICAL:
      return gfc_trans_omp_critical (code);
    case EXEC_OMP_DEPOBJ:
      return gfc_trans_omp_depobj (code);
    case EXEC_OMP_DISTRIBUTE:
    case EXEC_OMP_DO:
    case EXEC_OMP_LOOP:
    case EXEC_OMP_SIMD:
    case EXEC_OMP_TASKLOOP:
    case EXEC_OMP_TILE:
    case EXEC_OMP_UNROLL:
      return gfc_trans_omp_do (code, code->op, NULL, code->ext.omp_clauses,
			       NULL);
    case EXEC_OMP_DISPATCH:
      return gfc_trans_omp_dispatch (code);
    case EXEC_OMP_DISTRIBUTE_PARALLEL_DO:
    case EXEC_OMP_DISTRIBUTE_PARALLEL_DO_SIMD:
    case EXEC_OMP_DISTRIBUTE_SIMD:
      return gfc_trans_omp_distribute (code, NULL);
    case EXEC_OMP_DO_SIMD:
      return gfc_trans_omp_do_simd (code, NULL, NULL, NULL_TREE);
    case EXEC_OMP_ERROR:
      return gfc_trans_omp_error (code);
    case EXEC_OMP_FLUSH:
      return gfc_trans_omp_flush (code);
    case EXEC_OMP_MASKED:
      return gfc_trans_omp_masked (code, NULL);
    case EXEC_OMP_MASTER:
      return gfc_trans_omp_master (code);
    case EXEC_OMP_MASKED_TASKLOOP:
    case EXEC_OMP_MASKED_TASKLOOP_SIMD:
    case EXEC_OMP_MASTER_TASKLOOP:
    case EXEC_OMP_MASTER_TASKLOOP_SIMD:
      return gfc_trans_omp_master_masked_taskloop (code, code->op);
    case EXEC_OMP_METADIRECTIVE:
      return gfc_trans_omp_metadirective (code);
    case EXEC_OMP_ORDERED:
      return gfc_trans_omp_ordered (code);
    case EXEC_OMP_PARALLEL:
      return gfc_trans_omp_parallel (code);
    case EXEC_OMP_PARALLEL_DO:
      return gfc_trans_omp_parallel_do (code, false, NULL, NULL);
    case EXEC_OMP_PARALLEL_LOOP:
      return gfc_trans_omp_parallel_do (code, true, NULL, NULL);
    case EXEC_OMP_PARALLEL_DO_SIMD:
      return gfc_trans_omp_parallel_do_simd (code, NULL, NULL);
    case EXEC_OMP_PARALLEL_MASKED:
    case EXEC_OMP_PARALLEL_MASKED_TASKLOOP:
    case EXEC_OMP_PARALLEL_MASKED_TASKLOOP_SIMD:
    case EXEC_OMP_PARALLEL_MASTER:
    case EXEC_OMP_PARALLEL_MASTER_TASKLOOP:
    case EXEC_OMP_PARALLEL_MASTER_TASKLOOP_SIMD:
      return gfc_trans_omp_parallel_master_masked (code);
    case EXEC_OMP_PARALLEL_SECTIONS:
      return gfc_trans_omp_parallel_sections (code);
    case EXEC_OMP_PARALLEL_WORKSHARE:
      return gfc_trans_omp_parallel_workshare (code);
    case EXEC_OMP_SCOPE:
      return gfc_trans_omp_scope (code);
    case EXEC_OMP_SECTIONS:
      return gfc_trans_omp_sections (code, code->ext.omp_clauses);
    case EXEC_OMP_SINGLE:
      return gfc_trans_omp_single (code, code->ext.omp_clauses);
    case EXEC_OMP_TARGET:
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
      return gfc_trans_omp_target (code);
    case EXEC_OMP_TARGET_DATA:
      return gfc_trans_omp_target_data (code);
    case EXEC_OMP_TARGET_ENTER_DATA:
      return gfc_trans_omp_target_enter_data (code);
    case EXEC_OMP_TARGET_EXIT_DATA:
      return gfc_trans_omp_target_exit_data (code);
    case EXEC_OMP_TARGET_UPDATE:
      return gfc_trans_omp_target_update (code);
    case EXEC_OMP_TASK:
      return gfc_trans_omp_task (code);
    case EXEC_OMP_TASKGROUP:
      return gfc_trans_omp_taskgroup (code);
    case EXEC_OMP_TASKLOOP_SIMD:
      return gfc_trans_omp_taskloop (code, code->op);
    case EXEC_OMP_TASKWAIT:
      return gfc_trans_omp_taskwait (code);
    case EXEC_OMP_TASKYIELD:
      return gfc_trans_omp_taskyield ();
    case EXEC_OMP_TEAMS:
    case EXEC_OMP_TEAMS_DISTRIBUTE:
    case EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO:
    case EXEC_OMP_TEAMS_DISTRIBUTE_PARALLEL_DO_SIMD:
    case EXEC_OMP_TEAMS_DISTRIBUTE_SIMD:
    case EXEC_OMP_TEAMS_LOOP:
      return gfc_trans_omp_teams (code, NULL, NULL_TREE);
    case EXEC_OMP_WORKSHARE:
      return gfc_trans_omp_workshare (code, code->ext.omp_clauses);
    case EXEC_OMP_INTEROP:
      return gfc_trans_openmp_interop (code, code->ext.omp_clauses);
    default:
      gcc_unreachable ();
    }
}

void
gfc_trans_omp_declare_simd (gfc_namespace *ns)
{
  if (ns->entries)
    return;

  gfc_omp_declare_simd *ods;
  for (ods = ns->omp_declare_simd; ods; ods = ods->next)
    {
      tree c = gfc_trans_omp_clauses (NULL, ods->clauses, ods->where, true);
      tree fndecl = ns->proc_name->backend_decl;
      if (c != NULL_TREE)
	c = tree_cons (NULL_TREE, c, NULL_TREE);
      c = build_tree_list (get_identifier ("omp declare simd"), c);
      TREE_CHAIN (c) = DECL_ATTRIBUTES (fndecl);
      DECL_ATTRIBUTES (fndecl) = c;
    }
}

/* Translate the context selector list GFC_SELECTORS, using WHERE as the
   locus for error messages.  */

static tree
gfc_trans_omp_set_selector (gfc_omp_set_selector *gfc_selectors, locus where)
{
  tree set_selectors = NULL_TREE;
  gfc_omp_set_selector *oss;

  for (oss = gfc_selectors; oss; oss = oss->next)
    {
      tree selectors = NULL_TREE;
      gfc_omp_selector *os;
      enum omp_tss_code set = oss->code;
      gcc_assert (set != OMP_TRAIT_SET_INVALID);

      for (os = oss->trait_selectors; os; os = os->next)
	{
	  tree scoreval = NULL_TREE;
	  tree properties = NULL_TREE;
	  gfc_omp_trait_property *otp;
	  enum omp_ts_code sel = os->code;

	  /* Per the spec, "Implementations can ignore specified
	     selectors that are not those described in this section";
	     however, we  must record such selectors because they
	     cause match failures.  */
	  if (sel == OMP_TRAIT_INVALID)
	    {
	      selectors = make_trait_selector (sel, NULL_TREE, NULL_TREE,
					       selectors);
	      continue;
	    }

	  for (otp = os->properties; otp; otp = otp->next)
	    {
	      switch (otp->property_kind)
		{
		case OMP_TRAIT_PROPERTY_DEV_NUM_EXPR:
		case OMP_TRAIT_PROPERTY_BOOL_EXPR:
		  {
		    tree expr = NULL_TREE;
		    gfc_se se;
		    gfc_init_se (&se, NULL);
		    gfc_conv_expr (&se, otp->expr);
		    expr = se.expr;
		    properties = make_trait_property (NULL_TREE, expr,
						      properties);
		  }
		  break;
		case OMP_TRAIT_PROPERTY_ID:
		  properties
		    = make_trait_property (get_identifier (otp->name),
					   NULL_TREE, properties);
		  break;
		case OMP_TRAIT_PROPERTY_NAME_LIST:
		  {
		    tree prop = OMP_TP_NAMELIST_NODE;
		    tree value = NULL_TREE;
		    if (otp->is_name)
		      value = get_identifier (otp->name);
		    else
		      value = gfc_conv_constant_to_tree (otp->expr);

		    properties = make_trait_property (prop, value,
						      properties);
		  }
		  break;
		case OMP_TRAIT_PROPERTY_CLAUSE_LIST:
		  properties = gfc_trans_omp_clauses (NULL, otp->clauses,
						      where, true);
		  break;
		default:
		  gcc_unreachable ();
		}
	    }

	  if (os->score)
	    {
	      gfc_se se;
	      gfc_init_se (&se, NULL);
	      gfc_conv_expr (&se, os->score);
	      scoreval = se.expr;
	    }

	  selectors = make_trait_selector (sel, scoreval,
					   properties, selectors);
	}
      set_selectors = make_trait_set_selector (set, selectors, set_selectors);
    }
  return set_selectors;
}


void
gfc_trans_omp_declare_variant (gfc_namespace *ns)
{
  tree base_fn_decl = ns->proc_name->backend_decl;
  gfc_namespace *search_ns = ns;
  gfc_omp_declare_variant *next;

  for (gfc_omp_declare_variant *odv = search_ns->omp_declare_variant;
       search_ns; odv = next)
    {
      /* Look in the parent namespace if there are no more directives in the
	 current namespace.  */
      if (!odv)
	{
	  search_ns = search_ns->parent;
	  if (search_ns)
	    next = search_ns->omp_declare_variant;
	  continue;
	}

      next = odv->next;

      if (odv->error_p)
	continue;

      /* Check directive the first time it is encountered.  */
      bool error_found = true;

      if (odv->checked_p)
	error_found = false;
      if (odv->base_proc_symtree == NULL)
	{
	  if (!search_ns->proc_name->attr.function
	      && !search_ns->proc_name->attr.subroutine)
	    gfc_error ("The base name for %<declare variant%> must be "
		       "specified at %L", &odv->where);
	  else
	    error_found = false;
	}
      else
	{
	  if (!search_ns->contained
	      && strcmp (odv->base_proc_symtree->name,
			 ns->proc_name->name))
	    gfc_error ("The base name at %L does not match the name of the "
		       "current procedure", &odv->where);
	  else if (odv->base_proc_symtree->n.sym->attr.entry)
	    gfc_error ("The base name at %L must not be an entry name",
			&odv->where);
	  else if (odv->base_proc_symtree->n.sym->attr.generic)
	    gfc_error ("The base name at %L must not be a generic name",
			&odv->where);
	  else if (odv->base_proc_symtree->n.sym->attr.proc_pointer)
	    gfc_error ("The base name at %L must not be a procedure pointer",
			&odv->where);
	  else if (odv->base_proc_symtree->n.sym->attr.implicit_type)
	    gfc_error ("The base procedure at %L must have an explicit "
			"interface", &odv->where);
	  else
	    error_found = false;
	}

      odv->checked_p = true;
      if (error_found)
	{
	  odv->error_p = true;
	  continue;
	}

      /* Ignore directives that do not apply to the current procedure.  */
      if ((odv->base_proc_symtree == NULL && search_ns != ns)
	  || (odv->base_proc_symtree != NULL
	      && strcmp (odv->base_proc_symtree->name, ns->proc_name->name)))
	continue;

      tree set_selectors = gfc_trans_omp_set_selector (odv->set_selectors,
						       odv->where);
      const char *variant_proc_name = odv->variant_proc_symtree->name;
      gfc_symbol *variant_proc_sym = odv->variant_proc_symtree->n.sym;
      if (variant_proc_sym == NULL || variant_proc_sym->attr.implicit_type)
	{
	  gfc_symtree *proc_st;
	  gfc_find_sym_tree (variant_proc_name, gfc_current_ns, 1, &proc_st);
	  variant_proc_sym = proc_st->n.sym;
	}
      if (variant_proc_sym == NULL)
	{
	  gfc_error ("Cannot find symbol %qs", variant_proc_name);
	  continue;
	}
      set_selectors = omp_check_context_selector
	(gfc_get_location (&odv->where), set_selectors,
	 OMP_CTX_DECLARE_VARIANT);
      if (set_selectors != error_mark_node)
	{
	  if (!variant_proc_sym->attr.implicit_type
	      && !variant_proc_sym->attr.subroutine
	      && !variant_proc_sym->attr.function)
	    {
	      gfc_error ("variant %qs at %L is not a function or subroutine",
			 variant_proc_name, &odv->where);
	      variant_proc_sym = NULL;
	    }
	  else if (omp_get_context_selector (set_selectors,
					     OMP_TRAIT_SET_CONSTRUCT,
					     OMP_TRAIT_CONSTRUCT_SIMD)
		   == NULL_TREE)
	    {
	      char err[256];
	      gfc_formal_arglist *last_arg = NULL, *extra_arg = NULL;
	      int nappend_args = 0;
	      if (odv->append_args_list)
		{
		  gfc_formal_arglist *arg;
		  int nargs = 0;
		  for (arg = gfc_sym_get_dummy_args (ns->proc_name);
		       arg; arg = arg->next)
		    nargs++;

		  last_arg = gfc_sym_get_dummy_args (variant_proc_sym);
		  for (int i = 1 ; i < nargs && last_arg; i++)
		    last_arg = last_arg->next;
		  if (nargs == 0)
		    {
		      extra_arg = last_arg;
		      last_arg = NULL;
		      variant_proc_sym->formal = NULL;
		    }
		  else if (last_arg)
		    {
		      extra_arg = last_arg->next;
		      last_arg->next = NULL;
		    }
		  for (gfc_omp_namelist *n = odv->append_args_list; n != NULL;
		       n = n->next)
		    nappend_args++;
		}
	      if (!gfc_compare_interfaces (ns->proc_name, variant_proc_sym,
					   variant_proc_sym->name, 0, 1,
					   err, sizeof (err), NULL, NULL))
		{
		  gfc_error ("variant %qs and base %qs at %L have "
			     "incompatible types: %s",
			     variant_proc_name, ns->proc_name->name,
			     &odv->where, err);
		  if (nappend_args)
		    inform (gfc_get_location (&odv->append_args_list->where),
			    "%<append_args%> clause implies that %qs has %d "
			    "dummy arguments of integer type with "
			    "%<omp_interop_kind%> kind", variant_proc_name,
			    nappend_args);
		  variant_proc_sym = NULL;
		}
	      if (last_arg)
		last_arg->next = extra_arg;
	      else if (extra_arg)
		variant_proc_sym->formal = extra_arg;
	      locus *loc = (odv->append_args_list
			    ? &odv->append_args_list->where :  &odv->where);
	      int nextra_arg = 0;
	      for (; extra_arg; extra_arg = extra_arg->next)
		{
		  nextra_arg++;
		  if (!variant_proc_sym)
		    continue;
		  if (extra_arg->sym->ts.type != BT_INTEGER
		      || extra_arg->sym->ts.kind != gfc_index_integer_kind
		      || extra_arg->sym->attr.dimension
		      || extra_arg->sym->attr.codimension
		      || extra_arg->sym->attr.pointer
		      || extra_arg->sym->attr.allocatable
		      || extra_arg->sym->attr.proc_pointer)
		    {
		      gfc_error ("%qs at %L must be a nonpointer, "
				 "nonallocatable scalar integer dummy argument "
				 "of %<omp_interop_kind%> kind as it utilized "
				 "with the %<append_args%> clause at %L",
				 extra_arg->sym->name,
				 &extra_arg->sym->declared_at, loc);
		      variant_proc_sym = NULL;
		    }
		  if (extra_arg->sym->attr.optional)
		    {
		      gfc_error ("%qs at %L with OPTIONAL attribute "
				 "not support when utilized with the "
				 "%<append_args%> clause at %L",
				 extra_arg->sym->name,
				 &extra_arg->sym->declared_at, loc);
		      variant_proc_sym = NULL;
		    }
		}
	      if (variant_proc_sym && nappend_args != nextra_arg)
		{
		  gfc_error ("%qs at %L has %d but requires %d "
			     "%<omp_interop_kind%> kind dummy arguments as it "
			     "is utilized with the %<append_args%> clause at "
			     "%L", variant_proc_sym->name,
			     &variant_proc_sym->declared_at, nextra_arg,
			     nappend_args, loc);
		  variant_proc_sym = NULL;
		}
	    }
	  if ((odv->adjust_args_list != NULL || odv->append_args_list != NULL)
	      && omp_get_context_selector (set_selectors,
					   OMP_TRAIT_SET_CONSTRUCT,
					   OMP_TRAIT_CONSTRUCT_DISPATCH)
		   == NULL_TREE)
	    {
	      gfc_error ("the %qs clause can only be specified if "
			 "the %<dispatch%> selector of the construct "
			 "selector set appears in the %<match%> clause at %L",
			 odv->adjust_args_list ? "adjust_args" : "append_args",
			 &odv->where);
	      variant_proc_sym = NULL;
	    }
	  if (variant_proc_sym != NULL)
	    {
	      gfc_set_sym_referenced (variant_proc_sym);
	      tree construct
		= omp_get_context_selector_list (set_selectors,
						 OMP_TRAIT_SET_CONSTRUCT);
	      omp_mark_declare_variant (gfc_get_location (&odv->where),
					gfc_get_symbol_decl (variant_proc_sym),
					construct);
	      if (omp_context_selector_matches (set_selectors,
						NULL_TREE, false))
		{
		  tree need_device_ptr_list = NULL_TREE;
		  tree need_device_addr_list = NULL_TREE;
		  tree append_args_tree = NULL_TREE;
		  tree id = get_identifier ("omp declare variant base");
		  tree variant = gfc_get_symbol_decl (variant_proc_sym);
		  DECL_ATTRIBUTES (base_fn_decl)
		    = tree_cons (id, build_tree_list (variant, set_selectors),
				 DECL_ATTRIBUTES (base_fn_decl));
		  int arg_idx_offset = 0;
		  if (gfc_return_by_reference (ns->proc_name))
		    {
		      arg_idx_offset++;
		      if (ns->proc_name->ts.type == BT_CHARACTER)
			arg_idx_offset++;
		    }
		  int nargs = 0;
		  for (gfc_formal_arglist *arg
			= gfc_sym_get_dummy_args (ns->proc_name);
		       arg; arg = arg->next)
		    nargs++;
		  if (odv->append_args_list)
		    {
		      int append_arg_no = arg_idx_offset + nargs;
		      tree last_arg = NULL_TREE;
		      for (gfc_omp_namelist *n = odv->append_args_list;
			   n != NULL; n = n->next)
			{
			  tree pref = NULL_TREE;
			  if (n->u.init.len)
			    {
			      tree pref = build_string (n->u.init.len,
							n->u2.init_interop);
			      TREE_TYPE (pref) = build_array_type_nelts (
						   unsigned_char_type_node,
						   n->u.init.len);
			    }
			  /* Save location, (target + target sync) and
			     prefer_type list in a tree list.  */
			  tree t = build_tree_list (n->u.init.target
						    ? boolean_true_node
						    : boolean_false_node,
						    n->u.init.targetsync
						    ? boolean_true_node
						    : boolean_false_node);
			  t = build1_loc (gfc_get_location (&n->where),
					  NOP_EXPR, void_type_node, t);
			  t = build_tree_list (t, pref);
			  if (append_args_tree)
			    {
			      TREE_CHAIN (last_arg) = t;
			      last_arg = t;
			    }
			  else
			    append_args_tree = last_arg = t;
			}
		      /* Store as 'purpose' = arg number to be used for inserting
			 and 'value' = list of interop items.  */
		      append_args_tree = build_tree_list (
					   build_int_cst (integer_type_node,
							  append_arg_no),
					   append_args_tree);
		    }
		  vec<gfc_symbol *> adjust_args_list = vNULL;
		  for (gfc_omp_namelist *arg_list = odv->adjust_args_list;
		       arg_list != NULL; arg_list = arg_list->next)
		    {
		      int from, to;
		      if (arg_list->expr == NULL || arg_list->sym)
			from = ((arg_list->u.adj_args.omp_num_args_minus
				 || arg_list->u.adj_args.omp_num_args_plus)
				? nargs : 1);
		      else
			{
			  if (arg_list->u.adj_args.omp_num_args_plus)
			    mpz_add_ui (arg_list->expr->value.integer,
					arg_list->expr->value.integer, nargs);
			  if (arg_list->u.adj_args.omp_num_args_minus)
			    mpz_ui_sub (arg_list->expr->value.integer, nargs,
					arg_list->expr->value.integer);
			  if (mpz_sgn (arg_list->expr->value.integer) <= 0)
			    {
			      gfc_warning (OPT_Wopenmp,
					   "Expected positive argument index "
					   "at %L", &arg_list->where);
			      from = 1;
			    }
			  else
			    from
			      = (mpz_fits_sint_p (arg_list->expr->value.integer)
				 ? mpz_get_si (arg_list->expr->value.integer)
				 : INT_MAX);
			  if (from > nargs)
			    gfc_warning (OPT_Wopenmp,
					 "Argument index at %L exceeds number "
					 "of arguments %d", &arg_list->where,
					 nargs);
			}
		      locus loc = arg_list->where;
		      if (!arg_list->u.adj_args.range_start)
			to = from;
		      else
			{
			  loc = gfc_get_location_range (&arg_list->where, 0,
							&arg_list->where, 0,
							&arg_list->next->where);
			  if (arg_list->next->expr == NULL)
			    to = nargs;
			  else
			    {
			      if (arg_list->next->u.adj_args.omp_num_args_plus)
				mpz_add_ui (arg_list->next->expr->value.integer,
					    arg_list->next->expr->value.integer,
					    nargs);
			      if (arg_list->next->u.adj_args.omp_num_args_minus)
				mpz_ui_sub (arg_list->next->expr->value.integer,
					    nargs,
					    arg_list->next->expr->value.integer);
			      if (mpz_sgn (arg_list->next->expr->value.integer)
				  <= 0)
				{
				  gfc_warning (OPT_Wopenmp,
					       "Expected positive argument "
					       "index at %L", &loc);
				  to = 0;
				}
			      else
				to = mpz_get_si (
				       arg_list->next->expr->value.integer);
			    }
			  if (from > to && to != 0)
			    gfc_warning (OPT_Wopenmp,
					 "Upper argument index smaller than "
					 "lower one at %L", &loc);
			  if (to > nargs)
			    to = nargs;
			  arg_list = arg_list->next;
			}
		      if (from > nargs)
			continue;
		      /* Change to zero based index.  */
		      from--; to--;
		      gfc_formal_arglist *arg = ns->proc_name->formal;
		      if (!arg_list->sym && to >= from)
			for (int idx = 0; idx < from; idx++)
			  arg = arg->next;
		      for (int idx = from; idx <= to; idx++)
			{
			  if (idx > from)
			    arg = arg->next;
			  if (arg_list->sym)
			    {
			      for (arg = ns->proc_name->formal, idx = 0;
				   arg != NULL; arg = arg->next, idx++)
				if (arg->sym == arg_list->sym)
				  break;
			      if (!arg || !arg_list->sym->attr.dummy)
				{
				  gfc_error ("List item %qs at %L, declared at "
					     "%L, is not a dummy argument",
					     arg_list->sym->name, &loc,
					     &arg_list->sym->declared_at);
				  continue;
				}
			    }
			  if (arg_list->u.adj_args.need_ptr
			      && (arg->sym->ts.f90_type != BT_VOID
				  || !arg->sym->ts.u.derived->ts.is_iso_c
				  || (arg->sym->ts.u.derived->intmod_sym_id
				      != ISOCBINDING_PTR)
				  || arg->sym->attr.dimension))
			    {
			      gfc_error ("Argument %qs at %L to list item in "
					 "%<need_device_ptr%> at %L must be a "
					 "scalar of TYPE(C_PTR)",
					 arg->sym->name,
					 &arg->sym->declared_at, &loc);
			      if (!arg->sym->attr.value)
				inform (gfc_get_location (&loc),
					"Consider using %<need_device_addr%> "
					"instead");
			      continue;
			    }
			  if (arg_list->u.adj_args.need_addr
			      && arg->sym->attr.value)
			    {
			      gfc_error ("Argument %qs at %L to list item in "
					 "%<need_device_addr%> at %L must not "
					 "have the VALUE attribute",
					 arg->sym->name,
					 &arg->sym->declared_at, &loc);
			      continue;
			    }
			  if (adjust_args_list.contains (arg->sym))
			    {
			      gfc_error ("%qs at %L is specified more than "
					 "once", arg->sym->name, &loc);
			      continue;
			    }
			  adjust_args_list.safe_push (arg->sym);

			  if (arg_list->u.adj_args.need_addr)
			    {
			      /* TODO: Has to to support OPTIONAL and array
				 descriptors; should check for CLASS, coarrays?
				 Reject "abc" and 123 as actual arguments (in
				 gimplify.cc or in the FE? Reject noncontiguous
				 actuals?  Cf. also PR C++/118859.
				 Also check array-valued type(c_ptr).  */
			      static bool warned = false;
			      if (!warned)
				sorry_at (gfc_get_location (&loc),
					  "%<need_device_addr%> not yet "
					  "supported");
			      warned = true;
			      continue;
			    }
			  if (arg_list->u.adj_args.need_ptr
			      || arg_list->u.adj_args.need_addr)
			    {
			      // Store 0-based argument index,
			      // as in gimplify_call_expr
			      tree t
				= build_tree_list (
				    NULL_TREE,
				    build_int_cst (integer_type_node,
						   idx + arg_idx_offset));
			      if (arg_list->u.adj_args.need_ptr)
				need_device_ptr_list
				  = chainon (need_device_ptr_list, t);
			      else
				need_device_addr_list
				  = chainon (need_device_addr_list, t);
			    }
			}
		    }
		  tree t = NULL_TREE;
		  if (need_device_ptr_list
		      || need_device_addr_list
		      || append_args_tree)
		    {
		      t = build_tree_list (need_device_ptr_list,
					   need_device_addr_list),
		      TREE_CHAIN (t) = append_args_tree;
		      DECL_ATTRIBUTES (variant) = tree_cons (
			get_identifier ("omp declare variant variant args"), t,
			DECL_ATTRIBUTES (variant));
		    }
		}
	    }
	}
    }
}

/* Add ptr for tracking as being allocated by GOMP_alloc. */

tree
gfc_omp_call_add_alloc (tree ptr)
{
  static tree fn = NULL_TREE;
  if (fn == NULL_TREE)
    {
      fn = build_function_type_list (void_type_node, ptr_type_node, NULL_TREE);
      tree att = build_tree_list (NULL_TREE, build_string (4, ". R "));
      att = tree_cons (get_identifier ("fn spec"), att, TYPE_ATTRIBUTES (fn));
      fn = build_type_attribute_variant (fn, att);
      fn = build_fn_decl ("GOMP_add_alloc", fn);
    }
  return build_call_expr_loc (input_location, fn, 1, ptr);
}

/* Generated function returns true when it was tracked via GOMP_add_alloc and
   removes it from the tracking.  As called just before GOMP_free or omp_realloc
   the pointer is or might become invalid, thus, it is always removed. */

tree
gfc_omp_call_is_alloc (tree ptr)
{
  static tree fn = NULL_TREE;
  if (fn == NULL_TREE)
    {
      fn = build_function_type_list (boolean_type_node, ptr_type_node,
				     NULL_TREE);
      tree att = build_tree_list (NULL_TREE, build_string (4, ". R "));
      att = tree_cons (get_identifier ("fn spec"), att, TYPE_ATTRIBUTES (fn));
      fn = build_type_attribute_variant (fn, att);
      fn = build_fn_decl ("GOMP_is_alloc", fn);
    }
  return build_call_expr_loc (input_location, fn, 1, ptr);
}

tree
gfc_trans_omp_metadirective (gfc_code *code)
{
  gfc_omp_variant *variant = code->ext.omp_variants;

  tree metadirective_tree = make_node (OMP_METADIRECTIVE);
  SET_EXPR_LOCATION (metadirective_tree, gfc_get_location (&code->loc));
  TREE_TYPE (metadirective_tree) = void_type_node;
  OMP_METADIRECTIVE_VARIANTS (metadirective_tree) = NULL_TREE;

  tree tree_body = NULL_TREE;

  while (variant)
    {
      tree ctx = gfc_trans_omp_set_selector (variant->selectors,
					     variant->where);
      ctx = omp_check_context_selector (gfc_get_location (&variant->where),
					ctx, OMP_CTX_METADIRECTIVE);
      if (ctx == error_mark_node)
	return error_mark_node;

      /* If the selector doesn't match, drop the whole variant.  */
      if (!omp_context_selector_matches (ctx, NULL_TREE, false))
	{
	  variant = variant->next;
	  continue;
	}

      gfc_code *next_code = variant->code->next;
      if (next_code && tree_body == NULL_TREE)
	tree_body = gfc_trans_code (next_code);

      if (next_code)
	variant->code->next = NULL;
      tree directive = gfc_trans_code (variant->code);
      if (next_code)
	variant->code->next = next_code;

      tree body = next_code ? tree_body : NULL_TREE;
      tree omp_variant = make_omp_metadirective_variant (ctx, directive, body);
      OMP_METADIRECTIVE_VARIANTS (metadirective_tree)
	= chainon (OMP_METADIRECTIVE_VARIANTS (metadirective_tree),
		   omp_variant);
      variant = variant->next;
    }

  /* TODO: Resolve the metadirective here if possible.   */

  return metadirective_tree;
}
