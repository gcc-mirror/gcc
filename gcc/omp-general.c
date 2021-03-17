/* General types and functions that are uselful for processing of OpenMP,
   OpenACC and similar directivers at various stages of compilation.

   Copyright (C) 2005-2021 Free Software Foundation, Inc.

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

/* Find an OMP clause of type KIND within CLAUSES.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "tree.h"
#include "gimple.h"
#include "ssa.h"
#include "diagnostic-core.h"
#include "fold-const.h"
#include "langhooks.h"
#include "omp-general.h"
#include "stringpool.h"
#include "attribs.h"
#include "gimplify.h"
#include "cgraph.h"
#include "alloc-pool.h"
#include "symbol-summary.h"
#include "tree-pass.h"
#include "omp-device-properties.h"
#include "tree-iterator.h"
#include "data-streamer.h"
#include "streamer-hooks.h"

enum omp_requires omp_requires_mask;

tree
omp_find_clause (tree clauses, enum omp_clause_code kind)
{
  for (; clauses ; clauses = OMP_CLAUSE_CHAIN (clauses))
    if (OMP_CLAUSE_CODE (clauses) == kind)
      return clauses;

  return NULL_TREE;
}

/* True if OpenMP should regard this DECL as being a scalar which has Fortran's
   allocatable or pointer attribute.  */
bool
omp_is_allocatable_or_ptr (tree decl)
{
  return lang_hooks.decls.omp_is_allocatable_or_ptr (decl);
}

/* Check whether this DECL belongs to a Fortran optional argument.
   With 'for_present_check' set to false, decls which are optional parameters
   themselve are returned as tree - or a NULL_TREE otherwise. Those decls are
   always pointers.  With 'for_present_check' set to true, the decl for checking
   whether an argument is present is returned; for arguments with value
   attribute this is the hidden argument and of BOOLEAN_TYPE.  If the decl is
   unrelated to optional arguments, NULL_TREE is returned.  */

tree
omp_check_optional_argument (tree decl, bool for_present_check)
{
  return lang_hooks.decls.omp_check_optional_argument (decl, for_present_check);
}

/* Return true if DECL is a reference type.  */

bool
omp_is_reference (tree decl)
{
  return lang_hooks.decls.omp_privatize_by_reference (decl);
}

/* Adjust *COND_CODE and *N2 so that the former is either LT_EXPR or GT_EXPR,
   given that V is the loop index variable and STEP is loop step. */

void
omp_adjust_for_condition (location_t loc, enum tree_code *cond_code, tree *n2,
			  tree v, tree step)
{
  switch (*cond_code)
    {
    case LT_EXPR:
    case GT_EXPR:
      break;

    case NE_EXPR:
      gcc_assert (TREE_CODE (step) == INTEGER_CST);
      if (TREE_CODE (TREE_TYPE (v)) == INTEGER_TYPE)
	{
	  if (integer_onep (step))
	    *cond_code = LT_EXPR;
	  else
	    {
	      gcc_assert (integer_minus_onep (step));
	      *cond_code = GT_EXPR;
	    }
	}
      else
	{
	  tree unit = TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (v)));
	  gcc_assert (TREE_CODE (unit) == INTEGER_CST);
	  if (tree_int_cst_equal (unit, step))
	    *cond_code = LT_EXPR;
	  else
	    {
	      gcc_assert (wi::neg (wi::to_widest (unit))
			  == wi::to_widest (step));
	      *cond_code = GT_EXPR;
	    }
	}

      break;

    case LE_EXPR:
      if (POINTER_TYPE_P (TREE_TYPE (*n2)))
	*n2 = fold_build_pointer_plus_hwi_loc (loc, *n2, 1);
      else
	*n2 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (*n2), *n2,
			       build_int_cst (TREE_TYPE (*n2), 1));
      *cond_code = LT_EXPR;
      break;
    case GE_EXPR:
      if (POINTER_TYPE_P (TREE_TYPE (*n2)))
	*n2 = fold_build_pointer_plus_hwi_loc (loc, *n2, -1);
      else
	*n2 = fold_build2_loc (loc, MINUS_EXPR, TREE_TYPE (*n2), *n2,
			       build_int_cst (TREE_TYPE (*n2), 1));
      *cond_code = GT_EXPR;
      break;
    default:
      gcc_unreachable ();
    }
}

/* Return the looping step from INCR, extracted from the step of a gimple omp
   for statement.  */

tree
omp_get_for_step_from_incr (location_t loc, tree incr)
{
  tree step;
  switch (TREE_CODE (incr))
    {
    case PLUS_EXPR:
      step = TREE_OPERAND (incr, 1);
      break;
    case POINTER_PLUS_EXPR:
      step = fold_convert (ssizetype, TREE_OPERAND (incr, 1));
      break;
    case MINUS_EXPR:
      step = TREE_OPERAND (incr, 1);
      step = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (step), step);
      break;
    default:
      gcc_unreachable ();
    }
  return step;
}

/* Extract the header elements of parallel loop FOR_STMT and store
   them into *FD.  */

void
omp_extract_for_data (gomp_for *for_stmt, struct omp_for_data *fd,
		      struct omp_for_data_loop *loops)
{
  tree t, var, *collapse_iter, *collapse_count;
  tree count = NULL_TREE, iter_type = long_integer_type_node;
  struct omp_for_data_loop *loop;
  int i;
  struct omp_for_data_loop dummy_loop;
  location_t loc = gimple_location (for_stmt);
  bool simd = gimple_omp_for_kind (for_stmt) == GF_OMP_FOR_KIND_SIMD;
  bool distribute = gimple_omp_for_kind (for_stmt)
		    == GF_OMP_FOR_KIND_DISTRIBUTE;
  bool taskloop = gimple_omp_for_kind (for_stmt)
		  == GF_OMP_FOR_KIND_TASKLOOP;
  tree iterv, countv;

  fd->for_stmt = for_stmt;
  fd->pre = NULL;
  fd->have_nowait = distribute || simd;
  fd->have_ordered = false;
  fd->have_reductemp = false;
  fd->have_pointer_condtemp = false;
  fd->have_scantemp = false;
  fd->have_nonctrl_scantemp = false;
  fd->non_rect = false;
  fd->lastprivate_conditional = 0;
  fd->tiling = NULL_TREE;
  fd->collapse = 1;
  fd->ordered = 0;
  fd->first_nonrect = -1;
  fd->last_nonrect = -1;
  fd->sched_kind = OMP_CLAUSE_SCHEDULE_STATIC;
  fd->sched_modifiers = 0;
  fd->chunk_size = NULL_TREE;
  fd->simd_schedule = false;
  fd->first_inner_iterations = NULL_TREE;
  fd->factor = NULL_TREE;
  fd->adjn1 = NULL_TREE;
  collapse_iter = NULL;
  collapse_count = NULL;

  for (t = gimple_omp_for_clauses (for_stmt); t ; t = OMP_CLAUSE_CHAIN (t))
    switch (OMP_CLAUSE_CODE (t))
      {
      case OMP_CLAUSE_NOWAIT:
	fd->have_nowait = true;
	break;
      case OMP_CLAUSE_ORDERED:
	fd->have_ordered = true;
	if (OMP_CLAUSE_ORDERED_EXPR (t))
	  fd->ordered = tree_to_shwi (OMP_CLAUSE_ORDERED_EXPR (t));
	break;
      case OMP_CLAUSE_SCHEDULE:
	gcc_assert (!distribute && !taskloop);
	fd->sched_kind
	  = (enum omp_clause_schedule_kind)
	    (OMP_CLAUSE_SCHEDULE_KIND (t) & OMP_CLAUSE_SCHEDULE_MASK);
	fd->sched_modifiers = (OMP_CLAUSE_SCHEDULE_KIND (t)
			       & ~OMP_CLAUSE_SCHEDULE_MASK);
	fd->chunk_size = OMP_CLAUSE_SCHEDULE_CHUNK_EXPR (t);
	fd->simd_schedule = OMP_CLAUSE_SCHEDULE_SIMD (t);
	break;
      case OMP_CLAUSE_DIST_SCHEDULE:
	gcc_assert (distribute);
	fd->chunk_size = OMP_CLAUSE_DIST_SCHEDULE_CHUNK_EXPR (t);
	break;
      case OMP_CLAUSE_COLLAPSE:
	fd->collapse = tree_to_shwi (OMP_CLAUSE_COLLAPSE_EXPR (t));
	if (fd->collapse > 1)
	  {
	    collapse_iter = &OMP_CLAUSE_COLLAPSE_ITERVAR (t);
	    collapse_count = &OMP_CLAUSE_COLLAPSE_COUNT (t);
	  }
	break;
      case OMP_CLAUSE_TILE:
	fd->tiling = OMP_CLAUSE_TILE_LIST (t);
	fd->collapse = list_length (fd->tiling);
	gcc_assert (fd->collapse);
	collapse_iter = &OMP_CLAUSE_TILE_ITERVAR (t);
	collapse_count = &OMP_CLAUSE_TILE_COUNT (t);
	break;
      case OMP_CLAUSE__REDUCTEMP_:
	fd->have_reductemp = true;
	break;
      case OMP_CLAUSE_LASTPRIVATE:
	if (OMP_CLAUSE_LASTPRIVATE_CONDITIONAL (t))
	  fd->lastprivate_conditional++;
	break;
      case OMP_CLAUSE__CONDTEMP_:
	if (POINTER_TYPE_P (TREE_TYPE (OMP_CLAUSE_DECL (t))))
	  fd->have_pointer_condtemp = true;
	break;
      case OMP_CLAUSE__SCANTEMP_:
	fd->have_scantemp = true;
	if (!OMP_CLAUSE__SCANTEMP__ALLOC (t)
	    && !OMP_CLAUSE__SCANTEMP__CONTROL (t))
	  fd->have_nonctrl_scantemp = true;
	break;
      default:
	break;
      }

  if (fd->collapse > 1 || fd->tiling)
    fd->loops = loops;
  else
    fd->loops = &fd->loop;

  if (fd->ordered && fd->collapse == 1 && loops != NULL)
    {
      fd->loops = loops;
      iterv = NULL_TREE;
      countv = NULL_TREE;
      collapse_iter = &iterv;
      collapse_count = &countv;
    }

  /* FIXME: for now map schedule(auto) to schedule(static).
     There should be analysis to determine whether all iterations
     are approximately the same amount of work (then schedule(static)
     is best) or if it varies (then schedule(dynamic,N) is better).  */
  if (fd->sched_kind == OMP_CLAUSE_SCHEDULE_AUTO)
    {
      fd->sched_kind = OMP_CLAUSE_SCHEDULE_STATIC;
      gcc_assert (fd->chunk_size == NULL);
    }
  gcc_assert ((fd->collapse == 1 && !fd->tiling) || collapse_iter != NULL);
  if (taskloop)
    fd->sched_kind = OMP_CLAUSE_SCHEDULE_RUNTIME;
  if (fd->sched_kind == OMP_CLAUSE_SCHEDULE_RUNTIME)
    gcc_assert (fd->chunk_size == NULL);
  else if (fd->chunk_size == NULL)
    {
      /* We only need to compute a default chunk size for ordered
	 static loops and dynamic loops.  */
      if (fd->sched_kind != OMP_CLAUSE_SCHEDULE_STATIC
	  || fd->have_ordered)
	fd->chunk_size = (fd->sched_kind == OMP_CLAUSE_SCHEDULE_STATIC)
			 ? integer_zero_node : integer_one_node;
    }

  int cnt = fd->ordered ? fd->ordered : fd->collapse;
  int single_nonrect = -1;
  tree single_nonrect_count = NULL_TREE;
  enum tree_code single_nonrect_cond_code = ERROR_MARK;
  for (i = 1; i < cnt; i++)
    {
      tree n1 = gimple_omp_for_initial (for_stmt, i);
      tree n2 = gimple_omp_for_final (for_stmt, i);
      if (TREE_CODE (n1) == TREE_VEC)
	{
	  if (fd->non_rect)
	    {
	      single_nonrect = -1;
	      break;
	    }
	  for (int j = i - 1; j >= 0; j--)
	    if (TREE_VEC_ELT (n1, 0) == gimple_omp_for_index (for_stmt, j))
	      {
		single_nonrect = j;
		break;
	      }
	  fd->non_rect = true;
	}
      else if (TREE_CODE (n2) == TREE_VEC)
	{
	  if (fd->non_rect)
	    {
	      single_nonrect = -1;
	      break;
	    }
	  for (int j = i - 1; j >= 0; j--)
	    if (TREE_VEC_ELT (n2, 0) == gimple_omp_for_index (for_stmt, j))
	      {
		single_nonrect = j;
		break;
	      }
	  fd->non_rect = true;
	}
    }
  for (i = 0; i < cnt; i++)
    {
      if (i == 0
	  && fd->collapse == 1
	  && !fd->tiling
	  && (fd->ordered == 0 || loops == NULL))
	loop = &fd->loop;
      else if (loops != NULL)
	loop = loops + i;
      else
	loop = &dummy_loop;

      loop->v = gimple_omp_for_index (for_stmt, i);
      gcc_assert (SSA_VAR_P (loop->v));
      gcc_assert (TREE_CODE (TREE_TYPE (loop->v)) == INTEGER_TYPE
		  || TREE_CODE (TREE_TYPE (loop->v)) == POINTER_TYPE);
      var = TREE_CODE (loop->v) == SSA_NAME ? SSA_NAME_VAR (loop->v) : loop->v;
      loop->n1 = gimple_omp_for_initial (for_stmt, i);
      loop->m1 = NULL_TREE;
      loop->m2 = NULL_TREE;
      loop->outer = 0;
      loop->non_rect_referenced = false;
      if (TREE_CODE (loop->n1) == TREE_VEC)
	{
	  for (int j = i - 1; j >= 0; j--)
	    if (TREE_VEC_ELT (loop->n1, 0) == gimple_omp_for_index (for_stmt, j))
	      {
		loop->outer = i - j;
		if (loops != NULL)
		  loops[j].non_rect_referenced = true;
		if (fd->first_nonrect == -1 || fd->first_nonrect > j)
		  fd->first_nonrect = j;
		break;
	      }
	  gcc_assert (loop->outer);
	  loop->m1 = TREE_VEC_ELT (loop->n1, 1);
	  loop->n1 = TREE_VEC_ELT (loop->n1, 2);
	  fd->non_rect = true;
	  fd->last_nonrect = i;
	}

      loop->cond_code = gimple_omp_for_cond (for_stmt, i);
      loop->n2 = gimple_omp_for_final (for_stmt, i);
      gcc_assert (loop->cond_code != NE_EXPR
		  || (gimple_omp_for_kind (for_stmt)
		      != GF_OMP_FOR_KIND_OACC_LOOP));
      if (TREE_CODE (loop->n2) == TREE_VEC)
	{
	  if (loop->outer)
	    gcc_assert (TREE_VEC_ELT (loop->n2, 0)
			== gimple_omp_for_index (for_stmt, i - loop->outer));
	  else
	    for (int j = i - 1; j >= 0; j--)
	      if (TREE_VEC_ELT (loop->n2, 0) == gimple_omp_for_index (for_stmt, j))
		{
		  loop->outer = i - j;
		  if (loops != NULL)
		    loops[j].non_rect_referenced = true;
		  if (fd->first_nonrect == -1 || fd->first_nonrect > j)
		    fd->first_nonrect = j;
		  break;
		}
	  gcc_assert (loop->outer);
	  loop->m2 = TREE_VEC_ELT (loop->n2, 1);
	  loop->n2 = TREE_VEC_ELT (loop->n2, 2);
	  fd->non_rect = true;
	  fd->last_nonrect = i;
	}

      t = gimple_omp_for_incr (for_stmt, i);
      gcc_assert (TREE_OPERAND (t, 0) == var);
      loop->step = omp_get_for_step_from_incr (loc, t);

      omp_adjust_for_condition (loc, &loop->cond_code, &loop->n2, loop->v,
				loop->step);

      if (simd
	  || (fd->sched_kind == OMP_CLAUSE_SCHEDULE_STATIC
	      && !fd->have_ordered))
	{
	  if (fd->collapse == 1 && !fd->tiling)
	    iter_type = TREE_TYPE (loop->v);
	  else if (i == 0
		   || TYPE_PRECISION (iter_type)
		      < TYPE_PRECISION (TREE_TYPE (loop->v)))
	    iter_type
	      = build_nonstandard_integer_type
		  (TYPE_PRECISION (TREE_TYPE (loop->v)), 1);
	}
      else if (iter_type != long_long_unsigned_type_node)
	{
	  if (POINTER_TYPE_P (TREE_TYPE (loop->v)))
	    iter_type = long_long_unsigned_type_node;
	  else if (TYPE_UNSIGNED (TREE_TYPE (loop->v))
		   && TYPE_PRECISION (TREE_TYPE (loop->v))
		      >= TYPE_PRECISION (iter_type))
	    {
	      tree n;

	      if (loop->cond_code == LT_EXPR)
		n = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (loop->v),
				     loop->n2, loop->step);
	      else
		n = loop->n1;
	      if (loop->m1
		  || loop->m2
		  || TREE_CODE (n) != INTEGER_CST
		  || tree_int_cst_lt (TYPE_MAX_VALUE (iter_type), n))
		iter_type = long_long_unsigned_type_node;
	    }
	  else if (TYPE_PRECISION (TREE_TYPE (loop->v))
		   > TYPE_PRECISION (iter_type))
	    {
	      tree n1, n2;

	      if (loop->cond_code == LT_EXPR)
		{
		  n1 = loop->n1;
		  n2 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (loop->v),
					loop->n2, loop->step);
		}
	      else
		{
		  n1 = fold_build2_loc (loc, MINUS_EXPR, TREE_TYPE (loop->v),
					loop->n2, loop->step);
		  n2 = loop->n1;
		}
	      if (loop->m1
		  || loop->m2
		  || TREE_CODE (n1) != INTEGER_CST
		  || TREE_CODE (n2) != INTEGER_CST
		  || !tree_int_cst_lt (TYPE_MIN_VALUE (iter_type), n1)
		  || !tree_int_cst_lt (n2, TYPE_MAX_VALUE (iter_type)))
		iter_type = long_long_unsigned_type_node;
	    }
	}

      if (i >= fd->collapse)
	continue;

      if (collapse_count && *collapse_count == NULL)
	{
	  if (count && integer_zerop (count))
	    continue;
	  tree n1first = NULL_TREE, n2first = NULL_TREE;
	  tree n1last = NULL_TREE, n2last = NULL_TREE;
	  tree ostep = NULL_TREE;
	  if (loop->m1 || loop->m2)
	    {
	      if (count == NULL_TREE)
		continue;
	      if (single_nonrect == -1
		  || (loop->m1 && TREE_CODE (loop->m1) != INTEGER_CST)
		  || (loop->m2 && TREE_CODE (loop->m2) != INTEGER_CST)
		  || TREE_CODE (loop->n1) != INTEGER_CST
		  || TREE_CODE (loop->n2) != INTEGER_CST
		  || TREE_CODE (loop->step) != INTEGER_CST)
		{
		  count = NULL_TREE;
		  continue;
		}
	      tree var = gimple_omp_for_initial (for_stmt, single_nonrect);
	      tree itype = TREE_TYPE (var);
	      tree first = gimple_omp_for_initial (for_stmt, single_nonrect);
	      t = gimple_omp_for_incr (for_stmt, single_nonrect);
	      ostep = omp_get_for_step_from_incr (loc, t);
	      t = fold_binary (MINUS_EXPR, long_long_unsigned_type_node,
			       single_nonrect_count,
			       build_one_cst (long_long_unsigned_type_node));
	      t = fold_convert (itype, t);
	      first = fold_convert (itype, first);
	      ostep = fold_convert (itype, ostep);
	      tree last = fold_binary (PLUS_EXPR, itype, first,
				       fold_binary (MULT_EXPR, itype, t,
						    ostep));
	      if (TREE_CODE (first) != INTEGER_CST
		  || TREE_CODE (last) != INTEGER_CST)
		{
		  count = NULL_TREE;
		  continue;
		}
	      if (loop->m1)
		{
		  tree m1 = fold_convert (itype, loop->m1);
		  tree n1 = fold_convert (itype, loop->n1);
		  n1first = fold_binary (PLUS_EXPR, itype,
					 fold_binary (MULT_EXPR, itype,
						      first, m1), n1);
		  n1last = fold_binary (PLUS_EXPR, itype,
					fold_binary (MULT_EXPR, itype,
						     last, m1), n1);
		}
	      else
		n1first = n1last = loop->n1;
	      if (loop->m2)
		{
		  tree n2 = fold_convert (itype, loop->n2);
		  tree m2 = fold_convert (itype, loop->m2);
		  n2first = fold_binary (PLUS_EXPR, itype,
					 fold_binary (MULT_EXPR, itype,
						      first, m2), n2);
		  n2last = fold_binary (PLUS_EXPR, itype,
					fold_binary (MULT_EXPR, itype,
						     last, m2), n2);
		}
	      else
		n2first = n2last = loop->n2;
	      n1first = fold_convert (TREE_TYPE (loop->v), n1first);
	      n2first = fold_convert (TREE_TYPE (loop->v), n2first);
	      n1last = fold_convert (TREE_TYPE (loop->v), n1last);
	      n2last = fold_convert (TREE_TYPE (loop->v), n2last);
	      t = fold_binary (loop->cond_code, boolean_type_node,
			       n1first, n2first);
	      tree t2 = fold_binary (loop->cond_code, boolean_type_node,
				     n1last, n2last);
	      if (t && t2 && integer_nonzerop (t) && integer_nonzerop (t2))
		/* All outer loop iterators have at least one inner loop
		   iteration.  Try to compute the count at compile time.  */
		t = NULL_TREE;
	      else if (t && t2 && integer_zerop (t) && integer_zerop (t2))
		/* No iterations of the inner loop.  count will be set to
		   zero cst below.  */;
	      else if (TYPE_UNSIGNED (itype)
		       || t == NULL_TREE
		       || t2 == NULL_TREE
		       || TREE_CODE (t) != INTEGER_CST
		       || TREE_CODE (t2) != INTEGER_CST)
		{
		  /* Punt (for now).  */
		  count = NULL_TREE;
		  continue;
		}
	      else
		{
		  /* Some iterations of the outer loop have zero iterations
		     of the inner loop, while others have at least one.
		     In this case, we need to adjust one of those outer
		     loop bounds.  If ADJ_FIRST, we need to adjust outer n1
		     (first), otherwise outer n2 (last).  */
		  bool adj_first = integer_zerop (t);
		  tree n1 = fold_convert (itype, loop->n1);
		  tree n2 = fold_convert (itype, loop->n2);
		  tree m1 = loop->m1 ? fold_convert (itype, loop->m1)
				     : build_zero_cst (itype);
		  tree m2 = loop->m2 ? fold_convert (itype, loop->m2)
				     : build_zero_cst (itype);
		  t = fold_binary (MINUS_EXPR, itype, n1, n2);
		  t2 = fold_binary (MINUS_EXPR, itype, m2, m1);
		  t = fold_binary (TRUNC_DIV_EXPR, itype, t, t2);
		  t2 = fold_binary (MINUS_EXPR, itype, t, first);
		  t2 = fold_binary (TRUNC_MOD_EXPR, itype, t2, ostep);
		  t = fold_binary (MINUS_EXPR, itype, t, t2);
		  tree n1cur
		    = fold_binary (PLUS_EXPR, itype, n1,
				   fold_binary (MULT_EXPR, itype, m1, t));
		  tree n2cur
		    = fold_binary (PLUS_EXPR, itype, n2,
				   fold_binary (MULT_EXPR, itype, m2, t));
		  t2 = fold_binary (loop->cond_code, boolean_type_node,
				    n1cur, n2cur);
		  tree t3 = fold_binary (MULT_EXPR, itype, m1, ostep);
		  tree t4 = fold_binary (MULT_EXPR, itype, m2, ostep);
		  tree diff;
		  if (adj_first)
		    {
		      tree new_first;
		      if (integer_nonzerop (t2))
			{
			  new_first = t;
			  n1first = n1cur;
			  n2first = n2cur;
			  if (flag_checking)
			    {
			      t3 = fold_binary (MINUS_EXPR, itype, n1cur, t3);
			      t4 = fold_binary (MINUS_EXPR, itype, n2cur, t4);
			      t3 = fold_binary (loop->cond_code,
						boolean_type_node, t3, t4);
			      gcc_assert (integer_zerop (t3));
			    }
			}
		      else
			{
			  t3 = fold_binary (PLUS_EXPR, itype, n1cur, t3);
			  t4 = fold_binary (PLUS_EXPR, itype, n2cur, t4);
			  new_first = fold_binary (PLUS_EXPR, itype, t, ostep);
			  n1first = t3;
			  n2first = t4;
			  if (flag_checking)
			    {
			      t3 = fold_binary (loop->cond_code,
						boolean_type_node, t3, t4);
			      gcc_assert (integer_nonzerop (t3));
			    }
			}
		      diff = fold_binary (MINUS_EXPR, itype, new_first, first);
		      first = new_first;
		      fd->adjn1 = first;
		    }
		  else
		    {
		      tree new_last;
		      if (integer_zerop (t2))
			{
			  t3 = fold_binary (MINUS_EXPR, itype, n1cur, t3);
			  t4 = fold_binary (MINUS_EXPR, itype, n2cur, t4);
			  new_last = fold_binary (MINUS_EXPR, itype, t, ostep);
			  n1last = t3;
			  n2last = t4;
			  if (flag_checking)
			    {
			      t3 = fold_binary (loop->cond_code,
						boolean_type_node, t3, t4);
			      gcc_assert (integer_nonzerop (t3));
			    }
			}
		      else
			{
			  new_last = t;
			  n1last = n1cur;
			  n2last = n2cur;
			  if (flag_checking)
			    {
			      t3 = fold_binary (PLUS_EXPR, itype, n1cur, t3);
			      t4 = fold_binary (PLUS_EXPR, itype, n2cur, t4);
			      t3 = fold_binary (loop->cond_code,
						boolean_type_node, t3, t4);
			      gcc_assert (integer_zerop (t3));
			    }
			}
		      diff = fold_binary (MINUS_EXPR, itype, last, new_last);
		    }
		  if (TYPE_UNSIGNED (itype)
		      && single_nonrect_cond_code == GT_EXPR)
		    diff = fold_binary (TRUNC_DIV_EXPR, itype,
					fold_unary (NEGATE_EXPR, itype, diff),
					fold_unary (NEGATE_EXPR, itype,
						    ostep));
		  else
		    diff = fold_binary (TRUNC_DIV_EXPR, itype, diff, ostep);
		  diff = fold_convert (long_long_unsigned_type_node, diff);
		  single_nonrect_count
		    = fold_binary (MINUS_EXPR, long_long_unsigned_type_node,
				   single_nonrect_count, diff);
		  t = NULL_TREE;
		}
	    }
	  else
	    t = fold_binary (loop->cond_code, boolean_type_node,
			     fold_convert (TREE_TYPE (loop->v), loop->n1),
			     fold_convert (TREE_TYPE (loop->v), loop->n2));
	  if (t && integer_zerop (t))
	    count = build_zero_cst (long_long_unsigned_type_node);
	  else if ((i == 0 || count != NULL_TREE)
		   && TREE_CODE (TREE_TYPE (loop->v)) == INTEGER_TYPE
		   && TREE_CONSTANT (loop->n1)
		   && TREE_CONSTANT (loop->n2)
		   && TREE_CODE (loop->step) == INTEGER_CST)
	    {
	      tree itype = TREE_TYPE (loop->v);

	      if (POINTER_TYPE_P (itype))
		itype = signed_type_for (itype);
	      t = build_int_cst (itype, (loop->cond_code == LT_EXPR ? -1 : 1));
	      t = fold_build2 (PLUS_EXPR, itype,
			       fold_convert (itype, loop->step), t);
	      tree n1 = loop->n1;
	      tree n2 = loop->n2;
	      if (loop->m1 || loop->m2)
		{
		  gcc_assert (single_nonrect != -1);
		  n1 = n1first;
		  n2 = n2first;
		}
	      t = fold_build2 (PLUS_EXPR, itype, t, fold_convert (itype, n2));
	      t = fold_build2 (MINUS_EXPR, itype, t, fold_convert (itype, n1));
	      tree step = fold_convert_loc (loc, itype, loop->step);
	      if (TYPE_UNSIGNED (itype) && loop->cond_code == GT_EXPR)
		t = fold_build2 (TRUNC_DIV_EXPR, itype,
				 fold_build1 (NEGATE_EXPR, itype, t),
				 fold_build1 (NEGATE_EXPR, itype, step));
	      else
		t = fold_build2 (TRUNC_DIV_EXPR, itype, t, step);
	      tree llutype = long_long_unsigned_type_node;
	      t = fold_convert (llutype, t);
	      if (loop->m1 || loop->m2)
		{
		  /* t is number of iterations of inner loop at either first
		     or last value of the outer iterator (the one with fewer
		     iterations).
		     Compute t2 = ((m2 - m1) * ostep) / step
		     and niters = outer_count * t
				  + t2 * ((outer_count - 1) * outer_count / 2)
		   */
		  tree m1 = loop->m1 ? loop->m1 : integer_zero_node;
		  tree m2 = loop->m2 ? loop->m2 : integer_zero_node;
		  m1 = fold_convert (itype, m1);
		  m2 = fold_convert (itype, m2);
		  tree t2 = fold_build2 (MINUS_EXPR, itype, m2, m1);
		  t2 = fold_build2 (MULT_EXPR, itype, t2, ostep);
		  if (TYPE_UNSIGNED (itype) && loop->cond_code == GT_EXPR)
		    t2 = fold_build2 (TRUNC_DIV_EXPR, itype,
				      fold_build1 (NEGATE_EXPR, itype, t2),
				      fold_build1 (NEGATE_EXPR, itype, step));
		  else
		    t2 = fold_build2 (TRUNC_DIV_EXPR, itype, t2, step);
		  t2 = fold_convert (llutype, t2);
		  fd->first_inner_iterations = t;
		  fd->factor = t2;
		  t = fold_build2 (MULT_EXPR, llutype, t,
				   single_nonrect_count);
		  tree t3 = fold_build2 (MINUS_EXPR, llutype,
					 single_nonrect_count,
					 build_one_cst (llutype));
		  t3 = fold_build2 (MULT_EXPR, llutype, t3,
				    single_nonrect_count);
		  t3 = fold_build2 (TRUNC_DIV_EXPR, llutype, t3,
				    build_int_cst (llutype, 2));
		  t2 = fold_build2 (MULT_EXPR, llutype, t2, t3);
		  t = fold_build2 (PLUS_EXPR, llutype, t, t2);
		}
	      if (i == single_nonrect)
		{
		  if (integer_zerop (t) || TREE_CODE (t) != INTEGER_CST)
		    count = t;
		  else
		    {
		      single_nonrect_count = t;
		      single_nonrect_cond_code = loop->cond_code;
		      if (count == NULL_TREE)
			count = build_one_cst (llutype);
		    }
		}
	      else if (count != NULL_TREE)
		count = fold_build2 (MULT_EXPR, llutype, count, t);
	      else
		count = t;
	      if (TREE_CODE (count) != INTEGER_CST)
		count = NULL_TREE;
	    }
	  else if (count && !integer_zerop (count))
	    count = NULL_TREE;
	}
    }

  if (count
      && !simd
      && (fd->sched_kind != OMP_CLAUSE_SCHEDULE_STATIC
	  || fd->have_ordered))
    {
      if (!tree_int_cst_lt (count, TYPE_MAX_VALUE (long_integer_type_node)))
	iter_type = long_long_unsigned_type_node;
      else
	iter_type = long_integer_type_node;
    }
  else if (collapse_iter && *collapse_iter != NULL)
    iter_type = TREE_TYPE (*collapse_iter);
  fd->iter_type = iter_type;
  if (collapse_iter && *collapse_iter == NULL)
    *collapse_iter = create_tmp_var (iter_type, ".iter");
  if (collapse_count && *collapse_count == NULL)
    {
      if (count)
	{
	  *collapse_count = fold_convert_loc (loc, iter_type, count);
	  if (fd->first_inner_iterations && fd->factor)
	    {
	      t = make_tree_vec (4);
	      TREE_VEC_ELT (t, 0) = *collapse_count;
	      TREE_VEC_ELT (t, 1) = fd->first_inner_iterations;
	      TREE_VEC_ELT (t, 2) = fd->factor;
	      TREE_VEC_ELT (t, 3) = fd->adjn1;
	      *collapse_count = t;
	    }
	}
      else
	*collapse_count = create_tmp_var (iter_type, ".count");
    }

  if (fd->collapse > 1 || fd->tiling || (fd->ordered && loops))
    {
      fd->loop.v = *collapse_iter;
      fd->loop.n1 = build_int_cst (TREE_TYPE (fd->loop.v), 0);
      fd->loop.n2 = *collapse_count;
      if (TREE_CODE (fd->loop.n2) == TREE_VEC)
	{
	  gcc_assert (fd->non_rect);
	  fd->first_inner_iterations = TREE_VEC_ELT (fd->loop.n2, 1);
	  fd->factor = TREE_VEC_ELT (fd->loop.n2, 2);
	  fd->adjn1 = TREE_VEC_ELT (fd->loop.n2, 3);
	  fd->loop.n2 = TREE_VEC_ELT (fd->loop.n2, 0);
	}
      fd->loop.step = build_int_cst (TREE_TYPE (fd->loop.v), 1);
      fd->loop.m1 = NULL_TREE;
      fd->loop.m2 = NULL_TREE;
      fd->loop.outer = 0;
      fd->loop.cond_code = LT_EXPR;
    }
  else if (loops)
    loops[0] = fd->loop;
}

/* Build a call to GOMP_barrier.  */

gimple *
omp_build_barrier (tree lhs)
{
  tree fndecl = builtin_decl_explicit (lhs ? BUILT_IN_GOMP_BARRIER_CANCEL
					   : BUILT_IN_GOMP_BARRIER);
  gcall *g = gimple_build_call (fndecl, 0);
  if (lhs)
    gimple_call_set_lhs (g, lhs);
  return g;
}

/* Find OMP_FOR resp. OMP_SIMD with non-NULL OMP_FOR_INIT.  Also, fill in pdata
   array, pdata[0] non-NULL if there is anything non-trivial in between,
   pdata[1] is address of OMP_PARALLEL in between if any, pdata[2] is address
   of OMP_FOR in between if any and pdata[3] is address of the inner
   OMP_FOR/OMP_SIMD.  */

tree
find_combined_omp_for (tree *tp, int *walk_subtrees, void *data)
{
  tree **pdata = (tree **) data;
  *walk_subtrees = 0;
  switch (TREE_CODE (*tp))
    {
    case OMP_FOR:
      if (OMP_FOR_INIT (*tp) != NULL_TREE)
	{
	  pdata[3] = tp;
	  return *tp;
	}
      pdata[2] = tp;
      *walk_subtrees = 1;
      break;
    case OMP_SIMD:
      if (OMP_FOR_INIT (*tp) != NULL_TREE)
	{
	  pdata[3] = tp;
	  return *tp;
	}
      break;
    case BIND_EXPR:
      if (BIND_EXPR_VARS (*tp)
	  || (BIND_EXPR_BLOCK (*tp)
	      && BLOCK_VARS (BIND_EXPR_BLOCK (*tp))))
	pdata[0] = tp;
      *walk_subtrees = 1;
      break;
    case STATEMENT_LIST:
      if (!tsi_one_before_end_p (tsi_start (*tp)))
	pdata[0] = tp;
      *walk_subtrees = 1;
      break;
    case TRY_FINALLY_EXPR:
      pdata[0] = tp;
      *walk_subtrees = 1;
      break;
    case OMP_PARALLEL:
      pdata[1] = tp;
      *walk_subtrees = 1;
      break;
    default:
      break;
    }
  return NULL_TREE;
}

/* Return maximum possible vectorization factor for the target.  */

poly_uint64
omp_max_vf (void)
{
  if (!optimize
      || optimize_debug
      || !flag_tree_loop_optimize
      || (!flag_tree_loop_vectorize
	  && global_options_set.x_flag_tree_loop_vectorize))
    return 1;

  auto_vector_modes modes;
  targetm.vectorize.autovectorize_vector_modes (&modes, true);
  if (!modes.is_empty ())
    {
      poly_uint64 vf = 0;
      for (unsigned int i = 0; i < modes.length (); ++i)
	/* The returned modes use the smallest element size (and thus
	   the largest nunits) for the vectorization approach that they
	   represent.  */
	vf = ordered_max (vf, GET_MODE_NUNITS (modes[i]));
      return vf;
    }

  machine_mode vqimode = targetm.vectorize.preferred_simd_mode (QImode);
  if (GET_MODE_CLASS (vqimode) == MODE_VECTOR_INT)
    return GET_MODE_NUNITS (vqimode);

  return 1;
}

/* Return maximum SIMT width if offloading may target SIMT hardware.  */

int
omp_max_simt_vf (void)
{
  if (!optimize)
    return 0;
  if (ENABLE_OFFLOADING)
    for (const char *c = getenv ("OFFLOAD_TARGET_NAMES"); c;)
      {
	if (startswith (c, "nvptx"))
	  return 32;
	else if ((c = strchr (c, ':')))
	  c++;
      }
  return 0;
}

/* Store the construct selectors as tree codes from last to first,
   return their number.  */

int
omp_constructor_traits_to_codes (tree ctx, enum tree_code *constructs)
{
  int nconstructs = list_length (ctx);
  int i = nconstructs - 1;
  for (tree t2 = ctx; t2; t2 = TREE_CHAIN (t2), i--)
    {
      const char *sel = IDENTIFIER_POINTER (TREE_PURPOSE (t2));
      if (!strcmp (sel, "target"))
	constructs[i] = OMP_TARGET;
      else if (!strcmp (sel, "teams"))
	constructs[i] = OMP_TEAMS;
      else if (!strcmp (sel, "parallel"))
	constructs[i] = OMP_PARALLEL;
      else if (!strcmp (sel, "for") || !strcmp (sel, "do"))
	constructs[i] = OMP_FOR;
      else if (!strcmp (sel, "simd"))
	constructs[i] = OMP_SIMD;
      else
	gcc_unreachable ();
    }
  gcc_assert (i == -1);
  return nconstructs;
}

/* Return true if PROP is possibly present in one of the offloading target's
   OpenMP contexts.  The format of PROPS string is always offloading target's
   name terminated by '\0', followed by properties for that offloading
   target separated by '\0' and terminated by another '\0'.  The strings
   are created from omp-device-properties installed files of all configured
   offloading targets.  */

static bool
omp_offload_device_kind_arch_isa (const char *props, const char *prop)
{
  const char *names = getenv ("OFFLOAD_TARGET_NAMES");
  if (names == NULL || *names == '\0')
    return false;
  while (*props != '\0')
    {
      size_t name_len = strlen (props);
      bool matches = false;
      for (const char *c = names; c; )
	{
	  if (strncmp (props, c, name_len) == 0
	      && (c[name_len] == '\0'
		  || c[name_len] == ':'
		  || c[name_len] == '='))
	    {
	      matches = true;
	      break;
	    }
	  else if ((c = strchr (c, ':')))
	    c++;
	}
      props = props + name_len + 1;
      while (*props != '\0')
	{
	  if (matches && strcmp (props, prop) == 0)
	    return true;
	  props = strchr (props, '\0') + 1;
	}
      props++;
    }
  return false;
}

/* Return true if the current code location is or might be offloaded.
   Return true in declare target functions, or when nested in a target
   region or when unsure, return false otherwise.  */

static bool
omp_maybe_offloaded (void)
{
  if (!ENABLE_OFFLOADING)
    return false;
  const char *names = getenv ("OFFLOAD_TARGET_NAMES");
  if (names == NULL || *names == '\0')
    return false;

  if (symtab->state == PARSING)
    /* Maybe.  */
    return true;
  if (cfun && cfun->after_inlining)
    return false;
  if (current_function_decl
      && lookup_attribute ("omp declare target",
			   DECL_ATTRIBUTES (current_function_decl)))
    return true;
  if (cfun && (cfun->curr_properties & PROP_gimple_any) == 0)
    {
      enum tree_code construct = OMP_TARGET;
      if (omp_construct_selector_matches (&construct, 1, NULL))
	return true;
    }
  return false;
}

/* Return a name from PROP, a property in selectors accepting
   name lists.  */

static const char *
omp_context_name_list_prop (tree prop)
{
  if (TREE_PURPOSE (prop))
    return IDENTIFIER_POINTER (TREE_PURPOSE (prop));
  else
    {
      const char *ret = TREE_STRING_POINTER (TREE_VALUE (prop));
      if ((size_t) TREE_STRING_LENGTH (TREE_VALUE (prop)) == strlen (ret) + 1)
	return ret;
      return NULL;
    }
}

/* Return 1 if context selector matches the current OpenMP context, 0
   if it does not and -1 if it is unknown and need to be determined later.
   Some properties can be checked right away during parsing (this routine),
   others need to wait until the whole TU is parsed, others need to wait until
   IPA, others until vectorization.  */

int
omp_context_selector_matches (tree ctx)
{
  int ret = 1;
  for (tree t1 = ctx; t1; t1 = TREE_CHAIN (t1))
    {
      char set = IDENTIFIER_POINTER (TREE_PURPOSE (t1))[0];
      if (set == 'c')
	{
	  /* For now, ignore the construct set.  While something can be
	     determined already during parsing, we don't know until end of TU
	     whether additional constructs aren't added through declare variant
	     unless "omp declare variant variant" attribute exists already
	     (so in most of the cases), and we'd need to maintain set of
	     surrounding OpenMP constructs, which is better handled during
	     gimplification.  */
	  if (symtab->state == PARSING)
	    {
	      ret = -1;
	      continue;
	    }

	  enum tree_code constructs[5];
	  int nconstructs
	    = omp_constructor_traits_to_codes (TREE_VALUE (t1), constructs);

	  if (cfun && (cfun->curr_properties & PROP_gimple_any) != 0)
	    {
	      if (!cfun->after_inlining)
		{
		  ret = -1;
		  continue;
		}
	      int i;
	      for (i = 0; i < nconstructs; ++i)
		if (constructs[i] == OMP_SIMD)
		  break;
	      if (i < nconstructs)
		{
		  ret = -1;
		  continue;
		}
	      /* If there is no simd, assume it is ok after IPA,
		 constructs should have been checked before.  */
	      continue;
	    }

	  int r = omp_construct_selector_matches (constructs, nconstructs,
						  NULL);
	  if (r == 0)
	    return 0;
	  if (r == -1)
	    ret = -1;
	  continue;
	}
      for (tree t2 = TREE_VALUE (t1); t2; t2 = TREE_CHAIN (t2))
	{
	  const char *sel = IDENTIFIER_POINTER (TREE_PURPOSE (t2));
	  switch (*sel)
	    {
	    case 'v':
	      if (set == 'i' && !strcmp (sel, "vendor"))
		for (tree t3 = TREE_VALUE (t2); t3; t3 = TREE_CHAIN (t3))
		  {
		    const char *prop = omp_context_name_list_prop (t3);
		    if (prop == NULL)
		      return 0;
		    if ((!strcmp (prop, " score") && TREE_PURPOSE (t3))
			|| !strcmp (prop, "gnu"))
		      continue;
		    return 0;
		  }
	      break;
	    case 'e':
	      if (set == 'i' && !strcmp (sel, "extension"))
		/* We don't support any extensions right now.  */
		return 0;
	      break;
	    case 'a':
	      if (set == 'i' && !strcmp (sel, "atomic_default_mem_order"))
		{
		  if (cfun && (cfun->curr_properties & PROP_gimple_any) != 0)
		    break;

		  enum omp_memory_order omo
		    = ((enum omp_memory_order)
		       (omp_requires_mask
			& OMP_REQUIRES_ATOMIC_DEFAULT_MEM_ORDER));
		  if (omo == OMP_MEMORY_ORDER_UNSPECIFIED)
		    {
		      /* We don't know yet, until end of TU.  */
		      if (symtab->state == PARSING)
			{
			  ret = -1;
			  break;
			}
		      else
			omo = OMP_MEMORY_ORDER_RELAXED;
		    }
		  tree t3 = TREE_VALUE (t2);
		  const char *prop = IDENTIFIER_POINTER (TREE_PURPOSE (t3));
		  if (!strcmp (prop, " score"))
		    {
		      t3 = TREE_CHAIN (t3);
		      prop = IDENTIFIER_POINTER (TREE_PURPOSE (t3));
		    }
		  if (!strcmp (prop, "relaxed")
		      && omo != OMP_MEMORY_ORDER_RELAXED)
		    return 0;
		  else if (!strcmp (prop, "seq_cst")
			   && omo != OMP_MEMORY_ORDER_SEQ_CST)
		    return 0;
		  else if (!strcmp (prop, "acq_rel")
			   && omo != OMP_MEMORY_ORDER_ACQ_REL)
		    return 0;
		}
	      if (set == 'd' && !strcmp (sel, "arch"))
		for (tree t3 = TREE_VALUE (t2); t3; t3 = TREE_CHAIN (t3))
		  {
		    const char *arch = omp_context_name_list_prop (t3);
		    if (arch == NULL)
		      return 0;
		    int r = 0;
		    if (targetm.omp.device_kind_arch_isa != NULL)
		      r = targetm.omp.device_kind_arch_isa (omp_device_arch,
							    arch);
		    if (r == 0 || (r == -1 && symtab->state != PARSING))
		      {
			/* If we are or might be in a target region or
			   declare target function, need to take into account
			   also offloading values.  */
			if (!omp_maybe_offloaded ())
			  return 0;
			if (ENABLE_OFFLOADING)
			  {
			    const char *arches = omp_offload_device_arch;
			    if (omp_offload_device_kind_arch_isa (arches,
								  arch))
			      {
				ret = -1;
				continue;
			      }
			  }
			return 0;
		      }
		    else if (r == -1)
		      ret = -1;
		    /* If arch matches on the host, it still might not match
		       in the offloading region.  */
		    else if (omp_maybe_offloaded ())
		      ret = -1;
		  }
	      break;
	    case 'u':
	      if (set == 'i' && !strcmp (sel, "unified_address"))
		{
		  if (cfun && (cfun->curr_properties & PROP_gimple_any) != 0)
		    break;

		  if ((omp_requires_mask & OMP_REQUIRES_UNIFIED_ADDRESS) == 0)
		    {
		      if (symtab->state == PARSING)
			ret = -1;
		      else
			return 0;
		    }
		  break;
		}
	      if (set == 'i' && !strcmp (sel, "unified_shared_memory"))
		{
		  if (cfun && (cfun->curr_properties & PROP_gimple_any) != 0)
		    break;

		  if ((omp_requires_mask
		       & OMP_REQUIRES_UNIFIED_SHARED_MEMORY) == 0)
		    {
		      if (symtab->state == PARSING)
			ret = -1;
		      else
			return 0;
		    }
		  break;
		}
	      break;
	    case 'd':
	      if (set == 'i' && !strcmp (sel, "dynamic_allocators"))
		{
		  if (cfun && (cfun->curr_properties & PROP_gimple_any) != 0)
		    break;

		  if ((omp_requires_mask
		       & OMP_REQUIRES_DYNAMIC_ALLOCATORS) == 0)
		    {
		      if (symtab->state == PARSING)
			ret = -1;
		      else
			return 0;
		    }
		  break;
		}
	      break;
	    case 'r':
	      if (set == 'i' && !strcmp (sel, "reverse_offload"))
		{
		  if (cfun && (cfun->curr_properties & PROP_gimple_any) != 0)
		    break;

		  if ((omp_requires_mask & OMP_REQUIRES_REVERSE_OFFLOAD) == 0)
		    {
		      if (symtab->state == PARSING)
			ret = -1;
		      else
			return 0;
		    }
		  break;
		}
	      break;
	    case 'k':
	      if (set == 'd' && !strcmp (sel, "kind"))
		for (tree t3 = TREE_VALUE (t2); t3; t3 = TREE_CHAIN (t3))
		  {
		    const char *prop = omp_context_name_list_prop (t3);
		    if (prop == NULL)
		      return 0;
		    if (!strcmp (prop, "any"))
		      continue;
		    if (!strcmp (prop, "host"))
		      {
			if (omp_maybe_offloaded ())
			  ret = -1;
			continue;
		      }
		    if (!strcmp (prop, "nohost"))
		      {
			if (omp_maybe_offloaded ())
			  ret = -1;
			else
			  return 0;
			continue;
		      }
		    int r = 0;
		    if (targetm.omp.device_kind_arch_isa != NULL)
		      r = targetm.omp.device_kind_arch_isa (omp_device_kind,
							    prop);
		    else
		      r = strcmp (prop, "cpu") == 0;
		    if (r == 0 || (r == -1 && symtab->state != PARSING))
		      {
			/* If we are or might be in a target region or
			   declare target function, need to take into account
			   also offloading values.  */
			if (!omp_maybe_offloaded ())
			  return 0;
			if (ENABLE_OFFLOADING)
			  {
			    const char *kinds = omp_offload_device_kind;
			    if (omp_offload_device_kind_arch_isa (kinds, prop))
			      {
				ret = -1;
				continue;
			      }
			  }
			return 0;
		      }
		    else if (r == -1)
		      ret = -1;
		    /* If kind matches on the host, it still might not match
		       in the offloading region.  */
		    else if (omp_maybe_offloaded ())
		      ret = -1;
		  }
	      break;
	    case 'i':
	      if (set == 'd' && !strcmp (sel, "isa"))
		for (tree t3 = TREE_VALUE (t2); t3; t3 = TREE_CHAIN (t3))
		  {
		    const char *isa = omp_context_name_list_prop (t3);
		    if (isa == NULL)
		      return 0;
		    int r = 0;
		    if (targetm.omp.device_kind_arch_isa != NULL)
		      r = targetm.omp.device_kind_arch_isa (omp_device_isa,
							    isa);
		    if (r == 0 || (r == -1 && symtab->state != PARSING))
		      {
			/* If isa is valid on the target, but not in the
			   current function and current function has
			   #pragma omp declare simd on it, some simd clones
			   might have the isa added later on.  */
			if (r == -1
			    && targetm.simd_clone.compute_vecsize_and_simdlen
			    && (cfun == NULL || !cfun->after_inlining))
			  {
			    tree attrs
			      = DECL_ATTRIBUTES (current_function_decl);
			    if (lookup_attribute ("omp declare simd", attrs))
			      {
				ret = -1;
				continue;
			      }
			  }
			/* If we are or might be in a target region or
			   declare target function, need to take into account
			   also offloading values.  */
			if (!omp_maybe_offloaded ())
			  return 0;
			if (ENABLE_OFFLOADING)
			  {
			    const char *isas = omp_offload_device_isa;
			    if (omp_offload_device_kind_arch_isa (isas, isa))
			      {
				ret = -1;
				continue;
			      }
			  }
			return 0;
		      }
		    else if (r == -1)
		      ret = -1;
		    /* If isa matches on the host, it still might not match
		       in the offloading region.  */
		    else if (omp_maybe_offloaded ())
		      ret = -1;
		  }
	      break;
	    case 'c':
	      if (set == 'u' && !strcmp (sel, "condition"))
		for (tree t3 = TREE_VALUE (t2); t3; t3 = TREE_CHAIN (t3))
		  if (TREE_PURPOSE (t3) == NULL_TREE)
		    {
		      if (integer_zerop (TREE_VALUE (t3)))
			return 0;
		      if (integer_nonzerop (TREE_VALUE (t3)))
			break;
		      ret = -1;
		    }
	      break;
	    default:
	      break;
	    }
	}
    }
  return ret;
}

/* Compare construct={simd} CLAUSES1 with CLAUSES2, return 0/-1/1/2 as
   in omp_context_selector_set_compare.  */

static int
omp_construct_simd_compare (tree clauses1, tree clauses2)
{
  if (clauses1 == NULL_TREE)
    return clauses2 == NULL_TREE ? 0 : -1;
  if (clauses2 == NULL_TREE)
    return 1;

  int r = 0;
  struct declare_variant_simd_data {
    bool inbranch, notinbranch;
    tree simdlen;
    auto_vec<tree,16> data_sharing;
    auto_vec<tree,16> aligned;
    declare_variant_simd_data ()
      : inbranch(false), notinbranch(false), simdlen(NULL_TREE) {}
  } data[2];
  unsigned int i;
  for (i = 0; i < 2; i++)
    for (tree c = i ? clauses2 : clauses1; c; c = OMP_CLAUSE_CHAIN (c))
      {
	vec<tree> *v;
	switch (OMP_CLAUSE_CODE (c))
	  {
	  case OMP_CLAUSE_INBRANCH:
	    data[i].inbranch = true;
	    continue;
	  case OMP_CLAUSE_NOTINBRANCH:
	    data[i].notinbranch = true;
	    continue;
	  case OMP_CLAUSE_SIMDLEN:
	    data[i].simdlen = OMP_CLAUSE_SIMDLEN_EXPR (c);
	    continue;
	  case OMP_CLAUSE_UNIFORM:
	  case OMP_CLAUSE_LINEAR:
	    v = &data[i].data_sharing;
	    break;
	  case OMP_CLAUSE_ALIGNED:
	    v = &data[i].aligned;
	    break;
	  default:
	    gcc_unreachable ();
	  }
	unsigned HOST_WIDE_INT argno = tree_to_uhwi (OMP_CLAUSE_DECL (c));
	if (argno >= v->length ())
	  v->safe_grow_cleared (argno + 1, true);
	(*v)[argno] = c;
      }
  /* Here, r is used as a bitmask, 2 is set if CLAUSES1 has something
     CLAUSES2 doesn't, 1 is set if CLAUSES2 has something CLAUSES1
     doesn't.  Thus, r == 3 implies return value 2, r == 1 implies
     -1, r == 2 implies 1 and r == 0 implies 0.  */
  if (data[0].inbranch != data[1].inbranch)
    r |= data[0].inbranch ? 2 : 1;
  if (data[0].notinbranch != data[1].notinbranch)
    r |= data[0].notinbranch ? 2 : 1;
  if (!simple_cst_equal (data[0].simdlen, data[1].simdlen))
    {
      if (data[0].simdlen && data[1].simdlen)
	return 2;
      r |= data[0].simdlen ? 2 : 1;
    }
  if (data[0].data_sharing.length () < data[1].data_sharing.length ()
      || data[0].aligned.length () < data[1].aligned.length ())
    r |= 1;
  tree c1, c2;
  FOR_EACH_VEC_ELT (data[0].data_sharing, i, c1)
    {
      c2 = (i < data[1].data_sharing.length ()
	    ? data[1].data_sharing[i] : NULL_TREE);
      if ((c1 == NULL_TREE) != (c2 == NULL_TREE))
	{
	  r |= c1 != NULL_TREE ? 2 : 1;
	  continue;
	}
      if (c1 == NULL_TREE)
	continue;
      if (OMP_CLAUSE_CODE (c1) != OMP_CLAUSE_CODE (c2))
	return 2;
      if (OMP_CLAUSE_CODE (c1) != OMP_CLAUSE_LINEAR)
	continue;
      if (OMP_CLAUSE_LINEAR_VARIABLE_STRIDE (c1)
	  != OMP_CLAUSE_LINEAR_VARIABLE_STRIDE (c2))
	return 2;
      if (OMP_CLAUSE_LINEAR_KIND (c1) != OMP_CLAUSE_LINEAR_KIND (c2))
	return 2;
      if (!simple_cst_equal (OMP_CLAUSE_LINEAR_STEP (c1),
			     OMP_CLAUSE_LINEAR_STEP (c2)))
	return 2;
    }
  FOR_EACH_VEC_ELT (data[0].aligned, i, c1)
    {
      c2 = i < data[1].aligned.length () ? data[1].aligned[i] : NULL_TREE;
      if ((c1 == NULL_TREE) != (c2 == NULL_TREE))
	{
	  r |= c1 != NULL_TREE ? 2 : 1;
	  continue;
	}
      if (c1 == NULL_TREE)
	continue;
      if (!simple_cst_equal (OMP_CLAUSE_ALIGNED_ALIGNMENT (c1),
			     OMP_CLAUSE_ALIGNED_ALIGNMENT (c2)))
	return 2;
    }
  switch (r)
    {
    case 0: return 0;
    case 1: return -1;
    case 2: return 1;
    case 3: return 2;
    default: gcc_unreachable ();
    }
}

/* Compare properties of selectors SEL from SET other than construct.
   Return 0/-1/1/2 as in omp_context_selector_set_compare.
   Unlike set names or selector names, properties can have duplicates.  */

static int
omp_context_selector_props_compare (const char *set, const char *sel,
				    tree ctx1, tree ctx2)
{
  int ret = 0;
  for (int pass = 0; pass < 2; pass++)
    for (tree t1 = pass ? ctx2 : ctx1; t1; t1 = TREE_CHAIN (t1))
      {
	tree t2;
	for (t2 = pass ? ctx1 : ctx2; t2; t2 = TREE_CHAIN (t2))
	  if (TREE_PURPOSE (t1) == TREE_PURPOSE (t2))
	    {
	      if (TREE_PURPOSE (t1) == NULL_TREE)
		{
		  if (set[0] == 'u' && strcmp (sel, "condition") == 0)
		    {
		      if (integer_zerop (TREE_VALUE (t1))
			  != integer_zerop (TREE_VALUE (t2)))
			return 2;
		      break;
		    }
		  if (simple_cst_equal (TREE_VALUE (t1), TREE_VALUE (t2)))
		    break;
		}
	      else if (strcmp (IDENTIFIER_POINTER (TREE_PURPOSE (t1)),
			       " score") == 0)
		{
		  if (!simple_cst_equal (TREE_VALUE (t1), TREE_VALUE (t2)))
		    return 2;
		  break;
		}
	      else
		break;
	    }
	  else if (TREE_PURPOSE (t1)
		   && TREE_PURPOSE (t2) == NULL_TREE
		   && TREE_CODE (TREE_VALUE (t2)) == STRING_CST)
	    {
	      const char *p1 = omp_context_name_list_prop (t1);
	      const char *p2 = omp_context_name_list_prop (t2);
	      if (p2
		  && strcmp (p1, p2) == 0
		  && strcmp (p1, " score"))
		break;
	    }
	  else if (TREE_PURPOSE (t1) == NULL_TREE
		   && TREE_PURPOSE (t2)
		   && TREE_CODE (TREE_VALUE (t1)) == STRING_CST)
	    {
	      const char *p1 = omp_context_name_list_prop (t1);
	      const char *p2 = omp_context_name_list_prop (t2);
	      if (p1
		  && strcmp (p1, p2) == 0
		  && strcmp (p1, " score"))
		break;
	    }
	if (t2 == NULL_TREE)
	  {
	    int r = pass ? -1 : 1;
	    if (ret && ret != r)
	      return 2;
	    else if (pass)
	      return r;
	    else
	      {
		ret = r;
		break;
	      }
	  }
      }
  return ret;
}

/* Compare single context selector sets CTX1 and CTX2 with SET name.
   Return 0 if CTX1 is equal to CTX2,
   -1 if CTX1 is a strict subset of CTX2,
   1 if CTX2 is a strict subset of CTX1, or
   2 if neither context is a subset of another one.  */

int
omp_context_selector_set_compare (const char *set, tree ctx1, tree ctx2)
{
  bool swapped = false;
  int ret = 0;
  int len1 = list_length (ctx1);
  int len2 = list_length (ctx2);
  int cnt = 0;
  if (len1 < len2)
    {
      swapped = true;
      std::swap (ctx1, ctx2);
      std::swap (len1, len2);
    }
  if (set[0] == 'c')
    {
      tree t1;
      tree t2 = ctx2;
      tree simd = get_identifier ("simd");
      /* Handle construct set specially.  In this case the order
	 of the selector matters too.  */
      for (t1 = ctx1; t1; t1 = TREE_CHAIN (t1))
	if (TREE_PURPOSE (t1) == TREE_PURPOSE (t2))
	  {
	    int r = 0;
	    if (TREE_PURPOSE (t1) == simd)
	      r = omp_construct_simd_compare (TREE_VALUE (t1),
					      TREE_VALUE (t2));
	    if (r == 2 || (ret && r && (ret < 0) != (r < 0)))
	      return 2;
	    if (ret == 0)
	      ret = r;
	    t2 = TREE_CHAIN (t2);
	    if (t2 == NULL_TREE)
	      {
		t1 = TREE_CHAIN (t1);
		break;
	      }
	  }
	else if (ret < 0)
	  return 2;
	else
	  ret = 1;
      if (t2 != NULL_TREE)
	return 2;
      if (t1 != NULL_TREE)
	{
	  if (ret < 0)
	    return 2;
	  ret = 1;
	}
      if (ret == 0)
	return 0;
      return swapped ? -ret : ret;
    }
  for (tree t1 = ctx1; t1; t1 = TREE_CHAIN (t1))
    {
      tree t2;
      for (t2 = ctx2; t2; t2 = TREE_CHAIN (t2))
	if (TREE_PURPOSE (t1) == TREE_PURPOSE (t2))
	  {
	    const char *sel = IDENTIFIER_POINTER (TREE_PURPOSE (t1));
	    int r = omp_context_selector_props_compare (set, sel,
							TREE_VALUE (t1),
							TREE_VALUE (t2));
	    if (r == 2 || (ret && r && (ret < 0) != (r < 0)))
	      return 2;
	    if (ret == 0)
	      ret = r;
	    cnt++;
	    break;
	  }
      if (t2 == NULL_TREE)
	{
	  if (ret == -1)
	    return 2;
	  ret = 1;
	}
    }
  if (cnt < len2)
    return 2;
  if (ret == 0)
    return 0;
  return swapped ? -ret : ret;
}

/* Compare whole context selector specification CTX1 and CTX2.
   Return 0 if CTX1 is equal to CTX2,
   -1 if CTX1 is a strict subset of CTX2,
   1 if CTX2 is a strict subset of CTX1, or
   2 if neither context is a subset of another one.  */

static int
omp_context_selector_compare (tree ctx1, tree ctx2)
{
  bool swapped = false;
  int ret = 0;
  int len1 = list_length (ctx1);
  int len2 = list_length (ctx2);
  int cnt = 0;
  if (len1 < len2)
    {
      swapped = true;
      std::swap (ctx1, ctx2);
      std::swap (len1, len2);
    }
  for (tree t1 = ctx1; t1; t1 = TREE_CHAIN (t1))
    {
      tree t2;
      for (t2 = ctx2; t2; t2 = TREE_CHAIN (t2))
	if (TREE_PURPOSE (t1) == TREE_PURPOSE (t2))
	  {
	    const char *set = IDENTIFIER_POINTER (TREE_PURPOSE (t1));
	    int r = omp_context_selector_set_compare (set, TREE_VALUE (t1),
						      TREE_VALUE (t2));
	    if (r == 2 || (ret && r && (ret < 0) != (r < 0)))
	      return 2;
	    if (ret == 0)
	      ret = r;
	    cnt++;
	    break;
	  }
      if (t2 == NULL_TREE)
	{
	  if (ret == -1)
	    return 2;
	  ret = 1;
	}
    }
  if (cnt < len2)
    return 2;
  if (ret == 0)
    return 0;
  return swapped ? -ret : ret;
}

/* From context selector CTX, return trait-selector with name SEL in
   trait-selector-set with name SET if any, or NULL_TREE if not found.
   If SEL is NULL, return the list of trait-selectors in SET.  */

tree
omp_get_context_selector (tree ctx, const char *set, const char *sel)
{
  tree setid = get_identifier (set);
  tree selid = sel ? get_identifier (sel) : NULL_TREE;
  for (tree t1 = ctx; t1; t1 = TREE_CHAIN (t1))
    if (TREE_PURPOSE (t1) == setid)
      {
	if (sel == NULL)
	  return TREE_VALUE (t1);
	for (tree t2 = TREE_VALUE (t1); t2; t2 = TREE_CHAIN (t2))
	  if (TREE_PURPOSE (t2) == selid)
	    return t2;
      }
  return NULL_TREE;
}

/* Compute *SCORE for context selector CTX.  Return true if the score
   would be different depending on whether it is a declare simd clone or
   not.  DECLARE_SIMD should be true for the case when it would be
   a declare simd clone.  */

static bool
omp_context_compute_score (tree ctx, widest_int *score, bool declare_simd)
{
  tree construct = omp_get_context_selector (ctx, "construct", NULL);
  bool has_kind = omp_get_context_selector (ctx, "device", "kind");
  bool has_arch = omp_get_context_selector (ctx, "device", "arch");
  bool has_isa = omp_get_context_selector (ctx, "device", "isa");
  bool ret = false;
  *score = 1;
  for (tree t1 = ctx; t1; t1 = TREE_CHAIN (t1))
    if (TREE_VALUE (t1) != construct)
      for (tree t2 = TREE_VALUE (t1); t2; t2 = TREE_CHAIN (t2))
	if (tree t3 = TREE_VALUE (t2))
	  if (TREE_PURPOSE (t3)
	      && strcmp (IDENTIFIER_POINTER (TREE_PURPOSE (t3)), " score") == 0
	      && TREE_CODE (TREE_VALUE (t3)) == INTEGER_CST)
	    *score += wi::to_widest (TREE_VALUE (t3));
  if (construct || has_kind || has_arch || has_isa)
    {
      int scores[12];
      enum tree_code constructs[5];
      int nconstructs = 0;
      if (construct)
	nconstructs = omp_constructor_traits_to_codes (construct, constructs);
      if (omp_construct_selector_matches (constructs, nconstructs, scores)
	  == 2)
	ret = true;
      int b = declare_simd ? nconstructs + 1 : 0;
      if (scores[b + nconstructs] + 4U < score->get_precision ())
	{
	  for (int n = 0; n < nconstructs; ++n)
	    {
	      if (scores[b + n] < 0)
		{
		  *score = -1;
		  return ret;
		}
	      *score += wi::shifted_mask <widest_int> (scores[b + n], 1, false);
	    }
	  if (has_kind)
	    *score += wi::shifted_mask <widest_int> (scores[b + nconstructs],
						     1, false);
	  if (has_arch)
	    *score += wi::shifted_mask <widest_int> (scores[b + nconstructs] + 1,
						     1, false);
	  if (has_isa)
	    *score += wi::shifted_mask <widest_int> (scores[b + nconstructs] + 2,
						     1, false);
	}
      else /* FIXME: Implement this.  */
	gcc_unreachable ();
    }
  return ret;
}

/* Class describing a single variant.  */
struct GTY(()) omp_declare_variant_entry {
  /* NODE of the variant.  */
  cgraph_node *variant;
  /* Score if not in declare simd clone.  */
  widest_int score;
  /* Score if in declare simd clone.  */
  widest_int score_in_declare_simd_clone;
  /* Context selector for the variant.  */
  tree ctx;
  /* True if the context selector is known to match already.  */
  bool matches;
};

/* Class describing a function with variants.  */
struct GTY((for_user)) omp_declare_variant_base_entry {
  /* NODE of the base function.  */
  cgraph_node *base;
  /* NODE of the artificial function created for the deferred variant
     resolution.  */
  cgraph_node *node;
  /* Vector of the variants.  */
  vec<omp_declare_variant_entry, va_gc> *variants;
};

struct omp_declare_variant_hasher
  : ggc_ptr_hash<omp_declare_variant_base_entry> {
  static hashval_t hash (omp_declare_variant_base_entry *);
  static bool equal (omp_declare_variant_base_entry *,
		     omp_declare_variant_base_entry *);
};

hashval_t
omp_declare_variant_hasher::hash (omp_declare_variant_base_entry *x)
{
  inchash::hash hstate;
  hstate.add_int (DECL_UID (x->base->decl));
  hstate.add_int (x->variants->length ());
  omp_declare_variant_entry *variant;
  unsigned int i;
  FOR_EACH_VEC_SAFE_ELT (x->variants, i, variant)
    {
      hstate.add_int (DECL_UID (variant->variant->decl));
      hstate.add_wide_int (variant->score);
      hstate.add_wide_int (variant->score_in_declare_simd_clone);
      hstate.add_ptr (variant->ctx);
      hstate.add_int (variant->matches);
    }
  return hstate.end ();
}

bool
omp_declare_variant_hasher::equal (omp_declare_variant_base_entry *x,
				   omp_declare_variant_base_entry *y)
{
  if (x->base != y->base
      || x->variants->length () != y->variants->length ())
    return false;
  omp_declare_variant_entry *variant;
  unsigned int i;
  FOR_EACH_VEC_SAFE_ELT (x->variants, i, variant)
    if (variant->variant != (*y->variants)[i].variant
	|| variant->score != (*y->variants)[i].score
	|| (variant->score_in_declare_simd_clone
	    != (*y->variants)[i].score_in_declare_simd_clone)
	|| variant->ctx != (*y->variants)[i].ctx
	|| variant->matches != (*y->variants)[i].matches)
      return false;
  return true;
}

static GTY(()) hash_table<omp_declare_variant_hasher> *omp_declare_variants;

struct omp_declare_variant_alt_hasher
  : ggc_ptr_hash<omp_declare_variant_base_entry> {
  static hashval_t hash (omp_declare_variant_base_entry *);
  static bool equal (omp_declare_variant_base_entry *,
		     omp_declare_variant_base_entry *);
};

hashval_t
omp_declare_variant_alt_hasher::hash (omp_declare_variant_base_entry *x)
{
  return DECL_UID (x->node->decl);
}

bool
omp_declare_variant_alt_hasher::equal (omp_declare_variant_base_entry *x,
				       omp_declare_variant_base_entry *y)
{
  return x->node == y->node;
}

static GTY(()) hash_table<omp_declare_variant_alt_hasher>
  *omp_declare_variant_alt;

/* Try to resolve declare variant after gimplification.  */

static tree
omp_resolve_late_declare_variant (tree alt)
{
  cgraph_node *node = cgraph_node::get (alt);
  cgraph_node *cur_node = cgraph_node::get (cfun->decl);
  if (node == NULL
      || !node->declare_variant_alt
      || !cfun->after_inlining)
    return alt;

  omp_declare_variant_base_entry entry;
  entry.base = NULL;
  entry.node = node;
  entry.variants = NULL;
  omp_declare_variant_base_entry *entryp
    = omp_declare_variant_alt->find_with_hash (&entry, DECL_UID (alt));

  unsigned int i, j;
  omp_declare_variant_entry *varentry1, *varentry2;
  auto_vec <bool, 16> matches;
  unsigned int nmatches = 0;
  FOR_EACH_VEC_SAFE_ELT (entryp->variants, i, varentry1)
    {
      if (varentry1->matches)
	{
	  /* This has been checked to be ok already.  */
	  matches.safe_push (true);
	  nmatches++;
	  continue;
	}
      switch (omp_context_selector_matches (varentry1->ctx))
	{
	case 0:
          matches.safe_push (false);
	  break;
	case -1:
	  return alt;
	default:
	  matches.safe_push (true);
	  nmatches++;
	  break;
	}
    }

  if (nmatches == 0)
    return entryp->base->decl;

  /* A context selector that is a strict subset of another context selector
     has a score of zero.  */
  FOR_EACH_VEC_SAFE_ELT (entryp->variants, i, varentry1)
    if (matches[i])
      {
        for (j = i + 1;
	     vec_safe_iterate (entryp->variants, j, &varentry2); ++j)
	  if (matches[j])
	    {
	      int r = omp_context_selector_compare (varentry1->ctx,
						    varentry2->ctx);
	      if (r == -1)
		{
		  /* ctx1 is a strict subset of ctx2, ignore ctx1.  */
		  matches[i] = false;
		  break;
		}
	      else if (r == 1)
		/* ctx2 is a strict subset of ctx1, remove ctx2.  */
		matches[j] = false;
	    }
      }

  widest_int max_score = -1;
  varentry2 = NULL;
  FOR_EACH_VEC_SAFE_ELT (entryp->variants, i, varentry1)
    if (matches[i])
      {
	widest_int score
	  = (cur_node->simdclone ? varentry1->score_in_declare_simd_clone
	     : varentry1->score);
	if (score > max_score)
	  {
	    max_score = score;
	    varentry2 = varentry1;
	  }
      }
  return varentry2->variant->decl;
}

/* Hook to adjust hash tables on cgraph_node removal.  */

static void
omp_declare_variant_remove_hook (struct cgraph_node *node, void *)
{
  if (!node->declare_variant_alt)
    return;

  /* Drop this hash table completely.  */
  omp_declare_variants = NULL;
  /* And remove node from the other hash table.  */
  if (omp_declare_variant_alt)
    {
      omp_declare_variant_base_entry entry;
      entry.base = NULL;
      entry.node = node;
      entry.variants = NULL;
      omp_declare_variant_alt->remove_elt_with_hash (&entry,
						     DECL_UID (node->decl));
    }
}

/* Try to resolve declare variant, return the variant decl if it should
   be used instead of base, or base otherwise.  */

tree
omp_resolve_declare_variant (tree base)
{
  tree variant1 = NULL_TREE, variant2 = NULL_TREE;
  if (cfun && (cfun->curr_properties & PROP_gimple_any) != 0)
    return omp_resolve_late_declare_variant (base);

  auto_vec <tree, 16> variants;
  auto_vec <bool, 16> defer;
  bool any_deferred = false;
  for (tree attr = DECL_ATTRIBUTES (base); attr; attr = TREE_CHAIN (attr))
    {
      attr = lookup_attribute ("omp declare variant base", attr);
      if (attr == NULL_TREE)
	break;
      if (TREE_CODE (TREE_PURPOSE (TREE_VALUE (attr))) != FUNCTION_DECL)
	continue;
      cgraph_node *node = cgraph_node::get (base);
      /* If this is already a magic decl created by this function,
	 don't process it again.  */
      if (node && node->declare_variant_alt)
	return base;
      switch (omp_context_selector_matches (TREE_VALUE (TREE_VALUE (attr))))
	{
	case 0:
	  /* No match, ignore.  */
	  break;
	case -1:
	  /* Needs to be deferred.  */
	  any_deferred = true;
	  variants.safe_push (attr);
	  defer.safe_push (true);
	  break;
	default:
	  variants.safe_push (attr);
	  defer.safe_push (false);
	  break;
	}
    }
  if (variants.length () == 0)
    return base;

  if (any_deferred)
    {
      widest_int max_score1 = 0;
      widest_int max_score2 = 0;
      bool first = true;
      unsigned int i;
      tree attr1, attr2;
      omp_declare_variant_base_entry entry;
      entry.base = cgraph_node::get_create (base);
      entry.node = NULL;
      vec_alloc (entry.variants, variants.length ());
      FOR_EACH_VEC_ELT (variants, i, attr1)
	{
	  widest_int score1;
	  widest_int score2;
	  bool need_two;
	  tree ctx = TREE_VALUE (TREE_VALUE (attr1));
	  need_two = omp_context_compute_score (ctx, &score1, false);
	  if (need_two)
	    omp_context_compute_score (ctx, &score2, true);
	  else
	    score2 = score1;
	  if (first)
	    {
	      first = false;
	      max_score1 = score1;
	      max_score2 = score2;
	      if (!defer[i])
		{
		  variant1 = attr1;
		  variant2 = attr1;
		}
	    }
	  else
	    {
	      if (max_score1 == score1)
		variant1 = NULL_TREE;
	      else if (score1 > max_score1)
		{
		  max_score1 = score1;
		  variant1 = defer[i] ? NULL_TREE : attr1;
		}
	      if (max_score2 == score2)
		variant2 = NULL_TREE;
	      else if (score2 > max_score2)
		{
		  max_score2 = score2;
		  variant2 = defer[i] ? NULL_TREE : attr1;
		}
	    }
	  omp_declare_variant_entry varentry;
	  varentry.variant
	    = cgraph_node::get_create (TREE_PURPOSE (TREE_VALUE (attr1)));
	  varentry.score = score1;
	  varentry.score_in_declare_simd_clone = score2;
	  varentry.ctx = ctx;
	  varentry.matches = !defer[i];
	  entry.variants->quick_push (varentry);
	}

      /* If there is a clear winner variant with the score which is not
	 deferred, verify it is not a strict subset of any other context
	 selector and if it is not, it is the best alternative no matter
	 whether the others do or don't match.  */
      if (variant1 && variant1 == variant2)
	{
	  tree ctx1 = TREE_VALUE (TREE_VALUE (variant1));
	  FOR_EACH_VEC_ELT (variants, i, attr2)
	    {
	      if (attr2 == variant1)
		continue;
	      tree ctx2 = TREE_VALUE (TREE_VALUE (attr2));
	      int r = omp_context_selector_compare (ctx1, ctx2);
	      if (r == -1)
		{
		  /* The winner is a strict subset of ctx2, can't
		     decide now.  */
		  variant1 = NULL_TREE;
		  break;
		}
	    }
	  if (variant1)
	    {
	      vec_free (entry.variants);
	      return TREE_PURPOSE (TREE_VALUE (variant1));
	    }
	}

      static struct cgraph_node_hook_list *node_removal_hook_holder;
      if (!node_removal_hook_holder)
	node_removal_hook_holder
	  = symtab->add_cgraph_removal_hook (omp_declare_variant_remove_hook,
					     NULL);

      if (omp_declare_variants == NULL)
	omp_declare_variants
	  = hash_table<omp_declare_variant_hasher>::create_ggc (64);
      omp_declare_variant_base_entry **slot
	= omp_declare_variants->find_slot (&entry, INSERT);
      if (*slot != NULL)
	{
	  vec_free (entry.variants);
	  return (*slot)->node->decl;
	}

      *slot = ggc_cleared_alloc<omp_declare_variant_base_entry> ();
      (*slot)->base = entry.base;
      (*slot)->node = entry.base;
      (*slot)->variants = entry.variants;
      tree alt = build_decl (DECL_SOURCE_LOCATION (base), FUNCTION_DECL,
			     DECL_NAME (base), TREE_TYPE (base));
      DECL_ARTIFICIAL (alt) = 1;
      DECL_IGNORED_P (alt) = 1;
      TREE_STATIC (alt) = 1;
      tree attributes = DECL_ATTRIBUTES (base);
      if (lookup_attribute ("noipa", attributes) == NULL)
	{
	  attributes = tree_cons (get_identifier ("noipa"), NULL, attributes);
	  if (lookup_attribute ("noinline", attributes) == NULL)
	    attributes = tree_cons (get_identifier ("noinline"), NULL,
				    attributes);
	  if (lookup_attribute ("noclone", attributes) == NULL)
	    attributes = tree_cons (get_identifier ("noclone"), NULL,
				    attributes);
	  if (lookup_attribute ("no_icf", attributes) == NULL)
	    attributes = tree_cons (get_identifier ("no_icf"), NULL,
				    attributes);
	}
      DECL_ATTRIBUTES (alt) = attributes;
      DECL_INITIAL (alt) = error_mark_node;
      (*slot)->node = cgraph_node::create (alt);
      (*slot)->node->declare_variant_alt = 1;
      (*slot)->node->create_reference (entry.base, IPA_REF_ADDR);
      omp_declare_variant_entry *varentry;
      FOR_EACH_VEC_SAFE_ELT (entry.variants, i, varentry)
	(*slot)->node->create_reference (varentry->variant, IPA_REF_ADDR);
      if (omp_declare_variant_alt == NULL)
	omp_declare_variant_alt
	  = hash_table<omp_declare_variant_alt_hasher>::create_ggc (64);
      *omp_declare_variant_alt->find_slot_with_hash (*slot, DECL_UID (alt),
						     INSERT) = *slot;
      return alt;
    }

  if (variants.length () == 1)
    return TREE_PURPOSE (TREE_VALUE (variants[0]));

  /* A context selector that is a strict subset of another context selector
     has a score of zero.  */
  tree attr1, attr2;
  unsigned int i, j;
  FOR_EACH_VEC_ELT (variants, i, attr1)
    if (attr1)
      {
	tree ctx1 = TREE_VALUE (TREE_VALUE (attr1));
	FOR_EACH_VEC_ELT_FROM (variants, j, attr2, i + 1)
	  if (attr2)
	    {
	      tree ctx2 = TREE_VALUE (TREE_VALUE (attr2));
	      int r = omp_context_selector_compare (ctx1, ctx2);
	      if (r == -1)
		{
		  /* ctx1 is a strict subset of ctx2, remove
		     attr1 from the vector.  */
		  variants[i] = NULL_TREE;
		  break;
		}
	      else if (r == 1)
		/* ctx2 is a strict subset of ctx1, remove attr2
		   from the vector.  */
		variants[j] = NULL_TREE;
	    }
      }
  widest_int max_score1 = 0;
  widest_int max_score2 = 0;
  bool first = true;
  FOR_EACH_VEC_ELT (variants, i, attr1)
    if (attr1)
      {
	if (variant1)
	  {
	    widest_int score1;
	    widest_int score2;
	    bool need_two;
	    tree ctx;
	    if (first)
	      {
		first = false;
		ctx = TREE_VALUE (TREE_VALUE (variant1));
		need_two = omp_context_compute_score (ctx, &max_score1, false);
		if (need_two)
		  omp_context_compute_score (ctx, &max_score2, true);
		else
		  max_score2 = max_score1;
	      }
	    ctx = TREE_VALUE (TREE_VALUE (attr1));
	    need_two = omp_context_compute_score (ctx, &score1, false);
	    if (need_two)
	      omp_context_compute_score (ctx, &score2, true);
	    else
	      score2 = score1;
	    if (score1 > max_score1)
	      {
		max_score1 = score1;
		variant1 = attr1;
	      }
	    if (score2 > max_score2)
	      {
		max_score2 = score2;
		variant2 = attr1;
	      }
	  }
	else
	  {
	    variant1 = attr1;
	    variant2 = attr1;
	  }
      }
  /* If there is a disagreement on which variant has the highest score
     depending on whether it will be in a declare simd clone or not,
     punt for now and defer until after IPA where we will know that.  */
  return ((variant1 && variant1 == variant2)
	  ? TREE_PURPOSE (TREE_VALUE (variant1)) : base);
}

void
omp_lto_output_declare_variant_alt (lto_simple_output_block *ob,
				    cgraph_node *node,
				    lto_symtab_encoder_t encoder)
{
  gcc_assert (node->declare_variant_alt);

  omp_declare_variant_base_entry entry;
  entry.base = NULL;
  entry.node = node;
  entry.variants = NULL;
  omp_declare_variant_base_entry *entryp
    = omp_declare_variant_alt->find_with_hash (&entry, DECL_UID (node->decl));
  gcc_assert (entryp);

  int nbase = lto_symtab_encoder_lookup (encoder, entryp->base);
  gcc_assert (nbase != LCC_NOT_FOUND);
  streamer_write_hwi_stream (ob->main_stream, nbase);

  streamer_write_hwi_stream (ob->main_stream, entryp->variants->length ());

  unsigned int i;
  omp_declare_variant_entry *varentry;
  FOR_EACH_VEC_SAFE_ELT (entryp->variants, i, varentry)
    {
      int nvar = lto_symtab_encoder_lookup (encoder, varentry->variant);
      gcc_assert (nvar != LCC_NOT_FOUND);
      streamer_write_hwi_stream (ob->main_stream, nvar);

      for (widest_int *w = &varentry->score; ;
	   w = &varentry->score_in_declare_simd_clone)
	{
	  unsigned len = w->get_len ();
	  streamer_write_hwi_stream (ob->main_stream, len);
	  const HOST_WIDE_INT *val = w->get_val ();
	  for (unsigned j = 0; j < len; j++)
	    streamer_write_hwi_stream (ob->main_stream, val[j]);
	  if (w == &varentry->score_in_declare_simd_clone)
	    break;
	}

      HOST_WIDE_INT cnt = -1;
      HOST_WIDE_INT i = varentry->matches ? 1 : 0;
      for (tree attr = DECL_ATTRIBUTES (entryp->base->decl);
	   attr; attr = TREE_CHAIN (attr), i += 2)
	{
	  attr = lookup_attribute ("omp declare variant base", attr);
	  if (attr == NULL_TREE)
	    break;

	  if (varentry->ctx == TREE_VALUE (TREE_VALUE (attr)))
	    {
	      cnt = i;
	      break;
	    }
	}

      gcc_assert (cnt != -1);
      streamer_write_hwi_stream (ob->main_stream, cnt);
    }
}

void
omp_lto_input_declare_variant_alt (lto_input_block *ib, cgraph_node *node,
				   vec<symtab_node *> nodes)
{
  gcc_assert (node->declare_variant_alt);
  omp_declare_variant_base_entry *entryp
    = ggc_cleared_alloc<omp_declare_variant_base_entry> ();
  entryp->base = dyn_cast<cgraph_node *> (nodes[streamer_read_hwi (ib)]);
  entryp->node = node;
  unsigned int len = streamer_read_hwi (ib);
  vec_alloc (entryp->variants, len);

  for (unsigned int i = 0; i < len; i++)
    {
      omp_declare_variant_entry varentry;
      varentry.variant
	= dyn_cast<cgraph_node *> (nodes[streamer_read_hwi (ib)]);
      for (widest_int *w = &varentry.score; ;
	   w = &varentry.score_in_declare_simd_clone)
	{
	  unsigned len2 = streamer_read_hwi (ib);
	  HOST_WIDE_INT arr[WIDE_INT_MAX_ELTS];
	  gcc_assert (len2 <= WIDE_INT_MAX_ELTS);
	  for (unsigned int j = 0; j < len2; j++)
	    arr[j] = streamer_read_hwi (ib);
	  *w = widest_int::from_array (arr, len2, true);
	  if (w == &varentry.score_in_declare_simd_clone)
	    break;
	}

      HOST_WIDE_INT cnt = streamer_read_hwi (ib);
      HOST_WIDE_INT j = 0;
      varentry.ctx = NULL_TREE;
      varentry.matches = (cnt & 1) ? true : false;
      cnt &= ~HOST_WIDE_INT_1;
      for (tree attr = DECL_ATTRIBUTES (entryp->base->decl);
	   attr; attr = TREE_CHAIN (attr), j += 2)
	{
	  attr = lookup_attribute ("omp declare variant base", attr);
	  if (attr == NULL_TREE)
	    break;

	  if (cnt == j)
	    {
	      varentry.ctx = TREE_VALUE (TREE_VALUE (attr));
	      break;
	    }
	}
      gcc_assert (varentry.ctx != NULL_TREE);
      entryp->variants->quick_push (varentry);
    }
  if (omp_declare_variant_alt == NULL)
    omp_declare_variant_alt
      = hash_table<omp_declare_variant_alt_hasher>::create_ggc (64);
  *omp_declare_variant_alt->find_slot_with_hash (entryp, DECL_UID (node->decl),
						 INSERT) = entryp;
}

/* Encode an oacc launch argument.  This matches the GOMP_LAUNCH_PACK
   macro on gomp-constants.h.  We do not check for overflow.  */

tree
oacc_launch_pack (unsigned code, tree device, unsigned op)
{
  tree res;

  res = build_int_cst (unsigned_type_node, GOMP_LAUNCH_PACK (code, 0, op));
  if (device)
    {
      device = fold_build2 (LSHIFT_EXPR, unsigned_type_node,
			    device, build_int_cst (unsigned_type_node,
						   GOMP_LAUNCH_DEVICE_SHIFT));
      res = fold_build2 (BIT_IOR_EXPR, unsigned_type_node, res, device);
    }
  return res;
}

/* FIXME: What is the following comment for? */
/* Look for compute grid dimension clauses and convert to an attribute
   attached to FN.  This permits the target-side code to (a) massage
   the dimensions, (b) emit that data and (c) optimize.  Non-constant
   dimensions are pushed onto ARGS.

   The attribute value is a TREE_LIST.  A set of dimensions is
   represented as a list of INTEGER_CST.  Those that are runtime
   exprs are represented as an INTEGER_CST of zero.

   TODO: Normally the attribute will just contain a single such list.  If
   however it contains a list of lists, this will represent the use of
   device_type.  Each member of the outer list is an assoc list of
   dimensions, keyed by the device type.  The first entry will be the
   default.  Well, that's the plan.  */

/* Replace any existing oacc fn attribute with updated dimensions.  */

/* Variant working on a list of attributes.  */

tree
oacc_replace_fn_attrib_attr (tree attribs, tree dims)
{
  tree ident = get_identifier (OACC_FN_ATTRIB);

  /* If we happen to be present as the first attrib, drop it.  */
  if (attribs && TREE_PURPOSE (attribs) == ident)
    attribs = TREE_CHAIN (attribs);
  return tree_cons (ident, dims, attribs);
}

/* Variant working on a function decl.  */

void
oacc_replace_fn_attrib (tree fn, tree dims)
{
  DECL_ATTRIBUTES (fn)
    = oacc_replace_fn_attrib_attr (DECL_ATTRIBUTES (fn), dims);
}

/* Scan CLAUSES for launch dimensions and attach them to the oacc
   function attribute.  Push any that are non-constant onto the ARGS
   list, along with an appropriate GOMP_LAUNCH_DIM tag.  */

void
oacc_set_fn_attrib (tree fn, tree clauses, vec<tree> *args)
{
  /* Must match GOMP_DIM ordering.  */
  static const omp_clause_code ids[]
    = { OMP_CLAUSE_NUM_GANGS, OMP_CLAUSE_NUM_WORKERS,
	OMP_CLAUSE_VECTOR_LENGTH };
  unsigned ix;
  tree dims[GOMP_DIM_MAX];

  tree attr = NULL_TREE;
  unsigned non_const = 0;

  for (ix = GOMP_DIM_MAX; ix--;)
    {
      tree clause = omp_find_clause (clauses, ids[ix]);
      tree dim = NULL_TREE;

      if (clause)
	dim = OMP_CLAUSE_EXPR (clause, ids[ix]);
      dims[ix] = dim;
      if (dim && TREE_CODE (dim) != INTEGER_CST)
	{
	  dim = integer_zero_node;
	  non_const |= GOMP_DIM_MASK (ix);
	}
      attr = tree_cons (NULL_TREE, dim, attr);
    }

  oacc_replace_fn_attrib (fn, attr);

  if (non_const)
    {
      /* Push a dynamic argument set.  */
      args->safe_push (oacc_launch_pack (GOMP_LAUNCH_DIM,
					 NULL_TREE, non_const));
      for (unsigned ix = 0; ix != GOMP_DIM_MAX; ix++)
	if (non_const & GOMP_DIM_MASK (ix))
	  args->safe_push (dims[ix]);
    }
}

/* Verify OpenACC routine clauses.

   Returns 0 if FNDECL should be marked with an OpenACC 'routine' directive, 1
   if it has already been marked in compatible way, and -1 if incompatible.
   Upon returning, the chain of clauses will contain exactly one clause
   specifying the level of parallelism.  */

int
oacc_verify_routine_clauses (tree fndecl, tree *clauses, location_t loc,
			     const char *routine_str)
{
  tree c_level = NULL_TREE;
  tree c_p = NULL_TREE;
  for (tree c = *clauses; c; c_p = c, c = OMP_CLAUSE_CHAIN (c))
    switch (OMP_CLAUSE_CODE (c))
      {
      case OMP_CLAUSE_GANG:
      case OMP_CLAUSE_WORKER:
      case OMP_CLAUSE_VECTOR:
      case OMP_CLAUSE_SEQ:
	if (c_level == NULL_TREE)
	  c_level = c;
	else if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_CODE (c_level))
	  {
	    /* This has already been diagnosed in the front ends.  */
	    /* Drop the duplicate clause.  */
	    gcc_checking_assert (c_p != NULL_TREE);
	    OMP_CLAUSE_CHAIN (c_p) = OMP_CLAUSE_CHAIN (c);
	    c = c_p;
	  }
	else
	  {
	    error_at (OMP_CLAUSE_LOCATION (c),
		      "%qs specifies a conflicting level of parallelism",
		      omp_clause_code_name[OMP_CLAUSE_CODE (c)]);
	    inform (OMP_CLAUSE_LOCATION (c_level),
		    "... to the previous %qs clause here",
		    omp_clause_code_name[OMP_CLAUSE_CODE (c_level)]);
	    /* Drop the conflicting clause.  */
	    gcc_checking_assert (c_p != NULL_TREE);
	    OMP_CLAUSE_CHAIN (c_p) = OMP_CLAUSE_CHAIN (c);
	    c = c_p;
	  }
	break;
      default:
	gcc_unreachable ();
      }
  if (c_level == NULL_TREE)
    {
      /* Default to an implicit 'seq' clause.  */
      c_level = build_omp_clause (loc, OMP_CLAUSE_SEQ);
      OMP_CLAUSE_CHAIN (c_level) = *clauses;
      *clauses = c_level;
    }
  /* In *clauses, we now have exactly one clause specifying the level of
     parallelism.  */

  tree attr
    = lookup_attribute ("omp declare target", DECL_ATTRIBUTES (fndecl));
  if (attr != NULL_TREE)
    {
      /* Diagnose if "#pragma omp declare target" has also been applied.  */
      if (TREE_VALUE (attr) == NULL_TREE)
	{
	  /* See <https://gcc.gnu.org/PR93465>; the semantics of combining
	     OpenACC and OpenMP 'target' are not clear.  */
	  error_at (loc,
		    "cannot apply %<%s%> to %qD, which has also been"
		    " marked with an OpenMP 'declare target' directive",
		    routine_str, fndecl);
	  /* Incompatible.  */
	  return -1;
	}

      /* If a "#pragma acc routine" has already been applied, just verify
	 this one for compatibility.  */
      /* Collect previous directive's clauses.  */
      tree c_level_p = NULL_TREE;
      for (tree c = TREE_VALUE (attr); c; c = OMP_CLAUSE_CHAIN (c))
	switch (OMP_CLAUSE_CODE (c))
	  {
	  case OMP_CLAUSE_GANG:
	  case OMP_CLAUSE_WORKER:
	  case OMP_CLAUSE_VECTOR:
	  case OMP_CLAUSE_SEQ:
	    gcc_checking_assert (c_level_p == NULL_TREE);
	    c_level_p = c;
	    break;
	  default:
	    gcc_unreachable ();
	  }
      gcc_checking_assert (c_level_p != NULL_TREE);
      /* ..., and compare to current directive's, which we've already collected
	 above.  */
      tree c_diag;
      tree c_diag_p;
      /* Matching level of parallelism?  */
      if (OMP_CLAUSE_CODE (c_level) != OMP_CLAUSE_CODE (c_level_p))
	{
	  c_diag = c_level;
	  c_diag_p = c_level_p;
	  goto incompatible;
	}
      /* Compatible.  */
      return 1;

    incompatible:
      if (c_diag != NULL_TREE)
	error_at (OMP_CLAUSE_LOCATION (c_diag),
		  "incompatible %qs clause when applying"
		  " %<%s%> to %qD, which has already been"
		  " marked with an OpenACC 'routine' directive",
		  omp_clause_code_name[OMP_CLAUSE_CODE (c_diag)],
		  routine_str, fndecl);
      else if (c_diag_p != NULL_TREE)
	error_at (loc,
		  "missing %qs clause when applying"
		  " %<%s%> to %qD, which has already been"
		  " marked with an OpenACC 'routine' directive",
		  omp_clause_code_name[OMP_CLAUSE_CODE (c_diag_p)],
		  routine_str, fndecl);
      else
	gcc_unreachable ();
      if (c_diag_p != NULL_TREE)
	inform (OMP_CLAUSE_LOCATION (c_diag_p),
		"... with %qs clause here",
		omp_clause_code_name[OMP_CLAUSE_CODE (c_diag_p)]);
      else
	{
	  /* In the front ends, we don't preserve location information for the
	     OpenACC routine directive itself.  However, that of c_level_p
	     should be close.  */
	  location_t loc_routine = OMP_CLAUSE_LOCATION (c_level_p);
	  inform (loc_routine, "... without %qs clause near to here",
		  omp_clause_code_name[OMP_CLAUSE_CODE (c_diag)]);
	}
      /* Incompatible.  */
      return -1;
    }

  return 0;
}

/*  Process the OpenACC 'routine' directive clauses to generate an attribute
    for the level of parallelism.  All dimensions have a size of zero
    (dynamic).  TREE_PURPOSE is set to indicate whether that dimension
    can have a loop partitioned on it.  non-zero indicates
    yes, zero indicates no.  By construction once a non-zero has been
    reached, further inner dimensions must also be non-zero.  We set
    TREE_VALUE to zero for the dimensions that may be partitioned and
    1 for the other ones -- if a loop is (erroneously) spawned at
    an outer level, we don't want to try and partition it.  */

tree
oacc_build_routine_dims (tree clauses)
{
  /* Must match GOMP_DIM ordering.  */
  static const omp_clause_code ids[]
    = {OMP_CLAUSE_GANG, OMP_CLAUSE_WORKER, OMP_CLAUSE_VECTOR, OMP_CLAUSE_SEQ};
  int ix;
  int level = -1;

  for (; clauses; clauses = OMP_CLAUSE_CHAIN (clauses))
    for (ix = GOMP_DIM_MAX + 1; ix--;)
      if (OMP_CLAUSE_CODE (clauses) == ids[ix])
	{
	  level = ix;
	  break;
	}
  gcc_checking_assert (level >= 0);

  tree dims = NULL_TREE;

  for (ix = GOMP_DIM_MAX; ix--;)
    dims = tree_cons (build_int_cst (boolean_type_node, ix >= level),
		      build_int_cst (integer_type_node, ix < level), dims);

  return dims;
}

/* Retrieve the oacc function attrib and return it.  Non-oacc
   functions will return NULL.  */

tree
oacc_get_fn_attrib (tree fn)
{
  return lookup_attribute (OACC_FN_ATTRIB, DECL_ATTRIBUTES (fn));
}

/* Return true if FN is an OpenMP or OpenACC offloading function.  */

bool
offloading_function_p (tree fn)
{
  tree attrs = DECL_ATTRIBUTES (fn);
  return (lookup_attribute ("omp declare target", attrs)
	  || lookup_attribute ("omp target entrypoint", attrs));
}

/* Extract an oacc execution dimension from FN.  FN must be an
   offloaded function or routine that has already had its execution
   dimensions lowered to the target-specific values.  */

int
oacc_get_fn_dim_size (tree fn, int axis)
{
  tree attrs = oacc_get_fn_attrib (fn);

  gcc_assert (axis < GOMP_DIM_MAX);

  tree dims = TREE_VALUE (attrs);
  while (axis--)
    dims = TREE_CHAIN (dims);

  int size = TREE_INT_CST_LOW (TREE_VALUE (dims));

  return size;
}

/* Extract the dimension axis from an IFN_GOACC_DIM_POS or
   IFN_GOACC_DIM_SIZE call.  */

int
oacc_get_ifn_dim_arg (const gimple *stmt)
{
  gcc_checking_assert (gimple_call_internal_fn (stmt) == IFN_GOACC_DIM_SIZE
		       || gimple_call_internal_fn (stmt) == IFN_GOACC_DIM_POS);
  tree arg = gimple_call_arg (stmt, 0);
  HOST_WIDE_INT axis = TREE_INT_CST_LOW (arg);

  gcc_checking_assert (axis >= 0 && axis < GOMP_DIM_MAX);
  return (int) axis;
}

#include "gt-omp-general.h"
