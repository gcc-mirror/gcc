/* General types and functions that are useful for processing of OpenMP,
   OpenACC and similar directives at various stages of compilation.

   Copyright (C) 2005-2025 Free Software Foundation, Inc.

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
#include "opts.h"
#include "tree-pretty-print.h"

enum omp_requires omp_requires_mask;

/* Find an OMP clause of type KIND within CLAUSES.  */
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

/* Return true if TYPE is an OpenMP mappable type.  */

bool
omp_mappable_type (tree type)
{
  /* Mappable type has to be complete.  */
  if (type == error_mark_node || !COMPLETE_TYPE_P (type))
    return false;
  return true;
}

/* True if OpenMP should privatize what this DECL points to rather
   than the DECL itself.  */

bool
omp_privatize_by_reference (tree decl)
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
      if (TREE_CODE (TREE_TYPE (v)) == INTEGER_TYPE
	  || TREE_CODE (TREE_TYPE (v)) == BITINT_TYPE)
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
	{
	  tree unit = TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (v)));
	  gcc_assert (TREE_CODE (unit) == INTEGER_CST);
	  *n2 = fold_build_pointer_plus_loc (loc, *n2, unit);
	}
      else
	*n2 = fold_build2_loc (loc, PLUS_EXPR, TREE_TYPE (*n2), *n2,
			       build_int_cst (TREE_TYPE (*n2), 1));
      *cond_code = LT_EXPR;
      break;
    case GE_EXPR:
      if (POINTER_TYPE_P (TREE_TYPE (*n2)))
	{
	  tree unit = TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (v)));
	  gcc_assert (TREE_CODE (unit) == INTEGER_CST);
	  unit = convert_to_ptrofftype_loc (loc, unit);
	  unit = fold_build1_loc (loc, NEGATE_EXPR, TREE_TYPE (unit),
				  unit);
	  *n2 = fold_build_pointer_plus_loc (loc, *n2, unit);
	}
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
  bool order_reproducible = false;
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
	if (OMP_CLAUSE_ORDERED_DOACROSS (t))
	  {
	    if (OMP_CLAUSE_ORDERED_EXPR (t))
	      fd->ordered = tree_to_shwi (OMP_CLAUSE_ORDERED_EXPR (t));
	    else
	      fd->ordered = -1;
	  }
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
      case OMP_CLAUSE_ORDER:
	/* FIXME: For OpenMP 5.2 this should change to
	   if (OMP_CLAUSE_ORDER_REPRODUCIBLE (t))
	   (with the exception of loop construct but that lowers to
	   no schedule/dist_schedule clauses currently).  */
	if (!OMP_CLAUSE_ORDER_UNCONSTRAINED (t))
	  order_reproducible = true;
      default:
	break;
      }

  if (fd->ordered == -1)
    fd->ordered = fd->collapse;

  /* For order(reproducible:concurrent) schedule ({dynamic,guided,runtime})
     we have either the option to expensively remember at runtime how we've
     distributed work from first loop and reuse that in following loops with
     the same number of iterations and schedule, or just force static schedule.
     OpenMP API calls etc. aren't allowed in order(concurrent) bodies so
     users can't observe it easily anyway.  */
  if (order_reproducible)
    fd->sched_kind = OMP_CLAUSE_SCHEDULE_STATIC;
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
		  || TREE_CODE (TREE_TYPE (loop->v)) == BITINT_TYPE
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
	    {
	      if (TREE_CODE (iter_type) == BITINT_TYPE
		  || TREE_CODE (TREE_TYPE (loop->v)) == BITINT_TYPE)
		iter_type
		  = build_bitint_type (TYPE_PRECISION (TREE_TYPE (loop->v)),
				       1);
	      else
		iter_type
		  = build_nonstandard_integer_type
			(TYPE_PRECISION (TREE_TYPE (loop->v)), 1);
	    }
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
		   && (TREE_CODE (TREE_TYPE (loop->v)) == INTEGER_TYPE
		       || TREE_CODE (TREE_TYPE (loop->v)) == BITINT_TYPE)
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
    case CLEANUP_POINT_EXPR:
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

/* Return maximum possible vectorization factor for the target, or for
   the OpenMP offload target if one exists.  */

poly_uint64
omp_max_vf (bool offload)
{
  if (!optimize
      || optimize_debug
      || !flag_tree_loop_optimize
      || (!flag_tree_loop_vectorize
	  && OPTION_SET_P (flag_tree_loop_vectorize)))
    return 1;

  if (ENABLE_OFFLOADING && offload)
    {
      for (const char *c = getenv ("OFFLOAD_TARGET_NAMES"); c;)
	{
	  if (startswith (c, "amdgcn"))
	    return ordered_max (poly_uint64 (64), omp_max_vf (false));
	  else if ((c = strchr (c, ':')))
	    c++;
	}
      /* Otherwise, fall through to host VF.  */
    }

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
omp_maybe_offloaded (tree construct_context)
{
  /* No offload targets available?  */
  if (!ENABLE_OFFLOADING)
    return false;
  const char *names = getenv ("OFFLOAD_TARGET_NAMES");
  if (names == NULL || *names == '\0')
    return false;

  /* Parsing is too early to tell.  */
  if (symtab->state == PARSING)
    /* Maybe.  */
    return true;

  /* Late resolution of offloaded code happens in the offload compiler,
     where it's treated as native code instead.  So return false here.  */
  if (cfun && cfun->after_inlining)
    return false;

  /* Check if the function is marked for offloading (either explicitly
     or via omp_discover_implicit_declare_target).  */
  if (current_function_decl
      && lookup_attribute ("omp declare target",
			   DECL_ATTRIBUTES (current_function_decl)))
    return true;

  /* Check for nesting inside a target directive.  */
  for (tree ts = construct_context; ts; ts = TREE_CHAIN (ts))
    if (OMP_TS_CODE (ts) == OMP_TRAIT_CONSTRUCT_TARGET)
      return true;

  return false;
}

/* Lookup tables for context selectors.  */
const char *omp_tss_map[] =
  {
   "construct",
   "device",
   "target_device",
   "implementation",
   "user",
   NULL
};

/* Arrays of property candidates must be null-terminated.  */
static const char *const kind_properties[] =
  { "host", "nohost", "cpu", "gpu", "fpga", "any", NULL };
static const char *const vendor_properties[] =
  { "amd", "arm", "bsc", "cray", "fujitsu", "gnu", "hpe", "ibm", "intel",
    "llvm", "nec", "nvidia", "pgi", "ti", "unknown", NULL };
static const char *const extension_properties[] =
  { NULL };
static const char *const atomic_default_mem_order_properties[] =
  { "seq_cst", "relaxed", "acq_rel", "acquire", "release", NULL };

struct omp_ts_info omp_ts_map[] =
  {
   { "kind",
     (1 << OMP_TRAIT_SET_DEVICE) | (1 << OMP_TRAIT_SET_TARGET_DEVICE),
     OMP_TRAIT_PROPERTY_NAME_LIST, false,
     kind_properties
   },
   { "isa",
     (1 << OMP_TRAIT_SET_DEVICE) | (1 << OMP_TRAIT_SET_TARGET_DEVICE),
     OMP_TRAIT_PROPERTY_NAME_LIST, false,
     NULL
   },
   { "arch",
     (1 << OMP_TRAIT_SET_DEVICE) | (1 << OMP_TRAIT_SET_TARGET_DEVICE),
     OMP_TRAIT_PROPERTY_NAME_LIST, false,
     NULL
   },
   { "device_num",
     (1 << OMP_TRAIT_SET_TARGET_DEVICE),
     OMP_TRAIT_PROPERTY_DEV_NUM_EXPR, false,
     NULL
   },
   { "vendor",
     (1 << OMP_TRAIT_SET_IMPLEMENTATION),
     OMP_TRAIT_PROPERTY_NAME_LIST, true,
     vendor_properties,
   },
   { "extension",
     (1 << OMP_TRAIT_SET_IMPLEMENTATION),
     OMP_TRAIT_PROPERTY_NAME_LIST, true,
     extension_properties,
   },
   { "atomic_default_mem_order",
     (1 << OMP_TRAIT_SET_IMPLEMENTATION),
     OMP_TRAIT_PROPERTY_ID, true,
     atomic_default_mem_order_properties,
   },
   { "requires",
     (1 << OMP_TRAIT_SET_IMPLEMENTATION),
     OMP_TRAIT_PROPERTY_CLAUSE_LIST, true,
     NULL
   },
   { "unified_address",
     (1 << OMP_TRAIT_SET_IMPLEMENTATION),
     OMP_TRAIT_PROPERTY_NONE, true,
     NULL
   },
   { "unified_shared_memory",
     (1 << OMP_TRAIT_SET_IMPLEMENTATION),
     OMP_TRAIT_PROPERTY_NONE, true,
     NULL
   },
   { "self_maps",
     (1 << OMP_TRAIT_SET_IMPLEMENTATION),
     OMP_TRAIT_PROPERTY_NONE, true,
     NULL
   },
   { "dynamic_allocators",
     (1 << OMP_TRAIT_SET_IMPLEMENTATION),
     OMP_TRAIT_PROPERTY_NONE, true,
     NULL
   },
   { "reverse_offload",
     (1 << OMP_TRAIT_SET_IMPLEMENTATION),
     OMP_TRAIT_PROPERTY_NONE, true,
     NULL
   },
   { "condition",
     (1 << OMP_TRAIT_SET_USER),
     OMP_TRAIT_PROPERTY_BOOL_EXPR, true,
     NULL
   },
   { "target",
     (1 << OMP_TRAIT_SET_CONSTRUCT),
     OMP_TRAIT_PROPERTY_NONE, false,
     NULL
   },
   { "teams",
     (1 << OMP_TRAIT_SET_CONSTRUCT),
     OMP_TRAIT_PROPERTY_NONE, false,
     NULL
   },
   { "parallel",
     (1 << OMP_TRAIT_SET_CONSTRUCT),
     OMP_TRAIT_PROPERTY_NONE, false,
     NULL
   },
   { "for",
     (1 << OMP_TRAIT_SET_CONSTRUCT),
     OMP_TRAIT_PROPERTY_NONE, false,
     NULL
   },
   { "simd",
     (1 << OMP_TRAIT_SET_CONSTRUCT),
     OMP_TRAIT_PROPERTY_CLAUSE_LIST,  false,
     NULL
   },
   { "dispatch",
     (1 << OMP_TRAIT_SET_CONSTRUCT),
     OMP_TRAIT_PROPERTY_NONE,  false,
     NULL
   },
   { NULL, 0, OMP_TRAIT_PROPERTY_NONE, false, NULL }  /* OMP_TRAIT_LAST */
  };

/* Return a name from PROP, a property in selectors accepting
   name lists.  */

const char *
omp_context_name_list_prop (tree prop)
{
  gcc_assert (OMP_TP_NAME (prop) == OMP_TP_NAMELIST_NODE);
  tree val = OMP_TP_VALUE (prop);
  switch (TREE_CODE (val))
    {
    case IDENTIFIER_NODE:
      return IDENTIFIER_POINTER (val);
    case STRING_CST:
#ifdef ACCEL_COMPILER
      return TREE_STRING_POINTER (val);
#else
      {
	const char *ret = TREE_STRING_POINTER (val);
	if ((size_t) TREE_STRING_LENGTH (val)
	    == strlen (ret) + (lang_GNU_Fortran () ? 0 : 1))
	  return ret;
	return NULL;
      }
#endif
    default:
      return NULL;
    }
}


/* Helper function called via walk_tree, to determine if *TP is a
   PARM_DECL.  */
static tree
expr_uses_parm_decl (tree *tp, int *walk_subtrees ATTRIBUTE_UNUSED,
		     void *data ATTRIBUTE_UNUSED)
{
  if (TREE_CODE (*tp) == PARM_DECL)
    return *tp;
  return NULL_TREE;
}

/* Diagnose errors in an OpenMP context selector, return CTX if
   it is correct or error_mark_node otherwise.  */

tree
omp_check_context_selector (location_t loc, tree ctx,
			    enum omp_ctx_directive directive)
{
  bool tss_seen[OMP_TRAIT_SET_LAST], ts_seen[OMP_TRAIT_LAST];

  memset (tss_seen, 0, sizeof (tss_seen));
  for (tree tss = ctx; tss; tss = TREE_CHAIN (tss))
    {
      enum omp_tss_code tss_code = OMP_TSS_CODE (tss);
      bool saw_any_prop = false;
      bool saw_other_prop = false;

      /* Each trait-set-selector-name can only be specified once.  */
      if (tss_seen[tss_code])
	{
	  error_at (loc, "selector set %qs specified more than once",
		    OMP_TSS_NAME (tss));
	  return error_mark_node;
	}
      else
	tss_seen[tss_code] = true;

      memset (ts_seen, 0, sizeof (ts_seen));
      for (tree ts = OMP_TSS_TRAIT_SELECTORS (tss); ts; ts = TREE_CHAIN (ts))
	{
	  enum omp_ts_code ts_code = OMP_TS_CODE (ts);

	  /* Ignore unknown traits.  */
	  if (ts_code == OMP_TRAIT_INVALID)
	    continue;

	  /* Each trait-selector-name can only be specified once.  */
	  if (ts_seen[ts_code])
	    {
	      error_at (loc,
			"selector %qs specified more than once in set %qs",
			OMP_TS_NAME (ts),
			OMP_TSS_NAME (tss));
	      return error_mark_node;
	    }
	  else
	    ts_seen[ts_code] = true;

	  /* If trait-property "any" is specified in the "kind"
	     trait-selector of the "device" selector set or the
	     "target_device" selector sets, no other trait-property
	     may be specified in the same selector set.  */
	  if (ts_code == OMP_TRAIT_DEVICE_KIND)
	    for (tree p = OMP_TS_PROPERTIES (ts); p; p = TREE_CHAIN (p))
	      {
		const char *prop = omp_context_name_list_prop (p);
		if (!prop)
		  continue;
		else if (strcmp (prop, "any") == 0)
		  saw_any_prop = true;
		else
		  saw_other_prop = true;
	      }
	  /* It seems slightly suspicious that the spec's language covers
	     the device_num selector too, but
	       target_device={device_num(whatever),kind(any)}
	     is probably not terribly useful anyway.  */
	  else if (ts_code == OMP_TRAIT_DEVICE_ARCH
		   || ts_code == OMP_TRAIT_DEVICE_ISA
		   || ts_code == OMP_TRAIT_DEVICE_NUM)
	    saw_other_prop = true;

	  /* Each trait-property can only be specified once in a trait-selector
	     other than the construct selector set.  FIXME: only handles
	     name-list properties, not clause-list properties, since the
	     "requires" selector is not implemented yet (PR 113067).  */
	  if (tss_code != OMP_TRAIT_SET_CONSTRUCT)
	    for (tree p1 = OMP_TS_PROPERTIES (ts); p1; p1 = TREE_CHAIN (p1))
	      {
		if (OMP_TP_NAME (p1) != OMP_TP_NAMELIST_NODE)
		  break;
		const char *n1 = omp_context_name_list_prop (p1);
		if (!n1)
		  continue;
		for (tree p2 = TREE_CHAIN (p1); p2; p2 = TREE_CHAIN (p2))
		  {
		    const char *n2 = omp_context_name_list_prop (p2);
		    if (!n2)
		      continue;
		    if (!strcmp (n1, n2))
		      {
			error_at (loc,
				  "trait-property %qs specified more "
				  "than once in %qs selector",
				  n1, OMP_TS_NAME (ts));
			return error_mark_node;
		      }
		  }
	      }

	  /* This restriction is documented in the spec in the section
	     for the metadirective "when" clause (7.4.1 in the 5.2 spec).  */
	  if (directive == OMP_CTX_METADIRECTIVE
	      && ts_code == OMP_TRAIT_CONSTRUCT_SIMD
	      && OMP_TS_PROPERTIES (ts))
	    {
	      error_at (loc,
			"properties must not be specified for the %<simd%> "
			"selector in a %<metadirective%> context-selector");
	      return error_mark_node;
	    }

	  /* "simd" is not allowed at all in "begin declare variant"
	     selectors.  */
	  if (directive == OMP_CTX_BEGIN_DECLARE_VARIANT
	      && ts_code == OMP_TRAIT_CONSTRUCT_SIMD)
	    {
	      error_at (loc,
			"the %<simd%> selector is not permitted in a "
			"%<begin declare variant%> context selector");
	      return error_mark_node;
	    }

	  /* Reject expressions that reference parameter variables in
	     "declare variant", as this is not yet implemented.  FIXME;
	     see PR middle-end/113904.  */
	  if (directive != OMP_CTX_METADIRECTIVE
	      && (ts_code == OMP_TRAIT_DEVICE_NUM
		  || ts_code == OMP_TRAIT_USER_CONDITION))
	    {
	      tree exp = OMP_TS_PROPERTIES (ts);
	      if (walk_tree (&exp, expr_uses_parm_decl, NULL, NULL))
		{
		  sorry_at (loc,
			    "reference to function parameter in "
			    "%<declare variant%> dynamic selector expression");
		  return error_mark_node;
		}
	    }

	  /* Check for unknown properties.  */
	  if (omp_ts_map[ts_code].valid_properties == NULL)
	    continue;
	  for (tree p = OMP_TS_PROPERTIES (ts); p; p = TREE_CHAIN (p))
	    for (unsigned j = 0; ; j++)
	      {
		const char *candidate
		  = omp_ts_map[ts_code].valid_properties[j];
		if (candidate == NULL)
		  {
		    /* We've reached the end of the candidate array.  */
		    if (ts_code == OMP_TRAIT_IMPLEMENTATION_ADMO)
		      /* FIXME: not sure why this is an error vs warnings
			 for the others, + incorrect/unknown wording?  */
		      {
			error_at (loc,
				  "incorrect property %qs of %qs selector",
				  IDENTIFIER_POINTER (OMP_TP_NAME (p)),
				  "atomic_default_mem_order");
			return error_mark_node;
		      }
		    if (OMP_TP_NAME (p) == OMP_TP_NAMELIST_NODE
			&& (TREE_CODE (OMP_TP_VALUE (p)) == STRING_CST))
		      warning_at (loc, OPT_Wopenmp,
				  "unknown property %qE of %qs selector",
				  OMP_TP_VALUE (p),
				  OMP_TS_NAME (ts));
		    else if (OMP_TP_NAME (p) == OMP_TP_NAMELIST_NODE)
		      warning_at (loc, OPT_Wopenmp,
				  "unknown property %qs of %qs selector",
				  omp_context_name_list_prop (p),
				  OMP_TS_NAME (ts));
		    else if (OMP_TP_NAME (p))
		      warning_at (loc, OPT_Wopenmp,
				  "unknown property %qs of %qs selector",
				  IDENTIFIER_POINTER (OMP_TP_NAME (p)),
				  OMP_TS_NAME (ts));
		    break;
		  }
		else if (OMP_TP_NAME (p) == OMP_TP_NAMELIST_NODE)
		  /* Property-list traits.  */
		  {
		    const char *str = omp_context_name_list_prop (p);
		    if (str && !strcmp (str, candidate))
		      break;
		  }
		else if (!strcmp (IDENTIFIER_POINTER (OMP_TP_NAME (p)),
				  candidate))
		  /* Identifier traits.  */
		  break;
	      }
	}

      if (saw_any_prop && saw_other_prop)
	{
	  error_at (loc,
		    "no other trait-property may be specified "
		    "in the same selector set with %<kind(\"any\")%>");
	  return error_mark_node;
	}
    }
  return ctx;
}

/* Forward declarations.  */
static int omp_context_selector_set_compare (enum omp_tss_code, tree, tree);
static int omp_construct_simd_compare (tree, tree, bool);

/* Register VARIANT as variant of some base function marked with
   #pragma omp declare variant.  CONSTRUCT is corresponding list of
   trait-selectors for the construct selector set.  This is stashed as the
   value of the "omp declare variant variant" attribute on VARIANT.  */
void
omp_mark_declare_variant (location_t loc, tree variant, tree construct)
{
  /* Ignore this variant if it contains unknown construct selectors.
     It will never match, and the front ends have already issued a warning
     about it.  */
  for (tree c = construct; c; c = TREE_CHAIN (c))
    if (OMP_TS_CODE (c) == OMP_TRAIT_INVALID)
      return;

  tree attr = lookup_attribute ("omp declare variant variant",
				DECL_ATTRIBUTES (variant));
  if (attr == NULL_TREE)
    {
      attr = tree_cons (get_identifier ("omp declare variant variant"),
			unshare_expr (construct),
			DECL_ATTRIBUTES (variant));
      DECL_ATTRIBUTES (variant) = attr;
      return;
    }
  if ((TREE_VALUE (attr) != NULL_TREE) != (construct != NULL_TREE)
      || (construct != NULL_TREE
	  && omp_context_selector_set_compare (OMP_TRAIT_SET_CONSTRUCT,
					       TREE_VALUE (attr),
					       construct)))
    error_at (loc, "%qD used as a variant with incompatible %<construct%> "
		   "selector sets", variant);
}


/* Constructors for context selectors.  */

tree
make_trait_set_selector (enum omp_tss_code code, tree selectors, tree chain)
{
  return tree_cons (build_int_cst (integer_type_node, code),
		    selectors, chain);
}

tree
make_trait_selector (enum omp_ts_code code, tree score, tree properties,
		     tree chain)
{
  if (score == NULL_TREE)
    return tree_cons (build_int_cst (integer_type_node, code),
		      properties, chain);
  else
    return tree_cons (build_int_cst (integer_type_node, code),
		      tree_cons (OMP_TS_SCORE_NODE, score, properties),
		      chain);
}

tree
make_trait_property (tree name, tree value, tree chain)
{
  return tree_cons (name, value, chain);
}

/* Constructor for metadirective variants.  */
tree
make_omp_metadirective_variant (tree selector, tree directive, tree body)
{
  return build_tree_list (selector, build_tree_list (directive, body));
}

/* If the construct selector traits SELECTOR_TRAITS match the corresponding
   OpenMP context traits CONTEXT_TRAITS, return true and set *SCORE to the
   corresponding score if it is non-null.  */
static bool
omp_construct_traits_match (tree selector_traits, tree context_traits,
			    score_wide_int *score)
{
  int slength = list_length (selector_traits);
  int clength = list_length (context_traits);

  /* Trivial failure: the selector has more traits than the OpenMP context.  */
  if (slength > clength)
    return false;

  /* There's only one trait in the selector and it doesn't have any properties
     to match.  */
  if (slength == 1 && !OMP_TS_PROPERTIES (selector_traits))
    {
      int p = 0, i = 1;
      enum omp_ts_code code = OMP_TS_CODE (selector_traits);
      for (tree t = context_traits; t; t = TREE_CHAIN (t), i++)
	if (OMP_TS_CODE (t) == code)
	  p = i;
      if (p != 0)
	{
	  if (score)
	    *score = wi::shifted_mask <score_wide_int> (p - 1, 1, false);
	  return true;
	}
      else
	return false;
    }

  /* Now handle the more general cases.
     Both lists of traits are ordered from outside in, corresponding to
     the c1, ..., cN numbering for the OpenMP context specified in
     in section 7.1 of the OpenMP 5.2 spec.  Section 7.3 of the spec says
     "if the traits that correspond to the construct selector set appear
     multiple times in the OpenMP context, the highest valued subset of
     context traits that contains all trait selectors in the same order
     are used".  This means that we want to start the search for a match
     from the end of the list, rather than the beginning.  To facilitate
     that, transfer the lists to temporary arrays to allow random access
     to the elements (their order remains outside in).  */
  int i, j;
  tree s, c;

  tree *sarray = (tree *) alloca (slength * sizeof (tree));
  for (s = selector_traits, i = 0; s; s = TREE_CHAIN (s), i++)
    sarray[i] = s;

  tree *carray = (tree *) alloca (clength * sizeof (tree));
  for (c = context_traits, j = 0; c; c = TREE_CHAIN (c), j++)
    carray[j] = c;

  /* The variable "i" indexes the selector, "j" indexes the OpenMP context.
     Find the "j" corresponding to each sarray[i].  Note that the spec uses
     "p" as the 1-based position, but "j" is zero-based, e.g. equal to
     p - 1.  */
  score_wide_int result = 0;
  j = clength - 1;
  for (i = slength - 1; i >= 0; i--)
    {
      enum omp_ts_code code = OMP_TS_CODE (sarray[i]);
      tree props = OMP_TS_PROPERTIES (sarray[i]);
      for (; j >= 0; j--)
	{
	  if (OMP_TS_CODE (carray[j]) != code)
	    continue;
	  if (code == OMP_TRAIT_CONSTRUCT_SIMD
	      && props
	      && omp_construct_simd_compare (props,
					     OMP_TS_PROPERTIES (carray[j]),
					     true) > 0)
	    continue;
	  break;
	}
      /* If j >= 0, we have a match for this trait at position j.  */
      if (j < 0)
	return false;
      result += wi::shifted_mask <score_wide_int> (j, 1, false);
      j--;
    }
  if (score)
    *score = result;
  return true;
}

/* Return 1 if context selector CTX matches the current OpenMP context, 0
   if it does not and -1 if it is unknown and need to be determined later.
   Some properties can be checked right away during parsing, others need
   to wait until the whole TU is parsed, others need to wait until
   IPA, others until vectorization.

   CONSTRUCT_CONTEXT is a list of construct traits from the OpenMP context,
   which must be collected by omp_get_construct_context during
   gimplification.  It is ignored (and may be null) if this function is
   called during parsing.  Otherwise COMPLETE_P should indicate whether
   CONSTRUCT_CONTEXT is known to be complete and not missing constructs
   filled in later during compilation.

   Dynamic properties (which are evaluated at run-time) should always
   return 1.  */

int
omp_context_selector_matches (tree ctx,
			      tree construct_context,
			      bool complete_p)
{
  int ret = 1;
  bool maybe_offloaded = omp_maybe_offloaded (construct_context);

  for (tree tss = ctx; tss; tss = TREE_CHAIN (tss))
    {
      enum omp_tss_code set = OMP_TSS_CODE (tss);
      tree selectors = OMP_TSS_TRAIT_SELECTORS (tss);

      /* Immediately reject the match if there are any ignored
	 selectors present.  */
      for (tree ts = selectors; ts; ts = TREE_CHAIN (ts))
	if (OMP_TS_CODE (ts) == OMP_TRAIT_INVALID)
	  return 0;

      if (set == OMP_TRAIT_SET_CONSTRUCT)
	{
	  /* We cannot resolve the construct selector during parsing because
	     the OpenMP context (and CONSTRUCT_CONTEXT) isn't available
	     until gimplification.  */
	  if (symtab->state == PARSING)
	    {
	      ret = -1;
	      continue;
	    }

	  gcc_assert (selectors);

	  /* During gimplification, CONSTRUCT_CONTEXT is partial, and doesn't
	     include a construct for "declare simd" that may be added
	     when there is not an enclosing "target" construct.  We might
	     be able to find a positive match against the partial context
	     (although we cannot yet score it accurately), but if we can't,
	     treat it as unknown instead of no match.  */
	  if (!omp_construct_traits_match (selectors, construct_context, NULL))
	    {
	      /* If we've got a complete context, it's definitely a failed
		 match.  */
	      if (complete_p)
		return 0;

	      /* If the selector doesn't include simd, then we don't have
		 to worry about whether "declare simd" would cause it to
		 match; so this is also a definite failure.  */
	      bool have_simd = false;
	      for (tree ts = selectors; ts; ts = TREE_CHAIN (ts))
		if (OMP_TS_CODE (ts) == OMP_TRAIT_CONSTRUCT_SIMD)
		  {
		    have_simd = true;
		    break;
		  }
	      if (!have_simd)
		return 0;
	      else
		ret = -1;
	    }
	  continue;
	}
      else if (set == OMP_TRAIT_SET_TARGET_DEVICE)
	/* The target_device set is dynamic, so treat it as always
	   resolvable.  However, the current implementation doesn't
	   support it in a target region, so diagnose that as an error.
	   FIXME: maybe make this a warning and return 0 instead?  */
	{
	  for (tree ts = construct_context; ts; ts = TREE_CHAIN (ts))
	    if (OMP_TS_CODE (ts) == OMP_TRAIT_CONSTRUCT_TARGET)
	      sorry ("%<target_device%> selector set inside of %<target%> "
		     "directive");
	  continue;
	}

      for (tree ts = selectors; ts; ts = TREE_CHAIN (ts))
	{
	  enum omp_ts_code sel = OMP_TS_CODE (ts);
	  switch (sel)
	    {
	    case OMP_TRAIT_IMPLEMENTATION_VENDOR:
	      gcc_assert (set == OMP_TRAIT_SET_IMPLEMENTATION);
	      for (tree p = OMP_TS_PROPERTIES (ts); p; p = TREE_CHAIN (p))
		{
		  const char *prop = omp_context_name_list_prop (p);
		  if (prop == NULL)
		    return 0;
		  if (!strcmp (prop, "gnu"))
		    continue;
		  return 0;
		}
	      break;
	    case OMP_TRAIT_IMPLEMENTATION_EXTENSION:
	      gcc_assert (set == OMP_TRAIT_SET_IMPLEMENTATION);
	      /* We don't support any extensions right now.  */
	      return 0;
	      break;
	    case OMP_TRAIT_IMPLEMENTATION_ADMO:
	      gcc_assert (set == OMP_TRAIT_SET_IMPLEMENTATION);
	      if (cfun && (cfun->curr_properties & PROP_gimple_any) != 0)
		break;

	      {
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
		tree p = OMP_TS_PROPERTIES (ts);
		const char *prop = IDENTIFIER_POINTER (OMP_TP_NAME (p));
		if (!strcmp (prop, "relaxed")
		    && omo != OMP_MEMORY_ORDER_RELAXED)
		  return 0;
		else if (!strcmp (prop, "seq_cst")
			 && omo != OMP_MEMORY_ORDER_SEQ_CST)
		  return 0;
		else if (!strcmp (prop, "acq_rel")
			 && omo != OMP_MEMORY_ORDER_ACQ_REL)
		  return 0;
		else if (!strcmp (prop, "acquire")
			 && omo != OMP_MEMORY_ORDER_ACQUIRE)
		  return 0;
		else if (!strcmp (prop, "release")
			 && omo != OMP_MEMORY_ORDER_RELEASE)
		  return 0;
	      }
	      break;
	    case OMP_TRAIT_DEVICE_ARCH:
	      gcc_assert (set == OMP_TRAIT_SET_DEVICE);
	      for (tree p = OMP_TS_PROPERTIES (ts); p; p = TREE_CHAIN (p))
		{
		  const char *arch = omp_context_name_list_prop (p);
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
			 also offloading values.
			 Note that maybe_offloaded is always false in late
			 resolution; that's handled as native code (the
			 above case) in the offload compiler instead.  */
		      if (!maybe_offloaded)
			return 0;
		      if (ENABLE_OFFLOADING)
			{
			  const char *arches = omp_offload_device_arch;
			  if (omp_offload_device_kind_arch_isa (arches, arch))
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
		  else if (maybe_offloaded)
		    ret = -1;
		}
	      break;
	    case OMP_TRAIT_IMPLEMENTATION_UNIFIED_ADDRESS:
	      gcc_assert (set == OMP_TRAIT_SET_IMPLEMENTATION);
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
	    case OMP_TRAIT_IMPLEMENTATION_UNIFIED_SHARED_MEMORY:
	      gcc_assert (set == OMP_TRAIT_SET_IMPLEMENTATION);
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
	    case OMP_TRAIT_IMPLEMENTATION_SELF_MAPS:
	      gcc_assert (set == OMP_TRAIT_SET_IMPLEMENTATION);
	      if (cfun && (cfun->curr_properties & PROP_gimple_any) != 0)
		break;

	      if ((omp_requires_mask & OMP_REQUIRES_SELF_MAPS) == 0)
		{
		  if (symtab->state == PARSING)
		    ret = -1;
		  else
		    return 0;
		}
	      break;
	    case OMP_TRAIT_IMPLEMENTATION_DYNAMIC_ALLOCATORS:
	      gcc_assert (set == OMP_TRAIT_SET_IMPLEMENTATION);
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
	    case OMP_TRAIT_IMPLEMENTATION_REVERSE_OFFLOAD:
	      gcc_assert (set == OMP_TRAIT_SET_IMPLEMENTATION);
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
	    case OMP_TRAIT_DEVICE_KIND:
	      gcc_assert (set == OMP_TRAIT_SET_DEVICE);
	      for (tree p = OMP_TS_PROPERTIES (ts); p; p = TREE_CHAIN (p))
		{
		  const char *prop = omp_context_name_list_prop (p);
		  if (prop == NULL)
		    return 0;
		  if (!strcmp (prop, "any"))
		    continue;
		  if (!strcmp (prop, "host"))
		    {
#ifdef ACCEL_COMPILER
		      return 0;
#else
		      if (maybe_offloaded)
			ret = -1;
		      continue;
#endif
		    }
		  if (!strcmp (prop, "nohost"))
		    {
#ifndef ACCEL_COMPILER
		      if (maybe_offloaded)
			ret = -1;
		      else
			return 0;
#endif
		      continue;
		    }

		  int r = 0;
		  if (targetm.omp.device_kind_arch_isa != NULL)
		    r = targetm.omp.device_kind_arch_isa (omp_device_kind,
							  prop);
		  else
#ifndef ACCEL_COMPILER
		    r = strcmp (prop, "cpu") == 0;
#else
		    gcc_unreachable ();
#endif
		    if (r == 0 || (r == -1 && symtab->state != PARSING))
		    {
		      /* If we are or might be in a target region or
			 declare target function, need to take into account
			 also offloading values.
			 Note that maybe_offloaded is always false in late
			 resolution; that's handled as native code (the
			 above case) in the offload compiler instead.  */
		      if (!maybe_offloaded)
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
		  else if (maybe_offloaded)
		    ret = -1;
		}
	      break;
	    case OMP_TRAIT_DEVICE_ISA:
	      gcc_assert (set == OMP_TRAIT_SET_DEVICE);
	      for (tree p = OMP_TS_PROPERTIES (ts); p; p = TREE_CHAIN (p))
		{
		  const char *isa = omp_context_name_list_prop (p);
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
			 also offloading values.
			 Note that maybe_offloaded is always false in late
			 resolution; that's handled as native code (the
			 above case) in the offload compiler instead.  */
		      if (!maybe_offloaded)
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
		  else if (maybe_offloaded)
		    ret = -1;
		}
	      break;
	    case OMP_TRAIT_USER_CONDITION:
	      gcc_assert (set == OMP_TRAIT_SET_USER);
	      for (tree p = OMP_TS_PROPERTIES (ts); p; p = TREE_CHAIN (p))
		if (OMP_TP_NAME (p) == NULL_TREE)
		  {
		    /* If the expression is not a constant, the selector
		       is dynamic.  */
		    if (!tree_fits_shwi_p (OMP_TP_VALUE (p)))
		      break;

		    if (integer_zerop (OMP_TP_VALUE (p)))
		      return 0;
		    if (integer_nonzerop (OMP_TP_VALUE (p)))
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

/* Helper function for resolve_omp_target_device_matches, also used
   directly when we know in advance that the device is the host to avoid
   the overhead of late resolution.  SEL is the selector code and
   PROPERTIES are the properties to match.  The return value is a
   boolean.  */
static bool
omp_target_device_matches_on_host (enum omp_ts_code selector,
				   tree properties)
{
  bool result = 1;

  if (dump_file)
    fprintf (dump_file, "omp_target_device_matches_on_host:\n");

  switch (selector)
    {
    case OMP_TRAIT_DEVICE_KIND:
      for (tree p = properties; p && result; p = TREE_CHAIN (p))
	{
	  const char *prop = omp_context_name_list_prop (p);

	  if (prop == NULL)
	    result = 0;
	  else if (!strcmp (prop, "any"))
	    ;
	  else if (!strcmp (prop, "host"))
	    {
#ifdef ACCEL_COMPILER
	      result = 0;
#else
	      ;
#endif
	    }
	  else if (!strcmp (prop, "nohost"))
	    {
#ifdef ACCEL_COMPILER
	      ;
#else
	      result = 0;
#endif
	    }
	  else if (targetm.omp.device_kind_arch_isa != NULL)
	    result = targetm.omp.device_kind_arch_isa (omp_device_kind, prop);
	  else
#ifndef ACCEL_COMPILER
	    result = strcmp (prop, "cpu") == 0;
#else
	    gcc_unreachable ();
#endif
	  if (dump_file)
	    fprintf (dump_file, "Matching device kind %s = %s\n",
		     prop, (result ? "true" : "false"));
	}
      break;
    case OMP_TRAIT_DEVICE_ARCH:
      if (targetm.omp.device_kind_arch_isa != NULL)
	for (tree p = properties; p && result; p = TREE_CHAIN (p))
	  {
	    const char *prop = omp_context_name_list_prop (p);
	    if (prop == NULL)
	      result = 0;
	    else
	      result = targetm.omp.device_kind_arch_isa (omp_device_arch,
							 prop);
	    if (dump_file)
	      fprintf (dump_file, "Matching device arch %s = %s\n",
		       prop, (result ? "true" : "false"));
	  }
      else
	{
	  result = 0;
	  if (dump_file)
	    fprintf (dump_file, "Cannot match device arch on target\n");
	}
      break;
    case OMP_TRAIT_DEVICE_ISA:
      if (targetm.omp.device_kind_arch_isa != NULL)
	for (tree p = properties; p && result; p = TREE_CHAIN (p))
	  {
	    const char *prop = omp_context_name_list_prop (p);
	    if (prop == NULL)
	      result = 0;
	    else
	      result = targetm.omp.device_kind_arch_isa (omp_device_isa,
							 prop);
	    if (dump_file)
	      fprintf (dump_file, "Matching device isa %s = %s\n",
		       prop, (result ? "true" : "false"));
	  }
      else
	{
	  result = 0;
	  if (dump_file)
	    fprintf (dump_file, "Cannot match device isa on target\n");
	}
      break;
    default:
      gcc_unreachable ();
    }
  return result;
}

/* Called for late resolution of the OMP_TARGET_DEVICE_MATCHES tree node to
   a constant in omp-offload.cc.  This is used in code that is wrapped in a
   #pragma omp target construct to execute on the specified device, and
   can be reduced to a compile-time constant in the offload compiler.
   NODE is an OMP_TARGET_DEVICE_MATCHES tree node and the result is an
   INTEGER_CST.  */
tree
resolve_omp_target_device_matches (tree node)
{
  tree sel = OMP_TARGET_DEVICE_MATCHES_SELECTOR (node);
  enum omp_ts_code selector = (enum omp_ts_code) tree_to_shwi (sel);
  tree properties = OMP_TARGET_DEVICE_MATCHES_PROPERTIES (node);
  if (omp_target_device_matches_on_host (selector, properties))
    return integer_one_node;
  else
    return integer_zero_node;
}

/* Compare construct={simd} CLAUSES1 with CLAUSES2, return 0/-1/1/2 as
   in omp_context_selector_set_compare.  If MATCH_P is true, additionally
   apply the special matching rules for the "simdlen" and "aligned" clauses
   used to determine whether the selector CLAUSES1 is part of matches
   the OpenMP context containing CLAUSES2.  */

static int
omp_construct_simd_compare (tree clauses1, tree clauses2, bool match_p)
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
  tree e0, e1;
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
  e0 = data[0].simdlen;
  e1 = data[1].simdlen;
  if (!simple_cst_equal (e0, e1))
    {
      if (e0 && e1)
	{
	  if (match_p && tree_fits_uhwi_p (e0) && tree_fits_uhwi_p (e1))
	    {
	      /* The two simdlen clauses match if m is a multiple of n.  */
	      unsigned HOST_WIDE_INT n = tree_to_uhwi (e0);
	      unsigned HOST_WIDE_INT m = tree_to_uhwi (e1);
	      if (m % n != 0)
		return 2;
	    }
	  else
	    return 2;
	}
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
      e0 = OMP_CLAUSE_ALIGNED_ALIGNMENT (c1);
      e1 = OMP_CLAUSE_ALIGNED_ALIGNMENT (c2);
      if (!simple_cst_equal (e0, e1))
	{
	  if (e0 && e1
	      && match_p && tree_fits_uhwi_p (e0) && tree_fits_uhwi_p (e1))
	    {
	      /* The two aligned clauses match if n is a multiple of m.  */
	      unsigned HOST_WIDE_INT n = tree_to_uhwi (e0);
	      unsigned HOST_WIDE_INT m = tree_to_uhwi (e1);
	      if (n % m != 0)
		return 2;
	    }
	  else
	    return 2;
	}
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
   CTX1 and CTX2 are the lists of properties to compare.
   Return 0/-1/1/2 as in omp_context_selector_set_compare.
   Unlike set names or selector names, properties can have duplicates.  */

static int
omp_context_selector_props_compare (enum omp_tss_code set,
				    enum omp_ts_code sel,
				    tree ctx1, tree ctx2)
{
  int ret = 0;
  for (int pass = 0; pass < 2; pass++)
    for (tree p1 = pass ? ctx2 : ctx1; p1; p1 = TREE_CHAIN (p1))
      {
	tree p2;
	for (p2 = pass ? ctx1 : ctx2; p2; p2 = TREE_CHAIN (p2))
	  if (OMP_TP_NAME (p1) == OMP_TP_NAME (p2))
	    {
	      if (OMP_TP_NAME (p1) == NULL_TREE)
		{
		  if (set == OMP_TRAIT_SET_USER
		      && sel == OMP_TRAIT_USER_CONDITION)
		    {
		      /* Recognize constants that have equal truth values,
			 otherwise assume all expressions are unique.  */
		      tree v1 = OMP_TP_VALUE (p1);
		      tree v2 = OMP_TP_VALUE (p2);
		      if (TREE_CODE (v1) != INTEGER_CST
			  || TREE_CODE (v2) != INTEGER_CST
			  || integer_zerop (v1) != integer_zerop (v2))
			return 2;
		      break;
		    }
		  if (set == OMP_TRAIT_SET_TARGET_DEVICE
		      && sel == OMP_TRAIT_DEVICE_NUM)
		    {
		      /* Recognize constants that have equal values,
			 otherwise assume all expressions are unique.  */
		      tree v1 = OMP_TP_VALUE (p1);
		      tree v2 = OMP_TP_VALUE (p2);
		      if (TREE_CODE (v1) != INTEGER_CST
			  || TREE_CODE (v2) != INTEGER_CST
			  || tree_int_cst_compare (v1, v2) != 0)
			return 2;
		      break;
		    }
		  if (simple_cst_equal (OMP_TP_VALUE (p1), OMP_TP_VALUE (p2)))
		    break;
		}
	      else if (OMP_TP_NAME (p1) == OMP_TP_NAMELIST_NODE)
		{
		  /* Handle string constant vs identifier comparison for
		     name-list properties.  */
		  const char *n1 = omp_context_name_list_prop (p1);
		  const char *n2 = omp_context_name_list_prop (p2);
		  if (n1 && n2 && !strcmp (n1, n2))
		    break;
		}
	      else
		break;
	    }
	if (p2 == NULL_TREE)
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
   CTX1 and CTX2 are lists of trait-selectors.
   Return 0 if CTX1 is equal to CTX2,
   -1 if CTX1 is a strict subset of CTX2,
   1 if CTX2 is a strict subset of CTX1, or
   2 if neither context is a subset of another one.  */

static int
omp_context_selector_set_compare (enum omp_tss_code set, tree ctx1, tree ctx2)
{

  /* If either list includes an ignored selector trait, neither can
     be a subset of the other.  */
  for (tree ts = ctx1; ts; ts = TREE_CHAIN (ts))
    if (OMP_TS_CODE (ts) == OMP_TRAIT_INVALID)
      return 2;
  for (tree ts = ctx2; ts; ts = TREE_CHAIN (ts))
    if (OMP_TS_CODE (ts) == OMP_TRAIT_INVALID)
      return 2;

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

  if (set == OMP_TRAIT_SET_CONSTRUCT)
    {
      tree ts1;
      tree ts2 = ctx2;
      /* Handle construct set specially.  In this case the order
	 of the selector matters too.  */
      for (ts1 = ctx1; ts1; ts1 = TREE_CHAIN (ts1))
	if (OMP_TS_CODE (ts1) == OMP_TS_CODE (ts2))
	  {
	    int r = 0;
	    if (OMP_TS_CODE (ts1) == OMP_TRAIT_CONSTRUCT_SIMD)
	      r = omp_construct_simd_compare (OMP_TS_PROPERTIES (ts1),
					      OMP_TS_PROPERTIES (ts2),
					      false);
	    if (r == 2 || (ret && r && (ret < 0) != (r < 0)))
	      return 2;
	    if (ret == 0)
	      ret = r;
	    ts2 = TREE_CHAIN (ts2);
	    if (ts2 == NULL_TREE)
	      {
		ts1 = TREE_CHAIN (ts1);
		break;
	      }
	  }
	else if (ret < 0)
	  return 2;
	else
	  ret = 1;
      if (ts2 != NULL_TREE)
	return 2;
      if (ts1 != NULL_TREE)
	{
	  if (ret < 0)
	    return 2;
	  ret = 1;
	}
      if (ret == 0)
	return 0;
      return swapped ? -ret : ret;
    }
  for (tree ts1 = ctx1; ts1; ts1 = TREE_CHAIN (ts1))
    {
      enum omp_ts_code sel = OMP_TS_CODE (ts1);
      tree ts2;
      for (ts2 = ctx2; ts2; ts2 = TREE_CHAIN (ts2))
	if (sel == OMP_TS_CODE (ts2))
	  {
	    tree score1 = OMP_TS_SCORE (ts1);
	    tree score2 = OMP_TS_SCORE (ts2);
	    if ((score1 && score2 && !simple_cst_equal (score1, score2))
		|| (score1 && !score2)
		|| (!score1 && score2))
	      return 2;

	    int r = omp_context_selector_props_compare (set, OMP_TS_CODE (ts1),
							OMP_TS_PROPERTIES (ts1),
							OMP_TS_PROPERTIES (ts2));
	    if (r == 2 || (ret && r && (ret < 0) != (r < 0)))
	      return 2;
	    if (ret == 0)
	      ret = r;
	    cnt++;
	    break;
	  }
      if (ts2 == NULL_TREE)
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
  for (tree tss1 = ctx1; tss1; tss1 = TREE_CHAIN (tss1))
    {
      enum omp_tss_code set = OMP_TSS_CODE (tss1);
      tree tss2;
      for (tss2 = ctx2; tss2; tss2 = TREE_CHAIN (tss2))
	if (set == OMP_TSS_CODE (tss2))
	  {
	    int r
	      = omp_context_selector_set_compare
		  (set, OMP_TSS_TRAIT_SELECTORS (tss1),
		   OMP_TSS_TRAIT_SELECTORS (tss2));
	    if (r == 2 || (ret && r && (ret < 0) != (r < 0)))
	      return 2;
	    if (ret == 0)
	      ret = r;
	    cnt++;
	    break;
	  }
      if (tss2 == NULL_TREE)
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
   trait-selector-set with name SET if any, or NULL_TREE if not found.  */
tree
omp_get_context_selector (tree ctx, enum omp_tss_code set,
			  enum omp_ts_code sel)
{
  for (tree tss = ctx; tss; tss = TREE_CHAIN (tss))
    if (OMP_TSS_CODE (tss) == set)
      for (tree ts = OMP_TSS_TRAIT_SELECTORS (tss); ts; ts = TREE_CHAIN (ts))
	if (OMP_TS_CODE (ts) == sel)
	  return ts;
  return NULL_TREE;
}

/* Similar, but returns the whole trait-selector list for SET in CTX.  */
tree
omp_get_context_selector_list (tree ctx, enum omp_tss_code set)
{
  for (tree tss = ctx; tss; tss = TREE_CHAIN (tss))
    if (OMP_TSS_CODE (tss) == set)
      return OMP_TSS_TRAIT_SELECTORS (tss);
  return NULL_TREE;
}

/* Map string S onto a trait selector set code.  */
enum omp_tss_code
omp_lookup_tss_code (const char * s)
{
  for (int i = 0; i < OMP_TRAIT_SET_LAST; i++)
    if (strcmp (s, omp_tss_map[i]) == 0)
      return (enum omp_tss_code) i;
  return OMP_TRAIT_SET_INVALID;
}

/* Map string S onto a trait selector code for set SET.  */
enum omp_ts_code
omp_lookup_ts_code (enum omp_tss_code set, const char *s)
{
  unsigned int mask = 1 << set;
  for (int i = 0; i < OMP_TRAIT_LAST; i++)
    if ((mask & omp_ts_map[i].tss_mask) != 0
	&& strcmp (s, omp_ts_map[i].name) == 0)
      return (enum omp_ts_code) i;
  return OMP_TRAIT_INVALID;
}


/* Return true if the selector CTX is dynamic.  */
static bool
omp_selector_is_dynamic (tree ctx)
{
  tree user_sel = omp_get_context_selector (ctx, OMP_TRAIT_SET_USER,
					    OMP_TRAIT_USER_CONDITION);
  if (user_sel)
    {
      tree expr = OMP_TP_VALUE (OMP_TS_PROPERTIES (user_sel));

      /* The user condition is not dynamic if it is constant.  */
      if (!tree_fits_shwi_p (expr))
	return true;
    }

  tree target_device_ss
    = omp_get_context_selector_list (ctx, OMP_TRAIT_SET_TARGET_DEVICE);
  if (target_device_ss)
    return true;

  return false;
}

/* Helper function for omp_dynamic_cond: return a boolean tree expression
   that tests whether *DEVICE_NUM is a "conforming device number other
   than omp_invalid_device".  This may modify *DEVICE_NUM (i.e, to be
   a save_expr).  *IS_HOST is set to true if the device can be statically
   determined to be the host.  */

static tree
omp_device_num_check (tree *device_num, bool *is_host)
{
  /* First check for some constant values we can treat specially.  */
  if (tree_fits_shwi_p (*device_num))
    {
      HOST_WIDE_INT num = tree_to_shwi (*device_num);
      if (num < -1)
	return integer_zero_node;
      /* Initial device?  */
      if (num == -1)
	{
	  *is_host = true;
	  return integer_one_node;
	}
      /* There is always at least one device (the host + offload devices).  */
      if (num == 0)
	return integer_one_node;
      /* If there is no offloading, there is exactly one device.  */
      if (!ENABLE_OFFLOADING && num > 0)
	return integer_zero_node;
    }

  /* Also test for direct calls to OpenMP routines that return valid
     device numbers.  */
  if (TREE_CODE (*device_num) == CALL_EXPR)
    {
      tree fndecl = get_callee_fndecl (*device_num);
      if (fndecl && omp_runtime_api_call (fndecl))
	{
	  const char *fnname = IDENTIFIER_POINTER (DECL_NAME (fndecl));
	  if (strcmp (fnname, "omp_get_default_device") == 0
	      || strcmp (fnname, "omp_get_device_num") == 0)
	    return integer_one_node;
	  if (strcmp (fnname, "omp_get_num_devices") == 0
	      || strcmp (fnname, "omp_get_initial_device") == 0)
	    {
	      *is_host = true;
	      return integer_one_node;
	    }
	}
    }

  /* Otherwise, test that -1 <= *device_num <= omp_get_num_devices ().  */
  *device_num = save_expr (*device_num);
  tree lotest = build2 (GE_EXPR, integer_type_node, *device_num,
			integer_minus_one_node);
  tree fndecl = builtin_decl_explicit (BUILT_IN_OMP_GET_NUM_DEVICES);
  tree hitest = build2 (LE_EXPR, integer_type_node, *device_num,
			build_call_expr (fndecl, 0));
  return build2 (TRUTH_ANDIF_EXPR, integer_type_node, lotest, hitest);
}

/* Return a tree expression representing the dynamic part of the context
   selector CTX.  SUPERCONTEXT is the surrounding BLOCK, in case we need
   to introduce a new BLOCK in the result.  */
tree
omp_dynamic_cond (tree ctx, tree supercontext)
{
  tree user_cond = NULL_TREE, target_device_cond = NULL_TREE;

  /* Build the "user" part of the dynamic selector.  This is a test
     predicate taken directly for the "condition" trait in this set.  */
  tree user_sel = omp_get_context_selector (ctx, OMP_TRAIT_SET_USER,
					    OMP_TRAIT_USER_CONDITION);
  if (user_sel)
    {
      tree expr = OMP_TP_VALUE (OMP_TS_PROPERTIES (user_sel));

      /* The user condition is not dynamic if it is constant.  */
      if (!tree_fits_shwi_p (expr))
	user_cond = expr;
    }

  /* Build the "target_device" part of the dynamic selector.  In the
     most general case this requires building a bit of code that runs
     on the specified device_num using the same mechanism as
     "#pragma omp target" that uses the OMP_TARGET_DEVICE_MATCHES magic
     cookie to represent the kind/arch/isa tests which are and'ed together.
     These cookies can be resolved into a constant truth value by the
     offload compiler; see resolve_omp_target_device_matches, above.

     In some cases, we can (in)validate the device number in advance.
     If it is not valid, the whole selector fails to match.  If it is
     valid and refers to the host (e.g., constant -1), then we can
     resolve the match to a constant truth value now instead of having
     to create a OMP_TARGET_DEVICE_MATCHES.  */

  tree target_device_ss
    = omp_get_context_selector_list (ctx, OMP_TRAIT_SET_TARGET_DEVICE);
  if (target_device_ss)
    {
      tree device_num = NULL_TREE;
      tree kind = NULL_TREE;
      tree arch = NULL_TREE;
      tree isa = NULL_TREE;
      tree device_ok = NULL_TREE;
      bool is_host = !ENABLE_OFFLOADING;

      tree device_num_sel
	= omp_get_context_selector (ctx, OMP_TRAIT_SET_TARGET_DEVICE,
				    OMP_TRAIT_DEVICE_NUM);
      if (device_num_sel)
	{
	  device_num = OMP_TP_VALUE (OMP_TS_PROPERTIES (device_num_sel));
	  device_ok = omp_device_num_check (&device_num, &is_host);
	  /* If an invalid constant device number was specified, the
	     whole selector fails to match, and there's no point in
	     continuing to generate code that would never be executed.  */
	  if (device_ok == integer_zero_node)
	    {
	      target_device_cond = integer_zero_node;
	      goto wrapup;
	    }
	}

      tree kind_sel
	= omp_get_context_selector (ctx, OMP_TRAIT_SET_TARGET_DEVICE,
				    OMP_TRAIT_DEVICE_KIND);
      /* "any" is equivalent to omitting this trait selector.  */
      if (kind_sel
	  && strcmp (omp_context_name_list_prop (OMP_TS_PROPERTIES (kind_sel)),
		     "any"))
	{
	  tree props = OMP_TS_PROPERTIES (kind_sel);
	  if (!is_host)
	    kind = build2 (OMP_TARGET_DEVICE_MATCHES, integer_type_node,
			   build_int_cst (integer_type_node,
					  (int) OMP_TRAIT_DEVICE_KIND),
			   props);
	  else if (!omp_target_device_matches_on_host (OMP_TRAIT_DEVICE_KIND,
						       props))
	    {
	      /* The whole selector fails to match.  */
	      target_device_cond = integer_zero_node;
	      goto wrapup;
	    }
	  /* else it is statically resolved to true and is a no-op.  */
	}
      tree arch_sel
	= omp_get_context_selector (ctx, OMP_TRAIT_SET_TARGET_DEVICE,
				    OMP_TRAIT_DEVICE_ARCH);
      if (arch_sel)
	{
	  tree props = OMP_TS_PROPERTIES (arch_sel);
	  if (!is_host)
	    arch = build2 (OMP_TARGET_DEVICE_MATCHES, integer_type_node,
			   build_int_cst (integer_type_node,
					  (int) OMP_TRAIT_DEVICE_ARCH),
			   props);
	  else if (!omp_target_device_matches_on_host (OMP_TRAIT_DEVICE_ARCH,
						       props))
	    {
	      /* The whole selector fails to match.  */
	      target_device_cond = integer_zero_node;
	      goto wrapup;
	    }
	  /* else it is statically resolved to true and is a no-op.  */
	}

      tree isa_sel
	= omp_get_context_selector (ctx, OMP_TRAIT_SET_TARGET_DEVICE,
				    OMP_TRAIT_DEVICE_ISA);
      if (isa_sel)
	{
	  tree props = OMP_TS_PROPERTIES (isa_sel);
	  if (!is_host)
	    isa = build2 (OMP_TARGET_DEVICE_MATCHES, integer_type_node,
			  build_int_cst (integer_type_node,
					 (int) OMP_TRAIT_DEVICE_ISA),
			  props);
	  else if (!omp_target_device_matches_on_host (OMP_TRAIT_DEVICE_ISA,
						       props))
	    {
	      /* The whole selector fails to match.  */
	      target_device_cond = integer_zero_node;
	      goto wrapup;
	    }
	  /* else it is statically resolved to true and is a no-op.  */
	}

      /* AND the three possible tests together.  */
      tree test_expr = kind ? kind : NULL_TREE;
      if (arch && test_expr)
	test_expr = build2 (TRUTH_ANDIF_EXPR, integer_type_node,
			    arch, test_expr);
      else if (arch)
	test_expr = arch;
      if (isa && test_expr)
	test_expr = build2 (TRUTH_ANDIF_EXPR, integer_type_node,
			    isa, test_expr);
      else if (isa)
	test_expr = isa;

      if (!test_expr)
	/* This could happen if the selector includes only kind="any",
	   or is_host is true and it could be statically determined to
	   be true.  The selector always matches, but we still have to
	   evaluate the device_num expression.  */
	{
	  if (device_num)
	    target_device_cond = build2 (COMPOUND_EXPR, integer_type_node,
					 device_num, integer_one_node);
	  else
	    target_device_cond = integer_one_node;
	}
      else
	{
	  /* Arrange to evaluate test_expr in the offload compiler for
	     device device_num.  */
	  tree stmt = make_node (OMP_TARGET);
	  TREE_TYPE (stmt) = void_type_node;
	  tree result_var = create_tmp_var (integer_type_node, "td_match");
	  tree map = build_omp_clause (UNKNOWN_LOCATION, OMP_CLAUSE_MAP);
	  OMP_CLAUSE_DECL (map) = result_var;
	  OMP_CLAUSE_SET_MAP_KIND (map, GOMP_MAP_FROM);
	  OMP_TARGET_CLAUSES (stmt) = map;
	  if (device_num)
	    {
	      tree clause = build_omp_clause (UNKNOWN_LOCATION,
					      OMP_CLAUSE_DEVICE);
	      OMP_CLAUSE_CHAIN (clause) = NULL_TREE;
	      OMP_CLAUSE_DEVICE_ID (clause) = device_num;
	      OMP_CLAUSE_DEVICE_ANCESTOR (clause) = false;
	      OMP_CLAUSE_CHAIN (map) = clause;
	    }

	  tree block = make_node (BLOCK);
	  BLOCK_SUPERCONTEXT (block) = supercontext;

	  tree bind = build3 (BIND_EXPR, void_type_node, NULL_TREE,
			      build2 (MODIFY_EXPR, integer_type_node,
				      result_var, test_expr),
			      block);
	  TREE_SIDE_EFFECTS (bind) = 1;
	  OMP_TARGET_BODY (stmt) = bind;
	  target_device_cond = build2 (COMPOUND_EXPR, integer_type_node,
				       stmt, result_var);

	  /* If necessary, "and" target_device_cond with the test to
	     make sure the device number is valid.  */
	  if (device_ok && device_ok != integer_one_node)
	    target_device_cond = build2 (TRUTH_ANDIF_EXPR, integer_type_node,
					 device_ok, target_device_cond);

	  /* Set the bit to trigger resolution of OMP_TARGET_DEVICE_MATCHES
	     in the ompdevlow pass.  */
	  if (cfun && (cfun->curr_properties & PROP_gimple_any) != 0)
	    cgraph_node::get (cfun->decl)->has_omp_variant_constructs = 1;
	}
    }

 wrapup:
  if (user_cond && target_device_cond)
    return build2 (TRUTH_ANDIF_EXPR, integer_type_node,
		   user_cond, target_device_cond);
  else if (user_cond)
    return user_cond;
  else if (target_device_cond)
    return target_device_cond;
  else
    return NULL_TREE;
}


/* Given an omp_variant VARIANT, compute VARIANT->score and
   VARIANT->scorable.
   CONSTRUCT_CONTEXT is the OpenMP construct context; if this is null or
   COMPLETE_P is false (e.g., during parsing or gimplification) then it
   may not be possible to compute the score accurately and the scorable
   flag is set to false.

   Cited text in the comments is from section 7.2 of the OpenMP 5.2
   specification.  */

static void
omp_context_compute_score (struct omp_variant *variant,
			   tree construct_context, bool complete_p)
{
  int l = list_length (construct_context);
  tree ctx = variant->selector;
  variant->scorable = true;

  /* "the final score is the sum of the values of all specified selectors
     plus 1".  */
  variant->score = 1;
  for (tree tss = ctx; tss; tss = TREE_CHAIN (tss))
    {
      if (OMP_TSS_CODE (tss) == OMP_TRAIT_SET_CONSTRUCT)
	{
	  /* "Each trait selector for which the corresponding trait appears
	     in the context trait set in the OpenMP context..."  */
	  score_wide_int tss_score = 0;
	  omp_construct_traits_match (OMP_TSS_TRAIT_SELECTORS (tss),
				      construct_context, &tss_score);
	  variant->score += tss_score;
	  if (!complete_p)
	    variant->scorable = false;
	}
      else if (OMP_TSS_CODE (tss) == OMP_TRAIT_SET_DEVICE
	       || OMP_TSS_CODE (tss) == OMP_TRAIT_SET_TARGET_DEVICE)
	{
	  /* "The kind, arch, and isa selectors, if specified, are given
	     the values 2**l, 2**(l+1), and 2**(l+2), respectively..."
	     FIXME: the spec isn't clear what should happen if there are
	     both "device" and "target_device" selector sets specified.
	     This implementation adds up the bits rather than ORs them.  */
	  for (tree ts = OMP_TSS_TRAIT_SELECTORS (tss); ts;
	       ts = TREE_CHAIN (ts))
	    {
	      enum omp_ts_code code = OMP_TS_CODE (ts);
	      if (code == OMP_TRAIT_DEVICE_KIND)
		variant->score
		  += wi::shifted_mask <score_wide_int> (l, 1, false);
	      else if (code == OMP_TRAIT_DEVICE_ARCH)
		variant->score
		  += wi::shifted_mask <score_wide_int> (l + 1, 1, false);
	      else if (code == OMP_TRAIT_DEVICE_ISA)
		variant->score
		  += wi::shifted_mask <score_wide_int> (l + 2, 1, false);
	    }
	  if (!complete_p)
	    variant->scorable = false;
	}
      else
	{
	  /* "Trait selectors for which a trait-score is specified..."
	     Note that there are no implementation-defined selectors, and
	     "other selectors are given a value of zero".  */
	  for (tree ts = OMP_TSS_TRAIT_SELECTORS (tss); ts;
	       ts = TREE_CHAIN (ts))
	    {
	      tree s = OMP_TS_SCORE (ts);
	      if (s && TREE_CODE (s) == INTEGER_CST)
		variant->score
		  += score_wide_int::from (wi::to_wide (s),
					   TYPE_SIGN (TREE_TYPE (s)));
	    }
	}
    }
}

/* CONSTRUCT_CONTEXT contains "the directive names, each being a trait,
   of all enclosing constructs at that point in the program up to a target
   construct", per section 7.1 of the 5.2 specification.  The traits are
   collected during gimplification and are listed outermost first.

   This function attempts to apply the "if the point in the program is not
   enclosed by a target construct, the following rules are applied in order"
   requirements that follow in the same paragraph.  This may not be possible,
   depending on the compilation phase; in particular, "declare simd" clones
   are not known until late resolution.

   The augmented context is returned, and *COMPLETEP is set to true if
   the context is known to be complete, false otherwise.  */
static tree
omp_complete_construct_context (tree construct_context, bool *completep)
{
  /* The point in the program is enclosed by a target construct.  */
  if (construct_context
      && OMP_TS_CODE (construct_context) == OMP_TRAIT_CONSTRUCT_TARGET)
    *completep = true;

  /* At parse time we have none of the information we need to collect
     the missing pieces.  */
  else if (symtab->state == PARSING)
    *completep = false;

  else
    {
      tree attributes = DECL_ATTRIBUTES (current_function_decl);

      /* Add simd trait when in a simd clone.  This information is only
	 available during late resolution in the omp_device_lower pass,
	 however we can also rule out cases where we know earlier that
	 cfun is not a candidate for cloning.  */
      if (cfun && (cfun->curr_properties & PROP_gimple_any) != 0)
	{
	  cgraph_node *node = cgraph_node::get (cfun->decl);
	  if (node->simdclone)
	    construct_context = make_trait_selector (OMP_TRAIT_CONSTRUCT_SIMD,
						     NULL_TREE, NULL_TREE,
						     construct_context);
	  *completep = true;
	}
      else if (lookup_attribute ("omp declare simd", attributes))
	*completep = false;
      else
	*completep = true;

      /* Add construct selector set within a "declare variant" function.  */
      tree variant_attr
	= lookup_attribute ("omp declare variant variant", attributes);
      if (variant_attr)
	{
	  tree temp = NULL_TREE;
	  for (tree t = TREE_VALUE (variant_attr); t; t = TREE_CHAIN (t))
	    temp = chainon (temp, copy_node (t));
	  construct_context = chainon (temp, construct_context);
	}

      /* Add target trait when in a target variant.  */
      if (lookup_attribute ("omp declare target", attributes))
	construct_context = make_trait_selector (OMP_TRAIT_CONSTRUCT_TARGET,
						 NULL_TREE, NULL_TREE,
						 construct_context);
    }
  return construct_context;
}

/* Comparison function for sorting routines, to sort OpenMP metadirective
   variants by decreasing score.  */

static int
sort_variant (const void * a, const void *b, void *)
{
  score_wide_int score1
    = ((const struct omp_variant *) a)->score;
  score_wide_int score2
    = ((const struct omp_variant *) b)->score;

  if (score1 > score2)
    return -1;
  else if (score1 < score2)
    return 1;
  else
    return 0;
}

/* Return a vector of dynamic replacement candidates for the directive
   candidates in ALL_VARIANTS.  Return an empty vector if the candidates
   cannot be resolved.  */

vec<struct omp_variant>
omp_get_dynamic_candidates (vec <struct omp_variant> &all_variants,
			    tree construct_context)
{
  auto_vec <struct omp_variant> variants;
  struct omp_variant default_variant;
  bool default_found = false;
  bool complete_p;

  construct_context
    = omp_complete_construct_context (construct_context, &complete_p);

  if (dump_file)
    {
      fprintf (dump_file, "\nIn omp_get_dynamic_candidates:\n");
      if (symtab->state == PARSING)
	fprintf (dump_file, "invoked during parsing\n");
      else if (cfun && (cfun->curr_properties & PROP_gimple_any) == 0)
	fprintf (dump_file, "invoked during gimplification\n");
      else if (cfun && (cfun->curr_properties & PROP_gimple_any) != 0)
	fprintf (dump_file, "invoked during late resolution\n");
      else
	fprintf (dump_file, "confused about invocation context?!?\n");
      fprintf (dump_file, "construct_context has %d traits (%s)\n",
	       (construct_context ? list_length (construct_context) : 0),
	       (complete_p ? "complete" : "incomplete"));
    }

  for (unsigned int i = 0; i < all_variants.length (); i++)
    {
      struct omp_variant variant = all_variants[i];

      if (variant.selector == NULL_TREE)
	{
	  gcc_assert (!default_found);
	  default_found = true;
	  default_variant = variant;
	  default_variant.score = 0;
	  default_variant.scorable = true;
	  default_variant.matchable = true;
	  default_variant.dynamic_selector = false;
	  if (dump_file)
	    fprintf (dump_file,
		     "Considering default selector as candidate\n");
	  continue;
	}

      variant.matchable = true;
      variant.scorable = true;

      if (dump_file)
	{
	  fprintf (dump_file, "Considering selector ");
	  print_omp_context_selector (dump_file, variant.selector, TDF_NONE);
	  fprintf (dump_file, " as candidate - ");
	}

     switch (omp_context_selector_matches (variant.selector,
					   construct_context, complete_p))
	{
	case -1:
	  if (dump_file)
	    fprintf (dump_file, "unmatchable\n");
	  /* At parse time, just give up if we can't determine whether
	     things match.  */
	  if (symtab->state == PARSING)
	    {
	      variants.truncate (0);
	      return variants.copy ();
	    }
	  /* Otherwise we must be invoked from the gimplifier.  */
	  gcc_assert (cfun && (cfun->curr_properties & PROP_gimple_any) == 0);
	  variant.matchable = false;
	  /* FALLTHRU */
	case 1:
	  omp_context_compute_score (&variant, construct_context, complete_p);
	  variant.dynamic_selector
	    = omp_selector_is_dynamic (variant.selector);
	  variants.safe_push (variant);
	  if (dump_file && variant.matchable)
	    {
	      if (variant.dynamic_selector)
		fprintf (dump_file, "matched, dynamic");
	      else
		fprintf (dump_file, "matched, non-dynamic");
	    }
	  break;
	case 0:
	  if (dump_file)
	    fprintf (dump_file, "no match");
	  break;
	}

      if (dump_file)
	fprintf (dump_file, "\n");
    }

  /* There must be one default variant.  */
  gcc_assert (default_found);

  /* If there are no matching selectors, return the default.  */
  if (variants.length () == 0)
    {
      variants.safe_push (default_variant);
      return variants.copy ();
    }

  /* If there is only one matching selector, use it.  */
  if (variants.length () == 1)
    {
      if (variants[0].matchable)
	{
	  if (variants[0].dynamic_selector)
	    variants.safe_push (default_variant);
	  return variants.copy ();
	}
      else
	{
	  /* We don't know whether the one non-default selector will
	     actually match.  */
	  variants.truncate (0);
	  return variants.copy ();
	}
    }

  /* A context selector that is a strict subset of another context selector
     has a score of zero.  This only applies if the selector that is a
     superset definitely matches, though.  */
  for (unsigned int i = 0; i < variants.length (); i++)
    for (unsigned int j = i + 1; j < variants.length (); j++)
      {
	int r = omp_context_selector_compare (variants[i].selector,
					      variants[j].selector);
	if (r == -1 && variants[j].matchable)
	  {
	    /* variant i is a strict subset of variant j.  */
	    variants[i].score = 0;
	    variants[i].scorable = true;
	    break;
	  }
	else if (r == 1 && variants[i].matchable)
	  /* variant j is a strict subset of variant i.  */
	  {
	    variants[j].score = 0;
	    variants[j].scorable = true;
	  }
      }

  /* Sort the variants by decreasing score, preserving the original order
     in case of a tie.  */
  variants.stablesort (sort_variant, NULL);

  /* Add the default as a final choice.  */
  variants.safe_push (default_variant);

  if (dump_file)
    {
      fprintf (dump_file, "Sorted variants are:\n");
      for (unsigned i = 0; i < variants.length (); i++)
	{
	  HOST_WIDE_INT score = variants[i].score.to_shwi ();
	  fprintf (dump_file, "score %d matchable %d scorable %d ",
		   (int)score, (int)(variants[i].matchable),
		   (int)(variants[i].scorable));
	  if (variants[i].selector)
	    {
	      fprintf (dump_file, "selector ");
	      print_omp_context_selector (dump_file, variants[i].selector,
					  TDF_NONE);
	      fprintf (dump_file, "\n");
	    }
	  else
	    fprintf (dump_file, "default selector\n");
	}
    }

  /* Build the dynamic candidate list.  */
  for (unsigned i = 0; i < variants.length (); i++)
    {
      /* If we encounter a candidate that wasn't definitely matched,
	 give up now.  */
      if (!variants[i].matchable)
	{
	  variants.truncate (0);
	  break;
	}

      /* In general, we can't proceed if we can't accurately score any
	 of the selectors, since the sorting may be incorrect.  But, since
	 the actual score will never be lower than the guessed value, we
	 can use the first variant if it is not scorable but either the next
	 one is a subset of the first, is scorable, or we can make a
	 direct comparison of the high-order isa/arch/kind bits.  */
      if (!variants[i].scorable)
	{
	  bool ok = true;
	  if (i != 0)
	    ok = false;
	  else if (variants[i+1].scorable)
	    /* ok */
	    ;
	  else if (variants[i+1].score > 0)
	    {
	      /* To keep comparisons simple, reject selectors that contain
		 sets other than device, target_device, or construct.  */
	      for (tree tss = variants[i].selector;
		   tss && ok; tss = TREE_CHAIN (tss))
		{
		  enum omp_tss_code code = OMP_TSS_CODE (tss);
		  if (code != OMP_TRAIT_SET_DEVICE
		      && code != OMP_TRAIT_SET_TARGET_DEVICE
		      && code != OMP_TRAIT_SET_CONSTRUCT)
		    ok = false;
		}
	      for (tree tss = variants[i+1].selector;
		   tss && ok; tss = TREE_CHAIN (tss))
		{
		  enum omp_tss_code code = OMP_TSS_CODE (tss);
		  if (code != OMP_TRAIT_SET_DEVICE
		      && code != OMP_TRAIT_SET_TARGET_DEVICE
		      && code != OMP_TRAIT_SET_CONSTRUCT)
		    ok = false;
		}
	      /* Ignore the construct bits of the score.  If the isa/arch/kind
		 bits are strictly ordered, we're good to go.  Since
		 "the final score is the sum of the values of all specified
		 selectors plus 1", subtract that 1 from both scores before
		 getting rid of the low bits.  */
	      if (ok)
		{
		  size_t l = list_length (construct_context);
		  gcc_assert (variants[i].score > 0
			      && variants[i+1].score > 0);
		  if ((variants[i].score - 1) >> l
		      <= (variants[i+1].score - 1) >> l)
		    ok = false;
		}
	    }

	  if (!ok)
	    {
	      variants.truncate (0);
	      break;
	    }
	}

      if (dump_file)
	{
	  fprintf (dump_file, "Adding directive variant with ");

	  if (variants[i].selector)
	    {
	      fprintf (dump_file, "selector ");
	      print_omp_context_selector (dump_file, variants[i].selector,
					  TDF_NONE);
	    }
	  else
	    fprintf (dump_file, "default selector");

	  fprintf (dump_file, " as candidate.\n");
	}

      /* The last of the candidates is ended by a static selector.  */
      if (!variants[i].dynamic_selector)
	{
	  variants.truncate (i + 1);
	  break;
	}
    }

  return variants.copy ();
}

/* Two attempts are made to resolve calls to "declare variant" functions:
   early resolution in the gimplifier, and late resolution in the
   omp_device_lower pass.  If early resolution is not possible, the
   original function call is gimplified into the same form as metadirective
   and goes through the same late resolution code as metadirective.  */

/* Collect "declare variant" candidates for BASE.  CONSTRUCT_CONTEXT
   is the un-augmented context, or NULL_TREE if that information is not
   available yet.  */
vec<struct omp_variant>
omp_declare_variant_candidates (tree base, tree construct_context)
{
  auto_vec <struct omp_variant> candidates;
  bool complete_p;
  tree augmented_context
    = omp_complete_construct_context (construct_context, &complete_p);

  /* The variants are stored on (possible multiple) "omp declare variant base"
     attributes on the base function.  */
  for (tree attr = DECL_ATTRIBUTES (base); attr; attr = TREE_CHAIN (attr))
    {
      attr = lookup_attribute ("omp declare variant base", attr);
      if (attr == NULL_TREE)
	break;

      tree fndecl = TREE_PURPOSE (TREE_VALUE (attr));
      tree selector = TREE_VALUE (TREE_VALUE (attr));

      if (TREE_CODE (fndecl) != FUNCTION_DECL)
	continue;

      /* Ignore this variant if its selector is known not to match.  */
      if (!omp_context_selector_matches (selector, augmented_context,
					 complete_p))
	  continue;

      struct omp_variant candidate;
      candidate.selector = selector;
      candidate.dynamic_selector = false;
      candidate.alternative = fndecl;
      candidate.body = NULL_TREE;
      candidates.safe_push (candidate);
    }

  /* Add a default that is the base function.  */
  struct omp_variant v;
  v.selector = NULL_TREE;
  v.dynamic_selector = false;
  v.alternative = base;
  v.body = NULL_TREE;
  candidates.safe_push (v);
  return candidates.copy ();
}

/* Collect metadirective candidates for METADIRECTIVE.  CONSTRUCT_CONTEXT
   is the un-augmented context, or NULL_TREE if that information is not
   available yet.  */
vec<struct omp_variant>
omp_metadirective_candidates (tree metadirective, tree construct_context)
{
  auto_vec <struct omp_variant> candidates;
  tree variant = OMP_METADIRECTIVE_VARIANTS (metadirective);
  bool complete_p;
  tree augmented_context
    = omp_complete_construct_context (construct_context, &complete_p);

  gcc_assert (variant);
  for (; variant; variant = TREE_CHAIN (variant))
    {
      tree selector = OMP_METADIRECTIVE_VARIANT_SELECTOR (variant);

      /* Ignore this variant if its selector is known not to match.  */
      if (!omp_context_selector_matches (selector, augmented_context,
					 complete_p))
	continue;

      struct omp_variant candidate;
      candidate.selector = selector;
      candidate.dynamic_selector = false;
      candidate.alternative = OMP_METADIRECTIVE_VARIANT_DIRECTIVE (variant);
      candidate.body = OMP_METADIRECTIVE_VARIANT_BODY (variant);
      candidates.safe_push (candidate);
    }
  return candidates.copy ();
}

/* Return a vector of dynamic replacement candidates for the metadirective
   statement in METADIRECTIVE.  Return an empty vector if the metadirective
   cannot be resolved.  This function is intended to be called from the
   front ends, prior to gimplification.  */

vec<struct omp_variant>
omp_early_resolve_metadirective (tree metadirective)
{
  vec <struct omp_variant> candidates
    = omp_metadirective_candidates (metadirective, NULL_TREE);
  return omp_get_dynamic_candidates (candidates, NULL_TREE);
}

/* Return a vector of dynamic replacement candidates for the variant construct
   with SELECTORS and CONSTRUCT_CONTEXT.  This version is called during late
   resolution in the ompdevlow pass.  */

vec<struct omp_variant>
omp_resolve_variant_construct (tree construct_context, tree selectors)
{
  auto_vec <struct omp_variant> variants;

  for (int i = 0; i < TREE_VEC_LENGTH (selectors); i++)
    {
      struct omp_variant variant;

      variant.selector = TREE_VEC_ELT (selectors, i);
      variant.dynamic_selector = false;
      variant.alternative = build_int_cst (integer_type_node, i + 1);
      variant.body = NULL_TREE;

      variants.safe_push (variant);
    }

  return omp_get_dynamic_candidates (variants, construct_context);
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

/* Openacc compute grid dimension clauses are converted to an attribute
   attached to the function.  This permits the target-side code to (a) massage
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

/* Replace any existing oacc fn attribute in ATTRIBS with updated
   dimensions.  */

tree
oacc_replace_fn_attrib_attr (tree attribs, tree dims)
{
  tree ident = get_identifier (OACC_FN_ATTRIB);

  /* If we happen to be present as the first attrib, drop it.  */
  if (attribs && TREE_PURPOSE (attribs) == ident)
    attribs = TREE_CHAIN (attribs);
  return tree_cons (ident, dims, attribs);
}

/* Replace any existing oacc fn attribute on FN with updated
   dimensions.  */

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
  tree c_nohost = NULL_TREE;
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
      case OMP_CLAUSE_NOHOST:
	/* Don't worry about duplicate clauses here.  */
	c_nohost = c;
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
		    "cannot apply %qs to %qD, which has also been"
		    " marked with an OpenMP 'declare target' directive",
		    routine_str, fndecl);
	  /* Incompatible.  */
	  return -1;
	}

      /* If a "#pragma acc routine" has already been applied, just verify
	 this one for compatibility.  */
      /* Collect previous directive's clauses.  */
      tree c_level_p = NULL_TREE;
      tree c_nohost_p = NULL_TREE;
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
	  case OMP_CLAUSE_NOHOST:
	    gcc_checking_assert (c_nohost_p == NULL_TREE);
	    c_nohost_p = c;
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
      /* Matching 'nohost' clauses?  */
      if ((c_nohost == NULL_TREE) != (c_nohost_p == NULL_TREE))
	{
	  c_diag = c_nohost;
	  c_diag_p = c_nohost_p;
	  goto incompatible;
	}
      /* Compatible.  */
      return 1;

    incompatible:
      if (c_diag != NULL_TREE)
	error_at (OMP_CLAUSE_LOCATION (c_diag),
		  "incompatible %qs clause when applying"
		  " %qs to %qD, which has already been"
		  " marked with an OpenACC 'routine' directive",
		  omp_clause_code_name[OMP_CLAUSE_CODE (c_diag)],
		  routine_str, fndecl);
      else if (c_diag_p != NULL_TREE)
	error_at (loc,
		  "missing %qs clause when applying"
		  " %qs to %qD, which has already been"
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

/* Build COMPONENT_REF and set TREE_THIS_VOLATILE and TREE_READONLY on it
   as appropriate.  */

tree
omp_build_component_ref (tree obj, tree field)
{
  tree ret = build3 (COMPONENT_REF, TREE_TYPE (field), obj, field, NULL);
  if (TREE_THIS_VOLATILE (field))
    TREE_THIS_VOLATILE (ret) |= 1;
  if (TREE_READONLY (field))
    TREE_READONLY (ret) |= 1;
  return ret;
}

/* Return true if NAME is the name of an omp_* runtime API call.  */
bool
omp_runtime_api_procname (const char *name)
{
  if (!startswith (name, "omp_"))
    return false;

  static const char *omp_runtime_apis[] =
    {
      /* This array has 3 sections.  First omp_* calls that don't
	 have any suffixes.  */
      "aligned_alloc",
      "aligned_calloc",
      "alloc",
      "calloc",
      "free",
      "get_interop_int",
      "get_interop_ptr",
      "get_mapped_ptr",
      "get_num_interop_properties",
      "realloc",
      "target_alloc",
      "target_associate_ptr",
      "target_disassociate_ptr",
      "target_free",
      "target_is_accessible",
      "target_is_present",
      "target_memcpy",
      "target_memcpy_async",
      "target_memcpy_rect",
      "target_memcpy_rect_async",
      NULL,
      /* Now omp_* calls that are available as omp_* and omp_*_; however, the
	 DECL_NAME is always omp_* without tailing underscore.  */
      "capture_affinity",
      "destroy_allocator",
      "destroy_lock",
      "destroy_nest_lock",
      "display_affinity",
      "fulfill_event",
      "get_active_level",
      "get_affinity_format",
      "get_cancellation",
      "get_default_allocator",
      "get_default_device",
      "get_device_from_uid",
      "get_device_num",
      "get_dynamic",
      "get_initial_device",
      "get_interop_name",
      "get_interop_rc_desc",
      "get_interop_str",
      "get_interop_type_desc",
      "get_level",
      "get_max_active_levels",
      "get_max_task_priority",
      "get_max_teams",
      "get_max_threads",
      "get_nested",
      "get_num_devices",
      "get_num_places",
      "get_num_procs",
      "get_num_teams",
      "get_num_threads",
      "get_partition_num_places",
      "get_place_num",
      "get_proc_bind",
      "get_supported_active_levels",
      "get_team_num",
      "get_teams_thread_limit",
      "get_thread_limit",
      "get_thread_num",
      "get_wtick",
      "get_wtime",
      "in_explicit_task",
      "in_final",
      "in_parallel",
      "init_lock",
      "init_nest_lock",
      "is_initial_device",
      "pause_resource",
      "pause_resource_all",
      "set_affinity_format",
      "set_default_allocator",
      "set_lock",
      "set_nest_lock",
      "test_lock",
      "test_nest_lock",
      "unset_lock",
      "unset_nest_lock",
      NULL,
      /* And finally calls available as omp_*, omp_*_ and omp_*_8_; however,
	 as DECL_NAME only omp_* and omp_*_8 appear.  */
      "display_env",
      "get_ancestor_thread_num",
      "get_uid_from_device",
      "get_partition_place_nums",
      "get_place_num_procs",
      "get_place_proc_ids",
      "get_schedule",
      "get_team_size",
      "init_allocator",
      "set_default_device",
      "set_dynamic",
      "set_max_active_levels",
      "set_nested",
      "set_num_teams",
      "set_num_threads",
      "set_schedule",
      "set_teams_thread_limit"
    };

  int mode = 0;
  for (unsigned i = 0; i < ARRAY_SIZE (omp_runtime_apis); i++)
    {
      if (omp_runtime_apis[i] == NULL)
	{
	  mode++;
	  continue;
	}
      size_t len = strlen (omp_runtime_apis[i]);
      if (strncmp (name + 4, omp_runtime_apis[i], len) == 0
	  && (name[4 + len] == '\0'
	      || (mode > 1 && strcmp (name + 4 + len, "_8") == 0)))
	return true;
    }
  return false;
}

/* Return true if FNDECL is an omp_* runtime API call.  */

bool
omp_runtime_api_call (const_tree fndecl)
{
  tree declname = DECL_NAME (fndecl);
  if (!declname
      || (DECL_CONTEXT (fndecl) != NULL_TREE
	  && TREE_CODE (DECL_CONTEXT (fndecl)) != TRANSLATION_UNIT_DECL)
      || !TREE_PUBLIC (fndecl))
    return false;
  return omp_runtime_api_procname (IDENTIFIER_POINTER (declname));
}

/* See "Additional Definitions for the OpenMP API Specification" document;
   associated IDs are 1, 2, ...  */
static const char* omp_interop_fr_str[] = {"cuda", "cuda_driver", "opencl",
					   "sycl", "hip", "level_zero", "hsa"};

/* Returns the foreign-runtime ID if found or 0 otherwise.  */

char
omp_get_fr_id_from_name (const char *str)
{
  static_assert (GOMP_INTEROP_IFR_LAST == ARRAY_SIZE (omp_interop_fr_str), "");

  for (unsigned i = 0; i < ARRAY_SIZE (omp_interop_fr_str); ++i)
    if (!strcmp (str, omp_interop_fr_str[i]))
      return i + 1;
  return GOMP_INTEROP_IFR_UNKNOWN;
}

/* Returns the string value to a foreign-runtime integer value or NULL if value
   is not known.  */

const char *
omp_get_name_from_fr_id (int fr_id)
{
  if (fr_id < 1 || fr_id > (int) ARRAY_SIZE (omp_interop_fr_str))
    return "<unknown>";
  return omp_interop_fr_str[fr_id-1];
}

namespace omp_addr_tokenizer {

/* We scan an expression by recursive descent, and build a vector of
   "omp_addr_token *" pointers representing a "parsed" version of the
   expression.  The grammar we use is something like this:

     expr0::
       expr [section-access]

     expr::
	 structured-expr access-method
       | array-base access-method

     structured-expr::
       structure-base component-selector

     arbitrary-expr::
       (anything else)

     structure-base::
	 DECL access-method
       | structured-expr access-method
       | arbitrary-expr access-method

     array-base::
	 DECL
       | arbitrary-expr

     access-method::
	 DIRECT
       | REF
       | POINTER
       | REF_TO_POINTER
       | POINTER_OFFSET
       | REF_TO_POINTER_OFFSET
       | INDEXED_ARRAY
       | INDEXED_REF_TO_ARRAY
       | index-expr

     index-expr::
	 INDEX_EXPR access-method

     component-selector::
	 component-selector COMPONENT_REF
       | component-selector ARRAY_REF
       | COMPONENT_REF

   This tokenized form is then used both in parsing, for OpenMP clause
   expansion (for C and C++) and in gimplify.cc for sibling-list handling
   (for C, C++ and Fortran).  */

omp_addr_token::omp_addr_token (token_type t, tree e)
  : type(t), expr(e)
{
}

omp_addr_token::omp_addr_token (access_method_kinds k, tree e)
  : type(ACCESS_METHOD), expr(e)
{
  u.access_kind = k;
}

omp_addr_token::omp_addr_token (token_type t, structure_base_kinds k, tree e)
  : type(t), expr(e)
{
  u.structure_base_kind = k;
}

static bool
omp_parse_component_selector (tree *expr0)
{
  tree expr = *expr0;
  tree last_component = NULL_TREE;

  while (TREE_CODE (expr) == COMPONENT_REF
	 || TREE_CODE (expr) == ARRAY_REF)
    {
      if (TREE_CODE (expr) == COMPONENT_REF)
	last_component = expr;

      expr = TREE_OPERAND (expr, 0);

      if (TREE_CODE (TREE_TYPE (expr)) == REFERENCE_TYPE)
	break;
    }

  if (!last_component)
    return false;

  *expr0 = last_component;
  return true;
}

/* This handles references that have had convert_from_reference called on
   them, and also those that haven't.  */

static bool
omp_parse_ref (tree *expr0)
{
  tree expr = *expr0;

  if (TREE_CODE (TREE_TYPE (expr)) == REFERENCE_TYPE)
    return true;
  else if ((TREE_CODE (expr) == INDIRECT_REF
	    || (TREE_CODE (expr) == MEM_REF
		&& integer_zerop (TREE_OPERAND (expr, 1))))
	   && TREE_CODE (TREE_TYPE (TREE_OPERAND (expr, 0))) == REFERENCE_TYPE)
    {
      *expr0 = TREE_OPERAND (expr, 0);
      return true;
    }

  return false;
}

static bool
omp_parse_pointer (tree *expr0, bool *has_offset)
{
  tree expr = *expr0;

  *has_offset = false;

  if ((TREE_CODE (expr) == INDIRECT_REF
       || (TREE_CODE (expr) == MEM_REF
	   && integer_zerop (TREE_OPERAND (expr, 1))))
      && TREE_CODE (TREE_TYPE (TREE_OPERAND (expr, 0))) == POINTER_TYPE)
    {
      expr = TREE_OPERAND (expr, 0);

      /* The Fortran FE sometimes emits a no-op cast here.  */
      STRIP_NOPS (expr);

      while (1)
	{
	  if (TREE_CODE (expr) == COMPOUND_EXPR)
	    {
	      expr = TREE_OPERAND (expr, 1);
	      STRIP_NOPS (expr);
	    }
	  else if (TREE_CODE (expr) == SAVE_EXPR)
	    expr = TREE_OPERAND (expr, 0);
	  else if (TREE_CODE (expr) == POINTER_PLUS_EXPR)
	    {
	      *has_offset = true;
	      expr = TREE_OPERAND (expr, 0);
	    }
	  else
	    break;
	}

      STRIP_NOPS (expr);

      *expr0 = expr;
      return true;
    }

  return false;
}

static bool
omp_parse_access_method (tree *expr0, enum access_method_kinds *kind)
{
  tree expr = *expr0;
  bool has_offset;

  if (omp_parse_ref (&expr))
    *kind = ACCESS_REF;
  else if (omp_parse_pointer (&expr, &has_offset))
    {
      if (omp_parse_ref (&expr))
	*kind = has_offset ? ACCESS_REF_TO_POINTER_OFFSET
			   : ACCESS_REF_TO_POINTER;
      else
	*kind = has_offset ? ACCESS_POINTER_OFFSET : ACCESS_POINTER;
    }
  else if (TREE_CODE (expr) == ARRAY_REF)
    {
      while (TREE_CODE (expr) == ARRAY_REF)
	expr = TREE_OPERAND (expr, 0);
      if (omp_parse_ref (&expr))
	*kind = ACCESS_INDEXED_REF_TO_ARRAY;
      else
	*kind = ACCESS_INDEXED_ARRAY;
    }
  else
    *kind = ACCESS_DIRECT;

  STRIP_NOPS (expr);

  *expr0 = expr;
  return true;
}

static bool
omp_parse_access_methods (vec<omp_addr_token *> &addr_tokens, tree *expr0)
{
  tree expr = *expr0;
  enum access_method_kinds kind;
  tree am_expr;

  if (omp_parse_access_method (&expr, &kind))
    am_expr = expr;

  if (TREE_CODE (expr) == INDIRECT_REF
      || TREE_CODE (expr) == MEM_REF
      || TREE_CODE (expr) == ARRAY_REF)
    omp_parse_access_methods (addr_tokens, &expr);

  addr_tokens.safe_push (new omp_addr_token (kind, am_expr));

  *expr0 = expr;
  return true;
}

static bool omp_parse_structured_expr (vec<omp_addr_token *> &, tree *);

static bool
omp_parse_structure_base (vec<omp_addr_token *> &addr_tokens,
			  tree *expr0, structure_base_kinds *kind,
			  vec<omp_addr_token *> &base_access_tokens,
			  bool allow_structured = true)
{
  tree expr = *expr0;

  if (allow_structured)
    omp_parse_access_methods (base_access_tokens, &expr);

  if (DECL_P (expr))
    {
      *kind = BASE_DECL;
      return true;
    }

  if (allow_structured && omp_parse_structured_expr (addr_tokens, &expr))
    {
      *kind = BASE_COMPONENT_EXPR;
      *expr0 = expr;
      return true;
    }

  *kind = BASE_ARBITRARY_EXPR;
  *expr0 = expr;
  return true;
}

static bool
omp_parse_structured_expr (vec<omp_addr_token *> &addr_tokens, tree *expr0)
{
  tree expr = *expr0;
  tree base_component = NULL_TREE;
  structure_base_kinds struct_base_kind;
  auto_vec<omp_addr_token *> base_access_tokens;

  if (omp_parse_component_selector (&expr))
    base_component = expr;
  else
    return false;

  gcc_assert (TREE_CODE (expr) == COMPONENT_REF);
  expr = TREE_OPERAND (expr, 0);

  tree structure_base = expr;

  if (!omp_parse_structure_base (addr_tokens, &expr, &struct_base_kind,
				 base_access_tokens))
    return false;

  addr_tokens.safe_push (new omp_addr_token (STRUCTURE_BASE, struct_base_kind,
					     structure_base));
  addr_tokens.safe_splice (base_access_tokens);
  addr_tokens.safe_push (new omp_addr_token (COMPONENT_SELECTOR,
					     base_component));

  *expr0 = expr;

  return true;
}

static bool
omp_parse_array_expr (vec<omp_addr_token *> &addr_tokens, tree *expr0)
{
  tree expr = *expr0;
  structure_base_kinds s_kind;
  auto_vec<omp_addr_token *> base_access_tokens;

  if (!omp_parse_structure_base (addr_tokens, &expr, &s_kind,
				 base_access_tokens, false))
    return false;

  addr_tokens.safe_push (new omp_addr_token (ARRAY_BASE, s_kind, expr));
  addr_tokens.safe_splice (base_access_tokens);

  *expr0 = expr;
  return true;
}

/* Return TRUE if the ACCESS_METHOD token at index 'i' has a further
   ACCESS_METHOD chained after it (e.g., if we're processing an expression
   containing multiple pointer indirections).  */

bool
omp_access_chain_p (vec<omp_addr_token *> &addr_tokens, unsigned i)
{
  gcc_assert (addr_tokens[i]->type == ACCESS_METHOD);
  return (i + 1 < addr_tokens.length ()
	  && addr_tokens[i + 1]->type == ACCESS_METHOD);
}

/* Return the address of the object accessed by the ACCESS_METHOD token
   at 'i': either of the next access method's expr, or of EXPR if we're at
   the end of the list of tokens.  */

tree
omp_accessed_addr (vec<omp_addr_token *> &addr_tokens, unsigned i, tree expr)
{
  if (i + 1 < addr_tokens.length ())
    return build_fold_addr_expr (addr_tokens[i + 1]->expr);
  else
    return build_fold_addr_expr (expr);
}

} /* namespace omp_addr_tokenizer.  */

bool
omp_parse_expr (vec<omp_addr_token *> &addr_tokens, tree expr)
{
  using namespace omp_addr_tokenizer;
  auto_vec<omp_addr_token *> expr_access_tokens;

  if (!omp_parse_access_methods (expr_access_tokens, &expr))
    return false;

  if (omp_parse_structured_expr (addr_tokens, &expr))
    ;
  else if (omp_parse_array_expr (addr_tokens, &expr))
    ;
  else
    return false;

  addr_tokens.safe_splice (expr_access_tokens);

  return true;
}

DEBUG_FUNCTION void
debug_omp_tokenized_addr (vec<omp_addr_token *> &addr_tokens,
			  bool with_exprs)
{
  using namespace omp_addr_tokenizer;
  const char *sep = with_exprs ? "  " : "";

  for (auto e : addr_tokens)
    {
      const char *pfx = "";

      fputs (sep, stderr);

      switch (e->type)
	{
	case COMPONENT_SELECTOR:
	  fputs ("component_selector", stderr);
	  break;
	case ACCESS_METHOD:
	  switch (e->u.access_kind)
	    {
	    case ACCESS_DIRECT:
	      fputs ("access_direct", stderr);
	      break;
	    case ACCESS_REF:
	      fputs ("access_ref", stderr);
	      break;
	    case ACCESS_POINTER:
	      fputs ("access_pointer", stderr);
	      break;
	    case ACCESS_POINTER_OFFSET:
	      fputs ("access_pointer_offset", stderr);
	      break;
	    case ACCESS_REF_TO_POINTER:
	      fputs ("access_ref_to_pointer", stderr);
	      break;
	    case ACCESS_REF_TO_POINTER_OFFSET:
	      fputs ("access_ref_to_pointer_offset", stderr);
	      break;
	    case ACCESS_INDEXED_ARRAY:
	      fputs ("access_indexed_array", stderr);
	      break;
	    case ACCESS_INDEXED_REF_TO_ARRAY:
	      fputs ("access_indexed_ref_to_array", stderr);
	      break;
	    }
	  break;
	case ARRAY_BASE:
	case STRUCTURE_BASE:
	  pfx = e->type == ARRAY_BASE ? "array_" : "struct_";
	  switch (e->u.structure_base_kind)
	    {
	    case BASE_DECL:
	      fprintf (stderr, "%sbase_decl", pfx);
	      break;
	    case BASE_COMPONENT_EXPR:
	      fputs ("base_component_expr", stderr);
	      break;
	    case BASE_ARBITRARY_EXPR:
	      fprintf (stderr, "%sbase_arbitrary_expr", pfx);
	      break;
	    }
	  break;
	}
      if (with_exprs)
	{
	  fputs (" [", stderr);
	  print_generic_expr (stderr, e->expr);
	  fputc (']', stderr);
	  sep = ",\n  ";
	}
      else
	sep = " ";
    }

  fputs ("\n", stderr);
}

/* Return number of iterations of loop I in FOR_STMT.  If PSTEP is non-NULL,
   *PSTEP will be the loop step.  */

tree
omp_loop_number_of_iterations (tree for_stmt, int i, tree *pstep)
{
  tree t = TREE_VEC_ELT (OMP_FOR_INIT (for_stmt), i);
  gcc_assert (TREE_CODE (t) == MODIFY_EXPR);
  tree decl = TREE_OPERAND (t, 0);
  tree n1 = TREE_OPERAND (t, 1);
  tree type = TREE_TYPE (decl);
  tree cond = TREE_VEC_ELT (OMP_FOR_COND (for_stmt), i);
  gcc_assert (COMPARISON_CLASS_P (cond));
  gcc_assert (TREE_OPERAND (cond, 0) == decl);
  tree_code cond_code = TREE_CODE (cond);
  tree n2 = TREE_OPERAND (cond, 1);
  t = TREE_VEC_ELT (OMP_FOR_INCR (for_stmt), i);
  tree step = NULL_TREE;
  switch (TREE_CODE (t))
    {
    case PREINCREMENT_EXPR:
    case POSTINCREMENT_EXPR:
      gcc_assert (!POINTER_TYPE_P (type));
      gcc_assert (TREE_OPERAND (t, 0) == decl);
      step = build_int_cst (type, 1);
      break;
    case PREDECREMENT_EXPR:
    case POSTDECREMENT_EXPR:
      gcc_assert (!POINTER_TYPE_P (type));
      gcc_assert (TREE_OPERAND (t, 0) == decl);
      step = build_int_cst (type, -1);
      break;
    case MODIFY_EXPR:
      gcc_assert (TREE_OPERAND (t, 0) == decl);
      t = TREE_OPERAND (t, 1);
      switch (TREE_CODE (t))
	{
	case PLUS_EXPR:
	  if (TREE_OPERAND (t, 1) == decl)
	    {
	      TREE_OPERAND (t, 1) = TREE_OPERAND (t, 0);
	      TREE_OPERAND (t, 0) = decl;
	    }
	  /* FALLTHRU */
	case POINTER_PLUS_EXPR:
	case MINUS_EXPR:
	  step = omp_get_for_step_from_incr (EXPR_LOCATION (t), t);
	  break;
	default:
	  gcc_unreachable ();
	}
      break;
    default:
      gcc_unreachable ();
    }
  omp_adjust_for_condition (EXPR_LOCATION (for_stmt), &cond_code, &n2,
			    decl, step);
  if (pstep)
    *pstep = step;
  if (INTEGRAL_TYPE_P (type)
      && TYPE_PRECISION (type) < TYPE_PRECISION (long_long_integer_type_node))
    {
      n1 = fold_convert (long_long_integer_type_node, n1);
      n2 = fold_convert (long_long_integer_type_node, n2);
      step = fold_convert (long_long_integer_type_node, step);
    }
  if (cond_code == LT_EXPR
      || POINTER_TYPE_P (type)
      || !TYPE_UNSIGNED (TREE_TYPE (n1)))
    {
      if (POINTER_TYPE_P (type))
	t = fold_build2 (POINTER_DIFF_EXPR, ssizetype, n2, n1);
      else
	t = fold_build2 (MINUS_EXPR, TREE_TYPE (n1), n2, n1);
      t = fold_build2 (CEIL_DIV_EXPR, TREE_TYPE (t), t, step);
    }
  else
    {
      t = fold_build2 (MINUS_EXPR, type, n1, n2);
      t = fold_build2 (CEIL_DIV_EXPR, type, t,
		       fold_build1 (NEGATE_EXPR, type, step));
    }
  return t;
}

/* Tile transformation:
   Original loop:

  #pragma omp tile sizes(16, 32)
  for (i = 0; i < k; ++i)
    for (j = 0; j < 128; j += 2)
      {
	baz (i, j);
      }

   Transformed loop:
  #pragma omp tile sizes(16, 32)
  for (i.0 = 0; i.0 < k; i.0 += 16)
    for (j.0 = 0; j.0 < 128; j.0 += 64)
      {
	i = i.0;
	i.1 = MIN_EXPR <i.0 + 16, k>;
	goto <D.2783>;
	<D.2782>:;
	j = j.0;
	j.1 = j.0 + 32;
	goto <D.2786>;
	<D.2785>:;
	{
	  baz (i, j);
	}
	j += 2;
	<D.2786>:;
	if (j < j.1) goto <D.2785>; else goto <D.2787>;
	<D.2787>:;
	++i;
	<D.2783>:;
	if (i < i.1) goto <D.2782>; else goto <D.2784>;
	<D.2784>:;
      }

   where the grid loops have canonical form, but the inner
   loops don't and so are immediately lowered.  */

static void
omp_apply_tile (tree for_stmt, tree sizes, int size)
{
  tree pre_body = NULL_TREE, post_body = NULL_TREE;
  tree orig_sizes = sizes;
  if (OMP_FOR_NON_RECTANGULAR (for_stmt))
    {
      error_at (EXPR_LOCATION (for_stmt), "non-rectangular %<tile%>");
      return;
    }
  for (int i = 0; i < TREE_VEC_LENGTH (OMP_FOR_INIT (for_stmt)); i++)
    {
      if (orig_sizes)
	{
	  size = tree_to_uhwi (TREE_VALUE (sizes));
	  sizes = TREE_CHAIN (sizes);
	}
      if (size == 1)
	continue;
      if (OMP_FOR_ORIG_DECLS (for_stmt) == NULL_TREE)
	{
	  OMP_FOR_ORIG_DECLS (for_stmt)
	    = make_tree_vec (TREE_VEC_LENGTH (OMP_FOR_INIT (for_stmt)));
	  for (int j = 0; j < TREE_VEC_LENGTH (OMP_FOR_INIT (for_stmt)); j++)
	    {
	      gcc_assert (TREE_CODE (TREE_VEC_ELT (OMP_FOR_INIT (for_stmt), j))
			  == MODIFY_EXPR);
	      TREE_VEC_ELT (OMP_FOR_ORIG_DECLS (for_stmt), j)
		= TREE_OPERAND (TREE_VEC_ELT (OMP_FOR_INIT (for_stmt), j), 0);
	    }
	}
      tree step;
      tree iters = omp_loop_number_of_iterations (for_stmt, i, &step);
      tree t = TREE_VEC_ELT (OMP_FOR_INIT (for_stmt), i);
      tree decl = TREE_OPERAND (t, 0);
      tree type = TREE_TYPE (decl);
      tree griddecl = create_tmp_var_raw (type);
      DECL_CONTEXT (griddecl) = current_function_decl;
      t = build1 (DECL_EXPR, void_type_node, griddecl);
      append_to_statement_list (t, &OMP_FOR_PRE_BODY (for_stmt));
      TREE_OPERAND (TREE_VEC_ELT (OMP_FOR_INIT (for_stmt), i), 0) = griddecl;
      TREE_PRIVATE (TREE_VEC_ELT (OMP_FOR_INIT (for_stmt), i)) = 1;
      tree cond = TREE_VEC_ELT (OMP_FOR_COND (for_stmt), i);
      TREE_OPERAND (cond, 0) = griddecl;
      tree ub = save_expr (TREE_OPERAND (cond, 1));
      TREE_OPERAND (cond, 1) = ub;
      t = TREE_VEC_ELT (OMP_FOR_INCR (for_stmt), i);
      if (TREE_CODE (cond) == NE_EXPR)
	{
	  tree_code cond_code = TREE_CODE (cond);
	  omp_adjust_for_condition (EXPR_LOCATION (for_stmt), &cond_code,
				    &ub, griddecl, step);
	  TREE_SET_CODE (cond, cond_code);
	}
      step = save_expr (step);
      tree gridstep = fold_build2 (MULT_EXPR, TREE_TYPE (step),
				   step, build_int_cst (TREE_TYPE (step),
							size));
      if (POINTER_TYPE_P (type))
	t = build2 (POINTER_PLUS_EXPR, type, griddecl,
		    fold_convert (sizetype, gridstep));
      else
	t = build2 (PLUS_EXPR, type, griddecl, gridstep);
      t = build2 (MODIFY_EXPR, type, griddecl, t);
      TREE_VEC_ELT (OMP_FOR_INCR (for_stmt), i) = t;
      t = build2 (MODIFY_EXPR, type, decl, griddecl);
      append_to_statement_list (t, &pre_body);
      if (POINTER_TYPE_P (type))
	t = build2 (POINTER_PLUS_EXPR, type, griddecl,
		    fold_convert (sizetype, gridstep));
      else
	t = build2 (PLUS_EXPR, type, griddecl, gridstep);
      bool minmax_needed = true;
      if (TREE_CODE (iters) == INTEGER_CST)
	{
	  wide_int witers = wi::to_wide (iters);
	  wide_int wsize = wide_int::from (size, witers.get_precision (),
					   TYPE_SIGN (TREE_TYPE (iters)));
	  if (wi::multiple_of_p (witers, wsize, TYPE_SIGN (TREE_TYPE (iters))))
	    minmax_needed = false;
	}
      if (minmax_needed)
	switch (TREE_CODE (cond))
	  {
	  case LE_EXPR:
	    if (POINTER_TYPE_P (type))
	      t = build2 (MIN_EXPR, type, t,
			  build2 (POINTER_PLUS_EXPR, type, ub, size_int (1)));
	    else
	      t = build2 (MIN_EXPR, type, t,
			  build2 (PLUS_EXPR, type, ub, build_one_cst (type)));
	    break;
	  case LT_EXPR:
	    t = build2 (MIN_EXPR, type, t, ub);
	    break;
	  case GE_EXPR:
	    if (POINTER_TYPE_P (type))
	      t = build2 (MAX_EXPR, type, t,
			  build2 (POINTER_PLUS_EXPR, type, ub, size_int (-1)));
	    else
	      t = build2 (MAX_EXPR, type, t,
			  build2 (PLUS_EXPR, type, ub,
				  build_minus_one_cst (type)));
	    break;
	  case GT_EXPR:
	    t = build2 (MAX_EXPR, type, t, ub);
	    break;
	  default:
	    gcc_unreachable ();
	  }
      tree end = create_tmp_var_raw (type);
      DECL_CONTEXT (end) = current_function_decl;
      end = build4 (TARGET_EXPR, type, end, t, NULL_TREE, NULL_TREE);
      TREE_SIDE_EFFECTS (end) = 1;
      append_to_statement_list (end, &pre_body);
      tree lab1 = create_artificial_label (UNKNOWN_LOCATION);
      tree lab2 = create_artificial_label (UNKNOWN_LOCATION);
      t = build1 (GOTO_EXPR, void_type_node, lab2);
      append_to_statement_list (t, &pre_body);
      t = build1 (LABEL_EXPR, void_type_node, lab1);
      append_to_statement_list (t, &pre_body);
      tree this_post_body = NULL_TREE;
      if (POINTER_TYPE_P (type))
	t = build2 (POINTER_PLUS_EXPR, type, decl,
		    fold_convert (sizetype, step));
      else
	t = build2 (PLUS_EXPR, type, decl, step);
      t = build2 (MODIFY_EXPR, type, decl, t);
      append_to_statement_list (t, &this_post_body);
      t = build1 (LABEL_EXPR, void_type_node, lab2);
      append_to_statement_list (t, &this_post_body);
      t = build2 ((TREE_CODE (cond) == LT_EXPR || TREE_CODE (cond) == LE_EXPR)
		  ? LT_EXPR : GT_EXPR, boolean_type_node, decl, end);
      if (orig_sizes == NULL_TREE)
	{
	  gcc_assert (i == 0);
	  t = build3 (ANNOTATE_EXPR, TREE_TYPE (t), t,
		      build_int_cst (integer_type_node,
				     annot_expr_unroll_kind),
		      build_int_cst (integer_type_node, size));
	}
      t = build3 (COND_EXPR, void_type_node, t,
		  build1 (GOTO_EXPR, void_type_node, lab1), NULL_TREE);
      append_to_statement_list (t, &this_post_body);
      append_to_statement_list (post_body, &this_post_body);
      post_body = this_post_body;
    }
  if (pre_body || post_body)
    {
      append_to_statement_list (OMP_FOR_BODY (for_stmt), &pre_body);
      append_to_statement_list (post_body, &pre_body);
      OMP_FOR_BODY (for_stmt) = pre_body;
    }
}

/* Callback for walk_tree to find nested loop transforming construct.  */

static tree
find_nested_loop_xform (tree *tp, int *walk_subtrees, void *data)
{
  tree **pdata = (tree **) data;
  *walk_subtrees = 0;
  switch (TREE_CODE (*tp))
    {
    case OMP_TILE:
    case OMP_UNROLL:
      pdata[1] = tp;
      return *tp;
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
    case CLEANUP_POINT_EXPR:
      pdata[0] = tp;
      *walk_subtrees = 1;
      break;
    default:
      break;
    }
  return NULL;
}

/* Main entry point for performing OpenMP loop transformations.  */

void
omp_maybe_apply_loop_xforms (tree *expr_p, tree for_clauses)
{
  tree for_stmt = *expr_p;

  switch (TREE_CODE (for_stmt))
    {
    case OMP_TILE:
    case OMP_UNROLL:
      if (OMP_LOOPXFORM_LOWERED (for_stmt))
	return;
      break;
    default:
      break;
    }

  tree *inner_expr_p = expr_p;
  tree inner_for_stmt = for_stmt;
  for (int i = 0; i < TREE_VEC_LENGTH (OMP_FOR_INIT (for_stmt)); i++)
    {
      /* If some loop nest needs one or more loops in canonical form
	 from nested loop transforming constructs, first perform the
	 loop transformation on the nested construct and then move over
	 the corresponding loops in canonical form from the inner construct
	 to the outer one.  */
      if (TREE_VEC_ELT (OMP_FOR_INIT (for_stmt), i) == NULL_TREE)
	{
	  if (inner_for_stmt == for_stmt
	      && omp_find_clause (for_clauses ? for_clauses
				  : OMP_FOR_CLAUSES (for_stmt),
				  OMP_CLAUSE_ORDERED))
	    {
	      error_at (EXPR_LOCATION (for_stmt),
			"%<ordered%> clause used with generated loops");
	      *expr_p = void_node;
	      return;
	    }
	  tree *data[2] = { NULL, NULL };
	  walk_tree (&OMP_FOR_BODY (inner_for_stmt),
		     find_nested_loop_xform, &data, NULL);
	  gcc_assert (data[1]);
	  if (data[0])
	    {
	      /* If there is a BIND_EXPR declaring some vars, or statement
		 list with more than one stmt etc., move the intervening
		 code around the outermost loop.  */
	      tree t = *inner_expr_p;
	      *inner_expr_p = OMP_FOR_BODY (inner_for_stmt);
	      OMP_FOR_BODY (inner_for_stmt) = *data[1];
	      *data[1] = t;
	      inner_expr_p = data[1];
	      data[1] = &OMP_FOR_BODY (inner_for_stmt);
	    }
	  inner_for_stmt = *data[1];

	  omp_maybe_apply_loop_xforms (data[1], NULL_TREE);
	  if (*data[1] != inner_for_stmt)
	    {
	      tree *data2[2] = { NULL, NULL };
	      walk_tree (data[1], find_nested_loop_xform, &data2, NULL);
	      gcc_assert (data2[1]
			  && *data2[1] == inner_for_stmt
			  && data2[0]);
	      tree t = *inner_expr_p;
	      *inner_expr_p = *data[1];
	      *data[1] = *data2[1];
	      *data2[1] = t;
	      inner_expr_p = data2[1];
	    }
	  tree clauses = OMP_FOR_CLAUSES (inner_for_stmt);
	  gcc_checking_assert (TREE_CODE (inner_for_stmt) != OMP_UNROLL
			       || omp_find_clause (clauses,
						   OMP_CLAUSE_PARTIAL));
	  append_to_statement_list (OMP_FOR_PRE_BODY (inner_for_stmt),
				    &OMP_FOR_PRE_BODY (for_stmt));
	  OMP_FOR_PRE_BODY (inner_for_stmt) = NULL_TREE;
	  if (OMP_FOR_ORIG_DECLS (for_stmt) == NULL_TREE
	      && OMP_FOR_ORIG_DECLS (inner_for_stmt) != NULL_TREE)
	    {
	      OMP_FOR_ORIG_DECLS (for_stmt)
		= make_tree_vec (TREE_VEC_LENGTH (OMP_FOR_INIT (for_stmt)));
	      for (int j = 0; j < TREE_VEC_LENGTH (OMP_FOR_INIT (for_stmt));
		   j++)
		{
		  if (TREE_VEC_ELT (OMP_FOR_INIT (for_stmt), j) == NULL_TREE)
		    continue;
		  gcc_assert (TREE_CODE (TREE_VEC_ELT (OMP_FOR_INIT (for_stmt),
						       j)) == MODIFY_EXPR);
		  TREE_VEC_ELT (OMP_FOR_ORIG_DECLS (for_stmt), j)
		    = TREE_OPERAND (TREE_VEC_ELT (OMP_FOR_INIT (for_stmt), j),
				    0);
		}
	    }
	  for (int j = 0; j < TREE_VEC_LENGTH (OMP_FOR_INIT (inner_for_stmt));
	       ++j)
	    {
	      if (i + j == TREE_VEC_LENGTH (OMP_FOR_INIT (for_stmt)))
		break;
	      if (OMP_FOR_ORIG_DECLS (for_stmt))
		{
		  if (OMP_FOR_ORIG_DECLS (inner_for_stmt))
		    {
		      TREE_VEC_ELT (OMP_FOR_ORIG_DECLS (for_stmt), i + j)
			= TREE_VEC_ELT (OMP_FOR_ORIG_DECLS (inner_for_stmt),
					j);
		      TREE_VEC_ELT (OMP_FOR_ORIG_DECLS (inner_for_stmt), j)
			= NULL_TREE;
		    }
		  else
		    {
		      tree t = TREE_VEC_ELT (OMP_FOR_INIT (inner_for_stmt), j);
		      gcc_assert (TREE_CODE (t) == MODIFY_EXPR);
		      TREE_VEC_ELT (OMP_FOR_ORIG_DECLS (for_stmt), i + j)
			= TREE_OPERAND (t, 0);
		    }
		}
	      TREE_VEC_ELT (OMP_FOR_INIT (for_stmt), i + j)
		= TREE_VEC_ELT (OMP_FOR_INIT (inner_for_stmt), j);
	      TREE_VEC_ELT (OMP_FOR_COND (for_stmt), i + j)
		= TREE_VEC_ELT (OMP_FOR_COND (inner_for_stmt), j);
	      TREE_VEC_ELT (OMP_FOR_INCR (for_stmt), i + j)
		= TREE_VEC_ELT (OMP_FOR_INCR (inner_for_stmt), j);
	      TREE_VEC_ELT (OMP_FOR_INIT (inner_for_stmt), j) = NULL_TREE;
	      TREE_VEC_ELT (OMP_FOR_COND (inner_for_stmt), j) = NULL_TREE;
	      TREE_VEC_ELT (OMP_FOR_INCR (inner_for_stmt), j) = NULL_TREE;
	    }
	}
    }

  switch (TREE_CODE (for_stmt))
    {
    case OMP_TILE:
      tree sizes;
      sizes = omp_find_clause (OMP_FOR_CLAUSES (for_stmt), OMP_CLAUSE_SIZES);
      omp_apply_tile (for_stmt, OMP_CLAUSE_SIZES_LIST (sizes), 0);
      OMP_LOOPXFORM_LOWERED (for_stmt) = 1;
      break;
    case OMP_UNROLL:
      tree partial;
      partial = omp_find_clause (OMP_FOR_CLAUSES (for_stmt),
				 OMP_CLAUSE_PARTIAL);
      if (partial)
	omp_apply_tile (for_stmt, NULL_TREE,
			OMP_CLAUSE_PARTIAL_EXPR (partial)
			? tree_to_shwi (OMP_CLAUSE_PARTIAL_EXPR (partial))
			: 8);
      else if (omp_find_clause (OMP_FOR_CLAUSES (for_stmt), OMP_CLAUSE_FULL))
	{
	  tree iters = omp_loop_number_of_iterations (for_stmt, 0, NULL);
	  if (TREE_CODE (iters) != INTEGER_CST)
	    error_at (EXPR_LOCATION (for_stmt),
		      "non-constant iteration count of %<unroll full%> loop");
	}
      OMP_LOOPXFORM_LOWERED (for_stmt) = 1;
      break;
    default:
      break;
    }
}

