/* General types and functions that are useful for processing of OpenMP,
   OpenACC and similar directives at various stages of compilation.

   Copyright (C) 2005-2024 Free Software Foundation, Inc.

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

/* Store the construct selectors as tree codes from last to first.
   CTX is a list of trait selectors, nconstructs must be equal to its
   length, and the array CONSTRUCTS holds the output.  */

void
omp_construct_traits_to_codes (tree ctx, int nconstructs,
			       enum tree_code *constructs)
{
  int i = nconstructs - 1;

  /* Order must match the OMP_TRAIT_CONSTRUCT_* enumerators in
     enum omp_ts_code.  */
  static enum tree_code code_map[]
    = { OMP_TARGET, OMP_TEAMS, OMP_PARALLEL, OMP_FOR, OMP_SIMD, OMP_DISPATCH };

  for (tree ts = ctx; ts; ts = TREE_CHAIN (ts), i--)
    {
      enum omp_ts_code sel = OMP_TS_CODE (ts);
      int j = (int)sel - (int)OMP_TRAIT_CONSTRUCT_TARGET;
      gcc_assert (j >= 0 && (unsigned int) j < ARRAY_SIZE (code_map));
      constructs[i] = code_map[j];
    }
  gcc_assert (i == -1);
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
    "llvm", "nvidia", "pgi", "ti", "unknown", NULL };
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
      {
	const char *ret = TREE_STRING_POINTER (val);
	if ((size_t) TREE_STRING_LENGTH (val)
	    == strlen (ret) + (lang_GNU_Fortran () ? 0 : 1))
	  return ret;
	return NULL;
      }
    default:
      return NULL;
    }
}

/* Diagnose errors in an OpenMP context selector, return CTX if
   it is correct or error_mark_node otherwise.  */

tree
omp_check_context_selector (location_t loc, tree ctx)
{
  bool tss_seen[OMP_TRAIT_SET_LAST], ts_seen[OMP_TRAIT_LAST];

  memset (tss_seen, 0, sizeof (tss_seen));
  for (tree tss = ctx; tss; tss = TREE_CHAIN (tss))
    {
      enum omp_tss_code tss_code = OMP_TSS_CODE (tss);
      bool saw_any_prop = false;
      bool saw_other_prop = false;

      /* We can parse this, but not handle it yet.  */
      if (tss_code == OMP_TRAIT_SET_TARGET_DEVICE)
	sorry_at (loc, "%<target_device%> selector set is not supported yet");

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

/* Return 1 if context selector matches the current OpenMP context, 0
   if it does not and -1 if it is unknown and need to be determined later.
   Some properties can be checked right away during parsing (this routine),
   others need to wait until the whole TU is parsed, others need to wait until
   IPA, others until vectorization.  */

int
omp_context_selector_matches (tree ctx)
{
  int ret = 1;
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

	  int nconstructs = list_length (selectors);
	  enum tree_code *constructs = NULL;
	  if (nconstructs)
	    {
	      /* Even though this alloca appears in a loop over selector
		 sets, it does not repeatedly grow the stack, because
		 there can be only one construct selector set specified.
		 This is enforced by omp_check_context_selector.  */
	      constructs
		= (enum tree_code *) alloca (nconstructs
					     * sizeof (enum tree_code));
	      omp_construct_traits_to_codes (selectors, nconstructs,
					     constructs);
	    }

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
      for (tree ts = selectors; ts; ts = TREE_CHAIN (ts))
	{
	  enum omp_ts_code sel = OMP_TS_CODE (ts);
	  switch (sel)
	    {
	    case OMP_TRAIT_IMPLEMENTATION_VENDOR:
	      if (set == OMP_TRAIT_SET_IMPLEMENTATION)
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
	      if (set == OMP_TRAIT_SET_IMPLEMENTATION)
		/* We don't support any extensions right now.  */
		return 0;
	      break;
	    case OMP_TRAIT_IMPLEMENTATION_ADMO:
	      if (set == OMP_TRAIT_SET_IMPLEMENTATION)
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
	      if (set == OMP_TRAIT_SET_DEVICE)
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
	    case OMP_TRAIT_IMPLEMENTATION_UNIFIED_ADDRESS:
	      if (set == OMP_TRAIT_SET_IMPLEMENTATION)
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
		}
	      break;
	    case OMP_TRAIT_IMPLEMENTATION_UNIFIED_SHARED_MEMORY:
	      if (set == OMP_TRAIT_SET_IMPLEMENTATION)
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
		}
	      break;
	    case OMP_TRAIT_IMPLEMENTATION_SELF_MAPS:
	      if (set == OMP_TRAIT_SET_IMPLEMENTATION)
		{
		  if (cfun && (cfun->curr_properties & PROP_gimple_any) != 0)
		    break;

		  if ((omp_requires_mask
		       & OMP_REQUIRES_SELF_MAPS) == 0)
		    {
		      if (symtab->state == PARSING)
			ret = -1;
		      else
			return 0;
		    }
		}
	      break;
	    case OMP_TRAIT_IMPLEMENTATION_DYNAMIC_ALLOCATORS:
	      if (set == OMP_TRAIT_SET_IMPLEMENTATION)
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
		}
	      break;
	    case OMP_TRAIT_IMPLEMENTATION_REVERSE_OFFLOAD:
	      if (set == OMP_TRAIT_SET_IMPLEMENTATION)
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
		}
	      break;
	    case OMP_TRAIT_DEVICE_KIND:
	      if (set == OMP_TRAIT_SET_DEVICE)
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
			if (omp_maybe_offloaded ())
			  ret = -1;
			continue;
#endif
		      }
		    if (!strcmp (prop, "nohost"))
		      {
#ifndef ACCEL_COMPILER
			if (omp_maybe_offloaded ())
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
	    case OMP_TRAIT_DEVICE_ISA:
	      if (set == OMP_TRAIT_SET_DEVICE)
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
	    case OMP_TRAIT_USER_CONDITION:
	      if (set == OMP_TRAIT_SET_USER)
		for (tree p = OMP_TS_PROPERTIES (ts); p; p = TREE_CHAIN (p))
		  if (OMP_TP_NAME (p) == NULL_TREE)
		    {
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
		      if (integer_zerop (OMP_TP_VALUE (p1))
			  != integer_zerop (OMP_TP_VALUE (p2)))
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

int
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
					      OMP_TS_PROPERTIES (ts2));
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
	    if (score1 && score2 && !simple_cst_equal (score1, score2))
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

/* Needs to be a GC-friendly widest_int variant, but precision is
   desirable to be the same on all targets.  */
typedef generic_wide_int <fixed_wide_int_storage <1024> > score_wide_int;

/* Compute *SCORE for context selector CTX.  Return true if the score
   would be different depending on whether it is a declare simd clone or
   not.  DECLARE_SIMD should be true for the case when it would be
   a declare simd clone.  */

static bool
omp_context_compute_score (tree ctx, score_wide_int *score, bool declare_simd)
{
  tree selectors
    = omp_get_context_selector_list (ctx, OMP_TRAIT_SET_CONSTRUCT);
  bool has_kind = omp_get_context_selector (ctx, OMP_TRAIT_SET_DEVICE,
					    OMP_TRAIT_DEVICE_KIND);
  bool has_arch = omp_get_context_selector (ctx, OMP_TRAIT_SET_DEVICE,
					    OMP_TRAIT_DEVICE_ARCH);
  bool has_isa = omp_get_context_selector (ctx, OMP_TRAIT_SET_DEVICE,
					   OMP_TRAIT_DEVICE_ISA);
  bool ret = false;
  *score = 1;
  for (tree tss = ctx; tss; tss = TREE_CHAIN (tss))
    if (OMP_TSS_TRAIT_SELECTORS (tss) != selectors)
      for (tree ts = OMP_TSS_TRAIT_SELECTORS (tss); ts; ts = TREE_CHAIN (ts))
	{
	  tree s = OMP_TS_SCORE (ts);
	  if (s && TREE_CODE (s) == INTEGER_CST)
	    *score += score_wide_int::from (wi::to_wide (s),
					    TYPE_SIGN (TREE_TYPE (s)));
	}

  if (selectors || has_kind || has_arch || has_isa)
    {
      int nconstructs = list_length (selectors);
      enum tree_code *constructs = NULL;
      if (nconstructs)
	{
	  constructs
	    = (enum tree_code *) alloca (nconstructs
					 * sizeof (enum tree_code));
	  omp_construct_traits_to_codes (selectors, nconstructs, constructs);
	}
      int *scores
	= (int *) alloca ((2 * nconstructs + 2) * sizeof (int));
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
	      *score += wi::shifted_mask <score_wide_int> (scores[b + n], 1, false);
	    }
	  if (has_kind)
	    *score += wi::shifted_mask <score_wide_int> (scores[b + nconstructs],
						     1, false);
	  if (has_arch)
	    *score += wi::shifted_mask <score_wide_int> (scores[b + nconstructs] + 1,
						     1, false);
	  if (has_isa)
	    *score += wi::shifted_mask <score_wide_int> (scores[b + nconstructs] + 2,
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
  score_wide_int score;
  /* Score if in declare simd clone.  */
  score_wide_int score_in_declare_simd_clone;
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

  score_wide_int max_score = -1;
  varentry2 = NULL;
  FOR_EACH_VEC_SAFE_ELT (entryp->variants, i, varentry1)
    if (matches[i])
      {
	score_wide_int score
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

  if (omp_has_novariants () == 1)
    return base;

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
      score_wide_int max_score1 = 0;
      score_wide_int max_score2 = 0;
      bool first = true;
      unsigned int i;
      tree attr1, attr2;
      omp_declare_variant_base_entry entry;
      entry.base = cgraph_node::get_create (base);
      entry.node = NULL;
      vec_alloc (entry.variants, variants.length ());
      FOR_EACH_VEC_ELT (variants, i, attr1)
	{
	  score_wide_int score1;
	  score_wide_int score2;
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
      if (DECL_ASSEMBLER_NAME_SET_P (base))
	SET_DECL_ASSEMBLER_NAME (alt, DECL_ASSEMBLER_NAME (base));
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
  score_wide_int max_score1 = 0;
  score_wide_int max_score2 = 0;
  bool first = true;
  FOR_EACH_VEC_ELT (variants, i, attr1)
    if (attr1)
      {
	if (variant1)
	  {
	    score_wide_int score1;
	    score_wide_int score2;
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

      for (score_wide_int *w = &varentry->score; ;
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
      for (score_wide_int *w = &varentry.score; ;
	   w = &varentry.score_in_declare_simd_clone)
	{
	  unsigned len2 = streamer_read_hwi (ib);
	  HOST_WIDE_INT arr[WIDE_INT_MAX_HWIS (1024)];
	  gcc_assert (len2 <= WIDE_INT_MAX_HWIS (1024));
	  for (unsigned int j = 0; j < len2; j++)
	    arr[j] = streamer_read_hwi (ib);
	  *w = score_wide_int::from_array (arr, len2, true);
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

#include "gt-omp-general.h"
