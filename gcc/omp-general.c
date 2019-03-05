/* General types and functions that are uselful for processing of OpenMP,
   OpenACC and similar directivers at various stages of compilation.

   Copyright (C) 2005-2019 Free Software Foundation, Inc.

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

enum omp_requires omp_requires_mask;

tree
omp_find_clause (tree clauses, enum omp_clause_code kind)
{
  for (; clauses ; clauses = OMP_CLAUSE_CHAIN (clauses))
    if (OMP_CLAUSE_CODE (clauses) == kind)
      return clauses;

  return NULL_TREE;
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
  bool simd = gimple_omp_for_kind (for_stmt) & GF_OMP_FOR_SIMD;
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
  fd->tiling = NULL_TREE;
  fd->collapse = 1;
  fd->ordered = 0;
  fd->sched_kind = OMP_CLAUSE_SCHEDULE_STATIC;
  fd->sched_modifiers = 0;
  fd->chunk_size = NULL_TREE;
  fd->simd_schedule = false;
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

      loop->cond_code = gimple_omp_for_cond (for_stmt, i);
      loop->n2 = gimple_omp_for_final (for_stmt, i);
      gcc_assert (loop->cond_code != NE_EXPR
		  || (gimple_omp_for_kind (for_stmt)
		      != GF_OMP_FOR_KIND_OACC_LOOP));

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
	      if (TREE_CODE (n) != INTEGER_CST
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
	      if (TREE_CODE (n1) != INTEGER_CST
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
	      t = fold_build2_loc (loc, PLUS_EXPR, itype,
				   fold_convert_loc (loc, itype, loop->step),
				   t);
	      t = fold_build2_loc (loc, PLUS_EXPR, itype, t,
				   fold_convert_loc (loc, itype, loop->n2));
	      t = fold_build2_loc (loc, MINUS_EXPR, itype, t,
				   fold_convert_loc (loc, itype, loop->n1));
	      if (TYPE_UNSIGNED (itype) && loop->cond_code == GT_EXPR)
		{
		  tree step = fold_convert_loc (loc, itype, loop->step);
		  t = fold_build2_loc (loc, TRUNC_DIV_EXPR, itype,
				       fold_build1_loc (loc, NEGATE_EXPR,
							itype, t),
				       fold_build1_loc (loc, NEGATE_EXPR,
							itype, step));
		}
	      else
		t = fold_build2_loc (loc, TRUNC_DIV_EXPR, itype, t,
				     fold_convert_loc (loc, itype,
						       loop->step));
	      t = fold_convert_loc (loc, long_long_unsigned_type_node, t);
	      if (count != NULL_TREE)
		count = fold_build2_loc (loc, MULT_EXPR,
					 long_long_unsigned_type_node,
					 count, t);
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
	*collapse_count = fold_convert_loc (loc, iter_type, count);
      else
	*collapse_count = create_tmp_var (iter_type, ".count");
    }

  if (fd->collapse > 1 || fd->tiling || (fd->ordered && loops))
    {
      fd->loop.v = *collapse_iter;
      fd->loop.n1 = build_int_cst (TREE_TYPE (fd->loop.v), 0);
      fd->loop.n2 = *collapse_count;
      fd->loop.step = build_int_cst (TREE_TYPE (fd->loop.v), 1);
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

  auto_vector_sizes sizes;
  targetm.vectorize.autovectorize_vector_sizes (&sizes);
  if (!sizes.is_empty ())
    {
      poly_uint64 vf = 0;
      for (unsigned int i = 0; i < sizes.length (); ++i)
	vf = ordered_max (vf, sizes[i]);
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
	if (!strncmp (c, "nvptx", strlen ("nvptx")))
	  return 32;
	else if ((c = strchr (c, ',')))
	  c++;
      }
  return 0;
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

/*  Process the routine's dimension clauess to generate an attribute
    value.  Issue diagnostics as appropriate.  We default to SEQ
    (OpenACC 2.5 clarifies this). All dimensions have a size of zero
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
	  if (level >= 0)
	    error_at (OMP_CLAUSE_LOCATION (clauses),
		      "multiple loop axes specified for routine");
	  level = ix;
	  break;
	}

  /* Default to SEQ.  */
  if (level < 0)
    level = GOMP_DIM_MAX;

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
