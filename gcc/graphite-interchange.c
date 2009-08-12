/* Interchange heuristics and transform for loop interchange on
   polyhedral representation.

   Copyright (C) 2009 Free Software Foundation, Inc.
   Contributed by Sebastian Pop <sebastian.pop@amd.com> and
   Harsha Jagasia <harsha.jagasia@amd.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "ggc.h"
#include "tree.h"
#include "rtl.h"
#include "output.h"
#include "basic-block.h"
#include "diagnostic.h"
#include "tree-flow.h"
#include "toplev.h"
#include "tree-dump.h"
#include "timevar.h"
#include "cfgloop.h"
#include "tree-chrec.h"
#include "tree-data-ref.h"
#include "tree-scalar-evolution.h"
#include "tree-pass.h"
#include "domwalk.h"
#include "value-prof.h"
#include "pointer-set.h"
#include "gimple.h"
#include "params.h"

#ifdef HAVE_cloog
#include "cloog/cloog.h"
#include "ppl_c.h"
#include "sese.h"
#include "graphite-ppl.h"
#include "graphite.h"
#include "graphite-poly.h"

/* Returns the subscript dimension defined by CSTR in PDR.  */

static ppl_dimension_type
compute_subscript (poly_dr_p pdr, ppl_const_Constraint_t cstr)
{
  graphite_dim_t i;
  ppl_Linear_Expression_t expr;
  ppl_Coefficient_t coef;
  Value val;

  value_init (val);
  ppl_new_Coefficient (&coef);

  for (i = 0; i < pdr_nb_subscripts (pdr); i++)
    {
      ppl_dimension_type sub_dim = pdr_subscript_dim (pdr, i);

      ppl_new_Linear_Expression_from_Constraint (&expr, cstr);
      ppl_Linear_Expression_coefficient (expr, sub_dim, coef);
      ppl_delete_Linear_Expression (expr);
      ppl_Coefficient_to_mpz_t (coef, val);

      if (value_notzero_p (val))
	{
	  gcc_assert (value_one_p (val)
		      || value_mone_p (val));

	  value_clear (val);
	  ppl_delete_Coefficient (coef);
	  return sub_dim;
	}
    }

  gcc_unreachable ();
  return 0;
}

static void
compute_array_size_cstr (ppl_dimension_type sub_dim, Value res,
			 ppl_const_Constraint_t cstr)
{
  ppl_Linear_Expression_t expr;
  ppl_Coefficient_t coef;
  Value val;

  value_init (val);
  ppl_new_Coefficient (&coef);
  ppl_new_Linear_Expression_from_Constraint (&expr, cstr);
  ppl_Linear_Expression_coefficient (expr, sub_dim, coef);
  ppl_Coefficient_to_mpz_t (coef, val);

  value_set_si (res, 0);

  if (value_notzero_p (val))
    {
      gcc_assert (value_one_p (val) || value_mone_p (val));
      ppl_Linear_Expression_inhomogeneous_term (expr, coef);
      ppl_Coefficient_to_mpz_t (coef, res);
      value_absolute (res, res);
    }

  value_clear (val);
  ppl_delete_Coefficient (coef);
  ppl_delete_Linear_Expression (expr);
}

/* Returns in ARRAY_SIZE the size in bytes of the array PDR for the
   subscript at dimension SUB_DIM.  */

static void
compute_array_size_poly (poly_dr_p pdr, ppl_dimension_type sub_dim, Value array_size,
			 ppl_const_Polyhedron_t ph)
{
  ppl_const_Constraint_System_t pcs;
  ppl_Constraint_System_const_iterator_t cit, cend;
  ppl_const_Constraint_t cstr;
  Value val;
  Value res;

  if (sub_dim >= pdr_subscript_dim (pdr, pdr_nb_subscripts (pdr)))
    {
      value_set_si (array_size, 1);
      return;
    }

  value_init (val);
  value_init (res);

  value_set_si (res, 0);

  ppl_Polyhedron_get_constraints (ph, &pcs);
  ppl_new_Constraint_System_const_iterator (&cit);
  ppl_new_Constraint_System_const_iterator (&cend);
      
  for (ppl_Constraint_System_begin (pcs, cit),
	 ppl_Constraint_System_end (pcs, cend);
       !ppl_Constraint_System_const_iterator_equal_test (cit, cend);
       ppl_Constraint_System_const_iterator_increment (cit))
    {
      ppl_Constraint_System_const_iterator_dereference (cit, &cstr);

      if (ppl_Constraint_type (cstr) == PPL_CONSTRAINT_TYPE_EQUAL)
	continue;

      compute_array_size_cstr (sub_dim, val, cstr);
      value_max (res, res, val);
    }

  compute_array_size_poly (pdr, sub_dim + 1, val, ph);
  value_multiply (array_size, res, val);

  value_clear (res);
  value_clear (val);
}

/* Initializes ARRAY_SIZE, the size in bytes of the array for the
   subscript at dimension SUB_DIM in PDR.  */

static void
compute_array_size (poly_dr_p pdr, ppl_dimension_type sub_dim, Value array_size)
{
  ppl_Pointset_Powerset_C_Polyhedron_t data_container = PDR_DATA_CONTAINER (pdr);
  ppl_Pointset_Powerset_C_Polyhedron_iterator_t it, end;
  Value val;

  value_set_si (array_size, 1);
  if (sub_dim >= pdr_subscript_dim (pdr, pdr_nb_subscripts (pdr)))
    return;

  value_init (val);
  ppl_new_Pointset_Powerset_C_Polyhedron_iterator (&it);
  ppl_new_Pointset_Powerset_C_Polyhedron_iterator (&end);

  for (ppl_Pointset_Powerset_C_Polyhedron_iterator_begin (data_container, it),
       ppl_Pointset_Powerset_C_Polyhedron_iterator_end (data_container, end);
       !ppl_Pointset_Powerset_C_Polyhedron_iterator_equal_test (it, end);
       ppl_Pointset_Powerset_C_Polyhedron_iterator_increment (it))
    {
      ppl_const_Polyhedron_t ph;

      ppl_Pointset_Powerset_C_Polyhedron_iterator_dereference (it, &ph);
      compute_array_size_poly (pdr, sub_dim, val, ph);
      value_max (array_size, array_size, val);
    }

  value_clear (val);
  ppl_delete_Pointset_Powerset_C_Polyhedron_iterator (it);
  ppl_delete_Pointset_Powerset_C_Polyhedron_iterator (end);
}

/* Computes ACCESS_STRIDES, the sum of all the strides of PDR at
   LOOP_DEPTH.  */

static void
gather_access_strides_poly (poly_dr_p pdr, ppl_const_Polyhedron_t ph,
			    ppl_dimension_type loop_dim, Value res)
{
  ppl_const_Constraint_System_t pcs;
  ppl_Constraint_System_const_iterator_t cit, cend;
  ppl_const_Constraint_t cstr;
  ppl_Linear_Expression_t expr;
  ppl_Coefficient_t coef;
  Value stride;
  Value array_size;

  value_init (array_size);
  value_init (stride);
  ppl_new_Coefficient (&coef);
  value_set_si (res, 0);

  ppl_Polyhedron_get_constraints (ph, &pcs);
  ppl_new_Constraint_System_const_iterator (&cit);
  ppl_new_Constraint_System_const_iterator (&cend);

  for (ppl_Constraint_System_begin (pcs, cit),
	 ppl_Constraint_System_end (pcs, cend);
       !ppl_Constraint_System_const_iterator_equal_test (cit, cend);
       ppl_Constraint_System_const_iterator_increment (cit))
    {
      ppl_Constraint_System_const_iterator_dereference (cit, &cstr);
      ppl_new_Linear_Expression_from_Constraint (&expr, cstr);
      ppl_Linear_Expression_coefficient (expr, loop_dim, coef);
      ppl_delete_Linear_Expression (expr);
      ppl_Coefficient_to_mpz_t (coef, stride);

      if (value_zero_p (stride))
	continue;

      value_absolute (stride, stride);
      compute_array_size (pdr, compute_subscript (pdr, cstr), array_size);
      value_multiply (stride, stride, array_size);
      value_addto (res, res, stride);
    }

  value_clear (array_size);
  value_clear (stride);
  ppl_delete_Coefficient (coef);
  ppl_delete_Constraint_System_const_iterator (cit);
  ppl_delete_Constraint_System_const_iterator (cend);
}

/* Computes ACCESS_STRIDES, the sum of all the strides of PDR at
   LOOP_DEPTH.  */

static void
gather_access_strides (poly_dr_p pdr, graphite_dim_t loop_depth,
		       Value access_strides)
{
  ppl_dimension_type loop_dim = pdr_iterator_dim (pdr, loop_depth);

  ppl_Pointset_Powerset_C_Polyhedron_t accesses = PDR_ACCESSES (pdr);
  ppl_Pointset_Powerset_C_Polyhedron_iterator_t it, end;
  Value res;

  value_init (res);
  ppl_new_Pointset_Powerset_C_Polyhedron_iterator (&it);
  ppl_new_Pointset_Powerset_C_Polyhedron_iterator (&end);

  for (ppl_Pointset_Powerset_C_Polyhedron_iterator_begin (accesses, it),
       ppl_Pointset_Powerset_C_Polyhedron_iterator_end (accesses, end);
       !ppl_Pointset_Powerset_C_Polyhedron_iterator_equal_test (it, end);
       ppl_Pointset_Powerset_C_Polyhedron_iterator_increment (it))
    {
      ppl_const_Polyhedron_t ph;

      ppl_Pointset_Powerset_C_Polyhedron_iterator_dereference (it, &ph);
      gather_access_strides_poly (pdr, ph, loop_dim, res);
      value_addto (access_strides, access_strides, res);
    }

  value_clear (res);
  ppl_delete_Pointset_Powerset_C_Polyhedron_iterator (it);
  ppl_delete_Pointset_Powerset_C_Polyhedron_iterator (end);
}

/* Returns true when it is profitable to interchange loop at depth1
   and loop at depth2 with depth1 < depth2 for the polyhedral black
   box PBB.  */

static bool
pbb_interchange_profitable_p (graphite_dim_t depth1, graphite_dim_t depth2, poly_bb_p pbb)
{
  int i;
  poly_dr_p pdr;
  Value access_strides1, access_strides2;
  bool res;

  gcc_assert (depth1 < depth2);

  value_init (access_strides1);
  value_init (access_strides2);

  value_set_si (access_strides1, 0);
  value_set_si (access_strides2, 0);

  for (i = 0; VEC_iterate (poly_dr_p, PBB_DRS (pbb), i, pdr); i++)
    {
      gather_access_strides (pdr, depth1, access_strides1);
      gather_access_strides (pdr, depth2, access_strides2);
    }

  res = value_lt (access_strides1, access_strides2);

  value_clear (access_strides1);
  value_clear (access_strides2);

  return res;
}

/* Interchanges the loops at DEPTH1 and DEPTH2 of the original
   scattering and assigns the resulting polyhedron to the transformed
   scattering.  */

static void
pbb_interchange_loop_depths (graphite_dim_t depth1, graphite_dim_t depth2, poly_bb_p pbb)
{
  ppl_dimension_type i, dim;
  ppl_dimension_type *map;
  ppl_Polyhedron_t poly = PBB_TRANSFORMED_SCATTERING (pbb);
  ppl_dimension_type dim1 = psct_iterator_dim (pbb, depth1);
  ppl_dimension_type dim2 = psct_iterator_dim (pbb, depth2);

  ppl_Polyhedron_space_dimension (poly, &dim);
  map = (ppl_dimension_type *) XNEWVEC (ppl_dimension_type, dim);

  for (i = 0; i < dim; i++)
    map[i] = i;

  map[dim1] = dim2;
  map[dim2] = dim1;

  ppl_Polyhedron_map_space_dimensions (poly, map, dim);
  free (map);
}

/* Interchanges all the loop depths that are considered profitable for PBB.  */

static bool
pbb_do_interchange (poly_bb_p pbb, scop_p scop)
{
  graphite_dim_t i, j;
  bool transform_done = false;

  for (i = 0; i < pbb_dim_iter_domain (pbb); i++)
    for (j = i + 1; j < pbb_dim_iter_domain (pbb); j++)
      if (pbb_interchange_profitable_p (i, j, pbb))
	{
	  pbb_interchange_loop_depths (i, j, pbb);

	  if (graphite_legal_transform (scop))
	    {
	      transform_done = true;

	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file,
			 "PBB %d: loops at depths %d and %d will be interchanged.\n",
			 GBB_BB (PBB_BLACK_BOX (pbb))->index, (int) i, (int) j);
	    }
	  else
	    /* Undo the transform.  */
	    pbb_interchange_loop_depths (j, i, pbb);
	}

  return transform_done;
}

/* Interchanges all the loop depths that are considered profitable for SCOP.  */

bool
scop_do_interchange (scop_p scop)
{
  int i;
  poly_bb_p pbb;
  bool transform_done = false;

  store_scattering (scop);

  for (i = 0; VEC_iterate (poly_bb_p, SCOP_BBS (scop), i, pbb); i++)
    transform_done |= pbb_do_interchange (pbb, scop);

  if (!transform_done)
    return false;

  if (!graphite_legal_transform (scop))
    {
      restore_scattering (scop);
      return false;
    }

  return transform_done;
}

#endif

