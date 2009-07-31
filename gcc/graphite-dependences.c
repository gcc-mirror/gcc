/* Data dependence analysis for Graphite.
   Copyright (C) 2009 Free Software Foundation, Inc.
   Contributed by Sebastian Pop <sebastian.pop@amd.com> and
   Konrad Trifunovic <konrad.trifunovic@inria.fr>.

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
#include "pointer-set.h"
#include "gimple.h"

#ifdef HAVE_cloog
#include "cloog/cloog.h"
#include "ppl_c.h"
#include "sese.h"
#include "graphite-ppl.h"
#include "graphite.h"
#include "graphite-poly.h"
#include "graphite-dependences.h"

/* Creates a new polyhedral data reference pair and
   returns it.  Parameter SOURCE denotes a source data reference
   while parameter SINK denotes a sink data reference.  Both
   SOURCE and SINK define a pair of references, thus they
   define an edge in DDG (Data Dependence Graph).  */

static poly_dr_pair_p
new_poly_dr_pair (poly_dr_p source,
                  poly_dr_p sink,
                  ppl_Pointset_Powerset_C_Polyhedron_t ddp)
{
  poly_dr_pair_p pdrpp;

  pdrpp = XNEW (struct poly_dr_pair);
  pdrpp->source = source;
  pdrpp->sink = sink;
  pdrpp->ddp = ddp;

  return pdrpp;
}

/* Comparison function for poly_dr_pair hash table.  */

int
eq_poly_dr_pair_p (const void *pdrpp1, const void *pdrpp2)
{
  const struct poly_dr_pair *p1 = (const struct poly_dr_pair *) pdrpp1;
  const struct poly_dr_pair *p2 = (const struct poly_dr_pair *) pdrpp2;

  return (p1->source == p2->source
          && p1->sink == p2->sink);
}

/* Hash function for poly_dr_pair hashtable.  */

hashval_t
hash_poly_dr_pair_p (const void *pdrpp)
{
  const struct poly_dr_pair *p = (const struct poly_dr_pair *) pdrpp;

  return (hashval_t) ((long) p->source + (long) p->sink);
}

/* Returns a polyhedron of dimension DIM.

   Maps the dimensions [0, ..., cut - 1] of polyhedron P to OFFSET0
   and the dimensions [cut, ..., nb_dim] to DIM - GDIM.  */

static ppl_Pointset_Powerset_C_Polyhedron_t
map_into_dep_poly (graphite_dim_t dim, graphite_dim_t gdim,
		   ppl_Pointset_Powerset_C_Polyhedron_t p,
		   graphite_dim_t cut,
		   graphite_dim_t offset)
{
  ppl_Pointset_Powerset_C_Polyhedron_t res;

  ppl_new_Pointset_Powerset_C_Polyhedron_from_Pointset_Powerset_C_Polyhedron
    (&res, p);
  ppl_insert_dimensions_pointset (res, 0, offset);
  ppl_insert_dimensions_pointset (res, offset + cut,
				  dim - offset - cut - gdim);

  return res;
}

/* Swap [cut0, ..., cut1] to the end of DR: "a CUT0 b CUT1 c" is
   transformed into "a CUT0 c CUT1' b"

   Add NB0 zeros before "a":  "00...0 a CUT0 c CUT1' b"
   Add NB1 zeros between "a" and "c":  "00...0 a 00...0 c CUT1' b"
   Add DIM - NB0 - NB1 - PDIM zeros between "c" and "b":
   "00...0 a 00...0 c 00...0 b".  */

static ppl_Pointset_Powerset_C_Polyhedron_t
map_dr_into_dep_poly (graphite_dim_t dim,
		      ppl_Pointset_Powerset_C_Polyhedron_t dr,
		      graphite_dim_t cut0, graphite_dim_t cut1,
		      graphite_dim_t nb0, graphite_dim_t nb1)
{
  ppl_dimension_type pdim;
  ppl_dimension_type *map;
  ppl_Pointset_Powerset_C_Polyhedron_t res;
  ppl_dimension_type i;

  ppl_new_Pointset_Powerset_C_Polyhedron_from_Pointset_Powerset_C_Polyhedron
    (&res, dr);
  ppl_Pointset_Powerset_C_Polyhedron_space_dimension (res, &pdim);

  map = (ppl_dimension_type *) XNEWVEC (ppl_dimension_type, pdim);

  /* First mapping: move 'g' vector to right position.  */
  for (i = 0; i < cut0; i++)
    map[i] = i;

  for (i = cut0; i < cut1; i++)
    map[i] = pdim - cut1 + i;

  for (i = cut1; i < pdim; i++)
    map[i] = cut0 + i - cut1;

  ppl_Pointset_Powerset_C_Polyhedron_map_space_dimensions (res, map, pdim);
  free (map);

  /* After swapping 's' and 'g' vectors, we have to update a new cut.  */
  cut1 = pdim - cut1 + cut0;

  ppl_insert_dimensions_pointset (res, 0, nb0);
  ppl_insert_dimensions_pointset (res, nb0 + cut0, nb1);
  ppl_insert_dimensions_pointset (res, nb0 + nb1 + cut1,
				  dim - nb0 - nb1 - pdim);

  return res;
}

/* Builds a constraints of the form "POS1 - POS2 CSTR_TYPE C" */

static ppl_Constraint_t
build_pairwise_constraint (graphite_dim_t dim,
		           graphite_dim_t pos1, graphite_dim_t pos2,
			   int c, enum ppl_enum_Constraint_Type cstr_type)
{
  ppl_Linear_Expression_t expr;
  ppl_Constraint_t cstr;
  ppl_Coefficient_t coef;
  Value v, v_op, v_c;

  value_init (v);
  value_init (v_op);
  value_init (v_c);

  value_set_si (v, 1);
  value_set_si (v_op, -1);
  value_set_si (v_c, c);

  ppl_new_Coefficient (&coef);
  ppl_new_Linear_Expression_with_dimension (&expr, dim);

  ppl_assign_Coefficient_from_mpz_t (coef, v);
  ppl_Linear_Expression_add_to_coefficient (expr, pos1, coef);
  ppl_assign_Coefficient_from_mpz_t (coef, v_op);
  ppl_Linear_Expression_add_to_coefficient (expr, pos2, coef);
  ppl_assign_Coefficient_from_mpz_t (coef, v_c);
  ppl_Linear_Expression_add_to_inhomogeneous (expr, coef);

  ppl_new_Constraint (&cstr, expr, cstr_type);

  ppl_delete_Linear_Expression (expr);
  ppl_delete_Coefficient (coef);
  value_clear (v);
  value_clear (v_op);
  value_clear (v_c);

  return cstr;
}

/* Builds subscript equality constraints.  */

static ppl_Pointset_Powerset_C_Polyhedron_t
dr_equality_constraints (graphite_dim_t dim,
		         graphite_dim_t pos, graphite_dim_t nb_subscripts)
{
  ppl_Polyhedron_t subscript_equalities;
  ppl_Pointset_Powerset_C_Polyhedron_t res;
  Value v, v_op;
  graphite_dim_t i;

  value_init (v);
  value_init (v_op);
  value_set_si (v, 1);
  value_set_si (v_op, -1);

  ppl_new_C_Polyhedron_from_space_dimension (&subscript_equalities, dim, 0);
  for (i = 0; i < nb_subscripts; i++)
    {
      ppl_Linear_Expression_t expr;
      ppl_Constraint_t cstr;
      ppl_Coefficient_t coef;

      ppl_new_Coefficient (&coef);
      ppl_new_Linear_Expression_with_dimension (&expr, dim);

      ppl_assign_Coefficient_from_mpz_t (coef, v);
      ppl_Linear_Expression_add_to_coefficient (expr, pos + i, coef);
      ppl_assign_Coefficient_from_mpz_t (coef, v_op);
      ppl_Linear_Expression_add_to_coefficient (expr, pos + i + nb_subscripts,
						coef);

      ppl_new_Constraint (&cstr, expr, PPL_CONSTRAINT_TYPE_EQUAL);
      ppl_Polyhedron_add_constraint (subscript_equalities, cstr);

      ppl_delete_Linear_Expression (expr);
      ppl_delete_Constraint (cstr);
      ppl_delete_Coefficient (coef);
    }

  ppl_new_Pointset_Powerset_C_Polyhedron_from_C_Polyhedron
    (&res, subscript_equalities);
  value_clear (v);
  value_clear (v_op);
  ppl_delete_Polyhedron (subscript_equalities);

  return res;
}

/* Builds scheduling equality constraints.  */

static ppl_Pointset_Powerset_C_Polyhedron_t
build_pairwise_scheduling_equality (graphite_dim_t dim,
		                    graphite_dim_t pos, graphite_dim_t offset)
{
  ppl_Pointset_Powerset_C_Polyhedron_t res;
  ppl_Polyhedron_t equalities;
  ppl_Constraint_t cstr;

  ppl_new_C_Polyhedron_from_space_dimension (&equalities, dim, 0);

  cstr = build_pairwise_constraint (dim, pos, pos + offset, 0,
				    PPL_CONSTRAINT_TYPE_EQUAL);
  ppl_Polyhedron_add_constraint (equalities, cstr);
  ppl_delete_Constraint (cstr);

  ppl_new_Pointset_Powerset_C_Polyhedron_from_C_Polyhedron (&res, equalities);
  ppl_delete_Polyhedron (equalities);
  return res;
}

/* Builds scheduling inequality constraints.  */

static ppl_Pointset_Powerset_C_Polyhedron_t
build_pairwise_scheduling_inequality (graphite_dim_t dim,
				      graphite_dim_t pos,
				      graphite_dim_t offset,
				      bool direction)
{
  ppl_Pointset_Powerset_C_Polyhedron_t res;
  ppl_Polyhedron_t equalities;
  ppl_Constraint_t cstr;

  ppl_new_C_Polyhedron_from_space_dimension (&equalities, dim, 0);

  if (direction)
    cstr = build_pairwise_constraint (dim, pos, pos + offset, -1,
				      PPL_CONSTRAINT_TYPE_GREATER_OR_EQUAL);
  else
    cstr = build_pairwise_constraint (dim, pos, pos + offset, 1,
				      PPL_CONSTRAINT_TYPE_LESS_OR_EQUAL);

  ppl_Polyhedron_add_constraint (equalities, cstr);
  ppl_delete_Constraint (cstr);

  ppl_new_Pointset_Powerset_C_Polyhedron_from_C_Polyhedron (&res, equalities);
  ppl_delete_Polyhedron (equalities);
  return res;
}

/* Returns true when adding the lexicographical constraints at level I
   to the RES dependence polyhedron returns an empty polyhedron.  */

static bool
lexicographically_gt_p (ppl_Pointset_Powerset_C_Polyhedron_t res,
			graphite_dim_t dim,
			graphite_dim_t offset,
			bool direction, graphite_dim_t i)
{
  ppl_Pointset_Powerset_C_Polyhedron_t ineq;
  bool empty_p;

  ineq = build_pairwise_scheduling_inequality (dim, i, offset,
					       direction);
  ppl_Pointset_Powerset_C_Polyhedron_intersection_assign (ineq, res);
  empty_p = ppl_Pointset_Powerset_C_Polyhedron_is_empty (ineq);
  if (!empty_p)
    ppl_Pointset_Powerset_C_Polyhedron_intersection_assign (res, ineq);
  ppl_delete_Pointset_Powerset_C_Polyhedron (ineq);

  return !empty_p;
}

/* Build the precedence constraints for the lexicographical comparison
   of time vectors RES following the lexicographical order.  */

static void
build_lexicographically_gt_constraint (ppl_Pointset_Powerset_C_Polyhedron_t *res,
				       graphite_dim_t dim,
				       graphite_dim_t tdim1,
				       graphite_dim_t offset,
				       bool direction)
{
  graphite_dim_t i;

  if (lexicographically_gt_p (*res, dim, offset, direction, 0))
    return;

  for (i = 0; i < tdim1 - 1; i++)
    {
      ppl_Pointset_Powerset_C_Polyhedron_t sceq;

      sceq = build_pairwise_scheduling_equality (dim, i, offset);
      ppl_Pointset_Powerset_C_Polyhedron_intersection_assign (*res, sceq);
      ppl_delete_Pointset_Powerset_C_Polyhedron (sceq);

      if (lexicographically_gt_p (*res, dim, offset, direction, i + 1))
	return;
    }

  if (i == tdim1 - 1)
    {
      ppl_delete_Pointset_Powerset_C_Polyhedron (*res);
      ppl_new_Pointset_Powerset_C_Polyhedron_from_space_dimension (res, dim, 1);
    }
}

/* Build the dependence polyhedron for data references PDR1 and PDR2.  */

static ppl_Pointset_Powerset_C_Polyhedron_t
dependence_polyhedron_1 (poly_bb_p pbb1, poly_bb_p pbb2,
		         ppl_Pointset_Powerset_C_Polyhedron_t d1,
		         ppl_Pointset_Powerset_C_Polyhedron_t d2,
		         poly_dr_p pdr1, poly_dr_p pdr2,
	                 ppl_Polyhedron_t s1, ppl_Polyhedron_t s2,
		         bool direction,
		         bool original_scattering_p)
{
  scop_p scop = PBB_SCOP (pbb1);
  graphite_dim_t tdim1 = original_scattering_p ?
    pbb_nb_scattering_orig (pbb1) : pbb_nb_scattering_transform (pbb1);
  graphite_dim_t tdim2 = original_scattering_p ?
    pbb_nb_scattering_orig (pbb2) : pbb_nb_scattering_transform (pbb2);
  graphite_dim_t ddim1 = pbb_dim_iter_domain (pbb1);
  graphite_dim_t ddim2 = pbb_dim_iter_domain (pbb2);
  graphite_dim_t sdim1 = pdr_nb_subscripts (pdr1) + 1;
  graphite_dim_t gdim = scop_nb_params (scop);
  graphite_dim_t dim1 = pdr_dim (pdr1);
  graphite_dim_t dim2 = pdr_dim (pdr2);
  graphite_dim_t dim = tdim1 + tdim2 + dim1 + dim2;
  ppl_Pointset_Powerset_C_Polyhedron_t res;
  ppl_Pointset_Powerset_C_Polyhedron_t id1, id2, isc1, isc2, idr1, idr2;
  ppl_Pointset_Powerset_C_Polyhedron_t sc1, sc2, dreq;

  gcc_assert (PBB_SCOP (pbb1) == PBB_SCOP (pbb2));
  ppl_new_Pointset_Powerset_C_Polyhedron_from_C_Polyhedron (&sc1, s1);
  ppl_new_Pointset_Powerset_C_Polyhedron_from_C_Polyhedron (&sc2, s2);

  id1 = map_into_dep_poly (dim, gdim, d1, ddim1, tdim1);
  id2 = map_into_dep_poly (dim, gdim, d2, ddim2, tdim1 + ddim1 + tdim2);
  isc1 = map_into_dep_poly (dim, gdim, sc1, ddim1 + tdim1, 0);
  isc2 = map_into_dep_poly (dim, gdim, sc2, ddim2 + tdim2, tdim1 + ddim1);

  idr1 = map_dr_into_dep_poly (dim, PDR_ACCESSES (pdr1), ddim1, ddim1 + gdim,
			       tdim1, tdim2 + ddim2);
  idr2 = map_dr_into_dep_poly (dim, PDR_ACCESSES (pdr2), ddim2, ddim2 + gdim,
			       tdim1 + ddim1 + tdim2, sdim1);

  /* Now add the subscript equalities.  */
  dreq = dr_equality_constraints (dim, tdim1 + ddim1 + tdim2 + ddim2, sdim1);

  ppl_new_Pointset_Powerset_C_Polyhedron_from_space_dimension (&res, dim, 0);
  ppl_Pointset_Powerset_C_Polyhedron_intersection_assign (res, id1);
  ppl_Pointset_Powerset_C_Polyhedron_intersection_assign (res, id2);
  ppl_Pointset_Powerset_C_Polyhedron_intersection_assign (res, isc1);
  ppl_Pointset_Powerset_C_Polyhedron_intersection_assign (res, isc2);
  ppl_Pointset_Powerset_C_Polyhedron_intersection_assign (res, idr1);
  ppl_Pointset_Powerset_C_Polyhedron_intersection_assign (res, idr2);
  ppl_Pointset_Powerset_C_Polyhedron_intersection_assign (res, dreq);
  ppl_delete_Pointset_Powerset_C_Polyhedron (id1);
  ppl_delete_Pointset_Powerset_C_Polyhedron (id2);
  ppl_delete_Pointset_Powerset_C_Polyhedron (sc1);
  ppl_delete_Pointset_Powerset_C_Polyhedron (sc2);
  ppl_delete_Pointset_Powerset_C_Polyhedron (isc1);
  ppl_delete_Pointset_Powerset_C_Polyhedron (isc2);
  ppl_delete_Pointset_Powerset_C_Polyhedron (idr1);
  ppl_delete_Pointset_Powerset_C_Polyhedron (idr2);
  ppl_delete_Pointset_Powerset_C_Polyhedron (dreq);

  if (!ppl_Pointset_Powerset_C_Polyhedron_is_empty (res))
    build_lexicographically_gt_constraint (&res, dim, MIN (tdim1, tdim2),
					   tdim1 + ddim1, direction);
  return res;
}

/* Build the dependence polyhedron for data references PDR1 and PDR2.
   If possible use already cached information.  */

static ppl_Pointset_Powerset_C_Polyhedron_t
dependence_polyhedron (poly_bb_p pbb1, poly_bb_p pbb2,
		       ppl_Pointset_Powerset_C_Polyhedron_t d1,
		       ppl_Pointset_Powerset_C_Polyhedron_t d2,
		       poly_dr_p pdr1, poly_dr_p pdr2,
	               ppl_Polyhedron_t s1, ppl_Polyhedron_t s2,
		       bool direction,
		       bool original_scattering_p)
{
  poly_dr_pair tmp;
  PTR *x = NULL;
  ppl_Pointset_Powerset_C_Polyhedron_t res;

  if (original_scattering_p)
    {
      tmp.source = pdr1;
      tmp.sink = pdr2;
      x = htab_find_slot (SCOP_ORIGINAL_PDR_PAIRS (PBB_SCOP (pbb1)),
                          &tmp, INSERT);

      if (x && *x)
        {
          if (dump_file && (dump_flags & TDF_DETAILS))
            fprintf (dump_file, "\nddp cache: hit.\n");
          return ((poly_dr_pair *)*x)->ddp;
        }
      else if (dump_file && (dump_flags & TDF_DETAILS))
        fprintf (dump_file, "\nddp cache: miss.\n");
    }

  res = dependence_polyhedron_1 (pbb1, pbb2, d1, d2, pdr1, pdr2,
                                 s1, s2, direction, original_scattering_p);

  if (original_scattering_p)
    {
      gcc_assert (x && *x == NULL);
      *x = new_poly_dr_pair (pdr1, pdr2, res);

      if (dump_file && (dump_flags & TDF_DETAILS))
        fprintf (dump_file, "\nddp cache: add element.\n");
    }

  return res;
}

/* Returns true when the PBB_TRANSFORMED_SCATTERING functions of PBB1
   and PBB2 respect the data dependences of PBB_ORIGINAL_SCATTERING
   functions.  */

static bool
graphite_legal_transform_dr (poly_bb_p pbb1, poly_bb_p pbb2,
			     poly_dr_p pdr1, poly_dr_p pdr2)
{
  ppl_Pointset_Powerset_C_Polyhedron_t d1 = PBB_DOMAIN (pbb1);
  ppl_Pointset_Powerset_C_Polyhedron_t d2 = PBB_DOMAIN (pbb2);
  ppl_Polyhedron_t so1 = PBB_ORIGINAL_SCATTERING (pbb1);
  ppl_Polyhedron_t so2 = PBB_ORIGINAL_SCATTERING (pbb2);
  ppl_Pointset_Powerset_C_Polyhedron_t po;

  graphite_dim_t sdim1 = pdr_nb_subscripts (pdr1) + 1;
  graphite_dim_t sdim2 = pdr_nb_subscripts (pdr2) + 1;

  if (sdim1 != sdim2)
    return true;

  po = dependence_polyhedron (pbb1, pbb2, d1, d2, pdr1, pdr2, so1, so2,
			      true, true);

  if (ppl_Pointset_Powerset_C_Polyhedron_is_empty (po))
    return true;
  else
    {
      ppl_Polyhedron_t st1 = PBB_TRANSFORMED_SCATTERING (pbb1);
      ppl_Polyhedron_t st2 = PBB_TRANSFORMED_SCATTERING (pbb2);
      ppl_Pointset_Powerset_C_Polyhedron_t pt;
      graphite_dim_t ddim1 = pbb_dim_iter_domain (pbb1);
      graphite_dim_t otdim1 = pbb_nb_scattering_orig (pbb1);
      graphite_dim_t otdim2 = pbb_nb_scattering_orig (pbb2);
      graphite_dim_t ttdim1 = pbb_nb_scattering_transform (pbb1);
      graphite_dim_t ttdim2 = pbb_nb_scattering_transform (pbb2);

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "\nloop carries dependency.\n");
      pt = dependence_polyhedron (pbb1, pbb2, d1, d2, pdr1, pdr2, st1, st2,
				  false, false);

      /* Extend PO and PT to have the same dimensions.  */
      ppl_insert_dimensions_pointset (po, otdim1, ttdim1);
      ppl_insert_dimensions_pointset (po, otdim1 + ttdim1 + ddim1 + otdim2,
				      ttdim2);
      ppl_insert_dimensions_pointset (pt, 0, otdim1);
      ppl_insert_dimensions_pointset (pt, otdim1 + ttdim1 + ddim1, otdim2);

      ppl_Pointset_Powerset_C_Polyhedron_intersection_assign (po, pt);
      return ppl_Pointset_Powerset_C_Polyhedron_is_empty (po);
    }
}

/* Iterates over the data references of PBB1 and PBB2 and detect
   whether the transformed schedule is correct.  */

static bool
graphite_legal_transform_bb (poly_bb_p pbb1, poly_bb_p pbb2)
{
  int i, j;
  poly_dr_p pdr1, pdr2;

  for (i = 0; VEC_iterate (poly_dr_p, PBB_DRS (pbb1), i, pdr1); i++)
    for (j = 0; VEC_iterate (poly_dr_p, PBB_DRS (pbb2), j, pdr2); j++)
      if (!graphite_legal_transform_dr (pbb1, pbb2, pdr1, pdr2))
        return false;
  return true;
}

/* Iterates over the SCOP and detect whether the transformed schedule
   is correct.  */

bool
graphite_legal_transform (scop_p scop)
{
  int i, j;
  poly_bb_p pbb1, pbb2;

  for (i = 0; VEC_iterate (poly_bb_p, SCOP_BBS (scop), i, pbb1); i++)
    for (j = 0; VEC_iterate (poly_bb_p, SCOP_BBS (scop), j, pbb2); j++)
      if (!graphite_legal_transform_bb (pbb1, pbb2))
	return false;

  return true;
}

/* Remove all the dimensions except alias information at dimension
   ALIAS_DIM.  */

static void
build_alias_set_powerset (ppl_Pointset_Powerset_C_Polyhedron_t alias_powerset,
			  ppl_dimension_type alias_dim)
{
  ppl_dimension_type *ds;
  ppl_dimension_type access_dim;
  unsigned i, pos = 0;

  ppl_Pointset_Powerset_C_Polyhedron_space_dimension (alias_powerset,
						      &access_dim);
  ds = XNEWVEC (ppl_dimension_type, access_dim-1);
  for (i = 0; i < access_dim; i++)
    {
      if (i == alias_dim)
	continue;

      ds[pos] = i;
      pos++;
    }

  ppl_Pointset_Powerset_C_Polyhedron_remove_space_dimensions (alias_powerset,
							      ds,
							      access_dim - 1);
  free (ds);
}

/* Return true when PDR1 and PDR2 may alias.  */

static bool
poly_drs_may_alias_p (poly_dr_p pdr1, poly_dr_p pdr2)
{
  ppl_Pointset_Powerset_C_Polyhedron_t alias_powerset1, alias_powerset2;
  ppl_Pointset_Powerset_C_Polyhedron_t accesses1 = PDR_ACCESSES (pdr1);
  ppl_Pointset_Powerset_C_Polyhedron_t accesses2 = PDR_ACCESSES (pdr2);
  ppl_dimension_type alias_dim1 = pdr_alias_set_dim (pdr1);
  ppl_dimension_type alias_dim2 = pdr_alias_set_dim (pdr2);
  int empty_p;

  ppl_new_Pointset_Powerset_C_Polyhedron_from_Pointset_Powerset_C_Polyhedron
    (&alias_powerset1, accesses1);
  ppl_new_Pointset_Powerset_C_Polyhedron_from_Pointset_Powerset_C_Polyhedron
    (&alias_powerset2, accesses2);

  build_alias_set_powerset (alias_powerset1, alias_dim1);
  build_alias_set_powerset (alias_powerset2, alias_dim2);

  ppl_Pointset_Powerset_C_Polyhedron_intersection_assign
    (alias_powerset1, alias_powerset2);

  empty_p =  ppl_Pointset_Powerset_C_Polyhedron_is_empty (alias_powerset1);

  ppl_delete_Pointset_Powerset_C_Polyhedron (alias_powerset1);
  ppl_delete_Pointset_Powerset_C_Polyhedron (alias_powerset2);

  return !empty_p;
}

/* Returns TRUE when the dependence polyhedron between PDR1 and
   PDR2 represents a loop carried dependence at level LEVEL. Otherwise
   return FALSE.  */

static bool
graphite_carried_dependence_level_k (poly_dr_p pdr1, poly_dr_p pdr2,
				     int level)
{
  poly_bb_p pbb1 = PDR_PBB (pdr1);
  poly_bb_p pbb2 = PDR_PBB (pdr2);
  ppl_Pointset_Powerset_C_Polyhedron_t d1 = PBB_DOMAIN (pbb1);
  ppl_Pointset_Powerset_C_Polyhedron_t d2 = PBB_DOMAIN (pbb2);
  ppl_Polyhedron_t so1 = PBB_TRANSFORMED_SCATTERING (pbb1);
  ppl_Polyhedron_t so2 = PBB_TRANSFORMED_SCATTERING (pbb2);
  ppl_Pointset_Powerset_C_Polyhedron_t po;
  ppl_Pointset_Powerset_C_Polyhedron_t eqpp;
  graphite_dim_t sdim1 = pdr_nb_subscripts (pdr1) + 1;
  graphite_dim_t sdim2 = pdr_nb_subscripts (pdr2) + 1;
  graphite_dim_t tdim1 = pbb_nb_scattering_transform (pbb1);
  graphite_dim_t ddim1 = pbb_dim_iter_domain (pbb1);
  ppl_dimension_type dim;
  bool empty_p;

  if ((PDR_TYPE (pdr1) == PDR_READ && PDR_TYPE (pdr2) == PDR_READ)
      || !poly_drs_may_alias_p (pdr1, pdr2))
    return false;

  if (sdim1 != sdim2)
    return true;

  po = dependence_polyhedron (pbb1, pbb2, d1, d2, pdr1, pdr2, so1, so2,
			      true, false);
  if (ppl_Pointset_Powerset_C_Polyhedron_is_empty (po))
    {
      ppl_delete_Pointset_Powerset_C_Polyhedron (po);
      return false;
    }

  ppl_Pointset_Powerset_C_Polyhedron_space_dimension (po, &dim);
  eqpp = build_pairwise_scheduling_inequality (dim, level, tdim1 + ddim1, 1);

  ppl_Pointset_Powerset_C_Polyhedron_intersection_assign (eqpp, po);
  empty_p = ppl_Pointset_Powerset_C_Polyhedron_is_empty (eqpp);

  ppl_delete_Pointset_Powerset_C_Polyhedron (po);
  ppl_delete_Pointset_Powerset_C_Polyhedron (eqpp);
  return !empty_p;
}

/* Check data dependency between PBB1 and PBB2 at level LEVEL.  */

bool
dependency_between_pbbs_p (poly_bb_p pbb1, poly_bb_p pbb2, int level)
{
  int i, j;
  poly_dr_p pdr1, pdr2;

  for (i = 0; VEC_iterate (poly_dr_p, PBB_DRS (pbb1), i, pdr1); i++)
    for (j = 0; VEC_iterate (poly_dr_p, PBB_DRS (pbb2), j, pdr2); j++)
      if (graphite_carried_dependence_level_k (pdr1, pdr2, level))
	return true;

  return false;
}

#endif
