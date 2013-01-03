/* Data dependence analysis for Graphite.
   Copyright (C) 2009, 2010, 2013 Free Software Foundation, Inc.
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
#include "tree-flow.h"
#include "tree-dump.h"
#include "cfgloop.h"
#include "tree-chrec.h"
#include "tree-data-ref.h"
#include "tree-scalar-evolution.h"
#include "sese.h"

#ifdef HAVE_cloog
#include "ppl_c.h"
#include "graphite-ppl.h"
#include "graphite-poly.h"
#include "graphite-dependences.h"
#include "graphite-cloog-util.h"

/* Comparison function for poly_ddr hash table.  */

int
eq_poly_ddr_p (const void *pddr1, const void *pddr2)
{
  const struct poly_ddr *p1 = (const struct poly_ddr *) pddr1;
  const struct poly_ddr *p2 = (const struct poly_ddr *) pddr2;

  return (PDDR_SOURCE (p1) == PDDR_SOURCE (p2)
          && PDDR_SINK (p1) == PDDR_SINK (p2));
}

/* Hash function for poly_ddr hashtable.  */

hashval_t
hash_poly_ddr_p (const void *pddr)
{
  const struct poly_ddr *p = (const struct poly_ddr *) pddr;

  return (hashval_t) ((intptr_t) PDDR_SOURCE (p) + (intptr_t) PDDR_SINK (p));
}

/* Returns true when PDDR has no dependence.  */

static bool
pddr_is_empty (poly_ddr_p pddr)
{
  if (!pddr)
    return true;

  gcc_assert (PDDR_KIND (pddr) != unknown_dependence);

  return PDDR_KIND (pddr) == no_dependence ? true : false;
}

/* Prints to FILE the layout of the dependence polyhedron of PDDR:

   T1|I1|T2|I2|S1|S2|G

   with
   | T1 and T2 the scattering dimensions for PDDR_SOURCE and PDDR_SINK
   | I1 and I2 the iteration domains
   | S1 and S2 the subscripts
   | G the global parameters.  */

static void
print_dependence_polyhedron_layout (FILE *file, poly_ddr_p pddr)
{
  poly_dr_p pdr1 = PDDR_SOURCE (pddr);
  poly_dr_p pdr2 = PDDR_SINK (pddr);
  poly_bb_p pbb1 = PDR_PBB (pdr1);
  poly_bb_p pbb2 = PDR_PBB (pdr2);

  graphite_dim_t i;
  graphite_dim_t tdim1 = PDDR_ORIGINAL_SCATTERING_P (pddr) ?
    pbb_nb_scattering_orig (pbb1) : pbb_nb_scattering_transform (pbb1);
  graphite_dim_t tdim2 = PDDR_ORIGINAL_SCATTERING_P (pddr) ?
    pbb_nb_scattering_orig (pbb2) : pbb_nb_scattering_transform (pbb2);
  graphite_dim_t idim1 = pbb_dim_iter_domain (pbb1);
  graphite_dim_t idim2 = pbb_dim_iter_domain (pbb2);
  graphite_dim_t sdim1 = PDR_NB_SUBSCRIPTS (pdr1) + 1;
  graphite_dim_t sdim2 = PDR_NB_SUBSCRIPTS (pdr2) + 1;
  graphite_dim_t gdim = scop_nb_params (PBB_SCOP (pbb1));

  fprintf (file, "#  eq");

  for (i = 0; i < tdim1; i++)
    fprintf (file, "   t1_%d", (int) i);
  for (i = 0; i < idim1; i++)
    fprintf (file, "   i1_%d", (int) i);
  for (i = 0; i < tdim2; i++)
    fprintf (file, "   t2_%d", (int) i);
  for (i = 0; i < idim2; i++)
    fprintf (file, "   i2_%d", (int) i);
  for (i = 0; i < sdim1; i++)
    fprintf (file, "   s1_%d", (int) i);
  for (i = 0; i < sdim2; i++)
    fprintf (file, "   s2_%d", (int) i);
  for (i = 0; i < gdim; i++)
    fprintf (file, "    g_%d", (int) i);

  fprintf (file, "    cst\n");
}

/* Prints to FILE the poly_ddr_p PDDR.  */

void
print_pddr (FILE *file, poly_ddr_p pddr)
{
  fprintf (file, "pddr (kind: ");

  if (PDDR_KIND (pddr) == unknown_dependence)
    fprintf (file, "unknown_dependence");
  else if (PDDR_KIND (pddr) == no_dependence)
    fprintf (file, "no_dependence");
  else if (PDDR_KIND (pddr) == has_dependence)
    fprintf (file, "has_dependence");

  fprintf (file, "\n  source ");
  print_pdr (file, PDDR_SOURCE (pddr), 2);

  fprintf (file, "\n  sink ");
  print_pdr (file, PDDR_SINK (pddr), 2);

  if (PDDR_KIND (pddr) == has_dependence)
    {
      fprintf (file, "\n  dependence polyhedron (\n");
      print_dependence_polyhedron_layout (file, pddr);
      ppl_print_powerset_matrix (file, PDDR_DDP (pddr));
      ppl_io_fprint_Pointset_Powerset_C_Polyhedron (file, PDDR_DDP (pddr));
      fprintf (file, ")\n");
    }

  fprintf (file, ")\n");
}

/* Prints to STDERR the poly_ddr_p PDDR.  */

DEBUG_FUNCTION void
debug_pddr (poly_ddr_p pddr)
{
  print_pddr (stderr, pddr);
}


/* Remove all the dimensions except alias information at dimension
   ALIAS_DIM.  */

static void
build_alias_set_powerset (ppl_Pointset_Powerset_C_Polyhedron_t alias_powerset,
			  ppl_dimension_type alias_dim)
{
  ppl_dimension_type *ds;
  ppl_dimension_type access_dim;
  unsigned i, pos;

  ppl_Pointset_Powerset_C_Polyhedron_space_dimension (alias_powerset,
						      &access_dim);
  ds = XNEWVEC (ppl_dimension_type, access_dim - 1);
  gcc_assert (alias_dim < access_dim);

  for (pos = 0, i = 0; i < access_dim; i++)
    if (i != alias_dim)
      ds[pos++] = i;

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

  empty_p = ppl_Pointset_Powerset_C_Polyhedron_is_empty (alias_powerset1);

  ppl_delete_Pointset_Powerset_C_Polyhedron (alias_powerset1);
  ppl_delete_Pointset_Powerset_C_Polyhedron (alias_powerset2);

  return !empty_p;
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

/* Builds subscript equality constraints.  */

static ppl_Pointset_Powerset_C_Polyhedron_t
dr_equality_constraints (graphite_dim_t dim,
		         graphite_dim_t pos, graphite_dim_t nb_subscripts)
{
  ppl_Polyhedron_t eqs;
  ppl_Pointset_Powerset_C_Polyhedron_t res;
  graphite_dim_t i;

  ppl_new_C_Polyhedron_from_space_dimension (&eqs, dim, 0);

  for (i = 0; i < nb_subscripts; i++)
    {
      ppl_Constraint_t cstr
	= ppl_build_relation (dim, pos + i, pos + i + nb_subscripts,
			      0, PPL_CONSTRAINT_TYPE_EQUAL);
      ppl_Polyhedron_add_constraint (eqs, cstr);
      ppl_delete_Constraint (cstr);
    }

  ppl_new_Pointset_Powerset_C_Polyhedron_from_C_Polyhedron (&res, eqs);
  ppl_delete_Polyhedron (eqs);
  return res;
}

/* Builds scheduling inequality constraints: when DIRECTION is
   1 builds a GE constraint,
   0 builds an EQ constraint,
   -1 builds a LE constraint.
   DIM is the dimension of the scheduling space.
   POS and POS + OFFSET are the dimensions that are related.  */

static ppl_Pointset_Powerset_C_Polyhedron_t
build_pairwise_scheduling (graphite_dim_t dim,
			   graphite_dim_t pos,
			   graphite_dim_t offset,
			   int direction)
{
  ppl_Pointset_Powerset_C_Polyhedron_t res;
  ppl_Polyhedron_t equalities;
  ppl_Constraint_t cstr;
  graphite_dim_t a = pos;
  graphite_dim_t b = pos + offset;

  ppl_new_C_Polyhedron_from_space_dimension (&equalities, dim, 0);

  switch (direction)
    {
    case 1:
      /* Builds "a + 1 <= b.  */
      cstr = ppl_build_relation (dim, a, b, 1,
				 PPL_CONSTRAINT_TYPE_LESS_OR_EQUAL);
      break;

    case 0:
      /* Builds "a = b.  */
      cstr = ppl_build_relation (dim, a, b, 0,
				 PPL_CONSTRAINT_TYPE_EQUAL);
      break;

    case -1:
      /* Builds "a >= b + 1.  */
      cstr = ppl_build_relation (dim, a, b, -1,
				 PPL_CONSTRAINT_TYPE_GREATER_OR_EQUAL);
      break;

    default:
      gcc_unreachable ();
    }

  ppl_Polyhedron_add_constraint (equalities, cstr);
  ppl_delete_Constraint (cstr);

  ppl_new_Pointset_Powerset_C_Polyhedron_from_C_Polyhedron (&res, equalities);
  ppl_delete_Polyhedron (equalities);
  return res;
}

/* Add to a non empty polyhedron BAG the precedence constraints for
   the lexicographical comparison of time vectors in BAG following the
   lexicographical order.  DIM is the dimension of the polyhedron BAG.
   TDIM is the number of loops common to the two statements that are
   compared lexicographically, i.e. the number of loops containing
   both statements.  OFFSET is the number of dimensions needed to
   represent the first statement, i.e. dimT1 + dimI1 in the layout of
   the BAG polyhedron: T1|I1|T2|I2|S1|S2|G.  When DIRECTION is set to
   1, compute the direct dependence from PDR1 to PDR2, and when
   DIRECTION is -1, compute the reversed dependence relation, from
   PDR2 to PDR1.  */

static ppl_Pointset_Powerset_C_Polyhedron_t
build_lexicographical_constraint (ppl_Pointset_Powerset_C_Polyhedron_t bag,
				  graphite_dim_t dim,
				  graphite_dim_t tdim,
				  graphite_dim_t offset,
				  int direction)
{
  graphite_dim_t i;
  ppl_Pointset_Powerset_C_Polyhedron_t res, lex;

  ppl_new_Pointset_Powerset_C_Polyhedron_from_space_dimension (&res, dim, 1);

  lex = build_pairwise_scheduling (dim, 0, offset, direction);
  ppl_Pointset_Powerset_C_Polyhedron_intersection_assign (lex, bag);

  if (!ppl_powerset_is_empty (lex))
    ppl_Pointset_Powerset_C_Polyhedron_upper_bound_assign (res, lex);

  ppl_delete_Pointset_Powerset_C_Polyhedron (lex);

  for (i = 0; i < tdim - 1; i++)
    {
      ppl_Pointset_Powerset_C_Polyhedron_t sceq;

      sceq = build_pairwise_scheduling (dim, i, offset, 0);
      ppl_Pointset_Powerset_C_Polyhedron_intersection_assign (bag, sceq);
      ppl_delete_Pointset_Powerset_C_Polyhedron (sceq);

      if (ppl_powerset_is_empty (bag))
	break;

      lex = build_pairwise_scheduling (dim, i + 1, offset, direction);
      ppl_Pointset_Powerset_C_Polyhedron_intersection_assign (lex, bag);

      if (!ppl_powerset_is_empty (lex))
	ppl_Pointset_Powerset_C_Polyhedron_upper_bound_assign (res, lex);

      ppl_delete_Pointset_Powerset_C_Polyhedron (lex);
    }

  return res;
}

/* Build the dependence polyhedron for data references PDR1 and PDR2.
   The layout of the dependence polyhedron is:

   T1|I1|T2|I2|S1|S2|G

   with
   | T1 and T2 the scattering dimensions for PDR1 and PDR2
   | I1 and I2 the iteration domains
   | S1 and S2 the subscripts
   | G the global parameters.

   When DIRECTION is set to 1, compute the direct dependence from PDR1
   to PDR2, and when DIRECTION is -1, compute the reversed dependence
   relation, from PDR2 to PDR1.  */

static ppl_Pointset_Powerset_C_Polyhedron_t
dependence_polyhedron (poly_dr_p pdr1, poly_dr_p pdr2,
		       int direction, bool original_scattering_p)
{
  poly_bb_p pbb1 = PDR_PBB (pdr1);
  poly_bb_p pbb2 = PDR_PBB (pdr2);
  scop_p scop = PBB_SCOP (pbb1);
  graphite_dim_t tdim1 = original_scattering_p ?
    pbb_nb_scattering_orig (pbb1) : pbb_nb_scattering_transform (pbb1);
  graphite_dim_t tdim2 = original_scattering_p ?
    pbb_nb_scattering_orig (pbb2) : pbb_nb_scattering_transform (pbb2);
  graphite_dim_t ddim1 = pbb_dim_iter_domain (pbb1);
  graphite_dim_t ddim2 = pbb_dim_iter_domain (pbb2);
  graphite_dim_t sdim1 = PDR_NB_SUBSCRIPTS (pdr1) + 1;
  graphite_dim_t sdim2 = PDR_NB_SUBSCRIPTS (pdr2) + 1;
  graphite_dim_t gdim = scop_nb_params (scop);
  graphite_dim_t dim1 = pdr_dim (pdr1);
  graphite_dim_t dim2 = pdr_dim (pdr2);
  graphite_dim_t dim = tdim1 + tdim2 + dim1 + dim2 - gdim;
  ppl_Pointset_Powerset_C_Polyhedron_t res;
  ppl_Pointset_Powerset_C_Polyhedron_t idr1, idr2;
  ppl_Pointset_Powerset_C_Polyhedron_t sc1, sc2, dreq;
  ppl_Pointset_Powerset_C_Polyhedron_t lex;

  gcc_assert (PBB_SCOP (pbb1) == PBB_SCOP (pbb2));

  combine_context_id_scat (&sc1, pbb1, original_scattering_p);
  combine_context_id_scat (&sc2, pbb2, original_scattering_p);

  ppl_insert_dimensions_pointset (sc1, tdim1 + ddim1,
				  tdim2 + ddim2 + sdim1 + sdim2);

  ppl_insert_dimensions_pointset (sc2, 0, tdim1 + ddim1);
  ppl_insert_dimensions_pointset (sc2, tdim1 + ddim1 + tdim2 + ddim2,
				  sdim1 + sdim2);

  idr1 = map_dr_into_dep_poly (dim, PDR_ACCESSES (pdr1), ddim1, ddim1 + gdim,
			       tdim1, tdim2 + ddim2);
  idr2 = map_dr_into_dep_poly (dim, PDR_ACCESSES (pdr2), ddim2, ddim2 + gdim,
			       tdim1 + ddim1 + tdim2, sdim1);

  /* Now add the subscript equalities.  */
  dreq = dr_equality_constraints (dim, tdim1 + ddim1 + tdim2 + ddim2, sdim1);

  ppl_new_Pointset_Powerset_C_Polyhedron_from_space_dimension (&res, dim, 0);
  ppl_Pointset_Powerset_C_Polyhedron_intersection_assign (res, sc1);
  ppl_Pointset_Powerset_C_Polyhedron_intersection_assign (res, sc2);
  ppl_Pointset_Powerset_C_Polyhedron_intersection_assign (res, idr1);
  ppl_Pointset_Powerset_C_Polyhedron_intersection_assign (res, idr2);
  ppl_Pointset_Powerset_C_Polyhedron_intersection_assign (res, dreq);
  ppl_delete_Pointset_Powerset_C_Polyhedron (sc1);
  ppl_delete_Pointset_Powerset_C_Polyhedron (sc2);
  ppl_delete_Pointset_Powerset_C_Polyhedron (idr1);
  ppl_delete_Pointset_Powerset_C_Polyhedron (idr2);
  ppl_delete_Pointset_Powerset_C_Polyhedron (dreq);

  if (ppl_powerset_is_empty (res))
    return NULL;

  lex = build_lexicographical_constraint (res, dim, MIN (tdim1, tdim2),
					  tdim1 + ddim1, direction);
  ppl_delete_Pointset_Powerset_C_Polyhedron (res);

  return lex;
}

/* Build the dependence polyhedron for data references PDR1 and PDR2.
   If possible use already cached information.

   When DIRECTION is set to 1, compute the direct dependence from PDR1
   to PDR2, and when DIRECTION is -1, compute the reversed dependence
   relation, from PDR2 to PDR1.  */

static poly_ddr_p
new_poly_ddr (poly_dr_p pdr1, poly_dr_p pdr2,
	      int direction, bool original_scattering_p)
{
  PTR *x = NULL;
  poly_ddr_p res;
  bool may_alias;

  /* Return the PDDR from the cache if it already has been computed.  */
  if (original_scattering_p)
    {
      struct poly_ddr tmp;
      scop_p scop = PBB_SCOP (PDR_PBB (pdr1));

      tmp.source = pdr1;
      tmp.sink = pdr2;
      x = htab_find_slot (SCOP_ORIGINAL_PDDRS (scop),
                          &tmp, INSERT);

      if (x && *x)
	return (poly_ddr_p) *x;
    }

  res = XNEW (struct poly_ddr);
  PDDR_SOURCE (res) = pdr1;
  PDDR_SINK (res) = pdr2;
  PDDR_DDP (res) = NULL;
  PDDR_ORIGINAL_SCATTERING_P (res) = original_scattering_p;
  PDDR_KIND (res) = unknown_dependence;

  may_alias = poly_drs_may_alias_p (pdr1, pdr2);

  if (!(pdr_read_p (pdr1) && pdr_read_p (pdr2))
      && PDR_BASE_OBJECT_SET (pdr1) != PDR_BASE_OBJECT_SET (pdr2)
      && may_alias)
    PDDR_KIND (res) = unknown_dependence;

  else if (!(pdr_read_p (pdr1) && pdr_read_p (pdr2))
	   && same_pdr_p (pdr1, pdr2)
	   && may_alias)
    {
      PDDR_DDP (res) = dependence_polyhedron (pdr1, pdr2, direction,
					      original_scattering_p);
      if (PDDR_DDP (res))
	PDDR_KIND (res) = has_dependence;
      else
	PDDR_KIND (res) = no_dependence;
    }
  else
    PDDR_KIND (res) = no_dependence;

  if (original_scattering_p)
    *x = res;

  return res;
}

/* Free the data dependence relation poly_ddr_p P.  */

void
free_poly_ddr (void *p)
{
  poly_ddr_p pddr = (poly_ddr_p) p;
  ppl_delete_Pointset_Powerset_C_Polyhedron (PDDR_DDP (pddr));
  free (pddr);
}

/* Return true when the data dependence relation between the data
   references PDR1 belonging to PBB1 and PDR2 is part of a
   reduction.  */

static inline bool
reduction_dr_1 (poly_bb_p pbb1, poly_dr_p pdr1, poly_dr_p pdr2)
{
  int i;
  poly_dr_p pdr;

  FOR_EACH_VEC_ELT (poly_dr_p, PBB_DRS (pbb1), i, pdr)
    if (PDR_TYPE (pdr) == PDR_WRITE
	&& same_pdr_p (pdr, pdr1) && same_pdr_p (pdr, pdr2))
      return true;

  return false;
}

/* Return true when the data dependence relation between the data
   references PDR1 belonging to PBB1 and PDR2 belonging to PBB2 is
   part of a reduction.  */

static inline bool
reduction_dr_p (poly_dr_p pdr1, poly_dr_p pdr2)
{
  poly_bb_p pbb1 = PDR_PBB (pdr1);
  poly_bb_p pbb2 = PDR_PBB (pdr2);

  if (PBB_IS_REDUCTION (pbb1))
    return reduction_dr_1 (pbb1, pdr1, pdr2);

  if (PBB_IS_REDUCTION (pbb2))
    return reduction_dr_1 (pbb2, pdr2, pdr1);

  return false;
}

/* Returns true when the PBB_TRANSFORMED_SCATTERING functions of PBB1
   and PBB2 respect the data dependences of PBB_ORIGINAL_SCATTERING
   functions.  */

static bool
graphite_legal_transform_dr (poly_dr_p pdr1, poly_dr_p pdr2)
{
  ppl_Pointset_Powerset_C_Polyhedron_t po, pt;
  graphite_dim_t ddim1, otdim1, otdim2, ttdim1, ttdim2;
  ppl_Pointset_Powerset_C_Polyhedron_t po_temp;
  ppl_dimension_type pdim;
  bool is_empty_p;
  poly_ddr_p opddr, tpddr;
  poly_bb_p pbb1, pbb2;

  if (reduction_dr_p (pdr1, pdr2))
    return true;

  /* We build the reverse dependence relation for the transformed
     scattering, such that when we intersect it with the original PO,
     we get an empty intersection when the transform is legal:
     i.e. the transform should reverse no dependences, and so PT, the
     reversed transformed PDDR, should have no constraint from PO.  */
  opddr = new_poly_ddr (pdr1, pdr2, 1, true);

  if (PDDR_KIND (opddr) == unknown_dependence)
    return false;

    /* There are no dependences between PDR1 and PDR2 in the original
       version of the program, or after the transform, so the
       transform is legal.  */
  if (pddr_is_empty (opddr))
    return true;

  tpddr = new_poly_ddr (pdr1, pdr2, -1, false);

  if (PDDR_KIND (tpddr) == unknown_dependence)
    {
      free_poly_ddr (tpddr);
      return false;
    }

  if (pddr_is_empty (tpddr))
    {
      free_poly_ddr (tpddr);
      return true;
    }

  po = PDDR_DDP (opddr);
  pt = PDDR_DDP (tpddr);

  /* Copy PO into PO_TEMP, such that PO is not destroyed.  PO is
     stored in a cache and should not be modified or freed.  */
  ppl_Pointset_Powerset_C_Polyhedron_space_dimension (po, &pdim);
  ppl_new_Pointset_Powerset_C_Polyhedron_from_space_dimension (&po_temp,
							       pdim, 0);
  ppl_Pointset_Powerset_C_Polyhedron_intersection_assign (po_temp, po);

  /* Extend PO and PT to have the same dimensions.  */
  pbb1 = PDR_PBB (pdr1);
  pbb2 = PDR_PBB (pdr2);
  ddim1 = pbb_dim_iter_domain (pbb1);
  otdim1 = pbb_nb_scattering_orig (pbb1);
  otdim2 = pbb_nb_scattering_orig (pbb2);
  ttdim1 = pbb_nb_scattering_transform (pbb1);
  ttdim2 = pbb_nb_scattering_transform (pbb2);
  ppl_insert_dimensions_pointset (po_temp, otdim1, ttdim1);
  ppl_insert_dimensions_pointset (po_temp, otdim1 + ttdim1 + ddim1 + otdim2,
				  ttdim2);
  ppl_insert_dimensions_pointset (pt, 0, otdim1);
  ppl_insert_dimensions_pointset (pt, otdim1 + ttdim1 + ddim1, otdim2);

  ppl_Pointset_Powerset_C_Polyhedron_intersection_assign (po_temp, pt);
  is_empty_p = ppl_powerset_is_empty (po_temp);

  ppl_delete_Pointset_Powerset_C_Polyhedron (po_temp);
  free_poly_ddr (tpddr);

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nloop carries dependency.\n");

  return is_empty_p;
}

/* Return true when the data dependence relation for PBB1 and PBB2 is
   part of a reduction.  */

static inline bool
reduction_ddr_p (poly_bb_p pbb1, poly_bb_p pbb2)
{
  return pbb1 == pbb2 && PBB_IS_REDUCTION (pbb1);
}

/* Iterates over the data references of PBB1 and PBB2 and detect
   whether the transformed schedule is correct.  */

static bool
graphite_legal_transform_bb (poly_bb_p pbb1, poly_bb_p pbb2)
{
  int i, j;
  poly_dr_p pdr1, pdr2;

  if (!PBB_PDR_DUPLICATES_REMOVED (pbb1))
    pbb_remove_duplicate_pdrs (pbb1);

  if (!PBB_PDR_DUPLICATES_REMOVED (pbb2))
    pbb_remove_duplicate_pdrs (pbb2);

  if (reduction_ddr_p (pbb1, pbb2))
    return true;

  FOR_EACH_VEC_ELT (poly_dr_p, PBB_DRS (pbb1), i, pdr1)
    FOR_EACH_VEC_ELT (poly_dr_p, PBB_DRS (pbb2), j, pdr2)
      if (!graphite_legal_transform_dr (pdr1, pdr2))
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

  timevar_push (TV_GRAPHITE_DATA_DEPS);

  FOR_EACH_VEC_ELT (poly_bb_p, SCOP_BBS (scop), i, pbb1)
    FOR_EACH_VEC_ELT (poly_bb_p, SCOP_BBS (scop), j, pbb2)
      if (!graphite_legal_transform_bb (pbb1, pbb2))
	{
	  timevar_pop (TV_GRAPHITE_DATA_DEPS);
	  return false;
	}

  timevar_pop (TV_GRAPHITE_DATA_DEPS);
  return true;
}

/* Returns TRUE when the dependence polyhedron between PDR1 and
   PDR2 represents a loop carried dependence at level LEVEL.  */

static bool
graphite_carried_dependence_level_k (poly_dr_p pdr1, poly_dr_p pdr2,
				     int level)
{
  ppl_Pointset_Powerset_C_Polyhedron_t po;
  ppl_Pointset_Powerset_C_Polyhedron_t eqpp;
  poly_bb_p pbb = PDR_PBB (pdr1);
  graphite_dim_t tdim1 = pbb_nb_scattering_transform (pbb);
  graphite_dim_t ddim1 = pbb_dim_iter_domain (pbb);
  ppl_dimension_type dim;
  bool empty_p;
  poly_ddr_p pddr = new_poly_ddr (pdr1, pdr2, 1, false);
  graphite_dim_t pos;

  if (PDDR_KIND (pddr) == unknown_dependence)
    {
      free_poly_ddr (pddr);
      return true;
    }

  if (pddr_is_empty (pddr))
    {
      free_poly_ddr (pddr);
      return false;
    }

  po = PDDR_DDP (pddr);
  ppl_Pointset_Powerset_C_Polyhedron_space_dimension (po, &dim);
  pos = psct_dynamic_dim (pbb, level);
  eqpp = build_pairwise_scheduling (dim, pos, tdim1 + ddim1, 1);

  ppl_Pointset_Powerset_C_Polyhedron_intersection_assign (eqpp, po);
  empty_p = ppl_powerset_is_empty (eqpp);

  ppl_delete_Pointset_Powerset_C_Polyhedron (eqpp);
  free_poly_ddr (pddr);

  return !empty_p;
}

/* Check data dependency between PBB1 and PBB2 at level LEVEL.  */

bool
dependency_between_pbbs_p (poly_bb_p pbb1, poly_bb_p pbb2, int level)
{
  int i, j;
  poly_dr_p pdr1, pdr2;

  timevar_push (TV_GRAPHITE_DATA_DEPS);

  FOR_EACH_VEC_ELT (poly_dr_p, PBB_DRS (pbb1), i, pdr1)
    FOR_EACH_VEC_ELT (poly_dr_p, PBB_DRS (pbb2), j, pdr2)
      if (graphite_carried_dependence_level_k (pdr1, pdr2, level))
	{
	  timevar_pop (TV_GRAPHITE_DATA_DEPS);
	  return true;
	}

  timevar_pop (TV_GRAPHITE_DATA_DEPS);
  return false;
}

/* When ORIG is true, pretty print to FILE all the original data
   dependences of SCoP in DOT format, otherwise print the transformed
   data deps.  */

static void
dot_deps_stmt_2 (FILE *file, scop_p scop, bool orig)
{
  int i, j, k, l;
  poly_bb_p pbb1, pbb2;
  poly_dr_p pdr1, pdr2;

  FOR_EACH_VEC_ELT (poly_bb_p, SCOP_BBS (scop), i, pbb1)
    FOR_EACH_VEC_ELT (poly_bb_p, SCOP_BBS (scop), j, pbb2)
      {
	FOR_EACH_VEC_ELT (poly_dr_p, PBB_DRS (pbb1), k, pdr1)
	  FOR_EACH_VEC_ELT (poly_dr_p, PBB_DRS (pbb2), l, pdr2)
	    {
	      poly_ddr_p pddr = new_poly_ddr (pdr1, pdr2, 1, orig);

	      if (!pddr_is_empty (pddr))
		{
		  fprintf (file, orig ? "OS%d -> OS%d\n" : "TS%d -> TS%d\n",
			   pbb_index (pbb1), pbb_index (pbb2));

		  free_poly_ddr (pddr);
		  goto done;
		}

	      free_poly_ddr (pddr);
	    }
      done:;
      }
}

/* Pretty print to FILE all the data dependences of SCoP in DOT
   format.  */

static void
dot_deps_stmt_1 (FILE *file, scop_p scop)
{
  fputs ("digraph all {\n", file);

  dot_deps_stmt_2 (file, scop, true);
  dot_deps_stmt_2 (file, scop, false);

  fputs ("}\n\n", file);
}

/* When ORIG is true, pretty print to FILE all the original data
   dependences of SCoP in DOT format, otherwise print the transformed
   data deps.  */

static void
dot_deps_2 (FILE *file, scop_p scop, bool orig)
{
  int i, j, k, l;
  poly_bb_p pbb1, pbb2;
  poly_dr_p pdr1, pdr2;

  FOR_EACH_VEC_ELT (poly_bb_p, SCOP_BBS (scop), i, pbb1)
    FOR_EACH_VEC_ELT (poly_bb_p, SCOP_BBS (scop), j, pbb2)
      FOR_EACH_VEC_ELT (poly_dr_p, PBB_DRS (pbb1), k, pdr1)
	FOR_EACH_VEC_ELT (poly_dr_p, PBB_DRS (pbb2), l, pdr2)
          {
	    poly_ddr_p pddr = new_poly_ddr (pdr1, pdr2, 1, orig);

	    if (!pddr_is_empty (pddr))
	      fprintf (file, orig
		       ? "OS%d_D%d -> OS%d_D%d\n" : "TS%d_D%d -> TS%d_D%d\n",
		       pbb_index (pbb1), PDR_ID (pdr1),
		       pbb_index (pbb2), PDR_ID (pdr2));

	    free_poly_ddr (pddr);
	  }
}

/* Pretty print to FILE all the data dependences of SCoP in DOT
   format.  */

static void
dot_deps_1 (FILE *file, scop_p scop)
{
  fputs ("digraph all {\n", file);

  dot_deps_2 (file, scop, true);
  dot_deps_2 (file, scop, false);

  fputs ("}\n\n", file);
}

/* Display all the data dependences in SCoP using dotty.  */

DEBUG_FUNCTION void
dot_deps (scop_p scop)
{
  /* When debugging, enable the following code.  This cannot be used
     in production compilers because it calls "system".  */
#if 0
  FILE *stream = fopen ("/tmp/scopdeps.dot", "w");
  gcc_assert (stream);

  dot_deps_1 (stream, scop);
  fclose (stream);

  system ("dotty /tmp/scopdeps.dot &");
#else
  dot_deps_1 (stderr, scop);
#endif
}

/* Display all the statement dependences in SCoP using dotty.  */

DEBUG_FUNCTION void
dot_deps_stmt (scop_p scop)
{
  /* When debugging, enable the following code.  This cannot be used
     in production compilers because it calls "system".  */
#if 0
  FILE *stream = fopen ("/tmp/scopdeps.dot", "w");
  gcc_assert (stream);

  dot_deps_stmt_1 (stream, scop);
  fclose (stream);

  system ("dotty /tmp/scopdeps.dot &");
#else
  dot_deps_stmt_1 (stderr, scop);
#endif
}

#endif
