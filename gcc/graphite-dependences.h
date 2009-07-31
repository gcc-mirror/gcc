/* Graphite polyhedral representation.
   Copyright (C) 2009 Free Software Foundation, Inc.
   Contributed by Konrad Trifunovic <konrad.trifunovic@gmail.com>

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

#ifndef GCC_GRAPHITE_DEPENDENCES_H
#define GCC_GRAPHITE_DEPENDENCES_H

extern bool graphite_legal_transform (scop_p);
extern bool dependency_between_pbbs_p (poly_bb_p, poly_bb_p, int);

typedef struct poly_dr_pair *poly_dr_pair_p;

typedef struct poly_dr_pair
{
  /* Source polyhedral data reference of the dependence.  */
  poly_dr_p source;

  /* Sink data reference of the dependence.  */
  poly_dr_p sink;

  /* Data dependence polyhedron descibing dependence
     between SOURCE and SINK data references.  */
  ppl_Pointset_Powerset_C_Polyhedron_t ddp;
}poly_dr_pair;


#define PDRP_SOURCE(PDRP) (PDR->source)
#define PDRP_SINK(PDRP) (PDR->sink)
#define PDRP_DDP(PDRP) (PDR->ddp)

extern int eq_poly_dr_pair_p (const void *, const void *);
extern hashval_t hash_poly_dr_pair_p (const void *);

#endif
