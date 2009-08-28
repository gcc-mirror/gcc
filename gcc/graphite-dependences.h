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

enum poly_dependence_kind {
  unknown_dependence,
  no_dependence,
  has_dependence
};

typedef struct poly_ddr
{
  /* Source and sink data references of the dependence.  */
  poly_dr_p source, sink;

  /* Data dependence polyhedron.  */
  ppl_Pointset_Powerset_C_Polyhedron_t ddp;

  enum poly_dependence_kind kind;

} *poly_ddr_p;

#define PDDR_SOURCE(PDDR) (PDDR->source)
#define PDDR_SINK(PDDR) (PDDR->sink)
#define PDDR_DDP(PDDR) (PDDR->ddp)
#define PDDR_KIND(PDDR) (PDDR->kind)

extern int eq_poly_ddr_p (const void *, const void *);
extern hashval_t hash_poly_ddr_p (const void *);
extern void free_poly_ddr (void *);
extern void dot_deps (scop_p);

#endif
