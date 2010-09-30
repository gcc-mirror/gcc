/* Gimple Represented as Polyhedra.
   Copyright (C) 2009, 2010 Free Software Foundation, Inc.
   Contributed by Sebastian Pop <sebastian.pop@inria.fr>
   and Tobias Grosser <grosser@fim.uni-passau.de>.

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
#ifndef GRAPHITE_CLOOG_UTIL_H
#define GRAPHITE_CLOOG_UTIL_H

#include "cloog/cloog.h"
#include "graphite-cloog-compat.h"

CloogMatrix *new_Cloog_Matrix_from_ppl_Polyhedron (ppl_const_Polyhedron_t);
CloogDomain *new_Cloog_Domain_from_ppl_Polyhedron (ppl_const_Polyhedron_t,
						   int, CloogState *);
CloogScattering *new_Cloog_Scattering_from_ppl_Polyhedron
  (ppl_const_Polyhedron_t, int, int, CloogState *);
CloogDomain * new_Cloog_Domain_from_ppl_Pointset_Powerset
  (ppl_Pointset_Powerset_C_Polyhedron_t, int, CloogState *);
void new_C_Polyhedron_from_Cloog_Matrix (ppl_Polyhedron_t *, CloogMatrix *);
void openscop_print_polyhedron_matrix (FILE *, ppl_const_Polyhedron_t, int,
				       int, int, int);
void openscop_read_polyhedron_matrix (FILE *, ppl_Polyhedron_t *, int *, int *,
	       			     int *, int *);

extern int *openscop_read_N_int (FILE *, int);

#endif /* GRAPHITE_CLOOG_UTIL_H */
