/* Header file for openMP lowering directives.
   Copyright (C) 2013-2014 Free Software Foundation, Inc.

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

#ifndef GCC_OMP_LOW_H
#define GCC_OMP_LOW_H

struct omp_region;

extern tree find_omp_clause (tree, enum omp_clause_code);
extern void omp_expand_local (basic_block);
extern void free_omp_regions (void);
extern tree omp_reduction_init (tree, tree);
extern bool make_gimple_omp_edges (basic_block, struct omp_region **);

#endif /* GCC_OMP_LOW_H */
