/* Find single-entry, single-exit regions for OpenACC.

   Copyright (C) 2005-2017 Free Software Foundation, Inc.

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

#ifndef GCC_OMP_SESE_H
#define GCC_OMP_SESE_H

/* A pair of BBs.  We use this to represent SESE regions.  */
typedef std::pair<basic_block, basic_block> bb_pair_t;
typedef auto_vec<bb_pair_t> bb_pair_vec_t;

extern void omp_find_sese (auto_vec<basic_block> &blocks,
			   bb_pair_vec_t &regions);
extern void oacc_do_neutering (void);

#endif
