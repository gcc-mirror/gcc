/* Header file for minimum-cost maximal flow routines used to smooth basic
   block and edge frequency counts.
   Copyright (C) 2008
   Free Software Foundation, Inc.
   Contributed by Paul Yuan (yingbo.com@gmail.com)
       and Vinodha Ramasamy (vinodha@google.com).

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

#ifndef PROFILE_H
#define PROFILE_H

/* Additional information about edges. */
struct edge_info
{
  unsigned int count_valid:1;

  /* Is on the spanning tree.  */
  unsigned int on_tree:1;

  /* Pretend this edge does not exist (it is abnormal and we've
     inserted a fake to compensate).  */
  unsigned int ignore:1;
};

#define EDGE_INFO(e)  ((struct edge_info *) (e)->aux)

/* Smoothes the initial assigned basic block and edge counts using
   a minimum cost flow algorithm. */
extern void mcf_smooth_cfg (void);

extern gcov_type sum_edge_counts (VEC (edge, gc) *edges);

#endif /* PROFILE_H */
