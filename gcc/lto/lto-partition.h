/* LTO partitioning logic routines.
   Copyright 2009, 2010, 2011, 2012 Free Software Foundation, Inc.

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


/* Structure describing ltrans partitions.  */

struct ltrans_partition_def
{
  cgraph_node_set cgraph_set;
  varpool_node_set varpool_set;
  const char * name;
  int insns;
};

typedef struct ltrans_partition_def *ltrans_partition;
DEF_VEC_P(ltrans_partition);
DEF_VEC_ALLOC_P(ltrans_partition,heap);

extern VEC(ltrans_partition, heap) *ltrans_partitions;

void lto_1_to_1_map (void);
void lto_balanced_map (void);
void lto_promote_cross_file_statics (void);
void free_ltrans_partitions (void);
