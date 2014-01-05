/* Define UPC pointer-to-shared representation-independent operations
   Copyright (C) 2008, 2009, 2010, 2011
   Free Software Foundation, Inc.
   Contributed by Gary Funck <gary@intrepid.com>
     and Nenad Vukicevic <nenad@intrepid.com>.

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

#ifndef GCC_C_FAMILY_UPC_PTS_OPS_H
#define GCC_C_FAMILY_UPC_PTS_OPS_H 1

typedef struct upc_pts_ops_struct
  {
    tree (*build) (location_t, tree, tree, tree, tree);
    tree (*cond_expr) (location_t, tree);
    tree (*constant) (location_t, tree);
    tree (*cvt) (location_t, tree);
    tree (*diff) (location_t, tree);
    void (*init) (void);
    int (*is_null_p) (tree);
    tree (*sum) (location_t, tree);
    tree (*threadof) (location_t, tree);
  } upc_pts_ops_t;

/* Export the representation-specific handlers.  */
extern upc_pts_ops_t upc_pts;

/* In c/c-upc-pts-packed.c */
extern const upc_pts_ops_t upc_pts_packed_ops;

/* In c/c-upc-pts-struct.c */
extern const upc_pts_ops_t upc_pts_struct_ops;

#endif /* !GCC_C_FAMILY_UPC_PTS_OPS_H */
