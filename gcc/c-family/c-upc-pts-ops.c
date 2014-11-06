/* c-upc-pts-ops.c: implement UPC pointer-to-shared-operations.
   Copyright (C) 2001-2014 Free Software Foundation, Inc.
   Contributed by Gary Funck <gary@intrepid.com>
     and Nenad Vukicevic <nenad@intrepid.com>.
   Based on original implementation
     by Jesse M. Draper <jdraper@super.org>
     and William W. Carlson <wwc@super.org>.

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
#include "tree.h"
#include "c-upc-pts-ops.h"

/* UPC_PTS is a table of functions that implement various
   operations on expressions which refer to UPC pointers-to-shared,
   where their implementation varies with the representation
   of a pointer-to-shared value.  ('packed' or 'struct')  */

upc_pts_ops_t upc_pts;

/*  Initialize the handler table for the UPC pointer-to-shared
    representation that was selected when the compiler
    was configured.  */

void
upc_pts_init (void)
{
#if HAVE_UPC_PTS_PACKED_REP
    upc_pts = upc_pts_packed_ops;
#elif HAVE_UPC_PTS_STRUCT_REP
    upc_pts = upc_pts_struct_ops;
#else
#  error either HAVE_UPC_PTS_PACKED_REP or HAVE_UPC_PTS_STRUCT_REP must be defined.
#endif
  /* Define the various pre-defined types and values, like 'upc_shared_ptr_t'
     that depend upon the representation of UPC pointer-to-shared type.  */
  (*upc_pts.init) ();
}
