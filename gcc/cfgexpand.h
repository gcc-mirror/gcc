/* Header file for lowering trees to RTL.
   Copyright (C) 2013-2025 Free Software Foundation, Inc.

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

#ifndef GCC_CFGEXPAND_H
#define GCC_CFGEXPAND_H

extern tree gimple_assign_rhs_to_tree (gimple *);
extern HOST_WIDE_INT estimated_stack_frame_size (struct cgraph_node *);
extern void expand_remove_edge (edge);
extern void set_parm_rtl (tree, rtx);


#endif /* GCC_CFGEXPAND_H */
