/* Header file for gimple-lower-bitint.cc exports.
   Copyright (C) 2023-2024 Free Software Foundation, Inc.

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

#ifndef GCC_GIMPLE_LOWER_BITINT_H
#define GCC_GIMPLE_LOWER_BITINT_H

class live_track;
struct ssa_conflicts;
extern void build_bitint_stmt_ssa_conflicts (gimple *, live_track *,
					     ssa_conflicts *, bitmap,
					     void (*) (live_track *, tree,
						       ssa_conflicts *),
					     void (*) (live_track *, tree));

#endif /* GCC_GIMPLE_LOWER_BITINT_H */
