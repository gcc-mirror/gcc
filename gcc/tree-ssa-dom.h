/* Header file for SSA dominator optimizations.
   Copyright (C) 2013-2015 Free Software Foundation, Inc.

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

#ifndef GCC_TREE_SSA_DOM_H
#define GCC_TREE_SSA_DOM_H

extern void dump_dominator_optimization_stats (FILE *);
extern void debug_dominator_optimization_stats (void);
extern bool simple_iv_increment_p (gimple);

#endif /* GCC_TREE_SSA_DOM_H */
