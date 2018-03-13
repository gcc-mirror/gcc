/* Global ssa ranges. 
   Copyright (C) 2018 Free Software Foundation, Inc.
   Contributed by Andrew MacLeod <amacleod@redhat.com>.

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

void initialize_global_ssa_range_cache ();
void destroy_global_ssa_range_cache ();
void dump_global_ssa_range_cache (FILE *f);

bool get_global_ssa_range (irange& r, tree name);
void set_global_ssa_range (tree name, const irange&r);
