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

#ifndef GCC_SSA_RANGE_GLOBAL_H
#define GCC_SSA_RANGE_GLOBAL_H

#include "range.h"

// This global cache is used with the range engine as markers for what
// has been visited during this incarnation.  Once the ranger evaluates
// a name, it is not re-evaluated again.   
//
// An iterative ranger could detect that things have changed, and re-evaluate 
// a specific range
//
// When retreiving a global name, a check is first made to see if the 
// global irange cache has a range associated with it, and that is returned
// if it does.  If it does not, then any range assocaited with the
// existing SSA_NAME_RANGE_INFO field is extracted and that is returned,
// albeit with a false flagindicating there is not a global cache entry.

class ssa_global_cache
{
public:
  ssa_global_cache ();
  ~ssa_global_cache ();
  bool get_global_range (irange& r, tree name)  const;
  void set_global_range (tree name, const irange&r);
  void clear_global_range (tree name);
  void clear ();
  void dump (FILE *f = stderr);
private:
  vec<irange_storage *> m_tab;
};

#endif /* GCC_SSA_RANGE_GLOBAL_H  */
