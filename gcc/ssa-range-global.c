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

/* This "global" cache is used with the range engine until such time
   that we can unify everything to the ranges that are contained in 
   the ssa_name itself.  There appears to be some issues with reading an
   writing that at the moment, and there ought to be a central place to
   read/write global ranges anyway, rather than direct SSA_NAME read/write.

   When retreiving a global name, a check is first made to see if the 
   global irange cache has a range associated with it, and that is returned
   if it does.  If it does not, then any range assocaited with the
   existing SSA_NAME_RANGE_INFO field is extracted and that is used.

   Any SETs of ranges are localized to the global cache maintained here.

   At the end of range generation/processing, a call is made to
   copy_to_range_info() to flush this cache into the SSA_NAME_RANGE_INFO
   fields.  */


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "insn-codes.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "ssa.h"
#include "optabs-tree.h"
#include "gimple-pretty-print.h"
#include "diagnostic-core.h"
#include "ssa-range-global.h"

class ssa_global_cache
{
private:
  vec<irange_storage *> tab;
public:
  ssa_global_cache ();
  ~ssa_global_cache ();
  bool get_global_range (irange& r, tree name)  const;
  void set_global_range (tree name, const irange&r);
  void clear ();
  void copy_to_range_info ();
  void dump (FILE *f = stderr);
};

ssa_global_cache *globals = NULL;

/* Initialize the global cache.  */
void
initialize_global_ssa_range_cache ()
{
  gcc_assert (globals == NULL);
  globals = new ssa_global_cache ();
}

/* Destroy the global cache. NOte we currently don't call copy_to_range_info 
   due to some memory corruptions issues.  */
void
destroy_global_ssa_range_cache ()
{
  delete globals;
  globals = NULL;
}

/* Dump the contents of the global cache to F.  */
void
dump_global_ssa_range_cache (FILE *f)
{
  if (globals)
    globals->dump (f);
}


/* This function will retreive what is currently globally known about SSA_NAME.
   It first tries the global_cache, then resorts to the SSA_NAME_RANGE_INFO.  */
bool
get_global_ssa_range (irange& r, tree name)
{
  if (globals)
    return globals->get_global_range (r, name);

  r.set_range (name);
  return false;
}

/* Set a new global range for NAME.  */
void
set_global_ssa_range (tree name, const irange&r)
{
  gcc_assert (globals);
  globals->set_global_range (name, r);
}

// ---------------------------------------------------------------

/* Initialize a global cache.  */
ssa_global_cache::ssa_global_cache ()
{
  tab.create (0);
  tab.safe_grow_cleared (num_ssa_names);
}

/* Deconstruct a global cache.  */
ssa_global_cache::~ssa_global_cache ()
{
  tab.release ();
}

/* Retrieve the global range of NAME from cache memory if it exists.  Return
   the value in R.  */
bool
ssa_global_cache::get_global_range (irange &r, tree name) const
{
  irange_storage *stow = tab[SSA_NAME_VERSION (name)];
  if (stow)
    {
      r.set_range (stow, TREE_TYPE (name));
      return true;
    }
  r.set_range (name);
  return false;
}

/* Set the range for NAME to R in the glonbal cache.  */
void
ssa_global_cache::set_global_range (tree name, const irange& r)
{
  irange_storage *m = tab[SSA_NAME_VERSION (name)];

  if (m)
    m->set_irange (r);
  else
    {
      m = irange_storage::ggc_alloc_init (r);
      tab[SSA_NAME_VERSION (name)] = m;
    }
}

/* Clear the global cache.  */
void
ssa_global_cache::clear ()
{
  memset (tab.address(), 0, tab.length () * sizeof (irange_storage *));
}

/* If there is any range information in the global cache, flush it out to
   the legacy SSA_NAME_RANGE_INFO fields.  */
void
ssa_global_cache::copy_to_range_info ()
{
  unsigned x;
  irange r;
  for ( x = 1; x < num_ssa_names; x++)
    if (get_global_range (r, ssa_name (x)))
      {
        if (!r.range_for_type_p ())
	  {
	    irange_storage *stow = irange_storage::ggc_alloc_init (r);
	    SSA_NAME_RANGE_INFO (ssa_name (x)) = stow;
	  }
      }
}

/* Dump the contents of the global cache to F.  */
void
ssa_global_cache::dump (FILE *f)
{
  unsigned x;
  irange r;
  for ( x = 1; x < num_ssa_names; x++)
    if (valid_irange_ssa (ssa_name (x)) && get_global_range (r, ssa_name (x)))
      {
        print_generic_expr (f, ssa_name (x), 0);
	fprintf (f, "  : ");
        r.dump (f);
      }
}


