/* Null garbage collection for the GNU compiler.
   Copyright (C) 1998, 1999, 2000, 2003, 2004, 2005, 2007, 2010
   Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* This version is used by the gen* programs and certain language-specific
   targets (such as java), where we don't really need GC at all.
   This prevents problems with pulling in all the tree stuff.  */

#ifdef GENERATOR_FILE
#include "bconfig.h"
#else
#include "config.h"
#endif

#include "system.h"
#include "coretypes.h"
#include "ggc.h"

void *
ggc_alloc_typed_stat (enum gt_types_enum ARG_UNUSED (gte), size_t size
		      MEM_STAT_DECL)
{
  return xmalloc (size);
}

/* For a given size of memory requested for allocation, return the
   actual size that is going to be allocated.  */

size_t
ggc_round_alloc_size (size_t requested_size)
{
  return requested_size;
}

void *
ggc_internal_alloc_stat (size_t size MEM_STAT_DECL)
{
  return xmalloc (size);
}

void *
ggc_internal_cleared_alloc_stat (size_t size MEM_STAT_DECL)
{
  return xcalloc (size, 1);
}

void *
ggc_realloc_stat (void *x, size_t size MEM_STAT_DECL)
{
  return xrealloc (x, size);
}

void
ggc_free (void *p)
{
  free (p);
}

struct alloc_zone
{
  int dummy;
};

struct alloc_zone rtl_zone;
struct alloc_zone tree_zone;
struct alloc_zone tree_id_zone;

#if defined (GGC_ZONE) && !defined (GENERATOR_FILE)

void *
ggc_internal_alloc_zone_stat (size_t size,
                              struct alloc_zone * ARG_UNUSED(z) MEM_STAT_DECL)
{
    return xmalloc (size);
}

void *
ggc_internal_cleared_alloc_zone_stat (size_t size,
                                      struct alloc_zone * ARG_UNUSED(z)
                                      MEM_STAT_DECL)
{
    return xcalloc (size, 1);
}

#endif
