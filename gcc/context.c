/* context.c - Holder for global state
   Copyright (C) 2013 Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "ggc.h"
#include "context.h"
#include "pass_manager.h"

/* The singleton holder of global state: */
gcc::context *g;

void *
gcc::context::operator new (std::size_t size)
{
  return ggc_internal_cleared_alloc_stat (size MEM_STAT_INFO);
}

gcc::context::context()
{
  passes_ = new gcc::pass_manager (this);
}

/* Functions relating to the garbage collector.  */
void
gcc::context::gt_ggc_mx ()
{
  ::gt_ggc_mx (passes_);
}

void
gcc::context::gt_pch_nx ()
{
  ::gt_pch_nx (passes_);
}

void
gcc::context::gt_pch_nx (gt_pointer_operator op ATTRIBUTE_UNUSED,
			 void *cookie ATTRIBUTE_UNUSED)
{
  op (&passes_, cookie);
}
