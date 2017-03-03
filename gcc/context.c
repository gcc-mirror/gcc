/* context.c - Holder for global state
   Copyright (C) 2013-2017 Free Software Foundation, Inc.

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
#include "context.h"
#include "pass_manager.h"
#include "dumpfile.h"
#include "realmpfr.h"

/* The singleton holder of global state: */
gcc::context *g;

gcc::context::context ()
{
  have_offload = false;

  /* The pass manager's constructor uses the dump manager (to set up
     dumps for the various passes), so the dump manager must be set up
     before the pass manager.  */
  m_dumps = new gcc::dump_manager ();
  m_passes = new gcc::pass_manager (this);
}

gcc::context::~context ()
{
  delete m_passes;
  delete m_dumps;

  /* Release MPFR caches to avoid Valgrind leak reports.  */
  mpfr_free_cache ();
}
