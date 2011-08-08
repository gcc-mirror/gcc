/* Streamer hooks.  Support for adding streamer-specific callbacks to
   generic streaming routines.

   Copyright 2011 Free Software Foundation, Inc.
   Contributed by Diego Novillo <dnovillo@google.com>

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
#include "streamer-hooks.h"

/* Streamer hooks.  */
struct streamer_hooks streamer_hooks;

/* Initialize the current set of streamer hooks.  */

void
streamer_hooks_init (void)
{
  memset (&streamer_hooks, 0, sizeof (streamer_hooks));
}
