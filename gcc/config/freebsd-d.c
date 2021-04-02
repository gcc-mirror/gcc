/* FreeBSD support needed only by D front-end.
   Copyright (C) 2020-2021 Free Software Foundation, Inc.

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
#include "tm.h"
#include "tm_p.h"
#include "d/d-target.h"
#include "d/d-target-def.h"

/* Implement TARGET_D_OS_VERSIONS for FreeBSD targets.  */

static void
freebsd_d_os_builtins (void)
{
  char buf[16];
  snprintf (buf, sizeof (buf), "FreeBSD_%d", FBSD_MAJOR);

  d_add_builtin_version ("FreeBSD");
  d_add_builtin_version (xstrdup (buf));
  d_add_builtin_version ("Posix");
}

#undef TARGET_D_OS_VERSIONS
#define TARGET_D_OS_VERSIONS freebsd_d_os_builtins

struct gcc_targetdm targetdm = TARGETDM_INITIALIZER;
