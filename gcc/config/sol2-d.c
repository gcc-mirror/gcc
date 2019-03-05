/* Solaris support needed only by D front-end.
   Copyright (C) 2018-2019 Free Software Foundation, Inc.

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
#include "memmodel.h"
#include "tm_p.h"
#include "d/d-target.h"
#include "d/d-target-def.h"

/* Implement TARGET_D_OS_VERSIONS for Solaris targets.  */

static void
solaris_d_os_builtins (void)
{
  d_add_builtin_version ("Posix");
  d_add_builtin_version ("Solaris");			\
}

/* Implement TARGET_D_CRITSEC_SIZE for Solaris targets.  */

static unsigned
solaris_d_critsec_size (void)
{
  /* This is the sizeof pthread_mutex_t.  */
  return 24;
}

#undef TARGET_D_OS_VERSIONS
#define TARGET_D_OS_VERSIONS solaris_d_os_builtins

#undef TARGET_D_CRITSEC_SIZE
#define TARGET_D_CRITSEC_SIZE solaris_d_critsec_size

struct gcc_targetdm targetdm = TARGETDM_INITIALIZER;
