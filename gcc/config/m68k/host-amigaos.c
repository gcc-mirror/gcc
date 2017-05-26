/* AmigaOS/m68k host-specific hook definitions.
   Copyright (C) 2003 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "hosthooks.h"
#include "hosthooks-def.h"
#include "toplev.h"

static void * amigaos_m68k_gt_pch_get_address (size_t);

/* Return the address of the PCH address space, if the PCH will fit in it.  */

static void *
amigaos_m68k_gt_pch_get_address (size_t sz ATTRIBUTE_UNUSED)
{
  fatal_error ("PCH not supported\n");
}

#undef HOST_HOOKS_GT_PCH_GET_ADDRESS
#define HOST_HOOKS_GT_PCH_GET_ADDRESS amigaos_m68k_gt_pch_get_address

const struct host_hooks host_hooks = HOST_HOOKS_INITIALIZER; 