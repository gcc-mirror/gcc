/* Default macros to initialize the lang_hooks data structure.
   Copyright 2003 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifndef GCC_HOST_HOOKS_DEF_H
#define GCC_HOST_HOOKS_DEF_H

#include "hooks.h"

#define HOST_HOOKS_EXTRA_SIGNALS hook_void_void
#if HAVE_MMAP_FILE
#define HOST_HOOKS_GT_PCH_GET_ADDRESS mmap_gt_pch_get_address
#define HOST_HOOKS_GT_PCH_USE_ADDRESS mmap_gt_pch_use_address
#else
#define HOST_HOOKS_GT_PCH_GET_ADDRESS default_gt_pch_get_address
#define HOST_HOOKS_GT_PCH_USE_ADDRESS default_gt_pch_use_address
#endif

extern void* default_gt_pch_get_address (size_t, int);
extern int default_gt_pch_use_address (void *, size_t, int, size_t);
extern void* mmap_gt_pch_get_address (size_t, int);
extern int mmap_gt_pch_use_address (void *, size_t, int, size_t);

/* The structure is defined in hosthooks.h.  */
#define HOST_HOOKS_INITIALIZER {		\
  HOST_HOOKS_EXTRA_SIGNALS,			\
  HOST_HOOKS_GT_PCH_GET_ADDRESS,		\
  HOST_HOOKS_GT_PCH_USE_ADDRESS			\
}

#endif /* GCC_HOST_HOOKS_DEF_H */
