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
#define HOST_HOOKS_GT_PCH_GET_ADDRESS hook_voidp_size_t_null
#define HOST_HOOKS_GT_PCH_USE_ADDRESS hook_bool_voidp_size_t_false

/* The structure is defined in hosthooks.h.  */
#define HOST_HOOKS_INITIALIZER {		\
  HOST_HOOKS_EXTRA_SIGNALS,			\
  HOST_HOOKS_GT_PCH_GET_ADDRESS,		\
  HOST_HOOKS_GT_PCH_USE_ADDRESS			\
}

#endif /* GCC_HOST_HOOKS_DEF_H */
