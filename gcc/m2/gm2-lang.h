/* Language-dependent hooks for GNU Modula-2.
   Copyright (C) 2003-2024 Free Software Foundation, Inc.
   Contributed by Gaius Mulley <gaius@glam.ac.uk>

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#if !defined(GM2_LANG_H)
#  define GM2_LANG_H

#if defined(GM2_LANG_C)
#  define EXTERN
#else
#  define EXTERN extern
#endif
#include "config.h"
#include "system.h"
#include "ansidecl.h"
#include "coretypes.h"
#include "opts.h"
#include "tree.h"
#include "gimple.h"


EXTERN enum gimplify_status  gm2_gimplify_expr (tree *, tree *, tree *);
EXTERN bool gm2_mark_addressable (tree);
EXTERN tree gm2_type_for_size             (unsigned int bits, int unsignedp);
EXTERN tree gm2_type_for_mode             (enum machine_mode mode, int unsignedp);
EXTERN bool gm2_langhook_init (void);
EXTERN bool gm2_langhook_handle_option (size_t scode, const char *arg,
					int value,
					int kind ATTRIBUTE_UNUSED,
					location_t loc ATTRIBUTE_UNUSED,
					const struct cl_option_handlers *handlers ATTRIBUTE_UNUSED);
EXTERN void gm2_langhook_init_options (unsigned int decoded_options_count,
					   struct cl_decoded_option *decoded_options);
EXTERN void gm2_genericize (tree fndecl);
EXTERN tree convert_loc (location_t location, tree type, tree expr);


#undef EXTERN
#endif
