/* Language-dependent hooks for Objective-C.
   Copyright 2001 Free Software Foundation, Inc.
   Contributed by Ziemowit Laski  <zlaski@apple.com>

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "tree.h"
#include "c-tree.h"
#include "c-common.h"
#include "toplev.h"
#include "objc-act.h"
#include "langhooks.h"
#include "langhooks-def.h"

static void objc_init_options                   PARAMS ((void));
static void objc_post_options                   PARAMS ((void));

#undef LANG_HOOKS_NAME
#define LANG_HOOKS_NAME "GNU Objective-C"  
#undef LANG_HOOKS_INIT
#define LANG_HOOKS_INIT objc_init
#undef LANG_HOOKS_FINISH
#define LANG_HOOKS_FINISH c_common_finish
#undef LANG_HOOKS_INIT_OPTIONS
#define LANG_HOOKS_INIT_OPTIONS objc_init_options
#undef LANG_HOOKS_DECODE_OPTION
#define LANG_HOOKS_DECODE_OPTION objc_decode_option
#undef LANG_HOOKS_POST_OPTIONS
#define LANG_HOOKS_POST_OPTIONS objc_post_options
#undef LANG_HOOKS_STATICP
#define LANG_HOOKS_STATICP c_staticp
#undef LANG_HOOKS_PRINT_IDENTIFIER
#define LANG_HOOKS_PRINT_IDENTIFIER c_print_identifier
#undef LANG_HOOKS_SET_YYDEBUG
#define LANG_HOOKS_SET_YYDEBUG c_set_yydebug
/* Inlining hooks same as the C front end.  */
#undef LANG_HOOKS_TREE_INLINING_CANNOT_INLINE_TREE_FN
#define LANG_HOOKS_TREE_INLINING_CANNOT_INLINE_TREE_FN \
  c_cannot_inline_tree_fn
#undef LANG_HOOKS_TREE_INLINING_DISREGARD_INLINE_LIMITS
#define LANG_HOOKS_TREE_INLINING_DISREGARD_INLINE_LIMITS \
  c_disregard_inline_limits
#undef LANG_HOOKS_TREE_INLINING_ANON_AGGR_TYPE_P
#define LANG_HOOKS_TREE_INLINING_ANON_AGGR_TYPE_P \
  anon_aggr_type_p

/* Each front end provides its own hooks, for toplev.c.  */
const struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

static void 
objc_init_options ()
{
  c_common_init_options (clk_objective_c);
} 

/* Post-switch processing.  */

static void
objc_post_options ()
{
  c_common_post_options ();
}


