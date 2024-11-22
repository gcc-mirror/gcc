/* Language-dependent hooks for Objective-C++.
   Copyright (C) 2005-2024 Free Software Foundation, Inc.
   Contributed by Ziemowit Laski  <zlaski@apple.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "cp-tree.h"
#include "c-family/c-objc.h"
#include "objc-act.h"
#include "langhooks.h"
#include "langhooks-def.h"
#include "cp-objcp-common.h"

enum c_language_kind c_language = clk_objcxx;
static void objcxx_init_ts (void);

/* Lang hooks common to C++ and ObjC++ are declared in cp/cp-objcp-common.h;
   consequently, there should be very few hooks below.  */

#undef LANG_HOOKS_NAME
#define LANG_HOOKS_NAME "GNU Objective-C++"
#undef LANG_HOOKS_INIT
#define LANG_HOOKS_INIT objc_init
#undef LANG_HOOKS_GIMPLIFY_EXPR
#define LANG_HOOKS_GIMPLIFY_EXPR objc_gimplify_expr
#undef LANG_HOOKS_INIT_TS
#define LANG_HOOKS_INIT_TS objcxx_init_ts

/* Each front end provides its own lang hook initializer.  */
struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

/* Lang hook routines common to C++ and ObjC++ appear in cp/cp-objcp-common.cc;
   there should be very few (if any) routines below.  */

tree
objcp_tsubst_expr (tree t, tree args, tsubst_flags_t complain, tree in_decl)
{
#define RECURSE(NODE)							\
  tsubst_expr (NODE, args, complain, in_decl)

  /* The following two can only occur in Objective-C++.  */

  switch ((int) TREE_CODE (t))
    {
    case MESSAGE_SEND_EXPR:
      return objc_finish_message_expr
	(RECURSE (TREE_OPERAND (t, 0)),
	 TREE_OPERAND (t, 1),  /* No need to expand the selector.  */
	 RECURSE (TREE_OPERAND (t, 2)), NULL);

    case CLASS_REFERENCE_EXPR:
      {
	tree ident = TREE_OPERAND (t, 0);
	if (TYPE_P (ident))
	  ident = tsubst (ident, args, complain, in_decl);
	else
	  ident = RECURSE (ident);
	return objc_get_class_reference (ident);
      }

    default:
      break;
    }

  /* Fall back to C++ processing.  */
  return NULL_TREE;

#undef RECURSE
}

/* Implement c-family hook to add language-specific features
   for __has_{feature,extension}.  */

void
c_family_register_lang_features ()
{
  objc_common_register_features ();
  cp_register_features ();
}

static void
objcxx_init_ts (void)
{
  objc_common_init_ts ();
  cp_common_init_ts ();
}

#include "gtype-objcp.h"
