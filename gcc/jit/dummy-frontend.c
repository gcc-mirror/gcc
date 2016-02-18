/* jit.c -- Dummy "frontend" for use during JIT-compilation.
   Copyright (C) 2013-2016 Free Software Foundation, Inc.

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
#include "jit-playback.h"
#include "stor-layout.h"
#include "debug.h"
#include "langhooks.h"
#include "langhooks-def.h"


#include <mpfr.h>

/* Language-dependent contents of a type.  */

struct GTY(()) lang_type
{
  char dummy;
};

/* Language-dependent contents of a decl.  */

struct GTY((variable_size)) lang_decl
{
  char dummy;
};

/* Language-dependent contents of an identifier.  This must include a
   tree_identifier.  */

struct GTY(()) lang_identifier
{
  struct tree_identifier common;
};

/* The resulting tree type.  */

union GTY((desc ("TREE_CODE (&%h.generic) == IDENTIFIER_NODE"),
	   chain_next ("CODE_CONTAINS_STRUCT (TREE_CODE (&%h.generic), TS_COMMON) ? ((union lang_tree_node *) TREE_CHAIN (&%h.generic)) : NULL")))
lang_tree_node
{
  union tree_node GTY((tag ("0"),
		       desc ("tree_node_structure (&%h)"))) generic;
  struct lang_identifier GTY((tag ("1"))) identifier;
};

/* We don't use language_function.  */

struct GTY(()) language_function
{
  int dummy;
};

/* GC-marking callback for use from jit_root_tab.

   If there's an active playback context, call its marking method
   so that it can mark any pointers it references.  */

static void my_ggc_walker (void *)
{
  if (gcc::jit::active_playback_ctxt)
    gcc::jit::active_playback_ctxt->gt_ggc_mx ();
}

const char *dummy;

struct ggc_root_tab jit_root_tab[] =
  {
    {
      &dummy, 1, 0, my_ggc_walker, NULL
    },
    LAST_GGC_ROOT_TAB
  };

/* Language hooks.  */

static bool
jit_langhook_init (void)
{
  gcc_assert (gcc::jit::active_playback_ctxt);
  JIT_LOG_SCOPE (gcc::jit::active_playback_ctxt->get_logger ());

  static bool registered_root_tab = false;
  if (!registered_root_tab)
    {
      ggc_register_root_tab (jit_root_tab);
      registered_root_tab = true;
    }

  build_common_tree_nodes (false);

  /* I don't know why this has to be done explicitly.  */
  void_list_node = build_tree_list (NULL_TREE, void_type_node);

  build_common_builtin_nodes ();

  /* The default precision for floating point numbers.  This is used
     for floating point constants with abstract type.  This may
     eventually be controllable by a command line option.  */
  mpfr_set_default_prec (256);

  return true;
}

static void
jit_langhook_parse_file (void)
{
  /* Replay the activity by the client, recorded on the context.  */
  gcc_assert (gcc::jit::active_playback_ctxt);
  gcc::jit::active_playback_ctxt->replay ();
}

static tree
jit_langhook_type_for_mode (enum machine_mode mode, int unsignedp)
{
  if (mode == TYPE_MODE (float_type_node))
    return float_type_node;

  if (mode == TYPE_MODE (double_type_node))
    return double_type_node;

  if (mode == TYPE_MODE (intQI_type_node))
    return unsignedp ? unsigned_intQI_type_node : intQI_type_node;
  if (mode == TYPE_MODE (intHI_type_node))
    return unsignedp ? unsigned_intHI_type_node : intHI_type_node;
  if (mode == TYPE_MODE (intSI_type_node))
    return unsignedp ? unsigned_intSI_type_node : intSI_type_node;
  if (mode == TYPE_MODE (intDI_type_node))
    return unsignedp ? unsigned_intDI_type_node : intDI_type_node;
  if (mode == TYPE_MODE (intTI_type_node))
    return unsignedp ? unsigned_intTI_type_node : intTI_type_node;

  if (mode == TYPE_MODE (integer_type_node))
    return unsignedp ? unsigned_type_node : integer_type_node;

  if (mode == TYPE_MODE (long_integer_type_node))
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;

  if (mode == TYPE_MODE (long_long_integer_type_node))
    return unsignedp ? long_long_unsigned_type_node : long_long_integer_type_node;

  if (COMPLEX_MODE_P (mode))
    {
      if (mode == TYPE_MODE (complex_float_type_node))
	return complex_float_type_node;
      if (mode == TYPE_MODE (complex_double_type_node))
	return complex_double_type_node;
      if (mode == TYPE_MODE (complex_long_double_type_node))
	return complex_long_double_type_node;
      if (mode == TYPE_MODE (complex_integer_type_node) && !unsignedp)
	return complex_integer_type_node;
    }

  /* gcc_unreachable */
  return NULL;
}

static tree
jit_langhook_type_for_size (unsigned int bits ATTRIBUTE_UNUSED,
			    int unsignedp ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
  return NULL;
}

/* Record a builtin function.  We just ignore builtin functions.  */

static tree
jit_langhook_builtin_function (tree decl)
{
  return decl;
}

static bool
jit_langhook_global_bindings_p (void)
{
  gcc_unreachable ();
  return true;
}

static tree
jit_langhook_pushdecl (tree decl ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}

static tree
jit_langhook_getdecls (void)
{
  return NULL;
}

#undef LANG_HOOKS_NAME
#define LANG_HOOKS_NAME		"libgccjit"

#undef LANG_HOOKS_INIT
#define LANG_HOOKS_INIT		jit_langhook_init

#undef LANG_HOOKS_PARSE_FILE
#define LANG_HOOKS_PARSE_FILE		jit_langhook_parse_file

#undef LANG_HOOKS_TYPE_FOR_MODE
#define LANG_HOOKS_TYPE_FOR_MODE	jit_langhook_type_for_mode

#undef LANG_HOOKS_TYPE_FOR_SIZE
#define LANG_HOOKS_TYPE_FOR_SIZE	jit_langhook_type_for_size

#undef LANG_HOOKS_BUILTIN_FUNCTION
#define LANG_HOOKS_BUILTIN_FUNCTION	jit_langhook_builtin_function

#undef LANG_HOOKS_GLOBAL_BINDINGS_P
#define LANG_HOOKS_GLOBAL_BINDINGS_P	jit_langhook_global_bindings_p

#undef LANG_HOOKS_PUSHDECL
#define LANG_HOOKS_PUSHDECL		jit_langhook_pushdecl

#undef LANG_HOOKS_GETDECLS
#define LANG_HOOKS_GETDECLS		jit_langhook_getdecls

struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

#include "gt-jit-dummy-frontend.h"
#include "gtype-jit.h"
