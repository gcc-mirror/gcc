/* Language-dependent hooks for Algol 68.
   Copyright (C) 2025 Jose E. Marchesi.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "toplev.h"
#include "langhooks.h"
#include "langhooks-def.h"
#include "target.h"
#include "stringpool.h"
#include "debug.h"
#include "diagnostic.h"
#include "opts.h"
#include "machmode.h"
#include "stor-layout.h" /* For layout_type */
#include "vec.h"

#include "a68.h"

/* Global state for the Algol 68 front end.  */

A68_T a68_common;

/* Types expected by gcc's garbage collector.
   These types exist to allow language front-ends to
   add extra information in gcc's parse tree data structure. */

struct GTY(()) lang_type
{
  MOID_T * moid;
};

struct GTY(()) lang_decl
{
  NODE_T * node;
};

/* Language-specific identifier information.  This must include a
   tree_identifier.  */
struct GTY(()) lang_identifier
{
  struct tree_identifier common;
};


struct GTY(()) language_function
{
  int dummy;
};

/* The Algol68 frontend Type AST for GCC type NODE.  */
#define TYPE_LANG_FRONTEND(NODE) \
  (TYPE_LANG_SPECIFIC (NODE) \
   ? TYPE_LANG_SPECIFIC (NODE)->type : NULL)

/* The resulting tree type.  */

union GTY((desc ("TREE_CODE (&%h.generic) == IDENTIFIER_NODE"),
            chain_next ("CODE_CONTAINS_STRUCT (TREE_CODE (&%h.generic), "
                        "TS_COMMON) ? ((union lang_tree_node *) TREE_CHAIN "
                        "(&%h.generic)) : NULL"))) lang_tree_node
{
  union tree_node GTY((tag ("0"),
		       desc ("tree_node_structure (&%h)"))) generic;
  struct lang_identifier GTY ((tag ("1"))) identifier;
};

/* Allocate and return a lang specific structure for type tree nodes.  */

struct lang_type *
a68_build_lang_type (MOID_T *moid)
{
  tree ctype = CTYPE (moid);
  struct lang_type *lt = ctype ? TYPE_LANG_SPECIFIC (ctype) : NULL;

  if (lt == NULL)
    lt = (struct lang_type *) ggc_cleared_alloc <struct lang_type> ();
  if (lt->moid == NULL)
    lt->moid = moid;
  return lt;
}

/* Allocate and return a lang specific structure for decl tree nodes.  */

struct lang_decl *
a68_build_lang_decl (NODE_T *node)
{
  tree cdecl = CDECL (node);
  struct lang_decl *ld = cdecl ? DECL_LANG_SPECIFIC (cdecl) : NULL;

  if (ld == NULL)
    ld = (struct lang_decl *) ggc_cleared_alloc <struct lang_decl> ();
  if (ld->node == NULL)
    ld->node = node;
  return ld;
}

/* Get the front-end mode associated with the given TYPE.  If no mode is
   associated then this function returns NO_MODE.  */

MOID_T *
a68_type_moid (tree type)
{
  gcc_assert (TYPE_LANG_SPECIFIC (type) != NULL
	      && TYPE_LANG_SPECIFIC (type)->moid != NO_MOID);
  return TYPE_LANG_SPECIFIC (type)->moid;
}

/* Build the type trees in a68_global_trees.  */

static void
a68_build_a68_type_nodes (void)
{
  /* VOID */
  a68_void_type = make_node (RECORD_TYPE);
  TYPE_NAME (a68_void_type) = get_identifier ("void%");
  TYPE_FIELDS (a68_void_type) = NULL_TREE;
  TYPE_READONLY (a68_void_type) = 1;
  TYPE_CXX_ODR_P (a68_void_type) = 1;
  layout_type (a68_void_type);

  /* BOOL */
  a68_bool_type = boolean_type_node;

  /* CHAR */
  a68_char_type = uint32_type_node;

  /* SHORT SHORT INT
     SHORT INT
     INT */
  a68_short_short_int_type = signed_char_type_node;
  a68_short_int_type = short_integer_type_node;
  a68_int_type = integer_type_node;

  /* LONG INT */
  if (int_size_in_bytes (long_integer_type_node)
      > int_size_in_bytes (a68_int_type))
    a68_long_int_type = long_integer_type_node;
  else if (int_size_in_bytes (long_long_integer_type_node)
	   > int_size_in_bytes (a68_int_type))
    a68_long_int_type = long_long_integer_type_node;
  else
    a68_long_int_type = a68_int_type;

  /* LONG LONG INT */
  if (int_size_in_bytes (long_integer_type_node)
      > int_size_in_bytes (a68_long_int_type))
    a68_long_long_int_type = long_integer_type_node;
  else if (int_size_in_bytes (long_long_integer_type_node)
	   > int_size_in_bytes (a68_long_int_type))
    a68_long_long_int_type = long_long_integer_type_node;
  else
    a68_long_long_int_type = a68_long_int_type;

  /* SHORT SHORT BITS
     SHORT BITS
     BITS */
  a68_short_short_bits_type = unsigned_char_type_node;
  a68_short_bits_type = short_unsigned_type_node;
  a68_bits_type = unsigned_type_node;

  /* LONG BITS */
  if (int_size_in_bytes (long_unsigned_type_node)
      > int_size_in_bytes (a68_bits_type))
    a68_long_bits_type = long_unsigned_type_node;
  else if (int_size_in_bytes (long_long_unsigned_type_node)
	   > int_size_in_bytes (a68_bits_type))
    a68_long_bits_type = long_long_unsigned_type_node;
  else
    a68_long_bits_type = a68_bits_type;

  /* LONG LONG BITS */
  if (int_size_in_bytes (long_unsigned_type_node)
      > int_size_in_bytes (a68_long_bits_type))
    a68_long_long_bits_type = long_unsigned_type_node;
  else if (int_size_in_bytes (long_long_unsigned_type_node)
	   > int_size_in_bytes (a68_long_bits_type))
    a68_long_long_bits_type = long_long_unsigned_type_node;
  else
    a68_long_long_bits_type = a68_long_bits_type;

  /* BYTES
     LONG BYTES */
  a68_bytes_type = unsigned_type_node;
  a68_long_bytes_type = long_unsigned_type_node;

  /* REAL
     LONG REAL
     LONG LONG REAL */
  a68_real_type = float_type_node;
  a68_long_real_type = double_type_node;
  a68_long_long_real_type = long_double_type_node;
}

/* Language hooks data structures.  This is the main interface between
   the GCC front-end and the GCC middle-end/back-end.  A list of
   language hooks can be found in langhooks.h.  */

#undef LANG_HOOKS_NAME
#define LANG_HOOKS_NAME "GNU Algol 68"

/* LANG_HOOKS_INIT gets called to initialize the front-end.
   Invoked after option handling.  */

static bool
a68_init (void)
{
  build_common_tree_nodes (false);
  a68_build_a68_type_nodes ();
  targetm.init_builtins ();
  build_common_builtin_nodes ();
  a68_install_builtins ();

  /* Initialize binding contexts.  */
  a68_init_ranges ();

  /* Set the type of size_t.  */
  if (TYPE_MODE (long_unsigned_type_node) == ptr_mode)
    size_type_node = long_unsigned_type_node;
  else if (TYPE_MODE (long_long_unsigned_type_node) == ptr_mode)
    size_type_node = long_long_unsigned_type_node;
  else
    size_type_node = long_unsigned_type_node;

  return true;
}

#undef LANG_HOOKS_INIT
#define LANG_HOOKS_INIT a68_init

/* LANG_HOOKS_OPTION_LANG_MASK  */

static unsigned int
a68_option_lang_mask (void)
{
  return CL_Algol68;
}

#undef LANG_HOOKS_OPTION_LANG_MASK
#define LANG_HOOKS_OPTION_LANG_MASK a68_option_lang_mask


/* Return a data type that has machine mode MODE.  If the mode is an
   integer, then UNSIGNEDP selects between signed and unsigned types.  */

static tree
a68_type_for_mode (enum machine_mode mode, int unsignedp)
{
  if (mode == TImode)
    return unsignedp ? unsigned_intTI_type_node : intTI_type_node;

  if (mode == QImode)
    return unsignedp ? a68_short_short_bits_type :a68_short_short_int_type;

  if (mode == HImode)
    return unsignedp ? a68_short_bits_type : a68_short_int_type;

  if (mode == SImode)
    return unsignedp ? a68_bits_type : a68_int_type;

  if (mode == DImode)
    return unsignedp ? a68_long_bits_type : a68_long_int_type;

  if (mode == TYPE_MODE (a68_long_long_bits_type))
    return unsignedp ? a68_long_long_bits_type : a68_long_long_int_type;

  if (mode == TYPE_MODE (a68_real_type))
    return a68_real_type;

  if (mode == TYPE_MODE (a68_long_real_type))
    return a68_long_real_type;

  if (mode == TYPE_MODE (a68_long_long_real_type))
    return a68_long_long_real_type;

  if (mode == TYPE_MODE (build_pointer_type (char_type_node)))
    return build_pointer_type (char_type_node);

  if (mode == TYPE_MODE (build_pointer_type (integer_type_node)))
    return build_pointer_type (integer_type_node);

  for (int i = 0; i < NUM_INT_N_ENTS; i ++)
    {
      if (int_n_enabled_p[i] && mode == int_n_data[i].m)
	{
	  if (unsignedp)
	    return int_n_trees[i].unsigned_type;
	  else
	    return int_n_trees[i].signed_type;
	}
    }

  return 0;
}

#undef LANG_HOOKS_TYPE_FOR_MODE
#define LANG_HOOKS_TYPE_FOR_MODE a68_type_for_mode


/* Return an integer type with BITS bits of precision,
   that is unsigned if UNSIGNEDP is nonzero, otherwise signed.  */

static tree
a68_type_for_size (unsigned int bits, int unsignedp)
{
  if (unsignedp)
    {
      /* Handle TImode as a special case because it is used by some backends
	 (e.g. ARM) even though it is not available for normal use.  */
      if (bits == TYPE_PRECISION (unsigned_intTI_type_node))
	return unsigned_intTI_type_node;

      if (bits <= TYPE_PRECISION (a68_short_short_bits_type))
	return a68_short_short_bits_type;
      if (bits <= TYPE_PRECISION (a68_short_bits_type))
	return a68_short_bits_type;
      if (bits <= TYPE_PRECISION (a68_bits_type))
	return a68_bits_type;
      if (bits <= TYPE_PRECISION (a68_long_bits_type))
	return a68_long_bits_type;
      if (bits <= TYPE_PRECISION (a68_long_long_bits_type))
	return a68_long_long_bits_type;
    }
  else
    {
      /* Handle TImode as a special case because it is used by some backends
         (e.g. ARM) even though it is not available for normal use.  */
      if (bits == TYPE_PRECISION (intTI_type_node))
	return intTI_type_node;

      if (bits <= TYPE_PRECISION (a68_short_short_int_type))
	return a68_short_short_int_type;
      if (bits <= TYPE_PRECISION (a68_short_int_type))
	return a68_short_int_type;
      if (bits <= TYPE_PRECISION (a68_int_type))
	return a68_int_type;
      if (bits <= TYPE_PRECISION (a68_long_int_type))
	return a68_long_int_type;
      if (bits <= TYPE_PRECISION (a68_long_long_int_type))
	return a68_long_long_int_type;
    }

  for (int i = 0; i < NUM_INT_N_ENTS; ++i)
    {
      if (int_n_enabled_p[i] && bits == int_n_data[i].bitsize)
	{
	  if (unsignedp)
	    return int_n_trees[i].unsigned_type;
	  else
	    return int_n_trees[i].signed_type;
	}
    }

  return 0;
}

#undef LANG_HOOKS_TYPE_FOR_SIZE
#define LANG_HOOKS_TYPE_FOR_SIZE a68_type_for_size


/* Implements the lang_hooks.decls.global_bindings_p routine for Algol 68.
   Return true if we are in the global binding level.  */

static bool
a68_global_bindings_p (void)
{
  return (current_function_decl == NULL_TREE);
}

#undef LANG_HOOKS_GLOBAL_BINDINGS_P
#define LANG_HOOKS_GLOBAL_BINDINGS_P a68_global_bindings_p

/* Implements the lang_hooks.decls.getdecls routine.
   Return the list of declarations of the current level.  */

static tree
a68_getdecls (void)
{
  return a68_range_names ();
}

#undef LANG_HOOKS_GETDECLS
#define LANG_HOOKS_GETDECLS a68_getdecls

/* Return A68_GLOBAL_CONTEXT, but create it first if need be.  */

static tree
get_global_context (void)
{
  if (!A68_GLOBAL_CONTEXT)
    {
      A68_GLOBAL_CONTEXT = build_translation_unit_decl (NULL_TREE);
      debug_hooks->register_main_translation_unit (A68_GLOBAL_CONTEXT);
    }

  return A68_GLOBAL_CONTEXT;
}

/* Implements the lang_hooks.decls.pushdecl routine.
   Record DECL as belonging to the current lexical scope.  */

static tree
pushdecl (tree decl)
{
  /* Set the context of the decl.  If current_function_decl did not help in
     determining the context, use global scope.  */
  if (!DECL_CONTEXT (decl))
    {
      if (current_function_decl)
	DECL_CONTEXT (decl) = current_function_decl;
      else
	DECL_CONTEXT (decl) = get_global_context ();
    }

  /* Put decls on list in reverse order.  */
  if (TREE_STATIC (decl) || a68_global_bindings_p ())
    vec_safe_push (A68_GLOBAL_DECLARATIONS, decl);
  else
    a68_add_decl (decl);

  return decl;
}

#undef LANG_HOOKS_PUSHDECL
#define LANG_HOOKS_PUSHDECL pushdecl

/* Implements the lang_hooks.init_options routine for language Algol 68.  This
   initializes the global state for the frontend before calling the option
   handlers.  */

static void
a68_init_options (unsigned int argc ATTRIBUTE_UNUSED,
		  cl_decoded_option *decoded_options ATTRIBUTE_UNUSED)
{
  /* Create an empty module files map and fill in some modules that are
     provided by the run-time libga68 library.  */
  A68_MODULE_FILES = hash_map<nofree_string_hash,const char*>::create_ggc (16);
  A68_MODULE_FILES->empty ();
  A68_MODULE_FILES->put (ggc_strdup ("TRANSPUT"), ggc_strdup ("ga68"));
}

#undef LANG_HOOKS_INIT_OPTIONS
#define LANG_HOOKS_INIT_OPTIONS	a68_init_options


/* Handle -fcheck= option.  */

static void
a68_handle_runtime_check_option (const char *arg)
{
  int pos = 0;

  while (*arg)
    {
      /* We accept entries like -fcheck=nil,,bounds and -fcheck=,all.  */
      while (*arg == ',')
	arg++;

      while (arg[pos] && arg[pos] != ',')
	pos++;

      /* Process an option flag in the -fcheck= specification.

	 "all" means enable all run-time checks.
	 "none" means disable all run-time checks.

	 Options are processed from left to right, with increase
	 precedende.  */

      if (strncmp (arg, "all", pos) == 0)
	{
	  OPTION_NIL_CHECKING (&A68_JOB) = true;
	  OPTION_BOUNDS_CHECKING (&A68_JOB) = true;
	}
      else if (strncmp (arg, "none", pos) == 0)
	{
	  OPTION_NIL_CHECKING (&A68_JOB) = false;
	  OPTION_BOUNDS_CHECKING (&A68_JOB) = false;
	}
      else if (strncmp (arg, "nil", pos) == 0)
	OPTION_NIL_CHECKING (&A68_JOB) = true;
      else if (strncmp (arg, "no-nil", pos) == 0)
	OPTION_NIL_CHECKING (&A68_JOB) = false;
      else if (strncmp (arg, "bounds", pos) == 0)
	OPTION_BOUNDS_CHECKING (&A68_JOB) = true;
      else if (strncmp (arg, "no-bounds", pos) == 0)
	OPTION_BOUNDS_CHECKING (&A68_JOB) = false;
      else
	fatal_error (UNKNOWN_LOCATION,
		     "Argument to %<-fcheck%> is not valid: %s", arg);

      /* Process next flag.  */
      arg += pos;
      pos = 0;
    }
}

/* Handle Algol 68 specific options.  Return false if we didn't do
   anything.  */

static bool
a68_handle_option (size_t scode,
		   const char *arg,
		   HOST_WIDE_INT value ATTRIBUTE_UNUSED,
		   int kind ATTRIBUTE_UNUSED,
		   location_t loc ATTRIBUTE_UNUSED,
		   const cl_option_handlers *handlers ATTRIBUTE_UNUSED)
{
  opt_code code = (opt_code) scode;

  switch (code)
    {
    case OPT_fmodules_map:
    case OPT_fmodules_map_:
      {
	const char *errmsg;
	if (!a68_process_module_map (arg, &errmsg))
	  error ("invalid argument for %<-fmodules-map%>: %s", errmsg);
	break;
      }
    case OPT_fmodules_map_file:
    case OPT_fmodules_map_file_:
      {
	FILE *file = fopen (arg, "r");
	if (file == NULL)
	  fatal_error (UNKNOWN_LOCATION,
		       "cannot open modules map file %<%s%>", arg);
	
	ssize_t ssize = a68_file_size (fileno (file));
	if (ssize < 0)
	  fatal_error (UNKNOWN_LOCATION,
		       "cannot determine size of modules map file %<%s%>", arg);
	size_t fsize = ssize;

	char *buffer = (char *) xmalloc (fsize + 1);
	size_t bytes_read = a68_file_read (fileno (file), buffer, fsize);
	if (bytes_read != fsize)
	  fatal_error (UNKNOWN_LOCATION,
		       "cannot read contents of modules map file %<%s%>", arg);
	buffer[fsize] = '\0';

	const char *errmsg;
	if (!a68_process_module_map (buffer, &errmsg))
	  fatal_error (UNKNOWN_LOCATION, "%s: %s", arg, errmsg);
	free (buffer);
	break;
      }
    case OPT_std_algol68:
      OPTION_STRICT (&A68_JOB) = 1;
      break;
    case OPT_fbrackets:
      OPTION_BRACKETS (&A68_JOB) = flag_brackets;
      break;
    case OPT_fassert:
      OPTION_ASSERT (&A68_JOB) = flag_assert;
      break;
    case OPT_fcheck_:
      a68_handle_runtime_check_option (arg);
      break;
    case OPT_fstropping_:
      if (value == 0)
	OPTION_STROPPING (&A68_JOB) = UPPER_STROPPING;
      else
	OPTION_STROPPING (&A68_JOB) = SUPPER_STROPPING;
      break;
    case OPT_I:
      vec_safe_push (A68_INCLUDE_PATHS, arg);
      vec_safe_push (A68_IMPORT_PATHS, arg);
      break;
    case OPT_L:
      vec_safe_push (A68_IMPORT_PATHS, arg);
      break;
    default:
      break;
    }

  return true;
}

#undef LANG_HOOKS_HANDLE_OPTION
#define LANG_HOOKS_HANDLE_OPTION a68_handle_option

/* LANG_HOOKS_INIT_OPTIONS_STRUCT is called so the front-end can
   change some default values in the compiler's option structure.  */

static void
a68_init_options_struct (struct gcc_options *opts)
{
  /* Operations are always wrapping in algol68, even on signed
     integer.  */
  opts->x_flag_wrapv = 1;
  /* Do not warn for voiding by default.  */
  opts->x_warn_algol68_voiding = 0;
  /* Do not warn for usage of Algol 68 extensions by default.  */
  opts->x_warn_algol68_extensions = 0;
  /* Do not warn for potential scope violations by default.  */
  opts->x_warn_algol68_scope = 0;
  /* Do not warn for hidden declarations by default.  */
  opts->x_warn_algol68_hidden_declarations = 0;
  /* Enable assertions by default.  */
  OPTION_ASSERT (&A68_JOB) = 1;
  /* Disable run-time nil checking by default.  */
  OPTION_NIL_CHECKING (&A68_JOB) = 0;
  /* Enable run-time bounds checking by default.  */
  OPTION_BOUNDS_CHECKING (&A68_JOB) = 1;
  opts->x_flag_assert = 1;
  /* Allow GNU extensions by default.  */
  OPTION_STRICT (&A68_JOB) = 0;
  /* The default stropping regime is SUPPER.  */
  OPTION_STROPPING (&A68_JOB) = SUPPER_STROPPING;
}

#undef LANG_HOOKS_INIT_OPTIONS_STRUCT
#define LANG_HOOKS_INIT_OPTIONS_STRUCT a68_init_options_struct

/* Deal with any options that imply the turning on/off of features.  FILENAME
   is the main input file passed on the command line.  */

static bool
a68_post_options (const char **filename ATTRIBUTE_UNUSED)
{
  /* -fbounds-check is equivalent to -fcheck=bounds  */
  if (flag_bounds_check)
    OPTION_BOUNDS_CHECKING (&A68_JOB) = true;

  /* No psABI change warnings for Algol 68.  */
  warn_psabi = 0;

  return false;
}

#undef LANG_HOOKS_POST_OPTIONS
#define LANG_HOOKS_POST_OPTIONS	a68_post_options

/* LANG_HOOKS_PARSE_FILE is called to parse the input files.

   The input file names are available in the global variables
   in_fnames and num_in_fnames, and this function is required to
   create a complete parse tree from them in a global var, then
   return.  */

MOIF_T *moif;

static void
a68_parse_file (void)
{
  if (num_in_fnames != 1)
    fatal_error (UNKNOWN_LOCATION,
		 "exactly one source file must be specified on the command line");

  /* Run the Mailloux parser.  */
  a68_parser (in_fnames[0]);

  if (ERROR_COUNT (&A68_JOB) > 0)
    goto had_errors;

  /* Generate dumps if so requested.  */
  if (flag_a68_dump_modes)
    a68_dump_modes (TOP_MOID (&A68_JOB));
  if (flag_a68_dump_ast)
    a68_dump_parse_tree (TOP_NODE (&A68_JOB));

  /* Lower modes to GENERIC.  */
  a68_lower_moids (TOP_MOID (&A68_JOB));
  /* Lower the particular program.  */
  a68_lower_top_tree (TOP_NODE (&A68_JOB));

  if (ERROR_COUNT (&A68_JOB) > 0)
    goto had_errors;

  /* Emit exports information for any compiled module in this packet.  Note
     this must be done after the low pass.  */
  a68_do_exports (TOP_NODE (&A68_JOB));

  /* Process all file scopes in this compilation, and the external_scope,
     through wrapup_global_declarations.  */
  for (unsigned int i = 0; i < vec_safe_length (A68_GLOBAL_DECLARATIONS); i++)
    {
      tree decl = vec_safe_address (A68_GLOBAL_DECLARATIONS)[i];
      wrapup_global_declarations (&decl, 1);
    }

 had_errors:
  errorcount += ERROR_COUNT (&A68_JOB);
}

#undef LANG_HOOKS_PARSE_FILE
#define LANG_HOOKS_PARSE_FILE a68_parse_file

/* This hook is called for every GENERIC tree that gets gimplified.
   Its purpose is to gimplify language specific trees.

   At the moment we are not supporting any Algol 68 specific tree, so
   we just return FALSE.  */

static int
a68_gimplify_expr (tree *expr_p ATTRIBUTE_UNUSED,
                   gimple_seq *pre_p ATTRIBUTE_UNUSED,
                   gimple_seq *post_p ATTRIBUTE_UNUSED)
{
  return false;
}

#undef LANG_HOOKS_GIMPLIFY_EXPR
#define LANG_HOOKS_GIMPLIFY_EXPR a68_gimplify_expr

/* This function shall return the printable name of the language.  */

static const char *
a68_printable_name (tree decl, int kind ATTRIBUTE_UNUSED)
{
  tree decl_name = DECL_NAME (decl);

  if (decl_name == NULL_TREE)
    return "<unnamed>";
  else
    return IDENTIFIER_POINTER (decl_name);
}

#undef LANG_HOOKS_DECL_PRINTABLE_NAME
#define LANG_HOOKS_DECL_PRINTABLE_NAME a68_printable_name


/* Return true if a warning should be given about option OPTION, which is for
   the wrong language, false if it should be quietly ignored.  */

static bool
a68_complain_wrong_lang_p (const struct cl_option *option ATTRIBUTE_UNUSED)
{
  return false;
}

#undef LANG_HOOKS_COMPLAIN_WRONG_LANG_P
#define LANG_HOOKS_COMPLAIN_WRONG_LANG_P a68_complain_wrong_lang_p

/* Create an expression whose value is that of EXPR,
   converted to type TYPE.  The TREE_TYPE of the value
   is always TYPE.  This function implements all reasonable
   conversions; callers should filter out those that are
   not permitted by the language being compiled.

   Note that this function is not used outside the front-end.  This front-end
   doesn't currently use it at all.  */

tree convert (tree type ATTRIBUTE_UNUSED,
	      tree expr ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}

/* Implements the lang_hooks.types_compatible_p routine for Algol 68.
   Compares two types for equivalence in Algol 68.
   This routine should only return 1 if it is sure, even though the frontend
   should have already ensured that all types are compatible before handing
   over the parsed ASTs to the code generator.  */

static int
a68_types_compatible_p (tree x, tree y)
{
  MOID_T *mode_x = a68_type_moid (x);
  MOID_T *mode_y = a68_type_moid (y);

  if (mode_x != NO_MOID && mode_y != NO_MOID)
    return a68_is_equal_modes (mode_x, mode_y, SAFE_DEFLEXING);

  return false;
}

#undef LANG_HOOKS_TYPES_COMPATIBLE_P
#define LANG_HOOKS_TYPES_COMPATIBLE_P a68_types_compatible_p

/* Get a value for the SARIF v2.1.0 "artifact.sourceLanguage" property.  Algol
   68 is not yet listed in SARIF v2.1.0 Appendix J, but if/when it does, it
   will likely use this string.  */

const char *
a68_get_sarif_source_language (const char *)
{
  return "algol68";
}

#undef LANG_HOOKS_GET_SARIF_SOURCE_LANGUAGE
#define LANG_HOOKS_GET_SARIF_SOURCE_LANGUAGE a68_get_sarif_source_language

/* Expands all LANG_HOOKS_x o GCC.  */
struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

#include "gt-algol68-a68-lang.h"
#include "gtype-algol68.h"
