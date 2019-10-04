/* brig-lang.c -- brig (HSAIL) input gcc interface.
   Copyright (C) 2016-2019 Free Software Foundation, Inc.
   Contributed by Pekka Jaaskelainen <pekka.jaaskelainen@parmance.com>
   for General Processor Tech.

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
#include "ansidecl.h"
#include "coretypes.h"
#include "opts.h"
#include "tree.h"
#include "tree-iterator.h"
#include "print-tree.h"
#include "stringpool.h"
#include "basic-block.h"
#include "gimple-expr.h"
#include "gimplify.h"
#include "dumpfile.h"
#include "stor-layout.h"
#include "toplev.h"
#include "debug.h"
#include "options.h"
#include "flags.h"
#include "convert.h"
#include "diagnostic.h"
#include "langhooks.h"
#include "langhooks-def.h"
#include "target.h"
#include "vec.h"
#include "brigfrontend/brig-to-generic.h"
#include "machmode.h"
#include "fold-const.h"
#include "common/common-target.h"
#include <mpfr.h>
#include "brig-c.h"
#include "brig-builtins.h"

static tree handle_leaf_attribute (tree *, tree, tree, int, bool *);
static tree handle_const_attribute (tree *, tree, tree, int, bool *);
static tree handle_pure_attribute (tree *, tree, tree, int, bool *);
static tree handle_nothrow_attribute (tree *, tree, tree, int, bool *);
static tree handle_returns_twice_attribute (tree *, tree, tree, int, bool *);

/* This file is based on Go frontend's go-lang.c and gogo-tree.cc.  */

/* If -v set.  */

int gccbrig_verbose = 0;

/* Language-dependent contents of a type.  */

struct GTY (()) lang_type
{
  char dummy;
};

/* Language-dependent contents of a decl.  */

struct GTY ((variable_size)) lang_decl
{
  char dummy;
};

/* Language-dependent contents of an identifier.  This must include a
   tree_identifier.  */

struct GTY (()) lang_identifier
{
  struct tree_identifier common;
};

/* The resulting tree type.  */

union GTY ((desc ("TREE_CODE (&%h.generic) == IDENTIFIER_NODE"),
	    chain_next ("CODE_CONTAINS_STRUCT (TREE_CODE (&%h.generic), "
			"TS_COMMON) ? ((union lang_tree_node *) TREE_CHAIN "
			"(&%h.generic)) : NULL"))) lang_tree_node
{
  union tree_node GTY ((tag ("0"), desc ("tree_node_structure (&%h)"))) generic;
  struct lang_identifier GTY ((tag ("1"))) identifier;
};

/* We don't use language_function.  */

struct GTY (()) language_function
{
  int dummy;
};


/* The option mask.  */

static unsigned int
brig_langhook_option_lang_mask (void)
{
  return CL_BRIG;
}

/* Initialize the options structure.  */

static void
brig_langhook_init_options_struct (struct gcc_options *opts)
{
  /* Signed overflow is precisely defined.  */
  opts->x_flag_wrapv = 1;

  /* If we set this to one, the whole program optimizations internalize
     all global variables, making them invisible to the dyn loader (and
     thus the HSA runtime implementation).  */
  opts->x_flag_whole_program = 1;

  /* The builtin math functions should not set errno.  */
  opts->x_flag_errno_math = 0;
  opts->frontend_set_flag_errno_math = false;

  opts->x_flag_exceptions = 0;
  opts->x_flag_non_call_exceptions = 0;

  opts->x_flag_finite_math_only = 0;
  opts->x_flag_signed_zeros = 1;

  opts->x_optimize = 3;

  flag_no_builtin = 1;
}

/* Handle Brig specific options.  Return 0 if we didn't do anything.  */

static bool
brig_langhook_handle_option
  (size_t scode, const char *arg ATTRIBUTE_UNUSED,
  HOST_WIDE_INT value ATTRIBUTE_UNUSED, int kind ATTRIBUTE_UNUSED,
  location_t loc ATTRIBUTE_UNUSED,
  const struct cl_option_handlers *handlers ATTRIBUTE_UNUSED)
{
  enum opt_code code = (enum opt_code) scode;
  switch (code)
    {
    case OPT_v:
      gccbrig_verbose = 1;
      break;
    default:
      break;
    }
  return 1;
}

/* Run after parsing options.  */

static bool
brig_langhook_post_options (const char **pfilename ATTRIBUTE_UNUSED)
{
  if (flag_excess_precision == EXCESS_PRECISION_DEFAULT)
    flag_excess_precision = EXCESS_PRECISION_STANDARD;

  /* gccbrig casts pointers around like crazy, TBAA might produce broken
     code if not disabling it by default.  Some PRM conformance tests such
     as prm/core/memory/ordinary/ld/ld_u16 fail currently with strict
     aliasing (to fix).  It can be enabled from the command line for cases
     that are known not to break the C style aliasing requirements.  */
  if (!global_options_set.x_flag_strict_aliasing)
    flag_strict_aliasing = 0;
  else
    flag_strict_aliasing = global_options.x_flag_strict_aliasing;

  /* Returning false means that the backend should be used.  */
  return false;
}

static size_t
get_file_size (FILE *file)
{
  size_t size;
  fseek (file, 0, SEEK_END);
  size = (size_t) ftell (file);
  fseek (file, 0, SEEK_SET);
  return size;
}

static void
brig_langhook_parse_file (void)
{
  brig_to_generic brig_to_gen;

  std::vector <char*> brig_blobs;

  for (unsigned int i = 0; i < num_in_fnames; ++i)
    {

      FILE *f;
      f = fopen (in_fnames[i], "r");
      size_t fsize = get_file_size (f);
      char *brig_blob = new char[fsize];
      if (fread (brig_blob, 1, fsize, f) != fsize)
	{
	  error ("could not read the BRIG file");
	  exit (1);
	}
      fclose (f);

      brig_to_gen.analyze (brig_blob);
      brig_blobs.push_back (brig_blob);
    }

  for (size_t i = 0; i < brig_blobs.size(); ++i)
    {
      char *brig_blob = brig_blobs.at(i);
      brig_to_gen.parse (brig_blob);
    }

  brig_to_gen.write_globals ();

  for (size_t i = 0; i < brig_blobs.size (); ++i)
    delete brig_blobs[i];
}

static tree
brig_langhook_type_for_size (unsigned int bits,
			     int unsignedp)
{
  /* Copied from go-lang.c  */
  tree type;
  if (unsignedp)
    {
      if (bits == INT_TYPE_SIZE)
        type = unsigned_type_node;
      else if (bits == CHAR_TYPE_SIZE)
        type = unsigned_char_type_node;
      else if (bits == SHORT_TYPE_SIZE)
        type = short_unsigned_type_node;
      else if (bits == LONG_TYPE_SIZE)
        type = long_unsigned_type_node;
      else if (bits == LONG_LONG_TYPE_SIZE)
        type = long_long_unsigned_type_node;
      else
        type = make_unsigned_type(bits);
    }
  else
    {
      if (bits == INT_TYPE_SIZE)
        type = integer_type_node;
      else if (bits == CHAR_TYPE_SIZE)
        type = signed_char_type_node;
      else if (bits == SHORT_TYPE_SIZE)
        type = short_integer_type_node;
      else if (bits == LONG_TYPE_SIZE)
        type = long_integer_type_node;
      else if (bits == LONG_LONG_TYPE_SIZE)
        type = long_long_integer_type_node;
      else
        type = make_signed_type(bits);
    }
  return type;
}

static tree
brig_langhook_type_for_mode (machine_mode mode, int unsignedp)
{
  if (mode == TYPE_MODE (void_type_node))
    return void_type_node;

  if (VECTOR_MODE_P (mode))
    {
      tree inner;

      inner = brig_langhook_type_for_mode (GET_MODE_INNER (mode), unsignedp);
      if (inner != NULL_TREE)
	return build_vector_type_for_mode (inner, mode);
      gcc_unreachable ();
      return NULL_TREE;
    }

  scalar_int_mode imode;
  scalar_float_mode fmode;
  if (is_float_mode (mode, &fmode))
    {
      switch (GET_MODE_BITSIZE (fmode))
	{
	case 32:
	  return float_type_node;
	case 64:
	  return double_type_node;
	default:
	  /* We have to check for long double in order to support
	     i386 excess precision.  */
	  if (fmode == TYPE_MODE (long_double_type_node))
	    return long_double_type_node;

	  gcc_unreachable ();
	  return NULL_TREE;
	}
    }
  else if (is_int_mode (mode, &imode))
    return brig_langhook_type_for_size (GET_MODE_BITSIZE (imode), unsignedp);
  else
    {
      /* E.g., build_common_builtin_nodes () asks for modes/builtins
	       we do not generate or need.  Just ignore them silently for now.
      */
      return NULL_TREE;
    }
  return NULL_TREE;
}

static tree
brig_langhook_builtin_function (tree decl)
{
  return decl;
}

static GTY(()) tree registered_builtin_types;

static void
brig_langhook_register_builtin_type (tree type, const char *name)
{
  tree decl;

  if (!TYPE_NAME (type))
    {
      decl = build_decl (UNKNOWN_LOCATION, TYPE_DECL,
			 get_identifier (name), type);
      DECL_ARTIFICIAL (decl) = 1;
      TYPE_NAME (type) = decl;
    }

  registered_builtin_types = tree_cons (0, type, registered_builtin_types);
}


/* Return true if we are in the global binding level.  */

static bool
brig_langhook_global_bindings_p (void)
{
  return current_function_decl == NULL_TREE;
}

/* Push a declaration into the current binding level.  From Go: We can't
   usefully implement this since we don't want to convert from tree
   back to one of our internal data structures.  I think the only way
   this is used is to record a decl which is to be returned by
   getdecls, and we could implement it for that purpose if
   necessary.  */

static tree
brig_langhook_pushdecl (tree decl ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
}

/* This hook is used to get the current list of declarations as trees.
   From Go: We don't support that; instead we use the write_globals hook.
   This can't simply crash because it is called by -gstabs.  */

static tree
brig_langhook_getdecls (void)
{
  return NULL;
}

static int
brig_langhook_gimplify_expr (tree *expr_p, gimple_seq *pre_p ATTRIBUTE_UNUSED,
			     gimple_seq *post_p ATTRIBUTE_UNUSED)
{

  /* Strip off the static chain info that appears to function
     calls for some strange reason even though we don't add
     nested functions.  Maybe something wrong with the function
     declaration contexts? */
  if (TREE_CODE (*expr_p) == CALL_EXPR
      && CALL_EXPR_STATIC_CHAIN (*expr_p) != NULL_TREE)
    CALL_EXPR_STATIC_CHAIN (*expr_p) = NULL_TREE;
  return GS_UNHANDLED;
}

static tree
brig_langhook_eh_personality (void)
{
  gcc_unreachable ();
}

/* Functions called directly by the generic backend.
   Adapted from go-lang.c.  */

tree
convert (tree type, tree expr)
{
  if (type == error_mark_node || expr == error_mark_node
      || TREE_TYPE (expr) == error_mark_node)
    return error_mark_node;

  if (type == TREE_TYPE (expr))
    return expr;

  if (TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (TREE_TYPE (expr)))
    return fold_convert (type, expr);

  switch (TREE_CODE (type))
    {
    case VOID_TYPE:
    case BOOLEAN_TYPE:
      return fold_convert (type, expr);
    case INTEGER_TYPE:
      return fold (convert_to_integer (type, expr));
    case REAL_TYPE:
      return fold (convert_to_real (type, expr));
    case VECTOR_TYPE:
      return fold (convert_to_vector (type, expr));
    case POINTER_TYPE:
      return build1 (VIEW_CONVERT_EXPR, type, convert (size_type_node, expr));
    default:
      break;
    }

  gcc_unreachable ();
}

static GTY (()) tree brig_gc_root;

/* Preserve trees that we create from the garbage collector.  */

void
brig_preserve_from_gc (tree t)
{
  brig_gc_root = tree_cons (NULL_TREE, t, brig_gc_root);
}

/* Convert an identifier for use in an error message.  */

const char *
brig_localize_identifier (const char *ident)
{
  return identifier_to_locale (ident);
}

/* Define supported attributes and their handlers. Code copied from
   lto-lang.c */

/* Table of machine-independent attributes supported in GIMPLE.  */
const struct attribute_spec brig_attribute_table[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req,
       affects_type_identity, handler, exclude } */
  { "leaf",		      0, 0, true,  false, false, false,
			      handle_leaf_attribute, NULL },
  { "const",                  0, 0, true,  false, false, false,
			      handle_const_attribute, NULL },
  { "pure",                   0, 0, true,  false, false, false,
			      handle_pure_attribute, NULL },
  { "nothrow",                0, 0, true,  false, false, false,
			      handle_nothrow_attribute, NULL },
  { "returns_twice",          0, 0, true,  false, false, false,
			      handle_returns_twice_attribute, NULL },
  { NULL,                     0, 0, false, false, false, false, NULL, NULL }
};

/* Attribute handlers.  */
/* Handle a "leaf" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_leaf_attribute (tree *node, tree name,
		       tree ARG_UNUSED (args),
		       int ARG_UNUSED (flags), bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }
  if (!TREE_PUBLIC (*node))
    {
      warning (OPT_Wattributes,
	       "%qE attribute has no effect on unit local functions", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle a "const" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_const_attribute (tree *node, tree ARG_UNUSED (name),
			tree ARG_UNUSED (args), int ARG_UNUSED (flags),
			bool * ARG_UNUSED (no_add_attrs))
{
  tree type = TREE_TYPE (*node);

  /* See FIXME comment on noreturn in c_common_attribute_table.  */
  if (TREE_CODE (*node) == FUNCTION_DECL)
    TREE_READONLY (*node) = 1;
  else if (TREE_CODE (type) == POINTER_TYPE
	   && TREE_CODE (TREE_TYPE (type)) == FUNCTION_TYPE)
    TREE_TYPE (*node)
      = build_pointer_type
	(build_type_variant (TREE_TYPE (type), 1,
			     TREE_THIS_VOLATILE (TREE_TYPE (type))));
  else
    gcc_unreachable ();

  return NULL_TREE;
}

/* Handle a "pure" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_pure_attribute (tree *node, tree ARG_UNUSED (name),
		       tree ARG_UNUSED (args), int ARG_UNUSED (flags),
		       bool * ARG_UNUSED (no_add_attrs))
{
  if (TREE_CODE (*node) == FUNCTION_DECL)
    DECL_PURE_P (*node) = 1;
  else
    gcc_unreachable ();

  return NULL_TREE;
}

/* Handle a "nothrow" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
handle_nothrow_attribute (tree *node, tree ARG_UNUSED (name),
			  tree ARG_UNUSED (args), int ARG_UNUSED (flags),
			  bool * ARG_UNUSED (no_add_attrs))
{
  if (TREE_CODE (*node) == FUNCTION_DECL)
    TREE_NOTHROW (*node) = 1;
  else
    gcc_unreachable ();

  return NULL_TREE;
}

/* Handle a "returns_twice" attribute.  */

static tree
handle_returns_twice_attribute (tree *node, tree ARG_UNUSED (name),
				tree ARG_UNUSED (args),
				int ARG_UNUSED (flags),
				bool * ARG_UNUSED (no_add_attrs))
{
  gcc_assert (TREE_CODE (*node) == FUNCTION_DECL);

  DECL_IS_RETURNS_TWICE (*node) = 1;

  return NULL_TREE;
}


/* Built-in initialization code cribbed from lto-lang.c which cribbed it
   from c-common.c.  */


static GTY(()) tree built_in_attributes[(int) ATTR_LAST];


static GTY(()) tree builtin_types[(int) BT_LAST + 1];

static GTY(()) tree string_type_node;
static GTY(()) tree const_string_type_node;
static GTY(()) tree wint_type_node;
static GTY(()) tree intmax_type_node;
static GTY(()) tree uintmax_type_node;
static GTY(()) tree signed_size_type_node;

/* Flags needed to process builtins.def.  */
int flag_isoc94;
int flag_isoc99;
int flag_isoc11;
int flag_isoc2x;

static void
def_fn_type (builtin_type def, builtin_type ret, bool var, int n, ...)
{
  tree t;
  tree *args = XALLOCAVEC (tree, n);
  va_list list;
  int i;
  bool err = false;

  va_start (list, n);
  for (i = 0; i < n; ++i)
    {
      builtin_type a = (builtin_type) va_arg (list, int);
      t = builtin_types[a];
      if (t == error_mark_node)
	err = true;
      args[i] = t;
    }
  va_end (list);

  t = builtin_types[ret];
  if (err)
    t = error_mark_node;
  if (t == error_mark_node)
    ;
  else if (var)
    t = build_varargs_function_type_array (t, n, args);
  else
    t = build_function_type_array (t, n, args);

  builtin_types[def] = t;
}

/* Used to help initialize the builtin-types.def table.  When a type of
   the correct size doesn't exist, use error_mark_node instead of NULL.
   The later results in segfaults even when a decl using the type doesn't
   get invoked.  */

static tree
builtin_type_for_size (int size, bool unsignedp)
{
  tree type = brig_langhook_type_for_size (size, unsignedp);
  return type ? type : error_mark_node;
}

/* Support for DEF_BUILTIN.  */

static void
def_builtin_1 (enum built_in_function fncode, const char *name,
	       enum built_in_class fnclass ATTRIBUTE_UNUSED,
	       tree fntype, tree libtype ATTRIBUTE_UNUSED,
	       bool both_p ATTRIBUTE_UNUSED, bool fallback_p,
	       bool nonansi_p ATTRIBUTE_UNUSED, tree fnattrs,
	       bool implicit_p)
{
  tree decl;
  const char *libname;

  if (fntype == error_mark_node)
    return;

  libname = name + strlen ("__builtin_");
  decl = add_builtin_function (name, fntype, fncode, fnclass,
			       (fallback_p ? libname : NULL),
			       fnattrs);

  set_builtin_decl (fncode, decl, implicit_p);
}


/* Initialize the attribute table for all the supported builtins.  */

static void
brig_init_attributes (void)
{
  /* Fill in the built_in_attributes array.  */
#define DEF_ATTR_NULL_TREE(ENUM)				\
  built_in_attributes[(int) ENUM] = NULL_TREE;
#define DEF_ATTR_INT(ENUM, VALUE)				\
  built_in_attributes[(int) ENUM] = build_int_cst (NULL_TREE, VALUE);
#define DEF_ATTR_STRING(ENUM, VALUE)				\
  built_in_attributes[(int) ENUM] = build_string (strlen (VALUE), VALUE);
#define DEF_ATTR_IDENT(ENUM, STRING)				\
  built_in_attributes[(int) ENUM] = get_identifier (STRING);
#define DEF_ATTR_TREE_LIST(ENUM, PURPOSE, VALUE, CHAIN)	\
  built_in_attributes[(int) ENUM]			\
    = tree_cons (built_in_attributes[(int) PURPOSE],	\
		 built_in_attributes[(int) VALUE],	\
		 built_in_attributes[(int) CHAIN]);
#include "builtin-attrs.def"
#undef DEF_ATTR_NULL_TREE
#undef DEF_ATTR_INT
#undef DEF_ATTR_STRING
#undef DEF_ATTR_IDENT
#undef DEF_ATTR_TREE_LIST
}

/* Create builtin types and functions.  VA_LIST_REF_TYPE_NODE and
   VA_LIST_ARG_TYPE_NODE are used in builtin-types.def.  */

static void
brig_define_builtins (tree va_list_ref_type_node ATTRIBUTE_UNUSED,
		     tree va_list_arg_type_node ATTRIBUTE_UNUSED)
{
#define DEF_PRIMITIVE_TYPE(ENUM, VALUE) \
  builtin_types[ENUM] = VALUE;
#define DEF_FUNCTION_TYPE_0(ENUM, RETURN) \
  def_fn_type (ENUM, RETURN, 0, 0);
#define DEF_FUNCTION_TYPE_1(ENUM, RETURN, ARG1) \
  def_fn_type (ENUM, RETURN, 0, 1, ARG1);
#define DEF_FUNCTION_TYPE_2(ENUM, RETURN, ARG1, ARG2) \
  def_fn_type (ENUM, RETURN, 0, 2, ARG1, ARG2);
#define DEF_FUNCTION_TYPE_3(ENUM, RETURN, ARG1, ARG2, ARG3) \
  def_fn_type (ENUM, RETURN, 0, 3, ARG1, ARG2, ARG3);
#define DEF_FUNCTION_TYPE_4(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4) \
  def_fn_type (ENUM, RETURN, 0, 4, ARG1, ARG2, ARG3, ARG4);
#define DEF_FUNCTION_TYPE_5(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5)	\
  def_fn_type (ENUM, RETURN, 0, 5, ARG1, ARG2, ARG3, ARG4, ARG5);
#define DEF_FUNCTION_TYPE_6(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6)					\
  def_fn_type (ENUM, RETURN, 0, 6, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6);
#define DEF_FUNCTION_TYPE_7(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6, ARG7)					\
  def_fn_type (ENUM, RETURN, 0, 7, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6, ARG7);
#define DEF_FUNCTION_TYPE_8(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6, ARG7, ARG8)				\
  def_fn_type (ENUM, RETURN, 0, 8, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6,	\
	       ARG7, ARG8);
#define DEF_FUNCTION_TYPE_9(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			    ARG6, ARG7, ARG8, ARG9)			\
  def_fn_type (ENUM, RETURN, 0, 9, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6,	\
	       ARG7, ARG8, ARG9);
#define DEF_FUNCTION_TYPE_10(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			     ARG6, ARG7, ARG8, ARG9, ARG10)		 \
  def_fn_type (ENUM, RETURN, 0, 10, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6,	 \
	       ARG7, ARG8, ARG9, ARG10);
#define DEF_FUNCTION_TYPE_11(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
			     ARG6, ARG7, ARG8, ARG9, ARG10, ARG11)	 \
  def_fn_type (ENUM, RETURN, 0, 11, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6,	 \
	       ARG7, ARG8, ARG9, ARG10, ARG11);
#define DEF_FUNCTION_TYPE_VAR_0(ENUM, RETURN) \
  def_fn_type (ENUM, RETURN, 1, 0);
#define DEF_FUNCTION_TYPE_VAR_1(ENUM, RETURN, ARG1) \
  def_fn_type (ENUM, RETURN, 1, 1, ARG1);
#define DEF_FUNCTION_TYPE_VAR_2(ENUM, RETURN, ARG1, ARG2) \
  def_fn_type (ENUM, RETURN, 1, 2, ARG1, ARG2);
#define DEF_FUNCTION_TYPE_VAR_3(ENUM, RETURN, ARG1, ARG2, ARG3) \
  def_fn_type (ENUM, RETURN, 1, 3, ARG1, ARG2, ARG3);
#define DEF_FUNCTION_TYPE_VAR_4(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4) \
  def_fn_type (ENUM, RETURN, 1, 4, ARG1, ARG2, ARG3, ARG4);
#define DEF_FUNCTION_TYPE_VAR_5(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5) \
  def_fn_type (ENUM, RETURN, 1, 5, ARG1, ARG2, ARG3, ARG4, ARG5);
#define DEF_FUNCTION_TYPE_VAR_6(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
				 ARG6)	\
  def_fn_type (ENUM, RETURN, 1, 6, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6);
#define DEF_FUNCTION_TYPE_VAR_7(ENUM, RETURN, ARG1, ARG2, ARG3, ARG4, ARG5, \
				ARG6, ARG7)				\
  def_fn_type (ENUM, RETURN, 1, 7, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6, ARG7);
#define DEF_POINTER_TYPE(ENUM, TYPE) \
  builtin_types[(int) ENUM] = build_pointer_type (builtin_types[(int) TYPE]);

#include "builtin-types.def"

#undef DEF_PRIMITIVE_TYPE
#undef DEF_FUNCTION_TYPE_0
#undef DEF_FUNCTION_TYPE_1
#undef DEF_FUNCTION_TYPE_2
#undef DEF_FUNCTION_TYPE_3
#undef DEF_FUNCTION_TYPE_4
#undef DEF_FUNCTION_TYPE_5
#undef DEF_FUNCTION_TYPE_6
#undef DEF_FUNCTION_TYPE_7
#undef DEF_FUNCTION_TYPE_8
#undef DEF_FUNCTION_TYPE_9
#undef DEF_FUNCTION_TYPE_10
#undef DEF_FUNCTION_TYPE_11
#undef DEF_FUNCTION_TYPE_VAR_0
#undef DEF_FUNCTION_TYPE_VAR_1
#undef DEF_FUNCTION_TYPE_VAR_2
#undef DEF_FUNCTION_TYPE_VAR_3
#undef DEF_FUNCTION_TYPE_VAR_4
#undef DEF_FUNCTION_TYPE_VAR_5
#undef DEF_FUNCTION_TYPE_VAR_6
#undef DEF_FUNCTION_TYPE_VAR_7
#undef DEF_POINTER_TYPE
  builtin_types[(int) BT_LAST] = NULL_TREE;

  brig_init_attributes ();

#define DEF_BUILTIN(ENUM, NAME, CLASS, TYPE, LIBTYPE, BOTH_P, FALLBACK_P,\
		    NONANSI_P, ATTRS, IMPLICIT, COND)			\
    if (NAME && COND)							\
      def_builtin_1 (ENUM, NAME, CLASS, builtin_types[(int) TYPE],	\
		     builtin_types[(int) LIBTYPE], BOTH_P, FALLBACK_P,	\
		     NONANSI_P, built_in_attributes[(int) ATTRS], IMPLICIT);

#undef DEF_HSAIL_BUILTIN
#define DEF_HSAIL_BUILTIN(ENUM, HSAIL_OPCODE, HSAIL_TYPE, NAME, TYPE, ATTRS) \
  DEF_BUILTIN (ENUM, "__builtin_" NAME, BUILT_IN_NORMAL, TYPE, TYPE,    \
              false, true, true, ATTRS, false, true)

/* HSAIL atomic builtins do not have separate identifying opcodes.  */

#undef DEF_HSAIL_ATOMIC_BUILTIN
#define DEF_HSAIL_ATOMIC_BUILTIN(ENUM, ATOMIC_OPCODE, HSAIL_TYPE, NAME, \
       TYPE, ATTRS) \
  DEF_BUILTIN (ENUM, "__builtin_" NAME, BUILT_IN_NORMAL, TYPE, TYPE,    \
              false, true, true, ATTRS, false, true)

/* HSAIL saturating arithmetics builtins.  */

#undef DEF_HSAIL_SAT_BUILTIN
#define DEF_HSAIL_SAT_BUILTIN(ENUM, BRIG_OPCODE, HSAIL_TYPE, NAME, \
       TYPE, ATTRS) \
  DEF_BUILTIN (ENUM, "__builtin_" NAME, BUILT_IN_NORMAL, TYPE, TYPE,    \
              false, true, true, ATTRS, false, true)

/* HSAIL builtins used internally by the frontend.  */

#undef DEF_HSAIL_INTR_BUILTIN
#define DEF_HSAIL_INTR_BUILTIN(ENUM, NAME, TYPE, ATTRS) \
  DEF_BUILTIN (ENUM, "__builtin_" NAME, BUILT_IN_NORMAL, TYPE, TYPE,    \
              false, true, true, ATTRS, false, true)

/* HSAIL saturated conversions.  */

#undef DEF_HSAIL_CVT_ZEROI_SAT_BUILTIN
#define DEF_HSAIL_CVT_ZEROI_SAT_BUILTIN(ENUM, HSAIL_DEST_TYPE, HSAIL_SRC_TYPE, \
  NAME, TYPE, ATTRS) \
  DEF_BUILTIN (ENUM, "__builtin_" NAME, BUILT_IN_NORMAL, TYPE, TYPE,    \
              false, true, true, ATTRS, false, true)

#include "builtins.def"
}

/* Build nodes that would have be created by the C front-end; necessary
   for including builtin-types.def and ultimately builtins.def.  Borrowed
   from lto-lang.c.  */

static void
brig_build_c_type_nodes (void)
{
  gcc_assert (void_type_node);

  void_list_node = build_tree_list (NULL_TREE, void_type_node);
  string_type_node = build_pointer_type (char_type_node);
  const_string_type_node
    = build_pointer_type (build_qualified_type (char_type_node,
						TYPE_QUAL_CONST));

  if (strcmp (SIZE_TYPE, "unsigned int") == 0)
    {
      intmax_type_node = integer_type_node;
      uintmax_type_node = unsigned_type_node;
      signed_size_type_node = integer_type_node;
    }
  else if (strcmp (SIZE_TYPE, "long unsigned int") == 0)
    {
      intmax_type_node = long_integer_type_node;
      uintmax_type_node = long_unsigned_type_node;
      signed_size_type_node = long_integer_type_node;
    }
  else if (strcmp (SIZE_TYPE, "long long unsigned int") == 0)
    {
      intmax_type_node = long_long_integer_type_node;
      uintmax_type_node = long_long_unsigned_type_node;
      signed_size_type_node = long_long_integer_type_node;
    }
  else
    {
      int i;

      signed_size_type_node = NULL_TREE;
      for (i = 0; i < NUM_INT_N_ENTS; i++)
	if (int_n_enabled_p[i])
	  {
	    char name[50], altname[50];
	    sprintf (name, "__int%d unsigned", int_n_data[i].bitsize);
	    sprintf (altname, "__int%d__ unsigned", int_n_data[i].bitsize);

	    if (strcmp (name, SIZE_TYPE) == 0
		|| strcmp (altname, SIZE_TYPE) == 0)
	      {
		intmax_type_node = int_n_trees[i].signed_type;
		uintmax_type_node = int_n_trees[i].unsigned_type;
		signed_size_type_node = int_n_trees[i].signed_type;
	      }
	  }
      if (signed_size_type_node == NULL_TREE)
	gcc_unreachable ();
    }

  wint_type_node = unsigned_type_node;
  pid_type_node = integer_type_node;
}


static bool
brig_langhook_init (void)
{
  build_common_tree_nodes (false);

  /* Builtin initialization related code borrowed from lto-lang.c.  */
  void_list_node = build_tree_list (NULL_TREE, void_type_node);

  brig_build_c_type_nodes ();

  if (TREE_CODE (va_list_type_node) == ARRAY_TYPE)
    {
      tree x = build_pointer_type (TREE_TYPE (va_list_type_node));
      brig_define_builtins (x, x);
    }
  else
    {
      brig_define_builtins (build_reference_type (va_list_type_node),
			    va_list_type_node);
    }

  targetm.init_builtins ();
  build_common_builtin_nodes ();

  return true;
}

#undef LANG_HOOKS_NAME
#undef LANG_HOOKS_INIT
#undef LANG_HOOKS_OPTION_LANG_MASK
#undef LANG_HOOKS_INIT_OPTIONS_STRUCT
#undef LANG_HOOKS_HANDLE_OPTION
#undef LANG_HOOKS_POST_OPTIONS
#undef LANG_HOOKS_PARSE_FILE
#undef LANG_HOOKS_TYPE_FOR_MODE
#undef LANG_HOOKS_TYPE_FOR_SIZE
#undef LANG_HOOKS_REGISTER_BUILTIN_TYPE
#undef LANG_HOOKS_BUILTIN_FUNCTION
#undef LANG_HOOKS_GLOBAL_BINDINGS_P
#undef LANG_HOOKS_PUSHDECL
#undef LANG_HOOKS_GETDECLS
#undef LANG_HOOKS_WRITE_GLOBALS
#undef LANG_HOOKS_GIMPLIFY_EXPR
#undef LANG_HOOKS_EH_PERSONALITY

#define LANG_HOOKS_NAME "GNU Brig"
#define LANG_HOOKS_INIT brig_langhook_init
#define LANG_HOOKS_OPTION_LANG_MASK brig_langhook_option_lang_mask
#define LANG_HOOKS_INIT_OPTIONS_STRUCT brig_langhook_init_options_struct
#define LANG_HOOKS_HANDLE_OPTION brig_langhook_handle_option
#define LANG_HOOKS_POST_OPTIONS brig_langhook_post_options
#define LANG_HOOKS_PARSE_FILE brig_langhook_parse_file
#define LANG_HOOKS_TYPE_FOR_MODE brig_langhook_type_for_mode
#define LANG_HOOKS_TYPE_FOR_SIZE brig_langhook_type_for_size
#define LANG_HOOKS_REGISTER_BUILTIN_TYPE brig_langhook_register_builtin_type
#define LANG_HOOKS_BUILTIN_FUNCTION brig_langhook_builtin_function
#define LANG_HOOKS_GLOBAL_BINDINGS_P brig_langhook_global_bindings_p
#define LANG_HOOKS_PUSHDECL brig_langhook_pushdecl
#define LANG_HOOKS_GETDECLS brig_langhook_getdecls
#define LANG_HOOKS_GIMPLIFY_EXPR brig_langhook_gimplify_expr
#define LANG_HOOKS_EH_PERSONALITY brig_langhook_eh_personality

/* Attribute hooks.  */
#undef LANG_HOOKS_COMMON_ATTRIBUTE_TABLE
#define LANG_HOOKS_COMMON_ATTRIBUTE_TABLE brig_attribute_table

struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

#include "gt-brig-brig-lang.h"
#include "gtype-brig.h"
