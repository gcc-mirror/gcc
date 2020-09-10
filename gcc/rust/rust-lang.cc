#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "tree.h"
#include "gimple-expr.h"
#include "diagnostic.h"
#include "opts.h"
#include "fold-const.h"
#include "gimplify.h"
#include "stor-layout.h"
#include "debug.h"
#include "convert.h"
#include "langhooks.h"
#include "langhooks-def.h"
#include "common/common-target.h"

#include <mpfr.h>
// note: header files must be in this order or else forward declarations don't
// work properly. Kinda dumb system, but have to live with it. clang-format
// seems to mess it up
/* Order: config, system, coretypes, target, tree, gimple-expr, diagnostic,
 * opts, fold-const, gimplify, stor-layout, debug, convert, langhooks,
 * langhooks-def, common-target */

// FIXME: test saving intellisense
#include "options.h"

// version check to stop compiling if c++ isn't c++11 or higher
#if __cplusplus < 201103
#error                                                                         \
  "GCC Rust frontend requires C++11 or higher. You can compile the g++ frontend first and then compile the Rust frontend using that."
#endif
// TODO: is this best way to do it? Is it allowed? (should be)

/* General TODOs:
 *  - convert all copies of expensive-to-copy (deep copy) AST objects into
 * moves, if possible. Don't remove clone functionality - it may be required for e.g. HIR conversion.
 */

#include "rust-system.h"
#include "rust-session-manager.h"

#include "rust-target.h"

// Language-dependent contents of a type. GTY() mark used for garbage collector.
struct GTY (()) lang_type
{
  char dummy;
};

// Language-dependent contents of a decl.
struct GTY (()) lang_decl
{
  char dummy;
};

// Language-dependent contents of an identifier.  This must include a
// tree_identifier.
struct GTY (()) lang_identifier
{
  struct tree_identifier common;
};

// The resulting tree type.
union GTY ((
  desc ("TREE_CODE (&%h.generic) == IDENTIFIER_NODE"),
  chain_next (
    "CODE_CONTAINS_STRUCT (TREE_CODE (&%h.generic), "
    "TS_COMMON) ? ((union lang_tree_node *) TREE_CHAIN (&%h.generic)) : NULL")))
  lang_tree_node
{
  union tree_node GTY ((tag ("0"), desc ("tree_node_structure (&%h)"))) generic;
  struct lang_identifier GTY ((tag ("1"))) identifier;
};

// We don't use language_function.
struct GTY (()) language_function
{
  int dummy;
};

// Kinda HACK-ish - store parsing session as static variable
static Rust::Session session;

// has to be in same compilation unit as session, so here for now
void rust_add_target_info(const char* key, const char* value) {
    session.options.target_data.insert_key_value_pair(key, value);
}

/* Language hooks.  */

/* Initial lang hook called (possibly), used for initialisation.
 * Must call build_common_tree_nodes, set_sizetype, build_common_tree_nodes_2,
 * and build_common_builtin_nodes, as well as set global variable
 * void_list_node. Apparently called after option handling? */
static bool
grs_langhook_init (void)
{
  /* Something to do with this:
   This allows the code in d-builtins.cc to not have to worry about
   converting (C signed char *) to (D char *) for string arguments of
   built-in functions. The parameter (signed_char = false) specifies
   whether char is signed.  */
  build_common_tree_nodes (false);

  // Creates a new TREE_LIST node with purpose NULL_TREE and value
  // void_type_node
  void_list_node = build_tree_list (NULL_TREE, void_type_node);

  // Builds built-ins for middle-end after all front-end built-ins are already
  // instantiated
  build_common_builtin_nodes ();

  mpfr_set_default_prec (128);

  using_eh_for_cleanups ();

  // initialise compiler session
  session.init ();

  return true;
}

/* The option mask (something to do with options for specific frontends or
 * something). */
static unsigned int
grs_langhook_option_lang_mask (void)
{
  return CL_Rust;
}

/* Initialize the options structure. */
static void
grs_langhook_init_options_struct (struct gcc_options *opts)
{
  // nothing yet - used by frontends to change specific options for the language
  session.init_options ();
}

/* Main entry point for front-end, apparently. Finds input file names in global
 * vars in_fnames and num_in_fnames. From this, frontend can take over and do
 * actual parsing and initial compilation. This function must create a complete
 * parse tree in a global var, and then return.
 *
 * Some consider this the "start of compilation". */
static void
grs_langhook_parse_file (void)
{
  fprintf (stderr, "Preparing to parse files. \n");

  session.parse_files (num_in_fnames, in_fnames);
}

/* Seems to get the exact type for a specific type - e.g. for scalar float with
 * 32-bit bitsize, it returns float, and for 64-bit bitsize, it returns double.
 * Used to map RTL nodes to machine modes or something like that. */
static tree
grs_langhook_type_for_mode (machine_mode mode, int unsignedp)
{
  // TODO: change all this later to match rustc types
  if (mode == TYPE_MODE (float_type_node))
    return float_type_node;

  if (mode == TYPE_MODE (double_type_node))
    return double_type_node;

  if (mode == TYPE_MODE (intQI_type_node)) // quarter integer mode - single byte
					   // treated as integer
    return unsignedp ? unsigned_intQI_type_node : intQI_type_node;
  if (mode
      == TYPE_MODE (intHI_type_node)) // half integer mode - two-byte integer
    return unsignedp ? unsigned_intHI_type_node : intHI_type_node;
  if (mode
      == TYPE_MODE (intSI_type_node)) // single integer mode - four-byte integer
    return unsignedp ? unsigned_intSI_type_node : intSI_type_node;
  if (mode
      == TYPE_MODE (
	intDI_type_node)) // double integer mode - eight-byte integer
    return unsignedp ? unsigned_intDI_type_node : intDI_type_node;
  if (mode
      == TYPE_MODE (intTI_type_node)) // tetra integer mode - 16-byte integer
    return unsignedp ? unsigned_intTI_type_node : intTI_type_node;

  if (mode == TYPE_MODE (integer_type_node))
    return unsignedp ? unsigned_type_node : integer_type_node;

  if (mode == TYPE_MODE (long_integer_type_node))
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;

  if (mode == TYPE_MODE (long_long_integer_type_node))
    return unsignedp ? long_long_unsigned_type_node
		     : long_long_integer_type_node;

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

/* This appears to be used for creating different types for different bit sizes
 * (e.g. int and long). Also, the Go frontend calls this from type_for_mode to
 * determine the type from a specific bitsize for integer types.
 * FIXME: change this when working on AST-GENERIC conversion to allow the full
 * range of Rust type sizes. */
static tree
grs_langhook_type_for_size (unsigned int bits ATTRIBUTE_UNUSED,
			    int unsignedp ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
  return NULL_TREE;
  // nothing at the moment, but change later
}

// Record a builtin function. We just ignore builtin functions.
static tree
grs_langhook_builtin_function (tree decl ATTRIBUTE_UNUSED)
{
  return decl;
}

/* Return true if we are in the global binding level (which is never,
 * apparently). */
static bool
grs_langhook_global_bindings_p (void)
{
  // return current_function_decl == NULL_TREE;
  // gcc_unreachable();
  // return true;
  return false;
}

/* Push a declaration into the current binding level.  We can't
   usefully implement this since we don't want to convert from tree
   back to one of our internal data structures.  I think the only way
   this is used is to record a decl which is to be returned by
   getdecls, and we could implement it for that purpose if
   necessary.  */
static tree
grs_langhook_pushdecl (tree decl ATTRIBUTE_UNUSED)
{
  gcc_unreachable ();
  return NULL;
}

/* This hook is used to get the current list of declarations as trees.
   We don't support that; instead we use the write_globals hook.  This
   can't simply crash because it is called by -gstabs.  */
static tree
grs_langhook_getdecls (void)
{
  // gcc_unreachable();
  return NULL;
}

// Handle Rust-specific options. Return false if nothing happened.
static bool
grs_langhook_handle_option (
  size_t scode, const char *arg, HOST_WIDE_INT value, int kind ATTRIBUTE_UNUSED,
  location_t loc ATTRIBUTE_UNUSED,
  const struct cl_option_handlers *handlers ATTRIBUTE_UNUSED)
{
  // Convert integer code to lang.opt enum codes with names.
  enum opt_code code = (enum opt_code) scode;
  // used to store whether results of various stuff are successful
  // bool ret = true;

  // delegate to session manager
  return session.handle_option (code, arg, value, kind, loc, handlers);

  // Handles options as listed in lang.opt.
  /*switch (code) {
      case OPT_I:
	  // TODO: add search path
	  break;
      case OPT_L:
	  // TODO: add library link path or something
	  break;
      case OPT_frust_dump:
	  // enable dump and return whether this was successful
	  ret = rust_enable_dump(arg) ? true : false;
	  break;
      // no option handling for -o
      default:
	  // return 1 to indicate option is valid
	  break;
  }

  return ret;*/
}

/* Run after parsing options.  */
static bool
grs_langhook_post_options (const char **pfilename ATTRIBUTE_UNUSED)
{
  // can be used to override other options if required

  // satisfies an assert in init_excess_precision in toplev.c
  if (flag_excess_precision /*_cmdline*/ == EXCESS_PRECISION_DEFAULT)
    flag_excess_precision /*_cmdline*/ = EXCESS_PRECISION_STANDARD;

  /* Returning false means that the backend should be used.  */
  return false;
}

/* Rust-specific gimplification. May need to gimplify e.g.
 * CALL_EXPR_STATIC_CHAIN */
static int
grs_langhook_gimplify_expr (tree *expr_p ATTRIBUTE_UNUSED,
			    gimple_seq *pre_p ATTRIBUTE_UNUSED,
			    gimple_seq *post_p ATTRIBUTE_UNUSED)
{
  if (TREE_CODE (*expr_p) == CALL_EXPR
      && CALL_EXPR_STATIC_CHAIN (*expr_p) != NULL_TREE)
    gimplify_expr (&CALL_EXPR_STATIC_CHAIN (*expr_p), pre_p, post_p,
		   is_gimple_val, fb_rvalue);
  return GS_UNHANDLED;
}

static tree
grs_langhook_eh_personality (void)
{
  static tree personality_decl;
  if (personality_decl == NULL_TREE)
    {
      personality_decl = build_personality_function ("gccrs");
      rust_preserve_from_gc (personality_decl);
    }
  return personality_decl;
}

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
    case POINTER_TYPE:
      return fold (convert_to_pointer (type, expr));
    case REAL_TYPE:
      return fold (convert_to_real (type, expr));
    case COMPLEX_TYPE:
      return fold (convert_to_complex (type, expr));
    default:
      break;
    }

  gcc_unreachable ();
}

/* FIXME: This is a hack to preserve trees that we create from the
   garbage collector.  */

static GTY (()) tree rust_gc_root;

void
rust_preserve_from_gc (tree t)
{
  rust_gc_root = tree_cons (NULL_TREE, t, rust_gc_root);
}

/* Convert an identifier for use in an error message.  */

const char *
rust_localize_identifier (const char *ident)
{
  return identifier_to_locale (ident);
}

/* The language hooks data structure. This is the main interface between the GCC
 * front-end and the GCC middle-end/back-end. A list of language hooks could be
 * found in <gcc>/langhooks.h
 */
#undef LANG_HOOKS_NAME
#undef LANG_HOOKS_INIT
#undef LANG_HOOKS_OPTION_LANG_MASK
#undef LANG_HOOKS_INIT_OPTIONS_STRUCT
#undef LANG_HOOKS_HANDLE_OPTION
#undef LANG_HOOKS_POST_OPTIONS
#undef LANG_HOOKS_PARSE_FILE
#undef LANG_HOOKS_TYPE_FOR_MODE
#undef LANG_HOOKS_TYPE_FOR_SIZE
#undef LANG_HOOKS_BUILTIN_FUNCTION
#undef LANG_HOOKS_GLOBAL_BINDINGS_P
#undef LANG_HOOKS_PUSHDECL
#undef LANG_HOOKS_GETDECLS
#undef LANG_HOOKS_WRITE_GLOBALS
#undef LANG_HOOKS_GIMPLIFY_EXPR
#undef LANG_HOOKS_EH_PERSONALITY

#define LANG_HOOKS_NAME "GNU Rust"
#define LANG_HOOKS_INIT grs_langhook_init
#define LANG_HOOKS_OPTION_LANG_MASK grs_langhook_option_lang_mask
#define LANG_HOOKS_INIT_OPTIONS_STRUCT grs_langhook_init_options_struct
#define LANG_HOOKS_HANDLE_OPTION grs_langhook_handle_option
#define LANG_HOOKS_POST_OPTIONS grs_langhook_post_options
/* Main lang-hook, apparently. Finds input file names in global vars in_fnames
 * and num_in_fnames From this, frontend can take over and do actual parsing and
 * initial compilation.
 * This hook must create a complete parse tree in a global var, and then return.
 */
#define LANG_HOOKS_PARSE_FILE grs_langhook_parse_file
#define LANG_HOOKS_TYPE_FOR_MODE grs_langhook_type_for_mode
#define LANG_HOOKS_TYPE_FOR_SIZE grs_langhook_type_for_size
#define LANG_HOOKS_BUILTIN_FUNCTION grs_langhook_builtin_function
#define LANG_HOOKS_GLOBAL_BINDINGS_P grs_langhook_global_bindings_p
#define LANG_HOOKS_PUSHDECL grs_langhook_pushdecl
#define LANG_HOOKS_GETDECLS grs_langhook_getdecls
#define LANG_HOOKS_WRITE_GLOBALS grs_langhook_write_globals
#define LANG_HOOKS_GIMPLIFY_EXPR grs_langhook_gimplify_expr
#define LANG_HOOKS_EH_PERSONALITY grs_langhook_eh_personality

// Expands all LANG_HOOKS_x of GCC
struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

// These are for GCC's garbage collector to work properly or something
#include "gt-rust-rust-lang.h"
#include "gtype-rust.h"
