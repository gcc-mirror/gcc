/* Java(TM) language-specific utility routines.
   Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005
   Free Software Foundation, Inc.

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
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

/* Hacked by Per Bothner <bothner@cygnus.com> February 1996. */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "input.h"
#include "rtl.h"
#include "expr.h"
#include "java-tree.h"
#include "jcf.h"
#include "toplev.h"
#include "langhooks.h"
#include "langhooks-def.h"
#include "flags.h"
#include "ggc.h"
#include "diagnostic.h"
#include "tree-inline.h"
#include "splay-tree.h"
#include "tree-dump.h"
#include "opts.h"
#include "options.h"

static bool java_init (void);
static void java_finish (void);
static unsigned int java_init_options (unsigned int, const char **);
static bool java_post_options (const char **);

static int java_handle_option (size_t scode, const char *arg, int value);
static void put_decl_string (const char *, int);
static void put_decl_node (tree);
static void java_print_error_function (diagnostic_context *, const char *);
static tree java_tree_inlining_walk_subtrees (tree *, int *, walk_tree_fn,
					      void *, struct pointer_set_t *);
static int merge_init_test_initialization (void * *, void *);
static int inline_init_test_initialization (void * *, void *);
static bool java_can_use_bit_fields_p (void);
static bool java_dump_tree (void *, tree);
static void dump_compound_expr (dump_info_p, tree);
static bool java_decl_ok_for_sibcall (tree);
static tree java_get_callee_fndecl (tree);
static void java_clear_binding_stack (void);

#ifndef TARGET_OBJECT_SUFFIX
# define TARGET_OBJECT_SUFFIX ".o"
#endif

/* Table indexed by tree code giving a string containing a character
   classifying the tree code.  Possibilities are
   t, d, s, c, r, <, 1 and 2.  See java/java-tree.def for details.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) TYPE,

const enum tree_code_class tree_code_type[] = {
#include "tree.def"
  tcc_exceptional,
#include "java-tree.def"
};
#undef DEFTREECODE

/* Table indexed by tree code giving number of expression
   operands beyond the fixed part of the node structure.
   Not used for types or decls.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) LENGTH,

const unsigned char tree_code_length[] = {
#include "tree.def"
  0,
#include "java-tree.def"
};
#undef DEFTREECODE

/* Names of tree components.
   Used for printing out the tree and error messages.  */
#define DEFTREECODE(SYM, NAME, TYPE, LEN) NAME,

const char *const tree_code_name[] = {
#include "tree.def"
  "@@dummy",
#include "java-tree.def"
};
#undef DEFTREECODE

/* Used to avoid printing error messages with bogus function
   prototypes.  Starts out false.  */
static bool inhibit_error_function_printing;

int compiling_from_source;

const char *resource_name;

/* When nonzero, -Wall was turned on.  */
int flag_wall = 0;

/* The encoding of the source file.  */
const char *current_encoding = NULL;

/* When nonzero, report use of deprecated classes, methods, or fields.  */
int flag_deprecated = 1;

/* When zero, don't optimize static class initialization. This flag shouldn't
   be tested alone, use STATIC_CLASS_INITIALIZATION_OPTIMIZATION_P instead.  */
/* FIXME: Make this work with gimplify.  */
/* int flag_optimize_sci = 0;  */

/* Don't attempt to verify invocations.  */
int flag_verify_invocations = 0; 

/* When nonzero, print extra version information.  */
static int v_flag = 0;

JCF *current_jcf;

/* Variable controlling how dependency tracking is enabled in
   java_init.  */
static int dependency_tracking = 0;

/* Flag values for DEPENDENCY_TRACKING.  */
#define DEPEND_SET_FILE 1
#define DEPEND_ENABLE   2
#define DEPEND_TARGET_SET 4
#define DEPEND_FILE_ALREADY_SET 8

struct language_function GTY(())
{
  int unused;
};

#undef LANG_HOOKS_NAME
#define LANG_HOOKS_NAME "GNU Java"
#undef LANG_HOOKS_INIT
#define LANG_HOOKS_INIT java_init
#undef LANG_HOOKS_FINISH
#define LANG_HOOKS_FINISH java_finish
#undef LANG_HOOKS_INIT_OPTIONS
#define LANG_HOOKS_INIT_OPTIONS java_init_options
#undef LANG_HOOKS_HANDLE_OPTION
#define LANG_HOOKS_HANDLE_OPTION java_handle_option
#undef LANG_HOOKS_POST_OPTIONS
#define LANG_HOOKS_POST_OPTIONS java_post_options
#undef LANG_HOOKS_PARSE_FILE
#define LANG_HOOKS_PARSE_FILE java_parse_file
#undef LANG_HOOKS_MARK_ADDRESSABLE
#define LANG_HOOKS_MARK_ADDRESSABLE java_mark_addressable
#undef LANG_HOOKS_DUP_LANG_SPECIFIC_DECL
#define LANG_HOOKS_DUP_LANG_SPECIFIC_DECL java_dup_lang_specific_decl
#undef LANG_HOOKS_DECL_PRINTABLE_NAME
#define LANG_HOOKS_DECL_PRINTABLE_NAME lang_printable_name
#undef LANG_HOOKS_PRINT_ERROR_FUNCTION
#define LANG_HOOKS_PRINT_ERROR_FUNCTION	java_print_error_function
#undef LANG_HOOKS_CAN_USE_BIT_FIELDS_P
#define LANG_HOOKS_CAN_USE_BIT_FIELDS_P java_can_use_bit_fields_p

#undef LANG_HOOKS_TYPE_FOR_MODE
#define LANG_HOOKS_TYPE_FOR_MODE java_type_for_mode
#undef LANG_HOOKS_TYPE_FOR_SIZE
#define LANG_HOOKS_TYPE_FOR_SIZE java_type_for_size
#undef LANG_HOOKS_SIGNED_TYPE
#define LANG_HOOKS_SIGNED_TYPE java_signed_type
#undef LANG_HOOKS_UNSIGNED_TYPE
#define LANG_HOOKS_UNSIGNED_TYPE java_unsigned_type
#undef LANG_HOOKS_SIGNED_OR_UNSIGNED_TYPE
#define LANG_HOOKS_SIGNED_OR_UNSIGNED_TYPE java_signed_or_unsigned_type

#undef LANG_HOOKS_TREE_DUMP_DUMP_TREE_FN
#define LANG_HOOKS_TREE_DUMP_DUMP_TREE_FN java_dump_tree

#undef LANG_HOOKS_GIMPLIFY_EXPR
#define LANG_HOOKS_GIMPLIFY_EXPR java_gimplify_expr

#undef LANG_HOOKS_TREE_INLINING_WALK_SUBTREES
#define LANG_HOOKS_TREE_INLINING_WALK_SUBTREES java_tree_inlining_walk_subtrees

#undef LANG_HOOKS_DECL_OK_FOR_SIBCALL
#define LANG_HOOKS_DECL_OK_FOR_SIBCALL java_decl_ok_for_sibcall

#undef LANG_HOOKS_GET_CALLEE_FNDECL
#define LANG_HOOKS_GET_CALLEE_FNDECL java_get_callee_fndecl

#undef LANG_HOOKS_CALLGRAPH_EXPAND_FUNCTION
#define LANG_HOOKS_CALLGRAPH_EXPAND_FUNCTION java_expand_body

#undef LANG_HOOKS_CLEAR_BINDING_STACK
#define LANG_HOOKS_CLEAR_BINDING_STACK java_clear_binding_stack

#undef LANG_HOOKS_SET_DECL_ASSEMBLER_NAME
#define LANG_HOOKS_SET_DECL_ASSEMBLER_NAME java_mangle_decl

/* Each front end provides its own.  */
const struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

/*
 * process java-specific compiler command-line options
 * return 0, but do not complain if the option is not recognized.
 */
static int
java_handle_option (size_t scode, const char *arg, int value)
{
  enum opt_code code = (enum opt_code) scode;

  switch (code)
    {
    case OPT_I:
      jcf_path_include_arg (arg);
      break;

    case OPT_M:
      jcf_dependency_init (1);
      dependency_tracking |= DEPEND_ENABLE;
      break;

    case OPT_MD_:
      jcf_dependency_init (1);
      dependency_tracking |= DEPEND_SET_FILE | DEPEND_ENABLE;
      break;

    case OPT_MF:
      jcf_dependency_set_dep_file (arg);
      dependency_tracking |= DEPEND_FILE_ALREADY_SET;
      break;

    case OPT_MM:
      jcf_dependency_init (0);
      dependency_tracking |= DEPEND_ENABLE;
      break;

    case OPT_MMD_:
      jcf_dependency_init (0);
      dependency_tracking |= DEPEND_SET_FILE | DEPEND_ENABLE;
      break;

    case OPT_MP:
      jcf_dependency_print_dummies ();
      break;

    case OPT_MT:
      jcf_dependency_set_target (arg);
      dependency_tracking |= DEPEND_TARGET_SET;
      break;

    case OPT_Wall:
      flag_wall = value;
      flag_redundant = value;
      flag_extraneous_semicolon = value;
      /* When -Wall given, enable -Wunused.  We do this because the C
	 compiler does it, and people expect it.  */
      set_Wunused (value);
      break;

    case OPT_fenable_assertions_:
      add_enable_assert (arg, value);
      break;

    case OPT_fenable_assertions:
      add_enable_assert ("", value);
      break;

    case OPT_fdisable_assertions_:
      add_enable_assert (arg, !value);
      break;

    case OPT_fdisable_assertions:
      add_enable_assert ("", !value);
      break;

    case OPT_fassume_compiled_:
      add_assume_compiled (arg, !value);
      break;

    case OPT_fassume_compiled:
      add_assume_compiled ("", !value);
      break;

    case OPT_fbootclasspath_:
      jcf_path_bootclasspath_arg (arg);
      break;

    case OPT_fclasspath_:
    case OPT_fCLASSPATH_:
      jcf_path_classpath_arg (arg);
      break;

    case OPT_fcompile_resource_:
      resource_name = arg;
      break;

    case OPT_fdump_:
      if (!dump_switch_p (arg))
	return 0;
      break;

    case OPT_fencoding_:
      current_encoding = arg;
      break;

    case OPT_fextdirs_:
      jcf_path_extdirs_arg (arg);
      break;

    case OPT_foutput_class_dir_:
      jcf_write_base_directory = arg;
      break;

    case OPT_version:
      v_flag = 1;
      break;
      
    default:
      if (cl_options[code].flags & CL_Java)
	break;
      abort();
    }

  return 1;
}

/* Global open file.  */
FILE *finput;

static bool
java_init (void)
{
#if 0
  extern int flag_minimal_debug;
  flag_minimal_debug = 0;
#endif

  /* FIXME: Indirect dispatch isn't yet compatible with static class
     init optimization.  */
  if (flag_indirect_dispatch)
    always_initialize_class_p = true;

  /* Force minimum function alignment if g++ uses the least significant
     bit of function pointers to store the virtual bit. This is required
     to keep vtables compatible.  */
  if (TARGET_PTRMEMFUNC_VBIT_LOCATION == ptrmemfunc_vbit_in_pfn
      && force_align_functions_log < 1)
    force_align_functions_log = 1;

  jcf_path_seal (v_flag);

  java_init_decl_processing ();

  using_eh_for_cleanups ();

  return true;
}

static void
java_finish (void)
{
  jcf_dependency_write ();
}

/* Buffer used by lang_printable_name. */
static char *decl_buf = NULL;

/* Allocated size of decl_buf. */
static int decl_buflen = 0;

/* Length of used part of decl_buf;  position for next character. */
static int decl_bufpos = 0;

/* Append the string STR to decl_buf.
   It length is given by LEN;  -1 means the string is nul-terminated. */

static void
put_decl_string (const char *str, int len)
{
  if (len < 0)
    len = strlen (str);
  if (decl_bufpos + len >= decl_buflen)
    {
      if (decl_buf == NULL)
	{
	  decl_buflen = len + 100;
	  decl_buf = XNEWVEC (char, decl_buflen);
	}
      else
	{
	  decl_buflen *= 2;
	  decl_buf = xrealloc (decl_buf, decl_buflen);
	}
    }
  strcpy (decl_buf + decl_bufpos, str);
  decl_bufpos += len;
}

/* Append to decl_buf a printable name for NODE. */

static void
put_decl_node (tree node)
{
  int was_pointer = 0;
  if (TREE_CODE (node) == POINTER_TYPE)
    {
      node = TREE_TYPE (node);
      was_pointer = 1;
    }
  if (DECL_P (node) && DECL_NAME (node) != NULL_TREE)
    {
      if (TREE_CODE (node) == FUNCTION_DECL)
	{
	  /* We want to print the type the DECL belongs to. We don't do
	     that when we handle constructors. */
	  if (! DECL_CONSTRUCTOR_P (node)
	      && ! DECL_ARTIFICIAL (node) && DECL_CONTEXT (node))
	    {
	      put_decl_node (TYPE_NAME (DECL_CONTEXT (node)));
	      put_decl_string (".", 1);
	    }
	  if (! DECL_CONSTRUCTOR_P (node))
	    put_decl_node (DECL_NAME (node));
	  if (TREE_TYPE (node) != NULL_TREE)
	    {
	      int i = 0;
	      tree args = TYPE_ARG_TYPES (TREE_TYPE (node));
	      if (TREE_CODE (TREE_TYPE (node)) == METHOD_TYPE)
		args = TREE_CHAIN (args);
	      put_decl_string ("(", 1);
	      for ( ; args != end_params_node;  args = TREE_CHAIN (args), i++)
		{
		  if (i > 0)
		    put_decl_string (",", 1);
		  put_decl_node (TREE_VALUE (args));
		}
	      put_decl_string (")", 1);
	    }
	}
      else
	put_decl_node (DECL_NAME (node));
    }
  else if (TYPE_P (node) && TYPE_NAME (node) != NULL_TREE)
    {
      if (TREE_CODE (node) == RECORD_TYPE && TYPE_ARRAY_P (node))
	{
	  put_decl_node (TYPE_ARRAY_ELEMENT (node));
	  put_decl_string("[]", 2);
	}
      else if (node == promoted_byte_type_node)
	put_decl_string ("byte", 4);
      else if (node == promoted_short_type_node)
	put_decl_string ("short", 5);
      else if (node == promoted_char_type_node)
	put_decl_string ("char", 4);
      else if (node == promoted_boolean_type_node)
	put_decl_string ("boolean", 7);
      else if (node == void_type_node && was_pointer)
	put_decl_string ("null", 4);
      else
	put_decl_node (TYPE_NAME (node));
    }
  else if (TREE_CODE (node) == IDENTIFIER_NODE)
    put_decl_string (IDENTIFIER_POINTER (node), IDENTIFIER_LENGTH (node));
  else
    put_decl_string ("<unknown>", -1);
}

/* Return a user-friendly name for DECL.
   The resulting string is only valid until the next call.
   The value of the hook decl_printable_name is this function,
   which is also called directly by java_print_error_function. */

const char *
lang_printable_name (tree decl, int v)
{
  decl_bufpos = 0;
  if (v == 0 && TREE_CODE (decl) == FUNCTION_DECL)
    put_decl_node (DECL_NAME (decl));
  else
    put_decl_node (decl);
  put_decl_string ("", 1);
  return decl_buf;
}

/* Print on stderr the current class and method context.  This function
   is the value of the hook print_error_function. */

static GTY(()) tree last_error_function_context;
static GTY(()) tree last_error_function;
static void
java_print_error_function (diagnostic_context *context ATTRIBUTE_UNUSED,
			   const char *file)
{
  /* Don't print error messages with bogus function prototypes.  */
  if (inhibit_error_function_printing)
    return;

  if (current_function_decl != NULL
      && DECL_CONTEXT (current_function_decl) != last_error_function_context)
    {
      if (file)
	fprintf (stderr, "%s: ", file);

      last_error_function_context = DECL_CONTEXT (current_function_decl);
      fprintf (stderr, "In class '%s':\n",
	       lang_printable_name (last_error_function_context, 0));
    }
  if (last_error_function != current_function_decl)
    {
      if (file)
	fprintf (stderr, "%s: ", file);

      if (current_function_decl == NULL)
	fprintf (stderr, "At top level:\n");
      else
	{
	  const char *name = lang_printable_name (current_function_decl, 2);
	  fprintf (stderr, "In %s '%s':\n",
		   (DECL_CONSTRUCTOR_P (current_function_decl) ? "constructor"
		    : "method"),
		   name);
	}

      last_error_function = current_function_decl;
    }

}

/* Called to install the PRINT_ERROR_FUNCTION hook differently
   according to LEVEL. LEVEL is 1 during early parsing, when function
   prototypes aren't fully resolved. java_print_error_function is set
   so it doesn't print incomplete function prototypes. When LEVEL is
   2, function prototypes are fully resolved and can be printed when
   reporting errors.  */

void
lang_init_source (int level)
{
  inhibit_error_function_printing = (level == 1);
}

static unsigned int
java_init_options (unsigned int argc ATTRIBUTE_UNUSED,
		   const char **argv ATTRIBUTE_UNUSED)
{
  flag_bounds_check = 1;
  flag_exceptions = 1;
  flag_non_call_exceptions = 1;

  /* In Java floating point operations never trap.  */
  flag_trapping_math = 0;

  /* In Java arithmetic overflow always wraps around.  */
  flag_wrapv = 1;

  /* Java requires left-to-right evaluation of subexpressions.  */
  flag_evaluation_order = 1;

  /* Unit at a time is disabled for Java because it is considered
     too expensive.  */
  no_unit_at_a_time_default = 1;

  jcf_path_init ();

  return CL_Java;
}

static bool
java_can_use_bit_fields_p (void)
{
  /* The bit-field optimizations cause problems when generating class
     files.  */
  return flag_emit_class_files ? false : true;
}

/* Post-switch processing.  */
static bool
java_post_options (const char **pfilename)
{
  const char *filename = *pfilename;

  /* Use tree inlining.  */
  if (!flag_no_inline)
    flag_no_inline = 1;
  if (flag_inline_functions)
    flag_inline_trees = 2;

  /* An absolute requirement: if we're not using indirect dispatch, we
     must always verify everything.  */
  if (! flag_indirect_dispatch)
    flag_verify_invocations = true;

  /* Open input file.  */

  if (filename == 0 || !strcmp (filename, "-"))
    {
      finput = stdin;
      filename = "stdin";

      if (dependency_tracking)
	error ("can't do dependency tracking with input from stdin");
    }
  else
    {
      if (dependency_tracking)
	{
	  char *dot;

	  /* If the target is set and the output filename is set, then
	     there's no processing to do here.  Otherwise we must
	     compute one or the other.  */
	  if (! ((dependency_tracking & DEPEND_TARGET_SET)
		 && (dependency_tracking & DEPEND_FILE_ALREADY_SET)))
	    {
	      dot = strrchr (filename, '.');
	      if (dot == NULL)
		error ("couldn't determine target name for dependency tracking");
	      else
		{
		  char *buf = XNEWVEC (char, dot - filename +
				       3 + sizeof (TARGET_OBJECT_SUFFIX));
		  strncpy (buf, filename, dot - filename);

		  /* If emitting class files, we might have multiple
		     targets.  The class generation code takes care of
		     registering them.  Otherwise we compute the
		     target name here.  */
		  if ((dependency_tracking & DEPEND_TARGET_SET))
		    ; /* Nothing.  */
		  else if (flag_emit_class_files)
		    jcf_dependency_set_target (NULL);
		  else
		    {
		      strcpy (buf + (dot - filename), TARGET_OBJECT_SUFFIX);
		      jcf_dependency_set_target (buf);
		    }

		  if ((dependency_tracking & DEPEND_FILE_ALREADY_SET))
		    ; /* Nothing.  */
		  else if ((dependency_tracking & DEPEND_SET_FILE))
		    {
		      strcpy (buf + (dot - filename), ".d");
		      jcf_dependency_set_dep_file (buf);
		    }
		  else
		    jcf_dependency_set_dep_file ("-");

		  free (buf);
		}
	    }
	}
    }
#ifdef USE_MAPPED_LOCATION
  linemap_add (&line_table, LC_ENTER, false, filename, 0);
  linemap_add (&line_table, LC_RENAME, false, "<built-in>", 0);
#endif

  /* Initialize the compiler back end.  */
  return false;
}

/* Return either DECL or its known constant value (if it has one).  */

tree
decl_constant_value (tree decl)
{
  if (/* Don't change a variable array bound or initial value to a constant
	 in a place where a variable is invalid.  */
      current_function_decl != 0
      && ! TREE_THIS_VOLATILE (decl)
      && TREE_READONLY (decl)
      && DECL_INITIAL (decl) != 0
      && TREE_CODE (DECL_INITIAL (decl)) != ERROR_MARK
      /* This is invalid if initial value is not constant.
	 If it has either a function call, a memory reference,
	 or a variable, then re-evaluating it could give different results.  */
      && TREE_CONSTANT (DECL_INITIAL (decl))
      /* Check for cases where this is sub-optimal, even though valid.  */
      && TREE_CODE (DECL_INITIAL (decl)) != CONSTRUCTOR)
    return DECL_INITIAL (decl);
  return decl;
}

/* Walk the language specific tree nodes during inlining.  */

static tree
java_tree_inlining_walk_subtrees (tree *tp ATTRIBUTE_UNUSED,
				  int *subtrees ATTRIBUTE_UNUSED,
				  walk_tree_fn func ATTRIBUTE_UNUSED,
				  void *data ATTRIBUTE_UNUSED,
				  struct pointer_set_t *pset ATTRIBUTE_UNUSED)
{
  enum tree_code code;
  tree result;

#define WALK_SUBTREE(NODE)				\
  do							\
    {							\
      result = walk_tree (&(NODE), func, data, pset);	\
      if (result)					\
	return result;					\
    }							\
  while (0)

  tree t = *tp;
  if (!t)
    return NULL_TREE;

  code = TREE_CODE (t);
  switch (code)
    {
    case BLOCK:
      WALK_SUBTREE (BLOCK_EXPR_BODY (t));
      return NULL_TREE;

    case EXIT_BLOCK_EXPR:
      *subtrees = 0;
      return NULL_TREE;

    default:
      return NULL_TREE;
    }

  #undef WALK_SUBTREE
}

/* Every call to a static constructor has an associated boolean
   variable which is in the outermost scope of the calling method.
   This variable is used to avoid multiple calls to the static
   constructor for each class.

   It looks something like this:

   foo ()
   {
      boolean dummy = OtherClass.is_initialized;

     ...

     if (! dummy)
       OtherClass.initialize();

     ... use OtherClass.data ...
   }

   Each of these boolean variables has an entry in the
   DECL_FUNCTION_INIT_TEST_TABLE of a method.  When inlining a method
   we must merge the DECL_FUNCTION_INIT_TEST_TABLE from the function
   being inlined and create the boolean variables in the outermost
   scope of the method being inlined into.  */

/* Create a mapping from a boolean variable in a method being inlined
   to one in the scope of the method being inlined into.  */

static int
merge_init_test_initialization (void **entry, void *x)
{
  struct treetreehash_entry *ite = (struct treetreehash_entry *) *entry;
  splay_tree decl_map = (splay_tree)x;
  splay_tree_node n;
  tree *init_test_decl;

  /* See if we have remapped this declaration.  If we haven't there's
     a bug in the inliner.  */
  n = splay_tree_lookup (decl_map, (splay_tree_key) ite->value);
  if (! n)
    abort ();

  /* Create a new entry for the class and its remapped boolean
     variable.  If we already have a mapping for this class we've
     already initialized it, so don't overwrite the value.  */
  init_test_decl = java_treetreehash_new
    (DECL_FUNCTION_INIT_TEST_TABLE (current_function_decl), ite->key);
  if (!*init_test_decl)
    *init_test_decl = (tree)n->value;

  /* This fixes a weird case.

  The front end assumes that once we have called a method that
  initializes some class, we can assume the class is initialized.  It
  does this by setting the DECL_INITIAL of the init_test_decl for that
  class, and no initializations are emitted for that class.

  However, what if the method that is supposed to do the initialization
  is itself inlined in the caller?  When expanding the called method
  we'll assume that the class initialization has already been done,
  because the DECL_INITIAL of the init_test_decl is set.

  To fix this we remove the DECL_INITIAL (in the caller scope) of all
  the init_test_decls corresponding to classes initialized by the
  inlined method.  This makes the caller no longer assume that the
  method being inlined does any class initializations.  */
  DECL_INITIAL (*init_test_decl) = NULL;

  return true;
}

/* Merge the DECL_FUNCTION_INIT_TEST_TABLE from the function we're
   inlining.  */

void
java_inlining_merge_static_initializers (tree fn, void *decl_map)
{
  htab_traverse
    (DECL_FUNCTION_INIT_TEST_TABLE (fn),
     merge_init_test_initialization, decl_map);
}

/* Lookup a DECL_FUNCTION_INIT_TEST_TABLE entry in the method we're
   inlining into.  If we already have a corresponding entry in that
   class we don't need to create another one, so we create a mapping
   from the variable in the inlined class to the corresponding
   pre-existing one.  */

static int
inline_init_test_initialization (void **entry, void *x)
{
  struct treetreehash_entry *ite = (struct treetreehash_entry *) *entry;
  splay_tree decl_map = (splay_tree)x;

  tree h = java_treetreehash_find
    (DECL_FUNCTION_INIT_TEST_TABLE (current_function_decl), ite->key);
  if (! h)
    return true;
  splay_tree_insert (decl_map,
		     (splay_tree_key) ite->value,
		     (splay_tree_value) h);
  return true;
}

/* Look up the boolean variables in the DECL_FUNCTION_INIT_TEST_TABLE
   of a method being inlined.  For each hone, if we already have a
   variable associated with the same class in the method being inlined
   into, create a new mapping for it.  */

void
java_inlining_map_static_initializers (tree fn, void *decl_map)
{
  htab_traverse
    (DECL_FUNCTION_INIT_TEST_TABLE (fn),
     inline_init_test_initialization, decl_map);
}

/* Avoid voluminous output for deep recursion of compound exprs.  */

static void
dump_compound_expr (dump_info_p di, tree t)
{
  int i;

  for (i=0; i<2; i++)
    {
      switch (TREE_CODE (TREE_OPERAND (t, i)))
	{
	case COMPOUND_EXPR:
	  dump_compound_expr (di, TREE_OPERAND (t, i));
	  break;

	case EXPR_WITH_FILE_LOCATION:
	    {
	      tree wfl_node = EXPR_WFL_NODE (TREE_OPERAND (t, i));
	      dump_child ("expr", wfl_node);
	      break;
	    }

	default:
	  dump_child ("expr", TREE_OPERAND (t, i));
	}
    }
}

static bool
java_dump_tree (void *dump_info, tree t)
{
  enum tree_code code;
  dump_info_p di = (dump_info_p) dump_info;

  /* Figure out what kind of node this is.  */
  code = TREE_CODE (t);

  switch (code)
    {
    case FUNCTION_DECL:
      dump_child ("args", DECL_ARGUMENTS (t));
      if (DECL_EXTERNAL (t))
	dump_string (di, "undefined");
      if (TREE_PUBLIC (t))
	dump_string (di, "extern");
      else
	dump_string (di, "static");
      if (DECL_LANG_SPECIFIC (t))
	dump_child ("body", DECL_FUNCTION_BODY (t));
      if (DECL_LANG_SPECIFIC (t) && !dump_flag (di, TDF_SLIM, t))
	dump_child ("inline body", DECL_SAVED_TREE (t));
      return true;

    case RETURN_EXPR:
      dump_child ("expr", TREE_OPERAND (t, 0));
      return true;

    case GOTO_EXPR:
      dump_child ("goto", TREE_OPERAND (t, 0));
      return true;

    case LABEL_EXPR:
      dump_child ("label", TREE_OPERAND (t, 0));
      return true;

    case LABELED_BLOCK_EXPR:
      dump_child ("label", LABELED_BLOCK_LABEL (t));
      dump_child ("block", LABELED_BLOCK_BODY (t));
      return true;

    case EXIT_BLOCK_EXPR:
      dump_child ("block", EXIT_BLOCK_LABELED_BLOCK (t));
      return true;

    case BLOCK:
      if (BLOCK_EXPR_BODY (t))
	{
	  tree local = BLOCK_VARS (t);
	  while (local)
	    {
	      tree next = TREE_CHAIN (local);
	      dump_child ("var", local);
	      local = next;
	    }

	  {
	    tree block = BLOCK_EXPR_BODY (t);
	    dump_child ("body", block);
	    block = TREE_CHAIN (block);
	  }
	}
      return true;

    case COMPOUND_EXPR:
      if (!dump_flag (di, TDF_SLIM, t))
	return false;
      dump_compound_expr (di, t);
      return true;

    default:
      break;
    }
  return false;
}

/* Java calls can't, in general, be sibcalls because we need an
   accurate stack trace in order to guarantee correct operation of
   methods such as Class.forName(String) and
   SecurityManager.getClassContext().  */

static bool
java_decl_ok_for_sibcall (tree decl)
{
  return decl != NULL && DECL_CONTEXT (decl) == output_class;
}

/* Given a call_expr, try to figure out what its target might be.  In
   the case of an indirection via the atable, search for the decl.  If
   the decl is external, we return NULL.  If we don't, the optimizer
   will replace the indirection with a direct call, which undoes the
   purpose of the atable indirection.  */
static tree
java_get_callee_fndecl (tree call_expr)
{
  tree method, table, element, atable_methods;

  HOST_WIDE_INT index;

  /* FIXME: This is disabled because we end up passing calls through
     the PLT, and we do NOT want to do that.  */
  return NULL;

  if (TREE_CODE (call_expr) != CALL_EXPR)
    return NULL;
  method = TREE_OPERAND (call_expr, 0);
  STRIP_NOPS (method);
  if (TREE_CODE (method) != ARRAY_REF)
    return NULL;
  table = TREE_OPERAND (method, 0);
  if (! DECL_LANG_SPECIFIC(table)
      || !DECL_OWNER (table)
      || TYPE_ATABLE_DECL (DECL_OWNER (table)) != table)
    return NULL;

  atable_methods = TYPE_ATABLE_METHODS (DECL_OWNER (table));
  index = TREE_INT_CST_LOW (TREE_OPERAND (method, 1));

  /* FIXME: Replace this for loop with a hash table lookup.  */
  for (element = atable_methods; element; element = TREE_CHAIN (element))
    {
      if (index == 1)
	{
	  tree purpose = TREE_PURPOSE (element);
	  if (TREE_CODE (purpose) == FUNCTION_DECL
	      && ! DECL_EXTERNAL (purpose))
	    return purpose;
	  else
	    return NULL;
	}
      --index;
    }

  return NULL;
}


/* Clear the binding stack.  */
static void
java_clear_binding_stack (void)
{
  while (!global_bindings_p ())
    poplevel (0, 0, 0);
}

#include "gt-java-lang.h"
