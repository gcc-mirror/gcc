/* Java(TM) language-specific utility routines.
   Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
   2005, 2006, 2007, 2008, 2010 Free Software Foundation, Inc.

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
<http://www.gnu.org/licenses/>.

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
#include "java-tree.h"
#include "jcf.h"
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
#include "target.h"

static bool java_init (void);
static void java_finish (void);
static unsigned int java_option_lang_mask (void);
static void java_init_options_struct (struct gcc_options *);
static void java_init_options (unsigned int, struct cl_decoded_option *);
static bool java_post_options (const char **);

static bool java_handle_option (size_t, const char *, int, int, location_t,
				const struct cl_option_handlers *);
static void put_decl_string (const char *, int);
static void put_decl_node (tree, int);
static void java_print_error_function (diagnostic_context *, const char *,
				       diagnostic_info *);
static int merge_init_test_initialization (void * *, void *);
static int inline_init_test_initialization (void * *, void *);
static bool java_dump_tree (void *, tree);
static void dump_compound_expr (dump_info_p, tree);
static bool java_decl_ok_for_sibcall (const_tree);

static enum classify_record java_classify_record (tree type);

static tree java_eh_personality (void);

#ifndef TARGET_OBJECT_SUFFIX
# define TARGET_OBJECT_SUFFIX ".o"
#endif

/* Table of machine-independent attributes.  */
const struct attribute_spec java_attribute_table[] =
{
 { "nonnull",                0, -1, false, true, true,
			      NULL, false },
  { NULL,                     0, 0, false, false, false, NULL, false }
};

/* Used to avoid printing error messages with bogus function
   prototypes.  Starts out false.  */
static bool inhibit_error_function_printing;

const char *resource_name;

/* When nonzero, -Wall was turned on.  */
int flag_wall = 0;

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

struct GTY(()) language_function {
  int unused;
};

#undef LANG_HOOKS_NAME
#define LANG_HOOKS_NAME "GNU Java"
#undef LANG_HOOKS_INIT
#define LANG_HOOKS_INIT java_init
#undef LANG_HOOKS_FINISH
#define LANG_HOOKS_FINISH java_finish
#undef LANG_HOOKS_OPTION_LANG_MASK
#define LANG_HOOKS_OPTION_LANG_MASK java_option_lang_mask
#undef LANG_HOOKS_INIT_OPTIONS_STRUCT
#define LANG_HOOKS_INIT_OPTIONS_STRUCT java_init_options_struct
#undef LANG_HOOKS_INIT_OPTIONS
#define LANG_HOOKS_INIT_OPTIONS java_init_options
#undef LANG_HOOKS_HANDLE_OPTION
#define LANG_HOOKS_HANDLE_OPTION java_handle_option
#undef LANG_HOOKS_POST_OPTIONS
#define LANG_HOOKS_POST_OPTIONS java_post_options
#undef LANG_HOOKS_PARSE_FILE
#define LANG_HOOKS_PARSE_FILE java_parse_file
#undef LANG_HOOKS_DUP_LANG_SPECIFIC_DECL
#define LANG_HOOKS_DUP_LANG_SPECIFIC_DECL java_dup_lang_specific_decl
#undef LANG_HOOKS_DECL_PRINTABLE_NAME
#define LANG_HOOKS_DECL_PRINTABLE_NAME lang_printable_name
#undef LANG_HOOKS_PRINT_ERROR_FUNCTION
#define LANG_HOOKS_PRINT_ERROR_FUNCTION	java_print_error_function
#undef LANG_HOOKS_WRITE_GLOBALS
#define LANG_HOOKS_WRITE_GLOBALS java_write_globals

#undef LANG_HOOKS_TYPE_FOR_MODE
#define LANG_HOOKS_TYPE_FOR_MODE java_type_for_mode
#undef LANG_HOOKS_TYPE_FOR_SIZE
#define LANG_HOOKS_TYPE_FOR_SIZE java_type_for_size
#undef LANG_HOOKS_CLASSIFY_RECORD
#define LANG_HOOKS_CLASSIFY_RECORD java_classify_record

#undef LANG_HOOKS_TREE_DUMP_DUMP_TREE_FN
#define LANG_HOOKS_TREE_DUMP_DUMP_TREE_FN java_dump_tree

#undef LANG_HOOKS_GIMPLIFY_EXPR
#define LANG_HOOKS_GIMPLIFY_EXPR java_gimplify_expr

#undef LANG_HOOKS_DECL_OK_FOR_SIBCALL
#define LANG_HOOKS_DECL_OK_FOR_SIBCALL java_decl_ok_for_sibcall

#undef LANG_HOOKS_SET_DECL_ASSEMBLER_NAME
#define LANG_HOOKS_SET_DECL_ASSEMBLER_NAME java_mangle_decl

#undef LANG_HOOKS_ATTRIBUTE_TABLE
#define LANG_HOOKS_ATTRIBUTE_TABLE java_attribute_table

#undef LANG_HOOKS_EH_PERSONALITY
#define LANG_HOOKS_EH_PERSONALITY java_eh_personality

#undef LANG_HOOKS_EH_USE_CXA_END_CLEANUP
#define LANG_HOOKS_EH_USE_CXA_END_CLEANUP  true

/* Each front end provides its own.  */
struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

/*
 * process java-specific compiler command-line options
 * return false, but do not complain if the option is not recognized.
 */
static bool
java_handle_option (size_t scode, const char *arg, int value,
		    int kind ATTRIBUTE_UNUSED, location_t loc ATTRIBUTE_UNUSED,
		    const struct cl_option_handlers *handlers ATTRIBUTE_UNUSED)
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
      /* When -Wall given, enable -Wunused.  We do this because the C
	 compiler does it, and people expect it.  */
      warn_unused = value;
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

    case OPT_faux_classpath:
    case OPT_fclasspath_:
      jcf_path_classpath_arg (arg);
      break;

    case OPT_fcompile_resource_:
      resource_name = arg;
      break;

    case OPT_fdump_:
      if (!dump_switch_p (arg))
	return false;
      break;

    case OPT_fencoding_:
      /* Nothing.  */
      break;

    case OPT_fextdirs_:
      jcf_path_extdirs_arg (arg);
      break;

    case OPT_foutput_class_dir_:
      /* FIXME: remove; this is handled by ecj1 now.  */
      break;

    case OPT_version:
      v_flag = 1;
      break;
      
    case OPT_fsource_filename_:
      java_read_sourcefilenames (arg);
      break;
      
    default:
      if (cl_options[code].flags & CL_Java)
	break;
      gcc_unreachable ();
    }

  return true;
}

/* Global open file.  */
FILE *finput;

static bool
java_init (void)
{
  /* FIXME: Indirect dispatch isn't yet compatible with static class
     init optimization.  */
  if (flag_indirect_dispatch)
    always_initialize_class_p = true;

  if (!flag_indirect_dispatch)
    flag_indirect_classes = false;

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
	  decl_buf = XRESIZEVAR (char, decl_buf, decl_buflen);
	}
    }
  strcpy (decl_buf + decl_bufpos, str);
  decl_bufpos += len;
}

/* Append to decl_buf a printable name for NODE.
   Depending on VERBOSITY, more information about NODE
   is printed. Read the comments of decl_printable_name in
   langhooks.h for more.  */

static void
put_decl_node (tree node, int verbosity)
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
	  if (verbosity == 0 && DECL_NAME (node))
	  /* We have been instructed to just print the bare name
	     of the function.  */
	    {
	      put_decl_node (DECL_NAME (node), 0);
	      return;
	    }

	  /* We want to print the type the DECL belongs to. We don't do
	     that when we handle constructors. */
	  if (! DECL_CONSTRUCTOR_P (node)
	      && ! DECL_ARTIFICIAL (node) && DECL_CONTEXT (node)
              /* We want to print qualified DECL names only
                 if verbosity is higher than 1.  */
              && verbosity >= 1)
	    {
	      put_decl_node (TREE_CODE (DECL_CONTEXT (node)) == FUNCTION_DECL
			     ? DECL_CONTEXT (node)
			     : TYPE_NAME (DECL_CONTEXT (node)),
                               verbosity);
	      put_decl_string (".", 1);
	    }
	  if (! DECL_CONSTRUCTOR_P (node))
	    put_decl_node (DECL_NAME (node), verbosity);
	  if (TREE_TYPE (node) != NULL_TREE
              /* We want to print function parameters only if verbosity
                 is higher than 2.  */
              && verbosity >= 2)
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
		  put_decl_node (TREE_VALUE (args), verbosity);
		}
	      put_decl_string (")", 1);
	    }
	}
      else
	put_decl_node (DECL_NAME (node), verbosity);
    }
  else if (TYPE_P (node) && TYPE_NAME (node) != NULL_TREE)
    {
      if (TREE_CODE (node) == RECORD_TYPE && TYPE_ARRAY_P (node)
          /* Print detailed array information only if verbosity is higher
            than 2.  */
          && verbosity >= 2)
	{
	  put_decl_node (TYPE_ARRAY_ELEMENT (node), verbosity);
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
	put_decl_node (TYPE_NAME (node), verbosity);
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
  put_decl_node (decl, v);
  put_decl_string ("", 1);
  return decl_buf;
}

/* Print on stderr the current class and method context.  This function
   is the value of the hook print_error_function. */

static GTY(()) tree last_error_function_context;
static GTY(()) tree last_error_function;
static void
java_print_error_function (diagnostic_context *context ATTRIBUTE_UNUSED,
			   const char *file,
			   diagnostic_info *diagnostic ATTRIBUTE_UNUSED)
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
java_option_lang_mask (void)
{
  return CL_Java;
}

/* Initialize options structure OPTS.  */

static void
java_init_options_struct (struct gcc_options *opts)
{
  opts->x_flag_bounds_check = 1;
  opts->x_flag_exceptions = 1;
  opts->x_flag_non_call_exceptions = 1;

  /* In Java floating point operations never trap.  */
  opts->x_flag_trapping_math = 0;

  /* In Java arithmetic overflow always wraps around.  */
  opts->x_flag_wrapv = 1;

  /* Java requires left-to-right evaluation of subexpressions.  */
  opts->x_flag_evaluation_order = 1;

  /* Java catches NULL pointer exceptions, thus we can not necessarily
     rely on a pointer having a non-NULL value after a dereference.  */
  opts->x_flag_delete_null_pointer_checks = 0;
}

static void
java_init_options (unsigned int decoded_options_count ATTRIBUTE_UNUSED,
		   struct cl_decoded_option *decoded_options ATTRIBUTE_UNUSED)
{
  jcf_path_init ();
}

/* Post-switch processing.  */
static bool
java_post_options (const char **pfilename)
{
  const char *filename = *pfilename;

  /* Excess precision other than "fast" requires front-end
     support.  */
  if (flag_excess_precision_cmdline == EXCESS_PRECISION_STANDARD
      && TARGET_FLT_EVAL_METHOD_NON_DEFAULT)
    sorry ("-fexcess-precision=standard for Java");
  flag_excess_precision_cmdline = EXCESS_PRECISION_FAST;

  /* An absolute requirement: if we're not using indirect dispatch, we
     must always verify everything.  */
  if (! flag_indirect_dispatch)
    flag_verify_invocations = true;

  if (flag_reduced_reflection)
    {
      if (flag_indirect_dispatch)
        error ("-findirect-dispatch is incompatible "
               "with -freduced-reflection");
      if (flag_jni)
        error ("-fjni is incompatible with -freduced-reflection");
    }

  /* Open input file.  */

  if (filename == 0 || !strcmp (filename, "-"))
    {
      finput = stdin;
      filename = "stdin";

      if (dependency_tracking)
	error ("can%'t do dependency tracking with input from stdin");
    }
  else
    {
      if (dependency_tracking)
	{
	  const char *dot;

	  /* If the target is set and the output filename is set, then
	     there's no processing to do here.  Otherwise we must
	     compute one or the other.  */
	  if (! ((dependency_tracking & DEPEND_TARGET_SET)
		 && (dependency_tracking & DEPEND_FILE_ALREADY_SET)))
	    {
	      dot = strrchr (filename, '.');
	      if (dot == NULL)
		error ("couldn%'t determine target name for dependency tracking");
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
  linemap_add (line_table, LC_ENTER, false, filename, 0);
  linemap_add (line_table, LC_RENAME, false, "<built-in>", 0);

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
  gcc_assert (n);

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
java_decl_ok_for_sibcall (const_tree decl)
{
  return (decl != NULL && DECL_CONTEXT (decl) == output_class
          && !DECL_UNINLINABLE (decl));
}

static enum classify_record
java_classify_record (tree type)
{
  if (! CLASS_P (type))
    return RECORD_IS_STRUCT;

  if (CLASS_INTERFACE (TYPE_NAME (type)))
    return RECORD_IS_INTERFACE;

  return RECORD_IS_CLASS;
}

static GTY(()) tree java_eh_personality_decl;

static tree
java_eh_personality (void)
{
  if (!java_eh_personality_decl)
    java_eh_personality_decl = build_personality_function ("gcj");
  return java_eh_personality_decl;
}

#include "gt-java-lang.h"
