/* Java(TM) language-specific utility routines.
   Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001
   Free Software Foundation, Inc.

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
Boston, MA 02111-1307, USA. 

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

/* Hacked by Per Bothner <bothner@cygnus.com> February 1996. */

#include "config.h"
#include "system.h"
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
#include "xref.h"
#include "ggc.h"
#include "diagnostic.h"

struct string_option
{
  const char *const string;
  int *const variable;
  const int on_value;
};

static const char *java_init PARAMS ((const char *));
static void java_finish PARAMS ((void));
static void java_init_options PARAMS ((void));
static int java_decode_option PARAMS ((int, char **));
static void put_decl_string PARAMS ((const char *, int));
static void put_decl_node PARAMS ((tree));
static void java_print_error_function PARAMS ((diagnostic_context *,
					       const char *));
static int process_option_with_no PARAMS ((const char *,
					   const struct string_option *,
					   int));

#ifndef TARGET_OBJECT_SUFFIX
# define TARGET_OBJECT_SUFFIX ".o"
#endif

/* Table indexed by tree code giving a string containing a character
   classifying the tree code.  Possibilities are
   t, d, s, c, r, <, 1 and 2.  See java/java-tree.def for details.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) TYPE,

const char tree_code_type[] = {
#include "tree.def"
  'x',
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

char * resource_name;

int flag_emit_class_files = 0;

/* Nonzero if input file is a file with a list of filenames to compile. */

int flag_filelist_file = 0;

/* When non zero, we emit xref strings. Values of the flag for xref
   backends are defined in xref_flag_table, xref.c.  */

int flag_emit_xref = 0;

/* When non zero, -Wall was turned on.  */
int flag_wall = 0;

/* When non zero, check for redundant modifier uses.  */
int flag_redundant = 0;

/* When non zero, call a library routine to do integer divisions. */
int flag_use_divide_subroutine = 1;

/* When non zero, generate code for the Boehm GC.  */
int flag_use_boehm_gc = 0;

/* When non zero, assume the runtime uses a hash table to map an
   object to its synchronization structure.  */
int flag_hash_synchronization;

/* When non zero, assume all native functions are implemented with
   JNI, not CNI.  */
int flag_jni = 0;

/* When non zero, warn when source file is newer than matching class
   file.  */
int flag_newer = 1;

/* When non zero, generate checks for references to NULL.  */
int flag_check_references = 0;

/* The encoding of the source file.  */
const char *current_encoding = NULL;

/* When non zero, report the now deprecated empty statements.  */
int flag_extraneous_semicolon;

/* When non zero, always check for a non gcj generated classes archive.  */
int flag_force_classes_archive_check;

/* When zero, don't optimize static class initialization. This flag shouldn't
   be tested alone, use STATIC_CLASS_INITIALIZATION_OPTIMIZATION_P instead.  */
int flag_optimize_sci = 1;

/* When non zero, use offset tables for virtual method calls
   in order to improve binary compatibility. */
int flag_indirect_dispatch = 0;

/* When zero, don't generate runtime array store checks. */
int flag_store_check = 1;

/* When non zero, print extra version information.  */
static int version_flag = 0;

/* Table of language-dependent -f options.
   STRING is the option name.  VARIABLE is the address of the variable.
   ON_VALUE is the value to store in VARIABLE
    if `-fSTRING' is seen as an option.
   (If `-fno-STRING' is seen as an option, the opposite value is stored.)  */

static const struct string_option
lang_f_options[] =
{
  {"emit-class-file", &flag_emit_class_files, 1},
  {"emit-class-files", &flag_emit_class_files, 1},
  {"filelist-file", &flag_filelist_file, 1},
  {"use-divide-subroutine", &flag_use_divide_subroutine, 1},
  {"use-boehm-gc", &flag_use_boehm_gc, 1},
  {"hash-synchronization", &flag_hash_synchronization, 1},
  {"jni", &flag_jni, 1},
  {"check-references", &flag_check_references, 1},
  {"force-classes-archive-check", &flag_force_classes_archive_check, 1},
  {"optimize-static-class-initialization", &flag_optimize_sci, 1 },
  {"indirect-dispatch", &flag_indirect_dispatch, 1},
  {"store-check", &flag_store_check, 1}
};

static const struct string_option
lang_W_options[] =
{
  { "redundant-modifiers", &flag_redundant, 1 },
  { "extraneous-semicolon", &flag_extraneous_semicolon, 1 },
  { "out-of-date", &flag_newer, 1 }
};

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
#undef LANG_HOOKS_DECODE_OPTION
#define LANG_HOOKS_DECODE_OPTION java_decode_option
#undef LANG_HOOKS_PARSE_FILE
#define LANG_HOOKS_PARSE_FILE java_parse_file
#undef LANG_HOOKS_MARK_ADDRESSABLE
#define LANG_HOOKS_MARK_ADDRESSABLE java_mark_addressable
#undef LANG_HOOKS_EXPAND_EXPR
#define LANG_HOOKS_EXPAND_EXPR java_expand_expr
#undef LANG_HOOKS_TRUTHVALUE_CONVERSION
#define LANG_HOOKS_TRUTHVALUE_CONVERSION java_truthvalue_conversion
#undef LANG_HOOKS_DUP_LANG_SPECIFIC_DECL
#define LANG_HOOKS_DUP_LANG_SPECIFIC_DECL java_dup_lang_specific_decl
#undef LANG_HOOKS_DECL_PRINTABLE_NAME
#define LANG_HOOKS_DECL_PRINTABLE_NAME lang_printable_name
#undef LANG_HOOKS_PRINT_ERROR_FUNCTION
#define LANG_HOOKS_PRINT_ERROR_FUNCTION	java_print_error_function

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

/* Each front end provides its own.  */
const struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

/* Process an option that can accept a `no-' form.
   Return 1 if option found, 0 otherwise.  */
static int
process_option_with_no (p, table, table_size)
     const char *p;
     const struct string_option *table;
     int table_size;
{
  int j;

  for (j = 0; j < table_size; j++)
    {
      if (!strcmp (p, table[j].string))
	{
	  *table[j].variable = table[j].on_value;
	  return 1;
	}
      if (p[0] == 'n' && p[1] == 'o' && p[2] == '-'
	  && ! strcmp (p+3, table[j].string))
	{
	  *table[j].variable = ! table[j].on_value;
	  return 1;
	}
    }

  return 0;
}

/*
 * process java-specific compiler command-line options
 * return 0, but do not complain if the option is not recognised.
 */
static int
java_decode_option (argc, argv)
     int argc __attribute__ ((__unused__));
     char **argv;
{
  char *p = argv[0];

  jcf_path_init ();

  if (strcmp (p, "-version") == 0)
    {
      version_flag = 1;
      /* We return 0 so that the caller can process this.  */
      return 0;
    }

#define CLARG "-fcompile-resource="
  if (strncmp (p, CLARG, sizeof (CLARG) - 1) == 0)
    {
      resource_name = p + sizeof (CLARG) - 1;
      return 1;
    }
#undef CLARG
#define CLARG "-fassume-compiled="
  if (strncmp (p, CLARG, sizeof (CLARG) - 1) == 0)
    {
      add_assume_compiled (p + sizeof (CLARG) - 1, 0);
      return 1;
    }
#undef CLARG
#define CLARG "-fno-assume-compiled="
  if (strncmp (p, CLARG, sizeof (CLARG) - 1) == 0)
    {
      add_assume_compiled (p + sizeof (CLARG) - 1, 1);
      return 1;
    }
#undef CLARG
#define CLARG "-fassume-compiled"
  if (strncmp (p, CLARG, sizeof (CLARG) - 1) == 0)
    {
      add_assume_compiled ("", 0);
      return 1;
    }
#undef CLARG
#define CLARG "-fno-assume-compiled"
  if (strncmp (p, CLARG, sizeof (CLARG) - 1) == 0)
    {
      add_assume_compiled ("", 1);
      return 1;
    }
#undef CLARG
#define CLARG "-fCLASSPATH="
  if (strncmp (p, CLARG, sizeof (CLARG) - 1) == 0)
    {
      jcf_path_classpath_arg (p + sizeof (CLARG) - 1);
      return 1;
    }
#undef CLARG
#define CLARG "-fclasspath="
  if (strncmp (p, CLARG, sizeof (CLARG) - 1) == 0)
    {
      jcf_path_classpath_arg (p + sizeof (CLARG) - 1);
      return 1;
    }
#undef CLARG
#define CLARG "-fbootclasspath="
  if (strncmp (p, CLARG, sizeof (CLARG) - 1) == 0)
    {
      jcf_path_bootclasspath_arg (p + sizeof (CLARG) - 1);
      return 1;
    }
#undef CLARG
#define CLARG "-fextdirs="
  if (strncmp (p, CLARG, sizeof (CLARG) - 1) == 0)
    {
      jcf_path_extdirs_arg (p + sizeof (CLARG) - 1);
      return 1;
    }
#undef CLARG
  else if (strncmp (p, "-I", 2) == 0)
    {
      jcf_path_include_arg (p + 2);
      return 1;
    }

#define ARG "-foutput-class-dir="
  if (strncmp (p, ARG, sizeof (ARG) - 1) == 0)
    {
      jcf_write_base_directory = p + sizeof (ARG) - 1;
      return 1;
    }
#undef ARG
#define ARG "-fencoding="
  if (strncmp (p, ARG, sizeof (ARG) - 1) == 0)
    {
      current_encoding = p + sizeof (ARG) - 1;
      return 1;
    }
#undef ARG

  if (p[0] == '-' && p[1] == 'f')
    {
      /* Some kind of -f option.
	 P's value is the option sans `-f'.
	 Search for it in the table of options.  */
      p += 2;
      if (process_option_with_no (p, lang_f_options,
				  ARRAY_SIZE (lang_f_options)))
	return 1;
      return dump_switch_p (p);
    }

  if (strcmp (p, "-Wall") == 0)
    {
      flag_wall = 1;
      flag_redundant = 1;
      flag_extraneous_semicolon = 1;
      /* When -Wall given, enable -Wunused.  We do this because the C
	 compiler does it, and people expect it.  */
      set_Wunused (1);
      return 1;
    }

  if (p[0] == '-' && p[1] == 'W')
    {
      /* Skip `-W' and see if we accept the option or its `no-' form.  */
      p += 2;
      return process_option_with_no (p, lang_W_options,
				     ARRAY_SIZE (lang_W_options));
    }

  if (strcmp (p, "-MD") == 0)
    {
      jcf_dependency_init (1);
      dependency_tracking |= DEPEND_SET_FILE | DEPEND_ENABLE;
      return 1;
    }
  else if (strcmp (p, "-MMD") == 0)
    {
      jcf_dependency_init (0);
      dependency_tracking |= DEPEND_SET_FILE | DEPEND_ENABLE;
      return 1;
    }
  else if (strcmp (p, "-M") == 0)
    {
      jcf_dependency_init (1);
      dependency_tracking |= DEPEND_ENABLE;
      return 1;
    }
  else if (strcmp (p, "-MM") == 0)
    {
      jcf_dependency_init (0);
      dependency_tracking |= DEPEND_ENABLE;
      return 1;
    }
  else if (strcmp (p, "-MP") == 0)
    {
      jcf_dependency_print_dummies ();
      return 1;
    }
  else if (strcmp (p, "-MT") == 0)
    {
      jcf_dependency_set_target (argv[1]);
      dependency_tracking |= DEPEND_TARGET_SET;
      return 2;
    }
  else if (strcmp (p, "-MF") == 0)
    {
      jcf_dependency_set_dep_file (argv[1]);
      dependency_tracking |= DEPEND_FILE_ALREADY_SET;
      return 2;
    }

  return 0;
}

/* Global open file.  */
FILE *finput;

static const char *
java_init (filename)
     const char *filename;
{
#if 0
  extern int flag_minimal_debug;
  flag_minimal_debug = 0;
#endif

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
		  char *buf = (char *) xmalloc (dot - filename +
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

  jcf_path_init ();
  jcf_path_seal (version_flag);

  java_init_decl_processing ();

  using_eh_for_cleanups ();

  return filename;
}

static void
java_finish ()
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
put_decl_string (str, len)
     const char *str;
     int len;
{
  if (len < 0)
    len = strlen (str);
  if (decl_bufpos + len >= decl_buflen)
    {
      if (decl_buf == NULL)
	{
	  decl_buflen = len + 100;
	  decl_buf = (char *) xmalloc (decl_buflen);
	}
      else
	{
	  decl_buflen *= 2;
	  decl_buf = (char *) xrealloc (decl_buf, decl_buflen);
	}
    }
  strcpy (decl_buf + decl_bufpos, str);
  decl_bufpos += len;
}

/* Append to decl_buf a printable name for NODE. */

static void
put_decl_node (node)
     tree node;
{
  int was_pointer = 0;
  if (TREE_CODE (node) == POINTER_TYPE)
    {
      node = TREE_TYPE (node);
      was_pointer = 1;
    }
  if (TREE_CODE_CLASS (TREE_CODE (node)) == 'd'
      && DECL_NAME (node) != NULL_TREE)
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
  else if (TREE_CODE_CLASS (TREE_CODE (node)) == 't'
      && TYPE_NAME (node) != NULL_TREE)
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
lang_printable_name (decl, v)
     tree decl;
     int v  __attribute__ ((__unused__));
{
  decl_bufpos = 0;
  put_decl_node (decl);
  put_decl_string ("", 1);
  return decl_buf;
}

/* Does the same thing that lang_printable_name, but add a leading
   space to the DECL name string -- With Leading Space.  */

const char *
lang_printable_name_wls (decl, v)
     tree decl;
     int v  __attribute__ ((__unused__));
{
  decl_bufpos = 1;
  put_decl_node (decl);
  put_decl_string ("", 1);
  decl_buf [0] = ' ';
  return decl_buf;
}

/* Print on stderr the current class and method context.  This function
   is the value of the hook print_error_function. */

static GTY(()) tree last_error_function_context;
static GTY(()) tree last_error_function;
static void
java_print_error_function (context, file)
     diagnostic_context *context __attribute__((__unused__));
     const char *file;
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
      fprintf (stderr, "In class `%s':\n",
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
	  fprintf (stderr, "In %s `%s':\n",
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

void lang_init_source (level)
     int level;
{
  inhibit_error_function_printing = (level == 1);
}

static void
java_init_options ()
{
  flag_bounds_check = 1;
  flag_exceptions = 1;
  flag_non_call_exceptions = 1;

  /* In Java floating point operations never trap.  */
  flag_trapping_math = 0;
}

#include "gt-java-lang.h"
