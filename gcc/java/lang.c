/* Java(TM) language-specific utility routines.
   Copyright (C) 1996, 97-99, 2000 Free Software Foundation, Inc.

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
#include "flags.h"
#include "xref.h"

static void put_decl_string PARAMS ((const char *, int));
static void put_decl_node PARAMS ((tree));
static void java_dummy_print PARAMS ((const char *));
static void lang_print_error PARAMS ((const char *));

#ifndef OBJECT_SUFFIX
# define OBJECT_SUFFIX ".o"
#endif

/* Table indexed by tree code giving a string containing a character
   classifying the tree code.  Possibilities are
   t, d, s, c, r, <, 1 and 2.  See java/java-tree.def for details.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) TYPE,

char java_tree_code_type[] = {
  'x',
#include "java-tree.def"
};
#undef DEFTREECODE

/* Table indexed by tree code giving number of expression
   operands beyond the fixed part of the node structure.
   Not used for types or decls.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) LENGTH,

int java_tree_code_length[] = {
  0,
#include "java-tree.def"
};
#undef DEFTREECODE

/* Names of tree components.
   Used for printing out the tree and error messages.  */
#define DEFTREECODE(SYM, NAME, TYPE, LEN) NAME,

const char *java_tree_code_name[] = {
  "@@dummy",
#include "java-tree.def"
};
#undef DEFTREECODE

int compiling_from_source;

const char * const language_string = "GNU Java";

/* Nonzero if we should make is_compiled_class always return 1 for
   appropriate classes that we're referencing.  */

int flag_assume_compiled = 1;

int flag_emit_class_files = 0;

/* When non zero, we emit xref strings. Values of the flag for xref
   backends are defined in xref_flag_table, xref.c.  */

int flag_emit_xref = 0;

/* When non zero, -Wall was turned on.  */
int flag_wall = 0;

/* When non zero,  check for redundant modifier uses.  */
int flag_redundant = 0;

/* When non zero, warns about overridings that don't occur.  */
int flag_not_overriding = 0;

/* When non zero, warns that final local are treated as non final.  */
int flag_static_local_jdk1_1 = 0;

/* When non zero, call a library routine to do integer divisions. */
int flag_use_divide_subroutine = 1;

/* From gcc/flags.h, and indicates if exceptions are turned on or not.  */

extern int flag_new_exceptions;
extern int flag_exceptions;

/* Table of language-dependent -f options.
   STRING is the option name.  VARIABLE is the address of the variable.
   ON_VALUE is the value to store in VARIABLE
    if `-fSTRING' is seen as an option.
   (If `-fno-STRING' is seen as an option, the opposite value is stored.)  */

static struct { const char *string; int *variable; int on_value;}
lang_f_options[] =
{
  {"emit-class-file", &flag_emit_class_files, 1},
  {"emit-class-files", &flag_emit_class_files, 1},
  {"use-divide-subroutine", &flag_use_divide_subroutine, 1},
};

JCF *current_jcf;

/* Variable controlling how dependency tracking is enabled in
   init_parse.  */
static int dependency_tracking = 0;

/* Flag values for DEPENDENCY_TRACKING.  */
#define DEPEND_SET_FILE 1
#define DEPEND_ENABLE   2

/*
 * process java-specific compiler command-line options
 * return 0, but do not complain if the option is not recognised.
 */
int
lang_decode_option (argc, argv)
     int argc __attribute__ ((__unused__));
     char **argv;
{
  char *p = argv[0];

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
#define CLARG "-fclasspath="
  if (strncmp (p, CLARG, sizeof (CLARG) - 1) == 0)
    {
      jcf_path_classpath_arg (p + sizeof (CLARG) - 1);
      return 1;
    }
#undef CLARG
#define CLARG "-fCLASSPATH="
  if (strncmp (p, CLARG, sizeof (CLARG) - 1) == 0)
    {
      jcf_path_CLASSPATH_arg (p + sizeof (CLARG) - 1);
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

  if (p[0] == '-' && p[1] == 'f')
    {
      /* Some kind of -f option.
	 P's value is the option sans `-f'.
	 Search for it in the table of options.  */
      int found = 0, j;

      p += 2;

      for (j = 0;
	   !found 
	   && j < (int)(sizeof (lang_f_options) / sizeof (lang_f_options[0]));
	   j++)
	{
	  if (!strcmp (p, lang_f_options[j].string))
	    {
	      *lang_f_options[j].variable = lang_f_options[j].on_value;
	      /* A goto here would be cleaner,
		 but breaks the vax pcc.  */
	      found = 1;
	    }
	  if (p[0] == 'n' && p[1] == 'o' && p[2] == '-'
	      && ! strcmp (p+3, lang_f_options[j].string))
	    {
	      *lang_f_options[j].variable = ! lang_f_options[j].on_value;
	      found = 1;
	    }
	}

      return found;
    }

  if (strcmp (p, "-Wall") == 0)
    {
      flag_wall = 1;
      flag_redundant = 1;
      return 1;
    }

  if (strcmp (p, "-Wunsupported-jdk11") == 0)
    {
      flag_static_local_jdk1_1 = 1;
      return 1;
    }

  if (strcmp (p, "-Wredundant-modifiers") == 0)
    {
      flag_redundant = 1;
      return 1;
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

  return 0;
}

FILE *finput;
char *
init_parse (filename)
     char *filename;
{
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
	  dot = strrchr (filename, '.');
	  if (dot == NULL)
	    error ("couldn't determine target name for dependency tracking");
	  else
	    {
	      char *buf = (char *) xmalloc (dot - filename +
					    3 + sizeof (OBJECT_SUFFIX));
	      strncpy (buf, filename, dot - filename);

	      /* If emitting class files, we might have multiple
		 targets.  The class generation code takes care of
		 registering them.  Otherwise we compute the target
		 name here.  */
	      if (flag_emit_class_files)
		jcf_dependency_set_target (NULL);
	      else
		{
		  strcpy (buf + (dot - filename), OBJECT_SUFFIX);
		  jcf_dependency_set_target (buf);
		}

	      if ((dependency_tracking & DEPEND_SET_FILE))
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
  init_lex ();

  return filename;
}

void
finish_parse ()
{
  fclose (finput);
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
#if 0
      if (DECL_CONTEXT (node) != NULL_TREE)
	{
	  put_decl_node (DECL_CONTEXT (node));
	  put_decl_string (".", 1);
	}
#endif
      if (TREE_CODE (node) == FUNCTION_DECL
	  && DECL_NAME (node) == init_identifier_node
	  && !DECL_ARTIFICIAL (node) && current_class)
	put_decl_node (TYPE_NAME (current_class));
      else
	put_decl_node (DECL_NAME (node));
      if (TREE_CODE (node) == FUNCTION_DECL && TREE_TYPE (node) != NULL_TREE)
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
   which is also called directly by lang_print_error. */

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

/* Print on stderr the current class and method context.  This function
   is the value of the hook print_error_function, called from toplev.c. */

static void
lang_print_error (file)
     const char *file;
{
  static tree last_error_function_context = NULL_TREE;
  static tree last_error_function = NULL;

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
	  fprintf (stderr, "In method `%s':\n", name);
	}

      last_error_function = current_function_decl;
    }

}

void
lang_init ()
{
#if 0
  extern int flag_minimal_debug;
  flag_minimal_debug = 0;
#endif

  jcf_path_init ();
  jcf_path_seal ();

  decl_printable_name = lang_printable_name;
  print_error_function = lang_print_error;
  lang_expand_expr = java_lang_expand_expr;

  flag_exceptions = 1;

  /* Append to Gcc tree node definition arrays */

  memcpy (tree_code_type + (int) LAST_AND_UNUSED_TREE_CODE,
	  java_tree_code_type,
	  (int)LAST_JAVA_TREE_CODE - (int)LAST_AND_UNUSED_TREE_CODE);
  memcpy (tree_code_length + (int) LAST_AND_UNUSED_TREE_CODE,
	  java_tree_code_length,
	  (LAST_JAVA_TREE_CODE - 
	   (int)LAST_AND_UNUSED_TREE_CODE) * sizeof (int));
  memcpy (tree_code_name + (int) LAST_AND_UNUSED_TREE_CODE,
	  java_tree_code_name,
	  (LAST_JAVA_TREE_CODE - 
	   (int)LAST_AND_UNUSED_TREE_CODE) * sizeof (char *));

  using_eh_for_cleanups ();
}

/* This doesn't do anything on purpose. It's used to satisfy the
   print_error_function hook we don't print error messages with bogus
   function prototypes.  */

static void
java_dummy_print (s)
     const char *s __attribute__ ((__unused__));
{
}

/* Called to install the PRINT_ERROR_FUNCTION hook differently
   according to LEVEL. LEVEL is 1 during early parsing, when function
   prototypes aren't fully resolved. print_error_function is set so it
   doesn't print incomplete function prototypes. When LEVEL is 2,
   function prototypes are fully resolved and can be printed when
   reporting errors.  */

void lang_init_source (level)
     int level;
{
  if (level == 1)
    print_error_function = java_dummy_print;
  else 
    print_error_function = lang_print_error;
}

void
lang_init_options ()
{
  flag_new_exceptions = 1;
  flag_bounds_check = 1;
}

void
lang_finish ()
{
}

const char *
lang_identify ()
{
  return "Java";
}

/* Hooks for print_node.  */

void
print_lang_decl (file, node, indent)
     FILE *file __attribute ((__unused__));
     tree node __attribute ((__unused__));
     int indent __attribute ((__unused__));
{
}

void
print_lang_type (file, node, indent)
     FILE *file __attribute ((__unused__));
     tree node __attribute ((__unused__));
     int indent __attribute ((__unused__));
{
}

void
print_lang_identifier (file, node, indent)
     FILE *file __attribute ((__unused__));
     tree node __attribute ((__unused__));
     int indent __attribute ((__unused__));
{
}

void
print_lang_statistics ()
{
}

/* used by print-tree.c */

void
lang_print_xnode (file, node, indent)
     FILE *file __attribute ((__unused__));
     tree node __attribute ((__unused__));
     int indent __attribute ((__unused__));
{
}
