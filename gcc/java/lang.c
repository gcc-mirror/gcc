/* Java(TM) language-specific utility routines.
   Copyright (C) 1996, 1997, 1998 Free Software Foundation, Inc.

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
#include "java-tree.h"
#include "jcf.h"
#include "toplev.h"

int compiling_from_source;

char *language_string = "GNU Java";

/* Nonzero if we want to automatically do array bounds checking;
   on by default.  Use -fno-bounds-check to disable.  */

int flag_bounds_check = 1;

/* Nonzero if we should make is_compiled_class always return 1 for
   appropriate classes that we're referencing.  */

int flag_assume_compiled = 1;

int flag_emit_class_files = 0;

/* From gcc/flags.h, and idicates if exceptions are turned on or not. */

extern int flag_new_exceptions;
extern int flag_exceptions;

/* Table of language-dependent -f options.
   STRING is the option name.  VARIABLE is the address of the variable.
   ON_VALUE is the value to store in VARIABLE
    if `-fSTRING' is seen as an option.
   (If `-fno-STRING' is seen as an option, the opposite value is stored.)  */

static struct { char *string; int *variable; int on_value;} lang_f_options[] =
{
  {"bounds-check", &flag_bounds_check, 1},
  {"assume-compiled", &flag_assume_compiled, 1},
  {"emit-class-file", &flag_emit_class_files, 1},
  {"emit-class-files", &flag_emit_class_files, 1},
};

JCF main_jcf[1];
JCF *current_jcf;

/*
 * process java-specific compiler command-line options
 */
int
lang_decode_option (argc, argv)
     int argc;
     char **argv;
{
  char *p = argv[0];
  if (p[0] == '-' && p[1] == 'f')
    {
      /* Some kind of -f option.
	 P's value is the option sans `-f'.
	 Search for it in the table of options.  */
      int found = 0, j;

      p += 2;

      for (j = 0;
		!found && j < sizeof (lang_f_options) / sizeof (lang_f_options[0]);
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
    }
  else
    finput = fopen (filename, "r");
  if (finput == 0)
    pfatal_with_name (filename);

#ifdef IO_BUFFER_SIZE
  setvbuf (finput, (char *) xmalloc (IO_BUFFER_SIZE), _IOFBF, IO_BUFFER_SIZE);
#endif
  init_lex ();

  return filename;
}

void
finish_parse ()
{
  fclose (finput);
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
     char *str;
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
      put_decl_node (DECL_NAME (node));
      if (TREE_CODE (node) == FUNCTION_DECL && TREE_TYPE (node) != NULL_TREE)
	{
	  int i = 0;
	  tree args = TYPE_ARG_TYPES (TREE_TYPE (node));
	  if (TREE_CODE (TREE_TYPE (node)) == METHOD_TYPE)
	    args = TREE_CHAIN (args);
	  put_decl_string ("(", 1);
	  for ( ; args != NULL_TREE;  args = TREE_CHAIN (args), i++)
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

char *
lang_printable_name (decl, v)
     tree decl;
     int v;
{
  decl_bufpos = 0;
  put_decl_node (decl);
  put_decl_string ("", 1);
  return decl_buf;
}

/* Print on stderr the current class and method context.  This function
   is the value of the hook print_error_function, called from toplev.c. */

void
lang_print_error (file)
     char *file;
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
	       lang_printable_name (last_error_function_context));
    }
  if (last_error_function != current_function_decl)
    {
      if (file)
	fprintf (stderr, "%s: ", file);

      if (current_function_decl == NULL)
	fprintf (stderr, "At top level:\n");
      else
	{
	  char *name = lang_printable_name (current_function_decl, 2);
	  fprintf (stderr, "In method `%s':\n", name);
	}

      last_error_function = current_function_decl;
    }

}

void
lang_init ()
{
  extern struct rtx_def * java_lang_expand_expr ();
  extern struct rtx_def * (*lang_expand_expr) ();
  extern void (*print_error_function) PROTO((char *));
#if 0
  extern int flag_minimal_debug;
  flag_minimal_debug = 0;
#endif

  decl_printable_name = lang_printable_name;
  print_error_function = lang_print_error;
  lang_expand_expr = java_lang_expand_expr;

  JCF_ZERO (main_jcf);
  main_jcf->read_state = finput;
  main_jcf->filbuf = jcf_filbuf_from_stdio;
  current_jcf = main_jcf;

  flag_exceptions = 1;
}

/* This doesn't do anything on purpose. It's used to satisfy the
   print_error_function hook we don't print error messages with bogus
   function prototypes.  */

void java_dummy_print (s)
     char *s;
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
  extern void (*print_error_function) PROTO((char *));
  if (level == 1)
    print_error_function = java_dummy_print;
  else 
    print_error_function = lang_print_error;
}

void
lang_init_options ()
{
  flag_new_exceptions = 1;
}

void
lang_finish ()
{
}

char*
lang_identify ()
{
  return "Java";
}

/* Hooks for print_node.  */

void
print_lang_decl (file, node, indent)
     FILE *file;
     tree node;
     int indent;
{
}

void
print_lang_type (file, node, indent)
     FILE *file;
     tree node;
     int indent;
{
}

void
print_lang_identifier (file, node, indent)
     FILE *file;
     tree node;
     int indent;
{
}

void
print_lang_statistics ()
{
}

/* used by print-tree.c */

void
lang_print_xnode (file, node, indent)
     FILE *file;
     tree node;
     int indent;
{
}
