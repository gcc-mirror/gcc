/* Language-specific hook definitions for CHILL front end.
   Copyright (C) 1992, 93, 94, 98, 99, 2000 Free Software Foundation, Inc.

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
Boston, MA 02111-1307, USA.  */


#include "config.h"
#include "system.h"
#include "tree.h"
#include "ch-tree.h"
#include "lex.h"
#include "input.h"
#include "toplev.h"

/* Type node for boolean types.  */

tree boolean_type_node;

/* True if STRING(INDEX) yields a CHARS(1) (or BOOLS(1)) rather than
   a CHAR (or BOOL).  Also, makes CHARS(1) similar for CHAR,
   and BOOLS(1) similar to BOOL.  This is for compatibility
   for the 1984 version of Z.200.*/
int flag_old_strings = 0;

/* This is set non-zero to force user input tokens to lower case.
   This is non-standard.  See Z.200, page 8. */
int ignore_case = 1;

/* True if reserved and predefined words ('special' words in the Z.200
   terminology) are in uppercase.  Obviously, this had better not be 
   true if we're ignoring input case. */
int special_UC = 0;

/* The actual name of the input file, regardless of any #line directives */
char* chill_real_input_filename;
extern FILE* finput;

static int deep_const_expr			PARAMS ((tree));
static void chill_print_error_function		PARAMS ((const char *));

/* return 1 if the expression tree given has all
   constant nodes as its leaves; return 0 otherwise. */
static int
deep_const_expr (exp)
     tree exp;
{
  enum chill_tree_code code;
  int length;
  int i;

  if (exp == NULL_TREE)
    return 0;

  code = TREE_CODE (exp);
  length = tree_code_length[(int) code];

  /* constant leaf?  return TRUE */
  if (TREE_CODE_CLASS (code) == 'c')
    return 1;

  /* recursively check next level down */
  for (i = 0; i < length; i++)
    if (! deep_const_expr (TREE_OPERAND (exp, i)))
      return 0;
  return 1;      
}


tree
const_expr (exp)
     tree exp;
{
  if (TREE_CODE (exp) == INTEGER_CST)
    return exp;
  if (TREE_CODE (exp) == CONST_DECL)
    return const_expr (DECL_INITIAL (exp));
  if (TREE_CODE_CLASS (TREE_CODE (exp)) == 'd'
      && DECL_INITIAL (exp) != NULL_TREE
      && TREE_READONLY (exp))
    return DECL_INITIAL (exp);
  if (deep_const_expr (exp))
    return exp;
  if (TREE_CODE (exp) != ERROR_MARK)
    error ("non-constant expression");
  return error_mark_node;
}

/* Each of the functions defined here
   is an alternative to a function in objc-actions.c.  */
   
/* Used by c-lex.c, but only for objc.  */
tree
lookup_interface (arg)
     tree arg ATTRIBUTE_UNUSED;
{
  return 0;
}

int
maybe_objc_comptypes (lhs, rhs)
     tree lhs ATTRIBUTE_UNUSED, rhs ATTRIBUTE_UNUSED;
{
  return -1;
}

tree
maybe_building_objc_message_expr ()
{
  return 0;
}

int
recognize_objc_keyword ()
{
  return 0;
}

void
lang_init_options ()
{
}

/* used by print-tree.c */

void
lang_print_xnode (file, node, indent)
     FILE *file ATTRIBUTE_UNUSED;
     tree node ATTRIBUTE_UNUSED;
     int indent ATTRIBUTE_UNUSED;
{
}

void
GNU_xref_begin ()
{
  fatal ("GCC does not yet support XREF");
}

void
GNU_xref_end ()
{
  fatal ("GCC does not yet support XREF");
}

/*
 * process chill-specific compiler command-line options
 * do not complain if the option is not recognised
 */
int
lang_decode_option (argc, argv)
     int argc;
     char **argv;
{
  char *p = argv[0];
  static int explicit_ignore_case = 0;
  if (!strcmp(p, "-lang-chill"))
    ; /* do nothing */
  else if (!strcmp (p, "-fruntime-checking"))
    {
      range_checking = 1;
      empty_checking = 1;
    }
  else if (!strcmp (p, "-fno-runtime-checking"))
    {
      range_checking = 0;
      empty_checking = 0;
      runtime_checking_flag = 0;
    }
  else if (!strcmp (p, "-flocal-loop-counter"))
    flag_local_loop_counter = 1;
  else if (!strcmp (p, "-fno-local-loop-counter"))
    flag_local_loop_counter = 0;
  else if (!strcmp (p, "-fold-strings"))
    flag_old_strings = 1;
  else if (!strcmp (p, "-fno-old-strings"))
    flag_old_strings = 0;
  else if (!strcmp (p, "-fignore-case"))
    {
      explicit_ignore_case = 1;
      if (special_UC)
	{
	  error ("Ignoring case upon input and");
	  error ("making special words uppercase wouldn't work.");
	}
      else
	ignore_case = 1;
    }
  else if (!strcmp (p, "-fno-ignore-case"))
    ignore_case = 0;
  else if (!strcmp (p, "-fspecial_UC"))
    {
      if (explicit_ignore_case)
	{
	  error ("Making special words uppercase and");
	  error (" ignoring case upon input wouldn't work.");
	}
      else
	special_UC = 1, ignore_case = 0;
    }
  else if (!strcmp (p, "-fspecial_LC"))
    special_UC = 0;
  else if (!strcmp (p, "-fpack"))
    maximum_field_alignment = BITS_PER_UNIT;
  else if (!strcmp (p, "-fno-pack"))
    maximum_field_alignment = 0;
  else if (!strcmp (p, "-fchill-grant-only"))
    grant_only_flag = 1;
  else if (!strcmp (p, "-fgrant-only"))
    grant_only_flag = 1;
  /* user has specified a seize-file path */
  else if (p[0] == '-' && p[1] == 'I')
    register_seize_path (&p[2]);
  if (!strcmp(p, "-itu"))        /* Force Z.200 semantics */
    {
      pedantic = 1;   /* FIXME: new flag name? */
      flag_local_loop_counter = 1;      
    }
  else
    return c_decode_option (argc, argv);

  return 1;
}

static void
chill_print_error_function (file)
     const char *file;
{
  static tree last_error_function = NULL_TREE;
  static struct module *last_error_module = NULL;

  if (last_error_function == current_function_decl
      && last_error_module == current_module)
    return;

  last_error_function = current_function_decl;
  last_error_module = current_module;

  if (file)
    fprintf (stderr, "%s: ", file);

  if (current_function_decl == global_function_decl
      || current_function_decl == NULL_TREE)
    {
      if (current_module == NULL)
	fprintf (stderr, "At top level:\n");
      else
	fprintf (stderr, "In module %s:\n",
		 IDENTIFIER_POINTER (current_module->name));
    }
  else
    {
      const char *kind = "function";
      const char *name = (*decl_printable_name) (current_function_decl, 2);
      fprintf (stderr, "In %s `%s':\n", kind, name);
    }
}

/* Print an error message for invalid use of an incomplete type.
   VALUE is the expression that was used (or 0 if that isn't known)
   and TYPE is the type that was invalid.  */

void
incomplete_type_error (value, type)
     tree value ATTRIBUTE_UNUSED;
     tree type ATTRIBUTE_UNUSED;
{
  error ("internal error - use of undefined type");
}

void
lang_init ()
{
  chill_real_input_filename = input_filename;

  /* the beginning of the file is a new line; check for # */
  /* With luck, we discover the real source file's name from that
     and put it in input_filename.  */

  ungetc (check_newline (), finput);

  /* set default grant file */
  set_default_grant_file ();

  print_error_function = chill_print_error_function;
}
