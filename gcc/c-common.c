/* Subroutines shared by all languages that are variants of C.
   Copyright (C) 1992, 1993, 1994 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "config.h"
#include "tree.h"
#include "c-lex.h"
#include "c-tree.h"
#include "flags.h"
#include "obstack.h"
#include <stdio.h>
#include <ctype.h>

extern struct obstack permanent_obstack;

static void declare_hidden_char_array PROTO((char *, char *));

/* Make bindings for __FUNCTION__ and __PRETTY_FUNCTION__.  */

void
declare_function_name ()
{
  char *name, *printable_name;

  if (current_function_decl == NULL)
    {
      name = "";
      printable_name = "top level";
    }
  else
    {
      char *kind = "function";
      if (TREE_CODE (TREE_TYPE (current_function_decl)) == METHOD_TYPE)
	kind = "method";
      /* Allow functions to be nameless (such as artificial ones).  */
      if (DECL_NAME (current_function_decl))
        name = IDENTIFIER_POINTER (DECL_NAME (current_function_decl));
      else
	name = "";
      printable_name = (*decl_printable_name) (current_function_decl, &kind);
    }

  declare_hidden_char_array ("__FUNCTION__", name);
  declare_hidden_char_array ("__PRETTY_FUNCTION__", printable_name);
}

static void
declare_hidden_char_array (name, value)
     char *name, *value;
{
  tree decl, type, init;
  int vlen;

  /* If the default size of char arrays isn't big enough for the name,
     or if we want to give warnings for large objects, make a bigger one.  */
  vlen = strlen (value) + 1;
  type = char_array_type_node;
  if (TREE_INT_CST_LOW (TYPE_MAX_VALUE (TREE_TYPE (type))) < vlen
      || warn_larger_than)
    type = build_array_type (char_type_node,
			     build_index_type (build_int_2 (vlen, 0)));
  push_obstacks_nochange ();
  decl = build_decl (VAR_DECL, get_identifier (name), type);
  TREE_STATIC (decl) = 1;
  TREE_READONLY (decl) = 1;
  TREE_ASM_WRITTEN (decl) = 1;
  DECL_SOURCE_LINE (decl) = 0;
  DECL_IN_SYSTEM_HEADER (decl) = 1;
  DECL_IGNORED_P (decl) = 1;
  init = build_string (vlen, value);
  TREE_TYPE (init) = type;
  DECL_INITIAL (decl) = init;
  finish_decl (pushdecl (decl), init, NULL_TREE);
}

/* Given a chain of STRING_CST nodes,
   concatenate them into one STRING_CST
   and give it a suitable array-of-chars data type.  */

tree
combine_strings (strings)
     tree strings;
{
  register tree value, t;
  register int length = 1;
  int wide_length = 0;
  int wide_flag = 0;
  int wchar_bytes = TYPE_PRECISION (wchar_type_node) / BITS_PER_UNIT;
  int nchars;

  if (TREE_CHAIN (strings))
    {
      /* More than one in the chain, so concatenate.  */
      register char *p, *q;

      /* Don't include the \0 at the end of each substring,
	 except for the last one.
	 Count wide strings and ordinary strings separately.  */
      for (t = strings; t; t = TREE_CHAIN (t))
	{
	  if (TREE_TYPE (t) == wchar_array_type_node)
	    {
	      wide_length += (TREE_STRING_LENGTH (t) - wchar_bytes);
	      wide_flag = 1;
	    }
	  else
	    length += (TREE_STRING_LENGTH (t) - 1);
	}

      /* If anything is wide, the non-wides will be converted,
	 which makes them take more space.  */
      if (wide_flag)
	length = length * wchar_bytes + wide_length;

      p = savealloc (length);

      /* Copy the individual strings into the new combined string.
	 If the combined string is wide, convert the chars to ints
	 for any individual strings that are not wide.  */

      q = p;
      for (t = strings; t; t = TREE_CHAIN (t))
	{
	  int len = (TREE_STRING_LENGTH (t)
		     - ((TREE_TYPE (t) == wchar_array_type_node)
			? wchar_bytes : 1));
	  if ((TREE_TYPE (t) == wchar_array_type_node) == wide_flag)
	    {
	      bcopy (TREE_STRING_POINTER (t), q, len);
	      q += len;
	    }
	  else
	    {
	      int i;
	      for (i = 0; i < len; i++)
		((int *) q)[i] = TREE_STRING_POINTER (t)[i];
	      q += len * wchar_bytes;
	    }
	}
      if (wide_flag)
	{
	  int i;
	  for (i = 0; i < wchar_bytes; i++)
	    *q++ = 0;
	}
      else
	*q = 0;

      value = make_node (STRING_CST);
      TREE_STRING_POINTER (value) = p;
      TREE_STRING_LENGTH (value) = length;
      TREE_CONSTANT (value) = 1;
    }
  else
    {
      value = strings;
      length = TREE_STRING_LENGTH (value);
      if (TREE_TYPE (value) == wchar_array_type_node)
	wide_flag = 1;
    }

  /* Compute the number of elements, for the array type.  */ 
  nchars = wide_flag ? length / wchar_bytes : length;

  /* Create the array type for the string constant.
     -Wwrite-strings says make the string constant an array of const char
     so that copying it to a non-const pointer will get a warning.  */
  if (warn_write_strings
      && (! flag_traditional  && ! flag_writable_strings))
    {
      tree elements
	= build_type_variant (wide_flag ? wchar_type_node : char_type_node,
			      1, 0);
      TREE_TYPE (value)
	= build_array_type (elements,
			    build_index_type (build_int_2 (nchars - 1, 0)));
    }
  else
    TREE_TYPE (value)
      = build_array_type (wide_flag ? wchar_type_node : char_type_node,
			  build_index_type (build_int_2 (nchars - 1, 0)));
  TREE_CONSTANT (value) = 1;
  TREE_STATIC (value) = 1;
  return value;
}

/* Process the attributes listed in ATTRIBUTES
   and install them in DECL.  */

void
decl_attributes (decl, attributes)
     tree decl, attributes;
{
  tree a, name, args, type, new_attr;

  type = TREE_TYPE (decl);

  new_attr = TYPE_ATTRIBUTES (type);

  for (a = attributes; a; a = TREE_CHAIN (a))
    if (!(name = TREE_VALUE (a)))
	continue;
    else if (name == get_identifier ("packed")
	     || name == get_identifier ("__packed__"))
      {
	if (TREE_CODE (decl) == FIELD_DECL)
	  DECL_PACKED (decl) = 1;
	/* We can't set DECL_PACKED for a VAR_DECL, because the bit is
	   used for DECL_REGISTER.  It wouldn't mean anything anyway.  */
	else
	  warning_with_decl (decl, "`packed' attribute ignore");

      }
    else if (TREE_VALUE (a) == get_identifier ("noreturn")
	     || TREE_VALUE (a) == get_identifier ("__noreturn__")
	     || TREE_VALUE (a) == get_identifier ("volatile")
	     || TREE_VALUE (a) == get_identifier ("__volatile__"))
      {
	if (TREE_CODE (decl) == FUNCTION_DECL)
	  TREE_THIS_VOLATILE (decl) = 1;
	else if (TREE_CODE (type) == POINTER_TYPE
		 && TREE_CODE (TREE_TYPE (type)) == FUNCTION_TYPE)
	  TREE_TYPE (decl) = type 
	    = build_pointer_type
	      (build_type_variant (TREE_TYPE (type),
				   TREE_READONLY (TREE_TYPE (type)), 1));
	else
	  warning_with_decl (decl, "`%s' attribute ignored",
			     IDENTIFIER_POINTER (TREE_VALUE (a)));
      }
    else if (TREE_VALUE (a) == get_identifier ("const")
	     || TREE_VALUE (a) == get_identifier ("__const__"))
      {
	if (TREE_CODE (decl) == FUNCTION_DECL)
	  TREE_READONLY (decl) = 1;
	else if (TREE_CODE (type) == POINTER_TYPE
		 && TREE_CODE (TREE_TYPE (type)) == FUNCTION_TYPE)
	  TREE_TYPE (decl) = type
	    = build_pointer_type
	      (build_type_variant (TREE_TYPE (type), 1,
				   TREE_THIS_VOLATILE (TREE_TYPE (type))));
	else
	  warning_with_decl (decl, "`const' attribute ignored");
      }
    else if (TREE_VALUE (a) == get_identifier ("transparent_union")
	     || TREE_VALUE (a) == get_identifier ("__transparent_union__"))
      {
	if (TREE_CODE (decl) == PARM_DECL
	    && TREE_CODE (type) == UNION_TYPE
	    && TYPE_MODE (type) == DECL_MODE (TYPE_FIELDS (type)))
	  DECL_TRANSPARENT_UNION (decl) = 1;
	else if (TREE_CODE (decl) == TYPE_DECL
		 && TREE_CODE (type) == UNION_TYPE
		 && TYPE_MODE (type) == DECL_MODE (TYPE_FIELDS (type)))
	  TYPE_TRANSPARENT_UNION (type) = 1;
	else
	  warning_with_decl (decl, "`transparent_union' attribute ignored");
      }
    else if (TREE_CODE (name) != TREE_LIST)
     {
#ifdef VALID_MACHINE_ATTRIBUTE
	if (VALID_MACHINE_ATTRIBUTE (type, new_attr, name))
	  { 
	    register tree atlist;

	    for (atlist = new_attr; atlist; atlist = TREE_CHAIN (atlist))
	       if (TREE_VALUE (atlist) == name)
		  goto found_attr;

	    new_attr = tree_cons (NULL_TREE, name, new_attr);
found_attr:;
	  }
	else
#endif
	  warning ("`%s' attribute directive ignored",
		   IDENTIFIER_POINTER (name));
     }
    else if ( args = TREE_CHAIN(name),
	      (!strcmp (IDENTIFIER_POINTER (name = TREE_PURPOSE (name)), "mode")
	       || !strcmp (IDENTIFIER_POINTER (name), "__mode__"))
	      && list_length (args) == 1
	      && TREE_CODE (TREE_VALUE (args)) == IDENTIFIER_NODE)
      {
	int i;
	char *specified_name
	  = IDENTIFIER_POINTER (TREE_VALUE (args));

	/* Give this decl a type with the specified mode.  */
	for (i = 0; i < NUM_MACHINE_MODES; i++)
	  if (!strcmp (specified_name, GET_MODE_NAME (i)))
	    {
	      tree typefm
		= type_for_mode (i, TREE_UNSIGNED (type));
	      if (typefm != 0)
		{
		  TREE_TYPE (decl) = type = typefm;
		  DECL_SIZE (decl) = 0;
		  layout_decl (decl, 0);
		}
	      else
		error ("no data type for mode `%s'", specified_name);
	      break;
	    }
	if (i == NUM_MACHINE_MODES)
	  error_with_decl (decl, "unknown machine mode `%s'", specified_name);
      }
    else if ((!strcmp (IDENTIFIER_POINTER (name), "section")
	      || !strcmp (IDENTIFIER_POINTER (name), "__section__"))
	     && list_length (args) == 1
	     && TREE_CODE (TREE_VALUE (args)) == STRING_CST)
      {
#ifdef ASM_OUTPUT_SECTION_NAME
	if (TREE_CODE (decl) == FUNCTION_DECL || TREE_CODE (decl) == VAR_DECL)
	  {
	    if (TREE_CODE (decl) == VAR_DECL && current_function_decl != NULL_TREE)
	      error_with_decl (decl,
			       "section attribute cannot be specified for local variables");
	    /* The decl may have already been given a section attribute from
	       a previous declaration.  Ensure they match.  */
	    else if (DECL_SECTION_NAME (decl) != NULL_TREE
		     && strcmp (TREE_STRING_POINTER (DECL_SECTION_NAME (decl)),
				TREE_STRING_POINTER (TREE_VALUE (args))) != 0)
	      error_with_decl (decl,
			       "section of `%s' conflicts with previous declaration");
	    else
	      DECL_SECTION_NAME (decl) = TREE_VALUE (args);
	  }
	else
	  error_with_decl (decl,
			   "section attribute not allowed for `%s'");
#else
	error_with_decl (decl, "section attributes are not supported for this target");
#endif
      }
    else if ((!strcmp (IDENTIFIER_POINTER (name), "aligned")
	      || !strcmp (IDENTIFIER_POINTER (name), "__aligned__"))
	     && list_length (args) == 1
	     && TREE_CODE (TREE_VALUE (args)) == INTEGER_CST)
      {
	tree align_expr = TREE_VALUE (args);
	int align;

	/* Strip any NOPs of any kind.  */
	while (TREE_CODE (align_expr) == NOP_EXPR
	       || TREE_CODE (align_expr) == CONVERT_EXPR
	       || TREE_CODE (align_expr) == NON_LVALUE_EXPR)
	  align_expr = TREE_OPERAND (align_expr, 0);

	if (TREE_CODE (align_expr) != INTEGER_CST)
	  {
	    error_with_decl (decl,
			     "requested alignment of `%s' is not a constant");
	    continue;
	  }

	align = TREE_INT_CST_LOW (align_expr) * BITS_PER_UNIT;
	
	if (exact_log2 (align) == -1)
	  error_with_decl (decl,
			   "requested alignment of `%s' is not a power of 2");
	else if (TREE_CODE (decl) != VAR_DECL
		 && TREE_CODE (decl) != FIELD_DECL)
	  error_with_decl (decl,
			   "alignment specified for `%s'");
	else
	  DECL_ALIGN (decl) = align;
      }
    else if ((!strcmp (IDENTIFIER_POINTER (name), "format")
	      || !strcmp (IDENTIFIER_POINTER (name), "__format__"))
	     && list_length (args) == 3
	     && TREE_CODE (TREE_VALUE (args)) == IDENTIFIER_NODE
	     && TREE_CODE (TREE_VALUE (TREE_CHAIN (args))) == INTEGER_CST
	     && TREE_CODE (TREE_VALUE (TREE_CHAIN (TREE_CHAIN (args)))) == INTEGER_CST )
      {
        tree format_type = TREE_VALUE (args);
	tree format_num_expr = TREE_VALUE (TREE_CHAIN (args));
	tree first_arg_num_expr = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (args)));
	int format_num;
	int first_arg_num;
	int is_scan;
	tree argument;
	int arg_num;
	
	if (TREE_CODE (decl) != FUNCTION_DECL)
	  {
	    error_with_decl (decl,
			     "argument format specified for non-function `%s'");
	    continue;
	  }
	
	if (!strcmp (IDENTIFIER_POINTER (format_type), "printf")
	    || !strcmp (IDENTIFIER_POINTER (format_type), "__printf__"))
	  is_scan = 0;
	else if (!strcmp (IDENTIFIER_POINTER (format_type), "scanf")
		 || !strcmp (IDENTIFIER_POINTER (format_type), "__scanf__"))
	  is_scan = 1;
	else
	  {
	    error_with_decl (decl, "unrecognized format specifier for `%s'");
	    continue;
	  }
	
	/* Strip any conversions from the string index and first arg number
	   and verify they are constants.  */
	while (TREE_CODE (format_num_expr) == NOP_EXPR
	       || TREE_CODE (format_num_expr) == CONVERT_EXPR
	       || TREE_CODE (format_num_expr) == NON_LVALUE_EXPR)
	  format_num_expr = TREE_OPERAND (format_num_expr, 0);

	while (TREE_CODE (first_arg_num_expr) == NOP_EXPR
	       || TREE_CODE (first_arg_num_expr) == CONVERT_EXPR
	       || TREE_CODE (first_arg_num_expr) == NON_LVALUE_EXPR)
	  first_arg_num_expr = TREE_OPERAND (first_arg_num_expr, 0);

	if (TREE_CODE (format_num_expr) != INTEGER_CST
	    || TREE_CODE (first_arg_num_expr) != INTEGER_CST)
	  {
	    error_with_decl (decl,
		   "format string for `%s' has non-constant operand number");
	    continue;
	  }

	format_num = TREE_INT_CST_LOW (format_num_expr);
	first_arg_num = TREE_INT_CST_LOW (first_arg_num_expr);
	if (first_arg_num != 0 && first_arg_num <= format_num)
	  {
	    error_with_decl (decl,
	      "format string arg follows the args to be formatted, for `%s'");
	    continue;
	  }

	/* If a parameter list is specified, verify that the format_num
	   argument is actually a string, in case the format attribute
	   is in error.  */
	argument = TYPE_ARG_TYPES (type);
	if (argument)
	  {
	    for (arg_num = 1; ; ++arg_num)
	      {
		if (argument == 0 || arg_num == format_num)
		  break;
		argument = TREE_CHAIN (argument);
	      }
	    if (! argument
		|| TREE_CODE (TREE_VALUE (argument)) != POINTER_TYPE
		|| (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_VALUE (argument)))
		    != char_type_node))
	      {
		error_with_decl (decl,
			     "format string arg not a string type, for `%s'");
		continue;
	      }
	    if (first_arg_num != 0)
	      {
		/* Verify that first_arg_num points to the last arg, the ... */
		while (argument)
		  arg_num++, argument = TREE_CHAIN (argument);
		if (arg_num != first_arg_num)
		  {
		    error_with_decl (decl,
				 "args to be formatted is not ..., for `%s'");
		    continue;
		  }
	      }
	  }

	record_function_format (DECL_NAME (decl), DECL_ASSEMBLER_NAME (decl),
				is_scan, format_num, first_arg_num);
      }
    else
	warning ("`%s' attribute directive ignored",
                       IDENTIFIER_POINTER (name));

  TREE_TYPE (decl) = build_type_attribute_variant (type, new_attr);
}

/* Check a printf/fprintf/sprintf/scanf/fscanf/sscanf format against
   a parameter list.  */

#define T_I	&integer_type_node
#define T_L	&long_integer_type_node
#define T_LL	&long_long_integer_type_node
#define T_S	&short_integer_type_node
#define T_UI	&unsigned_type_node
#define T_UL	&long_unsigned_type_node
#define T_ULL	&long_long_unsigned_type_node
#define T_US	&short_unsigned_type_node
#define T_F	&float_type_node
#define T_D	&double_type_node
#define T_LD	&long_double_type_node
#define T_C	&char_type_node
#define T_V	&void_type_node
#define T_W	&wchar_type_node
#define T_ST    &sizetype

typedef struct {
  char *format_chars;
  int pointer_count;
  /* Type of argument if no length modifier is used.  */
  tree *nolen;
  /* Type of argument if length modifier for shortening is used.
     If NULL, then this modifier is not allowed.  */
  tree *hlen;
  /* Type of argument if length modifier `l' is used.
     If NULL, then this modifier is not allowed.  */
  tree *llen;
  /* Type of argument if length modifier `q' or `ll' is used.
     If NULL, then this modifier is not allowed.  */
  tree *qlen;
  /* Type of argument if length modifier `L' is used.
     If NULL, then this modifier is not allowed.  */
  tree *bigllen;
  /* List of other modifier characters allowed with these options.  */
  char *flag_chars;
} format_char_info;

static format_char_info print_char_table[] = {
  { "di",	0,	T_I,	T_I,	T_L,	T_LL,	T_LL,	"-wp0 +"	},
  { "oxX",	0,	T_UI,	T_UI,	T_UL,	T_ULL,	T_ULL,	"-wp0#"		},
  { "u",	0,	T_UI,	T_UI,	T_UL,	T_ULL,	T_ULL,	"-wp0"		},
/* Two GNU extensions.  */
  { "Z",	0,	T_ST,	NULL,	NULL,	NULL,	NULL,	"-wp0"		},
  { "m",	0,	T_UI,	T_UI,	T_UL,	NULL,	NULL,	"-wp"		},
  { "feEgG",	0,	T_D,	NULL,	NULL,	NULL,	T_LD,	"-wp0 +#"	},
  { "c",	0,	T_I,	NULL,	T_W,	NULL,	NULL,	"-w"		},
  { "C",	0,	T_W,	NULL,	NULL,	NULL,	NULL,	"-w"		},
  { "s",	1,	T_C,	NULL,	T_W,	NULL,	NULL,	"-wp"		},
  { "S",	1,	T_W,	NULL,	NULL,	NULL,	NULL,	"-wp"		},
  { "p",	1,	T_V,	NULL,	NULL,	NULL,	NULL,	"-w"		},
  { "n",	1,	T_I,	T_S,	T_L,	T_LL,	NULL,	""		},
  { NULL }
};

static format_char_info scan_char_table[] = {
  { "di",	1,	T_I,	T_S,	T_L,	T_LL,	T_LL,	"*"	},
  { "ouxX",	1,	T_UI,	T_US,	T_UL,	T_ULL,	T_ULL,	"*"	},	
  { "efgEG",	1,	T_F,	NULL,	T_D,	NULL,	T_LD,	"*"	},
  { "sc",	1,	T_C,	NULL,	T_W,	NULL,	NULL,	"*a"	},
  { "[",	1,	T_C,	NULL,	NULL,	NULL,	NULL,	"*a"	},
  { "C",	1,	T_W,	NULL,	NULL,	NULL,	NULL,	"*"	},
  { "S",	1,	T_W,	NULL,	NULL,	NULL,	NULL,	"*"	},
  { "p",	2,	T_V,	NULL,	NULL,	NULL,	NULL,	"*"	},
  { "n",	1,	T_I,	T_S,	T_L,	T_LL,	NULL,	""	},
  { NULL }
};

typedef struct function_format_info {
  struct function_format_info *next;  /* next structure on the list */
  tree name;			/* identifier such as "printf" */
  tree assembler_name;		/* optional mangled identifier (for C++) */
  int is_scan;			/* TRUE if *scanf */
  int format_num;		/* number of format argument */
  int first_arg_num;		/* number of first arg (zero for varargs) */
} function_format_info;

static function_format_info *function_format_list = NULL;

static void check_format_info PROTO((function_format_info *, tree));

/* Initialize the table of functions to perform format checking on.
   The ANSI functions are always checked (whether <stdio.h> is
   included or not), since it is common to call printf without
   including <stdio.h>.  There shouldn't be a problem with this,
   since ANSI reserves these function names whether you include the
   header file or not.  In any case, the checking is harmless.  */

void
init_function_format_info ()
{
  record_function_format (get_identifier ("printf"), NULL_TREE, 0, 1, 2);
  record_function_format (get_identifier ("fprintf"), NULL_TREE, 0, 2, 3);
  record_function_format (get_identifier ("sprintf"), NULL_TREE, 0, 2, 3);
  record_function_format (get_identifier ("scanf"), NULL_TREE, 1, 1, 2);
  record_function_format (get_identifier ("fscanf"), NULL_TREE, 1, 2, 3);
  record_function_format (get_identifier ("sscanf"), NULL_TREE, 1, 2, 3);
  record_function_format (get_identifier ("vprintf"), NULL_TREE, 0, 1, 0);
  record_function_format (get_identifier ("vfprintf"), NULL_TREE, 0, 2, 0);
  record_function_format (get_identifier ("vsprintf"), NULL_TREE, 0, 2, 0);
}

/* Record information for argument format checking.  FUNCTION_IDENT is
   the identifier node for the name of the function to check (its decl
   need not exist yet).  IS_SCAN is true for scanf-type format checking;
   false indicates printf-style format checking.  FORMAT_NUM is the number
   of the argument which is the format control string (starting from 1).
   FIRST_ARG_NUM is the number of the first actual argument to check
   against teh format string, or zero if no checking is not be done
   (e.g. for varargs such as vfprintf).  */

void
record_function_format (name, assembler_name, is_scan,
			format_num, first_arg_num)
      tree name;
      tree assembler_name;
      int is_scan;
      int format_num;
      int first_arg_num;
{
  function_format_info *info;

  /* Re-use existing structure if it's there.  */

  for (info = function_format_list; info; info = info->next)
    {
      if (info->name == name && info->assembler_name == assembler_name)
	break;
    }
  if (! info)
    {
      info = (function_format_info *) xmalloc (sizeof (function_format_info));
      info->next = function_format_list;
      function_format_list = info;

      info->name = name;
      info->assembler_name = assembler_name;
    }

  info->is_scan = is_scan;
  info->format_num = format_num;
  info->first_arg_num = first_arg_num;
}

static char	tfaff[] = "too few arguments for format";

/* Check the argument list of a call to printf, scanf, etc.
   NAME is the function identifier.
   ASSEMBLER_NAME is the function's assembler identifier.
   (Either NAME or ASSEMBLER_NAME, but not both, may be NULL_TREE.)
   PARAMS is the list of argument values.  */

void
check_function_format (name, assembler_name, params)
     tree name;
     tree assembler_name;
     tree params;
{
  function_format_info *info;

  /* See if this function is a format function.  */
  for (info = function_format_list; info; info = info->next)
    {
      if (info->assembler_name
	  ? (info->assembler_name == assembler_name)
	  : (info->name == name))
	{
	  /* Yup; check it.  */
	  check_format_info (info, params);
	  break;
	}
    }
}

/* Check the argument list of a call to printf, scanf, etc.
   INFO points to the function_format_info structure.
   PARAMS is the list of argument values.  */

static void
check_format_info (info, params)
     function_format_info *info;
     tree params;
{
  int i;
  int arg_num;
  int suppressed, wide, precise;
  int length_char;
  int format_char;
  int format_length;
  tree format_tree;
  tree cur_param;
  tree cur_type;
  tree wanted_type;
  tree first_fillin_param;
  char *format_chars;
  format_char_info *fci;
  static char message[132];
  char flag_chars[8];
  int has_operand_number = 0;

  /* Skip to format argument.  If the argument isn't available, there's
     no work for us to do; prototype checking will catch the problem.  */
  for (arg_num = 1; ; ++arg_num)
    {
      if (params == 0)
	return;
      if (arg_num == info->format_num)
	break;
      params = TREE_CHAIN (params);
    }
  format_tree = TREE_VALUE (params);
  params = TREE_CHAIN (params);
  if (format_tree == 0)
    return;
  /* We can only check the format if it's a string constant.  */
  while (TREE_CODE (format_tree) == NOP_EXPR)
    format_tree = TREE_OPERAND (format_tree, 0); /* strip coercion */
  if (format_tree == null_pointer_node)
    {
      warning ("null format string");
      return;
    }
  if (TREE_CODE (format_tree) != ADDR_EXPR)
    return;
  format_tree = TREE_OPERAND (format_tree, 0);
  if (TREE_CODE (format_tree) != STRING_CST)
    return;
  format_chars = TREE_STRING_POINTER (format_tree);
  format_length = TREE_STRING_LENGTH (format_tree);
  if (format_length <= 1)
    warning ("zero-length format string");
  if (format_chars[--format_length] != 0)
    {
      warning ("unterminated format string");
      return;
    }
  /* Skip to first argument to check.  */
  while (arg_num + 1 < info->first_arg_num)
    {
      if (params == 0)
	return;
      params = TREE_CHAIN (params);
      ++arg_num;
    }

  first_fillin_param = params;
  while (1)
    {
      int aflag;
      if (*format_chars == 0)
	{
	  if (format_chars - TREE_STRING_POINTER (format_tree) != format_length)
	    warning ("embedded `\\0' in format");
	  if (info->first_arg_num != 0 && params != 0 && ! has_operand_number)
	    warning ("too many arguments for format");
	  return;
	}
      if (*format_chars++ != '%')
	continue;
      if (*format_chars == 0)
	{
	  warning ("spurious trailing `%%' in format");
	  continue;
	}
      if (*format_chars == '%')
	{
	  ++format_chars;
	  continue;
	}
      flag_chars[0] = 0;
      suppressed = wide = precise = FALSE;
      if (info->is_scan)
	{
	  suppressed = *format_chars == '*';
	  if (suppressed)
	    ++format_chars;
	  while (isdigit (*format_chars))
	    ++format_chars;
	}
      else
	{
	  /* See if we have a number followed by a dollar sign.  If we do,
	     it is an operand number, so set PARAMS to that operand.  */
	  if (*format_chars >= '0' && *format_chars <= '9')
	    {
	      char *p = format_chars;

	      while (*p >= '0' && *p++ <= '9')
		;

	      if (*p == '$')
		{
		  int opnum = atoi (format_chars);

		  params = first_fillin_param;
		  format_chars = p + 1;
		  has_operand_number = 1;

		  for (i = 1; i < opnum && params != 0; i++)
		    params = TREE_CHAIN (params);

		  if (opnum == 0 || params == 0)
		    {
		      warning ("operand number out of range in format");
		      return;
		    }
		}
	    }

	  while (*format_chars != 0 && index (" +#0-", *format_chars) != 0)
	    {
	      if (index (flag_chars, *format_chars) != 0)
		{
		  sprintf (message, "repeated `%c' flag in format",
			   *format_chars);
		  warning (message);
		}
	      i = strlen (flag_chars);
	      flag_chars[i++] = *format_chars++;
	      flag_chars[i] = 0;
	    }
	  /* "If the space and + flags both appear, 
	     the space flag will be ignored."  */
	  if (index (flag_chars, ' ') != 0
	      && index (flag_chars, '+') != 0)
	    warning ("use of both ` ' and `+' flags in format");
	  /* "If the 0 and - flags both appear,
	     the 0 flag will be ignored."  */
	  if (index (flag_chars, '0') != 0
	      && index (flag_chars, '-') != 0)
	    warning ("use of both `0' and `-' flags in format");
	  if (*format_chars == '*')
	    {
	      wide = TRUE;
	      /* "...a field width...may be indicated by an asterisk.
		 In this case, an int argument supplies the field width..."  */
	      ++format_chars;
	      if (params == 0)
		{
		  warning (tfaff);
		  return;
		}
	      if (info->first_arg_num != 0)
		{
		  cur_param = TREE_VALUE (params);
		  params = TREE_CHAIN (params);
		  ++arg_num;
		  /* size_t is generally not valid here.
		     It will work on most machines, because size_t and int
		     have the same mode.  But might as well warn anyway,
		     since it will fail on other machines.  */
		  if ((TYPE_MAIN_VARIANT (TREE_TYPE (cur_param))
		       != integer_type_node)
		      &&
		      (TYPE_MAIN_VARIANT (TREE_TYPE (cur_param))
		       != unsigned_type_node))
		    {
		      sprintf (message,
			       "field width is not type int (arg %d)",
			       arg_num);
		      warning (message);
		    }
		}
	    }
	  else
	    {
	      while (isdigit (*format_chars))
		{
		  wide = TRUE;
		  ++format_chars;
		}
	    }
	  if (*format_chars == '.')
	    {
	      precise = TRUE;
	      ++format_chars;
	      if (*format_chars != '*' && !isdigit (*format_chars))
		warning ("`.' not followed by `*' or digit in format");
	      /* "...a...precision...may be indicated by an asterisk.
		 In this case, an int argument supplies the...precision."  */
	      if (*format_chars == '*')
		{
		  if (info->first_arg_num != 0)
		    {
		      ++format_chars;
		      if (params == 0)
		        {
			  warning (tfaff);
			  return;
			}
		      cur_param = TREE_VALUE (params);
		      params = TREE_CHAIN (params);
		      ++arg_num;
		      if (TYPE_MAIN_VARIANT (TREE_TYPE (cur_param))
			  != integer_type_node)
		        {
		          sprintf (message,
				   "field width is not type int (arg %d)",
				   arg_num);
		          warning (message);
		        }
		    }
		}
	      else
		{
		  while (isdigit (*format_chars))
		    ++format_chars;
		}
	    }
	}
      if (*format_chars == 'h' || *format_chars == 'l' || *format_chars == 'q' ||
	  *format_chars == 'L')
	length_char = *format_chars++;
      else
	length_char = 0;
      if (length_char == 'l' && *format_chars == 'l')
	length_char = 'q', format_chars++;
      aflag = 0;
      if (*format_chars == 'a')
	{
	  aflag = 1;
	  format_chars++;
	}
      if (suppressed && length_char != 0)
	{
	  sprintf (message,
		   "use of `*' and `%c' together in format",
		   length_char);
	  warning (message);
	}
      format_char = *format_chars;
      if (format_char == 0)
	{
	  warning ("conversion lacks type at end of format");
	  continue;
	}
      format_chars++;
      fci = info->is_scan ? scan_char_table : print_char_table;
      while (fci->format_chars != 0
	     && index (fci->format_chars, format_char) == 0)
	  ++fci;
      if (fci->format_chars == 0)
	{
	  if (format_char >= 040 && format_char < 0177)
	    sprintf (message,
		     "unknown conversion type character `%c' in format",
		     format_char);
	  else
	    sprintf (message,
		     "unknown conversion type character 0x%x in format",
		     format_char);
	  warning (message);
	  continue;
	}
      if (wide && index (fci->flag_chars, 'w') == 0)
	{
	  sprintf (message, "width used with `%c' format",
		   format_char);
	  warning (message);
	}
      if (precise && index (fci->flag_chars, 'p') == 0)
	{
	  sprintf (message, "precision used with `%c' format",
		   format_char);
	  warning (message);
	}
      if (aflag && index (fci->flag_chars, 'a') == 0)
	{
	  sprintf (message, "`a' flag used with `%c' format",
		   format_char);
	  warning (message);
	}
      if (info->is_scan && format_char == '[')
	{
	  /* Skip over scan set, in case it happens to have '%' in it.  */
	  if (*format_chars == '^')
	    ++format_chars;
	  /* Find closing bracket; if one is hit immediately, then
	     it's part of the scan set rather than a terminator.  */
	  if (*format_chars == ']')
	    ++format_chars;
	  while (*format_chars && *format_chars != ']')
	    ++format_chars;
	  if (*format_chars != ']')
	      /* The end of the format string was reached.  */
	      warning ("no closing `]' for `%%[' format");
	}
      if (suppressed)
	{
	  if (index (fci->flag_chars, '*') == 0)
	    {
	      sprintf (message,
		       "suppression of `%c' conversion in format",
		       format_char);
	      warning (message);
	    }
	  continue;
	}
      for (i = 0; flag_chars[i] != 0; ++i)
	{
	  if (index (fci->flag_chars, flag_chars[i]) == 0)
	    {
	      sprintf (message, "flag `%c' used with type `%c'",
		       flag_chars[i], format_char);
	      warning (message);
	    }
	}
      if (precise && index (flag_chars, '0') != 0
	  && (format_char == 'd' || format_char == 'i'
	      || format_char == 'o' || format_char == 'u'
	      || format_char == 'x' || format_char == 'x'))
	{
	  sprintf (message,
		   "precision and `0' flag not both allowed with `%c' format",
		   format_char);
	  warning (message);
	}
      switch (length_char)
	{
	default: wanted_type = fci->nolen ? *(fci->nolen) : 0; break;
	case 'h': wanted_type = fci->hlen ? *(fci->hlen) : 0; break;
	case 'l': wanted_type = fci->llen ? *(fci->llen) : 0; break;
	case 'q': wanted_type = fci->qlen ? *(fci->qlen) : 0; break;
	case 'L': wanted_type = fci->bigllen ? *(fci->bigllen) : 0; break;
	}
      if (wanted_type == 0)
	{
	  sprintf (message,
		   "use of `%c' length character with `%c' type character",
		   length_char, format_char);
	  warning (message);
	}

      /*
       ** XXX -- should kvetch about stuff such as
       **	{
       **		const int	i;
       **
       **		scanf ("%d", &i);
       **	}
       */

      /* Finally. . .check type of argument against desired type!  */
      if (info->first_arg_num == 0)
	continue;
      if (params == 0)
	{
	  warning (tfaff);
	  return;
	}
      cur_param = TREE_VALUE (params);
      params = TREE_CHAIN (params);
      ++arg_num;
      cur_type = TREE_TYPE (cur_param);

      /* Check the types of any additional pointer arguments
	 that precede the "real" argument.  */
      for (i = 0; i < fci->pointer_count; ++i)
	{
	  if (TREE_CODE (cur_type) == POINTER_TYPE)
	    {
	      cur_type = TREE_TYPE (cur_type);
	      continue;
	    }
	  sprintf (message,
		   "format argument is not a %s (arg %d)",
		   ((fci->pointer_count == 1) ? "pointer" : "pointer to a pointer"),
		   arg_num);
	  warning (message);
	  break;
	}

      /* Check the type of the "real" argument, if there's a type we want.  */
      if (i == fci->pointer_count && wanted_type != 0
	  && wanted_type != TYPE_MAIN_VARIANT (cur_type)
	  /* If we want `void *', allow any pointer type.
	     (Anything else would already have got a warning.)  */
	  && ! (wanted_type == void_type_node
		&& fci->pointer_count > 0)
	  /* Don't warn about differences merely in signedness.  */
	  && !(TREE_CODE (wanted_type) == INTEGER_TYPE
	       && TREE_CODE (TYPE_MAIN_VARIANT (cur_type)) == INTEGER_TYPE
	       && (TREE_UNSIGNED (wanted_type)
		   ? wanted_type == (cur_type = unsigned_type (cur_type))
		   : wanted_type == (cur_type = signed_type (cur_type))))
	  /* Likewise, "signed char", "unsigned char" and "char" are
	     equivalent but the above test won't consider them equivalent.  */
	  && ! (wanted_type == char_type_node
		&& (TYPE_MAIN_VARIANT (cur_type) == signed_char_type_node
		    || TYPE_MAIN_VARIANT (cur_type) == unsigned_char_type_node)))
	{
	  register char *this;
	  register char *that;
  
	  this = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (wanted_type)));
	  that = 0;
	  if (TREE_CODE (cur_type) != ERROR_MARK
	      && TYPE_NAME (cur_type) != 0
	      && TREE_CODE (cur_type) != INTEGER_TYPE
	      && !(TREE_CODE (cur_type) == POINTER_TYPE
		   && TREE_CODE (TREE_TYPE (cur_type)) == INTEGER_TYPE))
	    {
	      if (TREE_CODE (TYPE_NAME (cur_type)) == TYPE_DECL
		  && DECL_NAME (TYPE_NAME (cur_type)) != 0)
		that = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (cur_type)));
	      else
		that = IDENTIFIER_POINTER (TYPE_NAME (cur_type));
	    }

	  /* A nameless type can't possibly match what the format wants.
	     So there will be a warning for it.
	     Make up a string to describe vaguely what it is.  */
	  if (that == 0)
	    {
	      if (TREE_CODE (cur_type) == POINTER_TYPE)
		that = "pointer";
	      else
		that = "different type";
	    }

	  /* Make the warning better in case of mismatch of int vs long.  */
	  if (TREE_CODE (cur_type) == INTEGER_TYPE
	      && TREE_CODE (wanted_type) == INTEGER_TYPE
	      && TYPE_PRECISION (cur_type) == TYPE_PRECISION (wanted_type)
	      && TYPE_NAME (cur_type) != 0
	      && TREE_CODE (TYPE_NAME (cur_type)) == TYPE_DECL)
	    that = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (cur_type)));

	  if (strcmp (this, that) != 0)
	    {
	      sprintf (message, "%s format, %s arg (arg %d)",
			this, that, arg_num);
	      warning (message);
	    }
	}
    }
}

/* Print a warning if a constant expression had overflow in folding.
   Invoke this function on every expression that the language
   requires to be a constant expression.
   Note the ANSI C standard says it is erroneous for a
   constant expression to overflow.  */

void
constant_expression_warning (value)
     tree value;
{
  if ((TREE_CODE (value) == INTEGER_CST || TREE_CODE (value) == REAL_CST
       || TREE_CODE (value) == COMPLEX_CST)
      && TREE_CONSTANT_OVERFLOW (value) && pedantic)
    pedwarn ("overflow in constant expression");
}

/* Print a warning if an expression had overflow in folding.
   Invoke this function on every expression that
   (1) appears in the source code, and
   (2) might be a constant expression that overflowed, and
   (3) is not already checked by convert_and_check;
   however, do not invoke this function on operands of explicit casts.  */

void
overflow_warning (value)
     tree value;
{
  if ((TREE_CODE (value) == INTEGER_CST
       || (TREE_CODE (value) == COMPLEX_CST
	   && TREE_CODE (TREE_REALPART (value)) == INTEGER_CST))
      && TREE_OVERFLOW (value))
    {
      TREE_OVERFLOW (value) = 0;
      warning ("integer overflow in expression");
    }
  else if ((TREE_CODE (value) == REAL_CST
	    || (TREE_CODE (value) == COMPLEX_CST
		&& TREE_CODE (TREE_REALPART (value)) == REAL_CST))
	   && TREE_OVERFLOW (value))
    {
      TREE_OVERFLOW (value) = 0;
      warning ("floating-pointer overflow in expression");
    }
}

/* Print a warning if a large constant is truncated to unsigned,
   or if -Wconversion is used and a constant < 0 is converted to unsigned.
   Invoke this function on every expression that might be implicitly
   converted to an unsigned type.  */

void
unsigned_conversion_warning (result, operand)
     tree result, operand;
{
  if (TREE_CODE (operand) == INTEGER_CST
      && TREE_CODE (TREE_TYPE (result)) == INTEGER_TYPE
      && TREE_UNSIGNED (TREE_TYPE (result))
      && !int_fits_type_p (operand, TREE_TYPE (result)))
    {
      if (!int_fits_type_p (operand, signed_type (TREE_TYPE (result))))
	/* This detects cases like converting -129 or 256 to unsigned char.  */
	warning ("large integer implicitly truncated to unsigned type");
      else if (warn_conversion)
	warning ("negative integer implicitly converted to unsigned type");
    }
}

/* Convert EXPR to TYPE, warning about conversion problems with constants.
   Invoke this function on every expression that is converted implicitly,
   i.e. because of language rules and not because of an explicit cast.  */

tree
convert_and_check (type, expr)
     tree type, expr;
{
  tree t = convert (type, expr);
  if (TREE_CODE (t) == INTEGER_CST)
    {
      if (TREE_OVERFLOW (t))
	{
	  TREE_OVERFLOW (t) = 0;

	  /* No warning for converting 0x80000000 to int.  */
	  if (!(TREE_UNSIGNED (type) < TREE_UNSIGNED (TREE_TYPE (expr))
		&& TREE_CODE (TREE_TYPE (expr)) == INTEGER_TYPE
		&& TYPE_PRECISION (type) == TYPE_PRECISION (TREE_TYPE (expr))))
	    /* If EXPR fits in the unsigned version of TYPE,
	       don't warn unless pedantic.  */
	    if (pedantic
		|| TREE_UNSIGNED (type)
		|| ! int_fits_type_p (expr, unsigned_type (type)))
	      warning ("overflow in implicit constant conversion");
	}
      else
	unsigned_conversion_warning (t, expr);
    }
  return t;
}

void
c_expand_expr_stmt (expr)
     tree expr;
{
  /* Do default conversion if safe and possibly important,
     in case within ({...}).  */
  if ((TREE_CODE (TREE_TYPE (expr)) == ARRAY_TYPE && lvalue_p (expr))
      || TREE_CODE (TREE_TYPE (expr)) == FUNCTION_TYPE)
    expr = default_conversion (expr);

  if (TREE_TYPE (expr) != error_mark_node
      && TYPE_SIZE (TREE_TYPE (expr)) == 0
      && TREE_CODE (TREE_TYPE (expr)) != ARRAY_TYPE)
    error ("expression statement has incomplete type");

  expand_expr_stmt (expr);
}

/* Validate the expression after `case' and apply default promotions.  */

tree
check_case_value (value)
     tree value;
{
  if (value == NULL_TREE)
    return value;

  /* Strip NON_LVALUE_EXPRs since we aren't using as an lvalue.  */
  STRIP_TYPE_NOPS (value);

  if (TREE_CODE (value) != INTEGER_CST
      && value != error_mark_node)
    {
      error ("case label does not reduce to an integer constant");
      value = error_mark_node;
    }
  else
    /* Promote char or short to int.  */
    value = default_conversion (value);

  constant_expression_warning (value);

  return value;
}

/* Return an integer type with BITS bits of precision,
   that is unsigned if UNSIGNEDP is nonzero, otherwise signed.  */

tree
type_for_size (bits, unsignedp)
     unsigned bits;
     int unsignedp;
{
  if (bits == TYPE_PRECISION (signed_char_type_node))
    return unsignedp ? unsigned_char_type_node : signed_char_type_node;

  if (bits == TYPE_PRECISION (short_integer_type_node))
    return unsignedp ? short_unsigned_type_node : short_integer_type_node;

  if (bits == TYPE_PRECISION (integer_type_node))
    return unsignedp ? unsigned_type_node : integer_type_node;

  if (bits == TYPE_PRECISION (long_integer_type_node))
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;

  if (bits == TYPE_PRECISION (long_long_integer_type_node))
    return (unsignedp ? long_long_unsigned_type_node
	    : long_long_integer_type_node);

  if (bits <= TYPE_PRECISION (intQI_type_node))
    return unsignedp ? unsigned_intQI_type_node : intQI_type_node;

  if (bits <= TYPE_PRECISION (intHI_type_node))
    return unsignedp ? unsigned_intHI_type_node : intHI_type_node;

  if (bits <= TYPE_PRECISION (intSI_type_node))
    return unsignedp ? unsigned_intSI_type_node : intSI_type_node;

  if (bits <= TYPE_PRECISION (intDI_type_node))
    return unsignedp ? unsigned_intDI_type_node : intDI_type_node;

  return 0;
}

/* Return a data type that has machine mode MODE.
   If the mode is an integer,
   then UNSIGNEDP selects between signed and unsigned types.  */

tree
type_for_mode (mode, unsignedp)
     enum machine_mode mode;
     int unsignedp;
{
  if (mode == TYPE_MODE (signed_char_type_node))
    return unsignedp ? unsigned_char_type_node : signed_char_type_node;

  if (mode == TYPE_MODE (short_integer_type_node))
    return unsignedp ? short_unsigned_type_node : short_integer_type_node;

  if (mode == TYPE_MODE (integer_type_node))
    return unsignedp ? unsigned_type_node : integer_type_node;

  if (mode == TYPE_MODE (long_integer_type_node))
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;

  if (mode == TYPE_MODE (long_long_integer_type_node))
    return unsignedp ? long_long_unsigned_type_node : long_long_integer_type_node;

  if (mode == TYPE_MODE (intQI_type_node))
    return unsignedp ? unsigned_intQI_type_node : intQI_type_node;

  if (mode == TYPE_MODE (intHI_type_node))
    return unsignedp ? unsigned_intHI_type_node : intHI_type_node;

  if (mode == TYPE_MODE (intSI_type_node))
    return unsignedp ? unsigned_intSI_type_node : intSI_type_node;

  if (mode == TYPE_MODE (intDI_type_node))
    return unsignedp ? unsigned_intDI_type_node : intDI_type_node;

  if (mode == TYPE_MODE (float_type_node))
    return float_type_node;

  if (mode == TYPE_MODE (double_type_node))
    return double_type_node;

  if (mode == TYPE_MODE (long_double_type_node))
    return long_double_type_node;

  if (mode == TYPE_MODE (build_pointer_type (char_type_node)))
    return build_pointer_type (char_type_node);

  if (mode == TYPE_MODE (build_pointer_type (integer_type_node)))
    return build_pointer_type (integer_type_node);

  return 0;
}

/* Return the minimum number of bits needed to represent VALUE in a
   signed or unsigned type, UNSIGNEDP says which.  */

int
min_precision (value, unsignedp)
     tree value;
     int unsignedp;
{
  int log;

  /* If the value is negative, compute its negative minus 1.  The latter
     adjustment is because the absolute value of the largest negative value
     is one larger than the largest positive value.  This is equivalent to
     a bit-wise negation, so use that operation instead.  */

  if (tree_int_cst_sgn (value) < 0)
    value = fold (build1 (BIT_NOT_EXPR, TREE_TYPE (value), value));

  /* Return the number of bits needed, taking into account the fact
     that we need one more bit for a signed than unsigned type.  */

  if (integer_zerop (value))
    log = 0;
  else if (TREE_INT_CST_HIGH (value) != 0)
    log = HOST_BITS_PER_WIDE_INT + floor_log2 (TREE_INT_CST_HIGH (value));
  else
    log = floor_log2 (TREE_INT_CST_LOW (value));

  return log + 1 + ! unsignedp;
}

/* Print an error message for invalid operands to arith operation CODE.
   NOP_EXPR is used as a special case (see truthvalue_conversion).  */

void
binary_op_error (code)
     enum tree_code code;
{
  register char *opname = "unknown";

  switch (code)
    {
    case NOP_EXPR:
      error ("invalid truth-value expression");
      return;

    case PLUS_EXPR:
      opname = "+"; break;
    case MINUS_EXPR:
      opname = "-"; break;
    case MULT_EXPR:
      opname = "*"; break;
    case MAX_EXPR:
      opname = "max"; break;
    case MIN_EXPR:
      opname = "min"; break;
    case EQ_EXPR:
      opname = "=="; break;
    case NE_EXPR:
      opname = "!="; break;
    case LE_EXPR:
      opname = "<="; break;
    case GE_EXPR:
      opname = ">="; break;
    case LT_EXPR:
      opname = "<"; break;
    case GT_EXPR:
      opname = ">"; break;
    case LSHIFT_EXPR:
      opname = "<<"; break;
    case RSHIFT_EXPR:
      opname = ">>"; break;
    case TRUNC_MOD_EXPR:
    case FLOOR_MOD_EXPR:
      opname = "%"; break;
    case TRUNC_DIV_EXPR:
    case FLOOR_DIV_EXPR:
      opname = "/"; break;
    case BIT_AND_EXPR:
      opname = "&"; break;
    case BIT_IOR_EXPR:
      opname = "|"; break;
    case TRUTH_ANDIF_EXPR:
      opname = "&&"; break;
    case TRUTH_ORIF_EXPR:
      opname = "||"; break;
    case BIT_XOR_EXPR:
      opname = "^"; break;
    case LROTATE_EXPR:
    case RROTATE_EXPR:
      opname = "rotate"; break;
    }
  error ("invalid operands to binary %s", opname);
}

/* Subroutine of build_binary_op, used for comparison operations.
   See if the operands have both been converted from subword integer types
   and, if so, perhaps change them both back to their original type.
   This function is also responsible for converting the two operands
   to the proper common type for comparison.

   The arguments of this function are all pointers to local variables
   of build_binary_op: OP0_PTR is &OP0, OP1_PTR is &OP1,
   RESTYPE_PTR is &RESULT_TYPE and RESCODE_PTR is &RESULTCODE.

   If this function returns nonzero, it means that the comparison has
   a constant value.  What this function returns is an expression for
   that value.  */

tree
shorten_compare (op0_ptr, op1_ptr, restype_ptr, rescode_ptr)
     tree *op0_ptr, *op1_ptr;
     tree *restype_ptr;
     enum tree_code *rescode_ptr;
{
  register tree type;
  tree op0 = *op0_ptr;
  tree op1 = *op1_ptr;
  int unsignedp0, unsignedp1;
  int real1, real2;
  tree primop0, primop1;
  enum tree_code code = *rescode_ptr;

  /* Throw away any conversions to wider types
     already present in the operands.  */

  primop0 = get_narrower (op0, &unsignedp0);
  primop1 = get_narrower (op1, &unsignedp1);

  /* Handle the case that OP0 does not *contain* a conversion
     but it *requires* conversion to FINAL_TYPE.  */

  if (op0 == primop0 && TREE_TYPE (op0) != *restype_ptr)
    unsignedp0 = TREE_UNSIGNED (TREE_TYPE (op0));
  if (op1 == primop1 && TREE_TYPE (op1) != *restype_ptr)
    unsignedp1 = TREE_UNSIGNED (TREE_TYPE (op1));

  /* If one of the operands must be floated, we cannot optimize.  */
  real1 = TREE_CODE (TREE_TYPE (primop0)) == REAL_TYPE;
  real2 = TREE_CODE (TREE_TYPE (primop1)) == REAL_TYPE;

  /* If first arg is constant, swap the args (changing operation
     so value is preserved), for canonicalization.  Don't do this if
     the second arg is 0.  */

  if (TREE_CONSTANT (primop0)
      && ! integer_zerop (primop1) && ! real_zerop (primop1))
    {
      register tree tem = primop0;
      register int temi = unsignedp0;
      primop0 = primop1;
      primop1 = tem;
      tem = op0;
      op0 = op1;
      op1 = tem;
      *op0_ptr = op0;
      *op1_ptr = op1;
      unsignedp0 = unsignedp1;
      unsignedp1 = temi;
      temi = real1;
      real1 = real2;
      real2 = temi;

      switch (code)
	{
	case LT_EXPR:
	  code = GT_EXPR;
	  break;
	case GT_EXPR:
	  code = LT_EXPR;
	  break;
	case LE_EXPR:
	  code = GE_EXPR;
	  break;
	case GE_EXPR:
	  code = LE_EXPR;
	  break;
	}
      *rescode_ptr = code;
    }

  /* If comparing an integer against a constant more bits wide,
     maybe we can deduce a value of 1 or 0 independent of the data.
     Or else truncate the constant now
     rather than extend the variable at run time.

     This is only interesting if the constant is the wider arg.
     Also, it is not safe if the constant is unsigned and the
     variable arg is signed, since in this case the variable
     would be sign-extended and then regarded as unsigned.
     Our technique fails in this case because the lowest/highest
     possible unsigned results don't follow naturally from the
     lowest/highest possible values of the variable operand.
     For just EQ_EXPR and NE_EXPR there is another technique that
     could be used: see if the constant can be faithfully represented
     in the other operand's type, by truncating it and reextending it
     and see if that preserves the constant's value.  */

  if (!real1 && !real2
      && TREE_CODE (primop1) == INTEGER_CST
      && TYPE_PRECISION (TREE_TYPE (primop0)) < TYPE_PRECISION (*restype_ptr))
    {
      int min_gt, max_gt, min_lt, max_lt;
      tree maxval, minval;
      /* 1 if comparison is nominally unsigned.  */
      int unsignedp = TREE_UNSIGNED (*restype_ptr);
      tree val;

      type = signed_or_unsigned_type (unsignedp0, TREE_TYPE (primop0));

      maxval = TYPE_MAX_VALUE (type);
      minval = TYPE_MIN_VALUE (type);

      if (unsignedp && !unsignedp0)
	*restype_ptr = signed_type (*restype_ptr);

      if (TREE_TYPE (primop1) != *restype_ptr)
	primop1 = convert (*restype_ptr, primop1);
      if (type != *restype_ptr)
	{
	  minval = convert (*restype_ptr, minval);
	  maxval = convert (*restype_ptr, maxval);
	}

      if (unsignedp && unsignedp0)
	{
	  min_gt = INT_CST_LT_UNSIGNED (primop1, minval);
	  max_gt = INT_CST_LT_UNSIGNED (primop1, maxval);
	  min_lt = INT_CST_LT_UNSIGNED (minval, primop1);
	  max_lt = INT_CST_LT_UNSIGNED (maxval, primop1);
	}
      else
	{
	  min_gt = INT_CST_LT (primop1, minval);
	  max_gt = INT_CST_LT (primop1, maxval);
	  min_lt = INT_CST_LT (minval, primop1);
	  max_lt = INT_CST_LT (maxval, primop1);
	}

      val = 0;
      /* This used to be a switch, but Genix compiler can't handle that.  */
      if (code == NE_EXPR)
	{
	  if (max_lt || min_gt)
	    val = integer_one_node;
	}
      else if (code == EQ_EXPR)
	{
	  if (max_lt || min_gt)
	    val = integer_zero_node;
	}
      else if (code == LT_EXPR)
	{
	  if (max_lt)
	    val = integer_one_node;
	  if (!min_lt)
	    val = integer_zero_node;
	}
      else if (code == GT_EXPR)
	{
	  if (min_gt)
	    val = integer_one_node;
	  if (!max_gt)
	    val = integer_zero_node;
	}
      else if (code == LE_EXPR)
	{
	  if (!max_gt)
	    val = integer_one_node;
	  if (min_gt)
	    val = integer_zero_node;
	}
      else if (code == GE_EXPR)
	{
	  if (!min_lt)
	    val = integer_one_node;
	  if (max_lt)
	    val = integer_zero_node;
	}

      /* If primop0 was sign-extended and unsigned comparison specd,
	 we did a signed comparison above using the signed type bounds.
	 But the comparison we output must be unsigned.

	 Also, for inequalities, VAL is no good; but if the signed
	 comparison had *any* fixed result, it follows that the
	 unsigned comparison just tests the sign in reverse
	 (positive values are LE, negative ones GE).
	 So we can generate an unsigned comparison
	 against an extreme value of the signed type.  */

      if (unsignedp && !unsignedp0)
	{
	  if (val != 0)
	    switch (code)
	      {
	      case LT_EXPR:
	      case GE_EXPR:
		primop1 = TYPE_MIN_VALUE (type);
		val = 0;
		break;

	      case LE_EXPR:
	      case GT_EXPR:
		primop1 = TYPE_MAX_VALUE (type);
		val = 0;
		break;
	      }
	  type = unsigned_type (type);
	}

      if (!max_gt && !unsignedp0 && TREE_CODE (primop0) != INTEGER_CST)
	{
	  /* This is the case of (char)x >?< 0x80, which people used to use
	     expecting old C compilers to change the 0x80 into -0x80.  */
	  if (val == integer_zero_node)
	    warning ("comparison is always 0 due to limited range of data type");
	  if (val == integer_one_node)
	    warning ("comparison is always 1 due to limited range of data type");
	}

      if (!min_lt && unsignedp0 && TREE_CODE (primop0) != INTEGER_CST)
	{
	  /* This is the case of (unsigned char)x >?< -1 or < 0.  */
	  if (val == integer_zero_node)
	    warning ("comparison is always 0 due to limited range of data type");
	  if (val == integer_one_node)
	    warning ("comparison is always 1 due to limited range of data type");
	}

      if (val != 0)
	{
	  /* Don't forget to evaluate PRIMOP0 if it has side effects.  */
	  if (TREE_SIDE_EFFECTS (primop0))
	    return build (COMPOUND_EXPR, TREE_TYPE (val), primop0, val);
	  return val;
	}

      /* Value is not predetermined, but do the comparison
	 in the type of the operand that is not constant.
	 TYPE is already properly set.  */
    }
  else if (real1 && real2
	   && (TYPE_PRECISION (TREE_TYPE (primop0))
	       == TYPE_PRECISION (TREE_TYPE (primop1))))
    type = TREE_TYPE (primop0);

  /* If args' natural types are both narrower than nominal type
     and both extend in the same manner, compare them
     in the type of the wider arg.
     Otherwise must actually extend both to the nominal
     common type lest different ways of extending
     alter the result.
     (eg, (short)-1 == (unsigned short)-1  should be 0.)  */

  else if (unsignedp0 == unsignedp1 && real1 == real2
	   && TYPE_PRECISION (TREE_TYPE (primop0)) < TYPE_PRECISION (*restype_ptr)
	   && TYPE_PRECISION (TREE_TYPE (primop1)) < TYPE_PRECISION (*restype_ptr))
    {
      type = common_type (TREE_TYPE (primop0), TREE_TYPE (primop1));
      type = signed_or_unsigned_type (unsignedp0
				      || TREE_UNSIGNED (*restype_ptr),
				      type);
      /* Make sure shorter operand is extended the right way
	 to match the longer operand.  */
      primop0 = convert (signed_or_unsigned_type (unsignedp0, TREE_TYPE (primop0)),
			 primop0);
      primop1 = convert (signed_or_unsigned_type (unsignedp1, TREE_TYPE (primop1)),
			 primop1);
    }
  else
    {
      /* Here we must do the comparison on the nominal type
	 using the args exactly as we received them.  */
      type = *restype_ptr;
      primop0 = op0;
      primop1 = op1;

      if (!real1 && !real2 && integer_zerop (primop1)
	  && TREE_UNSIGNED (*restype_ptr))
	{
	  tree value = 0;
	  switch (code)
	    {
	    case GE_EXPR:
	      /* All unsigned values are >= 0, so we warn if extra warnings
		 are requested.  However, if OP0 is a constant that is
		 >= 0, the signedness of the comparison isn't an issue,
		 so suppress the warning.  */
	      if (extra_warnings
		  && ! (TREE_CODE (primop0) == INTEGER_CST
			&& ! TREE_OVERFLOW (convert (signed_type (type),
						     primop0))))
		warning ("unsigned value >= 0 is always 1");
	      value = integer_one_node;
	      break;

	    case LT_EXPR:
	      if (extra_warnings
		  && ! (TREE_CODE (primop0) == INTEGER_CST
			&& ! TREE_OVERFLOW (convert (signed_type (type),
						     primop0))))
		warning ("unsigned value < 0 is always 0");
	      value = integer_zero_node;
	    }

	  if (value != 0)
	    {
	      /* Don't forget to evaluate PRIMOP0 if it has side effects.  */
	      if (TREE_SIDE_EFFECTS (primop0))
		return build (COMPOUND_EXPR, TREE_TYPE (value),
			      primop0, value);
	      return value;
	    }
	}
    }

  *op0_ptr = convert (type, primop0);
  *op1_ptr = convert (type, primop1);

  *restype_ptr = integer_type_node;

  return 0;
}

/* Prepare expr to be an argument of a TRUTH_NOT_EXPR,
   or validate its data type for an `if' or `while' statement or ?..: exp.

   This preparation consists of taking the ordinary
   representation of an expression expr and producing a valid tree
   boolean expression describing whether expr is nonzero.  We could
   simply always do build_binary_op (NE_EXPR, expr, integer_zero_node, 1),
   but we optimize comparisons, &&, ||, and !.

   The resulting type should always be `integer_type_node'.  */

tree
truthvalue_conversion (expr)
     tree expr;
{
  if (TREE_CODE (expr) == ERROR_MARK)
    return expr;

#if 0 /* This appears to be wrong for C++.  */
  /* These really should return error_mark_node after 2.4 is stable.
     But not all callers handle ERROR_MARK properly.  */
  switch (TREE_CODE (TREE_TYPE (expr)))
    {
    case RECORD_TYPE:
      error ("struct type value used where scalar is required");
      return integer_zero_node;

    case UNION_TYPE:
      error ("union type value used where scalar is required");
      return integer_zero_node;

    case ARRAY_TYPE:
      error ("array type value used where scalar is required");
      return integer_zero_node;

    default:
      break;
    }
#endif /* 0 */

  switch (TREE_CODE (expr))
    {
      /* It is simpler and generates better code to have only TRUTH_*_EXPR
	 or comparison expressions as truth values at this level.  */
#if 0
    case COMPONENT_REF:
      /* A one-bit unsigned bit-field is already acceptable.  */
      if (1 == TREE_INT_CST_LOW (DECL_SIZE (TREE_OPERAND (expr, 1)))
	  && TREE_UNSIGNED (TREE_OPERAND (expr, 1)))
	return expr;
      break;
#endif

    case EQ_EXPR:
      /* It is simpler and generates better code to have only TRUTH_*_EXPR
	 or comparison expressions as truth values at this level.  */
#if 0
      if (integer_zerop (TREE_OPERAND (expr, 1)))
	return build_unary_op (TRUTH_NOT_EXPR, TREE_OPERAND (expr, 0), 0);
#endif
    case NE_EXPR: case LE_EXPR: case GE_EXPR: case LT_EXPR: case GT_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
      return convert (integer_type_node, expr);

    case ERROR_MARK:
      return expr;

    case INTEGER_CST:
      return integer_zerop (expr) ? integer_zero_node : integer_one_node;

    case REAL_CST:
      return real_zerop (expr) ? integer_zero_node : integer_one_node;

    case ADDR_EXPR:
      if (TREE_SIDE_EFFECTS (TREE_OPERAND (expr, 0)))
	return build (COMPOUND_EXPR, integer_type_node,
		      TREE_OPERAND (expr, 0), integer_one_node);
      else
	return integer_one_node;

    case COMPLEX_EXPR:
      return build_binary_op ((TREE_SIDE_EFFECTS (TREE_OPERAND (expr, 1))
			       ? TRUTH_OR_EXPR : TRUTH_ORIF_EXPR),
			      truthvalue_conversion (TREE_OPERAND (expr, 0)),
			      truthvalue_conversion (TREE_OPERAND (expr, 1)),
			      0);

    case NEGATE_EXPR:
    case ABS_EXPR:
    case FLOAT_EXPR:
    case FFS_EXPR:
      /* These don't change whether an object is non-zero or zero.  */
      return truthvalue_conversion (TREE_OPERAND (expr, 0));

    case LROTATE_EXPR:
    case RROTATE_EXPR:
      /* These don't change whether an object is zero or non-zero, but
	 we can't ignore them if their second arg has side-effects.  */
      if (TREE_SIDE_EFFECTS (TREE_OPERAND (expr, 1)))
	return build (COMPOUND_EXPR, integer_type_node, TREE_OPERAND (expr, 1),
		      truthvalue_conversion (TREE_OPERAND (expr, 0)));
      else
	return truthvalue_conversion (TREE_OPERAND (expr, 0));
      
    case COND_EXPR:
      /* Distribute the conversion into the arms of a COND_EXPR.  */
      return fold (build (COND_EXPR, integer_type_node, TREE_OPERAND (expr, 0),
			  truthvalue_conversion (TREE_OPERAND (expr, 1)),
			  truthvalue_conversion (TREE_OPERAND (expr, 2))));

    case CONVERT_EXPR:
      /* Don't cancel the effect of a CONVERT_EXPR from a REFERENCE_TYPE,
	 since that affects how `default_conversion' will behave.  */
      if (TREE_CODE (TREE_TYPE (expr)) == REFERENCE_TYPE
	  || TREE_CODE (TREE_TYPE (TREE_OPERAND (expr, 0))) == REFERENCE_TYPE)
	break;
      /* fall through... */
    case NOP_EXPR:
      /* If this is widening the argument, we can ignore it.  */
      if (TYPE_PRECISION (TREE_TYPE (expr))
	  >= TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (expr, 0))))
	return truthvalue_conversion (TREE_OPERAND (expr, 0));
      break;

    case MINUS_EXPR:
      /* With IEEE arithmetic, x - x may not equal 0, so we can't optimize
	 this case.  */
      if (TARGET_FLOAT_FORMAT == IEEE_FLOAT_FORMAT
	  && TREE_CODE (TREE_TYPE (expr)) == REAL_TYPE)
	break;
      /* fall through... */
    case BIT_XOR_EXPR:
      /* This and MINUS_EXPR can be changed into a comparison of the
	 two objects.  */
      if (TREE_TYPE (TREE_OPERAND (expr, 0))
	  == TREE_TYPE (TREE_OPERAND (expr, 1)))
	return build_binary_op (NE_EXPR, TREE_OPERAND (expr, 0),
				TREE_OPERAND (expr, 1), 1);
      return build_binary_op (NE_EXPR, TREE_OPERAND (expr, 0),
			      fold (build1 (NOP_EXPR,
					    TREE_TYPE (TREE_OPERAND (expr, 0)),
					    TREE_OPERAND (expr, 1))), 1);

    case BIT_AND_EXPR:
      if (integer_onep (TREE_OPERAND (expr, 1)))
	return expr;

    case MODIFY_EXPR:
      if (warn_parentheses && C_EXP_ORIGINAL_CODE (expr) == MODIFY_EXPR)
	warning ("suggest parentheses around assignment used as truth value");
      break;
    }

  if (TREE_CODE (TREE_TYPE (expr)) == COMPLEX_TYPE)
    return (build_binary_op
	    ((TREE_SIDE_EFFECTS (expr)
	      ? TRUTH_OR_EXPR : TRUTH_ORIF_EXPR),
	     truthvalue_conversion (build_unary_op (REALPART_EXPR, expr, 0)),
	     truthvalue_conversion (build_unary_op (IMAGPART_EXPR, expr, 0)),
	     0));

  return build_binary_op (NE_EXPR, expr, integer_zero_node, 1);
}

/* Read the rest of a #-directive from input stream FINPUT.
   In normal use, the directive name and the white space after it
   have already been read, so they won't be included in the result.
   We allow for the fact that the directive line may contain
   a newline embedded within a character or string literal which forms
   a part of the directive.

   The value is a string in a reusable buffer.  It remains valid
   only until the next time this function is called.  */

char *
get_directive_line (finput)
     register FILE *finput;
{
  static char *directive_buffer = NULL;
  static unsigned buffer_length = 0;
  register char *p;
  register char *buffer_limit;
  register int looking_for = 0;
  register int char_escaped = 0;

  if (buffer_length == 0)
    {
      directive_buffer = (char *)xmalloc (128);
      buffer_length = 128;
    }

  buffer_limit = &directive_buffer[buffer_length];

  for (p = directive_buffer; ; )
    {
      int c;

      /* Make buffer bigger if it is full.  */
      if (p >= buffer_limit)
        {
	  register unsigned bytes_used = (p - directive_buffer);

	  buffer_length *= 2;
	  directive_buffer
	    = (char *)xrealloc (directive_buffer, buffer_length);
	  p = &directive_buffer[bytes_used];
	  buffer_limit = &directive_buffer[buffer_length];
        }

      c = getc (finput);

      /* Discard initial whitespace.  */
      if ((c == ' ' || c == '\t') && p == directive_buffer)
	continue;

      /* Detect the end of the directive.  */
      if (c == '\n' && looking_for == 0)
	{
          ungetc (c, finput);
	  c = '\0';
	}

      *p++ = c;

      if (c == 0)
	return directive_buffer;

      /* Handle string and character constant syntax.  */
      if (looking_for)
	{
	  if (looking_for == c && !char_escaped)
	    looking_for = 0;	/* Found terminator... stop looking.  */
	}
      else
        if (c == '\'' || c == '"')
	  looking_for = c;	/* Don't stop buffering until we see another
				   another one of these (or an EOF).  */

      /* Handle backslash.  */
      char_escaped = (c == '\\' && ! char_escaped);
    }
}

/* Make a variant type in the proper way for C/C++, propagating qualifiers
   down to the element type of an array.  */

tree
c_build_type_variant (type, constp, volatilep)
     tree type;
     int constp, volatilep;
{
  if (TREE_CODE (type) == ARRAY_TYPE)
    {
      tree real_main_variant = TYPE_MAIN_VARIANT (type);

      push_obstacks (TYPE_OBSTACK (real_main_variant),
		     TYPE_OBSTACK (real_main_variant));
      type = build_array_type (c_build_type_variant (TREE_TYPE (type),
						     constp, volatilep),
			       TYPE_DOMAIN (type));

      /* TYPE must be on same obstack as REAL_MAIN_VARIANT.  If not,
	 make a copy.  (TYPE might have come from the hash table and
	 REAL_MAIN_VARIANT might be in some function's obstack.)  */

      if (TYPE_OBSTACK (type) != TYPE_OBSTACK (real_main_variant))
	{
	  type = copy_node (type);
	  TYPE_POINTER_TO (type) = TYPE_REFERENCE_TO (type) = 0;
	}

      TYPE_MAIN_VARIANT (type) = real_main_variant;
      pop_obstacks ();
    }
  return build_type_variant (type, constp, volatilep);
}
