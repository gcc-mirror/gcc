/* Subroutines shared by all languages that are variants of C.
   Copyright (C) 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000
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
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "tree.h"
#include "c-lex.h"
#include "c-tree.h"
#include "flags.h"
#include "toplev.h"
#include "output.h"
#include "c-pragma.h"
#include "rtl.h"
#include "ggc.h"
#include "expr.h"
#include "tm_p.h"

#if USE_CPPLIB
#include "cpplib.h"
cpp_reader  parse_in;
cpp_options parse_options;
enum cpp_token cpp_token;
#endif

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE TYPE_PRECISION (wchar_type_node)

/* The following symbols are subsumed in the c_global_trees array, and
   listed here individually for documentation purposes.

   INTEGER_TYPE and REAL_TYPE nodes for the standard data types.

	tree short_integer_type_node;
	tree long_integer_type_node;
	tree long_long_integer_type_node;

	tree short_unsigned_type_node;
	tree long_unsigned_type_node;
	tree long_long_unsigned_type_node;

	tree boolean_type_node;
	tree boolean_false_node;
	tree boolean_true_node;

	tree ptrdiff_type_node;

	tree unsigned_char_type_node;
	tree signed_char_type_node;
	tree wchar_type_node;
	tree signed_wchar_type_node;
	tree unsigned_wchar_type_node;

	tree float_type_node;
	tree double_type_node;
	tree long_double_type_node;

	tree complex_integer_type_node;
	tree complex_float_type_node;
	tree complex_double_type_node;
	tree complex_long_double_type_node;

	tree intQI_type_node;
	tree intHI_type_node;
	tree intSI_type_node;
	tree intDI_type_node;
	tree intTI_type_node;

	tree unsigned_intQI_type_node;
	tree unsigned_intHI_type_node;
	tree unsigned_intSI_type_node;
	tree unsigned_intDI_type_node;
	tree unsigned_intTI_type_node;

	tree widest_integer_literal_type_node;
	tree widest_unsigned_literal_type_node;

   Nodes for types `void *' and `const void *'.

	tree ptr_type_node, const_ptr_type_node;

   Nodes for types `char *' and `const char *'.

	tree string_type_node, const_string_type_node;

   Type `char[SOMENUMBER]'.
   Used when an array of char is needed and the size is irrelevant.

	tree char_array_type_node;

   Type `int[SOMENUMBER]' or something like it.
   Used when an array of int needed and the size is irrelevant.

	tree int_array_type_node;

   Type `wchar_t[SOMENUMBER]' or something like it.
   Used when a wide string literal is created.

	tree wchar_array_type_node;

   Type `int ()' -- used for implicit declaration of functions.

	tree default_function_type;

   Function types `int (int)', etc.

	tree int_ftype_int;
	tree void_ftype;
	tree void_ftype_ptr;
	tree int_ftype_int;
	tree ptr_ftype_sizetype;

   A VOID_TYPE node, packaged in a TREE_LIST.

	tree void_list_node;

*/

tree c_global_trees[CTI_MAX];

tree (*make_fname_decl)                PARAMS ((tree, const char *, int));

/* Nonzero means the expression being parsed will never be evaluated.
   This is a count, since unevaluated expressions can nest.  */
int skip_evaluation;

enum attrs {A_PACKED, A_NOCOMMON, A_COMMON, A_NORETURN, A_CONST, A_T_UNION,
	    A_NO_CHECK_MEMORY_USAGE, A_NO_INSTRUMENT_FUNCTION,
	    A_CONSTRUCTOR, A_DESTRUCTOR, A_MODE, A_SECTION, A_ALIGNED,
	    A_UNUSED, A_FORMAT, A_FORMAT_ARG, A_WEAK, A_ALIAS, A_MALLOC,
	    A_NO_LIMIT_STACK, A_PURE};

enum format_type { printf_format_type, scanf_format_type,
		   strftime_format_type };

static void add_attribute		PARAMS ((enum attrs, const char *,
						 int, int, int));
static void init_attributes		PARAMS ((void));
static void record_function_format	PARAMS ((tree, tree, enum format_type,
						 int, int));
static void record_international_format	PARAMS ((tree, tree, int));
static tree c_find_base_decl            PARAMS ((tree));
static int default_valid_lang_attribute PARAMS ((tree, tree, tree, tree));

/* Keep a stack of if statements.  We record the number of compound
   statements seen up to the if keyword, as well as the line number
   and file of the if.  If a potentially ambiguous else is seen, that
   fact is recorded; the warning is issued when we can be sure that
   the enclosing if statement does not have an else branch.  */
typedef struct
{
  int compstmt_count;
  int line;
  const char *file;
  int needs_warning;
} if_elt;
static void tfaff			PARAMS ((void));

static if_elt *if_stack;

/* Amount of space in the if statement stack.  */
static int if_stack_space = 0;

/* Stack pointer.  */
static int if_stack_pointer = 0;

/* Generate RTL for the start of an if-then, and record the start of it
   for ambiguous else detection.  */

void
c_expand_start_cond (cond, exitflag, compstmt_count)
     tree cond;
     int exitflag;
     int compstmt_count;
{
  /* Make sure there is enough space on the stack.  */
  if (if_stack_space == 0)
    {
      if_stack_space = 10;
      if_stack = (if_elt *)xmalloc (10 * sizeof (if_elt));
    }
  else if (if_stack_space == if_stack_pointer)
    {
      if_stack_space += 10;
      if_stack = (if_elt *)xrealloc (if_stack, if_stack_space * sizeof (if_elt));
    }

  /* Record this if statement.  */
  if_stack[if_stack_pointer].compstmt_count = compstmt_count;
  if_stack[if_stack_pointer].file = input_filename;
  if_stack[if_stack_pointer].line = lineno;
  if_stack[if_stack_pointer].needs_warning = 0;
  if_stack_pointer++;

  expand_start_cond (cond, exitflag);
}

/* Generate RTL for the end of an if-then.  Optionally warn if a nested
   if statement had an ambiguous else clause.  */

void
c_expand_end_cond ()
{
  if_stack_pointer--;
  if (if_stack[if_stack_pointer].needs_warning)
    warning_with_file_and_line (if_stack[if_stack_pointer].file,
				if_stack[if_stack_pointer].line,
				"suggest explicit braces to avoid ambiguous `else'");
  expand_end_cond ();
}

/* Generate RTL between the then-clause and the else-clause
   of an if-then-else.  */

void
c_expand_start_else ()
{
  /* An ambiguous else warning must be generated for the enclosing if
     statement, unless we see an else branch for that one, too.  */
  if (warn_parentheses
      && if_stack_pointer > 1
      && (if_stack[if_stack_pointer - 1].compstmt_count
	  == if_stack[if_stack_pointer - 2].compstmt_count))
    if_stack[if_stack_pointer - 2].needs_warning = 1;

  /* Even if a nested if statement had an else branch, it can't be
     ambiguous if this one also has an else.  So don't warn in that
     case.  Also don't warn for any if statements nested in this else.  */
  if_stack[if_stack_pointer - 1].needs_warning = 0;
  if_stack[if_stack_pointer - 1].compstmt_count--;

  expand_start_else ();
}

/* Make bindings for __FUNCTION__, __PRETTY_FUNCTION__, and __func__.  */

void
declare_function_name ()
{
  const char *name, *printable_name;

  if (current_function_decl == NULL)
    {
      name = "";
      printable_name = "top level";
    }
  else
    {
      /* Allow functions to be nameless (such as artificial ones).  */
      if (DECL_NAME (current_function_decl))
        name = IDENTIFIER_POINTER (DECL_NAME (current_function_decl));
      else
	name = "";
      printable_name = (*decl_printable_name) (current_function_decl, 2);
    }
  
  (*make_fname_decl) (get_identifier ("__FUNCTION__"), name, 0);
  (*make_fname_decl) (get_identifier ("__PRETTY_FUNCTION__"), printable_name, 1);
  /* The ISO C people "of course" couldn't use __FUNCTION__ in the
     ISO C 99 standard; instead a new variable is invented.  */
  (*make_fname_decl) (get_identifier ("__func__"), name, 0);
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

      p = ggc_alloc_string (NULL, length);

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
	      memcpy (q, TREE_STRING_POINTER (t), len);
	      q += len;
	    }
	  else
	    {
	      int i;
	      for (i = 0; i < len; i++)
		{
		  if (WCHAR_TYPE_SIZE == HOST_BITS_PER_SHORT)
		    ((short *) q)[i] = TREE_STRING_POINTER (t)[i];
		  else
		    ((int *) q)[i] = TREE_STRING_POINTER (t)[i];
		}
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
     so that copying it to a non-const pointer will get a warning.
     For C++, this is the standard behavior.  */
  if (flag_const_strings
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
  TREE_READONLY (value) = ! flag_writable_strings;
  TREE_STATIC (value) = 1;
  return value;
}

/* To speed up processing of attributes, we maintain an array of
   IDENTIFIER_NODES and the corresponding attribute types.  */

/* Array to hold attribute information.  */

static struct {enum attrs id; tree name; int min, max, decl_req;} attrtab[50];

static int attrtab_idx = 0;

/* Add an entry to the attribute table above.  */

static void
add_attribute (id, string, min_len, max_len, decl_req)
     enum attrs id;
     const char *string;
     int min_len, max_len;
     int decl_req;
{
  char buf[100];

  attrtab[attrtab_idx].id = id;
  attrtab[attrtab_idx].name = get_identifier (string);
  attrtab[attrtab_idx].min = min_len;
  attrtab[attrtab_idx].max = max_len;
  attrtab[attrtab_idx++].decl_req = decl_req;

  sprintf (buf, "__%s__", string);

  attrtab[attrtab_idx].id = id;
  attrtab[attrtab_idx].name = get_identifier (buf);
  attrtab[attrtab_idx].min = min_len;
  attrtab[attrtab_idx].max = max_len;
  attrtab[attrtab_idx++].decl_req = decl_req;
}

/* Initialize attribute table.  */

static void
init_attributes ()
{
  add_attribute (A_PACKED, "packed", 0, 0, 0);
  add_attribute (A_NOCOMMON, "nocommon", 0, 0, 1);
  add_attribute (A_COMMON, "common", 0, 0, 1);
  add_attribute (A_NORETURN, "noreturn", 0, 0, 1);
  add_attribute (A_NORETURN, "volatile", 0, 0, 1);
  add_attribute (A_UNUSED, "unused", 0, 0, 0);
  add_attribute (A_CONST, "const", 0, 0, 1);
  add_attribute (A_T_UNION, "transparent_union", 0, 0, 0);
  add_attribute (A_CONSTRUCTOR, "constructor", 0, 0, 1);
  add_attribute (A_DESTRUCTOR, "destructor", 0, 0, 1);
  add_attribute (A_MODE, "mode", 1, 1, 1);
  add_attribute (A_SECTION, "section", 1, 1, 1);
  add_attribute (A_ALIGNED, "aligned", 0, 1, 0);
  add_attribute (A_FORMAT, "format", 3, 3, 1);
  add_attribute (A_FORMAT_ARG, "format_arg", 1, 1, 1);
  add_attribute (A_WEAK, "weak", 0, 0, 1);
  add_attribute (A_ALIAS, "alias", 1, 1, 1);
  add_attribute (A_NO_INSTRUMENT_FUNCTION, "no_instrument_function", 0, 0, 1);
  add_attribute (A_NO_CHECK_MEMORY_USAGE, "no_check_memory_usage", 0, 0, 1);
  add_attribute (A_MALLOC, "malloc", 0, 0, 1);
  add_attribute (A_NO_LIMIT_STACK, "no_stack_limit", 0, 0, 1);
  add_attribute (A_PURE, "pure", 0, 0, 1);
}

/* Default implementation of valid_lang_attribute, below.  By default, there
   are no language-specific attributes.  */

static int
default_valid_lang_attribute (attr_name, attr_args, decl, type)
  tree attr_name ATTRIBUTE_UNUSED;
  tree attr_args ATTRIBUTE_UNUSED;
  tree decl ATTRIBUTE_UNUSED;
  tree type ATTRIBUTE_UNUSED;
{
  return 0;
}

/* Return a 1 if ATTR_NAME and ATTR_ARGS denote a valid language-specific
   attribute for either declaration DECL or type TYPE and 0 otherwise.  */

int (*valid_lang_attribute) PARAMS ((tree, tree, tree, tree))
     = default_valid_lang_attribute;

/* Process the attributes listed in ATTRIBUTES and PREFIX_ATTRIBUTES
   and install them in NODE, which is either a DECL (including a TYPE_DECL)
   or a TYPE.  PREFIX_ATTRIBUTES can appear after the declaration specifiers
   and declaration modifiers but before the declaration proper.  */

void
decl_attributes (node, attributes, prefix_attributes)
     tree node, attributes, prefix_attributes;
{
  tree decl = 0, type = 0;
  int is_type = 0;
  tree a;

  if (attrtab_idx == 0)
    init_attributes ();

  if (DECL_P (node))
    {
      decl = node;
      type = TREE_TYPE (decl);
      is_type = TREE_CODE (node) == TYPE_DECL;
    }
  else if (TYPE_P (node))
    type = node, is_type = 1;

#ifdef PRAGMA_INSERT_ATTRIBUTES
  /* If the code in c-pragma.c wants to insert some attributes then
     allow it to do so.  Do this before allowing machine back ends to
     insert attributes, so that they have the opportunity to override
     anything done here.  */
  PRAGMA_INSERT_ATTRIBUTES (node, & attributes, & prefix_attributes);
#endif

#ifdef INSERT_ATTRIBUTES
  INSERT_ATTRIBUTES (node, & attributes, & prefix_attributes);
#endif

  attributes = chainon (prefix_attributes, attributes);

  for (a = attributes; a; a = TREE_CHAIN (a))
    {
      tree name = TREE_PURPOSE (a);
      tree args = TREE_VALUE (a);
      int i;
      enum attrs id;

      for (i = 0; i < attrtab_idx; i++)
	if (attrtab[i].name == name)
	  break;

      if (i == attrtab_idx)
	{
	  if (! valid_machine_attribute (name, args, decl, type)
	      && ! (* valid_lang_attribute) (name, args, decl, type))
	    warning ("`%s' attribute directive ignored",
		     IDENTIFIER_POINTER (name));
	  else if (decl != 0)
	    type = TREE_TYPE (decl);
	  continue;
	}
      else if (attrtab[i].decl_req && decl == 0)
	{
	  warning ("`%s' attribute does not apply to types",
		   IDENTIFIER_POINTER (name));
	  continue;
	}
      else if (list_length (args) < attrtab[i].min
	       || list_length (args) > attrtab[i].max)
	{
	  error ("wrong number of arguments specified for `%s' attribute",
		 IDENTIFIER_POINTER (name));
	  continue;
	}

      id = attrtab[i].id;
      switch (id)
	{
	case A_PACKED:
	  if (is_type)
	    TYPE_PACKED (type) = 1;
	  else if (TREE_CODE (decl) == FIELD_DECL)
	    DECL_PACKED (decl) = 1;
	  /* We can't set DECL_PACKED for a VAR_DECL, because the bit is
	     used for DECL_REGISTER.  It wouldn't mean anything anyway.  */
	  else
	    warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
	  break;

	case A_NOCOMMON:
	  if (TREE_CODE (decl) == VAR_DECL)
	    DECL_COMMON (decl) = 0;
	  else
	    warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
	  break;

	case A_COMMON:
	  if (TREE_CODE (decl) == VAR_DECL)
	    DECL_COMMON (decl) = 1;
	  else
	    warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
	  break;

	case A_NORETURN:
	  if (TREE_CODE (decl) == FUNCTION_DECL)
	    TREE_THIS_VOLATILE (decl) = 1;
	  else if (TREE_CODE (type) == POINTER_TYPE
		   && TREE_CODE (TREE_TYPE (type)) == FUNCTION_TYPE)
	    TREE_TYPE (decl) = type
	      = build_pointer_type
		(build_type_variant (TREE_TYPE (type),
				     TREE_READONLY (TREE_TYPE (type)), 1));
	  else
	    warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
	  break;

	case A_MALLOC:
	  if (TREE_CODE (decl) == FUNCTION_DECL)
	    DECL_IS_MALLOC (decl) = 1;
	  /* ??? TODO: Support types.  */
	  else
	    warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
	  break;

	case A_UNUSED:
	  if (is_type)
	    TREE_USED (type) = 1;
	  else if (TREE_CODE (decl) == PARM_DECL
		   || TREE_CODE (decl) == VAR_DECL
		   || TREE_CODE (decl) == FUNCTION_DECL
		   || TREE_CODE (decl) == LABEL_DECL)
	    TREE_USED (decl) = 1;
	  else
	    warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
	  break;

	case A_CONST:
	  if (TREE_CODE (decl) == FUNCTION_DECL)
	    TREE_READONLY (decl) = 1;
	  else if (TREE_CODE (type) == POINTER_TYPE
		   && TREE_CODE (TREE_TYPE (type)) == FUNCTION_TYPE)
	    TREE_TYPE (decl) = type
	      = build_pointer_type
		(build_type_variant (TREE_TYPE (type), 1,
				     TREE_THIS_VOLATILE (TREE_TYPE (type))));
	  else
	    warning ( "`%s' attribute ignored", IDENTIFIER_POINTER (name));
	  break;

	case A_PURE:
	  if (TREE_CODE (decl) == FUNCTION_DECL)
	    DECL_IS_PURE (decl) = 1;
	  /* ??? TODO: Support types.  */
	  else
	    warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
	  break;


	case A_T_UNION:
	  if (is_type
	      && TREE_CODE (type) == UNION_TYPE
	      && (decl == 0
		  || (TYPE_FIELDS (type) != 0
		      && TYPE_MODE (type) == DECL_MODE (TYPE_FIELDS (type)))))
	    TYPE_TRANSPARENT_UNION (type) = 1;
	  else if (decl != 0 && TREE_CODE (decl) == PARM_DECL
		   && TREE_CODE (type) == UNION_TYPE
		   && TYPE_MODE (type) == DECL_MODE (TYPE_FIELDS (type)))
	    DECL_TRANSPARENT_UNION (decl) = 1;
	  else
	    warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
	  break;

	case A_CONSTRUCTOR:
	  if (TREE_CODE (decl) == FUNCTION_DECL
	      && TREE_CODE (type) == FUNCTION_TYPE
	      && decl_function_context (decl) == 0)
	    {
	      DECL_STATIC_CONSTRUCTOR (decl) = 1;
	      TREE_USED (decl) = 1;
	    }
	  else
	    warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
	  break;

	case A_DESTRUCTOR:
	  if (TREE_CODE (decl) == FUNCTION_DECL
	      && TREE_CODE (type) == FUNCTION_TYPE
	      && decl_function_context (decl) == 0)
	    {
	      DECL_STATIC_DESTRUCTOR (decl) = 1;
	      TREE_USED (decl) = 1;
	    }
	  else
	    warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
	  break;

	case A_MODE:
	  if (TREE_CODE (TREE_VALUE (args)) != IDENTIFIER_NODE)
	    warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
	  else
	    {
	      int j;
	      const char *p = IDENTIFIER_POINTER (TREE_VALUE (args));
	      int len = strlen (p);
	      enum machine_mode mode = VOIDmode;
	      tree typefm;

	      if (len > 4 && p[0] == '_' && p[1] == '_'
		  && p[len - 1] == '_' && p[len - 2] == '_')
		{
		  char *newp = (char *) alloca (len - 1);

		  strcpy (newp, &p[2]);
		  newp[len - 4] = '\0';
		  p = newp;
		}

	      /* Give this decl a type with the specified mode.
		 First check for the special modes.  */
	      if (! strcmp (p, "byte"))
		mode = byte_mode;
	      else if (!strcmp (p, "word"))
		mode = word_mode;
	      else if (! strcmp (p, "pointer"))
		mode = ptr_mode;
	      else
		for (j = 0; j < NUM_MACHINE_MODES; j++)
		  if (!strcmp (p, GET_MODE_NAME (j)))
		    mode = (enum machine_mode) j;

	      if (mode == VOIDmode)
		error ("unknown machine mode `%s'", p);
	      else if (0 == (typefm = type_for_mode (mode,
						     TREE_UNSIGNED (type))))
		error ("no data type for mode `%s'", p);
	      else
		{
		  TREE_TYPE (decl) = type = typefm;
		  DECL_SIZE (decl) = DECL_SIZE_UNIT (decl) = 0;
		  layout_decl (decl, 0);
		}
	    }
	  break;

	case A_SECTION:
#ifdef ASM_OUTPUT_SECTION_NAME
	  if ((TREE_CODE (decl) == FUNCTION_DECL
	       || TREE_CODE (decl) == VAR_DECL)
	      && TREE_CODE (TREE_VALUE (args)) == STRING_CST)
	    {
	      if (TREE_CODE (decl) == VAR_DECL
		  && current_function_decl != NULL_TREE
		  && ! TREE_STATIC (decl))
		error_with_decl (decl,
		  "section attribute cannot be specified for local variables");
	      /* The decl may have already been given a section attribute from
		 a previous declaration.  Ensure they match.  */
	      else if (DECL_SECTION_NAME (decl) != NULL_TREE
		       && strcmp (TREE_STRING_POINTER (DECL_SECTION_NAME (decl)),
				  TREE_STRING_POINTER (TREE_VALUE (args))) != 0)
		error_with_decl (node,
				 "section of `%s' conflicts with previous declaration");
	      else
		DECL_SECTION_NAME (decl) = TREE_VALUE (args);
	    }
	  else
	    error_with_decl (node,
			   "section attribute not allowed for `%s'");
#else
	  error_with_decl (node,
		  "section attributes are not supported for this target");
#endif
	  break;

	case A_ALIGNED:
	  {
	    tree align_expr
	      = (args ? TREE_VALUE (args)
		 : size_int (BIGGEST_ALIGNMENT / BITS_PER_UNIT));
	    int i;

	    /* Strip any NOPs of any kind.  */
	    while (TREE_CODE (align_expr) == NOP_EXPR
		   || TREE_CODE (align_expr) == CONVERT_EXPR
		   || TREE_CODE (align_expr) == NON_LVALUE_EXPR)
	      align_expr = TREE_OPERAND (align_expr, 0);

	    if (TREE_CODE (align_expr) != INTEGER_CST)
	      {
		error ("requested alignment is not a constant");
		continue;
	      }

	    if ((i = tree_log2 (align_expr)) == -1)
	      error ("requested alignment is not a power of 2");
	    else if (i > HOST_BITS_PER_INT - 2)
	      error ("requested alignment is too large");
	    else if (is_type)
	      TYPE_ALIGN (type) = (1 << i) * BITS_PER_UNIT;
	    else if (TREE_CODE (decl) != VAR_DECL
		     && TREE_CODE (decl) != FIELD_DECL)
	      error_with_decl (decl,
			       "alignment may not be specified for `%s'");
	    else
	      DECL_ALIGN (decl) = (1 << i) * BITS_PER_UNIT;
	  }
	  break;

	case A_FORMAT:
	  {
	    tree format_type_id = TREE_VALUE (args);
	    tree format_num_expr = TREE_VALUE (TREE_CHAIN (args));
	    tree first_arg_num_expr
	      = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (args)));
	    unsigned HOST_WIDE_INT format_num, first_arg_num;
	    enum format_type format_type;
	    tree argument;
	    unsigned int arg_num;

	    if (TREE_CODE (decl) != FUNCTION_DECL)
	      {
		error_with_decl (decl,
			 "argument format specified for non-function `%s'");
		continue;
	      }

	    if (TREE_CODE (format_type_id) != IDENTIFIER_NODE)
	      {
		error ("unrecognized format specifier");
		continue;
	      }
	    else
	      {
		const char *p = IDENTIFIER_POINTER (format_type_id);

		if (!strcmp (p, "printf") || !strcmp (p, "__printf__"))
		  format_type = printf_format_type;
		else if (!strcmp (p, "scanf") || !strcmp (p, "__scanf__"))
		  format_type = scanf_format_type;
		else if (!strcmp (p, "strftime")
			 || !strcmp (p, "__strftime__"))
		  format_type = strftime_format_type;
		else
		  {
		    warning ("`%s' is an unrecognized format function type", p);
		    continue;
		  }
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
		|| TREE_INT_CST_HIGH (format_num_expr) != 0
		|| TREE_CODE (first_arg_num_expr) != INTEGER_CST
		|| TREE_INT_CST_HIGH (first_arg_num_expr) != 0)
	      {
		error ("format string has invalid operand number");
		continue;
	      }

	    format_num = TREE_INT_CST_LOW (format_num_expr);
	    first_arg_num = TREE_INT_CST_LOW (first_arg_num_expr);
	    if (first_arg_num != 0 && first_arg_num <= format_num)
	      {
		error ("format string arg follows the args to be formatted");
		continue;
	      }

	    /* If a parameter list is specified, verify that the format_num
	       argument is actually a string, in case the format attribute
	       is in error.  */
	    argument = TYPE_ARG_TYPES (type);
	    if (argument)
	      {
		for (arg_num = 1; argument != 0 && arg_num != format_num;
		     ++arg_num, argument = TREE_CHAIN (argument))
		  ;

		if (! argument
		    || TREE_CODE (TREE_VALUE (argument)) != POINTER_TYPE
		  || (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_VALUE (argument)))
		      != char_type_node))
		  {
		    error ("format string arg not a string type");
		    continue;
		  }

		else if (first_arg_num != 0)
		  {
		    /* Verify that first_arg_num points to the last arg,
		       the ...  */
		    while (argument)
		      arg_num++, argument = TREE_CHAIN (argument);

		    if (arg_num != first_arg_num)
		      {
			error ("args to be formatted is not '...'");
			continue;
		      }
		  }
	      }

	    record_function_format (DECL_NAME (decl),
				    DECL_ASSEMBLER_NAME (decl),
				    format_type, format_num, first_arg_num);
	    break;
	  }

	case A_FORMAT_ARG:
	  {
	    tree format_num_expr = TREE_VALUE (args);
	    unsigned HOST_WIDE_INT format_num;
	    unsigned int arg_num;
	    tree argument;

	    if (TREE_CODE (decl) != FUNCTION_DECL)
	      {
		error_with_decl (decl,
			 "argument format specified for non-function `%s'");
		continue;
	      }

	    /* Strip any conversions from the first arg number and verify it
	       is a constant.  */
	    while (TREE_CODE (format_num_expr) == NOP_EXPR
		   || TREE_CODE (format_num_expr) == CONVERT_EXPR
		   || TREE_CODE (format_num_expr) == NON_LVALUE_EXPR)
	      format_num_expr = TREE_OPERAND (format_num_expr, 0);

	    if (TREE_CODE (format_num_expr) != INTEGER_CST
		|| TREE_INT_CST_HIGH (format_num_expr) != 0)
	      {
		error ("format string has invalid operand number");
		continue;
	      }

	    format_num = TREE_INT_CST_LOW (format_num_expr);

	    /* If a parameter list is specified, verify that the format_num
	       argument is actually a string, in case the format attribute
	       is in error.  */
	    argument = TYPE_ARG_TYPES (type);
	    if (argument)
	      {
		for (arg_num = 1; argument != 0 && arg_num != format_num;
		     ++arg_num, argument = TREE_CHAIN (argument))
		  ;

		if (! argument
		    || TREE_CODE (TREE_VALUE (argument)) != POINTER_TYPE
		  || (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_VALUE (argument)))
		      != char_type_node))
		  {
		    error ("format string arg not a string type");
		    continue;
		  }
	      }

	    if (TREE_CODE (TREE_TYPE (TREE_TYPE (decl))) != POINTER_TYPE
		|| (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (TREE_TYPE (decl))))
		    != char_type_node))
	      {
		error ("function does not return string type");
		continue;
	      }

	    record_international_format (DECL_NAME (decl),
					 DECL_ASSEMBLER_NAME (decl),
					 format_num);
	    break;
	  }

	case A_WEAK:
	  declare_weak (decl);
	  break;

	case A_ALIAS:
	  if ((TREE_CODE (decl) == FUNCTION_DECL && DECL_INITIAL (decl))
	      || (TREE_CODE (decl) != FUNCTION_DECL && ! DECL_EXTERNAL (decl)))
	    error_with_decl (decl,
			     "`%s' defined both normally and as an alias");
	  else if (decl_function_context (decl) == 0)
	    {
	      tree id;

	      id = TREE_VALUE (args);
	      if (TREE_CODE (id) != STRING_CST)
		{
		  error ("alias arg not a string");
		  break;
		}
	      id = get_identifier (TREE_STRING_POINTER (id));

	      if (TREE_CODE (decl) == FUNCTION_DECL)
		DECL_INITIAL (decl) = error_mark_node;
	      else
		DECL_EXTERNAL (decl) = 0;
	      assemble_alias (decl, id);
	    }
	  else
	    warning ("`%s' attribute ignored", IDENTIFIER_POINTER (name));
	  break;

	case A_NO_CHECK_MEMORY_USAGE:
	  if (TREE_CODE (decl) != FUNCTION_DECL)
	    {
	      error_with_decl (decl,
			       "`%s' attribute applies only to functions",
			       IDENTIFIER_POINTER (name));
	    }
	  else if (DECL_INITIAL (decl))
	    {
	      error_with_decl (decl,
			       "can't set `%s' attribute after definition",
			       IDENTIFIER_POINTER (name));
	    }
	  else
	    DECL_NO_CHECK_MEMORY_USAGE (decl) = 1;
	  break;

	case A_NO_INSTRUMENT_FUNCTION:
	  if (TREE_CODE (decl) != FUNCTION_DECL)
	    {
	      error_with_decl (decl,
			       "`%s' attribute applies only to functions",
			       IDENTIFIER_POINTER (name));
	    }
	  else if (DECL_INITIAL (decl))
	    {
	      error_with_decl (decl,
			       "can't set `%s' attribute after definition",
			       IDENTIFIER_POINTER (name));
	    }
	  else
	    DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT (decl) = 1;
	  break;

        case A_NO_LIMIT_STACK:
	  if (TREE_CODE (decl) != FUNCTION_DECL)
	    {
	      error_with_decl (decl,
			       "`%s' attribute applies only to functions",
			       IDENTIFIER_POINTER (name));
	    }
	  else if (DECL_INITIAL (decl))
	    {
	      error_with_decl (decl,
			       "can't set `%s' attribute after definition",
			       IDENTIFIER_POINTER (name));
	    }
	  else
	    DECL_NO_LIMIT_STACK (decl) = 1;
	  break;
	}
    }
}

/* Split SPECS_ATTRS, a list of declspecs and prefix attributes, into two
   lists.  SPECS_ATTRS may also be just a typespec (eg: RECORD_TYPE).

   The head of the declspec list is stored in DECLSPECS.
   The head of the attribute list is stored in PREFIX_ATTRIBUTES.

   Note that attributes in SPECS_ATTRS are stored in the TREE_PURPOSE of
   the list elements.  We drop the containing TREE_LIST nodes and link the
   resulting attributes together the way decl_attributes expects them.  */

void
split_specs_attrs (specs_attrs, declspecs, prefix_attributes)
     tree specs_attrs;
     tree *declspecs, *prefix_attributes;
{
  tree t, s, a, next, specs, attrs;

  /* This can happen after an __extension__ in pedantic mode.  */
  if (specs_attrs != NULL_TREE 
      && TREE_CODE (specs_attrs) == INTEGER_CST)
    {
      *declspecs = NULL_TREE;
      *prefix_attributes = NULL_TREE;
      return;
    }

  /* This can happen in c++ (eg: decl: typespec initdecls ';').  */
  if (specs_attrs != NULL_TREE
      && TREE_CODE (specs_attrs) != TREE_LIST)
    {
      *declspecs = specs_attrs;
      *prefix_attributes = NULL_TREE;
      return;
    }

  /* Remember to keep the lists in the same order, element-wise.  */

  specs = s = NULL_TREE;
  attrs = a = NULL_TREE;
  for (t = specs_attrs; t; t = next)
    {
      next = TREE_CHAIN (t);
      /* Declspecs have a non-NULL TREE_VALUE.  */
      if (TREE_VALUE (t) != NULL_TREE)
	{
	  if (specs == NULL_TREE)
	    specs = s = t;
	  else
	    {
	      TREE_CHAIN (s) = t;
	      s = t;
	    }
	}
      else
	{
	  if (attrs == NULL_TREE)
	    attrs = a = TREE_PURPOSE (t);
	  else
	    {
	      TREE_CHAIN (a) = TREE_PURPOSE (t);
	      a = TREE_PURPOSE (t);
	    }
	  /* More attrs can be linked here, move A to the end.  */
	  while (TREE_CHAIN (a) != NULL_TREE)
	    a = TREE_CHAIN (a);
	}
    }

  /* Terminate the lists.  */
  if (s != NULL_TREE)
    TREE_CHAIN (s) = NULL_TREE;
  if (a != NULL_TREE)
    TREE_CHAIN (a) = NULL_TREE;

  /* All done.  */
  *declspecs = specs;
  *prefix_attributes = attrs;
}

/* Strip attributes from SPECS_ATTRS, a list of declspecs and attributes.
   This function is used by the parser when a rule will accept attributes
   in a particular position, but we don't want to support that just yet.

   A warning is issued for every ignored attribute.  */

tree
strip_attrs (specs_attrs)
     tree specs_attrs;
{
  tree specs, attrs;

  split_specs_attrs (specs_attrs, &specs, &attrs);

  while (attrs)
    {
      warning ("`%s' attribute ignored",
	       IDENTIFIER_POINTER (TREE_PURPOSE (attrs)));
      attrs = TREE_CHAIN (attrs);
    }

  return specs;
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
#define T_UC	&unsigned_char_type_node
#define T_V	&void_type_node
#define T_W	&wchar_type_node
#define T_ST    &sizetype

typedef struct {
  const char *format_chars;
  int pointer_count;
  /* Type of argument if no length modifier is used.  */
  tree *nolen;
  /* Type of argument if length modifier for shortening to byte is used.
     If NULL, then this modifier is not allowed.  */
  tree *hhlen;
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
  /* Type of argument if length modifiers 'z' or `Z' is used.
     If NULL, then this modifier is not allowed.  */
  tree *zlen;
  /* List of other modifier characters allowed with these options.  */
  const char *flag_chars;
} format_char_info;

static format_char_info print_char_table[] = {
  { "di",	0,	T_I,	T_I,	T_I,	T_L,	T_LL,	T_LL,	T_ST,	"-wp0 +"	},
  { "oxX",	0,	T_UI,	T_UI,	T_UI,	T_UL,	T_ULL,	T_ULL,	T_ST,	"-wp0#"		},
  { "u",	0,	T_UI,	T_UI,	T_UI,	T_UL,	T_ULL,	T_ULL,	T_ST,	"-wp0"		},
/* A GNU extension.  */
  { "m",	0,	T_V,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	"-wp"		},
  { "feEgGaA",	0,	T_D,	NULL,	NULL,	NULL,	NULL,	T_LD,	NULL,	"-wp0 +#"	},
  { "c",	0,	T_I,	NULL,	NULL,	T_W,	NULL,	NULL,	NULL,	"-w"		},
  { "C",	0,	T_W,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	"-w"		},
  { "s",	1,	T_C,	NULL,	NULL,	T_W,	NULL,	NULL,	NULL,	"-wp"		},
  { "S",	1,	T_W,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	"-wp"		},
  { "p",	1,	T_V,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	"-w"		},
  { "n",	1,	T_I,	NULL,	T_S,	T_L,	T_LL,	NULL,	NULL,	""		},
  { NULL,	0,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL		}
};

static format_char_info scan_char_table[] = {
  { "di",	1,	T_I,	T_C,	T_S,	T_L,	T_LL,	T_LL,	NULL,	"*"	},
  { "ouxX",	1,	T_UI,	T_UC,	T_US,	T_UL,	T_ULL,	T_ULL,	NULL,	"*"	},
  { "efgEGaA",	1,	T_F,	NULL,	NULL,	T_D,	NULL,	T_LD,	NULL,	"*"	},
  { "c",	1,	T_C,	NULL,	NULL,	T_W,	NULL,	NULL,	NULL,	"*"	},
  { "s",	1,	T_C,	NULL,	NULL,	T_W,	NULL,	NULL,	NULL,	"*a"	},
  { "[",	1,	T_C,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	"*a"	},
  { "C",	1,	T_W,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	"*"	},
  { "S",	1,	T_W,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	"*a"	},
  { "p",	2,	T_V,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	"*"	},
  { "n",	1,	T_I,	T_C,	T_S,	T_L,	T_LL,	NULL,	NULL,	""	},
  { NULL,	0,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL	}
};

/* Handle format characters recognized by glibc's strftime.c.
   '2' - MUST do years as only two digits
   '3' - MAY do years as only two digits (depending on locale)
   'E' - E modifier is acceptable
   'O' - O modifier is acceptable to Standard C
   'o' - O modifier is acceptable as a GNU extension
   'G' - other GNU extensions  */

static format_char_info time_char_table[] = {
  { "y", 		0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, "2EO-_0w" },
  { "D", 		0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, "2" },
  { "g", 		0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, "2O-_0w" },
  { "cx", 		0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, "3E" },
  { "%RTXnrt",		0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, "" },
  { "P",		0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, "G" },
  { "HIMSUWdemw",	0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, "-_0Ow" },
  { "Vju",		0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, "-_0Oow" },
  { "Gklsz",		0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, "-_0OGw" },
  { "ABZa",		0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, "^#" },
  { "p",		0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, "#" },
  { "bh",		0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, "^" },
  { "CY",		0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, "-_0EOw" },
  { NULL,		0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL }
};

typedef struct function_format_info
{
  struct function_format_info *next;  /* next structure on the list */
  tree name;			/* identifier such as "printf" */
  tree assembler_name;		/* optional mangled identifier (for C++) */
  enum format_type format_type;	/* type of format (printf, scanf, etc.) */
  int format_num;		/* number of format argument */
  int first_arg_num;		/* number of first arg (zero for varargs) */
} function_format_info;

static function_format_info *function_format_list = NULL;

typedef struct international_format_info
{
  struct international_format_info *next;  /* next structure on the list */
  tree name;			/* identifier such as "gettext" */
  tree assembler_name;		/* optional mangled identifier (for C++) */
  int format_num;		/* number of format argument */
} international_format_info;

static international_format_info *international_format_list = NULL;

static void check_format_info	PARAMS ((function_format_info *, tree));

/* Initialize the table of functions to perform format checking on.
   The ANSI functions are always checked (whether <stdio.h> is
   included or not), since it is common to call printf without
   including <stdio.h>.  There shouldn't be a problem with this,
   since ANSI reserves these function names whether you include the
   header file or not.  In any case, the checking is harmless.

   Also initialize the name of function that modify the format string for
   internationalization purposes.  */

void
init_function_format_info ()
{
  record_function_format (get_identifier ("printf"), NULL_TREE,
			  printf_format_type, 1, 2);
  record_function_format (get_identifier ("fprintf"), NULL_TREE,
			  printf_format_type, 2, 3);
  record_function_format (get_identifier ("sprintf"), NULL_TREE,
			  printf_format_type, 2, 3);
  record_function_format (get_identifier ("scanf"), NULL_TREE,
			  scanf_format_type, 1, 2);
  record_function_format (get_identifier ("fscanf"), NULL_TREE,
			  scanf_format_type, 2, 3);
  record_function_format (get_identifier ("sscanf"), NULL_TREE,
			  scanf_format_type, 2, 3);
  record_function_format (get_identifier ("vprintf"), NULL_TREE,
			  printf_format_type, 1, 0);
  record_function_format (get_identifier ("vfprintf"), NULL_TREE,
			  printf_format_type, 2, 0);
  record_function_format (get_identifier ("vsprintf"), NULL_TREE,
			  printf_format_type, 2, 0);
  record_function_format (get_identifier ("strftime"), NULL_TREE,
			  strftime_format_type, 3, 0);

  record_international_format (get_identifier ("gettext"), NULL_TREE, 1);
  record_international_format (get_identifier ("dgettext"), NULL_TREE, 2);
  record_international_format (get_identifier ("dcgettext"), NULL_TREE, 2);
}

/* Record information for argument format checking.  FUNCTION_IDENT is
   the identifier node for the name of the function to check (its decl
   need not exist yet).
   FORMAT_TYPE specifies the type of format checking.  FORMAT_NUM is the number
   of the argument which is the format control string (starting from 1).
   FIRST_ARG_NUM is the number of the first actual argument to check
   against the format string, or zero if no checking is not be done
   (e.g. for varargs such as vfprintf).  */

static void
record_function_format (name, assembler_name, format_type,
			format_num, first_arg_num)
      tree name;
      tree assembler_name;
      enum format_type format_type;
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

  info->format_type = format_type;
  info->format_num = format_num;
  info->first_arg_num = first_arg_num;
}

/* Record information for the names of function that modify the format
   argument to format functions.  FUNCTION_IDENT is the identifier node for
   the name of the function (its decl need not exist yet) and FORMAT_NUM is
   the number of the argument which is the format control string (starting
   from 1).  */

static void
record_international_format (name, assembler_name, format_num)
      tree name;
      tree assembler_name;
      int format_num;
{
  international_format_info *info;

  /* Re-use existing structure if it's there.  */

  for (info = international_format_list; info; info = info->next)
    {
      if (info->name == name && info->assembler_name == assembler_name)
	break;
    }

  if (! info)
    {
      info
	= (international_format_info *)
	  xmalloc (sizeof (international_format_info));
      info->next = international_format_list;
      international_format_list = info;

      info->name = name;
      info->assembler_name = assembler_name;
    }

  info->format_num = format_num;
}

static void
tfaff ()
{
  warning ("too few arguments for format");
}

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
  int length_char = 0;
  int format_char;
  int format_length;
  tree format_tree;
  tree cur_param;
  tree cur_type;
  tree wanted_type;
  tree first_fillin_param;
  const char *format_chars;
  format_char_info *fci = NULL;
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

  if (TREE_CODE (format_tree) == CALL_EXPR
      && TREE_CODE (TREE_OPERAND (format_tree, 0)) == ADDR_EXPR
      && (TREE_CODE (TREE_OPERAND (TREE_OPERAND (format_tree, 0), 0))
	  == FUNCTION_DECL))
    {
      tree function = TREE_OPERAND (TREE_OPERAND (format_tree, 0), 0);

      /* See if this is a call to a known internationalization function
	 that modifies the format arg.  */
      international_format_info *info;

      for (info = international_format_list; info; info = info->next)
	if (info->assembler_name
	    ? (info->assembler_name == DECL_ASSEMBLER_NAME (function))
	    : (info->name == DECL_NAME (function)))
	  {
	    tree inner_args;
	    int i;

	    for (inner_args = TREE_OPERAND (format_tree, 1), i = 1;
		 inner_args != 0;
		 inner_args = TREE_CHAIN (inner_args), i++)
	      if (i == info->format_num)
		{
		  format_tree = TREE_VALUE (inner_args);

		  while (TREE_CODE (format_tree) == NOP_EXPR)
		    format_tree = TREE_OPERAND (format_tree, 0);
		}
	  }
    }

  if (integer_zerop (format_tree))
    {
      warning ("null format string");
      return;
    }
  if (TREE_CODE (format_tree) != ADDR_EXPR)
    {
      /* The user may get multiple warnings if the supplied argument
	 isn't even a string pointer.  */
      /* Functions taking a va_list normally pass a non-literal format
	 string.  These functions typically are declared with
	 first_arg_num == 0, so avoid warning in those cases.  */
      if (info->first_arg_num != 0 && warn_format > 1)
	warning ("format not a string literal, argument types not checked");
      return;
    }
  format_tree = TREE_OPERAND (format_tree, 0);
  if (TREE_CODE (format_tree) != STRING_CST)
    {
      /* The user may get multiple warnings if the supplied argument
	 isn't even a string pointer.  */
      /* Functions taking a va_list normally pass a non-literal format
	 string.  These functions typically are declared with
	 first_arg_num == 0, so avoid warning in those cases.  */
      if (info->first_arg_num != 0 && warn_format > 1)
	warning ("format not a string literal, argument types not checked");
      return;
    }
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
      if (info->format_type == scanf_format_type)
	{
	  suppressed = *format_chars == '*';
	  if (suppressed)
	    ++format_chars;
	  while (ISDIGIT (*format_chars))
	    ++format_chars;
	}
      else if (info->format_type == strftime_format_type)
        {
	  while (*format_chars != 0 && index ("_-0^#", *format_chars) != 0)
	    {
	      if (pedantic)
		warning ("ANSI C does not support the strftime `%c' flag",
			 *format_chars);
	      if (index (flag_chars, *format_chars) != 0)
		{
		  warning ("repeated `%c' flag in format",
			   *format_chars);
		  ++format_chars;
		}
	      else
		{
		  i = strlen (flag_chars);
		  flag_chars[i++] = *format_chars++;
		  flag_chars[i] = 0;
		}
	    }
	  while (ISDIGIT ((unsigned char) *format_chars))
	    {
	      wide = TRUE;
              ++format_chars;
	    }
	  if (wide && pedantic)
	    warning ("ANSI C does not support strftime format width");
	  if (*format_chars == 'E' || *format_chars == 'O')
	    {
	      i = strlen (flag_chars);
	      flag_chars[i++] = *format_chars++;
	      flag_chars[i] = 0;
	      if (*format_chars == 'E' || *format_chars == 'O')
	        {
		  warning ("multiple E/O modifiers in format");
		  while (*format_chars == 'E' || *format_chars == 'O')
		    ++format_chars;
		}
	    }
	}
      else if (info->format_type == printf_format_type)
	{
	  /* See if we have a number followed by a dollar sign.  If we do,
	     it is an operand number, so set PARAMS to that operand.  */
	  if (*format_chars >= '0' && *format_chars <= '9')
	    {
	      const char *p = format_chars;

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
		warning ("repeated `%c' flag in format", *format_chars++);
	      else
		{
		  i = strlen (flag_chars);
		  flag_chars[i++] = *format_chars++;
		  flag_chars[i] = 0;
		}
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
		  tfaff ();
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
		    warning ("field width is not type int (arg %d)", arg_num);
		}
	    }
	  else
	    {
	      while (ISDIGIT (*format_chars))
		{
		  wide = TRUE;
		  ++format_chars;
		}
	    }
	  if (*format_chars == '.')
	    {
	      precise = TRUE;
	      ++format_chars;
	      if (*format_chars != '*' && !ISDIGIT (*format_chars))
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
			  tfaff ();
			  return;
			}
		      cur_param = TREE_VALUE (params);
		      params = TREE_CHAIN (params);
		      ++arg_num;
		      if (TYPE_MAIN_VARIANT (TREE_TYPE (cur_param))
			  != integer_type_node)
			warning ("field width is not type int (arg %d)",
				 arg_num);
		    }
		}
	      else
		{
		  while (ISDIGIT (*format_chars))
		    ++format_chars;
		}
	    }
	}

      aflag = 0;

      if (info->format_type != strftime_format_type)
	{
	  if (*format_chars == 'h' || *format_chars == 'l')
	    length_char = *format_chars++;
	  else if (*format_chars == 'q' || *format_chars == 'L')
	    {
	      length_char = *format_chars++;
	      if (pedantic)
		warning ("ANSI C does not support the `%c' length modifier",
			 length_char);
	    }
	  else if (*format_chars == 'Z' || *format_chars == 'z')
	    {
	      length_char = *format_chars++;
	      if (pedantic && (length_char == 'Z' || !flag_isoc99))
		warning ("ANSI C does not support the `%c' length modifier",
			 length_char);
	    }
	  else
	    length_char = 0;
	  if (length_char == 'l' && *format_chars == 'l')
	    {
	      length_char = 'q', format_chars++;
	      if (pedantic && !flag_isoc99)
		warning ("ANSI C does not support the `ll' length modifier");
	    }
	  else if (length_char == 'h' && *format_chars == 'h')
	    {
	      length_char = 'H', format_chars++;
	      if (pedantic && !flag_isoc99)
		warning ("ANSI C does not support the `hh' length modifier");
	    }
	  if (*format_chars == 'a' && info->format_type == scanf_format_type)
	    {
	      if (format_chars[1] == 's' || format_chars[1] == 'S'
		  || format_chars[1] == '[')
		{
		  /* `a' is used as a flag.  */
		  aflag = 1;
		  format_chars++;
		}
	    }
	  if (suppressed && length_char != 0)
	    warning ("use of `*' and `%c' together in format", length_char);
	}
      format_char = *format_chars;
      if (format_char == 0
	  || (info->format_type != strftime_format_type && format_char == '%'))
	{
	  warning ("conversion lacks type at end of format");
	  continue;
	}
      /* The m, C, and S formats are GNU extensions.  */
      if (pedantic && info->format_type != strftime_format_type
	  && (format_char == 'm' || format_char == 'C' || format_char == 'S'))
	warning ("ANSI C does not support the `%c' format", format_char);
      /* The a and A formats are C99 extensions.  */
      if (pedantic && info->format_type != strftime_format_type
	  && (format_char == 'a' || format_char == 'A')
	  && !flag_isoc99)
	warning ("ANSI C does not support the `%c' format", format_char);
      format_chars++;
      switch (info->format_type)
	{
	case printf_format_type:
	  fci = print_char_table;
	  break;
	case scanf_format_type:
	  fci = scan_char_table;
	  break;
	case strftime_format_type:
	  fci = time_char_table;
	  break;
	default:
	  abort ();
	}
      while (fci->format_chars != 0
	     && index (fci->format_chars, format_char) == 0)
	  ++fci;
      if (fci->format_chars == 0)
	{
          if (ISGRAPH(format_char))
	    warning ("unknown conversion type character `%c' in format",
		     format_char);
	  else
	    warning ("unknown conversion type character 0x%x in format",
		     format_char);
	  continue;
	}
      if (pedantic)
	{
	  if (index (fci->flag_chars, 'G') != 0)
	    warning ("ANSI C does not support `%%%c'", format_char);
	  if (index (fci->flag_chars, 'o') != 0
	      && index (flag_chars, 'O') != 0)
	    warning ("ANSI C does not support `%%O%c'", format_char);
	}
      if (wide && index (fci->flag_chars, 'w') == 0)
	warning ("width used with `%c' format", format_char);
      if (index (fci->flag_chars, '2') != 0)
	warning ("`%%%c' yields only last 2 digits of year", format_char);
      else if (index (fci->flag_chars, '3') != 0)
	warning ("`%%%c' yields only last 2 digits of year in some locales",
		 format_char);
      if (precise && index (fci->flag_chars, 'p') == 0)
	warning ("precision used with `%c' format", format_char);
      if (aflag && index (fci->flag_chars, 'a') == 0)
	{
	  warning ("`a' flag used with `%c' format", format_char);
	  /* To simplify the following code.  */
	  aflag = 0;
	}
      /* The a flag is a GNU extension.  */
      else if (pedantic && aflag)
	warning ("ANSI C does not support the `a' flag");
      if (info->format_type == scanf_format_type && format_char == '[')
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
	    warning ("suppression of `%c' conversion in format", format_char);
	  continue;
	}
      for (i = 0; flag_chars[i] != 0; ++i)
	{
	  if (index (fci->flag_chars, flag_chars[i]) == 0)
	    warning ("flag `%c' used with type `%c'",
		     flag_chars[i], format_char);
	}
      if (info->format_type == strftime_format_type)
	continue;
      if (precise && index (flag_chars, '0') != 0
	  && (format_char == 'd' || format_char == 'i'
	      || format_char == 'o' || format_char == 'u'
	      || format_char == 'x' || format_char == 'X'))
	warning ("`0' flag ignored with precision specifier and `%c' format",
		 format_char);
      switch (length_char)
	{
	default: wanted_type = fci->nolen ? *(fci->nolen) : 0; break;
	case 'H': wanted_type = fci->hhlen ? *(fci->hhlen) : 0; break;
	case 'h': wanted_type = fci->hlen ? *(fci->hlen) : 0; break;
	case 'l': wanted_type = fci->llen ? *(fci->llen) : 0; break;
	case 'q': wanted_type = fci->qlen ? *(fci->qlen) : 0; break;
	case 'L': wanted_type = fci->bigllen ? *(fci->bigllen) : 0; break;
	case 'z': case 'Z': wanted_type = fci->zlen ? *fci->zlen : 0; break;
	}
      if (wanted_type == 0)
	warning ("use of `%c' length character with `%c' type character",
		 length_char, format_char);

      /* Finally. . .check type of argument against desired type!  */
      if (info->first_arg_num == 0)
	continue;
      if (fci->pointer_count == 0 && wanted_type == void_type_node)
	/* This specifier takes no argument.  */
	continue;
      if (params == 0)
	{
	  tfaff ();
	  return;
	}
      cur_param = TREE_VALUE (params);
      params = TREE_CHAIN (params);
      ++arg_num;
      cur_type = TREE_TYPE (cur_param);

      STRIP_NOPS (cur_param);

      /* Check the types of any additional pointer arguments
	 that precede the "real" argument.  */
      for (i = 0; i < fci->pointer_count + aflag; ++i)
	{
	  if (TREE_CODE (cur_type) == POINTER_TYPE)
	    {
	      cur_type = TREE_TYPE (cur_type);

	      if (cur_param != 0 && TREE_CODE (cur_param) == ADDR_EXPR)
		cur_param = TREE_OPERAND (cur_param, 0);
	      else
		cur_param = 0;

	      continue;
	    }
	  if (TREE_CODE (cur_type) != ERROR_MARK)
	    {
	      if (fci->pointer_count + aflag == 1)
		warning ("format argument is not a pointer (arg %d)", arg_num);
	      else
		warning ("format argument is not a pointer to a pointer (arg %d)", arg_num);
	    }
	  break;
	}

      /* See if this is an attempt to write into a const type with
	 scanf or with printf "%n".  */
      if ((info->format_type == scanf_format_type
	   || (info->format_type == printf_format_type
	       && format_char == 'n'))
	  && i == fci->pointer_count + aflag
	  && wanted_type != 0
	  && TREE_CODE (cur_type) != ERROR_MARK
	  && (TYPE_READONLY (cur_type)
	      || (cur_param != 0
		  && (TREE_CODE_CLASS (TREE_CODE (cur_param)) == 'c'
		      || (DECL_P (cur_param) && TREE_READONLY (cur_param))))))
	warning ("writing into constant object (arg %d)", arg_num);

      /* Check the type of the "real" argument, if there's a type we want.  */
      if (i == fci->pointer_count + aflag && wanted_type != 0
	  && TREE_CODE (cur_type) != ERROR_MARK
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
	  register const char *this;
	  register const char *that;

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
	    warning ("%s format, %s arg (arg %d)", this, that, arg_num);
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
      if (skip_evaluation == 0)
	warning ("integer overflow in expression");
    }
  else if ((TREE_CODE (value) == REAL_CST
	    || (TREE_CODE (value) == COMPLEX_CST
		&& TREE_CODE (TREE_REALPART (value)) == REAL_CST))
	   && TREE_OVERFLOW (value))
    {
      TREE_OVERFLOW (value) = 0;
      if (skip_evaluation == 0)
	warning ("floating point overflow in expression");
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
      && skip_evaluation == 0
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

	  /* Do not diagnose overflow in a constant expression merely
	     because a conversion overflowed.  */
	  TREE_CONSTANT_OVERFLOW (t) = TREE_CONSTANT_OVERFLOW (expr);

	  /* No warning for converting 0x80000000 to int.  */
	  if (!(TREE_UNSIGNED (type) < TREE_UNSIGNED (TREE_TYPE (expr))
		&& TREE_CODE (TREE_TYPE (expr)) == INTEGER_TYPE
		&& TYPE_PRECISION (type) == TYPE_PRECISION (TREE_TYPE (expr))))
	    /* If EXPR fits in the unsigned version of TYPE,
	       don't warn unless pedantic.  */
	    if ((pedantic
		 || TREE_UNSIGNED (type)
		 || ! int_fits_type_p (expr, unsigned_type (type)))
	        && skip_evaluation == 0)
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
      && !COMPLETE_OR_VOID_TYPE_P (TREE_TYPE (expr))
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
  if (bits == TYPE_PRECISION (integer_type_node))
    return unsignedp ? unsigned_type_node : integer_type_node;

  if (bits == TYPE_PRECISION (signed_char_type_node))
    return unsignedp ? unsigned_char_type_node : signed_char_type_node;

  if (bits == TYPE_PRECISION (short_integer_type_node))
    return unsignedp ? short_unsigned_type_node : short_integer_type_node;

  if (bits == TYPE_PRECISION (long_integer_type_node))
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;

  if (bits == TYPE_PRECISION (long_long_integer_type_node))
    return (unsignedp ? long_long_unsigned_type_node
	    : long_long_integer_type_node);

  if (bits == TYPE_PRECISION (widest_integer_literal_type_node))
    return (unsignedp ? widest_unsigned_literal_type_node
	    : widest_integer_literal_type_node);

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
  if (mode == TYPE_MODE (integer_type_node))
    return unsignedp ? unsigned_type_node : integer_type_node;

  if (mode == TYPE_MODE (signed_char_type_node))
    return unsignedp ? unsigned_char_type_node : signed_char_type_node;

  if (mode == TYPE_MODE (short_integer_type_node))
    return unsignedp ? short_unsigned_type_node : short_integer_type_node;

  if (mode == TYPE_MODE (long_integer_type_node))
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;

  if (mode == TYPE_MODE (long_long_integer_type_node))
    return unsignedp ? long_long_unsigned_type_node : long_long_integer_type_node;

  if (mode == TYPE_MODE (widest_integer_literal_type_node))
    return unsignedp ? widest_unsigned_literal_type_node
                     : widest_integer_literal_type_node;

  if (mode == TYPE_MODE (intQI_type_node))
    return unsignedp ? unsigned_intQI_type_node : intQI_type_node;

  if (mode == TYPE_MODE (intHI_type_node))
    return unsignedp ? unsigned_intHI_type_node : intHI_type_node;

  if (mode == TYPE_MODE (intSI_type_node))
    return unsignedp ? unsigned_intSI_type_node : intSI_type_node;

  if (mode == TYPE_MODE (intDI_type_node))
    return unsignedp ? unsigned_intDI_type_node : intDI_type_node;

#if HOST_BITS_PER_WIDE_INT >= 64
  if (mode == TYPE_MODE (intTI_type_node))
    return unsignedp ? unsigned_intTI_type_node : intTI_type_node;
#endif

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

/* Return an unsigned type the same as TYPE in other respects. */
tree
unsigned_type (type)
     tree type;
{
  tree type1 = TYPE_MAIN_VARIANT (type);
  if (type1 == signed_char_type_node || type1 == char_type_node)
    return unsigned_char_type_node;
  if (type1 == integer_type_node)
    return unsigned_type_node;
  if (type1 == short_integer_type_node)
    return short_unsigned_type_node;
  if (type1 == long_integer_type_node)
    return long_unsigned_type_node;
  if (type1 == long_long_integer_type_node)
    return long_long_unsigned_type_node;
  if (type1 == widest_integer_literal_type_node)
    return widest_unsigned_literal_type_node;
#if HOST_BITS_PER_WIDE_INT >= 64
  if (type1 == intTI_type_node)
    return unsigned_intTI_type_node;
#endif
  if (type1 == intDI_type_node)
    return unsigned_intDI_type_node;
  if (type1 == intSI_type_node)
    return unsigned_intSI_type_node;
  if (type1 == intHI_type_node)
    return unsigned_intHI_type_node;
  if (type1 == intQI_type_node)
    return unsigned_intQI_type_node;

  return signed_or_unsigned_type (1, type);
}

/* Return a signed type the same as TYPE in other respects.  */

tree
signed_type (type)
     tree type;
{
  tree type1 = TYPE_MAIN_VARIANT (type);
  if (type1 == unsigned_char_type_node || type1 == char_type_node)
    return signed_char_type_node;
  if (type1 == unsigned_type_node)
    return integer_type_node;
  if (type1 == short_unsigned_type_node)
    return short_integer_type_node;
  if (type1 == long_unsigned_type_node)
    return long_integer_type_node;
  if (type1 == long_long_unsigned_type_node)
    return long_long_integer_type_node;
  if (type1 == widest_unsigned_literal_type_node)
    return widest_integer_literal_type_node;
#if HOST_BITS_PER_WIDE_INT >= 64
  if (type1 == unsigned_intTI_type_node)
    return intTI_type_node;
#endif
  if (type1 == unsigned_intDI_type_node)
    return intDI_type_node;
  if (type1 == unsigned_intSI_type_node)
    return intSI_type_node;
  if (type1 == unsigned_intHI_type_node)
    return intHI_type_node;
  if (type1 == unsigned_intQI_type_node)
    return intQI_type_node;

  return signed_or_unsigned_type (0, type);
}

/* Return a type the same as TYPE except unsigned or
   signed according to UNSIGNEDP.  */

tree
signed_or_unsigned_type (unsignedp, type)
     int unsignedp;
     tree type;
{
  if (! INTEGRAL_TYPE_P (type)
      || TREE_UNSIGNED (type) == unsignedp)
    return type;

  if (TYPE_PRECISION (type) == TYPE_PRECISION (signed_char_type_node))
    return unsignedp ? unsigned_char_type_node : signed_char_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (integer_type_node))
    return unsignedp ? unsigned_type_node : integer_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (short_integer_type_node))
    return unsignedp ? short_unsigned_type_node : short_integer_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (long_integer_type_node))
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (long_long_integer_type_node))
    return (unsignedp ? long_long_unsigned_type_node
	    : long_long_integer_type_node);
  if (TYPE_PRECISION (type) == TYPE_PRECISION (widest_integer_literal_type_node))
    return (unsignedp ? widest_unsigned_literal_type_node
	    : widest_integer_literal_type_node);
  return type;
}

/* Return the minimum number of bits needed to represent VALUE in a
   signed or unsigned type, UNSIGNEDP says which.  */

unsigned int
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
  else
    log = tree_floor_log2 (value);

  return log + 1 + ! unsignedp;
}

/* Print an error message for invalid operands to arith operation CODE.
   NOP_EXPR is used as a special case (see truthvalue_conversion).  */

void
binary_op_error (code)
     enum tree_code code;
{
  register const char *opname;

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
    default:
      opname = "unknown"; break;
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
	default:
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

      /* If TYPE is an enumeration, then we need to get its min/max
	 values from it's underlying integral type, not the enumerated
	 type itself.  */
      if (TREE_CODE (type) == ENUMERAL_TYPE)
	type = type_for_size (TYPE_PRECISION (type), unsignedp0);

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
	    val = boolean_true_node;
	}
      else if (code == EQ_EXPR)
	{
	  if (max_lt || min_gt)
	    val = boolean_false_node;
	}
      else if (code == LT_EXPR)
	{
	  if (max_lt)
	    val = boolean_true_node;
	  if (!min_lt)
	    val = boolean_false_node;
	}
      else if (code == GT_EXPR)
	{
	  if (min_gt)
	    val = boolean_true_node;
	  if (!max_gt)
	    val = boolean_false_node;
	}
      else if (code == LE_EXPR)
	{
	  if (!max_gt)
	    val = boolean_true_node;
	  if (min_gt)
	    val = boolean_false_node;
	}
      else if (code == GE_EXPR)
	{
	  if (!min_lt)
	    val = boolean_true_node;
	  if (max_lt)
	    val = boolean_false_node;
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

	      default:
		break;
	      }
	  type = unsigned_type (type);
	}

      if (!max_gt && !unsignedp0 && TREE_CODE (primop0) != INTEGER_CST)
	{
	  /* This is the case of (char)x >?< 0x80, which people used to use
	     expecting old C compilers to change the 0x80 into -0x80.  */
	  if (val == boolean_false_node)
	    warning ("comparison is always false due to limited range of data type");
	  if (val == boolean_true_node)
	    warning ("comparison is always true due to limited range of data type");
	}

      if (!min_lt && unsignedp0 && TREE_CODE (primop0) != INTEGER_CST)
	{
	  /* This is the case of (unsigned char)x >?< -1 or < 0.  */
	  if (val == boolean_false_node)
	    warning ("comparison is always false due to limited range of data type");
	  if (val == boolean_true_node)
	    warning ("comparison is always true due to limited range of data type");
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
		warning ("comparison of unsigned expression >= 0 is always true");
	      value = boolean_true_node;
	      break;

	    case LT_EXPR:
	      if (extra_warnings
		  && ! (TREE_CODE (primop0) == INTEGER_CST
			&& ! TREE_OVERFLOW (convert (signed_type (type),
						     primop0))))
		warning ("comparison of unsigned expression < 0 is always false");
	      value = boolean_false_node;
	      break;

	    default:
	      break;
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

  *restype_ptr = boolean_type_node;

  return 0;
}

/* Prepare expr to be an argument of a TRUTH_NOT_EXPR,
   or validate its data type for an `if' or `while' statement or ?..: exp.

   This preparation consists of taking the ordinary
   representation of an expression expr and producing a valid tree
   boolean expression describing whether expr is nonzero.  We could
   simply always do build_binary_op (NE_EXPR, expr, boolean_false_node, 1),
   but we optimize comparisons, &&, ||, and !.

   The resulting type should always be `boolean_type_node'.  */

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
      return boolean_false_node;

    case UNION_TYPE:
      error ("union type value used where scalar is required");
      return boolean_false_node;

    case ARRAY_TYPE:
      error ("array type value used where scalar is required");
      return boolean_false_node;

    default:
      break;
    }
#endif /* 0 */

  switch (TREE_CODE (expr))
    {
    case EQ_EXPR:
    case NE_EXPR: case LE_EXPR: case GE_EXPR: case LT_EXPR: case GT_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
    case TRUTH_NOT_EXPR:
      TREE_TYPE (expr) = boolean_type_node;
      return expr;

    case ERROR_MARK:
      return expr;

    case INTEGER_CST:
      return integer_zerop (expr) ? boolean_false_node : boolean_true_node;

    case REAL_CST:
      return real_zerop (expr) ? boolean_false_node : boolean_true_node;

    case ADDR_EXPR:
      /* If we are taking the address of a external decl, it might be zero
	 if it is weak, so we cannot optimize.  */
      if (DECL_P (TREE_OPERAND (expr, 0))
	  && DECL_EXTERNAL (TREE_OPERAND (expr, 0)))
	break;

      if (TREE_SIDE_EFFECTS (TREE_OPERAND (expr, 0)))
	return build (COMPOUND_EXPR, boolean_type_node,
		      TREE_OPERAND (expr, 0), boolean_true_node);
      else
	return boolean_true_node;

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
	return build (COMPOUND_EXPR, boolean_type_node, TREE_OPERAND (expr, 1),
		      truthvalue_conversion (TREE_OPERAND (expr, 0)));
      else
	return truthvalue_conversion (TREE_OPERAND (expr, 0));

    case COND_EXPR:
      /* Distribute the conversion into the arms of a COND_EXPR.  */
      return fold (build (COND_EXPR, boolean_type_node, TREE_OPERAND (expr, 0),
			  truthvalue_conversion (TREE_OPERAND (expr, 1)),
			  truthvalue_conversion (TREE_OPERAND (expr, 2))));

    case CONVERT_EXPR:
      /* Don't cancel the effect of a CONVERT_EXPR from a REFERENCE_TYPE,
	 since that affects how `default_conversion' will behave.  */
      if (TREE_CODE (TREE_TYPE (expr)) == REFERENCE_TYPE
	  || TREE_CODE (TREE_TYPE (TREE_OPERAND (expr, 0))) == REFERENCE_TYPE)
	break;
      /* fall through...  */
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
      /* fall through...  */
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
      if (integer_onep (TREE_OPERAND (expr, 1))
	  && TREE_TYPE (expr) != boolean_type_node)
	/* Using convert here would cause infinite recursion.  */
	return build1 (NOP_EXPR, boolean_type_node, expr);
      break;

    case MODIFY_EXPR:
      if (warn_parentheses && C_EXP_ORIGINAL_CODE (expr) == MODIFY_EXPR)
	warning ("suggest parentheses around assignment used as truth value");
      break;

    default:
      break;
    }

  if (TREE_CODE (TREE_TYPE (expr)) == COMPLEX_TYPE)
    {
      tree tem = save_expr (expr);
      return (build_binary_op
	      ((TREE_SIDE_EFFECTS (expr)
		? TRUTH_OR_EXPR : TRUTH_ORIF_EXPR),
	       truthvalue_conversion (build_unary_op (REALPART_EXPR, tem, 0)),
	       truthvalue_conversion (build_unary_op (IMAGPART_EXPR, tem, 0)),
	       0));
    }

  return build_binary_op (NE_EXPR, expr, integer_zero_node, 1);
}

#if USE_CPPLIB
/* Read the rest of a #-directive from input stream FINPUT.
   In normal use, the directive name and the white space after it
   have already been read, so they won't be included in the result.
   We allow for the fact that the directive line may contain
   a newline embedded within a character or string literal which forms
   a part of the directive.

   The value is a string in a reusable buffer.  It remains valid
   only until the next time this function is called.  */
unsigned char *yy_cur, *yy_lim;

#define GETC() (yy_cur < yy_lim ? *yy_cur++ : yy_get_token ())
#define UNGETC(c) ((c) == EOF ? 0 : yy_cur--)

int
yy_get_token ()
{
  for (;;)
    {
      parse_in.limit = parse_in.token_buffer;
      cpp_token = cpp_get_token (&parse_in);
      if (cpp_token == CPP_EOF)
	return -1;
      yy_lim = CPP_PWRITTEN (&parse_in);
      yy_cur = parse_in.token_buffer;
      if (yy_cur < yy_lim)
	return *yy_cur++;
    }
}

char *
get_directive_line ()
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

      c = GETC ();

      /* Discard initial whitespace.  */
      if ((c == ' ' || c == '\t') && p == directive_buffer)
	continue;

      /* Detect the end of the directive.  */
      if (c == '\n' && looking_for == 0)
	{
          UNGETC (c);
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
#else
/* Read the rest of a #-directive from input stream FINPUT.
   In normal use, the directive name and the white space after it
   have already been read, so they won't be included in the result.
   We allow for the fact that the directive line may contain
   a newline embedded within a character or string literal which forms
   a part of the directive.

   The value is a string in a reusable buffer.  It remains valid
   only until the next time this function is called.

   The terminating character ('\n' or EOF) is left in FINPUT for the
   caller to re-read.  */

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
      if (looking_for == 0
	  && (c == '\n' || c == EOF))
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
				   one of these (or an EOF).  */

      /* Handle backslash.  */
      char_escaped = (c == '\\' && ! char_escaped);
    }
}
#endif /* !USE_CPPLIB */

/* Make a variant type in the proper way for C/C++, propagating qualifiers
   down to the element type of an array.  */

tree
c_build_qualified_type (type, type_quals)
     tree type;
     int type_quals;
{
  /* A restrict-qualified pointer type must be a pointer to object or
     incomplete type.  Note that the use of POINTER_TYPE_P also allows
     REFERENCE_TYPEs, which is appropriate for C++.  Unfortunately,
     the C++ front-end also use POINTER_TYPE for pointer-to-member
     values, so even though it should be illegal to use `restrict'
     with such an entity we don't flag that here.  Thus, special case
     code for that case is required in the C++ front-end.  */
  if ((type_quals & TYPE_QUAL_RESTRICT)
      && (!POINTER_TYPE_P (type)
	  || !C_TYPE_OBJECT_OR_INCOMPLETE_P (TREE_TYPE (type))))
    {
      error ("invalid use of `restrict'");
      type_quals &= ~TYPE_QUAL_RESTRICT;
    }

  if (TREE_CODE (type) == ARRAY_TYPE)
    return build_array_type (c_build_qualified_type (TREE_TYPE (type),
						     type_quals),
			     TYPE_DOMAIN (type));
  return build_qualified_type (type, type_quals);
}

/* Apply the TYPE_QUALS to the new DECL.  */

void
c_apply_type_quals_to_decl (type_quals, decl)
     int type_quals;
     tree decl;
{
  if (type_quals & TYPE_QUAL_CONST)
    TREE_READONLY (decl) = 1;
  if (type_quals & TYPE_QUAL_VOLATILE)
    {
      TREE_SIDE_EFFECTS (decl) = 1;
      TREE_THIS_VOLATILE (decl) = 1;
    }
  if (type_quals & TYPE_QUAL_RESTRICT)
    {
      if (!TREE_TYPE (decl)
	  || !POINTER_TYPE_P (TREE_TYPE (decl))
	  || !C_TYPE_OBJECT_OR_INCOMPLETE_P (TREE_TYPE (TREE_TYPE (decl))))
	error ("invalid use of `restrict'");
      else if (flag_strict_aliasing)
	{
	  /* No two restricted pointers can point at the same thing.
	     However, a restricted pointer can point at the same thing
	     as an unrestricted pointer, if that unrestricted pointer
	     is based on the restricted pointer.  So, we make the
	     alias set for the restricted pointer a subset of the
	     alias set for the type pointed to by the type of the
	     decl.  */

	  int pointed_to_alias_set
	    = get_alias_set (TREE_TYPE (TREE_TYPE (decl)));

	  if (!pointed_to_alias_set)
	    /* It's not legal to make a subset of alias set zero.  */
	    ;
	  else
	    {
	      DECL_POINTER_ALIAS_SET (decl) = new_alias_set ();
	      record_alias_subset  (pointed_to_alias_set,
				    DECL_POINTER_ALIAS_SET (decl));
	    }
	}
    }
}

/* T is an expression with pointer type.  Find the DECL on which this
   expression is based.  (For example, in `a[i]' this would be `a'.)
   If there is no such DECL, or a unique decl cannot be determined,
   NULL_TREE is retured.  */

static tree
c_find_base_decl (t)
     tree t;
{
  int i;
  tree decl;

  if (t == NULL_TREE || t == error_mark_node)
    return NULL_TREE;

  if (!POINTER_TYPE_P (TREE_TYPE (t)))
    return NULL_TREE;

  decl = NULL_TREE;

  if (TREE_CODE (t) == FIELD_DECL
      || TREE_CODE (t) == PARM_DECL
      || TREE_CODE (t) == VAR_DECL)
    /* Aha, we found a pointer-typed declaration.  */
    return t;

  /* It would be nice to deal with COMPONENT_REFs here.  If we could
     tell that `a' and `b' were the same, then `a->f' and `b->f' are
     also the same.  */

  /* Handle general expressions.  */
  switch (TREE_CODE_CLASS (TREE_CODE (t)))
    {
    case '1':
    case '2':
    case '3':
      for (i = tree_code_length [(int) TREE_CODE (t)]; --i >= 0;)
	{
	  tree d = c_find_base_decl (TREE_OPERAND (t, i));
	  if (d)
	    {
	      if (!decl)
		decl = d;
	      else if (d && d != decl)
		/* Two different declarations.  That's confusing; let's
		   just assume we don't know what's going on.  */
		decl = NULL_TREE;
	    }
	}
      break;

    default:
      break;
    }

  return decl;
}

/* Return the typed-based alias set for T, which may be an expression
   or a type.  */

int
c_get_alias_set (t)
     tree t;
{
  tree type;
  tree u;

  if (t == error_mark_node)
    return 0;

  type = (TYPE_P (t)) ? t : TREE_TYPE (t);

  if (type == error_mark_node)
    return 0;

  /* Deal with special cases first; for certain kinds of references
     we're interested in more than just the type.  */

  if (TREE_CODE (t) == BIT_FIELD_REF)
    /* Perhaps reads and writes to this piece of data alias fields
       neighboring the bitfield.  Perhaps that's impossible.  For now,
       let's just assume that bitfields can alias everything, which is
       the conservative assumption.  */
    return 0;

  /* Permit type-punning when accessing a union, provided the access
     is directly through the union.  For example, this code does not
     permit taking the address of a union member and then storing
     through it.  Even the type-punning allowed here is a GCC
     extension, albeit a common and useful one; the C standard says
     that such accesses have implementation-defined behavior.  */
  for (u = t;
       TREE_CODE (u) == COMPONENT_REF || TREE_CODE (u) == ARRAY_REF;
       u = TREE_OPERAND (u, 0))
    if (TREE_CODE (u) == COMPONENT_REF
	&& TREE_CODE (TREE_TYPE (TREE_OPERAND (u, 0))) == UNION_TYPE)
      return 0;

  if (TREE_CODE (t) == INDIRECT_REF)
    {
      /* Check for accesses through restrict-qualified pointers.  */
      tree decl = c_find_base_decl (TREE_OPERAND (t, 0));

      if (decl && DECL_POINTER_ALIAS_SET_KNOWN_P (decl))
	/* We use the alias set indicated in the declaration.  */
	return DECL_POINTER_ALIAS_SET (decl);
    }

  /* From here on, only the type matters.  */

  if (TREE_CODE (t) == COMPONENT_REF
      && DECL_BIT_FIELD_TYPE (TREE_OPERAND (t, 1)))
    /* Since build_modify_expr calls get_unwidened for stores to
       component references, the type of a bit field can be changed
       from (say) `unsigned int : 16' to `unsigned short' or from
       `enum E : 16' to `short'.  We want the real type of the
       bit-field in this case, not some the integral equivalent.  */
    type = DECL_BIT_FIELD_TYPE (TREE_OPERAND (t, 1));

  if (TYPE_ALIAS_SET_KNOWN_P (type))
    /* If we've already calculated the value, just return it.  */
    return TYPE_ALIAS_SET (type);
  else if (TYPE_MAIN_VARIANT (type) != type)
    /* The C standard specifically allows aliasing between
       cv-qualified variants of types.  */
    TYPE_ALIAS_SET (type) = c_get_alias_set (TYPE_MAIN_VARIANT (type));
  else if (TREE_CODE (type) == INTEGER_TYPE)
    {
      tree signed_variant;

      /* The C standard specifically allows aliasing between signed and
	 unsigned variants of the same type.  We treat the signed
	 variant as canonical.  */
      signed_variant = signed_type (type);

      if (signed_variant != type)
	TYPE_ALIAS_SET (type) = c_get_alias_set (signed_variant);
      else if (signed_variant == signed_char_type_node)
	/* The C standard guarantess that any object may be accessed
	   via an lvalue that has character type.  We don't have to
	   check for unsigned_char_type_node or char_type_node because
	   we are specifically looking at the signed variant.  */
	TYPE_ALIAS_SET (type) = 0;
    }
  else if (TREE_CODE (type) == ARRAY_TYPE)
    /* Anything that can alias one of the array elements can alias
       the entire array as well.  */
    TYPE_ALIAS_SET (type) = c_get_alias_set (TREE_TYPE (type));
  else if (TREE_CODE (type) == FUNCTION_TYPE)
    /* There are no objects of FUNCTION_TYPE, so there's no point in
       using up an alias set for them.  (There are, of course,
       pointers and references to functions, but that's
       different.)  */
    TYPE_ALIAS_SET (type) = 0;
  else if (TREE_CODE (type) == RECORD_TYPE
	   || TREE_CODE (type) == UNION_TYPE)
    /* If TYPE is a struct or union type then we're reading or
       writing an entire struct.  Thus, we don't know anything about
       aliasing.  (In theory, such an access can only alias objects
       whose type is the same as one of the fields, recursively, but
       we don't yet make any use of that information.)  */
    TYPE_ALIAS_SET (type) = 0;
  else if (TREE_CODE (type) == POINTER_TYPE
	   || TREE_CODE (type) == REFERENCE_TYPE)
    {
      tree t;

      /* Unfortunately, there is no canonical form of a pointer type.
	 In particular, if we have `typedef int I', then `int *', and
	 `I *' are different types.  So, we have to pick a canonical
	 representative.  We do this below.

	 Technically, this approach is actually more conservative that
	 it needs to be.  In particular, `const int *' and `int *'
	 chould be in different alias sets, according to the C and C++
	 standard, since their types are not the same, and so,
	 technically, an `int **' and `const int **' cannot point at
	 the same thing.

         But, the standard is wrong.  In particular, this code is
	 legal C++:

            int *ip;
            int **ipp = &ip;
            const int* const* cipp = &ip;

         And, it doesn't make sense for that to be legal unless you
	 can dereference IPP and CIPP.  So, we ignore cv-qualifiers on
	 the pointed-to types.  This issue has been reported to the
	 C++ committee.  */
      t = TYPE_MAIN_VARIANT (TREE_TYPE (type));
      t = ((TREE_CODE (type) == POINTER_TYPE)
	   ? build_pointer_type (t) : build_reference_type (t));
      if (t != type)
	TYPE_ALIAS_SET (type) = c_get_alias_set (t);
    }

  if (!TYPE_ALIAS_SET_KNOWN_P (type))
    /* TYPE is something we haven't seen before.  Put it in a new
       alias set.  */
    TYPE_ALIAS_SET (type) = new_alias_set ();

  return TYPE_ALIAS_SET (type);
}

/* Build tree nodes and builtin functions common to both C and C++ language
   frontends.
   CPLUS_MODE is nonzero if we are called from the C++ frontend, we generate
   some stricter prototypes in that case.
   NO_BUILTINS and NO_NONANSI_BUILTINS contain the respective values of
   the language frontend flags flag_no_builtin and
   flag_no_nonansi_builtin.  */
void
c_common_nodes_and_builtins (cplus_mode, no_builtins, no_nonansi_builtins)
    int cplus_mode, no_builtins, no_nonansi_builtins;
{
  tree temp;
  tree memcpy_ftype, memset_ftype, strlen_ftype;
  tree bzero_ftype, bcmp_ftype;
  tree endlink, int_endlink, double_endlink, unsigned_endlink;
  tree sizetype_endlink;
  tree ptr_ftype, ptr_ftype_unsigned;
  tree void_ftype_any, void_ftype_int, int_ftype_any;
  tree double_ftype_double, double_ftype_double_double;
  tree float_ftype_float, ldouble_ftype_ldouble;
  tree int_ftype_cptr_cptr_sizet;
  tree int_ftype_string_string, string_ftype_ptr_ptr;
  tree long_ftype_long;
  /* Either char* or void*.  */
  tree traditional_ptr_type_node;
  /* Either const char* or const void*.  */
  tree traditional_cptr_type_node;
  tree traditional_len_type_node;
  tree traditional_len_endlink;
  tree va_list_ref_type_node;
  tree va_list_arg_type_node;

  pushdecl (build_decl (TYPE_DECL, get_identifier ("__builtin_va_list"),
			va_list_type_node));

  pushdecl (build_decl (TYPE_DECL, get_identifier ("__builtin_ptrdiff_t"),
			ptrdiff_type_node));

  pushdecl (build_decl (TYPE_DECL, get_identifier ("__builtin_size_t"),
			sizetype));

  if (TREE_CODE (va_list_type_node) == ARRAY_TYPE)
    {
      va_list_arg_type_node = va_list_ref_type_node =
	build_pointer_type (TREE_TYPE (va_list_type_node));
    }
  else
    {
      va_list_arg_type_node = va_list_type_node;
      va_list_ref_type_node = build_reference_type (va_list_type_node);
    }
 
  endlink = void_list_node;
  int_endlink = tree_cons (NULL_TREE, integer_type_node, endlink);
  double_endlink = tree_cons (NULL_TREE, double_type_node, endlink);
  unsigned_endlink = tree_cons (NULL_TREE, unsigned_type_node, endlink);

  ptr_ftype = build_function_type (ptr_type_node, NULL_TREE);
  ptr_ftype_unsigned = build_function_type (ptr_type_node, unsigned_endlink);
  sizetype_endlink = tree_cons (NULL_TREE, TYPE_DOMAIN (sizetype), endlink);
  /* We realloc here because sizetype could be int or unsigned.  S'ok.  */
  ptr_ftype_sizetype = build_function_type (ptr_type_node, sizetype_endlink);

  int_ftype_any = build_function_type (integer_type_node, NULL_TREE);
  void_ftype_any = build_function_type (void_type_node, NULL_TREE);
  void_ftype = build_function_type (void_type_node, endlink);
  void_ftype_int = build_function_type (void_type_node, int_endlink);
  void_ftype_ptr
    = build_function_type (void_type_node,
 			   tree_cons (NULL_TREE, ptr_type_node, endlink));

  float_ftype_float
    = build_function_type (float_type_node,
			   tree_cons (NULL_TREE, float_type_node, endlink));

  double_ftype_double
    = build_function_type (double_type_node, double_endlink);

  ldouble_ftype_ldouble
    = build_function_type (long_double_type_node,
			   tree_cons (NULL_TREE, long_double_type_node,
				      endlink));

  double_ftype_double_double
    = build_function_type (double_type_node,
			   tree_cons (NULL_TREE, double_type_node,
				      double_endlink));

  int_ftype_int
    = build_function_type (integer_type_node, int_endlink);

  long_ftype_long
    = build_function_type (long_integer_type_node,
			   tree_cons (NULL_TREE, long_integer_type_node,
				      endlink));

  int_ftype_cptr_cptr_sizet
    = build_function_type (integer_type_node,
			   tree_cons (NULL_TREE, const_ptr_type_node,
				      tree_cons (NULL_TREE, const_ptr_type_node,
						 tree_cons (NULL_TREE,
							    sizetype,
							    endlink))));

  /* Prototype for strcpy.  */
  string_ftype_ptr_ptr
    = build_function_type (string_type_node,
			   tree_cons (NULL_TREE, string_type_node,
				      tree_cons (NULL_TREE,
						 const_string_type_node,
						 endlink)));

  traditional_len_type_node = (flag_traditional && ! cplus_mode
			       ? integer_type_node : sizetype);
  traditional_len_endlink = tree_cons (NULL_TREE, traditional_len_type_node,
				       endlink);

  /* Prototype for strcmp.  */
  int_ftype_string_string
    = build_function_type (integer_type_node,
			   tree_cons (NULL_TREE, const_string_type_node,
				      tree_cons (NULL_TREE,
						 const_string_type_node,
						 endlink)));

  /* Prototype for strlen.  */
  strlen_ftype
    = build_function_type (traditional_len_type_node,
			   tree_cons (NULL_TREE, const_string_type_node,
				      endlink));

  traditional_ptr_type_node = (flag_traditional && ! cplus_mode
			       ? string_type_node : ptr_type_node);
  traditional_cptr_type_node = (flag_traditional && ! cplus_mode
			       ? const_string_type_node : const_ptr_type_node);

  /* Prototype for memcpy.  */
  memcpy_ftype
    = build_function_type (traditional_ptr_type_node,
			   tree_cons (NULL_TREE, ptr_type_node,
				      tree_cons (NULL_TREE, const_ptr_type_node,
						 sizetype_endlink)));

  /* Prototype for memset.  */
  memset_ftype
    = build_function_type (traditional_ptr_type_node,
			   tree_cons (NULL_TREE, ptr_type_node,
				      tree_cons (NULL_TREE, integer_type_node,
						 tree_cons (NULL_TREE,
							    sizetype,
							    endlink))));

  /* Prototype for bzero.  */
  bzero_ftype
    = build_function_type (void_type_node,
			   tree_cons (NULL_TREE, traditional_ptr_type_node,
				      traditional_len_endlink));

  /* Prototype for bcmp.  */
  bcmp_ftype
    = build_function_type (integer_type_node,
			   tree_cons (NULL_TREE, traditional_cptr_type_node,
				      tree_cons (NULL_TREE,
						 traditional_cptr_type_node,
						 traditional_len_endlink)));

  builtin_function ("__builtin_constant_p", default_function_type,
		    BUILT_IN_CONSTANT_P, BUILT_IN_NORMAL, NULL_PTR);

  builtin_function ("__builtin_return_address", ptr_ftype_unsigned,
		    BUILT_IN_RETURN_ADDRESS, BUILT_IN_NORMAL, NULL_PTR);

  builtin_function ("__builtin_frame_address", ptr_ftype_unsigned,
		    BUILT_IN_FRAME_ADDRESS, BUILT_IN_NORMAL, NULL_PTR);

  builtin_function ("__builtin_alloca", ptr_ftype_sizetype,
		    BUILT_IN_ALLOCA, BUILT_IN_NORMAL, "alloca");
  builtin_function ("__builtin_ffs", int_ftype_int, BUILT_IN_FFS,
		    BUILT_IN_NORMAL, NULL_PTR);
  /* Define alloca, ffs as builtins.
     Declare _exit just to mark it as volatile.  */
  if (! no_builtins && ! no_nonansi_builtins)
    {
#ifndef SMALL_STACK
      temp = builtin_function ("alloca", ptr_ftype_sizetype,
			       BUILT_IN_ALLOCA, BUILT_IN_NORMAL, NULL_PTR);
      /* Suppress error if redefined as a non-function.  */
      DECL_BUILT_IN_NONANSI (temp) = 1;
#endif
      temp = builtin_function ("ffs", int_ftype_int, BUILT_IN_FFS,
			       BUILT_IN_NORMAL, NULL_PTR);
      /* Suppress error if redefined as a non-function.  */
      DECL_BUILT_IN_NONANSI (temp) = 1;
      temp = builtin_function ("_exit", void_ftype_int,
			       0, NOT_BUILT_IN, NULL_PTR);
      TREE_THIS_VOLATILE (temp) = 1;
      TREE_SIDE_EFFECTS (temp) = 1;
      /* Suppress error if redefined as a non-function.  */
      DECL_BUILT_IN_NONANSI (temp) = 1;

      /* The system prototypes for these functions have many
	 variations, so don't specify parameters to avoid conflicts.
	 The expand_* functions check the argument types anyway.  */
      temp = builtin_function ("bzero", void_ftype_any,
			       BUILT_IN_BZERO, BUILT_IN_NORMAL, NULL_PTR);
      DECL_BUILT_IN_NONANSI (temp) = 1;
      temp = builtin_function ("bcmp", int_ftype_any,
			       BUILT_IN_BCMP, BUILT_IN_NORMAL, NULL_PTR);
      DECL_BUILT_IN_NONANSI (temp) = 1;
    }

  builtin_function ("__builtin_abs", int_ftype_int, BUILT_IN_ABS,
		    BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ("__builtin_fabsf", float_ftype_float, BUILT_IN_FABS,
		    BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ("__builtin_fabs", double_ftype_double, BUILT_IN_FABS,
		    BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ("__builtin_fabsl", ldouble_ftype_ldouble, BUILT_IN_FABS,
		    BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ("__builtin_labs", long_ftype_long, BUILT_IN_LABS,
		    BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ("__builtin_saveregs", ptr_ftype, BUILT_IN_SAVEREGS,
		    BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ("__builtin_classify_type", default_function_type,
		    BUILT_IN_CLASSIFY_TYPE, BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ("__builtin_next_arg", ptr_ftype, BUILT_IN_NEXT_ARG,
		    BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ("__builtin_args_info", int_ftype_int, BUILT_IN_ARGS_INFO,
		    BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ("__builtin_setjmp",
		    build_function_type (integer_type_node,
					 tree_cons (NULL_TREE, ptr_type_node,
						    endlink)),
		    BUILT_IN_SETJMP, BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ("__builtin_longjmp",
		    build_function_type (void_type_node,
					 tree_cons (NULL_TREE, ptr_type_node,
						    tree_cons (NULL_TREE,
							       integer_type_node,
							       endlink))),
		    BUILT_IN_LONGJMP, BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ("__builtin_trap", void_ftype, BUILT_IN_TRAP,
		    BUILT_IN_NORMAL, NULL_PTR);

  /* ISO C99 IEEE Unordered compares.  */
  builtin_function ("__builtin_isgreater", default_function_type,
		    BUILT_IN_ISGREATER, BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ("__builtin_isgreaterequal", default_function_type,
		    BUILT_IN_ISGREATEREQUAL, BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ("__builtin_isless", default_function_type,
		    BUILT_IN_ISLESS, BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ("__builtin_islessequal", default_function_type,
		    BUILT_IN_ISLESSEQUAL, BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ("__builtin_islessgreater", default_function_type,
		    BUILT_IN_ISLESSGREATER, BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ("__builtin_isunordered", default_function_type,
		    BUILT_IN_ISUNORDERED, BUILT_IN_NORMAL, NULL_PTR);

  /* Untyped call and return.  */
  builtin_function ("__builtin_apply_args", ptr_ftype,
		    BUILT_IN_APPLY_ARGS, BUILT_IN_NORMAL, NULL_PTR);

  temp = tree_cons (NULL_TREE,
		    build_pointer_type (build_function_type (void_type_node,
							     NULL_TREE)),
		    tree_cons (NULL_TREE,
			       ptr_type_node,
			       tree_cons (NULL_TREE,
					  sizetype,
					  endlink)));
  builtin_function ("__builtin_apply",
		    build_function_type (ptr_type_node, temp),
		    BUILT_IN_APPLY, BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ("__builtin_return", void_ftype_ptr,
		    BUILT_IN_RETURN, BUILT_IN_NORMAL, NULL_PTR);

  /* Support for varargs.h and stdarg.h.  */
  builtin_function ("__builtin_varargs_start",
		    build_function_type (void_type_node,
					 tree_cons (NULL_TREE,
						    va_list_ref_type_node,
						    endlink)),
		    BUILT_IN_VARARGS_START, BUILT_IN_NORMAL, NULL_PTR);

  builtin_function ("__builtin_stdarg_start",
		    build_function_type (void_type_node,
					 tree_cons (NULL_TREE,
						    va_list_ref_type_node,
						    NULL_TREE)),
		    BUILT_IN_STDARG_START, BUILT_IN_NORMAL, NULL_PTR);

  builtin_function ("__builtin_va_end",
		    build_function_type (void_type_node,
					 tree_cons (NULL_TREE,
						    va_list_ref_type_node,
						    endlink)),
		    BUILT_IN_VA_END, BUILT_IN_NORMAL, NULL_PTR);

  builtin_function ("__builtin_va_copy",
		    build_function_type (void_type_node,
					 tree_cons (NULL_TREE,
						    va_list_ref_type_node,
						    tree_cons (NULL_TREE,
						      va_list_arg_type_node,
						      endlink))),
		    BUILT_IN_VA_COPY, BUILT_IN_NORMAL, NULL_PTR);

  /* ??? Ought to be `T __builtin_expect(T, T)' for any type T.  */
  builtin_function ("__builtin_expect",
		    build_function_type (long_integer_type_node,
					 tree_cons (NULL_TREE,
						    long_integer_type_node,
						    tree_cons (NULL_TREE,
							long_integer_type_node,
							endlink))),
		    BUILT_IN_EXPECT, BUILT_IN_NORMAL, NULL_PTR);

  /* Currently under experimentation.  */
  builtin_function ("__builtin_memcpy", memcpy_ftype, BUILT_IN_MEMCPY,
		    BUILT_IN_NORMAL, "memcpy");
  builtin_function ("__builtin_memcmp", int_ftype_cptr_cptr_sizet,
		    BUILT_IN_MEMCMP, BUILT_IN_NORMAL, "memcmp");
  builtin_function ("__builtin_memset", memset_ftype,
		    BUILT_IN_MEMSET, BUILT_IN_NORMAL, "memset");
  builtin_function ("__builtin_bzero", bzero_ftype,
		    BUILT_IN_BZERO, BUILT_IN_NORMAL, "bzero");
  builtin_function ("__builtin_bcmp", bcmp_ftype,
		    BUILT_IN_BCMP, BUILT_IN_NORMAL, "bcmp");
  builtin_function ("__builtin_strcmp", int_ftype_string_string,
		    BUILT_IN_STRCMP, BUILT_IN_NORMAL, "strcmp");
  builtin_function ("__builtin_strcpy", string_ftype_ptr_ptr,
		    BUILT_IN_STRCPY, BUILT_IN_NORMAL, "strcpy");
  builtin_function ("__builtin_strlen", strlen_ftype,
		    BUILT_IN_STRLEN, BUILT_IN_NORMAL, "strlen");
  builtin_function ("__builtin_sqrtf", float_ftype_float,
		    BUILT_IN_FSQRT, BUILT_IN_NORMAL, "sqrtf");
  builtin_function ("__builtin_fsqrt", double_ftype_double,
		    BUILT_IN_FSQRT, BUILT_IN_NORMAL, "sqrt");
  builtin_function ("__builtin_sqrtl", ldouble_ftype_ldouble,
		    BUILT_IN_FSQRT, BUILT_IN_NORMAL, "sqrtl");
  builtin_function ("__builtin_sinf", float_ftype_float,
		    BUILT_IN_SIN, BUILT_IN_NORMAL, "sinf");
  builtin_function ("__builtin_sin", double_ftype_double,
		    BUILT_IN_SIN, BUILT_IN_NORMAL, "sin");
  builtin_function ("__builtin_sinl", ldouble_ftype_ldouble,
		    BUILT_IN_SIN, BUILT_IN_NORMAL, "sinl");
  builtin_function ("__builtin_cosf", float_ftype_float,
		    BUILT_IN_COS, BUILT_IN_NORMAL, "cosf");
  builtin_function ("__builtin_cos", double_ftype_double,
		    BUILT_IN_COS, BUILT_IN_NORMAL, "cos");
  builtin_function ("__builtin_cosl", ldouble_ftype_ldouble,
		    BUILT_IN_COS, BUILT_IN_NORMAL, "cosl");

  if (! no_builtins)
    {
      builtin_function ("abs", int_ftype_int, BUILT_IN_ABS,
			BUILT_IN_NORMAL, NULL_PTR);
      builtin_function ("fabsf", float_ftype_float, BUILT_IN_FABS,
			BUILT_IN_NORMAL, NULL_PTR);
      builtin_function ("fabs", double_ftype_double, BUILT_IN_FABS,
			BUILT_IN_NORMAL, NULL_PTR);
      builtin_function ("fabsl", ldouble_ftype_ldouble, BUILT_IN_FABS,
			BUILT_IN_NORMAL, NULL_PTR);
      builtin_function ("labs", long_ftype_long, BUILT_IN_LABS,
			BUILT_IN_NORMAL, NULL_PTR);
      builtin_function ("memcpy", memcpy_ftype, BUILT_IN_MEMCPY,
			BUILT_IN_NORMAL, NULL_PTR);
      builtin_function ("memcmp", int_ftype_cptr_cptr_sizet, BUILT_IN_MEMCMP,
			BUILT_IN_NORMAL, NULL_PTR);
      builtin_function ("memset", memset_ftype, BUILT_IN_MEMSET,
			BUILT_IN_NORMAL, NULL_PTR);
      builtin_function ("strcmp", int_ftype_string_string, BUILT_IN_STRCMP,
			BUILT_IN_NORMAL, NULL_PTR);
      builtin_function ("strcpy", string_ftype_ptr_ptr, BUILT_IN_STRCPY,
			BUILT_IN_NORMAL, NULL_PTR);
      builtin_function ("strlen", strlen_ftype, BUILT_IN_STRLEN,
			BUILT_IN_NORMAL, NULL_PTR);
      builtin_function ("sqrtf", float_ftype_float, BUILT_IN_FSQRT,
			BUILT_IN_NORMAL, NULL_PTR);
      builtin_function ("sqrt", double_ftype_double, BUILT_IN_FSQRT,
			BUILT_IN_NORMAL, NULL_PTR);
      builtin_function ("sqrtl", ldouble_ftype_ldouble, BUILT_IN_FSQRT,
			BUILT_IN_NORMAL, NULL_PTR);
      builtin_function ("sinf", float_ftype_float, BUILT_IN_SIN,
			BUILT_IN_NORMAL, NULL_PTR);
      builtin_function ("sin", double_ftype_double, BUILT_IN_SIN,
			BUILT_IN_NORMAL, NULL_PTR);
      builtin_function ("sinl", ldouble_ftype_ldouble, BUILT_IN_SIN,
			BUILT_IN_NORMAL, NULL_PTR);
      builtin_function ("cosf", float_ftype_float, BUILT_IN_COS,
			BUILT_IN_NORMAL, NULL_PTR);
      builtin_function ("cos", double_ftype_double, BUILT_IN_COS,
			BUILT_IN_NORMAL, NULL_PTR);
      builtin_function ("cosl", ldouble_ftype_ldouble, BUILT_IN_COS,
			BUILT_IN_NORMAL, NULL_PTR);

      /* Declare these functions volatile
	 to avoid spurious "control drops through" warnings.  */
      temp = builtin_function ("abort", cplus_mode ? void_ftype : void_ftype_any,
			       0, NOT_BUILT_IN, NULL_PTR);
      TREE_THIS_VOLATILE (temp) = 1;
      TREE_SIDE_EFFECTS (temp) = 1;

#if 0 /* ??? The C++ frontend used to do this.  */
      /* Well, these are actually ANSI, but we can't set DECL_BUILT_IN on
	 them...  */
      DECL_BUILT_IN_NONANSI (temp) = 1;
#endif
      temp = builtin_function ("exit",
			       cplus_mode ? void_ftype_int : void_ftype_any,
			       0, NOT_BUILT_IN, NULL_PTR);
      TREE_THIS_VOLATILE (temp) = 1;
      TREE_SIDE_EFFECTS (temp) = 1;

#if 0 /* ??? The C++ frontend used to do this.  */
      /* Well, these are actually ANSI, but we can't set DECL_BUILT_IN on
	 them...  */
      DECL_BUILT_IN_NONANSI (temp) = 1;
#endif
    }

#if 0
  /* Support for these has not been written in either expand_builtin
     or build_function_call.  */
  builtin_function ("__builtin_div", default_ftype, BUILT_IN_DIV,
		    BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ("__builtin_ldiv", default_ftype, BUILT_IN_LDIV,
		    BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ("__builtin_ffloor", double_ftype_double, BUILT_IN_FFLOOR,
		    BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ("__builtin_fceil", double_ftype_double, BUILT_IN_FCEIL,
		    BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ("__builtin_fmod", double_ftype_double_double,
		    BUILT_IN_FMOD, BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ("__builtin_frem", double_ftype_double_double,
		    BUILT_IN_FREM, BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ("__builtin_getexp", double_ftype_double, BUILT_IN_GETEXP,
		    BUILT_IN_NORMAL, NULL_PTR);
  builtin_function ("__builtin_getman", double_ftype_double, BUILT_IN_GETMAN,
		    BUILT_IN_NORMAL, NULL_PTR);
#endif

  /* ??? Perhaps there's a better place to do this.  But it is related
     to __builtin_va_arg, so it isn't that off-the-wall.  */
  lang_type_promotes_to = simple_type_promotes_to;
}

tree
build_va_arg (expr, type)
     tree expr, type;
{
  return build1 (VA_ARG_EXPR, type, expr);
}

/* Given a type, apply default promotions wrt unnamed function arguments
   and return the new type.  Return NULL_TREE if no change.  */
/* ??? There is a function of the same name in the C++ front end that
   does something similar, but is more thorough and does not return NULL
   if no change.  We could perhaps share code, but it would make the
   self_promoting_type property harder to identify.  */

tree
simple_type_promotes_to (type)
     tree type;
{
  if (TYPE_MAIN_VARIANT (type) == float_type_node)
    return double_type_node;

  if (C_PROMOTING_INTEGER_TYPE_P (type))
    {
      /* Traditionally, unsignedness is preserved in default promotions.
         Also preserve unsignedness if not really getting any wider.  */
      if (TREE_UNSIGNED (type)
          && (flag_traditional
              || TYPE_PRECISION (type) == TYPE_PRECISION (integer_type_node)))
        return unsigned_type_node;
      return integer_type_node;
    }

  return NULL_TREE;
}

/* Return 1 if PARMS specifies a fixed number of parameters
   and none of their types is affected by default promotions.  */

int
self_promoting_args_p (parms)
     tree parms;
{
  register tree t;
  for (t = parms; t; t = TREE_CHAIN (t))
    {
      register tree type = TREE_VALUE (t);

      if (TREE_CHAIN (t) == 0 && type != void_type_node)
	return 0;

      if (type == 0)
	return 0;

      if (TYPE_MAIN_VARIANT (type) == float_type_node)
	return 0;

      if (C_PROMOTING_INTEGER_TYPE_P (type))
	return 0;
    }
  return 1;
}

/* Recognize certain built-in functions so we can make tree-codes
   other than CALL_EXPR.  We do this when it enables fold-const.c
   to do something useful.  */
/* ??? By rights this should go in builtins.c, but only C and C++
   implement build_{binary,unary}_op.  Not exactly sure what bits
   of functionality are actually needed from those functions, or
   where the similar functionality exists in the other front ends.  */

tree
expand_tree_builtin (function, params, coerced_params)
     tree function, params, coerced_params;
{
  enum tree_code code;

  if (DECL_BUILT_IN_CLASS (function) != BUILT_IN_NORMAL)
    return NULL_TREE;

  switch (DECL_FUNCTION_CODE (function))
    {
    case BUILT_IN_ABS:
    case BUILT_IN_LABS:
    case BUILT_IN_FABS:
      if (coerced_params == 0)
	return integer_zero_node;
      return build_unary_op (ABS_EXPR, TREE_VALUE (coerced_params), 0);

    case BUILT_IN_ISGREATER:
      if (TARGET_FLOAT_FORMAT == IEEE_FLOAT_FORMAT)
	code = UNLE_EXPR;
      else
	code = LE_EXPR;
      goto unordered_cmp;

    case BUILT_IN_ISGREATEREQUAL:
      if (TARGET_FLOAT_FORMAT == IEEE_FLOAT_FORMAT)
	code = UNLT_EXPR;
      else
	code = LT_EXPR;
      goto unordered_cmp;

    case BUILT_IN_ISLESS:
      if (TARGET_FLOAT_FORMAT == IEEE_FLOAT_FORMAT)
	code = UNGE_EXPR;
      else
	code = GE_EXPR;
      goto unordered_cmp;

    case BUILT_IN_ISLESSEQUAL:
      if (TARGET_FLOAT_FORMAT == IEEE_FLOAT_FORMAT)
	code = UNGT_EXPR;
      else
	code = GT_EXPR;
      goto unordered_cmp;

    case BUILT_IN_ISLESSGREATER:
      if (TARGET_FLOAT_FORMAT == IEEE_FLOAT_FORMAT)
	code = UNEQ_EXPR;
      else
	code = EQ_EXPR;
      goto unordered_cmp;

    case BUILT_IN_ISUNORDERED:
      if (TARGET_FLOAT_FORMAT != IEEE_FLOAT_FORMAT)
	return integer_zero_node;
      code = UNORDERED_EXPR;
      goto unordered_cmp;

    unordered_cmp:
      {
	tree arg0, arg1;

	if (params == 0
	    || TREE_CHAIN (params) == 0)
	  {
	    error ("too few arguments to function `%s'",
		   IDENTIFIER_POINTER (DECL_NAME (function)));
	    return error_mark_node;
	  }
	else if (TREE_CHAIN (TREE_CHAIN (params)) != 0)
	  {
	    error ("too many arguments to function `%s'",
		   IDENTIFIER_POINTER (DECL_NAME (function)));
	    return error_mark_node;
	  }

	arg0 = TREE_VALUE (params);
	arg1 = TREE_VALUE (TREE_CHAIN (params));
	arg0 = build_binary_op (code, arg0, arg1, 0);
	if (code != UNORDERED_EXPR)
	  arg0 = build_unary_op (TRUTH_NOT_EXPR, arg0, 0);
	return arg0;
      }
      break;

    default:
      break;
    }

  return NULL_TREE;
}
