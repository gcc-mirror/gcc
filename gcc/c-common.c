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
#include "flags.h"
#include "toplev.h"
#include "output.h"
#include "c-pragma.h"
#include "rtl.h"
#include "ggc.h"
#include "expr.h"
#include "c-common.h"
#include "tm_p.h"
#include "intl.h"
#include "diagnostic.h"

#if USE_CPPLIB
#include "cpplib.h"
cpp_reader  parse_in;
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

  The identifiers __FUNCTION__, __PRETTY_FUNCTION__, and __func__.

	tree function_id_node;
	tree pretty_function_id_node;
	tree func_id_node;

*/

tree c_global_trees[CTI_MAX];

/* Nonzero means don't recognize the non-ANSI builtin functions.  */

int flag_no_builtin;

/* Nonzero means don't recognize the non-ANSI builtin functions.
   -ansi sets this.  */

int flag_no_nonansi_builtin;

/* Nonzero means warn about possible violations of sequence point rules.  */

int warn_sequence_point;

/* The elements of `ridpointers' are identifier nodes for the reserved
   type names and storage classes.  It is indexed by a RID_... value.  */
tree *ridpointers;

tree (*make_fname_decl)                PARAMS ((tree, const char *, int));

/* If non-NULL, the address of a language-specific function that
   returns 1 for language-specific statement codes.  */
int (*lang_statement_code_p)           PARAMS ((enum tree_code));

/* If non-NULL, the address of a language-specific function that takes
   any action required right before expand_function_end is called.  */
void (*lang_expand_function_end)       PARAMS ((void));

/* If this variable is defined to a non-NULL value, it will be called
   after the file has been completely parsed.  */
void (*back_end_hook) PARAMS ((tree));

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
  tree if_stmt;
} if_elt;

static if_elt *if_stack;

/* Amount of space in the if statement stack.  */
static int if_stack_space = 0;

/* Stack pointer.  */
static int if_stack_pointer = 0;

/* Record the start of an if-then, and record the start of it
   for ambiguous else detection.  */

void
c_expand_start_cond (cond, compstmt_count)
     tree cond;
     int compstmt_count;
{
  tree if_stmt;

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

  if_stmt = build_stmt (IF_STMT, NULL_TREE, NULL_TREE, NULL_TREE);
  IF_COND (if_stmt) = cond;
  add_stmt (if_stmt);

  /* Record this if statement.  */
  if_stack[if_stack_pointer].compstmt_count = compstmt_count;
  if_stack[if_stack_pointer].file = input_filename;
  if_stack[if_stack_pointer].line = lineno;
  if_stack[if_stack_pointer].needs_warning = 0;
  if_stack[if_stack_pointer].if_stmt = if_stmt;
  if_stack_pointer++;
}

/* Called after the then-clause for an if-statement is processed.  */

void
c_finish_then ()
{
  tree if_stmt = if_stack[if_stack_pointer - 1].if_stmt;
  RECHAIN_STMTS (if_stmt, THEN_CLAUSE (if_stmt));
}

/* Record the end of an if-then.  Optionally warn if a nested
   if statement had an ambiguous else clause.  */

void
c_expand_end_cond ()
{
  if_stack_pointer--;
  if (if_stack[if_stack_pointer].needs_warning)
    warning_with_file_and_line (if_stack[if_stack_pointer].file,
				if_stack[if_stack_pointer].line,
				"suggest explicit braces to avoid ambiguous `else'");
  last_expr_type = NULL_TREE;
}

/* Called between the then-clause and the else-clause
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
}

/* Called after the else-clause for an if-statement is processed.  */

void
c_finish_else ()
{
  tree if_stmt = if_stack[if_stack_pointer - 1].if_stmt;
  RECHAIN_STMTS (if_stmt, ELSE_CLAUSE (if_stmt));
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

      /* ISO C99 defines __func__, which is a variable, not a string
	 constant, and which is not a defined symbol at file scope.  */
      (*make_fname_decl) (func_id_node, name, 0);
    }
  
  (*make_fname_decl) (function_id_node, name, 0);
  (*make_fname_decl) (pretty_function_id_node, printable_name, 1);
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
  const int nchars_max = flag_isoc99 ? 4095 : 509;

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

  if (pedantic && nchars > nchars_max && c_language == clk_c)
    pedwarn ("string length `%d' is greater than the minimum length `%d' ISO C%d is required to support",
	     nchars, nchars_max, flag_isoc99 ? 99 : 89);

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
	    if (decl)
	      TREE_USED (decl) = 1;
	    else
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
		  if (TYPE_PRECISION (typefm) > (TREE_UNSIGNED (type)
						 ? TYPE_PRECISION(uintmax_type_node)
						 : TYPE_PRECISION(intmax_type_node))
		      && pedantic)
		    pedwarn ("type with more precision than %s",
			     TREE_UNSIGNED (type) ? "uintmax_t" : "intmax_t");
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
	      {
		/* If we have a TYPE_DECL, then copy the type, so that we
		   don't accidentally modify a builtin type.  See pushdecl.  */
		if (decl && TREE_TYPE (decl) != error_mark_node
		    && DECL_ORIGINAL_TYPE (decl) == NULL_TREE)
		  {
		    tree tt = TREE_TYPE (decl);
		    DECL_ORIGINAL_TYPE (decl) = tt;
		    tt = build_type_copy (tt);
		    TYPE_NAME (tt) = decl;
		    TREE_USED (tt) = TREE_USED (decl);
		    TREE_TYPE (decl) = tt;
		    type = tt;
		  }

		TYPE_ALIGN (type) = (1 << i) * BITS_PER_UNIT;
		TYPE_USER_ALIGN (type) = 1;
	      }
	    else if (TREE_CODE (decl) != VAR_DECL
		     && TREE_CODE (decl) != FIELD_DECL)
	      error_with_decl (decl,
			       "alignment may not be specified for `%s'");
	    else
	      {
		DECL_ALIGN (decl) = (1 << i) * BITS_PER_UNIT;
		DECL_USER_ALIGN (decl) = 1;
	      }
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

	    if (format_type == strftime_format_type && first_arg_num != 0)
	      {
		error ("strftime formats cannot format arguments");
		continue;
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
	      /* This counts as a use of the object pointed to.  */
	      TREE_USED (id) = 1;

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

/* The meaningfully distinct length modifiers for format checking recognised
   by GCC.  */
enum format_lengths
{
  FMT_LEN_none,
  FMT_LEN_hh,
  FMT_LEN_h,
  FMT_LEN_l,
  FMT_LEN_ll,
  FMT_LEN_L,
  FMT_LEN_z,
  FMT_LEN_t,
  FMT_LEN_j,
  FMT_LEN_MAX
};


/* The standard versions in which various format features appeared.  */
enum format_std_version
{
  STD_C89,
  STD_C94,
  STD_C99,
  STD_EXT
};

/* The C standard version C++ is treated as equivalent to
   or inheriting from, for the purpose of format features supported.  */
#define CPLUSPLUS_STD_VER	STD_C89
/* The C standard version we are checking formats against when pedantic.  */
#define C_STD_VER		(c_language == clk_cplusplus		  \
				 ? CPLUSPLUS_STD_VER			  \
				 : (flag_isoc99				  \
				    ? STD_C99				  \
				    : (flag_isoc94 ? STD_C94 : STD_C89)))
/* The name to give to the standard version we are warning about when
   pedantic.  FEATURE_VER is the version in which the feature warned out
   appeared, which is higher than C_STD_VER.  */
#define C_STD_NAME(FEATURE_VER) (c_language == clk_cplusplus	\
				 ? "ISO C++"			\
				 : ((FEATURE_VER) == STD_EXT	\
				    ? "ISO C"			\
				    : "ISO C89"))

/* Flags that may apply to a particular kind of format checked by GCC.  */
enum
{
  /* This format converts arguments of types determined by the
     format string.  */
  FMT_FLAG_ARG_CONVERT = 1,
  /* The scanf allocation 'a' kludge applies to this format kind.  */
  FMT_FLAG_SCANF_A_KLUDGE = 2,
  /* A % during parsing a specifier is allowed to be a modified % rather
     that indicating the format is broken and we are out-of-sync.  */
  FMT_FLAG_FANCY_PERCENT_OK = 4,
  /* With $ operand numbers, it is OK to reference the same argument more
     than once.  */
  FMT_FLAG_DOLLAR_MULTIPLE = 8
  /* Not included here: details of whether width or precision may occur
     (controlled by width_char and precision_char); details of whether
     '*' can be used for these (width_type and precision_type); details
     of whether length modifiers can occur (length_char_specs); details
     of when $ operand numbers are allowed (always, for the formats
     supported, if arguments are converted).  */
};


/* Structure describing a length modifier supported in format checking, and
   possibly a doubled version such as "hh".  */
typedef struct
{
  /* Name of the single-character length modifier.  */
  const char *name;
  /* Index into a format_char_info.types array.  */
  enum format_lengths index;
  /* Standard version this length appears in.  */
  enum format_std_version std;
  /* Same, if the modifier can be repeated, or NULL if it can't.  */
  const char *double_name;
  enum format_lengths double_index;
  enum format_std_version double_std;
} format_length_info;


/* Structure desribing the combination of a conversion specifier
   (or a set of specifiers which act identically) and a length modifier.  */
typedef struct
{
  /* The standard version this combination of length and type appeared in.
     This is only relevant if greater than those for length and type
     individually; otherwise it is ignored.  */
  enum format_std_version std;
  /* The name to use for the type, if different from that generated internally
     (e.g., "signed size_t").  */
  const char *name;
  /* The type itself.  */
  tree *type;
} format_type_detail;


/* Macros to fill out tables of these.  */
#define BADLEN	{ 0, NULL, NULL }
#define NOLENGTHS	{ BADLEN, BADLEN, BADLEN, BADLEN, BADLEN, BADLEN, BADLEN, BADLEN, BADLEN }


/* Structure desribing a format conversion specifier (or a set of specifiers
   which act identically), and the length modifiers used with it.  */
typedef struct
{
  const char *format_chars;
  int pointer_count;
  enum format_std_version std;
  /* Types accepted for each length modifier.  */
  format_type_detail types[FMT_LEN_MAX];
  /* List of other modifier characters allowed with these specifiers.
     This lists flags, and additionally "w" for width, "p" for precision,
     "a" for scanf "a" allocation extension (not applicable in C99 mode),
     "*" for scanf suppression, and "E" and "O" for those strftime
     modifiers.  */
  const char *flag_chars;
  /* List of additional flags describing these conversion specifiers.
     "c" for generic character pointers being allowed, "2" for strftime
     two digit year formats, "3" for strftime formats giving two digit
     years in some locales, "4" for "2" which becomes "3" with an "E" modifier,
     "o" if use of strftime "O" is a GNU extension beyond C99,
     "W" if the argument is a pointer which is dereferenced and written into,
     "i" for printf integer formats where the '0' flag is ignored with
     precision, and "[" for the starting character of a scanf scanset.  */
  const char *flags2;
} format_char_info;


/* Structure describing a flag accepted by some kind of format.  */
typedef struct
{
  /* The flag character in question (0 for end of array).  */
  int flag_char;
  /* Zero if this entry describes the flag character in general, or a
     non-zero character that may be found in flags2 if it describes the
     flag when used with certain formats only.  If the latter, only
     the first such entry found that applies to the current conversion
     specifier is used; the values of `name' and `long_name' it supplies
     will be used, if non-NULL and the standard version is higher than
     the unpredicated one, for any pedantic warning.  For example, 'o'
     for strftime formats (meaning 'O' is an extension over C99).  */
  int predicate;
  /* The name to use for this flag in diagnostic messages.  For example,
     N_("`0' flag"), N_("field width").  */
  const char *name;
  /* Long name for this flag in diagnostic messages; currently only used for
     "ISO C does not support ...".  For example, N_("the `I' printf flag").  */
  const char *long_name;
  /* The standard version in which it appeared.  */
  enum format_std_version std;
} format_flag_spec;


/* Structure describing a combination of flags that is bad for some kind
   of format.  */
typedef struct
{
  /* The first flag character in question (0 for end of array).  */
  int flag_char1;
  /* The second flag character.  */
  int flag_char2;
  /* Non-zero if the message should say that the first flag is ignored with
     the second, zero if the combination should simply be objected to.  */
  int ignored;
  /* Zero if this entry applies whenever this flag combination occurs,
     a non-zero character from flags2 if it only applies in some
     circumstances (e.g. 'i' for printf formats ignoring 0 with precision).  */
  int predicate;
} format_flag_pair;


/* Structure describing a particular kind of format processed by GCC.  */
typedef struct
{
  /* The name of this kind of format, for use in diagnostics.  */
  const char *name;
  /* Specifications of the length modifiers accepted; possibly NULL.  */
  const format_length_info *length_char_specs;
  /* Details of the conversion specification characters accepted.  */
  const format_char_info *conversion_specs;
  /* String listing the flag characters that are accepted.  */
  const char *flag_chars;
  /* String listing modifier characters (strftime) accepted.  May be NULL.  */
  const char *modifier_chars;
  /* Details of the flag characters, including pseudo-flags.  */
  const format_flag_spec *flag_specs;
  /* Details of bad combinations of flags.  */
  const format_flag_pair *bad_flag_pairs;
  /* Flags applicable to this kind of format.  */
  int flags;
  /* Flag character to treat a width as, or 0 if width not used.  */
  int width_char;
  /* Flag character to treat a precision as, or 0 if precision not used.  */
  int precision_char;
  /* If a flag character has the effect of suppressing the conversion of
     an argument ('*' in scanf), that flag character, otherwise 0.  */
  int suppression_char;
  /* Flag character to treat a length modifier as (ignored if length
     modifiers not used).  Need not be placed in flag_chars for conversion
     specifiers, but is used to check for bad combinations such as length
     modifier with assignment suppression in scanf.  */
  int length_code_char;
  /* Pointer to type of argument expected if '*' is used for a width,
     or NULL if '*' not used for widths.  */
  tree *width_type;
  /* Pointer to type of argument expected if '*' is used for a precision,
     or NULL if '*' not used for precisions.  */
  tree *precision_type;
} format_kind_info;


/* Structure describing details of a type expected in format checking,
   and the type to check against it.  */
typedef struct format_wanted_type
{
  /* The type wanted.  */
  tree wanted_type;
  /* The name of this type to use in diagnostics.  */
  const char *wanted_type_name;
  /* The level of indirection through pointers at which this type occurs.  */
  int pointer_count;
  /* Whether, when pointer_count is 1, to allow any character type when
     pedantic, rather than just the character or void type specified.  */
  int char_lenient_flag;
  /* Whether the argument, dereferenced once, is written into and so the
     argument must not be a pointer to a const-qualified type.  */
  int writing_in_flag;
  /* If warnings should be of the form "field precision is not type int",
     the name to use (in this case "field precision"), otherwise NULL,
     for "%s format, %s arg" type messages.  If (in an extension), this
     is a pointer type, wanted_type_name should be set to include the
     terminating '*' characters of the type name to give a correct
     message.  */
  const char *name;
  /* The actual parameter to check against the wanted type.  */
  tree param;
  /* The argument number of that parameter.  */
  int arg_num;
  /* The next type to check for this format conversion, or NULL if none.  */
  struct format_wanted_type *next;
} format_wanted_type;


static const format_length_info printf_length_specs[] =
{
  { "h", FMT_LEN_h, STD_C89, "hh", FMT_LEN_hh, STD_C99 },
  { "l", FMT_LEN_l, STD_C89, "ll", FMT_LEN_ll, STD_C99 },
  { "q", FMT_LEN_ll, STD_EXT, NULL, 0, 0 },
  { "L", FMT_LEN_L, STD_C89, NULL, 0, 0 },
  { "z", FMT_LEN_z, STD_C99, NULL, 0, 0 },
  { "Z", FMT_LEN_z, STD_EXT, NULL, 0, 0 },
  { "t", FMT_LEN_t, STD_C99, NULL, 0, 0 },
  { "j", FMT_LEN_j, STD_C99, NULL, 0, 0 },
  { NULL, 0, 0, NULL, 0, 0 }
};


/* This differs from printf_length_specs only in that "Z" is not accepted.  */
static const format_length_info scanf_length_specs[] =
{
  { "h", FMT_LEN_h, STD_C89, "hh", FMT_LEN_hh, STD_C99 },
  { "l", FMT_LEN_l, STD_C89, "ll", FMT_LEN_ll, STD_C99 },
  { "q", FMT_LEN_ll, STD_EXT, NULL, 0, 0 },
  { "L", FMT_LEN_L, STD_C89, NULL, 0, 0 },
  { "z", FMT_LEN_z, STD_C99, NULL, 0, 0 },
  { "t", FMT_LEN_t, STD_C99, NULL, 0, 0 },
  { "j", FMT_LEN_j, STD_C99, NULL, 0, 0 },
  { NULL, 0, 0, NULL, 0, 0 }
};


static const format_flag_spec printf_flag_specs[] =
{
  { ' ',  0, N_("` ' flag"),        N_("the ` ' printf flag"),              STD_C89 },
  { '+',  0, N_("`+' flag"),        N_("the `+' printf flag"),              STD_C89 },
  { '#',  0, N_("`#' flag"),        N_("the `#' printf flag"),              STD_C89 },
  { '0',  0, N_("`0' flag"),        N_("the `0' printf flag"),              STD_C89 },
  { '-',  0, N_("`-' flag"),        N_("the `-' printf flag"),              STD_C89 },
  { '\'', 0, N_("`'' flag"),        N_("the `'' printf flag"),              STD_EXT },
  { 'I',  0, N_("`I' flag"),        N_("the `I' printf flag"),              STD_EXT },
  { 'w',  0, N_("field width"),     N_("field width in printf format"),     STD_C89 },
  { 'p',  0, N_("precision"),       N_("precision in printf format"),       STD_C89 },
  { 'L',  0, N_("length modifier"), N_("length modifier in printf format"), STD_C89 },
  { 0, 0, NULL, NULL, 0 }
};


static const format_flag_pair printf_flag_pairs[] =
{
  { ' ', '+', 1, 0   },
  { '0', '-', 1, 0   },
  { '0', 'p', 1, 'i' },
  { 0, 0, 0, 0 }
};


static const format_flag_spec scanf_flag_specs[] =
{
  { '*',  0, N_("assignment suppression"), N_("assignment suppression"),          STD_C89 },
  { 'a',  0, N_("`a' flag"),               N_("the `a' scanf flag"),              STD_EXT },
  { 'w',  0, N_("field width"),            N_("field width in scanf format"),     STD_C89 },
  { 'L',  0, N_("length modifier"),        N_("length modifier in scanf format"), STD_C89 },
  { '\'', 0, N_("`'' flag"),               N_("the `'' scanf flag"),              STD_EXT },
  { 'I',  0, N_("`I' flag"),               N_("the `I' scanf flag"),              STD_EXT },
  { 0, 0, NULL, NULL, 0 }
};


static const format_flag_pair scanf_flag_pairs[] =
{
  { '*', 'L', 0, 0 },
  { 0, 0, 0, 0 }
};


static const format_flag_spec strftime_flag_specs[] =
{
  { '_', 0,   N_("`_' flag"),     N_("the `_' strftime flag"),          STD_EXT },
  { '-', 0,   N_("`-' flag"),     N_("the `-' strftime flag"),          STD_EXT },
  { '0', 0,   N_("`0' flag"),     N_("the `0' strftime flag"),          STD_EXT },
  { '^', 0,   N_("`^' flag"),     N_("the `^' strftime flag"),          STD_EXT },
  { '#', 0,   N_("`#' flag"),     N_("the `#' strftime flag"),          STD_EXT },
  { 'w', 0,   N_("field width"),  N_("field width in strftime format"), STD_EXT },
  { 'E', 0,   N_("`E' modifier"), N_("the `E' strftime modifier"),      STD_C99 },
  { 'O', 0,   N_("`O' modifier"), N_("the `O' strftime modifier"),      STD_C99 },
  { 'O', 'o', NULL,               N_("the `O' modifier"),               STD_EXT },
  { 0, 0, NULL, NULL, 0 }
};


static const format_flag_pair strftime_flag_pairs[] =
{
  { 'E', 'O', 0, 0 },
  { '_', '-', 0, 0 },
  { '_', '0', 0, 0 },
  { '-', '0', 0, 0 },
  { '^', '#', 0, 0 },
  { 0, 0, 0, 0 }
};


#define T_I	&integer_type_node
#define T89_I	{ STD_C89, NULL, T_I }
#define T99_I	{ STD_C99, NULL, T_I }
#define T_L	&long_integer_type_node
#define T89_L	{ STD_C89, NULL, T_L }
#define T_LL	&long_long_integer_type_node
#define T99_LL	{ STD_C99, NULL, T_LL }
#define TEX_LL	{ STD_EXT, NULL, T_LL }
#define T_S	&short_integer_type_node
#define T89_S	{ STD_C89, NULL, T_S }
#define T_UI	&unsigned_type_node
#define T89_UI	{ STD_C89, NULL, T_UI }
#define T99_UI	{ STD_C99, NULL, T_UI }
#define T_UL	&long_unsigned_type_node
#define T89_UL	{ STD_C89, NULL, T_UL }
#define T_ULL	&long_long_unsigned_type_node
#define T99_ULL	{ STD_C99, NULL, T_ULL }
#define TEX_ULL	{ STD_EXT, NULL, T_ULL }
#define T_US	&short_unsigned_type_node
#define T89_US	{ STD_C89, NULL, T_US }
#define T_F	&float_type_node
#define T89_F	{ STD_C89, NULL, T_F }
#define T99_F	{ STD_C99, NULL, T_F }
#define T_D	&double_type_node
#define T89_D	{ STD_C89, NULL, T_D }
#define T99_D	{ STD_C99, NULL, T_D }
#define T_LD	&long_double_type_node
#define T89_LD	{ STD_C89, NULL, T_LD }
#define T99_LD	{ STD_C99, NULL, T_LD }
#define T_C	&char_type_node
#define T89_C	{ STD_C89, NULL, T_C }
#define T_SC	&signed_char_type_node
#define T99_SC	{ STD_C99, NULL, T_SC }
#define T_UC	&unsigned_char_type_node
#define T99_UC	{ STD_C99, NULL, T_UC }
#define T_V	&void_type_node
#define T89_V	{ STD_C89, NULL, T_V }
#define T_W	&wchar_type_node
#define T94_W	{ STD_C94, "wchar_t", T_W }
#define TEX_W	{ STD_EXT, "wchar_t", T_W }
#define T_WI	&wint_type_node
#define T94_WI	{ STD_C94, "wint_t", T_WI }
#define TEX_WI	{ STD_EXT, "wint_t", T_WI }
#define T_ST    &c_size_type_node
#define T99_ST	{ STD_C99, "size_t", T_ST }
#define T_SST   &signed_size_type_node
#define T99_SST	{ STD_C99, "signed size_t", T_SST }
#define T_PD    &ptrdiff_type_node
#define T99_PD	{ STD_C99, "ptrdiff_t", T_PD }
#define T_UPD   &unsigned_ptrdiff_type_node
#define T99_UPD	{ STD_C99, "unsigned ptrdiff_t", T_UPD }
#define T_IM    &intmax_type_node
#define T99_IM	{ STD_C99, "intmax_t", T_IM }
#define T_UIM   &uintmax_type_node
#define T99_UIM	{ STD_C99, "uintmax_t", T_UIM }

static const format_char_info print_char_table[] =
{
  /* C89 conversion specifiers.  */
  { "di",  0, STD_C89, { T89_I,   T99_SC,  T89_S,   T89_L,   T99_LL,  TEX_LL,  T99_SST, T99_PD,  T99_IM  }, "-wp0 +'I", "i" },
  { "oxX", 0, STD_C89, { T89_UI,  T99_UC,  T89_US,  T89_UL,  T99_ULL, TEX_ULL, T99_ST,  T99_UPD, T99_UIM }, "-wp0#",    "i" },
  { "u",   0, STD_C89, { T89_UI,  T99_UC,  T89_US,  T89_UL,  T99_ULL, TEX_ULL, T99_ST,  T99_UPD, T99_UIM }, "-wp0'I",   "i" },
  { "fgG", 0, STD_C89, { T89_D,   BADLEN,  BADLEN,  T99_D,   BADLEN,  T89_LD,  BADLEN,  BADLEN,  BADLEN  }, "-wp0 +#'", ""  },
  { "eE",  0, STD_C89, { T89_D,   BADLEN,  BADLEN,  T99_D,   BADLEN,  T89_LD,  BADLEN,  BADLEN,  BADLEN  }, "-wp0 +#",  ""  },
  { "c",   0, STD_C89, { T89_I,   BADLEN,  BADLEN,  T94_WI,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "-w",       ""  },
  { "s",   1, STD_C89, { T89_C,   BADLEN,  BADLEN,  T94_W,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "-wp",      "c" },
  { "p",   1, STD_C89, { T89_V,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "-w",       "c" },
  { "n",   1, STD_C89, { T89_I,   T99_SC,  T89_S,   T89_L,   T99_LL,  BADLEN,  T99_SST, T99_PD,  T99_IM  }, "",         "W" },
  /* C99 conversion specifiers.  */
  { "F",   0, STD_C99, { T99_D,   BADLEN,  BADLEN,  T99_D,   BADLEN,  T99_LD,  BADLEN,  BADLEN,  BADLEN  }, "-wp0 +#'", ""  },
  { "aA",  0, STD_C99, { T99_D,   BADLEN,  BADLEN,  T99_D,   BADLEN,  T99_LD,  BADLEN,  BADLEN,  BADLEN  }, "-wp0 +#",  ""  },
  /* X/Open conversion specifiers.  */
  { "C",   0, STD_EXT, { TEX_WI,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "-w",       ""  },
  { "S",   1, STD_EXT, { TEX_W,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "-wp",      ""  },
  /* GNU conversion specifiers.  */
  { "m",   0, STD_EXT, { T89_V,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "-wp",      ""  },
  { NULL,  0, 0, NOLENGTHS, NULL, NULL }
};

static const format_char_info scan_char_table[] =
{
  /* C89 conversion specifiers.  */
  { "di",    1, STD_C89, { T89_I,   T99_SC,  T89_S,   T89_L,   T99_LL,  TEX_LL,  T99_SST, T99_PD,  T99_IM  }, "*w'I", "W"   },
  { "u",     1, STD_C89, { T89_UI,  T99_UC,  T89_US,  T89_UL,  T99_ULL, TEX_ULL, T99_ST,  T99_UPD, T99_UIM }, "*w'I", "W"   },
  { "oxX",   1, STD_C89, { T89_UI,  T99_UC,  T89_US,  T89_UL,  T99_ULL, TEX_ULL, T99_ST,  T99_UPD, T99_UIM }, "*w",   "W"   },
  { "efgEG", 1, STD_C89, { T89_F,   BADLEN,  BADLEN,  T89_D,   BADLEN,  T89_LD,  BADLEN,  BADLEN,  BADLEN  }, "*w'",  "W"   },
  { "c",     1, STD_C89, { T89_C,   BADLEN,  BADLEN,  T94_W,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "*w",   "cW"  },
  { "s",     1, STD_C89, { T89_C,   BADLEN,  BADLEN,  T94_W,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "*aw",  "cW"  },
  { "[",     1, STD_C89, { T89_C,   BADLEN,  BADLEN,  T94_W,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "*aw",  "cW[" },
  { "p",     2, STD_C89, { T89_V,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "*w",   "W"   },
  { "n",     1, STD_C89, { T89_I,   T99_SC,  T89_S,   T89_L,   T99_LL,  BADLEN,  T99_SST, T99_PD,  T99_IM  }, "",     "W"   },
  /* C99 conversion specifiers.  */
  { "FaA",   1, STD_C99, { T99_F,   BADLEN,  BADLEN,  T99_D,   BADLEN,  T99_LD,  BADLEN,  BADLEN,  BADLEN  }, "*w'",  "W"   },
  /* X/Open conversion specifiers.  */
  { "C",     1, STD_EXT, { TEX_W,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "*w",   "W"   },
  { "S",     1, STD_EXT, { TEX_W,   BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN,  BADLEN  }, "*aw",  "W"   },
  { NULL, 0, 0, NOLENGTHS, NULL, NULL }
};

static format_char_info time_char_table[] =
{
  /* C89 conversion specifiers.  */
  { "ABZab",		0, STD_C89, NOLENGTHS, "^#",     ""   },
  { "cx", 		0, STD_C89, NOLENGTHS, "E",      "3"  },
  { "HIMSUWdmw",	0, STD_C89, NOLENGTHS, "-_0Ow",  ""   },
  { "j",		0, STD_C89, NOLENGTHS, "-_0Ow",  "o"  },
  { "p",		0, STD_C89, NOLENGTHS, "#",      ""   },
  { "X",		0, STD_C89, NOLENGTHS, "E",      ""   },
  { "y", 		0, STD_C89, NOLENGTHS, "EO-_0w", "4"  },
  { "Y",		0, STD_C89, NOLENGTHS, "-_0EOw", "o"  },
  { "%",		0, STD_C89, NOLENGTHS, "",       ""   },
  /* C99 conversion specifiers.  */
  { "C",		0, STD_C99, NOLENGTHS, "-_0EOw", "o"  },
  { "D", 		0, STD_C99, NOLENGTHS, "",       "2"  },
  { "eVu",		0, STD_C99, NOLENGTHS, "-_0Ow",  ""   },
  { "FRTnrt",		0, STD_C99, NOLENGTHS, "",       ""   },
  { "g", 		0, STD_C99, NOLENGTHS, "O-_0w",  "2o" },
  { "G",		0, STD_C99, NOLENGTHS, "-_0Ow",  "o"  },
  { "h",		0, STD_C99, NOLENGTHS, "^#",     ""   },
  { "z",		0, STD_C99, NOLENGTHS, "O",      "o"  },
  /* GNU conversion specifiers.  */
  { "kls",		0, STD_EXT, NOLENGTHS, "-_0Ow",  ""   },
  { "P",		0, STD_EXT, NOLENGTHS, "",       ""   },
  { NULL,		0, 0, NOLENGTHS, NULL, NULL }
};


/* This must be in the same order as enum format_type.  */
static const format_kind_info format_types[] =
{
  { "printf",   printf_length_specs, print_char_table, " +#0-'I", NULL, 
    printf_flag_specs, printf_flag_pairs,
    FMT_FLAG_ARG_CONVERT|FMT_FLAG_DOLLAR_MULTIPLE, 'w', 'p', 0, 'L',
    &integer_type_node, &integer_type_node
  },
  { "scanf",    scanf_length_specs,  scan_char_table,  "*'I", NULL, 
    scanf_flag_specs, scanf_flag_pairs,
    FMT_FLAG_ARG_CONVERT|FMT_FLAG_SCANF_A_KLUDGE, 'w', 0, '*', 'L',
    NULL, NULL
  },
  { "strftime", NULL,                time_char_table,  "_-0^#", "EO",
    strftime_flag_specs, strftime_flag_pairs,
    FMT_FLAG_FANCY_PERCENT_OK, 'w', 0, 0, 0,
    NULL, NULL
  }
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

/* Structure detailing the results of checking a format function call
   where the format expression may be a conditional expression with
   many leaves resulting from nested conditional expressions.  */
typedef struct
{
  /* Number of leaves of the format argument that could not be checked
     as they were not string literals.  */
  int number_non_literal;
  /* Number of leaves of the format argument that were null pointers or
     string literals, but had extra format arguments.  */
  int number_extra_args;
  /* Number of leaves of the format argument that were null pointers or
     string literals, but had extra format arguments and used $ operand
     numbers.  */
  int number_dollar_extra_args;
  /* Number of leaves of the format argument that were wide string
     literals.  */
  int number_wide;
  /* Number of leaves of the format argument that were empty strings.  */
  int number_empty;
  /* Number of leaves of the format argument that were unterminated
     strings.  */
  int number_unterminated;
  /* Number of leaves of the format argument that were not counted above.  */
  int number_other;
} format_check_results;

static void check_format_info	PARAMS ((int *, function_format_info *, tree));
static void check_format_info_recurse PARAMS ((int *, format_check_results *,
					       function_format_info *, tree,
					       tree, int));
static void check_format_info_main PARAMS ((int *, format_check_results *,
					    function_format_info *,
					    const char *, int, tree, int));
static void status_warning PARAMS ((int *, const char *, ...))
     ATTRIBUTE_PRINTF_2;

static void init_dollar_format_checking		PARAMS ((int, tree));
static int maybe_read_dollar_number		PARAMS ((int *, const char **, int,
							 tree, tree *,
							 const format_kind_info *));
static void finish_dollar_format_checking	PARAMS ((int *, format_check_results *));

static const format_flag_spec *get_flag_spec	PARAMS ((const format_flag_spec *,
							 int, const char *));

static void check_format_types	PARAMS ((int *, format_wanted_type *));
static int is_valid_printf_arglist PARAMS ((tree));
static rtx c_expand_builtin PARAMS ((tree, rtx, enum machine_mode, enum expand_modifier));
static rtx c_expand_builtin_printf PARAMS ((tree, rtx, enum machine_mode,
					    enum expand_modifier, int));

/* Initialize the table of functions to perform format checking on.
   The ISO C functions are always checked (whether <stdio.h> is
   included or not), since it is common to call printf without
   including <stdio.h>.  There shouldn't be a problem with this,
   since ISO C reserves these function names whether you include the
   header file or not.  In any case, the checking is harmless.  With
   -ffreestanding, these default attributes are disabled, and must be
   specified manually if desired.

   Also initialize the name of function that modify the format string for
   internationalization purposes.  */

void
init_function_format_info ()
{
  if (flag_hosted)
    {
      /* Functions from ISO/IEC 9899:1990.  */
      record_function_format (get_identifier ("printf"), NULL_TREE,
			      printf_format_type, 1, 2);
      record_function_format (get_identifier ("__builtin_printf"), NULL_TREE,
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
    }

  if (flag_hosted && flag_isoc99)
    {
      /* ISO C99 adds the snprintf and vscanf family functions.  */
      record_function_format (get_identifier ("snprintf"), NULL_TREE,
			      printf_format_type, 3, 4);
      record_function_format (get_identifier ("vsnprintf"), NULL_TREE,
			      printf_format_type, 3, 0);
      record_function_format (get_identifier ("vscanf"), NULL_TREE,
			      scanf_format_type, 1, 0);
      record_function_format (get_identifier ("vfscanf"), NULL_TREE,
			      scanf_format_type, 2, 0);
      record_function_format (get_identifier ("vsscanf"), NULL_TREE,
			      scanf_format_type, 2, 0);
    }

  if (flag_hosted && flag_noniso_default_format_attributes)
    {
      /* Uniforum/GNU gettext functions, not in ISO C.  */
      record_international_format (get_identifier ("gettext"), NULL_TREE, 1);
      record_international_format (get_identifier ("dgettext"), NULL_TREE, 2);
      record_international_format (get_identifier ("dcgettext"), NULL_TREE, 2);
    }
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

/* Check the argument list of a call to printf, scanf, etc.
   NAME is the function identifier.
   ASSEMBLER_NAME is the function's assembler identifier.
   (Either NAME or ASSEMBLER_NAME, but not both, may be NULL_TREE.)
   PARAMS is the list of argument values.  Also, if -Wmissing-format-attribute,
   warn for calls to vprintf or vscanf in functions with no such format
   attribute themselves.  */

void
check_function_format (status, name, assembler_name, params)
     int *status;
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
	  check_format_info (status, info, params);
	  if (warn_missing_format_attribute && info->first_arg_num == 0
	      && (format_types[info->format_type].flags & FMT_FLAG_ARG_CONVERT))
	    {
	      function_format_info *info2;
	      for (info2 = function_format_list; info2; info2 = info2->next)
		if ((info2->assembler_name
		     ? (info2->assembler_name == DECL_ASSEMBLER_NAME (current_function_decl))
		     : (info2->name == DECL_NAME (current_function_decl)))
		    && info2->format_type == info->format_type)
		  break;
	      if (info2 == NULL)
		warning ("function might be possible candidate for `%s' format attribute",
			 format_types[info->format_type].name);
	    }
	  break;
	}
    }
}

/* This function replaces `warning' inside the printf format checking
   functions.  If the `status' parameter is non-NULL, then it is
   dereferenced and set to 1 whenever a warning is caught.  Otherwise
   it warns as usual by replicating the innards of the warning
   function from diagnostic.c.  */
static void
status_warning VPARAMS ((int *status, const char *msgid, ...))
{
#ifndef ANSI_PROTOTYPES
  int *status;
  const char *msgid;
#endif
  va_list ap;
  diagnostic_context dc;

  if (status)
    *status = 1;
  else
    {
      VA_START (ap, msgid);

#ifndef ANSI_PROTOTYPES
      status = va_arg (ap, int *);
      msgid = va_arg (ap, const char *);
#endif

      /* This duplicates the warning function behavior.  */
      set_diagnostic_context
	(&dc, msgid, &ap, input_filename, lineno, /* warn = */ 1);
      report_diagnostic (&dc);

      va_end (ap);
    }
}

/* Variables used by the checking of $ operand number formats.  */
static char *dollar_arguments_used = NULL;
static int dollar_arguments_alloc = 0;
static int dollar_arguments_count;
static int dollar_first_arg_num;
static int dollar_max_arg_used;
static int dollar_format_warned;

/* Initialize the checking for a format string that may contain $
   parameter number specifications; we will need to keep track of whether
   each parameter has been used.  FIRST_ARG_NUM is the number of the first
   argument that is a parameter to the format, or 0 for a vprintf-style
   function; PARAMS is the list of arguments starting at this argument.  */

static void
init_dollar_format_checking (first_arg_num, params)
     int first_arg_num;
     tree params;
{
  dollar_first_arg_num = first_arg_num;
  dollar_arguments_count = 0;
  dollar_max_arg_used = 0;
  dollar_format_warned = 0;
  if (first_arg_num > 0)
    {
      while (params)
	{
	  dollar_arguments_count++;
	  params = TREE_CHAIN (params);
	}
    }
  if (dollar_arguments_alloc < dollar_arguments_count)
    {
      if (dollar_arguments_used)
	free (dollar_arguments_used);
      dollar_arguments_alloc = dollar_arguments_count;
      dollar_arguments_used = xmalloc (dollar_arguments_alloc);
    }
  if (dollar_arguments_alloc)
    memset (dollar_arguments_used, 0, dollar_arguments_alloc);
}


/* Look for a decimal number followed by a $ in *FORMAT.  If DOLLAR_NEEDED
   is set, it is an error if one is not found; otherwise, it is OK.  If
   such a number is found, check whether it is within range and mark that
   numbered operand as being used for later checking.  Returns the operand
   number if found and within range, zero if no such number was found and
   this is OK, or -1 on error.  PARAMS points to the first operand of the
   format; PARAM_PTR is made to point to the parameter referred to.  If
   a $ format is found, *FORMAT is updated to point just after it.  */

static int
maybe_read_dollar_number (status, format, dollar_needed, params, param_ptr,
			  fki)
     int *status;
     const char **format;
     int dollar_needed;
     tree params;
     tree *param_ptr;
     const format_kind_info *fki;
{
  int argnum;
  int overflow_flag;
  const char *fcp = *format;
  if (*fcp < '0' || *fcp > '9')
    {
      if (dollar_needed)
	{
	  status_warning (status, "missing $ operand number in format");
	  return -1;
	}
      else
	return 0;
    }
  argnum = 0;
  overflow_flag = 0;
  while (*fcp >= '0' && *fcp <= '9')
    {
      int nargnum;
      nargnum = 10 * argnum + (*fcp - '0');
      if (nargnum < 0 || nargnum / 10 != argnum)
	overflow_flag = 1;
      argnum = nargnum;
      fcp++;
    }
  if (*fcp != '$')
    {
      if (dollar_needed)
	{
	  status_warning (status, "missing $ operand number in format");
	  return -1;
	}
      else
	return 0;
    }
  *format = fcp + 1;
  if (pedantic && !dollar_format_warned)
    {
      status_warning (status,
		      "%s does not support %%n$ operand number formats",
		      C_STD_NAME (STD_EXT));
      dollar_format_warned = 1;
    }
  if (overflow_flag || argnum == 0
      || (dollar_first_arg_num && argnum > dollar_arguments_count))
    {
      status_warning (status, "operand number out of range in format");
      return -1;
    }
  if (argnum > dollar_max_arg_used)
    dollar_max_arg_used = argnum;
  /* For vprintf-style functions we may need to allocate more memory to
     track which arguments are used.  */
  while (dollar_arguments_alloc < dollar_max_arg_used)
    {
      int nalloc;
      nalloc = 2 * dollar_arguments_alloc + 16;
      dollar_arguments_used = xrealloc (dollar_arguments_used, nalloc);
      memset (dollar_arguments_used + dollar_arguments_alloc, 0,
	      nalloc - dollar_arguments_alloc);
      dollar_arguments_alloc = nalloc;
    }
  if (!(fki->flags & FMT_FLAG_DOLLAR_MULTIPLE)
      && dollar_arguments_used[argnum - 1] == 1)
    {
      dollar_arguments_used[argnum - 1] = 2;
      status_warning (status,
		      "format argument %d used more than once in %s format",
		      argnum, fki->name);
    }
  else
    dollar_arguments_used[argnum - 1] = 1;
  if (dollar_first_arg_num)
    {
      int i;
      *param_ptr = params;
      for (i = 1; i < argnum && *param_ptr != 0; i++)
	*param_ptr = TREE_CHAIN (*param_ptr);

      if (*param_ptr == 0)
	{
	  /* This case shouldn't be caught here.  */
	  abort ();
	}
    }
  else
    *param_ptr = 0;
  return argnum;
}


/* Finish the checking for a format string that used $ operand number formats
   instead of non-$ formats.  We check for unused operands before used ones
   (a serious error, since the implementation of the format function
   can't know what types to pass to va_arg to find the later arguments).
   and for unused operands at the end of the format (if we know how many
   arguments the format had, so not for vprintf).  If there were operand
   numbers out of range on a non-vprintf-style format, we won't have reached
   here.  */

static void
finish_dollar_format_checking (status, res)
     int *status;
     format_check_results *res;
{
  int i;
  for (i = 0; i < dollar_max_arg_used; i++)
    {
      if (!dollar_arguments_used[i])
	status_warning (status, "format argument %d unused before used argument %d in $-style format",
		 i + 1, dollar_max_arg_used);
    }
  if (dollar_first_arg_num && dollar_max_arg_used < dollar_arguments_count)
    {
      res->number_other--;
      res->number_dollar_extra_args++;
    }
}


/* Retrieve the specification for a format flag.  SPEC contains the
   specifications for format flags for the applicable kind of format.
   FLAG is the flag in question.  If PREDICATES is NULL, the basic
   spec for that flag must be retrieved and this function aborts if
   it cannot be found.  If PREDICATES is not NULL, it is a string listing
   possible predicates for the spec entry; if an entry predicated on any
   of these is found, it is returned, otherwise NULL is returned.  */

static const format_flag_spec *
get_flag_spec (spec, flag, predicates)
     const format_flag_spec *spec;
     int flag;
     const char *predicates;
{
  int i;
  for (i = 0; spec[i].flag_char != 0; i++)
    {
      if (spec[i].flag_char != flag)
	continue;
      if (predicates != NULL)
	{
	  if (spec[i].predicate != 0
	      && strchr (predicates, spec[i].predicate) != 0)
	    return &spec[i];
	}
      else if (spec[i].predicate == 0)
	return &spec[i];
    }
  if (predicates == NULL)
    abort ();
  else
    return NULL;
}


/* Check the argument list of a call to printf, scanf, etc.
   INFO points to the function_format_info structure.
   PARAMS is the list of argument values.  */

static void
check_format_info (status, info, params)
     int *status;
     function_format_info *info;
     tree params;
{
  int arg_num;
  tree format_tree;
  format_check_results res;
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

  res.number_non_literal = 0;
  res.number_extra_args = 0;
  res.number_dollar_extra_args = 0;
  res.number_wide = 0;
  res.number_empty = 0;
  res.number_unterminated = 0;
  res.number_other = 0;

  check_format_info_recurse (status, &res, info, format_tree, params, arg_num);

  if (res.number_non_literal > 0)
    {
      /* Functions taking a va_list normally pass a non-literal format
	 string.  These functions typically are declared with
	 first_arg_num == 0, so avoid warning in those cases.  */
      if (info->first_arg_num != 0 && warn_format > 1)
	status_warning (status, "format not a string literal, argument types not checked");
    }

  /* If there were extra arguments to the format, normally warn.  However,
     the standard does say extra arguments are ignored, so in the specific
     case where we have multiple leaves (conditional expressions or
     ngettext) allow extra arguments if at least one leaf didn't have extra
     arguments, but was otherwise OK (either non-literal or checked OK).
     If the format is an empty string, this should be counted similarly to the
     case of extra format arguments.  */
  if (res.number_extra_args > 0 && res.number_non_literal == 0
      && res.number_other == 0)
    status_warning (status, "too many arguments for format");
  if (res.number_dollar_extra_args > 0 && res.number_non_literal == 0
      && res.number_other == 0)
    status_warning (status, "unused arguments in $-style format");
  if (res.number_empty > 0 && res.number_non_literal == 0
      && res.number_other == 0)
    status_warning (status, "zero-length format string");

  if (res.number_wide > 0)
    status_warning (status, "format is a wide character string");

  if (res.number_unterminated > 0)
    status_warning (status, "unterminated format string");
}


/* Recursively check a call to a format function.  FORMAT_TREE is the
   format parameter, which may be a conditional expression in which
   both halves should be checked.  ARG_NUM is the number of the
   format argument; PARAMS points just after it in the argument list.  */

static void
check_format_info_recurse (status, res, info, format_tree, params, arg_num)
     int *status;
     format_check_results *res;
     function_format_info *info;
     tree format_tree;
     tree params;
     int arg_num;
{
  int format_length;
  const char *format_chars;
  tree array_size = 0;
  tree array_init;

  if (TREE_CODE (format_tree) == NOP_EXPR)
    {
      /* Strip coercion.  */
      check_format_info_recurse (status, res, info,
				 TREE_OPERAND (format_tree, 0), params,
				 arg_num);
      return;
    }

  if (TREE_CODE (format_tree) == CALL_EXPR
      && TREE_CODE (TREE_OPERAND (format_tree, 0)) == ADDR_EXPR
      && (TREE_CODE (TREE_OPERAND (TREE_OPERAND (format_tree, 0), 0))
	  == FUNCTION_DECL))
    {
      tree function = TREE_OPERAND (TREE_OPERAND (format_tree, 0), 0);

      /* See if this is a call to a known internationalization function
	 that modifies the format arg.  */
      international_format_info *iinfo;

      for (iinfo = international_format_list; iinfo; iinfo = iinfo->next)
	if (iinfo->assembler_name
	    ? (iinfo->assembler_name == DECL_ASSEMBLER_NAME (function))
	    : (iinfo->name == DECL_NAME (function)))
	  {
	    tree inner_args;
	    int i;

	    for (inner_args = TREE_OPERAND (format_tree, 1), i = 1;
		 inner_args != 0;
		 inner_args = TREE_CHAIN (inner_args), i++)
	      if (i == iinfo->format_num)
		{
		  /* FIXME: with Marc Espie's __attribute__((nonnull))
		     patch in GCC, we will have chained attributes,
		     and be able to handle functions like ngettext
		     with multiple format_arg attributes properly.  */
		  check_format_info_recurse (status, res, info,
					     TREE_VALUE (inner_args), params,
					     arg_num);
		  return;
		}
	  }
    }

  if (TREE_CODE (format_tree) == COND_EXPR)
    {
      /* Check both halves of the conditional expression.  */
      check_format_info_recurse (status, res, info,
				 TREE_OPERAND (format_tree, 1), params,
				 arg_num);
      check_format_info_recurse (status, res, info,
				 TREE_OPERAND (format_tree, 2), params,
				 arg_num);
      return;
    }

  if (integer_zerop (format_tree))
    {
      /* FIXME: this warning should go away once Marc Espie's
	 __attribute__((nonnull)) patch is in.  Instead, checking for
	 nonnull attributes should probably change this function to act
	 specially if info == NULL and add a res->number_null entry for
	 that case, or maybe add a function pointer to be called at
	 the end instead of hardcoding check_format_info_main.  */
      status_warning (status, "null format string");

      /* Skip to first argument to check, so we can see if this format
	 has any arguments (it shouldn't).  */
      while (arg_num + 1 < info->first_arg_num)
	{
	  if (params == 0)
	    return;
	  params = TREE_CHAIN (params);
	  ++arg_num;
	}

      if (params == 0)
	res->number_other++;
      else
	res->number_extra_args++;

      return;
    }

  if (TREE_CODE (format_tree) != ADDR_EXPR)
    {
      res->number_non_literal++;
      return;
    }
  format_tree = TREE_OPERAND (format_tree, 0);
  if (TREE_CODE (format_tree) == VAR_DECL
      && TREE_CODE (TREE_TYPE (format_tree)) == ARRAY_TYPE
      && (array_init = decl_constant_value (format_tree)) != format_tree
      && TREE_CODE (array_init) == STRING_CST)
    {
      /* Extract the string constant initializer.  Note that this may include
	 a trailing NUL character that is not in the array (e.g.
	 const char a[3] = "foo";).  */
      array_size = DECL_SIZE_UNIT (format_tree);
      format_tree = array_init;
    }
  if (TREE_CODE (format_tree) != STRING_CST)
    {
      res->number_non_literal++;
      return;
    }
  if (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (format_tree))) != char_type_node)
    {
      res->number_wide++;
      return;
    }
  format_chars = TREE_STRING_POINTER (format_tree);
  format_length = TREE_STRING_LENGTH (format_tree);
  if (array_size != 0)
    {
      /* Variable length arrays can't be initialized.  */
      if (TREE_CODE (array_size) != INTEGER_CST)
	abort ();
      if (host_integerp (array_size, 0))
	{
	  HOST_WIDE_INT array_size_value = TREE_INT_CST_LOW (array_size);
	  if (array_size_value > 0
	      && array_size_value == (int) array_size_value
	      && format_length > array_size_value)
	    format_length = array_size_value;
	}
    }
  if (format_length < 1)
    {
      res->number_unterminated++;
      return;
    }
  if (format_length == 1)
    {
      res->number_empty++;
      return;
    }
  if (format_chars[--format_length] != 0)
    {
      res->number_unterminated++;
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
  /* Provisionally increment res->number_other; check_format_info_main
     will decrement it if it finds there are extra arguments, but this way
     need not adjust it for every return.  */
  res->number_other++;
  check_format_info_main (status, res, info, format_chars, format_length,
			  params, arg_num);
}


/* Do the main part of checking a call to a format function.  FORMAT_CHARS
   is the NUL-terminated format string (which at this point may contain
   internal NUL characters); FORMAT_LENGTH is its length (excluding the
   terminating NUL character).  ARG_NUM is one less than the number of
   the first format argument to check; PARAMS points to that format
   argument in the list of arguments.  */

static void
check_format_info_main (status, res, info, format_chars, format_length,
			params, arg_num)
     int *status;
     format_check_results *res;
     function_format_info *info;
     const char *format_chars;
     int format_length;
     tree params;
     int arg_num;
{
  const char *orig_format_chars = format_chars;
  tree first_fillin_param = params;

  const format_kind_info *fki = &format_types[info->format_type];
  const format_flag_spec *flag_specs = fki->flag_specs;
  const format_flag_pair *bad_flag_pairs = fki->bad_flag_pairs;

  /* -1 if no conversions taking an operand have been found; 0 if one has
     and it didn't use $; 1 if $ formats are in use.  */
  int has_operand_number = -1;

  init_dollar_format_checking (info->first_arg_num, first_fillin_param);

  while (1)
    {
      int i;
      int suppressed = FALSE;
      const char *length_chars = NULL;
      enum format_lengths length_chars_val = FMT_LEN_none;
      enum format_std_version length_chars_std = STD_C89;
      int format_char;
      tree cur_param;
      tree wanted_type;
      int main_arg_num = 0;
      tree main_arg_params = 0;
      enum format_std_version wanted_type_std;
      const char *wanted_type_name;
      format_wanted_type width_wanted_type;
      format_wanted_type precision_wanted_type;
      format_wanted_type main_wanted_type;
      format_wanted_type *first_wanted_type = NULL;
      format_wanted_type *last_wanted_type = NULL;
      const format_length_info *fli = NULL;
      const format_char_info *fci = NULL;
      char flag_chars[256];
      int aflag = 0;
      if (*format_chars == 0)
	{
	  if (format_chars - orig_format_chars != format_length)
	    status_warning (status, "embedded `\\0' in format");
	  if (info->first_arg_num != 0 && params != 0
	      && has_operand_number <= 0)
	    {
	      res->number_other--;
	      res->number_extra_args++;
	    }
	  if (has_operand_number > 0)
	    finish_dollar_format_checking (status, res);
	  return;
	}
      if (*format_chars++ != '%')
	continue;
      if (*format_chars == 0)
	{
	  status_warning (status, "spurious trailing `%%' in format");
	  continue;
	}
      if (*format_chars == '%')
	{
	  ++format_chars;
	  continue;
	}
      flag_chars[0] = 0;

      if ((fki->flags & FMT_FLAG_ARG_CONVERT) && has_operand_number != 0)
	{
	  /* Possibly read a $ operand number at the start of the format.
	     If one was previously used, one is required here.  If one
	     is not used here, we can't immediately conclude this is a
	     format without them, since it could be printf %m or scanf %*.  */
	  int opnum;
	  opnum = maybe_read_dollar_number (status, &format_chars, 0,
					    first_fillin_param,
					    &main_arg_params, fki);
	  if (opnum == -1)
	    return;
	  else if (opnum > 0)
	    {
	      has_operand_number = 1;
	      main_arg_num = opnum + info->first_arg_num - 1;
	    }
	}

      /* Read any format flags, but do not yet validate them beyond removing
	 duplicates, since in general validation depends on the rest of
	 the format.  */
      while (*format_chars != 0
	     && strchr (fki->flag_chars, *format_chars) != 0)
	{
	  if (strchr (flag_chars, *format_chars) != 0)
	    {
	      const format_flag_spec *s = get_flag_spec (flag_specs,
							 *format_chars, NULL);
	      status_warning (status, "repeated %s in format", _(s->name));
	    }
	  else
	    {
	      i = strlen (flag_chars);
	      flag_chars[i++] = *format_chars;
	      flag_chars[i] = 0;
	    }
	  ++format_chars;
	}

      /* Read any format width, possibly * or *m$.  */
      if (fki->width_char != 0)
	{
	  if (fki->width_type != NULL && *format_chars == '*')
	    {
	      i = strlen (flag_chars);
	      flag_chars[i++] = fki->width_char;
	      flag_chars[i] = 0;
	      /* "...a field width...may be indicated by an asterisk.
		 In this case, an int argument supplies the field width..."  */
	      ++format_chars;
	      if (params == 0)
		{
		  status_warning (status, "too few arguments for format");
		  return;
		}
	      if (has_operand_number != 0)
		{
		  int opnum;
		  opnum = maybe_read_dollar_number (status, &format_chars,
						    has_operand_number == 1,
						    first_fillin_param,
						    &params, fki);
		  if (opnum == -1)
		    return;
		  else if (opnum > 0)
		    {
		      has_operand_number = 1;
		      arg_num = opnum + info->first_arg_num - 1;
		    }
		  else
		    has_operand_number = 0;
		}
	      if (info->first_arg_num != 0)
		{
		  cur_param = TREE_VALUE (params);
		  if (has_operand_number <= 0)
		    {
		      params = TREE_CHAIN (params);
		      ++arg_num;
		    }
		  width_wanted_type.wanted_type = *fki->width_type;
		  width_wanted_type.wanted_type_name = NULL;
		  width_wanted_type.pointer_count = 0;
		  width_wanted_type.char_lenient_flag = 0;
		  width_wanted_type.writing_in_flag = 0;
		  width_wanted_type.name = _("field width");
		  width_wanted_type.param = cur_param;
		  width_wanted_type.arg_num = arg_num;
		  width_wanted_type.next = NULL;
		  if (last_wanted_type != 0)
		    last_wanted_type->next = &width_wanted_type;
		  if (first_wanted_type == 0)
		    first_wanted_type = &width_wanted_type;
		  last_wanted_type = &width_wanted_type;
		}
	    }
	  else
	    {
	      /* Possibly read a numeric width.  If the width is zero,
		 we complain; for scanf this is bad according to the
		 standard, and for printf and strftime it cannot occur
		 because 0 is a flag.  */
	      int non_zero_width_char = FALSE;
	      int found_width = FALSE;
	      while (ISDIGIT (*format_chars))
		{
		  found_width = TRUE;
		  if (*format_chars != '0')
		    non_zero_width_char = TRUE;
		  ++format_chars;
		}
	      if (found_width && !non_zero_width_char)
		status_warning (status, "zero width in %s format",
				fki->name);
	      if (found_width)
		{
		  i = strlen (flag_chars);
		  flag_chars[i++] = fki->width_char;
		  flag_chars[i] = 0;
		}
	    }
	}

      /* Read any format precision, possibly * or *m$.  */
      if (fki->precision_char != 0 && *format_chars == '.')
	{
	  ++format_chars;
	  i = strlen (flag_chars);
	  flag_chars[i++] = fki->precision_char;
	  flag_chars[i] = 0;
	  if (fki->precision_type != NULL && *format_chars == '*')
	    {
	      /* "...a...precision...may be indicated by an asterisk.
		 In this case, an int argument supplies the...precision."  */
	      ++format_chars;
	      if (has_operand_number != 0)
		{
		  int opnum;
		  opnum = maybe_read_dollar_number (status, &format_chars,
						    has_operand_number == 1,
						    first_fillin_param,
						    &params, fki);
		  if (opnum == -1)
		    return;
		  else if (opnum > 0)
		    {
		      has_operand_number = 1;
		      arg_num = opnum + info->first_arg_num - 1;
		    }
		  else
		    has_operand_number = 0;
		}
	      if (info->first_arg_num != 0)
		{
		  if (params == 0)
		    {
		      status_warning (status, "too few arguments for format");
		      return;
		    }
		  cur_param = TREE_VALUE (params);
		  if (has_operand_number <= 0)
		    {
		      params = TREE_CHAIN (params);
		      ++arg_num;
		    }
		  precision_wanted_type.wanted_type = *fki->precision_type;
		  precision_wanted_type.wanted_type_name = NULL;
		  precision_wanted_type.pointer_count = 0;
		  precision_wanted_type.char_lenient_flag = 0;
		  precision_wanted_type.writing_in_flag = 0;
		  precision_wanted_type.name = _("field precision");
		  precision_wanted_type.param = cur_param;
		  precision_wanted_type.arg_num = arg_num;
		  precision_wanted_type.next = NULL;
		  if (last_wanted_type != 0)
		    last_wanted_type->next = &precision_wanted_type;
		  if (first_wanted_type == 0)
		    first_wanted_type = &precision_wanted_type;
		  last_wanted_type = &precision_wanted_type;
		}
	    }
	  else
	    {
	      while (ISDIGIT (*format_chars))
		++format_chars;
	    }
	}

      /* Read any length modifier, if this kind of format has them.  */
      fli = fki->length_char_specs;
      length_chars = NULL;
      length_chars_val = FMT_LEN_none;
      length_chars_std = STD_C89;
      if (fli)
	{
	  while (fli->name != 0 && fli->name[0] != *format_chars)
	    fli++;
	  if (fli->name != 0)
	    {
	      format_chars++;
	      if (fli->double_name != 0 && fli->name[0] == *format_chars)
		{
		  format_chars++;
		  length_chars = fli->double_name;
		  length_chars_val = fli->double_index;
		  length_chars_std = fli->double_std;
		}
	      else
		{
		  length_chars = fli->name;
		  length_chars_val = fli->index;
		  length_chars_std = fli->std;
		}
	      i = strlen (flag_chars);
	      flag_chars[i++] = fki->length_code_char;
	      flag_chars[i] = 0;
	    }
	  if (pedantic)
	    {
	      /* Warn if the length modifier is non-standard.  */
	      if (length_chars_std > C_STD_VER)
		status_warning (status, "%s does not support the `%s' %s length modifier",
				C_STD_NAME (length_chars_std), length_chars,
				fki->name);
	    }
	}

      /* Read any modifier (strftime E/O).  */
      if (fki->modifier_chars != NULL)
	{
	  while (*format_chars != 0
		 && strchr (fki->modifier_chars, *format_chars) != 0)
	    {
	      if (strchr (flag_chars, *format_chars) != 0)
		{
		  const format_flag_spec *s = get_flag_spec (flag_specs,
							     *format_chars, NULL);
		  status_warning (status, "repeated %s in format", _(s->name));
		}
	      else
		{
		  i = strlen (flag_chars);
		  flag_chars[i++] = *format_chars;
		  flag_chars[i] = 0;
		}
	      ++format_chars;
	    }
	}

      /* Handle the scanf allocation kludge.  */
      if (fki->flags & FMT_FLAG_SCANF_A_KLUDGE)
	{
	  if (*format_chars == 'a' && !flag_isoc99)
	    {
	      if (format_chars[1] == 's' || format_chars[1] == 'S'
		  || format_chars[1] == '[')
		{
		  /* `a' is used as a flag.  */
		  i = strlen (flag_chars);
		  flag_chars[i++] = 'a';
		  flag_chars[i] = 0;
		  format_chars++;
		}
	    }
	}

      format_char = *format_chars;
      if (format_char == 0
	  || (!(fki->flags & FMT_FLAG_FANCY_PERCENT_OK) && format_char == '%'))
	{
	  status_warning (status, "conversion lacks type at end of format");
	  continue;
	}
      format_chars++;
      fci = fki->conversion_specs;
      while (fci->format_chars != 0
	     && strchr (fci->format_chars, format_char) == 0)
	  ++fci;
      if (fci->format_chars == 0)
	{
          if (ISGRAPH(format_char))
	    status_warning (status, "unknown conversion type character `%c' in format",
		     format_char);
	  else
	    status_warning (status, "unknown conversion type character 0x%x in format",
		     format_char);
	  continue;
	}
      if (pedantic)
	{
	  if (fci->std > C_STD_VER)
	    status_warning (status, "%s does not support the `%%%c' %s format",
			    C_STD_NAME (fci->std), format_char, fki->name);
	}

      /* Validate the individual flags used, removing any that are invalid.  */
      {
	int d = 0;
	for (i = 0; flag_chars[i] != 0; i++)
	  {
	    const format_flag_spec *s = get_flag_spec (flag_specs,
						       flag_chars[i], NULL);
	    flag_chars[i - d] = flag_chars[i];
	    if (flag_chars[i] == fki->length_code_char)
	      continue;
	    if (strchr (fci->flag_chars, flag_chars[i]) == 0)
	      {
		status_warning (status, "%s used with `%%%c' %s format",
				_(s->name), format_char, fki->name);
		d++;
		continue;
	      }
	    if (pedantic)
	      {
		const format_flag_spec *t;
		if (s->std > C_STD_VER)
		  status_warning (status, "%s does not support %s",
				  C_STD_NAME (s->std), _(s->long_name));
		t = get_flag_spec (flag_specs, flag_chars[i], fci->flags2);
		if (t != NULL && t->std > s->std)
		  {
		    const char *long_name = (t->long_name != NULL
					     ? t->long_name
					     : s->long_name);
		    if (t->std > C_STD_VER)
		      status_warning (status, "%s does not support %s with the `%%%c' %s format",
				      C_STD_NAME (t->std), _(long_name),
				      format_char, fki->name);
		  }
	      }
	  }
	flag_chars[i - d] = 0;
      }

      if ((fki->flags & FMT_FLAG_SCANF_A_KLUDGE)
	  && strchr (flag_chars, 'a') != 0)
	aflag = 1;

      if (fki->suppression_char
	  && strchr (flag_chars, fki->suppression_char) != 0)
	suppressed = 1;

      /* Validate the pairs of flags used.  */
      for (i = 0; bad_flag_pairs[i].flag_char1 != 0; i++)
	{
	  const format_flag_spec *s, *t;
	  if (strchr (flag_chars, bad_flag_pairs[i].flag_char1) == 0)
	    continue;
	  if (strchr (flag_chars, bad_flag_pairs[i].flag_char2) == 0)
	    continue;
	  if (bad_flag_pairs[i].predicate != 0
	      && strchr (fci->flags2, bad_flag_pairs[i].predicate) == 0)
	    continue;
	  s = get_flag_spec (flag_specs, bad_flag_pairs[i].flag_char1, NULL);
	  t = get_flag_spec (flag_specs, bad_flag_pairs[i].flag_char2, NULL);
	  if (bad_flag_pairs[i].ignored)
	    {
	      if (bad_flag_pairs[i].predicate != 0)
		status_warning (status, "%s ignored with %s and `%%%c' %s format",
				_(s->name), _(t->name), format_char,
				fki->name);
	      else
		status_warning (status, "%s ignored with %s in %s format",
				_(s->name), _(t->name), fki->name);
	    }
	  else
	    {
	      if (bad_flag_pairs[i].predicate != 0)
		status_warning (status, "use of %s and %s together with `%%%c' %s format",
				_(s->name), _(t->name), format_char,
				fki->name);
	      else
		status_warning (status, "use of %s and %s together in %s format",
				_(s->name), _(t->name), fki->name);
	    }
	}

      /* Give Y2K warnings.  */
      {
	int y2k_level = 0;
	if (strchr (fci->flags2, '4') != 0)
	  if (strchr (flag_chars, 'E') != 0)
	    y2k_level = 3;
	  else
	    y2k_level = 2;
	else if (strchr (fci->flags2, '3') != 0)
	  y2k_level = 3;
	else if (strchr (fci->flags2, '2') != 0)
	  y2k_level = 2;
	if (y2k_level == 3)
	  status_warning (status, "`%%%c' yields only last 2 digits of year in some locales",
			  format_char);
	else if (y2k_level == 2)
	  status_warning (status, "`%%%c' yields only last 2 digits of year", format_char);
      }

      if (strchr (fci->flags2, '[') != 0)
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
	    status_warning (status, "no closing `]' for `%%[' format");
	}

      wanted_type = 0;
      wanted_type_name = 0;
      if (fki->flags & FMT_FLAG_ARG_CONVERT)
	{
	  wanted_type = (fci->types[length_chars_val].type
			 ? *fci->types[length_chars_val].type : 0);
	  wanted_type_name = fci->types[length_chars_val].name;
	  wanted_type_std = fci->types[length_chars_val].std;
	  if (wanted_type == 0)
	    {
	      status_warning (status, "use of `%s' length modifier with `%c' type character",
			      length_chars, format_char);
	      /* Heuristic: skip one argument when an invalid length/type
		 combination is encountered.  */
	      arg_num++;
	      if (params == 0)
		{
		  status_warning (status, "too few arguments for format");
		  return;
		}
	      params = TREE_CHAIN (params);
	      continue;
	    }
	  else if (pedantic
		   /* Warn if non-standard, provided it is more non-standard
		      than the length and type characters that may already
		      have been warned for.  */
		   && wanted_type_std > length_chars_std
		   && wanted_type_std > fci->std)
	    {
	      if (wanted_type_std > C_STD_VER)
		status_warning (status, "%s does not support the `%%%s%c' %s format",
				C_STD_NAME (wanted_type_std), length_chars,
				format_char, fki->name);
	    }
	}

      /* Finally. . .check type of argument against desired type!  */
      if (info->first_arg_num == 0)
	continue;
      if ((fci->pointer_count == 0 && wanted_type == void_type_node)
	  || suppressed)
	{
	  if (main_arg_num != 0)
	    {
	      if (suppressed)
		status_warning (status, "operand number specified with suppressed assignment");
	      else
		status_warning (status, "operand number specified for format taking no argument");
	    }
	}
      else
	{
	  if (main_arg_num != 0)
	    {
	      arg_num = main_arg_num;
	      params = main_arg_params;
	    }
	  else
	    {
	      ++arg_num;
	      if (has_operand_number > 0)
		{
		  status_warning (status, "missing $ operand number in format");
		  return;
		}
	      else
		has_operand_number = 0;
	      if (params == 0)
		{
		  status_warning (status, "too few arguments for format");
		  return;
		}
	    }
	  cur_param = TREE_VALUE (params);
	  params = TREE_CHAIN (params);
	  main_wanted_type.wanted_type = wanted_type;
	  main_wanted_type.wanted_type_name = wanted_type_name;
	  main_wanted_type.pointer_count = fci->pointer_count + aflag;
	  main_wanted_type.char_lenient_flag = 0;
	  if (strchr (fci->flags2, 'c') != 0)
	    main_wanted_type.char_lenient_flag = 1;
	  main_wanted_type.writing_in_flag = 0;
	  if (strchr (fci->flags2, 'W') != 0)
	    main_wanted_type.writing_in_flag = 1;
	  main_wanted_type.name = NULL;
	  main_wanted_type.param = cur_param;
	  main_wanted_type.arg_num = arg_num;
	  main_wanted_type.next = NULL;
	  if (last_wanted_type != 0)
	    last_wanted_type->next = &main_wanted_type;
	  if (first_wanted_type == 0)
	    first_wanted_type = &main_wanted_type;
	  last_wanted_type = &main_wanted_type;
	}

      if (first_wanted_type != 0)
	check_format_types (status, first_wanted_type);

    }
}


/* Check the argument types from a single format conversion (possibly
   including width and precision arguments).  */
static void
check_format_types (status, types)
     int *status;
     format_wanted_type *types;
{
  for (; types != 0; types = types->next)
    {
      tree cur_param;
      tree cur_type;
      tree orig_cur_type;
      tree wanted_type;
      tree promoted_type;
      int arg_num;
      int i;
      int char_type_flag;
      cur_param = types->param;
      cur_type = TREE_TYPE (cur_param);
      if (cur_type == error_mark_node)
	continue;
      char_type_flag = 0;
      wanted_type = types->wanted_type;
      arg_num = types->arg_num;

      /* The following should not occur here.  */
      if (wanted_type == 0)
	abort ();
      if (wanted_type == void_type_node && types->pointer_count == 0)
	abort ();

      if (types->pointer_count == 0)
	{
	  promoted_type = simple_type_promotes_to (wanted_type);
	  if (promoted_type != NULL_TREE)
	    wanted_type = promoted_type;
	}

      STRIP_NOPS (cur_param);

      /* Check the types of any additional pointer arguments
	 that precede the "real" argument.  */
      for (i = 0; i < types->pointer_count; ++i)
	{
	  if (TREE_CODE (cur_type) == POINTER_TYPE)
	    {
	      cur_type = TREE_TYPE (cur_type);
	      if (cur_type == error_mark_node)
		break;

	      /* Check for writing through a NULL pointer.  */
	      if (types->writing_in_flag
		  && i == 0
		  && cur_param != 0
		  && integer_zerop (cur_param))
		status_warning (status,
				"writing through null pointer (arg %d)",
				arg_num);

	      if (cur_param != 0 && TREE_CODE (cur_param) == ADDR_EXPR)
		cur_param = TREE_OPERAND (cur_param, 0);
	      else
		cur_param = 0;

	      /* See if this is an attempt to write into a const type with
		 scanf or with printf "%n".  Note: the writing in happens
		 at the first indirection only, if for example
		 void * const * is passed to scanf %p; passing
		 const void ** is simply passing an incompatible type.  */
	      if (types->writing_in_flag
		  && i == 0
		  && (TYPE_READONLY (cur_type)
		      || (cur_param != 0
			  && (TREE_CODE_CLASS (TREE_CODE (cur_param)) == 'c'
			      || (DECL_P (cur_param)
				  && TREE_READONLY (cur_param))))))
		status_warning (status, "writing into constant object (arg %d)", arg_num);

	      /* If there are extra type qualifiers beyond the first
		 indirection, then this makes the types technically
		 incompatible.  */
	      if (i > 0
		  && pedantic
		  && (TYPE_READONLY (cur_type)
		      || TYPE_VOLATILE (cur_type)
		      || TYPE_RESTRICT (cur_type)))
		status_warning (status, "extra type qualifiers in format argument (arg %d)",
			 arg_num);

	    }
	  else
	    {
	      if (types->pointer_count == 1)
		status_warning (status, "format argument is not a pointer (arg %d)", arg_num);
	      else
		status_warning (status, "format argument is not a pointer to a pointer (arg %d)", arg_num);
	      break;
	    }
	}

      if (i < types->pointer_count)
	continue;

      orig_cur_type = cur_type;
      cur_type = TYPE_MAIN_VARIANT (cur_type);

      /* Check whether the argument type is a character type.  This leniency
	 only applies to certain formats, flagged with 'c'.
      */
      if (types->char_lenient_flag)
	char_type_flag = (cur_type == char_type_node
			  || cur_type == signed_char_type_node
			  || cur_type == unsigned_char_type_node);

      /* Check the type of the "real" argument, if there's a type we want.  */
      if (wanted_type == cur_type)
	continue;
      /* If we want `void *', allow any pointer type.
	 (Anything else would already have got a warning.)
	 With -pedantic, only allow pointers to void and to character
	 types.  */
      if (wanted_type == void_type_node
	  && (!pedantic || (i == 1 && char_type_flag)))
	continue;
      /* Don't warn about differences merely in signedness, unless
	 -pedantic.  With -pedantic, warn if the type is a pointer
	 target and not a character type, and for character types at
	 a second level of indirection.  */
      if (TREE_CODE (wanted_type) == INTEGER_TYPE
	  && TREE_CODE (cur_type) == INTEGER_TYPE
	  && (! pedantic || i == 0 || (i == 1 && char_type_flag))
	  && (TREE_UNSIGNED (wanted_type)
	      ? wanted_type == unsigned_type (cur_type)
	      : wanted_type == signed_type (cur_type)))
	continue;
      /* Likewise, "signed char", "unsigned char" and "char" are
	 equivalent but the above test won't consider them equivalent.  */
      if (wanted_type == char_type_node
	  && (! pedantic || i < 2)
	  && char_type_flag)
	continue;
      /* Now we have a type mismatch.  */
      {
	register const char *this;
	register const char *that;

	this = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (wanted_type)));
	that = 0;
	if (TYPE_NAME (orig_cur_type) != 0
	    && TREE_CODE (orig_cur_type) != INTEGER_TYPE
	    && !(TREE_CODE (orig_cur_type) == POINTER_TYPE
		 && TREE_CODE (TREE_TYPE (orig_cur_type)) == INTEGER_TYPE))
	  {
	    if (TREE_CODE (TYPE_NAME (orig_cur_type)) == TYPE_DECL
		&& DECL_NAME (TYPE_NAME (orig_cur_type)) != 0)
	      that = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (orig_cur_type)));
	    else
	      that = IDENTIFIER_POINTER (TYPE_NAME (orig_cur_type));
	  }

	/* A nameless type can't possibly match what the format wants.
	   So there will be a warning for it.
	   Make up a string to describe vaguely what it is.  */
	if (that == 0)
	  {
	    if (TREE_CODE (orig_cur_type) == POINTER_TYPE)
	      that = "pointer";
	    else
	      that = "different type";
	  }

	/* Make the warning better in case of mismatch of int vs long.  */
	if (TREE_CODE (orig_cur_type) == INTEGER_TYPE
	    && TREE_CODE (wanted_type) == INTEGER_TYPE
	    && TYPE_PRECISION (orig_cur_type) == TYPE_PRECISION (wanted_type)
	    && TYPE_NAME (orig_cur_type) != 0
	    && TREE_CODE (TYPE_NAME (orig_cur_type)) == TYPE_DECL)
	  that = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (orig_cur_type)));

	if (strcmp (this, that) != 0)
	  {
	    /* There may be a better name for the format, e.g. size_t,
	       but we should allow for programs with a perverse typedef
	       making size_t something other than what the compiler
	       thinks.  */
	    if (types->wanted_type_name != 0
		&& strcmp (types->wanted_type_name, that) != 0)
	      this = types->wanted_type_name;
	    if (types->name != 0)
	      status_warning (status, "%s is not type %s (arg %d)", types->name, this,
		       arg_num);
	    else
	      status_warning (status, "%s format, %s arg (arg %d)", this, that, arg_num);
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

/* Describe a reversed version of a normal tree, so that we can get to the
   parent of each node.  */
struct reverse_tree
{
  /* All reverse_tree structures for a given tree are chained through this
     field.  */
  struct reverse_tree *next;
  /* The parent of this node.  */
  struct reverse_tree *parent;
  /* The actual tree node.  */
  tree x;
  /* The operand number this node corresponds to in the parent.  */
  int operandno;
  /* Describe whether this expression is written to or read.  */
  char read, write;
};

/* A list of all reverse_tree structures for a given expression, built by
   build_reverse_tree.  */
static struct reverse_tree *reverse_list;
/* The maximum depth of a tree, computed by build_reverse_tree.  */
static int reverse_max_depth;

static void build_reverse_tree PARAMS ((tree, struct reverse_tree *, int, int,
					int, int));
static struct reverse_tree *common_ancestor PARAMS ((struct reverse_tree *,
						     struct reverse_tree *,
						     struct reverse_tree **,
						     struct reverse_tree **));
static int modify_ok PARAMS ((struct reverse_tree *, struct reverse_tree *));
static void verify_sequence_points PARAMS ((tree));

/* Recursively process an expression, X, building a reverse tree while
   descending and recording OPERANDNO, READ, and WRITE in the created
   structures.  DEPTH is used to compute reverse_max_depth.
   FIXME: if walk_tree gets moved out of the C++ front end, this should
   probably use walk_tree.  */

static void
build_reverse_tree (x, parent, operandno, read, write, depth)
     tree x;
     struct reverse_tree *parent;
     int operandno, read, write, depth;
{
  struct reverse_tree *node;

  if (x == 0 || x == error_mark_node)
    return;

  node = (struct reverse_tree *) xmalloc (sizeof (struct reverse_tree));

  node->parent = parent;
  node->x = x;
  node->read = read;
  node->write = write;
  node->operandno = operandno;
  node->next = reverse_list;
  reverse_list = node;
  if (depth > reverse_max_depth)
    reverse_max_depth = depth;

  switch (TREE_CODE (x))
    {
    case PREDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
      build_reverse_tree (TREE_OPERAND (x, 0), node, 0, 1, 1, depth + 1);
      break;

    case CALL_EXPR:
      build_reverse_tree (TREE_OPERAND (x, 0), node, 0, 1, 0, depth + 1);
      x = TREE_OPERAND (x, 1);
      while (x)
	{
	  build_reverse_tree (TREE_VALUE (x), node, 1, 1, 0, depth + 1);
	  x = TREE_CHAIN (x);
	}
      break;

    case TREE_LIST:
      /* Scan all the list, e.g. indices of multi dimensional array.  */
      while (x)
	{
	  build_reverse_tree (TREE_VALUE (x), node, 0, 1, 0, depth + 1);
	  x = TREE_CHAIN (x);
	}
      break;

    case MODIFY_EXPR:
      build_reverse_tree (TREE_OPERAND (x, 0), node, 0, 0, 1, depth + 1);
      build_reverse_tree (TREE_OPERAND (x, 1), node, 1, 1, 0, depth + 1);
      break;

    default:
      switch (TREE_CODE_CLASS (TREE_CODE (x)))
	{
	case 'r':
	case '<':
	case '2':
	case 'b':
	case '1':
	case 'e':
	case 's':
	case 'x':
	  {
	    int lp;
	    int max = first_rtl_op (TREE_CODE (x));
	    for (lp = 0; lp < max; lp++)
	      build_reverse_tree (TREE_OPERAND (x, lp), node, lp, 1, 0,
				  depth + 1);
	    break;
	  }
	default:
	  break;
	}
      break;
    }
}

/* Given nodes P1 and P2 as well as enough scratch space pointed to by TMP1
   and TMP2, find the common ancestor of P1 and P2.  */

static struct reverse_tree *
common_ancestor (p1, p2, tmp1, tmp2)
     struct reverse_tree *p1, *p2;
     struct reverse_tree **tmp1, **tmp2;
{
  struct reverse_tree *t1 = p1;
  struct reverse_tree *t2 = p2;
  int i, j;

  /* First, check if we're actually looking at the same expression twice,
     which can happen if it's wrapped in a SAVE_EXPR - in this case there's
     no chance of conflict.  */
  while (t1 && t2 && t1->x == t2->x)
    {
      if (TREE_CODE (t1->x) == SAVE_EXPR)
	return 0;
      t1 = t1->parent;
      t2 = t2->parent;
    }

  for (i = 0; p1; i++, p1 = p1->parent)
    tmp1[i] = p1;
  for (j = 0; p2; j++, p2 = p2->parent)
    tmp2[j] = p2;
  while (tmp1[i - 1] == tmp2[j - 1])
    i--, j--;

  return tmp1[i];
}

/* Subroutine of verify_sequence_points to check whether a node T corresponding
   to a MODIFY_EXPR invokes undefined behaviour.  OTHER occurs somewhere in the
   RHS, and an identical expression is the LHS of T.
   For MODIFY_EXPRs, some special cases apply when testing for undefined
   behaviour if one of the expressions we found is the LHS of the MODIFY_EXPR.
   If the other expression is just a use, then there's no undefined behaviour.
   Likewise, if the other expression is wrapped inside another expression that
   will force a sequence point, then there's no undefined behaviour either.  */

static int
modify_ok (t, other)
     struct reverse_tree *t, *other;
{
  struct reverse_tree *p;

  if (! other->write)
    return 1;

  /* See if there's an intervening sequence point.  */
  for (p = other; p->parent != t; p = p->parent)
    {
      if ((TREE_CODE (p->parent->x) == COMPOUND_EXPR
	   || TREE_CODE (p->parent->x) == TRUTH_ANDIF_EXPR
	   || TREE_CODE (p->parent->x) == TRUTH_ORIF_EXPR
	   || TREE_CODE (p->parent->x) == COND_EXPR)
	  && p->operandno == 0)
	return 1;
      if (TREE_CODE (p->parent->x) == SAVE_EXPR)
	return 1;
      if (TREE_CODE (p->parent->x) == CALL_EXPR
	  && p->operandno != 0)
	return 1;
    }
  return 0;
}

/* Try to warn for undefined behaviour in EXPR due to missing sequence
   points.  */

static void
verify_sequence_points (expr)
     tree expr;
{
  struct reverse_tree **tmp1, **tmp2;
  struct reverse_tree *p;

  reverse_list = 0;
  reverse_max_depth = 0;
  build_reverse_tree (expr, NULL, 0, 1, 0, 1);

  tmp1 = (struct reverse_tree **) xmalloc (sizeof (struct reverse_tree *)
					   * reverse_max_depth);
  tmp2 = (struct reverse_tree **) xmalloc (sizeof (struct reverse_tree *)
					   * reverse_max_depth);

  /* Search for multiple occurrences of the same variable, where either both
     occurrences are writes, or one is a read and a write.  If we can't prove
     that these are ordered by a sequence point, warn that the expression is
     undefined.  */
  for (p = reverse_list; p; p = p->next)
    {
      struct reverse_tree *p2;
      if (TREE_CODE (p->x) != VAR_DECL && TREE_CODE (p->x) != PARM_DECL)
	continue;
      for (p2 = p->next; p2; p2 = p2->next)
	{
	  if ((TREE_CODE (p2->x) == VAR_DECL || TREE_CODE (p2->x) == PARM_DECL)
	      && DECL_NAME (p->x) == DECL_NAME (p2->x)
	      && (p->write || p2->write))
	    {
	      struct reverse_tree *t = common_ancestor (p, p2, tmp1, tmp2);

	      if (t == 0
		  || TREE_CODE (t->x) == COMPOUND_EXPR
		  || TREE_CODE (t->x) == TRUTH_ANDIF_EXPR
		  || TREE_CODE (t->x) == TRUTH_ORIF_EXPR
		  || TREE_CODE (t->x) == COND_EXPR)
		continue;
	      if (TREE_CODE (t->x) == MODIFY_EXPR
		  && p->parent == t
		  && modify_ok (t, p2))
		continue;
	      if (TREE_CODE (t->x) == MODIFY_EXPR
		  && p2->parent == t
		  && modify_ok (t, p))
		continue;

	      warning ("operation on `%s' may be undefined",
		       IDENTIFIER_POINTER (DECL_NAME (p->x)));
	      break;
	    }
	}
    }

  while (reverse_list)
    {
      struct reverse_tree *p = reverse_list;
      reverse_list = p->next;
      free (p);
    }
  free (tmp1);
  free (tmp2);
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

  if (warn_sequence_point)
    verify_sequence_points (expr);

  if (TREE_TYPE (expr) != error_mark_node
      && !COMPLETE_OR_VOID_TYPE_P (TREE_TYPE (expr))
      && TREE_CODE (TREE_TYPE (expr)) != ARRAY_TYPE)
    error ("expression statement has incomplete type");

  last_expr_type = TREE_TYPE (expr); 
  add_stmt (build_stmt (EXPR_STMT, expr));
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
  /* In C++, the following is allowed:

       const int i = 3;
       switch (...) { case i: ... }

     So, we try to reduce the VALUE to a constant that way.  */
  if (c_language == clk_cplusplus)
    {
      value = decl_constant_value (value);
      STRIP_TYPE_NOPS (value);
      value = fold (value);
    }

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

#ifdef VECTOR_MODE_SUPPORTED_P
  if (mode == TYPE_MODE (V4SF_type_node) && VECTOR_MODE_SUPPORTED_P (mode))
    return V4SF_type_node;
  if (mode == TYPE_MODE (V4SI_type_node) && VECTOR_MODE_SUPPORTED_P (mode))
    return V4SI_type_node;
  if (mode == TYPE_MODE (V2SI_type_node) && VECTOR_MODE_SUPPORTED_P (mode))
    return V2SI_type_node;
  if (mode == TYPE_MODE (V4HI_type_node) && VECTOR_MODE_SUPPORTED_P (mode))
    return V4HI_type_node;
  if (mode == TYPE_MODE (V8QI_type_node) && VECTOR_MODE_SUPPORTED_P (mode))
    return V8QI_type_node;
#endif

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
	      if (extra_warnings && !in_system_header
		  && ! (TREE_CODE (primop0) == INTEGER_CST
			&& ! TREE_OVERFLOW (convert (signed_type (type),
						     primop0))))
		warning ("comparison of unsigned expression >= 0 is always true");
	      value = boolean_true_node;
	      break;

	    case LT_EXPR:
	      if (extra_warnings && !in_system_header
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

#if !USE_CPPLIB
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
#endif /* USE_CPPLIB */

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
  if ((type_quals & TYPE_QUAL_CONST)
      || (TREE_TYPE (decl) 
	  && TREE_CODE (TREE_TYPE (decl)) == REFERENCE_TYPE))
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

	  HOST_WIDE_INT pointed_to_alias_set
	    = get_alias_set (TREE_TYPE (TREE_TYPE (decl)));

	  if (pointed_to_alias_set == 0)
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


/* Return the typed-based alias set for T, which may be an expression
   or a type.  Return -1 if we don't do anything special.  */

HOST_WIDE_INT
lang_get_alias_set (t)
     tree t;
{
  tree u;

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

  /* If this is a char *, the ANSI C standard says it can alias
     anything.  Note that all references need do this.  */
  if (TREE_CODE_CLASS (TREE_CODE (t)) == 'r'
      && TREE_CODE (TREE_TYPE (t)) == INTEGER_TYPE
      && TYPE_PRECISION (TREE_TYPE (t)) == TYPE_PRECISION (char_type_node))
    return 0;

  /* That's all the expressions we handle specially.  */
  if (! TYPE_P (t))
    return -1;

  /* The C standard specifically allows aliasing between signed and
     unsigned variants of the same type.  We treat the signed
     variant as canonical.  */
  if (TREE_CODE (t) == INTEGER_TYPE && TREE_UNSIGNED (t))
    {
      tree t1 = signed_type (t);

      /* t1 == t can happen for boolean nodes which are always unsigned.  */
      if (t1 != t)
	return get_alias_set (t1);
    }
  else if (POINTER_TYPE_P (t))
    {
      tree t1;

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
      t1 = TYPE_MAIN_VARIANT (TREE_TYPE (t));
      t1 = ((TREE_CODE (t) == POINTER_TYPE)
	   ? build_pointer_type (t1) : build_reference_type (t1));
      if (t1 != t)
	return get_alias_set (t1);
    }
  /* It's not yet safe to use alias sets for classes in C++ because
     the TYPE_FIELDs list for a class doesn't mention base classes.  */
  else if (c_language == clk_cplusplus && AGGREGATE_TYPE_P (t))
    return 0;

  return -1;
}

/* Build tree nodes and builtin functions common to both C and C++ language
   frontends.  */

void
c_common_nodes_and_builtins ()
{
  tree temp;
  tree memcpy_ftype, memset_ftype, strlen_ftype;
  tree bzero_ftype, bcmp_ftype, puts_ftype, printf_ftype;
  tree endlink, int_endlink, double_endlink, unsigned_endlink;
  tree sizetype_endlink;
  tree ptr_ftype, ptr_ftype_unsigned;
  tree void_ftype_any, void_ftype_int, int_ftype_any, sizet_ftype_any;
  tree double_ftype_double, double_ftype_double_double;
  tree float_ftype_float, ldouble_ftype_ldouble;
  tree int_ftype_cptr_cptr_sizet;
  tree int_ftype_string_string, string_ftype_ptr_ptr;
  tree string_ftype_string_int, string_ftype_string_string;
  tree long_ftype_long;
  tree longlong_ftype_longlong;
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

  sizet_ftype_any = build_function_type (sizetype, NULL_TREE);
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

  longlong_ftype_longlong
    = build_function_type (long_long_integer_type_node,
			   tree_cons (NULL_TREE, long_long_integer_type_node,
				      endlink));

  int_ftype_cptr_cptr_sizet
    = build_function_type (integer_type_node,
			   tree_cons (NULL_TREE, const_ptr_type_node,
				      tree_cons (NULL_TREE, const_ptr_type_node,
						 tree_cons (NULL_TREE,
							    sizetype,
							    endlink))));

  void_zero_node = build_int_2 (0, 0);
  TREE_TYPE (void_zero_node) = void_type_node;

  /* Prototype for strcpy.  */
  string_ftype_ptr_ptr
    = build_function_type (string_type_node,
			   tree_cons (NULL_TREE, string_type_node,
				      tree_cons (NULL_TREE,
						 const_string_type_node,
						 endlink)));

  traditional_len_type_node = ((flag_traditional && 
				c_language != clk_cplusplus)
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

  /* Prototype for strstr, strpbrk, etc.  */
  string_ftype_string_string
    = build_function_type (string_type_node,
			   tree_cons (NULL_TREE, const_string_type_node,
				      tree_cons (NULL_TREE,
						 const_string_type_node,
						 endlink)));

  /* Prototype for strchr.  */
  string_ftype_string_int
    = build_function_type (string_type_node,
			   tree_cons (NULL_TREE, const_string_type_node,
				      tree_cons (NULL_TREE,
						 integer_type_node,
						 endlink)));

  /* Prototype for strlen.  */
  strlen_ftype
    = build_function_type (traditional_len_type_node,
			   tree_cons (NULL_TREE, const_string_type_node,
				      endlink));

  traditional_ptr_type_node = ((flag_traditional && 
				c_language != clk_cplusplus)
			       ? string_type_node : ptr_type_node);
  traditional_cptr_type_node = ((flag_traditional && 
				 c_language != clk_cplusplus)
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

  /* Prototype for puts.  */
  puts_ftype
    = build_function_type (integer_type_node,
			   tree_cons (NULL_TREE, const_string_type_node,
				      endlink));

  /* Prototype for printf.  */
  printf_ftype
    = build_function_type (integer_type_node,
			   tree_cons (NULL_TREE, const_string_type_node,
				      NULL_TREE));

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
  if (! flag_no_builtin && ! flag_no_nonansi_builtin)
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
  builtin_function ("__builtin_llabs", longlong_ftype_longlong, BUILT_IN_LLABS,
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
  builtin_function ("__builtin_strstr", string_ftype_string_string,
		    BUILT_IN_STRSTR, BUILT_IN_NORMAL, "strstr");
  builtin_function ("__builtin_strpbrk", string_ftype_string_string,
		    BUILT_IN_STRPBRK, BUILT_IN_NORMAL, "strpbrk");
  built_in_decls[BUILT_IN_STRCHR] =
    builtin_function ("__builtin_strchr", string_ftype_string_int,
		    BUILT_IN_STRCHR, BUILT_IN_NORMAL, "strchr");
  builtin_function ("__builtin_strrchr", string_ftype_string_int,
		    BUILT_IN_STRRCHR, BUILT_IN_NORMAL, "strrchr");
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
  built_in_decls[BUILT_IN_PUTCHAR] =
    builtin_function ("__builtin_putchar", int_ftype_int,
		      BUILT_IN_PUTCHAR, BUILT_IN_NORMAL, "putchar");
  built_in_decls[BUILT_IN_PUTS] =
    builtin_function ("__builtin_puts", puts_ftype,
		      BUILT_IN_PUTS, BUILT_IN_NORMAL, "puts");
  builtin_function ("__builtin_printf", printf_ftype,
		    BUILT_IN_PRINTF, BUILT_IN_FRONTEND, "printf");
  /* We declare these without argument so that the initial declaration
     for these identifiers is a builtin.  That allows us to redeclare
     them later with argument without worrying about the explicit
     declarations in stdio.h being taken as the initial declaration.
     Also, save the _DECL for these so we can use them later.  */
  built_in_decls[BUILT_IN_FWRITE] =
    builtin_function ("__builtin_fwrite", sizet_ftype_any,
		      BUILT_IN_FWRITE, BUILT_IN_NORMAL, "fwrite");
  built_in_decls[BUILT_IN_FPUTC] =
    builtin_function ("__builtin_fputc", int_ftype_any,
		      BUILT_IN_FPUTC, BUILT_IN_NORMAL, "fputc");
  built_in_decls[BUILT_IN_FPUTS] =
    builtin_function ("__builtin_fputs", int_ftype_any,
		      BUILT_IN_FPUTS, BUILT_IN_NORMAL, "fputs");

  if (! flag_no_builtin)
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
      if (flag_isoc99 || ! flag_no_nonansi_builtin)
	builtin_function ("llabs", longlong_ftype_longlong, BUILT_IN_LLABS,
			  BUILT_IN_NORMAL, NULL_PTR);
      builtin_function ("memcpy", memcpy_ftype, BUILT_IN_MEMCPY,
			BUILT_IN_NORMAL, NULL_PTR);
      builtin_function ("memcmp", int_ftype_cptr_cptr_sizet, BUILT_IN_MEMCMP,
			BUILT_IN_NORMAL, NULL_PTR);
      builtin_function ("memset", memset_ftype, BUILT_IN_MEMSET,
			BUILT_IN_NORMAL, NULL_PTR);
      builtin_function ("strcmp", int_ftype_string_string, BUILT_IN_STRCMP,
			BUILT_IN_NORMAL, NULL_PTR);
      builtin_function ("strstr", string_ftype_string_string, BUILT_IN_STRSTR,
			BUILT_IN_NORMAL, NULL_PTR);
      builtin_function ("strchr", string_ftype_string_int, BUILT_IN_STRCHR,
			BUILT_IN_NORMAL, NULL_PTR);
      builtin_function ("strrchr", string_ftype_string_int, BUILT_IN_STRRCHR,
			BUILT_IN_NORMAL, NULL_PTR);
      builtin_function ("strpbrk", string_ftype_string_string, BUILT_IN_STRPBRK,
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
      builtin_function ("printf", printf_ftype, BUILT_IN_PRINTF,
			BUILT_IN_FRONTEND, NULL_PTR);
      /* We declare these without argument so that the initial
         declaration for these identifiers is a builtin.  That allows
         us to redeclare them later with argument without worrying
         about the explicit declarations in stdio.h being taken as the
         initial declaration.  */
      builtin_function ("fputc", int_ftype_any, BUILT_IN_FPUTC,
			BUILT_IN_NORMAL, NULL_PTR);
      builtin_function ("fputs", int_ftype_any, BUILT_IN_FPUTS,
			BUILT_IN_NORMAL, NULL_PTR);

      /* Declare these functions volatile
	 to avoid spurious "control drops through" warnings.  */
      temp = builtin_function ("abort", 
			       ((c_language == clk_cplusplus)
				? void_ftype : void_ftype_any),
			       0, NOT_BUILT_IN, NULL_PTR);
      TREE_THIS_VOLATILE (temp) = 1;
      TREE_SIDE_EFFECTS (temp) = 1;

      temp = builtin_function ("exit",
			       ((c_language == clk_cplusplus)
				? void_ftype_int : void_ftype_any),
			       0, NOT_BUILT_IN, NULL_PTR);
      TREE_THIS_VOLATILE (temp) = 1;
      TREE_SIDE_EFFECTS (temp) = 1;
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

  main_identifier_node = get_identifier ("main");

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
    case BUILT_IN_LLABS:
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

/* Returns non-zero if CODE is the code for a statement.  */

int
statement_code_p (code)
     enum tree_code code;
{
  switch (code)
    {
    case EXPR_STMT:
    case COMPOUND_STMT:
    case DECL_STMT:
    case IF_STMT:
    case FOR_STMT:
    case WHILE_STMT:
    case DO_STMT:
    case RETURN_STMT:
    case BREAK_STMT:
    case CONTINUE_STMT:
    case SCOPE_STMT:
    case SWITCH_STMT:
    case GOTO_STMT:
    case LABEL_STMT:
    case ASM_STMT:
    case CASE_LABEL:
      return 1;

    default:
      if (lang_statement_code_p)
	return (*lang_statement_code_p) (code);
      return 0;
    }
}

/* Walk the statemen tree, rooted at *tp.  Apply FUNC to all the
   sub-trees of *TP in a pre-order traversal.  FUNC is called with the
   DATA and the address of each sub-tree.  If FUNC returns a non-NULL
   value, the traversal is aborted, and the value returned by FUNC is
   returned.  If FUNC sets WALK_SUBTREES to zero, then the subtrees of
   the node being visited are not walked.

   We don't need a without_duplicates variant of this one because the
   statement tree is a tree, not a graph.  */

tree 
walk_stmt_tree (tp, func, data)
     tree *tp;
     walk_tree_fn func;
     void *data;
{
  enum tree_code code;
  int walk_subtrees;
  tree result;
  int i, len;

#define WALK_SUBTREE(NODE)				\
  do							\
    {							\
      result = walk_stmt_tree (&(NODE), func, data);	\
      if (result)					\
	return result;					\
    }							\
  while (0)

  /* Skip empty subtrees.  */
  if (!*tp)
    return NULL_TREE;

  /* Skip subtrees below non-statement nodes.  */
  if (!statement_code_p (TREE_CODE (*tp)))
    return NULL_TREE;

  /* Call the function.  */
  walk_subtrees = 1;
  result = (*func) (tp, &walk_subtrees, data);

  /* If we found something, return it.  */
  if (result)
    return result;

  /* Even if we didn't, FUNC may have decided that there was nothing
     interesting below this point in the tree.  */
  if (!walk_subtrees)
    return NULL_TREE;

  /* FUNC may have modified the tree, recheck that we're looking at a
     statement node.  */
  code = TREE_CODE (*tp);
  if (!statement_code_p (code))
    return NULL_TREE;

  /* Walk over all the sub-trees of this operand.  Statement nodes never
     contain RTL, and we needn't worry about TARGET_EXPRs.  */
  len = TREE_CODE_LENGTH (code);

  /* Go through the subtrees.  We need to do this in forward order so
     that the scope of a FOR_EXPR is handled properly.  */
  for (i = 0; i < len; ++i)
    WALK_SUBTREE (TREE_OPERAND (*tp, i));

  /* Finally visit the chain.  This can be tail-recursion optimized if
     we write it this way.  */
  return walk_stmt_tree (&TREE_CHAIN (*tp), func, data);

#undef WALK_SUBTREE
}

/* Used to compare case labels.  K1 and K2 are actually tree nodes
   representing case labels, or NULL_TREE for a `default' label.
   Returns -1 if K1 is ordered before K2, -1 if K1 is ordered after
   K2, and 0 if K1 and K2 are equal.  */

int
case_compare (k1, k2)
     splay_tree_key k1;
     splay_tree_key k2;
{
  /* Consider a NULL key (such as arises with a `default' label) to be
     smaller than anything else.  */
  if (!k1)
    return k2 ? -1 : 0;
  else if (!k2)
    return k1 ? 1 : 0;

  return tree_int_cst_compare ((tree) k1, (tree) k2);
}

/* Process a case label for the range LOW_VALUE ... HIGH_VALUE.  If
   LOW_VALUE and HIGH_VALUE are both NULL_TREE then this case label is
   actually a `default' label.  If only HIGH_VALUE is NULL_TREE, then
   case label was declared using the usual C/C++ syntax, rather than
   the GNU case range extension.  CASES is a tree containing all the
   case ranges processed so far; COND is the condition for the
   switch-statement itself.  Returns the CASE_LABEL created, or
   ERROR_MARK_NODE if no CASE_LABEL is created.  */

tree
c_add_case_label (cases, cond, low_value, high_value)
     splay_tree cases;
     tree cond;
     tree low_value;
     tree high_value;
{
  tree type;
  tree label;
  tree case_label;
  splay_tree_node node;

  /* Create the LABEL_DECL itself.  */
  label = build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);
  DECL_CONTEXT (label) = current_function_decl;

  /* If there was an error processing the switch condition, bail now
     before we get more confused.  */
  if (!cond || cond == error_mark_node)
    {
      /* Add a label anyhow so that the back-end doesn't think that
	 the beginning of the switch is unreachable.  */
      if (!cases->root)
	add_stmt (build_case_label (NULL_TREE, NULL_TREE, label));
      return error_mark_node;
    }

  if ((low_value && TREE_TYPE (low_value) 
       && POINTER_TYPE_P (TREE_TYPE (low_value))) 
      || (high_value && TREE_TYPE (high_value)
	  && POINTER_TYPE_P (TREE_TYPE (high_value))))
    error ("pointers are not permitted as case values");

  /* Case ranges are a GNU extension.  */
  if (high_value && pedantic)
    {
      if (c_language == clk_cplusplus)
	pedwarn ("ISO C++ forbids range expressions in switch statements");
      else
	pedwarn ("ISO C forbids range expressions in switch statements");
    }

  type = TREE_TYPE (cond);
  if (low_value)
    {
      low_value = check_case_value (low_value);
      low_value = convert_and_check (type, low_value);
    }
  if (high_value)
    {
      high_value = check_case_value (high_value);
      high_value = convert_and_check (type, high_value);
    }

  /* If an error has occurred, bail out now.  */
  if (low_value == error_mark_node || high_value == error_mark_node)
    {
      if (!cases->root)
	add_stmt (build_case_label (NULL_TREE, NULL_TREE, label));
      return error_mark_node;
    }

  /* If the LOW_VALUE and HIGH_VALUE are the same, then this isn't
     really a case range, even though it was written that way.  Remove
     the HIGH_VALUE to simplify later processing.  */
  if (tree_int_cst_equal (low_value, high_value))
    high_value = NULL_TREE;
  if (low_value && high_value 
      && !tree_int_cst_lt (low_value, high_value)) 
    warning ("empty range specified");

  /* Look up the LOW_VALUE in the table of case labels we already
     have.  */
  node = splay_tree_lookup (cases, (splay_tree_key) low_value);
  /* If there was not an exact match, check for overlapping ranges.
     There's no need to do this if there's no LOW_VALUE or HIGH_VALUE;
     that's a `default' label and the only overlap is an exact match.  */
  if (!node && (low_value || high_value))
    {
      splay_tree_node low_bound;
      splay_tree_node high_bound;

      /* Even though there wasn't an exact match, there might be an
	 overlap between this case range and another case range.
	 Since we've (inductively) not allowed any overlapping case
	 ranges, we simply need to find the greatest low case label
	 that is smaller that LOW_VALUE, and the smallest low case
	 label that is greater than LOW_VALUE.  If there is an overlap
	 it will occur in one of these two ranges.  */
      low_bound = splay_tree_predecessor (cases,
					  (splay_tree_key) low_value);
      high_bound = splay_tree_successor (cases,
					 (splay_tree_key) low_value);

      /* Check to see if the LOW_BOUND overlaps.  It is smaller than
	 the LOW_VALUE, so there is no need to check unless the
	 LOW_BOUND is in fact itself a case range.  */
      if (low_bound
	  && CASE_HIGH ((tree) low_bound->value)
	  && tree_int_cst_compare (CASE_HIGH ((tree) low_bound->value),
				    low_value) >= 0)
	node = low_bound;
      /* Check to see if the HIGH_BOUND overlaps.  The low end of that
	 range is bigger than the low end of the current range, so we
	 are only interested if the current range is a real range, and
	 not an ordinary case label.  */
      else if (high_bound 
	       && high_value
	       && (tree_int_cst_compare ((tree) high_bound->key,
					 high_value)
		   <= 0))
	node = high_bound;
    }
  /* If there was an overlap, issue an error.  */
  if (node)
    {
      tree duplicate = CASE_LABEL_DECL ((tree) node->value);

      if (high_value)
	{
	  error ("duplicate (or overlapping) case value");
	  error_with_decl (duplicate, 
			   "this is the first entry overlapping that value");
	}
      else if (low_value)
	{
	  error ("duplicate case value") ;
	  error_with_decl (duplicate, "previously used here");
	}
      else
	{
	  error ("multiple default labels in one switch");
	  error_with_decl (duplicate, "this is the first default label");
	}
      if (!cases->root)
	add_stmt (build_case_label (NULL_TREE, NULL_TREE, label));
    }

  /* Add a CASE_LABEL to the statement-tree.  */
  case_label = add_stmt (build_case_label (low_value, high_value, label));
  /* Register this case label in the splay tree.  */
  splay_tree_insert (cases, 
		     (splay_tree_key) low_value,
		     (splay_tree_value) case_label);

  return case_label;
}

/* Mark P (a stmt_tree) for GC.  The use of a `void *' for the
   parameter allows this function to be used as a GC-marking
   function.  */

void
mark_stmt_tree (p)
     void *p;
{
  stmt_tree st = (stmt_tree) p;

  ggc_mark_tree (st->x_last_stmt);
  ggc_mark_tree (st->x_last_expr_type);
}

/* Mark LD for GC.  */

void
c_mark_lang_decl (c)
     struct c_lang_decl *c;
{
  ggc_mark_tree (c->saved_tree);
}

/* Mark F for GC.  */

void
mark_c_language_function (f)
     struct language_function *f;
{
  if (!f)
    return;

  mark_stmt_tree (&f->x_stmt_tree);
  ggc_mark_tree (f->x_scope_stmt_stack);
}

/* Hook used by expand_expr to expand language-specific tree codes.  */

rtx
c_expand_expr (exp, target, tmode, modifier)
     tree exp;
     rtx target;
     enum machine_mode tmode;
     enum expand_modifier modifier;
{
  switch (TREE_CODE (exp))
    {
    case STMT_EXPR:
      {
	tree rtl_expr;
	rtx result;

	/* Since expand_expr_stmt calls free_temp_slots after every
	   expression statement, we must call push_temp_slots here.
	   Otherwise, any temporaries in use now would be considered
	   out-of-scope after the first EXPR_STMT from within the
	   STMT_EXPR.  */
	push_temp_slots ();
	rtl_expr = expand_start_stmt_expr ();
	expand_stmt (STMT_EXPR_STMT (exp));
	expand_end_stmt_expr (rtl_expr);
	result = expand_expr (rtl_expr, target, tmode, modifier);
	pop_temp_slots ();
	return result;
      }
      break;
      
    case CALL_EXPR:
      {
	if (TREE_CODE (TREE_OPERAND (exp, 0)) == ADDR_EXPR
	    && (TREE_CODE (TREE_OPERAND (TREE_OPERAND (exp, 0), 0))
		== FUNCTION_DECL)
	    && DECL_BUILT_IN (TREE_OPERAND (TREE_OPERAND (exp, 0), 0))
	    && (DECL_BUILT_IN_CLASS (TREE_OPERAND (TREE_OPERAND (exp, 0), 0))
		== BUILT_IN_FRONTEND))
	  return c_expand_builtin (exp, target, tmode, modifier);
	else
	  abort();
      }
      break;

    default:
      abort ();
    }

  abort ();
  return NULL;
}

/* Hook used by safe_from_p to handle language-specific tree codes.  */

int
c_safe_from_p (target, exp)
     rtx target;
     tree exp;
{
  /* We can see statements here when processing the body of a
     statement-expression.  For a declaration statement declaring a
     variable, look at the variable's initializer.  */
  if (TREE_CODE (exp) == DECL_STMT) 
    {
      tree decl = DECL_STMT_DECL (exp);

      if (TREE_CODE (decl) == VAR_DECL
	  && DECL_INITIAL (decl)
	  && !safe_from_p (target, DECL_INITIAL (decl), /*top_p=*/0))
	return 0;
    }

  /* For any statement, we must follow the statement-chain.  */
  if (statement_code_p (TREE_CODE (exp)) && TREE_CHAIN (exp))
    return safe_from_p (target, TREE_CHAIN (exp), /*top_p=*/0);

  /* Assume everything else is safe.  */
  return 1;
}

/* Tree code classes. */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) TYPE,

static char c_tree_code_type[] = {
  'x',
#include "c-common.def"
};
#undef DEFTREECODE

/* Table indexed by tree code giving number of expression
   operands beyond the fixed part of the node structure.
   Not used for types or decls.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) LENGTH,

static int c_tree_code_length[] = {
  0,
#include "c-common.def"
};
#undef DEFTREECODE

/* Names of tree components.
   Used for printing out the tree and error messages.  */
#define DEFTREECODE(SYM, NAME, TYPE, LEN) NAME,

static const char *c_tree_code_name[] = {
  "@@dummy",
#include "c-common.def"
};
#undef DEFTREECODE

/* Adds the tree codes specific to the C front end to the list of all
   tree codes. */

void
add_c_tree_codes ()
{
  memcpy (tree_code_type + (int) LAST_AND_UNUSED_TREE_CODE,
	  c_tree_code_type,
	  (int)LAST_C_TREE_CODE - (int)LAST_AND_UNUSED_TREE_CODE);
  memcpy (tree_code_length + (int) LAST_AND_UNUSED_TREE_CODE,
	  c_tree_code_length,
	  (LAST_C_TREE_CODE - (int)LAST_AND_UNUSED_TREE_CODE) * sizeof (int));
  memcpy (tree_code_name + (int) LAST_AND_UNUSED_TREE_CODE,
	  c_tree_code_name,
	  (LAST_C_TREE_CODE - (int)LAST_AND_UNUSED_TREE_CODE) * sizeof (char *));
}

#define CALLED_AS_BUILT_IN(NODE) \
   (!strncmp (IDENTIFIER_POINTER (DECL_NAME (NODE)), "__builtin_", 10))

static rtx
c_expand_builtin (exp, target, tmode, modifier)
     tree exp;
     rtx target;
     enum machine_mode tmode;
     enum expand_modifier modifier;
{
  tree type = TREE_TYPE (exp);
  tree fndecl = TREE_OPERAND (TREE_OPERAND (exp, 0), 0);
  tree arglist = TREE_OPERAND (exp, 1);
  enum built_in_function fcode = DECL_FUNCTION_CODE (fndecl);
  enum tree_code code = TREE_CODE (exp);
  const int ignore = (target == const0_rtx
		      || ((code == NON_LVALUE_EXPR || code == NOP_EXPR
			   || code == CONVERT_EXPR || code == REFERENCE_EXPR
			   || code == COND_EXPR)
			  && TREE_CODE (type) == VOID_TYPE));

  if (! optimize && ! CALLED_AS_BUILT_IN (fndecl))
    return expand_call (exp, target, ignore);

  switch (fcode)
    {
    case BUILT_IN_PRINTF:
      target = c_expand_builtin_printf (arglist, target, tmode,
					modifier, ignore);
      if (target)
	return target;
      break;

    default:			/* just do library call, if unknown builtin */
      error ("built-in function `%s' not currently supported",
	     IDENTIFIER_POINTER (DECL_NAME (fndecl)));
    }

  /* The switch statement above can drop through to cause the function
     to be called normally.  */
  return expand_call (exp, target, ignore);
}

/* Check an arglist to *printf for problems.  The arglist should start
   at the format specifier, with the remaining arguments immediately
   following it. */
static int
is_valid_printf_arglist (arglist)
  tree arglist;
{
  /* Save this value so we can restore it later. */
  const int SAVE_pedantic = pedantic;
  int diagnostic_occurred = 0;

  /* Set this to a known value so the user setting won't affect code
     generation.  */
  pedantic = 1;
  /* Check to make sure there are no format specifier errors. */
  check_function_format (&diagnostic_occurred,
			 maybe_get_identifier("printf"),
			 NULL_TREE, arglist);

  /* Restore the value of `pedantic'. */
  pedantic = SAVE_pedantic;

  /* If calling `check_function_format_ptr' produces a warning, we
     return false, otherwise we return true. */
  return ! diagnostic_occurred;
}

/* If the arguments passed to printf are suitable for optimizations,
   we attempt to transform the call. */
static rtx
c_expand_builtin_printf (arglist, target, tmode, modifier, ignore)
     tree arglist;
     rtx target;
     enum machine_mode tmode;
     enum expand_modifier modifier;
     int ignore;
{
  tree fn_putchar = built_in_decls[BUILT_IN_PUTCHAR],
    fn_puts = built_in_decls[BUILT_IN_PUTS];
  tree fn, format_arg, stripped_string;

  /* If the return value is used, or the replacement _DECL isn't
     initialized, don't do the transformation. */
  if (!ignore || !fn_putchar || !fn_puts)
    return 0;

  /* Verify the required arguments in the original call. */
  if (arglist == 0
      || (TREE_CODE (TREE_TYPE (TREE_VALUE (arglist))) != POINTER_TYPE))
    return 0;
  
  /* Check the specifier vs. the parameters. */
  if (!is_valid_printf_arglist (arglist))
    return 0;
  
  format_arg = TREE_VALUE (arglist);
  stripped_string = format_arg;
  STRIP_NOPS (stripped_string);
  if (stripped_string && TREE_CODE (stripped_string) == ADDR_EXPR)
    stripped_string = TREE_OPERAND (stripped_string, 0);

  /* If the format specifier isn't a STRING_CST, punt.  */
  if (TREE_CODE (stripped_string) != STRING_CST)
    return 0;
  
  /* OK!  We can attempt optimization.  */

  /* If the format specifier was "%s\n", call __builtin_puts(arg2). */
  if (strcmp (TREE_STRING_POINTER (stripped_string), "%s\n") == 0)
    {
      arglist = TREE_CHAIN (arglist);
      fn = fn_puts;
    }
  /* If the format specifier was "%c", call __builtin_putchar (arg2). */
  else if (strcmp (TREE_STRING_POINTER (stripped_string), "%c") == 0)
    {
      arglist = TREE_CHAIN (arglist);
      fn = fn_putchar;
    }
  else
    {
     /* We can't handle anything else with % args or %% ... yet. */
      if (strchr (TREE_STRING_POINTER (stripped_string), '%'))
	return 0;
      
      /* If the resulting constant string has a length of 1, call
         putchar.  Note, TREE_STRING_LENGTH includes the terminating
         NULL in its count.  */
      if (TREE_STRING_LENGTH (stripped_string) == 2)
        {
	  /* Given printf("c"), (where c is any one character,)
             convert "c"[0] to an int and pass that to the replacement
             function. */
	  arglist = build_int_2 (TREE_STRING_POINTER (stripped_string)[0], 0);
	  arglist = build_tree_list (NULL_TREE, arglist);
	  
	  fn = fn_putchar;
        }
      /* If the resulting constant was "string\n", call
         __builtin_puts("string").  Ensure "string" has at least one
         character besides the trailing \n.  Note, TREE_STRING_LENGTH
         includes the terminating NULL in its count.  */
      else if (TREE_STRING_LENGTH (stripped_string) > 2
	       && TREE_STRING_POINTER (stripped_string)
	       [TREE_STRING_LENGTH (stripped_string) - 2] == '\n')
        {
	  /* Create a NULL-terminated string that's one char shorter
	     than the original, stripping off the trailing '\n'.  */
	  const int newlen = TREE_STRING_LENGTH (stripped_string) - 1;
	  char *newstr = (char *) alloca (newlen);
	  memcpy (newstr, TREE_STRING_POINTER (stripped_string), newlen - 1);
	  newstr[newlen - 1] = 0;
	  
	  arglist = combine_strings (build_string (newlen, newstr));
	  arglist = build_tree_list (NULL_TREE, arglist);
	  fn = fn_puts;
	}
      else
	/* We'd like to arrange to call fputs(string) here, but we
           need stdout and don't have a way to get it ... yet.  */
	return 0;
    }
  
  return expand_expr (build_function_call (fn, arglist),
		      (ignore ? const0_rtx : target),
		      tmode, modifier);
}
