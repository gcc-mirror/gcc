/* Print GENERIC declaration (functions, variables, types) trees coming from
   the C and C++ front-ends as well as macros in Ada syntax.
   Copyright (C) 2010-2025 Free Software Foundation, Inc.
   Adapted from tree-pretty-print.cc by Arnaud Charlet  <charlet@adacore.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "stringpool.h"
#include "tree.h"
#include "c-ada-spec.h"
#include "fold-const.h"
#include "c-pragma.h"
#include "diagnostic.h"
#include "stringpool.h"
#include "attribs.h"
#include "bitmap.h"

/* Local functions, macros and variables.  */
static int  dump_ada_node (pretty_printer *, tree, tree, int, bool, bool);
static int  dump_ada_declaration (pretty_printer *, tree, tree, int);
static void dump_ada_structure (pretty_printer *, tree, tree, bool, int);
static char *to_ada_name (const char *, bool *);

#define INDENT(SPACE) \
  do { int i; for (i = 0; i<SPACE; i++) pp_space (pp); } while (0)

#define INDENT_INCR 3

/* Global hook used to perform C++ queries on nodes.  */
static int (*cpp_check) (tree, cpp_operation) = NULL;

/* Global variables used in macro-related callbacks.  */
static int max_ada_macros;
static int store_ada_macro_index;
static const char *macro_source_file;

/* Given a cpp MACRO, compute the max length BUFFER_LEN of the macro, as well
   as max length PARAM_LEN of arguments for fun_like macros, and also set
   SUPPORTED to 0 if the macro cannot be mapped to an Ada construct.  */

static void
macro_length (const cpp_macro *macro, int *supported, int *buffer_len,
	      int *param_len)
{
  int i;
  unsigned j;

  *supported = 1;
  *buffer_len = 0;
  *param_len = 0;

  if (macro->fun_like)
    {
      (*param_len)++;
      for (i = 0; i < macro->paramc; i++)
	{
	  cpp_hashnode *param = macro->parm.params[i];

	  *param_len += NODE_LEN (param);

	  if (i + 1 < macro->paramc)
	    {
	      *param_len += 2;  /* ", " */
	    }
	  else if (macro->variadic)
	    {
	      *supported = 0;
	      return;
	    }
	}
      *param_len += 2;  /* ")\0" */
    }

  for (j = 0; j < macro->count; j++)
    {
      const cpp_token *token = &macro->exp.tokens[j];

      if (token->flags & PREV_WHITE)
	(*buffer_len)++;

      if (token->flags & STRINGIFY_ARG || token->flags & PASTE_LEFT)
	{
	  *supported = 0;
	  return;
	}

      if (token->type == CPP_MACRO_ARG)
	*buffer_len +=
	  NODE_LEN (macro->parm.params[token->val.macro_arg.arg_no - 1]);
      else
	/* Include enough extra space to handle e.g. special characters.  */
	*buffer_len += (cpp_token_len (token) + 1) * 8;
    }

  (*buffer_len)++;
}

/* Return true if NUMBER is a preprocessing floating-point number.  */

static bool
is_cpp_float (unsigned char *number)
{
  /* In C, a floating constant need not have a point.  */
  while (*number != '\0')
    {
      if (*number == '.')
	return true;
      else if ((*number == 'e' || *number == 'E')
	       && (*(number + 1) == '+' || *(number + 1) == '-'))
	return true;
      else
	number++;
    }

  return false;
}

/* Dump all digits/hex chars from NUMBER to BUFFER and return a pointer
   to the character after the last character written.  If FLOAT_P is true,
   this is a floating-point number.  */

static unsigned char *
dump_number (unsigned char *number, unsigned char *buffer, bool float_p)
{
  /* In Ada, a real literal is a numeric literal that includes a point.  */
  if (float_p)
    {
      bool point_seen = false;

      while (*number != '\0')
	{
	  if (ISDIGIT (*number))
	    *buffer++ = *number++;
	  else if (*number == '.')
	    {
	      *buffer++ = *number++;
	      point_seen = true;
	    }
	  else if ((*number == 'e' || *number == 'E')
		   && (*(number + 1) == '+' || *(number + 1) == '-'))
	    {
	      if (!point_seen)
		{
		  *buffer++ = '.';
		  *buffer++ = '0';
		  point_seen = true;
		}
	       *buffer++ = *number++;
	       *buffer++ = *number++;
	    }
	  else
	    break;
	}
    }

  /* An integer literal is a numeric literal without a point.  */
  else
    while (*number != '\0'
	   && *number != 'U'
	   && *number != 'u'
	   && *number != 'l'
	   && *number != 'L')
      *buffer++ = *number++;

  return buffer;
}

/* Handle escape character C and convert to an Ada character into BUFFER.
   Return a pointer to the character after the last character written, or
   NULL if the escape character is not supported.  */

static unsigned char *
handle_escape_character (unsigned char *buffer, char c)
{
  switch (c)
    {
      case '"':
	*buffer++ = '"';
	*buffer++ = '"';
	break;

      case 'n':
	strcpy ((char *) buffer, "\" & ASCII.LF & \"");
	buffer += 16;
	break;

      case 'r':
	strcpy ((char *) buffer, "\" & ASCII.CR & \"");
	buffer += 16;
	break;

      case 't':
	strcpy ((char *) buffer, "\" & ASCII.HT & \"");
	buffer += 16;
	break;

      default:
	return NULL;
    }

  return buffer;
}

/* Callback used to count the number of macros from cpp_forall_identifiers.
   PFILE and V are not used.  NODE is the current macro to consider.  */

static int
count_ada_macro (cpp_reader *pfile ATTRIBUTE_UNUSED, cpp_hashnode *node,
		 void *v ATTRIBUTE_UNUSED)
{
  if (cpp_user_macro_p (node) && *NODE_NAME (node) != '_')
    {
      const cpp_macro *macro = node->value.macro;
      if (macro->count && LOCATION_FILE (macro->line) == macro_source_file)
	max_ada_macros++;
    }

  return 1;
}

/* Callback used to store relevant macros from cpp_forall_identifiers.
   PFILE is not used.  NODE is the current macro to store if relevant.
   MACROS is an array of cpp_hashnode* used to store NODE.  */

static int
store_ada_macro (cpp_reader *pfile ATTRIBUTE_UNUSED,
		 cpp_hashnode *node, void *macros)
{
  if (cpp_user_macro_p (node) && *NODE_NAME (node) != '_')
    {
      const cpp_macro *macro = node->value.macro;
      if (macro->count
	  && LOCATION_FILE (macro->line) == macro_source_file)
	((cpp_hashnode **) macros)[store_ada_macro_index++] = node;
    }
  return 1;
}

/* Callback used to compare (during qsort) macros.  NODE1 and NODE2 are the
   two macro nodes to compare.  */

static int
compare_macro (const void *node1, const void *node2)
{
  typedef const cpp_hashnode *const_hnode;

  const_hnode n1 = *(const const_hnode *) node1;
  const_hnode n2 = *(const const_hnode *) node2;

  return n1->value.macro->line - n2->value.macro->line;
}

/* Dump in PP all relevant macros appearing in FILE.  */

static void
dump_ada_macros (pretty_printer *pp, const char* file)
{
  int num_macros = 0, prev_line = -1;
  cpp_hashnode **macros;

  /* Initialize file-scope variables.  */
  max_ada_macros = 0;
  store_ada_macro_index = 0;
  macro_source_file = file;

  /* Count all potentially relevant macros, and then sort them by sloc.  */
  cpp_forall_identifiers (parse_in, count_ada_macro, NULL);
  macros = XALLOCAVEC (cpp_hashnode *, max_ada_macros);
  cpp_forall_identifiers (parse_in, store_ada_macro, macros);
  qsort (macros, max_ada_macros, sizeof (cpp_hashnode *), compare_macro);

  for (int j = 0; j < max_ada_macros; j++)
    {
      cpp_hashnode *node = macros[j];
      const cpp_macro *macro = node->value.macro;
      unsigned i;
      int supported = 1, prev_is_one = 0, buffer_len, param_len;
      int is_string = 0, is_char = 0;
      char *ada_name;
      unsigned char *s, *params, *buffer, *buf_param, *char_one = NULL, *tmp;

      macro_length (macro, &supported, &buffer_len, &param_len);
      s = buffer = XALLOCAVEC (unsigned char, buffer_len);
      params = buf_param = XALLOCAVEC (unsigned char, param_len);

      if (supported)
	{
	  if (macro->fun_like)
	    {
	      *buf_param++ = '(';
	      for (i = 0; i < macro->paramc; i++)
		{
		  cpp_hashnode *param = macro->parm.params[i];

		  memcpy (buf_param, NODE_NAME (param), NODE_LEN (param));
		  buf_param += NODE_LEN (param);

		  if (i + 1 < macro->paramc)
		    {
		      *buf_param++ = ',';
		      *buf_param++ = ' ';
		    }
		  else if (macro->variadic)
		    {
		      supported = 0;
		      break;
		    }
		}
	      *buf_param++ = ')';
	      *buf_param = '\0';
	    }

	  for (i = 0; supported && i < macro->count; i++)
	    {
	      const cpp_token *token = &macro->exp.tokens[i];
	      int is_one = 0;

	      if (token->flags & PREV_WHITE)
		*buffer++ = ' ';

	      if (token->flags & STRINGIFY_ARG || token->flags & PASTE_LEFT)
		{
		  supported = 0;
		  break;
		}

	      switch (token->type)
		{
		  case CPP_MACRO_ARG:
		    {
		      cpp_hashnode *param =
			macro->parm.params[token->val.macro_arg.arg_no - 1];
		      memcpy (buffer, NODE_NAME (param), NODE_LEN (param));
		      buffer += NODE_LEN (param);
		    }
		    break;

		  case CPP_EQ_EQ:       *buffer++ = '='; break;
		  case CPP_GREATER:     *buffer++ = '>'; break;
		  case CPP_LESS:        *buffer++ = '<'; break;
		  case CPP_PLUS:        *buffer++ = '+'; break;
		  case CPP_MINUS:       *buffer++ = '-'; break;
		  case CPP_MULT:        *buffer++ = '*'; break;
		  case CPP_DIV:         *buffer++ = '/'; break;
		  case CPP_COMMA:       *buffer++ = ','; break;
		  case CPP_OPEN_SQUARE:
		  case CPP_OPEN_PAREN:  *buffer++ = '('; break;
		  case CPP_CLOSE_SQUARE: /* fallthrough */
		  case CPP_CLOSE_PAREN: *buffer++ = ')'; break;
		  case CPP_DEREF:       /* fallthrough */
		  case CPP_SCOPE:       /* fallthrough */
		  case CPP_DOT:         *buffer++ = '.'; break;

		  case CPP_EQ:          *buffer++ = ':'; *buffer++ = '='; break;
		  case CPP_NOT_EQ:      *buffer++ = '/'; *buffer++ = '='; break;
		  case CPP_GREATER_EQ:  *buffer++ = '>'; *buffer++ = '='; break;
		  case CPP_LESS_EQ:     *buffer++ = '<'; *buffer++ = '='; break;

		  case CPP_NOT:
		    *buffer++ = 'n'; *buffer++ = 'o'; *buffer++ = 't'; break;
		  case CPP_MOD:
		    *buffer++ = 'm'; *buffer++ = 'o'; *buffer++ = 'd'; break;
		  case CPP_AND:
		    *buffer++ = 'a'; *buffer++ = 'n'; *buffer++ = 'd'; break;
		  case CPP_OR:
		    *buffer++ = 'o'; *buffer++ = 'r'; break;
		  case CPP_XOR:
		    *buffer++ = 'x'; *buffer++ = 'o'; *buffer++ = 'r'; break;
		  case CPP_AND_AND:
		    strcpy ((char *) buffer, " and then ");
		    buffer += 10;
		    break;
		  case CPP_OR_OR:
		    strcpy ((char *) buffer, " or else ");
		    buffer += 9;
		    break;

		  case CPP_PADDING:
		    *buffer++ = ' ';
		    is_one = prev_is_one;
		    break;

		  case CPP_COMMENT:
		    break;

		  case CPP_WSTRING:
		  case CPP_STRING16:
		  case CPP_STRING32:
		  case CPP_UTF8STRING:
		  case CPP_WCHAR:
		  case CPP_CHAR16:
		  case CPP_CHAR32:
		  case CPP_UTF8CHAR:
		  case CPP_NAME:
		    if (!macro->fun_like)
		      supported = 0;
		    else
		      buffer
			= cpp_spell_token (parse_in, token, buffer, false);
		    break;

		  case CPP_STRING:
		    if (is_string)
		      {
			*buffer++ = '&';
			*buffer++ = ' ';
		      }
		    else
		      is_string = 1;
		    {
		      const unsigned char *s = token->val.str.text;

		      for (; *s; s++)
			if (*s == '\\')
			  {
			    s++;
			    buffer = handle_escape_character (buffer, *s);
			    if (buffer == NULL)
			      {
				supported = 0;
				break;
			      }
			  }
			else
			  *buffer++ = *s;
		    }
		    break;

		  case CPP_CHAR:
		    is_char = 1;
		    {
		      unsigned chars_seen;
		      int ignored;
		      cppchar_t c;

		      c = cpp_interpret_charconst (parse_in, token,
						   &chars_seen, &ignored);
		      if (c >= 32 && c <= 126)
			{
			  *buffer++ = '\'';
			  *buffer++ = (char) c;
			  *buffer++ = '\'';
			}
		      else
			{
			  chars_seen = sprintf ((char *) buffer,
						"Character'Val (%d)", (int) c);
			  buffer += chars_seen;
			}
		    }
		    break;

		  case CPP_NUMBER:
		    tmp = cpp_token_as_text (parse_in, token);

		    switch (*tmp)
		      {
			case '0':
			  switch (tmp[1])
			    {
			      case '\0':
			      case 'l':
			      case 'L':
			      case 'u':
			      case 'U':
				*buffer++ = '0';
				break;

			      case 'x':
			      case 'X':
				*buffer++ = '1';
				*buffer++ = '6';
				*buffer++ = '#';
				buffer = dump_number (tmp + 2, buffer, false);
				*buffer++ = '#';
				break;

			      case 'b':
			      case 'B':
				*buffer++ = '2';
				*buffer++ = '#';
				buffer = dump_number (tmp + 2, buffer, false);
				*buffer++ = '#';
				break;

			      default:
				/* Dump floating-point constant unmodified.  */
				if (is_cpp_float (tmp))
				  buffer = dump_number (tmp, buffer, true);
				else
				  {
				    *buffer++ = '8';
				    *buffer++ = '#';
				    buffer
				      = dump_number (tmp + 1, buffer, false);
				    *buffer++ = '#';
				  }
				break;
			    }
			  break;

			case '1':
			  if (tmp[1] == '\0'
			      || tmp[1] == 'u'
			      || tmp[1] == 'U'
			      || tmp[1] == 'l'
			      || tmp[1] == 'L')
			    {
			      is_one = 1;
			      char_one = buffer;
			      *buffer++ = '1';
			      break;
			    }
			  /* fallthrough */

			default:
			  buffer
			    = dump_number (tmp, buffer, is_cpp_float (tmp));
			  break;
		      }
		    break;

		  case CPP_LSHIFT:
		    if (prev_is_one)
		      {
			/* Replace "1 << N" by "2 ** N" */
		        *char_one = '2';
		        *buffer++ = '*';
		        *buffer++ = '*';
		        break;
		      }
		    /* fallthrough */

		  case CPP_RSHIFT:
		  case CPP_COMPL:
		  case CPP_QUERY:
		  case CPP_EOF:
		  case CPP_PLUS_EQ:
		  case CPP_MINUS_EQ:
		  case CPP_MULT_EQ:
		  case CPP_DIV_EQ:
		  case CPP_MOD_EQ:
		  case CPP_AND_EQ:
		  case CPP_OR_EQ:
		  case CPP_XOR_EQ:
		  case CPP_RSHIFT_EQ:
		  case CPP_LSHIFT_EQ:
		  case CPP_PRAGMA:
		  case CPP_PRAGMA_EOL:
		  case CPP_HASH:
		  case CPP_PASTE:
		  case CPP_OPEN_BRACE:
		  case CPP_CLOSE_BRACE:
		  case CPP_SEMICOLON:
		  case CPP_ELLIPSIS:
		  case CPP_PLUS_PLUS:
		  case CPP_MINUS_MINUS:
		  case CPP_DEREF_STAR:
		  case CPP_DOT_STAR:
		  case CPP_ATSIGN:
		  case CPP_HEADER_NAME:
		  case CPP_AT_NAME:
		  case CPP_OTHER:
		  case CPP_OBJC_STRING:
		  default:
		    if (!macro->fun_like)
		      supported = 0;
		    else
		      buffer = cpp_spell_token (parse_in, token, buffer, false);
		    break;
		}

	      prev_is_one = is_one;
	    }

	  if (supported)
	    *buffer = '\0';
	}

      if (macro->fun_like && supported)
	{
	  char *start = (char *) s;
	  int is_function = 0;

	  pp_string (pp, "   --  arg-macro: ");

	  if (*start == '(' && buffer[-1] == ')')
	    {
	      start++;
	      buffer[-1] = '\0';
	      is_function = 1;
	      pp_string (pp, "function ");
	    }
	  else
	    {
	      pp_string (pp, "procedure ");
	    }

	  pp_string (pp, (const char *) NODE_NAME (node));
	  pp_space (pp);
	  pp_string (pp, (char *) params);
	  pp_newline (pp);
	  pp_string (pp, "   --    ");

	  if (is_function)
	    {
	      pp_string (pp, "return ");
	      pp_string (pp, start);
	      pp_semicolon (pp);
	    }
	  else
	    pp_string (pp, start);

	  pp_newline (pp);
	}
      else if (supported)
	{
	  expanded_location sloc = expand_location (macro->line);

	  if (sloc.line != prev_line + 1 && prev_line > 0)
	    pp_newline (pp);

	  num_macros++;
	  prev_line = sloc.line;

	  pp_string (pp, "   ");
	  ada_name = to_ada_name ((const char *) NODE_NAME (node), NULL);
	  pp_string (pp, ada_name);
	  free (ada_name);
	  pp_string (pp, " : ");

	  if (is_string)
	    pp_string (pp, "aliased constant String");
	  else if (is_char)
	    pp_string (pp, "aliased constant Character");
	  else
	    pp_string (pp, "constant");

	  pp_string (pp, " := ");
	  pp_string (pp, (char *) s);

	  if (is_string)
	    pp_string (pp, " & ASCII.NUL");

	  pp_string (pp, ";  --  ");
	  pp_string (pp, sloc.file);
	  pp_colon (pp);
	  pp_decimal_int (pp, sloc.line);
	  pp_newline (pp);
	}
      else
	{
	  pp_string (pp, "   --  unsupported macro: ");
	  pp_string (pp, (const char *) cpp_macro_definition (parse_in, node));
	  pp_newline (pp);
	}
    }

  if (num_macros > 0)
    pp_newline (pp);
}

/* Current source file being handled.  */
static const char *current_source_file;

/* Return sloc of DECL, using sloc of last field if LAST is true.  */

static location_t
decl_sloc (const_tree decl, bool last)
{
  tree field;

  /* Compare the declaration of struct-like types based on the sloc of their
     last field (if LAST is true), so that more nested types collate before
     less nested ones.  */
  if (TREE_CODE (decl) == TYPE_DECL
      && !DECL_ORIGINAL_TYPE (decl)
      && RECORD_OR_UNION_TYPE_P (TREE_TYPE (decl))
      && (field = TYPE_FIELDS (TREE_TYPE (decl))))
    {
      if (last)
	while (DECL_CHAIN (field))
	  field = DECL_CHAIN (field);
      return DECL_SOURCE_LOCATION (field);
    }

  return DECL_SOURCE_LOCATION (decl);
}

/* Compare two locations LHS and RHS.  */

static int
compare_location (location_t lhs, location_t rhs)
{
  expanded_location xlhs = expand_location (lhs);
  expanded_location xrhs = expand_location (rhs);

  if (xlhs.file != xrhs.file)
    return filename_cmp (xlhs.file, xrhs.file);

  if (xlhs.line != xrhs.line)
    return xlhs.line - xrhs.line;

  if (xlhs.column != xrhs.column)
    return xlhs.column - xrhs.column;

  return 0;
}

/* Compare two declarations (LP and RP) by their source location.  */

static int
compare_node (const void *lp, const void *rp)
{
  const_tree lhs = *((const tree *) lp);
  const_tree rhs = *((const tree *) rp);
  const int ret
    = compare_location (decl_sloc (lhs, true), decl_sloc (rhs, true));

  return ret ? ret : DECL_UID (lhs) - DECL_UID (rhs);
}

/* Compare two comments (LP and RP) by their source location.  */

static int
compare_comment (const void *lp, const void *rp)
{
  const cpp_comment *lhs = (const cpp_comment *) lp;
  const cpp_comment *rhs = (const cpp_comment *) rp;

  return compare_location (lhs->sloc, rhs->sloc);
}

static tree *to_dump = NULL;
static int to_dump_count = 0;
static bool bitfield_used = false;
static bool packed_layout = false;

/* Collect a list of declarations from T relevant to SOURCE_FILE to be dumped
   by a subsequent call to dump_ada_nodes.  */

void
collect_ada_nodes (tree t, const char *source_file)
{
  tree n;
  int i = to_dump_count;

  /* Count the likely relevant nodes: do not dump builtins (they are irrelevant
     in the context of bindings) and namespaces (we do not handle them properly
     yet).  */
  for (n = t; n; n = TREE_CHAIN (n))
    if (!DECL_IS_UNDECLARED_BUILTIN (n)
	&& TREE_CODE (n) != NAMESPACE_DECL
	&& LOCATION_FILE (decl_sloc (n, false)) == source_file)
      to_dump_count++;

  /* Allocate sufficient storage for all nodes.  */
  to_dump = XRESIZEVEC (tree, to_dump, to_dump_count);

  /* Store the relevant nodes.  */
  for (n = t; n; n = TREE_CHAIN (n))
    if (!DECL_IS_UNDECLARED_BUILTIN (n)
	&& TREE_CODE (n) != NAMESPACE_DECL
	&& LOCATION_FILE (decl_sloc (n, false)) == source_file)
      to_dump[i++] = n;
}

/* Call back for walk_tree to clear the TREE_VISITED flag of TP.  */

static tree
unmark_visited_r (tree *tp, int *walk_subtrees ATTRIBUTE_UNUSED,
		  void *data ATTRIBUTE_UNUSED)
{
  if (TREE_VISITED (*tp))
    TREE_VISITED (*tp) = 0;
  else
    *walk_subtrees = 0;

  return NULL_TREE;
}

/* Print a COMMENT to the output stream PP.  */

static void
print_comment (pretty_printer *pp, const char *comment)
{
  int len = strlen (comment);
  char *str = XALLOCAVEC (char, len + 1);
  char *tok;
  bool extra_newline = false;

  memcpy (str, comment, len + 1);

  /* Trim C/C++ comment indicators.  */
  if (str[len - 2] == '*' && str[len - 1] == '/')
    {
      str[len - 2] = ' ';
      str[len - 1] = '\0';
    }
  str += 2;

  tok = strtok (str, "\n");
  while (tok) {
    pp_string (pp, "  --");
    pp_string (pp, tok);
    pp_newline (pp);
    tok = strtok (NULL, "\n");

    /* Leave a blank line after multi-line comments.  */
    if (tok)
      extra_newline = true;
  }

  if (extra_newline)
    pp_newline (pp);
}

/* Dump nodes into PP relevant to SOURCE_FILE, as collected by previous calls
   to collect_ada_nodes.  */

static void
dump_ada_nodes (pretty_printer *pp, const char *source_file)
{
  int i, j;
  cpp_comment_table *comments;

  /* Sort the table of declarations to dump by sloc.  */
  qsort (to_dump, to_dump_count, sizeof (tree), compare_node);

  /* Fetch the table of comments.  */
  comments = cpp_get_comments (parse_in);

  /* Sort the comments table by sloc.  */
  if (comments->count > 1)
    qsort (comments->entries, comments->count, sizeof (cpp_comment),
	   compare_comment);

  /* Interleave comments and declarations in line number order.  */
  i = j = 0;
  do
    {
      /* Advance j until comment j is in this file.  */
      while (j != comments->count
	     && LOCATION_FILE (comments->entries[j].sloc) != source_file)
	j++;

      /* Advance j until comment j is not a duplicate.  */
      while (j < comments->count - 1
	     && !compare_comment (&comments->entries[j],
				  &comments->entries[j + 1]))
	j++;

      /* Write decls until decl i collates after comment j.  */
      while (i != to_dump_count)
	{
	  if (j == comments->count
	      || LOCATION_LINE (decl_sloc (to_dump[i], false))
	      <  LOCATION_LINE (comments->entries[j].sloc))
	    {
	      current_source_file = source_file;

	      if (dump_ada_declaration (pp, to_dump[i++], NULL_TREE,
					 INDENT_INCR))
		{
		  pp_newline (pp);
		  pp_newline (pp);
		}
	    }
	  else
	    break;
	}

      /* Write comment j, if there is one.  */
      if (j != comments->count)
	print_comment (pp, comments->entries[j++].comment);

    } while (i != to_dump_count || j != comments->count);

  /* Clear the TREE_VISITED flag over each subtree we've dumped.  */
  for (i = 0; i < to_dump_count; i++)
    walk_tree (&to_dump[i], unmark_visited_r, NULL, NULL);

  /* Finalize the to_dump table.  */
  if (to_dump)
    {
      free (to_dump);
      to_dump = NULL;
      to_dump_count = 0;
    }
}

/* Dump a newline and indent BUFFER by SPC chars.  */

static void
newline_and_indent (pretty_printer *pp, int spc)
{
  pp_newline (pp);
  INDENT (spc);
}

struct with { char *s; const char *in_file; bool limited; };
static struct with *withs = NULL;
static int withs_max = 4096;
static int with_len = 0;

/* Record a "with" clause on package S (a limited with if LIMITED_ACCESS is
   true), if not already done.  */

static void
append_withs (const char *s, bool limited_access)
{
  int i;

  if (withs == NULL)
    withs = XNEWVEC (struct with, withs_max);

  if (with_len == withs_max)
    {
      withs_max *= 2;
      withs = XRESIZEVEC (struct with, withs, withs_max);
    }

  for (i = 0; i < with_len; i++)
    if (!strcmp (s, withs[i].s)
	&& current_source_file == withs[i].in_file)
      {
	withs[i].limited &= limited_access;
	return;
      }

  withs[with_len].s = xstrdup (s);
  withs[with_len].in_file = current_source_file;
  withs[with_len].limited = limited_access;
  with_len++;
}

/* Reset "with" clauses.  */

static void
reset_ada_withs (void)
{
  int i;

  if (!withs)
    return;

  for (i = 0; i < with_len; i++)
    free (withs[i].s);
  free (withs);
  withs = NULL;
  withs_max = 4096;
  with_len = 0;
}

/* Dump "with" clauses in F.  */

static void
dump_ada_withs (FILE *f)
{
  int i;

  fprintf (f, "with Interfaces.C; use Interfaces.C;\n");

  for (i = 0; i < with_len; i++)
    fprintf
      (f, "%swith %s;\n", withs[i].limited ? "limited " : "", withs[i].s);
}

/* Return suitable Ada package name from FILE.  */

static char *
get_ada_package (const char *file)
{
  const char *base;
  char *res;
  const char *s;
  int i;
  size_t plen;

  s = strstr (file, "/include/");
  if (s)
    base = s + 9;
  else
    base = lbasename (file);

  if (ada_specs_parent == NULL)
    plen = 0;
  else
    plen = strlen (ada_specs_parent) + 1;

  res = XNEWVEC (char, plen + strlen (base) + 1);
  if (ada_specs_parent != NULL) {
    strcpy (res, ada_specs_parent);
    res[plen - 1] = '.';
  }

  for (i = plen; *base; base++, i++)
    switch (*base)
      {
	case '+':
	  res[i] = 'p';
	  break;

	case '.':
	case '-':
	case '_':
	case '/':
	case '\\':
	  res[i] = (i == 0 || res[i - 1] == '.' || res[i - 1] == '_') ? 'u' : '_';
	  break;

	default:
	  res[i] = *base;
	  break;
      }
  res[i] = '\0';

  return res;
}

static const char *ada_reserved[] = {
  "abort", "abs", "abstract", "accept", "access", "aliased", "all", "and",
  "array", "at", "begin", "body", "case", "constant", "declare", "delay",
  "delta", "digits", "do", "else", "elsif", "end", "entry", "exception",
  "exit", "for", "function", "generic", "goto", "if", "in", "interface", "is",
  "limited", "loop", "mod", "new", "not", "null", "others", "out", "of", "or",
  "overriding", "package", "pragma", "private", "procedure", "protected",
  "raise", "range", "record", "rem", "renames", "requeue", "return", "reverse",
  "select", "separate", "subtype", "synchronized", "tagged", "task",
  "terminate", "then", "type", "until", "use", "when", "while", "with", "xor",
  NULL};

/* ??? would be nice to specify this list via a config file, so that users
   can create their own dictionary of conflicts.  */
static const char *c_duplicates[] = {
  /* system will cause troubles with System.Address.  */
  "system",

  /* The following values have other definitions with same name/other
     casing.  */
  "funmap",
  "rl_vi_fWord",
  "rl_vi_bWord",
  "rl_vi_eWord",
  "rl_readline_version",
  "_Vx_ushort",
  "USHORT",
  "XLookupKeysym",
  NULL};

/* Return a declaration tree corresponding to TYPE.  */

static tree
get_underlying_decl (tree type)
{
  if (!type)
    return NULL_TREE;

  /* type is a declaration.  */
  if (DECL_P (type))
    return type;

  if (TYPE_P (type))
    {
      /* Strip qualifiers but do not look through typedefs.  */
      if (TYPE_QUALS_NO_ADDR_SPACE (type))
	type = TYPE_MAIN_VARIANT (type);

      /* type is a typedef.  */
      if (TYPE_NAME (type) && DECL_P (TYPE_NAME (type)))
	return TYPE_NAME (type);

      /* TYPE_STUB_DECL has been set for type.  */
      if (TYPE_STUB_DECL (type))
	return TYPE_STUB_DECL (type);
    }

  return NULL_TREE;
}

/* Return whether TYPE has static fields.  */

static bool
has_static_fields (const_tree type)
{
  if (!type || !RECORD_OR_UNION_TYPE_P (type) || !COMPLETE_TYPE_P (type))
    return false;

  for (tree fld = TYPE_FIELDS (type); fld; fld = TREE_CHAIN (fld))
    if (VAR_P (fld) && DECL_NAME (fld))
      return true;

  return false;
}

/* Return whether TYPE corresponds to an Ada tagged type (has a dispatch
   table).  */

static bool
is_tagged_type (const_tree type)
{
  if (!type || !RECORD_OR_UNION_TYPE_P (type) || !COMPLETE_TYPE_P (type))
    return false;

  for (tree fld = TYPE_FIELDS (type); fld; fld = TREE_CHAIN (fld))
    if (TREE_CODE (fld) == FUNCTION_DECL && DECL_VINDEX (fld))
      return true;

  return false;
}

/* Return whether TYPE has non-trivial methods, i.e. methods that do something
   for the objects of TYPE.  In C++, all classes have implicit special methods,
   e.g. constructors and destructors, but they can be trivial if the type is
   sufficiently simple.  */

static bool
has_nontrivial_methods (tree type)
{
  if (!type || !RECORD_OR_UNION_TYPE_P (type) || !COMPLETE_TYPE_P (type))
    return false;

  /* Only C++ types can have methods.  */
  if (!cpp_check)
    return false;

  /* A non-trivial type has non-trivial special methods.  */
  if (!cpp_check (type, IS_TRIVIAL))
    return true;

  /* If there are user-defined methods, they are deemed non-trivial.  */
  for (tree fld = TYPE_FIELDS (type); fld; fld = DECL_CHAIN (fld))
    if (TREE_CODE (fld) == FUNCTION_DECL && !DECL_ARTIFICIAL (fld))
      return true;

  return false;
}

#define INDEX_LENGTH 8

/* Generate a legal Ada name from a C/C++ NAME and return a malloc'ed string.
   SPACE_FOUND, if not NULL, is used to indicate whether a space was found in
   NAME.  */

static char *
to_ada_name (const char *name, bool *space_found)
{
  const char **names;
  const int len = strlen (name);
  int j, len2 = 0;
  bool found = false;
  char *s = XNEWVEC (char, len * 2 + 5);
  char c;

  if (space_found)
    *space_found = false;

  /* Add "c_" prefix if name is an Ada reserved word.  */
  for (names = ada_reserved; *names; names++)
    if (!strcasecmp (name, *names))
      {
	s[len2++] = 'c';
	s[len2++] = '_';
	found = true;
	break;
      }

  if (!found)
    /* Add "c_" prefix if name is a potential case sensitive duplicate.  */
    for (names = c_duplicates; *names; names++)
      if (!strcmp (name, *names))
	{
	  s[len2++] = 'c';
	  s[len2++] = '_';
	  found = true;
	  break;
	}

  for (j = 0; name[j] == '_'; j++)
    s[len2++] = 'u';

  if (j > 0)
    s[len2++] = '_';
  else if (*name == '.' || *name == '$')
    {
      s[0] = 'a';
      s[1] = 'n';
      s[2] = 'o';
      s[3] = 'n';
      len2 = 4;
      j++;
    }

  /* Replace unsuitable characters for Ada identifiers.  */
  for (; j < len; j++)
    switch (name[j])
      {
	case ' ':
	  if (space_found)
	    *space_found = true;
	  s[len2++] = '_';
	  break;

	/* ??? missing some C++ operators.  */
	case '=':
	  s[len2++] = '_';

	  if (name[j + 1] == '=')
	    {
	      j++;
	      s[len2++] = 'e';
	      s[len2++] = 'q';
	    }
	  else
	    {
	      s[len2++] = 'a';
	      s[len2++] = 's';
	    }
	  break;

	case '!':
	  s[len2++] = '_';
	  if (name[j + 1] == '=')
	    {
	      j++;
	      s[len2++] = 'n';
	      s[len2++] = 'e';
	    }
	  break;

	case '~':
	  s[len2++] = '_';
	  s[len2++] = 't';
	  s[len2++] = 'i';
	  break;

	case '&':
	case '|':
	case '^':
	  s[len2++] = '_';
	  s[len2++] = name[j] == '&' ? 'a' : name[j] == '|' ? 'o' : 'x';

	  if (name[j + 1] == '=')
	    {
	      j++;
	      s[len2++] = 'e';
	    }
	  break;

	case '+':
	case '-':
	case '*':
	case '/':
	case '(':
	case '[':
	  if (s[len2 - 1] != '_')
	    s[len2++] = '_';

	  switch (name[j + 1]) {
	    case '\0':
	      j++;
	      switch (name[j - 1]) {
		case '+': s[len2++] = 'p'; break;  /* + */
		case '-': s[len2++] = 'm'; break;  /* - */
		case '*': s[len2++] = 't'; break;  /* * */
		case '/': s[len2++] = 'd'; break;  /* / */
	      }
	      break;

	    case '=':
	      j++;
	      switch (name[j - 1]) {
		case '+': s[len2++] = 'p'; break;  /* += */
		case '-': s[len2++] = 'm'; break;  /* -= */
		case '*': s[len2++] = 't'; break;  /* *= */
		case '/': s[len2++] = 'd'; break;  /* /= */
	      }
	      s[len2++] = 'a';
	      break;

	    case '-':  /* -- */
	      j++;
	      s[len2++] = 'm';
	      s[len2++] = 'm';
	      break;

	    case '+':  /* ++ */
	      j++;
	      s[len2++] = 'p';
	      s[len2++] = 'p';
	      break;

	    case ')':  /* () */
	      j++;
	      s[len2++] = 'o';
	      s[len2++] = 'p';
	      break;

	    case ']':  /* [] */
	      j++;
	      s[len2++] = 'o';
	      s[len2++] = 'b';
	      break;
	  }

	  break;

	case '<':
	case '>':
	  c = name[j] == '<' ? 'l' : 'g';
	  s[len2++] = '_';

	  switch (name[j + 1]) {
	    case '\0':
	      s[len2++] = c;
	      s[len2++] = 't';
	      break;
	    case '=':
	      j++;
	      s[len2++] = c;
	      s[len2++] = 'e';
	      break;
	    case '>':
	      j++;
	      s[len2++] = 's';
	      s[len2++] = 'r';
	      break;
	    case '<':
	      j++;
	      s[len2++] = 's';
	      s[len2++] = 'l';
	      break;
	    default:
	      break;
	  }
	  break;

	case '_':
	  if (len2 && s[len2 - 1] == '_')
	    s[len2++] = 'u';
	  /* fall through */

	default:
	  s[len2++] = name[j];
      }

  if (s[len2 - 1] == '_')
    s[len2++] = 'u';

  s[len2] = '\0';

  return s;
}

/* Return true if DECL refers to a C++ class type for which a
   separate enclosing package has been or should be generated.  */

static bool
separate_class_package (tree decl)
{
  tree type = TREE_TYPE (decl);
  return has_nontrivial_methods (type) || has_static_fields (type);
}

static bool package_prefix = true;

/* Dump in PP the name of an identifier NODE of type TYPE, following Ada
   syntax.  LIMITED_ACCESS indicates whether NODE can be accessed through a
   limited 'with' clause rather than a regular 'with' clause.  */

static void
pp_ada_tree_identifier (pretty_printer *pp, tree node, tree type,
			bool limited_access)
{
  const char *name = IDENTIFIER_POINTER (node);
  bool space_found = false;
  char *s = to_ada_name (name, &space_found);
  tree decl = get_underlying_decl (type);

  if (decl)
    {
      /* If the entity comes from another file, generate a package prefix.  */
      const expanded_location xloc = expand_location (decl_sloc (decl, false));

      if (xloc.line && xloc.file && xloc.file != current_source_file)
	{
	  switch (TREE_CODE (type))
	    {
	      case ENUMERAL_TYPE:
	      case INTEGER_TYPE:
	      case REAL_TYPE:
	      case FIXED_POINT_TYPE:
	      case BOOLEAN_TYPE:
	      case REFERENCE_TYPE:
	      case POINTER_TYPE:
	      case ARRAY_TYPE:
	      case RECORD_TYPE:
	      case UNION_TYPE:
	      case TYPE_DECL:
		if (package_prefix)
		  {
		    char *s1 = get_ada_package (xloc.file);
		    append_withs (s1, limited_access);
		    pp_string (pp, s1);
		    pp_dot (pp);
		    free (s1);
		  }
		break;
	      default:
		break;
	    }

	  /* Generate the additional package prefix for C++ classes.  */
	  if (separate_class_package (decl))
	    {
	      pp_string (pp, "Class_");
	      pp_string (pp, s);
	      pp_dot (pp);
	    }
	}
    }

  if (space_found)
    if (!strcmp (s, "short_int"))
      pp_string (pp, "short");
    else if (!strcmp (s, "short_unsigned_int"))
      pp_string (pp, "unsigned_short");
    else if (!strcmp (s, "unsigned_int"))
      pp_string (pp, "unsigned");
    else if (!strcmp (s, "long_int"))
      pp_string (pp, "long");
    else if (!strcmp (s, "long_unsigned_int"))
      pp_string (pp, "unsigned_long");
    else if (!strcmp (s, "long_long_int"))
      pp_string (pp, "Long_Long_Integer");
    else if (!strcmp (s, "long_long_unsigned_int"))
      {
	if (package_prefix)
	  {
	    append_withs ("Interfaces.C.Extensions", false);
	    pp_string (pp, "Extensions.unsigned_long_long");
	  }
	else
	  pp_string (pp, "unsigned_long_long");
      }
    else
      pp_string (pp, s);
  else
    if (!strcmp (s, "u_Bool") || !strcmp (s, "bool"))
      {
	if (package_prefix)
	  {
	    append_withs ("Interfaces.C.Extensions", false);
	    pp_string (pp, "Extensions.bool");
	  }
	else
	  pp_string (pp, "bool");
      }
    else
      pp_string (pp, s);

  free (s);
}

/* Dump in PP the assembly name of T.  */

static void
pp_asm_name (pretty_printer *pp, tree t)
{
  tree name = DECL_ASSEMBLER_NAME (t);
  char *ada_name = XALLOCAVEC (char, IDENTIFIER_LENGTH (name) + 1), *s;
  const char *ident = IDENTIFIER_POINTER (name);

  for (s = ada_name; *ident; ident++)
    {
      if (*ident == ' ')
	break;
      else if (*ident != '*')
	*s++ = *ident;
    }

  *s = '\0';
  pp_string (pp, ada_name);
}

/* Dump in PP the name of a DECL node if set, in Ada syntax.
   LIMITED_ACCESS indicates whether NODE can be accessed via a
   limited 'with' clause rather than a regular 'with' clause.  */

static void
dump_ada_decl_name (pretty_printer *pp, tree decl, bool limited_access)
{
  if (DECL_NAME (decl))
    pp_ada_tree_identifier (pp, DECL_NAME (decl), decl, limited_access);
  else
    {
      tree type_name = TYPE_NAME (TREE_TYPE (decl));

      if (!type_name)
	{
	  pp_string (pp, "anon");
	  if (TREE_CODE (decl) == FIELD_DECL)
	    pp_decimal_int (pp, DECL_UID (decl));
	  else
	    pp_decimal_int (pp, TYPE_UID (TREE_TYPE (decl)));
	}
      else if (TREE_CODE (type_name) == IDENTIFIER_NODE)
	pp_ada_tree_identifier (pp, type_name, decl, limited_access);
    }
}

/* Dump in PP a name for the type T, which is a TYPE without TYPE_NAME.  */

static void
dump_anonymous_type_name (pretty_printer *pp, tree t)
{
  pp_string (pp, "anon");

  switch (TREE_CODE (t))
    {
    case ARRAY_TYPE:
      pp_string (pp, "_array");
      break;
    case ENUMERAL_TYPE:
      pp_string (pp, "_enum");
      break;
    case RECORD_TYPE:
      pp_string (pp, "_struct");
      break;
    case UNION_TYPE:
      pp_string (pp, "_union");
      break;
    default:
      pp_string (pp, "_unknown");
      break;
    }

  pp_decimal_int (pp, TYPE_UID (t));
}

/* Dump in PP aspect Import on a given node T.  SPC is the current
   indentation level.  */

static void
dump_ada_import (pretty_printer *pp, tree t, int spc)
{
  const char *name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (t));
  const bool is_stdcall
    = TREE_CODE (t) == FUNCTION_DECL
      && lookup_attribute ("stdcall", TYPE_ATTRIBUTES (TREE_TYPE (t)));

  pp_string (pp, "with Import => True, ");

  newline_and_indent (pp, spc + 5);

  if (is_stdcall)
    pp_string (pp, "Convention => Stdcall, ");
  else if (name[0] == '_' && name[1] == 'Z')
    pp_string (pp, "Convention => CPP, ");
  else
    pp_string (pp, "Convention => C, ");

  newline_and_indent (pp, spc + 5);

  tree sec = lookup_attribute ("section", DECL_ATTRIBUTES (t));
  if (sec)
    {
      pp_string (pp, "Linker_Section => \"");
      pp_string (pp, TREE_STRING_POINTER (TREE_VALUE (TREE_VALUE (sec))));
      pp_string (pp, "\", ");
      newline_and_indent (pp, spc + 5);
    }

  pp_string (pp, "External_Name => \"");

  if (is_stdcall)
    pp_string (pp, IDENTIFIER_POINTER (DECL_NAME (t)));
  else
    pp_asm_name (pp, t);

  pp_string (pp, "\";");
}

/* Check whether T and its type have different names, and append "the_"
   otherwise in PP.  */

static void
check_type_name_conflict (pretty_printer *pp, tree t)
{
  tree tmp = TREE_TYPE (t);

  while (TREE_CODE (tmp) == POINTER_TYPE && !TYPE_NAME (tmp))
    tmp = TREE_TYPE (tmp);

  if (TREE_CODE (tmp) != FUNCTION_TYPE && tmp != error_mark_node)
    {
      const char *s;

      if (TREE_CODE (tmp) == IDENTIFIER_NODE)
	s = IDENTIFIER_POINTER (tmp);
      else if (!TYPE_NAME (tmp))
	s = "";
      else if (TREE_CODE (TYPE_NAME (tmp)) == IDENTIFIER_NODE)
	s = IDENTIFIER_POINTER (TYPE_NAME (tmp));
      else if (!DECL_NAME (TYPE_NAME (tmp)))
	s = "";
      else
	s = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (tmp)));

      if (!strcasecmp (IDENTIFIER_POINTER (DECL_NAME (t)), s))
	pp_string (pp, "the_");
    }
}

/* Dump in PP a function declaration FUNC in Ada syntax.
   IS_METHOD indicates whether FUNC is a C++ method.
   IS_CONSTRUCTOR whether FUNC is a C++ constructor.
   IS_DESTRUCTOR whether FUNC is a C++ destructor.
   SPC is the current indentation level.  */

static void
dump_ada_function_declaration (pretty_printer *pp, tree func,
			       bool is_method, bool is_constructor,
			       bool is_destructor, int spc)
{
  tree type = TREE_TYPE (func);
  tree arg = TYPE_ARG_TYPES (type);
  tree t;
  char buf[18];
  int num, num_args = 0, have_args = true, have_ellipsis = false;

  /* Compute number of arguments.  */
  if (arg)
    {
      while (TREE_CHAIN (arg) && arg != error_mark_node)
	{
	  num_args++;
	  arg = TREE_CHAIN (arg);
	}

      if (TREE_CODE (TREE_VALUE (arg)) != VOID_TYPE)
	{
	  num_args++;
	  have_ellipsis = true;
	}
    }

  if (is_constructor)
    num_args--;

  if (is_destructor)
    num_args = 1;

  if (num_args > 2)
    newline_and_indent (pp, spc + 1);

  if (num_args > 0)
    {
      pp_space (pp);
      pp_left_paren (pp);
    }

  /* For a function, see if we have the corresponding arguments.  */
  if (TREE_CODE (func) == FUNCTION_DECL)
    {
      arg = DECL_ARGUMENTS (func);
      for (t = arg, num = 0; t; t = DECL_CHAIN (t))
	num++;
      if (num < num_args)
	arg = NULL_TREE;
    }
  else
    arg = NULL_TREE;

  /* Otherwise, only print the types.  */
  if (!arg)
    {
      have_args = false;
      arg = TYPE_ARG_TYPES (type);
    }

  if (is_constructor)
    arg = TREE_CHAIN (arg);

  /* Print the argument names (if available) and types.  */
  for (num = 1; num <= num_args; num++)
    {
      if (have_args)
	{
	  if (DECL_NAME (arg))
	    {
	      check_type_name_conflict (pp, arg);
	      pp_ada_tree_identifier (pp, DECL_NAME (arg), NULL_TREE,
				      false);
	      pp_string (pp, " : ");
	    }
	  else
	    {
	      sprintf (buf, "arg%d : ", num);
	      pp_string (pp, buf);
	    }

	  dump_ada_node (pp, TREE_TYPE (arg), type, spc, false, true);
	}
      else
	{
	  sprintf (buf, "arg%d : ", num);
	  pp_string (pp, buf);
	  dump_ada_node (pp, TREE_VALUE (arg), type, spc, false, true);
	}

      /* If the type is a pointer to a tagged type, we need to differentiate
	 virtual methods from the rest (non-virtual methods, static member
	 or regular functions) and import only them as primitive operations,
	 because they make up the virtual table which is mirrored on the Ada
	 side by the dispatch table.  So we add 'Class to the type of every
	 parameter that is not the first one of a method which either has a
	 slot in the virtual table or is a constructor.  */
      if (TREE_TYPE (arg)
	  && POINTER_TYPE_P (TREE_TYPE (arg))
	  && is_tagged_type (TREE_TYPE (TREE_TYPE (arg)))
	  && !(num == 1 && is_method && (DECL_VINDEX (func) || is_constructor)))
	pp_string (pp, "'Class");

      arg = TREE_CHAIN (arg);

      if (num < num_args)
	{
	  pp_semicolon (pp);

	  if (num_args > 2)
	    newline_and_indent (pp, spc + INDENT_INCR);
	  else
	    pp_space (pp);
	}
    }

  if (have_ellipsis)
    {
      pp_string (pp, "  -- , ...");
      newline_and_indent (pp, spc + INDENT_INCR);
    }

  if (num_args > 0)
    pp_right_paren (pp);

  if (is_constructor || !VOID_TYPE_P (TREE_TYPE (type)))
    {
      pp_string (pp, " return ");
      tree rtype = is_constructor ? DECL_CONTEXT (func) : TREE_TYPE (type);
      dump_ada_node (pp, rtype, rtype, spc, false, true);
    }
}

/* Dump in PP all the domains associated with an array NODE,
   in Ada syntax.  SPC is the current indentation level.  */

static void
dump_ada_array_domains (pretty_printer *pp, tree node, int spc)
{
  bool first = true;

  pp_left_paren (pp);

  for (; TREE_CODE (node) == ARRAY_TYPE; node = TREE_TYPE (node))
    {
      tree domain = TYPE_DOMAIN (node);

      if (domain)
	{
	  tree min = TYPE_MIN_VALUE (domain);
	  tree max = TYPE_MAX_VALUE (domain);

	  if (!first)
	    pp_string (pp, ", ");
	  first = false;

	  if (min)
	    dump_ada_node (pp, min, NULL_TREE, spc, false, true);
	  pp_string (pp, " .. ");

	  /* If the upper bound is zero, gcc may generate a NULL_TREE
	     for TYPE_MAX_VALUE rather than an integer_cst.  */
	  if (max)
	    dump_ada_node (pp, max, NULL_TREE, spc, false, true);
	  else
	    pp_string (pp, "0");
	}
      else
	{
	  pp_string (pp, "size_t");
	  first = false;
	}
    }
  pp_right_paren (pp);
}

/* Dump in PP file:line information related to NODE.  */

static void
dump_sloc (pretty_printer *pp, tree node)
{
  expanded_location xloc;

  if (DECL_P (node))
    xloc = expand_location (DECL_SOURCE_LOCATION (node));
  else if (EXPR_HAS_LOCATION (node))
    xloc = expand_location (EXPR_LOCATION (node));
  else
    xloc.file = NULL;

  if (xloc.file)
    {
      pp_string (pp, xloc.file);
      pp_colon (pp);
      pp_decimal_int (pp, xloc.line);
    }
}

/* Return true if type T designates a 1-dimension array of "char".  */

static bool
is_char_array (tree t)
{
  return TREE_CODE (t) == ARRAY_TYPE
	 && TREE_CODE (TREE_TYPE (t)) == INTEGER_TYPE
	 && id_equal (DECL_NAME (TYPE_NAME (TREE_TYPE (t))), "char");
}

/* Dump in PP an array type NODE in Ada syntax.  SPC is the indentation
   level.  */

static void
dump_ada_array_type (pretty_printer *pp, tree node, int spc)
{
  const bool char_array = is_char_array (node);

  /* Special case char arrays.  */
  if (char_array)
    pp_string (pp, "Interfaces.C.char_array ");
  else
    pp_string (pp, "array ");

  /* Print the dimensions.  */
  dump_ada_array_domains (pp, node, spc);

  /* Print the component type.  */
  if (!char_array)
    {
      tree tmp = strip_array_types (node);

      pp_string (pp, " of ");

      if (TREE_CODE (tmp) != POINTER_TYPE && !packed_layout)
	pp_string (pp, "aliased ");

      if (TYPE_NAME (tmp)
	  || (!RECORD_OR_UNION_TYPE_P (tmp)
	      && TREE_CODE (tmp) != ENUMERAL_TYPE))
	dump_ada_node (pp, tmp, node, spc, false, true);
      else
	dump_anonymous_type_name (pp, tmp);
    }
}

/* Dump in PP type names associated with a template, each prepended with
   '_'.  TYPES is the TREE_PURPOSE of a DECL_TEMPLATE_INSTANTIATIONS.  SPC is
   the indentation level.  */

static void
dump_template_types (pretty_printer *pp, tree types, int spc)
{
  for (int i = 0; i < TREE_VEC_LENGTH (types); i++)
    {
      tree elem = TREE_VEC_ELT (types, i);
      pp_underscore (pp);

      if (!dump_ada_node (pp, elem, NULL_TREE, spc, false, true))
	{
	  pp_string (pp, "unknown");
	  pp_scalar (pp, HOST_SIZE_T_PRINT_UNSIGNED,
		     (fmt_size_t) TREE_HASH (elem));
	}
    }
}

/* Dump in PP the contents of all class instantiations associated with
   a given template T.  SPC is the indentation level.  */

static int
dump_ada_template (pretty_printer *pp, tree t, int spc)
{
  /* DECL_SIZE_UNIT is DECL_TEMPLATE_INSTANTIATIONS in this context.  */
  tree inst = DECL_SIZE_UNIT (t);
  /* This emulates DECL_TEMPLATE_RESULT in this context.  */
  struct tree_template_decl {
    struct tree_decl_common common;
    tree arguments;
    tree result;
  };
  tree result = ((struct tree_template_decl *) t)->result;
  int num_inst = 0;

  /* Don't look at template declarations declaring something coming from
     another file.  This can occur for template friend declarations.  */
  if (LOCATION_FILE (decl_sloc (result, false))
      != LOCATION_FILE (decl_sloc (t, false)))
    return 0;

  for (; inst && inst != error_mark_node; inst = TREE_CHAIN (inst))
    {
      tree types = TREE_PURPOSE (inst);
      tree instance = TREE_VALUE (inst);

      if (TREE_VEC_LENGTH (types) == 0)
	break;

      if (!RECORD_OR_UNION_TYPE_P (instance))
	break;

      /* We are interested in concrete template instantiations only: skip
	 partially specialized nodes.  */
      if (RECORD_OR_UNION_TYPE_P (instance)
	  && cpp_check
	  && cpp_check (instance, HAS_DEPENDENT_TEMPLATE_ARGS))
	continue;

      num_inst++;
      INDENT (spc);
      pp_string (pp, "package ");
      package_prefix = false;
      dump_ada_node (pp, instance, t, spc, false, true);
      dump_template_types (pp, types, spc);
      pp_string (pp, " is");
      spc += INDENT_INCR;
      newline_and_indent (pp, spc);

      TREE_VISITED (get_underlying_decl (instance)) = 1;
      pp_string (pp, "type ");
      dump_ada_node (pp, instance, t, spc, false, true);
      package_prefix = true;

      if (is_tagged_type (instance))
	pp_string (pp, " is tagged limited ");
      else
	pp_string (pp, " is limited ");

      dump_ada_node (pp, instance, t, spc, false, false);
      pp_newline (pp);
      spc -= INDENT_INCR;
      newline_and_indent (pp, spc);

      pp_string (pp, "end;");
      newline_and_indent (pp, spc);
      pp_string (pp, "use ");
      package_prefix = false;
      dump_ada_node (pp, instance, t, spc, false, true);
      dump_template_types (pp, types, spc);
      package_prefix = true;
      pp_semicolon (pp);
      pp_newline (pp);
      pp_newline (pp);
    }

  return num_inst > 0;
}

/* Return true if NODE is a simple enumeral type that can be mapped to an
   Ada enumeration type directly.  */

static bool
is_simple_enum (tree node)
{
  HOST_WIDE_INT count = 0;

  for (tree value = TYPE_VALUES (node); value; value = TREE_CHAIN (value))
    {
      tree int_val = TREE_VALUE (value);

      if (TREE_CODE (int_val) != INTEGER_CST)
	int_val = DECL_INITIAL (int_val);

      if (!tree_fits_shwi_p (int_val) || tree_to_shwi (int_val) != count)
	return false;

      count++;
    }

  return true;
}

/* Dump in PP the declaration of enumeral NODE of type TYPE in Ada syntax.
   SPC is the indentation level.  */

static void
dump_ada_enum_type (pretty_printer *pp, tree node, tree type, int spc)
{
  if (is_simple_enum (node))
    {
      bool first = true;
      spc += INDENT_INCR;
      newline_and_indent (pp, spc - 1);
      pp_left_paren (pp);
      for (tree value = TYPE_VALUES (node); value; value = TREE_CHAIN (value))
	{
	  if (first)
	    first = false;
	  else
	    {
	      pp_comma (pp);
	      newline_and_indent (pp, spc);
	    }

	  pp_ada_tree_identifier (pp, TREE_PURPOSE (value), node, false);
	}
      pp_string (pp, ")");
      spc -= INDENT_INCR;
      newline_and_indent (pp, spc);
      pp_string (pp, "with Convention => C");
    }
  else
    {
      if (TYPE_UNSIGNED (node))
	pp_string (pp, "unsigned");
      else
	pp_string (pp, "int");

      for (tree value = TYPE_VALUES (node); value; value = TREE_CHAIN (value))
	{
	  tree int_val = TREE_VALUE (value);

	  if (TREE_CODE (int_val) != INTEGER_CST)
	    int_val = DECL_INITIAL (int_val);

	  pp_semicolon (pp);
	  newline_and_indent (pp, spc);

	  if (TYPE_NAME (node))
	    dump_ada_node (pp, node, NULL_TREE, spc, false, true);
	  else if (type)
	    dump_ada_node (pp, type, NULL_TREE, spc, false, true);
	  else
	    dump_anonymous_type_name (pp, node);
	  pp_underscore (pp);
	  pp_ada_tree_identifier (pp, TREE_PURPOSE (value), node, false);

	  pp_string (pp, " : constant ");

	  if (TYPE_NAME (node))
	    dump_ada_node (pp, node, NULL_TREE, spc, false, true);
	  else if (type)
	    dump_ada_node (pp, type, NULL_TREE, spc, false, true);
	  else
	    dump_anonymous_type_name (pp, node);

	  pp_string (pp, " := ");
	  dump_ada_node (pp, int_val, node, spc, false, true);
	}
    }
}

/* Return true if NODE is the __bf16 type.  */

static bool
is_float16 (tree node)
{
  if (!TYPE_NAME (node) || TREE_CODE (TYPE_NAME (node)) != TYPE_DECL)
    return false;

  tree name = DECL_NAME (TYPE_NAME (node));

  if (IDENTIFIER_POINTER (name) [0] != '_')
    return false;

  return id_equal (name, "__bf16");
}

/* Return true if NODE is the _Float32/_Float32x type.  */

static bool
is_float32 (tree node)
{
  if (!TYPE_NAME (node) || TREE_CODE (TYPE_NAME (node)) != TYPE_DECL)
    return false;

  tree name = DECL_NAME (TYPE_NAME (node));

  if (IDENTIFIER_POINTER (name) [0] != '_')
    return false;

  return id_equal (name, "_Float32") || id_equal (name, "_Float32x");
}

/* Return true if NODE is the _Float64/_Float64x type.  */

static bool
is_float64 (tree node)
{
  if (!TYPE_NAME (node) || TREE_CODE (TYPE_NAME (node)) != TYPE_DECL)
    return false;

  tree name = DECL_NAME (TYPE_NAME (node));

  if (IDENTIFIER_POINTER (name) [0] != '_')
    return false;

  return id_equal (name, "_Float64") || id_equal (name, "_Float64x");
}

/* Return true if NODE is the __float128/_Float128/_Float128x type.  */

static bool
is_float128 (tree node)
{
  if (!TYPE_NAME (node) || TREE_CODE (TYPE_NAME (node)) != TYPE_DECL)
    return false;

  tree name = DECL_NAME (TYPE_NAME (node));

  if (IDENTIFIER_POINTER (name) [0] != '_')
    return false;

  return id_equal (name, "__float128")
	 || id_equal (name, "_Float128")
	 || id_equal (name, "_Float128x");
}

/* Recursively dump in PP Ada declarations corresponding to NODE of type
   TYPE.  SPC is the indentation level.  LIMITED_ACCESS indicates whether NODE
   can be referenced via a "limited with" clause.  NAME_ONLY indicates whether
   we should only dump the name of NODE, instead of its full declaration.  */

static int
dump_ada_node (pretty_printer *pp, tree node, tree type, int spc,
	       bool limited_access, bool name_only)
{
  if (node == NULL_TREE)
    return 0;

  switch (TREE_CODE (node))
    {
    case ERROR_MARK:
      pp_string (pp, "<<< error >>>");
      return 0;

    case IDENTIFIER_NODE:
      pp_ada_tree_identifier (pp, node, type, limited_access);
      break;

    case TREE_LIST:
      pp_string (pp, "--- unexpected node: TREE_LIST");
      return 0;

    case TREE_BINFO:
      dump_ada_node (pp, BINFO_TYPE (node), type, spc, limited_access,
		     name_only);
      return 0;

    case TREE_VEC:
      pp_string (pp, "--- unexpected node: TREE_VEC");
      return 0;

    case NULLPTR_TYPE:
    case VOID_TYPE:
      if (package_prefix)
	{
	  append_withs ("System", false);
	  pp_string (pp, "System.Address");
	}
      else
	pp_string (pp, "address");
      break;

    case VECTOR_TYPE:
      pp_string (pp, "<vector>");
      break;

    case COMPLEX_TYPE:
      if (is_float128 (TREE_TYPE (node)))
	{
	  append_withs ("Interfaces.C.Extensions", false);
	  pp_string (pp, "Extensions.CFloat_128");
	}
      else if (TREE_TYPE (node) == float_type_node)
	{
	  append_withs ("Ada.Numerics.Complex_Types", false);
	  pp_string (pp, "Ada.Numerics.Complex_Types.Complex");
	}
      else if (TREE_TYPE (node) == double_type_node)
	{
	  append_withs ("Ada.Numerics.Long_Complex_Types", false);
	  pp_string (pp, "Ada.Numerics.Long_Complex_Types.Complex");
	}
      else if (TREE_TYPE (node) == long_double_type_node)
	{
	  append_withs ("Ada.Numerics.Long_Long_Complex_Types", false);
	  pp_string (pp, "Ada.Numerics.Long_Long_Complex_Types.Complex");
	}
      else
	pp_string (pp, "<complex>");
      break;

    case ENUMERAL_TYPE:
      if (name_only)
	dump_ada_node (pp, TYPE_NAME (node), node, spc, false, true);
      else
	dump_ada_enum_type (pp, node, type, spc);
      break;

    case REAL_TYPE:
      if (is_float16 (node))
	{
	  pp_string (pp, "Short_Float");
	  break;
	}
      else if (is_float32 (node))
	{
	  pp_string (pp, "Float");
	  break;
	}
      else if (is_float64 (node))
	{
	  pp_string (pp, "Long_Float");
	  break;
	}
      else if (is_float128 (node))
	{
	  append_withs ("Interfaces.C.Extensions", false);
	  pp_string (pp, "Extensions.Float_128");
	  break;
	}

      /* fallthrough */

    case INTEGER_TYPE:
    case FIXED_POINT_TYPE:
    case BOOLEAN_TYPE:
      if (TYPE_NAME (node)
	  && !(TREE_CODE (TYPE_NAME (node)) == TYPE_DECL
	       && !strncmp (IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (node))),
			   "__int128", 8)))
	{
	  if (TREE_CODE (TYPE_NAME (node)) == IDENTIFIER_NODE)
	    pp_ada_tree_identifier (pp, TYPE_NAME (node), node,
				    limited_access);
	  else if (TREE_CODE (TYPE_NAME (node)) == TYPE_DECL
		   && DECL_NAME (TYPE_NAME (node)))
	    dump_ada_decl_name (pp, TYPE_NAME (node), limited_access);
	  else
	    pp_string (pp, "<unnamed type>");
	}
      else if (TREE_CODE (node) == INTEGER_TYPE)
	{
	  append_withs ("Interfaces.C.Extensions", false);
	  bitfield_used = true;

	  if (TYPE_PRECISION (node) == 1)
	    pp_string (pp, "Extensions.Unsigned_1");
	  else
	    {
	      pp_string (pp, TYPE_UNSIGNED (node)
				 ? "Extensions.Unsigned_"
				 : "Extensions.Signed_");
	      pp_decimal_int (pp, TYPE_PRECISION (node));
	    }
	}
      else
	pp_string (pp, "<unnamed type>");
      break;

    case POINTER_TYPE:
    case REFERENCE_TYPE:
      if (name_only && TYPE_NAME (node))
	dump_ada_node (pp, TYPE_NAME (node), node, spc, limited_access,
		       true);

      else if (TREE_CODE (TREE_TYPE (node)) == FUNCTION_TYPE)
	{
	  if (VOID_TYPE_P (TREE_TYPE (TREE_TYPE (node))))
	    pp_string (pp, "access procedure");
	  else
	    pp_string (pp, "access function");

	  dump_ada_function_declaration (pp, node, false, false, false,
					 spc + INDENT_INCR);

	  /* If we are dumping the full type, it means we are part of a
	     type definition and need also a Convention C aspect.  */
	  if (!name_only)
	    {
	      newline_and_indent (pp, spc);
	      pp_string (pp, "with Convention => C");
	    }
	}
      else
	{
	  tree ref_type = TREE_TYPE (node);
	  const unsigned int quals = TYPE_QUALS (ref_type);
	  bool is_access;

	  if (VOID_TYPE_P (ref_type))
	    {
	      if (!name_only)
		pp_string (pp, "new ");
	      if (package_prefix)
		{
		  append_withs ("System", false);
		  pp_string (pp, "System.Address");
		}
	      else
		pp_string (pp, "address");
	    }
	  else
	    {
	      if (TREE_CODE (node) == POINTER_TYPE
		  && TREE_CODE (ref_type) == INTEGER_TYPE
		  && id_equal (DECL_NAME (TYPE_NAME (ref_type)), "char"))
		{
		  if (!name_only)
		    pp_string (pp, "new ");

		  if (package_prefix)
		    {
		      pp_string (pp, "Interfaces.C.Strings.chars_ptr");
		      append_withs ("Interfaces.C.Strings", false);
		    }
		  else
		    pp_string (pp, "chars_ptr");
		}
	      else
		{
		  tree stub = TYPE_STUB_DECL (ref_type);
		  tree type_name = TYPE_NAME (ref_type);

		  /* For now, handle access-to-access as System.Address.  */
		  if (TREE_CODE (ref_type) == POINTER_TYPE)
		    {
		      if (package_prefix)
			{
			  append_withs ("System", false);
			  if (!name_only)
			    pp_string (pp, "new ");
			  pp_string (pp, "System.Address");
			}
		      else
			pp_string (pp, "address");
		      return spc;
		    }

		  if (!package_prefix)
		    {
		      is_access = false;
		      pp_string (pp, "access");
		    }
		  else if (AGGREGATE_TYPE_P (ref_type))
		    {
		      if (!type || TREE_CODE (type) != FUNCTION_DECL)
			{
			  is_access = true;
			  pp_string (pp, "access ");

			  if (quals & TYPE_QUAL_CONST)
			    pp_string (pp, "constant ");
			  else if (!name_only)
			    pp_string (pp, "all ");
			}
		      else if (quals & TYPE_QUAL_CONST)
			{
			  is_access = false;
			  pp_string (pp, "in ");
			}
		      else
			{
			  is_access = true;
			  pp_string (pp, "access ");
			}
		    }
		  else
		    {
		      /* We want to use regular with clauses for scalar types,
			 as they are not involved in circular declarations.  */
		      is_access = false;
		      pp_string (pp, "access ");

		      if (!name_only)
			pp_string (pp, "all ");
		    }

		  /* If this is the anonymous original type of a typedef'ed
		     type, then use the name of the latter.  */
		  if (!type_name
		      && stub
		      && DECL_CHAIN (stub)
		      && TREE_CODE (DECL_CHAIN (stub)) == TYPE_DECL
		      && DECL_ORIGINAL_TYPE (DECL_CHAIN (stub)) == ref_type)
		    ref_type = TREE_TYPE (DECL_CHAIN (stub));

		  /* If this is a pointer to an anonymous array type, then use
		     the name of the component type.  */
		  else if (!type_name && is_access)
		    ref_type = strip_array_types (ref_type);

		  /* Generate "access <type>" instead of "access <subtype>"
		     if the subtype comes from another file, because subtype
		     declarations do not contribute to the limited view of a
		     package and thus subtypes cannot be referenced through
		     a limited_with clause.  */
		  else if (is_access)
		    while (type_name
			   && TREE_CODE (type_name) == TYPE_DECL
			   && DECL_ORIGINAL_TYPE (type_name)
			   && TYPE_NAME (DECL_ORIGINAL_TYPE (type_name)))
		      {
			const expanded_location xloc
			  = expand_location (decl_sloc (type_name, false));
			if (xloc.line
			    && xloc.file
			    && xloc.file != current_source_file)
			  {
			    ref_type = DECL_ORIGINAL_TYPE (type_name);
			    type_name = TYPE_NAME (ref_type);
			  }
			else
			  break;
		      }

		  dump_ada_node (pp, ref_type, ref_type, spc, is_access,
				 true);
		}
	    }
	}
      break;

    case ARRAY_TYPE:
      if (name_only)
	dump_ada_node (pp, TYPE_NAME (node), node, spc, limited_access,
		       true);
      else
	dump_ada_array_type (pp, node, spc);
      break;

    case RECORD_TYPE:
    case UNION_TYPE:
      if (name_only)
	dump_ada_node (pp, TYPE_NAME (node), node, spc, limited_access,
		       true);
      else
	dump_ada_structure (pp, node, type, false, spc);
      break;

    case INTEGER_CST:
      /* We treat the upper half of the sizetype range as negative.  This
	 is consistent with the internal treatment and makes it possible
	 to generate the (0 .. -1) range for flexible array members.  */
      if (TREE_TYPE (node) == sizetype)
	node = fold_convert (ssizetype, node);
      if (tree_fits_shwi_p (node))
	pp_wide_integer (pp, tree_to_shwi (node));
      else if (tree_fits_uhwi_p (node))
	pp_unsigned_wide_integer (pp, tree_to_uhwi (node));
      else
	{
	  wide_int val = wi::to_wide (node);
	  int i;
	  if (wi::neg_p (val))
	    {
	      pp_minus (pp);
	      val = -val;
	    }
	  sprintf (pp_buffer (pp)->m_digit_buffer,
		   "16#%" HOST_WIDE_INT_PRINT "x",
		   val.elt (val.get_len () - 1));
	  for (i = val.get_len () - 2; i >= 0; i--)
	    sprintf (pp_buffer (pp)->m_digit_buffer,
		     HOST_WIDE_INT_PRINT_PADDED_HEX, val.elt (i));
	  pp_string (pp, pp_buffer (pp)->m_digit_buffer);
	}
      break;

    case REAL_CST:
    case FIXED_CST:
    case COMPLEX_CST:
    case STRING_CST:
    case VECTOR_CST:
      return 0;

    case TYPE_DECL:
      if (DECL_IS_UNDECLARED_BUILTIN (node))
	{
	  /* Don't print the declaration of built-in types.  */
	  if (name_only)
	    {
	      /* If we're in the middle of a declaration, defaults to
		 System.Address.  */
	      if (package_prefix)
		{
		  append_withs ("System", false);
		  pp_string (pp, "System.Address");
		}
	      else
		pp_string (pp, "address");
	    }
	}
      else if (name_only)
	dump_ada_decl_name (pp, node, limited_access);
      else
	{
	  if (is_tagged_type (TREE_TYPE (node)))
	    {
	      int first = true;

	      /* Look for ancestors.  */
	      for (tree fld = TYPE_FIELDS (TREE_TYPE (node));
		   fld;
		   fld = TREE_CHAIN (fld))
		{
		  if (!DECL_NAME (fld) && is_tagged_type (TREE_TYPE (fld)))
		    {
		      if (first)
			{
			  pp_string (pp, "limited new ");
			  first = false;
			}
		      else
			pp_string (pp, " and ");

		      dump_ada_decl_name (pp, TYPE_NAME (TREE_TYPE (fld)),
					  false);
		    }
		}

	      pp_string (pp, first ? "tagged limited " : " with ");
	    }
	  else if (has_nontrivial_methods (TREE_TYPE (node)))
	    pp_string (pp, "limited ");

	  dump_ada_node (pp, TREE_TYPE (node), type, spc, false, false);
	}
      break;

    case FUNCTION_DECL:
    case CONST_DECL:
    case VAR_DECL:
    case PARM_DECL:
    case FIELD_DECL:
    case NAMESPACE_DECL:
      dump_ada_decl_name (pp, node, false);
      break;

    default:
      /* Ignore other nodes (e.g. expressions).  */
      return 0;
    }

  return 1;
}

/* Dump in PP NODE's methods.  SPC is the indentation level.  Return 1 if
   methods were printed, 0 otherwise.  */

static int
dump_ada_methods (pretty_printer *pp, tree node, int spc)
{
  if (!has_nontrivial_methods (node))
    return 0;

  pp_semicolon (pp);

  int res = 1;
  for (tree fld = TYPE_FIELDS (node); fld; fld = DECL_CHAIN (fld))
    if (TREE_CODE (fld) == FUNCTION_DECL)
      {
	if (res)
	  {
	    pp_newline (pp);
	    pp_newline (pp);
	  }

	res = dump_ada_declaration (pp, fld, node, spc);
      }

  return 1;
}

/* Dump in PP a forward declaration for TYPE present inside T.
   SPC is the indentation level.  */

static void
dump_forward_type (pretty_printer *pp, tree type, tree t, int spc)
{
  tree decl = get_underlying_decl (type);

  /* Anonymous pointer and function types.  */
  if (!decl)
    {
      if (TREE_CODE (type) == POINTER_TYPE)
	dump_forward_type (pp, TREE_TYPE (type), t, spc);
      else if (TREE_CODE (type) == FUNCTION_TYPE)
	{
	  function_args_iterator args_iter;
	  tree arg;
	  dump_forward_type (pp, TREE_TYPE (type), t, spc);
	  FOREACH_FUNCTION_ARGS (type, arg, args_iter)
	    dump_forward_type (pp, arg, t, spc);
	}
      return;
    }

  if (DECL_IS_UNDECLARED_BUILTIN (decl) || TREE_VISITED (decl))
    return;

  /* Forward declarations are only needed within a given file.  */
  if (DECL_SOURCE_FILE (decl) != DECL_SOURCE_FILE (t))
    return;

  if (TREE_CODE (type) == FUNCTION_TYPE)
    return;

  /* Generate an incomplete type declaration.  */
  pp_string (pp, "type ");
  dump_ada_node (pp, decl, NULL_TREE, spc, false, true);
  pp_semicolon (pp);
  newline_and_indent (pp, spc);

  /* Only one incomplete declaration is legal for a given type.  */
  TREE_VISITED (decl) = 1;
}

/* Bitmap of anonymous types already dumped.  Anonymous array types are shared
   throughout the compilation so it needs to be global.  */

static bitmap dumped_anonymous_types;

static void dump_nested_type (pretty_printer *, tree, tree, int);

/* Dump in PP anonymous types nested inside T's definition.  PARENT is the
   parent node of T.  DUMPED_TYPES is the bitmap of already dumped types.  SPC
   is the indentation level.

   In C anonymous nested tagged types have no name whereas in C++ they have
   one.  In C their TYPE_DECL is at top level whereas in C++ it is nested.
   In both languages untagged types (pointers and arrays) have no name.
   In C++ the nested TYPE_DECLs can come after their associated FIELD_DECL.

   Therefore, in order to have a common processing for both languages, we
   disregard anonymous TYPE_DECLs at top level and here we make a first
   pass on the nested TYPE_DECLs and a second pass on the unnamed types.  */

static void
dump_nested_types (pretty_printer *pp, tree t, int spc)
{
  tree type, field;

  /* Find possible anonymous pointers/arrays/structs/unions recursively.  */
  type = TREE_TYPE (t);
  if (!type)
    return;

  for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
    if (TREE_CODE (field) == TYPE_DECL
	&& DECL_NAME (field) != DECL_NAME (t)
	&& !DECL_ORIGINAL_TYPE (field)
	&& TYPE_NAME (TREE_TYPE (field)) != TYPE_NAME (type))
      dump_nested_type (pp, field, t, spc);

  for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
    if (TREE_CODE (field) == FIELD_DECL && !TYPE_NAME (TREE_TYPE (field)))
      dump_nested_type (pp, field, t, spc);
}

/* Dump in PP the anonymous type of FIELD inside T.  SPC is the indentation
   level.  */

static void
dump_nested_type (pretty_printer *pp, tree field, tree t, int spc)
{
  tree field_type = TREE_TYPE (field);
  tree decl, tmp;

  switch (TREE_CODE (field_type))
    {
    case POINTER_TYPE:
      tmp = TREE_TYPE (field_type);
      dump_forward_type (pp, tmp, t, spc);
      break;

    case ARRAY_TYPE:
      /* Anonymous array types are shared.  */
      if (!bitmap_set_bit (dumped_anonymous_types, TYPE_UID (field_type)))
	return;

      tmp = strip_array_types (field_type);
      decl = get_underlying_decl (tmp);
      if (decl
	  && !DECL_NAME (decl)
	  && DECL_SOURCE_FILE (decl) == DECL_SOURCE_FILE (t)
	  && !TREE_VISITED (decl))
	{
	  /* Generate full declaration.  */
	  dump_nested_type (pp, decl, t, spc);
	  TREE_VISITED (decl) = 1;
	}
      else if (!decl && TREE_CODE (tmp) == POINTER_TYPE)
	dump_forward_type (pp, TREE_TYPE (tmp), t, spc);

      /* Special case char arrays.  */
      if (is_char_array (field_type))
	pp_string (pp, "subtype ");
      else
	pp_string (pp, "type ");

      dump_anonymous_type_name (pp, field_type);
      pp_string (pp, " is ");
      dump_ada_array_type (pp, field_type, spc);
      pp_semicolon (pp);
      newline_and_indent (pp, spc);
      break;

    case ENUMERAL_TYPE:
      if (is_simple_enum (field_type))
	pp_string (pp, "type ");
      else
	pp_string (pp, "subtype ");

      if (TYPE_NAME (field_type))
	dump_ada_node (pp, field_type, NULL_TREE, spc, false, true);
      else
	dump_anonymous_type_name (pp, field_type);
      pp_string (pp, " is ");
      dump_ada_enum_type (pp, field_type, NULL_TREE, spc);
      pp_semicolon (pp);
      newline_and_indent (pp, spc);
      break;

    case RECORD_TYPE:
    case UNION_TYPE:
      dump_nested_types (pp, field, spc);

      pp_string (pp, "type ");

      if (TYPE_NAME (field_type))
	dump_ada_node (pp, field_type, NULL_TREE, spc, false, true);
      else
	dump_anonymous_type_name (pp, field_type);

      if (TREE_CODE (field_type) == UNION_TYPE)
	pp_string (pp, " (discr : unsigned := 0)");

      pp_string (pp, " is ");
      dump_ada_structure (pp, field_type, t, true, spc);
      pp_semicolon (pp);
      newline_and_indent (pp, spc);
      break;

    default:
      break;
    }
}

/* Hash table of overloaded names that we cannot support.  It is needed even
   in Ada 2012 because we merge different types, e.g. void * and const void *
   in System.Address, so we cannot have overloading for them in Ada.  */

struct overloaded_name_hash {
  hashval_t hash;
  tree name;
  unsigned int n;
};

struct overloaded_name_hasher : delete_ptr_hash<overloaded_name_hash>
{
  static inline hashval_t hash (overloaded_name_hash *t)
    { return t->hash; }
  static inline bool equal (overloaded_name_hash *a, overloaded_name_hash *b)
    { return a->name == b->name; }
};

typedef hash_table<overloaded_name_hasher> htable_t;

static htable_t *overloaded_names;

/* Add an overloaded NAME with N occurrences to TABLE.  */

static void
add_name (const char *name, unsigned int n, htable_t *table)
{
  struct overloaded_name_hash in, *h, **slot;
  tree id = get_identifier (name);
  hashval_t hash = htab_hash_pointer (id);
  in.hash = hash;
  in.name = id;
  slot = table->find_slot_with_hash (&in, hash, INSERT);
  h = new overloaded_name_hash;
  h->hash = hash;
  h->name = id;
  h->n = n;
  *slot = h;
}

/* Initialize the table with the problematic overloaded names.  */

static htable_t *
init_overloaded_names (void)
{
  static const char *names[] =
  /* The overloaded names from the /usr/include/string.h file.  */
  { "memchr", "rawmemchr", "memrchr", "strchr", "strrchr", "strchrnul",
    "strpbrk", "strstr", "strcasestr", "index", "rindex", "basename" };

  htable_t *table = new htable_t (64);

  for (unsigned int i = 0; i < ARRAY_SIZE (names); i++)
    add_name (names[i], 0, table);

  /* Consider that sigaction() is overloaded by struct sigaction for QNX.  */
  add_name ("sigaction", 1, table);

  /* Consider that stat() is overloaded by struct stat for QNX.  */
  add_name ("stat", 1, table);

  return table;
}

/* Return the overloading index of NAME or 0 if NAME is not overloaded.  */

static unsigned int
overloading_index (tree name)
{
  struct overloaded_name_hash in, *h;
  hashval_t hash = htab_hash_pointer (name);
  in.hash = hash;
  in.name = name;
  h = overloaded_names->find_with_hash (&in, hash);
  return h ? ++h->n : 0;
}

/* Dump in PP constructor spec corresponding to T for TYPE.  */

static void
print_constructor (pretty_printer *pp, tree t, tree type)
{
  tree decl_name = DECL_NAME (TYPE_NAME (type));

  pp_string (pp, "New_");
  pp_ada_tree_identifier (pp, decl_name, t, false);
}

/* Dump in PP destructor spec corresponding to T.  */

static void
print_destructor (pretty_printer *pp, tree t, tree type)
{
  tree decl_name = DECL_NAME (TYPE_NAME (type));

  pp_string (pp, "Delete_");
  if (startswith (IDENTIFIER_POINTER (DECL_NAME (t)), "__dt_del"))
    pp_string (pp, "And_Free_");
  pp_ada_tree_identifier (pp, decl_name, t, false);
}

/* Dump in PP assignment operator spec corresponding to T.  */

static void
print_assignment_operator (pretty_printer *pp, tree t, tree type)
{
  tree decl_name = DECL_NAME (TYPE_NAME (type));

  pp_string (pp, "Assign_");
  pp_ada_tree_identifier (pp, decl_name, t, false);
}

/* Return the name of type T.  */

static const char *
type_name (tree t)
{
  tree n = TYPE_NAME (t);

  if (TREE_CODE (n) == IDENTIFIER_NODE)
    return IDENTIFIER_POINTER (n);
  else
    return IDENTIFIER_POINTER (DECL_NAME (n));
}

/* Dump in PP the declaration of object T of type TYPE in Ada syntax.
   SPC is the indentation level.  Return 1 if a declaration was printed,
   0 otherwise.  */

static int
dump_ada_declaration (pretty_printer *pp, tree t, tree type, int spc)
{
  bool is_var = false;
  bool need_indent = false;
  bool is_class = false;
  tree name = TYPE_NAME (TREE_TYPE (t));
  tree decl_name = DECL_NAME (t);
  tree orig = NULL_TREE;

  if (cpp_check && cpp_check (t, IS_TEMPLATE))
    return dump_ada_template (pp, t, spc);

  /* Skip enumeral values: will be handled as part of the type itself.  */
  if (TREE_CODE (t) == CONST_DECL && TREE_CODE (TREE_TYPE (t)) == ENUMERAL_TYPE)
    return 0;

  if (TREE_CODE (t) == TYPE_DECL)
    {
      orig = DECL_ORIGINAL_TYPE (t);

      /* This is a typedef.  */
      if (orig && TYPE_STUB_DECL (orig))
	{
	  tree stub = TYPE_STUB_DECL (orig);

	  /* If this is a typedef of a named type, then output it as a subtype
	     declaration.  ??? Use a derived type declaration instead.  */
	  if (TYPE_NAME (orig))
	    {
	      /* If the types have the same name (ignoring casing), then ignore
		 the second type, but forward declare the first if need be.  */
	      if (type_name (orig) == type_name (TREE_TYPE (t))
		  || !strcasecmp (type_name (orig), type_name (TREE_TYPE (t))))
		{
		  if (RECORD_OR_UNION_TYPE_P (orig) && !TREE_VISITED (stub))
		    {
		      INDENT (spc);
		      dump_forward_type (pp, orig, t, 0);
		    }

		  TREE_VISITED (t) = 1;
		  return 0;
		}

	      INDENT (spc);

	      if (RECORD_OR_UNION_TYPE_P (orig) && !TREE_VISITED (stub))
		dump_forward_type (pp, orig, t, spc);

	      pp_string (pp, "subtype ");
	      dump_ada_node (pp, t, type, spc, false, true);
	      pp_string (pp, " is ");
	      dump_ada_node (pp, orig, type, spc, false, true);
	      pp_string (pp, ";  -- ");
	      dump_sloc (pp, t);

	      TREE_VISITED (t) = 1;
	      return 1;
	    }

	  /* This is a typedef of an anonymous type.  We'll output the full
	     type declaration of the anonymous type with the typedef'ed name
	     below.  Prevent forward declarations for the anonymous type to
	     be emitted from now on.  */
	  TREE_VISITED (stub) = 1;
	}

      /* Skip unnamed or anonymous structs/unions/enum types.  */
      if (!orig
	  && (RECORD_OR_UNION_TYPE_P (TREE_TYPE (t))
	      || TREE_CODE (TREE_TYPE (t)) == ENUMERAL_TYPE)
	  && !decl_name
	  && !name)
	return 0;

      /* Skip duplicates of structs/unions/enum types built in C++.  */
      if (!orig
	  && (RECORD_OR_UNION_TYPE_P (TREE_TYPE (t))
	      || TREE_CODE (TREE_TYPE (t)) == ENUMERAL_TYPE)
	  && decl_name
	  && (*IDENTIFIER_POINTER (decl_name) == '.'
	      || *IDENTIFIER_POINTER (decl_name) == '$'))
	return 0;

      INDENT (spc);

      switch (TREE_CODE (TREE_TYPE (t)))
	{
	  case RECORD_TYPE:
	  case UNION_TYPE:
	    if (!COMPLETE_TYPE_P (TREE_TYPE (t)))
	      {
		pp_string (pp, "type ");
		dump_ada_node (pp, t, type, spc, false, true);
		pp_string (pp, " is null record;   -- incomplete struct");
		TREE_VISITED (t) = 1;
		return 1;
	      }

	    /* Packed record layout is not fully supported.  */
	    if (TYPE_PACKED (TREE_TYPE (t)))
	      {
		warning_at (DECL_SOURCE_LOCATION (t), 0, "packed layout");
		pp_string (pp, "pragma Compile_Time_Warning (True, ");
		pp_string (pp, "\"packed layout may be incorrect\");");
		newline_and_indent (pp, spc);
		packed_layout = true;
	      }

	    if (orig && TYPE_NAME (orig))
	      pp_string (pp, "subtype ");
	    else
	      {
                if (separate_class_package (t))
		  {
		    is_class = true;
		    pp_string (pp, "package Class_");
		    dump_ada_node (pp, t, type, spc, false, true);
		    pp_string (pp, " is");
		    spc += INDENT_INCR;
		    newline_and_indent (pp, spc);
		  }

		dump_nested_types (pp, t, spc);

		pp_string (pp, "type ");
	      }
	    break;

	  case POINTER_TYPE:
	  case REFERENCE_TYPE:
	    dump_forward_type (pp, TREE_TYPE (TREE_TYPE (t)), t, spc);
	    if (orig && TYPE_NAME (orig))
	      pp_string (pp, "subtype ");
	    else
	      pp_string (pp, "type ");
	    break;

	  case ARRAY_TYPE:
	    if ((orig && TYPE_NAME (orig)) || is_char_array (TREE_TYPE (t)))
	      pp_string (pp, "subtype ");
	    else
	      pp_string (pp, "type ");
	    break;

	  case FUNCTION_TYPE:
	    pp_string (pp, "--  skipped function type ");
	    dump_ada_node (pp, t, type, spc, false, true);
	    return 1;

	  case ENUMERAL_TYPE:
	    if ((orig && TYPE_NAME (orig) && orig != TREE_TYPE (t))
		|| !is_simple_enum (TREE_TYPE (t)))
	      pp_string (pp, "subtype ");
	    else
	      pp_string (pp, "type ");
	    break;

	  default:
	    pp_string (pp, "subtype ");
	}

      TREE_VISITED (t) = 1;
    }
  else
    {
      if (VAR_P (t)
	  && decl_name
	  && *IDENTIFIER_POINTER (decl_name) == '_')
	return 0;

      need_indent = true;
    }

  /* Print the type and name.  */
  if (TREE_CODE (TREE_TYPE (t)) == ARRAY_TYPE)
    {
      if (need_indent)
	INDENT (spc);

      /* Print variable's name.  */
      dump_ada_node (pp, t, type, spc, false, true);

      if (TREE_CODE (t) == TYPE_DECL)
	{
	  pp_string (pp, " is ");

	  if (orig && TYPE_NAME (orig))
	    dump_ada_node (pp, TYPE_NAME (orig), type, spc, false, true);
	  else
	    dump_ada_array_type (pp, TREE_TYPE (t), spc);
	}
      else
	{
	  if (spc == INDENT_INCR || TREE_STATIC (t))
	    is_var = true;

	  pp_string (pp, " : ");

	  if (TREE_CODE (TREE_TYPE (TREE_TYPE (t))) != POINTER_TYPE
	      && !packed_layout)
	    pp_string (pp, "aliased ");

	  if (TYPE_NAME (TREE_TYPE (t)))
	    dump_ada_node (pp, TREE_TYPE (t), type, spc, false, true);
	  else if (type)
	    dump_anonymous_type_name (pp, TREE_TYPE (t));
	  else
	    dump_ada_array_type (pp, TREE_TYPE (t), spc);
	}
    }
  else if (TREE_CODE (t) == FUNCTION_DECL)
    {
      tree decl_name = DECL_NAME (t);
      bool is_abstract_class = false;
      bool is_method = TREE_CODE (TREE_TYPE (t)) == METHOD_TYPE;
      bool is_abstract = false;
      bool is_assignment_operator = false;
      bool is_constructor = false;
      bool is_destructor = false;
      bool is_copy_constructor = false;
      bool is_move_constructor = false;

      if (!decl_name)
	return 0;

      if (cpp_check)
	{
	  is_abstract = cpp_check (t, IS_ABSTRACT);
	  is_assignment_operator = cpp_check (t, IS_ASSIGNMENT_OPERATOR);
	  is_constructor = cpp_check (t, IS_CONSTRUCTOR);
	  is_destructor = cpp_check (t, IS_DESTRUCTOR);
	  is_copy_constructor = cpp_check (t, IS_COPY_CONSTRUCTOR);
	  is_move_constructor = cpp_check (t, IS_MOVE_CONSTRUCTOR);
	}

      /* Skip copy constructors and C++11 move constructors: some are internal
	 only and those that are not cannot be called easily from Ada.  */
      if (is_copy_constructor || is_move_constructor)
	return 0;

      if (is_constructor || is_destructor)
	{
	  /* ??? Skip implicit constructors/destructors for now.  */
	  if (DECL_ARTIFICIAL (t))
	    return 0;

	  /* Only consider complete constructors and deleting destructors.  */
	  if (!startswith (IDENTIFIER_POINTER (decl_name), "__ct_comp")
	      && !startswith (IDENTIFIER_POINTER (decl_name), "__dt_comp")
	      && !startswith (IDENTIFIER_POINTER (decl_name), "__dt_del"))
	    return 0;
	}

      else if (is_assignment_operator)
	{
	  /* ??? Skip implicit or non-method assignment operators for now.  */
	  if (DECL_ARTIFICIAL (t) || !is_method)
	    return 0;
	}

      /* If this function has an entry in the vtable, we cannot omit it.  */
      else if (!DECL_VINDEX (t) && *IDENTIFIER_POINTER (decl_name) == '_')
	{
	  INDENT (spc);
	  pp_string (pp, "--  skipped func ");
	  pp_string (pp, IDENTIFIER_POINTER (decl_name));
	  return 1;
	}

      INDENT (spc);

      dump_forward_type (pp, TREE_TYPE (t), t, spc);

      if (VOID_TYPE_P (TREE_TYPE (TREE_TYPE (t))) && !is_constructor)
	pp_string (pp, "procedure ");
      else
	pp_string (pp, "function ");

      if (is_constructor)
	print_constructor (pp, t, type);
      else if (is_destructor)
	print_destructor (pp, t, type);
      else if (is_assignment_operator)
	print_assignment_operator (pp, t, type);
      else
	{
	  const unsigned int suffix = overloading_index (decl_name);
	  pp_ada_tree_identifier (pp, decl_name, t, false);
	  if (suffix > 1)
	    pp_decimal_int (pp, suffix);
	}

      dump_ada_function_declaration
	(pp, t, is_method, is_constructor, is_destructor, spc);

      if (is_constructor && RECORD_OR_UNION_TYPE_P (type))
	for (tree fld = TYPE_FIELDS (type); fld; fld = DECL_CHAIN (fld))
	  if (TREE_CODE (fld) == FUNCTION_DECL && cpp_check (fld, IS_ABSTRACT))
	    {
	      is_abstract_class = true;
	      break;
	    }

      if (is_abstract || is_abstract_class)
	pp_string (pp, " is abstract");

      if (is_abstract || !DECL_ASSEMBLER_NAME (t))
	{
	  pp_semicolon (pp);
	  pp_string (pp, "  -- ");
	  dump_sloc (pp, t);
	}
      else if (is_constructor)
	{
	  pp_semicolon (pp);
	  pp_string (pp, "  -- ");
	  dump_sloc (pp, t);

	  newline_and_indent (pp, spc);
	  pp_string (pp, "pragma CPP_Constructor (");
	  print_constructor (pp, t, type);
	  pp_string (pp, ", \"");
	  pp_asm_name (pp, t);
	  pp_string (pp, "\");");
	}
      else
	{
	  pp_string (pp, "  -- ");
	  dump_sloc (pp, t);

	  newline_and_indent (pp, spc);
	  dump_ada_import (pp, t, spc);
	}

      return 1;
    }
  else if (TREE_CODE (t) == TYPE_DECL && !orig)
    {
      bool is_interface = false;
      bool is_abstract_record = false;

      /* Anonymous structs/unions.  */
      dump_ada_node (pp, TREE_TYPE (t), t, spc, false, true);

      if (TREE_CODE (TREE_TYPE (t)) == UNION_TYPE)
	pp_string (pp, " (discr : unsigned := 0)");

      pp_string (pp, " is ");

      /* Check whether we have an Ada interface compatible class.
	 That is only have a vtable non-static data member and no
	 non-abstract methods.  */
      if (cpp_check
	  && RECORD_OR_UNION_TYPE_P (TREE_TYPE (t)))
	{
	  bool has_fields = false;

	  /* Check that there are no fields other than the virtual table.  */
	  for (tree fld = TYPE_FIELDS (TREE_TYPE (t));
	       fld;
	       fld = TREE_CHAIN (fld))
	    {
	      if (TREE_CODE (fld) == FIELD_DECL)
		{
		  if (!has_fields && DECL_VIRTUAL_P (fld))
		    is_interface = true;
		  else
		    is_interface = false;
		  has_fields = true;
		}
	      else if (TREE_CODE (fld) == FUNCTION_DECL
		       && !DECL_ARTIFICIAL (fld))
		{
		  if (cpp_check (fld, IS_ABSTRACT))
		    is_abstract_record = true;
		  else
		    is_interface = false;
		}
	    }
	}

      TREE_VISITED (t) = 1;
      if (is_interface)
	{
	  pp_string (pp, "limited interface  -- ");
	  dump_sloc (pp, t);
	  newline_and_indent (pp, spc);
	  pp_string (pp, "with Import => True,");
	  newline_and_indent (pp, spc + 5);
	  pp_string (pp, "Convention => CPP");

	  dump_ada_methods (pp, TREE_TYPE (t), spc);
	}
      else
	{
	  if (is_abstract_record)
	    pp_string (pp, "abstract ");
	  dump_ada_node (pp, t, t, spc, false, false);
	}
    }
  else
    {
      if (need_indent)
	INDENT (spc);

      if ((TREE_CODE (t) == FIELD_DECL || VAR_P (t))
	  && DECL_NAME (t))
	check_type_name_conflict (pp, t);

      /* Print variable/type's name.  */
      dump_ada_node (pp, t, t, spc, false, true);

      if (TREE_CODE (t) == TYPE_DECL)
	{
	  const bool is_subtype = TYPE_NAME (orig);

	  if (!is_subtype && TREE_CODE (TREE_TYPE (t)) == UNION_TYPE)
	    pp_string (pp, " (discr : unsigned := 0)");

	  pp_string (pp, " is ");

	  dump_ada_node (pp, orig, t, spc, false, is_subtype);
	}
      else
	{
	  if (spc == INDENT_INCR || TREE_STATIC (t))
	    is_var = true;

	  pp_string (pp, " : ");

	  if (TREE_CODE (TREE_TYPE (t)) != POINTER_TYPE
	      && (TYPE_NAME (TREE_TYPE (t))
		  || (TREE_CODE (TREE_TYPE (t)) != INTEGER_TYPE
		      && TREE_CODE (TREE_TYPE (t)) != ENUMERAL_TYPE))
	      && !packed_layout)
	    pp_string (pp, "aliased ");

	  if (TREE_READONLY (t) && TREE_CODE (t) != FIELD_DECL)
	    pp_string (pp, "constant ");

	  if (TYPE_NAME (TREE_TYPE (t))
	      || (!RECORD_OR_UNION_TYPE_P (TREE_TYPE (t))
		  && TREE_CODE (TREE_TYPE (t)) != ENUMERAL_TYPE))
	    dump_ada_node (pp, TREE_TYPE (t), t, spc, false, true);
	  else if (type)
	    dump_anonymous_type_name (pp, TREE_TYPE (t));
	}
    }

  if (is_class)
    {
      spc -= INDENT_INCR;
      newline_and_indent (pp, spc);
      pp_string (pp, "end;");
      newline_and_indent (pp, spc);
      pp_string (pp, "use Class_");
      dump_ada_node (pp, t, type, spc, false, true);
      pp_semicolon (pp);
      pp_newline (pp);

      /* All needed indentation/newline performed already, so return 0.  */
      return 0;
    }
  else if (is_var)
    {
      pp_string (pp, "  -- ");
      dump_sloc (pp, t);
      newline_and_indent (pp, spc);
      dump_ada_import (pp, t, spc);
    }

  else
    {
      pp_string (pp, ";  -- ");
      dump_sloc (pp, t);
    }

  return 1;
}

/* Dump in PP a structure NODE of type TYPE in Ada syntax.  If NESTED is
   true, it's an anonymous nested type.  SPC is the indentation level.  */

static void
dump_ada_structure (pretty_printer *pp, tree node, tree type, bool nested,
		    int spc)
{
  const bool is_union = (TREE_CODE (node) == UNION_TYPE);
  char buf[32];
  int field_num = 0;
  int field_spc = spc + INDENT_INCR;
  int need_semicolon;

  bitfield_used = false;

  /* Print the contents of the structure.  */
  pp_string (pp, "record");

  if (is_union)
    {
      newline_and_indent (pp, spc + INDENT_INCR);
      pp_string (pp, "case discr is");
      field_spc = spc + INDENT_INCR * 3;
    }

  pp_newline (pp);

  /* Print the non-static fields of the structure.  */
  for (tree tmp = TYPE_FIELDS (node); tmp; tmp = TREE_CHAIN (tmp))
    {
      /* Add parent field if needed.  */
      if (!DECL_NAME (tmp))
	{
	  if (!is_tagged_type (TREE_TYPE (tmp)))
	    {
	      if (!TYPE_NAME (TREE_TYPE (tmp)))
		dump_ada_declaration (pp, tmp, type, field_spc);
	      else
		{
		  INDENT (field_spc);

		  if (field_num == 0)
		    pp_string (pp, "parent : aliased ");
		  else
		    {
		      sprintf (buf, "field_%d : aliased ", field_num + 1);
		      pp_string (pp, buf);
		    }
		  dump_ada_decl_name (pp, TYPE_NAME (TREE_TYPE (tmp)),
				      false);
		  pp_semicolon (pp);
		}

	      pp_newline (pp);
	      field_num++;
	    }
	}
      else if (TREE_CODE (tmp) == FIELD_DECL)
	{
	  /* Skip internal virtual table field.  */
	  if (!DECL_VIRTUAL_P (tmp))
	    {
	      if (is_union)
		{
		  if (TREE_CHAIN (tmp)
		      && TREE_TYPE (TREE_CHAIN (tmp)) != node
		      && TREE_CODE (TREE_CHAIN (tmp)) != TYPE_DECL)
		    sprintf (buf, "when %d =>", field_num);
		  else
		    sprintf (buf, "when others =>");

		  INDENT (spc + INDENT_INCR * 2);
		  pp_string (pp, buf);
		  pp_newline (pp);
		}

	      if (dump_ada_declaration (pp, tmp, type, field_spc))
		{
		  pp_newline (pp);
		  field_num++;
		}
	    }
	}
    }

  if (is_union)
    {
      INDENT (spc + INDENT_INCR);
      pp_string (pp, "end case;");
      pp_newline (pp);
    }

  if (field_num == 0)
    {
      INDENT (spc + INDENT_INCR);
      pp_string (pp, "null;");
      pp_newline (pp);
    }

  INDENT (spc);
  pp_string (pp, "end record");

  newline_and_indent (pp, spc);

  /* We disregard the methods for anonymous nested types.  */
  if (has_nontrivial_methods (node) && !nested)
    {
      pp_string (pp, "with Import => True,");
      newline_and_indent (pp, spc + 5);
      pp_string (pp, "Convention => CPP");
    }
  else
    pp_string (pp, "with Convention => C_Pass_By_Copy");

  if (is_union)
    {
      pp_comma (pp);
      newline_and_indent (pp, spc + 5);
      pp_string (pp, "Unchecked_Union => True");
    }

  if (bitfield_used || packed_layout)
    {
      char buf[32];
      pp_comma (pp);
      newline_and_indent (pp, spc + 5);
      pp_string (pp, "Pack => True");
      pp_comma (pp);
      newline_and_indent (pp, spc + 5);
      sprintf (buf, "Alignment => %d", TYPE_ALIGN (node) / BITS_PER_UNIT);
      pp_string (pp, buf);
      bitfield_used = false;
      packed_layout = false;
    }

  if (nested)
    return;

  need_semicolon = !dump_ada_methods (pp, node, spc);

  /* Print the static fields of the structure, if any.  */
  for (tree tmp = TYPE_FIELDS (node); tmp; tmp = TREE_CHAIN (tmp))
    {
      if (VAR_P (tmp) && DECL_NAME (tmp))
	{
	  if (need_semicolon)
	    {
	      need_semicolon = false;
	      pp_semicolon (pp);
	    }
	  pp_newline (pp);
	  pp_newline (pp);
	  dump_ada_declaration (pp, tmp, type, spc);
	}
    }
}

/* Dump all the declarations in SOURCE_FILE to an Ada spec.
   COLLECT_ALL_REFS is a front-end callback used to collect all relevant
   nodes for SOURCE_FILE.  CHECK is used to perform C++ queries on nodes.  */

static void
dump_ads (const char *source_file,
	  void (*collect_all_refs)(const char *),
	  int (*check)(tree, cpp_operation))
{
  char *ads_name;
  char *pkg_name;
  char *s;
  FILE *f;

  pkg_name = get_ada_package (source_file);

  /* Construct the .ads filename and package name.  */
  ads_name = xstrdup (pkg_name);

  for (s = ads_name; *s; s++)
    if (*s == '.')
      *s = '-';
    else
      *s = TOLOWER (*s);

  ads_name = reconcat (ads_name, ads_name, ".ads", NULL);

  /* Write out the .ads file.  */
  f = fopen (ads_name, "w");
  if (f)
    {
      pretty_printer pp;

      pp_needs_newline (&pp) = true;
      pp.set_output_stream (f);

      /* Dump all relevant macros.  */
      dump_ada_macros (&pp, source_file);

      /* Reset the table of withs for this file.  */
      reset_ada_withs ();

      (*collect_all_refs) (source_file);

      /* Dump all references.  */
      cpp_check = check;
      dump_ada_nodes (&pp, source_file);

      /* We require Ada 2012 syntax, so generate corresponding pragma.  */
      fputs ("pragma Ada_2012;\n\n", f);

      /* Disable style checks and warnings on unused entities since this file
	 is auto-generated and always has a with clause for Interfaces.C.  */
      fputs ("pragma Style_Checks (Off);\n", f);
      fputs ("pragma Warnings (Off, \"-gnatwu\");\n\n", f);

      /* Dump withs.  */
      dump_ada_withs (f);

      fprintf (f, "\npackage %s is\n\n", pkg_name);
      pp_write_text_to_stream (&pp);
      /* ??? need to free pp */
      fprintf (f, "end %s;\n\n", pkg_name);

      fputs ("pragma Style_Checks (On);\n", f);
      fputs ("pragma Warnings (On, \"-gnatwu\");\n", f);
      fclose (f);
    }

  free (ads_name);
  free (pkg_name);
}

static const char **source_refs = NULL;
static int source_refs_used = 0;
static int source_refs_allocd = 0;

/* Add an entry for FILENAME to the table SOURCE_REFS.  */

void
collect_source_ref (const char *filename)
{
  int i;

  if (!filename)
    return;

  if (source_refs_allocd == 0)
    {
      source_refs_allocd = 1024;
      source_refs = XNEWVEC (const char *, source_refs_allocd);
    }

  for (i = 0; i < source_refs_used; i++)
    if (filename == source_refs[i])
      return;

  if (source_refs_used == source_refs_allocd)
    {
      source_refs_allocd *= 2;
      source_refs = XRESIZEVEC (const char *, source_refs, source_refs_allocd);
    }

  source_refs[source_refs_used++] = filename;
}

/* Main entry point: dump all Ada specs corresponding to SOURCE_REFS
   using callbacks COLLECT_ALL_REFS and CHECK.
   COLLECT_ALL_REFS is a front-end callback used to collect all relevant
   nodes for a given source file.
   CHECK is used to perform C++ queries on nodes, or NULL for the C
   front-end.  */

void
dump_ada_specs (void (*collect_all_refs)(const char *),
		int (*check)(tree, cpp_operation))
{
  bitmap_obstack_initialize (NULL);

  overloaded_names = init_overloaded_names ();

  /* Iterate over the list of files to dump specs for.  */
  for (int i = 0; i < source_refs_used; i++)
    {
      dumped_anonymous_types = BITMAP_ALLOC (NULL);
      dump_ads (source_refs[i], collect_all_refs, check);
      BITMAP_FREE (dumped_anonymous_types);
    }

  /* Free various tables.  */
  free (source_refs);
  delete overloaded_names;

  bitmap_obstack_release (NULL);
}
