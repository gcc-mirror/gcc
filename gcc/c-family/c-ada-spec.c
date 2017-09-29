/* Print GENERIC declaration (functions, variables, types) trees coming from
   the C and C++ front-ends as well as macros in Ada syntax.
   Copyright (C) 2010-2017 Free Software Foundation, Inc.
   Adapted from tree-pretty-print.c by Arnaud Charlet  <charlet@adacore.com>

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
#include "tree.h"
#include "c-ada-spec.h"
#include "fold-const.h"
#include "c-pragma.h"
#include "cpp-id-data.h"
#include "stringpool.h"
#include "attribs.h"

/* Local functions, macros and variables.  */
static int  dump_generic_ada_node (pretty_printer *, tree, tree, int, bool,
				   bool);
static int  dump_ada_declaration (pretty_printer *, tree, tree, int);
static void dump_ada_struct_decl (pretty_printer *, tree, tree, int, bool);
static char *to_ada_name (const char *, unsigned int, bool *);

#define INDENT(SPACE) \
  do { int i; for (i = 0; i<SPACE; i++) pp_space (buffer); } while (0)

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
	  cpp_hashnode *param = macro->params[i];

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
      cpp_token *token = &macro->exp.tokens[j];

      if (token->flags & PREV_WHITE)
	(*buffer_len)++;

      if (token->flags & STRINGIFY_ARG || token->flags & PASTE_LEFT)
	{
	  *supported = 0;
	  return;
	}

      if (token->type == CPP_MACRO_ARG)
	*buffer_len +=
	  NODE_LEN (macro->params[token->val.macro_arg.arg_no - 1]);
      else
	/* Include enough extra space to handle e.g. special characters.  */
	*buffer_len += (cpp_token_len (token) + 1) * 8;
    }

  (*buffer_len)++;
}

/* Dump all digits/hex chars from NUMBER to BUFFER and return a pointer
   to the character after the last character written.  */

static unsigned char *
dump_number (unsigned char *number, unsigned char *buffer)
{
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
  const cpp_macro *macro = node->value.macro;

  if (node->type == NT_MACRO && !(node->flags & NODE_BUILTIN)
      && macro->count
      && *NODE_NAME (node) != '_'
      && LOCATION_FILE (macro->line) == macro_source_file)
    max_ada_macros++;

  return 1;
}

/* Callback used to store relevant macros from cpp_forall_identifiers.
   PFILE is not used.  NODE is the current macro to store if relevant.
   MACROS is an array of cpp_hashnode* used to store NODE.  */

static int
store_ada_macro (cpp_reader *pfile ATTRIBUTE_UNUSED,
		 cpp_hashnode *node, void *macros)
{
  const cpp_macro *macro = node->value.macro;

  if (node->type == NT_MACRO && !(node->flags & NODE_BUILTIN)
      && macro->count
      && *NODE_NAME (node) != '_'
      && LOCATION_FILE (macro->line) == macro_source_file)
    ((cpp_hashnode **) macros)[store_ada_macro_index++] = node;

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
		  cpp_hashnode *param = macro->params[i];

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
	      cpp_token *token = &macro->exp.tokens[i];
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
			macro->params[token->val.macro_arg.arg_no - 1];
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

		  case CPP_COMMENT: break;

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
		      buffer = cpp_spell_token (parse_in, token, buffer, false);
		    break;

		  case CPP_STRING:
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
			  chars_seen = sprintf
			    ((char *) buffer, "Character'Val (%d)", (int) c);
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
				buffer = dump_number (tmp + 2, buffer);
				*buffer++ = '#';
				break;

			      case 'b':
			      case 'B':
				*buffer++ = '2';
				*buffer++ = '#';
				buffer = dump_number (tmp + 2, buffer);
				*buffer++ = '#';
				break;

			      default:
				/* Dump floating constants unmodified.  */
				if (strchr ((const char *)tmp, '.'))
				  buffer = dump_number (tmp, buffer);
				else
				  {
				    *buffer++ = '8';
				    *buffer++ = '#';
				    buffer = dump_number (tmp + 1, buffer);
				    *buffer++ = '#';
				  }
				break;
			    }
			  break;

			case '1':
			  if (tmp[1] == '\0' || tmp[1] == 'l' || tmp[1] == 'u'
			      || tmp[1] == 'L' || tmp[1] == 'U')
			    {
			      is_one = 1;
			      char_one = buffer;
			      *buffer++ = '1';
			    }
			  else
			    buffer = dump_number (tmp, buffer);
			  break;

			default:
			  buffer = dump_number (tmp, buffer);
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
	  ada_name = to_ada_name ((const char *) NODE_NAME (node), 0, NULL);
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
	  pp_scalar (pp, "%d", sloc.line);
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

location_t
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

  return compare_location (decl_sloc (lhs, true), decl_sloc (rhs, true));
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
    if (!DECL_IS_BUILTIN (n)
	&& TREE_CODE (n) != NAMESPACE_DECL
	&& LOCATION_FILE (decl_sloc (n, false)) == source_file)
      to_dump_count++;

  /* Allocate sufficient storage for all nodes.  */
  to_dump = XRESIZEVEC (tree, to_dump, to_dump_count);

  /* Store the relevant nodes.  */
  for (n = t; n; n = TREE_CHAIN (n))
    if (!DECL_IS_BUILTIN (n)
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
newline_and_indent (pretty_printer *buffer, int spc)
{
  pp_newline (buffer);
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

  /* type is a typedef.  */
  if (TYPE_P (type) && TYPE_NAME (type) && DECL_P (TYPE_NAME (type)))
    return TYPE_NAME (type);

  /* TYPE_STUB_DECL has been set for type.  */
  if (TYPE_P (type) && TYPE_STUB_DECL (type))
    return TYPE_STUB_DECL (type);

  return NULL_TREE;
}

/* Return whether TYPE has static fields.  */

static bool
has_static_fields (const_tree type)
{
  if (!type || !RECORD_OR_UNION_TYPE_P (type))
    return false;

  for (tree fld = TYPE_FIELDS (type); fld; fld = TREE_CHAIN (fld))
    if (TREE_CODE (fld) == VAR_DECL && DECL_NAME (fld))
      return true;

  return false;
}

/* Return whether TYPE corresponds to an Ada tagged type (has a dispatch
   table).  */

static bool
is_tagged_type (const_tree type)
{
  if (!type || !RECORD_OR_UNION_TYPE_P (type))
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
  if (!type || !RECORD_OR_UNION_TYPE_P (type))
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
   INDEX, if non-zero, is used to disambiguate overloaded names.  SPACE_FOUND,
   if not NULL, is used to indicate whether a space was found in NAME.  */

static char *
to_ada_name (const char *name, unsigned int index, bool *space_found)
{
  const char **names;
  const int len = strlen (name);
  int j, len2 = 0;
  bool found = false;
  char *s = XNEWVEC (char, len * 2 + 5 + (index ? INDEX_LENGTH : 0));
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

  if (index)
    snprintf (&s[len2], INDEX_LENGTH, "_u_%d", index + 1);
  else
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

/* Dump in BUFFER the name of an identifier NODE of type TYPE, following Ada
   syntax.  INDEX, if non-zero, is used to disambiguate overloaded names.
   LIMITED_ACCESS indicates whether NODE can be accessed via a limited
   'with' clause rather than a regular 'with' clause.  */

static void
pp_ada_tree_identifier (pretty_printer *buffer, tree node, tree type,
			unsigned int index, bool limited_access)
{
  const char *name = IDENTIFIER_POINTER (node);
  bool space_found = false;
  char *s = to_ada_name (name, index, &space_found);
  tree decl = get_underlying_decl (type);

  /* If the entity comes from another file, generate a package prefix.  */
  if (decl)
    {
      expanded_location xloc = expand_location (decl_sloc (decl, false));

      if (xloc.file && xloc.line)
	{
	  if (xloc.file != current_source_file)
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
			pp_string (buffer, s1);
			pp_dot (buffer);
			free (s1);
		      }
		    break;
		  default:
		    break;
		}

	      /* Generate the additional package prefix for C++ classes.  */
	      if (separate_class_package (decl))
		{
		  pp_string (buffer, "Class_");
		  pp_string (buffer, s);
		  pp_dot (buffer);
		}
	     }
	}
    }

  if (space_found)
    if (!strcmp (s, "short_int"))
      pp_string (buffer, "short");
    else if (!strcmp (s, "short_unsigned_int"))
      pp_string (buffer, "unsigned_short");
    else if (!strcmp (s, "unsigned_int"))
      pp_string (buffer, "unsigned");
    else if (!strcmp (s, "long_int"))
      pp_string (buffer, "long");
    else if (!strcmp (s, "long_unsigned_int"))
      pp_string (buffer, "unsigned_long");
    else if (!strcmp (s, "long_long_int"))
      pp_string (buffer, "Long_Long_Integer");
    else if (!strcmp (s, "long_long_unsigned_int"))
      {
	if (package_prefix)
	  {
	    append_withs ("Interfaces.C.Extensions", false);
	    pp_string (buffer, "Extensions.unsigned_long_long");
	  }
	else
	  pp_string (buffer, "unsigned_long_long");
      }
    else
      pp_string(buffer, s);
  else
    if (!strcmp (s, "bool"))
      {
	if (package_prefix)
	  {
	    append_withs ("Interfaces.C.Extensions", false);
	    pp_string (buffer, "Extensions.bool");
	  }
	else
	  pp_string (buffer, "bool");
      }
    else
      pp_string(buffer, s);

  free (s);
}

/* Dump in BUFFER the assembly name of T.  */

static void
pp_asm_name (pretty_printer *buffer, tree t)
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
  pp_string (buffer, ada_name);
}

/* Hash table of overloaded names associating identifier nodes with DECL_UIDs.
   It is needed in Ada 2005 because we can have at most one import directive
   per subprogram name in a given scope, so we have to mangle the subprogram
   names on the Ada side to import overloaded subprograms from C++.  */

struct overloaded_name_hash {
  hashval_t hash;
  tree name;
  tree context;
  vec<unsigned int> homonyms;
};

struct overloaded_name_hasher : delete_ptr_hash<overloaded_name_hash>
{
  static inline hashval_t hash (overloaded_name_hash *t)
    { return t->hash; }
  static inline bool equal (overloaded_name_hash *a, overloaded_name_hash *b)
    { return a->name == b->name && a->context == b->context; }
};

static hash_table<overloaded_name_hasher> *overloaded_names;

/* Compute the overloading index of function DECL in its context.  */

static unsigned int
compute_overloading_index (tree decl)
{
  const hashval_t hashcode
    = iterative_hash_hashval_t (htab_hash_pointer (DECL_NAME (decl)),
			        htab_hash_pointer (DECL_CONTEXT (decl)));
  struct overloaded_name_hash in, *h, **slot;
  unsigned int index, *iter;

  if (!overloaded_names)
    overloaded_names = new hash_table<overloaded_name_hasher> (512);

  /* Look up the list of homonyms in the table.  */
  in.hash = hashcode;
  in.name = DECL_NAME (decl);
  in.context = DECL_CONTEXT (decl);
  slot = overloaded_names->find_slot_with_hash (&in, hashcode, INSERT);
  if (*slot)
    h = *slot;
  else
    {
      h = new overloaded_name_hash;
      h->hash = hashcode;
      h->name = DECL_NAME (decl);
      h->context = DECL_CONTEXT (decl);
      h->homonyms.create (0);
      *slot = h;
    }

  /* Look up the function in the list of homonyms.  */
  FOR_EACH_VEC_ELT (h->homonyms, index, iter)
    if (*iter == DECL_UID (decl))
      break;

  /* If it is not present, push it onto the list.  */
  if (!iter)
    h->homonyms.safe_push (DECL_UID (decl));

  return index;
}

/* Dump in BUFFER the name of a DECL node if set, following Ada syntax.
   LIMITED_ACCESS indicates whether NODE can be accessed via a limited
   'with' clause rather than a regular 'with' clause.  */

static void
dump_ada_decl_name (pretty_printer *buffer, tree decl, bool limited_access)
{
  if (DECL_NAME (decl))
    {
      const unsigned int index
	= (TREE_CODE (decl) == FUNCTION_DECL && cpp_check)
	  ? compute_overloading_index (decl) : 0;
      pp_ada_tree_identifier (buffer, DECL_NAME (decl), decl, index,
			      limited_access);
    }
  else
    {
      tree type_name = TYPE_NAME (TREE_TYPE (decl));

      if (!type_name)
	{
	  pp_string (buffer, "anon");
	  if (TREE_CODE (decl) == FIELD_DECL)
	    pp_scalar (buffer, "%d", DECL_UID (decl));
	  else
	    pp_scalar (buffer, "%d", TYPE_UID (TREE_TYPE (decl)));
	}
      else if (TREE_CODE (type_name) == IDENTIFIER_NODE)
	pp_ada_tree_identifier (buffer, type_name, decl, 0, limited_access);
    }
}

/* Dump in BUFFER a name based on both T1 and T2 followed by a suffix.  */

static void
dump_ada_double_name (pretty_printer *buffer, tree t1, tree t2)
{
  if (DECL_NAME (t1))
    pp_ada_tree_identifier (buffer, DECL_NAME (t1), t1, 0, false);
  else
    {
      pp_string (buffer, "anon");
      pp_scalar (buffer, "%d", TYPE_UID (TREE_TYPE (t1)));
    }

  pp_underscore (buffer);

  if (DECL_NAME (t2))
    pp_ada_tree_identifier (buffer, DECL_NAME (t2), t2, 0, false);
  else
    {
      pp_string (buffer, "anon");
      pp_scalar (buffer, "%d", TYPE_UID (TREE_TYPE (t2)));
    }

  switch (TREE_CODE (TREE_TYPE (t2)))
    {
    case ARRAY_TYPE:
      pp_string (buffer, "_array");
      break;
    case RECORD_TYPE:
      pp_string (buffer, "_struct");
      break;
    case UNION_TYPE:
      pp_string (buffer, "_union");
      break;
    default:
      pp_string (buffer, "_unknown");
      break;
    }
}

/* Dump in BUFFER pragma Import C/CPP on a given node T.  */

static void
dump_ada_import (pretty_printer *buffer, tree t)
{
  const char *name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (t));
  int is_stdcall = TREE_CODE (t) == FUNCTION_DECL &&
    lookup_attribute ("stdcall", TYPE_ATTRIBUTES (TREE_TYPE (t)));

  if (is_stdcall)
    pp_string (buffer, "pragma Import (Stdcall, ");
  else if (name[0] == '_' && name[1] == 'Z')
    pp_string (buffer, "pragma Import (CPP, ");
  else
    pp_string (buffer, "pragma Import (C, ");

  dump_ada_decl_name (buffer, t, false);
  pp_string (buffer, ", \"");

  if (is_stdcall)
    pp_string (buffer, IDENTIFIER_POINTER (DECL_NAME (t)));
  else
    pp_asm_name (buffer, t);

  pp_string (buffer, "\");");
}

/* Check whether T and its type have different names, and append "the_"
   otherwise in BUFFER.  */

static void
check_name (pretty_printer *buffer, tree t)
{
  const char *s;
  tree tmp = TREE_TYPE (t);

  while (TREE_CODE (tmp) == POINTER_TYPE && !TYPE_NAME (tmp))
    tmp = TREE_TYPE (tmp);

  if (TREE_CODE (tmp) != FUNCTION_TYPE)
    {
      if (TREE_CODE (tmp) == IDENTIFIER_NODE)
	s = IDENTIFIER_POINTER (tmp);
      else if (!TYPE_NAME (tmp))
	s = "";
      else if (TREE_CODE (TYPE_NAME (tmp)) == IDENTIFIER_NODE)
	s = IDENTIFIER_POINTER (TYPE_NAME (tmp));
      else
	s = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (tmp)));

      if (!strcasecmp (IDENTIFIER_POINTER (DECL_NAME (t)), s))
	pp_string (buffer, "the_");
    }
}

/* Dump in BUFFER a function declaration FUNC with Ada syntax.
   IS_METHOD indicates whether FUNC is a C++ method.
   IS_CONSTRUCTOR whether FUNC is a C++ constructor.
   IS_DESTRUCTOR whether FUNC is a C++ destructor.
   SPC is the current indentation level.  */

static void
dump_ada_function_declaration (pretty_printer *buffer, tree func,
			       bool is_method, bool is_constructor,
			       bool is_destructor, int spc)
{
  tree arg;
  const tree node = TREE_TYPE (func);
  char buf[17];
  int num = 0, num_args = 0, have_args = true, have_ellipsis = false;

  /* Compute number of arguments.  */
  arg = TYPE_ARG_TYPES (node);

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
    newline_and_indent (buffer, spc + 1);

  if (num_args > 0)
    {
      pp_space (buffer);
      pp_left_paren (buffer);
    }

  if (TREE_CODE (func) == FUNCTION_DECL)
    arg = DECL_ARGUMENTS (func);
  else
    arg = NULL_TREE;

  if (arg == NULL_TREE)
    {
      have_args = false;
      arg = TYPE_ARG_TYPES (node);

      if (arg && TREE_CODE (TREE_VALUE (arg)) == VOID_TYPE)
	arg = NULL_TREE;
    }

  if (is_constructor)
    arg = TREE_CHAIN (arg);

  /* Print the argument names (if available) & types.  */

  for (num = 1; num <= num_args; num++)
    {
      if (have_args)
	{
	  if (DECL_NAME (arg))
	    {
	      check_name (buffer, arg);
	      pp_ada_tree_identifier (buffer, DECL_NAME (arg), NULL_TREE, 0,
				      false);
	      pp_string (buffer, " : ");
	    }
	  else
	    {
	      sprintf (buf, "arg%d : ", num);
	      pp_string (buffer, buf);
	    }

	  dump_generic_ada_node (buffer, TREE_TYPE (arg), node, spc, 0, true);
	}
      else
	{
	  sprintf (buf, "arg%d : ", num);
	  pp_string (buffer, buf);
	  dump_generic_ada_node (buffer, TREE_VALUE (arg), node, spc, 0, true);
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
	pp_string (buffer, "'Class");

      arg = TREE_CHAIN (arg);

      if (num < num_args)
	{
	  pp_semicolon (buffer);

	  if (num_args > 2)
	    newline_and_indent (buffer, spc + INDENT_INCR);
	  else
	    pp_space (buffer);
	}
    }

  if (have_ellipsis)
    {
      pp_string (buffer, "  -- , ...");
      newline_and_indent (buffer, spc + INDENT_INCR);
    }

  if (num_args > 0)
    pp_right_paren (buffer);

  if (is_constructor || !VOID_TYPE_P (TREE_TYPE (node)))
    {
      pp_string (buffer, " return ");
      tree type = is_constructor ? DECL_CONTEXT (func) : TREE_TYPE (node);
      dump_generic_ada_node (buffer, type, type, spc, false, true);
    }
}

/* Dump in BUFFER all the domains associated with an array NODE,
   using Ada syntax.  SPC is the current indentation level.  */

static void
dump_ada_array_domains (pretty_printer *buffer, tree node, int spc)
{
  int first = 1;
  pp_left_paren (buffer);

  for (; TREE_CODE (node) == ARRAY_TYPE; node = TREE_TYPE (node))
    {
      tree domain = TYPE_DOMAIN (node);

      if (domain)
	{
	  tree min = TYPE_MIN_VALUE (domain);
	  tree max = TYPE_MAX_VALUE (domain);

	  if (!first)
	    pp_string (buffer, ", ");
	  first = 0;

	  if (min)
	    dump_generic_ada_node (buffer, min, NULL_TREE, spc, 0, true);
	  pp_string (buffer, " .. ");

	  /* If the upper bound is zero, gcc may generate a NULL_TREE
	     for TYPE_MAX_VALUE rather than an integer_cst.  */
	  if (max)
	    dump_generic_ada_node (buffer, max, NULL_TREE, spc, 0, true);
	  else
	    pp_string (buffer, "0");
	}
      else
	pp_string (buffer, "size_t");
    }
  pp_right_paren (buffer);
}

/* Dump in BUFFER file:line information related to NODE.  */

static void
dump_sloc (pretty_printer *buffer, tree node)
{
  expanded_location xloc;

  xloc.file = NULL;

  if (DECL_P (node))
    xloc = expand_location (DECL_SOURCE_LOCATION (node));
  else if (EXPR_HAS_LOCATION (node))
    xloc = expand_location (EXPR_LOCATION (node));

  if (xloc.file)
    {
      pp_string (buffer, xloc.file);
      pp_colon (buffer);
      pp_decimal_int (buffer, xloc.line);
    }
}

/* Return true if T designates a one dimension array of "char".  */

static bool
is_char_array (tree t)
{
  tree tmp;
  int num_dim = 0;

  /* Retrieve array's type.  */
  tmp = t;
  while (TREE_CODE (TREE_TYPE (tmp)) == ARRAY_TYPE)
    {
      num_dim++;
      tmp = TREE_TYPE (tmp);
    }

  tmp = TREE_TYPE (tmp);
  return num_dim == 1 && TREE_CODE (tmp) == INTEGER_TYPE
    && id_equal (DECL_NAME (TYPE_NAME (tmp)), "char");
}

/* Dump in BUFFER an array type T in Ada syntax.  Assume that the "type"
   keyword and name have already been printed.  PARENT is the parent node of T.
   SPC is the indentation level.  */

static void
dump_ada_array_type (pretty_printer *buffer, tree t, tree parent, int spc)
{
  const bool char_array = is_char_array (t);
  tree tmp;

  /* Special case char arrays.  */
  if (char_array)
    {
      pp_string (buffer, "Interfaces.C.char_array ");
    }
  else
    pp_string (buffer, "array ");

  /* Print the dimensions.  */
  dump_ada_array_domains (buffer, TREE_TYPE (t), spc);

  /* Retrieve the element type.  */
  tmp = TREE_TYPE (t);
  while (TREE_CODE (tmp) == ARRAY_TYPE)
    tmp = TREE_TYPE (tmp);

  /* Print array's type.  */
  if (!char_array)
    {
      pp_string (buffer, " of ");

      if (TREE_CODE (tmp) != POINTER_TYPE)
	pp_string (buffer, "aliased ");

      if (TYPE_NAME (tmp) || !RECORD_OR_UNION_TYPE_P (tmp))
	dump_generic_ada_node (buffer, tmp, TREE_TYPE (t), spc, false, true);
      else
	dump_ada_double_name (buffer, parent, get_underlying_decl (tmp));
    }
}

/* Dump in BUFFER type names associated with a template, each prepended with
   '_'.  TYPES is the TREE_PURPOSE of a DECL_TEMPLATE_INSTANTIATIONS.  SPC is
   the indentation level.  */

static void
dump_template_types (pretty_printer *buffer, tree types, int spc)
{
  size_t i;
  size_t len = TREE_VEC_LENGTH (types);

  for (i = 0; i < len; i++)
    {
      tree elem = TREE_VEC_ELT (types, i);
      pp_underscore (buffer);
      if (!dump_generic_ada_node (buffer, elem, 0, spc, false, true))
	{
	  pp_string (buffer, "unknown");
	  pp_scalar (buffer, "%lu", (unsigned long) TREE_HASH (elem));
	}
    }
}

/* Dump in BUFFER the contents of all class instantiations associated with
   a given template T.  SPC is the indentation level.  */

static int
dump_ada_template (pretty_printer *buffer, tree t, int spc)
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
      pp_string (buffer, "package ");
      package_prefix = false;
      dump_generic_ada_node (buffer, instance, t, spc, false, true);
      dump_template_types (buffer, types, spc);
      pp_string (buffer, " is");
      spc += INDENT_INCR;
      newline_and_indent (buffer, spc);

      TREE_VISITED (get_underlying_decl (instance)) = 1;
      pp_string (buffer, "type ");
      dump_generic_ada_node (buffer, instance, t, spc, false, true);
      package_prefix = true;

      if (is_tagged_type (instance))
	pp_string (buffer, " is tagged limited ");
      else
	pp_string (buffer, " is limited ");

      dump_generic_ada_node (buffer, instance, t, spc, false, false);
      pp_newline (buffer);
      spc -= INDENT_INCR;
      newline_and_indent (buffer, spc);

      pp_string (buffer, "end;");
      newline_and_indent (buffer, spc);
      pp_string (buffer, "use ");
      package_prefix = false;
      dump_generic_ada_node (buffer, instance, t, spc, false, true);
      dump_template_types (buffer, types, spc);
      package_prefix = true;
      pp_semicolon (buffer);
      pp_newline (buffer);
      pp_newline (buffer);
    }

  return num_inst > 0;
}

/* Return true if NODE is a simple enum types, that can be mapped to an
   Ada enum type directly.  */

static bool
is_simple_enum (tree node)
{
  HOST_WIDE_INT count = 0;
  tree value;

  for (value = TYPE_VALUES (node); value; value = TREE_CHAIN (value))
    {
      tree int_val = TREE_VALUE (value);

      if (TREE_CODE (int_val) != INTEGER_CST)
	int_val = DECL_INITIAL (int_val);

      if (!tree_fits_shwi_p (int_val))
	return false;
      else if (tree_to_shwi (int_val) != count)
	return false;

      count++;
    }

  return true;
}

static bool bitfield_used = false;

/* Recursively dump in BUFFER Ada declarations corresponding to NODE of type
   TYPE.  SPC is the indentation level.  LIMITED_ACCESS indicates whether NODE
   can be referenced via a "limited with" clause.  NAME_ONLY indicates whether
   we should only dump the name of NODE, instead of its full declaration.  */

static int
dump_generic_ada_node (pretty_printer *buffer, tree node, tree type, int spc,
		       bool limited_access, bool name_only)
{
  if (node == NULL_TREE)
    return 0;

  switch (TREE_CODE (node))
    {
    case ERROR_MARK:
      pp_string (buffer, "<<< error >>>");
      return 0;

    case IDENTIFIER_NODE:
      pp_ada_tree_identifier (buffer, node, type, 0, limited_access);
      break;

    case TREE_LIST:
      pp_string (buffer, "--- unexpected node: TREE_LIST");
      return 0;

    case TREE_BINFO:
      dump_generic_ada_node
	(buffer, BINFO_TYPE (node), type, spc, limited_access, name_only);
      return 0;

    case TREE_VEC:
      pp_string (buffer, "--- unexpected node: TREE_VEC");
      return 0;

    case VOID_TYPE:
      if (package_prefix)
	{
	  append_withs ("System", false);
	  pp_string (buffer, "System.Address");
	}
      else
	pp_string (buffer, "address");
      break;

    case VECTOR_TYPE:
      pp_string (buffer, "<vector>");
      break;

    case COMPLEX_TYPE:
      pp_string (buffer, "<complex>");
      break;

    case ENUMERAL_TYPE:
      if (name_only)
	dump_generic_ada_node (buffer, TYPE_NAME (node), node, spc, 0, true);
      else
	{
	  tree value = TYPE_VALUES (node);

	  if (is_simple_enum (node))
	    {
	      bool first = true;
	      spc += INDENT_INCR;
	      newline_and_indent (buffer, spc - 1);
	      pp_left_paren (buffer);
	      for (; value; value = TREE_CHAIN (value))
		{
		  if (first)
		    first = false;
		  else
		    {
		      pp_comma (buffer);
		      newline_and_indent (buffer, spc);
		    }

		  pp_ada_tree_identifier (buffer, TREE_PURPOSE (value), node,
					  0, false);
		}
	      pp_string (buffer, ");");
	      spc -= INDENT_INCR;
	      newline_and_indent (buffer, spc);
	      pp_string (buffer, "pragma Convention (C, ");
	      dump_generic_ada_node
		(buffer, DECL_NAME (type) ? type : TYPE_NAME (node), type,
		 spc, 0, true);
	      pp_right_paren (buffer);
	    }
	  else
	    {
	      if (TYPE_UNSIGNED (node))
		pp_string (buffer, "unsigned");
	      else
		pp_string (buffer, "int");
	      for (; value; value = TREE_CHAIN (value))
		{
		  pp_semicolon (buffer);
		  newline_and_indent (buffer, spc);

		  pp_ada_tree_identifier (buffer, TREE_PURPOSE (value), node,
					  0, false);
		  pp_string (buffer, " : constant ");

		  dump_generic_ada_node
		    (buffer, DECL_NAME (type) ? type : TYPE_NAME (node), type,
		     spc, 0, true);

		  pp_string (buffer, " := ");
		  dump_generic_ada_node
		    (buffer,
		     TREE_CODE (TREE_VALUE (value)) == INTEGER_CST ?
		       TREE_VALUE (value) : DECL_INITIAL (TREE_VALUE (value)),
		     node, spc, false, true);
		}
	    }
	}
      break;

    case INTEGER_TYPE:
    case REAL_TYPE:
    case FIXED_POINT_TYPE:
    case BOOLEAN_TYPE:
      {
	enum tree_code_class tclass;

	tclass = TREE_CODE_CLASS (TREE_CODE (node));

	if (tclass == tcc_declaration)
	  {
	    if (DECL_NAME (node))
	      pp_ada_tree_identifier (buffer, DECL_NAME (node), NULL_TREE, 0,
				      limited_access);
	    else
	      pp_string (buffer, "<unnamed type decl>");
	  }
	else if (tclass == tcc_type)
	  {
	    if (TYPE_NAME (node))
	      {
		if (TREE_CODE (TYPE_NAME (node)) == IDENTIFIER_NODE)
		  pp_ada_tree_identifier (buffer, TYPE_NAME (node), node, 0,
					  limited_access);
		else if (TREE_CODE (TYPE_NAME (node)) == TYPE_DECL
			 && DECL_NAME (TYPE_NAME (node)))
		  dump_ada_decl_name (buffer, TYPE_NAME (node), limited_access);
		else
		  pp_string (buffer, "<unnamed type>");
	      }
	    else if (TREE_CODE (node) == INTEGER_TYPE)
	      {
		append_withs ("Interfaces.C.Extensions", false);
		bitfield_used = true;

		if (TYPE_PRECISION (node) == 1)
		  pp_string (buffer, "Extensions.Unsigned_1");
		else
		  {
		    pp_string (buffer, (TYPE_UNSIGNED (node)
					? "Extensions.Unsigned_"
					: "Extensions.Signed_"));
		    pp_decimal_int (buffer, TYPE_PRECISION (node));
		  }
	      }
	    else
	      pp_string (buffer, "<unnamed type>");
	  }
	break;
      }

    case POINTER_TYPE:
    case REFERENCE_TYPE:
      if (name_only && TYPE_NAME (node))
	dump_generic_ada_node
	  (buffer, TYPE_NAME (node), node, spc, limited_access, true);

      else if (TREE_CODE (TREE_TYPE (node)) == FUNCTION_TYPE)
	{
	  if (VOID_TYPE_P (TREE_TYPE (TREE_TYPE (node))))
	    pp_string (buffer, "access procedure ");
	  else
	    pp_string (buffer, "access function ");

	  dump_ada_function_declaration
	    (buffer, node, false, false, false, spc + INDENT_INCR);

	  /* If we are dumping the full type, it means we are part of a
	     type definition and need also a Convention C pragma.  */
	  if (!name_only)
	    {
	      pp_semicolon (buffer);
	      newline_and_indent (buffer, spc);
	      pp_string (buffer, "pragma Convention (C, ");
	      dump_generic_ada_node (buffer, type, 0, spc, false, true);
	      pp_right_paren (buffer);
	    }
	}
      else
	{
	  int is_access = false;
	  unsigned int quals = TYPE_QUALS (TREE_TYPE (node));

	  if (VOID_TYPE_P (TREE_TYPE (node)))
	    {
	      if (!name_only)
		pp_string (buffer, "new ");
	      if (package_prefix)
		{
		  append_withs ("System", false);
		  pp_string (buffer, "System.Address");
		}
	      else
		pp_string (buffer, "address");
	    }
	  else
	    {
	      if (TREE_CODE (node) == POINTER_TYPE
		  && TREE_CODE (TREE_TYPE (node)) == INTEGER_TYPE
		  && !strcmp
			(IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME
			  (TREE_TYPE (node)))), "char"))
		{
		  if (!name_only)
		    pp_string (buffer, "new ");

		  if (package_prefix)
		    {
		      pp_string (buffer, "Interfaces.C.Strings.chars_ptr");
		      append_withs ("Interfaces.C.Strings", false);
		    }
		  else
		    pp_string (buffer, "chars_ptr");
		}
	      else
		{
		  tree type_name = TYPE_NAME (TREE_TYPE (node));
		  tree decl = get_underlying_decl (TREE_TYPE (node));
		  tree enclosing_decl = get_underlying_decl (type);

		  /* For now, handle access-to-access, access-to-empty-struct
		     or access-to-incomplete as opaque system.address.  */
		  if (TREE_CODE (TREE_TYPE (node)) == POINTER_TYPE
		      || (RECORD_OR_UNION_TYPE_P (TREE_TYPE (node))
			  && !TYPE_FIELDS (TREE_TYPE (node)))
		      || !decl
		      || (!enclosing_decl
			  && !TREE_VISITED (decl)
			  && DECL_SOURCE_FILE (decl) == current_source_file)
		      || (enclosing_decl
			  && !TREE_VISITED (decl)
			  && DECL_SOURCE_FILE (decl)
			       == DECL_SOURCE_FILE (enclosing_decl)
			  && decl_sloc (decl, true)
			       > decl_sloc (enclosing_decl, true)))
		    {
		      if (package_prefix)
			{
			  append_withs ("System", false);
			  if (!name_only)
			    pp_string (buffer, "new ");
			  pp_string (buffer, "System.Address");
			}
		      else
			pp_string (buffer, "address");
		      return spc;
		    }

		  if (!package_prefix)
		    pp_string (buffer, "access");
		  else if (AGGREGATE_TYPE_P (TREE_TYPE (node)))
		    {
		      if (!type || TREE_CODE (type) != FUNCTION_DECL)
			{
			  pp_string (buffer, "access ");
			  is_access = true;

			  if (quals & TYPE_QUAL_CONST)
			    pp_string (buffer, "constant ");
			  else if (!name_only)
			    pp_string (buffer, "all ");
			}
		      else if (quals & TYPE_QUAL_CONST)
			pp_string (buffer, "in ");
		      else
			{
			  is_access = true;
			  pp_string (buffer, "access ");
			  /* ??? should be configurable: access or in out.  */
			}
		    }
		  else
		    {
		      is_access = true;
		      pp_string (buffer, "access ");

		      if (!name_only)
			pp_string (buffer, "all ");
		    }

		  if (RECORD_OR_UNION_TYPE_P (TREE_TYPE (node)) && type_name)
		    dump_generic_ada_node (buffer, type_name, TREE_TYPE (node),
					   spc, is_access, true);
		  else
		    dump_generic_ada_node (buffer, TREE_TYPE (node),
					   TREE_TYPE (node), spc, 0, true);
		}
	    }
	}
      break;

    case ARRAY_TYPE:
      if (name_only)
	dump_generic_ada_node
	  (buffer, TYPE_NAME (node), node, spc, limited_access, true);
      else
	dump_ada_array_type (buffer, node, type, spc);
      break;

    case RECORD_TYPE:
    case UNION_TYPE:
      if (name_only)
	{
	  if (TYPE_NAME (node))
	    dump_generic_ada_node
	      (buffer, TYPE_NAME (node), node, spc, limited_access, true);
	  else
	    {
	      pp_string (buffer, "anon_");
	      pp_scalar (buffer, "%d", TYPE_UID (node));
	    }
	}
      else
	dump_ada_struct_decl (buffer, node, type, spc, true);
      break;

    case INTEGER_CST:
      /* We treat the upper half of the sizetype range as negative.  This
	 is consistent with the internal treatment and makes it possible
	 to generate the (0 .. -1) range for flexible array members.  */
      if (TREE_TYPE (node) == sizetype)
	node = fold_convert (ssizetype, node);
      if (tree_fits_shwi_p (node))
	pp_wide_integer (buffer, tree_to_shwi (node));
      else if (tree_fits_uhwi_p (node))
	pp_unsigned_wide_integer (buffer, tree_to_uhwi (node));
      else
	{
	  wide_int val = node;
	  int i;
	  if (wi::neg_p (val))
	    {
	      pp_minus (buffer);
	      val = -val;
	    }
	  sprintf (pp_buffer (buffer)->digit_buffer,
		   "16#%" HOST_WIDE_INT_PRINT "x",
		   val.elt (val.get_len () - 1));
	  for (i = val.get_len () - 2; i >= 0; i--)
	    sprintf (pp_buffer (buffer)->digit_buffer,
		     HOST_WIDE_INT_PRINT_PADDED_HEX, val.elt (i));
	  pp_string (buffer, pp_buffer (buffer)->digit_buffer);
	}
      break;

    case REAL_CST:
    case FIXED_CST:
    case COMPLEX_CST:
    case STRING_CST:
    case VECTOR_CST:
      return 0;

    case TYPE_DECL:
      if (DECL_IS_BUILTIN (node))
	{
	  /* Don't print the declaration of built-in types.  */

	  if (name_only)
	    {
	      /* If we're in the middle of a declaration, defaults to
		 System.Address.  */
	      if (package_prefix)
		{
		  append_withs ("System", false);
		  pp_string (buffer, "System.Address");
		}
	      else
		pp_string (buffer, "address");
	    }
	  break;
	}

      if (name_only)
	dump_ada_decl_name (buffer, node, limited_access);
      else
	{
	  if (is_tagged_type (TREE_TYPE (node)))
	    {
	      int first = 1;

	      /* Look for ancestors.  */
	      for (tree fld = TYPE_FIELDS (TREE_TYPE (node));
		   fld;
		   fld = TREE_CHAIN (fld))
		{
		  if (!DECL_NAME (fld) && is_tagged_type (TREE_TYPE (fld)))
		    {
		      if (first)
			{
			  pp_string (buffer, "limited new ");
			  first = 0;
			}
		      else
			pp_string (buffer, " and ");

		      dump_ada_decl_name (buffer, TYPE_NAME (TREE_TYPE (fld)),
					  false);
		    }
		}

	      pp_string (buffer, first ? "tagged limited " : " with ");
	    }
	  else if (has_nontrivial_methods (TREE_TYPE (node)))
	    pp_string (buffer, "limited ");

	  dump_generic_ada_node
	    (buffer, TREE_TYPE (node), type, spc, false, false);
	}
      break;

    case FUNCTION_DECL:
    case CONST_DECL:
    case VAR_DECL:
    case PARM_DECL:
    case FIELD_DECL:
    case NAMESPACE_DECL:
      dump_ada_decl_name (buffer, node, false);
      break;

    default:
      /* Ignore other nodes (e.g. expressions).  */
      return 0;
    }

  return 1;
}

/* Dump in BUFFER NODE's methods.  SPC is the indentation level.  Return 1 if
   methods were printed, 0 otherwise.  */

static int
dump_ada_methods (pretty_printer *buffer, tree node, int spc)
{
  if (!has_nontrivial_methods (node))
    return 0;

  pp_semicolon (buffer);

  int res = 1;
  for (tree fld = TYPE_FIELDS (node); fld; fld = DECL_CHAIN (fld))
    if (TREE_CODE (fld) == FUNCTION_DECL)
      {
	if (res)
	  {
	    pp_newline (buffer);
	    pp_newline (buffer);
	  }

	res = dump_ada_declaration (buffer, fld, node, spc);
      }

  return 1;
}

static void dump_nested_type (pretty_printer *, tree, tree, tree, int);

/* Dump in BUFFER anonymous types nested inside T's definition.
   PARENT is the parent node of T.
   FORWARD indicates whether a forward declaration of T should be generated.
   SPC is the indentation level.

   In C anonymous nested tagged types have no name whereas in C++ they have
   one.  In C their TYPE_DECL is at top level whereas in C++ it is nested.
   In both languages untagged types (pointers and arrays) have no name.
   In C++ the nested TYPE_DECLs can come after their associated FIELD_DECL.

   Therefore, in order to have a common processing for both languages, we
   disregard anonymous TYPE_DECLs at top level and here we make a first
   pass on the nested TYPE_DECLs and a second pass on the unnamed types.  */

static void
dump_nested_types (pretty_printer *buffer, tree t, tree parent, bool forward,
		   int spc)
{
  tree type, field;

  /* Avoid recursing over the same tree.  */
  if (TREE_VISITED (t))
    return;

  /* Find possible anonymous pointers/arrays/structs/unions recursively.  */
  type = TREE_TYPE (t);
  if (type == NULL_TREE)
    return;

  if (forward)
    {
      pp_string (buffer, "type ");
      dump_generic_ada_node (buffer, t, t, spc, false, true);
      pp_semicolon (buffer);
      newline_and_indent (buffer, spc);
      TREE_VISITED (t) = 1;
    }

  for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
    if (TREE_CODE (field) == TYPE_DECL
	&& DECL_NAME (field) != DECL_NAME (t)
	&& TYPE_NAME (TREE_TYPE (field)) != TYPE_NAME (type))
      dump_nested_type (buffer, field, t, parent, spc);

  for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
    if (TREE_CODE (field) == FIELD_DECL && !TYPE_NAME (TREE_TYPE (field)))
      dump_nested_type (buffer, field, t, parent, spc);

  TREE_VISITED (t) = 1;
}

/* Dump in BUFFER the anonymous type of FIELD inside T.
   PARENT is the parent node of T.
   FORWARD indicates whether a forward declaration of T should be generated.
   SPC is the indentation level.  */

static void
dump_nested_type (pretty_printer *buffer, tree field, tree t, tree parent,
		  int spc)
{
  tree field_type = TREE_TYPE (field);
  tree decl, tmp;

  switch (TREE_CODE (field_type))
    {
    case POINTER_TYPE:
      tmp = TREE_TYPE (field_type);

      if (TREE_CODE (tmp) == FUNCTION_TYPE)
	for (tmp = TREE_TYPE (tmp);
	     tmp && TREE_CODE (tmp) == POINTER_TYPE;
	     tmp = TREE_TYPE (tmp))
	  ;

      decl = get_underlying_decl (tmp);
      if (decl
	  && !DECL_IS_BUILTIN (decl)
	  && (!RECORD_OR_UNION_TYPE_P (TREE_TYPE (decl))
	      || TYPE_FIELDS (TREE_TYPE (decl)))
	  && !TREE_VISITED (decl)
	  && DECL_SOURCE_FILE (decl) == DECL_SOURCE_FILE (t)
	  && decl_sloc (decl, true) > decl_sloc (t, true))
	{
	  /* Generate forward declaration.  */
	  pp_string (buffer, "type ");
	  dump_generic_ada_node (buffer, decl, 0, spc, false, true);
	  pp_semicolon (buffer);
	  newline_and_indent (buffer, spc);
	  TREE_VISITED (decl) = 1;
	}
      break;

    case ARRAY_TYPE:
      tmp = TREE_TYPE (field_type);
      while (TREE_CODE (tmp) == ARRAY_TYPE)
	tmp = TREE_TYPE (tmp);
      decl = get_underlying_decl (tmp);
      if (decl && !DECL_NAME (decl) && !TREE_VISITED (decl))
	{
	  /* Generate full declaration.  */
	  dump_nested_type (buffer, decl, t, parent, spc);
	  TREE_VISITED (decl) = 1;
	}

      /* Special case char arrays.  */
      if (is_char_array (field))
	pp_string (buffer, "sub");

      pp_string (buffer, "type ");
      dump_ada_double_name (buffer, parent, field);
      pp_string (buffer, " is ");
      dump_ada_array_type (buffer, field, parent, spc);
      pp_semicolon (buffer);
      newline_and_indent (buffer, spc);
      break;

    case RECORD_TYPE:
    case UNION_TYPE:
      if (TYPE_NAME (TREE_TYPE (t)) && !TREE_VISITED (t))
	{
	  pp_string (buffer, "type ");
	  dump_generic_ada_node (buffer, t, parent, spc, false, true);
	  pp_semicolon (buffer);
	  newline_and_indent (buffer, spc);
	}

      TREE_VISITED (t) = 1;
      dump_nested_types (buffer, field, t, false, spc);

      pp_string (buffer, "type ");

      if (TYPE_NAME (field_type))
	{
	  dump_generic_ada_node (buffer, field_type, 0, spc, false, true);
	  if (TREE_CODE (field_type) == UNION_TYPE)
	    pp_string (buffer, " (discr : unsigned := 0)");
	  pp_string (buffer, " is ");
	  dump_ada_struct_decl (buffer, field_type, t, spc, false);

	  pp_string (buffer, "pragma Convention (C_Pass_By_Copy, ");
	  dump_generic_ada_node (buffer, field_type, 0, spc, false, true);
	  pp_string (buffer, ");");
	  newline_and_indent (buffer, spc);

	  if (TREE_CODE (field_type) == UNION_TYPE)
	    {
	      pp_string (buffer, "pragma Unchecked_Union (");
	      dump_generic_ada_node (buffer, field_type, 0, spc, false, true);
	      pp_string (buffer, ");");
	    }
	}
      else
	{
	  dump_ada_double_name (buffer, parent, field);
	  if (TREE_CODE (field_type) == UNION_TYPE)
	    pp_string (buffer, " (discr : unsigned := 0)");
	  pp_string (buffer, " is ");
	  dump_ada_struct_decl (buffer, field_type, t, spc, false);

	  pp_string (buffer, "pragma Convention (C_Pass_By_Copy, ");
	  dump_ada_double_name (buffer, parent, field);
	  pp_string (buffer, ");");
	  newline_and_indent (buffer, spc);

	  if (TREE_CODE (field_type) == UNION_TYPE)
	    {
	      pp_string (buffer, "pragma Unchecked_Union (");
	      dump_ada_double_name (buffer, parent, field);
	      pp_string (buffer, ");");
	    }
	}

    default:
      break;
    }
}

/* Dump in BUFFER constructor spec corresponding to T for TYPE.  */

static void
print_constructor (pretty_printer *buffer, tree t, tree type)
{
  tree decl_name = DECL_NAME (TYPE_NAME (type));

  pp_string (buffer, "New_");
  pp_ada_tree_identifier (buffer, decl_name, t, 0, false);
}

/* Dump in BUFFER destructor spec corresponding to T.  */

static void
print_destructor (pretty_printer *buffer, tree t, tree type)
{
  tree decl_name = DECL_NAME (TYPE_NAME (type));

  pp_string (buffer, "Delete_");
  pp_ada_tree_identifier (buffer, decl_name, t, 0, false);
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

/* Dump in BUFFER the declaration of a variable T of type TYPE in Ada syntax.
   SPC is the indentation level.  Return 1 if a declaration was printed,
   0 otherwise.  */

static int
dump_ada_declaration (pretty_printer *buffer, tree t, tree type, int spc)
{
  int is_var = 0, need_indent = 0;
  int is_class = false;
  tree name = TYPE_NAME (TREE_TYPE (t));
  tree decl_name = DECL_NAME (t);
  tree orig = NULL_TREE;

  if (cpp_check && cpp_check (t, IS_TEMPLATE))
    return dump_ada_template (buffer, t, spc);

  if (TREE_CODE (t) == CONST_DECL && TREE_CODE (TREE_TYPE (t)) == ENUMERAL_TYPE)
    /* Skip enumeral values: will be handled as part of the type itself.  */
    return 0;

  if (TREE_CODE (t) == TYPE_DECL)
    {
      orig = DECL_ORIGINAL_TYPE (t);

      if (orig && TYPE_STUB_DECL (orig))
	{
	  tree stub = TYPE_STUB_DECL (orig);
	  tree typ = TREE_TYPE (stub);

	  if (TYPE_NAME (typ))
	    {
	      /* If types have same representation, and same name (ignoring
		 casing), then ignore the second type.  */
	      if (type_name (typ) == type_name (TREE_TYPE (t))
		  || !strcasecmp (type_name (typ), type_name (TREE_TYPE (t))))
		{
		  TREE_VISITED (t) = 1;
		  return 0;
		}

	      INDENT (spc);

	      if (RECORD_OR_UNION_TYPE_P (typ) && !TYPE_FIELDS (typ))
		{
		  pp_string (buffer, "--  skipped empty struct ");
		  dump_generic_ada_node (buffer, t, type, spc, false, true);
		}
	      else
		{
		  if (RECORD_OR_UNION_TYPE_P (typ)
		      && DECL_SOURCE_FILE (stub) == current_source_file)
		    dump_nested_types (buffer, stub, stub, true, spc);

		  pp_string (buffer, "subtype ");
		  dump_generic_ada_node (buffer, t, type, spc, false, true);
		  pp_string (buffer, " is ");
		  dump_generic_ada_node (buffer, typ, type, spc, false, true);
		  pp_string (buffer, ";  -- ");
		  dump_sloc (buffer, t);
		}

	      TREE_VISITED (t) = 1;
	      return 1;
	    }
	}

      /* Skip unnamed or anonymous structs/unions/enum types.  */
      if (!orig && !decl_name && !name
	  && (RECORD_OR_UNION_TYPE_P (TREE_TYPE (t))
	      || TREE_CODE (TREE_TYPE (t)) == ENUMERAL_TYPE))
	return 0;

	/* Skip anonymous enum types (duplicates of real types).  */
      if (!orig
	  && TREE_CODE (TREE_TYPE (t)) == ENUMERAL_TYPE
	  && decl_name
	  && (*IDENTIFIER_POINTER (decl_name) == '.'
	      || *IDENTIFIER_POINTER (decl_name) == '$'))
	return 0;

      INDENT (spc);

      switch (TREE_CODE (TREE_TYPE (t)))
	{
	  case RECORD_TYPE:
	  case UNION_TYPE:
	    /* Skip empty structs (typically forward references to real
	       structs).  */
	    if (!TYPE_FIELDS (TREE_TYPE (t)))
	      {
		pp_string (buffer, "--  skipped empty struct ");
		dump_generic_ada_node (buffer, t, type, spc, false, true);
		return 1;
	      }

	    if (decl_name
		&& (*IDENTIFIER_POINTER (decl_name) == '.'
		    || *IDENTIFIER_POINTER (decl_name) == '$'))
	      {
		pp_string (buffer, "--  skipped anonymous struct ");
		dump_generic_ada_node (buffer, t, type, spc, false, true);
		TREE_VISITED (t) = 1;
		return 1;
	      }

	    if (orig && TYPE_NAME (orig) && orig != TREE_TYPE (t))
	      pp_string (buffer, "subtype ");
	    else
	      {
		dump_nested_types (buffer, t, t, false, spc);

                if (separate_class_package (t))
		  {
		    is_class = true;
		    pp_string (buffer, "package Class_");
		    dump_generic_ada_node (buffer, t, type, spc, false, true);
		    pp_string (buffer, " is");
		    spc += INDENT_INCR;
		    newline_and_indent (buffer, spc);
		  }

		pp_string (buffer, "type ");
	      }
	    break;

	  case ARRAY_TYPE:
	  case POINTER_TYPE:
	  case REFERENCE_TYPE:
	    if ((orig && TYPE_NAME (orig) && orig != TREE_TYPE (t))
		|| is_char_array (t))
	      pp_string (buffer, "subtype ");
	    else
	      pp_string (buffer, "type ");
	    break;

	  case FUNCTION_TYPE:
	    pp_string (buffer, "--  skipped function type ");
	    dump_generic_ada_node (buffer, t, type, spc, false, true);
	    return 1;

	  case ENUMERAL_TYPE:
	    if ((orig && TYPE_NAME (orig) && orig != TREE_TYPE (t))
		|| !is_simple_enum (TREE_TYPE (t)))
	      pp_string (buffer, "subtype ");
	    else
	      pp_string (buffer, "type ");
	    break;

	  default:
	    pp_string (buffer, "subtype ");
	}
      TREE_VISITED (t) = 1;
    }
  else
    {
      if (VAR_P (t)
	  && decl_name
	  && *IDENTIFIER_POINTER (decl_name) == '_')
	return 0;

      need_indent = 1;
    }

  /* Print the type and name.  */
  if (TREE_CODE (TREE_TYPE (t)) == ARRAY_TYPE)
    {
      if (need_indent)
	INDENT (spc);

      /* Print variable's name.  */
      dump_generic_ada_node (buffer, t, type, spc, false, true);

      if (TREE_CODE (t) == TYPE_DECL)
	{
	  pp_string (buffer, " is ");

	  if (orig && TYPE_NAME (orig) && orig != TREE_TYPE (t))
	    dump_generic_ada_node
	      (buffer, TYPE_NAME (orig), type, spc, false, true);
	  else
	    dump_ada_array_type (buffer, t, type, spc);
	}
      else
	{
	  tree tmp = TYPE_NAME (TREE_TYPE (t));

	  if (spc == INDENT_INCR || TREE_STATIC (t))
	    is_var = 1;

	  pp_string (buffer, " : ");

	  if (TREE_CODE (TREE_TYPE (TREE_TYPE (t))) != POINTER_TYPE)
	    pp_string (buffer, "aliased ");

	  if (tmp)
	    dump_generic_ada_node (buffer, tmp, type, spc, false, true);
	  else if (type)
	    dump_ada_double_name (buffer, type, t);
	  else
	    dump_ada_array_type (buffer, t, type, spc);
	}
    }
  else if (TREE_CODE (t) == FUNCTION_DECL)
    {
      bool is_abstract_class = false;
      bool is_method = TREE_CODE (TREE_TYPE (t)) == METHOD_TYPE;
      tree decl_name = DECL_NAME (t);
      bool is_abstract = false;
      bool is_constructor = false;
      bool is_destructor = false;
      bool is_copy_constructor = false;
      bool is_move_constructor = false;

      if (!decl_name)
	return 0;

      if (cpp_check)
	{
	  is_abstract = cpp_check (t, IS_ABSTRACT);
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

	  /* Only consider constructors/destructors for complete objects.  */
	  if (strncmp (IDENTIFIER_POINTER (decl_name), "__ct_comp", 9) != 0
	      && strncmp (IDENTIFIER_POINTER (decl_name), "__dt_comp", 9) != 0)
	    return 0;
	}

      /* If this function has an entry in the vtable, we cannot omit it.  */
      else if (!DECL_VINDEX (t) && *IDENTIFIER_POINTER (decl_name) == '_')
	{
	  INDENT (spc);
	  pp_string (buffer, "--  skipped func ");
	  pp_string (buffer, IDENTIFIER_POINTER (decl_name));
	  return 1;
	}

      if (need_indent)
	INDENT (spc);

      if (VOID_TYPE_P (TREE_TYPE (TREE_TYPE (t))) && !is_constructor)
	pp_string (buffer, "procedure ");
      else
	pp_string (buffer, "function ");

      if (is_constructor)
	print_constructor (buffer, t, type);
      else if (is_destructor)
	print_destructor (buffer, t, type);
      else
	dump_ada_decl_name (buffer, t, false);

      dump_ada_function_declaration
	(buffer, t, is_method, is_constructor, is_destructor, spc);

      if (is_constructor && RECORD_OR_UNION_TYPE_P (type))
	for (tree fld = TYPE_FIELDS (type); fld; fld = DECL_CHAIN (fld))
	  if (TREE_CODE (fld) == FUNCTION_DECL && cpp_check (fld, IS_ABSTRACT))
	    {
	      is_abstract_class = true;
	      break;
	    }

      if (is_abstract || is_abstract_class)
	pp_string (buffer, " is abstract");

      pp_semicolon (buffer);
      pp_string (buffer, "  -- ");
      dump_sloc (buffer, t);

      if (is_abstract || !DECL_ASSEMBLER_NAME (t))
	return 1;

      newline_and_indent (buffer, spc);

      if (is_constructor)
	{
	  pp_string (buffer, "pragma CPP_Constructor (");
	  print_constructor (buffer, t, type);
	  pp_string (buffer, ", \"");
	  pp_asm_name (buffer, t);
	  pp_string (buffer, "\");");
	}
      else if (is_destructor)
	{
	  pp_string (buffer, "pragma Import (CPP, ");
	  print_destructor (buffer, t, type);
	  pp_string (buffer, ", \"");
	  pp_asm_name (buffer, t);
	  pp_string (buffer, "\");");
	}
      else
	dump_ada_import (buffer, t);

      return 1;
    }
  else if (TREE_CODE (t) == TYPE_DECL && !DECL_ORIGINAL_TYPE (t))
    {
      int is_interface = 0;
      int is_abstract_record = 0;

      if (need_indent)
	INDENT (spc);

      /* Anonymous structs/unions */
      dump_generic_ada_node (buffer, TREE_TYPE (t), t, spc, false, true);

      if (TREE_CODE (TREE_TYPE (t)) == UNION_TYPE)
	{
	  pp_string (buffer, " (discr : unsigned := 0)");
	}

      pp_string (buffer, " is ");

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
		    is_interface = 1;
		  else
		    is_interface = 0;
		  has_fields = true;
		}
	      else if (TREE_CODE (fld) == FUNCTION_DECL
		       && !DECL_ARTIFICIAL (fld))
		{
		  if (cpp_check (fld, IS_ABSTRACT))
		    is_abstract_record = 1;
		  else
		    is_interface = 0;
		}
	    }
	}

      TREE_VISITED (t) = 1; 
      if (is_interface)
	{
	  pp_string (buffer, "limited interface;  -- ");
	  dump_sloc (buffer, t);
	  newline_and_indent (buffer, spc);
	  pp_string (buffer, "pragma Import (CPP, ");
 	  dump_generic_ada_node
	    (buffer, TYPE_NAME (TREE_TYPE (t)), type, spc, false, true);
  	  pp_right_paren (buffer);

	  dump_ada_methods (buffer, TREE_TYPE (t), spc);
	}
      else
	{
	  if (is_abstract_record)
	    pp_string (buffer, "abstract ");
	  dump_generic_ada_node (buffer, t, t, spc, false, false);
	}
    }
  else
    {
      if (need_indent)
	INDENT (spc);

      if (TREE_CODE (t) == FIELD_DECL && DECL_NAME (t))
	check_name (buffer, t);

      /* Print variable/type's name.  */
      dump_generic_ada_node (buffer, t, t, spc, false, true);

      if (TREE_CODE (t) == TYPE_DECL)
	{
	  tree orig = DECL_ORIGINAL_TYPE (t);
	  int is_subtype = orig && TYPE_NAME (orig) && orig != TREE_TYPE (t);

	  if (!is_subtype && TREE_CODE (TREE_TYPE (t)) == UNION_TYPE)
	    pp_string (buffer, " (discr : unsigned := 0)");

	  pp_string (buffer, " is ");

	  dump_generic_ada_node (buffer, orig, t, spc, false, is_subtype);
	}
      else
	{
	  if (spc == INDENT_INCR || TREE_STATIC (t))
	    is_var = 1;

	  pp_string (buffer, " : ");

	  if (RECORD_OR_UNION_TYPE_P (TREE_TYPE (t)))
	    {
	      pp_string (buffer, "aliased ");

	      if (TREE_READONLY (t))
		pp_string (buffer, "constant ");

	      if (TYPE_NAME (TREE_TYPE (t)))
		dump_generic_ada_node
		  (buffer, TREE_TYPE (t), t, spc, false, true);
	      else if (type)
		dump_ada_double_name (buffer, type, t);
	    }
	  else
	    {
	      if (TREE_CODE (TREE_TYPE (t)) != POINTER_TYPE
		  && (TYPE_NAME (TREE_TYPE (t))
		      || TREE_CODE (TREE_TYPE (t)) != INTEGER_TYPE))
		pp_string (buffer, "aliased ");

	      if (TREE_READONLY (t))
		pp_string (buffer, "constant ");

	      dump_generic_ada_node
		(buffer, TREE_TYPE (t), TREE_TYPE (t), spc, false, true);
	    }
	}
    }

  if (is_class)
    {
      spc -= INDENT_INCR;
      newline_and_indent (buffer, spc);
      pp_string (buffer, "end;");
      newline_and_indent (buffer, spc);
      pp_string (buffer, "use Class_");
      dump_generic_ada_node (buffer, t, type, spc, false, true);
      pp_semicolon (buffer);
      pp_newline (buffer);

      /* All needed indentation/newline performed already, so return 0.  */
      return 0;
    }
  else
    {
      pp_string (buffer, ";  -- ");
      dump_sloc (buffer, t);
    }

  if (is_var)
    {
      newline_and_indent (buffer, spc);
      dump_ada_import (buffer, t);
    }

  return 1;
}

/* Dump in BUFFER a structure NODE of type TYPE: name, fields, and methods
   with Ada syntax.  SPC is the indentation level.  If DISPLAY_CONVENTION is
   true, also print the pragma Convention for NODE.  */

static void
dump_ada_struct_decl (pretty_printer *buffer, tree node, tree type, int spc,
		       bool display_convention)
{
  tree tmp;
  const bool is_union = (TREE_CODE (node) == UNION_TYPE);
  char buf[32];
  int field_num = 0;
  int field_spc = spc + INDENT_INCR;
  int need_semicolon;

  bitfield_used = false;

  if (TYPE_FIELDS (node))
    {
      /* Print the contents of the structure.  */
      pp_string (buffer, "record");

      if (is_union)
	{
	  newline_and_indent (buffer, spc + INDENT_INCR);
	  pp_string (buffer, "case discr is");
	  field_spc = spc + INDENT_INCR * 3;
	}

      pp_newline (buffer);

      /* Print the non-static fields of the structure.  */
      for (tmp = TYPE_FIELDS (node); tmp; tmp = TREE_CHAIN (tmp))
	{
	  /* Add parent field if needed.  */
	  if (!DECL_NAME (tmp))
	    {
	      if (!is_tagged_type (TREE_TYPE (tmp)))
		{
		  if (!TYPE_NAME (TREE_TYPE (tmp)))
		    dump_ada_declaration (buffer, tmp, type, field_spc);
		  else
		    {
		      INDENT (field_spc);

		      if (field_num == 0)
			pp_string (buffer, "parent : aliased ");
		      else
			{
			  sprintf (buf, "field_%d : aliased ", field_num + 1);
			  pp_string (buffer, buf);
			}
		      dump_ada_decl_name
			(buffer, TYPE_NAME (TREE_TYPE (tmp)), false);
		      pp_semicolon (buffer);
		    }
		  pp_newline (buffer);
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
		      pp_string (buffer, buf);
		      pp_newline (buffer);
		    }

		  if (dump_ada_declaration (buffer, tmp, type, field_spc))
		    {
		      pp_newline (buffer);
		      field_num++;
		    }
		}
	    }
	}

      if (is_union)
	{
	  INDENT (spc + INDENT_INCR);
	  pp_string (buffer, "end case;");
	  pp_newline (buffer);
	}

      if (field_num == 0)
	{
	  INDENT (spc + INDENT_INCR);
	  pp_string (buffer, "null;");
	  pp_newline (buffer);
	}

      INDENT (spc);
      pp_string (buffer, "end record;");
    }
  else
    pp_string (buffer, "null record;");

  newline_and_indent (buffer, spc);

  if (!display_convention)
    return;

  if (RECORD_OR_UNION_TYPE_P (TREE_TYPE (type)))
    {
      if (has_nontrivial_methods (TREE_TYPE (type)))
	pp_string (buffer, "pragma Import (CPP, ");
      else
	pp_string (buffer, "pragma Convention (C_Pass_By_Copy, ");
    }
  else
    pp_string (buffer, "pragma Convention (C, ");

  package_prefix = false;
  dump_generic_ada_node (buffer, TREE_TYPE (type), type, spc, false, true);
  package_prefix = true;
  pp_right_paren (buffer);

  if (is_union)
    {
      pp_semicolon (buffer);
      newline_and_indent (buffer, spc);
      pp_string (buffer, "pragma Unchecked_Union (");

      dump_generic_ada_node (buffer, TREE_TYPE (type), type, spc, false, true);
      pp_right_paren (buffer);
    }

  if (bitfield_used)
    {
      pp_semicolon (buffer);
      newline_and_indent (buffer, spc);
      pp_string (buffer, "pragma Pack (");
      dump_generic_ada_node
	(buffer, TREE_TYPE (type), type, spc, false, true);
      pp_right_paren (buffer);
      bitfield_used = false;
    }

  need_semicolon = !dump_ada_methods (buffer, node, spc);

  /* Print the static fields of the structure, if any.  */
  for (tmp = TYPE_FIELDS (node); tmp; tmp = TREE_CHAIN (tmp))
    {
      if (TREE_CODE (tmp) == VAR_DECL && DECL_NAME (tmp))
	{
	  if (need_semicolon)
	    {
	      need_semicolon = false;
	      pp_semicolon (buffer);
	    }
	  pp_newline (buffer);
	  pp_newline (buffer);
	  dump_ada_declaration (buffer, tmp, type, spc);
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
      pp.buffer->stream = f;

      /* Dump all relevant macros.  */
      dump_ada_macros (&pp, source_file);

      /* Reset the table of withs for this file.  */
      reset_ada_withs ();

      (*collect_all_refs) (source_file);

      /* Dump all references.  */
      cpp_check = check;
      dump_ada_nodes (&pp, source_file);

      /* Requires Ada 2005 syntax, so generate corresponding pragma.
         Also, disable style checks since this file is auto-generated.  */
      fprintf (f, "pragma Ada_2005;\npragma Style_Checks (Off);\n\n");

      /* Dump withs.  */
      dump_ada_withs (f);

      fprintf (f, "\npackage %s is\n\n", pkg_name);
      pp_write_text_to_stream (&pp);
      /* ??? need to free pp */
      fprintf (f, "end %s;\n", pkg_name);
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
  /* Iterate over the list of files to dump specs for.  */
  for (int i = 0; i < source_refs_used; i++)
    dump_ads (source_refs[i], collect_all_refs, check);

  /* Free various tables.  */
  free (source_refs);
  delete overloaded_names;
}
