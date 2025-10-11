/* Lowering routines for all things related to STRINGs.
   Copyright (C) 2025 Jose E. Marchesi.

   Written by Jose E. Marchesi.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"

#include "tree.h"
#include "fold-const.h"
#include "diagnostic.h"
#include "langhooks.h"
#include "tm.h"
#include "function.h"
#include "cgraph.h"
#include "toplev.h"
#include "varasm.h"
#include "predict.h"
#include "stor-layout.h"
#include "tree-iterator.h"
#include "stringpool.h"
#include "print-tree.h"
#include "gimplify.h"
#include "dumpfile.h"
#include "convert.h"

#include "a68.h"

/* Return a tree with the yielding of SKIP for M_STRING.  */

tree
a68_get_string_skip_tree (void)
{
  return a68_get_multiple_skip_tree (M_FLEX_ROW_CHAR);
}

/* Copy chars from STR to ELEMENTS starting at TO_INDEX chars in ELEMENTS.  */

static void
copy_string (tree elements, tree to_index, tree str)
{
  tree char_pointer_type = build_pointer_type (a68_char_type);
  tree num_elems
    = a68_lower_tmpvar ("num_elems%", sizetype, a68_multiple_num_elems (str));

  tree from_index
    = a68_lower_tmpvar ("from_index%", sizetype, size_zero_node);
  tree from_offset
    = a68_lower_tmpvar ("from_offset%", sizetype, size_zero_node);

  /* Begin of loop body.  */
  a68_push_range (NULL);
  {
    /* if (from_index == num_elems) break;  */
    a68_add_stmt (fold_build1 (EXIT_EXPR, void_type_node,
			       fold_build2 (GE_EXPR, sizetype,
					    from_index, num_elems)));

    /* *(elements + to_index) = *(elements + from_index)  */
    tree to_offset = fold_build2 (MULT_EXPR, sizetype,
				  to_index, size_in_bytes (a68_char_type));
    a68_add_stmt (fold_build2 (MODIFY_EXPR,
			       void_type_node,
			       fold_build2 (MEM_REF, a68_char_type,
					    fold_build2 (POINTER_PLUS_EXPR,
							 char_pointer_type,
							 elements, to_offset),
					    fold_convert (char_pointer_type,
							  integer_zero_node)),
			       fold_build2 (MEM_REF, a68_char_type,
					    fold_build2 (POINTER_PLUS_EXPR,
							 char_pointer_type,
							 a68_multiple_elements (str),
							 from_offset),
					    fold_convert (char_pointer_type,
							  integer_zero_node))));

    /* from_offset = from_offset + stride */
    a68_add_stmt (fold_build2 (MODIFY_EXPR, void_type_node,
			       from_offset,
			       fold_build2 (PLUS_EXPR, sizetype,
					    from_offset,
					    a68_multiple_stride (str, size_zero_node))));
    /* to_index = to_index + 1 */
    a68_add_stmt (fold_build2 (POSTINCREMENT_EXPR, sizetype, to_index, size_one_node));

    /* from_index = from_index + 1 */
    a68_add_stmt (fold_build2 (POSTINCREMENT_EXPR, sizetype, from_index, size_one_node));
  }

  /* End of loop body.  */
  tree loop_body = a68_pop_range ();
  a68_add_stmt (fold_build1 (LOOP_EXPR,
			     void_type_node,
			     loop_body));
}

/* Given two STRINGs STR1 and STR2, allocate a new string on the stack with a
   copy of the concatenated characters of the given string.  */

tree
a68_string_concat (tree str1, tree str2)
{
  tree char_pointer_type = build_pointer_type (a68_char_type);
  static tree string_concat_fndecl;

  if (string_concat_fndecl == NULL_TREE)
    {
      string_concat_fndecl
	= a68_low_toplevel_func_decl ("string_concat",
				      build_function_type_list (char_pointer_type,
								TREE_TYPE (str1),
								TREE_TYPE (str2),
								NULL_TREE));
      announce_function (string_concat_fndecl);

      tree s1 = a68_low_func_param (string_concat_fndecl, "s1", TREE_TYPE (str1));
      tree s2 = a68_low_func_param (string_concat_fndecl, "s2", TREE_TYPE (str2));
      DECL_ARGUMENTS (string_concat_fndecl) = chainon (s1, s2);

      a68_push_function_range (string_concat_fndecl, char_pointer_type,
			       true /* top_level */);

      tree n1 = a68_lower_tmpvar ("n1%", sizetype, a68_multiple_num_elems (s1));
      tree n2 = a68_lower_tmpvar ("n2%", sizetype, a68_multiple_num_elems (s2));
      tree num_elems = a68_lower_tmpvar ("num_elems%", sizetype,
					 fold_build2 (PLUS_EXPR, sizetype, n1, n2));

      /* First allocate memory for the result string.  We need enough space to
	 hold the elements of both strings with a stride of 1S.  */
      tree char_pointer_type = build_pointer_type (a68_char_type);
      tree elements_size = fold_build2 (MULT_EXPR, sizetype,
					size_in_bytes (a68_char_type),
					num_elems);
      tree elements = a68_lower_tmpvar ("elements%", char_pointer_type,
					a68_lower_malloc (a68_char_type, elements_size));

      /* Copy elements.  */
      tree to_index = a68_lower_tmpvar ("to_index%", sizetype, size_zero_node);
      copy_string (elements, to_index, s1);
      copy_string (elements, to_index, s2);
      a68_pop_function_range (elements);
    }

  /* Build the resulting multiple.  */
  str1 = save_expr (str1);
  str2 = save_expr (str2);
  tree n1 = a68_multiple_num_elems (str1);
  tree n2 = a68_multiple_num_elems (str2);
  tree num_elems = save_expr (fold_build2 (PLUS_EXPR, sizetype, n1, n2));
  tree elements_size = fold_build2 (MULT_EXPR, sizetype,
				    size_in_bytes (a68_char_type),
				    num_elems);
  tree lower_bound = ssize_int (1);
  tree upper_bound = fold_convert (ssizetype, num_elems);
  tree elements = build_call_nary (char_pointer_type,
				   fold_build1 (ADDR_EXPR,
						build_pointer_type (TREE_TYPE (string_concat_fndecl)),
						string_concat_fndecl),
				   2, str1, str2);
  return a68_row_value (CTYPE (M_STRING), 1 /* dim */,
			elements, elements_size,
			&lower_bound, &upper_bound);
}

/* Given a STRING STR and an INT FACTOR, return STRING concatenated to itself
   FACTOR - 1 times.

   Negative values of FACTOR are interpreted as zero.  */

tree
a68_string_mult (tree str, tree factor)
{
  a68_push_range (M_STRING);

  str = save_expr (str);
  tree ssize_one_node = ssize_int (1);
  tree res = a68_lower_tmpvar ("res%", CTYPE (M_STRING), str);
  tree index = a68_lower_tmpvar ("index%", ssizetype, ssize_one_node);

  /* Begin of loop body.  */
  a68_push_range (NULL);

  /* if (index == FACTOR) break;  */
  a68_add_stmt (fold_build1 (EXIT_EXPR,
			     void_type_node,
			     fold_build2 (GE_EXPR, ssizetype,
					  index,
					  fold_convert (ssizetype, factor))));

  /* res += str */
  a68_add_stmt (fold_build2 (MODIFY_EXPR, TREE_TYPE (res),
			     res,
			     a68_string_concat (res, str)));

  /* index++ */
  a68_add_stmt (fold_build2 (POSTINCREMENT_EXPR,
			     ssizetype,
			     index, ssize_one_node));
  tree loop_body = a68_pop_range ();
  /* End of loop body.  */
  a68_add_stmt (fold_build1 (LOOP_EXPR,
			     void_type_node,
			     loop_body));
  a68_add_stmt (res);
  return a68_pop_range ();
}

/* Given a CHAR C, build a string whose contents are just that CHAR.  */

tree
a68_string_from_char (tree c)
{
  tree lower_bound = ssize_int (1);
  tree upper_bound = lower_bound;
  tree char_pointer_type = build_pointer_type (a68_char_type);

  a68_push_range (M_STRING);

  tree elements = a68_lower_tmpvar ("elements%", char_pointer_type,
				    a68_lower_malloc (a68_char_type,
						      size_one_node));
  a68_add_stmt (fold_build2 (MODIFY_EXPR,
			     void_type_node,
			     fold_build1 (INDIRECT_REF, a68_char_type, elements),
			     c));
  a68_add_stmt (a68_row_value (CTYPE (M_STRING), 1 /* dim */,
			       elements,
			       size_in_bytes (a68_char_type),
			       &lower_bound, &upper_bound));
  return a68_pop_range ();
}

/* Compare the two given strings lexicographically and return -1 (less than), 0
   (equal to) or 1 (bigger than) reflecting the result of the comparison.  */

tree
a68_string_cmp (tree s1, tree s2)
{
  s1 = save_expr (s1);
  tree s1_elems = a68_multiple_elements (s1);
  tree s1_len = a68_multiple_num_elems (s1);
  tree s1_stride = a68_multiple_stride (s1, size_zero_node);

  s2 = save_expr (s2);
  tree s2_elems = a68_multiple_elements (s2);
  tree s2_len = a68_multiple_num_elems (s2);
  tree s2_stride = a68_multiple_stride (s2, size_zero_node);

  return a68_build_libcall (A68_LIBCALL_U32_CMP2,
			    a68_int_type, 6,
			    s1_elems, s1_len, s1_stride,
			    s2_elems, s2_len, s2_stride);
}

/* Return a newly allocated UTF-8 string resulting from processing the string
   breaks in STR.  This function assumes the passed string is well-formed (the
   scanner is in charge of seeing that is true) and just ICEs if it is not.
   NODE is used as the location for diagnostics in case the string breaks
   contain some invalid data.  */

char *
a68_string_process_breaks (NODE_T *node, const char *str)
{
  size_t len = 0;
  char *res = NULL;

  /* First calculate the size of the resulting string.  */
  for (const char *p = str; *p != '\0';)
    {
      if (*p == '\'')
	{
	  switch (p[1])
	    {
	    case '\'':
	    case 'n':
	    case 'f':
	    case 'r':
	    case 't':
	      len += 1;
	      p += 2;
	      break;
	    case '(':
	      p += 2;
	      while (1)
		{
		  if (p[0] == ')')
		    {
		      p++;
		      break;
		    }
		  else if (p[0] == ',' || ISSPACE (p[0]))
		    {
		      p++;
		      continue;
		    }

		  /* An Unicode codepoint encoded in UTF-8 occupies at most six
		     octets.  */
		  len += 6;
		  p += (p[0] == 'u' ? 5 : 9);
		}
	      break;
	    default:
	      gcc_unreachable ();
	    }
	}
      else
	{
	  len += 1;
	  p += 1;
	}
    }

  /* Now and allocate it, adding space for a trailing NULL.  */
  res = (char *) xmalloc (len + 1);

  /* Finally fill it with the result of expanding all the string breaks.  */
  size_t offset = 0;
  for (const char *p = str; *p != '\0';)
    {
      if (*p == '\'')
	{
	  switch (p[1])
	    {
	    case '\'': res[offset] = '\''; p += 2; offset += 1; break;
	    case 'n': res[offset] = '\n'; p += 2;  offset += 1; break;
	    case 't': res[offset] = '\t'; p += 2;  offset += 1; break;
	    case 'r': res[offset] = '\r'; p += 2;  offset += 1; break;
	    case 'f': res[offset] = '\f'; p += 2;  offset += 1; break;
	    case '(':
	      {
		p += 2;
		while (1)
		  {
		    if (p[0] == ')')
		      {
			p++;
			break;
		      }
		    else if (p[0] == ',' || ISSPACE (p[0]))
		      {
			p++;
			continue;
		      }

		    /* Skip the u or U.  */
		    gcc_assert (p[0] == 'u' || p[0] == 'U');
		    p++;

		    const char *begin = p;
		    char *end;
		    int64_t codepoint = strtol (p, &end, 16);
		    gcc_assert (end > p);
		    p = end;
		    /* Append the UTF-8 encoding of the obtained codepoint to
		       the `res' string.  */
		    int n = a68_u8_uctomb ((uint8_t *) res + offset, codepoint, 6);
		    if (n < 0)
		      {
			char *start = CHAR_IN_LINE (INFO (node)) + (begin - str);
			a68_scan_error (LINE (INFO (node)), start,
					"invalid Unicode codepoint in string literal");
		      }

		    offset += n;
		  }
		break;
	      }
	    default: gcc_unreachable ();
	    }
	}
      else
	{
	  res[offset] = *p;
	  offset += 1;
	  p += 1;
	}
    }
  res[offset] = '\0';

  return res;
}
