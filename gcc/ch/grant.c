/* Implement grant-file output & seize-file input for CHILL.
   Copyright (C) 1992, 1993, 1994, 1995, 1996, 1998,
   1999, 2000 Free Software Foundation, Inc.

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
#include "flags.h"
#include "actions.h"
#include "input.h"
#include "rtl.h"
#include "tasking.h"
#include "toplev.h"
#include "output.h"

#define APPEND(X,Y) X = append (X, Y)
#define PREPEND(X,Y) X = prepend (X, Y);
#define FREE(x) strfree (x)
#define ALLOCAMOUNT	10000
/* may be we can handle this in a more exciting way,
   but this also should work for the moment */
#define MAYBE_NEWLINE(X)                       \
do                                             \
{                                              \
  if (X->len && X->str[X->len - 1] != '\n')    \
    APPEND (X, ";\n");                         \
} while (0)

extern tree process_type;
extern char *asm_file_name;
extern char *dump_base_name;

/* forward declarations */

/* variable indicates compilation at module level */
int chill_at_module_level = 0;


/* mark that a SPEC MODULE was generated */
static int spec_module_generated = 0;

/* define a faster string handling */
typedef struct
{
  char	*str;
  int		len;
  int		allocated;
} MYSTRING;

/* structure used for handling multiple grant files */
char	*grant_file_name;
MYSTRING	*gstring = NULL;
MYSTRING        *selective_gstring = NULL;

static MYSTRING *decode_decl                PARAMS ((tree));
static MYSTRING *decode_constant            PARAMS ((tree));
static void      grant_one_decl             PARAMS ((tree));
static MYSTRING *get_type                   PARAMS ((tree));
static MYSTRING *decode_mode                PARAMS ((tree));
static MYSTRING *decode_prefix_rename       PARAMS ((tree));
static MYSTRING *decode_constant_selective  PARAMS ((tree, tree));
static MYSTRING *decode_mode_selective      PARAMS ((tree, tree));
static MYSTRING *get_type_selective         PARAMS ((tree, tree));
static MYSTRING *decode_decl_selective      PARAMS ((tree, tree));
static MYSTRING *newstring                  PARAMS ((const char *));
static void strfree                         PARAMS ((MYSTRING *));
static MYSTRING *append                     PARAMS ((MYSTRING *, const char *));
static MYSTRING *prepend                    PARAMS ((MYSTRING *, const char *));
static void grant_use_seizefile             PARAMS ((const char *));
static MYSTRING *decode_layout              PARAMS ((tree));
static MYSTRING *grant_array_type           PARAMS ((tree));
static MYSTRING *grant_array_type_selective PARAMS ((tree, tree));
static MYSTRING *get_tag_value              PARAMS ((tree));
static MYSTRING *get_tag_value_selective    PARAMS ((tree, tree));
static MYSTRING *print_enumeral             PARAMS ((tree));
static MYSTRING *print_enumeral_selective   PARAMS ((tree, tree));
static MYSTRING *print_integer_type         PARAMS ((tree));
static tree find_enum_parent                PARAMS ((tree, tree));
static MYSTRING *print_integer_selective    PARAMS ((tree, tree));
static MYSTRING *print_struct               PARAMS ((tree));
static MYSTRING *print_struct_selective     PARAMS ((tree, tree));
static MYSTRING *print_proc_exceptions      PARAMS ((tree));
static MYSTRING *print_proc_tail            PARAMS ((tree, tree, int));
static MYSTRING *print_proc_tail_selective  PARAMS ((tree, tree, tree));
static tree find_in_decls                   PARAMS ((tree, tree));
static int in_ridpointers                   PARAMS ((tree));
static void grant_seized_identifier         PARAMS ((tree));
static void globalize_decl                  PARAMS ((tree));
static void grant_one_decl_selective        PARAMS ((tree, tree));
static int compare_memory_file              PARAMS ((const char *, const char *));
static int search_in_list                   PARAMS ((tree, tree));
static int really_grant_this                PARAMS ((tree, tree));

/* list of the VAR_DECLs of the module initializer entries */
tree      module_init_list = NULL_TREE;

/* handle different USE_SEIZE_FILE's in case of selective granting */
typedef struct SEIZEFILELIST
{
  struct SEIZEFILELIST *next;
  tree filename;
  MYSTRING *seizes;
} seizefile_list;

static seizefile_list *selective_seizes = 0;


static MYSTRING *
newstring (str)
    const char	*str;
{
    MYSTRING	*tmp = (MYSTRING *) xmalloc (sizeof (MYSTRING));
    unsigned	len = strlen (str);
    
    tmp->allocated = len + ALLOCAMOUNT;
    tmp->str = xmalloc ((unsigned)tmp->allocated);
    strcpy (tmp->str, str);
    tmp->len = len;
    return (tmp);
}

static void
strfree (str)
    MYSTRING	*str;
{
    free (str->str);
    free (str);
}

static MYSTRING *
append (inout, in)
    MYSTRING	*inout;
    const char	*in;
{
    int	inlen = strlen (in);
    int amount = ALLOCAMOUNT;

    if (inlen >= amount)
      amount += inlen;
    if ((inout->len + inlen) >= inout->allocated)
	inout->str = xrealloc (inout->str, inout->allocated += amount);
    strcpy (inout->str + inout->len, in);
    inout->len += inlen;
    return (inout);
}

static MYSTRING *
prepend (inout, in)
    MYSTRING	*inout;
    const char	*in;
{
  MYSTRING *res = inout;
  if (strlen (in))
    {
      res = newstring (in);
      res = APPEND (res, inout->str);
      FREE (inout);
    }
  return res;
}

static void
grant_use_seizefile (seize_filename)
     const char *seize_filename;
{
  APPEND (gstring, "<> USE_SEIZE_FILE \"");
  APPEND (gstring, seize_filename);
  APPEND (gstring, "\" <>\n");
}

static MYSTRING *
decode_layout (layout)
    tree layout;
{
  tree temp;
  tree stepsize = NULL_TREE;
  int  was_step = 0;
  MYSTRING *result = newstring ("");
  MYSTRING *work;

  if (layout == integer_zero_node) /* NOPACK */
    {
      APPEND (result, " NOPACK");
      return result;
    }

  if (layout == integer_one_node) /* PACK */
    {
      APPEND (result, " PACK");
      return result;
    }

  APPEND (result, " ");
  temp = layout;
  if (TREE_PURPOSE (temp) == NULL_TREE)
    {
      APPEND (result, "STEP(");
      was_step = 1;
      temp = TREE_VALUE (temp);
      stepsize = TREE_VALUE (temp);
    }
  APPEND (result, "POS(");

  /* Get the starting word */
  temp = TREE_PURPOSE (temp);
  work = decode_constant (TREE_PURPOSE (temp));
  APPEND (result, work->str);
  FREE (work);

  temp = TREE_VALUE (temp);
  if (temp != NULL_TREE)
    {
      /* Get the starting bit */
      APPEND (result, ", ");
      work = decode_constant (TREE_PURPOSE (temp));
      APPEND (result, work->str);
      FREE (work);

      temp = TREE_VALUE (temp);
      if (temp != NULL_TREE)
	{
	  /* Get the length or the ending bit */
	  tree what = TREE_PURPOSE (temp);
	  if (what == integer_zero_node) /* length */
	    {
	      APPEND (result, ", ");
	    }
	  else
	    {
	      APPEND (result, ":");
	    }
	  work = decode_constant (TREE_VALUE (temp));
	  APPEND (result, work->str);
	  FREE (work);
	}
    }
  APPEND (result, ")");

  if (was_step)
    {
      if (stepsize != NULL_TREE)
	{
	  APPEND (result, ", ");
	  work = decode_constant (stepsize);
	  APPEND (result, work->str);
	  FREE (work);
	}
      APPEND (result, ")");
    }

  return result;
}

static MYSTRING *
grant_array_type (type)
     tree type;
{
  MYSTRING	*result = newstring ("");
  MYSTRING	*mode_string;
  tree           layout;
  int            varying = 0;

  if (chill_varying_type_p (type))
    {
      varying = 1;
      type = CH_VARYING_ARRAY_TYPE (type);
    }
  if (CH_STRING_TYPE_P (type))
    {
      tree fields = TYPE_DOMAIN (type);
      tree maxval = TYPE_MAX_VALUE (fields);

      if (TREE_CODE (TREE_TYPE (type)) == CHAR_TYPE)
	APPEND (result, "CHARS (");
      else
	APPEND (result, "BOOLS (");
      if (TREE_CODE (maxval) == INTEGER_CST)
	{
	  char	wrk[20];
	  sprintf (wrk, HOST_WIDE_INT_PRINT_DEC,
		   TREE_INT_CST_LOW (maxval) + 1);
	  APPEND (result, wrk);
	}
      else if (TREE_CODE (maxval) == MINUS_EXPR
	       && TREE_OPERAND (maxval, 1) == integer_one_node)
	{
	  mode_string = decode_constant (TREE_OPERAND (maxval, 0));
	  APPEND (result, mode_string->str);
	  FREE (mode_string);
	}
      else
	{
	  mode_string = decode_constant (maxval);
	  APPEND (result, mode_string->str);
	  FREE (mode_string);
	  APPEND (result, "+1");
	}
      APPEND (result, ")");
      if (varying)
	APPEND (result, " VARYING");
      return result;
    }

  APPEND (result, "ARRAY (");
  if (TREE_CODE (TYPE_DOMAIN (type)) == INTEGER_TYPE
     && TREE_TYPE (TYPE_DOMAIN (type)) == ridpointers[(int) RID_RANGE])
    {
      mode_string = decode_constant (TYPE_MIN_VALUE (TYPE_DOMAIN (type)));
      APPEND (result, mode_string->str);
      FREE (mode_string);
      
      APPEND (result, ":");
      mode_string = decode_constant (TYPE_MAX_VALUE (TYPE_DOMAIN (type)));
      APPEND (result, mode_string->str);
      FREE (mode_string);
    }
  else
    {
      mode_string = decode_mode (TYPE_DOMAIN (type));
      APPEND (result, mode_string->str);
      FREE (mode_string);
    }
  APPEND (result, ") ");
  if (varying)
    APPEND (result, "VARYING ");

  mode_string = get_type (TREE_TYPE (type));
  APPEND (result, mode_string->str);
  FREE (mode_string);

  layout = TYPE_ATTRIBUTES (type);
  if (layout != NULL_TREE)
    {
      mode_string = decode_layout (layout);
      APPEND (result, mode_string->str);
      FREE (mode_string);
    }
    
  return result;
}

static MYSTRING *
grant_array_type_selective (type, all_decls)
     tree type;
     tree all_decls;
{
  MYSTRING	*result = newstring ("");
  MYSTRING	*mode_string;
  int            varying = 0;

  if (chill_varying_type_p (type))
    {
      varying = 1;
      type = CH_VARYING_ARRAY_TYPE (type);
    }
  if (CH_STRING_TYPE_P (type))
    {
      tree fields = TYPE_DOMAIN (type);
      tree maxval = TYPE_MAX_VALUE (fields);

      if (TREE_CODE (maxval) != INTEGER_CST)
	{
	  if (TREE_CODE (maxval) == MINUS_EXPR
	      && TREE_OPERAND (maxval, 1) == integer_one_node)
	    {
	      mode_string = decode_constant_selective (TREE_OPERAND (maxval, 0), all_decls);
	      if (mode_string->len)
		APPEND (result, mode_string->str);
	      FREE (mode_string);
	    }
	  else
	    {
	      mode_string = decode_constant_selective (maxval, all_decls);
	      if (mode_string->len)
		APPEND (result, mode_string->str);
	      FREE (mode_string);
	    }
	}
      return result;
    }

  if (TREE_CODE (TYPE_DOMAIN (type)) == INTEGER_TYPE
     && TREE_TYPE (TYPE_DOMAIN (type)) == ridpointers[(int) RID_RANGE])
    {
      mode_string = decode_constant_selective (TYPE_MIN_VALUE (TYPE_DOMAIN (type)), all_decls);
      if (mode_string->len)
	APPEND (result, mode_string->str);
      FREE (mode_string);
      
      mode_string = decode_constant_selective (TYPE_MAX_VALUE (TYPE_DOMAIN (type)), all_decls);
      if (mode_string->len)
	{
	  MAYBE_NEWLINE (result);
	  APPEND (result, mode_string->str);
	}
      FREE (mode_string);
    }
  else
    {
      mode_string = decode_mode_selective (TYPE_DOMAIN (type), all_decls);
      if (mode_string->len)
	APPEND (result, mode_string->str);
      FREE (mode_string);
    }

  mode_string = get_type_selective (TREE_TYPE (type),  all_decls);
  if (mode_string->len)
    {
      MAYBE_NEWLINE (result);
      APPEND (result, mode_string->str);
    }
  FREE (mode_string);

  return result;
}

static MYSTRING *
get_tag_value (val)
    tree	val;
{
  MYSTRING	*result;
    
  if (TREE_CODE (val) == CONST_DECL && DECL_NAME (val))
    {
      result = newstring (IDENTIFIER_POINTER (DECL_NAME (val)));
    }
  else if (TREE_CODE (val) == CONST_DECL)
    {
      /* it's a synonym -- get the value */
      result = decode_constant (DECL_INITIAL (val));
    }
  else
    {
      result = decode_constant (val);
    }
  return (result);
}

static MYSTRING *
get_tag_value_selective (val, all_decls)
    tree	val;
    tree        all_decls;
{
  MYSTRING	*result;
    
  if (TREE_CODE (val) == CONST_DECL && DECL_NAME (val))
      result = newstring ("");
  else if (TREE_CODE (val) == CONST_DECL)
    {
      /* it's a synonym -- get the value */
      result = decode_constant_selective (DECL_INITIAL (val), all_decls);
    }
  else
    {
      result = decode_constant_selective (val, all_decls);
    }
  return (result);
}

static MYSTRING *
print_enumeral (type)
     tree type;
{
  MYSTRING	*result = newstring ("");
  tree	fields;

#if 0
  if (TYPE_LANG_SPECIFIC (type) == NULL)
#endif
    {
      
      APPEND (result, "SET (");
      for (fields = TYPE_VALUES (type);
	   fields != NULL_TREE;
	   fields = TREE_CHAIN (fields))
	{
	  if (TREE_PURPOSE (fields) == NULL_TREE)
	    APPEND (result, "*");
	  else
	    {
	      tree decl = TREE_VALUE (fields);
	      APPEND (result, IDENTIFIER_POINTER (TREE_PURPOSE (fields)));
	      if (TREE_CODE (decl) == CONST_DECL && DECL_INITIAL (decl))
		{
		  MYSTRING *val_string = decode_constant (DECL_INITIAL (decl));
		  APPEND (result, " = ");
		  APPEND (result, val_string->str);
		  FREE (val_string);
		}
	    }
	  if (TREE_CHAIN (fields) != NULL_TREE)
	    APPEND (result, ",\n     ");
	}
      APPEND (result, ")");
    }
  return result;
}

static MYSTRING *
print_enumeral_selective (type, all_decls)
     tree type;
     tree all_decls;
{
  MYSTRING	*result = newstring ("");
  tree	fields;

  for (fields = TYPE_VALUES (type);
       fields != NULL_TREE;
       fields = TREE_CHAIN (fields))
    {
      if (TREE_PURPOSE (fields) != NULL_TREE)
	{
	  tree decl = TREE_VALUE (fields);
	  if (TREE_CODE (decl) == CONST_DECL && DECL_INITIAL (decl))
	    {
	      MYSTRING *val_string = decode_constant_selective (DECL_INITIAL (decl), all_decls);
	      if (val_string->len)
		APPEND (result, val_string->str);
	      FREE (val_string);
	    }
	}
    }
  return result;
}

static MYSTRING *
print_integer_type (type)
     tree type;
{
  MYSTRING *result = newstring ("");
  MYSTRING *mode_string;
  const char *name_ptr;
  tree	    base_type;

  if (TREE_TYPE (type))
    {
      mode_string = decode_mode (TREE_TYPE (type));
      APPEND (result, mode_string->str);
      FREE (mode_string);
      
      APPEND (result, "(");
      mode_string = decode_constant (TYPE_MIN_VALUE (type));
      APPEND (result, mode_string->str);
      FREE (mode_string);

      if (TREE_TYPE (type) != ridpointers[(int) RID_BIN])
	{
	  APPEND (result, ":");
	  mode_string = decode_constant (TYPE_MAX_VALUE (type));
	  APPEND (result, mode_string->str);
	  FREE (mode_string);
	}

      APPEND (result, ")");
      return result;
    }
  /* We test TYPE_MAIN_VARIANT because pushdecl often builds
     a copy of a built-in type node, which is logically id-
     entical but has a different address, and the same
     TYPE_MAIN_VARIANT. */
  /* FIXME this should not be needed! */

  base_type = TREE_TYPE (type) ? TREE_TYPE (type) : type;

  if (TREE_UNSIGNED (base_type))
    {
      if (base_type == chill_unsigned_type_node
	  || TYPE_MAIN_VARIANT(base_type) ==
	     TYPE_MAIN_VARIANT (chill_unsigned_type_node))
	name_ptr = "UINT";
      else if (base_type == long_integer_type_node
	       || TYPE_MAIN_VARIANT(base_type) ==
	          TYPE_MAIN_VARIANT (long_unsigned_type_node))
	name_ptr = "ULONG";
      else if (type == unsigned_char_type_node
	       || TYPE_MAIN_VARIANT(base_type) ==
	          TYPE_MAIN_VARIANT (unsigned_char_type_node))
	name_ptr = "UBYTE";
      else if (type == duration_timing_type_node
	       || TYPE_MAIN_VARIANT (base_type) ==
	          TYPE_MAIN_VARIANT (duration_timing_type_node))
	name_ptr = "DURATION";
      else if (type == abs_timing_type_node
	       || TYPE_MAIN_VARIANT (base_type) ==
	          TYPE_MAIN_VARIANT (abs_timing_type_node))
	name_ptr = "TIME";
      else
	name_ptr = "UINT";
    }
  else
    {
      if (base_type == chill_integer_type_node
	  || TYPE_MAIN_VARIANT (base_type) ==
	     TYPE_MAIN_VARIANT (chill_integer_type_node))
	name_ptr = "INT";
      else if (base_type == long_integer_type_node
	       || TYPE_MAIN_VARIANT (base_type) ==
	          TYPE_MAIN_VARIANT (long_integer_type_node))
	name_ptr = "LONG";
      else if (type == signed_char_type_node
	       || TYPE_MAIN_VARIANT (base_type) ==
	          TYPE_MAIN_VARIANT (signed_char_type_node))
	name_ptr = "BYTE";
      else
	name_ptr = "INT";
    }
  
  APPEND (result, name_ptr);
  
  /* see if we have a range */
  if (TREE_TYPE (type) != NULL)
    {
      mode_string = decode_constant (TYPE_MIN_VALUE (type));
      APPEND (result, mode_string->str);
      FREE (mode_string);
      APPEND (result, ":");
      mode_string = decode_constant (TYPE_MAX_VALUE (type));
      APPEND (result, mode_string->str);
      FREE (mode_string);
    }

  return result;
}

static tree
find_enum_parent (enumname, all_decls)
     tree enumname;
     tree all_decls;
{
  tree wrk;

  for (wrk = all_decls; wrk != NULL_TREE; wrk = TREE_CHAIN (wrk))
    {
      if (TREE_TYPE (wrk) != NULL_TREE && TREE_CODE (wrk) != CONST_DECL &&
	  TREE_CODE (TREE_TYPE (wrk)) == ENUMERAL_TYPE)
	{
	  tree list;
	  for (list = TYPE_VALUES (TREE_TYPE (wrk)); list != NULL_TREE; list = TREE_CHAIN (list))
	    {
	      if (DECL_NAME (TREE_VALUE (list)) == enumname)
		return wrk;
	    }
	}
    }
  return NULL_TREE;
}

static MYSTRING *
print_integer_selective (type, all_decls)
     tree type;
     tree all_decls;
{
  MYSTRING *result = newstring ("");
  MYSTRING *mode_string;

  if (TREE_TYPE (type))
    {
      mode_string = decode_mode_selective (TREE_TYPE (type), all_decls);
      if (mode_string->len)
	APPEND (result, mode_string->str);
      FREE (mode_string);

      if (TREE_TYPE (type) == ridpointers[(int)RID_RANGE] &&
	  TREE_CODE (TYPE_MIN_VALUE (type)) == IDENTIFIER_NODE &&
	  TREE_CODE (TYPE_MAX_VALUE (type)) == IDENTIFIER_NODE)
	{
	  /* we have a range of a set. Find parant mode and write it
	     to SPEC MODULE. This will loose if the parent mode was SEIZED from
	     another file.*/
	  tree minparent = find_enum_parent (TYPE_MIN_VALUE (type), all_decls);
	  tree maxparent = find_enum_parent (TYPE_MAX_VALUE (type), all_decls);

	  if (minparent != NULL_TREE)
	    {
	      if (! CH_ALREADY_GRANTED (minparent))
		{
		  mode_string = decode_decl (minparent);
		  if (mode_string->len)
		    APPEND (result, mode_string->str);
		  FREE (mode_string);
		  CH_ALREADY_GRANTED (minparent) = 1;
		}
	    }
	  if (minparent != maxparent && maxparent != NULL_TREE)
	    {
	      if (!CH_ALREADY_GRANTED (maxparent))
		{
		  mode_string = decode_decl (maxparent);
		  if (mode_string->len)
		    {
		      MAYBE_NEWLINE (result);
		      APPEND (result, mode_string->str);
		    }
		  FREE (mode_string);
		  CH_ALREADY_GRANTED (maxparent) = 1;
		}
	    }
	}
      else
	{
	  mode_string = decode_constant_selective (TYPE_MIN_VALUE (type), all_decls);
	  if (mode_string->len)
	    {
	      MAYBE_NEWLINE (result);
	      APPEND (result, mode_string->str);
	    }
	  FREE (mode_string);
	  
	  mode_string = decode_constant_selective (TYPE_MAX_VALUE (type), all_decls);
	  if (mode_string->len)
	    {
	      MAYBE_NEWLINE (result);
	      APPEND (result, mode_string->str);
	    }
	  FREE (mode_string);
	}
      return result;
    }

  /* see if we have a range */
  if (TREE_TYPE (type) != NULL)
    {
      mode_string = decode_constant_selective (TYPE_MIN_VALUE (type), all_decls);
      if (mode_string->len)
	APPEND (result, mode_string->str);
      FREE (mode_string);

      mode_string = decode_constant_selective (TYPE_MAX_VALUE (type), all_decls);
      if (mode_string->len)
	{
	  MAYBE_NEWLINE (result);
	  APPEND (result, mode_string->str);
	}
      FREE (mode_string);
    }

  return result;
}

static MYSTRING *
print_struct (type)
     tree type;
{
  MYSTRING	*result = newstring ("");
  MYSTRING	*mode_string;
  tree	fields;

  if (chill_varying_type_p (type))
    {
      mode_string = grant_array_type (type);
      APPEND (result, mode_string->str);
      FREE (mode_string);
    }
  else
    {
      fields = TYPE_FIELDS (type);
      
      APPEND (result, "STRUCT (");
      while (fields != NULL_TREE)
	{
	  if (TREE_CODE (TREE_TYPE (fields)) == UNION_TYPE)
	    {
	      tree variants;
	      /* Format a tagged variant record type.  */
	      APPEND (result, " CASE ");
	      if (TYPE_TAGFIELDS (TREE_TYPE (fields)) != NULL_TREE)
		{
		  tree tag_list = TYPE_TAGFIELDS (TREE_TYPE (fields));
		  for (;;)
		    {
		      tree tag_name = DECL_NAME (TREE_VALUE (tag_list));
		      APPEND (result, IDENTIFIER_POINTER (tag_name));
		      tag_list = TREE_CHAIN (tag_list);
		      if (tag_list == NULL_TREE)
			break;
		      APPEND (result, ", ");
		    }
		}
	      APPEND (result, " OF\n");
	      variants = TYPE_FIELDS (TREE_TYPE (fields));
	      
	      /* Each variant is a FIELD_DECL whose type is an anonymous
		 struct within the anonymous union.  */
	      while (variants != NULL_TREE)
		{
		  tree tag_list = TYPE_TAG_VALUES (TREE_TYPE (variants));
		  tree struct_elts = TYPE_FIELDS (TREE_TYPE (variants));
		  
		  while (tag_list != NULL_TREE)
		    {
		      tree tag_values = TREE_VALUE (tag_list);
		      APPEND (result, "   (");
		      while (tag_values != NULL_TREE)
			{
			  mode_string = get_tag_value (TREE_VALUE (tag_values));
			  APPEND (result, mode_string->str);
			  FREE (mode_string);
			  if (TREE_CHAIN (tag_values) != NULL_TREE)
			    {
			      APPEND (result, ",\n    ");
			      tag_values = TREE_CHAIN (tag_values);
			    }
			  else break;
			}
		      APPEND (result, ")");
		      tag_list = TREE_CHAIN (tag_list);
		      if (tag_list)
			APPEND (result, ",");
		      else
			break;
		    }
		  APPEND (result, " : ");
		  
		  while (struct_elts != NULL_TREE)
		    {
		      mode_string = decode_decl (struct_elts);
		      APPEND (result, mode_string->str);
		      FREE (mode_string);
		      
		      if (TREE_CHAIN (struct_elts) != NULL_TREE)
			APPEND (result, ",\n     ");
		      struct_elts = TREE_CHAIN (struct_elts);
		    }
		  
		  variants = TREE_CHAIN (variants);
		  if (variants != NULL_TREE
		      && TREE_CHAIN (variants) == NULL_TREE
		      && DECL_NAME (variants) == ELSE_VARIANT_NAME)
		    {
		      tree else_elts = TYPE_FIELDS (TREE_TYPE (variants));
		      APPEND (result, "\n   ELSE ");
		      while (else_elts != NULL_TREE)
			{
			  mode_string = decode_decl (else_elts);
			  APPEND (result, mode_string->str);
			  FREE (mode_string);
			  if (TREE_CHAIN (else_elts) != NULL_TREE)
			    APPEND (result, ",\n     ");
			  else_elts = TREE_CHAIN (else_elts);
			}
		      break;
		    }
		  if (variants != NULL_TREE)
		    APPEND (result, ",\n");
		}
	      
	      APPEND (result, "\n   ESAC");
	    }
	  else
	    {
	      mode_string = decode_decl (fields);
	      APPEND (result, mode_string->str);
	      FREE (mode_string);
	    }
	  
	  fields = TREE_CHAIN (fields);
	  if (fields != NULL_TREE)
	    APPEND (result, ",\n    ");
	}
      APPEND (result, ")");
    }
  return result;
}

static MYSTRING *
print_struct_selective (type, all_decls)
     tree type;
     tree all_decls;
{
  MYSTRING	*result = newstring ("");
  MYSTRING	*mode_string;
  tree	fields;

  if (chill_varying_type_p (type))
    {
      mode_string = grant_array_type_selective (type, all_decls);
      if (mode_string->len)
	APPEND (result, mode_string->str);
      FREE (mode_string);
    }
  else
    {
      fields = TYPE_FIELDS (type);
      
      while (fields != NULL_TREE)
	{
	  if (TREE_CODE (TREE_TYPE (fields)) == UNION_TYPE)
	    {
	      tree variants;
	      /* Format a tagged variant record type.  */

	      variants = TYPE_FIELDS (TREE_TYPE (fields));
	      
	      /* Each variant is a FIELD_DECL whose type is an anonymous
		 struct within the anonymous union.  */
	      while (variants != NULL_TREE)
		{
		  tree tag_list = TYPE_TAG_VALUES (TREE_TYPE (variants));
		  tree struct_elts = TYPE_FIELDS (TREE_TYPE (variants));
		  
		  while (tag_list != NULL_TREE)
		    {
		      tree tag_values = TREE_VALUE (tag_list);
		      while (tag_values != NULL_TREE)
			{
			  mode_string = get_tag_value_selective (TREE_VALUE (tag_values),
								 all_decls);
			  if (mode_string->len)
			    {
			      MAYBE_NEWLINE (result);
			      APPEND (result, mode_string->str);
			    }
			  FREE (mode_string);
			  if (TREE_CHAIN (tag_values) != NULL_TREE)
			      tag_values = TREE_CHAIN (tag_values);
			  else break;
			}
		      tag_list = TREE_CHAIN (tag_list);
		      if (!tag_list)
			break;
		    }
		  
		  while (struct_elts != NULL_TREE)
		    {
		      mode_string = decode_decl_selective (struct_elts, all_decls);
		      if (mode_string->len)
			{
			  MAYBE_NEWLINE (result);
			  APPEND (result, mode_string->str);
			}
		      FREE (mode_string);
		      
		      struct_elts = TREE_CHAIN (struct_elts);
		    }
		  
		  variants = TREE_CHAIN (variants);
		  if (variants != NULL_TREE
		      && TREE_CHAIN (variants) == NULL_TREE
		      && DECL_NAME (variants) == ELSE_VARIANT_NAME)
		    {
		      tree else_elts = TYPE_FIELDS (TREE_TYPE (variants));
		      while (else_elts != NULL_TREE)
			{
			  mode_string = decode_decl_selective (else_elts, all_decls);
			  if (mode_string->len)
			    {
			      MAYBE_NEWLINE (result);
			      APPEND (result, mode_string->str);
			    }
			  FREE (mode_string);
			  else_elts = TREE_CHAIN (else_elts);
			}
		      break;
		    }
		}
	    }
	  else
	    {
	      mode_string = decode_decl_selective (fields, all_decls);
	      APPEND (result, mode_string->str);
	      FREE (mode_string);
	    }
	  
	  fields = TREE_CHAIN (fields);
	}
    }
  return result;
}

static MYSTRING *
print_proc_exceptions (ex)
     tree ex;
{
  MYSTRING	*result = newstring ("");

  if (ex != NULL_TREE)
    {
      APPEND (result, "\n  EXCEPTIONS (");
      for ( ; ex != NULL_TREE; ex = TREE_CHAIN (ex))
	{
	  APPEND (result, IDENTIFIER_POINTER (TREE_VALUE (ex)));
	  if (TREE_CHAIN (ex) != NULL_TREE)
	    APPEND (result, ",\n    ");
	}
      APPEND (result, ")");
    }
  return result;
}

static MYSTRING *
print_proc_tail (type, args, print_argnames)
     tree type;
     tree args;
     int print_argnames;
{
  MYSTRING	*result = newstring ("");
  MYSTRING	*mode_string;
  int count = 0;
  int stopat = list_length (args) - 3;

  /* do the argument modes */
  for ( ; args != NULL_TREE; 
       args = TREE_CHAIN (args), count++)
    {
      char buf[20];
      tree argmode = TREE_VALUE (args);
      tree attribute = TREE_PURPOSE (args);

      if (argmode == void_type_node)
	continue;

      /* if we have exceptions don't print last 2 arguments */
      if (TYPE_RAISES_EXCEPTIONS (type) && count == stopat)
	break;
      
      if (count)
	APPEND (result, ",\n       ");
      if (print_argnames)
	{
	  sprintf(buf, "arg%d ", count);
	  APPEND (result, buf);
	}

      if (attribute == ridpointers[(int) RID_LOC])
	argmode = TREE_TYPE (argmode);
      mode_string = get_type (argmode);
      APPEND (result, mode_string->str);
      FREE (mode_string);

      if (attribute != NULL_TREE)
	{
	  sprintf (buf, " %s", IDENTIFIER_POINTER (attribute));
	  APPEND (result, buf);
	}
    }
  APPEND (result, ")");
  
  /* return type */
  {
    tree retn_type = TREE_TYPE (type);

    if (retn_type != NULL_TREE
	&& TREE_CODE (retn_type) != VOID_TYPE)
      {
	mode_string = get_type (retn_type);
	APPEND (result, "\n  RETURNS (");
	APPEND (result, mode_string->str);
	FREE (mode_string);
	if (TREE_CODE (retn_type) == REFERENCE_TYPE)
	  APPEND (result, " LOC");
	APPEND (result, ")");
      }
  }

  mode_string = print_proc_exceptions (TYPE_RAISES_EXCEPTIONS (type));
  APPEND (result, mode_string->str);
  FREE (mode_string);
	
  return result;
}

static MYSTRING *
print_proc_tail_selective (type, args, all_decls)
     tree type;
     tree args;
     tree all_decls;
{
  MYSTRING	*result = newstring ("");
  MYSTRING	*mode_string;
  int count = 0;
  int stopat = list_length (args) - 3;

  /* do the argument modes */
  for ( ; args != NULL_TREE; 
       args = TREE_CHAIN (args), count++)
    {
      tree argmode = TREE_VALUE (args);
      tree attribute = TREE_PURPOSE (args);

      if (argmode == void_type_node)
	continue;

      /* if we have exceptions don't process last 2 arguments */
      if (TYPE_RAISES_EXCEPTIONS (type) && count == stopat)
	break;
      
      if (attribute == ridpointers[(int) RID_LOC])
	argmode = TREE_TYPE (argmode);
      mode_string = get_type_selective (argmode, all_decls);
      if (mode_string->len)
	{
	  MAYBE_NEWLINE (result);
	  APPEND (result, mode_string->str);
	}
      FREE (mode_string);
    }
  
  /* return type */
  {
    tree retn_type = TREE_TYPE (type);

    if (retn_type != NULL_TREE
	&& TREE_CODE (retn_type) != VOID_TYPE)
      {
	mode_string = get_type_selective (retn_type, all_decls);
	if (mode_string->len)
	  {
	    MAYBE_NEWLINE (result);
	    APPEND (result, mode_string->str);
	  }
	FREE (mode_string);
      }
  }
	
  return result;
}

/* output a mode (or type). */

static MYSTRING *
decode_mode (type)
    tree type;
{
  MYSTRING	*result = newstring ("");
  MYSTRING	*mode_string;

  switch ((enum chill_tree_code)TREE_CODE (type))
    {
    case TYPE_DECL:
      if (DECL_NAME (type))
	{
	  APPEND (result, IDENTIFIER_POINTER (DECL_NAME (type)));
	  return result;
	}
      type = TREE_TYPE (type);
      break;

    case IDENTIFIER_NODE:
      APPEND (result, IDENTIFIER_POINTER (type));
      return result;

    case LANG_TYPE:
      /* LANG_TYPE are only used until satisfy is done,
	 as place-holders for 'READ T', NEWMODE/SYNMODE modes,
	 parameterised modes, and old-fashioned CHAR(N). */
      if (TYPE_READONLY (type))
	APPEND (result, "READ ");

      mode_string = get_type (TREE_TYPE (type));
      APPEND (result, mode_string->str);
      if (TYPE_DOMAIN (type) != NULL_TREE)
	{
	  /* Parameterized mode,
	     or old-fashioned CHAR(N) string declaration.. */
	  APPEND (result, "(");
	  mode_string = decode_constant (TYPE_DOMAIN (type));
	  APPEND (result, mode_string->str);
	  APPEND (result, ")");
	}
      FREE (mode_string);
      break;

    case ARRAY_TYPE:
      mode_string = grant_array_type (type);
      APPEND (result, mode_string->str);
      FREE (mode_string);
      break;

    case BOOLEAN_TYPE:
      APPEND (result, "BOOL");
      break;

    case CHAR_TYPE:
      APPEND (result, "CHAR");
      break;

    case ENUMERAL_TYPE:
      mode_string = print_enumeral (type); 
      APPEND (result, mode_string->str);
      FREE (mode_string);
      break;
	
    case FUNCTION_TYPE:
      {
	tree args = TYPE_ARG_TYPES (type);

	APPEND (result, "PROC (");

	mode_string = print_proc_tail (type, args, 0);
	APPEND (result, mode_string->str);
	FREE (mode_string);
      }
      break;

    case INTEGER_TYPE:
      mode_string = print_integer_type (type);
      APPEND (result, mode_string->str);
      FREE (mode_string);
      break;
	
    case RECORD_TYPE:
      if (CH_IS_INSTANCE_MODE (type))
	{
	  APPEND (result, "INSTANCE");
	  return result;
	}
      else if (CH_IS_BUFFER_MODE (type) || CH_IS_EVENT_MODE (type))
	{ tree bufsize = max_queue_size (type);
	  APPEND (result, CH_IS_BUFFER_MODE (type) ? "BUFFER " : "EVENT ");
	  if (bufsize != NULL_TREE)
	    {
	      APPEND (result, "(");
	      mode_string = decode_constant (bufsize);
	      APPEND (result, mode_string->str);
	      APPEND (result, ") ");
	      FREE (mode_string);
	    }
	  if (CH_IS_BUFFER_MODE (type))
	    {
	      mode_string = decode_mode (buffer_element_mode (type));
	      APPEND (result, mode_string->str);
	      FREE (mode_string);
	    }
	  break;
	}
      else if (CH_IS_ACCESS_MODE (type))
	{
	  tree indexmode, recordmode, dynamic;

	  APPEND (result, "ACCESS");
	  recordmode = access_recordmode (type);
	  indexmode = access_indexmode (type);
	  dynamic = access_dynamic (type);

	  if (indexmode != void_type_node)
	    {
	      mode_string = decode_mode (indexmode);
	      APPEND (result, " (");
	      APPEND (result, mode_string->str);
	      APPEND (result, ")");
	      FREE (mode_string);
	    }
	  if (recordmode != void_type_node)
	    {
	      mode_string = decode_mode (recordmode);
	      APPEND (result, " ");
	      APPEND (result, mode_string->str);
	      FREE (mode_string);
	    }
	  if (dynamic != integer_zero_node)
	    APPEND (result, " DYNAMIC");
	  break;
	}
      else if (CH_IS_TEXT_MODE (type))
	{
	  tree indexmode, dynamic, length;

	  APPEND (result, "TEXT (");
	  length = text_length (type);
	  indexmode = text_indexmode (type);
	  dynamic = text_dynamic (type);

	  mode_string = decode_constant (length);
	  APPEND (result, mode_string->str);
	  FREE (mode_string);
	  APPEND (result, ")");
	  if (indexmode != void_type_node)
	    {
	      APPEND (result, " ");
	      mode_string = decode_mode (indexmode);
	      APPEND (result, mode_string->str);
	      FREE (mode_string);
	    }
	  if (dynamic != integer_zero_node)
	    APPEND (result, " DYNAMIC");
	  return result;
	}
      mode_string = print_struct (type);
      APPEND (result, mode_string->str);
      FREE (mode_string);
      break;

    case POINTER_TYPE:
      if (TREE_CODE (TREE_TYPE (type)) == VOID_TYPE)
	APPEND (result, "PTR");
      else
	{
	  if (TREE_CODE (TREE_TYPE (type)) == FUNCTION_TYPE)
	    {
	      mode_string = get_type (TREE_TYPE (type));
	      APPEND (result, mode_string->str);
	      FREE (mode_string);
	    }
	  else
	    {
	      APPEND (result, "REF ");
	      mode_string = get_type (TREE_TYPE (type));
	      APPEND (result, mode_string->str);
	      FREE (mode_string);
	    }
	}
      break;

    case REAL_TYPE:
      if (TREE_INT_CST_LOW (TYPE_SIZE (type)) == 32)
	APPEND (result, "REAL");
      else
	APPEND (result, "LONG_REAL");
      break;

    case SET_TYPE:
      if (CH_BOOLS_TYPE_P (type))
	mode_string = grant_array_type (type);
      else
	{
	  APPEND (result, "POWERSET ");
	  mode_string = get_type (TYPE_DOMAIN (type));
	}
      APPEND (result, mode_string->str);
      FREE (mode_string);
      break;
	
    case REFERENCE_TYPE:
      mode_string = get_type (TREE_TYPE (type));
      APPEND (result, mode_string->str);
      FREE (mode_string);
      break;
      
    default:
      APPEND (result, "/* ---- not implemented ---- */");
      break;
    }

  return (result);
}

static tree
find_in_decls (id, all_decls)
     tree id;
     tree all_decls;
{
  tree wrk;

  for (wrk = all_decls; wrk != NULL_TREE; wrk = TREE_CHAIN (wrk))
    {
      if (DECL_NAME (wrk) == id || DECL_POSTFIX (wrk) == id)
	return wrk;
    }
  return NULL_TREE;
}

static int
in_ridpointers (id)
     tree id;
{
  int i;
  for (i = RID_UNUSED; i < RID_MAX; i++)
    {
      if (id == ridpointers[i])
	return 1;
    }
  return 0;
}

static void
grant_seized_identifier (decl)
     tree decl;
{
  seizefile_list *wrk = selective_seizes;
  MYSTRING *mode_string;

  CH_ALREADY_GRANTED (decl) = 1;

  /* comes from a SPEC MODULE in the module */
  if (DECL_SEIZEFILE (decl) == NULL_TREE)
    return;

  /* search file already in process */
  while (wrk != 0)
    {
      if (wrk->filename == DECL_SEIZEFILE (decl))
	break;
      wrk = wrk->next;
    }
  if (!wrk)
    {
      wrk = (seizefile_list *)xmalloc (sizeof (seizefile_list));
      wrk->next = selective_seizes;
      selective_seizes = wrk;
      wrk->filename = DECL_SEIZEFILE (decl);
      wrk->seizes = newstring ("<> USE_SEIZE_FILE \"");
      APPEND (wrk->seizes, IDENTIFIER_POINTER (DECL_SEIZEFILE (decl)));
      APPEND (wrk->seizes, "\" <>\n");
    }
  APPEND (wrk->seizes, "SEIZE ");
  mode_string = decode_prefix_rename (decl);
  APPEND (wrk->seizes, mode_string->str);
  FREE (mode_string);
  APPEND (wrk->seizes, ";\n");
}

static MYSTRING *
decode_mode_selective (type, all_decls)
    tree type;
    tree all_decls;
{
  MYSTRING	*result = newstring ("");
  MYSTRING	*mode_string;
  tree decl;

  switch ((enum chill_tree_code)TREE_CODE (type))
    {
    case TYPE_DECL:
      /* FIXME: could this ever happen ?? */
      if (DECL_NAME (type))
	{
	  FREE (result);
	  result = decode_mode_selective (DECL_NAME (type), all_decls);
	  return result;
	}
      break;

    case IDENTIFIER_NODE:
      if (in_ridpointers (type))
	/* it's a predefined, we must not search the whole list */
	return result;

      decl = find_in_decls (type, all_decls);
      if (decl != NULL_TREE)
	{
	  if (CH_ALREADY_GRANTED (decl))
	    /* already processed */
	    return result;

	  if (TREE_CODE (decl) == ALIAS_DECL && DECL_POSTFIX (decl) != NULL_TREE)
	    {
	      /* If CH_DECL_GRANTED, decl was granted into this scope, and
		 so wasn't in the source code. */
	      if (!CH_DECL_GRANTED (decl))
		{
		  grant_seized_identifier (decl);
		}
	    }
	  else
	    {
	      result = decode_decl (decl);
	      mode_string = decode_decl_selective (decl, all_decls);
	      if (mode_string->len)
		{
		  PREPEND (result, mode_string->str);
		}
	      FREE (mode_string);
	    }
	}
      return result;

    case LANG_TYPE:
      mode_string = get_type_selective (TREE_TYPE (type), all_decls);
      APPEND (result, mode_string->str);
      FREE (mode_string);
      break;

    case ARRAY_TYPE:
      mode_string = grant_array_type_selective (type, all_decls);
      APPEND (result, mode_string->str);
      FREE (mode_string);
      break;

    case BOOLEAN_TYPE:
      return result;
      break;

    case CHAR_TYPE:
      return result;
      break;

    case ENUMERAL_TYPE:
      mode_string = print_enumeral_selective (type, all_decls);
      if (mode_string->len)
	APPEND (result, mode_string->str);
      FREE (mode_string);
      break;
	
    case FUNCTION_TYPE:
      {
	tree args = TYPE_ARG_TYPES (type);

	mode_string = print_proc_tail_selective (type, args, all_decls);
	if (mode_string->len)
	  APPEND (result, mode_string->str);
	FREE (mode_string);
      }
      break;

    case INTEGER_TYPE:
      mode_string = print_integer_selective (type, all_decls);
      if (mode_string->len)
	APPEND (result, mode_string->str);
      FREE (mode_string);
      break;
	
    case RECORD_TYPE:
      if (CH_IS_INSTANCE_MODE (type))
	{
	  return result;
	}
      else if (CH_IS_BUFFER_MODE (type) || CH_IS_EVENT_MODE (type))
	{
	  tree bufsize = max_queue_size (type);
	  if (bufsize != NULL_TREE)
	    {
	      mode_string = decode_constant_selective (bufsize, all_decls);
	      if (mode_string->len)
		APPEND (result, mode_string->str);
	      FREE (mode_string);
	    }
	  if (CH_IS_BUFFER_MODE (type))
	    {
	      mode_string = decode_mode_selective (buffer_element_mode (type), all_decls);
	      if (mode_string->len)
		{
		  MAYBE_NEWLINE (result);
		  APPEND (result, mode_string->str);
		}
	      FREE (mode_string);
	    }
	  break;
	}      
      else if (CH_IS_ACCESS_MODE (type))
	{
	  tree indexmode = access_indexmode (type);
	  tree recordmode = access_recordmode (type);
	      
	  if (indexmode != void_type_node)
	    {
	      mode_string = decode_mode_selective (indexmode, all_decls);
	      if (mode_string->len)
		{
		  if (result->len && result->str[result->len - 1] != '\n')
		    APPEND (result, ";\n");
		  APPEND (result, mode_string->str);
		}
	      FREE (mode_string);
	    }
	  if (recordmode != void_type_node)
	    {
	      mode_string = decode_mode_selective (recordmode, all_decls);
	      if (mode_string->len)
		{
		  if (result->len && result->str[result->len - 1] != '\n')
		    APPEND (result, ";\n");
		  APPEND (result, mode_string->str);
		}
	      FREE (mode_string);
	    }
	  break;
	}
      else if (CH_IS_TEXT_MODE (type))
	{
	  tree indexmode = text_indexmode (type);
	  tree length = text_length (type);

	  mode_string = decode_constant_selective (length, all_decls);
	  if (mode_string->len)
	    APPEND (result, mode_string->str);
	  FREE (mode_string);
	  if (indexmode != void_type_node)
	    {
	      mode_string = decode_mode_selective (indexmode, all_decls);
	      if (mode_string->len)
		{
		  if (result->len && result->str[result->len - 1] != '\n')
		    APPEND (result, ";\n");
		  APPEND (result, mode_string->str);
		}
	      FREE (mode_string);
	    }
	  break;
	}
      mode_string = print_struct_selective (type, all_decls);
      if (mode_string->len)
	{
	  MAYBE_NEWLINE (result);
	  APPEND (result, mode_string->str);
	}
      FREE (mode_string);
      break;

    case POINTER_TYPE:
      if (TREE_CODE (TREE_TYPE (type)) == VOID_TYPE)
	break;
      else
	{
	  if (TREE_CODE (TREE_TYPE (type)) == FUNCTION_TYPE)
	    {
	      mode_string = get_type_selective (TREE_TYPE (type), all_decls);
	      if (mode_string->len)
		APPEND (result, mode_string->str);
	      FREE (mode_string);
	    }
	  else
	    {
	      mode_string = get_type_selective (TREE_TYPE (type), all_decls);
	      if (mode_string->len)
		APPEND (result, mode_string->str);
	      FREE (mode_string);
	    }
	}
      break;

    case REAL_TYPE:
      return result;
      break;

    case SET_TYPE:
      if (CH_BOOLS_TYPE_P (type))
	mode_string = grant_array_type_selective (type, all_decls);
      else
	mode_string = get_type_selective (TYPE_DOMAIN (type), all_decls);
      if (mode_string->len)
	APPEND (result, mode_string->str);
      FREE (mode_string);
      break;
	
    case REFERENCE_TYPE:
      mode_string = get_type_selective (TREE_TYPE (type), all_decls);
      if (mode_string->len)
	APPEND (result, mode_string->str);
      FREE (mode_string);
      break;
      
    default:
      APPEND (result, "/* ---- not implemented ---- */");
      break;
    }

  return (result);
}

static MYSTRING *
get_type (type)
    tree	type;
{
  if (type == NULL_TREE || TREE_CODE (type) == ERROR_MARK)
    return newstring ("");

  return (decode_mode (type));
}

static MYSTRING *
get_type_selective (type, all_decls)
    tree	type;
    tree        all_decls;
{
  if (type == NULL_TREE || TREE_CODE (type) == ERROR_MARK)
    return newstring ("");

  return (decode_mode_selective (type, all_decls));
}

#if 0
static int
is_forbidden (str, forbid)
    tree	str;
    tree	forbid;
{
  if (forbid == NULL_TREE)
    return (0);
  
  if (TREE_CODE (forbid) == INTEGER_CST)
    return (1);
  
  while (forbid != NULL_TREE)
    {
      if (TREE_VALUE (forbid) == str)
	return (1);
      forbid = TREE_CHAIN (forbid);
    }
  /* nothing found */
  return (0);
}
#endif

static MYSTRING *
decode_constant (init)
     tree	init;
{
  MYSTRING *result = newstring ("");
  MYSTRING *tmp_string;
  tree	    type = TREE_TYPE (init);
  tree	val = init;
  const char *op;
  char	wrk[256];
  MYSTRING *mode_string;
    
  switch ((enum chill_tree_code)TREE_CODE (val))
    {
    case CALL_EXPR:
      tmp_string = decode_constant (TREE_OPERAND (val, 0));
      APPEND (result, tmp_string->str);
      FREE (tmp_string);
      val = TREE_OPERAND (val, 1);  /* argument list */
      if (val != NULL_TREE && TREE_CODE (val) != TREE_LIST)
	{
	  APPEND (result, " ");
	  tmp_string = decode_constant (val);
	  APPEND (result, tmp_string->str);
	  FREE (tmp_string);
	}
      else
	{
	  APPEND (result, " (");
	  if (val != NULL_TREE)
	    {
	      for (;;)
		{
		  tmp_string = decode_constant (TREE_VALUE (val));
		  APPEND (result, tmp_string->str);
		  FREE (tmp_string);
		  val = TREE_CHAIN (val);
		  if (val == NULL_TREE)
		    break;
		  APPEND (result, ", ");
		}
	    }
	  APPEND (result, ")");
	}
      return result;

    case NOP_EXPR:
      /* Generate an "expression conversion" expression (a cast). */
      tmp_string = decode_mode (type);

      APPEND (result, tmp_string->str);
      FREE (tmp_string);
      APPEND (result, "(");
      val = TREE_OPERAND (val, 0);
      type = TREE_TYPE (val);

      /* If the coercee is a tuple, make sure it is prefixed by its mode. */
      if (TREE_CODE (val) == CONSTRUCTOR
	&& !CH_BOOLS_TYPE_P (type) && !chill_varying_type_p (type))
	{
	  tmp_string = decode_mode (type);
	  APPEND (result, tmp_string->str);
	  FREE (tmp_string);
	  APPEND (result, " ");
	}

      tmp_string = decode_constant (val);
      APPEND (result, tmp_string->str);
      FREE (tmp_string);
      APPEND (result, ")");
      return result;

    case IDENTIFIER_NODE:
      APPEND (result, IDENTIFIER_POINTER (val));
      return result;

    case PAREN_EXPR:
      APPEND (result, "(");
      tmp_string = decode_constant (TREE_OPERAND (val, 0));
      APPEND (result, tmp_string->str);
      FREE (tmp_string);
      APPEND (result, ")");
      return result;

    case UNDEFINED_EXPR:
      APPEND (result, "*");
      return result;

    case PLUS_EXPR:        op = "+";       goto binary;
    case MINUS_EXPR:       op = "-";       goto binary;
    case MULT_EXPR:        op = "*";       goto binary;
    case TRUNC_DIV_EXPR:   op = "/";       goto binary;
    case FLOOR_MOD_EXPR:   op = " MOD ";   goto binary;
    case TRUNC_MOD_EXPR:   op = " REM ";   goto binary;
    case CONCAT_EXPR:      op = "//";      goto binary;
    case BIT_IOR_EXPR:     op = " OR ";    goto binary;
    case BIT_XOR_EXPR:     op = " XOR ";   goto binary;
    case TRUTH_ORIF_EXPR:  op = " ORIF ";  goto binary;
    case BIT_AND_EXPR:     op = " AND ";   goto binary;
    case TRUTH_ANDIF_EXPR: op = " ANDIF "; goto binary;
    case GT_EXPR:          op = ">";       goto binary;
    case GE_EXPR:          op = ">=";      goto binary;
    case SET_IN_EXPR:      op = " IN ";    goto binary;
    case LT_EXPR:          op = "<";       goto binary;
    case LE_EXPR:          op = "<=";      goto binary;
    case EQ_EXPR:          op = "=";       goto binary;
    case NE_EXPR:          op = "/=";      goto binary;
    case RANGE_EXPR:
      if (TREE_OPERAND (val, 0) == NULL_TREE)
	{
	  APPEND (result, TREE_OPERAND (val, 1) == NULL_TREE ? "*" : "ELSE");
	  return result;
	}
      op = ":";       goto binary;
    binary:
      tmp_string = decode_constant (TREE_OPERAND (val, 0));
      APPEND (result, tmp_string->str);
      FREE (tmp_string);
      APPEND (result, op);
      tmp_string = decode_constant (TREE_OPERAND (val, 1));
      APPEND (result, tmp_string->str);
      FREE (tmp_string);
      return result;

    case REPLICATE_EXPR:
      APPEND (result, "(");
      tmp_string = decode_constant (TREE_OPERAND (val, 0));
      APPEND (result, tmp_string->str);
      FREE (tmp_string);
      APPEND (result, ")");
      tmp_string = decode_constant (TREE_OPERAND (val, 1));
      APPEND (result, tmp_string->str);
      FREE (tmp_string);
      return result;

    case NEGATE_EXPR:     op = "-";     goto unary;
    case BIT_NOT_EXPR:    op = " NOT "; goto unary;
    case ADDR_EXPR:       op = "->"; goto unary;
    unary:
      APPEND (result, op);
      tmp_string = decode_constant (TREE_OPERAND (val, 0));
      APPEND (result, tmp_string->str);
      FREE (tmp_string);
      return result;

    case INTEGER_CST:
      APPEND (result, display_int_cst (val));
      return result;

    case REAL_CST:
#ifndef REAL_IS_NOT_DOUBLE
      sprintf (wrk, "%.20g", TREE_REAL_CST (val));
#else
      REAL_VALUE_TO_DECIMAL (TREE_REAL_CST (val), "%.20g", wrk);
#endif
      APPEND (result, wrk);
      return result;

    case STRING_CST:
      {
	const char *ptr = TREE_STRING_POINTER (val);
	int i = TREE_STRING_LENGTH (val);
	APPEND (result, "\"");
	while (--i >= 0)
	  {
	    char buf[10];
	    unsigned char c = *ptr++;
	    if (c == '^')
	      APPEND (result, "^^");
	    else if (c == '"')
	      APPEND (result, "\"\"");
	    else if (c == '\n')
	      APPEND (result, "^J");
	    else if (c < ' ' || c > '~')
	      {
		sprintf (buf, "^(%u)", c);
		APPEND (result, buf);
	      }
	    else
	      {
		buf[0] = c;
		buf[1] = 0;
		APPEND (result, buf);
	      }
	  }
	APPEND (result, "\"");
	return result;
      }

    case CONSTRUCTOR:
      val = TREE_OPERAND (val, 1);
      if (type != NULL && TREE_CODE (type) == SET_TYPE
	  && CH_BOOLS_TYPE_P (type))
	{
	  /* It's a bitstring. */
	  tree domain = TYPE_DOMAIN (type);
	  tree domain_max = TYPE_MAX_VALUE (domain);
	  char *buf;
	  register char *ptr;
	  int len;
	  if (TREE_CODE (domain_max) != INTEGER_CST
	      || (val && TREE_CODE (val) != TREE_LIST))
	    goto fail;

	  len = TREE_INT_CST_LOW (domain_max) + 1;
	  if (TREE_CODE (init) != CONSTRUCTOR)
	    goto fail;
	  buf = (char *) alloca (len + 10);
	  ptr = buf;
	  *ptr++ = ' ';	  
	  *ptr++ = 'B';
	  *ptr++ = '\'';
	  if (get_set_constructor_bits (init, ptr, len))
	    goto fail;
	  for (; --len >= 0; ptr++)
	    *ptr += '0';
	  *ptr++ = '\'';
	  *ptr = '\0';
	  APPEND (result, buf);
	  return result;
	}
      else
	{ /* It's some kind of tuple */
	  if (type != NULL_TREE)
	    {
	      mode_string = get_type (type);
	      APPEND (result, mode_string->str);
	      FREE (mode_string);
	      APPEND (result, " ");
	    }
	  if (val == NULL_TREE
	      || TREE_CODE (val) == ERROR_MARK)
	    APPEND (result, "[ ]");
	  else if (TREE_CODE (val) != TREE_LIST)
	    goto fail;
	  else
	    {
	      APPEND (result, "[");
	      for ( ; ; )
		{
		  tree lo_val = TREE_PURPOSE (val);
		  tree hi_val = TREE_VALUE (val);
		  MYSTRING *val_string;
		  if (TUPLE_NAMED_FIELD (val))
		    APPEND(result, ".");
		  if (lo_val != NULL_TREE)
		    {
		      val_string = decode_constant (lo_val);
		      APPEND (result, val_string->str);
		      FREE (val_string);
		      APPEND (result, ":");
		    }
		  val_string = decode_constant (hi_val);
		  APPEND (result, val_string->str);
		  FREE (val_string);
		  val = TREE_CHAIN (val);
		  if (val == NULL_TREE)
		    break;
		  APPEND (result, ", ");
		}
	      APPEND (result, "]");
	    }
	}
      return result;
    case COMPONENT_REF:
      {
	tree op1;

	mode_string = decode_constant (TREE_OPERAND (init, 0));
	APPEND (result, mode_string->str);
	FREE (mode_string);
	op1 = TREE_OPERAND (init, 1);
	if (TREE_CODE (op1) != IDENTIFIER_NODE)
	  {
	    error ("decode_constant: invalid component_ref");
	    break;
	  }
	APPEND (result, ".");
	APPEND (result, IDENTIFIER_POINTER (op1));
	return result;
      }
    fail:
      error ("decode_constant: mode and value mismatch");
      break;
    default:
      error ("decode_constant: cannot decode this mode");
      break;
    }
  return result;
}

static MYSTRING *
decode_constant_selective (init, all_decls)
     tree	init;
     tree       all_decls;
{
  MYSTRING *result = newstring ("");
  MYSTRING *tmp_string;
  tree	    type = TREE_TYPE (init);
  tree	val = init;
  MYSTRING *mode_string;
    
  switch ((enum chill_tree_code)TREE_CODE (val))
    {
    case CALL_EXPR:
      tmp_string = decode_constant_selective (TREE_OPERAND (val, 0), all_decls);
      if (tmp_string->len)
	APPEND (result, tmp_string->str);
      FREE (tmp_string);
      val = TREE_OPERAND (val, 1);  /* argument list */
      if (val != NULL_TREE && TREE_CODE (val) != TREE_LIST)
	{
	  tmp_string = decode_constant_selective (val, all_decls);
	  if (tmp_string->len)
	    {
	      MAYBE_NEWLINE (result);
	      APPEND (result, tmp_string->str);
	    }
	  FREE (tmp_string);
	}
      else
	{
	  if (val != NULL_TREE)
	    {
	      for (;;)
		{
		  tmp_string = decode_constant_selective (TREE_VALUE (val), all_decls);
		  if (tmp_string->len)
		    {
		      MAYBE_NEWLINE (result);
		      APPEND (result, tmp_string->str);
		    }
		  FREE (tmp_string);
		  val = TREE_CHAIN (val);
		  if (val == NULL_TREE)
		    break;
		}
	    }
	}
      return result;

    case NOP_EXPR:
      /* Generate an "expression conversion" expression (a cast). */
      tmp_string = decode_mode_selective (type, all_decls);
      if (tmp_string->len)
	APPEND (result, tmp_string->str);
      FREE (tmp_string);
      val = TREE_OPERAND (val, 0);
      type = TREE_TYPE (val);

      /* If the coercee is a tuple, make sure it is prefixed by its mode. */
      if (TREE_CODE (val) == CONSTRUCTOR
	&& !CH_BOOLS_TYPE_P (type) && !chill_varying_type_p (type))
	{
	  tmp_string = decode_mode_selective (type, all_decls);
	  if (tmp_string->len)
	    APPEND (result, tmp_string->str);
	  FREE (tmp_string);
	}

      tmp_string = decode_constant_selective (val, all_decls);
      if (tmp_string->len)
	APPEND (result, tmp_string->str);
      FREE (tmp_string);
      return result;

    case IDENTIFIER_NODE:
      tmp_string = decode_mode_selective (val, all_decls);
      if (tmp_string->len)
	APPEND (result, tmp_string->str);
      FREE (tmp_string);
      return result;

    case PAREN_EXPR:
      tmp_string = decode_constant_selective (TREE_OPERAND (val, 0), all_decls);
      if (tmp_string->len)
	APPEND (result, tmp_string->str);
      FREE (tmp_string);
      return result;

    case UNDEFINED_EXPR:
      return result;

    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
    case TRUNC_DIV_EXPR:
    case FLOOR_MOD_EXPR:
    case TRUNC_MOD_EXPR:
    case CONCAT_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case TRUTH_ORIF_EXPR:
    case BIT_AND_EXPR:
    case TRUTH_ANDIF_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case SET_IN_EXPR:
    case LT_EXPR:
    case LE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
      goto binary;
    case RANGE_EXPR:
      if (TREE_OPERAND (val, 0) == NULL_TREE)
	  return result;

    binary:
      tmp_string = decode_constant_selective (TREE_OPERAND (val, 0), all_decls);
      if (tmp_string->len)
	APPEND (result, tmp_string->str);
      FREE (tmp_string);
      tmp_string = decode_constant_selective (TREE_OPERAND (val, 1), all_decls);
      if (tmp_string->len)
	{
	  MAYBE_NEWLINE (result);
	  APPEND (result, tmp_string->str);
	}
      FREE (tmp_string);
      return result;

    case REPLICATE_EXPR:
      tmp_string = decode_constant_selective (TREE_OPERAND (val, 0), all_decls);
      if (tmp_string->len)
	APPEND (result, tmp_string->str);
      FREE (tmp_string);
      tmp_string = decode_constant_selective (TREE_OPERAND (val, 1), all_decls);
      if (tmp_string->len)
	{
	  MAYBE_NEWLINE (result);
	  APPEND (result, tmp_string->str);
	}
      FREE (tmp_string);
      return result;

    case NEGATE_EXPR:
    case BIT_NOT_EXPR:
    case ADDR_EXPR:
      tmp_string = decode_constant_selective (TREE_OPERAND (val, 0), all_decls);
      if (tmp_string->len)
	APPEND (result, tmp_string->str);
      FREE (tmp_string);
      return result;

    case INTEGER_CST:
      return result;

    case REAL_CST:
      return result;

    case STRING_CST:
      return result;

    case CONSTRUCTOR:
      val = TREE_OPERAND (val, 1);
      if (type != NULL && TREE_CODE (type) == SET_TYPE
	  && CH_BOOLS_TYPE_P (type))
	  /* It's a bitstring. */
	  return result;
      else
	{ /* It's some kind of tuple */
	  if (type != NULL_TREE)
	    {
	      mode_string = get_type_selective (type, all_decls);
	      if (mode_string->len)
		APPEND (result, mode_string->str);
	      FREE (mode_string);
	    }
	  if (val == NULL_TREE
	      || TREE_CODE (val) == ERROR_MARK)
	    return result;
	  else if (TREE_CODE (val) != TREE_LIST)
	    goto fail;
	  else
	    {
	      for ( ; ; )
		{
		  tree lo_val = TREE_PURPOSE (val);
		  tree hi_val = TREE_VALUE (val);
		  MYSTRING *val_string;
		  if (lo_val != NULL_TREE)
		    {
		      val_string = decode_constant_selective (lo_val, all_decls);
		      if (val_string->len)
			APPEND (result, val_string->str);
		      FREE (val_string);
		    }
		  val_string = decode_constant_selective (hi_val, all_decls);
		  if (val_string->len)
		    {
		      MAYBE_NEWLINE (result);
		      APPEND (result, val_string->str);
		    }
		  FREE (val_string);
		  val = TREE_CHAIN (val);
		  if (val == NULL_TREE)
		    break;
		}
	    }
	}
      return result;
    case COMPONENT_REF:
      {
	mode_string = decode_constant_selective (TREE_OPERAND (init, 0), all_decls);
	if (mode_string->len)
	  APPEND (result, mode_string->str);
	FREE (mode_string);
	return result;
      }
    fail:
      error ("decode_constant_selective: mode and value mismatch");
      break;
    default:
      error ("decode_constant_selective: cannot decode this mode");
      break;
    }
  return result;
}

/* Assuming DECL is an ALIAS_DECL, return its prefix rename clause. */

static MYSTRING *
decode_prefix_rename (decl)
    tree decl;
{
  MYSTRING *result = newstring ("");
  if (DECL_OLD_PREFIX (decl) || DECL_NEW_PREFIX (decl))
    {
      APPEND (result, "(");
      if (DECL_OLD_PREFIX (decl))
	APPEND (result, IDENTIFIER_POINTER (DECL_OLD_PREFIX (decl)));
      APPEND (result, "->");
      if (DECL_NEW_PREFIX (decl))
 	APPEND (result, IDENTIFIER_POINTER (DECL_NEW_PREFIX (decl)));
      APPEND (result, ")!");
    }
  if (DECL_POSTFIX_ALL (decl))
    APPEND (result, "ALL");
  else
    APPEND (result, IDENTIFIER_POINTER  (DECL_POSTFIX (decl)));
  return result;
}

static MYSTRING *
decode_decl (decl)
    tree decl;
{
  MYSTRING *result = newstring ("");
  MYSTRING *mode_string;
  tree      type;
  
  switch ((enum chill_tree_code)TREE_CODE (decl))
    {
    case VAR_DECL:
    case BASED_DECL:
      APPEND (result, "DCL ");
      APPEND (result, IDENTIFIER_POINTER (DECL_NAME (decl)));
      APPEND (result, " ");
      mode_string = get_type (TREE_TYPE (decl));
      APPEND (result, mode_string->str);
      FREE (mode_string);
      if ((enum chill_tree_code)TREE_CODE (decl) == BASED_DECL)
        {
          APPEND (result, " BASED (");
          APPEND (result, IDENTIFIER_POINTER (DECL_ABSTRACT_ORIGIN (decl)));
          APPEND (result, ")");
        }
      break;

    case TYPE_DECL:
      if (CH_DECL_SIGNAL (decl))
	{
	  /* this is really a signal */
	  tree fields = TYPE_FIELDS (TREE_TYPE (decl));
	  tree signame = DECL_NAME (decl);
	  tree sigdest;
	  
	  APPEND (result, "SIGNAL ");
	  APPEND (result, IDENTIFIER_POINTER (signame));
	  if (IDENTIFIER_SIGNAL_DATA (signame))
	    {
	      APPEND (result, " = (");
	      for ( ; fields != NULL_TREE;
		   fields = TREE_CHAIN (fields))
		{
		  MYSTRING *mode_string;
		  
		  mode_string = get_type (TREE_TYPE (fields));
		  APPEND (result, mode_string->str);
		  FREE (mode_string);
		  if (TREE_CHAIN (fields) != NULL_TREE)
		    APPEND (result, ", ");
		}
	      APPEND (result, ")");
	    }
	  sigdest = IDENTIFIER_SIGNAL_DEST (signame);
	  if (sigdest != NULL_TREE)
	    {
	      APPEND (result, " TO ");
	      APPEND (result, IDENTIFIER_POINTER (DECL_NAME (sigdest)));
	    }
	}
      else
	{
	  /* avoid defining a mode as itself */
	  if (CH_NOVELTY (TREE_TYPE (decl)) == decl)
	    APPEND (result, "NEWMODE ");
	  else
	    APPEND (result, "SYNMODE ");
	  APPEND (result, IDENTIFIER_POINTER (DECL_NAME (decl)));
	  APPEND (result, " = ");
	  mode_string = decode_mode (TREE_TYPE (decl));
	  APPEND (result, mode_string->str);
	  FREE (mode_string);
	}
      break;
      
    case FUNCTION_DECL:
      {
	tree	args;
	
	type = TREE_TYPE (decl);
	args = TYPE_ARG_TYPES (type);
	
	APPEND (result, IDENTIFIER_POINTER (DECL_NAME (decl)));
	
	if (CH_DECL_PROCESS (decl))
	  APPEND (result, ": PROCESS (");
	else
	  APPEND (result, ": PROC (");

	args = TYPE_ARG_TYPES (type);
	
	mode_string = print_proc_tail (type, args, 1);
	APPEND (result, mode_string->str);
	FREE (mode_string);
	
	/* generality */
	if (CH_DECL_GENERAL (decl))
	  APPEND (result, " GENERAL");
	if (CH_DECL_SIMPLE (decl))
	  APPEND (result, " SIMPLE");
	if (DECL_INLINE (decl))
	  APPEND (result, " INLINE");
	if (CH_DECL_RECURSIVE (decl))
	  APPEND (result, " RECURSIVE");
	APPEND (result, " END");
      }
      break;
      
    case FIELD_DECL:
      APPEND (result, IDENTIFIER_POINTER (DECL_NAME (decl)));
      APPEND (result, " ");
      mode_string = get_type (TREE_TYPE (decl));
      APPEND (result, mode_string->str);
      FREE (mode_string);
      if (DECL_INITIAL (decl) != NULL_TREE)
	{
	  mode_string = decode_layout (DECL_INITIAL (decl));
	  APPEND (result, mode_string->str);
	  FREE (mode_string);
	}
#if 0
      if (is_forbidden (DECL_NAME (decl), forbid))
	APPEND (result, " FORBID");
#endif
      break;
      
    case CONST_DECL:
      if (DECL_INITIAL (decl) == NULL_TREE 
	  || TREE_CODE (DECL_INITIAL (decl)) == ERROR_MARK)
	break;
      APPEND (result, "SYN ");
      APPEND (result, IDENTIFIER_POINTER (DECL_NAME (decl)));
      APPEND (result, " ");
      mode_string = get_type (TREE_TYPE (decl));
      APPEND (result, mode_string->str);
      FREE (mode_string);
      APPEND (result, " = ");
      mode_string = decode_constant (DECL_INITIAL (decl));
      APPEND (result, mode_string->str);
      FREE (mode_string);
      break;
      
    case ALIAS_DECL:
      /* If CH_DECL_GRANTED, decl was granted into this scope, and
	 so wasn't in the source code. */
      if (!CH_DECL_GRANTED (decl))
	{
	  static int restricted = 0;
	    
	  if (DECL_SEIZEFILE (decl) != use_seizefile_name
	      && DECL_SEIZEFILE (decl))
	    {
	      use_seizefile_name = DECL_SEIZEFILE (decl);
	      restricted = use_seizefile_name == NULL_TREE ? 0 : CH_USE_SEIZEFILE_RESTRICTED (use_seizefile_name);
	      if (! restricted)
	        grant_use_seizefile (IDENTIFIER_POINTER (use_seizefile_name));
	      mark_use_seizefile_written (use_seizefile_name);
	    }
	  if (! restricted)
	    {
	      APPEND (result, "SEIZE ");
	      mode_string = decode_prefix_rename (decl);
	      APPEND (result, mode_string->str);
	      FREE (mode_string);
	    }
	}
      break;

    default:
      APPEND (result, "----- not implemented ------");
      break;
    }
  return (result);
}

static MYSTRING *
decode_decl_selective (decl, all_decls)
    tree decl;
    tree all_decls;
{
  MYSTRING *result = newstring ("");
  MYSTRING *mode_string;
  tree      type;

  if (CH_ALREADY_GRANTED (decl))
    /* do nothing */
    return result;

  CH_ALREADY_GRANTED (decl) = 1;

  switch ((int)TREE_CODE (decl))
    {
    case VAR_DECL:
    case BASED_DECL:
      mode_string = get_type_selective (TREE_TYPE (decl), all_decls);
      if (mode_string->len)
	APPEND (result, mode_string->str);
      FREE (mode_string);
      if ((enum chill_tree_code)TREE_CODE (decl) == BASED_DECL)
        {
	  mode_string = decode_mode_selective (DECL_ABSTRACT_ORIGIN (decl), all_decls);
	  if (mode_string->len)
	    PREPEND (result, mode_string->str);
	  FREE (mode_string);
        }
      break;

    case TYPE_DECL:
      if (CH_DECL_SIGNAL (decl))
	{
	  /* this is really a signal */
	  tree fields = TYPE_FIELDS (TREE_TYPE (decl));
	  tree signame = DECL_NAME (decl);
	  tree sigdest;
	  
	  if (IDENTIFIER_SIGNAL_DATA (signame))
	    {
	      for ( ; fields != NULL_TREE;
		   fields = TREE_CHAIN (fields))
		{
		  MYSTRING *mode_string;
		  
		  mode_string = get_type_selective (TREE_TYPE (fields),
						    all_decls);
		  if (mode_string->len)
		    APPEND (result, mode_string->str);
		  FREE (mode_string);
		}
	    }
	  sigdest = IDENTIFIER_SIGNAL_DEST (signame);
	  if (sigdest != NULL_TREE)
	    {
	      mode_string = decode_mode_selective (DECL_NAME (sigdest), all_decls);
	      if (mode_string->len)
		{
		  MAYBE_NEWLINE (result);
		  APPEND (result, mode_string->str);
		}
	      FREE (mode_string);
	    }
	}
      else
	{
	  /* avoid defining a mode as itself */
	  mode_string = decode_mode_selective (TREE_TYPE (decl), all_decls);
	  APPEND (result, mode_string->str);
	  FREE (mode_string);
	}
      break;
      
    case FUNCTION_DECL:
      {
	tree	args;
	
	type = TREE_TYPE (decl);
	args = TYPE_ARG_TYPES (type);
	
	args = TYPE_ARG_TYPES (type);
	
	mode_string = print_proc_tail_selective (type, args, all_decls);
	if (mode_string->len)
	  APPEND (result, mode_string->str);
	FREE (mode_string);
      }
      break;
      
    case FIELD_DECL:
      mode_string = get_type_selective (TREE_TYPE (decl), all_decls);
      if (mode_string->len)
	APPEND (result, mode_string->str);
      FREE (mode_string);
      break;
      
    case CONST_DECL:
      if (DECL_INITIAL (decl) == NULL_TREE 
	  || TREE_CODE (DECL_INITIAL (decl)) == ERROR_MARK)
	break;
      mode_string = get_type_selective (TREE_TYPE (decl), all_decls);
      if (mode_string->len)
	APPEND (result, mode_string->str);
      FREE (mode_string);
      mode_string = decode_constant_selective (DECL_INITIAL (decl), all_decls);
      if (mode_string->len)
	{
	  MAYBE_NEWLINE (result);
	  APPEND (result, mode_string->str);
	}
      FREE (mode_string);
      break;
      
    }
  MAYBE_NEWLINE (result);
  return (result);
}

static void
globalize_decl (decl)
    tree	decl;
{
  if (!TREE_PUBLIC (decl) && DECL_NAME (decl) &&
      (TREE_CODE (decl) == VAR_DECL || TREE_CODE (decl) == FUNCTION_DECL))
    {
      extern	FILE	*asm_out_file;
      extern	char	*first_global_object_name;
      const char	*name = XSTR (XEXP (DECL_RTL (decl), 0), 0);
	
      if (!first_global_object_name)
	first_global_object_name = name + (name[0] == '*');
      ASM_GLOBALIZE_LABEL (asm_out_file, name);
    }
}


static void
grant_one_decl (decl)
    tree	decl;
{
  MYSTRING	*result;

  if (DECL_SOURCE_LINE (decl) == 0)
    return;
  result = decode_decl (decl);
  if (result->len)
    {
      APPEND (result, ";\n");
      APPEND (gstring, result->str);
    }
  FREE (result);
}

static void
grant_one_decl_selective (decl, all_decls)
     tree decl;
     tree all_decls;
{
  MYSTRING *result;
  MYSTRING *fixups;

  tree     d = DECL_ABSTRACT_ORIGIN (decl);

  if (CH_ALREADY_GRANTED (d))
    /* already done */
    return;

  result = decode_decl (d);
  if (!result->len)
    {
      /* nothing to do */
      FREE (result);
      return;
    }

  APPEND (result, ";\n");

  /* now process all undefined items in the decl */
  fixups = decode_decl_selective (d, all_decls);
  if (fixups->len)
    {
      PREPEND (result, fixups->str);
    }
  FREE (fixups);

  /* we have finished a decl */
  APPEND (selective_gstring, result->str);
  FREE (result);
}

static int
compare_memory_file (fname, buf)
    const char	*fname;
    const char	*buf;
{
  FILE	*fb;
  int		c;

  /* check if we have something to write */
  if (!buf || !strlen (buf))
    return (0);
    
  if ((fb = fopen (fname, "r")) == NULL)
    return (1);
    
  while ((c = getc (fb)) != EOF)
    {
      if (c != *buf++)
	{
	  fclose (fb);
	  return (1);
	}
    }
  fclose (fb);
  return (*buf ? 1 : 0);
}

void
write_grant_file ()
{
  FILE	*fb;

  /* We only write out the grant file if it has changed,
     to avoid changing its time-stamp and triggering an
     unnecessary 'make' action.  Return if no change. */
  if (gstring == NULL || !spec_module_generated ||
      !compare_memory_file (grant_file_name, gstring->str))
    return;

  fb = fopen (grant_file_name, "w");
  if (fb == NULL)
      pfatal_with_name (grant_file_name);
    
  /* write file. Due to problems with record sizes on VAX/VMS
     write string to '\n' */
#ifdef VMS
  /* do it this way for VMS, cause of problems with
     record sizes */
  p = gstring->str;
  while (*p)
    {
      p1 = strchr (p, '\n');
      c = *++p1;
      *p1 = '\0';
      fprintf (fb, "%s", p);
      *p1 = c;
      p = p1;
    }
#else
  /* faster way to write */
  if (write (fileno (fb), gstring->str, gstring->len) < 0)
    {
      int save_errno = errno;
      unlink (grant_file_name);
      errno = save_errno;
      pfatal_with_name (grant_file_name);
    }
#endif
  fclose (fb);
}


/* handle grant statement */

void
set_default_grant_file ()
{
    char	*p, *tmp, *fname;

    if (dump_base_name)
      fname = dump_base_name; /* Probably invoked via gcc */
    else
      { /* Probably invoked directly (not via gcc) */
	fname = asm_file_name;
	if (!fname)
	  fname = main_input_filename ? main_input_filename : input_filename;
	if (!fname)
	  return;
      }

    p = strrchr (fname, '.');
    if (!p)
    {
	tmp = (char *) alloca (strlen (fname) + 10);
	strcpy (tmp, fname);
    }
    else
    {
	int	i = p - fname;
	
	tmp = (char *) alloca (i + 10);
	strncpy (tmp, fname, i);
	tmp[i] = '\0';
    }
    strcat (tmp, ".grt");
    default_grant_file = build_string (strlen (tmp), tmp);

    grant_file_name = TREE_STRING_POINTER (default_grant_file);

    if (gstring == NULL)
      gstring = newstring ("");
    if (selective_gstring == NULL)
      selective_gstring = newstring ("");
}

/* Make DECL visible under the name NAME in the (fake) outermost scope. */

void
push_granted (name, decl)
     tree name ATTRIBUTE_UNUSED, decl ATTRIBUTE_UNUSED;
{
#if 0
  IDENTIFIER_GRANTED_VALUE (name) = decl;
  granted_decls = tree_cons (name, decl, granted_decls);
#endif
}

void
chill_grant (old_prefix, new_prefix, postfix, forbid)
     tree old_prefix;
     tree new_prefix;
     tree postfix;
     tree forbid;
{
  if (pass == 1)
    {
#if 0
      tree old_name = old_prefix == NULL_TREE ? postfix
	: get_identifier3 (IDENTIFIER_POINTER (old_prefix),
			   "!", IDENTIFIER_POINTER (postfix));
      tree new_name = new_prefix == NULL_TREE ? postfix
	: get_identifier3 (IDENTIFIER_POINTER (new_prefix),
			   "!", IDENTIFIER_POINTER (postfix));
#endif
      tree alias = build_alias_decl (old_prefix, new_prefix, postfix);
      CH_DECL_GRANTED (alias) = 1;
      DECL_SEIZEFILE (alias) = current_seizefile_name;
      TREE_CHAIN (alias) = current_module->granted_decls;
      current_module->granted_decls = alias;

      if (forbid)
	warning ("FORBID is not yet implemented");  /* FIXME */
    }
}

/* flag GRANT ALL only once. Avoids search in case of GRANT ALL. */
static int grant_all_seen = 0;

/* check if a decl is in the list of granted decls. */
static int
search_in_list (name, granted_decls)
    tree name;
    tree granted_decls;
{
  tree vars;
  
  for (vars = granted_decls; vars != NULL_TREE; vars = TREE_CHAIN (vars))
    if (DECL_SOURCE_LINE (vars))
      {
	if (DECL_POSTFIX_ALL (vars))
	  {
	    grant_all_seen = 1;
	    return 1;
	  }
	else if (name == DECL_NAME (vars))
	  return 1;
      }
  /* not found */
  return 0;
}

static int
really_grant_this (decl, granted_decls)
    tree decl;
    tree granted_decls;
{
  /* we never grant labels at module level */
  if ((enum chill_tree_code)TREE_CODE (decl) == LABEL_DECL)
    return 0;

  if (grant_all_seen)
    return 1;
    
  switch ((enum chill_tree_code)TREE_CODE (decl))
    {
    case VAR_DECL:
    case BASED_DECL:
    case FUNCTION_DECL:
      return search_in_list (DECL_NAME (decl), granted_decls);
    case ALIAS_DECL:
    case CONST_DECL:
      return 1;
    case TYPE_DECL:
      if (CH_DECL_SIGNAL (decl))
	return search_in_list (DECL_NAME (decl), granted_decls);
      else
	return 1;
    default:
      break;
    }

  /* this nerver should happen */
  error_with_decl (decl, "function \"really_grant_this\" called for `%s'.");
  return 1;
}

/* Write a SPEC MODULE using the declarations in the list DECLS. */
static int header_written = 0;
#define HEADER_TEMPLATE "--\n-- WARNING: this file was generated by\n\
-- GNUCHILL version %s\n-- based on gcc version %s\n--\n"

void
write_spec_module (decls, granted_decls)
     tree decls;
     tree granted_decls;
{
  tree	 vars;
  char   *hdr;

  if (granted_decls == NULL_TREE)
    return;
  
  use_seizefile_name = NULL_TREE;

  if (!header_written)
    {
      hdr = (char*) alloca (strlen (gnuchill_version)
			    + strlen (version_string)
			    + sizeof (HEADER_TEMPLATE) /* includes \0 */);
      sprintf (hdr, HEADER_TEMPLATE, gnuchill_version, version_string);
      APPEND (gstring, hdr);
      header_written = 1;
    }      
  APPEND (gstring, IDENTIFIER_POINTER (current_module->name));
  APPEND (gstring, ": SPEC MODULE\n");

  /* first of all we look for GRANT ALL specified */
  search_in_list (NULL_TREE, granted_decls);

  if (grant_all_seen != 0)
    {
      /* write all identifiers to grant file */
      for (vars = decls; vars != NULL_TREE; vars = TREE_CHAIN (vars))
	{
	  if (DECL_SOURCE_LINE (vars))
	    {
	      if (DECL_NAME (vars))
		{
		  if ((TREE_CODE (vars) != CONST_DECL || !CH_DECL_ENUM (vars)) &&
		      really_grant_this (vars, granted_decls))
		    grant_one_decl (vars);
		}
	      else if (DECL_POSTFIX_ALL (vars))
		{
		  static int restricted = 0;
		
		  if (DECL_SEIZEFILE (vars) != use_seizefile_name
		      && DECL_SEIZEFILE (vars))
		    {
		      use_seizefile_name = DECL_SEIZEFILE (vars);
		      restricted = use_seizefile_name == NULL_TREE ? 0 : CH_USE_SEIZEFILE_RESTRICTED (use_seizefile_name);
		      if (! restricted)
			grant_use_seizefile (IDENTIFIER_POINTER (use_seizefile_name));
		      mark_use_seizefile_written (use_seizefile_name);
		    }
		  if (! restricted)
		    {
		      APPEND (gstring, "SEIZE ALL;\n");
		    }
		}
	    }
	}
    }
  else
    {
      seizefile_list *wrk, *x;

      /* do a selective write to the grantfile. This will reduce the
	 size of a grantfile and speed up compilation of 
	 modules depending on this grant file */

      if (selective_gstring == 0)
	selective_gstring = newstring ("");

      /* first of all process all SEIZE ALL's */
      for (vars = decls; vars != NULL_TREE; vars = TREE_CHAIN (vars))
	{
	  if (DECL_SOURCE_LINE (vars)
	      && DECL_POSTFIX_ALL (vars))
	    grant_seized_identifier (vars);
	}

      /* now walk through granted decls */
      granted_decls = nreverse (granted_decls);
      for (vars = granted_decls; vars != NULL_TREE; vars = TREE_CHAIN (vars))
	{
	  grant_one_decl_selective (vars, decls);
	}
      granted_decls = nreverse (granted_decls);

      /* append all SEIZES */
      wrk = selective_seizes;
      while (wrk != 0)
	{
	  x = wrk->next;
	  APPEND (gstring, wrk->seizes->str);
	  FREE (wrk->seizes);
	  free (wrk);
	  wrk = x;
	}
      selective_seizes = 0;
      
      /* append generated string to grant file */
      APPEND (gstring, selective_gstring->str);
      FREE (selective_gstring);
      selective_gstring = NULL;
    }

  for (vars = granted_decls; vars != NULL_TREE; vars = TREE_CHAIN (vars))
    if (DECL_SOURCE_LINE (vars))
      {
	MYSTRING *mode_string = decode_prefix_rename (vars);
	APPEND (gstring, "GRANT ");
	APPEND (gstring, mode_string->str);
	FREE (mode_string);
	APPEND (gstring, ";\n");
      }

  APPEND (gstring, "END;\n");
  spec_module_generated = 1;

  /* initialize this for next spec module */
  grant_all_seen = 0;
}

/*
 * after the dark comes, after all of the modules are at rest,
 * we tuck the compilation unit to bed...  A story in pass 1
 * and a hug-and-a-kiss goodnight in pass 2.
 */
void
chill_finish_compile ()
{
  tree global_list;
  tree chill_init_function;

  tasking_setup ();
  build_enum_tables ();
  
  /* We only need an initializer function for the source file if
     a) there's module-level code to be called, or
     b) tasking-related stuff to be initialized. */
  if (module_init_list != NULL_TREE || tasking_list != NULL_TREE)
    {
      extern tree initializer_type;
      static tree chill_init_name;

      /* declare the global initializer list */
      global_list = do_decl (get_identifier ("_ch_init_list"),
			     build_chill_pointer_type (initializer_type), 1, 0,
			     NULL_TREE, 1);

      /* Now, we're building the function which is the *real*
	 constructor - if there's any module-level code in this
	 source file, the compiler puts the file's initializer entry
	 onto the global initializer list, so each module's body code
	 will eventually get called, after all of the processes have
	 been started up.  */
      
      /* This is better done in pass 2 (when first_global_object_name
	 may have been set), but that is too late.
	 Perhaps rewrite this so nothing is done in pass 1. */
      if (pass == 1)
	{
	  extern char *first_global_object_name;
	  /* If we don't do this spoof, we get the name of the first
	     tasking_code variable, and not the file name. */
	  char *tmp = first_global_object_name;

	  first_global_object_name = NULL;
	  chill_init_name = get_file_function_name ('I');
	  first_global_object_name = tmp;
	  /* strip off the file's extension, if any. */
	  tmp = strrchr (IDENTIFIER_POINTER (chill_init_name), '.');
	  if (tmp)
	    *tmp = '\0';
	}

      start_chill_function (chill_init_name, void_type_node, NULL_TREE,
			    NULL_TREE, NULL_TREE);
      TREE_PUBLIC (current_function_decl) = 1;
      chill_init_function = current_function_decl;
      
      /* For each module that we've compiled, that had module-level 
	 code to be called, add its entry to the global initializer
	 list. */
	 
      if (pass == 2)
	{
	  tree module_init;

	  for (module_init = module_init_list;  
	       module_init != NULL_TREE;
	       module_init = TREE_CHAIN (module_init))
	    {
	      tree init_entry      = TREE_VALUE (module_init);

	      /* assign module_entry.next := _ch_init_list; */
	      expand_expr_stmt (
		build_chill_modify_expr (
		  build_component_ref (init_entry,
		    get_identifier ("__INIT_NEXT")),
		      global_list));

	      /* assign _ch_init_list := &module_entry; */
	      expand_expr_stmt (
		build_chill_modify_expr (global_list,
		  build1 (ADDR_EXPR, ptr_type_node, init_entry)));
	    }
	}

      tasking_registry ();

      make_decl_rtl (current_function_decl, NULL, 1);

      finish_chill_function ();

      if (pass == 2)
	{
	  assemble_constructor (IDENTIFIER_POINTER (chill_init_name));
	  globalize_decl (chill_init_function);
	}

      /* ready now to link decls onto this list in pass 2. */
      module_init_list = NULL_TREE;
      tasking_list = NULL_TREE;
    }
}


