/* Generate information regarding function declarations and definitions based
   on information stored in GCC's tree structure.  This code implements the
   -aux-info option.
   Copyright (C) 1989, 1991, 1994, 1995 Free Software Foundation, Inc.
   Contributed by Ron Guilmette (rfg@segfault.us.com).

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

#include <stdio.h>
#include "config.h"
#include "flags.h"
#include "tree.h"
#include "c-tree.h"

extern char* xmalloc ();

enum formals_style_enum {
  ansi,
  k_and_r_names,
  k_and_r_decls
};
typedef enum formals_style_enum formals_style;


static char* data_type;

static char * concat ();
static char * concat3 ();
static char * gen_formal_list_for_type ();
static int    deserves_ellipsis ();
static char * gen_formal_list_for_func_def ();
static char * gen_type ();
static char * gen_decl ();
void   gen_aux_info_record ();

/*  Take two strings and mash them together into a newly allocated area.  */

static char*
concat (s1, s2)
     char* s1;
     char* s2;
{
  int size1, size2;
  char* ret_val;

  if (!s1)
    s1 = "";
  if (!s2)
    s2 = "";

  size1 = strlen (s1);
  size2 = strlen (s2);
  ret_val = xmalloc (size1 + size2 + 1);
  strcpy (ret_val, s1);
  strcpy (&ret_val[size1], s2);
  return ret_val;
}

/*  Take three strings and mash them together into a newly allocated area.  */

static char*
concat3 (s1, s2, s3)
     char* s1;
     char* s2;
     char* s3;
{
  int size1, size2, size3;
  char* ret_val;

  if (!s1)
    s1 = "";
  if (!s2)
    s2 = "";
  if (!s3)
    s3 = "";

  size1 = strlen (s1);
  size2 = strlen (s2);
  size3 = strlen (s3);
  ret_val = xmalloc (size1 + size2 + size3 + 1);
  strcpy (ret_val, s1);
  strcpy (&ret_val[size1], s2);
  strcpy (&ret_val[size1+size2], s3);
  return ret_val;
}

/* Given a string representing an entire type or an entire declaration
   which only lacks the actual "data-type" specifier (at its left end),
   affix the data-type specifier to the left end of the given type
   specification or object declaration.

   Because of C language weirdness, the data-type specifier (which normally
   goes in at the very left end) may have to be slipped in just to the
   right of any leading "const" or "volatile" qualifiers (there may be more
   than one).  Actually this may not be strictly necessary because it seems
   that GCC (at least) accepts `<data-type> const foo;' and treats it the
   same as `const <data-type> foo;' but people are accustomed to seeing
   `const char *foo;' and *not* `char const *foo;' so we try to create types
   that look as expected.  */

static char*
affix_data_type (type_or_decl)
     char *type_or_decl;
{
  char *p = type_or_decl;
  char *qualifiers_then_data_type;
  char saved;

  /* Skip as many leading const's or volatile's as there are.  */

  for (;;)
    {
      if (!strncmp (p, "volatile ", 9))
        {
          p += 9;
          continue;
        }
      if (!strncmp (p, "const ", 6))
        {
          p += 6;
          continue;
        }
      break;
    }

  /* p now points to the place where we can insert the data type.  We have to
     add a blank after the data-type of course.  */

  if (p == type_or_decl)
    return concat3 (data_type, " ", type_or_decl);

  saved = *p;
  *p = '\0';
  qualifiers_then_data_type = concat (type_or_decl, data_type);
  *p = saved;
  return concat3 (qualifiers_then_data_type, " ", p);
}

/* Given a tree node which represents some "function type", generate the
   source code version of a formal parameter list (of some given style) for
   this function type.  Return the whole formal parameter list (including
   a pair of surrounding parens) as a string.   Note that if the style
   we are currently aiming for is non-ansi, then we just return a pair
   of empty parens here. */

static char*
gen_formal_list_for_type (fntype, style)
     tree fntype;
     formals_style style;
{
  char* formal_list = "";
  tree formal_type;

  if (style != ansi)
    return "()";

  formal_type = TYPE_ARG_TYPES (fntype);
  while (formal_type && TREE_VALUE (formal_type) != void_type_node)
    {
      char* this_type;

      if (*formal_list)
        formal_list = concat (formal_list, ", ");

      this_type = gen_type ("", TREE_VALUE (formal_type), ansi);
      formal_list =
          (strlen (this_type))
              ? concat (formal_list, affix_data_type (this_type))
              : concat (formal_list, data_type);

      formal_type = TREE_CHAIN (formal_type);
    }

  /* If we got to here, then we are trying to generate an ANSI style formal
     parameters list.

     New style prototyped ANSI formal parameter lists should in theory always
     contain some stuff between the opening and closing parens, even if it is
     only "void".

     The brutal truth though is that there is lots of old K&R code out there
     which contains declarations of "pointer-to-function" parameters and
     these almost never have fully specified formal parameter lists associated
     with them.  That is, the pointer-to-function parameters are declared
     with just empty parameter lists.

     In cases such as these, protoize should really insert *something* into
     the vacant parameter lists, but what?  It has no basis on which to insert
     anything in particular.

     Here, we make life easy for protoize by trying to distinguish between
     K&R empty parameter lists and new-style prototyped parameter lists
     that actually contain "void".  In the latter case we (obviously) want
     to output the "void" verbatim, and that what we do.  In the former case,
     we do our best to give protoize something nice to insert.

     This "something nice" should be something that is still valid (when
     re-compiled) but something that can clearly indicate to the user that
     more typing information (for the parameter list) should be added (by
     hand) at some convenient moment.

     The string chosen here is a comment with question marks in it.  */

  if (!*formal_list)
    {
      if (TYPE_ARG_TYPES (fntype))
        /* assert (TREE_VALUE (TYPE_ARG_TYPES (fntype)) == void_type_node);  */
        formal_list = "void";
      else
        formal_list = "/* ??? */";
    }
  else
    {
      /* If there were at least some parameters, and if the formals-types-list
         petered out to a NULL (i.e. without being terminated by a
         void_type_node) then we need to tack on an ellipsis.  */
      if (!formal_type)
        formal_list = concat (formal_list, ", ...");
    }

  return concat3 (" (", formal_list, ")");
}

/* For the generation of an ANSI prototype for a function definition, we have
   to look at the formal parameter list of the function's own "type" to
   determine if the function's formal parameter list should end with an
   ellipsis.  Given a tree node, the following function will return non-zero
   if the "function type" parameter list should end with an ellipsis.  */

static int
deserves_ellipsis (fntype)
     tree fntype;
{
  tree formal_type;

  formal_type = TYPE_ARG_TYPES (fntype);
  while (formal_type && TREE_VALUE (formal_type) != void_type_node)
    formal_type = TREE_CHAIN (formal_type);

  /* If there were at least some parameters, and if the formals-types-list
     petered out to a NULL (i.e. without being terminated by a void_type_node)
     then we need to tack on an ellipsis.  */

  return (!formal_type && TYPE_ARG_TYPES (fntype));
}

/* Generate a parameter list for a function definition (in some given style).

   Note that this routine has to be separate (and different) from the code that
   generates the prototype parameter lists for function declarations, because
   in the case of a function declaration, all we have to go on is a tree node
   representing the function's own "function type".  This can tell us the types
   of all of the formal parameters for the function, but it cannot tell us the
   actual *names* of each of the formal parameters.  We need to output those
   parameter names for each function definition.

   This routine gets a pointer to a tree node which represents the actual
   declaration of the given function, and this DECL node has a list of formal
   parameter (variable) declarations attached to it.  These formal parameter
   (variable) declaration nodes give us the actual names of the formal
   parameters for the given function definition.

   This routine returns a string which is the source form for the entire
   function formal parameter list.  */

static char*
gen_formal_list_for_func_def (fndecl, style)
     tree fndecl;
     formals_style style;
{
  char* formal_list = "";
  tree formal_decl;

  formal_decl = DECL_ARGUMENTS (fndecl);
  while (formal_decl)
    {
      char *this_formal;

      if (*formal_list && ((style == ansi) || (style == k_and_r_names)))
        formal_list = concat (formal_list, ", ");
      this_formal = gen_decl (formal_decl, 0, style);
      if (style == k_and_r_decls)
        formal_list = concat3 (formal_list, this_formal, "; ");
      else
        formal_list = concat (formal_list, this_formal);
      formal_decl = TREE_CHAIN (formal_decl);
    }
  if (style == ansi)
    {
      if (!DECL_ARGUMENTS (fndecl))
        formal_list = concat (formal_list, "void");
      if (deserves_ellipsis (TREE_TYPE (fndecl)))
        formal_list = concat (formal_list, ", ...");
    }
  if ((style == ansi) || (style == k_and_r_names))
    formal_list = concat3 (" (", formal_list, ")");
  return formal_list;
}

/* Generate a string which is the source code form for a given type (t).  This
   routine is ugly and complex because the C syntax for declarations is ugly
   and complex.  This routine is straightforward so long as *no* pointer types,
   array types, or function types are involved.

   In the simple cases, this routine will return the (string) value which was
   passed in as the "ret_val" argument.  Usually, this starts out either as an
   empty string, or as the name of the declared item (i.e. the formal function
   parameter variable).

   This routine will also return with the global variable "data_type" set to
   some string value which is the "basic" data-type of the given complete type.
   This "data_type" string can be concatenated onto the front of the returned
   string after this routine returns to its caller.

   In complicated cases involving pointer types, array types, or function
   types, the C declaration syntax requires an "inside out" approach, i.e. if
   you have a type which is a "pointer-to-function" type, you need to handle
   the "pointer" part first, but it also has to be "innermost" (relative to
   the declaration stuff for the "function" type).  Thus, is this case, you
   must prepend a "(*" and append a ")" to the name of the item (i.e. formal
   variable).  Then you must append and prepend the other info for the
   "function type" part of the overall type.

   To handle the "innermost precedence" rules of complicated C declarators, we
   do the following (in this routine).  The input parameter called "ret_val"
   is treated as a "seed".  Each time gen_type is called (perhaps recursively)
   some additional strings may be appended or prepended (or both) to the "seed"
   string.  If yet another (lower) level of the GCC tree exists for the given
   type (as in the case of a pointer type, an array type, or a function type)
   then the (wrapped) seed is passed to a (recursive) invocation of gen_type()
   this recursive invocation may again "wrap" the (new) seed with yet more
   declarator stuff, by appending, prepending (or both).  By the time the
   recursion bottoms out, the "seed value" at that point will have a value
   which is (almost) the complete source version of the declarator (except
   for the data_type info).  Thus, this deepest "seed" value is simply passed
   back up through all of the recursive calls until it is given (as the return
   value) to the initial caller of the gen_type() routine.  All that remains
   to do at this point is for the initial caller to prepend the "data_type"
   string onto the returned "seed".  */

static char*
gen_type (ret_val, t, style)
     char* ret_val;
     tree t;
     formals_style style;
{
  tree chain_p;

  if (TYPE_NAME (t) && DECL_NAME (TYPE_NAME (t)))
    data_type = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (t)));
  else
    {
      switch (TREE_CODE (t))
        {
        case POINTER_TYPE:
          if (TYPE_READONLY (t))
            ret_val = concat ("const ", ret_val);
          if (TYPE_VOLATILE (t))
            ret_val = concat ("volatile ", ret_val);

          ret_val = concat ("*", ret_val);

	  if (TREE_CODE (TREE_TYPE (t)) == ARRAY_TYPE || TREE_CODE (TREE_TYPE (t)) == FUNCTION_TYPE)
	    ret_val = concat3 ("(", ret_val, ")");

          ret_val = gen_type (ret_val, TREE_TYPE (t), style);

          return ret_val;

        case ARRAY_TYPE:
	  if (TYPE_SIZE (t) == 0 || TREE_CODE (TYPE_SIZE (t)) != INTEGER_CST)
	    ret_val = gen_type (concat (ret_val, "[]"), TREE_TYPE (t), style);
	  else if (int_size_in_bytes (t) == 0)
	    ret_val = gen_type (concat (ret_val, "[0]"), TREE_TYPE (t), style);
	  else
	    {
	      int size = (int_size_in_bytes (t) / int_size_in_bytes (TREE_TYPE (t)));
	      char buff[10];
	      sprintf (buff, "[%d]", size);
	      ret_val = gen_type (concat (ret_val, buff),
				  TREE_TYPE (t), style);
	    }
          break;

        case FUNCTION_TYPE:
          ret_val = gen_type (concat (ret_val, gen_formal_list_for_type (t, style)), TREE_TYPE (t), style);
          break;

        case IDENTIFIER_NODE:
          data_type = IDENTIFIER_POINTER (t);
          break;

	/* The following three cases are complicated by the fact that a
           user may do something really stupid, like creating a brand new
           "anonymous" type specification in a formal argument list (or as
           part of a function return type specification).  For example:

		int f (enum { red, green, blue } color);

	   In such cases, we have no name that we can put into the prototype
	   to represent the (anonymous) type.  Thus, we have to generate the
	   whole darn type specification.  Yuck!  */

        case RECORD_TYPE:
	  if (TYPE_NAME (t))
	    data_type = IDENTIFIER_POINTER (TYPE_NAME (t));
	  else
	    {
	      data_type = "";
	      chain_p = TYPE_FIELDS (t);
	      while (chain_p)
		{
		  data_type = concat (data_type, gen_decl (chain_p, 0, ansi));
		  chain_p = TREE_CHAIN (chain_p);
		  data_type = concat (data_type, "; ");
		}
	      data_type = concat3 ("{ ", data_type, "}");
	    }
	  data_type = concat ("struct ", data_type);
	  break;

        case UNION_TYPE:
	  if (TYPE_NAME (t))
	    data_type = IDENTIFIER_POINTER (TYPE_NAME (t));
	  else
	    {
	      data_type = "";
	      chain_p = TYPE_FIELDS (t);
	      while (chain_p)
		{
		  data_type = concat (data_type, gen_decl (chain_p, 0, ansi));
		  chain_p = TREE_CHAIN (chain_p);
		  data_type = concat (data_type, "; ");
		}
	      data_type = concat3 ("{ ", data_type, "}");
	    }
	  data_type = concat ("union ", data_type);
	  break;

        case ENUMERAL_TYPE:
	  if (TYPE_NAME (t))
	    data_type = IDENTIFIER_POINTER (TYPE_NAME (t));
	  else
	    {
	      data_type = "";
	      chain_p = TYPE_VALUES (t);
	      while (chain_p)
		{
		  data_type = concat (data_type,
			IDENTIFIER_POINTER (TREE_PURPOSE (chain_p)));
		  chain_p = TREE_CHAIN (chain_p);
		  if (chain_p)
		    data_type = concat (data_type, ", ");
		}
	      data_type = concat3 ("{ ", data_type, " }");
	    }
	  data_type = concat ("enum ", data_type);
	  break;

        case TYPE_DECL:
          data_type = IDENTIFIER_POINTER (DECL_NAME (t));
          break;
 
        case INTEGER_TYPE:
          data_type = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (t)));
          /* Normally, `unsigned' is part of the deal.  Not so if it comes
    	     with `const' or `volatile'.  */
          if (TREE_UNSIGNED (t) && (TYPE_READONLY (t) || TYPE_VOLATILE (t)))
    	    data_type = concat ("unsigned ", data_type);
	  break;

        case REAL_TYPE:
          data_type = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (t)));
          break;

        case VOID_TYPE:
          data_type = "void";
          break;

	case ERROR_MARK:
	  data_type = "[ERROR]";
	  break;

        default:
          abort ();
        }
    }
  if (TYPE_READONLY (t))
    ret_val = concat ("const ", ret_val);
  if (TYPE_VOLATILE (t))
    ret_val = concat ("volatile ", ret_val);
  return ret_val;
}

/* Generate a string (source) representation of an entire entity declaration
   (using some particular style for function types).

   The given entity may be either a variable or a function.

   If the "is_func_definition" parameter is non-zero, assume that the thing
   we are generating a declaration for is a FUNCTION_DECL node which is
   associated with a function definition.  In this case, we can assume that
   an attached list of DECL nodes for function formal arguments is present.  */

static char*
gen_decl (decl, is_func_definition, style)
     tree decl;
     int is_func_definition;
     formals_style style;
{
  char* ret_val;

  if (DECL_NAME (decl))
    ret_val = IDENTIFIER_POINTER (DECL_NAME (decl));
  else
    ret_val = "";

  /* If we are just generating a list of names of formal parameters, we can
     simply return the formal parameter name (with no typing information
     attached to it) now.  */

  if (style == k_and_r_names)
    return ret_val;

  /* Note that for the declaration of some entity (either a function or a
     data object, like for instance a parameter) if the entity itself was
     declared as either const or volatile, then const and volatile properties
     are associated with just the declaration of the entity, and *not* with
     the `type' of the entity.  Thus, for such declared entities, we have to
     generate the qualifiers here.  */

  if (TREE_THIS_VOLATILE (decl))
    ret_val = concat ("volatile ", ret_val);
  if (TREE_READONLY (decl))
    ret_val = concat ("const ", ret_val);

  data_type = "";

  /* For FUNCTION_DECL nodes, there are two possible cases here.  First, if
     this FUNCTION_DECL node was generated from a function "definition", then
     we will have a list of DECL_NODE's, one for each of the function's formal
     parameters.  In this case, we can print out not only the types of each
     formal, but also each formal's name.  In the second case, this
     FUNCTION_DECL node came from an actual function declaration (and *not*
     a definition).  In this case, we do nothing here because the formal
     argument type-list will be output later, when the "type" of the function
     is added to the string we are building.  Note that the ANSI-style formal
     parameter list is considered to be a (suffix) part of the "type" of the
     function.  */

  if (TREE_CODE (decl) == FUNCTION_DECL && is_func_definition)
    {
      ret_val = concat (ret_val, gen_formal_list_for_func_def (decl, ansi));

      /* Since we have already added in the formals list stuff, here we don't
         add the whole "type" of the function we are considering (which
         would include its parameter-list info), rather, we only add in
         the "type" of the "type" of the function, which is really just
         the return-type of the function (and does not include the parameter
         list info).  */

      ret_val = gen_type (ret_val, TREE_TYPE (TREE_TYPE (decl)), style);
    }
  else
    ret_val = gen_type (ret_val, TREE_TYPE (decl), style);

  ret_val = affix_data_type (ret_val);

  if (DECL_REGISTER (decl))
    ret_val = concat ("register ", ret_val);
  if (TREE_PUBLIC (decl))
    ret_val = concat ("extern ", ret_val);
  if (TREE_CODE (decl) == FUNCTION_DECL && !TREE_PUBLIC (decl))
    ret_val = concat ("static ", ret_val);

  return ret_val;
}

extern FILE* aux_info_file;

/* Generate and write a new line of info to the aux-info (.X) file.  This
   routine is called once for each function declaration, and once for each
   function definition (even the implicit ones).  */

void
gen_aux_info_record (fndecl, is_definition, is_implicit, is_prototyped)
     tree fndecl;
     int is_definition;
     int is_implicit;
     int is_prototyped;
{
  if (flag_gen_aux_info)
    {
      static int compiled_from_record = 0;

      /* Each output .X file must have a header line.  Write one now if we
	 have not yet done so.  */

      if (! compiled_from_record++)
	{
	  /* The first line tells which directory file names are relative to.
	     Currently, -aux-info works only for files in the working
	     directory, so just use a `.' as a placeholder for now.  */
	  fprintf (aux_info_file, "/* compiled from: . */\n");
	}

      /* Write the actual line of auxiliary info.  */

      fprintf (aux_info_file, "/* %s:%d:%c%c */ %s;",
	       DECL_SOURCE_FILE (fndecl),
	       DECL_SOURCE_LINE (fndecl),
	       (is_implicit) ? 'I' : (is_prototyped) ? 'N' : 'O',
	       (is_definition) ? 'F' : 'C',
	       gen_decl (fndecl, is_definition, ansi));

      /* If this is an explicit function declaration, we need to also write
	 out an old-style (i.e. K&R) function header, just in case the user
	 wants to run unprotoize.  */

      if (is_definition)
	{
	  fprintf (aux_info_file, " /*%s %s*/",
		   gen_formal_list_for_func_def (fndecl, k_and_r_names),
		   gen_formal_list_for_func_def (fndecl, k_and_r_decls));
	}

      fprintf (aux_info_file, "\n");
    }
}
