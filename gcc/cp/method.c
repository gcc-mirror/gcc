/* Handle the hair of processing (but not expanding) inline functions.
   Also manage function and variable name overloading.
   Copyright (C) 1987, 89, 92-96, 1997 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com)

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


#ifndef PARM_CAN_BE_ARRAY_TYPE
#define PARM_CAN_BE_ARRAY_TYPE 1
#endif

/* Handle method declarations.  */
#include "config.h"
#include <stdio.h>
#include "tree.h"
#include "cp-tree.h"
#include "class.h"
#include "obstack.h"
#include <ctype.h>
#include "rtl.h"
#include "expr.h"
#include "output.h"
#include "hard-reg-set.h"
#include "flags.h"

#ifdef HAVE_STRING_H
#include <string.h>
#else
extern char *index ();
#endif

/* TREE_LIST of the current inline functions that need to be
   processed.  */
struct pending_inline *pending_inlines;

int static_labelno;

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

/* Obstack where we build text strings for overloading, etc.  */
static struct obstack scratch_obstack;
static char *scratch_firstobj;

static void icat PROTO((HOST_WIDE_INT));
static void dicat PROTO((HOST_WIDE_INT, HOST_WIDE_INT));
static void flush_repeats PROTO((tree));
static void build_overload_identifier PROTO((tree));
static void build_overload_nested_name PROTO((tree));
static void build_overload_int PROTO((tree, int));
static void build_overload_identifier PROTO((tree));
static void build_qualified_name PROTO((tree));
static void build_overload_value PROTO((tree, tree, int));
static char *thunk_printable_name PROTO((tree));
static void do_build_assign_ref PROTO((tree));
static void do_build_copy_constructor PROTO((tree));
static tree largest_union_member PROTO((tree));
static tree build_decl_overload_real PROTO((tree, tree, tree, tree,
					    tree, int)); 
static void build_template_parm_names PROTO((tree, tree));
static void build_underscore_int PROTO((int));

# define OB_INIT() (scratch_firstobj ? (obstack_free (&scratch_obstack, scratch_firstobj), 0) : 0)
# define OB_PUTC(C) (obstack_1grow (&scratch_obstack, (C)))
# define OB_PUTC2(C1,C2)	\
  (obstack_1grow (&scratch_obstack, (C1)), obstack_1grow (&scratch_obstack, (C2)))
# define OB_PUTS(S) (obstack_grow (&scratch_obstack, (S), sizeof (S) - 1))
# define OB_PUTID(ID)  \
  (obstack_grow (&scratch_obstack, IDENTIFIER_POINTER (ID),	\
		 IDENTIFIER_LENGTH (ID)))
# define OB_PUTCP(S) (obstack_grow (&scratch_obstack, (S), strlen (S)))
# define OB_FINISH() (obstack_1grow (&scratch_obstack, '\0'))
# define OB_LAST() (obstack_next_free (&scratch_obstack)[-1])

void
init_method ()
{
  gcc_obstack_init (&scratch_obstack);
  scratch_firstobj = (char *)obstack_alloc (&scratch_obstack, 0);
}

/* This must be large enough to hold any printed integer or floating-point
   value.  */
static char digit_buffer[128];

/* Move inline function definitions out of structure so that they
   can be processed normally.  CNAME is the name of the class
   we are working from, METHOD_LIST is the list of method lists
   of the structure.  We delete friend methods here, after
   saving away their inline function definitions (if any).  */

void
do_inline_function_hair (type, friend_list)
     tree type, friend_list;
{
  tree method = TYPE_METHODS (type);

  if (method && TREE_CODE (method) == TREE_VEC)
    {
      if (TREE_VEC_ELT (method, 1))
	method = TREE_VEC_ELT (method, 1);
      else if (TREE_VEC_ELT (method, 0))
	method = TREE_VEC_ELT (method, 0);
      else
	method = TREE_VEC_ELT (method, 2);
    }

  while (method)
    {
      /* Do inline member functions.  */
      struct pending_inline *info = DECL_PENDING_INLINE_INFO (method);
      if (info)
	{
	  tree args;

	  my_friendly_assert (info->fndecl == method, 238);
	  args = DECL_ARGUMENTS (method);
	  while (args)
	    {
	      DECL_CONTEXT (args) = method;
	      args = TREE_CHAIN (args);
	    }

	  /* Allow this decl to be seen in global scope.  Don't do this for
             local class methods, though.  */
	  if (! current_function_decl)
	    IDENTIFIER_GLOBAL_VALUE (DECL_ASSEMBLER_NAME (method)) = method;
	}
      method = TREE_CHAIN (method);
    }
  while (friend_list)
    {
      tree fndecl = TREE_VALUE (friend_list);
      struct pending_inline *info = DECL_PENDING_INLINE_INFO (fndecl);
      if (info)
	{
	  tree args;

	  my_friendly_assert (info->fndecl == fndecl, 239);
	  args = DECL_ARGUMENTS (fndecl);
	  while (args)
	    {
	      DECL_CONTEXT (args) = fndecl;
	      args = TREE_CHAIN (args);
	    }

	  /* Allow this decl to be seen in global scope */
	  if (! current_function_decl)
	    IDENTIFIER_GLOBAL_VALUE (DECL_ASSEMBLER_NAME (fndecl)) = fndecl;
	}

      friend_list = TREE_CHAIN (friend_list);
    }
}

/* Report an argument type mismatch between the best declared function
   we could find and the current argument list that we have.  */

void
report_type_mismatch (cp, parmtypes, name_kind)
     struct candidate *cp;
     tree parmtypes;
     char *name_kind;
{
  int i = cp->u.bad_arg;
  tree ttf, tta;
  char *tmp_firstobj;

  switch (i)
    {
    case -4:
      my_friendly_assert (TREE_CODE (cp->function) == TEMPLATE_DECL, 240);
      cp_error ("type unification failed for function template `%#D'",
		cp->function);
      return;

    case -2:
      cp_error ("too few arguments for %s `%#D'", name_kind, cp->function);
      return;
    case -1:
      cp_error ("too many arguments for %s `%#D'", name_kind, cp->function);
      return;
    case 0:
      if (TREE_CODE (TREE_TYPE (cp->function)) != METHOD_TYPE)
	break;
    case -3:
      /* Happens when the implicit object parameter is rejected.  */
      my_friendly_assert (! TYPE_READONLY (TREE_TYPE (TREE_VALUE (parmtypes))),
			  241);
      if (TYPE_VOLATILE (TREE_TYPE (TREE_TYPE (TREE_VALUE (parmtypes))))
	  && ! TYPE_VOLATILE (TREE_TYPE (TREE_VALUE (TYPE_ARG_TYPES (TREE_TYPE (cp->function))))))
	cp_error ("call to non-volatile %s `%#D' with volatile object",
		  name_kind, cp->function);
      else
	cp_error ("call to non-const %s `%#D' with const object",
		  name_kind, cp->function);
      return;
    }

  ttf = TYPE_ARG_TYPES (TREE_TYPE (cp->function));
  tta = parmtypes;

  while (i-- > 0)
    {
      ttf = TREE_CHAIN (ttf);
      tta = TREE_CHAIN (tta);
    }

  OB_INIT ();
  OB_PUTS ("bad argument ");
  sprintf (digit_buffer, "%d", cp->u.bad_arg
	   - (TREE_CODE (TREE_TYPE (cp->function)) == METHOD_TYPE)
	   + 1);
  OB_PUTCP (digit_buffer);

  OB_PUTS (" for function `");
  OB_PUTCP (decl_as_string (cp->function, 1));
  OB_PUTS ("' (type was ");

  /* Reset `i' so that type printing routines do the right thing.  */
  if (tta)
    {
      enum tree_code code = TREE_CODE (TREE_TYPE (TREE_VALUE (tta)));
      if (code == ERROR_MARK)
	OB_PUTS ("(failed type instantiation)");
      else
	{
	  i = (code == FUNCTION_TYPE || code == METHOD_TYPE);
	  OB_PUTCP (type_as_string (TREE_TYPE (TREE_VALUE (tta)), 1));
	}
    }
  else OB_PUTS ("void");
  OB_PUTC (')');
  OB_FINISH ();

  tmp_firstobj = (char *)alloca (obstack_object_size (&scratch_obstack));
  bcopy (obstack_base (&scratch_obstack), tmp_firstobj,
	 obstack_object_size (&scratch_obstack));
  error (tmp_firstobj);
}

/* Here is where overload code starts.  */

/* Array of types seen so far in top-level call to `build_overload_name'.
   Allocated and deallocated by caller.  */
static tree *typevec;

/* Number of types interned by `build_overload_name' so far.  */
static int maxtype;

/* Number of occurrences of last type seen.  */
static int nrepeats;

/* Nonzero if we should not try folding parameter types.  */
static int nofold;

#define ALLOCATE_TYPEVEC(PARMTYPES) \
  do { maxtype = 0, nrepeats = 0; \
       typevec = (tree *)alloca (list_length (PARMTYPES) * sizeof (tree)); } while (0)

#define DEALLOCATE_TYPEVEC(PARMTYPES) \
  do { tree t = (PARMTYPES); \
       while (t) { TREE_USED (TREE_VALUE (t)) = 0; t = TREE_CHAIN (t); } \
  } while (0)

/* Code to concatenate an asciified integer to a string.  */

static
#ifdef __GNUC__
__inline
#endif
void
icat (i)
     HOST_WIDE_INT i;
{
  unsigned HOST_WIDE_INT ui;

  /* Handle this case first, to go really quickly.  For many common values,
     the result of ui/10 below is 1.  */
  if (i == 1)
    {
      OB_PUTC ('1');
      return;
    }

  if (i >= 0)
    ui = i;
  else
    {
      OB_PUTC ('m');
      ui = -i;
    }

  if (ui >= 10)
    icat (ui / 10);

  OB_PUTC ('0' + (ui % 10));
}

static void
dicat (lo, hi)
     HOST_WIDE_INT lo, hi;
{
  unsigned HOST_WIDE_INT ulo, uhi, qlo, qhi;

  if (hi >= 0)
    {
      uhi = hi;
      ulo = lo;
    }
  else
    {
      uhi = (lo == 0 ? -hi : -hi-1);
      ulo = -lo;
    }
  if (uhi == 0
      && ulo < ((unsigned HOST_WIDE_INT)1 << (HOST_BITS_PER_WIDE_INT - 1)))
    {
      icat (ulo);
      return;
    }
  /* Divide 2^HOST_WIDE_INT*uhi+ulo by 10. */
  qhi = uhi / 10;
  uhi = uhi % 10;
  qlo = uhi * (((unsigned HOST_WIDE_INT)1 << (HOST_BITS_PER_WIDE_INT - 1)) / 5);
  qlo += ulo / 10;
  ulo = ulo % 10;
  ulo += uhi * (((unsigned HOST_WIDE_INT)1 << (HOST_BITS_PER_WIDE_INT - 1)) % 5)
	 * 2;
  qlo += ulo / 10;
  ulo = ulo % 10;
  /* Quotient is 2^HOST_WIDE_INT*qhi+qlo, remainder is ulo. */
  dicat (qlo, qhi);
  OB_PUTC ('0' + ulo);
}

static
#ifdef __GNUC__
__inline
#endif
void
flush_repeats (type)
     tree type;
{
  int tindex = 0;

  while (typevec[tindex] != type)
    tindex++;

  if (nrepeats > 1)
    {
      OB_PUTC ('N');
      icat (nrepeats);
      if (nrepeats > 9)
	OB_PUTC ('_');
    }
  else
    OB_PUTC ('T');
  nrepeats = 0;
  icat (tindex);
  if (tindex > 9)
    OB_PUTC ('_');
}

static int numeric_output_need_bar;

static void
build_overload_nested_name (decl)
     tree decl;
{
  if (DECL_CONTEXT (decl))
    {
      tree context = DECL_CONTEXT (decl);
      /* For a template type parameter, we want to output an 'Xn'
	 rather than 'T' or some such. */
      if (TREE_CODE (context) == TEMPLATE_TYPE_PARM)
	build_overload_name (context, 0, 0);
      else
	{
	  if (TREE_CODE_CLASS (TREE_CODE (context)) == 't')
	    context = TYPE_NAME (context);
	  build_overload_nested_name (context);
	}
    }

  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      tree name = DECL_ASSEMBLER_NAME (decl);
      char *label;

      ASM_FORMAT_PRIVATE_NAME (label, IDENTIFIER_POINTER (name), static_labelno);
      static_labelno++;

      if (numeric_output_need_bar)
	OB_PUTC ('_');
      icat (strlen (label));
      OB_PUTCP (label);
      numeric_output_need_bar = 1;
    }
  else				/* TYPE_DECL */
    build_overload_identifier (decl);
}

static void
build_underscore_int (i)
     int i;
{
  if (i > 9)
    OB_PUTC ('_');
  icat (i);
  if (i > 9)
    OB_PUTC ('_');
}

/* Encoding for an INTEGER_CST value.  */

static void
build_overload_int (value, in_template)
     tree value;
     int in_template;
{
  if (in_template && TREE_CODE (value) != INTEGER_CST)
    /* We don't ever want this output, but it's inconvenient not to
       be able to build the string.  This should cause assembler
       errors we'll notice.  */
    {
      static int n;
      sprintf (digit_buffer, " *%d", n++);
      OB_PUTCP (digit_buffer);
      return;
    }

  my_friendly_assert (TREE_CODE (value) == INTEGER_CST, 243);
  if (TYPE_PRECISION (TREE_TYPE (value)) == 2 * HOST_BITS_PER_WIDE_INT)
    {
      if (TREE_INT_CST_HIGH (value)
	  != (TREE_INT_CST_LOW (value) >> (HOST_BITS_PER_WIDE_INT - 1)))
	{
	  /* need to print a DImode value in decimal */
	  dicat (TREE_INT_CST_LOW (value), TREE_INT_CST_HIGH (value));
	  return;
	}
      /* else fall through to print in smaller mode */
    }
  /* Wordsize or smaller */
  icat (TREE_INT_CST_LOW (value));
}

static void
build_overload_value (type, value, in_template)
     tree type, value;
     int in_template;
{
  while (TREE_CODE (value) == NON_LVALUE_EXPR
	 || TREE_CODE (value) == NOP_EXPR)
    value = TREE_OPERAND (value, 0);
  my_friendly_assert (TREE_CODE (type) == PARM_DECL, 242);
  type = TREE_TYPE (type);

  if (numeric_output_need_bar)
    {
      OB_PUTC ('_');
      numeric_output_need_bar = 0;
    }

  if (TREE_CODE (value) == TEMPLATE_CONST_PARM)
    {
      OB_PUTC ('Y');
      build_underscore_int (TEMPLATE_CONST_IDX (value));
      build_underscore_int (TEMPLATE_CONST_LEVEL (value));
      return;
    }

  if (TREE_CODE (type) == POINTER_TYPE
      && TREE_CODE (TREE_TYPE (type)) == OFFSET_TYPE)
    {
      /* Handle a pointer to data member as a template instantiation
	 parameter, boy, what fun!  */
      type = integer_type_node;
      if (TREE_CODE (value) != INTEGER_CST)
	{
	  sorry ("unknown pointer to member constant");
	  return;
	}
    }

  if (TYPE_PTRMEMFUNC_P (type))
    type = TYPE_PTRMEMFUNC_FN_TYPE (type);

  switch (TREE_CODE (type))
    {
    case INTEGER_TYPE:
    case ENUMERAL_TYPE:
    case BOOLEAN_TYPE:
      {
	build_overload_int (value, in_template);
	numeric_output_need_bar = 1;
	return;
      }
    case REAL_TYPE:
      {
	REAL_VALUE_TYPE val;
	char *bufp = digit_buffer;

	pedwarn ("ANSI C++ forbids floating-point template arguments");

	my_friendly_assert (TREE_CODE (value) == REAL_CST, 244);
	val = TREE_REAL_CST (value);
	if (REAL_VALUE_ISNAN (val))
	  {
	    sprintf (bufp, "NaN");
	  }
	else
	  {
	    if (REAL_VALUE_NEGATIVE (val))
	      {
		val = REAL_VALUE_NEGATE (val);
		*bufp++ = 'm';
	      }
	    if (REAL_VALUE_ISINF (val))
	      {
		sprintf (bufp, "Infinity");
	      }
	    else
	      {
		REAL_VALUE_TO_DECIMAL (val, "%.20e", bufp);
		bufp = (char *) index (bufp, 'e');
		if (!bufp)
		  strcat (digit_buffer, "e0");
		else
		  {
		    char *p;
		    bufp++;
		    if (*bufp == '-')
		      {
			*bufp++ = 'm';
		      }
		    p = bufp;
		    if (*p == '+')
		      p++;
		    while (*p == '0')
		      p++;
		    if (*p == 0)
		      {
			*bufp++ = '0';
			*bufp = 0;
		      }
		    else if (p != bufp)
		      {
			while (*p)
			  *bufp++ = *p++;
			*bufp = 0;
		      }
		  }
#ifdef NO_DOT_IN_LABEL
		bufp = (char *) index (bufp, '.');
		if (bufp)
		  *bufp = '_';
#endif
	      }
	  }
	OB_PUTCP (digit_buffer);
	numeric_output_need_bar = 1;
	return;
      }
    case POINTER_TYPE:
      if (TREE_CODE (TREE_TYPE (type)) == METHOD_TYPE
	  && TREE_CODE (value) != ADDR_EXPR)
	{
	  if (TREE_CODE (value) == CONSTRUCTOR)
	    {
	      /* This is dangerous code, crack built up pointer to members.  */
	      tree args = CONSTRUCTOR_ELTS (value);
	      tree a1 = TREE_VALUE (args);
	      tree a2 = TREE_VALUE (TREE_CHAIN (args));
	      tree a3 = CONSTRUCTOR_ELTS (TREE_VALUE (TREE_CHAIN (TREE_CHAIN (args))));
	      a3 = TREE_VALUE (a3);
	      STRIP_NOPS (a3);
	      if (TREE_CODE (a1) == INTEGER_CST
		  && TREE_CODE (a2) == INTEGER_CST)
		{
		  build_overload_int (a1, in_template);
		  OB_PUTC ('_');
		  build_overload_int (a2, in_template);
		  OB_PUTC ('_');
		  if (TREE_CODE (a3) == ADDR_EXPR)
		    {
		      a3 = TREE_OPERAND (a3, 0);
		      if (TREE_CODE (a3) == FUNCTION_DECL)
			{
			  numeric_output_need_bar = 0;
			  build_overload_identifier (DECL_ASSEMBLER_NAME (a3));
			  return;
			}
		    }
		  else if (TREE_CODE (a3) == INTEGER_CST)
		    {
		      OB_PUTC ('i');
		      build_overload_int (a3, in_template);
		      numeric_output_need_bar = 1;
		      return;
		    }
		}
	    }
	  sorry ("template instantiation with pointer to method that is too complex");
	  return;
	}
      if (TREE_CODE (value) == INTEGER_CST
	  || TREE_CODE (value) == TEMPLATE_CONST_PARM)
	{
	  build_overload_int (value, in_template);
	  numeric_output_need_bar = 1;
	  return;
	}
      value = TREE_OPERAND (value, 0);
      if (TREE_CODE (value) == VAR_DECL)
	{
	  my_friendly_assert (DECL_NAME (value) != 0, 245);
	  build_overload_identifier (DECL_ASSEMBLER_NAME (value));
	  return;
	}
      else if (TREE_CODE (value) == FUNCTION_DECL)
	{
	  my_friendly_assert (DECL_NAME (value) != 0, 246);
	  build_overload_identifier (DECL_ASSEMBLER_NAME (value));
	  return;
	}
      else
	my_friendly_abort (71);
      break; /* not really needed */

    default:
      sorry ("conversion of %s as template parameter",
	     tree_code_name [(int) TREE_CODE (type)]);
      my_friendly_abort (72);
    }
}


/* Add encodings for the vector of template parameters in PARMLIST,
   given the vector of arguments to be substituted in ARGLIST.  */

static void
build_template_parm_names (parmlist, arglist)
     tree parmlist;
     tree arglist;
{
  int i, nparms;
  
  nparms = TREE_VEC_LENGTH (parmlist);
  icat (nparms);
  for (i = 0; i < nparms; i++)
    {
      tree parm = TREE_VALUE (TREE_VEC_ELT (parmlist, i));
      tree arg = TREE_VEC_ELT (arglist, i);
      if (TREE_CODE (parm) == TYPE_DECL)
	{
	  /* This parameter is a type.  */
	  OB_PUTC ('Z');
	  build_overload_name (arg, 0, 0);
	}
      else
	{
	  parm = tsubst (parm, arglist,
			 TREE_VEC_LENGTH (arglist), NULL_TREE);
	  /* It's a PARM_DECL.  */
	  build_overload_name (TREE_TYPE (parm), 0, 0);
	  build_overload_value (parm, arg, uses_template_parms (arglist));
	}
    }
 }


static void
build_overload_identifier (name)
     tree name;
{
  if (TREE_CODE (name) == TYPE_DECL
      && IS_AGGR_TYPE (TREE_TYPE (name))
      && CLASSTYPE_TEMPLATE_INFO (TREE_TYPE (name))
      && PRIMARY_TEMPLATE_P (CLASSTYPE_TI_TEMPLATE (TREE_TYPE (name))))
    {
      tree template, parmlist, arglist, tname;
      template = CLASSTYPE_TEMPLATE_INFO (TREE_TYPE (name));
      arglist = TREE_VALUE (template);
      template = TREE_PURPOSE (template);
      tname = DECL_NAME (template);
      parmlist = DECL_INNERMOST_TEMPLATE_PARMS (template);
      OB_PUTC ('t');
      icat (IDENTIFIER_LENGTH (tname));
      OB_PUTID (tname);
      build_template_parm_names (parmlist, arglist);
    }
  else
    {
      if (TREE_CODE (name) == TYPE_DECL)
	name = DECL_NAME (name);
      if (numeric_output_need_bar)
	{
	  OB_PUTC ('_');
	  numeric_output_need_bar = 0;
	}
      icat (IDENTIFIER_LENGTH (name));
      OB_PUTID (name);
    }
}

/* Given DECL, either a class TYPE, TYPE_DECL or FUNCTION_DECL, produce
   the mangling for it.  Used by build_overload_name and build_static_name.  */

static void
build_qualified_name (decl)
     tree decl;
{
  tree context;
  int i = 1;

  if (TREE_CODE_CLASS (TREE_CODE (decl)) == 't')
    decl = TYPE_NAME (decl);

  /* If DECL_ASSEMBLER_NAME has been set properly, use it.  */
  if (TREE_CODE (decl) == TYPE_DECL
      && DECL_ASSEMBLER_NAME (decl) != DECL_NAME (decl))
    {
      OB_PUTID (DECL_ASSEMBLER_NAME (decl));
      return;
    }

  context = decl;
  while (DECL_CONTEXT (context))
    {
      i += 1;
      context = DECL_CONTEXT (context);
      if (TREE_CODE_CLASS (TREE_CODE (context)) == 't')
	context = TYPE_NAME (context);
    }

  if (i > 1)
    {
      OB_PUTC ('Q');
      if (i > 9)
	OB_PUTC ('_');
      icat (i);
      if (i > 9)
	OB_PUTC ('_');
      numeric_output_need_bar = 0;
    }
  build_overload_nested_name (decl);
}

/* Given a list of parameters in PARMTYPES, create an unambiguous
   overload string. Should distinguish any type that C (or C++) can
   distinguish. I.e., pointers to functions are treated correctly.

   Caller must deal with whether a final `e' goes on the end or not.

   Any default conversions must take place before this function
   is called.

   BEGIN and END control initialization and finalization of the
   obstack where we build the string.  */

char *
build_overload_name (parmtypes, begin, end)
     tree parmtypes;
     int begin, end;
{
  int just_one;
  tree parmtype;

  if (begin) OB_INIT ();
  numeric_output_need_bar = 0;

  if ((just_one = (TREE_CODE (parmtypes) != TREE_LIST)))
    {
      parmtype = parmtypes;
      goto only_one;
    }

  while (parmtypes)
    {
      parmtype = TREE_VALUE (parmtypes);

    only_one:

      if (! nofold && ! just_one)
	{
	  /* Every argument gets counted.  */
	  typevec[maxtype++] = parmtype;

	  if (TREE_USED (parmtype) && parmtype == typevec[maxtype-2])
	    {
	      nrepeats++;
	      goto next;
	    }

	  if (nrepeats)
	    flush_repeats (typevec[maxtype-2]);

	  if (TREE_USED (parmtype))
	    {
#if 0
	      /* We can turn this on at some point when we want
		 improved symbol mangling.  */
	      nrepeats++;
#else
	      /* This is bug compatible with 2.7.x  */
	      flush_repeats (parmtype);
#endif
	      goto next;
	    }

	  /* Only cache types which take more than one character.  */
	  if (parmtype != TYPE_MAIN_VARIANT (parmtype)
	      || (TREE_CODE (parmtype) != INTEGER_TYPE
		  && TREE_CODE (parmtype) != REAL_TYPE))
	    TREE_USED (parmtype) = 1;
	}

      if (TYPE_PTRMEMFUNC_P (parmtype))
	parmtype = TYPE_PTRMEMFUNC_FN_TYPE (parmtype);

      if (TREE_READONLY (parmtype))
	OB_PUTC ('C');
      if (TREE_CODE (parmtype) == INTEGER_TYPE
	  && TYPE_MAIN_VARIANT (parmtype) == unsigned_type (TYPE_MAIN_VARIANT (parmtype)))
	OB_PUTC ('U');
      if (TYPE_VOLATILE (parmtype))
	OB_PUTC ('V');

      switch (TREE_CODE (parmtype))
	{
	case OFFSET_TYPE:
	  OB_PUTC ('O');
	  build_overload_name (TYPE_OFFSET_BASETYPE (parmtype), 0, 0);
	  OB_PUTC ('_');
	  build_overload_name (TREE_TYPE (parmtype), 0, 0);
	  break;

	case REFERENCE_TYPE:
	  OB_PUTC ('R');
	  goto more;

	case ARRAY_TYPE:
#if PARM_CAN_BE_ARRAY_TYPE
	  {
	    tree length;

	    OB_PUTC ('A');
	    if (TYPE_DOMAIN (parmtype) == NULL_TREE)
	      error ("pointer or reference to array of unknown bound in parm type");
	    else
	      {
		length = array_type_nelts (parmtype);
		if (TREE_CODE (length) == INTEGER_CST)
		  icat (TREE_INT_CST_LOW (length) + 1);
	      }
	    OB_PUTC ('_');
	    goto more;
	  }
#else
	  OB_PUTC ('P');
	  goto more;
#endif

	case POINTER_TYPE:
	  OB_PUTC ('P');
	more:
	  build_overload_name (TREE_TYPE (parmtype), 0, 0);
	  break;

	case FUNCTION_TYPE:
	case METHOD_TYPE:
	  {
	    tree firstarg = TYPE_ARG_TYPES (parmtype);
	    /* Otherwise have to implement reentrant typevecs,
	       unmark and remark types, etc.  */
	    int old_nofold = nofold;
	    nofold = 1;

	    if (nrepeats)
	      flush_repeats (typevec[maxtype-1]);

	    /* @@ It may be possible to pass a function type in
	       which is not preceded by a 'P'.  */
	    if (TREE_CODE (parmtype) == FUNCTION_TYPE)
	      {
		OB_PUTC ('F');
		if (firstarg == NULL_TREE)
		  OB_PUTC ('e');
		else if (firstarg == void_list_node)
		  OB_PUTC ('v');
		else
		  build_overload_name (firstarg, 0, 0);
	      }
	    else
	      {
		int constp = TYPE_READONLY (TREE_TYPE (TREE_VALUE (firstarg)));
		int volatilep = TYPE_VOLATILE (TREE_TYPE (TREE_VALUE (firstarg)));
		OB_PUTC ('M');
		firstarg = TREE_CHAIN (firstarg);

		build_overload_name (TYPE_METHOD_BASETYPE (parmtype), 0, 0);
		if (constp)
		  OB_PUTC ('C');
		if (volatilep)
		  OB_PUTC ('V');

		/* For cfront 2.0 compatibility.  */
		OB_PUTC ('F');

		if (firstarg == NULL_TREE)
		  OB_PUTC ('e');
		else if (firstarg == void_list_node)
		  OB_PUTC ('v');
		else
		  build_overload_name (firstarg, 0, 0);
	      }

	    /* Separate args from return type.  */
	    OB_PUTC ('_');
	    build_overload_name (TREE_TYPE (parmtype), 0, 0);
	    nofold = old_nofold;
	    break;
	  }

	case INTEGER_TYPE:
	  parmtype = TYPE_MAIN_VARIANT (parmtype);
	  if (parmtype == integer_type_node
	      || parmtype == unsigned_type_node)
	    OB_PUTC ('i');
	  else if (parmtype == long_integer_type_node
		   || parmtype == long_unsigned_type_node)
	    OB_PUTC ('l');
	  else if (parmtype == short_integer_type_node
		   || parmtype == short_unsigned_type_node)
	    OB_PUTC ('s');
	  else if (parmtype == signed_char_type_node)
	    {
	      OB_PUTC ('S');
	      OB_PUTC ('c');
	    }
	  else if (parmtype == char_type_node
		   || parmtype == unsigned_char_type_node)
	    OB_PUTC ('c');
	  else if (parmtype == wchar_type_node)
	    OB_PUTC ('w');
	  else if (parmtype == long_long_integer_type_node
	      || parmtype == long_long_unsigned_type_node)
	    OB_PUTC ('x');
#if 0
	  /* it would seem there is no way to enter these in source code,
	     yet.  (mrs) */
	  else if (parmtype == long_long_long_integer_type_node
	      || parmtype == long_long_long_unsigned_type_node)
	    OB_PUTC ('q');
#endif
	  else
	    my_friendly_abort (73);
	  break;

	case BOOLEAN_TYPE:
	  OB_PUTC ('b');
	  break;

	case REAL_TYPE:
	  parmtype = TYPE_MAIN_VARIANT (parmtype);
	  if (parmtype == long_double_type_node)
	    OB_PUTC ('r');
	  else if (parmtype == double_type_node)
	    OB_PUTC ('d');
	  else if (parmtype == float_type_node)
	    OB_PUTC ('f');
	  else my_friendly_abort (74);
	  break;

	case COMPLEX_TYPE:
	  OB_PUTC ('J');
	  build_overload_name (TREE_TYPE (parmtype), 0, 0);
	  break;

	case VOID_TYPE:
	  if (! just_one)
	    {
#if 0
	      extern tree void_list_node;

	      /* See if anybody is wasting memory.  */
	      my_friendly_assert (parmtypes == void_list_node, 247);
#endif
	      /* This is the end of a parameter list.  */
	      if (end) OB_FINISH ();
	      return (char *)obstack_base (&scratch_obstack);
	    }
	  OB_PUTC ('v');
	  break;

	case ERROR_MARK:	/* not right, but nothing is anyway */
	  break;

	  /* have to do these */
	case UNION_TYPE:
	case RECORD_TYPE:
	  if (! just_one)
	    /* Make this type signature look incompatible
	       with AT&T.  */
	    OB_PUTC ('G');
	  goto common;
	case ENUMERAL_TYPE:
	common:
	  {
	    tree name = TYPE_NAME (parmtype);

	    if (TREE_CODE (name) == IDENTIFIER_NODE)
	      {
		build_overload_identifier (TYPE_NAME (parmtype));
		break;
	      }
	    my_friendly_assert (TREE_CODE (name) == TYPE_DECL, 248);

	    build_qualified_name (name);
	    break;
	  }

	case UNKNOWN_TYPE:
	  /* We can get here if __null is defined to have type ({unkown
	     type}*), which it is if -ansi is not used.  Treat this
	     like 'void*'.  */
	  OB_PUTC ('v');
	  break;

	case TEMPLATE_TYPE_PARM:
	  OB_PUTC ('X');
	  build_underscore_int (TEMPLATE_TYPE_IDX (parmtype));
	  build_underscore_int (TEMPLATE_TYPE_LEVEL (parmtype));
	  break;
	    
	case TYPENAME_TYPE:
	  /* When mangling the type of a function template whose
	     declaration looks like:

	     template <class T> void foo(typename T::U)
	     
	     we have to mangle these.  */
	  build_qualified_name (parmtype);
	  break;

	default:
	  my_friendly_abort (75);
	}

    next:
      if (just_one) break;
      parmtypes = TREE_CHAIN (parmtypes);
    }
  if (! just_one)
    {
      if (nrepeats)
	flush_repeats (typevec[maxtype-1]);

      /* To get here, parms must end with `...'.  */
      OB_PUTC ('e');
    }

  if (end) OB_FINISH ();
  return (char *)obstack_base (&scratch_obstack);
}

/* Produce the mangling for a variable named NAME in CONTEXT, which can
   be either a class TYPE or a FUNCTION_DECL.  */

tree
build_static_name (context, name)
     tree context, name;
{
  OB_INIT ();
  numeric_output_need_bar = 0;
#ifdef JOINER
  OB_PUTC ('_');
  build_qualified_name (context);
  OB_PUTC (JOINER);
#else
  OB_PUTS ("__static_");
  build_qualified_name (context);
  OB_PUTC ('_');
#endif
  OB_PUTID (name);
  OB_FINISH ();

  return get_identifier ((char *)obstack_base (&scratch_obstack));
}

static tree 
build_decl_overload_real (dname, parms, ret_type, tparms, targs,
			  for_method) 
     tree dname;
     tree parms;
     tree ret_type;
     tree tparms;
     tree targs;
     int for_method;
{
  char *name = IDENTIFIER_POINTER (dname);

  /* member operators new and delete look like methods at this point.  */
  if (! for_method && parms != NULL_TREE && TREE_CODE (parms) == TREE_LIST)
    {
      if (dname == ansi_opname[(int) DELETE_EXPR])
	return get_identifier ("__builtin_delete");
      else if (dname == ansi_opname[(int) VEC_DELETE_EXPR])
	return get_identifier ("__builtin_vec_delete");
      else if (TREE_CHAIN (parms) == void_list_node)
	{
	  if (dname == ansi_opname[(int) NEW_EXPR])
	    return get_identifier ("__builtin_new");
	  else if (dname == ansi_opname[(int) VEC_NEW_EXPR])
	    return get_identifier ("__builtin_vec_new");
	}
    }

  OB_INIT ();
  if (for_method != 2)
    OB_PUTCP (name);
  /* Otherwise, we can divine that this is a constructor,
     and figure out its name without any extra encoding.  */

  OB_PUTC2 ('_', '_');
  if (for_method)
    {
#if 0
      /* We can get away without doing this.  */
      OB_PUTC ('M');
#endif
      if (tparms != NULL_TREE)
	OB_PUTC ('H');
      {
	tree this_type = TREE_VALUE (parms);

	if (TREE_CODE (this_type) == RECORD_TYPE)  /* a signature pointer */
	  parms = temp_tree_cons (NULL_TREE, SIGNATURE_TYPE (this_type),
				  TREE_CHAIN (parms));
	else
	  parms = temp_tree_cons (NULL_TREE, TREE_TYPE (this_type),
				  TREE_CHAIN (parms));
      }
    }
  else if (tparms)
    OB_PUTC ('H');
  else
    OB_PUTC ('F');

  if (tparms)
    {
      build_template_parm_names (tparms, targs);
      OB_PUTC ('_');
    }

  if (parms == NULL_TREE)
    OB_PUTC ('e');
  else if (parms == void_list_node)
    OB_PUTC ('v');
  else
    {
      ALLOCATE_TYPEVEC (parms);
      nofold = 0;
      if (for_method)
	{
	  build_overload_name (TREE_VALUE (parms), 0, 0);

	  typevec[maxtype++] = TREE_VALUE (parms);
	  TREE_USED (TREE_VALUE (parms)) = 1;

	  if (TREE_CHAIN (parms))
	    build_overload_name (TREE_CHAIN (parms), 0, 0);
	  else
	    OB_PUTC ('e');
	}
      else
	build_overload_name (parms, 0, 0);
      DEALLOCATE_TYPEVEC (parms);
    }

  if (ret_type != NULL_TREE && for_method != 2)
    {
      /* Add the return type. */
      OB_PUTC ('_');
      build_overload_name (ret_type, 0, 0);
    }

  OB_FINISH ();
  {
    tree n = get_identifier (obstack_base (&scratch_obstack));
    if (IDENTIFIER_OPNAME_P (dname))
      IDENTIFIER_OPNAME_P (n) = 1;
    return n;
  }
}

/* Change the name of a function definition so that it may be
   overloaded. NAME is the name of the function to overload,
   PARMS is the parameter list (which determines what name the
   final function obtains).

   FOR_METHOD is 1 if this overload is being performed
   for a method, rather than a function type.  It is 2 if
   this overload is being performed for a constructor.  */

tree
build_decl_overload (dname, parms, for_method)
     tree dname;
     tree parms;
     int for_method;
{
  return build_decl_overload_real (dname, parms, NULL_TREE, NULL_TREE,
				   NULL_TREE, for_method); 
}


/* Like build_decl_overload, but for template functions. */

tree
build_template_decl_overload (dname, parms, ret_type, tparms, targs,
			      for_method) 
     tree dname;
     tree parms;
     tree ret_type;
     tree tparms;
     tree targs;
     int for_method;
{
  return build_decl_overload_real (dname, parms, ret_type, tparms, targs,
				   for_method); 
}


/* Build an overload name for the type expression TYPE.  */

tree
build_typename_overload (type)
     tree type;
{
  tree id;

  OB_INIT ();
  OB_PUTID (ansi_opname[(int) TYPE_EXPR]);
  nofold = 1;
  build_overload_name (type, 0, 1);
  id = get_identifier (obstack_base (&scratch_obstack));
  IDENTIFIER_OPNAME_P (id) = 1;
#if 0
  IDENTIFIER_GLOBAL_VALUE (id) = TYPE_MAIN_DECL (type);
#endif
  TREE_TYPE (id) = type;
  return id;
}

tree
build_overload_with_type (name, type)
     tree name, type;
{
  OB_INIT ();
  OB_PUTID (name);
  nofold = 1;

  build_overload_name (type, 0, 1);
  return get_identifier (obstack_base (&scratch_obstack));
}

tree
get_id_2 (name, name2)
     char *name;
     tree name2;
{
  OB_INIT ();
  OB_PUTCP (name);
  OB_PUTID (name2);
  OB_FINISH ();
  return get_identifier (obstack_base (&scratch_obstack));
}

/* Given a tree_code CODE, and some arguments (at least one),
   attempt to use an overloaded operator on the arguments.

   For unary operators, only the first argument need be checked.
   For binary operators, both arguments may need to be checked.

   Member functions can convert class references to class pointers,
   for one-level deep indirection.  More than that is not supported.
   Operators [](), ()(), and ->() must be member functions.

   We call function call building calls with LOOKUP_COMPLAIN if they
   are our only hope.  This is true when we see a vanilla operator
   applied to something of aggregate type.  If this fails, we are free
   to return `error_mark_node', because we will have reported the
   error.

   Operators NEW and DELETE overload in funny ways: operator new takes
   a single `size' parameter, and operator delete takes a pointer to the
   storage being deleted.  When overloading these operators, success is
   assumed.  If there is a failure, report an error message and return
   `error_mark_node'.  */

/* NOSTRICT */
tree
build_opfncall (code, flags, xarg1, xarg2, arg3)
     enum tree_code code;
     int flags;
     tree xarg1, xarg2, arg3;
{
  tree rval = 0;
  tree arg1, arg2;
  tree type1, type2, fnname;
  tree fields1 = 0, parms = 0;
  tree global_fn;
  int try_second;
  int binary_is_unary;

  if (flag_ansi_overloading)
    return build_new_op (code, flags, xarg1, xarg2, arg3);

  if (xarg1 == error_mark_node)
    return error_mark_node;

  if (code == COND_EXPR)
    {
      if (xarg2 == error_mark_node
	  || arg3 == error_mark_node)
	return error_mark_node;
    }
  if (code == COMPONENT_REF)
    if (TREE_CODE (TREE_TYPE (xarg1)) == POINTER_TYPE)
      return rval;

  /* First, see if we can work with the first argument */
  type1 = TREE_TYPE (xarg1);

  /* Some tree codes have length > 1, but we really only want to
     overload them if their first argument has a user defined type.  */
  switch (code)
    {
    case PREINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case COMPONENT_REF:
      binary_is_unary = 1;
      try_second = 0;
      break;

      /* ARRAY_REFs and CALL_EXPRs must overload successfully.
	 If they do not, return error_mark_node instead of NULL_TREE.  */
    case ARRAY_REF:
      if (xarg2 == error_mark_node)
	return error_mark_node;
    case CALL_EXPR:
      rval = error_mark_node;
      binary_is_unary = 0;
      try_second = 0;
      break;

    case VEC_NEW_EXPR:
    case NEW_EXPR:
      {
	tree args = expr_tree_cons (NULL_TREE, xarg2, arg3);
	fnname = ansi_opname[(int) code];
	if (flags & LOOKUP_GLOBAL)
	  return build_overload_call (fnname, args, flags & LOOKUP_COMPLAIN);

	rval = build_method_call
	  (build_indirect_ref (build1 (NOP_EXPR, xarg1, error_mark_node),
			       "new"),
	   fnname, args, NULL_TREE, flags);
	if (rval == error_mark_node)
	  /* User might declare fancy operator new, but invoke it
	     like standard one.  */
	  return rval;

	TREE_TYPE (rval) = xarg1;
	TREE_CALLS_NEW (rval) = 1;
	return rval;
      }
      break;

    case VEC_DELETE_EXPR:
    case DELETE_EXPR:
      {
	fnname = ansi_opname[(int) code];
	if (flags & LOOKUP_GLOBAL)
	  return build_overload_call (fnname,
				      build_expr_list (NULL_TREE, xarg1),
				      flags & LOOKUP_COMPLAIN);
	arg1 = TREE_TYPE (xarg1);

	/* This handles the case where we're trying to delete
	   X (*a)[10];
	   a=new X[5][10];
	   delete[] a; */
	   
	if (TREE_CODE (TREE_TYPE (arg1)) == ARRAY_TYPE)
	  {
	    /* Strip off the pointer and the array.  */
	    arg1 = TREE_TYPE (TREE_TYPE (arg1));

	    while (TREE_CODE (arg1) == ARRAY_TYPE)
		arg1 = (TREE_TYPE (arg1));

	    arg1 = build_pointer_type (arg1);
	  }

	rval = build_method_call
	  (build_indirect_ref (build1 (NOP_EXPR, arg1,
				       error_mark_node),
			       NULL_PTR),
	   fnname, expr_tree_cons (NULL_TREE, xarg1,
			       build_expr_list (NULL_TREE, xarg2)),
	   NULL_TREE, flags);
#if 0
	/* This can happen when operator delete is protected.  */
	my_friendly_assert (rval != error_mark_node, 250);
	TREE_TYPE (rval) = void_type_node;
#endif
	return rval;
      }
      break;

    default:
      binary_is_unary = 0;
      try_second = tree_code_length [(int) code] == 2;
      if (try_second && xarg2 == error_mark_node)
	return error_mark_node;
      break;
    }

  if (try_second && xarg2 == error_mark_node)
    return error_mark_node;

  /* What ever it was, we do not know how to deal with it.  */
  if (type1 == NULL_TREE)
    return rval;

  if (TREE_CODE (type1) == OFFSET_TYPE)
    type1 = TREE_TYPE (type1);

  if (TREE_CODE (type1) == REFERENCE_TYPE)
    {
      arg1 = convert_from_reference (xarg1);
      type1 = TREE_TYPE (arg1);
    }
  else
    {
      arg1 = xarg1;
    }

  if (!IS_AGGR_TYPE (type1) || TYPE_PTRMEMFUNC_P (type1))
    {
      /* Try to fail. First, fail if unary */
      if (! try_second)
	return rval;
      /* Second, see if second argument is non-aggregate.  */
      type2 = TREE_TYPE (xarg2);
      if (TREE_CODE (type2) == OFFSET_TYPE)
	type2 = TREE_TYPE (type2);
      if (TREE_CODE (type2) == REFERENCE_TYPE)
	{
	  arg2 = convert_from_reference (xarg2);
	  type2 = TREE_TYPE (arg2);
	}
      else
	{
	  arg2 = xarg2;
	}

      if (!IS_AGGR_TYPE (type2))
	return rval;
      try_second = 0;
    }

  if (try_second)
    {
      /* First arg may succeed; see whether second should.  */
      type2 = TREE_TYPE (xarg2);
      if (TREE_CODE (type2) == OFFSET_TYPE)
	type2 = TREE_TYPE (type2);
      if (TREE_CODE (type2) == REFERENCE_TYPE)
	{
	  arg2 = convert_from_reference (xarg2);
	  type2 = TREE_TYPE (arg2);
	}
      else
	{
	  arg2 = xarg2;
	}

      if (! IS_AGGR_TYPE (type2))
	try_second = 0;
    }

  if (type1 == unknown_type_node
      || (try_second && TREE_TYPE (xarg2) == unknown_type_node))
    {
      /* This will not be implemented in the foreseeable future.  */
      return rval;
    }

  if (code == MODIFY_EXPR)
    fnname = ansi_assopname[(int) TREE_CODE (arg3)];
  else
    fnname = ansi_opname[(int) code];

  global_fn = lookup_name_nonclass (fnname);

  /* This is the last point where we will accept failure.  This
     may be too eager if we wish an overloaded operator not to match,
     but would rather a normal operator be called on a type-converted
     argument.  */

  if (IS_AGGR_TYPE (type1))
    {
      fields1 = lookup_fnfields (TYPE_BINFO (type1), fnname, 0);
      /* ARM $13.4.7, prefix/postfix ++/--.  */
      if (code == POSTINCREMENT_EXPR || code == POSTDECREMENT_EXPR)
	{
	  xarg2 = integer_zero_node;
	  binary_is_unary = 0;

	  if (fields1)
	    {
	      tree t, t2;
	      int have_postfix = 0;

	      /* Look for an `operator++ (int)'.  If they didn't have
		 one, then we fall back to the old way of doing things.  */
	      for (t = TREE_VALUE (fields1); t ; t = DECL_CHAIN (t))
		{
		  t2 = TYPE_ARG_TYPES (TREE_TYPE (t));
		  if (TREE_CHAIN (t2) != NULL_TREE
		      && TREE_VALUE (TREE_CHAIN (t2)) == integer_type_node)
		    {
		      have_postfix = 1;
		      break;
		    }
		}

	      if (! have_postfix)
		{
		  char *op = POSTINCREMENT_EXPR ? "++" : "--";

		  /* There's probably a LOT of code in the world that
		     relies upon this old behavior.  */
		  pedwarn ("no `operator%s (int)' declared for postfix `%s', using prefix operator instead",
			   op, op);
		  xarg2 = NULL_TREE;
		  binary_is_unary = 1;
		}
	    }
	}
    }

  if (fields1 == NULL_TREE && global_fn == NULL_TREE)
    return rval;

  /* If RVAL winds up being `error_mark_node', we will return
     that... There is no way that normal semantics of these
     operators will succeed.  */

  /* This argument may be an uncommitted OFFSET_REF.  This is
     the case for example when dealing with static class members
     which are referenced from their class name rather than
     from a class instance.  */
  if (TREE_CODE (xarg1) == OFFSET_REF
      && TREE_CODE (TREE_OPERAND (xarg1, 1)) == VAR_DECL)
    xarg1 = TREE_OPERAND (xarg1, 1);
  if (try_second && xarg2 && TREE_CODE (xarg2) == OFFSET_REF
      && TREE_CODE (TREE_OPERAND (xarg2, 1)) == VAR_DECL)
    xarg2 = TREE_OPERAND (xarg2, 1);

  if (global_fn)
    flags |= LOOKUP_GLOBAL;

  if (code == CALL_EXPR)
    {
      /* This can only be a member function.  */
      return build_method_call (xarg1, fnname, xarg2,
				NULL_TREE, LOOKUP_NORMAL);
    }
  else if (tree_code_length[(int) code] == 1 || binary_is_unary)
    {
      parms = NULL_TREE;
      rval = build_method_call (xarg1, fnname, NULL_TREE, NULL_TREE, flags);
    }
  else if (code == COND_EXPR)
    {
      parms = expr_tree_cons (NULL_TREE, xarg2, build_expr_list (NULL_TREE, arg3));
      rval = build_method_call (xarg1, fnname, parms, NULL_TREE, flags);
    }
  else if (code == METHOD_CALL_EXPR)
    {
      /* must be a member function.  */
      parms = expr_tree_cons (NULL_TREE, xarg2, arg3);
      return build_method_call (xarg1, fnname, parms, NULL_TREE,
				LOOKUP_NORMAL);
    }
  else if (fields1)
    {
      parms = build_expr_list (NULL_TREE, xarg2);
      rval = build_method_call (xarg1, fnname, parms, NULL_TREE, flags);
    }
  else
    {
      parms = expr_tree_cons (NULL_TREE, xarg1,
			 build_expr_list (NULL_TREE, xarg2));
      rval = build_overload_call (fnname, parms, flags);
    }

  return rval;
}

/* This function takes an identifier, ID, and attempts to figure out what
   it means. There are a number of possible scenarios, presented in increasing
   order of hair:

   1) not in a class's scope
   2) in class's scope, member name of the class's method
   3) in class's scope, but not a member name of the class
   4) in class's scope, member name of a class's variable

   NAME is $1 from the bison rule. It is an IDENTIFIER_NODE.
   VALUE is $$ from the bison rule. It is the value returned by lookup_name ($1)

   As a last ditch, try to look up the name as a label and return that
   address.

   Values which are declared as being of REFERENCE_TYPE are
   automatically dereferenced here (as a hack to make the
   compiler faster).  */

tree
hack_identifier (value, name)
     tree value, name;
{
  tree type;

  if (value == error_mark_node)
    {
      if (current_class_name)
	{
	  tree fields = lookup_fnfields (TYPE_BINFO (current_class_type), name, 1);
	  if (fields == error_mark_node)
	    return error_mark_node;
	  if (fields)
	    {
	      tree fndecl;

	      fndecl = TREE_VALUE (fields);
	      my_friendly_assert (TREE_CODE (fndecl) == FUNCTION_DECL, 251);
	      if (DECL_CHAIN (fndecl) == NULL_TREE)
		{
		  warning ("methods cannot be converted to function pointers");
		  return fndecl;
		}
	      else
		{
		  error ("ambiguous request for method pointer `%s'",
			 IDENTIFIER_POINTER (name));
		  return error_mark_node;
		}
	    }
	}
      if (flag_labels_ok && IDENTIFIER_LABEL_VALUE (name))
	{
	  return IDENTIFIER_LABEL_VALUE (name);
	}
      return error_mark_node;
    }

  type = TREE_TYPE (value);
  if (TREE_CODE (value) == FIELD_DECL)
    {
      if (current_class_ptr == NULL_TREE)
	{
	  error ("request for member `%s' in static member function",
		 IDENTIFIER_POINTER (DECL_NAME (value)));
	  return error_mark_node;
	}
      TREE_USED (current_class_ptr) = 1;

      /* Mark so that if we are in a constructor, and then find that
	 this field was initialized by a base initializer,
	 we can emit an error message.  */
      TREE_USED (value) = 1;
      value = build_component_ref (current_class_ref, name, NULL_TREE, 1);
    }
  else if (really_overloaded_fn (value))
    {
#if 0
      tree t = get_first_fn (value);
      for (; t; t = DECL_CHAIN (t))
	{
	  if (TREE_CODE (t) == TEMPLATE_DECL)
	    continue;

	  assemble_external (t);
	  TREE_USED (t) = 1;
	}
#endif
    }
  else if (TREE_CODE (value) == TREE_LIST)
    {
      /* Ambiguous reference to base members, possibly other cases?.  */
      tree t = value;
      while (t && TREE_CODE (t) == TREE_LIST)
	{
	  mark_used (TREE_VALUE (t));
	  t = TREE_CHAIN (t);
	}
    }
  else
    mark_used (value);

  if (TREE_CODE (value) == VAR_DECL || TREE_CODE (value) == PARM_DECL)
    {
      tree context = decl_function_context (value);
      if (context != NULL_TREE && context != current_function_decl
	  && ! TREE_STATIC (value))
	{
	  cp_error ("use of %s from containing function",
		      (TREE_CODE (value) == VAR_DECL
		       ? "`auto' variable" : "parameter"));
	  cp_error_at ("  `%#D' declared here", value);
	  value = error_mark_node;
	}
    }

  if (TREE_CODE_CLASS (TREE_CODE (value)) == 'd' && DECL_NONLOCAL (value))
    {
      if (DECL_LANG_SPECIFIC (value)
	  && DECL_CLASS_CONTEXT (value) != current_class_type)
	{
	  tree path, access;
	  register tree context
	    = (TREE_CODE (value) == FUNCTION_DECL && DECL_VIRTUAL_P (value))
	      ? DECL_CLASS_CONTEXT (value)
	      : DECL_CONTEXT (value);

	  get_base_distance (context, current_class_type, 0, &path);
	  if (path)
	    {
	      access = compute_access (path, value);
	      if (access != access_public_node)
		{
		  if (TREE_CODE (value) == VAR_DECL)
		    error ("static member `%s' is %s",
			   IDENTIFIER_POINTER (name),
			   TREE_PRIVATE (value) ? "private"
						: "from a private base class");
		  else
		    error ("enum `%s' is from private base class",
			   IDENTIFIER_POINTER (name));
		  return error_mark_node;
		}
	    }
	}
    }
  else if (TREE_CODE (value) == TREE_LIST && TREE_NONLOCAL_FLAG (value))
    {
      if (type == 0)
	{
	  error ("request for member `%s' is ambiguous in multiple inheritance lattice",
		 IDENTIFIER_POINTER (name));
	  return error_mark_node;
	}

      return value;
    }

  if (TREE_CODE (type) == REFERENCE_TYPE && ! processing_template_decl)
    value = convert_from_reference (value);
  return value;
}


static char *
thunk_printable_name (decl)
     tree decl;
{
  return "<thunk function>";
}

tree
make_thunk (function, delta)
     tree function;
     int delta;
{
  char buffer[250];
  tree thunk_id;
  tree thunk;
  char *func_name;
  tree func_decl;
  if (TREE_CODE (function) != ADDR_EXPR)
    abort ();
  func_decl = TREE_OPERAND (function, 0);
  if (TREE_CODE (func_decl) != FUNCTION_DECL)
    abort ();
  func_name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (func_decl));
  if (delta<=0)
    sprintf (buffer, "__thunk_%d_%s", -delta, func_name);
  else
    sprintf (buffer, "__thunk_n%d_%s", delta, func_name);
  thunk_id = get_identifier (buffer);
  thunk = IDENTIFIER_GLOBAL_VALUE (thunk_id);
  if (thunk && TREE_CODE (thunk) != THUNK_DECL)
    {
      cp_error ("implementation-reserved name `%D' used", thunk_id);
      IDENTIFIER_GLOBAL_VALUE (thunk_id) = thunk = NULL_TREE;
    }
  if (thunk == NULL_TREE)
    {
      thunk = build_decl (FUNCTION_DECL, thunk_id, TREE_TYPE (func_decl));
      TREE_READONLY (thunk) = TREE_READONLY (func_decl);
      TREE_THIS_VOLATILE (thunk) = TREE_THIS_VOLATILE (func_decl);
      comdat_linkage (thunk);
      TREE_SET_CODE (thunk, THUNK_DECL);
      DECL_INITIAL (thunk) = function;
      THUNK_DELTA (thunk) = delta;
      DECL_EXTERNAL (thunk) = 1;
      DECL_ARTIFICIAL (thunk) = 1;
      /* So that finish_file can write out any thunks that need to be: */
      pushdecl_top_level (thunk);
    }
  return thunk;
}

/* Emit the definition of a C++ multiple inheritance vtable thunk.  */

void
emit_thunk (thunk_fndecl)
     tree thunk_fndecl;
{
  tree function = TREE_OPERAND (DECL_INITIAL (thunk_fndecl), 0);
  int delta = THUNK_DELTA (thunk_fndecl);

  if (TREE_ASM_WRITTEN (thunk_fndecl))
    return;

  TREE_ASM_WRITTEN (thunk_fndecl) = 1;

  TREE_ADDRESSABLE (function) = 1;
  mark_used (function);

  if (current_function_decl)
    abort ();

  TREE_SET_CODE (thunk_fndecl, FUNCTION_DECL);

  {
#ifdef ASM_OUTPUT_MI_THUNK
    char *fnname;
    current_function_decl = thunk_fndecl;
    /* Make sure we build up its RTL before we go onto the
       temporary obstack.  */
    make_function_rtl (thunk_fndecl);
    temporary_allocation ();
    DECL_RESULT (thunk_fndecl)
      = build_decl (RESULT_DECL, 0, integer_type_node);
    fnname = XSTR (XEXP (DECL_RTL (thunk_fndecl), 0), 0);
    init_function_start (thunk_fndecl, input_filename, lineno);
    assemble_start_function (thunk_fndecl, fnname);
    ASM_OUTPUT_MI_THUNK (asm_out_file, thunk_fndecl, delta, function);
    assemble_end_function (thunk_fndecl, fnname);
    permanent_allocation (1);
    current_function_decl = 0;
#else /* ASM_OUTPUT_MI_THUNK */
  /* If we don't have the necessary macro for efficient thunks, generate a
     thunk function that just makes a call to the real function.
     Unfortunately, this doesn't work for varargs.  */

    tree a, t;

    if (varargs_function_p (function))
      cp_error ("generic thunk code fails for method `%#D' which uses `...'",
		function);

    /* Set up clone argument trees for the thunk.  */
    t = NULL_TREE;
    for (a = DECL_ARGUMENTS (function); a; a = TREE_CHAIN (a))
      {
	tree x = copy_node (a);
	TREE_CHAIN (x) = t;
	DECL_CONTEXT (x) = thunk_fndecl;
	t = x;
      }
    a = nreverse (t);
    DECL_ARGUMENTS (thunk_fndecl) = a;
    DECL_RESULT (thunk_fndecl) = NULL_TREE;
    DECL_LANG_SPECIFIC (thunk_fndecl) = DECL_LANG_SPECIFIC (function);
    copy_lang_decl (thunk_fndecl);
    DECL_INTERFACE_KNOWN (thunk_fndecl) = 1;
    DECL_NOT_REALLY_EXTERN (thunk_fndecl) = 1;

    start_function (NULL_TREE, thunk_fndecl, NULL_TREE, 1);
    store_parm_decls ();
    current_function_is_thunk = 1;

    /* Build up the call to the real function.  */
    t = build_int_2 (delta, -1 * (delta < 0));
    TREE_TYPE (t) = signed_type (sizetype);
    t = fold (build (PLUS_EXPR, TREE_TYPE (a), a, t));
    t = expr_tree_cons (NULL_TREE, t, NULL_TREE);
    for (a = TREE_CHAIN (a); a; a = TREE_CHAIN (a))
      t = expr_tree_cons (NULL_TREE, a, t);
    t = nreverse (t);
    t = build_call (function, TREE_TYPE (TREE_TYPE (function)), t);
    c_expand_return (t);

    finish_function (lineno, 0, 0);

    /* Don't let the backend defer this function.  */
    if (DECL_DEFER_OUTPUT (thunk_fndecl))
      {
	output_inline_function (thunk_fndecl);
	permanent_allocation (1);
      }
#endif /* ASM_OUTPUT_MI_THUNK */
  }

  TREE_SET_CODE (thunk_fndecl, THUNK_DECL);
}

/* Code for synthesizing methods which have default semantics defined.  */

/* For the anonymous union in TYPE, return the member that is at least as
   large as the rest of the members, so we can copy it.  */

static tree
largest_union_member (type)
     tree type;
{
  tree f, type_size = TYPE_SIZE (type);

  for (f = TYPE_FIELDS (type); f; f = TREE_CHAIN (f))
    if (simple_cst_equal (DECL_SIZE (f), type_size) == 1)
      return f;

  /* We should always find one.  */
  my_friendly_abort (323);
  return NULL_TREE;
}

/* Generate code for default X(X&) constructor.  */

static void
do_build_copy_constructor (fndecl)
     tree fndecl;
{
  tree parm = TREE_CHAIN (DECL_ARGUMENTS (fndecl));
  tree t;

  clear_last_expr ();
  push_momentary ();

  if (TYPE_USES_VIRTUAL_BASECLASSES (current_class_type))
    parm = TREE_CHAIN (parm);
  parm = convert_from_reference (parm);

  if (TYPE_HAS_TRIVIAL_INIT_REF (current_class_type))
    {
      t = build (INIT_EXPR, void_type_node, current_class_ref, parm);
      TREE_SIDE_EFFECTS (t) = 1;
      cplus_expand_expr_stmt (t);
    }
  else
    {
      tree fields = TYPE_FIELDS (current_class_type);
      int n_bases = CLASSTYPE_N_BASECLASSES (current_class_type);
      tree binfos = TYPE_BINFO_BASETYPES (current_class_type);
      int i;

      for (t = CLASSTYPE_VBASECLASSES (current_class_type); t;
	   t = TREE_CHAIN (t))
	{
	  tree basetype = BINFO_TYPE (t);
	  tree p = convert_to_reference
	    (build_reference_type (basetype), parm,
	     CONV_IMPLICIT|CONV_CONST, LOOKUP_COMPLAIN, NULL_TREE);
	  p = convert_from_reference (p);

	  if (p == error_mark_node)
	    cp_error ("in default copy constructor");
	  else 
	    current_base_init_list = tree_cons (basetype,
						p, current_base_init_list);
	}
	
      for (i = 0; i < n_bases; ++i)
	{
	  tree p, basetype = TREE_VEC_ELT (binfos, i);
	  if (TREE_VIA_VIRTUAL (basetype))
	    continue; 

	  basetype = BINFO_TYPE (basetype);
	  p = convert_to_reference
	    (build_reference_type (basetype), parm,
	     CONV_IMPLICIT|CONV_CONST, LOOKUP_COMPLAIN, NULL_TREE);

	  if (p == error_mark_node) 
	    cp_error ("in default copy constructor");
	  else 
	    {
	      p = convert_from_reference (p);
	      current_base_init_list = tree_cons (basetype,
						  p, current_base_init_list);
	    }
	}
      for (; fields; fields = TREE_CHAIN (fields))
	{
	  tree init, t;
	  tree field = fields;

	  if (TREE_CODE (field) != FIELD_DECL)
	    continue;

	  init = parm;
	  if (DECL_NAME (field))
	    {
	      if (VFIELD_NAME_P (DECL_NAME (field)))
		continue;
	      if (VBASE_NAME_P (DECL_NAME (field)))
		continue;

	      /* True for duplicate members.  */
	      if (IDENTIFIER_CLASS_VALUE (DECL_NAME (field)) != field)
		continue;
	    }
	  else if ((t = TREE_TYPE (field)) != NULL_TREE
		   && TREE_CODE (t) == UNION_TYPE
		   && ANON_AGGRNAME_P (TYPE_IDENTIFIER (t))
		   && TYPE_FIELDS (t) != NULL_TREE)
	    {
	      do
		{
		  init = build (COMPONENT_REF, t, init, field);
		  field = largest_union_member (t);
		}
	      while ((t = TREE_TYPE (field)) != NULL_TREE
		     && TREE_CODE (t) == UNION_TYPE
		     && ANON_AGGRNAME_P (TYPE_IDENTIFIER (t))
		     && TYPE_FIELDS (t) != NULL_TREE);
	    }
	  else
	    continue;

	  init = build (COMPONENT_REF, TREE_TYPE (field), init, field);
	  init = build_tree_list (NULL_TREE, init);

	  current_member_init_list
	    = tree_cons (DECL_NAME (field), init, current_member_init_list);
	}
      current_member_init_list = nreverse (current_member_init_list);
      current_base_init_list = nreverse (current_base_init_list);
      setup_vtbl_ptr ();
    }

  pop_momentary ();
}

static void
do_build_assign_ref (fndecl)
     tree fndecl;
{
  tree parm = TREE_CHAIN (DECL_ARGUMENTS (fndecl));

  clear_last_expr ();
  push_momentary ();

  parm = convert_from_reference (parm);

  if (TYPE_HAS_TRIVIAL_ASSIGN_REF (current_class_type))
    {
      tree t = build (MODIFY_EXPR, void_type_node, current_class_ref, parm);
      TREE_SIDE_EFFECTS (t) = 1;
      cplus_expand_expr_stmt (t);
    }
  else
    {
      tree fields = TYPE_FIELDS (current_class_type);
      int n_bases = CLASSTYPE_N_BASECLASSES (current_class_type);
      tree binfos = TYPE_BINFO_BASETYPES (current_class_type);
      int i;

      for (i = 0; i < n_bases; ++i)
	{
	  tree basetype = BINFO_TYPE (TREE_VEC_ELT (binfos, i));
	  tree p = convert_to_reference
	    (build_reference_type (basetype), parm,
	     CONV_IMPLICIT|CONV_CONST, LOOKUP_COMPLAIN, NULL_TREE);
	  p = convert_from_reference (p);
	  p = build_member_call (basetype, ansi_opname [MODIFY_EXPR],
				 build_expr_list (NULL_TREE, p));
	  expand_expr_stmt (p);
	}
      for (; fields; fields = TREE_CHAIN (fields))
	{
	  tree comp, init, t;
	  tree field = fields;

	  if (TREE_CODE (field) != FIELD_DECL)
	    continue;

	  if (TREE_READONLY (field))
	    {
	      if (DECL_NAME (field))
		cp_error ("non-static const member `%#D', can't use default assignment operator", field);
	      else
		cp_error ("non-static const member in type `%T', can't use default assignment operator", current_class_type);
	      continue;
	    }
	  else if (TREE_CODE (TREE_TYPE (field)) == REFERENCE_TYPE)
	    {
	      if (DECL_NAME (field))
		cp_error ("non-static reference member `%#D', can't use default assignment operator", field);
	      else
		cp_error ("non-static reference member in type `%T', can't use default assignment operator", current_class_type);
	      continue;
	    }

	  comp = current_class_ref;
	  init = parm;

	  if (DECL_NAME (field))
	    {
	      if (VFIELD_NAME_P (DECL_NAME (field)))
		continue;
	      if (VBASE_NAME_P (DECL_NAME (field)))
		continue;

	      /* True for duplicate members.  */
	      if (IDENTIFIER_CLASS_VALUE (DECL_NAME (field)) != field)
		continue;
	    }
	  else if ((t = TREE_TYPE (field)) != NULL_TREE
		   && TREE_CODE (t) == UNION_TYPE
		   && ANON_AGGRNAME_P (TYPE_IDENTIFIER (t))
		   && TYPE_FIELDS (t) != NULL_TREE)
	    {
	      do
		{
		  comp = build (COMPONENT_REF, t, comp, field);
		  init = build (COMPONENT_REF, t, init, field);
		  field = largest_union_member (t);
		}
	      while ((t = TREE_TYPE (field)) != NULL_TREE
		     && TREE_CODE (t) == UNION_TYPE
		     && ANON_AGGRNAME_P (TYPE_IDENTIFIER (t))
		     && TYPE_FIELDS (t) != NULL_TREE);
	    }
	  else
	    continue;

	  comp = build (COMPONENT_REF, TREE_TYPE (field), comp, field);
	  init = build (COMPONENT_REF, TREE_TYPE (field), init, field);

	  expand_expr_stmt (build_modify_expr (comp, NOP_EXPR, init));
	}
    }
  c_expand_return (current_class_ref);
  pop_momentary ();
}

void
synthesize_method (fndecl)
     tree fndecl;
{
  int nested = (current_function_decl != NULL_TREE);
  tree context = hack_decl_function_context (fndecl);

  if (at_eof)
    import_export_decl (fndecl);

  if (! context)
    push_to_top_level ();
  else if (nested)
    push_cp_function_context (context);

  interface_unknown = 1;
  start_function (NULL_TREE, fndecl, NULL_TREE, 1);
  store_parm_decls ();

  if (DECL_NAME (fndecl) == ansi_opname[MODIFY_EXPR])
    do_build_assign_ref (fndecl);
  else if (DESTRUCTOR_NAME_P (DECL_ASSEMBLER_NAME (fndecl)))
    ;
  else
    {
      tree arg_chain = FUNCTION_ARG_CHAIN (fndecl);
      if (DECL_CONSTRUCTOR_FOR_VBASE_P (fndecl))
	arg_chain = TREE_CHAIN (arg_chain);
      if (arg_chain != void_list_node)
	do_build_copy_constructor (fndecl);
      else if (TYPE_NEEDS_CONSTRUCTING (current_class_type))
	setup_vtbl_ptr ();
    }

  finish_function (lineno, 0, nested);

  extract_interface_info ();
  if (! context)
    pop_from_top_level ();
  else if (nested)
    pop_cp_function_context (context);
}
