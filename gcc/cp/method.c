/* Handle the hair of processing (but not expanding) inline functions.
   Also manage function and variable name overloading.
   Copyright (C) 1987, 89, 92, 93, 94, 1995 Free Software Foundation, Inc.
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
#include <stdio.h>
#include "config.h"
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

/* TREE_LIST of the current inline functions that need to be
   processed.  */
struct pending_inline *pending_inlines;

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

/* Obstack where we build text strings for overloading, etc.  */
static struct obstack scratch_obstack;
static char *scratch_firstobj;

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

#ifdef NO_AUTO_OVERLOAD
int is_overloaded ();
#endif

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
      if (TREE_VEC_ELT (method, 0))
	method = TREE_VEC_ELT (method, 0);
      else
	method = TREE_VEC_ELT (method, 1);
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
     int i;
{
  /* Handle this case first, to go really quickly.  For many common values,
     the result of i/10 below is 1.  */
  if (i == 1)
    {
      OB_PUTC ('1');
      return;
    }

  if (i < 0)
    {
      OB_PUTC ('m');
      i = -i;
    }
  if (i < 10)
    OB_PUTC ('0' + i);
  else
    {
      icat (i / 10);
      OB_PUTC ('0' + (i % 10));
    }
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
static void build_overload_identifier ();

static void
build_overload_nested_name (decl)
     tree decl;
{
  if (DECL_CONTEXT (decl))
    {
      tree context = DECL_CONTEXT (decl);
      if (TREE_CODE_CLASS (TREE_CODE (context)) == 't')
	context = TYPE_MAIN_DECL (context);
      build_overload_nested_name (context);
    }

  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      tree name = DECL_ASSEMBLER_NAME (decl);
      char *label;
      extern int var_labelno;

      ASM_FORMAT_PRIVATE_NAME (label, IDENTIFIER_POINTER (name), var_labelno);
      var_labelno++;

      if (numeric_output_need_bar)
	{
	  OB_PUTC ('_');
	  numeric_output_need_bar = 0;
	}
      icat (strlen (label));
      OB_PUTCP (label);
    }
  else				/* TYPE_DECL */
    {
      tree name = DECL_NAME (decl);
      build_overload_identifier (name);
    }
}

/* Encoding for an INTEGER_CST value. */
static void
build_overload_int (value)
     tree value;
{
  my_friendly_assert (TREE_CODE (value) == INTEGER_CST, 243);
  if (TYPE_PRECISION (value) == 2 * HOST_BITS_PER_WIDE_INT)
    {
      if (tree_int_cst_lt (value, integer_zero_node))
	{
	  OB_PUTC ('m');
	  value = build_int_2 (~ TREE_INT_CST_LOW (value),
			       - TREE_INT_CST_HIGH (value));
	}
      if (TREE_INT_CST_HIGH (value)
	  != (TREE_INT_CST_LOW (value) >> (HOST_BITS_PER_WIDE_INT - 1)))
	{
	  /* need to print a DImode value in decimal */
	  sorry ("conversion of long long as PT parameter");
	}
      /* else fall through to print in smaller mode */
    }
  /* Wordsize or smaller */
  icat (TREE_INT_CST_LOW (value));
}

static void
build_overload_value (type, value)
     tree type, value;
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
	build_overload_int (value);
	numeric_output_need_bar = 1;
	return;
      }
#ifndef REAL_IS_NOT_DOUBLE
    case REAL_TYPE:
      {
	REAL_VALUE_TYPE val;
	char *bufp = digit_buffer;
	extern char *index ();

	my_friendly_assert (TREE_CODE (value) == REAL_CST, 244);
	val = TREE_REAL_CST (value);
	if (val < 0)
	  {
	    val = -val;
	    *bufp++ = 'm';
	  }
	sprintf (bufp, "%e", val);
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
	OB_PUTCP (digit_buffer);
	numeric_output_need_bar = 1;
	return;
      }
#endif
    case POINTER_TYPE:
      if (TREE_CODE (TREE_TYPE (type)) == METHOD_TYPE
	  && TREE_CODE (value) != ADDR_EXPR)
	{
	  if (TREE_CODE (value) == CONSTRUCTOR)
	    {
	      /* This is dangerous code, crack built up pointer to members. */
	      tree args = CONSTRUCTOR_ELTS (value);
	      tree a1 = TREE_VALUE (args);
	      tree a2 = TREE_VALUE (TREE_CHAIN (args));
	      tree a3 = CONSTRUCTOR_ELTS (TREE_VALUE (TREE_CHAIN (TREE_CHAIN (args))));
	      a3 = TREE_VALUE (a3);
	      STRIP_NOPS (a3);
	      if (TREE_CODE (a1) == INTEGER_CST
		  && TREE_CODE (a2) == INTEGER_CST)
		{
		  build_overload_int (a1);
		  OB_PUTC ('_');
		  build_overload_int (a2);
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
		      build_overload_int (a3);
		      numeric_output_need_bar = 1;
		      return;
		    }
		}
	    }
	  sorry ("template instantiation with pointer to method that is too complex");
	  return;
	}
      if (TREE_CODE (value) == INTEGER_CST)
	{
	  build_overload_int (value);
	  numeric_output_need_bar = 1;
	  return;
	}
      value = TREE_OPERAND (value, 0);
      if (TREE_CODE (value) == VAR_DECL)
	{
	  my_friendly_assert (DECL_NAME (value) != 0, 245);
	  build_overload_identifier (DECL_NAME (value));
	  return;
	}
      else if (TREE_CODE (value) == FUNCTION_DECL)
	{
	  my_friendly_assert (DECL_NAME (value) != 0, 246);
	  build_overload_identifier (DECL_NAME (value));
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

static void
build_overload_identifier (name)
     tree name;
{
  if (IDENTIFIER_TEMPLATE (name))
    {
      tree template, parmlist, arglist, tname;
      int i, nparms;
      template = IDENTIFIER_TEMPLATE (name);
      arglist = TREE_VALUE (template);
      template = TREE_PURPOSE (template);
      tname = DECL_NAME (template);
      parmlist = DECL_ARGUMENTS (template);
      nparms = TREE_VEC_LENGTH (parmlist);
      OB_PUTC ('t');
      icat (IDENTIFIER_LENGTH (tname));
      OB_PUTID (tname);
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
	      parm = tsubst (parm, &TREE_VEC_ELT (arglist, 0),
			     TREE_VEC_LENGTH (arglist), NULL_TREE);
	      /* It's a PARM_DECL.  */
	      build_overload_name (TREE_TYPE (parm), 0, 0);
	      build_overload_value (parm, arg);
	    }
	}
    }
  else
    {
      if (numeric_output_need_bar)
	{
	  OB_PUTC ('_');
	  numeric_output_need_bar = 0;
	}
      icat (IDENTIFIER_LENGTH (name));
      OB_PUTID (name);
    }
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
	      flush_repeats (parmtype);
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
	    int i = 1;

	    if (TREE_CODE (name) == TYPE_DECL)
	      {
		tree context = name;

		/* If DECL_ASSEMBLER_NAME has been set properly, use it. */
		if (DECL_ASSEMBLER_NAME (context) != DECL_NAME (context))
		  {
		    OB_PUTID (DECL_ASSEMBLER_NAME (context));
		    break;
		  }
		while (DECL_CONTEXT (context))
		  {
		    i += 1;
		    context = DECL_CONTEXT (context);
		    if (TREE_CODE_CLASS (TREE_CODE (context)) == 't')
		      context = TYPE_NAME (context);
		  }
		name = DECL_NAME (name);
	      }
	    my_friendly_assert (TREE_CODE (name) == IDENTIFIER_NODE, 248);
	    if (i > 1)
	      {
		OB_PUTC ('Q');
		if (i > 9)
		  OB_PUTC ('_');
		icat (i);
		if (i > 9)
		  OB_PUTC ('_');
		numeric_output_need_bar = 0;
		build_overload_nested_name (TYPE_MAIN_DECL (parmtype));
	      }
	    else
	      build_overload_identifier (name);
	    break;
	  }

	case UNKNOWN_TYPE:
	  /* This will take some work.  */
	  OB_PUTC ('?');
	  break;

	case TEMPLATE_TYPE_PARM:
	case TEMPLATE_CONST_PARM:
        case UNINSTANTIATED_P_TYPE:
	  /* We don't ever want this output, but it's inconvenient not to
	     be able to build the string.  This should cause assembler
	     errors we'll notice.  */
	  {
	    static int n;
	    sprintf (digit_buffer, " *%d", n++);
	    OB_PUTCP (digit_buffer);
	  }
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

      /* To get here, parms must end with `...'. */
      OB_PUTC ('e');
    }

  if (end) OB_FINISH ();
  return (char *)obstack_base (&scratch_obstack);
}

tree
build_static_name (basetype, name)
  tree basetype, name;
{
  char *basename  = build_overload_name (basetype, 1, 1);
  char *buf = (char *) alloca (IDENTIFIER_LENGTH (name)
			       + sizeof (STATIC_NAME_FORMAT)
			       + strlen (basename));
  sprintf (buf, STATIC_NAME_FORMAT, basename, IDENTIFIER_POINTER (name));
  return get_identifier (buf);
}  

/* Generate an identifier that encodes the (ANSI) exception TYPE. */

/* This should be part of `ansi_opname', or at least be defined by the std.  */
#define EXCEPTION_NAME_PREFIX "__ex"
#define EXCEPTION_NAME_LENGTH 4

tree
cplus_exception_name (type)
     tree type;
{
  OB_INIT ();
  OB_PUTS (EXCEPTION_NAME_PREFIX);
  return get_identifier (build_overload_name (type, 0, 1));
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
  else
    OB_PUTC ('F');

  if (parms == NULL_TREE)
    OB_PUTC2 ('e', '\0');
  else if (parms == void_list_node)
    OB_PUTC2 ('v', '\0');
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
	    build_overload_name (TREE_CHAIN (parms), 0, 1);
	  else
	    OB_PUTC2 ('e', '\0');
	}
      else
	build_overload_name (parms, 0, 1);
      DEALLOCATE_TYPEVEC (parms);
    }
  {
    tree n = get_identifier (obstack_base (&scratch_obstack));
    if (IDENTIFIER_OPNAME_P (dname))
      IDENTIFIER_OPNAME_P (n) = 1;
    return n;
  }
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
  IDENTIFIER_GLOBAL_VALUE (id) = TYPE_NAME (type);
#endif
  TREE_TYPE (id) = type;
  return id;
}

#ifndef NO_DOLLAR_IN_LABEL
#define T_DESC_FORMAT "TD$"
#define I_DESC_FORMAT "ID$"
#define M_DESC_FORMAT "MD$"
#else
#if !defined(NO_DOT_IN_LABEL)
#define T_DESC_FORMAT "TD."
#define I_DESC_FORMAT "ID."
#define M_DESC_FORMAT "MD."
#else
#define T_DESC_FORMAT "__t_desc_"
#define I_DESC_FORMAT "__i_desc_"
#define M_DESC_FORMAT "__m_desc_"
#endif
#endif

/* Build an overload name for the type expression TYPE.  */
tree
build_t_desc_overload (type)
     tree type;
{
  OB_INIT ();
  OB_PUTS (T_DESC_FORMAT);
  nofold = 1;

#if 0
  /* Use a different format if the type isn't defined yet.  */
  if (TYPE_SIZE (type) == NULL_TREE)
    {
      char *p;
      int changed;

      for (p = tname; *p; p++)
	if (isupper (*p))
	  {
	    changed = 1;
	    *p = tolower (*p);
	  }
      /* If there's no change, we have an inappropriate T_DESC_FORMAT.  */
      my_friendly_assert (changed != 0, 249);
    }
#endif

  build_overload_name (type, 0, 1);
  return get_identifier (obstack_base (&scratch_obstack));
}

/* Top-level interface to explicit overload requests. Allow NAME
   to be overloaded. Error if NAME is already declared for the current
   scope. Warning if function is redundantly overloaded. */

void
declare_overloaded (name)
     tree name;
{
#ifdef NO_AUTO_OVERLOAD
  if (is_overloaded (name))
    warning ("function `%s' already declared overloaded",
	     IDENTIFIER_POINTER (name));
  else if (IDENTIFIER_GLOBAL_VALUE (name))
    error ("overloading function `%s' that is already defined",
	   IDENTIFIER_POINTER (name));
  else
    {
      TREE_OVERLOADED (name) = 1;
      IDENTIFIER_GLOBAL_VALUE (name) = build_tree_list (name, NULL_TREE);
      TREE_TYPE (IDENTIFIER_GLOBAL_VALUE (name)) = unknown_type_node;
    }
#else
  if (current_lang_name == lang_name_cplusplus)
    {
      if (0)
	warning ("functions are implicitly overloaded in C++");
    }
  else if (current_lang_name == lang_name_c)
    error ("overloading function `%s' cannot be done in C language context");
  else
    my_friendly_abort (76);
#endif
}

#ifdef NO_AUTO_OVERLOAD
/* Check to see if NAME is overloaded. For first approximation,
   check to see if its TREE_OVERLOADED is set.  This is used on
   IDENTIFIER nodes.  */
int
is_overloaded (name)
     tree name;
{
  /* @@ */
  return (TREE_OVERLOADED (name)
	  && (! IDENTIFIER_CLASS_VALUE (name) || current_class_type == 0)
	  && ! IDENTIFIER_LOCAL_VALUE (name));
}
#endif

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

  if (xarg1 == error_mark_node)
    return error_mark_node;

  if (code == COND_EXPR)
    {
      if (TREE_CODE (xarg2) == ERROR_MARK
	  || TREE_CODE (arg3) == ERROR_MARK)
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
	tree args = tree_cons (NULL_TREE, xarg2, arg3);
	fnname = ansi_opname[(int) code];
	if (flags & LOOKUP_GLOBAL)
	  return build_overload_call (fnname, args, flags & LOOKUP_COMPLAIN,
				      (struct candidate *)0);

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
				      build_tree_list (NULL_TREE, xarg1),
				      flags & LOOKUP_COMPLAIN,
				      (struct candidate *)0);

	rval = build_method_call
	  (build_indirect_ref (build1 (NOP_EXPR, TREE_TYPE (xarg1),
				       error_mark_node),
			       NULL_PTR),
	   fnname, tree_cons (NULL_TREE, xarg1,
			       build_tree_list (NULL_TREE, xarg2)),
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
      /* Second, see if second argument is non-aggregate. */
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
		  if (! flag_traditional)
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
      parms = tree_cons (0, xarg2, build_tree_list (NULL_TREE, arg3));
      rval = build_method_call (xarg1, fnname, parms, NULL_TREE, flags);
    }
  else if (code == METHOD_CALL_EXPR)
    {
      /* must be a member function.  */
      parms = tree_cons (NULL_TREE, xarg2, arg3);
      return build_method_call (xarg1, fnname, parms, NULL_TREE,
				LOOKUP_NORMAL);
    }
  else if (fields1)
    {
      parms = build_tree_list (NULL_TREE, xarg2);
      rval = build_method_call (xarg1, fnname, parms, NULL_TREE, flags);
    }
  else
    {
      parms = tree_cons (NULL_TREE, xarg1,
			 build_tree_list (NULL_TREE, xarg2));
      rval = build_overload_call (fnname, parms, flags,
				  (struct candidate *)0);
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
   yychar is the pending input character (suitably encoded :-).

   As a last ditch, try to look up the name as a label and return that
   address.

   Values which are declared as being of REFERENCE_TYPE are
   automatically dereferenced here (as a hack to make the
   compiler faster).  */

tree
hack_identifier (value, name, yychar)
     tree value, name;
     int yychar;
{
  tree type;

  if (TREE_CODE (value) == ERROR_MARK)
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
      if (current_class_decl == NULL_TREE)
	{
	  error ("request for member `%s' in static member function",
		 IDENTIFIER_POINTER (DECL_NAME (value)));
	  return error_mark_node;
	}
      TREE_USED (current_class_decl) = 1;

      /* Mark so that if we are in a constructor, and then find that
	 this field was initialized by a base initializer,
	 we can emit an error message.  */
      TREE_USED (value) = 1;
      return build_component_ref (C_C_D, name, 0, 1);
    }

  if (really_overloaded_fn (value))
    {
      tree t = get_first_fn (value);
      for (; t; t = DECL_CHAIN (t))
	{
	  if (TREE_CODE (t) == TEMPLATE_DECL)
	    continue;

	  assemble_external (t);
	  TREE_USED (t) = 1;
	}
    }
  else if (TREE_CODE (value) == TREE_LIST)
    {
      tree t = value;
      while (t && TREE_CODE (t) == TREE_LIST)
	{
	  assemble_external (TREE_VALUE (t));
	  TREE_USED (t) = 1;
	  t = TREE_CHAIN (t);
	}
    }
  else
    {
      assemble_external (value);
      TREE_USED (value) = 1;
    }

  if (TREE_CODE_CLASS (TREE_CODE (value)) == 'd' && DECL_NONLOCAL (value))
    {
      if (DECL_LANG_SPECIFIC (value)
	  && DECL_CLASS_CONTEXT (value) != current_class_type)
	{
	  tree path;
	  enum access_type access;
	  register tree context
	    = (TREE_CODE (value) == FUNCTION_DECL && DECL_VIRTUAL_P (value))
	      ? DECL_CLASS_CONTEXT (value)
	      : DECL_CONTEXT (value);

	  get_base_distance (context, current_class_type, 0, &path);
	  if (path)
	    {
	      access = compute_access (path, value);
	      if (access != access_public)
		{
		  if (TREE_CODE (value) == VAR_DECL)
		    error ("static member `%s' is %s",
			   IDENTIFIER_POINTER (name),
			   TREE_PRIVATE (value) ? "private" :
			   "from a private base class");
		  else
		    error ("enum `%s' is from private base class",
			   IDENTIFIER_POINTER (name));
		  return error_mark_node;
		}
	    }
	}
      return value;
    }
  if (TREE_CODE (value) == TREE_LIST && TREE_NONLOCAL_FLAG (value))
    {
      if (type == 0)
	{
	  error ("request for member `%s' is ambiguous in multiple inheritance lattice",
		 IDENTIFIER_POINTER (name));
	  return error_mark_node;
	}

      return value;
    }

  if (TREE_CODE (type) == REFERENCE_TYPE)
    {
      my_friendly_assert (TREE_CODE (value) == VAR_DECL
			  || TREE_CODE (value) == PARM_DECL
			  || TREE_CODE (value) == RESULT_DECL, 252);
      return convert_from_reference (value);
    }
  return value;
}


#if 0
/* Given an object OF, and a type conversion operator COMPONENT
   build a call to the conversion operator, if a call is requested,
   or return the address (as a pointer to member function) if one is not.

   OF can be a TYPE_DECL or any kind of datum that would normally
   be passed to `build_component_ref'.  It may also be NULL_TREE,
   in which case `current_class_type' and `current_class_decl'
   provide default values.

   BASETYPE_PATH, if non-null, is the path of basetypes
   to go through before we get the the instance of interest.

   PROTECT says whether we apply C++ scoping rules or not.  */
tree
build_component_type_expr (of, component, basetype_path, protect)
     tree of, component, basetype_path;
     int protect;
{
  tree cname = NULL_TREE;
  tree tmp, last;
  tree name;
  int flags = protect ? LOOKUP_NORMAL : LOOKUP_COMPLAIN;

  if (of)
    my_friendly_assert (IS_AGGR_TYPE (TREE_TYPE (of)), 253);
  my_friendly_assert (TREE_CODE (component) == TYPE_EXPR, 254);

  tmp = TREE_OPERAND (component, 0);
  last = NULL_TREE;

  while (tmp)
    {
      switch (TREE_CODE (tmp))
	{
	case CALL_EXPR:
	  if (last)
	    TREE_OPERAND (last, 0) = TREE_OPERAND (tmp, 0);
	  else
	    TREE_OPERAND (component, 0) = TREE_OPERAND (tmp, 0);

	  last = groktypename (build_tree_list (TREE_TYPE (component),
						TREE_OPERAND (component, 0)));
	  name = build_typename_overload (last);
	  TREE_TYPE (name) = last;

	  if (TREE_OPERAND (tmp, 0)
	      && TREE_OPERAND (tmp, 0) != void_list_node)
	    {
	      cp_error ("`operator %T' requires empty parameter list", last);
	      TREE_OPERAND (tmp, 0) = NULL_TREE;
	    }

	  if (of && TREE_CODE (of) != TYPE_DECL)
	    return build_method_call (of, name, NULL_TREE, NULL_TREE, flags);
	  else if (of)
	    {
	      tree this_this;

	      if (current_class_decl == NULL_TREE)
		{
		  cp_error ("object required for `operator %T' call",
			    TREE_TYPE (name));
		  return error_mark_node;
		}

	      this_this = convert_pointer_to (TREE_TYPE (of),
					      current_class_decl);
	      this_this = build_indirect_ref (this_this, NULL_PTR);
	      return build_method_call (this_this, name, NULL_TREE,
					NULL_TREE, flags | LOOKUP_NONVIRTUAL);
	    }
	  else if (current_class_decl)
	    return build_method_call (tmp, name, NULL_TREE, NULL_TREE, flags);

	  cp_error ("object required for `operator %T' call",
		    TREE_TYPE (name));
	  return error_mark_node;

	case INDIRECT_REF:
	case ADDR_EXPR:
	case ARRAY_REF:
	  break;

	case SCOPE_REF:
	  my_friendly_assert (cname == 0, 255);
	  cname = TREE_OPERAND (tmp, 0);
	  tmp = TREE_OPERAND (tmp, 1);
	  break;

	default:
	  my_friendly_abort (77);
	}
      last = tmp;
      tmp = TREE_OPERAND (tmp, 0);
    }

  last = groktypename (build_tree_list (TREE_TYPE (component), TREE_OPERAND (component, 0)));
  name = build_typename_overload (last);
  TREE_TYPE (name) = last;
  if (of && TREE_CODE (of) == TYPE_DECL)
    {
      if (cname == NULL_TREE)
	{
	  cname = DECL_NAME (of);
	  of = NULL_TREE;
	}
      else my_friendly_assert (cname == DECL_NAME (of), 256);
    }

  if (of)
    {
      tree this_this;

      if (current_class_decl == NULL_TREE)
	{
	  cp_error ("object required for `operator %T' call",
		    TREE_TYPE (name));
	  return error_mark_node;
	}

      this_this = convert_pointer_to (TREE_TYPE (of), current_class_decl);
      return build_component_ref (this_this, name, 0, protect);
    }
  else if (cname)
    return build_offset_ref (cname, name);
  else if (current_class_name)
    return build_offset_ref (current_class_name, name);

  cp_error ("object required for `operator %T' member reference",
	    TREE_TYPE (name));
  return error_mark_node;
}
#endif

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
  tree thunk_fndecl, thunk_id;
  tree thunk;
  char *func_name;
  static int thunk_number = 0;
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
      error_with_decl ("implementation-reserved name `%s' used");
      IDENTIFIER_GLOBAL_VALUE (thunk_id) = thunk = NULL_TREE;
    }
  if (thunk == NULL_TREE)
    {
      thunk = build_decl (THUNK_DECL, thunk_id, TREE_TYPE (func_decl));
      DECL_RESULT (thunk)
	= build_decl (RESULT_DECL, 0, TYPE_MAIN_VARIANT (TREE_TYPE (vtable_entry_type)));
      TREE_READONLY (thunk) = TYPE_READONLY (TREE_TYPE (vtable_entry_type));
      TREE_THIS_VOLATILE (thunk) = TYPE_VOLATILE (TREE_TYPE (vtable_entry_type));
      make_function_rtl (thunk);
      DECL_INITIAL (thunk) = function;
      THUNK_DELTA (thunk) = delta;
      /* So that finish_file can write out any thunks that need to be: */
      pushdecl_top_level (thunk);
    }
  return thunk;
}

void
emit_thunk (thunk_fndecl)
  tree thunk_fndecl;
{
  rtx insns;
  char *fnname;
  char buffer[250];
  tree argp;
  struct args_size stack_args_size;
  tree function = TREE_OPERAND (DECL_INITIAL (thunk_fndecl), 0);
  int delta = THUNK_DELTA (thunk_fndecl);
  int tem;
  int failure = 0;
  int current_call_is_indirect = 0;	/* needed for HPPA FUNCTION_ARG */

  /* Used to remember which regs we need to emit a USE rtx for. */
  rtx need_use[FIRST_PSEUDO_REGISTER];
  int need_use_count = 0;

  /* rtx for the 'this' parameter. */
  rtx this_rtx = 0, this_reg_rtx = 0, fixed_this_rtx;

  char *(*save_decl_printable_name) () = decl_printable_name;
  /* Data on reg parms scanned so far.  */
  CUMULATIVE_ARGS args_so_far;

  if (TREE_ASM_WRITTEN (thunk_fndecl))
    return;

  TREE_ASM_WRITTEN (thunk_fndecl) = 1;

  if (TREE_PUBLIC (function))
    {
      TREE_PUBLIC (thunk_fndecl) = 1;
      if (DECL_EXTERNAL (function))
	{
	  DECL_EXTERNAL (thunk_fndecl) = 1;
	  assemble_external (thunk_fndecl);
	  return;
	}
    }

  decl_printable_name = thunk_printable_name;
  if (current_function_decl)
    abort ();
  current_function_decl = thunk_fndecl;
  init_function_start (thunk_fndecl, input_filename, lineno);
  pushlevel (0);
  expand_start_bindings (1);

  /* Start updating where the next arg would go.  */
  INIT_CUMULATIVE_ARGS (args_so_far, TREE_TYPE (function), NULL_RTX);
  stack_args_size.constant = 0;
  stack_args_size.var = 0;
  /* SETUP for possible structure return address FIXME */

  /* Now look through all the parameters, make sure that we
     don't clobber any registers used for parameters.
     Also, pick up an rtx for the first "this" parameter. */
  for (argp = TYPE_ARG_TYPES (TREE_TYPE (function));
       argp != NULL_TREE;
       argp = TREE_CHAIN (argp))

    {
      tree passed_type = TREE_VALUE (argp);
      register rtx entry_parm;
      int named = 1; /* FIXME */
      struct args_size stack_offset;
      struct args_size arg_size;

      if (passed_type == void_type_node)
	break;

      if ((TREE_CODE (TYPE_SIZE (passed_type)) != INTEGER_CST
	   && contains_placeholder_p (TYPE_SIZE (passed_type)))
#ifdef FUNCTION_ARG_PASS_BY_REFERENCE
	  || FUNCTION_ARG_PASS_BY_REFERENCE (args_so_far,
					     TYPE_MODE (passed_type),
					     passed_type, named)
#endif
	  )
	passed_type = build_pointer_type (passed_type);

      entry_parm = FUNCTION_ARG (args_so_far,
				 TYPE_MODE (passed_type),
				 passed_type,
				 named);
      if (entry_parm != 0)
	need_use[need_use_count++] = entry_parm;

      locate_and_pad_parm (TYPE_MODE (passed_type), passed_type,
#ifdef STACK_PARMS_IN_REG_PARM_AREA
			   1,
#else
			   entry_parm != 0,
#endif
			   thunk_fndecl,
			   &stack_args_size, &stack_offset, &arg_size);

/*    REGNO (entry_parm);*/
      if (this_rtx == 0)
	{
	  this_reg_rtx = entry_parm;
	  if (!entry_parm)
	    {
	      rtx offset_rtx = ARGS_SIZE_RTX (stack_offset);

	      rtx internal_arg_pointer, stack_parm;

	      if ((ARG_POINTER_REGNUM == STACK_POINTER_REGNUM
		   || ! (fixed_regs[ARG_POINTER_REGNUM]
			 || ARG_POINTER_REGNUM == FRAME_POINTER_REGNUM)))
		internal_arg_pointer = copy_to_reg (virtual_incoming_args_rtx);
	      else
		internal_arg_pointer = virtual_incoming_args_rtx;

	      if (offset_rtx == const0_rtx)
		entry_parm = gen_rtx (MEM, TYPE_MODE (passed_type),
				      internal_arg_pointer);
	      else
		entry_parm = gen_rtx (MEM, TYPE_MODE (passed_type),
				      gen_rtx (PLUS, Pmode,
					       internal_arg_pointer, 
					       offset_rtx));
	    }
	  
	  this_rtx = entry_parm;
	}

      FUNCTION_ARG_ADVANCE (args_so_far,
			    TYPE_MODE (passed_type),
			    passed_type,
			    named);
    }

  fixed_this_rtx = plus_constant (this_rtx, delta);
  if (this_rtx != fixed_this_rtx)
    emit_move_insn (this_rtx, fixed_this_rtx);

  if (this_reg_rtx)
    emit_insn (gen_rtx (USE, VOIDmode, this_reg_rtx));

  emit_indirect_jump (XEXP (DECL_RTL (function), 0));

  while (need_use_count > 0)
    emit_insn (gen_rtx (USE, VOIDmode, need_use[--need_use_count]));

  expand_end_bindings (NULL, 1, 0);
  poplevel (0, 0, 1);

  /* From now on, allocate rtl in current_obstack, not in saveable_obstack.
     Note that that may have been done above, in save_for_inline_copying.
     The call to resume_temporary_allocation near the end of this function
     goes back to the usual state of affairs.  */

  rtl_in_current_obstack ();

  insns = get_insns ();

  /* Copy any shared structure that should not be shared.  */

  unshare_all_rtl (insns);

  /* Instantiate all virtual registers.  */

  instantiate_virtual_regs (current_function_decl, get_insns ());

  /* We are no longer anticipating cse in this function, at least.  */

  cse_not_expected = 1;

  /* Now we choose between stupid (pcc-like) register allocation
     (if we got the -noreg switch and not -opt)
     and smart register allocation.  */

  if (optimize > 0)			/* Stupid allocation probably won't work */
    obey_regdecls = 0;		/* if optimizations being done.  */

  regclass_init ();

  regclass (insns, max_reg_num ());
  if (obey_regdecls)
    {
      stupid_life_analysis (insns, max_reg_num (), NULL);
      failure = reload (insns, 0, NULL);
    }
  else
    {
      /* Do control and data flow analysis,
	 and write some of the results to dump file.  */

      flow_analysis (insns, max_reg_num (), NULL);
      local_alloc ();
      failure = global_alloc (NULL);
    }

  reload_completed = 1;

#ifdef LEAF_REGISTERS
  leaf_function = 0;
  if (optimize > 0 && only_leaf_regs_used () && leaf_function_p ())
    leaf_function = 1;
#endif

  /* If a machine dependent reorganization is needed, call it.  */
#ifdef MACHINE_DEPENDENT_REORG
   MACHINE_DEPENDENT_REORG (insns);
#endif

  /* Now turn the rtl into assembler code.  */

    {
      char *fnname = XSTR (XEXP (DECL_RTL (thunk_fndecl), 0), 0);
      assemble_start_function (thunk_fndecl, fnname);
      final (insns, asm_out_file, optimize, 0);
      assemble_end_function (thunk_fndecl, fnname);
    };

 exit_rest_of_compilation:

  reload_completed = 0;

  /* Cancel the effect of rtl_in_current_obstack.  */

  resume_temporary_allocation ();

  decl_printable_name = save_decl_printable_name;
  current_function_decl = 0;
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
void
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
      t = build (INIT_EXPR, void_type_node, C_C_D, parm);
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
	  current_base_init_list = tree_cons (TYPE_NESTED_NAME (basetype),
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
	  p = convert_from_reference (p);
	  current_base_init_list = tree_cons (TYPE_NESTED_NAME (basetype),
					      p, current_base_init_list);
	}
      for (; fields; fields = TREE_CHAIN (fields))
	{
	  tree name, init, t;
	  tree field = fields;

	  if (TREE_CODE (field) != FIELD_DECL)
	    continue;
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
	    field = largest_union_member (t);
	  else
	    continue;

	  init = build (COMPONENT_REF, TREE_TYPE (field), parm, field);
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

void
do_build_assign_ref (fndecl)
     tree fndecl;
{
  tree parm = TREE_CHAIN (DECL_ARGUMENTS (fndecl));

  clear_last_expr ();
  push_momentary ();

  parm = convert_from_reference (parm);

  if (TYPE_HAS_TRIVIAL_ASSIGN_REF (current_class_type))
    {
      tree t = build (MODIFY_EXPR, void_type_node, C_C_D, parm);
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
	  if (TYPE_HAS_ASSIGN_REF (basetype))
	    {
	      tree p = convert_to_reference
		(build_reference_type (basetype), parm,
		 CONV_IMPLICIT|CONV_CONST, LOOKUP_COMPLAIN, NULL_TREE);
	      p = convert_from_reference (p);
	      p = build_member_call (TYPE_NESTED_NAME (basetype),
				     ansi_opname [MODIFY_EXPR],
				     build_tree_list (NULL_TREE, p));
	      expand_expr_stmt (p);
	    }
	}
      for (; fields; fields = TREE_CHAIN (fields))
	{
	  tree comp, init, t;
	  tree field = fields;

	  if (TREE_CODE (field) != FIELD_DECL)
	    continue;
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
	    field = largest_union_member (t);
	  else
	    continue;

	  comp = build (COMPONENT_REF, TREE_TYPE (field), C_C_D, field);
	  init = build (COMPONENT_REF, TREE_TYPE (field), parm, field);

	  expand_expr_stmt (build_modify_expr (comp, NOP_EXPR, init));
	}
    }
  c_expand_return (C_C_D);
  pop_momentary ();
}

void push_cp_function_context ();
void pop_cp_function_context ();

void
synthesize_method (fndecl)
     tree fndecl;
{
  int nested = (current_function_decl != NULL_TREE);
  tree context = decl_function_context (fndecl);
  char *f = input_filename;
  tree base = DECL_CLASS_CONTEXT (fndecl);

  if (nested)
    push_cp_function_context (context);

  input_filename = DECL_SOURCE_FILE (fndecl);
  interface_unknown = CLASSTYPE_INTERFACE_UNKNOWN (base);
  interface_only = CLASSTYPE_INTERFACE_ONLY (base);
  start_function (NULL_TREE, fndecl, NULL_TREE, NULL_TREE, 1);
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

  /* Do we really *want* to inline this function?  */
  if (DECL_INLINE (fndecl))
    {
      /* Turn off DECL_INLINE for the moment so function_cannot_inline_p
         will check our size.  */
      DECL_INLINE (fndecl) = 0;
      if (function_cannot_inline_p (fndecl) == 0)
	DECL_INLINE (fndecl) = 1;
    }

  input_filename = f;
  extract_interface_info ();
  if (nested)
    pop_cp_function_context (context);
}
