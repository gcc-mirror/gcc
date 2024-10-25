/* Routines dealing with ObjC encoding of types
   Copyright (C) 1992-2024 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "options.h"
#include "stringpool.h"
#include "stor-layout.h"

#ifdef OBJCPLUS
#include "cp/cp-tree.h"
#else
#include "c/c-tree.h"
#include "c/c-lang.h"
#endif

#include "c-family/c-objc.h"

#include "objc-encoding.h"
#include "objc-act.h"

/* For my_build_string().  */
#include "objc-runtime-shared-support.h"

/* For BITS_PER_UNIT.  */

/* When building Objective-C++, we are not linking against the C front-end
   and so need to replicate the C tree-construction functions in some way.  */
#ifdef OBJCPLUS
#define OBJCP_REMAP_FUNCTIONS
#include "objcp-decl.h"
#endif  /* OBJCPLUS */

/* Set up for use of obstacks.  */

/* This obstack is used to accumulate the encoding of a data type.  */
static struct obstack util_obstack;

/* This points to the beginning of obstack contents, so we can free
   the whole contents.  */
static char *util_firstobj;

void objc_encoding_init (void)
{
  gcc_obstack_init (&util_obstack);
  util_firstobj = (char *) obstack_finish (&util_obstack);
}

int generating_instance_variables = 0;

static void encode_type_qualifiers (tree);
static void encode_type (tree, int, int);
static void encode_field (tree field_decl, int curtype, int format);

static tree
objc_method_parm_type (tree type)
{
  type = TREE_VALUE (TREE_TYPE (type));
  if (TREE_CODE (type) == TYPE_DECL)
    type = TREE_TYPE (type);
  return type;
}

static int
objc_encoded_type_size (tree type)
{
  int sz = int_size_in_bytes (type);

  /* Make all integer and enum types at least as large
     as an int.  */
  if (sz > 0 && INTEGRAL_TYPE_P (type))
    sz = MAX (sz, int_size_in_bytes (integer_type_node));
  /* Treat arrays as pointers, since that's how they're
     passed in.  */
  else if (TREE_CODE (type) == ARRAY_TYPE)
    sz = int_size_in_bytes (ptr_type_node);
  return sz;
}

/* Encode a method prototype.  */
tree
encode_method_prototype (tree method_decl)
{
  tree parms;
  int parm_offset, i;
  char buf[40];
  tree result;

  /* ONEWAY and BYCOPY, for remote object are the only method qualifiers.  */
  encode_type_qualifiers (TREE_PURPOSE (TREE_TYPE (method_decl)));

  /* Encode return type.  */
  encode_type (objc_method_parm_type (method_decl),
	       obstack_object_size (&util_obstack),
	       OBJC_ENCODE_INLINE_DEFS);

  /* Stack size.  */
  /* The first two arguments (self and _cmd) are pointers; account for
     their size.  */
  i = int_size_in_bytes (ptr_type_node);
  parm_offset = 2 * i;
  for (parms = METHOD_SEL_ARGS (method_decl); parms;
       parms = DECL_CHAIN (parms))
    {
      tree type = objc_method_parm_type (parms);
      int sz = objc_encoded_type_size (type);

      /* If a type size is not known, bail out.  */
      if (sz < 0)
	{
	  error_at (DECL_SOURCE_LOCATION (method_decl),
		    "type %qT does not have a known size",
		    type);
	  /* Pretend that the encoding succeeded; the compilation will
	     fail nevertheless.  */
	  goto finish_encoding;
	}
      parm_offset += sz;
    }

  sprintf (buf, "%d@0:%d", parm_offset, i);
  obstack_grow (&util_obstack, buf, strlen (buf));

  /* Argument types.  */
  parm_offset = 2 * i;
  for (parms = METHOD_SEL_ARGS (method_decl); parms;
       parms = DECL_CHAIN (parms))
    {
      tree type = objc_method_parm_type (parms);

      /* Process argument qualifiers for user supplied arguments.  */
      encode_type_qualifiers (TREE_PURPOSE (TREE_TYPE (parms)));

      /* Type.  */
      encode_type (type, obstack_object_size (&util_obstack),
		   OBJC_ENCODE_INLINE_DEFS);

      /* Compute offset.  */
      sprintf (buf, "%d", parm_offset);
      parm_offset += objc_encoded_type_size (type);

      obstack_grow (&util_obstack, buf, strlen (buf));
    }

  finish_encoding:
  obstack_1grow (&util_obstack, '\0');
  result = get_identifier (XOBFINISH (&util_obstack, char *));
  obstack_free (&util_obstack, util_firstobj);
  return result;
}

/* This is used to implement @encode().  */
tree
objc_build_encode_expr (tree type)
{
  tree result;
  const char *string;

  encode_type (type, obstack_object_size (&util_obstack),
	       OBJC_ENCODE_INLINE_DEFS);
  obstack_1grow (&util_obstack, 0);    /* null terminate string */
  string = XOBFINISH (&util_obstack, const char *);

  /* Synthesize a string that represents the encoded struct/union.  */
  result = my_build_string (strlen (string) + 1, string);
  obstack_free (&util_obstack, util_firstobj);
  return result;
}

/* "Encode" a data type into a string, which grows in util_obstack.

   The format is described in gcc/doc/objc.texi, section 'Type
   encoding'.

   Most of the encode_xxx functions have a 'type' argument, which is
   the type to encode, and an integer 'curtype' argument, which is the
   index in the encoding string of the beginning of the encoding of
   the current type, and allows you to find what characters have
   already been written for the current type (they are the ones in the
   current encoding string starting from 'curtype').

   For example, if we are encoding a method which returns 'int' and
   takes a 'char **' argument, then when we get to the point of
   encoding the 'char **' argument, the encoded string already
   contains 'i12@0:4' (assuming a pointer size of 4 bytes).  So,
   'curtype' will be set to 7 when starting to encode 'char **'.
   During the whole of the encoding of 'char **', 'curtype' will be
   fixed at 7, so the routine encoding the second pointer can find out
   that it's actually encoding a pointer to a pointer by looking
   backwards at what has already been encoded for the current type,
   and seeing there is a "^" (meaning a pointer) in there.  */


/* Encode type qualifiers encodes one of the "PQ" Objective-C
   keywords, ie 'in', 'out', 'inout', 'bycopy', 'byref', 'oneway'.
   'const', instead, is encoded directly as part of the type.  */
static void
encode_type_qualifiers (tree declspecs)
{
  tree spec;

  for (spec = declspecs; spec; spec = TREE_CHAIN (spec))
    {
      /* FIXME: Shouldn't we use token->keyword here ? */
      if (ridpointers[(int) RID_IN] == TREE_VALUE (spec))
	obstack_1grow (&util_obstack, 'n');
      else if (ridpointers[(int) RID_INOUT] == TREE_VALUE (spec))
	obstack_1grow (&util_obstack, 'N');
      else if (ridpointers[(int) RID_OUT] == TREE_VALUE (spec))
	obstack_1grow (&util_obstack, 'o');
      else if (ridpointers[(int) RID_BYCOPY] == TREE_VALUE (spec))
	obstack_1grow (&util_obstack, 'O');
      else if (ridpointers[(int) RID_BYREF] == TREE_VALUE (spec))
        obstack_1grow (&util_obstack, 'R');
      else if (ridpointers[(int) RID_ONEWAY] == TREE_VALUE (spec))
	obstack_1grow (&util_obstack, 'V');
      else
	gcc_unreachable ();
    }
}

/* Determine if a pointee is marked read-only.  Only used by the NeXT
   runtime to be compatible with gcc-3.3.  */
static bool
pointee_is_readonly (tree pointee)
{
  while (POINTER_TYPE_P (pointee))
    pointee = TREE_TYPE (pointee);

  return TYPE_READONLY (pointee);
}

/* Encode a pointer type.  */
static void
encode_pointer (tree type, int curtype, int format)
{
  tree pointer_to = TREE_TYPE (type);

  if (flag_next_runtime)
    {
      /* This code is used to be compatible with gcc-3.3.  */
      /* For historical/compatibility reasons, the read-only qualifier
	 of the pointee gets emitted _before_ the '^'.  The read-only
	 qualifier of the pointer itself gets ignored, _unless_ we are
	 looking at a typedef!  Also, do not emit the 'r' for anything
	 but the outermost type!  */
      if (!generating_instance_variables
	  && (obstack_object_size (&util_obstack) - curtype <= 1)
	  && (TYPE_NAME (type) && TREE_CODE (TYPE_NAME (type)) == TYPE_DECL
	      ? TYPE_READONLY (type)
	      : pointee_is_readonly (pointer_to)))
	obstack_1grow (&util_obstack, 'r');
    }

  if (TREE_CODE (pointer_to) == RECORD_TYPE)
    {
      if (OBJC_TYPE_NAME (pointer_to)
	  && TREE_CODE (OBJC_TYPE_NAME (pointer_to)) == IDENTIFIER_NODE)
	{
	  const char *name = IDENTIFIER_POINTER (OBJC_TYPE_NAME (pointer_to));

	  if (strcmp (name, TAG_OBJECT) == 0) /* '@' */
	    {
	      obstack_1grow (&util_obstack, '@');
	      return;
	    }
	  else if (TYPE_HAS_OBJC_INFO (pointer_to)
		   && TYPE_OBJC_INTERFACE (pointer_to))
	    {
              if (generating_instance_variables)
	        {
	          obstack_1grow (&util_obstack, '@');
	          obstack_1grow (&util_obstack, '"');
	          obstack_grow (&util_obstack, name, strlen (name));
	          obstack_1grow (&util_obstack, '"');
	          return;
		}
              else
	        {
	          obstack_1grow (&util_obstack, '@');
	          return;
		}
	    }
	  else if (strcmp (name, TAG_CLASS) == 0) /* '#' */
	    {
	      obstack_1grow (&util_obstack, '#');
	      return;
	    }
	  else if (strcmp (name, TAG_SELECTOR) == 0) /* ':' */
	    {
	      obstack_1grow (&util_obstack, ':');
	      return;
	    }
	}
    }
  else if (TREE_CODE (pointer_to) == INTEGER_TYPE
	   && TYPE_MODE (pointer_to) == QImode)
    {
      tree pname = TREE_CODE (OBJC_TYPE_NAME (pointer_to)) == IDENTIFIER_NODE
	          ? OBJC_TYPE_NAME (pointer_to)
	          : DECL_NAME (OBJC_TYPE_NAME (pointer_to));

      /* (BOOL *) are an exception and are encoded as ^c, while all
	 other pointers to char are encoded as *.   */
      if (strcmp (IDENTIFIER_POINTER (pname), "BOOL"))
	{
	  if (!flag_next_runtime)
	    {
	      /* The NeXT runtime adds the 'r' before getting here.  */

	      /* It appears that "r*" means "const char *" rather than
		 "char *const".  "char *const" is encoded as "*",
		 which is identical to "char *", so the "const" is
		 unfortunately lost.  */
	      if (TYPE_READONLY (pointer_to))
		obstack_1grow (&util_obstack, 'r');
	    }

	  obstack_1grow (&util_obstack, '*');
	  return;
	}
    }

  /* We have a normal pointer type that does not get special treatment.  */
  obstack_1grow (&util_obstack, '^');
  encode_type (pointer_to, curtype, format);
}

static void
encode_array (tree type, int curtype, int format)
{
  tree an_int_cst = TYPE_SIZE (type);
  tree array_of = TREE_TYPE (type);
  char buffer[40];

  if (an_int_cst == NULL)
    {
      /* We are trying to encode an incomplete array.  An incomplete
	 array is forbidden as part of an instance variable; but it
	 may occur if the instance variable is a pointer to such an
	 array.  */

      /* So the only case in which an incomplete array could occur
	 (without being pointed to) is if we are encoding the
	 arguments or return value of a method.  In that case, an
	 incomplete array argument or return value (eg,
	 -(void)display: (char[])string) is treated like a pointer
	 because that is how the compiler does the function call.  A
	 special, more complicated case, is when the incomplete array
	 is the last member of a struct (eg, if we are encoding
	 "struct { unsigned long int a;double b[];}"), which is again
	 part of a method argument/return value.  In that case, we
	 really need to communicate to the runtime that there is an
	 incomplete array (not a pointer!) there.  So, we detect that
	 special case and encode it as a zero-length array.

	 Try to detect that we are part of a struct.  We do this by
	 searching for '=' in the type encoding for the current type.
	 NB: This hack assumes that you can't use '=' as part of a C
	 identifier.
      */
      {
	char *enc = (char *) obstack_base (&util_obstack) + curtype;
	if (memchr (enc, '=',
		    obstack_object_size (&util_obstack) - curtype) == NULL)
	  {
	    /* We are not inside a struct.  Encode the array as a
	       pointer.  */
	    encode_pointer (type, curtype, format);
	    return;
	  }
      }

      /* Else, we are in a struct, and we encode it as a zero-length
	 array.  */
      sprintf (buffer, "[" HOST_WIDE_INT_PRINT_DEC, HOST_WIDE_INT_0);
    }
  else if (TREE_INT_CST_LOW (TYPE_SIZE (array_of)) == 0)
   sprintf (buffer, "[" HOST_WIDE_INT_PRINT_DEC, HOST_WIDE_INT_0);
  else
    sprintf (buffer, "[" HOST_WIDE_INT_PRINT_DEC,
	     TREE_INT_CST_LOW (an_int_cst)
	      / TREE_INT_CST_LOW (TYPE_SIZE (array_of)));

  obstack_grow (&util_obstack, buffer, strlen (buffer));
  encode_type (array_of, curtype, format);
  obstack_1grow (&util_obstack, ']');
  return;
}

/* Encode a vector.  The vector type is a GCC extension to C.  */
static void
encode_vector (tree type, int curtype, int format)
{
  tree vector_of = TREE_TYPE (type);
  char buffer[40];

  /* Vectors are like simple fixed-size arrays.  */

  /* Output ![xx,yy,<code>] where xx is the vector_size, yy is the
     alignment of the vector, and <code> is the base type.  Eg, int
     __attribute__ ((vector_size (16))) gets encoded as ![16,32,i]
     assuming that the alignment is 32 bytes.  We include size and
     alignment in bytes so that the runtime does not have to have any
     knowledge of the actual types.
  */
  sprintf (buffer, "![" HOST_WIDE_INT_PRINT_DEC ",%d",
	   /* We want to compute the equivalent of sizeof (<vector>).
	      Code inspired by c_sizeof_or_alignof_type.  */
	   ((TREE_INT_CST_LOW (TYPE_SIZE_UNIT (type))
	     / (TYPE_PRECISION (char_type_node) / BITS_PER_UNIT))),
	   /* We want to compute the equivalent of __alignof__
	      (<vector>).  Code inspired by
	      c_sizeof_or_alignof_type.  */
	   TYPE_ALIGN_UNIT (type));
  obstack_grow (&util_obstack, buffer, strlen (buffer));
  encode_type (vector_of, curtype, format);
  obstack_1grow (&util_obstack, ']');
  return;
}

static void
encode_aggregate_fields (tree type, bool pointed_to, int curtype, int format)
{
  tree field = TYPE_FIELDS (type);

  for (; field; field = DECL_CHAIN (field))
    {
#ifdef OBJCPLUS
      /* C++ static members, and things that are not field at all,
	 should not appear in the encoding.  */
      if (TREE_CODE (field) != FIELD_DECL || TREE_STATIC (field))
	continue;
#endif

      /* Recursively encode fields of embedded base classes.  */
      if (DECL_ARTIFICIAL (field) && !DECL_NAME (field)
	  && TREE_CODE (TREE_TYPE (field)) == RECORD_TYPE)
	{
	  encode_aggregate_fields (TREE_TYPE (field),
				   pointed_to, curtype, format);
	  continue;
	}

      if (generating_instance_variables && !pointed_to)
	{
	  tree fname = DECL_NAME (field);

	  obstack_1grow (&util_obstack, '"');

	  if (fname && TREE_CODE (fname) == IDENTIFIER_NODE)
	    obstack_grow (&util_obstack,
			  IDENTIFIER_POINTER (fname),
			  strlen (IDENTIFIER_POINTER (fname)));

	  obstack_1grow (&util_obstack, '"');
        }

      encode_field (field, curtype, format);
    }
}

static void
encode_aggregate_within (tree type, int curtype, int format, int left,
			 int right)
{
  tree name;
  /* NB: aggregates that are pointed to have slightly different encoding
     rules in that you never encode the names of instance variables.  */
  int ob_size = obstack_object_size (&util_obstack);
  bool inline_contents = false;
  bool pointed_to = false;

  if (flag_next_runtime)
    {
      if (ob_size > 0
	  && *((char *) obstack_next_free (&util_obstack) - 1) == '^')
	pointed_to = true;

      if ((format == OBJC_ENCODE_INLINE_DEFS || generating_instance_variables)
	  && (!pointed_to || ob_size - curtype == 1
	      || (ob_size - curtype == 2
		  && *((char *) obstack_next_free (&util_obstack) - 2) == 'r')))
	inline_contents = true;
    }
  else
    {
      /* c0 and c1 are the last two characters in the encoding of the
	 current type; if the last two characters were '^' or '^r',
	 then we are encoding an aggregate that is "pointed to".  The
	 comment above applies: in that case we should avoid encoding
	 the names of instance variables.
      */
      char c0, c1;

      c1 = ob_size > 1 ? *((char *) obstack_next_free (&util_obstack) - 2) : 0;
      c0 = ob_size > 0 ? *((char *) obstack_next_free (&util_obstack) - 1) : 0;
      if (c0 == '^' || (c1 == '^' && c0 == 'r'))
	pointed_to = true;

      if (format == OBJC_ENCODE_INLINE_DEFS || generating_instance_variables)
	{
	  if (!pointed_to)
	    inline_contents = true;
	  else
	    {
	      /* Note that the check (ob_size - curtype < 2) prevents
		 infinite recursion when encoding a structure which is
		 a linked list (eg, struct node { struct node *next;
		 }).  Each time we follow a pointer, we add one
		 character to ob_size, and curtype is fixed, so after
		 at most two pointers we stop inlining contents and
		 break the loop.

		 The other case where we don't inline is "^r", which
		 is a pointer to a constant struct.
	      */
	      if ((ob_size - curtype <= 2) && !(c0 == 'r'))
		inline_contents = true;
	    }
	}
    }

  /* Traverse struct aliases; it is important to get the
     original struct and its tag name (if any).  */
  type = TYPE_MAIN_VARIANT (type);
  name = OBJC_TYPE_NAME (type);
  /* Open parenth/bracket.  */
  obstack_1grow (&util_obstack, left);

  /* Encode the struct/union tag name, or '?' if a tag was
     not provided.  Typedef aliases do not qualify.  */
#ifdef OBJCPLUS
  /* For compatibility with the NeXT runtime, ObjC++ encodes template
     args as a composite struct tag name. */
  if (name && TREE_CODE (name) == IDENTIFIER_NODE
      /* Did this struct have a tag?  */
      && !TYPE_WAS_UNNAMED (type))
    obstack_grow (&util_obstack,
		  decl_as_string (type, TFF_DECL_SPECIFIERS | TFF_UNQUALIFIED_NAME),
		  strlen (decl_as_string (type, TFF_DECL_SPECIFIERS | TFF_UNQUALIFIED_NAME)));
#else
  if (name && TREE_CODE (name) == IDENTIFIER_NODE)
    obstack_grow (&util_obstack,
		  IDENTIFIER_POINTER (name),
		  strlen (IDENTIFIER_POINTER (name)));
#endif
  else
    obstack_1grow (&util_obstack, '?');

  /* Encode the types (and possibly names) of the inner fields,
     if required.  */
  if (inline_contents)
    {
      obstack_1grow (&util_obstack, '=');
      encode_aggregate_fields (type, pointed_to, curtype, format);
    }
  /* Close parenth/bracket.  */
  obstack_1grow (&util_obstack, right);
}

/* Encode a bitfield NeXT-style (i.e., without a bit offset or the underlying
   field type.  */
static void
encode_next_bitfield (int width)
{
  char buffer[40];
  sprintf (buffer, "b%d", width);
  obstack_grow (&util_obstack, buffer, strlen (buffer));
}

/* Encodes 'type', ignoring type qualifiers (which you should encode
   beforehand if needed) with the exception of 'const', which is
   encoded by encode_type.  See above for the explanation of
   'curtype'.  'format' can be OBJC_ENCODE_INLINE_DEFS or
   OBJC_ENCODE_DONT_INLINE_DEFS.  */
static void
encode_type (tree type, int curtype, int format)
{
  enum tree_code code = TREE_CODE (type);

  /* Ignore type qualifiers other than 'const' when encoding a
     type.  */

  if (type == error_mark_node)
    return;

  if (!flag_next_runtime)
    {
      if (TYPE_READONLY (type))
	obstack_1grow (&util_obstack, 'r');
    }

  switch (code)
    {
    case ENUMERAL_TYPE:
      if (flag_next_runtime)
	{
	  /* Kludge for backwards-compatibility with gcc-3.3: enums
	     are always encoded as 'i' no matter what type they
	     actually are (!).  */
	  obstack_1grow (&util_obstack, 'i');
	  break;
	}
      /* Else, they are encoded exactly like the integer type that is
	 used by the compiler to store them.  */
      /* FALLTHRU */
    case INTEGER_TYPE:
      {
	char c;
	switch (GET_MODE_BITSIZE (SCALAR_INT_TYPE_MODE (type)))
	  {
	  case 8:  c = TYPE_UNSIGNED (type) ? 'C' : 'c'; break;
	  case 16: c = TYPE_UNSIGNED (type) ? 'S' : 's'; break;
	  case 32:
	    {
	      tree int_type = type;
	      if (flag_next_runtime)
		{
		  /* Another legacy kludge for compatibility with
		     gcc-3.3: 32-bit longs are encoded as 'l' or 'L',
		     but not always.  For typedefs, we need to use 'i'
		     or 'I' instead if encoding a struct field, or a
		     pointer!  */
		  int_type =  ((!generating_instance_variables
				&& (obstack_object_size (&util_obstack)
				    == (unsigned) curtype))
			       ? TYPE_MAIN_VARIANT (type)
			       : type);
		}
	      if (int_type == long_unsigned_type_node
		  || int_type == long_integer_type_node)
		c = TYPE_UNSIGNED (type) ? 'L' : 'l';
	      else
		c = TYPE_UNSIGNED (type) ? 'I' : 'i';
	    }
	    break;
	  case 64:  c = TYPE_UNSIGNED (type) ? 'Q' : 'q'; break;
	  case 128: c = TYPE_UNSIGNED (type) ? 'T' : 't'; break;
	  default: gcc_unreachable ();
	  }
	obstack_1grow (&util_obstack, c);
	break;
      }
    case REAL_TYPE:
      {
	char c;
	/* Floating point types.  */
	switch (GET_MODE_BITSIZE (SCALAR_FLOAT_TYPE_MODE (type)))
	  {
	  case 32:  c = 'f'; break;
	  case 64:  c = 'd'; break;
	  case 96:
	  case 128: c = 'D'; break;
	  default: gcc_unreachable ();
	  }
	obstack_1grow (&util_obstack, c);
	break;
      }
    case VOID_TYPE:
      obstack_1grow (&util_obstack, 'v');
      break;

    case BOOLEAN_TYPE:
      obstack_1grow (&util_obstack, 'B');
      break;

    case ARRAY_TYPE:
      encode_array (type, curtype, format);
      break;

    case POINTER_TYPE:
#ifdef OBJCPLUS
    case REFERENCE_TYPE:
#endif
      encode_pointer (type, curtype, format);
      break;

    case RECORD_TYPE:
      encode_aggregate_within (type, curtype, format, '{', '}');
      break;

    case UNION_TYPE:
      encode_aggregate_within (type, curtype, format, '(', ')');
      break;

    case FUNCTION_TYPE: /* '?' means an unknown type.  */
      obstack_1grow (&util_obstack, '?');
      break;

    case COMPLEX_TYPE:
      /* A complex is encoded as 'j' followed by the inner type (eg,
	 "_Complex int" is encoded as 'ji').  */
      obstack_1grow (&util_obstack, 'j');
      encode_type (TREE_TYPE (type), curtype, format);
      break;

    case VECTOR_TYPE:
      encode_vector (type, curtype, format);
      break;

    default:
      warning (0, "unknown type %<%T%> found during Objective-C encoding",
	       TREE_TYPE (type));
      obstack_1grow (&util_obstack, '?');
      break;
    }

  if (flag_next_runtime)
    {
      /* Super-kludge.  Some ObjC qualifier and type combinations need
	 to be rearranged for compatibility with gcc-3.3.  */
      if (code == POINTER_TYPE && obstack_object_size (&util_obstack) >= 3)
	{
	  char *enc = (char *) obstack_base (&util_obstack) + curtype;

	  /* Rewrite "in const" from "nr" to "rn".  */
	  if (curtype >= 1 && startswith (enc - 1, "nr"))
	    memcpy (enc - 1, "rn", 2);
	}
    }
}

static void
encode_gnu_bitfield (int position, tree type, int size)
{
  enum tree_code code = TREE_CODE (type);
  char buffer[40];
  char charType = '?';

  /* This code is only executed for the GNU runtime, so we can ignore
     the NeXT runtime kludge of always encoding enums as 'i' no matter
     what integers they actually are.  */
  if (code == INTEGER_TYPE  ||  code == ENUMERAL_TYPE)
    {
      if (integer_zerop (TYPE_MIN_VALUE (type)))
	/* Unsigned integer types.  */
	{
	  switch (TYPE_MODE (type))
	    {
	    case E_QImode:
	      charType = 'C'; break;
	    case E_HImode:
	      charType = 'S'; break;
	    case E_SImode:
	      {
		if (type == long_unsigned_type_node)
		  charType = 'L';
		else
		  charType = 'I';
		break;
	      }
	    case E_DImode:
	      charType = 'Q'; break;
	    default:
	      gcc_unreachable ();
	    }
	}
      else
	/* Signed integer types.  */
	{
	  switch (TYPE_MODE (type))
	    {
	    case E_QImode:
	      charType = 'c'; break;
	    case E_HImode:
	      charType = 's'; break;
	    case E_SImode:
	      {
		if (type == long_integer_type_node)
		  charType = 'l';
		else
		  charType = 'i';
		break;
	      }
	    case E_DImode:
	      charType = 'q'; break;
	    default:
	      gcc_unreachable ();
	    }
	}
    }
  else
    {
      /* Do not do any encoding, produce an error and keep going.  */
      error ("trying to encode non-integer type as a bit-field");
      return;
    }

  sprintf (buffer, "b%d%c%d", position, charType, size);
  obstack_grow (&util_obstack, buffer, strlen (buffer));
}

static void
encode_field (tree field_decl, int curtype, int format)
{
#ifdef OBJCPLUS
  /* C++ static members, and things that are not fields at all,
     should not appear in the encoding.  */
  if (TREE_CODE (field_decl) != FIELD_DECL || TREE_STATIC (field_decl))
    return;
#endif

  /* Generate the bitfield typing information, if needed.  Note the difference
     between GNU and NeXT runtimes.  */
  if (DECL_BIT_FIELD_TYPE (field_decl))
    {
      int size = tree_to_uhwi (DECL_SIZE (field_decl));

      if (flag_next_runtime)
	encode_next_bitfield (size);
      else
	encode_gnu_bitfield (int_bit_position (field_decl),
			     DECL_BIT_FIELD_TYPE (field_decl), size);
    }
  else
    encode_type (TREE_TYPE (field_decl), curtype, format);
}

tree
encode_field_decl (tree field_decl)
{
  tree result;

  encode_field (field_decl,
		obstack_object_size (&util_obstack),
		OBJC_ENCODE_DONT_INLINE_DEFS);

  /* Null terminate string.  */
  obstack_1grow (&util_obstack, 0);

  /* Get identifier for the string.  */
  result = get_identifier (XOBFINISH (&util_obstack, char *));
  obstack_free (&util_obstack, util_firstobj);

  return result;
}

/* This routine encodes the attribute of the input PROPERTY according
   to following formula:

   Property attributes are stored as a comma-delimited C string.
   Simple attributes such as readonly are encoded as single
   character. The parametrized attributes, getter=name and
   setter=name, are encoded as a single character followed by an
   identifier.  Property types are also encoded as a parametrized
   attribute.  The characters used to encode these attributes are
   defined by the following enumeration:

   enum PropertyAttributes {
     kPropertyReadOnly = 'R',
     kPropertyBycopy = 'C',
     kPropertyByref = '&',
     kPropertyDynamic = 'D',
     kPropertyGetter = 'G',
     kPropertySetter = 'S',
     kPropertyInstanceVariable = 'V',
     kPropertyType = 'T',
     kPropertyWeak = 'W',
     kPropertyStrong = 'P',
     kPropertyNonAtomic = 'N'
   };  */
tree
objc_v2_encode_prop_attr (tree property)
{
  const char *string;
  tree type = TREE_TYPE (property);

  obstack_1grow (&util_obstack, 'T');
  encode_type (type, obstack_object_size (&util_obstack),
	       OBJC_ENCODE_INLINE_DEFS);

  if (PROPERTY_READONLY (property))
    obstack_grow (&util_obstack, ",R", 2);

  switch (PROPERTY_ASSIGN_SEMANTICS (property))
    {
    case OBJC_PROPERTY_COPY:
      obstack_grow (&util_obstack, ",C", 2);
      break;
    case OBJC_PROPERTY_RETAIN:
      obstack_grow (&util_obstack, ",&", 2);
      break;
    case OBJC_PROPERTY_ASSIGN:
    default:
      break;
    }

  if (PROPERTY_DYNAMIC (property))
    obstack_grow (&util_obstack, ",D", 2);

  if (PROPERTY_NONATOMIC (property))
    obstack_grow (&util_obstack, ",N", 2);

  /* Here we want to encode the getter name, but only if it's not the
     standard one.  */
  if (PROPERTY_GETTER_NAME (property) != PROPERTY_NAME (property))
    {
      obstack_grow (&util_obstack, ",G", 2);
      string = IDENTIFIER_POINTER (PROPERTY_GETTER_NAME (property));
      obstack_grow (&util_obstack, string, strlen (string));
    }

  if (!PROPERTY_READONLY (property))
    {
      /* Here we want to encode the setter name, but only if it's not
	 the standard one.  */
      tree standard_setter = get_identifier (objc_build_property_setter_name (PROPERTY_NAME (property)));
      if (PROPERTY_SETTER_NAME (property) != standard_setter)
	{
	  obstack_grow (&util_obstack, ",S", 2);
	  string = IDENTIFIER_POINTER (PROPERTY_SETTER_NAME (property));
	  obstack_grow (&util_obstack, string, strlen (string));
	}
    }

  /* TODO: Encode strong ('P'), weak ('W') for garbage collection.  */

  if (!PROPERTY_DYNAMIC (property))
    {
      obstack_grow (&util_obstack, ",V", 2);
      if (PROPERTY_IVAR_NAME (property))
	string = IDENTIFIER_POINTER (PROPERTY_IVAR_NAME (property));
      else
	string = IDENTIFIER_POINTER (PROPERTY_NAME (property));
      obstack_grow (&util_obstack, string, strlen (string));
    }

  /* NULL-terminate string.  */
  obstack_1grow (&util_obstack, 0);
  string = XOBFINISH (&util_obstack, char *);
  obstack_free (&util_obstack, util_firstobj);
  return get_identifier (string);
}
