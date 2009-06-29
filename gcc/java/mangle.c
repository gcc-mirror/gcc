/* Functions related to mangling class names for the GNU compiler
   for the Java(TM) language.
   Copyright (C) 1998, 1999, 2001, 2002, 2003, 2006, 2007, 2008
   Free Software Foundation, Inc.

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
<http://www.gnu.org/licenses/>. 

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

/* Written by Per Bothner <bothner@cygnus.com> */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "jcf.h"
#include "tree.h"
#include "java-tree.h"
#include "obstack.h"
#include "toplev.h"
#include "ggc.h"
#include "langhooks-def.h"

static void mangle_class_field (tree);
static void mangle_vtable (tree);
static void mangle_field_decl (tree);
static void mangle_method_decl (tree);
static void mangle_local_cni_method_decl (tree);

static void mangle_type (tree);
static void mangle_pointer_type (tree);
static void mangle_array_type (tree);
static int  mangle_record_type (tree, int);

static int find_compression_pointer_match (tree);
static int find_compression_array_match (tree);
static int find_compression_record_match (tree, tree *);
static int find_compression_array_template_match (tree);

static void set_type_package_list (tree);
static int  entry_match_pointer_p (tree, int);
static void emit_compression_string (int);

static void init_mangling (void);
static tree finish_mangling (void);
static void compression_table_add (tree);

static void mangle_member_name (tree);

static struct obstack mangle_obstack_1;
struct obstack *mangle_obstack;

#define MANGLE_RAW_STRING(S) \
  obstack_grow (mangle_obstack, (S), sizeof (S)-1)

/* atms: array template mangled string. */
static GTY(()) tree atms;

/* This is the mangling interface: a decl, a class field (.class) and
   the vtable. */

void
java_mangle_decl (tree decl)
{
  /* A copy of the check from the beginning of lhd_set_decl_assembler_name.
     Only FUNCTION_DECLs and VAR_DECLs for variables with static storage
     duration need a real DECL_ASSEMBLER_NAME.  */
  gcc_assert (TREE_CODE (decl) == FUNCTION_DECL
	      || (TREE_CODE (decl) == VAR_DECL
		  && (TREE_STATIC (decl)
		      || DECL_EXTERNAL (decl)
		      || TREE_PUBLIC (decl))));
  
  /* Mangling only applies to class members.  */
  if (DECL_CONTEXT (decl) && TYPE_P (DECL_CONTEXT (decl)))
    {
      init_mangling ();
      switch (TREE_CODE (decl))
	{
	case VAR_DECL:
	  if (DECL_LANG_SPECIFIC (decl))
	    {
	      if (DECL_CLASS_FIELD_P (decl))
		{
		  mangle_class_field (decl);
		  break;
		}
	      else if (DECL_VTABLE_P (decl))
		{
		  mangle_vtable (DECL_CONTEXT (decl));
		  break;
		}
	    }
	  mangle_field_decl (decl);
	  break;

	case FUNCTION_DECL:
	  if (DECL_LANG_SPECIFIC (decl) && DECL_LOCAL_CNI_METHOD_P (decl))
	    mangle_local_cni_method_decl (decl);
	  else
	    mangle_method_decl (decl);
	  break;

	default:
	  gcc_unreachable ();
	}
      SET_DECL_ASSEMBLER_NAME (decl, finish_mangling ());
    }
  else
    lhd_set_decl_assembler_name (decl);
}

/* Beginning of the helper functions */

static void
mangle_class_field (tree decl)
{
  tree type = DECL_CONTEXT (decl);
  mangle_record_type (type, /* for_pointer = */ 0);
  if (TREE_CODE (TREE_TYPE (decl)) == RECORD_TYPE)
    MANGLE_RAW_STRING ("6class$");
  else
    MANGLE_RAW_STRING ("7class$$");
  obstack_1grow (mangle_obstack, 'E');
}

static void
mangle_vtable (tree type)
{
  MANGLE_RAW_STRING ("TV");
  mangle_record_type (type, /* for_pointer = */ 0);
  obstack_1grow (mangle_obstack, 'E');
}

/* This mangles a field decl */

static void
mangle_field_decl (tree decl)
{
  /* Mangle the name of the this the field belongs to */
  mangle_record_type (DECL_CONTEXT (decl), /* for_pointer = */ 0);
  
  /* Mangle the name of the field */
  mangle_member_name (DECL_NAME (decl));

  /* Terminate the mangled name */
  obstack_1grow (mangle_obstack, 'E');
}

/* This mangles a method decl, first mangling its name and then all
   its arguments. */

static void
mangle_method_decl (tree mdecl)
{
  tree method_name = DECL_NAME (mdecl);
  tree arglist;

  /* Mangle the name of the type that contains mdecl */
  mangle_record_type (DECL_CONTEXT (mdecl), /* for_pointer = */ 0);

  /* Mangle the function name.  There are two cases:
       - mdecl is a constructor, use `C1' for its name, (denotes a
         complete object constructor.)
       - mdecl is not a constructor, standard mangling is performed.
     We terminate the mangled function name with a `E'. */
  if (ID_INIT_P (method_name))
    obstack_grow (mangle_obstack, "C1", 2);
  else
    mangle_member_name (method_name);
  obstack_1grow (mangle_obstack, 'E');

  /* We mangled type.methodName. Now onto the arguments. */
  arglist = TYPE_ARG_TYPES (TREE_TYPE (mdecl));
  if (TREE_CODE (TREE_TYPE (mdecl)) == METHOD_TYPE)
    arglist = TREE_CHAIN (arglist);
  
  /* Output literal 'J' and mangle the return type IF not a 
     constructor.  */
  if (!ID_INIT_P (method_name))
    {
      obstack_1grow (mangle_obstack, 'J');
      mangle_type(TREE_TYPE(TREE_TYPE(mdecl)));
    }
  
  /* No arguments is easy. We shortcut it. */
  if (arglist == end_params_node)
    obstack_1grow (mangle_obstack, 'v');
  else
    {
      tree arg;
      for (arg = arglist; arg != end_params_node;  arg = TREE_CHAIN (arg))
	mangle_type (TREE_VALUE (arg));
    }
}

/* This mangles a CNI method for a local class.  If the target supports
   hidden aliases, then G++ will have generated one for us.  It is the
   responsibility of java_mark_class_local to check target support, since
   we need to set DECL_VISIBILITY (or not) much earlier.  */

static void
mangle_local_cni_method_decl (tree decl)
{
  MANGLE_RAW_STRING ("GA");
  mangle_method_decl (decl);
}

/* This mangles a member name, like a function name or a field
   name. Handle cases were `name' is a C++ keyword.  Return a nonzero
   value if unicode encoding was required.  */

static void
mangle_member_name (tree name)
{
  append_gpp_mangled_name (IDENTIFIER_POINTER (name),
			   IDENTIFIER_LENGTH (name));
}

/* Append the mangled name of TYPE onto OBSTACK.  */

static void
mangle_type (tree type)
{
  switch (TREE_CODE (type))
    {
      char code;
    case BOOLEAN_TYPE: code = 'b';  goto primitive;
    case VOID_TYPE:    code = 'v';  goto primitive;
    case INTEGER_TYPE:
      if (type == char_type_node || type == promoted_char_type_node)
	{
	  code = 'w';
	  goto primitive;
	}
      /* Get the original type instead of the arguments promoted type.
	 Avoid symbol name clashes. Should call a function to do that.
	 FIXME.  */
      if (type == promoted_short_type_node)
	type = short_type_node;
      if (type == promoted_byte_type_node)
        type = byte_type_node;
      switch (TYPE_PRECISION (type))
	{
	case  8:       code = 'c';  goto primitive;
	case 16:       code = 's';  goto primitive;
	case 32:       code = 'i';  goto primitive;
	case 64:       code = 'x';  goto primitive;
	default:  goto bad_type;
	}
    primitive:
      obstack_1grow (mangle_obstack, code);
      break;

    case REAL_TYPE:
      switch (TYPE_PRECISION (type))
	{
	case 32:       code = 'f';  goto primitive;
	case 64:       code = 'd';  goto primitive;
	default:  goto bad_type;
	}
    case POINTER_TYPE:
      if (TYPE_ARRAY_P (TREE_TYPE (type)))
	mangle_array_type (type);
      else
	mangle_pointer_type (type);
      break;
    bad_type:
    default:
      gcc_unreachable ();
    }
}

/* The compression table is a vector that keeps track of things we've
   already seen, so they can be reused. For example, java.lang.Object
   would generate three entries: two package names and a type. If
   java.lang.String is presented next, the java.lang will be matched
   against the first two entries (and kept for compression as S0_), and
   type String would be added to the table. See mangle_record_type.
   COMPRESSION_NEXT is the index to the location of the next insertion
   of an element.  */

static GTY(()) tree compression_table;
static int  compression_next;

/* Find a POINTER_TYPE in the compression table. Use a special
   function to match pointer entries and start from the end */

static int
find_compression_pointer_match (tree type)
{
  int i;

  for (i = compression_next-1; i >= 0; i--)
    if (entry_match_pointer_p (type, i))
      return i;
  return -1;
}

/* Already recorder arrays are handled like pointer as they're always
   associated with it.  */

static int
find_compression_array_match (tree type)
{
  return find_compression_pointer_match (type);
}

/* Match the table of type against STRING.  */

static int
find_compression_array_template_match (tree string)
{
  int i;
  for (i = 0; i < compression_next; i++)
    if (TREE_VEC_ELT (compression_table, i) == string) 
      return i;
  return -1;
}

/* We go through the compression table and try to find a complete or
   partial match. The function returns the compression table entry
   that (eventually partially) matches TYPE. *NEXT_CURRENT can be set
   to the rest of TYPE to be mangled. */

static int
find_compression_record_match (tree type, tree *next_current)
{
  int i, match = -1;
  tree current, saved_current = NULL_TREE;

  current = TYPE_PACKAGE_LIST (type);
      
  for (i = 0; i < compression_next; i++)
    {
      tree compression_entry = TREE_VEC_ELT (compression_table, i);
      if (current && compression_entry == TREE_PURPOSE (current))
        {
	  match = i;
	  saved_current = current;
	  current = TREE_CHAIN (current);
	}
      else
	/* We don't want to match an element that appears in the middle
	   of a package name, so skip forward to the next complete type name.
	   IDENTIFIER_NODEs (except for a "6JArray") are partial package
	   names while RECORD_TYPEs represent complete type names. */
	while (i < compression_next 
	       && TREE_CODE (compression_entry) == IDENTIFIER_NODE
	       && compression_entry != atms)
	  compression_entry = TREE_VEC_ELT (compression_table, ++i);
    }

  if (!next_current)
    return match;

  /* If we have a match, set next_current to the item next to the last
     matched value. */
  if (match >= 0)
    *next_current = TREE_CHAIN (saved_current);
  /* We had no match: we'll have to start from the beginning. */
  if (match < 0)
    *next_current = TYPE_PACKAGE_LIST (type);

  return match;
}

/* Mangle a record type. If a nonzero value is returned, it means
   that a 'N' was emitted (so that a matching 'E' can be emitted if
   necessary.)  FOR_POINTER indicates that this element is for a pointer
   symbol, meaning it was preceded by a 'P'. */

static int
mangle_record_type (tree type, int for_pointer)
{
  tree current;
  int match;
  int nadded_p = 0;
  int qualified;
  
  /* Does this name have a package qualifier? */
  qualified = QUALIFIED_P (DECL_NAME (TYPE_NAME (type)));

#define ADD_N() \
  do { obstack_1grow (mangle_obstack, 'N'); nadded_p = 1; } while (0)

  gcc_assert (TREE_CODE (type) == RECORD_TYPE);

  if (!TYPE_PACKAGE_LIST (type))
    set_type_package_list (type);

  match = find_compression_record_match (type, &current);
  if (match >= 0)
    {
      /* If we had a pointer, and there's more, we need to emit
	 'N' after 'P' (for_pointer tells us we already emitted it.) */
      if (for_pointer && current)
	ADD_N();
      emit_compression_string (match);
    }
  while (current)
    {
      /* Add the new type to the table */
      compression_table_add (TREE_PURPOSE (current));
      /* Add 'N' if we never got a chance to, but only if we have a qualified
         name.  For non-pointer elements, the name is always qualified. */
      if ((qualified || !for_pointer) && !nadded_p)
	ADD_N();
      /* Use the bare type name for the mangle. */
      append_gpp_mangled_name (IDENTIFIER_POINTER (TREE_VALUE (current)),
			       IDENTIFIER_LENGTH (TREE_VALUE (current)));
      current = TREE_CHAIN (current);
    }
  return nadded_p;
#undef ADD_N
}

/* Mangle a pointer type. There are two cases: the pointer is already
   in the compression table: the compression is emitted sans 'P'
   indicator. Otherwise, a 'P' is emitted and, depending on the type,
   a partial compression or/plus the rest of the mangling. */

static void
mangle_pointer_type (tree type)
{
  int match;
  tree pointer_type;

  /* Search for the type already in the compression table */
  if ((match = find_compression_pointer_match (type)) >= 0) 
    {
      emit_compression_string (match);
      return;
    }
  
  /* This didn't work. We start by mangling the pointed-to type */
  pointer_type = type;
  type = TREE_TYPE (type);
  gcc_assert (TREE_CODE (type) == RECORD_TYPE);
  
  obstack_1grow (mangle_obstack, 'P');
  if (mangle_record_type (type, /* for_pointer = */ 1))
    obstack_1grow (mangle_obstack, 'E');

  /* Don't forget to insert the pointer type in the table */
  compression_table_add (pointer_type);
}

/* Mangle an array type. Search for an easy solution first, then go
   through the process of finding out whether the bare array type or even
   the template indicator were already used and compressed appropriately.
   It handles pointers. */

static void
mangle_array_type (tree p_type)
{
  tree type, elt_type;
  int match;

  type = TREE_TYPE (p_type);
  gcc_assert (type);

  elt_type = TYPE_ARRAY_ELEMENT (type);

  /* We cache a bit of the Jarray <> mangle. */
  if (!atms)
    {
      atms = get_identifier ("6JArray");
    }

  /* Maybe we have what we're looking for in the compression table. */
  if ((match = find_compression_array_match (p_type)) >= 0)
    {
      emit_compression_string (match);
      return;
    }

  /* We know for a fact that all arrays are pointers */
  obstack_1grow (mangle_obstack, 'P');
  /* Maybe we already have a Jarray<t> somewhere. PSx_ will be enough. */
  if ((match = find_compression_record_match (type, NULL)) > 0)
    {
      emit_compression_string (match);
      return;
    }

  /* Maybe we already have just JArray somewhere */
  if ((match = find_compression_array_template_match (atms)) > 0)
    emit_compression_string (match);
  else
    {
      /* Start the template mangled name */
      obstack_grow (mangle_obstack, 
		    IDENTIFIER_POINTER (atms), IDENTIFIER_LENGTH (atms));
      /* Insert in the compression table */
      compression_table_add (atms);
    } 

  /* Mangle Jarray <elt_type> */
  obstack_1grow (mangle_obstack, 'I');
  mangle_type (elt_type);
  obstack_1grow (mangle_obstack, 'E');

  /* Add `Jarray <elt_type>' and `Jarray <elt_type> *' to the table */
  compression_table_add (type);
  compression_table_add (p_type);
}

/* Write a substitution string for entry I. Substitution string starts a
   -1 (encoded S_.) The base is 36, and the code shamelessly taken from
   cp/mangle.c.  */

static void
emit_compression_string (int i)
{
  i -= 1;			/* Adjust */
  obstack_1grow (mangle_obstack, 'S');
  if (i >= 0)
    {
      static const char digits[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
      unsigned HOST_WIDE_INT n;
      unsigned HOST_WIDE_INT m=1;
      /* How many digits for I in base 36? */
      for (n = i; n >= 36; n /= 36, m *=36);
      /* Write the digits out */
      while (m > 0)
	{
	  int digit = i / m;
	  obstack_1grow (mangle_obstack, digits [digit]);
	  i -= digit * m;
	  m /= 36;
	}
    }
  obstack_1grow (mangle_obstack, '_');
}

/* If search the compression table at index I for a pointer type
   equivalent to TYPE (meaning that after all the indirection, which
   might all be unique, we find the same RECORD_TYPE.) */

static int
entry_match_pointer_p (tree type, int i)
{
  tree t = TREE_VEC_ELT (compression_table, i);
  
  while (TREE_CODE (type) == POINTER_TYPE
	 && TREE_CODE (t) == POINTER_TYPE)
    {
      t = TREE_TYPE (t);
      type = TREE_TYPE (type);
    }
  return (TREE_CODE (type) == RECORD_TYPE
	  && TREE_CODE (t) == RECORD_TYPE
	  && t == type);
}

/* Go through all qualification of type and build a list of list node
   elements containings as a purpose what should be used for a match and
   inserted in the compression table; and as it value the raw name of the
   part. The result is stored in TYPE_PACKAGE_LIST to be reused.  */

static void
set_type_package_list (tree type)
{
  int i;
  const char *type_string = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type)));
  const char *ptr;
  int qualifications;
  tree list = NULL_TREE, elt;

  for (ptr = type_string, qualifications = 0; *ptr; ptr++)
    if (*ptr == '.')
      qualifications += 1;

  for (ptr = type_string, i = 0; i < qualifications; ptr++)
    {
      if (ptr [0] == '.')
	{
	  tree const identifier
	    = get_identifier_with_length (type_string, ptr - type_string);

	  elt = build_tree_list (identifier, identifier);
	  TREE_CHAIN (elt) = list;
	  list = elt;
	  type_string = ptr+1;
	  i += 1;
	}
    }

  elt = build_tree_list (type, get_identifier (type_string));
  TREE_CHAIN (elt) = list;
  list = elt;
  TYPE_PACKAGE_LIST (type) = nreverse (list);
}

/* Add TYPE as the last element of the compression table. Resize the
   compression table if necessary.  */

static void
compression_table_add (tree type)
{
  if (compression_next == TREE_VEC_LENGTH (compression_table))
    {
      tree new_table = make_tree_vec (2*compression_next);
      int i;

      for (i = 0; i < compression_next; i++)
	TREE_VEC_ELT (new_table, i) = TREE_VEC_ELT (compression_table, i);

      compression_table = new_table;
    }
  TREE_VEC_ELT (compression_table, compression_next++) = type;
}

/* Mangle an embedded resource file name.  "_ZGr" is the prefix.  A
   '_' is prepended to the name so that names starting with a digit
   can be demangled.  The length and then the resulting name itself
   are appended while escaping '$', '.', and '/' to: "$$", "$_", and
   "$S".  */

tree
java_mangle_resource_name (const char *name)
{
  int len = strlen (name);
  char *buf = (char *) alloca (2 * len + 1);
  char *pos;
  const unsigned char *w1 = (const unsigned char *) name;
  const unsigned char *w2;
  const unsigned char *limit = w1 + len;

  pos = buf;

  init_mangling ();
  MANGLE_RAW_STRING ("Gr");

  *pos++ = '_';
  while (w1 < limit)
    {
      int ch;
      w2 = w1;
      ch = UTF8_GET (w1, limit);
      gcc_assert (ch > 0);
      switch (ch)
	{
	case '$':
	  *pos++ = '$';
	  *pos++ = '$';
	  break;
	case '.':
	  *pos++ = '$';
	  *pos++ = '_';
	  break;
	case '/':
	  *pos++ = '$';
	  *pos++ = 'S';
	  break;
	default:
	  memcpy (pos, w2, w1 - w2);
	  pos += w1 - w2;
	  break;
	}
    }
  append_gpp_mangled_name (buf, pos - buf);

  return finish_mangling ();
}

/* Mangling initialization routine.  */

static void
init_mangling (void)
{
  if (!mangle_obstack)
    {
      mangle_obstack = &mangle_obstack_1;
      gcc_obstack_init (mangle_obstack);
    }

  gcc_assert (compression_table == NULL);
  compression_table = make_tree_vec (10);

  /* Mangled name are to be suffixed */
  MANGLE_RAW_STRING ("_Z");
}

/* Mangling finalization routine. The mangled name is returned as a
   IDENTIFIER_NODE.  */

static tree
finish_mangling (void)
{
  tree result;

  gcc_assert (compression_table);

  compression_table = NULL_TREE;
  compression_next = 0;
  obstack_1grow (mangle_obstack, '\0');
  result = get_identifier (obstack_base (mangle_obstack));
  obstack_free (mangle_obstack, obstack_base (mangle_obstack));

  return result;
}

#include "gt-java-mangle.h"
