/* Handle the constant pool of the Java(TM) Virtual Machine.
   Copyright (C) 1997, 1998, 1999, 2000, 2001 Free Software Foundation, Inc.

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
Boston, MA 02111-1307, USA. 

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

#include "config.h"
#include "system.h"
#include "jcf.h"
#include "tree.h"
#include "java-tree.h"
#include "toplev.h"
#include "ggc.h"

static void set_constant_entry PARAMS ((CPool *, int, int, jword));
static int find_class_or_string_constant PARAMS ((CPool *, int, tree));
static int find_name_and_type_constant PARAMS ((CPool *, tree, tree));
static tree get_tag_node PARAMS ((int));
static tree build_constant_data_ref PARAMS ((void));

/* Set the INDEX'th constant in CPOOL to have the given TAG and VALUE. */

static void
set_constant_entry (cpool, index, tag, value)
     CPool *cpool;
     int index;
     int tag;
     jword value;
{
  if (cpool->data == NULL)
    {
      cpool->capacity = 100;
      cpool->tags = xmalloc (sizeof(uint8) * cpool->capacity);
      cpool->data = xmalloc (sizeof(jword) * cpool->capacity);
      cpool->count = 1;
    }
  if (index >= cpool->capacity)
    {
      cpool->capacity *= 2;
      if (index >= cpool->capacity)
	cpool->capacity = index + 10;
      cpool->tags = xrealloc (cpool->tags, sizeof(uint8) * cpool->capacity);
      cpool->data = xrealloc (cpool->data, sizeof(jword) * cpool->capacity);
    }
  if (index >= cpool->count)
    cpool->count = index + 1;
  cpool->tags[index] = tag;
  cpool->data[index] = value;
}

/* Find (or create) a constant pool entry matching TAG and VALUE. */

int
find_constant1 (cpool, tag, value)
     CPool *cpool;
     int tag;
     jword value;
{
  int i;
  for (i = cpool->count;  --i > 0; )
    {
      if (cpool->tags[i] == tag && cpool->data[i] == value)
	return i;
    }
  i = cpool->count == 0 ? 1 : cpool->count;
  set_constant_entry (cpool, i, tag, value);
  return i;
}

/* Find a double-word constant pool entry matching TAG and WORD1/WORD2. */

int
find_constant2 (cpool, tag, word1, word2)
     CPool *cpool;
     int tag;
     jword word1, word2;
{
  int i;
  for (i = cpool->count - 1;  --i > 0; )
    {
      if (cpool->tags[i] == tag
	  && cpool->data[i] == word1
	  && cpool->data[i+1] == word2)
	return i;
    }
  i = cpool->count == 0 ? 1 : cpool->count;
  set_constant_entry (cpool, i, tag, word1);
  set_constant_entry (cpool, i+1, 0, word2);
  return i;
}

int
find_utf8_constant (cpool, name)
     CPool *cpool;
     tree name;
{
  if (name == NULL_TREE)
    return 0;
  return find_constant1 (cpool, CONSTANT_Utf8, (jword) name);
}

static int
find_class_or_string_constant (cpool, tag, name)
     CPool *cpool;
     int tag;
     tree name;
{
  int j = find_utf8_constant (cpool, name);
  int i;
  for (i = cpool->count;  --i > 0; )
    {
      if (cpool->tags[i] == tag && cpool->data[i] == (jword) j)
	return i;
    }
  i = cpool->count;
  set_constant_entry (cpool, i, tag, (jword) j);
  return i;
}

int
find_class_constant (cpool, type)
     CPool *cpool;
     tree type;
{
  return find_class_or_string_constant (cpool, CONSTANT_Class,
					build_internal_class_name (type));
}

/* Allocate a CONSTANT_string entry given a STRING_CST. */

int
find_string_constant (cpool, string)
     CPool *cpool;
     tree string;
{
  string = get_identifier (TREE_STRING_POINTER (string));
  return find_class_or_string_constant (cpool, CONSTANT_String, string);

}

/* Find (or create) a CONSTANT_NameAndType matching NAME and TYPE.
   Return its index in the constant pool CPOOL. */

static int
find_name_and_type_constant (cpool, name, type)
     CPool *cpool;
     tree name;
     tree type;
{
  int name_index = find_utf8_constant (cpool, name);
  int type_index = find_utf8_constant (cpool, build_java_signature (type));
  return find_constant1 (cpool, CONSTANT_NameAndType,
			 (name_index << 16) | type_index);
}

/* Find (or create) a CONSTANT_Fieldref for DECL (a FIELD_DECL or VAR_DECL).
   Return its index in the constant pool CPOOL. */

int
find_fieldref_index (cpool, decl)
     CPool *cpool;
     tree decl;
{
  int class_index = find_class_constant (cpool, DECL_CONTEXT (decl));
  int name_type_index
    = find_name_and_type_constant (cpool, DECL_NAME (decl), TREE_TYPE (decl));
  return find_constant1 (cpool, CONSTANT_Fieldref,
			 (class_index << 16) | name_type_index);
}

/* Find (or create) a CONSTANT_Methodref for DECL (a FUNCTION_DECL).
   Return its index in the constant pool CPOOL. */

int
find_methodref_index (cpool, decl)
     CPool *cpool;
     tree decl;
{
  return find_methodref_with_class_index (cpool, decl, DECL_CONTEXT (decl));
}

int
find_methodref_with_class_index (cpool, decl, mclass)
     CPool *cpool;
     tree decl;
     tree mclass;
{
  int class_index = find_class_constant (cpool, mclass);
  tree name = DECL_CONSTRUCTOR_P (decl) ? init_identifier_node
    : DECL_NAME (decl);
  int name_type_index;
  name_type_index = 
      find_name_and_type_constant (cpool, name, TREE_TYPE (decl));
  return find_constant1 (cpool,
			 CLASS_INTERFACE (TYPE_NAME (mclass))
			 ? CONSTANT_InterfaceMethodref
			 : CONSTANT_Methodref,
			 (class_index << 16) | name_type_index);
}

#define PUT1(X)  (*ptr++ = (X))
#define PUT2(X)  (PUT1((X) >> 8), PUT1(X))
#define PUT4(X)  (PUT2((X) >> 16), PUT2(X))
#define PUTN(P, N)  (memcpy(ptr, (P), (N)), ptr += (N))

/* Give the number of bytes needed in a .class file for the CPOOL
   constant pool.  Includes the 2-byte constant_pool_count. */

int
count_constant_pool_bytes (cpool)
     CPool *cpool;
{
  int size = 2;
  int i = 1;
  for ( ;  i < cpool->count;  i++)
    {
      size++;
      switch (cpool->tags[i])
	{
	case CONSTANT_NameAndType:
	case CONSTANT_Fieldref:
	case CONSTANT_Methodref:
	case CONSTANT_InterfaceMethodref:
	case CONSTANT_Float:
	case CONSTANT_Integer:
	  size += 4;
	  break;
	case CONSTANT_Class:
	case CONSTANT_String:
	  size += 2;
	  break;
	case CONSTANT_Long:
	case CONSTANT_Double:
	  size += 8;
	  i++;
	  break;
	case CONSTANT_Utf8:
	  {
	    tree t = (tree) cpool->data[i];
	    int len = IDENTIFIER_LENGTH (t);
	    size += len + 2;
	  }
	  break;
	default:
	  /* Second word of CONSTANT_Long and  CONSTANT_Double. */
	  size--;
	}
    }
  return size;
}

/* Write the constant pool CPOOL into BUFFER.
   The length of BUFFER is LENGTH, which must match the needed length. */

void
write_constant_pool (cpool, buffer, length)
     CPool *cpool;
     unsigned char *buffer;
     int length;
{
  unsigned char *ptr = buffer;
  int i = 1;
  jword *datap = &cpool->data[1];
  PUT2 (cpool->count);
  for ( ;  i < cpool->count;  i++, datap++)
    {
      int tag = cpool->tags[i];
      PUT1 (tag);
      switch (tag)
	{
	case CONSTANT_NameAndType:
	case CONSTANT_Fieldref:
	case CONSTANT_Methodref:
	case CONSTANT_InterfaceMethodref:
	case CONSTANT_Float:
	case CONSTANT_Integer:
	  PUT4 (*datap);
	  break;
	case CONSTANT_Class:
	case CONSTANT_String:
	  PUT2 (*datap);
	  break;
	  break;
	case CONSTANT_Long:
	case CONSTANT_Double:
	  PUT4(*datap);
	  i++;
	  datap++;
	  PUT4 (*datap);
	  break;
	case CONSTANT_Utf8:
	  {
	    tree t = (tree) *datap;
	    int len = IDENTIFIER_LENGTH (t);
	    PUT2 (len);
	    PUTN (IDENTIFIER_POINTER (t), len);
	  }
	  break;
	}
    }

  if (ptr != buffer + length)
    abort ();
}

CPool *outgoing_cpool;

static GTY(()) tree tag_nodes[13];
static tree
get_tag_node (tag)
     int tag;
{
  /* A Cache for build_int_2 (CONSTANT_XXX, 0). */

  if (tag_nodes[tag] == NULL_TREE)
    tag_nodes[tag] = build_int_2 (tag, 0);
  return tag_nodes[tag];
}

/* Look for a constant pool entry that matches TAG and NAME.
   Creates a new entry if not found.
   TAG is one of CONSTANT_Utf8, CONSTANT_String or CONSTANT_Class.
   NAME is an IDENTIFIER_NODE naming the Utf8 constant, string, or class.
   Returns the index of the entry. */

int
alloc_name_constant (tag, name)
     int tag;
     tree name;
{
  return find_constant1 (outgoing_cpool, tag, (jword) name);
}

/* Build an identifier for the internal name of reference type TYPE. */

tree
build_internal_class_name (type)
     tree type;
{
  tree name;
  if (TYPE_ARRAY_P (type))
    name = build_java_signature (type);
  else
    {
      name = TYPE_NAME (type);
      if (TREE_CODE (name) != IDENTIFIER_NODE)
	name = DECL_NAME (name);
      name = identifier_subst (name, "", '.', '/', "");
    }
  return name;
}

/* Look for a CONSTANT_Class entry for CLAS, creating a new one if needed. */

int
alloc_class_constant (clas)
     tree clas;
{
  tree class_name = build_internal_class_name (clas);
  
  return alloc_name_constant (CONSTANT_Class,
			      (unmangle_classname 
			       (IDENTIFIER_POINTER(class_name),
				IDENTIFIER_LENGTH(class_name))));
}

/* Return a reference to the data array of the current constant pool. */

static tree
build_constant_data_ref ()
{
  if (TYPE_CPOOL_DATA_REF (current_class))
    current_constant_pool_data_ref = TYPE_CPOOL_DATA_REF (current_class);

  else if (current_constant_pool_data_ref == NULL_TREE)
    {
      tree decl;
      tree decl_name = mangled_classname ("_CD_", current_class);
      decl = build_decl (VAR_DECL, decl_name,
			 build_array_type (ptr_type_node,
					   one_elt_array_domain_type));
      TREE_STATIC (decl) = 1;
      make_decl_rtl (decl, NULL);
      TYPE_CPOOL_DATA_REF (current_class) = current_constant_pool_data_ref
	= build1 (ADDR_EXPR, ptr_type_node, decl);
    }
  return current_constant_pool_data_ref;
}

/* Get the pointer value at the INDEX'th element of the constant pool. */

tree
build_ref_from_constant_pool (index)
     int index;
{
  tree t = build_constant_data_ref ();
  index *= int_size_in_bytes (ptr_type_node);
  t = fold (build (PLUS_EXPR, ptr_type_node,
                              t, build_int_2 (index, 0)));
  return build1 (INDIRECT_REF, ptr_type_node, t);
}

/* Build an initializer for the constants field of the current constal pool.
   Should only be called at top-level, since it may emit declarations. */

tree
build_constants_constructor ()
{
  tree tags_value, data_value;
  tree cons;
  tree tags_list = NULL_TREE;
  tree data_list = NULL_TREE;
  int i;
  for (i = outgoing_cpool->count;  --i > 0; )
    {
      tags_list
	= tree_cons (NULL_TREE, get_tag_node (outgoing_cpool->tags[i]),
		     tags_list);
      data_list
	= tree_cons (NULL_TREE, build_utf8_ref ((tree)outgoing_cpool->data[i]),
		     data_list);
    }
  if (outgoing_cpool->count > 0)
    {
      tree index_type;
      tree data_decl, tags_decl, tags_type;
      tree max_index = build_int_2 (outgoing_cpool->count - 1, 0);
      TREE_TYPE (max_index) = sizetype;
      index_type = build_index_type (max_index);

      /* Add dummy 0'th element of constant pool. */
      tags_list = tree_cons (NULL_TREE, get_tag_node (0), tags_list);
      data_list = tree_cons (NULL_TREE, null_pointer_node, data_list);
  
      data_decl = TREE_OPERAND (build_constant_data_ref (), 0);
      TREE_TYPE (data_decl) = build_array_type (ptr_type_node, index_type), 
      DECL_INITIAL (data_decl) = build (CONSTRUCTOR, TREE_TYPE (data_decl),
					NULL_TREE, data_list);
      DECL_SIZE (data_decl) = TYPE_SIZE (TREE_TYPE (data_decl));
      DECL_SIZE_UNIT (data_decl) = TYPE_SIZE_UNIT (TREE_TYPE (data_decl));
      rest_of_decl_compilation (data_decl, (char *) 0, 1, 0);
      data_value = build_address_of (data_decl);

      tags_type = build_array_type (unsigned_byte_type_node, index_type);
      tags_decl = build_decl (VAR_DECL, mangled_classname ("_CT_", 
							   current_class),
			      tags_type);
      TREE_STATIC (tags_decl) = 1;
      DECL_INITIAL (tags_decl) = build (CONSTRUCTOR, tags_type,
					NULL_TREE, tags_list);
      rest_of_decl_compilation (tags_decl, (char*) 0, 1, 0);
      tags_value = build_address_of (tags_decl);
    }
  else
    {
      data_value = null_pointer_node;
      tags_value = null_pointer_node;
    }
  START_RECORD_CONSTRUCTOR (cons, constants_type_node);
  PUSH_FIELD_VALUE (cons, "size", build_int_2 (outgoing_cpool->count, 0));
  PUSH_FIELD_VALUE (cons, "tags", tags_value);
  PUSH_FIELD_VALUE (cons, "data", data_value);
  FINISH_RECORD_CONSTRUCTOR (cons);
  return cons;
}

#include "gt-java-constants.h"
