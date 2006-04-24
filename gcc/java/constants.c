/* Handle the constant pool of the Java(TM) Virtual Machine.
   Copyright (C) 1997, 1998, 1999, 2000, 2001, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA. 

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "jcf.h"
#include "tree.h"
#include "java-tree.h"
#include "toplev.h"
#include "ggc.h"

static void set_constant_entry (CPool *, int, int, jword);
static int find_tree_constant (CPool *, int, tree);
static int find_class_or_string_constant (CPool *, int, tree);
static int find_name_and_type_constant (CPool *, tree, tree);
static tree get_tag_node (int);
static tree build_constant_data_ref (void);
static CPool *cpool_for_class (tree);

/* Set the INDEX'th constant in CPOOL to have the given TAG and VALUE. */

static void
set_constant_entry (CPool *cpool, int index, int tag, jword value)
{
  if (cpool->data == NULL)
    {
      cpool->capacity = 100;
      cpool->tags = ggc_alloc_cleared (sizeof(uint8) * cpool->capacity);
      cpool->data = ggc_alloc_cleared (sizeof(union cpool_entry)
				       * cpool->capacity);
      cpool->count = 1;
    }
  if (index >= cpool->capacity)
    {
      int old_cap = cpool->capacity;
      cpool->capacity *= 2;
      if (index >= cpool->capacity)
	cpool->capacity = index + 10;
      cpool->tags = ggc_realloc (cpool->tags, 
				 sizeof(uint8) * cpool->capacity);
      cpool->data = ggc_realloc (cpool->data,
				 sizeof(union cpool_entry) * cpool->capacity);

      /* Make sure GC never sees uninitialized tag values.  */
      memset (cpool->tags + old_cap, 0, cpool->capacity - old_cap);
      memset (cpool->data + old_cap, 0,
	      (cpool->capacity - old_cap) * sizeof (union cpool_entry));
    }
  if (index >= cpool->count)
    cpool->count = index + 1;
  cpool->tags[index] = tag;
  cpool->data[index].w = value;
}

/* Find (or create) a constant pool entry matching TAG and VALUE. */

int
find_constant1 (CPool *cpool, int tag, jword value)
{
  int i;
  for (i = cpool->count;  --i > 0; )
    {
      if (cpool->tags[i] == tag && cpool->data[i].w == value)
	return i;
    }
  i = cpool->count == 0 ? 1 : cpool->count;
  set_constant_entry (cpool, i, tag, value);
  return i;
}

/* Find a double-word constant pool entry matching TAG and WORD1/WORD2. */

int
find_constant2 (CPool *cpool, int tag, jword word1, jword word2)
{
  int i;
  for (i = cpool->count - 1;  --i > 0; )
    {
      if (cpool->tags[i] == tag
	  && cpool->data[i].w == word1
	  && cpool->data[i+1].w == word2)
	return i;
    }
  i = cpool->count == 0 ? 1 : cpool->count;
  set_constant_entry (cpool, i, tag, word1);
  set_constant_entry (cpool, i+1, 0, word2);
  return i;
}

static int
find_tree_constant (CPool *cpool, int tag, tree value)
{
  int i;
  for (i = cpool->count;  --i > 0; )
    {
      if (cpool->tags[i] == tag && cpool->data[i].t == value)
	return i;
    }
  i = cpool->count == 0 ? 1 : cpool->count;
  set_constant_entry (cpool, i, tag, 0);
  cpool->data[i].t = value;
  return i;
}


int
find_utf8_constant (CPool *cpool, tree name)
{
  if (name == NULL_TREE)
    return 0;
  return find_tree_constant (cpool, CONSTANT_Utf8, name);
}

static int
find_class_or_string_constant (CPool *cpool, int tag, tree name)
{
  jword j = find_utf8_constant (cpool, name);
  int i;
  for (i = cpool->count;  --i > 0; )
    {
      if (cpool->tags[i] == tag && cpool->data[i].w == j)
	return i;
    }
  i = cpool->count;
  set_constant_entry (cpool, i, tag, j);
  return i;
}

int
find_class_constant (CPool *cpool, tree type)
{
  return find_class_or_string_constant (cpool, CONSTANT_Class,
					build_internal_class_name (type));
}

/* Allocate a CONSTANT_string entry given a STRING_CST. */

int
find_string_constant (CPool *cpool, tree string)
{
  string = get_identifier (TREE_STRING_POINTER (string));
  return find_class_or_string_constant (cpool, CONSTANT_String, string);

}

/* Find (or create) a CONSTANT_NameAndType matching NAME and TYPE.
   Return its index in the constant pool CPOOL. */

static int
find_name_and_type_constant (CPool *cpool, tree name, tree type)
{
  int name_index = find_utf8_constant (cpool, name);
  int type_index = find_utf8_constant (cpool, build_java_signature (type));
  return find_constant1 (cpool, CONSTANT_NameAndType,
			 (name_index << 16) | type_index);
}

/* Find (or create) a CONSTANT_Fieldref for DECL (a FIELD_DECL or VAR_DECL).
   Return its index in the constant pool CPOOL. */

int
find_fieldref_index (CPool *cpool, tree decl)
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
find_methodref_index (CPool *cpool, tree decl)
{
  return find_methodref_with_class_index (cpool, decl, DECL_CONTEXT (decl));
}

int
find_methodref_with_class_index (CPool *cpool, tree decl, tree mclass)
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
count_constant_pool_bytes (CPool *cpool)
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
	    tree t = cpool->data[i].t;
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
write_constant_pool (CPool *cpool, unsigned char *buffer, int length)
{
  unsigned char *ptr = buffer;
  int i = 1;
  union cpool_entry *datap = &cpool->data[1];
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
	  PUT4 (datap->w);
	  break;
	case CONSTANT_Class:
	case CONSTANT_String:
	  PUT2 (datap->w);
	  break;
	  break;
	case CONSTANT_Long:
	case CONSTANT_Double:
	  PUT4(datap->w);
	  i++;
	  datap++;
	  PUT4 (datap->w);
	  break;
	case CONSTANT_Utf8:
	  {
	    tree t = datap->t;
	    int len = IDENTIFIER_LENGTH (t);
	    PUT2 (len);
	    PUTN (IDENTIFIER_POINTER (t), len);
	  }
	  break;
	}
    }

  gcc_assert (ptr == buffer + length);
}

static GTY(()) tree tag_nodes[13];
static tree
get_tag_node (int tag)
{
  /* A Cache for build_int_cst (CONSTANT_XXX, 0). */

  if (tag_nodes[tag] == NULL_TREE)
    tag_nodes[tag] = build_int_cst (NULL_TREE, tag);
  return tag_nodes[tag];
}

/* Given a class, return its constant pool, creating one if necessary.  */

static CPool *
cpool_for_class (tree class)
{
  CPool *cpool = TYPE_CPOOL (class);

  if (cpool == NULL)
    {
      cpool = ggc_alloc_cleared (sizeof (struct CPool));
      TYPE_CPOOL (class) = cpool;
    }
  return cpool;
}

/* Look for a constant pool entry that matches TAG and NAME.
   Creates a new entry if not found.
   TAG is one of CONSTANT_Utf8, CONSTANT_String or CONSTANT_Class.
   NAME is an IDENTIFIER_NODE naming the Utf8 constant, string, or class.
   Returns the index of the entry. */

int
alloc_name_constant (int tag, tree name)
{
  CPool *outgoing_cpool = cpool_for_class (output_class);
  return find_tree_constant (outgoing_cpool, tag, name);
}

/* Create a constant pool entry for a name_and_type.  This one has '.'
   rather than '/' because it isn't going into a class file, it's
   going into a compiled object.  We don't use the '/' separator in
   compiled objects.  */

static int
find_name_and_type_constant_tree (CPool *cpool, tree name, tree type)
{
  int name_index = find_utf8_constant (cpool, name);
  int type_index 
    = find_utf8_constant (cpool, 
			  identifier_subst (build_java_signature (type), 
					    "", '/', '.', ""));
  return find_constant1 (cpool, CONSTANT_NameAndType,
			 (name_index << 16) | type_index);
}

/* Look for a field ref that matches DECL in the constant pool of
   CLASS.  
   Return the index of the entry.  */

int
alloc_constant_fieldref (tree class, tree decl)
{
  CPool *outgoing_cpool = cpool_for_class (class);
  int class_index 
    = find_tree_constant (outgoing_cpool, CONSTANT_Class, 
			  DECL_NAME (TYPE_NAME (DECL_CONTEXT (decl))));
  int name_type_index
    = find_name_and_type_constant_tree (outgoing_cpool, DECL_NAME (decl), 
					TREE_TYPE (decl));
  return find_constant1 (outgoing_cpool, CONSTANT_Fieldref,
			 (class_index << 16) | name_type_index);
}

/* Build an identifier for the internal name of reference type TYPE. */

tree
build_internal_class_name (tree type)
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
alloc_class_constant (tree clas)
{
  tree class_name = build_internal_class_name (clas);
  
  return alloc_name_constant (CONSTANT_Class,
			      (unmangle_classname 
			       (IDENTIFIER_POINTER(class_name),
				IDENTIFIER_LENGTH(class_name))));
}

/* Return the decl of the data array of the current constant pool. */

static tree
build_constant_data_ref (void)
{
  tree decl = TYPE_CPOOL_DATA_REF (output_class);

  if (decl == NULL_TREE)
    {
      tree type;
      tree decl_name = mangled_classname ("_CD_", output_class);

      /* Build a type with unspecified bounds.  The will make sure
	 that targets do the right thing with whatever size we end
	 up with at the end.  Using bounds that are too small risks
	 assuming the data is in the small data section.  */
      type = build_array_type (ptr_type_node, NULL_TREE);

      /* We need to lay out the type ourselves, since build_array_type
	 thinks the type is incomplete.  */
      layout_type (type);

      decl = build_decl (VAR_DECL, decl_name, type);
      TREE_STATIC (decl) = 1;
      TYPE_CPOOL_DATA_REF (output_class) = decl;
    }

  return decl;
}

/* Get the pointer value at the INDEX'th element of the constant pool. */

tree
build_ref_from_constant_pool (int index)
{
  tree d = build_constant_data_ref ();
  tree i = build_int_cst (NULL_TREE, index);
  if (flag_indirect_classes)
    {
      tree decl = build_class_ref (output_class);
      tree klass = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (decl)),
			   decl);
      tree constants = build3 (COMPONENT_REF, 
			       TREE_TYPE (constants_field_decl_node), klass,
			       constants_field_decl_node,
			       NULL_TREE);
      tree data = build3 (COMPONENT_REF, 
			  TREE_TYPE (constants_data_field_decl_node), 
			  constants,
			  constants_data_field_decl_node,
			  NULL_TREE);
      data = fold_convert (build_pointer_type (TREE_TYPE (d)), data);
      d = build1 (INDIRECT_REF, TREE_TYPE (d), data);
      /* FIXME: These should be cached.  */
      TREE_INVARIANT (d) = 1;
    }
  d = build4 (ARRAY_REF, TREE_TYPE (TREE_TYPE (d)), d, i,
		 NULL_TREE, NULL_TREE);
  TREE_INVARIANT (d) = 1;
  return d;
}

/* Build an initializer for the constants field of the current constant pool.
   Should only be called at top-level, since it may emit declarations. */

tree
build_constants_constructor (void)
{
  CPool *outgoing_cpool = cpool_for_class (current_class);
  tree tags_value, data_value;
  tree cons;
  tree tags_list = NULL_TREE;
  tree data_list = NULL_TREE;
  int i;
  for (i = outgoing_cpool->count;  --i > 0; )
    switch (outgoing_cpool->tags[i])
      {
      case CONSTANT_Fieldref:
      case CONSTANT_NameAndType:
	{
	  unsigned HOST_WIDE_INT temp = outgoing_cpool->data[i].w;

	  /* Make sure that on a 64-bit big-endian machine this
	     32-bit jint appears in the first word.  
	     FIXME: This is a kludge.  The field we're initializing is
	     not a scalar but a union, and that's how we should
	     represent it in the compiler.  We should fix this.  */
	  if (BYTES_BIG_ENDIAN && BITS_PER_WORD > 32)
	    temp <<= BITS_PER_WORD - 32;

	  tags_list
	    = tree_cons (NULL_TREE, 
			 build_int_cst (NULL_TREE, outgoing_cpool->tags[i]),
			 tags_list);
	  data_list
	    = tree_cons (NULL_TREE, 
			 fold_convert (ptr_type_node, 
				       (build_int_cst (NULL_TREE, temp))),
			 data_list);
	}
	break;
      default:
	tags_list
	  = tree_cons (NULL_TREE, get_tag_node (outgoing_cpool->tags[i]),
		       tags_list);
	data_list
	  = tree_cons (NULL_TREE, build_utf8_ref (outgoing_cpool->data[i].t),
		       data_list);
	break;
      }
  if (outgoing_cpool->count > 0)
    {
      tree data_decl, tags_decl, tags_type;
      tree max_index = build_int_cst (sizetype, outgoing_cpool->count - 1);
      tree index_type = build_index_type (max_index);

      /* Add dummy 0'th element of constant pool. */
      tags_list = tree_cons (NULL_TREE, get_tag_node (0), tags_list);
      data_list = tree_cons (NULL_TREE, null_pointer_node, data_list);
  
      data_decl = build_constant_data_ref ();
      TREE_TYPE (data_decl) = build_array_type (ptr_type_node, index_type);
      DECL_INITIAL (data_decl) = build_constructor_from_list
				  (TREE_TYPE (data_decl), data_list);
      DECL_SIZE (data_decl) = TYPE_SIZE (TREE_TYPE (data_decl));
      DECL_SIZE_UNIT (data_decl) = TYPE_SIZE_UNIT (TREE_TYPE (data_decl));
      rest_of_decl_compilation (data_decl, 1, 0);
      data_value = build_address_of (data_decl);

      tags_type = build_array_type (unsigned_byte_type_node, index_type);
      tags_decl = build_decl (VAR_DECL, mangled_classname ("_CT_", 
							   current_class),
			      tags_type);
      TREE_STATIC (tags_decl) = 1;
      DECL_INITIAL (tags_decl) = build_constructor_from_list
				 (tags_type, tags_list);
      rest_of_decl_compilation (tags_decl, 1, 0);
      tags_value = build_address_of (tags_decl);
    }
  else
    {
      data_value = null_pointer_node;
      tags_value = null_pointer_node;
    }
  START_RECORD_CONSTRUCTOR (cons, constants_type_node);
  PUSH_FIELD_VALUE (cons, "size",
		    build_int_cst (NULL_TREE, outgoing_cpool->count));
  PUSH_FIELD_VALUE (cons, "tags", tags_value);
  PUSH_FIELD_VALUE (cons, "data", data_value);
  FINISH_RECORD_CONSTRUCTOR (cons);
  return cons;
}

#include "gt-java-constants.h"
