/* This file read a Java(TM) .class file.
   It is not stand-alone:  It depends on tons of macros, and the
   intent is you #include this file after you've defined the macros.
   Copyright (C) 1996-2014 Free Software Foundation, Inc.

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

#include "ggc.h"
#include "jcf.h"
#include "zipfile.h"

static int get_attribute (JCF *, int, jv_attr_type);
static int jcf_parse_preamble (JCF *);
static int jcf_parse_constant_pool (JCF *);
static void jcf_parse_class (JCF *);
static int jcf_parse_fields (JCF *);
static int jcf_parse_one_method (JCF *, int);
static int jcf_parse_methods (JCF *);
static int jcf_parse_final_attributes (JCF *);
static int jcf_parse_bootstrap_methods (JCF *, int) ATTRIBUTE_UNUSED;
#ifdef NEED_PEEK_ATTRIBUTE
static int peek_attribute (JCF *, int, const char *, int);
#endif
#ifdef NEED_SKIP_ATTRIBUTE
static void skip_attribute (JCF *, int);
#endif

/* Go through all available attribute (ATTRIBUTE_NUMER) and try to
   identify PEEKED_NAME.  Return 1 if PEEKED_NAME was found, 0
   otherwise. JCF is restored to its initial position before
   returning.  */

#ifdef NEED_PEEK_ATTRIBUTE	/* Not everyone uses this function */
static int
peek_attribute (JCF *jcf, int attribute_number, const char *peeked_name,
		int peeked_name_length)
{
  int to_return = 0;
  long absolute_offset = (long)JCF_TELL (jcf);
  int i;

  for (i = 0; !to_return && i < attribute_number; i++)
    {
      uint16 attribute_name = (JCF_FILL (jcf, 6), JCF_readu2 (jcf));
      uint32 attribute_length = JCF_readu4 (jcf);
      int name_length;
      const unsigned char *name_data; 

      JCF_FILL (jcf, (long) attribute_length);
      if (attribute_name <= 0 || attribute_name >= JPOOL_SIZE(jcf)
	  || JPOOL_TAG (jcf, attribute_name) != CONSTANT_Utf8)
	continue;

      name_length = JPOOL_UTF_LENGTH (jcf, attribute_name);
      name_data = JPOOL_UTF_DATA (jcf, attribute_name);

      if (name_length == peeked_name_length 
	  && ! memcmp (name_data, peeked_name, peeked_name_length)) 
	{
	  to_return = 1; 
	  break;
	}
      
      JCF_SKIP (jcf, attribute_length);
    }

  JCF_SEEK (jcf, absolute_offset);
  return to_return;
}
#endif

#ifdef NEED_SKIP_ATTRIBUTE	/* Not everyone uses this function */
static void
skip_attribute (JCF *jcf, int number_of_attribute)
{
  while (number_of_attribute--)
    {
      JCF_u4 N;
      JCF_FILL (jcf, 6);
      (void) JCF_readu2 (jcf);
      N = JCF_readu4 (jcf);
      JCF_SKIP (jcf, N);
    }
}
#endif

static int
get_attribute (JCF *jcf, int index, 
	       jv_attr_type attr_type ATTRIBUTE_UNUSED)
{
  uint16 attribute_name = (JCF_FILL (jcf, 6), JCF_readu2 (jcf));
  uint32 attribute_length = JCF_readu4 (jcf);
  uint32 start_pos = JCF_TELL(jcf);
  int name_length;
  const unsigned char *name_data;
  JCF_FILL (jcf, (long) attribute_length);
  if (attribute_name <= 0 || attribute_name >= JPOOL_SIZE(jcf))
    return -2;
  if (JPOOL_TAG (jcf, attribute_name) != CONSTANT_Utf8)
    return -2;
  name_length = JPOOL_UTF_LENGTH (jcf, attribute_name);
  name_data = JPOOL_UTF_DATA (jcf, attribute_name);

#define MATCH_ATTRIBUTE(S) \
  (name_length == sizeof (S)-1 && memcmp (name_data, S, sizeof (S)-1) == 0)

#ifdef IGNORE_ATTRIBUTE
   if (IGNORE_ATTRIBUTE (jcf, attribute_name, attribute_length))
     {
       JCF_SKIP (jcf, attribute_length);
     }
   else
#endif
#ifdef HANDLE_SOURCEFILE
  if (MATCH_ATTRIBUTE ("SourceFile"))
    {
      uint16 sourcefile_index = JCF_readu2 (jcf);
      HANDLE_SOURCEFILE(sourcefile_index);
    }
  else
#endif
#ifdef HANDLE_CONSTANTVALUE
  if (MATCH_ATTRIBUTE ("ConstantValue"))
    {
      uint16 constantvalue_index = JCF_readu2 (jcf);
      if (constantvalue_index <= 0 || constantvalue_index >= JPOOL_SIZE(jcf))
	return -2;
      HANDLE_CONSTANTVALUE(constantvalue_index);
    }
  else
#endif
#ifdef HANDLE_CODE_ATTRIBUTE
  if (MATCH_ATTRIBUTE ("Code"))
    {
      uint16 j;
      uint16 max_stack ATTRIBUTE_UNUSED = JCF_readu2 (jcf);
      uint16 max_locals ATTRIBUTE_UNUSED = JCF_readu2 (jcf);
      uint32 code_length = JCF_readu4 (jcf);
      uint16 exception_table_length, attributes_count;
      if (code_length + 12 > attribute_length)
	return -1;
      HANDLE_CODE_ATTRIBUTE(max_stack, max_locals, code_length);
      JCF_SKIP (jcf, code_length);
      exception_table_length = JCF_readu2 (jcf);
      if (code_length + 8 * exception_table_length + 12 > attribute_length)
	return -1;
#ifdef HANDLE_EXCEPTION_TABLE
      HANDLE_EXCEPTION_TABLE (jcf->read_ptr, exception_table_length);
#endif
      JCF_SKIP (jcf, 2 * 4 * exception_table_length);
      attributes_count = JCF_readu2 (jcf);
      for (j = 0; j < attributes_count; j++)
	{
	  int code = get_attribute (jcf, index, JV_METHOD_ATTR);
	  if (code != 0)
	    return code;
	}
    }
  else
#endif /* HANDLE_CODE_ATTRIBUTE */
#ifdef HANDLE_EXCEPTIONS_ATTRIBUTE
  if (MATCH_ATTRIBUTE ("Exceptions"))
    {
      uint16 count = JCF_readu2 (jcf);
      HANDLE_EXCEPTIONS_ATTRIBUTE (count);
    }
  else
#endif
#ifdef HANDLE_LINENUMBERTABLE_ATTRIBUTE
  if (MATCH_ATTRIBUTE ("LineNumberTable"))
    {
      uint16 count = JCF_readu2 (jcf);
      HANDLE_LINENUMBERTABLE_ATTRIBUTE (count);
    }
  else
#endif
#ifdef HANDLE_LOCALVARIABLETABLE_ATTRIBUTE
  if (MATCH_ATTRIBUTE ("LocalVariableTable"))
    {
      uint16 count = JCF_readu2 (jcf);
      HANDLE_LOCALVARIABLETABLE_ATTRIBUTE (count);
    }
  else
#endif
#ifdef HANDLE_LOCALVARIABLETYPETABLE_ATTRIBUTE
  if (MATCH_ATTRIBUTE ("LocalVariableTypeTable"))
    {
      uint16 count = JCF_readu2 (jcf);
      HANDLE_LOCALVARIABLETYPETABLE_ATTRIBUTE (count);
    }
  else
#endif
#ifdef HANDLE_INNERCLASSES_ATTRIBUTE
  if (MATCH_ATTRIBUTE ("InnerClasses"))
    {
      uint16 count = JCF_readu2 (jcf);
      HANDLE_INNERCLASSES_ATTRIBUTE (count);
    }
  else
#endif
#ifdef HANDLE_SYNTHETIC_ATTRIBUTE
  if (MATCH_ATTRIBUTE ("Synthetic"))
    {
      HANDLE_SYNTHETIC_ATTRIBUTE ();
    }
  else
#endif
#ifdef HANDLE_GCJCOMPILED_ATTRIBUTE
  if (MATCH_ATTRIBUTE ("gnu.gcj.gcj-compiled"))
    {
      HANDLE_GCJCOMPILED_ATTRIBUTE ();
    }
  else
#endif
#ifdef HANDLE_DEPRECATED_ATTRIBUTE
  if (MATCH_ATTRIBUTE ("Deprecated"))
    {
      HANDLE_DEPRECATED_ATTRIBUTE ();
    }
  else
#endif
#ifdef HANDLE_SOURCEDEBUGEXTENSION_ATTRIBUTE
  if (MATCH_ATTRIBUTE ("SourceDebugExtension")) /* JSR 45 */
    {
      HANDLE_SOURCEDEBUGEXTENSION_ATTRIBUTE (attribute_length);
    }
  else
#endif
#ifdef HANDLE_ENCLOSINGMETHOD_ATTRIBUTE
  if (MATCH_ATTRIBUTE ("EnclosingMethod"))
    {
      HANDLE_ENCLOSINGMETHOD_ATTRIBUTE ();
    }
  else
#endif
#ifdef HANDLE_SIGNATURE_ATTRIBUTE
  if (MATCH_ATTRIBUTE ("Signature"))
    {
      HANDLE_SIGNATURE_ATTRIBUTE ();
    }
  else
#endif
#ifdef HANDLE_RUNTIMEVISIBLEANNOTATIONS_ATTRIBUTE
  if (MATCH_ATTRIBUTE ("RuntimeVisibleAnnotations"))
    {
      HANDLE_RUNTIMEVISIBLEANNOTATIONS_ATTRIBUTE ();
    }
  else
#endif
#ifdef HANDLE_RUNTIMEINVISIBLEANNOTATIONS_ATTRIBUTE
  if (MATCH_ATTRIBUTE ("RuntimeInvisibleAnnotations"))
    {
      HANDLE_RUNTIMEINVISIBLEANNOTATIONS_ATTRIBUTE ();
    }
  else
#endif
#ifdef HANDLE_RUNTIMEVISIBLEPARAMETERANNOTATIONS_ATTRIBUTE
  if (MATCH_ATTRIBUTE ("RuntimeVisibleParameterAnnotations"))
    {
      HANDLE_RUNTIMEVISIBLEPARAMETERANNOTATIONS_ATTRIBUTE ();
    }
  else
#endif
#ifdef HANDLE_RUNTIMEINVISIBLEPARAMETERANNOTATIONS_ATTRIBUTE
  if (MATCH_ATTRIBUTE ("RuntimeInvisibleParameterAnnotations"))
    {
      HANDLE_RUNTIMEINVISIBLEPARAMETERANNOTATIONS_ATTRIBUTE ();
    }
  else
#endif
#ifdef HANDLE_ANNOTATIONDEFAULT_ATTRIBUTE
  if (MATCH_ATTRIBUTE ("AnnotationDefault"))
    {
      HANDLE_ANNOTATIONDEFAULT_ATTRIBUTE ();
    }
  else
#endif
  if (MATCH_ATTRIBUTE ("BootstrapMethods"))
    {
#ifdef HANDLE_BOOTSTRAP_METHODS_ATTRIBUTE
      HANDLE_BOOTSTRAP_METHODS_ATTRIBUTE();
#else
      JCF_SKIP (jcf, attribute_length);
#endif
    }
   else
    {
#ifdef PROCESS_OTHER_ATTRIBUTE
      PROCESS_OTHER_ATTRIBUTE(jcf, attribute_name, attribute_length);
#else
      JCF_SKIP (jcf, attribute_length);
#endif
    }
  if ((long) (start_pos + attribute_length) != JCF_TELL(jcf))
    return -1;
  return 0;
}

/* Read and handle the pre-amble. */
static int
jcf_parse_preamble (JCF* jcf)
{
  uint32 magic = (JCF_FILL (jcf, 8), JCF_readu4 (jcf));
  uint16 minor_version ATTRIBUTE_UNUSED = JCF_readu2 (jcf);
  uint16 major_version ATTRIBUTE_UNUSED = JCF_readu2 (jcf);
#ifdef HANDLE_MAGIC
  HANDLE_MAGIC (magic, minor_version, major_version);
#endif
  if (magic != 0xcafebabe)
    return -1;
  else
    return 0;
}

/* Read and handle the constant pool.

   Return 0 if OK.
   Return -2 if a bad cross-reference (index of other constant) was seen.
*/
static int
jcf_parse_constant_pool (JCF* jcf)
{
  int i, n;
  JPOOL_SIZE (jcf) = (JCF_FILL (jcf, 2), JCF_readu2 (jcf));
  jcf->cpool.tags = (uint8 *) ggc_alloc_atomic (JPOOL_SIZE (jcf));
  jcf->cpool.data = ggc_alloc_cpool_entry (sizeof (jword) * JPOOL_SIZE (jcf));
  jcf->cpool.tags[0] = 0;
#ifdef HANDLE_START_CONSTANT_POOL
  HANDLE_START_CONSTANT_POOL (JPOOL_SIZE (jcf));
#endif
  for (i = 1; i < (int) JPOOL_SIZE (jcf); i++)
    {
      int constant_kind;
       
      /* Make sure at least 9 bytes are available.  This is enough
	 for all fixed-sized constant pool entries (so we don't need many
	 more JCF_FILL calls below), but is is small enough that
	 we are guaranteed to not hit EOF (in a valid .class file). */
      JCF_FILL (jcf, 9);
      constant_kind = JCF_readu (jcf);
      jcf->cpool.tags[i] = constant_kind;
      switch (constant_kind)
	{
	case CONSTANT_String:
	case CONSTANT_Class:
	  jcf->cpool.data[i].w = JCF_readu2 (jcf);
	  break;
	case CONSTANT_Fieldref:
	case CONSTANT_Methodref:
	case CONSTANT_InterfaceMethodref:
	case CONSTANT_NameAndType:
	  jcf->cpool.data[i].w = JCF_readu2 (jcf);
	  jcf->cpool.data[i].w |= JCF_readu2 (jcf) << 16;
	  break;
	case CONSTANT_Integer:
	case CONSTANT_Float:
	  jcf->cpool.data[i].w = JCF_readu4 (jcf);
	  break;
	case CONSTANT_Long:
	case CONSTANT_Double:
	  jcf->cpool.data[i].w = JCF_readu4 (jcf);
	  i++; /* These take up two spots in the constant pool */
	  jcf->cpool.tags[i] = 0;
	  jcf->cpool.data[i].w = JCF_readu4 (jcf);
	  break;
	case CONSTANT_Utf8:
	  n = JCF_readu2 (jcf);
	  JCF_FILL (jcf, n);
#ifdef HANDLE_CONSTANT_Utf8
	  HANDLE_CONSTANT_Utf8(jcf, i, n);
#else
	  jcf->cpool.data[i].w = JCF_TELL(jcf) - 2;
	  JCF_SKIP (jcf, n);
#endif
	  break;
	case CONSTANT_MethodHandle:
	  jcf->cpool.data[i].w = JCF_readu (jcf);
	  jcf->cpool.data[i].w |= JCF_readu2 (jcf) << 16;
	  break;
	case CONSTANT_MethodType:
	  jcf->cpool.data[i].w = JCF_readu2 (jcf);
	  break;
	case CONSTANT_InvokeDynamic:
	  jcf->cpool.data[i].w = JCF_readu2 (jcf);
	  jcf->cpool.data[i].w |= JCF_readu2 (jcf) << 16;
	  break;
	default:
	  return i;
	}
    }
  return 0;
}

/* Read various class flags and numbers. */

static void
jcf_parse_class (JCF* jcf)
{
  int i;
  uint16 interfaces_count;
  JCF_FILL (jcf, 8);
  jcf->access_flags = JCF_readu2 (jcf);
  jcf->this_class = JCF_readu2 (jcf);
  jcf->super_class = JCF_readu2 (jcf);
  interfaces_count = JCF_readu2 (jcf);

#ifdef HANDLE_CLASS_INFO
  HANDLE_CLASS_INFO(jcf->access_flags, jcf->this_class, jcf->super_class, interfaces_count);
#endif

  JCF_FILL (jcf, 2 * interfaces_count);

  /* Read interfaces. */
  for (i = 0; i < interfaces_count; i++)
    {
      uint16 index ATTRIBUTE_UNUSED = JCF_readu2 (jcf);
#ifdef HANDLE_CLASS_INTERFACE
      HANDLE_CLASS_INTERFACE (index);
#endif
    }
}

/* Read fields. */
static int
jcf_parse_fields (JCF* jcf)
{
  int i, j;
  uint16 fields_count;
  JCF_FILL (jcf, 2);
  fields_count = JCF_readu2 (jcf);

#ifdef HANDLE_START_FIELDS
  HANDLE_START_FIELDS (fields_count);
#endif
  for (i = 0; i < fields_count; i++)
    {
      uint16 access_flags = (JCF_FILL (jcf, 8), JCF_readu2 (jcf));
      uint16 name_index = JCF_readu2 (jcf);
      uint16 signature_index = JCF_readu2 (jcf);
      uint16 attribute_count = JCF_readu2 (jcf);
#ifdef HANDLE_START_FIELD
      HANDLE_START_FIELD (access_flags, name_index, signature_index,
			  attribute_count);
#endif
      for (j = 0; j < attribute_count; j++)
	{
	  int code = get_attribute (jcf, i, JV_FIELD_ATTR);
	  if (code != 0)
	    return code;
	}
#ifdef HANDLE_END_FIELD
      HANDLE_END_FIELD ();
#endif
    }
#ifdef HANDLE_END_FIELDS
  HANDLE_END_FIELDS ();
#endif
  return 0;
}

/* Read methods. */

static int
jcf_parse_one_method (JCF* jcf, int index)
{
  int i;
  uint16 access_flags = (JCF_FILL (jcf, 8), JCF_readu2 (jcf));
  uint16 name_index = JCF_readu2 (jcf);
  uint16 signature_index = JCF_readu2 (jcf);
  uint16 attribute_count = JCF_readu2 (jcf);
#ifdef HANDLE_METHOD
  HANDLE_METHOD(access_flags, name_index, signature_index, attribute_count);
#endif
  for (i = 0; i < attribute_count; i++)
    {
      int code = get_attribute (jcf, index, JV_METHOD_ATTR);
      if (code != 0)
	return code;
    }
#ifdef HANDLE_END_METHOD
  HANDLE_END_METHOD ();
#endif
  return 0;
}

static int
jcf_parse_methods (JCF* jcf)
{
  int i;
  uint16 methods_count;
  JCF_FILL (jcf, 2);
  methods_count = JCF_readu2 (jcf);
#ifdef HANDLE_START_METHODS
  HANDLE_START_METHODS (methods_count);
#endif
  for (i = 0; i < methods_count; i++)
    {
      int code = jcf_parse_one_method (jcf, i);
      if (code != 0)
	return code;
    }
#ifdef HANDLE_END_METHODS
  HANDLE_END_METHODS ();
#endif
  return 0;
}

/* Read attributes. */
static int
jcf_parse_final_attributes (JCF *jcf)
{
  int i;
  uint16 attributes_count = (JCF_FILL (jcf, 2), JCF_readu2 (jcf));
#ifdef START_FINAL_ATTRIBUTES
  START_FINAL_ATTRIBUTES (attributes_count)
#endif
  for (i = 0; i < attributes_count; i++)
    {
      int code = get_attribute (jcf, i, JV_CLASS_ATTR);
      if (code != 0)
	return code;
    }
  return 0;
}

/* Read and handle the "BootstrapMethods" attribute.

   Return 0 if OK.
*/
static int
jcf_parse_bootstrap_methods (JCF* jcf, int attribute_length ATTRIBUTE_UNUSED)
{
  int i;
  uint16 num_methods = JCF_readu2 (jcf);
  jcf->bootstrap_methods.count = num_methods;
  jcf->bootstrap_methods.methods
    = (bootstrap_method *) ggc_alloc_atomic (num_methods
					      * sizeof (bootstrap_method));
#ifdef HANDLE_START_BOOTSTRAP_METHODS
  HANDLE_START_BOOTSTRAP_METHODS (jcf, num_methods);
#endif

  for (i = 0; i < num_methods; i++)
    {
      unsigned j;
      bootstrap_method *m = &jcf->bootstrap_methods.methods[i];
      m->method_ref = JCF_readu2 (jcf);
      m->num_arguments = JCF_readu2 (jcf);
      m->bootstrap_arguments
	= (unsigned *) ggc_alloc_atomic (m->num_arguments
					 * sizeof (unsigned));
      for (j = 0; j < m->num_arguments; j++)
	m->bootstrap_arguments[j] = JCF_readu2 (jcf);
    }

#ifdef HANDLE_END_BOOTSTRAP_METHODS
  HANDLE_END_BOOTSTRAP_METHODS (num_methods);
#endif

  return 0;
}
