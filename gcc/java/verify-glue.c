/* Glue to interface gcj with bytecode verifier.
   Copyright (C) 2003, 2004, 2005 Free Software Foundation, Inc.

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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

/* Written by Tom Tromey <tromey@redhat.com>.  */

#include "config.h"

#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "errors.h"
#include "parse.h"

#include "verify.h"
#include "java-tree.h"
#include "java-except.h"

void *
vfy_alloc (size_t bytes)
{
  return xmalloc (bytes);
}

void
vfy_free (void *mem)
{
  free (mem);
}

bool
vfy_strings_equal (vfy_string one, vfy_string two)
{
  return one == two;
}

const char *
vfy_string_bytes (vfy_string str)
{
  return IDENTIFIER_POINTER (str);
}

int
vfy_string_length (vfy_string str)
{
  return IDENTIFIER_LENGTH (str);
}

vfy_string
vfy_init_name (void)
{
  return init_identifier_node;
}

vfy_string
vfy_clinit_name (void)
{
  return clinit_identifier_node;
}

static const char*
skip_one_type (const char* ptr)
{
  int ch = *ptr++;

  while (ch == '[')
    { 
      ch = *ptr++;
    }
  
  if (ch == 'L')
    {
      do { ch = *ptr++; } while (ch != ';');
    }

  return ptr;
}

int
vfy_count_arguments (vfy_string signature)
{
  const char *ptr = IDENTIFIER_POINTER (signature);
  int arg_count = 0;

  /* Skip '('.  */
  ptr++;

  /* Count args.  */
  while (*ptr != ')')
    {
      ptr = skip_one_type (ptr);
      arg_count += 1;
    }

  return arg_count;
}

vfy_string
vfy_get_string (const char *s, int len)
{
  return get_identifier_with_length (s, len);
}

vfy_string
vfy_get_signature (vfy_method *method)
{
  return method->signature;
}

vfy_string
vfy_get_method_name (vfy_method *method)
{
  return method->name;
}

bool
vfy_is_static (vfy_method *method)
{
  return METHOD_STATIC (method->method);
}

const unsigned char *
vfy_get_bytecode (vfy_method *method)
{
  return method->bytes;
}

vfy_exception *
vfy_get_exceptions (vfy_method *method)
{
  return method->exceptions;
}

void
vfy_get_exception (vfy_exception *exceptions, int index, int *handler,
		   int *start, int *end, int *handler_type)
{
  *handler = exceptions[index].handler;
  *start = exceptions[index].start;
  *end = exceptions[index].end;
  *handler_type = exceptions[index].type;
}

int
vfy_tag (vfy_constants *pool, int index)
{
  int result = JPOOL_TAG (pool, index);
  /* gcj will resolve constant pool entries other than string and
     class references.  The verifier doesn't care about the values, so
     we just strip off the resolved flag.  */
  if ((result & CONSTANT_ResolvedFlag) != 0
      && result != CONSTANT_ResolvedString
      && result != CONSTANT_ResolvedClass)
    result &= ~ CONSTANT_ResolvedFlag;
  return result;
}

void
vfy_load_indexes (vfy_constants *pool, int index,
		  vfy_uint_16 *index0, vfy_uint_16 *index1)
{
  *index0 = JPOOL_USHORT1 (pool, index);
  *index1 = JPOOL_USHORT2 (pool, index);
}

vfy_constants *
vfy_get_constants (vfy_jclass klass)
{
  return TYPE_JCF (klass);
}

int
vfy_get_constants_size (vfy_jclass klass)
{
  return JPOOL_SIZE (TYPE_JCF (klass));
}

vfy_string
vfy_get_pool_string (vfy_constants *pool, int index)
{
  return get_name_constant (pool, index);
}

vfy_jclass
vfy_get_pool_class (vfy_constants *pool, int index)
{
  vfy_jclass k;
  k = get_class_constant (pool, index);
  return k;
}

vfy_string
vfy_make_string (const char *s, int len)
{
  tree result;
  char *s2 = (char *) s;
  char save = s2[len];
  s2[len] = '\0';
  result = get_identifier (s2);
  s2[len] = save;
  return result;  
}

vfy_string
vfy_get_class_name (vfy_jclass klass)
{
  return DECL_NAME (TYPE_NAME (klass));
}

bool
vfy_is_assignable_from (vfy_jclass target, vfy_jclass source)
{
  /* Any class is always assignable to itself, or java.lang.Object. */
  if (source == target || target == object_type_node)
    return true;

  /* For the C++ ABI, perform this test statically. */
  if (! flag_indirect_dispatch)
    return can_widen_reference_to (source, target);

  /* For the BC-ABI, we assume at compile time that reference types are always 
  compatible.  However, a type assertion table entry is emitted so that the
  runtime can detect binary-incompatible changes.  */

  add_type_assertion (current_class, JV_ASSERT_TYPES_COMPATIBLE, source,
		      target);
  return true;
}

char
vfy_get_primitive_char (vfy_jclass klass)
{
  tree sig;
  if (! vfy_is_primitive (klass))
    abort ();
  sig = build_java_signature (klass);
  return (IDENTIFIER_POINTER (sig))[0];
}

bool
vfy_is_array (vfy_jclass klass)
{
  return TYPE_ARRAY_P (klass);
}

bool
vfy_is_interface (vfy_jclass klass)
{
  return CLASS_INTERFACE (TYPE_NAME (klass));
}

bool
vfy_is_primitive (vfy_jclass klass)
{
  return JPRIMITIVE_TYPE_P (klass);
}

vfy_jclass
vfy_get_superclass (vfy_jclass klass)
{
  vfy_jclass k;
  k = CLASSTYPE_SUPER (klass);
  return k;
}

vfy_jclass
vfy_get_array_class (vfy_jclass klass)
{
  vfy_jclass k;
  k = build_java_array_type (klass, -1);
  return k;
}

vfy_jclass
vfy_get_component_type (vfy_jclass klass)
{
  vfy_jclass k;
  if (! vfy_is_array (klass))
    abort ();
  k = TYPE_ARRAY_ELEMENT (klass);
  if (TREE_CODE (k) == POINTER_TYPE)
    k = TREE_TYPE (k);
  return k;
}

bool
vfy_is_abstract (vfy_jclass klass)
{
  return CLASS_ABSTRACT (TYPE_NAME (klass));
}

vfy_jclass
vfy_find_class (vfy_jclass ignore ATTRIBUTE_UNUSED, vfy_string name)
{
  vfy_jclass k;

  k = get_type_from_signature (name);
  if (TREE_CODE (k) == POINTER_TYPE)
    k = TREE_TYPE (k);

  return k;
}

vfy_jclass
vfy_object_type (void)
{
  vfy_jclass k;
  k = object_type_node;
  return k;
}

vfy_jclass
vfy_string_type (void)
{
  vfy_jclass k;
  k = string_type_node;
  return k;
}

vfy_jclass
vfy_throwable_type (void)
{
  vfy_jclass k;
  k = throwable_type_node;
  return k;
}

vfy_jclass
vfy_unsuitable_type (void)
{
  return TYPE_SECOND;
}

vfy_jclass
vfy_return_address_type (void)
{
  return TYPE_RETURN_ADDR;
}

vfy_jclass
vfy_null_type (void)
{
  return TYPE_NULL;
}

bool
vfy_class_has_field (vfy_jclass klass, vfy_string name,
		     vfy_string signature)
{
  tree field = TYPE_FIELDS (klass);
  while (field != NULL_TREE)
    {
      if (DECL_NAME (field) == name
	  && build_java_signature (TREE_TYPE (field)) == signature)
	return true;
      field = TREE_CHAIN (field);
    }
  return false;
}

int
vfy_fail (const char *message, int pc, vfy_jclass ignore1 ATTRIBUTE_UNUSED,
	  vfy_method *ignore2 ATTRIBUTE_UNUSED)
{
  if (pc == -1)
    error ("verification failed: %s", message);
  else
    error ("verification failed at PC=%d: %s", pc, message);
  /* We have to return a value for the verifier to throw.  */
  return 1;
}

vfy_jclass
vfy_get_primitive_type (int type)
{
  vfy_jclass k;
  k = decode_newarray_type (type);
  return k;
}

void
vfy_note_stack_depth (vfy_method *method, int pc, int depth)
{
  tree label = lookup_label (pc);
  LABEL_TYPE_STATE (label) = make_tree_vec (method->max_locals + depth);
}

void
vfy_note_stack_type (vfy_method *method, int pc, int slot, vfy_jclass type)
{
  tree label, vec;
  
  slot += method->max_locals;

  if (type == object_type_node)
    type = object_ptr_type_node;

  label = lookup_label (pc);
  vec = LABEL_TYPE_STATE (label);
  TREE_VEC_ELT (vec, slot) = type;
}

void
vfy_note_local_type (vfy_method *method ATTRIBUTE_UNUSED, int pc, int slot,
		     vfy_jclass type)
{
  tree label, vec;
  
  if (type == object_type_node)
    type = object_ptr_type_node;

  label = lookup_label (pc);
  vec = LABEL_TYPE_STATE (label);
  TREE_VEC_ELT (vec, slot) = type;
}

void
vfy_note_instruction_seen (int pc)
{
  instruction_bits[pc] |= BCODE_VERIFIED;
}

/* Verify the bytecodes of the current method.
   Return 1 on success, 0 on failure. */
int
verify_jvm_instructions_new (JCF *jcf, const unsigned char *byte_ops,
			 long length)
{
  vfy_method method;
  int i, result, eh_count;
  vfy_exception *exceptions;

  method_init_exceptions ();

  JCF_SEEK (jcf, DECL_CODE_OFFSET (current_function_decl) + length);
  eh_count = JCF_readu2 (jcf);

  exceptions = (vfy_exception *) xmalloc (eh_count * sizeof (vfy_exception));
  for (i = 0; i < eh_count; ++i)
    {
      int start_pc, end_pc, handler_pc, catch_type;
      unsigned char *p = jcf->read_ptr + 8 * i;
      start_pc = GET_u2 (p);
      end_pc = GET_u2 (p+2);
      handler_pc = GET_u2 (p+4);
      catch_type = GET_u2 (p+6);

      if (start_pc < 0 || start_pc >= length
	  || end_pc < 0 || end_pc > length || start_pc >= end_pc
	  || handler_pc < 0 || handler_pc >= length)
	{
	  error ("bad pc in exception_table");
	  free (exceptions);
	  return 0;
	}

      exceptions[i].handler = handler_pc;
      exceptions[i].start = start_pc;
      exceptions[i].end = end_pc;
      exceptions[i].type = catch_type;

      add_handler (start_pc, end_pc,
		   lookup_label (handler_pc),
		   catch_type == 0 ? NULL_TREE
		   : get_class_constant (jcf, catch_type));
      instruction_bits[handler_pc] |= BCODE_EXCEPTION_TARGET;
    }

  handle_nested_ranges ();

  method.method = current_function_decl;
  method.signature = build_java_signature (TREE_TYPE (current_function_decl));
  method.name = DECL_NAME (current_function_decl);
  method.bytes = byte_ops;
  method.exceptions = exceptions;
  method.defining_class = DECL_CONTEXT (current_function_decl);
  method.max_stack = DECL_MAX_STACK (current_function_decl);
  method.max_locals = DECL_MAX_LOCALS (current_function_decl);
  method.code_length = length;
  method.exc_count = eh_count;

  result = verify_method (&method);

  free (exceptions);

  return result;
}
