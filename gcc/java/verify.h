/* Declarations to interface gcj with bytecode verifier.
   Copyright (C) 2003, 2004, 2005, 2006, 2007 Free Software Foundation, Inc.

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

/* Written by Tom Tromey <tromey@redhat.com>.  */

#ifndef GCC_VERIFY_H
#define GCC_VERIFY_H

#ifdef __cplusplus
extern "C"
{
#endif

#include "system.h"
#include "coretypes.h"
#include "jcf.h"
#include "tree.h"
#include "java-tree.h"

typedef JCF vfy_constants;

/* For our purposes a string is the same as an identifier.  */
typedef tree vfy_string;

/* The TYPE_DECL for a class or primitive type.  */
typedef tree vfy_jclass;

/* An unsigned jshort.  */
typedef uint16 vfy_uint_16;

typedef struct
{
  int handler, start, end, type;
} vfy_exception;

typedef struct
{
  tree method;
  vfy_string signature;
  vfy_string name;
  const unsigned char *bytes;
  vfy_exception *exceptions;

  /* These fields are referred to directly by the verifier.  */
  vfy_jclass defining_class;
  int max_stack;
  int max_locals;
  int code_length;
  int exc_count;
} vfy_method;

/* Entry point to the verifier.  */
int verify_jvm_instructions_new (JCF *jcf, const unsigned char *byte_ops,
				 long length);

void *vfy_alloc (size_t bytes);
void vfy_free (void *mem);
bool vfy_strings_equal (vfy_string one, vfy_string two);
const char *vfy_string_bytes (vfy_string str);
int vfy_string_length (vfy_string str);
vfy_string vfy_get_string (const char *chars, int length);
vfy_string vfy_init_name (void);
vfy_string vfy_clinit_name (void);
int vfy_count_arguments (vfy_string signature);
vfy_string vfy_get_signature (vfy_method *method);
vfy_string vfy_get_method_name (vfy_method *method);
bool vfy_is_static (vfy_method *method);
const unsigned char *vfy_get_bytecode (vfy_method *method);
vfy_exception *vfy_get_exceptions (vfy_method *method);
void vfy_get_exception (vfy_exception *, int index, int *handler,
			int *start, int *end, int *handler_type);
int vfy_tag (vfy_constants *pool, int index);
void vfy_load_indexes (vfy_constants *pool, int index,
		       vfy_uint_16 *index0, vfy_uint_16 *index1);
vfy_constants *vfy_get_constants (vfy_jclass klass);
int vfy_get_constants_size (vfy_jclass klass);
vfy_string vfy_get_pool_string (vfy_constants *pool, int index);
vfy_jclass vfy_get_pool_class (vfy_constants *pool, int index);
vfy_string vfy_get_class_name (vfy_jclass klass);
bool vfy_is_assignable_from (vfy_jclass target, vfy_jclass source);
char vfy_get_primitive_char (vfy_jclass klass);
int vfy_get_interface_count (vfy_jclass klass);
vfy_jclass vfy_get_interface (vfy_jclass klass, int index);
bool vfy_is_array (vfy_jclass klass);
bool vfy_is_interface (vfy_jclass klass);
bool vfy_is_primitive (vfy_jclass klass);
vfy_jclass vfy_get_superclass (vfy_jclass klass);
vfy_jclass vfy_get_array_class (vfy_jclass klass);
vfy_jclass vfy_get_component_type (vfy_jclass klass);
bool vfy_is_abstract (vfy_jclass klass);
vfy_jclass vfy_find_class (vfy_jclass klass, vfy_string name);
vfy_jclass vfy_object_type (void);
vfy_jclass vfy_class_type (void);
vfy_jclass vfy_string_type (void);
vfy_jclass vfy_throwable_type (void);
vfy_jclass vfy_unsuitable_type (void);
vfy_jclass vfy_return_address_type (void);
vfy_jclass vfy_null_type (void);
int vfy_fail (const char *message, int pc, vfy_jclass ignore1,
	      vfy_method *method);
vfy_jclass vfy_get_primitive_type (int type);
void vfy_note_stack_depth (vfy_method *method, int pc, int depth);
void vfy_note_stack_type (vfy_method *method, int pc, int slot,
			  vfy_jclass type);
void vfy_note_local_type (vfy_method *method, int pc, int slot,
			  vfy_jclass type);
void vfy_note_instruction_seen (int pc);
bool vfy_class_has_field (vfy_jclass klass, vfy_string name,
			  vfy_string signature);

#define GLOM(name, stuff) name ## stuff
#define VFY_PRIMITIVE_CLASS(name) \
  vfy_get_primitive_type ((int) (GLOM (name, _type)))

typedef enum
{
#define JAVAOP(name, num, ignore1, ignore2, ignore3) \
  GLOM (op_, name) = num,
#include "javaop.def"
  java_opcode_end
} java_opcode;


#define JV_CONSTANT_Class CONSTANT_Class
#define JV_CONSTANT_ResolvedClass CONSTANT_ResolvedClass
#define JV_CONSTANT_String CONSTANT_String
#define JV_CONSTANT_ResolvedString CONSTANT_ResolvedString
#define JV_CONSTANT_Integer CONSTANT_Integer
#define JV_CONSTANT_Float CONSTANT_Float
#define JV_CONSTANT_Long CONSTANT_Long
#define JV_CONSTANT_Double CONSTANT_Double
#define JV_CONSTANT_Fieldref CONSTANT_Fieldref
#define JV_CONSTANT_InterfaceMethodref CONSTANT_InterfaceMethodref
#define JV_CONSTANT_Methodref CONSTANT_Methodref

int verify_method (vfy_method *meth);

#ifdef __cplusplus
}
#endif

#endif /* ! GCC_VERIFY_H */
