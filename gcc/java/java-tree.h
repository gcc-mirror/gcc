/* Definitions for parsing and type checking for the GNU compiler for
   the Java(TM) language.
   Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005
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

/* Hacked by Per Bothner <bothner@cygnus.com> February 1996. */

#ifndef GCC_JAVA_TREE_H
#define GCC_JAVA_TREE_H

#include "hashtab.h"

/* Java language-specific tree codes.  */
#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) SYM,
enum java_tree_code {
  __DUMMY = LAST_AND_UNUSED_TREE_CODE,
#include "java-tree.def"
  LAST_JAVA_TREE_CODE
};
#undef DEFTREECODE

struct JCF;

/* Usage of TREE_LANG_FLAG_?:
   0: IS_A_SINGLE_IMPORT_CLASSFILE_NAME_P (in IDENTIFIER_NODE)
      FOR_LOOP_P (in LOOP_EXPR)
      SUPPRESS_UNREACHABLE_ERROR (for other _EXPR nodes)
      ANONYMOUS_CLASS_P (in RECORD_TYPE)
      ARG_FINAL_P (in TREE_LIST)
   1: IS_A_CLASSFILE_NAME (in IDENTIFIER_NODE)
      COMPOUND_ASSIGN_P (in EXPR (binop_*))
      LOCAL_CLASS_P (in RECORD_TYPE)
      BLOCK_IS_IMPLICIT (in BLOCK)
   2: QUALIFIED_P (in IDENTIFIER_NODE)
      PRIMARY_P (in EXPR_WITH_FILE_LOCATION)
      MODIFY_EXPR_FROM_INITIALIZATION_P (in MODIFY_EXPR)
      CLASS_METHOD_CHECKED_P (in RECORD_TYPE) 
      CLASS_FILE_P (in a TRANSLATION_UNIT_DECL in current_file_list)
   3: IS_AN_IMPORT_ON_DEMAND_P (in IDENTIFIER_NODE)
      RESOLVE_PACKAGE_NAME_P (in EXPR_WITH_FILE_LOCATION)
      SWITCH_HAS_DEFAULT (in SWITCH_EXPR)
      HAS_FINALIZER (in RECORD_TYPE)
   4: IS_A_COMMAND_LINE_FILENAME_P (in IDENTIFIER_NODE)
      RESOLVE_TYPE_NAME_P (in EXPR_WITH_FILE_LOCATION)
      CALL_USING_SUPER (in CALL_EXPR)
      IS_ARRAY_LENGTH_ACCESS (in INDIRECT_REF)
   5: HAS_BEEN_ALREADY_PARSED_P (in IDENTIFIER_NODE)
      IS_BREAK_STMT_P (in EXPR_WITH_FILE_LOCATION)
      IS_CRAFTED_STRING_BUFFER_P (in CALL_EXPR)
      IS_INIT_CHECKED (in SAVE_EXPR)
   6: CAN_COMPLETE_NORMALLY (in statement nodes)
      NESTED_FIELD_ACCESS_IDENTIFIER_P (in IDENTIFIER_NODE)

   Usage of TYPE_LANG_FLAG_?:
   0: CLASS_ACCESS0_GENERATED_P (in RECORD_TYPE)
   1: TYPE_ARRAY_P (in RECORD_TYPE).
   2: CLASS_PARSED_P (in RECORD_TYPE).
   3: CLASS_FROM_SOURCE_P (in RECORD_TYPE).
   4: CLASS_P (in RECORD_TYPE).
   5: CLASS_FROM_CURRENTLY_COMPILED_SOURCE_P (in RECORD_TYPE)
   6: CLASS_BEING_LAIDOUT (in RECORD_TYPE)

   Usage of DECL_LANG_FLAG_?:
   0: METHOD_DEPRECATED (in FUNCTION_DECL).
      FIELD_DEPRECATED (in FIELD_DECL).
      CLASS_DEPRECATED (in TYPE_DECL).
   1: METHOD_PUBLIC (in FUNCTION_DECL).
      FIELD_PUBLIC (in FIELD_DECL).
      CLASS_PUBLIC (in TYPE_DECL).
   2: METHOD_STATIC (in FUNCTION_DECL).
      (But note that FIELD_STATIC uses TREE_STATIC!)
      FIELD_SYNTHETIC (in FIELD_DECL)
      CLASS_COMPLETE_P (in TYPE_DECL)
   3: METHOD_FINAL (in FUNCTION_DECL)
      FIELD_FINAL (in FIELD_DECL)
      CLASS_FINAL (in TYPE_DECL)
      DECL_FINAL (in any decl)
   4: METHOD_SYNCHRONIZED (in FUNCTION_DECL).
      CLASS_INTERFACE (in TYPE_DECL)
      FIELD_VOLATILE (int FIELD_DECL)
   5: METHOD_ABSTRACT (in FUNCTION_DECL).
      CLASS_ABSTRACT (in TYPE_DECL)
      FIELD_TRANSIENT (in FIELD_DECL)
   6: CLASS_SUPER (in TYPE_DECL, ACC_SUPER flag)
      FIELD_LOCAL_ALIAS (in FIELD_DECL)
   7: DECL_CONSTRUCTOR_P (in FUNCTION_DECL).
      CLASS_STATIC (in TYPE_DECL)
      FIELD_LOCAL_ALIAS_USED (in FIELD_DECL)
      FIELD_THISN (in FIELD_DECL)
*/

#define VAR_OR_FIELD_CHECK(DECL) \
  TREE_CHECK3 (DECL, FIELD_DECL, VAR_DECL, PARM_DECL)

/* True if the class whose TYPE_BINFO this is has a superclass.
   (True of all classes except Object.) */
#define CLASS_HAS_SUPER_FLAG(BINFO) BINFO_FLAG_1 (BINFO)
#define CLASS_HAS_SUPER(TYPE) \
  (TYPE_BINFO (TYPE) && CLASS_HAS_SUPER_FLAG (TYPE_BINFO (TYPE)))

/* Return the supertype of class TYPE, or NULL_TREE is it has none. */
#define CLASSTYPE_SUPER(TYPE) (CLASS_HAS_SUPER (TYPE) \
  ? BINFO_TYPE (BINFO_BASE_BINFO (TYPE_BINFO (TYPE), 0)) \
  : NULL_TREE)

/* True if the class we are compiling is a .java source file;
   false if it is a .class bytecode file. */
extern int compiling_from_source;

/* The class defined by the actual (main) file we are compiling. */
#define main_class \
  java_global_trees[JTI_MAIN_CLASS]

/* The class we use as the base for name resolution.  It's usually the
   class we're generating code for but sometimes it points to an inner
   class.  If you really want to know the class we're currently
   generating code for, use output_class instead.  */
#define current_class \
  java_global_trees[JTI_CURRENT_CLASS]

/* The class we are currently generating.  Really.  */
#define output_class \
  java_global_trees[JTI_OUTPUT_CLASS]

/* List of all class DECLs seen so far.  */
#define all_class_list \
  java_global_trees[JTI_ALL_CLASS_LIST]

/* List of virtual decls referred to by this translation unit, used to
   generate virtual method offset symbol table.  */

/* The virtual offset table.  This is emitted as uninitialized data of
   the required length, and filled out at run time during class
   linking. */

/* The virtual offset symbol table. Used by the runtime to fill out
   the otable. */

extern int flag_emit_class_files;

extern int flag_filelist_file;

/* When nonzero, permit the use of the assert keyword.  */

extern int flag_assert;

/* When nonzero, assume all native functions are implemented with
   JNI, not CNI.  */

extern int flag_jni;

/* When nonzero, report the now deprecated empty statements.  */

extern int flag_extraneous_semicolon;

/* When nonzero, always check for a non gcj generated classes archive.  */

extern int flag_force_classes_archive_check;

/* Resource name.  */
extern const char *resource_name;

/* Turned to 1 if -Wall was encountered. See lang.c for their meanings.  */
extern int flag_wall;
extern int flag_redundant;
extern int flag_not_overriding;
extern int flag_static_local_jdk1_1;

/* When nonzero, warn when source file is newer than matching class
   file.  */
extern int flag_newer;

/* When nonzero, call a library routine to do integer divisions. */
extern int flag_use_divide_subroutine;

/* When nonzero, generate code for the Boehm GC.  */
extern int flag_use_boehm_gc;

/* When nonzero, assume the runtime uses a hash table to map an
   object to its synchronization structure.  */
extern int flag_hash_synchronization;

/* When nonzero, generate checks for references to NULL.  */
extern int flag_check_references;

/* Used through STATIC_CLASS_INIT_OPT_P to check whether static
   initialization optimization should be performed.  */
extern int flag_optimize_sci;

/* When nonzero, use offset tables for virtual method calls
   in order to improve binary compatibility. */
extern int flag_indirect_dispatch;

/* When zero, don't generate runtime array store checks. */
extern int flag_store_check;

/* Encoding used for source files.  */
extern const char *current_encoding;

/* The Java .class file that provides main_class;  the main input file. */
extern GTY(()) struct JCF * current_jcf;

/* Set to nonzero value in order to emit class initialization code
   before static field references.  */
extern int always_initialize_class_p;

extern int flag_verify_invocations;

/* Largest pc so far in this method that has been passed to lookup_label. */
extern int highest_label_pc_this_method;

/* Base value for this method to add to pc to get generated label. */
extern int start_label_pc_this_method;

typedef struct CPool constant_pool;

#define CONSTANT_ResolvedFlag 16

/* The cpool->data[i] for a ResolvedString points to a STRING_CST. */
#define CONSTANT_ResolvedString    (CONSTANT_String+CONSTANT_ResolvedFlag)

/* The cpool->data[i] for a ResolvedClass points to a RECORD_TYPE. */
#define CONSTANT_ResolvedClass     (CONSTANT_Class+CONSTANT_ResolvedFlag)

#define CPOOL_UTF(CPOOL, INDEX) ((CPOOL)->data[INDEX].t)

/* A NameAndType constant is represented as a TREE_LIST.
   The type is the signature string (as an IDENTIFIER_NODE).  */

#define NAME_AND_TYPE_NAME(CPOOL, IDX) \
  CPOOL_UTF(CPOOL, CPOOL_USHORT1(CPOOL, IDX))
#define NAME_AND_TYPE_SIGNATURE(CPOOL, IDX) \
  CPOOL_UTF(CPOOL, CPOOL_USHORT2(CPOOL, IDX))

/* A FieldRef, MethodRef or InterfaceMethodRef constant
   is represented as a TREE_LIST. */

#define COMPONENT_REF_CLASS_INDEX(CPOOL, IDX) CPOOL_USHORT1(CPOOL, IDX)
#define COMPONENT_REF_NAME_AND_TYPE(CPOOL, IDX) CPOOL_USHORT2(CPOOL, IDX)
#define COMPONENT_REF_NAME(CPOOL, IDX) \
  NAME_AND_TYPE_NAME (CPOOL, COMPONENT_REF_NAME_AND_TYPE(CPOOL, IDX))
#define COMPONENT_REF_SIGNATURE(CPOOL, IDX) \
  NAME_AND_TYPE_SIGNATURE (CPOOL, COMPONENT_REF_NAME_AND_TYPE(CPOOL, IDX))

extern GTY(()) tree java_lang_cloneable_identifier_node;
extern GTY(()) tree java_io_serializable_identifier_node;
extern GTY(()) tree gcj_abi_version;

enum java_tree_index
{
  JTI_PROMOTED_BYTE_TYPE_NODE,
  JTI_PROMOTED_SHORT_TYPE_NODE,
  JTI_PROMOTED_CHAR_TYPE_NODE,
  JTI_PROMOTED_BOOLEAN_TYPE_NODE,

  JTI_BYTE_TYPE_NODE,
  JTI_SHORT_TYPE_NODE,
  JTI_INT_TYPE_NODE,
  JTI_LONG_TYPE_NODE,
  
  JTI_UNSIGNED_BYTE_TYPE_NODE,
  JTI_UNSIGNED_SHORT_TYPE_NODE,
  JTI_UNSIGNED_INT_TYPE_NODE,
  JTI_UNSIGNED_LONG_TYPE_NODE,
  
  JTI_DECIMAL_INT_MAX_NODE,
  JTI_DECIMAL_LONG_MAX_NODE,

  JTI_OBJECT_TYPE_NODE,
  JTI_UNQUALIFIED_OBJECT_ID_NODE,
  JTI_OBJECT_PTR_TYPE_NODE,
  JTI_STRING_TYPE_NODE,
  JTI_STRING_PTR_TYPE_NODE,
  JTI_THROWABLE_TYPE_NODE,
  JTI_EXCEPTION_TYPE_NODE,
  JTI_RUNTIME_EXCEPTION_TYPE_NODE,
  JTI_ERROR_EXCEPTION_TYPE_NODE,
  JTI_RAWDATA_PTR_TYPE_NODE,

  JTI_BYTE_ARRAY_TYPE_NODE,
  JTI_SHORT_ARRAY_TYPE_NODE,
  JTI_INT_ARRAY_TYPE_NODE,
  JTI_LONG_ARRAY_TYPE_NODE,
  JTI_BOOLEAN_ARRAY_TYPE_NODE,
  JTI_CHAR_ARRAY_TYPE_NODE,
  JTI_DOUBLE_ARRAY_TYPE_NODE,
  JTI_FLOAT_ARRAY_TYPE_NODE,
  JTI_ARRAY_ARRAY_TYPE_NODE,
  JTI_OBJECT_ARRAY_TYPE_NODE,
  JTI_STRING_ARRAY_TYPE_NODE,
  JTI_BOOLEAN_ARRAY_VTABLE,
  JTI_BYTE_ARRAY_VTABLE,
  JTI_CHAR_ARRAY_VTABLE,
  JTI_SHORT_ARRAY_VTABLE,
  JTI_INT_ARRAY_VTABLE,
  JTI_LONG_ARRAY_VTABLE,
  JTI_FLOAT_ARRAY_VTABLE,
  JTI_DOUBLE_ARRAY_VTABLE,
  JTI_TYPE_IDENTIFIER_NODE,      
  JTI_INIT_IDENTIFIER_NODE,      
  JTI_CLINIT_IDENTIFIER_NODE,      
  JTI_FINIT_IDENTIFIER_NODE,      
  JTI_INSTINIT_IDENTIFIER_NODE,
  JTI_VOID_SIGNATURE_NODE,       
  JTI_LENGTH_IDENTIFIER_NODE,  
  JTI_FINALIZE_IDENTIFIER_NODE,
  JTI_THIS_IDENTIFIER_NODE,  
  JTI_SUPER_IDENTIFIER_NODE,  
  JTI_CONTINUE_IDENTIFIER_NODE,  
  JTI_ACCESS0_IDENTIFIER_NODE, 
  JTI_CLASSDOLLAR_IDENTIFIER_NODE,
  JTI_ONE_ELT_ARRAY_DOMAIN_TYPE,

  JTI_RETURN_ADDRESS_TYPE_NODE,

  JTI_LONG_ZERO_NODE,
  JTI_FLOAT_ZERO_NODE,
  JTI_DOUBLE_ZERO_NODE,
  JTI_INTEGER_TWO_NODE,
  JTI_INTEGER_FOUR_NODE,

  JTI_METHODTABLE_TYPE,
  JTI_METHODTABLE_PTR_TYPE,

  JTI_UTF8CONST_TYPE,
  JTI_UTF8CONST_PTR_TYPE,

  JTI_CLASS_TYPE_NODE,
  JTI_CLASS_PTR_TYPE,
  JTI_FIELD_TYPE_NODE,
  JTI_CONSTANTS_TYPE_NODE,
  JTI_DTABLE_TYPE, 
  JTI_DTABLE_PTR_TYPE,
  JTI_FIELD_PTR_TYPE_NODE,
  JTI_FIELD_INFO_UNION_NODE,
  JTI_EXCEPTION_TYPE,
  JTI_EXCEPTION_PTR_TYPE,
  JTI_LINENUMBERENTRY_TYPE,
  JTI_LINENUMBERS_TYPE,
  JTI_METHOD_TYPE_NODE,
  JTI_METHOD_PTR_TYPE_NODE,
  JTI_OTABLE_TYPE,
  JTI_OTABLE_PTR_TYPE,
  JTI_ATABLE_TYPE,
  JTI_ATABLE_PTR_TYPE,
  JTI_ITABLE_TYPE,
  JTI_ITABLE_PTR_TYPE,
  JTI_SYMBOL_TYPE,
  JTI_SYMBOLS_ARRAY_TYPE,
  JTI_SYMBOLS_ARRAY_PTR_TYPE,
  JTI_ASSERTION_ENTRY_TYPE,
  JTI_ASSERTION_TABLE_TYPE,

  JTI_END_PARAMS_NODE,

  JTI_THROW_NODE,
  JTI_ALLOC_OBJECT_NODE,
  JTI_ALLOC_NO_FINALIZER_NODE,
  JTI_SOFT_INSTANCEOF_NODE,
  JTI_SOFT_CHECKCAST_NODE,
  JTI_SOFT_INITCLASS_NODE,
  JTI_SOFT_NEWARRAY_NODE,
  JTI_SOFT_ANEWARRAY_NODE,
  JTI_SOFT_MULTIANEWARRAY_NODE,
  JTI_SOFT_BADARRAYINDEX_NODE,
  JTI_SOFT_NULLPOINTER_NODE,
  JTI_SOFT_ABSTRACTMETHOD_NODE,
  JTI_SOFT_NOSUCHFIELD_NODE,
  JTI_SOFT_CHECKARRAYSTORE_NODE,
  JTI_SOFT_MONITORENTER_NODE,
  JTI_SOFT_MONITOREXIT_NODE,
  JTI_SOFT_LOOKUPINTERFACEMETHOD_NODE,
  JTI_SOFT_LOOKUPINTERFACEMETHODBYNAME_NODE,
  JTI_SOFT_LOOKUPJNIMETHOD_NODE,
  JTI_SOFT_GETJNIENVNEWFRAME_NODE,
  JTI_SOFT_JNIPOPSYSTEMFRAME_NODE,
  JTI_SOFT_UNWRAPJNI_NODE,
  JTI_SOFT_FMOD_NODE,
  JTI_SOFT_IDIV_NODE,
  JTI_SOFT_IREM_NODE,
  JTI_SOFT_LDIV_NODE,
  JTI_SOFT_LREM_NODE,

  JTI_ACCESS_FLAGS_TYPE_NODE,

  JTI_NATIVECODE_PTR_ARRAY_TYPE_NODE,

  JTI_WFL_OPERATOR,

  JTI_MAIN_CLASS,
  JTI_CURRENT_CLASS,
  JTI_OUTPUT_CLASS,
  JTI_ALL_CLASS_LIST,

  JTI_PREDEF_FILENAMES,

  JTI_MAX
};

extern GTY(()) tree java_global_trees[JTI_MAX];

/* "Promoted types" that are used for primitive types smaller
   than int.  We could use int_type_node, but then we would lose
   type information (such as needed for debugging). */
#define promoted_byte_type_node \
  java_global_trees[JTI_PROMOTED_BYTE_TYPE_NODE]
#define promoted_short_type_node \
  java_global_trees[JTI_PROMOTED_SHORT_TYPE_NODE]
#define promoted_char_type_node \
  java_global_trees[JTI_PROMOTED_CHAR_TYPE_NODE]
#define promoted_boolean_type_node \
  java_global_trees[JTI_PROMOTED_BOOLEAN_TYPE_NODE]

#define byte_type_node \
  java_global_trees[JTI_BYTE_TYPE_NODE]
#define short_type_node \
  java_global_trees[JTI_SHORT_TYPE_NODE]
#define int_type_node \
  java_global_trees[JTI_INT_TYPE_NODE]
#define long_type_node \
  java_global_trees[JTI_LONG_TYPE_NODE]

#define unsigned_byte_type_node \
  java_global_trees[JTI_UNSIGNED_BYTE_TYPE_NODE]
#define unsigned_short_type_node \
  java_global_trees[JTI_UNSIGNED_SHORT_TYPE_NODE]
#define unsigned_int_type_node \
  java_global_trees[JTI_UNSIGNED_INT_TYPE_NODE]
#define unsigned_long_type_node \
  java_global_trees[JTI_UNSIGNED_LONG_TYPE_NODE]

#define decimal_int_max \
  java_global_trees[JTI_DECIMAL_INT_MAX_NODE]
#define decimal_long_max \
  java_global_trees[JTI_DECIMAL_LONG_MAX_NODE]

#define object_type_node \
  java_global_trees[JTI_OBJECT_TYPE_NODE]
#define unqualified_object_id_node \
  java_global_trees[JTI_UNQUALIFIED_OBJECT_ID_NODE]
#define object_ptr_type_node \
  java_global_trees[JTI_OBJECT_PTR_TYPE_NODE]
#define string_type_node \
  java_global_trees[JTI_STRING_TYPE_NODE]
#define string_ptr_type_node \
  java_global_trees[JTI_STRING_PTR_TYPE_NODE]
#define throwable_type_node \
  java_global_trees[JTI_THROWABLE_TYPE_NODE]
#define exception_type_node \
  java_global_trees[JTI_EXCEPTION_TYPE_NODE]
#define runtime_exception_type_node \
  java_global_trees[JTI_RUNTIME_EXCEPTION_TYPE_NODE]
#define error_exception_type_node \
  java_global_trees[JTI_ERROR_EXCEPTION_TYPE_NODE]
#define rawdata_ptr_type_node \
  java_global_trees[JTI_RAWDATA_PTR_TYPE_NODE]

#define byte_array_type_node \
  java_global_trees[JTI_BYTE_ARRAY_TYPE_NODE]
#define short_array_type_node \
  java_global_trees[JTI_SHORT_ARRAY_TYPE_NODE]
#define int_array_type_node \
  java_global_trees[JTI_INT_ARRAY_TYPE_NODE]
#define long_array_type_node \
  java_global_trees[JTI_LONG_ARRAY_TYPE_NODE]
#define boolean_array_type_node \
  java_global_trees[JTI_BOOLEAN_ARRAY_TYPE_NODE]
#define char_array_type_node \
  java_global_trees[JTI_CHAR_ARRAY_TYPE_NODE]
#define double_array_type_node \
  java_global_trees[JTI_DOUBLE_ARRAY_TYPE_NODE]
#define float_array_type_node \
  java_global_trees[JTI_FLOAT_ARRAY_TYPE_NODE]
#define array_array_type_node \
  java_global_trees[JTI_ARRAY_ARRAY_TYPE_NODE]
#define object_array_type_node \
  java_global_trees[JTI_OBJECT_ARRAY_TYPE_NODE]
#define string_array_type_node \
  java_global_trees[JTI_STRING_ARRAY_TYPE_NODE]
#define boolean_array_vtable \
  java_global_trees[JTI_BOOLEAN_ARRAY_VTABLE]
#define byte_array_vtable \
  java_global_trees[JTI_BYTE_ARRAY_VTABLE]
#define char_array_vtable \
  java_global_trees[JTI_CHAR_ARRAY_VTABLE]
#define short_array_vtable \
  java_global_trees[JTI_SHORT_ARRAY_VTABLE]
#define int_array_vtable \
  java_global_trees[JTI_INT_ARRAY_VTABLE]
#define long_array_vtable \
  java_global_trees[JTI_LONG_ARRAY_VTABLE]
#define float_array_vtable \
  java_global_trees[JTI_FLOAT_ARRAY_VTABLE]
#define double_array_vtable \
  java_global_trees[JTI_DOUBLE_ARRAY_VTABLE]
#define TYPE_identifier_node \
  java_global_trees[JTI_TYPE_IDENTIFIER_NODE]      /* "TYPE" */
#define init_identifier_node \
  java_global_trees[JTI_INIT_IDENTIFIER_NODE]      /* "<init>" */
#define clinit_identifier_node \
  java_global_trees[JTI_CLINIT_IDENTIFIER_NODE]      /* "<clinit>" */
#define finit_identifier_node \
  java_global_trees[JTI_FINIT_IDENTIFIER_NODE]      /* "finit$" */
/* FIXME "instinit$" and "finit$" should be merged  */
#define instinit_identifier_node \
  java_global_trees[JTI_INSTINIT_IDENTIFIER_NODE]  /* "instinit$" */
#define void_signature_node \
  java_global_trees[JTI_VOID_SIGNATURE_NODE]       /* "()V" */
#define length_identifier_node \
  java_global_trees[JTI_LENGTH_IDENTIFIER_NODE]  /* "length" */
#define finalize_identifier_node \
  java_global_trees[JTI_FINALIZE_IDENTIFIER_NODE]  /* "finalize" */
#define this_identifier_node \
  java_global_trees[JTI_THIS_IDENTIFIER_NODE]  /* "this" */
#define super_identifier_node \
  java_global_trees[JTI_SUPER_IDENTIFIER_NODE]  /* "super" */
#define continue_identifier_node \
  java_global_trees[JTI_CONTINUE_IDENTIFIER_NODE]  /* "continue" */
#define access0_identifier_node \
  java_global_trees[JTI_ACCESS0_IDENTIFIER_NODE] /* "access$0" */
#define classdollar_identifier_node \
  java_global_trees[JTI_CLASSDOLLAR_IDENTIFIER_NODE] /* "class$" */
#define one_elt_array_domain_type \
  java_global_trees[JTI_ONE_ELT_ARRAY_DOMAIN_TYPE]
/* The type of the return address of a subroutine. */
#define return_address_type_node \
  java_global_trees[JTI_RETURN_ADDRESS_TYPE_NODE]

/* Integer constants not declared in tree.h. */
#define long_zero_node \
  java_global_trees[JTI_LONG_ZERO_NODE]
#define float_zero_node \
  java_global_trees[JTI_FLOAT_ZERO_NODE]
#define double_zero_node \
  java_global_trees[JTI_DOUBLE_ZERO_NODE]
#define integer_two_node \
  java_global_trees[JTI_INTEGER_TWO_NODE]
#define integer_four_node \
  java_global_trees[JTI_INTEGER_FOUR_NODE]

/* The type for struct methodtable. */
#define methodtable_type \
  java_global_trees[JTI_METHODTABLE_TYPE]
#define methodtable_ptr_type \
  java_global_trees[JTI_METHODTABLE_PTR_TYPE]

#define utf8const_type \
  java_global_trees[JTI_UTF8CONST_TYPE]
#define utf8const_ptr_type \
  java_global_trees[JTI_UTF8CONST_PTR_TYPE]

#define class_type_node \
  java_global_trees[JTI_CLASS_TYPE_NODE]
#define class_ptr_type \
  java_global_trees[JTI_CLASS_PTR_TYPE]
#define field_type_node \
  java_global_trees[JTI_FIELD_TYPE_NODE]
#define constants_type_node \
  java_global_trees[JTI_CONSTANTS_TYPE_NODE]
#define dtable_type \
  java_global_trees[JTI_DTABLE_TYPE]
#define dtable_ptr_type \
  java_global_trees[JTI_DTABLE_PTR_TYPE]
#define field_ptr_type_node \
  java_global_trees[JTI_FIELD_PTR_TYPE_NODE]
#define field_info_union_node \
  java_global_trees[JTI_FIELD_INFO_UNION_NODE]
#define jexception_type \
  java_global_trees[JTI_EXCEPTION_TYPE]
#define jexception_ptr_type \
  java_global_trees[JTI_EXCEPTION_PTR_TYPE]
#define lineNumberEntry_type \
  java_global_trees[JTI_LINENUMBERENTRY_TYPE]
#define lineNumbers_type \
  java_global_trees[JTI_LINENUMBERS_TYPE]
#define method_type_node \
  java_global_trees[JTI_METHOD_TYPE_NODE]
#define method_ptr_type_node \
  java_global_trees[JTI_METHOD_PTR_TYPE_NODE]
#define otable_type \
  java_global_trees[JTI_OTABLE_TYPE]
#define atable_type \
  java_global_trees[JTI_ATABLE_TYPE]
#define itable_type \
  java_global_trees[JTI_ITABLE_TYPE]
#define otable_ptr_type \
  java_global_trees[JTI_OTABLE_PTR_TYPE]
#define atable_ptr_type \
  java_global_trees[JTI_ATABLE_PTR_TYPE]
#define itable_ptr_type \
  java_global_trees[JTI_ITABLE_PTR_TYPE]
#define symbol_type \
  java_global_trees[JTI_SYMBOL_TYPE]
#define symbols_array_type \
  java_global_trees[JTI_SYMBOLS_ARRAY_TYPE]
#define symbols_array_ptr_type \
  java_global_trees[JTI_SYMBOLS_ARRAY_PTR_TYPE]  
#define assertion_entry_type \
  java_global_trees[JTI_ASSERTION_ENTRY_TYPE]
#define assertion_table_type \
  java_global_trees[JTI_ASSERTION_TABLE_TYPE]

#define end_params_node \
  java_global_trees[JTI_END_PARAMS_NODE]

/* References to internal libjava functions we use. */
#define throw_node \
  java_global_trees[JTI_THROW_NODE]
#define alloc_object_node \
  java_global_trees[JTI_ALLOC_OBJECT_NODE]
#define alloc_no_finalizer_node \
  java_global_trees[JTI_ALLOC_NO_FINALIZER_NODE]
#define soft_instanceof_node \
  java_global_trees[JTI_SOFT_INSTANCEOF_NODE]
#define soft_checkcast_node \
  java_global_trees[JTI_SOFT_CHECKCAST_NODE]
#define soft_initclass_node \
  java_global_trees[JTI_SOFT_INITCLASS_NODE]
#define soft_newarray_node \
  java_global_trees[JTI_SOFT_NEWARRAY_NODE]
#define soft_anewarray_node \
  java_global_trees[JTI_SOFT_ANEWARRAY_NODE]
#define soft_multianewarray_node \
  java_global_trees[JTI_SOFT_MULTIANEWARRAY_NODE]
#define soft_badarrayindex_node \
  java_global_trees[JTI_SOFT_BADARRAYINDEX_NODE]
#define soft_nullpointer_node \
  java_global_trees[JTI_SOFT_NULLPOINTER_NODE]
#define soft_abstractmethod_node \
  java_global_trees[JTI_SOFT_ABSTRACTMETHOD_NODE]
#define soft_nosuchfield_node \
  java_global_trees[JTI_SOFT_NOSUCHFIELD_NODE]
#define soft_checkarraystore_node \
  java_global_trees[JTI_SOFT_CHECKARRAYSTORE_NODE]
#define soft_monitorenter_node \
  java_global_trees[JTI_SOFT_MONITORENTER_NODE]
#define soft_monitorexit_node \
  java_global_trees[JTI_SOFT_MONITOREXIT_NODE]
#define soft_lookupinterfacemethod_node \
  java_global_trees[JTI_SOFT_LOOKUPINTERFACEMETHOD_NODE]
#define soft_lookupinterfacemethodbyname_node \
  java_global_trees[JTI_SOFT_LOOKUPINTERFACEMETHODBYNAME_NODE]
#define soft_lookupjnimethod_node \
  java_global_trees[JTI_SOFT_LOOKUPJNIMETHOD_NODE]
#define soft_getjnienvnewframe_node \
  java_global_trees[JTI_SOFT_GETJNIENVNEWFRAME_NODE]
#define soft_jnipopsystemframe_node \
  java_global_trees[JTI_SOFT_JNIPOPSYSTEMFRAME_NODE]
#define soft_unwrapjni_node \
  java_global_trees[JTI_SOFT_UNWRAPJNI_NODE]
#define soft_fmod_node \
  java_global_trees[JTI_SOFT_FMOD_NODE]
#define soft_idiv_node \
  java_global_trees[JTI_SOFT_IDIV_NODE]
#define soft_irem_node \
  java_global_trees[JTI_SOFT_IREM_NODE]
#define soft_ldiv_node \
  java_global_trees[JTI_SOFT_LDIV_NODE]
#define soft_lrem_node \
  java_global_trees[JTI_SOFT_LREM_NODE]

#define access_flags_type_node \
  java_global_trees[JTI_ACCESS_FLAGS_TYPE_NODE]

#define nativecode_ptr_array_type_node \
  java_global_trees[JTI_NATIVECODE_PTR_ARRAY_TYPE_NODE]

#define predef_filenames \
  java_global_trees[JTI_PREDEF_FILENAMES]

#define nativecode_ptr_type_node ptr_type_node

#define wfl_operator \
  java_global_trees[JTI_WFL_OPERATOR]

/* The decl for "_Jv_ResolvePoolEntry".  */
extern GTY(()) tree soft_resolvepoolentry_node;

extern const char *cyclic_inheritance_report;

struct lang_identifier GTY(())
{
  struct tree_identifier ignore;
  tree global_value;
  tree local_value;

  /* If non-NULL:  An ADDR_REF to a VAR_DECL that contains
   * the Utf8Const representation of the identifier.  */
  tree utf8_ref;
};

/* The resulting tree type.  */
union lang_tree_node 
  GTY((desc ("TREE_CODE (&%h.generic) == IDENTIFIER_NODE"),
       chain_next ("(union lang_tree_node *)TREE_CHAIN (&%h.generic)")))
{
  union tree_node GTY ((tag ("0"), 
			desc ("tree_node_structure (&%h)"))) 
    generic;
  struct lang_identifier GTY ((tag ("1"))) identifier;
};

/* Macros for access to language-specific slots in an identifier.  */
/* Unless specified, each of these slots contains a DECL node or null.  */

/* This represents the value which the identifier has in the
   file-scope namespace.  */
#define IDENTIFIER_GLOBAL_VALUE(NODE)   \
  (((struct lang_identifier *)(NODE))->global_value)
/* This represents the value which the identifier has in the current
   scope.  */
#define IDENTIFIER_LOCAL_VALUE(NODE)    \
  (((struct lang_identifier *)(NODE))->local_value)

/* Given an identifier NODE, get the corresponding class.
   E.g. IDENTIFIER_CLASS_VALUE(get_identifier ("java.lang.Number"))
   is the corresponding RECORD_TYPE. */
#define IDENTIFIER_CLASS_VALUE(NODE) IDENTIFIER_GLOBAL_VALUE(NODE)

/* Given a signature of a reference (or array) type, or a method, return the
   corresponding type (if one has been allocated).
   Do not use for primitive types, since they may be ambiguous.
   (E.g. is "I" a signature or a class name?) */
#define IDENTIFIER_SIGNATURE_TYPE(NODE) IDENTIFIER_GLOBAL_VALUE(NODE)

/* If non-NULL:  An ADDR_REF to a VAR_DECL that contains
   the Utf8Const representation of the identifier.  */
#define IDENTIFIER_UTF8_REF(NODE) \
  (((struct lang_identifier *)(NODE))->utf8_ref)

#define IDENTIFIER_UTF8_DECL(NODE) \
  TREE_OPERAND((((struct lang_identifier *)(NODE))->utf8_ref), 0)

/* For a FUNCTION_DECL, if we are compiling a .class file, then this is
   the position in the .class file of the method code.
   Specifically, this is the code itself, not the code attribute. */
#define DECL_CODE_OFFSET(DECL) (DECL_LANG_SPECIFIC(DECL)->u.f.code_offset)
/* Similarly, the length of the bytecode. */
#define DECL_CODE_LENGTH(DECL) (DECL_LANG_SPECIFIC(DECL)->u.f.code_length)
/* Similarly, the position of the LineNumberTable attribute. */
#define DECL_LINENUMBERS_OFFSET(DECL) \
  (DECL_LANG_SPECIFIC(DECL)->u.f.linenumbers_offset)
/* Similarly, the position of the LocalVariableTable attribute
   (following the standard attribute header). */
#define DECL_LOCALVARIABLES_OFFSET(DECL) \
  (DECL_LANG_SPECIFIC(DECL)->u.f.localvariables_offset)

#define DECL_MAX_LOCALS(DECL) (DECL_LANG_SPECIFIC(DECL)->u.f.max_locals)
#define DECL_MAX_STACK(DECL) (DECL_LANG_SPECIFIC(DECL)->u.f.max_stack)
/* Number of local variable slots needed for the arguments of this function. */
#define DECL_ARG_SLOT_COUNT(DECL) \
  (DECL_LANG_SPECIFIC(DECL)->u.f.arg_slot_count)
/* Source location of end of function. */
#define DECL_FUNCTION_LAST_LINE(DECL) (DECL_LANG_SPECIFIC(DECL)->u.f.last_line)
/* Information on declaration location */
#define DECL_FUNCTION_WFL(DECL)  (DECL_LANG_SPECIFIC(DECL)->u.f.wfl)
/* List of checked thrown exceptions, as specified with the `throws'
   keyword */
#define DECL_FUNCTION_THROWS(DECL) (DECL_LANG_SPECIFIC(DECL)->u.f.throws_list)
/* List of other constructors of the same class that this constructor
   calls */
#define DECL_CONSTRUCTOR_CALLS(DECL) \
  (DECL_LANG_SPECIFIC(DECL)->u.f.called_constructor)
/* When the function is an access function, the DECL it was trying to
   access */
#define DECL_FUNCTION_ACCESS_DECL(DECL) \
  (DECL_LANG_SPECIFIC(DECL)->u.f.called_constructor)
/* The identifier of the access method used to invoke this method from
   an inner class.  */
#define DECL_FUNCTION_INNER_ACCESS(DECL) \
  (DECL_LANG_SPECIFIC(DECL)->u.f.inner_access)
/* Pointer to the function's current's COMPOUND_EXPR tree (while
   completing its body) or the function's block */
#define DECL_FUNCTION_BODY(DECL) \
  (DECL_LANG_SPECIFIC(DECL)->u.f.function_decl_body)
/* How specific the function is (for method selection - Java source
   code front-end */
#define DECL_SPECIFIC_COUNT(DECL) DECL_ARG_SLOT_COUNT(DECL)
/* For each function decl, init_test_table contains a hash table whose
   entries are keyed on class names, and whose values are local
   boolean decls.  The variables are intended to be TRUE when the
   class has been initialized in this function, and FALSE otherwise.  */
#define DECL_FUNCTION_INIT_TEST_TABLE(DECL) \
  (DECL_LANG_SPECIFIC(DECL)->u.f.init_test_table)
/* If LOCAL_CLASS_INITIALIZATION_FLAG_P(decl), give class it initializes. */
#define DECL_FUNCTION_INIT_TEST_CLASS(DECL) \
  (DECL_LANG_SPECIFIC(DECL)->u.v.slot_chain)
/* For each static function decl, itc contains a hash table whose
   entries are keyed on class named that are definitively initialized
   in DECL.  */
#define DECL_FUNCTION_INITIALIZED_CLASS_TABLE(DECL) \
  (DECL_LANG_SPECIFIC(DECL)->u.f.ict)
/* A list of all the static method calls in the method DECL (if optimizing).
   Actually each TREE_VALUE points to a COMPONT_EXPR that wraps the
   invocation so we can later patch it.  */
#define DECL_FUNCTION_STATIC_METHOD_INVOCATION_COMPOUND(DECL) \
  (DECL_LANG_SPECIFIC(DECL)->u.f.smic)
/* The Number of Artificial Parameters (NAP) DECL contains. this$<n>
   is excluded, because sometimes created as a parameter before the
   function decl exists. */
#define DECL_FUNCTION_NAP(DECL) (DECL_LANG_SPECIFIC(DECL)->u.f.nap)
/* True if DECL is a synthetic ctor.  */
#define DECL_FUNCTION_SYNTHETIC_CTOR(DECL) \
  (DECL_LANG_SPECIFIC(DECL)->u.f.synthetic_ctor)
#define DECL_FIXED_CONSTRUCTOR_P(DECL) \
  (DECL_LANG_SPECIFIC(DECL)->u.f.fixed_ctor)

#define DECL_LOCAL_CNI_METHOD_P(NODE) \
    (DECL_LANG_SPECIFIC (NODE)->u.f.local_cni)

/* A constructor that calls this. */
#define DECL_INIT_CALLS_THIS(DECL) \
  (DECL_LANG_SPECIFIC(DECL)->u.f.init_calls_this)

/* True when DECL (a field) is Synthetic.  */
#define FIELD_SYNTHETIC(DECL) DECL_LANG_FLAG_2 (VAR_OR_FIELD_CHECK (DECL))

/* True when DECL aliases an outer context local variable.  */
#define FIELD_LOCAL_ALIAS(DECL) DECL_LANG_FLAG_6 (VAR_OR_FIELD_CHECK (DECL))

/* True when DECL, which aliases an outer context local variable is
   used by the inner classe */
#define FIELD_LOCAL_ALIAS_USED(DECL) DECL_LANG_FLAG_7 (VAR_OR_FIELD_CHECK (DECL))

/* True when DECL is a this$<n> field. Note that
   FIELD_LOCAL_ALIAS_USED can be differentiated when tested against
   FIELD_LOCAL_ALIAS.  */
#define FIELD_THISN(DECL) DECL_LANG_FLAG_7 (VAR_OR_FIELD_CHECK (DECL))

/* In a LABEL_DECL, a TREE_VEC that saves the type_map at that point. */
#define LABEL_TYPE_STATE(NODE) (LABEL_DECL_CHECK (NODE)->label_decl.java_field_1)

/* In a LABEL_DECL, the corresponding bytecode program counter. */
#define LABEL_PC(NODE) (LABEL_DECL_CHECK (NODE)->label_decl.java_field_4)

/* In a LABEL_DECL, true if we have verified instructions starting here. */
#define LABEL_VERIFIED(NODE) \
  (instruction_bits[LABEL_PC (NODE)] & BCODE_VERIFIED)

/* The slot number for this local variable. */
#define DECL_LOCAL_SLOT_NUMBER(NODE) \
  (DECL_LANG_SPECIFIC (NODE)->u.v.slot_number)
/* The start (bytecode) pc for the valid range of this local variable. */
#define DECL_LOCAL_START_PC(NODE)  (DECL_LANG_SPECIFIC (NODE)->u.v.start_pc)
/* The end (bytecode) pc for the valid range of this local variable. */
#define DECL_LOCAL_END_PC(NODE)    (DECL_LANG_SPECIFIC (NODE)->u.v.end_pc)
/* For a VAR_DECL or PARM_DECL, used to chain decls with the same
   slot_number in decl_map. */
#define DECL_LOCAL_SLOT_CHAIN(NODE) (DECL_LANG_SPECIFIC(NODE)->u.v.slot_chain)
/* For a FIELD_DECL, holds the name of the access method. Used to
   read/write the content of the field across nested class boundaries.  */
#define FIELD_NESTED_ACCESS(DECL) \
  (DECL_LANG_SPECIFIC (VAR_OR_FIELD_CHECK (DECL))->u.v.am)
/* Safely tests whether FIELD_NESTED_ACCESS exists or not.  */
#define FIELD_NESTED_ACCESS_P(DECL) \
  DECL_LANG_SPECIFIC (DECL) && FIELD_NESTED_ACCESS (DECL)
/* True if a final field was initialized upon its declaration
   or in an initializer.  Set after definite assignment.  */
#define DECL_FIELD_FINAL_IUD(NODE)  (DECL_LANG_SPECIFIC (NODE)->u.v.final_iud)
/* The original WFL of a final variable. */
#define DECL_FIELD_FINAL_WFL(NODE)  (DECL_LANG_SPECIFIC(NODE)->u.v.wfl)
/* The class that's the owner of a dynamic binding table.  */
#define DECL_OWNER(NODE)            (DECL_LANG_SPECIFIC(NODE)->u.v.owner)
/* True if NODE is a local variable final. */
#define LOCAL_FINAL_P(NODE) (DECL_LANG_SPECIFIC (NODE) && DECL_FINAL (NODE))
/* True if a final local variable was initialized upon its declaration.  */
#define DECL_LOCAL_FINAL_IUD(NODE)  (DECL_LANG_SPECIFIC (NODE)->u.v.final_iud)
/* True if NODE is a final field. */
#define FINAL_VARIABLE_P(NODE) (FIELD_FINAL (NODE) && !FIELD_STATIC (NODE))
/* True if NODE is a class final field. */
#define CLASS_FINAL_VARIABLE_P(NODE) \
  (FIELD_FINAL (NODE) && FIELD_STATIC (NODE))
/* True if NODE is a class initialization flag. This macro accesses
   the flag to read or set it.  */
#define LOCAL_CLASS_INITIALIZATION_FLAG(NODE) \
    (DECL_LANG_SPECIFIC (NODE)->u.v.cif)
/* True if NODE is a class initialization flag. */
#define LOCAL_CLASS_INITIALIZATION_FLAG_P(NODE) \
    (DECL_LANG_SPECIFIC (NODE) && LOCAL_CLASS_INITIALIZATION_FLAG(NODE))
/* True if NODE is a variable that is out of scope.  */
#define LOCAL_VAR_OUT_OF_SCOPE_P(NODE) \
    (DECL_LANG_SPECIFIC (NODE)->u.v.freed)
#define LOCAL_SLOT_P(NODE) \
    (DECL_LANG_SPECIFIC (NODE)->u.v.local_slot)

#define DECL_CLASS_FIELD_P(NODE) \
    (DECL_LANG_SPECIFIC (NODE)->u.v.class_field)
#define DECL_VTABLE_P(NODE) \
    (DECL_LANG_SPECIFIC (NODE)->u.v.vtable)

/* Create a DECL_LANG_SPECIFIC if necessary. */
#define MAYBE_CREATE_VAR_LANG_DECL_SPECIFIC(T)			\
  if (DECL_LANG_SPECIFIC (T) == NULL)				\
    {								\
      DECL_LANG_SPECIFIC ((T))					\
	= ggc_alloc_cleared (sizeof (struct lang_decl));	\
      DECL_LANG_SPECIFIC (T)->desc = LANG_DECL_VAR;		\
    }

/* A ConstantExpression, after folding and name resolution. */
#define CONSTANT_VALUE_P(NODE) \
  (TREE_CODE (NODE) == STRING_CST \
   || (TREE_CODE (NODE) == INTEGER_CST \
       && TREE_CODE (TREE_TYPE (NODE)) != POINTER_TYPE) \
   || TREE_CODE (NODE) == REAL_CST)

/* For a local VAR_DECL or PARM_DECL, holds the index into a words bitstring
   that specifies if this decl is definitively assigned.
   The value -1 means the variable has been definitely assigned (and not
   definitely unassigned).  The value -2 means we already reported an error. */
#define DECL_BIT_INDEX(DECL) VAR_OR_FIELD_CHECK (DECL)->decl_common.pointer_alias_set

/* DECL_LANG_SPECIFIC for FUNCTION_DECLs. */
struct lang_decl_func GTY(())
{
  /*  tree chain; not yet used. */
  long code_offset;
  int code_length;
  long linenumbers_offset;
  long localvariables_offset;
  int arg_slots;
  int max_locals;
  int max_stack;
  int arg_slot_count;
  /* A temporary lie for the sake of ggc.  Actually, last_line is
     only a source_location if USE_MAPPED_LOCATION.  FIXME.  */
  source_location last_line;	/* End line number for a function decl */
  tree wfl;			/* Information on the original location */
  tree throws_list;		/* Exception specified by `throws' */
  tree function_decl_body;	/* Hold all function's statements */
  tree called_constructor;	/* When decl is a constructor, the
				   list of other constructor it calls */

  /* Class initialization test variables  */
  htab_t GTY ((param_is (struct treetreehash_entry))) init_test_table;
				
  /* Initialized (static) Class Table */
  htab_t GTY ((param_is (union tree_node))) ict;

  tree smic;			/* Static method invocation compound */
  tree inner_access;		/* The identifier of the access method
				   used for invocation from inner classes */
  int nap;			/* Number of artificial parameters */
  unsigned int native : 1;	/* Nonzero if this is a native method  */
  unsigned int synthetic_ctor : 1; /* Nonzero if this is a synthetic ctor */
  unsigned int init_final : 1;	/* Nonzero all finals are initialized */
  unsigned int fixed_ctor : 1;
  unsigned int init_calls_this : 1;
  unsigned int strictfp : 1;
  unsigned int invisible : 1;	/* Set for methods we generate
				   internally but which shouldn't be
				   written to the .class file.  */
  unsigned int dummy : 1;
  unsigned int local_cni : 1;	/* Decl needs mangle_local_cni_method.  */
};

struct treetreehash_entry GTY(())
{
  tree key;
  tree value;
};

/* These represent the possible assertion_code's that can be emitted in the
   type assertion table.  */
enum
{
  JV_ASSERT_END_OF_TABLE = 0,     /* Last entry in table.  */
  JV_ASSERT_TYPES_COMPATIBLE = 1, /* Operand A is assignable to Operand B.  */
  JV_ASSERT_IS_INSTANTIABLE = 2   /* Operand A is an instantiable class.  */
};

typedef struct type_assertion GTY(())
{
  int assertion_code; /* 'opcode' for the type of this assertion. */
  tree op1;           /* First operand. */
  tree op2;           /* Second operand. */
} type_assertion;

extern tree java_treetreehash_find (htab_t, tree);
extern tree * java_treetreehash_new (htab_t, tree);
extern htab_t java_treetreehash_create (size_t size, int ggc);

/* DECL_LANG_SPECIFIC for VAR_DECL, PARM_DECL and sometimes FIELD_DECL
   (access methods on outer class fields) and final fields. */
struct lang_decl_var GTY(())
{
  int slot_number;
  int start_pc;
  int end_pc;
  tree slot_chain;
  tree am;			/* Access method for this field (1.1) */
  tree wfl;			/* Original wfl */
  tree owner;
  unsigned int final_iud : 1;	/* Final initialized upon declaration */
  unsigned int cif : 1;		/* True: decl is a class initialization flag */
  unsigned int freed : 1;		/* Decl is no longer in scope.  */
  unsigned int local_slot : 1;	/* Decl is a temporary in the stack frame.  */
  unsigned int class_field : 1; /* Decl needs mangle_class_field.  */
  unsigned int vtable : 1;	/* Decl needs mangle_vtable.  */
};

/* This is what 'lang_decl' really points to.  */

enum lang_decl_desc {LANG_DECL_FUNC, LANG_DECL_VAR};

struct lang_decl GTY(())
{
  enum lang_decl_desc desc;
  union lang_decl_u
    {
      struct lang_decl_func GTY ((tag ("LANG_DECL_FUNC"))) f;
      struct lang_decl_var GTY ((tag ("LANG_DECL_VAR"))) v;
    } GTY ((desc ("%0.desc"))) u;
};

/* Macro to access fields in `struct lang_type'.  */

#define TYPE_SIGNATURE(T)	(TYPE_LANG_SPECIFIC (T)->signature)
#define TYPE_JCF(T)		(TYPE_LANG_SPECIFIC (T)->jcf)
#define TYPE_CPOOL(T)		(TYPE_LANG_SPECIFIC (T)->cpool)
#define TYPE_CPOOL_DATA_REF(T)	(TYPE_LANG_SPECIFIC (T)->cpool_data_ref)
#define MAYBE_CREATE_TYPE_TYPE_LANG_SPECIFIC(T) \
  if (TYPE_LANG_SPECIFIC ((T)) == NULL)		\
     TYPE_LANG_SPECIFIC ((T))			\
     = ggc_alloc_cleared (sizeof (struct lang_type));

#define TYPE_FINIT_STMT_LIST(T)  (TYPE_LANG_SPECIFIC (T)->finit_stmt_list)
#define TYPE_CLINIT_STMT_LIST(T) (TYPE_LANG_SPECIFIC (T)->clinit_stmt_list)
#define TYPE_II_STMT_LIST(T)     (TYPE_LANG_SPECIFIC (T)->ii_block)
/* The decl of the synthetic method `class$' used to handle `.class'
   for non primitive types when compiling to bytecode. */

#define TYPE_DUMMY(T)		(TYPE_LANG_SPECIFIC(T)->dummy_class)

#define TYPE_DOT_CLASS(T)        (TYPE_LANG_SPECIFIC (T)->dot_class)
#define TYPE_PACKAGE_LIST(T)     (TYPE_LANG_SPECIFIC (T)->package_list)
#define TYPE_IMPORT_LIST(T)      (TYPE_LANG_SPECIFIC (T)->import_list)
#define TYPE_IMPORT_DEMAND_LIST(T) (TYPE_LANG_SPECIFIC (T)->import_demand_list)
#define TYPE_PRIVATE_INNER_CLASS(T) (TYPE_LANG_SPECIFIC (T)->pic)
#define TYPE_PROTECTED_INNER_CLASS(T) (TYPE_LANG_SPECIFIC (T)->poic)
#define TYPE_STRICTFP(T) (TYPE_LANG_SPECIFIC (T)->strictfp)
#define TYPE_USES_ASSERTIONS(T) (TYPE_LANG_SPECIFIC (T)->assertions)

#define TYPE_ATABLE_METHODS(T)   (TYPE_LANG_SPECIFIC (T)->atable_methods)
#define TYPE_ATABLE_SYMS_DECL(T) (TYPE_LANG_SPECIFIC (T)->atable_syms_decl)
#define TYPE_ATABLE_DECL(T)      (TYPE_LANG_SPECIFIC (T)->atable_decl)

#define TYPE_OTABLE_METHODS(T)   (TYPE_LANG_SPECIFIC (T)->otable_methods)
#define TYPE_OTABLE_SYMS_DECL(T) (TYPE_LANG_SPECIFIC (T)->otable_syms_decl)
#define TYPE_OTABLE_DECL(T)      (TYPE_LANG_SPECIFIC (T)->otable_decl)

#define TYPE_ITABLE_METHODS(T)   (TYPE_LANG_SPECIFIC (T)->itable_methods)
#define TYPE_ITABLE_SYMS_DECL(T) (TYPE_LANG_SPECIFIC (T)->itable_syms_decl)
#define TYPE_ITABLE_DECL(T)      (TYPE_LANG_SPECIFIC (T)->itable_decl)

#define TYPE_CTABLE_DECL(T)      (TYPE_LANG_SPECIFIC (T)->ctable_decl)
#define TYPE_CATCH_CLASSES(T)    (TYPE_LANG_SPECIFIC (T)->catch_classes)
#define TYPE_VERIFY_METHOD(T)    (TYPE_LANG_SPECIFIC (T)->verify_method)

#define TYPE_TO_RUNTIME_MAP(T)   (TYPE_LANG_SPECIFIC (T)->type_to_runtime_map)
#define TYPE_ASSERTIONS(T)   	 (TYPE_LANG_SPECIFIC (T)->type_assertions)
#define TYPE_PACKAGE(T)     	 (TYPE_LANG_SPECIFIC (T)->package)

struct lang_type GTY(())
{
  tree signature;
  struct JCF *jcf;
  struct CPool *cpool;
  tree cpool_data_ref;		/* Cached */
  tree finit_stmt_list;		/* List of statements finit$ will use */
  tree clinit_stmt_list;	/* List of statements <clinit> will use  */
  tree ii_block;		/* Instance initializer block */
  tree dot_class;		/* The decl of the `class$' function that
				   needs to be invoked and generated when
				   compiling to bytecode to implement
				   <non_primitive_type>.class */
  tree package_list;		/* List of package names, progressive */
  tree import_list;		/* Imported types, in the CU of this class */
  tree import_demand_list;	/* Imported types, in the CU of this class */

  tree otable_methods;          /* List of static decls referred to by this
				   class.  */
  tree otable_decl;		/* The static address table.  */
  tree otable_syms_decl;

  tree atable_methods;          /* List of static decls referred to by this
				   class.  */
  tree atable_decl;		/* The static address table.  */
  tree atable_syms_decl;

  tree itable_methods;          /* List of interfaces methods referred
				   to by this class.  */
  tree itable_decl;		/* The interfaces table.  */
  tree itable_syms_decl;

  tree ctable_decl;             /* The table of classes for the runtime
				   type matcher.  */
  tree catch_classes;

  tree verify_method;		/* The verify method for this class.
				   Used in split verification.  */

  htab_t GTY ((param_is (struct treetreehash_entry))) type_to_runtime_map;   
                                /* The mapping of classes to exception region
				   markers.  */

  htab_t GTY ((param_is (struct type_assertion))) type_assertions;
				/* Table of type assertions to be evaluated 
  				   by the runtime when this class is loaded. */

  tree package;			/* IDENTIFIER_NODE for package this class is
  				   a member of.  */

  unsigned pic:1;		/* Private Inner Class. */
  unsigned poic:1;		/* Protected Inner Class. */
  unsigned strictfp:1;		/* `strictfp' class.  */
  unsigned assertions:1;	/* Any method uses `assert'.  */
  unsigned dummy_class:1;		/* Not a real class, just a placeholder.  */
};

#define JCF_u4 unsigned long
#define JCF_u2 unsigned short

/* Possible values to pass to lookup_argument_method_generic.  */
#define SEARCH_INTERFACE      1
#define SEARCH_SUPER          2
#define SEARCH_VISIBLE        4

/* Defined in java-except.h  */
struct eh_range;

extern void java_parse_file (int);
extern bool java_mark_addressable (tree);
extern tree java_type_for_mode (enum machine_mode, int);
extern tree java_type_for_size (unsigned int, int);
extern tree java_unsigned_type (tree);
extern tree java_signed_type (tree);
extern tree java_signed_or_unsigned_type (int, tree);
extern tree java_truthvalue_conversion (tree);
extern void add_assume_compiled (const char *, int);
extern void add_enable_assert (const char *, int);
extern bool enable_assertions (tree);
extern tree lookup_class (tree);
extern tree lookup_java_constructor (tree, tree);
extern tree lookup_java_method (tree, tree, tree);
extern tree lookup_argument_method (tree, tree, tree);
extern tree lookup_argument_method_generic (tree, tree, tree, int);
extern int has_method (tree, tree);
extern tree promote_type (tree);
extern tree get_constant (struct JCF*, int);
extern tree get_name_constant (struct JCF*, int);
extern tree get_class_constant (struct JCF*, int);
extern tree parse_signature (struct JCF *jcf, int sig_index);
extern tree add_field (tree, tree, tree, int);
extern tree add_method (tree, int, tree, tree);
extern tree add_method_1 (tree, int, tree, tree);
extern tree make_class (void);
extern tree push_class (tree, tree);
extern tree unmangle_classname (const char *name, int name_length);
extern tree parse_signature_string (const unsigned char *, int);
extern tree get_type_from_signature (tree);
extern void layout_class (tree);
extern int get_interface_method_index (tree, tree);
extern tree layout_class_method (tree, tree, tree, tree);
extern void layout_class_methods (tree);
extern tree build_class_ref (tree);
extern tree build_dtable_decl (tree);
extern tree build_internal_class_name (tree);
extern tree build_constants_constructor (void);
extern tree build_ref_from_constant_pool (int);
extern tree build_utf8_ref (tree);
extern tree ident_subst (const char *, int, const char *, int, int,
			 const char *);
extern tree identifier_subst (const tree, const char *, int, int,
			      const char *);
extern int global_bindings_p (void);
extern tree getdecls (void);
extern void pushlevel (int);
extern tree poplevel (int,int, int);
extern void insert_block (tree);
extern tree pushdecl (tree);
extern void java_init_decl_processing (void);
extern void java_dup_lang_specific_decl (tree);
extern tree build_java_signature (tree);
extern tree build_java_argument_signature (tree);
extern void set_java_signature (tree, tree);
extern tree build_static_field_ref (tree);
extern tree build_address_of (tree);
extern tree find_local_variable (int index, tree type, int pc);
extern void update_aliases (tree decl, int index, int pc);
extern tree find_stack_slot (int index, tree type);
extern tree build_prim_array_type (tree, HOST_WIDE_INT);
extern tree build_java_array_type (tree, HOST_WIDE_INT);
extern int is_compiled_class (tree);
extern tree mangled_classname (const char *, tree);
extern tree lookup_label (int);
extern tree pop_type_0 (tree, char **);
extern tree pop_type (tree);
extern tree decode_newarray_type (int);
extern tree lookup_field (tree *, tree);
extern int is_array_type_p (tree);
extern HOST_WIDE_INT java_array_type_length (tree);
extern int read_class (tree);
extern void load_class (tree, int);

extern tree check_for_builtin (tree, tree);
extern void initialize_builtins (void);

extern tree lookup_name (tree);
extern tree build_known_method_ref (tree, tree, tree, tree, tree);
extern tree build_class_init (tree, tree);
extern int attach_init_test_initialization_flags (void **, void *);
extern tree build_invokevirtual (tree, tree);
extern tree build_invokeinterface (tree, tree);
extern tree build_jni_stub (tree);
extern tree invoke_build_dtable (int, tree);
extern tree build_field_ref (tree, tree, tree);
extern void pushdecl_force_head (tree);
extern tree build_java_binop (enum tree_code, tree, tree, tree);
extern tree build_java_soft_divmod (enum tree_code, tree, tree, tree);
extern tree binary_numeric_promotion (tree, tree, tree *, tree *);
extern tree build_java_arrayaccess (tree, tree, tree);
extern tree build_java_arraystore_check (tree, tree);
extern tree build_newarray (int, tree);
extern tree build_anewarray (tree, tree);
extern tree build_new_array (tree, tree);
extern tree build_java_array_length_access (tree);
extern tree build_java_indirect_ref (tree, tree, int);
extern tree java_check_reference (tree, int);
extern tree build_get_class (tree);
extern tree build_instanceof (tree, tree);
extern tree create_label_decl (tree);
extern void push_labeled_block (tree);
extern tree prepare_eh_table_type (tree);
extern void java_expand_catch_classes (tree);
extern tree build_exception_object_ref (tree);
extern tree generate_name (void);
extern void pop_labeled_block (void);
extern const char *lang_printable_name (tree, int);
extern tree maybe_add_interface (tree, tree);
extern void set_super_info (int, tree, tree, int);
extern void set_class_decl_access_flags (int, tree);
extern int get_access_flags_from_decl (tree);
extern int interface_of_p (tree, tree);
extern int inherits_from_p (tree, tree);
extern int common_enclosing_context_p (tree, tree);
extern int common_enclosing_instance_p (tree, tree);
extern int enclosing_context_p (tree, tree);
extern tree build_result_decl (tree);
extern void set_method_index (tree decl, tree method_index);
extern tree get_method_index (tree decl);
extern void make_class_data (tree);
extern int alloc_name_constant (int, tree);
extern int alloc_constant_fieldref (tree, tree);
extern void emit_register_classes (tree *);
extern tree emit_symbol_table (tree, tree, tree, tree, tree, int);
extern void lang_init_source (int);
extern void write_classfile (tree);
extern char *print_int_node (tree);
extern void finish_class (void);
extern void java_layout_seen_class_methods (void);
extern void check_for_initialization (tree, tree);

extern tree pushdecl_top_level (tree);
extern tree pushdecl_function_level (tree);
extern tree java_replace_reference (tree, bool);
extern int alloc_class_constant (tree);
extern void init_expr_processing (void);
extern void push_super_field (tree, tree);
extern void init_class_processing (void);
extern void add_type_assertion (tree, int, tree, tree);
extern int can_widen_reference_to (tree, tree);
extern int class_depth (tree);
extern int verify_jvm_instructions_new (struct JCF *, const unsigned char *,
					long);
extern void maybe_pushlevels (int);
extern void maybe_poplevels (int);
extern void force_poplevels (int);
extern int process_jvm_instruction (int, const unsigned char *, long);
extern int maybe_adjust_start_pc (struct JCF *, int, int, int);
extern void set_local_type (int, tree);
extern int merge_type_state (tree);
extern int push_type_0 (tree);
extern void push_type (tree);
extern void load_type_state (tree);
extern void add_interface (tree, tree);
extern tree force_evaluation_order (tree);
extern tree java_create_object (tree);
extern int verify_constant_pool (struct JCF *);
extern void start_java_method (tree);
extern void end_java_method (void);
extern void give_name_to_locals (struct JCF *);
extern void note_instructions (struct JCF *, tree);
extern void expand_byte_code (struct JCF *, tree);
extern int open_in_zip (struct JCF *, const char *, const char *, int);
extern void set_constant_value (tree, tree);
#ifdef jword
extern int find_constant1 (struct CPool *, int, jword);
extern int find_constant2 (struct CPool *, int, jword, jword);
#endif
extern int find_utf8_constant (struct CPool *, tree);
extern int find_string_constant (struct CPool *, tree);
extern int find_class_constant (struct CPool *, tree);
extern int find_fieldref_index (struct CPool *, tree);
extern int find_methodref_index (struct CPool *, tree);
extern int find_methodref_with_class_index (struct CPool *, tree, tree);
extern void write_constant_pool (struct CPool *, unsigned char *, int);
extern int count_constant_pool_bytes (struct CPool *);
extern int encode_newarray_type (tree);
#ifdef uint64
extern void format_int (char *, jlong, int);
extern void format_uint (char *, uint64, int);
#endif
extern void jcf_trim_old_input (struct JCF *);
#ifdef BUFSIZ
extern void jcf_print_utf8 (FILE *, const unsigned char *, int);
extern void jcf_print_char (FILE *, int);
extern void jcf_print_utf8_replace (FILE *, const unsigned char *, int,
				    int, int);
extern const char* open_class (const char *, struct JCF *, int, const char *);
#endif
extern void java_debug_context (void);
extern void safe_layout_class (tree);

extern tree get_boehm_type_descriptor (tree);
extern bool class_has_finalize_method (tree);
extern void java_check_methods (tree);
extern void init_jcf_parse (void);
extern void init_src_parse (void);

extern int cxx_keyword_p (const char *, int);
extern void java_mangle_decl (tree);
extern tree java_mangle_class_field (struct obstack *, tree);
extern tree java_mangle_vtable (struct obstack *, tree);
extern void append_gpp_mangled_name (const char *, int);

extern void add_predefined_file (tree);
extern int predefined_filename_p (tree);

extern tree decl_constant_value (tree);

extern void java_mark_class_local (tree);

extern void java_inlining_merge_static_initializers (tree, void *);
extern void java_inlining_map_static_initializers (tree, void *);

extern void compile_resource_data (const char *name, const char *buffer, int);
extern void compile_resource_file (const char *, const char *);
extern void write_resource_constructor (tree *);
extern tree build_java_empty_stmt (void);
extern tree add_stmt_to_compound (tree, tree, tree);
extern tree java_add_stmt (tree);
extern tree java_add_local_var (tree decl);
extern tree *get_stmts (void);
extern void register_exception_range(struct eh_range *, int, int);

extern void finish_method (tree);
extern void java_expand_body (tree);

extern int get_symbol_table_index (tree, tree *);

extern tree make_catch_class_record (tree, tree);
extern tree emit_catch_table (tree);

extern void gen_indirect_dispatch_tables (tree type);
extern int split_qualified_name (tree *left, tree *right, tree source);
extern int in_same_package (tree, tree);

extern tree builtin_function (const char *, tree, int, enum built_in_class,
			      const char *, tree);

#define DECL_FINAL(DECL) DECL_LANG_FLAG_3 (DECL)

/* Access flags etc for a method (a FUNCTION_DECL): */

#define METHOD_DUMMY(DECL) (DECL_LANG_SPECIFIC (DECL)->u.f.dummy)

#define METHOD_PUBLIC(DECL) DECL_LANG_FLAG_1 (FUNCTION_DECL_CHECK (DECL))
#define METHOD_PRIVATE(DECL) TREE_PRIVATE (FUNCTION_DECL_CHECK (DECL))
#define METHOD_PROTECTED(DECL) TREE_PROTECTED (FUNCTION_DECL_CHECK (DECL))
#define METHOD_STATIC(DECL) DECL_LANG_FLAG_2 (FUNCTION_DECL_CHECK (DECL))
#define METHOD_FINAL(DECL) DECL_FINAL (FUNCTION_DECL_CHECK (DECL))
#define METHOD_SYNCHRONIZED(DECL) DECL_LANG_FLAG_4 (FUNCTION_DECL_CHECK (DECL))
#define METHOD_NATIVE(DECL) \
  (DECL_LANG_SPECIFIC (FUNCTION_DECL_CHECK (DECL))->u.f.native)
#define METHOD_ABSTRACT(DECL) DECL_LANG_FLAG_5 (FUNCTION_DECL_CHECK (DECL))
#define METHOD_STRICTFP(DECL) \
  (DECL_LANG_SPECIFIC (FUNCTION_DECL_CHECK (DECL))->u.f.strictfp)
#define METHOD_INVISIBLE(DECL) \
  (DECL_LANG_SPECIFIC (FUNCTION_DECL_CHECK (DECL))->u.f.invisible)

#define CLASS_FILE_P(NODE) TREE_LANG_FLAG_3 (NODE)

/* Other predicates on method decls  */

#define DECL_CONSTRUCTOR_P(DECL) DECL_LANG_FLAG_7 (FUNCTION_DECL_CHECK (DECL))

#define DECL_INIT_P(DECL)   (ID_INIT_P (DECL_NAME (DECL)))
#define DECL_FINIT_P(DECL)  (ID_FINIT_P (DECL_NAME (DECL)))
#define DECL_CLINIT_P(DECL) (ID_CLINIT_P (DECL_NAME (DECL)))
#define DECL_INSTINIT_P(DECL) (ID_INSTINIT_P (DECL_NAME (DECL)))

/* Predicates on method identifiers. Kept close to other macros using
   them  */
#define ID_INIT_P(ID)   ((ID) == init_identifier_node)
#define ID_FINIT_P(ID)  ((ID) == finit_identifier_node)
#define ID_CLINIT_P(ID) ((ID) == clinit_identifier_node)
#define ID_CLASSDOLLAR_P(ID) ((ID) == classdollar_identifier_node)
#define ID_INSTINIT_P(ID) ((ID) == instinit_identifier_node)

/* Access flags etc for variable/field (FIELD_DECL, VAR_DECL, or PARM_DECL): */

#define FIELD_PRIVATE(DECL)	TREE_PRIVATE (VAR_OR_FIELD_CHECK (DECL))
#define FIELD_PROTECTED(DECL)	TREE_PROTECTED (VAR_OR_FIELD_CHECK (DECL))
#define FIELD_PUBLIC(DECL)	DECL_LANG_FLAG_1 (VAR_OR_FIELD_CHECK (DECL))
#define FIELD_STATIC(DECL)	TREE_STATIC (VAR_OR_FIELD_CHECK (DECL))
#define FIELD_FINAL(DECL)	DECL_FINAL (VAR_OR_FIELD_CHECK (DECL))
#define FIELD_VOLATILE(DECL)	DECL_LANG_FLAG_4 (VAR_OR_FIELD_CHECK (DECL))
#define FIELD_TRANSIENT(DECL)	DECL_LANG_FLAG_5 (VAR_OR_FIELD_CHECK (DECL))

/* Access flags etc for a class (a TYPE_DECL): */

#define CLASS_PUBLIC(DECL)	DECL_LANG_FLAG_1 (TYPE_DECL_CHECK (DECL))
#define CLASS_FINAL(DECL)	DECL_FINAL (TYPE_DECL_CHECK (DECL))
#define CLASS_INTERFACE(DECL)	DECL_LANG_FLAG_4 (TYPE_DECL_CHECK (DECL))
#define CLASS_ABSTRACT(DECL)	DECL_LANG_FLAG_5 (TYPE_DECL_CHECK (DECL))
#define CLASS_SUPER(DECL)	DECL_LANG_FLAG_6 (TYPE_DECL_CHECK (DECL))
#define CLASS_STATIC(DECL)	DECL_LANG_FLAG_7 (TYPE_DECL_CHECK (DECL))
#define CLASS_PRIVATE(DECL)	(TYPE_PRIVATE_INNER_CLASS (TREE_TYPE (DECL)))
#define CLASS_PROTECTED(DECL)	(TYPE_PROTECTED_INNER_CLASS (TREE_TYPE (DECL)))
#define CLASS_STRICTFP(DECL)	(TYPE_STRICTFP (TREE_TYPE (DECL)))
#define CLASS_USES_ASSERTIONS(DECL) (TYPE_USES_ASSERTIONS (TREE_TYPE (DECL)))

/* @deprecated marker flag on methods, fields and classes */

#define METHOD_DEPRECATED(DECL) DECL_LANG_FLAG_0 (DECL)
#define FIELD_DEPRECATED(DECL) DECL_LANG_FLAG_0 (DECL)
#define CLASS_DEPRECATED(DECL) DECL_LANG_FLAG_0 (DECL)
#define DECL_DEPRECATED(DECL) DECL_LANG_FLAG_0 (DECL)

/* The number of virtual methods in this class's dispatch table.
   Does not include initial two dummy entries (one points to the
   Class object, and the other is for G++ -fvtable-thunks compatibility). */
#define TYPE_NVIRTUALS(TYPE) BINFO_VIRTUALS (TYPE_BINFO (TYPE))

/* A TREE_VEC (indexed by DECL_VINDEX) containing this class's
   virtual methods. */
#define TYPE_VTABLE(TYPE) BINFO_VTABLE(TYPE_BINFO (TYPE))

/* Use CLASS_LOADED_P? FIXME */
#define CLASS_COMPLETE_P(DECL) DECL_LANG_FLAG_2 (DECL) 

/* This maps a bytecode offset (PC) to various flags,
   listed below (starting with BCODE_). */
extern char *instruction_bits;

/* True iff the byte is the start of an instruction. */
#define BCODE_INSTRUCTION_START 1

/* True iff there is a jump or a return to this location. */
#define BCODE_JUMP_TARGET 2

/* True iff this is the start of an exception handler. */
#define BCODE_EXCEPTION_TARGET 16

/* True iff there is a jump to this location (and it needs a label). */
#define BCODE_TARGET (BCODE_JUMP_TARGET| BCODE_EXCEPTION_TARGET)

/* True iff there is an entry in the linenumber table for this location. */
#define BCODE_HAS_LINENUMBER 32

/* True iff there is more than one entry in the linenumber table for
   this location.  (This probably does not make much sense.)  */
#define BCODE_HAS_MULTI_LINENUMBERS 64

/* True if this instruction has been verified. */
#define BCODE_VERIFIED 8

/* A pointer to the line number table of the current method. */
extern const unsigned char *linenumber_table;
/* The length (in items) of the line number table. */
extern int linenumber_count;

/* In type_map, means that slot is uninitialized or otherwise unusable. */
#define TYPE_UNKNOWN NULL_TREE

/* In type_map, means the second half of a 64-bit double or long. */
#define TYPE_SECOND void_type_node

/* In type_map, means the null type (i.e. type of a null reference). */ 
#define TYPE_NULL ptr_type_node

/* In a type map means the type the address subroutine return address. */
#define TYPE_RETURN_ADDR return_address_type_node

/* In a subroutine's return type map, indicates that the slot was neither
   used nor set in the subroutine. */
#define TYPE_UNUSED error_mark_node

/* When returned from pop_type_0, indicates stack underflow. */
#define TYPE_UNDERFLOW integer_zero_node

/* When returned from pop_type_0, indicates a type mismatch. */
#define TYPE_UNEXPECTED NULL_TREE

/* A array mapping variable/stack slot index to the type current
   in that variable/stack slot.
   TYPE_UNKNOWN, TYPE_SECOND, and TYPE_NULL are special cases. */
extern tree *type_map;

/* Map a stack index to the type currently in that slot. */
#define stack_type_map (type_map + DECL_MAX_LOCALS (current_function_decl))

/* True iff TYPE takes two variable/stack slots. */
#define TYPE_IS_WIDE(TYPE) \
  ((TYPE) == double_type_node || (TYPE) == long_type_node)

/* True iif CLASS has it's access$0 method generated.  */
#define CLASS_ACCESS0_GENERATED_P(CLASS) TYPE_LANG_FLAG_0 (CLASS)

/* True iff TYPE is a Java array type. */
#define TYPE_ARRAY_P(TYPE) TYPE_LANG_FLAG_1 (TYPE)

/* True for an INDIRECT_REF created from a 'ARRAY.length' operation. */
#define IS_ARRAY_LENGTH_ACCESS(NODE) TREE_LANG_FLAG_4 (NODE)

/* If FUNCTION_TYPE or METHOD_TYPE: cache for build_java_argument_signature. */
#define TYPE_ARGUMENT_SIGNATURE(TYPE) \
  (TREE_CHECK2 (TYPE, FUNCTION_TYPE, METHOD_TYPE)->type.minval)

/* Given an array type, give the type of the elements. */
/* FIXME this use of TREE_TYPE conflicts with something or other. */
#define TYPE_ARRAY_ELEMENT(ATYPE) TREE_TYPE (ATYPE)

/* True if class TYPE has been loaded (i.e. parsed plus laid out).
   (The check for CLASS_PARSED_P is needed because of Object and Class.) */
#define CLASS_LOADED_P(TYPE) (TYPE_SIZE (TYPE) != NULL_TREE \
			      && (CLASS_PARSED_P(TYPE) || TYPE_ARRAY_P(TYPE)))

/* True if class TYPE has been parsed (first pass). */
#define CLASS_PARSED_P(TYPE) TYPE_LANG_FLAG_2 (TYPE)

/* True if class TYPE was defined in Java source code. */
#define CLASS_FROM_SOURCE_P(TYPE) TYPE_LANG_FLAG_3 (TYPE)

/* True of a RECORD_TYPE of a class/interface type (not array type) */
#define CLASS_P(TYPE) TYPE_LANG_FLAG_4 (TYPE)

/* True if class TYPE was requested (on command line) to be compiled.*/
#define CLASS_FROM_CURRENTLY_COMPILED_P(TYPE) TYPE_LANG_FLAG_5 (TYPE)

/* True if class TYPE is currently being laid out. Helps in detection
   of inheritance cycle occurring as a side effect of performing the
   layout of a class.  */
#define CLASS_BEING_LAIDOUT(TYPE) TYPE_LANG_FLAG_6 (TYPE)

/* True if class TYPE has a field initializer finit$ function */
#define CLASS_HAS_FINIT_P(TYPE) TYPE_FINIT_STMT_LIST (TYPE)

/* True if identifier ID was seen while processing a single type import stmt */
#define IS_A_SINGLE_IMPORT_CLASSFILE_NAME_P(ID) TREE_LANG_FLAG_0 (ID)

/* True if identifier ID was seen while processing an import statement */
#define IS_A_CLASSFILE_NAME(ID) TREE_LANG_FLAG_1 (ID)

/* True if ID is a qualified named (contains . or /) */
#define QUALIFIED_P(ID) TREE_LANG_FLAG_2 (ID)

/* True if ID is an already processed import on demand */
#define IS_AN_IMPORT_ON_DEMAND_P(ID) TREE_LANG_FLAG_3 (ID)

/* True if ID is a command-line specified filename */
#define IS_A_COMMAND_LINE_FILENAME_P(ID) TREE_LANG_FLAG_4 (ID)

/* True if filename ID has already been parsed */
#define HAS_BEEN_ALREADY_PARSED_P(ID) TREE_LANG_FLAG_5 (ID)

/* True if EXPR is RHS sub-tree of a compound assign expression */
#define COMPOUND_ASSIGN_P(EXPR) TREE_LANG_FLAG_1 (EXPR)

/* True if a SWITCH_EXPR has a DEFAULT_EXPR. */
#define SWITCH_HAS_DEFAULT(NODE) TREE_LANG_FLAG_3 (SWITCH_EXPR_CHECK (NODE))

/* True if EXPR (a WFL in that case) was created after the
   reduction of PRIMARY . XXX */
#define PRIMARY_P(EXPR) TREE_LANG_FLAG_2 (EXPR_CHECK (EXPR))

/* True if EXPR (a MODIFY_EXPR in that case) is the result of variable
   initialization during its declaration */
#define MODIFY_EXPR_FROM_INITIALIZATION_P(EXPR) \
  TREE_LANG_FLAG_2 (MODIFY_EXPR_CHECK (EXPR))

/* True if EXPR (a TREE_TYPE denoting a class type) has its methods
   already checked (for redefinitions, etc, see java_check_regular_methods.) */
#define CLASS_METHOD_CHECKED_P(EXPR) TREE_LANG_FLAG_2 (EXPR)

/* True if TYPE (a TREE_TYPE denoting a class type) was found to
   feature a finalizer method. */
#define HAS_FINALIZER_P(EXPR) TREE_LANG_FLAG_3 (EXPR)

/* True if EXPR (a LOOP_EXPR in that case) is part of a for statement */
#define FOR_LOOP_P(EXPR) TREE_LANG_FLAG_0 (EXPR_CHECK (EXPR))

/* True if NODE (a RECORD_TYPE in that case) is an anonymous class.  */
#define ANONYMOUS_CLASS_P(NODE) TREE_LANG_FLAG_0 (RECORD_TYPE_CHECK (NODE))

/* True if NODE (a RECORD_TYPE in that case) is a block local class.  */
#define LOCAL_CLASS_P(NODE) TREE_LANG_FLAG_1 (RECORD_TYPE_CHECK (NODE))

/* True if NODE (a TREE_LIST) hold a pair of argument name/type
   declared with the final modifier */
#define ARG_FINAL_P(NODE) TREE_LANG_FLAG_0 (TREE_LIST_CHECK (NODE))

/* True if NODE (some kind of EXPR, but not a WFL) should not give an
   error if it is found to be unreachable.  This can only be applied
   to those EXPRs which can be used as the update expression of a
   `for' loop.  In particular it can't be set on a LOOP_EXPR.  */
#define SUPPRESS_UNREACHABLE_ERROR(NODE) TREE_LANG_FLAG_0 (EXPR_CHECK (NODE))

/* True if EXPR (a WFL in that case) resolves into a package name */
#define RESOLVE_PACKAGE_NAME_P(WFL) TREE_LANG_FLAG_3 (EXPR_CHECK (WFL))

/* True if EXPR (a WFL in that case) resolves into a type name */
#define RESOLVE_TYPE_NAME_P(WFL) TREE_LANG_FLAG_4 (EXPR_CHECK (WFL))

/* True if STMT (a WFL in that case) holds a BREAK statement */
#define IS_BREAK_STMT_P(WFL) TREE_LANG_FLAG_5 (WFL)

/* True if EXPR (a CALL_EXPR in that case) is a crafted StringBuffer */
#define IS_CRAFTED_STRING_BUFFER_P(EXPR) TREE_LANG_FLAG_5 (EXPR)

/* True if EXPR (a SAVE_EXPR in that case) had its content already
   checked for (un)initialized local variables.  */
#define IS_INIT_CHECKED(EXPR) TREE_LANG_FLAG_5 (SAVE_EXPR_CHECK (EXPR))

/* If set in CALL_EXPR, the receiver is 'super'. */
#define CALL_USING_SUPER(EXPR) TREE_LANG_FLAG_4 (EXPR_CHECK (EXPR))

/* True if NODE (a statement) can complete normally. */
#define CAN_COMPLETE_NORMALLY(NODE) TREE_LANG_FLAG_6 (NODE)

/* True if NODE (an IDENTIFIER) bears the name of an outer field from
   inner class (or vice versa) access function.  */
#define NESTED_FIELD_ACCESS_IDENTIFIER_P(NODE) \
  TREE_LANG_FLAG_6 (IDENTIFIER_NODE_CHECK (NODE))

/* True if NODE belongs to an inner class TYPE_DECL node.
   Verifies that NODE as the attributes of a decl.  */
#define INNER_CLASS_DECL_P(NODE) (TYPE_NAME (TREE_TYPE (NODE)) == NODE	\
				  && DECL_CONTEXT (NODE))

/* True if NODE is a top level class TYPE_DECL node: NODE isn't
   an inner class or NODE is a static class.  */
#define TOPLEVEL_CLASS_DECL_P(NODE) (!INNER_CLASS_DECL_P (NODE) 	\
				     || CLASS_STATIC (NODE))

/* True if the class decl NODE was declared in an inner scope and is
   not a toplevel class */
#define PURE_INNER_CLASS_DECL_P(NODE) \
  (INNER_CLASS_DECL_P (NODE) && !CLASS_STATIC (NODE))

/* True if NODE belongs to an inner class RECORD_TYPE node. Checks
   that TYPE_NAME bears a decl. An array type wouldn't.  */
#define INNER_CLASS_TYPE_P(NODE) (TREE_CODE (TYPE_NAME (NODE)) == TYPE_DECL \
				  && DECL_CONTEXT (TYPE_NAME (NODE)))

#define TOPLEVEL_CLASS_TYPE_P(NODE) (!INNER_CLASS_TYPE_P (NODE) 	\
				     || CLASS_STATIC (TYPE_NAME (NODE)))

/* True if the class type NODE was declared in an inner scope and is
   not a toplevel class */
#define PURE_INNER_CLASS_TYPE_P(NODE) \
  (INNER_CLASS_TYPE_P (NODE) && !CLASS_STATIC (TYPE_NAME (NODE)))

/* True if NODE (a TYPE_DECL or a RECORD_TYPE) is an inner class.  */
#define INNER_CLASS_P(NODE) (TREE_CODE (NODE) == TYPE_DECL ? 		      \
			     INNER_CLASS_DECL_P (NODE) :		      \
			     (TREE_CODE (NODE) == RECORD_TYPE ? 	      \
			      INNER_CLASS_TYPE_P (NODE) : 		      \
			      (abort (), 0)))

/* On a TYPE_DECL, hold the list of inner classes defined within the
   scope of TYPE_DECL.  */
#define DECL_INNER_CLASS_LIST(NODE) DECL_INITIAL (TYPE_DECL_CHECK (NODE))

/* Add a FIELD_DECL to RECORD_TYPE RTYPE.
   The field has name NAME (a char*), and type FTYPE.
   Unless this is the first field, FIELD most hold the previous field.
   FIELD is set to the newly created FIELD_DECL.

   We set DECL_ARTIFICIAL so these fields get skipped by make_class_data
   if compiling java.lang.Object or java.lang.Class. */

#define PUSH_FIELD(RTYPE, FIELD, NAME, FTYPE) \
{ tree _field = build_decl (FIELD_DECL, get_identifier ((NAME)), (FTYPE)); \
  if (TYPE_FIELDS (RTYPE) == NULL_TREE)	\
    TYPE_FIELDS (RTYPE) = _field; 	\
  else					\
    TREE_CHAIN(FIELD) = _field;		\
  DECL_CONTEXT (_field) = (RTYPE);	\
  DECL_ARTIFICIAL (_field) = 1;		\
  FIELD = _field; }

#define FINISH_RECORD(RTYPE) layout_type (RTYPE)

/* Start building a RECORD_TYPE constructor with a given TYPE in CONS. */
#define START_RECORD_CONSTRUCTOR(CONS, CTYPE) \
  do \
    { \
      CONS = build_constructor ((CTYPE), VEC_alloc (constructor_elt, gc, 0)); \
      CONSTRUCTOR_APPEND_ELT (CONSTRUCTOR_ELTS (CONS), TYPE_FIELDS (CTYPE), \
			      NULL); \
    } \
  while (0)

/* Append a field initializer to CONS for the dummy field for the inherited
   fields.  The dummy field has the given VALUE, and the same type as the
   super-class.   Must be specified before calls to PUSH_FIELD_VALUE. */
#define PUSH_SUPER_VALUE(CONS, VALUE) \
  do \
    { \
      constructor_elt *_elt___ = VEC_last (constructor_elt, \
					   CONSTRUCTOR_ELTS (CONS)); \
      tree _next___ = TREE_CHAIN (_elt___->index); \
      gcc_assert (!DECL_NAME (_elt___->index)); \
      _elt___->value = VALUE; \
      CONSTRUCTOR_APPEND_ELT (CONSTRUCTOR_ELTS (CONS), _next___, NULL); \
    } \
  while (0)

/* Append a field initializer to CONS for a field with the given VALUE.
   NAME is a char* string used for error checking;
   the initializer must be specified in order. */
#define PUSH_FIELD_VALUE(CONS, NAME, VALUE) 				\
  do \
    { \
      constructor_elt *_elt___ = VEC_last (constructor_elt, \
					   CONSTRUCTOR_ELTS (CONS)); \
      tree _next___ = TREE_CHAIN (_elt___->index); \
      gcc_assert (strcmp (IDENTIFIER_POINTER (DECL_NAME (_elt___->index)), \
			  NAME) == 0); \
      _elt___->value = VALUE; \
      CONSTRUCTOR_APPEND_ELT (CONSTRUCTOR_ELTS (CONS), _next___, NULL); \
    } \
  while (0)

/* Finish creating a record CONSTRUCTOR CONS. */
#define FINISH_RECORD_CONSTRUCTOR(CONS) \
  VEC_pop (constructor_elt, CONSTRUCTOR_ELTS (CONS))

/* Macros on constructors invocations.  */
#define CALL_CONSTRUCTOR_P(NODE)		\
  (TREE_CODE (NODE) == NEW_CLASS_EXPR || CALL_EXPLICIT_CONSTRUCTOR_P (NODE))

#define CALL_EXPLICIT_CONSTRUCTOR_P(NODE)				\
  (CALL_THIS_CONSTRUCTOR_P (NODE) || CALL_SUPER_CONSTRUCTOR_P (NODE))

#define CALL_THIS_CONSTRUCTOR_P(NODE)					\
  (TREE_CODE (NODE) == CALL_EXPR					\
   && EXPR_WFL_NODE (TREE_OPERAND (NODE, 0)) == this_identifier_node)

#define CALL_SUPER_CONSTRUCTOR_P(NODE)					\
  (TREE_CODE (NODE) == CALL_EXPR					\
   && EXPR_WFL_NODE (TREE_OPERAND (NODE, 0)) == super_identifier_node)

/* Using a FINALLY_EXPR node */
#define FINALLY_EXPR_LABEL(NODE) TREE_OPERAND (FINALLY_EXPR_CHECK (NODE), 0)
#define FINALLY_EXPR_BLOCK(NODE) TREE_OPERAND (FINALLY_EXPR_CHECK (NODE), 1)

#define BLOCK_EXPR_DECLS(NODE)  BLOCK_VARS(NODE)
#define BLOCK_EXPR_BODY(NODE)   BLOCK_SUBBLOCKS(NODE)

/* True for an implicit block surrounding declaration not at start of {...}. */
#define BLOCK_IS_IMPLICIT(NODE) TREE_LANG_FLAG_1 (BLOCK_CHECK (NODE))
#define BLOCK_EMPTY_P(NODE) \
  (TREE_CODE (NODE) == BLOCK && BLOCK_EXPR_BODY (NODE) == empty_stmt_node)

#define BUILD_MONITOR_ENTER(WHERE, ARG)					\
  {									\
    (WHERE) = build3 (CALL_EXPR, int_type_node,				\
		      build_address_of (soft_monitorenter_node),	\
		      build_tree_list (NULL_TREE, (ARG)),	 	\
		      NULL_TREE);					\
    TREE_SIDE_EFFECTS (WHERE) = 1;					\
  }

#define BUILD_MONITOR_EXIT(WHERE, ARG)				\
  {								\
    (WHERE) = build3 (CALL_EXPR, int_type_node,			\
		      build_address_of (soft_monitorexit_node),	\
		      build_tree_list (NULL_TREE, (ARG)),	\
		      NULL_TREE);				\
    TREE_SIDE_EFFECTS (WHERE) = 1;				\
  }

/* Nonzero if TYPE is an unchecked exception */
#define IS_UNCHECKED_EXCEPTION_P(TYPE)				\
  (inherits_from_p ((TYPE), runtime_exception_type_node)	\
   || inherits_from_p ((TYPE), error_exception_type_node))

/* True when we can perform static class initialization optimization */
#define STATIC_CLASS_INIT_OPT_P() \
  (flag_optimize_sci && (optimize >= 2) && ! flag_emit_class_files)

extern int java_error_count;

/* Make the current function where this macro is invoked report error
   messages and and return, if any */
#define java_parse_abort_on_error()					\
  {									\
     if (java_error_count > save_error_count)				\
       return;								\
   }

/* These are the possible values for the `state' field of the class
   structure.  This must be kept in sync with libgcj.  */
enum
{
  JV_STATE_NOTHING = 0,		/* Set by compiler.  */

  JV_STATE_PRELOADING = 1,	/* Can do _Jv_FindClass.  */
  JV_STATE_LOADING = 3,		/* Has super installed.  */
  JV_STATE_READ = 4,		/* Has been completely defined.  */
  JV_STATE_LOADED = 5,		/* Has Miranda methods defined.  */

  JV_STATE_COMPILED = 6,	/* This was a compiled class.  */

  JV_STATE_PREPARED = 7,	/* Layout & static init done.  */
  JV_STATE_LINKED = 9,		/* Strings interned.  */

  JV_STATE_IN_PROGRESS = 10,	/* <Clinit> running.  */
  JV_STATE_ERROR = 12,

  JV_STATE_DONE = 14		/* Must be last.  */
};

#undef DEBUG_JAVA_BINDING_LEVELS

/* In a LABELED_BLOCK_EXPR node.  */
#define LABELED_BLOCK_LABEL(NODE) \
  TREE_OPERAND_CHECK_CODE (NODE, LABELED_BLOCK_EXPR, 0)
#define LABELED_BLOCK_BODY(NODE) \
  TREE_OPERAND_CHECK_CODE (NODE, LABELED_BLOCK_EXPR, 1)

/* In an EXIT_BLOCK_EXPR node.  */
#define EXIT_BLOCK_LABELED_BLOCK(NODE) \
  TREE_OPERAND_CHECK_CODE (NODE, EXIT_BLOCK_EXPR, 0)

/* In an EXPR_WITH_FILE_LOCATION node.  */
#define EXPR_WFL_EMIT_LINE_NOTE(NODE) \
  (EXPR_WITH_FILE_LOCATION_CHECK (NODE)->common.public_flag)
#undef EXPR_WFL_NODE
#define EXPR_WFL_NODE(NODE) \
  TREE_OPERAND (EXPR_WITH_FILE_LOCATION_CHECK (NODE), 0)
#ifdef USE_MAPPED_LOCATION
#define EXPR_WFL_LINECOL(NODE) ((NODE)->exp.locus)
#define EXPR_WFL_FILENAME(NODE) EXPR_FILENAME (NODE)
#define EXPR_WFL_LINENO(NODE) EXPR_LINENO (NODE)
extern tree build_expr_wfl (tree, source_location);
extern tree expr_add_location (tree, source_location, bool);
#define build_unknown_wfl(NODE) build_expr_wfl(NODE, UNKNOWN_LOCATION)
#else
#define EXPR_WFL_LINECOL(NODE) (EXPR_CHECK (NODE)->exp.complexity)
#define EXPR_WFL_LINENO(NODE) (EXPR_WFL_LINECOL (NODE) >> 12)
#define EXPR_WFL_COLNO(NODE) (EXPR_WFL_LINECOL (NODE) & 0xfff)
#undef EXPR_WFL_FILENAME_NODE
#define EXPR_WFL_FILENAME_NODE(NODE) \
  TREE_OPERAND (EXPR_WITH_FILE_LOCATION_CHECK (NODE), 2)
#define EXPR_WFL_FILENAME(NODE) \
  IDENTIFIER_POINTER (EXPR_WFL_FILENAME_NODE (NODE))
/* ??? Java uses this in all expressions.  */
#define EXPR_WFL_SET_LINECOL(NODE, LINE, COL) \
  (EXPR_WFL_LINECOL(NODE) = ((LINE) << 12) | ((COL) & 0xfff))

extern tree build_expr_wfl (tree, const char *, int, int);
#define build_unknown_wfl(NODE) build_expr_wfl(NODE, NULL, 0, 0)
#endif

extern void java_genericize (tree);
extern int java_gimplify_expr (tree *, tree *, tree *);

extern tree extract_field_decl (tree);

#endif /* ! GCC_JAVA_TREE_H */
