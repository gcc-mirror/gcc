/* Definitions for parsing and type checking for the GNU compiler for
   the Java(TM) language.
   Copyright (C) 1997, 1998, 1999, 2000 Free Software Foundation, Inc.

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

/* Hacked by Per Bothner <bothner@cygnus.com> February 1996. */

#include "hash.h"

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
      RESOLVE_EXPRESSION_NAME_P (in EXPR_WITH_FILE_LOCATION)
      IS_FOR_LOOP_P (in LOOP_EXPR)
   1: CLASS_HAS_SUPER_FLAG (in TREE_VEC).
      IS_A_CLASSFILE_NAME (in IDENTIFIER_NODE)
      COMPOUND_ASSIGN_P (in EXPR (binop_*))
   2: RETURN_MAP_ADJUSTED (in TREE_VEC).
      QUALIFIED_P (in IDENTIFIER_NODE)
      PRIMARY_P (in EXPR_WITH_FILE_LOCATION)
      MODIFY_EXPR_FROM_INITIALIZATION_P (in MODIFY_EXPR)
   3: IS_AN_IMPORT_ON_DEMAND_P (in IDENTIFIER_NODE)
      RESOLVE_PACKAGE_NAME_P (in EXPR_WITH_FILE_LOCATION)
      SWITCH_HAS_DEFAULT (in SWITCH_EXPR)
   4: IS_A_COMMAND_LINE_FILENAME_P (in IDENTIFIER_NODE)
      RESOLVE_TYPE_NAME_P (in EXPR_WITH_FILE_LOCATION)
      CALL_USING_SUPER (in CALL_EXPR)
   5: HAS_BEEN_ALREADY_PARSED_P (in IDENTIFIER_NODE)
      IS_BREAK_STMT_P (in EXPR_WITH_FILE_LOCATION)
      IS_CRAFTED_STRING_BUFFER_P (in CALL_EXPR)
   6: CAN_COMPLETE_NORMALLY (in statement nodes).

   Usage of TYPE_LANG_FLAG_?:
   1: TYPE_ARRAY_P (in RECORD_TYPE).
   2: CLASS_LOADED_P (in RECORD_TYPE).
   3: CLASS_FROM_SOURCE_P (in RECORD_TYPE).
   4: CLASS_P (in RECORD_TYPE).
   5: CLASS_FROM_CURRENTLY_COMPILED_SOURCE_P (in RECORD_TYPE)
   6: CLASS_HAS_FINIT_P (in RECORD_TYPE)

   Usage of DECL_LANG_FLAG_?:
   0: METHOD_DEPRECATED (in FUNCTION_DECL).
      FIELD_DEPRECATED (in FIELD_DECL).
      CLASS_DEPRECATED (in TYPE_DECL).
   1: METHOD_PUBLIC (in FUNCTION_DECL).
      FIELD_PUBLIC (in FIELD_DECL).
      CLASS_PUBLIC (in TYPE_DECL).
   2: METHOD_STATIC (in FUNCTION_DECL).
      (But note that FIELD_STATIC uses TREE_STATIC!)
      CLASS_COMPLETE_P (in TYPE_DECL)
   3: METHOD_FINAL (in FUNCTION_DECL)
      FIELD_FINAL (in FIELD_DECL)
      CLASS_FINAL (in TYPE_DECL)
   4: METHOD_SYNCHRONIZED (in FUNCTION_DECL).
      LABEL_IN_SUBR (in LABEL_DECL)
      CLASS_INTERFACE (in TYPE_DECL)
      FIELD_VOLATILE (int FIELD_DECL)
   5: METHOD_ABSTRACT (in FUNCTION_DECL).
      LABEL_IS_SUBR_START (in LABEL_DECL)
      CLASS_ABSTRACT (in TYPE_DECL)
      FIELD_TRANSIENT (in FIELD_DECL)
   6: METHOD_TRANSIENT (in FUNCTION_DECL)
      LABEL_CHANGED (in LABEL_DECL)
      CLASS_SUPER (in TYPE_DECL, ACC_SUPER flag)
   7: DECL_CONSTRUCTOR_P (in FUNCTION_DECL).
*/

/* True if the class whose TYPE_BINFO this is has a superclass.
   (True of all classes except Object.) */
#define CLASS_HAS_SUPER_FLAG(BINFO) TREE_LANG_FLAG_1(BINFO)
#define CLASS_HAS_SUPER(TYPE) CLASS_HAS_SUPER_FLAG (TYPE_BINFO (TYPE))

/* Return the supertype of class TYPE, or NULL_TREE is it has none. */
#define CLASSTYPE_SUPER(TYPE) (CLASS_HAS_SUPER (TYPE) ? \
  BINFO_TYPE (TREE_VEC_ELT (TYPE_BINFO_BASETYPES (TYPE), 0)) : NULL_TREE)

/* True if the class we are compiling is a .java source file;
   false if it is a .class bytecode file. */
extern int compiling_from_source;

/* The class defined by the actual (main) file we are compiling. */
extern tree main_class;

/* The class we are currently processing. */
extern tree current_class;

/* List of all class DECLs seen so far.  */
extern tree all_class_list;

/* Nonzero if we should make is_compiled_class always return 1 for
   appropriate classes that we're referencing.  */

extern int flag_assume_compiled;

extern int flag_emit_class_files;

/* When non zero, we emit xref strings. Values of the flag for xref
   backends are defined in xref.h.  */

extern int flag_emit_xref;

/* When doing xrefs, tell when not to fold.   */
extern int do_not_fold;

/* Turned to 1 if -Wall was encountered. See lang.c for their meanings.  */
extern int flag_wall;
extern int flag_redundant;
extern int flag_not_overriding;
extern int flag_static_local_jdk1_1;

/* When non zero, call a library routine to do integer divisions. */
extern int flag_use_divide_subroutine;

/* The Java .class file that provides main_class;  the main input file. */
extern struct JCF *current_jcf;

typedef struct CPool constant_pool;

#define CONSTANT_ResolvedFlag 16

/* The cpool->data[i] for a ResolvedString points to a STRING_CST. */
#define CONSTANT_ResolvedString    (CONSTANT_String+CONSTANT_ResolvedFlag)

/* The cpool->data[i] for a ResolvedClass points to a RECORD_TYPE. */
#define CONSTANT_ResolvedClass     (CONSTANT_Class+CONSTANT_ResolvedFlag)

#define CPOOL_UTF(CPOOL, INDEX) ((tree) (CPOOL)->data[INDEX])

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

/* "Promoted types" that are used for primitive types smaller
   than int.  We could use int_type_node, but then we would lose
   type information (such as needed for debugging). */
extern tree promoted_byte_type_node;
extern tree promoted_short_type_node;
extern tree promoted_char_type_node;
extern tree promoted_boolean_type_node;

extern tree byte_type_node;
extern tree short_type_node;
extern tree int_type_node;
extern tree long_type_node;

extern tree unsigned_byte_type_node;
extern tree unsigned_short_type_node;
extern tree unsigned_int_type_node;
extern tree unsigned_long_type_node;

extern tree boolean_type_node;

extern tree object_type_node;
extern tree unqualified_object_id_node;
extern tree object_ptr_type_node;
extern tree string_type_node;
extern tree string_ptr_type_node;
extern tree throwable_type_node;
extern tree runtime_exception_type_node;
extern tree error_exception_type_node;

extern tree *predef_filenames;
extern int predef_filenames_size;

extern tree byte_array_type_node;
extern tree short_array_type_node;
extern tree int_array_type_node;
extern tree long_array_type_node;
extern tree boolean_array_type_node;
extern tree char_array_type_node;
extern tree double_array_type_node;
extern tree float_array_type_node;
extern tree array_array_type_node;
extern tree object_array_type_node;
extern tree string_array_type_node;
extern tree TYPE_identifier_node;      /* "TYPE" */
extern tree init_identifier_node;      /* "<init>" */
extern tree clinit_identifier_node;      /* "<clinit>" */
extern tree finit_identifier_node;      /* "$finit$" */
extern tree void_signature_node;       /* "()V" */
extern tree length_identifier_node;  /* "length" */
extern tree this_identifier_node;  /* "this" */
extern tree super_identifier_node;  /* "super" */
extern tree continue_identifier_node;  /* "continue" */
extern tree one_elt_array_domain_type;
/* The type of the return address of a subroutine. */
extern tree return_address_type_node;

/* Nodes for boolean constants TRUE and FALSE. */
extern tree boolean_true_node, boolean_false_node;

/* Integer constants not declared in tree.h. */
extern tree long_zero_node;
extern tree float_zero_node;
extern tree double_zero_node;
extern tree integer_negative_one_node;
extern tree integer_two_node;
extern tree integer_four_node;
extern tree empty_stmt_node;

/* The type for struct methodtable. */
extern tree methodtable_type;
extern tree methodtable_ptr_type;

extern tree utf8const_type;
extern tree utf8const_ptr_type;

extern tree class_type_node;
extern tree class_ptr_type;
extern tree field_type_node;
extern tree constants_type_node;
extern tree dtable_type, dtable_ptr_type;
extern tree field_ptr_type_node;
extern tree field_info_union_node;
extern tree method_type_node;
extern tree method_ptr_type_node;
#define nativecode_ptr_type_node ptr_type_node

extern tree end_params_node;

/* References to internal libjava functions we use. */
extern tree alloc_object_node;
extern tree soft_instanceof_node;
extern tree soft_checkcast_node;
extern tree soft_initclass_node;
extern tree soft_newarray_node;
extern tree soft_anewarray_node;
extern tree soft_multianewarray_node;
extern tree soft_badarrayindex_node;
extern tree throw_node[];
extern tree soft_checkarraystore_node;
extern tree soft_monitorenter_node;
extern tree soft_monitorexit_node;
extern tree soft_lookupinterfacemethod_node;
extern tree soft_fmod_node;
extern tree soft_exceptioninfo_call_node;
extern tree soft_idiv_node;
extern tree soft_irem_node;
extern tree soft_ldiv_node;
extern tree soft_lrem_node;

extern tree access_flags_type_node;

extern tree class_dtable_decl;

/* They need to be reset before processing each class */
extern struct CPool *outgoing_cpool; 
extern tree current_constant_pool_data_ref;

extern tree wfl_operator;

struct lang_identifier
{
  struct tree_identifier ignore;
  tree global_value, local_value;

  /* If non-NULL:  An ADDR_REF to a VAR_DECL that contains
   * the Utf8Const representation of the identifier.  */
  tree utf8_ref;
};

/* Macros for access to language-specific slots in an identifier.  */
/* UNless specifide, each of these slots contains a DECL node or null.  */

/* This represents the value which the identifier has in the
   file-scope namespace.  */
#define IDENTIFIER_GLOBAL_VALUE(NODE)   \
  (((struct lang_identifier *)(NODE))->global_value)
/* This represents the value which the identifier has in the current
   scope.  */
#define IDENTIFIER_LOCAL_VALUE(NODE)    \
  (((struct lang_identifier *)(NODE))->local_value)

/* Given an identifier NODE, get the corresponding (non-handle) class.
   For get_identifier ("java.lang.Number"), the result is
   the struct whose DECL_ASSEMBLER_NAME is "Classjava_lang_Number". */
#define IDENTIFIER_CLASS_VALUE(NODE) IDENTIFIER_GLOBAL_VALUE(NODE)

/* Given an identifier NODE, get the corresponding handle class.
   For get_identifier ("java.lang.Number"), the result is
   the struct whose DECL_ASSEMBLER_NAME is "Hjava_lang_Number". */
#define IDENTIFIER_HANDLECLASS_VALUE(NODE) ???

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
#define DECL_CODE_OFFSET(DECL) (DECL_LANG_SPECIFIC(DECL)->code_offset)
/* Similarly, the length of the bytecode. */
#define DECL_CODE_LENGTH(DECL) (DECL_LANG_SPECIFIC(DECL)->code_length)
/* Similarly, the position of the LineNumberTable attribute. */
#define DECL_LINENUMBERS_OFFSET(DECL) \
  (DECL_LANG_SPECIFIC(DECL)->linenumbers_offset)
/* Similarly, the position of the LocalVariableTable attribute
   (following the standard attribute header). */
#define DECL_LOCALVARIABLES_OFFSET(DECL) \
  (DECL_LANG_SPECIFIC(DECL)->localvariables_offset)

#define DECL_MAX_LOCALS(DECL) (DECL_LANG_SPECIFIC(DECL)->max_locals)
#define DECL_MAX_STACK(DECL) (DECL_LANG_SPECIFIC(DECL)->max_stack)
/* Number of local variable slots needed for the arguments of this function. */
#define DECL_ARG_SLOT_COUNT(DECL) (DECL_LANG_SPECIFIC(DECL)->arg_slot_count)
/* List of checked thrown exceptions, as specified with the `throws'
   keyword */
#define DECL_FUNCTION_THROWS(DECL) (DECL_LANG_SPECIFIC(DECL)->throws_list)
/* List of other constructors of the same class that this constructor
   calls */
#define DECL_CONSTRUCTOR_CALLS(DECL) \
  (DECL_LANG_SPECIFIC(DECL)->called_constructor)
/* Pointer to the function's current's COMPOUND_EXPR tree (while
   completing its body) or the function's block */
#define DECL_FUNCTION_BODY(DECL) (DECL_LANG_SPECIFIC(DECL)->function_decl_body)
/* How specific the function is (for method selection - Java source
   code front-end */
#define DECL_SPECIFIC_COUNT(DECL) DECL_ARG_SLOT_COUNT(DECL)
/* For each function decl, init_test_table contains a hash table whose
   entries are keyed on class names, and whose values are local
   boolean decls.  The variables are intended to be TRUE when the
   class has been initialized in this function, and FALSE otherwise.  */
#define DECL_FUNCTION_INIT_TEST_TABLE(DECL) \
  (DECL_LANG_SPECIFIC(DECL)->init_test_table)

/* In a LABEL_DECL, a TREE_VEC that saves the type_map at that point. */
#define LABEL_TYPE_STATE(NODE) (DECL_INITIAL (NODE))

/* In the label of a subroutine, a dummy label that records the
   state following a merge of all the ret instructions in this subroutine. */
#define LABEL_RETURN_LABEL(DECL) DECL_ARGUMENTS(DECL)

/* In the label of a sub-routine, records the type state at return.
 * A local may be TYPE_UNUSED, which means that the local is not
 * used (stored to or loaded from) in this subroutine - at least for
 * code that we have verified so far. */
#define LABEL_RETURN_TYPE_STATE(NODE) LABEL_TYPE_STATE (LABEL_RETURN_LABEL (NODE))

/* In a TREE_VEC for a LABEL_RETURN_TYPE_STATE, notes that
   TREE_VEC_LENGTH has been adjust to the correct stack size. */
#define RETURN_MAP_ADJUSTED(NODE) TREE_LANG_FLAG_2(NODE)

/* In the label of a sub-routine, a chain of the return location labels. */
#define LABEL_RETURN_LABELS(node) DECL_RESULT (LABEL_RETURN_LABEL(node))

/* In a LABEL_DECL, the next pending label.
   See pending_blocks in expr.c. */
#define LABEL_PENDING_CHAIN(NODE) DECL_RESULT(NODE)

/* In a LABEL_DECL, the corresponding bytecode program counter. */
#define LABEL_PC(NODE) ((NODE)->decl.saved_insns.i)

/* Used during verification to mark the label has "changed". (See JVM Spec). */
#define LABEL_CHANGED(NODE) DECL_LANG_FLAG_6(NODE)

/* In a LABEL_DECL, true if we have verified instructions starting here. */
#define LABEL_VERIFIED(NODE) (instruction_bits[LABEL_PC(NODE)]&BCODE_VERIFIED)

/* True if this code is within a subroutine (target of a jsr). */
#define LABEL_IN_SUBR(NODE) DECL_LANG_FLAG_4(NODE)
/* True if this code is the start of a subroutine (target of a jsr). */
#define LABEL_IS_SUBR_START(NODE) DECL_LANG_FLAG_5(NODE)

/* In a LABEL_DECL, if LABEL_IN_SUBR(NODE), points to start of subroutine. */
#define LABEL_SUBR_START(NODE) DECL_ABSTRACT_ORIGIN(NODE)

/* In a LABEL_DECL that has LABEL_IS_SUBR_START, this points to the start
   of surrounding subroutine in the case of a nested subroutine,
   and NULL_TREE otherwise. */
#define LABEL_SUBR_CONTEXT(NODE) DECL_CONTEXT (LABEL_RETURN_LABEL (NODE))

/* The slot number for this local variable. */
#define DECL_LOCAL_SLOT_NUMBER(NODE) \
  (((struct lang_decl_var*)DECL_LANG_SPECIFIC(NODE))->slot_number)
/* The start (bytecode) pc for the valid range of this local variable. */
#define DECL_LOCAL_START_PC(NODE) \
  (((struct lang_decl_var*)DECL_LANG_SPECIFIC(NODE))->start_pc)
/* The end (bytecode) pc for the valid range of this local variable. */
#define DECL_LOCAL_END_PC(NODE) \
  (((struct lang_decl_var*)DECL_LANG_SPECIFIC(NODE))->end_pc)
/* For a VAR_DECLor PARM_DECL, used to chain decls with the same
   slot_number in decl_map. */
#define DECL_LOCAL_SLOT_CHAIN(NODE) \
  (((struct lang_decl_var*)DECL_LANG_SPECIFIC(NODE))->slot_chain)

/* For a local VAR_DECL, holds the index into a words bitstring that
   specifies if this decl is definitively assigned.
   A DECL_BIT_INDEX of -1 means we no longer care. */
#define DECL_BIT_INDEX(DECL) DECL_FIELD_SIZE(DECL)

/* DECL_LANG_SPECIFIC for FUNCTION_DECLs. */
struct lang_decl
{
  /*  tree chain; not yet used. */
  long code_offset;
  int code_length;
  long linenumbers_offset;
  long localvariables_offset;
  int arg_slots;
  int max_locals, max_stack, arg_slot_count;
  tree throws_list;		/* Exception specified by `throws' */
  tree function_decl_body;	/* Hold all function's statements */
  tree called_constructor;	/* When decl is a constructor, the
				   list of other constructor it calls. */
  struct hash_table init_test_table;
                                /* Class initialization test variables.  */
};

/* init_test_table hash table entry structure.  */
struct init_test_hash_entry
{
  struct hash_entry root;
  tree init_test_decl;
};


/* DECL_LANG_SPECIFIC for VAR_DECL and PARM_DECL. */
struct lang_decl_var
{
  int slot_number;
  int start_pc;
  int end_pc;
  tree slot_chain;
};

struct lang_type
{
  tree signature;
  struct JCF *jcf;
};

#ifdef JAVA_USE_HANDLES
/* TYPE_BINFO_HANDLE points from a handle-class to its corresponding
   non-handle-class, and vice verse. */

#define BINFO_HANDLE(NODE) TREE_VEC_ELT ((NODE), 6)

/* Given a RECORD_TYPE for a handle type, return the corresponding class. */
#define HANDLE_TO_CLASS_TYPE(HTYPE) BINFO_HANDLE (TYPE_BINFO (HTYPE))

/* Given a RECORD_TYPE for a class, return the corresponding handle type. */
#define CLASS_TO_HANDLE_TYPE(TYPE) BINFO_HANDLE (TYPE_BINFO (TYPE))
#else
#define HANDLE_TO_CLASS_TYPE(HTYPE) (HTYPE)
#define CLASS_TO_HANDLE_TYPE(TYPE) (TYPE)
#endif

#define JCF_u4 unsigned long
#define JCF_u2 unsigned short

extern void add_assume_compiled PARAMS ((const char *, int));
extern tree lookup_class PARAMS ((tree));
extern tree lookup_java_constructor PARAMS ((tree, tree));
extern tree lookup_java_method PARAMS ((tree, tree, tree));
extern tree lookup_argument_method PARAMS ((tree, tree, tree));
extern tree promote_type PARAMS ((tree));
extern tree get_constant PARAMS ((struct JCF*, int));
extern tree get_name_constant PARAMS ((struct JCF*, int));
extern tree get_class_constant PARAMS ((struct JCF*, int));
extern tree parse_signature PARAMS ((struct JCF *jcf, int sig_index));
extern void jcf_parse PARAMS ((struct JCF*));
extern tree add_field PARAMS ((tree, tree, tree, int));
extern tree add_method PARAMS ((tree, int, tree, tree));
extern tree add_method_1 PARAMS ((tree, int, tree, tree));
extern tree make_class PARAMS ((void));
extern tree push_class PARAMS ((tree, tree));
extern tree unmangle_classname PARAMS ((const char *name, int name_length));
extern tree parse_signature_string PARAMS ((const unsigned char *, int));
extern tree get_type_from_signature PARAMS ((tree));
extern void layout_class PARAMS ((tree));
extern tree layout_class_method PARAMS ((tree, tree, tree, tree));
extern void layout_class_methods PARAMS ((tree));
extern tree build_class_ref PARAMS ((tree));
extern tree build_dtable_decl PARAMS ((tree));
extern tree build_internal_class_name PARAMS ((tree));
extern tree build_constants_constructor PARAMS ((void));
extern tree build_ref_from_constant_pool PARAMS ((int));
extern tree build_utf8_ref PARAMS ((tree));
extern tree ident_subst PARAMS ((const char*, int,
				const char*, int, int, const char*));
extern tree identifier_subst PARAMS ((const tree,
				     const char *, int, int, const char *));
extern tree build_java_signature PARAMS ((tree));
extern tree build_java_argument_signature PARAMS ((tree));
extern void set_java_signature PARAMS ((tree, tree));
extern tree build_static_field_ref PARAMS ((tree));
extern tree build_address_of PARAMS ((tree));
extern tree find_local_variable PARAMS ((int index, tree type, int pc));
extern tree find_stack_slot PARAMS ((int index, tree type));
extern tree build_prim_array_type PARAMS ((tree, HOST_WIDE_INT));
extern tree build_java_array_type PARAMS ((tree, HOST_WIDE_INT));
extern int is_compiled_class PARAMS ((tree));
extern tree mangled_classname PARAMS ((const char*, tree));
extern tree lookup_label PARAMS ((int));
extern tree pop_type_0 PARAMS ((tree));
extern tree pop_type PARAMS ((tree));
extern void pop_argument_types PARAMS ((tree));
extern tree decode_newarray_type PARAMS ((int));
extern tree lookup_field PARAMS ((tree*, tree));
extern int is_array_type_p PARAMS ((tree));
extern HOST_WIDE_INT java_array_type_length PARAMS ((tree));
extern int read_class PARAMS ((tree));
extern void load_class PARAMS ((tree, int));

extern tree lookup_name PARAMS ((tree));
extern tree build_known_method_ref PARAMS ((tree, tree, tree, tree, tree));
extern tree build_class_init PARAMS ((tree, tree));
extern tree build_invokevirtual PARAMS ((tree, tree));
extern tree build_invokeinterface PARAMS ((tree, tree, tree));
extern tree invoke_build_dtable PARAMS ((int, tree));
extern tree build_field_ref PARAMS ((tree, tree, tree));
extern void pushdecl_force_head PARAMS ((tree));
extern tree build_java_binop PARAMS ((enum tree_code, tree, tree, tree));
extern tree build_java_soft_divmod PARAMS ((enum tree_code, tree, tree, tree));
extern tree binary_numeric_promotion PARAMS ((tree, tree, tree *, tree *));
extern tree build_java_arrayaccess PARAMS ((tree, tree, tree));
extern tree build_newarray PARAMS ((int, tree));
extern tree build_anewarray PARAMS ((tree, tree));
extern tree build_new_array PARAMS ((tree, tree));
extern tree build_java_array_length_access PARAMS ((tree));
extern tree build_java_arraynull_check PARAMS ((tree, tree, tree));
extern tree create_label_decl PARAMS ((tree));
extern void push_labeled_block PARAMS ((tree));
extern tree prepare_eh_table_type PARAMS ((tree));
extern void java_set_exception_lang_code PARAMS ((void));
extern tree generate_name PARAMS ((void));
extern void pop_labeled_block PARAMS ((void));
extern const char *lang_printable_name PARAMS ((tree, int));
extern tree maybe_add_interface PARAMS ((tree, tree));
extern void set_super_info PARAMS ((int, tree, tree, int));
extern int get_access_flags_from_decl PARAMS ((tree));
extern int interface_of_p PARAMS ((tree, tree));
extern int inherits_from_p PARAMS ((tree, tree));
extern void complete_start_java_method PARAMS ((tree));
extern tree build_result_decl PARAMS ((tree));
extern void emit_handlers PARAMS ((void));
extern void init_outgoing_cpool PARAMS ((void));
extern void make_class_data PARAMS ((tree));
extern void register_class PARAMS ((void));
extern int alloc_name_constant PARAMS ((int, tree));
extern void emit_register_classes PARAMS ((void));
extern void lang_init_source PARAMS ((int));
extern void write_classfile PARAMS ((tree));
extern char *print_int_node PARAMS ((tree));
extern void parse_error_context PARAMS ((tree cl, const char *, ...))
  ATTRIBUTE_PRINTF_2;
extern tree build_primtype_type_ref PARAMS ((const char *));
extern tree java_get_real_method_name PARAMS ((tree));
extern void finish_class PARAMS ((void));
extern void java_layout_seen_class_methods PARAMS ((void));
extern void check_for_initialization PARAMS ((tree));

extern tree pushdecl_top_level PARAMS ((tree));
extern int alloc_class_constant PARAMS ((tree));
extern int unicode_mangling_length PARAMS ((const char *, int));
extern void init_expr_processing PARAMS ((void));
extern void push_super_field PARAMS ((tree, tree));
extern void init_class_processing PARAMS ((void));
extern int can_widen_reference_to PARAMS ((tree, tree));
extern int class_depth PARAMS ((tree));
extern int verify_jvm_instructions PARAMS ((struct JCF *, const unsigned char *, long));
extern void maybe_pushlevels PARAMS ((int));
extern void maybe_poplevels PARAMS ((int));
extern void force_poplevels PARAMS ((int));
extern int process_jvm_instruction PARAMS ((int, const unsigned char *, long));
extern void set_local_type PARAMS ((int, tree));
extern int merge_type_state PARAMS ((tree));
extern void push_type PARAMS ((tree));
extern void load_type_state PARAMS ((tree));
extern void add_interface PARAMS ((tree, tree));
extern void append_gpp_mangled_name PARAMS ((struct obstack *, const char *, int));
extern void append_gpp_mangled_classtype PARAMS ((struct obstack *, const char *));
extern void emit_unicode_mangled_name PARAMS ((struct obstack *, const char *, int));
extern tree force_evaluation_order PARAMS ((tree));
extern int verify_constant_pool PARAMS ((struct JCF *));
extern void start_java_method PARAMS ((tree));
extern void end_java_method PARAMS ((void));
extern void give_name_to_locals PARAMS ((struct JCF *));
extern void expand_byte_code PARAMS ((struct JCF *, tree));
extern int open_in_zip PARAMS ((struct JCF *, const char *, const char *, int));
extern void set_constant_value PARAMS ((tree, tree));
#ifdef jword
extern int find_constant1 PARAMS ((struct CPool *, int, jword));
extern int find_constant2 PARAMS ((struct CPool *, int, jword, jword));
#endif
extern int find_utf8_constant PARAMS ((struct CPool *, tree));
extern int find_string_constant PARAMS ((struct CPool *, tree));
extern int find_class_constant PARAMS ((struct CPool *, tree));
extern int find_fieldref_index PARAMS ((struct CPool *, tree));
extern int find_methodref_index PARAMS ((struct CPool *, tree));
extern void write_constant_pool PARAMS ((struct CPool *, unsigned char *, int));
extern int count_constant_pool_bytes PARAMS ((struct CPool *));
extern int encode_newarray_type PARAMS ((tree));
#ifdef uint64
extern void format_int PARAMS ((char *, jlong, int));
extern void format_uint PARAMS ((char *, uint64, int));
#endif
extern void jcf_trim_old_input PARAMS ((struct JCF *));
#ifdef BUFSIZ
extern void jcf_print_utf8 PARAMS ((FILE *, const unsigned char *, int));
extern void jcf_print_char PARAMS ((FILE *, int));
extern void jcf_print_utf8_replace PARAMS ((FILE *, const unsigned char *,
					   int, int, int));
# if JCF_USE_STDIO
extern char* open_class PARAMS ((char *, struct JCF *, FILE *, const char *));
# else
extern char* open_class PARAMS ((char *, struct JCF *, int, const char *));
# endif /* JCF_USE_STDIO */
#endif
void java_debug_context PARAMS ((void));

/* We use ARGS_SIZE_RTX to indicate that gcc/expr.h has been included
   to declare `enum expand_modifier'. */
#if defined (TREE_CODE) && defined(RTX_CODE) && defined (HAVE_MACHINE_MODES) && defined (ARGS_SIZE_RTX)
struct rtx_def * java_lang_expand_expr PARAMS ((tree, rtx, enum machine_mode,
					       enum expand_modifier)); 
#endif /* TREE_CODE && RTX_CODE && HAVE_MACHINE_MODES && ARGS_SIZE_RTX */

/* Access flags etc for a method (a FUNCTION_DECL): */

#define METHOD_PUBLIC(DECL) DECL_LANG_FLAG_1 (DECL)
#define METHOD_PRIVATE(DECL) TREE_PRIVATE (DECL)
#define METHOD_PROTECTED(DECL) TREE_PROTECTED (DECL)
#define METHOD_STATIC(DECL) DECL_LANG_FLAG_2 (DECL)
#define METHOD_FINAL(DECL) DECL_LANG_FLAG_3 (DECL)
#define METHOD_SYNCHRONIZED(DECL) DECL_LANG_FLAG_4 (DECL)
#define METHOD_NATIVE(DECL) DECL_EXTERNAL(DECL)
#define METHOD_ABSTRACT(DECL) DECL_LANG_FLAG_5 (DECL)
#define METHOD_TRANSIENT(DECL) DECL_LANG_FLAG_6 (DECL)

#define DECL_CONSTRUCTOR_P(DECL) DECL_LANG_FLAG_7(DECL)

/* Access flags etc for a variable/field (a FIELD_DECL): */

#define FIELD_PRIVATE(DECL) TREE_PRIVATE (DECL)
#define FIELD_PROTECTED(DECL) TREE_PROTECTED (DECL)
#define FIELD_PUBLIC(DECL) DECL_LANG_FLAG_1 (DECL)
#define FIELD_STATIC(DECL) TREE_STATIC (DECL)
#define FIELD_FINAL(DECL) DECL_LANG_FLAG_3 (DECL)
#define FIELD_VOLATILE(DECL) DECL_LANG_FLAG_4 (DECL)
#define FIELD_TRANSIENT(DECL) DECL_LANG_FLAG_5 (DECL)

/* Access flags etc for a class (a TYPE_DECL): */

#define CLASS_PUBLIC(DECL) DECL_LANG_FLAG_1 (DECL)
#define CLASS_FINAL(DECL) DECL_LANG_FLAG_3 (DECL)
#define CLASS_INTERFACE(DECL) DECL_LANG_FLAG_4 (DECL)
#define CLASS_ABSTRACT(DECL) DECL_LANG_FLAG_5 (DECL)
#define CLASS_SUPER(DECL) DECL_LANG_FLAG_6 (DECL)

/* @deprecated marker flag on methods, fields and classes */

#define METHOD_DEPRECATED(DECL) DECL_LANG_FLAG_0 (DECL)
#define FIELD_DEPRECATED(DECL) DECL_LANG_FLAG_0 (DECL)
#define CLASS_DEPRECATED(DECL) DECL_LANG_FLAG_0 (DECL)
#define DECL_DEPRECATED(DECL) DECL_LANG_FLAG_0 (DECL)

/* The number of virtual methods in this class's dispatch table.
 Does not include initial two dummy entries (one points to the
 Class object, and the other is for G++ -fvtable-thunks compatibility). */
#define TYPE_NVIRTUALS(TYPE) TYPE_BINFO_VIRTUALS (TYPE)

/* A TREE_VEC (indexed by DECL_VINDEX) containing this class's
   virtual methods. */
#define TYPE_VTABLE(TYPE) TYPE_BINFO_VTABLE(TYPE)

/* Use CLASS_LOADED_P? FIXME */
#define CLASS_COMPLETE_P(DECL) DECL_LANG_FLAG_2 (DECL) 

/* This maps a bytecode offset (PC) to various flags,
   listed below (starting with BCODE_). */
extern char *instruction_bits;

/* True iff the byte is the start of an instruction. */
#define BCODE_INSTRUCTION_START 1

/* True iff there is a jump to this location. */
#define BCODE_JUMP_TARGET 2

/* True iff there is a return to this location.
   (I.e. the preceedng instruction was a call.) */
#define BCODE_RETURN_TARGET 4

/* True iff this is the start of an exception handler. */
#define BCODE_EXCEPTION_TARGET 16

/* True iff there is a jump to this location (and it needs a label). */
#define BCODE_TARGET \
  (BCODE_JUMP_TARGET|BCODE_RETURN_TARGET \
   | BCODE_EXCEPTION_TARGET)

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

/* A array mapping variable/stack slot index to the type current
   in that variable/stack slot.
   TYPE_UNKNOWN, TYPE_SECOND, and TYPE_NULL are special cases. */
extern tree *type_map;

/* Map a stack index to the type currently in that slot. */
#define stack_type_map (type_map+DECL_MAX_LOCALS(current_function_decl))

/* True iff TYPE takes two variable/stack slots. */
#define TYPE_IS_WIDE(TYPE) \
  ((TYPE) == double_type_node || (TYPE) == long_type_node)

/* True iff TYPE is a Java array type. */
#define TYPE_ARRAY_P(TYPE) TYPE_LANG_FLAG_1 (TYPE)

/* If FUNCTION_TYPE or METHOD_TYPE: cache for build_java_argument_signature. */
#define TYPE_ARGUMENT_SIGNATURE(TYPE) TYPE_VFIELD(TYPE)

/* Given an array type, give the type of the elements. */
/* FIXME this use of TREE_TYPE conflicts with something or other. */
#define TYPE_ARRAY_ELEMENT(ATYPE) TREE_TYPE(ATYPE)

/* True if class TYPE has been loaded. */
#define CLASS_LOADED_P(TYPE) TYPE_LANG_FLAG_2 (TYPE)

/* True if class TYPE was defined in Java source code. */
#define CLASS_FROM_SOURCE_P(TYPE) TYPE_LANG_FLAG_3 (TYPE)

/* True of a RECORD_TYPE of a class/interface type (not array type) */
#define CLASS_P(TYPE) TYPE_LANG_FLAG_4 (TYPE)

/* True if class TYPE was defined in a Java source file compiled. */
#define CLASS_FROM_CURRENTLY_COMPILED_SOURCE_P(TYPE) \
  TYPE_LANG_FLAG_5 (TYPE)

/* True if class TYPE has a field initializer $finit$ function */
#define CLASS_HAS_FINIT_P(TYPE) TYPE_LANG_FLAG_6 (TYPE)

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
#define SWITCH_HAS_DEFAULT(NODE) TREE_LANG_FLAG_3 (NODE)

/* True if EXPR (a WFL in that case) was created after the
   reduction of PRIMARY . XXX */
#define PRIMARY_P(EXPR) TREE_LANG_FLAG_2 (EXPR)

/* True if EXPR (a MODIFY_EXPR in that case) is the result of variable
   initialization during its declaration */
#define MODIFY_EXPR_FROM_INITIALIZATION_P(EXPR) TREE_LANG_FLAG_2 (EXPR)

/* True if EXPR (a WFL in that case) resolves into an expression name */
#define RESOLVE_EXPRESSION_NAME_P(WFL) TREE_LANG_FLAG_0 (WFL)

/* True if EXPR (a LOOP_EXPR in that case) is part of a for statement */
#define IS_FOR_LOOP_P(EXPR) TREE_LANG_FLAG_0 (EXPR)

/* True if EXPR (a WFL in that case) resolves into a package name */
#define RESOLVE_PACKAGE_NAME_P(WFL) TREE_LANG_FLAG_3 (WFL)

/* True if EXPR (a WFL in that case) resolves into a type name */
#define RESOLVE_TYPE_NAME_P(WFL) TREE_LANG_FLAG_4 (WFL)

/* True if STMT (a WFL in that case) holds a BREAK statement */
#define IS_BREAK_STMT_P(WFL) TREE_LANG_FLAG_5 (WFL)

/* True if EXPR (a CALL_EXPR in that case) is a crafted StringBuffer */
#define IS_CRAFTED_STRING_BUFFER_P(EXPR) TREE_LANG_FLAG_5 (EXPR)

/* If set in CALL_EXPR, the receiver is 'super'. */
#define CALL_USING_SUPER(EXPR) TREE_LANG_FLAG_4 (EXPR)

/* True if NODE (a statement) can complete normally. */
#define CAN_COMPLETE_NORMALLY(NODE) TREE_LANG_FLAG_6(NODE)

/* Add a FIELD_DECL to RECORD_TYPE RTYPE.
   The field has name NAME (a char*), and type FTYPE.
   Unless this is the first field, FIELD most hold the previous field.
   FIELD is set to the newly created FIELD_DECL.

   We set DECL_ARTIFICIAL so these fields get skipped by make_class_data
   if compiling java.lang.Object or java.lang.Class. */

#define PUSH_FIELD(RTYPE, FIELD, NAME, FTYPE) \
{ tree tmp_field = build_decl (FIELD_DECL, get_identifier(NAME), FTYPE); \
  if (TYPE_FIELDS (RTYPE) == NULL_TREE) TYPE_FIELDS (RTYPE) = tmp_field; \
  else TREE_CHAIN(FIELD) = tmp_field; \
  DECL_CONTEXT (tmp_field) = RTYPE; \
  DECL_ARTIFICIAL (tmp_field) = 1; \
  FIELD = tmp_field; }

#define FINISH_RECORD(RTYPE) layout_type (RTYPE)

/* Start building a RECORD_TYPE constructor with a given TYPE in CONS. */
#define START_RECORD_CONSTRUCTOR(CONS, CTYPE) { \
  CONS = build (CONSTRUCTOR, CTYPE, NULL_TREE, NULL_TREE);\
  TREE_CHAIN(CONS) = TYPE_FIELDS (CTYPE); }

/* Append a field initializer to CONS for the dummy field for the inherited
   fields.  The dummy field has the given VALUE, and the same type as the
   super-class.   Must be specified before calls to PUSH_FIELD_VALUE. */

#define PUSH_SUPER_VALUE(CONS, VALUE) {\
  tree field = TREE_CHAIN(CONS);\
  if (DECL_NAME (field) != NULL_TREE) abort();\
  CONSTRUCTOR_ELTS(CONS) = tree_cons (field, VALUE, CONSTRUCTOR_ELTS(CONS));\
  TREE_CHAIN(CONS) = TREE_CHAIN (field); }

/* Append a field initializer to CONS for a field with the given VALUE.
   NAME is a char* string used for error checking;
   the initializer must be specified in order. */
#define PUSH_FIELD_VALUE(CONS, NAME, VALUE) {\
  tree field = TREE_CHAIN(CONS);\
  if (strcmp (IDENTIFIER_POINTER (DECL_NAME (field)), NAME) != 0) abort();\
  CONSTRUCTOR_ELTS(CONS) = tree_cons (field, VALUE, CONSTRUCTOR_ELTS(CONS));\
  TREE_CHAIN(CONS) = TREE_CHAIN (field); }

/* Finish creating a record CONSTRUCTOR CONS. */
#define FINISH_RECORD_CONSTRUCTOR(CONS) \
  CONSTRUCTOR_ELTS(CONS) = nreverse (CONSTRUCTOR_ELTS(CONS))

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
#define FINALLY_EXPR_LABEL(NODE) TREE_OPERAND ((NODE), 0)
#define FINALLY_EXPR_BLOCK(NODE) TREE_OPERAND ((NODE), 1)

#define BLOCK_EXPR_DECLS(NODE)  BLOCK_VARS(NODE)
#define BLOCK_EXPR_BODY(NODE)   BLOCK_SUBBLOCKS(NODE)

#define BUILD_MONITOR_ENTER(WHERE, ARG)				\
  {								\
    (WHERE) = build (CALL_EXPR, int_type_node,			\
		     build_address_of (soft_monitorenter_node),	\
		     build_tree_list (NULL_TREE, (ARG)), 	\
		     NULL_TREE);				\
    TREE_SIDE_EFFECTS (WHERE) = 1;				\
  }

#define BUILD_MONITOR_EXIT(WHERE, ARG)				\
  {								\
    (WHERE) = build (CALL_EXPR, int_type_node,			\
		     build_address_of (soft_monitorexit_node),	\
		     build_tree_list (NULL_TREE, (ARG)),	\
		     NULL_TREE);				\
    TREE_SIDE_EFFECTS (WHERE) = 1;				\
  }

/* Non zero if TYPE is an unchecked exception */
#define IS_UNCHECKED_EXCEPTION_P(TYPE)				\
  (inherits_from_p ((TYPE), runtime_exception_type_node)	\
   || inherits_from_p ((TYPE), error_exception_type_node))

extern int java_error_count;					\

/* Make the current function where this macro is invoked report error
   messages and and return, if any */
#define java_parse_abort_on_error()					\
  {									\
     if (java_error_count > save_error_count)				\
       return;								\
   }

#undef DEBUG_JAVA_BINDING_LEVELS
