/* Definitions for C parsing and type checking.
   Copyright (C) 1987, 1993, 1994, 1995, 1997, 1998,
   1999, 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#ifndef GCC_C_TREE_H
#define GCC_C_TREE_H

#include "c-common.h"
#include "diagnostic.h"

/* struct lang_identifier is private to c-decl.c, but langhooks.c needs to
   know how big it is.  This is sanity-checked in c-decl.c.  */
#define C_SIZEOF_STRUCT_LANG_IDENTIFIER \
  (sizeof (struct c_common_identifier) + 3 * sizeof (void *))

/* For gc purposes, return the most likely link for the longest chain.  */
#define C_LANG_TREE_NODE_CHAIN_NEXT(T)				\
  ((union lang_tree_node *)					\
   (TREE_CODE (T) == INTEGER_TYPE ? TYPE_NEXT_VARIANT (T)	\
    : TREE_CODE (T) == COMPOUND_EXPR ? TREE_OPERAND (T, 1)	\
    : TREE_CHAIN (T)))

/* Language-specific declaration information.  */

struct lang_decl GTY(())
{
  /* The return types and parameter types may have variable size.
     This is a list of any SAVE_EXPRs that need to be evaluated to
     compute those sizes.  */
  tree pending_sizes;
};

/* In a RECORD_TYPE or UNION_TYPE, nonzero if any component is read-only.  */
#define C_TYPE_FIELDS_READONLY(TYPE) TREE_LANG_FLAG_1 (TYPE)

/* In a RECORD_TYPE or UNION_TYPE, nonzero if any component is volatile.  */
#define C_TYPE_FIELDS_VOLATILE(TYPE) TREE_LANG_FLAG_2 (TYPE)

/* In a RECORD_TYPE or UNION_TYPE or ENUMERAL_TYPE
   nonzero if the definition of the type has already started.  */
#define C_TYPE_BEING_DEFINED(TYPE) TYPE_LANG_FLAG_0 (TYPE)

/* In an incomplete RECORD_TYPE or UNION_TYPE, a list of variable
   declarations whose type would be completed by completing that type.  */
#define C_TYPE_INCOMPLETE_VARS(TYPE) TYPE_VFIELD (TYPE)

/* In an IDENTIFIER_NODE, nonzero if this identifier is actually a
   keyword.  C_RID_CODE (node) is then the RID_* value of the keyword,
   and C_RID_YYCODE is the token number wanted by Yacc.  */
#define C_IS_RESERVED_WORD(ID) TREE_LANG_FLAG_0 (ID)

struct lang_type GTY(())
{
  /* In a RECORD_TYPE, a sorted array of the fields of the type.  */
  struct sorted_fields_type * GTY ((reorder ("resort_sorted_fields"))) s;
  /* In an ENUMERAL_TYPE, the min and max values.  */
  tree enum_min;
  tree enum_max;
};

/* Record whether a type or decl was written with nonconstant size.
   Note that TYPE_SIZE may have simplified to a constant.  */
#define C_TYPE_VARIABLE_SIZE(TYPE) TYPE_LANG_FLAG_1 (TYPE)
#define C_DECL_VARIABLE_SIZE(TYPE) DECL_LANG_FLAG_0 (TYPE)

/* Store a value in that field.  */
#define C_SET_EXP_ORIGINAL_CODE(EXP, CODE) \
  (TREE_COMPLEXITY (EXP) = (int) (CODE))

/* Record whether a typedef for type `int' was actually `signed int'.  */
#define C_TYPEDEF_EXPLICITLY_SIGNED(EXP) DECL_LANG_FLAG_1 (EXP)

/* For a FUNCTION_DECL, nonzero if it was defined without an explicit
   return type.  */
#define C_FUNCTION_IMPLICIT_INT(EXP) DECL_LANG_FLAG_1 (EXP)

/* For a FUNCTION_DECL, nonzero if it was an implicit declaration.  */
#define C_DECL_IMPLICIT(EXP) DECL_LANG_FLAG_2 (EXP)

/* For FUNCTION_DECLs, evaluates true if the decl is built-in but has
   been declared.  */
#define C_DECL_DECLARED_BUILTIN(EXP) DECL_LANG_FLAG_3 (EXP)

/* Record whether a decl was declared register.  This is strictly a
   front-end flag, whereas DECL_REGISTER is used for code generation;
   they may differ for structures with volatile fields.  */
#define C_DECL_REGISTER(EXP) DECL_LANG_FLAG_4 (EXP)

/* Nonzero for a decl which either doesn't exist or isn't a prototype.
   N.B. Could be simplified if all built-in decls had complete prototypes
   (but this is presently difficult because some of them need FILE*).  */
#define C_DECL_ISNT_PROTOTYPE(EXP)			\
       (EXP == 0					\
	|| (TYPE_ARG_TYPES (TREE_TYPE (EXP)) == 0	\
	    && !DECL_BUILT_IN (EXP)))

/* For FUNCTION_TYPE, a hidden list of types of arguments.  The same as
   TYPE_ARG_TYPES for functions with prototypes, but created for functions
   without prototypes.  */
#define TYPE_ACTUAL_ARG_TYPES(NODE) TYPE_BINFO (NODE)

/* Save and restore the variables in this file and elsewhere
   that keep track of the progress of compilation of the current function.
   Used for nested functions.  */

struct language_function GTY(())
{
  struct c_language_function base;
  tree x_break_label;
  tree x_cont_label;
  struct c_switch * GTY((skip)) x_switch_stack;
  int returns_value;
  int returns_null;
  int returns_abnormally;
  int warn_about_return_type;
  int extern_inline;
};


/* in c-parse.in */
extern void c_parse_init (void);

/* in c-aux-info.c */
extern void gen_aux_info_record (tree, int, int, int);

/* in c-decl.c */
extern tree c_break_label;
extern tree c_cont_label;

extern int global_bindings_p (void);
extern void push_scope (void);
extern tree pop_scope (void);
extern void insert_block (tree);
extern tree pushdecl (tree);
extern void c_expand_body (tree);

extern void c_init_decl_processing (void);
extern void c_dup_lang_specific_decl (tree);
extern void c_print_identifier (FILE *, tree, int);
extern tree build_array_declarator (tree, tree, int, int);
extern tree build_enumerator (tree, tree);
extern void check_for_loop_decls (void);
extern void mark_forward_parm_decls (void);
extern int  complete_array_type (tree, tree, int);
extern void declare_parm_level (void);
extern void undeclared_variable (tree);
extern tree declare_label (tree);
extern tree define_label (location_t, tree);
extern void finish_decl (tree, tree, tree);
extern tree finish_enum (tree, tree, tree);
extern void finish_function (void);
extern tree finish_struct (tree, tree, tree);
extern tree get_parm_info (bool);
extern tree grokfield (tree, tree, tree);
extern tree groktypename (tree);
extern tree groktypename_in_parm_context (tree);
extern tree implicitly_declare (tree);
extern void keep_next_level (void);
extern tree lookup_name (tree);
extern void pending_xref_error (void);
extern void c_push_function_context (struct function *);
extern void c_pop_function_context (struct function *);
extern void push_parm_decl (tree);
extern tree pushdecl_top_level (tree);
extern tree set_array_declarator_type (tree, tree, int);
extern void shadow_tag (tree);
extern void shadow_tag_warned (tree, int);
extern tree start_enum (tree);
extern int  start_function (tree, tree, tree);
extern tree start_decl (tree, tree, int, tree);
extern tree start_struct (enum tree_code, tree);
extern void store_parm_decls (void);
extern tree xref_tag (enum tree_code, tree);
extern int c_expand_decl (tree);
extern void c_static_assembler_name (tree);
extern tree make_pointer_declarator (tree, tree);

/* in c-objc-common.c */
extern int c_disregard_inline_limits (tree);
extern int c_cannot_inline_tree_fn (tree *);
extern bool c_objc_common_init (void);
extern bool c_missing_noreturn_ok_p (tree);
extern tree c_objc_common_truthvalue_conversion (tree expr);
extern void c_objc_common_finish_file (void);
extern int defer_fn (tree);
extern bool c_warn_unused_global_decl (tree);
extern void c_initialize_diagnostics (diagnostic_context *);

#define c_build_type_variant(TYPE, CONST_P, VOLATILE_P)		  \
  c_build_qualified_type ((TYPE),				  \
			  ((CONST_P) ? TYPE_QUAL_CONST : 0) |	  \
			  ((VOLATILE_P) ? TYPE_QUAL_VOLATILE : 0))

#define c_sizeof_nowarn(T)  c_sizeof_or_alignof_type (T, SIZEOF_EXPR, 0)

/* in c-typeck.c */
extern struct c_switch *c_switch_stack;

extern tree require_complete_type (tree);
extern int same_translation_unit_p (tree, tree);
extern int comptypes (tree, tree);
extern tree c_size_in_bytes (tree);
extern bool c_mark_addressable (tree);
extern void c_incomplete_type_error (tree, tree);
extern tree c_type_promotes_to (tree);
extern tree composite_type (tree, tree);
extern tree build_component_ref (tree, tree);
extern tree build_indirect_ref (tree, const char *);
extern tree build_array_ref (tree, tree);
extern tree build_external_ref (tree, int);
extern tree parser_build_binary_op (enum tree_code, tree, tree);
extern void readonly_error (tree, const char *);
extern tree build_conditional_expr (tree, tree, tree);
extern tree build_compound_expr (tree);
extern tree c_cast_expr (tree, tree);
extern tree build_c_cast (tree, tree);
extern tree build_modify_expr (tree, enum tree_code, tree);
extern void store_init_value (tree, tree);
extern void error_init (const char *);
extern void pedwarn_init (const char *);
extern void start_init (tree, tree, int);
extern void finish_init (void);
extern void really_start_incremental_init (tree);
extern void push_init_level (int);
extern tree pop_init_level (int);
extern void set_init_index (tree, tree);
extern void set_init_label (tree);
extern void process_init_element (tree);
extern tree build_compound_literal (tree, tree);
extern void pedwarn_c90 (const char *, ...) ATTRIBUTE_PRINTF_1;
extern void pedwarn_c99 (const char *, ...) ATTRIBUTE_PRINTF_1;
extern tree c_start_case (tree);
extern void c_finish_case (tree);
extern tree build_asm_expr (tree, tree, tree, tree, bool);
extern tree build_asm_stmt (tree, tree);
extern tree c_convert_parm_for_inlining (tree, tree, tree, int);
extern int c_types_compatible_p (tree, tree);
extern tree c_begin_compound_stmt (bool);
extern tree c_end_compound_stmt (tree, bool);
extern void c_finish_if_stmt (location_t, tree, tree, tree, bool);
extern void c_finish_loop (location_t, tree, tree, tree, tree, tree, bool);
extern tree c_begin_stmt_expr (void);
extern tree c_finish_stmt_expr (tree);
extern tree c_process_expr_stmt (tree);
extern tree c_finish_expr_stmt (tree);
extern tree c_finish_return (tree);
extern tree c_finish_bc_stmt (tree *, bool);
extern tree c_finish_goto_label (tree);
extern tree c_finish_goto_ptr (tree);
extern tree build_offsetof (tree, tree);

/* Set to 0 at beginning of a function definition, set to 1 if
   a return statement that specifies a return value is seen.  */

extern int current_function_returns_value;

/* Set to 0 at beginning of a function definition, set to 1 if
   a return statement with no argument is seen.  */

extern int current_function_returns_null;

/* Set to 0 at beginning of a function definition, set to 1 if
   a call to a noreturn function is seen.  */

extern int current_function_returns_abnormally;

/* Nonzero means we are reading code that came from a system header file.  */

extern int system_header_p;

/* True means global_bindings_p should return false even if the scope stack
   says we are in file scope.  */

extern bool c_override_global_bindings_to_false;

/* True means we've initialized exception handling.  */
extern bool c_eh_initialized_p;

/* In c-decl.c */
extern void c_finish_incomplete_decl (tree);
extern void *get_current_scope (void);
extern void objc_mark_locals_volatile (void *);
extern void c_write_global_declarations (void);

extern GTY(()) tree static_ctors;
extern GTY(()) tree static_dtors;

/* In order for the format checking to accept the C frontend
   diagnostic framework extensions, you must include this file before
   toplev.h, not after.  */
#define GCC_DIAG_STYLE __gcc_cdiag__

#endif /* ! GCC_C_TREE_H */
