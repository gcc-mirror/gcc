/* Definitions for C parsing and type checking.
   Copyright (C) 1987, 1993, 1994, 1995, 1997, 1998,
   1999, 2000, 2001, 2002, 2003 Free Software Foundation, Inc.

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

/* Language-dependent contents of an identifier.  */

/* The limbo_value is used for block level extern declarations, which need
   to be type checked against subsequent extern declarations.  They can't
   be referenced after they fall out of scope, so they can't be global.

   The rid_code field is used for keywords.  It is in all
   lang_identifier nodes, because some keywords are only special in a
   particular context.  */

struct lang_identifier GTY(())
{
  struct c_common_identifier common_id;
  tree symbol_value;
  tree tag_value;
  tree label_value;
};

/* The resulting tree type.  */

union lang_tree_node
  GTY((desc ("TREE_CODE (&%h.generic) == IDENTIFIER_NODE"),
       chain_next ("TREE_CODE (&%h.generic) == INTEGER_TYPE ? (union lang_tree_node *)TYPE_NEXT_VARIANT (&%h.generic) : (union lang_tree_node *)TREE_CHAIN (&%h.generic)")))
{
  union tree_node GTY ((tag ("0"),
			desc ("tree_node_structure (&%h)")))
    generic;
  struct lang_identifier GTY ((tag ("1"))) identifier;
};

/* Language-specific declaration information.  */

struct lang_decl GTY(())
{
  /* The return types and parameter types may have variable size.
     This is a list of any SAVE_EXPRs that need to be evaluated to
     compute those sizes.  */
  tree pending_sizes;
};

/* Macros for access to language-specific slots in an identifier.  */
/* Each of these slots contains a DECL node or null.  */

/* The value of the identifier in the namespace of "ordinary identifiers"
   (data objects, enum constants, functions, typedefs).  */
#define IDENTIFIER_SYMBOL_VALUE(NODE)	\
  (((struct lang_identifier *) (NODE))->symbol_value)
/* The value of the identifier in the namespace of struct, union,
   and enum tags.  */
#define IDENTIFIER_TAG_VALUE(NODE)	\
  (((struct lang_identifier *) (NODE))->tag_value)
/* The value of the identifier in the namespace of labels.  */
#define IDENTIFIER_LABEL_VALUE(NODE)	\
  (((struct lang_identifier *) (NODE))->label_value)

/* In identifiers, C uses the following fields in a special way:
   TREE_PUBLIC        to record that there was a previous local extern decl.
   TREE_USED          to record that such a decl was used.
   TREE_ADDRESSABLE   to record that the address of such a decl was used.  */

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

/* In a RECORD_TYPE, a sorted array of the fields of the type.  */
struct lang_type GTY(())
{
  struct sorted_fields_type * GTY ((reorder ("resort_sorted_fields"))) s;
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

/* Nonzero for a declaration of an external object which is not
   currently in scope.  This is either a built-in declaration of
   a library function, before a real declaration has been seen,
   or a declaration that appeared in an inner scope that has ended.  */
#define C_DECL_INVISIBLE(EXP) DECL_LANG_FLAG_3 (EXP)

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

/* Values for the first parameter to poplevel.  */
#define KEEP_NO		0
#define KEEP_YES	1
#define KEEP_MAYBE	2

/* Save and restore the variables in this file and elsewhere
   that keep track of the progress of compilation of the current function.
   Used for nested functions.  */

struct language_function GTY(())
{
  struct c_language_function base;
  int returns_value;
  int returns_null;
  int returns_abnormally;
  int warn_about_return_type;
  int extern_inline;
  int x_in_iteration_stmt;
  int x_in_case_stmt;
};


/* in c-parse.in */
extern void c_parse_init (void);

/* in c-aux-info.c */
extern void gen_aux_info_record (tree, int, int, int);

/* in c-decl.c */
extern int c_in_iteration_stmt;
extern int c_in_case_stmt;

extern int global_bindings_p (void);
extern tree getdecls (void);
extern void pushlevel (int);
extern void insert_block (tree);
extern void set_block (tree);
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
extern tree get_parm_info (int);
extern tree grokfield (tree, tree, tree);
extern tree groktypename (tree);
extern tree groktypename_in_parm_context (tree);
extern tree implicitly_declare (tree);
extern int  in_parm_level_p (void);
extern void keep_next_level (void);
extern tree lookup_name (tree);
extern void pending_xref_error (void);
extern void c_push_function_context (struct function *);
extern void c_pop_function_context (struct function *);
extern void push_parm_decl (tree);
extern tree pushdecl_top_level (tree);
extern void pushtag (tree, tree);
extern tree set_array_declarator_type (tree, tree, int);
extern void shadow_tag (tree);
extern void shadow_tag_warned (tree, int);
extern tree start_enum (tree);
extern int  start_function (tree, tree, tree);
extern tree start_decl (tree, tree, int, tree);
extern tree start_struct (enum tree_code, tree);
extern void store_parm_decls (void);
extern tree xref_tag (enum tree_code, tree);
extern tree c_begin_compound_stmt (void);
extern void c_expand_deferred_function (tree);
extern void c_expand_decl_stmt (tree);
extern void c_static_assembler_name (tree);
extern tree make_pointer_declarator (tree, tree);
extern void merge_translation_unit_decls (void);

/* in c-objc-common.c */
extern int c_disregard_inline_limits (tree);
extern int c_cannot_inline_tree_fn (tree *);
extern bool c_objc_common_init (void);
extern int c_missing_noreturn_ok_p (tree);
extern void c_objc_common_finish_file (void);
extern int defer_fn (tree);
extern bool c_warn_unused_global_decl (tree);

#define c_build_type_variant(TYPE, CONST_P, VOLATILE_P)		  \
  c_build_qualified_type ((TYPE),				  \
			  ((CONST_P) ? TYPE_QUAL_CONST : 0) |	  \
			  ((VOLATILE_P) ? TYPE_QUAL_VOLATILE : 0))

#define c_sizeof_nowarn(T)  c_sizeof_or_alignof_type (T, SIZEOF_EXPR, 0)

/* in c-typeck.c */

/* For use with comptypes.  */
enum {
  COMPARE_STRICT = 0
};

extern tree require_complete_type (tree);
extern int comptypes (tree, tree, int);
extern tree c_size_in_bytes (tree);
extern bool c_mark_addressable (tree);
extern void c_incomplete_type_error (tree, tree);
extern tree c_type_promotes_to (tree);
extern tree build_component_ref (tree, tree);
extern tree build_indirect_ref (tree, const char *);
extern tree build_array_ref (tree, tree);
extern tree build_external_ref (tree, int);
extern tree parser_build_binary_op (enum tree_code, tree, tree);
extern int c_tree_expr_nonnegative_p (tree);
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
extern void c_finish_case (void);
extern tree simple_asm_stmt (tree);
extern tree build_asm_stmt (tree, tree, tree, tree, tree);
extern tree c_convert_parm_for_inlining (tree, tree, tree, int);

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
