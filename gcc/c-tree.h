/* Definitions for C parsing and type checking.
   Copyright (C) 1987, 1993, 1994, 1995, 1997, 1998,
   1999, 2000, 2001, 2002 Free Software Foundation, Inc.

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
  tree global_value;
  tree local_value;
  tree label_value;
  tree implicit_decl;
  tree error_locus;
  tree limbo_value;
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

/* Language-specific declaration information.  */

struct lang_decl GTY(())
{
  struct c_lang_decl base;
  /* The return types and parameter types may have variable size.
     This is a list of any SAVE_EXPRs that need to be evaluated to
     compute those sizes.  */
  tree pending_sizes;
};

/* Macros for access to language-specific slots in an identifier.  */
/* Each of these slots contains a DECL node or null.  */

/* This represents the value which the identifier has in the
   file-scope namespace.  */
#define IDENTIFIER_GLOBAL_VALUE(NODE)	\
  (((struct lang_identifier *) (NODE))->global_value)
/* This represents the value which the identifier has in the current
   scope.  */
#define IDENTIFIER_LOCAL_VALUE(NODE)	\
  (((struct lang_identifier *) (NODE))->local_value)
/* This represents the value which the identifier has as a label in
   the current label scope.  */
#define IDENTIFIER_LABEL_VALUE(NODE)	\
  (((struct lang_identifier *) (NODE))->label_value)
/* This records the extern decl of this identifier, if it has had one
   at any point in this compilation.  */
#define IDENTIFIER_LIMBO_VALUE(NODE)	\
  (((struct lang_identifier *) (NODE))->limbo_value)
/* This records the implicit function decl of this identifier, if it
   has had one at any point in this compilation.  */
#define IDENTIFIER_IMPLICIT_DECL(NODE)	\
  (((struct lang_identifier *) (NODE))->implicit_decl)
/* This is the last function in which we printed an "undefined variable"
   message for this identifier.  Value is a FUNCTION_DECL or null.  */
#define IDENTIFIER_ERROR_LOCUS(NODE)	\
  (((struct lang_identifier *) (NODE))->error_locus)

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

/* In an IDENTIFIER_NODE, nonzero if this identifier is actually a
   keyword.  C_RID_CODE (node) is then the RID_* value of the keyword,
   and C_RID_YYCODE is the token number wanted by Yacc.  */
#define C_IS_RESERVED_WORD(ID) TREE_LANG_FLAG_0 (ID)

/* This function was declared inline.  This flag controls the linkage
   semantics of 'inline'; whether or not the function is inlined is
   controlled by DECL_INLINE.  */
#define DECL_DECLARED_INLINE_P(NODE) \
  (DECL_LANG_SPECIFIC (NODE)->base.declared_inline)

/* In a RECORD_TYPE, a sorted array of the fields of the type.  */
struct lang_type GTY(())
{
  int len;
  tree GTY((length ("%h.len"))) elts[1];
};

/* Record whether a type or decl was written with nonconstant size.
   Note that TYPE_SIZE may have simplified to a constant.  */
#define C_TYPE_VARIABLE_SIZE(TYPE) TYPE_LANG_FLAG_1 (TYPE)
#define C_DECL_VARIABLE_SIZE(TYPE) DECL_LANG_FLAG_0 (TYPE)

#if 0 /* Not used.  */
/* Record whether a decl for a function or function pointer has
   already been mentioned (in a warning) because it was called
   but didn't have a prototype.  */
#define C_MISSING_PROTOTYPE_WARNED(DECL) DECL_LANG_FLAG_2 (DECL)
#endif

/* Store a value in that field.  */
#define C_SET_EXP_ORIGINAL_CODE(EXP, CODE) \
  (TREE_COMPLEXITY (EXP) = (int) (CODE))

/* Record whether a typedef for type `int' was actually `signed int'.  */
#define C_TYPEDEF_EXPLICITLY_SIGNED(EXP) DECL_LANG_FLAG_1 (EXP)

/* For a FUNCTION_DECL, nonzero if it was defined without an explicit
   return type.  */
#define C_FUNCTION_IMPLICIT_INT(EXP) DECL_LANG_FLAG_1 (EXP)

/* Nonzero for a declaration of a built in function if there has been no
   occasion that would declare the function in ordinary C.
   Using the function draws a pedantic warning in this case.  */
#define C_DECL_ANTICIPATED(EXP) DECL_LANG_FLAG_3 (EXP)

/* For FUNCTION_TYPE, a hidden list of types of arguments.  The same as
   TYPE_ARG_TYPES for functions with prototypes, but created for functions
   without prototypes.  */
#define TYPE_ACTUAL_ARG_TYPES(NODE) TYPE_BINFO (NODE)


/* in c-lang.c and objc-act.c */
extern tree lookup_interface			PARAMS ((tree));
extern tree is_class_name			PARAMS ((tree));
extern tree objc_is_id				PARAMS ((tree));
extern void objc_check_decl			PARAMS ((tree));
extern void finish_file				PARAMS ((void));
extern int objc_comptypes                 	PARAMS ((tree, tree, int));
extern tree objc_message_selector		PARAMS ((void));
extern tree lookup_objc_ivar			PARAMS ((tree));


/* in c-parse.in */
extern void c_parse_init			PARAMS ((void));

/* in c-aux-info.c */
extern void gen_aux_info_record                 PARAMS ((tree, int, int, int));

/* in c-decl.c */
extern int global_bindings_p			PARAMS ((void));
extern int kept_level_p				PARAMS ((void));
extern tree getdecls				PARAMS ((void));
extern void pushlevel				PARAMS ((int));
extern tree poplevel				PARAMS ((int,int, int));
extern void insert_block			PARAMS ((tree));
extern void set_block				PARAMS ((tree));
extern tree pushdecl				PARAMS ((tree));

extern void c_insert_default_attributes		PARAMS ((tree));
extern void c_init_decl_processing		PARAMS ((void));
extern void c_dup_lang_specific_decl		PARAMS ((tree));
extern void c_print_identifier			PARAMS ((FILE *, tree, int));
extern tree build_array_declarator              PARAMS ((tree, tree, int, int));
extern tree build_enumerator                    PARAMS ((tree, tree));
extern void check_for_loop_decls                PARAMS ((void));
extern void clear_parm_order                    PARAMS ((void));
extern int  complete_array_type                 PARAMS ((tree, tree, int));
extern void declare_parm_level                  PARAMS ((int));
extern tree define_label                        PARAMS ((const char *, int,
							 tree));
extern void finish_decl                         PARAMS ((tree, tree, tree));
extern tree finish_enum                         PARAMS ((tree, tree, tree));
extern void finish_function                     PARAMS ((int, int));
extern tree finish_struct                       PARAMS ((tree, tree, tree));
extern tree get_parm_info                       PARAMS ((int));
extern tree grokfield                           PARAMS ((const char *, int, tree, tree, tree));
extern tree groktypename                        PARAMS ((tree));
extern tree groktypename_in_parm_context        PARAMS ((tree));
extern tree implicitly_declare                  PARAMS ((tree));
extern void implicit_decl_warning               PARAMS ((tree));
extern int  in_parm_level_p                     PARAMS ((void));
extern void keep_next_level                     PARAMS ((void));
extern tree lookup_name                         PARAMS ((tree));
extern tree lookup_name_current_level		PARAMS ((tree));
extern void parmlist_tags_warning               PARAMS ((void));
extern void pending_xref_error                  PARAMS ((void));
extern void c_push_function_context             PARAMS ((struct function *));
extern void c_pop_function_context              PARAMS ((struct function *));
extern void pop_label_level                     PARAMS ((void));
extern void push_label_level                    PARAMS ((void));
extern void push_parm_decl                      PARAMS ((tree));
extern tree pushdecl_top_level                  PARAMS ((tree));
extern void pushtag                             PARAMS ((tree, tree));
extern tree set_array_declarator_type           PARAMS ((tree, tree, int));
extern tree shadow_label                        PARAMS ((tree));
extern void shadow_tag                          PARAMS ((tree));
extern void shadow_tag_warned                   PARAMS ((tree, int));
extern tree start_enum                          PARAMS ((tree));
extern int  start_function                      PARAMS ((tree, tree, tree));
extern tree start_decl                          PARAMS ((tree, tree, int,
							 tree));
extern tree start_struct                        PARAMS ((enum tree_code, tree));
extern void store_parm_decls                    PARAMS ((void));
extern tree xref_tag                            PARAMS ((enum tree_code, tree));
extern tree c_begin_compound_stmt               PARAMS ((void));
extern void c_expand_deferred_function          PARAMS ((tree));
extern void c_expand_decl_stmt                  PARAMS ((tree));
extern tree make_pointer_declarator		PARAMS ((tree, tree));

/* in c-objc-common.c */
extern int c_disregard_inline_limits		PARAMS ((tree));
extern int c_cannot_inline_tree_fn		PARAMS ((tree *));
extern const char *c_objc_common_init		PARAMS ((const char *));
extern int c_missing_noreturn_ok_p		PARAMS ((tree));
extern void c_objc_common_finish_file		PARAMS ((void));
extern int defer_fn				PARAMS ((tree));
extern bool c_warn_unused_global_decl		PARAMS ((tree));

#define c_build_type_variant(TYPE, CONST_P, VOLATILE_P)		  \
  c_build_qualified_type ((TYPE),				  \
			  ((CONST_P) ? TYPE_QUAL_CONST : 0) |	  \
			  ((VOLATILE_P) ? TYPE_QUAL_VOLATILE : 0))

#define c_sizeof_nowarn(T)  c_sizeof_or_alignof_type (T, SIZEOF_EXPR, 0)
/* in c-typeck.c */
extern tree require_complete_type		PARAMS ((tree));
extern int comptypes				PARAMS ((tree, tree));
extern tree c_size_in_bytes                     PARAMS ((tree));
extern bool c_mark_addressable			PARAMS ((tree));
extern void c_incomplete_type_error		PARAMS ((tree, tree));
extern tree c_type_promotes_to			PARAMS ((tree));
extern tree build_component_ref                 PARAMS ((tree, tree));
extern tree build_indirect_ref                  PARAMS ((tree, const char *));
extern tree build_array_ref                     PARAMS ((tree, tree));
extern tree build_external_ref			PARAMS ((tree, int));
extern tree parser_build_binary_op              PARAMS ((enum tree_code,
							 tree, tree));
extern int c_tree_expr_nonnegative_p          	PARAMS ((tree));
extern void readonly_warning			PARAMS ((tree, const char *));
extern tree build_conditional_expr              PARAMS ((tree, tree, tree));
extern tree build_compound_expr                 PARAMS ((tree));
extern tree c_cast_expr				PARAMS ((tree, tree));
extern tree build_c_cast	                PARAMS ((tree, tree));
extern tree build_modify_expr                   PARAMS ((tree, enum tree_code,
							 tree));
extern void store_init_value                    PARAMS ((tree, tree));
extern void error_init				PARAMS ((const char *));
extern void pedwarn_init			PARAMS ((const char *));
extern void start_init				PARAMS ((tree, tree, int));
extern void finish_init				PARAMS ((void));
extern void really_start_incremental_init	PARAMS ((tree));
extern void push_init_level			PARAMS ((int));
extern tree pop_init_level			PARAMS ((int));
extern void set_init_index			PARAMS ((tree, tree));
extern void set_init_label			PARAMS ((tree));
extern void process_init_element		PARAMS ((tree));
extern tree build_compound_literal		PARAMS ((tree, tree));
extern void pedwarn_c99				PARAMS ((const char *, ...))
							ATTRIBUTE_PRINTF_1;
extern tree c_start_case                        PARAMS ((tree));
extern void c_finish_case                       PARAMS ((void));
extern tree simple_asm_stmt			PARAMS ((tree));
extern tree build_asm_stmt			PARAMS ((tree, tree, tree,
							 tree, tree));
extern tree c_convert_parm_for_inlining		PARAMS ((tree, tree, tree));

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
extern void c_finish_incomplete_decl PARAMS ((tree));

extern GTY(()) tree static_ctors;
extern GTY(()) tree static_dtors;

#endif /* ! GCC_C_TREE_H */
