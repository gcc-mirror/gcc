/* Definitions for C parsing and type checking.
   Copyright (C) 1987 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Language-dependent contents of an identifier.  */

/* The limbo_value is used for block level extern declarations, which need
   to be type checked against subsequent extern declarations.  They can't
   be referenced after they fall out of scope, so they can't be global.  */

struct lang_identifier
{
  struct tree_identifier ignore;
  tree global_value, local_value, label_value, implicit_decl;
  tree error_locus, limbo_value;
};

/* Macros for access to language-specific slots in an identifier.  */
/* Each of these slots contains a DECL node or null.  */

/* This represents the value which the identifier has in the
   file-scope namespace.  */
#define IDENTIFIER_GLOBAL_VALUE(NODE)	\
  (((struct lang_identifier *)(NODE))->global_value)
/* This represents the value which the identifier has in the current
   scope.  */
#define IDENTIFIER_LOCAL_VALUE(NODE)	\
  (((struct lang_identifier *)(NODE))->local_value)
/* This represents the value which the identifier has as a label in
   the current label scope.  */
#define IDENTIFIER_LABEL_VALUE(NODE)	\
  (((struct lang_identifier *)(NODE))->label_value)
/* This records the extern decl of this identifier, if it has had one
   at any point in this compilation.  */
#define IDENTIFIER_LIMBO_VALUE(NODE)	\
  (((struct lang_identifier *)(NODE))->limbo_value)
/* This records the implicit function decl of this identifier, if it
   has had one at any point in this compilation.  */
#define IDENTIFIER_IMPLICIT_DECL(NODE)	\
  (((struct lang_identifier *)(NODE))->implicit_decl)
/* This is the last function in which we printed an "undefined variable"
   message for this identifier.  Value is a FUNCTION_DECL or null.  */
#define IDENTIFIER_ERROR_LOCUS(NODE)	\
  (((struct lang_identifier *)(NODE))->error_locus)

/* In identifiers, C uses the following fields in a special way:
   TREE_PUBLIC        to record that there was a previous local extern decl.
   TREE_USED          to record that such a decl was used.
   TREE_ADDRESSABLE   to record that the address of such a decl was used.  */

/* Nonzero means reject anything that ANSI standard C forbids.  */
extern int pedantic;

/* In a RECORD_TYPE or UNION_TYPE, nonzero if any component is read-only.  */
#define C_TYPE_FIELDS_READONLY(type) TREE_LANG_FLAG_1 (type)

/* In a RECORD_TYPE or UNION_TYPE, nonzero if any component is volatile.  */
#define C_TYPE_FIELDS_VOLATILE(type) TREE_LANG_FLAG_2 (type)

/* In a RECORD_TYPE or UNION_TYPE or ENUMERAL_TYPE
   nonzero if the definition of the type has already started.  */
#define C_TYPE_BEING_DEFINED(type) TYPE_LANG_FLAG_0 (type)

/* In a RECORD_TYPE, a sorted array of the fields of the type.  */
struct lang_type
{
  int len;
  tree elts[1];
};

/* Mark which labels are explicitly declared.
   These may be shadowed, and may be referenced from nested functions.  */
#define C_DECLARED_LABEL_FLAG(label) TREE_LANG_FLAG_1 (label)

/* Record whether a type or decl was written with nonconstant size.
   Note that TYPE_SIZE may have simplified to a constant.  */
#define C_TYPE_VARIABLE_SIZE(type) TYPE_LANG_FLAG_1 (type)
#define C_DECL_VARIABLE_SIZE(type) DECL_LANG_FLAG_0 (type)

/* Record in each node resulting from a binary operator
   what operator was specified for it.  */
#define C_EXP_ORIGINAL_CODE(exp) ((enum tree_code) TREE_COMPLEXITY (exp))

#if 0 /* Not used.  */
/* Record whether a decl for a function or function pointer has
   already been mentioned (in a warning) because it was called
   but didn't have a prototype.  */
#define C_MISSING_PROTOTYPE_WARNED(decl) DECL_LANG_FLAG_2(decl)
#endif

/* Store a value in that field.  */
#define C_SET_EXP_ORIGINAL_CODE(exp, code) \
  (TREE_COMPLEXITY (exp) = (int)(code))

/* Record whether a typedef for type `int' was actually `signed int'.  */
#define C_TYPEDEF_EXPLICITLY_SIGNED(exp) DECL_LANG_FLAG_1 ((exp))

/* Nonzero for a declaration of a built in function if there has been no
   occasion that would declare the function in ordinary C.
   Using the function draws a pedantic warning in this case.  */
#define C_DECL_ANTICIPATED(exp) DECL_LANG_FLAG_3 ((exp))

/* For FUNCTION_TYPE, a hidden list of types of arguments.  The same as
   TYPE_ARG_TYPES for functions with prototypes, but created for functions
   without prototypes.  */
#define TYPE_ACTUAL_ARG_TYPES(NODE) TYPE_NONCOPIED_PARTS (NODE)

/* Nonzero if the type T promotes to itself.
   ANSI C states explicitly the list of types that promote;
   in particular, short promotes to int even if they have the same width.  */
#define C_PROMOTING_INTEGER_TYPE_P(t)				\
  (TREE_CODE ((t)) == INTEGER_TYPE				\
   && (TYPE_MAIN_VARIANT (t) == char_type_node			\
       || TYPE_MAIN_VARIANT (t) == signed_char_type_node	\
       || TYPE_MAIN_VARIANT (t) == unsigned_char_type_node	\
       || TYPE_MAIN_VARIANT (t) == short_integer_type_node	\
       || TYPE_MAIN_VARIANT (t) == short_unsigned_type_node))

/* in c-typecheck.c */
extern tree build_component_ref (), build_conditional_expr (), build_compound_expr ();
extern tree build_unary_op (), build_binary_op (), build_function_call ();
extern tree parser_build_binary_op ();
extern tree build_indirect_ref (), build_array_ref (), build_c_cast ();
extern tree build_modify_expr ();
extern tree c_sizeof (), c_alignof (), c_alignof_expr ();
extern void store_init_value ();
extern tree digest_init ();
extern tree c_expand_start_case ();
extern tree default_conversion ();

/* Given two integer or real types, return the type for their sum.
   Given two compatible ANSI C types, returns the merged type.  */

extern tree common_type ();

/* in c-decl.c */
extern tree build_label ();

extern int start_function ();
extern void finish_function ();
extern void store_parm_decls ();
extern tree get_parm_info ();
extern tree combine_parm_decls ();

extern void pushlevel ();
extern tree poplevel ();

extern tree groktypename (), lookup_name ();

extern tree lookup_label (), define_label (), shadow_label ();

extern tree implicitly_declare (), getdecls (), gettags ();

extern tree start_decl ();
extern void finish_decl ();

extern tree start_struct (), finish_struct (), xref_tag ();
extern tree grokfield ();

extern tree start_enum (), finish_enum ();
extern tree build_enumerator ();

extern tree make_index_type ();

/* Add qualifiers to a type, in the fashion for C.  */
extern tree c_build_type_variant ();

/* Declare a predefined function.  Return the declaration.  */
extern tree builtin_function ();

/* Functions in c-common.c: */

/* Concatenate a list of STRING_CST nodes into one STRING_CST.  */
extern tree combine_strings ();

/* Validate the expression after `case' and apply default promotions.  */
extern tree check_case_value ();

/* Print an error message for invalid operands to arith operation CODE.
   NOP_EXPR is used as a special case (see truthvalue_conversion).  */

extern void binary_op_error ();

/* Subroutine of build_binary_op, used for comparison operations.
   See if the operands have both been converted from subword integer types
   and, if so, perhaps change them both back to their original type.  */

extern tree shorten_compare ();

/* Read the rest of the current #-directive line.  */
extern char *get_directive_line ();

/* Prepare expr to be an argument of a TRUTH_NOT_EXPR,
   or validate its data type for an `if' or `while' statement or ?..: exp. */
extern tree truthvalue_conversion ();

extern int maybe_objc_comptypes ();
extern tree maybe_building_objc_message_expr ();

/* Standard named or nameless data types of the C compiler.  */

extern tree short_integer_type_node, integer_type_node;
extern tree long_integer_type_node, long_long_integer_type_node;
extern tree short_unsigned_type_node, unsigned_type_node;
extern tree long_unsigned_type_node, long_long_unsigned_type_node;
extern tree ptrdiff_type_node;
extern tree unsigned_char_type_node, signed_char_type_node, char_type_node;
extern tree wchar_type_node, signed_wchar_type_node, unsigned_wchar_type_node;
extern tree float_type_node, double_type_node, long_double_type_node;
extern tree intQI_type_node, unsigned_intQI_type_node;
extern tree intHI_type_node, unsigned_intHI_type_node;
extern tree intSI_type_node, unsigned_intSI_type_node;
extern tree intDI_type_node, unsigned_intDI_type_node;
extern tree void_type_node, ptr_type_node, const_ptr_type_node;
extern tree string_type_node, const_string_type_node;
extern tree char_array_type_node, int_array_type_node, wchar_array_type_node;
extern tree default_function_type;
extern tree double_ftype_double, double_ftype_double_double;
extern tree int_ftype_int, long_ftype_long;
extern tree void_ftype_ptr_ptr_int, int_ftype_ptr_ptr_int;
extern tree void_ftype_ptr_int_int, string_ftype_ptr_ptr;
extern tree int_ftype_string_string, int_ftype_cptr_cptr_sizet;

/* Set to 0 at beginning of a function definition, set to 1 if
   a return statement that specifies a return value is seen.  */

extern int current_function_returns_value;

/* Set to 0 at beginning of a function definition, set to 1 if
   a return statement with no argument is seen.  */

extern int current_function_returns_null;

/* Nonzero means `$' can be in an identifier.  */

extern int dollars_in_ident;

/* Nonzero means allow type mismatches in conditional expressions;
   just make their values `void'.   */

extern int flag_cond_mismatch;

/* Nonzero means don't recognize the keyword `asm'.  */

extern int flag_no_asm;

/* Nonzero means ignore `#ident' directives.  */

extern int flag_no_ident;

/* Nonzero means warn about implicit declarations.  */

extern int warn_implicit;

/* Nonzero means give string constants the type `const char *'
   to get extra warnings from them.  These warnings will be too numerous
   to be useful, except in thoroughly ANSIfied programs.  */

extern int warn_write_strings;

/* Nonzero means warn about sizeof (function) or addition/subtraction
   of function pointers.  */

extern int warn_pointer_arith;

/* Nonzero means warn for all old-style non-prototype function decls.  */

extern int warn_strict_prototypes;

/* Nonzero means warn about multiple (redundant) decls for the same single
   variable or function.  */

extern int warn_redundant_decls;

/* Nonzero means warn about extern declarations of objects not at
   file-scope level and about *all* declarations of functions (whether
   extern or static) not at file-scope level.  Note that we exclude
   implicit function declarations.  To get warnings about those, use
   -Wimplicit.  */

extern int warn_nested_externs;

/* Nonzero means warn about pointer casts that can drop a type qualifier
   from the pointer target type.  */

extern int warn_cast_qual;

/* Warn about traditional constructs whose meanings changed in ANSI C.  */

extern int warn_traditional;

/* Warn about *printf or *scanf format/argument anomalies. */

extern int warn_format;

/* Warn about a subscript that has type char.  */

extern int warn_char_subscripts;

/* Warn if a type conversion is done that might have confusing results.  */

extern int warn_conversion;

/* Nonzero means do some things the same way PCC does.  */

extern int flag_traditional;

/* Nonzero means warn about suggesting putting in ()'s.  */

extern int warn_parentheses;

/* Nonzero means this is a function to call to perform comptypes
   on two record types.  */

extern int (*comptypes_record_hook) ();

/* Nonzero means we are reading code that came from a system header file.  */

extern int system_header_p;
