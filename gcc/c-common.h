/* Definitions for c-common.c.
   Copyright (C) 1987, 93, 94, 95, 97, 98, 1999 Free Software Foundation, Inc.

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
Boston, MA 02111-1307, USA.  */

/* Standard named or nameless data types of the C compiler.  */

enum c_tree_index
{
    CTI_INTQI_TYPE,
    CTI_INTHI_TYPE,
    CTI_INTSI_TYPE,
    CTI_INTDI_TYPE,
    CTI_INTTI_TYPE,

    CTI_UINTQI_TYPE,
    CTI_UINTHI_TYPE,
    CTI_UINTSI_TYPE,
    CTI_UINTDI_TYPE,
    CTI_UINTTI_TYPE,
    
    CTI_SIGNED_CHAR_TYPE,
    CTI_UNSIGNED_CHAR_TYPE,
    CTI_WCHAR_TYPE,
    CTI_SIGNED_WCHAR_TYPE,
    CTI_UNSIGNED_WCHAR_TYPE,
    CTI_SHORT_INTEGER_TYPE,
    CTI_SHORT_UNSIGNED_TYPE,
    CTI_LONG_INTEGER_TYPE,
    CTI_LONG_UNSIGNED_TYPE,
    CTI_LONG_LONG_INTEGER_TYPE,
    CTI_LONG_LONG_UNSIGNED_TYPE,
    CTI_WIDEST_INT_LIT_TYPE,
    CTI_WIDEST_UINT_LIT_TYPE,

    CTI_COMPLEX_INTEGER_TYPE,
    CTI_COMPLEX_FLOAT_TYPE,
    CTI_COMPLEX_DOUBLE_TYPE,
    CTI_COMPLEX_LONG_DOUBLE_TYPE,

    CTI_FLOAT_TYPE,
    CTI_DOUBLE_TYPE,
    CTI_LONG_DOUBLE_TYPE,

    CTI_CHAR_ARRAY_TYPE,
    CTI_WCHAR_ARRAY_TYPE,
    CTI_INT_ARRAY_TYPE,
    CTI_STRING_TYPE,
    CTI_CONST_STRING_TYPE,

    CTI_CONST_PTR_TYPE,
    CTI_PTRDIFF_TYPE,

    CTI_BOOLEAN_TYPE,
    CTI_BOOLEAN_TRUE,
    CTI_BOOLEAN_FALSE,
    CTI_DEFAULT_FUNCTION_TYPE,
    CTI_VOID_LIST,

    CTI_VOID_FTYPE,
    CTI_VOID_FTYPE_PTR,
    CTI_INT_FTYPE_INT,
    CTI_PTR_FTYPE_SIZETYPE,
    
    CTI_MAX
};

extern tree c_global_trees[CTI_MAX];

#define intQI_type_node			c_global_trees[CTI_INTQI_TYPE]
#define intHI_type_node			c_global_trees[CTI_INTHI_TYPE]
#define intSI_type_node			c_global_trees[CTI_INTSI_TYPE]
#define intDI_type_node			c_global_trees[CTI_INTDI_TYPE]
#if HOST_BITS_PER_WIDE_INT >= 64
#define intTI_type_node			c_global_trees[CTI_INTTI_TYPE]
#endif

#define unsigned_intQI_type_node	c_global_trees[CTI_UINTQI_TYPE]
#define unsigned_intHI_type_node	c_global_trees[CTI_UINTHI_TYPE]
#define unsigned_intSI_type_node	c_global_trees[CTI_UINTSI_TYPE]
#define unsigned_intDI_type_node	c_global_trees[CTI_UINTDI_TYPE]
#if HOST_BITS_PER_WIDE_INT >= 64
#define unsigned_intTI_type_node	c_global_trees[CTI_UINTTI_TYPE]
#endif

#define signed_char_type_node		c_global_trees[CTI_SIGNED_CHAR_TYPE]
#define unsigned_char_type_node		c_global_trees[CTI_UNSIGNED_CHAR_TYPE]
#define wchar_type_node			c_global_trees[CTI_WCHAR_TYPE]
#define signed_wchar_type_node		c_global_trees[CTI_SIGNED_WCHAR_TYPE]
#define unsigned_wchar_type_node	c_global_trees[CTI_UNSIGNED_WCHAR_TYPE]
#define short_integer_type_node		c_global_trees[CTI_SHORT_INTEGER_TYPE]
#define short_unsigned_type_node	c_global_trees[CTI_SHORT_UNSIGNED_TYPE]
#define long_integer_type_node		c_global_trees[CTI_LONG_INTEGER_TYPE]
#define long_unsigned_type_node		c_global_trees[CTI_LONG_UNSIGNED_TYPE]
#define long_long_integer_type_node	c_global_trees[CTI_LONG_LONG_INTEGER_TYPE]
#define long_long_unsigned_type_node	c_global_trees[CTI_LONG_LONG_UNSIGNED_TYPE]
#define widest_integer_literal_type_node c_global_trees[CTI_WIDEST_INT_LIT_TYPE]
#define widest_unsigned_literal_type_node c_global_trees[CTI_WIDEST_UINT_LIT_TYPE]

#define float_type_node			c_global_trees[CTI_FLOAT_TYPE]
#define double_type_node		c_global_trees[CTI_DOUBLE_TYPE]
#define long_double_type_node		c_global_trees[CTI_LONG_DOUBLE_TYPE]

#define complex_integer_type_node	c_global_trees[CTI_COMPLEX_INTEGER_TYPE]
#define complex_float_type_node		c_global_trees[CTI_COMPLEX_FLOAT_TYPE]
#define complex_double_type_node	c_global_trees[CTI_COMPLEX_DOUBLE_TYPE]
#define complex_long_double_type_node	c_global_trees[CTI_COMPLEX_LONG_DOUBLE_TYPE]

#define boolean_type_node		c_global_trees[CTI_BOOLEAN_TYPE]
#define boolean_true_node		c_global_trees[CTI_BOOLEAN_TRUE]
#define boolean_false_node		c_global_trees[CTI_BOOLEAN_FALSE]

#define const_ptr_type_node		c_global_trees[CTI_CONST_PTR_TYPE]
#define ptrdiff_type_node		c_global_trees[CTI_PTRDIFF_TYPE]

#define char_array_type_node		c_global_trees[CTI_CHAR_ARRAY_TYPE]
#define wchar_array_type_node		c_global_trees[CTI_WCHAR_ARRAY_TYPE]
#define int_array_type_node		c_global_trees[CTI_INT_ARRAY_TYPE]
#define string_type_node		c_global_trees[CTI_STRING_TYPE]
#define const_string_type_node		c_global_trees[CTI_CONST_STRING_TYPE]

#define default_function_type		c_global_trees[CTI_DEFAULT_FUNCTION_TYPE]
#define void_list_node			c_global_trees[CTI_VOID_LIST]
#define void_ftype			c_global_trees[CTI_VOID_FTYPE]
#define void_ftype_ptr			c_global_trees[CTI_VOID_FTYPE_PTR]
#define int_ftype_int			c_global_trees[CTI_INT_FTYPE_INT]
#define ptr_ftype_sizetype		c_global_trees[CTI_PTR_FTYPE_SIZETYPE]

extern void declare_function_name		PROTO((void));
extern void decl_attributes			PROTO((tree, tree, tree));
extern void init_function_format_info		PROTO((void));
extern void check_function_format		PROTO((tree, tree, tree));
extern void c_apply_type_quals_to_decl		PROTO((int, tree));
extern int c_get_alias_set			PROTO((tree));
/* Print an error message for invalid operands to arith operation CODE.
   NOP_EXPR is used as a special case (see truthvalue_conversion).  */
extern void binary_op_error			PROTO((enum tree_code));
extern void c_expand_expr_stmt			PROTO((tree));
extern void c_expand_start_cond			PROTO((tree, int, int));
extern void c_expand_start_else			PROTO((void));
extern void c_expand_end_cond			PROTO((void));
/* Validate the expression after `case' and apply default promotions.  */
extern tree check_case_value			PROTO((tree));
/* Concatenate a list of STRING_CST nodes into one STRING_CST.  */
extern tree combine_strings			PROTO((tree));
extern void constant_expression_warning		PROTO((tree));
extern tree convert_and_check			PROTO((tree, tree));
extern void overflow_warning			PROTO((tree));
extern void unsigned_conversion_warning		PROTO((tree, tree));
/* Read the rest of the current #-directive line.  */
#if USE_CPPLIB
extern char *get_directive_line			PROTO((void));
#define GET_DIRECTIVE_LINE() get_directive_line ()
#else
extern char *get_directive_line			PROTO((FILE *));
#define GET_DIRECTIVE_LINE() get_directive_line (finput)
#endif

/* Subroutine of build_binary_op, used for comparison operations.
   See if the operands have both been converted from subword integer types
   and, if so, perhaps change them both back to their original type.  */
extern tree shorten_compare			PROTO((tree *, tree *, tree *, enum tree_code *));
/* Prepare expr to be an argument of a TRUTH_NOT_EXPR,
   or validate its data type for an `if' or `while' statement or ?..: exp. */
extern tree truthvalue_conversion		PROTO((tree));
extern tree type_for_mode			PROTO((enum machine_mode, int));
extern tree type_for_size			PROTO((unsigned, int));

/* Declare a predefined function.  Return the declaration.  This function is
   provided by each language frontend.  */
extern tree builtin_function			PROTO((const char *, tree, enum built_in_function, const char *));

/* Build tree nodes and builtin functions common to both C and C++ language
   frontends.  */
extern void c_common_nodes_and_builtins		PROTO((int, int, int));

extern tree build_va_arg			PROTO((tree, tree));
