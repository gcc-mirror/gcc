/* Definitions for c-common.c.
   Copyright (C) 1987, 1993, 1994, 1995, 1997, 1998,
   1999, 2000 Free Software Foundation, Inc.

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
    CTI_WCHAR_TYPE,
    CTI_SIGNED_WCHAR_TYPE,
    CTI_UNSIGNED_WCHAR_TYPE,
    CTI_WIDEST_INT_LIT_TYPE,
    CTI_WIDEST_UINT_LIT_TYPE,

    CTI_CHAR_ARRAY_TYPE,
    CTI_WCHAR_ARRAY_TYPE,
    CTI_INT_ARRAY_TYPE,
    CTI_STRING_TYPE,
    CTI_CONST_STRING_TYPE,

    CTI_BOOLEAN_TYPE,
    CTI_BOOLEAN_TRUE,
    CTI_BOOLEAN_FALSE,
    CTI_DEFAULT_FUNCTION_TYPE,
    CTI_VOID_LIST,

    CTI_VOID_FTYPE,
    CTI_VOID_FTYPE_PTR,
    CTI_INT_FTYPE_INT,
    CTI_PTR_FTYPE_SIZETYPE,

    CTI_G77_INTEGER_TYPE,
    CTI_G77_UINTEGER_TYPE,
    CTI_G77_LONGINT_TYPE,
    CTI_G77_ULONGINT_TYPE,

    CTI_MAX
};

extern tree c_global_trees[CTI_MAX];

typedef enum c_language_kind
{
  clk_c,           /* A dialect of C: K&R C, ANSI/ISO C89, C2000,
		       etc. */
  clk_cplusplus,   /* ANSI/ISO C++ */
  clk_objective_c  /* Objective C */
} 
c_language_kind;

/* The variant of the C language being processed.  Each C language
   front-end defines this variable.  */
extern c_language_kind c_language;

#define wchar_type_node			c_global_trees[CTI_WCHAR_TYPE]
#define signed_wchar_type_node		c_global_trees[CTI_SIGNED_WCHAR_TYPE]
#define unsigned_wchar_type_node	c_global_trees[CTI_UNSIGNED_WCHAR_TYPE]
#define widest_integer_literal_type_node c_global_trees[CTI_WIDEST_INT_LIT_TYPE]
#define widest_unsigned_literal_type_node c_global_trees[CTI_WIDEST_UINT_LIT_TYPE]

#define boolean_type_node		c_global_trees[CTI_BOOLEAN_TYPE]
#define boolean_true_node		c_global_trees[CTI_BOOLEAN_TRUE]
#define boolean_false_node		c_global_trees[CTI_BOOLEAN_FALSE]

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

/* g77 integer types, which which must be kept in sync with f/com.h */
#define g77_integer_type_node		c_global_trees[CTI_G77_INTEGER_TYPE]
#define g77_uinteger_type_node		c_global_trees[CTI_G77_UINTEGER_TYPE]
#define g77_longint_type_node		c_global_trees[CTI_G77_LONGINT_TYPE]
#define g77_ulongint_type_node		c_global_trees[CTI_G77_ULONGINT_TYPE]

/* Pointer to function to generate the VAR_DECL for __FUNCTION__ etc.
   ID is the identifier to use, NAME is the string.
   TYPE_DEP indicates whether it depends on type of the function or not
   (i.e. __PRETTY_FUNCTION__).  */

extern tree (*make_fname_decl)                  PARAMS ((tree, const char *, int));

extern void declare_function_name		PARAMS ((void));
extern void decl_attributes			PARAMS ((tree, tree, tree));
extern void init_function_format_info		PARAMS ((void));
extern void check_function_format		PARAMS ((tree, tree, tree));
extern void c_apply_type_quals_to_decl		PARAMS ((int, tree));
/* Print an error message for invalid operands to arith operation CODE.
   NOP_EXPR is used as a special case (see truthvalue_conversion).  */
extern void binary_op_error			PARAMS ((enum tree_code));
extern void c_expand_expr_stmt			PARAMS ((tree));
extern void c_expand_start_cond			PARAMS ((tree, int, int));
extern void c_expand_start_else			PARAMS ((void));
extern void c_expand_end_cond			PARAMS ((void));
/* Validate the expression after `case' and apply default promotions.  */
extern tree check_case_value			PARAMS ((tree));
/* Concatenate a list of STRING_CST nodes into one STRING_CST.  */
extern tree combine_strings			PARAMS ((tree));
extern void constant_expression_warning		PARAMS ((tree));
extern tree convert_and_check			PARAMS ((tree, tree));
extern void overflow_warning			PARAMS ((tree));
extern void unsigned_conversion_warning		PARAMS ((tree, tree));
/* Read the rest of the current #-directive line.  */
#if USE_CPPLIB
extern char *get_directive_line			PARAMS ((void));
#define GET_DIRECTIVE_LINE() get_directive_line ()
#else
extern char *get_directive_line			PARAMS ((FILE *));
#define GET_DIRECTIVE_LINE() get_directive_line (finput)
#endif

/* Subroutine of build_binary_op, used for comparison operations.
   See if the operands have both been converted from subword integer types
   and, if so, perhaps change them both back to their original type.  */
extern tree shorten_compare			PARAMS ((tree *, tree *, tree *, enum tree_code *));
/* Prepare expr to be an argument of a TRUTH_NOT_EXPR,
   or validate its data type for an `if' or `while' statement or ?..: exp. */
extern tree truthvalue_conversion		PARAMS ((tree));
extern tree type_for_mode			PARAMS ((enum machine_mode, int));
extern tree type_for_size			PARAMS ((unsigned, int));

/* Build tree nodes and builtin functions common to both C and C++ language
   frontends.  */
extern void c_common_nodes_and_builtins		PARAMS ((int, int, int));

extern tree build_va_arg			PARAMS ((tree, tree));

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

extern int self_promoting_args_p		PARAMS ((tree));
extern tree simple_type_promotes_to		PARAMS ((tree));


/* These macros provide convenient access to the various _STMT nodes
   created when parsing template declarations.  */

/* IF_STMT accessors. These give access to the condtion of the if
   statement, the then block of the if statement, and the else block
   of the if stsatement if it exists. */
#define IF_COND(NODE)           TREE_OPERAND (IF_STMT_CHECK (NODE), 0)
#define THEN_CLAUSE(NODE)       TREE_OPERAND (IF_STMT_CHECK (NODE), 1)
#define ELSE_CLAUSE(NODE)       TREE_OPERAND (IF_STMT_CHECK (NODE), 2)

/* WHILE_STMT accessors. These give access to the condtion of the
   while statement and the body of the while statement, respectively. */
#define WHILE_COND(NODE)        TREE_OPERAND (WHILE_STMT_CHECK (NODE), 0)
#define WHILE_BODY(NODE)        TREE_OPERAND (WHILE_STMT_CHECK (NODE), 1)

/* DO_STMT accessors. These give access to the condition of the do
   statement and the body of the do statement, respectively. */
#define DO_COND(NODE)           TREE_OPERAND (DO_STMT_CHECK (NODE), 0)
#define DO_BODY(NODE)           TREE_OPERAND (DO_STMT_CHECK (NODE), 1)

/* RETURN_STMT accessor. This gives the expression associated with a
   return statement. */
#define RETURN_EXPR(NODE)       TREE_OPERAND (RETURN_STMT_CHECK (NODE), 0)

/* EXPR_STMT accessor. This gives the expression associated with an
   expression statement. */
#define EXPR_STMT_EXPR(NODE)    TREE_OPERAND (EXPR_STMT_CHECK (NODE), 0)

/* FOR_STMT accessors. These give access to the init statement,
   condition, update expression, and body of the for statement,
   respectively. */
#define FOR_INIT_STMT(NODE)     TREE_OPERAND (FOR_STMT_CHECK (NODE), 0)
#define FOR_COND(NODE)          TREE_OPERAND (FOR_STMT_CHECK (NODE), 1)
#define FOR_EXPR(NODE)          TREE_OPERAND (FOR_STMT_CHECK (NODE), 2)
#define FOR_BODY(NODE)          TREE_OPERAND (FOR_STMT_CHECK (NODE), 3)

/* SWITCH_STMT accessors. These give access to the condition and body
   of the switch statement, respectively. */
#define SWITCH_COND(NODE)       TREE_OPERAND (SWITCH_STMT_CHECK (NODE), 0)
#define SWITCH_BODY(NODE)       TREE_OPERAND (SWITCH_STMT_CHECK (NODE), 1)

/* CASE_LABEL accessors. These give access to the high and low values
   of a case label, respectively. */
#define CASE_LOW(NODE)          TREE_OPERAND (CASE_LABEL_CHECK (NODE), 0)
#define CASE_HIGH(NODE)         TREE_OPERAND (CASE_LABEL_CHECK (NODE), 1)

/* GOTO_STMT accessor. This gives access to the label associated with
   a goto statement. */
#define GOTO_DESTINATION(NODE)  TREE_OPERAND (GOTO_STMT_CHECK (NODE), 0)

/* COMPOUND_STMT accessor. This gives access to the TREE_LIST of
   statements assocated with a compound statement. The result is the
   first statement in the list. Succeeding nodes can be acccessed by
   calling TREE_CHAIN on a node in the list. */
#define COMPOUND_BODY(NODE)     TREE_OPERAND (COMPOUND_STMT_CHECK (NODE), 0)

/* ASM_STMT accessors. ASM_STRING returns a STRING_CST for the
   instruction (e.g., "mov x, y"). ASM_OUTPUTS, ASM_INPUTS, and
   ASM_CLOBBERS represent the outputs, inputs, and clobbers for the
   statement. */
#define ASM_CV_QUAL(NODE)       TREE_OPERAND (ASM_STMT_CHECK (NODE), 0)
#define ASM_STRING(NODE)        TREE_OPERAND (ASM_STMT_CHECK (NODE), 1)
#define ASM_OUTPUTS(NODE)       TREE_OPERAND (ASM_STMT_CHECK (NODE), 2)
#define ASM_INPUTS(NODE)        TREE_OPERAND (ASM_STMT_CHECK (NODE), 3)
#define ASM_CLOBBERS(NODE)      TREE_OPERAND (ASM_STMT_CHECK (NODE), 4)

/* DECL_STMT accessor. This gives access to the DECL associated with
   the given declaration statement. */ 
#define DECL_STMT_DECL(NODE)    TREE_OPERAND (DECL_STMT_CHECK (NODE), 0)

/* STMT_EXPR accessor. */
#define STMT_EXPR_STMT(NODE)    TREE_OPERAND (STMT_EXPR_CHECK (NODE), 0)

/* LABEL_STMT accessor. This gives access to the label associated with
   the given label statement. */
#define LABEL_STMT_LABEL(NODE)  TREE_OPERAND (LABEL_STMT_CHECK (NODE), 0)

/* Nonzero if this SCOPE_STMT is for the beginning of a scope.  */
#define SCOPE_BEGIN_P(NODE) \
  (TREE_LANG_FLAG_0 (SCOPE_STMT_CHECK (NODE))) 

/* Nonzero if this SCOPE_STMT is for the end of a scope.  */
#define SCOPE_END_P(NODE) \
  (!SCOPE_BEGIN_P (SCOPE_STMT_CHECK (NODE)))

/* The BLOCK containing the declarations contained in this scope.  */
#define SCOPE_STMT_BLOCK(NODE) \
  (TREE_OPERAND (SCOPE_STMT_CHECK (NODE), 0))

/* Nonzero for a SCOPE_STMT if there were no variables in this scope.  */
#define SCOPE_NULLIFIED_P(NODE) \
  (SCOPE_STMT_BLOCK ((NODE)) == NULL_TREE)

/* Nonzero for a SCOPE_STMT which represents a lexical scope, but
   which should be treated as non-existant from the point of view of
   running cleanup actions.  */
#define SCOPE_NO_CLEANUPS_P(NODE) \
  (TREE_LANG_FLAG_3 (SCOPE_STMT_CHECK (NODE)))

/* Nonzero for a SCOPE_STMT if this statement is for a partial scope.
   For example, in:
  
     S s;
     l:
     S s2;
     goto l;

   there is (implicitly) a new scope after `l', even though there are
   no curly braces.  In particular, when we hit the goto, we must
   destroy s2 and then re-construct it.  For the implicit scope,
   SCOPE_PARTIAL_P will be set.  */
#define SCOPE_PARTIAL_P(NODE) \
  (TREE_LANG_FLAG_4 (SCOPE_STMT_CHECK (NODE)))

/* Nonzero for an ASM_STMT if the assembly statement is volatile.  */
#define ASM_VOLATILE_P(NODE)			\
  (ASM_CV_QUAL (ASM_STMT_CHECK (NODE)) != NULL_TREE)

/* The line-number at which a statement began.  But if
   STMT_LINENO_FOR_FN_P does holds, then this macro gives the
   line number for the end of the current function instead.  */
#define STMT_LINENO(NODE)			\
  (TREE_COMPLEXITY ((NODE)))

/* If non-zero, the STMT_LINENO for NODE is the line at which the
   function ended.  */
#define STMT_LINENO_FOR_FN_P(NODE) 		\
  (TREE_LANG_FLAG_2 ((NODE)))


#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) SYM,

enum c_tree_code {
  C_DUMMY_TREE_CODE = LAST_AND_UNUSED_TREE_CODE,
#include "c-common.def"
  LAST_C_TREE_CODE
};

#undef DEFTREECODE

extern void add_c_tree_codes		        PARAMS ((void));
