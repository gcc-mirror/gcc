/* Header for code translation functions
   Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.
   Contributed by Paul Brook

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GFC_TRANS_H
#define GFC_TRANS_H

/* Mangled symbols take the form __module__name.  */
#define GFC_MAX_MANGLED_SYMBOL_LEN  (GFC_MAX_SYMBOL_LEN*2+4)

/* Struct for holding a block of statements.  It should be treated as an
   opaque entity and not modified directly.  This allows us to change the
   underlying representation of statement lists.  */
typedef struct
{
  tree head;
  unsigned int has_scope:1;
}
stmtblock_t;

/* a simplified expression */
typedef struct gfc_se
{
  /* Code blocks to be executed before and after using the value.  */
  stmtblock_t pre;
  stmtblock_t post;

  /* the result of the expression */
  tree expr;

  /* The length of a character string value.  */
  tree string_length;

  /* If set gfc_conv_variable will return an expression for the array
     descriptor. When set, want_pointer should also be set.
     If not set scalarizing variables will be substituted.  */
  unsigned descriptor_only:1;

  /* When this is set gfc_conv_expr returns the address of a variable.  Only
     applies to EXPR_VARIABLE nodes.
     Also used by gfc_conv_array_parameter. When set this indicates a pointer
     to the descriptor should be returned, rather than the descriptor itself.
   */
  unsigned want_pointer:1;

  /* An array function call returning without a temporary.  Also used for array
     pointer assignments.  */
  unsigned direct_byref:1;

  /* Ignore absent optional arguments.  Used for some intrinsics.  */
  unsigned ignore_optional:1;

  /* When this is set the data and offset fields of the returned descriptor
     are NULL.  Used by intrinsic size.  */
  unsigned data_not_needed:1;

  /* If set, gfc_conv_procedure_call does not put byref calls into se->pre.  */
  unsigned no_function_call:1;

  /* Scalarization parameters.  */
  struct gfc_se *parent;
  struct gfc_ss *ss;
  struct gfc_loopinfo *loop;
}
gfc_se;


/* Scalarization State chain.  Created by walking an expression tree before
   creating the scalarization loops. Then passed as part of a gfc_se structure
   to translate the expression inside the loop.  Note that these chains are
   terminated by gfc_se_terminator, not NULL.  A NULL pointer in a gfc_se
   indicates to gfc_conv_* that this is a scalar expression.
   Note that some member arrays correspond to scalarizer rank and others
   are the variable rank.  */

typedef struct gfc_ss_info
{
  int dimen;
  /* The ref that holds information on this section.  */
  gfc_ref *ref;
  /* The descriptor of this array.  */
  tree descriptor;
  /* holds the pointer to the data array.  */
  tree data;
  /* To move some of the array index calculation out of the innermost loop.  */
  tree offset;
  tree saved_offset;
  tree stride0;
  /* Holds the SS for a subscript.  Indexed by actual dimension.  */
  struct gfc_ss *subscript[GFC_MAX_DIMENSIONS];

  /* stride and delta are used to access this inside a scalarization loop.
     start is used in the calculation of these.  Indexed by scalarizer
     dimension.  */
  tree start[GFC_MAX_DIMENSIONS];
  tree end[GFC_MAX_DIMENSIONS];
  tree stride[GFC_MAX_DIMENSIONS];
  tree delta[GFC_MAX_DIMENSIONS];

  /* Translation from scalarizer dimensions to actual dimensions.
     actual = dim[scalarizer]  */
  int dim[GFC_MAX_DIMENSIONS];
}
gfc_ss_info;

typedef enum
{
  /* A scalar value.  This will be evaluated before entering the
     scalarization loop.  */
  GFC_SS_SCALAR,

  /* Like GFC_SS_SCALAR except it evaluates a pointer to the expression.
     Used for elemental function parameters.  */
  GFC_SS_REFERENCE,

  /* An array section.  Scalarization indices will be substituted during
     expression translation.  */
  GFC_SS_SECTION,

  /* A non-elemental function call returning an array.  The call is executed
     before entering the scalarization loop, storing the result in a
     temporary.  This temporary is then used inside the scalarization loop.
     Simple assignments, e.g. a(:) = fn(), are handled without a temporary
     as a special case.  */
  GFC_SS_FUNCTION,

  /* An array constructor.  The current implementation is sub-optimal in
     many cases.  It allocated a temporary, assigns the values to it, then
     uses this temporary inside the scalarization loop.  */
  GFC_SS_CONSTRUCTOR,

  /* A vector subscript.  The vector's descriptor is cached in the
     "descriptor" field of the associated gfc_ss_info.  */
  GFC_SS_VECTOR,

  /* A temporary array allocated by the scalarizer.  Its rank can be less
     than that of the assignment expression.  */
  GFC_SS_TEMP,

  /* An intrinsic function call.  Many intrinsic functions which map directly
     to library calls are created as GFC_SS_FUNCTION nodes.  */
  GFC_SS_INTRINSIC,
  
  /* A component of a derived type.  */
  GFC_SS_COMPONENT
}
gfc_ss_type;

/* SS structures can only belong to a single loopinfo.  They must be added
   otherwise they will not get freed.  */
typedef struct gfc_ss
{
  gfc_ss_type type;
  gfc_expr *expr;
  mpz_t *shape;
  tree string_length;
  union
  {
    /* If type is GFC_SS_SCALAR or GFC_SS_REFERENCE.  */
    struct
    {
      tree expr;
    }
    scalar;

    /* GFC_SS_TEMP.  */
    struct
    {
      /* The rank of the temporary.  May be less than the rank of the
         assigned expression.  */
      int dimen;
      tree type;
    }
    temp;
    /* All other types.  */
    gfc_ss_info info;
  }
  data;

  /* All the SS in a loop and linked through loop_chain.  The SS for an
     expression are linked by the next pointer.  */
  struct gfc_ss *loop_chain;
  struct gfc_ss *next;

  /* This is used by assignments requiring temporaries. The bits specify which
     loops the terms appear in.  This will be 1 for the RHS expressions,
     2 for the LHS expressions, and 3(=1|2) for the temporary.  The bit
     'where' suppresses precalculation of scalars in WHERE assignments.  */
  unsigned useflags:2, where:1;
}
gfc_ss;
#define gfc_get_ss() XCNEW (gfc_ss)

/* The contents of this aren't actually used.  A NULL SS chain indicates a
   scalar expression, so this pointer is used to terminate SS chains.  */
extern gfc_ss * const gfc_ss_terminator;

/* Holds information about an expression while it is being scalarized.  */
typedef struct gfc_loopinfo
{
  stmtblock_t pre;
  stmtblock_t post;

  int dimen;

  /* All the SS involved with this loop.  */
  gfc_ss *ss;
  /* The SS describing the temporary used in an assignment.  */
  gfc_ss *temp_ss;

  /* The scalarization loop index variables.  */
  tree loopvar[GFC_MAX_DIMENSIONS];

  /* The bounds of the scalarization loops.  */
  tree from[GFC_MAX_DIMENSIONS];
  tree to[GFC_MAX_DIMENSIONS];
  gfc_ss *specloop[GFC_MAX_DIMENSIONS];

  /* The code member contains the code for the body of the next outer loop.  */
  stmtblock_t code[GFC_MAX_DIMENSIONS];

  /* Order in which the dimensions should be looped, innermost first.  */
  int order[GFC_MAX_DIMENSIONS];

  /* The number of dimensions for which a temporary is used.  */
  int temp_dim;

  /* If set we don't need the loop variables.  */
  unsigned array_parameter:1;
}
gfc_loopinfo;


/* Information about a symbol that has been shadowed by a temporary.  */
typedef struct
{
  symbol_attribute attr;
  tree decl;
}
gfc_saved_var;


/* Advance the SS chain to the next term.  */
void gfc_advance_se_ss_chain (gfc_se *);

/* Call this to initialize a gfc_se structure before use
   first parameter is structure to initialize, second is
   parent to get scalarization data from, or NULL.  */
void gfc_init_se (gfc_se *, gfc_se *);

/* Create an artificial variable decl and add it to the current scope.  */
tree gfc_create_var (tree, const char *);
/* Like above but doesn't add it to the current scope.  */
tree gfc_create_var_np (tree, const char *);

/* Store the result of an expression in a temp variable so it can be used
   repeatedly even if the original changes */
void gfc_make_safe_expr (gfc_se * se);

/* Makes sure se is suitable for passing as a function string parameter.  */
void gfc_conv_string_parameter (gfc_se * se);

/* Compare two strings.  */
tree gfc_build_compare_string (tree, tree, tree, tree, int);

/* Add an item to the end of TREE_LIST.  */
tree gfc_chainon_list (tree, tree);

/* When using the gfc_conv_* make sure you understand what they do, i.e.
   when a POST chain may be created, and what the returned expression may be
   used for.  Note that character strings have special handling.  This
   should not be a problem as most statements/operations only deal with
   numeric/logical types.  See the implementations in trans-expr.c
   for details of the individual functions.  */

void gfc_conv_expr (gfc_se * se, gfc_expr * expr);
void gfc_conv_expr_val (gfc_se * se, gfc_expr * expr);
void gfc_conv_expr_lhs (gfc_se * se, gfc_expr * expr);
void gfc_conv_expr_reference (gfc_se * se, gfc_expr *);
void gfc_conv_expr_type (gfc_se * se, gfc_expr *, tree);

/* trans-expr.c */
void gfc_conv_scalar_char_value (gfc_symbol *sym, gfc_se *se, gfc_expr **expr);

/* Find the decl containing the auxiliary variables for assigned variables.  */
void gfc_conv_label_variable (gfc_se * se, gfc_expr * expr);
/* If the value is not constant, Create a temporary and copy the value.  */
tree gfc_evaluate_now (tree, stmtblock_t *);

/* Intrinsic function handling.  */
void gfc_conv_intrinsic_function (gfc_se *, gfc_expr *);

/* Does an intrinsic map directly to an external library call.  */
int gfc_is_intrinsic_libcall (gfc_expr *);

/* Used to call ordinary functions/subroutines
   and procedure pointer components.  */
int gfc_conv_procedure_call (gfc_se *, gfc_symbol *, gfc_actual_arglist *,
			    gfc_expr *, tree);

void gfc_conv_subref_array_arg (gfc_se *, gfc_expr *, int, sym_intent, bool);

/* gfc_trans_* shouldn't call push/poplevel, use gfc_push/pop_scope */

/* Generate code for a scalar assignment.  */
tree gfc_trans_scalar_assign (gfc_se *, gfc_se *, gfc_typespec, bool, bool);

/* Translate COMMON blocks.  */
void gfc_trans_common (gfc_namespace *);

/* Translate a derived type constructor.  */
void gfc_conv_structure (gfc_se *, gfc_expr *, int);

/* Return an expression which determines if a dummy parameter is present.  */
tree gfc_conv_expr_present (gfc_symbol *);
/* Convert a missing, dummy argument into a null or zero.  */
void gfc_conv_missing_dummy (gfc_se *, gfc_expr *, gfc_typespec, int);

/* Generate code to allocate a string temporary.  */
tree gfc_conv_string_tmp (gfc_se *, tree, tree);
/* Get the string length variable belonging to an expression.  */
tree gfc_get_expr_charlen (gfc_expr *);
/* Initialize a string length variable.  */
void gfc_conv_string_length (gfc_charlen *, gfc_expr *, stmtblock_t *);
/* Ensure type sizes can be gimplified.  */
void gfc_trans_vla_type_sizes (gfc_symbol *, stmtblock_t *);

/* Add an expression to the end of a block.  */
void gfc_add_expr_to_block (stmtblock_t *, tree);
/* Add a block to the end of a block.  */
void gfc_add_block_to_block (stmtblock_t *, stmtblock_t *);
/* Add a MODIFY_EXPR to a block.  */
void gfc_add_modify (stmtblock_t *, tree, tree);

/* Initialize a statement block.  */
void gfc_init_block (stmtblock_t *);
/* Start a new statement block.  Like gfc_init_block but also starts a new
   variable scope.  */
void gfc_start_block (stmtblock_t *);
/* Finish a statement block.  Also closes the scope if the block was created
   with gfc_start_block.  */
tree gfc_finish_block (stmtblock_t *);
/* Merge the scope of a block with its parent.  */
void gfc_merge_block_scope (stmtblock_t * block);

/* Return the backend label decl.  */
tree gfc_get_label_decl (gfc_st_label *);

/* Return the decl for an external function.  */
tree gfc_get_extern_function_decl (gfc_symbol *);

/* Return the decl for a function.  */
tree gfc_get_function_decl (gfc_symbol *);

/* Build an ADDR_EXPR.  */
tree gfc_build_addr_expr (tree, tree);

/* Build an ARRAY_REF.  */
tree gfc_build_array_ref (tree, tree, tree);

/* Creates a label.  Decl is artificial if label_id == NULL_TREE.  */
tree gfc_build_label_decl (tree);

/* Return the decl used to hold the function return value.
   Do not use if the function has an explicit result variable.  */
tree gfc_get_fake_result_decl (gfc_symbol *, int);

/* Get the return label for the current function.  */
tree gfc_get_return_label (void);

/* Add a decl to the binding level for the current function.  */
void gfc_add_decl_to_function (tree);

/* Make prototypes for runtime library functions.  */
void gfc_build_builtin_function_decls (void);

/* Set the backend source location of a decl.  */
void gfc_set_decl_location (tree, locus *);

/* Return the variable decl for a symbol.  */
tree gfc_get_symbol_decl (gfc_symbol *);

/* Build a static initializer.  */
tree gfc_conv_initializer (gfc_expr *, gfc_typespec *, tree, bool, bool);

/* Assign a default initializer to a derived type.  */
tree gfc_init_default_dt (gfc_symbol *, tree);

/* Substitute a temporary variable in place of the real one.  */
void gfc_shadow_sym (gfc_symbol *, tree, gfc_saved_var *);

/* Restore the original variable.  */
void gfc_restore_sym (gfc_symbol *, gfc_saved_var *);

/* Setting a decl assembler name, mangling it according to target rules
   (like Windows @NN decorations).  */
void gfc_set_decl_assembler_name (tree, tree);

/* Returns true if a variable of specified size should go on the stack.  */
int gfc_can_put_var_on_stack (tree);

/* Allocate the lang-specific part of a decl node.  */
void gfc_allocate_lang_decl (tree);

/* Advance along a TREE_CHAIN.  */
tree gfc_advance_chain (tree, int);

/* Create a decl for a function.  */
void gfc_create_function_decl (gfc_namespace *);
/* Generate the code for a function.  */
void gfc_generate_function_code (gfc_namespace *);
/* Output a BLOCK DATA program unit.  */
void gfc_generate_block_data (gfc_namespace *);
/* Output a decl for a module variable.  */
void gfc_generate_module_vars (gfc_namespace *);

struct GTY(()) module_htab_entry {
  const char *name;
  tree namespace_decl;
  htab_t GTY ((param_is (union tree_node))) decls;
};

struct module_htab_entry *gfc_find_module (const char *);
void gfc_module_add_decl (struct module_htab_entry *, tree);

/* Get and set the current location.  */
void gfc_set_backend_locus (locus *);
void gfc_get_backend_locus (locus *);

/* Handle static constructor functions.  */
extern GTY(()) tree gfc_static_ctors;
void gfc_generate_constructors (void);

/* Get the string length of an array constructor.  */
bool get_array_ctor_strlen (stmtblock_t *, gfc_constructor *, tree *);

/* Generate a runtime error call.  */
tree gfc_trans_runtime_error (bool, locus*, const char*, ...);
tree gfc_trans_runtime_error_vararg (bool, locus*, const char*, va_list);

/* Generate a runtime warning/error check.  */
void gfc_trans_runtime_check (bool, bool, tree, stmtblock_t *, locus *,
			      const char *, ...);

/* Generate a runtime check for same string length.  */
void gfc_trans_same_strlen_check (const char*, locus*, tree, tree,
				  stmtblock_t*);

/* Generate a call to free() after checking that its arg is non-NULL.  */
tree gfc_call_free (tree);

/* Allocate memory after performing a few checks.  */
tree gfc_call_malloc (stmtblock_t *, tree, tree);

/* Build a memcpy call.  */
tree gfc_build_memcpy_call (tree, tree, tree);

/* Allocate memory for arrays, with optional status variable.  */
tree gfc_allocate_array_with_status (stmtblock_t*, tree, tree, tree, gfc_expr*);

/* Allocate memory, with optional status variable.  */
tree gfc_allocate_with_status (stmtblock_t *, tree, tree);

/* Generate code to deallocate an array.  */
tree gfc_deallocate_with_status (tree, tree, bool, gfc_expr*);

/* Generate code to call realloc().  */
tree gfc_call_realloc (stmtblock_t *, tree, tree);

/* Generate code for an assignment, includes scalarization.  */
tree gfc_trans_assignment (gfc_expr *, gfc_expr *, bool);

/* Generate code for a pointer assignment.  */
tree gfc_trans_pointer_assignment (gfc_expr *, gfc_expr *);

/* Initialize function decls for library functions.  */
void gfc_build_intrinsic_lib_fndecls (void);
/* Create function decls for IO library functions.  */
void gfc_trans_io_runtime_check (tree, tree, int, const char *, stmtblock_t *);
void gfc_build_io_library_fndecls (void);
/* Build a function decl for a library function.  */
tree gfc_build_library_function_decl (tree, tree, int, ...);

/* Process the local variable decls of a block construct.  */
void gfc_process_block_locals (gfc_namespace*);

/* Output initialization/clean-up code that was deferred.  */
tree gfc_trans_deferred_vars (gfc_symbol*, tree);

/* somewhere! */
tree pushdecl (tree);
tree pushdecl_top_level (tree);
void pushlevel (int);
tree poplevel (int, int, int);
tree getdecls (void);
tree gfc_truthvalue_conversion (tree);
tree gfc_builtin_function (tree);
struct array_descr_info;
bool gfc_get_array_descr_info (const_tree, struct array_descr_info *);

/* In trans-openmp.c */
bool gfc_omp_privatize_by_reference (const_tree);
enum omp_clause_default_kind gfc_omp_predetermined_sharing (tree);
tree gfc_omp_clause_default_ctor (tree, tree, tree);
tree gfc_omp_clause_copy_ctor (tree, tree, tree);
tree gfc_omp_clause_assign_op (tree, tree, tree);
tree gfc_omp_clause_dtor (tree, tree);
bool gfc_omp_disregard_value_expr (tree, bool);
bool gfc_omp_private_debug_clause (tree, bool);
bool gfc_omp_private_outer_ref (tree);
struct gimplify_omp_ctx;
void gfc_omp_firstprivatize_type_sizes (struct gimplify_omp_ctx *, tree);

/* Runtime library function decls.  */
extern GTY(()) tree gfor_fndecl_pause_numeric;
extern GTY(()) tree gfor_fndecl_pause_string;
extern GTY(()) tree gfor_fndecl_stop_numeric;
extern GTY(()) tree gfor_fndecl_stop_string;
extern GTY(()) tree gfor_fndecl_runtime_error;
extern GTY(()) tree gfor_fndecl_runtime_error_at;
extern GTY(()) tree gfor_fndecl_runtime_warning_at;
extern GTY(()) tree gfor_fndecl_os_error;
extern GTY(()) tree gfor_fndecl_generate_error;
extern GTY(()) tree gfor_fndecl_set_fpe;
extern GTY(()) tree gfor_fndecl_set_options;
extern GTY(()) tree gfor_fndecl_ttynam;
extern GTY(()) tree gfor_fndecl_ctime;
extern GTY(()) tree gfor_fndecl_fdate;
extern GTY(()) tree gfor_fndecl_in_pack;
extern GTY(()) tree gfor_fndecl_in_unpack;
extern GTY(()) tree gfor_fndecl_associated;

/* Math functions.  Many other math functions are handled in
   trans-intrinsic.c.  */

typedef struct GTY(()) gfc_powdecl_list {
  tree integer;
  tree real;
  tree cmplx;
}
gfc_powdecl_list;

extern GTY(()) gfc_powdecl_list gfor_fndecl_math_powi[4][3];
extern GTY(()) tree gfor_fndecl_math_ishftc4;
extern GTY(()) tree gfor_fndecl_math_ishftc8;
extern GTY(()) tree gfor_fndecl_math_ishftc16;

/* BLAS functions.  */
extern GTY(()) tree gfor_fndecl_sgemm;
extern GTY(()) tree gfor_fndecl_dgemm;
extern GTY(()) tree gfor_fndecl_cgemm;
extern GTY(()) tree gfor_fndecl_zgemm;

/* String functions.  */
extern GTY(()) tree gfor_fndecl_compare_string;
extern GTY(()) tree gfor_fndecl_concat_string;
extern GTY(()) tree gfor_fndecl_string_len_trim;
extern GTY(()) tree gfor_fndecl_string_index;
extern GTY(()) tree gfor_fndecl_string_scan;
extern GTY(()) tree gfor_fndecl_string_verify;
extern GTY(()) tree gfor_fndecl_string_trim;
extern GTY(()) tree gfor_fndecl_string_minmax;
extern GTY(()) tree gfor_fndecl_adjustl;
extern GTY(()) tree gfor_fndecl_adjustr;
extern GTY(()) tree gfor_fndecl_select_string;
extern GTY(()) tree gfor_fndecl_compare_string_char4;
extern GTY(()) tree gfor_fndecl_concat_string_char4;
extern GTY(()) tree gfor_fndecl_string_len_trim_char4;
extern GTY(()) tree gfor_fndecl_string_index_char4;
extern GTY(()) tree gfor_fndecl_string_scan_char4;
extern GTY(()) tree gfor_fndecl_string_verify_char4;
extern GTY(()) tree gfor_fndecl_string_trim_char4;
extern GTY(()) tree gfor_fndecl_string_minmax_char4;
extern GTY(()) tree gfor_fndecl_adjustl_char4;
extern GTY(()) tree gfor_fndecl_adjustr_char4;
extern GTY(()) tree gfor_fndecl_select_string_char4;

/* Conversion between character kinds.  */
extern GTY(()) tree gfor_fndecl_convert_char1_to_char4;
extern GTY(()) tree gfor_fndecl_convert_char4_to_char1;

/* Other misc. runtime library functions.  */
extern GTY(()) tree gfor_fndecl_size0;
extern GTY(()) tree gfor_fndecl_size1;
extern GTY(()) tree gfor_fndecl_iargc;
extern GTY(()) tree gfor_fndecl_clz128;
extern GTY(()) tree gfor_fndecl_ctz128;

/* Implemented in Fortran.  */
extern GTY(()) tree gfor_fndecl_sc_kind;
extern GTY(()) tree gfor_fndecl_si_kind;
extern GTY(()) tree gfor_fndecl_sr_kind;


/* True if node is an integer constant.  */
#define INTEGER_CST_P(node) (TREE_CODE(node) == INTEGER_CST)

/* G95-specific declaration information.  */

enum gfc_array_kind
{
  GFC_ARRAY_UNKNOWN,
  GFC_ARRAY_ASSUMED_SHAPE,
  GFC_ARRAY_ALLOCATABLE,
  GFC_ARRAY_POINTER
};

/* Array types only.  */
struct GTY(())	lang_type	 {
  int rank;
  enum gfc_array_kind akind;
  tree lbound[GFC_MAX_DIMENSIONS];
  tree ubound[GFC_MAX_DIMENSIONS];
  tree stride[GFC_MAX_DIMENSIONS];
  tree size;
  tree offset;
  tree dtype;
  tree dataptr_type;
  tree span;
  tree base_decl[2];
};

struct GTY(()) lang_decl {
  /* Dummy variables.  */
  tree saved_descriptor;
  /* Assigned integer nodes.  Stringlength is the IO format string's length.
     Addr is the address of the string or the target label. Stringlength is
     initialized to -2 and assigned to -1 when addr is assigned to the
     address of target label.  */
  tree stringlen;
  tree addr;
  tree span;
};


#define GFC_DECL_ASSIGN_ADDR(node) DECL_LANG_SPECIFIC(node)->addr
#define GFC_DECL_STRING_LEN(node) DECL_LANG_SPECIFIC(node)->stringlen
#define GFC_DECL_SPAN(node) DECL_LANG_SPECIFIC(node)->span
#define GFC_DECL_SAVED_DESCRIPTOR(node) \
  (DECL_LANG_SPECIFIC(node)->saved_descriptor)
#define GFC_DECL_PACKED_ARRAY(node) DECL_LANG_FLAG_0(node)
#define GFC_DECL_PARTIAL_PACKED_ARRAY(node) DECL_LANG_FLAG_1(node)
#define GFC_DECL_ASSIGN(node) DECL_LANG_FLAG_2(node)
#define GFC_DECL_COMMON_OR_EQUIV(node) DECL_LANG_FLAG_3(node)
#define GFC_DECL_CRAY_POINTEE(node) DECL_LANG_FLAG_4(node)
#define GFC_DECL_RESULT(node) DECL_LANG_FLAG_5(node)
#define GFC_DECL_SUBREF_ARRAY_P(node) DECL_LANG_FLAG_6(node)

/* An array descriptor.  */
#define GFC_DESCRIPTOR_TYPE_P(node) TYPE_LANG_FLAG_1(node)
/* An array without a descriptor.  */
#define GFC_ARRAY_TYPE_P(node) TYPE_LANG_FLAG_2(node)
/* Fortran POINTER type.  */
#define GFC_POINTER_TYPE_P(node) TYPE_LANG_FLAG_3(node)
/* The GFC_TYPE_ARRAY_* members are present in both descriptor and
   descriptorless array types.  */
#define GFC_TYPE_ARRAY_LBOUND(node, dim) \
  (TYPE_LANG_SPECIFIC(node)->lbound[dim])
#define GFC_TYPE_ARRAY_UBOUND(node, dim) \
  (TYPE_LANG_SPECIFIC(node)->ubound[dim])
#define GFC_TYPE_ARRAY_STRIDE(node, dim) \
  (TYPE_LANG_SPECIFIC(node)->stride[dim])
#define GFC_TYPE_ARRAY_RANK(node) (TYPE_LANG_SPECIFIC(node)->rank)
#define GFC_TYPE_ARRAY_SIZE(node) (TYPE_LANG_SPECIFIC(node)->size)
#define GFC_TYPE_ARRAY_OFFSET(node) (TYPE_LANG_SPECIFIC(node)->offset)
#define GFC_TYPE_ARRAY_AKIND(node) (TYPE_LANG_SPECIFIC(node)->akind)
/* Code should use gfc_get_dtype instead of accessing this directly.  It may
   not be known when the type is created.  */
#define GFC_TYPE_ARRAY_DTYPE(node) (TYPE_LANG_SPECIFIC(node)->dtype)
#define GFC_TYPE_ARRAY_DATAPTR_TYPE(node) \
  (TYPE_LANG_SPECIFIC(node)->dataptr_type)
#define GFC_TYPE_ARRAY_SPAN(node) (TYPE_LANG_SPECIFIC(node)->span)
#define GFC_TYPE_ARRAY_BASE_DECL(node, internal) \
  (TYPE_LANG_SPECIFIC(node)->base_decl[(internal)])

/* Build an expression with void type.  */
#define build1_v(code, arg) fold_build1(code, void_type_node, arg)
#define build2_v(code, arg1, arg2) fold_build2(code, void_type_node, \
                                               arg1, arg2)
#define build3_v(code, arg1, arg2, arg3) fold_build3(code, void_type_node, \
                                                     arg1, arg2, arg3)
#define build4_v(code, arg1, arg2, arg3, arg4) build4(code, void_type_node, \
						      arg1, arg2, arg3, arg4)

/* This group of functions allows a caller to evaluate an expression from
   the callee's interface.  It establishes a mapping between the interface's
   dummy arguments and the caller's actual arguments, then applies that
   mapping to a given gfc_expr.

   You can initialize a mapping structure like so:

       gfc_interface_mapping mapping;
       ...
       gfc_init_interface_mapping (&mapping);

   You should then evaluate each actual argument into a temporary
   gfc_se structure, here called "se", and map the result to the
   dummy argument's symbol, here called "sym":

       gfc_add_interface_mapping (&mapping, sym, &se);

   After adding all mappings, you should call:

       gfc_finish_interface_mapping (&mapping, pre, post);

   where "pre" and "post" are statement blocks for initialization
   and finalization code respectively.  You can then evaluate an
   interface expression "expr" as follows:

       gfc_apply_interface_mapping (&mapping, se, expr);

   Once you've evaluated all expressions, you should free
   the mapping structure with:

       gfc_free_interface_mapping (&mapping); */


/* This structure represents a mapping from OLD to NEW, where OLD is a
   dummy argument symbol and NEW is a symbol that represents the value
   of an actual argument.  Mappings are linked together using NEXT
   (in no particular order).  */
typedef struct gfc_interface_sym_mapping
{
  struct gfc_interface_sym_mapping *next;
  gfc_symbol *old;
  gfc_symtree *new_sym;
  gfc_expr *expr;
}
gfc_interface_sym_mapping;


/* This structure is used by callers to evaluate an expression from
   a callee's interface.  */
typedef struct gfc_interface_mapping
{
  /* Maps the interface's dummy arguments to the values that the caller
     is passing.  The whole list is owned by this gfc_interface_mapping.  */
  gfc_interface_sym_mapping *syms;

  /* A list of gfc_charlens that were needed when creating copies of
     expressions.  The whole list is owned by this gfc_interface_mapping.  */
  gfc_charlen *charlens;
}
gfc_interface_mapping;

void gfc_init_interface_mapping (gfc_interface_mapping *);
void gfc_free_interface_mapping (gfc_interface_mapping *);
void gfc_add_interface_mapping (gfc_interface_mapping *,
				gfc_symbol *, gfc_se *, gfc_expr *);
void gfc_finish_interface_mapping (gfc_interface_mapping *,
				   stmtblock_t *, stmtblock_t *);
void gfc_apply_interface_mapping (gfc_interface_mapping *,
				  gfc_se *, gfc_expr *);


/* Standard error messages used in all the trans-*.c files.  */
extern const char gfc_msg_bounds[];
extern const char gfc_msg_fault[];
extern const char gfc_msg_wrong_return[];

#define OMPWS_WORKSHARE_FLAG	1	/* Set if in a workshare construct.  */
#define OMPWS_CURR_SINGLEUNIT	2	/* Set if current gfc_code in workshare
					   construct is not workshared.  */
#define OMPWS_SCALARIZER_WS	4	/* Set if scalarizer should attempt
					   to create parallel loops.  */
#define OMPWS_NOWAIT		8	/* Use NOWAIT on OMP_FOR.  */
extern int ompws_flags;

#endif /* GFC_TRANS_H */
