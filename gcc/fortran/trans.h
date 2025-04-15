/* Header for code translation functions
   Copyright (C) 2002-2025 Free Software Foundation, Inc.
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

#include "predict.h"  /* For enum br_predictor and PRED_*.  */

/* Mangled symbols take the form __module__name or __module.submodule__name.  */
#define GFC_MAX_MANGLED_SYMBOL_LEN  (GFC_MAX_SYMBOL_LEN*3+5)

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

  /* Carries finalization code that is required to be executed execution of the
     innermost executable construct.  */
  stmtblock_t finalblock;

  /* the result of the expression */
  tree expr;

  /* The length of a character string value.  */
  tree string_length;

  /* When expr is a reference to a class object, store its vptr access
     here.  */
  tree class_vptr;

  /* When expr is a reference to a direct subobject of a class, store
     the reference to the class object here.  */
  tree class_container;

  /* Whether expr is a reference to an unlimited polymorphic object.  */
  unsigned unlimited_polymorphic:1;

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

  /* If direct_byref is set, do work out the descriptor as in that case but
     do still create a new descriptor variable instead of using an
     existing one.  This is useful for special pointer assignments like
     rank remapping where we have to process the descriptor before
     assigning to final one.  */
  unsigned byref_noassign:1;

  /* Ignore absent optional arguments.  Used for some intrinsics.  */
  unsigned ignore_optional:1;

  /* When this is set the data and offset fields of the returned descriptor
     are NULL.  Used by intrinsic size.  */
  unsigned data_not_needed:1;

  /* If set, gfc_conv_procedure_call does not put byref calls into se->pre.  */
  unsigned no_function_call:1;

  /* If set, we will force the creation of a temporary. Useful to disable
     non-copying procedure argument passing optimizations, when some function
     args alias.  */
  unsigned force_tmp:1;

  /* If set, will pass subref descriptors without a temporary.  */
  unsigned force_no_tmp:1;

  /* Unconditionally calculate offset for array segments and constant
     arrays in gfc_conv_expr_descriptor.  */
  unsigned use_offset:1;

  unsigned want_coarray:1;

  /* Scalarization parameters.  */
  struct gfc_se *parent;
  struct gfc_ss *ss;
  struct gfc_loopinfo *loop;
}
gfc_se;

typedef struct gfc_co_subroutines_args
{
  tree image_index;
  tree stat;
  tree errmsg;
  tree errmsg_len;
}
gfc_co_subroutines_args;

/* Denotes different types of coarray.
   Please keep in sync with libgfortran/caf/libcaf.h.  */
enum gfc_coarray_regtype
{
  GFC_CAF_COARRAY_STATIC,
  GFC_CAF_COARRAY_ALLOC,
  GFC_CAF_LOCK_STATIC,
  GFC_CAF_LOCK_ALLOC,
  GFC_CAF_CRITICAL,
  GFC_CAF_EVENT_STATIC,
  GFC_CAF_EVENT_ALLOC,
  GFC_CAF_COARRAY_ALLOC_REGISTER_ONLY,
  GFC_CAF_COARRAY_ALLOC_ALLOCATE_ONLY
};


/* Describes the action to take on _caf_deregister.  Keep in sync with
   gcc/fortran/trans.h.  The negative values are not valid for the library and
   are used by the drivers for building the correct call.  */
enum gfc_coarray_deregtype {
  /* This is no coarray, i.e. build a call to a free ().  */
  GFC_CAF_COARRAY_NOCOARRAY = -2,
  /* The driver is to analyze which _caf_deregister ()-call to generate.  */
  GFC_CAF_COARRAY_ANALYZE = -1,
  GFC_CAF_COARRAY_DEREGISTER = 0,
  GFC_CAF_COARRAY_DEALLOCATE_ONLY
};


/* Specify the type of ref handed to the caf communication functions.
   Please keep in sync with libgfortran/caf/libcaf.h.  */
enum gfc_caf_ref_type_t {
  GFC_CAF_REF_COMPONENT,
  GFC_CAF_REF_ARRAY,
  GFC_CAF_REF_STATIC_ARRAY
};


/* Give the reference type of an array ref.
   Please keep in sync with libgfortran/caf/libcaf.h.  */
enum gfc_caf_array_ref_t {
  GFC_CAF_ARR_REF_NONE = 0,
  GFC_CAF_ARR_REF_VECTOR,
  GFC_CAF_ARR_REF_FULL,
  GFC_CAF_ARR_REF_RANGE,
  GFC_CAF_ARR_REF_SINGLE,
  GFC_CAF_ARR_REF_OPEN_END,
  GFC_CAF_ARR_REF_OPEN_START
};


/* trans-array (structure_alloc_comps) caf_mode bits.  */
enum gfc_structure_caf_mode_t {
  GFC_STRUCTURE_CAF_MODE_ENABLE_COARRAY = 1 << 0,
  GFC_STRUCTURE_CAF_MODE_IN_COARRAY = 1 << 1,
  GFC_STRUCTURE_CAF_MODE_DEALLOC_ONLY = 1 << 2
};


/* The array-specific scalarization information.  The array members of
   this struct are indexed by actual array index, and thus can be sparse.  */

typedef struct gfc_array_info
{
  mpz_t *shape;

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
}
gfc_array_info;

enum gfc_ss_type
{
  /* A scalar value.  This will be evaluated before entering the
     scalarization loop.  */
  GFC_SS_SCALAR,

  /* Like GFC_SS_SCALAR it evaluates the expression outside the
     loop.  Is always evaluated as a reference to the temporary, unless
     temporary evaluation can result in a NULL pointer dereferencing (case of
     optional arguments).  Used for elemental function arguments.  */
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
};


typedef struct gfc_ss_info
{
  int refcount;
  gfc_ss_type type;
  gfc_expr *expr;
  tree string_length;
  tree class_container;

  union
  {
    /* If type is GFC_SS_SCALAR or GFC_SS_REFERENCE.  */
    struct
    {
      /* If the scalar is passed as actual argument to an (elemental) procedure,
	 this is the corresponding dummy argument.  */
      gfc_dummy_arg *dummy_arg;
      tree value;
      /* Tells that the scalar is a reference to a variable that might
	 be present on the lhs, so that we should evaluate the value
	 itself before the loop, not just the reference.  */
      unsigned needs_temporary:1;
    }
    scalar;

    /* GFC_SS_TEMP.  */
    struct
    {
      tree type;
    }
    temp;

    /* All other types.  */
    gfc_array_info array;
  }
  data;

  /* This is used by assignments requiring temporaries.  The bits specify which
     loops the terms appear in.  This will be 1 for the RHS expressions,
     2 for the LHS expressions, and 3(=1|2) for the temporary.  */
  unsigned useflags:2;

  /* Suppresses precalculation of scalars in WHERE assignments.  */
  unsigned where:1;

  /* This set for an elemental function that contains expressions for
     external arrays, thereby triggering creation of a temporary.  */
  unsigned array_outer_dependency:1;

  /* Tells whether the SS is for an actual argument which can be a NULL
     reference.  In other words, the associated dummy argument is OPTIONAL.
     Used to handle elemental procedures.  */
  bool can_be_null_ref;
}
gfc_ss_info;

#define gfc_get_ss_info() XCNEW (gfc_ss_info)


/* Scalarization State chain.  Created by walking an expression tree before
   creating the scalarization loops.  Then passed as part of a gfc_se structure
   to translate the expression inside the loop.  Note that these chains are
   terminated by gfc_ss_terminator, not NULL.  A NULL pointer in a gfc_se
   indicates to gfc_conv_* that this is a scalar expression.
   SS structures can only belong to a single loopinfo.  They must be added
   otherwise they will not get freed.  */

typedef struct gfc_ss
{
  gfc_ss_info *info;

  int dimen;
  /* Translation from loop dimensions to actual array dimensions.
     actual_dim = dim[loop_dim]  */
  int dim[GFC_MAX_DIMENSIONS];

  /* All the SS in a loop and linked through loop_chain.  The SS for an
     expression are linked by the next pointer.  */
  struct gfc_ss *loop_chain;
  struct gfc_ss *next;

  /* Non-null if the ss is part of a nested loop.  */
  struct gfc_ss *parent;

  /* If the evaluation of an expression requires a nested loop (for example
     if the sum intrinsic is evaluated inline), this points to the nested
     loop's gfc_ss.  */
  struct gfc_ss *nested_ss;

  /* The loop this gfc_ss is in.  */
  struct gfc_loopinfo *loop;

  unsigned is_alloc_lhs:1;
  unsigned no_bounds_check:1;
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

  /* Non-null if this loop is nested in another one.  */
  struct gfc_loopinfo *parent;

  /* Chain of nested loops.  */
  struct gfc_loopinfo *nested, *next;

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

  /* Enum to control loop reversal.  */
  gfc_reverse reverse[GFC_MAX_DIMENSIONS];

  /* The number of dimensions for which a temporary is used.  */
  int temp_dim;

  /* If set we don't need the loop variables.  */
  unsigned array_parameter:1;
}
gfc_loopinfo;

#define gfc_get_loopinfo() XCNEW (gfc_loopinfo)

/* Information about a symbol that has been shadowed by a temporary.  */
typedef struct
{
  symbol_attribute attr;
  tree decl;
}
gfc_saved_var;


/* Store information about a block of code together with special
   initialization and clean-up code.  This can be used to incrementally add
   init and cleanup, and in the end put everything together to a
   try-finally expression.  */
typedef struct
{
  tree init;
  tree cleanup;
  tree code;
}
gfc_wrapped_block;

/* Class API functions.  */
tree gfc_class_set_static_fields (tree, tree, tree);
tree gfc_class_data_get (tree);
tree gfc_class_vptr_get (tree);
tree gfc_class_len_get (tree);
tree gfc_resize_class_size_with_len (stmtblock_t *, tree, tree);
gfc_expr * gfc_find_and_cut_at_last_class_ref (gfc_expr *, bool is_mold = false,
					       gfc_typespec **ts = NULL);
/* Get an accessor to the class' vtab's * field, when a class handle is
   available.  */
tree gfc_class_vtab_hash_get (tree);
tree gfc_class_vtab_size_get (tree);
tree gfc_class_vtab_def_init_get (tree);
tree gfc_class_vtab_copy_get (tree);
tree gfc_class_vtab_final_get (tree);
/* Get an accessor to the vtab's * field, when a vptr handle is present.  */
tree gfc_vptr_hash_get (tree);
tree gfc_vptr_size_get (tree);
tree gfc_vptr_def_init_get (tree);
tree gfc_vptr_copy_get (tree);
tree gfc_vptr_final_get (tree);
tree gfc_vptr_deallocate_get (tree);
void gfc_reset_vptr (stmtblock_t *, gfc_expr *, tree = NULL_TREE,
		     gfc_symbol * = nullptr);
void gfc_class_set_vptr (stmtblock_t *, tree, tree);
void gfc_reset_len (stmtblock_t *, gfc_expr *);
tree gfc_get_class_from_gfc_expr (gfc_expr *);
tree gfc_get_class_from_expr (tree);
tree gfc_get_vptr_from_expr (tree);
tree gfc_copy_class_to_class (tree, tree, tree, bool);
bool gfc_add_finalizer_call (stmtblock_t *, gfc_expr *, tree = NULL_TREE);
bool gfc_add_comp_finalizer_call (stmtblock_t *, tree, gfc_component *, bool);
void gfc_finalize_tree_expr (gfc_se *, gfc_symbol *, symbol_attribute, int);
bool gfc_assignment_finalizer_call (gfc_se *, gfc_expr *, bool);

void gfc_class_array_data_assign (stmtblock_t *, tree, tree, bool);
void gfc_conv_derived_to_class (gfc_se *, gfc_expr *, gfc_symbol *fsym, tree,
				bool, bool, const char *, tree * = nullptr);
void gfc_conv_class_to_class (gfc_se *, gfc_expr *, gfc_typespec, bool, bool,
			      bool, bool);

/* Initialize an init/cleanup block.  */
void gfc_start_wrapped_block (gfc_wrapped_block* block, tree code);
/* Add a pair of init/cleanup code to the block.  Each one might be a
   NULL_TREE if not required.  */
void gfc_add_init_cleanup (gfc_wrapped_block* block, tree init, tree cleanup,
			   bool back = false);
/* Finalize the block, that is, create a single expression encapsulating the
   original code together with init and clean-up code.  */
tree gfc_finish_wrapped_block (gfc_wrapped_block* block);


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
/* Ensure that tree can be used as an lvalue.  */
tree gfc_trans_force_lval (stmtblock_t *, tree);

/* Store the result of an expression in a temp variable so it can be used
   repeatedly even if the original changes */
void gfc_make_safe_expr (gfc_se * se);

/* Makes sure se is suitable for passing as a function string parameter.  */
void gfc_conv_string_parameter (gfc_se * se);

/* Compare two strings.  */
tree gfc_build_compare_string (tree, tree, tree, tree, int, enum tree_code);

/* When using the gfc_conv_* make sure you understand what they do, i.e.
   when a POST chain may be created, and what the returned expression may be
   used for.  Note that character strings have special handling.  This
   should not be a problem as most statements/operations only deal with
   numeric/logical types.  See the implementations in trans-expr.cc
   for details of the individual functions.  */

void gfc_conv_expr (gfc_se * se, gfc_expr * expr);
void gfc_conv_expr_val (gfc_se * se, gfc_expr * expr);
void gfc_conv_expr_lhs (gfc_se * se, gfc_expr * expr);
void gfc_conv_expr_reference (gfc_se * se, gfc_expr * expr);
void gfc_conv_expr_type (gfc_se * se, gfc_expr *, tree);


/* trans-expr.cc */
tree gfc_get_character_len_in_bytes (tree);
tree gfc_conv_scalar_to_descriptor (gfc_se *, tree, symbol_attribute);
tree gfc_get_ultimate_alloc_ptr_comps_caf_token (gfc_se *, gfc_expr *);
tree gfc_string_to_single_character (tree len, tree str, int kind);
tree gfc_get_tree_for_caf_expr (gfc_expr *);
void gfc_get_caf_token_offset (gfc_se*, tree *, tree *, tree, tree, gfc_expr *);
tree gfc_caf_get_image_index (stmtblock_t *, gfc_expr *, tree);
void gfc_simple_for_loop (stmtblock_t *, tree, tree, tree, enum tree_code, tree,
			  tree);

/* Find the decl containing the auxiliary variables for assigned variables.  */
void gfc_conv_label_variable (gfc_se * se, gfc_expr * expr);
/* If the value is not constant, Create a temporary and copy the value.  */
tree gfc_evaluate_now_loc (location_t, tree, stmtblock_t *);
tree gfc_evaluate_now (tree, stmtblock_t *);
tree gfc_evaluate_data_ref_now (tree, stmtblock_t *);
tree gfc_evaluate_now_function_scope (tree, stmtblock_t *);

/* Find the appropriate variant of a math intrinsic.  */
tree gfc_builtin_decl_for_float_kind (enum built_in_function, int);

tree size_of_string_in_bytes (int, tree);

/* Intrinsic procedure handling.  */
tree gfc_conv_intrinsic_subroutine (gfc_code *);
void gfc_conv_intrinsic_function (gfc_se *, gfc_expr *);
bool gfc_conv_ieee_arithmetic_function (gfc_se *, gfc_expr *);
tree gfc_save_fp_state (stmtblock_t *);
void gfc_restore_fp_state (stmtblock_t *, tree);

bool gfc_expr_is_variable (gfc_expr *);

/* Does an intrinsic map directly to an external library call
   This is true for array-returning intrinsics, unless
   gfc_inline_intrinsic_function_p returns true.  */
int gfc_is_intrinsic_libcall (gfc_expr *);

/* Used to call ordinary functions/subroutines
   and procedure pointer components.  */
int gfc_conv_procedure_call (gfc_se *, gfc_symbol *, gfc_actual_arglist *,
			     gfc_expr *, vec<tree, va_gc> *);

void gfc_conv_subref_array_arg (gfc_se *, gfc_expr *, int, sym_intent, bool,
				const gfc_symbol *fsym = NULL,
				const char *proc_name = NULL,
				gfc_symbol *sym = NULL,
				bool check_contiguous = false);

void gfc_conv_is_contiguous_expr (gfc_se *, gfc_expr *);

/* Generate code for a scalar assignment.  */
tree
gfc_trans_scalar_assign (gfc_se *, gfc_se *, gfc_typespec, bool, bool,
			 bool = false, bool = false);

/* Translate COMMON blocks.  */
void gfc_trans_common (gfc_namespace *);

/* Translate a derived type constructor.  */
void gfc_conv_structure (gfc_se *, gfc_expr *, int);

/* Return an expression which determines if a dummy parameter is present.  */
tree gfc_conv_expr_present (gfc_symbol *, bool use_saved_decl = false);
/* Convert a missing, dummy argument into a null or zero.  */
void gfc_conv_missing_dummy (gfc_se *, gfc_expr *, gfc_typespec, int);

/* Lowering of component references.  */
void gfc_conv_component_ref (gfc_se * se, gfc_ref * ref);
void conv_parent_component_references (gfc_se * se, gfc_ref * ref);

/* Automatically dereference var.  */
tree gfc_maybe_dereference_var (gfc_symbol *, tree, bool desc_only = false,
				bool is_classarray = false);

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
/* Add an expression to the beginning of a block.  */
void gfc_prepend_expr_to_block (stmtblock_t *, tree);
/* Add a block to the end of a block.  */
void gfc_add_block_to_block (stmtblock_t *, stmtblock_t *);
/* Add a MODIFY_EXPR to a block.  */
void gfc_add_modify_loc (location_t, stmtblock_t *, tree, tree);
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
tree gfc_get_extern_function_decl (gfc_symbol *,
				   gfc_actual_arglist *args = NULL,
				   const char *fnspec = NULL);

/* Build an ADDR_EXPR.  */
tree gfc_build_addr_expr (tree, tree);

/* Build an ARRAY_REF.  */
tree gfc_build_array_ref (tree, tree, tree,
			  bool non_negative_offset = false,
			  tree vptr = NULL_TREE);

/* Build an array ref using pointer arithmetic.  */
tree gfc_build_spanned_array_ref (tree base, tree offset, tree span);

/* Creates a label.  Decl is artificial if label_id == NULL_TREE.  */
tree gfc_build_label_decl (tree);

/* Return the decl used to hold the function return value.
   Do not use if the function has an explicit result variable.  */
tree gfc_get_fake_result_decl (gfc_symbol *, int);

/* Add a decl to the binding level for the current function.  */
void gfc_add_decl_to_function (tree);

/* Make prototypes for runtime library functions.  */
void gfc_build_builtin_function_decls (void);

/* Set the backend source location of a decl.  */
void gfc_set_decl_location (tree, locus *);

/* Get a module symbol backend_decl if possible.  */
bool gfc_get_module_backend_decl (gfc_symbol *);

/* Return the variable decl for a symbol.  */
tree gfc_get_symbol_decl (gfc_symbol *);

/* Build a static initializer.  */
tree gfc_conv_initializer (gfc_expr *, gfc_typespec *, tree, bool, bool, bool);

/* Assign a default initializer to a derived type.  */
void gfc_init_default_dt (gfc_symbol *, stmtblock_t *, bool);

/* Substitute a temporary variable in place of the real one.  */
void gfc_shadow_sym (gfc_symbol *, tree, gfc_saved_var *);

/* Restore the original variable.  */
void gfc_restore_sym (gfc_symbol *, gfc_saved_var *);

/* Setting a decl assembler name, mangling it according to target rules
   (like Windows @NN decorations).  */
void gfc_set_decl_assembler_name (tree, tree);

/* Returns true if a variable of specified size should go on the stack.  */
bool gfc_can_put_var_on_stack (tree);

/* Set GFC_DECL_SCALAR_* on decl from sym if needed.  */
void gfc_finish_decl_attrs (tree, symbol_attribute *);

/* Allocate the lang-specific part of a decl node.  */
void gfc_allocate_lang_decl (tree);

/* Advance along a TREE_CHAIN.  */
tree gfc_advance_chain (tree, int);

/* Create a decl for a function.  */
void gfc_create_function_decl (gfc_namespace *, bool);
/* Generate the code for a function.  */
void gfc_generate_function_code (gfc_namespace *);
/* Output a BLOCK DATA program unit.  */
void gfc_generate_block_data (gfc_namespace *);
/* Output a decl for a module variable.  */
void gfc_generate_module_vars (gfc_namespace *);
/* Get the appropriate return statement for a procedure.  */
tree gfc_generate_return (void);

struct module_decl_hasher : ggc_ptr_hash<tree_node>
{
  typedef const char *compare_type;

  static hashval_t hash (tree);
  static bool equal (tree, const char *);
};

struct GTY((for_user)) module_htab_entry {
  const char *name;
  tree namespace_decl;
  hash_table<module_decl_hasher> *GTY (()) decls;
};

struct module_htab_entry *gfc_find_module (const char *);
void gfc_module_add_decl (struct module_htab_entry *, tree);

void gfc_locus_from_location (locus *, location_t);

/* Handle static constructor functions.  */
extern GTY(()) tree gfc_static_ctors;
void gfc_generate_constructors (void);

/* Get the string length of an array constructor.  */
bool get_array_ctor_strlen (stmtblock_t *, gfc_constructor_base, tree *);

/* Mark a condition as likely or unlikely.  */
tree gfc_likely (tree, enum br_predictor);
tree gfc_unlikely (tree, enum br_predictor);

/* Return the string length of a deferred character length component.  */
bool gfc_deferred_strlen (gfc_component *, tree *);

/* Generate a runtime error call.  */
tree gfc_trans_runtime_error (bool, locus*, const char*, ...);

/* Generate a runtime warning/error check.  */
void gfc_trans_runtime_check (bool, bool, tree, stmtblock_t *, locus *,
			      const char *, ...);

/* Generate a runtime check for same string length.  */
void gfc_trans_same_strlen_check (const char*, locus*, tree, tree,
				  stmtblock_t*);

/* Generate a call to free().  */
tree gfc_call_free (tree);

/* Allocate memory after performing a few checks.  */
tree gfc_call_malloc (stmtblock_t *, tree, tree);

/* Build a memcpy call.  */
tree gfc_build_memcpy_call (tree, tree, tree);

/* Register memory with the coarray library.  */
void gfc_allocate_using_caf_lib (stmtblock_t *, tree, tree, tree, tree, tree,
				 tree, gfc_coarray_regtype);

/* Allocate memory for allocatable variables, with optional status variable.  */
void gfc_allocate_allocatable (stmtblock_t*, tree, tree, tree, tree,
			       tree, tree, tree, gfc_expr*, int,
			       tree = NULL_TREE, tree = NULL_TREE,
			       tree = NULL_TREE);

/* Allocate memory, with optional status variable.  */
void gfc_allocate_using_malloc (stmtblock_t *, tree, tree, tree,
				tree = NULL_TREE, tree = NULL_TREE,
				tree = NULL_TREE);

/* Generate code to deallocate an array.  */
tree gfc_deallocate_with_status (tree, tree, tree, tree, tree, bool,
				 gfc_expr *, int, tree = NULL_TREE,
				 tree a = NULL_TREE, tree c = NULL_TREE);
tree gfc_deallocate_scalar_with_status (tree, tree, tree, bool, gfc_expr*,
					gfc_typespec, tree = NULL_TREE,
					bool c = false);

/* Generate code to call realloc().  */
tree gfc_call_realloc (stmtblock_t *, tree, tree);

/* Assign a derived type constructor to a variable.  */
tree gfc_trans_structure_assign (tree, gfc_expr *, bool, bool c = false);

/* Generate code for an assignment, includes scalarization.  */
tree gfc_trans_assignment (gfc_expr *, gfc_expr *, bool, bool, bool p = false,
			   bool a = true);

/* Generate code for a pointer assignment.  */
tree gfc_trans_pointer_assignment (gfc_expr *, gfc_expr *);

/* Initialize function decls for library functions.  */
void gfc_build_intrinsic_lib_fndecls (void);
/* Create function decls for IO library functions.  */
void gfc_build_io_library_fndecls (void);
/* Build a function decl for a library function.  */
tree gfc_build_library_function_decl (tree, tree, int, ...);
tree gfc_build_library_function_decl_with_spec (tree name, const char *spec,
						tree rettype, int nargs, ...);

/* Process the local variable decls of a block construct.  */
void gfc_start_saved_local_decls ();
void gfc_stop_saved_local_decls ();
void gfc_process_block_locals (gfc_namespace*);

/* Output initialization/clean-up code that was deferred.  */
void gfc_trans_deferred_vars (gfc_symbol*, gfc_wrapped_block *);

/* In f95-lang.cc.  */
tree pushdecl (tree);
tree pushdecl_top_level (tree);
void pushlevel (void);
tree poplevel (int, int);
tree getdecls (void);

/* In trans-types.cc.  */
struct array_descr_info;
bool gfc_get_array_descr_info (const_tree, struct array_descr_info *);

/* In trans-openmp.cc */
tree gfc_omp_call_add_alloc (tree);
tree gfc_omp_call_is_alloc (tree);
bool gfc_omp_is_allocatable_or_ptr (const_tree);
tree gfc_omp_check_optional_argument (tree, bool);
tree gfc_omp_array_data (tree, bool);
tree gfc_omp_array_size (tree, gimple_seq *);
bool gfc_omp_privatize_by_reference (const_tree);
enum omp_clause_default_kind gfc_omp_predetermined_sharing (tree);
enum omp_clause_defaultmap_kind gfc_omp_predetermined_mapping (tree);
tree gfc_omp_report_decl (tree);
tree gfc_omp_clause_default_ctor (tree, tree, tree);
tree gfc_omp_clause_copy_ctor (tree, tree, tree);
tree gfc_omp_clause_assign_op (tree, tree, tree);
tree gfc_omp_clause_linear_ctor (tree, tree, tree, tree);
tree gfc_omp_clause_dtor (tree, tree);
void gfc_omp_finish_clause (tree, gimple_seq *, bool);
bool gfc_omp_deep_mapping_p (const gimple *, tree);
tree gfc_omp_deep_mapping_cnt (const gimple *, tree, gimple_seq *);
void gfc_omp_deep_mapping (const gimple *, tree, unsigned HOST_WIDE_INT, tree,
			   tree, tree, tree, tree, gimple_seq *);
bool gfc_omp_allocatable_p (tree);
bool gfc_omp_scalar_p (tree, bool);
bool gfc_omp_scalar_target_p (tree);
bool gfc_omp_disregard_value_expr (tree, bool);
bool gfc_omp_private_debug_clause (tree, bool);
bool gfc_omp_private_outer_ref (tree);
struct gimplify_omp_ctx;
void gfc_omp_firstprivatize_type_sizes (struct gimplify_omp_ctx *, tree);

/* In trans-intrinsic.cc.  */
void gfc_conv_intrinsic_mvbits (gfc_se *, gfc_actual_arglist *,
				gfc_loopinfo *);

/* Runtime library function decls.  */
extern GTY(()) tree gfor_fndecl_pause_numeric;
extern GTY(()) tree gfor_fndecl_pause_string;
extern GTY(()) tree gfor_fndecl_stop_numeric;
extern GTY(()) tree gfor_fndecl_stop_string;
extern GTY(()) tree gfor_fndecl_error_stop_numeric;
extern GTY(()) tree gfor_fndecl_error_stop_string;
extern GTY(()) tree gfor_fndecl_runtime_error;
extern GTY(()) tree gfor_fndecl_runtime_error_at;
extern GTY(()) tree gfor_fndecl_runtime_warning_at;
extern GTY(()) tree gfor_fndecl_os_error_at;
extern GTY(()) tree gfor_fndecl_generate_error;
extern GTY(()) tree gfor_fndecl_set_fpe;
extern GTY(()) tree gfor_fndecl_set_options;
extern GTY(()) tree gfor_fndecl_ttynam;
extern GTY(()) tree gfor_fndecl_ctime;
extern GTY(()) tree gfor_fndecl_fdate;
extern GTY(()) tree gfor_fndecl_in_pack;
extern GTY(()) tree gfor_fndecl_in_unpack;
extern GTY(()) tree gfor_fndecl_in_pack_class;
extern GTY(()) tree gfor_fndecl_in_unpack_class;
extern GTY(()) tree gfor_fndecl_associated;
extern GTY(()) tree gfor_fndecl_system_clock4;
extern GTY(()) tree gfor_fndecl_system_clock8;


/* Coarray run-time library function decls.  */
extern GTY(()) tree gfor_fndecl_caf_init;
extern GTY(()) tree gfor_fndecl_caf_finalize;
extern GTY(()) tree gfor_fndecl_caf_this_image;
extern GTY(()) tree gfor_fndecl_caf_num_images;
extern GTY(()) tree gfor_fndecl_caf_register;
extern GTY(()) tree gfor_fndecl_caf_deregister;
extern GTY(()) tree gfor_fndecl_caf_register_accessor;
extern GTY(()) tree gfor_fndecl_caf_register_accessors_finish;
extern GTY(()) tree gfor_fndecl_caf_get_remote_function_index;
extern GTY(()) tree gfor_fndecl_caf_get_from_remote;
extern GTY(()) tree gfor_fndecl_caf_send_to_remote;
extern GTY(()) tree gfor_fndecl_caf_transfer_between_remotes;
extern GTY(()) tree gfor_fndecl_caf_sync_all;
extern GTY(()) tree gfor_fndecl_caf_sync_memory;
extern GTY(()) tree gfor_fndecl_caf_sync_images;
extern GTY(()) tree gfor_fndecl_caf_stop_numeric;
extern GTY(()) tree gfor_fndecl_caf_stop_str;
extern GTY(()) tree gfor_fndecl_caf_error_stop;
extern GTY(()) tree gfor_fndecl_caf_error_stop_str;
extern GTY(()) tree gfor_fndecl_caf_atomic_def;
extern GTY(()) tree gfor_fndecl_caf_atomic_ref;
extern GTY(()) tree gfor_fndecl_caf_atomic_cas;
extern GTY(()) tree gfor_fndecl_caf_atomic_op;
extern GTY(()) tree gfor_fndecl_caf_lock;
extern GTY(()) tree gfor_fndecl_caf_unlock;
extern GTY(()) tree gfor_fndecl_caf_event_post;
extern GTY(()) tree gfor_fndecl_caf_event_wait;
extern GTY(()) tree gfor_fndecl_caf_event_query;
extern GTY(()) tree gfor_fndecl_caf_fail_image;
extern GTY(()) tree gfor_fndecl_caf_failed_images;
extern GTY(()) tree gfor_fndecl_caf_image_status;
extern GTY(()) tree gfor_fndecl_caf_stopped_images;
extern GTY(()) tree gfor_fndecl_caf_form_team;
extern GTY(()) tree gfor_fndecl_caf_change_team;
extern GTY(()) tree gfor_fndecl_caf_end_team;
extern GTY(()) tree gfor_fndecl_caf_get_team;
extern GTY(()) tree gfor_fndecl_caf_sync_team;
extern GTY(()) tree gfor_fndecl_caf_team_number;
extern GTY(()) tree gfor_fndecl_co_broadcast;
extern GTY(()) tree gfor_fndecl_co_max;
extern GTY(()) tree gfor_fndecl_co_min;
extern GTY(()) tree gfor_fndecl_co_reduce;
extern GTY(()) tree gfor_fndecl_co_sum;
extern GTY(()) tree gfor_fndecl_caf_is_present_on_remote;

/* Math functions.  Many other math functions are handled in
   trans-intrinsic.cc.  */

typedef struct GTY(()) gfc_powdecl_list {
  tree integer;
  tree real;
  tree cmplx;
}
gfc_powdecl_list;

extern GTY(()) gfc_powdecl_list gfor_fndecl_math_powi[4][3];
extern GTY(()) tree gfor_fndecl_unsigned_pow_list[5][5];

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
extern GTY(()) tree gfor_fndecl_iargc;
extern GTY(()) tree gfor_fndecl_kill;
extern GTY(()) tree gfor_fndecl_kill_sub;
extern GTY(()) tree gfor_fndecl_is_contiguous0;

/* Implemented in Fortran.  */
extern GTY(()) tree gfor_fndecl_sc_kind;
extern GTY(()) tree gfor_fndecl_si_kind;
extern GTY(()) tree gfor_fndecl_sl_kind;
extern GTY(()) tree gfor_fndecl_sr_kind;

/* IEEE-related.  */
extern GTY(()) tree gfor_fndecl_ieee_procedure_entry;
extern GTY(()) tree gfor_fndecl_ieee_procedure_exit;

/* RANDOM_INIT.  */
extern GTY(()) tree gfor_fndecl_random_init;
extern GTY(()) tree gfor_fndecl_caf_random_init;

/* True if node is an integer constant.  */
#define INTEGER_CST_P(node) (TREE_CODE(node) == INTEGER_CST)

/* gfortran-specific declaration information, the _CONT versions denote
   arrays with CONTIGUOUS attribute.  */

#define GFC_DTYPE_ELEM_LEN 0
#define GFC_DTYPE_VERSION 1
#define GFC_DTYPE_RANK 2
#define GFC_DTYPE_TYPE 3
#define GFC_DTYPE_ATTRIBUTE 4

enum gfc_array_kind
{
  GFC_ARRAY_UNKNOWN,
  GFC_ARRAY_ASSUMED_SHAPE,
  GFC_ARRAY_ASSUMED_SHAPE_CONT,
  GFC_ARRAY_ASSUMED_RANK,
  GFC_ARRAY_ASSUMED_RANK_CONT,
  GFC_ARRAY_ASSUMED_RANK_ALLOCATABLE,
  GFC_ARRAY_ASSUMED_RANK_POINTER,
  GFC_ARRAY_ASSUMED_RANK_POINTER_CONT,
  GFC_ARRAY_ALLOCATABLE,
  GFC_ARRAY_POINTER,
  GFC_ARRAY_POINTER_CONT
};

/* Array types only.  */
struct GTY(())	lang_type	 {
  int rank, corank;
  enum gfc_array_kind akind;
  tree lbound[GFC_MAX_DIMENSIONS];
  tree ubound[GFC_MAX_DIMENSIONS];
  tree stride[GFC_MAX_DIMENSIONS];
  tree size;
  tree offset;
  tree dtype;
  tree dataptr_type;
  tree base_decl[2];
  tree nonrestricted_type;
  tree caf_token;
  tree caf_offset;
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
  /* For assumed-shape coarrays.  */
  tree token, caf_offset;
  unsigned int scalar_allocatable : 1;
  unsigned int scalar_pointer : 1;
  unsigned int scalar_target : 1;
  unsigned int optional_arg : 1;
};


#define GFC_DECL_ASSIGN_ADDR(node) DECL_LANG_SPECIFIC(node)->addr
#define GFC_DECL_STRING_LEN(node) DECL_LANG_SPECIFIC(node)->stringlen
#define GFC_DECL_TOKEN(node) DECL_LANG_SPECIFIC(node)->token
#define GFC_DECL_CAF_OFFSET(node) DECL_LANG_SPECIFIC(node)->caf_offset
#define GFC_DECL_SAVED_DESCRIPTOR(node) \
  (DECL_LANG_SPECIFIC(node)->saved_descriptor)
#define GFC_DECL_SCALAR_ALLOCATABLE(node) \
  (DECL_LANG_SPECIFIC (node)->scalar_allocatable)
#define GFC_DECL_SCALAR_POINTER(node) \
  (DECL_LANG_SPECIFIC (node)->scalar_pointer)
#define GFC_DECL_SCALAR_TARGET(node) \
  (DECL_LANG_SPECIFIC (node)->scalar_target)
#define GFC_DECL_OPTIONAL_ARGUMENT(node) \
  (DECL_LANG_SPECIFIC (node)->optional_arg)
#define GFC_DECL_GET_SCALAR_ALLOCATABLE(node) \
  (DECL_LANG_SPECIFIC (node) ? GFC_DECL_SCALAR_ALLOCATABLE (node) : 0)
#define GFC_DECL_GET_SCALAR_POINTER(node) \
  (DECL_LANG_SPECIFIC (node) ? GFC_DECL_SCALAR_POINTER (node) : 0)
#define GFC_DECL_GET_SCALAR_TARGET(node) \
  (DECL_LANG_SPECIFIC (node) ? GFC_DECL_SCALAR_TARGET (node) : 0)
#define GFC_DECL_PACKED_ARRAY(node) DECL_LANG_FLAG_0(node)
#define GFC_DECL_PARTIAL_PACKED_ARRAY(node) DECL_LANG_FLAG_1(node)
#define GFC_DECL_ASSIGN(node) DECL_LANG_FLAG_2(node)
#define GFC_DECL_COMMON_OR_EQUIV(node) DECL_LANG_FLAG_3(node)
#define GFC_DECL_CRAY_POINTEE(node) DECL_LANG_FLAG_4(node)
#define GFC_DECL_RESULT(node) DECL_LANG_FLAG_5(node)
#define GFC_DECL_PTR_ARRAY_P(node) DECL_LANG_FLAG_6(node)
#define GFC_DECL_ASSOCIATE_VAR_P(node) DECL_LANG_FLAG_7(node)
#define GFC_DECL_CLASS(node) DECL_LANG_FLAG_8(node)

/* An array descriptor.  */
#define GFC_DESCRIPTOR_TYPE_P(node) TYPE_LANG_FLAG_1(node)
/* An array without a descriptor.  */
#define GFC_ARRAY_TYPE_P(node) TYPE_LANG_FLAG_2(node)
/* Fortran CLASS type.  */
#define GFC_CLASS_TYPE_P(node) TYPE_LANG_FLAG_4(node)
/* The GFC_TYPE_ARRAY_* members are present in both descriptor and
   descriptorless array types.  */
#define GFC_TYPE_ARRAY_LBOUND(node, dim) \
  (TYPE_LANG_SPECIFIC(node)->lbound[dim])
#define GFC_TYPE_ARRAY_UBOUND(node, dim) \
  (TYPE_LANG_SPECIFIC(node)->ubound[dim])
#define GFC_TYPE_ARRAY_STRIDE(node, dim) \
  (TYPE_LANG_SPECIFIC(node)->stride[dim])
#define GFC_TYPE_ARRAY_RANK(node) (TYPE_LANG_SPECIFIC(node)->rank)
#define GFC_TYPE_ARRAY_CORANK(node) (TYPE_LANG_SPECIFIC(node)->corank)
#define GFC_TYPE_ARRAY_CAF_TOKEN(node) (TYPE_LANG_SPECIFIC(node)->caf_token)
#define GFC_TYPE_ARRAY_CAF_OFFSET(node) (TYPE_LANG_SPECIFIC(node)->caf_offset)
#define GFC_TYPE_ARRAY_SIZE(node) (TYPE_LANG_SPECIFIC(node)->size)
#define GFC_TYPE_ARRAY_OFFSET(node) (TYPE_LANG_SPECIFIC(node)->offset)
#define GFC_TYPE_ARRAY_AKIND(node) (TYPE_LANG_SPECIFIC(node)->akind)
/* Code should use gfc_get_dtype instead of accessing this directly.  It may
   not be known when the type is created.  */
#define GFC_TYPE_ARRAY_DTYPE(node) (TYPE_LANG_SPECIFIC(node)->dtype)
#define GFC_TYPE_ARRAY_DATAPTR_TYPE(node) \
  (TYPE_LANG_SPECIFIC(node)->dataptr_type)
#define GFC_TYPE_ARRAY_BASE_DECL(node, internal) \
  (TYPE_LANG_SPECIFIC(node)->base_decl[(internal)])


/* Build an expression with void type.  */
#define build1_v(code, arg) \
	fold_build1_loc (input_location, code, void_type_node, arg)
#define build2_v(code, arg1, arg2) \
	fold_build2_loc (input_location, code, void_type_node, arg1, arg2)
#define build3_v(code, arg1, arg2, arg3) \
	fold_build3_loc (input_location, code, void_type_node, arg1, arg2, arg3)
#define build4_v(code, arg1, arg2, arg3, arg4) \
	build4_loc (input_location, code, void_type_node, arg1, arg2, \
		    arg3, arg4)

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
void gfc_apply_interface_mapping (gfc_interface_mapping *,
				  gfc_se *, gfc_expr *);


/* Standard error messages used in all the trans-*.c files.  */
extern const char gfc_msg_fault[];

#define OMPWS_WORKSHARE_FLAG	1	/* Set if in a workshare construct.  */
#define OMPWS_CURR_SINGLEUNIT	2	/* Set if current gfc_code in workshare
					   construct is not workshared.  */
#define OMPWS_SCALARIZER_WS	4	/* Set if scalarizer should attempt
					   to create parallel loops.  */
#define OMPWS_SCALARIZER_BODY	8	/* Set if handling body of potential
					   parallel loop.  */
#define OMPWS_NOWAIT		16	/* Use NOWAIT on OMP_FOR.  */
extern int ompws_flags;

#endif /* GFC_TRANS_H */
